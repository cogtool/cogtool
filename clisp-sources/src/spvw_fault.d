# Page fault and protection handling.

# ------------------------------ Specification --------------------------------

# Physical page size. When a fault occurs, an entire physical page must
# change its protections.
  local /* uintL */ aint physpagesize;  # = map_pagesize or mmap_pagesize

# 2^physpageshift = physpagesize
  local uintL physpageshift;

# Initialization.
# local void init_physpagesize (void);

# Tries to repair a page fault at a single address.
typedef enum { handler_failed, handler_done } handle_fault_result_t;
local handle_fault_result_t handle_fault (aint address, int verbose);

#if defined(GENERATIONAL_GC) && defined(SPVW_MIXED)
# Tries to repair a fault spanning a range of pages.
# handle_fault_range(PROT_READ,start,end) ensures an address range is readable.
# handle_fault_range(PROT_READ_WRITE,start,end) makes it writable.
  global bool handle_fault_range (int prot, aint start_address, aint end_address);
#endif

#ifdef GENERATIONAL_GC

# Does the same as mprotect.
# Aborts if unsuccessful.
local void xmprotect (aint addr, uintM len, int prot);

# Applies mprotect to all (multi-)mappings of an address range.
# Aborts if unsuccessful.
local void xmmprotect (Heap* heap, aint addr, uintM len, int prot);

#endif

# ------------------------------ Implementation -------------------------------

# Initialization.
  #define init_physpageshift()  \
    { var uintL x = physpagesize;                 \
      var uintL i = 0;                            \
      until ((x >>= 1) == 0) { i++; }             \
      if (!((1UL << i) == physpagesize)) abort(); \
      physpageshift = i;                          \
    }
  #ifdef MAP_MEMORY
    #define init_physpagesize()  \
      physpagesize = map_pagesize; \
      init_physpageshift();
  #else
    #define init_physpagesize()  \
      physpagesize = mmap_pagesize; \
      init_physpageshift();
  #endif

#ifdef GENERATIONAL_GC # implies SPVW_PURE_BLOCKS <==> SINGLEMAP_MEMORY
                       # or      SPVW_MIXED_BLOCKS_STAGGERED
                       # or      SPVW_MIXED_BLOCKS_OPPOSITE

# subroutine for protection: PROT_NONE -> PROT_READ
local int handle_read_fault (aint address, physpage_state_t* physpage)
{
  # During GC the physpage cache contents may be abused by MORRIS_GC,
  # so don't use it.
  if (inside_gc) {
    fprintf(stderr,"\n*** - " "handle_fault called at a point inside GC where it shouldn't!\n");
    OS_set_errno(0);
    return -1;
  }
  # bring page up to date with the state of the cache:
  {
    var uintL count = physpage->cache_size;
    if (count > 0) {
      var old_new_pointer_t* ptr = physpage->cache;
      #if !defined(MULTIMAP_MEMORY)
      if (mprotect((void*)address, physpagesize, PROT_READ_WRITE) < 0)
        return -1;
      #endif
      dotimespL(count,count, {
        DEBUG_SPVW_ASSERT(consp(*(ptr->p))
                          ? consp(ptr->o) && is_valid_cons_address(as_oint(ptr->o))
                          : !consp(ptr->o) && is_valid_varobject_address(as_oint(ptr->o)));
        *(ptr->p) = ptr->o;
        ptr++;
      });
    }
  }
  # superimpose page read-only:
  #if !defined(MULTIMAP_MEMORY)
  if (mprotect((void*)address, physpagesize, PROT_READ) < 0)
    return -1;
  #else # MULTIMAP_MEMORY
  #if !defined(WIDE_SOFT)
  ASSERT(address == upointer(address));
  #endif
  {
    var uintL type;
    for (type = 0; type < typecount; type++)
      if (mem.heapnr_from_type[type] >= 0) # type listed in MM_TYPECASES?
        if (mprotect((void*)combine(type,address), physpagesize, PROT_READ) < 0)
          return -1;
  }
  #endif
  physpage->protection = PROT_READ;
  return 0;
}

# subroutine for protection: PROT_READ -> PROT_READ_WRITE
local int handle_readwrite_fault (aint address, physpage_state_t* physpage)
{
  # superimose page read-write:
  #if !defined(MULTIMAP_MEMORY)
  if (mprotect((void*)address, physpagesize, PROT_READ_WRITE) < 0)
    return -1;
  #else # MULTIMAP_MEMORY
  ASSERT(address == upointer(address));
  {
    var uintL type;
    for (type = 0; type < typecount; type++)
      if (mem.heapnr_from_type[type] >= 0) # type listed in MM_TYPECASES?
        if (mprotect((void*)combine(type,address), physpagesize, PROT_READ_WRITE) < 0)
          return -1;
  }
  #endif
  physpage->protection = PROT_READ_WRITE;
  return 0;
}

# mapped generation: the old one
#define heap_mgen_start  heap_gen0_start
#define heap_mgen_end    heap_gen0_end

#else

#define heap_mgen_start  heap_start
#define heap_mgen_end    heap_end

#endif # GENERATIONAL_GC

local handle_fault_result_t handle_fault (aint address, int verbose)
{
  var uintL heapnr;
  var object obj = as_object((oint)address << oint_addr_shift);
  var aint uaddress = canon(address); # hopefully = canonaddr(obj);
  var aint pa_uaddress = uaddress & -physpagesize; # page aligned address
  #ifdef SPVW_PURE_BLOCKS
  heapnr = typecode(obj);
  #elif defined(SPVW_MIXED_BLOCKS_STAGGERED)
  heapnr = (uaddress >= mem.heaps[1].heap_mgen_start ? 1 : 0);
  #else # SPVW_MIXED_BLOCKS_OPPOSITE
  heapnr = (uaddress >= mem.heaps[1].heap_start ? 1 : 0);
  #endif
  {
    #ifdef GENERATIONAL_GC
    var Heap* heap = &mem.heaps[heapnr];
    if (!is_heap_containing_objects(heapnr))
      goto error1;
    if (!((heap->heap_gen0_start <= uaddress) && (uaddress < heap->heap_gen0_end)))
      goto error2;
    if (heap->physpages == NULL)
      goto error5;
    {
      var uintL pageno = (pa_uaddress>>physpageshift)-(heap->heap_gen0_start>>physpageshift);
      {
        var physpage_state_t* physpage = &heap->physpages[pageno];
        switch (physpage->protection) {
          case PROT_NONE:
            # protection: PROT_NONE -> PROT_READ
            if (handle_read_fault(pa_uaddress,physpage) < 0)
              goto error6;
            return handler_done;
          case PROT_READ:
            # protection: PROT_READ -> PROT_READ_WRITE
            if (handle_readwrite_fault(pa_uaddress,physpage) < 0)
              goto error7;
            return handler_done;
          case PROT_READ_WRITE:
            goto error8;
          default:
            goto error9;
        }
       error6: # handle_read_fault() failed
        if (verbose) {
          var int saved_errno = OS_errno;
          fprintf(stderr,"\n*** - " "handle_fault error6 ! mprotect(0x%lx,0x%lx,...) -> ", address & -physpagesize, physpagesize);
          errno_out(saved_errno);
        }
        goto error;
       error7: # handle_readwrite_fault() failed
        if (verbose) {
          var int saved_errno = OS_errno;
          fprintf(stderr,"\n*** - " "handle_fault error7 ! mprotect(0x%lx,0x%lx,%d) -> ", address & -physpagesize, physpagesize, PROT_READ_WRITE);
          errno_out(saved_errno);
        }
        goto error;
       error8: # fault on a read-write page
        if (verbose)
          fprintf(stderr,"\n*** - " "handle_fault error8 ! protection = %d", physpage->protection);
        goto error;
       error9: # invalid protection value
        if (verbose)
          fprintf(stderr,"\n*** - " "handle_fault error9 ! protection = %d", physpage->protection);
        goto error;
      }
    }
   error5: # fault on a read-write page with no physpages array
    if (verbose)
      fprintf(stderr,"\n*** - " "handle_fault error5 !");
    goto error;
   error1: # A fault was not expected on this type of heap.
    if (verbose)
      fprintf(stderr,"\n*** - " "handle_fault error1 !");
    goto error;
   error2: # The address is outside of the used address range for this heap.
    if (verbose)
      fprintf(stderr,"\n*** - " "handle_fault error2 ! address = 0x%lx not in [0x%lx,0x%lx) !", address, heap->heap_mgen_start, heap->heap_mgen_end);
    goto error;
    #endif
  }
 error:
  return handler_failed;
}

#if defined(GENERATIONAL_GC) && defined(SPVW_MIXED_BLOCKS)
# System calls like read() and write(), when they operate on pages with
# insufficient permissions, don't signal SIGSEGV. Instead, they return with
# errno=EFAULT and unpredictable side effects.
# handle_fault_range(PROT_READ,start,end) makes an address range readable.
# handle_fault_range(PROT_READ_WRITE,start,end) makes an address range writable.
global bool handle_fault_range (int prot, aint start_address, aint end_address)
{
  start_address = canon(start_address);
  end_address = canon(end_address);
  if (!(start_address < end_address))
    return true;
  var Heap* heap = &mem.heaps[0]; # varobject_heap
  var bool did_pagein = false;
  if ((end_address <= heap->heap_mgen_start) || (heap->heap_mgen_end <= start_address))
    return true; # nothing to do, but strange that an error occurred at all
  #ifdef GENERATIONAL_GC
    if (heap->physpages == NULL) {
      if (did_pagein)
        return true;
      return false;
    }
    {
      var aint pa_uaddress;
      for (pa_uaddress = start_address & -physpagesize; pa_uaddress < end_address; pa_uaddress += physpagesize)
        if ((heap->heap_gen0_start <= pa_uaddress) && (pa_uaddress < heap->heap_gen0_end)) {
          var uintL pageno = (pa_uaddress>>physpageshift)-(heap->heap_gen0_start>>physpageshift);
          var physpage_state_t* physpage = &heap->physpages[pageno];
          if ((physpage->protection == PROT_NONE) && (prot == PROT_READ || prot == PROT_READ_WRITE)) {
            # protection: PROT_NONE -> PROT_READ
            if (handle_read_fault(pa_uaddress,physpage) < 0)
              return false;
          }
          if (!(physpage->protection == PROT_READ_WRITE) && (prot == PROT_READ_WRITE)) {
            # protection: PROT_READ -> PROT_READ_WRITE
            if (handle_readwrite_fault(pa_uaddress,physpage) < 0)
              return false;
          }
        }
    }
    return true;
  #else
    return did_pagein;
  #endif
}
#endif

#ifdef GENERATIONAL_GC

local void xmprotect (aint addr, uintM len, int prot) {
  if (mprotect((void*)addr,len,prot) < 0) {
    fprintf(stderr,GETTEXTL("mprotect(0x%lx,%d,%d) failed."),addr,len,prot);
    errno_out(OS_errno);
    abort();
  }
}

#ifdef MULTIMAP_MEMORY
  local void xmmprotect (Heap* heap, aint addr, uintM len, int prot)
  {
    unused heap;
    var uintL type;
    for (type = 0; type < typecount; type++)
      if (mem.heapnr_from_type[type] >= 0) # type listed in MM_TYPECASES?
        xmprotect((aint)combine(type,addr),len,prot);
  }
#else
  #define xmmprotect(heap,addr,len,prot)  xmprotect(addr,len,prot)
#endif

#endif # GENERATIONAL_GC
