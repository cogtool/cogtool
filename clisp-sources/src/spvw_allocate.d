# General macro for object allocation.

# ------------------------------ Specification --------------------------------

# Asks the OS for a piece of memory of need bytes, and verifies that it lies
# in the address range usable for Lisp objects. Returns NULL if couldn't
# satisfy the request.
  local void* mymalloc (uintM need);

# Allocates a Lisp object.
# allocate(type,flag,size,ptrtype,ptr,statement)
# > type: the typecode
# > flag: a literal bool, true for varobject, false for two-pointer-object
# > size: the needed memory size (including header and alignment). Must be
#         a constant expression or a variable.
# ptrtype: the C type of `ptr'
# ptr: a C variable
# Fetches a piece of memory of size `size', suitable for a Lisp object of type
# `type', sets `ptr' to its start address. Then `statement' is executed.
# Finally `ptr' is combined with the type info and returned from the current
# function (via `return').

# ------------------------------ Implementation -------------------------------

# error-message because of full memory
nonreturning_function(local, fehler_speicher_voll, (void)) {
  dynamic_bind(S(use_clcs),NIL); # bind SYS::*USE-CLCS* to NIL
  if (posfixnump(Symbol_value(S(gc_statistics_stern)))) {
     # bind SYS::*GC-STATISTICS* to 0
    dynamic_bind(S(gc_statistics_stern),Fixnum_0);
  }
  fehler(storage_condition,GETTEXT("No more room for LISP objects"));
}

#if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && RESERVE

# emergency measure, when memory is full: tap the reserve and error-message.
  nonreturning_function(local, error_speicher_voll, (void)) {
    # Abhilfe: Reservespeicher wird halbiert.
    var uintL reserve = mem.MEMTOP - mem.MEMRES; # noch freie Reserve
    if (reserve>=8) { # reserve memory also full?
      # no -> tap reserve memory and issue error-message
      # half reserve
      move_conses(round_down(floor(reserve,2),varobject_alignment));
      # halved reserve, aligned: shift up the conses by that amount
      fehler_speicher_voll();
    } else {
      # yes -> hard error-message
      fputs("\n",stderr);
      fprintf(stderr,GETTEXTL("*** - " "No more room for LISP objects: RESET"));
      fputs("\n",stderr);
      fflush(stderr);
      reset(1); /* and return to the last driver-frame */
    }
  }

# On relaxed situation: fill up reserve again.
# Invariant: (mem.conses.heap_start-mem.varobjects.heap_end >= need).
  local void relax_reserve (uintM need)
  {
    # Now, enough space is available. Maybe even enough, to enlarge
    # the reserve memory to normal size?
    var uintM free = (mem.conses.heap_start-mem.varobjects.heap_end) - need;
                     # bytes still free
    var uintM free_reserve = mem.MEMTOP-mem.MEMRES;
                     # bytes still free in reserve, <=RESERVE
    var uintM free_total = free + free_reserve;
                     # free object memory + free reserve
    if (free_total >= RESERVE) { # at least normal value RESERVE ?
      # yes -> bring reserve memory to normal size, by shifting
      # the conses down by (RESERVE - free_reserve) :
      move_conses(free_reserve-RESERVE);
      # thus, there will be enough free for 'need'.
    }
  }

#else

#define error_speicher_voll()  fehler_speicher_voll()
#define relax_reserve(need)

#endif

# determines, if an address lies in the interval [0..2^oint_addr_len-1] :
  #if !defined(TYPECODES) || defined(WIDE_SOFT)
    #define pointable_usable_test(a)  true
  #else
    #define pointable_usable_test(a)  \
      ((void*)pointable(type_pointer_object(0,a)) == (void*)(a))
  #endif

# fetches memory from the operating system
  local void* mymalloc (uintM need)
  {
    var void* addr;
    begin_system_call();
    addr = malloc(need);
    end_system_call();
    if (addr==NULL)
      return NULL;
    # Interval [addr,addr+need-1] must lie in [0..2^oint_addr_len-1] :
    {
      var aint a = (aint)addr; # a = lower interval bound
      if (pointable_usable_test(a)) {
        a = round_down(a + need-1,bit(addr_shift)); # a = upper interval bound
        if (pointable_usable_test(a))
          return addr;
      }
    }
    # we cannot do anything with this piece of memory, return it again:
    begin_system_call();
    free(addr);
    end_system_call();
    return NULL;
  }

#ifdef DEBUG_GCSAFETY
  # A counter that is incremented each time an allocation occurs that could
  # trigger GC.
  global uintL alloccount = 1;
  #define inc_alloccount()  (void)(alloccount++)
#else
  #define inc_alloccount()  (void)0
#endif

#if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && !defined(TRIVIALMAP_MEMORY)

# make room for a new object.
# if none is available -> error-message.
# make_space_FLAG(need);
# > flag: if object is of variable length or not
# > uintM need: requested space in bytes (a variable or constant)
  # the test, if room is available, as macro, the rest as function:
  #define make_space_true(need)  make_space(need)
  #define make_space_false(need)  make_space(need)
  #define make_space(need)  \
    { if (not_enough_room_p(need)) make_space_gc(need); \
      inc_alloccount();                                 \
    }
  #if !defined(GENERATIONAL_GC)
    #define not_enough_room_p(need)  (mem.conses.heap_start-mem.varobjects.heap_end < (uintP)(need))
  #else
    #define not_enough_room_p(need)  (mem.total_room < (uintM)(need))
  #endif
  local void make_space_gc (uintM need)
  {
    # (mem.conses.heap_start-mem.varobjects.heap_end < need)  resp.
    # (mem.total_room < need)  is already checked, so there
    # is not enough room
   not_enough_room:
    {
      gar_col_simple(); # call garbage collector
     doing_gc:
      # Test for keyboard interrupt
      interruptp({
        pushSTACK(S(gc)); tast_break();
        if (not_enough_room_p(need))
          goto not_enough_room;
        else
          return;
      });
      if (mem.conses.heap_start-mem.varobjects.heap_end < (uintP)(need)) { # and test again
        # There is really not enough room.
        # [work with 'realloc' under UNIX??]
        # remedy: try a full GC.
        #ifdef GENERATIONAL_GC
        if (!mem.last_gc_full) {
          gar_col(); goto doing_gc;
        } else
        #endif
          { error_speicher_voll(); }
      } else {
        # Now, there is enough room. Maybe even enough, to enlarge
        # the reserve memory to normal size?
        relax_reserve(need);
        # now for sure (mem.conses.heap_start-mem.varobjects.heap_end >= need).
        #ifdef GENERATIONAL_GC
        # if (mem.total_room < need), we ignore that:
        if (mem.total_room < need)
          mem.total_room = need;
        #endif
      }
    }
  }

#endif

#if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && defined(TRIVIALMAP_MEMORY)

# make room for a new object.
# if none is available -> error-message.
# make_space_FLAG(need);
# > flag: if object is of variable length or not
# > uintM need: requested space in bytes (a variable or constant)
  # the test, if room is available, as macro, the rest as function:
  #define make_space_true(need)  \
    { if ((mem.total_room < (uintM)(need))                                      \
          || (mem.varobjects.heap_limit - mem.varobjects.heap_end < (uintP)(need)) \
         )                                                                      \
        make_space_gc_true(need,&mem.varobjects);                               \
      inc_alloccount();                                                         \
    }
  #define make_space_false(need)  \
    { if ((mem.total_room < (uintM)(need))                                   \
          || (mem.conses.heap_start - mem.conses.heap_limit < (uintP)(need)) \
         )                                                                   \
        make_space_gc_false(need,&mem.conses);                               \
      inc_alloccount();                                                      \
    }
  local void make_space_gc_true (uintM need, Heap* heapptr)
  {
    # (mem.total_room < need) || (heapptr->heap_limit - heapptr->heap_end < need)
    # is already checked, so there is not enough room.
   not_enough_room:
    {
      var bool done_gc = false;
      if (mem.total_room < need) {
       do_gc:
        gar_col_simple(); # call garbage collector
       doing_gc:
        # Test for keyboard interrupt
        interruptp({
          pushSTACK(S(gc)); tast_break();
          if ((mem.total_room < need) || (heapptr->heap_limit - heapptr->heap_end < need))
            goto not_enough_room;
          else
            return;
        });
        done_gc = true;
      }
      # Either now is (mem.total_room >= need), or we have just performed
      # a GC. In both cases we concentrate on
      # enlarging heapptr->heap_limit .
      {
        var aint needed_limit = heapptr->heap_end + need;
        if (needed_limit <= heapptr->heap_limit) # has the GC done its job?
          return; # yes -> finished
        # round up to the next page boundary:
        needed_limit = (needed_limit + map_pagesize-1) & -map_pagesize; # for suer > heapptr->heap_limit
        # allocate new memory:
        if (needed_limit <= mem.conses.heap_limit) { # avoid crossover
          var aint mapstart = heapptr->heap_limit;
          #if varobjects_misaligned
          mapstart &= -map_pagesize;
          #endif
          begin_system_call();
          var int ergebnis = zeromap((void*)mapstart,needed_limit - mapstart);
          end_system_call();
          if (ergebnis >= 0)
            goto sufficient;
          fprintf(stderr,GETTEXTL("Trying to make room through a GC..."));
          fputs("\n",stderr);
        }
        # not successful
        if (!done_gc)
          goto do_gc;
        #ifdef GENERATIONAL_GC
        if (!mem.last_gc_full) {
          gar_col(); goto doing_gc;
        }
        #endif
        error_speicher_voll();
       sufficient:
        heapptr->heap_limit = needed_limit;
      }
      # now for sure (heapptr->heap_limit - heapptr->heap_end >= need).
      # if (mem.total_room < need), we ignore that:
      if (mem.total_room < need)
        mem.total_room = need;
    }
  }
  local void make_space_gc_false (uintM need, Heap* heapptr)
  {
    # (mem.total_room < need) || (heapptr->heap_start - heapptr->heap_limit < need)
    # is already checked, so there is not enough room.
   not_enough_room:
    {
      var bool done_gc = false;
      if (mem.total_room < need) {
       do_gc:
        gar_col_simple(); # call garbage collector
       doing_gc:
        # Test for keyboard interrupt
        interruptp({
          pushSTACK(S(gc)); tast_break();
          if ((mem.total_room < need) || (heapptr->heap_start - heapptr->heap_limit < need))
            goto not_enough_room;
          else
            return;
        });
        done_gc = true;
      }
      # Either now is (mem.total_room >= need), or we have just performed
      # a GC. In both cases we concentrate on
      # reducing heapptr->heap_limit .
      {
        var aint needed_limit = heapptr->heap_start - need;
        if (needed_limit > heapptr->heap_start) # wraparound?
          goto failed;
        if (needed_limit >= heapptr->heap_limit) # has the GC done its job?
          return; # yes -> finished
        # round down to the next page boundary:
        needed_limit = needed_limit & -map_pagesize; # for sure < heapptr->heap_limit
        # allocate new memory:
        if (needed_limit >= mem.varobjects.heap_limit) { # avoid crossover
          begin_system_call();
          var int ergebnis = zeromap((void*)needed_limit,heapptr->heap_limit - needed_limit);
          end_system_call();
          if (ergebnis >= 0)
            goto sufficient;
          fprintf(stderr,GETTEXTL("Trying to make room through a GC..."));
          fputs("\n",stderr);
        }
        # not successful
       failed:
        if (!done_gc)
          goto do_gc;
        #ifdef GENERATIONAL_GC
        if (!mem.last_gc_full) {
          gar_col(); goto doing_gc;
        }
        #endif
        error_speicher_voll();
       sufficient:
        heapptr->heap_limit = needed_limit;
      }
      # now for sure (heapptr->heap_start - heapptr->heap_limit >= need).
      # if (mem.total_room < need), we ignore that:
      if (mem.total_room < need)
        mem.total_room = need;
    }
  }

#endif

#if defined(SPVW_PURE_BLOCKS) || defined(SPVW_MIXED_BLOCKS_STAGGERED) # <==> (SINGLEMAP_MEMORY || TRIVIALMAP_MEMORY) && !SPVW_MIXED_BLOCKS_OPPOSITE

# make room for a new object.
# if none is available -> error-message.
# make_space(need,heapptr);
# > uintM need: requested space in bytes (a variable or constant)
# > Heap* heapptr: pointer to the heap, where the room has to be taken from
  # the test, if room is available, as macro, the rest as function:
  #define make_space(need,heapptr)  \
    { if ((mem.total_room < (uintM)(need))                                 \
          || ((heapptr)->heap_limit - (heapptr)->heap_end < (uintP)(need)) \
         )                                                                 \
        make_space_gc(need,heapptr);                                       \
      inc_alloccount();                                                    \
    }
  local void make_space_gc (uintM need, Heap* heapptr)
  {
    # (mem.total_room < need) || (heapptr->heap_limit - heapptr->heap_end < need)
    # is already checked, so there is not enough room.
   not_enough_room:
    {
      var bool done_gc = false;
      if (mem.total_room < need) {
       do_gc:
        gar_col_simple(); # call garbage collector
       doing_gc:
        # Test for keyboard interrupt
        interruptp({
          pushSTACK(S(gc)); tast_break();
          if ((mem.total_room < need) || (heapptr->heap_limit - heapptr->heap_end < need))
            goto not_enough_room;
          else
            return;
        });
        done_gc = true;
      }
      # Either now is (mem.total_room >= need), or we have just performed
      # a GC. In both cases we concentrate on
      # enlarging heapptr->heap_limit .
      {
        var aint needed_limit = heapptr->heap_end + need;
        if (needed_limit <= heapptr->heap_limit) # has the GC done its job?
          return; # yes -> finished
        # round up to the next page boundary:
        #ifndef GENERATIONAL_GC
        needed_limit = round_up(needed_limit,map_pagesize); # for sure > heapptr->heap_limit
        #else # map_pagesize is commonly known a power of two
        needed_limit = (needed_limit + map_pagesize-1) & -map_pagesize; # for sure > heapptr->heap_limit
        #endif
        # allocate new memory:
        if (needed_limit-1 <= heapptr->heap_hardlimit-1) {
          var aint mapstart = heapptr->heap_limit;
          #if varobjects_misaligned
          mapstart &= -map_pagesize;
          #endif
          begin_system_call();
          var int ergebnis = zeromap((void*)mapstart,needed_limit - mapstart);
          end_system_call();
          if (ergebnis >= 0)
            goto sufficient;
          fprintf(stderr,GETTEXTL("Trying to make room through a GC..."));
          fputs("\n",stderr);
        }
        # not successful
        if (!done_gc)
          goto do_gc;
        #ifdef GENERATIONAL_GC
        if (!mem.last_gc_full) {
          gar_col(); goto doing_gc;
        }
        #endif
        fehler_speicher_voll();
       sufficient:
        heapptr->heap_limit = needed_limit;
      }
      # now for sure (heapptr->heap_limit - heapptr->heap_end >= need).
      # if (mem.total_room < need), we ignore that:
      if (mem.total_room < need)
        mem.total_room = need;
    }
  }

#endif

#ifdef SPVW_PAGES

# make room for a new object.
# if none is available -> error-message.
# make_space(need,heap_ptr,stack_ptr, page);
# > uintM need: requested space in bytes (a variable or constant)
# > Heap* heap_ptr: address of the heap, where the room has to be taken from
# > AVL(AVLID,stack) * stack_ptr: Address of a local stack,
#   for a later AVL(AVLID,move)
# < Pages page: found page, where the room is located
  # the test, if room is available, as macro, the rest as function:
  #define make_space(need,heap_ptr,stack_ptr,pagevar)  \
    { pagevar = AVL(AVLID,least)(need,&(heap_ptr)->inuse,stack_ptr); \
      if (pagevar==EMPTY)                                            \
        pagevar = make_space_gc(need,heap_ptr,stack_ptr);            \
      inc_alloccount();                                              \
    }
  local Pages make_space_gc (uintM need, Heap* heap_ptr, AVL(AVLID,stack) * stack_ptr)
  {
    var Pages* pages_ptr = &heap_ptr->inuse;
    var uintM misaligned = heap_ptr->misaligned;
    # AVL(AVLID,least)(need,pages_ptr,stack_ptr) == EMPTY
    # is already checked,
    # so there is not enough room.
   not_enough_room:
    # Test for keyboard interrupt
    #define handle_interrupt_after_gc()                               \
      interruptp({                                                    \
        pushSTACK(S(gc)); tast_break();                               \
       {var Pages page = AVL(AVLID,least)(need,pages_ptr,stack_ptr);  \
        if (page==EMPTY) goto not_enough_room;                        \
        else return page;                                             \
      }})
    #if !defined(AVL_SEPARATE)
      # try to get space from the operating system:
      #define make_space_using_malloc()                                     \
        do {                                                                \
          var uintM size1 = round_up(misaligned+need,sizeof(cons_));        \
          if (size1 < std_page_size) { size1 = std_page_size; }             \
         {var uintM size2 = size1 + sizeof(NODE) + (varobject_alignment-1); \
          var aint addr = (aint)mymalloc(size2);                            \
          if ((void*)addr != NULL) {                                        \
            # get page from the OS.                                         \
            var Pages page = (Pages)addr;                                   \
            page->m_start = addr; page->m_length = size2;                   \
            # initialize:                                                   \
            page->page_start = page->page_end = page_start0(page) + misaligned; \
            page->page_room = size1;                                        \
            # add to this heap:                                             \
            *pages_ptr = AVL(AVLID,insert1)(page,*pages_ptr);               \
            if (AVL(AVLID,least)(need,pages_ptr,stack_ptr) != page)         \
              abort();                                                      \
            mem.total_space += size1;                                       \
            return page;                                                    \
        }}} while(0)
    #else # AVL_SEPARATE
      # try to get space from the operating system:
      #define make_space_using_malloc()                                   \
        do {                                                              \
          var uintM size1 = round_up(misaligned+need,sizeof(cons_));      \
          if (size1 < std_page_size) { size1 = std_page_size; }           \
          begin_system_call();                                            \
         {var Pages page = (NODE*)malloc(sizeof(NODE));                   \
          end_system_call();                                              \
          if (page != NULL) {                                             \
            var uintM size2 = size1 + (varobject_alignment-1);            \
            var aint addr = (aint)mymalloc(size2);                        \
            if ((void*)addr != NULL) {                                    \
              # get page from the OS.                                     \
              page->m_start = addr; page->m_length = size2;               \
              # Initialize:                                               \
              page->page_start = page->page_end = page_start0(page) + misaligned; \
              page->page_room = size1;                                    \
              # add to this heap:                                         \
              *pages_ptr = AVL(AVLID,insert1)(page,*pages_ptr);           \
              if (AVL(AVLID,least)(need,pages_ptr,stack_ptr) != page)     \
                abort();                                                  \
              mem.total_space += size1;                                   \
              return page;                                                \
           } else {                                                       \
             begin_system_call(); free(page); end_system_call();          \
           }                                                              \
        }}} while(0)
    #endif
    if ((misaligned+need <= std_page_size) && !(mem.free_pages == NULL)) {
      # take a normal sized page from the common pool:
      var Pages page = mem.free_pages;
      mem.free_pages = (Pages)page->page_gcpriv.next;
      # page is already partially correctly initialized:
      # page->page_room =
      #   round_down(page->m_start + page->m_length,varobject_alignment)
      page->page_start = page->page_end = page_start0(page) + misaligned;
      # and add to this heap:
      *pages_ptr = AVL(AVLID,insert1)(page,*pages_ptr);
      if (!(AVL(AVLID,least)(need,pages_ptr,stack_ptr) == page))
        abort();
      mem.total_space += page->page_room;
      return page;
    }
    if (used_space()+need < mem.gctrigger_space) {
      # used space did not even grow by 25% since the last GC ->
      # try it at the operating system;
      # we do the GC, when the 25%-boundary is reached.
      make_space_using_malloc();
    }
    gar_col_simple(); # call garbage collector
    handle_interrupt_after_gc();
    # and test again:
    var Pages page = AVL(AVLID,least)(need,pages_ptr,stack_ptr);
    if (page==EMPTY) {
      if (!mem.last_gc_compacted) {
        gar_col_compact(); # call compacting garbage collector
        handle_interrupt_after_gc();
        page = AVL(AVLID,least)(need,pages_ptr,stack_ptr);
      }
      if (page==EMPTY) {
        # now try it at the operating system, after all:
        make_space_using_malloc();
        fehler_speicher_voll();
      }
    }
    # treat .reserve??
    return page;
    #undef make_space_using_malloc
    #undef handle_interrupt_after_gc
  }

#endif

# Macro for the memory-allocation of a Lisp-object:
# allocate(type,flag,size,ptrtype,ptr,statement)
# > type: Expression, that returns the typecode
# > flag: if object is of variable length or not
# > size: Expression (constant or var), that specifies the size of the
#         needed piece of memory
# ptrtype: C-type of ptr
# ptr: C-Variable
# A memory piece of length size, suitable for a Lisp-object of Type type,
# is fetched and ptr is set to its start address. Then the 'statement' is
# executed (initialization of the memory piece) and finally ptr is
# returned as result, provided with the correct typeinfo.
  #ifdef SPVW_BLOCKS
   #if defined(SPVW_PURE_BLOCKS) || defined(TRIVIALMAP_MEMORY) || defined(GENERATIONAL_GC)
    #define decrement_total_room(amount)  mem.total_room -= (amount);
   #else
    #define decrement_total_room(amount)
   #endif
   #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
    #define allocate(type_expr,flag,size_expr,ptrtype,ptrvar,statement)  \
      allocate_##flag (type_expr,size_expr,ptrtype,ptrvar,statement)
    # object of variable length:
    #define allocate_true(type_expr,size_expr,ptrtype,ptrvar,statement)       \
      do {                                                                    \
        make_space_true(size_expr);                                           \
        set_break_sem_1(); # lock Break                                       \
       {var ptrtype ptrvar;                                                   \
        var object obj;                                                       \
        ptrvar = (ptrtype) mem.varobjects.heap_end; # pointer to memory piece \
        mem.varobjects.heap_end += (size_expr); # adjust memory partitioning  \
        decrement_total_room(size_expr);                                      \
        ptrvar->GCself = obj = # self pointer                                 \
          bias_type_pointer_object(varobject_bias,type_expr,ptrvar);          \
        statement; # initialize memory piece                                  \
        clr_break_sem_1(); # allow Break                                      \
        CHECK_GC_CONSISTENCY();                                               \
        return obj;                                                           \
      }} while(0)
    # Cons or similar:
    #define allocate_false(type_expr,size_expr,ptrtype,ptrvar,statement) \
      do {                                                               \
        make_space_false(size_expr);                                     \
        set_break_sem_1(); # lock Break                                  \
       {var ptrtype ptrvar;                                              \
        # pointer to memory piece:                                       \
        ptrvar = (ptrtype)(mem.conses.heap_start -= size_expr);          \
        decrement_total_room(size_expr);                                 \
        statement; # initialize memory piece                             \
        clr_break_sem_1(); # allow Break                                 \
        CHECK_GC_CONSISTENCY();                                          \
        return bias_type_pointer_object(cons_bias,type_expr,ptrvar);     \
      }} while(0)
   #endif
   #ifdef SPVW_MIXED_BLOCKS_STAGGERED
    #define allocate(type_expr,flag,size_expr,ptrtype,ptrvar,statement)  \
      allocate_##flag (type_expr,size_expr,ptrtype,ptrvar,statement)
    # Object of variable length:
    #define allocate_true(type_expr,size_expr,ptrtype,ptrvar,statement)       \
      do {                                                                    \
        make_space(size_expr,&mem.varobjects);                                \
        set_break_sem_1(); # lock Break                                       \
       {var ptrtype ptrvar;                                                   \
        var object obj;                                                       \
        ptrvar = (ptrtype) mem.varobjects.heap_end; # pointer to memory piece \
        mem.varobjects.heap_end += (size_expr); # adjust memory partitioning  \
        decrement_total_room(size_expr);                                      \
        ptrvar->GCself = obj = # self pointer                                 \
          bias_type_pointer_object(varobject_bias,type_expr,ptrvar);          \
        statement; # initialize memory piece                                  \
        clr_break_sem_1(); # allow Break                                      \
        CHECK_GC_CONSISTENCY();                                               \
        return obj;                                                           \
      }} while(0)
    # Cons or similar:
    #define allocate_false(type_expr,size_expr,ptrtype,ptrvar,statement) \
      do {                                                               \
        make_space(size_expr,&mem.conses);                               \
        set_break_sem_1(); # lock Break                                  \
        # pointer to memory piece:                                       \
       {var ptrtype ptrvar = (ptrtype) mem.conses.heap_end;              \
        mem.conses.heap_end += (size_expr); # adjust memory partitioning \
        decrement_total_room(size_expr);                                 \
        statement; # initialize memory piece                             \
        clr_break_sem_1(); # allow Break                                 \
        CHECK_GC_CONSISTENCY();                                          \
        return bias_type_pointer_object(cons_bias,type_expr,ptrvar);     \
      }} while(0)
   #endif
   #ifdef SPVW_PURE
    #define allocate(type_expr,flag,size_expr,ptrtype,ptrvar,statement) \
      do {                                                              \
        var tint _type = (type_expr);                                   \
        var Heap* heapptr = &mem.heaps[_type];                          \
        make_space(size_expr,heapptr);                                  \
        set_break_sem_1(); # lock Break                                 \
        # pointer to memory piece:                                      \
       {var ptrtype ptrvar = (ptrtype)(heapptr->heap_end);              \
        heapptr->heap_end += (size_expr); # adjust memory partitioning  \
        decrement_total_room(size_expr);                                \
        allocate_##flag (ptrvar);                                       \
        statement; # initialize memory piece                            \
        clr_break_sem_1(); # allow Break                                \
        CHECK_GC_CONSISTENCY();                                         \
        return as_object((oint)ptrvar);                                 \
      }} while(0)
    # Object of variable length:
    #define allocate_true(ptrvar)  \
      ptrvar->GCself = as_object((oint)ptrvar); # store self pointer
    # Cons or similar:
    #define allocate_false(ptrvar)
   #endif
  #endif
  #ifdef SPVW_PAGES
    #define allocate(type_expr,flag,size_expr,ptrtype,ptrvar,statement)  \
      allocate_##flag (type_expr,size_expr,ptrtype,ptrvar,statement)
   #ifdef SPVW_MIXED
    # Object of variable length:
    #define allocate_true(type_expr,size_expr,ptrtype,ptrvar,statement)    \
      do {                                                                 \
        # search the page with the smallest page_room >= size_expr:        \
        var AVL(AVLID,stack) stack;                                        \
        var Pages page;                                                    \
        make_space(size_expr,&mem.varobjects,&stack, page);                \
        set_break_sem_1(); # lock Break                                    \
        # pointer to memory piece:                                         \
       {var ptrtype ptrvar = (ptrtype)(page->page_end);                    \
        var object obj;                                                    \
        ptrvar->GCself = obj = # self pointer                              \
          bias_type_pointer_object(varobject_bias,type_expr,ptrvar);       \
        statement; # initialize memory piece                               \
        page->page_room -= (size_expr); # adjust memory partitioning       \
        page->page_end += (size_expr);                                     \
        mem.used_space += (size_expr);                                     \
        AVL(AVLID,move)(&stack); # attach page again to the right position \
        clr_break_sem_1(); # allow Break                                   \
        CHECK_AVL_CONSISTENCY();                                           \
        CHECK_GC_CONSISTENCY();                                            \
        return obj;                                                        \
      }} while(0)
    # Cons or similar:
    #define allocate_false(type_expr,size_expr,ptrtype,ptrvar,statement) \
      do {                                                               \
        # search a page with the smallest page_room >= size_expr = 8:    \
        var Pages page;                                                  \
        # first attempt: last used page                                  \
        page = mem.conses.lastused;                                      \
         # test for page->page_room < size_expr = sizeof(cons_): \       \
        if (page->page_room == 0) {                                      \
          var AVL(AVLID,stack) stack;                                    \
          # second attempt:                                              \
          make_space(size_expr,&mem.conses,&stack, page);                \
          mem.conses.lastused = page;                                    \
        }                                                                \
        set_break_sem_1(); # lock Break                                  \
       {var ptrtype ptrvar =                                             \
          (ptrtype)(page->page_end); # pointer to memory piece           \
        statement; # initialize memory piece                             \
        page->page_room -= (size_expr); # adjust memory partitioning     \
        page->page_end += (size_expr);                                   \
        mem.used_space += (size_expr);                                   \
        # As page_room now became =0 or stayed >=sizeof(cons_) ,         \
        # the sorting order of the pages remains unchanged.              \
        clr_break_sem_1(); # allow Break                                 \
        CHECK_AVL_CONSISTENCY();                                         \
        CHECK_GC_CONSISTENCY();                                          \
        return bias_type_pointer_object(cons_bias,type_expr,ptrvar);     \
      }} while(0)
   #endif
   #ifdef SPVW_PURE
    # Object of variable length:
    #define allocate_true(type_expr,size_expr,ptrtype,ptrvar,statement)      \
      do {                                                                   \
        # search a page with the smallest page_room >= size_expr:            \
        var AVL(AVLID,stack) stack;                                          \
        var Pages page;                                                      \
        var tint _type = (type_expr);                                        \
        make_space(size_expr,&mem.heaps[_type],&stack, page);                \
        set_break_sem_1(); # lock Break                                      \
       {var ptrtype ptrvar =                                                 \
          (ptrtype)(page->page_end); # pointer to memory piece               \
        var object obj;                                                      \
        # self pointer:                                                      \
        ptrvar->GCself = obj = type_pointer_object(_type,ptrvar);            \
        statement; # initialize memory piece                                 \
        page->page_room -= (size_expr); # adjust memory partitioning         \
        page->page_end += (size_expr);                                       \
        mem.used_space += (size_expr);                                       \
        AVL(AVLID,move)(&stack); # attach page again to the right position   \
        clr_break_sem_1(); # allow Break                                     \
        CHECK_AVL_CONSISTENCY();                                             \
        CHECK_GC_CONSISTENCY();                                              \
        return obj;                                                          \
      }} while(0)
    # Cons or similar:
    #define allocate_false(type_expr,size_expr,ptrtype,ptrvar,statement) \
      do {                                                               \
        # search a page with the smallest page_room >= size_expr = 8:    \
        var Pages page;                                                  \
        var tint _type = (type_expr);                                    \
        var Heap* heapptr = &mem.heaps[_type];                           \
        # first attempt: last used page                                  \
        page = heapptr->lastused;                                        \
        # test for page->page_room < size_expr = sizeof(cons_):          \
        if (page->page_room == 0) {                                      \
          var AVL(AVLID,stack) stack;                                    \
          # second attempt:                                              \
          make_space(size_expr,heapptr,&stack, page);                    \
          heapptr->lastused = page;                                      \
        }                                                                \
        set_break_sem_1(); # lock Break                                  \
       {var ptrtype ptrvar =                                             \
          (ptrtype)(page->page_end); # pointer to memory piece           \
        statement; # initialize memory piece                             \
        page->page_room -= (size_expr); # adjust memory partitioning     \
        page->page_end += (size_expr);                                   \
        mem.used_space += (size_expr);                                   \
        # As page_room now became =0 or stayed >=sizeof(cons_) ,         \
        # the sorting order of the pages remains unchanged.              \
        clr_break_sem_1(); # allow Break                                 \
        CHECK_AVL_CONSISTENCY();                                         \
        CHECK_GC_CONSISTENCY();                                          \
        return type_pointer_object(_type,ptrvar);                        \
      }} while(0)
   #endif
  #endif
