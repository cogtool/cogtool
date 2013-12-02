# Memory mapping support.

#if defined(HAVE_MMAP_ANON) || defined(HAVE_MMAP_DEVZERO) || defined(HAVE_MACH_VM) || defined(HAVE_WIN32_VM)

# ------------------------------ Specification --------------------------------

# This adds support for mapping private memory pages at given addresses.
# If HAVE_MMAP is defined, it also supports private mappings of files
# at given addresses.

# The size of a single page. (This may be a multiple of the actual physical
# page size.) Always a power of two. Initialized by mmap_init_pagesize().
  local uintP mmap_pagesize;
  local void mmap_init_pagesize (void);

# Initialize the mmap system.
# mmap_init()
# < returns 0 upon success, -1 upon failure
  local int mmap_init (void);

# Reserves an address range for use with mmap_zeromap().
# It tries to reserve the range [*map_addr,*map_endaddr). If this is not
# possible and shrinkp is true, *map_addr is increased and *map_endaddr is
# reduced as necessary.
  local int mmap_prepare (uintP* map_addr, uintP* map_endaddr, bool shrinkp);

# Fill a memory range [map_addr,map_addr+map_len-1] with empty pages.
# mmap_zeromap(map_addr,map_len)
# map_addr and map_len must be multiples of mmap_pagesize.
  local int mmap_zeromap (void* map_addr, uintM map_len);

#ifdef HAVE_MMAP
# Fill a memory range [map_addr,map_addr+map_len-1] with pages mapped in from
# file fd starting at position offset.
# map_addr and map_len must be multiples of mmap_pagesize.
  local void* mmap_filemap (void* map_addr, uintM map_len, int fd, off_t offset);
#endif

# Unmaps a memory range.
#if 0 /* Already declared in <sys/mman.h> on those platforms that have it. */
  global int munmap (void* addr, size_t len);
#endif

# Changes the access protection for a memory range.
#if 0 /* Already declared in <sys/mman.h> on those platforms that have it. */
  global int mprotect ([const] void* addr, size_t len, int prot);
#endif

# ------------------------------ Implementation -------------------------------

#if defined(HAVE_MACH_VM)

  local void mmap_init_pagesize (void)
  {
    mmap_pagesize = vm_page_size;
  }

  #define mmap_init()  0

  #define mmap_prepare(map_addr,map_endaddr,shrinkp)  0

  local int mmap_zeromap (void* map_addr, uintM map_len)
  {
    if (vm_allocate(task_self(), (vm_address_t*) &map_addr, map_len, false)
        != KERN_SUCCESS) {
      fprintf(stderr,GETTEXTL("Cannot map memory to address 0x%lx ."),
              map_addr);
      errno_out();
      return -1; /* error */
    }
    return 0;
  }

  local void* mmap_filemap (void* map_addr, uintM map_len, int fd, off_t offset)
  {
    switch (vm_allocate(task_self(), (vm_address_t*) &map_addr, map_len, false)) {
      case KERN_SUCCESS:
        break;
      default:
        errno = EINVAL; return (void*)(-1);
    }
    switch (map_fd(fd, offset, (vm_address_t*) &map_addr, 0, map_len)) {
      case KERN_SUCCESS:
        return map_addr;
      case KERN_INVALID_ADDRESS:
      case KERN_INVALID_ARGUMENT:
      default:
        errno = EINVAL; return (void*)(-1);
    }
  }

  # We need to implement munmap() ourselves.
  global int munmap (void* addr, size_t len)
  {
    switch (vm_deallocate(task_self(),addr,len)) {
      case KERN_SUCCESS:
        return 0;
      case KERN_INVALID_ADDRESS:
      default:
        errno = EINVAL; return -1;
    }
  }

  # We need to implement mprotect() ourselves.
  global int mprotect (void* addr, size_t len, int prot)
  {
    switch (vm_protect(task_self(),addr,len,0,prot)) {
      case KERN_SUCCESS:
        return 0;
      case KERN_PROTECTION_FAILURE:
        errno = EACCES; return -1;
      case KERN_INVALID_ADDRESS:
      default:
        errno = EINVAL; return -1;
    }
  }

#endif

#if defined(HAVE_WIN32_VM)

  # Return the hardware page size. (0x1000 on i386.)
  local DWORD getpagesize (void)
  {
    var SYSTEM_INFO sinfo;
    GetSystemInfo(&sinfo);
    return sinfo.dwPageSize;
  }

  local void mmap_init_pagesize (void)
  {
    mmap_pagesize = getpagesize();
  }

  #define mmap_init()  0

  # With Win32 VM, you cannot simply map a page of memory anywhere you want.
  # You first have to reserve address space before you can do that.
  # It's more programming, but it has the advantage that you cannot accidentally
  # overwrite some of the shared libraries or malloc regions. (If you try that,
  # VirtualAlloc(..,MEM_RESERVE,..) will return an error.)
  # This function reserves an address range for use with mmap_zeromap().
  # It tries to reserve the range [*map_addr,*map_endaddr). If this is not
  # possible and shrinkp is true, *map_addr is increased and *map_endaddr is
  # reduced as necessary.
  local int mmap_prepare (aint* map_addr, aint* map_endaddr, bool shrinkp)
  {
    var uintM map_len = *map_endaddr - *map_addr;
    var aint start_addr = round_down(*map_addr,0x10000);
    var aint end_addr = round_up(*map_addr+map_len,0x10000);
    if (shrinkp) {
      # Try to find the largest free address range subinterval of
      # [start_addr,end_addr).
      var MEMORY_BASIC_INFORMATION info;
      var aint largest_start_addr = start_addr;
      var uintM largest_len = 0;
      var aint addr = start_addr;
      while (VirtualQuery((void*)addr,&info,sizeof(info)) == sizeof(info)) {
        # Always info.BaseAddress = addr.
        addr = (aint)info.BaseAddress;
        var uintM len = (info.RegionSize >= end_addr-addr ? end_addr-addr : info.RegionSize);
        if ((info.State == MEM_FREE) && (len > largest_len)) {
          largest_start_addr = addr; largest_len = len;
        }
        if (info.RegionSize >= end_addr-addr)
          break;
        addr += info.RegionSize;
      }
      if (largest_len < 0x10000) {
        fprintf(stderr,GETTEXTL("Cannot reserve address range at 0x%lx ."),
                *map_addr);
        # DumpProcessMemoryMap();
        return -1;
      }
      *map_addr = start_addr = round_up(largest_start_addr,0x10000);
      *map_endaddr = end_addr = largest_start_addr + largest_len;
    }
    if (!VirtualAlloc((void*)start_addr,end_addr-start_addr,MEM_RESERVE,PAGE_NOACCESS/*dummy*/)) {
      var DWORD errcode = GetLastError();
      fprintf(stderr,GETTEXTL("Cannot reserve address range 0x%lx-0x%lx ."),
              start_addr,end_addr-1);
      errno_out(errcode);
      # DumpProcessMemoryMap();
      return -1;
    }
    #ifdef DEBUG_SPVW
    fprintf(stderr,"Reserved address range 0x%lx-0x%lx .\n",
            start_addr,end_addr-1);
    #endif
    return 0;
  }

  local int mmap_zeromap (void* map_addr, uintM map_len)
  {
    if (!VirtualAlloc(map_addr,map_len,MEM_COMMIT,PAGE_READWRITE)) {
      var DWORD errcode = GetLastError();
      fprintf(stderr,GETTEXTL("Cannot map memory to address 0x%lx ."),
              map_addr);
      errno_out(errcode);
      return -1; /* error */
    }
    return 0;
  }

  #if 0
  # This implementation, on top of MapViewOfFileEx(), has three severe flaws:
  # - It forces `map_addr' and `offset' to be aligned to 64 KB (not to the
  #   pagesize, 4KB, as indicated in the documentation), thus the mem files for
  #   SINGLEMAP_MEMORY get big.
  # - On an address range prepared with mmap_prepare(), MapViewOfFileEx()
  #   returns the error code ERROR_INVALID_ADDRESS. We would have to map the
  #   first part of each heap to the file and mmap_prepare() only the remainder
  #   of the heap. This would give problems once a heap shrinks too much:
  #   munmap() below wouldn't work.
  # - It doesn't work on Win95: MapViewOfFileEx() on Win95 cannot guarantee
  #   that it will be able to map at the desired address.
  local void* mmap_filemap (void* map_addr, uintM map_len, Handle fd,
                            off_t offset)
  {
    if (map_len==0)
      return map_addr;
    var HANDLE maphandle = CreateFileMapping(fd,NULL,PAGE_WRITECOPY,0,0,NULL);
    if (maphandle == NULL) {
      var DWORD errcode = GetLastError();
      fprintf(stderr,GETTEXTL("CreateFileMapping() failed."));
      errno_out(errcode);
      return (void*)(-1);
    }
    var void* resultaddr = MapViewOfFileEx(maphandle,FILE_MAP_COPY,0,
                                           (DWORD)offset,map_len,map_addr);
    if (resultaddr == NULL) {
      var DWORD errcode = GetLastError();
      fprintf(stderr,GETTEXTL("MapViewOfFileEx(addr=0x%x,off=0x%x) failed."),
              map_addr,offset);
      errno_out(errcode);
      return (void*)(-1);
    }
    if (resultaddr != map_addr) {
      fprintf(stderr,GETTEXTL("MapViewOfFileEx() returned 0x%x instead of 0x%x."),
              resultaddr,map_addr);
      fputs("\n",stderr);
      UnmapViewOfFile(resultaddr);
      return (void*)(-1);
    }
    return map_addr;
  }
  #endif

  # We need to implement munmap() ourselves.
  global int munmap (void* addr, size_t len)
  {
    if (!VirtualFree(addr,len,MEM_DECOMMIT)) {
      var DWORD errcode = GetLastError();
      fprintf(stderr,GETTEXTL("VirtualFree() failed."));
      errno_out(errcode);
      return -1;
    }
    return 0;
  }

  # We need to implement mprotect() ourselves.
  global int mprotect (void* addr, size_t len, int prot)
  {
    var DWORD oldprot;
    if (!VirtualProtect(addr,len,prot,&oldprot)) {
      var DWORD errcode = GetLastError();
      fprintf(stderr,GETTEXTL("VirtualProtect() failed."));
      errno_out(errcode);
      return -1;
    }
    return 0;
  }

#endif

#if defined(HAVE_MMAP_ANON) || defined(HAVE_MMAP_DEVZERO)

# We don't need both mmap() methods. One is sufficient.
#ifdef HAVE_MMAP_ANON
  #undef HAVE_MMAP_DEVZERO
#endif

#ifdef HAVE_MMAP_DEVZERO
  local int mmap_zero_fd; # open handle for /dev/zero
  # How to access /dev/zero: Sometimes /dev/zero has permissions 0644.
  # Therefore we can OPEN() it only with O_RDONLY instead of O_RDWR.
  # Therefore, in the mmap() call, we use MAP_PRIVATE instead of MAP_SHARED.
  #ifdef MAP_FILE
    #define map_flags  MAP_FILE | MAP_PRIVATE
  #else
    #define map_flags  MAP_PRIVATE
  #endif
#endif
#ifdef HAVE_MMAP_ANON
  #define mmap_zero_fd  -1 # any invalid handles works!
  #define map_flags  MAP_ANON | MAP_PRIVATE
#endif

  local void mmap_init_pagesize (void)
  {
    mmap_pagesize =
      #if (defined(UNIX_SUNOS5) || defined(UNIX_LINUX)) && defined(SPARC)
        # Normal SPARCs have PAGESIZE=4096, UltraSPARCs have PAGESIZE=8192.
        # For compatibility of the .mem files between the architectures,
        # choose the same value for both here.
        8192
      #elif defined(UNIX_IRIX) && defined(MIPS)
        # Normal MIPSs have pagesize=4096, the Onyx platform has it =16384.
        # For compatibility of the .mem files between the architectures,
        # choose the same value for both here.
        16384
      #elif defined(UNIX_LINUX) && defined(IA64)
        # The pagesize can be 4, 8, 16 or 64 KB.
        # For compatibility of the .mem files, choose always the same value.
        65536
      #elif defined(HAVE_GETPAGESIZE)
        getpagesize()
      #elif defined(UNIX_SUNOS5)
        # UNIX_SUNOS5 (Solaris < 2.5) has mmap(), but no getpagesize() !
        PAGESIZE # see <sys/param.h>
      #elif defined(UNIX_SINIX) && defined(MIPS)
        16384
      #elif defined(HAVE_SHM)
        SHMLBA # just a wild guess
      #else
        4096 # just another wild guess
      #endif
      ;
  }

  local int mmap_init (void)
  {
    #ifdef HAVE_MMAP_DEVZERO
    {
      var int fd = OPEN("/dev/zero",O_RDONLY,my_open_mask);
      if (fd<0) {
        fprintf(stderr,GETTEXTL("Cannot open <%s>."),"/dev/zero");
        errno_out(errno);
        return -1; /* error */
      }
      mmap_zero_fd = fd;
    }
    #endif
    return 0;
  }

  #define mmap_prepare(map_addr,map_endaddr,shrinkp)  0

  local int mmap_zeromap (void* map_addr, uintM map_len)
  {
    if ( (void*) mmap((void*)map_addr, /* wished address */
                      map_len, /* length */
                      PROT_READ_WRITE, /* access rights */
                      map_flags | MAP_FIXED, /* exactly at this address! */
                      mmap_zero_fd, 0) /* put empty pages */
         == (void*)(-1)) {
      fprintf(stderr,GETTEXTL("Cannot map memory to address 0x%lx ."),
              map_addr);
      errno_out(errno);
      return -1; /* error */
    }
    return 0;
  }

  #ifdef HAVE_MMAP
  local void* mmap_filemap (void* map_addr, uintM map_len, int fd, off_t offset)
  {
    return (void*) mmap((void*)map_addr,
                        map_len,
                        PROT_READ_WRITE,
                        MAP_FIXED | MAP_PRIVATE,
                        fd, offset
                       );
  }
  #endif

#endif

# -----------------------------------------------------------------------------

#endif
