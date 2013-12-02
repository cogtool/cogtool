# Support for SINGLEMAP_MEMORY and TRIVIALMAP_MEMORY model.

# ------------------------------ Specification ---------------------------------

# The operating system permits to add memory to arbitrary addresses, which
# behaves like memory created through malloc().

# The granularity of memory mapping, i.e. the length of a memory page.
# This is a power of two, usually 4096.
  local /* uintL */ aint map_pagesize;
# Initialize it.
# local void init_map_pagesize (void);

# Initialization:
# initmap()
# Returns 0 is successful, -1 on error.

# Covers the range [map_addr,map_addr+map_len-1] with empty pages.
# (map_addr and map_len must be multiples of map_pagesize.)
# zeromap(map_addr,map_len)
# Returns 0 is successful, -1/errno on error.

# Reserves an address range for use with mmap_zeromap().
# local int prepare_zeromap (uintP* map_addr, uintP* map_endaddr, bool shrinkp);

#ifdef HAVE_MMAP
# Fill a memory range [map_addr,map_addr+map_len-1] with pages mapped in from
# file fd starting at position offset.
# map_addr and map_len must be multiples of mmap_pagesize.
  local void* filemap (void* map_addr, uintM map_len, int fd, off_t offset)
;
#endif

# ------------------------------ Implementation --------------------------------

  #define init_map_pagesize()  \
    { map_pagesize = mmap_pagesize; }

  #define initmap()  mmap_init()

  #define zeromap  mmap_zeromap
  #define prepare_zeromap  mmap_prepare

#ifdef HAVE_MMAP
  #define filemap  mmap_filemap
#endif
