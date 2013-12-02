# Support for MULTIMAP_MEMORY model.

# ------------------------------ Specification --------------------------------

# The operating system permits to use the same (virtual) memory areas under
# different addresses. There are some restrictions, however:
# - The mapping of addresses can be specified only for a memory page as a whole.
# - We need the address range only (not its contents), but nevertheless we have
#   malloc it and not give it back, because the address range shall remain
#   under our control. (We don't want subsequent malloc() calls to write data
#   into it.)

# The granularity of memory mapping, i.e. the length of a memory page.
# This is a power of two, usually 4096.
  local /* uintL */ aint map_pagesize;
# Initialize it.
# local void init_map_pagesize (void);

# Initialization:
# initmap() [MULTIMAP_MEMORY_VIA_SHM] or
# initmap(tmpdir) [MULTIMAP_MEMORY_VIA_FILE]
# Returns 0 is successful, -1 on error.

# Covers the range [map_addr,map_addr+map_len-1] with empty pages.
# (map_addr and map_len must be multiples of map_pagesize.)
# zeromap(map_addr,map_len)
# Returns 0 is successful, -1/errno on error.

# Covers the range [map_addr,map_addr+map_len-1] with pages, which shall
# be accessible at the typecodes enumerated in typecases.
# multimap(typecases,map_addr,map_len,save_flag);

# Number of possible typecodes.
# mm_types_count

# The list of typecodes satisfying !gcinvariant_type_p(type).
# MM_TYPECASES

# Combines a typecode and an address.
# combine(type,addr)

#ifdef HAVE_WORKING_MPROTECT
# Changes the memory protection of the range [map_addr,map_addr+map_len-1].
# mprotect(map_addr,map_len,PROT_{NONE,READ,READ_WRITE})
# Returns 0 is successful, -1/errno on error.
#endif

# Clean up and finish.
# exitmap();

# ------------------------------ Implementation -------------------------------

  #define mm_types_count  typecount

  #define MM_TYPECASES  \
    case_array: \
    case_closure: _case_structure _case_stream case_orecord: case_instance: \
    case_lrecord: \
    case_system: \
    case_bignum: case_ratio: case_ffloat: case_dfloat: case_lfloat: case_complex: \
    case_symbolflagged: case_cons:

  # Normal type+address combination.
  #define combine(type,addr)  ThePointer(type_pointer_object(type,addr))


#ifdef MULTIMAP_MEMORY_VIA_FILE

  #define init_map_pagesize()  \
    { map_pagesize = getpagesize(); }

  # Debug level for tempfile:
  #   0 = remove file immediately         "lisptemp.mem"
  #   1 = filename depends on process id  "lisptemp.mem.pid.1"
  #   2 = reuse file next time            "lisptemp.mem.1"
  #define TEMPFILE_DEBUG_LEVEL  0

  local char tempfilename[MAXPATHLEN]; # name of a temporary file
  local int zero_fd = -1; # handle of /dev/zero
  # access to /dev/zero: /dev/zero has sometimes permissions 0644. Hence,
  # OPEN() only with O_RDONLY instead of O_RDWR.
  # Hence MAP_PRIVATE instead of MAP_SHARED.

  local int initmap (const char* tmpdir)
  {
    # build up virtual memory mapping:
    # we need a temporary file.
    # tempfilename := (string-concat tmpdir "/" "lisptemp.mem")
    {
      var const char* ptr1 = tmpdir;
      var char* ptr2 = &tempfilename[0];
      while (!(*ptr1 == '\0')) { *ptr2++ = *ptr1++; }
      if (!((ptr2 > &tempfilename[0]) && (ptr2[-1] == '/')))
        *ptr2++ = '/';
      ptr1 = "lisptemp.mem";
      while (!(*ptr1 == '\0')) { *ptr2++ = *ptr1++; }
      #if (TEMPFILE_DEBUG_LEVEL > 0)
      *ptr2++ = '.';
      #if (TEMPFILE_DEBUG_LEVEL == 1)
      {
        unsigned int pid = getpid();
        *ptr2++ = ((pid >> 12) & 0x0f) + 'a';
        *ptr2++ = ((pid >> 8) & 0x0f) + 'a';
        *ptr2++ = ((pid >> 4) & 0x0f) + 'a';
        *ptr2++ = (pid & 0x0f) + 'a';
      }
      #endif
      *ptr2++ = '0';
      #endif
      *ptr2 = '\0';
    }
    {
      var int fd = OPEN("/dev/zero",O_RDONLY,my_open_mask);
      if (fd<0) {
        fprintf(stderr,GETTEXTL("Cannot open <%s>."),"/dev/zero");
        errno_out(errno);
        return -1; # error
      }
      zero_fd = fd;
    }
    return 0;
  }

  #ifdef HAVE_MSYNC
    typedef struct { void* mm_addr; uintM mm_len; } mmap_interval_t;
    local mmap_interval_t mmap_intervals[256]; # 256 is abundant.
    local mmap_interval_t* mmap_intervals_ptr = &mmap_intervals[0];
    local void remember_mmap_interval (void* map_addr, uintM map_len)
    {
      if (mmap_intervals_ptr == &mmap_intervals[256])
        abort();
      mmap_intervals_ptr->mm_addr = map_addr; mmap_intervals_ptr->mm_len = map_len;
      mmap_intervals_ptr++;
    }
    local void msync_mmap_intervals (void) {
      var mmap_interval_t* ptr = &mmap_intervals[0];
      while (ptr != mmap_intervals_ptr) {
        if (msync((void*)ptr->mm_addr,ptr->mm_len,MS_INVALIDATE) < 0) {
          fprintf(stderr,GETTEXTL("msync(0x%lx,0x%x,MS_INVALIDATE) failed."),
                  ptr->mm_addr, ptr->mm_len);
          errno_out(errno);
        }
        ptr++;
      }
    }
  #else
     #define remember_mmap_interval(map_addr,map_len)
     #define msync_mmap_intervals()
  #endif

  local int fdmap (int fd, void* map_addr, uintM map_len, int readonly, int shared, int remember)
  {
    if ( (void*) mmap((void*)map_addr, # wished address
                      map_len, # length
                      readonly ? PROT_READ : PROT_READ_WRITE, # access rights
                      (shared ? MAP_SHARED : 0) | MAP_FIXED, # exactly to this address!
                      fd, 0 # put file at position 0
                     )
         == (void*)(-1)
       ) {
      fprintf(stderr,GETTEXTL("Cannot map memory to address 0x%lx ."),
              map_addr);
      errno_out(errno);
      return -1; # error
    }
    #ifdef HAVE_MSYNC
    if (remember)
      remember_mmap_interval(map_addr,map_len);
    #endif
    return 0;
  }

  local int zeromap (void* map_addr, uintM map_len)
  {
    return fdmap(zero_fd,map_addr,map_len,false,false,false);
  }

  local int open_temp_fd (uintM map_len)
  {
    var int fd;
    #if (TEMPFILE_DEBUG_LEVEL > 0)
    tempfilename[strlen(tempfilename)-1]++;
    #endif
    #if (TEMPFILE_DEBUG_LEVEL <= 1)
    fd = OPEN(tempfilename,O_RDWR|O_CREAT|O_TRUNC|O_EXCL,my_open_mask);
    #else
    fd = OPEN(tempfilename,O_RDWR|O_CREAT,my_open_mask);
    #endif
    if (fd<0) {
      fprintf(stderr,GETTEXTL("Cannot open <%s>."),tempfilename);
      errno_out(errno);
      return -1; # error
    }
    #if (TEMPFILE_DEBUG_LEVEL == 0)
    # and hopefully make it inaccessible by deleting it:
    # (the operating system will delete the file only when at the end of
    # this process in _exit() a close(fd) is performed.)
    if ( unlink(tempfilename) <0) {
      fprintf(stderr,GETTEXTL("Cannot delete <%s>."),tempfilename);
      errno_out(errno);
      return -1; # error
    }
    #endif
    # check, if there is enough disk space:
    {
      #if HAVE_SYS_STATVFS_H
      var struct statvfs statbuf;
      if (!( fstatvfs(fd,&statbuf) <0))
      #else
      var struct statfs statbuf;
      if (!( fstatfs(fd,&statbuf) <0))
      #endif
        if (!(statbuf.f_bsize == (long)(-1)) && !(statbuf.f_bavail == (long)(-1))) {
          var uintM available = (uintM)(statbuf.f_bsize) * (uintM)(statbuf.f_bavail);
          if (available < map_len) {
            # there is likely too few disk space
            fprintf(stderr,GETTEXTL("** WARNING: ** Too little free disk space for <%s>."),tempfilename);
            fputs("\n",stderr);
            fprintf(stderr,GETTEXTL("Please restart LISP with less memory (option -m)."));
            fputs("\n",stderr);
          }
        }
    }
    # inflate to the size map_len:
    {
      var uintB dummy = 0;
      if (( lseek(fd,map_len-1,SEEK_SET) <0) || (!( full_write(fd,&dummy,1) ==1))) {
        fprintf(stderr,GETTEXTL("Cannot make <%s> long enough."),
                tempfilename);
        errno_out(errno);
        return -1; # error
      }
    }
    return fd;
  }

  #if !defined(MAP_MEMORY_TABLES)
    # copies the content of the interval [map_addr..map_addr+map_len-1] to the file.
    local int fdsave (int fd, void* map_addr, uintM map_len)
    {
      if (( lseek(fd,0,SEEK_SET) <0) || (!( full_write(fd,map_addr,map_len) == map_len))) {
        fprintf(stderr,GETTEXTL("Cannot fill <%s>."),tempfilename);
        errno_out(errno);
        return -1; # error
      }
      return 0;
    }
  #else
    #define fdsave(fd,map_addr,map_len)  0
  #endif

  local int close_temp_fd (int fd)
   {
    if ( CLOSE(fd) <0) {
      fprintf(stderr,GETTEXTL("Cannot close <%s>."),tempfilename);
      errno_out(errno);
      return -1; # error
    }
    return 0;
  }

  # procedure for multimap:
  # 1. open temporary file
    #define open_mapid(map_len)  open_temp_fd(map_len) # -> fd
  # 2. put file multimapped into the memory
    #define map_mapid(fd,map_addr,map_len,readonly)  fdmap(fd,map_addr,map_len,readonly,true,true)
  # 3. close file
  # (the operating system will close and delete the file only when at the end of
  # this process in _exit() an munmap() is performed.)
    #define close_mapid(fd)  close_temp_fd(fd)

  #define multimap1(type,typecases,mapid,map_addr,map_len)  \
    { switch (type) {    \
        typecases        \
          if ( map_mapid(mapid,combine(type,map_addr),map_len,false) <0) \
            goto no_mem; \
          break;         \
        default: break;  \
      }                  \
    }

  #define done_mapid(mapid,map_addr,map_len)  \
    if ( close_mapid(mapid) <0) \
      goto no_mem;
  #define exitmap()  \
    msync_mmap_intervals();                                         \
    if (zero_fd >= 0)                                               \
      if ( CLOSE(zero_fd) <0) {                                     \
        fprintf(stderr,GETTEXTL("Cannot close <%s>."),"/dev/zero"); \
        errno_out(errno);                                           \
      }

  #define multimap(typecases,map_addr,map_len,save_flag)  \
    { # open temporary file:                                        \
      var int mapid = open_mapid(map_len);                          \
      if (mapid<0) goto no_mem;                                     \
      if (save_flag) {                                              \
        if ( fdsave(mapid,(void*)map_addr,map_len) <0) goto no_mem; \
      }                                                             \
      # and put multimapped into the memory:                        \
      { var oint type;                                              \
        for (type=0; type < mm_types_count; type++) {               \
          multimap1(type,typecases,mapid,map_addr,map_len);         \
        }                                                           \
      }                                                             \
      # and poss. make publicly inaccessible:                       \
      done_mapid(mapid,map_addr,map_len);                           \
    }

#endif # MULTIMAP_MEMORY_VIA_FILE


#ifdef MULTIMAP_MEMORY_VIA_SHM

# build up virtual memory mapping via shared memory:

  #define init_map_pagesize()  \
    { map_pagesize = SHMLBA; }

  local int initmap (void)
  {
    #ifdef UNIX_LINUX
    {
      var struct shminfo shminfo;
      if ( shmctl(0,IPC_INFO,(struct shmid_ds *)&shminfo) <0)
        if (errno==ENOSYS) {
          fprintf(stderr,GETTEXTL("Recompile your operating system with SYSV IPC support."));
          fputs("\n",stderr);
          return -1; # error
        }
    }
    #endif
    return 0;
  }

  local int open_shmid (uintM map_len)
  {
    var int shmid = shmget(IPC_PRIVATE,map_len,0700|IPC_CREAT); # 0700 = 'Read/Write/Execute only for me'
    if (shmid<0) {
      fprintf(stderr,GETTEXTL("Cannot allocate private shared memory segment of size %d."),map_len);
      errno_out(errno);
      return -1; # error
    }
    return shmid;
  }

  #ifndef SHM_REMAP  # Only UNIX_LINUX needs SHM_REMAP in the shmflags
    #define SHM_REMAP  0
  #endif
  local int idmap (int shmid, void* map_addr, int shmflags)
  {
    if ( shmat(shmid,
               map_addr, # address
               shmflags # flags (default: read/write)
              )
         == (void*)(-1)) {
      fprintf(stderr,GETTEXTL("Cannot map shared memory to address 0x%lx."),
              map_addr);
      errno_out(errno);
      return -1; # error
    }
    return 0;
  }

  #if !defined(MAP_MEMORY_TABLES)
    # copies the content of the interval [map_addr..map_addr+map_len-1] into
    # the shared-memory-segment.
    local int shmsave (int shmid, void* map_addr, uintM map_len)
    {
      var void* temp_addr = shmat(shmid,
                                  0, # address: arbitrary
                                  0 # flags: need none
                                 );
      if (temp_addr == (void*)(-1)) {
        fprintf(stderr,GETTEXTL("%s: Cannot fill shared memory."),"shmat");
        errno_out(errno);
        return -1; # error
      }
      memcpy(temp_addr,map_addr,map_len);
      if (shmdt(temp_addr) < 0) {
        fprintf(stderr,GETTEXTL("%s: Cannot fill shared memory."),"shmdt");
        errno_out(errno);
        return -1; # error
      }
      return 0;
    }
  #else
    #define shmsave(shmid,map_addr,map_len)  0
  #endif

  local int close_shmid (int shmid)
  {
    if ( shmctl(shmid,IPC_RMID,NULL) <0) {
      fprintf(stderr,GETTEXTL("Cannot remove shared memory segment."));
      errno_out(errno);
      return -1; # error
    }
    return 0;
  }

  local int zeromap (void* map_addr, uintM map_len)
  {
    var int shmid = open_shmid(map_len);
    if (shmid<0)
      return -1; # error
    if (idmap(shmid,map_addr,0) < 0)
      return -1; # error
    return close_shmid(shmid);
  }

  # procedure for multimap:
  # 1. make shared-memory-region available
    #define open_mapid(map_len)  open_shmid(map_len) # -> shmid
  # 2. put shared-memory multimapped into the memory
    #define map_mapid(shmid,map_addr,map_len,flags)  idmap(shmid,map_addr,flags)
  # 3. make publicly inaccessible by deleting it:
  # (the operating system will delete the shared memory only when at the end
  # of this process in _exit() a munmap() is performed.)
    #define close_mapid(shmid)  close_shmid(shmid)

  #define multimap1(type,typecases,mapid,map_addr,map_len)  \
    { switch (type) {                                 \
        typecases                                  \
          if ( map_mapid(mapid, combine(type,map_addr), map_len, \
                         (type==0 ? SHM_REMAP : 0) \
                        )                          \
               <0                                  \
             )                                     \
            goto no_mem;                           \
          break;                                   \
        default: break;                            \
      }                                            \
    }

  #define done_mapid(mapid,map_addr,map_len)  \
    if ( close_mapid(mapid) <0) \
      goto no_mem;
  #define exitmap()

  #define multimap(typecases,total_map_addr,total_map_len,save_flag)  \
    { var uintM remaining_len = total_map_len;                                 \
      var aint map_addr = total_map_addr;                                      \
      do {                                                                     \
        var uintM map_len = (remaining_len > SHMMAX ? SHMMAX : remaining_len); \
        # open shared-memory-region:                                           \
        var int mapid = open_mapid(map_len);                                   \
        if (mapid<0) goto no_mem;                                              \
        if (save_flag && (map_addr==total_map_addr)) {                         \
          if ( shmsave(mapid,(void*)total_map_addr,total_map_len) <0)          \
            goto no_mem;                                                       \
        }                                                                      \
        # and put multimapped into the memory:                                 \
        { var oint type;                                                       \
          for (type=0; type < mm_types_count; type++) {                        \
            multimap1(type,typecases,mapid,map_addr,map_len);                  \
          }                                                                    \
        }                                                                      \
        # and poss. make publicly inaccessible:                                \
        done_mapid(mapid,map_addr,map_len);                                    \
        map_addr += map_len; remaining_len -= map_len;                         \
      } until (remaining_len==0);                                              \
    }

#endif # MULTIMAP_MEMORY_VIA_SHM
