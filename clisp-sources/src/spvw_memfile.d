/* Saving and loading of memory images. */

/* --------------------------- Specification ---------------------------- */

/* UP: Saves a memory image on disk.
 savemem(stream);
 > object stream: open file output stream
 > bool exec_p: should the result include runtime?
 As a side effect, the stream is closed.
 can trigger GC */
global maygc void savemem (object stream, bool exec_p);

/* UP: Restores a memory image from disk.
 loadmem(filename);
 This overwrites all Lisp data. */
local void loadmem (const char* filename);

/* --------------------------- Implementation --------------------------- */

/* Flags, that influence the format of a MEM-file: */
local const uint32 memflags =
  /* typecode allocation: */
 #ifdef WIDE
  bit(0) |
 #endif
 #ifdef TYPECODES
  bit(1) |
 #endif
 #if 0 /* defined(STANDARD_TYPECODES) */
  bit(2) |
 #endif
 #if 0 /* defined(PACKED_TYPECODES) */
  bit(3) |
 #endif
 #if 0 /* defined(SEVENBIT_TYPECODES) */
  bit(4) |
 #endif
 #if 0 /* defined(SIXBIT_TYPECODES) */
  bit(5) |
 #endif
 #ifdef case_structure
  bit(6) |
 #endif
 #ifdef case_stream
  bit(7) |
 #endif
  /* coding of numbers: */
 #ifdef FAST_FLOAT
  bit(8) |
 #endif
 #ifdef FAST_DOUBLE
  bit(9) |
 #endif
  /* coding of streams: */
 #if 1 /* defined(STRM_WR_SS) */
  bit(10) |
 #endif
 #if (SIZEOF_OFF_T > 4)
  bit(11) |
 #endif
  /* coding of strmtype: */
 #if 1 /* defined(HANDLES) */
  bit(12) |
 #endif
 #ifdef KEYBOARD
  bit(13) |
 #endif
 #ifdef SCREEN
  bit(14) |
 #endif
 #ifdef PRINTER
  bit(15) |
 #endif
 #ifdef PIPES
  bit(16) |
 #endif
 #ifdef X11SOCKETS
  bit(17) |
 #endif
 #ifdef GENERIC_STREAMS
  bit(18) |
 #endif
 #ifdef SOCKET_STREAMS
  bit(19) |
 #endif
 #ifdef UNICODE
  bit(20) |
 #endif
  0;

/* Maximum length of (machine-instance) return value: */
#define DUMPHOST_LEN  (64+45+3)  /* 64 for host name, 45 for IPv6 address */

/* Format: */
/* a header: */
typedef struct {
  uintL _magic; /* recognition */
 #define memdump_magic  0x70768BD2UL
  uint32 _memflags;
  oint _oint_type_mask;
  oint _oint_addr_mask;
 #ifdef TYPECODES
  tint _cons_type, _complex_type, _symbol_type, _system_type;
 #endif
  uintC _varobject_alignment;
  uintC _hashtable_length;
  uintC _pathname_length;
  uintC _intDsize;
  uintC _module_count;
  uintL _module_names_size;
  uintC _fsubr_anz;
  uintC _pseudofun_anz;
  uintC _symbol_anz;
  uintL _page_alignment;
  aint _subr_tab_addr;
  aint _symbol_tab_addr;
 #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
  aint _mem_varobjects_start;
  aint _mem_varobjects_end;
  aint _mem_conses_start;
  aint _mem_conses_end;
 #endif
 #ifndef SPVW_MIXED_BLOCKS_OPPOSITE
  uintC _heapcount;
 #endif
  uintL _dumptime;
  char _dumphost[DUMPHOST_LEN+1];
} memdump_header_t;
/* then the module names,
 then fsubr_tab, pseudofun_tab, symbol_tab,
 and for each module subr_addr, subr_anz, object_anz, subr_tab, object_tab, */
#ifdef SPVW_MIXED_BLOCKS_OPPOSITE
/* then the objects of variable length
 (between mem.varobjects.heap_start and mem.varobjects.heap_end),
 then the conses (between mem.conses.heap_start and mem.conses.heap_end). */
#else
  #if defined(SPVW_PURE_BLOCKS) || defined(SPVW_MIXED_BLOCKS_STAGGERED)
    /* then for each heap (block) the start- and end address, */
  #endif
  #ifdef SPVW_PAGES
   /* SPVW_PAGES: then for each heap the number of pages,
      then for each heap and for each page of the heap
      the start- and end address, */
  #endif
typedef struct {
  aint _page_start;
  aint _page_end;
} memdump_page_t;
  #if defined(SPVW_PURE_BLOCKS) && defined(GENERATIONAL_GC)
 /* then for each heap the length of physpages,
    then for each heap the complete physpages-array, */
typedef struct {
  gcv_object_t* continued_addr;
  uintC continued_count;
  aint firstobject;
} memdump_physpage_state_t;
  #endif
  /* then the content of the pages in the same order. */
  #ifdef SPVW_PURE_BLOCKS
   /* Finally, the addresses of all objects within the heaps that
      have to be updated by loadmem_update(), the addresses of the
      hashtables that have to be marked with set_ht_invalid(), the addresses
      of the foreign-pointers that have to be marked with mark_fp_invalid(),
      the addresses of the Fsubrs that have to be relocated with
      loadmem_update_fsubr(). But beforehand, their numbers.
      (That is redundant, but reduces the startup times.) */
typedef struct {
  uintL reloccount;
  uintL htcount;
  uintL fpcount;
  uintL fscount;
} memdump_reloc_header_t;
  #endif
#endif

/* page_alignment = Alignment for the page contents in the file. */
#if ((defined(SPVW_PURE_BLOCKS) && defined(SINGLEMAP_MEMORY)) || (defined(SPVW_MIXED_BLOCKS_STAGGERED) && defined(TRIVIALMAP_MEMORY))) && defined(HAVE_MMAP)
  #define page_alignment  map_pagesize
  #define WRITE_page_alignment(position)                                \
    do {                                                                \
      var uintL aligncount = (uintL)(-(position)) % page_alignment;     \
      if (aligncount > 0) { /* get a piece of zeroed memory: */         \
        var DYNAMIC_ARRAY(zeroes,uintB,aligncount);                     \
        var uintB* ptr = &zeroes[0];                                    \
        var uintL count;                                                \
        dotimespL(count,aligncount, { *ptr++ = 0; } );                  \
        /* and write: */                                                \
        WRITE(&zeroes[0],aligncount);                                   \
        FREE_DYNAMIC_ARRAY(zeroes);                                     \
      }                                                                 \
    } while(0)
  #define READ_page_alignment(position)                                 \
    do {                                                                \
      var uintL aligncount = (uintL)(-(position)) % page_alignment;     \
      if (aligncount > 0) {                                             \
        var DYNAMIC_ARRAY(dummy,uintB,aligncount);                      \
        READ(&dummy[0],aligncount);                                     \
        FREE_DYNAMIC_ARRAY(dummy);                                      \
      }                                                                 \
    } while(0)
#else
  #define page_alignment  1
  #define WRITE_page_alignment(position)
  #define READ_page_alignment(position)
#endif

#if defined(UNIX)
  #define CLOSE_HANDLE CLOSE
#elif defined(WIN32_NATIVE)
  #define CLOSE_HANDLE CloseHandle
#else
  #error define CLOSE_HANDLE for your platform
#endif

#define WRITE(buf,len)                                                  \
  do {                                                                  \
    begin_system_call();                                                \
    { var ssize_t ergebnis = full_write(handle,(void*)buf,len);         \
      if (ergebnis != (ssize_t)(len)) {                                 \
        end_system_call();                                              \
        builtin_stream_close(&STACK_0,0);                               \
        if (ergebnis<0) /* error occurred? */                           \
          { OS_file_error(TheStream(STACK_0)->strm_file_truename); }    \
        /* FILE-ERROR slot PATHNAME */                                  \
        pushSTACK(TheStream(STACK_0)->strm_file_truename);              \
        fehler(file_error,GETTEXT("disk full"));                        \
      }                                                                 \
    }                                                                   \
    end_system_call();                                                  \
  } while(0)

/* write the executable into the handle */
local Handle open_filename (const char* filename);
local void find_memdump (Handle fd);
/* the size of the runtime executable for executable dumping
   == the start of memory image in the executable */
static size_t mem_start = (size_t)-1;
static bool mem_searched = false; /* have we looked for memdump already */
static void savemem_with_runtime (Handle handle) {
  var char *executable_name = get_executable_name();
  var char buf[BUFSIZ];
  begin_system_call();
  var Handle runtime = open_filename(executable_name);
  /* if we did not look for memory image in the executable yet, do it now!
     we want to avoid this scenario:
     $ clisp -x '(saveinitmem "foo" :executable t)'
     $ ./foo -M lispinit.mem -x '(saveinitmem "bar" :executable t)'
     bar should not include 2 images, but foo received the -M option
     and thus did not call loadmem_from_executable(),
     so mem_searched is false and mem_start is 0,
     so we need to call find_memdump() now
     so that bar will get just one image */
  if (!mem_searched) {
    find_memdump(runtime);      /* search for memdump_header_t */
    lseek(runtime,0,SEEK_SET);  /* reset position */
  } /* now:  mem_searched == true */
  if (mem_start != (size_t)-1) { /* ==> have an image - cut it off */
    var uintL remains = mem_start;
    while (remains > 0) {
      var ssize_t res = full_read(runtime,(void*)buf,BUFSIZ);
      end_system_call();
      if (res <= 0) {
        builtin_stream_close(&STACK_0,0);
        if (res < 0) /* error occurred? */
          OS_file_error(TheStream(STACK_0)->strm_file_truename);
        /* FILE-ERROR slot PATHNAME */
        pushSTACK(asciz_to_string(executable_name,O(pathname_encoding)));
        pushSTACK(fixnum(remains));
        fehler(file_error,GETTEXT("runtime too small (~S bytes missing)"));
      }
      var uintL len = (remains > res ? res : remains);
      remains -= len;
      WRITE(buf,len);
      begin_system_call();
    }
  } else { /* mem_start == -1 ==> no memory image in the executable,
              just copy everything */
    mem_start = 0;
    while (1) {
      var ssize_t res = full_read(runtime,(void*)buf,BUFSIZ);
      if (res == 0) break;
      end_system_call();
      if (res < 0) {
        builtin_stream_close(&STACK_0,0);
        OS_file_error(TheStream(STACK_0)->strm_file_truename);
      }
      WRITE(buf,res);
      mem_start += res;
      begin_system_call();
    }
  }
#if defined(UNIX) && defined(HAVE_FCHMOD)
  { /* make the saved image executable */
    var mode_t mode = 0;
    var struct stat st;
    if (fstat(handle,&st) == 0) mode = st.st_mode;
    if (fstat(runtime,&st) == 0) mode |= st.st_mode;
    fchmod(handle,mode);
  }
#endif
  CLOSE_HANDLE(runtime); end_system_call();
}

/* fill the header's constant slots, excluding _dumptime & _dumphost
 > memdump_header_t *header: filled
 return the total size of all module names */
local uintL fill_memdump_header (memdump_header_t *header) {
  var uintL module_names_size;
  header->_magic = memdump_magic;
  header->_memflags = memflags;
  header->_oint_type_mask = oint_type_mask;
  header->_oint_addr_mask = oint_addr_mask;
 #ifdef TYPECODES
  header->_cons_type    = cons_type;
  header->_complex_type = complex_type;
  header->_symbol_type  = symbol_type;
  header->_system_type  = system_type;
 #endif
  header->_varobject_alignment = varobject_alignment;
  header->_hashtable_length = hashtable_length;
  header->_pathname_length = pathname_length;
  header->_intDsize = intDsize;
  header->_module_count = module_count;
  {
    var module_t* module;
    module_names_size = 0;
    for_modules(all_modules, {
      module_names_size += asciz_length(module->name)+1;
    });
    module_names_size = round_up(module_names_size,varobject_alignment);
  }
  header->_module_names_size = module_names_size;
  header->_fsubr_anz     = fsubr_anz;
  header->_pseudofun_anz = pseudofun_anz;
  header->_symbol_anz    = symbol_anz;
  header->_page_alignment = page_alignment;
  header->_subr_tab_addr   = (aint)(&subr_tab);
  header->_symbol_tab_addr = (aint)(&symbol_tab);
 #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
  #if !defined(GENERATIONAL_GC)
  header->_mem_varobjects_start = mem.varobjects.heap_start;
  header->_mem_varobjects_end   = mem.varobjects.heap_end;
  header->_mem_conses_start     = mem.conses.heap_start;
  header->_mem_conses_end       = mem.conses.heap_end;
  #else /* defined(GENERATIONAL_GC) */
  header->_mem_varobjects_start = mem.varobjects.heap_gen0_start;
  header->_mem_varobjects_end   = mem.varobjects.heap_gen0_end;
  header->_mem_conses_start     = mem.conses.heap_gen0_start;
  header->_mem_conses_end       = mem.conses.heap_gen0_end;
  #endif
 #endif
 #ifndef SPVW_MIXED_BLOCKS_OPPOSITE
  header->_heapcount = heapcount;
 #endif
  return module_names_size;
}

/* UP, stores the memory image on disk
 savemem(stream);
 > object stream: open File-Output-Stream, will be closed
 > bool exec_p: should the result include runtime?
 can trigger GC */
global maygc void savemem (object stream, bool exec_p)
{ /* We need the stream only because of the handle provided by it.
     In case of an error we have to close it (the caller makes no
     WITH-OPEN-FILE, but only OPEN). Hence, the whole stream is passed
     to us, so that we can close it. */
  var Handle handle = TheHandle(TheStream(stream)->strm_buffered_channel);
  pushSTACK(stream); /* save stream */
  /* GET-UNIVERSAL-TIME and MACHINE-INSTANCE cons,
     so they should be called before gar_col() */
  funcall(L(get_universal_time),0);
  var uintL universal_time = I_to_UL(value1);
  funcall(L(machine_instance),0);
  var char hostname[DUMPHOST_LEN+1];
  memset(hostname,'\0',DUMPHOST_LEN+1);
  if (!nullp(value1)) {
    with_string_0(value1,Symbol_value(S(utf_8)),host,{
      strncpy(hostname,host,DUMPHOST_LEN);
    });
  }
  /* execute one GC first: */
  gar_col();
  if (exec_p) savemem_with_runtime(handle);
  /* write basic information: */
  var memdump_header_t header;
  var uintL module_names_size = fill_memdump_header(&header);
  header._dumptime = universal_time;
  memcpy(&header._dumphost[0],&hostname[0],DUMPHOST_LEN+1);
  WRITE(&header,sizeof(header));
  { /* write module name: */
    var DYNAMIC_ARRAY(module_names_buffer,char,module_names_size);
    var char* ptr2 = &module_names_buffer[0];
    var module_t* module;
    var uintC count;
    for_modules(all_modules, {
      var const char* ptr1 = module->name;
      while ((*ptr2++ = *ptr1++) != '\0') ;
    });
    dotimesC(count,&module_names_buffer[module_names_size] - ptr2, {
      *ptr2++ = 0;
    });
    WRITE(module_names_buffer,module_names_size);
    FREE_DYNAMIC_ARRAY(module_names_buffer);
  }
  /* write fsubr_tab, pseudofun_tab, symbol_tab: */
  WRITE(&fsubr_tab,sizeof(fsubr_tab));
  WRITE(&pseudofun_tab,sizeof(pseudofun_tab));
  WRITE(&symbol_tab,sizeof(symbol_tab));
  { /* write for each module subr_addr, subr_anz, object_anz,
       subr_tab, object_tab: */
    var module_t* module;
    for_modules(all_modules, {
      WRITE(&module->stab,sizeof(subr_t*));
      WRITE(module->stab_size,sizeof(uintC));
      WRITE(module->otab_size,sizeof(uintC));
      WRITE(module->stab,*module->stab_size*sizeof(subr_t));
      WRITE(module->otab,*module->otab_size*sizeof(gcv_object_t));
    });
  }
 #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
  { /* write objects of variable length: */
    var uintM len = header._mem_varobjects_end - header._mem_varobjects_start;
    WRITE(header._mem_varobjects_start,len);
  }
  { /* write conses: */
    var uintM len = header._mem_conses_end - header._mem_conses_start;
    WRITE(header._mem_conses_start,len);
  }
 #endif
 #ifndef SPVW_MIXED_BLOCKS_OPPOSITE
  #ifdef SPVW_PAGES
  {
    var uintL heapnr;
    for (heapnr=0; heapnr<heapcount; heapnr++) {
      var uintC pagecount = 0;
      map_heap(mem.heaps[heapnr],page, { pagecount++; } );
      WRITE(&pagecount,sizeof(pagecount));
    }
  }
  #endif
  {
    var uintL heapnr;
    for (heapnr=0; heapnr<heapcount; heapnr++) {
     #if !defined(GENERATIONAL_GC)
      map_heap(mem.heaps[heapnr],page, {
        var memdump_page_t _page;
        _page._page_start = page->page_start;
        _page._page_end = page->page_end;
        WRITE(&_page,sizeof(_page));
      });
     #else /* defined(GENERATIONAL_GC) */
      var Heap* heap = &mem.heaps[heapnr];
      var memdump_page_t _page;
      _page._page_start = heap->heap_gen0_start;
      _page._page_end = heap->heap_gen0_end;
      WRITE(&_page,sizeof(_page));
     #endif
    }
  }
  #if defined(SPVW_PURE_BLOCKS) && defined(GENERATIONAL_GC)
  {
    var uintL numphyspages[heapcount];
    var uintL heapnr;
    for (heapnr=0; heapnr<heapcount; heapnr++) {
      var Heap* heap = &mem.heaps[heapnr];
      numphyspages[heapnr] =
        (heap->physpages==NULL ? 0 :
         (((heap->heap_gen0_end + (physpagesize-1)) & -physpagesize)
          - (heap->heap_gen0_start & -physpagesize)
          ) >> physpageshift);
    }
    WRITE(&numphyspages,sizeof(numphyspages));
    for (heapnr=0; heapnr<heapcount; heapnr++)
      if (numphyspages[heapnr] > 0) {
        var uintL count = numphyspages[heapnr];
        var Heap* heap = &mem.heaps[heapnr];
        var physpage_state_t* physpages = heap->physpages;
        var DYNAMIC_ARRAY(_physpages,memdump_physpage_state_t,count);
        var uintL i;
        for (i=0; i<count; i++) {
          _physpages[i].continued_addr  = physpages[i].continued_addr;
          _physpages[i].continued_count = physpages[i].continued_count;
          _physpages[i].firstobject     = physpages[i].firstobject;
        }
        WRITE(_physpages,count*sizeof(memdump_physpage_state_t));
        FREE_DYNAMIC_ARRAY(_physpages);
      }
  }
  #endif
  #if (defined(SPVW_PURE_BLOCKS) && defined(SINGLEMAP_MEMORY)) || (defined(SPVW_MIXED_BLOCKS_STAGGERED) && defined(TRIVIALMAP_MEMORY))
   #if defined(HAVE_MMAP) /* else, page_alignment is = 1, anyway */
  { /* put alignment into practice: */
    begin_system_call();
    var off_t ergebnis = lseek(handle,0,SEEK_CUR); /* fetch file-position */
    end_system_call();
    if (ergebnis<0) { builtin_stream_close(&STACK_0,0); OS_file_error(TheStream(STACK_0)->strm_file_truename); } /* error? */
    WRITE_page_alignment(ergebnis);
  }
   #endif
  #endif
  {
    var uintL heapnr;
    for (heapnr=0; heapnr<heapcount; heapnr++) {
      var uintM misaligned = 0;
     #if ((defined(SPVW_PURE_BLOCKS) && defined(SINGLEMAP_MEMORY)) || (defined(SPVW_MIXED_BLOCKS_STAGGERED) && defined(TRIVIALMAP_MEMORY))) && defined(HAVE_MMAP) && varobjects_misaligned
      if (is_varobject_heap(heapnr)) {
        var uintB zeroes[varobjects_misaligned];
        var uintB* ptr = &zeroes[0];
        doconsttimes(varobjects_misaligned, { *ptr++ = 0; } );
        /* write zeroes: */
        WRITE(&zeroes[0],varobjects_misaligned);
        misaligned = varobjects_misaligned;
      }
     #endif
     #if !defined(GENERATIONAL_GC)
      map_heap(mem.heaps[heapnr],page, {
        var uintM len = page->page_end - page->page_start;
        WRITE(page->page_start,len);
        WRITE_page_alignment(misaligned+len);
      });
     #else /* defined(GENERATIONAL_GC) */
      var Heap* heap = &mem.heaps[heapnr];
      var uintM len = heap->heap_gen0_end - heap->heap_gen0_start;
      WRITE(heap->heap_gen0_start,len);
      WRITE_page_alignment(misaligned+len);
     #endif
    }
  }
  #ifdef SPVW_PURE_BLOCKS
  { /* write relocations:
     (only frame-pointers, subr, machine must be relocated, and
     hashtables and fpointers must be marked, see
     update_varobjects(), update_record(), loadmem_update().) */
    var memdump_reloc_header_t rheader;
    rheader.reloccount = 0;
    rheader.htcount = 0;
    rheader.fpcount = 0;
    rheader.fscount = 0;
    #if !defined(GENERATIONAL_GC)
     #define update_conspage  update_conspage_normal
     #define update_page  update_page_normal
    #else /* defined(GENERATIONAL_GC) */
     #define update_conspage(page)  /* ignores page, uses heapnr */ \
      do { var aint objptr = mem.heaps[heapnr].heap_gen0_start;         \
        var aint objptrend = mem.heaps[heapnr].heap_gen0_end;           \
        /* update all pointers in the (new) CONS-region */              \
        /* start <= address < end: */                                   \
        while (objptr != objptrend) {                                   \
          update((gcv_object_t*)objptr);                                \
          objptr += sizeof(gcv_object_t);                               \
          update((gcv_object_t*)objptr);                                \
          objptr += sizeof(gcv_object_t);                               \
        }} while(0)
     #define update_page(page,updater)  /* ignores page, uses heapnr */ \
      do { var aint ptr = mem.heaps[heapnr].heap_gen0_start;            \
        var aint ptrend = mem.heaps[heapnr].heap_gen0_end;              \
        /* traverse all objects with address >=ptr, <ptrend : */        \
        while (ptr != ptrend) { /* until ptr has reached the end */     \
          /* traverse next object with address ptr (< ptrend) : */      \
          updater(typecode_at(ptr)); /* and advance */                  \
        }} while(0)
    #endif
    #define update_hashtable_invalid  true
    #define update_unrealloc  false
    #define update_ss_unrealloc(obj)
    #define update_in_unrealloc(obj)
    #ifdef FOREIGN
     #define update_fpointer_invalid  true
    #else
     #define update_fpointer_invalid  false
    #endif
    #define update_fsubr_function  true
    #define update(objptr)                                              \
      do { switch (mtypecode(*(gcv_object_t*)objptr)) {                 \
        case_system:                                                    \
          if (wbit_test(as_oint(*(gcv_object_t*)objptr),0+oint_addr_shift)) \
            break;                                                      \
        case_subr:                                                      \
        case_machine:                                                   \
          rheader.reloccount++;                                         \
        default:                                                        \
          break;                                                        \
        }} while(0)
    #define update_ht_invalid(obj)  rheader.htcount++;
    #define update_fp_invalid(obj)  rheader.fpcount++;
    #define update_fs_function(obj)  rheader.fscount++;
    update_conses();
    update_varobjects();
    #undef update_fs_function
    #undef update_fp_invalid
    #undef update_ht_invalid
    #undef update
    var DYNAMIC_ARRAY(relocbuf,gcv_object_t*,rheader.reloccount);
    var DYNAMIC_ARRAY(htbuf,Hashtable,rheader.htcount);
    var DYNAMIC_ARRAY(fpbuf,Record,rheader.fpcount);
    var DYNAMIC_ARRAY(fsbuf,Fsubr,rheader.fscount);
    var gcv_object_t** relocbufptr = &relocbuf[0];
    var Hashtable* htbufptr = &htbuf[0];
    var Record* fpbufptr = &fpbuf[0];
    var Fsubr* fsbufptr = &fsbuf[0];
    #define update(objptr)                                                \
      do {                                                                \
        switch (mtypecode(*(gcv_object_t*)objptr)) {                      \
          case_system:                                                    \
            if (wbit_test(as_oint(*(gcv_object_t*)objptr),0+oint_addr_shift)) \
              break;                                                      \
          case_subr:                                                      \
          case_machine:                                                   \
            *relocbufptr++ = (gcv_object_t*)objptr;                       \
          default:                                                        \
            break;                                                        \
        }                                                                 \
      } while(0)
    #define update_ht_invalid(obj)  *htbufptr++ = (obj);
    #define update_fp_invalid(obj)  *fpbufptr++ = (obj);
    #define update_fs_function(obj)  *fsbufptr++ = (obj);
    update_conses();
    update_varobjects();
    #undef update_fs_function
    #undef update_fp_invalid
    #undef update_ht_invalid
    #undef update
    #undef update_fsubr_function
    #undef update_fpointer_invalid
    #undef update_in_unrealloc
    #undef update_ss_unrealloc
    #undef update_unrealloc
    #undef update_hashtable_invalid
    #undef update_page
    #undef update_conspage
    WRITE(&rheader,sizeof(rheader));
    WRITE(&relocbuf[0],rheader.reloccount*sizeof(gcv_object_t*));
    WRITE(&htbuf[0],rheader.htcount*sizeof(Hashtable));
    WRITE(&fpbuf[0],rheader.fpcount*sizeof(Record));
    WRITE(&fsbuf[0],rheader.fscount*sizeof(Fsubr));
    FREE_DYNAMIC_ARRAY(fsbuf);
    FREE_DYNAMIC_ARRAY(fpbuf);
    FREE_DYNAMIC_ARRAY(htbuf);
    FREE_DYNAMIC_ARRAY(relocbuf);
  }
  #endif
 #endif
  if (exec_p) WRITE(&mem_start,sizeof(size_t)); /* see find_memdump() */
  else { size_t tmp = (size_t)-1; WRITE(&tmp,sizeof(size_t)); }
  /* close stream (stream-buffer is unchanged, but thus also the
     handle at the operating system is closed): */
  builtin_stream_close(&STACK_0,0);
  skipSTACK(1);
}
#undef WRITE

/* UP, loads memory image from disk
 loadmem(filename);
 destroys all LISP-data. */
local void loadmem_from_handle (Handle handle, const char* filename);
/* update of an object in memory: */
#ifdef SPVW_MIXED_BLOCKS_OPPOSITE
local var oint offset_varobjects_o;
local var oint offset_conses_o;
#endif
#ifdef SPVW_MIXED_BLOCKS_STAGGERED
local var oint offset_heaps_o[heapcount];
#define offset_varobjects_o  offset_heaps_o[0]
#define offset_conses_o      offset_heaps_o[1]
#endif
#ifdef SINGLEMAP_MEMORY_RELOCATE
local var oint offset_heaps_o[heapcount];
local var bool offset_heaps_all_zero;
#endif
#ifdef SPVW_PAGES
typedef struct { aint old_page_start; oint offset_page_o; } offset_pages_t;
local var offset_pages_t *offset_pages;
#define addr_mask  ~(((oint_addr_mask>>oint_addr_shift) & ~ (wbitm(oint_addr_relevant_len)-1)) << addr_shift) /* mostly = ~0 */
#define pagenr_of(addr)  floor(addr,min_page_size_brutto)
#define offset_pages_len  (pagenr_of((oint)((wbitm(oint_addr_relevant_len)-1)<<addr_shift))+1)
#endif
#if !defined(SINGLEMAP_MEMORY)
local var oint offset_symbols_o;
#if !defined(MULTIMAP_MEMORY_SYMBOL_TAB)
local var oint old_symbol_tab_o;
#endif
#endif
typedef struct { oint low_o; oint high_o; oint offset_o; } offset_subrs_t;
local var offset_subrs_t* offset_subrs;
local var uintC offset_subrs_anz;
local var struct fsubr_tab_ old_fsubr_tab;
local var struct pseudofun_tab_ old_pseudofun_tab;
local void loadmem_update (gcv_object_t* objptr)
{
 #ifdef TYPECODES
  switch (mtypecode(*objptr))
 #else
    if (orecordp(*objptr)) {
      goto case_record;
    } else if (consp(*objptr)) {
      goto case_cons;
    } else if (immsubrp(*objptr)) {
      goto case_subr;
    } else if (machinep(*objptr)) {
      goto case_machine;
    } else {
      return;
    }
  switch (0)
 #endif
  {
    #ifdef TYPECODES
    case_symbol: /* symbol */
     #ifndef SPVW_PURE_BLOCKS
      #if !defined(MULTIMAP_MEMORY_SYMBOL_TAB)
      if (as_oint(*objptr) - old_symbol_tab_o
          < ((oint)sizeof(symbol_tab)<<(oint_addr_shift-addr_shift))) {
        /* symbol from symbol_tab */
        *objptr = as_object(as_oint(*objptr) + offset_symbols_o); break;
      }
      #else
      if (as_oint(*objptr) - (oint)(&symbol_tab)
          < (sizeof(symbol_tab)<<(oint_addr_shift-addr_shift))) {
        /* symbol from symbol_tab experiences no displacement */
        break;
      }
      #endif
      /* other symbols are objects of variable length. */
     #endif
    #endif
    case_record:
     #ifdef HEAPCODES
      if (as_oint(*objptr) - old_symbol_tab_o
          < ((oint)sizeof(symbol_tab)<<(oint_addr_shift-addr_shift))) {
        /* symbol from symbol_tab */
        *objptr = as_object(as_oint(*objptr) + offset_symbols_o); break;
      }
     #endif
     #ifdef LINUX_NOEXEC_HEAPCODES
      { /* Test for a SUBR in one of the modules. */
        var oint addr = as_oint(*objptr);
        var offset_subrs_t* ptr = offset_subrs;
        var uintC count;
        dotimespC(count,offset_subrs_anz, {
          if ((ptr->low_o <= addr) && (addr < ptr->high_o)) {
            *objptr = as_object(as_oint(*objptr) + ptr->offset_o);
            goto found_subr;
          }
          ptr++;
        });
      }
     #endif
    #ifdef TYPECODES
    case_array:
    case_bignum:
    #ifndef IMMEDIATE_FFLOAT
    case_ffloat:
    #endif
    case_dfloat:
    case_lfloat:
    #endif
      /* object of variable length */
     #ifdef SPVW_MIXED_BLOCKS
      *objptr = as_object(as_oint(*objptr) + offset_varobjects_o); break;
     #endif
    case_pair:
      /* Two-Pointer-Object */
     #ifdef SPVW_MIXED_BLOCKS
      *objptr = as_object(as_oint(*objptr) + offset_conses_o); break;
     #endif
     #ifdef SPVW_PAGES
      {
        var aint addr = /* address */
         #ifdef TYPECODES
          upointer(*(gcv_object_t*)objptr);
         #else
          as_oint(*(gcv_object_t*)objptr);
         #endif
          /* As pages have a minimal length, so the start addresses
             of different pages have at least a distance of
             min_page_size_brutto, it is quite simple, to conclude from the
             address to the page: */
          var uintL pagenr = pagenr_of(addr & addr_mask);
          if (addr < offset_pages[pagenr].old_page_start) { pagenr--; }
          *objptr = as_object(as_oint(*objptr) +
                              offset_pages[pagenr].offset_page_o);
      }
      break;
     #endif
     #ifdef SPVW_PURE_BLOCKS /* SINGLEMAP_MEMORY */
      #ifdef SINGLEMAP_MEMORY_RELOCATE
      *objptr = as_object(as_oint(*objptr) +
                          offset_heaps_o[mtypecode(*objptr)]);
      break;
      #else
      break; /* everything so far experiences no displacement */
      #endif
     #endif
    /*---NOTREACHED---*/
   #ifndef LINUX_NOEXEC_HEAPCODES
    case_subr: { /* SUBR */
        var oint addr = as_oint(*objptr);
        var offset_subrs_t* ptr = offset_subrs;
        var uintC count;
        dotimespC(count,offset_subrs_anz, {
          if ((ptr->low_o <= addr) && (addr < ptr->high_o)) {
            *objptr = as_object(as_oint(*objptr) + ptr->offset_o);
            goto found_subr;
          }
          ptr++;
        });
      }
      /* SUBR not found -> #<UNBOUND> */
      *objptr = unbound;
   #endif
    found_subr:
      break;
    /*---NOTREACHED---*/
   #ifdef TYPECODES
    case_system: /* frame-pointer or small-read-label or system-constant */
      if ((as_oint(*objptr) & wbit(0+oint_addr_shift)) ==0) {
        /* Frame-Pointer -> #<DISABLED> */
        *objptr = disabled;
      }
      break;
   #endif
    /*---NOTREACHED---*/
    case_machine: { /* pseudo-function or other machine pointer */
        /* conversion old_pseudofun_tab -> pseudofun_tab : */
        var object addr = *objptr;
        {
          var uintC i = pseudofun_anz;
          var const object* ptr = &old_pseudofun_tab.pointer[pseudofun_anz];
          while (i!=0) {
            i--;
            if (eq(*--ptr,addr)) {
              *objptr = pseudofun_tab.pointer[i]; break;
            }
          }
        }
        /* other machine pointer */
        break;
      }
    /*---NOTREACHED---*/
   #ifdef TYPECODES
    case_char:
    case_fixnum:
    case_sfloat:
    #ifdef IMMEDIATE_FFLOAT
    case_ffloat:
    #endif
   #endif
      break;
    /*---NOTREACHED---*/
   #ifdef LINUX_NOEXEC_HEAPCODES
    case_subr: /* immediate Subrs don't exist in this case */
   #endif
    default: /*NOTREACHED*/ abort();
  }
}
local void loadmem_update_fsubr (Fsubr fsubrptr)
{
  var void* addr = fsubrptr->function;
  var uintC i = fsubr_anz;
  var fsubr_t* p = &((fsubr_t*)(&old_fsubr_tab))[fsubr_anz];
  while (i!=0) {
    i--;
    if ((void*) *--p == addr) {
      fsubrptr->function = (void*) ((const fsubr_t *)(&fsubr_tab))[i];
      break;
    }
  }
}
local Handle open_filename (const char* filename)
{ /* open file for reading: */
 #if defined(UNIX)
  return OPEN((char*)filename,O_RDONLY|O_BINARY,my_open_mask);
 #elif defined(WIN32_NATIVE)
  #define CYGDRIVE "/cygdrive/"
  #define CYGDRIVE_LEN 10
  #ifdef __MINGW32__
  if (!strncasecmp(filename,CYGDRIVE,CYGDRIVE_LEN))
  #else
  if (!strncmp(filename,CYGDRIVE,CYGDRIVE_LEN)) /* MS lacks strncasecmp */
  #endif
    {
      var uintL len = asciz_length(filename);
      var char* newfilename = (char*)alloca(len);
      newfilename[0] = filename[CYGDRIVE_LEN];
      newfilename[1] = ':';
      memcpy(newfilename+2,filename+CYGDRIVE_LEN+1,len-CYGDRIVE_LEN);
      filename = newfilename;
    }
  #undef CYGDRIVE
  #undef CYGDRIVE_LEN
  var char resolved[MAX_PATH];
  return /* try to resolve shell shortcuts in the filename */
    CreateFile((real_path(filename,resolved) ? resolved : filename),
               GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
               NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
 #else
  #error "missing open_filename()"
 #endif
}
local void loadmem (const char* filename)
{
#if defined(UNIX)
 #define INVALID_HANDLE_P(handle)  (handle<0)
#elif defined(WIN32_NATIVE)
 #define INVALID_HANDLE_P(handle)  (handle == INVALID_HANDLE)
#else
 #error "missing INVALID_HANDLE_P()"
#endif
  var Handle handle;
  begin_system_call();
  handle = open_filename(filename);
  if (INVALID_HANDLE_P(handle)) { /* try filename.mem */
    var char* filename_mem = (char*)alloca(strlen(filename)+4);
    strcpy(filename_mem,filename);
    strcat(filename_mem,".mem");
    handle = open_filename(filename_mem);
    if (INVALID_HANDLE_P(handle)) goto abort1;
  }
  end_system_call();
#undef INVALID_HANDLE_P
  loadmem_from_handle(handle,filename);
  return;
 abort1: {
    var int abort_errno = OS_errno;
    fprintf(stderr,GETTEXTL("%s: operating system error during load of initialization file `%s'"),program_name,filename);
    errno_out(abort_errno);
  }
  goto abort_quit;
 abort_quit:
  /* first close file, if it had been opened successfully.
     (Thus, now really all errors are ignored!) */
  if (handle != INVALID_HANDLE) {
    begin_system_call(); CLOSE_HANDLE(handle); end_system_call();
  }
  quit_sofort(1);
}
local void loadmem_from_handle (Handle handle, const char* filename)
{
  var memdump_header_t header;
  {
   #if (defined(SPVW_PURE_BLOCKS) && defined(SINGLEMAP_MEMORY)) || (defined(SPVW_MIXED_BLOCKS_STAGGERED) && defined(TRIVIALMAP_MEMORY))
    #if defined(HAVE_MMAP)
    local var bool use_mmap = true;
    #endif
    var off_t file_offset;
    #define set_file_offset(x)  file_offset = (x)
    #define inc_file_offset(x)  file_offset += (uintM)(x)
   #else
    #define set_file_offset(x)
    #define inc_file_offset(x)
   #endif
   #if defined(DEBUG_SPVW)
    #define FILE_LINE  fprintf(stderr,"[%s:%d] ",__FILE__,__LINE__)
   #else
    #define FILE_LINE  /*noop*/
   #endif
    #define ABORT_SYS do { FILE_LINE; goto abort_sys; } while(0)
    #define ABORT_INI do { FILE_LINE; goto abort_ini; } while(0)
    #define ABORT_MEM do { FILE_LINE; goto abort_mem; } while(0)
    #define READ(buf,len)                                               \
      do {                                                              \
        begin_system_call();                                            \
        { var ssize_t ergebnis = full_read(handle,(void*)buf,len);      \
          end_system_call();                                            \
          if (ergebnis<0) ABORT_SYS;                                       \
          if (ergebnis != (ssize_t)(len)) ABORT_INI;                       \
          inc_file_offset(len);                                         \
        }                                                               \
      } while(0)
   begin_read:
    set_file_offset(0);
    /* read basic information: */
    READ(&header,sizeof(header));
    if (header._magic != memdump_magic) {
     #ifdef UNIX
      /* try to unzip the file on the fly with GZIP. */
      var uintB* file_header = (uintB*)&header; /* use sizeof(header) >= 2 */
      if (file_header[0] == '#' && file_header[1] == '!') { /* executable magic ? */
        /* skip first text line */
        var char c;
        begin_system_call();
        if ( lseek(handle,-(off_t)sizeof(header),SEEK_CUR) <0)
          ABORT_SYS;               /* in file, back to the start */
        do { READ(&c,1); } while (c!='\n');
        end_system_call();
       #if ((defined(SPVW_PURE_BLOCKS) && defined(SINGLEMAP_MEMORY)) || (defined(SPVW_MIXED_BLOCKS_STAGGERED) && defined(TRIVIALMAP_MEMORY))) && defined(HAVE_MMAP)
        use_mmap = false; /* the file-offsets have been displaced! */
       #endif
        goto begin_read;
      }
      if (file_header[0] == 0x1F && file_header[1] == 0x8B) { /* gzip magic ? */
        /* open pipe, see make_pipe_input_stream in STREAM.D */
        var int handles[2];
        var int child;
        begin_system_call();
        if ( lseek(handle,-(off_t)sizeof(header),SEEK_CUR) <0)
          ABORT_SYS;               /* in file, back to the start */
        if (pipe(handles) != 0) ABORT_SYS;
        if ((child = vfork()) ==0) {
          if ( dup2(handles[1],stdout_handle) >=0)
            if ( CLOSE(handles[1]) ==0)
              if ( CLOSE(handles[0]) ==0)
                if ( dup2(handle,stdin_handle) >=0) {
                  /* be the File the input of the decompression */
                  /* call decompressor. NB: "gzip -d" == "gunzip" */
                 #if 0
                  execl("/bin/sh","/bin/sh","-c","gzip -d -c",NULL);
                 #else /* it works also without shell */
                  execlp("gzip","gzip","-d","-c",NULL);
                 #endif
                }
          _exit(-1);
        }
        if (child==-1) {
          CLOSE(handles[1]); CLOSE(handles[0]); ABORT_SYS;
        }
        if (CLOSE(handles[1]) !=0) ABORT_SYS;
        if (CLOSE(handle) != 0) ABORT_SYS;
        end_system_call();
       #if ((defined(SPVW_PURE_BLOCKS) && defined(SINGLEMAP_MEMORY)) || (defined(SPVW_MIXED_BLOCKS_STAGGERED) && defined(TRIVIALMAP_MEMORY))) && defined(HAVE_MMAP)
        use_mmap = false; /* mmap can not be done with a pipe! */
       #endif
        loadmem_from_handle(handles[0],filename); /* now, we read from the pipe */
        begin_system_call();
        wait2(child); /* remove zombie-child */
        end_system_call();
        return;
      }
     #endif  /* UNIX */
      ABORT_INI;
    }
    if (header._memflags != memflags) ABORT_INI;
    if (header._oint_type_mask != oint_type_mask) ABORT_INI;
    if (header._oint_addr_mask != oint_addr_mask) ABORT_INI;
   #ifdef TYPECODES
    if (header._cons_type != cons_type) ABORT_INI;
    if (header._complex_type != complex_type) ABORT_INI;
    if (header._symbol_type != symbol_type) ABORT_INI;
    if (header._system_type != system_type) ABORT_INI;
   #endif
    if (header._varobject_alignment != varobject_alignment) ABORT_INI;
    if (header._hashtable_length != hashtable_length) ABORT_INI;
    if (header._pathname_length != pathname_length) ABORT_INI;
    if (header._intDsize != intDsize) ABORT_INI;
    if (header._fsubr_anz != fsubr_anz) ABORT_INI;
    if (header._pseudofun_anz != pseudofun_anz) ABORT_INI;
    if (header._symbol_anz != symbol_anz) ABORT_INI;
    if (header._page_alignment != page_alignment) ABORT_INI;
   #ifndef SPVW_MIXED_BLOCKS_OPPOSITE
    if (header._heapcount != heapcount) ABORT_INI;
   #endif
   #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
    { /* calculate offsets (offset = new address - old address): */
      var sintM offset_varobjects = /* offset for objects of variable length */
        mem.varobjects.heap_start - header._mem_varobjects_start;
      var sintM offset_conses = /* offset for two-pointer-objects */
        mem.conses.heap_end - header._mem_conses_end;
      /* calculate new memory partitioning: */
      mem.varobjects.heap_end = header._mem_varobjects_end + offset_varobjects;
      mem.conses.heap_start = header._mem_conses_start + offset_conses;
      /* determine, if there is enough memory:
         it suffices exactly, if
         required room <= available room  <==>
         header._mem_conses_end-header._mem_conses_start
         + header._mem_varobjects_end-header._mem_varobjects_start
         <= mem.conses.heap_end - mem.varobjects.heap_start  <==>
         header._mem_varobjects_end
         + mem.varobjects.heap_start-header._mem_varobjects_start
         <= header._mem_conses_start
         + mem.conses.heap_end-header._mem_conses_end  <==>
         mem.varobjects.heap_end <= mem.conses.heap_start */
      if ((saint)(mem.varobjects.heap_end) > (saint)(mem.conses.heap_start))
        ABORT_MEM;
      /* prepare update: */
      offset_varobjects_o = (oint)offset_varobjects << (oint_addr_shift-addr_shift);
      offset_conses_o = (oint)offset_conses << (oint_addr_shift-addr_shift);
    }
   #endif  /* SPVW_MIXED_BLOCKS_OPPOSITE */
   #ifdef SPVW_PURE_BLOCKS /* SINGLEMAP_MEMORY */
    if ((aint)(&subr_tab) != header._subr_tab_addr) ABORT_INI;
    if ((aint)(&symbol_tab) != header._symbol_tab_addr) ABORT_INI;
   #else
    offset_symbols_o = ((oint)(aint)(&symbol_tab) - (oint)header._symbol_tab_addr) << (oint_addr_shift-addr_shift);
    #ifdef MULTIMAP_MEMORY_SYMBOL_TAB
    if (offset_symbols_o != 0) ABORT_INI;
    #else
     #ifdef TYPECODES
    old_symbol_tab_o = as_oint(type_pointer_object(symbol_type,header._symbol_tab_addr));
     #else
    old_symbol_tab_o = (oint)header._symbol_tab_addr;
     #endif  /* TYPECODES */
    #endif  /* MULTIMAP_MEMORY_SYMBOL_TAB */
   #endif  /* SPVW_PURE_BLOCKS */
    /* initialize offset-of-SUBRs-table: */
    offset_subrs_anz = 1+header._module_count;
    begin_system_call();
    offset_subrs = MALLOC(offset_subrs_anz,offset_subrs_t);
    end_system_call();
    if (offset_subrs==NULL) ABORT_MEM;
    /* read module names and compare with the existing modules: */
    var DYNAMIC_ARRAY(old_modules,module_t*,1+header._module_count);
    {
      var DYNAMIC_ARRAY(module_names_buffer,char,header._module_names_size);
      READ(module_names_buffer,header._module_names_size);
      {
        var module_t* * old_module = &old_modules[0];
        var const char* old_name = &module_names_buffer[0];
        var uintC count;
        dotimespC(count,1+header._module_count, {
          var module_t* module;
          for_modules(all_modules, {
            if (asciz_equal(old_name,module->name))
              goto found_module;
          });
          /* old_name not found */
          ABORT_INI;
         found_module:
          /* Reading the module data from file initializes the module. */
          module->initialized = true;
          *old_module++ = module;
          old_name += asciz_length(old_name)+1;
        });
      }
      FREE_DYNAMIC_ARRAY(module_names_buffer);
    }
    /* read fsubr_tab, pseudofun_tab, symbol_tab: */
    READ(&old_fsubr_tab,sizeof(fsubr_tab));
    READ(&old_pseudofun_tab,sizeof(pseudofun_tab));
    READ(&symbol_tab,sizeof(symbol_tab));
    { /* for each module read subr_addr, subr_anz, object_anz, subr_tab,
         object_tab : */
      var module_t* * old_module = &old_modules[0];
      var offset_subrs_t* offset_subrs_ptr = &offset_subrs[0];
      var uintC count = 1+header._module_count;
      do {
        var subr_t* old_subr_addr;
        var uintC old_subr_anz;
        var uintC old_object_anz;
        READ(&old_subr_addr,sizeof(subr_t*));
        READ(&old_subr_anz,sizeof(uintC));
        READ(&old_object_anz,sizeof(uintC));
        if (old_subr_anz != *(*old_module)->stab_size) ABORT_INI;
        if (old_object_anz != *(*old_module)->otab_size) ABORT_INI;
        offset_subrs_ptr->low_o = as_oint(subr_tab_ptr_as_object(old_subr_addr));
        offset_subrs_ptr->high_o = as_oint(subr_tab_ptr_as_object(old_subr_addr+old_subr_anz));
        offset_subrs_ptr->offset_o = as_oint(subr_tab_ptr_as_object((*old_module)->stab)) - offset_subrs_ptr->low_o;
        if (old_subr_anz > 0) {
          var DYNAMIC_ARRAY(old_subr_tab,subr_t,old_subr_anz);
          READ(old_subr_tab,old_subr_anz*sizeof(subr_t));
          var subr_t* ptr1 = old_subr_tab;
          var subr_t* ptr2 = (*old_module)->stab;
          var uintC counter = old_subr_anz;
          do {
            if (!(   (ptr1->req_anz == ptr2->req_anz)
                  && (ptr1->opt_anz == ptr2->opt_anz)
                  && (ptr1->rest_flag == ptr2->rest_flag)
                  && (ptr1->key_flag == ptr2->key_flag)
                  && (ptr1->key_anz == ptr2->key_anz)))
              ABORT_INI;
            ptr2->name = ptr1->name; ptr2->keywords = ptr1->keywords;
            ptr2->argtype = ptr1->argtype;
            ptr1++; ptr2++;
          } while (--counter);
          FREE_DYNAMIC_ARRAY(old_subr_tab);
        }
        if (old_object_anz > 0) {
          READ((*old_module)->otab,old_object_anz*sizeof(gcv_object_t));
        }
        old_module++; offset_subrs_ptr++;
      } while (--count);
    }
   #ifdef SPVW_PURE_BLOCKS
    #ifdef SINGLEMAP_MEMORY_RELOCATE
    { /* read start- and end-addresses of each Heap and compare
         with mem.heaps[]: */
      var memdump_page_t old_pages[heapcount];
      var memdump_page_t* old_page;
      var uintL heapnr;
      READ(&old_pages,sizeof(old_pages));
      offset_heaps_all_zero = true;
      old_page = &old_pages[0];
      for (heapnr=0; heapnr<heapcount; heapnr++) {
        var Heap* heapptr = &mem.heaps[heapnr];
        if (old_page->_page_end - old_page->_page_start
            > heapptr->heap_hardlimit - heapptr->heap_limit)
          ABORT_MEM;
        heapptr->heap_start = heapptr->heap_limit;
        heapptr->heap_end = heapptr->heap_limit + (old_page->_page_end - old_page->_page_start);
        offset_heaps_o[heapnr] = (oint)heapptr->heap_start - (oint)old_page->_page_start;
        if (offset_heaps_o[heapnr] != 0)
          offset_heaps_all_zero = false;
        old_page++;
      }
     #if defined(HAVE_MMAP)
      if (!offset_heaps_all_zero)
        use_mmap = false;
     #endif
    }
    #else
    { /* take over start- and end-addresses of each heap in mem.heaps[] : */
      var uintL heapnr;
      var memdump_page_t old_pages[heapcount];
      var memdump_page_t* old_page;
      READ(&old_pages,sizeof(old_pages));
      old_page = &old_pages[0];
      for (heapnr=0; heapnr<heapcount; heapnr++) {
        map_heap(mem.heaps[heapnr],page, {
          page->page_start = old_page->_page_start;
          page->page_end = old_page->_page_end;
          old_page++;
        });
      }
    }
    #endif  /* SINGLEMAP_MEMORY_RELOCATE */
   #endif  /* SPVW_PURE_BLOCKS */
   #ifdef SPVW_MIXED_BLOCKS_STAGGERED
    { /* read start- and end-addresses of each heap and adjust
         the size in mem.heaps[] to the same length: */
      var uintL heapnr;
      var memdump_page_t old_pages[heapcount];
      var memdump_page_t* old_page;
      READ(&old_pages,sizeof(old_pages));
      old_page = &old_pages[0];
      for (heapnr=0; heapnr<heapcount; heapnr++) {
        map_heap(mem.heaps[heapnr],page, {
          page->page_end = page->page_start + (old_page->_page_end - old_page->_page_start);
          offset_heaps_o[heapnr] = (oint)(sintM)(page->page_start - old_page->_page_start) << (oint_addr_shift-addr_shift);
          old_page++;
        });
      }
    }
   #endif  /* SPVW_MIXED_BLOCKS_STAGGERED */
   #if defined(SPVW_PURE_BLOCKS) && defined(GENERATIONAL_GC)
    {
      var uintL numphyspages[heapcount];
      var uintL heapnr;
      READ(&numphyspages,sizeof(numphyspages));
      for (heapnr=0; heapnr<heapcount; heapnr++) {
        var uintL count = numphyspages[heapnr];
        var Heap* heap = &mem.heaps[heapnr];
        if (count > 0) {
          var DYNAMIC_ARRAY(_physpages,memdump_physpage_state_t,count);
          var physpage_state_t* physpages;
          READ(_physpages,count*sizeof(memdump_physpage_state_t));
          physpages = MALLOC(count,physpage_state_t);
          if (physpages != NULL) {
            var uintL i;
            for (i=0; i<count; i++) {
              physpages[i].continued_addr  = _physpages[i].continued_addr;
              physpages[i].continued_count = _physpages[i].continued_count;
              physpages[i].firstobject     = _physpages[i].firstobject;
              physpages[i].protection = PROT_READ;
              physpages[i].cache_size = 0; physpages[i].cache = NULL;
            }
          }
          FREE_DYNAMIC_ARRAY(_physpages);
          heap->physpages = physpages;
        } else {
          heap->physpages = NULL;
        }
      }
    }
   #endif  /* SPVW_PURE_BLOCKS) && GENERATIONAL_GC */
   #ifdef SPVW_PAGES
    {
      var uintC total_pagecount;
      var uintC pagecounts[heapcount];
      /* initialize the pages-per-heap-table: */
      READ(&pagecounts,sizeof(pagecounts));
      { /* calculate total_pagecount: */
        var uintL heapnr;
        total_pagecount = 0;
        for (heapnr=0; heapnr<heapcount; heapnr++)
          total_pagecount += pagecounts[heapnr];
      }
      /* initialize offset-per-page-table: */
      begin_system_call();
      offset_pages = MALLOC(offset_pages_len,offset_pages_t);
      end_system_call();
      if (offset_pages==NULL) ABORT_MEM;
      {
        var uintL pagenr;
        for (pagenr=0; pagenr<offset_pages_len; pagenr++) {
          offset_pages[pagenr].old_page_start = ~0L;
          offset_pages[pagenr].offset_page_o = 0;
        }
      }
      /* read addresses and sizes of the pages and allocate pages: */
      var DYNAMIC_ARRAY(old_pages,memdump_page_t,total_pagecount);
      READ(old_pages,total_pagecount*sizeof(memdump_page_t));
      var DYNAMIC_ARRAY(new_pages,aint,total_pagecount);
      {
        var memdump_page_t* old_page_ptr = &old_pages[0];
        var aint* new_page_ptr = &new_pages[0];
        var uintL heapnr;
        for (heapnr=0; heapnr<heapcount; heapnr++) {
          var Pages* pages_ptr = &mem.heaps[heapnr].inuse;
          var uintC pagecount = pagecounts[heapnr];
          while (pagecount!=0) {
            var uintM need = old_page_ptr->_page_end - old_page_ptr->_page_start;
            var uintM misaligned = mem.heaps[heapnr].misaligned;
            var uintM size1 = round_up(misaligned+need,sizeof(cons_));
            if (size1 < std_page_size) { size1 = std_page_size; }
            {
              var uintM size2 = size1 + sizeof_NODE + (varobject_alignment-1);
              var aint addr = (aint)mymalloc(size2);
              var Pages page;
              if ((void*)addr == NULL) ABORT_MEM;
             #if !defined(AVL_SEPARATE)
              page = (Pages)addr;
             #else
              begin_system_call();
              page = (NODE*)malloc(sizeof(NODE));
              end_system_call();
              if (page == NULL) ABORT_MEM;
             #endif
              /* get page from operating system. */
              page->m_start = addr; page->m_length = size2;
              /* initialize: */
              page->page_start = page_start0(page) + misaligned;
              page->page_end = page->page_start + need;
              page->page_room = size1 - need;
              /* add to this heap: */
              *pages_ptr = AVL(AVLID,insert1)(page,*pages_ptr);
              *new_page_ptr = page->page_start;
              var aint old_page_start = old_page_ptr->_page_start;
              var aint old_page_end = old_page_ptr->_page_end;
              var oint offset_page_o = ((oint)page->page_start - (oint)old_page_start) << (oint_addr_shift-addr_shift);
              var uintL pagenr = pagenr_of(old_page_start & addr_mask);
              do {
                if (offset_pages[pagenr].old_page_start != ~0L) { abort(); }
                offset_pages[pagenr].old_page_start = old_page_start;
                offset_pages[pagenr].offset_page_o = offset_page_o;
                pagenr++;
              } while (pagenr < pagenr_of(old_page_end & addr_mask));
            }
            old_page_ptr++; new_page_ptr++;
            pagecount--;
          }
        }
      }
      { /* read content of the pages pages: */
        var memdump_page_t* old_page_ptr = &old_pages[0];
        var aint* new_page_ptr = &new_pages[0];
        while (total_pagecount != 0) {
          var uintM len = old_page_ptr->_page_end - old_page_ptr->_page_start;
          READ(*new_page_ptr,len);
          old_page_ptr++; new_page_ptr++;
          total_pagecount--;
        }
      }
      FREE_DYNAMIC_ARRAY(new_pages);
      FREE_DYNAMIC_ARRAY(old_pages);
    }
   #endif  /* SPVW_PAGES */
   #if defined(SPVW_PURE_BLOCKS) || defined(SPVW_MIXED_BLOCKS_STAGGERED) /* SINGLEMAP_MEMORY || TRIVIALMAP_MEMORY && !SPVW_MIXED_BLOCKS_OPPOSITE */
    /* put alignment into practice: */
    READ_page_alignment(file_offset);
    { /* read content of the blocks: */
      var uintL heapnr;
      for (heapnr=0; heapnr<heapcount; heapnr++) {
        var Heap* heapptr = &mem.heaps[heapnr];
        var uintM len = heapptr->heap_end - heapptr->heap_start;
        var uintM misaligned =
          (is_varobject_heap(heapnr) ? varobjects_misaligned : 0);
        var uintM map_len = round_up(misaligned+len,map_pagesize);
        heapptr->heap_limit = (heapptr->heap_start-misaligned) + map_len;
        if (map_len > 0) {
          if (heapptr->heap_limit-1 > heapptr->heap_hardlimit-1) ABORT_MEM;
         #if defined(HAVE_MMAP)
          /* if possible, we put the initialization file into memory.
             This should accelerate the start and delay unnecessary
             loading until the first GC.
             the page_alignment is necessary for this purpose! */
          if (use_mmap) {
            if (filemap((void*)(heapptr->heap_start-misaligned),map_len,
                        handle,file_offset)
                != (void*)(-1))
              {
               #if 0
                /* unnecessary, because mmap() needs no lseek()
                   and only CLOSE(handle) follows afterwards. */
                if ( lseek(handle,map_len,SEEK_CUR) <0) ABORT_SYS;
               #endif
                inc_file_offset(map_len);
                goto block_done;
              } else {
                fprintf(stderr,GETTEXTL("%s: Cannot map the initialization file `%s' into memory."),program_name,filename);
                errno_out(errno);
                use_mmap = false;
                /* before continuing with READ(handle),
                   an lseek() is poss. necessary. */
                if ( lseek(handle,file_offset,SEEK_SET) <0) ABORT_SYS;
            }
          }
         #endif  /* HAVE_MMAP */
          if (zeromap((void*)(heapptr->heap_start-misaligned),map_len) <0)
            ABORT_MEM;
         #if varobjects_misaligned
          if (is_varobject_heap(heapnr)) {
            var uintB dummy[varobjects_misaligned];
            READ(&dummy[0],varobjects_misaligned);
          }
         #endif
          READ(heapptr->heap_start,len);
          READ_page_alignment(misaligned+len);
         block_done: ;
        }
      }
    }
    #if defined(HAVE_MMAP)
    if (use_mmap) { /* check the length of the mmap-ed files: */
     #ifdef UNIX
      var struct stat statbuf;
      if (fstat(handle,&statbuf) < 0) ABORT_SYS;
      /* executable size is appended to the image as size_t */
      if (statbuf.st_size < file_offset + sizeof(size_t)) ABORT_INI;
     #endif
     #ifdef WIN32_NATIVE
      var DWORD fsize_hi;
      var DWORD fsize_lo = GetFileSize(handle,&fsize_hi);
      if (fsize_lo == (DWORD)(-1) && GetLastError() != NO_ERROR) ABORT_SYS;
      var off_t fsize = ((uint64)fsize_hi << 32) | fsize_lo;
      /* executable size is appended to the image as size_t */
      if (fsize < file_offset + sizeof(size_t)) ABORT_INI;
     #endif
    }
    #endif  /* HAVE_MMAP */
   #endif  /* SPVW_PURE_BLOCKS || SPVW_MIXED_BLOCKS_STAGGERED */
   #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
    { /* read objects of variable length: */
      var uintM len = header._mem_varobjects_end - header._mem_varobjects_start;
     #ifdef TRIVIALMAP_MEMORY
      var uintM map_len = round_up(len+varobjects_misaligned,map_pagesize);
      mem.varobjects.heap_limit = (mem.varobjects.heap_start-varobjects_misaligned) + map_len;
      if (zeromap((void*)(mem.varobjects.heap_start-varobjects_misaligned),map_len) <0) ABORT_MEM;
     #endif
      READ(mem.varobjects.heap_start,len);
    }
    { /* read conses: */
      var uintM len = header._mem_conses_end - header._mem_conses_start;
     #ifdef TRIVIALMAP_MEMORY
      var uintM map_len = round_up(len,map_pagesize);
      mem.conses.heap_limit = mem.conses.heap_end - map_len;
      if (zeromap((void*)mem.conses.heap_limit,map_len) <0) ABORT_MEM;
     #endif
      READ(mem.conses.heap_start,len);
    }
   #endif  /* SPVW_MIXED_BLOCKS_OPPOSITE */
   #ifdef GENERATIONAL_GC
    { /* make the SIGSEGV-handler functional. */
      var uintL heapnr;
      for (heapnr=0; heapnr<heapcount; heapnr++) {
        var Heap* heapptr = &mem.heaps[heapnr];
        heapptr->heap_gen0_start = heapptr->heap_start;
        heapptr->heap_gen0_end = heapptr->heap_end;
       #ifndef SPVW_PURE_BLOCKS
        heapptr->physpages = NULL;
       #endif
      }
    }
   #endif
    /* traverse all LISP-objects and update: */
    #define update  loadmem_update
    /* update program constants: */
    update_tables();
   #ifdef SINGLEMAP_MEMORY_RELOCATE
    if (!offset_heaps_all_zero)
   #endif
     #if !defined(SPVW_PURE_BLOCKS) || defined(SINGLEMAP_MEMORY_RELOCATE)
      { /* update pointers in the cons-cells: */
       #define update_conspage  update_conspage_normal
        update_conses();
       #undef update_conspage
        /* update pointers in the objects of variable length: */
       #define update_page  update_page_normal
       #define update_hashtable_invalid  true
       #define update_unrealloc  false
       #define update_ss_unrealloc(ptr)
       #define update_in_unrealloc(ptr)
       #ifdef FOREIGN
         #define update_fpointer_invalid  true
       #else
         #define update_fpointer_invalid  false
       #endif
       #define update_fsubr_function  true
       #define update_ht_invalid  set_ht_invalid_if_needed
       #define update_fp_invalid  mark_fp_invalid
       #define update_fs_function  loadmem_update_fsubr
        update_varobjects();
       #undef update_fs_function
       #undef update_fp_invalid
       #undef update_ht_invalid
       #undef update_fsubr_function
       #undef update_fpointer_invalid
       #undef update_in_unrealloc
       #undef update_ss_unrealloc
       #undef update_unrealloc
       #undef update_hashtable_invalid
       #undef update_page
      }
     #endif  /* SPVW_PURE_BLOCKS || SINGLEMAP_MEMORY_RELOCATE */
   #ifdef SINGLEMAP_MEMORY_RELOCATE
    else /* i.e. if (offset_heaps_all_zero) */
   #endif
     #ifdef SPVW_PURE_BLOCKS
      { /* update the pointers in the cons-cells and objects of variable
           length. There are only few of those pointers, and
           they were listed when the memimage was stored. */
       #if defined(HAVE_MMAP)
        if (use_mmap) {
          if ( lseek(handle,file_offset,SEEK_SET) <0) ABORT_SYS;
        }
       #endif
        var memdump_reloc_header_t rheader;
        READ(&rheader,sizeof(rheader));
        if (rheader.reloccount > 0) {
          var DYNAMIC_ARRAY(relocbuf,gcv_object_t*,rheader.reloccount);
          var gcv_object_t** relocbufptr = &relocbuf[0];
          var uintL count;
          READ(&relocbuf[0],rheader.reloccount*sizeof(gcv_object_t*));
          dotimespL(count,rheader.reloccount, { update(*relocbufptr++); });
          FREE_DYNAMIC_ARRAY(relocbuf);
        }
        if (rheader.htcount > 0) {
          var DYNAMIC_ARRAY(htbuf,Hashtable,rheader.htcount);
          var Hashtable* htbufptr = &htbuf[0];
          var uintL count;
          READ(&htbuf[0],rheader.htcount*sizeof(Hashtable));
          dotimespL(count,rheader.htcount, {
            var Hashtable ptr = *htbufptr++; set_ht_invalid_if_needed(ptr);
          });
          FREE_DYNAMIC_ARRAY(htbuf);
        }
        if (rheader.fpcount > 0) {
          var DYNAMIC_ARRAY(fpbuf,Record,rheader.fpcount);
          var Record* fpbufptr = &fpbuf[0];
          var uintL count;
          READ(&fpbuf[0],rheader.fpcount*sizeof(Record));
          dotimespL(count,rheader.fpcount, {
            var Record ptr = *fpbufptr++; mark_fp_invalid(ptr);
          });
          FREE_DYNAMIC_ARRAY(fpbuf);
        }
        if (rheader.fscount > 0) {
          var DYNAMIC_ARRAY(fsbuf,Fsubr,rheader.fscount);
          var Fsubr* fsbufptr = &fsbuf[0];
          var uintL count;
          READ(&fsbuf[0],rheader.fscount*sizeof(Fsubr));
          dotimespL(count,rheader.fscount, {
            var Fsubr fsubrptr = *fsbufptr++; loadmem_update_fsubr(fsubrptr);
          });
          FREE_DYNAMIC_ARRAY(fsbuf);
        }
      }
     #endif  /* SPVW_PURE_BLOCKS */
    #undef update
    /* close file: */
    #undef READ
    begin_system_call();
    #ifdef UNIX
    if ( CLOSE(handle) <0) ABORT_SYS;
    #elif defined(WIN32_NATIVE)
    if (!CloseHandle(handle)) { handle = INVALID_HANDLE_VALUE; ABORT_SYS; }
    #endif
    end_system_call();
   #ifdef SPVW_PAGES
    begin_system_call(); free(offset_pages); end_system_call();
    recalc_space(false);
   #endif
   #if defined(SPVW_PURE_BLOCKS) || defined(TRIVIALMAP_MEMORY) || defined(GENERATIONAL_GC) /* SINGLEMAP_MEMORY || TRIVIALMAP_MEMORY || GENERATIONAL_GC */
    #ifdef GENERATIONAL_GC
    {
      var uintL heapnr;
      for (heapnr=0; heapnr<heapcount; heapnr++) {
        var Heap* heap = &mem.heaps[heapnr];
       #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
        if (is_cons_heap(heapnr)) {
          heap->heap_start = heap->heap_gen1_end
            = heap->heap_start & -physpagesize;
        } else {
          heap->heap_gen1_start = heap->heap_end
            = ((heap->heap_end + (physpagesize-1)) & -physpagesize) + varobjects_misaligned;
          heap->heap_limit = heap->heap_end;
        }
       #else /* defined(SPVW_PURE_BLOCKS) || defined(SPVW_MIXED_BLOCKS_STAGGERED) */
        heap->heap_gen1_start = heap->heap_end
          = ((heap->heap_end + (physpagesize-1)) & -physpagesize)
            + (is_varobject_heap(heapnr) ? varobjects_misaligned : 0);
        heap->heap_limit = heap->heap_end;
       #endif  /* SPVW_MIXED_BLOCKS_OPPOSITE */
       #ifdef SPVW_PURE_BLOCKS
        /* Don't need to rebuild the cache. */
        xmmprotect_old_generation_cache(heapnr);
       #else
        if (!is_unused_heap(heapnr))
          build_old_generation_cache(heapnr);
       #endif
      }
    }
    #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
    if (mem.varobjects.heap_end > mem.conses.heap_start) ABORT_MEM;
    #endif
    /* now wee need the SIGSEGV-handler. */
    install_segv_handler();
    #endif  /* GENERATIONAL_GC */
    {
      var uintM space = used_space();
      set_total_room(space); /* we have plenty of time until the next GC */
     #ifdef GENERATIONAL_GC
      mem.last_gcend_space0 = space;
      mem.last_gcend_space1 = 0;
     #endif
    }
   #endif /* SPVW_PURE_BLOCKS || TRIVIALMAP_MEMORY || GENERATIONAL_GC */
    FREE_DYNAMIC_ARRAY(old_modules);
    begin_system_call(); free(offset_subrs); end_system_call();
  }
  /* declare open files as closed: */
  closed_all_files();
 #ifdef GENERATIONAL_GC
  O(gc_count) = Fixnum_0;  /* so far no GCs: */
 #endif
  { # Initialize markwatchset:
    var uintM need = 0;
    var object L;
    for (L = O(all_weakpointers);
         !eq(L,Fixnum_0);
         L = ((Weakpointer)TheRecord(L))->wp_cdr)
      need += 1 + max_watchset_count(L);
    if (need > 0) {
      markwatchset_allocated = markwatchset_size = need;
      begin_system_call();
      markwatchset = (markwatch_t*)malloc(markwatchset_allocated*sizeof(markwatch_t));
      end_system_call();
      if (markwatchset==NULL) ABORT_MEM;
    }
  }
  /* Delete cache of standard file streams. */
  O(standard_input_file_stream) = NIL;
  O(standard_output_file_stream) = NIL;
  O(standard_error_file_stream) = NIL;
 #ifdef MACHINE_KNOWN
  /* declare (MACHINE-TYPE), (MACHINE-VERSION), (MACHINE-INSTANCE)
     as unknown again: */
  O(machine_type_string) = NIL;
  O(machine_version_string) = NIL;
  O(machine_instance_string) = NIL;
 #endif
 #ifndef LANGUAGE_STATIC
  /* delete cache of (LISP-IMPLEMENTATION-VERSION)
     (depends on (SYS::CURRENT-LANGUAGE) ): */
  O(lisp_implementation_version_string) = NIL;
 #endif
  CHECK_AVL_CONSISTENCY();
  CHECK_GC_CONSISTENCY();
  CHECK_GC_UNMARKED(); CHECK_NULLOBJ(); CHECK_GC_CACHE(); CHECK_GC_GENERATIONAL(); SAVE_GC_DATA();
  CHECK_PACK_CONSISTENCY();
  { /* Retrieve misc. data from header. (Can trigger GC!) */
    #if 0
      char memdumptime[4+1+2+1+2 +1+ 2+1+2+1+2+1]; // YYYY-MM-DD HH:MM:SS
      sprintf(memdumptime,"%04u-%02u-%02u %02u:%02u:%02u",
              (uintL)posfixnum_to_V(header._dumptime.Jahr),
              (uintL)posfixnum_to_V(header._dumptime.Monat),
              (uintL)posfixnum_to_V(header._dumptime.Tag),
              (uintL)posfixnum_to_V(header._dumptime.Stunden),
              (uintL)posfixnum_to_V(header._dumptime.Minuten),
              (uintL)posfixnum_to_V(header._dumptime.Sekunden));
    #endif
    char memdumptime[10+1];
    sprintf(memdumptime,"%u",header._dumptime);
    O(memory_image_timestamp) = ascii_to_string(memdumptime);
  }
  O(memory_image_host) = asciz_to_string(header._dumphost,
                                         Symbol_value(S(utf_8)));
  return;
#undef ABORT_SYS
#undef ABORT_INI
#undef ABORT_MEM
 abort_sys: {
    var int abort_errno = OS_errno;
    fprintf(stderr,GETTEXTL("%s: operating system error during load of initialization file `%s'"),program_name,filename);
    errno_out(abort_errno);
  }
  goto abort_quit;
 abort_ini:
  fprintf(stderr,GETTEXTL("%s: initialization file `%s' was not created by this version of CLISP runtime"),program_name,filename);
  fputs("\n",stderr);
  goto abort_quit;
 abort_mem:
  fprintf(stderr,GETTEXTL("%s: not enough memory for initialization"),program_name);
  fputs("\n",stderr);
  goto abort_quit;
 abort_quit:
  /* close the file beforehand. */
  begin_system_call(); CLOSE_HANDLE(handle); end_system_call();
  quit_sofort(1);
}

#if defined(LOADMEM_TRY_SEARCH)
/* find the marker of given size in the open file handle */
local size_t find_marker (Handle handle, char* marker, size_t marker_len) {
  char buf[BUFSIZ];
  size_t marker_pos = 0;
  size_t pos = 0;
  while (1) {
    size_t result = full_read(handle,(void*)buf,BUFSIZ);
    size_t i;
    if (result <= 0)
      return (size_t)-1;
    for (i = 0; i < result; i++) {
      pos++;
      if (buf[i] == marker[marker_pos]) {
        if (++marker_pos == marker_len) /* found! */
          return pos - marker_len;
      } else
        marker_pos = 0;
    }
  }
  return (size_t)-1;
}
#endif

/* find the memory image in the file
 there are two methods:
  - the last sizeof(size_t) bytes in the executable are mem_start
    this method is fast, i.e., O(1): constant time
 #if defined(LOADMEM_TRY_SEARCH)
  - find the memdump_header_t inside the file as if by CL:SEARCH
    this method is expensive, i.e., O(NM) where N is the size of the executable
    (runtime+image) and M is the size of the header (header_size below);
    and can increase the startup time by as much as a few seconds
 #endif
 Since we always record mem_start in every executable image we write,
 there is no reason to do search.
 If "image size" somehow fails, we want a bug report right away.
 > fd : the open file descriptor (its position is changed)
 < set mem_start and mem_searched */
local void find_memdump (Handle fd) {
  var memdump_header_t header;
  var size_t header_size = offsetof(memdump_header_t,_symbol_tab_addr)
    + sizeof(header._symbol_tab_addr);
  fill_memdump_header(&header);
  /* "sizeof(size_t)" is unsigned, so "-sizeof(size_t)" is also unsigned,
     so we need the "(off_t)" cast to pass a negative number to lseek() */
  if (lseek(fd,-(off_t)sizeof(size_t),SEEK_END) > 0
      && full_read(fd,(void*)&mem_start,sizeof(size_t)) == sizeof(size_t)
      && lseek(fd,mem_start,SEEK_SET) == mem_start) {
    var memdump_header_t header1;
    full_read(fd,(void*)&header1,header_size);
    if (memcmp((void*)&header,(void*)&header1,header_size) != 0)
      mem_start = (size_t)-1;   /* bad header => no image */
  } else {
   #if defined(LOADMEM_TRY_SEARCH)
    /* lseek+read does not work ==> use marker */
    lseek(fd,0,SEEK_SET);
    mem_start = find_marker(fd,(char*)&header,header_size);
    if (mem_start  != (size_t)-1)
      /* image size failed, but header is found -- this is fishy! */
      fprintf(stderr,GETTEXTL("%s: 'image size' method failed, but found image header at %d\n"),get_executable_name(),mem_start);
   #else
    mem_start = (size_t)-1;
   #endif
  }
  mem_searched = true;
}

/* load the memory image from the currently running executable
 return 0 on success
    and 1 on error (when the executable does not contain a memory image) */
local int loadmem_from_executable (void) {
  var char* executable_name = get_executable_name();
  var Handle handle = open_filename(executable_name);
  var int success = 1;
  if (handle != INVALID_HANDLE) { /* just in case ... */
    find_memdump(handle);
    if (mem_start != (size_t)-1) { /* found! */
      lseek(handle,mem_start,SEEK_SET);
      loadmem_from_handle(handle,executable_name);
      success = 0;
    }
    CLOSE_HANDLE(handle);
  }
  return success;
}
