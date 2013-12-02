# Moved out of memory management:
# table of all SUBRs
# Bruno Haible 1990-2004

#include "lispbibl.c"

#undef LISPFUN

# table of all SUBRs
global struct subr_tab_ subr_tab_data
  #if defined(HEAPCODES) && (alignment_long < varobject_alignment) && defined(GNU)
    # Force all Subrs to be allocated with a 4/8-byte alignment. GC needs this.
    __attribute__ ((aligned (varobject_alignment)))
  #endif
  #if defined(INIT_SUBR_TAB)
    = {
        #if varobjects_misaligned
        { 0 },
        #endif
        #if NIL_IS_CONSTANT
          #define LISPFUN  LISPFUN_G
        #else
          #define LISPFUN  LISPFUN_F
        #endif
        #include "subr.c"
        #undef LISPFUN
      }
  #endif
  ;
global uintC subr_tab_data_size = (sizeof(subr_tab_data)-varobjects_misaligned)/sizeof(subr_t);

