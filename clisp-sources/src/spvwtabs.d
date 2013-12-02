/*
 * Moved out of memory management:
 * table of all fixed symbols
 * Bruno Haible 1990-2004
 */

#include "lispbibl.c"

#undef LISPSYM

/* Yet another macro-problem. Grr... */
#undef inline
/* WIN32.D does "#define read" and "#define write" */
#undef read
#undef write
/* LISPBIBL.D does "#define export" */
#undef export
/* Large File Support on some versions of Solaris does "#define open open64",
   "#define truncate truncate64", "#define ftruncate ftruncate64" */
#undef open
#undef truncate
#undef ftruncate

/* Table of all fixed symbols: */
global struct symbol_tab_ symbol_tab_data
  #if defined(HEAPCODES) && (alignment_long < varobject_alignment) && defined(GNU)
    # Force all symbols to be allocated with a 4/8-byte alignment. GC needs this.
    __attribute__ ((aligned (varobject_alignment)))
  #endif
  #if defined(INIT_SYMBOL_TAB) && NIL_IS_CONSTANT
    = {
        #if varobjects_misaligned
        { 0 },
        #endif
        #define LISPSYM  LISPSYM_B
        #include "constsym.c"
        #undef LISPSYM
      }
  #endif
  ;

