# Moved out of memory management:
# Table of all fixed objects
# Bruno Haible 1990-2002

#include "lispbibl.c"

#undef LISPOBJ

# Table of all fixed objects
global struct object_tab_ object_tab
    #if defined(INIT_OBJECT_TAB) && NIL_IS_CONSTANT
    = {
        #define LISPOBJ LISPOBJ_B
        #include "constobj.c"
        #undef LISPOBJ
      }
    #endif
    ;
global uintC object_tab_size = sizeof(object_tab)/sizeof(gcv_object_t);

