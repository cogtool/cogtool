# Updating all heap objects in the world (i.e. in the heap and stack).

# ------------------------------ Specification --------------------------------

# For the following macros, the macro update(objptr) must be defined, with the
# signature:  local void update (object* objptr);

# Update all the world, except the heaps and the stacks.
#   update_tables();

# Update the cons heaps.
#   #define update_conspage ...
#   update_conses();
#   #undef update_conspage
# Some possible implementation of update_conspage.
#   update_conspage_normal

# Update the varobject heaps.
#   #define update_hashtable_invalid ...
#   #define update_ht_invalid ...
#   #define update_unrealloc ...
#   #define update_ss_unrealloc ...
#   #define update_in_unrealloc ...
#   #define update_fpointer_invalid ...
#   #define update_fp_invalid ...
#   #define update_fsubr_function ...
#   #define update_fs_function ...
#   #define update_page ...
#   update_varobjects();
#   #undef update_page
#   #undef update_fs_function
#   #undef update_fsubr_function
#   #undef update_fp_invalid
#   #undef update_fpointer_invalid
#   #undef update_in_unrealloc
#   #undef update_ss_unrealloc
#   #undef update_unrealloc
#   #undef update_ht_invalid
#   #undef update_hashtable_invalid
# Some possible implementation of update_page.
#   update_page_normal

# Update the stacks.
#   #define update_stackobj ...
#   update_stacks();
#   #undef update_stackobj
# Some possible implementation of update_stackobj.
#   update_stackobj_normal

/* Update the C stacks.
   update_back_traces(); */

# ------------------------------ Implementation -------------------------------

# update program constants:
#define update_subr_tab()          \
  for_all_subrs({                  \
    var gcv_object_t* p;           \
    p = &ptr->name; update(p);     \
    p = &ptr->keywords; update(p); \
  })
#define update_symbol_tab()                     \
  for_all_constsyms({ # traverse symbol_tab     \
    var gcv_object_t* p;                        \
    p = &ptr->symvalue; update(p);              \
    p = &ptr->symfunction; update(p);           \
    p = &ptr->hashcode; update(p);              \
    p = &ptr->proplist; update(p);              \
    p = &ptr->pname; update(p);                 \
    p = &ptr->homepackage; update(p);           \
  })
#define update_object_tab()                                     \
  do {                                                          \
    for_all_constobjs( update(objptr); ); # traverse object_tab \
    for_all_threadobjs( update(objptr); ); # traverse threads   \
  } while(0)
#define update_tables()                      \
  do {                                       \
    update_subr_tab();                       \
    update_symbol_tab();                     \
    update_object_tab();                     \
  } while(0)

# update the pointers in the Cons-cells:
#define update_conspage_normal(page)                                       \
  do {                                                                     \
    var aint objptr = page->page_start;                                    \
    var aint objptrend = page->page_end;                                   \
    # update all pointers in the (new) CONS-region start <= address < end: \
    while (objptr != objptrend) {                                          \
      update((gcv_object_t*)objptr);                                       \
      objptr += sizeof(gcv_object_t);                                      \
      update((gcv_object_t*)objptr);                                       \
      objptr += sizeof(gcv_object_t);                                      \
    }                                                                      \
  } while(0)
#define update_conses() for_each_cons_page(page, update_conspage(page) )

# update pointers in the objects of variable length:
#define update_page_normal(page,updater)                     \
  do {                                                       \
    var aint ptr = page->page_start;                         \
    var aint ptrend = page->page_end;                        \
    # traverse all objects with address >=ptr, <ptrend :     \
    while (ptr != ptrend) { # until ptr has reached the end  \
      # traverse next object with address ptr (< ptrend) :   \
      updater(typecode_at(ptr)); # and advance               \
    }                                                        \
  } while(0)
# subroutines:
#define do_update_symbol()                                                            \
  do {                                                                                \
    var gcv_object_t* p = (gcv_object_t*)pointerplus(ptr,symbol_objects_offset);      \
    var uintC count;                                                                  \
    dotimespC(count,((sizeof(symbol_)-symbol_objects_offset)/sizeof(gcv_object_t)), { \
      update(p); p++;                                                                 \
    });                                                                               \
  } while(0)
#define do_update_svector()                           \
  do {                                                \
    var uintL count = svector_length((Svector)ptr);   \
    if (count != 0) {                                 \
      var gcv_object_t* p = &((Svector)ptr)->data[0]; \
      dotimespL(count,count, { update(p); p++; } );   \
    }                                                 \
  } while(0)
#define do_update_iarray()  \
  do { var gcv_object_t* p = &((Iarray)ptr)->data; update(p); } while(0)
#define do_update_sistring()  \
  do { var gcv_object_t* p = &((Sistring)ptr)->data; update(p); } while(0)
#define do_update_sxrecord()                                              \
  do {                                                                    \
    # on update of pointers, the hash-tables are invalidated              \
    # (because the hash function of an object depends on its address,     \
    # which is now changed).                                              \
    if (update_hashtable_invalid && # a hash-table ?                      \
        record_type((Record)ptr) == Rectype_Hashtable) {                  \
      update_ht_invalid((Hashtable)ptr); # yes -> note for reorganisation \
    } else if (update_unrealloc && # Instance ?                           \
               (record_type((Record)ptr) == Rectype_Instance              \
                || (record_type((Record)ptr) == Rectype_Closure           \
                    && (closure_flags((Closure)ptr) & closflags_instance_B)))) {          \
      update_in_unrealloc((Record)ptr); # yes -> cleanup forward ptr mark \
    } else if (update_fpointer_invalid &&  # foreign-pointer ?            \
               (record_type((Record)ptr) == Rectype_Fpointer)) {          \
      update_fp_invalid((Record)ptr); # yes -> poss. invalidate           \
    } else if (update_fsubr_function &&  # Fsubr ?                        \
               (record_type((Record)ptr) == Rectype_Fsubr)) {             \
      update_fs_function((Fsubr)ptr); # yes -> poss. update address       \
    }                                                                     \
   {var uintC count = (record_type((Record)ptr) < rectype_limit           \
                       ? srecord_length((Srecord)ptr)                     \
                       : xrecord_length((Xrecord)ptr));                   \
    if (count != 0) {                                                     \
      var gcv_object_t* p = &((Record)ptr)->recdata[0];                   \
      dotimespC(count,count, { update(p); p++; } );                       \
    }                                                                     \
  }} while(0)
#define do_update_lrecord()                             \
  do {                                                  \
    var uintC count = lrecord_length((Lrecord)ptr);     \
    if (count != 0) {                                   \
      var gcv_object_t* p = &((Record)ptr)->recdata[0]; \
      dotimespC(count,count, { update(p); p++; } );     \
    }                                                   \
  } while(0)
# updates the object at 'ptr', whose typecode is given by 'type_expr'
# and advances ptr:
#ifdef SPVW_MIXED
 #ifdef TYPECODES
  #define update_varobject(type_expr)                                      \
   do {                                                                    \
     var tint type = (type_expr); # typeinfo                               \
     var uintM laenge = objsize((Varobject)ptr); # determine length        \
     var aint newptr = ptr+laenge; # pointer to next object                \
     # fall differentiation according to:                                  \
     # symbol; simple-vector; non-simple array;                            \
     # Record (esp. hash-table); rest.                                     \
     switch (type) {                                                       \
       case_symbolwithflags: # Symbol: update all pointers                 \
         do_update_symbol();                                               \
         break;                                                            \
       case_svector: # Simple-vector: update all pointers                  \
         do_update_svector();                                              \
         break;                                                            \
       case_sstring: # Simple-string                                       \
         if_HAVE_SMALL_SSTRING(                                            \
           if (sstring_reallocatedp((Sstring)ptr))                         \
             do_update_sistring(); # update data vector                    \
           else if (update_unrealloc) {                                    \
             update_ss_unrealloc((Sstring)ptr); # cleanup forward ptr mark \
           }                                                               \
         )                                                                 \
         break;                                                            \
       case_mdarray: case_obvector: case_ob2vector: case_ob4vector:        \
       case_ob8vector: case_ob16vector: case_ob32vector: case_ostring:     \
       case_ovector: # non-simple array: update data vector                \
         do_update_iarray();                                               \
         break;                                                            \
       case_sxrecord: # Record: update all pointers                        \
         do_update_sxrecord();                                             \
         break;                                                            \
       case_lrecord: # Lrecord: update all pointers                        \
         do_update_lrecord();                                              \
         break;                                                            \
       default:  # all others contain no pointer that need update          \
         break; # -> do nothing                                            \
     }                                                                     \
     # advance to the next object                                          \
     ptr = newptr;                                                         \
   } while(0)
 #else # TYPECODES
  #define update_varobject(type_expr)                                      \
   do {                                                                    \
     var uintM laenge = objsize((Varobject)ptr); # determine length        \
     var aint newptr = ptr+laenge; # pointer to the next object            \
     switch (record_type((Record)ptr)) { # type of the next object         \
       case Rectype_mdarray:                                               \
       case Rectype_bvector:                                               \
       case Rectype_b2vector:                                              \
       case Rectype_b4vector:                                              \
       case Rectype_b8vector:                                              \
       case Rectype_b16vector:                                             \
       case Rectype_b32vector:                                             \
       case Rectype_string:                                                \
       case Rectype_vector:                                                \
         # non-simple array: update data vector                            \
         do_update_iarray();                                               \
         break;                                                            \
       if_HAVE_SMALL_SSTRING(                                              \
       case Rectype_reallocstring:                                         \
         # reallocated simple string: update data vector                   \
         do_update_sistring();                                             \
         break;                                                            \
       )                                                                   \
       case Rectype_S16string: case Rectype_Imm_S16string:                 \
       case Rectype_S32string: case Rectype_Imm_S32string:                 \
         if_HAVE_SMALL_SSTRING(                                            \
           if (update_unrealloc) {                                         \
             update_ss_unrealloc((Sstring)ptr); # cleanup forward ptr mark \
           }                                                               \
         )                                                                 \
         break;                                                            \
       case Rectype_Svector:                                               \
         # Simple-vector: update all pointers                              \
         do_update_svector();                                              \
         break;                                                            \
       case Rectype_Sbvector:                                              \
       case Rectype_Sb2vector:                                             \
       case Rectype_Sb4vector:                                             \
       case Rectype_Sb8vector:                                             \
       case Rectype_Sb16vector:                                            \
       case Rectype_Sb32vector:                                            \
       case Rectype_S8string: case Rectype_Imm_S8string:                   \
       case Rectype_Bignum: case Rectype_Ffloat:                           \
       case Rectype_Dfloat: case Rectype_Lfloat:                           \
         # these contain no pointers that need update -> do nothing        \
         break;                                                            \
       default: # Record: update all pointers                              \
         if (record_type((Record)ptr) < rectype_longlimit)                 \
           do_update_sxrecord();                                           \
         else                                                              \
           do_update_lrecord();                                            \
         break;                                                            \
     }                                                                     \
     # advance to the next object                                          \
     ptr = newptr;                                                         \
   } while(0)
 #endif # TYPECODES
 #define update_varobjects()  \
   for_each_varobject_page(page, update_page(page,update_varobject); )
#endif # SPVW_MIXED
#ifdef SPVW_PURE
 #define update_symbol(type_expr)  # ignores type_expr                 \
   do {                                                                \
     var uintL laenge = objsize_symbol((void*)ptr); # determine length \
     var aint newptr = ptr+laenge; # pointer to the next object        \
     # Symbol: update all pointers                                     \
     do_update_symbol();                                               \
     ptr = newptr; # advance to the next object                        \
   } while(0)
 #define update_svector(type_expr)  # ignores type_expr                 \
   do {                                                                 \
     var uintM laenge = objsize_svector((void*)ptr); # determine length \
     var aint newptr = ptr+laenge; # pointer to the next object         \
     # Simple-vector: update all pointers                               \
     do_update_svector();                                               \
     ptr = newptr; # advance to the next object                         \
   } while(0)
 #define update_iarray(type_expr)  # ignores type_expr                 \
   do {                                                                \
     var uintL laenge = objsize_iarray((void*)ptr); # determine length \
     var aint newptr = ptr+laenge; # pointer to the next object        \
     # non-simple array: update data vector                            \
     do_update_iarray();                                               \
     ptr = newptr; # advance to the next object                        \
   } while(0)
 #define update_sstring(type_expr)  # ignores type_expr                 \
   do {                                                                 \
     var uintL laenge = objsize_sstring((void*)ptr); # determine length \
     var aint newptr = ptr+laenge; # pointer to the next object         \
     if (sstring_reallocatedp((Sstring)ptr)) {                          \
       # reallocated simple string: update data vector                  \
       do_update_sistring();                                            \
     } else if (update_unrealloc) {                                     \
       update_ss_unrealloc((Sstring)ptr); # cleanup forward ptr mark    \
     }                                                                  \
     ptr = newptr; # advance to the next object                         \
   } while(0)
 #define update_sxrecord(type_expr)  # ignores type_expr                 \
   do {                                                                  \
     var uintL laenge = objsize_sxrecord((void*)ptr); # determine length \
     var aint newptr = ptr+laenge; # pointer to the next object          \
     # Record: update all pointers                                       \
     do_update_sxrecord();                                               \
     ptr = newptr; # advance to the next object                          \
   } while(0)
 #define update_lrecord(type_expr)  # ignores type_expr                 \
   do {                                                                 \
     var uintL laenge = objsize_lrecord((void*)ptr); # determine length \
     var aint newptr = ptr+laenge; # pointer to the next object         \
     # Record: update all pointers                                      \
     do_update_lrecord();                                               \
     ptr = newptr; # advance to the next object                         \
   } while(0)
 #define update_varobjects()                                           \
   for_each_varobject_page(page,{                                      \
     # fall differentiation according to:                              \
     # symbol; simple-vector; non-simpler array;                       \
     # Record (esp. hash-table); rest.                                 \
     switch (heapnr) {                                                 \
       case_symbol: update_page(page,update_symbol); break;            \
       case_sstring:                                                   \
         if_HAVE_SMALL_SSTRING( update_page(page,update_sstring); )    \
         break;                                                        \
       case_svector: update_page(page,update_svector); break;          \
       case_mdarray: case_obvector: case_ob2vector: case_ob4vector:    \
       case_ob8vector: case_ob16vector: case_ob32vector: case_ostring: \
       case_ovector: update_page(page,update_iarray); break;           \
       case_sxrecord: update_page(page,update_sxrecord); break;        \
       case_lrecord: update_page(page,update_lrecord); break;          \
       default: # all others contain no pointer that need update       \
         break; # -> do nothing                                        \
     }                                                                 \
   })
#endif # SPVW_PURE


# update STACKs :
#define update_stackobj_normal(objptr)    update(objptr);
#define update_STACKs()                                                       \
  for_all_STACKs(while (!eq(*objptr,nullobj)) { # until STACK is finished:    \
    if ( as_oint(*objptr) & wbit(frame_bit_o) ) { # here starts a frame?      \
      if (( as_oint(*objptr) & wbit(skip2_bit_o) ) == 0) # without skip2-Bit? \
        objptr skipSTACKop 2; # yes -> advance by 2                           \
      else                                                                    \
        objptr skipSTACKop 1; # no -> advance by 1                            \
    } else { # normal object, update:                                         \
      update_stackobj(objptr);                                                \
      objptr skipSTACKop 1; # advance                                         \
   }                                                                          \
  })

/* Update C stacks: */
#define update_back_traces()             \
  for_all_back_traces(                   \
    for (; bt != NULL; bt = bt->bt_next) \
      update(&bt->bt_function))
