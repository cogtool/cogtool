# Walking the heap.

# ------------------------------ Specification --------------------------------

# Walks through all objects, and calls a given function on every object.
  global void map_heap_objects (map_heap_function_t* fun, void* arg);

# ------------------------------ Implementation -------------------------------

# UP: Walks through the whole memory and calls for each
# object obj: fun(arg,obj,bytelen) .
# map_heap_objects(fun,arg)
# > fun: C-function
# > arg: arbitrary given argument
global void map_heap_objects (map_heap_function_t* fun, void* arg) {
  # program constants:
  for_all_subrs({
    fun(arg,subr_tab_ptr_as_object(ptr),sizeof(subr_t));
  });
  for_all_constsyms({
    fun(arg,symbol_tab_ptr_as_object(ptr),sizeof(symbol_));
  });
 #if defined(SPVW_PURE_BLOCKS) # && defined(SINGLEMAP_MEMORY)
  #define varobject_typecode_at(type,p)
  #define cons_typecode_at(type,p)
  #define with_typecode(p)  as_object(p)
 #else
  #ifdef SPVW_MIXED
   #ifdef TYPECODES
    #define varobject_typecode_at(type,p)               \
      var tint type = typecode_at(p);                   \
      switch (type) {                                   \
       case_symbolwithflags: type = symbol_type; break; \
       default: break;                                  \
      }
    #define cons_typecode_at(type,p)    var tint type = cons_type
   #else
    #define varobject_typecode_at(type,p)  var oint type = varobject_bias
    #define cons_typecode_at(type,p)       var oint type = cons_bias
   #endif
  #endif
  #ifdef SPVW_PURE
   #define varobject_typecode_at(type,p)    var tint type = heapnr
   #define cons_typecode_at(type,p)         var tint type = heapnr
  #endif
  #ifdef TYPECODES
   #define with_typecode(p)  type_pointer_object(type,p)
  #else
   #define with_typecode(p)  as_object((oint)(p)+(oint)type)
  #endif
 #endif
 #ifdef GENERATIONAL_GC
  # objects of variable length:
  for_each_varobject_heap(heap, {
    var_prepare_objsize;
    {
      var aint p = heap->heap_gen0_start;
      var aint p_end = heap->heap_gen0_end;
      while (p != p_end) {
        varobject_typecode_at(type,p);
        var uintM laenge = objsize((Varobject)p);
        fun(arg,with_typecode(p),laenge);
        p += laenge;
      }
    }
    {
      var aint p = heap->heap_gen1_start;
      var aint p_end = heap->heap_end;
      while (p != p_end) {
        varobject_typecode_at(type,p);
        var uintM laenge = objsize((Varobject)p);
        fun(arg,with_typecode(p),laenge);
        p += laenge;
      }
    }
  });
  # two-pointer-objects:
  #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
  for_each_cons_heap(heap, {
    {
      var aint p = heap->heap_start;
      var aint p_end = heap->heap_gen1_end;
      while (p != p_end) {
        cons_typecode_at(type,p);
        fun(arg,with_typecode(p),sizeof(cons_));
        p += sizeof(cons_);
      }
    }
    {
      var aint p = heap->heap_gen0_start;
      var aint p_end = heap->heap_gen0_end;
      while (p != p_end) {
        cons_typecode_at(type,p);
        fun(arg,with_typecode(p),sizeof(cons_));
        p += sizeof(cons_);
      }
    }
  });
  #else
  for_each_cons_heap(heap, {
    {
      var aint p = heap->heap_gen0_start;
      var aint p_end = heap->heap_gen0_end;
      while (p != p_end) {
        cons_typecode_at(type,p);
        fun(arg,with_typecode(p),sizeof(cons_));
        p += sizeof(cons_);
      }
    }
    {
      var aint p = heap->heap_gen1_start;
      var aint p_end = heap->heap_end;
      while (p != p_end) {
        cons_typecode_at(type,p);
        fun(arg,with_typecode(p),sizeof(cons_));
        p += sizeof(cons_);
      }
    }
  });
  #endif # SPVW_MIXED_BLOCKS_OPPOSITE
 #else
  # objects of variable length:
  for_each_varobject_page(page, {
    var aint p = page->page_start;
    var aint p_end = page->page_end;
    var_prepare_objsize;
    while (p != p_end) {
      varobject_typecode_at(type,p);
      var uintM laenge = objsize((Varobject)p);
      fun(arg,with_typecode(p),laenge);
      p += laenge;
    }
  });
  # two-pointer-objects:
  for_each_cons_page(page, {
    var aint p = page->page_start;
    var aint p_end = page->page_end;
    while (p != p_end) {
      cons_typecode_at(type,p);
      fun(arg,with_typecode(p),sizeof(cons_));
      p += sizeof(cons_);
    }
  });
 #endif # GENERATIONAL_GC
  #undef varobject_typecode_at
  #undef cons_typecode_at
  #undef with_typecode
}
