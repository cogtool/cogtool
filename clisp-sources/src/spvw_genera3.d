# Support for GENERATIONAL_GC, part 3.

# ------------------------------ Specification --------------------------------

#ifdef GENERATIONAL_GC

# Update the pointers inside the old generation.
# This assumes the current meaning of the `update' macro!
  local void update_old_generation (void);

#endif

# ------------------------------ Implementation -------------------------------

#ifdef GENERATIONAL_GC

local void update_at (gcv_object_t* ptr) {
  update(ptr);
}

local void update_old_generation (void) {
  var uintL heapnr;
  for (heapnr=0; heapnr<heapcount; heapnr++)
    if (is_heap_containing_objects(heapnr)) {
      # objects that contain no pointers do not need to be traversed.
      var Heap* heap = &mem.heaps[heapnr];
      var aint gen0_start = heap->heap_gen0_start;
      var aint gen0_end = heap->heap_gen0_end;
      if (gen0_start < gen0_end) {
        if (heap->physpages==NULL) {
          walk_area_(heapnr,gen0_start,gen0_end,update_at); # fallback
        } else {
          var physpage_state_t* physpage = heap->physpages;
          gen0_start &= -physpagesize;
          do {
            if ((physpage->protection == PROT_NONE)
                || (physpage->protection == PROT_READ)) {
              # take advantage of cache, update cached pointers:
              var uintL count = physpage->cache_size;
              if (count > 0) {
                var old_new_pointer_t* ptr = physpage->cache;
                do {
                  #if defined(DEBUG_SPVW) && !defined(MORRIS_GC)
                  bool was_cons = consp(ptr->o);
                  #endif
                  update(&ptr->o);
                  #if !defined(MORRIS_GC)
                  DEBUG_SPVW_ASSERT(was_cons
                                    ? consp(ptr->o) && is_valid_cons_address(as_oint(ptr->o))
                                    : !consp(ptr->o) && is_valid_varobject_address(as_oint(ptr->o)));
                  #endif
                  ptr++;
                } while (--count > 0);
                if (!(physpage->protection == PROT_NONE)) {
                  xmmprotect(heap, gen0_start,physpagesize,PROT_NONE);
                  physpage->protection = PROT_NONE;
                }
              }
            } else {
              # update the entire page-content:
              walk_physpage_(heapnr,physpage,gen0_start+physpagesize,gen0_end,update_at);
            }
            gen0_start += physpagesize;
            physpage++;
          } while (gen0_start < gen0_end);
        }
      }
    }
}

#endif
