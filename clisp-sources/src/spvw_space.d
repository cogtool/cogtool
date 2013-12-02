# Statistics about heap space usage.

# ------------------------------ Specification --------------------------------

# Returns the size of the memory occupied by static (gcinvariant) heap objects.
  global uintM static_space (void);

# Returns the size of the memory occupied by (dynamically) allocated heap
# objects.
  global uintM used_space (void);

# Returns the size of memory available until the next GC occurs.
  global uintM free_space (void);

#ifdef SPVW_PAGES
# Recomputes mem.used_space and mem.total_space.
# > bool check: if true, mem.used_space must remain the same.
  local void recalc_space (bool check);
#endif

# ------------------------------ Implementation -------------------------------

global uintM static_space (void) {
  var uintM sum = 0;
  # space of symbol_tab: cf. macro for_all_constsyms
  sum += symbol_anz * sizeof(symbol_);
  # space of subr_tab: cf. macro for_all_subrs
 #ifdef MAP_MEMORY_TABLES
  sum += total_subr_anz * sizeof(subr_t);
 #else
  {
    var module_t* module; # traverse modules
    for_modules(all_modules, {
      if (module->initialized)
        sum += *module->stab_size * sizeof(subr_t);
    });
  }
 #endif
  return sum;
}

  global uintM used_space (void);
  #ifdef SPVW_BLOCKS
   #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
    global uintM used_space() {
     #if !defined(GENERATIONAL_GC)
      #define Heap_used_space(h)  ((uintM)((h).pages.end - (h).pages.start))
      return Heap_used_space(mem.varobjects) # space for objects of variable length
        + Heap_used_space(mem.conses); # space for conses
     #else # defined(GENERATIONAL_GC)
      return
        (uintM)(mem.varobjects.heap_gen0_end - mem.varobjects.heap_gen0_start)
        + (uintM)(mem.varobjects.heap_end - mem.varobjects.heap_gen1_start)
        + (uintM)(mem.conses.heap_gen1_end - mem.conses.heap_start)
        + (uintM)(mem.conses.heap_gen0_end - mem.conses.heap_gen0_start);
     #endif
    }
   #else
    global uintM used_space() {
      var uintM sum = 0;
     #if !defined(GENERATIONAL_GC)
      for_each_page(page, {
        sum += page->page_end - page->page_start;
      });
     #else # defined(GENERATIONAL_GC)
      for_each_heap(heap, {
        sum += (heap->heap_gen0_end - heap->heap_gen0_start)
          + (heap->heap_end - heap->heap_gen1_start);
      });
     #endif
      return sum;
    }
   #endif
  #endif
  #ifdef SPVW_PAGES
    #if 0
    global uintM used_space() {
      var uintM sum = 0;
      for_each_page(page, {
        sum += page->page_end - page->page_start;
      });
      return sum;
    }
    #else
    # the calculation of used_space() accesses each page once, which
    # can cause lots of paging, so the result is saved in mem.used_space.
    global uintM used_space() {
      return mem.used_space;
    }
    #endif
  #endif

  global uintM free_space (void);
  #ifdef SPVW_BLOCKS
   #if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && !defined(TRIVIALMAP_MEMORY) && !defined(GENERATIONAL_GC)
    global uintM free_space() { # space in the big gap
      return (mem.conses.heap_start-mem.varobjects.heap_end);
    }
   #else
    global uintM free_space() {
      return mem.total_room; # space, that may be consumed until the next GC
    }
   #endif
  #endif
  #ifdef SPVW_PAGES
    #if 0
    global uintM free_space() {
      var uintM sum = 0;
      for_each_page(page, {
        sum += page->page_room;
      });
      return sum;
    }
    #else
    # the calculation of free_space() accesses each page once, which
    # can cause lots of paging, so the result is calculated with
    # help of mem.used_space.
    global uintM free_space() {
      return mem.total_space - mem.used_space;
    }
    #endif
  #endif

#ifdef SPVW_PAGES
local void recalc_space (bool check) {
  var uintM sum_used = 0;
  var uintM sum_free = 0;
  for_each_page(page, {
    sum_used += page->page_end - page->page_start;
    sum_free += page->page_room;
  });
  if (check) {
    if (!(mem.used_space == sum_used))
      abort();
  } else {
    mem.used_space = sum_used;
  }
  mem.total_space = sum_used + sum_free;
}
#endif
