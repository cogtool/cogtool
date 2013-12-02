/* Garbage collector. */

/* --------------------------- Specification --------------------------- */

/* Execute a simple garbage collection.
 can trigger GC */
local maygc void gar_col_simple (void);

/* Execute a full garbage collection.
 can trigger GC */
global maygc void gar_col (void);

#ifdef SPVW_PAGES
/* Supplement a simple garbage collection with a compactification.
 can trigger GC */
local maygc void gar_col_compact (void);
#endif

#if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && RESERVE
/* Move the conses, to make a little more room. */
local void move_conses (sintM delta);
#endif

/* --------------------------- Implementation -------------------------- */

/* overall strategy:
 1. pseudo-recursive marking by setting of garcol_bit.
 2. relocate objects of fixed length (conses and similar),
    calculation of displacement of objects of variable length.
 3. update of pointers.
 4. perform the displacements of objects of variable length.
*/

#include "spvw_genera1.c"

/* marking-subroutine
 procedure: marking routine without stack usage (i.e.
  non-"recursive") by descent into the structure to be marked
  with pointer-modification (pointers are reversed,
  so that they can serve as "ariadne thread")
 Convention: an object X counts as marked, if
  - an object of variable length: bit garcol_bit,(X) is set
  - a two-pointer-object: bit garcol_bit,(X) is set
  - a SUBR/FSUBR: bit garcol_bit,(X+const_offset) is set
  - Character, Short-Float, Fixnum etc.: always. */

#if DEBUG_GC_MARK
  #define IF_DEBUG_GC_MARK(statement)  statement
  #if defined(WIDE_SOFT) || defined(WIDE_AUXI)
    /* oint is defined as uint64. */
    #define PRIoint "ll"
  #else
    /* oint is defined as uintP. Assume pointer_bitsize == long_bitsize. */
    #define PRIoint "l"
  #endif
#else
  #define IF_DEBUG_GC_MARK(statement)  /*nop*/
#endif

#define MARK(obj) mark(obj)
#include "spvw_gcmark.c"
#undef MARK

/* pack a pointer into an object, without  typeinfo.
 pointer_as_object(ptr): void* --> object
 pointer_was_object(obj): object --> void* */
  #ifdef TYPECODES
    #define pointer_as_object(ptr)  type_pointer_object(0,ptr)
    #define pointer_was_object(obj)  type_pointable(0,obj)
  #else
    #if defined(WIDE_AUXI)
      #define pointer_as_object(ptr)  as_object_with_auxi((aint)(ptr))
      #define pointer_was_object(obj)  ((void*)((obj).one_o))
    #else
      #define pointer_as_object(ptr)  as_object((oint)(ptr))
      #define pointer_was_object(obj)  ((void*)as_oint(obj))
    #endif
  #endif

/*  marking phase:
 All "active" structures are marked.
 everything is active, that is reachable
 - from the LISP-stack or
 - at Generational-GC: from the old generation or
 - as program-constant (the list of all packages belongs to this). */
local void gc_mark_stack (gcv_object_t* objptr)
{
  while (!eq(*objptr,nullobj)) { /* until STACK is finished: */
    IF_DEBUG_GC_MARK(fprintf(stderr,"gc_mark_stack: 0x%lx/%lu (%lu)\n",
                             objptr,objptr,as_oint(*objptr)));
    if (as_oint(*objptr) & wbit(frame_bit_o)) { /* does a frame start here? */
      if ((as_oint(*objptr) & wbit(skip2_bit_o)) == 0) /* without skip2-Bit? */
        objptr skipSTACKop 2; /* yes -> advance by 2 */
      else
        objptr skipSTACKop 1; /* no -> advance by 1 */
    } else { /* normal object, mark: */
      var object obj = *objptr;
     #ifndef NO_symbolflags
      switch (typecode(obj)) { /* poss. remove Symbol-flags */
        case_symbolflagged:
          obj = symbol_without_flags(obj);
        default: break;
      }
     #endif
      gc_mark(obj);
      objptr skipSTACKop 1; /* advance */
    }
  }
}

#include "spvw_genera2.c"

local void gc_markphase (void)
{
  /* Mark all the STACKs */
  for_all_STACKs(gc_mark_stack(objptr));
 #ifdef GENERATIONAL_GC
  /* mark old generation, whereas it is perused sparingly: */
  if (generation > 0) { gc_mark_old_generation(); }
 #endif
  /* mark all program constants: */
 #if !defined(GENERATIONAL_GC)
  for_all_subrs(gc_mark(subr_tab_ptr_as_object(ptr));); /* subr_tab */
  for_all_constsyms(gc_mark(symbol_tab_ptr_as_object(ptr));); /* symbol_tab */
 #else
  /* Because of the macro in_old_generation(), gc_mark() may regard all
     constant symbols and all subrs as belonging to the old generation and
     may not walk through their pointers recursively. So do it by hand. */
  for_all_subrs({ /* peruse subr_tab */
    gc_mark(ptr->name);
    gc_mark(ptr->keywords);
  });
  for_all_constsyms({ /* peruse symbol_tab */
    gc_mark(ptr->symvalue);
    gc_mark(ptr->symfunction);
    gc_mark(ptr->hashcode);
    gc_mark(ptr->proplist);
    gc_mark(ptr->pname);
    gc_mark(ptr->homepackage);
  });
 #endif
  for_all_constobjs( gc_mark(*objptr); ); /* object_tab */
  for_all_threadobjs( gc_mark(*objptr); ); /* threads */
  /* The callers in back_trace are mostly already marked:
     they refer to subrs and closures that are currently being
     called and therefore cannot possibly be garbage-collected.
     But a few remain unmarked, so make sure all are really marked: */
  for_all_back_traces({
    for (; bt != NULL; bt = bt->bt_next)
      gc_mark(bt->bt_function);
  });
}

# UP: Determine, if an object is still "live".
# I.e. if the mark bit is set after the marking phase.
  local bool alive (object obj)
  {
    #ifdef TYPECODES
    switch (typecode(obj)) { # according to type
      case_pair: # Cons
        if (in_old_generation(obj,typecode(obj),1)) return true;
        if (marked(ThePointer(obj))) return true; else return false;
      case_symbol: # Symbol
      case_array: # Array
      case_bignum: # Bignum
      #ifndef IMMEDIATE_FFLOAT
      case_ffloat: # Single-Float
      #endif
      case_dfloat: # Double-Float
      case_lfloat: # Long-Float
      case_record: # Record
        if (in_old_generation(obj,typecode(obj),0)) return true;
        if (marked(ThePointer(obj))) return true; else return false;
      case_subr: # Subr
        if (marked(TheSubr(obj))) return true; else return false;
      case_machine: # Machine Pointer
      case_char: # Character
      case_system: # Frame-pointer, Small-Read-label, system
      case_fixnum: # Fixnum
      case_sfloat: # Short-Float
      #ifdef IMMEDIATE_FFLOAT
      case_ffloat: # Single-Float
      #endif
        return true;
      default:
        # these are no objects.
        /*NOTREACHED*/ abort();
    }
    #else
    switch (as_oint(obj) & nonimmediate_heapcode_mask) {
      case varobject_bias+varobjects_misaligned:
        if (in_old_generation(obj,,0)) return true;
        if (marked(ThePointer(obj))) return true; else return false;
      case cons_bias+conses_misaligned:
        #ifdef STANDARD_HEAPCODES
        /* NB: (immediate_bias & nonimmediate_heapcode_mask) == cons_bias. */
        if (immediate_object_p(obj)) return true;
        #endif
        if (in_old_generation(obj,,1)) return true;
        if (marked(ThePointer(obj))) return true; else return false;
      #ifdef STANDARD_HEAPCODES
      case subr_bias:
        if (marked(TheSubr(obj))) return true; else return false;
      #endif
      default:
        return true;
    }
    #endif
  }

#include "spvw_weak.c"

# unmark SUBRs and fixed Symbols:
  local void unmark_fixed_varobjects (void)
  {
    /* Even if defined(GENERATIONAL_GC), because the macro in_old_generation()
       has undefined behaviour for constsyms and subrs, therefore we don't know
       a priori whether the constsyms and subrs have their mark bit set. */
    for_all_subrs( unmark(&((Subr)ptr)->GCself); ); # unmark each Subr
    for_all_constsyms( unmark(&((Symbol)ptr)->GCself); ); # unmark each Symbol in symbol_tab
  }

#if !defined(MORRIS_GC)

 #ifdef SPVW_MIXED_BLOCKS_OPPOSITE

  # concentrate CONS-cells between page->page_start and page->page_end
  # aloft:
  local void gc_compact_cons_page (Page* page)
  {
    # the pointer p1 moves from below and the pointer p2 from
    # above through the memory region, until they collide.
    # Marked structures are moved above unmarked.
    var aint p1 = page->page_start; # lower bound
    var aint p2 = page->page_end; # upper bound
    sweeploop:
      # search the next-higher unmarked cell <p2 and unmark all:
      sweeploop1:
        if (p1==p2) goto sweepok2; # bounds are equal -> finished
        p2 -= sizeof(cons_); # capture next cell from above
        if (marked(p2)) { # marked?
          unmark(p2); # unmark
          goto sweeploop1;
        }
      # p1 <= p2, p2 points to an unmarked cell.
      # search next lower marked cell >=p1:
      sweeploop2:
        if (p1==p2) goto sweepok1; # bounds are equal -> finished
        if (!marked(p1)) { # unmarked?
          p1 += sizeof(cons_); # at the next lower cell
          goto sweeploop2; # continue search
        }
      # p1 < p2, p1 points to a marked cell.
      unmark(p1); # unmark
      # copy content of cell into the unmark cell:
      ((gcv_object_t*)p2)[0] = ((gcv_object_t*)p1)[0];
      ((gcv_object_t*)p2)[1] = ((gcv_object_t*)p1)[1];
      *(gcv_object_t*)p1 = pointer_as_object(p2); # leave new addresse
      mark(p1); # and mark (as identification for the update)
      p1 += sizeof(cons_); # this cell is finished.
      goto sweeploop; # continue
    sweepok1: p1 += sizeof(cons_); # skip last unmarked Cons
    sweepok2:
    # p1 = new lower bound of the Cons-region
    page->page_start = p1;
  }

 #else

  # concentrate CONS-cells between page->page_start and page->page_end
  # below:
  local void gc_compact_cons_page (Page* page)
  {
    # the pointer p1 moves from below and the pointer p2 from
    # above through the memory region, until they collide.
    # Marked structures are moved above unmarked.
    var aint p1 = page->page_start; # lower bound
    var aint p2 = page->page_end; # upper bound
    sweeploop:
      # search next higher marked cell <p2:
      sweeploop1:
        if (p1==p2) goto sweepok2; # bounds are equal -> finished
        p2 -= sizeof(cons_); # capture next cell from above
        if (!marked(p2)) goto sweeploop1; # unmarked?
      # p1 <= p2, p2 points to a marked cell.
      unmark(p2); # unmark
      # search next lower unmarked cell >=p1 and unmark all:
      sweeploop2:
        if (p1==p2) goto sweepok1; # bounds are equal -> finished
        if (marked(p1)) { # marked?
          unmark(p1); # unmark
          p1 += sizeof(cons_); # at next upper cell
          goto sweeploop2; # continue search
        }
      # p1 < p2, p1 points to an unmarked cell.
      # copy cell content from the marked into the unmark cell:
      ((gcv_object_t*)p1)[0] = ((gcv_object_t*)p2)[0];
      ((gcv_object_t*)p1)[1] = ((gcv_object_t*)p2)[1];
      *(gcv_object_t*)p2 = pointer_as_object(p1); # leave new address
      mark(p2); # and mark (as identification for update)
      p1 += sizeof(cons_); # this cell is finished.
      goto sweeploop; # continue
    sweepok1: p1 += sizeof(cons_); # skip last marked Cons
    sweepok2:
    # p1 = new upper bound of the Cons-region
    page->page_end = p1;
  }

 #endif

#else # defined(MORRIS_GC)

# Algorithm see:
# [F. Lockwood Morris: A time- and space-efficient garbage collection algorithm.
#  CACM 21,8 (August 1978), 662-665.]

  # Delete all unmarked CONS-cells and unmark the marked CONS-cells,
  # so that the mark bit is available for the reverse spointers.
  local void gc_morris1 (Page* page)
  {
    var aint p1 = page->page_start; # lower bound
    var aint p2 = page->page_end; # upper bound
    var aint d = 0; # also count free memory
    until (p1==p2) {
      if (!marked(p1)) {
        ((gcv_object_t*)p1)[0] = nullobj;
        ((gcv_object_t*)p1)[1] = nullobj;
        d += sizeof(cons_);
      } else {
        unmark(p1);
        #ifdef DEBUG_SPVW
        if (eq(((gcv_object_t*)p1)[0],nullobj) || eq(((gcv_object_t*)p1)[1],nullobj))
          abort();
        #endif
      }
      p1 += sizeof(cons_); # this cell is finished.
    }
    page->page_gcpriv.d = d; # store free memory
  }

 #ifdef SPVW_MIXED_BLOCKS_OPPOSITE

  # There is only one page with Two-Pointer-Objects.

  local void gc_morris2 (Page* page)
  {
    # Each cell within a Cons now contains a list of all
    # addresses of pointers to this cell, that point to this cell
    # from a root or from a Varobject.
    #
    # traverse the undeleted conses from left to right:
    # (in between, each cell contains a list of all addresses
    # of pointers to this cell, that point to this cell from a root,
    # from a varobject or a cons lying further to the left.)
    var aint p1 = page->page_start; # lower bound
    var aint p2 = p1 + page->page_gcpriv.d; # later lower bound
    var aint p1limit = page->page_end; # upper bound
    until (p1==p1limit) { # always: p1 <= p2 <= p1limit
      # both cells of a cons are treated exactly the same.
      var object obj = *(gcv_object_t*)p1;
      if (!eq(obj,nullobj)) {
        # p1 is moved to p2.
        #ifdef TYPECODES
          # the so far registered pointers to this cell are updated:
          until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) { # process list
            obj = without_mark_bit(obj);
            var aint p = upointer(obj);
            var object next_obj = *(gcv_object_t*)p;
            *(gcv_object_t*)p = type_pointer_object(typecode(obj),p2);
            obj = next_obj;
          }
          # if the cell contains a pointer "to the right",
          # it is reversed.
          {
            var tint type = typecode(obj);
            switch (type) {
              case_pair:
                {
                  var aint p = upointer(obj);
                  if (!in_old_generation(obj,type,1) && (p > p1)) {
                    # For later update, insert
                    # p1 in the list of pointers to p:
                    *(gcv_object_t*)p1 = *(gcv_object_t*)p;
                    *(gcv_object_t*)p = with_mark_bit(type_pointer_object(type,p1));
                    break;
                  }
                }
              default:
                *(gcv_object_t*)p1 = obj;
            }
          }
        #else # no TYPECODES
          # the so far registered pointers to this cell are updated:
          until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) { # process list
            obj = without_mark_bit(obj);
            var aint p = (aint)ThePointer(obj);
            var object next_obj = *(gcv_object_t*)p;
            *(gcv_object_t*)p = as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2);
            obj = next_obj;
          }
          # if the cell contains a pointer "to the right",
          # it is reversed.
          if (consp(obj)) {
            var aint p = (aint)ThePointer(obj);
            if (!in_old_generation(obj,,1) && (p > p1)) {
              # For later update, insert
              # p1 in the list of pointers to p:
              *(gcv_object_t*)p1 = *(gcv_object_t*)p;
              *(gcv_object_t*)p = with_mark_bit(as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p1));
            } else {
              *(gcv_object_t*)p1 = obj;
            }
          } else {
            *(gcv_object_t*)p1 = obj;
          }
        #endif
        p2 += sizeof(gcv_object_t);
      }
      p1 += sizeof(gcv_object_t);
    }
    if (!(p2==p1limit))
      abort();
  }
  local void gc_morris3 (Page* page)
  {
    # Each cell within a cons now contains again the original
    # content.
    #
    # Traverse the undeleted conses from right to left
    # and compact them on the right:
    # (in between, each cell contains a list of all addresses
    # of pointers to this cell, that point to this cell
    # from a cons lying further to the right.)
    var aint p1limit = page->page_start; # lower bound
    var aint p1 = page->page_end; # upper bound
    var aint p2 = p1; # upper bound
    #ifdef DEBUG_SPVW
    until (p1==p1limit) {
      p1 -= 2*sizeof(gcv_object_t);
      if (eq(*(gcv_object_t*)p1,nullobj)+eq(*(gcv_object_t*)(p1^sizeof(gcv_object_t)),nullobj)==1)
        abort();
    }
    p1 = page->page_end;
    #endif
    until (p1==p1limit) { # always: p1limit <= p1 <= p2
      # both cells of a cons are treated exactly the same.
      p1 -= sizeof(gcv_object_t);
      #ifdef DEBUG_SPVW
      if (eq(*(gcv_object_t*)p1,nullobj)+eq(*(gcv_object_t*)(p1^sizeof(gcv_object_t)),nullobj)==1)
        abort();
      if (!((p1 % (2*sizeof(gcv_object_t))) == 0)) {
        if (!((p2 % (2*sizeof(gcv_object_t))) == 0))
          abort();
      }
      #endif
      var object obj = *(gcv_object_t*)p1;
      if (!eq(obj,nullobj)) {
        p2 -= sizeof(gcv_object_t);
        # p1 is moved to p2.
        #ifdef TYPECODES
          # The newly registered pointers to this cell are updated:
          until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) { # process list
            obj = without_mark_bit(obj);
            var aint p = upointer(obj);
            var object next_obj = *(gcv_object_t*)p;
            *(gcv_object_t*)p = type_pointer_object(typecode(obj),p2);
            obj = next_obj;
          }
          #ifdef DEBUG_SPVW
          if (eq(obj,nullobj)) abort();
          #endif
          *(gcv_object_t*)p2 = obj;
          {
            var tint type = typecode(obj);
            if (!gcinvariant_type_p(type)) # un-movable -> do nothing
              switch (type) {
                case_pair: # Two-Pointer-Object
                  {
                    var aint p = upointer(obj);
                    if (p < p1) { # pointer to the left?
                      # For later update, insert
                      # p2 into the list of pointers to p:
                      #ifdef DEBUG_SPVW
                      if (eq(*(gcv_object_t*)p,nullobj)) abort();
                      #endif
                      *(gcv_object_t*)p2 = *(gcv_object_t*)p;
                      *(gcv_object_t*)p = with_mark_bit(type_pointer_object(type,p2));
                    } elif (p == p1) { # pointer to itself?
                      *(gcv_object_t*)p2 = type_pointer_object(type,p2);
                    }
                  }
                  break;
                default: # object of variable length
                  if (marked(ThePointer(obj))) # marked?
                    *(gcv_object_t*)p2 = type_untype_object(type,untype(*(gcv_object_t*)ThePointer(obj)));
                  break;
              }
          }
        #else # no TYPECODES
          # The newly registered pointers to this cell are updated:
          until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) { # process list
            obj = without_mark_bit(obj);
            var aint p = (aint)ThePointer(obj);
            var object next_obj = *(gcv_object_t*)p;
            *(gcv_object_t*)p = as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2);
            obj = next_obj;
          }
          #ifdef DEBUG_SPVW
          if (eq(obj,nullobj)) abort();
          #endif
          *(gcv_object_t*)p2 = obj;
          if (!gcinvariant_object_p(obj)) { # un-movable -> do nothing
            if (consp(obj)) {
              # Two-Pointer-Object
              var aint p = (aint)ThePointer(obj);
              if (p < p1) { # pointer to the left?
                # for later update, insert
                # p2 into the list of pointers to p:
                #ifdef DEBUG_SPVW
                if (eq(*(gcv_object_t*)p,nullobj)) abort();
                #endif
                *(gcv_object_t*)p2 = *(gcv_object_t*)p;
                *(gcv_object_t*)p = with_mark_bit(as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2));
              } elif (p == p1) { # pointer to itself?
                *(gcv_object_t*)p2 = as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2);
              }
            } else {
              # object of variable length
              if (marked(ThePointer(obj))) # marked?
                *(gcv_object_t*)p2 = as_object((as_oint(obj) & nonimmediate_bias_mask) | (as_oint(*(gcv_object_t*)ThePointer(obj)) & ~wbit(garcol_bit_o) & ~(oint)nonimmediate_bias_mask));
            }
          }
        #endif
      }
    }
    # p2 = new lower bound of the Cons-region
    if (!(p2 == page->page_start + page->page_gcpriv.d))
      abort();
    page->page_start = p2;
  }

 #elif defined(SPVW_MIXED_BLOCKS_STAGGERED)

  local void gc_morris2 (Page* page)
  {
    # Each cell within a Cons now contains a list of all
    # addresses of pointers to this cell, that point to this cell
    # from a root or from a Varobject.
    #
    # Traverse the undeleted conses from right to left:
    # (in between, each cell contains a liste of all addresses
    # of pointers to this cell, that point to this cell from a root,
    # from a varobject or a cons lying further to the right.)
    var aint p1 = page->page_end; # upper bound
    var aint p2 = p1 - page->page_gcpriv.d; # later upper bound
    var aint p1limit = page->page_start; # lower bound
    #ifdef DEBUG_SPVW
    until (p1==p1limit) {
      p1 -= 2*sizeof(gcv_object_t);
      if (eq(*(gcv_object_t*)p1,nullobj)+eq(*(gcv_object_t*)(p1^sizeof(gcv_object_t)),nullobj)==1)
        abort();
    }
    p1 = page->page_end;
    #endif
    until (p1==p1limit) { # always: p1limit <= p2 <= p1
      # both cells of a cons are treated exactly the same.
      p1 -= sizeof(gcv_object_t);
      #ifdef DEBUG_SPVW
      if (eq(*(gcv_object_t*)p1,nullobj)+eq(*(gcv_object_t*)(p1^sizeof(gcv_object_t)),nullobj)==1)
        abort();
      #endif
      var object obj = *(gcv_object_t*)p1;
      if (!eq(obj,nullobj)) {
        p2 -= sizeof(gcv_object_t);
        # p1 is moved to p2.
        #ifdef TYPECODES
          # the so far registered pointers to this cell are updated:
          until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) { # process list
            obj = without_mark_bit(obj);
            var aint p = upointer(obj);
            var object next_obj = *(gcv_object_t*)p;
            *(gcv_object_t*)p = type_pointer_object(typecode(obj),p2);
            obj = next_obj;
          }
          # obj = original content of the cell p1.
          #ifdef DEBUG_SPVW
          if (eq(obj,nullobj)) abort();
          #endif
          # if the cell contains a pointer "to the left", it is reversed.
          {
            var tint type = typecode(obj);
            switch (type) {
              case_pair:
                {
                  var aint p = upointer(obj);
                  if (!in_old_generation(obj,type,1) && (p < p1)) {
                    # For later update, insert
                    # p1 into the list of pointers to p:
                    *(gcv_object_t*)p1 = *(gcv_object_t*)p;
                    *(gcv_object_t*)p = with_mark_bit(type_pointer_object(type,p1));
                    break;
                  }
                }
              default:
                *(gcv_object_t*)p1 = obj;
            }
          }
        #else
          # the so far registered pointers to this cell are updated:
          until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) { # process list
            obj = without_mark_bit(obj);
            var aint p = (aint)ThePointer(obj);
            var object next_obj = *(gcv_object_t*)p;
            *(gcv_object_t*)p = as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2);
            obj = next_obj;
          }
          # obj = original content of the cell p1.
          #ifdef DEBUG_SPVW
          if (eq(obj,nullobj)) abort();
          #endif
          # if the cell contains a pointer "to the left", it is reversed.
          if (consp(obj)) {
            var aint p = (aint)ThePointer(obj);
            if (!in_old_generation(obj,,1) && (p < p1)) {
              # For later update, insert
              # p1 into the list of pointers to p:
              *(gcv_object_t*)p1 = *(gcv_object_t*)p;
              *(gcv_object_t*)p = with_mark_bit(as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p1));
            } else {
              *(gcv_object_t*)p1 = obj;
            }
          } else {
            *(gcv_object_t*)p1 = obj;
          }
        #endif
      }
    }
    if (!(p2==p1limit))
      abort();
  }
  local void gc_morris3 (Page* page)
  {
    # Each cell within a cons now contains again the original
    # content.
    #
    # Traverse the undeleted conses from left to right
    # and compact them on the left:
    # (in between, each cell contains a list of all addresses
    # of pointers to this cell, that point to this cell
    # from a cons lying further to the left.)
    var aint p1limit = page->page_end; # obere Grenze
    var aint p1 = page->page_start; # lower bound
    var aint p2 = p1; # lower bound
    until (p1==p1limit) { # always: p1limit <= p1 <= p2
      # both cells of a cons are treated exactly the same.
      var object obj = *(gcv_object_t*)p1;
      if (!eq(obj,nullobj)) {
        # p1 is moved to p2.
        #ifdef TYPECODES
          # The newly registered pointers to this cell are updated:
          until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) { # process list
            obj = without_mark_bit(obj);
            var aint p = upointer(obj);
            var object next_obj = *(gcv_object_t*)p;
            *(gcv_object_t*)p = type_pointer_object(typecode(obj),p2);
            obj = next_obj;
          }
          # obj = true content of the cell p1.
          {
            var tint type = typecode(obj);
            if (!gcinvariant_type_p(type)) # un-movable -> do nothing
              switch (type) {
                case_pair: # Two-Pointer-Object
                  {
                    var aint p = upointer(obj);
                    if (p > p1) { # pointer to the right?
                      # For later update, insert
                      # p2 into the list of pointers to p:
                      #ifdef DEBUG_SPVW
                      if (eq(*(gcv_object_t*)p,nullobj)) abort();
                      #endif
                      *(gcv_object_t*)p2 = *(gcv_object_t*)p;
                      *(gcv_object_t*)p = with_mark_bit(type_pointer_object(type,p2));
                    } elif (p == p1) { # Pointer to itself?
                      *(gcv_object_t*)p2 = type_pointer_object(type,p2);
                    } else {
                      *(gcv_object_t*)p2 = obj;
                    }
                  }
                  break;
                default: # object of variable length
                  if (marked(ThePointer(obj))) # marked?
                    *(gcv_object_t*)p2 = type_untype_object(type,untype(*(gcv_object_t*)ThePointer(obj)));
                  else
                    *(gcv_object_t*)p2 = obj;
                  break;
              }
            else { # un-movable or pointer into the old generation -> do nothing
              *(gcv_object_t*)p2 = obj;
            }
          }
        #else
          # The newly registered pointers to this cell are updated:
          until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) { # process list
            obj = without_mark_bit(obj);
            var aint p = (aint)ThePointer(obj);
            var object next_obj = *(gcv_object_t*)p;
            *(gcv_object_t*)p = as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2);
            obj = next_obj;
          }
          # obj = true content of the cell p1.
          if (!gcinvariant_object_p(obj)) { # un-movable -> do nothing
            if (consp(obj)) {
              # Two-Pointer-Object
              var aint p = (aint)ThePointer(obj);
              if (p > p1) { # pointer to the right?
                # For later update, insert
                # p2 into the list of pointers to p:
                #ifdef DEBUG_SPVW
                if (eq(*(gcv_object_t*)p,nullobj)) abort();
                #endif
                *(gcv_object_t*)p2 = *(gcv_object_t*)p;
                *(gcv_object_t*)p = with_mark_bit(as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2));
              } elif (p == p1) { # pointer to itself?
                *(gcv_object_t*)p2 = as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)p2);
              } else {
                *(gcv_object_t*)p2 = obj;
              }
            } else {
              # Object of variable length
              if (marked(ThePointer(obj))) # marked?
                *(gcv_object_t*)p2 = as_object((as_oint(obj) & nonimmediate_bias_mask) | (as_oint(*(gcv_object_t*)ThePointer(obj)) & ~wbit(garcol_bit_o) & ~(oint)nonimmediate_bias_mask));
              else
                *(gcv_object_t*)p2 = obj;
            }
          } else { # un-movable or pointer into the old generation -> do nothing
            *(gcv_object_t*)p2 = obj;
          }
        #endif
        p2 += sizeof(gcv_object_t);
      }
      p1 += sizeof(gcv_object_t);
    }
    # p2 = new upper bound of the Cons-region
    if (!(p2 == page->page_end - page->page_gcpriv.d))
      abort();
    page->page_end = p2;
  }

 #else # SPVW_PURE_BLOCKS <==> SINGLEMAP_MEMORY

  # gc_morris2 and gc_morris3 must be called for each page exactly once,
  # first gc_morris2 from right to left, then gc_morris3 from left to right
  # (in terms of the positioning of the addresses)!

  local void gc_morris2 (Page* page)
  {
    # Each cell within a Cons now contains a list of all
    # addresses of pointers to this cell, that point to this cell
    # from a root or from a Varobject.
    #
    # Traverse the undeleted conses from right to left:
    # (in between, each cell contains a liste of all addresses
    # of pointers to this cell, that point to this cell from a root,
    # from a varobject or a cons lying further to the right.)
    var aint p1 = page->page_end; # upper bound
    var aint p2 = p1 - page->page_gcpriv.d; # later upper bound
    var aint p1limit = page->page_start; # lower bound
    until (p1==p1limit) { # always: p1limit <= p2 <= p1
      # both cells of a cons are treated exactly the same.
      p1 -= sizeof(gcv_object_t);
      var object obj = *(gcv_object_t*)p1;
      if (!eq(obj,nullobj)) {
        p2 -= sizeof(gcv_object_t);
        # p1 is moved to p2.
        # the so far registered pointers to this cell are updated:
        until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) { # process list
          obj = without_mark_bit(obj);
          var object next_obj = *(gcv_object_t*)pointable(obj);
          *(gcv_object_t*)pointable(obj) = as_object(p2);
          obj = next_obj;
        }
        # obj = original content of the cell p1.
        # if the cell contains a pointer "to the left", it is reversed.
        if (is_cons_heap(typecode(obj))
            && !in_old_generation(obj,typecode(obj),1)
            && ((aint)pointable(obj) < p1)
           ) {
          # For later update, insert
          # p1 into the list of pointers to obj:
          *(gcv_object_t*)p1 = *(gcv_object_t*)pointable(obj);
          *(gcv_object_t*)pointable(obj) = with_mark_bit(as_object(p1));
        } else {
          *(gcv_object_t*)p1 = obj;
        }
      }
    }
    if (!(p2==p1limit))
      abort();
  }
  local void gc_morris3 (Page* page)
  {
    # Each cell within a cons now contains again the original
    # content.
    #
    # Traverse the undeleted conses from left to right
    # and compact them on the left:
    # (in between, each cell contains a list of all addresses
    # of pointers to this cell, that point to this cell
    # from a cons lying further to the left.)
    var aint p1limit = page->page_end; # upper bound
    var aint p1 = page->page_start; # lower bound
    var aint p2 = p1; # lower bound
    until (p1==p1limit) { # always: p1limit <= p1 <= p2
      # both cells of a cons are treated exactly the same.
      var object obj = *(gcv_object_t*)p1;
      if (!eq(obj,nullobj)) {
        # p1 is moved to p2.
        # The newly registered pointers to this cell are updated:
        until ((as_oint(obj) & wbit(garcol_bit_o)) == 0) { # process list
          obj = without_mark_bit(obj);
          var object next_obj = *(gcv_object_t*)pointable(obj);
          *(gcv_object_t*)pointable(obj) = as_object(p2);
          obj = next_obj;
        }
        # obj = true content of cell p1.
        {
          var tint type = typecode(obj);
          if (!is_unused_heap(type) && !in_old_generation(obj,type,?))
            if (is_cons_heap(type)) {
              # Two-Pointer-Object
              if ((aint)pointable(obj) > p1) { # pointer to the right?
                # For later update, insert
                # p2 into the list of pointers to obj:
                *(gcv_object_t*)p2 = *(gcv_object_t*)pointable(obj);
                *(gcv_object_t*)pointable(obj) = with_mark_bit(as_object(p2));
              } elif ((aint)pointable(obj) == p1) { # pointer to itself?
                *(gcv_object_t*)p2 = as_object(p2);
              } else {
                *(gcv_object_t*)p2 = obj;
              }
            } else {
              # object of variable length
              if (marked(ThePointer(obj))) # marked?
                *(gcv_object_t*)p2 = type_untype_object(type,untype(*(gcv_object_t*)ThePointer(obj)));
              else
                *(gcv_object_t*)p2 = obj;
            } else { # un-movable or pointer into the old generation -> do nothing
              *(gcv_object_t*)p2 = obj;
            }
        }
        p2 += sizeof(gcv_object_t);
      }
      p1 += sizeof(gcv_object_t);
    }
    # p2 = new upper bound of the Cons-region
    if (!(p2 == page->page_end - page->page_gcpriv.d))
      abort();
    page->page_end = p2;
  }

 #endif

#endif

# modify the self-pointer of an object of variable length:
# set_GCself(p,type,addr);
# sets p->GCself to type_pointer_object(type,addr).
  #ifdef TYPECODES
    #if !(exact_uint_size_p(oint_type_len) && ((oint_type_shift%hfintsize)==0) && (tint_type_mask == bit(oint_type_len)-1))
      #ifdef MAP_MEMORY
        # addr contains typeinfo
        #define make_GCself(type,addr)  \
          type_pointer_object((type)&(tint_type_mask),(addr)&(oint_addr_mask))
      #else
        # addr contains no typeinfo
        #define make_GCself(type,addr)  \
          type_pointer_object((type)&(tint_type_mask),addr)
      #endif
      #define set_GCself(p,type,addr)  \
        ((Varobject)(p))->GCself = make_GCself(type,addr)
    #else # better: though two memory accesses, but less arithmetics
      #define make_GCself(type,addr)  \
        type_pointer_object((type)&(tint_type_mask),(addr)&~(oint_type_mask))
      #define set_GCself(p,type,addr)  \
        (((Varobject)(p))->GCself = type_pointer_object(0,addr), \
         ((Varobject)(p))->header_flags = (type))
    #endif
  #else
    #define make_GCself(type,addr)  /* ignore type */ \
      as_object((oint)(addr))
    #define set_GCself(p,type,addr)  /* ignore type */ \
      ((Varobject)(p))->GCself = make_GCself(type,addr)
  #endif

#ifdef HAVE_SMALL_SSTRING
# Special handling of forward pointers among simple-strings.
  local void gc_sweep1_sstring_forward (aint p2) {
    var gcv_object_t forward = ((Sistring)p2)->data;
    if (sstring_flags(TheSstring(forward)) & sstringflags_relocated_B) {
      var gcv_object_t target = TheSstring(forward)->GCself;
      var aint backchain = p2;
      for (;;) {
        var gcv_object_t backpointer = ((Varobject)backchain)->GCself;
        ((Varobject)backchain)->GCself = target;
        sstring_flags_set((Sstring)backchain,sstringflags_relocated_B);
        if (sstring_flags((Sstring)backchain) & sstringflags_backpointer_B)
          backchain = (aint)ThePointer(without_mark_bit(backpointer));
        else
          break;
      }
    } else {
      # Leave a backpointer for later fixup.
      # Each string can have only one forward pointer directly pointing
      # to it. This ensures that the backchain is a singly linked list.
      if (sstring_flags(TheSstring(forward)) & sstringflags_backpointer_B)
        /*NOTREACHED*/ abort();
      TheSstring(forward)->GCself = with_mark_bit(make_GCself(sstring_type,p2));
      sstring_flags_set(TheSstring(forward),sstringflags_backpointer_B);
    }
    # Don't reclaim the space at p2 during this GC, because
    # 1. we need the mark bit at p2 so that update() does the
    #    relocation, and the mark bit tells gc_sweep2_varobject_page
    #    that the object is not yet reclaimed.
    # 2. otherwise last_open_ptr may be set to &((Varobject)p2)->GCself
    #    later.
  }
  local void gc_sweep1_sstring_target (aint p2, aint p1) {
    if (sstring_flags((Sstring)p2) & sstringflags_relocated_B)
      /*NOTREACHED*/ abort();
    var gcv_object_t target; target = with_mark_bit(make_GCself(sstring_type,p1));
    var aint backchain = p2;
    for (;;) {
      var gcv_object_t backpointer = ((Varobject)backchain)->GCself;
      ((Varobject)backchain)->GCself = target;
      sstring_flags_set((Sstring)backchain,sstringflags_relocated_B);
      if (sstring_flags((Sstring)backchain) & sstringflags_backpointer_B)
        backchain = (aint)ThePointer(without_mark_bit(backpointer));
      else
        break;
    }
  }
#endif

# Special handling of forward pointers among CLOS instances.
  local void gc_sweep1_instance_forward (aint p2) {
    var gcv_object_t forward = ((Instance)p2)->inst_class_version;
    if (record_flags(TheInstance(forward)) & instflags_relocated_B) {
      var gcv_object_t target = TheInstance(forward)->GCself;
      var aint backchain = p2;
      for (;;) {
        var gcv_object_t backpointer = ((Varobject)backchain)->GCself;
        ((Varobject)backchain)->GCself = target;
        record_flags_set((Record)backchain,instflags_relocated_B);
        if (record_flags((Record)backchain) & instflags_backpointer_B)
          backchain = (aint)ThePointer(without_mark_bit(backpointer));
        else
          break;
      }
    } else {
      # Leave a backpointer for later fixup.
      # Each instance can have only one forward pointer directly pointing
      # to it. This ensures that the backchain is a singly linked list.
      if (record_flags(TheInstance(forward)) & instflags_backpointer_B)
        /*NOTREACHED*/ abort();
      #ifdef TYPECODES
      # The type is either instance_type or closure_type.
      var tint type = mtypecode(((Varobject)p2)->GCself) & ~bit(garcol_bit_t);
      #endif
      TheInstance(forward)->GCself = with_mark_bit(make_GCself(type,p2));
      record_flags_set(TheInstance(forward),instflags_backpointer_B);
    }
    # Don't reclaim the space at p2 during this GC, because
    # 1. we need the mark bit at p2 so that update() does the
    #    relocation, and the mark bit tells gc_sweep2_varobject_page
    #    that the object is not yet reclaimed.
    # 2. otherwise last_open_ptr may be set to &((Varobject)p2)->GCself
    #    later.
  }
  local void gc_sweep1_instance_target (aint p2, aint p1) {
    if (record_flags((Instance)p2) & instflags_relocated_B)
      /*NOTREACHED*/ abort();
    #ifdef TYPECODES
    # The type is either instance_type or closure_type.
    var tint type = mtypecode(((Varobject)p2)->GCself) & ~bit(garcol_bit_t);
    #endif
    var gcv_object_t target; target = with_mark_bit(make_GCself(type,p1));
    var aint backchain = p2;
    for (;;) {
      var gcv_object_t backpointer = ((Varobject)backchain)->GCself;
      ((Varobject)backchain)->GCself = target;
      record_flags_set((Record)backchain,instflags_relocated_B);
      if (record_flags((Record)backchain) & instflags_backpointer_B)
        backchain = (aint)ThePointer(without_mark_bit(backpointer));
      else
        break;
    }
  }

# Prepare objects of variable length between page->page_start and
# page->page_end for compactification below. Therefore, in each marked
# object the pointer in front is pointed to the location, where the
# object will be located later (including typeinfo). If the sequencing
# object is unmarked, then its first pointer is oriented to the address
# of the next marked object.
  #ifdef SPVW_PURE
  local aint gc_sweep1_varobject_page (uintL heapnr, aint start, aint end, gcv_object_t* firstmarked, aint dest)
  #elif defined(GENERATIONAL_GC)
  local aint gc_sweep1_varobject_page (aint start, aint end, gcv_object_t* firstmarked, aint dest)
  #else
  local void gc_sweep1_varobject_page (Page* page)
  #endif
  {
    #if defined(SPVW_PURE) || defined(GENERATIONAL_GC)
    var gcv_object_t* last_open_ptr = firstmarked;
    var aint p2 = start; # source-pointer
    var aint p2end = end; # upper bound of the source-region
    var aint p1 = dest; # destination-pointer
    #else
    var gcv_object_t* last_open_ptr = &page->page_gcpriv.firstmarked;
      # In *last_open_ptr, always store the address of the next marked
      # object (als oint) .
      # Via chained-list-mechanism: At the end, page->page_gcpriv.firstmarked
      # contains the address of the 1. marked object
    var aint p2 = page->page_start; # source-pointer
    var aint p2end = page->page_end; # upper bound of the source-region
    var aint p1 = p2; # destination-pointer
    #endif
    # start <= p1 <= p2 <= end, p1 and p2 grow, p2 faster than p1.
    var_prepare_objsize;
    sweeploop1:
      # search next marked object.
      # enter address of the next marked object in *last_open_ptr .
      if (p2==p2end) # upper bound reached -> finished
        goto sweepok1;
      {
        #ifdef TYPECODES
        var tint flags = mtypecode(((Varobject)p2)->GCself);
        # save typeinfo (and flags for symbols)
        #endif
        var uintM laenge = objsize((Varobject)p2); # determine byte-length
        if (!marked(p2)) { # object unmarked?
          p2 += laenge; goto sweeploop1; # yes -> goto next object
        }
        # object marked
        # Elimination of forward pointers:
       #ifdef HAVE_SMALL_SSTRING
        #ifdef SPVW_PURE
        if (heapnr == sstring_type)
        #else
         #ifdef TYPECODES
          if ((flags & ~bit(garcol_bit_t)) == sstring_type)
         #else
          # NB: No need to handle Rectype_[Imm_]S8string here.
          if ((uintB)(record_type((Record)p2) - Rectype_S16string)
              <= Rectype_reallocstring - Rectype_S16string)
         #endif
        #endif
          {
            if (sstring_reallocatedp((Sstring)p2)) {
              # A forward pointer.
              gc_sweep1_sstring_forward(p2);
            } else {
              # Possibly the target of a forward pointer.
              gc_sweep1_sstring_target(p2,p1);
            }
          }
        else
       #endif
        #ifdef SPVW_PURE
        if (heapnr == instance_type
            || (heapnr == closure_type
                && (closure_flags((Closure)p2) & closflags_instance_B)))
        #else
         #ifdef TYPECODES
          if ((flags & ~bit(garcol_bit_t)) == instance_type
              || ((flags & ~bit(garcol_bit_t)) == closure_type
                  && (closure_flags((Closure)p2) & closflags_instance_B)))
         #else
          if (record_type((Record)p2) == Rectype_Instance
              || (record_type((Record)p2) == Rectype_Closure
                  && (closure_flags((Closure)p2) & closflags_instance_B)))
         #endif
        #endif
          {
            if (record_flags((Instance)p2) & instflags_forwarded_B) {
              # A forward pointer.
              gc_sweep1_instance_forward(p2);
            } else {
              # Possibly the target of a forward pointer.
              gc_sweep1_instance_target(p2,p1);
            }
          }
        else {
          set_GCself(p2, flags,p1); # enter new address, with old
                         # typeinfo (the mark bit is contained within)
          #ifndef TYPECODES
          mark(p2);
          #endif
        }
        *last_open_ptr = pointer_as_object(p2); # store address
        p2 += laenge; # source address for next object
        p1 += laenge; # destination address for next object
      }
    sweeploop2:
      # search next unmarked object.
      if (p2==p2end) # upper bound reached -> finished
        goto sweepok2;
      {
        #ifdef TYPECODES
        var tint flags = mtypecode(((Varobject)p2)->GCself);
        # save typeinfo (and flags for symbols)
        #endif
        var uintM laenge = objsize((Varobject)p2); # determine byte-length
        if (!marked(p2)) { # object unmarked?
          last_open_ptr = (gcv_object_t*)p2; # yes -> store the next pointer here
          p2 += laenge; goto sweeploop1; # goto next object
        }
        # object marked
        # Elimination of forward pointers:
       #ifdef HAVE_SMALL_SSTRING
        #ifdef SPVW_PURE
        if (heapnr == sstring_type)
        #else
         #ifdef TYPECODES
          if ((flags & ~bit(garcol_bit_t)) == sstring_type)
         #else
          # NB: No need to handle Rectype_[Imm_]S8string here.
          if ((uintB)(record_type((Record)p2) - Rectype_S16string)
              <= Rectype_reallocstring - Rectype_S16string)
         #endif
        #endif
          {
            if (sstring_reallocatedp((Sstring)p2)) {
              # A forward pointer.
              gc_sweep1_sstring_forward(p2);
            } else {
              # Possibly the target of a forward pointer.
              gc_sweep1_sstring_target(p2,p1);
            }
          }
        else
       #endif
        #ifdef SPVW_PURE
        if (heapnr == instance_type
            || (heapnr == closure_type
                && (closure_flags((Closure)p2) & closflags_instance_B)))
        #else
         #ifdef TYPECODES
          if ((flags & ~bit(garcol_bit_t)) == instance_type
              || ((flags & ~bit(garcol_bit_t)) == closure_type
                  && (closure_flags((Closure)p2) & closflags_instance_B)))
         #else
          if (record_type((Record)p2) == Rectype_Instance
              || (record_type((Record)p2) == Rectype_Closure
                  && (closure_flags((Closure)p2) & closflags_instance_B)))
         #endif
        #endif
          {
            if (record_flags((Instance)p2) & instflags_forwarded_B) {
              # A forward pointer.
              gc_sweep1_instance_forward(p2);
            } else {
              # Possibly the target of a forward pointer.
              gc_sweep1_instance_target(p2,p1);
            }
          }
        else {
          set_GCself(p2, flags,p1); # enter new address, with old
                         # typeinfo (the mark bit is contained within)
          #ifndef TYPECODES
          mark(p2);
          #endif
        }
        p2 += laenge; # source address for next object
        p1 += laenge; # destination address for next object
        goto sweeploop2;
      }
    sweepok1: *last_open_ptr = pointer_as_object(p2);
    sweepok2: ;
    #if defined(SPVW_PURE) || defined(GENERATIONAL_GC)
    return p1;
    #endif
  }

# update phase:
  # The entire LISP-memory is perused and old addresses are replaced
  # with new ones.
  # update of an object *objptr :
    #if !defined(MORRIS_GC)
      #ifdef TYPECODES
        #define update(objptr)  \
          { var tint type = mtypecode(*(gcv_object_t*)objptr);             \
            if (!gcinvariant_type_p(type)) { # un-movable -> do nothing    \
              var object obj = *(gcv_object_t*)objptr; # object            \
              if (!in_old_generation(obj,type,mem.heapnr_from_type[type])) \
                # older generation -> do nothing (object stayed there)     \
                if (marked(ThePointer(obj))) { # marked?                   \
                  # no -> do nothing (object stayed there)                 \
                  # yes -> enter new address and typeinfobyte (incl.       \
                  #        poss. symbol-binding-flag)                      \
                  var object newptr =                                      \
                    type_untype_object(type,untype(*(gcv_object_t*)ThePointer(obj))); \
                  DEBUG_SPVW_ASSERT(is_valid_heap_object_address(as_oint(newptr)) \
                                    || is_valid_stack_address(as_oint(newptr))); \
                  *(gcv_object_t*)objptr = newptr;                         \
                }                                                          \
            }                                                              \
          }
      #else
        #ifdef GENERATIONAL_GC
          #define update(objptr)  \
            { var object obj = *(gcv_object_t*)objptr; # object               \
              if (!gcinvariant_object_p(obj)) # un-movable -> do nothing      \
                if (!(consp(obj) ? in_old_generation(obj,,1) : in_old_generation(obj,,0))) \
                  # older generation -> do nothing (object stayed there)      \
                  if (marked(ThePointer(obj))) { # marked?                    \
                    # no ->  do nothing (object stayed there)                 \
                    # yes -> enter new address                                \
                    var object newptr =                                       \
                      as_object((as_oint(obj) & nonimmediate_bias_mask) | (as_oint(*(gcv_object_t*)ThePointer(obj)) & ~wbit(garcol_bit_o))); \
                    DEBUG_SPVW_ASSERT((consp(obj) ? is_valid_cons_address(as_oint(newptr)) : is_valid_varobject_address(as_oint(newptr))) \
                                      || is_valid_stack_address(as_oint(newptr))); \
                    *(gcv_object_t*)objptr = newptr;                          \
                  }                                                           \
            }
        #else
          #define update(objptr)  \
            { var object obj = *(gcv_object_t*)objptr; # object               \
              if (!gcinvariant_object_p(obj)) # un-movable -> do nothing      \
                if (!in_old_generation(obj,,))                                \
                  # older generation -> do nothing (object stayed there)      \
                  if (marked(ThePointer(obj))) { # marked?                    \
                    # no ->  do nothing (object stayed there)                 \
                    # yes -> enter new address                                \
                    var object newptr =                                       \
                      as_object((as_oint(obj) & nonimmediate_bias_mask) | (as_oint(*(gcv_object_t*)ThePointer(obj)) & ~wbit(garcol_bit_o))); \
                    DEBUG_SPVW_ASSERT((consp(obj) ? is_valid_cons_address(as_oint(newptr)) : is_valid_varobject_address(as_oint(newptr))) \
                                      || is_valid_stack_address(as_oint(newptr))); \
                    *(gcv_object_t*)objptr = newptr;                          \
                  }                                                           \
            }
        #endif
      #endif
    #else # defined(MORRIS_GC)
      #if defined(SPVW_MIXED_BLOCKS)
        #ifdef TYPECODES
          #define update(objptr)  \
            { var tint type = mtypecode(*(gcv_object_t*)objptr);        \
              if (!gcinvariant_type_p(type)) # un-movable -> do nothing \
                switch (type) {                                         \
                  default: # object of variable length                  \
                    { var object obj = *(gcv_object_t*)objptr; # object \
                      if (!in_old_generation(obj,type,0))               \
                        if (marked(ThePointer(obj))) { # marked?        \
                          var object newptr =                           \
                            type_untype_object(type,untype(*(gcv_object_t*)ThePointer(obj))); \
                          DEBUG_SPVW_ASSERT(is_valid_varobject_address(as_oint(newptr)) \
                                            || is_valid_stack_address(as_oint(newptr))); \
                          *(gcv_object_t*)objptr = newptr;              \
                        }                                               \
                    }                                                   \
                    break;                                              \
                  case_pair: # Two-Pointer-Object                       \
                    { var object obj = *(gcv_object_t*)objptr; # object \
                      if (!in_old_generation(obj,type,1)) {             \
                        # for later update, insert into its list:       \
                        *(gcv_object_t*)objptr = *(gcv_object_t*)ThePointer(obj); \
                        *(gcv_object_t*)ThePointer(obj) = with_mark_bit(type_pointer_object(type,objptr)); \
                      }                                                 \
                    }                                                   \
                    break;                                              \
                }                                                       \
            }
        #else
          #define update(objptr)  \
            { var object obj = *(gcv_object_t*)objptr; # object               \
              if (!gcinvariant_object_p(obj)) {                               \
                if (consp(obj)) {                                             \
                  # Two-Pointer-Object                                        \
                  if (!in_old_generation(obj,,1)) {                           \
                    # for later update, insert into its list:                 \
                    *(gcv_object_t*)objptr = *(gcv_object_t*)ThePointer(obj); \
                    *(gcv_object_t*)ThePointer(obj) = with_mark_bit(as_object((as_oint(obj) & nonimmediate_bias_mask) | (oint)objptr)); \
                  }                                                           \
                } else {                                                      \
                  # object of variable length                                 \
                  if (!in_old_generation(obj,,0)) {                           \
                    if (marked(ThePointer(obj))) { # marked?                  \
                      var object newptr =                                     \
                        as_object((as_oint(obj) & nonimmediate_bias_mask) | (as_oint(*(gcv_object_t*)ThePointer(obj)) & ~wbit(garcol_bit_o) & ~(oint)nonimmediate_bias_mask)); \
                      DEBUG_SPVW_ASSERT(is_valid_varobject_address(as_oint(newptr)) \
                                        || is_valid_stack_address(as_oint(newptr))); \
                      *(gcv_object_t*)objptr = newptr;                        \
                    }                                                         \
                  }                                                           \
                }                                                             \
              }                                                               \
            }
        #endif
      #else # defined(SPVW_PURE_BLOCKS) # && defined(SINGLEMAP_MEMORY)
        #define update(objptr)  \
          { var tint type = mtypecode(*(gcv_object_t*)objptr);              \
            if (!is_unused_heap(type)) { # unmovable -> do nothing          \
              var object obj = *(gcv_object_t*)objptr; # object             \
              if (!in_old_generation(obj,type,?)) {                         \
                # older generation -> do nothing (object stayed there)      \
                if (is_varobject_heap(type)) {                              \
                  # object of variable length                               \
                  if (marked(ThePointer(obj))) { # marked?                  \
                    var object newptr =                                     \
                      type_untype_object(type,untype(*(gcv_object_t*)ThePointer(obj))); \
                    DEBUG_SPVW_ASSERT(is_valid_varobject_address(as_oint(newptr)) \
                                      || is_valid_stack_address(as_oint(newptr))); \
                    *(gcv_object_t*)objptr = newptr;                        \
                  }                                                         \
                } else {                                                    \
                  # Two-Pointer-Object                                      \
                  # for later update, insert into its list:                 \
                  *(gcv_object_t*)objptr = *(gcv_object_t*)ThePointer(obj); \
                  *(gcv_object_t*)ThePointer(obj) = with_mark_bit(pointer_as_object(objptr)); \
                }                                                           \
              }                                                             \
            }                                                               \
          }
      #endif
    #endif
    #ifndef NO_symbolflags
      #define update_stackobj(objptr)  \
        switch (mtypecode(*objptr)) {                           \
          case_symbolflagged: # symbol, poss. with flags        \
            { var object obj1 = *objptr;                        \
              var object obj2 = symbol_without_flags(obj1);     \
              var oint flags = as_oint(obj1) ^ as_oint(obj2);   \
              *objptr = obj2; # delete flags                    \
              update(objptr); # then update                     \
              *objptr = as_object(as_oint(*objptr) | flags); # then back again \
              break;                                            \
            }                                                   \
          default: update(objptr); break;                       \
        }
    #else
      #define update_stackobj(objptr)  \
        update(objptr);
    #endif
  # update of old generation:
    #include "spvw_genera3.c"

# second SWEEP-phase:
  # relocation of an object of variable length, advance p1 and p2:
  # move_aligned_p1_p2(count);
  #if (varobject_alignment==1)
    #define uintVLA  uintB
  #elif (varobject_alignment==2)
    #define uintVLA  uintW
  #elif (varobject_alignment==4)
    #define uintVLA  uintL
  #elif (varobject_alignment==8)
    #define uintVLA  uintL2
  #else
    #error "Unknown value for 'varobject_alignment'!"
  #endif
  #if defined(GNU) && (__GNUC__ < 3) && !defined(__cplusplus) # better for optimization
    #if defined(fast_dotimesL) && (intMsize==intLsize)
      #define move_aligned_p1_p2(count)  \
        dotimespL(count,count/varobject_alignment, *((uintVLA*)p2)++ = *((uintVLA*)p1)++; )
    #else
      #define move_aligned_p1_p2(count)  \
        do { *((uintVLA*)p2)++ = *((uintVLA*)p1)++; count -= varobject_alignment; } until (count==0)
    #endif
  #else # other compilers do not accept ((type*)p)++ .
    # how efficient is this here??
    #define move_aligned_p1_p2(count)  \
      do {                                                    \
        *(uintVLA*)p2 = *(uintVLA*)p1;                        \
        p1 += varobject_alignment; p2 += varobject_alignment; \
        count -= varobject_alignment;                         \
      } until (count==0)
  #endif
  # the objects of variable length are moved into the preordained
  # new places.
  #ifdef SPVW_PURE
  local void gc_sweep2_varobject_page (Page* page, uintL heapnr)
  #else
  local void gc_sweep2_varobject_page (Page* page)
  #endif
  {
    # peruse from below and shift down:
    var aint p1 = (aint)pointer_was_object(page->page_gcpriv.firstmarked); # source-pointer, first marked object
    var aint p1end = page->page_end;
    var aint p2 = page->page_start; # destination-pointer
    var_prepare_objsize;
    until (p1==p1end) { # upper bound reached -> finished
      # next object has address p1
      if (marked(p1)) { # marked?
        unmark(p1); # delete mark
        # keep object and relocate:
        var uintM count = objsize((Varobject)p1); # length (divisible by varobject_alignment , >0)
        if (!(p1==p2)) { # if relocation is necessary
          move_aligned_p1_p2(count); # relocate and advance
        } else { # else only advance:
          p1 += count; p2 += count;
        }
      } else {
        p1 = (aint)pointer_was_object(*(gcv_object_t*)p1); # with pointer (typeinfo=0) to the next marked object
      }
    }
    page->page_end = p2; # set upper bound of the objects of variable length
  }

#if defined(DEBUG_SPVW) && !defined(GENERATIONAL_GC)
  # check, if everything is really unmarked:
  #define CHECK_GC_UNMARKED()  gc_unmarkcheck()
local void gc_unmarkcheck (void) {
  for_each_varobject_page(page, { /* peruse from above: */
    var aint p1 = page->page_start;
    var aint p1end = page->page_end;
    var_prepare_objsize;
    while (p1!=p1end) { /* lower bound reached -> finished */
      /* next object has address p1 */
      if (marked(p1)) { /* marked? */
        fprintf(stderr,"\nObject 0x%lx marked!!\n",p1);
        abort();
      }
      p1 += objsize((Varobject)p1);
    }
  });
  for_each_cons_page(page, { /* peruse from below: */
    var aint p1 = page->page_start;
    var aint p1end = page->page_end;
    while (p1!=p1end) { /* upper bound reached -> finished */
      /* next object has address p1 */
      if (marked(p1)) { /* marked? */
        fprintf(stderr,"\nObject 0x%lx marked!!\n",p1);
        abort();
      }
      p1 += sizeof(cons_);
    }
  });
}
#else
  #define CHECK_GC_UNMARKED()
#endif

#ifdef DEBUG_SPVW
  /* check against nullpointer: */
  #define CHECK_NULLOBJ()  nullobjcheck(false)
  local void nullobjcheck (bool in_gc);
  local void nullobjcheck_range (aint p1, aint p1end, bool in_gc)
  {
    until (p1==p1end) { # upper bound reached -> finished
      # next object has address p1
      if (eq(((Cons)p1)->cdr,nullobj) || eq(((Cons)p1)->car,nullobj))
        if (!(in_gc && eq(((Cons)p1)->cdr,nullobj) && eq(((Cons)p1)->car,nullobj)))
          abort();
      p1 += sizeof(cons_);
    }
  }
  local void nullobjcheck (bool in_gc)
  {
    # peruse from below:
    #ifdef GENERATIONAL_GC
      #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
      for_each_cons_heap(heap, {
        nullobjcheck_range(heap->heap_start,heap->heap_gen1_end,in_gc);
        nullobjcheck_range(heap->heap_gen0_start,heap->heap_gen0_end,in_gc);
      });
      #else
      for_each_cons_heap(heap, {
        nullobjcheck_range(heap->heap_gen0_start,heap->heap_gen0_end,in_gc);
        nullobjcheck_range(heap->heap_gen1_start,heap->heap_end,in_gc);
      });
      #endif
    #else
      for_each_cons_page(page, {
        nullobjcheck_range(page->page_start,page->page_end,in_gc);
      });
    #endif
  }
#else
  #define CHECK_NULLOBJ()
#endif

#ifdef SPVW_PAGES
  # free superfluous pages:
  # if the space that is available in mem.free_pages after GC,
  # amounts to more than 25% of what we currently need, the rest
  # is returned back to the operating system.
  local void free_some_unused_pages (void)
  {
    var uintM needed_space = floor(mem.last_gcend_space,4); # 25%
    var uintM accu_space = 0;
    var Pages* pageptr = &mem.free_pages;
    var Pages page = *pageptr;
    until (page==NULL) {
      var Pages nextpage = (Pages) page->page_gcpriv.next;
      if (accu_space < needed_space) {
        # retain page
        accu_space += page->page_room;
        pageptr = (Pages*)&page->page_gcpriv.next; page = nextpage;
      } else {
        # free page
        free_page(page); page = *pageptr = nextpage;
      }
    }
  }
#endif

# perform normal Garbage Collection:
  local void gar_col_normal (void)
  {
    var uintM gcstart_space; # occupied memory at GC-start
    var uintM gcend_space; # occupied memory at GC-end
    var object all_weakpointers; # list of active Weak-pointers
    var object all_finalizers; # list of finalizers
    #ifdef GC_CLOSES_FILES
    var object files_to_close; # list of files to be closed
    #endif
    set_break_sem_1(); # disable BREAK during Garbage Collection
    gc_signalblock_on(); # disable Signals during Garbage Collection
    gc_timer_on();
    gcstart_space = used_space(); # detect occupied memory
    #ifdef HAVE_VADVISE
      begin_system_call();
      vadvise(VA_ANOM); # Paging-behaviour now becomes a little unusual
      end_system_call();
    #endif
    CHECK_GC_UNMARKED(); CHECK_NULLOBJ(); CHECK_GC_CACHE(); CHECK_GC_GENERATIONAL(); SAVE_GC_DATA();
    #ifdef SPVW_PAGES
      {
        var uintL heapnr;
        for (heapnr=0; heapnr<heapcount; heapnr++) {
          AVL_map(mem.heaps[heapnr].inuse,page,
                  page->page_room += page->page_end;
                 );
          # the end of usable space is stored in page_room.
        }
      }
    #endif
    #ifdef GENERATIONAL_GC
      if (generation == 0) {
        # update old generation with help of the cache:
        prepare_old_generation();
      } else {
        # only treat the new generation. Hide old generation:
        #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
        mem.varobjects.heap_start = mem.varobjects.heap_gen1_start;
        mem.conses.heap_end = mem.conses.heap_gen1_end;
        #else
        var uintL heapnr;
        for (heapnr=0; heapnr<heapcount; heapnr++)
          mem.heaps[heapnr].heap_start = mem.heaps[heapnr].heap_gen1_start;
        #endif
      }
    #endif
    CHECK_GC_GENERATIONAL();
    # mark phase:
    all_weakpointers = O(all_weakpointers); O(all_weakpointers) = Fixnum_0;
    all_finalizers = O(all_finalizers); O(all_finalizers) = Fixnum_0;
    #ifdef GC_CLOSES_FILES
    files_to_close = O(open_files); O(open_files) = NIL; # O(files_to_close) = NIL;
    #endif
    gc_markphase();
    gc_mark_weakpointers(all_weakpointers);
    # Now only, after gc_mark_weakpointers, can alive() be called.
    # FIXME: This use of alive() and gc_mark() doesn't integrate well with
    # the weak-pointer handling.
    # Split (still unmarked) list all_finalizers into two lists:
    {
      var object Lu = all_finalizers;
      var gcv_object_t* L1 = &O(all_finalizers);
      var gcv_object_t* L2 = &O(pending_finalizers);
      until (eq(*L2,Fixnum_0)) {
        L2 = &TheFinalizer(*L2)->fin_cdr;
      }
      until (eq(Lu,Fixnum_0)) {
        # if fin_alive is dead, the finalizer is thrown away,
        # without being executed:
        if (!alive(TheFinalizer(Lu)->fin_alive)) {
          Lu = TheFinalizer(Lu)->fin_cdr;
        } else {
          # if fin_trigger dies, the finalizer is executed:
          if (alive(TheFinalizer(Lu)->fin_trigger)) { # is fin_trigger still alive?
            # yes -> take over in O(all_finalizers) :
            *L1 = Lu; L1 = &TheFinalizer(Lu)->fin_cdr; Lu = *L1;
          } else {
            # no -> take over in O(pending_finalizers) :
            *L2 = Lu; L2 = &TheFinalizer(Lu)->fin_cdr; Lu = *L2;
          }
        }
      }
      *L1 = Fixnum_0; *L2 = Fixnum_0;
    }
    gc_mark(O(all_finalizers)); gc_mark(O(pending_finalizers)); # mark both lists now
    #ifdef GC_CLOSES_FILES
    # FIXME: This use of marked() and gc_mark() doesn't integrate well with
    # the weak-pointer handling.
    # Split (still unmarked) list files_to_close into two lists:
    {
      var object Lu = files_to_close;
      var gcv_object_t* L1 = &O(open_files);
      var gcv_object_t* L2 = &O(files_to_close);
      while (consp(Lu)) {
        if (in_old_generation(Car(Lu),stream_type,0)
            || marked(TheStream(Car(Lu))) # (car Lu) marked?
           ) {
          # yes -> take over in O(open_files) :
          *L1 = Lu; L1 = &Cdr(Lu); Lu = *L1;
        } else {
          # no -> take over in O(files_to_close) :
          *L2 = Lu; L2 = &Cdr(Lu); Lu = *L2;
        }
      }
      *L1 = NIL; *L2 = NIL;
    }
    gc_mark(O(open_files)); gc_mark(O(files_to_close)); # mark both lists now
    #endif
    # No more gc_mark operations from here on.
    clean_weakpointers(all_weakpointers);
    inside_gc = true;
    # All active objects are marked now:
    # active objects of variable length and active two-pointer-objects carry
    # in their first byte a set mark bit, active SUBRs carry
    # in their first constant pointer a set mark bit, all other
    # mark bits are deleted.
    # "Sweep"-Phase:
      # the CONSes and similar (objects with 2 pointers) are compacted.
      # the destinations of the objects of variable length for phase 4
      # are calculated and stored.
      # SUBRs and fixed symbols (they are all active) are unmarked first:
        unmark_fixed_varobjects();
      #ifndef MORRIS_GC
      # compact CONS-cells:
        for_each_cons_page(page, { gc_compact_cons_page(page); } );
      #endif
      # prepare objects of variable length for compactification below:
        #ifdef SPVW_PURE
          #ifdef GENERATIONAL_GC
          if (generation == 0) {
            for_each_varobject_heap(heap, {
              if (heap->heap_gen0_end < heap->heap_gen1_start) {
                # Bridge the gap by putting a pointer.
                var aint tmp =
                  gc_sweep1_varobject_page(heapnr,
                                           heap->heap_gen0_start,heap->heap_gen0_end,
                                           &heap->pages.page_gcpriv.firstmarked,
                                           heap->heap_gen0_start);
                gc_sweep1_varobject_page(heapnr,
                                         heap->heap_gen1_start,heap->heap_end,
                                         (gcv_object_t*)heap->heap_gen0_end,
                                         tmp);
              } else {
                # no gap
                gc_sweep1_varobject_page(heapnr,
                                         heap->heap_gen0_start,heap->heap_end,
                                         &heap->pages.page_gcpriv.firstmarked,
                                         heap->heap_gen0_start);
              }
            });
          } else
          #endif
          for_each_varobject_page(page, {
            gc_sweep1_varobject_page(heapnr,
                                     page->page_start,page->page_end,
                                     &page->page_gcpriv.firstmarked,
                                     page->page_start);
          });
        #else # SPVW_MIXED
          #ifdef GENERATIONAL_GC
          if (generation == 0) {
            for_each_varobject_heap(heap, {
              if (heap->heap_gen0_end < heap->heap_gen1_start) {
                # Bridge the gap by putting a pointer.
                var aint tmp =
                  gc_sweep1_varobject_page(heap->heap_gen0_start,heap->heap_gen0_end,
                                           &heap->pages.page_gcpriv.firstmarked,
                                           heap->heap_gen0_start);
                gc_sweep1_varobject_page(heap->heap_gen1_start,heap->heap_end,
                                         (gcv_object_t*)(heap->heap_gen0_end),
                                         tmp);
              } else {
                # no gap
                gc_sweep1_varobject_page(heap->heap_gen0_start,heap->heap_end,
                                         &heap->pages.page_gcpriv.firstmarked,
                                         heap->heap_gen0_start);
              }
            });
          } else
            for_each_varobject_page(page, {
              gc_sweep1_varobject_page(page->page_start,page->page_end,
                                       &page->page_gcpriv.firstmarked,
                                       page->page_start);
            });
          #else
          for_each_varobject_page(page, { gc_sweep1_varobject_page(page); } );
          #endif
        #endif
    # Now all active objects are prepared for update:
    # For active objects of variable length at objptr, *objptr is the address,
    # where the object will be situated after the GC (incl. Typeinfo and
    # mark bit and poss. symbol-flags).
    # For active two-pointer-objects at objptr, objptr either stays where it is
    # (then the mark bit in *objptr is cleared), or objptr is relocated
    # (then *objptr is the new address, without typeinfo, but including mark
    # bit).
    # update phase:
      # The entire LISP-memory is perused and old addresses
      # are replaced with new ones.
      #ifdef MORRIS_GC
      for_each_cons_page(page, { gc_morris1(page); } );
      #endif
      # peruse all LISP-objects and update:
        # Update pointers in all LISP-stacks:
          update_STACKs();
        # Update pointers in all C stacks:
          update_back_traces();
        # Update program constants:
          update_tables();
        #ifndef MORRIS_GC
        # update pointers in the Cons-cells:
          #define update_conspage  update_conspage_normal
          update_conses();
          #undef update_conspage
        #endif
        # update the pointers in the objects of variable length:
          #define update_page(page,updater)  \
            { var aint ptr = (aint)pointer_was_object(page->page_gcpriv.firstmarked); \
              var aint ptrend = page->page_end;                            \
              # peruse all objects with address >=ptr, <ptrend :           \
              until (ptr==ptrend) { # until ptr has reached the end        \
                # peruse next object with address ptr (< ptrend) :         \
                if (marked(ptr)) { # marked?                               \
                  # take typeinfo without mark bit!                        \
                  updater(typecode_at(ptr) & ~bit(garcol_bit_t));          \
                } else {                                                   \
                  # go with pointer (typeinfo=0) to the next marked object \
                  ptr = (aint)pointer_was_object(*(gcv_object_t*)ptr);     \
                }                                                          \
              }                                                            \
            }
          #ifdef GENERATIONAL_GC
            #define update_hashtable_invalid  false
          #else
            #define update_hashtable_invalid  true
          #endif
          #define update_unrealloc  true
          #define update_fpointer_invalid  false
          #define update_fsubr_function  false
          #define update_ht_invalid  set_ht_invalid_if_needed
          #define update_ss_unrealloc  mark_sstring_clean
          #define update_in_unrealloc  mark_inst_clean
          #define update_fp_invalid  mark_fp_invalid
          #define update_fs_function(ptr)
          update_varobjects();
          #undef update_fs_function
          #undef update_fp_invalid
          #undef update_in_unrealloc
          #undef update_ss_unrealloc
          #undef update_ht_invalid
          #undef update_fsubr_function
          #undef update_fpointer_invalid
          #undef update_unrealloc
          #undef update_hashtable_invalid
          #undef update_page
        #ifdef GENERATIONAL_GC
        # update pointers in the objects of the old generation:
          if (generation > 0)
            update_old_generation();
        #endif
      #ifdef MORRIS_GC
      # finally, the conses are relocated and simultaneously, all
      # pointers to them (at present, maintained in lists!) are updated.
      for_each_cons_page_reversed(page, { gc_morris2(page); } );
      #endif
      inside_gc = false;
      #ifdef MORRIS_GC
      for_each_cons_page(page, { gc_morris3(page); } );
      #endif
    # now, all active objects are provided with correct content (all
    # pointers within point to the correct addresses after the GC).
    # The active two-pointer-objects are already at the right location and
    # unmarked; the objects of variable length are still at the old
    # location and marked, if active.
    # Second SWEEP-phase:
      # The objects of variable length are moved to the previously
      # calculated new locations.
      #if !defined(GENERATIONAL_GC)
      #ifdef SPVW_MIXED
      for_each_varobject_page(page, { gc_sweep2_varobject_page(page); } );
      #else # SPVW_PURE
      for_each_varobject_page(page, { gc_sweep2_varobject_page(page,heapnr); } );
      #endif
      #else # defined(GENERATIONAL_GC)
      {
        var uintL heapnr;
        for (heapnr=0; heapnr<heapcount; heapnr++) {
          var Heap* heap = &mem.heaps[heapnr];
          if (!is_unused_heap(heapnr)) {
            if (is_varobject_heap(heapnr)) {
              #ifdef SPVW_MIXED
              gc_sweep2_varobject_page(&heap->pages);
              #else # SPVW_PURE
              gc_sweep2_varobject_page(&heap->pages,heapnr);
              #endif
            }
            if (generation == 0) {
              # The remainder forms the new generation 0.
              #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
              if (is_cons_heap(heapnr)) {
                var aint start = heap->heap_start;
                heap->heap_gen0_start = start;
                start = start & -physpagesize;
                heap->heap_start = heap->heap_gen1_end = start;
              } else
              #endif
              {
                var aint end = heap->heap_end;
                heap->heap_gen0_end = end;
                end = (end + (physpagesize-1)) & -physpagesize;
                #if varobjects_misaligned
                if (is_varobject_heap(heapnr)) {
                  end += varobjects_misaligned;
                  if (heap->heap_limit < end) {
                    if (end - heap->heap_limit > varobjects_misaligned)
                      abort();
                    heap->heap_limit = end;
                  }
                }
                #endif
                heap->heap_gen1_start = heap->heap_end = end;
              }
              build_old_generation_cache(heapnr);
            } else
              rebuild_old_generation_cache(heapnr);
          }
          #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
          if (is_cons_heap(heapnr))
            heap->heap_end = heap->heap_gen0_end;
          else
          #endif
            heap->heap_start = heap->heap_gen0_start;
        }
      }
      #endif
    # Now, all active objects are provided with correct content,
    # at the right location, and unmarked again.
    #ifdef SPVW_PAGES
      {
        var uintL heapnr;
        for (heapnr=0; heapnr<heapcount; heapnr++) {
          var Pages* heapptr = &mem.heaps[heapnr].inuse;
          AVL_map(*heapptr,page,
                  page->page_room -= page->page_end;
                 );
          # the available space is now stored in page_room again.
          # sort pages according to the available space:
          *heapptr = AVL(AVLID,sort)(*heapptr);
        }
      }
      for_each_cons_heap(heap, { heap->lastused = dummy_lastused; } );
      # treat .reserve??
    #endif
    CHECK_AVL_CONSISTENCY();
    CHECK_GC_CONSISTENCY();
    CHECK_GC_UNMARKED(); CHECK_NULLOBJ(); CHECK_GC_CACHE(); CHECK_GC_GENERATIONAL(); SAVE_GC_DATA();
    CHECK_PACK_CONSISTENCY();
    # end of Garbage Collection.
    #ifdef HAVE_VADVISE
      begin_system_call();
      vadvise(VA_NORM); # no Paging-behaviour becomes normal again
      end_system_call();
    #endif
    inc_gc_count(); # count GCs
    # detect occupied memory:
    #ifdef SPVW_PAGES
    recalc_space(false);
    #endif
    gcend_space = used_space();
    #ifdef SPVW_PAGES
    mem.last_gcend_space = gcend_space;
    # we let the used space grow up to 25%, only then
    # the next GC is triggered:
    {
      var uintM total_room = floor(mem.last_gcend_space,4);
      if (total_room < 512*1024) { total_room = 512*1024; } # at least 512 KB
      mem.gctrigger_space = mem.last_gcend_space + total_room;
    }
    #endif
    #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
    # make_space() expects, that mem.total_room <= length of the big gap.
    #define set_total_room(space_used_now)  \
      { set_total_room_(space_used_now);                                    \
        if (mem.total_room > mem.conses.heap_start-mem.varobjects.heap_end) \
          mem.total_room = mem.conses.heap_start-mem.varobjects.heap_end;   \
      }
    #else
    #define set_total_room  set_total_room_
    #endif
    #if (defined(SPVW_PURE_BLOCKS) || defined(TRIVIALMAP_MEMORY)) && !defined(GENERATIONAL_GC)
    # we let the used space grow by up to 50%, only then
    # the next GC is triggered:
    #define set_total_room_(space_used_now)  \
      { mem.total_room = floor(space_used_now,2); # 50% of the now used space \
        if (mem.total_room < 512*1024) { mem.total_room = 512*1024; } # at least 512 KB \
      }
    set_total_room(gcend_space);
    #endif
    #if defined(GENERATIONAL_GC)
    # we let the used space grow up to 25%, only then
    # the next GC is triggered:
    #define set_total_room_(space_used_now)  \
      { mem.total_room = floor(space_used_now,4); # 25% of the now used space \
        if (mem.total_room < 512*1024) { mem.total_room = 512*1024; } # at least 512 KB \
      }
    {
      var uintM gen0_sum = 0; # current size of the old generation
      var uintM gen1_sum = 0; # current size of the new generation
      for_each_heap(heap, {
        gen0_sum += heap->heap_gen0_end - heap->heap_gen0_start;
      });
      #ifdef SPVW_MIXED_BLOCKS_OPPOSITE
      gen1_sum += mem.varobjects.heap_end - mem.varobjects.heap_gen1_start;
      gen1_sum += mem.conses.heap_gen1_end - mem.conses.heap_start;
      #else
      for_each_heap(heap, {
        gen1_sum += heap->heap_end - heap->heap_gen1_start;
      });
      #endif
      # NB: gcend_space == gen0_sum + gen1_sum.
      set_total_room(gen0_sum);
      mem.last_gcend_space0 = gen0_sum;
      mem.last_gcend_space1 = gen1_sum;
    }
    #endif
    {
      var uintM freed = gcstart_space - gcend_space; # freed memory by this GC
      inc_gc_space(freed); # add this to the 64-Bit-Accu gc_space
    }
    #ifdef SPVW_PAGES
    free_some_unused_pages();
    #endif
    #if (defined(SINGLEMAP_MEMORY) || defined(TRIVIALMAP_MEMORY)) && defined(VIRTUAL_MEMORY) && defined(HAVE_MUNMAP)
    # free unused, empty pages, so that they do not have to moved
    # by the OS to the swap space:
      begin_system_call();
      #ifndef SPVW_MIXED_BLOCKS_OPPOSITE
      for_each_heap(heap, {
        var aint needed_limit = round_up(heap->heap_end,map_pagesize);
        if (needed_limit > round_up(heap->heap_limit,map_pagesize))
          abort();
        if (needed_limit < heap->heap_limit) {
          if (munmap((void*)needed_limit,heap->heap_limit-needed_limit) < 0)
            goto munmap_failure;
          heap->heap_limit = needed_limit;
        }
      });
      #else # SPVW_MIXED_BLOCKS_OPPOSITE
      for_each_heap(heap, {
        if (is_cons_heap(heapnr)) {
          var aint needed_limit = round_down(heap->heap_start,map_pagesize);
          if (needed_limit < heap->heap_limit)
            abort();
          if (needed_limit > heap->heap_limit) {
            if (munmap((void*)heap->heap_limit,needed_limit-heap->heap_limit) < 0)
              goto munmap_failure;
            heap->heap_limit = needed_limit;
          }
        } else {
          var aint needed_limit = round_up(heap->heap_end,map_pagesize);
          if (needed_limit > round_up(heap->heap_limit,map_pagesize))
            abort();
          if (needed_limit < heap->heap_limit) {
            if (munmap((void*)needed_limit,heap->heap_limit-needed_limit) < 0)
              goto munmap_failure;
            heap->heap_limit = needed_limit;
          }
        }
      });
      #endif
      if (false) {
       munmap_failure:
        end_system_call();
        fputs(GETTEXTL("munmap() failed."),stderr);
        errno_out(OS_errno);
        abort();
      }
      end_system_call();
    #endif
    # add time used by this GC to the GC-total-time:
    gc_timer_off();
    #ifdef GC_CLOSES_FILES
    close_some_files(O(files_to_close)); # close previously unmarked files
    O(files_to_close) = NIL;
    #endif
    #ifdef GENERATIONAL_GC
    O(gc_count) = fixnum_inc(O(gc_count),1); # count GCs
    #endif
    gc_signalblock_off(); # release signals again
    clr_break_sem_1(); # allow BREAK again
  }

# end of one Garbage Collection.
# can trigger GC!
  local maygc void gar_col_done (void)
  {
    # perform finalizer-functions:
    until (eq(O(pending_finalizers),Fixnum_0)) {
      var object obj = O(pending_finalizers);
      O(pending_finalizers) = TheFinalizer(obj)->fin_cdr;
      pushSTACK(TheFinalizer(obj)->fin_trigger);
      if (!boundp(TheFinalizer(obj)->fin_alive)) {
        funcall(TheFinalizer(obj)->fin_function,1); # (FUNCALL function trigger)
      } else {
        pushSTACK(TheFinalizer(obj)->fin_alive);
        funcall(TheFinalizer(obj)->fin_function,2); # (FUNCALL function trigger alive)
      }
    }
  }

#ifdef SPVW_PAGES

# a little sorting-routine:
#define SORTID  spvw
#define SORT_ELEMENT  Pages
#define SORT_KEY  uintM
#define SORT_KEYOF(page)  (page)->page_gcpriv.l
#define SORT_COMPARE(key1,key2)  (sintL)((key1)-(key2))
#define SORT_LESS(key1,key2)  ((key1) < (key2))
#include "sort.c"
#undef SORT_LESS
#undef SORT_COMPARE
#undef SORT_KEYOF
#undef SORT_KEY
#undef SORT_ELEMENT
#undef SORTID

# list of pages, that have to be freed, as soon as the update
# is completed:
  local var Page* delayed_pages = NULL;
# insertion of a page in this list:
  #define free_page_later(page)  \
    { (page)->page_gcpriv.next = delayed_pages; delayed_pages = page; }
# release of all pages in the list:
  #define free_delayed_pages()  \
    { var Page* page = delayed_pages;                   \
      until (page==NULL) {                              \
        var Page* next = (Page*)page->page_gcpriv.next; \
        free_page(page);                                \
        page = next;                                    \
      }                                                 \
      delayed_pages = NULL;                             \
    }

# compactification of a page by "decanting" into other pages of the same kind:
  #ifdef SPVW_PURE
  local void gc_compact_from_varobject_page (Heap* heapptr, Page* page, uintL heapnr)
  #else
  local void gc_compact_from_varobject_page (Heap* heapptr, Page* page)
  #endif
  {
    var aint p1 = page->page_start;
    var aint p1end = page->page_end;
    var_prepare_objsize;
    {
      var Pages new_page = EMPTY; # Page, which is being filled
      var AVL(AVLID,stack) stack; # path from the root to the page
      var aint p2; # cache of new_page->page_end
      var uintM l2; # cache of new_page->page_room
      # try to copy all objects between p1 and p1end :
      loop {
        if (p1==p1end) # upper bound reached -> finished
          break;
        var uintM laenge = objsize((Varobject)p1); # determine byte-length
        # search a page, that has still 'laenge' free bytes:
        if ((new_page == EMPTY) || (l2 < laenge)) {
          if (!(new_page == EMPTY)) { # empty cache?
            new_page->page_end = p2;
            new_page->page_room = l2;
            AVL(AVLID,move)(&stack);
          }
          new_page = AVL(AVLID,least)(laenge,&heapptr->inuse,&stack);
          if (new_page==EMPTY)
            break;
          new_page->page_gcpriv.d = -1L; # mark new_page as "to be filled"
          p2 = new_page->page_end;
          l2 = new_page->page_room;
        }
        var aint old_p1 = p1;
        var aint old_p2 = p2;
        # copy the object:
        l2 -= laenge; move_aligned_p1_p2(laenge);
        # leave a pointer to the new position:
        *(gcv_object_t*)old_p1 = with_mark_bit(pointer_as_object(old_p2));
        # p1 = source address for the next object
      }
      if (!(new_page == EMPTY)) { # empty cache?
        new_page->page_end = p2;
        new_page->page_room = l2;
        AVL(AVLID,move)(&stack);
      }
    }
    # the not copied objects experience a constant shift downward:
    {
      var aint p2 = page->page_start;
      page->page_gcpriv.d = p1 - p2; # shift
      page->page_start = p1; # current start of the page
      if (!(p1==p2)) # if shift is necessary
        until (p1==p1end) { # upper bound reached -> finished
          var uintM laenge = objsize((Varobject)p1); # calculate byte-length
          #ifdef TYPECODES
          var tint flags = mtypecode(((Varobject)p1)->GCself); # save typeinfo (and flags for symbols) retten
          #endif
          set_GCself(p1, flags,p2); # store new address, with old typeinfo
          mark(p1); # with mark bit
          p1 += laenge; p2 += laenge;
        }
    }
  }
  local void gc_compact_from_cons_page (Heap* heapptr, Page* page)
  {
    var aint p1 = page->page_end;
    var aint p1start = page->page_start;
    {
      var Pages new_page = EMPTY; # page, which is filled
      var AVL(AVLID,stack) stack; # path from the root to the page
      var aint p2; # cache of new_page->page_end
      var uintM l2; # cache of new_page->page_room
      # try to copy all objects between p1start and p1:
      loop {
        if (p1==p1start) # lower bound reached -> finished
          break;
        # search a page, that has at least sizeof(cons_) bytes free:
        if ((new_page == EMPTY) || (l2 == 0)) { # l2 < sizeof(cons_) means l2 = 0
          if (!(new_page == EMPTY)) { # empty cache?
            new_page->page_end = p2;
            new_page->page_room = l2;
            AVL(AVLID,move)(&stack);
          }
          new_page = AVL(AVLID,least)(sizeof(cons_),&heapptr->inuse,&stack);
          if (new_page==EMPTY)
            break;
          new_page->page_gcpriv.d = -1L; # mark new_page as "to be filled"
          p2 = new_page->page_end;
          l2 = new_page->page_room;
        }
        p1 -= sizeof(cons_); # p1 = source address for next object
        # copy the object:
        ((gcv_object_t*)p2)[0] = ((gcv_object_t*)p1)[0];
        ((gcv_object_t*)p2)[1] = ((gcv_object_t*)p1)[1];
        # leave a pointer to the new position:
        *(gcv_object_t*)p1 = with_mark_bit(pointer_as_object(p2));
        p2 += sizeof(cons_); l2 -= sizeof(cons_);
      }
      if (!(new_page == EMPTY)) { # empty cache?
        new_page->page_end = p2;
        new_page->page_room = l2;
        AVL(AVLID,move)(&stack);
      }
    }
    # the not copied objects remain on the spot.
    page->page_gcpriv.d = page->page_end - p1; # gain
    page->page_end = p1; # current end of the page
  }

# compactification of all pages of a certain kind:
  #ifdef SPVW_PURE
  local void gc_compact_heap (Heap* heapptr, sintB heaptype, uintL heapnr)
  #else
  local void gc_compact_heap (Heap* heapptr, sintB heaptype)
  #endif
  {
    # first, create a list of all pages, sorted ascending
    # according to the number of occupied bytes:
    var uintL pagecount = 0;
    map_heap(*heapptr,page,
             { page->page_gcpriv.l = page->page_end - page->page_start; # number of occupied bytes
               pagecount++;
             });
    # pagecount = number of pages.
    var DYNAMIC_ARRAY(pages_sorted,Pages,pagecount);
    {
      var uintL index = 0;
      map_heap(*heapptr,page, { pages_sorted[index++] = page; } );
    }
    # pages_sorted = Array of pages.
    SORT(spvw,sort)(pages_sorted,pagecount);
    # pages_sorted = Array of pages, sorted according to number
    # of occupied bytes.
    # In each page, page_gcpriv.d means the shift downwards,
    # that must occur to the page in Phase 3 (>=0).
    # page_gcpriv.d = -1L for the pages to be filled.
    # page_gcpriv.d = -2L for the yet untreated pages.
    map_heap(*heapptr,page, { page->page_gcpriv.d = -2L; } ); # all pages still untreated
    {
      var uintL index;
      for (index=0; index<pagecount; index++) { # peruse all pages
        var Pages page = pages_sorted[index]; # next page
        if (page->page_gcpriv.d == -2L) { # still untreated and
                                          # not yet marked as "to be filled"?
          # page is being emptied.
          heapptr->inuse = AVL(AVLID,delete1)(page,heapptr->inuse); # take out page
          # empty page:
          if (heaptype==0)
            gc_compact_from_cons_page(heapptr,page);
          else
            #ifdef SPVW_PURE
            gc_compact_from_varobject_page(heapptr,page,heapnr);
            #else
            gc_compact_from_varobject_page(heapptr,page);
            #endif
        }
      }
    }
    CHECK_AVL_CONSISTENCY();
    CHECK_GC_CONSISTENCY_2();
    {
      var uintL index;
      for (index=0; index<pagecount; index++) { # peruse all pages
        var Pages page = pages_sorted[index]; # next page
        if (!(page->page_gcpriv.d == -1L)) { # a page to be emptied
          page->page_room += page->page_gcpriv.d; # room, we have created now
          if (page->page_start == page->page_end) {
            # page completely emptied
            # free page:
            if (page->m_length > min_page_size_brutto) {
              # ultralarge page
              free_page_later(page); # return to OS later
            } else {
              # normal large page
              # keep; page->page_room remains the same!
              # insert into the pool mem.free_pages:
              page->page_gcpriv.next = mem.free_pages;
              mem.free_pages = page;
            }
          } else {
            # Page could not be emptied entirely
            heapptr->inuse = AVL(AVLID,insert1)(page,heapptr->inuse); # insert page again
          }
        }
      }
    }
    FREE_DYNAMIC_ARRAY(pages_sorted);
    CHECK_AVL_CONSISTENCY();
    CHECK_GC_CONSISTENCY_2();
  }

# perform compacting Garbage Collection.
# Is called, after gar_col_simple() could not get sufficient room
# in one piece.
# Note: This function does not garbage collect anything; it only reorganizes
# the existing objects in fewer pages. Therefore it does not need to be
# wrapped in with_gc_statistics() calls like do_gar_col_simple and do_gar_col.
  local maygc void gar_col_compact (void)
  {
    # Lisp-objects from almost empty pages are filled into other pages,
    # in order to return those now empty pages.
    # 1. For each kind of page:
    #    divide pages in pages to be emptied and pages to be filled and
    #    copy as many data as possible from the to be emptied pages into
    #    the pages to be filled. If a page cannot be emptied entirely,
    #    leave it as it is, and within it move the remaining data
    #    just downwards.
    #    return of the completely empty pages.
    # 2. update of pointers.
    # 3. execution of the relocations into the not entirely emptied pages.
    set_break_sem_1(); # disable BREAK during Garbage Collection
    gc_signalblock_on(); # disable signals during Garbage Collection
    gc_timer_on();
    CHECK_GC_UNMARKED(); CHECK_NULLOBJ();
    inside_gc = true;
    {
      var uintL heapnr;
      for (heapnr=0; heapnr<heapcount; heapnr++)
        if (!is_unused_heap(heapnr))
          #ifdef SPVW_PURE
          gc_compact_heap(&mem.heaps[heapnr],mem.heaptype[heapnr],heapnr);
          #endif
          #ifdef SPVW_MIXED
          gc_compact_heap(&mem.heaps[heapnr],1-heapnr);
          #endif
    }
    # update phase:
      # The entire LISP-memory is perused and old addresses
      # are replaced with new ones.
      # peruse all LISP-objects and update:
        # Update pointers in the LISP-stacks:
          update_STACKs();
        # Update pointers in the C stacks:
          update_back_traces();
        # Update program constants:
          update_tables();
        # Update pointers in the cons-cells:
          #define update_conspage  update_conspage_normal
          update_conses();
          #undef update_conspage
        # update pointers in the objects of variable length:
          #define update_page(page,updater)  \
            { var aint ptr = page->page_start;                                \
              var aint ptrend = page->page_end;                               \
              # peruse all objects with address >=ptr, <ptrend :              \
              until (ptr==ptrend) { # until ptr has reached the end           \
                # peruse next object with address ptr (< ptrend) :            \
                updater(typecode_at(ptr) & ~bit(garcol_bit_t)); # and advance \
              }                                                               \
            }
          #ifdef GENERATIONAL_GC
            #define update_hashtable_invalid  false
          #else
            #define update_hashtable_invalid  true
          #endif
          #define update_unrealloc  false
          #define update_fpointer_invalid  false
          #define update_fsubr_function  false
          #define update_ht_invalid  set_ht_invalid_if_needed
          #define update_ss_unrealloc(ptr)
          #define update_in_unrealloc(ptr)
          #define update_fp_invalid  mark_fp_invalid
          #define update_fs_function(ptr)
          update_varobjects();
          #undef update_fs_function
          #undef update_fp_invalid
          #undef update_in_unrealloc
          #undef update_ss_unrealloc
          #undef update_ht_invalid
          #undef update_fsubr_function
          #undef update_fpointer_invalid
          #undef update_unrealloc
          #undef update_hashtable_invalid
          #undef update_page
    # execution of the relocations in the not entirely emptied pages:
      for_each_varobject_page(page, {
        if (!(page->page_gcpriv.d == -1L)) {
          var aint p1 = page->page_start;
          var aint p1end = page->page_end;
          var aint p2 = p1 - page->page_gcpriv.d;
          if (!(p1==p2)) { # if relocation is necessary
            var_prepare_objsize;
            page->page_start = p2;
            until (p1==p1end) { # upper bound reached -> finished
              # next object has address p1, is marked
              unmark(p1); # delete mark
              # retain object and relocate:
              var uintM count = objsize((Varobject)p1); # length (divisible by varobject_alignment, >0)
              move_aligned_p1_p2(count); # relocate and advance
            }
            page->page_end = p2;
          }
        }
      });
    for_each_cons_heap(heap, { heap->lastused = dummy_lastused; } );
    recalc_space(true);
    free_delayed_pages();
    free_some_unused_pages();
    inside_gc = false;
    CHECK_AVL_CONSISTENCY();
    CHECK_GC_CONSISTENCY();
    CHECK_GC_UNMARKED(); CHECK_NULLOBJ();
    CHECK_PACK_CONSISTENCY();
    gc_timer_off();
    gc_signalblock_off(); # release signals again
    clr_break_sem_1(); # allow BREAK again
  }

#endif

# perform Garbage Collection:
  local maygc void gar_col_simple (void);
  local void do_gar_col_simple (void)
  {
    #ifdef NOCOST_SP_CHECK
    # Better flag a stack overflow before GC than during GC. (If the
    # stack overflow handler is called during GC, a crash is unavoidable.)
    if (near_SP_overflow()) SP_ueber();
    #endif
    #if !defined(GENERATIONAL_GC)
    gar_col_normal();
    #ifdef SPVW_PAGES
    #if defined(UNIX) || defined(WIN32)
    # if the allocated, but unoccupied memory in pages
    # comprises more than 25% of what is occupied, compactification
    # is worthwhile, because a half-empty page costs the
    # operating system just as much as a full page:
    if (free_space() > floor(mem.last_gcend_space,4)) {
      gar_col_compact(); mem.last_gc_compacted = true;
    } else
    #endif
      mem.last_gc_compacted = false;
    #endif
    #else # defined(GENERATIONAL_GC)
    # If after the last GC the objects in the new generation
    # amount to more than 25% of the objects in the old generation,
    # then we will perform a full Garbage-Collection this time (both
    # generations at once.)
    if (mem.last_gcend_space1 > floor(mem.last_gcend_space0,4)) {
      generation = 0; gar_col_normal(); mem.last_gc_full = true;
    } else {
      generation = 1; gar_col_normal(); mem.last_gc_full = false;
    }
    #endif
    gar_col_done();
  }
  local maygc void gar_col_simple()
  {
    var uintC saved_mv_count = mv_count; # save mv_count
    with_gc_statistics(&do_gar_col_simple); # GC and statistics
    mv_count = saved_mv_count; # restore mv_count
  }

# perform full Garbage Collection:
  global maygc void gar_col (void);
  local void do_gar_col (void)
  {
    #ifdef NOCOST_SP_CHECK
    # Better flag a stack overflow before GC than during GC. (If the
    # stack overflow handler is called during GC, a crash is unavoidable.)
    if (near_SP_overflow()) SP_ueber();
    #endif
    #if !defined(GENERATIONAL_GC)
    gar_col_normal();
    #ifdef SPVW_PAGES
    gar_col_compact(); mem.last_gc_compacted = true;
    #endif
    #else # defined(GENERATIONAL_GC)
    generation = 0; gar_col_normal(); mem.last_gc_full = true;
    #endif
    gar_col_done();
  }
  global maygc void gar_col()
  {
    var uintC saved_mv_count = mv_count; # save mv_count
    with_gc_statistics(&do_gar_col); # GC and statistics
    mv_count = saved_mv_count; # restore mv_count
  }

# Macro update is now unnecessary:
  #undef update_stackobj
  #undef update

#if defined(SPVW_MIXED_BLOCKS_OPPOSITE) && RESERVE

# For Reorganization of the object memory after GC or before and after EXECUTE:
  # sub-program for relocation of the conses.
  # move_conses(delta);
  # the reserve memory is shrinked by delta bytes (divisible by
  # varobject_alignment), the conses are shifted upwards by delta bytes.
  local void move_conses (sintM delta)
  {
    if (delta==0) # no relocation necessary?
      return;
    set_break_sem_1(); # disable BREAK
    gc_signalblock_on(); # disable signals
    gc_timer_on();
    if (delta>0) {
      # shift upwards, from above
      var gcv_object_t* source = (gcv_object_t*) mem.conses.heap_end;
      var gcv_object_t* source_end = (gcv_object_t*) mem.conses.heap_start;
      #if !(defined(MIPS) && !defined(GNU))
      var gcv_object_t* dest = (gcv_object_t*) (mem.conses.heap_end += delta);
      #else # circumvent IRIX 4 "cc -ansi" compiler-bug??
      var gcv_object_t* dest = (mem.conses.heap_end += delta, (gcv_object_t*)mem.conses.heap_end);
      #endif
      mem.conses.heap_start += delta;
      until (source==source_end) {
        *--dest = *--source; # copy an entire cons upwards
        *--dest = *--source;
      }
    } else { # delta<0
      # shift downwards, from below
      var gcv_object_t* source = (gcv_object_t*) mem.conses.heap_start;
      var gcv_object_t* source_end = (gcv_object_t*) mem.conses.heap_end;
      #if !(defined(MIPS) && !defined(GNU))
      var gcv_object_t* dest = (gcv_object_t*) (mem.conses.heap_start += delta);
      #else # circumvent IRIX 4 "cc -ansi" compiler-bug??
      var gcv_object_t* dest = (mem.conses.heap_start += delta, (gcv_object_t*)mem.conses.heap_start);
      #endif
      mem.conses.heap_end += delta;
      until (source==source_end) {
        *dest++ = *source++; # copy an entire cons downwards
        *dest++ = *source++;
      }
    }
    # update pointers to conses and similar:
    {
      var soint odelta = (soint)delta<<(oint_addr_shift-addr_shift); # Offset in the oint
      # The entire LISP-memory is perused and old addresses
      # are replaced with new ones.
      # update of an object *objptr :
        #ifdef TYPECODES
          #define update(objptr)  \
            { switch (mtypecode(*(gcv_object_t*)(objptr))) {              \
                case_pair: # Two-Pointer-Object?                          \
                  *(gcv_object_t*)(objptr) = as_object(as_oint(*(gcv_object_t*)(objptr)) + odelta); \
                  break;                                                  \
                default: break;                                           \
              }                                                           \
            }
        #else
          #define update(objptr)  \
            { if (consp(*(gcv_object_t*)(objptr)))                          \
                *(gcv_object_t*)(objptr) = as_object(as_oint(*(gcv_object_t*)(objptr)) + odelta); \
            }
        #endif
      # peruse all LISP-objects and update:
        # Update pointers in all LISP-stacks:
          #define update_stackobj  update_stackobj_normal
          update_STACKs();
          #undef update_stackobj
        # Update pointers in all C stacks:
          update_back_traces();
        # Update program constants:
          update_tables();
        # update pointers in the Cons-cells:
          #define update_conspage  update_conspage_normal
          update_conses();
          #undef update_conspage
        # update pointers in the objects of variable length:
          #define update_page  update_page_normal
          #ifdef GENERATIONAL_GC
            #define update_hashtable_invalid  false
          #else
            #define update_hashtable_invalid  true
          #endif
          #define update_unrealloc  false
          #define update_fpointer_invalid  false
          #define update_fsubr_function  false
          #define update_ht_invalid  set_ht_invalid_if_needed
          #define update_ss_unrealloc(ptr)
          #define update_in_unrealloc(ptr)
          #define update_fp_invalid  mark_fp_invalid
          #define update_fs_function(ptr)
          update_varobjects();
          #undef update_fs_function
          #undef update_fp_invalid
          #undef update_in_unrealloc
          #undef update_ss_unrealloc
          #undef update_ht_invalid
          #undef update_fsubr_function
          #undef update_fpointer_invalid
          #undef update_unrealloc
          #undef update_hashtable_invalid
          #undef update_page
      # Macro update is now unnecessary:
        #undef update
    }
    # End of relocation and update.
    # add needed time to GC-total-time:
    gc_timer_off();
    gc_signalblock_off(); # release signals again
    clr_break_sem_1(); # allow BREAK again
  }

#endif
