/*
 * Garbage collection with weak references in CLISP
 * Bruno Haible 2004-2005
 */

/* An array that contains the addresses of those objects whose mark bit is
   being watched, plus room for the gc_mark queue. */
typedef struct markwatch_t {
  uintP address;
  gcv_object_t weakobj;
  uintL weakindex;
  struct markwatch_t * q_next;
} markwatch_t;
local markwatch_t* markwatchset;
/* Its allocated size. */
local uintM markwatchset_allocated;
/* Its public size. */
local uintM markwatchset_size;
/* Number of entries currently used. */
local uintL markwatchset_count;
/* Tail of the queue. */
local markwatch_t** markwatch_queue_tail;

/* The markwatchset is sorted according to the address. */
#define SORTID  markwatch
#define SORT_ELEMENT  markwatch_t
#define SORT_KEY  uintP
#define SORT_KEYOF(element)  (element).address
#define SORT_COMPARE(key1,key2)  \
  ((key1) > (key2) ? 1 : (key1) < (key2) ? -1 : 0)
#define SORT_LESS(key1,key2)  ((key1) < (key2))
#include "sort.c"
#undef SORT_LESS
#undef SORT_COMPARE
#undef SORT_KEYOF
#undef SORT_KEY
#undef SORT_ELEMENT
#undef SORTID

/* Lookup the markwatchset entry of an address. Returns NULL if there is
   none. */
local markwatch_t* markwatchset_lookup (uintP addr) {
  /* Binary search of addr. */
  if (markwatchset_count == 0)
    return NULL;
  var uintL i1 = 0;
  var uintL i2 = markwatchset_count;
  for (;;) {
    var uintL i = (i1+i2)/2;
    if (markwatchset[i].address == addr)
      return &markwatchset[i];
    else if (markwatchset[i].address < addr) {
      if (i1 == i) return NULL;
      /* Note here: i1 < i < i2. */
      i1 = i;
    } else if (markwatchset[i].address > addr) {
      if (i2 == i) return NULL;
      /* Note here: i1 <= i < i2. */
      i2 = i;
    } else
      abort();
  }
}

/* Add the markwatchset entries with the given address to the queue, unless
   they are already enqueued. */
local void markwatch_enqueue (markwatch_t* entry) {
  /* If there is no entry, i.e. the object being marked is not being watched,
     there is nothing to do. */
  if (entry != NULL)
    /* If the object is already enqueued, nothing to do. */
    if (entry->q_next == NULL) {
      /* The same address can be mentioned by multiple adjacent entries;
         enqueue them all. */
      while (entry > &markwatchset[0] && entry[-1].address == entry[0].address)
        entry--;
      for (;; entry++) {
        entry->q_next = *markwatch_queue_tail;
        *markwatch_queue_tail = entry;
        markwatch_queue_tail = &entry->q_next;
        if (!(entry < &markwatchset[markwatchset_count-1]
              && entry[1].address == entry[0].address))
          break;
      }
    }
}

/* gc_mark_with_watchset(obj) marks the object recursively. When it marks
   an object, it also removes it from markwatchset.
   We use this separate marking routine, thus duplicating the gc_mark code,
   because updating the markwatchset is quite expensive, compared to just
   setting a bit at given address, and it is only needed for those few objects
   that are held in memory only through weak mappings. */
#define gc_mark gc_mark_with_watchset
#define MARK(obj)  \
  { mark(obj);                                                  \
    markwatch_enqueue(markwatchset_lookup(canon((aint)(obj)))); \
  }
#include "spvw_gcmark.c"
#undef MARK
#undef gc_mark

/* Given an object of Rectype_Weak* type, returns the maximum number of
   pointers contained in it, whose liveness needs to be watched, because some
   action needs to be done depending on it. Replacing a pointer with unbound
   doesn't count here, as it's done afterwards and has no consequences. */
local uintL max_watchset_count (object obj) {
  if_recordp(obj, ; , abort(); );
  switch (Record_type(obj)) {
    case Rectype_Weakpointer:
      return 0;
    case Rectype_Weakmapping:
      return 1;
    case Rectype_WeakAnd:
    case Rectype_WeakOr: {
      var uintL n = Lrecord_length(obj)-2;
      return n;
    }
    case Rectype_WeakList:
      return 0;
    case Rectype_WeakAndMapping:
    case Rectype_WeakOrMapping: {
      var uintL n = Lrecord_length(obj)-3;
      return n;
    }
    case Rectype_WeakAlist_Key:
    case Rectype_WeakAlist_Value: {
      var uintL n = (Lrecord_length(obj)-2) / 2;
      return n;
    }
    case Rectype_WeakAlist_Either:
      return 0;
    case Rectype_WeakAlist_Both: {
      var uintL n = (Lrecord_length(obj)-2) / 2;
      return 2*n;
    }
    case Rectype_WeakHashedAlist_Key:
    case Rectype_WeakHashedAlist_Value: {
      var uintL n = (Lrecord_length(obj)-4) / 3;
      return n;
    }
    case Rectype_WeakHashedAlist_Either:
      return 0;
    case Rectype_WeakHashedAlist_Both: {
      var uintL n = (Lrecord_length(obj)-4) / 3;
      return 2*n;
    }
    default: abort();
  }
}

/* Adds a pointer to a markwatch_t array, unless it is GC-invariant. */
local inline void add_watchable (markwatch_t** accumulatorp, object obj, uintL index,
                                 object ptr) {
  /* NB: For ptr = unbound, nothing is done, because unbound is gcinvariant. */
  if (!gcinvariant_object_p(ptr))
    if (!marked(ThePointer(ptr))) {
      (*accumulatorp)->address = canonaddr(ptr);
      (*accumulatorp)->weakobj = obj;
      (*accumulatorp)->weakindex = index;
      (*accumulatorp)->q_next = NULL;
      (*accumulatorp)++;
    }
}

/* Given an object of Rectype_Weak* type, pushes into a markwatch_t array
   all the pointers in it, whose liveness needs to be watched, because some
   action needs to be done depending on it. Replacing a pointer with unbound
   doesn't count here, as it's done afterwards and has no consequences. */
local markwatch_t* get_watchset (object obj, markwatch_t* accumulator) {
  if_recordp(obj, ; , abort(); );
  switch (Record_type(obj)) {
    case Rectype_Weakpointer:
      break;
    case Rectype_Weakmapping:
      add_watchable(&accumulator,obj,0,TheWeakmapping(obj)->wm_key);
      break;
    case Rectype_WeakAnd: {
      var uintL n = Lrecord_length(obj)-2;
      var uintL i;
      for (i = 0; i < n; i++)
        add_watchable(&accumulator,obj,0,TheWeakAnd(obj)->war_keys[i]);
      break;
    }
    case Rectype_WeakOr: {
      var uintL n = Lrecord_length(obj)-2;
      var uintL i;
      for (i = 0; i < n; i++)
        add_watchable(&accumulator,obj,0,TheWeakOr(obj)->wor_keys[i]);
      break;
    }
    case Rectype_WeakList:
      break;
    case Rectype_WeakAndMapping: {
      var uintL n = Lrecord_length(obj)-3;
      var uintL i;
      for (i = 0; i < n; i++)
        add_watchable(&accumulator,obj,0,TheWeakAndMapping(obj)->wam_keys[i]);
      break;
    }
    case Rectype_WeakOrMapping: {
      var uintL n = Lrecord_length(obj)-3;
      var uintL i;
      for (i = 0; i < n; i++)
        add_watchable(&accumulator,obj,0,TheWeakOrMapping(obj)->wom_keys[i]);
      break;
    }
    case Rectype_WeakAlist_Key: {
      var uintL n = (Lrecord_length(obj)-2) / 2;
      var uintL i;
      for (i = 0; i < n; i++)
        add_watchable(&accumulator,obj,i,TheWeakAlist(obj)->wal_data[2*i+0]);
      break;
    }
    case Rectype_WeakAlist_Value: {
      var uintL n = (Lrecord_length(obj)-2) / 2;
      var uintL i;
      for (i = 0; i < n; i++)
        add_watchable(&accumulator,obj,i,TheWeakAlist(obj)->wal_data[2*i+1]);
      break;
    }
    case Rectype_WeakAlist_Either:
      break;
    case Rectype_WeakAlist_Both: {
      var uintL n = (Lrecord_length(obj)-2) / 2;
      var uintL i;
      for (i = 0; i < n; i++) {
        add_watchable(&accumulator,obj,i,TheWeakAlist(obj)->wal_data[2*i+0]);
        add_watchable(&accumulator,obj,i,TheWeakAlist(obj)->wal_data[2*i+1]);
      }
      break;
    }
    case Rectype_WeakHashedAlist_Key: {
      var uintL n = (Lrecord_length(obj)-4) / 3;
      var uintL i;
      for (i = 0; i < n; i++)
        add_watchable(&accumulator,obj,i,TheWeakHashedAlist(obj)->whal_data[3*i+0]);
      break;
    }
    case Rectype_WeakHashedAlist_Value: {
      var uintL n = (Lrecord_length(obj)-4) / 3;
      var uintL i;
      for (i = 0; i < n; i++)
        add_watchable(&accumulator,obj,i,TheWeakHashedAlist(obj)->whal_data[3*i+1]);
      break;
    }
    case Rectype_WeakHashedAlist_Either:
      break;
    case Rectype_WeakHashedAlist_Both: {
      var uintL n = (Lrecord_length(obj)-4) / 3;
      var uintL i;
      for (i = 0; i < n; i++) {
        add_watchable(&accumulator,obj,i,TheWeakHashedAlist(obj)->whal_data[3*i+0]);
        add_watchable(&accumulator,obj,i,TheWeakHashedAlist(obj)->whal_data[3*i+1]);
      }
      break;
    }
    default: abort();
  }
  return accumulator;
}

/* Given an object of Rectype_Weak* type, propagates the liveness from
   the keys to the values and similar.
   If the index is (uintL)~(uintL)0, operate on the entire object,
   otherwise only on the part with the given index. */
local void propagate_through_weak (object obj, uintL index) {
  if_recordp(obj, ; , abort(); );
  switch (Record_type(obj)) {
    case Rectype_Weakpointer:
      break;
    case Rectype_Weakmapping: {
      var object key = TheWeakmapping(obj)->wm_key;
      if (!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)))
        gc_mark_with_watchset(TheWeakmapping(obj)->wm_value);
      break;
    }
    case Rectype_WeakAnd: {
      var uintL n = Lrecord_length(obj)-2;
      var bool all_alive = true;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakAnd(obj)->war_keys[i];
        if (!(!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)))) {
          all_alive = false;
          break;
        }
      }
      if (all_alive)
        gc_mark_with_watchset(TheWeakAnd(obj)->war_keys_list);
      break;
    }
    case Rectype_WeakOr: {
      var uintL n = Lrecord_length(obj)-2;
      var bool some_alive = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakOr(obj)->wor_keys[i];
        if (!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key))) {
          some_alive = true;
          break;
        }
      }
      if (some_alive)
        gc_mark_with_watchset(TheWeakOr(obj)->wor_keys_list);
      break;
    }
    case Rectype_WeakList:
      break;
    case Rectype_WeakAndMapping: {
      var uintL n = Lrecord_length(obj)-3;
      var bool all_alive = true;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakAndMapping(obj)->wam_keys[i];
        if (!(!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)))) {
          all_alive = false;
          break;
        }
      }
      if (all_alive) {
        gc_mark_with_watchset(TheWeakAndMapping(obj)->wam_keys_list);
        gc_mark_with_watchset(TheWeakAndMapping(obj)->wam_value);
      }
      break;
    }
    case Rectype_WeakOrMapping: {
      var uintL n = Lrecord_length(obj)-3;
      var bool some_alive = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakOrMapping(obj)->wom_keys[i];
        if (!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key))) {
          some_alive = true;
          break;
        }
      }
      if (some_alive) {
        gc_mark_with_watchset(TheWeakOrMapping(obj)->wom_keys_list);
        gc_mark_with_watchset(TheWeakOrMapping(obj)->wom_value);
      }
      break;
    }
    case Rectype_WeakAlist_Key: {
      var uintL n = (Lrecord_length(obj)-2) / 2;
      var uintL i;
      if (index == (uintL)~(uintL)0)
        for (i = 0; i < n; i++) {
          var object key = TheWeakAlist(obj)->wal_data[2*i+0];
          if (!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)))
            gc_mark_with_watchset(TheWeakAlist(obj)->wal_data[2*i+1]);
        }
      else {
        i = index;
        var object key = TheWeakAlist(obj)->wal_data[2*i+0];
        if (!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)))
          gc_mark_with_watchset(TheWeakAlist(obj)->wal_data[2*i+1]);
      }
      break;
    }
    case Rectype_WeakAlist_Value: {
      var uintL n = (Lrecord_length(obj)-2) / 2;
      var uintL i;
      if (index == (uintL)~(uintL)0)
        for (i = 0; i < n; i++) {
          var object value = TheWeakAlist(obj)->wal_data[2*i+1];
          if (!eq(value,unbound) && (gcinvariant_object_p(value) || alive(value)))
            gc_mark_with_watchset(TheWeakAlist(obj)->wal_data[2*i+0]);
        }
      else {
        i = index;
        var object value = TheWeakAlist(obj)->wal_data[2*i+1];
        if (!eq(value,unbound) && (gcinvariant_object_p(value) || alive(value)))
          gc_mark_with_watchset(TheWeakAlist(obj)->wal_data[2*i+0]);
      }
      break;
    }
    case Rectype_WeakAlist_Either:
      break;
    case Rectype_WeakAlist_Both: {
      var uintL n = (Lrecord_length(obj)-2) / 2;
      var uintL i;
      if (index == (uintL)~(uintL)0)
        for (i = 0; i < n; i++) {
          var object key = TheWeakAlist(obj)->wal_data[2*i+0];
          var object value = TheWeakAlist(obj)->wal_data[2*i+1];
          if (!eq(key,unbound) && !eq(value,unbound)) {
            var bool key_alive = (gcinvariant_object_p(key) || alive(key));
            var bool value_alive = (gcinvariant_object_p(value) || alive(value));
            if (key_alive && !value_alive)
              gc_mark_with_watchset(value);
            if (!key_alive && value_alive)
              gc_mark_with_watchset(key);
          }
        }
      else {
        i = index;
        var object key = TheWeakAlist(obj)->wal_data[2*i+0];
        var object value = TheWeakAlist(obj)->wal_data[2*i+1];
        if (!eq(key,unbound) && !eq(value,unbound)) {
          var bool key_alive = (gcinvariant_object_p(key) || alive(key));
          var bool value_alive = (gcinvariant_object_p(value) || alive(value));
          if (key_alive && !value_alive)
            gc_mark_with_watchset(value);
          if (!key_alive && value_alive)
            gc_mark_with_watchset(key);
        }
      }
      break;
    }
    case Rectype_WeakHashedAlist_Key: {
      var uintL n = (Lrecord_length(obj)-4) / 3;
      var uintL i;
      if (index == (uintL)~(uintL)0) {
        gc_mark_with_watchset(TheWeakHashedAlist(obj)->whal_itable);
        for (i = 0; i < n; i++) {
          var object key = TheWeakHashedAlist(obj)->whal_data[3*i+0];
          if (!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)))
            gc_mark_with_watchset(TheWeakHashedAlist(obj)->whal_data[3*i+1]);
        }
      } else {
        i = index;
        var object key = TheWeakHashedAlist(obj)->whal_data[3*i+0];
        if (!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)))
          gc_mark_with_watchset(TheWeakHashedAlist(obj)->whal_data[3*i+1]);
      }
      break;
    }
    case Rectype_WeakHashedAlist_Value: {
      var uintL n = (Lrecord_length(obj)-4) / 3;
      var uintL i;
      if (index == (uintL)~(uintL)0) {
        gc_mark_with_watchset(TheWeakHashedAlist(obj)->whal_itable);
        for (i = 0; i < n; i++) {
          var object value = TheWeakHashedAlist(obj)->whal_data[3*i+1];
          if (!eq(value,unbound) && (gcinvariant_object_p(value) || alive(value)))
            gc_mark_with_watchset(TheWeakHashedAlist(obj)->whal_data[3*i+0]);
        }
      } else {
        i = index;
        var object value = TheWeakHashedAlist(obj)->whal_data[3*i+1];
        if (!eq(value,unbound) && (gcinvariant_object_p(value) || alive(value)))
          gc_mark_with_watchset(TheWeakHashedAlist(obj)->whal_data[3*i+0]);
      }
      break;
    }
    case Rectype_WeakHashedAlist_Either:
      if (index == (uintL)~(uintL)0) {
        gc_mark_with_watchset(TheWeakHashedAlist(obj)->whal_itable);
      }
      break;
    case Rectype_WeakHashedAlist_Both: {
      var uintL n = (Lrecord_length(obj)-4) / 3;
      var uintL i;
      if (index == (uintL)~(uintL)0) {
        gc_mark_with_watchset(TheWeakHashedAlist(obj)->whal_itable);
        for (i = 0; i < n; i++) {
          var object key = TheWeakHashedAlist(obj)->whal_data[3*i+0];
          var object value = TheWeakHashedAlist(obj)->whal_data[3*i+1];
          if (!eq(key,unbound) && !eq(value,unbound)) {
            var bool key_alive = (gcinvariant_object_p(key) || alive(key));
            var bool value_alive = (gcinvariant_object_p(value) || alive(value));
            if (key_alive && !value_alive)
              gc_mark_with_watchset(value);
            if (!key_alive && value_alive)
              gc_mark_with_watchset(key);
          }
        }
      } else {
        i = index;
        var object key = TheWeakHashedAlist(obj)->whal_data[3*i+0];
        var object value = TheWeakHashedAlist(obj)->whal_data[3*i+1];
        if (!eq(key,unbound) && !eq(value,unbound)) {
          var bool key_alive = (gcinvariant_object_p(key) || alive(key));
          var bool value_alive = (gcinvariant_object_p(value) || alive(value));
          if (key_alive && !value_alive)
            gc_mark_with_watchset(value);
          if (!key_alive && value_alive)
            gc_mark_with_watchset(key);
        }
      }
      break;
    }
    default: abort();
  }
}

/* Straightens one pointer into a WeakHashedAlist. */
local inline object weak_hashed_alist_update_one (object kvtable, uintL n, object x) {
  loop {
    if (!(posfixnump(x) && posfixnum_to_V(x) < n)) abort();
    var gcv_object_t* KVptr = &TheWeakHashedAlist(kvtable)->whal_data[3*posfixnum_to_V(x)];
    if (!eq(KVptr[0],unbound))
      # Found an alive key/value pair at index x.
      break;
    # The key/value pair at index x is dead, continue following the chain.
    var object y = KVptr[2];
    # Move the cell at index x to the freelist.
    KVptr[2] = TheWeakHashedAlist(kvtable)->whal_freelist;
    TheWeakHashedAlist(kvtable)->whal_freelist = x;
    x = y;
    # Test whether the last element of the list was removed.
    if (eq(x,unbound))
      break;
  }
  return x;
}

/* Update the list structure inside a WeakHashedAlist after one or more
   (key, value) pairs have been removed.
   This pass doesn't assume anything about the hash code function or about
   whether the hash table's list structure is invalid and needs rehashing or
   not. All it does is to straighten pointers from itable to kvtable or
   inside kvtable, by removing list elements which correspond to removed
   (key, value) pairs. This pass _does_ assume that the itable and the
   kvtable's next fields contain indices that realize a list structure of
   pairwise disjoint lists of finite length. */
local void weak_hashed_alist_update (object kvtable) {
  var uintL n = (Lrecord_length(kvtable)-4) / 3;
  {
    var object itable = TheWeakHashedAlist(kvtable)->whal_itable;
    if (!eq(itable,unbound)) {
      if (!simple_vector_p(itable)) abort();
      var uintL m = Svector_length(itable);
      var uintL i;
      for (i = 0; i < m; i++) {
        var object x = TheSvector(itable)->data[i];
        if (!eq(x,unbound))
          TheSvector(itable)->data[i] =
            weak_hashed_alist_update_one(kvtable,n,x);
      }
    }
  }
  {
    var uintL i;
    for (i = 0; i < n; i++) {
      var object x = TheWeakHashedAlist(kvtable)->whal_data[3*i+2];
      if (!eq(x,unbound))
        TheWeakHashedAlist(kvtable)->whal_data[3*i+2] =
          weak_hashed_alist_update_one(kvtable,n,x);
    }
  }
}

/* Given an object of Rectype_Weak* type, clean the dead parts.
   Return a flag indicating whether the object should still be kept active. */
local bool weak_clean_dead (object obj) {
  if_recordp(obj, ; , abort(); );
  switch (Record_type(obj)) {
    case Rectype_Weakpointer: {
      var object value = TheWeakpointer(obj)->wp_value;
      if (gcinvariant_object_p(value))
        return false;
      if (alive(value))
        return true;
      else {
        TheWeakpointer(obj)->wp_value = unbound;
        return false;
      }
    }
    case Rectype_Weakmapping: {
      var object key = TheWeakmapping(obj)->wm_key;
      if (!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)))
        return true;
      else {
        TheWeakmapping(obj)->wm_key = unbound;
        TheWeakmapping(obj)->wm_value = unbound;
        return false;
      }
    }
    case Rectype_WeakAnd: {
      var uintL n = Lrecord_length(obj)-2;
      var bool all_alive = true;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakAnd(obj)->war_keys[i];
        if (!(!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)))) {
          all_alive = false;
          break;
        }
      }
      if (all_alive)
        return true;
      else {
        for (i = 0; i < n; i++)
          TheWeakAnd(obj)->war_keys[i] = unbound;
        TheWeakAnd(obj)->war_keys_list = unbound;
        return false;
      }
    }
    case Rectype_WeakOr: {
      var uintL n = Lrecord_length(obj)-2;
      var bool some_alive = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakOr(obj)->wor_keys[i];
        if (!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key))) {
          some_alive = true;
          break;
        }
      }
      if (some_alive)
        return true;
      else {
        for (i = 0; i < n; i++)
          TheWeakOr(obj)->wor_keys[i] = unbound;
        TheWeakOr(obj)->wor_keys_list = unbound;
        return false;
      }
    }
    case Rectype_WeakList: {
      var uintL n = Lrecord_length(obj)-2;
      var bool keep = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object value = TheWeakList(obj)->wl_elements[i];
        if (gcinvariant_object_p(value))
          ;
        else if (alive(value))
          keep = true;
        else {
          TheWeakList(obj)->wl_elements[i] = unbound;
          TheWeakList(obj)->wl_count = fixnum_inc(TheWeakList(obj)->wl_count,-1);
        }
      }
      return keep;
    }
    case Rectype_WeakAndMapping: {
      var uintL n = Lrecord_length(obj)-3;
      var bool all_alive = true;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakAndMapping(obj)->wam_keys[i];
        if (!(!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)))) {
          all_alive = false;
          break;
        }
      }
      if (all_alive)
        return true;
      else {
        for (i = 0; i < n; i++)
          TheWeakAndMapping(obj)->wam_keys[i] = unbound;
        TheWeakAndMapping(obj)->wam_keys_list = unbound;
        TheWeakAndMapping(obj)->wam_value = unbound;
        return false;
      }
    }
    case Rectype_WeakOrMapping: {
      var uintL n = Lrecord_length(obj)-3;
      var bool some_alive = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakOrMapping(obj)->wom_keys[i];
        if (!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key))) {
          some_alive = true;
          break;
        }
      }
      if (some_alive)
        return true;
      else {
        for (i = 0; i < n; i++)
          TheWeakOrMapping(obj)->wom_keys[i] = unbound;
        TheWeakOrMapping(obj)->wom_keys_list = unbound;
        TheWeakOrMapping(obj)->wom_value = unbound;
        return false;
      }
    }
    case Rectype_WeakAlist_Key: {
      var uintL n = (Lrecord_length(obj)-2) / 2;
      var bool keep = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakAlist(obj)->wal_data[2*i+0];
        if (eq(key,unbound))
          ;
        else if (gcinvariant_object_p(key) || alive(key))
          keep = true;
        else {
          TheWeakAlist(obj)->wal_data[2*i+0] = unbound;
          TheWeakAlist(obj)->wal_data[2*i+1] = unbound;
          TheWeakAlist(obj)->wal_count = fixnum_inc(TheWeakAlist(obj)->wal_count,-1);
        }
      }
      return keep;
    }
    case Rectype_WeakAlist_Value: {
      var uintL n = (Lrecord_length(obj)-2) / 2;
      var bool keep = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object value = TheWeakAlist(obj)->wal_data[2*i+1];
        if (eq(value,unbound))
          ;
        else if (gcinvariant_object_p(value) || alive(value))
          keep = true;
        else {
          TheWeakAlist(obj)->wal_data[2*i+0] = unbound;
          TheWeakAlist(obj)->wal_data[2*i+1] = unbound;
          TheWeakAlist(obj)->wal_count = fixnum_inc(TheWeakAlist(obj)->wal_count,-1);
        }
      }
      return keep;
    }
    case Rectype_WeakAlist_Either: {
      var uintL n = (Lrecord_length(obj)-2) / 2;
      var bool keep = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakAlist(obj)->wal_data[2*i+0];
        var object value = TheWeakAlist(obj)->wal_data[2*i+1];
        if (eq(key,unbound) && eq(value,unbound))
          ;
        else if ((!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)))
                 && (!eq(value,unbound) && (gcinvariant_object_p(value) || alive(value))))
          # both alive
          keep = true;
        else {
          TheWeakAlist(obj)->wal_data[2*i+0] = unbound;
          TheWeakAlist(obj)->wal_data[2*i+1] = unbound;
          TheWeakAlist(obj)->wal_count = fixnum_inc(TheWeakAlist(obj)->wal_count,-1);
        }
      }
      return keep;
    }
    case Rectype_WeakAlist_Both: {
      var uintL n = (Lrecord_length(obj)-2) / 2;
      var bool keep = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakAlist(obj)->wal_data[2*i+0];
        var object value = TheWeakAlist(obj)->wal_data[2*i+1];
        if (eq(key,unbound) && eq(value,unbound))
          ;
        else {
          bool key_alive = (!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)));
          bool value_alive = (!eq(value,unbound) && (gcinvariant_object_p(value) || alive(value)));
          if (key_alive || value_alive) {
            # key or value alive; the other one must be kept alive too.
            if (!(key_alive && value_alive)) abort();
            keep = true;
          } else {
            TheWeakAlist(obj)->wal_data[2*i+0] = unbound;
            TheWeakAlist(obj)->wal_data[2*i+1] = unbound;
            TheWeakAlist(obj)->wal_count = fixnum_inc(TheWeakAlist(obj)->wal_count,-1);
          }
        }
      }
      return keep;
    }
    case Rectype_WeakHashedAlist_Key: {
      var uintL n = (Lrecord_length(obj)-4) / 3;
      {
        var object itable = TheWeakHashedAlist(obj)->whal_itable;
        if (!eq(itable,unbound) && !alive(itable))
          abort();
      }
      if (!gcinvariant_object_p(TheWeakHashedAlist(obj)->whal_freelist))
        abort();
      var bool any_died = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakHashedAlist(obj)->whal_data[3*i+0];
        if (eq(key,unbound))
          ;
        else if (gcinvariant_object_p(key) || alive(key))
          ;
        else {
          TheWeakHashedAlist(obj)->whal_data[3*i+0] = unbound;
          TheWeakHashedAlist(obj)->whal_data[3*i+1] = unbound;
          TheWeakHashedAlist(obj)->whal_count = fixnum_inc(TheWeakHashedAlist(obj)->whal_count,-1);
          any_died = true;
        }
        if (!gcinvariant_object_p(TheWeakHashedAlist(obj)->whal_data[3*i+2]))
          abort();
      }
      if (any_died)
        weak_hashed_alist_update(obj);
      # Must keep obj active, even if its whal_count is 0: the whal_itable is
      # not seen by spvw_gcmark (due to Lrecord_nonweak_length(obj) = 0) and
      # must nevertheless be updated during the next GCs.
      return true;
    }
    case Rectype_WeakHashedAlist_Value: {
      var uintL n = (Lrecord_length(obj)-4) / 3;
      {
        var object itable = TheWeakHashedAlist(obj)->whal_itable;
        if (!eq(itable,unbound) && !alive(itable))
          abort();
      }
      if (!gcinvariant_object_p(TheWeakHashedAlist(obj)->whal_freelist))
        abort();
      var bool any_died = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object value = TheWeakHashedAlist(obj)->whal_data[3*i+1];
        if (eq(value,unbound))
          ;
        else if (gcinvariant_object_p(value) || alive(value))
          ;
        else {
          TheWeakHashedAlist(obj)->whal_data[3*i+0] = unbound;
          TheWeakHashedAlist(obj)->whal_data[3*i+1] = unbound;
          TheWeakHashedAlist(obj)->whal_count = fixnum_inc(TheWeakHashedAlist(obj)->whal_count,-1);
          any_died = true;
        }
        if (!gcinvariant_object_p(TheWeakHashedAlist(obj)->whal_data[3*i+2]))
          abort();
      }
      if (any_died)
        weak_hashed_alist_update(obj);
      # Must keep obj active, even if its whal_count is 0: the whal_itable is
      # not seen by spvw_gcmark (due to Lrecord_nonweak_length(obj) = 0) and
      # must nevertheless be updated during the next GCs.
      return true;
    }
    case Rectype_WeakHashedAlist_Either: {
      var uintL n = (Lrecord_length(obj)-4) / 3;
      {
        var object itable = TheWeakHashedAlist(obj)->whal_itable;
        if (!eq(itable,unbound) && !alive(itable))
          abort();
      }
      if (!gcinvariant_object_p(TheWeakHashedAlist(obj)->whal_freelist))
        abort();
      var bool any_died = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakHashedAlist(obj)->whal_data[3*i+0];
        var object value = TheWeakHashedAlist(obj)->whal_data[3*i+1];
        if (eq(key,unbound) && eq(value,unbound))
          ;
        else if ((!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)))
                 && (!eq(value,unbound) && (gcinvariant_object_p(value) || alive(value))))
          # both alive
          ;
        else {
          TheWeakHashedAlist(obj)->whal_data[3*i+0] = unbound;
          TheWeakHashedAlist(obj)->whal_data[3*i+1] = unbound;
          TheWeakHashedAlist(obj)->whal_count = fixnum_inc(TheWeakHashedAlist(obj)->whal_count,-1);
          any_died = true;
        }
        if (!gcinvariant_object_p(TheWeakHashedAlist(obj)->whal_data[3*i+2]))
          abort();
      }
      if (any_died)
        weak_hashed_alist_update(obj);
      # Must keep obj active, even if its whal_count is 0: the whal_itable is
      # not seen by spvw_gcmark (due to Lrecord_nonweak_length(obj) = 0) and
      # must nevertheless be updated during the next GCs.
      return true;
    }
    case Rectype_WeakHashedAlist_Both: {
      var uintL n = (Lrecord_length(obj)-4) / 3;
      {
        var object itable = TheWeakHashedAlist(obj)->whal_itable;
        if (!eq(itable,unbound) && !alive(itable))
          abort();
      }
      if (!gcinvariant_object_p(TheWeakHashedAlist(obj)->whal_freelist))
        abort();
      var bool any_died = false;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object key = TheWeakHashedAlist(obj)->whal_data[3*i+0];
        var object value = TheWeakHashedAlist(obj)->whal_data[3*i+1];
        if (eq(key,unbound) && eq(value,unbound))
          ;
        else {
          bool key_alive = (!eq(key,unbound) && (gcinvariant_object_p(key) || alive(key)));
          bool value_alive = (!eq(value,unbound) && (gcinvariant_object_p(value) || alive(value)));
          if (key_alive || value_alive) {
            # key or value alive; the other one must be kept alive too.
            if (!(key_alive && value_alive)) abort();
          } else {
            TheWeakHashedAlist(obj)->whal_data[3*i+0] = unbound;
            TheWeakHashedAlist(obj)->whal_data[3*i+1] = unbound;
            TheWeakHashedAlist(obj)->whal_count = fixnum_inc(TheWeakHashedAlist(obj)->whal_count,-1);
            any_died = true;
          }
        }
        if (!gcinvariant_object_p(TheWeakHashedAlist(obj)->whal_data[3*i+2]))
          abort();
      }
      if (any_died)
        weak_hashed_alist_update(obj);
      # Must keep obj active, even if its whal_count is 0: the whal_itable is
      # not seen by spvw_gcmark (due to Lrecord_nonweak_length(obj) = 0) and
      # must nevertheless be updated during the next GCs.
      return true;
    }
    default: abort();
  }
}

/* Handle the weak pointers at the end of the mark phase. */
local void gc_mark_weakpointers (object all_weakpointers) {
  /* 1. Build up the markwatchset. */
  { /* Gather all watchable objects. */
    var markwatch_t* accumulator = &markwatchset[0];
    var object L;
    for (L = all_weakpointers;
         !eq(L,Fixnum_0);
         L = ((Weakpointer)TheRecord(L))->wp_cdr) {
      var object obj = L;
      /* Watch the mark bit of the weak pointer itself. */
      add_watchable(&accumulator,obj,(uintL)~(uintL)0,obj);
      /* Watch the mark bits of the weak references that it contains. */
      accumulator = get_watchset(obj,accumulator);
    }
    markwatchset_count = accumulator - &markwatchset[0];
  }
  { /* Sort the markwatchset. */
    SORT(markwatch,sort)(markwatchset,markwatchset_count);
  }
  /* 2. Initialize the queue to empty. */
  var markwatch_t markwatch_queue_end;
  var markwatch_t* markwatch_queue_head = &markwatch_queue_end;
  markwatch_queue_tail = &markwatch_queue_head;
  /* 3. Propagate through the initially marked weak objects.
        (This is equivalent to initializing the queue with the initially
         marked weak objects. It is because of this step that add_watchable
         can ignore objects that are initially already marked.) */
  {
    var object L;
    for (L = all_weakpointers;
         !eq(L,Fixnum_0);
         L = ((Weakpointer)TheRecord(L))->wp_cdr) {
      var object obj = L;
      if (alive(obj))
        propagate_through_weak(obj,(uintL)~(uintL)0);
    }
  }
  /* 4. Execute the queued propagations. Each propagation can enqueue new
        requests, but since the markwatchset is finite, the queue eventually
        stops growing. */
  {
    var markwatch_t* queue_rest;
    for (queue_rest = markwatch_queue_head;
         queue_rest != &markwatch_queue_end;
         queue_rest = queue_rest->q_next) {
      var object obj = queue_rest->weakobj;
      if (alive(obj))
        propagate_through_weak(obj,queue_rest->weakindex);
    }
  }
}

/* Clean the dead parts of all_weakpointers and store the rest in
   O(all_weakpointers). */
local void clean_weakpointers (object all_weakpointers) {
  /* 5. Clean the dead parts. */
  var object Lu = all_weakpointers;
  var gcv_object_t* L1 = &O(all_weakpointers);
  while (!eq(Lu,Fixnum_0)) {
    if (!alive(Lu)) {
      /* The weak-pointer itself is dead. Remove it from the list. */
      markwatchset_size -= 1 + max_watchset_count(Lu);
      Lu = ((Weakpointer)TheRecord(Lu))->wp_cdr;
    } else {
      /* Clean dead parts inside the weak-pointer. */
      if (weak_clean_dead(Lu)) {
        /* Keep it in the list. */
        *L1 = Lu; L1 = &((Weakpointer)TheRecord(Lu))->wp_cdr; Lu = *L1;
      } else {
        var object next = ((Weakpointer)TheRecord(Lu))->wp_cdr;
        ((Weakpointer)TheRecord(Lu))->wp_cdr = unbound;
        Lu = next;
      }
    }
  }
  *L1 = Fixnum_0;
}

/* --------------------- Code executed outside the GC --------------------- */

/* We keep all WEAK-POINTER objects on the O(all_weakpointers) list unless the
   value that the WEAK-POINTER points to is GC-invariant.  This requires that
   we add the WEAK-POINTER to O(all_weakpointers) when the value is changed
   to a non-GC-invariant one, and GC removes the WEAK-POINTERs with GC-invariant
   values from O(all_weakpointers).
   The alternative is to keep all the WEAK-POINTERs on the list.
   We do not do that because we assume that the lifetime of a WEAK-POINTER is
   relatively high compared to GC timeout, so there will be several GCs while
   the given WEAK-POINTER is alive (why would one use a WEAK-POINTER otherwise?)
   and therefore it is worth the effort to keep O(all_weakpointers) as short
   as possible. */

/* Given a fresh object of Rectype_Weak* type, tells whether it should be
   added to the O(all_weakpointers). */
local bool weak_must_activate (object obj) {
  if_recordp(obj, ; , abort(); );
  switch (Record_type(obj)) {
    case Rectype_Weakpointer: {
      var object value = TheWeakpointer(obj)->wp_value;
      return (!gcinvariant_object_p(value));
    }
    case Rectype_WeakList: {
      var uintL n = Lrecord_length(obj)-2;
      var uintL i;
      for (i = 0; i < n; i++) {
        var object value = TheWeakList(obj)->wl_elements[i];
        if (!gcinvariant_object_p(value))
          return true;
      }
      return false;
    }
    case Rectype_Weakmapping:
    case Rectype_WeakAnd:
    case Rectype_WeakOr:
    case Rectype_WeakAndMapping:
    case Rectype_WeakOrMapping:
    case Rectype_WeakAlist_Key:
    case Rectype_WeakAlist_Value:
    case Rectype_WeakAlist_Either:
    case Rectype_WeakAlist_Both:
    case Rectype_WeakHashedAlist_Key:
    case Rectype_WeakHashedAlist_Value:
    case Rectype_WeakHashedAlist_Either:
    case Rectype_WeakHashedAlist_Both:
      return true;
    default: abort();
  }
}

/* Adds a freshly allocated object to the list of weak pointers.
 activate_weak(obj);
 > obj: A fresh but filled object of type Rectype_Weak* */
global void activate_weak (object obj) {
  if (weak_must_activate(obj)) {
    /* Ensure that markwatchset has enough room for the next GC. */
    var uintL need = 1 + max_watchset_count(obj);
    var uintM new_markwatchset_size = markwatchset_size + need;
    if (new_markwatchset_size > markwatchset_allocated) {
      /* Increment markwatchset_allocated by a proportional factor 1.5. */
      var uintM new_markwatchset_allocated = markwatchset_allocated + markwatchset_allocated/2;
      if (new_markwatchset_allocated < new_markwatchset_size)
        new_markwatchset_allocated = new_markwatchset_size;
      var markwatch_t* new_markwatchset = (markwatch_t*)my_malloc(new_markwatchset_allocated*sizeof(markwatch_t));
      /* Now that malloc() succeeded, we can free the old markwatchset and
         update the variables. */
      var markwatch_t* old_markwatchset = markwatchset;
      markwatchset = new_markwatchset;
      markwatchset_allocated = new_markwatchset_allocated;
      if (old_markwatchset != NULL)
        free(old_markwatchset);
    }
    markwatchset_size = new_markwatchset_size;
    /* Now that markwatchset is large enough, add obj to O(all_weakpointers). */
    ((Weakpointer)TheRecord(obj))->wp_cdr = O(all_weakpointers);
    O(all_weakpointers) = obj;
  }
}
