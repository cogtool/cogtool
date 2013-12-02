/*
 * Functions for weak references in CLISP
 * Bruno Haible 1999-2005
 * Sam Steingold 2003
 */
#include "lispbibl.c"

/* ============================= Weak Pointers ============================= */

/* (MAKE-WEAK-POINTER value)
   returns a fresh weak pointer referring to value. */
LISPFUN(make_weak_pointer,seclass_no_se,1,0,norest,nokey,0,NIL) {
  var object wp = allocate_xrecord(0,Rectype_Weakpointer,weakpointer_length,0,
                                   orecord_type);
  var object obj = popSTACK();
  TheWeakpointer(wp)->wp_value = obj;
  TheWeakpointer(wp)->wp_cdr = unbound; /* a GC-invariant dummy */
  activate_weak(wp); /* add to O(all_weakpointers) if needed */
  VALUES1(wp);
}

/* (WEAK-POINTER-P object)
   returns true if the object is of type WEAK-POINTER. */
LISPFUNNF(weak_pointer_p,1) {
  var object obj = popSTACK();
  VALUES_IF(weakpointerp(obj));
}

/* check_weakpointer_replacement(obj)
 > obj: not a weak-pointer
 < result: a weak-pointer, a replacement
 can trigger GC */
local maygc object check_weakpointer_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_pointer)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_pointer)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  } while (!weakpointerp(obj));
  return obj;
}
/* check_weakpointer(obj)
 > obj: an object
 < result: a weak-pointer, either the same as obj or a replacement
 can trigger GC */
local inline maygc object check_weakpointer (object obj) {
  if (!weakpointerp(obj))
    obj = check_weakpointer_replacement(obj);
  return obj;
}

/* (WEAK-POINTER-VALUE weak-pointer) returns two values: The original value
 and T, if the value has not yet been garbage collected, else NIL and NIL. */
LISPFUNNR(weak_pointer_value,1) {
  var object wp = check_weakpointer(popSTACK());
  if (weakpointer_broken_p(wp))
    VALUES2(NIL,NIL);
  else
    VALUES2(TheWeakpointer(wp)->wp_value, T);
}

/* (SETF (WEAK-POINTER-VALUE weak-pointer) value) */
LISPFUNN(set_weak_pointer_value,2) {
  # Stack layout: value, weak-pointer.
  var object wp = check_weakpointer(STACK_0);
  var object value = STACK_1;
  skipSTACK(2);
  TheWeakpointer(wp)->wp_value = value;
  if (eq(TheWeakpointer(wp)->wp_cdr,unbound))
    activate_weak(wp); /* add to O(all_weakpointers) if needed */
  /* If value is gc-invariant, we leave wp where it is. For removing it
     from O(all_weakpointers), this list ought to be a doubly-linked list.
     Anyway, the next GC will remove it from the list. */
  VALUES1(value);
}

/* ============================== Weak Lists ============================== */

/* Copy a list of length len into a weak-list of size maxlen >= len. */
local void copy_list_into_weak_list (object l, uintL len, object wl, uintL maxlen) {
  var uintL i;
  for (i = 0; i < len; i++, l = Cdr(l)) {
    if (atomp(l)) # Huh? The list became shorter meanwhile.
      break;
    TheWeakList(wl)->wl_elements[i] = Car(l);
  }
  TheWeakList(wl)->wl_count = fixnum(i);
  for (; i < maxlen; i++)
    TheWeakList(wl)->wl_elements[i] = unbound;
}

/* (MAKE-WEAK-LIST list)
   creates a weak list pointing to each of the elements in the given list. */
LISPFUNN(make_weak_list,1) {
  STACK_0 = check_list(STACK_0);
  var uintL len = llength(STACK_0);
  pushSTACK(allocate_xrecord(0,Rectype_MutableWeakList,mutableweaklist_length,
                             0,orecord_type));
  var object wl = allocate_lrecord(Rectype_WeakList,2+len,lrecord_type);
  TheWeakList(wl)->wp_cdr = unbound; /* a GC-invariant dummy */
  copy_list_into_weak_list(STACK_1,len,wl,len);
  activate_weak(wl); /* add to O(all_weakpointers) if needed */
  var object result = STACK_0;
  TheMutableWeakList(result)->mwl_list = wl;
  skipSTACK(2);
  VALUES1(result);
}

/* (WEAK-LIST-P object)
   returns true if the object is of type WEAK-LIST. */
LISPFUNNF(weak_list_p,1) {
  var object obj = popSTACK();
  VALUES_IF(orecordp(obj) && Record_type(obj)==Rectype_MutableWeakList);
}

/* check_weaklist_replacement(obj)
 > obj: not a weak-list
 < result: a weak-list, a replacement
 can trigger GC */
local maygc object check_weaklist_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_list)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_list)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  } while (!(orecordp(obj) && Record_type(obj)==Rectype_MutableWeakList));
  return obj;
}
/* check_weaklist(obj)
 > obj: an object
 < result: a weak-list, either the same as obj or a replacement
 can trigger GC */
local inline maygc object check_weaklist (object obj) {
  if (!(orecordp(obj) && Record_type(obj)==Rectype_MutableWeakList))
    obj = check_weaklist_replacement(obj);
  return obj;
}

/* (WEAK-LIST-LIST weak-list)
   returns a list of those objects from the list that are still alive. */
LISPFUNNR(weak_list_list,1) {
  var object mwl = STACK_0 = check_weaklist(STACK_0);
  var object wl = TheMutableWeakList(mwl)->mwl_list;
  var uintL len = posfixnum_to_V(TheWeakList(wl)->wl_count);
  # Allocate result list.
  pushSTACK(NIL);
  var object result = make_list(len);
  skipSTACK(1);
  # Fetch mwl, wl, len again.
  mwl = popSTACK();
  wl = TheMutableWeakList(mwl)->mwl_list;
  var uintL newlen = posfixnum_to_V(TheWeakList(wl)->wl_count);
  # The weak list may have shrunk during the allocation of the result.
  ASSERT(newlen <= len);
  for (; len > newlen; len--)
    result = Cdr(result);
  { # Fill the result list.
    var uintL i = 0;
    var object l;
    for (l = result; newlen > 0; l = Cdr(l), newlen--) {
      var object element;
      do
        element = TheWeakList(wl)->wl_elements[i++];
      while (eq(element,unbound));
      Car(l) = element;
    }
  }
  VALUES1(result);
}

/* (SETF (WEAK-LIST-LIST weak-list) list)
   replaces the list of objects stored by a weak-list. */
LISPFUNN(set_weak_list_list,2) {
  # Stack layout: list, weak-list.
  STACK_0 = check_weaklist(STACK_0);
  STACK_1 = check_list(STACK_1);
  var uintL len = llength(STACK_1);
  var uintL maxlen = Lrecord_length(TheMutableWeakList(STACK_0)->mwl_list)-2;
  if (len <= maxlen) {
    # Can reuse the WeakList object.
    var object wl = TheMutableWeakList(STACK_0)->mwl_list;
    set_break_sem_1();
    copy_list_into_weak_list(STACK_1,len,wl,maxlen);
    clr_break_sem_1();
    if (eq(TheWeakList(wl)->wp_cdr,unbound))
      activate_weak(wl); /* add to O(all_weakpointers) if needed */
  } else {
    # Need to allocate a new WeakList object.
    maxlen = maxlen + maxlen/4; # augment size proportionally
    if (maxlen < len)
      maxlen = len;
    var object wl = allocate_lrecord(Rectype_WeakList,2+maxlen,lrecord_type);
    TheWeakList(wl)->wp_cdr = unbound; /* a GC-invariant dummy */
    copy_list_into_weak_list(STACK_1,len,wl,maxlen);
    activate_weak(wl); /* add to O(all_weakpointers) if needed */
    TheMutableWeakList(STACK_0)->mwl_list = wl;
  }
  VALUES1(STACK_1);
  skipSTACK(2);
}

/* ========================== Weak And-Relations ========================== */

/* (MAKE-WEAK-AND-RELATION list)
   creates a weak "and" relation between the objects in the given list. */
LISPFUNN(make_weak_and_relation,1) {
  STACK_0 = copy_list(check_list(STACK_0));
  var uintL len = llength(STACK_0);
  var object war = allocate_lrecord(Rectype_WeakAnd,2+len,lrecord_type);
  TheWeakAnd(war)->wp_cdr = unbound; /* a GC-invariant dummy */
  var object keys_list = popSTACK();
  TheWeakAnd(war)->war_keys_list = keys_list;
  {
    var uintL i;
    var object l;
    for (i = 0, l = keys_list; i < len; i++, l = Cdr(l))
      TheWeakAnd(war)->war_keys[i] = Car(l);
  }
  if (len > 0)
    activate_weak(war); /* add to O(all_weakpointers) if needed */
  VALUES1(war);
}

/* (WEAK-AND-RELATION-P object)
   returns true if the object is of type WEAK-AND-RELATION. */
LISPFUNNF(weak_and_relation_p,1) {
  var object obj = popSTACK();
  VALUES_IF(lrecordp(obj) && Record_type(obj)==Rectype_WeakAnd);
}

/* check_weakandrelation_replacement(obj)
 > obj: not a weak-and-relation
 < result: a weak-and-relation, a replacement
 can trigger GC */
local maygc object check_weakandrelation_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_and_relation)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_and_relation)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  } while (!(lrecordp(obj) && Record_type(obj)==Rectype_WeakAnd));
  return obj;
}
/* check_weakandrelation(obj)
 > obj: an object
 < result: a weak-and-relation, either the same as obj or a replacement
 can trigger GC */
local inline maygc object check_weakandrelation (object obj) {
  if (!(lrecordp(obj) && Record_type(obj)==Rectype_WeakAnd))
    obj = check_weakandrelation_replacement(obj);
  return obj;
}

/* (WEAK-AND-RELATION-LIST weak-and-relation)
   returns the list of objects stored in the relation. The returned list must
   not be destructively modified. */
LISPFUNNR(weak_and_relation_list,1) {
  var object war = check_weakandrelation(popSTACK());
  var object keys_list = TheWeakAnd(war)->war_keys_list;
  if (eq(keys_list,unbound))
    keys_list = NIL;
  VALUES1(keys_list);
}

/* =========================== Weak Or-Relations =========================== */

/* (MAKE-WEAK-OR-RELATION list)
   creates a weak "or" relation between the objects in the given list. */
LISPFUNN(make_weak_or_relation,1) {
  STACK_0 = copy_list(check_list(STACK_0));
  var uintL len = llength(STACK_0);
  var object wor = allocate_lrecord(Rectype_WeakOr,2+len,lrecord_type);
  TheWeakOr(wor)->wp_cdr = unbound; /* a GC-invariant dummy */
  var object keys_list = popSTACK();
  TheWeakOr(wor)->wor_keys_list = keys_list;
  {
    var uintL i;
    var object l;
    for (i = 0, l = keys_list; i < len; i++, l = Cdr(l))
      TheWeakOr(wor)->wor_keys[i] = Car(l);
  }
  if (len > 0)
    activate_weak(wor); /* add to O(all_weakpointers) if needed */
  VALUES1(wor);
}

/* (WEAK-OR-RELATION-P object)
   returns true if the object is of type WEAK-OR-RELATION. */
LISPFUNNF(weak_or_relation_p,1) {
  var object obj = popSTACK();
  VALUES_IF(lrecordp(obj) && Record_type(obj)==Rectype_WeakOr);
}

/* check_weakorrelation_replacement(obj)
 > obj: not a weak-or-relation
 < result: a weak-or-relation, a replacement
 can trigger GC */
local maygc object check_weakorrelation_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_or_relation)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_or_relation)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  } while (!(lrecordp(obj) && Record_type(obj)==Rectype_WeakOr));
  return obj;
}
/* check_weakorrelation(obj)
 > obj: an object
 < result: a weak-or-relation, either the same as obj or a replacement
 can trigger GC */
local inline maygc object check_weakorrelation (object obj) {
  if (!(lrecordp(obj) && Record_type(obj)==Rectype_WeakOr))
    obj = check_weakorrelation_replacement(obj);
  return obj;
}

/* (WEAK-OR-RELATION-LIST weak-or-relation)
   returns the list of objects stored in the relation. The returned list must
   not be destructively modified. */
LISPFUNNR(weak_or_relation_list,1) {
  var object wor = check_weakorrelation(popSTACK());
  var object keys_list = TheWeakOr(wor)->wor_keys_list;
  if (eq(keys_list,unbound))
    keys_list = NIL;
  VALUES1(keys_list);
}

/* ============================= Weak Mappings ============================= */

/* (MAKE-WEAK-MAPPING key value)
   creates a weak association. */
LISPFUNN(make_weak_mapping,2) {
  var object wm = allocate_xrecord(0,Rectype_Weakmapping,weakmapping_length,0,
                                   orecord_type);
  TheWeakmapping(wm)->wm_value = popSTACK();
  TheWeakmapping(wm)->wm_key = popSTACK();
  TheWeakmapping(wm)->wp_cdr = unbound; /* a GC-invariant dummy */
  activate_weak(wm); /* add to O(all_weakpointers) if needed */
  VALUES1(wm);
}

/* (WEAK-MAPPING-P object)
   returns true if the object is of type WEAK-MAPPING. */
LISPFUNNF(weak_mapping_p,1) {
  var object obj = popSTACK();
  VALUES_IF(orecordp(obj) && Record_type(obj)==Rectype_Weakmapping);
}

/* check_weakmapping_replacement(obj)
 > obj: not a weak-mapping
 < result: a weak-mapping, a replacement
 can trigger GC */
local maygc object check_weakmapping_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_mapping)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_mapping)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  } while (!(orecordp(obj) && Record_type(obj)==Rectype_Weakmapping));
  return obj;
}
/* check_weakmapping(obj)
 > obj: an object
 < result: a weak-mapping, either the same as obj or a replacement
 can trigger GC */
local inline maygc object check_weakmapping (object obj) {
  if (!(orecordp(obj) && Record_type(obj)==Rectype_Weakmapping))
    obj = check_weakmapping_replacement(obj);
  return obj;
}

/* (WEAK-MAPPING-PAIR weak-mapping)
   returns three values: the original key, the original value, and T, if the
   key has not yet been garbage-collected, else NIL, NIL, NIL. */
LISPFUNNR(weak_mapping_pair,1) {
  var object wm = check_weakmapping(popSTACK());
  if (eq(TheWeakmapping(wm)->wm_key,unbound))
    VALUES3(NIL,NIL,NIL);
  else
    VALUES3(TheWeakmapping(wm)->wm_key,TheWeakmapping(wm)->wm_value,T);
}

/* (WEAK-MAPPING-VALUE weak-mapping)
   returns the value, if the key has not yet been garbage-collected, else
   NIL. */
LISPFUNNR(weak_mapping_value,1) {
  var object wm = check_weakmapping(popSTACK());
  if (eq(TheWeakmapping(wm)->wm_key,unbound))
    VALUES1(NIL);
  else
    VALUES1(TheWeakmapping(wm)->wm_value);
}

/* (SETF (WEAK-MAPPING-VALUE weak-mapping) value)
   replaces the value stored in a weak-mapping. It has no effect when the key
   has already been garbage-collected. */
LISPFUNN(set_weak_mapping_value,2) {
  # Stack layout: value, weak-mapping.
  var object wm = check_weakmapping(STACK_0);
  var object value = STACK_1;
  skipSTACK(2);
  if (!eq(TheWeakmapping(wm)->wm_key,unbound)) {
    TheWeakmapping(wm)->wm_value = value;
    if (eq(TheWeakmapping(wm)->wp_cdr,unbound))
      activate_weak(wm); /* add to O(all_weakpointers) if needed */
  }
  VALUES1(value);
}

/* =========================== Weak And-Mappings =========================== */

/* (MAKE-WEAK-AND-MAPPING keys value)
   creates a weak "and" mapping between the keys objects in the given list
   and the given value. The keys list must be non-empty. */
LISPFUNN(make_weak_and_mapping,2) {
  STACK_1 = check_list(STACK_1);
  if (matomp(STACK_1)) {
    pushSTACK(STACK_1); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(cons)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: the keys list argument is empty"));
  }
  STACK_1 = copy_list(STACK_1);
  var uintL len = llength(STACK_1);
  var object wam = allocate_lrecord(Rectype_WeakAndMapping,3+len,lrecord_type);
  TheWeakAndMapping(wam)->wp_cdr = unbound; /* a GC-invariant dummy */
  TheWeakAndMapping(wam)->wam_value = popSTACK();
  var object keys_list = popSTACK();
  TheWeakAndMapping(wam)->wam_keys_list = keys_list;
  {
    var uintL i;
    var object l;
    for (i = 0, l = keys_list; i < len; i++, l = Cdr(l))
      TheWeakAndMapping(wam)->wam_keys[i] = Car(l);
  }
  activate_weak(wam); /* add to O(all_weakpointers) if needed */
  VALUES1(wam);
}

/* (WEAK-AND-MAPPING-P object) returns true if the object is of type
   WEAK-AND-MAPPING. */
LISPFUNNF(weak_and_mapping_p,1) {
  var object obj = popSTACK();
  VALUES_IF(lrecordp(obj) && Record_type(obj)==Rectype_WeakAndMapping);
}

/* check_weakandmapping_replacement(obj)
 > obj: not a weak-and-mapping
 < result: a weak-and-mapping, a replacement
 can trigger GC */
local maygc object check_weakandmapping_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_and_mapping)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_and_mapping)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  } while (!(lrecordp(obj) && Record_type(obj)==Rectype_WeakAndMapping));
  return obj;
}
/* check_weakandmapping(obj)
 > obj: an object
 < result: a weak-and-mapping, either the same as obj or a replacement
 can trigger GC */
local inline maygc object check_weakandmapping (object obj) {
  if (!(lrecordp(obj) && Record_type(obj)==Rectype_WeakAndMapping))
    obj = check_weakandmapping_replacement(obj);
  return obj;
}

/* (WEAK-AND-MAPPING-PAIR weak-and-mapping)
   returns three values: the list of keys, the value, and T, if none of the
   keys have been garbage-collected, else NIL, NIL, NIL. The returned keys list
   must not be destructively modified. */
LISPFUNNR(weak_and_mapping_pair,1) {
  var object wam = check_weakandmapping(popSTACK());
  if (eq(TheWeakAndMapping(wam)->wam_keys_list,unbound))
    VALUES3(NIL,NIL,NIL);
  else
    VALUES3(TheWeakAndMapping(wam)->wam_keys_list,TheWeakAndMapping(wam)->wam_value,T);
}

/* (WEAK-AND-MAPPING-VALUE weak-and-mapping)
   returns the value, if none of the keys have been garbage-collected, else
   NIL. */
LISPFUNNR(weak_and_mapping_value,1) {
  var object wam = check_weakandmapping(popSTACK());
  if (eq(TheWeakAndMapping(wam)->wam_keys_list,unbound))
    VALUES1(NIL);
  else
    VALUES1(TheWeakAndMapping(wam)->wam_value);
}

/* (SETF (WEAK-AND-MAPPING-VALUE weak-and-mapping-value) value)
   replaces the value stored in a weak-and-mapping. It has no effect when some
   key has already been garbage-collected. */
LISPFUNN(set_weak_and_mapping_value,2) {
  # Stack layout: value, weak-and-mapping.
  var object wam = check_weakandmapping(STACK_0);
  var object value = STACK_1;
  skipSTACK(2);
  if (!eq(TheWeakAndMapping(wam)->wam_keys_list,unbound)) {
    TheWeakAndMapping(wam)->wam_value = value;
    if (eq(TheWeakAndMapping(wam)->wp_cdr,unbound))
      activate_weak(wam); /* add to O(all_weakpointers) if needed */
  }
  VALUES1(value);
}

/* =========================== Weak Or-Mappings =========================== */

/* (MAKE-WEAK-OR-MAPPING keys value)
   creates a weak "or" mapping between the keys objects in the given list
   and the given value. The keys list must be non-empty. */
LISPFUNN(make_weak_or_mapping,2) {
  STACK_1 = check_list(STACK_1);
  if (matomp(STACK_1)) {
    pushSTACK(STACK_1); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(cons)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: the keys list argument is empty"));
  }
  STACK_1 = copy_list(STACK_1);
  var uintL len = llength(STACK_1);
  var object wom = allocate_lrecord(Rectype_WeakOrMapping,3+len,lrecord_type);
  TheWeakOrMapping(wom)->wp_cdr = unbound; /* a GC-invariant dummy */
  TheWeakOrMapping(wom)->wom_value = popSTACK();
  var object keys_list = popSTACK();
  TheWeakOrMapping(wom)->wom_keys_list = keys_list;
  {
    var uintL i;
    var object l;
    for (i = 0, l = keys_list; i < len; i++, l = Cdr(l))
      TheWeakOrMapping(wom)->wom_keys[i] = Car(l);
  }
  activate_weak(wom); /* add to O(all_weakpointers) if needed */
  VALUES1(wom);
}

/* (WEAK-OR-MAPPING-P object) returns true if the object is of type
   WEAK-OR-MAPPING. */
LISPFUNNF(weak_or_mapping_p,1) {
  var object obj = popSTACK();
  VALUES_IF(lrecordp(obj) && Record_type(obj)==Rectype_WeakOrMapping);
}

/* check_weakormapping_replacement(obj)
 > obj: not a weak-or-mapping
 < result: a weak-or-mapping, a replacement
 can trigger GC */
local maygc object check_weakormapping_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_or_mapping)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_or_mapping)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  } while (!(lrecordp(obj) && Record_type(obj)==Rectype_WeakOrMapping));
  return obj;
}
/* check_weakormapping(obj)
 > obj: an object
 < result: a weak-or-mapping, either the same as obj or a replacement
 can trigger GC */
local inline maygc object check_weakormapping (object obj) {
  if (!(lrecordp(obj) && Record_type(obj)==Rectype_WeakOrMapping))
    obj = check_weakormapping_replacement(obj);
  return obj;
}

/* (WEAK-OR-MAPPING-PAIR weak-or-mapping)
   returns three values: the list of keys, the value, and T, if the keys have
   not yet been garbage-collected, else NIL, NIL, NIL. The returned keys list
   must not be destructively modified. */
LISPFUNNR(weak_or_mapping_pair,1) {
  var object wom = check_weakormapping(popSTACK());
  if (eq(TheWeakOrMapping(wom)->wom_keys_list,unbound))
    VALUES3(NIL,NIL,NIL);
  else
    VALUES3(TheWeakOrMapping(wom)->wom_keys_list,TheWeakOrMapping(wom)->wom_value,T);
}

/* (WEAK-OR-MAPPING-VALUE weak-or-mapping)
   returns the value, if the keys have not yet been garbage-collected, else
   NIL. */
LISPFUNNR(weak_or_mapping_value,1) {
  var object wom = check_weakormapping(popSTACK());
  if (eq(TheWeakOrMapping(wom)->wom_keys_list,unbound))
    VALUES1(NIL);
  else
    VALUES1(TheWeakOrMapping(wom)->wom_value);
}

/* (SETF (WEAK-OR-MAPPING-VALUE weak-or-mapping-value) value)
   replaces the value stored in a weak-or-mapping. It has no effect when the
   keys have already been garbage-collected. */
LISPFUNN(set_weak_or_mapping_value,2) {
  # Stack layout: value, weak-or-mapping.
  var object wom = check_weakormapping(STACK_0);
  var object value = STACK_1;
  skipSTACK(2);
  if (!eq(TheWeakOrMapping(wom)->wom_keys_list,unbound)) {
    TheWeakOrMapping(wom)->wom_value = value;
    if (eq(TheWeakOrMapping(wom)->wp_cdr,unbound))
      activate_weak(wom); /* add to O(all_weakpointers) if needed */
  }
  VALUES1(value);
}

/* ======================== Weak Association Lists ======================== */

# Note that the GC is not allowed to compact the pairs in a WeakAlist,
# because that would lead to undefined behaviour in WEAK-ALIST-ASSOC and
# WEAK-ALIST-RASSOC. But (SETF WEAK-ALIST-VALUE) is allowed to compact a
# WeakAlist.

/* Copy an alist of length len into a weak-alist of size maxlen >= len. */
local void copy_alist_into_weak_alist (object list, uintL len, object wal, uintL maxlen) {
  var uintL i;
  var object l;
  for (i = 0, l = list; i < len; i++, l = Cdr(l)) {
    if (atomp(l)) # Huh? The list became shorter meanwhile.
      break;
    var object pair = Car(l);
    if (!consp(pair)) {
      clr_break_sem_1();
      pushSTACK(pair);    /* TYPE-ERROR slot DATUM */
      pushSTACK(S(cons)); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(list); pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,GETTEXT("~S: ~S is not an association list"));
    }
    TheWeakAlist(wal)->wal_data[2*i+0] = Car(pair);
    TheWeakAlist(wal)->wal_data[2*i+1] = Cdr(pair);
  }
  TheWeakAlist(wal)->wal_count = fixnum(i);
  for (; i < maxlen; i++) {
    TheWeakAlist(wal)->wal_data[2*i+0] = unbound;
    TheWeakAlist(wal)->wal_data[2*i+1] = unbound;
  }
}

/* (MAKE-WEAK-ALIST :type :initial-contents)
   creates a weak association. The type argument must be one of the four
   mentioned types; the default is :KEY. The initial-contents argument must be
   an alist. */
LISPFUN(make_weak_alist,seclass_read,0,0,norest,key,2,
        (kw(type),kw(initial_contents)) )
{ # Stack layout: type, initial-contents.
  # Check the type.
  var object type = STACK_1;
  var sintB rectype;
  if (eq(type,unbound) || eq(type,S(Kkey))) # :KEY
    rectype = Rectype_WeakAlist_Key;
  else if (eq(type,S(Kvalue))) # :VALUE
    rectype = Rectype_WeakAlist_Value;
  else if (eq(type,S(Kkey_and_value))) # :KEY-AND-VALUE
    rectype = Rectype_WeakAlist_Either;
  else if (eq(type,S(Kkey_or_value))) # :KEY-OR-VALUE
    rectype = Rectype_WeakAlist_Both;
  else {
    pushSTACK(type); /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_weak_alist)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(Kkey_or_value)); pushSTACK(S(Kkey_and_value)); pushSTACK(S(Kvalue)); pushSTACK(S(Kkey));
    pushSTACK(type); pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: argument ~S should be ~S, ~S, ~S or ~S."));
  }
  # Check the initial-contents.
  if (eq(STACK_0,unbound))
    STACK_0 = NIL;
  else
    STACK_0 = check_list(STACK_0);
  var uintL len = llength(STACK_0);
  pushSTACK(allocate_xrecord(0,Rectype_MutableWeakAlist,mutableweakalist_length,
                             0,orecord_type));
  var object wal = allocate_lrecord(rectype,2+2*len,lrecord_type);
  TheWeakList(wal)->wp_cdr = unbound; /* a GC-invariant dummy */
  copy_alist_into_weak_alist(STACK_1,len,wal,len);
  activate_weak(wal); /* add to O(all_weakpointers) if needed */
  var object result = STACK_0;
  TheMutableWeakAlist(result)->mwal_list = wal;
  skipSTACK(3);
  VALUES1(result);
}

/* (WEAK-ALIST-P object)
   returns true if the object is of type WEAK-ALIST. */
LISPFUNNF(weak_alist_p,1) {
  var object obj = popSTACK();
  VALUES_IF(orecordp(obj) && Record_type(obj)==Rectype_MutableWeakAlist);
}

/* check_weakalist_replacement(obj)
 > obj: not a weak-alist
 < result: a weak-alist, a replacement
 can trigger GC */
local maygc object check_weakalist_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_list)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(weak_list)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  } while (!(orecordp(obj) && Record_type(obj)==Rectype_MutableWeakAlist));
  return obj;
}
/* check_weakalist(obj)
 > obj: an object
 < result: a weak-alist, either the same as obj or a replacement
 can trigger GC */
local inline maygc object check_weakalist (object obj) {
  if (!(orecordp(obj) && Record_type(obj)==Rectype_MutableWeakAlist))
    obj = check_weakalist_replacement(obj);
  return obj;
}

/* (WEAK-ALIST-TYPE weak-alist)
   returns the type of the weak-alist. */
LISPFUNNR(weak_alist_type,1) {
  var object type;
  switch (Record_type(TheMutableWeakAlist(popSTACK())->mwal_list)) {
    case Rectype_WeakAlist_Key:
      type = S(Kkey); break;
    case Rectype_WeakAlist_Value:
      type = S(Kvalue); break;
    case Rectype_WeakAlist_Either:
      type = S(Kkey_and_value); break;
    case Rectype_WeakAlist_Both:
      type = S(Kkey_or_value); break;
    default: NOTREACHED;
  }
  VALUES1(type);
}

/* (WEAK-ALIST-CONTENTS weak-alist)
   returns an alist that corresponds to the current contents of the
   weak-alist. */
LISPFUNNR(weak_alist_contents,1) {
  var object mwal = STACK_0 = check_weakalist(STACK_0);
  var object wal = TheMutableWeakAlist(mwal)->mwal_list;
  var uintL len = posfixnum_to_V(TheWeakAlist(wal)->wal_count);
  # Allocate result list.
  pushSTACK(NIL);
  var object result = make_list(2*len);
  skipSTACK(1);
  # Fetch mwal, wal, len again.
  mwal = popSTACK();
  wal = TheMutableWeakAlist(mwal)->mwal_list;
  var uintL newlen = posfixnum_to_V(TheWeakAlist(wal)->wal_count);
  # The weak alist may have shrunk during the allocation of the result.
  ASSERT(newlen <= len);
  for (; len > newlen; len--)
    result = Cdr(Cdr(result));
  { # Fill the result list.
    var uintL i = 0;
    var object l;
    for (l = result; newlen > 0; l = Cdr(l), newlen--) {
      var object key;
      var object value;
      bool bound;
      do {
        key = TheWeakAlist(wal)->wal_data[2*i+0];
        value = TheWeakAlist(wal)->wal_data[2*i+1];
        bound = !eq(key,unbound);
        ASSERT(bound == !eq(value,unbound));
        i++;
      } while (!bound);
      var object pair = Cdr(l);
      Cdr(l) = Cdr(pair);
      Car(l) = pair;
      Car(pair) = key; Cdr(pair) = value;
    }
  }
  VALUES1(result);
}

/* (SETF (WEAK-ALIST-CONTENTS weak-alist) contents)
   replaces the contents of a weak-alist. The contents argument must be an
   alist. */
LISPFUNN(set_weak_alist_contents,2) {
  # Stack layout: contents, weak-alist.
  STACK_0 = check_weakalist(STACK_0);
  STACK_1 = check_list(STACK_1);
  var uintL len = llength(STACK_1);
  var uintL maxlen = (Lrecord_length(TheMutableWeakAlist(STACK_0)->mwal_list)-2)/2;
  if (len <= maxlen) {
    # Can reuse the WeakAlist object.
    var object wal = TheMutableWeakAlist(STACK_0)->mwal_list;
    set_break_sem_1();
    copy_alist_into_weak_alist(STACK_1,len,wal,maxlen);
    clr_break_sem_1();
    #if 0 /* weak_must_activate(wal) is independent of its contents. */
    if (eq(TheWeakAlist(wal)->wp_cdr,unbound))
      activate_weak(wal); /* add to O(all_weakpointers) if needed */
    #endif
  } else {
    # Need to allocate a new WeakAlist object.
    maxlen = maxlen + maxlen/4; # augment size proportionally
    if (maxlen < len)
      maxlen = len;
    var sintB rectype = Record_type(TheMutableWeakAlist(STACK_0)->mwal_list);
    var object wal = allocate_lrecord(rectype,2+2*maxlen,lrecord_type);
    TheWeakAlist(wal)->wp_cdr = unbound; /* a GC-invariant dummy */
    copy_alist_into_weak_alist(STACK_1,len,wal,maxlen);
    activate_weak(wal); /* add to O(all_weakpointers) if needed */
    TheMutableWeakAlist(STACK_0)->mwal_list = wal;
  }
  VALUES1(STACK_1);
  skipSTACK(2);
}

# UP: Überprüft das :KEY-Argument
# test_key_arg()
# > STACK_0: optionales Argument
# < STACK_0: korrekte KEY-Funktion
local void test_key_arg (void) {
  var object key_arg = STACK_0;
  if (missingp(key_arg))
    STACK_0 = L(identity); # #'IDENTITY als Default für :KEY
}

# Applies a :KEY argument.
# funcall_key(key,item);
# > key: value of the :KEY argument
# > item: object being considered
# < value1: (FUNCALL key item)
#define funcall_key(key,item)                     \
  {                                               \
    var object _key = (key);                      \
    var object _item = (item);                    \
    # shortcut for :KEY #'IDENTITY, very frequent \
    if (!eq(_key,L(identity))) {                  \
      pushSTACK(_item); funcall(_key,1);          \
    } else {                                      \
      value1 = _item;                             \
    }                                             \
  }

# Unterprogramm zum Ausführen des Tests :TEST
# up_test(stackptr,x)
# > *(stackptr+1): die Testfunktion
# > *(stackptr+3): das zu vergleichende Item
# > x: Argument
# < ergebnis: true falls der Test erfüllt ist, false sonst
# can trigger GC
local maygc bool up_test (const gcv_object_t* stackptr, object x) {
  # nach CLTL S. 247 ein (funcall testfun item x) ausführen:
  var object item = *(stackptr STACKop 3);
  var object fun = *(stackptr STACKop 1);
  # Special case the most frequent cases,
  if (eq(fun,L(eq)))
    return eq(item,x);
  if (eq(fun,L(eql)))
    return eql(item,x);
  if (eq(fun,L(equal)))
    return equal(item,x);
  pushSTACK(item);
  pushSTACK(x); # x
  funcall(fun,2);
  if (nullp(value1))
    return false;
  else
    return true;
}

# Unterprogramm zum Ausführen des Tests :TEST-NOT
# up_test_not(stackptr,x)
# > *(stackptr+0): die Testfunktion
# > *(stackptr+3): das zu vergleichende Item
# > x: Argument
# < ergebnis: true falls der Test erfüllt ist, false sonst
# can trigger GC
local maygc bool up_test_not (const gcv_object_t* stackptr, object x) {
  # nach CLTL S. 247 ein (not (funcall testfun item x)) ausführen:
  pushSTACK(*(stackptr STACKop 3)); # item
  pushSTACK(x); # x
  funcall(*(stackptr STACKop 0),2);
  if (nullp(value1))
    return true;
  else
    return false;
}

# UP: Überprüft die :TEST, :TEST-NOT - Argumente
# test_test_args()
# > stackptr: Pointer in den STACK
# > *(stackptr+1): :TEST-Argument
# > *(stackptr+0): :TEST-NOT-Argument
# < *(stackptr+1): verarbeitetes :TEST-Argument
# < *(stackptr+0): verarbeitetes :TEST-NOT-Argument
# < up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#       > stackptr: derselbe Pointer in den Stack, *(stackptr+3) = item,
#         *(stackptr+1) = :test-Argument, *(stackptr+0) = :test-not-Argument,
#       > x: Argument
#       < true, falls der Test erfüllt ist, false sonst.
  # up_function_t sei der Typ der Adresse einer solchen Testfunktion:
typedef maygc bool (*up_function_t) (const gcv_object_t* stackptr, object x);
local up_function_t test_test_args (gcv_object_t* stackptr) {
  var object test_arg = *(stackptr STACKop 1);
  if (!boundp(test_arg))
    test_arg = NIL;
  # test_arg ist das :TEST-Argument
  var object test_not_arg = *(stackptr STACKop 0);
  if (!boundp(test_not_arg))
    test_not_arg = NIL;
  # test_not_arg ist das :TEST-NOT-Argument
  if (nullp(test_not_arg)) {
    # :TEST-NOT wurde nicht angegeben
    if (nullp(test_arg))
      *(stackptr STACKop 1) = L(eql); # #'EQL als Default für :TEST
    return &up_test;
  } else {
    # :TEST-NOT wurde angegeben
    if (nullp(test_arg))
      return &up_test_not;
    else
      fehler_both_tests();
  }
}

/* (WEAK-ALIST-ASSOC item weak-alist [:test] [:test-not] [:key])
   is equivalent to
   (ASSOC item (WEAK-ALIST-CONTENTS weak-alist) [:test] [:test-not] [:key]). */
LISPFUN(weak_alist_assoc,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
{ # Stack layout: item, weak-alist, test, test-not, key.
  # Check weak-alist argument:
  STACK_3 = check_weakalist(STACK_3);
  # Check :TEST/:TEST-NOT arguments in STACK_2,STACK_1:
  var up_function_t up_fun = test_test_args(&STACK_1);
  # Check :KEY argument in STACK_0:
  test_key_arg();
  # Search:
  var object wal = TheMutableWeakAlist(STACK_3)->mwal_list;
  # We cannot use TheWeakAlist(wal)->wal_count here, because it can be
  # decremented by a GC happening during the loop.
  var uintL maxlen = (Lrecord_length(wal)-2)/2;
  pushSTACK(wal);
  pushSTACK(NIL);
  pushSTACK(NIL);
  # Stack layout: item, weak-alist, test, test-not, key,
  #               wal, -, -.
  var uintL i;
  for (i = 0; i < maxlen; i++) {
    var object key = TheWeakAlist(wal)->wal_data[2*i+0];
    if (!eq(key,unbound)) {
      STACK_1 = key;
      STACK_0 = TheWeakAlist(wal)->wal_data[2*i+1];
      funcall_key(STACK_(0+3),key);
      if (up_fun(&STACK_(1+3),value1)) {
        var object result = allocate_cons();
        Car(result) = STACK_1; Cdr(result) = STACK_0;
        VALUES1(result);
        skipSTACK(5+3);
        return;
      }
      wal = STACK_2;
    }
  }
  VALUES1(NIL);
  skipSTACK(5+3);
}

/* (WEAK-ALIST-RASSOC item weak-alist [:test] [:test-not] [:key])
   is equivalent to
   (RASSOC item (WEAK-ALIST-CONTENTS weak-alist) [:test] [:test-not] [:key]). */
LISPFUN(weak_alist_rassoc,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
{ # Stack layout: item, weak-alist, test, test-not, key.
  # Check weak-alist argument:
  STACK_3 = check_weakalist(STACK_3);
  # Check :TEST/:TEST-NOT arguments in STACK_2,STACK_1:
  var up_function_t up_fun = test_test_args(&STACK_1);
  # Check :KEY argument in STACK_0:
  test_key_arg();
  # Search:
  var object wal = TheMutableWeakAlist(STACK_3)->mwal_list;
  # We cannot use TheWeakAlist(wal)->wal_count here, because it can be
  # decremented by a GC happening during the loop.
  var uintL maxlen = (Lrecord_length(wal)-2)/2;
  pushSTACK(wal);
  pushSTACK(NIL);
  pushSTACK(NIL);
  # Stack layout: item, weak-alist, test, test-not, key,
  #               wal, -, -.
  var uintL i;
  for (i = 0; i < maxlen; i++) {
    var object value = TheWeakAlist(wal)->wal_data[2*i+1];
    if (!eq(value,unbound)) {
      STACK_0 = value;
      STACK_1 = TheWeakAlist(wal)->wal_data[2*i+0];
      funcall_key(STACK_(0+3),value);
      if (up_fun(&STACK_(1+3),value1)) {
        var object result = allocate_cons();
        Car(result) = STACK_1; Cdr(result) = STACK_0;
        VALUES1(result);
        skipSTACK(5+3);
        return;
      }
      wal = STACK_2;
    }
  }
  VALUES1(NIL);
  skipSTACK(5+3);
}

/* (WEAK-ALIST-VALUE item weak-alist [:test] [:test-not])
   is equivalent to
   (CDR (WEAK-ALIST-ASSOC item weak-alist [:test] [:test-not]). */
LISPFUN(weak_alist_value,seclass_default,2,0,norest,key,2,
        (kw(test),kw(test_not)) )
{ # Stack layout: item, weak-alist, test, test-not.
  # Check weak-alist argument:
  STACK_2 = check_weakalist(STACK_2);
  # Check :TEST/:TEST-NOT arguments in STACK_1,STACK_0:
  var up_function_t up_fun = test_test_args(&STACK_0);
  # Search:
  var object wal = TheMutableWeakAlist(STACK_2)->mwal_list;
  # We cannot use TheWeakAlist(wal)->wal_count here, because it can be
  # decremented by a GC happening during the loop.
  var uintL maxlen = (Lrecord_length(wal)-2)/2;
  pushSTACK(wal);
  pushSTACK(NIL);
  # Stack layout: item, weak-alist, test, test-not,
  #               wal, -.
  var uintL i;
  for (i = 0; i < maxlen; i++) {
    var object key = TheWeakAlist(wal)->wal_data[2*i+0];
    if (!eq(key,unbound)) {
      STACK_0 = TheWeakAlist(wal)->wal_data[2*i+1];
      if (up_fun(&STACK_(0+2),key)) {
        VALUES1(STACK_0);
        skipSTACK(4+2);
        return;
      }
      wal = STACK_1;
    }
  }
  VALUES1(NIL);
  skipSTACK(4+2);
}

/* (SETF (WEAK-ALIST-VALUE item weak-alist [:test] [:test-not]) value)
   replaces the value stored for key in a weak-alist. When a pair with the
   given key does not exist or has already been garbage-collected, a new pair
   is added to the alist. */
LISPFUN(set_weak_alist_value,seclass_default,3,0,norest,key,2,
        (kw(test),kw(test_not)) )
{ # Stack layout: value, item, weak-alist, test, test-not.
  # Check weak-alist argument:
  STACK_2 = check_weakalist(STACK_2);
  # Check :TEST/:TEST-NOT arguments in STACK_1,STACK_0:
  var up_function_t up_fun = test_test_args(&STACK_0);
  # Search:
  var object wal = TheMutableWeakAlist(STACK_2)->mwal_list;
  # We cannot use TheWeakAlist(wal)->wal_count here, because it can be
  # decremented by a GC happening during the loop.
  var uintL maxlen = (Lrecord_length(wal)-2)/2;
  pushSTACK(wal);
  pushSTACK(NIL);
  pushSTACK(NIL);
  # Stack layout: value, item, weak-alist, test, test-not,
  #               wal, -, -.
  var uintL i;
  for (i = 0; i < maxlen; i++) {
    var object key = TheWeakAlist(wal)->wal_data[2*i+0];
    if (!eq(key,unbound)) {
      # Store the key and old value in the STACK, to keep the entry alive
      # while we call the :TEST/:TEST-NOT function.
      STACK_1 = key;
      STACK_0 = TheWeakAlist(wal)->wal_data[2*i+1];
      if (up_fun(&STACK_(0+3),key)) {
        # Replace the pair's value.
        wal = STACK_2;
        TheWeakAlist(wal)->wal_data[2*i+1] = STACK_(4+3);
        VALUES1(STACK_(4+3));
        skipSTACK(5+3);
        return;
      }
      wal = STACK_2;
    }
  }
  # Append a new pair (item . value) to the weak alist.
  if (i < maxlen) {
    # There is room at the end of wal, from index i to maxlen-1.
    TheWeakAlist(wal)->wal_data[2*i+0] = STACK_(3+3);
    TheWeakAlist(wal)->wal_data[2*i+1] = STACK_(4+3);
    TheWeakAlist(wal)->wal_count = fixnum_inc(TheWeakAlist(wal)->wal_count,1);
    #if 0 /* weak_must_activate(wal) is independent of its contents. */
    if (eq(TheWeakAlist(wal)->wp_cdr,unbound))
      activate_weak(wal); /* add to O(all_weakpointers) if needed */
    #endif
  } else {
    var uintL count = posfixnum_to_V(TheWeakAlist(wal)->wal_count);
    if (count < maxlen) {
      # There is room in wal, but not at the end. We have to compact it first,
      # before we can append a new pair.
      var uintL j;
      for (i = 0, j = 0; j < maxlen; j++)
        if (!eq(TheWeakAlist(wal)->wal_data[2*j+0],unbound)) {
          # Copy entry j to entry i (0 <= i <= j):
          if (i < j) {
            TheWeakAlist(wal)->wal_data[2*i+0] = TheWeakAlist(wal)->wal_data[2*j+0];
            TheWeakAlist(wal)->wal_data[2*i+1] = TheWeakAlist(wal)->wal_data[2*j+1];
          }
          i++;
        }
      ASSERT(i == count);
      TheWeakAlist(wal)->wal_data[2*i+0] = STACK_(3+3);
      TheWeakAlist(wal)->wal_data[2*i+1] = STACK_(4+3);
      TheWeakAlist(wal)->wal_count = fixnum_inc(TheWeakAlist(wal)->wal_count,1);
      #if 0 /* weak_must_activate(wal) is independent of its contents. */
      if (eq(TheWeakAlist(wal)->wp_cdr,unbound))
        activate_weak(wal); /* add to O(all_weakpointers) if needed */
      #endif
    } else {
      # No more room in wal. Need to allocate a new WeakAlist object.
      var uintL old_maxlen = maxlen;
      maxlen = maxlen + maxlen/4; # augment size proportionally
      if (maxlen < count+1)
        maxlen = count+1;
      # Allocate a new wal.
      var sintB rectype = Record_type(wal);
      wal = allocate_lrecord(rectype,2+2*maxlen,lrecord_type);
      TheWeakAlist(wal)->wp_cdr = unbound; /* a GC-invariant dummy */
      var object old_wal = STACK_2;
      # Copy the old contents into it.
      var uintL j;
      for (i = 0, j = 0; j < old_maxlen; j++)
        if (!eq(TheWeakAlist(old_wal)->wal_data[2*j+0],unbound)) {
          # Copy entry j of old_wal to entry i of wal (0 <= i <= j):
          TheWeakAlist(wal)->wal_data[2*i+0] = TheWeakAlist(old_wal)->wal_data[2*j+0];
          TheWeakAlist(wal)->wal_data[2*i+1] = TheWeakAlist(old_wal)->wal_data[2*j+1];
          i++;
        }
      ASSERT(i == count);
      # Add the new pair.
      TheWeakAlist(wal)->wal_data[2*i+0] = STACK_(3+3);
      TheWeakAlist(wal)->wal_data[2*i+1] = STACK_(4+3);
      i++;
      TheWeakAlist(wal)->wal_count = fixnum(i);
      for (; i < maxlen; i++) {
        TheWeakAlist(wal)->wal_data[2*i+0] = unbound;
        TheWeakAlist(wal)->wal_data[2*i+1] = unbound;
      }
      activate_weak(wal); /* add to O(all_weakpointers) if needed */
      TheMutableWeakAlist(STACK_(2+3))->mwal_list = wal;
    }
  }
  VALUES1(STACK_(4+3));
  skipSTACK(5+3);
}

/* ========================================================================= */
