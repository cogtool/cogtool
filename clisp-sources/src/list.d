/*
 * List functions for CLISP
 * Bruno Haible 1990-2005
 * Marcus Daniels 8.4.1994
 * Sam Steingold 1999-2004
 */
#include "lispbibl.c"

/* (PROG1 (CONS STACK_1 STACK_0) skipSTACK(2))
 removes 2 objects from STACK
 can trigger GC */
local inline maygc object cons_from_stack (void)
{
  var object ret = allocate_cons();
  Cdr(ret) = popSTACK();
  Car(ret) = popSTACK();
  return ret;
}

# UP: Kopiert eine Liste
# copy_list(list)
# > list: Liste
# < ergebnis: Kopie der Liste
# can trigger GC
global maygc object copy_list (object old_list) {
  # Methode: (copy-list l) = (mapcar #'identity l), mapcar vorwärts
  if (atomp(old_list))
    return old_list;
  else { # Liste mit mindestens einem Element
    var object lauf;
    pushSTACK(old_list);
    {
      var object new_list = allocate_cons();
      lauf = STACK_0; # lauf läuft durch die alte Liste durch
      Car(new_list) = Car(lauf);
      STACK_0 = new_list;
      pushSTACK(new_list);
    }
    # Schleife: STACK_1 ist die Gesamtkopie, STACK_0 = LAST davon,
    # lauf = das entsprechende Cons der Original-Liste.
    while ( lauf=Cdr(lauf), consp(lauf) ) {
      # es kommt noch ein Cons
      pushSTACK(lauf); # lauf retten
      var object new_cons = allocate_cons(); # neues Cons allozieren
      lauf = popSTACK(); # lauf zurück
      Cdr(STACK_0) = new_cons; # und als CDR des LAST einhängen
      Car(new_cons) = Car(lauf); # CAR kopieren
      STACK_0 = new_cons; # das ist nun das neue LAST
    }
    Cdr(popSTACK()) = lauf; # selben (CDR (LAST old_list)) beibehalten
    return popSTACK();
  }
}

# UP: Dreht eine Liste konstruktiv um.
# reverse(list)
# > list: Liste (x1 ... xm)
# < ergebnis: umgedrehte Liste (xm ... x1)
# can trigger GC
global maygc object reverse (object list) {
  pushSTACK(list); pushSTACK(NIL);
  while (!endp(list)) {
    # Hier ist für r=1,...,m:
    # STACK_0 = (xr-1 ... x1), list = (xr ... xm)
    STACK_1 = Cdr(list);
    # Hier ist für r=1,...,m:
    # STACK_0 = (xr-1 ... x1), STACK_1 = (xr+1 ... xm)
    pushSTACK(Car(list));
    {
      var object new_cons = allocate_cons();
      Car(new_cons) = popSTACK(); # = xr
      Cdr(new_cons) = STACK_0; # = (xr-1 ... x1)
      STACK_0 = new_cons; # = (xr ... x1)
    }
    list = STACK_1; # list := (xr+1 ... xm)
  }
  list = popSTACK(); skipSTACK(1); return list;
}
#if 0
# andere Möglichkeit:
global object reverse (object list) {
  pushSTACK(list); pushSTACK(NIL);
  while (mconsp(STACK_1)) {
    var object new_cons = allocate_cons();
    var object old_cons = STACK_1;
    STACK_1 = Cdr(old_cons);
    Car(new_cons) = Car(old_cons);
    Cdr(new_cons) = STACK_0;
    STACK_0 = new_cons;
  }
  list = popSTACK(); skipSTACK(1); return list;
}
#endif

/* UP: get the list length and the last atom
 > obj: object
 < len: list length
 < last: the last atom */
global uintL llength1 (object list, object* last) {
  var uintL count = 0;
  while (consp(list)) {
    count++; list=Cdr(list);
  }
  if (last) *last = list;
  return count;
}

# UP: Bildet eine Liste mit genau len Elementen
# make_list(len)
# > STACK_0: Initialisierungswert für die Elemente
# > uintL len: gewünschte Listenlänge
# < ergebnis: Liste mit len Elementen
# can trigger GC
global maygc object make_list (uintL len) {
  pushSTACK(NIL);
  dotimesL(len,len, {
    # STACK_0 = bisherige Liste, STACK_1 = Initialisierungswert
    var object new_cons = allocate_cons();
    Car(new_cons) = STACK_1; Cdr(new_cons) = STACK_0;
    STACK_0 = new_cons;
  });
  return popSTACK();
}

# UP: Dreht eine Liste destruktiv um.
# nreverse(list)
# > list: Liste (x1 ... xm)
# < ergebnis: Liste (xm ... x1), EQ zur alten
global object nreverse (object list) {
  # Algorithm:
  # (lambda (L)
  #   (cond ((atom L) L)
  #         ((atom (cdr L)) L)
  #         ((atom (cddr L)) (rotatef (car L) (cadr L)) L)
  #         (t (let ((L1 (cdr L)))
  #              (do ((L3 L1 (cdr L3))
  #                   (L2 nil (rplacd L3 L2)))
  #                  ((atom (cdr L3))
  #                   (setf (cdr L) L2)
  #                   (setf (cdr L1) L3)
  #                   (rotatef (car L) (car L3))
  #              )   )
  #              L
  # ) )     )  )
  if (consp(list)) { # (atom L) -> L
    var object list3 = Cdr(list); # L3 := (cdr L)
    if (!endp(list3)) { # (atom (cdr L)) -> L
      if (!endp(Cdr(list3))) {
        var object list1 = list3; # mit L1 = L3 = (cdr L)
        var object list2 = NIL; # und L2 = NIL anfangen
        do {
          var object h = Cdr(list3); # (cdr L3) retten,
          Cdr(list3) = list2; # durch L2 ersetzen,
          list2 = list3; # L2 := altes L3
          list3 = h; # L3 := altes (cdr L3)
        } while (!endp(Cdr(list3))); # (atom (cdr L3)) -> beenden
        # L3 ist das letzte und L2 das vorletzte Listen-Cons.
        Cdr(list) = list2; # (setf (cdr L) L2)
        Cdr(list1) = list3; # (setf (cdr L1) L3)
      }
      # vertausche (car list) und (car list3):
      var object h = Car(list);
      Car(list) = Car(list3);
      Car(list3) = h;
    }
  }
  return list;
}

# UP: A0 := (nreconc A0 A1)
# nreconc(list,obj)
# > list: Liste
# > obj: Objekt
# < ergebnis: (nreconc A0 A1)
global object nreconc (object list, object obj) {
  if (!endp(list)) { # (atom L) -> L
    var object list3 = Cdr(list); # L3 := (cdr L)
    if (!endp(list3)) { # (atom (cdr L)) -> L
      if (!endp(Cdr(list3))) {
        var object list1 = list3; # mit L1 = L3 = (cdr L)
        var object list2 = NIL; # und L2 = NIL anfangen
        do {
          var object h = Cdr(list3); # (cdr L3) retten,
          Cdr(list3) = list2; # durch L2 ersetzen,
          list2 = list3; # L2 := altes L3
          list3 = h; # L3 := altes (cdr L3)
        } while (!endp(Cdr(list3))); # (atom (cdr L3)) -> beenden
        # L3 ist das letzte und L2 das vorletzte Listen-Cons.
        Cdr(list) = list2; # (setf (cdr L) L2)
        Cdr(list1) = list3; # (setf (cdr L1) L3)
      }
      # vertausche (car list) und (car list3):
      {
        var object h = Car(list);
        Car(list) = Car(list3);
        Car(list3) = h;
      }
      Cdr(list3) = obj; # (setf (cdr L3) O)
    } else {
      Cdr(list) = obj;
    }
    return list;
  } else
    return obj;
}

# UP: Bilde (delete obj (the list list) :test #'EQ)
# deleteq(list,obj)
# Entferne aus der Liste list alle Elemente, die EQ zu obj sind.
# > obj: zu streichendes Element
# > list: Liste
# < ergebnis: modifizierte Liste
global object deleteq (object list, object obj) {
  var object list1 = list;
  var object list2 = list;
  while (!atomp(list2)) {
    # Hier ist entweder list1=list2=list oder (cdr list1) = list2.
    if (eq(Car(list2),obj))
      # Streiche (car list2):
      if (eq(list2,list)) {
        # noch am Listenanfang
        list2 = list1 = list = Cdr(list2);
      } else {
        # weiter hinten in der Liste
        Cdr(list1) = list2 = Cdr(list2);
      }
    else {
      # Nichts streichen, weiterrücken:
      list1 = list2; list2 = Cdr(list2);
    }
  }
  return list;
}

# UP: Liefert (car obj), mit Typprüfung
local object car (object obj) {
  if (consp(obj))
    return Car(obj);
  else if (nullp(obj))
    return obj;
  else
    fehler_list(obj);
}

# UP: Liefert (cdr obj), mit Typprüfung
local object cdr (object obj) {
  if (consp(obj))
    return Cdr(obj);
  else if (nullp(obj))
    return obj;
  else
    fehler_list(obj);
}

LISPFUNNR(car,1)
{ /* (CAR list), CLTL p. 262 */
  VALUES1(car(popSTACK()));
}

LISPFUNNR(cdr,1)
{ /* (CDR list), CLTL p. 262 */
  VALUES1(cdr(popSTACK()));
}

LISPFUNNR(caar,1)
{ /* (CAAR list), CLTL p. 263 */
  VALUES1(car(car(popSTACK())));
}

LISPFUNNR(cadr,1)
{ /* (CADR list), CLTL p. 263 */
  VALUES1(car(cdr(popSTACK())));
}

LISPFUNNR(cdar,1)
{ /* (CDAR list), CLTL p. 263 */
  VALUES1(cdr(car(popSTACK())));
}

LISPFUNNR(cddr,1)
{ /* (CDDR list), CLTL p. 263 */
  VALUES1(cdr(cdr(popSTACK())));
}

LISPFUNNR(caaar,1)
{ /* (CAAAR list), CLTL p. 263 */
  VALUES1(car(car(car(popSTACK()))));
}

LISPFUNNR(caadr,1)
{ /* (CAADR list), CLTL p. 263 */
  VALUES1(car(car(cdr(popSTACK()))));
}

LISPFUNNR(cadar,1)
{ /* (CADAR list), CLTL p. 263 */
  VALUES1(car(cdr(car(popSTACK()))));
}

LISPFUNNR(caddr,1)
{ /* (CADDR list), CLTL p. 263 */
  VALUES1(car(cdr(cdr(popSTACK()))));
}

LISPFUNNR(cdaar,1)
{ /* (CDAAR list), CLTL p. 263 */
  VALUES1(cdr(car(car(popSTACK()))));
}

LISPFUNNR(cdadr,1)
{ /* (CDADR list), CLTL p. 263 */
  VALUES1(cdr(car(cdr(popSTACK()))));
}

LISPFUNNR(cddar,1)
{ /* (CDDAR list), CLTL p. 263 */
  VALUES1(cdr(cdr(car(popSTACK()))));
}

LISPFUNNR(cdddr,1)
{ /* (CDDDR list), CLTL p. 263 */
  VALUES1(cdr(cdr(cdr(popSTACK()))));
}

LISPFUNNR(caaaar,1)
{ /* (CAAAAR list), CLTL p. 263 */
  VALUES1(car(car(car(car(popSTACK())))));
}

LISPFUNNR(caaadr,1)
{ /* (CAAADR list), CLTL p. 263 */
  VALUES1(car(car(car(cdr(popSTACK())))));
}

LISPFUNNR(caadar,1)
{ /* (CAADAR list), CLTL p. 263 */
  VALUES1(car(car(cdr(car(popSTACK())))));
}

LISPFUNNR(caaddr,1)
{ /* (CAADDR list), CLTL p. 263 */
  VALUES1(car(car(cdr(cdr(popSTACK())))));
}

LISPFUNNR(cadaar,1)
{ /* (CADAAR list), CLTL p. 263 */
  VALUES1(car(cdr(car(car(popSTACK())))));
}

LISPFUNNR(cadadr,1)
{ /* (CADADR list), CLTL p. 263 */
  VALUES1(car(cdr(car(cdr(popSTACK())))));
}

LISPFUNNR(caddar,1)
{ /* (CADDAR list), CLTL p. 263 */
  VALUES1(car(cdr(cdr(car(popSTACK())))));
}

LISPFUNNR(cadddr,1)
{ /* (CADDDR list), CLTL p. 263 */
  VALUES1(car(cdr(cdr(cdr(popSTACK())))));
}

LISPFUNNR(cdaaar,1)
{ /* (CDAAAR list), CLTL p. 263 */
  VALUES1(cdr(car(car(car(popSTACK())))));
}

LISPFUNNR(cdaadr,1)
{ /* (CDAADR list), CLTL p. 263 */
  VALUES1(cdr(car(car(cdr(popSTACK())))));
}

LISPFUNNR(cdadar,1)
{ /* (CDADAR list), CLTL p. 263 */
  VALUES1(cdr(car(cdr(car(popSTACK())))));
}

LISPFUNNR(cdaddr,1)
{ /* (CDADDR list), CLTL p. 263 */
  VALUES1(cdr(car(cdr(cdr(popSTACK())))));
}

LISPFUNNR(cddaar,1)
{ /* (CDDAAR list), CLTL p. 263 */
  VALUES1(cdr(cdr(car(car(popSTACK())))));
}

LISPFUNNR(cddadr,1)
{ /* (CDDADR list), CLTL p. 263 */
  VALUES1(cdr(cdr(car(cdr(popSTACK())))));
}

LISPFUNNR(cdddar,1)
{ /* (CDDDAR list), CLTL p. 263 */
  VALUES1(cdr(cdr(cdr(car(popSTACK())))));
}

LISPFUNNR(cddddr,1)
{ /* (CDDDDR list), CLTL p. 263 */
  VALUES1(cdr(cdr(cdr(cdr(popSTACK())))));
}

LISPFUN(cons,seclass_no_se,2,0,norest,nokey,0,NIL)
{ /* (CONS obj1 obj2), CLTL p. 264 */
  VALUES1(cons_from_stack());
}


# Unterprogramm zum Ausführen des Tests :TEST
# up2_test(stackptr,arg1,arg2)
# > *(stackptr+1): die Testfunktion
# > arg1,arg2: Argumente
# < ergebnis: true falls der Test erfüllt ist, false sonst
# can trigger GC
local maygc bool up2_test (const gcv_object_t* stackptr, object arg1, object arg2) {
  var object fun = *(stackptr STACKop 1);
  # Special case the most frequent cases
  if (eq(fun,L(eq)))
    return eq(arg1,arg2);
  if (eq(fun,L(eql)))
    return eql(arg1,arg2);
  if (eq(fun,L(equal)))
    return equal(arg1,arg2);
  pushSTACK(arg1); pushSTACK(arg2); funcall(fun,2);
  return !nullp(value1);
}

# Unterprogramm zum Ausführen des Tests :TEST-NOT
# up2_test_not(stackptr,arg1,arg2)
# > *(stackptr+0): die Testfunktion
# > arg1,arg2: Argumente
# < ergebnis: true falls der Test erfüllt ist, false sonst
# can trigger GC
local maygc bool up2_test_not (const gcv_object_t* stackptr, object arg1, object arg2) {
  pushSTACK(arg1); pushSTACK(arg2); funcall(*(stackptr STACKop 0),2);
  return nullp(value1);
}

# UP: Überprüft die :TEST, :TEST-NOT - Argumente
# test_test2_args(stackptr)
# > stackptr: Pointer in den STACK
# > *(stackptr+1): :TEST-Argument
# > *(stackptr+0): :TEST-NOT-Argument
# < *(stackptr+1): verarbeitetes :TEST-Argument
# < *(stackptr+0): verarbeitetes :TEST-NOT-Argument
# < up2_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#       > stackptr: derselbe Pointer in den Stack, arg1, arg2: Argumente
#       < true, falls der Test erfüllt ist, false sonst.
# up2_function_t sei der Typ der Adresse einer solchen Testfunktion:
typedef maygc bool (*up2_function_t) (const gcv_object_t* stackptr,
                                      object arg1, object arg2);
local up2_function_t test_test2_args (gcv_object_t* stackptr) {
  var object test_arg = *(stackptr STACKop 1);
  if (!boundp(test_arg))
    test_arg=NIL;
  # test_arg ist das :TEST-Argument
  var object test_not_arg = *(stackptr STACKop 0);
  if (!boundp(test_not_arg))
    test_not_arg=NIL;
  # test_not_arg ist das :TEST-NOT-Argument
  if (nullp(test_not_arg)) {
    # :TEST-NOT wurde nicht angegeben
    if (nullp(test_arg))
      *(stackptr STACKop 1) = L(eql); # #'EQL als Default für :TEST
    return &up2_test;
  } else {
    # :TEST-NOT wurde angegeben
    if (nullp(test_arg))
      return &up2_test_not;
    else
      fehler_both_tests();
  }
}

# UP: Testet, ob zwei Bäume gleich sind.
# tree_equal(stackptr,up2_fun,arg1,arg2)
# > arg1,arg2: Bäume
# > stackptr: Pointer in den Stack
# > A5: Adresse einer Testfunktion, die arg1 und arg2 vergleicht und dabei auf
#       die :TEST/:TEST-NOT-Argumente in *(stackptr+1).L bzw.
#       *(stackprt+0).L zugreifen kann.
# < ergebnis: true, falls gleich, false sonst
# can trigger GC
local maygc bool tree_equal (const gcv_object_t* stackptr, up2_function_t up2_fun,
                             object arg1, object arg2) {
 start:
  if (atomp(arg1))
    if (atomp(arg2))
      # arg1 und arg2 sind beide Atome
      return up2_fun(stackptr,arg1,arg2);
    else
      return false;
  else
    if (atomp(arg2))
      return false;
    else {
      # arg1 und arg2 sind beides Conses
      check_STACK(); check_SP();
      pushSTACK(Cdr(arg1)); pushSTACK(Cdr(arg2));
      if (tree_equal(stackptr,up2_fun,Car(arg1),Car(arg2))) { # rekursiv die CARs vergleichen
        # falls gleich, tail-end-rekursiv die CDRs vergleichen
        arg2 = popSTACK(); arg1 = popSTACK(); goto start;
      } else {
        skipSTACK(2); return false;
      }
    }
}

LISPFUN(tree_equal,seclass_default,2,0,norest,key,2, (kw(test),kw(test_not)) )
{ /* (TREE-EQUAL x y :test :test-not), CLTL p. 264 */
  var gcv_object_t* stackptr = &STACK_0;
  /* check :TEST/:TEST-NOT arguments: */
  var up2_function_t up2_fun = test_test2_args(stackptr);
  VALUES_IF(tree_equal(stackptr,up2_fun,STACK_3,STACK_2));
  skipSTACK(4);
}

/* UP: check whether OBJ ends a proper list
 endp(obj)
 > obj: object
 < result: true if obj is the list end NIL,
           false if obj is a Cons.
           error otherwise */
global bool endp (object obj) {
  if (consp(obj))
    return false;
  else if (nullp(obj))
    return true;
  else
    fehler_proper_list_dotted(TheSubr(subr_self)->name,obj);
}

LISPFUNNF(endp,1)
{ /* (ENDP object), CLTL p. 264 */
  VALUES_IF(endp(popSTACK()));
}

/* Finds the length of a possibly circular or dotted list.
 list_length(list,&dotted)
 > list: an object
 < result: the length (integer >= 0, or NIL for circular lists)
 < dotted: if non-circular, the last atom, i.e., the indicator whether the list
           is dotted
 can trigger GC */
global maygc object list_length (object list, object *dottedp) {
/* (defun list-length (list)
     (do ((n 0 (+ n 2))
          (fast list (cddr fast))
          (slow list (cdr slow)))
         (nil)
       (when (endp fast) (return n))
       (when (endp (cdr fast)) (return (1+ n)))
       (when (eq (cdr fast) slow) (return nil))))
 (see CLtL p 265) */
  var object fast = list;
  var object slow = fast;
  var uintL n = 0;
  while (consp(fast)) {
    fast = Cdr(fast); n++;
    if (atomp(fast))
      break;
    if (eq(fast,slow))
      return NIL;
    fast = Cdr(fast); n++;
    slow = Cdr(slow);
  }
  pushSTACK(fast);
  var object len = UL_to_I(n);
  *dottedp = popSTACK();
  return len;
}

LISPFUNNR(list_length,1)
{ /* (LIST-LENGTH list), CLTL p. 265 */
  var object tail = NIL;
  var object len = list_length(popSTACK(),&tail);
  if (nullp(tail))
    VALUES1(len);
  else
    fehler_proper_list_dotted(S(list_length),tail);
}

LISPFUNNR(list_length_dotted,1)
{ /* traverses the list just once, otherwise equivalent to
   (defun list-length-dotted (l)
     (let ((ll (list-length l)))
       (when ll (values ll (cdr (last l)))))) */
  var object tail = NIL;
  var object len = list_length(popSTACK(),&tail);
  if (nullp(len)) VALUES1(NIL);
  else VALUES2(len,tail);
}

LISPFUNNR(list_length_proper,1)
{ /* traverses the list just once, otherwise equivalent to
   (defun list-length-proper (l)
     (if (proper-list-p l) (length l)
         (error ...))) */
  var object tail = NIL;
  var object len = list_length(STACK_0,&tail);
  if (!nullp(tail)) fehler_proper_list_dotted(TheSubr(subr_self)->name,tail);
  if (nullp(len)) fehler_proper_list_circular(TheSubr(subr_self)->name,STACK_0);
  VALUES1(len); skipSTACK(1);
}

/* proper_list_p(obj)
   returns true if obj is a proper list, i.e. a list which is neither dotted
   nor circular, i.e. a list which ends in NIL. */
global bool proper_list_p (object obj) {
  var object fast = obj;
  var object slow = fast;
  while (consp(fast)) {
    fast = Cdr(fast);
    if (atomp(fast))
      break;
    if (eq(fast,slow))
      goto no;
    fast = Cdr(fast);
    slow = Cdr(slow);
  }
  if (nullp(fast))
    return true;
  else {
   no: return false;
  }
}

/* we cannot have lists longer than 1<<32 for RAM reasons
 but we must accept arbitrary positive integers in NTH, LAST &c.
 Here we truncate large integers to ~0.
 can trigger GC */
local maygc uintL get_integer_truncate (object number) {
  /* for speed, handle the most common case first */
  if (posfixnump(number)) {
   #if (intVsize>intLsize)
    if (posfixnum_to_V(number) >= vbitm(intLsize))
      return ~(uintL)0; /* most-positive-uintL */
   #endif
    return posfixnum_to_V(number);
  }
  number = check_pos_integer(number);
  if (uint32_p(number)) return I_to_UL(number);
  return ~(uintL)0; /* most-positive-uintL */
}

LISPFUNNR(nth,2)
{ /* (NTH integer list), CLTL p. 265 */
  var uintL count = get_integer_truncate(STACK_1);
  var object list = STACK_0;
  while (count--) { list = cdr(list); } /* count CDRs */
  VALUES1(car(list));                   /* one CAR */
  skipSTACK(2);
}

LISPFUNNR(first,1)
{ /* (FIRST list), CLTL p. 266 */
  VALUES1(car(popSTACK()));
}

LISPFUNNR(second,1)
{ /* (SECOND list), CLTL p. 266 */
  VALUES1(car(cdr(popSTACK())));
}

LISPFUNNR(third,1)
{ /* (THIRD list), CLTL p. 266 */
  VALUES1(car(cdr(cdr(popSTACK()))));
}

LISPFUNNR(fourth,1)
{ /* (FOURTH list), CLTL p. 266 */
  VALUES1(car(cdr(cdr(cdr(popSTACK())))));
}

LISPFUNNR(fifth,1)
{ /* (FIFTH list), CLTL p. 266 */
  VALUES1(car(cdr(cdr(cdr(cdr(popSTACK()))))));
}

LISPFUNNR(sixth,1)
{ /* (SIXTH list), CLTL p. 266 */
  VALUES1(car(cdr(cdr(cdr(cdr(cdr(popSTACK())))))));
}

LISPFUNNR(seventh,1)
{ /* (SEVENTH list), CLTL p. 266 */
  VALUES1(car(cdr(cdr(cdr(cdr(cdr(cdr(popSTACK()))))))));
}

LISPFUNNR(eighth,1)
{ /* (EIGHTH list), CLTL p. 266 */
  VALUES1(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(popSTACK())))))))));
}

LISPFUNNR(ninth,1)
{ /* (NINTH list), CLTL p. 266 */
  VALUES1(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(popSTACK()))))))))));
}

LISPFUNNR(tenth,1)
{ /* (TENTH list), CLTL p. 266 */
  VALUES1(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(popSTACK())))))))))));
}

LISPFUNNR(rest,1)
{ /* (REST list), CLTL p. 266 */
  VALUES1(cdr(popSTACK()));
}

LISPFUNNR(nthcdr,2)
{ /* (NTHCDR integer list), CLTL p. 267 */
  var uintL count = get_integer_truncate(STACK_1);
  var object list = STACK_0;
  dotimesL(count,count, {
    if (consp(list))
      /* Walk list. */
      list = Cdr(list);
    else if (nullp(list))
      /* End of list reached. */
      break;
    else
      fehler_list(list);
  });
  VALUES1(list);
  skipSTACK(2);
}

/* (SYS::CONSES-P n object) determines whether the object is a list
   consisting of length n at least. Similar to
   (if (= n 0) t (consp (nthcdr (- n 1) object)))
   except that it is robust against dotted lists, or to
   (if (= n 0) t (and (listp object) (>= (length object) n)))
   except that it is robust against circular and dotted lists. */
LISPFUNNR(conses_p,2)
{
  var uintL count = get_integer_truncate(STACK_1);
  var object list = STACK_0;
  value1 = T;
  if (count > 0) {
    if (atomp(list))
      value1 = NIL;
    else
      for (; --count > 0;) {
        list = Cdr(list);
        if (atomp(list)) {
          value1 = NIL;
          break;
        }
      }
  }
  mv_count=1;
  skipSTACK(2);
}

/* get a replacement for the circular list
 can trigger GC */
local maygc object replace_circular_list (object list) {
  dynamic_bind(S(print_circle),T);
  pushSTACK(NIL);               /* no PLACE */
  pushSTACK(list); pushSTACK(TheSubr(subr_self)->name);
  check_value(error,GETTEXT("~S: ~S is a circular list"));
  dynamic_unbind(S(print_circle));
  return value1;
}

LISPFUN(last,seclass_read,1,1,norest,nokey,0,NIL)
{ /* (LAST list [n]), CLtL2 p. 416-417, dpANS p. 14-34
 (defun last (list &optional (n 1))
   (check-type n (integer 0 *))
   (check-type list list)
   (do ((l list (cdr l))
        (r list)
        (i 0 (+ i 1)))
       ((atom l) r)
     (when (>= i n) (pop r)))) */
  var object intarg = popSTACK();
  /* check optional integer argument: */
  var uintL count = (boundp(intarg) ? get_integer_truncate(intarg) : 1);
  var object list = check_list(popSTACK());
  # Optimierung der beiden häufigsten Fälle count=1 und count=0:
  switch (count) {
    case 0: { last_0_restart:
      var object slow = list;
      while (consp(list)) {
        list = Cdr(list);
        if (atomp(list)) break;
        if (eq(list,slow)) {
          list = check_list(replace_circular_list(list));
          goto last_0_restart;
        }
        list = Cdr(list);
        slow = Cdr(slow);
      }
    } break;
    case 1: { last_1_restart:
      var object list2;
      var object slow = list;
      if (consp(list)) {
        loop {
          /* list is a Cons. */
          list2 = Cdr(list); if (atomp(list2)) break; list = list2;
          if (eq(list,slow)) {
            list = check_list(replace_circular_list(list));
            goto last_1_restart;
          }
          list2 = Cdr(list); if (atomp(list2)) break; list = list2;
          slow = Cdr(slow);
        }
      }
    }
      break;
    default: { last_default_restart:
      var object list2 = list;
      var object slow = list;
      var uintL ii = count;
      do {
        if (atomp(list2))
          goto done;
        list2 = Cdr(list2);
      } while (--ii);
      while (consp(list2)) {
        list2 = Cdr(list2); list = Cdr(list); if (atomp(list2)) break;
        if (eq(list,slow)) {
          list = check_list(replace_circular_list(list));
          goto last_default_restart;
        }
        list2 = Cdr(list2); list = Cdr(list);
      }
       done: ;
    }
      break;
  }
  VALUES1(list);
}

# UP: Bildet eine Liste mit gegebenen Elementen.
# listof(len)
# > uintC len: gewünschte Listenlänge
# > auf STACK: len Objekte, erstes zuoberst
# < ergebnis: Liste dieser Objekte
# Erhöht STACK
# changes STACK, can trigger GC
global maygc object listof (uintC len) {
  pushSTACK(NIL); # bisherige Gesamtliste
  # die len Argumente vor diese Liste consen:
  dotimesC(len,len, {
    var object new_cons = allocate_cons();
    Cdr(new_cons) = popSTACK();
    Car(new_cons) = STACK_0;
    STACK_0 = new_cons;
  });
  return popSTACK();
}

LISPFUN(list,seclass_no_se,0,0,rest,nokey,0,NIL)
{ /* (LIST {object}), CLTL p. 267 */
  VALUES1(listof(argcount));
}

LISPFUN(liststern,seclass_no_se,1,0,rest,nokey,0,NIL)
{ /* (LIST* obj1 {object}), CLTL p. 267 */
  /* bisherige Gesamtliste bereits im Stack */
  /* die argcount restlichen Argumente vor diese Liste consen: */
  dotimesC(argcount,argcount, {
    var object new_cons = allocate_cons();
    Cdr(new_cons) = popSTACK(); /* nächstes Argument davor */
    Car(new_cons) = STACK_0;
    STACK_0 = new_cons;
  });
  VALUES1(popSTACK());
}

LISPFUN(make_list,seclass_no_se,1,0,norest,key,1, (kw(initial_element)) )
{ /* (MAKE-LIST size :initial-element), CLTL p. 268 */
  if (!boundp(STACK_0)) /* check :initial-element */
    STACK_0 = NIL; /* default :initial-element is NIL */
  VALUES1(make_list(I_to_UL(check_uint32(STACK_1))));
  skipSTACK(2);
}

LISPFUN(append,seclass_read,0,0,rest,nokey,0,NIL)
{ /* (APPEND {list}), CLTL p. 268 */
    if (argcount==0) {
      VALUES1(NIL); # keine Argumente -> NIL als Ergebnis
    } else {
      # Argumente aneinanderhängen. Dazu Schleife argcount-1 mal durchlaufen:
      dotimesC(argcount,argcount-1, {
        # STACK_0 = bisherige Gesamtliste von rechts.
        # STACK_1 := (append STACK_1 STACK_0), STACK um 1 erhöhen:
        var object list1;
        {
          var object list2 = popSTACK(); # bisherige Gesamtliste (von rechts)
          list1 = STACK_0; # vorne anzuhängendes Argument
          STACK_0 = list2; # bisherige Gesamtliste wieder stacken
        }
        # list1 muss Liste sein:
        if (atomp(list1))
          if (nullp(list1))
            ; # falls list1=NIL: (append nil x) = x, nichts tun
          else
            fehler_list(list1);
        else {
          # (append list1 STACK_0), wobei list1 ein Cons ist:
          # Kopiere list1 und halte das letzte Cons fest:
          var object lauf;
          pushSTACK(list1);
          {
            var object new_list = allocate_cons();
            lauf = STACK_0; # lauf läuft durch die alte Liste list1 durch
            Car(new_list) = Car(lauf);
            STACK_0 = new_list;
            pushSTACK(new_list);
          }
          # Schleife: STACK_1 ist die Gesamtkopie, STACK_0 = LAST davon,
          # lauf = das entsprechende Cons der Original-Liste list1.
          while ( lauf=Cdr(lauf), !endp(lauf) ) {
            # es kommt noch ein Cons
            pushSTACK(lauf); # lauf retten
            var object new_cons = allocate_cons(); # neues Cons allozieren
            lauf = popSTACK(); # lauf zurück
            Cdr(STACK_0) = new_cons; # und als CDR des LAST einhängen
            Car(new_cons) = Car(lauf); # CAR kopieren
            STACK_0 = new_cons; # das ist nun das neue LAST
          }
          # Kopie fertig. STACK_2 = bisherige Gesamtliste,
          # STACK_1 = Kopie von list1, STACK_0 = LAST davon.
          lauf = popSTACK(); # Ende der Kopie
          list1 = popSTACK(); # ganze Kopie
          /*if (!nullp(Cdr(lauf))) ????
            fehler_proper_list_dotted(TheSubr(subr_self)->name,Cdr(lauf));*/
          Cdr(lauf) = STACK_0; # bisherige Gesamtkopie einhängen
          STACK_0 = list1; # und die Kopie ist die neue Gesamtliste
        }
      });
      VALUES1(popSTACK()); # Gesamtliste als Wert
    }
}

LISPFUNNR(copy_list,1)
{ /* (COPY-LIST list), CLTL p. 268 */
  VALUES1(copy_list(check_list(popSTACK())));
}

# UP: Kopiert eine Aliste
# copy_alist(alist)
# > alist: Aliste
# < ergebnis: Kopie der Aliste
# can trigger GC
local maygc object copy_alist (object alist) {
  # Algorithm:
  # Instead of
  #   (mapcar #'(lambda (x) (if (consp x) (cons (car x) (cdr x)) x)) l)
  # the list is first copied via copy-list, then the conses among the top
  # level elements of the copy are replaced with conses with same CAR and CDR.
  alist = copy_list(alist);
  pushSTACK(alist); # Gesamtliste retten
  # alist läuft durch die Gesamtliste
  while (!endp(alist)) {
    if (mconsp(Car(alist))) {
      pushSTACK(alist); # alist retten
      var object new_cons = allocate_cons(); # neues Cons
      alist = popSTACK(); # alist zurück
      {
        var object old_cons = Car(alist);
        Car(new_cons) = Car(old_cons); Cdr(new_cons) = Cdr(old_cons);
      }
      Car(alist) = new_cons;
    }
    alist = Cdr(alist);
  }
  return popSTACK();
}

LISPFUNNR(copy_alist,1) /* (COPY-ALIST alist), CLTL p. 268 */
{ VALUES1(copy_alist(popSTACK())); }

# UP: Kopiert einen Baum.
local object copy_tree (object tree) {
  if (atomp(tree))
    return tree; # Atome unverändert zurückgeben
  else {
    check_STACK(); check_SP();
    pushSTACK(Cdr(tree)); # CDR retten
    {
      var object temp = copy_tree(Car(tree)); # den CAR rekursiv kopieren
      tree = STACK_0;
      STACK_0 = temp; # CAR-Kopie retten
      temp = copy_tree(tree); # den CDR rekursiv kopieren
      pushSTACK(temp); # CDR-Kopie retten
    }
    return cons_from_stack();
  }
}

LISPFUNNR(copy_tree,1) /* (COPY-TREE tree), CLTL p. 269 */
{ VALUES1(copy_tree(popSTACK())); }

LISPFUNNR(revappend,2)
{ /* (REVAPPEND list object), CLTL p. 269 */
  while (!endp(STACK_1)) {
    var object new_cons = allocate_cons(); # neues Cons
    Car(new_cons) = Car(STACK_1); Cdr(new_cons) = STACK_0; # (cons (car list) object)
    STACK_0 = new_cons; # das ist das neue, verlängerte object
    STACK_1 = Cdr(STACK_1); # list verkürzen
  }
  VALUES1(popSTACK());
  skipSTACK(1);
}

LISPFUN(nconc,seclass_default,0,0,rest,nokey,0,NIL) # (NCONC {list}), CLTL S. 269
  {
    if (argcount==0) {
      VALUES1(NIL); # keine Argumente -> NIL als Ergebnis
    } else {
      # Argumente aneinanderhängen. Dazu Schleife argcount-1 mal durchlaufen:
      dotimesC(argcount,argcount-1, {
        # STACK_0 = bisherige Gesamtliste von rechts.
        # STACK_1 := (nconc STACK_1 STACK_0), STACK um 1 erhöhen:
        if (matomp(STACK_1))
          if (nullp(STACK_1)) {
            STACK_1 = STACK_0; skipSTACK(1); # Gesamtliste bleibt, Argument vergessen
          } else
            fehler_list(STACK_1);
        else {
          # Gesamtliste in (cdr (last STACK_1)) einhängen:
          var object list1 = STACK_1;
          var object list2;
          loop {
            # Hier ist list1 ein Cons.
            list2 = Cdr(list1);
            if (atomp(list2))
              break;
            list1 = list2;
          }
          # list1 ist das letzte Cons des Arguments STACK_1
          Cdr(list1) = popSTACK(); # bisherige Gesamtliste einhängen
          # STACK_0 = neue Gesamtliste
        }
      });
      VALUES1(popSTACK());
    }
  }

LISPFUNN(nreconc,2)             /* (NRECONC list1 list2), CLTL S. 269 */
{
  var object list1 = check_list(STACK_1);
  var object list2 = STACK_0; skipSTACK(2);
  VALUES1(nreconc(list1,list2));
}

LISPFUNN(list_nreverse,1) # (SYS::LIST-NREVERSE list)
# wie (NREVERSE list), wenn list eine Liste ist.
  {
    VALUES1(nreverse(popSTACK()));
  }

/* check that the argument is a non-circular list and return its length
 can trigger GC */
local inline maygc uintL check_list_length (gcv_object_t *list_) {
  while(1) {
    /* Give an error if the argument is not a list. (It's stupid to allow
       dotted lists of length > 0 but to forbid dotted lists of length 0,
       but that's how ANSI CL specifies it.) */
    if (!listp(*list_)) *list_ = check_list_replacement(*list_);
    var object dotted_p;
    var object llen = list_length(*list_,&dotted_p);
    if (!nullp(llen)) return I_to_UL(llen);
    *list_ = replace_circular_list(*list_);
  }
}

LISPFUN(butlast,seclass_read,1,1,norest,nokey,0,NIL)
{ /* (BUTLAST list [integer]), CLTL S. 271 */
  var object intarg = popSTACK();
  /* check optional integer argument: */
  var uintL count = (boundp(intarg) ? get_integer_truncate(intarg) : 1);
  var uintL len = check_list_length(&STACK_0); /* list length */
  if (len<=count) {
    VALUES1(NIL); skipSTACK(1); /* length(list)<=count -> return NIL */
  } else {
    var uintL new_len = len - count; /* >0 */
    # Liefere eine Kopie der ersten new_len Conses der Liste STACK_0:
    var object new_list = make_list(new_len); # neue Liste allozieren
    # Listenelemente einzeln kopieren, bis new_list voll ist:
    var object new_lauf = new_list; # läuft durch die neue Liste
    var object old_lauf = popSTACK(); # läuft durch die alte Liste
    do {
      Car(new_lauf) = Car(old_lauf);
      old_lauf = Cdr(old_lauf); new_lauf = Cdr(new_lauf);
    } while (!atomp(new_lauf));
    VALUES1(new_list);
  }
}

LISPFUN(nbutlast,seclass_default,1,1,norest,nokey,0,NIL)
{ /* (NBUTLAST list [integer]), CLTL p. 271 */
  var object intarg = popSTACK();
  /* check optional integer argument: */
  var uintL count = (boundp(intarg) ? get_integer_truncate(intarg) : 1);
  var uintL len = check_list_length(&STACK_0); /* list length */
  if (len<=count) {
    VALUES1(NIL); skipSTACK(1); /* length(list)<=count -> return NIL */
  } else {
    var uintL new_len = len - count; # >0
    var object lauf = STACK_0; # läuft durch die Liste
    # new_len-1 mal den CDR nehmen und dann den CDR auf NIL setzen:
    dotimesL(new_len,new_len-1, { lauf = Cdr(lauf); } );
    Cdr(lauf) = NIL;
    VALUES1(popSTACK()); /* return list */
  }
}

LISPFUNNR(ldiff,2)
{ /* (LDIFF list sublist), CLTL p. 272 */
    var object sublist = popSTACK();
    # Suche, wo sublist in list beginnt:
    var uintL new_len = 0;
    var bool found_p = false;
    {
      var object listr = STACK_0;
      #ifndef X3J13_175
      until ((found_p = eql(listr,sublist)) || endp(listr)) {
        listr = Cdr(listr); new_len++;
      }
      #else
      if (!listp(listr))
        fehler_list(listr);
      until ((found_p = eql(listr,sublist)) || atomp(listr)) {
        listr = Cdr(listr); new_len++;
      }
      #endif
    }
    # Liefere eine Kopie der ersten new_len Conses der Liste STACK_0:
    var object new_list = make_list(new_len); # neue Liste allozieren
    # Listenelemente einzeln kopieren, bis new_list voll ist:
    var object new_lauf = new_list; # läuft durch die neue Liste
    var object old_lauf = popSTACK(); # läuft durch die alte Liste
    if (consp(new_lauf)) loop { # loop!
      Car(new_lauf) = Car(old_lauf);
      if (atomp(Cdr(new_lauf))) {
        if (!found_p)
          Cdr(new_lauf) = Cdr(old_lauf);
        break;
      }
      old_lauf = Cdr(old_lauf); new_lauf = Cdr(new_lauf);
    }
    VALUES1(new_list);
}

/* check_cons(obj)
 > obj: an object
 < result: a cons, either the same as obj or a replacement
 can trigger GC */
local maygc object check_cons_replacement (object obj) {
  do {
    pushSTACK(NIL);               /* no PLACE */
    pushSTACK(obj);               /* TYPE-ERROR slot DATUM */
    pushSTACK(S(cons));           /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a pair"));
    obj = value1;
  } while (!consp(obj));
  return obj;
}
local inline maygc object check_cons (object obj) {
  if (!consp(obj))
    obj = check_cons_replacement(obj);
  return obj;
}

LISPFUNN(rplaca,2)              /* (RPLACA cons object), CLTL S. 272 */
{
  var object arg1 = check_cons(STACK_1);
  var object arg2 = STACK_0;
  skipSTACK(2);
  Car(arg1) = arg2;
  VALUES1(arg1);
}

LISPFUNN(prplaca,2)             /* (SYS::%RPLACA cons object) */
{ /* like (RPLACA cons object), but return object as value */
  var object arg1 = check_cons(STACK_1);
  var object arg2 = STACK_0;
  skipSTACK(2);
  Car(arg1) = arg2;
  VALUES1(arg2);
}

LISPFUNN(rplacd,2)              /* (RPLACD cons object), CLTL S. 272 */
{
  var object arg1 = check_cons(STACK_1);
  var object arg2 = STACK_0;
  skipSTACK(2);
  Cdr(arg1) = arg2;
  VALUES1(arg1);
}

LISPFUNN(prplacd,2)             /* (SYS::%RPLACD cons object) */
{ /* like (RPLACD cons object), but return object as value */
  var object arg1 = check_cons(STACK_1);
  var object arg2 = STACK_0;
  skipSTACK(2);
  Cdr(arg1) = arg2;
  VALUES1(arg2);
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
  return !nullp(value1);
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
  return nullp(value1);
}

# Unterprogramm zum Ausführen des Tests -IF
# up_if(stackptr,x)
# > *(stackptr+1): das Testprädikat
# > x: Argument
# < ergebnis: true falls der Test erfüllt ist, false sonst
# can trigger GC
local maygc bool up_if (const gcv_object_t* stackptr, object x) {
  # nach CLTL S. 247 ein (funcall predicate x) ausführen:
  pushSTACK(x); funcall(*(stackptr STACKop 1),1);
  return !nullp(value1);
}

# Unterprogramm zum Ausführen des Tests -IF-NOT
# up_if_not(stackptr,x)
# > *(stackptr+1): das Testprädikat
# > x: Argument
# < ergebnis: true falls der Test erfüllt ist, false sonst
# can trigger GC
local maygc bool up_if_not (const gcv_object_t* stackptr, object x) {
  # nach CLTL S. 247 ein (not (funcall predicate x)) ausführen:
  pushSTACK(x); funcall(*(stackptr STACKop 1),1);
  return nullp(value1);
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

# UP: Überprüft die :TEST, :TEST-NOT - Argumente
# test_test_args()
# > stackptr:=&STACK_1 : Pointer in den STACK
# > STACK_2: :TEST-Argument
# > STACK_1: :TEST-NOT-Argument
# < STACK_2: verarbeitetes :TEST-Argument
# < STACK_1: verarbeitetes :TEST-NOT-Argument
# < up_fun: Adresse einer Testfunktion, die wie folgt spezifiziert ist:
#       > stackptr: derselbe Pointer in den Stack, *(stackptr+3) = item,
#         *(stackptr+1) = :test-Argument, *(stackptr+0) = :test-not-Argument,
#       > x: Argument
#       < true, falls der Test erfüllt ist, false sonst.
  # up_function_t sei der Typ der Adresse einer solchen Testfunktion:
typedef maygc bool (*up_function_t) (const gcv_object_t* stackptr, object x);
local up_function_t test_test_args (void) {
  var object test_arg = STACK_2;
  if (!boundp(test_arg))
    test_arg=NIL;
  # test_arg ist das :TEST-Argument
  var object test_not_arg = STACK_1;
  if (!boundp(test_not_arg))
    test_not_arg=NIL;
  # test_not_arg ist das :TEST-NOT-Argument
  if (nullp(test_not_arg)) {
    # :TEST-NOT wurde nicht angegeben
    if (nullp(test_arg))
      STACK_2 = L(eql); # #'EQL als Default für :TEST
    return &up_test;
  } else {
    # :TEST-NOT wurde angegeben
    if (nullp(test_arg))
      return &up_test_not;
    else
      fehler_both_tests();
  }
}

# UP: Ersetzt im Baum tree alle x, deren KEY der TESTFUNktion genügen,
# durch NEW. Konstruktiv.
# subst(tree,stackptr,up_fun)
# > tree: Baum
# > stackptr: *(stackptr-2) = NEW, *(stackptr-1) = KEY
# > up_fun: TESTFUN = Adresse der Testfunktion,
#       wird selbem stackptr und mit (KEY x) als Argument angesprungen.
#       Sie liefert true, falls der Test erfüllt ist, false sonst.
# < ergebnis: (evtl. neuer) Baum
# can trigger GC
local maygc object subst (object tree, gcv_object_t* stackptr, up_function_t up_fun) {
  # erst (KEY tree) berechnen und TESTFUN aufrufen:
  pushSTACK(tree); # tree retten
  funcall_key(*(stackptr STACKop -1),tree); # (KEY tree)
  if (up_fun(stackptr,value1)) { # TESTFUN aufrufen
    # Test erfüllt
    skipSTACK(1); return *(stackptr STACKop -2); # NEW als Wert
  } else
    # Test nicht erfüllt
    if (matomp(STACK_0)) {
      # Argument Atom -> unverändert lassen
      return popSTACK();
    } else {
      # Argument ist ein Cons -> SUBST rekursiv aufrufen:
      check_STACK(); check_SP();
      # rekursiv für den CDR aufrufen:
      var object new_cdr = subst(Cdr(STACK_0),stackptr,up_fun);
      pushSTACK(new_cdr); # CDR-Ergebnis retten
      # rekursiv für den CAR aufrufen:
      var object new_car = subst(Car(STACK_1),stackptr,up_fun);
      if (eq(new_car,Car(STACK_1)) && eq(STACK_0,Cdr(STACK_1))) {
        # beides unverändert
        skipSTACK(1); # CDR-Ergebnis vergessen
        return popSTACK();
      } else {
        STACK_1 = new_car; # CAR-Ergebnis retten
        return cons_from_stack();
      }
    }
}

LISPFUN(subst,seclass_default,3,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
{ /* (SUBST new old tree :test :test-not :key), CLTL p. 273 */
  test_key_arg(); /* :KEY-Argument in STACK_0 */
  var up_function_t up_fun = test_test_args(); /* :TEST/:TEST-NOT-Arguments in STACK_2,STACK_1 */
  { var object newobj = STACK_5; pushSTACK(newobj); }
  /* stack layout: new, old, tree, test, test_not, key, new. */
  VALUES1(subst(STACK_4,&STACK_2,up_fun)); /* do the substitution */
  skipSTACK(7);
}

LISPFUN(subst_if,seclass_default,3,0,norest,key,1, (kw(key)) )
{ /* (SUBST-IF new pred tree :key), CLTL p. 273 */
  test_key_arg(); /* :KEY-Argument in STACK_0 */
  { var object newobj = STACK_3; pushSTACK(newobj); }
  /* stack layout: new, pred, tree, key, new. */
  VALUES1(subst(STACK_2,&STACK_2,&up_if)); /* do the substitution */
  skipSTACK(5);
}

LISPFUN(subst_if_not,seclass_default,3,0,norest,key,1, (kw(key)) )
{ /* (SUBST-IF-NOT new pred tree :key), CLTL S. 273 */
  test_key_arg(); /* :KEY-Argument in STACK_0 */
  { var object newobj = STACK_3; pushSTACK(newobj); }
  /* stack layout: new, pred, tree, key, new. */
  VALUES1(subst(STACK_2,&STACK_2,&up_if_not)); /* do the substitution */
  skipSTACK(5);
}

# UP: Ersetzt im Baum tree alle x, deren KEY der TESTFUNktion genügen,
# durch NEW. Destruktiv.
# nsubst(tree,stackptr,up_fun)
# > tree: Baum
# > stackptr: *(stackptr-2) = NEW, *(stackptr-1) = KEY
# > up_fun: TESTFUN = Adresse der Testfunktion,
#       wird selbem stackptr und mit (KEY x) als Argument angesprungen.
#       Sie liefert true, falls der Test erfüllt ist, false sonst.
# < ergebnis: Baum
# can trigger GC
local maygc object nsubst (object tree, gcv_object_t* stackptr, up_function_t up_fun) {
  # erst (KEY tree) berechnen und TESTFUN aufrufen:
  pushSTACK(tree); # tree retten
  funcall_key(*(stackptr STACKop -1),tree); # (KEY tree)
  if (up_fun(stackptr,value1)) { # TESTFUN aufrufen
    # Test erfüllt
    skipSTACK(1); return *(stackptr STACKop -2); # NEW als Wert
  } else {
    # Test nicht erfüllt
    if (mconsp(STACK_0)) {
      # Argument ist ein Cons -> NSUBST rekursiv aufrufen:
      check_STACK(); check_SP();
      # rekursiv für den CDR aufrufen:
      {
        var object modified_cdr = nsubst(Cdr(STACK_0),stackptr,up_fun);
        Cdr(STACK_0) = modified_cdr;
      }
      # rekursiv für den CAR aufrufen:
      {
        var object modified_car = nsubst(Car(STACK_0),stackptr,up_fun);
        Car(STACK_0) = modified_car;
      }
    }
    return popSTACK(); # ursprünglicher Baum zurück
  }
}

LISPFUN(nsubst,seclass_default,3,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
{ /* (NSUBST new old tree :test :test-not :key), CLTL p. 274 */
  test_key_arg(); /* :KEY-Argument in STACK_0 */
  var up_function_t up_fun = test_test_args(); /* :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1 */
  { var object newobj = STACK_5; pushSTACK(newobj); }
  /* stack layout: new, old, tree, test, test_not, key, new. */
  VALUES1(nsubst(STACK_4,&STACK_2,up_fun)); /* do the substitution */
  skipSTACK(7);
}

LISPFUN(nsubst_if,seclass_default,3,0,norest,key,1, (kw(key)) )
{ /* (NSUBST-IF new pred tree :key), CLTL p. 274 */
  test_key_arg(); /* :KEY-Argument in STACK_0 */
  { var object newobj = STACK_3; pushSTACK(newobj); }
  /* stack layout: new, pred, tree, key, new. */
  VALUES1(nsubst(STACK_2,&STACK_2,&up_if)); /* do the substitution */
  skipSTACK(5);
}

LISPFUN(nsubst_if_not,seclass_default,3,0,norest,key,1, (kw(key)) )
{ /* (NSUBST-IF-NOT new pred tree :key), CLTL p. 274 */
  test_key_arg(); /* :KEY-Argument in STACK_0 */
  { var object newobj = STACK_3; pushSTACK(newobj); }
  /* stack layout: new, pred, tree, key, new. */
  VALUES1(nsubst(STACK_2,&STACK_2,&up_if_not)); /* do the substitution */
  skipSTACK(5);
}

/* UP: return the first list element, whose CAR passed the TESTFUNction.
 sublis_assoc(stackptr)
 > *(stackptr+3) = alist
 > stackptr: *(stackptr-1) = KEY
 > *(stackptr-3) = TESTFUN = test function, called on each list element
       (u . v) with the same stackptr and with (KEY x) and u as arguments.
       returns true, when the test passes, false otherwise.
 < return: list element (a CONS) or NIL
 can trigger GC */
local maygc object sublis_assoc (gcv_object_t* stackptr)
{
  var object alist = *(stackptr STACKop 3);
  pushSTACK(alist); /* save the list ((u . v) ...) */
  while (!endp(STACK_0)) {
    /* How to treat atoms in the list?
       a. One can ignore them.
       b. One can signal an error on them.
       c. One can signal an error only for non-NIL atoms.
       Obviously (b) is best, because it provides the best possible
       error checking. But CLtL2 and CLHS both contain a "note" that
       suggests to some people that atoms are ignored, therefore I
       assume that there is code outside which assumes this behaviour,
       and we must not signal an error on it.
       Note: To other people this note suggests that only NILs are
       ignored, and they suggest (c). This is inconsistent with the
       definition of "association list" in the CLHS glossary and with
       the general use of alists as lookup tables.
       Therefore we implement (a).
       SDS 2003-03-08: I am changing the behavior to (c) because
       it is more in line with the ASSOC behavior */
    var object head = Car(STACK_0);
    if (mconsp(head)) { /* skip atoms in the list */
      /* test whether the 2-argument test function
         *(stackptr-3) (an adress!), called on u and the
         value in *(stackptr-2), returns true: */
      var bool erg = /* 2-argument test function, called on (KEY x) and u */
        (*(up2_function_t)TheMachineCode(*(stackptr STACKop -3)))
        ( stackptr, *(stackptr STACKop -2), Car(head) );
      if (erg) /* test passed ==> return x = (u . v) = (CAR alist) */
        return Car(popSTACK());
      /* test failed */
    } else if (!nullp(head))
      fehler_list(head);
    STACK_0 = Cdr(STACK_0); /* tail recursion */
  }
  skipSTACK(1); /* forget alist */
  /* reached list end ==> return NIL */
  return NIL;
}

# UP: Ersetzt im Baum tree alle x durch ihr ALIST-Abbild (mittels ASSOC):
# x wird durch das erste v ersetzt, so dass (u . v) in ALIST vorkommt und
# (KEY x) und u der TESTFUNktion genügen. Konstruktiv.
# sublis(tree,stackptr)
# > tree: Baum
# > stackptr: *(stackptr-1) = KEY, *(stackptr+3) = ALIST,
#             *(stackptr-2) ist frei für (KEY x)
# < ergebnis: (evtl. neuer) Baum
# can trigger GC
local maygc object sublis (object tree, gcv_object_t* stackptr) {
  # erst (KEY tree) berechnen und ASSOC aufrufen:
  pushSTACK(tree); # tree retten
  funcall_key(*(stackptr STACKop -1),tree); # (KEY tree)
  *(stackptr STACKop -2) = value1; # retten für sublis_assoc
  var object assoc_erg = sublis_assoc(stackptr);
  if (consp(assoc_erg)) {
    # Test erfüllt
    skipSTACK(1); return Cdr(assoc_erg); # (CDR (ASSOC ...)) als Wert
  } else
    # Test nicht erfüllt
    if (matomp(STACK_0)) {
      # Argument Atom -> unverändert lassen
      return popSTACK();
    } else {
      # Argument ist ein Cons -> SUBLIS rekursiv aufrufen:
      check_STACK(); check_SP();
      # rekursiv für den CDR aufrufen:
      var object new_cdr = sublis(Cdr(STACK_0),stackptr);
      pushSTACK(new_cdr); # CDR-Ergebnis retten
      # rekursiv für den CAR aufrufen:
      var object new_car = sublis(Car(STACK_1),stackptr);
      if (eq(new_car,Car(STACK_1)) && eq(STACK_0,Cdr(STACK_1))) {
        # beides unverändert
        skipSTACK(1); # CDR-Ergebnis vergessen
        return popSTACK();
      } else {
        STACK_1 = new_car; # CAR-Ergebnis retten
        return cons_from_stack();
      }
    }
}

LISPFUN(sublis,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
  # (SUBLIS alist tree :test :test-not :key), CLTL S. 274
  {
    test_key_arg(); # :KEY-Argument in STACK_0
    var gcv_object_t* stackptr = &STACK_1;
    var up2_function_t up2_fun = test_test2_args(stackptr); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    # up2_fun = Testfunktion, wird mit stackptr und (KEY x) und u als
    # Argumenten angesprungen. Sie liefert true, falls der Test erfüllt ist.
    if (nullp(STACK_4)) { # shortcut: nothing to do if alist = ()
      VALUES1(STACK_3);
      skipSTACK(5);
    } else {
      pushSTACK(NIL); # Dummy
      pushSTACK(make_machine_code(up2_fun)); # Testfunktion, wegen Typinfo=machine_type GC-sicher!
      # stack layout: alist, tree, test, test_not, key, dummy, up2_fun.
      VALUES1(sublis(STACK_5,stackptr)); /* do the substitution */
      skipSTACK(7);
    }
  }

# UP: Ersetzt im Baum tree alle x durch ihr ALIST-Abbild (mittels ASSOC):
# x wird durch das erste v ersetzt, so dass (u . v) in ALIST vorkommt und
# (KEY x) und u der TESTFUNktion genügen. Destruktiv.
# nsublis(tree,stackptr)
# > tree: Baum
# > stackptr: *(stackptr-1) = KEY, *(stackptr+3) = ALIST,
#             *(stackptr-2) ist frei für (KEY x)
# < ergebnis: Baum
# can trigger GC
local maygc object nsublis (object tree, gcv_object_t* stackptr) {
  # erst (KEY tree) berechnen und ASSOC aufrufen:
  pushSTACK(tree); # tree retten
  funcall_key(*(stackptr STACKop -1),tree); # (KEY tree)
  *(stackptr STACKop -2) = value1; # retten für sublis_assoc
  var object assoc_erg = sublis_assoc(stackptr);
  if (consp(assoc_erg)) {
    # Test erfüllt
    skipSTACK(1); return Cdr(assoc_erg); # (CDR (ASSOC ...)) als Wert
  } else {
    # Test nicht erfüllt
    if (mconsp(STACK_0)) {
      # Argument ist ein Cons -> NSUBLIS rekursiv aufrufen:
      check_STACK(); check_SP();
      # rekursiv für den CDR aufrufen:
      {
        var object modified_cdr = nsublis(Cdr(STACK_0),stackptr);
        Cdr(STACK_0) = modified_cdr;
      }
      # rekursiv für den CAR aufrufen:
      {
        var object modified_car = nsublis(Car(STACK_0),stackptr);
        Car(STACK_0) = modified_car;
      }
    }
    return popSTACK(); # ursprünglicher Baum zurück
  }
}

LISPFUN(nsublis,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
  # (NSUBLIS alist tree :test :test-not :key), CLTL S. 275
  {
    test_key_arg(); # :KEY-Argument in STACK_0
    var gcv_object_t* stackptr = &STACK_1;
    var up2_function_t up2_fun = test_test2_args(stackptr); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    # up2_fun = Testfunktion, wird mit stackptr und (KEY x) und u als
    # Argumenten angesprungen. Sie liefert true, falls der Test erfüllt ist.
    if (nullp(STACK_4)) { # shortcut: nothing to do if alist = ()
      VALUES1(STACK_3);
      skipSTACK(5);
    } else {
      pushSTACK(NIL); # Dummy
      pushSTACK(make_machine_code(up2_fun)); # Testfunktion, wegen Typinfo=machine_type GC-sicher!
      # Stackaufbau: alist, tree, test, test_not, key, dummy, up2_fun.
      VALUES1(nsublis(STACK_5,stackptr)); /* do the substitution */
      skipSTACK(7);
    }
  }

# UP: find OBJ in LIS: (MEMBER OBJ LIS :TEST #'EQ)
global object memq (const object obj, const object lis) {
  var object l = lis;
  while (consp(l)) {
    if (eq(Car(l),obj)) return l;
    l = Cdr(l);
  }
  if (!nullp(l))
    fehler_proper_list_dotted(TheSubr(subr_self)->name,l);
  return NIL;
}

/* (SYS::MEMQ OBJECT LIST) == (MEMBER OBJECT LIST :TEST #'EQ) */
LISPFUNNR(memq,2) {
  var object lis = popSTACK();
  var object obj = popSTACK();
  VALUES1(memq(obj,lis));
}

# UP: Liefert den Listenrest ab dem Listenelement, das der TESTFUNktion
# genügt.
# member(list,stackptr,up_fun)
# > list: Liste
# > stackptr: *(stackptr-1) = KEY
# > up_fun: TESTFUN = Adresse der Testfunktion,
#       wird selbem stackptr und mit (KEY x) als Argument angesprungen.
#       Sie liefert true, falls der Test erfüllt ist, false sonst.
# < ergebnis: Listenrest
# can trigger GC
local maygc object member (object list, gcv_object_t* stackptr, up_function_t up_fun) {
  while (!endp(list)) {
    pushSTACK(list); # Listenrest retten
    funcall_key(*(stackptr STACKop -1),Car(list)); # (KEY x)
    {
      var bool erg = up_fun(stackptr,value1); # TESTFUN aufrufen
      list = popSTACK();
      if (erg)
        return list; # Test erfüllt -> list als Ergebnis
    }
    # Test nicht erfüllt -> (member ... (cdr list)) aufrufen:
    list = Cdr(list); # tail-end-rekursiv
  }
  return list; # NIL als Ergebnis
}

LISPFUN(member,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
  # (MEMBER item list :test :test-not :key), CLTL S. 275
  {
    test_key_arg(); # :KEY-Argument in STACK_0
    var up_function_t up_fun = test_test_args(); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    VALUES1(member(STACK_3,&STACK_1,up_fun)); /* do the search */
    skipSTACK(5);
  }

LISPFUN(member_if,seclass_default,2,0,norest,key,1, (kw(key)) )
  # (MEMBER-IF pred list :key), CLTL S. 275
  {
    test_key_arg(); # :KEY-Argument in STACK_0
    VALUES1(member(STACK_1,&STACK_1,&up_if)); /* do the search */
    skipSTACK(3);
  }

LISPFUN(member_if_not,seclass_default,2,0,norest,key,1, (kw(key)) )
  # (MEMBER-IF-NOT pred list :key), CLTL S. 275
  {
    test_key_arg(); # :KEY-Argument in STACK_0
    VALUES1(member(STACK_1,&STACK_1,&up_if_not)); /* do the search */
    skipSTACK(3);
  }

LISPFUNNR(tailp,2) # (TAILP sublist list), CLTL S. 275
  #ifndef X3J13_175
  # (defun tailp (sublist list)
  #   (do ((l list (rest l)))
  #       ((endp l) (null sublist))
  #     (when (eq l sublist) (return t))
  # ) )
  #else
  # (defun tailp (sublist list)
  #   (loop
  #     (when (eql sublist list) (return t))
  #     (when (atom list) (return nil))
  #     (setq list (cdr list))
  # ) )
  #endif
  {
    var object list = popSTACK();
    var object sublist = popSTACK();
    #ifndef X3J13_175
    while (!endp(list)) {
      if (eq(list,sublist))
        goto yes;
      list = Cdr(list);
    }
    if (nullp(sublist))
      goto yes;
    #else
    loop {
      if (eql(list,sublist))
        goto yes;
      if (atomp(list))
        break;
      list = Cdr(list);
    }
    #endif
    VALUES1(NIL); return; # NIL als Wert
   yes:
    VALUES1(T); return; # T als Wert
  }

LISPFUN(adjoin,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
  # (ADJOIN item list :test :test-not :key), CLTL S. 276
  {
    # erst Test auf (MEMBER (key item) list :test :test-not :key):
    test_key_arg(); # :KEY-Argument in STACK_0
    var up_function_t up_fun = test_test_args(); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    {
      var object item = STACK_4;
      pushSTACK(item); # item retten
      funcall_key(STACK_1,item); STACK_5 = value1; # item := (funcall key item)
    }
    # Stackaufbau: (key item), list, test, test-not, key, item
    if (nullp(member(STACK_4,&STACK_2,up_fun))) { # Suche durchführen
      # item noch nicht in list gefunden: muss consen
      var object new_cons = allocate_cons();
      Cdr(new_cons) = STACK_4; # = list
      Car(new_cons) = STACK_0; # = item
      VALUES1(new_cons);
    } else {
      VALUES1(STACK_4); # list als Wert
    }
    skipSTACK(6); return;
  }

LISPFUN(acons,seclass_no_se,3,0,norest,nokey,0,NIL)
{ /* (ACONS key val alist) = (CONS (CONS key val) alist), CLTL p. 279 */
  {
    var object new_cons = allocate_cons();
    Car(new_cons) = STACK_2; /* key */
    Cdr(new_cons) = STACK_1; /* value */
    STACK_1 = new_cons;
  }
  VALUES1(cons_from_stack());
  skipSTACK(1);
}

LISPFUN(pairlis,seclass_read,2,1,norest,nokey,0,NIL)
{ /* (PAIRLIS keys data [alist]), CLTL p. 280 */
  if (!boundp(STACK_0))
    STACK_0=NIL; /* ALIST defaults to NIL */
  pushSTACK(STACK_2);     /* keys */
  pushSTACK(STACK_(1+1)); /* data */
  loop { /* stack layout: keys, data, alist, keysr, datar. */
    if (endp(STACK_0)) /* data is over? */
      if (endp(STACK_1)) /* keys is over? */
        goto end;
      else
        goto fehler_lengths;
    else
      if (endp(STACK_1)) /* keys is over? */
        goto fehler_lengths;
      else {
        var object new_cons = allocate_cons();
        Car(new_cons) = Car(STACK_1); # nächstes key als CAR
        Cdr(new_cons) = Car(STACK_0); # nächstes data als CDR
        STACK_1 = Cdr(STACK_1); # keys verkürzen
        STACK_0 = Cdr(STACK_0); # data verkürzen
        pushSTACK(new_cons);
        new_cons = allocate_cons(); # weiteres neues Cons
        Car(new_cons) = popSTACK(); # mit (key . data) als CAR
        Cdr(new_cons) = STACK_2; # und alist als CDR
        STACK_2 = new_cons; # ergibt neues alist
      }
  }
 fehler_lengths:
  skipSTACK(3);
  {
    var object data_list = popSTACK();
    var object keys_list = popSTACK();
    pushSTACK(data_list); pushSTACK(keys_list);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: lists ~S and ~S are not of same length"));
  }
 end:
  VALUES1(STACK_2); skipSTACK(5); # alist als Wert
}

# UP: Liefert das erste Listenelement, dessen CAR der TESTFUNktion genügt.
# assoc(alist,stackptr)
# > alist: Aliste
# > stackptr: *(stackptr-1) = KEY
# > up_fun: TESTFUN = Adresse der Testfunktion, wird für alle Listenelemente
#       (u . v) mit selbem stackptr und mit (KEY u) als Argument angesprungen.
#       Sie liefert true, falls der Test erfüllt ist, false sonst.
# < ergebnis: Listenelement (ein Cons) oder NIL
# can trigger GC
local maygc object assoc (object alist, gcv_object_t* stackptr, up_function_t up_fun)
{
 start:
  if (endp(alist)) /* end of alist ==> NIL */
    return NIL;
  else {
    var object head = Car(alist);
    if (mconsp(head)) { # atomare Listenelemente überspringen
      pushSTACK(alist); # Listenrest ((u . v) ...) retten
      funcall_key(*(stackptr STACKop -1),Car(head)); # (KEY u)
      var bool erg = up_fun(stackptr,value1); # TESTFUN aufrufen
      alist = popSTACK();
      if (erg)
        # Test erfüllt -> x = (u . v) = (CAR alist) als Ergebnis
        return Car(alist);
      # Test nicht erfüllt
    } else if (!nullp(head))
      fehler_list(head);
    # tail-end-rekursiv (assoc ... (cdr alist)) aufrufen:
    alist = Cdr(alist); goto start;
  }
}

LISPFUN(assoc,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
  # (ASSOC item alist :test :test-not :key), CLTL S. 280
  {
    test_key_arg(); # :KEY-Argument in STACK_0
    var up_function_t up_fun = test_test_args(); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
    VALUES1(assoc(STACK_3,&STACK_1,up_fun)); /* do the search */
    skipSTACK(5);
  }

LISPFUN(assoc_if,seclass_default,2,0,norest,key,1, (kw(key)) )
  # (ASSOC-IF pred alist :key), CLTL S. 280
  {
    test_key_arg(); # :KEY-Argument in STACK_0
    VALUES1(assoc(STACK_1,&STACK_1,&up_if)); /* do the search */
    skipSTACK(3);
  }

LISPFUN(assoc_if_not,seclass_default,2,0,norest,key,1, (kw(key)) )
  # (ASSOC-IF-NOT pred alist :key), CLTL S. 280
  {
    test_key_arg(); # :KEY-Argument in STACK_0
    VALUES1(assoc(STACK_1,&STACK_1,&up_if_not)); /* do the search */
    skipSTACK(3);
  }

# UP: Liefert das erste Listenelement, dessen CDR der TESTFUNktion genügt.
# rassoc(alist,stackptr)
# > alist: Aliste
# > stackptr: *(stackptr-1) = KEY
# > up_fun: TESTFUN = Adresse der Testfunktion, wird für alle Listenelemente
#       (u . v) mit selbem stackptr und mit (KEY v) als Argument angesprungen.
#       Sie liefert true, falls der Test erfüllt ist, false sonst.
# < ergebnis: Listenelement (ein Cons) oder NIL
# can trigger GC
local maygc object rassoc (object alist, gcv_object_t* stackptr,
                           up_function_t up_fun)
{
 start:
  if (endp(alist)) /* end of alist ==> NIL */
    return NIL;
  else {
    var object head = Car(alist);
    if (mconsp(head)) { # atomare Listenelemente überspringen
      pushSTACK(alist); # Listenrest ((u . v) ...) retten
      funcall_key(*(stackptr STACKop -1),Cdr(head)); # (KEY v)
      var bool erg = up_fun(stackptr,value1); # TESTFUN aufrufen
      alist = popSTACK();
      if (erg)
        # Test erfüllt -> x = (u . v) = (CAR alist) als Ergebnis
        return Car(alist);
      # Test nicht erfüllt
    } else if (!nullp(head))
      fehler_list(head);
    # tail-end-rekursiv (rassoc ... (cdr alist)) aufrufen:
    alist = Cdr(alist); goto start;
  }
}

LISPFUN(rassoc,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
{ /* (RASSOC item alist :test :test-not :key), CLTL S. 281 */
  test_key_arg(); # :KEY-Argument in STACK_0
  var up_function_t up_fun = test_test_args(); # :TEST/:TEST-NOT-Argumente in STACK_2,STACK_1
  VALUES1(rassoc(STACK_3,&STACK_1,up_fun)); /* do the search */
  skipSTACK(5);
}

LISPFUN(rassoc_if,seclass_default,2,0,norest,key,1, (kw(key)) )
{ /* (RASSOC-IF pred alist :key), CLTL S. 281 */
  test_key_arg(); # :KEY-Argument in STACK_0
  VALUES1(rassoc(STACK_1,&STACK_1,&up_if)); /* do the search */
  skipSTACK(3);
}

LISPFUN(rassoc_if_not,seclass_default,2,0,norest,key,1, (kw(key)) )
{ /* (RASSOC-IF-NOT pred alist :key), CLTL S. 281 */
  test_key_arg(); # :KEY-Argument in STACK_0
  VALUES1(rassoc(STACK_1,&STACK_1,&up_if_not)); /* do the search */
  skipSTACK(3);
}

/* functions making lists sequences: */

LISPFUNN(list_upd,2)
{ /* #'(lambda (seq pointer) (cdr pointer)) */
  VALUES1(cdr(popSTACK())); skipSTACK(1);
}

LISPFUNN(list_endtest,2)
{ /* #'(lambda (seq pointer) (endp pointer)) */
  VALUES_IF(endp(STACK_0)); skipSTACK(2);
}

LISPFUNN(list_fe_init,1)
{ /* #'(lambda (seq) (revappend seq nil)) */
  pushSTACK(NIL); C_revappend();
}

LISPFUNN(list_access,2)
{ /* #'(lambda (seq pointer) (car pointer)) */
  var object pointer = check_cons(STACK_0);
  VALUES1(Car(pointer));
  skipSTACK(2);
}

LISPFUNN(list_access_set,3)
{ /* #'(lambda (seq pointer value) (rplaca pointer value)) */
  var object pointer = check_cons(STACK_1);
  var object value = STACK_0;
  Car(pointer) = value;
  VALUES1(value);
  skipSTACK(3);
}

/* UP: get the list element at the given index
 elt_up(seq,index)
 > seq
 > index
 < ergebnis: list element at this index */
local object elt_up (object seq, object index) {
  var object l = seq;
  var object n = Fixnum_0;
  loop {
    if (atomp(l))
      goto index_too_large;
    if (eq(n,index))
      break;
    l = Cdr(l);
    n = fixnum_inc(n,1);
  }
  return l;
 index_too_large:
  pushSTACK(index);             /* TYPE-ERROR slot DATUM */
  pushSTACK(NIL);
  pushSTACK(seq); pushSTACK(index); pushSTACK(TheSubr(subr_self)->name);
  {
    var object tmp;
    pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(n);
    tmp = listof(1); pushSTACK(tmp); tmp = listof(3);
    STACK_3 = tmp;              /* TYPE-ERROR slot EXPECTED-TYPE */
  }
  fehler(type_error,GETTEXT("~S: index ~S too large for ~S"));
}

LISPFUNN(list_elt,2)
{ /* (lambda (seq index)
       (do ((L seq (cdr L)) (N 0 (1+ N)))
           (nil)
         (if (atom L) (error "Zu großer Index in ELT: ~S" index))
         (if (= N index) (return (car L))))) */
  var object index = popSTACK();
  var object seq = popSTACK();
  VALUES1(Car(elt_up(seq,index)));
}

LISPFUNN(list_set_elt,3)
{ /* (lambda (seq index value)
       (do ((L seq (cdr L)) (N 0 (1+ N)))
           (nil)
         (if (atom L) (error "Zu großer Index in ELT: ~S" index))
         (if (= N index) (return (rplaca L value))))) */
  var object nthcdr = elt_up(STACK_2,STACK_1);
  VALUES1(Car(nthcdr) = popSTACK());
  skipSTACK(2);
}

LISPFUNN(list_init_start,2)
{ /* (lambda (seq index)
       (do ((L seq (cdr L)) (N 0 (1+ N)))
           ((= N index) (return L))
         (if (atom L) (error "Unzulässiger :START - Index : ~S" index)))) */
  var object index = popSTACK();
  var object seq = popSTACK();
  var object l = seq;
  var object n = Fixnum_0;
  while (!eq(n,index)) {
    if (atomp(l))
      goto index_too_large;
    l = Cdr(l);
    n = fixnum_inc(n,1);
  }
  VALUES1(l); return;
 index_too_large:
  pushSTACK(seq);
  pushSTACK(index);             /* TYPE-ERROR slot DATUM */
  {
    var object tmp;
    pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(n);
    tmp = listof(3); pushSTACK(tmp); /* TYPE-ERROR slot EXPECTED-TYPE */
  }
  pushSTACK(STACK_2);           /* seq */
  pushSTACK(STACK_2);           /* index */
  pushSTACK(S(list_init_start));
  fehler(type_error,GETTEXT("~S: start index ~S too large for ~S"));
}

LISPFUNN(list_fe_init_end,2)
{ /* (lambda (seq index)
       (if (<= 0 index)
         (do* ((L1 nil (cons (car L2) L1))
               (L2 seq (cdr L2))
               (i index (1- i)))
              ((zerop i) L1)
           (if (atom L2)
             (error "Unzulässiger :END - Index : ~S" index)))
         (error "Unzulässiger :END - Index : ~S" index))) */
  /* index is known to be an Integer >=0. */
  pushSTACK(NIL);                               /* L1 := nil */
  { var object seq = STACK_2; pushSTACK(seq); } /* L2 := seq */
  pushSTACK(Fixnum_0);                          /* i := 0 */
  loop {
    /* stack layout: seq, index, L1, L2, i */
    if (eq(STACK_0,STACK_3))    /* i=index ? */
      goto end;
    if (matomp(STACK_1))        /* (atom L2) ? */
      goto index_too_large;
    var object new_cons = allocate_cons();      /* new Cons */
    var object L2 = STACK_1; STACK_1 = Cdr(L2); /* (pop L2) */
    Car(new_cons) = Car(L2);                    /* als CAR */
    Cdr(new_cons) = STACK_2;                    /* L1 als CDR */
    STACK_2 = new_cons;                         /* L1 := new Cons */
    STACK_0 = fixnum_inc(STACK_0,1);            /* i := i+1 */
  }
 index_too_large:
  pushSTACK(STACK_3);           /* TYPE-ERROR slot DATUM */
  {
    var object tmp;
    pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(STACK_(0+3));
    tmp = listof(3); pushSTACK(tmp); /* TYPE-ERROR slot EXPECTED-TYPE */
  }
  pushSTACK(STACK_(4+2));
  pushSTACK(STACK_(3+3));
  pushSTACK(S(list_fe_init_end));
  fehler(type_error,GETTEXT("~S: end index ~S too large for ~S"));
 end:
  VALUES1(STACK_2); /* return L1 */
  skipSTACK(5);
}

