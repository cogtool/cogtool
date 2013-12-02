# Funktionen betr. Symbole für CLISP
# Bruno Haible 1990-2005

#include "lispbibl.c"

/* Error when the symbol's property list has odd length.
 fehler_sym_plist_odd(symbol);
 > symbol: Symbol */
nonreturning_function(local, fehler_sym_plist_odd, (object symbol)) {
  pushSTACK(Symbol_plist(symbol)); /* TYPE-ERROR slot DATUM */
  pushSTACK(S(plist));          /* TYPE-ERROR slot EXPECTED-TYPE*/
  pushSTACK(symbol); pushSTACK(S(get));
  fehler(type_error,GETTEXT("~S: the property list of ~S has an odd length"));
}

/* UP: find the key in the property list
 > plist_: the address of the plist
 > key: indicator
 < tail: eq(Car(*tail),key), or a pointer to an atom if not found,
         or NULL if odd length */
local inline gcv_object_t* plist_find (gcv_object_t *plist_, object key) {
  loop {
    var object plistr = *plist_;
    if (atomp(plistr)) /* not found */
      return plist_;
    if (eq(Car(plistr),key)) /* found */
      return plist_;
    plistr = Cdr(plistr);
    if (atomp(plistr)) /* odd length --> error */
      return NULL;
    plist_ = &Cdr(plistr);
  }
}

/* UP: find a property in the property list of the symbol.
 get(symbol,key)
 > symbol: a Symbol
 > key: indicator
 < value: the value of key in the property list or unbound. */
global object get (object symbol, object key) {
  var gcv_object_t* plistr_ = plist_find(&(Symbol_plist(symbol)),key);
  if (plistr_ == NULL) /* property list has odd length */
    fehler_sym_plist_odd(symbol);
  var object plistr = *plistr_;
  if (endp(plistr)) /* not found */
    return unbound;
  /* key found */
  plistr = Cdr(plistr);
  if (atomp(plistr))
    fehler_sym_plist_odd(symbol);
  return Car(plistr);
}

LISPFUNN(putd,2)
{/* (SYS::%PUTD symbol function) */
  var object symbol = check_symbol(STACK_1);
  var object fun = STACK_0;
  /* fun must be a SUBR, FSUBR, Closure or #<MACRO expander>,
     not a lambda-expression. */
  if (functionp(fun) || fsubrp(fun))
    goto ok;
  else if (macrop(fun)) /* #<MACRO expander> is ok */
    goto ok;
  else if (consp(fun) && eq(Car(fun),S(lambda))) { /* Lambda-Expression? */
    fehler_lambda_expression(S(putd),fun);
  }
  fun = check_function(fun);
 ok: /* fun is correct, store in the function slot: */
  VALUES1(popSTACK()); /* return the function argument */
  Symbol_function(popSTACK()) = fun;
}

LISPFUNN(find_subr,1)
# (SYS::%FIND-SUBR symbol)
# (defun sys::%find-subr (symbol)
#   (assert (symbolp symbol))
#   (or (get symbol 'sys::traced-definition) (symbol-function symbol))
# )
  {
    var object symbol = check_symbol(popSTACK());
    var object result = get(symbol,S(traced_definition));
    if (! boundp(result))
      result = Symbol_function(symbol);
    if (!subrp(result)) {
      pushSTACK(symbol);
      pushSTACK(S(find_subr));
      fehler(error,GETTEXT("~S: ~S is not a system function"));
    }
    VALUES1(result);
  }

LISPFUNN(proclaim_constant,2)
# (SYS::%PROCLAIM-CONSTANT symbol value) erklärt ein Symbol zu einer Konstanten
# und ihm einen Wert zu.
  {
    var object symbol = check_symbol(STACK_1);
    var object val = STACK_0; skipSTACK(2);
    if (symmacro_var_p(TheSymbol(symbol))) {
      /* HyperSpec/Body/mac_define-symbol-macro.html says that making a
         global symbol-macro special is undefined; likewise for constants. */
      pushSTACK(symbol); pushSTACK(TheSubr(subr_self)->name);
      fehler(program_error,
             GETTEXT("~S: attempting to turn ~S into a constant, but it is already a global symbol-macro."));
    }
    set_const_flag(TheSymbol(symbol)); # symbol zu einer Konstanten machen
    Symbol_value(symbol) = val; # ihren Wert setzen
    VALUES1(symbol); /* return symbol */
  }

/* (SYS::%PROCLAIM-SYMBOL-MACRO symbol)
   turns the symbol into a global symbol macro. */
LISPFUNN(proclaim_symbol_macro,1)
{
  var object symbol = check_symbol(popSTACK());
  if (special_var_p(TheSymbol(symbol))) {
    /* HyperSpec/Body/mac_define-symbol-macro.html mandates a program-error. */
    pushSTACK(symbol); pushSTACK(TheSubr(subr_self)->name);
    fehler(program_error,
           GETTEXT("~S: attempting to turn ~S into a global symbol-macro, but it is already declared SPECIAL."));
  }
  set_symmacro_flag(TheSymbol(symbol)); /* Change symbol to a symbol-macro. */
  VALUES1(symbol); /* return symbol */
}

LISPFUN(get,seclass_read,2,1,norest,nokey,0,NIL)
{ /* (GET symbol key [not-found]), CLTL p. 164 */
  var object symbol = check_symbol(STACK_2);
  var object result = get(symbol,STACK_1); # suchen
  if (! boundp(result)) { /* not found? */
    result = STACK_0; # Defaultwert ist not-found
    if (! boundp(result)) /* not supplied */
      result = NIL; # dann NIL.
  }
  VALUES1(result);
  skipSTACK(3);
}

LISPFUN(getf,seclass_read,2,1,norest,nokey,0,NIL)
{ /* (GETF place key [not-found]), CLTL p. 166 */
  var gcv_object_t *plistr_ = plist_find(&STACK_2,STACK_1);
  if (plistr_ == NULL) /* property list has odd length */
    fehler_plist_odd(STACK_2);
  var object plistr = *plistr_;
  if (endp(plistr)) { /* key not found */
    if (eq( value1 = STACK_0, unbound)) /* default value is not-found */
      value1 = NIL; /* if not supplied, then NIL. */
    mv_count=1; skipSTACK(3); return;
  }
  /* found key */
  plistr = Cdr(plistr);
  if (atomp(plistr))
    fehler_plist_odd(STACK_2);
  VALUES1(Car(plistr)); skipSTACK(3);
}

LISPFUNN(putf,3)
{ /* (setf place (SYS::%PUTF place key value)) ==
     (setf (getf place key) value)
  see places.lisp: this will return NIL if place was a CONS, i.e.,
  if the list was modified "in place" and the PLACE does not have to be set */
  var gcv_object_t *tail = plist_find(&STACK_2,STACK_1);
  if (tail == NULL) fehler_plist_odd(STACK_2);
  var object plistr = *tail;
  if (endp(plistr)) { /* key not found => extend plist with 2 conses */
    pushSTACK(allocate_cons());
    var object cons1 = allocate_cons();
    var object cons2 = popSTACK();
    Cdr(cons1) = cons2;
    if (consp(STACK_2)) { /* can modify in-place */
      Cdr(cons2) = Cdr(STACK_2);
      Car(cons2) = Car(STACK_2);
      Car(STACK_2) = STACK_1; /* key */
      Cdr(STACK_2) = cons1;
      Car(cons1) = STACK_0; /* value */
      VALUES1(NIL);
    } else { /* prepend */
      Car(cons2) = STACK_0; /* value */
      Cdr(cons2) = STACK_2; /* tail */
      Car(cons1) = STACK_1; /* key */
      VALUES1(cons1);
    }
  } else {
    plistr = Cdr(plistr);
    if (atomp(plistr)) fehler_plist_odd(STACK_2);
    Car(plistr) = STACK_0; /* value */
    VALUES1(NIL);
  }
  skipSTACK(3);
}

LISPFUNN(remf,2)
{ /* (remf place key) ==
     (multiple-value-bind (new-place removed-p) (SYS::%REMF place key)
       (when (and removed (null new-place)) (setf place new-place)) removed-p)
  see places.lisp: PLACE has to be modified only if the new value is ATOM */
  var gcv_object_t *tail = plist_find(&STACK_1,STACK_0);
  if (tail == NULL) fehler_plist_odd(STACK_1);
  var object plistr = *tail;
  if (endp(plistr)) value2 = NIL; /* key not found => not removed */
  else {
    plistr = Cdr(plistr);
    if (atomp(plistr)) fehler_plist_odd(STACK_1);
    plistr = Cdr(plistr);
    if (atomp(plistr)) *tail = plistr;
    else { /* shorten the property list by 2 elements */
      Car(*tail) = Car(plistr);
      Cdr(*tail) = Cdr(plistr);
    }
    value2 = T;
  }
  value1 = STACK_1; mv_count = 2; skipSTACK(2);
}

LISPFUNNR(get_properties,2)
{ /* (GET-PROPERTIES place keylist), CLTL p. 167 */
  var object keylist = popSTACK();
  var object plist = popSTACK();
  var object plistr = plist;
  loop {
    if (endp(plistr))
      goto notfound;
    var object item = Car(plistr);
    if (!nullp(memq(item,keylist)))
      goto found;
    plistr = Cdr(plistr);
    if (atomp(plistr))
      goto odd;
    plistr = Cdr(plistr);
  }
 found: /* key found */
  value3 = plistr; /* 3rd value = list rest */
  value1 = Car(plistr); /* 1st value = found key */
  plistr = Cdr(plistr);
  if (atomp(plistr))
    goto odd;
  value2 = Car(plistr); /* 2nd value = value for key */
  mv_count=3; return; /* 2 values */
 odd: /* property list has odd length */
  fehler_plist_odd(plist);
 notfound: /* key not found */
  VALUES3(NIL,NIL,NIL); return; /* all 3 values */
}

LISPFUNN(putplist,2)
# (SYS::%PUTPLIST symbol list) == (SETF (SYMBOL-PLIST symbol) list)
  {
    var object symbol = check_symbol(STACK_1);
    var object list = STACK_0; skipSTACK(2);
    VALUES1(Symbol_plist(symbol) = list);
  }

LISPFUNN(put,3)
{ /* (SYS::%PUT symbol key value) == (SETF (GET symbol key) value) */
  var object symbol = check_symbol(STACK_2);
  var gcv_object_t *tail = plist_find(&Symbol_plist(symbol),STACK_1);
  if (tail == NULL) /* property list has odd length */
    fehler_sym_plist_odd(symbol);
  var object plistr = *tail;
  if (endp(plistr)) { /* key not found => extend plist with 2 conses */
    pushSTACK(allocate_cons());
    var object cons1 = allocate_cons();
    var object cons2 = popSTACK();
    Car(cons2) = STACK_0; /* value */
    Cdr(cons2) = Symbol_plist(STACK_2);
    Car(cons1) = STACK_1; /* key */
    Cdr(cons1) = cons2;
    Symbol_plist(STACK_2) = cons1;
  } else {
    plistr = Cdr(plistr);
    if (atomp(plistr)) fehler_sym_plist_odd(symbol); /* odd length --> error */
    Car(plistr) = STACK_0;
  }
  VALUES1(STACK_0);
  skipSTACK(3);
}

LISPFUNN(remprop,2)
{ /* (REMPROP symbol indicator), CLTL p. 166 */
  var object symbol = check_symbol(STACK_1);
  var object key = STACK_0; skipSTACK(2);
  var gcv_object_t *tail = plist_find(&Symbol_plist(symbol),key);
  if (tail == NULL) fehler_sym_plist_odd(symbol);
  var object plistr = *tail;
  if (endp(plistr)) value1 = NIL; /* key not found */
  else { /* key found */
    plistr = Cdr(plistr);
    if (atomp(plistr)) fehler_sym_plist_odd(symbol);
    *tail = Cdr(plistr); /* shorten the property list by 2 elements */
    value1 = T;
  }
  mv_count = 1;
}

LISPFUNNR(symbol_package,1)
{ /* (SYMBOL-PACKAGE symbol), CLTL p. 170 */
  var object symbol = check_symbol(popSTACK());
  VALUES1(Symbol_package(symbol));
}

LISPFUNNR(symbol_plist,1)
{ /* (SYMBOL-PLIST symbol), CLTL p. 166 */
  var object symbol = check_symbol(popSTACK());
  VALUES1(Symbol_plist(symbol));
}

LISPFUN(symbol_name,seclass_no_se,1,0,norest,nokey,0,NIL)
{ /* (SYMBOL-NAME symbol), CLTL S. 168 */
  var object symbol = check_symbol(popSTACK());
  VALUES1(Symbol_name(symbol));
}

/* (CS-COMMON-LISP:SYMBOL-NAME symbol) */
LISPFUNNR(cs_symbol_name,1)
{ /* Return the case-inverted symbol name. */
  var object symbol = check_symbol(popSTACK());
  VALUES1(string_invertcase(Symbol_name(symbol)));
}

LISPFUNNR(keywordp,1)
{ /* (KEYWORDP object), CLTL p. 170 */
  var object obj = popSTACK();
  VALUES_IF(symbolp(obj) && keywordp(obj));
}

LISPFUN(gensym,seclass_read,0,1,norest,nokey,0,NIL)
# (GENSYM x), CLTL S. 169, CLtL2 S. 245-246
# (defun gensym (&optional (x nil s))
#   (let ((prefix "G") ; ein String
#         (counter *gensym-counter*)) ; ein Integer >=0
#     (when s
#       (cond ((stringp x) (setq prefix x))
#             ((integerp x)
#              (if (minusp x)
#                (error-of-type 'type-error
#                       :datum x :expected-type '(INTEGER 0 *)
#                       (ENGLISH "~S: index ~S is negative")
#                       'gensym x
#                )
#                (setq counter x)
#             ))
#             (t (error-of-type 'type-error
#                       :datum x :expected-type '(OR STRING INTEGER)
#                       (ENGLISH "~S: invalid argument ~S")
#                       'gensym x
#             )  )
#     ) )
#     (prog1
#       (make-symbol
#         (string-concat
#           prefix
#           #-CLISP (write-to-string counter :base 10 :radix nil)
#           #+CLISP (sys::decimal-string counter)
#       ) )
#       (unless (integerp x) (setq *gensym-counter* (1+ counter)))
# ) ) )
  {
    var object prefix = O(gensym_prefix); # "G"
    var object counter = Symbol_value(S(gensym_counter)); # *GENSYM-COUNTER*
    var object x = popSTACK(); # Argument
    if (boundp(x)) {
      # x angegeben
      if (stringp(x)) {
        prefix = x; # prefix setzen
      } elif (integerp(x)) {
        if (R_minusp(x)) {
          pushSTACK(x); # TYPE-ERROR slot DATUM
          pushSTACK(O(type_posinteger)); # TYPE-ERROR slot EXPECTED-TYPE
          pushSTACK(x);
          pushSTACK(S(gensym));
          fehler(type_error,
                 GETTEXT("~S: index ~S is negative")
                );
        }
        # x ist ein Integer >=0
        counter = x; # counter setzen
      } else {
        pushSTACK(x); # TYPE-ERROR slot DATUM
        pushSTACK(O(type_string_integer)); # TYPE-ERROR slot EXPECTED-TYPE
        pushSTACK(x);
        pushSTACK(S(gensym));
        fehler(type_error,
               GETTEXT("~S: invalid argument ~S")
              );
      }
    }
    # String zusammenbauen:
    pushSTACK(prefix); # 1. Teilstring
    pushSTACK(counter); # counter
    if (!integerp(x)) {
      if (!(integerp(counter) && !R_minusp(counter))) { # sollte Integer >= 0 sein
        var object new_value = Symbol_value(S(gensym_counter)) = Fixnum_0; # *GENSYM-COUNTER* zurücksetzen
        pushSTACK(counter);            # TYPE-ERROR slot DATUM
        pushSTACK(O(type_posinteger)); # TYPE-ERROR slot EXPECTED-TYPE
        pushSTACK(new_value); pushSTACK(counter);
        fehler(type_error,
               GETTEXT("The value of *GENSYM-COUNTER* was not a nonnegative integer. Old value ~S. New value ~S.")
              );
      }
      Symbol_value(S(gensym_counter)) = I_1_plus_I(counter); # (incf *GENSYM-COUNTER*)
    }
    funcall(L(decimal_string),1); # (sys::decimal-string counter)
    pushSTACK(value1); # 2. String
    VALUES1(make_symbol(coerce_imm_ss(string_concat(2))));
  }

