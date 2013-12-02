/*
 * Functions for records and structures in CLISP
 * Bruno Haible 1990-2005
 * Sam Steingold 1998-2004
 * German comments translated into English: Stefan Kain 2002-04-16
 */
#include "lispbibl.c"

/* ===========================================================================
 * general records:

 (SYS::%RECORD-REF record index) return the index'th entry in the record.
 (SYS::%RECORD-STORE record index value) store value as the index'th
   entry in the record and return value.
 (SYS::%RECORD-LENGTH record) return the length of the record.
*/
/* Error message
 > STACK_1: record
 > STACK_0: (bad) index
 > limit: exclusive upper bound on the index */
nonreturning_function(local, fehler_index, (uintL limit)) {
  pushSTACK(STACK_0); /* TYPE-ERROR slot DATUM */
  {
    var object tmp;
    pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(limit));
    tmp = listof(1); pushSTACK(tmp); tmp = listof(3);
    pushSTACK(tmp); /* TYPE-ERROR slot EXPECTED-TYPE */
  }
  pushSTACK(STACK_(1+2)); /* record */
  pushSTACK(STACK_(0+3)); /* index */
  pushSTACK(TheSubr(subr_self)->name); /* function name */
  fehler(type_error,GETTEXT("~S: ~S is not a valid index into ~S"));
}

/* Error message
 > STACK_0: (bad) record */
nonreturning_function(local, fehler_record, (void)) {
  pushSTACK(TheSubr(subr_self)->name); /* function name */
  fehler(error, /* type_error ?? */
         GETTEXT("~S: ~S is not a record"));
}

/* Subroutine for record access functions
 > STACK_1: record argument
 > STACK_0: index argument
 < STACK: cleared up
 < returns: the address of the referred record item */
local gcv_object_t* record_up (void) {
  /* the record must be a Closure/Structure/Stream/OtherRecord: */
  if_recordp(STACK_1, ; , { skipSTACK(1); fehler_record(); } );
  var object record = STACK_1;
  var uintL length = Record_length(record);
  var uintV index;
  if (!(posfixnump(STACK_0) && ((index = posfixnum_to_V(STACK_0)) < length)))
    /* extract and check index */
    fehler_index(length);
  skipSTACK(2); /* clear up stack */
  return &TheRecord(record)->recdata[index]; /* record element address */
}

/* (SYS::%RECORD-REF record index) return the index'th entry in the record */
LISPFUNNR(record_ref,2)
{
  VALUES1(*(record_up())); /* record element as value */
}

/* (SYS::%RECORD-STORE record index value) store value as the index'th
   entry in the record and return value. */
LISPFUNN(record_store,3)
{
  var object value = popSTACK();
  VALUES1(*(record_up()) = value); /* set record element */
}

/* (SYS::%RECORD-LENGTH record) return the length of the record. */
LISPFUNNR(record_length,1)
{
  /* the record must be a Closure/Structure/Stream/OtherRecord: */
  if_recordp(STACK_0, ; , { fehler_record(); } );
  var object record = popSTACK();
  var uintL length = Record_length(record);
  VALUES1(fixnum(length)); /* length as Fixnum */
}

/* check that the length is of type (INTEGER (0) (65536))
 > STACK_0: length
 < uintV length: checked length */
#define test_record_length(length)                                            \
  if (!(posfixnump(STACK_0)                                                   \
        && ((length = posfixnum_to_V(STACK_0)) <= (uintV)(vbitm(intWsize)-1)) \
        && (length>0)))                                                       \
    fehler_record_length()
nonreturning_function(local, fehler_record_length, (void)) {
  /* STACK_0 = length, TYPE-ERROR slot DATUM */
  pushSTACK(O(type_posint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(O(type_posint16)); /* type */
  pushSTACK(STACK_2); /* length */
  pushSTACK(TheSubr(subr_self)->name); /* function name */
  fehler(type_error,GETTEXT("~S: length ~S should be of type ~S"));
}

/* ===========================================================================
 * Structures:

 (SYS::%STRUCTURE-REF type structure index) returns for a structure of
   given Type type (a Symbol) the entry at index>=1.
 (SYS::%STRUCTURE-STORE type structure index object) stores object as
   Entry index in a structure of given Type type and returns object.
 (SYS::%MAKE-STRUCTURE type length) creates a structure with length>=1
   elements of Type type.
 (COPY-STRUCTURE structure) returns a copy of the Structure structure,
   of the same type.
 (SYS::%STRUCTURE-TYPE-P type object) checks if object is a
   structure that has the Type type, which can be recognized in
   component 0. There, an object (name_1 ... name_i-1 name_i) should
   be located with one of the names EQ to type.
*/
/* subroutine for structure-access-functions:
 > STACK_2: type-argument
 > STACK_1: structure-argument
 > STACK_0: index-argument
 < result: Address of the structure-element */
local gcv_object_t* structure_up (void) {
  /* structure must be of Type structure: */
  if (!structurep(STACK_1)) {
   fehler_bad_structure: /* STACK_2 = type, STACK_1 = structure */
    pushSTACK(STACK_1); /* TYPE-ERROR slot DATUM */
    pushSTACK(STACK_(2+1)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(STACK_(2+2));
    pushSTACK(STACK_(1+3));
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    fehler(type_error,GETTEXT("~S: ~S is not a structure of type ~S"));
  }
  var object structure = STACK_1;
  /* check if type occurs in namelist = (name_1 ... name_i-1 name_i) : */
  if (!nullp(memq(STACK_2,TheStructure(structure)->structure_types)))
    goto yes;
  /* type did not occur -> Error: */
  goto fehler_bad_structure;
 yes: { /* type did occur: */
    var uintL length = (uintL)Structure_length(structure);
    var uintV index;
    /* fetch index and check */
    if (!(posfixnump(STACK_0) && ((index = posfixnum_to_V(STACK_0)) < length)))
      fehler_index(length);
    /* address of the structure-component */
    return &TheStructure(structure)->recdata[index];
  }
}

/* (SYS::%%STRUCTURE-REF type structure index) returns for a structure of
   the given Type type (a symbol) the entry index>=1.
 #<UNBOUND> is possible. */
LISPFUNNR(pstructure_ref,3) {
  VALUES1(*(structure_up())); /* structure-element as value */
  skipSTACK(3); /* clean up stack */
}

/* (SYS::%STRUCTURE-REF type structure index) returns for a structure of
   the given Type type (a symbol) the entry index>=1. */
LISPFUNNR(structure_ref,3) {
  VALUES1(*(structure_up())); /* structure-element as value */
  if (!boundp(value1)) {
    /* could be = #<UNBOUND> , after use of SLOT-MAKUNBOUND
       or after incomplete INITIALIZE-INSTANCE */
    dynamic_bind(S(print_length),Fixnum_0); /* bind *PRINT-LENGTH* to 0 */
    pushSTACK(STACK_(1+3)); /* UNBOUND-SLOT slot INSTANCE */
    /* (clos:slot-definition-name
         (find index (clos::class-slots (find-class type))
                     :key #'clos:slot-definition-location)) */
    pushSTACK(STACK_(2+3+1)); funcall(L(find_class),1);
    pushSTACK(value1); funcall(S(class_slots),1);
    pushSTACK(STACK_(0+3+1)); pushSTACK(value1); pushSTACK(S(Kkey));
    pushSTACK(Symbol_function(S(slot_definition_location))); funcall(L(find),4);
    value1 = TheSlotDefinition(value1)->slotdef_name;
    pushSTACK(value1); /* UNBOUND-SLOT slot NAME */
    pushSTACK(STACK_(1+3+2));
    pushSTACK(value1);
    pushSTACK(S(structure_ref));
    fehler(unbound_slot,GETTEXT("~S: Slot ~S of ~S has no value"));
  }
  skipSTACK(3); /* clean up stack */
}

/* (SYS::%STRUCTURE-STORE type structure index object) stores object as
   entry index in a structure of given Type type and returns object. */
LISPFUNN(structure_store,4) {
  var object value = popSTACK();
  VALUES1(*(structure_up()) = value); /* enter structure-element */
  skipSTACK(3); /* clean up stack */
}

/* (SYS::%MAKE-STRUCTURE type length) creates a structure with length>=1
   elements of Type type. */
LISPFUNNR(make_structure,2) {
  /* check length, should be a fixnum /=0  that fits into a uintW: */
  var uintV length;
  test_record_length(length);
  skipSTACK(1);
  var object structure = allocate_structure(length);
  /* new structure, filled with NILs */
  TheStructure(structure)->structure_types = popSTACK(); /* type component */
  VALUES1(structure); /* structure as value */
}

/* check_structure_replacement(obj)
 > obj: not a structure object
 < result: a structure object, a replacement
 can trigger GC */
global maygc object check_structure_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(structure_object)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(structure_object)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  } while (!structurep(obj));
  return obj;
}

/* (COPY-STRUCTURE structure) returns a copy of the Structure structure
   of the same type. */
LISPFUNNR(copy_structure,1) {
  STACK_0 = check_structure(STACK_0);
  var uintC length = Structure_length(STACK_0);
  var object new_structure = allocate_structure(length);
  copy_mem_o(&TheStructure(new_structure)->structure_types,
             &TheStructure(popSTACK())->structure_types,length);
  VALUES1(new_structure);
}

/* (SYS::%STRUCTURE-TYPE-P type object) checks if object is a
   structure that has the Type type, which can be recognized in
   component 0. There, an object (name_1 ... name_i-1 name_i) should
   be located with one of the names EQ to type. */
LISPFUNNR(structure_type_p,2) {
  /* check object for structure: */
  if (!structurep(STACK_0)) { skipSTACK(2); goto no; }
  {
    var object namelist = TheStructure(popSTACK())->structure_types;
    var object type = popSTACK();
    /* test, if type occurs in namelist = (name_1 ... name_i-1 name_i) : */
    if (!nullp(memq(type,namelist)))
      goto yes;
  }
 no: /* type did not occur: */
  VALUES1(NIL); return; /* 1 value NIL */
 yes: /* type did occur: */
  VALUES1(T); return;
}

/* ===========================================================================
 * Closures:

 (SYS::CLOSURE-NAME closure) returns the name of a closure.
 (SYS::CLOSURE-CODEVEC closure) returns the code-vector of a compiled
   closure as an array of fixnums >=0, <256.
 (SYS::CLOSURE-CONSTS closure) returns a list of all constants of a
   compiled closure.
 (SYS::MAKE-CODE-VECTOR list) returns for a list of fixnums >=0, <256
   a simple-8bit-vector of the same length, that contains these numbers
   as bytes.
 (SYS::%MAKE-CLOSURE name codevec consts seclass) returns a closure with given
   name (a symbol), given code-vector (a simple-bit-vector) and
   further given constants.
 (SYS::MAKE-CONSTANT-INITFUNCTION value) returns a closure that, when called
   with 0 arguments, returns the given value.
 (SYS::CONSTANT-INITFUNCTION-P object) tests whether an object was returned by
   SYS::MAKE-CONSTANT-INITFUNCTION.
 (CLOS:SET-FUNCALLABLE-INSTANCE-FUNCTION closure function) redirects closure
   so that it calls the given function.
 (SYS::%COPY-GENERIC-FUNCTION venv closure) copies the closure, which must be
   a generic function with venv slot, copying in the given venv.
 (SYS::GENERIC-FUNCTION-EFFECTIVE-METHOD-FUNCTION generic-function)
   returns a function, which delivers the effective methods, so that
   (APPLY generic-function arguments)
   == (APPLY (APPLY ergebnis arguments) arguments) .
*/

/* (SYS::CLOSURE-NAME closure) returns the name of a closure. */
LISPFUNNR(closure_name,1) {
  var object closure = popSTACK();
  if (!closurep(closure)) {
    pushSTACK(closure);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    fehler(error, /* type_error ?? */
           GETTEXT("~S: ~S is not a closure"));
  }
  VALUES1(Closure_name(closure));
}

/* ((SETF SYS::CLOSURE-NAME) new-value closure) changes the name of a
   closure. */
LISPFUNN(set_closure_name,2) {
  var object closure = popSTACK();
  if (!closurep(closure)) {
    pushSTACK(closure);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    fehler(error, /* type_error ?? */
           GETTEXT("~S: ~S is not a closure"));
  }
  var object new_name = popSTACK();
  if (Closure_instancep(closure))
    TheCclosure(closure)->clos_consts[1] = new_name;
  else
    TheClosure(closure)->clos_name_or_class_version = new_name;
  VALUES1(new_name);
}

/* error, if argument is not a compiled closure */
nonreturning_function(local, fehler_cclosure, (object obj)) {
  pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name); /* function name */
  fehler(error, /* type_error ?? */
         GETTEXT("~S: This is not a compiled closure: ~S"));
}

/* (SYS::CLOSURE-CODEVEC closure) returns the code-vector of a compiled
   closure, as an array of fixnums >=0, <256. */
LISPFUNNR(closure_codevec,1) {
  var object closure = popSTACK();
  if (!(cclosurep(closure))) fehler_cclosure(closure);
  var object codevec = TheCclosure(closure)->clos_codevec;
  VALUES1(codevec);
}

/* (SYS::CLOSURE-CONSTS closure) returns a list of all constants of a
   compiled closure. */
LISPFUNNR(closure_consts,1) {
  var object closure = popSTACK();
  if (!(cclosurep(closure))) fehler_cclosure(closure);
  /* comprise elements 2,3,... to a list: */
  var uintC index = Cclosure_length(closure)-2; /* index := length */
  /* step through closure from behind and push constants onto a list: */
  pushSTACK(closure); /* closure */
  pushSTACK(NIL); /* list := () */
  while (index != 0) {
    index--; /* decrement index */
    /* put new cons in front of the list: */
    var object new_cons = allocate_cons();
    Cdr(new_cons) = popSTACK();
    Car(new_cons) = TheCclosure(STACK_0)->clos_consts[(uintP)index]; /* fetch constant */
    pushSTACK(new_cons);
  }
  VALUES1(STACK_0); skipSTACK(2); /* list as value */
}

/* (SYS::MAKE-CODE-VECTOR list) returns for a list of fixnums >=0, <256
   a simple-8bit-vector of the same length, that contains these numbers
   as bytes. */
LISPFUNNR(make_code_vector,1) {
  var object bv = allocate_bit_vector(Atype_8Bit,llength(STACK_0)); /* simple-8bit-vector */
  /* fill: */
  var object listr = popSTACK(); /* list */
  var uintB* ptr = &TheSbvector(bv)->data[0]; /* loop through the bit-vector */
  while (consp(listr)) {
    var uintV byte;
    /* list element must be a fixnum >=0, <256 : */
    if (!(posfixnump(Car(listr))
          && ((byte = posfixnum_to_V(Car(listr))) < (1<<intBsize))))
      goto bad_byte;
    /* put into the bit-vector: */
    *ptr++ = (uintB)byte;
    listr = Cdr(listr);
  }
  VALUES1(bv); return;
 bad_byte:
  pushSTACK(Car(listr)); /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_uint8)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(STACK_1);
  fehler(type_error,GETTEXT("~S is not a valid code-vector byte"));
}

/* parse the seclass object (NIL or SECLASS, see compiler.lisp)
   into a seclass_t */
local seclass_t parse_seclass (object sec, object closure)
{
  if (nullp(sec)) return seclass_foldable;
  if (!consp(sec) || !consp(Cdr(sec)) || !consp(Cdr(Cdr(sec)))) {
    pushSTACK(closure); pushSTACK(sec);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: invalid side-effect class ~S for function ~S"));
  }
  var object modifies = Car(Cdr(sec));
  return (nullp(Car(sec))
          ? (nullp(modifies) ? seclass_no_se : seclass_write)
          : (nullp(modifies) ? seclass_read : seclass_default));
}

/* (SYS::%MAKE-CLOSURE name codevec consts seclass) returns a closure
   with given name (a symbol), given code-vector (a simple-bit-vector),
   given constants, and given side-effect class. */
LISPFUNNR(make_closure,4) {
  var seclass_t seclass = parse_seclass(STACK_0,STACK_3); skipSTACK(1);
  /* codevec must be a simple-bit-vector: */
  if (!simple_bit_vector_p(Atype_8Bit,STACK_1)) {
    /* STACK_1 = codevec */
    pushSTACK(STACK_1); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(simple_bit_vector)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(STACK_(1+2));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: invalid code-vector ~S"));
  }
  /* create a new closure of length (+ 2 (length consts)) : */
  var uintL length = 2+llength(STACK_0);
  if (!(length <= (uintL)(bitm(intWsize)-1))) { /* should fit into a uintW */
    /* STACK_0 = consts */
    pushSTACK(STACK_2); /* name */
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: function ~S is too big: ~S"));
  }
  var object closure = allocate_closure(length,seclass<<4);
  TheCclosure(closure)->clos_name_or_class_version = STACK_2; /* fill name */
  TheCclosure(closure)->clos_codevec = STACK_1; /* fill codevector */
  { /* fill constants: */
    var object constsr = popSTACK();
    var gcv_object_t* ptr = &TheCclosure(closure)->clos_consts[0];
    while (consp(constsr)) {
      *ptr++ = Car(constsr); constsr = Cdr(constsr);
    }
  }
  VALUES1(closure); skipSTACK(2);
}

/* (SYS::MAKE-CONSTANT-INITFUNCTION value) returns a closure that, when called
   with 0 arguments, returns the given value. */
LISPFUNN(make_constant_initfunction,1)
{
  var object consts = listof(1);
  pushSTACK(S(constant_initfunction));
  pushSTACK(O(constant_initfunction_code));
  pushSTACK(consts);
  pushSTACK(O(seclass_no_se));
  C_make_closure();
}

/* (SYS::CONSTANT-INITFUNCTION-P object) tests whether an object was returned by
   SYS::MAKE-CONSTANT-INITFUNCTION. */
LISPFUNN(constant_initfunction_p,1)
{
  var object obj = popSTACK();
  VALUES_IF(closurep(obj)
            && eq(TheClosure(obj)->clos_name_or_class_version,S(constant_initfunction))
            && eq(TheClosure(obj)->clos_codevec,O(constant_initfunction_code)));
}

LISPFUNN(closure_set_seclass,2)
{ /* (CLOSURE-SET-SECLASS closure new-seclass)
 - for adding methods to generic functions; return the old seclass */
  var object closure = STACK_1;
  if (!cclosurep(closure)) fehler_cclosure(closure);
  var seclass_t new_seclass = parse_seclass(STACK_0,closure);
  VALUES1(seclass_object((seclass_t)Cclosure_seclass(closure)));
  Cclosure_set_seclass(closure,new_seclass);
  skipSTACK(2);
}

/* (CLOS:SET-FUNCALLABLE-INSTANCE-FUNCTION closure function) redirects closure
   so that it calls the given function. */
LISPFUNN(set_funcallable_instance_function,2)
{
  var object closure = STACK_1;
  if (!funcallable_instance_p(closure)) {
    pushSTACK(closure); pushSTACK(TheSubr(subr_self)->name);
    fehler(error, /* type_error ?? */
           GETTEXT("~S: argument is not a funcallable instance: ~S"));
  }
  var object function = STACK_0;
  if (!(subrp(function) || closurep(function) || ffunctionp(function))) {
    pushSTACK(function);    /* TYPE-ERROR slot DATUM */
    pushSTACK(S(function)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(function); pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error, GETTEXT("~S: argument is not a function: ~S"));
  }
  var object codevec;
  var object venv;
  if (cclosurep(function) && Cclosure_length(function) <= 3) {
    codevec = TheCclosure(function)->clos_codevec;
    venv = (Cclosure_length(function) >= 3
            ? (object)TheCclosure(function)->clos_venv : NIL);
  } else {
    codevec = (pushSTACK(function), funcall(S(make_trampoline),1), value1);
    venv = STACK_0;
    closure = STACK_1;
  }
  if (record_flags(TheClosure(closure)) & instflags_forwarded_B) {
    var object closure_forwarded = TheClosure(closure)->clos_name_or_class_version;
    /* We know that there is at most one indirection. */
    ASSERT(!(record_flags(TheClosure(closure_forwarded)) & instflags_forwarded_B));
    /* Replace codevec and venv in both the original and the forwarded closure. */
    TheCclosure(closure_forwarded)->clos_codevec = codevec;
    TheCclosure(closure_forwarded)->clos_venv = venv;
  }
  TheCclosure(closure)->clos_codevec = codevec;
  TheCclosure(closure)->clos_venv = venv;
  VALUES1(closure); skipSTACK(2);
}

/* check_genericlambda_function(obj)
 > obj: an object
 < result: a function with a code vector produced by %GENERIC-FUNCTION-LAMBDA,
           either the same as obj or a replacement
 can trigger GC */
local maygc object check_genericlambda_function_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(standard_generic_function)); /* slot EXPECTED-TYPE */
    pushSTACK(S(standard_generic_function)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  } while (!genericlambda_function_p(obj));
  return obj;
}
local inline maygc object check_genericlambda_function (object obj) {
  if (!genericlambda_function_p(obj))
    obj = check_genericlambda_function_replacement(obj);
  return obj;
}

/* (SYS::%COPY-GENERIC-FUNCTION venv closure) copies the closure, which must be
   a generic function with venv slot, copying in the given venv. */
LISPFUNN(copy_generic_function,2) {
  /* Note: closure's clos_consts[0] is a simple-vector #(NIL c1 ... cn) where
     c1,...,cn are constant objects, and NIL is the placeholder to be replaced
     with the passed venv. */
  var object oldclos = check_genericlambda_function(STACK_0);
  var object vector = TheCclosure(oldclos)->clos_consts[0];
  if (!(simple_vector_p(vector)
        && (Svector_length(vector) > 0)
        && nullp(TheSvector(vector)->data[0]))) {
    pushSTACK(oldclos);
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    fehler(error,
           GETTEXT("~S: This is not a prototype of a generic function: ~S"));
  }
  vector = copy_svector(vector); /* copy the vector */
  TheSvector(vector)->data[0] = STACK_1; /* put in venv */
  STACK_1 = vector;
  /* Copy the function: */
  var object newclos = allocate_cclosure_copy(STACK_0);
  oldclos = STACK_0;
  do_cclosure_copy(newclos,oldclos);
  /* Put in the copied vector with venv: */
  TheCclosure(newclos)->clos_consts[0] = STACK_1;
  VALUES1(newclos);
  skipSTACK(2);
}

/* (SYS::GENERIC-FUNCTION-EFFECTIVE-METHOD-FUNCTION generic-function)
   returns a function, which delivers the effective methods, so that
   (APPLY generic-function arguments)
   == (APPLY (APPLY ergebnis arguments) arguments) .
   is used for CALL-NEXT-METHOD; can assume that the
   generic-function has already been called, i.e. that the dispatch has
   already been installed. */
LISPFUNN(generic_function_effective_method_function,1) {
  var object oldclos = STACK_0 = check_genericlambda_function(STACK_0);
  /* allocate closure of equal length: */
  var object newclos = allocate_cclosure_copy(oldclos);
  oldclos = STACK_0;
  do_cclosure_copy(newclos,oldclos);
  STACK_0 = newclos;
  /* copy the code-vector likewise: */
  var object newcodevec = copy_sbvector(TheClosure(newclos)->clos_codevec);
  /* set the bit therein which is queried by the RETGF-instruction: */
  TheCodevec(newcodevec)->ccv_flags |= bit(3);
  newclos = popSTACK();
  TheClosure(newclos)->clos_codevec = newcodevec;
  VALUES1(newclos);
}

/* ===========================================================================
 * load-time-eval:

 (SYS::MAKE-LOAD-TIME-EVAL form) returns a load-time-eval-object that
  - if printed and read again - evaluates form. */
LISPFUN(make_load_time_eval,seclass_no_se,1,0,norest,nokey,0,NIL) {
  var object lte = allocate_loadtimeeval();
  TheLoadtimeeval(lte)->loadtimeeval_form = popSTACK();
  VALUES1(lte);
}

/* ===========================================================================
 * symbol-macro:

 (SYS::MAKE-SYMBOL-MACRO expansion) returns a symbol-macro-object
   that represents the given expansion.
 (SYS::SYMBOL-MACRO-P object) tests for symbol-macro.

 Due to their special meaning in the interpreter, symbol-macro-objects
 - like #<UNBOUND> and #<SPECDECL> - are not first class objects.
 They can be passed only as values. They cannot be assigned to
 variables, however.

 (SYMBOL-MACRO-EXPAND symbol) tests if a symbol represents a symbol-macro
 in the global environment and returns T and the expansion if true, NIL if
 false.
*/
/* (SYS::MAKE-SYMBOL-MACRO expansion) returns a symbol-macro-object,
   that represents the given expansion. */
LISPFUN(make_symbol_macro,seclass_no_se,1,0,norest,nokey,0,NIL) {
  var object sm = allocate_symbolmacro();
  TheSymbolmacro(sm)->symbolmacro_expansion = popSTACK();
  VALUES1(sm);
}

LISPFUNNF(symbol_macro_p,1)
{ /* (SYS::SYMBOL-MACRO-P object) tests for symbol-macro. */
  var object obj = popSTACK();
  VALUES_IF(symbolmacrop(obj));
}

/* (SYMBOL-MACRO-EXPAND symbol) tests if a symbol represents a symbol-macro
 and returns T and the expansion if true, NIL if false.
 (defun symbol-macro-expand (v)
   (unless (symbolp v) (error ...))
   (and (sys::global-symbol-macro-p v)
        (values t (sys::%record-ref (get v 'SYS::SYMBOLMACRO) 0)))) */
LISPFUNN(symbol_macro_expand,1) {
  var object obj = check_symbol(popSTACK());
  if (symmacro_var_p(TheSymbol(obj))) {
    /* Fetch the symbol-macro definition from the property list: */
    var object symbolmacro = get(obj,S(symbolmacro));
    if (!eq(symbolmacro,unbound)) {
      ASSERT(globalsymbolmacrop(symbolmacro));
      VALUES2(T, TheSymbolmacro(TheGlobalSymbolmacro(symbolmacro)->globalsymbolmacro_definition)->symbolmacro_expansion);
      return;
    }
    /* Huh? The symbol-macro definition got lost. */
    clear_symmacro_flag(TheSymbol(obj));
  }
  VALUES1(NIL);
}

/* ===========================================================================
 * global-symbol-macro:

 The GlobalSymbolmacro object is used to wrap a Symbolmacro object while it
 is stored on the property list, so that user code that iterates over the
 elements of a property list in interpreted mode is not trapped.

 (SYS::MAKE-GLOBAL-SYMBOL-MACRO expansion) returns a global-symbol-macro object
   containing a symbol-macro object that represents the given expansion.
 (SYS::GLOBAL-SYMBOL-MACRO-DEFINITION object) unwraps a global-symbol-macro
   object and returns the symbol-macro object inside it.
*/

/* (SYS::MAKE-GLOBAL-SYMBOL-MACRO expansion) returns a global-symbol-macro object
   containing a symbol-macro object that represents the given expansion. */
LISPFUN(make_global_symbol_macro,seclass_no_se,1,0,norest,nokey,0,NIL) {
  pushSTACK(allocate_symbolmacro());
  var object gsm = allocate_globalsymbolmacro();
  var object sm = popSTACK();
  TheSymbolmacro(sm)->symbolmacro_expansion = popSTACK();
  TheGlobalSymbolmacro(gsm)->globalsymbolmacro_definition = sm;
  VALUES1(gsm);
}

/* (SYS::GLOBAL-SYMBOL-MACRO-DEFINITION object) unwraps a global-symbol-macro
   object and returns the symbol-macro object inside it. */
LISPFUNN(global_symbol_macro_definition,1)
{
  var object obj = popSTACK();
  while (!globalsymbolmacrop(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(global_symbol_macro)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(global_symbol_macro)); pushSTACK(obj);
    pushSTACK(S(global_symbol_macro_definition)); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  }
  VALUES1(TheGlobalSymbolmacro(obj)->globalsymbolmacro_definition);
}

/* ===========================================================================
 * Macro:

 (SYS::MAKE-MACRO expander) returns a Macro object with the given expander
 function.
 (SYS::MACROP object) tests for a Macro.
 (SYS::MACRO-EXPANDER macro) returns the macro's expander function. */

/* (SYS::MAKE-MACRO expander) returns a Macro object with the given expander
 function. */
LISPFUNN(make_macro,1) {
  STACK_0 = check_function(STACK_0);
  var object m = allocate_macro();
  TheMacro(m)->macro_expander = popSTACK();
  VALUES1(m);
}

/* (SYS::MACROP object) tests for a Macro. */
LISPFUNN(macrop,1) {
  var object obj = popSTACK();
  VALUES_IF(macrop(obj));
}

/* (SYS::MACRO-EXPANDER macro) returns the macro's expander function. */
LISPFUNN(macro_expander,1) {
  var object obj = popSTACK();
  while (!macrop(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(macro)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(macro)); pushSTACK(obj);
    pushSTACK(S(macro_expander)); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  }
  VALUES1(TheMacro(obj)->macro_expander);
}

/* ===========================================================================
 * FunctionMacro:

 (SYS::MAKE-FUNCTION-MACRO function expander) returns a FunctionMacro object
 for the given function and with the given expander function.
 (SYS::FUNCTION-MACRO-P object) tests for a FunctionMacro.
 (SYS::FUNCTION-MACRO-FUNCTION macro) returns the functionmacro's function.
 (SYS::FUNCTION-MACRO-EXPANDER macro) returns the functionmacro's expander. */

/* (SYS::MAKE-FUNCTION-MACRO function expander) returns a FunctionMacro object
 for the given function and with the given expander function. */
LISPFUNN(make_function_macro,2) {
  STACK_0 = check_function(STACK_0);
  STACK_1 = check_function(STACK_1);
  var object m = allocate_functionmacro();
  TheFunctionMacro(m)->functionmacro_macro_expander = popSTACK();
  TheFunctionMacro(m)->functionmacro_function = popSTACK();
  VALUES1(m);
}

/* (SYS::FUNCTION-MACRO-P object) tests for a FunctionMacro. */
LISPFUNN(function_macro_p,1) {
  var object obj = popSTACK();
  VALUES_IF(functionmacrop(obj));
}

/* ensure that the OBJ is a FUNCTION-MACRO and return it
 can trigger GC */
local maygc object check_function_macro (object obj) {
  while (!functionmacrop(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(function_macro)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(S(function_macro)); pushSTACK(obj);
    pushSTACK(S(function_macro_expander)); /* function name */
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1;
  }
  return obj;
}

/* (SYS::FUNCTION-MACRO-FUNCTION macro)
   returns the FunctionMacro's function. */
LISPFUNN(function_macro_function,1) {
  var object obj = check_function_macro(popSTACK());
  VALUES1(TheFunctionMacro(obj)->functionmacro_function);
}

/* (SYS::FUNCTION-MACRO-EXPANDER macro)
   returns the FunctionMacro's expander. */
LISPFUNN(function_macro_expander,1) {
  var object obj = check_function_macro(popSTACK());
  VALUES1(TheFunctionMacro(obj)->functionmacro_macro_expander);
}

/* ===========================================================================
 * Finalizer: */

/* (FINALIZE object function &optional alive)
 records that function is called if object dies through GC, with
 object and poss. alive as argument. If alive dies before object dies,
 nothing will be done. */
LISPFUN(finalize,seclass_default,2,1,norest,nokey,0,NIL) {
  STACK_1 = coerce_function(STACK_1);
  if (!gcinvariant_object_p(STACK_2)) {
    var object f = allocate_finalizer();
    TheFinalizer(f)->fin_trigger = STACK_2;
    TheFinalizer(f)->fin_function = STACK_1;
    TheFinalizer(f)->fin_alive = STACK_0; /* The default #<UNBOUND> lives forever. */
    TheFinalizer(f)->fin_cdr = O(all_finalizers);
    O(all_finalizers) = f;
  }
  skipSTACK(3); VALUES1(NIL);
}

/* ===========================================================================
 * CLOS objects: */

/* (CLOS::STRUCTURE-OBJECT-P object) tests if object is a structure. */
LISPFUNNF(structure_object_p,1)
{
  var object obj = popSTACK();
  VALUES_IF(structurep(obj));
}

/* (CLOS::STD-INSTANCE-P object) tests if object is a CLOS-object
   (funcallable or not). */
LISPFUNNF(std_instance_p,1)
{
  var object obj = popSTACK();
  VALUES_IF(instancep(obj));
}

/* (CLOS::FUNCALLABLE-INSTANCE-P object) tests if object is a funcallable
   CLOS-object. */
LISPFUNNF(funcallable_instance_p,1)
{
  var object obj = popSTACK();
  VALUES_IF(funcallable_instance_p(obj));
}

/* returns (CLOS:CLASS-OF object). Especially efficient for CLOS objects.
 can trigger GC */
local inline maygc object class_of (object obj) {
  if (instancep(obj)) {
    var object obj_forwarded = obj;
    instance_un_realloc(obj_forwarded);
    if ((record_flags(TheInstance(obj_forwarded)) & instflags_beingupdated_B) == 0) {
      /* We need instance_update here because CLHS 4.3.6. says:
         "Updating such an instance occurs at an implementation-dependent time,
          but no later than the next time a slot of that instance is read or
          written." */
      instance_update(obj,obj_forwarded);
      var object cv = TheInstance(obj_forwarded)->inst_class_version;
      return TheClassVersion(cv)->cv_newest_class;
    } else {
      /* Accessing an instance which is being updated. */
      var object cv = TheInstance(obj_forwarded)->inst_class_version;
      return TheClassVersion(cv)->cv_class;
    }
  } else {
    pushSTACK(obj); C_class_of(); return value1;
  }
}

/* (CLOS::ALLOCATE-METAOBJECT-INSTANCE class-version n) returns a CLOS-instance
 of length n, with ClassVersion class-version and n-1 additional slots. It does
 this without marking the class as being instantiated and is therefore suitable
 only for classes that are never redefined, such as CLASS, SLOT-DEFINITION. */
LISPFUNN(allocate_metaobject_instance,2) {
  /* check length, should be a fixnum >0 that fits into a uintW: */
  var uintV length;
  test_record_length(length);
  skipSTACK(1);
  {
    var object cv = STACK_0;
    if (!(simple_vector_p(cv) && Svector_length(cv) == classversion_length)) {
      pushSTACK(cv);
      pushSTACK(TheSubr(subr_self)->name); /* function name */
      fehler(error,GETTEXT("~S: ~S is not a CLOS class-version"));
    }
  }
  var object instance =
    allocate_srecord(0,Rectype_Instance,length,instance_type);
  TheInstance(instance)->inst_class_version = popSTACK();
  /* fill the slots of the instance with #<UNBOUND> : */
  length--;
  if (length > 0) {
    var gcv_object_t* ptr = &TheInstance(instance)->other[0];
    dotimespV(length,length, { *ptr++ = unbound; } );
  }
  VALUES1(instance); /* instance as value */
}

/* (CLOS::ALLOCATE-STD-INSTANCE class n) returns a CLOS-instance of length n,
 with Class class and n-1 additional slots. */
LISPFUNN(allocate_std_instance,2) {
  /* check length, should be a fixnum >0 that fits into a uintW: */
  var uintV length;
  test_record_length(length);
  skipSTACK(1);
  { /* Fetch the class-version now, before any possible GC, at which the
       user could redefine the class of which we are creating an instance. */
    var object clas = STACK_0;
    if_defined_class_p(clas, ; , fehler_class(clas); );
    TheClass(clas)->instantiated = T;
    STACK_0 = TheClass(clas)->current_version;
  }
  var object instance =
    allocate_srecord(0,Rectype_Instance,length,instance_type);
  TheInstance(instance)->inst_class_version = popSTACK();
  /* fill the slots of the instance with #<UNBOUND> : */
  length--;
  if (length > 0) {
    var gcv_object_t* ptr = &TheInstance(instance)->other[0];
    dotimespV(length,length, { *ptr++ = unbound; } );
  }
  VALUES1(instance); /* instance as value */
}

/* (CLOS::ALLOCATE-FUNCALLABLE-INSTANCE class n) returns a funcallable
 CLOS-instance of length n, with Class class and n-3 additional slots. */
LISPFUNN(allocate_funcallable_instance,2) {
  /* check length, should be a fixnum >3 that fits into a uintW: */
  var uintV length;
  test_record_length(length);
  if (!(length>3)) fehler_record_length();
  skipSTACK(1);
  { /* Fetch the class-version now, before any possible GC, at which the
       user could redefine the class of which we are creating an instance. */
    var object clas = STACK_0;
    if_defined_class_p(clas, ; , fehler_class(clas); );
    TheClass(clas)->instantiated = T;
    STACK_0 = TheClass(clas)->current_version;
  }
  /* Allocate the closure. seclass is seclass_default (= *seclass-dirty*)
     because even simple generic functions can signal a NO-APPLICABLE-METHOD
     error. */
  var object instance =
    allocate_srecord(closflags_instance_B|(seclass_default<<4),
                     Rectype_Closure,length,closure_type);
  TheCclosure(instance)->clos_name_or_class_version = popSTACK();
  /* Provide a dummy codevector, in case the funcallable instance is called too
     early. */
  TheCclosure(instance)->clos_codevec = O(endless_loop_code);
  TheCclosure(instance)->clos_venv = NIL;
  /* fill the slots of the instance with #<UNBOUND> : */
  length -= 3;
  {
    var gcv_object_t* ptr = &TheCclosure(instance)->clos_consts[1];
    dotimespV(length,length, { *ptr++ = unbound; } );
  }
  VALUES1(instance); /* instance as value */
}

/* Checks that the argcount last words on the STACK form an
   "initialization argument list". */
local inline void check_initialization_argument_list (uintL argcount, object caller) {
  if (argcount%2 != 0)
    fehler_key_odd(argcount,caller);
  if (argcount > 0) {
    var gcv_object_t* argptr = STACK STACKop argcount;
    do {
      if (!symbolp(Next(argptr))) {
        pushSTACK(Next(argptr)); pushSTACK(caller);
        /* ANSI CL 3.5.1.5. wants a PROGRAM-ERROR here. */
        fehler(program_error,GETTEXT("~S: invalid initialization argument ~S"));
      }
      argptr skipSTACKop -2;
      argcount -= 2;
    } while (argcount > 0);
  }
}

local Values do_allocate_instance (object clas);
/* (CLOS::%ALLOCATE-INSTANCE class &rest initargs)
  returns an instance of the class.
  class must be an instance of <standard-class> or <structure-class>. */
LISPFUN(pallocate_instance,seclass_read,1,0,rest,nokey,0,NIL) {
  check_initialization_argument_list(argcount,S(allocate_instance));
  # No need to check the validity of the initargs, because ANSI CL says
  # "The caller of allocate-instance is expected to have already checked
  #  the initialization arguments."
  set_args_end_pointer(rest_args_pointer); /* clean up STACK */
  return_Values do_allocate_instance(popSTACK());
}
local Values do_allocate_instance (object clas) {
  /* Make a distinction between <semi-standard-class> and <structure-class> for
     allocate-instance: Is (class-current-version class) a vector, or
     is (class-names class) a cons? */
  if (matomp(TheClass(clas)->current_version)) {
    /* <semi-standard-class>. */
    if (!eq(TheClass(clas)->initialized,fixnum(6))) {
      /* Call (CLOS:FINALIZE-INHERITANCE class). */
      pushSTACK(clas); /* save clas */
      pushSTACK(clas); funcall(S(finalize_inheritance),1);
      clas = popSTACK(); /* restore clas */
      /* The class must be finalized now, otherwise FINALIZE-INHERITANCE has
         not done its job. */
      ASSERT(eq(TheClass(clas)->initialized,fixnum(6)));
    }
    /* Make a distinction between <standard-class> and
       <funcallable-standard-class>. */
    pushSTACK(clas); pushSTACK(TheClass(clas)->instance_size);
    if (nullp(TheClass(clas)->funcallablep)) {
      /* <standard-class>. */
      /* (CLOS::ALLOCATE-STD-INSTANCE class (class-instance-size class)) */
      C_allocate_std_instance();
    } else {
      /* <funcallable-standard-class>. */
      /* (CLOS::ALLOCATE-FUNCALLABLE-INSTANCE class (class-instance-size class)) */
      C_allocate_funcallable_instance();
    }
  } else {
    /* <structure-class>. */
    /* (SYS::%MAKE-STRUCTURE (class-names class) (class-instance-size class))*/
    pushSTACK(TheClass(clas)->current_version);
    pushSTACK(TheClass(clas)->instance_size);
    C_make_structure();
    /* fill the slots of the structure with #<UNBOUND> for
       INITIALIZE-INSTANCE to enter the default-values later: */
    var uintL count = Structure_length(value1)-1;
    if (count > 0) {
      var gcv_object_t* ptr = &TheStructure(value1)->recdata[1];
      dotimespL(count,count, { *ptr++ = unbound; } );
    }
  }
}

/* (CLOS:SLOT-VALUE instance slot-name)
   (CLOS::SET-SLOT-VALUE instance slot-name new-value)
   (CLOS:SLOT-BOUNDP instance slot-name)
   (CLOS:SLOT-MAKUNBOUND instance slot-name)
   (CLOS:SLOT-EXISTS-P instance slot-name)
   CLtL2 p. 855,857
 The functions CLOS::%SLOT-...-USING-CLASS are the default methods; they
 access the cell indicated by the slot's location. The functions CLOS:SLOT-...
 are the general wrapper; they dispatch to the SLOT-...-USING-CLASS generic
 function if necessary and - as an optimization - access the cell indicated
 by the slot's location if possible. */

/* Derives the address of an existing slot in an instance of a standard-
 or structure-class from a slot-location-info. */
local inline gcv_object_t* ptr_to_slot (object instance, object slotinfo,
                                        object slotname) {
  instance_un_realloc(instance); /* by this time update_instance() is done */
  if (posfixnump(slotinfo)) /* local slot, slotinfo is index */
    return &TheSrecord(instance)->recdata[posfixnum_to_V(slotinfo)];
  if (consp(slotinfo)) /* shared slot, slotinfo is (class-version . index) */
    return &TheSvector(TheClassVersion(Car(slotinfo))->cv_shared_slots)
      ->data[posfixnum_to_V(Cdr(slotinfo))];
  /* invalid location, probably bad :allocation slot option */
  pushSTACK(instance); pushSTACK(slotname);
  pushSTACK(slotinfo); pushSTACK(TheSubr(subr_self)->name);
  fehler(error,GETTEXT("~S: Invalid location ~S of slot ~S in ~S (check the :ALLOCATION slot option"));
}

/* UP: visits a slot.
 slot_using_class_up()
 > STACK_2: class
 > STACK_1: instance
 > STACK_0: slot-definition
 < result: pointer to the slot */
local gcv_object_t* slot_using_class_up (void) {
  /* The method applicability already guarantees that
     - the class is a <semi-standard-class>,
     - the slot is a <standard-effective-slot-definition>. */
  var object clas = class_of(STACK_1); /* determine (CLASS-OF instance) */
  if (!eq(clas,STACK_2)) {
    pushSTACK(STACK_1); pushSTACK(STACK_(2+1));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: invalid arguments: class argument ~S is not the class of ~S"));
  }
  var object slotinfo = TheSlotDefinition(STACK_0)->slotdef_location;
  return ptr_to_slot(STACK_1,slotinfo,STACK_0);
}

/* (CLOS::%SLOT-VALUE-USING-CLASS class instance slot) */
LISPFUNN(pslot_value_using_class,3) {
  var gcv_object_t* slot = slot_using_class_up();
  var object value = *slot;
  if (boundp(value)) {
    value1 = value;
    skipSTACK(3);
  } else {
    /* (SLOT-UNBOUND class instance slot-name) */
    STACK_0 = TheSlotDefinition(STACK_0)->slotdef_name;
    funcall(S(slot_unbound),3);
  }
  mv_count=1;
}

/* (CLOS::%SET-SLOT-VALUE-USING-CLASS new-value class instance slot) */
LISPFUNN(pset_slot_value_using_class,4) {
  var gcv_object_t* slot = slot_using_class_up();
  value1 = *slot = STACK_3;
  mv_count=1;
  skipSTACK(4);
}

/* (CLOS::%SLOT-BOUNDP-USING-CLASS class instance slot) */
LISPFUNN(pslot_boundp_using_class,3) {
  var gcv_object_t* slot = slot_using_class_up();
  VALUES_IF(boundp(*slot));
  skipSTACK(3);
}

/* (CLOS::%SLOT-MAKUNBOUND-USING-CLASS class instance slot) */
LISPFUNN(pslot_makunbound_using_class,3) {
  var gcv_object_t* slot = slot_using_class_up();
  *slot = unbound;
  VALUES1(STACK_1); /* instance as value */
  skipSTACK(3);
}

/* (CLOS:SLOT-VALUE instance slot-name) */
LISPFUNN(slot_value,2) {
  /* stack layout: instance, slot-name. */
  var object clas = class_of(STACK_1); /* determine (CLASS-OF instance) */
  var object slotinfo = /* (GETHASH slot-name (class-slot-location-table class)) */
    gethash(STACK_0,TheClass(clas)->slot_location_table,false);
  if (!eq(slotinfo,nullobj)) { /* found? */
    if (regular_instance_p(slotinfo)) {
      if (!eq(TheSlotDefinition(slotinfo)->slotdef_efm_svuc,L(pslot_value_using_class))) {
        # Call the effective method of CLOS:SLOT-VALUE-USING-CLASS.
        var object efm = TheSlotDefinition(slotinfo)->slotdef_efm_svuc;
        pushSTACK(clas); pushSTACK(STACK_(1+1)); pushSTACK(slotinfo);
        funcall(efm,3);
        goto done;
      }
      slotinfo = TheSlotDefinition(slotinfo)->slotdef_location;
    }
    var gcv_object_t* slot = ptr_to_slot(STACK_1,slotinfo,STACK_0);
    var object value = *slot;
    if (boundp(value)) {
      value1 = value;
    } else {
      /* (SLOT-UNBOUND class instance slot-name) */
      pushSTACK(clas); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
      funcall(S(slot_unbound),3);
    }
  } else {
    /* missing slot -> (SLOT-MISSING class instance slot-name 'slot-value) */
    pushSTACK(clas); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
    pushSTACK(S(slot_value));
    funcall(S(slot_missing),4);
  }
 done:
  mv_count=1;
  skipSTACK(2);
}

/* (CLOS::SET-SLOT-VALUE instance slot-name new-value) */
LISPFUNN(set_slot_value,3) {
  /* stack layout: instance, slot-name, new-value. */
  var object clas = class_of(STACK_2); /* determine (CLASS-OF instance) */
  var object slotinfo = /* (GETHASH slot-name (class-slot-location-table class)) */
    gethash(STACK_1,TheClass(clas)->slot_location_table,false);
  if (!eq(slotinfo,nullobj)) { /* found? */
    if (regular_instance_p(slotinfo)) {
      if (!eq(TheSlotDefinition(slotinfo)->slotdef_efm_ssvuc,L(pset_slot_value_using_class))) {
        # Call the effective method of (SETF CLOS:SLOT-VALUE-USING-CLASS).
        var object efm = TheSlotDefinition(slotinfo)->slotdef_efm_ssvuc;
        pushSTACK(STACK_0); pushSTACK(clas); pushSTACK(STACK_(2+2));
        pushSTACK(slotinfo);
        funcall(efm,4);
        # It must return the new-value. But anyway, just for safety
        # (don't trust user-defined methods):
        value1 = STACK_0;
        goto done;
      }
      slotinfo = TheSlotDefinition(slotinfo)->slotdef_location;
    }
    value1 = *ptr_to_slot(STACK_2,slotinfo,STACK_1) = STACK_0;
  } else {
    /* missing slot
       -> (SLOT-MISSING class instance slot-name 'setf new-value) */
    pushSTACK(clas); pushSTACK(STACK_(2+1)); pushSTACK(STACK_(1+2));
    pushSTACK(S(setf)); pushSTACK(STACK_(0+4));
    funcall(S(slot_missing),5);
    value1 = STACK_0;
  }
 done:
  mv_count=1;
  skipSTACK(3);
}

/* (CLOS:SLOT-BOUNDP instance slot-name) */
LISPFUNN(slot_boundp,2) {
  /* stack layout: instance, slot-name. */
  var object clas = class_of(STACK_1); /* determine (CLASS-OF instance) */
  var object slotinfo = /* (GETHASH slot-name (class-slot-location-table class)) */
    gethash(STACK_0,TheClass(clas)->slot_location_table,false);
  if (!eq(slotinfo,nullobj)) { /* found? */
    if (regular_instance_p(slotinfo)) {
      if (!eq(TheSlotDefinition(slotinfo)->slotdef_efm_sbuc,L(pslot_boundp_using_class))) {
        # Call the effective method of CLOS:SLOT-BOUNDP-USING-CLASS.
        var object efm = TheSlotDefinition(slotinfo)->slotdef_efm_sbuc;
        pushSTACK(clas); pushSTACK(STACK_(1+1)); pushSTACK(slotinfo);
        funcall(efm,3);
        goto done;
      }
      slotinfo = TheSlotDefinition(slotinfo)->slotdef_location;
    }
    var gcv_object_t* slot = ptr_to_slot(STACK_1,slotinfo,STACK_0);
    VALUES_IF(boundp(*slot));
  } else {
    /* missing slot -> (SLOT-MISSING class instance slot-name 'slot-boundp) */
    pushSTACK(clas); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
    pushSTACK(S(slot_boundp));
    funcall(S(slot_missing),4);
    VALUES_IF(!nullp(value1));
  }
 done:
  skipSTACK(2);
}

/* (CLOS:SLOT-MAKUNBOUND instance slot-name) */
LISPFUNN(slot_makunbound,2) {
  /* stack layout: instance, slot-name. */
  var object clas = class_of(STACK_1); /* determine (CLASS-OF instance) */
  var object slotinfo = /* (GETHASH slot-name (class-slot-location-table class)) */
    gethash(STACK_0,TheClass(clas)->slot_location_table,false);
  if (!eq(slotinfo,nullobj)) { /* found? */
    if (regular_instance_p(slotinfo)) {
      if (!eq(TheSlotDefinition(slotinfo)->slotdef_efm_smuc,L(pslot_makunbound_using_class))) {
        # Call the effective method of CLOS:SLOT-MAKUNBOUND-USING-CLASS.
        var object efm = TheSlotDefinition(slotinfo)->slotdef_efm_smuc;
        pushSTACK(clas); pushSTACK(STACK_(1+1)); pushSTACK(slotinfo);
        funcall(efm,3);
        goto done;
      }
      slotinfo = TheSlotDefinition(slotinfo)->slotdef_location;
    }
    var gcv_object_t* slot = ptr_to_slot(STACK_1,slotinfo,STACK_0);
    *slot = unbound;
  } else {
    /* missing slot -> (SLOT-MISSING class instance slot-name 'slot-makunbound) */
    pushSTACK(clas); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
    pushSTACK(S(slot_makunbound));
    funcall(S(slot_missing),4);
  }
 done:
  VALUES1(STACK_1); /* instance as value */
  skipSTACK(2);
}

/* (CLOS:SLOT-EXISTS-P instance slot-name) */
LISPFUNNR(slot_exists_p,2) {
  var object clas = class_of(STACK_1); /* determine (CLASS-OF instance) */
  var object slotinfo = /* (GETHASH slot-name (class-slot-location-table class)) */
    gethash(STACK_0,TheClass(clas)->slot_location_table,false);
  VALUES_IF(! eq(slotinfo,nullobj)); skipSTACK(2);
}

/* (CLOS:STANDARD-INSTANCE-ACCESS instance location)
   ((SETF CLOS:STANDARD-INSTANCE-ACCESS) new-value instance location)
   Unlike specified in the MOP, these work for non-updated obsolete instances
   as well. */

/* UP: visits a slot.
   slot_access_up()
   > STACK_1: instance
   > STACK_0: location
   < result: pointer to the slot */
local gcv_object_t* slot_access_up (void) {
  var object obj = STACK_1;
  /* Preparations like as in class_of. */
  if (instancep(obj)) {
    var object obj_forwarded = obj;
    instance_un_realloc(obj_forwarded);
    if ((record_flags(TheInstance(obj_forwarded)) & instflags_beingupdated_B) == 0) {
      instance_update(obj,obj_forwarded);
    }
    var object slotinfo = STACK_0;
    if (atomp(slotinfo)) {
      /* local slot, slotinfo is index */
      var uintL length = srecord_length(TheInstance(obj_forwarded));
      var uintV index;
      if (posfixnump(slotinfo) && ((index = posfixnum_to_V(slotinfo)) < length)) {
        return &((Srecord)TheInstance(obj_forwarded))->recdata[index];
      } else {
        fehler_index(length);
      }
    } else if (consp(slotinfo)) {
      /* shared slot, slotinfo is (class-version . index) */
      return &TheSvector(TheClassVersion(Car(slotinfo))->cv_shared_slots)
                ->data[posfixnum_to_V(Cdr(slotinfo))];
    } else {
      /* location already in STACK_0. */
      pushSTACK(TheSubr(subr_self)->name); /* function name */
      fehler(error,GETTEXT("~S: invalid slot location ~S"));
    }
  } else {
    /* instance already in STACK_1. TYPE-ERROR slot DATUM */
    STACK_0 = S(standard_object); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: not a CLOS instance: ~S"));
  }
}

/* (CLOS:STANDARD-INSTANCE-ACCESS instance location) */
LISPFUNNR(standard_instance_access,2) {
  var gcv_object_t* ptr = slot_access_up();
  VALUES1(*ptr); skipSTACK(2);
}

/* ((SETF CLOS:STANDARD-INSTANCE-ACCESS) new-value instance location) */
LISPFUNN(set_standard_instance_access,3) {
  var gcv_object_t* ptr = slot_access_up();
  VALUES1(*ptr = STACK_2); skipSTACK(3);
}

/* (SYS::%UNBOUND) */
LISPFUNNF(punbound,0) {
  VALUES1(unbound);
}

/* update_instance(obj)
 updates a CLOS instance after its class or one of its superclasses has been
 redefined.
 > user_obj: a CLOS instance, possibly a forward pointer
 > obj: the same CLOS instance, not a forward pointer
 < result: the same CLOS instance, not a forward pointer
 can trigger GC */
global maygc object update_instance (object user_obj, object obj) {
  /* Note about the handling of multiple consecutive class redefinitions:
     When there are multiple class redefinitions before an instance gets to
     be updated, we call UPDATE-INSTANCE-FOR-REDEFINED-CLASS once for each
     redefinition, not once for all changes together.
     Rationale:
       1. CLHS 4.3.6. says
            "When the class C is redefined, changes are propagated to its
             instances ... Updating such an instance occurs at an
             implementation-dependent time, but no later than the next time
             a slot of that instance is read or written."
          This implies that conceptually, there is an update for each class
          redefinition.
       2. It's easier for the user to write customization methods for
          UPDATE-INSTANCE-FOR-REDEFINED-CLASS that take into account each
          step separately, rather than arbitrary groupings of consecutive
          steps.
       3. When in a redefinition, a local slot is discarded, and in a later
          redefinition, a local slot of the same name is added, we would need
          to pass the slot both among the added-slots and among the discarded-
          slots, and it's questionable whether user-defined
          UPDATE-INSTANCE-FOR-REDEFINED-CLASS methods handle this correctly.
     The downside of this way of handling multiple redefinitions is that while
     UPDATE-INSTANCE-FOR-REDEFINED-CLASS is processing, slot accesses to the
     instance being redefined must *not* invoke update_instance (otherwise we
     get an unwanted recursion; this slows down SLOT-VALUE. */
 #if defined(STACKCHECKS) || defined(STACKCHECKC)
  var gcv_object_t *saved_stack = STACK;
 #endif
  pushSTACK(user_obj);
  {
    var gcv_object_t* top_of_frame = STACK;
    var sp_jmp_buf returner; /* return point */
    finish_entry_frame(UNWIND_PROTECT,returner,, goto clean_up; );
  }
  record_flags_set(TheInstance(obj),instflags_beingupdated_B);
  do {
    pushSTACK(obj);
    var object cv = TheInstance(obj)->inst_class_version;
    # We know that the next class is already finalized before
    # TheInstance(obj)->inst_class_version is filled.
    {
      var object newclass = TheClassVersion(TheClassVersion(cv)->cv_next)->cv_class;
      if (!eq(TheClass(newclass)->initialized,fixnum(6)))
        NOTREACHED;
    }
    # Compute the information needed for the update, if not already done.
    if (nullp(TheClassVersion(cv)->cv_slotlists_valid_p)) {
      # Invoke (CLOS::CLASS-VERSION-COMPUTE-SLOTLISTS cv):
      pushSTACK(cv); funcall(S(class_version_compute_slotlists),1);
      obj = STACK_0;
      cv = TheInstance(obj)->inst_class_version;
      ASSERT(!nullp(TheClassVersion(cv)->cv_slotlists_valid_p));
    }
    pushSTACK(TheClassVersion(cv)->cv_added_slots);
    pushSTACK(TheClassVersion(cv)->cv_discarded_slots);
    # Fetch the values of the local slots that are discarded.
    {
      var uintV max_local_slots = posfixnum_to_V(TheClass(TheClassVersion(cv)->cv_class)->instance_size);
      get_space_on_STACK(2*max_local_slots);
      var object plist = TheClassVersion(cv)->cv_discarded_slot_locations;
      var uintL count = 0;
      while (consp(plist)) {
        var object slotname = Car(plist);
        plist = Cdr(plist);
        var object slotinfo = Car(plist);
        plist = Cdr(plist);
        ASSERT(atomp(slotinfo));
        var object value = TheSrecord(obj)->recdata[posfixnum_to_V(slotinfo)];
        if (!eq(value,unbound)) {
          pushSTACK(slotname);
          pushSTACK(value);
          count += 2;
        }
      }
      plist = listof(count);
      pushSTACK(plist);
    }
    obj = STACK_3;
    cv = TheInstance(obj)->inst_class_version;
    # Fetch the values of the slots that remain local or were shared and
    # become local. These values are retained.
    var uintL kept_slots;
    {
      var object oldclass = TheClassVersion(cv)->cv_class;
      var object newclass = TheClassVersion(TheClassVersion(cv)->cv_next)->cv_class;
      var uintV max_local_slots = posfixnum_to_V(TheClass(newclass)->instance_size);
      get_space_on_STACK(2*max_local_slots);
      var object plist = TheClassVersion(cv)->cv_kept_slot_locations;
      var uintL count = 0;
      while (consp(plist)) {
        var object old_slotinfo = Car(plist);
        plist = Cdr(plist);
        var object new_slotinfo = Car(plist);
        plist = Cdr(plist);
        var object value =
          (atomp(old_slotinfo)
           /* local slot, old_slotinfo is index */
           ? TheSrecord(obj)->recdata[posfixnum_to_V(old_slotinfo)]
           /* shared slot, old_slotinfo is (class . index) */
           : TheSvector(TheClassVersion(Car(old_slotinfo))->cv_shared_slots)
               ->data[posfixnum_to_V(Cdr(old_slotinfo))]);
        if (!eq(value,unbound)) {
          pushSTACK(value);
          pushSTACK(new_slotinfo);
          count++;
        }
      }
      kept_slots = count;
    }
    # STACK layout: user-obj, UNWIND-PROTECT frame,
    #               obj, added-slots, discarded-slots, propertylist,
    #               {old-value, new-slotinfo}*kept_slots.
    # ANSI CL 4.3.6.1. Modifying the Structure of Instances
    {
      var object newclass = TheClassVersion(TheClassVersion(cv)->cv_next)->cv_class;
      /* (CLOS::ALLOCATE-STD-INSTANCE newclass (class-instance-size newclass)) or
         (CLOS::ALLOCATE-FUNCALLABLE-INSTANCE newclass (class-instance-size newclass)): */
      pushSTACK(newclass); pushSTACK(TheClass(newclass)->instance_size);
      if (nullp(TheClass(newclass)->funcallablep))
        C_allocate_std_instance();
      else
        C_allocate_funcallable_instance();
    }
    obj = value1;
    record_flags_set(TheInstance(obj),instflags_beingupdated_B);
    { /* Turn user-obj into a forward-pointer (see the instance_un_realloc
         macro): */
      set_break_sem_1(); /* forbid interrupts */
      var Instance ptr = TheInstance(STACK_(2+4+2*kept_slots));
      record_flags_set(ptr,instflags_forwarded_B);
      ptr->inst_class_version = obj;
      clr_break_sem_1(); /* permit interrupts again */
    }
    ASSERT(Record_flags(STACK_(2+4+2*kept_slots)) & instflags_forwarded_B);
    dotimesL(kept_slots,kept_slots, {
      var object new_slotinfo = popSTACK();
      ASSERT(atomp(new_slotinfo));
      TheSrecord(obj)->recdata[posfixnum_to_V(new_slotinfo)] = popSTACK();
    });
    STACK_3 = STACK_(2+4);
    # STACK layout: user-obj, UNWIND-PROTECT frame,
    #               user-obj, added-slots, discarded-slots, propertylist.
    # ANSI CL 4.3.6.2. Initializing Newly Added Local Slots
    funcall(S(update_instance_frc),4);
    # STACK layout: user-obj, UNWIND-PROTECT frame.
    obj = STACK_2;
    instance_un_realloc(obj);
  } while (!instance_valid_p(obj));
  record_flags_clr(TheInstance(obj),instflags_beingupdated_B);
  skipSTACK(1+2); /* unwind UNWIND-PROTECT frame, drop user-obj */
 #if defined(STACKCHECKS) || defined(STACKCHECKC)
  if (saved_stack != STACK) abort();
 #endif
  return obj;
 clean_up: {
  var restartf_t fun = unwind_protect_to_save.fun;
  var gcv_object_t* arg = unwind_protect_to_save.upto_frame;
  skipSTACK(2); /* unwind UNWIND-PROTECT frame */
  /* Mark the instance update as being terminated. */
  obj = STACK_0;
  instance_un_realloc(obj);
  record_flags_clr(TheInstance(obj),instflags_beingupdated_B);
  fun(arg); /* jump further */
  NOTREACHED;
 }
}

/* UP: check keywords, cf. SYSTEM::KEYWORD-TEST
 keyword_test(caller,rest_args_pointer,argcount,valid_keywords);
 > caller: caller (a symbol)
 > rest_args_pointer: pointer to the arguments
 > argcount: number of arguments / 2
 > valid_keywords: list of valid keywords or T if all are valid */
local void keyword_test (object caller, gcv_object_t* rest_args_pointer,
                         uintC argcount, object valid_keywords) {
  if (argcount==0)
    return;
  if (eq(valid_keywords,T))
    return;
  { /* check whether all specified keywords occur in valid_keywords: */
    var gcv_object_t* ptr = rest_args_pointer;
    var uintC count = argcount;
    do {
      var object key = NEXT(ptr);
      var object val = NEXT(ptr);
      if (eq(key,S(Kallow_other_keys))) {
        if (nullp(val)) break;  /* need a check */
        else return;            /* no check */
      }
    } while(--count);
    ptr = rest_args_pointer;
    count = argcount;
    do {
      var object key = NEXT(ptr);
      var object val = NEXT(ptr);
      if (!symbolp(key))
        fehler_key_notkw(key,caller);
      if (!eq(key,S(Kallow_other_keys))
          && nullp(memq(key,valid_keywords))) /* not found */
        fehler_key_badkw(caller,key,val,valid_keywords);
    } while(--count);
  }
}

/* UP: find initarg of the slot in the arglist */
local inline gcv_object_t* slot_in_arglist (const object slot, uintC argcount,
                                            gcv_object_t* rest_args_pointer) {
  var object l = TheSlotDefinition(slot)->slotdef_initargs;
  var gcv_object_t* ptr = rest_args_pointer;
  var uintC count;
  dotimespC(count,argcount, {
    var object initarg = NEXT(ptr);
    if (!nullp(memq(initarg,l)))
      return ptr;
    NEXT(ptr);
  });
  return NULL;
}

/* (CLOS::%SHARED-INITIALIZE instance slot-names &rest initargs)
 instance is an Instance of <standard-object> or <structure-object>,
 initargs is a list of pairs.
 This is the primary method of CLOS:SHARED-INITIALIZE.
 cf. clos.lisp
 (defmethod shared-initialize ((instance standard-object) slot-names &rest initargs)
   (check-initialization-argument-list initargs 'shared-initialize)
   (dolist (slot (class-slots (class-of instance)))
     (let ((slotname (slot-definition-name slot)))
       (multiple-value-bind (init-key init-value foundp)
           (get-properties initargs (slot-definition-initargs slot))
         (declare (ignore init-key))
         (if foundp
           (setf (slot-value instance slotname) init-value)
           (unless (slot-boundp instance slotname)
             (let ((initfunction (slot-definition-initfunction slot)))
               (when initfunction
                 (when (or (eq slot-names 'T)
                           (member slotname slot-names :test #'eq))
                   (setf (slot-value instance slotname)
                         (funcall initfunction))))))))))
   instance) */
LISPFUN(pshared_initialize,seclass_default,2,0,rest,nokey,0,NIL) {
  check_initialization_argument_list(argcount,S(shared_initialize));
  argcount = argcount/2; /* number of Initarg/Value-pairs */
  { /* stack layout: instance, slot-names, argcount Initarg/Value-Pairs. */
    var object instance = Before(rest_args_pointer STACKop 1);
    /* Instance of <standard-class> or <structure-class>: */
    var object clas = class_of(instance); /* instance var is now invalid */
    /* list of all slots (as slot-definitions): */
    var object slots = TheClass(clas)->slots;
    while (consp(slots)) {
      var object slot = Car(slots);
      slots = Cdr(slots);
      /* search if the slot is initialized by the initargs: */
      if (argcount > 0) {
        var gcv_object_t* ptr = slot_in_arglist(slot,argcount,rest_args_pointer);
        if (ptr == NULL)
          goto initarg_not_found;
        value1 = NEXT(ptr);
        goto fill_slot;
      }
     initarg_not_found:
      { /* not found -> test for (slot-boundp instance slotname) first: */
        var object slotinfo = slot;
        if (regular_instance_p(slotinfo)) {
          if (!eq(TheSlotDefinition(slotinfo)->slotdef_efm_sbuc,L(pslot_boundp_using_class))) {
            # Call (eff-SLOT-BOUNDP-USING-CLASS clas instance slot):
            var object efm = TheSlotDefinition(slotinfo)->slotdef_efm_sbuc;
            pushSTACK(clas); pushSTACK(slots); pushSTACK(slot);
            pushSTACK(clas); pushSTACK(Before(rest_args_pointer STACKop 1)); pushSTACK(slot);
            funcall(efm,3);
            slot = popSTACK(); slots = popSTACK(); clas = popSTACK();
            if (!nullp(value1))
              goto slot_done;
            goto slot_is_unbound;
          }
          slotinfo = TheSlotDefinition(slotinfo)->slotdef_location;
        }
        if (!eq(*ptr_to_slot(Before(rest_args_pointer STACKop 1),slotinfo,slot),
                unbound))
          goto slot_done;
      }
     slot_is_unbound:
      { /* slot does not have a value yet. Poss. evaluate the initform: */
        var object init = Cdr(TheSlotDefinition(slot)->slotdef_inheritable_initer); /* (slot-definition-initfunction slot) */
        if (nullp(init))
          goto slot_done;
        { /* search slot in slot-names: */
          var object slotnames = Before(rest_args_pointer);
          if (eq(slotnames,T))
            goto eval_init;
          var object slotname = TheSlotDefinition(slot)->slotdef_name;
          if (!nullp(memq(slotname,slotnames)))
            goto eval_init;
          goto slot_done;
        }
       eval_init:
        /* evaluate the initform: */
        if (closurep(init)
            && eq(TheClosure(init)->clos_name_or_class_version,S(constant_initfunction))
            && eq(TheClosure(init)->clos_codevec,O(constant_initfunction_code))) {
          value1 = TheClosure(init)->other[0];
        } else {
          pushSTACK(clas); pushSTACK(slots); pushSTACK(slot);
          funcall(init,0);
          slot = popSTACK(); slots = popSTACK(); clas = popSTACK();
        }
      }
     fill_slot: {
        /* initialize slot with value1: */
        var object slotinfo = slot;
        if (regular_instance_p(slotinfo)) {
          if (!eq(TheSlotDefinition(slotinfo)->slotdef_efm_ssvuc,L(pset_slot_value_using_class))) {
            # Call (eff-SET-SLOT-VALUE-USING-CLASS value1 clas instance slot):
            var object efm = TheSlotDefinition(slotinfo)->slotdef_efm_ssvuc;
            pushSTACK(clas); pushSTACK(slots);
            pushSTACK(value1); pushSTACK(clas); pushSTACK(Before(rest_args_pointer STACKop 1)); pushSTACK(slot);
            funcall(efm,4);
            slots = popSTACK(); clas = popSTACK();
            goto slot_done;
          }
          slotinfo = TheSlotDefinition(slotinfo)->slotdef_location;
        }
        *ptr_to_slot(Before(rest_args_pointer STACKop 1),slotinfo,slot)
          = value1;
      }
     slot_done: ;
    }
  }
  VALUES1(Before(rest_args_pointer STACKop 1)); /* instance as value */
  set_args_end_pointer(rest_args_pointer STACKop 2); /* clean up STACK */
}

/* UP: call the non-%SHARED-INITIALIZE init function */
local inline void call_init_fun (object fun, object last,
                                 gcv_object_t* rest_args_pointer,
                                 uintC argcount) {
  /* shift initargs in the stack down by 1, then call fun: */
  if (argcount > 0) {
    var gcv_object_t* ptr = rest_args_pointer;
    var uintC count;
    dotimespC(count,argcount, {
      var object next = Next(ptr); NEXT(ptr) = last;
      last = Next(ptr); NEXT(ptr) = next;
    });
  }
  pushSTACK(last);
  funcall(fun,2*argcount+2);
}

/* (CLOS::%REINITIALIZE-INSTANCE instance &rest initargs)
 instance is an Instance of <standard-object> or <structure-object>,
 initargs as list of pairs.
 This is the primary method of CLOS:REINITIALIZE-INSTANCE.
 cf. clos.lisp
 (defmethod reinitialize-instance ((instance standard-object) &rest initargs
                                   &key &allow-other-keys)
   (check-initialization-argument-list initargs 'reinitialize-instance)
   (let ((h (gethash (class-of instance) *reinitialize-instance-table*)))
     (if h
       (progn
         ; 28.1.9.2. validity of initialization arguments
         (let ((valid-keywords (car h)))
           (sys::keyword-test initargs valid-keywords))
         (if (not (eq (cdr h) #'clos::%shared-initialize))
           ; apply effective method of shared-initialize:
           (apply (cdr h) instance 'NIL initargs)
           ; clos::%shared-initialize with slot-names=NIL can be simplified:
           (progn
             (dolist (slot (class-slots (class-of instance)))
               (let ((slotname (slot-definition-name slot)))
                 (multiple-value-bind (init-key init-value foundp)
                     (get-properties initargs (slot-definition-initargs slot))
                   (declare (ignore init-key))
                   (if foundp
                     (setf (slot-value instance slotname) init-value)))))
             instance)))
       (apply #'initial-reinitialize-instance instance initargs)))) */
LISPFUN(preinitialize_instance,seclass_default,1,0,rest,nokey,0,NIL) {
  var object instance = Before(rest_args_pointer);
  /* instance of <standard-class> or <structure-class>: */
  var object clas = class_of(instance); /* instance var is now invalid */
  { /* search (GETHASH class *REINITIALIZE-INSTANCE-TABLE*): */
    var object info =
      gethash(clas,Symbol_value(S(reinitialize_instance_table)),false);
    if (eq(info,nullobj)) {
      /* calculate hash-table-entry freshly. See clos.lisp. */
      funcall(S(initial_reinitialize_instance),argcount+1); return;
    }
    check_initialization_argument_list(argcount,S(reinitialize_instance));
    argcount = argcount/2; /* number of Initarg/Value-pairs */
    keyword_test(S(reinitialize_instance),rest_args_pointer,
                 argcount,Car(info));
    /* stack layout: instance, slot-names, argcount Initarg/Value-pairs. */
    var object fun = Cdr(info);
    if (!eq(fun,L(pshared_initialize))) {
      call_init_fun(fun,NIL,rest_args_pointer,argcount);
      return;
    }
  }
  { /* CLOS::%SHARED-INITIALIZE with slot-names=NIL can be simplified:
       list of all slots (as slot-definitions): */
    var object slots = TheClass(clas)->slots;
    while (consp(slots)) {
      var object slot = Car(slots);
      slots = Cdr(slots);
      /* search if the slot is initialized by the initargs: */
      if (argcount > 0) {
        var gcv_object_t* ptr = slot_in_arglist(slot,argcount,rest_args_pointer);
        if (ptr != NULL) {
          var object value = NEXT(ptr);
          /* initialize slot with value: */
          var object slotinfo = slot;
          if (regular_instance_p(slotinfo)) {
            if (!eq(TheSlotDefinition(slotinfo)->slotdef_efm_ssvuc,L(pset_slot_value_using_class))) {
              # Call (eff-SET-SLOT-VALUE-USING-CLASS value clas instance slot):
              var object efm = TheSlotDefinition(slotinfo)->slotdef_efm_ssvuc;
              pushSTACK(clas); pushSTACK(slots);
              pushSTACK(value); pushSTACK(clas); pushSTACK(Before(rest_args_pointer)); pushSTACK(slot);
              funcall(efm,4);
              slots = popSTACK(); clas = popSTACK();
              goto slot_done;
            }
            slotinfo = TheSlotDefinition(slotinfo)->slotdef_location;
          }
          *ptr_to_slot(Before(rest_args_pointer),slotinfo,slot) = value;
         slot_done: ;
        }
      }
    }
  }
  VALUES1(Before(rest_args_pointer)); /* instance as value */
  set_args_end_pointer(rest_args_pointer STACKop 1); /* clean up STACK */
}

/* (CLOS::%INITIALIZE-INSTANCE instance &rest initargs)
 instance is an Instance of <standard-object> or <structure-object>,
 initargs is a list of pairs.
 This is the primary method of CLOS:INITIALIZE-INSTANCE.
 cf. clos.lisp
 (defmethod initialize-instance ((instance standard-object) &rest initargs
                                 &key &allow-other-keys)
   (check-initialization-argument-list initargs 'initialize-instance)
   (let ((h (gethash class *make-instance-table*)))
     (if h
       (if (not (eq (svref h 3) #'clos::%shared-initialize))
         ; apply effective method of shared-initialize:
         (apply (svref h 3) instance 'T initargs)
         ; clos::%shared-initialize with slot-names=T can be simplified:
         (progn
           (dolist (slot (class-slots (class-of instance)))
             (let ((slotname (slot-definition-name slot)))
               (multiple-value-bind (init-key init-value foundp)
                   (get-properties initargs (slot-definition-initargs slot))
                 (declare (ignore init-key))
                 (if foundp
                   (setf (slot-value instance slotname) init-value)
                   (unless (slot-boundp instance slotname)
                     (let ((initfunction (slot-definition-initfunction slot)))
                       (when initfunction
                         (setf (slot-value instance slotname)
                               (funcall initfunction)))))))))
           instance))
       (apply #'initial-initialize-instance instance initargs)))) */
local Values do_initialize_instance (object info,
                                     gcv_object_t* rest_args_pointer,
                                     uintC argcount);
LISPFUN(pinitialize_instance,seclass_default,1,0,rest,nokey,0,NIL) {
  var object instance = Before(rest_args_pointer);
  /* instance of <standard-class> or <structure-class>: */
  var object clas = class_of(instance); /* instance var is not invalid */
  { /* search (GETHASH class *MAKE-INSTANCE-TABLE*): */
    var object info = gethash(clas,Symbol_value(S(make_instance_table)),true);
    if (eq(info,nullobj)) {
      /* calculate hash-table-entry freshly. See clos.lisp. */
      funcall(S(initial_initialize_instance),argcount+1); return;
    }
    check_initialization_argument_list(argcount,S(initialize_instance));
    argcount = argcount/2; /* number of Initarg/Value-pairs */
    return_Values do_initialize_instance(info,rest_args_pointer,argcount);
  }
}
local Values do_initialize_instance (object info,
                                     gcv_object_t* rest_args_pointer,
                                     uintC argcount) {
  { /* stack layout: instance, argcount Initarg/Value-pairs. */
    var object fun = TheSvector(info)->data[3];
    if (!eq(fun,L(pshared_initialize))) {
      call_init_fun(fun,T,rest_args_pointer,argcount);
      return;
    }
  }
  { /* CLOS::%SHARED-INITIALIZE with slot-names=T can be simplified: */
    var object instance = Before(rest_args_pointer);
    var object clas = class_of(instance); /* instance of <standard-class> or <structure-class> */
    var object slots = TheClass(clas)->slots; /* list of all slots (as slot-definitions) */
    while (consp(slots)) {
      var object slot = Car(slots);
      slots = Cdr(slots);
      /* search if the slot is initialized by the initargs: */
      if (argcount > 0) {
        var gcv_object_t* ptr = slot_in_arglist(slot,argcount,rest_args_pointer);
        if (ptr == NULL)
          goto initarg_not_found;
        value1 = NEXT(ptr);
        goto fill_slot;
      }
     initarg_not_found:
      { /* not found -> first test for (slot-boundp instance slotname): */
        var object slotinfo = slot;
        if (regular_instance_p(slotinfo)) {
          if (!eq(TheSlotDefinition(slotinfo)->slotdef_efm_sbuc,L(pslot_boundp_using_class))) {
            # Call (eff-SLOT-BOUNDP-USING-CLASS clas instance slot):
            var object efm = TheSlotDefinition(slotinfo)->slotdef_efm_sbuc;
            pushSTACK(clas); pushSTACK(slots); pushSTACK(slot);
            pushSTACK(clas); pushSTACK(Before(rest_args_pointer)); pushSTACK(slot);
            funcall(efm,3);
            slot = popSTACK(); slots = popSTACK(); clas = popSTACK();
            if (!nullp(value1))
              goto slot_done;
            goto slot_is_unbound;
          }
          slotinfo = TheSlotDefinition(slotinfo)->slotdef_location;
        }
        if (!eq(*ptr_to_slot(Before(rest_args_pointer),slotinfo,slot),unbound))
          goto slot_done;
      }
     slot_is_unbound:
      { /* Slot has no value yet. Evaluate the initform: */
        var object init = Cdr(TheSlotDefinition(slot)->slotdef_inheritable_initer); /* (slot-definition-initfunction slot) */
        if (nullp(init))
          goto slot_done;
        if (closurep(init)
            && eq(TheClosure(init)->clos_name_or_class_version,S(constant_initfunction))
            && eq(TheClosure(init)->clos_codevec,O(constant_initfunction_code))) {
          value1 = TheClosure(init)->other[0];
        } else {
          pushSTACK(clas); pushSTACK(slots); pushSTACK(slot);
          funcall(init,0);
          slot = popSTACK(); slots = popSTACK(); clas = popSTACK();
        }
      }
     fill_slot: {
        /* initialize slot with value1: */
        var object slotinfo = slot;
        if (regular_instance_p(slotinfo)) {
          if (!eq(TheSlotDefinition(slotinfo)->slotdef_efm_ssvuc,L(pset_slot_value_using_class))) {
            # Call (eff-SET-SLOT-VALUE-USING-CLASS value1 clas instance slot):
            var object efm = TheSlotDefinition(slotinfo)->slotdef_efm_ssvuc;
            pushSTACK(clas); pushSTACK(slots);
            pushSTACK(value1); pushSTACK(clas); pushSTACK(Before(rest_args_pointer)); pushSTACK(slot);
            funcall(efm,4);
            slots = popSTACK(); clas = popSTACK();
            goto slot_done;
          }
          slotinfo = TheSlotDefinition(slotinfo)->slotdef_location;
        }
        *ptr_to_slot(Before(rest_args_pointer),slotinfo,slot) = value1;
      }
     slot_done: ;
    }
  }
  VALUES1(Before(rest_args_pointer)); /* instance as value */
  set_args_end_pointer(rest_args_pointer STACKop 1); /* clean up STACK */
}

/* (CLOS::%MAKE-INSTANCE class &rest initargs)
 class is an Instance of <standard-class> or <structure-class>,
 initargs is a list (of pairs, hopefully).
 cf. clos.lisp
 (defun %make-instance (class &rest initargs &key &allow-other-keys)
   (check-initialization-argument-list initargs 'make-instance)
   ; take note of 28.1.9.3., 28.1.9.4. default-initargs:
   (dolist (default-initarg (class-default-initargs class))
     (let ((nothing default-initarg))
       (when (eq (getf initargs (car default-initarg) nothing) nothing)
         (setq initargs
               (append initargs
                 (list (car default-initarg)
                       (funcall (caddr default-initarg))))))))
   (let ((h (gethash class *make-instance-table*)))
     (if h
       (progn
         ; 28.1.9.2. validity of initialization arguments
         (let ((valid-keywords (svref h 0)))
           (sys::keyword-test initargs valid-keywords))
         (let ((instance (apply #'allocate-instance class initargs)))
           (if (not (eq (svref h 2) #'clos::%initialize-instance))
             ; apply effective method of initialize-instance:
             (apply (svref h 2) instance initargs)
             ; clos::%initialize-instance can be simplified (one does not
             ; even have to look it up in *make-instance-table*):
             (if (not (eq (svref h 3) #'clos::%shared-initialize))
               ; apply effective method of shared-initialize:
               (apply (svref h 3) instance 'T initargs)
               ...))))
       (apply #'initial-make-instance class initargs)))) */
LISPFUN(pmake_instance,seclass_default,1,0,rest,nokey,0,NIL) {
  check_initialization_argument_list(argcount,S(make_instance));
  argcount = argcount/2; /* number of Initarg/Value-pairs */
  /* stack layout: class, argcount Initarg/Value-pairs. */
  { /* add default-initargs: */
    var object clas = Before(rest_args_pointer);
    if (!eq(TheClass(clas)->initialized,fixnum(6))) {
      /* Call (CLOS:FINALIZE-INHERITANCE class). */
      pushSTACK(clas); funcall(S(finalize_inheritance),1);
      clas = Before(rest_args_pointer);
      /* The class must be finalized now, otherwise FINALIZE-INHERITANCE has
         not done its job. */
      ASSERT(eq(TheClass(clas)->initialized,fixnum(6)));
    }
    var object l = TheClass(clas)->default_initargs;
    while (consp(l)) {
      var object default_initarg = Car(l);
      l = Cdr(l);
      var object key = Car(default_initarg);
      /* search key among the initargs so far: */
      if (argcount > 0) {
        var gcv_object_t* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,argcount, {
          if (eq(NEXT(ptr),key))
            goto key_found;
          NEXT(ptr);
        });
      }
      /* not found */
      pushSTACK(key); /* Initarg in the stack */
      {
        var object init = Car(Cdr(Cdr(default_initarg)));
        if (closurep(init)
            && eq(TheClosure(init)->clos_name_or_class_version,S(constant_initfunction))
            && eq(TheClosure(init)->clos_codevec,O(constant_initfunction_code))) {
          pushSTACK(TheClosure(init)->other[0]); /* default in the stack */
        } else {
          pushSTACK(l);
          funcall(init,0);
          l = STACK_0;
          STACK_0 = value1; /* default in the stack */
        }
      }
      argcount++;
     key_found: ;
    }
  }
  { /* search (GETHASH class *MAKE-INSTANCE-TABLE*): */
    var object clas = Before(rest_args_pointer);
    var object info = gethash(clas,Symbol_value(S(make_instance_table)),false);
    if (eq(info,nullobj)) {
      /* calculate hash-table-entry freshly. See clos.lisp. */
      return_Values funcall(S(initial_make_instance),2*argcount+1);
    } else { /* check keywords: */
      keyword_test(S(make_instance),rest_args_pointer,
                   argcount,TheSvector(info)->data[0]);
      /* call the effective method of ALLOCATE-INSTANCE: */
      pushSTACK(info);
      {
        var object fun = TheSvector(info)->data[1];
        if (!eq(fun,L(pallocate_instance))) {
          var gcv_object_t* ptr = rest_args_pointer STACKop 1;
          var uintC count;
          dotimespC(count,2*argcount+1, { pushSTACK(NEXT(ptr)); });
          funcall(fun,2*argcount+1);
          pushSTACK(value1); /* save instance */
          var object cls = class_of(value1);
          if (!eq(cls,Before(rest_args_pointer))) {
            /* instance already in STACK_0 */
            pushSTACK(Before(rest_args_pointer));
            pushSTACK(S(allocate_instance));
            fehler(error,GETTEXT("~S method for ~S returned ~S"));
          }
          value1 = popSTACK(); /* restore instance */
        } else {
          do_allocate_instance(clas);
        }
      }
      info = popSTACK();
      /* call the effective method of INITIALIZE-INSTANCE:
         instance as the 1st argument instead of class: */
      Before(rest_args_pointer) = value1;
      var object fun = TheSvector(info)->data[2];
      /* save the instance in case INITIALIZE-INSTANCE returns junk
         see 7.1.7 "Definitions of Make-Instance and Initialize-Instance"
         http://www.lisp.org/HyperSpec/Body/sec_7-1-7.html */
      pushSTACK(value1);
      if (argcount>0) { /* (rotatef STACK_0 ... STACK_(2*argcount)) */
        var uintC count;
        var gcv_object_t* ptr = &STACK_0;
        dotimespC(count,2*argcount,
        { *ptr = *(ptr STACKop 1); ptr skipSTACKop 1; });
        *ptr = value1;
      }
      rest_args_pointer skipSTACKop -1;
      if (eq(fun,L(pinitialize_instance)))
        /* CLOS::%INITIALIZE-INSTANCE can be simplified
           (do not have to look into *make-instance-table* again): */
        do_initialize_instance(info,rest_args_pointer,argcount);
      else
        funcall(fun,2*argcount+1);
      VALUES1(popSTACK());
    }
  }
}

/* (CLOS::%CHANGE-CLASS instance new-class)
   Copy instance, and return the copy.
   Make instance point to a new instance of new-class. */
LISPFUNN(pchange_class,2) {
  /* Stack layout: instance, new-class. */
  /* Create the new object, to be filled in Lisp: */
  do_allocate_instance(STACK_0);
  STACK_0 = value1;
  /* Stack layout: instance, new-instance. */
  /* Create copy of the old instance: */
  var object clas = class_of(STACK_1); /* Calls instance_update if necessary */
  pushSTACK(clas);
  do_allocate_instance(STACK_0); /* these values are returned */
  /* Stack layout: instance, new-instance, old-class. */
  var object old_instance = STACK_2;
  {
    var object old_instance_forwarded = old_instance;
    instance_un_realloc(old_instance_forwarded);
    copy_mem_o(&TheInstance(value1)->inst_class_version,
               &TheInstance(old_instance_forwarded)->inst_class_version,
               posfixnum_to_V(TheClass(STACK_0)->instance_size));
  }
  { /* Turn instance into a realloc (see the instance_un_realloc macro): */
    set_break_sem_1(); /* forbid interrupts */
    var Instance ptr = TheInstance(old_instance);
    record_flags_set(ptr,instflags_forwarded_B);
    ptr->inst_class_version = STACK_1;
    clr_break_sem_1(); /* permit interrupts again */
  }
  ASSERT(Record_flags(old_instance) & instflags_forwarded_B);
  skipSTACK(3);
}
