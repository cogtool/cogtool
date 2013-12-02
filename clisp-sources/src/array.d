/*
 * Array functions
 * Bruno Haible 1990-2005
 * Sam Steingold 1998-2004
 * German comments translated into English: Stefan Kain 2002-09-23
 */

#include "lispbibl.c"
#include "arilev0.c" /* for bit_op, also defines mulu24 and mulu32_unchecked */

/* ======================================================================== */
/* Global auxiliary functions */

/* Function: Copies a simple-vector.
 copy_svector(vector)
 > vector: simple-vector
 < result: fresh simple-vector with the same contents
 can trigger GC */
global maygc object copy_svector (object vector) {
  var uintL length = Svector_length(vector);
  pushSTACK(vector);
  var object newvector = allocate_vector(length); /* vector of same length */
  vector = popSTACK();
  /* copy the contents of vector into newvector: */
  if (length != 0) {
    var gcv_object_t* ptr1 = &TheSvector(vector)->data[0];
    var gcv_object_t* ptr2 = &TheSvector(newvector)->data[0];
    dotimespL(length,length, {
      *ptr2++ = *ptr1++;
    });
  }
  return newvector;
}

/* Function: Copies a simple-bit/byte-vector.
 copy_sbvector(vector)
 > vector: simple-bit/byte-vector
 < result: fresh simple-bit/byte-vector with the same contents
 can trigger GC */
global maygc object copy_sbvector (object vector) {
  var uintB atype = sbNvector_atype(vector);
  var uintL length = Sbvector_length(vector);
  pushSTACK(vector);
  var object newvector = allocate_bit_vector(atype,length); /* vector of same length */
  vector = popSTACK();
  if (length != 0) {
    var const uintB* ptr1 = &TheSbvector(vector)->data[0];
    var uintB* ptr2 = &TheSbvector(newvector)->data[0];
    dotimespL(length,ceiling(length<<atype,8), {
      *ptr2++ = *ptr1++;
    });
  }
  return newvector;
}

LISPFUNNR(copy_simple_vector,1)
{ /* (SYS::%COPY-SIMPLE-VECTOR vector) returns a copy
     of the simple-vector VECTOR. */
  var object obj = popSTACK();
  if (!simple_vector_p(obj))
    fehler_kein_svector(S(copy_simple_vector),obj);
  VALUES1(copy_svector(obj));
}

/* Function: Returns the active length of a vector (same as LENGTH).
 vector_length(vector)
 > vector: a vector
 < result: its length */
global uintL vector_length (object vector) {
  if (array_simplep(vector)) {
    if (simple_string_p(vector)) {
      sstring_un_realloc(vector);
      return Sstring_length(vector);
    } else
      return Sarray_length(vector);
  }
  /* Indirect Array */
  var Iarray addr = TheIarray(vector);
  var uintL offset = offsetofa(iarray_,dims);
  if (iarray_flags(addr) & bit(arrayflags_dispoffset_bit))
    offset += sizeof(uintL);
  /* The dimensions start at addr+offset. */
  if (iarray_flags(addr) & bit(arrayflags_fillp_bit)) /* fill-pointer ? */
    offset += sizeof(uintL);
  return *(uintL*)pointerplus(addr,offset);
}

/* Function: Canonicalizes an array element-type and returns its
 element type code.
 ** When this function is changed, also update UPGRADED-ARRAY-ELEMENT-TYPE
 ** and SUBTYPE-SEQUENCE in type.lisp!
 eltype_code(element_type)
 > element_type: type specifier
 < result: element type code Atype_xxx
 The canonicalized types are the possible results of ARRAY-ELEMENT-TYPE
 (symbols T, BIT, CHARACTER and lists (UNSIGNED-BYTE n)).
 The result type is a supertype of element_type.
 can trigger GC */
global maygc uintB eltype_code (object obj)
{ /* (cond ((eq obj 'BIT) Atype_Bit)
           ((eq obj 'CHARACTER) Atype_Char)
           ((eq obj 'T) Atype_T)
           ((eq obj 'NIL) Atype_NIL)
           (t (if (subtypep obj 'NIL)
                Atype_NIL
                (multiple-value-bind (low high) (sys::subtype-integer obj)
                  ;; Now (or (null low) (subtypep obj `(INTEGER ,low ,high)))
                  (if (and (integerp low) (not (minusp low)) (integerp high))
                    (let ((l (integer-length high)))
                      ;; Now (subtypep obj `(UNSIGNED-BYTE ,l))
                      (cond ((<= l 1) Atype_Bit)
                            ((<= l 2) Atype_2Bit)
                            ((<= l 4) Atype_4Bit)
                            ((<= l 8) Atype_8Bit)
                            ((<= l 16) Atype_16Bit)
                            ((<= l 32) Atype_32Bit)
                            (t Atype_T)))
                    (if (subtypep type 'CHARACTER)
                      Atype_Char
                      Atype_T)))))) */
  if (eq(obj,S(bit))) { /* symbol BIT ? */
    return Atype_Bit;
  } else if (eq(obj,S(character))) { /* symbol CHARACTER ? */
    return Atype_Char;
  } else if (eq(obj,S(t))) { /* symbol T ? */
    return Atype_T;
  } else if (nullp(obj)) /* symbol NIL ? */
    return Atype_NIL;
  pushSTACK(obj); /* save obj */
  /* (SUBTYPEP obj 'NIL) */
  pushSTACK(obj); pushSTACK(S(nil)); funcall(S(subtypep),2);
  if (!nullp(value1)) {
    skipSTACK(1);
    return Atype_NIL;
  }
  /* (SYS::SUBTYPE-INTEGER obj) */
  pushSTACK(STACK_0); funcall(S(subtype_integer),1);
  obj = popSTACK(); /* restore obj */
  if ((mv_count>1) && integerp(value1)
      && positivep(value1) && integerp(value2)) {
    var uintL l = I_integer_length(value2); /* (INTEGER-LENGTH high) */
    if (l<=1)
      return Atype_Bit;
    if (l<=2)
      return Atype_2Bit;
    if (l<=4)
      return Atype_4Bit;
    if (l<=8)
      return Atype_8Bit;
    if (l<=16)
      return Atype_16Bit;
    if (l<=32)
      return Atype_32Bit;
  }
  /* (SUBTYPEP obj 'CHARACTER) */
  pushSTACK(obj); pushSTACK(S(character)); funcall(S(subtypep),2);
  if (!nullp(value1))
    return Atype_Char;
  return Atype_T;
}

/* Function: Creates a simple-vector with given elements.
 vectorof(len)
 > uintC len: desired vector length
 > STACK_(len-1), ..., STACK_(0): len objects
 < result: simple-vector containing these objects
 Pops n objects off STACK.
 can trigger GC */
global maygc object vectorof (uintC len) {
  var object new_vector = allocate_vector(len);
  if (len > 0) {
    var gcv_object_t* topargptr = STACK STACKop len;
    var gcv_object_t* argptr = topargptr;
    var gcv_object_t* ptr = &TheSvector(new_vector)->data[0];
    dotimespC(len,len, {
      *ptr++ = NEXT(argptr);
    });
    set_args_end_pointer(topargptr);
  }
  return new_vector;
}

LISPFUN(vector,seclass_no_se,0,0,rest,nokey,0,NIL)
{ /* (VECTOR {object}), CLTL p. 290 */
  VALUES1(vectorof(argcount));
}

/* ======================================================================== */
/* Index checking, retrieving the storage vector */

/* An indirect array contains a pointer to another array:
     TheIarray(array)->data.
 The "storage vector" of an array is a 1-dimensional array, of the same
 element type as the original array, without fill-pointer or adjustable bit;
 for arrays of element type NIL, the "storage vector" is the symbol NIL.
 It can be obtained by repeatedly taking TheIarray(array)->data, until
 array satisfies array_simplep || simple_nilarray_p. */

/* Function: Follows the TheIarray(array)->data chain until the storage-vector
 is reached, and thereby sums up displaced-offsets. This function is useful
 for accessing a single array element.
 iarray_displace(array,&index);
 > array: indirect array
 > index: row-major-index
 < result: storage-vector
 < index: absolute index into the storage vector
 It is checked whether the addressed array element lies within the bounds of
 every intermediate array.
 It is not checked whether the chain is ultimately circular. */
local object iarray_displace (object array, uintL* index) {
  loop {
    if (*index >= TheIarray(array)->totalsize)
      goto fehler_bad_index;
    if (!(Iarray_flags(array) & bit(arrayflags_displaced_bit)))
      goto notdisplaced;
    /* array is displaced */
    *index += TheIarray(array)->dims[0]; /* add displaced-offset */
    array = TheIarray(array)->data; /* next array in the chain */
    if (array_simplep(array)) /* next array indirect? */
      goto simple;
  }
 notdisplaced:
  /* array is indirect, but not displaced */
  array = TheIarray(array)->data; /* next array is the storage-vector */
 simple:
  /* have reached the storage-vector, not indirect */
  if (!simple_nilarray_p(array)) {
    if (simple_string_p(array)) {
      sstring_un_realloc(array);
      if (*index >= Sstring_length(array))
        goto fehler_bad_index;
    } else {
      if (*index >= Sarray_length(array))
        goto fehler_bad_index;
    }
  }
  return array;
 fehler_bad_index:
  fehler(error,GETTEXT("index too large")); /* more details?? */
}

/* error: a displaced array does not fit into its target array. */
nonreturning_function(local, fehler_displaced_inconsistent, (void)) {
  fehler(error,GETTEXT("An array has been shortened by adjusting it while another array was displaced to it."));
}

/* Function: For an indirect array, returns the storage vector and the offset.
 Also verifies that all elements of the array are physically present.
 iarray_displace_check(array,size,&index)
 > object array: indirect array
 > uintL size: size
 < result: storage vector
 < index: is incremented by the offset into the storage vector */
global object iarray_displace_check (object array, uintL size, uintL* index) {
  loop {
    if (*index+size > TheIarray(array)->totalsize)
      goto fehler_bad_index;
    if (!(Iarray_flags(array) & bit(arrayflags_displaced_bit)))
      goto notdisplaced;
    /* array is displaced */
    *index += TheIarray(array)->dims[0]; /* add displaced-offset */
    array = TheIarray(array)->data; /* next array in the chain */
    if (array_simplep(array)) /* next array indirect? */
      goto simple;
  }
 notdisplaced:
  /* array is indirect, but not displaced */
  array = TheIarray(array)->data; /* next array is the storage-vector */
 simple:
  /* have reached the storage-vector, not indirect */
  if (!simple_nilarray_p(array)) {
    if (simple_string_p(array)) {
      sstring_un_realloc(array);
      if (*index+size > Sstring_length(array))
        goto fehler_bad_index;
    } else {
      if (*index+size > Sarray_length(array))
        goto fehler_bad_index;
    }
  }
  return array;
 fehler_bad_index:
  fehler_displaced_inconsistent();
}

/* Function: For an array, returns the storage vector and the offset.
 Also verifies that all elements of the array are physically present.
 array_displace_check(array,size,&index)
 > object array: array
 > uintV size: size
 < result: storage vector
 < index: is incremented by the offset into the storage vector */
global object array_displace_check (object array, uintV size, uintL* index) {
  if (array_simplep(array)) /* array indirect? */
    goto simple;
  loop {
    if (*index+size > TheIarray(array)->totalsize)
      goto fehler_bad_index;
    if (!(Iarray_flags(array) & bit(arrayflags_displaced_bit)))
      goto notdisplaced;
    /* array is displaced */
    *index += TheIarray(array)->dims[0]; /* add displaced-offset */
    array = TheIarray(array)->data; /* next array in the chain */
    if (array_simplep(array)) /* next array indirect? */
      goto simple;
  }
 notdisplaced:
  /* array is indirect, but not displaced */
  array = TheIarray(array)->data; /* next array is the storage-vector */
 simple:
  /* have reached the storage-vector, not indirect */
  if (!simple_nilarray_p(array)) {
    if (simple_string_p(array)) {
      sstring_un_realloc(array);
      if (*index+size > Sstring_length(array))
        goto fehler_bad_index;
    } else {
      if (*index+size > Sarray_length(array))
        goto fehler_bad_index;
    }
  }
  return array;
 fehler_bad_index:
  fehler_displaced_inconsistent();
}

/* ======================================================================== */
/* Accessing and storing a single element */

/* Returns the rank of an array.
 arrayrank(array)
 > array: an array
 < object result: rank as a fixnum */
#define arrayrank(array)                                                \
  (mdarrayp(array)                                                      \
   ? fixnum((uintL)Iarray_rank(array)) /* multi-dimensional array */    \
   : Fixnum_1) /* vector has rank 1 */

/* error: bad number of subscripts
 > array: array
 > argcount: (wrong) number of subscripts */
nonreturning_function(local, fehler_subscript_anz,
                      (object array, uintC argcount)) {
  pushSTACK(arrayrank(array));
  pushSTACK(array);
  pushSTACK(fixnum(argcount));
  pushSTACK(TheSubr(subr_self)->name);
  fehler(error,GETTEXT("~S: got ~S subscripts, but ~S has rank ~S"));
}

/* error: bad subscript values
 > argcount: number of subscripts
 > STACK_(argcount): array
 > STACK_(argcount-1),...,STACK_(0): subscripts */
nonreturning_function(local, fehler_subscript_type, (uintC argcount)) {
  var object list = listof(argcount); /* list of subscripts */
  /* STACK_0 is now the array. */
  pushSTACK(list);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(error,GETTEXT("~S: subscripts ~S for ~S are not of type `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))"));
}

/* error: bad subscript values
 > argcount: number of subscripts
 > STACK_(argcount): array
 > STACK_(argcount-1),...,STACK_(0): subscripts */
nonreturning_function(local, fehler_subscript_range,
                      (uintC argcount, uintL subscript, uintL bound)) {
  var object list = listof(argcount); /* list of subscripts */
  pushSTACK(list);
  /* On STACK: array, subscript-list. */
  pushSTACK(UL_to_I(subscript)); /* slot DATUM of TYPE-ERROR */
  {
    var object tmp;
    pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(bound));
    tmp = listof(1); pushSTACK(tmp); tmp = listof(3);
    pushSTACK(tmp); /* slot EXPECTED-TYPE of TYPE-ERROR */
  }
  pushSTACK(STACK_(1+2));
  pushSTACK(STACK_(0+3));
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: subscripts ~S for ~S are out of range"));
}

/* checks subscripts for an AREF/STORE-access, removes them from STACK
 and returns the row-major-index (>=0, <arraysize_limit).
 test_subscripts(array,argptr,argcount)
 > array : non-simpler array
 > argptr : pointer to the Subscripts
 > argcount : number of subscripts
 < result : row-major-index */
local uintL test_subscripts (object array, gcv_object_t* argptr, uintC argcount) {
  var gcv_object_t* args_pointer = argptr; /* save argptr for later */
  /* check number of subscripts: */
  if (argcount != Iarray_rank(array)) /* should be = rank */
    fehler_subscript_anz(array,argcount);
  /* check subscripts themself: */
  var uintL row_major_index = 0;
  var const uintL* dimptr = &TheIarray(array)->dims[0];
  if (Iarray_flags(array) & bit(arrayflags_dispoffset_bit))
    dimptr++; /* poss. skip displaced-offset */
  {
    var uintC count;
    dotimesC(count,argcount, {
      var object subscriptobj = NEXT(argptr); /* Subscript as object */
      if (!(posfixnump(subscriptobj))) { /* subscript must be fixnum>=0. */
        Before(args_pointer) = array;
        fehler_subscript_type(argcount);
      }
      var uintV subscript = posfixnum_to_V(subscriptobj); /* as uintL */
      var uintL dim = *dimptr++; /* corresponding dimension */
      if (subscript>=dim) { /* subscript must be smaller than dimension */
        Before(args_pointer) = array;
        fehler_subscript_range(argcount,subscript,dim);
      }
      /* form row_major_index := row_major_index*dim+subscript: */
      row_major_index =
        mulu32_unchecked(row_major_index,dim)+subscript;
      /* This does not produce an overflow, because it is
         < product of all dimensions so far
         <= product of all dimensions < arraysize_limit <= 2^32
         (exception: When a later dimension is =0 .
         But then there will be an error message, anyway.) */
    });
  }
  set_args_end_pointer(args_pointer);
  return row_major_index;
}

/* error: bad index
 > array: array (usually a vector)
 > STACK_0: (erroneous) index */
nonreturning_function(local, fehler_index_type, (object array)) {
  pushSTACK(STACK_0); /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_array_index)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(array);
  pushSTACK(STACK_(0+3));
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: index ~S for ~S is not of type `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))"));
}

/* error: bad index
 > array: array (usually a vector)
 > STACK_0: (erroneous) index */
nonreturning_function(global, fehler_index_range, (object array, uintL bound)) {
  var object tmp;
  pushSTACK(STACK_0); /* TYPE-ERROR slot DATUM */
  pushSTACK(array);
  pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(bound));
  tmp = listof(1); pushSTACK(tmp); tmp = listof(3);
  array = STACK_0;
  STACK_0 = tmp; /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(array);
  pushSTACK(STACK_(0+3));
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: index ~S for ~S is out of range"));
}

/* checks an index for a AREF/STORE-access into a simple vector.
 test_index(vector)
 > vector: not-reallocated simple Vector
 > STACK_0: index
 < result: index as uintL */
local uintL test_index (object vector) {
  if (!posfixnump(STACK_0)) /* index must be fixnum>=0 . */
    fehler_index_type(vector);
  var uintV index = posfixnum_to_V(STACK_0); /* index as uintL */
  var uintL length = (simple_string_p(vector) ? Sstring_length(vector) : Sarray_length(vector));
  if (index >= length) /* index must be smaller then length */
    fehler_index_range(vector,length);
  return index;
}

/* checks subscripts for a AREF/STORE-access, removes them from STACK
 and returns the row-major-index (>=0, <arraysize_limit) and the data vector.
 subscripts_to_index(array,argptr,argcount, &index)
 > array : array
 > argptr : pointer to the subscripts
 > argcount : number of subscripts
 < index_ : index into the data vector
 < result : the data vector */
local object subscripts_to_index (object array, gcv_object_t* argptr,
                                  uintC argcount, uintL* index_) {
  if (array_simplep(array)) { /* simple vector, will be treated separately: */
    /* check number of subscripts: */
    if (argcount != 1) /* should be = 1 */
      fehler_subscript_anz(array,argcount);
    sstring_un_realloc(array);
    /* check subscript itself: */
    *index_ = test_index(array); /* index = row-major-index = subscript */
    skipSTACK(1); return array;
  } else { /* non-simple array */
    /* check Subscripts, calculate row-major-index, clean up STACK: */
    *index_ = test_subscripts(array,argptr,argcount);
    /* fetch dat vector and absolut index: */
    return iarray_displace(array,&(*index_));
  }
}

/* error message: attempt to retrieve a value from (ARRAY NIL) */
nonreturning_function(global, fehler_nilarray_retrieve, (void)) {
  pushSTACK(TheSubr(subr_self)->name);
  fehler(error,GETTEXT("~S: cannot retrieve values from an array of element type NIL"));
}

/* error message: attempt to store a value in (ARRAY NIL) */
nonreturning_function(global, fehler_nilarray_store, (void)) {
  pushSTACK(TheSubr(subr_self)->name);
  fehler(error,GETTEXT("~S: cannot store values in an array of element type NIL"));
}

/* error message: attempt to access a value from (ARRAY NIL) */
nonreturning_function(global, fehler_nilarray_access, (void)) {
  pushSTACK(TheSubr(subr_self)->name);
  fehler(error,GETTEXT("~S: cannot access values of an array of element type NIL"));
}

/* Function: Performs an AREF access.
 storagevector_aref(storagevector,index)
 > storagevector: a storage vector (simple vector or semi-simple byte vector)
 > index: (already checked) index into the storage vector
 < result: (AREF storagevector index)
 can trigger GC - if the element type is (UNSIGNED-BYTE 32) */
global /*maygc*/ object storagevector_aref (object datenvektor, uintL index) {
  GCTRIGGER_IF(Array_type(datenvektor) == Array_type_sb32vector,
               GCTRIGGER1(datenvektor));
  switch (Array_type(datenvektor)) {
    case Array_type_svector: /* Simple-Vector */
      return TheSvector(datenvektor)->data[index];
    case Array_type_sbvector: /* Simple-Bit-Vector */
      return ( sbvector_btst(datenvektor,index) ? Fixnum_1 : Fixnum_0 );
    case Array_type_sb2vector:
      return fixnum((TheSbvector(datenvektor)->data[index/4]>>(2*((~index)%4)))&(bit(2)-1));
    case Array_type_sb4vector:
      return fixnum((TheSbvector(datenvektor)->data[index/2]>>(4*((~index)%2)))&(bit(4)-1));
    case Array_type_sb8vector:
      return fixnum(TheSbvector(datenvektor)->data[index]);
    case Array_type_sb16vector:
      return fixnum(((uint16*)&TheSbvector(datenvektor)->data[0])[index]);
    case Array_type_sb32vector:
      return UL_to_I(((uint32*)&TheSbvector(datenvektor)->data[0])[index]);
    case Array_type_sstring: /* Simple-String */
      return code_char(schar(datenvektor,index));
    case Array_type_snilvector:  /* (VECTOR NIL) */
      fehler_nilarray_retrieve();
    default: NOTREACHED;
  }
}

/* error: attempting to store an invalid value in an array.
 fehler_store(array,value); */
nonreturning_function(global, fehler_store, (object array, object value)) {
  pushSTACK(value); /* TYPE-ERROR slot DATUM */
  pushSTACK(NIL); /* TYPE-ERROR slot EXPECTED-TYPE */
  if (!simple_nilarray_p(array)) {
    pushSTACK(array);
    STACK_1 = array_element_type(array); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(STACK_2); /* value */
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: ~S does not fit into ~S, bad type"));
  } else {
    pushSTACK(STACK_1); /* value */
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: ~S cannot be stored in an array of element type NIL"));
  }
}

/* performs a STORE-access.
 storagevector_store(datenvektor,index,element,allowgc)
 > datenvektor : a data vector (simple vector or semi-simple byte-vector)
 > index : (checked) index into the data vector
 > element : (unchecked) object to be written
 > allowgc : whether GC is allowed, if datenvektor is a string and element is a character
 > STACK_0 : array (for error-message)
 < datenvektor: possibly reallocated storage vector
 can trigger GC, if datenvektor is a string and element is a character */
local /*maygc*/ object storagevector_store (object datenvektor, uintL index,
                                            object element, bool allowgc) {
  GCTRIGGER_IF(allowgc, GCTRIGGER2(datenvektor,element));
  switch (Array_type(datenvektor)) {
    case Array_type_svector: /* Simple-Vector */
      TheSvector(datenvektor)->data[index] = element;
      return datenvektor;
    case Array_type_sbvector: { /* Simple-Bit-Vector */
      var uintB* addr = &TheSbvector(datenvektor)->data[index/8];
      var uintL bitnummer = (~index)%8; /* 7 - (index mod 8) */
      if (eq(element,Fixnum_0)) {
        *addr &= ~bit(bitnummer);
        return datenvektor;
      } else if (eq(element,Fixnum_1)) {
        *addr |= bit(bitnummer);
        return datenvektor;
      }
    }
      break;
    case Array_type_sb2vector: {
      var uintV wert;
      if (posfixnump(element) && ((wert = posfixnum_to_V(element)) < bit(2))) {
        var uintB* ptr = &TheSbvector(datenvektor)->data[index/4];
        *ptr ^= (*ptr ^ (wert<<(2*((~index)%4)))) & ((bit(2)-1)<<(2*((~index)%4)));
        return datenvektor;
      }
    }
      break;
    case Array_type_sb4vector: {
      var uintV wert;
      if (posfixnump(element) && ((wert = posfixnum_to_V(element)) < bit(4))) {
        var uintB* ptr = &TheSbvector(datenvektor)->data[index/2];
        *ptr ^= (*ptr ^ (wert<<(4*((~index)%2)))) & ((bit(4)-1)<<(4*((~index)%2)));
        return datenvektor;
      }
    }
      break;
    case Array_type_sb8vector: {
      var uintV wert;
      if (posfixnump(element) && ((wert = posfixnum_to_V(element)) < bit(8))) {
        TheSbvector(datenvektor)->data[index] = wert;
        return datenvektor;
      }
    }
      break;
    case Array_type_sb16vector: {
      var uintV wert;
      if (posfixnump(element) && ((wert = posfixnum_to_V(element)) < bit(16))) {
        ((uint16*)&TheSbvector(datenvektor)->data[0])[index] = wert;
        return datenvektor;
      }
    }
      break;
    case Array_type_sb32vector:
      ((uint32*)&TheSbvector(datenvektor)->data[0])[index] = I_to_UL(element); /* poss. error-message does I_to_UL */
      return datenvektor;
    #ifdef TYPECODES
    case_sstring:
      if (sstring_immutable(TheSstring(datenvektor)))
        fehler_sstring_immutable(datenvektor);
      #ifdef HAVE_SMALL_SSTRING
      switch (sstring_eltype(TheSstring(datenvektor))) {
        case Sstringtype_8Bit: goto case_s8string;
        case Sstringtype_16Bit: goto case_s16string;
        case Sstringtype_32Bit: goto case_s32string;
        default: NOTREACHED;
      }
      #else
      goto case_s32string;
      #endif
    #else
    case Rectype_Imm_S8string:
    case Rectype_Imm_S16string:
    case Rectype_Imm_S32string: /* immutable Simple-String */
      fehler_sstring_immutable(datenvektor);
    #ifdef HAVE_SMALL_SSTRING
    case Rectype_S8string: /* mutable Simple-String */
      goto case_s8string;
    case Rectype_S16string: /* mutable Simple-String */
      goto case_s16string;
    case Rectype_S32string: /* mutable Simple-String */
      goto case_s32string;
    #else
    case Rectype_S8string: case Rectype_S16string: case Rectype_S32string:
      goto case_s32string;
    #endif
    #endif
    #ifdef HAVE_SMALL_SSTRING
    case_s8string:
      if (charp(element)) {
        if (char_int(element) < cint8_limit)
          TheS8string(datenvektor)->data[index] = char_int(element);
        else if (allowgc) {
          if (char_int(element) < cint16_limit) {
            datenvektor = reallocate_small_string(datenvektor,Sstringtype_16Bit);
            TheS16string(TheSistring(datenvektor)->data)->data[index] = char_int(element);
          } else {
            datenvektor = reallocate_small_string(datenvektor,Sstringtype_32Bit);
            TheS32string(TheSistring(datenvektor)->data)->data[index] = char_int(element);
          }
        } else
          NOTREACHED;
        return datenvektor;
      }
      break;
    case_s16string: /* mutable Simple-String */
      if (charp(element)) {
        if (char_int(element) < cint16_limit)
          TheS16string(datenvektor)->data[index] = char_int(element);
        else if (allowgc) {
          datenvektor = reallocate_small_string(datenvektor,Sstringtype_32Bit);
          TheS32string(TheSistring(datenvektor)->data)->data[index] = char_int(element);
        } else
          NOTREACHED;
        return datenvektor;
      }
      break;
    #endif
    case_s32string: /* mutable Simple-String */
      if (charp(element)) {
        TheS32string(datenvektor)->data[index] = char_int(element);
        return datenvektor;
      }
      break;
    case Array_type_snilvector: /* (VECTOR NIL) */
      break;
    default: NOTREACHED;
  }
  /* Object was of wrong type. */
  fehler_store(STACK_0,element);
}

LISPFUN(aref,seclass_read,1,0,rest,nokey,0,NIL)
{ /* (AREF array {subscript}), CLTL p. 290 */
  var object array = check_array(Before(rest_args_pointer)); /* fetch array */
  /* process subscripts and fetch data vector and index: */
  var uintL index;
  var object datenvektor =
    subscripts_to_index(array,rest_args_pointer,argcount, &index);
  /* fetch element of the data vector: */
  VALUES1(storagevector_aref(datenvektor,index));
  skipSTACK(1);
}

LISPFUN(store,seclass_default,2,0,rest,nokey,0,NIL)
{ /* (SYS::STORE array {subscript} object)
   = (SETF (AREF array {subscript}) object), CLTL p. 291 */
  rest_args_pointer skipSTACKop 1; /* pointer to first Subscript */
  var object array = Before(rest_args_pointer) = check_array(Before(rest_args_pointer)); /* fetch array */
  var object element = popSTACK();
  /* process subscripts and fetch data vector and index: */
  var uintL index;
  var object datenvektor =
    subscripts_to_index(array,rest_args_pointer,argcount, &index);
  /* store element in the data vector: */
  pushSTACK(STACK_0); STACK_1 = element;
  /* Stack layout: element, array. */
  storagevector_store(datenvektor,index,element,true);
  VALUES1(STACK_1);
  skipSTACK(2);
}

LISPFUNNR(svref,2)
{ /* (SVREF simple-vector index), CLTL p. 291 */
  /* check simple-vector: */
  if (!simple_vector_p(STACK_1))
    fehler_kein_svector(TheSubr(subr_self)->name,STACK_1);
  /* check index: */
  var uintL index = test_index(STACK_1);
  /* fetch element: */
  VALUES1(TheSvector(STACK_1)->data[index]);
  skipSTACK(2);
}

LISPFUNN(svstore,3)
{ /* (SYS::SVSTORE simple-vector index element)
   = (SETF (SVREF simple-vector index) element), CLTL p. 291 */
  var object element = popSTACK();
  /* check simple-vector: */
  if (!simple_vector_p(STACK_1))
    fehler_kein_svector(TheSubr(subr_self)->name,STACK_1);
  /* check index: */
  var uintL index = test_index(STACK_1);
  /* store element: */
  TheSvector(STACK_1)->data[index] = element;
  VALUES1(element);
  skipSTACK(2);
}

LISPFUNN(psvstore,3)
{ /* (SYS::%SVSTORE element simple-vector index)
   = (SETF (SVREF simple-vector index) element) */
  /* check simple-vector: */
  if (!simple_vector_p(STACK_1))
    fehler_kein_svector(TheSubr(subr_self)->name,STACK_1);
  /* check index: */
  var uintL index = test_index(STACK_1);
  /* store element: */
  VALUES1(TheSvector(STACK_1)->data[index] = STACK_2);
  skipSTACK(3);
}

LISPFUNNR(row_major_aref,2)
{ /* (ROW-MAJOR-AREF array index), CLtL2 p. 450 */
  var object array = check_array(STACK_1);
  /* check index: */
  if (!posfixnump(STACK_0))
    fehler_index_type(array);
  var uintV indexv = posfixnum_to_V(STACK_0);
  if (indexv >= array_total_size(array)) /* index must be smaller than size */
    fehler_index_range(array,array_total_size(array));
  var uintL index = indexv;
  if (array_simplep(array)) {
    sstring_un_realloc(array);
  } else {
    array = iarray_displace(array,&index);
  }
  VALUES1(storagevector_aref(array,index));
  skipSTACK(2);
}

LISPFUNN(row_major_store,3)
{ /* (SYS::ROW-MAJOR-STORE array index element)
   = (SETF (ROW-MAJOR-AREF array index) element), CLtL2 p. 450 */
  var object array = STACK_2 = check_array(STACK_2);
  var object element = popSTACK();
  /* check index: */
  if (!posfixnump(STACK_0))
    fehler_index_type(array);
  var uintV indexv = posfixnum_to_V(STACK_0);
  if (indexv >= array_total_size(array)) /* index must be smaller than size */
    fehler_index_range(array,array_total_size(array));
  var uintL index = indexv;
  STACK_0 = array; STACK_1 = element;
  /* Stack layout: element, array. */
  if (array_simplep(array)) {
    sstring_un_realloc(array);
  } else {
    array = iarray_displace(array,&index);
  }
  storagevector_store(array,index,element,true);
  VALUES1(STACK_1);
  skipSTACK(2);
}

/* ======================================================================== */
/* Information about an array */

/* return Atype for the given array
 exported for the sake of modules */
global uintBWL array_atype (object array)
{
  switch (Array_type(array)) {
    case Array_type_mdarray: /* general array -> look at Arrayflags */
    case Array_type_string: /* STRING or (VECTOR NIL) */
      return Iarray_flags(array) & arrayflags_atype_mask;
    case Array_type_sbvector:
    case Array_type_sb2vector:
    case Array_type_sb4vector:
    case Array_type_sb8vector:
    case Array_type_sb16vector:
    case Array_type_sb32vector:
      return sbNvector_atype(array);
    case Array_type_bvector:
    case Array_type_b2vector:
    case Array_type_b4vector:
    case Array_type_b8vector:
    case Array_type_b16vector:
    case Array_type_b32vector:
      return bNvector_atype(array);
    case Array_type_sstring:
      return Atype_Char;
    case Array_type_svector:
    case Array_type_vector: /* [GENERAL-]VECTOR */
      return Atype_T;
    #if 0 /* not necessary */
    case Array_type_snilvector:
      return Atype_NIL;
    #endif
    default: NOTREACHED;
  }
}

/* Function: Returns the element-type of an array.
 array_element_type(array)
 > array: an array
 < result: element-type, one of the symbols T, BIT, CHARACTER, or a list
 can trigger GC */
global maygc object array_element_type (object array) {
  var uintBWL atype = array_atype(array);
  switch (atype) {
    case Atype_T:           return S(t);         /* T */
    case Atype_Bit:         return S(bit);       /* BIT */
    case Atype_Char:        return S(character); /* CHARACTER */
    case Atype_2Bit:        /* (UNSIGNED-BYTE 2) */
    case Atype_4Bit:        /* (UNSIGNED-BYTE 4) */
    case Atype_8Bit:        /* (UNSIGNED-BYTE 8) */
    case Atype_16Bit:       /* (UNSIGNED-BYTE 16) */
    case Atype_32Bit:       /* (UNSIGNED-BYTE 32) */
      break;
    case Atype_NIL:         return S(nil); /* (VECTOR NIL) -> NIL */
    default: NOTREACHED;
  }
  pushSTACK(S(unsigned_byte));
  pushSTACK(fixnum(bit(atype)));
  return listof(2);
}

LISPFUNNF(array_element_type,1)
{ /* (ARRAY-ELEMENT-TYPE array), CLTL p. 291 */
  var object array = check_array(popSTACK());
  VALUES1(array_element_type(array));
}

LISPFUNNF(array_rank,1)
{ /* (ARRAY-RANK array), CLTL p. 292 */
  var object array = check_array(popSTACK());
  VALUES1(arrayrank(array));
}

LISPFUNNR(array_dimension,2)
{ /* (ARRAY-DIMENSION array axis-number), CLTL p. 292 */
  var object array = check_array(STACK_1);
  var object axis_number = STACK_0;
  skipSTACK(2);
  if (array_simplep(array)) {
    /* simple vector: axis-number must be =0, value is then the length. */
    if (eq(axis_number,Fixnum_0)) {
      if (simple_string_p(array)) {
        sstring_un_realloc(array);
        VALUES1(fixnum(Sstring_length(array)));
      } else {
        VALUES1(fixnum(Sarray_length(array)));
      }
      return;
    } else
      goto fehler_axis;
  } else { /* non-simple array */
    if (posfixnump(axis_number)) { /* axis-number must be a fixnum >=0, */
      var uintV axis = posfixnum_to_V(axis_number);
      if (axis < (uintL)Iarray_rank(array)) { /* and <rank */
        var uintL* dimptr = &TheIarray(array)->dims[0];
        if (Iarray_flags(array) & bit(arrayflags_dispoffset_bit))
          dimptr++; /* poss. skip displaced-offset */
        VALUES1(fixnum(dimptr[axis])); return;
      } else
        goto fehler_axis;
    } else
      goto fehler_axis;
  }
 fehler_axis:
  pushSTACK(array);
  pushSTACK(axis_number); /* TYPE-ERROR slot DATUM */
  { /* TYPE-ERROR slot EXPECTED-TYPE */
    var object tmp;
    pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(arrayrank(array));
    tmp = listof(1); pushSTACK(tmp); tmp = listof(3); pushSTACK(tmp);
  }
  pushSTACK(STACK_2); /* array */
  pushSTACK(STACK_2); /* axis_number */
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: ~S is not an nonnegative integer less than the rank of ~S"));
}

/* Returns the rank of an array.
 array_rank(array)
 > array: an array
 < uintL result: its rank = number of dimensions
 exists for the sake of modules */
global uintL array_rank (object array) {
  if (mdarrayp(array))
    /* multi-dimensional array */
    return Iarray_rank(array);
  else
    /* vector has rank 1 */
    return 1;
}

/* Returns the dimensions of an array.
 get_array_dimensions(array,rank,&dimensions[]);
 > array: an array
 > uintL rank: = array_rank(array)
 > uintL dimensions[0..rank-1]: room for rank dimensions
 < uintL dimensions[0..rank-1]: the array's dimensions
 exists for the sake of modules */
global void get_array_dimensions (object array, uintL rank, uintL* dimensions) {
  if (array_simplep(array)) {
    /* simple vector */
    ASSERT(rank == 1);
    if (simple_string_p(array)) {
      sstring_un_realloc(array);
      dimensions[0] = Sstring_length(array);
    } else
      dimensions[0] = Sarray_length(array);
  } else {
    ASSERT(rank == Iarray_rank(array));
    if (rank > 0) {
      var uintL* dimptr = &TheIarray(array)->dims[0];
      if (Iarray_flags(array) & bit(arrayflags_dispoffset_bit))
        dimptr++; /* poss. skip displaced-offset */
      dotimespL(rank,rank, { *dimensions++ = *dimptr++; });
    }
  }
}

/* Function: Returns the list of dimensions of an array.
 array_dimensions(array)
 > array: an array
 < result: list of its dimensions
 can trigger GC */
global maygc object array_dimensions (object array) {
  if (array_simplep(array)) { /* simple vector, form (LIST length) */
    var object len; /* length as fixnum (non endangered by GC) */
    if (simple_string_p(array)) {
      sstring_un_realloc(array);
      len = fixnum(Sstring_length(array));
    } else {
      len = fixnum(Sarray_length(array));
    }
    var object new_cons = allocate_cons();
    Car(new_cons) = len; Cdr(new_cons) = NIL;
    return new_cons;
  } else { /* non-simple array: */
    /* All dimensions as fixnums on the STACK, then turn it into a list. */
    var uintC rank = Iarray_rank(array);
    if (rank > 0) {
      var uintL* dimptr = &TheIarray(array)->dims[0];
      if (Iarray_flags(array) & bit(arrayflags_dispoffset_bit))
        dimptr++; /* poss. skip displaced-offset */
      get_space_on_STACK(sizeof(gcv_object_t)*(uintL)rank); /* check STACK */
      {
        var uintC count;
        dotimespC(count,rank, { /* next dimension as fixnum into the stack: */
          pushSTACK(fixnum(*dimptr++));
        });
      }
    }
    return listof(rank); /* form list */
  }
}

LISPFUNNR(array_dimensions,1)
{ /* (ARRAY-DIMENSIONS array), CLTL p. 292 */
  var object array = check_array(popSTACK());
  VALUES1(array_dimensions(array));
}

/* Function: Returns the dimensions of an array and their partial products.
 iarray_dims_sizes(array,&dims_sizes);
 > array: indirect array of rank r
 > struct { uintL dim; uintL dimprod; } dims_sizes[r]: room for the result
 < for i=1,...r:  dims_sizes[r-i] = { Dim_i, Dim_i * ... * Dim_r } */
global void iarray_dims_sizes (object array, array_dim_size_t* dims_sizes) {
  var uintC r = Iarray_rank(array); /* rank */
  if (r > 0) {
    var const uintL* dimptr = &TheIarray(array)->dims[0];
    if (Iarray_flags(array) & bit(arrayflags_dispoffset_bit))
      dimptr++; /* poss. skip displaced-offset */
    dimptr = &dimptr[(uintL)r]; /* pointer behind the dimensions */
    var uintL produkt = 1;
    dotimespC(r,r, { /* loop over the r dimensions from behind */
      var uintL dim = *--dimptr; /* next dimension */
      produkt = mulu32_unchecked(produkt,dim); /* multiply to the product */
      /* There will be no overflow, because this is
         < product of the dimensions so far
         <= product of all dimensions < arraysize_limit <= 2^32 .
         (exception: if a dimension of smaller number is =0 .
         But then the current product is anyway irrelevant, because
         each loop over this dimension is an empty loop.) */
      dims_sizes->dim = dim; dims_sizes->dimprod = produkt;
      dims_sizes++;
    });
  }
}

LISPFUNNR(array_total_size,1)
{ /* (ARRAY-TOTAL-SIZE array), CLTL p. 292 */
  var object array = check_array(popSTACK());
  VALUES1(fixnum(array_total_size(array)));
}

LISPFUN(array_in_bounds_p,seclass_read,1,0,rest,nokey,0,NIL)
{ /* (ARRAY-IN-BOUNDS-P array {subscript}), CLTL p. 292 */
  var gcv_object_t* argptr = rest_args_pointer;
  var object array = check_array(BEFORE(rest_args_pointer)); /* fetch array */
  if (array_simplep(array)) { /* simple vector is treated separately: */
    /* check number of subscripts: */
    if (argcount != 1) /* should be = 1 */
      fehler_subscript_anz(array,argcount);
    /* check subscript itself: */
    var object subscriptobj = STACK_0; /* subscript as object */
    if (!integerp(subscriptobj)) /* must be an integer */
      fehler_index_type(array);
    /* subscript must be fixnum>=0 , */
    /* subscript as uintL must be smaller than length: */
    if (!posfixnump(subscriptobj)) goto no;
    if (simple_string_p(array)) {
      sstring_un_realloc(array);
      if (!(posfixnum_to_V(subscriptobj) < Sstring_length(array))) goto no;
    } else {
      if (!(posfixnum_to_V(subscriptobj) < Sarray_length(array))) goto no;
    }
    goto yes;
  } else { /* non-simple array */
    /* check number of subscripts: */
    if (!(argcount == Iarray_rank(array))) /* should be = rank */
      fehler_subscript_anz(array,argcount);
    /* check subscripts itself: */
    if (argcount > 0) {
      var uintL* dimptr = &TheIarray(array)->dims[0];
      if (Iarray_flags(array) & bit(arrayflags_dispoffset_bit))
        dimptr++; /* poss. skip displaced-offset */
      var uintC count;
      dotimespC(count,argcount, {
        var object subscriptobj = NEXT(argptr); /* subscript as object */
        if (!integerp(subscriptobj)) { /* must be an integer */
          Next(rest_args_pointer) = array;
          fehler_subscript_type(argcount);
        }
        /* subscript must be fixnum>=0 , and subscript as uintL
           must be smaller than the corresponding dimension: */
        if (!( posfixnump(subscriptobj)
               && (posfixnum_to_V(subscriptobj) < *dimptr++) ))
          goto no;
      });
    }
    goto yes;
  }
 yes:
  VALUES1(T); set_args_end_pointer(rest_args_pointer); return;
 no:
  VALUES1(NIL); set_args_end_pointer(rest_args_pointer); return;
}

LISPFUN(array_row_major_index,seclass_read,1,0,rest,nokey,0,NIL)
{ /* (ARRAY-ROW-MAJOR-INDEX array {subscript}), CLTL p. 293 */
  var object array = check_array(Before(rest_args_pointer)); /* fetch array */
  var uintL index;
  if (array_simplep(array)) { /* simple vector is treated separately: */
    /* check number of subscripts: */
    if (argcount != 1) /* should be = 1 */
      fehler_subscript_anz(array,argcount);
    sstring_un_realloc(array);
    /* check subscript itself: */
    test_index(array);
    VALUES1(popSTACK()); /* Index = Row-Major-Index = Subscript */
    skipSTACK(1);
  } else { /* non-simple array */
    /* check subscripts, calculate row-major-index, clean up STACK: */
    index = test_subscripts(array,rest_args_pointer,argcount);
    /* return index as fixnum: */
    VALUES1(fixnum(index));
    skipSTACK(1);
  }
}

LISPFUNNF(adjustable_array_p,1)
{ /* (ADJUSTABLE-ARRAY-P array), CLTL p. 293 */
  var object array = check_array(popSTACK()); /* fetch argument */
  VALUES_IF(!array_simplep(array)
            && (Iarray_flags(array) & bit(arrayflags_adjustable_bit)));
}

LISPFUNN(array_displacement,1)
{ /* (ARRAY-DISPLACEMENT array), CLHS */
  var object array = check_array(popSTACK()); /* fetch argument */
  if (!array_simplep(array)
      && (Iarray_flags(array) & bit(arrayflags_displaced_bit))) {
    VALUES2(TheIarray(array)->data, /* next array */
            fixnum(TheIarray(array)->dims[0])); /* displaced offset */
  } else {
    VALUES2(NIL, Fixnum_0);
  }
}

/* ======================================================================== */
/* Bit arrays and bit vectors */

/* error: not a bit array
 fehler_bit_array()
 > array: array, that is not a bit-array */
nonreturning_function(local, fehler_bit_array, (object array)) {
  pushSTACK(array); /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_array_bit)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(array);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: ~S is not an array of bits"));
}

LISPFUN(bit,seclass_read,1,0,rest,nokey,0,NIL)
{ /* (BIT bit-array {subscript}), CLTL p. 293 */
  var object array = check_array(Before(rest_args_pointer)); /* fetch array */
  /* process subscripts and fetch data vector and index: */
  var uintL index;
  var object datenvektor =
    subscripts_to_index(array,rest_args_pointer,argcount, &index);
  if (!simple_bit_vector_p(Atype_Bit,datenvektor))
    fehler_bit_array(array);
  /* data vector is a simple-bit-vector. Fetch element of the data vector: */
  VALUES1(( sbvector_btst(datenvektor,index) ? Fixnum_1 : Fixnum_0 ));
  skipSTACK(1);
}

LISPFUN(sbit,seclass_read,1,0,rest,nokey,0,NIL)
{ /* (SBIT bit-array {subscript}), CLTL p. 293 */
  var object array = check_array(Before(rest_args_pointer)); /* fetch array */
  /* process subscripts and fetch data vector and index: */
  var uintL index;
  var object datenvektor =
    subscripts_to_index(array,rest_args_pointer,argcount, &index);
  if (!simple_bit_vector_p(Atype_Bit,datenvektor))
    fehler_bit_array(array);
  /* data vector is a simple-bit-vector. Fetch element of the data vector: */
  VALUES1(( sbvector_btst(datenvektor,index) ? Fixnum_1 : Fixnum_0 ));
  skipSTACK(1);
}

/* For subroutines for bit vectors:
 We work with bit-blocks with bitpack bits.
 uint_bitpack_t is an unsigned integer with bitpack bits.
 uint_2bitpack_t is an unsigned integer with 2*bitpack bits.
 R_bitpack(x) returns the right (lower) half of a uint_2bitpack_t.
 L_bitpack(x) returns the left  (upper) half of a uint_2bitpack_t.
 LR_2bitpack(x,y) returns for x,y the uint_2bitpack_t concatenated from
                   the left half x and the right half y.
 Use LR_0_bitpack(y) if x=0, LR_bitpack_0(x) if y=0. */
#if defined(WIDE_HARD) && BIG_ENDIAN_P && (varobject_alignment%4 == 0)
 /* On big-endian-64-bit-machines we can work with 32 bit at a
    time (so long as varobject_alignment is divisible by 4 bytes): */
  #define bitpack  32
  #define uint_bitpack_t  uint32
  #define uint_2bitpack_t  uint64
  #define R_bitpack(x)  ((uint32)(uint64)(x))
  #define L_bitpack(x)  ((uint32)((uint64)(x)>>32))
  #define LR_2bitpack(x,y)  (((uint64)(uint32)(x)<<32)|(uint64)(uint32)(y))
  #define LR_0_bitpack(y)  ((uint64)(uint32)(y))
  #define LR_bitpack_0(x)  ((uint64)(uint32)(x)<<32)
#elif BIG_ENDIAN_P && (varobject_alignment%2 == 0)
 /* On big-endian-machines we can work with 16 bit at a time
    (so long as varobject_alignment is divisible by 2 bytes): */
  #define bitpack  16
  #define uint_bitpack_t  uint16
  #define uint_2bitpack_t  uint32
  #define R_bitpack(x)  low16(x)
  #define L_bitpack(x)  high16(x)
  #define LR_2bitpack(x,y)  highlow32(x,y)
  #define LR_0_bitpack(y)  ((uint32)(uint16)(y))
  #define LR_bitpack_0(x)  highlow32_0(x)
#else
 /* Otherwise we can take only 8 bits at a time: */
  #define bitpack  8
  #define uint_bitpack_t  uint8
  #define uint_2bitpack_t  uint16
  #define R_bitpack(x)  ((uint_bitpack_t)(uint_2bitpack_t)(x))
  #define L_bitpack(x)  ((uint_bitpack_t)((uint_2bitpack_t)(x) >> bitpack))
  #define LR_2bitpack(x,y)  \
    (((uint_2bitpack_t)(uint_bitpack_t)(x) << bitpack)        \
     | (uint_2bitpack_t)(uint_bitpack_t)(y))
  #define LR_0_bitpack(y)  LR_2bitpack(0,y)
  #define LR_bitpack_0(x)  LR_2bitpack(x,0)
#endif

/* Function: Compares two slices of simple-bit-vectors.
 bit_compare(array1,index1,array2,index2,count)
 > array1: first simple-bit-vector
 > index1: absolute index into array1
 > array2: second simple-bit-vector
 > index2: absolute index into array2
 > count: number of bits to be compared, > 0
 < result: true, if both slices are the same, bit for bit, else false. */
global bool bit_compare (object array1, uintL index1,
                         object array2, uintL index2,
                         uintL bitcount)
{
  var const uint_bitpack_t* ptr1 = &((uint_bitpack_t*)(&TheSbvector(array1)->data[0]))[index1/bitpack];
  var const uint_bitpack_t* ptr2 = &((uint_bitpack_t*)(&TheSbvector(array2)->data[0]))[index2/bitpack];
  /* ptr1 points to the first word of the 1st bit-array.
     ptr2 points to the first word of the 2nd bit-array. */
  index1 = index1 % bitpack; /* bit-offset in the 1st bit-array */
  index2 = index2 % bitpack; /* bit-offset in the 2nd bit-array */
  if (index1 == index2) { /* process first word: */
    if (index1 != 0) {
      var uintL count1 = bitpack - index1;
      if (count1 >= bitcount) {
        /* compare bits bitpack-index1-1..bitpack-index1-bitcount
           in *ptr1 und *ptr2. */
        return (((*ptr1 ^ *ptr2) & (bit(count1)-bit(count1-bitcount))) == 0);
      }
      if (((*ptr1 ^ *ptr2) & (bit(count1)-1)) != 0)
        return false;
      ptr1++;
      ptr2++;
      bitcount -= count1; /* still > 0 */
    }
    /* Now we can assume index1 = index2 = 0 . */
    var uintL bitpackcount = bitcount/bitpack;
    /* bitpackcount = number of entire words */
    var uintL bitcount_rest = bitcount % bitpack;
    /* bitcount_rest = number of remaining bits */
    /* simple loop, because all bit-offsets in word are =0 : */
    dotimesL(bitpackcount,bitpackcount, {
      if (*ptr1++ != *ptr2++)
        return false;
    });
    /* bitcount_rest = number of bits still to be compared */
    if (bitcount_rest!=0) {
      /* compare last word: */
      if (!(( (*ptr1 ^ *ptr2)
              & /* set bitmask with bits bitpack-1..bitpack-bitcount_rest */
              ~( (uint_bitpack_t)(bitm(bitpack)-1) >> bitcount_rest)) ==0))
        return false;
    }
    return true;
  } else {
    /* complicated loop: */
    var uintL bitpackcount = bitcount/bitpack;
    /* bitpackcount = number of entire words */
    var uintL bitcount_rest = bitcount % bitpack;
    /* bitcount_rest = number of remaining bits
       We distinguish three cases in order to avoid a memory overrun bug.
       The tighter loops are just an added benefit for speed. */
    if (index1 == 0) {
      /* index1 = 0, index2 > 0. */
      var uint_2bitpack_t carry2 = LR_bitpack_0((*ptr2++) << index2);
      /* carry2 has in its upper bitpack-index2 bits
         (bits 2*bitpack-1..bitpack+index2)
         the affected bits of the 1st word of the 2nd array, else nulls. */
      dotimesL(bitpackcount,bitpackcount, {
        /* comparison loop (wordwise):
           after n>=0 loop runs ptr1  is advanced by n,
           nd ptr2 is advanced by n+1 words, which means pointer to
           the next word of the 1st resp. 2nd array,
           bitpackcount = number of entire words to be combined - n,
           carry2 = carry from 2nd array
                    (in the bitpack-index2 upper bits, else null). */
        if (!(*ptr1++
              ==
              ( carry2 |=
                LR_0_bitpack(*ptr2++) /* read next word of the 2nd array */
                << index2, /* add to carry2 */
                L_bitpack(carry2)))) /* and use the left word from it */
          return false;
        carry2 = LR_bitpack_0(R_bitpack(carry2)); /* carry2 := right word of carry2 */
      });
      /* still bitcount_rest bits to compare: */
      if (bitcount_rest!=0) { /* compare last word: */
        if (!(((*ptr1++
                ^
                (carry2 |=
                 LR_0_bitpack(*ptr2++) /* read the next word of the 2nd array */
                 << index2, /* add to carry2 */
                 L_bitpack(carry2))) /* and use the left word from it */
               & /* set bitmask with bits bitpack-1..bitpack-bitcount_rest */
               ~( (uint_bitpack_t)(bitm(bitpack)-1) >> bitcount_rest)) ==0))
          return false;
      }
      return true;
    } else if (index2 == 0) { /* index1 > 0, index2 = 0. */
      var uint_2bitpack_t carry1 = LR_bitpack_0((*ptr1++) << index1);
      /* carry1 has in its upper bitpack-index1 bits
         (bits 2*bitpack-1..bitpack+index1)
         the affected bits of the 1st word of the 1st array, else nulls. */
      dotimesL(bitpackcount,bitpackcount, {
        /* comparison loop (wordwise):
           after n>=0 loop runs, ptr1 is advanced by n+1,
           and ptr2 is advanced by n words, which means pointer to
           the next word to be read of the 1st resp. 2nd array,
           bitpackcount = number of entire words to be combined
           carry1 = carry from 1st array
                    (in the bitpack-index1 upper bits, else null). */
        if (!((carry1 |=
               LR_0_bitpack(*ptr1++) /* read the next word of the 1st array */
               << index1, /* add to carry1 */
               L_bitpack(carry1)) /* and use the left word from it */
              ==
              *ptr2++))
          return false;
        carry1 = LR_bitpack_0(R_bitpack(carry1)); /* carry1 := right word of carry1 */
      });
      /* Still bitcount_rest bits to compare: */
      if (bitcount_rest!=0) {
        /* compare last word: */
        if (!((((carry1 |=
                 LR_0_bitpack(*ptr1++) /* read the next word of 1st array */
                 << index1, /* add to carry1 */
                 L_bitpack(carry1)) /* and use the left word from it */
                 ^
                *ptr2++)
               & /* set bitmask with bits bitpack-1..bitpack-bitcount_rest */
               ~( (uint_bitpack_t)(bitm(bitpack)-1) >> bitcount_rest)) ==0))
          return false;
      }
      return true;
    } else {
      var uint_2bitpack_t carry1 = LR_bitpack_0((*ptr1++) << index1);
      /* carry1 has in its upper bitpack-index1 bits
         (bits 2*bitpack-1..bitpack+index1)
         the affected bits of the 1st word of the 1st array, else nulls. */
      var uint_2bitpack_t carry2 = LR_bitpack_0((*ptr2++) << index2);
      /* carry2 has in its upper bitpack-index2 bits
         (bits 2*bitpack-1..bitpack+index2)
         the affected bits of the 1st word of the 2nd array, else nulls. */
      dotimesL(bitpackcount,bitpackcount, {
        /* comparison loop (wordwise):
           After n>=0 loop runs ptr1 and ptr2 are advanced
           by n+1 words, which means pointer to the
           next word to be read of the 1st resp. 2nd array,
           bitpackcount = number of entire words to be combined - n,
           carry1 = carry from 1st array
                    (in the bitpack-index1 upper bits, else null),
           carry2 = carryfrom 2nd array
                    (in the bitpack-index2 upper bits, else null). */
        if (!((carry1 |=
               LR_0_bitpack(*ptr1++) /* read next word of the 1st array */
               << index1, /* add to carry1 */
               L_bitpack(carry1)) /* and use the left word from it */
               ==
              ( carry2 |=
                LR_0_bitpack(*ptr2++) /* read next word of the 2nd array */
                << index2, /* add to carry2 */
                L_bitpack(carry2)))) /* and use the left word from it */
          return false;
        carry1 = LR_bitpack_0(R_bitpack(carry1)); /* carry1 := right word of carry1 */
        carry2 = LR_bitpack_0(R_bitpack(carry2)); /* carry2 := right word of carry2 */
      });
      /* still bitcount_rest bits to compare: */
      if (bitcount_rest!=0) { /* compare last word: */
        if (!((((carry1 |=
                 LR_0_bitpack(*ptr1++) /* read next word of 1st array */
                 << index1, /* add to carry1 */
                 L_bitpack(carry1)) /* and use the left word from it */
                 ^
                ( carry2 |=
                  LR_0_bitpack(*ptr2++) /* read next word of the 2nd array */
                  << index2, /* add to carry2 */
                  L_bitpack(carry2))) /* and use the left word from it */
               & /* set bitmask with bits bitpack-1..bitpack-bitcount_rest */
               ~( (uint_bitpack_t)(bitm(bitpack)-1) >> bitcount_rest)) ==0))
          return false;
      }
      return true;
    }
  }
}

/* Function: Copies a slice of a simple-bit-vector into another
 simple-bit-vector.
 bit_copy(array1,index1,array2,index2,count);
 > array1: source simple-bit-vector
 > index1: absolute index into array1
 > array2: destination simple-bit-vector
 > index2: absolute index into array2
 > count: number of bits to be copied, > 0 */
local void bit_copy (object array1, uintL index1,
                     object array2, uintL index2,
                     uintL bitcount)
{
  var const uint_bitpack_t* ptr1 = &((uint_bitpack_t*)(&TheSbvector(array1)->data[0]))[index1/bitpack];
  var uint_bitpack_t* ptr2 = &((uint_bitpack_t*)(&TheSbvector(array2)->data[0]))[index2/bitpack];
  /* ptr1 point to the first affected word in array1
     ptr2 point to the first affected word in array2 */
  index1 = index1 % bitpack; /* bit-offset in array1 */
  index2 = index2 % bitpack; /* bit-offset in array2 */
  if (index1 == index2) {
    /* Treat the first word. */
    if (index1 != 0) {
      var uintL count1 = bitpack - index1;
      if (count1 >= bitcount) {
        /* copy bits bitpack-index1-1..bitpack-index1-bitcount
           from *ptr1 to *ptr2 */
        *ptr2 ^= (bit(count1)-bit(count1-bitcount)) & (*ptr2 ^ *ptr1);
        return;
      }
      *ptr2 ^= (bit(count1)-1) & (*ptr2 ^ *ptr1);
      ptr1++;
      ptr2++;
      bitcount -= count1; /* still > 0 */
    }
    /* We can now assume index1 = index2 = 0. */
    var uintL bitpackcount = bitcount/bitpack;
    /* bitpackcount = number of complete words */
    var uintL bitcount_rest = bitcount % bitpack;
    /* bitcount_rest = number of remaining bits */
    /* simple loop, since all bit offsets are 0. */
    dotimesL(bitpackcount,bitpackcount, {
      *ptr2++ = *ptr1++;
    });
    if (bitcount_rest!=0)
      *ptr2 ^= ~( (uint_bitpack_t)(bitm(bitpack)-1) >> bitcount_rest) & (*ptr2 ^ *ptr1);
  } else {
    var uintL bitpackcount = bitcount/bitpack;
    /* bitpackcount = number of complete words */
    var uintL bitcount_rest = bitcount % bitpack;
    /* bitcount_rest = number of remaining bits */
    var uint_2bitpack_t carry2 =
      LR_bitpack_0( ( ~ ( (uint_bitpack_t)(bitm(bitpack)-1) >> index2) ) & *ptr2 );
    /* The upper index2 bits of carry2 are exactly those bits of *ptr2
       which must not be modified.
       We distinguish two cases in order to avoid a memory overrun bug.
       The tighter loop is just an added benefit for speed. */
    if (index1 == 0) {
      loop {
        /* After n>=0 rounds ptr1 has advanced by n words, i.e. it points
           to the next word to be read, and ptr2 has advanced by n words, i.e.
           it points to the next word to be written. bitpackcount has been
           decremented by n. */
        carry2 |= LR_bitpack_0(*ptr1++) >> index2;
        if (bitpackcount==0)
          break;
        *ptr2++ = L_bitpack(carry2);
        carry2 = LR_bitpack_0(R_bitpack(carry2));
        bitpackcount--;
      }
    } else { /* index1 > 0. */
      var uint_2bitpack_t carry1 = LR_bitpack_0((*ptr1++) << index1);
      /* The upper bitpack-index1 bits of carry1 are the affected bits of
         the first word of array1. The other bits in carry1 are zero. */
      loop {
        /* After n>=0 rounds ptr1 has advanced by n+1 words, i.e. it points
           to the next word to be read, and ptr2 has advanced by n words, i.e.
           it points to the next word to be written. bitpackcount has been
           decremented by n. */
        var uint_bitpack_t temp =
          (carry1 |= LR_0_bitpack(*ptr1++) << index1, L_bitpack(carry1));
        carry1 = LR_bitpack_0(R_bitpack(carry1));
        carry2 |= LR_bitpack_0(temp) >> index2;
        if (bitpackcount==0)
          break;
        *ptr2++ = L_bitpack(carry2);
        carry2 = LR_bitpack_0(R_bitpack(carry2));
        bitpackcount--;
      }
    }
    /* Special handling for the last word (now containd in the bits
       2*bitpack-index2-1..bitpack-index2 of carry2): Only bitcount_rest
       bits must be stored in array2 */
    bitcount_rest = index2+bitcount_rest;
    var uint_bitpack_t last_carry;
    if (bitcount_rest>=bitpack) {
      *ptr2++ = L_bitpack(carry2);
      last_carry = R_bitpack(carry2);
      bitcount_rest -= bitpack;
    } else {
      last_carry = L_bitpack(carry2);
    }
    if (bitcount_rest!=0)
      *ptr2 ^= (*ptr2 ^ last_carry) & (~( (uint_bitpack_t)(bitm(bitpack)-1) >> bitcount_rest ));
  }
}

/* subroutine for bitvector-operations:
 bit_op(array1,index1,array2,index2,array3,index3,op,count);
 > array1: first bit-array,
 > index1: absolute index in array1
 > array2: second bit-array,
 > index2: absolute index in array2
 > array3: third bit-array,
 > index3: absoluter Index in array3
 > op: address of the operation
 > count: number of bits to combine, > 0
 bit_op_fun_t is a function that combines two bitpack-bit-words: */
typedef uint_bitpack_t bit_op_fun_t (uint_bitpack_t x, uint_bitpack_t y);
local void bit_op (object array1, uintL index1,
                   object array2, uintL index2,
                   object array3, uintL index3,
                   bit_op_fun_t* op, uintL bitcount)
{
  var const uint_bitpack_t* ptr1 = &((uint_bitpack_t*)(&TheSbvector(array1)->data[0]))[index1/bitpack];
  var const uint_bitpack_t* ptr2 = &((uint_bitpack_t*)(&TheSbvector(array2)->data[0]))[index2/bitpack];
  var uint_bitpack_t* ptr3 = &((uint_bitpack_t*)(&TheSbvector(array3)->data[0]))[index3/bitpack];
  /* ptr1 points to the first word of the 1st bit-array.
     ptr2 points to the first word of the 2nd bit-array.
     ptr3 points to the first word of the 3rd bit-array. */
  var uintL bitpackcount = bitcount/bitpack;
  /* bitpackcount = number of entire words */
  var uintL bitcount_rest = bitcount % bitpack;
  /* bitcount_rest = number of remaining bits */
  index1 = index1 % bitpack; /* bit-offset in the 1st bit-array */
  index2 = index2 % bitpack; /* bit-offset in the 2nd bit-array */
  index3 = index3 % bitpack; /* bit-offset in the 3rd bit-array */
  if ((index1==0) && (index2==0) && (index3==0)) {
    /* simple loop, since all bit offsets in word are =0. */
    dotimesL(bitpackcount,bitpackcount, {
      *ptr3++ = (*op)(*ptr1++,*ptr2++);
    });
    /* bitcount_rest = remaining bits to file */
    if (bitcount_rest!=0) {
      /* file last word: */
      var uint_bitpack_t temp = (*op)(*ptr1,*ptr2);
      *ptr3 =
        ( ~
          ( (uint_bitpack_t)(bitm(bitpack)-1) >> bitcount_rest)
          /* set bitmask with bits bitpack-bitcount_rest-1..0 */
          /* set bitmask with bits bitpack-1..bitpack-bitcount_rest */
          &
          (*ptr3 ^ temp)) /* bits to change */
        ^ *ptr3;
    }
  } else {
    /* complicated loop: */
    var uint_2bitpack_t carry3 =
      LR_bitpack_0(
                   (~
                    (
                     (uint_bitpack_t)(bitm(bitpack)-1) >> index3)
                     /* set bitmask with bits bitpack-index3-1..0 */
                    ) /* set bitmask with bits bitpack-1..bitpack-index3 */
                   & (*ptr3));
    /* carry3 has in its upper index3 bits (bits 2*bitpack-1..2*bitpack-index3)
       precisely the bits of *ptr3 that must not be changed.
       We distinguish four cases in order to avoid a memory overrun bug.
       The tighter loops are just an added benefit for speed. */
    if (index1 == 0) {
      if (index2 == 0) {
        /* index1 = 0, index2 = 0. */
        loop {
          /* combination loop (wordwise):
             After n>=0 loop runs ptr1 and ptr2 are advanced
             by n words, which means pointer to the
             next word to read from the 1st resp. 2nd array,
             ptr3 is advanced by n words, which means pointer to the
             next word to write from the 3rd array,
             bitpackcount = number of entire words - n,
             carry3 = carry of bits still to save
                      (in the index3 upper bits, else null). */
          var uint_bitpack_t temp =
            (*op)(*ptr1++,*ptr2++) ; /* combine both via *op */
          carry3 |= LR_bitpack_0(temp) >> index3;
          /* store the upper bitpack+index3 bits from carry3. */
          if (bitpackcount==0)
            break;
          *ptr3++ = L_bitpack(carry3); /* store bitpack bits */
          carry3 = LR_bitpack_0(R_bitpack(carry3)); /* and keep index3 bits for later. */
          bitpackcount--;
        }
      } else {
        /* index1 = 0, index2 > 0. */
        var uint_2bitpack_t carry2 = LR_bitpack_0((*ptr2++) << index2);
        /* carry2 has in its upper bitpack-index2 bits
           (bits 2*bitpack-1..bitpack+index2)
           the affected bits of the 1st word of the 2nd array, else nulls. */
        loop {
          /* combination loop (wordwise):
             After n>=0 loop runs ptr1 is advanced by n words
             and ptr2 has advanced by n+1 words, which means pointer to the
             next word to read from the 1st resp. 2nd array,
             ptr3 is advanced by n words, which means pointer to the
             next word to write from the 3rd array,
             bitpackcount = number of entire words - n,
             carry2 = carry from second array
             (in the bitpack-index2 upper bits, else Null),
             carry3 = carry of bits still to save
             (in the index3 upper bits, else null). */
          var uint_bitpack_t temp =
            (*op)(*ptr1++,
                  ( carry2 |=
                    LR_0_bitpack(*ptr2++) /* read next word of the 2nd array */
                    << index2, /* add to carry2 */
                    L_bitpack(carry2) /* and use the left word from it */
                    )) ; /* combine both via *op */
          carry2 = LR_bitpack_0(R_bitpack(carry2)); /* carry2 := right word of carry2 */
          carry3 |= LR_bitpack_0(temp) >> index3;
          /* save the upper bitpack+index3 bits from carry3. */
          if (bitpackcount==0)
            break;
          *ptr3++ = L_bitpack(carry3); /* save bitpack bits */
          carry3 = LR_bitpack_0(R_bitpack(carry3)); /* and keep index3 bits for later. */
          bitpackcount--;
        }
      }
    } else {
      if (index2 == 0) {
        /* index1 > 0, index2 = 0. */
        var uint_2bitpack_t carry1 = LR_bitpack_0((*ptr1++) << index1);
        /* carry1 has in its upper bitpack-index1 bits
           (bits 2*bitpack-1..bitpack+index1)
           the affected bits of the 1st word of the 1st array, else nulls. */
        loop {
          /* combination loop (wordwise):
             After n>=0 loop runs ptr1 is advanced by n+1 words
             and ptr2 has advanced by n words, which means pointer to the
             next word to read from the 1st resp. 2nd array,
             ptr3 is advanced by n words, which means pointer to the
             next word to write from the 3rd array,
             bitpackcount = number of entire words - n,
             carry1 = carry from first array
                      (in the bitpack-index1 upper bits, else Null),
             carry3 = carry of bits still to save
                      (in the index3 upper bits, else null). */
          var uint_bitpack_t temp =
            (*op)(
                  ( carry1 |=
                    LR_0_bitpack(*ptr1++) /* read the next word of 1st array */
                    << index1, /* add to carry1 */
                    L_bitpack(carry1) /* and use the left word from it */
                    ),
                  *ptr2++) ; /* combine both via *op */
          carry1 = LR_bitpack_0(R_bitpack(carry1)); /* carry1 := right word of carry1 */
          carry3 |= LR_bitpack_0(temp) >> index3;
          /* store the upper bitpack+index3 bits of carry3. */
          if (bitpackcount==0)
            break;
          *ptr3++ = L_bitpack(carry3); /* store bitpack bits */
          carry3 = LR_bitpack_0(R_bitpack(carry3)); /* and keep index3 bits for later. */
          bitpackcount--;
        }
      } else {
        /* index1 > 0, index2 > 0. */
        var uint_2bitpack_t carry1 = LR_bitpack_0((*ptr1++) << index1);
        /* carry1 has in its upper bitpack-index1 bits
           (bits 2*bitpack-1..bitpack+index1)
           the affected bits of the 1st word of the 1st array, else nulls. */
        var uint_2bitpack_t carry2 = LR_bitpack_0((*ptr2++) << index2);
        /* carry2 has in its upper bitpack-index2 bits
           (bits 2*bitpack-1..bitpack+index2)
           the affected bits of the 1st word of the 2nd array, else nulls. */
        loop {
          /* combination loop (wordwise):
             After n>=0 loop runs ptr1 and ptr2  are advanced
             by n+1 words, which means pointer to the
             next word to read from the 1st resp. 2nd array,
             ptr3 is advanced by n words, which means pointer to the
             next word to write from the 3rd array,
             bitpackcount = number of entire words - n,
             carry1 = carry from first array
                      (in the bitpack-index1 upper bits, else Null),
             carry2 = carry from second array
                      (in the bitpack-index2 upper bits, else Null),
             carry3 = carry of bits still to save
                      (in the index3 upper bits, else null). */
          var uint_bitpack_t temp =
            (*op)(
                  ( carry1 |=
                    LR_0_bitpack(*ptr1++) /* read next word of 1st array */
                    << index1, /* add to carry1 */
                    L_bitpack(carry1) /* and use the left word from it */
                    ),
                  ( carry2 |=
                    LR_0_bitpack(*ptr2++) /* read next word of 2nd array */
                    << index2, /* add to carry2 */
                    L_bitpack(carry2) /* and use the left word from it */
                    )
                  ) ; /* combine both via *op */
          carry1 = LR_bitpack_0(R_bitpack(carry1)); /* carry1 := right word of carry1 */
          carry2 = LR_bitpack_0(R_bitpack(carry2)); /* carry2 := right word of carry2 */
          carry3 |= LR_bitpack_0(temp) >> index3;
          /* store the upper bitpack+index3 bits of carry3. */
          if (bitpackcount==0)
            break;
          *ptr3++ = L_bitpack(carry3); /* store bitpack bits */
          carry3 = LR_bitpack_0(R_bitpack(carry3)); /* and keep index3 bits for later. */
          bitpackcount--;
        }
      }
    }
    /* treat last (half) data word specially:
       From the last word (now in the bits
       2*bitpack-index3-1..bitpack-index3 of carry3)
       only bitcount_rest bits may be stored in the 3rd array. */
    bitcount_rest = index3+bitcount_rest;
    var uint_bitpack_t last_carry;
    /* store the upper bitcount_rest bits: */
    if (bitcount_rest>=bitpack) {
      *ptr3++ = L_bitpack(carry3);
      last_carry = R_bitpack(carry3);
      bitcount_rest -= bitpack;
    } else {
      last_carry = L_bitpack(carry3);
    }
    /* store the remaining bitcount_rest bits of last_carry: */
    if (bitcount_rest!=0)
      *ptr3 ^=
        (*ptr3 ^ last_carry)
        & (~( (uint_bitpack_t)(bitm(bitpack)-1) >> bitcount_rest ));
    /* bitmask, where the upper bitcount_rest bits are set */
  }
}

/* subroutine for bit-combination with 2 operands
 bit_up(op)
 > STACK_2: bit-array1
 > STACK_1: bit-array2
 > STACK_0: result-bit-array or #<UNBOUND>
 > op: address of the combination routine
 < value1/mv_count: function value
 tests the arguments, cleans up STACK. */
local Values bit_up (bit_op_fun_t* op)
{
  /* main distinction: vector / multi-dimensional array */
  var uintL len; /* length (of the 1st array), if vectors */
  var uintC rank; /* rank and */
  var uintL* dimptr; /* pointer to dimensions, if multi-dimensional */
  /* examine type of bit-array1 and branch accordingly: */
 #ifndef TYPECODES
  if (!orecordp(STACK_2))
    goto fehler2;
 #endif
  switch (Array_type(STACK_2)) {
    case Array_type_sbvector:
      len = Sbvector_length(STACK_2); goto vector;
    case Array_type_bvector:
      len = TheIarray(STACK_2)->totalsize; goto vector;
    case Array_type_mdarray: {
      var Iarray array1 = TheIarray(STACK_2);
      /* bit-array1 must have the element type BIT : */
      if ((iarray_flags(array1) & arrayflags_atype_mask) != Atype_Bit)
        goto fehler2;
      /* store rank: */
      rank = iarray_rank(array1);
      /* store dimensions: */
      dimptr = &array1->dims[0];
      if (iarray_flags(array1) & bit(arrayflags_dispoffset_bit))
        dimptr++;
      /* Totalsize is the number of the bits to combine: */
      len = array1->totalsize;
      goto array;
    }
    default:
      goto fehler2;
  }
 vector: /* The first argument is a  bit-vector, with length len. */
  /* test, if this also applies to the other(s) : */
  /* check bit-array2: */
 #ifndef TYPECODES
  if (!orecordp(STACK_1))
    goto fehler2;
 #endif
  switch (Array_type(STACK_1)) {
    case Array_type_sbvector:
      if (len != Sbvector_length(STACK_1))
        goto fehler2;
      break;
    case Array_type_bvector:
      if (len != TheIarray(STACK_1)->totalsize)
        goto fehler2;
      break;
    default:
      goto fehler2;
  }
  { /* check bit-array3: */
    var object array3 = STACK_0;
    if (missingp(array3)) { /* unbound or NIL? */
      /* yes -> create new vector: */
      STACK_0 = allocate_bit_vector(Atype_Bit,len);
    } else if (eq(array3,T)) {
      STACK_0 = STACK_2; /* instead of T, use bit-array1 */
    } else {
     #ifndef TYPECODES
      if (!orecordp(STACK_0))
        goto fehler3;
     #endif
      switch (Array_type(STACK_0)) {
        case Array_type_sbvector:
          if (len != Sbvector_length(array3))
            goto fehler3;
          break;
        case Array_type_bvector:
          if (len != TheIarray(array3)->totalsize)
            goto fehler3;
              break;
        default:
          goto fehler3;
      }
    }
  }
  goto weiter;
 array: /* first argument was a multi-dimensional bit-array */
  /* with Rank rank, Dimensions at dimptr and Totalsize len. */
  /* check bit-array2: */
 #ifndef TYPECODES
  if (!orecordp(STACK_1))
    goto fehler2;
 #endif
  switch (Array_type(STACK_1)) {
    case Array_type_mdarray: {
      var Iarray array2 = TheIarray(STACK_1);
      /* bit-array2 must have the element type BIT : */
      if ((iarray_flags(array2) & arrayflags_atype_mask) != Atype_Bit)
        goto fehler2;
      /* compare rank: */
      if (rank != iarray_rank(array2))
        goto fehler2;
      /* compare dimensions: */
      if (rank > 0) {
        var uintC count;
        var uintL* dimptr1 = dimptr;
        var uintL* dimptr2 = &array2->dims[0];
        if (iarray_flags(array2) & bit(arrayflags_dispoffset_bit))
          dimptr2++;
        dotimespC(count,rank, {
          if (*dimptr1++ != *dimptr2++)
            goto fehler2;
        });
      }
    }
      break;
    default:
      goto fehler2;
  }
  { /* check bit-array3: */
    var object array3 = STACK_0;
    if (missingp(array3)) { /* unbound or NIL? */
      /* yes -> create new array: */
      STACK_0 = allocate_bit_vector(Atype_Bit,len); /* create bitvector */
      array3 = allocate_iarray(Atype_Bit,rank,Array_type_mdarray); /* create array */
      TheIarray(array3)->data = STACK_0; /* store data vector */
      TheIarray(array3)->totalsize = len;
      /* store dimensions: */
      if (rank > 0) {
        var uintC count;
        /* dimptr1 is the same as dimptr, but we have to re-init it
           becase of the GC-safety issues: the above allocations
           might have invalidated dimptr */
        var uintL* dimptr1 = &TheIarray(STACK_2)->dims[0];
        var uintL* dimptr2 = &TheIarray(array3)->dims[0];
        if (iarray_flags(TheIarray(STACK_2)) & bit(arrayflags_dispoffset_bit))
          dimptr1++;
        dotimespC(count,rank, { *dimptr2++ = *dimptr1++;});
      }
      STACK_0 = array3; /* store new array */
    } else if (eq(array3,T)) {
      STACK_0 = STACK_2; /* instead of T, use bit-array1 */
    } else {
     #ifndef TYPECODES
      if (!orecordp(STACK_0))
        goto fehler3;
     #endif
      switch (Array_type(STACK_0)) {
        case Array_type_mdarray: {
          var Iarray iarr3 = TheIarray(STACK_0);
          /* bit-array3 must have the element type BIT : */
          if ((iarray_flags(iarr3) & arrayflags_atype_mask) != Atype_Bit)
            goto fehler3;
          /* compare rank: */
          if (rank != iarray_rank(iarr3))
            goto fehler3;
          /* compare dimensions: */
          if (rank > 0) {
            var uintC count;
            var uintL* dimptr1 = dimptr;
            var uintL* dimptr2 = &iarr3->dims[0];
            if (iarray_flags(iarr3) & bit(arrayflags_dispoffset_bit))
              dimptr2++;
            dotimespC(count,rank, {
              if (*dimptr1++ != *dimptr2++)
                goto fehler3;
            });
          }
        }
          break;
        default:
          goto fehler3;
      }
    }
  }
 weiter: /* preparations are completed: */
  /* STACK_2 = bit-array1, STACK_1 = bit-array2, STACK_0 = bit-array3, */
  /* all of the same dimensions, with len bits. */
  if (len > 0) {
    var uintL index1 = 0; /* index in data vector of bit-array1 */
    var object array1 = /* data vector of bit-array1 */
      (simple_bit_vector_p(Atype_Bit,STACK_2)
       ? (object)STACK_2
       : iarray_displace_check(STACK_2,len,&index1));
    var uintL index2 = 0; /* index in data vector of bit-array2 */
    var object array2 = /* data vector of bit-array2 */
      (simple_bit_vector_p(Atype_Bit,STACK_1)
       ? (object)STACK_1
       : iarray_displace_check(STACK_1,len,&index2));
    var uintL index3 = 0; /* index in data vector of bit-array3 */
    var object array3 = /* data vector of bit-array3 */
      (simple_bit_vector_p(Atype_Bit,STACK_0)
       ? (object)STACK_0
       : iarray_displace_check(STACK_0,len,&index3));
    /* Go ahead: */
    bit_op(array1,index1,array2,index2,array3,index3,op,len);
  }
  /* done: */
  VALUES1(popSTACK()); /* bit-array3 is the value */
  skipSTACK(2);
  return;
 fehler2: { /* error-message for (at least) 2 arguments */
    var object array1 = STACK_2;
    var object array2 = STACK_1;
    pushSTACK(array2); pushSTACK(array1);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,
           GETTEXT("~S: The arguments ~S and ~S should be arrays of bits with the same dimensions"));
  }
 fehler3: { /* error-message for 3 arguments */
    var object array1 = STACK_2;
    var object array2 = STACK_1;
    /* array3 already in STACK_0 */
    pushSTACK(array2); pushSTACK(array1);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,
           GETTEXT("~S: The arguments ~S, ~S and ~S should be arrays of bits with the same dimensions"));
  }
}

/* The operators for BIT-AND etc.: */
local uint_bitpack_t bitpack_and (uint_bitpack_t x, uint_bitpack_t y)
{ return x&y; }
local uint_bitpack_t bitpack_ior (uint_bitpack_t x, uint_bitpack_t y)
{ return x|y; }
local uint_bitpack_t bitpack_xor (uint_bitpack_t x, uint_bitpack_t y)
{ return x^y; }
local uint_bitpack_t bitpack_eqv (uint_bitpack_t x, uint_bitpack_t y)
{ return ~(x^y); }
local uint_bitpack_t bitpack_nand (uint_bitpack_t x, uint_bitpack_t y)
{ return ~(x&y); }
local uint_bitpack_t bitpack_nor (uint_bitpack_t x, uint_bitpack_t y)
{ return ~(x|y); }
local uint_bitpack_t bitpack_andc1 (uint_bitpack_t x, uint_bitpack_t y)
{ return (~x)&y; }
local uint_bitpack_t bitpack_andc2 (uint_bitpack_t x, uint_bitpack_t y)
{ return x&(~y); }
local uint_bitpack_t bitpack_orc1 (uint_bitpack_t x, uint_bitpack_t y)
{ return (~x)|y; }
local uint_bitpack_t bitpack_orc2 (uint_bitpack_t x, uint_bitpack_t y)
{ return x|(~y); }
local uint_bitpack_t bitpack_not (uint_bitpack_t x, uint_bitpack_t y)
{ return ~x; }

LISPFUN(bit_and,seclass_default,2,1,norest,nokey,0,NIL)
{ /* (BIT-AND bit-array1 bit-array2 [result-bit-array]), CLTL p. 294 */
  return_Values bit_up(&bitpack_and);
}

LISPFUN(bit_ior,seclass_default,2,1,norest,nokey,0,NIL)
{ /* (BIT-IOR bit-array1 bit-array2 [result-bit-array]), CLTL p. 294 */
  return_Values bit_up(&bitpack_ior);
}

LISPFUN(bit_xor,seclass_default,2,1,norest,nokey,0,NIL)
{ /* (BIT-XOR bit-array1 bit-array2 [result-bit-array]), CLTL p. 294 */
  return_Values bit_up(&bitpack_xor);
}

LISPFUN(bit_eqv,seclass_default,2,1,norest,nokey,0,NIL)
{ /* (BIT-EQV bit-array1 bit-array2 [result-bit-array]), CLTL p. 294 */
  return_Values bit_up(&bitpack_eqv);
}

LISPFUN(bit_nand,seclass_default,2,1,norest,nokey,0,NIL)
{ /* (BIT-NAND bit-array1 bit-array2 [result-bit-array]), CLTL p. 294 */
  return_Values bit_up(&bitpack_nand);
}

LISPFUN(bit_nor,seclass_default,2,1,norest,nokey,0,NIL)
{ /* (BIT-NOR bit-array1 bit-array2 [result-bit-array]), CLTL p. 294 */
  return_Values bit_up(&bitpack_nor);
}

LISPFUN(bit_andc1,seclass_default,2,1,norest,nokey,0,NIL)
{ /* (BIT-ANDC1 bit-array1 bit-array2 [result-bit-array]), CLTL p. 294 */
  return_Values bit_up(&bitpack_andc1);
}

LISPFUN(bit_andc2,seclass_default,2,1,norest,nokey,0,NIL)
{ /* (BIT-ANDC2 bit-array1 bit-array2 [result-bit-array]), CLTL p. 294 */
  return_Values bit_up(&bitpack_andc2);
}

LISPFUN(bit_orc1,seclass_default,2,1,norest,nokey,0,NIL)
{ /* (BIT-ORC1 bit-array1 bit-array2 [result-bit-array]), CLTL p. 294 */
  return_Values bit_up(&bitpack_orc1);
}

LISPFUN(bit_orc2,seclass_default,2,1,norest,nokey,0,NIL)
{ /* (BIT-ORC2 bit-array1 bit-array2 [result-bit-array]), CLTL p. 294 */
  return_Values bit_up(&bitpack_orc2);
}

LISPFUN(bit_not,seclass_default,1,1,norest,nokey,0,NIL)
{ /* (BIT-NOT bit-array [result-bit-array]), CLTL p. 295 */
  /* double first argument (is ignored during the operation): */
  var object array3 = STACK_0;
  pushSTACK(array3);
  STACK_1 = STACK_2;
  return_Values bit_up(&bitpack_not);
}

/* ======================================================================== */
/* Polymorphic copying */

/* Function: Copies a slice of an array array1 into another array array2.
 elt_copy(dv1,index1,dv2,index2,count);
 > dv1: source storage-vector
 > index1: start index in dv1
 > dv2: destination storage-vector
 > index2: start index in dv2
 > count: number of elements to be copied, > 0
 can trigger GC - if dv1 and dv2 have different element types or
                  if both are strings and dv1 is wider than dv2 */
global /*maygc*/ void elt_copy (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count);
local void elt_copy_Bit_Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
local void elt_copy_2Bit_2Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
local void elt_copy_4Bit_4Bit (object dv1, uintL index1, object dv2, uintL index2, uintL count);
local void elt_copy_T_T (object dv1, uintL index1,
                         object dv2, uintL index2, uintL count) {
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var gcv_object_t* ptr2 = &TheSvector(dv2)->data[index2];
  dotimespL(count,count, {
    *ptr2++ = *ptr1++;
  });
}
local void elt_copy_Char_T (object dv1, uintL index1,
                            object dv2, uintL index2, uintL count) {
  var gcv_object_t* ptr2 = &TheSvector(dv2)->data[index2];
  SstringDispatch(dv1,X1, {
    var const cintX1* ptr1 = &((SstringX1)TheVarobject(dv1))->data[index1];
    dotimespL(count,count, {
      *ptr2++ = code_char(as_chart(*ptr1)); ptr1++;
    });
  });
}
local void elt_copy_Bit_T (object dv1, uintL index1,
                           object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
  var gcv_object_t* ptr2 = &TheSvector(dv2)->data[index2];
  dotimespL(count,count, {
    *ptr2++ = fixnum((*ptr1 >> ((~index1)%8)) & (bit(1)-1));
    index1++;
    ptr1 += ((index1%8)==0);
  });
}
local void elt_copy_2Bit_T (object dv1, uintL index1,
                            object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
  var gcv_object_t* ptr2 = &TheSvector(dv2)->data[index2];
  dotimespL(count,count, {
    *ptr2++ = fixnum((*ptr1 >> (2*((~index1)%4))) & (bit(2)-1));
    index1++;
    ptr1 += ((index1%4)==0);
  });
}
local void elt_copy_4Bit_T (object dv1, uintL index1,
                            object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
  var gcv_object_t* ptr2 = &TheSvector(dv2)->data[index2];
  dotimespL(count,count, {
    *ptr2++ = fixnum((*ptr1 >> (4*((~index1)%2))) & (bit(4)-1));
    index1++;
    ptr1 += ((index1%2)==0);
  });
}
local void elt_copy_8Bit_T (object dv1, uintL index1,
                            object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
  var gcv_object_t* ptr2 = &TheSvector(dv2)->data[index2];
  dotimespL(count,count, {
    *ptr2++ = fixnum(*ptr1++);
  });
}
local void elt_copy_16Bit_T (object dv1, uintL index1,
                             object dv2, uintL index2, uintL count) {
  var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
  var gcv_object_t* ptr2 = &TheSvector(dv2)->data[index2];
  dotimespL(count,count, {
    *ptr2++ = fixnum(*ptr1++);
  });
}
local maygc void elt_copy_32Bit_T (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count) {
 #if (intLsize<=oint_data_len)
  /* UL_to_I(x) = fixnum(x), cannot trigger GC */
  var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
  var gcv_object_t* ptr2 = &TheSvector(dv2)->data[index2];
  dotimespL(count,count, {
    *ptr2++ = fixnum(*ptr1++);
  });
 #else
  pushSTACK(dv1);
  pushSTACK(dv2);
  dotimespL(count,count, {
    var object x = UL_to_I(((uint32*)&TheSbvector(STACK_1)->data[0])[index1++]);
    TheSvector(STACK_0)->data[index2++] = x;
  });
  skipSTACK(2);
 #endif
}
local maygc void elt_copy_T_Char (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count) {
  if (simple_nilarray_p(dv2)) fehler_nilarray_store();
  check_sstring_mutable(dv2);
 restart_it:
  SstringCase(dv2,{
    for (;;) {
      var object value = TheSvector(dv1)->data[index1++];
      if (!charp(value)) fehler_store(dv2,value);
      if (as_cint(char_code(value)) < cint8_limit) {
        TheS8string(dv2)->data[index2++] = as_cint(char_code(value));
        if (--count == 0)
          break;
      } else {
        dv2 = sstring_store(dv2,index2++,char_code(value));
        if (--count == 0)
          break;
        if (sstring_reallocatedp(TheSstring(dv2))) { /* reallocated? */
          dv2 = TheSistring(dv2)->data;
          goto restart_it;
        }
      }
    }
  },{
    for (;;) {
      var object value = TheSvector(dv1)->data[index1++];
      if (!charp(value)) fehler_store(dv2,value);
      if (as_cint(char_code(value)) < cint16_limit) {
        TheS16string(dv2)->data[index2++] = as_cint(char_code(value));
        if (--count == 0)
          break;
      } else {
        dv2 = sstring_store(dv2,index2++,char_code(value));
        if (--count == 0)
          break;
        if (sstring_reallocatedp(TheSstring(dv2))) { /* reallocated? */
          dv2 = TheSistring(dv2)->data;
          goto restart_it;
        }
      }
    }
  },{
    var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
    var cint32* ptr2 = &TheS32string(dv2)->data[index2];
    dotimespL(count,count, {
      var object value = *ptr1++;
      if (!charp(value)) fehler_store(dv2,value);
      *ptr2++ = as_cint(char_code(value));
    });
  },{
    NOTREACHED;
  });
}
local /*maygc*/ void elt_copy_Char_Char (object dv1, uintL index1,
                                         object dv2, uintL index2, uintL count) {
  GCTRIGGER_IF(sstring_eltype(TheSstring(dv1)) > sstring_eltype(TheSstring(dv2)),
               GCTRIGGER2(dv1,dv2));
  if (simple_nilarray_p(dv2)) fehler_nilarray_store();
  check_sstring_mutable(dv2);
  SstringCase(dv1,{
    var const cint8* ptr1 = &TheS8string(dv1)->data[index1];
    SstringCase(dv2,{
      /* Equivalent to copy_8bit_8bit, but we inline it here. */
      var cint8* ptr2 = &TheS8string(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    },{
      /* Equivalent to copy_8bit_16bit, but we inline it here. */
      var cint16* ptr2 = &TheS16string(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    },{
      /* Equivalent to copy_8bit_32bit, but we inline it here. */
      var cint32* ptr2 = &TheS32string(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    },{
      NOTREACHED;
    });
  },{
   restart16:
    SstringCase(dv2,{
      pushSTACK(dv1);
      for (;;) {
        var chart ch = as_chart(TheS16string(dv1)->data[index1++]);
        if (as_cint(ch) < cint8_limit) {
          TheS8string(dv2)->data[index2++] = as_cint(ch);
          if (--count == 0)
            break;
        } else {
          dv2 = sstring_store(dv2,index2++,ch);
          if (--count == 0)
            break;
          if (sstring_reallocatedp(TheSstring(dv2))) { /* reallocated? */
            dv2 = TheSistring(dv2)->data;
            dv1 = popSTACK();
            goto restart16;
          }
        }
      }
      skipSTACK(1);
    },{
      /* Equivalent to copy_16bit_16bit, but we inline it here. */
      var const cint16* ptr1 = &TheS16string(dv1)->data[index1];
      var cint16* ptr2 = &TheS16string(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    },{
      /* Equivalent to copy_16bit_32bit, but we inline it here. */
      var const cint16* ptr1 = &TheS16string(dv1)->data[index1];
      var cint32* ptr2 = &TheS32string(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    },{
      NOTREACHED;
    });
  },{
   restart32:
    SstringCase(dv2,{
      pushSTACK(dv1);
      for (;;) {
        var chart ch = as_chart(TheS32string(dv1)->data[index1++]);
        if (as_cint(ch) < cint8_limit) {
          TheS8string(dv2)->data[index2++] = as_cint(ch);
          if (--count == 0)
            break;
        } else {
          dv2 = sstring_store(dv2,index2++,ch);
          if (--count == 0)
            break;
          if (sstring_reallocatedp(TheSstring(dv2))) { /* reallocated? */
            dv2 = TheSistring(dv2)->data;
            dv1 = popSTACK();
            goto restart32;
          }
        }
      }
      skipSTACK(1);
    },{
      pushSTACK(dv1);
      for (;;) {
        var chart ch = as_chart(TheS32string(dv1)->data[index1++]);
        if (as_cint(ch) < cint16_limit) {
          TheS16string(dv2)->data[index2++] = as_cint(ch);
          if (--count == 0)
            break;
        } else {
          dv2 = sstring_store(dv2,index2++,ch);
          if (--count == 0)
            break;
          if (sstring_reallocatedp(TheSstring(dv2))) { /* reallocated? */
            dv2 = TheSistring(dv2)->data;
            dv1 = popSTACK();
            goto restart32;
          }
        }
      }
      skipSTACK(1);
    },{
      /* Equivalent to copy_32bit_32bit, but we inline it here. */
      var const cint32* ptr1 = &TheS32string(dv1)->data[index1];
      var cint32* ptr2 = &TheS32string(dv2)->data[index2];
      dotimespL(count,count, {
        *ptr2++ = *ptr1++;
      });
    },{
      NOTREACHED;
    });
  },{
    fehler_nilarray_retrieve();
  });
}
local void elt_copy_T_Bit (object dv1, uintL index1,
                           object dv2, uintL index2, uintL count) {
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
  dotimespL(count,count, {
    var object value = *ptr1++;
    if (!uint1_p(value)) fehler_store(dv2,value);
    *ptr2 ^= (*ptr2 ^ (I_to_uint8(value) << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
    index2++;
    ptr2 += ((index2%8)==0);
  });
}
#define elt_copy_Bit_Bit(dv1,index1,dv2,index2,count)   \
  bit_copy(dv1,index1,dv2,index2,count)
local void elt_copy_2Bit_Bit (object dv1, uintL index1,
                              object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
  dotimespL(count,count, {
    var uint8 value = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
    if (value >= bit(1)) fehler_store(dv2,fixnum(value));
    *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
    index1++;
    ptr1 += ((index1%4)==0);
    index2++;
    ptr2 += ((index2%8)==0);
  });
}
local void elt_copy_4Bit_Bit (object dv1, uintL index1,
                              object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
  dotimespL(count,count, {
    var uint8 value = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
    if (value >= bit(1)) fehler_store(dv2,fixnum(value));
    *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
    index1++;
    ptr1 += ((index1%2)==0);
    index2++;
    ptr2 += ((index2%8)==0);
  });
}
local void elt_copy_8Bit_Bit (object dv1, uintL index1,
                              object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
  dotimespL(count,count, {
    var uint8 value = *ptr1++;
    if (value >= bit(1)) fehler_store(dv2,fixnum(value));
    *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
    index2++;
    ptr2 += ((index2%8)==0);
  });
}
local void elt_copy_16Bit_Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count) {
  var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
  dotimespL(count,count, {
    var uint16 value = *ptr1++;
    if (value >= bit(1)) fehler_store(dv2,fixnum(value));
    *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
    index2++;
    ptr2 += ((index2%8)==0);
  });
}
local void elt_copy_32Bit_Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count) {
  var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
  dotimespL(count,count, {
    var uint32 value = *ptr1++;
    if (value >= bit(1)) {
      pushSTACK(dv2);
      var object tmp = UL_to_I(value);
      fehler_store(popSTACK(),tmp);
    }
    *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
    index2++;
    ptr2 += ((index2%8)==0);
  });
}
local void elt_copy_T_2Bit (object dv1, uintL index1,
                            object dv2, uintL index2, uintL count) {
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
  dotimespL(count,count, {
    var object value = *ptr1++;
    if (!uint2_p(value)) fehler_store(dv2,value);
    *ptr2 ^= (*ptr2 ^ (I_to_uint8(value) << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
    index2++;
    ptr2 += ((index2%4)==0);
  });
}
local void elt_copy_Bit_2Bit (object dv1, uintL index1,
                              object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
  dotimespL(count,count, {
    var uint8 value = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
    *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
    index1++;
    ptr1 += ((index1%8)==0);
    index2++;
    ptr2 += ((index2%4)==0);
  });
}
#define elt_copy_2Bit_2Bit(dv1,index1,dv2,index2,count)         \
  bit_copy(dv1,(index1)<<1,dv2,(index2)<<1,(count)<<1)
local void elt_copy_4Bit_2Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
  dotimespL(count,count, {
    var uint8 value = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
    if (value >= bit(2)) fehler_store(dv2,fixnum(value));
    *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
    index1++;
    ptr1 += ((index1%2)==0);
    index2++;
    ptr2 += ((index2%4)==0);
  });
}
local void elt_copy_8Bit_2Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
  dotimespL(count,count, {
    var uint8 value = *ptr1++;
    if (value >= bit(2)) fehler_store(dv2,fixnum(value));
    *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
    index2++;
    ptr2 += ((index2%4)==0);
  });
}
local void elt_copy_16Bit_2Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
  dotimespL(count,count, {
    var uint16 value = *ptr1++;
    if (value >= bit(2)) fehler_store(dv2,fixnum(value));
    *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
    index2++;
    ptr2 += ((index2%4)==0);
  });
}
local void elt_copy_32Bit_2Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
  dotimespL(count,count, {
    var uint32 value = *ptr1++;
    if (value >= bit(2)) {
      pushSTACK(dv2);
      var object tmp = UL_to_I(value);
      fehler_store(popSTACK(),tmp);
    }
    *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
    index2++;
    ptr2 += ((index2%4)==0);
  });
}
local void elt_copy_T_4Bit (object dv1, uintL index1,
                            object dv2, uintL index2, uintL count) {
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
  dotimespL(count,count, {
    var object value = *ptr1++;
    if (!uint4_p(value)) fehler_store(dv2,value);
    *ptr2 ^= (*ptr2 ^ (I_to_uint8(value) << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
    index2++;
    ptr2 += ((index2%2)==0);
  });
}
local void elt_copy_Bit_4Bit (object dv1, uintL index1,
                              object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
  dotimespL(count,count, {
    var uint8 value = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
    *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
    index1++;
    ptr1 += ((index1%8)==0);
    index2++;
    ptr2 += ((index2%2)==0);
  });
}
local void elt_copy_2Bit_4Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
  dotimespL(count,count, {
    var uint8 value = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
    *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
    index1++;
    ptr1 += ((index1%4)==0);
    index2++;
    ptr2 += ((index2%2)==0);
  });
}
#define elt_copy_4Bit_4Bit(dv1,index1,dv2,index2,count)         \
  bit_copy(dv1,(index1)<<2,dv2,(index2)<<2,(count)<<2)
local void elt_copy_8Bit_4Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
  dotimespL(count,count, {
    var uint8 value = *ptr1++;
    if (value >= bit(4)) fehler_store(dv2,fixnum(value));
    *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
    index2++;
    ptr2 += ((index2%2)==0);
  });
}
local void elt_copy_16Bit_4Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
  dotimespL(count,count, {
    var uint16 value = *ptr1++;
    if (value >= bit(4)) fehler_store(dv2,fixnum(value));
    *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
    index2++;
    ptr2 += ((index2%2)==0);
  });
}
local void elt_copy_32Bit_4Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
  dotimespL(count,count, {
    var uint32 value = *ptr1++;
    if (value >= bit(4)) {
      pushSTACK(dv2);
      var object tmp = UL_to_I(value);
      fehler_store(popSTACK(),tmp);
    }
    *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
    index2++;
    ptr2 += ((index2%2)==0);
  });
}
local void elt_copy_T_8Bit (object dv1, uintL index1,
                            object dv2, uintL index2, uintL count) {
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
  dotimespL(count,count, {
    var object value = *ptr1++;
    if (!uint8_p(value)) fehler_store(dv2,value);
    *ptr2++ = I_to_uint8(value);
  });
}
local void elt_copy_Bit_8Bit (object dv1, uintL index1,
                              object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
  dotimespL(count,count, {
    *ptr2++ = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
    index1++;
    ptr1 += ((index1%8)==0);
  });
}
local void elt_copy_2Bit_8Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
  dotimespL(count,count, {
    *ptr2++ = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
    index1++;
    ptr1 += ((index1%4)==0);
  });
}
local void elt_copy_4Bit_8Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
  dotimespL(count,count, {
    *ptr2++ = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
    index1++;
    ptr1 += ((index1%2)==0);
  });
}
local void elt_copy_8Bit_8Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
  dotimespL(count,count, {
    *ptr2++ = *ptr1++;
  });
}
local void elt_copy_16Bit_8Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
  dotimespL(count,count, {
    var uint16 value = *ptr1++;
    if (value >= bit(8)) fehler_store(dv2,fixnum(value));
    *ptr2++ = value;
  });
}
local void elt_copy_32Bit_8Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
  var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
  dotimespL(count,count, {
    var uint32 value = *ptr1++;
    if (value >= bit(8)) {
      pushSTACK(dv2);
      var object tmp = UL_to_I(value);
      fehler_store(popSTACK(),tmp);
    }
    *ptr2++ = value;
  });
}
local void elt_copy_T_16Bit (object dv1, uintL index1,
                             object dv2, uintL index2, uintL count) {
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    var object value = *ptr1++;
    if (!uint16_p(value)) fehler_store(dv2,value);
    *ptr2++ = I_to_uint16(value);
  });
}
local void elt_copy_Bit_16Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
  var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    *ptr2++ = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
    index1++;
    ptr1 += ((index1%8)==0);
  });
}
local void elt_copy_2Bit_16Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
  var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    *ptr2++ = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
    index1++;
    ptr1 += ((index1%4)==0);
  });
}
local void elt_copy_4Bit_16Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
  var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    *ptr2++ = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
    index1++;
    ptr1 += ((index1%2)==0);
  });
}
local void elt_copy_8Bit_16Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
  var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    *ptr2++ = *ptr1++;
  });
}
local void elt_copy_16Bit_16Bit (object dv1, uintL index1,
                                 object dv2, uintL index2, uintL count) {
  var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
  var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    *ptr2++ = *ptr1++;
  });
}
local void elt_copy_32Bit_16Bit (object dv1, uintL index1,
                                 object dv2, uintL index2, uintL count) {
  var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
  var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    var uint32 value = *ptr1++;
    if (value >= bit(16)) {
      pushSTACK(dv2);
      var object tmp = UL_to_I(value);
      fehler_store(popSTACK(),tmp);
    }
    *ptr2++ = value;
  });
}
local void elt_copy_T_32Bit (object dv1, uintL index1,
                             object dv2, uintL index2, uintL count) {
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    var object value = *ptr1++;
    if (!uint32_p(value)) fehler_store(dv2,value);
    *ptr2++ = I_to_uint32(value);
  });
}
local void elt_copy_Bit_32Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
  var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    *ptr2++ = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
    index1++;
    ptr1 += ((index1%8)==0);
  });
}
local void elt_copy_2Bit_32Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
  var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    *ptr2++ = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
    index1++;
    ptr1 += ((index1%4)==0);
  });
}
local void elt_copy_4Bit_32Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
  var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    *ptr2++ = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
    index1++;
    ptr1 += ((index1%2)==0);
  });
}
local void elt_copy_8Bit_32Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
  var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    *ptr2++ = *ptr1++;
  });
}
local void elt_copy_16Bit_32Bit (object dv1, uintL index1,
                                 object dv2, uintL index2, uintL count) {
  var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
  var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    *ptr2++ = *ptr1++;
  });
}
local void elt_copy_32Bit_32Bit (object dv1, uintL index1,
                                 object dv2, uintL index2, uintL count) {
  var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
  var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    *ptr2++ = *ptr1++;
  });
}
global /*maygc*/ void elt_copy (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  GCTRIGGER_IF(Array_type(dv1) != Array_type(dv2)
               || (simple_string_p(dv1) && simple_string_p(dv2)
                   && sstring_eltype(TheSstring(dv1)) > sstring_eltype(TheSstring(dv2))),
               GCTRIGGER2(dv1,dv2));
  switch (Array_type(dv1)) {
    case Array_type_svector: /* Simple-Vector */
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          elt_copy_T_T(dv1,index1,dv2,index2,count); return;
        case Array_type_sbvector: /* Simple-Bit-Vector */
          elt_copy_T_Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb2vector:
          elt_copy_T_2Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb4vector:
          elt_copy_T_4Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb8vector:
          elt_copy_T_8Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb16vector:
          elt_copy_T_16Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb32vector:
          elt_copy_T_32Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sstring: /* Simple-String */
          elt_copy_T_Char(dv1,index1,dv2,index2,count); return;
        case Array_type_snilvector: /* (VECTOR NIL) */
          break; /* fehler_store because count > 0 */
        default: NOTREACHED;
      }
      break;
    case Array_type_sbvector: /* Simple-Bit-Vector */
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          elt_copy_Bit_T(dv1,index1,dv2,index2,count); return;
        case Array_type_sbvector: /* Simple-Bit-Vector */
          elt_copy_Bit_Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb2vector:
          elt_copy_Bit_2Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb4vector:
          elt_copy_Bit_4Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb8vector:
          elt_copy_Bit_8Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb16vector:
          elt_copy_Bit_16Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb32vector:
          elt_copy_Bit_32Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sstring: /* Simple-String */
        case Array_type_snilvector: /* (VECTOR NIL) */
          break; /* fehler_store because count > 0 */
        default: NOTREACHED;
      }
      break;
    case Array_type_sb2vector:
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          elt_copy_2Bit_T(dv1,index1,dv2,index2,count); return;
        case Array_type_sbvector: /* Simple-Bit-Vector */
          elt_copy_2Bit_Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb2vector:
          elt_copy_2Bit_2Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb4vector:
          elt_copy_2Bit_4Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb8vector:
          elt_copy_2Bit_8Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb16vector:
          elt_copy_2Bit_16Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb32vector:
          elt_copy_2Bit_32Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sstring: /* Simple-String */
        case Array_type_snilvector: /* (VECTOR NIL) */
          break; /* fehler_store because count > 0 */
        default: NOTREACHED;
      }
      break;
    case Array_type_sb4vector:
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          elt_copy_4Bit_T(dv1,index1,dv2,index2,count); return;
        case Array_type_sbvector: /* Simple-Bit-Vector */
          elt_copy_4Bit_Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb2vector:
          elt_copy_4Bit_2Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb4vector:
          elt_copy_4Bit_4Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb8vector:
          elt_copy_4Bit_8Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb16vector:
          elt_copy_4Bit_16Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb32vector:
          elt_copy_4Bit_32Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sstring: /* Simple-String */
        case Array_type_snilvector: /* (VECTOR NIL) */
          break; /* fehler_store because count > 0 */
        default: NOTREACHED;
      }
      break;
    case Array_type_sb8vector:
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          elt_copy_8Bit_T(dv1,index1,dv2,index2,count); return;
        case Array_type_sbvector: /* Simple-Bit-Vector */
          elt_copy_8Bit_Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb2vector:
          elt_copy_8Bit_2Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb4vector:
          elt_copy_8Bit_4Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb8vector:
          elt_copy_8Bit_8Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb16vector:
          elt_copy_8Bit_16Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb32vector:
          elt_copy_8Bit_32Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sstring: /* Simple-String */
        case Array_type_snilvector: /* (VECTOR NIL) */
          break; /* fehler_store because count > 0 */
        default: NOTREACHED;
      }
      break;
    case Array_type_sb16vector:
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          elt_copy_16Bit_T(dv1,index1,dv2,index2,count); return;
        case Array_type_sbvector: /* Simple-Bit-Vector */
          elt_copy_16Bit_Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb2vector:
          elt_copy_16Bit_2Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb4vector:
          elt_copy_16Bit_4Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb8vector:
          elt_copy_16Bit_8Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb16vector:
          elt_copy_16Bit_16Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb32vector:
          elt_copy_16Bit_32Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sstring: /* Simple-String */
        case Array_type_snilvector: /* (VECTOR NIL) */
          break; /* fehler_store because count > 0 */
        default: NOTREACHED;
      }
      break;
    case Array_type_sb32vector:
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          elt_copy_32Bit_T(dv1,index1,dv2,index2,count); return;
        case Array_type_sbvector: /* Simple-Bit-Vector */
          elt_copy_32Bit_Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb2vector:
          elt_copy_32Bit_2Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb4vector:
          elt_copy_32Bit_4Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb8vector:
          elt_copy_32Bit_8Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb16vector:
          elt_copy_32Bit_16Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sb32vector:
          elt_copy_32Bit_32Bit(dv1,index1,dv2,index2,count); return;
        case Array_type_sstring: /* Simple-String */
        case Array_type_snilvector: /* (VECTOR NIL) */
          break; /* fehler_store because count > 0 */
        default: NOTREACHED;
      }
      break;
    case Array_type_sstring: /* Simple-String */
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          elt_copy_Char_T(dv1,index1,dv2,index2,count); return;
        case Array_type_sbvector: /* Simple-Bit-Vector */
        case Array_type_sb2vector:
        case Array_type_sb4vector:
        case Array_type_sb8vector:
        case Array_type_sb16vector:
        case Array_type_sb32vector:
        case Array_type_snilvector: /* (VECTOR NIL) */
          break; /* fehler_store because count > 0 */
        case Array_type_sstring: /* Simple-String */
          elt_copy_Char_Char(dv1,index1,dv2,index2,count); return;
        default: NOTREACHED;
      }
      break;
    case Array_type_snilvector: /* (VECTOR NIL) */
      switch (Array_type(dv2)) {
        case Array_type_snilvector:
          return;
        case Array_type_svector: /* Simple-Vector */
        case Array_type_sbvector: /* Simple-Bit-Vector */
        case Array_type_sb2vector:
        case Array_type_sb4vector:
        case Array_type_sb8vector:
        case Array_type_sb16vector:
        case Array_type_sb32vector:
        case Array_type_sstring: /* Simple-String */
          fehler_nilarray_retrieve();
        default: NOTREACHED;
      }
    default: NOTREACHED;
  }
  pushSTACK(dv2);
  var object elt1 = storagevector_aref(dv1,index1);
  fehler_store(popSTACK(),elt1);
}

/* Function: Copies a slice of an array array1 into another array array2 of
 the same element type. Handles overlapping arrays correctly.
 elt_move(dv1,index1,dv2,index2,count);
 > dv1: source storage-vector
 > index1: start index in dv1
 > dv2: destination storage-vector
 > index2: start index in dv2
 > count: number of elements to be copied, > 0
 can trigger GC - if both are strings and dv1 is wider than dv2 */
global /*maygc*/ void elt_move (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count);
local void elt_move_T (object dv1, uintL index1,
                       object dv2, uintL index2, uintL count) {
  if (eq(dv1,dv2) && index1 < index2 && index2 < index1+count) {
    var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1+count];
    var gcv_object_t* ptr2 = &TheSvector(dv2)->data[index2+count];
    dotimespL(count,count, {
      *--ptr2 = *--ptr1;
    });
  } else {
    var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
    var gcv_object_t* ptr2 = &TheSvector(dv2)->data[index2];
    dotimespL(count,count, {
      *ptr2++ = *ptr1++;
    });
  }
}
local /*maygc*/ void elt_move_Char (object dv1, uintL index1,
                                    object dv2, uintL index2, uintL count) {
  GCTRIGGER_IF(sstring_eltype(TheSstring(dv1)) > sstring_eltype(TheSstring(dv2)),
               GCTRIGGER2(dv1,dv2));
  if (simple_nilarray_p(dv2)) fehler_nilarray_store();
  check_sstring_mutable(dv2);
  if (eq(dv1,dv2) && index1 < index2 && index2 < index1+count) {
    SstringDispatch(dv1,X, {
      var const cintX* ptr1 = &((SstringX)TheVarobject(dv1))->data[index1+count];
      var cintX* ptr2 = &((SstringX)TheVarobject(dv2))->data[index2+count];
      dotimespL(count,count, {
        *--ptr2 = *--ptr1;
      });
    });
  } else {
    /* Too large to inline. */
    elt_copy_Char_Char(dv1,index1,dv2,index2,count);
  }
}
local void elt_move_Bit (object dv1, uintL index1,
                         object dv2, uintL index2, uintL count) {
  if (eq(dv1,dv2) && index1 < index2+64 && index2 < index1+count+64) {
    if (index1 < index2 && index2 < index1+count) {
      index1 += count;
      index2 += count;
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
      dotimespL(count,count, {
        ptr1 -= ((index1%8)==0);
        index1--;
        ptr2 -= ((index2%8)==0);
        index2--;
        var uint8 value = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
        *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
      });
    } else {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
      dotimespL(count,count, {
        var uint8 value = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
        *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
        index1++;
        ptr1 += ((index1%8)==0);
        index2++;
        ptr2 += ((index2%8)==0);
      });
    }
  } else
    elt_copy_Bit_Bit(dv1,index1,dv2,index2,count);
}
local void elt_move_2Bit (object dv1, uintL index1,
                          object dv2, uintL index2, uintL count) {
  if (eq(dv1,dv2) && index1 < index2+32 && index2 < index1+count+32) {
    if (index1 < index2 && index2 < index1+count) {
      index1 += count;
      index2 += count;
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
      dotimespL(count,count, {
        ptr1 -= ((index1%4)==0);
        index1--;
        ptr2 -= ((index2%4)==0);
        index2--;
        var uint8 value = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
        *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
      });
    } else {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
      dotimespL(count,count, {
        var uint8 value = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
        *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
        index1++;
        ptr1 += ((index1%4)==0);
        index2++;
        ptr2 += ((index2%4)==0);
      });
    }
  } else
    elt_copy_2Bit_2Bit(dv1,index1,dv2,index2,count);
}
local void elt_move_4Bit (object dv1, uintL index1,
                          object dv2, uintL index2, uintL count) {
  if (eq(dv1,dv2) && index1 < index2+16 && index2 < index1+count+16) {
    if (index1 < index2 && index2 < index1+count) {
      index1 += count;
      index2 += count;
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
      dotimespL(count,count, {
        ptr1 -= ((index1%2)==0);
        index1--;
        ptr2 -= ((index2%2)==0);
        index2--;
        var uint8 value = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
        *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
      });
    } else {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
      dotimespL(count,count, {
        var uint8 value = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
        *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
        index1++;
        ptr1 += ((index1%2)==0);
        index2++;
        ptr2 += ((index2%2)==0);
      });
    }
  } else
    elt_copy_4Bit_4Bit(dv1,index1,dv2,index2,count);
}
local void elt_move_8Bit (object dv1, uintL index1,
                          object dv2, uintL index2, uintL count) {
  if (eq(dv1,dv2) && index1 < index2 && index2 < index1+count) {
    var const uint8* ptr1 = &TheSbvector(dv1)->data[index1+count];
    var uint8* ptr2 = &TheSbvector(dv2)->data[index2+count];
    dotimespL(count,count, {
      *--ptr2 = *--ptr1;
    });
  } else {
    var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
    var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
    dotimespL(count,count, {
      *ptr2++ = *ptr1++;
    });
  }
}
local void elt_move_16Bit (object dv1, uintL index1,
                           object dv2, uintL index2, uintL count) {
  if (eq(dv1,dv2) && index1 < index2 && index2 < index1+count) {
    var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1+count];
    var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2+count];
    dotimespL(count,count, {
      *--ptr2 = *--ptr1;
    });
  } else {
    var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
    var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
    dotimespL(count,count, {
      *ptr2++ = *ptr1++;
    });
  }
}
local void elt_move_32Bit (object dv1, uintL index1,
                           object dv2, uintL index2, uintL count) {
  if (eq(dv1,dv2) && index1 < index2 && index2 < index1+count) {
    var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1+count];
    var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2+count];
    dotimespL(count,count, {
      *--ptr2 = *--ptr1;
    });
  } else {
    var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
    var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
    dotimespL(count,count, {
      *ptr2++ = *ptr1++;
    });
  }
}
global /*maygc*/ void elt_move (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count) {
  GCTRIGGER_IF(simple_string_p(dv1) && simple_string_p(dv2)
               && sstring_eltype(TheSstring(dv1)) > sstring_eltype(TheSstring(dv2)),
               GCTRIGGER2(dv1,dv2));
  ASSERT(Array_type(dv1) == Array_type(dv2));
  switch (Array_type(dv1)) {
    case Array_type_svector: /* Simple-Vector */
      elt_move_T(dv1,index1,dv2,index2,count);
      break;
    case Array_type_sbvector: /* Simple-Bit-Vector */
      elt_move_Bit(dv1,index1,dv2,index2,count);
      break;
    case Array_type_sb2vector:
      elt_move_2Bit(dv1,index1,dv2,index2,count);
      break;
    case Array_type_sb4vector:
      elt_move_4Bit(dv1,index1,dv2,index2,count);
      break;
    case Array_type_sb8vector:
      elt_move_8Bit(dv1,index1,dv2,index2,count);
      break;
    case Array_type_sb16vector:
      elt_move_16Bit(dv1,index1,dv2,index2,count);
      break;
    case Array_type_sb32vector:
      elt_move_32Bit(dv1,index1,dv2,index2,count);
      break;
    case Array_type_sstring: /* Simple-String */
      elt_move_Char(dv1,index1,dv2,index2,count);
      break;
    case Array_type_snilvector:
      return;
    default: NOTREACHED;
  }
}

/* Function: Fills a slice of an array with an element.
 elt_fill(dv,index,count,element)
 > dv: destination storage-vector
 > index: start index in dv
 > count: number of elements to be filled
 < result: true if element does not fit, false when done
 can trigger GC */
global maygc bool elt_fill (object dv, uintL index, uintL count, object element) {
#define SIMPLE_FILL(p,c,e)    dotimespL(c,c, { *p++ = e; })
  switch (Array_type(dv)) {
    case Array_type_svector: /* Simple-Vector */
      if (count > 0) {
        var gcv_object_t* ptr = &TheSvector(dv)->data[index];
        SIMPLE_FILL(ptr,count,element);
      }
      break;
  #if 0 /* unoptimized */
    case Array_type_sbvector: /* Simple-Bit-Vector */
      if (!uint1_p(element)) return true;
      if (count > 0) {
        var uint8 x = I_to_uint8(element);
        var uint8* ptr = &TheSbvector(dv)->data[index/8];
        dotimespL(count,count, {
          *ptr ^= (*ptr ^ (x << ((~index)%8))) & ((bit(1)-1) << ((~index)%8));
          index++;
          ptr += ((index%8)==0);
        });
      }
      break;
    case Array_type_sb2vector:
      if (!uint2_p(element)) return true;
      if (count > 0) {
        var uint8 x = I_to_uint8(element);
        var uint8* ptr = &TheSbvector(dv)->data[index/4];
        dotimespL(count,count, {
          *ptr ^= (*ptr ^ (x << (2*((~index)%4)))) & ((bit(2)-1) << (2*((~index)%4)));
          index++;
          ptr += ((index%4)==0);
        });
      }
      break;
    case Array_type_sb4vector:
      if (!uint4_p(element)) return true;
      if (count > 0) {
        var uint8 x = I_to_uint8(element);
        var uint8* ptr = &TheSbvector(dv)->data[index/2];
        dotimespL(count,count, {
          *ptr ^= (*ptr ^ (x << (4*((~index)%2)))) & ((bit(4)-1) << (4*((~index)%2)));
          index++;
          ptr += ((index%2)==0);
        });
      }
      break;
    case Array_type_sb8vector:
      if (!uint8_p(element)) return true;
      if (count > 0) {
        var uint8 x = I_to_uint8(element);
        var uint8* ptr = &TheSbvector(dv)->data[index];
        SIMPLE_FILL(ptr,count,x);
      }
      break;
    case Array_type_sb16vector:
      if (!uint16_p(element)) return true;
      if (count > 0) {
        var uint16 x = I_to_uint16(element);
        var uint16* ptr = &((uint16*)&TheSbvector(dv)->data[0])[index];
        SIMPLE_FILL(ptr,count,x);
      }
      break;
    case Array_type_sb32vector:
      if (!uint32_p(element)) return true;
      if (count > 0) {
        var uint32 x = I_to_uint32(element);
        var uint32* ptr = &((uint32*)&TheSbvector(dv)->data[0])[index];
        SIMPLE_FILL(ptr,count,x);
      }
      break;
  #else /* optimized: use 32-bit accesses where possible */
      var uint32 x;
    case Array_type_sbvector: /* Simple-Bit-Vector */
      if (!uint1_p(element)) return true;
      if (count == 0) break;
      x = I_to_uint8(element);
      x |= x << 1;
      x |= x << 2;
      x |= x << 4;
      if (index & 7) {
        var uint8* ptr = &TheSbvector(dv)->data[index/8];
        var uintL i = 8-(index&7);
        if (i >= count) {
          *ptr ^= (*ptr ^ x) & (bit(i)-bit(i-count));
          break;
        }
        *ptr ^= (*ptr ^ x) & (bit(i)-1);
        count -= i;
        index += i;
      }
      index = index/8;
      if (count & 7) {
        var uint8* ptr = &TheSbvector(dv)->data[index+count/8];
        *ptr ^= (*ptr ^ x) & (bit(8)-bit(8-(count&7)));
        count = count & ~7;
        if (count == 0) break;
      }
      count = count/8;
      goto store8;
    case Array_type_sb2vector:
      if (!uint2_p(element)) return true;
      if (count == 0) break;
      x = I_to_uint8(element);
      x |= x << 2;
      x |= x << 4;
      if (index & 3) {
        var uint8* ptr = &TheSbvector(dv)->data[index/4];
        var uintL i = 4-(index&3);
        if (i >= count) {
          *ptr ^= (*ptr ^ x) & (bit(2*i)-bit(2*(i-count)));
          break;
        }
        *ptr ^= (*ptr ^ x) & (bit(2*i)-1);
        count -= i;
        index += i;
      }
      index = index/4;
      if (count & 3) {
        var uint8* ptr = &TheSbvector(dv)->data[index+count/4];
        *ptr ^= (*ptr ^ x) & (bit(8)-bit(8-2*(count&3)));
        count = count & ~3;
        if (count == 0) break;
      }
      count = count/4;
      goto store8;
    case Array_type_sb4vector:
      if (!uint4_p(element)) return true;
      if (count == 0) break;
      x = I_to_uint8(element);
      x |= x << 4;
      if (index & 1) {
        var uint8* ptr = &TheSbvector(dv)->data[index/2];
        *ptr ^= (*ptr ^ x) & (bit(4)-1);
        index++;
        if (--count == 0) break;
      }
      index = index/2;
      if (count & 1) {
        var uint8* ptr = &TheSbvector(dv)->data[index+count/2];
        *ptr ^= (*ptr ^ x) & (bit(8)-bit(4));
        if (--count == 0) break;
      }
      count = count/2;
      goto store8;
    case Array_type_sb8vector:
      if (!uint8_p(element)) return true;
      if (count == 0) break;
      x = I_to_uint8(element);
    store8:
      if (index & 1) {
        TheSbvector(dv)->data[index] = x;
        index++;
        if (--count == 0) break;
      }
      if (count & 1) {
        TheSbvector(dv)->data[index+count-1] = x;
        if (--count == 0) break;
      }
      count = count/2;
      index = index/2;
      x |= x << 8;
      goto store16;
    case Array_type_sb16vector:
      if (!uint16_p(element)) return true;
      if (count == 0) break;
      x = I_to_uint16(element);
    store16:
      if (index & 1) {
        ((uint16*)&TheSbvector(dv)->data[0])[index] = x;
        index++;
        if (--count == 0) break;
      }
      if (count & 1) {
        ((uint16*)&TheSbvector(dv)->data[0])[index+count-1] = x;
        if (--count == 0) break;
      }
      count = count/2;
      index = index/2;
      x |= x << 16;
      goto store32;
    case Array_type_sb32vector:
      if (!uint32_p(element)) return true;
      if (count == 0) break;
      x = I_to_uint32(element);
  store32:
      {
        var uint32* ptr = &((uint32*)&TheSbvector(dv)->data[0])[index];
        SIMPLE_FILL(ptr,count,x);
      }
      break;
  #endif
    case Array_type_sstring: /* Simple-String */
      if (!charp(element)) return true;
      if (count > 0) {
        sstring_un_realloc(dv);
        check_sstring_mutable(dv);
        var chart c = char_code(element);
        /* The first store can cause reallocation, the remaining ones cannot. */
        dv = sstring_store(dv,index++,c);
        sstring_un_realloc1(dv); /* reallocated? */
        if (--count > 0) {
          SstringDispatch(dv,X, {
            var cintX* ptr = &((SstringX)TheVarobject(dv))->data[index];
            SIMPLE_FILL(ptr,count,as_cint(c));
          });
        }
      }
      break;
    case Array_type_snilvector: /* (VECTOR NIL) */
      return true;
    default: NOTREACHED;
  }
  return false;
#undef SIMPLE_FILL
}

/* Function: Reverses a slice of an array, copying it into another array
 of the same element type.
 elt_reverse(dv1,index1,dv2,index2,count);
 > dv1: source storage-vector
 > index1: start index in dv1
 > dv2: destination storage-vector
 > index2: start index in dv2
 > count: number of elements to be copied, > 0
 can trigger GC */
global maygc void elt_reverse (object dv1, uintL index1, object dv2, uintL index2,
                               uintL count) {
#define SIMPLE_REVERSE(p1,p2,c)   dotimespL(c,c, { *p2-- = *p1++; })
  index2 += count-1;
  switch (Array_type(dv1)) {
    case Array_type_svector: { /* Simple-Vector */
      var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
      var gcv_object_t* ptr2 = &TheSvector(dv2)->data[index2];
      SIMPLE_REVERSE(ptr1,ptr2,count);
    }
      break;
    case Array_type_sbvector: { /* Simple-Bit-Vector */
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/8];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/8];
      dotimespL(count,count, {
        var uint8 value = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
        *ptr2 ^= (*ptr2 ^ (value << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
        index1++;
        ptr1 += ((index1%8)==0);
        ptr2 -= ((index2%8)==0);
        index2--;
      });
    }
      break;
    case Array_type_sb2vector: {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/4];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/4];
      dotimespL(count,count, {
        var uint8 value = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
        *ptr2 ^= (*ptr2 ^ (value << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
        index1++;
        ptr1 += ((index1%4)==0);
        ptr2 -= ((index2%4)==0);
        index2--;
      });
    }
      break;
    case Array_type_sb4vector: {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1/2];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2/2];
      dotimespL(count,count, {
        var uint8 value = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
        *ptr2 ^= (*ptr2 ^ (value << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
        index1++;
        ptr1 += ((index1%2)==0);
        ptr2 -= ((index2%2)==0);
        index2--;
      });
    }
      break;
    case Array_type_sb8vector: {
      var const uint8* ptr1 = &TheSbvector(dv1)->data[index1];
      var uint8* ptr2 = &TheSbvector(dv2)->data[index2];
      SIMPLE_REVERSE(ptr1,ptr2,count);
    }
      break;
    case Array_type_sb16vector: {
      var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
      var uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
      SIMPLE_REVERSE(ptr1,ptr2,count);
    }
      break;
    case Array_type_sb32vector: {
      var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
      var uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
      SIMPLE_REVERSE(ptr1,ptr2,count) ;
    }
      break;
    case Array_type_sstring: { /* Simple-String */
      check_sstring_mutable(dv2);
      SstringCase(dv1,{
        var const cint8* ptr1 = &TheS8string(dv1)->data[index1];
        SstringCase(dv2,{
          var cint8* ptr2 = &TheS8string(dv2)->data[index2];
          dotimespL(count,count, {
            *ptr2-- = *ptr1++;
          });
        },{
          var cint16* ptr2 = &TheS16string(dv2)->data[index2];
          dotimespL(count,count, {
                *ptr2-- = *ptr1++;
          });
        },{
          var cint32* ptr2 = &TheS32string(dv2)->data[index2];
          dotimespL(count,count, {
            *ptr2-- = *ptr1++;
          });
        },{
          fehler_nilarray_store();
        });
      },{
       restart16:
        SstringCase(dv2,{
          pushSTACK(dv1);
          for (;;) {
            var chart ch = as_chart(TheS16string(dv1)->data[index1++]);
            dv2 = sstring_store(dv2,index2--,ch);
            if (--count == 0)
              break;
            if (sstring_reallocatedp(TheSstring(dv2))) { /* reallocated? */
              dv2 = TheSistring(dv2)->data;
              dv1 = popSTACK();
              goto restart16;
            }
          }
          skipSTACK(1);
        },{
          var const cint16* ptr1 = &TheS16string(dv1)->data[index1];
          var cint16* ptr2 = &TheS16string(dv2)->data[index2];
          dotimespL(count,count, {
            *ptr2-- = *ptr1++;
          });
        },{
          var const cint16* ptr1 = &TheS16string(dv1)->data[index1];
          var cint32* ptr2 = &TheS32string(dv2)->data[index2];
          dotimespL(count,count, {
            *ptr2-- = *ptr1++;
          });
        },{
          fehler_nilarray_store();
        });
      },{
       restart32:
        SstringCase(dv2,{
          pushSTACK(dv1);
          for (;;) {
            var chart ch = as_chart(TheS32string(dv1)->data[index1++]);
            dv2 = sstring_store(dv2,index2--,ch);
            if (--count == 0)
              break;
            if (sstring_reallocatedp(TheSstring(dv2))) { /* reallocated? */
              dv2 = TheSistring(dv2)->data;
              dv1 = popSTACK();
              goto restart32;
            }
          }
          skipSTACK(1);
        },{
          pushSTACK(dv1);
          for (;;) {
            var chart ch = as_chart(TheS32string(dv1)->data[index1++]);
            dv2 = sstring_store(dv2,index2--,ch);
            if (--count == 0)
              break;
            if (sstring_reallocatedp(TheSstring(dv2))) { /* reallocated? */
              dv2 = TheSistring(dv2)->data;
              dv1 = popSTACK();
              goto restart32;
            }
          }
          skipSTACK(1);
        },{
          var const cint32* ptr1 = &TheS32string(dv1)->data[index1];
          var cint32* ptr2 = &TheS32string(dv2)->data[index2];
          dotimespL(count,count, {
            *ptr2-- = *ptr1++;
          });
        },{
          fehler_nilarray_store();
        });
      },{
        fehler_nilarray_retrieve();
      });
    }
      break;
    case Array_type_snilvector:
      fehler_nilarray_retrieve();
    default: NOTREACHED;
  }
#undef SIMPLE_REVERSE
}

/* Function: Reverses a slice of an array destructively.
 elt_nreverse(dv,index,count);
 > dv: storage-vector
 > index: start index in dv
 > count: number of elements to be reversed, > 0 */
global void elt_nreverse (object dv, uintL index, uintL count) {
#define SIMPLE_NREVERSE(TYPE,p1,p2,c)  \
  dotimespL(c,c, { var TYPE tmp = *p1; *p1++ = *p2; *p2-- = tmp; })
  var uintL index1 = index;
  var uintL index2 = index+count-1;
  count = floor(count,2);
  switch (Array_type(dv)) {
    case Array_type_svector: /* Simple-Vector */
      if (count > 0) {
        var gcv_object_t* ptr1 = &TheSvector(dv)->data[index1];
        var gcv_object_t* ptr2 = &TheSvector(dv)->data[index2];
        SIMPLE_NREVERSE(gcv_object_t,ptr1,ptr2,count);
      }
      break;
    case Array_type_sbvector: /* Simple-Bit-Vector */
      if (count > 0) {
        var uint8* ptr1 = &TheSbvector(dv)->data[index1/8];
        var uint8* ptr2 = &TheSbvector(dv)->data[index2/8];
        dotimespL(count,count, {
          var uint8 x1 = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
          var uint8 x2 = (*ptr2 >> ((~index2)%8)) & (bit(1)-1);
          *ptr1 ^= (*ptr1 ^ (x2 << ((~index1)%8))) & ((bit(1)-1) << ((~index1)%8));
          *ptr2 ^= (*ptr2 ^ (x1 << ((~index2)%8))) & ((bit(1)-1) << ((~index2)%8));
          index1++;
          ptr1 += ((index1%8)==0);
          ptr2 -= ((index2%8)==0);
          index2--;
        });
      }
      break;
    case Array_type_sb2vector:
      if (count > 0) {
        var uint8* ptr1 = &TheSbvector(dv)->data[index1/4];
        var uint8* ptr2 = &TheSbvector(dv)->data[index2/4];
        dotimespL(count,count, {
          var uint8 x1 = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
          var uint8 x2 = (*ptr2 >> (2*((~index2)%4))) & (bit(2)-1);
          *ptr1 ^= (*ptr1 ^ (x2 << (2*((~index1)%4)))) & ((bit(2)-1) << (2*((~index1)%4)));
          *ptr2 ^= (*ptr2 ^ (x1 << (2*((~index2)%4)))) & ((bit(2)-1) << (2*((~index2)%4)));
          index1++;
          ptr1 += ((index1%4)==0);
          ptr2 -= ((index2%4)==0);
          index2--;
        });
      }
      break;
    case Array_type_sb4vector:
      if (count > 0) {
        var uint8* ptr1 = &TheSbvector(dv)->data[index1/2];
        var uint8* ptr2 = &TheSbvector(dv)->data[index2/2];
        dotimespL(count,count, {
          var uint8 x1 = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
          var uint8 x2 = (*ptr2 >> (4*((~index2)%2))) & (bit(4)-1);
          *ptr1 ^= (*ptr1 ^ (x2 << (4*((~index1)%2)))) & ((bit(4)-1) << (4*((~index1)%2)));
          *ptr2 ^= (*ptr2 ^ (x1 << (4*((~index2)%2)))) & ((bit(4)-1) << (4*((~index2)%2)));
          index1++;
          ptr1 += ((index1%2)==0);
          ptr2 -= ((index2%2)==0);
          index2--;
        });
      }
      break;
    case Array_type_sb8vector:
      if (count > 0) {
        var uint8* ptr1 = &TheSbvector(dv)->data[index1];
        var uint8* ptr2 = &TheSbvector(dv)->data[index2];
        SIMPLE_NREVERSE(uint8,ptr1,ptr2,count);
      }
      break;
    case Array_type_sb16vector:
      if (count > 0) {
        var uint16* ptr1 = &((uint16*)&TheSbvector(dv)->data[0])[index1];
        var uint16* ptr2 = &((uint16*)&TheSbvector(dv)->data[0])[index2];
        SIMPLE_NREVERSE(uint16,ptr1,ptr2,count);
      }
      break;
    case Array_type_sb32vector:
      if (count > 0) {
        var uint32* ptr1 = &((uint32*)&TheSbvector(dv)->data[0])[index1];
        var uint32* ptr2 = &((uint32*)&TheSbvector(dv)->data[0])[index2];
        SIMPLE_NREVERSE(uint32,ptr1,ptr2,count);
      }
      break;
    case Array_type_sstring: /* Simple-String */
      check_sstring_mutable(dv);
      if (count > 0) {
        SstringDispatch(dv,X, {
          var cintX* ptr1 = &((SstringX)TheVarobject(dv))->data[index1];
          var cintX* ptr2 = &((SstringX)TheVarobject(dv))->data[index2];
          SIMPLE_NREVERSE(cintX,ptr1,ptr2,count);
        });
      }
      break;
    case Array_type_snilvector:
      fehler_nilarray_retrieve();
    default: NOTREACHED;
  }
#undef SIMPLE_NREVERSE
}

/* ======================================================================== */
/* Fill pointers, extendable vectors */

/* Function: Tests whether an array has a fill-pointer.
 array_has_fill_pointer_p(array)
 > array: ein Array
 < result: true, if it has a fill-pointer, else false. */
global bool array_has_fill_pointer_p (object array) {
  if (simplep(array)) {
    return false;
  } else {
    if (Iarray_flags(array) & bit(arrayflags_fillp_bit))
      return true;
    else
      return false;
  }
}

LISPFUNNR(array_has_fill_pointer_p,1)
{ /* (ARRAY-HAS-FILL-POINTER-P array), CLTL p. 296 */
  var object array = check_array(popSTACK());
  VALUES_IF(array_has_fill_pointer_p(array));
}

/* check, if object is a vector with fill-pointer, and returns
 the address of the fill-pointer.
 *get_fill_pointer(obj) is the fill-pointer itself.
 get_fill_pointer(obj)[-1] is the length (dimension 0) of the vector. */
local uintL* get_fill_pointer (object obj) {
  /* obj must be a vector: */
  if (!vectorp(obj))
    fehler_vector(obj);
  /* must not be simple: */
  if (simplep(obj))
    goto fehler_fillp;
  /* must contain a fill-pointer: */
  if (!(Iarray_flags(obj) & bit(arrayflags_fillp_bit)))
    goto fehler_fillp;
  /* where is the fill-pointer? */
  return ((Iarray_flags(obj) & bit(arrayflags_dispoffset_bit))
          ? &TheIarray(obj)->dims[2] /* behind displaced-offset and dimension 0 */
          : &TheIarray(obj)->dims[1]); /* behind dimension 0 */
 fehler_fillp:
  /* error-message: */
  pushSTACK(obj); /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_vector_with_fill_pointer)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: vector ~S has no fill pointer"));
}

LISPFUNNR(fill_pointer,1) { /* (FILL-POINTER vector), CLTL p. 296 */
  var object obj = popSTACK();
  VALUES1(fixnum(* get_fill_pointer(obj))); /* get fill-pointer as Fixnum */
}

LISPFUNN(set_fill_pointer,2)
{ /* (SYS::SET-FILL-POINTER vector index)
   = (SETF (FILL-POINTER vector) index), CLTL p. 296 */
  var uintL* fillp = get_fill_pointer(STACK_1); /* fillpointer-address */
  if (!posfixnump(STACK_0)) /* new fill-pointer must be fixnum>=0 . */
    fehler_index_type(STACK_1);
  var uintV newfillp = posfixnum_to_V(STACK_0); /* as uintL */
  if (!(newfillp <= fillp[-1])) /* must be <= length */
    fehler_index_range(STACK_1,fillp[-1]+1);
  *fillp = newfillp; /* store new fill-pointer */
  VALUES1(STACK_0); /* return new fill-pointer */
  skipSTACK(2);
}

LISPFUNN(vector_push,2) /* (VECTOR-PUSH new-element vector), CLTL p. 296 */
{
  var uintL* fillp = get_fill_pointer(STACK_0); /* fillpointer-address */
  var uintL oldfillp = *fillp; /* old value of the fillpointer */
  if (oldfillp >= fillp[-1]) { /* Fill-Pointer at the end? */
    VALUES1(NIL); /* return NIL */
  } else {
    var uintL index = oldfillp;
    var object datenvektor = iarray_displace(STACK_0,&index);
    storagevector_store(datenvektor,index,STACK_1,true); /* store new-element */
    fillp = get_fill_pointer(STACK_0); /* fill pointer address, again */
    (*fillp)++; /* increase fill-pointer */
    VALUES1(fixnum(oldfillp));
    /* old fill-pointer as vaue */
  }
  skipSTACK(2);
}

LISPFUNN(vector_pop,1) /* (VECTOR-POP vector), CLTL p. 296 */
{
  var object array = popSTACK();
  var uintL* fillp = get_fill_pointer(array);
  if (*fillp==0) {
    /* fill-pointer was =0 -> error-message */
    pushSTACK(array); pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: ~S has length zero"));
  } else {
    var uintL index = --(*fillp); /* decrease fill-pointer */
    var object datenvektor = iarray_displace(array,&index);
    VALUES1(storagevector_aref(datenvektor,index)); /* return element */
  }
}

/* Vector will be too long -> error */
nonreturning_function(local, fehler_extension, (object extension)) {
  pushSTACK(extension); pushSTACK(TheSubr(subr_self)->name);
  fehler(error,
         GETTEXT("~S: extending the vector by ~S elements makes it too long"));
}

LISPFUN(vector_push_extend,seclass_default,2,1,norest,nokey,0,NIL)
{ /* (VECTOR-PUSH-EXTEND new-element vector [extension]), CLTL p. 296 */
  var uintL* fillp = get_fill_pointer(STACK_1); /* fillpointer-address */
  var uintL oldfillp = *fillp; /* old value of the fillpointer */
  if (oldfillp < fillp[-1]) { /* fill-pointer not yet at the end? */
    skipSTACK(1);
    var uintL index = oldfillp;
    var object datenvektor = iarray_displace(STACK_0,&index);
    storagevector_store(datenvektor,index,STACK_1,true); /* store new-element */
    fillp = get_fill_pointer(STACK_0); /* fill pointer address, again */
    (*fillp)++; /* increase fill-pointer */
  } else { /* fill-pointer at the end -> try to extend the vector: */
    var object extension = popSTACK();
    var object array = STACK_0;
    if (!(Iarray_flags(array) & bit(arrayflags_adjustable_bit))) {
      /* vector not adjustable -> error-message: */
      /* array still in STACK_0 */
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,GETTEXT("~S works only on adjustable arrays, not on ~S"));
    }
    var uintB atype = Iarray_flags(array) & arrayflags_atype_mask;
    var uintL len = fillp[-1]; /* former length (dimension 0) */
    var uintV inc; /* desired increment of the length */
    if (boundp(extension)) {
      /* extension should be a fixnum >0, <arraysize_limit : */
      if ( !posfixnump(extension)
           || ((inc = posfixnum_to_V(extension)) == 0)
          #ifndef UNIX_DEC_ULTRIX_GCCBUG
           || (inc > arraysize_limit_1)
          #endif
           ) {
        pushSTACK(extension); /* TYPE-ERROR slot DATUM */
        pushSTACK(O(type_posfixnum1)); /* TYPE-ERROR slot EXPECTED-TYPE */
        pushSTACK(extension); pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,
               GETTEXT("~S: extension ~S should be a positive fixnum"));
      }
    } else {
      /* default-extension: */
      switch (atype) {
        case Atype_NIL: case Atype_T:
          inc = 16; break; /* for general-vectors: 16 objects */
        case Atype_Char: inc =  64; break; /* for strings: 64 characters */
        case Atype_Bit:  inc = 128; break; /* for bit-vectors: 128 bits */
        case Atype_2Bit: case Atype_4Bit: case Atype_8Bit:
        case Atype_16Bit: case Atype_32Bit: /* for byte-vectors: accordingly */
          inc = bit(floor(14-atype,2)); break;
        default: NOTREACHED;
      }
      /* but at least the former length: */
      if (inc<len)
        inc = len;
      extension = UV_to_I(inc);
    }
    var uintV newlen = len + inc; /* new length */
   #ifndef UNIX_DEC_ULTRIX_GCCBUG
    if (newlen > arraysize_limit_1)
      fehler_extension(extension);
   #endif
    /* fetch new data vector. Distinguish cases according to type: */
    var object neuer_datenvektor;
    switch (atype) {
      case Atype_T: /* array is a general-vector */
        neuer_datenvektor = allocate_vector(newlen);
        array = STACK_0; /* fetch array again */
        /* copy old into the new data vector: */
        if (len>0) {
          var uintL index = 0;
          var object datenvektor = iarray_displace_check(array,len,&index);
          elt_copy_T_T(datenvektor,index,neuer_datenvektor,0,len);
        }
        /* then append new_element: */
        TheSvector(neuer_datenvektor)->data[len] = STACK_1;
        break;
      case Atype_Char: /* array is a string */
        if (newlen > stringsize_limit_1)
          fehler_extension(extension);
        neuer_datenvektor = allocate_string(newlen);
        array = STACK_0; /* fetch array again */
        /* copy old into the new data vector: */
        if (len>0) {
          var uintL index = 0;
          var object datenvektor = iarray_displace_check(array,len,&index);
          elt_copy_Char_Char(datenvektor,index,neuer_datenvektor,0,len);
        }
        /* then append new_element: */
        if (!charp(STACK_1))
          goto fehler_type;
        TheSnstring(neuer_datenvektor)->data[len] = char_code(STACK_1);
        break;
      case Atype_Bit: /* array is a bit-vector */
      case Atype_2Bit: case Atype_4Bit: case Atype_8Bit:
      case Atype_16Bit: case Atype_32Bit: /* array is a byte-vector */
        neuer_datenvektor = allocate_bit_vector(atype,newlen);
        array = STACK_0; /* fetch array */
        /* copy old into the new data vector: */
        if (len>0) {
          var uintL index = 0;
          var object datenvektor = iarray_displace_check(array,len,&index);
          switch (atype) {
            case Atype_Bit:
            case Atype_2Bit:
            case Atype_4Bit:
              bit_copy(datenvektor,index<<atype,neuer_datenvektor,0<<atype,len<<atype);
              break;
            case Atype_8Bit:
              elt_copy_8Bit_8Bit(datenvektor,index,neuer_datenvektor,0,len);
              break;
            case Atype_16Bit:
              elt_copy_16Bit_16Bit(datenvektor,index,neuer_datenvektor,0,len);
              break;
            case Atype_32Bit:
              elt_copy_32Bit_32Bit(datenvektor,index,neuer_datenvektor,0,len);
              break;
            default: NOTREACHED;
          }
        }
        /* store new-element: */
        storagevector_store(neuer_datenvektor,len,STACK_1,false);
        break;
      case Atype_NIL: goto fehler_type;
      default: NOTREACHED;
      fehler_type: {
        /* stack layout: new-element, vector. */
        pushSTACK(STACK_1); /* TYPE-ERROR slot DATUM */
        pushSTACK(array_element_type(STACK_(0+1))); /* TYPE-ERROR slot EXPECTED-TYPE */
        pushSTACK(STACK_(0+2)); pushSTACK(STACK_(1+3));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(type_error,GETTEXT("~S: cannot push ~S into array ~S (bad type)"));
      }
    }
    set_break_sem_1(); /* forbid interrupts */
    TheIarray(array)->data = neuer_datenvektor; /* store new vector as data vector */
    iarray_flags_clr(TheIarray(array),bit(arrayflags_displaced_bit)); /* delete displaced-bit */
    TheIarray(array)->dims[2] += 1; /* increase fillpointer by 1 */
    TheIarray(array)->dims[1] = newlen; /* store new length */
    TheIarray(array)->totalsize = newlen; /* is also the new totalsize */
    clr_break_sem_1(); /* permit interrupts again */
  }
  VALUES1(fixnum(oldfillp));
  /* old fill-pointer as value */
  skipSTACK(2);
}

/* ======================================================================== */
/* Bit vectors */

/* Function: Allocates a new simple-bit-vector, filled with zeroes.
 allocate_bit_vector_0(len)
 > uintL len: length of the desired bit-vector (number of bits)
 < result: fresh simple-bit-vector, filled with zeroes
 can trigger GC */
global maygc object allocate_bit_vector_0 (uintL len) {
  var object newvec = allocate_bit_vector(Atype_Bit,len); /* new bit-vector */
  var uintL count = ceiling(len,bitpack); /* fill ceiling(len/bitpack) words with zeroes */
  if (count!=0) {
    var uint_bitpack_t* ptr = (uint_bitpack_t*)(&TheSbvector(newvec)->data[0]);
    dotimespL(count,count, {
      *ptr++ = 0;
    });
  }
  return newvec;
}

#if 0 /* only as reserve, in case, that we encounter a GCC-bug again */

/* UP: deletes a bit in a simple-bit-vector
 sbvector_bclr(sbvector,index);
 > sbvector: a simple-bit-vector
 > index: index (variable, should be < (length sbvector) ) */
global void sbvector_bclr (object sbvector, uintL index) {
  /* in byte (index div 8), delete the bit 7 - (index mod 8) : */
  TheSbvector(sbvector)->data[index/8] &= ~bit((~index) % 8);
}

/* UP: sets a bit in a simple-bit-vector
 sbvector_bset(sbvector,index);
 > sbvector: a simple-bit-vector
 > index: index (variable, should be < (length sbvector) ) */
global void sbvector_bset (object sbvector, uintL index) {
  /* in byte (index div 8), set the bit 7 - (index mod 8) : */
  TheSbvector(sbvector)->data[index/8] |= bit((~index) % 8);
}

#endif

/* error: bad dimension
 > dim: wrong dimension */
nonreturning_function(local, fehler_dim_type, (object dim)) {
  pushSTACK(dim); /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_array_index)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(dim);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: dimension ~S is not of type `(INTEGER 0 (,ARRAY-DIMENSION-LIMIT))"));
}

/* ======================================================================== */
/* Semi-simple strings */

/* The following functions work on "semi-simple string"s.
 That are CHARACTER arrays with FILL-POINTER, (pro forma) not adjustable and
 not displaced, whose storagevector is a normal-simple-string. When their
 length is exceeded, the length is doubled (so that the resizing effort
 becomes unimportant: adding a character is still O(1) on average.) */

/* Function: Returns a fresh semi-simple-string of given length, with
 fill-pointer = 0.
 make_ssstring(len)
 > uintL len: desired length, must be >0
 < fresh: fresh semi-simple-string of the given length
 can trigger GC */
global maygc object make_ssstring (uintL len) {
  if (len > arraysize_limit_1)
    fehler_dim_type(UL_to_I(len));
  if (len > stringsize_limit_1)
    fehler_stringsize(len);
  pushSTACK(allocate_string(len));
  var object new_array =
    allocate_iarray(bit(arrayflags_fillp_bit)|Atype_Char,1,Array_type_string);
  /* Flags: only FILL_POINTER_BIT, element type CHARACTER, rank=1 */
  TheIarray(new_array)->dims[1] = 0; /* fill-pointer := 0 */
  TheIarray(new_array)->totalsize =
    TheIarray(new_array)->dims[0] = len; /* length and total-size */
  TheIarray(new_array)->data = popSTACK(); /* data vector */
  return new_array;
}

/* Function: extend the string to length <= arraysize_limit_1
 > ssstring: a semi-simple-string
 > size:     how much to allocate
 < returns:  the same semi-simple-string
 can trigger GC */
local maygc object ssstring_extend_low (object ssstring, uintL size) {
  if (size > arraysize_limit_1)
    fehler_dim_type(UL_to_I(size));
  if (size > stringsize_limit_1)
    fehler_stringsize(size);
  pushSTACK(ssstring);
  var object new_data = allocate_string(size);
  ssstring = popSTACK();
  if (TheIarray(ssstring)->dims[1] > 0) {
    #ifdef UNICODE
    copy_32bit_32bit
    #else
    copy_8bit_8bit
    #endif
      (TheS32string(TheIarray(ssstring)->data)->data,
       TheS32string(new_data)->data,TheIarray(ssstring)->dims[1]);
  }
  set_break_sem_1(); /* forbid interrupts */
  TheIarray(ssstring)->data = new_data;
  TheIarray(ssstring)->totalsize = TheIarray(ssstring)->dims[0] = size;
  clr_break_sem_1(); /* permit interrupts again */
  return ssstring;
}

/* Function: Adds a character to a semi-simple-string, thereby possibly
 extending it.
 ssstring_push_extend(ssstring,ch)
 > ssstring: a semi-simple-string
 > ch: a character
 < result: the same semi-simple-string
 can trigger GC */
global maygc object ssstring_push_extend (object ssstring, chart ch) {
  var object sstring = TheIarray(ssstring)->data; /* normal-simple-string */
  var uintL len = Sstring_length(sstring);
  if (TheIarray(ssstring)->dims[1] >= len) { /* fill-pointer >= length ? */
    len *= 2;
    if (len > arraysize_limit_1) /* cannot extend beyond arraysize_limit_1 */
      len = arraysize_limit_1;
    if (TheIarray(ssstring)->dims[1] >= len) /* still no good! */
      fehler_extension(Fixnum_1);
    ssstring = ssstring_extend_low(ssstring,len);
    sstring = TheIarray(ssstring)->data;
  }
  /* now sstring is still the data vector, and we have
     fill-pointer < length(data vector).
     push the character in and increase the fill-pointer: */
  TheSnstring(sstring)->data[ TheIarray(ssstring)->dims[1]++ ] = ch;
  return ssstring;
}

/* Function: Ensures that a semi-simple-string has at least a given length,
 possibly extending it.
 ssstring_extend(ssstring,size)
 > ssstring: a semi-simple-string
 > size: desired minimum length
 < result: the same semi-simple-string
 can trigger GC */
global maygc object ssstring_extend (object ssstring, uintL needed_len) {
  var object sstring = TheIarray(ssstring)->data; /* normal simple string */
  var uintL now_len = Sstring_length(sstring); /* current maximal lenth */
  if (needed_len > arraysize_limit_1) /* cannot extend beyond arraysize_limit_1 */
    fehler_extension(UL_to_I(needed_len-TheIarray(ssstring)->dims[1]));
  if (needed_len > now_len) {
    /* yes -> lengthen the string at least by a factor of 2: */
    now_len *= 2;
    if (now_len > arraysize_limit_1) /* cannot extend beyond arraysize_limit_1 */
      now_len = arraysize_limit_1;
    else if (needed_len > now_len)
      now_len = needed_len; /* increase now_len */
    ssstring = ssstring_extend_low(ssstring,now_len);
  }
  return ssstring;
}

/* Function: Adds a substring to a semi-simple-string, thereby possibly
 extending it.
 ssstring_append_extend(ssstring,sstring,start,len)
 > ssstring: a semi-simple-string
 > srcstring: a simple-string
 > start: the start index into the sstring
 > len: the number of characters to be pushed, starting from start; >0
 < result: the same semi-simple-string
 can trigger GC */
global maygc object ssstring_append_extend (object ssstring, object srcstring,
                                            uintL start, uintL len) {
  var uintL old_len = TheIarray(ssstring)->dims[1]; /* length = fill-pointer */
  if (old_len + len > TheIarray(ssstring)->dims[0]) { /* len bytes will not fit */
    pushSTACK(srcstring);
    ssstring = ssstring_extend(ssstring,old_len+len);
    srcstring = popSTACK();
  }
  { /* push the characters in: */
    var cint32* ptr = &TheS32string(TheIarray(ssstring)->data)->data[old_len];
    #ifdef UNICODE
    SstringCase(srcstring,
      { copy_8bit_32bit(&TheS8string(srcstring)->data[start],ptr,len); },
      { copy_16bit_32bit(&TheS16string(srcstring)->data[start],ptr,len); },
      { copy_32bit_32bit(&TheS32string(srcstring)->data[start],ptr,len); },
      { NOTREACHED; });
    #else
    copy_8bit_8bit(&TheS8string(srcstring)->data[start],ptr,len);
    #endif
  }
  /* increase the fill-pointer: */
  TheIarray(ssstring)->dims[1] += len;
  return ssstring;
}

/* ======================================================================== */
/* Semi-simple byte vectors */

/* The following functions work on "semi-simple byte-vector"s.
 That are bit vectors with FILL-POINTER, (pro forma) not adjustable and
 not displaced, whose storagevector is a simple-bit-vector. When their
 length is exceeded, the length is doubled (so that the resizing effort
 becomes unimportant: adding a character is still O(1) on average.) */

/* Function: Returns a fresh semi-simple byte-vector of given length, with
 fill-pointer = 0.
 make_ssbvector(len)
 > uintL len: length (number of bytes!), must be >0
 < result: fresh semi-simple byte-vector of the given length
 can trigger GC */
global maygc object make_ssbvector (uintL len) {
  if (len > arraysize_limit_1)
    fehler_dim_type(UL_to_I(len));
  pushSTACK(allocate_bit_vector(Atype_8Bit,len));
  var object new_array =
    allocate_iarray(bit(arrayflags_fillp_bit)|Atype_8Bit,1,Array_type_b8vector);
  /* Flags: only FILL_POINTER_BIT, element type BIT, rank=1 */
  TheIarray(new_array)->dims[1] = 0; /* fill-pointer := 0 */
  TheIarray(new_array)->totalsize =
    TheIarray(new_array)->dims[0] = len; /* length and total-size */
  TheIarray(new_array)->data = popSTACK(); /* data vector */
  return new_array;
}

/* Function: Adds a byte to a semi-simple byte vector, thereby possibly
 extending it.
 ssbvector_push_extend(ssbvector,b)
 > ssbvector: a semi-simple byte-vector
 > b: byte
 < result: the same semi-simple byte-vector
 can trigger GC */
global maygc object ssbvector_push_extend (object ssbvector, uintB b) {
  var object sbvector = TheIarray(ssbvector)->data; /* simple-8bit-vector */
  var uintL len = Sbvector_length(sbvector);
  if (TheIarray(ssbvector)->dims[1] >= len) { /* fill-pointer >= length ? */
    /* yes -> double the length of data vector */
    len *= 2;
    if (len > arraysize_limit_1) /* cannot extend beyond arraysize_limit_1 */
      len = arraysize_limit_1;
    if (TheIarray(ssbvector)->dims[1] >= len) /* still no good! */
      fehler_extension(Fixnum_1);
    pushSTACK(ssbvector); /* save ssbvector */
    pushSTACK(sbvector); /* save data vector */
    var object new_sbvector = allocate_bit_vector(Atype_8Bit,len);
    /* new simple-8bit-vector of double length */
    sbvector = popSTACK(); /* restore sbvector */
    /* copy the contents of sbvector into new_sbvector: */
    elt_copy_8Bit_8Bit(sbvector,0,new_sbvector,0,Sbvector_length(sbvector));
    ssbvector = popSTACK(); /* restore ssbvector */
    set_break_sem_1(); /* forbid interrupts */
    TheIarray(ssbvector)->data = new_sbvector; /* new bit-vector as the data */
    TheIarray(ssbvector)->totalsize = /* new length */
      TheIarray(ssbvector)->dims[0] = Sbvector_length(new_sbvector);
    clr_break_sem_1(); /* permit interrupts again */
    sbvector = new_sbvector;
  }
  /* now sbvector is still the data vector, and we have
     fill-pointer < length(data vector).
     push the byte in and increase the fill-pointer: */
  TheSbvector(sbvector)->data[ TheIarray(ssbvector)->dims[1]++ ] = b;
  return ssbvector;
}

/* ======================================================================== */
/* MAKE-ARRAY */

/* Stack layout of MAKE-ARRAY :
   dims, adjustable, element-type, initial-element, initial-contents,
   fill-pointer, displaced-to, displaced-index-offset.
 stack layout of ADJUST-ARRAY :
   dims, array, element-type, initial-element, initial-contents,
   fill-pointer, displaced-to, displaced-index-offset. */

/* auxiliary routine for MAKE-ARRAY and ADJUST-ARRAY:
 checks the dimensions and returns the rank and the total size.
 test_dims(&totalsize)
 > STACK_7: dimension or dimension list
 < totalsize: total size = product of the dimensions
 < result: Rang = number of the dimensions */
local uintL test_dims (uintL* totalsize_) {
  var object dims = STACK_7;
  if (listp(dims)) {
    var uintL rank = 0; /* number of dimensions so far */
    var uintL totalsize = 1; /* product of dimensions so far, */
    /* remains < arraysize_limit */
    while (consp(dims)) {
      var object dim = Car(dims); /* next dimension */
      /* if (!integerp(dim)) fehler_dim_type(dim); */
      if (!posfixnump(dim)) fehler_dim_type(dim); /* must be Fixnum >=0 */
     #if (oint_data_len>32)
      if (posfixnum_to_V(dim) >= vbit(32)) /* must fit in 32 bits */
        fehler_dim_type(dim);
     #endif
      /* calculate totalsize * dim: */
      var uintL produkt_hi;
      var uintL produkt_lo;
     #if (oint_data_len<=24)
      mulu24(totalsize,posfixnum_to_V(dim), produkt_hi=,produkt_lo=);
     #else
      mulu32(totalsize,posfixnum_to_V(dim), produkt_hi=,produkt_lo=);
     #endif
     #ifndef UNIX_DEC_ULTRIX_GCCBUG
      if (!((produkt_hi==0) && (produkt_lo<=arraysize_limit_1))) /* product < 2^24 ? */
     #else
      if (produkt_hi != 0)
     #endif
        { /* no -> (provided that there is not a dimension=0 )
             total-size too large */
          pushSTACK(STACK_7); /* dims */
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,GETTEXT("~S: dimensions ~S produce too large total-size"));
        }
      totalsize = produkt_lo;
      rank++;
      dims = Cdr(dims);
    }
    *totalsize_ = totalsize;
    return rank;
  } else {
    /* dims is not a list. Should be a single dimension: */
    if (!posfixnump(dims)) fehler_dim_type(dims); /* must be Fixnum >=0 */
   #if (oint_data_len>32)
    if (posfixnum_to_V(dims) >= vbit(32)) /* must fit in 32 bits */
      fehler_dim_type(dims);
   #endif
    *totalsize_ = posfixnum_to_V(dims); /* Totalsize = single dimension */
    return 1; /* Rang = 1 */
  }
}

/* auxiliary routine for MAKE-ARRAY and ADJUST-ARRAY:
 checks some of the keywords. */
local void test_otherkeys (void) {
  /* fill-pointer has default value NIL: */
  if (!boundp(STACK_2))
    STACK_2 = NIL;
  /* displaced-to has default value NIL: */
  if (!boundp(STACK_1))
    STACK_1 = NIL;
  { /* test, if more than one initialization
       (:initial-element, :initial-contents, :displaced-to) was specified: */
    var uintC initcount = 0; /* counter */
    if (boundp(STACK_4)) /* initial-element supplied? */
      initcount++;
    if (boundp(STACK_3)) /* initial-contents supplied? */
      initcount++;
    if (!nullp(STACK_1)) /* displaced-to supplied? */
      initcount++;
    if (initcount > 1) { /* more than one initialization? */
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~S: ambiguous, more than one initialization specified"));
    }
  }
  /* test, if :displaced-index-offset was used without :displaced-to: */
  if (boundp(STACK_0) /* displaced-index-offset supplied? */
      && (nullp(STACK_1))) { /* and displaced-to not supplied? */
    pushSTACK(S(Kdisplaced_to));
    pushSTACK(S(Kdisplaced_index_offset));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: ~S must not be specified without ~S"));
  }
}

/* auxiliary routine for MAKE-ARRAY and ADJUST-ARRAY:
 fills the new data vector with initial-element, if supplied.
 fill_initial_element(len,vector)
 > len: length
 > vector: data vector
 > STACK_4: initial-element
 < result: vector filled, if necessary
 can trigger GC */
local maygc object fill_initial_element (uintL len, object vector) {
  if (boundp(STACK_4) /* initial-element supplied? */
      && (len != 0)) { /* and length > 0 ? */
    pushSTACK(vector);
    if (elt_fill(vector,0,len,STACK_(4+1))) {
      pushSTACK(STACK_(4+1)); /* TYPE-ERROR slot DATUM */
      pushSTACK(STACK_(5+2)); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(STACK_(5+3)); /* element-type */
      pushSTACK(STACK_(4+4)); /* initial-element */
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,GETTEXT("~S: the initial-element ~S is not of type ~S"));
    }
    vector = popSTACK();
   #ifdef HAVE_SMALL_SSTRING
    ASSERT(!sarray_reallocstringp(vector));
   #endif
  }
  return vector;
}

/* auxiliary routine for MAKE-ARRAY and ADJUST-ARRAY:
 creates a data vector of given length
 and fills it with initial-element, if supplied.
 make_storagevector(len,eltype)
 > len: length
 > eltype: elementtype-code
 > STACK_4: initial-element
 < result: simple vector of given type, poss. filled.
 can trigger GC */
local maygc object make_storagevector (uintL len, uintB eltype) {
  var object vector;
  switch (eltype) {
    case Atype_T: /* create simple-vector */
      vector = allocate_vector(len);
      break;
    case Atype_Char: /* create simple-string */
      check_stringsize(len);
     #ifdef HAVE_SMALL_SSTRING
      if (charp(STACK_4) && len>0) {
        var cint initial_element = char_int(STACK_4);
        if (initial_element < cint8_limit)
          vector = allocate_s8string(len);
        else if (initial_element < cint16_limit)
          vector = allocate_s16string(len);
        else
          vector = allocate_s32string(len);
      } else
        vector = allocate_s8string(len);
     #else
      vector = allocate_string(len);
     #endif
      break;
    case Atype_Bit:
    case Atype_2Bit:
    case Atype_4Bit:
    case Atype_8Bit:
    case Atype_16Bit:
    case Atype_32Bit: /* create simple bit/byte-vector */
      vector = allocate_bit_vector(eltype,len);
      break;
    case Atype_NIL:
      vector = NIL;
      break;
    default: NOTREACHED;
  }
  return fill_initial_element(len,vector);
}

/* auxiliary routine for MAKE-ARRAY and ADJUST-ARRAY:
 Fills a vector lexicographically with the content of a nested
 sequence-structure, which is supplied as argument for
 keyword :initial-contents with MAKE-ARRAY and ADJUST-ARRAY.
 initial_contents(datenvektor,dims,rank,contents)
 > datenvektor: a simple vector
 > dims: dimension or dimension list, all dimensions Fixnums,
         Length(datenvektor) = product of the dimensions
 > rank: number of dimensions
 > contents: nested sequence-structure
 < result: the same data vector
 not reentrant!
 can trigger GC */
typedef struct {
  gcv_object_t* localptr; /* pointer to data vector and dimensions */
  uintL index; /* index into the data vector */
  uintL depth; /* recursion depth */
} initial_contents_locals_t;
local map_sequence_function_t initial_contents_aux;
local maygc object initial_contents (object datenvektor, object dims,
                                     uintL rank, object contents) {
  /* put all dimensions on the stack: */
  get_space_on_STACK(rank*sizeof(gcv_object_t));
  if (listp(dims)) {
    while (consp(dims)) {
      pushSTACK(Car(dims)); dims = Cdr(dims);
    }
  } else {
    pushSTACK(dims);
  }
  var initial_contents_locals_t locals;
  locals.localptr = &STACK_0; /* memorize current STACK-value */
  locals.index = 0; /* index := 0 */
  locals.depth = rank; /* depth := rank */
  pushSTACK(datenvektor); /* push data vector on Stack */
  initial_contents_aux(&locals,contents); /* call initial_contents_aux */
  datenvektor = popSTACK(); /* pop data vector */
  skipSTACK(rank); /* clean up STACK */
  return datenvektor;
}

/* auxiliary routine for initial_contents:
 processes the sequence-structure recursively. */
local maygc void initial_contents_aux (void* arg, object obj) {
  var initial_contents_locals_t* locals = (initial_contents_locals_t*)arg;
  /* the following is passed:
     locals->depth = recursion depth,
     locals->index = index into the data vector,
     locals->localptr = pointer to the dimensions,
     when Depth depth>0 :
     dimension (rank-depth) = *(localptr+depth-1),
     data vector = *(localptr-1), caller = *(localptr-2). */
  var gcv_object_t* localptr = locals->localptr;
  if (locals->depth==0) {
    /* depth 0 -> store element obj in the data vector: */
    var object datenvektor = *(localptr STACKop -1);
    pushSTACK(obj);
    pushSTACK(datenvektor);
    datenvektor = storagevector_store(datenvektor,locals->index,STACK_1,true);
   #ifdef HAVE_SMALL_SSTRING
    if (sarray_reallocstringp(datenvektor)) /* has it been reallocated? */
      *(localptr STACKop -1) = datenvektor = TheSistring(datenvektor)->data;
   #endif
    locals->index++;
    skipSTACK(2); /* clean up stack */
  } else { /* depth >0 -> call recursively: */
    locals->depth--;
    pushSTACK(obj);
    /* obj = STACK_0 must be a sequence of correct length: */
    pushSTACK(STACK_0); funcall(L(length),1); /* determine length */
    /* must be EQL (which means EQ) to dimension *(localptr+depth) : */
    if (!(eq(value1,*(localptr STACKop locals->depth)))) {
      /* defective sequence seq still in STACK_0. */
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,GETTEXT("~S: ~S is of incorrect length"));
    }
    /* length is correct, now execute (MAP NIL #'INITIAL-CONTENTS-AUX seq) : */
    map_sequence(STACK_0,&initial_contents_aux,locals);
    locals->depth++;
    skipSTACK(1); /* clean up stack */
  }
}

/* auxiliary routine for MAKE-ARRAY and ADJUST-ARRAY:
 check a displaced-to-argument and the belonging offset.
 test_displaced(eltype,totalsize)
 > eltype: elementtype-code of the creating array
 > totalsize: total size of the creating array
 < result: value of the displaced-index-offset */
local uintL test_displaced (uintB eltype, uintL totalsize) {
  /* check displaced-to, must be a array: */
  var object displaced_to = STACK_1;
  if (!arrayp(displaced_to)) {
    pushSTACK(displaced_to); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(array)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(displaced_to);
    pushSTACK(S(Kdisplaced_to));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: ~S-argument ~S is not an array"));
  }
  { /* determine element type of displaced_to: */
    var uintB displaced_eltype = array_atype(STACK_1);
    /* displaced_eltype is the ATYPE of the :displaced-to argument. */
    /* compare given element type with it: */
    if (eltype != displaced_eltype) {
      pushSTACK(displaced_to); /* TYPE-ERROR slot DATUM */
      pushSTACK(S(array)); pushSTACK(STACK_(5+2));
      { /* TYPE-ERROR slot EXPECTED-TYPE */
        object exp_type = listof(2); pushSTACK(exp_type);
      }
      pushSTACK(STACK_(5+2)); /* element-type */
      pushSTACK(STACK_2); /* displaced_to */
      pushSTACK(S(Kdisplaced_to));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,
             GETTEXT("~S: ~S-argument ~S does not have element type ~S"));
    }
  }
  /* check displaced-index-offset: */
  var uintV displaced_index_offset;
  if (!boundp(STACK_0))
    displaced_index_offset = 0; /* default is 0 */
  else if (posfixnump(STACK_0))
    displaced_index_offset = posfixnum_to_V(STACK_0);
  else {
    pushSTACK(STACK_0); /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_array_index)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(STACK_(0+2));
    pushSTACK(S(Kdisplaced_index_offset));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: ~S-argument ~S is not of type `(INTEGER 0 (,ARRAY-TOTAL-SIZE-LIMIT))"));
  }
  { /* check, if addressed sub part fits completely into displaced-to: */
    var uintL displaced_totalsize = array_total_size(displaced_to);
    if (!(displaced_index_offset+totalsize <= displaced_totalsize)) {
      pushSTACK(S(Kdisplaced_to));
      pushSTACK(fixnum(displaced_totalsize));
      pushSTACK(fixnum(displaced_index_offset));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~S: array-total-size + displaced-offset (= ~S) exceeds total size ~S of ~S-argument"));
    }
  }
  return displaced_index_offset;
}

/* auxiliary routine for MAKE-ARRAY and ADJUST-ARRAY:
 check a fill-pointer-argument /=NIL.
 test_fillpointer(len)
 > totalsize: maximum value of fill-pointer
 < result: value of the fill-pointer */
local uintL test_fillpointer (uintL totalsize) {
  /* fill-pointer was supplied and /=NIL */
  if (eq(STACK_2,S(t))) { /* T supplied -> */
    return totalsize; /* fill-pointer := length = total size */
  } else if (!posfixnump(STACK_2)) { /* no Fixnum >=0 -> error */
    pushSTACK(STACK_2); /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_posfixnum)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(STACK_(2+2));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~S: fill-pointer ~S should be a nonnegative fixnum"));
  } else {
    var uintV fillpointer = posfixnum_to_V(STACK_2);
    if (!(fillpointer <= totalsize)) { /* compare with length */
      pushSTACK(fixnum(totalsize));
      pushSTACK(STACK_(2+1));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,GETTEXT("~S: fill-pointer argument ~S is larger than the length ~S"));
    }
    return fillpointer;
  }
}

LISPFUN(make_array,seclass_read,1,0,norest,key,7,
        (kw(adjustable),kw(element_type),kw(initial_element),
         kw(initial_contents),kw(fill_pointer),
         kw(displaced_to),kw(displaced_index_offset)) )
/* (MAKE-ARRAY dimensions :adjustable :element-type :initial-element
   :initial-contents :fill-pointer :displaced-to :displaced-index-offset),
   CLTL p. 286
 stack layout:
   dims, adjustable, element-type, initial-element, initial-contents,
   fill-pointer, displaced-to, displaced-index-offset. */
{
  /* check dimensions and calculate rank and total-size: */
  var uintL totalsize;
  var uintL rank = test_dims(&totalsize);
  /* adjustable has default value NIL: */
  if (!boundp(STACK_6))
    STACK_6 = NIL;
  /* convert element-type into a code: */
  var uintB eltype;
  if (boundp(STACK_5)) {
    eltype = eltype_code(STACK_5);
  } else { /* default value is T. */
    STACK_5 = S(t); eltype = Atype_T;
  }
  test_otherkeys(); /* do some other checks */
  var uintB flags = eltype;
  var uintL displaced_index_offset;
  var uintL fillpointer;
  /* if not displaced, create data vector and poss. fill: */
  if (nullp(STACK_1)) { /* displaced-to not supplied? */
    /* create data vector: */
    var object datenvektor = make_storagevector(totalsize,eltype);
    if (boundp(STACK_3)) /* and if initial-contents supplied: */
      datenvektor = initial_contents(datenvektor,STACK_7,rank,STACK_3); /* fill */
    /* if displaced-to is not supplied
       and fill-pointer is not supplied
       and adjustable is not supplied
       and rank=1 ,
       then return a (semi-)simple vector: */
    if ((rank==1) && nullp(STACK_6) && nullp(STACK_2) && !simple_nilarray_p(datenvektor)) {
      DBGREALLOC(datenvektor);
      VALUES1(datenvektor); /* return datenvektor */
      skipSTACK(8); return;
    }
    /* return a general array. */
    STACK_1 = datenvektor; /* store datenvektor as "displaced-to" */
    displaced_index_offset = 0; /* with displacement 0 */
    /* and without displacement-bit in the flags */
  } else {
    /* displaced-to supplied -> return a general array. */
    displaced_index_offset = test_displaced(eltype,totalsize);
    /* flags contain the displacement-bit: */
    flags |= bit(arrayflags_displaced_bit)|bit(arrayflags_dispoffset_bit);
  }
  /* create a general array.
     check rank: */
 #ifndef UNIX_DEC_ULTRIX_GCCBUG
  if (rank > arrayrank_limit_1) {
    pushSTACK(fixnum(rank)); /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_array_rank)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(fixnum(rank));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: attempted rank ~S is too large"));
  }
 #endif
  /* assemble flags for allocate_iarray: */
  /* "flags" already contains eltype and poss. displacement-bit. */
  if (!nullp(STACK_6)) /* adjustable supplied? */
    flags |= bit(arrayflags_adjustable_bit)|bit(arrayflags_dispoffset_bit);
  if (!nullp(STACK_2)) { /* fill-pointer supplied? */
    if (rank!=1) { /* rank must be 1 */
      pushSTACK(fixnum(rank));
      pushSTACK(S(Kfill_pointer));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,
             GETTEXT("~S: ~S may not be specified for an array of rank ~S"));
    }
    flags |= bit(arrayflags_fillp_bit);
    fillpointer = test_fillpointer(totalsize); /* fill-pointer-value */
  }
  /* determine type info for the object to create: */
  var tint type;
  if (rank==1) {
    /* vector: get type info from table */
    local const tint type_table[arrayflags_atype_mask+1] = {
      /* table for assignment  ATYPE-byte -> vector type info */
      Array_type_bvector,   /* Atype_Bit   -> Array_type_bvector */
      Array_type_b2vector,  /* Atype_2Bit  -> Array_type_b2vector */
      Array_type_b4vector,  /* Atype_4Bit  -> Array_type_b4vector */
      Array_type_b8vector,  /* Atype_8Bit  -> Array_type_b8vector */
      Array_type_b16vector, /* Atype_16Bit -> Array_type_b16vector */
      Array_type_b32vector, /* Atype_32Bit -> Array_type_b32vector */
      Array_type_vector,    /* Atype_T     -> Array_type_vector */
      Array_type_string,    /* Atype_Char  -> Array_type_string */
      Array_type_string,    /* Atype_NIL   -> Array_type_string */
      Array_type_vector,    /* unused yet */
      Array_type_vector,    /* unused yet */
      Array_type_vector,    /* unused yet */
      Array_type_vector,    /* unused yet */
      Array_type_vector,    /* unused yet */
      Array_type_vector,    /* unused yet */
      Array_type_vector,    /* unused yet */
    };
    type = type_table[eltype];
  } else { /* general array */
    type = Array_type_mdarray;
  }
  /* allocate Array: */
  var object array = allocate_iarray(flags,rank,type);
  TheIarray(array)->totalsize = totalsize; /* store total-size */
  {
    var uintL* dimptr = &TheIarray(array)->dims[0];
    if (flags & bit(arrayflags_dispoffset_bit))
      *dimptr++ = displaced_index_offset; /* store displaced-index-offset */
    { /* store dimensions: */
      var object dims = STACK_7;
      if (listp(dims)) {
        while (consp(dims)) {
          *dimptr++ = posfixnum_to_V(Car(dims)); dims = Cdr(dims);
        }
      } else {
        *dimptr++ = posfixnum_to_V(dims);
      }
    }
    /* poss. store fill-pointer: */
    if (flags & bit(arrayflags_fillp_bit))
      /* fill-pointer was supplied and /=NIL */
      *dimptr++ = fillpointer;
  }
  /* store data vector: */
  TheIarray(array)->data = STACK_1; /* displaced-to-Argument or new data vector */
  /* array as value: */
  VALUES1(array); skipSTACK(8);
}

/* ======================================================================== */
/* ADJUST-ARRAY */

/* auxiliary function for the filling task with ADJUST-ARRAY:
 Fills the data vector of an array partly with the content of another
 data vector, so that the elements for index tuples, that are valid
 for both arrays, match.
 reshape(newvec,newdims,oldvec,olddims,offset,rank,eltype);
 > newvec: simple vector, target for filling.
 > newdims: dimension(s) of the array,
            in which newvec is the data vector (with offset 0).
 > oldvec: simple vector, source for filling.
 > olddims: pointer to the dimensions of the array,
            in which oldvec is the data vector (with offset offset).
 > rank: dimension number of newdims = dimension number of olddims.
 > eltype: element type of newvec = element type of oldvec.
 method: pseudo-recursive, with pseudo-stack, that is placed below STACK. */
typedef struct {
  uintL olddim; /* dimension of olddims */
  uintL newdim; /* dimension of newdims */
  uintL mindim; /* minimum dimensions of both */
  uintL subscript; /* subscript, runs from 0 to mindim-1 */
  uintL oldindex; /* row-major-index in oldvec */
  uintL newindex; /* row-major-index in newvec */
  uintL olddelta; /* increment of oldindex for subscript++ */
  uintL newdelta; /* increment of newindex for subscript++ */
} reshape_data_t;
local void reshape (object newvec, object newdims, object oldvec,
                    const uintL* olddims, uintL offset, uintL rank,
                    uintB eltype) {
  /* get space for the pseudo-stack: */
  get_space_on_STACK(rank*sizeof(reshape_data_t));
  /* starting point: */
  var reshape_data_t* reshape_stack = &STACKblock_(reshape_data_t,-1);
  /* fill pseudo-stack: */
  if (rank!=0) {
    var reshape_data_t* ptr;
    var uintC count;
    /* store newdim: */
    ptr = reshape_stack;
    if (consp(newdims)) {
      dotimespC(count,rank, {
        ptr->newdim = posfixnum_to_V(Car(newdims)); newdims = Cdr(newdims);
        ptr = ptr STACKop -1;
      });
    } else {
      ptr->newdim = posfixnum_to_V(newdims);
    }
    /* store olddim and mindim: */
    ptr = reshape_stack;
    dotimespC(count,rank, {
      var uintL olddim;
      var uintL newdim;
      olddim = ptr->olddim = *olddims++;
      newdim = ptr->newdim;
      ptr->mindim = (olddim<newdim ? olddim : newdim);
      ptr = ptr STACKop -1;
    });
    { /* store olddelta and newdelta: */
      var uintL olddelta = 1;
      var uintL newdelta = 1;
      dotimespC(count,rank, {
        ptr = ptr STACKop 1;
        ptr->olddelta = olddelta;
        olddelta = mulu32_unchecked(olddelta,ptr->olddim);
        ptr->newdelta = newdelta;
        newdelta = mulu32_unchecked(newdelta,ptr->newdim);
      });
    }
  }
  /* Start of pseudo-recursion: */
  var reshape_data_t* ptr = reshape_stack;
  var uintL oldindex = offset; /* row-major-index in oldvec */
  var uintL newindex = 0; /* row-major-index in newvec */
  var uintL depth = rank;
 entry: /* entry for recursion */
  if (depth==0) {
    /* copy element:
       (setf (aref newvec newindex) (aref oldvec oldindex))
       copy so that no GC can be triggered: */
    if (eltype == Atype_32Bit) {
      ((uint32*)&TheSbvector(newvec)->data[0])[newindex]
        = ((uint32*)&TheSbvector(oldvec)->data[0])[oldindex];
    } else {
      storagevector_store(newvec,newindex,storagevector_aref(oldvec,oldindex),false);
    }
  } else if (depth==1) {
    /* optimization: copy a complete row of elements
       (notice: ptr->olddelta = ptr->newdelta = 1). */
    if (ptr->mindim > 0)
      elt_copy(oldvec,oldindex,newvec,newindex,ptr->mindim);
  } else {
    /* loop over all shared indices: */
    ptr->oldindex = oldindex; ptr->newindex = newindex;
    if (ptr->mindim > 0) {
      depth--;
      dotimespL(ptr->subscript,ptr->mindim, {
        oldindex = ptr->oldindex; newindex = ptr->newindex;
        ptr = ptr STACKop -1;
        goto entry;
                 reentry:
        ptr = ptr STACKop 1;
        ptr->oldindex += ptr->olddelta;
        ptr->newindex += ptr->newdelta;
      });
      depth++;
    }
  }
  /* exit from recursion: */
  if (depth<rank)
    goto reentry;
}

/* (ADJUST-ARRAY array dimensions :element-type :initial-element
   :initial-contents :fill-pointer :displaced-to :displaced-index-offset),
   CLTL p. 297 */
LISPFUN(adjust_array,seclass_default,2,0,norest,key,6,
        (kw(element_type),kw(initial_element),
         kw(initial_contents),kw(fill_pointer),
         kw(displaced_to),kw(displaced_index_offset)) )
{
  var uintL totalsize, rank;
  { /* check the array : */
    var object array = check_array(STACK_7);
    STACK_7 = STACK_6; STACK_6 = array; /* for test_dims() */
    /* check dimensions and rank and compute total-size: */
    rank = test_dims(&totalsize);
    { /* check rank, must be == (array-rank array): */
      var uintL oldrank = array_simplep(STACK_6) ? 1
        : (uintL)Iarray_rank(STACK_6);
      if (rank != oldrank) {
        pushSTACK(STACK_7); /* dims */
        pushSTACK(STACK_(6+1)); /* array */
        pushSTACK(fixnum(oldrank));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,GETTEXT("~S: rank ~S of array ~S cannot be altered: ~S"));
      }
    }
  }
  /* stack layout:
     dims, array, element-type, initial-element, initial-contents,
     fill-pointer, displaced-to, displaced-index-offset. */
  /* check element-type and convert it into code: */
  var uintB eltype;
  if (boundp(STACK_5)) {
    eltype = eltype_code(STACK_5);
    /* compare with the element-type of the array argument */
    if (eltype != array_atype(STACK_6)) {
      pushSTACK(STACK_6); /* TYPE-ERROR slot DATUM */
      pushSTACK(S(array)); pushSTACK(STACK_(5+2));
      { /* TYPE-ERROR slot EXPECTED-TYPE */
        object exp_type = listof(2); pushSTACK(exp_type); }
      pushSTACK(STACK_(5+2)); /* element-type */
      pushSTACK(STACK_(6+3)); /* array */
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,GETTEXT("~S: array ~S does not have element-type ~S"));
    }
  } else { /* default is the element-type of the array-argument */
    eltype = array_atype(STACK_6);
    STACK_5 = array_element_type(STACK_6);
  }
  if (array_simplep(STACK_6)
      || ((Iarray_flags(STACK_6) & bit(arrayflags_adjustable_bit)) == 0)) {
    /* not an adjustable array ==> new array
       if no :initial-contents and no :displaced-to, copy contents */
    var bool copy_p = !boundp(STACK_3) && missingp(STACK_1);
    var object array = STACK_6;
    pushSTACK(STACK_1); pushSTACK(STACK_1);
    /* :FILL-POINTER NIL means keep it as it was */
    STACK_2 = !missingp(STACK_4) ? (object)STACK_4 :
      array_has_fill_pointer_p(array) ? fixnum(*get_fill_pointer(array)) : NIL;
    STACK_3 = STACK_5; STACK_4 = STACK_6; STACK_5 = STACK_7;
    STACK_6 = NIL; /* :ADJUSTABLE NIL */
    STACK_7 = STACK_9; /* dims */
    STACK_8 = array; /* save array */
    C_make_array(); /* MAKE-ARRAY with all the args but first */
    /* stack layout: dims, array */
    if (copy_p) {
      var uintL offset1 = 0;
      var object dv1 = /* original: may be simple! */
        array_displace_check(STACK_0,array_total_size(STACK_0),&offset1);
      var uintL offset2 = 0;
      var object dv2 = /* new: may be simple! */
        array_displace_check(value1,totalsize,&offset2);
      var uintL* dimptr;
      if (array_simplep(STACK_0)) {
        if (simple_string_p(STACK_0)) {
          sstring_un_realloc(STACK_0);
          offset2 = Sstring_length(STACK_0);
        } else
          offset2 = Sarray_length(STACK_0);
        dimptr = &offset2;
      } else {
        dimptr = &TheIarray(STACK_0)->dims[0];
        if (Iarray_flags(STACK_0) & bit(arrayflags_dispoffset_bit))
          dimptr++;
        /* use DIMENSION, not FILL-POINTER! */
      }
      reshape(dv2,STACK_1,dv1,dimptr,offset1,rank,eltype);
    }
    skipSTACK(2); /* drop array & new dimensions */
    return;
  }
  test_otherkeys(); /* do some other checks */
  var uintB flags = Iarray_flags(STACK_6);
  /* the Flags contain exactly eltype as Atype and
     arrayflags_adjustable_bit and thus also
     arrayflags_dispoffset_bit and maybe also arrayflags_fillp_bit
     (these will not be modified) and maybe also
     arrayflags_displaced_bit (this can be modified). */
  var uintL displaced_index_offset;
  var uintL fillpointer;
  /* if not displaced, create data vector and poss. fill: */
  if (nullp(STACK_1)) { /* displaced-to not supplied? */
    var object datenvektor;
    if (boundp(STACK_3)) { /* and if initial-contents supplied: */
      /* create data vector: */
      datenvektor = make_storagevector(totalsize,eltype);
      /* fill with the initial-contents argument: */
      datenvektor = initial_contents(datenvektor,STACK_7,rank,STACK_3);
    } else { /* create data vector: */
      if (eltype == Atype_Char) {
        check_stringsize(totalsize);
       #ifdef HAVE_SMALL_SSTRING
        var uintL oldoffset = 0;
        var object olddatenvektor = iarray_displace(STACK_6,&oldoffset);
        SstringCase(olddatenvektor,
          { datenvektor = allocate_s8string(totalsize); },
          { datenvektor = allocate_s16string(totalsize); },
          { datenvektor = allocate_s32string(totalsize); },
          { NOTREACHED; });
       #else
        datenvektor = allocate_string(totalsize);
       #endif
        datenvektor = fill_initial_element(totalsize,datenvektor);
      } else
        datenvektor = make_storagevector(totalsize,eltype);
      /* fill with the original content of array: */
      var object oldarray = STACK_6; /* array */
      var uintL oldoffset = 0;
      var object oldvec =
        iarray_displace_check(oldarray,TheIarray(oldarray)->totalsize,
                              &oldoffset);
      /* oldvec is the data vector, with displaced-offset oldoffset. */
      var uintL* olddimptr = &TheIarray(oldarray)->dims[1];
      /* At olddimptr are the old dimensions of array
         (notice: As arrayflags_adjustable_bit is set, also
         arrayflags_dispoffset_bit is set, thus
         TheIarray(array)->data[0] is reserved for the displaced-offset.) */
      reshape(datenvektor,STACK_7,oldvec,olddimptr,oldoffset,rank,eltype);
    }
    STACK_1 = datenvektor; /* store data vector as "displaced-to" */
    displaced_index_offset = 0; /* with displacement 0 */
    flags &= ~bit(arrayflags_displaced_bit); /* and without displacement-bit in the flags */
  } else {
    /* displaced-to supplied. */
    displaced_index_offset = test_displaced(eltype,totalsize);
    { /* test for accruing cycle: */
      var object array = STACK_6; /* array, that has to be displaced */
      var object to_array = STACK_1; /* array, to which displacement takes place */
      /* test, if array occurs in the data vector chain of to_array: */
      loop {
        /* if array = to_array, we have a cycle. */
        if (eq(array,to_array)) {
          pushSTACK(array);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,GETTEXT("~S: cannot displace array ~S to itself"));
        }
        /* if to_array is simple (thus not displaced), */
        /* there is no cycle. */
        if (simplep(to_array))
          break;
        /* follow displaced-chain of to_array: */
        to_array = TheIarray(to_array)->data;
      }
    }
    /* flags contain the displacement-bit: */
    flags |= bit(arrayflags_displaced_bit);
  }
  /* flags are now correct. */
  /* modify the given array. */
  if (!nullp(STACK_2)) { /* fill-pointer supplied? */
    /* array must have fill-pointer: */
    if (!(Iarray_flags(STACK_6) & bit(arrayflags_fillp_bit))) {
      pushSTACK(STACK_6); /* TYPE-ERROR slot DATUM */
      pushSTACK(O(type_vector_with_fill_pointer)); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(STACK_(6+2));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,GETTEXT("~S: array ~S has no fill-pointer"));
    }
    fillpointer = test_fillpointer(totalsize); /* fill-pointer-value */
  } else {
    /* If array has a fill-pointer, it must be <= the new total-size: */
    var object array = STACK_6;
    if (Iarray_flags(array) & bit(arrayflags_fillp_bit))
      if (!(TheIarray(array)->dims[2] <= totalsize)) {
        /* dims[0] = displaced-offset, dims[1] = length, dims[2] = fill-pointer */
        pushSTACK(fixnum(totalsize));
        pushSTACK(fixnum(TheIarray(array)->dims[2]));
        pushSTACK(array);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~S: the fill-pointer of array ~S is ~S, greater than ~S"));
      }
  }
  { /* modify array: */
    var object array = STACK_6;
    set_break_sem_1(); /* forbid interrupts */
    iarray_flags_replace(TheIarray(array),flags); /* store new flags */
    TheIarray(array)->totalsize = totalsize; /* store new total-size */
    {
      var uintL* dimptr = &TheIarray(array)->dims[0];
      *dimptr++ = displaced_index_offset; /* store displaced-index-offset */
      { /* store new dimensions: */
        var object dims = STACK_7;
        if (listp(dims)) {
          while (consp(dims)) {
            *dimptr++ = posfixnum_to_V(Car(dims)); dims = Cdr(dims);
          }
        } else {
          *dimptr++ = posfixnum_to_V(dims);
        }
      }
      /* poss. store fill-pointer resp. correct it: */
      if (flags & bit(arrayflags_fillp_bit)) /* array with fill-pointer? */
        if (!nullp(STACK_2)) /* is fill-pointer supplied? */
          /* fill-pointer was supplied and /=NIL */
          *dimptr = fillpointer;
    }
    /* store data vector: */
    TheIarray(array)->data = STACK_1; /* displaced-to-argument or new data vector */
    clr_break_sem_1(); /* permit interrupts again */
    /* array as value: */
    VALUES1(array); skipSTACK(8);
  }
}

/* ======================================================================== */
/* Arrays as sequences */

/* functions, that turn vectors into sequences: */

LISPFUNN(vector_init,1) /* #'(lambda (seq) 0) */
{
  skipSTACK(1);
  VALUES1(Fixnum_0);
}

LISPFUNN(vector_upd,2)
{ /* #'(lambda (seq pointer) (1+ pointer)) */
  if (posfixnump(STACK_0)) {
    var object newpointer = fixnum_inc(STACK_0,1); /* increase Fixnum >=0 by 1 */
    if (posfixnump(newpointer)) {
      /* remained a Fixnum >=0 */
      skipSTACK(2);
      VALUES1(newpointer);
      return;
    }
  }
  /* pointer is before or after the increment not a Fixnum >=0 */
  funcall(L(einsplus),1); /* (1+ pointer) as value */
  skipSTACK(1);
}

LISPFUNN(vector_endtest,2)
{ /* #'(lambda (seq pointer) (= pointer (vector-length seq))) */
  var object seq = check_vector(STACK_1);
  VALUES_IF(eq(fixnum(vector_length(seq)),STACK_0));
  skipSTACK(2);
}

LISPFUNN(vector_fe_init,1)
{ /* #'(lambda (seq) (1- (vector-length seq))) */
  var object seq = check_vector(popSTACK());
  var uintL len = vector_length(seq);
  /* len = (vector-length seq) as Fixnum, and decrease by 1: */
  VALUES1(len==0 ? Fixnum_minus1 : fixnum(len-1));
}

LISPFUNN(vector_fe_upd,2)
{ /* #'(lambda (seq pointer) (1- pointer)) */
  if (posfixnump(STACK_0)) {
    var object pointer = popSTACK();
    VALUES1(eq(pointer,Fixnum_0)
            ? Fixnum_minus1
            : fixnum_inc(pointer,-1)); /* Fixnum >0 decrement by 1 */
  } else {
    /* pointer is before or after the decrement not a Fixnum >=0 */
    funcall(L(einsminus),1); /* (1- pointer) as value */
  }
  skipSTACK(1);
}

LISPFUNN(vector_fe_endtest,2)
{ /* #'(lambda (seq pointer) (minusp pointer)) */
  VALUES_IF(! positivep(STACK_0)); /* return the sign of pointer */
  skipSTACK(2);
}

LISPFUNN(vector_length,1)
{
  VALUES1(fixnum(vector_length(check_vector(popSTACK()))));
}

LISPFUNN(vector_init_start,2)
{ /* #'(lambda (seq index)
     (if (<= 0 index (vector-length seq))
       index
       (error "Illegal :START - Index : ~S" index))) */
  var object seq = check_vector(STACK_1);
  var uintL len = vector_length(seq);
  /* index should be a Fixnum between 0 and len (inclusive) : */
  if (posfixnump(STACK_0) && (posfixnum_to_V(STACK_0)<=len)) {
    VALUES1(STACK_0); skipSTACK(2); /* return index */
  } else {
    /* stack layout: seq, index. */
    pushSTACK(STACK_0); /* TYPE-ERROR slot DATUM */
    {
      var object tmp;
      pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(len));
      tmp = listof(3); pushSTACK(tmp); /* TYPE-ERROR slot EXPECTED-TYPE */
    }
    pushSTACK(STACK_3); /* seq */
    pushSTACK(STACK_3); /* index */
    fehler(type_error,GETTEXT("Illegal START index ~S for ~S"));
  }
}

LISPFUNN(vector_fe_init_end,2)
{ /* #'(lambda (seq index)
     (if (<= 0 index (vector-length seq))
       (1- index)
       (error "Illegal :END - Index : ~S" index))) */
  var object seq = check_vector(STACK_1);
  var uintL len = vector_length(seq);
  /* index should be a Fixnum between 0 and len (inclusive) : */
  if (posfixnump(STACK_0) && (posfixnum_to_V(STACK_0)<=len)) {
    var object index = STACK_0;
    skipSTACK(2);
    VALUES1(eq(index,Fixnum_0)
            ? Fixnum_minus1
            : fixnum_inc(index,-1)); /* Fixnum >0 decrement by 1 */
  } else {
    /* stack layout: seq, index. */
    pushSTACK(STACK_0); /* TYPE-ERROR slot DATUM */
    {
      var object tmp;
      pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(len));
      tmp = listof(3); pushSTACK(tmp); /* TYPE-ERROR slot EXPECTED-TYPE */
    }
    pushSTACK(STACK_3); /* seq */
    pushSTACK(STACK_3); /* index */
    fehler(type_error,GETTEXT("Illegal END index ~S for ~S"));
  }
}

LISPFUNN(make_bit_vector,1)
{ /* (SYS::MAKE-BIT-VECTOR size) returns a Bit-Vector with size bits. */
  var uintL size;
  if (!posfixnump(STACK_0)) {
   bad_size:
    /* STACK_0 = size, TYPE-ERROR slot DATUM */
    pushSTACK(O(type_array_length)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(STACK_1); /* size */
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: invalid bit-vector length ~S"));
  }
  var uintV size = posfixnum_to_V(STACK_0); /* length */
 #if (intVsize>intLsize)
  if (size >= vbit(intLsize)) goto bad_size;
 #endif
  VALUES1(allocate_bit_vector(Atype_Bit,size)); /* return a bit-vector */
  skipSTACK(1);
}
