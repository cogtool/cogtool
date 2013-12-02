/* Basic functions for working with integers */

/* conversion routines digit-sequence-part <--> longword:
 get_32_Dptr(ptr)
   fetches the next 32 bits from the 32/intDsize digits starting at ptr.
 set_32_Dptr(ptr,wert);
   stores the value wert (32 bits) in the 32/intDsize digits starting at ptr.
 get_max32_Dptr(count,ptr)
   fetches the next count bits from the ceiling(count/intDsize) digits at ptr.
 set_max32_Dptr(count,ptr,wert)
   stores wert (count bits) in the ceiling(count/intDsize) digits at ptr.
 each time: ptr a variable of type uintD*,
            wert a variable of type uint32,
            count a variable or constant-expression with value >=0, <=32. */
#if (intDsize==32)
  #define get_32_Dptr(ptr)  ((uint32)((ptr)[0]))
  #define set_32_Dptr(ptr,wert)  ((ptr)[0] = (uintD)(wert))
  #define get_max32_Dptr(count,ptr)  \
    ((count)==0 ? 0 : (uint32)((ptr)[0]))
  #define set_max32_Dptr(count,ptr,wert)  \
    ((count)==0 ? 0 : ((ptr)[0] = (uintD)(wert)))
#endif
#if (intDsize==16)
  # define get_32_Dptr(ptr)  (((uint32)((ptr)[0])<<16) | ((uint32)((ptr)[1])))
  #define get_32_Dptr(ptr)  highlow32_at(ptr)
  # define set_32_Dptr(ptr,wert)  ((ptr)[0] = (uintD)((wert)>>16), (ptr)[1] = (uintD)(wert))
  #define set_32_Dptr(ptr,wert)  set_highlow32_at(ptr,wert)
  #define get_max32_Dptr(count,ptr)  \
    ((count)==0 ? 0 :                   \
     (count)<=16 ? (uint32)((ptr)[0]) : highlow32_at(ptr))
  #define set_max32_Dptr(count,ptr,wert)  \
    ((count)==0 ? 0 :                           \
     (count)<=16 ? ((ptr)[0] = (uintD)(wert)) : \
                   set_highlow32_at(ptr,wert))
#endif
#if (intDsize==8)
  #define get_32_Dptr(ptr)  (((((( (uint32)((ptr)[0]) <<8) | (uint32)((ptr)[1])) <<8) | (uint32)((ptr)[2])) <<8) | (uint32)((ptr)[3]))
  #define set_32_Dptr(ptr,wert)  ((ptr)[0] = (uintD)((wert)>>24), (ptr)[1] = (uintD)((wert)>>16), (ptr)[2] = (uintD)((wert)>>8), (ptr)[3] = (uintD)(wert))
  #define get_max32_Dptr(count,ptr)  \
    ((count)==0 ? 0 : \
     (count)<=8 ? (uint32)((ptr)[0]) : \
     (count)<=16 ? (( (uint32)((ptr)[0]) <<8) | (uint32)((ptr)[1])) : \
     (count)<=24 ? (((( (uint32)((ptr)[0]) <<8) | (uint32)((ptr)[1])) <<8) | (uint32)((ptr)[2])) : \
                   (((((( (uint32)((ptr)[0]) <<8) | (uint32)((ptr)[1])) <<8) | (uint32)((ptr)[2])) <<8) | (uint32)((ptr)[3])))
  #define set_max32_Dptr(count,ptr,wert)  \
    ((count)==0 ? 0 : \
     (count)<=8 ? ((ptr)[0] = (uintD)(wert)) : \
     (count)<=16 ? ((ptr)[0] = (uintD)((wert)>>8), (ptr)[1] = (uintD)(wert)) : \
     (count)<=24 ? ((ptr)[0] = (uintD)((wert)>>16), (ptr)[1] = (uintD)((wert)>>8), (ptr)[2] = (uintD)(wert)) : \
                   ((ptr)[0] = (uintD)((wert)>>24), (ptr)[1] = (uintD)((wert)>>16), (ptr)[2] = (uintD)((wert)>>8), (ptr)[3] = (uintD)(wert)))
#endif

/* conversion routines digit-sequence-part <--> longword:
 get_maxV_Dptr(count,ptr)
   fetches the next count bits from the ceiling(count/intDsize) digits at ptr.
 ptr a variable of type uintD*,
 wert a variable of type uintV,
 count a variable or constant-expression with value >=0, <=intVsize. */
#if (intVsize==32)
  #define get_maxV_Dptr  get_max32_Dptr
#else
  #if (intDsize==32)
    #define get_maxV_Dptr(count,ptr)  \
      ((count)==0 ? 0 : \
       (count)<=32 ? (uint64)((ptr)[0]) : \
                     (( (uint64)((ptr)[0]) <<32) | (uint64)((ptr)[1])))
  #endif
  #if (intDsize==16)
    #define get_maxV_Dptr(count,ptr)  \
      ((count)==0 ? 0 : \
       (count)<=16 ? (uint64)((ptr)[0]) : \
       (count)<=32 ? (( (uint64)((ptr)[0]) <<16) | (uint64)((ptr)[1])) : \
       (count)<=48 ? (((( (uint64)((ptr)[0]) <<16) | (uint64)((ptr)[1])) <<16) | (uint64)((ptr)[2])) : \
                     (((((( (uint64)((ptr)[0]) <<16) | (uint64)((ptr)[1])) <<16) | (uint64)((ptr)[2])) <<16) | (uint64)((ptr)[3])))
  #endif
#endif

/* get_uint1D_Dptr(ptr)  fetches 1 digit (unsigned) at ptr
 get_uint2D_Dptr(ptr)  fetches 2 digits (unsigned) at ptr
 get_uint3D_Dptr(ptr)  fetches 3 digits (unsigned) at ptr
 get_uint4D_Dptr(ptr)  fetches 4 digits (unsigned) at ptr
 get_sint1D_Dptr(ptr)  fetches 1 digit (signed) at ptr
 get_sint2D_Dptr(ptr)  fetches 2 digits (signed) at ptr
 get_sint3D_Dptr(ptr)  fetches 3 digits (signed) at ptr
 get_sint4D_Dptr(ptr)  fetches 4 digits (signed) at ptr
 Each time: ptr a variable of type uintD*. */
#define get_uint1D_Dptr(ptr)  ((uint32)((ptr)[0]))
#define get_uint2D_Dptr(ptr)  (((uint32)((ptr)[0]) << intDsize) | (uint32)((ptr)[1]))
#define get_uint3D_Dptr(ptr)  (((((uint32)((ptr)[0]) << intDsize) | (uint32)((ptr)[1])) << intDsize) | (uint32)((ptr)[2]))
#define get_uint4D_Dptr(ptr)  (((((((uint32)((ptr)[0]) << intDsize) | (uint32)((ptr)[1])) << intDsize) | (uint32)((ptr)[2])) << intDsize) | (uint32)((ptr)[3]))
#define get_sint1D_Dptr(ptr)  ((sint32)(sintD)((ptr)[0]))
#define get_sint2D_Dptr(ptr)  (((sint32)(sintD)((ptr)[0]) << intDsize) | (uint32)((ptr)[1]))
#define get_sint3D_Dptr(ptr)  (((((sint32)(sintD)((ptr)[0]) << intDsize) | (uint32)((ptr)[1])) << intDsize) | (uint32)((ptr)[2]))
#define get_sint4D_Dptr(ptr)  (((((((sint32)(sintD)((ptr)[0]) << intDsize) | (uint32)((ptr)[1])) << intDsize) | (uint32)((ptr)[2])) << intDsize) | (uint32)((ptr)[3]))
#if (intDsize==16) && (defined(MC680X0) && !defined(MC680Y0)) /* improvement: */
  #undef get_uint2D_Dptr
  #undef get_sint2D_Dptr
  #define get_uint2D_Dptr(ptr)  highlow32_at(ptr)
  #define get_sint2D_Dptr(ptr)  (sint32)highlow32_at(ptr)
#endif
#if (intDsize==16)
  #undef get_uint3D_Dptr
  #undef get_uint4D_Dptr
  #undef get_sint3D_Dptr
  #undef get_sint4D_Dptr
  #define get_uint3D_Dptr(ptr)  get_uint2D_Dptr(&(ptr)[1])
  #define get_uint4D_Dptr(ptr)  get_uint2D_Dptr(&(ptr)[2])
  #define get_sint3D_Dptr  get_uint3D_Dptr
  #define get_sint4D_Dptr  get_uint4D_Dptr
#endif
#if (intDsize==32)
  #undef get_uint2D_Dptr
  #undef get_uint3D_Dptr
  #undef get_uint4D_Dptr
  #undef get_sint2D_Dptr
  #undef get_sint3D_Dptr
  #undef get_sint4D_Dptr
  #define get_uint2D_Dptr(ptr)  get_uint1D_Dptr(&(ptr)[1])
  #define get_uint3D_Dptr(ptr)  get_uint1D_Dptr(&(ptr)[2])
  #define get_uint4D_Dptr(ptr)  get_uint1D_Dptr(&(ptr)[3])
  #define get_sint2D_Dptr  get_uint2D_Dptr
  #define get_sint3D_Dptr  get_uint3D_Dptr
  #define get_sint4D_Dptr  get_uint4D_Dptr
#endif

/* conversion routines integer <--> longword: */

/* converts fixnum into longword.
 FN_to_V(obj)
 > obj: a fixnum
 < result: the value of the fixnum as intVsize-bit-number. */
local sintV FN_to_V (object obj);
#if 1
  #define FN_to_V(obj)  fixnum_to_V(obj)
#else
local sintV FN_to_V (object obj)
{
  if (R_minusp(obj)) /* negative: fill with 1-bits */
    return (as_oint(obj) >> oint_data_shift) | ~ (FN_value_mask >> oint_data_shift);
  else /* >=0: fill with 0-bits */
    return (as_oint(obj) >> oint_data_shift) & (FN_value_mask >> oint_data_shift);
}
#endif

/* FN_V_zerop(x,x_) determines, if x = 0 .
 x is a fixnum and x_ = FN_to_V(x). */
#if (oint_data_len<intVsize)
  #define FN_V_zerop(x,x_)  (x_==0)
#else
  #define FN_V_zerop(x,x_)  (eq(x,Fixnum_0))
#endif

/* FN_V_minusp(x,x_) determines, if x < 0 .
 x is a fixnum and x_ = FN_to_V(x). */
#if (oint_data_len<intVsize)
  #define FN_V_minusp(x,x_)  (x_<0)
#else
  #define FN_V_minusp(x,x_)  (R_minusp(x))
#endif

#ifdef intQsize
/* converts fixnum into quadword.
 FN_to_Q(obj)
 > obj: a fixnum
 < result: the value of the fixnum as 64-bit-number. */
local sint64 FN_to_Q (object obj);
#define FN_to_Q(obj)  fixnum_to_Q(obj)
#endif

/* converts integer >=0 into unsigned longword.
 I_to_UL(obj)
 > obj: an object, should be an integer >=0, <2^32
 < result: the value of the integer as 32-bit-number. */
global uint32 I_to_UL (object obj)
{
 #ifdef TYPECODES
  switch (typecode(obj))
 #else
  if (posfixnump(obj))
    goto case_posfixnum;
  else if (posbignump(obj))
    goto case_posbignum;
  else
    switch (0)
 #endif
  {
   case_posfixnum: /* fixnum >=0 */
   #if (intVsize>intLsize)
    if (posfixnum_to_V(obj) >= vbitm(intLsize)) goto bad;
   #endif
    return posfixnum_to_V(obj);
   case_posbignum: { /* bignum >0 */
    var Bignum bn = TheBignum(obj);
    var uintC len = bignum_length(bn);
   #define IF_LENGTH(i)  \
    if (bn_minlength <= i) /* exactly i digits possible at all? */      \
      if (len == i) /* exactly i digits? */                             \
        /* 2^((i-1)*intDsize-1) <= obj < 2^(i*intDsize-1) */            \
        if ( (i*intDsize-1 > 32)                                        \
             && ( ((i-1)*intDsize-1 >= 32)                              \
                  || (bn->data[0] >= (uintD)bitc(32-(i-1)*intDsize))))  \
          goto bad;                                                     \
        else
    IF_LENGTH(1)
      return get_uint1D_Dptr(bn->data);
    IF_LENGTH(2)
      return get_uint2D_Dptr(bn->data);
    IF_LENGTH(3)
      return get_uint3D_Dptr(bn->data);
    IF_LENGTH(4)
      return get_uint4D_Dptr(bn->data);
    IF_LENGTH(5)
      return get_uint4D_Dptr(&bn->data[1]);
   #undef IF_LENGTH
    }
   default:
   bad: /* unsuitable object */
     pushSTACK(obj); /* TYPE-ERROR slot DATUM */
     pushSTACK(O(type_uint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
     pushSTACK(obj);
     fehler(type_error,GETTEXT("not a 32-bit integer: ~S"));
  }
}

/* converts integer into signed longword.
 I_to_L(obj)
 > obj: an object, should be an integer >=-2^31, <2^31
 < result: the value of the integer as 32-bit-number. */
global sint32 I_to_L (object obj)
{
 #ifdef TYPECODES
  switch (typecode(obj))
 #else
  if (fixnump(obj)) {
    if (FN_positivep(obj))
      goto case_posfixnum;
    else
      goto case_negfixnum;
  } else if (bignump(obj)) {
    if (BN_positivep(obj))
      goto case_posbignum;
    else
      goto case_negbignum;
  } else
    switch (0)
 #endif
  {
   case_posfixnum: { /* fixnum >=0 */
    var sintV wert = posfixnum_to_V(obj);
   #if (intVsize>intLsize)
    if ((uintV)wert >= vbit(intLsize-1)) goto bad;
   #else
    if ((oint_data_len+1 > intLsize) && (wert < 0)) goto bad;
   #endif
    return wert;
   }
   case_posbignum: { /* bignum >0 */
    var Bignum bn = TheBignum(obj);
    var uintC len = bignum_length(bn);
    #define IF_LENGTH(i)                                                 \
      if (bn_minlength <= i) /* exactly i digits possible at all? */      \
        if (len == i) /* exactly i digits? */                             \
          /* 2^((i-1)*intDsize-1) <= obj < 2^(i*intDsize-1) */            \
          if ( (i*intDsize > 32)                                          \
               && ( ((i-1)*intDsize >= 32)                                \
                    || (bn->data[0] >= (uintD)bitc(31-(i-1)*intDsize))))  \
            goto bad;                                                     \
          else
    IF_LENGTH(1)
      return get_uint1D_Dptr(bn->data);
    IF_LENGTH(2)
      return get_uint2D_Dptr(bn->data);
    IF_LENGTH(3)
      return get_uint3D_Dptr(bn->data);
    IF_LENGTH(4)
      return get_uint4D_Dptr(bn->data);
    #undef IF_LENGTH
    goto bad;
   }
   case_negfixnum: { /* fixnum <0 */
    var sintV wert = negfixnum_to_V(obj);
   #if (intVsize>intLsize)
    if ((uintV)wert < (uintV)minus_vbit(intLsize-1)) goto bad;
   #else
    if ((oint_data_len+1 > intLsize) && (wert >= 0)) goto bad;
   #endif
    return wert;
   }
   case_negbignum: { /* bignum <0 */
    var Bignum bn = TheBignum(obj);
    var uintC len = bignum_length(bn);
    #define IF_LENGTH(i)                                                    \
      if (bn_minlength <= i) /* exactly i digits possible at all? */      \
        if (len == i) /* exactly i digits? */                             \
          /* - 2^(i*intDsize-1) <= obj < - 2^((i-1)*intDsize-1) */        \
          if ( (i*intDsize > 32)                                          \
               && ( ((i-1)*intDsize >= 32)                                \
                    || (bn->data[0] < (uintD)(-bitc(31-(i-1)*intDsize))))) \
            goto bad;                                                     \
          else
    IF_LENGTH(1)
      return get_sint1D_Dptr(bn->data);
    IF_LENGTH(2)
      return get_sint2D_Dptr(bn->data);
    IF_LENGTH(3)
      return get_sint3D_Dptr(bn->data);
    IF_LENGTH(4)
      return get_sint4D_Dptr(bn->data);
    #undef IF_LENGTH
    goto bad;
   }
   default:
   bad: /* unsuitable object */
     pushSTACK(obj); /* TYPE-ERROR slot DATUM */
     pushSTACK(O(type_sint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
     pushSTACK(obj);
     fehler(type_error,GETTEXT("not a 32-bit integer: ~S"));
  }
}

#if defined(HAVE_LONGLONG)

/* converts integer >=0 into unsigned quadword.
 I_to_UQ(obj)
 > obj: an object, should be an integer >=0, <2^64
 < result: the value of the integer as 64-bit-number. */
global uint64 I_to_UQ (object obj)
{
 #ifdef TYPECODES
  switch (typecode(obj))
 #else
  if (posfixnump(obj))
    goto case_posfixnum;
  else if (posbignump(obj))
    goto case_posbignum;
  else
    switch (0)
 #endif
  {
   case_posfixnum: /* fixnum >=0 */
    return (uint64)posfixnum_to_V(obj);
   case_posbignum: { /* bignum >0 */
      var Bignum bn = TheBignum(obj);
      var uintC len = bignum_length(bn);
      #define IF_LENGTH(i)                                               \
        if (bn_minlength <= i) /* exactly i digits possible at all? */    \
          if (len == i) /* exactly i digits? */                           \
            /* 2^((i-1)*intDsize-1) <= obj < 2^(i*intDsize-1) */          \
            if ( (i*intDsize-1 > 64)                                      \
                 && ( ((i-1)*intDsize-1 >= 64)                            \
                      || (bn->data[0] >= (uintD)bitc(64-(i-1)*intDsize)))) \
              goto bad;                                                   \
            else
     #if (intDsize==32)
      IF_LENGTH(1)
        return (uint64)get_uint1D_Dptr(bn->data);
      IF_LENGTH(2)
        return ((uint64)get_uint1D_Dptr(bn->data) << 32) | (uint64)get_uint1D_Dptr(&bn->data[1]);
      IF_LENGTH(3)
        return ((uint64)get_uint1D_Dptr(&bn->data[1]) << 32) | (uint64)get_uint1D_Dptr(&bn->data[2]);
     #endif
     #if (intDsize==16)
      IF_LENGTH(1)
        return (uint64)get_uint1D_Dptr(bn->data);
      IF_LENGTH(2)
        return (uint64)get_uint2D_Dptr(bn->data);
      IF_LENGTH(3)
        return ((uint64)get_uint1D_Dptr(bn->data) << 32) | (uint64)get_uint2D_Dptr(&bn->data[1]);
      IF_LENGTH(4)
        return ((uint64)get_uint2D_Dptr(bn->data) << 32) | (uint64)get_uint2D_Dptr(&bn->data[2]);
      IF_LENGTH(5)
        return ((uint64)get_uint2D_Dptr(&bn->data[1]) << 32) | (uint64)get_uint2D_Dptr(&bn->data[3]);
     #endif
     #if (intDsize==8)
      IF_LENGTH(1)
        return (uint64)get_uint1D_Dptr(bn->data);
      IF_LENGTH(2)
        return (uint64)get_uint2D_Dptr(bn->data);
      IF_LENGTH(3)
        return (uint64)get_uint3D_Dptr(bn->data);
      IF_LENGTH(4)
        return (uint64)get_uint4D_Dptr(bn->data);
      IF_LENGTH(5)
        return ((uint64)get_uint1D_Dptr(bn->data) << 32) | (uint64)get_uint4D_Dptr(&bn->data[1]);
      IF_LENGTH(6)
        return ((uint64)get_uint2D_Dptr(bn->data) << 32) | (uint64)get_uint4D_Dptr(&bn->data[2]);
      IF_LENGTH(7)
        return ((uint64)get_uint3D_Dptr(bn->data) << 32) | (uint64)get_uint4D_Dptr(&bn->data[3]);
      IF_LENGTH(8)
        return ((uint64)get_uint4D_Dptr(bn->data) << 32) | (uint64)get_uint4D_Dptr(&bn->data[4]);
      IF_LENGTH(9)
        return ((uint64)get_uint4D_Dptr(&bn->data[1]) << 32) | (uint64)get_uint4D_Dptr(&bn->data[5]);
     #endif
      #undef IF_LENGTH
    }
    default:
    bad: /* unsuitable object */
      pushSTACK(obj); /* TYPE-ERROR slot DATUM */
      pushSTACK(O(type_uint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(obj);
      fehler(type_error,GETTEXT("not a 64-bit integer: ~S"));
  }
}

#endif

#if defined(HAVE_LONGLONG)

/* converts integer into signed quadword.
 I_to_Q(obj)
 > obj: an object, should be an integer >=-2^63, <2^63
 < result: the value of the integer as 64-bit-number. */
global sint64 I_to_Q (object obj)
{
 #ifdef TYPECODES
  switch (typecode(obj))
 #else
  if (fixnump(obj)) {
    if (FN_positivep(obj))
      goto case_posfixnum;
    else
      goto case_negfixnum;
  } else if (bignump(obj)) {
    if (BN_positivep(obj))
      goto case_posbignum;
    else
      goto case_negbignum;
  } else
    switch (0)
 #endif
  {
   case_posfixnum: /* Fixnum >=0 */
    return (uint64)posfixnum_to_V(obj);
   case_posbignum: { /* Bignum >0 */
      var Bignum bn = TheBignum(obj);
      var uintC len = bignum_length(bn);
      #define IF_LENGTH(i)  \
        if (bn_minlength <= i) /* exactly i digits possible at all? */    \
          if (len == i) /* exaclty i digits? */                           \
            /* 2^((i-1)*intDsize-1) <= obj < 2^(i*intDsize-1) */          \
            if ( (i*intDsize > 64)                                        \
                 && ( ((i-1)*intDsize >= 64)                              \
                      || (bn->data[0] >= (uintD)bitc(63-(i-1)*intDsize)))) \
              goto bad;                                                   \
            else
     #if (intDsize==32)
      IF_LENGTH(1)
        return (uint64)get_uint1D_Dptr(bn->data);
      IF_LENGTH(2)
        return ((uint64)get_uint1D_Dptr(bn->data) << 32) | (uint64)get_uint1D_Dptr(&bn->data[1]);
     #endif
     #if (intDsize==16)
      IF_LENGTH(1)
        return (uint64)get_uint1D_Dptr(bn->data);
      IF_LENGTH(2)
        return (uint64)get_uint2D_Dptr(bn->data);
      IF_LENGTH(3)
        return ((uint64)get_uint1D_Dptr(bn->data) << 32) | (uint64)get_uint2D_Dptr(&bn->data[1]);
      IF_LENGTH(4)
        return ((uint64)get_uint2D_Dptr(bn->data) << 32) | (uint64)get_uint2D_Dptr(&bn->data[2]);
     #endif
     #if (intDsize==8)
      IF_LENGTH(1)
        return (uint64)get_uint1D_Dptr(bn->data);
      IF_LENGTH(2)
        return (uint64)get_uint2D_Dptr(bn->data);
      IF_LENGTH(3)
        return (uint64)get_uint3D_Dptr(bn->data);
      IF_LENGTH(4)
        return (uint64)get_uint4D_Dptr(bn->data);
      IF_LENGTH(5)
        return ((uint64)get_uint1D_Dptr(bn->data) << 32) | (uint64)get_uint4D_Dptr(&bn->data[1]);
      IF_LENGTH(6)
        return ((uint64)get_uint2D_Dptr(bn->data) << 32) | (uint64)get_uint4D_Dptr(&bn->data[2]);
      IF_LENGTH(7)
        return ((uint64)get_uint3D_Dptr(bn->data) << 32) | (uint64)get_uint4D_Dptr(&bn->data[3]);
      IF_LENGTH(8)
        return ((uint64)get_uint4D_Dptr(bn->data) << 32) | (uint64)get_uint4D_Dptr(&bn->data[4]);
     #endif
      #undef IF_LENGTH
      goto bad;
    }
   case_negfixnum: /* Fixnum <0 */
    return (uint64)negfixnum_to_V(obj) | (-wbitm(intVsize));
   case_negbignum: { /* Bignum <0 */
      var Bignum bn = TheBignum(obj);
      var uintC len = bignum_length(bn);
      #define IF_LENGTH(i)  \
        if (bn_minlength <= i) /* exactly i digits possible at all? */    \
          if (len == i) /* exactly i digits? */                           \
            /* - 2^(i*intDsize-1) <= obj < - 2^((i-1)*intDsize-1) */      \
            if ( (i*intDsize > 64)                                        \
                 && ( ((i-1)*intDsize >= 64)                              \
                      || (bn->data[0] < (uintD)(-bitc(63-(i-1)*intDsize))))) \
              goto bad;                                                   \
            else
     #if (intDsize==32)
      IF_LENGTH(1)
        return (sint64)get_sint1D_Dptr(bn->data);
      IF_LENGTH(2)
        return ((sint64)get_sint1D_Dptr(bn->data) << 32) | (uint64)get_uint1D_Dptr(&bn->data[1]);
     #endif
     #if (intDsize==16)
      IF_LENGTH(1)
        return (sint64)get_sint1D_Dptr(bn->data);
      IF_LENGTH(2)
        return (sint64)get_sint2D_Dptr(bn->data);
      IF_LENGTH(3)
        return ((sint64)get_sint1D_Dptr(bn->data) << 32) | (uint64)get_uint2D_Dptr(&bn->data[1]);
      IF_LENGTH(4)
        return ((sint64)get_sint2D_Dptr(bn->data) << 32) | (uint64)get_uint2D_Dptr(&bn->data[2]);
     #endif
     #if (intDsize==8)
      IF_LENGTH(1)
        return (sint64)get_sint1D_Dptr(bn->data);
      IF_LENGTH(2)
        return (sint64)get_sint2D_Dptr(bn->data);
      IF_LENGTH(3)
        return (sint64)get_sint3D_Dptr(bn->data);
      IF_LENGTH(4)
        return (sint64)get_sint4D_Dptr(bn->data);
      IF_LENGTH(5)
        return ((sint64)get_sint1D_Dptr(bn->data) << 32) | (uint64)get_uint4D_Dptr(&bn->data[1]);
      IF_LENGTH(6)
        return ((sint64)get_sint2D_Dptr(bn->data) << 32) | (uint64)get_uint4D_Dptr(&bn->data[2]);
      IF_LENGTH(7)
        return ((sint64)get_sint3D_Dptr(bn->data) << 32) | (uint64)get_uint4D_Dptr(&bn->data[3]);
      IF_LENGTH(8)
        return ((sint64)get_sint4D_Dptr(bn->data) << 32) | (uint64)get_uint4D_Dptr(&bn->data[4]);
     #endif
      #undef IF_LENGTH
      goto bad;
    }
    default:
    bad: /* unsuitable object */
      pushSTACK(obj); /* TYPE-ERROR slot DATUM */
      pushSTACK(O(type_sint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(obj);
      fehler(type_error,GETTEXT("not a 64-bit integer: ~S"));
  }
}

#endif

/* converts longword into fixnum.
 L_to_FN(wert)
 > wert: value of the fixnum, a signed 32-bit-integer
         >= -2^oint_data_len, < 2^oint_data_len
 < result: fixnum with this value.
 wert should be a variable. */
#if (oint_data_shift <= sign_bit_o)
  #define L_to_FN(wert)                                                   \
    as_object((( (soint)(sint32)(wert)                                      \
                 & (FN_value_vz_mask>>oint_data_shift) /* mask the unneeded */ \
                 ) << oint_data_shift)                                  \
              | ((oint)fixnum_type<<oint_type_shift)) /* store typeinfo instead */
#else /* (oint_data_shift > sign_bit_o) */
  #define L_to_FN(wert)                                                   \
    as_object((( (soint)(sint32)(wert) << oint_data_shift )                 \
               & FN_value_mask /* mask the unneeded */)                 \
              | ((soint)(sint32)sign_of_sint32((sint32)(wert)) & bit(sign_bit_o)) \
              | ((oint)fixnum_type<<oint_type_shift)) /* store typeinfo instead */
#endif

/* converts longword into integer.
 L_to_I(wert)
 > wert: value of the integer, a signed 32-bit-integer.
 < result: integer with this value.
 can trigger GC */
global maygc object L_to_I (sint32 wert);
#if (oint_data_len+1 >= intLsize)
global maygc object L_to_I (sint32 wert)
{ return L_to_FN(wert); }
#define L_to_I(wert)  L_to_FN(wert)
#else
global maygc object L_to_I (sint32 wert)
{
  {
    var uint32 test = wert & (uint32)(~(FN_value_mask >> oint_data_shift));
    /* test contains the bits, that do not fit into the Fixnum-value. */
    if (test == (uint32)0) /* all =0 ? */
      return as_object(((oint)fixnum_type<<oint_type_shift) | ((oint)wert<<oint_data_shift));
    if (test == (uint32)(~(FN_value_mask >> oint_data_shift))) /* alle =1 ? */
      return as_object(((((oint)fixnum_vz_type<<oint_type_shift)+FN_value_mask) & ((oint)wert<<oint_data_shift))
                       |(((oint)fixnum_vz_type<<oint_type_shift) & (wbit(oint_data_shift)-1)));
  }
  /* create bignum:
     (its length  bn_minlength <= n <= ceiling(32/intDsize)  ) */
  if (bn_minlength == ceiling(32,intDsize)) {
   #if (intDsize==8)
    if (wert >= 0) goto pos4; else goto neg4; /* bignum with 32/intDsize = 4 digits */
   #endif
   #if (intDsize==16)
    if (wert >= 0) goto pos2; else goto neg2; /* bignum with 32/intDsize = 2 digits */
   #endif
   #if (intDsize==32)
    if (wert >= 0) goto pos1; else goto neg1; /* bignum with 32/intDsize = 1 digit */
   #endif
  } else {
    #define FILL_1_DIGIT(from)  \
      *ptr-- = (uintD)from;
    #define FILL_2_DIGITS(from)  \
      *ptr-- = (uintD)from; from = from >> intDsize; \
      *ptr-- = (uintD)from;
    #define FILL_3_DIGITS(from)  \
      *ptr-- = (uintD)from; from = from >> intDsize; \
      *ptr-- = (uintD)from; from = from >> intDsize; \
      *ptr-- = (uintD)from;
    #define FILL_4_DIGITS(from)  \
      *ptr-- = (uintD)from; from = from >> intDsize; \
      *ptr-- = (uintD)from; from = from >> intDsize; \
      *ptr-- = (uintD)from; from = from >> intDsize; \
      *ptr-- = (uintD)from;
    #define FILL_1  FILL_1_DIGIT(wert);
    #define FILL_2  FILL_2_DIGITS(wert);
    #define FILL_3  FILL_3_DIGITS(wert);
    #define FILL_4  FILL_4_DIGITS(wert);
    #define OK  return newnum;
    if (wert >= 0) {
      #define ALLOC(i)  \
        var object newnum = allocate_bignum(i,0); \
        var uintD* ptr = &TheBignum(newnum)->data[i-1];
      #define IF_LENGTH(i)  \
        if ((bn_minlength <= i) && (i*intDsize <= 32))       \
          if (!((i+1)*intDsize <= 32)                        \
              || ((uint32)wert < (uint32)bitc(i*intDsize-1)))
     #if (intDsize <= 32)
      IF_LENGTH(1)
        pos1: { ALLOC(1); FILL_1; OK; } /* bignum with 1 digit */
     #if (intDsize <= 16)
      IF_LENGTH(2)
        pos2: { ALLOC(2); FILL_2; OK; } /* bignum with 2 digits */
     #if (intDsize <= 8)
      IF_LENGTH(3)
      { ALLOC(3); FILL_3; OK; } /* bignum with 3 digits */
      IF_LENGTH(4)
        pos4: { ALLOC(4); FILL_4; OK; } /* bignum with 4 digits */
     #endif
     #endif
     #endif
      #undef IF_LENGTH
      #undef ALLOC
    } else {
      #define ALLOC(i)  \
        var object newnum = allocate_bignum(i,-1); \
        var uintD* ptr = &TheBignum(newnum)->data[i-1];
      #define IF_LENGTH(i)  \
        if ((bn_minlength <= i) && (i*intDsize <= 32))           \
          if (!((i+1)*intDsize <= 32)                            \
              || ((uint32)wert >= (uint32)(-bitc(i*intDsize-1))) \
             )
     #if (intDsize <= 32)
      IF_LENGTH(1)
        neg1: { ALLOC(1); FILL_1; OK; } /* bignum with 1 digit */
     #if (intDsize <= 16)
      IF_LENGTH(2)
        neg2: { ALLOC(2); FILL_2; OK; } /* bignum with 2 digits */
     #if (intDsize <= 8)
      IF_LENGTH(3)
      { ALLOC(3); FILL_3; OK; } /* bignum with 3 digits */
      IF_LENGTH(4)
        neg4: { ALLOC(4); FILL_4; OK; } /* bignum with 4 digits */
     #endif
     #endif
     #endif
      #undef IF_LENGTH
      #undef ALLOC
    }
    #undef OK
    #undef FILL_4
    #undef FILL_3
    #undef FILL_2
    #undef FILL_1
    #undef FILL_4_DIGITS
    #undef FILL_3_DIGITS
    #undef FILL_2_DIGITS
    #undef FILL_1_DIGIT
  }
}
#endif

/* converts unsigned longword in integer >=0 .
 UL_to_I(wert)
 > wert: value of the integer, an unsigned 32-bit-integer.
 < result: integer with this value.
 can trigger GC */
#if !(intLsize<=oint_data_len) /* if not already defined in lispbibl.d */
global maygc object UL_to_I (uint32 wert)
{
  if ((wert & ~ (FN_value_mask >> oint_data_shift)) == 0)
    /* all bits, that do not fit into the fixnum-value, =0 ? */
    return as_object(((oint)fixnum_type<<oint_type_shift) | (wert<<oint_data_shift));
  /* create bignum:
     (its length  bn_minlength <= n <= ceiling((32+1)/intDsize)  ) */
 #define UL_maxlength  ceiling(32+1,intDsize)
 #if (bn_minlength <= 1) && (UL_maxlength >= 1)
  if ((1*intDsize-1 < 32)
      ? (wert <= (uint32)(bitc(1*intDsize-1)-1))
      : true) { /* bignum with 1 digit */
    var object newnum = allocate_bignum(1,0);
    TheBignum(newnum)->data[0] = (uintD)wert;
    return newnum;
  }
 #endif
 #if (bn_minlength <= 2) && (UL_maxlength >= 2)
  if ((2*intDsize-1 < 32)
      ? (wert <= (uint32)(bitc(2*intDsize-1)-1))
      : true) { /* bignum with 2 digits */
    var object newnum = allocate_bignum(2,0);
    var uintD* ptr = &TheBignum(newnum)->data[1];
    *ptr-- = (uintD)wert;
   #if (intDsize>=32)
    *ptr = 0;
   #else
    wert = wert >> intDsize; *ptr = (uintD)wert;
   #endif
    return newnum;
  }
 #endif
 #if (bn_minlength <= 3) && (UL_maxlength >= 3)
  if ((3*intDsize-1 < 32)
      ? (wert <= (uint32)(bitc(3*intDsize-1)-1))
      : true) { /* bignum with 3 digits */
    var object newnum = allocate_bignum(3,0);
    var uintD* ptr = &TheBignum(newnum)->data[2];
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert;
   #if (2*intDsize>=32)
    *ptr = 0;
   #else
    wert = wert >> intDsize; *ptr = (uintD)wert;
   #endif
    return newnum;
  }
 #endif
 #if (bn_minlength <= 4) && (UL_maxlength >= 4)
  if ((4*intDsize-1 < 32)
      ? (wert <= (uint32)(bitc(4*intDsize-1)-1))
      : true) { /* bignum with 4 digits */
    var object newnum = allocate_bignum(4,0);
    var uintD* ptr = &TheBignum(newnum)->data[3];
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert;
   #if (3*intDsize>=32)
    *ptr = 0;
   #else
    wert = wert >> intDsize; *ptr = (uintD)wert;
   #endif
    return newnum;
  }
 #endif
 #if (bn_minlength <= 5) && (UL_maxlength >= 5)
  if (true) { /* bignum with 5 digits */
    var object newnum = allocate_bignum(5,0);
    var uintD* ptr = &TheBignum(newnum)->data[4];
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert;
   #if (4*intDsize>=32)
    *ptr = 0;
   #else
    wert = wert >> intDsize; *ptr = (uintD)wert;
   #endif
    return newnum;
  }
 #endif
}
#endif

/* converts double-longword in integer.
 L2_to_I(wert_hi,wert_lo)
 > wert_hi|wert_lo: value of the integer, a signed 64-bit-integer.
 < result: integer with this value.
 can trigger GC */
#if !(intVsize>32) /* if not already defined in lispbibl.d */
global maygc object L2_to_I (sint32 wert_hi, uint32 wert_lo)
{
  if (wert_hi == 0) {
    if ((wert_lo & (uint32)(~(FN_value_mask >> oint_data_shift))) /* bits of wert_lo, that do not fit into the fixnum-value */
        == (uint32)0) /* all =0 ? */
      return as_object(((oint)fixnum_type<<oint_type_shift) | ((oint)wert_lo<<oint_data_shift));
  } else if (wert_hi == ~(uintL)0) {
    if ((wert_lo & (uint32)(~(FN_value_mask >> oint_data_shift))) /* bits of wert_lo, that do not fit into the fixnum-value */
        == (uint32)(~(FN_value_mask >> oint_data_shift))) /* all =1 ? */
     #ifndef WIDE
      return as_object(((((oint)fixnum_vz_type<<oint_type_shift)+FN_value_mask) & (wert_lo<<oint_data_shift))
                       |(((oint)fixnum_vz_type<<oint_type_shift) & (wbit(oint_data_shift)-1)));
     #else
      return as_object(((oint)fixnum_vz_type<<oint_type_shift) | ((oint)(wert_lo & (uint32)(FN_value_mask >> oint_data_shift)) << oint_data_shift));
     #endif
  }
  /* create bignum:
     (its length bn_minlength <= n <= ceiling(64/intDsize) ) */
  #define FILL_1_DIGIT(from)  \
    *ptr-- = (uintD)from;
  #define FILL_2_DIGITS(from)  \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from;
  #define FILL_3_DIGITS(from)  \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from;
  #define FILL_4_DIGITS(from)  \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from;
  #if (32/intDsize==1)
    #define FILL_1  FILL_1_DIGIT(wert_lo);
    #define FILL_2  FILL_1_DIGIT(wert_lo); FILL_1_DIGIT(wert_hi);
    #define FILL_3
    #define FILL_4
    #define FILL_5
    #define FILL_6
    #define FILL_7
    #define FILL_8
  #endif
  #if (32/intDsize==2)
    #define FILL_1  FILL_1_DIGIT(wert_lo);
    #define FILL_2  FILL_2_DIGITS(wert_lo);
    #define FILL_3  FILL_2_DIGITS(wert_lo); FILL_1_DIGIT(wert_hi);
    #define FILL_4  FILL_2_DIGITS(wert_lo); FILL_2_DIGITS(wert_hi);
    #define FILL_5
    #define FILL_6
    #define FILL_7
    #define FILL_8
  #endif
  #if (32/intDsize==4)
    #define FILL_1  FILL_1_DIGIT(wert_lo);
    #define FILL_2  FILL_2_DIGITS(wert_lo);
    #define FILL_3  FILL_3_DIGITS(wert_lo);
    #define FILL_4  FILL_4_DIGITS(wert_lo);
    #define FILL_5  FILL_4_DIGITS(wert_lo); FILL_1_DIGIT(wert_hi);
    #define FILL_6  FILL_4_DIGITS(wert_lo); FILL_2_DIGITS(wert_hi);
    #define FILL_7  FILL_4_DIGITS(wert_lo); FILL_3_DIGITS(wert_hi);
    #define FILL_8  FILL_4_DIGITS(wert_lo); FILL_4_DIGITS(wert_hi);
  #endif
  #define OK  return newnum;
  if (wert_hi >= 0) {
    #define ALLOC(i)  \
      var object newnum = allocate_bignum(i,0); \
      var uintD* ptr = &TheBignum(newnum)->data[i-1];
    #define IF_LENGTH(i)  \
      if ((bn_minlength <= i) && (i*intDsize <= 64))                         \
        if (!((i+1)*intDsize <= 64)                                          \
            || (i*intDsize-1 < 32                                            \
                ? ((wert_hi == 0) && (wert_lo < (uint32)bitc(i*intDsize-1))) \
                : ((uint32)wert_hi < (uint32)bitc(i*intDsize-1-32))))
    IF_LENGTH(1)
      { ALLOC(1); FILL_1; OK; } /* bignum with 1 digit */
    IF_LENGTH(2)
      { ALLOC(2); FILL_2; OK; } /* bignum with 2 digits */
    IF_LENGTH(3)
      { ALLOC(3); FILL_3; OK; } /* bignum with 3 digits */
    IF_LENGTH(4)
      { ALLOC(4); FILL_4; OK; } /* bignum with 4 digits */
    IF_LENGTH(5)
      { ALLOC(5); FILL_5; OK; } /* bignum with 5 digits */
    IF_LENGTH(6)
      { ALLOC(6); FILL_6; OK; } /* bignum with 6 digits */
    IF_LENGTH(7)
      { ALLOC(7); FILL_7; OK; } /* bignum with 7 digits */
    IF_LENGTH(8)
      { ALLOC(8); FILL_8; OK; } /* bignum with 8 digits */
    #undef IF_LENGTH
    #undef ALLOC
  } else {
    #define ALLOC(i)  \
      var object newnum = allocate_bignum(i,-1); \
      var uintD* ptr = &TheBignum(newnum)->data[i-1];
    #define IF_LENGTH(i)  \
      if ((bn_minlength <= i) && (i*intDsize <= 64))                    \
        if (!((i+1)*intDsize <= 64)                                     \
            || (i*intDsize-1 < 32                                       \
                ? ((wert_hi == ~(uint32)0) && (wert_lo >= (uint32)(-bitc(i*intDsize-1)))) \
                : ((uint32)wert_hi >= (uint32)(-bitc(i*intDsize-1-32)))))
    IF_LENGTH(1)
      { ALLOC(1); FILL_1; OK; } /* bignum with 1 digit */
    IF_LENGTH(2)
      { ALLOC(2); FILL_2; OK; } /* bignum with 2 digits */
    IF_LENGTH(3)
      { ALLOC(3); FILL_3; OK; } /* bignum with 3 digits */
    IF_LENGTH(4)
      { ALLOC(4); FILL_4; OK; } /* bignum with 4 digits */
    IF_LENGTH(5)
      { ALLOC(5); FILL_5; OK; } /* bignum with 5 digits */
    IF_LENGTH(6)
      { ALLOC(6); FILL_6; OK; } /* bignum with 6 digits */
    IF_LENGTH(7)
      { ALLOC(7); FILL_7; OK; } /* bignum with 7 digits */
    IF_LENGTH(8)
      { ALLOC(8); FILL_8; OK; } /* bignum with 8 digits */
    #undef IF_LENGTH
    #undef ALLOC
  }
  #undef OK
  #undef FILL_8
  #undef FILL_7
  #undef FILL_6
  #undef FILL_5
  #undef FILL_4
  #undef FILL_3
  #undef FILL_2
  #undef FILL_1
  #undef FILL_4_DIGITS
  #undef FILL_3_DIGITS
  #undef FILL_2_DIGITS
  #undef FILL_1_DIGIT
}
#endif

/* converts an unsigned doppel-longword into an integer.
 UL2_to_I(wert_hi,wert_lo)
 > wert_hi|wert_lo: value of the integer, an unsigned 64-bit-integer.
 < result: integer with this value.
 can trigger GC */
#if !(intVsize>32) /* if not already defined in lispbibl.d */
global maygc object UL2_to_I (uint32 wert_hi, uint32 wert_lo)
{
  if ((wert_hi == 0)
      && ((wert_lo & (uint32)(~(FN_value_mask >> oint_data_shift))) /* bits of wert_lo, that do not fit into the fixnum-value */
          == (uint32)0)) /* all =0 ? */
    return as_object(((oint)fixnum_type<<oint_type_shift) | ((oint)wert_lo<<oint_data_shift));
  /* create bignum:
     (its length bn_minlength <= n <= ceiling((64+1)/intDsize) ) */
  #define UL2_maxlength  ceiling(64+1,intDsize)
  #define FILL_1_DIGIT(from)  \
    *ptr-- = (uintD)from;
  #define FILL_2_DIGITS(from)  \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from;
  #define FILL_3_DIGITS(from)  \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from;
  #define FILL_4_DIGITS(from)  \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from;
  #if (32/intDsize==1)
    #define FILL_1  FILL_1_DIGIT(wert_lo);
    #define FILL_2  FILL_1_DIGIT(wert_lo); FILL_1_DIGIT(wert_hi);
    #define FILL_3  FILL_2 *ptr-- = 0;
    #define FILL_4
    #define FILL_5
    #define FILL_6
    #define FILL_7
    #define FILL_8
    #define FILL_9
  #endif
  #if (32/intDsize==2)
    #define FILL_1  FILL_1_DIGIT(wert_lo);
    #define FILL_2  FILL_2_DIGITS(wert_lo);
    #define FILL_3  FILL_2_DIGITS(wert_lo); FILL_1_DIGIT(wert_hi);
    #define FILL_4  FILL_2_DIGITS(wert_lo); FILL_2_DIGITS(wert_hi);
    #define FILL_5  FILL_4 *ptr-- = 0;
    #define FILL_6
    #define FILL_7
    #define FILL_8
    #define FILL_9
  #endif
  #if (32/intDsize==4)
    #define FILL_1  FILL_1_DIGIT(wert_lo);
    #define FILL_2  FILL_2_DIGITS(wert_lo);
    #define FILL_3  FILL_3_DIGITS(wert_lo);
    #define FILL_4  FILL_4_DIGITS(wert_lo);
    #define FILL_5  FILL_4_DIGITS(wert_lo); FILL_1_DIGIT(wert_hi);
    #define FILL_6  FILL_4_DIGITS(wert_lo); FILL_2_DIGITS(wert_hi);
    #define FILL_7  FILL_4_DIGITS(wert_lo); FILL_3_DIGITS(wert_hi);
    #define FILL_8  FILL_4_DIGITS(wert_lo); FILL_4_DIGITS(wert_hi);
    #define FILL_9  FILL_8 *ptr-- = 0;
  #endif
  #define OK  return newnum;
  #define ALLOC(i)  \
    var object newnum = allocate_bignum(i,0); \
    var uintD* ptr = &TheBignum(newnum)->data[i-1];
  #define IF_LENGTH(i)  \
    if ((bn_minlength <= i) && (UL2_maxlength >= i))                       \
      if ((i*intDsize >= 64+1)                                             \
          || (i*intDsize-1 < 32                                            \
              ? ((wert_hi == 0) && (wert_lo < (uint32)bitc(i*intDsize-1))) \
              : (wert_hi < (uint32)bitc(i*intDsize-1-32))))
  IF_LENGTH(1)
    { ALLOC(1); FILL_1; OK; } /* bignum with 1 digit */
  IF_LENGTH(2)
    { ALLOC(2); FILL_2; OK; } /* bignum with 2 digits */
  IF_LENGTH(3)
    { ALLOC(3); FILL_3; OK; } /* bignum with 3 digits */
  IF_LENGTH(4)
    { ALLOC(4); FILL_4; OK; } /* bignum with 4 digits */
  IF_LENGTH(5)
    { ALLOC(5); FILL_5; OK; } /* bignum with 5 digits */
  IF_LENGTH(6)
    { ALLOC(6); FILL_6; OK; } /* bignum with 6 digits */
  IF_LENGTH(7)
    { ALLOC(7); FILL_7; OK; } /* bignum with 7 digits */
  IF_LENGTH(8)
    { ALLOC(8); FILL_8; OK; } /* bignum with 8 digits */
  IF_LENGTH(8)
    { ALLOC(9); FILL_9; OK; } /* bignum with 9 digits */
  #undef IF_LENGTH
  #undef ALLOC
  #undef OK
  #undef FILL_9
  #undef FILL_8
  #undef FILL_7
  #undef FILL_6
  #undef FILL_5
  #undef FILL_4
  #undef FILL_3
  #undef FILL_2
  #undef FILL_1
  #undef FILL_4_DIGITS
  #undef FILL_3_DIGITS
  #undef FILL_2_DIGITS
  #undef FILL_1_DIGIT
}
#endif

#if defined(intQsize) || (intVsize>32)
/* converts quadword into integer.
 Q_to_I(wert)
 > wert: value of the integer, a signed 64-bit-integer.
 < result: integer with this value.
 can trigger GC */
global maygc object Q_to_I (sint64 wert)
{
  {
    var uint64 test = wert & ~(uint64)(FN_value_mask >> oint_data_shift);
    /* test contains the bits, that do not fit into the fixnum-value. */
    if (test == (uint64)0) /* all =0 ? */
      return as_object(((oint)fixnum_type<<oint_type_shift) | ((oint)wert<<oint_data_shift));
    if (test == ~(uint64)(FN_value_mask >> oint_data_shift)) /* all =1 ? */
      return as_object(((((oint)fixnum_vz_type<<oint_type_shift)+FN_value_mask) & ((oint)wert<<oint_data_shift))
                       |(((oint)fixnum_vz_type<<oint_type_shift) & (wbit(oint_data_shift)-1)));
  }
  /* create bignum:
     (its length bn_minlength <= n <= ceiling(64/intDsize) = 2 ) */
  #define FILL_1_DIGIT(from)  \
    *ptr-- = (uintD)from;
  #define FILL_2_DIGITS(from)  \
    *ptr-- = (uintD)from; from = from >> intDsize; \
    *ptr-- = (uintD)from;
  #define FILL_1  FILL_1_DIGIT(wert);
  #define FILL_2  FILL_2_DIGITS(wert);
  #define OK  return newnum;
  if (wert >= 0) {
    #define ALLOC(i)  \
      var object newnum = allocate_bignum(i,0); \
      var uintD* ptr = &TheBignum(newnum)->data[i-1];
    #define IF_LENGTH(i)  \
      if ((bn_minlength <= i) && (i*intDsize <= 64))        \
        if (!((i+1)*intDsize <= 64)                         \
            || ((uint64)wert < (uint64)wbitc(i*intDsize-1)) \
           )
    IF_LENGTH(1)
      { ALLOC(1); FILL_1; OK; } /* bignum with 1 digit */
    IF_LENGTH(2)
      { ALLOC(2); FILL_2; OK; } /* bignum with 2 digits */
    #undef IF_LENGTH
    #undef ALLOC
  } else {
    #define ALLOC(i)  \
      var object newnum = allocate_bignum(i,-1); \
      var uintD* ptr = &TheBignum(newnum)->data[i-1];
    #define IF_LENGTH(i)  \
      if ((bn_minlength <= i) && (i*intDsize <= 64))          \
        if (!((i+1)*intDsize <= 64)                           \
            || ((uint64)wert >= -(uint64)wbitc(i*intDsize-1)) \
           )
    IF_LENGTH(1)
      { ALLOC(1); FILL_1; OK; } /* bignum with 1 digit */
    IF_LENGTH(2)
      { ALLOC(2); FILL_2; OK; } /* bignum with 2 digits */
    #undef IF_LENGTH
    #undef ALLOC
  }
  #undef OK
  #undef FILL_2
  #undef FILL_1
  #undef FILL_2_DIGITS
  #undef FILL_1_DIGIT
}
#endif

#if defined(intQsize) || (intVsize>32) || defined(WIDE_HARD) || (SIZEOF_OFF_T > 4) || (SIZEOF_INO_T > 4)
/* converts unsigned quadword into integer >=0 .
 UQ_to_I(wert)
 > wert: value of the integer, an unsigned 64-bit-integer.
 < result: integer with this value.
 can trigger GC */
global maygc object UQ_to_I (uint64 wert)
{
  if ((wert & ~(uint64)(FN_value_mask >> oint_data_shift)) == 0)
    /* all bits, that do not fit into the fixnum-value, =0 ? */
    return as_object(((oint)fixnum_type<<oint_type_shift) | (oint)(wert<<oint_data_shift));
  /* create bignum:
     (its length bn_minlength <= n <= ceiling((64+1)/intDsize) ) */
 #define UQ_maxlength  ceiling(64+1,intDsize)
 #if (bn_minlength <= 1) && (UQ_maxlength >= 1)
  if ((1*intDsize-1 < 64)
      ? (wert <= (uint64)(wbitc(1*intDsize-1)-1))
      : true) { /* bignum with 1 digit */
    var object newnum = allocate_bignum(1,0);
    TheBignum(newnum)->data[0] = (uintD)wert;
    return newnum;
  }
 #endif
 #if (bn_minlength <= 2) && (UQ_maxlength >= 2)
  if ((2*intDsize-1 < 64)
      ? (wert <= (uint64)(wbitc(2*intDsize-1)-1))
      : true) { /* bignum with 2 digits */
    var object newnum = allocate_bignum(2,0);
    var uintD* ptr = &TheBignum(newnum)->data[1];
    *ptr-- = (uintD)wert;
   #if (intDsize>=64)
    *ptr = 0;
   #else
    wert = wert >> intDsize; *ptr = (uintD)wert;
   #endif
    return newnum;
  }
 #endif
 #if (bn_minlength <= 3) && (UQ_maxlength >= 3)
  if ((3*intDsize-1 < 64)
      ? (wert <= (uint64)(wbitc(3*intDsize-1)-1))
      : true) { /* bignum with 3 digits */
    var object newnum = allocate_bignum(3,0);
    var uintD* ptr = &TheBignum(newnum)->data[2];
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert;
   #if (2*intDsize>=64)
    *ptr = 0;
   #else
    wert = wert >> intDsize; *ptr = (uintD)wert;
   #endif
    return newnum;
  }
 #endif
 #if (bn_minlength <= 4) && (UQ_maxlength >= 4)
  if ((4*intDsize-1 < 64)
      ? (wert <= (uint64)(wbitc(4*intDsize-1)-1))
      : true) { /* bignum with 4 digits */
    var object newnum = allocate_bignum(4,0);
    var uintD* ptr = &TheBignum(newnum)->data[3];
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert;
   #if (3*intDsize>=64)
    *ptr = 0;
   #else
    wert = wert >> intDsize; *ptr = (uintD)wert;
   #endif
    return newnum;
  }
 #endif
 #if (bn_minlength <= 5) && (UQ_maxlength >= 5)
  if ((5*intDsize-1 < 64)
      ? (wert <= (uint64)(wbitc(5*intDsize-1)-1))
      : true) { /* bignum with 5 digits */
    var object newnum = allocate_bignum(5,0);
    var uintD* ptr = &TheBignum(newnum)->data[4];
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert;
   #if (4*intDsize>=64)
    *ptr = 0;
   #else
    wert = wert >> intDsize; *ptr = (uintD)wert;
   #endif
    return newnum;
  }
 #endif
 #if (bn_minlength <= 6) && (UQ_maxlength >= 6)
  if ((6*intDsize-1 < 64)
      ? (wert <= (uint64)(wbitc(6*intDsize-1)-1))
      : true) { /* bignum with 6 digits */
    var object newnum = allocate_bignum(6,0);
    var uintD* ptr = &TheBignum(newnum)->data[5];
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert;
   #if (5*intDsize>=64)
    *ptr = 0;
   #else
    wert = wert >> intDsize; *ptr = (uintD)wert;
   #endif
    return newnum;
  }
 #endif
 #if (bn_minlength <= 7) && (UQ_maxlength >= 7)
  if ((7*intDsize-1 < 64)
      ? (wert <= (uint64)(wbitc(7*intDsize-1)-1))
      : true) { /* bignum with 7 digits */
    var object newnum = allocate_bignum(7,0);
    var uintD* ptr = &TheBignum(newnum)->data[6];
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert;
   #if (6*intDsize>=64)
    *ptr = 0;
   #else
    wert = wert >> intDsize; *ptr = (uintD)wert;
   #endif
    return newnum;
  }
 #endif
 #if (bn_minlength <= 8) && (UQ_maxlength >= 8)
  if ((8*intDsize-1 < 64)
      ? (wert <= (uint64)(wbitc(8*intDsize-1)-1))
      : true) { /* bignum with 8 digits */
    var object newnum = allocate_bignum(8,0);
    var uintD* ptr = &TheBignum(newnum)->data[7];
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert;
   #if (7*intDsize>=64)
    *ptr = 0;
   #else
    wert = wert >> intDsize; *ptr = (uintD)wert;
   #endif
    return newnum;
  }
 #endif
 #if (bn_minlength <= 9) && (UQ_maxlength >= 9)
  if (true) { /* bignum with 9 digits */
    var object newnum = allocate_bignum(9,0);
    var uintD* ptr = &TheBignum(newnum)->data[8];
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert; wert = wert >> intDsize;
    *ptr-- = (uintD)wert;
   #if (8*intDsize>=64)
    *ptr = 0;
   #else
    wert = wert >> intDsize; *ptr = (uintD)wert;
   #endif
    return newnum;
  }
 #endif
}
#endif

/* returns the differenz x-y of two unsigned longwords x,y as integer.
 UL_UL_minus_I(x,y) */
local maygc object UL_UL_minus_I (object x, object y);
#ifdef intQsize
  #define UL_UL_minus_I(x,y)  Q_to_I((sintQ)(uintQ)(x)-(sintQ)(uintQ)(y))
#else
  #define UL_UL_minus_I(x,y)  L2_to_I( ((x)<(y) ? -1L : 0), (x)-(y) )
#endif

/* conversion routines digit sequence --> integer: */

/* Normalized Digit sequence to Integer
 NDS_to_I(MSDptr,len)
 convert digit sequence MSDptr/len/.. in integer.
 can trigger GC */
local maygc object NDS_to_I (const uintD* MSDptr, uintC len)
{
  /* more than bn_minlength digits -> bignum.
     less than bn_minlength digits -> fixnum.
     exactly   bn_minlength Digits -> bignum or fixnum. */
  if (len < bn_minlength) {
    /* 0..bn_minlength-1 digits, fits into a fixnum: */
    if (bn_minlength>1 ? (len==0) : true)
      /* 0 digits */
      return Fixnum_0;
   #if !(defined(intQsize) || (intVsize>32))
    var sint32 wert;
    if (bn_minlength>2 ? (len==1) : true) { /* 1 digit */
     len_1: wert = get_sint1D_Dptr(MSDptr);
    } else if (bn_minlength>3 ? (len==2) : true) { /* 2 digits */
     len_2: wert = get_sint2D_Dptr(MSDptr);
    } else if (bn_minlength>4 ? (len==3) : true) { /* 3 digits */
     len_3: wert = get_sint3D_Dptr(MSDptr);
    } else if (true) { /* 4 digits */
     len_4: wert = get_sint4D_Dptr(MSDptr);
    } else if (false) { /* 5 digits */
     len_5: wert = get_sint4D_Dptr(&MSDptr[1]); }
   #else /* (defined(intQsize) || (intVsize>32)) && (intDsize==32) */
    var sint64 wert;
    if (true) { /* 1 digit */
     len_1: wert = (sint64)(sintD)MSDptr[0];
    } else if (true) { /* 2 digits */
     len_2:
      wert = ((sint64)(sintD)MSDptr[0] << intDsize) | (uint64)(uintD)MSDptr[1];
    }
   #endif
    return
     #if (oint_data_shift <= sign_bit_o) && ((oint_data_len+1 <= intLsize) || defined(intQsize))
      as_object((( (soint)wert
                   & (FN_value_vz_mask>>oint_data_shift) /* mask the unneeded */
                   ) << oint_data_shift
                 )
                | ((oint)fixnum_type<<oint_type_shift) /* store typeinfo instead */
                )
     #else
      /* if (oint_data_shift > sign_bit_o) or if the sign bit is not in wert */
      as_object((( (soint)wert << oint_data_shift )
                 & FN_value_mask /* mask the unneeded */
                 )
                | ((soint)(sint32)sign_of_sintD(MSDptr[0]) & wbit(sign_bit_o))
                | ((oint)fixnum_type<<oint_type_shift) /* store typeinfo instead */
                )
     #endif
      ;
  }
  if (len == bn_minlength) {
    /* bn_minlength digits, i.e. between (bn_minlength-1)*intDsize+1
       and bn_minlength*intDsize bits (incl. sign).
       at most oint_data_len+1 bits -> fits into a fixnum: */
    if (  (MSDptr[0] <= (uintD)(bit(oint_data_len-(bn_minlength-1)*intDsize)-1)) /* fixnum >=0 ? */
          ||(MSDptr[0] >= (uintD)(-bit(oint_data_len-(bn_minlength-1)*intDsize)))) /* fixnum <0 ? */
     #if (bn_minlength==1)
      goto len_1;
     #endif
     #if (bn_minlength==2)
      goto len_2;
     #endif
     #if (bn_minlength==3)
      goto len_3;
     #endif
     #if (bn_minlength==4)
      goto len_4;
     #endif
     #if (bn_minlength==5)
      goto len_5;
     #endif
  }
  /* at least bn_minlength digits, create a bignum */
  var object newnum = allocate_bignum(len,(sintB)sign_of_sintD(MSDptr[0]));
  /* fill new bignum with the content of the NDS: */
  copy_loop_up(MSDptr,&TheBignum(newnum)->data[0],len);
  return newnum;
}

/* report Bignum-overflow: */
nonreturning_function(local, BN_ueberlauf, (void)) {
  fehler(arithmetic_error,GETTEXT("bignum overflow"));
}

/* Normalized Unsigned Digit Sequence to Integer
 NUDS_to_I(MSDptr,len)
 convert Normalized UDS MSDptr/len/.. into Integer >=0 .
 there must be room for 1 digit below of MSDptr.
 can trigger GC */
local maygc object NUDS_to_I (uintD* MSDptr, uintC len)
{
  if ((len!=0) && ((sintD)MSDptr[0] < 0)) {
    /* if the length is >0 and the most significant bit is = 1 ,
       extend the digit sequence by one zero-digit: */
    *--MSDptr = 0;
    len++;
    if (uintWCoverflow(len)) /* overflow of the length? */
      BN_ueberlauf();
  }
  return NDS_to_I(MSDptr,len);
}

/* Unsigned Digit Sequence to Integer
 UDS_to_I(MSDptr,len)
 convert UDS MSDptr/len/.. into Integer >=0 .
 there must be room for 1 digit below of MSDptr.
 can trigger GC */
global maygc object UDS_to_I (uintD* MSDptr, uintC len)
{
  while ( (len!=0) && (MSDptr[0]==0) ) { /* so long as len>0 and MSD = 0, */
    MSDptr++; len--; /* discard null-digit */
  }
  /* Then proceed like in NUDS_to_I : */
  if ((len!=0) && ((sintD)MSDptr[0] < 0)) {
    /* if the length is >0 and the most significant bit is = 1 ,
       extend the digit sequence by one null digit: */
    *--MSDptr = 0;
    len++;
    if (uintWCoverflow(len)) /* overflow of the length? */
      BN_ueberlauf();
  }
  return NDS_to_I(MSDptr,len);
}

/* Digit Sequence to Integer
 DS_to_I(MSDptr,len)
 convert DS MSDptr/len/.. into Integer.
 can trigger GC */
global maygc object DS_to_I (const uintD* MSDptr, uintC len)
{
  /* first normalize. poss. increase MSDptr and decrease len: */
  if (len!=0) { /* empty DS is normalized */
    var uintC count = len-1;
    if ((sintD)MSDptr[0] >= 0) { /* number >= 0 */
      /* try to discard at most len-1 leading null-digits: */
      while (!(count==0) && (MSDptr[0]==0) && ((sintD)MSDptr[1]>=0)) {
        MSDptr++; len--; count--; /* discard nulldigit */
      }
    } else { /* number < 0 */
      /* try to discard at most len-1 leading one-digits: */
      while (!(count==0) && ((sintD)MSDptr[0]==-1) && ((sintD)MSDptr[1]<0)) {
        MSDptr++; len--; count--; /* discard one-digit */
      }
    }
  }
  /* possibly, len is exceptionally =1 at the DS 0,
     but NDS_to_I will cope with it. */
  return NDS_to_I(MSDptr,len);
}

/* conversion routines Integer --> Digit sequence: */

/* subdivision of a fixnum in digits:
 intDsize=8 -> MSD=LSD3,LSD2,LSD1,LSD0, should be FN_maxlength=4 .
 intDsize=16 -> MSD=LSD1,LSD0, should be FN_maxlength=2 .
 intDsize=32 -> MSD=LSD0, should be FN_maxlength=1 .
 WIDE -> likewise, except that FN_maxlength is bigger by one. */

#if FN_maxlength>1
  #define FN_LSD0(obj)  ((uintD)(as_oint(obj)>>oint_data_shift))
#elif FN_maxlength==1
  #define FN_LSD0  FN_MSD
#endif
#if FN_maxlength>2
  #define FN_LSD1(obj)  ((uintD)(as_oint(obj)>>(oint_data_shift+intDsize)))
#elif FN_maxlength==2
  #define FN_LSD1  FN_MSD
#else /* FN_maxlength<2 */
  #define FN_LSD1(obj)  0; NOTREACHED  /* should not be called! */
#endif
#if FN_maxlength>3
  #define FN_LSD2(obj)  ((uintD)(as_oint(obj)>>(oint_data_shift+2*intDsize)))
#elif FN_maxlength==3
  #define FN_LSD2  FN_MSD
#else /* FN_maxlength<3 */
  #define FN_LSD2(obj)  0; NOTREACHED  /* should not be called! */
#endif
#if FN_maxlength>4
  #define FN_LSD3(obj)  ((uintD)(as_oint(obj)>>(oint_data_shift+3*intDsize)))
#elif FN_maxlength==4
  #define FN_LSD3  FN_MSD
#else /* FN_maxlength<4 */
  #define FN_LSD3(obj)  0; NOTREACHED  /* should not be called! */
#endif
#if FN_maxlength==5
  #define FN_LSD4  FN_MSD
#else /* FN_maxlength<5 */
  #define FN_LSD4(obj)  0; NOTREACHED  /* should not be called! */
#endif
/* FN_MSD: must be shifted by (FN_maxlength-1)*intDsize+oint_data_shift bits
 to the right, altogether. */
#if defined(WIDE) && defined(TYPECODES) && ((oint_data_len%intDsize)==0)
  #define FN_MSD(obj)  \
    ((uintD)( (sintD)(typecode(obj) << (intDsize-1-sign_bit_t)) >> (intDsize-1)))
#elif (sign_bit_o == oint_data_len+oint_data_shift) || ((oint_data_len==(FN_maxlength-1)*intDsize) && (sign_bit_o >= intDsize-1))
  #if (sign_bit_o >= intDsize)
    #define FN_MSD(obj)  \
      ((sintD)(as_oint(obj) >> (sign_bit_o-(intDsize-1))) >> (oint_data_shift-sign_bit_o+FN_maxlength*intDsize-1))
  #else
    #define FN_MSD(obj)  \
      (((sintD)as_oint(obj) << (intDsize-1-sign_bit_o)) >> (FN_maxlength*intDsize-1-sign_bit_o+oint_data_shift))
  #endif
#else
 /* signD_of_sintD(x,k) returns the sign of x as sintD; the rear
    k bits are irrelevant. */
  #if HAVE_DD
    #define signD_of_sintD(x,k)  ((sintDD)(sintD)(x)>>intDsize)
  #else
    #define signD_of_sintD(x,k)  ((sintD)(x)>>(intDsize-1-(k)))
  #endif
  #if (sign_bit_o >= intDsize)
    #define FN_MSD(obj)  \
      ( ((sintD)(as_oint(obj)>>(oint_data_shift+(FN_maxlength-1)*intDsize))&(bit(oint_data_len-(FN_maxlength-1)*intDsize)-1)) \
       |((sintD)signD_of_sintD(as_oint(obj)>>(sign_bit_o-(intDsize-1)),oint_data_len-(FN_maxlength-1)*intDsize)&(-bit(oint_data_len-(FN_maxlength-1)*intDsize))) \
      )
  #else /* (sign_bit_o < intDsize) */
    #define FN_MSD(obj)  \
      ( ((sintD)(as_oint(obj)>>(oint_data_shift+(FN_maxlength-1)*intDsize))&(bit(oint_data_len-(FN_maxlength-1)*intDsize)-1)) \
       |((sintD)signD_of_sintD(as_oint(obj)<<((intDsize-1)-sign_bit_o),oint_data_len-(FN_maxlength-1)*intDsize)&(-bit(oint_data_len-(FN_maxlength-1)*intDsize))) \
      )
  #endif
#endif

/* Fixnum to Normalized Digit sequence
 { FN_to_NDS_nocopy(obj, MSDptr=,len=,LSDptr=); ... }
 > obj: a fixnum
 < MSDptr/len/LSDptr: normalized digit sequence, in machine stack */
#define FN_to_NDS_nocopy(obj,MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung) \
  var uintD CONCAT(FN_to_NDS_room_,__LINE__)[FN_maxlength];             \
  FN_to_NDS_nocopy_(obj,CONCAT(FN_to_NDS_room_,__LINE__),_EMA_ MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung)

/* Fixnum to Normalized Digit sequence
 FN_to_NDS(obj, MSDptr=,len=,LSDptr=);
 > obj: a fixnum
 < MSDptr/len/LSDptr: normalized digit sequence, may be modified.
 num_stack is decreased. */
#define FN_to_NDS(obj,MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
  FN_to_NDS_(copy,obj,_EMA_ MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung)
#define alloc_FNDS_copy  num_stack_need

/* Fixnum to Normalized Digit sequence
 FN_to_NDS_1(obj, MSDptr=,len=,LSDptr=);
 > obj: a fixnum
 < MSDptr/len/LSDptr: normalized digit sequence, may be modified.
 below MSDptr, there is still room for one 1 digit.
 num_stack is decreased. */
#define FN_to_NDS_1(obj,MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung) \
  FN_to_NDS_(copy_1,obj,_EMA_ MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung)
#define alloc_FNDS_copy_1  num_stack_need_1

/* only needed, when FN_maxlength >= 2, i.e. intDsize-1 < oint_data_len */
#define FN_MSD1_mask                                                    \
  (FN_value_vz_mask & ~((oint)(bitc(intDsize-1)-1)<<oint_data_shift))
/* only needed, when FN_maxlength >= 3, i.e. 2*intDsize-1 < oint_data_len */
#define FN_MSD2_mask                                                    \
  (FN_value_vz_mask & ~((oint)(bitc(2*intDsize-1)-1)<<oint_data_shift))
/* only needed, when FN_maxlength >= 4, i.e. 3*intDsize-1 < oint_data_len */
#define FN_MSD3_mask                                                    \
  (FN_value_vz_mask & ~((oint)(bitc(3*intDsize-1)-1)<<oint_data_shift))
/* only needed, when FN_maxlength >= 5, i.e. 4*intDsize-1 < oint_data_len */
#define FN_MSD4_mask                                                    \
  (FN_value_vz_mask & ~((oint)(bitc(4*intDsize-1)-1)<<oint_data_shift))
#define FN_to_NDS_(option, obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung) \
  do { var oint fix_from_FN_to_NDS = as_oint(obj);                      \
    var uintC len_from_FN_to_NDS;                                       \
    var uintD* ptr_from_FN_to_NDS;                                      \
    /* determine length of the NDS: */                                  \
    if (eq(as_object(fix_from_FN_to_NDS),Fixnum_0)) { /* need at least 1 digit? */ \
      len_from_FN_to_NDS=0;                                             \
    } else {                                                            \
      var oint testMSD; /* fore bits of fix_from_FN_to_NDS */           \
      if ((FN_maxlength<=1) ||                                          \
          (((testMSD = fix_from_FN_to_NDS & FN_MSD1_mask) == 0)         \
           || (testMSD == FN_MSD1_mask))) {                             \
        len_from_FN_to_NDS=1; /* only one digit to store */             \
      } else if ((FN_maxlength<=2) ||                                   \
                 (((testMSD = fix_from_FN_to_NDS & FN_MSD2_mask) == 0)  \
                  || (testMSD == FN_MSD2_mask))) {                      \
        len_from_FN_to_NDS=2; /* two digits to store */                 \
      } else if ((FN_maxlength<=3) ||                                   \
                 (((testMSD = fix_from_FN_to_NDS & FN_MSD3_mask) == 0)  \
                  || (testMSD == FN_MSD3_mask))) {                      \
        len_from_FN_to_NDS=3; /* thre digits to store */                \
      } else if ((FN_maxlength<=4) ||                                   \
                 (((testMSD = fix_from_FN_to_NDS & FN_MSD4_mask) == 0)  \
                  || (testMSD == FN_MSD4_mask))) {                      \
        len_from_FN_to_NDS=4; /* four digits to store */                \
      } else {                                                          \
        len_from_FN_to_NDS=5; /* five digits to store */                \
      }                                                                 \
    }                                                                   \
    len_zuweisung len_from_FN_to_NDS;                                   \
    /* allocate space: */                                               \
    CONCAT(alloc_FNDS_,option)                                          \
      (len_from_FN_to_NDS, MSDptr_zuweisung ptr_from_FN_to_NDS =,_EMA_ LSDptr_zuweisung); \
    /* fill space: */                                                   \
    if (len_from_FN_to_NDS > 0) {                                       \
      if ((FN_maxlength>1) && (len_from_FN_to_NDS > 1)) {               \
        if ((FN_maxlength>2) && (len_from_FN_to_NDS > 2)) {             \
          if ((FN_maxlength>3) && (len_from_FN_to_NDS > 3)) {           \
            if ((FN_maxlength>4) && (len_from_FN_to_NDS > 4)) {         \
              /* five digits to store: */                               \
              *ptr_from_FN_to_NDS++ = FN_LSD4(as_object(fix_from_FN_to_NDS)); \
            }                                                           \
            /* still four digits to store abzulegen: */                 \
            *ptr_from_FN_to_NDS++ = FN_LSD3(as_object(fix_from_FN_to_NDS)); \
          }                                                             \
          /* still three digits to store: */                            \
          *ptr_from_FN_to_NDS++ = FN_LSD2(as_object(fix_from_FN_to_NDS)); \
        }                                                               \
        /* still two digits to store: */                                \
        *ptr_from_FN_to_NDS++ = FN_LSD1(as_object(fix_from_FN_to_NDS)); \
      }                                                                 \
      /* still one digit to store: */                                   \
      *ptr_from_FN_to_NDS = FN_LSD0(as_object(fix_from_FN_to_NDS));     \
    }                                                                   \
  } while(0)
#define FN_to_NDS_nocopy_(obj, room, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung) \
  do { var oint fix_from_FN_to_NDS = as_oint(obj);                      \
    var uintC len_from_FN_to_NDS;                                       \
    /* determine the length of the NDS and fill space: */               \
    if (eq(as_object(fix_from_FN_to_NDS),Fixnum_0)) { /* need at least 1 digit? */ \
      len_from_FN_to_NDS=0;                                             \
    } else {                                                            \
      var oint testMSD; /* fore bits of fix_from_FN_to_NDS */           \
      var uintD* ptr_from_FN_to_NDS = room;                             \
      if ((FN_maxlength<=1) ||                                          \
          (((testMSD = fix_from_FN_to_NDS & FN_MSD1_mask) == 0)         \
           || (testMSD == FN_MSD1_mask))) {                             \
        len_from_FN_to_NDS=1; /* only one digit to store */             \
      } else {                                                          \
        if ((FN_maxlength<=2) ||                                        \
            (((testMSD = fix_from_FN_to_NDS & FN_MSD2_mask) == 0)       \
             || (testMSD == FN_MSD2_mask))) {                           \
          len_from_FN_to_NDS=2; /* two digits to store */               \
        } else {                                                        \
          if ((FN_maxlength<=3) ||                                      \
              (((testMSD = fix_from_FN_to_NDS & FN_MSD3_mask) == 0)     \
               || (testMSD == FN_MSD3_mask))) {                         \
            len_from_FN_to_NDS=3; /* three digits to store */           \
          } else {                                                      \
            if ((FN_maxlength<=4) ||                                    \
                (((testMSD = fix_from_FN_to_NDS & FN_MSD4_mask) == 0)   \
                 || (testMSD == FN_MSD4_mask))) {                       \
              len_from_FN_to_NDS=4; /* four digits to store */          \
            } else {                                                    \
              len_from_FN_to_NDS=5; /* five digits to store */          \
              *ptr_from_FN_to_NDS++ = FN_LSD4(as_object(fix_from_FN_to_NDS)); \
            }                                                           \
            *ptr_from_FN_to_NDS++ = FN_LSD3(as_object(fix_from_FN_to_NDS)); \
          }                                                             \
          *ptr_from_FN_to_NDS++ = FN_LSD2(as_object(fix_from_FN_to_NDS)); \
        }                                                               \
        *ptr_from_FN_to_NDS++ = FN_LSD1(as_object(fix_from_FN_to_NDS)); \
      }                                                                 \
      *ptr_from_FN_to_NDS = FN_LSD0(as_object(fix_from_FN_to_NDS));     \
    }                                                                   \
    len_zuweisung len_from_FN_to_NDS;                                   \
    unused (LSDptr_zuweisung (MSDptr_zuweisung room) + len_from_FN_to_NDS); \
  } while(0)

/* Bignum to Normalized Digit sequence, copying unnecessary
 BN_to_NDS_nocopy(obj, MSDptr=,len=,LSDptr=);
 > obj: a Bignum
 < MSDptr/len/LSDptr: Normalized Digit sequence */
#define BN_to_NDS_nocopy(obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung) \
  do { var Bignum bn_from_BN_to_NDS_nocopy = TheBignum(obj);            \
    unused (MSDptr_zuweisung &bn_from_BN_to_NDS_nocopy->data[0]);       \
    unused (LSDptr_zuweisung &bn_from_BN_to_NDS_nocopy->data[(uintP)(   \
            len_zuweisung bignum_length(bn_from_BN_to_NDS_nocopy) )]);  \
  } while(0)

/* Bignum to Normalized Digit sequence
 BN_to_NDS(obj, MSDptr=,len=,LSDptr=);
 > obj: a Bignum
 < MSDptr/len/LSDptr: Normalized Digit sequence, may be modified.
 num_stack is decreased. */
#define BN_to_NDS(obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung) \
  do { var object obj_from_BN_to_NDS = (obj);                           \
    var uintD* MSDptr_from_BN_to_NDS;                                   \
    var uintC len_from_BN_to_NDS;                                       \
    len_zuweisung len_from_BN_to_NDS = Bignum_length(obj_from_BN_to_NDS); \
    num_stack_need(len_from_BN_to_NDS, MSDptr_zuweisung MSDptr_from_BN_to_NDS = ,_EMA_ LSDptr_zuweisung); \
    copy_loop_up(&TheBignum(obj_from_BN_to_NDS)->data[0],MSDptr_from_BN_to_NDS,len_from_BN_to_NDS); \
  } while(0)

/* Bignum to Normalized Digit sequence
 BN_to_NDS_1(obj, MSDptr=,len=,LSDptr=);
 > obj: a Bignum
 < MSDptr/len/LSDptr: Normalized Digit sequence, may be modified.
 below MSDptr, there is still room for one 1 digit.
 num_stack is decreased. */
#define BN_to_NDS_1(obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung) \
  do { var object obj_from_BN_to_NDS = (obj);                           \
    var uintD* MSDptr_from_BN_to_NDS;                                   \
    var uintC len_from_BN_to_NDS;                                       \
    len_zuweisung len_from_BN_to_NDS = Bignum_length(obj_from_BN_to_NDS); \
    num_stack_need_1(len_from_BN_to_NDS, MSDptr_zuweisung MSDptr_from_BN_to_NDS = ,_EMA_ LSDptr_zuweisung); \
    copy_loop_up(&TheBignum(obj_from_BN_to_NDS)->data[0],MSDptr_from_BN_to_NDS,len_from_BN_to_NDS); \
  } while(0)

/* Integer to Normalized Digit sequence, copying unnecessary.
 { I_to_NDS_nocopy(obj, MSDptr=,len=,LSDptr=); ... }
 > obj: an Integer
 < MSDptr/len/LSDptr: Normalized Digit sequence */
#define I_to_NDS_nocopy(obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung) \
  var uintD CONCAT(I_to_NDS_room_,__LINE__)[FN_maxlength]; do {         \
    var object obj_from_I_to_NDS_nocopy = (obj);                        \
    if (I_fixnump(obj_from_I_to_NDS_nocopy))                            \
      FN_to_NDS_nocopy_(obj_from_I_to_NDS_nocopy,CONCAT(I_to_NDS_room_,__LINE__),_EMA_ MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung); \
    else                                                                \
      BN_to_NDS_nocopy(obj_from_I_to_NDS_nocopy,_EMA_ MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung); \
  } while(0)

/* Integer to Normalized Digit sequence
 I_to_NDS(obj, MSDptr=,len=,LSDptr=);
 > obj: an Integer
 < MSDptr/len/LSDptr: Normalized Digit sequence, may be modified.
 num_stack is decreased. */
#define I_to_NDS(obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
  do { var object obj_from_I_to_NDS = (obj);                            \
    if (I_fixnump(obj_from_I_to_NDS))                                   \
      FN_to_NDS(obj_from_I_to_NDS,_EMA_ MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung); \
    else                                                                \
      BN_to_NDS(obj_from_I_to_NDS,_EMA_ MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung); \
  } while(0)

/* Integer to Normalized Digit sequence
 I_to_NDS_1(obj, MSDptr=,len=,LSDptr=);
 > obj: an Integer
 < MSDptr/len/LSDptr: Normalized Digit sequence, may be modified.
 below MSDptr, there is still room for one 1 digit.
 num_stack is decreased. */
#define I_to_NDS_1(obj, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung) \
  do { var object obj_from_I_to_NDS = (obj);                            \
    if (I_fixnump(obj_from_I_to_NDS))                                   \
      FN_to_NDS_1(obj_from_I_to_NDS,_EMA_ MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung); \
    else                                                                \
      BN_to_NDS_1(obj_from_I_to_NDS,_EMA_ MSDptr_zuweisung,len_zuweisung,_EMA_ LSDptr_zuweisung); \
  } while(0)

/* Fetches the next pFN_maxlength digits into a uintV:
 _ptr is of type uintD*. */
#if (pFN_maxlength==1)
  #define pFN_maxlength_digits_at(_ptr)  \
    (uintV)(_ptr[0])
#elif (pFN_maxlength==2) && (intDsize==16)
  #define pFN_maxlength_digits_at(_ptr)  \
    highlow32_at(_ptr)
#elif (pFN_maxlength==2)
  #define pFN_maxlength_digits_at(_ptr)  \
    (((uintV)(_ptr[0])<<intDsize)|       \
      (uintV)(_ptr[1]))
#elif (pFN_maxlength==3)
  #define pFN_maxlength_digits_at(_ptr)  \
    (((((uintV)(_ptr[0])<<intDsize)|     \
        (uintV)(_ptr[1]))<<intDsize)|    \
        (uintV)(_ptr[2]))
#elif (pFN_maxlength==4)
  #define pFN_maxlength_digits_at(_ptr)  \
    (((((((uintV)(_ptr[0])<<intDsize)|   \
          (uintV)(_ptr[1]))<<intDsize)|  \
          (uintV)(_ptr[2]))<<intDsize)|  \
          (uintV)(_ptr[3]))
#endif

/* writes a uint32 into the next pFN_maxlength digits:
 _ptr is of type uintD*, _wert of type uintV. */
#if (pFN_maxlength==1)
  #define set_pFN_maxlength_digits_at(_ptr,_wert)  \
    (_ptr[0] = (uintD)_wert)
#elif (pFN_maxlength==2) && (intDsize==16)
  #define set_pFN_maxlength_digits_at(_ptr,_wert)  \
    set_highlow32_at(_ptr,_wert)
#elif (pFN_maxlength==2)
  #define set_pFN_maxlength_digits_at(_ptr,_wert)  \
    (_ptr[0] = (uintD)(_wert>>intDsize), \
     _ptr[1] = (uintD)(_wert))
#elif (pFN_maxlength==3)
  #define set_pFN_maxlength_digits_at(_ptr,_wert)  \
    (_ptr[0] = (uintD)(_wert>>(2*intDsize)), \
     _ptr[1] = (uintD)(_wert>>intDsize),     \
     _ptr[2] = (uintD)(_wert))
#elif (pFN_maxlength==4)
  #define set_pFN_maxlength_digits_at(_ptr,_wert)  \
    (_ptr[0] = (uintD)(_wert>>(3*intDsize)), \
     _ptr[1] = (uintD)(_wert>>(2*intDsize)), \
     _ptr[2] = (uintD)(_wert>>intDsize),     \
     _ptr[3] = (uintD)(_wert))
#endif
