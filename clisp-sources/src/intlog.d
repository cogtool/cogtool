/* Logical operations on integers */

/* UP: returns the number of digits that an integer needs as a DS.
 (slightly rounded up.) */
local uintC I_to_DS_need (object obj) {
  if (I_fixnump(obj))
    return FN_maxlength; /* that will suffice */
  else
    return Bignum_length(obj);
}

/* Integer to Digit sequence, n Digits
 I_to_DS_n(obj,n,ptr=);
 Turn integer obj into a digit sequence MSDptr/n/LSDptr ,
 that has precisely n digits (n >= demand and n >= FN_maxlength ).
 The new digit sequence may be modified.
 < ptr: MSDptr of the new DS
 num_stack is decreased. */
#define I_to_DS_n(obj,n,ptr_zuweisung)          \
  do { var uintD* destptr;                      \
       num_stack_need(n,_EMA_,destptr=);        \
       ptr_zuweisung I_to_DS_n_(obj,n,destptr); \
  } while(0)
local uintD* I_to_DS_n_ (object obj, uintC n, uintD* destptr) {
  /* Now there is room for n digits below destptr.
     fill upper part of the DS from obj, decrease destptr: */
  if (I_fixnump(obj)) { /* fixnum: */
    var uintV wert = FN_to_V(obj);
    #define FN_maxlength_a  (intVsize/intDsize)
    #define FN_maxlength_b  (FN_maxlength<=FN_maxlength_a ? FN_maxlength : FN_maxlength_a)
    /* store FN_maxlength. FN_maxlength_b digits can be taken from wert. */
   #if (FN_maxlength_b > 1)
    doconsttimes(FN_maxlength_b-1,
                 *--destptr = (uintD)wert; wert = wert >> intDsize;);
   #endif
    *--destptr = (uintD)wert;
   #if (FN_maxlength > FN_maxlength_b)
    /* oint_data_len = intVsize, we still need
       FN_maxlength-FN_maxlength_b = 1 digit. */
    *--destptr = (sintD)FN_sign(obj);
   #endif
    n -= FN_maxlength;
  } else { /* bignum: */
    var uintC len = Bignum_length(obj);
    /* determine pointer: */
    var uintD* ptr = &TheBignum(obj)->data[(uintP)len];
    n -= len;
    destptr = copy_loop_down(ptr,destptr,len); /* copy DS */
  }
  /* fill lower part with fill digits, formed by the sign: */
  if (n!=0)
    destptr = fill_loop_down(destptr,n,sign_of_sintD(destptr[0]));
  /* destptr points to the lower part of the DS. */
  return destptr;
}

/* Logical operations on integers:
 Method: Calculate an upper bound from the lengths of both arguments
 for the length of the result (the maximum of both lengths and
 FN_maxlength), so that the MSD counts for infinitely many bits.
 Then convert both argument into digit sequences of equal size, perform
 operation with a simple loop. */

/* (LOGIOR x y), with x, y being integers.
 result: integer.
 can trigger GC */
local maygc object I_I_logior_I (object x, object y) {
  if (I_fixnump(x) && I_fixnump(y)) { /* both fixnums -> very simple: */
    return as_object /* return bitwise as fixnum */
      (as_oint(x) | as_oint(y));
  } else {
    SAVE_NUM_STACK /* save num_stack */
    var uintC n; /* number of digits */
    {
      var uintC nx = I_to_DS_need(x);
      var uintC ny = I_to_DS_need(y);
      n = (nx>=ny ? nx : ny);
    }
    var uintD* xptr; I_to_DS_n(x,n,xptr=); /* pointer in DS for x */
    var uintD* yptr; I_to_DS_n(y,n,yptr=); /* pointer in DS for y */
    var uintD* zptr = xptr; /* pointer to the result */
    or_loop_up(xptr,yptr,n); /* combine with OR */
    var object result = DS_to_I(zptr,n); /* result as integer */
    RESTORE_NUM_STACK /* restore num_stack */
    return result;
  }
}

/* (LOGXOR x y), with x, y being integers.
 result: Integer.
 can trigger GC */
local maygc object I_I_logxor_I (object x, object y) {
  if (I_fixnump(x) && I_fixnump(y)) { /* both fixnums -> very simple: */
    return as_object /* return bitwise as fixnum */
      ((as_oint(x) ^ as_oint(y)) | ((oint)fixnum_type << oint_type_shift));
  } else {
    SAVE_NUM_STACK /* save num_stack */
    var uintC n; /* number of digits */
    {
      var uintC nx = I_to_DS_need(x);
      var uintC ny = I_to_DS_need(y);
      n = (nx>=ny ? nx : ny);
    }
    var uintD* xptr; I_to_DS_n(x,n,xptr=); /* pointer in DS for x */
    var uintD* yptr; I_to_DS_n(y,n,yptr=); /* pointer in DS for y */
    var uintD* zptr = xptr; /* pointer to result  */
    xor_loop_up(xptr,yptr,n); /* compine with XOR */
    var object result = DS_to_I(zptr,n); /* result as integer */
    RESTORE_NUM_STACK /* restore num_stack */
    return result;
  }
}

/* (LOGAND x y), with x, y being integers.
 result: integer.
 can trigger GC */
local maygc object I_I_logand_I (object x, object y) {
  if (I_fixnump(x) && I_fixnump(y)) { /* both fixnums -> very simple: */
    return as_object /* return bitwise as fixnum */
      (as_oint(x) & as_oint(y));
  } else if (posfixnump(x)) { /* PosFixnum AND Bignum -> PosFixnum */
    var uintD* yLSDptr;
    BN_to_NDS_nocopy(y, ,,yLSDptr=);
    var uintV y_low = get_maxV_Dptr(pFN_maxlength*intDsize,yLSDptr-pFN_maxlength);
    #if (oint_type_len+oint_type_len <= oint_data_shift)
    return as_object(as_oint(x) & as_oint(posfixnum(y_low)));
    #else /* the fixnum_type tag can collide with the high bits of y_low */
    return as_object(as_oint(x) & as_oint(posfixnum(y_low&(vbitm(oint_data_len)-1))));
    #endif
  } else if (posfixnump(y)) { /* Bignum AND PosFixnum -> PosFixnum */
    var uintD* xLSDptr;
    BN_to_NDS_nocopy(x, ,,xLSDptr=);
    var uintV x_low = get_maxV_Dptr(pFN_maxlength*intDsize,xLSDptr-pFN_maxlength);
    #if (oint_type_len+oint_type_len <= oint_data_shift)
    return as_object(as_oint(posfixnum(x_low)) & as_oint(y));
    #else /* the fixnum_type tag can collide with the high bits of x_low */
    return as_object(as_oint(posfixnum(x_low&(vbitm(oint_data_len)-1))) & as_oint(y));
    #endif
  } else {
    SAVE_NUM_STACK /* save num_stack */
    var uintC n; /* number of digits */
    {
      var uintC nx = I_to_DS_need(x);
      var uintC ny = I_to_DS_need(y);
      n = (nx>=ny ? nx : ny);
    }
    var uintD* xptr; I_to_DS_n(x,n,xptr=); /* pointer in DS for x */
    var uintD* yptr; I_to_DS_n(y,n,yptr=); /* pointer in DS for y */
    var uintD* zptr = xptr; /* pointer to the result */
    and_loop_up(xptr,yptr,n); /* combine with AND */
    var object result = DS_to_I(zptr,n); /* result as integer */
    RESTORE_NUM_STACK /* restore num_stack */
    return result;
  }
}

/* (LOGEQV x y), with x, y being integers.
 result: integer.
 can trigger GC */
local maygc object I_I_logeqv_I (object x, object y) {
  if (I_fixnump(x) && I_fixnump(y)) { /* both fixnums -> very simple: */
    return as_object /* return bitwise as fixnum */
      ( ~(as_oint(x) ^ as_oint(y))
        & (((oint)fixnum_type << oint_type_shift) | FN_value_vz_mask));
  } else {
    SAVE_NUM_STACK /* save num_stack */
    var uintC n; /* number of digits */
    {
      var uintC nx = I_to_DS_need(x);
      var uintC ny = I_to_DS_need(y);
      n = (nx>=ny ? nx : ny);
    }
    var uintD* xptr; I_to_DS_n(x,n,xptr=); /* pointer in DS for x */
    var uintD* yptr; I_to_DS_n(y,n,yptr=); /* pointer in DS for y */
    var uintD* zptr = xptr; /* pointer to result */
    eqv_loop_up(xptr,yptr,n); /* combine with NOT XOR */
    var object result = DS_to_I(zptr,n); /* result as integer */
    RESTORE_NUM_STACK /* restore num_stack */
    return result;
  }
}

/* (LOGNAND x y), with x, y being integers.
 result: integer.
 can trigger GC */
local maygc object I_I_lognand_I (object x, object y) {
  if (I_fixnump(x) && I_fixnump(y)) { /* both fixnums -> very simple: */
    return as_object /* return bitwise as fixnum */
      ((as_oint(x) & as_oint(y)) ^ FN_value_vz_mask);
  } else if (posfixnump(x)) {
    /* PosFixnum AND Bignum -> PosFixnum */
    var uintD* yLSDptr;
    BN_to_NDS_nocopy(y, ,,yLSDptr=);
    var uintV y_low = get_maxV_Dptr(pFN_maxlength*intDsize,yLSDptr-pFN_maxlength);
    #if (oint_type_len+oint_type_len <= oint_data_shift)
    return as_object((as_oint(x) & ((oint)y_low << oint_data_shift)) ^ as_oint(Fixnum_minus1));
    #else /* the fixnum_type tag can collide with the high bits of y_low */
    return as_object((as_oint(x) & ((oint)(y_low&(vbitm(oint_data_len)-1)) << oint_data_shift)) ^ as_oint(Fixnum_minus1));
    #endif
  } else if (posfixnump(y)) {
    /* Bignum AND PosFixnum -> PosFixnum */
    var uintD* xLSDptr;
    BN_to_NDS_nocopy(x, ,,xLSDptr=);
    var uintV x_low = get_maxV_Dptr(pFN_maxlength*intDsize,xLSDptr-pFN_maxlength);
    #if (oint_type_len+oint_type_len <= oint_data_shift)
    return as_object((((oint)x_low << oint_data_shift) & as_oint(y)) ^ as_oint(Fixnum_minus1));
    #else /* the fixnum_type tag can collide with the high bits of x_low */
    return as_object((((oint)(x_low&(vbitm(oint_data_len)-1)) << oint_data_shift) & as_oint(y)) ^ as_oint(Fixnum_minus1));
    #endif
  } else {
    SAVE_NUM_STACK /* save num_stack */
    var uintC n; /* number of digits */
    {
      var uintC nx = I_to_DS_need(x);
      var uintC ny = I_to_DS_need(y);
      n = (nx>=ny ? nx : ny);
    }
    var uintD* xptr; I_to_DS_n(x,n,xptr=); /* pointer in DS for x */
    var uintD* yptr; I_to_DS_n(y,n,yptr=); /* pointer in DS for y */
    var uintD* zptr = xptr; /* pointer to result */
    nand_loop_up(xptr,yptr,n); /* combine with NOT AND */
    var object result = DS_to_I(zptr,n); /* result as integer */
    RESTORE_NUM_STACK /* restore num_stack */
    return result;
  }
}

/* (LOGNOR x y), with x, y being integers.
 result: Integer.
 can trigger GC */
local maygc object I_I_lognor_I (object x, object y) {
  if (I_fixnump(x) && I_fixnump(y)) { # both fixnums -> very simple:
    return as_object # return bitwise as fixnum
      ((as_oint(x) | as_oint(y)) ^ FN_value_vz_mask);
  } else {
    SAVE_NUM_STACK # save num_stack
    var uintC n; # number of digits
    {
      var uintC nx = I_to_DS_need(x);
      var uintC ny = I_to_DS_need(y);
      n = (nx>=ny ? nx : ny);
    }
    var uintD* xptr; I_to_DS_n(x,n,xptr=); # pointer in DS for x
    var uintD* yptr; I_to_DS_n(y,n,yptr=); # pointer in DS for y
    var uintD* zptr = xptr; # pointer to result
    nor_loop_up(xptr,yptr,n); # combine with NOT OR
    var object result = DS_to_I(zptr,n); # result as integer
    RESTORE_NUM_STACK # restore num_stack
    return result;
  }
}

/* (LOGANDC2 x y), with x, y being integers.
 result: Integer.
 can trigger GC */
local maygc object I_I_logandc2_I (object x, object y) {
  if (I_fixnump(x) && I_fixnump(y)) { /* both fixnums -> very simple: */
    return as_object /* return bitwise as fixnum */
      ((as_oint(x) & ~as_oint(y)) | ((oint)fixnum_type << oint_type_shift));
  } else if (posfixnump(x)) {
    /* PosFixnum AND Bignum -> PosFixnum */
    var uintD* yLSDptr;
    BN_to_NDS_nocopy(y, ,,yLSDptr=);
    var uintV y_low = get_maxV_Dptr(pFN_maxlength*intDsize,yLSDptr-pFN_maxlength);
    #if (oint_type_len+oint_type_len <= oint_data_shift)
    return as_object(as_oint(x) & ~((oint)y_low << oint_data_shift));
    #else /* the fixnum_type tag can collide with the high bits of y_low */
    return as_object(as_oint(x) & ~((oint)(y_low&(vbitm(oint_data_len)-1)) << oint_data_shift));
    #endif
  } else {
    SAVE_NUM_STACK /* save num_stack */
    var uintC n; /* number of digits */
    {
      var uintC nx = I_to_DS_need(x);
      var uintC ny = I_to_DS_need(y);
      n = (nx>=ny ? nx : ny);
    }
    var uintD* xptr; I_to_DS_n(x,n,xptr=); /* pointer in DS for x */
    var uintD* yptr; I_to_DS_n(y,n,yptr=); /* pointer in DS for y */
    var uintD* zptr = xptr; /* pointer to result */
    andc2_loop_up(xptr,yptr,n); /* combine with AND NOT */
    var object result = DS_to_I(zptr,n); /* result as integer */
    RESTORE_NUM_STACK /* restore num_stack */
    return result;
  }
}

/* (LOGANDC1 x y), with x, y being integers.
 result: Integer.
 can trigger GC */
#if 1 /* macro spares code */
  #define I_I_logandc1_I(x,y)  I_I_logandc2_I(y,x)
#else
local maygc object I_I_logandc1_I (object x, object y) {
  return I_I_logandc2_I(y,x);
}
#endif

/* (LOGORC2 x y), with x, y being integers.
 result: Integer.
 can trigger GC */
local maygc object I_I_logorc2_I (object x, object y) {
  if (I_fixnump(x) && I_fixnump(y)) { /* both fixnums -> very simple: */
    return as_object /* return bitwise as fixnum */
      ((as_oint(x) | ~as_oint(y))
       & (((oint)fixnum_type << oint_type_shift) | FN_value_vz_mask));
  } else {
    SAVE_NUM_STACK /* save num_stack */
    var uintC n; /* number of digits */
    {
      var uintC nx = I_to_DS_need(x);
      var uintC ny = I_to_DS_need(y);
      n = (nx>=ny ? nx : ny);
    }
    var uintD* xptr; I_to_DS_n(x,n,xptr=); /* pointer in DS for x */
    var uintD* yptr; I_to_DS_n(y,n,yptr=); /* pointer in DS for y */
    var uintD* zptr = xptr; /* pointer to result */
    orc2_loop_up(xptr,yptr,n); /* combine with OR NOT */
    var object result = DS_to_I(zptr,n); /* result as integer */
    RESTORE_NUM_STACK /* restore num_stack */
    return result;
  }
}

/* (LOGORC1 x y), with x, y being integers.
 result: Integer.
 can trigger GC */
#if 1 /* macro spares code */
  #define I_I_logorc1_I(x,y)  I_I_logorc2_I(y,x)
#else
local maygc object I_I_logorc1_I (object x, object y) {
  return I_I_logorc2_I(y,x);
}
#endif

/* (LOGNOT x), with x being an integer.
 result: Integer.
 can trigger GC */
local maygc object I_lognot_I (object x) {
  if (I_fixnump(x)) { /* fixnum -> very simple: */
    return as_object /* return bitwise as fixnum */
      (as_oint(x) ^ FN_value_vz_mask);
  } else {
    /* Bignum: */
    SAVE_NUM_STACK /* save num_stack */
    var uintD* MSDptr;
    var uintC n;
    BN_to_NDS(x, MSDptr=,n=,); /* build NDS for x */
    /* n>=bn_minlength, */
    /* and the first intDsize+1 bits are not all equal. */
    not_loop_up(MSDptr,n); /* complement via NOT, */
    /* because of n>0 the sign bit is also flipped
       MSDptr/n/LSDptr is still an NDS, because n>=bn_minlength
       and the first intDsize+1 bits are not all equal. */
    var object result = NDS_to_I(MSDptr,n); /* result as integer */
    RESTORE_NUM_STACK /* restore num_stack */
    return result;
  }
}

/* constants for BOOLE:
 bit-value in 'integer1' + 2 * bit-value in 'integer2' = k
 Fixnum with 4 bits: bit k indicates, what happens with these two bit-values.
        Name             k=0 k=1 k=2 k=3 (bit values: [00] [10] [01] [11]) */
#define boole_clr     0  #  0   0   0   0
#define boole_set    15  #  1   1   1   1
#define boole_1      10  #  0   1   0   1
#define boole_2      12  #  0   0   1   1
#define boole_c1      5  #  1   0   1   0
#define boole_c2      3  #  1   1   0   0
#define boole_and     8  #  0   0   0   1
#define boole_ior    14  #  0   1   1   1
#define boole_xor     6  #  0   1   1   0
#define boole_eqv     9  #  1   0   0   1
#define boole_nand    7  #  1   1   1   0
#define boole_nor     1  #  1   0   0   0
#define boole_andc1   4  #  0   0   1   0
#define boole_andc2   2  #  0   1   0   0
#define boole_orc1   13  #  1   0   1   1
#define boole_orc2   11  #  1   1   0   1

/* (BOOLE op x y), with x and y being integers and op an object.
 result: Integer.
 OP_I_I_boole_I(op,x,y)
 can trigger GC */
local maygc object OP_I_I_boole_I (object op, object x, object y) {
  switch (as_oint(op) ^ as_oint(Fixnum_0)) {
    case (oint)( boole_clr )<<oint_data_shift:
      return Fixnum_0;
    case (oint)( boole_set )<<oint_data_shift:
      return Fixnum_minus1;
    case (oint)( boole_1 )<<oint_data_shift:
      return x;
    case (oint)( boole_2 )<<oint_data_shift:
      return y;
    case (oint)( boole_c1 )<<oint_data_shift:
      return I_lognot_I(x);
    case (oint)( boole_c2 )<<oint_data_shift:
      return I_lognot_I(y);
    case (oint)( boole_and )<<oint_data_shift:
      return I_I_logand_I(x,y);
    case (oint)( boole_ior )<<oint_data_shift:
      return I_I_logior_I(x,y);
    case (oint)( boole_xor )<<oint_data_shift:
      return I_I_logxor_I(x,y);
    case (oint)( boole_eqv )<<oint_data_shift:
      return I_I_logeqv_I(x,y);
    case (oint)( boole_nand )<<oint_data_shift:
      return I_I_lognand_I(x,y);
    case (oint)( boole_nor )<<oint_data_shift:
      return I_I_lognor_I(x,y);
    case (oint)( boole_andc1 )<<oint_data_shift:
      return I_I_logandc1_I(x,y);
    case (oint)( boole_andc2 )<<oint_data_shift:
      return I_I_logandc2_I(x,y);
    case (oint)( boole_orc1 )<<oint_data_shift:
      return I_I_logorc1_I(x,y);
    case (oint)( boole_orc2 )<<oint_data_shift:
      return I_I_logorc2_I(x,y);
    default: /* wrong operator */
      pushSTACK(op); /* TYPE-ERROR slot DATUM */
      pushSTACK(O(type_boole)); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(op); pushSTACK(S(boole));
      fehler(type_error,GETTEXT("~S: ~S is not a valid boolean operation"));
  }
}

/* Tests, if (LOGTEST x y), with x and y being integers.
 (LOGTEST x y) = (NOT (ZEROP (LOGAND x y))).
 I_I_logtest(x,y)
 < result: /=0, if yes; =0, if no. */
local bool I_I_logtest (object x, object y) {
/* method:
  treat fixnums separately.
  Without loss of generality let x be the shorter of the two numbers
   (in digits).
  x is shorter and x<0 ->
    [one of the most signif. intDsize+1 bits if y is 1] yes.
  both have equal length or x>=0 ->
   We can confine it to the lowest length(x) digits.
   traverse with AND , abort (with "Yes") if /=0. At the end: No. */
  if (I_fixnump(x)) {
    if (I_fixnump(y)) { /* both fixnums */
      if ((as_oint(x) & as_oint(y) & FN_value_vz_mask)==0)
        return false;
      else
        return true;
    } else { /* x fixnum, y bignum, so x is truly shorter */
     xFN_yBN:
      if (R_minusp(x))
        return true; /* x<0 -> yes. */
      /* x>=0. combine x with the pFN_maxlength last digits of y. */
      var uintD* yLSDptr;
      var uintV x_ = posfixnum_to_V(x);
      BN_to_NDS_nocopy(y, _EMA_,_EMA_,yLSDptr=);
     #if (pFN_maxlength > 1)
      doconsttimes(pFN_maxlength-1,
                   if (*--yLSDptr & (uintD)x_)
                     return true;
                   x_ = x_ >> intDsize;);
     #endif
      if (*--yLSDptr & (uintD)x_)
        return true;
      return false;
    }
  } else {
    if (I_fixnump(y)) { /* x bignum, y fixnum */
      {var object h = x; x = y; y = h; } /* swap x and y */
      goto xFN_yBN; /* and continue above */
    } else { /* x,y bignums */
      var uintD* xMSDptr;
      var uintC xlen;
      var uintD* yMSDptr;
      var uintC ylen;
      BN_to_NDS_nocopy(x, xMSDptr=,xlen=,);
      BN_to_NDS_nocopy(y, yMSDptr=,ylen=,);
      /* notice: xlen>0, ylen>0. */
      if (!(xlen==ylen)) {
        /* both have different length */
        if (xlen<ylen) { /* x is the truly shorter DS. */
          if ((sintD)xMSDptr[0]<0) /* the truly shorter is negative? */
            return true;
          /* the truly shorter is positive. */
          yMSDptr += ylen-xlen;
        } else { /* y is the truly shorter DS. */
          if ((sintD)yMSDptr[0]<0) /* the truly shorter is negative? */
            return true;
          /* the truly shorter is positive. */
          xMSDptr += xlen-ylen;
          xlen = ylen;
        }
      }
      /* search for common bits in xMSDptr/xlen/..  and yMSDptr/xlen/..: */
      return and_test_loop_up(xMSDptr,yMSDptr,xlen);
    }
  }
}

/* Tests, if (LOGBITP x y), with x and y being integers.
 I_I_logbitp(x,y)
 result: /=0, if yes; =0, if no. */
local bool I_I_logbitp (object x, object y)
/* method:
 if x<0, Error.
 if x>=0: if x>=intDsize*length(y), test sign of y.
          else x=intDsize*k+i,
            test bit i starting at word nr. k+1 (downwards). */
{
  if (!R_minusp(x)) { /* x>=0 ? */
    if (I_fixnump(x)) {
      var uintV x_ = posfixnum_to_V(x);
      var uintC ylen;
      var uintD* yLSDptr;
      I_to_NDS_nocopy(y, _EMA_,ylen=,yLSDptr=); /* DS for y */
      if (x_ < intDsize*(uintL)ylen) {
        /* x is a fixnum >=0, < intDsize*ylen */
        if (yLSDptr[-(uintP)floor(x_,intDsize)-1] & bit(x_%intDsize))
          return true;
        else
          return false;
      }
    }
    /* test sign of y */
    if (R_minusp(y))
      return true;
    else
      return false;
  } else {
    /* x<0 */
    pushSTACK(x); /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_posinteger)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(x); pushSTACK(S(logbitp));
    fehler(type_error,GETTEXT("~S: index ~S is negative"));
  }
}

/* Tests, if (ODDP x), with x being an integer.
 I_oddp(x)
 result: /=0, if yes; =0, if no. */
local bool I_oddp (object x) {
  if (I_fixnump(x)) { /* fixnum: check bit 0 */
    if (as_oint(x) & wbit(0+oint_data_shift))
      return true;
    else
      return false;
  } else { /* bignum: check bit 0 in the last */
    var Bignum x_ = TheBignum(x);
    if (x_->data[(uintP)bignum_length(x_)-1] & bit(0))
      return true;
    else
      return false;
  }
}

/* (ASH x y), with x and y being integers. result: Integer.
 I_I_ash_I(x,y)
 can trigger GC */
global maygc object I_I_ash_I (object x, object y)
/* method:
 x = 0 -> 0 as result
 y = 0 -> x as result
 y > 0 -> y = intDsize*k + i, j=k+(1 if i>0, 0 if i=0).
          reserve j more words, k nullwords, then carry,
          for i>0: shift left by i bits (i=1 is easier).
 y < 0 -> y <= - intDsize * (length(A0) in Digits) -> result = 0 or -1.
          else: -y = intDsize*k + i with k<length(A0).
                  carry the (length(A0)-k) MSDigits,
                  if i>0: shift them right by i bits (i=1 is easier). */
{
  if (eq(x,Fixnum_0))
    return x; /* x=0 -> 0 as result */
  else if (eq(y,Fixnum_0))
    return x; /* y=0 -> x as result */
  else {
    SAVE_NUM_STACK /* save num_stack */
    if (!R_minusp(y)) { /* y>0 */
      if (I_bignump(y) /* y a bignum */
          || ((log2_intDsize+intWCsize < oint_data_len) /* intDsize*2^intWCsize < 2^oint_data_len ? */
              && (as_oint(y) >= as_oint(fixnum(intDsize*vbitm(intWCsize)))))) { /* a fixnum > bitlength of all integers */
        /* y so large, that even (ASH 1 y) would cause an overflow. */
        goto badamount;
      } else {
        var uintV y_ = (as_oint(y)-as_oint(Fixnum_0))>>oint_data_shift; /* value of y, >=0, <intDsize*2^intWCsize */
        var uintL i = y_%intDsize; /* i = y mod intDsize, >=0, <intDsize */
        var uintV k = floor(y_,intDsize); /* k = y div intDsize, >=0, <2^intWCsize */
        var uintD* LSDptr;
        var uintC len;
        var uintD* x_LSDptr;
        I_to_NDS_nocopy(x, _EMA_,len=,x_LSDptr=); /* build DS for x. */
        if (len >= (uintWC)(~(uintWC)k)) /* can len+k+1 cause an overflow? */
          goto badamount; /* yes -> error */
        num_stack_need_1(len+(uintC)k,_EMA_,LSDptr=);
        LSDptr = clear_loop_down(LSDptr,k); /* k Nulldigits */
        var uintD* MSDptr = copy_loop_down(x_LSDptr,LSDptr,len);
        /* Now, MSDptr/len/LSDptr is the DS for x.
           Above it are k nulldigits, below is room for 1 digit.
           MSDptr/len+k/.. is now the total-DS.
           shift left by i bits: */
        if (!(i==0)) { /* if i>0 */
          { /* add a further Digit (sign) */
            var uintD sign = sign_of_sintD(MSDptr[0]);
            *--MSDptr = sign;
            len++;
          }
          /* shift loop: shift the lower len digits by i bits */
          begin_arith_call();
          if (i==1) {
            shift1left_loop_down(LSDptr,len);
          } else {
            shiftleft_loop_down(LSDptr,len,i,0);
          }
          end_arith_call();
        }
        x = DS_to_I(MSDptr,len+(uintC)k);
      }
    } else { /* y<0 */
      if (I_bignump(y)) {
        goto sign; /* y a bignum -> return sign of x */
      } else {
        var uintV y_ = ((as_oint(Fixnum_minus1)-as_oint(y))>>oint_data_shift)+1; /* value of -y, >0 */
        #if (oint_data_len==intVsize)
        if (y_==0) goto sign; /* y = most-negative-fixnum -> return sign of x */
        #endif
        var uintL i = y_%intDsize; /* i = (-y) mod intDsize, >=0, <intDsize */
        var uintV k = floor(y_,intDsize); /* k = (-y) div intDsize, >=0 */
        /* build DS for x: */
        var uintD* MSDptr;
        var uintC len;
        I_to_NDS(x, MSDptr=,len=,); /* build DS for x. */
        if (k>=len)
          goto sign; /* -y >= intDsize*len -> restore sign of x */
        len -= k; /* simply discard the right k digits */
        /* still len>0. Shift right by i bits: */
        if (!(i==0)) { /* if i>0: */
          /* shift len digits at MSDptr by i bits to the right: */
          begin_arith_call();
          if (i==1) {
            shift1right_loop_up(MSDptr,len,sign_of_sintD(MSDptr[0]));
          } else {
            shiftrightsigned_loop_up(MSDptr,len,i);
          }
          end_arith_call();
        }
        x = DS_to_I(MSDptr,len);
      }
    }
    if (false)
    sign: /* result is 0, if x>=0, and -1, if x<0: */
      x = (R_minusp(x) ? Fixnum_minus1 : Fixnum_0 );
    RESTORE_NUM_STACK /* restore num_stack */
      return x;
   badamount:
    RESTORE_NUM_STACK /* restore num_stack */
      pushSTACK(y); pushSTACK(S(ash));
    fehler(arithmetic_error,GETTEXT("~S: too large shift amount ~S"));
  }
}

/* (LOGCOUNT x), with x being an integer. result: Integer >=0.
 I_logcount_I(x)
 can trigger GC */
local maygc object I_logcount_I (object x);
/* count bits of x8: (input x8, output x8) */
#define logcount_8()                                            \
    (                                                           \
      /* x8 consists of 8 1-bit-counters (0,1). */              \
      x8 = (x8 & 0x55U) + ((x8 & 0xAAU) >> 1),                  \
      /* x8 consists of 4 2-bit-counters (0,1,2). */            \
      x8 = (x8 & 0x33U) + ((x8 & 0xCCU) >> 2),                  \
      /* x8 consists of 2 4-bit-counters (0,1,2,3,4). */        \
      x8 = (x8 & 0x0FU) + (x8 >> 4)                             \
      /* x8 consists of 1 8-bit-counter (0,...,8). */           \
    )
/* count bits of x16: (input x16, output x16) */
#define logcount_16()                                           \
    (                                                           \
      /* x16 consists of 16 1-bit-counters (0,1). */            \
      x16 = (x16 & 0x5555U) + ((x16 & 0xAAAAU) >> 1),           \
      /* x16 consists of 8 2-bit-counters (0,1,2). */           \
      x16 = (x16 & 0x3333U) + ((x16 & 0xCCCCU) >> 2),           \
      /* x16 consists of 4 4-bit-counters (0,1,2,3,4). */       \
      x16 = (x16 & 0x0F0FU) + ((x16 & 0xF0F0U) >> 4),           \
      /* x16 consists of 2 8-bit-counters (0,...,8). */         \
      x16 = (x16 & 0x00FFU) + (x16 >> 8)                        \
      /* x16 consists of 1 16-bit-counter (0,...,16). */        \
    )
/* count bits of x32: (input x32, output x16) */
#define logcount_32()                                           \
    (                                                           \
      /* x32 consists of 32 1-bit-counters (0,1). */            \
      x32 = (x32 & 0x55555555UL) + ((x32 & 0xAAAAAAAAUL) >> 1), \
      /* x32 consists of 16 2-bit-counters (0,1,2). */          \
      x32 = (x32 & 0x33333333UL) + ((x32 & 0xCCCCCCCCUL) >> 2), \
      /* x32 consists of 8 4-bit-counters (0,1,2,3,4). */       \
      x16 = high16(x32)+low16(x32),                             \
      /* x16 consists of 4 4-bit-counters (0,...,8). */         \
      x16 = (x16 & 0x0F0FU) + ((x16 & 0xF0F0U) >> 4),           \
      /* x16 consists of 2 8-bit-counters (0,...,16). */        \
      x16 = (x16 & 0x00FFU) + (x16 >> 8)                        \
      /* x16 consists of 1 16-bit-counter (0,...,32). */        \
    )
/* count bits of x32: (input x32, output x16) */
#define logcount_64()                                           \
    (                                                           \
      /* x32 consists of 64 1-bit-counters (0,1). */            \
      x32 = (x32 & ULL(0x5555555555555555)) + ((x32 & ULL(0xAAAAAAAAAAAAAAAA)) >> 1), \
      /* x64 consists of 32 2-bit-counters (0,1,2). */          \
      x32 = (x32 & ULL(0x3333333333333333)) + ((x32 & ULL(0xCCCCCCCCCCCCCCCC)) >> 2), \
      /* x32 consists of 16 4-bit-counters (0,1,2,3,4). */      \
      x32 = high32(x32)+low32(x32),                             \
      /* x32 consists of 8 4-bit-counters (0,...,8). */         \
      x32 = (x32 & 0x0F0F0F0FUL) + ((x32 & 0xF0F0F0F0UL) >> 4), \
      /* x32 consists of 4 8-bit-counters (0,...,16). */        \
      x32 = (x32 & 0x00FF00FFUL) + ((x32 & 0xFF00FF00UL) >> 8), \
      /* x32 consists of 2 16-bit-counter (0,...,32). */        \
      x16 = (x32 & 0x0000FFFFUL) + (x32 >> 16)                  \
      /* x16 consists of 1 16-bit-counter (0,...,64). */        \
    )
#if (intWLsize==intLsize)
  #define x16  x32
#endif
local maygc object I_logcount_I (object x)
{
  if (I_fixnump(x)) {
    var uint16 x16; /* auxiliary variable */
   {var uintV x32 = FN_to_V(x); /* x as intVsize-bit-number */
    if (FN_V_minusp(x,(sintV)x32))
      x32 = ~ x32; /* if <0, make 1-complement */
   #if (intVsize>32)
    logcount_64(); /* count bits of x32 */
   #else
    logcount_32(); /* count bits of x32 */
   #endif
    return fixnum((uintL)x16);
  }} else {
    var uintD* MSDptr;
    var uintC len;
    BN_to_NDS_nocopy(x, MSDptr=,len=,); /* buil DS for x, len>0. */
    var uintL bitcount = 0; /* bitcounter */
    var uintD* ptr = MSDptr; /* traverses the digits */
    var uintD sign = sign_of_sintD(ptr[0]); /* sign */
   #if (intDsize==8)
    dotimespC(len,len, {
      var uintD x8 = (*ptr++) ^ sign; /* next intDsize-bit-package, */
      /* negative numbers are complemented */
      /* count bits of x8, increase total counter: */
      bitcount += (uintL)(logcount_8(), x8);
    });
   #endif
   #if (intDsize==16)
    dotimespC(len,len, {
      var uintD x16 = (*ptr++) ^ sign; /* next intDsize-bit-package, */
      /* negative numbers are complemented */
      /* count bits of x16, increase total counter: */
      bitcount += (uintL)(logcount_16(), x16);
    });
   #endif
   #if (intDsize==32)
    dotimespC(len,len, {
      var uint16 x16; /* auxiliary variable */
     {var uintD x32 = (*ptr++) ^ sign; /* next intDsize-bit-package, */
      /* negative numbers are complemented */
      /* count bits of x32, increase total counter: */
      bitcount += (uintL)(logcount_32(), x16);
    }});
   #endif
    /* 0 <= bitcount < intDsize*2^intWCsize, fits poss. into a fixnum. */
    if (log2_intDsize+intWCsize<=oint_data_len) /* intDsize*2^intWCsize <= 2^oint_data_len ? */
      return fixnum(bitcount);
    else
      return UL_to_I(bitcount);
  }
}
#undef x16
#undef logcount_32
#undef logcount_16
#undef logcount_8

/* count bits of a digit:
 integerlengthD(digit,size=);
 sets size to the highest bit number occurring in digit.
 > digit: a uintD >0
 < size: >0, <=intDsize, with 2^(size-1) <= digit < 2^size */
#if defined(GNU) && defined(MC680Y0) && !defined(NO_ASM)
  #define integerlength8(digit,size_zuweisung)                          \
    {                                                                   \
      var uintL zero_counter; /* counts the leading nullbits in digit */ \
      __asm__("bfffo %1{#0:#8},%0" : "=d" (zero_counter) : "dm" ((uint8)(digit)) ); \
      size_zuweisung (8-zero_counter);                                  \
    }
#elif defined(SPARC) && !defined(SPARC64)
  #define integerlength8(digit,size_zuweisung)  \
    integerlength32((uint32)(digit),size_zuweisung) /* see below */
#elif defined(GNU) && defined(I80386) && !defined(NO_ASM)
  #define integerlength8(digit,size_zuweisung)  \
    integerlength16((uint16)(digit),size_zuweisung)
#else
  #define integerlength8(digit,size_zuweisung)                  \
    {                                                           \
      var uintC bitsize = 1;                                    \
      var uintBWL x8 = (uint8)(digit);                          \
      /* x8 has at most 8 bits. */                              \
      if (x8 >= bit(4)) { x8 = x8>>4; bitsize += 4; }           \
      /* x8 has at most 4 bits. */                              \
      if (x8 >= bit(2)) { x8 = x8>>2; bitsize += 2; }           \
      /* x8 has at most 2 bits. */                              \
      if (x8 >= bit(1)) { /* x8 = x8>>1; */ bitsize += 1; }     \
      /* x8 has at most 1 bit. This bit must be set. */         \
      size_zuweisung bitsize;                                   \
    }
#endif
#if defined(GNU) && defined(MC680Y0) && !defined(NO_ASM)
  #define integerlength16(digit,size_zuweisung)                         \
    {                                                                   \
      var uintL zero_counter; /* counts the leading nullbits in digit */ \
      __asm__("bfffo %1{#0:#16},%0" : "=d" (zero_counter) : "dm" ((uint16)(digit)) ); \
      size_zuweisung (16-zero_counter);                                 \
    }
#elif defined(SPARC) && !defined(SPARC64)
  #define integerlength16(digit,size_zuweisung)  \
    integerlength32((uint32)(digit),size_zuweisung) /* see below */
#elif (defined(GNU) || defined(INTEL)) && defined(I80386) && !defined(NO_ASM)
  #define integerlength16(digit,size_zuweisung)                         \
    {                                                                   \
      var uintW one_position; /* position of the leading 1 */           \
      __asm__("bsrw %1,%0" : "=r" (one_position) : "r" ((uint16)(digit)) ); \
      size_zuweisung (1+one_position);                                  \
    }
/* The others are in gcc/longlong.h : */
#elif defined(GNU) && defined(__ibm032__) && !defined(NO_ASM) /* RT/ROMP */
  #define integerlength16(digit,size_zuweisung)                         \
    {                                                                   \
      var uintL zero_counter; /* counts the leading nullbits in digit */ \
      __asm__("clz %0,%1" : "=r" (zero_counter) : "r" ((uint32)(digit)) ); \
      size_zuweisung (16-zero_counter);                                 \
    }
#else
  #define integerlength16(digit,size_zuweisung)                 \
    {                                                           \
      var uintC bitsize = 1;                                    \
      var uintWL x16 = (uint16)(digit);                         \
      /* x16 has at most 16 bits. */                            \
      if (x16 >= bit(8)) { x16 = x16>>8; bitsize += 8; }        \
      /* x16 has at most 8 bits. */                             \
      if (x16 >= bit(4)) { x16 = x16>>4; bitsize += 4; }        \
      /* x16 has at most 4 bits. */                             \
      if (x16 >= bit(2)) { x16 = x16>>2; bitsize += 2; }        \
      /* x16 has at most 2 bits. */                             \
      if (x16 >= bit(1)) { /* x16 = x16>>1; */ bitsize += 1; }  \
      /* x16 has at most 1 bit. This bit must be set. */        \
      size_zuweisung bitsize;                                   \
    }
#endif
#if defined(GNU) && defined(MC680Y0) && !defined(NO_ASM)
  #define integerlength32(digit,size_zuweisung)  \
    {                                                                                 \
      var uintL zero_counter; /* counts the leading nullbits in digit */ \
      __asm__("bfffo %1{#0:#32},%0" : "=d" (zero_counter) : "dm" ((uint32)(digit)) ); \
      size_zuweisung (32-zero_counter);                                               \
    }
#elif defined(SPARC) && !defined(SPARC64) && defined(FAST_DOUBLE)
  #define integerlength32(digit,size_zuweisung)                         \
    {                                                                   \
      var union { double f; uint32 i[2]; } __fi;                        \
      /* form 2^52 + digit: */                                          \
      __fi.i[0] = (uint32)(DF_mant_len+1+DF_exp_mid) << (DF_mant_len-32); /* sign 0, exponent 53 */ \
      __fi.i[1] = (digit); /* set the lower 32 bits (uses BIG_ENDIAN_P !) */ \
      /* subtract 2^52: */                                              \
      __fi.f = __fi.f - (double)(4503599627370496.0L);                  \
      /* fetch the exponent: */                                         \
      size_zuweisung ((__fi.i[0] >> (DF_mant_len-32)) - DF_exp_mid);    \
    }
#elif (defined(GNU) || defined(INTEL)) && defined(I80386) && !defined(NO_ASM)
  #define integerlength32(digit,size_zuweisung)                         \
    {                                                                   \
      var uintL one_position; /* position of the leading 1 */           \
      __asm__("bsrl %1,%0" : "=r" (one_position) : "rm" ((uint32)(digit)) ); \
      size_zuweisung (1+one_position);                                  \
    }
#elif defined(HPPA) && !defined(NO_ASM)
  #define integerlength32(digit,size_zuweisung)  \
    size_zuweisung length32(digit);
  extern_C uintL length32 (uintL digit); /* extern in assembler */
/* The others are in gcc/longlong.h : */
#elif defined(GNU) && (defined(__a29k__) || defined(___AM29K__)) && !defined(NO_ASM)
  #define integerlength32(digit,size_zuweisung)                         \
    {                                                                   \
      var uintL zero_counter; /* counts the leading nullbits in digit */ \
      __asm__("clz %0,%1" : "=r" (zero_counter) : "r" ((uint32)(digit)) ); \
      size_zuweisung (32-zero_counter);                                 \
    }
#elif defined(GNU) && defined(__gmicro__) && !defined(NO_ASM)
  #define integerlength32(digit,size_zuweisung)                         \
    {                                                                   \
      var uintL zero_counter; /* counts the leading nullbits in digit */ \
      __asm__("bsch/1 %1,%0" : "=g" (zero_counter) : "g" ((uint32)(digit)) ); \
      size_zuweisung (32-zero_counter);                                 \
    }
#elif defined(GNU) && defined(RS6000) && !defined(NO_ASM)
 #ifdef _AIX
  /* old assembler syntax */
  #define integerlength32(digit,size_zuweisung)                         \
    {                                                                   \
      var uintL zero_counter; /* counts the leading nullbits in digit */ \
      __asm__("cntlz %0,%1" : "=r" (zero_counter) : "r" ((uint32)(digit)) ); \
      size_zuweisung (32-zero_counter);                                 \
    }
 #else
  /* new assembler syntax */
  #define integerlength32(digit,size_zuweisung)                         \
    {                                                                   \
      var uintL zero_counter; /* counts the leading nullbits in digit */ \
      __asm__("cntlzw %0,%1" : "=r" (zero_counter) : "r" ((uint32)(digit)) ); \
      size_zuweisung (32-zero_counter);                                 \
    }
 #endif
#elif defined(GNU) && defined(M88000) && !defined(NO_ASM)
  #define integerlength32(digit,size_zuweisung)                         \
    {                                                                   \
      var uintL one_position; /* position of the leading 1 */           \
      __asm__("ff1 %0,%1" : "=r" (one_position) : "r" ((uint32)(digit)) ); \
      size_zuweisung (1+one_position);                                  \
    }
#elif defined(GNU) && defined(__ibm032__) && !defined(NO_ASM) /* RT/ROMP */
  #define integerlength32(digit,size_zuweisung)         \
    {                                                   \
      var uintL x32 = (uint32)(digit);                  \
      if (x32 >= bit(16)) {                             \
        integerlength16(x32>>16,size_zuweisung 16 + );  \
      } else {                                          \
        integerlength16(x32,size_zuweisung);            \
      }                                                 \
    }
#else
  #if (intWLsize==intLsize)
    #define integerlength32(digit,size_zuweisung)                       \
      {                                                                 \
        var uintC bitsize = 1;                                          \
        var uintL x32 = (uint32)(digit);                                \
        /* x32 has at most 32 bits. */                                  \
        if (x32 >= bit(16)) { x32 = x32>>16; bitsize += 16; }           \
        /* x32 has at most 16 bits. */                                  \
        if (x32 >= bit(8)) { x32 = x32>>8; bitsize += 8; }              \
        /* x32 has at most 8 bits. */                                   \
        if (x32 >= bit(4)) { x32 = x32>>4; bitsize += 4; }              \
        /* x32 has at most 4 bits. */                                   \
        if (x32 >= bit(2)) { x32 = x32>>2; bitsize += 2; }              \
        /* x32 has at most 2 bits. */                                   \
        if (x32 >= bit(1)) { /* x32 = x32>>1; */ bitsize += 1; }        \
        /* x32 has at most 1 bit. This bit must be set. */              \
        size_zuweisung bitsize;                                         \
      }
  #else
    #define integerlength32(digit,size_zuweisung)                       \
      {                                                                 \
        var uintC bitsize = 1;                                          \
        var uintL x32 = (digit);                                        \
        var uintWL x16;                                                 \
        /* x32 has at most 32 bits. */                                  \
        if (x32 >= bit(16)) { x16 = x32>>16; bitsize += 16; } else { x16 = x32; } \
        /* x16 has at most 16 bits. */                                  \
        if (x16 >= bit(8)) { x16 = x16>>8; bitsize += 8; }              \
        /* x16 has at most 8 bits. */                                   \
        if (x16 >= bit(4)) { x16 = x16>>4; bitsize += 4; }              \
        /* x16 has at most 4 bits. */                                   \
        if (x16 >= bit(2)) { x16 = x16>>2; bitsize += 2; }              \
        /* x16 has at most 2 bits. */                                   \
        if (x16 >= bit(1)) { /* x16 = x16>>1; */ bitsize += 1; }        \
        /* x16 has at most 1 bit. This bit must be set. */              \
        size_zuweisung bitsize;                                         \
      }
  #endif
#endif
#define integerlength64(digit,size_zuweisung)                               \
  {                                                                         \
    var uint64 x64 = (digit);                                               \
    var uintC bitsize64 = 0;                                                \
    var uint32 x32_from_integerlength64;                                    \
    if (x64 >= ((uint64)1 << 32)) {                                         \
      x32_from_integerlength64 = x64>>32; bitsize64 += 32;                  \
    } else {                                                                \
      x32_from_integerlength64 = x64;                                       \
    }                                                                       \
    integerlength32(x32_from_integerlength64, size_zuweisung bitsize64 + ); \
  }
#if (intDsize==8)
  #define integerlengthD  integerlength8
#endif
#if (intDsize==16)
  #define integerlengthD  integerlength16
#endif
#if (intDsize==32)
  #define integerlengthD  integerlength32
#endif

/* (INTEGER-LENGTH x), with x being an integer. Result uintL.
 I_integer_length(x) */
global uintL I_integer_length (object x) {
  if (I_fixnump(x)) {
    var uintL bitcount = 0;
    var uintV x_ = FN_to_V(x); /* x as intVsize-bit-number */
    if (FN_V_minusp(x,(sintV)x_))
      x_ = ~ x_; /* if <0, make 1-complement */
    if (!(x_==0)) {
     #if (intVsize>32)
      integerlength64(x_,bitcount=);
     #else
      integerlength32(x_,bitcount=);
     #endif
    }
    return bitcount; /* 0 <= bitcount < intVsize. */
  } else {
    var uintD* MSDptr;
    var uintC len;
    BN_to_NDS_nocopy(x, MSDptr=,len=,); /* form normalized DS for x. */
    var uintL bitcount = intDsize*(uintL)(len-1); /* number of digits times intDsize */
    /* take MSDigit, test, which one is the highest bit, that is different
       from the sign bit: */
    var uintD msd = MSDptr[0]; /* MSDigit */
    if ((sintD)msd < 0)
      msd = ~msd; /* if negative, invert */
    /* search the position of the highest bit in msd and increase
       bit_count accordingly (by at most intDsize-1): */
    if (!(msd == 0)) {
      integerlengthD(msd, bitcount += );
    }
    return bitcount; /* 0 <= bitcount < intDsize*2^intWCsize. */
  }
}

/* (INTEGER-LENGTH x), with x being an integer. result: Integer >=0.
 I_integer_length_I(x)
 can trigger GC */
local maygc object I_integer_length_I (object x) {
  if (I_fixnump(x)) {
    var uintL bitcount = 0;
    var uintV x_ = FN_to_V(x); /* x as intVsize-bit-number */
    if (FN_V_minusp(x,(sintV)x_))
      x_ = ~ x_; /* if <0, make 1-complement */
    if (!(x_==0)) {
     #if (intVsize>32)
      integerlength64(x_,bitcount=);
     #else
      integerlength32(x_,bitcount=);
     #endif
    }
    /* 0 <= bitcount < intVsize, fits in a fixnum. */
    return fixnum(bitcount);
  } else {
    var uintD* MSDptr;
    var uintC len;
    BN_to_NDS_nocopy(x, MSDptr=,len=,); /* form normalized DS for x. */
    var uintL bitcount = intDsize*(uintL)(len-1); /* number of digits times intDsize */
    /* take MSDigit, test, which one is the highest bit, that is different
       from the sign bit: */
    var uintD msd = MSDptr[0]; /* MSDigit */
    if ((sintD)msd < 0)
      msd = ~msd; /* if negative, invert */
    /* search the position of the highest bit in msd and increase
       bit_count accordingly (by at most intDsize-1): */
    if (!(msd == 0)) {
      integerlengthD(msd, bitcount += );
    }
    /* 0 <= bitcount < intDsize*2^intWCsize, fits poss. into a fixnum. */
    if (log2_intDsize+intWCsize<=oint_data_len) /* intDsize*2^intWCsize <= 2^oint_data_len ? */
      return fixnum(bitcount);
    else
      return UL_to_I(bitcount);
  }
}

/* count rear nullbits of a 32-bit-word:
 ord2_32(digit,count=);
 sets size to the smallest bit number occurring in digit.
 > digit: a uint32 >0
 < count: >=0, <32, with 2^count | digit, digit/2^count being odd */
#if (defined(GNU) || defined(INTEL)) && defined(I80386) && !defined(NO_ASM)
  #define ord2_32(digit,count_zuweisung)                                \
    {                                                                   \
      var uintL one_position; /* position of the last 1 */              \
      __asm__("bsfl %1,%0" : "=r" (one_position) : "rm" ((uint32)(digit)) ); \
      count_zuweisung one_position;                                     \
    }
#elif defined(SPARC) && !defined(SPARC64)
  #define ord2_32(digit,count_zuweisung)                                \
    {                                                                   \
      /* static const char ord2_tab [64] = {-1,0,1,12,2,6,-1,13,3,-1,7,-1,-1,-1,-1,14,10,4,-1,-1,8,-1,-1,25,-1,-1,-1,-1,-1,21,27,15,31,11,5,-1,-1,-1,-1,-1,9,-1,-1,24,-1,-1,20,26,30,-1,-1,-1,-1,23,-1,19,29,-1,22,18,28,17,16,-1}; */ \
      var uint32 n = (digit);                                           \
      n = n | -n;                                                       \
      n = (n<<4) + n;                                                   \
      n = (n<<6) + n;                                                   \
      n = n - (n<<16); /* or  n = n ^ (n<<16);  or  n = n &~ (n<<16); */ \
      /* count_zuweisung ord2_tab[n>>26]; */                            \
      count_zuweisung "\377\000\001\014\002\006\377\015\003\377\007\377\377\377\377\016\012\004\377\377\010\377\377\031\377\377\377\377\377\025\033\017\037\013\005\377\377\377\377\377\011\377\377\030\377\377\024\032\036\377\377\377\377\027\377\023\035\377\026\022\034\021\020"[n>>26]; \
    }
#endif

/* count rear nullbits of a digit:
 ord2_D(digit,count=);
 sets size to the smallest bit number occurring in digit.
 > digit: a uintD >0
 < count: >=0, <intDsize, with 2^count | digit, digit/2^count being odd */
#ifdef ord2_32
  #define ord2_D(digit,count_zuweisung)           \
    ord2_32((uint32)(digit),count_zuweisung)
#endif

/* (ORD2 x) = max{n>=0: 2^n | x }, with x being an integer /=0 . result uintL.
 I_ord2(x)
 method 1a:
   let n = ord2(x). Then, logxor(x,x-1) = 2^n + (2^n-1) = 2^(n+1)-1.
   so  (ord2 x) = (1- (integer-length (logxor x (1- x)))) .
 method 1b:
   let n = ord2(x). Then, logand(x,-x) = 2^n.
   so  (ord2 x) = (1- (integer-length (logand x (- x)))) .
 method 1c:
   let n = ord2(x). Then, lognot(logior(x,-x)) = 2^n-1.
   so  (ord2 x) = (integer-length (lognot (logior x (- x)))) .
 method 2:
   count nullbits at the end of x:
   (ord2 x) = intDsize * number of nulldigits at the end
              + number of nullbits at the end of the last digit /=0. */
local uintL I_ord2 (object x);
#ifndef ord2_32
/* Here, digit must be a variable. Digit is modified! */
  #define ord2_32(digit,count_zuweisung)                \
    digit = digit ^ (digit - 1); /* method 1a */        \
    integerlength32(digit,count_zuweisung -1 + )
#endif
#ifndef ord2_64
/* Here, digit must be a variable. Digit is modified! */
  #define ord2_64(digit,count_zuweisung)                \
    digit = digit ^ (digit - 1); /* method 1a */        \
    integerlength64(digit,count_zuweisung -1 + )
#endif
#ifndef ord2_D
/* Here, digit must be a variable. Digit is modified! */
  #define ord2_D(digit,count_zuweisung)                 \
    digit = digit ^ (digit - 1); /* method 1a */        \
    integerlengthD(digit,count_zuweisung -1 + )
#endif
local uintL I_ord2 (object x)
{
  if (I_fixnump(x)) {
    var uintV x_ = FN_to_V(x); /* x as intVsize-bit-number */
   #if (oint_data_len < intVsize)
    #if (intVsize>32)
    ord2_64(x_,return);
    #else
    ord2_32(x_,return);
    #endif
   #else /* oint_data_len=intVsize, x_ can also be =0 . */
    /* Only method 1c works at x = most-negative-fixnum. */
    x_ = x_ | (- x_); x_ = ~ x_;
    if (x_ == 0)
      return 0;
    #if (intVsize>32)
    integerlength64(x_,return);
    #else
    integerlength32(x_,return);
    #endif
   #endif
  } else {
    var uintL bitcount = 0;
    var uintD* ptr;
    BN_to_NDS_nocopy(x, _EMA_,_EMA_,ptr=); /* form normalized DS for x. */
    while (*--ptr == 0) { bitcount += intDsize; } /* count nulldigits */
    var uintD lsd = *ptr; /* last digit /=0 */
    ord2_D(lsd,bitcount +=); /* count its nullbits */
    return bitcount;
  }
}

/* I_power2p(x) determines, if an integer x>0 is a power of two.
 result: n>0, if x=2^(n-1), else 0.
 method 1: if ord2(x) = integer_length(x)-1.
 method 2: if logand(x,x-1) = 0.
 method 3: if the first digit /=0 is a power of two and all further
            digits are null. */
local uintL I_power2p (object x) {
  if (I_fixnump(x)) {
    var uintV x_ = posfixnum_to_V(x);
    if (!((x_ & (x_-1)) == 0))
      return 0; /* no power of two */
   #if (intVsize>32)
    integerlength64(x_,return); /* power of two: n = integer_length(x) */
   #else
    integerlength32(x_,return); /* power of two: n = integer_length(x) */
   #endif
  } else {
    var uintD* MSDptr;
    var uintC len;
    BN_to_NDS_nocopy(x, MSDptr=,len=,); /* form normalized DS for x. */
    var uintD msd = MSDptr[0];
    if (msd==0) {
      MSDptr++; msd = MSDptr[0]; len--;
    }
    /* len = number of digits at MSDptr, len>0, msd = first digit (/=0) */
    if (!((msd & (msd-1)) == 0))
      return 0; /* first digit must be a power of two */
    if (test_loop_up(&MSDptr[1],len-1))
      return 0; /* all null afterward */
    var uintL msdlen;
    integerlengthD(msd, msdlen=);
    return intDsize*(uintL)(len-1) + msdlen; /* integer_length(x) as result */
  }
}
