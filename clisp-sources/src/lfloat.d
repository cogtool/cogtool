/* Basic functions for Long-Floats */

/* error-message for too long Long-FLoat */
nonreturning_function(local, fehler_LF_toolong, (void)) {
  fehler(arithmetic_error,GETTEXT("long float too long"));
}

/* Decoding a Long-Float:
 LF_decode(obj, zero_statement, sign=,exp=,mantMSDptr=,mantlen=,mantLSDptr=);
 decodes a Long-Float obj.
 If obj=0.0, zero_statement is executed.
 Else: signean sign = sign (0 = +, -1 = -),
        sintL exp = exponent (with sign),
        UDS mantMSDptr/mantlen/mantLSDptr = mantissa
          (>= 2^(intDsize*mantlen-1), < 2^(intDsize*mantlen)),
          with mantlen>=LF_minlen. */
#define LF_decode(obj,zero_statement,sign_zuweisung,exp_zuweisung,mantMSDptr_zuweisung,mantlen_zuweisung,mantLSDptr_zuweisung)   do { \
  var object _obj = (obj);                                              \
  var Lfloat _x = TheLfloat(_obj);                                      \
  var uintL uexp = _x->expo;                                            \
  if (uexp==0) {                                                        \
    unused (mantlen_zuweisung lfloat_length(_x));                       \
    zero_statement; /* e=0 -> number 0.0 */                             \
  } else {                                                              \
    exp_zuweisung (sintL)(uexp - LF_exp_mid);     /* exponent */        \
    sign_zuweisung LF_sign(_obj);                 /* sign */            \
    unused (mantMSDptr_zuweisung &(_x->data[0])); /* mantissa-UDS */    \
    unused (mantLSDptr_zuweisung &(_x->data[(uintP)( mantlen_zuweisung lfloat_length(_x) )])); \
  }                                                                     \
 } while(0)

/* encoding a Long-Float:
 encode_LF0(len,erg_zuweisung) returns a Long-Float 0.0 with len digits.
 > uintC len: number of digits
 < object erg: new Long-Float 0.0 with len digits
 can trigger GC */
#define encode_LF0(len,erg_zuweisung)   do {                            \
  var uintC _len = (len);                                               \
  var object _erg = allocate_lfloat(_len,0,0); /* exponent 0, sign + */ \
  clear_loop_up(&TheLfloat(_erg)->data[0],_len); /* mantissa := 0 */    \
  erg_zuweisung _erg;                                                   \
 } while(0)

/* encoding a Long-Float:
 encode_LF1s(sign,len,erg_zuweisung) returns a Long-Float +-1.0 with len digits.
 > signean sign: sign
 > uintC len: number of digits
 < object erg: new Long-Float +1.0 or -1.0 with len digits
 can trigger GC */
#define encode_LF1s(sign,len,erg_zuweisung)    do {                     \
  var uintC _len = (len);                                               \
  var object _erg = allocate_lfloat(_len,LF_exp_mid+1,(sign)); /* exponent 1 */ \
  TheLfloat(_erg)->data[0] = bit(intDsize-1); /* mantissa := 2^(intDsize*len-1) */ \
  clear_loop_up(&TheLfloat(_erg)->data[1],_len-1);                      \
  erg_zuweisung _erg;                                                   \
 } while(0)

/* encoding a Long-Float:
 encode_LF1(len,erg_zuweisung) returns a Long-Float 1.0 with len digits.
 > uintC len: number of digits
 < object erg: new Long-Float 1.0 with len digits
 can trigger GC */
#define encode_LF1(len,erg_zuweisung)  encode_LF1s(0,len,erg_zuweisung)

/* encoding a Long-Float:
 encode_LFu(sign,uexp,mantMSDptr,mantlen, erg_zuweisung) returns a Long-Float
 > signean sign: sign
 > uintL exp: exponent + LF_exp_mid
 > uintD* mantMSDptr: pointer to a NUDS with set highest bit
 > uintC mantlen: number of digits, >= LF_minlen
 < object erg: new Long-Float with the UDS mantMSDptr/mantlen/.. as mantissa
 the exponent is not tested for overflow/underflow.
 can trigger GC */
#define encode_LFu(sign,uexp,mantMSDptr,mantlen,erg_zuweisung)        do { \
  var uintC _len = (mantlen);                                           \
  var object _erg = allocate_lfloat(_len,uexp,(sign)); /* exponent */   \
  copy_loop_up((mantMSDptr),&TheLfloat(_erg)->data[0],_len); /* mantissa copied */ \
  erg_zuweisung _erg;                                                   \
 } while(0)

/* encoding a Long-Float:
 encode_LF(sign,exp,mantMSDptr,mantlen, erg_zuweisung) returns a Long-Float
 > signean sign: sign
 > sintL exp: exponent
 > uintD* mantMSDptr: pointer to a NUDS with set highest bit
 > uintC mantlen: number of digits, >= LF_minlen
 < object erg: new Long-Float with the UDS mantMSDptr/mantlen/.. as mantissa
 the exponent is not tested for overflow/underflow.
 can trigger GC */
#define encode_LF(sign,exp,mantMSDptr,mantlen,erg_zuweisung)            \
  encode_LFu(sign,LF_exp_mid+(uintL)(exp),mantMSDptr,mantlen,_EMA_ erg_zuweisung)

/* hash-code of a Long-Float: mixture of exponent, length, first 32 bits */
global uint32 hashcode_lfloat (object obj) {
  return TheLfloat(obj)->expo + Lfloat_length(obj)
    + get_32_Dptr(&TheLfloat(obj)->data[0]);
}

/* LF_zerop(x) determines, if a Long-Float x is = 0.0 . */
#define LF_zerop(x)  (TheLfloat(x)->expo == 0)

/* returns for a Long-Float x : (ftruncate x), a LF.
 LF_ftruncate_LF(x)
 x is rounded towards 0 to the next whole number.
 can trigger GC */
local maygc object LF_ftruncate_LF (object x);
/* method:
 x = 0.0 or e<=0 -> result 0.0
 1<=e<=16n -> set the last (16n-e) bits of the mantissa to 0,
              keep exponent and sign
 e>=16n -> result x */
#if 0
local maygc object LF_ftruncate_LF (object x)
{
  var signean sign;
  var sintL exp;
  var uintD* mantMSDptr;
  var uintC mantlen;
  LF_decode(x, { return x; }, sign=,exp=,mantMSDptr=,mantlen=,);
  if (exp<=0) { encode_LF0(mantlen, return); } /* e<=0 -> result 0.0 */
  if ((uintL)exp >= intDsize*(uintL)mantlen) { /* e>=16n -> x as result */
    return x;
  } else { /* 0 < e < 16n */
    /* create new NUDS wit e bits from mant and 16n-e nullbits: */
    SAVE_NUM_STACK /* save num_stack */
    var uintD* MSDptr;
    num_stack_need(mantlen, MSDptr=,);
    {
      var uintC count = floor((uintL)exp,intDsize); /* digits to copy, < mantlen */
      var uintC bitcount = ((uintL)exp) % intDsize; /* bits to copy behind it, >=0, <intDsize */
      var uintD* ptr = /* copy count complete digits */
        copy_loop_up(mantMSDptr,MSDptr,count);
      *ptr++ = mantMSDptr[count] & minus_bitm(intDsize-bitcount); /* then copy bitcount bits */
      clear_loop_up(ptr,mantlen-count-1); /* fill rest with Nulls */
    }
    var object result;
    encode_LF(sign,exp,MSDptr,mantlen, result =);
    RESTORE_NUM_STACK /* restore num_stack */
    return result;
  }
}
#else
local maygc object LF_ftruncate_LF (object x)
{
  var uintC len = Lfloat_length(x);
  var uintL uexp = TheLfloat(x)->expo;
  if (uexp <= LF_exp_mid) {
    if (uexp == 0)
      return x; /* x=0.0 -> result 0.0 */
    encode_LF0(len, return); /* e<=0 -> result 0.0 */
  }
  var uintL exp = uexp - LF_exp_mid;
  if (exp >= intDsize*(uintL)len) /* e>=16n -> x as result */
    return x;
  /* 0 < e < 16n */
  pushSTACK(x);
  var object y = allocate_lfloat(len,uexp,LF_sign(x)); /* new Long-Float */
  x = popSTACK();
  /* y_mant := NUDS with e bits from x_mant and 16n-e nullbits: */
  var uintC count = floor(exp,intDsize); /* digits to copy, < mantlen */
  var uintC bitcount = exp % intDsize; /* bits to copy behind it, >=0, <intDsize */
  var uintD* x_mantMSDptr = &TheLfloat(x)->data[0];
  var uintD* ptr = /* copy count complete digits */
    copy_loop_up(x_mantMSDptr,&TheLfloat(y)->data[0],count);
  *ptr++ = x_mantMSDptr[count] & minus_bitm(intDsize-bitcount); /* then copy bitcount bits */
  clear_loop_up(ptr,len-count-1); /* fill rest with Nulls */
  return y;
}
#endif

/* Returns for a Long-Float x : (futruncate x), a LF.
 LF_futruncate_LF(x)
 x is rounded away from the 0 to the next whole number.
 can trigger GC */
local maygc object LF_futruncate_LF (object x);
/* method:
 x = 0.0 -> result 0.0
 e<=0 -> result 1.0 or -1.0, according to sign of x.
 1<=e<16n -> extract the last (16n-e) bits from x.
             If they are all =0 -> result x.
             Else set them all to 0 and then increase the front e bits
             by 1.
             No Overflow -> done.
             Else (result, a power of two): mantissa := .1000...000,
               e:=e+1. (test for overflow superfluous because of e<=16n)
 e>=16n -> result x. */
#if 0
local maygc object LF_futruncate_LF (object x)
{
  var signean sign;
  var sintL exp;
  var uintD* mantMSDptr;
  var uintC mantlen;
  LF_decode(x, { return x; }, sign=,exp=,mantMSDptr=,mantlen=,);
  if (exp<=0) { encode_LF1s(sign,mantlen, return); } /* e<=0 -> result +-1.0 */
  if ((uintL)exp >= intDsize*(uintL)mantlen) { /* e>=16n -> x as result */
    return x;
  } else { /* 0 < e < 16n */
    /* test, if all rear 16n-e bits are =0: */
    var uintC count = floor((uintL)exp,intDsize); /* digits to copy, < mantlen */
    var uintC bitcount = ((uintL)exp) % intDsize; /* bits to copy behind it, >=0, <intDsize */
    var uintD mask = minus_bitm(intDsize-bitcount); /* mask with bitcount bits */
    var uintD* mantptr = &mantMSDptr[count];
    if (((mantptr[0] & ~mask) ==0)
        && !test_loop_up(&mantptr[1],mantlen-count-1))
      return x;
    /* create new NUDS with e bits from mant with increment
       and 16n-e nullbits: */
    {
      SAVE_NUM_STACK /* save num_stack */
      var uintD* MSDptr;
      num_stack_need(mantlen, MSDptr=,);
      {
        var uintD* ptr = /* copy count complete digits */
          copy_loop_up(mantMSDptr,MSDptr,count);
        if ((ptr[0] = ((mantptr[0] & mask) - mask)) == 0) /* then copy bitcount bits and increment */
          if (inc_loop_down(ptr,count)!=0) { /* poss. continue to increment */
            MSDptr[0] = bit(intDsize-1); exp = exp+1; /* poss. increase exponent */
          }
        clear_loop_up(&ptr[1],mantlen-count-1); /* fill restrest with Nulls */
      }
      var object result;
      encode_LF(sign,exp,MSDptr,mantlen, result =);
      RESTORE_NUM_STACK /* restore num_stack */
      return result;
    }
  }
}
#else
local maygc object LF_futruncate_LF (object x)
{
  var uintC len = Lfloat_length(x);
  var uintL uexp = TheLfloat(x)->expo;
  if (uexp <= LF_exp_mid) {
    if (uexp == 0)
      return x; /* x=0.0 -> result 0.0 */
    encode_LF1s(LF_sign(x),len, return); /* e<=0 -> result +-1.0 */
  }
  var uintL exp = uexp - LF_exp_mid;
  if (exp >= intDsize*(uintL)len) /* e>=16n -> x as result */
    return x;
  /* 0 < e < 16n */
  /* test, if all rear 16n-e bits are =0 : */
  var uintC count = floor(exp,intDsize); /* digits to copy, < mantlen */
  var uintC bitcount = exp % intDsize; /* bits to copy behind it, >=0, <intDsize */
  var uintD mask = minus_bitm(intDsize-bitcount); /* mask with bitcount bits */
  {
    var uintD* mantptr = &TheLfloat(x)->data[count];
    if (((mantptr[0] & ~mask) ==0)
        && !test_loop_up(&mantptr[1],len-count-1))
      return x;
  }
  /* no -> produce new Long-Float: */
  pushSTACK(x);
  var object y = allocate_lfloat(len,uexp,LF_sign(x)); /* new Long-Float */
  x = popSTACK();
  /* y_mant := NUDS with e bits from x_mant with increment and 16n-e nullbits: */
  var uintD* x_mantMSDptr = &TheLfloat(x)->data[0];
  var uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
  var uintD* ptr = /* copy count complete digits */
    copy_loop_up(x_mantMSDptr,y_mantMSDptr,count);
  if ((ptr[0] = ((x_mantMSDptr[count] & mask) - mask)) == 0) /* then copy bitcount bits and increment */
    if (inc_loop_down(ptr,count)!=0) { /* poss. continue to increment */
      y_mantMSDptr[0] = bit(intDsize-1); (TheLfloat(y)->expo)++; /* poss. increase exponent */
    }
  clear_loop_up(&ptr[1],len-count-1); /* fill rest with Nulls */
  return y;
}
#endif

/* returns for a Long-Float x : (fround x), a LF.
 LF_fround_LF(x)
 x is rounded to the next whole number.
 can trigger GC */
local maygc object LF_fround_LF (object x);
/* method:
 x = 0.0 or e<0 -> result 0.0
 0<=e<16n -> round away the last (16n-e) bits of the mantissa,
             keep exponent and sign.
 e>=16n -> result x */
#if 0
local maygc object LF_fround_LF (object x)
{
  var signean sign;
  var sintL exp;
  var uintD* mantMSDptr;
  var uintC mantlen;
  LF_decode(x, { return x; }, sign=,exp=,mantMSDptr=,mantlen=,);
  if (exp<0) { encode_LF0(mantlen, return); } /* e<0 -> result 0.0 */
  if ((uintL)exp >= intDsize*(uintL)mantlen) { /* e>=16n -> x as result */
    return x;
  } else { /* 0 <= e < 16n */
    /* round away all rear 16n-e bits : */
    var uintC count = floor((uintL)exp,intDsize); /* digits to copy, < mantlen */
    var uintC bitcount = ((uintL)exp) % intDsize; /* bits to copy behind it, >=0, <intDsize */
    var uintD mask = minus_bit(intDsize-bitcount-1); /* mask with bitcount+1 bits */
    var uintD* mantptr = &mantMSDptr[count];
    if ((mantptr[0] & -mask) ==0) /* bit 16n-e-1 =0 -> round off */
      goto ab;
    if (!((mantptr[0] & ~mask) ==0)) /* bit 16n-e-1 =1 and bits 16n-e-2..0 >0 -> round up */
      goto auf;
    if (test_loop_up(&mantptr[1],mantlen-count-1))
      goto auf;
    /* round-to-even, according to bit 16n-e : */
    if (bitcount>0) {
      if ((mantptr[0] & (-2*mask)) ==0)
        goto ab;
      else
        goto auf;
    } else if (count>0) {
      if ((mantptr[-1] & bit(0)) ==0)
        goto ab;
      else
        goto auf;
    } else {
      /* bitcount=0, count=0, so exp=0: round off from +-0.5 to 0.0 */
      encode_LF0(mantlen, return);
    }
   ab: { /* round off */
      SAVE_NUM_STACK /* save num_stack */
      var uintD* MSDptr;
      num_stack_need(mantlen, MSDptr=,);
      var uintD* ptr = /* copy count complete digits */
        copy_loop_up(mantMSDptr,MSDptr,count);
      *ptr++ = mantMSDptr[count] & mask; /* then copy bitcount bits */
      clear_loop_up(ptr,mantlen-count-1); /* fill rest with Nulls */
      var object result;
      encode_LF(sign,exp,MSDptr,mantlen, result =);
      RESTORE_NUM_STACK /* restore num_stack */
      return result;
    }
   auf: { /* round up */
      SAVE_NUM_STACK /* save num_stack */
      var uintD* MSDptr;
      num_stack_need(mantlen, MSDptr=,);
      var uintD* ptr = /* copy count complete digits */
        copy_loop_up(mantMSDptr,MSDptr,count);
      if ((ptr[0] = ((mantptr[0] & mask) - mask)) == 0) /* then copy bitcount bits and increment */
        if (inc_loop_down(ptr,count)!=0) { /* poss. continue to increment */
          MSDptr[0] = bit(intDsize-1); exp = exp+1; /* poss. increase exponent */
        }
      clear_loop_up(&ptr[1],mantlen-count-1); /* fill rest with Nulls */
      var object result;
      encode_LF(sign,exp,MSDptr,mantlen, result =);
      RESTORE_NUM_STACK /* restore num_stack */
      return result;
    }
  }
}
#else
local maygc object LF_fround_LF (object x)
{
  var uintC len = Lfloat_length(x);
  var uintL uexp = TheLfloat(x)->expo;
  if (uexp < LF_exp_mid) {
    if (uexp == 0)
      return x; /* x=0.0 -> result 0.0 */
    encode_LF0(len, return); /* e<0 -> result 0.0 */
  }
  var uintL exp = uexp - LF_exp_mid;
  if (exp >= intDsize*(uintL)len) /* e>=16n -> x as result */
    return x;
  /* 0 <= e < 16n */
  /* round away all rear 16n-e bits: */
  var uintC count = floor(exp,intDsize); /* digits to copy, < mantlen */
  var uintC bitcount = exp % intDsize; /* bits to copy behind it, >=0, <intDsize */
  var uintD mask = minus_bit(intDsize-bitcount-1); /* mask with bitcount+1 bits */
  {
    var uintD* mantptr = &TheLfloat(x)->data[count];
   #if !(defined(__GNUC__) && (__GNUC__ == 2) && (__GNUC_MINOR__ == 7))
    if ((mantptr[0] & -mask) ==0) /* Bit 16n-e-1 =0 -> round off */
   #else
    /* Work around gcc-2.7.x bug on i386/ELF */
    if ((mantptr[0] & ((~mask)+1)) ==0) /* Bit 16n-e-1 =0 -> round off */
   #endif
      goto ab;
    if ((mantptr[0] & ~mask)!=0) /* Bit 16n-e-1 =1 and Bits 16n-e-2..0 >0 -> round up */
      goto auf;
    if (test_loop_up(&mantptr[1],len-count-1))
      goto auf;
    /* round-to-even, according to bit 16n-e : */
    if (bitcount>0) {
      if ((mantptr[0] & (-2*mask)) ==0)
        goto ab;
      else
        goto auf;
    } else if (count>0) {
      if ((mantptr[-1] & bit(0)) ==0)
        goto ab;
      else
        goto auf;
    } else {
      /* bitcount=0, count=0, so exp=0: rounding off from +-0.5 to 0.0 */
      encode_LF0(len, return);
    }
  }
 ab: { /* round off */
    pushSTACK(x);
    var object y = allocate_lfloat(len,uexp,LF_sign(x)); /* new Long-Float */
    x = popSTACK();
    /* y_mant := NUDS with e bits from x_mant and 16n-e Nullbits: */
    var uintD* x_mantMSDptr = &TheLfloat(x)->data[0];
    var uintD* ptr =
      copy_loop_up(x_mantMSDptr,&TheLfloat(y)->data[0],count); /* copy count complete digits */
    *ptr++ = x_mantMSDptr[count] & mask; /* then copy bitcount bits */
    clear_loop_up(ptr,len-count-1); /* fill rest with Nulls */
    return y;
  }
 auf: { /* round up */
    pushSTACK(x);
    var object y = allocate_lfloat(len,uexp,LF_sign(x)); /* new Long-Float */
    x = popSTACK();
    /* y_mant := NUDS with e bits from x_mant with increment and 16n-e Nullbits: */
    var uintD* x_mantMSDptr = &TheLfloat(x)->data[0];
    var uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
    var uintD* ptr = /* copy count complete digits */
      copy_loop_up(x_mantMSDptr,y_mantMSDptr,count);
    if ((ptr[0] = ((x_mantMSDptr[count] & mask) - mask)) == 0) /* then copy bitcount bits and increment */
      if (inc_loop_down(ptr,count)!=0) { /* poss. continue to increment */
        y_mantMSDptr[0] = bit(intDsize-1); (TheLfloat(y)->expo)++; /* poss. increase exponent */
      }
    clear_loop_up(&ptr[1],len-count-1); /* fill rest with Nulls */
    return y;
  }
}
#endif

/* returns for a Long-Float x : (- x), a LF.
 LF_minus_LF(x)
 can trigger GC
 method:
 if x=0.0, done. Else flip sign bit and keep pointer. */
local maygc object LF_minus_LF (object x)
{
  if (TheLfloat(x)->expo == 0) {
    return x;
  } else {
   #if defined(SPVW_MIXED) && defined(TYPECODES)
    return as_object(as_oint(x) ^ wbit(vorz_bit_o));
   #else
    var uintC len = Lfloat_length(x);
    pushSTACK(x);
    var object mx = allocate_lfloat(len,TheLfloat(x)->expo,~LF_sign(x));
    x = popSTACK();
    copy_loop_up(&TheLfloat(x)->data[0],&TheLfloat(mx)->data[0],len);
    return mx;
   #endif
  }
}

/* LF_LF_comp(x,y) compares two Long-Floats x and y.
 result: 0 if x=y, +1 if x>y, -1 if x<y.
 method:
 x and y have different signs ->
    x < 0 -> x < y
    x >= 0 -> x > y
 x and y have equal signs ->
    x >=0 -> compare x and y (the right 24 bits)
    x <0 -> compare y and x (the right 24 bits) */
local signean LF_LF_comp (object x, object y)
{
  if (!R_minusp(y)) { /* y>=0 */
    if (!R_minusp(x)) { /* y>=0, x>=0 */
      { /* compare exponents and mantissas: */
        var uintL x_uexp = TheLfloat(x)->expo;
        var uintL y_uexp = TheLfloat(y)->expo;
        if (x_uexp < y_uexp)
          return signean_minus; /* x<y */
        if (x_uexp > y_uexp)
          return signean_plus; /* x>y */
      }
      {
        var uintC x_len = Lfloat_length(x);
        var uintC y_len = Lfloat_length(y);
        var uintC len = (x_len<y_len ? x_len : y_len); /* min(x_len,y_len) */
        /* compare len digits: */
        var signean erg =
          compare_loop_up(&TheLfloat(x)->data[0],&TheLfloat(y)->data[0],len);
        if (erg!=0)
          return erg; /* different -> done */
        /* commen sub part was equal */
        if (x_len == y_len)
          return signean_null; /* equal length -> done */
        if (x_len > y_len) { /* x longer than y */
          if (test_loop_up(&TheLfloat(x)->data[y_len],x_len-y_len))
            return signean_plus; /* x>y */
          else
            return signean_null;
        } else { /* y longer than x */
          if (test_loop_up(&TheLfloat(y)->data[x_len],y_len-x_len))
            return signean_minus; /* x<y */
          else
            return signean_null;
        }
      }
    } else { /* y>=0, x<0 */
      return signean_minus; /* x<y */
    }
  } else {
    if (!R_minusp(x)) { /* y<0, x>=0 */
      return signean_plus; /* x>y */
    } else { /* y<0, x<0 */
      { /* compare exponents and mantissas: */
        var uintL x_uexp = TheLfloat(x)->expo;
        var uintL y_uexp = TheLfloat(y)->expo;
        if (x_uexp < y_uexp)
          return signean_plus; /* |x|<|y| -> x>y */
        if (x_uexp > y_uexp)
          return signean_minus; /* |x|>|y| -> x<y */
      }
      {
        var uintC x_len = Lfloat_length(x);
        var uintC y_len = Lfloat_length(y);
        var uintC len = (x_len<y_len ? x_len : y_len); /* min(x_len,y_len) */
        /* compare len digits: */
        var signean erg =
          compare_loop_up(&TheLfloat(y)->data[0],&TheLfloat(x)->data[0],len);
        if (erg!=0)
          return erg; /* different -> done */
        /* common sub part was equal */
        if (x_len == y_len)
          return signean_null; /* equal length -> done */
        if (x_len > y_len) { /* x longer than y */
          if (test_loop_up(&TheLfloat(x)->data[y_len],x_len-y_len))
            return signean_minus; /* |x|>|y| -> x<y */
          else
            return signean_null;
        } else { /* y longer than x */
          if (test_loop_up(&TheLfloat(y)->data[x_len],y_len-x_len))
            return signean_plus; /* |x|<|y| -> x>y */
          else
            return signean_null;
        }
      }
    }
  }
}

/* LF_shorten_LF(x,len) shortens a Long-Float x to given length len
 and rounds.
 > object x: a Long-FLoat
 > uintC len: wished length (>= LF_minlen, < Lfloat_length(x))
 < object result: shortened Long-Float
 can trigger GC */
local maygc object LF_shorten_LF (object x, uintC len)
{
  /* x = 0.0 does not need to be caught, because when mantissa is 0
     we round of anyway, so the mantissa remains 0. */
  pushSTACK(x);
  var object y = allocate_lfloat(len,TheLfloat(x)->expo,LF_sign(x)); /* new LF */
  x = popSTACK();
  var uintC oldlen = Lfloat_length(x); /* old length, > len */
  /* copy mantissa from x to y: */
  copy_loop_up(&TheLfloat(x)->data[0],&TheLfloat(y)->data[0],len);
  /* decide, if to round up or off: */
  var uintD* ptr = &TheLfloat(x)->data[len];
  if (((sintD)ptr[0] >= 0) /* next bit a 0 -> round off */
       || (((ptr[0] & ((uintD)bit(intDsize-1)-1)) ==0) /* a 1 and all further Nulls? */
           && !test_loop_up(&ptr[1],oldlen-len-1)
           /* round-to-even */
           && ((ptr[-1] & bit(0)) ==0))) {
    /* round off */
  } else { /* round up */
    if ( inc_loop_down(&TheLfloat(y)->data[len],len) ) {
      /* carry by rounding up */
      TheLfloat(y)->data[0] = bit(intDsize-1); /* mantissa := 10...0 */
      /* increase exponent: */
      if (++(TheLfloat(y)->expo) == (uint32)(LF_exp_high+1))
        fehler_overflow();
    }
  }
  return y;
}

/* LF_extend_LF(x,len) extends a Long-Float x to a given length len.
 > object x: a Long-FLoat
 > uintC len: wished length (> Lfloat_length(x))
 < object result: extended Long-Float
 can trigger GC */
local maygc object LF_extend_LF (object x, uintC len)
{
  pushSTACK(x);
  var object y = allocate_lfloat(len,TheLfloat(x)->expo,LF_sign(x)); /* new LF */
  x = popSTACK();
  var uintC oldlen = Lfloat_length(x); /* old length, < len */
  /* copy mantissa from x to y: */
  var uintD* ptr =
    copy_loop_up(&TheLfloat(x)->data[0],&TheLfloat(y)->data[0],oldlen);
  /* and complete with Null digits: */
  clear_loop_up(ptr,len-oldlen);
  return y;
}

/* LF_to_LF(x,len) converts a Long-Float x into a Long-Float of given length
 len and rounds if necessary.
 > object x: a Long-FLoat
 > uintC len: wished length (>= LF_minlen)
 < object result: Long-Float of given length
 can trigger GC */
local maygc object LF_to_LF (object x, uintC len)
{
  var uintC oldlen = Lfloat_length(x);
  if (len < oldlen)
    return LF_shorten_LF(x,len);
  if (len > oldlen)
    return LF_extend_LF(x,len);
  /* len = oldlen */
  return x;
}

/* returns for two equal-length Long-Float x and y : (+ x y), a LF.
 LF_LF_plus_LF(x,y)
 can trigger GC
 method (according to [Knuth, II, Seminumerical Algorithms, section 4.2.1., S.200]):
 if e1<e2, swap x1 and x2.
 So e1 >= e2.
 if e2=0, so x2=0.0, result x1.
 if e1 - e2 >= 16n+2, result x1.
 Extend the mantissas right by 3 bits (bit -1 as protection bit, bits -2,-3
   as rounding bits: 00 exact, 01 1. half, 10 exact middle, 11 2. half.)
 Shift the mantissa of x2 by e0-e1 bits to the right. (Execute the
 rounding: bit -3 is the logical Or of the bits -3,-4,-5,...)
 if x1,x2 have equal sign: Add it to the mantissa of x1.
 if x1,x2 have different sign: Subtract it from the
    mantissa of x1. <0 -> (e1=e2) swap the signs, negate.
                    =0 -> result 0.0
 exponent is e1.
 normalize, done. */
local maygc object LF_LF_plus_LF (object x1, object x2)
{
  var uintL uexp1 = TheLfloat(x1)->expo;
  var uintL uexp2 = TheLfloat(x2)->expo;
  if (uexp1 < uexp2) { /* swap x1 and x2 */
    swap(object, x1,x2); swap(uintL, uexp1,uexp2);
  }
  /* uexp1 >= uexp2 */
  if (uexp2==0)
    return x1; /* x2=0.0 -> x1 as result */
  var uintC len = Lfloat_length(x1); /* length n of x1 and x2 */
  var uintL expdiff = uexp1-uexp2; /* e1-e2 */
 #if !(defined(SPVW_MIXED) && defined(TYPECODES))
  if ((expdiff == 0) && !same_sign_p(x1,x2)) {
    /* different signs, but same exponent */
    /* determine sign of the result: */
    var signean erg = /* compare mantissas (len digits at a time)  */
      compare_loop_up(&TheLfloat(x1)->data[0],&TheLfloat(x2)->data[0],len);
    if (erg==0) { /* mantissas equal */
      encode_LF0(len, return); /* result 0.0 */
    }
    if (erg<0) { /* |x1| < |x2| */
      /* swap x1 and x2, expdiff remains =0 */
      swap(object, x1,x2); swap(uintL, uexp1,uexp2);
    }
  }
 #endif
  if (expdiff >= intDsize * (uintL)len + 2) /* e1-e2 >= 16n+2 ? */
    return x1; /* yes -> x1 as result */
  /* allocate new Long-Float: */
  pushSTACK(x1); pushSTACK(x2);
  var object y = allocate_lfloat(len,uexp1,LF_sign(x1));
  x2 = popSTACK(); x1 = popSTACK();
  var uintL i = floor(expdiff,intDsize); /* e1-e2 div 16 (>=0, <=n) */
  var uintL j = expdiff % intDsize; /* e1-e2 mod 16 (>=0, <16) */
  /* Mantissa of x2 must be shifted by intDsize*i+j bits to the right. */
  var uintC x2_len = len - i; /* n-i digits of x2 used */
  /* shift x2_len digits by j bits to the right and copy them: */
  SAVE_NUM_STACK /* save num_stack */
  var uintD* x2_MSDptr;
  var uintD* x2_LSDptr;
  var uintD rounding_bits;
  num_stack_need(x2_len, x2_MSDptr=,x2_LSDptr=); /* x2_len digits room */
  begin_arith_call();
  if (j==0) {
    copy_loop_up(&TheLfloat(x2)->data[0],x2_MSDptr,x2_len); rounding_bits = 0;
  } else {
    rounding_bits = shiftrightcopy_loop_up(&TheLfloat(x2)->data[0],x2_MSDptr,x2_len,j,0);
  }
  /* x2_MSDptr/x2_len/x2_LSDptr are the essential digits of x2.
     rounding_bits contains the last j shifted out bits.
     build the 3 rounding bits from rounding_bits and the next i digits
     (as bits intDsize-1..intDsize-3 from rounding_bits) : */
  if (j>=2) { /* j>=2 -> bits -1,-2 are OK, determine bit -3: */
    if ((rounding_bits & (bit(intDsize-3)-1)) ==0) {
      if (test_loop_up(&TheLfloat(x2)->data[x2_len],i))
        rounding_bits |= bit(intDsize-3); /* set rounding bit -3 */
    } else {
      rounding_bits |= bit(intDsize-3); /* set rounding bit -3 */
      rounding_bits &= bitm(intDsize)-bit(intDsize-3); /* delete other bits */
    }
  } else {
    /* j<=3 -> bits intDsize-4..0 from rounding_bits are already Null.
       pull up next and further i-1 digits: */
    if (i > 0) { /* i=0 -> bits -1,-2,-3 are OK. */
      var uintD* ptr = &TheLfloat(x2)->data[x2_len];
      rounding_bits |= (ptr[0] >> j); /* add further relevant bits of the next digit */
      if ((rounding_bits & (bit(intDsize-3)-1)) ==0) { /* all bits -3,-4,... =0 ? */
        if (((ptr[0] & (bit(3)-1))!=0) /* j (<=3) lower bits of ptr[0] all =0 ? */
            || test_loop_up(&ptr[1],i-1))
          rounding_bits |= bit(intDsize-3); /* set rounding bit -3 */
      } else {
        rounding_bits |= bit(intDsize-3); /* set rounding bit -3 */
        rounding_bits &= bitm(intDsize)-bit(intDsize-3); /* delete other bits */
      }
    }
  }
  /* x2 is there in shifted form in the UDS x2_MSDptr/x2_len/x2_LSDptr ,
     with rounding bits in bit intDsize-1..intDsize-3 from rounding_bits. */
  {
    var uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
    var uintD* y_mantLSDptr = &y_mantMSDptr[(uintP)len];
    if (same_sign_p(x1,x2)) { /* equal sign -> add mantissas */
      /* first the right mantissa part (x2_len digits) by addition: */
      var uintD carry =
        add_loop_down(&TheLfloat(x1)->data[(uintP)len],x2_LSDptr,
                      y_mantLSDptr, x2_len);
      /* then copy the left mantissa part (i digits) directly: */
      var uintD* ptr =
        copy_loop_up(&TheLfloat(x1)->data[0],y_mantMSDptr,i);
      /* then add carry from the right part to the left mantissa part: */
      if (carry!=0)
        if ( inc_loop_down(ptr,i) ) { /* carry beyond the first digit */
          /* increment exponent of y : */
          if (++(TheLfloat(y)->expo) == (uint32)(LF_exp_high+1))
            fehler_overflow();
          /* normalize by shifting by 1 bit to the right: */
          var uintD carry_rechts =
            shift1right_loop_up(y_mantMSDptr,len,(uintD)(-1));
          rounding_bits = rounding_bits>>1; /* also shift rounding bits */
          if (!(carry_rechts==0))
            rounding_bits |= bit(intDsize-1);
        }
    } else { /* different signs -> subtract mantissas */
      /* first the right mantissa part (x2_len digits) by subtraction: */
      rounding_bits = -rounding_bits;
      var uintD carry =
        subx_loop_down(&TheLfloat(x1)->data[(uintP)len],x2_LSDptr,
                       y_mantLSDptr, x2_len,
                       (rounding_bits==0 ? 0 : -1L));
      /* then copy the left mantissa part (i digits) directly: */
      var uintD* ptr =
        copy_loop_up(&TheLfloat(x1)->data[0],y_mantMSDptr,i);
      /* then subtract carry from the right part from the left mantissa part: */
      if (carry!=0)
        if ( dec_loop_down(ptr,i) ) {
          /* carry beyond the first digit, so e1=e2 */
         #if !(defined(SPVW_MIXED) && defined(TYPECODES))
          NOTREACHED; /* we have already treated this case */
         #else
          /* negate: */
          y = as_object(as_oint(y) ^ wbit(vorz_bit_o));
          rounding_bits = -rounding_bits;
          if (rounding_bits==0) {
            /* negate without carry */
            neg_loop_down(y_mantLSDptr,len);
          } else { /* negate with carry from the right */
            /* not_loop_down(y_mantLSDptr,len); // or */
            not_loop_up(y_mantMSDptr,len);
          }
         #endif
        }
    }
    /* normalize UDS y_mantMSDptr/len/y_mantLSDptr/rounding_bits: */
    {
      var uintD* ptr = y_mantMSDptr;
      var uintL k = 0;
      var uintC count;
      dotimesC(count,len, {
        if (!(ptr[0]==0))
          goto nonzero_found;
        ptr++; k++;
      });
      if (!(rounding_bits==0))
        goto nonzero_found;
      /* the UDS is completely Null. So e1=e2, no rounding bits. */
      end_arith_call();
      RESTORE_NUM_STACK /* restore num_stack */
     #if !(defined(SPVW_MIXED) && defined(TYPECODES))
      NOTREACHED; /* we have already treated this case */
     #else
      TheLfloat(y)->expo = 0; /* 0.0 as result */
      return as_object(as_oint(y) & ~wbit(vorz_bit_o));
     #endif
     nonzero_found: /* digit /=0 found */
      /* copy UDS from ptr to y_mantMSDptr by k digits downwards: */
      if (k>0) {
        /* at least one leading Nulldigit. So, e1-e2 = 0 or 1. */
        ptr = copy_loop_up(ptr,y_mantMSDptr,len-k); /* shift len-k digits */
        *ptr++ = rounding_bits; /* rounding bits as further digit */
        clear_loop_up(ptr,k-1); /* then k-1 nulldigits */
        rounding_bits = 0; /* and no further rounding bits */
        /* decrease exponent by intDsize*k : */
        k = intDsize*k;
        var uintL uexp = TheLfloat(y)->expo;
       #if !(LF_exp_low==1)
        if (uexp < k+LF_exp_low)
       #else
        if (uexp <= k)
       #endif
          {
            end_arith_call();
            RESTORE_NUM_STACK /* restore num_stack */
            if (underflow_allowed()) {
              fehler_underflow();
            } else {
              encode_LF0(len, return); /* result 0.0 */
            }
          }
        TheLfloat(y)->expo = uexp - k;
      }
    }
    /* normalize NUDS y_mantMSDptr/len/y_mantLSDptr/rounding_bits : */
    {
      var uintL s;
      integerlengthD(y_mantMSDptr[0], s = intDsize - );
      /* s = number of leading nullbits in first word (>=0, <intDsize) */
      if (s > 0) {
        /* Shift the NUDS y_mantMSDptr/len/y_mantLSDptr/rounding_bits
           by s bits to the left.
           (e1-e2>1 enforces s=1.) */
        if (s==1) {
          shift1left_loop_down(y_mantLSDptr,len);
          if (rounding_bits & bit(intDsize-1))
            y_mantLSDptr[-1] |= bit(0);
          rounding_bits = rounding_bits << 1;
        } else { /* s>1, so e1-e2 <= 1 <= s. */
          shiftleft_loop_down(y_mantLSDptr,len,s,rounding_bits>>(intDsize-s));
          rounding_bits = 0; /* = rounding_bits << s; */
        }
        /* decrease exponent by s : */
        var uintL uexp = TheLfloat(y)->expo;
       #if !(LF_exp_low==1)
        if (uexp < s+LF_exp_low)
       #else
        if (uexp <= s)
       #endif
          {
            end_arith_call();
            RESTORE_NUM_STACK /* restore num_stack */
            if (underflow_allowed()) {
              fehler_underflow();
            } else {
              encode_LF0(len, return); /* result 0.0 */
            }
          }
        TheLfloat(y)->expo = uexp - s;
      }
    }
    /* here, rounding_bits contains bit -1 as Bit intDsize-1, bit -2 as
       bit intDsize-2, Bit -3 as Or(Bits intDsize-3..0) !
       Round. Inspect rounding_bits: */
    if ((rounding_bits & bit(intDsize-1)) ==0) /* bit -1 deleted -> round off */
      goto ab;
    rounding_bits = rounding_bits<<1; /* bits -2,-3 */
    if (!(rounding_bits==0)) /* bit -2 or bit -3 set -> round up */
      goto auf;
    /* round-to-even: */
    if ((y_mantLSDptr[-1] & bit(0)) ==0)
      goto ab;
   auf: /* round up */
    if ( inc_loop_down(y_mantLSDptr,len) ) {
      /* carry by rounding up */
      y_mantMSDptr[0] = bit(intDsize-1); /* mantissa := 10...0 */
      /* increase exponent: */
      if (++(TheLfloat(y)->expo) == (uint32)(LF_exp_high+1)) {
        end_arith_call(); RESTORE_NUM_STACK; fehler_overflow();
      }
    }
   ab: /* round off */
    ;
  }
  end_arith_call();
  RESTORE_NUM_STACK /* restore num_stack */
  /* y done. */
  return y;
}

/* returns for two equal-length Long-Float x and y : (- x y), a LF.
 LF_LF_minus_LF(x,y)
 can trigger GC
 method:
 (- x1 x2) = (+ x1 (- x2)) */
local maygc object LF_LF_minus_LF (object x1, object x2)
{
  if (TheLfloat(x2)->expo == 0) {
    return x1;
  } else {
   #if defined(SPVW_MIXED) && defined(TYPECODES)
    return LF_LF_plus_LF(x1, as_object(as_oint(x2) ^ wbit(vorz_bit_o)) );
   #else
    var uintC len2 = Lfloat_length(x2);
    pushSTACK(x1); pushSTACK(x2);
    var object mx2 = allocate_lfloat(len2,TheLfloat(x2)->expo,~LF_sign(x2));
    x2 = popSTACK();
    copy_loop_up(&TheLfloat(x2)->data[0],&TheLfloat(mx2)->data[0],len2);
    return LF_LF_plus_LF(popSTACK(),mx2);
   #endif
  }
}

/* returns for a Long-Float x : (* x x), a LF.
 LF_square_LF(x)
 can trigger GC
 method:
 If x=0.0 -> result 0.0
 Else: result-sign = positive.
        result-exponent = 2 * exponent of x.
        calcualte square of the mantissas (2n digits).
        If the leading bit is =0 : shift mantissa product by 1 bit to
          the left (the front n+1 digits are enough)
          and decrement exponent.
        rounding to n digits yields the result-mantissa. */
local maygc object LF_square_LF (object x)
{
  var uintL uexp = TheLfloat(x)->expo;
  if (uexp==0)
    return x; /* x=0.0 -> result 0.0 */
  /* add exponents:
     (uexp-LF_exp_mid) + (uexp-LF_exp_mid) = (2*uexp-LF_exp_mid)-LF_exp_mid */
  if ((sintL)uexp >= 0) { /* no Carry */
    uexp = 2*uexp;
    if (uexp < LF_exp_mid+LF_exp_low) {
      if (underflow_allowed()) {
        fehler_underflow();
      } else {
        encode_LF0(Lfloat_length(x), return); /* result 0.0 */
      }
    }
  } else { /* Carry */
    uexp = 2*uexp;
    if (uexp > (uintL)(LF_exp_mid+LF_exp_high+1))
      fehler_overflow();
  }
  uexp = uexp - LF_exp_mid;
  /* now, LF_exp_low <= uexp <= LF_exp_high+1.
     allocate new Long-Float: */
  pushSTACK(x);
  var uintC len = Lfloat_length(x); /* length n of x */
  var object y = allocate_lfloat(len,uexp,0);
  SAVE_NUM_STACK /* save num_stack */
    x = popSTACK();
  { /* form product: */
    var uintD* MSDptr;
    begin_arith_call();
    UDS_square_UDS(len,&TheLfloat(x)->data[(uintP)len],
                   MSDptr=,_EMA_,);
    var uintD* midptr = &MSDptr[(uintP)len]; /* pointer into the middle of the 2n digits */
    if ((sintD)MSDptr[0] >= 0) { /* test leading bit */
      /* shift the first n+1 digits by 1 bit to the left: */
      shift1left_loop_down(&midptr[1],len+1);
      /* decrement exponent: */
      if ((TheLfloat(y)->expo)-- == LF_exp_low-1) {
        end_arith_call();
        RESTORE_NUM_STACK /* restore num_stack */
          if (underflow_allowed()) {
            fehler_underflow();
          } else {
            encode_LF0(len, return); /* result 0.0 */
          }
      }
    }
    end_arith_call();
    /* carry forward the first half of the mantissa product: */
    var uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
    var uintD* y_mantLSDptr =
      copy_loop_up(MSDptr,y_mantMSDptr,len);
    /* round: */
    if (((sintD)midptr[0] >= 0) /* next bit =0 -> round off */
        || (((midptr[0] & ((uintD)bit(intDsize-1)-1)) ==0) /* bit =1, further bits >0 -> round up */
            && !test_loop_up(&midptr[1],len-1)
            /* round-to-even */
            && ((midptr[-1] & bit(0)) ==0))) {
      /* round off */
    } else {
      /* round up */
      if ( inc_loop_down(y_mantLSDptr,len) ) {
        /* carry due to rounding up (can only happen,
           if a shift by 1 bit to the left took place, beforehand) */
        y_mantMSDptr[0] = bit(intDsize-1); /* mantissa := 10...0 */
        (TheLfloat(y)->expo)++; /* re-increment exponent again */
      }
    }
    /* assure LF_exp_low <= exp <= LF_exp_high : */
    if (TheLfloat(y)->expo == (uint32)(LF_exp_high+1)) {
      RESTORE_NUM_STACK; fehler_overflow();
    }
  }
  RESTORE_NUM_STACK /* restore num_stack */
  return y;
}

/* returns for two equal-length Long-Float x and y : (* x y), a LF.
 LF_LF_mal_LF(x,y)
 can trigger GC
 method:
 If x1=0.0 or x2=0.0 -> result 0.0
 Else: result-sign = sign of x1 xor sign of x2.
        result-exponent = sum of exponents of x1 and x2.
        form product of mantissas (2n Digits).
        If the leading bit is =0 : shift mantissa product by 1 bit to
          the left (the front n+1 digits are enough)
          and decrement exponent.
        Rounding to n digits yields the result-mantissa. */
local maygc object LF_LF_mal_LF (object x1, object x2)
{
  var uintL uexp1 = TheLfloat(x1)->expo;
  if (uexp1==0)
    return x1; /* x1=0.0 -> result 0.0 */
  var uintL uexp2 = TheLfloat(x2)->expo;
  if (uexp2==0)
    return x2; /* x2=0.0 -> result 0.0 */
  /* add exponents:
     (uexp1-LF_exp_mid) + (uexp2-LF_exp_mid) =
     (uexp1+uexp2-LF_exp_mid)-LF_exp_mid */
  uexp1 = uexp1 + uexp2;
  if (uexp1 >= uexp2) { /* no Carry */
    if (uexp1 < LF_exp_mid+LF_exp_low) {
      if (underflow_allowed()) {
        fehler_underflow();
      } else {
        encode_LF0(Lfloat_length(x1), return); /* result 0.0 */
      }
    }
  } else { /* Carry */
    if (uexp1 > (uintL)(LF_exp_mid+LF_exp_high+1))
      fehler_overflow();
  }
  uexp1 = uexp1 - LF_exp_mid;
  /* now, LF_exp_low <= uexp1 <= LF_exp_high+1. */
  /* allocate new Long-Float: */
  pushSTACK(x1); pushSTACK(x2);
  var uintC len = Lfloat_length(x1); /* length n of x1 and x2 */
 #ifdef TYPECODES
  var signean sign = R_sign(as_object(as_oint(x1) ^ as_oint(x2))); /* combine signs */
 #else
  var signean sign = LF_sign(x1) ^ LF_sign(x2);
 #endif
  var object y = allocate_lfloat(len,uexp1,sign);
  SAVE_NUM_STACK /* save num_stack */
  x2 = popSTACK(); x1 = popSTACK();
  { /* form product: */
    var uintD* MSDptr;
    begin_arith_call();
    UDS_UDS_mal_UDS(len,&TheLfloat(x1)->data[(uintP)len],
                    len,&TheLfloat(x2)->data[(uintP)len],
                    MSDptr=,_EMA_,);
    var uintD* midptr = &MSDptr[(uintP)len]; /* pointer into the middle of the 2n digits */
    if ((sintD)MSDptr[0] >= 0) { /* test leading bit */
      /* shift the first n+1 Digits by 1 bit to the left: */
      shift1left_loop_down(&midptr[1],len+1);
      /* decrement exponent: */
      if (--(TheLfloat(y)->expo) == LF_exp_low-1) {
        end_arith_call();
        RESTORE_NUM_STACK /* restore num_stack */
        if (underflow_allowed()) {
          fehler_underflow();
        } else {
          encode_LF0(len, return); /* result 0.0 */
        }
      }
    }
    end_arith_call();
    /* carry first half of the mantissa product : */
    var uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
    var uintD* y_mantLSDptr = copy_loop_up(MSDptr,y_mantMSDptr,len);
    /* round: */
    if (((sintD)midptr[0] >= 0) /* next bit =0 -> round off */
         || (((midptr[0] & ((uintD)bit(intDsize-1)-1)) ==0) /* bit =1, further bits >0 -> round up */
             && !test_loop_up(&midptr[1],len-1)
             /* round-to-even */
             && ((midptr[-1] & bit(0)) ==0))) {
      /* round off */
    } else { /* round up */
      if ( inc_loop_down(y_mantLSDptr,len) ) {
        /* carry due to rounding up (can only happen,
           if shift by  1 bit to the left took place, beforehand) */
        y_mantMSDptr[0] = bit(intDsize-1); /* mantissa := 10...0 */
        (TheLfloat(y)->expo)++; /* re-increment exponent again */
      }
    }
    /* assure LF_exp_low <= exp <= LF_exp_high : */
    if (TheLfloat(y)->expo == (uint32)(LF_exp_high+1)) {
      RESTORE_NUM_STACK; fehler_overflow();
    }
  }
  RESTORE_NUM_STACK /* restore num_stack */
  return y;
}

/* returns for two equal-length Long-Float x and y : (/ x y), a LF.
 LF_LF_durch_LF(x,y)
 can trigger GC
 method:
 x2 = 0.0 -> Error
 x1 = 0.0 -> result 0.0
 Else:
 result-sign     = xor of the two sings of x1 and x2
 result-exponent = difference of the two exponents of x1 and x2
 result-mantissa = mantissa mant1 / mantissa mant2, rounded.
   mant1/mant2 > 1/2, mant1/mant2 < 2;
   after rounding mant1/mant2 >=1/2, <=2*mant1<2.
   When mant1/mant2 >=1 we need 16n-1 bits behind the dot,
   when mant1/mant2 <1 we need 16n bits behind the dot.
   For Rounding: We need one rounding bit (rest specifies, if exact).
   Hence, we need 16n+1 bits behind the dot from mant1/mant2, altogether.
   Divide daher (as Unsigned Integers)
     2^16(n+1)*(2^16n*m0) by (2^16n*m1).
   If the quotient is >=2^16(n+1) , shift it by 1 bit to the right,
     increase the exponent by 1 and round the last digit away.
   If the quotient is <2^16(n+1) , round the last digit away. When rounding
    overflow occurs, shift by 1 bit to the right and increase exponent by 1. */
/* work around a gcc-2.7.0 bug on i386. */
#if defined(__GNUC__) && (__GNUC__ == 2) && (__GNUC_MINOR__ == 7)
  #define workaround_gcc270_bug()  *&uexp1 = *&uexp1;
#else
  #define workaround_gcc270_bug()
#endif
local maygc object LF_LF_durch_LF (object x1, object x2)
{
  var uintL uexp2 = TheLfloat(x2)->expo;
  if (uexp2==0)
    divide_0(); /* x2=0.0 -> Error */
  var uintL uexp1 = TheLfloat(x1)->expo;
  if (uexp1==0)
    return x1; /* x1=0.0 -> result 0.0 */
  /* suvtract exponents:
     (uexp1-LF_exp_mid) - (uexp2-LF_exp_mid) =
     (uexp1-uexp2+LF_exp_mid)-LF_exp_mid */
  if (uexp1 >= uexp2) {
    uexp1 = uexp1 - uexp2; /* no carry */
    workaround_gcc270_bug();
    if (uexp1 > LF_exp_high-LF_exp_mid)
      fehler_overflow();
    uexp1 = uexp1 + LF_exp_mid;
  } else {
    uexp1 = uexp1 - uexp2; /* carry */
    workaround_gcc270_bug();
    if (uexp1 < (uintL)(LF_exp_low-1-LF_exp_mid)) {
      if (underflow_allowed()) {
        fehler_underflow();
      } else {
        encode_LF0(Lfloat_length(x1), return); /* result 0.0 */
      }
    }
    uexp1 = uexp1 + LF_exp_mid;
  }
  /* LF_exp_low-1 <= uexp1 <= LF_exp_high. */
  /* allocate new Long-Float: */
  pushSTACK(x1); pushSTACK(x2);
  var uintC len = Lfloat_length(x1); /* length n of x1 and x2 */
#ifdef TYPECODES
  var signean sign = R_sign(as_object(as_oint(x1) ^ as_oint(x2))); /* combine sign */
#else
  var signean sign = LF_sign(x1) ^ LF_sign(x2);
#endif
  var object y = allocate_lfloat(len,uexp1,sign);
  x2 = popSTACK(); x1 = popSTACK();
  { /* form counter: */
    var uintD* z_MSDptr;
    var uintL z_len;
    var uintD* z_LSDptr;
    z_len = 2*(uintL)len + 1;
    if ((intWCsize < 32) && (z_len > (uintL)(bitc(intWCsize)-1)))
      fehler_LF_toolong();
    {
      SAVE_NUM_STACK /* save num_stack */
      num_stack_need(z_len, z_MSDptr=,z_LSDptr=);
      {
        var uintD* ptr =
          copy_loop_up(&TheLfloat(x1)->data[0],z_MSDptr,len); /* copy n digits */
        clear_loop_up(ptr,len+1); /* and n+1 Null-Digits */
      }
      /* form quotient: divide 2n+1-digit-number by n-digit-number */
      begin_arith_call();
      var DS q;
      var DS r;
      {
        var uintD* x2_mantMSDptr = &TheLfloat(x2)->data[0];
        UDS_divide(z_MSDptr,z_len,z_LSDptr,
                   x2_mantMSDptr,len,&x2_mantMSDptr[(uintP)len],
                   &q, &r);
      }
      /* q is the quotient with n+1 or n+2 digits, r the rest. */
      if (q.len > len+1) {
        /* quotient has n+2 digits -> shift by 1 bit to the right: */
        var uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
        var uintD carry_rechts =
          shiftrightcopy_loop_up(&q.MSDptr[1],y_mantMSDptr,len,1,
                                 /* carry left = q.MSDptr[0] = 1 */ 1 );
        /* increment exponent: */
        if (++(TheLfloat(y)->expo) == (uint32)(LF_exp_high+1))
          fehler_overflow();
        /* round: */
        if ((carry_rechts == 0) /* shifted out bit =0 -> round off */
            || ((q.LSDptr[-1]==0) /* =1 and further bits >0 or rest >0 -> round up */
                && (r.len==0)
                /* round-to-even */
                && ((q.LSDptr[-2] & bit(1)) ==0))) {
          /* round off */
        } else {
          /* round up */
          inc_loop_down(&y_mantMSDptr[(uintP)len],len);
        }
      } else {
        /* quotient has n+1 digits -> justs copy: */
        var uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
        copy_loop_up(q.MSDptr,y_mantMSDptr,len);
        /* round: */
        if (((sintD)(q.LSDptr[-1]) >= 0) /* next bit =0 -> round off */
            || (((q.LSDptr[-1] & ((uintD)bit(intDsize-1)-1)) ==0) /* =1 and further bits >0 or rest >0 -> round up */
                && (r.len==0)
                /* round-to-even */
                && ((q.LSDptr[-2] & bit(0)) ==0))) {
          /* round off */
        } else { /* round up */
          if ( inc_loop_down(&y_mantMSDptr[(uintP)len],len) ) {
            /* carry created by rounding up */
            y_mantMSDptr[0] = bit(intDsize-1); /* mantissa := 10...0 */
            /* increment exponents: */
            if (++(TheLfloat(y)->expo) == (uint32)(LF_exp_high+1))
              fehler_overflow();
          }
        }
      }
      end_arith_call();
      RESTORE_NUM_STACK /* restore num_stack */
    }
    /* assure LF_exp_low <= exp <= LF_exp_high : */
    if (TheLfloat(y)->expo == LF_exp_low-1) {
      if (underflow_allowed()) {
        fehler_underflow();
      } else {
        encode_LF0(len, return); /* result 0.0 */
      }
    }
  }
  return y;
}

/* returns for a Long-Float x>=0 : (sqrt x), a LF.
 LF_sqrt_LF(x)
 can trigger GC
 method:
 x = 0.0 -> result 0.0
 result-sign := positive,
 result-exponent := ceiling(e/2),
 result-mantissa:
   Extend the mantissa (n digits) by n+2 nulldigits behind.
   if e is odd, shift it (or only the first n+1 digits)
     by 1 bit to the right.
   calculate the integer-root, a n+1-digit-number with a
     leading 1.
   Round the last digit away:
     bit 15 = 0 -> round off,
     bit 15 = 1, rest =0 and root exact -> round-to-even,
     else round up.
   When rounding overflow occurs, shift mantissa by 1 bit to the right
     and increment exponent. */
local maygc object LF_sqrt_LF (object x)
{
  var uintL uexp = TheLfloat(x)->expo;
  if (uexp==0)
    return x; /* x=0.0 -> 0.0 as result */
  var uintC len = Lfloat_length(x);
  /* create radicand: */
  var uintD* r_MSDptr;
  var uintD* r_LSDptr;
  var uintL r_len = 2*(uintL)len+2; /* length of the radicand */
  if ((intWCsize < 32) && (r_len > (uintL)(bitc(intWCsize)-1)))
    fehler_LF_toolong();
  {
    SAVE_NUM_STACK /* save num_stack */
    num_stack_need(r_len, r_MSDptr=,r_LSDptr=);
    if ((uexp & bit(0)) == (LF_exp_mid & bit(0))) { /* exponent even */
      var uintD* ptr =
        copy_loop_up(&TheLfloat(x)->data[0],r_MSDptr,len); /* copy n digits */
      clear_loop_up(ptr,len+2); /* append n+2 Nulldigits */
    } else { /* exponent odd */
      begin_arith_call();
      var uintD carry_rechts = /* copy n digits and shift by 1 bit to the right */
        shiftrightcopy_loop_up(&TheLfloat(x)->data[0],r_MSDptr,len,1,0);
      var uintD* ptr = &r_MSDptr[(uintP)len];
      *ptr++ = carry_rechts; /* append carry and */
      clear_loop_up(ptr,len+1); /* n+1 Nulldigits */
      end_arith_call();
    }
    /* Compute ((uexp - LF_exp_mid + 1) >> 1) + LF_exp_mid without risking
       uintL overflow. */
    uexp = ((uexp - ((LF_exp_mid - 1) & 1)) >> 1) - ((LF_exp_mid - 1) >> 1)
           + LF_exp_mid;
    /* allocate result: */
    var object y = allocate_lfloat(len,uexp,0);
    var uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
    /* calculate root: */
    var DS w;
    var bool exactp;
    UDS_sqrt(r_MSDptr,r_len,r_LSDptr, &w, exactp=);
    /* w is the integer-root, a n+1-digit-number. */
    copy_loop_up(w.MSDptr,y_mantMSDptr,len); /* copy NUDS to y */
    /* round: */
    if (((sintD)(w.LSDptr[-1]) >= 0) /* next bit =0 -> round off */
        || (((w.LSDptr[-1] & ((uintD)bit(intDsize-1)-1)) ==0) /* =1 and further bits >0 or rest >0 -> round up */
            && exactp
            /* round-to-even */
            && ((w.LSDptr[-2] & bit(0)) ==0))) {
      /* round off */
    } else { /* round up */
      if ( inc_loop_down(&y_mantMSDptr[(uintP)len],len) ) {
        /* carry by rounding up */
        y_mantMSDptr[0] = bit(intDsize-1); /* mantissa := 10...0 */
        (TheLfloat(y)->expo)++; /* increment exponent  */
      }
    }
    RESTORE_NUM_STACK /* restore num_stack */
    return y;
  }
}

/* LF_to_I(x) converts a Long-Float x, that represents an integer number,
 into an Integer.
 can trigger GC
 method:
 If x=0.0, result 0.
 Else (ASH sign*mantissa (e-16n)). */
local maygc object LF_to_I (object x)
{
  var uintL uexp = TheLfloat(x)->expo;
  if (uexp==0)
    return Fixnum_0; /* x=0.0 -> result 0 */
  /* turn mantissa into an Integer: */
  var uintD* MSDptr;
  var uintD* LSDptr;
  var uintC len = Lfloat_length(x);
  var uintC len1 = len+1; /* need 1 Digit more */
  if (uintWCoverflow(len1))
    fehler_LF_toolong();
  {
    SAVE_NUM_STACK /* save num_stack */
    num_stack_need(len1, MSDptr=,LSDptr=);
    copy_loop_up(&TheLfloat(x)->data[0],&MSDptr[1],len); /* copy mantissa */
    MSDptr[0] = 0; /* and additional nulldigit */
    /* mantissa is the UDS MSDptr/len1/LSDptr. */
    if (R_minusp(x))
      /* x<0 -> negate mantissa: */
      neg_loop_down(LSDptr,len1);
    /* sign*mantissa is the DS MSDptr/len1/LSDptr. */
    pushSTACK(DS_to_I(MSDptr,len1)); /* sign*mantissa as Integer */
    RESTORE_NUM_STACK /* restore num_stack */
  }
  /* form e-16n = uexp-LF_exp_mid-16n as Integer: */
  var uintL sub = LF_exp_mid + intDsize*(uintL)len;
  var object shiftcount = UL_UL_minus_I(uexp,sub);
  /* execute (ASH Vorzeichen*Mantisse (- e 16n)) : */
  return I_I_ash_I(popSTACK(),shiftcount);
}

/* I_to_LF(x,len,signal_overflow) converts an integer x to a long-float with
 len digits, and rounds thereby.
 can trigger GC
 method:
 x=0 -> result 0.0
 Memorize sign of x.
 x:=(abs x)
 exponent:=(integer-length x)
 let mantissa contain the most significant 16n bits of the Integer x
    (with the leading 16-(e mod 16) nullbits to be discarded).
 Round the further bits away:
   There are no more -> round off,
   next bit = 0 -> round off,
   next bit bit = 1 and rest =0 -> round-to-even,
   next bit bit = 1 and rest >0 -> round up.
 When rounding up: rounding overflow -> shift mantissa by 1 bit to the right
   and increment exponent. */
local maygc object I_to_LF (object x, uintC len, bool signal_overflow)
{
  if (eq(x,Fixnum_0)) {
    encode_LF0(len, return); /* x=0 -> result 0.0 */
  }
  var signean sign = R_sign(x); /* sign of x */
  if (!(sign==0))
    x = I_minus_I(x); /* take absolut value of x */
  var uintL exp = I_integer_length(x); /* (integer-length x) < intDsize*2^intWCsize */
  /* test, if exp <= LF_exp_high-LF_exp_mid : */
  if ((log2_intDsize+intWCsize < 32)
      && ((uintL)(intDsize*bitc(intWCsize)-1) <=
          (uintL)(LF_exp_high-LF_exp_mid))) {
    /* guarantees exp <= intDsize*2^intWCsize-1 <= LF_exp_high-LF_exp_mid */
  } else {
    if (!(exp <= (uintL)(LF_exp_high-LF_exp_mid))) {
      if (signal_overflow) fehler_overflow(); else return nullobj;
    }
  }
  /* build Long-Float: */
  pushSTACK(x);
  var object y = allocate_lfloat(len,exp+LF_exp_mid,sign);
  var uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
  var uintD* x_MSDptr;
  var uintC x_len;
  I_to_NDS_nocopy(popSTACK(), x_MSDptr=,x_len=,); /* form NDS for x, x_len>0 */
  /* shift x_MSDptr/x_len/.. by (exp mod 16) bits to the right and fill
     into y (particularly: only at most len digits): */
  var uintL shiftcount = exp % intDsize;
  /* the NDS begins with intDsize-shiftcount nullbits, then comes a 1. */
  begin_arith_call();
  if (x_len > len) {
    x_len -= 1+len;
    if (shiftcount>0) {
      var uintD carry_rechts =
        shiftrightcopy_loop_up(&x_MSDptr[1],y_mantMSDptr,len,shiftcount,x_MSDptr[0]);
      /* mantissa is filled. Round: */
      if (((sintD)carry_rechts >= 0) /* next bit =0 -> round off */
          || (((carry_rechts & ((uintD)bit(intDsize-1)-1)) ==0) /* =1, rest >0 -> round up */
              && !test_loop_up(&x_MSDptr[1+(uintP)len],x_len)
              /* round-to-even */
              && ((y_mantMSDptr[(uintP)len-1] & bit(0)) ==0)))
        goto ab; /* round off */
      else
        goto auf; /* round up */
    } else {
      copy_loop_up(&x_MSDptr[1],y_mantMSDptr,len);
      /* mantissa is filled. Round: */
      var uintD* ptr = &x_MSDptr[1+(uintP)len];
      if ((x_len==0) /* no more bits -> round off */
          || ((sintD)ptr[0] >= 0) /* next bit =0 -> round off */
          || (((ptr[0] & ((uintD)bit(intDsize-1)-1)) ==0) /* =1, rest >0 -> round up */
              && !test_loop_up(&ptr[1],x_len-1)
              /* round-to-even */
              && ((ptr[-1] & bit(0)) ==0)))
        goto ab; /* round off */
      else
        goto auf; /* round up */
    }
   auf: /* round up */
    if ( inc_loop_down(&y_mantMSDptr[(uintP)len],len) ) {
      /* carry due to rounding up */
      y_mantMSDptr[0] = bit(intDsize-1); /* mantissa := 10...0 */
      /* increment exponent: */
      if ((log2_intDsize+intWCsize < 32)
          && ((uintL)(intDsize*bitc(intWCsize)-1) <
              (uintL)(LF_exp_high-LF_exp_mid))) {
        /* guarantees exp < intDsize*2^intWCsize-1 <= LF_exp_high-LF_exp_mid */
        (TheLfloat(y)->expo)++; /* now, exp <= LF_exp_high-LF_exp_mid */
      } else {
        if (++(TheLfloat(y)->expo) == (uint32)(LF_exp_high+1)) {
          if (signal_overflow) fehler_overflow(); else return nullobj;
        }
      }
    }
   ab: /* round off */
    ;
  } else { /* x_len <= len */
    var uintD carry_rechts;
    len -= x_len;
    x_len -= 1;
    if (shiftcount>0) {
      carry_rechts = shiftrightcopy_loop_up(&x_MSDptr[1],y_mantMSDptr,x_len,shiftcount,x_MSDptr[0]);
    } else {
      copy_loop_up(&x_MSDptr[1],y_mantMSDptr,x_len); carry_rechts = 0;
    }
    var uintD* y_ptr = &y_mantMSDptr[x_len];
    *y_ptr++ = carry_rechts; /* carry as next digit */
    clear_loop_up(y_ptr,len); /* then len-x_len nulldigits */
  }
  end_arith_call();
  return y;
}

/* RA_to_LF(x,len,signal_overflow) converts a rational number x to a long-float
 with len digits and rounds thereby.
 can trigger GC
 method:
 x integer -> obvious.
 x = +/- a/b with Integers a,b>0:
   let k,m be chosen, so that
     2^(k-1) <= a < 2^k, 2^(m-1) <= b < 2^m.
   Then, 2^(k-m-1) < a/b < 2^(k-m+1).
   result-sign := sign of x.
   Calculate k=(integer-length a) and m=(integer-length b).
   result-exponent := k-m.
   result-mantissa:
     calculate floor(2^(-k+m+16n+1)*a/b) :
       When k-m>=16n+1, divide a by (ash b (k-m-16n-1)),
       when k-m<16n+1 divide (ash a (-k+m+16n+1)) by b.
     The first value is >=2^16n, <2^(16n+2).
     If it is >=2^(16n+1) , increase exponent by 1,
       round 2 bits away and shift by 2 bits to the right;
     if it is <2^(16n+1) ,
       round 1 bit away and shift by 1 bit to the right. */
local maygc object RA_to_LF (object x, uintC len, bool signal_overflow)
{
  if (RA_integerp(x))
    return I_to_LF(x,len,signal_overflow);
  /* x Ratio */
  pushSTACK(TheRatio(x)->rt_den); /* b */
  var signean sign = RT_sign(x); /* sign */
  x = TheRatio(x)->rt_num; /* +/- a */
  if (!(sign==0))
    x = I_minus_I(x); /* take absolute value, return a */
  pushSTACK(x);
  /* stack layout: b, a. */
  var sintL lendiff = I_integer_length(x) /* (integer-length a) */
    - I_integer_length(STACK_1); /* (integer-length b) */
  /* |lendiff| < intDsize*2^intWCsize. For LF-Exponenten there is a sintL
     available, so we need no test for Overflow or Underflow. */
  {
    var uintL difflimit = intDsize*(uintL)len + 1; /* 16n+1 */
    var object zaehler;
    var object nenner;
    if (lendiff > (sintL)difflimit) {
      /* 0 <= k-m-16n-1 < k < intDsize*2^intWCsize */
      nenner = I_I_ash_I(STACK_1,(log2_intDsize+intWCsize<=oint_data_len /* intDsize*2^intWCsize <= 2^oint_data_len ? */
                                  ? fixnum( (uintL)(lendiff - difflimit))
                                  : UL_to_I((uintL)(lendiff - difflimit))));
      zaehler = popSTACK(); /* a */
      skipSTACK(1);
    } else { /* 0 < -k+m+16n+1 <= m+1 + 16n < intDsize*2^intWCsize + intDsize*2^intWCsize */
      var object shiftcount = /* -k+m+16n+1 */
        (log2_intDsize+intWCsize+1<=oint_data_len /* 2*intDsize*2^intWCsize <= 2^oint_data_len ? */
         ? fixnum( (uintL)(difflimit - lendiff))
         : UL_to_I((uintL)(difflimit - lendiff)));
      zaehler = I_I_ash_I(popSTACK(),shiftcount); /* (ash a -k+m+16n+1) */
      nenner = popSTACK(); /* b */
    }
    /* execute division zaehler/nenner (engl. numerator/denominator): */
    I_I_divide_I_I(zaehler,nenner);
  }
  /* stack layout: q, r. */
  /* 2^16n <= q < 2^(16n+2), so q is Bignum with n+1 Digits. */
  var object y = allocate_lfloat(len,lendiff+LF_exp_mid,sign); /* new Long-Float */
  var uintD* y_mantMSDptr = &TheLfloat(y)->data[0];
  begin_arith_call();
  {
    var uintD* q_MSDptr = &TheBignum(STACK_1)->data[0];
    if (q_MSDptr[0] == 1) { /* first digit =1 or =2,3 ? */
      /* 2^16n <= q < 2^(16n+1), so 2^(k-m-1) < a/b < 2^(k-m). */
      /* fill mantissa with a shift loop by 1 bit to the right: */
      var uintD rounding_bit =
        shiftrightcopy_loop_up(&q_MSDptr[1],y_mantMSDptr,len,1,1);
      if ((rounding_bit == 0) /* shifted out bit =0 -> round off */
          || (eq(STACK_0,Fixnum_0) /* =1 and rest r > 0 -> round up */
              /* round-to-even */
              && ((y_mantMSDptr[(uintP)len-1] & bit(0)) ==0)))
        goto ab; /* round off */
      else
        goto auf; /* round up */
    } else {
      /* 2^(16n+1) <= q < 2^(16n+2), also 2^(k-m) < a/b < 2^(k-m+1). */
      /* fill mantissa with a shift loop by 2 bits to the right: */
      var uintD rounding_bits =
        shiftrightcopy_loop_up(&q_MSDptr[1],y_mantMSDptr,len,2,q_MSDptr[0]);
      (TheLfloat(y)->expo)++; /* increment exponent to k-m+1 */
      if (((sintD)rounding_bits >= 0) /* shifted out bit =0 -> round off */
          || (((rounding_bits & bit(intDsize-2)) ==0) /* =1 and next bit =1 or rest r > 0 -> round up */
              && eq(STACK_0,Fixnum_0)
              /* round-to-even */
              && ((y_mantMSDptr[(uintP)len-1] & bit(0)) ==0)))
        goto ab; /* round off */
      else
        goto auf; /* round up */
    }
  }
 auf: /* round up */
  if ( inc_loop_down(&y_mantMSDptr[(uintP)len],len) ) {
    /* carry due to rounding up */
    y_mantMSDptr[0] = bit(intDsize-1); /* mantissa := 10...0 */
    (TheLfloat(y)->expo)++; /* increment exponent */
  }
 ab: /* round off */
  end_arith_call();
  skipSTACK(2);
  return y;
}

