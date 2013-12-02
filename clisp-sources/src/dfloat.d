/* Basic Functions for Double-Floats */

/* Unpacking of a Double-Float: */
#ifdef intQsize
/* DF_decode(obj, zero_statement, sign=,exp=,mant=);
 decodes a double-float obj.
 If obj=0.0, then zero_statement is executed.
 else: signean sign = Sign (0 = +, -1 = -),
         sintWL exp = Exponent (with sign),
         uintQ mant = Mantissa (>= 2^DF_mant_len, < 2^(DF_mant_len+1)) */
  #define float_value_semhi  float_value
  #define DF_uexp(x)  (((x) >> DF_mant_len) & (bit(DF_exp_len)-1))
  #define DF_decode(obj, zero_statement, sign_zuweisung,exp_zuweisung,mant_zuweisung) do { \
    var dfloat _x = TheDfloat(obj)->float_value;                        \
    var uintWL uexp = DF_uexp(_x);                                      \
    if (uexp==0) {                                                      \
      zero_statement; /* e=0 -> number 0.0 */                           \
    } else {                                                            \
      exp_zuweisung (sintWL)(uexp - DF_exp_mid); /* exponent */         \
      unused (sign_zuweisung ((sint64)_x >> 63));  /* sign */           \
      mant_zuweisung (bit(DF_mant_len) | (_x & (bit(DF_mant_len)-1)));  \
    }                                                                   \
  } while(0)
#else
/* DF_decode2(obj, zero_statement, sign=,exp=,manthi=,mantlo=);
 decodes a double-float obj.
 If obj=0.0, then zero_statement is executed.
 else: signean sign = Sign (0 = +, -1 = -),
         sintWL exp = Exponent (with sign),
         uintL manthi,mantlo = Mantissa 2^32*manthi+mantlo
                              (>= 2^DF_mant_len, < 2^(DF_mant_len+1)) */
  #define float_value_semhi  float_value.semhi
  #define DF_uexp(semhi)  (((semhi) >> (DF_mant_len-32)) & (bit(DF_exp_len)-1))
  #define DF_decode2(obj,zero_statement,sign_zuweisung,exp_zuweisung,manthi_zuweisung,mantlo_zuweisung) do { \
    var uint32 semhi = TheDfloat(obj)->float_value.semhi;               \
    var uint32 mlo = TheDfloat(obj)->float_value.mlo;                   \
    var uintWL uexp = DF_uexp(semhi);                                   \
    if (uexp==0) {                                                      \
      zero_statement; /* e=0 -> number 0.0 */                           \
    } else {                                                            \
      exp_zuweisung (sintWL)(uexp - DF_exp_mid);             /* exponent */ \
      unused (sign_zuweisung sign_of_sint32((sint32)(semhi))); /* sign */ \
      manthi_zuweisung (bit(DF_mant_len-32) | (semhi & (bit(DF_mant_len-32)-1))); \
      mantlo_zuweisung mlo;                                             \
    }                                                                   \
  } while(0)
#endif

/* Encoding a Double-Float: */
#ifdef intQsize
/* encode_DF(sign,exp,mant, ergebnis=);
 returns a Double-Float.
 > signean sign: Sign, 0 for +, -1 for negative.
 > sintWL exp: Exponent
 > uintQ mant: Mantissa, should be >= 2^DF_mant_len and < 2^(DF_mant_len+1) .
 < object result: a Double-Float
 the Exponent is tested for Overflow/Underflow.
 can trigger GC */
 #define encode_DF(sign,exp,mant, erg_zuweisung) do {                   \
   if ((exp) < (sintWL)(DF_exp_low-DF_exp_mid)) {                       \
     if (underflow_allowed()) {                                         \
       fehler_underflow();                                              \
     } else {                                                           \
       erg_zuweisung DF_0;                                              \
     }                                                                  \
   } else                                                               \
     if ((exp) > (sintWL)(DF_exp_high-DF_exp_mid)) {                    \
       fehler_overflow();                                               \
     } else                                                             \
       erg_zuweisung allocate_dfloat                                    \
         (  ((sint64)(sign) & bit(63))                  /* Sign */      \
          | ((uint64)((exp)+DF_exp_mid) << DF_mant_len) /* Exponent */  \
          | ((uint64)(mant) & (bit(DF_mant_len)-1)));     /* Mantissa */ \
 } while(0)
#else
/* encode2_DF(sign,exp,manthi,mantlo, ergebnis=);
 returns a Double-Float.
 > signean sign: sign, 0 for +, -1 for negative.
 > sintWL exp: Exponent
 > uintL manthi,mantlo: Mantissa 2^32*manthi+mantlo,
                        should be >= 2^DF_mant_len and < 2^(DF_mant_len+1) .
 < object result: a Double-Float
 the Exponent is tested for Overflow/Underflow.
 can trigger GC */
 #define encode2_DF(sign,exp,manthi,mantlo, erg_zuweisung)  do {         \
   if ((exp) < (sintWL)(DF_exp_low-DF_exp_mid)) {                       \
     if (underflow_allowed()) {                                         \
       fehler_underflow();                                              \
     } else {                                                           \
       erg_zuweisung DF_0;                                              \
     }                                                                  \
   } else                                                               \
     if ((exp) > (sintWL)(DF_exp_high-DF_exp_mid)) {                    \
       fehler_overflow();                                               \
     } else                                                             \
       erg_zuweisung allocate_dfloat                                    \
         (  ((sint32)(sign) & bit(31))                       /* Sign */ \
          | ((uint32)((exp)+DF_exp_mid) << (DF_mant_len-32)) /* Exponent */ \
          | ((uint32)(manthi) & (bit(DF_mant_len-32)-1))     /* Mantissa */ \
            , mantlo);                                                  \
 } while(0)
#endif

#ifdef FAST_DOUBLE
/* Unpacking a Double: */
  #define DF_to_double(obj)  (TheDfloat(obj)->representation.machine_double)
/* Testing and Packing of a IEEE-Floats that is returned by the
 'double'-routines.
 Classification:
   1 <= e <= 2046 : normalized number
   e=0, m/=0: subnormal number
   e=0, m=0: signed 0.0
   e=2047, m=0: signed Infinity
   e=2047, m/=0: NaN
 specification of the possible special cases:
   maybe_overflow: Operation overflows, returns IEEE-Infinity
   maybe_subnormal: result very small, returns IEEE-subnormal number
   maybe_underflow: result very small and /=0, returns IEEE-Null
   maybe_divide_0: result undetermined, returns IEEE-Infinity
   maybe_nan: result undetermined, returns IEEE-NaN */
#ifdef intQsize
 #define double_to_DF(expr,ergebnis_zuweisung,maybe_overflow,maybe_subnormal,maybe_underflow,maybe_divide_0,maybe_nan)  do { \
   var dfloatjanus _erg; _erg.machine_double = (expr);                  \
   if ((_erg.eksplicit & ((uint64)bit(DF_exp_len+DF_mant_len)-bit(DF_mant_len))) == 0) { /* e=0 ? */ \
     if ((maybe_underflow                                               \
          || (maybe_subnormal && ((_erg.eksplicit << 1) != 0)))         \
         && underflow_allowed()) {                                      \
       fehler_underflow(); /* subnormal or even smaller -> Underflow */ \
     } else {                                                           \
       ergebnis_zuweisung DF_0; /* +/- 0.0 -> 0.0 */                    \
     }                                                                  \
   } else if ((maybe_overflow || maybe_divide_0)                        \
              && (((~_erg.eksplicit) & ((uint64)bit(DF_exp_len+DF_mant_len)-bit(DF_mant_len))) == 0)) { /* e=2047 ? */ \
     if (maybe_nan && ((_erg.eksplicit<<(64-DF_mant_len)) != 0)) {      \
       /* NaN, singularity -> "Division by 0" */                        \
       divide_0();                                                      \
     } else { /* Infinity */                                            \
       if (!maybe_overflow || maybe_divide_0)                           \
         divide_0(); /* Infinity, Division by 0 */                      \
       else                                                             \
         fehler_overflow(); /* Infinity, Overflow */                    \
     }                                                                  \
   } else {                                                             \
     ergebnis_zuweisung allocate_dfloat(_erg.eksplicit);                \
   }                                                                    \
 } while(0)
#else
 #define double_to_DF(expr,ergebnis_zuweisung,maybe_overflow,maybe_subnormal,maybe_underflow,maybe_divide_0,maybe_nan) do { \
   var dfloatjanus _erg; _erg.machine_double = (expr);                  \
   if ((_erg.eksplicit.semhi & ((uint32)bit(DF_exp_len+DF_mant_len-32)-bit(DF_mant_len-32))) == 0) { /* e=0 ? */ \
     if ((maybe_underflow                                               \
          || (maybe_subnormal                                           \
              && !(((_erg.eksplicit.semhi << 1) == 0) && (_erg.eksplicit.mlo == 0)))) \
         && underflow_allowed()) {                                      \
       fehler_underflow(); /* subnormal or even smaller -> Underflow */ \
     } else {                                                           \
       ergebnis_zuweisung DF_0; /* +/- 0.0 -> 0.0 */                    \
     }                                                                  \
   } else if ((maybe_overflow || maybe_divide_0)                        \
              && (((~_erg.eksplicit.semhi) & ((uint32)bit(DF_exp_len+DF_mant_len-32)-bit(DF_mant_len-32))) == 0)) { /* e=2047 ? */ \
     if (maybe_nan && !(((_erg.eksplicit.semhi<<(64-DF_mant_len)) == 0) && (_erg.eksplicit.mlo==0))) { \
       /* NaN, Singularity -> "Division by 0" */                        \
       divide_0();                                                      \
     } else { /* Infinity */                                            \
       if (!maybe_overflow || maybe_divide_0)                           \
         divide_0(); /* Infinity, Division by 0 */                      \
       else                                                             \
         fehler_overflow(); /* Infinity, Overflow */                    \
     }                                                                  \
   } else {                                                             \
     ergebnis_zuweisung allocate_dfloat(_erg.eksplicit.semhi,_erg.eksplicit.mlo); \
   }                                                                    \
 } while(0)
#endif
#endif

/* DF_zerop(x) determines, if a Double-Float x is  = 0.0 . */
  # define DF_zerop(x)  (DF_uexp(TheDfloat(x)->float_value_semhi) == 0)
  #define DF_zerop(x)  (TheDfloat(x)->float_value_semhi == 0)

/* Returns for a Double-Float x : (ftruncate x), a DF.
 DF_ftruncate_DF(x)
 x is rounded towards 0, to the next integer number.
 can trigger GC */
local maygc object DF_ftruncate_DF (object x);
/* Method:
 x = 0.0 or e<=0 -> result 0.0
 1<=e<=52 -> set last (53-e) bits of the mantissa to 0 ,
             keep exponent and sign
 e>=53 -> result x */
#ifdef intQsize
local maygc object DF_ftruncate_DF (object x) {
  var dfloat x_ = TheDfloat(x)->float_value;
  var uintWL uexp = DF_uexp(x_); /* e + DF_exp_mid */
  if (uexp <= DF_exp_mid) { /* 0.0 or e<=0 ? */
    return DF_0;
  } else {
    if (uexp > DF_exp_mid+DF_mant_len) /* e > 52 ? */
      return x;
    else /* 1<=e<=52 */
      return allocate_dfloat
        ( x_ & /* bitmask: bits 52-e..0 deleted, all others set */
          ~(bit(DF_mant_len+1+DF_exp_mid-uexp)-1));
  }
}
#else
local maygc object DF_ftruncate_DF (object x) {
  var uint32 semhi = TheDfloat(x)->float_value.semhi;
  var uint32 mlo = TheDfloat(x)->float_value.mlo;
  var uintWL uexp = DF_uexp(semhi); /* e + DF_exp_mid */
  if (uexp <= DF_exp_mid) { /* 0.0 or e<=0 ? */
    return DF_0;
  } else {
    if (uexp > DF_exp_mid+DF_mant_len) { /* e > 52 ? */
      return x;
    } else { /* 1<=e<=52 */
      if (uexp > DF_exp_mid+DF_mant_len+1-32) /* e > 21 ? */
        return allocate_dfloat
          ( semhi,
            mlo & /* bitmask: bits 52-e..0 deleted, all others set */
            ~(bit(DF_mant_len+1+DF_exp_mid-uexp)-1));
      else
        return allocate_dfloat
          ( semhi & /* bitmask: bits 20-e..0 deleted, all others set */
            ~(bit(DF_mant_len+1+DF_exp_mid-32-uexp)-1),
            0);
    }
  }
}
#endif

/* Returns for a double-float x : (futruncate x), a DF.
 DF_futruncate_DF(x)
 x is rounded away from 0 to the the next integer number.
 can trigger GC */
local maygc object DF_futruncate_DF (object x);
/* method:
 x = 0.0 -> result 0.0
 e<=0 -> result 1.0 or -1.0, according to sign of x.
 1<=e<=52 -> take the last (53-e) bits of x .
             Are they all =0 -> result x.
             Else set them all and then increase the last digit by 1.
             No Overflow of the 52 bits -> done.
             Else (result a power of two): Mantissa := .1000...000,
               e:=e+1. (test for overflow because of e<=53 superfluous)
 e>=53 -> result x. */
#ifdef intQsize
local maygc object DF_futruncate_DF (object x) {
  var dfloat x_ = TheDfloat(x)->float_value;
  var uintWL uexp = DF_uexp(x_); /* e + DF_exp_mid */
  if (uexp==0) /* 0.0 ? */
    return x;
  if (uexp <= DF_exp_mid) { /* e<=0 ? */
    /* set exponent to 1, mantissa to .1000...000 . */
    return ((x_ & bit(63))==0 ? DF_1 : DF_minus1);
  } else {
    if (uexp > DF_exp_mid+DF_mant_len) { /* e > 52 ? */
      return x;
    } else {
      var uint64 mask = /* bitmask: bits 52-e..0 set, all others deleted */
        bit(DF_mant_len+1+DF_exp_mid-uexp)-1;
      if ((x_ & mask)==0) /* all these bits =0 ? */
        return x;
      return allocate_dfloat
        ((x_ | mask) /* set all these bits */
         + 1); /* increase last digit, thereby poss. increment exponent */
    }
  }
}
#else
local maygc object DF_futruncate_DF (object x) {
  var uint32 semhi = TheDfloat(x)->float_value.semhi;
  var uint32 mlo = TheDfloat(x)->float_value.mlo;
  var uintWL uexp = DF_uexp(semhi); /* e + DF_exp_mid */
  if (uexp==0) /* 0.0 ? */
    return x;
  if (uexp <= DF_exp_mid) { /* e<=0 ? */
    /* set exponent to 1, mantissa to .1000...000 . */
    return ((semhi & bit(31))==0 ? DF_1 : DF_minus1);
  } else {
    if (uexp > DF_exp_mid+DF_mant_len) { /* e > 52 ? */
      return x;
    } else {
      if (uexp > DF_exp_mid+DF_mant_len+1-32) { /* e > 21 ? */
        var uint32 mask = /* bitmask: bits 52-e..0 set, all others deleted */
          bit(DF_mant_len+1+DF_exp_mid-uexp)-1;
        if ((mlo & mask)==0) /* all these bits =0 ? */
          return x;
        mlo = (mlo | mask) /* set all these bits */
          + 1; /* increase last digit, */
        if (mlo==0)
          semhi += 1; /* thereby poss. increment exponent */
        return allocate_dfloat(semhi,mlo);
      } else {
        var uint32 mask = /* bitmask: bits 20-e..0 set, all others deleted */
          bit(DF_mant_len+1+DF_exp_mid-32-uexp)-1;
        if ((mlo==0) && ((semhi & mask)==0)) /* all these bits and mlo =0 ? */
          return x;
        return allocate_dfloat
          ((semhi | mask) /* set all these bits */
           + 1, /* increase last digit, thereby poss. increment exponent */
           0);
      }
    }
  }
}
#endif

/* Returns for a Double-Float x : (fround x), a DF.
 DF_fround_DF(x)
 x is rounded to the next integer number.
 can trigger GC */
local maygc object DF_fround_DF (object x);
/* method:
 x = 0.0 or e<0 -> result 0.0
 0<=e<=52 -> round away last (53-e) bits of the mantissa,
             keep exponent and sign.
 e>52 -> result x */
#ifdef intQsize
local maygc object DF_fround_DF (object x) {
  var dfloat x_ = TheDfloat(x)->float_value;
  var uintWL uexp = DF_uexp(x_); /* e + DF_exp_mid */
  if (uexp < DF_exp_mid) { /* x = 0.0 or e<0 ? */
    return DF_0;
  } else {
    if (uexp > DF_exp_mid+DF_mant_len) { /* e > 52 ? */
      return x;
    } else {
      if (uexp > DF_exp_mid+1) { /* e>1 ? */
        var uint64 bitmask = /* bitmask: bit 52-e set, all others deleted */
          bit(DF_mant_len+DF_exp_mid-uexp);
        var uint64 mask = /* bitmask: bits 51-e..0 set, all others deleted */
          bitmask-1;
        if (((x_ & bitmask) ==0) /* bit 52-e =0 -> round off */
            || (((x_ & mask) ==0) /* bit 52-e =1 and bits 51-e..0 >0 -> round up */
                /* round-to-even, according to bit 53-e : */
                && ((x_ & (bitmask<<1)) ==0))) {
          /* round off */
          mask |= bitmask; /* bitmask: bits 52-e..0 set, all others deleted */
          return allocate_dfloat( x_ & ~mask );
        } else { /* round up */
          return allocate_dfloat
            ((x_ | mask) /* set all these bits 51-e..0 (bit 52-e already set) */
             + 1); /* increase last digit, thereby poss. increment exponent */
        }
      } else if (uexp == DF_exp_mid+1) { /* e=1 ? */
        /* like with 1 < e <= 52, only that bit 53-e is always set. */
        if ((x_ & bit(DF_mant_len-1)) ==0) /* bit 52-e =0 -> round off */
          /* round off */
          return allocate_dfloat( x_ & ~(bit(DF_mant_len)-1) );
        else /* round up */
          return allocate_dfloat
            ((x_ | (bit(DF_mant_len)-1)) /* set all these bits 52-e..0 */
             + 1); /* increase last digit, thereby poss. increment exponent */
      } else { /* e=0 ? */
        /* like with 1 < e <= 52, only that bit 52-e is always set
           and bit 53-e is always deleted. */
        if ((x_ & (bit(DF_mant_len)-1)) ==0) /* round off from +-0.5 zu 0.0 */
          return DF_0;
        else /* round up */
          return allocate_dfloat
            ((x_ | (bit(DF_mant_len)-1)) /* set all bits 51-e..0 */
             + 1); /* increase last digit, thereby increment exponent */
      }
    }
  }
}
#else
local maygc object DF_fround_DF (object x) {
  var uint32 semhi = TheDfloat(x)->float_value.semhi;
  var uint32 mlo = TheDfloat(x)->float_value.mlo;
  var uintWL uexp = DF_uexp(semhi); /* e + DF_exp_mid */
  if (uexp < DF_exp_mid) { /* x = 0.0 or e<0 ? */
    return DF_0;
  } else {
    if (uexp > DF_exp_mid+DF_mant_len) { /* e > 52 ? */
      return x;
    } else {
      if (uexp > DF_exp_mid+1) { /* e>1 ? */
        if (uexp > DF_exp_mid+DF_mant_len-32) { /* e > 20 ? */
          var uint32 bitmask = /* bitmask: bit 52-e set, all others deleted */
            bit(DF_mant_len+DF_exp_mid-uexp);
          var uint32 mask = /* bitmask: bits 51-e..0 set, all others deleted */
            bitmask-1;
          if (((mlo & bitmask) ==0) /* bit 52-e =0 -> round off */
              || (((mlo & mask) ==0) /* bit 52-e =1 and bits 51-e..0 >0 -> round up */
                  /* round-to-even, according to bit 53-e : */
                  && (((bitmask<<1) == 0) /* e=21 ? */
                      ? ((semhi & bit(0)) ==0)
                      : ((mlo & (bitmask<<1)) ==0)))) { /* round off */
            mask |= bitmask; /* bitmask: bits 52-e..0 set, all others deleted */
            return allocate_dfloat(semhi, mlo & ~mask );
          } else { /* round up */
            mlo = (mlo | mask) /* set all these bits 51-e..0 (bit 52-e already set) */
              + 1; /* increase last digit, */
            if (mlo==0)
              semhi += 1; /* thereby poss. increment exponent */
            return allocate_dfloat(semhi,mlo);
          }
        } else {
          var uint32 bitmask = /* bitmask: bit 20-e set, all others deleted */
            bit(DF_mant_len+DF_exp_mid-32-uexp);
          var uint32 mask = /* bitmask: bits 19-e..0 set, all others deleted */
            bitmask-1;
          if (((semhi & bitmask) ==0) /* bit 52-e =0 -> round off */
              || ((mlo==0) && ((semhi & mask) ==0) /* bit 52-e =1 and bits 51-e..0 >0 -> round up */
                  /* round-to-even, according to bit 53-e : */
                  && ((semhi & (bitmask<<1)) ==0))) { /* round off */
            mask |= bitmask; /* bitmask: bits 20-e..0 set, all others deleted */
            return allocate_dfloat( semhi & ~mask, 0 );
          } else { /* round up */
            return allocate_dfloat
              ((semhi | mask) /* set all these bits 19-e..0 (bit 20-e already set) */
               + 1, /* increase last digit, thereby poss. increment exponent */
               0);
          }
        }
      } else if (uexp == DF_exp_mid+1) { /* e=1 ? */
        /* like with 1 < e <= 20, only that bit 53-e is always set. */
        if ((semhi & bit(DF_mant_len-32-1)) ==0) /* bit 52-e =0 -> round off */
          /* round off */
          return allocate_dfloat( semhi & ~(bit(DF_mant_len-32)-1) , 0 );
        else /* round up */
          return allocate_dfloat
            ((semhi | (bit(DF_mant_len-32)-1)) /* set all these bits 52-e..0 */
             + 1, /* increase last digit, thereby poss. increment exponent */
             0);
      } else { /* e=0 ? */
        /* like with 1 < e <= 20, only that bit 52-e is always set */
        /* and bit 53-e is always deleted. */
        if ((mlo==0) && ((semhi & (bit(DF_mant_len-32)-1)) ==0))
          /* round off from +-0.5 to 0.0 */
          return DF_0;
        else /* round up */
          return allocate_dfloat
            ((semhi | (bit(DF_mant_len-32)-1)) /* set all bits 51-e..0 */
             + 1, /* increase last digit, thereby increment exponent */
             0);
      }
    }
  }
}
#endif

/* Returns for a Double-Float x : (- x), a DF.
 DF_minus_DF(x)
 can trigger GC */
local maygc object DF_minus_DF (object x);
/* method:
 if x=0.0, done. Else reverse sign bit. */
#ifdef intQsize
local maygc object DF_minus_DF (object x) {
  var dfloat x_ = TheDfloat(x)->float_value;
  return (DF_uexp(x_) == 0
          ? x
          : allocate_dfloat( x_ ^ bit(63) ));
}
#else
local maygc object DF_minus_DF (object x) {
  var uint32 semhi = TheDfloat(x)->float_value.semhi;
  var uint32 mlo = TheDfloat(x)->float_value.mlo;
  return (DF_uexp(semhi) == 0
          ? x
          : allocate_dfloat( semhi ^ bit(31), mlo ));
}
#endif

/* DF_DF_comp(x,y) compares two Double-Floats x and y.
 result: 0 if x=y, +1 if x>y, -1 if x<y. */
local signean DF_DF_comp (object x, object y);
/* method:
 x and y have different sign ->
    x < 0 -> x < y
    x >= 0 -> x > y
 x and y have equal sign ->
    x >=0 -> compare x and y (the right 53 bits)
    x <0 -> compare y and x (the right 53 bits) */
#ifdef intQsize
local signean DF_DF_comp (object x, object y) {
  var dfloat x_ = TheDfloat(x)->float_value;
  var dfloat y_ = TheDfloat(y)->float_value;
  if ((sint64)y_ >= 0) { /* y>=0 */
    if ((sint64)x_ >= 0) { /* y>=0, x>=0 */
      if (x_ < y_)
        return signean_minus; /* x<y */
      if (x_ > y_)
        return signean_plus; /* x>y */
      return signean_null;
    } else { /* y>=0, x<0 */
      return signean_minus; /* x<y */
    }
  } else {
    if ((sint64)x_ >= 0) { /* y<0, x>=0 */
      return signean_plus; /* x>y */
    } else { /* y<0, x<0 */
      if (x_ > y_)
        return signean_minus; /* |x|>|y| -> x<y */
      if (x_ < y_)
        return signean_plus; /* |x|<|y| -> x>y */
      return signean_null;
    }
  }
}
#else
local signean DF_DF_comp (object x, object y) {
  var uint32 x_semhi = TheDfloat(x)->float_value.semhi;
  var uint32 y_semhi = TheDfloat(y)->float_value.semhi;
  var uint32 x_mlo = TheDfloat(x)->float_value.mlo;
  var uint32 y_mlo = TheDfloat(y)->float_value.mlo;
  if ((sint32)y_semhi >= 0) { /* y>=0 */
    if ((sint32)x_semhi >= 0) { /* y>=0, x>=0 */
      if (x_semhi < y_semhi)
        return signean_minus; /* x<y */
      if (x_semhi > y_semhi)
        return signean_plus; /* x>y */
      if (x_mlo < y_mlo)
        return signean_minus; /* x<y */
      if (x_mlo > y_mlo)
        return signean_plus; /* x>y */
      return signean_null;
    } else { /* y>=0, x<0 */
      return signean_minus; /* x<y */
    }
  } else {
    if ((sint32)x_semhi >= 0) { /* y<0, x>=0 */
      return signean_plus; /* x>y */
    } else { /* y<0, x<0 */
      if (x_semhi > y_semhi)
        return signean_minus; /* |x|>|y| -> x<y */
      if (x_semhi < y_semhi)
        return signean_plus; /* |x|<|y| -> x>y */
      if (x_mlo > y_mlo)
        return signean_minus; /* |x|>|y| -> x<y */
      if (x_mlo < y_mlo)
        return signean_plus; /* |x|<|y| -> x>y */
      return signean_null;
    }
  }
}
#endif

/* Returns for two Double-Float x and y : (+ x y), a DF.
 DF_DF_plus_DF(x,y)
 can trigger GC */
  local maygc object DF_DF_plus_DF (object x, object y);
/* method (according to [Knuth, II, Seminumerical Algorithms, section 4.2.1., p.200]):
 x1=0.0 -> result x2.
 x2=0.0 -> result x1.
 if e1<e2, swap x1 and x2.
 Thus, e1 >= e2.
 if e1 - e2 >= 52 + 3, result x1.
 Shift both Mantissas by 3 bits to the left (preparation of rounding:
   for e1-e2=0,1 no rounding is needed, for e1-e2>1, the exponent of the
   result =e1-1, =e1 or =e1+1. So I need on protection bit 1 and two
   rounding bits: 00 exactly, 01 1. half, 10 exact middle, 11 2. half.)
 Shift the mantissa of x2 by e0-e1 bits to the right. (Thereby do the
 rounding: Bit 0 is the logical OR of the bits 0,-1,-2,...)
 If x1,x2 have the same sign: Add it to the mantissa of x1.
 If x1,x2 have different sign: Subtract it from the
   mantissa of x1. <0 -> (e1=e2) swap the signs, negate.
                    =0 -> result 0.0
 Exponent is e1.
 Normalize, done. */
#ifdef FAST_DOUBLE
local maygc object DF_DF_plus_DF (object x1, object x2) {
  double_to_DF(DF_to_double(x1) + DF_to_double(x2), return ,
               true, true, /* catch Overflow and subnormal number */
               false, /* no Underflow with result +/- 0.0 possible */
               /* (according to definition of subnormal numbers) */
               false, false); /* no singularity, no NaN as result possible */
}
#elif defined(intQsize)
local maygc object DF_DF_plus_DF (object x1, object x2) {
  /* unpack x1,x2: */
  var signean sign1;
  var sintWL exp1;
  var uint64 mant1;
  var signean sign2;
  var sintWL exp2;
  var uint64 mant2;
  DF_decode(x1, return x2, sign1=,exp1=,mant1=);
  DF_decode(x2, return x1, sign2=,exp2=,mant2=);
  if (exp1 < exp2) {
    swap(object,  x1   ,x2   );
    swap(signean, sign1,sign2);
    swap(sintWL,  exp1 ,exp2 );
    swap(uint64,   mant1,mant2);
  }
  /* Now: exp1>=exp2. */
  var uintL expdiff = exp1 - exp2; /* exponent difference */
  if (expdiff >= DF_mant_len+3) /* >= 52+3 ? */
    return x1;
  mant1 = mant1 << 3; mant2 = mant2 << 3;
  /* Now, 2^(DF_mant_len+3) <= mant1,mant2 < 2^(DF_mant_len+4). */
  {
    var uint64 mant2_last = mant2 & (bit(expdiff)-1); /* last expdiff bits of mant2 */
    mant2 = mant2 >> expdiff; if (mant2_last!=0) { mant2 |= bit(0); }
  }
  /* mant2 = mantissa of x2 shifted to the right by expdiff bits and rounded */
  if (sign1!=sign2) { /* different signs -> subtract mantissas */
    if (mant1 > mant2) {
      mant1 = mant1 - mant2;
      goto norm_2;
    }
    if (mant1 == mant2) /* result 0 ? */
      return DF_0;
    /* negative subtraction result */
    mant1 = mant2 - mant1; sign1 = sign2; goto norm_2;
  } else { /* same sig -> add mantissas */
    mant1 = mant1 + mant2;
  }
  /* mant1 = result-mantissa >0, sign1 = result-sign,
   exp1 = result-exponent.
   aside from that: for expdiff=0,1 the two last bits of mant1 are null,
   with expdiff>=2  mant1 is >= 2^(DF_mant_len+2).
   mant1 is always < 2^(DF_mant_len+5). (Hence, the 2 rounding bits
   are shifted by at most one position to the left afterwards.)
   [Knuth, p.201, slightly modified:
     N1. m>=1 -> goto N4.
     N2. [here m<1] m>=1/2 -> goto N5.
         N3. m:=2*m, e:=e-1, goto N2.
     N4. [here 1<=m<2] m:=m/2, e:=e+1.
     N5. [here 1/2<=m<1] round m to 53 Bits behind the dot.
         if thus m became =1 , set m:=m/2, e:=e+1.
   ]
   m=mant1/2^(DF_mant_len+4),
   at step N5: m=mant1/2^(DF_mant_len+1). */
 norm_1: /* [Knuth, p.201, step N1] */
  if (mant1 >= bit(DF_mant_len+4))
    goto norm_4;
 norm_2: /* [Knuth, p.201, step N2] here is mant1 < 2^(DF_mant_len+4) */
  if (mant1 >= bit(DF_mant_len+3))
    goto norm_5;
  /* [Knuth, p.201, step N3] */
  mant1 = mant1 << 1; exp1 = exp1-1; /* shift mantissa left */
  goto norm_2;
 norm_4: /* [Knuth, p.201, step N4] */
  /* here is 2^(DF_mant_len+4) <= mant1 < 2^(DF_mant_len+5) */
  exp1 = exp1+1;
  mant1 = (mant1>>1) | (mant1 & bit(0)); /* shift mantissa right */
 norm_5: /* [Knuth, p.201, step N5]
            here is 2^(DF_mant_len+3) <= mant1 < 2^(DF_mant_len+4)
            round to DF_mant_len real mantissa bits, i.e. round away the
            right 3 bits, and thereby shift mant1 by 3 bits to the right: */
  {
    var uint64 rounding_bits = mant1 & (bit(3)-1);
    mant1 = mant1 >> 3;
    if ( (rounding_bits < bit(2)) /* 000,001,010,011 are rounded off */
         || ( (rounding_bits == bit(2)) /* 100 (exactly half-numbered) */
              && ((mant1 & bit(0)) ==0))) { /* -> round-to-even */
      /* round off */
    } else { /* round up */
      mant1 = mant1+1;
      if (mant1 >= bit(DF_mant_len+1)) {
        /* On overflow during the rounding, shift again to the right
           (rounding is here superfluous): */
        mant1 = mant1>>1; exp1 = exp1+1; /* shift mantissa right */
      }
    }
  }/* rounding done */
  encode_DF(sign1,exp1,mant1, return);
}
#else
local maygc object DF_DF_plus_DF (object x1, object x2) {
  /* unpack x1,x2: */
  var signean sign1;
  var sintWL exp1;
  var uintL manthi1;
  var uintL mantlo1;
  var signean sign2;
  var sintWL exp2;
  var uintL manthi2;
  var uintL mantlo2;
  DF_decode2(x1, return x2, sign1=,exp1=,manthi1=,mantlo1=);
  DF_decode2(x2, return x1, sign2=,exp2=,manthi2=,mantlo2=);
  if (exp1 < exp2) {
    swap(object,  x1   ,x2   );
    swap(signean, sign1,sign2);
    swap(sintWL,  exp1 ,exp2 );
    swap(uintL,   manthi1,manthi2);
    swap(uintL,   mantlo1,mantlo2);
  }
  /* now, exp1>=exp2. */
  var uintL expdiff = exp1 - exp2; /* exponent difference */
  if (expdiff >= DF_mant_len+3) /* >= 52+3 ? */
    return x1;
  manthi1 = (manthi1 << 3) | (mantlo1 >> (32-3)); mantlo1 = mantlo1 << 3;
  manthi2 = (manthi2 << 3) | (mantlo2 >> (32-3)); mantlo2 = mantlo2 << 3;
  /* now, 2^(DF_mant_len+3) <= mant1,mant2 < 2^(DF_mant_len+4). */
  if (expdiff<32) {
    if (expdiff!=0) {
      var uintL mant2_last = mantlo2 & (bit(expdiff)-1); /* last expdiff bits of mant2 */
      mantlo2 = (mantlo2 >> expdiff) | (manthi2 << (32-expdiff));
      manthi2 = manthi2 >> expdiff;
      if (mant2_last!=0)
        mantlo2 |= bit(0);
    }
  } else {
    var uintL mant2_last = (manthi2 & (bit(expdiff-32)-1)) | mantlo2; /* last expdiff bits of mant2 */
    mantlo2 = manthi2 >> (expdiff-32); manthi2 = 0;
    if (mant2_last!=0)
      mantlo2 |= bit(0);
  }
  /* mant2 = mantissa of x2 right-shifted by expdiff bits and rounded */
  if (sign1!=sign2) { /* different signs -> subtract mantissas */
    if (manthi1 > manthi2) {
      manthi1 = manthi1 - manthi2;
      if (mantlo1 < mantlo2)
        manthi1 -= 1;
      mantlo1 = mantlo1 - mantlo2;
      goto norm_2;
    }
    if (manthi1 == manthi2) {
      if (mantlo1 > mantlo2) {
        manthi1 = 0; mantlo1 = mantlo1 - mantlo2;
        goto norm_2;
      }
      if (mantlo1 == mantlo2) /* result 0 ? */
        return DF_0;
    }
    /*  ((manthi1 < manthi2) || ((manthi1 == manthi2) && (mantlo1 < mantlo2)))
        negative subtraction result */
    manthi1 = manthi2 - manthi1;
    if (mantlo2 < mantlo1)
      manthi1 -= 1;
    mantlo1 = mantlo2 - mantlo1;
    sign1 = sign2;
    goto norm_2;
  } else { /* same signs -> add mantissas */
    manthi1 = manthi1 + manthi2;
    if ((mantlo1 = mantlo1 + mantlo2) < mantlo2)
      manthi1 += 1;
  }
  /* mant1 = result-mantissa >0, sign1 = result-sign,
   exp1 = result-exponent.
   moreover: On expdiff=0,1, the last two bits of mant1 are Null,
   On expdiff>=2, mant1 >= 2^(DF_mant_len+2).
   Always, mant1 < 2^(DF_mant_len+5). (Hence, the 2 rounding bits
   are shifted by at most one position to the left afterwards.)
   [Knuth, p.201, slightly modified:
     N1. m>=1 -> goto N4.
     N2. [here m<1] m>=1/2 -> goto N5.
         N3. m:=2*m, e:=e-1, goto N2.
     N4. [here 1<=m<2] m:=m/2, e:=e+1.
     N5. [here 1/2<=m<1] round m to 53 Bits behind the dot.
         if thus m became =1 , set m:=m/2, e:=e+1.
   ]
   m=mant1/2^(DF_mant_len+4),
   at step N5,  m=mant1/2^(DF_mant_len+1). */
 norm_1: /* [Knuth, p.201, step N1] */
  if (manthi1 >= bit(DF_mant_len-32+4))
    goto norm_4;
 norm_2: /* [Knuth, p.201, step N2] here, mant1 < 2^(DF_mant_len+4) */
  if (manthi1 >= bit(DF_mant_len-32+3))
    goto norm_5;
  /* [Knuth, p.201, step N3] */
  manthi1 = (manthi1 << 1) | (mantlo1 >> 31); /* shift mantissa left */
  mantlo1 = mantlo1 << 1;
  exp1 = exp1-1;
  goto norm_2;
 norm_4: /* [Knuth, p.201, step N4]
            here, 2^(DF_mant_len+4) <= mant1 < 2^(DF_mant_len+5) */
  exp1 = exp1+1;
  mantlo1 = (mantlo1 >> 1) | (manthi1 << 31) | (mantlo1 & bit(0)); /* shift mantissa right */
  manthi1 = (manthi1 >> 1);
 norm_5: /* [Knuth, p.201, step N5]
            here, 2^(DF_mant_len+3) <= mant1 < 2^(DF_mant_len+4)
       round to DF_mant_len real mantissa bits, i.e. round away the
       right 3 bits, and thereby shift mant1 by 3 bits to the right: */
  {
    var uintL rounding_bits = mantlo1 & (bit(3)-1);
    mantlo1 = (mantlo1 >> 3) | (manthi1 << (32-3)); manthi1 = manthi1 >> 3;
    if ( (rounding_bits < bit(2)) /* 000,001,010,011 are rounded off */
         || ( (rounding_bits == bit(2)) /* 100 (exactly half-numbered) */
              && ((mantlo1 & bit(0)) ==0))) { /* -> round-to-even */
      /* round off */
    } else { /* round up */
      mantlo1 = mantlo1+1;
      if (mantlo1==0) {
        manthi1 = manthi1+1;
        if (manthi1 >= bit(DF_mant_len-32+1)) {
          /* On overflow during the rounding, shift again to the right
             (rounding is here superfluous): */
          manthi1 = manthi1>>1; exp1 = exp1+1; /* shift mantissa right */
        }
      }
    }
  }/* rounding done */
  encode2_DF(sign1,exp1,manthi1,mantlo1, return);
}
#endif

/* Returns for two Double-Float x and y : (- x y), a DF.
 DF_DF_minus_DF(x,y)
 can trigger GC */
local maygc object DF_DF_minus_DF (object x, object y);
/* method:
 (- x1 x2) = (+ x1 (- x2)) */
#ifdef FAST_DOUBLE
local maygc object DF_DF_minus_DF (object x1, object x2) {
  double_to_DF(DF_to_double(x1) - DF_to_double(x2), return ,
               true, true, /* catch Overflow and subnormal number */
               false, /* no Underflow with result +/- 0.0 possible */
               /* (according to definition of subnormal numbers) */
               false, false); /* no singularity, no NaN as result possible */
}
#elif defined(intQsize)
local maygc object DF_DF_minus_DF (object x1, object x2) {
  var dfloat x2_ = TheDfloat(x2)->float_value;
  if (DF_uexp(x2_) == 0) {
    return x1;
  } else {
    pushSTACK(x1);
    x2 = allocate_dfloat(x2_ ^ bit(63));
    return DF_DF_plus_DF(popSTACK(),x2);
  }
}
#else
local maygc object DF_DF_minus_DF (object x1, object x2) {
  var uint32 x2_semhi = TheDfloat(x2)->float_value.semhi;
  var uint32 x2_mlo = TheDfloat(x2)->float_value.mlo;
  if (DF_uexp(x2_semhi) == 0) {
    return x1;
  } else {
    pushSTACK(x1);
    x2 = allocate_dfloat(x2_semhi ^ bit(31), x2_mlo);
    return DF_DF_plus_DF(popSTACK(),x2);
  }
}
#endif

/* Returns for two Double-Float x and y : (* x y), a DF.
 DF_DF_mal_DF(x,y)
 can trigger GC */
local maygc object DF_DF_mal_DF (object x, object y);
/* method:
 If x1=0.0 or x2=0.0 -> result 0.0
 Else: result-sign = sign of x1 xor sign of x2.
        result-exponent = sum of exponents of x1 and x2.
        result-mantissa = product of mantissas of x1 and x2, rounded:
          2^-53 * mant1  *  2^-53 * mant2  =  2^-106 * (mant1*mant2),
          the parenthesis is >=2^104, <=(2^53-1)^2<2^106 .
          if the parenthesis is >=2^105 , shift by 53 bits to the right and
            round: If bit 52 is Null, round off; if bit 52 is One and
            bits 51..0 are all Null, round-to-even; else, round up.
          if the parenthesis is <2^105 , shift by 52 bits to the right and
            round: If bit 51 is Null, round off; if bit 51 is One and
            bits 50..0 are all Null, round-to-even; else, round up. After
            rounding up: If =2^53, shift by 1 bit to the right. Else,
            decrement exponent by 1. */
#ifdef FAST_DOUBLE
local maygc object DF_DF_mal_DF (object x1, object x2) {
  double_to_DF(DF_to_double(x1) * DF_to_double(x2), return ,
               true, true, /* catch Overflow and subnormal number */
               !(DF_zerop(x1) || DF_zerop(x2)), /* a result +/- 0.0 */
               /* is exactly then really an Underflow */
               false, false); /* no singularity, no NaN possible as result */
}
#else
local maygc object DF_DF_mal_DF (object x1, object x2) {
  /* unpack x1,x2: */
  var signean sign1;
  var sintWL exp1;
  var uintL manthi1;
  var uintL mantlo1;
  var signean sign2;
  var sintWL exp2;
  var uintL manthi2;
  var uintL mantlo2;
 #ifdef intQsize
  {
    var uint64 mant1;
    DF_decode(x1, return x1, sign1=,exp1=,mant1=);
    manthi1 = (uint32)(mant1>>32); mantlo1 = (uint32)mant1;
  }
  {
    var uint64 mant2;
    DF_decode(x2, return x2, sign2=,exp2=,mant2=);
    manthi2 = (uint32)(mant2>>32); mantlo2 = (uint32)mant2;
  }
 #else
  DF_decode2(x1, return x1, sign1=,exp1=,manthi1=,mantlo1=);
  DF_decode2(x2, return x2, sign2=,exp2=,manthi2=,mantlo2=);
 #endif
  exp1 = exp1 + exp2; /* summ of exponents */
  sign1 = sign1 ^ sign2; /* result-sign */
  /* multiply the mantissas mant1 and mant2 (64x64-bit-multiplication): */
  var uintD mant1 [64/intDsize];
  var uintD mant2 [64/intDsize];
  var uintD mant [128/intDsize];
 #if (intDsize==32) || (intDsize==16) || (intDsize==8)
  set_32_Dptr(mant1,manthi1); set_32_Dptr(&mant1[32/intDsize],mantlo1);
  set_32_Dptr(mant2,manthi2); set_32_Dptr(&mant2[32/intDsize],mantlo2);
 #else
  {
    var uintD* ptr;
    ptr = &mant1[64/intDsize];
    doconsttimes(32/intDsize, { *--ptr = (uintD)mantlo1; mantlo1 = mantlo1>>intDsize; } );
    doconsttimes(32/intDsize, { *--ptr = (uintD)manthi1; manthi1 = manthi1>>intDsize; } );
  }
  {
    var uintD* ptr;
    ptr = &mant2[64/intDsize];
    doconsttimes(32/intDsize, { *--ptr = (uintD)mantlo2; mantlo2 = mantlo2>>intDsize; } );
    doconsttimes(32/intDsize, { *--ptr = (uintD)manthi2; manthi2 = manthi2>>intDsize; } );
  }
 #endif
  begin_arith_call();
  mulu_2loop_down(&mant1[64/intDsize],64/intDsize,
                  &mant2[64/intDsize],64/intDsize,
                  &mant[128/intDsize]);
  end_arith_call();
 #ifdef intQsize
  var uint64 manterg;
 #else
  var uintL manthi;
  var uintL mantlo;
 #endif
  /* product mant = mant1 * mant2 is >= 2^104, < 2^106. check bit 105: */
#define mant_bit(k)  (mant[128/intDsize - 1 - floor(k,intDsize)] & bit((k)%intDsize))
  if (mant_bit(2*DF_mant_len+1)) {
    /* mant>=2^(2*DF_mant_len+1), shift by DF_mant_len+1 bits to the right:
       fetch bits 105..53: */
   #if defined(intQsize) /* && (intDsize==32) */
    manterg = ((uint64)mant[0] << 43) | ((uint64)mant[1] << 11) | ((uint64)mant[2] >> 21); /* bits 116..53 */
    #define mantrest() ((mant[2] & (bit(21)-1)) || mant[3])
   #elif (intDsize==32)
    manthi = ((uint32)mant[0] << 11) | ((uint32)mant[1] >> 21); /* bits 116..85 */
    mantlo = ((uint32)mant[1] << 11) | ((uint32)mant[2] >> 21); /* bits 84..53 */
    #define mantrest() ((mant[2] & (bit(21)-1)) || mant[3])
   #elif (intDsize==16)
    manthi = /* ((uint32)mant[0] << 27) | ((uint32)mant[1] << 11) | ((uint32)mant[2] >> 5); // bits 116..85 */
      (highlow32_at(&mant[0])<<11) | ((uint32)mant[2] >> 5); /* Bits 116..85 */
    mantlo = /* ((uint32)mant[2] << 27) | ((uint32)mant[3] << 11) | ((uint32)mant[4] >> 5); // bits 84..53 */
      (highlow32_at(&mant[2])<<11) | ((uint32)mant[4] >> 5); /* bits 84..53 */
    #define mantrest() ((mant[4] & (bit(5)-1)) || mant[5] || mant[6] || mant[7])
   #elif (intDsize==8)
    manthi = ((uint32)mant[1] << 27) | ((uint32)mant[2] << 19) | ((uint32)mant[3] << 11) | ((uint32)mant[4] << 3) | ((uint32)mant[5] >> 5); /* bits 116..85 */
    mantlo = ((uint32)mant[5] << 27) | ((uint32)mant[6] << 19) | ((uint32)mant[7] << 11) | ((uint32)mant[8] << 3) | ((uint32)mant[9] >> 5); /* bits 84..53 */
    #define mantrest() ((mant[9] & (bit(5)-1)) || mant[10] || mant[11] || mant[12] || mant[13] || mant[14] || mant[15])
   #endif
    if ( (mant_bit(DF_mant_len) ==0) /* bit DF_mant_len =0 -> round off */
         || ( !mantrest() /* bit DF_mant_len =1 and bits DF_mant_len-1..0 >0 -> round up */
              /* round-to-even, according to bit DF_mant_len+1 : */
              && (mant_bit(DF_mant_len+1) ==0))) /* round off */
      goto ab;
    else /* round up */
      goto auf;
    #undef mantrest
  } else {
    /* mant<2^(2*DF_mant_len+1), shift by DF_mant_len bits to the right: */
    exp1 = exp1-1; /* decrement exponent */
    /* fetch bits 104..52: */
   #if defined(intQsize) /* && (intDsize==32) */
    manterg = ((uint64)mant[0] << 44) | ((uint64)mant[1] << 12) | ((uint64)mant[2] >> 20); /* bits 115..52 */
    #define mantrest() ((mant[2] & (bit(20)-1)) || mant[3])
   #elif (intDsize==32)
    manthi = ((uint32)mant[0] << 12) | ((uint32)mant[1] >> 20); /* bits 115..84 */
    mantlo = ((uint32)mant[1] << 12) | ((uint32)mant[2] >> 20); /* bits 83..52 */
    #define mantrest() ((mant[2] & (bit(20)-1)) || mant[3])
   #elif (intDsize==16)
    manthi = /* ((uint32)mant[0] << 28) | ((uint32)mant[1] << 12) | ((uint32)mant[2] >> 4); // bits 115..84 */
      (highlow32_at(&mant[0])<<12) | ((uint32)mant[2] >> 4); /* bits 115..84 */
    mantlo = /* ((uint32)mant[2] << 28) | ((uint32)mant[3] << 12) | ((uint32)mant[4] >> 4); // bits 83..52 */
      (highlow32_at(&mant[2])<<12) | ((uint32)mant[4] >> 4); /* bits 83..52 */
    #define mantrest() ((mant[4] & (bit(4)-1)) || mant[5] || mant[6] || mant[7])
   #elif (intDsize==8)
    manthi = ((uint32)mant[1] << 28) | ((uint32)mant[2] << 20) | ((uint32)mant[3] << 12) | ((uint32)mant[4] << 4) | ((uint32)mant[5] >> 4); /* bits 115..84 */
    mantlo = ((uint32)mant[5] << 28) | ((uint32)mant[6] << 20) | ((uint32)mant[7] << 12) | ((uint32)mant[8] << 4) | ((uint32)mant[9] >> 4); /* bits 83..52 */
    #define mantrest() ((mant[9] & (bit(4)-1)) || mant[10] || mant[11] || mant[12] || mant[13] || mant[14] || mant[15])
   #endif
    if ( (mant_bit(DF_mant_len-1) ==0) /* bit DF_mant_len-1 =0 -> round off */
         || ( !mantrest() /* bit DF_mant_len-1 =1 and bits DF_mant_len-2..0 >0 -> round up */
              /* round-to-even, according to bit DF_mant_len : */
              && (mant_bit(DF_mant_len) ==0))) /* round off */
      goto ab;
    else /* round up */
      goto auf;
    #undef mantrest
  }
  #undef mant_bit
 auf:
 #ifdef intQsize
  manterg = manterg+1;
  /* here, 2^DF_mant_len <= manterg <= 2^(DF_mant_len+1) */
  if (manterg >= bit(DF_mant_len+1)) { /* rounding overflow? */
    manterg = manterg>>1; exp1 = exp1+1; /* shift right */
  }
 #else
  mantlo = mantlo+1;
  if (mantlo==0) {
    manthi = manthi+1;
    /* here, 2^(DF_mant_len-32) <= manthi <= 2^(DF_mant_len-32+1) */
    if (manthi >= bit(DF_mant_len-32+1)) { /* rounding overflow? */
      manthi = manthi>>1; exp1 = exp1+1; /* shift to the right */
    }
  }
 #endif
 ab:
  /* rounding done, 2^DF_mant_len <= manterg < 2^(DF_mant_len+1) */
 #ifdef intQsize
  encode_DF(sign1,exp1,manterg, return);
 #else
  encode2_DF(sign1,exp1,manthi,mantlo, return);
 #endif
}
#endif

/* Return for two Double-Float x and y : (/ x y), a DF.
 DF_DF_durch_DF(x,y)
 can trigger GC */
  local maygc object DF_DF_durch_DF (object x, object y);
/* method:
 x2 = 0.0 -> Error
 x1 = 0.0 -> result 0.0
 Else:
 result-sign = xor of the two signs of x1 and x2
 result-exponent = difference of the two exponents of x1 and x2
 result-mantissa = mantissa mant1 / mantissa mant2, rounded.
   mant1/mant2 > 1/2, mant1/mant2 < 2;
   after rounding mant1/mant2 >=1/2, <=2*mant1<2.
   for mant1/mant2 >=1, we need 52 bits behind the dot,
   for mant1/mant2 <1 , we need 53 bits behind the dot.
   for rounding: need a rounding bit (rest specifies, if exact).
   Hence, we need altogether 54 bits behind the dot from mant1/mant2.
   Hence, divide (as Unsigned Integers) 2^54*(2^53*mant1) by (2^53*mant2).
   if the quotient is >=2^54 , round the last two bits away and
     increase the exponent by 1.
   if the quotient is <2^54 , round the last bit away. On rounding
     overflow, shift by one further bit to the right, increment exponent. */
#if defined(FAST_DOUBLE) && !defined(DOUBLE_DIV0_EXCEPTION) && !defined(I80386)
local maygc object DF_DF_durch_DF (object x1, object x2) {
  double_to_DF(DF_to_double(x1) / DF_to_double(x2), return ,
               true, true, /* catch Overflow and subnormal number */
               !DF_zerop(x1), /* a result +/- 0.0 */
               /* is exactly then really an Underflow */
               DF_zerop(x2), /* catch Division by Null */
               false); /* no NaN possible as result */
}
#else
local maygc object DF_DF_durch_DF (object x1, object x2) {
  /* unpack x1,x2: */
  var signean sign1;
  var sintWL exp1;
  var uintL manthi1;
  var uintL mantlo1;
  var signean sign2;
  var sintWL exp2;
  var uintL manthi2;
  var uintL mantlo2;
 #ifdef intQsize
  var uint64 mant1;
  var uint64 mant2;
  DF_decode(x2, divide_0(), sign2=,exp2=,mant2=);
  DF_decode(x1, return x1, sign1=,exp1=,mant1=);
 #else
  DF_decode2(x2, divide_0(), sign2=,exp2=,manthi2=,mantlo2=);
  DF_decode2(x1, return x1, sign1=,exp1=,manthi1=,mantlo1=);
 #endif
  exp1 = exp1 - exp2; /* difference of the exponents */
  sign1 = sign1 ^ sign2; /* result-sign */
  /* divide 2^54*mant1 by mant2 or (equivalent)
     2^i*2^54*mant1 by 2^i*mant2 for any i with 0 <= i <= 64-53 :
     choose i = 64-(DF_mant_len+1), thus i+(DF_mant_len+2) = 65. */
 #ifdef intQsize
  mant1 = mant1 << 1;
  mant2 = mant2 << (64-(DF_mant_len+1));
  manthi1 = (uint32)(mant1>>32); mantlo1 = (uint32)mant1;
  manthi2 = (uint32)(mant2>>32); mantlo2 = (uint32)mant2;
 #else
  manthi1 = (manthi1 << 1) | (mantlo1 >> 31); mantlo1 = mantlo1 << 1;
  manthi2 = (manthi2 << (64-(DF_mant_len+1))) | (mantlo2 >> ((DF_mant_len+1)-32)); mantlo2 = mantlo2 << (64-(DF_mant_len+1));
 #endif
  var uintD mant1 [128/intDsize];
  var uintD mant2 [64/intDsize];
 #if (intDsize==32) || (intDsize==16) || (intDsize==8)
  set_32_Dptr(mant1,manthi1); set_32_Dptr(&mant1[32/intDsize],mantlo1);
  set_32_Dptr(&mant1[2*32/intDsize],0); set_32_Dptr(&mant1[3*32/intDsize],0);
  set_32_Dptr(mant2,manthi2); set_32_Dptr(&mant2[32/intDsize],mantlo2);
 #else
  {
    var uintD* ptr;
    ptr = &mant1[128/intDsize];
    doconsttimes(64/intDsize, { *--ptr = 0; } );
    doconsttimes(32/intDsize, { *--ptr = (uintD)mantlo1; mantlo1 = mantlo1>>intDsize; } );
    doconsttimes(32/intDsize, { *--ptr = (uintD)manthi1; manthi1 = manthi1>>intDsize; } );
  }
  {
    var uintD* ptr;
    ptr = &mant2[64/intDsize];
    doconsttimes(32/intDsize, { *--ptr = (uintD)mantlo2; mantlo2 = mantlo2>>intDsize; } );
    doconsttimes(32/intDsize, { *--ptr = (uintD)manthi2; manthi2 = manthi2>>intDsize; } );
  }
 #endif
  var uintL mantlo;
 #ifdef intQsize
  var uint64 manthi;
 #else
  var uintL manthi;
 #endif
  { SAVE_NUM_STACK /* save num_stack */
    {
      var DS q;
      var DS r;
      begin_arith_call();
      UDS_divide(&mant1[0],128/intDsize,&mant1[128/intDsize],
                 &mant2[0],64/intDsize,&mant2[64/intDsize],
                 &q, &r);
      end_arith_call();
      /* it is 2^53 <= q < 2^55, so
         q.len = ceiling(54/intDsize)=ceiling(55/intDsize),
         and r=0 if and only if r.len=0. */
      ASSERT(q.len==ceiling(54,intDsize));
      {
        var uintD* ptr = q.MSDptr;
        manthi = get_max32_Dptr(23,ptr);
        mantlo = get_32_Dptr(&ptr[ceiling(23,intDsize)]);
      }
      /* q = 2^32*manthi+mantlo. */
     #ifdef intQsize
      manthi = (manthi<<32) | (uint64)mantlo;
      if (manthi >= bit(DF_mant_len+2)) {
        /* quotient >=2^54 -> round away 2 bits */
        var uint64 rounding_bits = manthi & (bit(2)-1);
        exp1 += 1; /* increment exponent */
        manthi = manthi >> 2;
        if ( (rounding_bits < bit(1)) /* 00,01 are rounded off */
             || ( (rounding_bits == bit(1)) /* 10 */
                  && (r.len == 0) /* and exactly half-numbered */
                  && ((manthi & bit(0)) ==0))) { /* -> round-to-even */
          /* round off */
        } else {
          /* round up */
          manthi += 1;
        }
      } else { /* quotient <2^54 -> round away 1 bit */
        var uint64 rounding_bit = manthi & bit(0);
        manthi = manthi >> 1;
        if ( (rounding_bit == 0) /* 0 is rounded off */
             || ( (r.len == 0) /* exactly half-numbered */
                  && ((manthi & bit(0)) ==0))) { /* -> round-to-even */
          /* round off */
        } else { /* round up */
          manthi += 1;
          if (manthi >= bit(DF_mant_len+1)) { /* rounding overflow? */
            manthi = manthi>>1; exp1 = exp1+1;
          }
        }
      }
     #else
      if (manthi >= bit(DF_mant_len-32+2)) {
        /* quotient >=2^54 -> round away 2 bits */
        var uintL rounding_bits = mantlo & (bit(2)-1);
        exp1 += 1; /* increment exponent */
        mantlo = (mantlo >> 2) | (manthi << 30); manthi = manthi >> 2;
        if ( (rounding_bits < bit(1)) /* 00,01 are rounded off */
             || ( (rounding_bits == bit(1)) /* 10 */
                  && (r.len == 0) /* and exactly half-numbered */
                  && ((mantlo & bit(0)) ==0))) { /* -> round-to-even */
          /* round off */
        } else { /* round up */
          mantlo += 1;
          if (mantlo==0)
            manthi += 1;
        }
      } else { /* quotient <2^54 -> round away 1 bit */
        var uintL rounding_bit = mantlo & bit(0);
        mantlo = (mantlo >> 1) | (manthi << 31); manthi = manthi >> 1;
        if ( (rounding_bit == 0) /* 0 is rounded off */
             || ( (r.len == 0) /* exactly half-numbered */
                  && ((mantlo & bit(0)) ==0))) { /* -> round-to-even */
          /* round off */
        } else { /* round up */
          mantlo += 1;
          if (mantlo==0) {
            manthi += 1;
            if (manthi >= bit(DF_mant_len-32+1)) { /* rounding overflow? */
              manthi = manthi>>1; exp1 = exp1+1;
            }
          }
        }
      }
     #endif
    }
    RESTORE_NUM_STACK } /* num_stack back */
 #ifdef intQsize
  encode_DF(sign1,exp1,manthi, return);
 #else
  encode2_DF(sign1,exp1,manthi,mantlo, return);
 #endif
}
#endif

/* Return for a Double-Float x>=0 : (sqrt x), a DF.
 DF_sqrt_DF(x)
 can trigger GC */
local maygc object DF_sqrt_DF (object x);
/* method:
 x = 0.0 -> result 0.0
 result-sign := positiv,
 result-exponent := ceiling(e/2),
 result-mantissa:
   Calculate from [1,m51,...,m0,(55 Nullbits)] on even e,
             from [0,1,m51,...,m0,(54 Nullbits)] on odd e
   the integer root, a 54 bit number with a leading 1.
   round the last bit away:
     Bit 0 = 0 -> round off,
     Bit 0 = 1 and root exact -> round-to-even,
     Bit 0 = 1 and rest >0 -> round up.
   Shift by one bit to the right.
   On rounding up to 2^53 (rounding overflow) shift mantissa by 1 bit to
     to the right and increment exponent. */
#ifdef intQsize /* && (intDsize==32) */
local maygc object DF_sqrt_DF (object x) {
  /* unpack x: */
  var sintWL exp;
  var uint64 mantx;
  DF_decode(x, return x, _EMA_,exp=,mantx=);
  /* In order to take advantage of the 128 bit integer root, we add
     74 resp. 75 instead of 54 resp. 55 null bits. */
  if (exp & bit(0)) { /* e odd */
    mantx = mantx << (63-(DF_mant_len+1)); exp = exp+1;
  } else { /* e even */
    mantx = mantx << (64-(DF_mant_len+1));
  }
  exp = exp >> 1; /* exp := exp/2 */
  {
    var uintD mant [128/intDsize];
    set_32_Dptr(mant,(uint32)(mantx>>32)); set_32_Dptr(&mant[32/intDsize],(uint32)mantx);
    set_32_Dptr(&mant[2*32/intDsize],0); set_32_Dptr(&mant[3*32/intDsize],0);
    { SAVE_NUM_STACK /* save num_stack */
      var DS wurzel;
      var bool exactp;
      UDS_sqrt(&mant[0],128/intDsize,&mant[128/intDsize], &wurzel, exactp=);
      { /* root = isqrt(2^74_75 * mant), a 64 bit number. */
        var uintD* ptr = wurzel.MSDptr;
        mantx = ((uint64)get_32_Dptr(ptr) << 32) | (uint64)get_32_Dptr(&ptr[32/intDsize]);
      }
      /* round away the 63-DF_mant_len bits behind: */
      if (((mantx & bit(62-DF_mant_len)) ==0) /* bit 10 =0 -> round off */
          || (((mantx & (bit(62-DF_mant_len)-1)) ==0) /* bit 10 =1 and bits 9..0 >0 -> round up */
              && exactp /* bit 10 =1 and bits 9..0 =0, but rest -> round up */
              /* round-to-even, according to bit 11 : */
              && ((mantx & bit(63-DF_mant_len)) ==0))) { /* round off */
        mantx = mantx >> (63-DF_mant_len);
      } else { /* round up */
        mantx = mantx >> (63-DF_mant_len);
        mantx += 1;
        if (mantx >= bit(DF_mant_len+1)) { /* rounding overflow? */
          mantx = mantx>>1; exp = exp+1;
        }
      }
      RESTORE_NUM_STACK } /* num_stack back */
  }
  encode_DF(0,exp,mantx, return);
}
#else
local maygc object DF_sqrt_DF (object x) {
  /* unpack x: */
  var sintWL exp;
  var uint32 manthi;
  var uint32 mantlo;
  DF_decode2(x, return x, _EMA_,exp=,manthi=,mantlo=);
  /* In order to take advantage of the 128 bit integer root, we add
     74 resp. 75 instead of 54 resp. 55 null bits. */
  if (exp & bit(0)) { /* e odd */
    manthi = (manthi << (63-(DF_mant_len+1))) | (mantlo >> ((DF_mant_len+1)-31));
    mantlo = mantlo << (63-(DF_mant_len+1));
    exp = exp+1;
  } else { /* e even */
    manthi = (manthi << (64-(DF_mant_len+1))) | (mantlo >> ((DF_mant_len+1)-32));
    mantlo = mantlo << (64-(DF_mant_len+1));
  }
  exp = exp >> 1; /* exp := exp/2 */
  {
    var uintD mant [128/intDsize];
   #if (intDsize==32) || (intDsize==16) || (intDsize==8)
    set_32_Dptr(mant,manthi); set_32_Dptr(&mant[32/intDsize],mantlo);
    set_32_Dptr(&mant[2*32/intDsize],0); set_32_Dptr(&mant[3*32/intDsize],0);
   #else
    {
      var uintD* ptr;
      ptr = &mant[128/intDsize];
      doconsttimes(64/intDsize, { *--ptr = 0; } );
      doconsttimes(32/intDsize, { *--ptr = (uintD)mantlo; mantlo = mantlo>>intDsize; } );
      doconsttimes(32/intDsize, { *--ptr = (uintD)manthi; manthi = manthi>>intDsize; } );
    }
   #endif
    {
      SAVE_NUM_STACK /* save num_stack */
        var DS wurzel;
      var bool exactp;
      UDS_sqrt(&mant[0],128/intDsize,&mant[128/intDsize], &wurzel, exactp=);
      { /* root = isqrt(2^74_75 * mant), a 64 bit number. */
        var uintD* ptr = wurzel.MSDptr;
        manthi = get_32_Dptr(ptr); mantlo = get_32_Dptr(&ptr[32/intDsize]);
      }
      /* round away the 63-DF_mant_len bits behind: */
      if (((mantlo & bit(62-DF_mant_len)) ==0) /* bit 10 =0 -> round off */
           || (((mantlo & (bit(62-DF_mant_len)-1)) ==0) /* bit 10 =1 and bits 9..0 >0 -> round up */
                && exactp /* bit 10 =1 and bits 9..0 =0, but rest -> round up */
                /* round-to-even, according to bit 11 : */
                && ((mantlo & bit(63-DF_mant_len)) ==0))) { /* round off */
        mantlo = (mantlo >> (63-DF_mant_len)) | (manthi << (DF_mant_len-32+1));
        manthi = manthi >> (63-DF_mant_len);
      } else { /* round up */
        mantlo = (mantlo >> (63-DF_mant_len)) | (manthi << (DF_mant_len-32+1));
        manthi = manthi >> (63-DF_mant_len);
        mantlo += 1;
        if (mantlo==0) {
          manthi += 1;
          if (manthi >= bit(DF_mant_len-32+1)) { /* rounding overflow? */
            manthi = manthi>>1; exp = exp+1;
          }
        }
      }
      RESTORE_NUM_STACK /* restore num_stack */
        }
  }
  encode2_DF(0,exp,manthi,mantlo, return);
}
#endif

/* DF_to_I(x) converts a Double-Float x, that represents an integer,
 into an Integer.
 can trigger GC */
local maygc object DF_to_I (object x);
/* method:
 if x=0.0, result 0.
 else (ASH sign*mantissa (e-53)). */
#ifdef intQsize
local maygc object DF_to_I (object x) {
  /* unpack x: */
  var signean sign;
  var sintWL exp;
  var uint64 mant;
  DF_decode(x, return Fixnum_0, sign=,exp=,mant=);
  exp = exp-(DF_mant_len+1);
  /* add sign to mant: */
  if (sign!=0)
    mant = -mant;
  /* convert into a bignum and shift: */
  return I_I_ash_I( Q_to_I(mant), L_to_FN(exp) );
}
#else
local maygc object DF_to_I (object x) {
  /* unpack x: */
  var signean sign;
  var sintWL exp;
  var uint32 manthi;
  var uint32 mantlo;
  DF_decode2(x, return Fixnum_0, sign=,exp=,manthi=,mantlo=);
  exp = exp-(DF_mant_len+1);
  /* add sign to mant: */
  if (sign!=0) {
    manthi = -manthi;
    mantlo = -mantlo;
    if (mantlo!=0)
      manthi -= 1;
  }
  /* convert into a bignum and shift: */
  return I_I_ash_I( L2_to_I(manthi,mantlo), L_to_FN(exp) );
}
#endif

/* I_to_DF(x,signal_overflow) converts an integer x to a double-float, and
 rounds thereby.
 can trigger GC */
local maygc object I_to_DF (object x, bool signal_overflow);
/* method:
 x=0 -> result 0.0
 memorize sign of x.
 x:=(abs x)
 exponent:=(integer-length x)
   extract the 54 most significant bits (lead by a 1).
   round the last bit away:
     Bit 0 = 0 -> round off,
     Bit 0 = 1 and rest =0 -> round-to-even,
     Bit 0 = 1 and rest >0 -> round up.
   Shift by one bit to the right.
   On rounding up to 2^53 (rounding overflow) shift mantissa by 1 bit
     to the right and increment exponent. */
local maygc object I_to_DF (object x, bool signal_overflow) {
  if (eq(x,Fixnum_0))
    return DF_0;
  var signean sign = R_sign(x); /* sign */
  if (sign!=0)
    x = I_minus_I(x); /* if x<0: x := (- x) */
  var uintL exp = I_integer_length(x); /* (integer-length x) */
  /* form NDS for x>0 : */
  var uintD* MSDptr;
  var uintC len;
  I_to_NDS_nocopy(x, MSDptr=,len=,);
  /* MSDptr/len/LSDptr is the NDS for x, len>0.
     Fetch leading digits: Need DF_mant_len+1 bits, plus intDsize
     bits (the NDS can start with up to intDsize nullbits).
     Then, these bits are shifted by (exp mod intDsize) to the right. */
  var uintD msd = *MSDptr++; /* first digit */
  var uint32 msdd = 0; /* further min(len-1,32/intDsize) digits */
  var uint32 msddf = 0; /* further maximum 32/intDsize digits */
 #define NEXT_DIGIT(i)    {                                    \
   if (--len == 0) goto ok;                                    \
   msdd |= (uint32)(*MSDptr++) << (32-(i+1)*intDsize);         \
 }
  DOCONSTTIMES(32/intDsize,NEXT_DIGIT);
 #undef NEXT_DIGIT
 #define NEXT_DIGIT(i) {                                        \
   if (--len == 0) goto ok;                                     \
   msddf |= (uint32)(*MSDptr++) << (32-(i+1)*intDsize);         \
 }
  DOCONSTTIMES(32/intDsize,NEXT_DIGIT);
 #undef NEXT_DIGIT
  --len; ok: {
    /* the NDS consists of msd, msdd, msddf and len further digits.
       the highest set bit in 2^64*msd+2^32*msdd+msddf is bit number
       63 + (exp mod intDsize). */
    var uintL shiftcount = exp % intDsize;
    #define fehler_overflow() \
      if (signal_overflow) (fehler_overflow)(); else return nullobj;
   #ifdef intQsize
    var uint64 mant = /* leading 64 bits */
      (shiftcount==0
       ? (((uint64)msdd << 32) | (uint64)msddf)
       : (((uint64)msd << (64-shiftcount)) | ((uint64)msdd << (32-shiftcount))
          | ((uint64)msddf >> shiftcount)));
    /* the highest set bit in mant is bit number 63. */
    if (((mant & bit(62-DF_mant_len)) ==0) /* bit 10 =0 -> round off */
        || (((mant & (bit(62-DF_mant_len)-1)) ==0) /* bit 10 =1 and bits 9..0 =0 */
            && ((msddf & (bit(shiftcount)-1)) ==0) /* and further bits from msddf =0 */
            && (!test_loop_up(MSDptr,len)) /* and all further digits =0 */
            /* round-to-even, according to bit 11 : */
            && ((mant & bit(63-DF_mant_len)) ==0))) { /* round off */
      mant = mant >> (63-DF_mant_len);
    } else { /* round up */
      mant = mant >> (63-DF_mant_len);
      mant += 1;
      if (mant >= bit(DF_mant_len+1)) { /* rounding overflow? */
        mant = mant>>1; exp = exp+1;
      }
    }
    encode_DF(sign,(sintL)exp,mant, return);
   #else
    var uint32 manthi; /* leading 32 bits */
    var uint32 mantlo; /* next 32 bits */
    if (shiftcount==0) {
      manthi = msdd; mantlo = msddf;
    } else {
      manthi = ((uint32)msd << (32-shiftcount)) | (msdd >> shiftcount);
      mantlo = (msdd << (32-shiftcount)) | (msddf >> shiftcount);
    }
    /* the highest set bit in mant is bit number 63. */
    if (((mantlo & bit(62-DF_mant_len)) ==0) /* bit 10 =0 -> round off */
        || (((mantlo & (bit(62-DF_mant_len)-1)) ==0) /* bit 10 =1 and bits 9..0 =0 */
            && ((msddf & (bit(shiftcount)-1)) ==0) /* and further bits from msddf =0 */
            && (!test_loop_up(MSDptr,len)) /* and all further digits =0 */
            /* round-to-even, according to bit 11 : */
            && ((mantlo & bit(63-DF_mant_len)) ==0))) { /* round off */
      mantlo = (mantlo >> (63-DF_mant_len)) | (manthi << (DF_mant_len-32+1));
      manthi = manthi >> (63-DF_mant_len);
    } else { /* round up */
      mantlo = (mantlo >> (63-DF_mant_len)) | (manthi << (DF_mant_len-32+1));
      manthi = manthi >> (63-DF_mant_len);
      mantlo += 1;
      if (mantlo==0) {
        manthi += 1;
        if (manthi >= bit(DF_mant_len-32+1)) { /* rounding overflow? */
          manthi = manthi>>1; exp = exp+1;
        }
      }
    }
    encode2_DF(sign,(sintL)exp,manthi,mantlo, return);
   #endif
    #undef fehler_overflow
  }
}

/* RA_to_DF(x,signal_overflow) converts a rational number x to a double-float,
 and rounds thereby.
 can trigger GC */
local maygc object RA_to_DF (object x, bool signal_overflow);
/* method:
 x integer -> obvious.
 x = +/- a/b with Integers a,b>0:
   let n,m be chosen, so that
     2^(n-1) <= a < 2^n, 2^(m-1) <= b < 2^m.
   Then, 2^(n-m-1) < a/b < 2^(n-m+1).
   Calculate n=(integer-length a) and m=(integer-length b) and
   floor(2^(-n+m+54)*a/b) :
   If n-m>=54 divide a by (ash b (n-m-54)),
   if n-m<54 divide (ash a (-n+m+54)) by b.
   The first value is >=2^53, <2^55.
   If it is >=2^54 , round 2 bits away,
   if it is <2^54 , round 1 bit away. */
local maygc object RA_to_DF (object x, bool signal_overflow) {
  if (RA_integerp(x))
    return I_to_DF(x,signal_overflow);
  /* x ratio */
  #define fehler_overflow() \
    if (signal_overflow) (fehler_overflow)(); else return nullobj;
  pushSTACK(TheRatio(x)->rt_den); /* b */
  var signean sign = RT_sign(x); /* sign */
  x = TheRatio(x)->rt_num; /* +/- a */
  if (sign!=0)
    x = I_minus_I(x); /* take absolute value, returns a */
  pushSTACK(x);
  /* stack layout: b, a. */
  var sintL lendiff = I_integer_length(x) /* (integer-length a) */
    - I_integer_length(STACK_1); /* (integer-length b) */
  if (lendiff > DF_exp_high-DF_exp_mid) /* exponent >= n-m > upper limit ? */
    fehler_overflow(); /* -> Overflow */
  if (lendiff < DF_exp_low-DF_exp_mid-2) { /* Exponent <= n-m+2 < lower limit ? */
    if (underflow_allowed()) {
      fehler_underflow(); /* -> Underflow */
    } else {
      skipSTACK(2); return DF_0;
    }
  }
  var object zaehler;
  var object nenner;
  if (lendiff >= DF_mant_len+2) {
    /* n-m-54>=0 */
    nenner = I_I_ash_I(STACK_1,fixnum((uint32)(lendiff - (DF_mant_len+2)))); /* (ash b n-m-54) */
    zaehler = popSTACK(); /* a */
    skipSTACK(1);
  } else {
    zaehler = I_I_ash_I(popSTACK(),fixnum((uint32)((DF_mant_len+2) - lendiff))); /* (ash a -n+m+54) */
    nenner = popSTACK(); /* b */
  }
  /* execute division zaehler/nenner : */
  I_I_divide_I_I(zaehler,nenner);
  /* stack layout: q, r. */
  /* 2^53 <= q < 2^55, hence q is bignum with ceiling(55/intDsize) digits. */
  var uintD* ptr = &TheBignum(STACK_1)->data[0];
 #ifdef intQsize
  var uint64 mant =
    ((uint64)get_max32_Dptr(23,ptr) << 32)
    | (uint64)get_32_Dptr(&ptr[ceiling(23,intDsize)]);
  if (mant >= bit(DF_mant_len+2)) {
    /* 2^54 <= q < 2^55, shift by 2 bits to the right */
    var uint64 rounding_bits = mant & (bit(2)-1);
    lendiff = lendiff+1; /* exponent := n-m+1 */
    mant = mant >> 2;
    if ( (rounding_bits < bit(1)) /* 00,01 are rounded off */
         || ( (rounding_bits == bit(1)) /* 10 */
              && (eq(STACK_0,Fixnum_0)) /* and exactly half-numbered (r=0) */
              && ((mant & bit(0)) ==0))) /* -> round-to-even */
      /* round off */
      goto ab;
    else /* round up */
      goto auf;
  } else {
    var uint64 rounding_bit = mant & bit(0);
    mant = mant >> 1;
    if ((rounding_bit == 0) /* 0 is rounded off */
        || ((eq(STACK_0,Fixnum_0)) /* exactly half-numbered (r=0) */
            && ((mant & bit(0)) ==0))) /* -> round-to-even */
      /* round off */
      goto ab;
    else /* round up */
      goto auf;
  }
 auf:
  mant += 1;
  if (mant >= bit(DF_mant_len+1)) { /* rounding overflow? */
    mant = mant>>1; lendiff = lendiff+1;
  }
 ab:
  skipSTACK(2);
  /* done. */
  encode_DF(sign,lendiff,mant, return);
 #else
  var uint32 manthi = get_max32_Dptr(23,ptr);
  var uint32 mantlo = get_32_Dptr(&ptr[ceiling(23,intDsize)]);
  if (manthi >= bit(DF_mant_len-32+2)) {
    /* 2^54 <= q < 2^55, shift by 2 bits to the right */
    var uintL rounding_bits = mantlo & (bit(2)-1);
    lendiff = lendiff+1; /* exponent := n-m+1 */
    mantlo = (mantlo >> 2) | (manthi << 30); manthi = manthi >> 2;
    if ((rounding_bits < bit(1)) /* 00,01 are rounded off */
        || ((rounding_bits == bit(1)) /* 10 */
            && (eq(STACK_0,Fixnum_0)) /* and exactly half-numbered (r=0) */
            && ((mantlo & bit(0)) ==0))) /* -> round-to-even */
      /* round off */
      goto ab;
    else /* round up */
      goto auf;
  } else {
    var uintL rounding_bit = mantlo & bit(0);
    mantlo = (mantlo >> 1) | (manthi << 31); manthi = manthi >> 1;
    if ((rounding_bit == 0) /* 0 is rounded off */
        || ((eq(STACK_0,Fixnum_0)) /* exactly half-numbered (r=0) */
            && ((mantlo & bit(0)) ==0))) /* -> round-to-even */
      /* round off */
      goto ab;
    else /* round up */
      goto auf;
  }
 auf:
  mantlo += 1;
  if (mantlo==0) {
    manthi += 1;
    if (manthi >= bit(DF_mant_len-32+1)) { /* rounding overflow? */
      manthi = manthi>>1; lendiff = lendiff+1;
    }
      }
 ab:
  skipSTACK(2);
  /* done. */
  encode2_DF(sign,lendiff,manthi,mantlo, return);
 #endif
  #undef fehler_overflow
}
