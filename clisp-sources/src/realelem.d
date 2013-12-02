/* Elementary functions for real numbers */

local bool R_zerop (object x)
{ /* R_zerop(x) determines, if (= x 0), with x being a real number. */
  if (R_rationalp(x)) /* with rational numbers: test for 0 */
    return eq(x,Fixnum_0);
  /* with floats: case differentiation */
  floatcase(x,
            { return SF_zerop(x); },
            { return FF_zerop(x); },
            { return DF_zerop(x); },
            { return LF_zerop(x); });
}

/* R_plusp(x) determines, if (> x 0), with x being a real number. */
local bool R_plusp (object x)
{ if (R_minusp(x)) return false; /* x<0 -> no */
  else if (R_zerop(x)) return false; /* x=0 -> no */
  else return true; /* else x>0. */
}

/* R_minusp(x) determines, if (< x 0), with x being a real number.
 (Macro in LISPBIBL.D) */

/* I_F_float_F(x,y) converts an Integer x into the float-format of the float y
 and rounds if necessary.
 > x: an Integer
 > y: a Float
 < result: (float x y)
 can trigger GC */
local maygc object I_F_float_F (object x, object y)
{ floatcase(y,
            { return I_to_SF(x,true); },
            { return I_to_FF(x,true); },
            { return I_to_DF(x,true); },
            { return I_to_LF(x,Lfloat_length(y),true); });
}

/* RA_F_float_F(x,y,signal_overflow) converts a rational number x to the
 float-format of the Float y and rounds if necessary.
 > x: a rational number
 > y: a float
 < result: (float x y)
 can trigger GC */
local maygc object RA_F_float_F (object x, object y, bool signal_overflow)
{ floatcase(y,
            { return RA_to_SF(x,signal_overflow); },
            { return RA_to_FF(x,signal_overflow); },
            { return RA_to_DF(x,signal_overflow); },
            { return RA_to_LF(x,Lfloat_length(y),signal_overflow); });
}

/* R_F_float_F(x,y) converts a real number x into the float-format of the float
 y and rounds if necessary.
 > x: a real number
 > y: a float
 < result: (float x y)
 can trigger GC */
local maygc object R_F_float_F (object x, object y)
{ return (R_rationalp(x) ? RA_F_float_F(x,y,true) : F_F_float_F(x,y)); }

/* R_to_SF(x) converts a real number x into a short-float.
 < result: (coerce x 'short-float)
 can trigger GC */
local maygc object R_to_SF (object x)
{ return (R_rationalp(x) ? RA_to_SF(x,true) : F_to_SF(x)); }

/* R_to_FF(x) converts a real number x into a single-float.
 < result: (coerce x 'single-float)
 can trigger GC */
local maygc object R_to_FF (object x)
{ return (R_rationalp(x) ? RA_to_FF(x,true) : F_to_FF(x)); }

/* R_to_DF(x) converts a real number x into a double-float.
 < result: (coerce x 'double-float)
 can trigger GC */
local maygc object R_to_DF (object x)
{ return (R_rationalp(x) ? RA_to_DF(x,true) : F_to_DF(x)); }

/* R_to_LF(x,len) converts a real number x into a long-float with len digits.
 > uintC len: desired number of digits, >=LF_minlen
 < result: (coerce x `(long-float ,len))
 can trigger GC */
local maygc object R_to_LF (object x, uintC len)
{ return (R_rationalp(x) ? RA_to_LF(x,len,true) : F_to_LF(x,len)); }

/* R_R_contagion_R(x,y) returns a real number, that is as imprecise as the
 more imprecise one of both real numbers x and y. */
local maygc object R_R_contagion_R (object x, object y)
{
#define X  { return x; }
#define Y  { return y; }
#define WX  goto warn_x;
#define WY  goto warn_y;
  if (R_rationalp(x)) Y
  else if (R_rationalp(y)) X
  else
    floatcase(x,
              /* x SF */ floatcase(y, X,WX,WX,WX),
              /* x FF */ floatcase(y, WY,X,WX,WX),
              /* x DF */ floatcase(y, WY,WY,X,WX),
              /* x LF */ floatcase(y, WY,WY,WY, { /* y LF */
                if (Lfloat_length(x) == Lfloat_length(y)) X
                else if (Lfloat_length(x) <= Lfloat_length(y)) WX
                else WY
              }));
 warn_x: {
    var object result = (nullpSv(floating_point_contagion_ansi) ? x : y);
    if (!nullpSv(warn_on_floating_point_contagion)) {
      pushSTACK(result);
      warn_floating_point_contagion();
      result = popSTACK();
    }
    return result;
  }
 warn_y: {
    var object result = (nullpSv(floating_point_contagion_ansi) ? y : x);
    if (!nullpSv(warn_on_floating_point_contagion)) {
      pushSTACK(result);
      warn_floating_point_contagion();
      result = popSTACK();
    }
    return result;
  }
#undef WY
#undef WX
#undef Y
#undef X
}

local maygc object N_N_contagion_R (object x, object y)
{
  pushSTACK(y);
  if (complexp(x))
    x = R_R_contagion_R(TheComplex(x)->c_real,TheComplex(x)->c_imag);
  y = STACK_0; STACK_0 = x;
  if (complexp(y))
    y = R_R_contagion_R(TheComplex(y)->c_real,TheComplex(y)->c_imag);
  x = popSTACK();
  return R_R_contagion_R(x,y);
}

/* Warns, if floating-point and rational numbers are combined, and the
 mathematical result could be a rational number.
 warn_floating_point_rational_contagion();
 can trigger GC */
local maygc void warn_floating_point_rational_contagion (void) {
  pushSTACK(CLSTEXT("Numerical operation combines exact and inexact numbers "
                    "(rational numbers and floating-point numbers), "
                    "and the mathematical result is exact. "
                    "See ANSI CL 12.1.4.1 and the CLISP impnotes for details. "
                    "The result's actual exactness is controlled by ~S. "
                    "To shut off this warning, set ~S to ~S."));
  pushSTACK(S(floating_point_rational_contagion_ansi));
  pushSTACK(S(warn_on_floating_point_rational_contagion));
  pushSTACK(NIL);
  funcall(eq(Symbol_value(S(warn_on_floating_point_rational_contagion)),S(error))
          ? S(error) : S(warn),4);
}

/* When an operation combines a rational and a floating-point number and the
 mathematical result is a rational number, this returns the actual result.
 RA_F_exact_contagion_R(result,float_argument)
 can trigger GC */
local maygc object RA_F_exact_contagion_R (object result, object float_argument) {
  if (!nullpSv(floating_point_rational_contagion_ansi))
    result = RA_F_float_F(result,float_argument,true);
  if (!nullpSv(warn_on_floating_point_rational_contagion)) {
    pushSTACK(result);
    warn_floating_point_rational_contagion();
    result = popSTACK();
  }
  return result;
}

/* Macro: distributes according to the default-float-type to 4 statements.
 defaultfloatcase(symbol, SF_statement,FF_statement,DF_statement,LF_statement,
                  save_statement,restore_statement);
 symbol should be a S(..)-symbol. Its value should be SHORT-FLOAT or
 SINGLE-FLOAT or DOUBLE-FLOAT or LONG-FLOAT. If this is not the case,
 the value is set to SINGLE-FLOAT and a warning is issued.
 can trigger GC, but only between save_statement and restore_statement. */
#define defaultfloatcase(symbol,num,SF_statement,FF_statement,DF_statement,LF_statement,save_statement,restore_statement) do { \
  var object def;                                                       \
  if (floatp(num))                                                      \
    floatcase(num,goto _DFC_SF,goto _DFC_FF,goto _DFC_DF,goto _DFC_LF); \
  def = Symbol_value(symbol); /* fetch value */                         \
  if (eq(def,S(short_float))) { _DFC_SF: SF_statement; }                \
  else if (eq(def,S(single_float))) { _DFC_FF: FF_statement; }          \
  else if (eq(def,S(double_float))) { _DFC_DF: DF_statement; }          \
  else if (eq(def,S(long_float))) { _DFC_LF: LF_statement; }            \
  else {                                                                \
    Symbol_value(symbol) = S(single_float); /* correct value */         \
    save_statement;                                                     \
    /* issue warning:                                                   \
       (WARN "The variable ~S had an illegal value.                     \
              ~S has been reset to ~S."                                 \
             symbol symbol (symbol-value symbol))  */                   \
    pushSTACK(NIL);                                                     \
    pushSTACK(symbol);                                                  \
    pushSTACK(symbol);                                                  \
    pushSTACK(Symbol_value(symbol));                                    \
    STACK_3 = CLSTEXT("The variable ~S had an illegal value.\n"         \
                      "~S has been reset to ~S.");                      \
    funcall(S(warn),4);                                                 \
    restore_statement;                                                  \
    { FF_statement; }                                                   \
  }} while(0)

/* I_float_F(x) converts an Integer x into a Float and rounds.
 > x: an Integer
 < result: (float x)
 can trigger GC */
local maygc object I_float_F (object x) {
  defaultfloatcase(S(default_float_format),Fixnum_0,
                   return I_to_SF(x,true),
                   return I_to_FF(x,true),
                   return I_to_DF(x,true),
                   return I_to_LF(x,I_to_UL(O(LF_digits)),true); ,
                   pushSTACK(x), x = popSTACK());
}

/* RA_float_F(x) converts a rational number x into a Float and rounds.
 > x: a rational number
 < result: (float x)
 can trigger GC */
local maygc object RA_float_F (object x) {
  defaultfloatcase(S(default_float_format),Fixnum_0,
                   return RA_to_SF(x,true),
                   return RA_to_FF(x,true),
                   return RA_to_DF(x,true),
                   return RA_to_LF(x,I_to_UL(O(LF_digits)),true); ,
                   pushSTACK(x), x = popSTACK());
}

/* R_float_F(x) converts a real number x into a Float
 and rounds if necessary.
 > x: a real number
 < result: (float x)
 can trigger GC */
local maygc object R_float_F (object x)
{ return (R_rationalp(x) ? RA_float_F(x) : x); }

/* convert a float(rational) X to the appropriate format for Y
 same as F_F_float_F(x,R_float_F(y)) but without consing
 an intermediate float
 can trigger GC */
local maygc object F_R_float_F (object x, object y)
{
 #if SAFETY>=1
  if (!floatp(x)) abort();
  if_realp(y,,abort(););
 #endif
  defaultfloatcase(S(default_float_format),y,
                   return F_to_SF(x),
                   return F_to_FF(x),
                   return F_to_DF(x),
                   return F_to_LF(x,(long_float_p(y) ? Lfloat_length(y)
                                     : I_to_UL(O(LF_digits)))),
                   pushSTACK(x), x = popSTACK());
}
local maygc object RA_R_float_F (object x, object y)
{
  defaultfloatcase(S(default_float_format),y,
                   return RA_to_SF(x,true),
                   return RA_to_FF(x,true),
                   return RA_to_DF(x,true),
                   return RA_to_LF(x,(long_float_p(y) ? Lfloat_length(y)
                                      : I_to_UL(O(LF_digits))),
                                   true),
                   pushSTACK(x), x = popSTACK());
}
local maygc object R_R_float_F (object x, object y)
{
  if (floatp(x)) return F_R_float_F(x,y);
  return RA_R_float_F(x,y);
}
local maygc object C_R_float_C (object c, object y)
{
  pushSTACK(c); pushSTACK(y);
  var object realpart = R_R_float_F(TheComplex(c)->c_real,y);
  var object imagpart = TheComplex(STACK_1)->c_imag;
  STACK_1 = realpart;
  imagpart = R_R_float_F(imagpart,popSTACK());
  realpart = popSTACK();
  return make_complex(realpart,imagpart);
}
local maygc object N_N_float_N (object x, object y)
{
  if (complexp(y)) {
    pushSTACK(x);
    y = R_R_contagion_R(TheComplex(y)->c_real,TheComplex(y)->c_imag);
    x = popSTACK();
  }
  if (complexp(x))
    return C_R_float_C(x,y);
  return R_R_float_F(x,y);
}

/* R_float_digits(x) returns (float-digits (float x)), with x being a real.
 < result: a uintL >0 */
local maygc uintL R_float_digits (object x)
{
  if (R_floatp(x)) {
    return F_float_digits(x);
  } else {
    defaultfloatcase(S(default_float_format),Fixnum_0,
                     { return SF_mant_len+1; }, /* 17 */
                     { return FF_mant_len+1; }, /* 24 */
                     { return DF_mant_len+1; }, /* 53 */
                     { return intDsize*I_to_UL(O(LF_digits)); }, /* 16n */
                     ,);
  }
}

/* Generates a function like R_floor_I_R
 Returns whole-numbered and fractional part of a real number.
 (q,r) := (rounding x)
 R_rounding_I_R(x);
 > x: real number
 < STACK_1: quotient q, an integer
 < STACK_0: remainder r, a real number
 Decreases STACK by 2
 can trigger GC
 method:
 x rational -> RA_rounding_I_RA(x)
 x float -> F_rounding_I_F(x) */
#define GEN_R_round(rounding)                             \
  local maygc void CONCAT3(R_,rounding,_I_R) (object x) { \
    GCTRIGGER1(x);                                        \
    if (R_rationalp(x))                                   \
      { CONCAT3(RA_,rounding,_I_RA) (x); }                \
      else                                                \
      { CONCAT3(F_,rounding,_I_F) (x); }                  \
  }

/* R_floor_I_R(x) => (floor x), with x being a real number.
 Both values into the stack.
 can trigger GC */
local maygc void R_floor_I_R (object x);
GEN_R_round(floor)

/* R_ceiling_I_R(x) => (ceiling x), with x being a real number.
 Both values into the stack.
 can trigger GC */
local maygc void R_ceiling_I_R (object x);
GEN_R_round(ceiling)

/* R_truncate_I_R(x) => (truncate x), with x being a real number.
 Both values into the stack.
 can trigger GC */
local maygc void R_truncate_I_R (object x);
GEN_R_round(truncate)

/* R_round_I_R(x) => (round x), with x being a real number.
 Both values into the stack.
 can trigger GC */
local maygc void R_round_I_R (object x);
GEN_R_round(round)

/* Generates a function like R_ffloor_F_R *
 Returns whole-numbered and fractional part of a real number.
 (q,r) := (frounding x)
 R_frounding_F_R(x);
 > x: real number
 < STACK_1: quotient q, an integer-valued float
 < STACK_0: remainder r, a real number
 Decreases STACK by 2
 can trigger GC
 method:
 x rational -> RA_rounding_I_RA(x), convert quotient into Float.
 x Float -> F_frounding_F_F(x). */
#define GEN_R_fround(rounding)                                          \
  local maygc void CONCAT3(R_f,rounding,_F_R) (object x) {              \
    GCTRIGGER1(x);                                                      \
    if (R_rationalp(x)) {                                               \
      CONCAT3(RA_,rounding,_I_RA) (x); /* rational-routine */           \
      STACK_1 = I_float_F(STACK_1); /* convert 1. value into in Float */ \
    } else                                                              \
      { CONCAT3(F_f,rounding,_F_F) (x); } /* float-routine */           \
  }

/* R_ffloor_F_R(x) => (ffloor x), with x being a real number.
 Both values into the stack.
 can trigger GC */
local maygc void R_ffloor_F_R (object x);
GEN_R_fround(floor)

/* R_fceiling_F_R(x) => (fceiling x), with x being a real number.
 Both values into the stack.
 can trigger GC */
local maygc void R_fceiling_F_R (object x);
GEN_R_fround(ceiling)

/* R_ftruncate_F_R(x) => (ftruncate x), with x being a real number.
 Both values into the stack.
 can trigger GC */
local maygc void R_ftruncate_F_R (object x);
GEN_R_fround(truncate)

/* R_fround_F_R(x) => (fround x), with x being a real number.
 Both values into the stack.
 can trigger GC */
local maygc void R_fround_F_R (object x);
GEN_R_fround(round)

/* Generates a function like R_R_plus_R */
#define GEN_R_op21(arg1,arg2,op,return_statement)   do {                \
  if (R_rationalp(arg1)) {                                              \
    if (R_rationalp(arg2)) { /* both rational numbers */                \
      return_statement CONCAT3(RA_RA_,op,_RA) (arg1,arg2);              \
    } else { /* arg1 rational, arg2 Float -> convert arg1 into Float */ \
      pushSTACK(arg2); arg1 = RA_F_float_F(arg1,arg2,true); arg2 = popSTACK(); \
      return_statement CONCAT3(F_F_,op,_F) (arg1,arg2);                 \
    }                                                                   \
  } else {                                                              \
    if (R_rationalp(arg2)) {                                            \
      /* arg1 Float, arg2 rational -> convert arg2 into Float */        \
      pushSTACK(arg1); arg2 = RA_F_float_F(arg2,arg1,true); arg1 = popSTACK(); \
      return_statement CONCAT3(F_F_,op,_F) (arg1,arg2);                 \
    } else { /* both Floats */                                          \
      return_statement CONCAT3(F_F_,op,_F) (arg1,arg2);                 \
    }                                                                   \
  }                                                                     \
 } while(0)

/* R_minus_R(x) => (- x), with x being a real number.
 can trigger GC */
local maygc object R_minus_R (object x)
{ return (R_rationalp(x) ? RA_minus_RA(x) : F_minus_F(x)); }

/* R_abs_R(x) => (abs x), with x being a real number.
 can trigger GC */
local maygc object R_abs_R (object x)
{ return (R_minusp(x) ? R_minus_R(x) : x); } /* x<0 -> (- x), x>=0 -> x */

/* R_R_plus_R(x,y) => (+ x y), with x and y being real numbers.
 can trigger GC */
local maygc object R_R_plus_R (object x, object y)
{
  if (eq(y,Fixnum_0))
    return x;
  else if (eq(x,Fixnum_0))
    return y;
  else
    GEN_R_op21(x,y,plus,return);
}

/* R_R_minus_R(x,y) => (- x y), with x and y being real numbers.
 can trigger GC */
local maygc object R_R_minus_R (object x, object y)
{
  if (eq(y,Fixnum_0))
    return x;
  else if (eq(x,Fixnum_0))
    return R_minus_R(y);
  else
    GEN_R_op21(x,y,minus,return);
}

/* R_square_R(x) => (* x x), with x being a real number.
 can trigger GC */
local maygc object R_square_R (object x)
{ return (R_rationalp(x) ? RA_square_RA(x) : F_square_F(x)); }

/* R_R_mal_R(x,y) => (* x y), with x and y being real numbers.
 can trigger GC */
local maygc object R_R_mal_R (object x, object y)
{
  if (eq(x,Fixnum_0)) {
    /* 0 * y = exact 0 */
    if (R_floatp(y))
      return RA_F_exact_contagion_R(x,y);
    else
      return x;
  } else if (eq(y,Fixnum_0)) {
    /* x * 0 = exact 0 */
    if (R_floatp(x))
      return RA_F_exact_contagion_R(y,x);
    else
      return y;
  } else
    GEN_R_op21(x,y,mal,return);
}

/* R_durch_R(x) => (/ x), with x being a real number.
 can trigger GC */
local maygc object R_durch_R (object x)
{ return (R_rationalp(x) ? RA_durch_RA(x) : F_durch_F(x)); }

/* R_R_durch_R(x,y) => (/ x y), with x and y being real numbers.
 can trigger GC */
local maygc object R_R_durch_R (object x, object y)
{
  if (eq(x,Fixnum_0)) { /* 0 / y = exact 0, except if y=0 */
    if (R_zerop(y)) { divide_0(); }
    if (R_floatp(y))
      return RA_F_exact_contagion_R(x,y);
    else
      return x;
  } else
    GEN_R_op21(x,y,durch,return);
}

/* Generates a function like R_R_floor_I_R
 Returns whole-numbered quotient and remainder
 of a division of real numbers.
 (q,r) := (rounding x y)
 R_R_rounding_I_R(x,y);
 > x,y: real numbers
 < STACK_1: quotient q, an integer
 < STACK_0: remainder r, a real number
 decrease STACK by 2
 can trigger GC
 method:
 both integers -> I_I_rounding_I_I(x,y).
 Else: R_rounding_I_R(x/y) -> (q,r). Return q and x-y*q=y*r. */
#define GEN_R_R_round(rounding)                                         \
  local maygc void CONCAT3(R_R_,rounding,_I_R) (object x, object y) {   \
    GCTRIGGER2(x,y);                                                    \
    if (N_integerp(x) && N_integerp(y)) { /* both integers? */          \
      CONCAT3(I_I_,rounding,_I_I) (x,y);  /* yes -> integer-routine */  \
    } else {                                                            \
      pushSTACK(y);                                                     \
      /* form whole-numbered part of the quotient: */                   \
      CONCAT3(R_,rounding,_I_R) (R_R_durch_R(x,y));                     \
      y = STACK_2; STACK_2 = STACK_1;                                   \
      /* multiply part behind decimal point with y: */                  \
      STACK_1 = R_R_mal_R(y,STACK_0);                                   \
      skipSTACK(1);                                                     \
    }                                                                   \
  }

/* R_R_floor_I_R(x,y) => (floor x y), with x and y being real numbers.
 Both values into the stack.
 can trigger GC */
local maygc void R_R_floor_I_R (object x, object y);
GEN_R_R_round(floor)

/* R_R_ceiling_I_R(x,y) => (ceiling x y), with x and y being real numbers.
 Both values into the stack.
 can trigger GC */
local maygc void R_R_ceiling_I_R (object x, object y);
GEN_R_R_round(ceiling)

/* R_R_truncate_I_R(x,y) => (truncate x y), with x and y being real numbers.
 Both values into the stack.
 can trigger GC */
local maygc void R_R_truncate_I_R (object x, object y);
GEN_R_R_round(truncate)

/* R_R_round_I_R(x,y) => (round x y), with x and y being real numbers.
 Both values into the stack.
 can trigger GC */
local maygc void R_R_round_I_R (object x, object y);
GEN_R_R_round(round)

/* Generates a function like R_R_mod_R
 Returns the remainder of a division of real numbers.
 (remainder x y) = (- x (* y (rounding x y)))
                 = (* y (nth-value 1 (rounding x y)))
 R_R_remainder_R(x,y)
 > x,y: real numbers
 < result: remainder r, a real number
 can trigger GC
 method:
 both integers -> I_I_remainder_I(x,y).
 else: R_rounding_I_R(x/y) -> (q,r). return x-y*q=y*r.  */
#define GEN_R_R_mod(remainder,rounding)                                 \
  local maygc object CONCAT3(R_R_,remainder,_R) (object x, object y) {  \
    GCTRIGGER2(x,y);                                                    \
    if (N_integerp(x) && N_integerp(y)) { /* both integers? */          \
      return CONCAT3(I_I_,remainder,_I) (x,y);  /* yes -> integer-routine */ \
    } else {                                                            \
      pushSTACK(y);                                                     \
      /* form whole-numbered part of the quotient: */                   \
      CONCAT3(R_,rounding,_I_R) (R_R_durch_R(x,y));                     \
      y = STACK_2; x = STACK_0; skipSTACK(3);                           \
      return R_R_mal_R(y,x); /* multiply part behind decimal point with y */ \
    }                                                                   \
  }

/* R_R_mod_R(x,y) = (mod x y), with x and y being real numbers.
 can trigger GC */
local maygc object R_R_mod_R (object x, object y);
GEN_R_R_mod(mod,floor)

/* R_R_rem_R(x,y) = (rem x y), with x and y being real numbers.
 can trigger GC */
local maygc object R_R_rem_R (object x, object y);
GEN_R_R_mod(rem,truncate)

/* Generates a function like R_R_ffloor_F_R
 Returns whole-numbered quotient (as float) and remainder \
 of a division of real numbers.                       \
 (q,r) := (frounding x y)                             \
 R_R_frounding_F_R(x,y);                              \
 > x,y: real numbers                                  \
 < STACK_1: quotient q, an integer-valued float       \
 < STACK_0: remainder r, a real number                \
 decreases STACK by 2                                 \
 can trigger GC                                       \
 method:                                                             \
 x,y both rational:                                                  \
   R_R_rounding_I_R(x,y), convert quotient into a float.             \
 else:                                                               \
   R_frounding_F_R(x/y) -> q,r. Return the values q and x-y*q = y*r. \ */
#define GEN_R_R_fround(rounding)                                        \
  local maygc void CONCAT3(R_R_f,rounding,_F_R) (object x, object y) {  \
    GCTRIGGER2(x,y);                                                    \
    if (R_rationalp(x) && R_rationalp(y)) { /* both rational numbers? */ \
      CONCAT3(R_R_,rounding,_I_R) (x,y); /* division with remainder */  \
      STACK_1 = I_float_F(STACK_1); /* turn quotient into a float */    \
    } else {                                                            \
      pushSTACK(y);                                                     \
      /* form whole-numbered part of the quotient: */                   \
      CONCAT3(R_f,rounding,_F_R) (R_R_durch_R(x,y));                    \
      y = STACK_2; STACK_2 = STACK_1;                                   \
      /* multiply part behind decimal point with y: */                  \
      STACK_1 = R_R_mal_R(y,STACK_0);                                   \
      skipSTACK(1);                                                     \
    }                                                                   \
  }

/* R_R_ffloor_F_R(x,y) => (ffloor x y), with x and y being real numbers.
 Both values into the stack.
 can trigger GC */
local maygc void R_R_ffloor_F_R (object x, object y);
GEN_R_R_fround(floor)

/* R_R_fceiling_F_R(x,y) => (fceiling x y), with x and y being real numbers.
 Both values into the stack.
 can trigger GC */
local maygc void R_R_fceiling_F_R (object x, object y);
GEN_R_R_fround(ceiling)

/* R_R_ftruncate_F_R(x,y) => (ftruncate x y), with x and y being real numbers.
 Both values into the stack.
 can trigger GC */
local maygc void R_R_ftruncate_F_R (object x, object y);
GEN_R_R_fround(truncate)

/* R_R_fround_F_R(x,y) => (fround x y), with x and y being real numbers.
 Both values into the stack.
 can trigger GC */
local maygc void R_R_fround_F_R (object x, object y);
GEN_R_R_fround(round)

/* R_1_plus_R(x) => (1+ x), with x being a real number.
 can trigger GC */
local maygc object R_1_plus_R (object x)
{ return (R_rationalp(x) ? RA_1_plus_RA(x) : R_R_plus_R(x,Fixnum_1)); }

/* R_minus1_plus_R(x) => (1- x), with x being a real number.
 can trigger GC */
local maygc object R_minus1_plus_R (object x)
{ return R_rationalp(x) ? RA_minus1_plus_RA(x) : R_R_plus_R(x,Fixnum_minus1); }

/* F_rational_RA(x) => (rational x), wo x ein Float ist.
 can trigger GC
 method:
 The mathematical value of a Float is, if INTEGER-DECODE-FLOAT returns
 the three numbers m,e,s (mantissa, exponent, sign),
 = s * 2^e * m.
 n:=m. If s<0, set n:=-m.
 if e>=0, the result is (ash n e) ,
 else the result is the rational number (/ n (ash 1 (- e))) . */
local maygc object F_rational_RA (object x)
{ F_integer_decode_float_I_I_I(x);
  /* stack layout: m, e, s. */
 {var object n = STACK_2;
  if (R_minusp(STACK_0)) { n = I_minus_I(n); } /* s<0 -> set n := (- n) */
 {var object e = STACK_1;
  skipSTACK(3);
  if (!R_minusp(e)) {
    return I_I_ash_I(n,e);  /* e>=0 -> (ash n e) */
  } else {
    pushSTACK(n);
    e = I_I_ash_I(Fixnum_1,I_minus_I(e)); /* (ash 1 (- e)) */
    return I_posI_durch_RA(popSTACK(),e); /* fraction (/ n (ash 1 (- e))) */
  }
}}}

/* R_rational_RA(x) => (rational x), with x being a real number.
 can trigger GC */
local maygc object R_rational_RA (object x)
{ return (R_rationalp(x) ? x : F_rational_RA(x)); }

/* R_R_comp(x,y) compares two real numbers x and y.
 result: 0 if x=y, +1 if x>y, -1 if x<y.
 can trigger GC
 method:
 both rational or both floats -> clear.
 one rational, one float ->
   Turnt the rational number into a float, compare.
   different? -> that's it.
   equal -> turn the float with RATIONAL into a rational, compare again. */
local maygc signean R_R_comp (object x, object y)
{
  if (R_rationalp(x)) {
    if (R_rationalp(y)) /* both rational numbers */
      return RA_RA_comp(x,y);
    else { /* x rational, y Float -> convert x into a float */
      pushSTACK(x); pushSTACK(y);
      var object xf = RA_F_float_F(x,y,false); /* convert x into a float */
      if (eq(xf,nullobj)) { /* overflow? */
        skipSTACK(1); return RA_RA_comp(popSTACK(),Fixnum_0);
      }
      var signean erg = F_F_comp(xf,STACK_0); /* and compare with y */
      if (erg!=0) { skipSTACK(2); return erg; } /* unequal -> done */
      y = F_rational_RA(popSTACK()); /* convert y into a rational number */
      return RA_RA_comp(popSTACK(),y); /* compare again */
    }
  } else {
    if (R_rationalp(y)) { /* x Float, y rational -> convert y into a float */
      pushSTACK(y); pushSTACK(x);
      var object yf = RA_F_float_F(y,x,false); /* convert y into a float */
      if (eq(yf,nullobj)) { /* overflow? */
        skipSTACK(1); return RA_RA_comp(Fixnum_0,popSTACK());
      }
      var signean erg = F_F_comp(STACK_0,yf); /* and compare with x */
      if (erg!=0) { skipSTACK(2); return erg; } /* unequal -> done */
      x = F_rational_RA(popSTACK()); /* convert x into a rational number */
      return RA_RA_comp(x,popSTACK()); /* compare again */
    } else /* both floats */
      return F_F_comp(x,y);
  }
}

/* R_R_gleich(x,y) compares two real numbers x and y.
 result: true if x=y, else false.
 method:
 When are x and y equal? According to CLTL, 2nd ed., p. 290 the
 exact mathematical values have to be compared.
 x,y both rational: (because x,y are there as shortened fractions with positive
 denominator) iff denominator and numerator are equal.
 x,y both floats: iff the signs and the exponents
   match and the mantissa of the longer one consists of the mantissa
   of the shorter one except for zeroes.
 x rational, y Float: (the exact value of y is an Integer * 2^Exponent)
   iff the signs match, the denominator of x
   is a power of two and y = (-1)^s * m * 2^e and x = a / 2^c
   are connected by the equation m * 2^(e+c) = |a| .

 test for equality of two integers: either both are EQ or both
 are Bignums of same length and same digits (including sign).
 returns wit false_statement, if not equal.
 #define I_I_gleich(x,y) (eq(x,y) || (I_bignump(x) && I_bignump(y) && (x_len==y_len) && (compare_loop_up(x_data,y_data)==0)))
*/
#define I_I_gleich(x_,y_,false_statement)                       \
  { var object _x = (x_);                                       \
    var object _y = (y_);                                       \
    if (!eq(_x,_y)) {                                           \
      if (!I_I_bignums_p(_x,_y)) { false_statement }            \
     {var uintC xlen = Bignum_length(_x);                       \
      var uintC ylen = Bignum_length(_y);                       \
      if (xlen!=ylen) { false_statement }                       \
      if (compare_loop_up(&TheBignum(_x)->data[0],              \
                          &TheBignum(_y)->data[0],xlen)!=0)     \
        { false_statement }                                     \
  }}}
local bool R_R_gleich (object x, object y)
{
  if (R_rationalp(x)) { /* x rational */
    if (R_rationalp(y)) { /* x,y both rational */
      if (RA_integerp(x)) {
        if (!RA_integerp(y)) return false;
        /* x,y both integers */
        I_I_gleich(x,y, { return false; } );
        return true;
      } else {
        if (RA_integerp(y)) return false;
        /* x,y both ratio */
        /* compare denominators: */
        I_I_gleich(TheRatio(x)->rt_den,TheRatio(y)->rt_den, {return false;} );
        /* compare numerators: */
        I_I_gleich(TheRatio(x)->rt_num,TheRatio(y)->rt_num, {return false;} );
        return true;
      }
    } else /* x rational, y float */
      { var object tmp = x; x = y; y = tmp; }
  }
  /* x float, y float or rational.
     unpack x and y, return a sign, a mantissa
     (NUDS with highest set bit) and an exponent. */
  {SAVE_NUM_STACK /* save num_stack */
   var signean x_sign;
   var uintD* x_MSDptr;
   var uintC x_len;
   var sintL x_exp;
   var signean y_sign;
   var uintD* y_MSDptr;
   var uintC y_len;
   var sintL y_exp;
   floatcase(x, { /* x SF */
     var uint32 x_mant;
     SF_decode(x, { goto x_zero; }, x_sign=,x_exp=,x_mant=);
     x_mant = x_mant << (32-(SF_mant_len+1));
     num_stack_need(32/intDsize, x_MSDptr=,); x_len = 32/intDsize;
     set_32_Dptr(x_MSDptr,x_mant);
   }, { /* x FF */
     var uint32 x_mant;
     FF_decode(x, { goto x_zero; }, x_sign=,x_exp=,x_mant=);
     x_mant = x_mant << (32-(FF_mant_len+1));
     num_stack_need(32/intDsize, x_MSDptr=,); x_len = 32/intDsize;
     set_32_Dptr(x_MSDptr,x_mant);
   }, { /* x DF */
     ifdef_intQsize({
       var uint64 x_mant;
       DF_decode(x, { goto x_zero; }, x_sign=,x_exp=,x_mant=);
       x_mant = x_mant << (64-(DF_mant_len+1));
       num_stack_need(64/intDsize, x_MSDptr=,);
       x_len = 64/intDsize;
       set_32_Dptr(&x_MSDptr[0],(uint32)(x_mant>>32));
       set_32_Dptr(&x_MSDptr[32/intDsize],(uint32)x_mant);
     }, {
       var uint32 x_manthi;
       var uint32 x_mantlo;
       DF_decode2(x, { goto x_zero; }, x_sign=,x_exp=,x_manthi=,x_mantlo=);
       x_manthi = (x_manthi << (64-(DF_mant_len+1)))
         | (x_mantlo >> ((DF_mant_len+1)-32));
       x_mantlo = x_mantlo << (64-(DF_mant_len+1));
       num_stack_need(64/intDsize, x_MSDptr=,);
       x_len = 64/intDsize;
       set_32_Dptr(&x_MSDptr[0],x_manthi);
       set_32_Dptr(&x_MSDptr[32/intDsize],x_mantlo);
     });
   }, { /* x LF */
     LF_decode(x, { goto x_zero; }, x_sign=,x_exp=,x_MSDptr=,x_len=,);
   });
   if (!R_rationalp(y)) {
     floatcase(y, { /* y SF */
       var uint32 y_mant;
       SF_decode(y, { goto y_zero; }, y_sign=,y_exp=,y_mant=);
       y_mant = y_mant << (32-(SF_mant_len+1));
       num_stack_need(32/intDsize, y_MSDptr=,); y_len = 32/intDsize;
       set_32_Dptr(y_MSDptr,y_mant);
     }, { /* y FF */
       var uint32 y_mant;
       FF_decode(y, { goto y_zero; }, y_sign=,y_exp=,y_mant=);
       y_mant = y_mant << (32-(FF_mant_len+1));
       num_stack_need(32/intDsize, y_MSDptr=,); y_len = 32/intDsize;
       set_32_Dptr(y_MSDptr,y_mant);
     }, { /* y DF */
       ifdef_intQsize({
         var uint64 y_mant;
         DF_decode(y, { goto y_zero; }, y_sign=,y_exp=,y_mant=);
         y_mant = y_mant << (64-(DF_mant_len+1));
         num_stack_need(64/intDsize, y_MSDptr=,);
         y_len = 64/intDsize;
         set_32_Dptr(&y_MSDptr[0],(uint32)(y_mant>>32));
         set_32_Dptr(&y_MSDptr[32/intDsize],(uint32)y_mant);
       }, {
         var uint32 y_manthi;
         var uint32 y_mantlo;
         DF_decode2(y, { goto y_zero; }, y_sign=,y_exp=,y_manthi=,y_mantlo=);
         y_manthi = (y_manthi << (64-(DF_mant_len+1)))
           | (y_mantlo >> ((DF_mant_len+1)-32));
         y_mantlo = y_mantlo << (64-(DF_mant_len+1));
         num_stack_need(64/intDsize, y_MSDptr=,);
         y_len = 64/intDsize;
         set_32_Dptr(&y_MSDptr[0],y_manthi);
         set_32_Dptr(&y_MSDptr[32/intDsize],y_mantlo);
       });
     }, { /* y LF */
       LF_decode(y, { goto y_zero; }, y_sign=,y_exp=,y_MSDptr=,y_len=,);
     });
   } else {
     var uintL y_den_exp;
     var uintD* y_LSDptr;
     var uintL s;
     if (RA_integerp(y))
        y_den_exp = 0;
     else {
       y_den_exp = I_power2p(TheRatio(y)->rt_den);
       if (y_den_exp == 0)
         { goto no; } /* x float, y's denominator not a power of 2 */
       y_den_exp--;
       y = TheRatio(y)->rt_num;
     }
     I_to_NDS(y,y_MSDptr=,y_len=,y_LSDptr=); /* fetch NDS */
     if (y_len == 0) { goto y_zero; }
     /* not all leading intDsize+1 bits are equal. */
     if ((sintD)y_MSDptr[0] < 0) /* if <0, negate */
       { y_sign = -1; neg_loop_down(y_LSDptr,y_len); }
     else
       { y_sign = 0; }
     /* not all leading intDsize+1 bits are =0. */
     if (y_MSDptr[0] == 0) /* normalize (remove max. 1 nulldigit) */
       { y_MSDptr++; y_len--; }
     /* now y_MSDptr[0]/=0 and y_len>0. */
     /* normalize leading bit to 1: */
     integerlengthD(y_MSDptr[0], s = intDsize - );
     if (s > 0) {
       begin_arith_call();
       shiftleft_loop_down(y_LSDptr,y_len,s,0);
       end_arith_call();
     }
     y_exp = (sintL)((uintL)y_len * intDsize - s) - (sintL)y_den_exp;
   }
   /* compare signs, exponents and mantissas: */
   if ((x_sign ^ y_sign) < 0) { goto no; }
   if (x_exp != y_exp) { goto no; }
   if (x_len > y_len) {
     if (test_loop_up(&x_MSDptr[y_len],x_len-y_len)) goto no;
     x_len = y_len;
   } else if (y_len > x_len)
     { if (test_loop_up(&y_MSDptr[x_len],y_len-x_len)) goto no; }
   if (compare_loop_up(x_MSDptr,y_MSDptr,x_len)) goto no;
   /* comparison complies. */
   RESTORE_NUM_STACK /* restore num_stack */
   return true;
  x_zero:
   RESTORE_NUM_STACK /* restore num_stack */
   if (R_zerop(y)) { return true; } else { return false; }
  y_zero:
   no:
   RESTORE_NUM_STACK /* restore num_stack */
   return false;
}}

/* EQUALP-hash-code of a real number:
 mixture of exponent, length, first 32 bits,
 but done, so that (hashcode (rational x)) = (hashcode x)
 and (hashcode 0.0) = 0 (important because of "complex canonicalization"). */
global uint32 hashcode4_real (object obj);
global uint32 hashcode4_uint32 (uint32 x);
#define hashcode4_(msd,exp,sign)                                        \
  (((((uint32)(msd) << 7) | ((uint32)(msd) >> 25)) ^ ((sint32)(sign) << 30)) \
   + (uintL)(exp))
#define hashcode4_one  hashcode4_(bit(31),1,0)
global uint32 hashcode4_real (object obj)
{
  var signean sign;
  var uint32 msd;
  var sintL exp;
  if (ratiop(obj)) {
    /* Making sure that a float and its rational equivalent have
       the same hash code is tricky. This code depends on the fact
       that the above hashcode4_() macro is linear in `exp'. */
    return hashcode4_real(TheRatio(obj)->rt_num)
      - hashcode4_real(TheRatio(obj)->rt_den)
      + hashcode4_one;
  }
  {SAVE_NUM_STACK /* save num_stack */
   if (R_rationalp(obj)) { /* obj integer */
     var uintD* MSDptr;
     var uintC len;
     var uint32 msd2;
     I_to_NDS_nocopy(obj,MSDptr=,len=,);
     /* not all leading intDsize+1 bits are equal. */
     if (len >= 64/intDsize) {
       msd = get_32_Dptr(&MSDptr[0]);
       msd2 = get_32_Dptr(&MSDptr[32/intDsize]);
     } else if (len > 32/intDsize) {
       msd = get_32_Dptr(&MSDptr[0]);
       msd2 = get_max32_Dptr(intDsize*len-32,&MSDptr[32/intDsize])
              << (64-intDsize*len);
     } else if (len == 32/intDsize) {
       msd = get_32_Dptr(&MSDptr[0]);
       msd2 = 0;
     } else if (len > 0) { /* 0 < len < 32/intDsize */
       msd = get_max32_Dptr(intDsize*len,&MSDptr[0]) << (32-intDsize*len);
       msd2 = 0;
     } else /* (len == 0) */
       { goto zero; }
     if ((sint32)msd < 0) { /* if <0, negate */
       sign = -1;
       /* msd|msd2 := - msd|msd2 - (1 if further bits /= 0) */
       msd = ~msd; msd2 = ~msd2;
       if ((len <= 64/intDsize)
           || !test_loop_up(&MSDptr[64/intDsize],len-64/intDsize))
         { msd2++; if (msd2==0) { msd++; } }
     } else
       { sign = 0; }
     exp = (uintL)len * intDsize;
     /* not all leading intDsize+1 Bits are =0.
        because of intDsize<=32: not all leading 33 bits are =0. */
     if (msd==0)
       { msd = msd2; msd2 = 0; exp -= 32; }
     /* not all leading 32 bits are =0. */
     /* normalize leading bit to 1 : */
     else {
       var uintL s;
       integerlength32(msd, s = 32 - );
       if (s > 0) { msd = (msd << s) | (msd2 >> (32-s)); }
       exp -= s;
     }
   } else { /* obj Float */
     floatcase(obj, { /* SF */
       var uint32 mant;
       SF_decode(obj, { goto zero; }, sign=,exp=,mant=);
       msd = mant << (32-(SF_mant_len+1));
     }, { /* FF */
       var uint32 mant;
       FF_decode(obj, { goto zero; }, sign=,exp=,mant=);
       msd = mant << (32-(FF_mant_len+1));
     }, { /* DF */
       ifdef_intQsize({
         var uint64 mant;
         DF_decode(obj, { goto zero; }, sign=,exp=,mant=);
         msd = mant >> ((DF_mant_len+1)-32);
       }, {
         var uint32 manthi;
         var uint32 mantlo;
         DF_decode2(obj, { goto zero; }, sign=,exp=,manthi=,mantlo=);
         msd = (manthi << (64-(DF_mant_len+1)))
           | (mantlo >> ((DF_mant_len+1)-32));
       });
     }, { /* LF */
       var uintD* MSDptr;
       LF_decode(obj, { goto zero; }, sign=,exp=,MSDptr=,,);
       msd = get_32_Dptr(MSDptr);
     });
   }
   RESTORE_NUM_STACK /* restore num_stack */
   return hashcode4_(msd,exp,sign);
  zero:
   RESTORE_NUM_STACK /* restore num_stack */
   return 0;
}}
global uint32 hashcode4_uint32 (uint32 x)
{ if (x == 0) return 0;
  /* normalize leading bit to 1: */
 {var uintL exp;
  integerlength32(x, exp = );
 {var uint32 msd = x << (32-exp);
  return hashcode4_(msd,exp,0);
}}}
global uint32 hashcode4_uint4 [16] = {
  0,
  hashcode4_( 1*(uint32)bit(31),1,0),
  hashcode4_( 2*(uint32)bit(30),2,0),
  hashcode4_( 3*(uint32)bit(30),2,0),
  hashcode4_( 4*(uint32)bit(29),3,0),
  hashcode4_( 5*(uint32)bit(29),3,0),
  hashcode4_( 6*(uint32)bit(29),3,0),
  hashcode4_( 7*(uint32)bit(29),3,0),
  hashcode4_( 8*(uint32)bit(28),4,0),
  hashcode4_( 9*(uint32)bit(28),4,0),
  hashcode4_(10*(uint32)bit(28),4,0),
  hashcode4_(11*(uint32)bit(28),4,0),
  hashcode4_(12*(uint32)bit(28),4,0),
  hashcode4_(13*(uint32)bit(28),4,0),
  hashcode4_(14*(uint32)bit(28),4,0),
  hashcode4_(15*(uint32)bit(28),4,0)
};

/* R_R_max_R(x,y) => (max x y), with x and y being real numbers.
 can trigger GC */
local maygc object R_R_max_R (object x, object y)
{ pushSTACK(x); pushSTACK(y); /* save both */
 {var object erg =
     (R_R_comp(x,y) >= 0 /* compare */
      ? STACK_1 /* x>=y -> x */
      : STACK_0); /* x<y -> y */
  skipSTACK(2);
  return erg;
}}

/* R_R_min_R(x,y) => (min x y), with x and y being real numbers.
 can trigger GC */
local maygc object R_R_min_R (object x, object y)
{ pushSTACK(x); pushSTACK(y); /* save both */
 {var object erg =
     (R_R_comp(x,y) <= 0 /* compare */
      ? STACK_1 /* x<=y -> x */
      : STACK_0); /* x>y -> y */
  skipSTACK(2);
  return erg;
}}

/* R_signum_R(x) return (signum x), with x being a real number.
 can trigger GC */
local maygc object R_signum_R (object x)
{
  if (R_rationalp(x)) { /* x rational */
    if (R_minusp(x)) return Fixnum_minus1; /* x<0 -> -1 */
    else if (eq(x,Fixnum_0)) return x; /* x=0 -> 0 */
    else return Fixnum_1; /* x>0 -> +1 */
  } else { /* x float */
    floatcase(x, { /* x SF */
      if (R_minusp(x)) return SF_minus1; /* x<0 -> -1.0 */
      else if (SF_zerop(x)) return x; /* x=0 -> 0.0 */
      else return SF_1; /* x>0 -> +1.0 */
    }, { /* x FF */
      if (R_minusp(x)) return FF_minus1; /* x<0 -> -1.0 */
      else if (FF_zerop(x)) return x; /* x=0 -> 0.0 */
      else return FF_1; /* x>0 -> +1.0 */
    }, { /* x DF */
      if (R_minusp(x)) return DF_minus1; /* x<0 -> -1.0 */
      else if (DF_zerop(x)) return x; /* x=0 -> 0.0 */
      else return DF_1; /* x>0 -> +1.0 */
    }, { /* x LF */
      if (LF_zerop(x)) return x; /* x=0 -> 0.0 */
      else /* according to sign of x */
        { encode_LF1s(LF_sign(x),Lfloat_length(x), return); }
    });
  }
}

/* R_sqrt_R(x) = (sqrt x) calculates the root of a real number x >=0.
 can trigger GC */
local maygc object R_sqrt_R (object x)
{
  if (R_rationalp(x)) { /* x rational number >=0 */
    pushSTACK(x); /* save x */
    x = RA_sqrtp(x); /* test for square */
    if (!eq(x,nullobj)) { /* was square, x is the root */
      skipSTACK(1); return x;
    } else /* convert x into float, then calculate the root: */
      return F_sqrt_F(RA_float_F(popSTACK()));
  } else
    return F_sqrt_F(x);
}
#define RA_sqrt_R  R_sqrt_R

/* R_I_expt_R(x,y) => (expt x y),
   with x being a real number and y being an integer.
 can trigger GC
 method:
 For y>0:
   a:=x, b:=y.
   As long as b is even, set a:=a*a, b:=b/2. [a^b stays invariant, = x^y.]
   c:=a.
   As long as b:=floor(b/2) >0 ,
     set a:=a*a, and if b is odd, set c:=a*c.
   result c.
 For y=0: result 1.
 Forr y<0: (/ (expt x (- y))). */
local maygc object R_I_expt_R (object x, object y)
{
  if (eq(y,Fixnum_0)) {
    /* y=0 -> result 1 */
    if (R_floatp(x))
      return RA_F_exact_contagion_R(Fixnum_1,x);
    else
      return Fixnum_1;
  }
  pushSTACK(x);
 {var bool y_negative = false;
  if (R_minusp(y)) /* take absolute value of y */
    { y = I_minus_I(y); y_negative = true; }
  /* now y>0. */
  if (R_rationalp(STACK_0)) { /* x rational? */
    x = RA_I_expt_RA(popSTACK(),y); /* yes -> faster routine */
  } else {
    pushSTACK(y);
    /* stack layout: a, b. */
    while (!I_oddp(y)) {
      STACK_1 = R_square_R(STACK_1); /* a:=a*a */
      STACK_0 = y = I_I_ash_I(STACK_0,Fixnum_minus1); /* b := (ash b -1) */
    }
    pushSTACK(STACK_1); /* c:=a */
    /* stack layout: a, b, c. */
    while (!eq(y=STACK_1,Fixnum_1)) { /* as long as b/=1 */
      STACK_1 = I_I_ash_I(y,Fixnum_minus1); /* b := (ash b -1) */
     {var object a = STACK_2 = R_square_R(STACK_2); /* a:=a*a */
      if (I_oddp(STACK_1)) STACK_0 = R_R_mal_R(a,STACK_0); /* poss. c:=a*c */
    }}
    x = STACK_0; skipSTACK(3);
  }
  /* (expt x (abs y)) is now in x. */
  return (y_negative ? R_durch_R(x) : x); /* poss. take reciprocal value */
}}

/* R_rationalize_RA(x) => (rationalize x), with x being a real number.
 can trigger GC
 Algorithm (recursively presented):
   If x is a rational number, return x.
   If x = 0.0, return 0.
   If x < 0.0, return (- (rationalize (- x))).
   If x > 0.0:
     Call (integer-decode-float x). It returns a m,e,s=1 (mantissa,
     exponent, sign).
     If e >= 0: return x = m*2^e.
     Search a rational number between a = (m-1/2)*2^e and b = (m+1/2)*2^e
     with smallest possible numerator and denominator.
     Note 1: If m is a power of 2, we ought to take a = (m-1/4)*2^e.
       But in this case the result will be x itself anyway, regardless of
       the choice of a. Therefore we can simply ignore this case.
     Note 2: At first, we need to consider the closed interval [a,b].
       but since a and b have the denominator 2^(|e|+1) whereas x itself
       has a denominator <= 2^|e|, we can restrict the seach to the open
       interval (a,b).
     So, for given a and b (0 < a < b) we are searching a rational number
     y with a <= y <= b.
     Recursive algorithm fraction_between(a,b):
       c := (ceiling a)
       if c < b
         then return c       ; because a <= c < b, c integer
         else
           ; a is not integer (otherwise we would have had c = a < b)
           k := c-1          ; k = floor(a), k < a < b <= k+1
           return y = k + 1/fraction_between(1/(b-k), 1/(a-k))
                             ; note 1 <= 1/(b-k) < 1/(a-k)

  You can see that we are actually computing a continued fraction expansion.

 Algorithm (iterative):
   If x is rational, return x.
   Call (integer-decode-float x). It returns a m,e,s (mantissa,
     exponent, sign).
   If e >= 0, return m*2^e*s. (This includes the case x = 0.0.)
   Create rational numbers a := (2*m-1)*2^(e-1) and b := (2*m+1)*2^(e-1)
   (positive and already in lowest terms because the denominator is a
   power of two and the numerator is odd).
   Start a continued fraction expansion
     p[-1] := 0, p[0] := 1, q[-1] := 1, q[0] := 0, i := 0.
   Loop
     c := (ceiling a)
     if c >= b
       then k := c-1, partial_quotient(k), (a,b) := (1/(b-k),1/(a-k)),
            goto Loop
   finally partial_quotient(c).
   Here partial_quotient(c) denotes the iteration
     i := i+1, p[i] := c*p[i-1]+p[i-2], q[i] := c*q[i-1]+q[i-2].
   At the end, return s * (p[i]/q[i]).
   This rational number is already in lowest terms because
   p[i]*q[i-1]-p[i-1]*q[i] = (-1)^i. */
local maygc object R_rationalize_RA (object x)
{ if (R_rationalp(x)) { return x; } /* x rational -> x as result. */
  F_integer_decode_float_I_I_I(x);
  /* stack layout: m, e, s. */
  if (!R_minusp(STACK_1)) { /* e>=0. */
    var object y = I_I_ash_I(STACK_2,STACK_1); /* form (ash m e) */
    if (R_minusp(STACK_0)) { y = I_minus_I(y); } /* with s<0: y := (- y) */
    skipSTACK(3); return y;
  }
  /* e<0. */
  {
    var object m2 = I_I_ash_I(STACK_2,Fixnum_1); /* 2*m */
    pushSTACK(m2); pushSTACK(I_minus1_plus_I(m2)); /* form 2*m-1 */
    STACK_1 = I_1_plus_I(STACK_1); /* form 2*m+1 */
  }
  /* stack layout: -, e, s, 2*m+1, 2*m-1. */
  /* (ash 1 (1+ (- e))) : */
  STACK_3 = I_I_ash_I(Fixnum_1,I_1_plus_I(I_minus_I(STACK_3)));
  /* stack layout: -, 2^(1-e), s, 2*m+1, 2*m-1. */
  STACK_0 = I_I_to_RT(STACK_0,STACK_3); /* (2*m-1)/(2^(1-e)) = a */
  STACK_1 = I_I_to_RT(STACK_1,STACK_3); /* (2*m+1)/(2^(1-e)) = b */
  /* stack layout: -, 2^(1-e), s, b, a. */
  pushSTACK(Fixnum_0); pushSTACK(Fixnum_1);
  pushSTACK(Fixnum_1); pushSTACK(Fixnum_0);
  /* stack layout: -, -, s, b, a, p[i-1], p[i], q[i-1], q[i]. */
  while (1) {
    RA_ceiling_I_RA(STACK_4); /* c := (ceiling a) */
    /* stack layout: ..., c, -. */
    if (RA_RA_comp(STACK_1,STACK_(5+2))<0) break; /* if c<b end of loop */
   {var object k = I_minus1_plus_I(STACK_1); /* k = c-1 */
    skipSTACK(2);
    /* "digit" k : */
    STACK_7 = k; /* save k */
    k = I_I_mal_I(k,STACK_2); /* multiply with p[i] */
    k = I_I_plus_I(k,STACK_3); /* and add p[i-1] */
    STACK_3 = STACK_2; STACK_2 = k; /* store as p[i+1] */
    k = STACK_7;
    k = I_I_mal_I(k,STACK_0); /* multiply with q[i] */
    k = I_I_plus_I(k,STACK_1); /* and add q[i-1] */
    STACK_1 = STACK_0; STACK_0 = k; /* store as q[i+1] */
   }/* calculate new b: b := (/ (- a k)) */
   {var object new_b = RA_durch_RA(RA_RA_minus_RA(STACK_4,STACK_7));
    var object old_b = STACK_5;
    STACK_5 = new_b;
    /* calculate new a: a := (/ (- b k)) */
    STACK_4 = RA_durch_RA(RA_RA_minus_RA(old_b,STACK_7));
  }}
  /* last "digit" k=c : */
 {var object q = I_I_mal_I(STACK_1,STACK_(0+2)); /* multiply c with q[i] */
  q = I_I_plus_I(q,STACK_(1+2)); /* and add q[i-1] */
  STACK_(0+2) = q; /* store as last q[i] */
 }
 {var object p = I_I_mal_I(STACK_1,STACK_(2+2)); /* multiply c with p[i] */
  p = I_I_plus_I(p,STACK_(3+2)); /* and add p[i-1], yields last p[i] */
  /* result is (s*p[i])/q[i]: */
  if (R_minusp(STACK_(6+2))) /* with s<0: (- p[i]) instead of p[i] */
    { p = I_minus_I(p); }
 {var object q = STACK_(0+2);
  skipSTACK(9+2); /* clean up stack */
  return I_I_to_RA(p,q); /* form (/ +-p[i] q[i]) */
}}}
