/* remaining float-functions */

/* Macro: distributes according to Float-type of a Float x to 4 statements.
 floatcase(x, SF_statement,FF_statement,DF_statement,LF_statement);
 x should be a variable. */
#ifdef TYPECODES
  #define floatcase(obj,SF_statement,FF_statement,DF_statement,LF_statement) \
    do {                                                                     \
      if (!number_wbit_test(as_oint(obj),float1_bit_o)) {                    \
        if (!number_wbit_test(as_oint(obj),float2_bit_o)) { SF_statement; }  \
        else { FF_statement; }                                               \
      } else {                                                               \
        if (!number_wbit_test(as_oint(obj),float2_bit_o)) { DF_statement; }  \
        else { LF_statement; }                                               \
      }                                                                      \
    } while(0)
#else
  #define floatcase(obj,SF_statement,FF_statement,DF_statement,LF_statement) \
    do {                                                                     \
      if (number_immediatep(obj)) { SF_statement; }                          \
      else {                                                                 \
        if (Record_type(obj) > Rectype_Dfloat) { FF_statement; }             \
        else if (Record_type(obj) == Rectype_Dfloat) { DF_statement; }       \
        else { LF_statement; }                                               \
      }                                                                      \
    } while(0)
#endif
/* DF_statement must not contain an #if enthalten. Hence: */
#ifdef intQsize
 #define ifdef_intQsize(A,B)  A
#else
 #define ifdef_intQsize(A,B)  B
#endif

/* Warns, if Floats of different type are combined.
 warn_floating_point_contagion();
 can trigger GC */
local maygc void warn_floating_point_contagion (void) {
  pushSTACK(CLSTEXT("Floating point operation combines numbers of different precision. "
                    "See ANSI CL 12.1.4.4 and the CLISP impnotes for details. "
                    "The result's actual precision is controlled by ~S. "
                    "To shut off this warning, set ~S to ~S."));
  pushSTACK(S(floating_point_contagion_ansi));
  pushSTACK(S(warn_on_floating_point_contagion));
  pushSTACK(NIL);
  funcall(eq(Symbol_value(S(warn_on_floating_point_contagion)),S(error))
          ? S(error) : S(warn),4);
}


/* generates a Float-operation F_op_F like F_minus_F or F_durch_F */
#define GEN_F_op1(op)                                \
    local maygc object CONCAT3(F_,op,_F) (object x)  \
    {                                                \
      GCTRIGGER1(x);                                 \
      floatcase(x,                                   \
                { return CONCAT3(SF_,op,_SF) (x); }, \
                { return CONCAT3(FF_,op,_FF) (x); }, \
                { return CONCAT3(DF_,op,_DF) (x); }, \
                { return CONCAT3(LF_,op,_LF) (x); }  \
               );                                    \
    }

/* F_minus_F(x) returns (- x), with x being a Float.
 can trigger GC */
local maygc object F_minus_F (object x);
GEN_F_op1(minus)

/* F_abs_F(x) returns (abs x), with x being a Float.
 can trigger GC */
local maygc object F_abs_F (object x) {
  return (R_minusp(x) ? F_minus_F(x) : x); /* x<0 -> (- x), x>=0 -> x */
}

/* SF_square_SF(x) returns (* x x), with x being a SF. */
#define SF_square_SF(x)  SF_SF_mal_SF(x,x)

/* FF_square_FF(x) returns (* x x), with x being a FF.
 can trigger GC */
#define FF_square_FF(x)  FF_FF_mal_FF(x,x)

/* DF_square_DF(x) returns (* x x), with x being a DF.
 can trigger GC */
#define DF_square_DF(x)  DF_DF_mal_DF(x,x)

/* F_square_F(x) returns (* x x), with x being a Float.
 can trigger GC */
local maygc object F_square_F (object x);
GEN_F_op1(square)

/* SF_durch_SF(x) returns (/ x), with x being a SF. */
#define SF_durch_SF(x)  SF_SF_durch_SF(SF_1,x)

/* FF_durch_FF(x) returns (/ x), with x being a FF.
 can trigger GC */
#define FF_durch_FF(x)  FF_FF_durch_FF(FF_1,x)

/* DF_durch_DF(x) returns (/ x), with x being a DF.
 can trigger GC */
#define DF_durch_DF(x)  DF_DF_durch_DF(DF_1,x)

/* LF_durch_LF(x) returns (/ x), with x being a LF.
 can trigger GC */
local maygc object LF_durch_LF (object x)
{
  pushSTACK(x);
  encode_LF1(Lfloat_length(x), x=);
  return LF_LF_durch_LF(x,popSTACK());
}

/* F_durch_F(x) returns (/ x), with x being a Float.
 can trigger GC */
local maygc object F_durch_F (object x);
GEN_F_op1(durch)

/* F_sqrt_F(x) returns (sqrt x), with x being a Float >=0.
 can trigger GC */
local maygc object F_sqrt_F (object x);
GEN_F_op1(sqrt)


/* Generates a Float-function with two arguments.
 The function is executed only after both arguments have been converted
 to the same Float-format (the longer of both) ; after that the
 r (=0,1 or 2) results are converted to the shorter format of
 both Float-formats.
 s (=0 or 1): As LF_LF_comp handles Long-Floats of varied lengths,
 when s=1, a SF, FF or DF only needs to be turned into a LF of
 length LF_minlen. */
#define GEN_F_op2(arg1,arg2,SF_op,FF_op,DF_op,LF_op,r,s,RETURN)    {    \
  floatcase(arg1,/* arg1 SF */ {                                        \
    floatcase(arg2, { /* arg2 SF */                                     \
      RETURN SF_op(arg1,arg2);                                          \
    }, { /* arg2 FF */                                                  \
      pushSTACK(arg2);                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion)) {          \
        pushSTACK(arg1); warn_floating_point_contagion(); arg1 = popSTACK(); \
      }                                                                 \
      arg1 = SF_to_FF(arg1); arg2 = popSTACK();                         \
      if (nullpSv(floating_point_contagion_ansi)) {                     \
        RETURN CONCAT(TO_F_,r) (FF_op(arg1,arg2),FF_to_SF);             \
      } else {                                                          \
        RETURN FF_op(arg1,arg2);                                        \
      }                                                                 \
    }, { /* arg2 DF */                                                  \
      pushSTACK(arg2);                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion)) {          \
        pushSTACK(arg1); warn_floating_point_contagion(); arg1 = popSTACK(); \
      }                                                                 \
      arg1 = SF_to_DF(arg1); arg2 = popSTACK();                         \
      if (nullpSv(floating_point_contagion_ansi)) {                     \
        RETURN CONCAT(TO_F_,r) (DF_op(arg1,arg2),DF_to_SF);             \
      } else {                                                          \
        RETURN DF_op(arg1,arg2);                                        \
      }                                                                 \
    }, { /* arg2 LF */                                                  \
      pushSTACK(arg2);                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion)) {          \
        pushSTACK(arg1); warn_floating_point_contagion(); arg1 = popSTACK(); \
        if (s==0) { arg2 = STACK_0; }                                   \
      }                                                                 \
      arg1 = SF_to_LF(arg1,CONCAT(LFlen,s)(arg2)); arg2 = popSTACK();   \
      if (nullpSv(floating_point_contagion_ansi)) {                     \
        RETURN CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_to_SF);             \
      } else {                                                          \
        RETURN LF_op(arg1,arg2);                                        \
      }                                                                 \
    });                                                                 \
  }, { /* arg1 FF */                                                    \
    floatcase(arg2, { /* arg2 SF */                                     \
      pushSTACK(arg1);                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion)) {          \
        pushSTACK(arg2); warn_floating_point_contagion(); arg2 = popSTACK(); \
      }                                                                 \
      arg2 = SF_to_FF(arg2); arg1 = popSTACK();                         \
      if (nullpSv(floating_point_contagion_ansi)) {                     \
        RETURN CONCAT(TO_F_,r) (FF_op(arg1,arg2),FF_to_SF);             \
      } else {                                                          \
        RETURN FF_op(arg1,arg2);                                        \
      }                                                                 \
    }, { /* arg2 FF */                                                  \
      RETURN FF_op(arg1,arg2);                                          \
    }, { /* arg2 DF */                                                  \
      pushSTACK(arg2);                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion)) {          \
        pushSTACK(arg1); warn_floating_point_contagion(); arg1 = popSTACK(); \
      }                                                                 \
      arg1 = FF_to_DF(arg1); arg2 = popSTACK();                         \
      if (nullpSv(floating_point_contagion_ansi)) {                     \
        RETURN CONCAT(TO_F_,r) (DF_op(arg1,arg2),DF_to_FF);             \
      } else {                                                          \
        RETURN DF_op(arg1,arg2);                                        \
      }                                                                 \
    }, { /* arg2 LF */                                                  \
      pushSTACK(arg2);                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion)) {          \
        pushSTACK(arg1); warn_floating_point_contagion(); arg1 = popSTACK(); \
        if (s==0) { arg2 = STACK_0; }                                   \
      }                                                                 \
      arg1 = FF_to_LF(arg1,CONCAT(LFlen,s)(arg2)); arg2 = popSTACK();   \
      if (nullpSv(floating_point_contagion_ansi)) {                     \
        RETURN CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_to_FF);             \
      } else {                                                          \
        RETURN LF_op(arg1,arg2);                                        \
      }                                                                 \
    });                                                                 \
  }, { /* arg1 DF */                                                    \
    floatcase(arg2, { /* arg2 SF */                                     \
      pushSTACK(arg1);                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion)) {          \
        pushSTACK(arg2); warn_floating_point_contagion(); arg2 = popSTACK(); \
      }                                                                 \
      arg2 = SF_to_DF(arg2); arg1 = popSTACK();                         \
      if (nullpSv(floating_point_contagion_ansi)) {                     \
        RETURN CONCAT(TO_F_,r) (DF_op(arg1,arg2),DF_to_SF);             \
      } else {                                                          \
        RETURN DF_op(arg1,arg2);                                        \
      }                                                                 \
    }, { /* arg2 FF */                                                  \
      pushSTACK(arg1);                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion)) {          \
        pushSTACK(arg2); warn_floating_point_contagion(); arg2 = popSTACK(); \
      }                                                                 \
      arg2 = FF_to_DF(arg2); arg1 = popSTACK();                         \
      if (nullpSv(floating_point_contagion_ansi)) {                     \
        RETURN CONCAT(TO_F_,r) (DF_op(arg1,arg2),DF_to_FF);             \
      } else {                                                          \
        RETURN DF_op(arg1,arg2);                                        \
      }                                                                 \
    }, { /* arg2 DF */                                                  \
      RETURN DF_op(arg1,arg2);                                          \
    }, { /* arg2 LF */                                                  \
      pushSTACK(arg2);                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion)) {          \
        pushSTACK(arg1); warn_floating_point_contagion(); arg1 = popSTACK(); \
        if (s==0) { arg2 = STACK_0; }                                   \
      }                                                                 \
      arg1 = DF_to_LF(arg1,CONCAT(LFlen,s)(arg2)); arg2 = popSTACK();   \
      if (nullpSv(floating_point_contagion_ansi)) {                     \
        RETURN CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_to_DF);             \
      } else {                                                          \
        RETURN LF_op(arg1,arg2);                                        \
      }                                                                 \
    });                                                                 \
  }, { /* arg1 LF */                                                    \
    floatcase(arg2, { /* arg2 SF */                                     \
      pushSTACK(arg1);                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion)) {          \
        pushSTACK(arg2); warn_floating_point_contagion(); arg2 = popSTACK(); \
        if (s==0) { arg1 = STACK_0; }                                   \
      }                                                                 \
      arg2 = SF_to_LF(arg2,CONCAT(LFlen,s)(arg1)); arg1 = popSTACK();   \
      if (nullpSv(floating_point_contagion_ansi)) {                     \
        RETURN CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_to_SF);             \
      } else {                                                          \
        RETURN LF_op(arg1,arg2);                                        \
      }                                                                 \
    }, { /* arg2 FF */                                                  \
      pushSTACK(arg1);                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion)) {          \
        pushSTACK(arg2); warn_floating_point_contagion(); arg2 = popSTACK(); \
        if (s==0) { arg1 = STACK_0; }                                   \
      }                                                                 \
      arg2 = FF_to_LF(arg2,CONCAT(LFlen,s)(arg1)); arg1 = popSTACK();   \
      if (nullpSv(floating_point_contagion_ansi)) {                     \
        RETURN CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_to_FF);             \
      } else {                                                          \
        RETURN LF_op(arg1,arg2);                                        \
      }                                                                 \
    }, { /* arg2 DF */                                                  \
      pushSTACK(arg1);                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion)) {          \
        pushSTACK(arg2); warn_floating_point_contagion(); arg2 = popSTACK(); \
        if (s==0) { arg1 = STACK_0; }                                   \
      }                                                                 \
      arg2 = DF_to_LF(arg2,CONCAT(LFlen,s)(arg1)); arg1 = popSTACK();   \
      if (nullpSv(floating_point_contagion_ansi)) {                     \
        RETURN CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_to_DF);             \
      } else {                                                          \
        RETURN LF_op(arg1,arg2);                                        \
      }                                                                 \
    }, { /* arg2 LF */                                                  \
      if (r>0 && !nullpSv(warn_on_floating_point_contagion))            \
        if (Lfloat_length(arg1) != Lfloat_length(arg2)) {               \
          pushSTACK(arg1); pushSTACK(arg2);                             \
          warn_floating_point_contagion();                              \
          arg2 = popSTACK(); arg1 = popSTACK();                         \
        }                                                               \
      CONCAT(GEN_LF_op2_,s)(arg1,arg2,LF_op,r,_EMA_ RETURN);            \
    });                                                                 \
  });                                                                   \
}

/* Auxiliary macro, if arg1 and arg2 are both LF: */
#define GEN_LF_op2_0(arg1,arg2,LF_op,r,result_assignment)     {         \
  var uintC len1 = Lfloat_length(arg1);                                 \
  var uintC len2 = Lfloat_length(arg2);                                 \
  if (len1==len2) { /* equal -> execute directly */                     \
    result_assignment LF_op(arg1,arg2);                                 \
  } else if (len1>len2) { /* -> bring arg2 to the length of arg1 */     \
    pushSTACK(arg1); arg2 = LF_extend_LF(arg2,len1); arg1 = popSTACK(); \
    if (nullpSv(floating_point_contagion_ansi)) {                       \
      result_assignment CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_shorten_LF_2); \
    } else {                                                            \
      result_assignment LF_op(arg1,arg2);                               \
    }                                                                   \
  } else { /* (len1<len2) -> bring arg1 to the length of arg2 */        \
    pushSTACK(arg2); arg1 = LF_extend_LF(arg1,len2); arg2 = popSTACK(); \
    if (nullpSv(floating_point_contagion_ansi)) {                       \
      result_assignment CONCAT(TO_F_,r) (LF_op(arg1,arg2),LF_shorten_LF_1); \
    } else {                                                            \
      result_assignment LF_op(arg1,arg2);                               \
    }                                                                   \
  }                                                                     \
 }
#define GEN_LF_op2_1(arg1,arg2,LF_op,r,result_assignment)       \
     result_assignment LF_op(arg1,arg2);
#define LF_shorten_LF_1(arg)  LF_shorten_LF(arg,len1)
#define LF_shorten_LF_2(arg)  LF_shorten_LF(arg,len2)
/* Auxiliary macro for getting the target length
   for conversion SF,FF,DF -> LF : */
#define LFlen0(arg)  Lfloat_length(arg)
#define LFlen1(arg)  LF_minlen
/* Auxiliary macro for conversion of the result back to the shorter format: */
#define TO_F_0(erg,to)  erg
#define TO_F_1(erg,to)  to(erg)
#define TO_F_2(erg,to)                                  \
     erg; /* execute operation */                       \
     {                                                  \
       STACK_1 = to(STACK_1); /* convert 1. value */    \
       STACK_0 = to(STACK_0); /* convert 2. value */    \
     }

/* F_F_plus_F(x,y) returns (+ x y), with x and y being Floats.
 can trigger GC */
local maygc object F_F_plus_F (object x, object y)
{
  GEN_F_op2(x,y,SF_SF_plus_SF,FF_FF_plus_FF,DF_DF_plus_DF,LF_LF_plus_LF,
            1,0,return)
}

/* F_F_minus_F(x,y) returns (- x y), with x and y being Floats.
 can trigger GC */
local maygc object F_F_minus_F (object x, object y)
{
  GEN_F_op2(x,y,SF_SF_minus_SF,FF_FF_minus_FF,DF_DF_minus_DF,LF_LF_minus_LF,
            1,0,return)
}

/* F_F_mal_F(x,y) returns (* x y), with x and y being Floats.
 can trigger GC */
local maygc object F_F_mal_F (object x, object y)
{
  GEN_F_op2(x,y,SF_SF_mal_SF,FF_FF_mal_FF,DF_DF_mal_DF,LF_LF_mal_LF,1,0,return)
}

/* F_F_durch_F(x,y) returns (/ x y), with x and y being Floats.
 can trigger GC */
local maygc object F_F_durch_F (object x, object y)
{
  GEN_F_op2(x,y,SF_SF_durch_SF,FF_FF_durch_FF,DF_DF_durch_DF,LF_LF_durch_LF,
            1,0,return)
}

/* F_F_comp(x,y) compares two Floats x and y.
 Result: 0 if x=y, +1 if x>y, -1 if x<y.
 can trigger GC */
local maygc signean F_F_comp (object x, object y)
{
  GEN_F_op2(x,y,SF_SF_comp,FF_FF_comp,DF_DF_comp,LF_LF_comp,0,1,return)
}


/* Generates a function like SF_ffloor_SF
 Method: x<0 -> round away from 0, else round towards 0. */
#define GEN_ffloor(F)                                   \
  local maygc object CONCAT3(F,_ffloor_,F) (object x) { \
    GCTRIGGER1(x);                                      \
    return (R_minusp(x)                                 \
            ? CONCAT3(F,_futruncate_,F) (x)             \
            : CONCAT3(F,_ftruncate_,F) (x));            \
  }

/* SF_ffloor_SF(x) returns (ffloor x), with x being a SF. */
local maygc object SF_ffloor_SF (object x);
GEN_ffloor(SF)

/* FF_ffloor_FF(x) returns (ffloor x), with x being a FF.
 can trigger GC */
local maygc object FF_ffloor_FF (object x);
GEN_ffloor(FF)

/* DF_ffloor_DF(x) returns (ffloor x), with x being a DF.
 can trigger GC */
local maygc object DF_ffloor_DF (object x);
GEN_ffloor(DF)

/* LF_ffloor_LF(x) returns (ffloor x), with x being a LF.
 can trigger GC */
local maygc object LF_ffloor_LF (object x);
GEN_ffloor(LF)

/* Generates a function like SF_fceiling_SF
 Method: x<0 -> round towards 0, else round away from 0. */
#define GEN_fceiling(F)                                   \
  local maygc object CONCAT3(F,_fceiling_,F) (object x) { \
    GCTRIGGER1(x);                                        \
    return (R_minusp(x)                                   \
            ? CONCAT3(F,_ftruncate_,F) (x)                \
            : CONCAT3(F,_futruncate_,F) (x));             \
  }

/* SF_fceiling_SF(x) returns (fceiling x), with x being a SF. */
local maygc object SF_fceiling_SF (object x);
GEN_fceiling(SF)

/* FF_fceiling_FF(x) returns (fceiling x), with x being a FF.
 can trigger GC */
local maygc object FF_fceiling_FF (object x);
GEN_fceiling(FF)

/* DF_fceiling_DF(x) returns (fceiling x), with x being a DF.
 can trigger GC */
local maygc object DF_fceiling_DF (object x);
GEN_fceiling(DF)

/* LF_fceiling_LF(x) returns (fceiling x), with x being a LF.
 can trigger GC */
local maygc object LF_fceiling_LF (object x);
GEN_fceiling(LF)


/* Generates a function like SF_fround_SF_SF */
#define GEN_fround(F,rounding)                                          \
  local maygc void CONCAT7(F,_f,rounding,_,F,_,F) (object x) {          \
    GCTRIGGER1(x);                                                      \
    pushSTACK(x);                                                       \
   {var object y = CONCAT5(F,_f,rounding,_,F) (x); /* integer part of x */ \
    x = STACK_0; STACK_0 = y;                                           \
    pushSTACK( CONCAT5(F,_,F,_minus_,F) (x,y) ); /* x-y = fractional part of x */ \
  }}

/* SF_ffloor_SF_SF(x) returns (ffloor x), with x being a SF.
 Both values into the stack. */
local maygc void SF_ffloor_SF_SF (object x);
GEN_fround(SF,floor)

/* FF_ffloor_FF_FF(x) returns (ffloor x), with x being a FF.
 Both values into the stack.
 can trigger GC */
local maygc void FF_ffloor_FF_FF (object x);
GEN_fround(FF,floor)

/* DF_ffloor_DF_DF(x) returns (ffloor x), with x being a DF.
 Both values into the stack.
 can trigger GC */
local maygc void DF_ffloor_DF_DF (object x);
GEN_fround(DF,floor)

/* LF_ffloor_LF_LF(x) returns (ffloor x), with x being a LF.
 Both values into the stack.
 can trigger GC */
local maygc void LF_ffloor_LF_LF (object x);
GEN_fround(LF,floor)

/* SF_fceiling_SF_SF(x) returns (fceiling x), with x being a SF.
 Both values into the stack. */
local maygc void SF_fceiling_SF_SF (object x);
GEN_fround(SF,ceiling)

/* FF_fceiling_FF_FF(x) returns (fceiling x), with x being a FF.
 Both values into the stack.
 can trigger GC */
local maygc void FF_fceiling_FF_FF (object x);
GEN_fround(FF,ceiling)

/* DF_fceiling_DF_DF(x) returns (fceiling x), with x being a DF.
 Both values into the stack.
 can trigger GC */
local maygc void DF_fceiling_DF_DF (object x);
GEN_fround(DF,ceiling)

/* LF_fceiling_LF_LF(x) returns (fceiling x), with x being a LF.
 Both values into the stack.
 can trigger GC */
local maygc void LF_fceiling_LF_LF (object x);
GEN_fround(LF,ceiling)

/* SF_ftruncate_SF_SF(x) returns (ftruncate x), with x being a SF.
 Both values into the stack. */
local maygc void SF_ftruncate_SF_SF (object x);
GEN_fround(SF,truncate)

/* FF_ftruncate_FF_FF(x) returns (ftruncate x), with x being a FF.
 Both values into the stack.
 can trigger GC */
local maygc void FF_ftruncate_FF_FF (object x);
GEN_fround(FF,truncate)

/* DF_ftruncate_DF_DF(x) returns (ftruncate x), with x being a DF.
 Both values into the stack.
 can trigger GC */
local maygc void DF_ftruncate_DF_DF (object x);
GEN_fround(DF,truncate)

/* LF_ftruncate_LF_LF(x) returns (ftruncate x), with x being a LF.
 Both values into the stack.
 can trigger GC */
local maygc void LF_ftruncate_LF_LF (object x);
GEN_fround(LF,truncate)

/* SF_fround_SF_SF(x) returns (fround x), with x being a SF.
 Both values into the stack. */
local maygc void SF_fround_SF_SF (object x);
GEN_fround(SF,round)

/* FF_fround_FF_FF(x) returns (fround x), with x being a FF.
 Both values into the stack.
 can trigger GC */
local maygc void FF_fround_FF_FF (object x);
GEN_fround(FF,round)

/* DF_fround_DF_DF(x) returns (fround x), with x being a DF.
 Both values into the stack.
 can trigger GC */
local maygc void DF_fround_DF_DF (object x);
GEN_fround(DF,round)

/* LF_fround_LF_LF(x) returns (fround x), with x being a LF.
 Both values into the stack.
 can trigger GC */
local maygc void LF_fround_LF_LF (object x);
GEN_fround(LF,round)


/* Generates a function like SF_round_I_SF */
#define GEN_round(F,rounding)                                           \
  local maygc void CONCAT7(F,_,rounding,_,I,_,F) (object x) {           \
    GCTRIGGER1(x);                                                      \
    CONCAT7(F,_f,rounding,_,F,_,F) (x);                                 \
    STACK_1 = CONCAT3(F,_to_,I) (STACK_1); /* integer part as Integer */ \
  }

/* SF_floor_I_SF(x) returns (floor x), with x being a SF.
 Both values into the stack. */
local maygc void SF_floor_I_SF (object x);
GEN_round(SF,floor)

/* FF_floor_I_FF(x) returns (floor x), with x being a FF.
 Both values into the stack.
 can trigger GC */
local maygc void FF_floor_I_FF (object x);
GEN_round(FF,floor)

/* DF_floor_I_DF(x) returns (floor x), with x being a DF.
 Both values into the stack.
 can trigger GC */
local maygc void DF_floor_I_DF (object x);
GEN_round(DF,floor)

/* LF_floor_I_LF(x) returns (floor x), with x being a LF.
 Both values into the stack.
 can trigger GC */
local maygc void LF_floor_I_LF (object x);
GEN_round(LF,floor)

/* SF_ceiling_I_SF(x) returns (ceiling x), with x being a SF.
 Both values into the stack. */
local maygc void SF_ceiling_I_SF (object x);
GEN_round(SF,ceiling)

/* FF_ceiling_I_FF(x) returns (ceiling x), with x being a FF.
 Both values into the stack.
 can trigger GC */
local maygc void FF_ceiling_I_FF (object x);
GEN_round(FF,ceiling)

/* DF_ceiling_I_DF(x) returns (ceiling x), with x being a DF.
 Both values into the stack.
 can trigger GC */
local maygc void DF_ceiling_I_DF (object x);
GEN_round(DF,ceiling)

/* LF_ceiling_I_LF(x) returns (ceiling x), with x being a LF.
 Both values into the stack.
 can trigger GC */
local maygc void LF_ceiling_I_LF (object x);
GEN_round(LF,ceiling)

/* SF_truncate_I_SF(x) returns (truncate x), with x being a SF.
 Both values into the stack. */
local maygc void SF_truncate_I_SF (object x);
GEN_round(SF,truncate)

/* FF_truncate_I_FF(x) returns (truncate x), with x being a FF.
 Both values into the stack.
 can trigger GC */
local maygc void FF_truncate_I_FF (object x);
GEN_round(FF,truncate)

/* DF_truncate_I_DF(x) returns (truncate x), with x being a DF.
 Both values into the stack.
 can trigger GC */
local maygc void DF_truncate_I_DF (object x);
GEN_round(DF,truncate)

/* LF_truncate_I_LF(x) returns (truncate x), with x being a LF.
 Both values into the stack.
 can trigger GC */
local maygc void LF_truncate_I_LF (object x);
GEN_round(LF,truncate)

/* SF_round_I_SF(x) returns (round x), with x being a SF.
 Both values into the stack. */
local maygc void SF_round_I_SF (object x);
GEN_round(SF,round)

/* FF_round_I_FF(x) returns (round x), with x being a FF.
 Both values into the stack.
 can trigger GC */
local maygc void FF_round_I_FF (object x);
GEN_round(FF,round)

/* DF_round_I_DF(x) returns (round x), with x being a DF.
 Both values into the stack.
 can trigger GC */
local maygc void DF_round_I_DF (object x);
GEN_round(DF,round)

/* LF_round_I_LF(x) returns (round x), with x being a LF.
 Both values into the stack.
 can trigger GC */
local maygc void LF_round_I_LF (object x);
GEN_round(LF,round)


/* Generates a function like F_fround_F_F */
#define GEN_F_fround(rounding)                                  \
  local maygc void CONCAT3(F_f,rounding,_F_F) (object x) {      \
    GCTRIGGER1(x);                                              \
    floatcase(x,                                                \
              { CONCAT3(SF_f,rounding,_SF_SF) (x); return; },   \
              { CONCAT3(FF_f,rounding,_FF_FF) (x); return; },   \
              { CONCAT3(DF_f,rounding,_DF_DF) (x); return; },   \
              { CONCAT3(LF_f,rounding,_LF_LF) (x); return; });  \
  }

/* F_ffloor_F_F(x) returns (ffloor x), with x being a Float.
 Both values into the stack.
 can trigger GC */
local maygc void F_ffloor_F_F (object x);
GEN_F_fround(floor)

/* F_fceiling_F_F(x) returns (fceiling x), with x being a Float.
 Both values into the stack.
 can trigger GC */
local maygc void F_fceiling_F_F (object x);
GEN_F_fround(ceiling)

/* F_ftruncate_F_F(x) returns (ftruncate x), with x being a Float.
 Both values into the stack.
 can trigger GC */
local maygc void F_ftruncate_F_F (object x);
GEN_F_fround(truncate)

/* F_fround_F_F(x) returns (fround x), with x being a Float.
 Both values into the stack.
 can trigger GC */
local maygc void F_fround_F_F (object x);
GEN_F_fround(round)


/* Generates a function like F_round_I_F */
#define GEN_F_round(rounding)                                           \
  local maygc void CONCAT3(F_,rounding,_I_F) (object x) {               \
    GCTRIGGER1(x);                                                      \
    floatcase(x,                                                        \
              { CONCAT3(SF_,rounding,_I_SF) (x); return; },             \
              { CONCAT3(FF_,rounding,_I_FF) (x); return; },             \
              { CONCAT3(DF_,rounding,_I_DF) (x); return; },             \
              { CONCAT3(LF_,rounding,_I_LF) (x); return; });            \
  }

/* F_floor_I_F(x) returns (floor x), with x being a Float.
 Both values into the stack.
 can trigger GC */
local maygc void F_floor_I_F (object x);
GEN_F_round(floor)

/* F_ceiling_I_F(x) returns (ceiling x), with x being a Float.
 Both values into the stack.
 can trigger GC */
local maygc void F_ceiling_I_F (object x);
GEN_F_round(ceiling)

/* F_truncate_I_F(x) returns (truncate x), with x being a Float.
 Both values into the stack.
 can trigger GC */
local maygc void F_truncate_I_F (object x);
GEN_F_round(truncate)

/* F_round_I_F(x) returns (round x), with x being a Float.
 Both values into the stack.
 can trigger GC */
local maygc void F_round_I_F (object x);
GEN_F_round(round)


/* Generates a function like F_F_floor_I_F */
#define GEN_F_F_round(rounding)                                         \
  /* Returns whole-numbered quotient and remainder                      \
     of a division of real numbers.                                     \
     (q,r) := (rounding x y)                                            \
     F_F_rounding_I_F(x,y);                                             \
     > x,y: real numbers                                                \
     < STACK_1: quotient q, an integer                                  \
     < STACK_0: remainder r, a real number                              \
     Decreases STACK by 2                                               \
     can trigger GC                                                     \
     Method:                                                            \
     F_rounding_I_F(x/y) -> (q,r). Return q and x-y*q=y*r. \ */         \
  local maygc void CONCAT3(F_F_,rounding,_I_F) (object x, object y) {   \
    GCTRIGGER2(x,y);                                                    \
    pushSTACK(y);                                                       \
    CONCAT3(F_,rounding,_I_F) (F_F_durch_F(x,y)); /* form whole-numbered part of the quotient */ \
    y = STACK_2; STACK_2 = STACK_1;                                     \
    STACK_1 = F_F_mal_F(y,STACK_0); /* multiply fractional part with y */ \
    skipSTACK(1);                                                       \
  }

/* F_F_floor_I_F(x,y) returns (floor x y), with x and y being Floats.
 Both values into the stack.
 can trigger GC */
local maygc void F_F_floor_I_F (object x, object y);
GEN_F_F_round(floor)

#if 0 /* unused */

/* F_F_ceiling_I_F(x,y) returns (ceiling x y), with x and y being Floats.
 Both values into the stack.
 can trigger GC */
local maygc void F_F_ceiling_I_F (object x, object y);
GEN_F_F_round(ceiling)

/* F_F_truncate_I_F(x,y) returns (truncate x y), with x and y being Floats.
 Both values into the stack.
 can trigger GC */
local maygc void F_F_truncate_I_F (object x, object y);
GEN_F_F_round(truncate)

/* F_F_round_I_F(x,y) returns (round x y), with x and y being Floats.
 Both values into the stack.
 can trigger GC */
local maygc void F_F_round_I_F (object x, object y);
GEN_F_F_round(round)

#endif


/* F_to_SF(x) converts a Float x into a Short-Float and thereby rounds. */
local object F_to_SF (object x)
{
  floatcase(x,
            { return x; },
            { return FF_to_SF(x); },
            { return DF_to_SF(x); },
            { return LF_to_SF(x); });
}

/* F_to_FF(x) converts a Float into a Single-Float and thereby rounds.
 can trigger GC */
local maygc object F_to_FF (object x)
{
  floatcase(x,
            { return SF_to_FF(x); },
            { return x; },
            { return DF_to_FF(x); },
            { return LF_to_FF(x); });
}

/* F_to_DF(x) converts a Float into a Double-Float and thereby rounds.
 can trigger GC */
local maygc object F_to_DF (object x)
{
  floatcase(x,
            { return SF_to_DF(x); },
            { return FF_to_DF(x); },
            { return x; },
            { return LF_to_DF(x); });
}

/* F_to_LF(x,len) converts a Float x into a Long-Float with len digits
 and thereby rounds.
 > uintC len: wished number of digits, >=LF_minlen
 can trigger GC */
local maygc object F_to_LF (object x, uintC len)
{
  floatcase(x,
            { return SF_to_LF(x,len); },
            { return FF_to_LF(x,len); },
            { return DF_to_LF(x,len); },
            { return LF_to_LF(x,len); });
}

/* F_F_float_F(x,y) converts a Float x into the Float-Format of the Float y
 and thereby rounds if necessary.
 > x,y: Floats
 < result: (float x y)
 can trigger GC */
local maygc object F_F_float_F (object x, object y)
{
  floatcase(y,
            { return F_to_SF(x); },
            { return F_to_FF(x); },
            { return F_to_DF(x); },
            { return F_to_LF(x,Lfloat_length(y)); });
}


/* Enlarges a Long-Float-length n, so that d = intDsize*n
 becomes at least d+sqrt(d)+2 .
 Method with intDsize=16:
 n -> n+1 for n<=12 because of 16n+sqrt(16n)+2 < 16(n+1)
 n -> n+2 for n<=56 because of 16n+sqrt(16n)+2 < 16(n+2)
 n -> n+4 for n<=240
 n -> n+8 for n<=992
 n -> n+16 for n<=4032
 n -> n+32 for n<=16256
 n -> n+65 for n<=65535
 In general: intDsize*n + sqrt(intDsize*n) + 2 < intDsize*(n+inc)
 <==>       sqrt(intDsize*n) + 2 < intDsize*inc
 <==>       sqrt(intDsize*n) < intDsize*inc - 2
 <==>       intDsize*n < intDsize^2*inc^2 - 4*intDsize*inc + 4
 <==>       n <= intDsize*inc^2 - 4*inc */
local uintC lf_len_extend (uintC n)
{
  var uintC inc =
 #define FITS(n,k)  ((n) <= (uintL)((intDsize*(k)-4)*(k)))
 #define n_max  (uintL)(bitm(intWCsize)-1)
 #define TEST(i)  FITS(n_max,1UL<<i) || FITS(n,1UL<<i) ? 1UL<<i :
    TEST(0) TEST(1) TEST(2) TEST(3) TEST(4) TEST(5) TEST(6) TEST(7)
    TEST(8) TEST(9) TEST(10) TEST(11) TEST(12) TEST(13)
    (fehler_LF_toolong(),0);
 #undef TEST
 #undef n_max
 #undef FITS
  if ((uintWC)(n = n+inc) < (uintWC)inc)
    fehler_LF_toolong();
  return n;
}

/* F_extend_F(x) extends the precision of a Floats x by one step
 SF -> FF -> DF -> LF(4) -> LF(5) -> LF(6) -> ...
 A Float with d mantissa bits becomes a Float with
 at least d+sqrt(d)+2 mantissa bits.
 SF -> FF because of 17+sqrt(17)+2 = 23.2 < 24
 FF -> DF because of 24+sqrt(24)+2 = 30.9 < 53
 DF -> LF(4) because of 53+sqrt(53)+2 = 62.3 < 64
 LF(n) -> LF(n+1) for n<=12 because of 16n+sqrt(16n)+2 < 16(n+1)
 LF(n) -> LF(n+2) for n<=56 because of 16n+sqrt(16n)+2 < 16(n+2)
 LF(n) -> LF(n+4) for n<=240
 LF(n) -> LF(n+8) for n<=992
 LF(n) -> LF(n+16) for n<=4032
 LF(n) -> LF(n+32) for n<=16256
 LF(n) -> LF(n+65) for n<=65535
 can trigger GC */
local maygc object F_extend_F (object x)
{
  floatcase(x,{
    return (SF_mant_len+1<=17 ? SF_to_FF(x) /* 17+sqrt(17)+2 = 23.2 < 24 */
            : SF_to_DF(x)); /* 24+sqrt(24)+2 = 30.9 < 53 */
  }, {
    return FF_to_DF(x); /* 24+sqrt(24)+2 = 30.9 < 53 */
  }, {
    return DF_to_LF(x,ceiling(63,intDsize)); /* 53+sqrt(53)+2 = 62.3 < 63 */
  }, {
    return LF_extend_LF(x,lf_len_extend(Lfloat_length(x)));
  });
}


/* F_decode_float_F_I_F(x) returns for a Float x:
 (decode-float x), all three values into the stack.
 x = 0.0 returns (0.0, 0, 1.0).
 x = (-1)^s * 2^e * m returns ((-1)^0 * 2^0 * m, e as integer, (-1)^s).
 can trigger GC */
local maygc void F_decode_float_F_I_F (object x)
{
  floatcase(x, { /* x SF */
    /* unpack x: */
    var signean sign;
    var sintWL exp;
    var uint32 mant;
    SF_decode(x, { pushSTACK(SF_0); pushSTACK(Fixnum_0); pushSTACK(SF_1);
                return; },
              sign=,exp=,mant= );
    encode_SF(0,0,mant, x=); pushSTACK(x); /*  create (-1)^0 * 2^0 * m */
    pushSTACK(L_to_FN((sintL)exp)); /* e as Fixnum */
    encode_SF(sign,1,bit(SF_mant_len), x=); pushSTACK(x); /* create (-1)^s */
    return;
  }, { /* x FF */
    /* unpack x: */
    var signean sign;
    var sintWL exp;
    var uint32 mant;
    FF_decode(x, { pushSTACK(FF_0); pushSTACK(Fixnum_0); pushSTACK(FF_1);
                return; },
              sign=,exp=,mant=);
    encode_FF(0,0,mant, x=); pushSTACK(x); /* create (-1)^0 * 2^0 * m */
    pushSTACK(L_to_FN((sintL)exp)); /* e as Fixnum */
    encode_FF(sign,1,bit(FF_mant_len), x=); pushSTACK(x); /* create (-1)^s */
    return;
  }, { /* x DF */
    /* unpack x: */
    var signean sign;
    var sintWL exp;
    ifdef_intQsize({
      var uint64 mant;
      DF_decode(x, { pushSTACK(DF_0); pushSTACK(Fixnum_0); pushSTACK(DF_1);
                  return; },
                sign=,exp=,mant= );
      encode_DF(0,0,mant, x=); pushSTACK(x); /* create (-1)^0 * 2^0 * m */
      pushSTACK(L_to_FN((sintL)exp)); /* e as Fixnum */
      encode_DF(sign,1,bit(DF_mant_len), x=); pushSTACK(x); /* create (-1)^s */
    },{
      var uint32 manthi;
      var uint32 mantlo;
      DF_decode2(x, { pushSTACK(DF_0); pushSTACK(Fixnum_0); pushSTACK(DF_1);
                   return; },
                 sign=,exp=,manthi=,mantlo= );
      encode2_DF(0,0,manthi,mantlo, x=); pushSTACK(x); /* create (-1)^0 * 2^0 * m */
      pushSTACK(L_to_FN((sintL)exp)); /* e as Fixnum */
      encode2_DF(sign,1,bit(DF_mant_len-32),0, x=); pushSTACK(x); /* create (-1)^s */
    });
    return;
  }, { /* x LF */
    /* unpack x: */
    var signean sign;
    var sintL exp;
    var uintC mantlen;
    LF_decode(x, { pushSTACK(x); /* 0.0 */
                pushSTACK(Fixnum_0); /* 0 */
                encode_LF1(mantlen, x=); pushSTACK(x); /* 1.0 */
                return; },
              sign=,exp=,,mantlen=,);
    pushSTACK(x); /* save x */
    x = allocate_lfloat(mantlen,0+LF_exp_mid,0); /* create (-1)^0 * 2^0 * m */
    copy_loop_up(&TheLfloat(STACK_0)->data[0],&TheLfloat(x)->data[0],mantlen); /* copy m in */
    STACK_0 = x; /* 1. value done */
    pushSTACK(L_to_I(exp)); /* e as Fixnum */
    encode_LF1s(sign,mantlen, x=); pushSTACK(x); /* create (-1)^s */
    return;
  });
}

/* F_exponent_L(x) returns for a Float x:
 the exponent of (decode-float x).
 x = 0.0 returns 0.
 x = (-1)^s * 2^e * m returns e. */
local sintL F_exponent_L (object x)
{
  floatcase(x, { /* x SF */
    var uintBWL uexp = SF_uexp(x);
    if (uexp==0) { return 0; }
    return (sintL)(sintWL)((uintWL)uexp - SF_exp_mid);
  }, { /* x FF */
    var uintBWL uexp = FF_uexp(ffloat_value(x));
    if (uexp==0) { return 0; }
    return (sintL)(sintWL)((uintWL)uexp - FF_exp_mid);
  }, { /* x DF */
    var uintWL uexp = DF_uexp(TheDfloat(x)->float_value_semhi);
    if (uexp==0) { return 0; }
    return (sintL)(sintWL)(uexp - DF_exp_mid);
  }, { /* x LF */
    var uintL uexp = TheLfloat(x)->expo;
    if (uexp==0) { return 0; }
    return (sintL)(uexp - LF_exp_mid);
  });
}

/* SF_I_scale_float_SF(x,delta) returns x*2^delta, with x being a SF. */
local object SF_I_scale_float_SF (object x, object delta)
{ /* method:
 x=0.0 -> x as result
 delta must be a fixnum with absolute value <= SF_exp_high-SF_exp_low .
 Form new SF with exponent increased by delta. */
  /* unpack x: */
  var signean sign;
  var sintWL exp;
  var uint32 mant;
  SF_decode(x, { return x; }, sign=,exp=,mant=);
  if (!R_minusp(delta)) { /* delta>=0 */
    var uintV udelta;
    if (I_fixnump(delta)
        && ((udelta = posfixnum_to_V(delta))
            <= (uintV)(SF_exp_high-SF_exp_low))) {
      exp = exp+udelta;
      encode_SF(sign,exp,mant, return);
    } else {
      fehler_overflow();
    }
  } else { /* delta<0 */
    var uintV udelta;
    if (I_fixnump(delta)
        && ((udelta = negfixnum_abs_V(delta))
            <= (uintV)(SF_exp_high-SF_exp_low))
        && ((oint_data_len<intVsize) || !(udelta==0))) {
      exp = exp-udelta;
      encode_SF(sign,exp,mant, return);
    } else {
      if (underflow_allowed())
        fehler_underflow();
      else
        return SF_0;
    }
  }
}

/* FF_I_scale_float_FF(x,delta) returns x*2^delta, with x being a FF.
 can trigger GC */
local maygc object FF_I_scale_float_FF (object x, object delta)
{ /* method:
 x=0.0 -> x as result
 delta must be a Fixnum with absolute value <= FF_exp_high-FF_exp_low.
 Form new FF with exponent increased by delta. */
  /* unpack x: */
  var signean sign;
  var sintWL exp;
  var uint32 mant;
  FF_decode(x, { return x; }, sign=,exp=,mant=);
  if (!R_minusp(delta)) { /* delta>=0 */
    var uintV udelta;
    if (I_fixnump(delta)
        && ((udelta = posfixnum_to_V(delta))
            <= (uintV)(FF_exp_high-FF_exp_low))) {
      exp = exp+udelta;
      encode_FF(sign,exp,mant, return);
    } else {
      fehler_overflow();
    }
  } else { /* delta<0 */
    var uintV udelta;
    if (I_fixnump(delta)
        && ((udelta = negfixnum_abs_V(delta))
            <= (uintV)(FF_exp_high-FF_exp_low))
        && ((oint_data_len<intVsize) || !(udelta==0))) {
      exp = exp-udelta;
      encode_FF(sign,exp,mant, return);
    } else {
      if (underflow_allowed())
        fehler_underflow();
      else
        return FF_0;
    }
  }
}

/* DF_I_scale_float_DF(x,delta) returns x*2^delta, with x being a DF.
 can trigger GC */
local maygc object DF_I_scale_float_DF (object x, object delta)
{ /* method:
 x=0.0 -> x as result
 delta must be a Fixnum with absolute value <= DF_exp_high-DF_exp_low.
 Form new DF with exponent increased by delta. */
  /* unpack x: */
  var signean sign;
  var sintWL exp;
 #ifdef intQsize
  var uint64 mant;
  DF_decode(x, { return x; }, sign=,exp=,mant=);
 #else
  var uint32 manthi;
  var uint32 mantlo;
  DF_decode2(x, { return x; }, sign=,exp=,manthi=,mantlo=);
 #endif
  if (!R_minusp(delta)) { /* delta>=0 */
    var uintV udelta;
    if (I_fixnump(delta)
        && ((udelta = posfixnum_to_V(delta))
            <= (uintV)(DF_exp_high-DF_exp_low))) {
      exp = exp+udelta;
     #ifdef intQsize
      encode_DF(sign,exp,mant, return);
     #else
      encode2_DF(sign,exp,manthi,mantlo, return);
     #endif
    } else {
      fehler_overflow();
    }
  } else { /* delta<0 */
    var uintV udelta;
    if (I_fixnump(delta)
        && ((udelta = negfixnum_abs_V(delta))
            <= (uintV)(DF_exp_high-DF_exp_low))
        && ((oint_data_len<intVsize) || !(udelta==0))) {
      exp = exp-udelta;
     #ifdef intQsize
      encode_DF(sign,exp,mant, return);
     #else
      encode2_DF(sign,exp,manthi,mantlo, return);
     #endif
    } else {
      if (underflow_allowed())
        fehler_underflow();
      else
        return DF_0;
    }
  }
}

/* LF_I_scale_float_LF(x,delta) returns x*2^delta, with x being a LF.
 can trigger GC */
local maygc object LF_I_scale_float_LF (object x, object delta)
{ /* method:
 delta=0 -> x as result
 x=0.0 -> x as result
 delta must be a Fixnum with absolute value <= LF_exp_high-LF_exp_low .
 Form new LF with exponent increased by delta. */
  if (eq(delta,Fixnum_0)) /* delta=0 -> x as result */
    return x;
  var uintL uexp = TheLfloat(x)->expo;
  if (uexp==0)
    return x;
  pushSTACK(x); /* save x */
  var uintV udelta;
  /* |delta| must be <= LF_exp_high-LF_exp_low < 2^32 . Like with I_to_UL: */
 #ifdef TYPECODES
  switch (typecode(delta))
 #else
    if (fixnump(delta)) {
      if (FN_positivep(delta)) goto case_posfixnum; else goto case_negfixnum;
    } else if (bignump(delta)) {
      if (BN_positivep(delta)) goto case_posbignum; else goto case_negbignum;
    } else switch (0)
 #endif
  {
    case_posfixnum: /* Fixnum >=0 */
    udelta = posfixnum_to_V(delta); goto pos;
    case_posbignum: { /* Bignum >0 */
      var Bignum bn = TheBignum(delta);
     #define IF_LENGTH(i)                                               \
      if (bn_minlength <= i) /* are exactly i digits possible at all? */ \
        if (bignum_length(bn) == i) /* exactly i digits? */             \
          /* 2^((i-1)*intDsize-1) <= obj < 2^(i*intDsize-1) */          \
          if ( (i*intDsize-1 > 32)                                      \
               && ( ((i-1)*intDsize-1 >= 32)                            \
                    || (bn->data[0] >= (uintD)bitc(32-(i-1)*intDsize)))) \
            goto overflow;                                              \
          else
      IF_LENGTH(1) { udelta = get_uint1D_Dptr(bn->data); goto pos; }
      IF_LENGTH(2) { udelta = get_uint2D_Dptr(bn->data); goto pos; }
      IF_LENGTH(3) { udelta = get_uint3D_Dptr(bn->data); goto pos; }
      IF_LENGTH(4) { udelta = get_uint4D_Dptr(bn->data); goto pos; }
      IF_LENGTH(5) { udelta = get_uint4D_Dptr(&bn->data[1]); goto pos; }
     #undef IF_LENGTH
    }
    goto overflow; /* delta too large */
    case_negfixnum: /* Fixnum <0 */
    udelta = negfixnum_to_V(delta); goto neg;
    case_negbignum: { /* Bignum <0 */
      var Bignum bn = TheBignum(delta);
     #define IF_LENGTH(i)                                               \
      if (bn_minlength <= i) /* are exactly i digits possible at all? */ \
        if (bignum_length(bn) == i) /* exactly i digits? */             \
          /* - 2^((i-1)*intDsize-1) > obj >= - 2^(i*intDsize-1) */      \
          if ( (i*intDsize-1 > 32)                                      \
               && ( ((i-1)*intDsize-1 >= 32)                            \
                    || (bn->data[0] < (uintD)(-bitc(32-(i-1)*intDsize))))) \
            goto underflow;                                             \
          else
      IF_LENGTH(1) { udelta = get_sint1D_Dptr(bn->data); goto neg; }
      IF_LENGTH(2) { udelta = get_sint2D_Dptr(bn->data); goto neg; }
      IF_LENGTH(3) { udelta = get_sint3D_Dptr(bn->data); goto neg; }
      IF_LENGTH(4) { udelta = get_sint4D_Dptr(bn->data); goto neg; }
      IF_LENGTH(5) { udelta = get_sint4D_Dptr(&bn->data[1]); goto neg; }
     #undef IF_LENGTH
    }
    goto underflow; /* delta too small */
    pos: /* udelta = delta >=0 */
    if (   ((uexp = uexp+udelta) < udelta) /* exponent-overflow? */
       #ifndef UNIX_DEC_ULTRIX_GCCBUG
         || (uexp > LF_exp_high) /* or exponent too large? */
       #endif
        )
      fehler_overflow(); /* yes -> overflow */
    break; /* else OK */
    neg: /* delta <0, udelta = 2^32+delta */
    if (((uexp = uexp+udelta) >= udelta) /* or exponent-underflow? */
        || (uexp < LF_exp_low)) /* or exponent too small? */
      goto underflow; /* yes -> underflow */
    break; /* else OK */
    default: /* unfitting Integer */
      if (!R_minusp(delta)) {
       overflow: fehler_overflow(); /* delta too large */
      } else {
       underflow: /* delta too small */
        if (underflow_allowed()) {
          fehler_underflow();
        } else {
          skipSTACK(1);
          encode_LF0(Lfloat_length(x),return);
        }
      }
  }
  var uintC mantlen = Lfloat_length(x);
  x = allocate_lfloat(mantlen,uexp,LF_sign(x)); /* new Long-Float */
  copy_loop_up(&TheLfloat(popSTACK())->data[0],&TheLfloat(x)->data[0],mantlen); /* fill */
  return x;
}

/* F_I_scale_float_F(x,delta) returns x*2^delta, with x being a Float.
 can trigger GC */
local maygc object F_I_scale_float_F (object x, object delta)
{
  floatcase(x,
            { return SF_I_scale_float_SF(x,delta); },
            { return FF_I_scale_float_FF(x,delta); },
            { return DF_I_scale_float_DF(x,delta); },
            { return LF_I_scale_float_LF(x,delta); });
}

/* F_float_radix_I(x) returns (float-radix x), with x being a Float. */
local object F_float_radix_I (object x);
#if 0
local object F_float_radix_I (object x)
{ return fixnum(2); /* always 2 as result */ }
#else /* macro spares code */
 #define F_float_radix_I(obj)  (unused(obj),fixnum(2)) /* always 2 as result */
#endif

/* F_float_sign_F(x) returns (float-sign x), with x being a Float.
 can trigger GC */
local maygc object F_float_sign_F (object x)
{ /* method: x>=0 -> result 1.0; x<0 -> result -1.0 */
  floatcase(x, { /* x SF */
    encode_SF(SF_sign(x),1,bit(SF_mant_len), return);
  }, { /* x FF */
    /*  encode_FF(FF_sign(x),1,bit(FF_mant_len), return); */
    return (!R_minusp(x) ? FF_1 : FF_minus1); /* better! */
  }, { /* x DF */
    /* ifdef_intQsize(
           encode_DF(DF_sign(x),1,bit(DF_mant_len), return); ,
           encode2_DF(DF_sign(x),1,bit(DF_mant_len-32),0, return); ) */
    return (!R_minusp(x) ? DF_1 : DF_minus1); /* better! */
  }, { /* x LF */
    encode_LF1s(LF_sign(x),Lfloat_length(x), return);
  });
}

/* F_F_float_sign_F(x) returns (float-sign x y), with x and y being Floats.
 can trigger GC */
local maygc object F_F_float_sign_F (object x, object y)
{ /* method:
 if x<0 xor y<0, result (- y), else result y. */
  return (!same_sign_p(x,y) ? F_minus_F(y) : y);
}

/* F_float_digits(x) returns (float-digits x), with x being a Float.
 < result: a uintL >0 */
local uintL F_float_digits (object x)
{
  floatcase(x,
            { return SF_mant_len+1; }, /* 17 */
            { return FF_mant_len+1; }, /* 24 */
            { return DF_mant_len+1; }, /* 53 */
            { return intDsize*(uintL)Lfloat_length(x); }); /* 16n */
}

/* F_float_digits_I(x) returns (float-digits x), with x being a Float.
 < result: an Integer >0
 can trigger GC */
local maygc object F_float_digits_I (object x)
{
  floatcase(x,
            { return fixnum(SF_mant_len+1); }, /* Fixnum 17 */
            { return fixnum(FF_mant_len+1); }, /* Fixnum 24 */
            { return fixnum(DF_mant_len+1); }, /* Fixnum 53 */
            {
              var uintL bitcount = intDsize*(uintL)Lfloat_length(x); /* 16n */
              /* intDsize*2^intWCsize <= 2^oint_data_len ? */
              return (log2_intDsize+intWCsize<=oint_data_len
                      ? fixnum(bitcount)
                      : UL_to_I(bitcount));
            });
}

/* F_float_precision_I(x) returns (float-precision x), with x being a Float.
 < result: an Integer >=0
 can trigger GC */
local maygc object F_float_precision_I (object x)
{ /* method: if x=0.0, result 0, else (float-digits x). */
  floatcase(x,{
    if (SF_zerop(x))
      return Fixnum_0;
    return fixnum(SF_mant_len+1); /* Fixnum 17 */
  },{
    if (FF_zerop(x))
      return Fixnum_0;
    return fixnum(FF_mant_len+1); /* Fixnum 24 */
  },{
    if (DF_zerop(x))
      return Fixnum_0;
    return fixnum(DF_mant_len+1); /* Fixnum 53 */
  },{
    if (LF_zerop(x))
      return Fixnum_0;
    var uintL bitcount = intDsize*(uintL)Lfloat_length(x); /* 16n */
    return (log2_intDsize+intWCsize<=oint_data_len /* intDsize*2^intWCsize <= 2^oint_data_len ? */
            ? fixnum(bitcount)
            : UL_to_I(bitcount));
  });
}

/* F_integer_decode_float_I_I_I(x) returns for a Float x:
 (integer-decode-float x), all three value into the stack.
 x = 0.0 returns (0, 0, 1).
 x = (-1)^s * 2^e * m with float-precision p returns
   (mantissa 2^p * m as integer, e-p as integer, (-1)^s as fixnum).
 can trigger GC */
local maygc void F_integer_decode_float_I_I_I (object x)
{
  var object x_sign = (!R_minusp(x) ? Fixnum_1 : Fixnum_minus1); /* sign of x (not threatened by GC!) */
  floatcase(x,{ /* x SF */
    /* unpack x: */
    var sintWL exp;
    var uint32 mant;
    SF_decode(x, { goto zero; }, _EMA_,exp=,mant=);
    pushSTACK(fixnum(mant)); /* mantissa as fixnum (>0, <2^17) */
    pushSTACK(L_to_FN((sintL)(exp-(SF_mant_len+1)))); /* e-17 as fixnum */
  },{ /* x FF */
    /* unpack x: */
    var sintWL exp;
    var uint32 mant;
    FF_decode(x, { goto zero; }, _EMA_,exp=,mant=);
    pushSTACK( /* mantissa (>0, <2^24) as Integer */
              (FF_mant_len+1 <= oint_data_len
               ? fixnum(mant) /* mantissa as Fixnum */
               : UL_to_I(mant))); /* or poss. as Bignum */
    pushSTACK(L_to_FN((sintL)(exp-(FF_mant_len+1)))); /* e-24 as Fixnum */
  },{ /* x DF */
    /* unpack x: */
    var sintWL exp;
    ifdef_intQsize({
      var uint64 mant;
      DF_decode(x, { goto zero; }, _EMA_,exp=,mant=);
      pushSTACK(Q_to_I(mant)); /* mantissa (>0, <2^53) as Bignum */
    },{
      var uint32 manthi;
      var uint32 mantlo;
      DF_decode2(x, { goto zero; }, _EMA_,exp=,manthi=,mantlo=);
      pushSTACK(L2_to_I(manthi,mantlo)); /* mantissa (>0, <2^53) as Bignum */
    });
    pushSTACK(L_to_FN((sintL)(exp-(DF_mant_len+1)))); /* e-53 as Fixnum */
  }, { /* x LF */
    var uintL uexp = TheLfloat(x)->expo;
    if (uexp == 0)
      goto zero;
    pushSTACK(x); /* save x */
    var uintC len = Lfloat_length(x); /* number of mantissa digits */
    var uintC len1 = len+1; /* need one more digit */
    if (uintWCoverflow(len1)) { fehler_LF_toolong(); }
    /* intDsize*len >= 53 implies intDsize*len >= 64 >= oint_data_len+1,
       hence len >= bn_minlength. */
    {
      var object mant = allocate_bignum(len1,0); /* integer for mantissa */
      var uintD* mantptr = &TheBignum(mant)->data[0];
      *mantptr++ = 0; /* one nulldigit in front, so that it becomes a NDS */
      copy_loop_up(&TheLfloat(STACK_0)->data[0],mantptr,len); /* copy NUDS */
      STACK_0 = mant; /* 1. value done */
    }
    { /* form e-16n = uexp-LF_exp_mid-16n as Integer: */
      var uintL sub = LF_exp_mid + intDsize*(uintL)len;
      pushSTACK(UL_UL_minus_I(uexp,sub));
    }
  });
  pushSTACK(x_sign);
  return;
 zero:
  pushSTACK(Fixnum_0); pushSTACK(Fixnum_0); pushSTACK(Fixnum_1); return;
}
