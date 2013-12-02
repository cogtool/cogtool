/*
 * Arithmetics for CLISP
 * Bruno Haible 1990-2005
 * Sam Steingold 1998-2004
 * German comments translated into English: Stefan Kain 2002-12-23
 */

#include "lispbibl.c"

#define LISPARIT  /* in the following not only macros, but also functions */

#undef LF         /* LF here does not mean 'Linefeed', but 'LongFloat' */
#undef truncate   /* undo a possible "#define truncate truncate64" from LFS */
#undef ftruncate  /* undo a possible "#define ftruncate ftruncate64" from LFS */


/* UP: decides over number equality
 number_gleich(x,y)
 > x,y: two numbers
 < result: true, if (= x y)  */
global bool number_gleich (object x, object y);
#define N_N_gleich  number_gleich  /* N_N_gleich is defined later */


/* arithmetics in general: */
#include "aridecl.c"  /* declarations */
#include "arilev0.c"  /* machine-arithmetics */
#include "arilev1.c"  /* digit-sequences */
/* for integers: */
#include "intelem.c"  /* elementary operations on integers */
#include "intlog.c"   /* logical operations on integers */
#include "intplus.c"  /* addition, subtraction on integers */
#include "intcomp.c"  /* comparison operations on integers */
#include "intbyte.c"  /* byte-operations LDB, LOAD-BYTE, ... */
#include "intmal.c"   /* multiplication of integers */
#include "intdiv.c"   /* division of Integers */
#include "intgcd.c"   /* gcd and lcm */
#include "int2adic.c" /* operations with 2-adic integers */
#include "intsqrt.c"  /* Root, ISQRT */
#include "intprint.c" /* auxiliary function for output of integers */
#include "intread.c"  /* auxiliary function for input  of integers */
#include "intserial.c" /* serialization of integers */
/* for rational numbers: */
#include "rational.c" /* rational numbers */
/* for floats: */
#include "sfloat.c"   /* Short-Float basic functions */
#include "ffloat.c"   /* Single-Float basic functions */
#include "dfloat.c"   /* Double-Float basic functions */
#include "lfloat.c"   /* Long-Float basic functions */
#include "flo_konv.c" /* float conversions */
#include "flo_rest.c" /* floats in general */
/* for real numbers: */
#include "realelem.c" /* elementary functions for real numbers */
#include "realrand.c" /* functions for random numbers */
#include "realtran.c" /* transcendental functions for real numbers */
/* for complex numbers: */
#include "compelem.c" /* elementary functions for complex numbers */
#include "comptran.c" /* transcendental functions for complex numbers */


/* ==========================================================================
 *                 input routines for numbers */

/* UP: multiplies an integer with 10 and adds a second digit.
 mal_10_plus_x(y,x)
 > y: integer Y (>=0)
 > x: digit value X (>=0,<10)
 < result: integer Y*10+X (>=0)
 can trigger GC */
global maygc object mal_10_plus_x (object y, uintB x) {
  SAVE_NUM_STACK
  var uintD* MSDptr;
  var uintC len;
  var uintD* LSDptr;
  I_to_NDS_1(y, MSDptr=,len=,LSDptr=); /* NDS for Y */
  begin_arith_call();
  var uintD carry = mulusmall_loop_down(10,LSDptr,len,x); /* *10 +x */
  end_arith_call();
  if (!(carry==0)) {
    *--MSDptr = carry; len++;
    if (uintWCoverflow(len)) { /* overflow of length? */
      RESTORE_NUM_STACK; BN_ueberlauf();
    }
  }
  var object result = UDS_to_I(MSDptr,len); /* UDS as integer */
  RESTORE_NUM_STACK
  return result;
}

/* UP: Converts a string with integer-syntax into an integer.
 dots are skipped.
 read_integer(base,sign,string,index1,index2)
 > base: reading base (>=2, <=36)
 > sign: sign (/=0 if negative)
 > string: simple-string (contains digits with value <base and poss. dot)
 > index1: index of the first digit
 > index2: index after the last digit
   (that is index2-index1 digits, incl. poss. decimal point at the end)
 < result: integer
 can trigger GC */
global maygc object read_integer (uintWL base, signean sign, object string,
                                  uintL index1, uintL index2) {
  var const chart* charptr;
  unpack_sstring_alloca(string,index2-index1,index1, charptr=);
  var object x = /* convert into integer: */
    DIGITS_to_I(charptr,index2-index1,(uintD)base);
  if (sign==0)
    return x;
  else
    return I_minus_I(x); /* negative sign -> change sign */
}

/* UP: converts a string with rational-syntax into a rational number.
 read_rational(base,sign,string,index1,index3,index2)
 > base: reading base (>=2, <=36)
 > sign: sign (/=0 if negative)
 > string: normal-simple-string (contains digits with value <base
           and fraction bar)
 > index1: index of the first digit
 > index3: index of '/'
 > index2: index after the last digit
   (that is index3-index1 numerator-digits, index2-index3-1 denominator-digits)
 < result: rational number
 can trigger GC */
global maygc object read_rational (uintWL base, signean sign, object string,
                                   uintL index1, uintL index3, uintL index2) {
  pushSTACK(string); /* save string */
  {
    var uintL index3_1 = index3+1; /* index of the first denominator digit */
    var object x = /* denominator */
      DIGITS_to_I(&TheSnstring(string)->data[index3_1],index2-index3_1,
                  (uintD)base);
    if (eq(x,Fixnum_0)) /* catch division by 0 */
      divide_0();
    string = STACK_0; STACK_0 = x;
  }
  {
    var object x = /* numerator */
      DIGITS_to_I(&TheSnstring(string)->data[index1],index3-index1,(uintD)base);
    if (!(sign==0))
      x = I_minus_I(x); /* incl. sign */
    return I_posI_durch_RA(x,popSTACK()); /* numerator/denominator */
  }
}

/* UP: converts a string with float-syntax into a float.
 read_float(base,sign,string,index1,index4,index2,index3)
 > base: reading base (=10)
 > sign: sign (/=0 if negative)
 > string: normal-simple-string (contains digits and poss.
           dot and exponent marker)
 > index1: index of start of mantissa (excl. sign)
 > index4: index after the end of the mantissa
 > index2: index at the end of the characters
 > index3: index after the decimal point (=index4 if there is none)
   (that is mantissa with index4-index1 characters: digits and max. 1 '.')
   (that is index4-index3 digits behind the decimal point)
   (at index4<index2: index4 = index of the exponent-marker,
    index4+1 = index of the exponent-sign or the first
    digit of the exponent)
 < result: Float
 can trigger GC */
global maygc object read_float (uintWL base, signean sign, object string,
                                uintL index1, uintL index4, uintL index2,
                                uintL index3) {
  pushSTACK(string); /* save string */
  /* exponent: */
  var chart exp_marker;
  var object exponent;
  var uintL exp_len = index2-index4; /* number of digits of exponent */
  if (exp_len > 0) {
    /* points to the exponent marker: */
    var const chart* ptr = &TheSnstring(string)->data[index4];
    exp_marker = *ptr++; exp_len--; /* skip exponent marker (as capital
                                       latter, converted by the caller) */
    var signean exp_sign = 0; /* exponent-sign */
    switch (as_cint(*ptr)) {
      case '-': exp_sign = ~exp_sign; /* sign := negative */
      case '+': ptr++; exp_len--; /* skip exponent-sign */
      default: ;
    }
    /* convert exponent into integer: */
    exponent = DIGITS_to_I(ptr,exp_len,(uintD)base);
    if (exp_sign!=0)
      exponent = I_minus_I(exponent); /* incl. sign */
  } else { /* no exponent there */
    exp_marker = ascii('E'); exponent = Fixnum_0;
  }
  /* exp_marker = exponentmarker as capital letter,
     exponent = exponent as integer. */
  exponent = /* exponent - number of trailing digits */
    I_I_minus_I(exponent,fixnum(index4-index3));
  exponent = /* 10^exponent = power of 10 to be multiplied */
    R_I_expt_R(fixnum(base),exponent);
  string = STACK_0; STACK_0 = exponent;
  /* mantissa: */
  var object mantisse = /* mantissa as integer */
    DIGITS_to_I(&TheSnstring(string)->data[index1],index4-index1,(uintD)base);
  exponent = popSTACK();
  /* mantissa (integer) and exponent (rational >0) inelegant to multiply: */
  if (RA_integerp(exponent)) {
    mantisse = I_I_mal_I(mantisse,exponent);
  } else {
    /* if mantissa/=0, replace in exponent=1/10^i the numerator by mantissa
       (returns unshortened fraction, caution!) */
    if (!(eq(mantisse,Fixnum_0))) {
      TheRatio(exponent)->rt_num = mantisse; mantisse = exponent;
    }
  }
  /* mantissa = mantissa * power of ten, as unshortened rationale number! */
  switch (as_cint(exp_marker)) {
    case 'S': SF: { /* convert into a short-float */
      var object x = RA_to_SF(mantisse,true);
      return (sign==0 ? x : SF_minus_SF(x)); /* poss. still a sign-change */
    }
    case 'F': FF: { /* convert into a single-float */
      var object x = RA_to_FF(mantisse,true);
      return (sign==0 ? x : FF_minus_FF(x)); /* poss. still a sign-change */
    }
    case 'D': DF: { /* convert into a double-float */
      var object x = RA_to_DF(mantisse,true);
      return (sign==0 ? x : DF_minus_DF(x)); /* poss. still a sign-change */
    }
    case 'L': LF: { /* convert into a long-float of default-exactness */
      var object x = RA_to_LF(mantisse,I_to_UL(O(LF_digits)),true);
      return (sign==0 ? x : LF_minus_LF(x)); /* poss. still a sign-change */
    }
    default: /* case 'E': */
      defaultfloatcase(S(read_default_float_format),Fixnum_0,
                       goto SF, goto FF, goto DF, goto LF,
                       pushSTACK(mantisse), mantisse = popSTACK());
  }
}


/* ===========================================================================
 *                       output routines for numbers */

/* UP: prints an integer.
 print_integer(z,base,&stream);
 > z: integer
 > base: base (>=2, <=36)
 > stream: stream
 < stream: stream
 can trigger GC */
global maygc void print_integer (object z, uintWL base, const gcv_object_t* stream_)
{
  if (R_minusp(z)) { /* z<0 -> print sign: */
    pushSTACK(z);
    write_ascii_char(stream_,'-');
    z = I_minus_I(popSTACK());
  }
  {
    SAVE_NUM_STACK
    var uintD* MSDptr;
    var uintC len;
    I_to_NDS(z, MSDptr=,len=,); /* z as UDS */
    var uintL need = digits_need(len,base);
    var DYNAMIC_STRING(digits,need);
    pushSTACK(digits);
    var DIGITS erg; erg.LSBptr = &TheSnstring(digits)->data[need];
    UDS_to_DIGITS(MSDptr,len,(uintD)base,&erg); /* conversion into digits */
    /* print digits: */
    write_char_array(stream_,&STACK_0,erg.MSBptr-&TheSnstring(digits)->data[0],
                     erg.len);
    FREE_DYNAMIC_STRING(STACK_0);
    skipSTACK(1);
    RESTORE_NUM_STACK
  }
}

/* UP: prints a float.
 print_float(z,&stream);
 > z: float
 > stream: Stream
 < stream: stream
 can trigger GC */
global maygc void print_float (object z, const gcv_object_t* stream_) {
  /* if SYS::WRITE-FLOAT-DECIMAL is defined, call
     (SYS::WRITE-FLOAT-DECIMAL stream z) : */
  var object fun = Symbol_function(S(write_float_decimal));
  if (boundp(fun)) { /* call function */
    pushSTACK(*stream_); pushSTACK(z); funcall(fun,2);
  } else {
    /* own routine: prints
       sign, point, mantissa (binary),
       (binarysystem-)Exponent (decimal). */
    pushSTACK(z);
    F_integer_decode_float_I_I_I(z);
    /* stack layout: z, m, e, s. */
    /* print sign, if <0: */
    if (eq(STACK_0,Fixnum_minus1))
      write_ascii_char(stream_,'-');
    /* print mantissa binary(!) : */
    write_ascii_char(stream_,'.');
    print_integer(STACK_2,2,stream_);
    { /* print exponent-marker: */
      var object exp_marker;
      floatcase(STACK_3,
                { exp_marker = ascii_char('s'); },
                { exp_marker = ascii_char('f'); },
                { exp_marker = ascii_char('d'); },
                { exp_marker = ascii_char('L'); });
      write_char(stream_,exp_marker);
    }
    /* print exponents decimally: */
    print_integer(L_to_I(F_exponent_L(STACK_3)),10,stream_);
    skipSTACK(4);
  }
}


/* ===========================================================================
 *                           Lisp-functions */

/* error-message because of illegal digits-argument obj.
 > obj: object */
nonreturning_function(local, fehler_digits, (object obj)) {
  pushSTACK(obj);                /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_posfixnum1)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: argument should be a positive fixnum, not ~S"));
}

/* check_number(obj)
 > obj: an object
 < result: a number, either the same as obj or a replacement
 can trigger GC */
local maygc object check_number_replacement (object obj);
static inline maygc object check_number (object obj) {
  if (!numberp(obj))
    obj = check_number_replacement(obj);
  return obj;
}
/* check_number_replacement(obj)
 > obj: not a number
 < result: a number, a replacement
 can trigger GC */
local maygc object check_number_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);       /* TYPE-ERROR slot DATUM */
    pushSTACK(S(number)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a number"));
    obj = value1;
  } while (!numberp(obj));
  return obj;
}

/* check_real_replacement(obj)
 > obj: not a real number
 < result: a real number, a replacement
 can trigger GC */
global maygc object check_real_replacement (object obj) {
  for (;;) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);     /* TYPE-ERROR slot DATUM */
    pushSTACK(S(real)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a real number"));
    obj = value1;
    if_realp(obj, break; , continue; );
  }
  return obj;
}

/* check_float(obj)
 > obj: an object
 < result: a floating-point number, either the same as obj or a replacement
 can trigger GC */
local maygc object check_float_replacement (object obj);
static inline maygc object check_float (object obj) {
  if (!floatp(obj))
    obj = check_float_replacement(obj);
  return obj;
}
/* check_float_replacement(obj)
 > obj: not a floating-point number
 < result: a floating-point number, a replacement
 can trigger GC */
local maygc object check_float_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);      /* TYPE-ERROR slot DATUM */
    pushSTACK(S(float)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a floating-point number"));
    obj = value1;
  } while (!floatp(obj));
  return obj;
}

/* check_rational(obj)
 > obj: an object
 < result: a rational number, either the same as obj or a replacement
 can trigger GC */
local maygc object check_rational_replacement (object obj);
static inline maygc object check_rational (object obj) {
  if_rationalp(obj, ; , { obj = check_rational_replacement(obj); });
  return obj;
}
/* check_rational_replacement(obj)
 > obj: not a rational number
 < result: a rational number, a replacement
 can trigger GC */
local maygc object check_rational_replacement (object obj) {
  for (;;) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);     /* TYPE-ERROR slot DATUM */
    pushSTACK(S(rational)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a rational number"));
    obj = value1;
    if_rationalp(obj, break; , continue; );
  }
  return obj;
}

/* UP: Returns the decimal string representation of an integer >= 0.
 decimal_string(x)
 > object x: an integer >= 0
 < object result: a normal-simple-string containing the digits
 can trigger GC */
global maygc object decimal_string (object x) {
  SAVE_NUM_STACK
  var uintD* MSDptr;
  var uintC len;
  I_to_NDS(x, MSDptr=,len=,); /* x (>=0) as UDS */
  var uintL need = digits_need(len,10);
  var DYNAMIC_ARRAY(ziffern,chart,need); /* space for the digits */
  var DIGITS erg; erg.LSBptr = &ziffern[need];
  UDS_to_DIGITS(MSDptr,len,10,&erg); /* conversion into digits */
  /* write digits into normal-simple-string: */
  check_stringsize(erg.len);
  var object string = allocate_string(erg.len);
  if (erg.len > 0) {
    var const chart* p = erg.MSBptr;
    var cint32* q = &TheS32string(string)->data[0];
    var uintL count;
    dotimespL(count,erg.len, { *q++ = as_cint(*p++); });
  }
  FREE_DYNAMIC_ARRAY(ziffern);
  RESTORE_NUM_STACK
  return string;
}

LISPFUN(decimal_string,seclass_no_se,1,0,norest,nokey,0,NIL)
{ /* (SYS::DECIMAL-STRING integer)
 returns for an integer >=0  (write-to-string integer :base 10 :radix nil),
 which is the sequence of digits as a simple-string. */
  var object x = check_integer(popSTACK());
  VALUES1(decimal_string(x));
}

LISPFUNNF(zerop,1)
{ /* (ZEROP number), CLTL p. 195 */
  var object x = check_number(popSTACK());
  VALUES_IF(N_zerop(x));
}

LISPFUNNF(plusp,1)
{ /* (PLUSP real), CLTL p. 196 */
  var object x = check_real(popSTACK());
  VALUES_IF(R_plusp(x));
}

LISPFUNNF(minusp,1)
{ /* (MINUSP real), CLTL p. 196 */
  var object x = check_real(popSTACK());
  VALUES_IF(R_minusp(x));
}

LISPFUNNF(oddp,1)
{ /* (ODDP integer), CLTL p. 196 */
  var object x = check_integer(popSTACK());
  VALUES_IF(I_oddp(x));
}

LISPFUNNF(evenp,1)
{ /* (EVENP integer), CLTL p. 196 */
  var object x = check_integer(popSTACK());
  VALUES_IF(! I_oddp(x));
}

/* UP: tests, if all argcount+1 arguments below args_pointer
 are numbers. if not, error.
 > argcount: number of arguments-1
 > args_pointer: pointer to the arguments */
local maygc void test_number_args (uintC argcount, gcv_object_t* args_pointer) {
  do {
    var gcv_object_t* argptr = &NEXT(args_pointer);
    var object arg = *argptr; /* next argument */
    if (!numberp(arg)) /* must be a number */
      *argptr = check_number(arg);
  } while (argcount--); /* sic: not --argcount! */
}

/* UP: tests, if all argcount+1 arguments below args_pointer
 are real numbers. if not, Error.
 > argcount: number of arguments-1
 > args_pointer: pointer to the arguments */
local maygc void test_real_args (uintC argcount, gcv_object_t* args_pointer) {
  do {
    var gcv_object_t* argptr = &NEXT(args_pointer);
    var object arg = *argptr; /* next argument */
    if_realp(arg, ; , { *argptr = check_real(arg); });
  } while (argcount--); /* sic: not --argcount! */
}

/* UP: Testet, ob alle argcount+1 Argumente unterhalb von args_pointer
 ganze Zahlen sind. Wenn nein, Error.
 > argcount: number of arguments-1
 > args_pointer: pointer to the arguments */
local maygc void test_integer_args (uintC argcount, gcv_object_t* args_pointer) {
  do {
    var gcv_object_t* argptr = &NEXT(args_pointer);
    var object arg = *argptr; /* next argument */
    if (!integerp(arg)) /* must be a integer */
      *argptr = check_integer(arg);
  } while (argcount--); /* sic: not --argcount! */
}

LISPFUN(gleich,seclass_foldable,1,0,rest,nokey,0,NIL)
{ /* (= number {number}), CLTL p. 196 */
  var gcv_object_t* args_pointer = rest_args_pointer STACKop 1;
  test_number_args(argcount,args_pointer); /* all arguments numbers? */
  /* method:
     n+1 arguments Arg[0..n].
     for i:=0 to n-1 do ( if Arg[i]/=Arg[i+1] then return(NIL) ), return(T). */
  if (argcount > 0) {
    var const gcv_object_t* arg_i_ptr = args_pointer;
    dotimespC(argcount,argcount, {
      var object arg_i = NEXT(arg_i_ptr);
      if (!N_N_gleich(arg_i,Next(arg_i_ptr))) goto no;
    });
  }
 yes:
  value1 = T; goto ok;
 no:
  value1 = NIL; goto ok;
 ok:
  mv_count=1; set_args_end_pointer(args_pointer);
}

LISPFUN(ungleich,seclass_foldable,1,0,rest,nokey,0,NIL)
{ /* (/= number {number}), CLTL p. 196 */
  var gcv_object_t* args_pointer = rest_args_pointer STACKop 1;
  test_number_args(argcount,args_pointer); /* all arguments numbers? */
  /* method:
     n+1 arguments Arg[0..n].
     for j:=1 to n do
     for i:=0 to j-1 do
     if Arg[i]=Arg[j] then return(NIL),
     return(T). */
  if (argcount > 0) {
    var const gcv_object_t* arg_j_ptr = rest_args_pointer;
    dotimespC(argcount,argcount, {
      var const gcv_object_t* arg_i_ptr = args_pointer;
      do { if (N_N_gleich(NEXT(arg_i_ptr),Next(arg_j_ptr))) goto no; }
      while (arg_i_ptr != arg_j_ptr);
      arg_j_ptr skipSTACKop -1;
    });
  }
 yes:
  value1 = T; goto ok;
 no:
  value1 = NIL; goto ok;
 ok:
  mv_count=1; set_args_end_pointer(args_pointer);
}

LISPFUN(kleiner,seclass_foldable,1,0,rest,nokey,0,NIL)
{ /* (< real {real}), CLTL p. 196 */
  var gcv_object_t* args_pointer = rest_args_pointer STACKop 1;
  test_real_args(argcount,args_pointer); /* all arguments real numbers? */
  /* method:
     n+1 arguments Arg[0..n].
     for i:=0 to n-1 do ( if Arg[i]>=Arg[i+1] then return(NIL) ), return(T). */
  if (argcount > 0) {
    var const gcv_object_t* arg_i_ptr = args_pointer;
    dotimespC(argcount,argcount, {
      var object arg_i = NEXT(arg_i_ptr);
      if (R_R_comp(arg_i,Next(arg_i_ptr))>=0) goto no;
    });
  }
 yes:
  value1 = T; goto ok;
 no:
  value1 = NIL; goto ok;
 ok:
  mv_count=1; set_args_end_pointer(args_pointer);
}

LISPFUN(groesser,seclass_foldable,1,0,rest,nokey,0,NIL)
{ /* (> real {real}), CLTL p. 196 */
  var gcv_object_t* args_pointer = rest_args_pointer STACKop 1;
  test_real_args(argcount,args_pointer); /* all arguments real numbers? */
  /* method:
     n+1 arguments Arg[0..n].
     for i:=0 to n-1 do ( if Arg[i]<=Arg[i+1] then return(NIL) ), return(T). */
  if (argcount > 0) {
    var const gcv_object_t* arg_i_ptr = args_pointer;
    dotimespC(argcount,argcount, {
      var object arg_i = NEXT(arg_i_ptr);
      if (R_R_comp(arg_i,Next(arg_i_ptr))<=0) goto no;
    });
  }
 yes:
  value1 = T; goto ok;
 no:
  value1 = NIL; goto ok;
 ok:
  mv_count=1; set_args_end_pointer(args_pointer);
}

LISPFUN(klgleich,seclass_foldable,1,0,rest,nokey,0,NIL)
{ /* (<= real {real}), CLTL p. 196 */
  var gcv_object_t* args_pointer = rest_args_pointer STACKop 1;
  test_real_args(argcount,args_pointer); /* all arguments real numbers? */
  /* method:
     n+1 arguments Arg[0..n].
     for i:=0 to n-1 do ( if Arg[i]>Arg[i+1] then return(NIL) ), return(T). */
  if (argcount > 0) {
    var const gcv_object_t* arg_i_ptr = args_pointer;
    dotimespC(argcount,argcount, {
      var object arg_i = NEXT(arg_i_ptr);
      if (R_R_comp(arg_i,Next(arg_i_ptr))>0) goto no;
    });
  }
 yes:
  value1 = T; goto ok;
 no:
  value1 = NIL; goto ok;
 ok:
  mv_count=1; set_args_end_pointer(args_pointer);
}

LISPFUN(grgleich,seclass_foldable,1,0,rest,nokey,0,NIL)
{ /* (>= real {real}), CLTL p. 196 */
  var gcv_object_t* args_pointer = rest_args_pointer STACKop 1;
  test_real_args(argcount,args_pointer); /* all arguments real numbers? */
  /* method:
     n+1 arguments Arg[0..n].
     for i:=0 to n-1 do ( if Arg[i]<Arg[i+1] then return(NIL) ), return(T). */
  if (argcount > 0) {
    var const gcv_object_t* arg_i_ptr = args_pointer;
    dotimespC(argcount,argcount, {
      var object arg_i = NEXT(arg_i_ptr);
      if (R_R_comp(arg_i,Next(arg_i_ptr))<0) goto no;
    });
  }
 yes:
  value1 = T; goto ok;
 no:
  value1 = NIL; goto ok;
 ok:
  mv_count=1; set_args_end_pointer(args_pointer);
}

LISPFUN(max,seclass_foldable,1,0,rest,nokey,0,NIL)
{ /* (MAX real {real}), CLTL p. 198
     method:
     (max x1 x2 x3 ... xn) = (max ...(max (max x1 x2) x3)... xn) */
  var gcv_object_t* args_pointer = rest_args_pointer STACKop 1;
  test_real_args(argcount,args_pointer); /* all arguments real numbers? */
  /* method:
     n+1 arguments Arg[0..n].
     x:=Arg[0], for i:=1 to n do ( x := max(x,Arg[i]) ), return(x). */
  var gcv_object_t* arg_i_ptr = args_pointer;
  var object x = NEXT(arg_i_ptr); /* maximum so far */
  dotimesC(argcount,argcount, { x = R_R_max_R(x,NEXT(arg_i_ptr)); } );
  VALUES1(x); set_args_end_pointer(args_pointer);
}

LISPFUN(min,seclass_foldable,1,0,rest,nokey,0,NIL)
{ /* (MIN real {real}), CLTL p. 198
     method:
     (min x1 x2 x3 ... xn) = (min ...(min (min x1 x2) x3)... xn) */
  var gcv_object_t* args_pointer = rest_args_pointer STACKop 1;
  test_real_args(argcount,args_pointer); /* all arguments real numbers? */
  /* method:
     n+1 arguments Arg[0..n].
     x:=Arg[0], for i:=1 to n do ( x := min(x,Arg[i]) ), return(x). */
  var gcv_object_t* arg_i_ptr = args_pointer;
  var object x = NEXT(arg_i_ptr); /* minimum so far */
  dotimesC(argcount,argcount, { x = R_R_min_R(x,NEXT(arg_i_ptr)); } );
  VALUES1(x); set_args_end_pointer(args_pointer);
}

LISPFUN(plus,seclass_foldable,0,0,rest,nokey,0,NIL)
{ /* (+ {number}), CLTL p. 199
     method:
     (+) = 0
     (+ x1 x2 x3 ... xn) = (+ ...(+ (+ x1 x2) x3)... xn) */
  if (argcount==0) {
    VALUES1(Fixnum_0); return;
  }
  argcount--;
  test_number_args(argcount,rest_args_pointer); /* all arguments numbers? */
  /* method:
     n+1 arguments Arg[0..n].
     x:=Arg[0], for i:=1 to n do ( x := x+Arg[i] ), return(x). */
  var gcv_object_t* arg_i_ptr = rest_args_pointer;
  var object x = NEXT(arg_i_ptr); /* sum so far */
  dotimesC(argcount,argcount, { x = N_N_plus_N(x,NEXT(arg_i_ptr)); } );
  VALUES1(x); set_args_end_pointer(rest_args_pointer);
}

LISPFUN(minus,seclass_foldable,1,0,rest,nokey,0,NIL)
{ /* (- number {number}), CLTL p. 199
     method:
     (- x) extra.
     (- x1 x2 x3 ... xn) = (- ...(- (- x1 x2) x3)... xn) */
  var gcv_object_t* args_pointer = rest_args_pointer STACKop 1;
  test_number_args(argcount,args_pointer); /* all arguments numbers? */
  if (argcount==0) { /* unary minus */
    value1 = N_minus_N(Next(args_pointer));
  } else {
    /* method:
       n+1 arguments Arg[0..n].
       x:=Arg[0], for i:=1 to n do ( x := x-Arg[i] ), return(x). */
    var gcv_object_t* arg_i_ptr = args_pointer;
    var object x = NEXT(arg_i_ptr); /* difference so far */
    dotimespC(argcount,argcount, { x = N_N_minus_N(x,NEXT(arg_i_ptr)); } );
    value1 = x;
  }
  mv_count=1; set_args_end_pointer(args_pointer);
}

LISPFUN(mal,seclass_foldable,0,0,rest,nokey,0,NIL)
{ /* (* {number}), CLTL p. 199
     method:
     (*) = 1
     (* x1 x2 x3 ... xn) = (* ...(* (* x1 x2) x3)... xn) */
  if (argcount==0) {
    VALUES1(Fixnum_1); return;
  }
  argcount--;
  test_number_args(argcount,rest_args_pointer); /* all arguments numbers? */
  /* method:
     n+1 arguments Arg[0..n].
     x:=Arg[0], for i:=1 to n do ( x := x*Arg[i] ), return(x). */
  var gcv_object_t* arg_i_ptr = rest_args_pointer;
  var object x = NEXT(arg_i_ptr); /* product so far */
  dotimesC(argcount,argcount, {
    var object arg = NEXT(arg_i_ptr);
    x = (eq(x,arg) ? N_square_N(x) : N_N_mal_N(x,arg));
  });
  VALUES1(x); set_args_end_pointer(rest_args_pointer);
}

LISPFUN(durch,seclass_foldable,1,0,rest,nokey,0,NIL)
{ /* (/ number {number}), CLTL p. 200
     method:
     (/ x) extra.
     (/ x1 x2 x3 ... xn) = (/ ...(/ (/ x1 x2) x3)... xn) */
  var gcv_object_t* args_pointer = rest_args_pointer STACKop 1;
  test_number_args(argcount,args_pointer); /* all arguments numbers? */
  if (argcount==0) {
    /* unary division */
    value1 = N_durch_N(Next(args_pointer));
  } else {
    /* method:
       n+1 arguments Arg[0..n].
       x:=Arg[0], for i:=1 to n do ( x := x/Arg[i] ), return(x). */
    var gcv_object_t* arg_i_ptr = args_pointer;
    var object x = NEXT(arg_i_ptr); /* difference so far */
    dotimespC(argcount,argcount, { x = N_N_durch_N(x,NEXT(arg_i_ptr)); } );
    value1 = x;
  }
  mv_count=1; set_args_end_pointer(args_pointer);
}

LISPFUNNF(einsplus,1)
{ /* (1+ number), CLTL p. 200 */
  var object x = check_number(popSTACK());
  VALUES1(N_1_plus_N(x));
}

LISPFUNNF(einsminus,1)
{ /* (1- number), CLTL p. 200 */
  var object x = check_number(popSTACK());
  VALUES1(N_minus1_plus_N(x));
}

LISPFUNNF(conjugate,1)
{ /* (CONJUGATE number), CLTL p. 201 */
  var object x = check_number(popSTACK());
  VALUES1(N_conjugate_N(x));
}

LISPFUN(gcd,seclass_foldable,0,0,rest,nokey,0,NIL)
{ /* (GCD {integer}), CLTL p. 202
     method:
     (gcd) = 0
     (gcd x) = (abs x)
     (gcd x1 x2 x3 ... xn) = (gcd ...(gcd (gcd x1 x2) x3)... xn) */
  if (argcount==0) {
    VALUES1(Fixnum_0); return;
  }
  argcount--;
  test_integer_args(argcount,rest_args_pointer); /* all arguments integers? */
  if (argcount==0) {
    VALUES1(I_abs_I(Next(rest_args_pointer)));
  } else {
    /* method:
       n+1 arguments Arg[0..n].
       x:=Arg[0], for i:=1 to n do ( x := gcd(x,Arg[i]) ), return(x). */
    var gcv_object_t* arg_i_ptr = rest_args_pointer;
    var object x = NEXT(arg_i_ptr); /* ggT so far */
    dotimespC(argcount,argcount, { x = I_I_gcd_I(x,NEXT(arg_i_ptr)); } );
    VALUES1(x);
  }
  set_args_end_pointer(rest_args_pointer);
}

LISPFUN(xgcd,seclass_foldable,0,0,rest,nokey,0,NIL)
{ /* (XGCD {integer})
 (XGCD x1 ... xn) returns n+1 Werte: g = (gcd x1 ... xn), an integer >=0,
 and n integers u1,...,un with g = u1*x1+...+un*xn.
 method:
 (xgcd) = 0
 (xgcd x) = (abs x), (signum x)
 (xgcd x1 x2 x3 ... xn) with n>=2:
   (g,u[1],u[2]) := (xgcd x1 x2),
   for i=3,...,n:
     (g',u,v) := (xgcd g xi),
     (g,u[1],...,u[i]) := (g',u*u[1],...,u*u[i-1],v). */
  if (argcount==0) {
    VALUES1(Fixnum_0); return;
  }
  argcount--;
  test_integer_args(argcount,rest_args_pointer); /* all arguments integers? */
  if (argcount==0) {
    var object arg = Next(rest_args_pointer);
    if (R_minusp(arg)) {
      VALUES2(arg, Fixnum_minus1);
    } else {
      VALUES2(I_minus_I(arg), Fixnum_1);
    }
  } else {
    /* method:
       n+1 arguments Arg[0..n].
       (g,u,v):=xgcd(Arg[0],Arg[1]), Arg[0]:=u, Arg[1]:=v,
       for i:=2 to n do
         ( (g,u,v):=xgcd(g,Arg[i]), Arg[i]:=v,
           for j:=i-1 downto 0 do Arg[j]:=u*Arg[j], ),
       return values(g,Arg[0],...,Arg[n]). */
    var gcv_object_t* arg_i_ptr = rest_args_pointer;
    var object g; /* ggT so far */
    {
      var object arg_0 = NEXT(arg_i_ptr);
      var object arg_1 = Next(arg_i_ptr);
      I_I_xgcd_I_I_I(arg_0,arg_1);
      Before(arg_i_ptr) = STACK_2;
    }
    loop {
      NEXT(arg_i_ptr) = STACK_1;
      g = STACK_0; skipSTACK(3);
      if (arg_i_ptr == args_end_pointer)
        break;
      I_I_xgcd_I_I_I(g,Next(arg_i_ptr));
      var gcv_object_t* arg_j_ptr = arg_i_ptr;
      do {
        var object arg_j = Before(arg_j_ptr);
        BEFORE(arg_j_ptr) = I_I_mal_I(STACK_2,arg_j);
      } while (arg_j_ptr != rest_args_pointer);
    }
    value1 = g; /* g as 1. value */
    { /* factors as further values: */
      var object* mvp = &value2;
      var gcv_object_t* arg_i_ptr = rest_args_pointer;
      if (argcount >= mv_limit-2)
        fehler_mv_zuviel(S(xgcd));
      mv_count = argcount+2;
      dotimespC(argcount,argcount+1, { *mvp++ = NEXT(arg_i_ptr); } );
    }
  }
  set_args_end_pointer(rest_args_pointer);
}

LISPFUN(lcm,seclass_foldable,0,0,rest,nokey,0,NIL)
{ /* (LCM {integer})
 method:
 (lcm) = 1 (neutral element of lcm-operation)
 (lcm x) = (abs x)
 (lcm x1 x2 x3 ... xn) = (lcm ...(lcm (lcm x1 x2) x3)... xn) */
  if (argcount==0) {
    VALUES1(Fixnum_1); return;
  }
  argcount--;
  test_integer_args(argcount,rest_args_pointer); /* all arguments integers? */
  if (argcount==0) {
    VALUES1(I_abs_I(Next(rest_args_pointer)));
  } else {
    /* method:
       n+1 arguments Arg[0..n].
       x:=Arg[0], for i:=1 to n do ( x := lcm(x,Arg[i]) ), return(x). */
    var gcv_object_t* arg_i_ptr = rest_args_pointer;
    var object x = NEXT(arg_i_ptr); /* kgV so far */
    dotimespC(argcount,argcount, { x = I_I_lcm_I(x,NEXT(arg_i_ptr)); } );
    VALUES1(x);
  }
  set_args_end_pointer(rest_args_pointer);
}

LISPFUNNR(exp,1)
{ /* (EXP number), CLTL p. 203 */
  STACK_0 = check_number(STACK_0);
  if (complexp(STACK_0))
    pushSTACK(R_R_contagion_R(TheComplex(STACK_0)->c_real,
                              TheComplex(STACK_0)->c_imag));
  else
    pushSTACK(STACK_0);
  VALUES1(N_exp_N(STACK_1,true,&STACK_0));
  skipSTACK(2);
}

LISPFUNNR(expt,2)
{ /* (EXPT number number), CLTL p. 203 */
  STACK_0 = check_number(STACK_0); STACK_1 = check_number(STACK_1);
  VALUES1(N_N_expt_N(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUN(log,seclass_read,1,1,norest,nokey,0,NIL)
{ /* (LOG number [base-number]), CLTL p. 204 */
  STACK_1 = check_number(STACK_1);
  var object base = STACK_0;
  if (!boundp(base)) { /* LOG with one argument */
    var object arg = STACK_1;
    /* Determine the floating-point format to use for the result. */
    if (complexp(arg)) {
      STACK_0 = R_R_contagion_R(TheComplex(arg)->c_real,TheComplex(arg)->c_imag);
      arg = STACK_1;
    } else
      STACK_0 = arg;
    VALUES1(N_log_N(arg,&STACK_0));
  } else { /* LOG with two arguments */
    base = check_number(base);
    VALUES1(N_N_log_N(STACK_1,base));
  }
  skipSTACK(2);
}

LISPFUNNR(sqrt,1)
{ /* (SQRT number), CLTL p. 205 */
  var object x = check_number(popSTACK());
  VALUES1(N_sqrt_N(x));
}

LISPFUNNF(isqrt,1)
{ /* (ISQRT integer), CLTL p. 205 */
  var object x = check_pos_integer(popSTACK());
  VALUES1((I_isqrt_I(x), popSTACK()));
}

LISPFUNNR(abs,1)
{ /* (ABS number), CLTL p. 205 */
  var object x = check_number(popSTACK());
  VALUES1(N_abs_R(x));
}

LISPFUNNR(phase,1)
{ /* (PHASE number), CLTL p. 206 */
  var object x = check_number(popSTACK());
  VALUES1(N_phase_R(x,false));
}

LISPFUNNR(signum,1)
{ /* (SIGNUM number), CLTL p. 206 */
  var object x = check_number(popSTACK());
  VALUES1(N_signum_N(x));
}

LISPFUNNR(sin,1)
{ /* (SIN number), CLTL p. 207 */
  var object x = check_number(popSTACK());
  VALUES1(N_sin_N(x));
}

LISPFUNNR(cos,1)
{ /* (COS number), CLTL p. 207 */
  var object x = check_number(popSTACK());
  VALUES1(N_cos_N(x));
}

LISPFUNNR(tan,1)
{ /* (TAN number), CLTL p. 207 */
  var object x = check_number(popSTACK());
  VALUES1(N_tan_N(x));
}

LISPFUNNR(cis,1)
{ /* (CIS number), CLTL p. 207 */
  var object x = check_number(popSTACK());
  VALUES1(N_cis_N(x));
}

LISPFUNNR(asin,1)
{ /* (ASIN number), CLTL p. 207 */
  var object x = check_number(popSTACK());
  VALUES1(N_asin_N(x));
}

LISPFUNNR(acos,1)
{ /* (ACOS number), CLTL p. 207 */
  var object x = check_number(popSTACK());
  VALUES1(N_acos_N(x));
}

LISPFUN(atan,seclass_read,1,1,norest,nokey,0,NIL)
{ /* (ATAN number [real]), CLTL p. 207 */
  if (!boundp(STACK_0)) { /* 1 argument */
    VALUES1(N_atan_N(check_number(STACK_1)));
  } else { /* 2 arguments */
    STACK_0 = check_real(STACK_0);
    STACK_1 = check_real(STACK_1);
    VALUES1(R_R_atan_R(STACK_0,STACK_1)); /* atan(X=arg2,Y=arg1) */
  }
  skipSTACK(2);
}

LISPFUNNR(sinh,1)
{ /* (SINH number), CLTL p. 209 */
  var object x = check_number(popSTACK());
  VALUES1(N_sinh_N(x));
}

LISPFUNNR(cosh,1)
{ /* (COSH number), CLTL p. 209 */
  var object x = check_number(popSTACK());
  VALUES1(N_cosh_N(x));
}

LISPFUNNR(tanh,1)
{ /* (TANH number), CLTL p. 209 */
  var object x = check_number(popSTACK());
  VALUES1(N_tanh_N(x));
}

LISPFUNNR(asinh,1)
{ /* (ASINH number), CLTL p. 209 */
  var object x = check_number(popSTACK());
  VALUES1(N_asinh_N(x));
}

LISPFUNNR(acosh,1)
{ /* (ACOSH number), CLTL p. 209 */
  var object x = check_number(popSTACK());
  VALUES1(N_acosh_N(x));
}

LISPFUNNR(atanh,1)
{ /* (ATANH number), CLTL p. 209 */
  var object x = check_number(popSTACK());
  VALUES1(N_atanh_N(x));
}

LISPFUN(float,seclass_read,1,1,norest,nokey,0,NIL)
{ /* (FLOAT number [float]), CLTL p. 214 */
  STACK_1 = check_real(STACK_1);
  if (!boundp(STACK_0)) { /* 1 argument */
    VALUES1(R_float_F(STACK_1));
  } else { /* 2 arguments */
    STACK_0 = check_float(STACK_0);
    VALUES1(R_F_float_F(STACK_1,STACK_0));
  }
  skipSTACK(2);
}

/* UP: Converts an object into a float of given type.
 coerce_float(obj,type)
 > obj: object
 > type: one of the symbols
         FLOAT, SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT
 < result: (coerce obj type)
 can trigger GC */
global maygc object coerce_float (object obj, object type) {
  pushSTACK(type); obj = check_real(obj); type = popSTACK();
  if (eq(type,S(short_float))) /* SHORT-FLOAT */
    return R_to_SF(obj);
  else if (eq(type,S(single_float))) /* SINGLE-FLOAT */
    return R_to_FF(obj);
  else if (eq(type,S(double_float))) /* DOUBLE-FLOAT */
    return R_to_DF(obj);
  else if (eq(type,S(long_float))) /* LONG-FLOAT */
    return R_to_LF(obj,I_to_UL(O(LF_digits))); /* default precision */
  else /* FLOAT */
    return R_float_F(obj);
}

/* Converts a function's argument to a C 'double'.
 to_double(obj)
 > obj: an object, usually a real number
 < result: its value as a C 'double'
 can trigger GC */
global maygc double to_double (object x) {
  double ret;
  x = check_real(x);
  DF_to_c_double(R_to_DF(x), (dfloatjanus*)&ret);
  return ret;
}

/* Converts a function's argument to a C 'int'.
 to_int(obj)
 > obj: an object, usually an integer
 < result: its value as a C 'int'
 can trigger GC */
global maygc int to_int (object x) {
  x = check_integer(x);
  return I_to_L(x);
}

LISPFUNNF(rational,1)
{ /* (RATIONAL real), CLTL p. 214 */
  var object x = check_real(popSTACK());
  VALUES1(R_rational_RA(x));
}

LISPFUNNF(rationalize,1)
{ /* (RATIONALIZE real), CLTL p. 214 */
  var object x = check_real(popSTACK());
  VALUES1(R_rationalize_RA(x));
}

LISPFUNNF(numerator,1)
{ /* (NUMERATOR rational), CLTL p. 215 */
  var object x = check_rational(popSTACK());
  VALUES1(RA_integerp(x) ? x : (object)TheRatio(x)->rt_num);
}

LISPFUNNF(denominator,1)
{ /* (DENOMINATOR rational), CLTL p. 215 */
  var object x = check_rational(popSTACK());
  VALUES1(RA_integerp(x) ? Fixnum_1 : (object)TheRatio(x)->rt_den);
}

LISPFUN(floor,seclass_foldable,1,1,norest,nokey,0,NIL)
{ /* (FLOOR real [real]), CLTL p. 215 */
  STACK_1 = check_real(STACK_1); /* x */
  if (!boundp(STACK_0) || eq(STACK_0,Fixnum_1)) {
    /* 1 argument or 2nd argument =1 */
    R_floor_I_R(STACK_1);
  } else { /* 2 arguments */
    STACK_0 = check_real(STACK_0);
    R_R_floor_I_R(STACK_1,STACK_0);
  }
  /* stack layout: x, y, q, r. */
  VALUES2(STACK_1, STACK_0); skipSTACK(4);
}

LISPFUN(ceiling,seclass_foldable,1,1,norest,nokey,0,NIL)
{ /* (CEILING real [real]), CLTL p. 215 */
  STACK_1 = check_real(STACK_1); /* x */
  if (!boundp(STACK_0) || eq(STACK_0,Fixnum_1)) {
    /* 1 argument or 2nd argument =1 */
    R_ceiling_I_R(STACK_1);
  } else { /* 2 arguments */
    STACK_0 = check_real(STACK_0);
    R_R_ceiling_I_R(STACK_1,STACK_0);
  }
  /* stack layout: x, y, q, r. */
  VALUES2(STACK_1, STACK_0); skipSTACK(4);
}

LISPFUN(truncate,seclass_foldable,1,1,norest,nokey,0,NIL)
{ /* (TRUNCATE real [real]), CLTL p. 215 */
  STACK_1 = check_real(STACK_1); /* x */
  if (!boundp(STACK_0) || eq(STACK_0,Fixnum_1)) {
    /* 1 argument or 2nd argument =1 */
    R_truncate_I_R(STACK_1);
  } else { /* 2 arguments */
    STACK_0 = check_real(STACK_0);
    R_R_truncate_I_R(STACK_1,STACK_0);
  }
  /* stack layout: x, y, q, r. */
  VALUES2(STACK_1, STACK_0); skipSTACK(4);
}

LISPFUN(round,seclass_foldable,1,1,norest,nokey,0,NIL)
{ /* (ROUND real [real]), CLTL p. 215 */
  STACK_1 = check_real(STACK_1); /* x */
  if (!boundp(STACK_0) || eq(STACK_0,Fixnum_1)) {
    /* 1 argument or 2nd argument =1 */
    R_round_I_R(STACK_1);
  } else { /* 2 arguments */
    STACK_0 = check_real(STACK_0);
    R_R_round_I_R(STACK_1,STACK_0);
  }
  /* stack layout: x, y, q, r. */
  VALUES2(STACK_1, STACK_0); skipSTACK(4);
}

LISPFUNNF(mod,2)
{ /* (MOD real real), CLTL p. 217 */
  STACK_0 = check_real(STACK_0); STACK_1 = check_real(STACK_1);
  VALUES1(R_R_mod_R(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUNNF(rem,2)
{ /* (REM real real), CLTL p. 217 */
  STACK_0 = check_real(STACK_0); STACK_1 = check_real(STACK_1);
  VALUES1(R_R_rem_R(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUN(ffloor,seclass_read,1,1,norest,nokey,0,NIL)
{ /* (FFLOOR real [real]), CLTL p. 217 */
  STACK_1 = check_real(STACK_1);
  if (!boundp(STACK_0) || eq(STACK_0,Fixnum_1)) {
    /* 1 argument or 2nd argument =1 */
    R_ffloor_F_R(STACK_1);
  } else { /* 2 arguments */
    check_real(STACK_0);
    R_R_ffloor_F_R(STACK_1,STACK_0);
  }
  /* stack layout: x, y, q, r. */
  VALUES2(STACK_1, STACK_0); skipSTACK(4);
}

LISPFUN(fceiling,seclass_read,1,1,norest,nokey,0,NIL)
{ /* (FCEILING real [real]), CLTL p. 217 */
  STACK_1 = check_real(STACK_1);
  if (!boundp(STACK_0) || eq(STACK_0,Fixnum_1)) {
    /* 1 argument or 2nd argument =1 */
    R_fceiling_F_R(STACK_1);
  } else { /* 2 arguments */
    check_real(STACK_0);
    R_R_fceiling_F_R(STACK_1,STACK_0);
  }
  /* stack layout: x, y, q, r. */
  VALUES2(STACK_1, STACK_0); skipSTACK(4);
}

LISPFUN(ftruncate,seclass_read,1,1,norest,nokey,0,NIL)
{ /* (FTRUNCATE real [real]), CLTL p. 217 */
  STACK_1 = check_real(STACK_1);
  if (!boundp(STACK_0) || eq(STACK_0,Fixnum_1)) {
    /* 1 argument or 2nd argument =1 */
    R_ftruncate_F_R(STACK_1);
  } else { /* 2 arguments */
    check_real(STACK_0);
    R_R_ftruncate_F_R(STACK_1,STACK_0);
  }
  /* stack layout: x, y, q, r. */
  VALUES2(STACK_1, STACK_0); skipSTACK(4);
}

LISPFUN(fround,seclass_read,1,1,norest,nokey,0,NIL)
{ /* (FROUND real [real]), CLTL p. 217 */
  STACK_1 = check_real(STACK_1);
  if (!boundp(STACK_0) || eq(STACK_0,Fixnum_1)) {
    /* 1 argument or 2nd argument =1 */
    R_fround_F_R(STACK_1);
  } else { /* 2 arguments */
    check_real(STACK_0);
    R_R_fround_F_R(STACK_1,STACK_0);
  }
  /* stack layout: x, y, q, r. */
  VALUES2(STACK_1, STACK_0); skipSTACK(4);
}

LISPFUNNF(decode_float,1)
{ /* (DECODE-FLOAT float), CLTL p. 218 */
  var object f = check_float(popSTACK());
  F_decode_float_F_I_F(f);
  VALUES3(STACK_2, STACK_1, STACK_0); skipSTACK(3);
}

LISPFUNNF(scale_float,2)
{ /* (SCALE-FLOAT float integer), CLTL p. 218 */
  STACK_1 = check_float(STACK_1); STACK_0 = check_integer(STACK_0);
  VALUES1(F_I_scale_float_F(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUNNF(float_radix,1)
{ /* (FLOAT-RADIX float), CLTL p. 218 */
  var object f = check_float(popSTACK());
  VALUES1(F_float_radix_I(f));
}

LISPFUN(float_sign,seclass_foldable,1,1,norest,nokey,0,NIL)
{ /* (FLOAT-SIGN float [float]), CLTL p. 218 */
  STACK_1 = check_float(STACK_1);
  if (!boundp(STACK_0)) { /* 1 argument */
    VALUES1(F_float_sign_F(STACK_1));
  } else { /* 2 arguments */
    STACK_0 = check_float(STACK_0);
    VALUES1(F_F_float_sign_F(STACK_1,STACK_0));
  }
  skipSTACK(2);
}

LISPFUN(float_digits,seclass_foldable,1,1,norest,nokey,0,NIL)
{ /* (FLOAT-DIGITS number [digits]), CLTL p. 218 */
  if (!boundp(STACK_0)) { /* 1 argument: (FLOAT-DIGITS float) */
    STACK_1 = check_float(STACK_1);
    VALUES1(F_float_digits_I(STACK_1));
  } else { /* 2 arguments: (FLOAT-DIGITS number digits) */
    if (!posfixnump(STACK_0)) /* not a fixnum!?? */
      fehler_digits(STACK_0);
    var uintV d = posfixnum_to_V(STACK_0); /* = I_to_UL(STACK_0); ?? */
    if (d==0) /* should be >0 */
      fehler_digits(STACK_0);
    STACK_1 = check_real(STACK_1);
    /* convert STACK_1 into a float with at least d bits: */
    if (d > DF_mant_len+1) { /* -> long-float */
      d = ceiling(d,intDsize);
     #if (intWCsize<intVsize)
      if (d >= vbit(intWCsize))
        fehler_LF_toolong();
     #endif
      VALUES1(R_to_LF(STACK_1,d));
    } else if (d > FF_mant_len+1) /* a double-float is sufficient */
      VALUES1(R_to_DF(STACK_1));  /* -> double-float */
    else if (d > SF_mant_len+1) /* a single-float is sufficient */
      VALUES1(R_to_FF(STACK_1)); /* -> single-float */
    else /* a short-float is sufficient */
      VALUES1(R_to_SF(STACK_1));
  }
  skipSTACK(2);
}

LISPFUNNF(float_precision,1)
{ /* (FLOAT-PRECISION float), CLTL p. 218 */
  var object f = check_float(popSTACK());
  VALUES1(F_float_precision_I(f));
}

LISPFUNNF(integer_decode_float,1)
{ /* (INTEGER-DECODE-FLOAT float), CLTL p. 218 */
  var object f = check_float(popSTACK());
  F_integer_decode_float_I_I_I(f);
  VALUES3(STACK_2, STACK_1, STACK_0); skipSTACK(3);
}

LISPFUN(complex,seclass_foldable,1,1,norest,nokey,0,NIL)
{ /* (COMPLEX real [real]), CLTL p. 220
 deviation from CLTL:
 for real x, (COMPLEX x) = x.
 reason:  (COMPLEX 1) = 1 shows, that (COMPLEX x) is to be interpreted as
 (COMPLEX x 0) . We can have complex numbers with a real part
 and imaginary part of different types (cf. CLTL, page 19),
 and then: (COMPLEX x 0) = x. */
  STACK_1 = check_real(STACK_1);
  if (!boundp(STACK_0)) { /* 1 argument */
    VALUES1(STACK_1);
  } else { /* 2 arguments */
    STACK_0 = check_real(STACK_0);
    VALUES1(R_R_complex_N(STACK_1,STACK_0));
  }
  skipSTACK(2);
}

LISPFUNNF(realpart,1)
{ /* (REALPART number), CLTL p. 220 */
  var object x = check_number(popSTACK());
  VALUES1(N_realpart_R(x));
}

LISPFUNNF(imagpart,1)
{ /* (IMAGPART number), CLTL p. 220 */
  var object x = check_number(popSTACK());
  VALUES1(N_imagpart_R(x));
}

LISPFUN(logior,seclass_foldable,0,0,rest,nokey,0,NIL)
{ /* (LOGIOR {integer}), CLTL p. 221
 method:
 (logior) = 0
 (logior x1 x2 x3 ... xn) = (logior ...(logior (logior x1 x2) x3)... xn) */
  if (argcount==0) {
    VALUES1(Fixnum_0); return;
  }
  argcount--;
  test_integer_args(argcount,rest_args_pointer); /* all arguments integers? */
  /* method:
     n+1 arguments Arg[0..n].
     x:=Arg[0], for i:=1 to n do ( x := logior(x,Arg[i]) ), return(x). */
  var gcv_object_t* arg_i_ptr = rest_args_pointer;
  var object x = NEXT(arg_i_ptr); /* "or" so far */
  dotimesC(argcount,argcount, { x = I_I_logior_I(x,NEXT(arg_i_ptr)); } );
  VALUES1(x); set_args_end_pointer(rest_args_pointer);
}

LISPFUN(logxor,seclass_foldable,0,0,rest,nokey,0,NIL)
{ /* (LOGXOR {integer}), CLTL p. 221
 method:
 (logxor) = 0
 (logxor x1 x2 x3 ... xn) = (logxor ...(logxor (logxor x1 x2) x3)... xn) */
  if (argcount==0) {
    VALUES1(Fixnum_0); return;
  }
  argcount--;
  test_integer_args(argcount,rest_args_pointer); /* all arguments integers? */
  /* method:
     n+1 arguments Arg[0..n].
     x:=Arg[0], for i:=1 to n do ( x := logxor(x,Arg[i]) ), return(x). */
  var gcv_object_t* arg_i_ptr = rest_args_pointer;
  var object x = NEXT(arg_i_ptr); /*  "Xor" so far */
  dotimesC(argcount,argcount, { x = I_I_logxor_I(x,NEXT(arg_i_ptr)); } );
  VALUES1(x); set_args_end_pointer(rest_args_pointer);
}

LISPFUN(logand,seclass_foldable,0,0,rest,nokey,0,NIL)
{ /* (LOGAND {integer}), CLTL p. 221
  method:
  (logand) = -1
  (logand x1 x2 x3 ... xn) = (logand ...(logand (logand x1 x2) x3)... xn) */
  if (argcount==0) {
    VALUES1(Fixnum_minus1); return;
  }
  argcount--;
  test_integer_args(argcount,rest_args_pointer); /* all arguments integers? */
  /* method:
     n+1 arguments Arg[0..n].
     x:=Arg[0], for i:=1 to n do ( x := logand(x,Arg[i]) ), return(x). */
  var gcv_object_t* arg_i_ptr = rest_args_pointer;
  var object x = NEXT(arg_i_ptr); /* "And" so far */
  dotimesC(argcount,argcount, { x = I_I_logand_I(x,NEXT(arg_i_ptr)); } );
  VALUES1(x); set_args_end_pointer(rest_args_pointer);
}

LISPFUN(logeqv,seclass_foldable,0,0,rest,nokey,0,NIL)
{ /* (LOGEQV {integer}), CLTL p. 221
  method:
  (logeqv) = -1
  (logeqv x1 x2 x3 ... xn) = (logeqv ...(logeqv (logeqv x1 x2) x3)... xn) */
  if (argcount==0) {
    VALUES1(Fixnum_minus1); return;
  }
  argcount--;
  test_integer_args(argcount,rest_args_pointer); /* all arguments integers? */
  /* method:
     n+1 arguments Arg[0..n].
     x:=Arg[0], for i:=1 to n do ( x := logeqv(x,Arg[i]) ), return(x). */
  var gcv_object_t* arg_i_ptr = rest_args_pointer;
  var object x = NEXT(arg_i_ptr); /* intermediate-EQV so far */
  dotimesC(argcount,argcount, { x = I_I_logeqv_I(x,NEXT(arg_i_ptr)); } );
  VALUES1(x); set_args_end_pointer(rest_args_pointer);
}

LISPFUNNF(lognand,2)
{ /* (LOGNAND integer integer), CLTL p. 221 */
  STACK_0 = check_integer(STACK_0); STACK_1 = check_integer(STACK_1);
  VALUES1(I_I_lognand_I(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUNNF(lognor,2)
{ /* (LOGNOR integer integer), CLTL p. 221 */
  STACK_0 = check_integer(STACK_0); STACK_1 = check_integer(STACK_1);
  VALUES1(I_I_lognor_I(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUNNF(logandc1,2)
{ /* (LOGANDC1 integer integer), CLTL p. 221 */
  STACK_0 = check_integer(STACK_0); STACK_1 = check_integer(STACK_1);
  VALUES1(I_I_logandc1_I(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUNNF(logandc2,2)
{ /* (LOGANDC2 integer integer), CLTL p. 221 */
  STACK_0 = check_integer(STACK_0); STACK_1 = check_integer(STACK_1);
  VALUES1(I_I_logandc2_I(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUNNF(logorc1,2)
{ /* (LOGORC1 integer integer), CLTL p. 221 */
  STACK_0 = check_integer(STACK_0); STACK_1 = check_integer(STACK_1);
  VALUES1(I_I_logorc1_I(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUNNF(logorc2,2)
{ /* (LOGORC2 integer integer), CLTL p. 221 */
  STACK_0 = check_integer(STACK_0); STACK_1 = check_integer(STACK_1);
  VALUES1(I_I_logorc2_I(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUNNF(boole,3)
{ /* (BOOLE op integer integer), CLTL p. 222 */
  var object op = STACK_2; /* operator, not a typetest */
  STACK_0 = check_integer(STACK_0); STACK_1 = check_integer(STACK_1);
  VALUES1(OP_I_I_boole_I(STACK_2,STACK_1,STACK_0)); skipSTACK(3);
}

LISPFUNNF(lognot,1)
{ /* (LOGNOT integer), CLTL p. 223 */
  var object x = check_integer(popSTACK());
  VALUES1(I_lognot_I(x));
}

LISPFUNNF(logtest,2)
{ /* (LOGTEST integer integer), CLTL p. 223 */
  STACK_0 = check_integer(STACK_0); STACK_1 = check_integer(STACK_1);
  VALUES_IF(I_I_logtest(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUNNF(logbitp,2)
{ /* (LOGBITP integer integer), CLTL p. 224 */
  STACK_0 = check_integer(STACK_0); STACK_1 = check_integer(STACK_1);
  VALUES_IF(I_I_logbitp(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUNNF(ash,2)
{ /* (ASH integer integer), CLTL p. 224 */
  STACK_0 = check_integer(STACK_0); STACK_1 = check_integer(STACK_1);
  VALUES1(I_I_ash_I(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUNNF(logcount,1)
{ /* (LOGCOUNT integer), CLTL p. 224 */
  var object x = check_integer(popSTACK());
  VALUES1(I_logcount_I(x));
}

LISPFUNNF(integer_length,1)
{ /* (INTEGER-LENGTH integer), CLTL p. 224 */
  var object x = check_integer(popSTACK());
  VALUES1(I_integer_length_I(x));
}

LISPFUNNR(byte,2)
{ /* (BYTE size position), CLTL p. 225 */
  var object s = STACK_1;
  var object p = STACK_0;
  skipSTACK(2);
  VALUES1(I_I_Byte(s,p)); /* type checks there. Why fixnums?? */
}

LISPFUNNR(bytesize,1)
{ /* (BYTE-SIZE bytespec), CLTL p. 226 */
  var object b = popSTACK();
  VALUES1(Byte_size(b)); /* type check there */
}

LISPFUNNR(byteposition,1)
{ /* (BYTE-POSITION bytespec), CLTL p. 226 */
  var object b = popSTACK();
  VALUES1(Byte_position(b)); /* type check there */
}

LISPFUNNF(ldb,2)
{ /* (LDB bytespec integer), CLTL p. 226 */
  var object x = check_integer(STACK_0);
  var object b = STACK_1; /* Type check will take place later */
  skipSTACK(2);
  VALUES1(I_Byte_ldb_I(x,b));
}

LISPFUNNF(ldb_test,2)
{ /* (LDB-TEST bytespec integer), CLTL p. 226 */
  var object x = check_integer(STACK_0);
  var object b = STACK_1; /* Type check will take place later */
  skipSTACK(2);
  VALUES_IF(I_Byte_ldb_test(x,b));
}

LISPFUNNF(mask_field,2)
{ /* (MASK_FIELD bytespec integer), CLTL p. 226 */
  var object x = check_integer(STACK_0);
  var object b = STACK_1; /* Type check will take place later */
  skipSTACK(2);
  VALUES1(I_Byte_mask_field_I(x,b));
}

LISPFUNNF(dpb,3)
{ /* (DPB integer bytespec integer), CLTL p. 227 */
  /* STACK_1 - type check will take place later */
  STACK_2 = check_integer(STACK_2); STACK_0 = check_integer(STACK_0);
  VALUES1(I_I_Byte_dpb_I(STACK_2,STACK_0,STACK_1)); skipSTACK(3);
}

LISPFUNNF(deposit_field,3)
{ /* (DEPOSIT-FIELD integer bytespec integer), CLTL p. 227 */
  /* STACK_1 - type check will take place later */
  STACK_2 = check_integer(STACK_2); STACK_0 = check_integer(STACK_0);
  VALUES1(I_I_Byte_deposit_field_I(STACK_2,STACK_0,STACK_1)); skipSTACK(3);
}

/* checks an optional random-state-argument obj.
 check_random_state(obj)
 > obj: optional random-state-argument
 < result: the adressed random-state */
local object check_random_state (object obj) {
  if (boundp(obj)) { /* specified -> must be a random-state: */
    if (random_state_p(obj)) {
      return obj;
    } else {
      pushSTACK(obj); /* TYPE-ERROR slot DATUM */
      pushSTACK(S(random_state)); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(obj); pushSTACK(S(random_state));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,GETTEXT("~S: argument should be a ~S, not ~S"));
    }
  } else { /* not specified -> default from *RANDOM-STATE* */
    obj = Symbol_value(S(random_state_stern)); /* value of *RANDOM-STATE* */
    if (random_state_p(obj)) {
      return obj;
    } else {
      pushSTACK(obj); /* TYPE-ERROR slot DATUM */
      pushSTACK(S(random_state)); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(obj); pushSTACK(S(random_state));
      pushSTACK(S(random_state_stern));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(type_error,GETTEXT("~S: the value of ~S should be a ~S, not ~S"));
    }
  }
}

LISPFUN(random,seclass_default,1,1,norest,nokey,0,NIL)
{ /* (RANDOM number [state]), CLTL p. 228 */
  var object x = check_real(STACK_1); /* x - real, >0, Float or Integer */
  var object r = check_random_state(STACK_0);
  skipSTACK(2);
  if (R_plusp(x)) {
    if (R_floatp(x)) {
      VALUES1(F_random_F(r,x));
      return;
    } else if (RA_integerp(x)) {
      VALUES1(I_random_I(r,x));
      return;
    }
  }
  pushSTACK(x); /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_random_arg)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(x); pushSTACK(S(random));
  fehler(type_error,
         GETTEXT("~S: argument should be positive and an integer or float, not ~S"));
}

/* (SYS::RANDOM-POSFIXNUM [state]) returns a random nonnegative fixnum.
   Doesn't trigger GC. */
LISPFUN(random_posfixnum,seclass_default,0,1,norest,nokey,0,NIL)
{
  var object r = check_random_state(popSTACK());
 #if (oint_data_len <= 32)
  var uintL value = random_L(r) >> (32-oint_data_len);
 #else
  #define random1_len (oint_data_len/2)
  #define random2_len ((oint_data_len+1)/2)
  var uintV value = ((uint64)(random_L(r) >> (32-random1_len)) << random2_len)
                    | (uint64)(random_L(r) >> (32-random2_len));
 #endif
  VALUES1(fixnum(value));
}

/* make_random_state(r) returns a new random-state with initial state
 - random, if r=T,
 - from random-state *RANDOM-STATE*, if r=NIL or r=unbound,
 - from random-state r itself, otherwise.
 can trigger GC */
local maygc object make_random_state (object r)
{
  var uint32 seed_hi;
  var uint32 seed_lo;
  if (eq(r,T)) {
    /* initialize with random-bits from the operating system: */
   #if defined(UNIX)
    #ifdef TIME_UNIX
    var internal_time_t real_time; /* time */
    get_real_time(&real_time);
    seed_lo = highlow32(real_time.tv_sec,real_time.tv_usec); /* 16+16 random bits */
    #endif
    #ifdef TIME_UNIX_TIMES
    seed_lo = get_real_time(); /* time, CLK_TCK Hz */
    #endif
    begin_system_call();
    seed_hi = (rand() /* random 31 bits (on UNIX_BSD) resp. 16 bits (on UNIX_SYSV) */
               << 8) ^ (uintL)(getpid()); /* ca. 8 bits from the Process ID */
    end_system_call();
   #elif defined(WIN32_NATIVE)
    var internal_time_t real_time; /* time */
    get_real_time(&real_time);
    seed_lo = real_time.dwHighDateTime ^ real_time.dwLowDateTime;
    begin_system_call();
    seed_hi = CoGetCurrentProcess();
    end_system_call();
   #else
    #error "make_random_state() is not defined on this platform!"
   #endif
  } else { /* check random-state: */
    r = check_random_state( (nullp(r) ? unbound : r) );
    /* extract its state: */
    var object seed = The_Random_state(r)->random_state_seed;
    var uintD* seedMSDptr = (uintD*)(&TheSbvector(seed)->data[0]);
    seed_hi = get_32_Dptr(seedMSDptr);
    seed_lo = get_32_Dptr(&seedMSDptr[32/intDsize]);
  }
  { /* fetch state-bitvector and fill: */
    var object seed = allocate_bit_vector(Atype_Bit,64);
    var uintD* seedMSDptr = (uintD*)(&TheSbvector(seed)->data[0]);
    set_32_Dptr(seedMSDptr,seed_hi);
    set_32_Dptr(&seedMSDptr[32/intDsize],seed_lo);
    pushSTACK(seed);
  }
  var object state = allocate_random_state(); /* new random-state */
  The_Random_state(state)->random_state_seed = popSTACK(); /* fill with bit-vector */
  return state;
}

LISPFUN(make_random_state,seclass_default,0,1,norest,nokey,0,NIL)
{ /* (MAKE-RANDOM-STATE [state]), CLTL p. 230 */
  VALUES1(make_random_state(popSTACK()));
}

LISPFUNNF(fakultaet,1)
{ /* (! integer) */
  var object x = check_posfixnum(popSTACK()); /* x is a fixnum >=0. */
  VALUES1(FN_fak_I(x));
}

LISPFUNNF(exquo,2)
{ /* (EXT:EXQUO x y) returns the quotient of x and y. The caller
  asserts that x is a multiple of y.
  (EXQUO x y) == (THE INTEGER (/ (THE INTEGER x) (THE INTEGER y))) */
  STACK_0 = check_integer(STACK_0); STACK_1 = check_integer(STACK_1);
  VALUES1(I_I_exquo_I(STACK_1,STACK_0)); skipSTACK(2);
}

LISPFUNNF(mod_expt,3)
{ /*(EXT:MOD-EXPT base exponent modulo) = (MOD (EXPT base exponent) modulus)*/
  STACK_0 = check_integer(STACK_0); STACK_2 = check_integer(STACK_2);
  STACK_1 = check_pos_integer(STACK_1);
  VALUES1(I_I_I_mod_expt_I(STACK_2,STACK_1,STACK_0)); skipSTACK(3);
}

LISPFUNN(long_float_digits,0)
{ /* (EXT:LONG-FLOAT-DIGITS) returns the default bitsize of long-floats */
  VALUES1(UL_to_I(intDsize * I_to_UL(O(LF_digits))));
}

/* Sets the default-long-float length to the value len (>= LF_minlen).
 set_lf_digits(len);
 can trigger GC */
local maygc void set_lf_digits (uintC len) {
  O(LF_digits) = UL_to_I(len);
  /* MOST-POSITIVE-LONG-FLOAT and MOST-NEGATIVE-LONG-FLOAT : */
  { /* exponent as big as possible, mantissa 1...1 */
    var object x = allocate_lfloat(len,LF_exp_high,0);
    fill_loop_up(&TheLfloat(x)->data[0],len,~(uintD)0);
    define_variable(S(most_positive_long_float),x);
    x = LF_minus_LF(x);
    define_variable(S(most_negative_long_float),x);
  }
  /* LEAST-POSITIVE-LONG-FLOAT and LEAST-NEGATIVE-LONG-FLOAT : */
  { /* exponent as small as possible, mantissa 10...0 */
    var object x = allocate_lfloat(len,LF_exp_low,0);
    var uintD* ptr = &TheLfloat(x)->data[0];
    *ptr++ = bit(intDsize-1);
    clear_loop_up(ptr,len-1);
    define_variable(S(least_positive_long_float),x);
    define_variable(S(least_positive_normalized_long_float),x); /* X3J13 vote <79> */
    x = LF_minus_LF(x);
    define_variable(S(least_negative_long_float),x);
    define_variable(S(least_negative_normalized_long_float),x); /* X3J13 vote <79> */
  }
  /* LONG-FLOAT-EPSILON = 2^-16n*(1+2^(1-16n)) : */
  { /* exponent 1-16n, mantissa 10...01 */
    var object x = allocate_lfloat(len,LF_exp_mid+1-intDsize*(uintL)len,0);
    var uintD* ptr = &TheLfloat(x)->data[0];
    *ptr++ = bit(intDsize-1);
    ptr = clear_loop_up(ptr,len-2);
    *ptr = bit(0);
    define_variable(S(long_float_epsilon),x);
  }
  /* LONG-FLOAT-NEGATIVE-EPSILON = 2^(-16n-1)*(1+2^(1-16n)) : */
  { /* exponent -16n, mantissa 10...01 */
    var object x = allocate_lfloat(len,LF_exp_mid-intDsize*(uintL)len,0);
    var uintD* ptr = &TheLfloat(x)->data[0];
    *ptr++ = bit(intDsize-1);
    ptr = clear_loop_up(ptr,len-2);
    *ptr = bit(0);
    define_variable(S(long_float_negative_epsilon),x);
    /* PI : */
    x = O(pi) = pi_F_float_F(x);
    define_variable(S(pi),x);
  }
}

LISPFUNN(set_long_float_digits,1)
{ /* ((SETF LONG-FLOAT-DIGITS) digits) */
  var object arg = STACK_0;
  if (!posfixnump(arg)) /* not necessarily Fixnum!?? */
    fehler_digits(arg);
  var uintV d = posfixnum_to_V(arg); /* = I_to_UL(arg); ?? */
  if (d==0) /* should be >0 */
    fehler_digits(arg);
  d = ceiling(d,intDsize);
 #if (intWCsize<intVsize)
  if (d >= vbit(intWCsize))
    fehler_LF_toolong();
 #endif
  if (d < LF_minlen)
    d = LF_minlen; /* coerce d>=LF_minlen */
  set_lf_digits(d);
  VALUES1(popSTACK()); /* digits as value */
}

/* UP for LOG2 and LOG10: calculate logarithm of the fixnum x with at least
 digits bits and - if necessary - update the value in *objptr. */
local maygc object log_digits (object x, object digits, gcv_object_t* objptr) {
  /* check digits-argument: */
  if (!posfixnump(digits)) /* not necessarily Fixnum!?? */
    fehler_digits(digits);
  var uintV d = posfixnum_to_V(digits); /* = I_to_UL(digits); ?? */
  if (d==0) /* should be >0 */
    fehler_digits(digits);
  /* fetch known value: */
  var object ln_x = *objptr;
  /* convert ln_x into a float with at least d bits: */
  if (d > DF_mant_len+1) { /* -> long-float */
    d = ceiling(d,intDsize);
   #if (intWCsize<intVsize)
    if (d >= vbit(intWCsize))
      fehler_LF_toolong();
   #endif
    var uintC oldlen = Lfloat_length(ln_x); /* existing length */
    if (d < oldlen)
      return LF_shorten_LF(ln_x,d);
    if (d == oldlen)
      return ln_x;
    /* desired length > existing length -> must recalculate:
       let Lfloat_length(ln_x) grow by at least a constant factor
       > 1 , so that it is not recalculated to often: */
    oldlen += floor(oldlen,2); /* oldlen * 3/2 */
    var uintC newlen = (d < oldlen ? oldlen : d);
    ln_x = *objptr = LF_shorten_LF(R_ln_R(I_to_LF(x,newlen,true),NULL),
                                   newlen); /* (ln x) - LF of len newlen */
    return (d < newlen ? LF_shorten_LF(ln_x,d) : ln_x);
  } else if (d > FF_mant_len+1) /* a double-float is sufficient */
    return LF_to_DF(ln_x); /* -> double-float */
  else if (d > SF_mant_len+1) /* a single-float is sufficient */
    return LF_to_FF(ln_x); /* -> single-float */
  else /* a short-float is sufficient */
    return LF_to_SF(ln_x);
}

LISPFUNNR(log2,1)
{ /* (SYS::LOG2 digits) returns ln(2) with at least 'digits' bits. */
  VALUES1(log_digits(fixnum(2),popSTACK(),&O(LF_ln2)));
}

LISPFUNNR(log10,1)
{ /* (SYS::LOG10 digits) returns ln(10) with at least 'digits' Bits. */
  VALUES1(log_digits(fixnum(10),popSTACK(),&O(LF_ln10)));
}


/* ==========================================================================
 *                             Initialization */

/* mantissa of pi : */
local const uintD pi_mantisse [2048/intDsize] = {
  D(0xC9,0x0F,0xDA,0xA2,) D(0x21,0x68,0xC2,0x34,) D(0xC4,0xC6,0x62,0x8B,)
  D(0x80,0xDC,0x1C,0xD1,) D(0x29,0x02,0x4E,0x08,) D(0x8A,0x67,0xCC,0x74,)
  D(0x02,0x0B,0xBE,0xA6,) D(0x3B,0x13,0x9B,0x22,) D(0x51,0x4A,0x08,0x79,)
  D(0x8E,0x34,0x04,0xDD,) D(0xEF,0x95,0x19,0xB3,) D(0xCD,0x3A,0x43,0x1B,)
  D(0x30,0x2B,0x0A,0x6D,) D(0xF2,0x5F,0x14,0x37,) D(0x4F,0xE1,0x35,0x6D,)
  D(0x6D,0x51,0xC2,0x45,) D(0xE4,0x85,0xB5,0x76,) D(0x62,0x5E,0x7E,0xC6,)
  D(0xF4,0x4C,0x42,0xE9,) D(0xA6,0x37,0xED,0x6B,) D(0x0B,0xFF,0x5C,0xB6,)
  D(0xF4,0x06,0xB7,0xED,) D(0xEE,0x38,0x6B,0xFB,) D(0x5A,0x89,0x9F,0xA5,)
  D(0xAE,0x9F,0x24,0x11,) D(0x7C,0x4B,0x1F,0xE6,) D(0x49,0x28,0x66,0x51,)
  D(0xEC,0xE4,0x5B,0x3D,) D(0xC2,0x00,0x7C,0xB8,) D(0xA1,0x63,0xBF,0x05,)
  D(0x98,0xDA,0x48,0x36,) D(0x1C,0x55,0xD3,0x9A,) D(0x69,0x16,0x3F,0xA8,)
  D(0xFD,0x24,0xCF,0x5F,) D(0x83,0x65,0x5D,0x23,) D(0xDC,0xA3,0xAD,0x96,)
  D(0x1C,0x62,0xF3,0x56,) D(0x20,0x85,0x52,0xBB,) D(0x9E,0xD5,0x29,0x07,)
  D(0x70,0x96,0x96,0x6D,) D(0x67,0x0C,0x35,0x4E,) D(0x4A,0xBC,0x98,0x04,)
  D(0xF1,0x74,0x6C,0x08,) D(0xCA,0x18,0x21,0x7C,) D(0x32,0x90,0x5E,0x46,)
  D(0x2E,0x36,0xCE,0x3B,) D(0xE3,0x9E,0x77,0x2C,) D(0x18,0x0E,0x86,0x03,)
  D(0x9B,0x27,0x83,0xA2,) D(0xEC,0x07,0xA2,0x8F,) D(0xB5,0xC5,0x5D,0xF0,)
  D(0x6F,0x4C,0x52,0xC9,) D(0xDE,0x2B,0xCB,0xF6,) D(0x95,0x58,0x17,0x18,)
  D(0x39,0x95,0x49,0x7C,) D(0xEA,0x95,0x6A,0xE5,) D(0x15,0xD2,0x26,0x18,)
  D(0x98,0xFA,0x05,0x10,) D(0x15,0x72,0x8E,0x5A,) D(0x8A,0xAA,0xC4,0x2D,)
  D(0xAD,0x33,0x17,0x0D,) D(0x04,0x50,0x7A,0x33,) D(0xA8,0x55,0x21,0xAB,)
  D(0xDF,0x1C,0xBA,0x65,)
};

/* mantissa of ln(2) : */
local const uintD ln2_mantisse [64/intDsize] = {
  D(0xB1,0x72,0x17,0xF7,) D(0xD1,0xCF,0x79,0xAC,)
};

/* mantissa of ln(10) : */
local const uintD ln10_mantisse [64/intDsize] = {
  D(0x93,0x5D,0x8D,0xDD,) D(0xAA,0xA8,0xAC,0x17,)
};

/* UP: Initializes the arithmetics.
 init_arith();
 can trigger GC */
global maygc void init_arith (void)
{
  /* different constants: */
 #ifndef IMMEDIATE_FFLOAT
  O(FF_zero) = allocate_ffloat(0); /* 0.0F0 */
  /* encode_FF(0,1,bit(FF_mant_len), O(FF_one)=);  // 1.0F0 */
  /* encode_FF(-1,1,bit(FF_mant_len), O(FF_minusone)=); // -1.0F0 */
 #endif
 #ifdef intQsize
  O(DF_zero) = allocate_dfloat(0); /* 0.0D0 */
  /* encode_DF(0,1,bit(DF_mant_len), O(DF_one)=); // 1.0D0 */
  /* encode_DF(-1,1,bit(DF_mant_len), O(DF_minusone)=); // -1.0D0 */
 #else
  O(DF_zero) = allocate_dfloat(0,0); /* 0.0D0 */
  /* encode2_DF(0,1,bit(DF_mant_len-32),0, O(DF_one)=); // 1.0D0 */
  /* encode2_DF(-1,1,bit(DF_mant_len-32),0, O(DF_minusone)=); // -1.0D0 */
 #endif
  /* variable Long-Floats: */
  encode_LF(0,2,&pi_mantisse[0],2048/intDsize, O(LF_pi)=); /* pi in 2048 bits */
  encode_LF(0,0,&ln2_mantisse[0],64/intDsize, O(LF_ln2)=); /* ln(2) in 64 bits */
  encode_LF(0,2,&ln10_mantisse[0],64/intDsize, O(LF_ln10)=); /* ln(10) in 64 bits */
  /* default length of long-floats as small as possible: */
  set_lf_digits(LF_minlen);
  /* pi as short-, single-, double-float: */
  O(SF_pi) = LF_to_SF(O(pi));
  O(FF_pi) = LF_to_FF(O(pi));
  O(DF_pi) = LF_to_DF(O(pi));
  /* MOST-POSITIVE-FIXNUM, MOST-NEGATIVE-FIXNUM : */
  define_constant(S(most_positive_fixnum),Fixnum_mpos);
  define_constant(S(most_negative_fixnum),Fixnum_mneg);
  /* MOST/LEAST-POSITIVE/NEGATIVE-SHORT-FLOAT: */
  define_constant(S(most_positive_short_float),
                  make_SF(0,SF_exp_high,bit(SF_mant_len+1)-1));
  define_constant(S(least_positive_short_float),
                  make_SF(0,SF_exp_low,bit(SF_mant_len)));
  define_constant(S(least_negative_short_float),
                  make_SF(-1,SF_exp_low,bit(SF_mant_len)));
  define_constant(S(most_negative_short_float),
                  make_SF(-1,SF_exp_high,bit(SF_mant_len+1)-1));
  { /* MOST/LEAST-POSITIVE/NEGATIVE-SINGLE-FLOAT: */
    var object obj; encode_FF(0,FF_exp_high-FF_exp_mid,bit(FF_mant_len+1)-1, obj=);
    define_constant(S(most_positive_single_float),obj);
  }
  {
    var object obj; encode_FF(0,FF_exp_low-FF_exp_mid,bit(FF_mant_len), obj=);
    define_constant(S(least_positive_single_float),obj);
  }
  {
    var object obj; encode_FF(-1,FF_exp_low-FF_exp_mid,bit(FF_mant_len), obj=);
    define_constant(S(least_negative_single_float),obj);
  }
  {
    var object obj; encode_FF(-1,FF_exp_high-FF_exp_mid,bit(FF_mant_len+1)-1, obj=);
    define_constant(S(most_negative_single_float),obj);
  }
  { /* MOST/LEAST-POSITIVE/NEGATIVE-DOUBLE-FLOAT: */
    var object obj;
   #ifdef intQsize
    encode_DF(0,DF_exp_high-DF_exp_mid,bit(DF_mant_len+1)-1, obj=);
   #else
    encode2_DF(0,DF_exp_high-DF_exp_mid,bit(DF_mant_len-32+1)-1,bitm(32)-1, obj=);
   #endif
    define_constant(S(most_positive_double_float),obj);
  }
  {
    var object obj;
   #ifdef intQsize
    encode_DF(0,DF_exp_low-DF_exp_mid,bit(DF_mant_len), obj=);
   #else
    encode2_DF(0,DF_exp_low-DF_exp_mid,bit(DF_mant_len-32),0, obj=);
   #endif
    define_constant(S(least_positive_double_float),obj);
  }
  {
    var object obj;
   #ifdef intQsize
    encode_DF(-1,DF_exp_low-DF_exp_mid,bit(DF_mant_len), obj=);
   #else
    encode2_DF(-1,DF_exp_low-DF_exp_mid,bit(DF_mant_len-32),0, obj=);
   #endif
    define_constant(S(least_negative_double_float),obj);
  }
  {
    var object obj;
   #ifdef intQsize
    encode_DF(-1,DF_exp_high-DF_exp_mid,bit(DF_mant_len+1)-1, obj=);
   #else
    encode2_DF(-1,DF_exp_high-DF_exp_mid,bit(DF_mant_len-32+1)-1,bitm(32)-1, obj=);
   #endif
    define_constant(S(most_negative_double_float),obj);
  }
  /* For floats with d bits (incl. Hidden bit,  d = ?F_mant_len+1)
     is ...-FLOAT-EPSILON = 2^-d*(1+2^(1-d))
     and ...-FLOAT-NEGATIVE-EPSILON = 2^(-d-1)*(1+2^(1-d)) . */
  define_constant(S(short_float_epsilon),
                  make_SF(0,SF_exp_mid-SF_mant_len,bit(SF_mant_len)+1));
  define_constant(S(short_float_negative_epsilon),
                  make_SF(0,SF_exp_mid-SF_mant_len-1,bit(SF_mant_len)+1));
  {
    var object obj; encode_FF(0,-FF_mant_len,bit(FF_mant_len)+1, obj=);
    define_constant(S(single_float_epsilon),obj);
  }
  {
    var object obj; encode_FF(0,-FF_mant_len-1,bit(FF_mant_len)+1, obj=);
    define_constant(S(single_float_negative_epsilon),obj);
  }
  {
    var object obj;
   #ifdef intQsize
    encode_DF(0,-DF_mant_len,bit(DF_mant_len)+1, obj=);
   #else
    encode2_DF(0,-DF_mant_len,bit(DF_mant_len-32),1, obj=);
   #endif
    define_constant(S(double_float_epsilon),obj);
  }
  {
    var object obj;
   #ifdef intQsize
    encode_DF(0,-DF_mant_len-1,bit(DF_mant_len)+1, obj=);
   #else
    encode2_DF(0,-DF_mant_len-1,bit(DF_mant_len-32),1, obj=);
   #endif
    define_constant(S(double_float_negative_epsilon),obj);
  }
  /* further variables: */
  define_variable(S(default_float_format),S(single_float)); /* *DEFAULT-FLOAT-FORMAT* := 'SINGLE-FLOAT */
  define_variable(S(read_default_float_format),S(single_float)); /* *READ-DEFAULT-FLOAT-FORMAT* := 'SINGLE-FLOAT */
  {
    var object obj = make_random_state(T); /* new random Random-State */
    define_variable(S(random_state_stern),obj); /* =: *RANDOM-STATE* */
  }
  /* SYS::*INHIBIT-FLOATING-POINT-UNDERFLOW* := NIL */
  define_variable(S(inhibit_floating_point_underflow),NIL);
  /* *WARN-ON-FLOATING-POINT-CONTAGION* := NIL */
  define_variable(S(warn_on_floating_point_contagion),NIL);
  /* *FLOATING-POINT-CONTAGION-ANSI* := NIL */
  define_variable(S(floating_point_contagion_ansi),NIL);
  /* *WARN-ON-FLOATING-POINT-RATIONAL-CONTAGION* := NIL */
  define_variable(S(warn_on_floating_point_rational_contagion),NIL);
  /* *FLOATING-POINT-RATIONAL-CONTAGION-ANSI* := NIL */
  define_variable(S(floating_point_rational_contagion_ansi),NIL);
  /* *PHASE-ANSI* := NIL */
  define_variable(S(phase_ansi),NIL);
}
