/* Determine some float parameters, much like gcc's "enquire.c". */
/* Bruno Haible 24.8.1996 */

/* This program expects to be compiled by an ANSI C or C++ compiler. */

#include <stdio.h>

#if !(defined(__STDC__) || defined(__cplusplus))
/* Only for use in function parameter lists and as function return type. */
#define void
#endif

typedef int boolean;
#define TRUE  1
#define FALSE 0

#if HAVE_LONG_DOUBLE
typedef long double ldouble;
#endif

static void header (void)
{
  printf("/* Rounding modes, for use below */\n");
  printf("#define rounds_to_nearest        0  /* 0.5 ulp */\n");
  printf("#define rounds_to_zero           1  /* 1 ulp */\n");
  printf("#define rounds_to_infinity       2  /* 1 ulp */\n");
  printf("#define rounds_to_minus_infinity 3  /* 1 ulp */\n");
  printf("\n");
}

#define check(type,typeprefix,typestr,equalfn,mainfn)  \
static boolean equalfn (type* x, type* y);				\
static void mainfn (void)						\
{									\
  int mant_bits;							\
  int epsilon_bits = -1;						\
  int negepsilon_bits = -1;						\
  boolean rounds_to_nearest;						\
  {									\
    type x = 1.0; type y; type z;					\
    for (y = 1.0; ; y = 0.5*y) {					\
      z = x + y; if (equalfn(&x,&z)) break;				\
      z = z - x; if (!equalfn(&y,&z)) break;				\
      epsilon_bits++;							\
    }									\
  }									\
  {									\
    type x = 1.0; type y; type z;					\
    for (y = -1.0; ; y = 0.5*y) {					\
      z = x + y; if (equalfn(&x,&z)) break;				\
      z = z - x; if (!equalfn(&y,&z)) break;				\
      negepsilon_bits++;						\
    }									\
  }									\
  printf("/* Properties of type '%s': */\n",typestr);			\
  printf("/* Largest n for which 1+2^(-n) is exactly represented is %d. */\n",epsilon_bits); \
  printf("/* Largest n for which 1-2^(-n) is exactly represented is %d. */\n",negepsilon_bits); \
  if (negepsilon_bits <= epsilon_bits) {				\
    printf("#error \"No exponent jump at 1.0 for type %s!\"\n",typestr); \
    mant_bits = -1;							\
  } else {								\
    if (negepsilon_bits > epsilon_bits+1)				\
      printf("/* Base for type '%s' is 2^%d\n",typestr,negepsilon_bits-epsilon_bits); \
    mant_bits = epsilon_bits+1;						\
    printf("#define %s_mant_bits %d\n",typeprefix,mant_bits);		\
  }									\
  {									\
    int i; type x, y1, y2, ys1, ys2, z1, z2, zs1, zs2;			\
    x = 1.0; for (i = 0; i < epsilon_bits; i++) x = 0.5*x;		\
    y1 = 1.0 + 5.0*x; y2 = 1.0 + 6.0*x;					\
    ys1 = 1.0 + 5.4*x; ys2 = 1.0 + 5.6*x;				\
    z1 = -1.0 + (-5.0)*x; z2 = -1.0 + (-6.0)*x;				\
    zs1 = -1.0 + (-5.4)*x; zs2 = -1.0 + (-5.6)*x;			\
    rounds_to_nearest = FALSE;						\
    if (equalfn(&ys1,&y1) && equalfn(&ys2,&y2) && equalfn(&zs1,&z1) && equalfn(&zs2,&z2)) { \
      printf("#define %s_rounds rounds_to_nearest\n",typeprefix);	\
      rounds_to_nearest = TRUE;						\
    } else if (equalfn(&ys1,&y1) && equalfn(&ys2,&y1) && equalfn(&zs1,&z1) && equalfn(&zs2,&z1)) \
      printf("#define %s_rounds rounds_to_zero\n",typeprefix);		\
    else if (equalfn(&ys1,&y2) && equalfn(&ys2,&y2) && equalfn(&zs1,&z1) && equalfn(&zs2,&z1)) \
      printf("#define %s_rounds rounds_to_infinity\n",typeprefix);	\
    else if (equalfn(&ys1,&y1) && equalfn(&ys2,&y1) && equalfn(&zs1,&z2) && equalfn(&zs2,&z2)) \
      printf("#define %s_rounds rounds_to_minus_infinity\n",typeprefix); \
    else								\
      printf("#error \"Unknown rounding mode for type %s!\"\n",typestr); \
  }									\
  if (rounds_to_nearest) {						\
    int i; type x;							\
    type one;			/* 1 */					\
    type one_eps;		/* 1+2^(-epsilon_bits) */		\
    type one_negeps;		/* 1-2^(-negepsilon_bits) */		\
    type one_small;		/* 2^(-epsilon_bits-1) */		\
    type one_eps_small;		/* (1+2^(-epsilon_bits)) * 2^(-epsilon_bits-1) */ \
    type one_negeps_small;	/* (1-2^(-negepsilon_bits)) * 2^(-epsilon_bits-1) */ \
    type sum1;	/* one + one_small, should round down (round to even) */\
    type sum2;	/* one + one_eps_small, should round up */		\
    type sum3;	/* one + one_negeps_small, should round down */		\
    one = 1;								\
    x = one; for (i = 0; i < epsilon_bits; i++) x = 0.5*x; one_eps = one + x; \
    x = one; for (i = 0; i < negepsilon_bits; i++) x = 0.5*x; one_negeps = one - x; \
    x = one; for (i = 0; i < epsilon_bits+1; i++) x = 0.5*x; one_small = x; \
    x = one_eps; for (i = 0; i < epsilon_bits+1; i++) x = 0.5*x; one_eps_small = x; \
    x = one_negeps; for (i = 0; i < epsilon_bits+1; i++) x = 0.5*x; one_negeps_small = x; \
    sum1 = one + one_small; sum2 = one + one_eps_small; sum3 = one + one_negeps_small; \
    if (equalfn(&sum1,&one) && !equalfn(&sum2,&one) && equalfn(&sum3,&one)) \
      printf("#define %s_rounds_correctly 1\n",typeprefix);		\
    else								\
      printf("#define %s_rounds_correctly 0\n",typeprefix);		\
  }									\
  printf("\n");								\
}									\
static boolean equalfn (type* x, type* y) { return *x == *y; }		\

check(float,"float","float",equal_float,main_float)
check(double,"double","double",equal_double,main_double)
#ifdef HAVE_LONGDOUBLE
check(ldouble,"long_double","long double",equal_ldouble,main_ldouble)
#endif


int main()
{
  header();
  main_float();
  main_double();
#ifdef HAVE_LONGDOUBLE
  main_ldouble();
#endif

  if (ferror(stdout) || fclose(stdout)) return 1;
  return 0;
}
