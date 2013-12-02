/*
 * Interface to the queens() function
 */

#include "clisp.h"

DEFMODULE(queens,"USER")

#define nmax 100

/* extern declaration */
extern unsigned int queens (unsigned int n);

/*
 * (USER::QUEENS n) returns the number of solutions to the n-queens problem.
 * n ought to be an integer > 0, <= nmax. Otherwise it returns NIL.
 */
DEFUN(USER::QUEENS, n)
{ /*
   * garbage collection is not a problem,
   * so we get the argument from the STACK immediately,
   * and clean up STACK at the same time
   */
  unsigned int n = I_to_uint(check_uint(popSTACK()));
  if (n>0 && n<=nmax) { /* the argument is good. Do our job: */
    unsigned int result;
    begin_call();
    result = queens(n); /* call external function */
    end_call();
    /*
     * Assume result is >=0 and <2^32 (which is guaranteed by the type
     * of problem we have and the amount of time queens() may have run).
     * So an unsigned int' is enough, and the following call is appropriate.
     */
    VALUES1(uint_to_I(result)); /* convert result to a nonnegative integer */
  } else /* We could issue an error. We prefer to return NIL here. */
    VALUES1(NIL);
}

