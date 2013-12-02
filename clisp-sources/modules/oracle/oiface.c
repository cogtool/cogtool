/* CLISP Oracle client interface */

/* (C) 2002 Alma Mater Software, Inc. */

/* This library implements a simplified, but fairly complete interface
   on top of the Oracle V.8 OCI client library.  It maintains one or
   more connections to an Oracle server and allows each connection to
   issue any SQL command and retrieve results.

   $Id: oiface.c,v 1.1 2002/09/11 14:27:50 hin Exp $

  */
   
#include <stdlib.h>
#include <oci.h>
#include "oiface.h"

/* Map (char *) 0 to the empty string */
static char * valid_string(char *);

/* ---------------------------------------------------------------------- */

/* Return text of last error */ 
char * oracle_last_error(void * dbptr)
{
  struct db_conn * db = (struct db_conn *) dbptr;
  return valid_string(db->errmsg);
}

/* ----------------   Local routines   ---------------------- */

/* Map null pointer to empty string */
static char * valid_string(char *s)
{
  return s ? s : "";
}
