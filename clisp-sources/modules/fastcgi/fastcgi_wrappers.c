/* fastcgi_wrappers.c

   These functions are in a separate file that includes the FastCGI
   headers, which override the normal stdio routines.

   Copyright (C) 2003 Alma Mater Software, Inc.
   Author: "John Kelley Hinsdale" <hin@alma.com>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2 as
   published by the Free Software Foundation; see file GNU-GPL.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

   $Id: fastcgi_wrappers.c,v 1.2 2005/05/20 20:03:51 haible Exp $
*/

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef _WIN32
#include <process.h>
#else
extern char **environ;
#endif

/* For strchr(), strncmp() and friends */
#include <string.h> 

#include "fcgi_stdio.h"

/* Crank this up as needed */
#define TEMPBUFSIZE 65536

/* Local functions */
static char * read_stdio(FILE *);
static int    write_stdio(FILE *, char *, int);

/* Externally visible functions */

/* Searh environment for variable */
char * fcgi_getenv(char * var) {
  char **envp = environ;
  for ( ; *envp != NULL; envp++) {
	char * equ = strchr(*envp, '=');
	if ( ! equ )
	  continue;
	if ( 0 == strncmp(var, *envp, equ - *envp) )
	  return equ + 1;
  }
  /* Variable not found in environment */
  return 0;
}

/* Read some bytes from stdin.  Return a null-terminated string.  This
   does NOT necessarily read up to end of file, but rather will read until
   its buffer is full.  Therefore, if you want to slurp in the entire contents
   of STDIN (which you usually do) you have to call this repeatedly.

   Furthermore, the result is returned to CLISP as a null-terminated
   string, so that occurrences of NUL (binary 0) characters in the
   data will cause Lisp to get a short string.  In web forms, however,
   such data will usually be encoded.

*/
char * fcgi_read_stdin() {
  return read_stdio(stdin);
}
static char * read_stdio(FILE * f) {

  static char 	buf[TEMPBUFSIZE + 1];
  size_t 	  	nact = 0;

  if ( ! feof(f) )
	nact = fread(buf, 1, TEMPBUFSIZE, f);
  if ( ferror(f) )
	nact = 0;
  buf[nact] = '\0';
  return buf;
}

/* Write to stdout or stderr */
int fcgi_write_stdout(char * data, int len) {
  return write_stdio(stdout, data, len);
}
int fcgi_write_stderr(char * data, int len) {
  return write_stdio(stderr, data, len);
}
int write_stdio(FILE * f, char * data, int len) {
  return fwrite(data, 1, len, f);
}

/* Wrappers. These are needed only due to the user of upper case (how
   annoying) */
int fcgi_is_cgi_wrapper() {
  return FCGX_IsCGI();
}
int fcgi_accept_wrapper() {
  return FCGI_Accept();
}
void fcgi_finish_wrapper() {
  FCGI_Finish();
}
