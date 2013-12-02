/* safe-lstat.h -- EINTR-safe interface to lstat
   Copyright (C) 1994 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */
   
/* Written by Jim Meyering <meyering@comco.com>.  */

#ifndef _safe_lstat_h_
#define _safe_lstat_h_ 1

/* NOTE: you must include the following headers (in the listed order)
   before this one: <sys/types.h>, <sys/stat.h>.  */

#if !defined(S_ISLNK) && defined(S_IFLNK)
#define S_ISLNK(m) (((m) & S_IFMT) == S_IFLNK)
#endif

#ifndef S_ISLNK
#include "safe-stat.h"
#define SAFE_LSTAT SAFE_STAT
#define safe_lstat safe_stat
#else

#include <errno.h>

#ifndef errno
extern int errno;
#endif

#ifndef __GNUC__
#define __inline /* empty */
#endif

/* On some systems, lstat can return EINTR.  */

#ifndef EINTR
# define SAFE_LSTAT(name, buf) lstat (name, buf)
#else
# ifndef __static
#   define __static static
# endif
# define SAFE_LSTAT(name, buf) safe_lstat (name, buf)
__static __inline int
safe_lstat (name, buf)
     const char *name;
     struct stat *buf;
{
  int ret;

  do
    ret = lstat (name, buf);
  while (ret < 0 && errno == EINTR);

  return ret;
}
#endif

#endif /* S_ISLNK */
#endif /* _safe_lstat_h_ */
