/* safe-stat.h -- EINTR-safe interface to stat
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

#ifndef _safe_stat_h_
#define _safe_stat_h_ 1

/* NOTE: you must include the following headers (in the listed order)
   before this one: <sys/types.h>, <sys/stat.h>.  */

#include <errno.h>

#ifndef errno
extern int errno;
#endif

#ifndef __GNUC__
#define __inline /* empty */
#endif

/* On some systems, stat can return EINTR.  */

#ifndef EINTR
# define SAFE_STAT(name, buf) stat (name, buf)
#else
# ifndef __static
#   define __static static
# endif
# define SAFE_STAT(name, buf) safe_stat (name, buf)
__static __inline int
safe_stat (name, buf)
     const char *name;
     struct stat *buf;
{
  int ret;

  do
    ret = stat (name, buf);
  while (ret < 0 && errno == EINTR);

  return ret;
}
#endif

#endif /* _safe_stat_h_ */
