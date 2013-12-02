/* Display width functions.
   Copyright (C) 2001-2002 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License as published
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
   USA.  */

#ifndef _UNIWIDTH_H
#define _UNIWIDTH_H

#include "unitypes.h"

/* Get size_t.  */
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif


/* Display width.  */

/* These functions are locale dependent.  The encoding argument identifies
   the encoding (e.g. "ISO-8859-2" for Polish).  */

/* Return the encoding of the current locale.  */
extern const char *
       locale_charset PARAMS ((void));

/* Determine number of column positions required for UC.  */
extern int
       uc_width PARAMS ((ucs4_t uc, const char *encoding));

/* Determine number of column positions required for first N units
   (or fewer if S ends before this) in S.  */
extern int
       u8_width PARAMS ((const uint8_t *s, size_t n, const char *encoding));
extern int
       u16_width PARAMS ((const uint16_t *s, size_t n, const char *encoding));
extern int
       u32_width PARAMS ((const uint32_t *s, size_t n, const char *encoding));

/* Determine number of column positions required for S.  */
extern int
       u8_strwidth PARAMS ((const uint8_t *s, const char *encoding));
extern int
       u16_strwidth PARAMS ((const uint16_t *s, const char *encoding));
extern int
       u32_strwidth PARAMS ((const uint32_t *s, const char *encoding));


#ifdef __cplusplus
}
#endif

#endif /* _UNIWIDTH_H */
