/* copy structs */

/*
 * Copyright 1995-1999, 2005 Bruno Haible, <bruno@clisp.org>
 *
 * This is free software distributed under the GNU General Public Licence
 * described in the file COPYING. Contact the author if you don't have this
 * or can't live with it. There is ABSOLUTELY NO WARRANTY, explicit or implied,
 * on this software.
 */

void __structcpy (void* dest, const void* src, unsigned long size, unsigned long alignment)
{
  if (alignment % sizeof(long))
    { char* d = (char*)dest;
      const char* s = (const char*)src;
      do { *d++ = *s++; } while (--size > 0);
    }
  else
    /* If the alignment is a multiple of sizeof(long), the size is as well. */
    { long* d = (long*)dest;
      const long* s = (const long*)src;
      do { *d++ = *s++; } while ((size -= sizeof(long)) > 0);
    }
}
