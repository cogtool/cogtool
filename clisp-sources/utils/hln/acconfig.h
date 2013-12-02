/* acconfig.h
   This file is in the public domain.

   Descriptive text for the C preprocessor macros that
   the distributed Autoconf macros can define.
   No software package will use all of them; autoheader copies the ones
   your configure.in uses into your configuration header file templates.

   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  Although this order
   can split up related entries, it makes it easier to check whether
   a given entry is in the file.

   Leave the following blank line there!!  Autoheader needs it.  */


/* Define to `unsigned long' if <sys/types.h> doesn't define.  */
#undef ino_t

/* Define if there is a member named d_ino in the struct describing
   directory headers.  */
#undef D_INO_IN_DIRENT

/* Define as the real value of ELOOP even if it is hidden in <errno.h>. */
#undef ELOOP_VALUE


/* Leave that blank line there!!  Autoheader needs it.
   If you're adding to this file, keep in mind:
   The entries are in sort -df order: alphabetical, case insensitive,
   ignoring punctuation (such as underscores).  */
