/* Konversionsprogramm CR/LF -> NL */
/* Bruno Haible 17.1.1991 */

#include <stdio.h>

#define CR 13
#define LF 10
#define NL 10
main ()
  { int c;
    loop:
      c = getchar(); if (c==EOF) goto eof;
      if (c==CR)
        { c = getchar(); if (c==EOF) { putchar(CR); goto eof; }
          if (!(c==LF)) { putchar(CR); putchar(c); }
             else { putchar(NL); }
        }
        else
        { putchar(c); }
      goto loop;
    eof: ;
    if (ferror(stdin) || ferror(stdout)) { exit(1); }
    exit(0);
  }
