/* Konversionsprogramm NL -> CR/LF */
/* Bruno Haible 17.1.1991 */

#include <stdio.h>

#define NL 10
#define CR 13
#define LF 10
main ()
  { int c;
    while (!((c = getchar()) == EOF))
      { if (c==NL) { putchar(CR); putchar(LF); } else { putchar(c); } }
    if (ferror(stdin) || ferror(stdout)) { exit(1); }
    exit(0);
  }
