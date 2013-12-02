/* C-Programm-Präprozessor-Hilfe:
   Fügt _EMA_ in leere Macro-Argumente ein.
   Äquivalent zu  sed -e 's/,)/,_EMA_)/g'
   Bruno Haible 29.12.1993
*/

#include <stdio.h>

#ifdef __cplusplus
extern "C" void exit(int);
#endif

int main ()
{
  int c;
  int c1 = -1;
  while (1) {
    c = getchar(); if (c==EOF) break;
    if ((c == ')') && (c1 == ','))
      fputs("_EMA_",stdout);
    putchar(c);
    c1 = c;
  }
  if (ferror(stdin) || ferror(stdout) || fclose(stdout)) { exit(1); }
  exit(0);
}

