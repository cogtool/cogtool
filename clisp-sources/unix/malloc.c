/* compile:  cc -o malloc malloc.c
   try:      malloc 50000
             malloc 100000
             malloc 1000000
*/
#include <stdio.h>

void printf_address (unsigned long addr) {
  if (sizeof(unsigned long) <= 4)
    printf ("#x%08X", (unsigned int)addr);
  else
    printf ("#x%08X%08X",(unsigned int)(addr>>32),
            (unsigned int)(addr & 0xFFFFFFFF));
}

void test_malloc (unsigned long size) {
  unsigned long result = (unsigned long)malloc(size);
  printf ("malloc(%12d) = ",size); printf_address (result); printf ("\n");
}

int main (int argc, char* argv[]) {
  int i;
  if (argc == 1)
    for (i = 1; i < 40; i++)
      test_malloc(1 << i);
  else
    for (i = 1; i < argc; i++)
      test_malloc(atol(argv[i]));
  printf ("&main = "); printf_address ((unsigned long)&main); printf ("\n");
  return 0;
}
