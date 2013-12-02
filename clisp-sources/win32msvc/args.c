/*
 * Small program, printing its arguments.
 * Used to test the RUN-PROGRAM function.
 */

#include <stdio.h>

int main (int argc, char* argv[])
{
  int i;
  printf("argc = %d\n",argc);
  for (i = 0; i < argc; i++)
    printf("argv[%d] = |%s|\n",i,argv[i]);
  return 0;
}
