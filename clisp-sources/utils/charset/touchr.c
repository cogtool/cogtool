/* Copies the date from one file to another file. */
/* "touchr file1 file2" is equivalent to
   "touch -r file1 file2", assuming GNU touch. */
/* Bruno Haible 8.7.1994 */

#include <sys/types.h>
#include <sys/stat.h> /* stat */
#include <sys/time.h> /* struct timeval, utimes */
#include <errno.h> /* EINTR, perror */

int main (argc,argv)
  int argc;
  char* argv[];
{ if (argc != 1+2) { exit(1); }
 {char* filename1 = argv[1];
  char* filename2 = argv[2];
  struct stat statbuf;
  struct timeval tv[2];
  if (stat(filename1,&statbuf) < 0) { perror(filename1); exit(1); }
  tv[0].tv_sec = statbuf.st_atime; tv[0].tv_usec = 0;
  tv[1].tv_sec = statbuf.st_mtime; tv[1].tv_usec = 0;
  if (utimes(filename2,tv) < 0) { perror(filename2); exit(1); }
  exit(0);
}}

