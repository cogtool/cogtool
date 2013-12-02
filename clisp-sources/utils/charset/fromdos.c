/* Conversion utility CR/LF -> NL */
/* multiple files, in place, restores write date */
/* Bruno Haible 23.1.1994 */
/* Make:  gcc -O2 -fomit-frame-pointer -N -o fromdos fromdos.c  */

#include <sys/types.h>
#include <unistd.h> /* open, read, write, close, lseek, ftruncate */
#include <fcntl.h> /* O_RDONLY, O_RDWR */
#include <sys/stat.h> /* fstat */
#include <sys/time.h> /* struct timeval, utimes */
#include <errno.h> /* EINTR, perror */

#define CR 13
#define LF 10
#define NL 10
#define BUFLEN 4096

int full_read (int fd, char* buf, size_t nbyte);
int full_write (int fd, char* buf, size_t nbyte);

int main (int argc, char* argv[])
{ int i;
  for (i=1; i<argc; i++)
    { char* filename = argv[i];
      /* open filename twice so that there is no need to lseek */
      int rfd = open(filename,O_RDONLY);
      if (rfd<0) { perror(filename); goto next; }
     {struct stat statbuf;
      if (fstat(rfd,&statbuf)<0) { perror(filename); goto next; }
      { int wfd = open(filename,O_RDWR);
        if (wfd<0) { perror(filename); close(rfd); goto next; }
        { char buffer[BUFLEN];
          char* bufend = &buffer[BUFLEN];
          register char* srcptr = &buffer[0];
          register char* destptr;
          while (1) /* srcptr is either &buffer[0] or &buffer[1] here */
            { register int count = full_read(rfd,srcptr,bufend-srcptr);
              if (count < 0)
                { perror(filename); close(wfd); close(rfd); goto next; }
              if (count == 0) break; /* EOF */
              count += (srcptr - &buffer[0]);
              destptr = srcptr = &buffer[0];
              do /* srcptr + count remains constant, destptr<=srcptr, count>0 */
                 { if (*srcptr == CR)
                     { if (count > 1)
                         { if (srcptr[1] == LF)
                             { *destptr++ = NL; srcptr += 2; count -= 2; }
                             else
                             { *destptr++ = *srcptr++; count--; }
                         }
                         else
                         break; /* CR at the end of the buffer -> finish loop */
                     }
                     else
                     { *destptr++ = *srcptr++; count--; }
                 }
                 while (count > 0);
              /* count = 0 or = 1 here */
              if (full_write(wfd,buffer,destptr-buffer) < 0)
                { perror(filename); close(wfd); close(rfd); goto next; }
              if (count > 0)
                { buffer[0] = *srcptr; srcptr = &buffer[1]; }
                else
                { srcptr = &buffer[0]; }
            }
          if (srcptr!=buffer)
            { if (full_write(wfd,buffer,srcptr-buffer) < 0)
                { perror(filename); close(wfd); close(rfd); goto next; }
            }
        }
        { off_t length = lseek(wfd,0,SEEK_CUR);
          if (length < 0)
            { perror(filename); close(wfd); close(rfd); goto next; }
          if (ftruncate(wfd,length) < 0)
            { perror(filename); close(wfd); close(rfd); goto next; }
        }
        close(wfd);
      }
      close(rfd);
      /* reset the access and modification times */
      { struct timeval tv[2];
        tv[0].tv_sec = statbuf.st_atime; tv[0].tv_usec = 0;
        tv[1].tv_sec = statbuf.st_mtime; tv[1].tv_usec = 0;
        if (utimes(filename,tv) < 0) { perror(filename); goto next; }
      }
     }
      next: ;
    }
  exit(0);
}

/* On POSIX systems, read() and write() may return partial results without
   failing.  Redefine the functions read() and write() such that they retry
   whenever a partial result is received or the system call is interrupted
   by a signal.
   The function full_read and full_write behave like BSD read() and write():
     on success, they return nbyte;
     on EOF, they return 0;
     on failure, they return a negative value and set errno.
*/

int full_read (fd, buf, nbyte)
  int fd;
  char *buf;
  size_t nbyte;
{
  register int retval;
  size_t done = 0;
  while (nbyte > 0)
    {
      retval = read (fd, buf, nbyte);
      if (retval == 0)
        break;
      else if (retval < 0)
        {
#ifdef EINTR
          if (errno != EINTR)
#endif
            return retval;
        }
      else
        {
          buf += retval;
          done += (size_t) retval;
          nbyte -= (size_t) retval;
        }
    }
  return done;
}

int full_write (fd, buf, nbyte)
  int fd;
  char *buf;
  size_t nbyte;
{
  register int retval;
  size_t done = 0;
  while (nbyte > 0)
    {
      retval = write (fd, buf, nbyte);
      if (retval == 0)
        break;
      else if (retval < 0)
        {
#ifdef EINTR
          if (errno != EINTR)
#endif
            return retval;
        }
      else
        {
          buf += retval;
          done += (size_t) retval;
          nbyte -= (size_t) retval;
        }
    }
  return done;
}

