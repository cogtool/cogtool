/*
 * Hardcode some pathnames into an existing executable.
 *
 * This program is used when installing clisp binaries on a system which
 * lacks an ANSI C compiler in its default configuration (such as Solaris
 * or HP-UX).
 *
 * Usage: hardcode [-Dname=value]... input.exe output.exe
 *
 * Bruno Haible 17.5.1999
 */


/* Declare strchr(), strcmp(), strcpy(). */
#include <string.h>

/* Declare malloc(). */
#include <stdlib.h>

/* Declare stderr, fprintf(). */
#include <stdio.h>

/* Declare open(), read(), creat(), write(), chmod(), close(). */
#include <unistd.h>
#include <fcntl.h>

/* Declare fstat(). */
#include <sys/types.h>
#include <sys/stat.h>

/* Declare perror() on some systems. Define EINTR. */
#include <errno.h>


struct substitution {
  struct substitution* next;
  const char* name;
  const char* value;
};

int main (int argc, char* argv[])
{
  struct substitution* defs = NULL;
  const char* input = NULL;
  const char* output = NULL;
  const char* program_name = argv[0];

  /* Check arguments. */
  {
    int i;
    for (i = 1; i < argc; i++) {
      char* arg = argv[i];
      if (arg[0] == '-') {
        if (arg[1] == 'D') {
          char* equals = strchr(&arg[2],'=');
          if (equals) {
            struct substitution* newdef = (struct substitution*) malloc(sizeof(struct substitution));
            newdef->name = &arg[2];
            *equals = '\0';
            newdef->value = equals+1;
            newdef->next = defs;
            defs = newdef;
            continue;
          }
        }
      } else {
        if (!input) {
          input = arg;
          continue;
        }
        if (!output) {
          output = arg;
          continue;
        }
      }
      goto usage;
    }
  }
  if (!input || !output) goto usage;

  /* Eat the file. */
  {
    int inputfd;
    int outputfd;
    struct stat inputstat;
    int buflen;
    char* buf;
    
    /* Read the input file. */
    inputfd = open(input,O_RDONLY);
    if (inputfd < 0) {
      perror(input);
      exit(1);
    }
    if (fstat(inputfd,&inputstat) < 0) {
      perror(input);
      exit(1);
    }
    buflen = inputstat.st_size;
    buf = (char*) malloc(buflen+1);
    if (!buf) {
      errno = ENOMEM;
      perror(program_name);
      exit(1);
    }
    {
      int count = 0;
      while (count < buflen) {
        int n = read(inputfd,buf+count,buflen-count);
        if (n < 0) {
          if (errno != EINTR) {
            perror(input);
            exit(1);
          }
        } else if (n == 0) {
          fprintf(stderr,"Cannot read all of %s\n",input);
          exit(1);
        } else
          count += n;
      }
    }
    buf[buflen] = '\0'; /* so we can use strchr() and strcmp() in the buffer */
    close(inputfd);
    /* Replace all values. */
    {
      char* ptr;
      for (ptr = buf; ptr+7 < buf+buflen; ptr++) {
        if (ptr[0] == '%'
            && ptr[1] == 'M'
            && ptr[2] == 'A'
            && ptr[3] == 'G'
            && ptr[4] == 'I'
            && ptr[5] == 'C'
            && ptr[6] == '%') {
          char* assignment = ptr+7;
          char* equals = strchr(ptr,'=');
          if (!equals) {
            fprintf(stderr,"Malformed contents in %s\n",input);
            exit(1);
          }
          *equals = '\0';
          {
            struct substitution* def;
            for (def = defs; def != NULL; def = def->next) {
              if (!strcmp(def->name,assignment)) {
                strcpy(equals+1,def->value);
                break;
              }
            }
            if (def == NULL) {
              fprintf(stderr,"Warning: No replacement given for %s\n",assignment);
            }
          }
          *equals = '=';
        }
      }
    }
    /* Write the output file. */
    outputfd = creat(output,0644);
    if (outputfd < 0) {
      perror(output);
      exit(1);
    }
    {
      int count = 0;
      while (count < buflen) {
        int n = write(outputfd,buf+count,buflen-count);
        if (n < 0) {
          if (errno != EINTR) {
            perror(output);
            exit(1);
          }
        } else if (n == 0) {
          fprintf(stderr,"Disk full while writing to %s\n",output);
          exit(1);
        } else
          count += n;
      }
    }
    close(outputfd);
    if (chmod(output,0755) < 0) {
      perror(output);
      exit(1);
    }
  }
  return 0;

 usage:
  fprintf(stderr, "Usage: %s [-Dname=value]... input.exe output.exe\n", program_name);
  exit(1);
}
