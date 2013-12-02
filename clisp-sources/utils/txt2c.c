/*
 * text file with #if contructs --> C program that prints that file
 * Bruno Haible 21.2.1993, 3.1.1995, 15.6.1996
 * Sam Steingold 2002-07-31
 */

#include <stdlib.h> /* malloc(), realloc(), exit() */
#include <string.h> /* strcpy(), strcat() */

#include <stdio.h>

/* Directory in which to search files for #include <...> */
char* standard_include_path = NULL;

/* get a line, terminated with '\n', or NULL if EOF is encountered
   c is its first character */
static unsigned char * get_line (FILE * fp, int c) {
  int len = 1;
  unsigned char * line = (unsigned char *) malloc(len);
  int index = 0;
  while (1) {
    if (!line) { fprintf(stderr,"Out of memory.\n"); exit(1); }
    line[index++] = c;
    if (c=='\n') break;
    c = getc(fp); if (c==EOF) { free(line); return (unsigned char *) 0; }
    if (index >= len)
      { len = 2*len; line = (unsigned char *) realloc((void*)line,len); }
  }
  return line;
}

/* put back a line to the pool */
static void put_line (unsigned char * line)
{ free(line); }

static void process_file (FILE * fp) {
  int c;
  while (1) {
    c = getc(fp); if (c==EOF) goto eof;
    if (c=='#') {
      c = getc(fp); if (c==EOF) { putchar('#'); goto eof; }
      if ((c=='i') || (c=='e')) {
        /* heuristic for #if, #else, #endif, #include */
        unsigned char * line = get_line(fp,c);
        if (!line) goto eof;
        if (line[0]=='i' && line[1]=='n' && line[2]=='c' && line[3]=='l'
            && line[4]=='u' && line[5]=='d' && line[6]=='e' && line[7]==' ') {
          char pathname[10240];
          unsigned char expect;
          unsigned char * ptr = &line[8];
          if (!(*ptr == '"' || *ptr == '<')) goto illegal_include;
          expect = (*ptr == '"' ? '"' : '>');
          ptr++;
          while (!(*ptr == '"' || *ptr == '>' || *ptr == '\n')) { ptr++; }
          if (*ptr != expect) goto illegal_include;
          if (!(ptr[1] == '\n'))
          illegal_include:
            { fprintf(stderr,"Invalid #include syntax\n"); exit(1); }
          ptr[0] = '\0'; /* replace '"' by '\0' */
          if (expect == '"') {
            /* Search for the file in the current directory. */
            strcpy(pathname, (const char *) &line[9]);
          } else {
            /* Search for the file in the standard_include_path. */
            strcpy(pathname,standard_include_path);
            strcat(pathname, (const char *) &line[9]);
          }
          { FILE * ifp = fopen(pathname,"r");
            if (!ifp) {
              fprintf(stderr,"Could not open include file %s\n",pathname);
              exit(1);
            }
            process_file(ifp);
            fclose(ifp);
          }
        } else {                /* pass unchanged */
          unsigned char * ptr = line;
          putchar('#');
          do { c = *ptr++; putchar(c); } while (!(c=='\n'));
        }
        put_line(line);
        goto line_ok;
      }
      printf("printf(\"#\");\n");
    }
    /* convert line --> printf() instruction */
    printf("  printf(\"");
    while (1) {
      if ((c=='\\') || (c=='\"')) { putchar('\\'); putchar(c); }
#if defined(sun) && !defined(__GNUC__)
      else if (c==0377)
        { putchar('\\'); putchar(c); }
#endif
#ifdef QUOTE_QUOTES
      else if (c=='\'') { /* instead of "'" output "\047": */
        putchar('\\');
        putchar('0'+((((unsigned char)'\'')/64)%8));
        putchar('0'+((((unsigned char)'\'')/8)%8));
        putchar('0'+(((unsigned char)'\'')%8));
      }
#endif
      else if (c=='%')
        { putchar(c); putchar(c); }
      else if (c!='\n')
        { putchar(c); }
      else
        { putchar('\\'); putchar('n'); break; }
      c = getc(fp); if (c==EOF) { printf("\");\n"); goto eof; }
    }
    printf("\");\n");
   line_ok: ;
  }
 eof: ;
}

int main (int argc, char* argv[]) {
  int i;
  for (i = 1; i < argc; i++) {
    char* arg = argv[i];
    if (arg[0]=='-' && arg[1]=='I') {
      standard_include_path = &arg[2];
    } else {
      fprintf(stderr,"Bad option.\n"); exit(1);
    }
  }
#ifdef CROSS
  printf("#include \"lispbibl.h\"\n");
#else
  printf("#include \"lispbibl.c\"\n");
#endif
  printf("#include <stdio.h>\n");
  printf("#ifdef __cplusplus\n");
  printf("extern \"C\" void exit(int);\n");
  printf("#endif\n");
  printf("\n");
  printf("int main () {\n");
  process_file(stdin);
  printf("  if (ferror(stdout) || fclose(stdout)) { exit(1); }\n");
  printf("  exit(0);\n");
  printf("}\n");
  if (ferror(stdout) || fclose(stdout)) { exit(1); }
  exit(0);
}
