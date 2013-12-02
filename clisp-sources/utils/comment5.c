/* dient zum Umwandeln von Kommentaren:
 Input:
      text # comment
 Output:
      text / * comment * /
Bruno Haible 9.7.1990, 6.8.1990, 15.8.1990, 22.9.1990, 17.1.1991, 31.8.1991

Aufrufmöglichkeiten, um program.d in program.c umzuwandeln:
   comment program
   comment           und dann von Hand 'program' eingeben
   comment program.d program.c
   comment program.d > program.c

Methode:
Das Inputfile wird Zeile für Zeile umgewandelt. Ein '#', gefolgt von ' ',
leitet einen Kommentar ein, der bis Zeilenende geht.
'#'' ' wird durch '/''*'' ' ersetzt, und vors Zeilenende kommt ' ''*''/'.
Ein '\' unmittelbar am Ende einer Zeile wird dabei zum Zeilenende gerechnet.
Als erste Zeile wird  '#line 1 "sourcefile"' eingefügt.

Syntax eines Inputfile:
-------> Char ----> '#' -> ' ' ----> Char ----> '\\' ---> '\n' ----->
   / /         \                 /         \ \       /           \
  |  \         /                 \         /  --->---             |
   \  --<------                   --<------                      /
    -------------------<-----------------------------------------
*/

#include <stdlib.h>

#include <stdio.h>
#define fopen_read_ascii  "r"
#define fopen_write_ascii  "w"
#define fputc  putc
#define fgetc  getc

int main (int argc, char* argv[]) {
  char infilenamebuffer[1000];
  char outfilenamebuffer[1000] = "stdout";
  FILE * infile;
  FILE * outfile;
  if (argc==3) { /* Aufruf der Form 'comment source destination' */
    { char* p1 = argv[1]; char* p2 = infilenamebuffer;
      while ((*p2++ = *p1++) != '\0');
    }
    { char* p1 = argv[2]; char* p2 = outfilenamebuffer;
      while ((*p2++ = *p1++) != '\0');
    }
  } else {
    char filenamebuffer[1000];
    char* filename;
    if (argc==2)
      { filename = argv[1]; }
    else {
      printf("Filename: ");
      filename = fgets(filenamebuffer,sizeof(filenamebuffer),stdin);
      if (filename==NULL) { exit(__LINE__); }
    }
    { /* infilename basteln: filename+".d" */
      char* p = infilenamebuffer;
      { char* p2 = filename;
        char c; while ((c = *p2++) != '\0') { *p++ = c; }
      }
      /* Endet filename bereits mit ".d" ? */
      if ((&p[-2] >= infilenamebuffer) && (p[-2]=='.') && (p[-1]=='d'))
        { *p++ = '\0'; goto to_stdout; } /* ja -> Output nach stdout */
      *p++ = '.'; *p++ = 'd';
      *p++ = '\0';
    }
    { /* outfilename basteln: filename+".c" */
      char* p = outfilenamebuffer;
      { char* p2 = filename;
        char c; while ((c = *p2++) != '\0') { *p++ = c; }
      }
      *p++ = '.'; *p++ = 'c';
      *p++ = '\0';
    }
  }
  if ((infile = fopen(infilenamebuffer,fopen_read_ascii))==NULL)
    { perror(infilenamebuffer); exit(__LINE__); }
  if ((outfile = fopen(outfilenamebuffer,fopen_write_ascii))==NULL)
    { fclose(infile); perror(outfilenamebuffer); exit(__LINE__); }
  if (0) {
   to_stdout:
    if ((infile = fopen(infilenamebuffer,fopen_read_ascii))==NULL)
      { perror(infilenamebuffer); exit(__LINE__); }
    outfile = stdout; /* outfile = Standard-Output */
  }
  { /* write header in outfile: */
    fputs("#line 1 \"",outfile);
    fputs(infilenamebuffer,outfile);
    fputs("\"\n",outfile);
  }
  /* infile in outfile kopieren: */
#define fput_startcomment(outfile)                                      \
    { fputc('/',outfile); fputc('*',outfile); fputc(' ',outfile); }
#define fput_endcomment(outfile)                                        \
    { fputc(' ',outfile); fputc('*',outfile); fputc('/',outfile); }
  { register int c;
    L1:  /* innerhalb einer Zeile, vor Kommentar */
         c = fgetc(infile) ;
    L1a: if (c==EOF){ goto L3; }
         if (!(c=='#')) goto L4a;
         /* innerhalb einer Zeile, nach '#', vor ' ' */
         c = fgetc(infile) ;
         if (!(c==' ')) { fputc('#',outfile); goto L1a; }
         fput_startcomment(outfile);
    L2:  /* innerhalb eines Kommentars */
         /* inside '#' comment */
         c = fgetc(infile) ;
    L2a: if (c==EOF) { fput_endcomment(outfile); goto L3; }
         if (c=='\n') { fput_endcomment(outfile); fputc(c,outfile); goto L1; }
         if (!(c=='\\')) { fputc(c,outfile); goto L2; }
         /* innerhalb eines Kommentars, nach '\\' */
         c = fgetc(infile) ;
         if (!(c=='\n')) { fputc('\\',outfile); goto L2a; }
         fput_endcomment(outfile); fputc('\\',outfile); fputc(c,outfile);
         goto L1;
    L4a: /* possibly start of '／＊' comment */
         /* EOF impossible */
         fputc(c,outfile);
         if (!(c=='/')) {  goto L1; } 
         c = fgetc(infile) ;
         if (c==EOF){ goto L3; }
         fputc(c,outfile);
         if (!(c=='*')) goto L1;
    L5:  /* Inside '／＊' comment */
         c = fgetc(infile) ;
    L5a: if (c==EOF){ goto L3; } /* do not fix programmer errors */
         fputc(c,outfile);
         if (c=='*') goto L6;
         goto L5;
    L6:  /* after asterisk in '／＊' comment */
         c = fgetc(infile) ;
    L6a: if (c==EOF){ goto L3; }
         fputc(c,outfile);
         if (c=='/') goto L1;
         goto L5;
    L3:  ; /* am File-Ende */
  }
  /* error checking should work after file closing, but it does not */
  if (ferror(infile) || fclose(infile))
    { perror(infilenamebuffer); exit(__LINE__); }
  if (ferror(outfile) || fclose(outfile))
    { perror(outfilenamebuffer); exit(__LINE__); }
  exit(0); /* OK */
}

