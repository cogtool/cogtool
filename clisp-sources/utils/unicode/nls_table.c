/*
 * Generates an 8-bit character set table from a .TXT file as found on
 * ftp.unicode.org.
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

int main (int argc, char *argv[])
{
  const char* charsetname;
  const char* filename;
  const char* directory;
  char* c_charsetname;
  int charset2uni[0x100];

  if (argc != 3 && argc != 4)
    exit(1);
  charsetname = argv[1];
  filename = argv[2];
  directory = (argc > 3 ? argv[3] : "");

  fprintf(stderr, "Creating %s%s\n", directory, filename);

  {
    int i, c;
    /* Read a unicode.org style .TXT file. */
    for (i = 0; i < 0x100; i++)
      charset2uni[i] = 0xfffd;
    for (;;) {
      c = getc(stdin);
      if (c == EOF)
        break;
      if (c == '\n' || c == ' ' || c == '\t')
        continue;
      if (c == '#') {
        do { c = getc(stdin); } while (!(c == EOF || c == '\n'));
        continue;
      }
      ungetc(c,stdin);
      if (scanf("0x%x", &i) != 1 || !(i >= 0 && i < 0x100))
        exit(1);
      do { c = getc(stdin); } while (c == ' ' || c == '\t');
      if (c != EOF)
        ungetc(c,stdin);
      if (c == '\n' || c == '#')
        continue;
      if (scanf("0x%x", &charset2uni[i]) != 1)
        exit(1);
    }
  }

  /* Write the output file. */
  {
    FILE* f;
    bool pages[0x100];
    int i, i1, i2, i3, j, j1, j2, p, p1, p2;

    {
      char* fname = malloc(strlen(directory)+strlen(filename)+1);
      strcpy(fname,directory); strcat(fname,filename);
      f = fopen(fname,"w");
      if (f == NULL)
        exit(1);
    }

    c_charsetname = strdup(charsetname);
    for (i = 0; i < strlen(c_charsetname); i++)
      if (c_charsetname[i] == '-')
        c_charsetname[i] = '_';

    fprintf(f, "/*\n");
    fprintf(f, " * nls_%s.c\n", charsetname);
    fprintf(f, " *\n");
    fprintf(f, " * Charset %s translation tables.\n", charsetname);
    fprintf(f, " * Generated automatically by the nls_table utility.\n");
    fprintf(f, " */\n");
    fprintf(f, "\n");
    fprintf(f, "#define charset2uni %s_charset2uni\n", c_charsetname);
    fprintf(f, "\n");
    fprintf(f, "static const unsigned short charset2uni[256] = {\n");
    for (i1 = 0; i1 < 16; i1++) {
      fprintf(f, "  /* 0x%02x */\n", 16*i1);
      for (i2 = 0; i2 < 2; i2++) {
        fprintf(f, "  ");
        for (i3 = 0; i3 < 8; i3++) {
          if (i3 > 0)
            fprintf(f, " ");
          i = 16*i1 + 8*i2 + i3;
          j = charset2uni[i];
          fprintf(f, "0x%04x%s", j, i<255?",":" ");
        }
        fprintf(f, "\n");
      }
    }
    fprintf(f, "};\n");
    fprintf(f, "\n");
    for (i = 0; i < 0x100; i++)
      pages[i] = false;
    for (i = 0; i < 0x100; i++)
      if (charset2uni[i] != 0xfffd)
        pages[charset2uni[i]>>8] = true;
    fprintf(f, "#define uni2charset %s_uni2charset\n", c_charsetname);
    for (p = 0; p < 0x100; p++)
      if (pages[p])
        fprintf(f, "#define page%02x %s_page%02x\n", p, c_charsetname, p);
    fprintf(f, "\n");
    for (p = 0; p < 0x100; p++)
      if (pages[p]) {
        fprintf(f, "static const unsigned char page%02x[256] = {\n", p);
        for (j1 = 0; j1 < 32; j1++) {
          fprintf(f, "  ");
          for (j2 = 0; j2 < 8; j2++) {
            j = 256*p + 8*j1 + j2;
            if (j != 0xfffd) {
              for (i = 0; i < 256; i++)
                if (charset2uni[i] == j)
                  break;
              if (i == 256)
                i = 0;
            } else
              i = 0;
            fprintf(f, "0x%02x%s ", i, 8*j1+j2<255?",":" ");
          }
          fprintf(f, "/* 0x%02x-0x%02x */\n", 8*j1, 8*j1+7);
        }
        fprintf(f, "};\n");
        fprintf(f, "\n");
      }
    fprintf(f, "static const unsigned char * const uni2charset[256] = {\n");
    for (p1 = 0; p1 < 32; p1++) {
      fprintf(f, "  ");
      for (p2 = 0; p2 < 8; p2++) {
        p = 8*p1 + p2;
        if (pages[p])
          fprintf(f, "page%02x", p);
        else
          fprintf(f, "nopage");
        fprintf(f, "%s ", p<255?",":" ");
      }
      fprintf(f, "\n");
    }
    fprintf(f, "};\n");
    fprintf(f, "\n");
    for (p = 0x100-1; p >= 0; p--)
      if (pages[p])
        fprintf(f, "#undef page%02x\n", p);
    fprintf(f, "\n");
    fprintf(f, "struct nls_table_t nls_%s_table = {\n", c_charsetname);
    fprintf(f, "  \"%s\",\n", charsetname);
    fprintf(f, "  uni2charset,\n");
    fprintf(f, "  charset2uni,\n");
    {
      bool is_ascii_extension = true;
      for (i = 0; i < 128; i++)
        if (charset2uni[i] != i) {
          is_ascii_extension = false;
          break;
        }
      fprintf(f, "  %s\n", is_ascii_extension?"1":"0");
    }
    fprintf(f, "};\n");
    fprintf(f, "\n");
    fprintf(f, "#undef uni2charset\n");
    fprintf(f, "#undef charset2uni\n");
    fprintf(f, "\n");

    if (ferror(f) || fclose(f))
      exit(1);
  }

  exit(0);
}
