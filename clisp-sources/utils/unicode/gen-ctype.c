/* Generate Unicode conforming character classification tables from a
   UnicodeData file.
   Copyright (C) 2000-2002 Free Software Foundation, Inc.
   Written by Bruno Haible <bruno@clisp.org>, 2000-2002.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  */

/* See also: gen-ctype.c in libunistring, gen-unicode-ctype.c in glibc.  */

/* Usage example:
     $ gen-ctype /usr/local/share/Unidata/UnicodeData.txt 3.1.0
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <time.h>

/* ========================================================================= */

/* Reading UnicodeData.txt.  */
/* See UnicodeData-3.1.0.html.  */

/* This structure represents one line in the UnicodeData.txt file.  */
struct unicode_attribute
{
  const char *name;           /* Character name */
  const char *category;       /* General category */
  const char *combining;      /* Canonical combining classes */
  const char *bidi;           /* Bidirectional category */
  const char *decomposition;  /* Character decomposition mapping */
  const char *decdigit;       /* Decimal digit value */
  const char *digit;          /* Digit value */
  const char *numeric;        /* Numeric value */
  bool mirrored;              /* mirrored */
  const char *oldname;        /* Old Unicode 1.0 name */
  const char *comment;        /* Comment */
  unsigned int upper;         /* Uppercase mapping */
  unsigned int lower;         /* Lowercase mapping */
  unsigned int title;         /* Titlecase mapping */
};

/* Missing fields are represented with "" for strings, and NONE for
   characters.  */
#define NONE (~(unsigned int)0)

/* The entire contents of the UnicodeData.txt file.  */
struct unicode_attribute unicode_attributes [0x110000];

/* Stores in unicode_attributes[i] the values from the given fields.  */
static void
fill_attribute (unsigned int i,
		const char *field1, const char *field2,
		const char *field3, const char *field4,
		const char *field5, const char *field6,
		const char *field7, const char *field8,
		const char *field9, const char *field10,
		const char *field11, const char *field12,
		const char *field13, const char *field14)
{
  struct unicode_attribute * uni;

  if (i >= 0x110000)
    {
      fprintf (stderr, "index too large\n");
      exit (1);
    }
  if (strcmp (field2, "Cs") == 0)
    /* Surrogates are UTF-16 artefacts, not real characters. Ignore them.  */
    return;
  uni = &unicode_attributes[i];
  /* Copy the strings.  */
  uni->name          = strdup (field1);
  uni->category      = (field2[0] == '\0' ? "" : strdup (field2));
  uni->combining     = (field3[0] == '\0' ? "" : strdup (field3));
  uni->bidi          = (field4[0] == '\0' ? "" : strdup (field4));
  uni->decomposition = (field5[0] == '\0' ? "" : strdup (field5));
  uni->decdigit      = (field6[0] == '\0' ? "" : strdup (field6));
  uni->digit         = (field7[0] == '\0' ? "" : strdup (field7));
  uni->numeric       = (field8[0] == '\0' ? "" : strdup (field8));
  uni->mirrored      = (field9[0] == 'Y');
  uni->oldname       = (field10[0] == '\0' ? "" : strdup (field10));
  uni->comment       = (field11[0] == '\0' ? "" : strdup (field11));
  uni->upper = (field12[0] =='\0' ? NONE : strtoul (field12, NULL, 16));
  uni->lower = (field13[0] =='\0' ? NONE : strtoul (field13, NULL, 16));
  uni->title = (field14[0] =='\0' ? NONE : strtoul (field14, NULL, 16));
}

/* Maximum length of a field in the UnicodeData.txt file.  */
#define FIELDLEN 120

/* Reads the next field from STREAM.  The buffer BUFFER has size FIELDLEN.
   Reads up to (but excluding) DELIM.
   Returns 1 when a field was successfully read, otherwise 0.  */
static int
getfield (FILE *stream, char *buffer, int delim)
{
  int count = 0;
  int c;

  for (; (c = getc (stream)), (c != EOF && c != delim); )
    {
      /* The original unicode.org UnicodeData.txt file happens to have
	 CR/LF line terminators.  Silently convert to LF.  */
      if (c == '\r')
	continue;

      /* Put c into the buffer.  */
      if (++count >= FIELDLEN - 1)
	{
	  fprintf (stderr, "field too long\n");
	  exit (1);
	}
      *buffer++ = c;
    }

  if (c == EOF)
    return 0;

  *buffer = '\0';
  return 1;
}

/* Stores in unicode_attributes[] the entire contents of the UnicodeData.txt
   file.  */
static void
fill_attributes (const char *unicodedata_filename)
{
  unsigned int i, j;
  FILE *stream;
  char field0[FIELDLEN];
  char field1[FIELDLEN];
  char field2[FIELDLEN];
  char field3[FIELDLEN];
  char field4[FIELDLEN];
  char field5[FIELDLEN];
  char field6[FIELDLEN];
  char field7[FIELDLEN];
  char field8[FIELDLEN];
  char field9[FIELDLEN];
  char field10[FIELDLEN];
  char field11[FIELDLEN];
  char field12[FIELDLEN];
  char field13[FIELDLEN];
  char field14[FIELDLEN];
  int lineno = 0;

  for (i = 0; i < 0x110000; i++)
    unicode_attributes[i].name = NULL;

  stream = fopen (unicodedata_filename, "r");
  if (stream == NULL)
    {
      fprintf (stderr, "error during fopen of '%s'\n", unicodedata_filename);
      exit (1);
    }

  for (;;)
    {
      int n;

      lineno++;
      n = getfield (stream, field0, ';');
      n += getfield (stream, field1, ';');
      n += getfield (stream, field2, ';');
      n += getfield (stream, field3, ';');
      n += getfield (stream, field4, ';');
      n += getfield (stream, field5, ';');
      n += getfield (stream, field6, ';');
      n += getfield (stream, field7, ';');
      n += getfield (stream, field8, ';');
      n += getfield (stream, field9, ';');
      n += getfield (stream, field10, ';');
      n += getfield (stream, field11, ';');
      n += getfield (stream, field12, ';');
      n += getfield (stream, field13, ';');
      n += getfield (stream, field14, '\n');
      if (n == 0)
	break;
      if (n != 15)
	{
	  fprintf (stderr, "short line in'%s':%d\n",
		   unicodedata_filename, lineno);
	  exit (1);
	}
      i = strtoul (field0, NULL, 16);
      if (field1[0] == '<'
	  && strlen (field1) >= 9
	  && !strcmp (field1 + strlen(field1) - 8, ", First>"))
	{
	  /* Deal with a range. */
	  lineno++;
	  n = getfield (stream, field0, ';');
	  n += getfield (stream, field1, ';');
	  n += getfield (stream, field2, ';');
	  n += getfield (stream, field3, ';');
	  n += getfield (stream, field4, ';');
	  n += getfield (stream, field5, ';');
	  n += getfield (stream, field6, ';');
	  n += getfield (stream, field7, ';');
	  n += getfield (stream, field8, ';');
	  n += getfield (stream, field9, ';');
	  n += getfield (stream, field10, ';');
	  n += getfield (stream, field11, ';');
	  n += getfield (stream, field12, ';');
	  n += getfield (stream, field13, ';');
	  n += getfield (stream, field14, '\n');
	  if (n != 15)
	    {
	      fprintf (stderr, "missing end range in '%s':%d\n",
		       unicodedata_filename, lineno);
	      exit (1);
	    }
	  if (!(field1[0] == '<'
		&& strlen (field1) >= 8
		&& !strcmp (field1 + strlen (field1) - 7, ", Last>")))
	    {
	      fprintf (stderr, "missing end range in '%s':%d\n",
		       unicodedata_filename, lineno);
	      exit (1);
	    }
	  field1[strlen (field1) - 7] = '\0';
	  j = strtoul (field0, NULL, 16);
	  for (; i <= j; i++)
	    fill_attribute (i, field1+1, field2, field3, field4, field5,
			       field6, field7, field8, field9, field10,
			       field11, field12, field13, field14);
	}
      else
	{
	  /* Single character line */
	  fill_attribute (i, field1, field2, field3, field4, field5,
			     field6, field7, field8, field9, field10,
			     field11, field12, field13, field14);
	}
    }
  if (ferror (stream) || fclose (stream))
    {
      fprintf (stderr, "error reading from '%s'\n", unicodedata_filename);
      exit (1);
    }
}

/* ========================================================================= */

/* Character mappings.  */

static unsigned int
to_upper (unsigned int ch)
{
  if (unicode_attributes[ch].name != NULL
      && unicode_attributes[ch].upper != NONE)
    return unicode_attributes[ch].upper;
  else
    return ch;
}

static unsigned int
to_lower (unsigned int ch)
{
  if (unicode_attributes[ch].name != NULL
      && unicode_attributes[ch].lower != NONE)
    return unicode_attributes[ch].lower;
  else
    return ch;
}

/* Common Lisp only wants the bijective lower/upper case conversions.  */
static bool
is_CL_both_case (unsigned int ch)
{
  unsigned int ch1 = to_upper (ch);
  unsigned int ch2 = to_lower (ch);
  return (ch1 == ch || ch2 == ch)
         && (ch1 != ch2)
         && to_upper (ch1) == ch1
         && to_upper (ch2) == ch1
         && to_lower (ch1) == ch2
         && to_lower (ch2) == ch2;
}

static void
check_bothcase ()
{
  unsigned int ch;
  for (ch = 0; ch < 0x110000; ch++)
    {
      unsigned int ch1 = to_upper (ch);
      unsigned int ch2 = to_lower (ch);
      if (!(ch1 == ch || ch2 == ch))
        printf ("character 0x%04x is neither upper nor lower case\n", ch);
      else if (is_CL_both_case (ch))
        {
          if (ch == ch1)
            {
              // ch is upper case
              if (to_lower (ch2) != ch2)
                printf ("character 0x%04x has a lower case mapping 0x%04x which is not lower case\n", ch, ch2);
              else if (to_upper (ch2) != ch)
                printf ("character 0x%04x has a lower case mapping 0x%04x whose upper case mapping isn't the first character\n", ch, ch2);
            }
          else if (ch == ch2)
            {
              // ch is lower case
              if (to_upper (ch1) != ch1)
                printf ("character 0x%04x has an upper case mapping 0x%04x which is not upper case\n", ch, ch1);
              else if (to_lower (ch1) != ch)
                printf ("character 0x%04x has an upper case mapping 0x%04x whose lower case mapping isn't the first character\n", ch, ch1);
            }
        }
    }
}

static unsigned int
CL_to_upper (unsigned int ch)
{
  return (is_CL_both_case (ch) ? to_upper (ch) : ch);
}

static unsigned int
CL_to_lower (unsigned int ch)
{
  return (is_CL_both_case (ch) ? to_lower (ch) : ch);
}

static void
output_casemapping (const char *filename,
                    const char *mapname, unsigned int (*map) (unsigned int),
                    const char *version)
{
  FILE *stream;
  bool pages[0x1100];
  unsigned int max_nonempty_page;

  stream = fopen (filename, "w");
  if (stream == NULL)
    {
      fprintf (stderr, "cannot open '%s' for writing\n", filename);
      exit (1);
    }

  fprintf (stream, "/*\n");
  fprintf (stream, " * %s\n", filename);
  fprintf (stream, " *\n");
  fprintf (stream, " * Common Lisp %scase table.\n", mapname);
  fprintf (stream, " * Generated automatically by the gen-ctype utility for Unicode %s.\n", version);
  fprintf (stream, " */\n");
  fprintf (stream, "\n");
  {
    unsigned int p;
    for (p = 0; p < 0x1100; p++)
      pages[p] = false;
    max_nonempty_page = 0;
  }
  {
    unsigned int p, i1;
    for (p = 0; p < 0x1100; p++)
      for (i1 = 0; i1 < 0x100; i1++)
        {
          unsigned int ch = (p << 8) + i1;
          if (map (ch) != ch) {
            pages[p] = true;
            max_nonempty_page = p;
            break;
        }
      }
  }
  {
    unsigned int p, i1, i2;
    for (p = 0; p < 0x1100; p++)
      if (pages[p])
        {
          fprintf (stream, "static const uint16 %s_case_table_page%02x[256] = {\n", mapname, p);
          for (i1 = 0; i1 < 32; i1++)
            {
              fprintf (stream, "  ");
              for (i2 = 0; i2 < 8; i2++)
                {
                  unsigned int ch = 256*p + 8*i1 + i2;
                  unsigned int ch2 = map (ch);
                  int j = ch2 - ch;
                  if (j > 0x7fff || j < -0x7fff)
                    {
                      fprintf (stderr, "%scase(0x%04x) differs from 0x%04x by more than 15 bits\n", mapname, ch, ch);
                      exit (1);
                    }
                  fprintf (stream, "0x%04x%s ", j & 0xffffU, (8*i1+i2<255 ? "," : " "));
                }
              fprintf (stream, "/* 0x%02x-0x%02x */\n", 8*i1, 8*i1+7);
            }
          fprintf (stream, "};\n");
          fprintf (stream, "\n");
        }
  }
  {
    unsigned int p;
    fprintf (stream, "static const uint16 * const %s_case_table[%d] = {\n", mapname, max_nonempty_page+1);
    for (p = 0; p <= max_nonempty_page; p++)
      {
        if ((p % 4) == 0)
          fprintf (stream, "  ");
        if (pages[p])
          fprintf (stream, "%s_case_table_page%02x", mapname, p);
        else
          fprintf (stream, "nop_page");
        fprintf (stream, "%s ", (p<max_nonempty_page ? "," : " "));
        if ((p % 4) == 3 || p == max_nonempty_page)
          fprintf (stream, "/* 0x%02x-0x%02x */\n", 4*(p/4), p);
      }
    fprintf (stream, "};\n");
    fprintf (stream, "\n");
  }

  if (ferror (stream) || fclose (stream))
    {
      fprintf (stderr, "error writing to '%s'\n", filename);
      exit (1);
    }
}

/* ========================================================================= */

/* Character class properties.  */

static bool
is_graphic (unsigned int ch)
{
  return (unicode_attributes[ch].name != NULL
	  && strcmp (unicode_attributes[ch].name, "<control>"));
}

static bool
is_alpha (unsigned int ch)
{
  return (unicode_attributes[ch].name != NULL
	  && ((unicode_attributes[ch].category[0] == 'L'
	       /* Theppitak Karoonboonyanan <thep@links.nectec.or.th> says
		  <U0E2F>, <U0E46> should belong to is_punct.  */
	       && (ch != 0x0E2F) && (ch != 0x0E46))
	      /* Theppitak Karoonboonyanan <thep@links.nectec.or.th> says
		 <U0E31>, <U0E34>..<U0E3A>, <U0E47>..<U0E4E> are is_alpha.  */
	      || (ch == 0x0E31)
	      || (ch >= 0x0E34 && ch <= 0x0E3A)
	      || (ch >= 0x0E47 && ch <= 0x0E4E)
#if 0
	      /* Avoid warning for <U0345>.  */
	      || (ch == 0x0345)
	      /* Avoid warnings for <U2160>..<U217F>.  */
	      || (unicode_attributes[ch].category[0] == 'N'
		  && unicode_attributes[ch].category[1] == 'l')
	      /* Avoid warnings for <U24B6>..<U24E9>.  */
	      || (unicode_attributes[ch].category[0] == 'S'
		  && unicode_attributes[ch].category[1] == 'o'
		  && strstr (unicode_attributes[ch].name, " LETTER ")
		     != NULL)
#endif
         )   );
}

static bool
is_numeric (unsigned int ch)
{
  return (unicode_attributes[ch].name != NULL
	  && unicode_attributes[ch].category[0] == 'N'
	  && unicode_attributes[ch].category[1] == 'd');
}

static void
output_attributes (const char *filename, const char *version)
{
  FILE *stream;
  enum
    {
      NON_GRAPHIC = 0,
      GRAPHIC_NON_ALPHANUMERIC = 1,
      NUMERIC = 2,
      ALPHABETIC = 3
    };
  unsigned char attribute[0x110000];
  unsigned char attribute_packed[0x44000];
  unsigned int pages[0x440];
  unsigned int max_nonempty_page;

  /* Compute the attribute table.  */
  {
    unsigned int ch;
    for (ch = 0; ch < 0x110000; ch++)
      {
        bool graphic = is_graphic (ch);
        bool alpha = is_alpha (ch);
        bool numeric = is_numeric (ch);
        if (alpha && numeric)
          fprintf (stderr, "Character 0x%04x is both alpha and numeric\n", ch);
        if (alpha && !graphic)
          fprintf (stderr, "Character 0x%04x is alpha but not graphic\n", ch);
        if (numeric && !graphic)
          fprintf (stderr, "Character 0x%04x is numeric but not graphic\n", ch);
        attribute[ch] = (alpha ? ALPHABETIC :
                         numeric ? NUMERIC :
                         graphic ? GRAPHIC_NON_ALPHANUMERIC :
                         NON_GRAPHIC
                        );
      }
  }

  stream = fopen (filename, "w");
  if (stream == NULL)
    {
      fprintf (stderr, "cannot open '%s' for writing\n", filename);
      exit (1);
    }

  fprintf (stream, "/*\n");
  fprintf (stream, " * %s\n", filename);
  fprintf (stream, " *\n");
  fprintf (stream, " * Common Lisp character attribute table.\n");
  fprintf (stream, " * Generated automatically by the gen-ctype utility for Unicode %s.\n", version);
  fprintf (stream, " */\n");
  fprintf (stream, "\n");

  /* Pack the table, 4 entries into each byte.  */
  {
    unsigned int i1, i2;
    for (i1 = 0; i1 < 0x44000; i1++)
      {
        unsigned char b = 0;
        for (i2 = 0; i2 < 4; i2++)
          b |= attribute[4*i1+i2] << (2*i2);
        attribute_packed[i1] = b;
      }
  }

  /* Remove duplicate pages, to save space.  */
  {
    unsigned int i, j;
    for (i = 0; i < 0x440; i++)
      {
        for (j = 0; j < i; j++)
          if (memcmp (&attribute_packed[j<<8], &attribute_packed[i<<8], 1<<8) == 0)
            break;
        pages[i] = j;
      }
  }

  max_nonempty_page = 0;
  {
    unsigned int i1, i2;
    for (i1 = 0; i1 < 0x440; i1++)
      for (i2 = 0; i2 < 0x100; i2++)
        if (attribute_packed[(i1<<8)+i2] != 0)
          {
            max_nonempty_page = i1;
            break;
          }
  }

  {
    unsigned int i1, i2a, i2b;
    for (i1 = 0; i1 < 0x440; i1++)
      if (pages[i1] == i1)
        {
          fprintf (stream, "static const uintB unicode_attribute_table_page%02x[256] = {\n", 4*i1);
          for (i2a = 0; i2a < 32; i2a++)
            {
              fprintf (stream, "  ");
              for (i2b = 0; i2b < 8; i2b++)
                {
                  unsigned int i2 = 8*i2a + i2b;
                  fprintf (stream, "0x%02x%s ", attribute_packed[(i1<<8)+i2], (i2<255 ? "," : " "));
                }
              fprintf (stream, "/* 0x%04x-0x%04x */\n", 4*((i1<<8)+(8*i2a)), 4*((i1<<8)+(8*i2a+7))+3);
            }
          fprintf (stream, "};\n");
          fprintf (stream, "\n");
        }
  }
  {
    unsigned int i1;
    fprintf (stream, "const uintB * const unicode_attribute_table[%d] = {\n", max_nonempty_page+1);
    for (i1 = 0; i1 <= max_nonempty_page; i1++)
      {
        if ((i1 % 2) == 0)
          fprintf (stream, " ");
        fprintf (stream, " unicode_attribute_table_page%02x%s", 4*pages[i1], (i1<max_nonempty_page ? "," : ""));
        if ((i1 % 2) == 1 || i1 == max_nonempty_page)
          fprintf (stream, "\n");
      }
    fprintf (stream, "};\n");
    fprintf (stream, "\n");
  }

  if (ferror (stream) || fclose (stream))
    {
      fprintf (stderr, "error writing to '%s'\n", filename);
      exit (1);
    }
}

/* ========================================================================= */

int
main (int argc, char * argv[])
{
  const char *unicodedata_filename;
  const char *version;

  if (argc != 3)
    {
      fprintf (stderr, "Usage: %s UnicodeData.txt version\n",
	       argv[0]);
      exit (1);
    }

  unicodedata_filename = argv[1];
  version = argv[2];

  fill_attributes (unicodedata_filename);

  output_casemapping ("uni_upcase.c", "up", CL_to_upper, version);
  output_casemapping ("uni_downcase.c", "down", CL_to_lower, version);
  output_attributes ("uni_attribute.c", version);

  return 0;
}
