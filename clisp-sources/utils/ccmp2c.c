/*
 * CCMP2C - C code macro preprocessor, generating C
 * Bruno Haible 22.6.1997
 */

/*
 This preprocessor takes a text file containing macros of the form
   ##define MACRONAME(macroarg,...)  expansion
 and generates some C code which prints the expansion.
 Example input:
   ##define DEFACCESSOR(clas,name)   \
     DEFREADER(clas,name)            \
     DEFWRITER(clas,name)
   ##define DEFREADER(clas,name)  \
     LISPFUNN(clas##_##name,2)    \
     { value1 = *get_slot_##clas##_##name(popSTACK()); }
   ##define DEFWRITER(clas,name)         \
     LISPFUNN(clas##_##name##_setter,3)  \
     { var object newvalue = popSTACK(); \
       value1 = *get_slot_##clas##_##name(popSTACK()) = newvalue1; }
   DEFACCESSOR(ship,x)
 Example output (modulo #line statements): something like that:
   static void emit_DEFACCESSOR(TEXT *clas, TEXT *name);
   static void emit_DEFREADER(TEXT *clas, TEXT *name);
   static void emit_DEFWRITER(TEXT *clas, TEXT *name);
   static void emit_DEFACCESSOR(TEXT *clas, TEXT *name) {
     emit("  "); emit_DEFREADER(clas,name); emit("           \n");
     emit("  "); emit_DEFWRITER(clas,name);
   }
   static void emit_DEFREADER(TEXT *clas, TEXT *name) {
     emit("  LISPFUNN("); emit(clas); emit("_"); emit(name); emit(",2) \n");
     emit("  { value1 = *get_slot_"); emit(clas); emit("_"); emit(name);
     emit("(popSTACK()); }");
   }
   static void emit_DEFWRITER(TEXT *clas, TEXT *name) {
     emit("  LISPFUNN("); emit(clas); emit("_"); emit(name);
     emit("_setter,3) \n");
     emit("  { var object newvalue = popSTACK(); \n");
     emit("    value1 = *get_slot_"); emit(clas); emit("_"); emit(name);
     emit("(popSTACK()) = newvalue1; }");
   }
   emit_DEFACCESSOR("ship","x"); emit("\n");
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* Boolean type.  */
/* Not a typedef because AIX <sys/types.h> already defines boolean_t.  */
#define boolean_t int
#define FALSE 0
#define TRUE 1

/* Maximum number of macro arguments.  */
#define MAXARGCOUNT        50
/* Define as 1 if C++ style comments "//" shall be understood.  */
#define CPLUSPLUS_COMMENTS 1
/* Define as 1 if comment5 style comments "# " shall be understood.  */
#define COMMENT5_COMMENTS  1
/* Functions are split after this many statements.  This helps reducing the
   compilation time of the generated code.  */
#define MAXFUNCLENGTH  20


/* ================================ Strings ================================ */

/* The type 'string_t' represents a simple string.  */
typedef struct
{
  unsigned long length;
  unsigned char *data;
} string_t;

/* Compare two strings, returning TRUE when they are equal (not like
   strcmp!).  */
static boolean_t
string_compare (const string_t *s1, const string_t *s2)
{
  if (s1->length == s2->length)
    {
      unsigned long count = s1->length;
      unsigned char *p1 = s1->data;
      unsigned char *p2 = s2->data;
      while (count > 0)
        {
          if (*p1 != *p2)
            return FALSE;
          p1++; p2++; count--;
        }
      return TRUE;
    }
  else
    return FALSE;
}


/* =========================== Lists of strings =========================== */

/* The type 'string_list_t' represents a list of strings.  */
typedef struct
{
  unsigned long index;    /* Number of used strings in data[] */
  unsigned long size;     /* Number of allocated strings in data[] */
  string_t *data;
} string_list_t;

/* Initialize a list of strings.  */
static void
stringlist_init (string_list_t *l)
{
  l->data = (string_t *) malloc ((l->size = 1) * sizeof (string_t));
  if (!l->data)
    {
      fprintf (stderr, "Out of memory.\n");
      exit (1);
    }
  l->index = 0;
}

/* Add a given string to the end of a list of strings.  */
static void
stringlist_add (string_list_t *l, const string_t *s)
{
  if (l->index >= l->size)
    {
      l->data =
        (string_t *)
        realloc (l->data, (l->size = 2 * l->size + 1) * sizeof (string_t));
      if (!l->data)
        {
          fprintf (stderr, "Out of memory.\n");
          exit (1);
        }
    }
  l->data[l->index++] = *s;
}

/* Return TRUE if a given string is contained in a given list of strings.  */
static boolean_t
stringlist_lookup (string_list_t *l, string_t *s)
{
  unsigned long i;

  for (i = l->index; i > 0; )
    {
      i--;
      if (string_compare (s, &l->data[i]))
        return TRUE;
    }
  return FALSE;
}


/* ============================= Reading lines ============================= */

/* Line number of the last line returned by get_line(). */
static unsigned long line_number;

/* Read a line from fp, terminated with '\n', or
   NULL if EOF is encountered.  */
static unsigned char *
get_line (FILE *fp)
{
  int len = 1;
  unsigned char *line = (unsigned char *) malloc (len);
  int index = 0;

  for (;;)
    {
      /* Read a character.  */
      int c = fgetc (fp);
      if (c == EOF)
        {
          free (line);
          return NULL;
        }

      /* Add it to the line.  */
      if (index >= len)
        {
          len = 2 * len;
          line = (unsigned char *) realloc (line, len);
        }
      if (!line)
        {
          fprintf (stderr, "Out of memory.\n");
          exit (1);
        }
      line[index++] = c;

      /* Stop when we've read a newline.  */
      if (c == '\n')
        break;
    }
  line_number++;
  return line;
}


/* ============================= Input parsing ============================= */

/* Return TRUE if the given line contains a ##if/##else/##endif directive.  */
static boolean_t
control_line_p (const unsigned char *line)
{
  return (line[0]=='#' && line[1]=='#'
          && ((   line[2]=='i'
               && line[3]=='f'
               && ((line[4]==' ' || line[4]=='\t')
                   || (   line[4]=='d'
                       && line[5]=='e'
                       && line[6]=='f'
                       && (line[7]==' ' || line[7]=='\t'))
                   || (   line[4]=='n'
                       && line[5]=='d'
                       && line[6]=='e'
                       && line[7]=='f'
                       && (line[8]==' ' || line[8]=='\t'))))
              || (   line[2]=='e'
                  && line[3]=='l'
                  && ((   line[4]=='s'
                       && line[5]=='e'
                       && (line[6]==' ' || line[6]=='\t' || line[6]=='\n'))
                      || (   line[4]=='i'
                          && line[5]=='f'
                          && (line[6]==' ' || line[6]=='\t'))))
              || (   line[2]=='e'
                  && line[3]=='n'
                  && line[4]=='d'
                  && line[5]=='i'
                  && line[6]=='f'
                  && (line[7]==' ' || line[7]=='\t' || line[7]=='\n'))));
}

/* Return TRUE if the given line contains a macro definition.  */
static boolean_t
define_line_p (const unsigned char *line)
{
  return (line[0]=='#' && line[1]=='#'
          && line[2]=='d'
          && line[3]=='e'
          && line[4]=='f'
          && line[5]=='i'
          && line[6]=='n'
          && line[7]=='e'
          && (line[8]==' ' || line[8]=='\t'));
}

/* Read the continuation lines of a macro definition line.
   Return a string containing the entire macro definition (with
   backslash-newline replaced with newline).
   firstline is free()d here, and *result is filled with the whole definition
   string.  */
static void
whole_definition (unsigned char *firstline, FILE *infile, string_t *result)
{
  unsigned long size = 1;      /* Bytes allocated for whole.  */
  unsigned char *whole = (unsigned char *) malloc (size);
  unsigned long len = 0;       /* Length of whole so far.  */

  unsigned char *line = firstline;

  /* Check against memory allocation failure.  */
  if (!whole)
    {
      fprintf (stderr, "Out of memory.\n");
      exit (1);
    }

  for (;;)
    {
      unsigned long linelen;
      boolean_t backslashed;

      /* Count number of bytes of the next line.  */
      linelen = 0;
      while (line[linelen] != '\n')
        linelen++;
      /* Does it have a backslash at the end?  */
      backslashed = (linelen > 0 && line[linelen-1]=='\\');
      /* If it has a backslash at the end, we drop it.  */
      if (backslashed)
        linelen--;

      /* Append line to whole.  */
      while (!(size >= len + 1 + linelen))
        {
          whole = (unsigned char *) realloc (whole, (size = 2 * size + 1));
          if (!whole)
            {
              fprintf (stderr, "Out of memory.\n");
              exit (1);
            }
        }
      {
        unsigned char *ptr = &whole[len];
        if (len > 0)
          {
            *ptr++ = '\n';
            len++;
          }
        {
          unsigned long i;

          for (i = 0; i < linelen; i++)
            ptr[i] = line[i];
        }
        len += linelen;
      }

      /* Free the line, now that we have digested it.  */
      free (line);

      /* A line without backslash at the end terminates the definition.  */
      if (!backslashed)
        break;

      /* Otherwise continue, reading the next line.  */
      line = get_line (infile);
      if (!line)
        {
          fprintf (stderr, "Encountered EOF during macro definition.\n");
          exit (1);
        }
    }

  /* That's it.  Return the result.  */
  result->data = whole; result->length = len;
}


/* ======================= Tokenisation of a buffer ======================= */

/* A FILE-like buffer where one can read from.  */
typedef struct
{
  unsigned char *buf;
  unsigned long position;
  unsigned long endindex;
} BUFFILE_t;

/* Return the next 'unsigned char' from the given BUFFILE_t, or EOF.  */
static int
next_char (BUFFILE_t *infile)
{
  if (infile->position == infile->endindex)
    return EOF;
  else
    return infile->buf[infile->position++];
}

/* Return the next 'unsigned char' from the given BUFFILE_t, without consuming
   it, or EOF.  */
static int
peek_char (BUFFILE_t *infile)
{
  if (infile->position == infile->endindex)
    return EOF;
  else
    return infile->buf[infile->position];
}

/* These are the different token types.  */
enum token_type_t
{
  eof,           /* End of file */
  eofcomment,    /* End of file inside comment */
  eol,           /* End of line */
  ident,         /* Identifier */
  number,        /* Number */
  charconst,     /* Character literal */
  stringconst,   /* String literal */
  sep            /* Separator */
};

/* A token, as seen in a buffer.  */
typedef struct
{
  enum token_type_t type;
  unsigned long startindex;    /* Start index in buffer */
  unsigned long endindex;      /* End index in buffer */
  unsigned char ch;            /* Detail about operator/separator */
} token_t;

/* A list of tokens.  */
typedef struct
{
  unsigned long index;    /* Number of used tokens in data[] */
  unsigned long size;     /* Number of allocated tokens in data[] */
  token_t *data;
} tokens_t;

/* Initialize a list of tokens.  */
static void
tokens_init (tokens_t *tokens)
{
  tokens->index = 0;
  tokens->data = (token_t *) malloc ((tokens->size = 1000) * sizeof (token_t));
}

/* Destroy a list of tokens.  */
static void tokens_del (tokens_t *tokens)
{
  if (tokens->size > 0)
    free (tokens->data);
}

/* Fetch the next token from the given BUFFILE_t, and append it to the given
   list of tokens.
   We need within_prep_directive because inside preprocessor directives,
   newline counts as a separate token, and '#' doesn't introduce a nested
   preprocessor directive.  */
static token_t *
nexttoken (BUFFILE_t *infile, tokens_t *tokens,
           boolean_t within_comment, boolean_t within_prep_directive)
{
  /* Ensure room for the next token.  */
  if (tokens->index >= tokens->size)
    {
      tokens->data =
        (tokens->size == 0
         ? (token_t *) malloc ((tokens->size = 1) * sizeof (token_t))
         : (token_t *)
           realloc (tokens->data,
                    (tokens->size = 2*tokens->size + 1) * sizeof (token_t)));
      if (!tokens->data)
        {
          fprintf (stderr, "Out of memory.\n");
          exit (1);
        }
    }
  {
    token_t *token = &tokens->data[tokens->index];
    int c;

    if (within_comment)
      goto in_comment;

    for (;;)
      {
        token = &tokens->data[tokens->index];
        token->startindex = infile->position;
        c = next_char (infile);
        switch (c)
          {
          case EOF: /* EOF */
            token->type = eof;
            goto done;
          case ' ': case '\v': case '\t':
            continue;
          case '\n': /* EOL */
            if (within_prep_directive)
              {
                token->type = eol;
                goto done;
              }
            else
              continue;
          case '\\':
            if (peek_char (infile) == '\n')
              {
                next_char (infile);
                continue;
              }
            else
              goto separator;
          case '/':
            if (peek_char (infile) == '*')
              {
                next_char (infile);
              in_comment:
                for (;;)
                  {
                    c = next_char (infile);
                    if (c==EOF)
                      {
                        token->type = eofcomment;
                        goto done;
                      }
                    if (c == '*' && peek_char (infile) == '/')
                      {
                        next_char (infile);
                        break;
                      }
                  }
                continue;
              }
            else
#if CPLUSPLUS_COMMENTS
              if (peek_char (infile) == '/')
                {
                  next_char (infile);
                  do
                    c = next_char (infile);
                  while (c != EOF && c != '\n');
                  continue;
                }
              else
#endif
                goto separator;
          case '*':
            if (peek_char (infile) == '/')
              {
                fflush (stdout);
                fprintf (stderr, "end of comment without corresponding start of comment\n");
              }
            goto separator;
          case '#':
            if (within_prep_directive)
              goto separator;
            else
#if COMMENT5_COMMENTS
              if (peek_char (infile) == ' ')
                {
                  next_char (infile);
                  do
                    c = next_char (infile);
                  while (c != EOF && c != '\n');
                  continue;
                }
              else
#endif
                {
                  for (;;) {
                    token_t *subtoken = nexttoken (infile, tokens, FALSE, TRUE);
                    if (subtoken->type == eof
                        || subtoken->type == eofcomment
                        || subtoken->type == eol)
                      break;
                  }
                  continue;
                }
          case '.':
            c = peek_char (infile);
            if (!((c >= '0' && c <= '9') || c == '.'))
              goto separator;
            /* FALLTHROUGH */
          case '0': case '1': case '2': case '3': case '4':
          case '5': case '6': case '7': case '8': case '9':
            for (;;)
              {
                c = peek_char (infile);
                if ((c >= '0' && c <= '9')
                    || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
                    || c == '.')
                  next_char (infile);
                else
                  break;
              }
            token->type = number;
            goto done;
          case '\'':
            /* Character constant.  */
            for (;;)
              {
                c = next_char (infile);
                if (c == EOF)
                  {
                    fflush (stdout);
                    fprintf (stderr, "unterminated character constant\n");
                    break;
                  }
                if (c == '\'')
                  break;
                if (c == '\\')
                  c = next_char (infile);
              }
            token->type = charconst;
            goto done;
          case '\"':
            /* String constant.  */
            for (;;)
              {
                c = next_char (infile);
                if (c == EOF)
                  {
                    fflush (stdout);
                    fprintf (stderr, "unterminated string constant\n");
                    break;
                  }
                if (c == '\"')
                  break;
                if (c == '\\')
                  c = next_char (infile);
              }
            token->type = stringconst;
            goto done;
          case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
          case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
          case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
          case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
          case 'Y': case 'Z':
          case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
          case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
          case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
          case 's': case 't': case 'u': case 'v': case 'w': case 'x':
          case 'y': case 'z':
          case '_':
            /* Identifier.  */
            for (;;)
              {
                c = peek_char (infile);
                if (   (c >= '0' && c <= '9')
                    || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
                    || c == '_')
                  next_char (infile);
                else
                  break;
              }
            token->type = ident;
            goto done;
          default:
          separator:
            token->type = sep; token->ch = c;
            goto done;
          }
      done:
        token->endindex = infile->position;
        tokens->index++;
        return token;
      }
  }
}

/* Tokenify a buffer. Return the list of tokens.  */
static void
tokenify (unsigned char *line,
          unsigned long startindex, unsigned long endindex,
          boolean_t *within_comment, boolean_t within_prep_directive,
          tokens_t *tokens)
{
  BUFFILE_t infile;
  infile.buf = line; infile.position = startindex; infile.endindex = endindex;

  for (;;)
    {
      token_t *next =
        nexttoken (&infile, tokens, *within_comment, within_prep_directive);

      if (next->type == eof)
        {
          *within_comment = FALSE;
          break;
        }
      if (next->type == eofcomment)
        {
          *within_comment = TRUE;
          break;
        }
    }
  /* Remove the last token (eof or eofcomment) from the result.  */
  tokens->index--;
}

/* ============================ Code generation ============================ */

/* During output, we split the code into many small functions, so that it's
   easier to compile. (Some C compilers have problems compiling functions
   with a lot of code and no if/for/do/while control structure because this
   function is a single huge "basic block".) */

/* A counter: denotes number of the current/last function.  */
static int func_number = 0;

/* Number of statements in the current function so far.
   -1 when the function has been closed.  */
static int func_statements = -1;

/* The lines for main() that have been generated so far, including the
   control lines.  */
static string_list_t main_output;

/* Finish the current function.  */
static void
finish_func (void)
{
  if (func_statements >= 0)
    {
      printf ("}\n");
      func_statements = -1;
    }
}

/* Start a new statement.  Begin a new function if the current function
   is getting too big.  */
static void
new_statement (void)
{
  if (func_statements >= MAXFUNCLENGTH)
    /* Finish the current function.  */
    finish_func ();
  if (func_statements < 0)
    {
      /* Start a new function.  */
      func_number++; func_statements = 0;
      printf ("static void main%d (TEXT *curr)\n", func_number);
      printf ("{\n");
      {
        string_t main_line;
        main_line.data = (unsigned char *) malloc (30);
        if (!main_line.data)
          {
            fprintf (stderr, "Out of memory.\n");
            exit (1);
          }
        sprintf ((char *) main_line.data, "$  main%d (&curr);", func_number);
        main_line.length = strlen ((char *) main_line.data);
        stringlist_add (&main_output, &main_line);
      }
    }
  func_statements++;
}

/* Emit an indentation.  */
static void
emit_indent (unsigned long indent)
{
  while (indent > 0)
    {
      putchar (' ');
      indent--;
    }
}

/* Emit a string.  */
static void
emit_string (string_t *s)
{
  unsigned long i;

  for (i = 0; i < s->length; i++)
    putchar (s->data[i]);
}

/* Emit a string containing a control directive.
   (Skip the first character '#' and the newlines. Terminate with a newline.)
 */
static void
emit_control_directive (string_t *s) {
  unsigned long i;

  for (i = 1; i < s->length; i++)
    if (s->data[i] != '\n')
      putchar (s->data[i]);
  putchar ('\n');
}

/* Emit the code for generating a literal string.  */
static void
emit_literal (unsigned char *line,
              unsigned long startindex, unsigned long endindex,
              unsigned long lineno,
              const char *textvar, unsigned long indent)
{
  unsigned long i;
 restart:
  if (startindex == endindex)
    return;
  emit_indent (indent);
  printf ("TEXT_addstring (%s, %lu, \"", textvar, lineno);
  for (i = startindex; i < endindex; i++)
    {
      unsigned char ch = line[i];
      if (ch == '\\' || ch == '"')
        {
          putchar ('\\');
          putchar (ch);
        }
      else if (ch >= ' ' && ch != '\'')
        {
          putchar (ch);
        }
      else if (ch == '\n')
        {
          printf ("\\n\");\n");
          startindex = i + 1;
          if (lineno > 0)
            lineno++;
          goto restart;
        }
      else if (ch == '\t')
        {
          printf ("\\t");
        }
      else if (ch == '\b')
        {
          printf ("\\b");
        }
      else if (ch == '\r')
        {
          printf ("\\r");
        }
      else if (ch == '\f')
        {
          printf ("\\f");
        }
      else if (ch == '\v')
        {
          printf ("\\v");
        }
      else
        {
          putchar ('\\');
          putchar ('0' + ((ch / 64) % 8));
          putchar ('0' + ((ch / 8) % 8));
          putchar ('0' + (ch % 8));
        }
    }
  printf ("\");\n");
}


/* =========================== Expanding macros ============================ */

/* The list of globally defined macros.  */
static string_list_t macronames;

/* A counter for temporary variable names.  */
static int gensym_count = 0;

/* Emit the code for generating a certain string.  */
static void
emit_expansion (unsigned char *line,
                unsigned long startindex, unsigned long endindex,
                unsigned long lineno,
                boolean_t *within_comment, string_list_t *macroargs,
                const char *textvar, unsigned long indent)
{
  tokens_t tokens;

  tokens_init (&tokens);
  /* Tokenify the buffer.  */
  tokenify (line, startindex, endindex, within_comment, macroargs != NULL,
            &tokens);

  /* "#" and "##" eat their surrounding spaces.  */
  if (macroargs)
    {
      unsigned long tokindex;
      for (tokindex = 0; tokindex < tokens.index; tokindex++)
        {
          token_t *t = &tokens.data[tokindex];
          if (t->type == sep && t->ch == '#')
            {
              tokens.data[tokindex].startindex =
                (tokindex > 0 ? tokens.data[tokindex-1].endindex : startindex);
              tokens.data[tokindex].endindex =
                (tokindex < tokens.index-1
                 ? tokens.data[tokindex+1].startindex
                 : endindex);
            }
        }
    }

  /* Process the tokens. Only some ident tokens are active,
     and "#" tokens are skipped. */
  {
    unsigned long currindex;
    unsigned long tokindex;

    currindex = startindex;
    for (tokindex = 0; tokindex < tokens.index; tokindex++)
      {
        token_t *t = &tokens.data[tokindex];
        if (t->type == sep && t->ch == '#' && macroargs)
          {
            emit_literal (line, currindex, t->startindex, 0, textvar, indent);
            currindex = t->endindex;
          }
        else if (t->type == ident)
          {
            string_t id;
            id.length = t->endindex - t->startindex;
            id.data = &line[t->startindex];

            if (macroargs && stringlist_lookup (macroargs, &id))
              {
                /* Emit a reference to one of the macro arguments.  */
                emit_literal (line, currindex, t->startindex, 0,
                              textvar, indent);
                emit_indent (indent);
                printf ("TEXT_addtext (%s, ", textvar);
                printf ("arg_"); emit_string (&id);
                printf (");\n");
                currindex = t->endindex;
              }
            else if (stringlist_lookup (&macronames, &id)
                     && tokindex < tokens.index-1
                     && tokens.data[tokindex+1].type == sep
                     && tokens.data[tokindex+1].ch == '(')
              {
                /* Emit a macro call.  */
                unsigned long param_count = 0;
                struct param_t
                {
                  unsigned long startindex;
                  unsigned long endindex;
                  char temp[12];
                } params[MAXARGCOUNT];

                emit_literal (line, currindex, t->startindex, 0,
                              textvar, indent);
                tokindex += 2;
                if (tokindex >= tokens.index)
                  goto no_more_tokens;
                if (tokens.data[tokindex].type == sep
                    && tokens.data[tokindex].ch == ')')
                  {
                    /* Macro call without arguments. */
                    emit_indent (indent);
                    printf ("emit_"); emit_string (&id);
                    printf (" (%s);\n", textvar);
                  }
                else
                  { /* Macro call with arguments.  */
                    for (;;)
                      {
                        if (param_count == MAXARGCOUNT)
                          {
                            fprintf (stderr, "too many macro arguments\n");
                            exit (1);
                          }
                        gensym_count++;
                        sprintf (params[param_count].temp, "&temp%04d",
                                 gensym_count);
                        params[param_count].startindex =
                        params[param_count].endindex =
                          tokens.data[tokindex].startindex;
                        {
                          unsigned long paren_count = 0;

                          for (;;)
                            {
                              if (tokens.data[tokindex].type == sep)
                                {
                                  if (tokens.data[tokindex].ch == '(')
                                    {
                                      paren_count++;
                                    }
                                  else if (tokens.data[tokindex].ch == ')')
                                    {
                                      if (paren_count == 0)
                                        break;
                                      else
                                        paren_count--;
                                    }
                                  else if (tokens.data[tokindex].ch == ',')
                                    {
                                      if (paren_count == 0)
                                        break;
                                    }
                                }
                              params[param_count].endindex =
                                tokens.data[tokindex].endindex;
                              tokindex++;
                              if (tokindex >= tokens.index)
                                goto no_more_tokens;
                            }
                        }
                        param_count++;
                        if (tokens.data[tokindex].ch == ')')
                          break;
                        tokindex++;
                        if (tokindex >= tokens.index)
                          goto no_more_tokens;
                      }
                    if (FALSE)
                      {
                      no_more_tokens:
                        fprintf (stderr, "unterminated macro call\n");
                        exit (1);
                      }
                    {
                      unsigned long i;

                      for (i = 0; i < param_count; i++)
                        {
                          boolean_t dummy = FALSE;

                          emit_indent (indent);
                          printf ("{\n");
                          indent += 2;

                          emit_indent (indent);
                          printf ("TEXT %s;\n", &params[i].temp[1]);

                          emit_indent (indent);
                          printf ("TEXT_init (%s);\n", params[i].temp);

                          emit_expansion (line,
                                          params[i].startindex,
                                          params[i].endindex,
                                          0,
                                          &dummy, macroargs,
                                          params[i].temp, indent);
                        }
                      emit_indent (indent);
                      printf ("emit_"); emit_string (&id);
                      printf (" (%s", textvar);
                      for (i = 0; i < param_count; i++)
                        printf (", %s", params[i].temp);
                      printf (");\n");
                      for (i = param_count; i > 0; )
                        {
                          i--;
                          emit_indent (indent);
                          printf ("TEXT_del (%s);\n", params[i].temp);
                          indent -= 2;
                          emit_indent (indent); printf ("}\n");
                        }
                    }
                  }
                currindex = tokens.data[tokindex].endindex;
              }
          }
      }
    emit_literal (line, currindex, endindex,
                  currindex==0 ? lineno : 0,
                  textvar, indent);
  }
  tokens_del (&tokens);
}

int
main (int argc, char *argv[])
{
  char *infilename;
  FILE *infile;
  if (argc != 2)
    {
      fprintf (stderr, "Usage: %s inputfile\n", argv[0]);
      exit (1);
    }
  infilename = argv[1];

  /* Emit prologue. */
  printf ("#include <stdio.h>\n");
  printf ("#include <stdlib.h>\n");
  printf ("#include <string.h>\n");
  printf ("\n");
  printf ("int do_line = 0;\n");
  printf ("const char *infilename = \"%s\";\n", infilename);
  printf ("const char *outfilename = \"stdout\";\n");
  printf ("\n");
  printf ("typedef struct\n");
  printf ("{\n");
  printf ("  FILE *file;\n");
  printf ("  unsigned int file_lineno;\n");
  printf ("  unsigned int file_slineno;\n");
  printf ("  char *buf;\n");
  printf ("  int buflen;\n");
  printf ("  int len;\n");
  printf ("} TEXT;\n");
  printf ("static void TEXT_init1 (TEXT *text, FILE *file)\n");
  printf ("{\n");
  printf ("  text->file = file;\n");
  printf ("  text->file_lineno = 1;\n");
  printf ("  text->file_slineno = 0;\n");
  printf ("}\n");
  printf ("static void TEXT_init (TEXT *text)\n");
  printf ("{\n");
  printf ("  text->file = NULL;\n");
  printf ("  text->buf = (char *) malloc (text->buflen = 1);\n");
  printf ("  if (!text->buf)\n");
  printf ("    {\n");
  printf ("      fprintf (stderr, \"Out of memory.\\n\");\n");
  printf ("      exit (1);\n");
  printf ("    }\n");
  printf ("  text->buf[text->len = 0] = '\\0';\n");
  printf ("}\n");
  printf ("static void TEXT_del (TEXT *text)\n");
  printf ("{\n");
  printf ("  if (!text->file)\n");
  printf ("    free (text->buf);\n");
  printf ("}\n");
  printf ("static void TEXT_addstring (TEXT *text, unsigned int source_lineno, const char *s)\n");
  printf ("{\n");
  printf ("  if (text->file)\n");
  printf ("    {\n");
  printf ("      if (do_line)\n");
  printf ("        {\n");
  printf ("          if (source_lineno)\n");
  printf ("            {\n");
  printf ("              if (text->file_slineno != source_lineno)\n");
  printf ("                {\n");
  printf ("                  text->file_lineno++;\n");
  printf ("                  fprintf (text->file, \"#line %%u \\\"%%s\\\"\\n\", source_lineno, infilename);\n");
  printf ("                  text->file_slineno = source_lineno;\n");
  printf ("                }\n");
  printf ("            }\n");
  printf ("          else\n");
  printf ("            {\n");
  printf ("              if (text->file_slineno != 0)\n");
  printf ("                {\n");
  printf ("                  text->file_lineno++;\n");
  printf ("                  fprintf (text->file, \"#line %%u \\\"%%s\\\"\\n\", text->file_lineno, outfilename);\n");
  printf ("                  text->file_slineno = 0;\n");
  printf ("                }\n");
  printf ("            }\n");
  printf ("        }\n");
  printf ("      fprintf (text->file, \"%%s\", s);\n");
  printf ("      for (; *s; s++)\n");
  printf ("        if (*s == '\\n')\n");
  printf ("          {\n");
  printf ("            text->file_lineno++;\n");
  printf ("            if (text->file_slineno != 0)\n");
  printf ("              text->file_slineno++;\n");
  printf ("          }\n");
  printf ("    }\n");
  printf ("  else\n");
  printf ("    {\n");
  printf ("      int slen = strlen (s);\n");
  printf ("      int newlen = text->len + slen + 1;\n");
  printf ("      if (newlen > text->buflen)\n");
  printf ("        {\n");
  printf ("          text->buf = (char *) realloc (text->buf, text->buflen = 2 * newlen);\n");
  printf ("          if (!text->buf)\n");
  printf ("            {\n");
  printf ("              fprintf (stderr, \"Out of memory.\\n\");\n");
  printf ("              exit (1);\n");
  printf ("            }\n");
  printf ("        }\n");
  printf ("      strcpy (text->buf + text->len, s);\n");
  printf ("      text->len += slen;\n");
  printf ("    }\n");
  printf ("}\n");
  printf ("static void TEXT_addtext (TEXT *text, const TEXT *s)\n");
  printf ("{\n");
  printf ("  if (s->file)\n");
  printf ("    abort ();\n");
  printf ("  else\n");
  printf ("    TEXT_addstring (text, 0, s->buf);\n");
  printf ("}\n");

  /* Emit forward declarations.  */
  stringlist_init (&macronames);
  {
    if ((infile = fopen (infilename, "r")) == NULL)
      exit (1);
    line_number = 0;
    for (;;)
      {
        unsigned char *line = get_line (infile);
        if (!line)
          break;
        if (define_line_p (line))
          {
            string_t definition;
            boolean_t within_comment;
            tokens_t tokens;
            string_t id;
            unsigned long tokindex;
            string_list_t macroargs;

            whole_definition (line, infile, &definition);
            within_comment = FALSE;
            tokens_init (&tokens);
            tokenify (definition.data, 9, definition.length,
                      &within_comment, TRUE, &tokens);
            if (tokens.index == 0)
              {
                fprintf (stderr, "macro name missing\n");
                exit (1);
              }
            if (tokens.data[0].type != ident)
              {
                fprintf (stderr, "bad macro name\n");
                exit (1);
              }
            id.length = tokens.data[0].endindex - tokens.data[0].startindex;
            id.data = &definition.data[tokens.data[0].startindex];
            stringlist_add (&macronames, &id);
            stringlist_init (&macroargs);
            tokindex = 1;
            if (!(tokindex < tokens.index
                  && tokens.data[tokindex].type == sep
                  && tokens.data[tokindex].ch == '('))
              {
                fprintf (stderr, "macro parameter list missing\n");
                exit (1);
              }
            for (;;)
              {
                tokindex++;
                if (tokindex >= tokens.index)
                  {
                    fprintf (stderr, "unterminated macro parameter list\n");
                    exit (1);
                  }
                if (tokindex==2 && tokens.data[tokindex].type == sep
                    && tokens.data[tokindex].ch == ')')
                  break;
                if (tokens.data[tokindex].type != ident)
                  {
                    fprintf (stderr, "missing parameter name in macro parameter list\n");
                    exit (1);
                  }
                {
                  string_t pid;
                  pid.length =
                    tokens.data[tokindex].endindex
                    - tokens.data[tokindex].startindex;
                  pid.data =
                    &definition.data[tokens.data[tokindex].startindex];
                  stringlist_add (&macroargs, &pid);
                }
                tokindex++;
                if (tokindex >= tokens.index)
                  {
                    fprintf (stderr, "unterminated macro parameter list\n");
                    exit (1);
                  }
                if (tokens.data[tokindex].type == sep
                    && tokens.data[tokindex].ch == ')')
                  break;
                if (!(tokens.data[tokindex].type == sep
                      && tokens.data[tokindex].ch == ','))
                  {
                    fprintf (stderr, "syntax error in macro parameter list\n");
                    exit (1);
                  }
              }
            tokens_del (&tokens);
            printf ("static void emit_"); emit_string (&id);
            printf (" (TEXT *curr");
            {
              unsigned long i;
              for (i = 0; i < macroargs.index; i++)
                {
                  printf (", TEXT *arg_"); emit_string (&macroargs.data[i]);
                }
            }
            printf (");\n");
          }
        else if (control_line_p (line))
          {
            string_t definition;

            whole_definition (line, infile, &definition);
            emit_control_directive (&definition);
            free (definition.data);
          }
        else
          free (line);
      }
    fclose (infile);
  }

  /* Process file and emit most of the stuff.  */
  stringlist_init (&main_output);
  {
    boolean_t within_comment = FALSE;

    if ((infile = fopen (infilename, "r")) == NULL)
      exit (1);
    line_number = 0;
    for (;;)
      {
        unsigned char *line = get_line (infile);
        if (!line)
          break;
        if (define_line_p (line))
          {
            string_t definition;
            boolean_t dummy;
            tokens_t tokens;
            string_t id;
            unsigned long tokindex;
            string_list_t macroargs;
            unsigned long expansion_startindex;

            whole_definition (line, infile, &definition);
            dummy = FALSE;
            tokens_init (&tokens);
            tokenify (definition.data, 9, definition.length, &dummy, TRUE,
                      &tokens);
            if (tokens.index == 0)
              {
                fprintf (stderr, "macro name missing\n");
                exit (1);
              }
            if (tokens.data[0].type != ident)
              {
                fprintf (stderr, "bad macro name\n");
                exit (1);
              }
            id.length = tokens.data[0].endindex - tokens.data[0].startindex;
            id.data = &definition.data[tokens.data[0].startindex];
            stringlist_init (&macroargs);
            tokindex = 1;
            if (!(tokindex < tokens.index
                  && tokens.data[tokindex].type == sep
                  && tokens.data[tokindex].ch == '('))
              {
                fprintf (stderr, "macro parameter list missing\n");
                exit (1);
              }
            for (;;)
              {
                tokindex++;
                if (tokindex >= tokens.index)
                  {
                    fprintf (stderr, "unterminated macro parameter list\n");
                    exit (1);
                  }
                if (tokindex==2 && tokens.data[tokindex].type == sep
                    && tokens.data[tokindex].ch == ')')
                  break;
                if (tokens.data[tokindex].type != ident)
                  {
                    fprintf (stderr, "missing parameter name in macro parameter list\n");
                    exit (1);
                  }
                {
                  string_t pid;
                  pid.length =
                    tokens.data[tokindex].endindex
                    - tokens.data[tokindex].startindex;
                  pid.data =
                    &definition.data[tokens.data[tokindex].startindex];
                  stringlist_add (&macroargs, &pid);
                }
                tokindex++;
                if (tokindex >= tokens.index)
                  {
                    fprintf (stderr, "unterminated macro parameter list\n");
                    exit (1);
                  }
                if (tokens.data[tokindex].type == sep
                    && tokens.data[tokindex].ch == ')')
                  break;
                if (!(tokens.data[tokindex].type == sep
                      && tokens.data[tokindex].ch == ','))
                  {
                    fprintf (stderr, "syntax error in macro parameter list\n");
                    exit (1);
                  }
              }
            tokindex++;
            expansion_startindex =
              (tokindex < tokens.index
               ? tokens.data[tokindex].startindex
               : definition.length);
            tokens_del (&tokens);
            finish_func ();
            printf ("static void emit_"); emit_string (&id);
            printf (" (TEXT *curr");
            {
              unsigned long i;
              for (i = 0; i < macroargs.index; i++)
                {
                  printf (", TEXT *arg_"); emit_string (&macroargs.data[i]);
                }
            }
            printf (")\n");
            printf ("{\n");
            dummy = FALSE;
            emit_expansion (definition.data,
                            expansion_startindex, definition.length,
                            0, &dummy, &macroargs, "curr", 2);
            printf ("}\n");
          }
        else if (control_line_p (line))
          {
            /* Control directive.  */
            string_t definition;
            whole_definition (line, infile, &definition);
            finish_func ();
            emit_control_directive (&definition);
            stringlist_add (&main_output, &definition);
          }
        else
          {
            /* Normal text.  */
            unsigned long linelen;
            linelen = 0;
            while (line[linelen] != '\n')
              linelen++;
            new_statement ();
            emit_expansion (line, 0, linelen + 1,
                            line_number, &within_comment, NULL, "curr", 2);
            free (line);
          }
      }
    if (within_comment)
      {
        fflush (stdout);
        fprintf (stderr, "unterminated comment\n");
        exit (1);
      }
    fclose (infile);
  }
  finish_func ();

  /* Emit main function.  */
  printf ("int main (int argc, char **argv)\n");
  printf ("{\n");
  printf ("  TEXT curr;\n");
  printf ("  char *progname = argv[0];\n");
  printf ("  argv++; argc--;\n");
  printf ("  while (argc > 0)\n");
  printf ("    {\n");
  printf ("      if (strcmp (*argv, \"-l\") == 0)\n");
  printf ("        {\n");
  printf ("          do_line = 1;\n");
  printf ("          argv++; argc--;\n");
  printf ("        }\n");
  printf ("      else if (strcmp (*argv, \"-i\") == 0 && argc > 1)\n");
  printf ("        {\n");
  printf ("          infilename = argv[1];\n");
  printf ("          argv += 2; argc -= 2;\n");
  printf ("        }\n");
  printf ("      else if (strcmp (*argv, \"-o\") == 0 && argc > 1)\n");
  printf ("        {\n");
  printf ("          outfilename = argv[1];\n");
  printf ("          argv += 2; argc -= 2;\n");
  printf ("        }\n");
  printf ("      else\n");
  printf ("        {\n");
  printf ("          fprintf (stderr, \"Usage: %%s [-l] [-i filename] [-o filename]\\n\", progname);\n");
  printf ("          exit (1);\n");
  printf ("        }\n");
  printf ("    }\n");
  printf ("  TEXT_init1 (&curr, stdout);\n");
  {
    unsigned long i;
    for (i = 0; i < main_output.index; i++)
      emit_control_directive (&main_output.data[i]);
  }
  printf ("  if (ferror (stdout) || fclose (stdout))\n");
  printf ("    exit (1);\n");
  printf ("  exit (0);\n");
  printf ("}\n");

  /* Done. */
  if (ferror (stdout) || fclose (stdout))
    exit (1);
  exit (0);
}
