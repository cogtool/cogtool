# Program for adding GCTRIGGER statements at the head of function bodies.
# Bruno Haible 2004-12-07

# Goal:
# Convert declarations
#     ... maygc ... funname (type1 vname1, ..., typek vnamek)
#     {
#       ...
#     }
# to
#     ... maygc ... funname (type1 vname1, ..., typek vnamek)
#     { GCTRIGGERj(vname...); {
#       ...
#     }}
# where those vnamei are passed to GCTRIGGER whose types are 'object'.
# All comments, preprocessor commands etc. are preserved.

# Method:
# Knowing about preprocessor commands, comments, tokens, we look for the
# opening brace '{' at the outermost level, which is immediately preceded
# by a closing parenthesis ')'. The contents of the parentheses is analysed.
# The GCTRIGGER statement is inserted. At the corresponding closing brace '}'
# an additional brace is inserted.

#define local static
#define global
#define var
#define loop  while (1)
#define until(exp)  while (!(exp))
typedef unsigned char  uintB;
typedef unsigned short  uintW;
typedef unsigned long  uintL;
typedef int  boolean;
#define FALSE 0
#define TRUE 1

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if !(defined(__GNUC__) && !defined(__STRICT_ANSI__))
#define inline
#endif


# ============================= Memory utilities =============================

local char* xmalloc (uintL count)
{
  var char* tmp = (char*)malloc(count);
  if (!tmp) {
    fprintf(stderr,"Virtual memory exhausted.\n");
    exit(1);
  }
  return tmp;
}

local char* xrealloc (void* data, uintL count)
{
  var char* tmp = (char*)realloc(data,count);
  if (!tmp) {
    fprintf(stderr,"Virtual memory exhausted.\n");
    exit(1);
  }
  return tmp;
}

#ifdef unused
local inline void xfree (void* ptr)
{
  free((char*)ptr);
}
#endif


# ============================ Character utilities ============================

#ifdef unused
# Determine whether a character (not newline) is whitespace.
local inline boolean is_whitespace (char c)
{
  return (c == ' ') || (c == '\t');
}
#endif

#ifdef unused
# Determine whether a character is a digit (locale independent).
local inline boolean is_digit (char c)
{
  return (c >= '0') && (c <= '9');
}
#endif


# ============================= String utilities =============================

# Returns the freshly allocated copy of a strings.
local char* concat1 (const char* str1)
{
  var uintL len1 = strlen(str1);
  var char* result = xmalloc(len1+1);
  memcpy(result+0,str1,len1+1);
  return result;
}

#ifdef unused
# Returns the freshly allocated contenation of 2 strings.
local char* concat2 (const char* str1, const char* str2)
{
  var uintL len1 = strlen(str1);
  var uintL len2 = strlen(str2);
  var char* result = xmalloc(len1+len2+1);
  memcpy(result+0,str1,len1);
  memcpy(result+len1,str2,len2+1);
  return result;
}
#endif

#ifdef unused
# Returns the freshly allocated contenation of 3 strings.
local char* concat3 (const char* str1, const char* str2, const char* str3)
{
  var uintL len1 = strlen(str1);
  var uintL len2 = strlen(str2);
  var uintL len3 = strlen(str3);
  var char* result = xmalloc(len1+len2+len3+1);
  memcpy(result+0,str1,len1);
  memcpy(result+len1,str2,len2);
  memcpy(result+len1+len2,str3,len3+1);
  return result;
}
#endif

#ifdef unused
# Returns the freshly allocated contenation of 4 strings.
local char* concat4 (const char* str1, const char* str2, const char* str3, const char* str4)
{
  var uintL len1 = strlen(str1);
  var uintL len2 = strlen(str2);
  var uintL len3 = strlen(str3);
  var uintL len4 = strlen(str4);
  var char* result = xmalloc(len1+len2+len3+len4+1);
  memcpy(result+0,str1,len1);
  memcpy(result+len1,str2,len2);
  memcpy(result+len1+len2,str3,len3);
  memcpy(result+len1+len2+len3,str4,len4+1);
  return result;
}
#endif

#ifdef unused
# Returns a freshly allocated substring.
local char* substring (const char* str, uintL index1, uintL index2)
{
  if (!(index1 <= index2)) abort();
  if (!(index2 <= strlen(str))) abort();
  { var uintL len = index2-index1;
    var char* result = xmalloc(len+1);
    if (len > 0) memcpy(result,str+index1,len);
    result[len] = '\0';
    return result;
} }
#endif

#ifdef unused
# Returns a freshly allocated substring.
local char* substring_from_to (const char* p1, const char* p2)
{
  var uintL length = p2 - p1;
  var char* result = (char*) xmalloc(length+1);
  memcpy(result,p1,length);
  result[length] = '\0';
  return result;
}
#endif

# Compares two strings for equality.
local inline boolean String_equals (const char* str1, const char* str2)
{
  return !strcmp(str1,str2);
}

#ifdef unused
# Compares two strings for case-insensitive equality.
local boolean String_equalsIgnoreCase (const char* str1, const char* str2)
{
  while (*str1 != '\0' && *str2 != '\0') {
    var unsigned char c1 = (unsigned char)(*str1++);
    var unsigned char c2 = (unsigned char)(*str2++);
    if (c1 < 0x80) /* isascii(c1) */
      c1 = toupper(c1);
    if (c2 < 0x80) /* isascii(c2) */
      c2 = toupper(c2);
    if (c1 != c2)
      return FALSE;
  }
  # Now *str1 == '\0' || *str2 == '\0'.
  return (*str1 == *str2);
}
#endif


# ========================= Extensible string buffers =========================

typedef struct {
  uintL index;
  uintL size;
  char* data;
} StringBuffer;

local inline void StringBuffer_init (StringBuffer* sb)
{
  sb->size = 10;
  sb->data = (char*) xmalloc(sb->size);
  sb->index = 0;
}

#ifdef unused
local inline uintL StringBuffer_length (const StringBuffer* sb)
{
  return sb->index;
}
#endif

local void StringBuffer_append1 (StringBuffer* sb, char c)
{
  if (sb->index == sb->size) {
    sb->size = 2 * sb->size;
    sb->data = (char*) xrealloc(sb->data, sb->size);
  }
  sb->data[sb->index++] = c;
}

#ifdef unused
local void StringBuffer_append (StringBuffer* sb, const char* s)
{
  var uintL s_len = strlen(s);
  if (s_len > 0) {
    var uintL needed = sb->index + s_len;
    if (needed > sb->size) {
      sb->size = 2 * sb->size;
      if (sb->size < needed)
        sb->size = needed;
      sb->data = (char*) xrealloc(sb->data, sb->size);
    }
    memcpy(sb->data+sb->index,s,s_len);
    sb->index += s_len;
  }
}
#endif

local char* StringBuffer_toString (const StringBuffer* sb)
{
  var uintL s_len = sb->index;
  var char* s = (char*) xmalloc(s_len+1);
  memcpy(s,sb->data,s_len);
  s[s_len] = '\0';
  return s;
}

local inline void StringBuffer_delete (StringBuffer* sb)
{
  free(sb->data);
}


# ============================ Extensible vectors ============================

typedef struct {
  uintL index;
  uintL size;
  const void* * data;
} Vector;

local inline void Vector_init (Vector* v)
{
  v->size = 5;
  v->data = (const void* *) xmalloc(v->size * sizeof(const void*));
  v->index = 0;
}

local inline uintL Vector_length (const Vector* v)
{
  return v->index;
}

local void Vector_add (Vector* v, const void* elt)
{
  if (v->index >= v->size) {
    v->size = 2 * v->size;
    v->data = (const void* *) xrealloc(v->data, v->size * sizeof(const void*));
  }
  v->data[v->index++] = elt;
}

local const void * Vector_element (const Vector* v, uintL i)
{
  if (!(i < v->index)) {
    fprintf(stderr,"vector index out of bounds");
    abort();
  }
  return v->data[i];
}

#ifdef unused
local void Vector_set_element (Vector* v, uintL i, const void* elt)
{
  if (!(i < v->index)) {
    fprintf(stderr,"vector index out of bounds");
    abort();
  }
  v->data[i] = elt;
}
#endif

#ifdef unused
local void Vector_remove_element (Vector* v, uintL i)
{
  if (!(i < v->index)) {
    fprintf(stderr,"vector index out of bounds");
    abort();
  }
  v->index--;
  for (; i < v->index; i++)
    v->data[i] = v->data[i+1];
}
#endif

#ifdef unused
local void Vector_init_clone (Vector* w, const Vector* v)
{
  w->size = (v->size < 5 ? 5 : v->size);
  w->data = (const void* *) xmalloc(w->size * sizeof(const void*));
  memcpy(w->data,v->data,v->size * sizeof(const void*));
  w->index = v->size;
}
#endif

#ifdef unused
local Vector* Vector_clone (const Vector* v)
{
  var Vector* w = (Vector*) xmalloc(sizeof(Vector));
  Vector_init_clone(w,v);
  return w;
}
#endif

local inline void Vector_delete (Vector* v)
{
  free(v->data);
}


# A vector of strings.

typedef struct {
  Vector rep;
} VectorString;

local inline void VectorString_init (VectorString* v)
{
  Vector_init(&v->rep);
}

#ifdef unused
local VectorString* make_VectorString ()
{
  var VectorString* v = (VectorString*) xmalloc(sizeof(VectorString));
  VectorString_init(v);
  return v;
}
#endif

local inline uintL VectorString_length (const VectorString* v)
{
  return Vector_length(&v->rep);
}

local inline void VectorString_add (VectorString* v, const char* elt)
{
  Vector_add(&v->rep,elt);
}

local inline const char* VectorString_element (const VectorString* v, uintL i)
{
  return (const char*) Vector_element(&v->rep,i);
}

#ifdef unused
local inline void VectorString_set_element (VectorString* v, uintL i, const char* elt)
{
  Vector_set_element(&v->rep,i,elt);
}
#endif

#ifdef unused
local inline void VectorString_init_clone (VectorString* w, const VectorString* v)
{
  Vector_init_clone(&w->rep,&v->rep);
}
#endif

#ifdef unused
local VectorString* VectorString_clone (const VectorString* v)
{
  var VectorString* w = (VectorString*) xmalloc(sizeof(VectorString));
  VectorString_init_clone(w,v);
  return w;
}
#endif

local inline void VectorString_delete (VectorString* v)
{
  Vector_delete(&v->rep);
}

#ifdef unused
# Tests whether v equals w.
local boolean VectorString_equals (const VectorString* v, const VectorString* w)
{
  var uintL n = VectorString_length(v);
  if (VectorString_length(w) == n) {
    var uintL i;
    for (i = 0; i < n; i++)
      if (!String_equals(VectorString_element(v,i),VectorString_element(w,i)))
        return FALSE;
    return TRUE;
  }
  return FALSE;
}
#endif

#ifdef unused
# Tests whether v starts with w.
local boolean VectorString_startsWith (const VectorString* v, const VectorString* w)
{
  var uintL n = VectorString_length(w);
  if (VectorString_length(v) >= n) {
    var uintL i;
    for (i = 0; i < n; i++)
      if (!String_equals(VectorString_element(v,i),VectorString_element(w,i)))
        return FALSE;
    return TRUE;
  }
  return FALSE;
}
#endif


# =================================== Input ===================================

local FILE* infile;

local uintL input_line;

local int in_char (void)
{
  var int c = getc(infile);
  if (c=='\n')
    input_line++;
  return c;
}

local int peek_char (void)
{
  var int c = getc(infile);
  if (!(c==EOF))
    ungetc(c,infile);
  return c;
}

local uintL last_good_input_line;


# ================================== Output ==================================

local FILE* outfile;

local void out_char (uintB ch)
{
  putc(ch,outfile);
}


# ============================= Lexical Analysis =============================

# Fetches the next character.
local int next_char (void)
{
  var int c = in_char();
  if (!(c==EOF))
    out_char(c); # output c
  return c;
}

# For our purpose, ++ -> != etc. don't need to be recognized as tokens of their
# own. Therefore we distinguish only:
#   - EOF
#   - identifier
#   - number literals
#   - character literals
#   - string literals
#   - operator/separator
# Generalized tokens can be expressions, with balanced parentheses.
enum token_type {
  eof,
  eol,
  ident,
  number,
  charliteral,
  stringliteral,
  sep,
  expr
};
typedef struct {
  enum token_type type;
  char* string; # for identifier
  uintB ch; # for operator/separator
} Token;

local Token* Token_dup (const Token* token)
{
  Token* duplicate = (Token*) xmalloc(sizeof(Token));
  *duplicate = *token;
  return duplicate;
}

local inline void Token_delete (Token* token)
{
  if (token->type == ident)
    free(token->string);
}


# A vector of tokens.

typedef struct {
  Vector rep;
} VectorToken;

local inline void VectorToken_init (VectorToken* v)
{
  Vector_init(&v->rep);
}

#ifdef unused
local VectorToken* make_VectorToken ()
{
  var VectorToken* v = (VectorToken*) xmalloc(sizeof(VectorToken));
  VectorToken_init(v);
  return v;
}
#endif

local inline uintL VectorToken_length (const VectorToken* v)
{
  return Vector_length(&v->rep);
}

local inline void VectorToken_add (VectorToken* v, Token* elt)
{
  Vector_add(&v->rep,elt);
}

local inline Token* VectorToken_element (const VectorToken* v, uintL i)
{
  return (Token*) Vector_element(&v->rep,i);
}

#ifdef unused
local inline void VectorToken_set_element (VectorToken* v, uintL i, Token* elt)
{
  Vector_set_element(&v->rep,i,elt);
}
#endif

#ifdef unused
local inline void VectorToken_init_clone (VectorToken* w, const VectorToken* v)
{
  Vector_init_clone(&w->rep,&v->rep);
}
#endif

#ifdef unused
local VectorToken* VectorToken_clone (const VectorToken* v)
{
  var VectorToken* w = (VectorToken*) xmalloc(sizeof(VectorToken));
  VectorToken_init_clone(w,v);
  return w;
}
#endif

local inline void VectorToken_delete (VectorToken* v)
{
  Vector_delete(&v->rep);
}


# Fetches the next token.
# (Inside preprocessor directives, newline counts as token of its own, and '#'
# doesn't introduce a nested preprocessor directive.)
local Token nexttoken (boolean within_prep_directive)
{
  Token token;
 restart:
  { var int c = next_char();
    switch (c) {
      case EOF:
        token.type = eof; return token;
      case ' ': case '\v': case '\t':
        # Ignore whitespace.
        goto restart;
      case '\n':
        # End of line.
        if (within_prep_directive) {
          token.type = eol; return token;
        } else
          # Ignore whitespace.
          goto restart;
      case '\\':
        if (peek_char()=='\n') {
          # Ignore backslash-newline sequence.
          next_char(); goto restart;
        } else
          goto separator;
      case '/':
        if (peek_char() == '*') {
          # Comment.
          next_char();
          loop {
            c = next_char();
            if (c==EOF) {
              fprintf(stderr,"Unfinished comment\n"); break;
            }
            if ((c=='*') && (peek_char()=='/')) {
              next_char(); break;
            }
          }
          goto restart;
        } else
          goto separator;
      case '*':
        if (peek_char() == '/')
          # Invalid end of comment.
          fprintf(stderr,"Comment end outside of comment in line %lu\n",input_line);
        goto separator;
      case '#':
        if (within_prep_directive)
          goto separator;
        else {
          # Preprocessor directive. Read until end of line or EOF.
          loop {
            var Token subtoken = nexttoken(TRUE);
            if (subtoken.type == eof || subtoken.type == eol)
              break;
            Token_delete(&subtoken);
          }
          # Ignore it.
          goto restart;
        }
      case '.':
        c = peek_char();
        if (!(((c>='0') && (c<='9')) || (c=='.')))
          goto separator;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        # Number. Continue reading while alphanumeric character or '.':
        loop {
          c = peek_char();
          if (((c>='0') && (c<='9'))
              || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z'))
              || (c=='.'))
            next_char();
          else
            break;
        }
        token.type = number; return token;
      case '\'':
        # Character literal.
        loop {
          c = next_char();
          if (c==EOF) {
            fprintf(stderr,"unterminated character constant\n"); break;
          }
          if (c=='\'')
            break;
          if (c=='\\')
            c = next_char();
        }
        token.type = charliteral; return token;
      case '\"':
        # String literal.
        loop {
          c = next_char();
          if (c==EOF) {
            fprintf(stderr,"unterminated string constant\n"); break;
          }
          if (c=='\"')
            break;
          if (c=='\\')
            c = next_char();
        }
        token.type = stringliteral; return token;
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
        # Identifier.
        {
          StringBuffer accumulator;
          StringBuffer_init(&accumulator);
          loop {
            StringBuffer_append1(&accumulator,c);
            c = peek_char();
            if (   ((c>='0') && (c<='9'))
                || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z'))
                || (c=='_'))
              next_char();
            else
              break;
          }
          token.type = ident; token.string = StringBuffer_toString(&accumulator);
          StringBuffer_delete(&accumulator);
          return token;
        }
      default:
      separator:
        token.type = sep; token.ch = c; return token;
    }
  }
}

local inline Token next_token (void)
{
  return nexttoken(FALSE);
}

# Counting parentheses and braces.
#define MAXBRACES 1000 # maximal nesting depth of parentheses and braces
local struct {
  uintL count;
  struct { uintB brace_type; uintL input_line; } opening[MAXBRACES];
} open_braces;

local void handle_opening_token (const Token* token)
{
  if (open_braces.count < MAXBRACES) {
    open_braces.opening[open_braces.count].brace_type = token->ch;
    open_braces.opening[open_braces.count].input_line = input_line;
  }
  open_braces.count++;
}

local inline void handle_closing_token (const Token* token)
{
  open_braces.count--;
}

# Read the next expression with balanced parentheses and braces '()', '{}',
# '[]'. It reads until open_braces.count==open_braces_start. Upon entry,
# open_braces.count may already be > open_braces_start.
local Token next_balanced_token (Token* start_token, uintL open_braces_start)
{
  var Token token = (start_token==NULL ? next_token() : *start_token);
  var enum token_type final_type = token.type;
  loop {
    # Here always  open_braces.count >= open_braces_start .
    switch (token.type) {
      case eof:
        if (open_braces.count > open_braces_start) {
          if (open_braces.count <= MAXBRACES)
            fprintf(stderr,"unclosed '%c' in line %lu\n",
                    open_braces.opening[open_braces.count-1].brace_type,
                    open_braces.opening[open_braces.count-1].input_line);
          else
            fprintf(stderr,"unclosed '(' or '{' or '['\n");
        }
        return token; # return the EOF token
      case sep:
        switch (token.ch) {
          case '(': case '{': case '[':
            handle_opening_token(&token);
            break;
          case ')': case '}': case ']':
            if (open_braces.count > open_braces_start) {
              handle_closing_token(&token);
              if (open_braces.count < MAXBRACES) {
                var uintB opening_ch = open_braces.opening[open_braces.count].brace_type;
                var uintB closing_ch = token.ch;
                if (!(   ((opening_ch == '(') && (closing_ch == ')'))
                      || ((opening_ch == '{') && (closing_ch == '}'))
                      || ((opening_ch == '[') && (closing_ch == ']')))) {
                  fprintf(stderr,"opening '%c' in line %lu\n and closing '%c'\n in line %lu do not match.\n",
                          opening_ch,open_braces.opening[open_braces.count].input_line,
                          closing_ch,input_line);
                }
              }
            } else {
              if (open_braces.count == 0)
                fprintf(stderr,"not opened '%c' in line %lu\n",token.ch,input_line);
              if (token.type != final_type)
                Token_delete(&token);
              token.type = final_type;
              return token;
            }
            break;
          default:
            break;
        }
      default: ;
        # Everything else is balanced.
    }
    if (open_braces.count == open_braces_start) # done with balanced token?
      break;
    # no -> read next token:
    Token_delete(&token);
    token = next_token();
    final_type = expr;
  }
  if (token.type != final_type)
    Token_delete(&token);
  token.type = final_type;
  return token;
}

# Convert the function definitions in an entire file.
local void convert (void)
{
  var boolean seen_maygc = FALSE;
  input_line = 1; last_good_input_line = 1;
  open_braces.count = 0;
  loop {
    var Token token = next_token();
   restart:
    if (token.type == sep && token.ch == ';')
      seen_maygc = FALSE;
    else if (token.type == ident && String_equals(token.string,"maygc"))
      seen_maygc = TRUE;
    else if (seen_maygc && token.type == sep && token.ch == '(') {
      var VectorString parameters_of_type_object;
      handle_opening_token(&token);
      # Remember the variable names from the parameter list.
      VectorString_init(&parameters_of_type_object);
      loop {
        # Parse a single parameter declaration.
        VectorToken parameter_declaration;
        VectorToken_init(&parameter_declaration);
        loop {
          token = next_balanced_token(NULL,open_braces.count);
          if (token.type == eof)
            break;
          if (token.type == sep && (token.ch == ',' || token.ch == ')'))
            break;
          VectorToken_add(&parameter_declaration,Token_dup(&token));
        }
        if (VectorToken_length(&parameter_declaration) == 2
            && VectorToken_element(&parameter_declaration,0)->type == ident
            && String_equals(VectorToken_element(&parameter_declaration,0)->string,"object")
            && VectorToken_element(&parameter_declaration,1)->type == ident) {
          char* varname =
            concat1(VectorToken_element(&parameter_declaration,1)->string);
          VectorString_add(&parameters_of_type_object,varname);
        }
        # Free memory.
        {
          var uintL i;
          for (i = 0; i < VectorToken_length(&parameter_declaration); i++) {
            Token_delete(VectorToken_element(&parameter_declaration,i));
            free(VectorToken_element(&parameter_declaration,i));
          }
        }
        VectorToken_delete(&parameter_declaration);
        if (token.type == eof)
          break;
        if (token.ch == ')') {
          handle_closing_token(&token);
          break;
        }
      }
      if (token.type != eof) {
        token = next_token();
        if (token.type == sep && token.ch == '{') {
          # Here's the point where we insert the GCTRIGGER statement.
          fputs(" GCTRIGGER",outfile);
          if (VectorString_length(&parameters_of_type_object) > 0) {
            fprintf(outfile,"%u",(unsigned int)VectorString_length(&parameters_of_type_object));
            out_char('(');
            {
              var uintL i;
              for (i = 0; i < VectorString_length(&parameters_of_type_object); i++) {
                if (i > 0)
                  out_char(',');
                fputs(VectorString_element(&parameters_of_type_object,i),outfile);
              }
            }
            out_char(')');
          } else {
            fputs("()",outfile);
          }
          fputs("; {",outfile);
          token = next_balanced_token(&token,0);
          # Here's the point where we insert the closing brace.
          out_char('}');
          seen_maygc = FALSE;
        }
      }
      # Free memory.
      {
        var uintL i;
        for (i = 0; i < VectorString_length(&parameters_of_type_object); i++)
          free((char*)VectorString_element(&parameters_of_type_object,i));
      }
      VectorString_delete(&parameters_of_type_object);
      # Continue the loop with the new token (as it might be a semicolon).
      goto restart;
    }
    if (token.type == eof)
      break;
    Token_delete(&token);
  }
}

int main ()
{
  infile = stdin;
  outfile = stdout;
  convert();
  if (ferror(stdin) || ferror(stdout) || fclose(stdout))
    exit(1);
  exit(0);
}

# This program has been tested with valgrind-2.2.0, using the command line
# valgrind --tool=memcheck --num-callers=20 --leak-check=yes --leak-resolution=high --show-reachable=yes gctrigger
