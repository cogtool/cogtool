# VARBRACE - CLISP source preprocessor
# Bruno Haible 15.8.1999

# This preprocessor adds braces to source code, so that variable declarations
# (introduced with the pseudo-keyword `var') can be used within blocks, like
# in C++.
# Example:
#   {
#     var decl1;
#     statement1;
#     var decl2;
#     statement2;
#   }
# becomes
#   {
#     {var decl1;
#     statement1;
#     {var decl2;
#     statement2;
#    }}
#   }

# Restrictions and caveats:
# - The last line in the input file should be terminated with a newline.
# - #line lines should not be separated into multiple lines using
#   backslash-newline.
# - No multi-line comments should start in a preprocessor line.
# - Closing braces should not be enclosed in #if conditionals.
# - #if conditions are assumed to be constant from the `var' declaration to
#   the closing brace.


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
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>

#ifndef NULL
#define NULL ((void*)0)
#endif

#ifdef __cplusplus
extern "C" void exit(int);
#endif

#if !(defined(__GNUC__) && !defined(__STRICT_ANSI__))
#define inline
#endif

# g++ 3.3 doesn't accept compound expressions as initializers; as a workaround,
# we transform "var object foo = ..." into "var object foo; foo = ...",
# and likewise "var chart foo = ..." into "var chart foo; foo = ...".
#if defined(__GNUG__) && (__GNUC__ == 3) && (__GNUC_MINOR__ == 3)
#define SPLIT_OBJECT_INITIALIZATIONS
#endif


# Memory utilities.

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

local inline void xfree (void* ptr)
{
  free((char*)ptr);
}


# Character utilities.

# Determine whether a character (not newline) is whitespace.
local inline boolean is_whitespace (char c)
{
  return (c == ' ') || (c == '\t');
}

# Determine whether a charater is a digit (locale independent).
local inline boolean is_digit (char c)
{
  return (c >= '0') && (c <= '9');
}


# String utilities.

# Returns the freshly allocated copy of a strings.
local char* concat1 (const char* str1)
{
  var uintL len1 = strlen(str1);
  var char* result = xmalloc(len1+1);
  memcpy(result+0,str1,len1+1);
  return result;
}

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


# Extensible vectors.

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

local void Vector_set_element (Vector* v, uintL i, const void* elt)
{
  if (!(i < v->index)) {
    fprintf(stderr,"vector index out of bounds");
    abort();
  }
  v->data[i] = elt;
}

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


# A vector of strings.

typedef struct {
  Vector rep;
} VectorString;

local inline void VectorString_init (VectorString* v)
{
  Vector_init(&v->rep);
}

local VectorString* make_VectorString ()
{
  var VectorString* v = (VectorString*) xmalloc(sizeof(VectorString));
  VectorString_init(v);
  return v;
}

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

local inline void VectorString_set_element (VectorString* v, uintL i, const char* elt)
{
  Vector_set_element(&v->rep,i,elt);
}

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


# A vector of vectors of strings.

typedef struct {
  Vector rep;
} VectorVectorString;

local inline void VectorVectorString_init (VectorVectorString* v)
{
  Vector_init(&v->rep);
}

local VectorVectorString* make_VectorVectorString ()
{
  var VectorVectorString* v = (VectorVectorString*) xmalloc(sizeof(VectorVectorString));
  VectorVectorString_init(v);
  return v;
}

local inline uintL VectorVectorString_length (const VectorVectorString* v)
{
  return Vector_length(&v->rep);
}

local inline void VectorVectorString_add (VectorVectorString* v, const VectorString* elt)
{
  Vector_add(&v->rep,elt);
}

local inline const VectorString* VectorVectorString_element (const VectorVectorString* v, uintL i)
{
  return (const VectorString*) Vector_element(&v->rep,i);
}

#ifdef unused
local inline void VectorVectorString_set_element (VectorVectorString* v, uintL i, const VectorString* elt)
{
  Vector_set_element(&v->rep,i,elt);
}
#endif

#ifdef unused
local inline void VectorVectorString_init_clone (VectorVectorString* w, const VectorVectorString* v)
{
  Vector_init_clone(&w->rep,&v->rep);
}
#endif

#ifdef unused
local VectorVectorString* VectorVectorString_clone (const VectorVectorString* v)
{
  var VectorVectorString* w = (VectorVectorString*) xmalloc(sizeof(VectorVectorString));
  VectorVectorString_init_clone(w,v);
  return w;
}
#endif


# A stack of vectors of strings.

typedef struct {
  Vector rep;
} StackVectorString;

local inline void StackVectorString_init (StackVectorString* v)
{
  Vector_init(&v->rep);
}

local StackVectorString* make_StackVectorString ()
{
  var StackVectorString* v = (StackVectorString*) xmalloc(sizeof(StackVectorString));
  StackVectorString_init(v);
  return v;
}

local inline uintL StackVectorString_length (const StackVectorString* v)
{
  return Vector_length(&v->rep);
}

#ifdef unused
local inline boolean StackVectorString_is_empty (const StackVectorString* v)
{
  return StackVectorString_length(v) == 0;
}
#endif

local inline void StackVectorString_push (StackVectorString* v, const VectorString* elt)
{
  Vector_add(&v->rep,elt);
}

local inline VectorString* StackVectorString_element (const StackVectorString* v, uintL i)
{
  return (VectorString*) Vector_element(&v->rep,i);
}

local VectorString* StackVectorString_peek (StackVectorString* v)
{
  var uintL n = StackVectorString_length(v);
  if (n == 0) { fprintf(stderr,"stack empty\n"); exit(1); }
  return StackVectorString_element(v,n-1);
}

local VectorString* StackVectorString_pop (StackVectorString* v)
{
  var uintL n = StackVectorString_length(v);
  if (n == 0) { fprintf(stderr,"stack empty\n"); exit(1); }
  { var VectorString* result = StackVectorString_element(v,n-1);
    v->rep.index -= 1;
    return result;
} }

#ifdef unused
# Push elt, and optimize: elt can be removed if is starts with an already
# present string sequence. If another element starts with elt, that one can
# be removed.
local void StackVectorString_push_optimize (StackVectorString* v, const VectorString* elt)
{
  var uintL n = StackVectorString_length(v);
  var uintL i;
  for (i = 0; i < n; i++)
    if (VectorString_startsWith(elt,StackVectorString_element(v,i)))
      return;
  for (i = 0; i < n; )
    if (VectorString_startsWith(StackVectorString_element(v,i),elt)) {
      Vector_remove_element(&v->rep,i);
      n--;
    } else
      i++;
  Vector_add(&v->rep,elt);
}
#endif


# The #if[def] stack. All the conditions are implicitly combined by &&.
# For every #if we start a new entry in the stack, which is popped when we
# see the corresponding #endif. This is a stack of vector of string, not a
# stack of string, because when a #elif is seen, we add an element to the
# stack without popping the previous one.

local StackVectorString* ifdef_stack;

# Operations on the #if[def] stack.

local void do_if (const char * condition)
{
  var VectorString* v = make_VectorString();
  VectorString_add(v,condition);
  StackVectorString_push(ifdef_stack,v);
}

local void do_else ()
{
  var VectorString* v = StackVectorString_peek(ifdef_stack);
  var uintL i = VectorString_length(v) - 1;
  var const char* lastcondition = VectorString_element(v,i);
  lastcondition = concat3("!(",lastcondition,")");
  VectorString_set_element(v,i,lastcondition);
}

local void do_elif (const char * condition)
{
  var VectorString* v = StackVectorString_peek(ifdef_stack);
  var uintL i = VectorString_length(v) - 1;
  var const char* lastcondition = VectorString_element(v,i);
  lastcondition = concat3("!(",lastcondition,")");
  VectorString_set_element(v,i,lastcondition);
  VectorString_add(v,condition);
}

local void do_endif ()
{
  StackVectorString_pop(ifdef_stack);
}

# Returns the current #if condition.
# It is a vector of strings, implicitly combined by &&.
# The vector is freshly constructed, but the strings are shared.

local VectorString* current_condition ()
{
  var VectorString* result = make_VectorString();
  var uintL n = StackVectorString_length(ifdef_stack);
  var uintL i;
  for (i = 0; i < n; i++) {
    var const VectorString* v = StackVectorString_element(ifdef_stack,i);
    var uintL m = VectorString_length(v);
    var uintL j;
    for (j = 0; j < m; j++)
      VectorString_add(result,VectorString_element(v,j));
  }
  return result;
}

# Reduces a condition modulo the current condition, i.e. removes all
# && clauses which are already implied in the current condition.

local const VectorString* modulo_current_condition (const VectorString* condition)
{
  # Quick and dirty: Just remove the start segment:
  # condition[0]==current_condition[0], ... condition[n]==current_condition[n].
  var VectorString* current = current_condition();
  var uintL i;
  for (i = 0; i < VectorString_length(current) && i < VectorString_length(condition); i++)
    if (!String_equals(VectorString_element(current,i),VectorString_element(condition,i)))
      break;
  if (i == 0)
    return condition;
  else {
    var VectorString* new_condition = make_VectorString();
    for (; i < VectorString_length(condition); i++)
      VectorString_add(new_condition,VectorString_element(condition,i));
    return new_condition;
  }
}


# Parsing of #if/#else/#elif/#endif lines.

local const char* is_if (const char* line)
{
  var uintL n = strlen(line);
  var uintL i = 0;
  # Skip whitespace.
  for (; i < n && is_whitespace(line[i]); i++);
  # Parse a '#'.
  if (i < n && line[i] == '#')
    i++;
  else
    return NULL;
  # Skip whitespace.
  for (; i < n && is_whitespace(line[i]); i++);
  # Check for "if".
  if (i+2 < n
      && line[i+0] == 'i'
      && line[i+1] == 'f'
      && is_whitespace(line[i+2])) {
    i += 3;
    for (; i < n && is_whitespace(line[i]); i++);
    for (; n > i && is_whitespace(line[n-1]); n--);
    return substring(line,i,n);
  }
  # Check for "ifdef".
  if (i+5 < n
      && line[i+0] == 'i'
      && line[i+1] == 'f'
      && line[i+2] == 'd'
      && line[i+3] == 'e'
      && line[i+4] == 'f'
      && is_whitespace(line[i+5])) {
    i += 6;
    for (; i < n && is_whitespace(line[i]); i++);
    for (; n > i && is_whitespace(line[n-1]); n--);
    { var char* term = substring(line,i,n);
      var const char* result = concat3("defined(",term,")");
      xfree(term);
      return result;
  } }
  # Check for "ifndef".
  if (i+6 < n
      && line[i+0] == 'i'
      && line[i+1] == 'f'
      && line[i+2] == 'n'
      && line[i+3] == 'd'
      && line[i+4] == 'e'
      && line[i+5] == 'f'
      && is_whitespace(line[i+6])) {
    i += 7;
    for (; i < n && is_whitespace(line[i]); i++);
    for (; n > i && is_whitespace(line[n-1]); n--);
    { var char* term = substring(line,i,n);
      var const char* result = concat3("!defined(",term,")");
      xfree(term);
      return result;
  } }
  return NULL;
}

local boolean is_else (const char* line)
{
  var uintL n = strlen(line);
  var uintL i = 0;
  # Skip whitespace.
  for (; i < n && is_whitespace(line[i]); i++);
  # Parse a '#'.
  if (i < n && line[i] == '#')
    i++;
  else
    return FALSE;
  # Skip whitespace.
  for (; i < n && is_whitespace(line[i]); i++);
  # Check for "else".
  if (i+4 <= n
      && line[i+0] == 'e'
      && line[i+1] == 'l'
      && line[i+2] == 's'
      && line[i+3] == 'e'
      && (i+4 == n || is_whitespace(line[i+4]))) {
    return TRUE;
  }
  return FALSE;
}

local const char* is_elif (const char* line)
{
  var uintL n = strlen(line);
  var uintL i = 0;
  # Skip whitespace.
  for (; i < n && is_whitespace(line[i]); i++);
  # Parse a '#'.
  if (i < n && line[i] == '#')
    i++;
  else
    return NULL;
  # Skip whitespace.
  for (; i < n && is_whitespace(line[i]); i++);
  # Check for "elif".
  if (i+4 < n
      && line[i+0] == 'e'
      && line[i+1] == 'l'
      && line[i+2] == 'i'
      && line[i+3] == 'f'
      && is_whitespace(line[i+4])) {
    i += 5;
    for (; i < n && is_whitespace(line[i]); i++);
    for (; n > i && is_whitespace(line[n-1]); n--);
    return substring(line,i,n);
  }
  return NULL;
}

local boolean is_endif (const char* line)
{
  var uintL n = strlen(line);
  var uintL i = 0;
  # Skip whitespace.
  for (; i < n && is_whitespace(line[i]); i++);
  # Parse a '#'.
  if (i < n && line[i] == '#')
    i++;
  else
    return FALSE;
  # Skip whitespace.
  for (; i < n && is_whitespace(line[i]); i++);
  # Check for "endif".
  if (i+5 <= n
      && line[i+0] == 'e'
      && line[i+1] == 'n'
      && line[i+2] == 'd'
      && line[i+3] == 'i'
      && line[i+4] == 'f'
      && (i+5 == n || is_whitespace(line[i+5]))) {
    return TRUE;
  }
  return FALSE;
}


# Print a list (cond1 && cond2 && ...) to a stream.
local void print_condition_part (FILE* stream, const VectorString* condition)
{
  var uintL n = VectorString_length(condition);
  if (n == 0) {
    fprintf(stream,"1");
    return;
  }
  if (n == 1) {
    fprintf(stream,"%s",VectorString_element(condition,0));
    return;
  }
  {
    var uintL i;
    for (i = 0; i < n; i++) {
      if (i > 0)
        fprintf(stream," && ");
      fprintf(stream,"(%s)",VectorString_element(condition,i));
    }
  }
}

# Tests whether a condition is equivalent to 1 (true).
local inline boolean is_true_condition_part (const VectorString* condition)
{
  var uintL n = VectorString_length(condition);
  return (n == 0);
}

#ifdef unused
# Print a list of lists (cond1 || cond2 || ...) to a stream.
local void print_condition (FILE* stream, const VectorVectorString* condition)
{
  var uintL n = VectorVectorString_length(condition);
  if (n == 0) {
    fprintf(stream,"0");
    return;
  }
  if (n == 1) {
    print_condition_part(stream,VectorVectorString_element(condition,0));
    return;
  }
  {
    var uintL i;
    for (i = 0; i < n; i++) {
      if (i > 0)
        fprintf(stream," || ");
      fprintf(stream,"(");
      print_condition_part(stream,VectorVectorString_element(condition,i));
      fprintf(stream,")");
    }
  }
}
#endif

#ifdef unused
# Tests whether a condition is equivalent to 0 (false).
local inline boolean is_false_condition (const VectorVectorString* condition)
{
  var uintL n = VectorVectorString_length(condition);
  return (n == 0);
}
#endif

#ifdef unused
# Tests whether a condition is equivalent to 1 (true).
local boolean is_true_condition (const VectorVectorString* condition)
{
  var uintL n = VectorVectorString_length(condition);
  var uintL i;
  for (i = 0; i < n; i++)
    if (is_true_condition_part(VectorVectorString_element(condition,i)))
      return TRUE;
  return FALSE;
}
#endif


#ifdef unused
# Read a line, or NULL if EOF is encountered.
local char* get_line (FILE* fp)
{
  var int len = 1;
  var char* line = (char*) xmalloc(len);
  var int index = 0;
  loop {
    var int c = getc(fp);
    if (c==EOF) { if (index>0) break; else { xfree(line); return NULL; } }
    if (c=='\n') break;
    if (index >= len-1) {
      len = 2*len;
      line = (char*) xrealloc(line,len);
    }
    line[index++] = c;
  }
  line[index] = '\0';
  return line;
}
#endif


# ================================== INPUT ==================================

local FILE* infile;

local const char* input_filename = "(stdin)";

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
  if (c != EOF)
    ungetc(c,infile);
  return c;
}


# Decode a #line directive. If the line represents a #line directive,
# return the line number. Else return -1.

local int decode_line_directive (const char* line)
{
  var uintL n = strlen(line);
  var uintL i = 0;
  # Skip whitespace.
  for (; i < n && is_whitespace(line[i]); i++);
  # Parse a '#'.
  if (i < n && line[i] == '#')
    i++;
  else
    return -1;
  # Skip whitespace.
  for (; i < n && is_whitespace(line[i]); i++);
  # Check for "line".
  if (i+4 < n
      && line[i+0] == 'l'
      && line[i+1] == 'i'
      && line[i+2] == 'n'
      && line[i+3] == 'e'
      && is_whitespace(line[i+4])) {
    i += 4;
    for (; i < n && is_whitespace(line[i]); i++);
  }
  # Check for a digit.
  if (!(i < n && is_digit(line[i])))
    return -1;
  { var uintL i1 = i;
    for (; i < n && is_digit(line[i]); i++);
    { var uintL i2 = i;
      # Convert digit string to a `long'.
      var char* digits = substring(line,i1,i2);
      errno = 0;
      { var long result = strtol(digits,NULL,10);
        xfree(digits);
        if (errno != 0) return -1;
        if (result < 0) abort();
        # Check for a source file name.
        for (; i < n && is_whitespace(line[i]); i++);
        if (i < n && line[i] == '"') {
          var uintL i3;
          i++;
          i3 = i;
          for (; i < n && line[i] != '"'; i++);
          if (i < n && line[i] == '"') {
            var uintL i4 = i;
            input_filename = substring(line,i3,i4);
          }
        }
        return result;
} } } }


# ================================== OUTPUT =================================

local FILE* outfile;

# Output can be buffered for a while.
enum out_mode { direct, buffered };
#define MAXHEADERLEN  5000
local struct {
  enum out_mode mode;
  uintB buffer[MAXHEADERLEN];
  uintL buffindex;
} out;

local inline void char_out (uintB c)
{
  putc(c,outfile);
}

# Turn off output buffering.
local void outbuffer_off (void)
{
  if (out.mode==buffered) {
    var uintL index = 0;
    while (index < out.buffindex) {
      char_out(out.buffer[index]); index++;
    }
    out.mode = direct;
  }
}

# Turn off output buffering, inserting a string at a given place.
local void outbuffer_off_insert (uintL insertpoint, char* insert)
{
  if (out.mode==buffered) {
    var uintL index = 0;
    loop {
      if (index==insertpoint) {
        while (!(*insert==0)) {
          char_out(*insert++);
        }
      }
      if (index == out.buffindex)
        break;
      char_out(out.buffer[index]); index++;
    }
    out.mode = direct;
  }
}

# Turn on output buffering.
local void outbuffer_on (void)
{
  if (out.mode==direct) {
    out.buffindex = 0;
    out.mode = buffered;
  }
}

# Output a character.
local void out_char (int c)
{
  if (out.mode==buffered) {
    if (out.buffindex < MAXHEADERLEN)
      out.buffer[out.buffindex++] = c;
    else {
      # Buffer full -> turn it off
      outbuffer_off(); char_out(c);
    }
  } else {
    char_out(c);
  }
}

# Output a line.
local void out_line (const char* line)
{
  for (; *line != '\0'; line++)
    out_char(*line);
  out_char('\n');
}


# =============================== MAIN PROGRAM ==============================

local int next_char (void)
{
  var int c = in_char();
  if (c != EOF)
    out_char(c);
  return c;
}

local char* next_line (void)
{
  var int len = 1;
  var char* line = (char*) xmalloc(len);
  var int index = 0;
  loop {
    var int c = next_char();
    if (c==EOF) { if (index>0) break; else { xfree(line); return NULL; } }
    if (c=='\n') break;
    if (index >= len-1) {
      len = 2*len;
      line = (char*) xrealloc(line,len);
    }
    line[index++] = c;
  }
  line[index] = '\0';
  return line;
}


# Sadly, #line directives output during a skipped portion of #if are ignored
# by the C preprocessor. Therefore we must re-output them after every
# #else/#elif/#endif line that belongs to a #if that was in effect when the
# line directive was seen.

# The maximum #if level that needs repeated #line directives.
# This is always <= StackVectorString_length(ifdef_stack).
local uintL ifdef_line_repeat;

# Emit a #line note after the line number in the outfile has changed
# differently from the line number in the infile.
local inline void line_emit (void)
{
  fprintf(outfile,"#line %ld\n",input_line);
  if (ifdef_line_repeat < StackVectorString_length(ifdef_stack))
    ifdef_line_repeat = StackVectorString_length(ifdef_stack);
}

# #line treatment for the #if[def] stack.

local inline void line_repeat_else ()
{
  if (ifdef_line_repeat == StackVectorString_length(ifdef_stack)) {
    char buf[20];
    sprintf(buf,"#line %ld",input_line);
    out_line(buf);
  }
}

local inline void line_repeat_endif ()
{
  if (ifdef_line_repeat > StackVectorString_length(ifdef_stack)) {
    char buf[20];
    sprintf(buf,"#line %ld",input_line);
    out_line(buf);
    ifdef_line_repeat--;
  }
}


enum token_type { eof, ident, number, charconst, stringconst, sep, expr };
typedef struct {
  enum token_type type;
  # if buffered:
  uintL startindex;
  uintL endindex;
  # if type==sep (operator or separator):
  uintB ch;
} Token;

local Token next_token (void)
{
  var Token token;
 restart:
  outbuffer_off();
  outbuffer_on();
  token.startindex = out.buffindex;
  {
    var int c = next_char();
    switch (c) {
      case EOF:
        # EOF
        token.type = eof;
        goto done;
      case ' ': case '\v': case '\t': case '\n':
        # whitespace, ignore
        goto restart;
      case '\\':
        if (peek_char()=='\n') {
          # backslash newline, ignore
          next_char();
          goto restart;
        }
        goto separator;
      case '/':
        if (peek_char() == '*') {
          # Comment
          next_char();
          loop {
            c = next_char();
            if (c==EOF) {
              fprintf(stderr,"Unfinished comment\n");
              break;
            }
            if ((c=='*') && (peek_char()=='/')) {
              next_char();
              break;
            }
          }
          goto restart;
        }
        goto separator;
      case '*':
        if (peek_char() == '/')
          fprintf(stderr,"End of comment outside comment in line %lu\n",input_line);
        goto separator;
      case '#':
        # preprocessor directive
        {
          var char* line = next_line();
          if (line) {
            var char* old_line = line;
            line = concat2("#",line);
            xfree(old_line);
          } else
            line = concat1("#");
          while (line[strlen(line)-1] == '\\') {
            var char* continuation_line = next_line();
            line[strlen(line)-1] = '\0';
            if (continuation_line) {
              var char* old_line = line;
              line = concat2(line,continuation_line);
              xfree(old_line);
              xfree(continuation_line);
            }
          }
          {
            var const char* condition;
            var long line_directive;
            if ((condition = is_if(line)) != NULL) {
              do_if(condition);
            } else if (is_else(line)) {
              do_else();
              line_repeat_else();
            } else if ((condition = is_elif(line)) != NULL) {
              do_elif(condition);
              line_repeat_else();
            } else if (is_endif(line)) {
              do_endif();
              line_repeat_endif();
            } else if ((line_directive = decode_line_directive(line)) >= 0)
              input_line = line_directive;
#ifdef SPLIT_OBJECT_INITIALIZATIONS
            else {
              # Replace "var object foo = ..." with "var object foo; foo = ..."
              # in macros as well.
              if (out.buffindex < MAXHEADERLEN) {
                out.buffer[out.buffindex] = '\0';
                var uintB* p;
                for (p = &out.buffer[token.startindex]; ; p++) {
                  p = (uintB*) strstr((char*)p,"var ");
                  if (p == NULL)
                    break;
                  if ((strncmp((char*)p,"var object ",strlen("var object "))==0
                       || strncmp((char*)p,"var chart ",strlen("var chart "))==0)
                      && (p[-1] == ' ' || p[-1] == '{')) {
                    if (strncmp((char*)p,"var object ",strlen("var object "))==0)
                      p += strlen("var object ");
                    else if (strncmp((char*)p,"var chart ",strlen("var chart "))==0)
                      p += strlen("var chart ");
                    var uintB* q = p;
                    if ((*q >= 'A' && *q <= 'Z') || (*q >= 'a' && *q <= 'z') || *q == '_') {
                      do
                        q++;
                      while ((*q >= 'A' && *q <= 'Z') || (*q >= 'a' && *q <= 'z') || (*q >= '0' && *q <= '9') || *q == '_');
                      while (*q == ' ')
                        q++;
                      if (*q == '=') {
                        var uintL insertlen = 2+(q-p);
                        if (out.buffindex + insertlen < MAXHEADERLEN) {
                          memmove(q+insertlen,q,&out.buffer[out.buffindex]-q+1);
                          q[0] = ';'; q[1] = ' ';
                          memcpy(q+2, p, q-p);
                          out.buffindex += insertlen;
                        }
                      }
                    }
                  }
                }
              }
            }
#endif
          }
          xfree(line);
        }
        goto separator;
      case '.':
        c = peek_char();
        if (!(((c>='0') && (c<='9')) || (c=='.')))
          goto separator;
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        # Digit. Continue reading as long as alphanumeric or '.'.
        loop {
          c = peek_char();
          if (((c>='0') && (c<='9')) || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z')) || (c=='.'))
            next_char();
          else
            break;
        }
        token.type = number;
        goto done;
      case '\'':
        # Character constant
        loop {
          c = next_char();
          if (c==EOF) {
            fprintf(stderr,"Unterminated character constant\n");
            break;
          }
          if (c=='\'')
            break;
          if (c=='\\')
            c = next_char();
        }
        token.type = charconst;
        goto done;
      case '\"':
        # String constant
        loop {
          c = next_char();
          if (c==EOF) {
            fprintf(stderr,"Unterminated string constant\n");
            break;
          }
          if (c=='\"')
            break;
          if (c=='\\')
            c = next_char();
        }
        token.type = stringconst;
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
        # Identifier.
        loop {
          c = peek_char();
          if (((c>='0') && (c<='9')) || ((c>='A') && (c<='Z')) || ((c>='a') && (c<='z')) || (c=='_'))
            next_char();
          else
            break;
        }
        token.type = ident;
        goto done;
      default:
      separator:
        token.type = sep;
        token.ch = c;
        goto done;
    }
  }
 done:
  token.endindex = out.buffindex;
  return token;
}

#define MAXBRACES 1000
local struct {
  uintL count;
  struct {
    uintB brace_type;
    uintL input_line;
    VectorString* condition;
    VectorVectorString* pending_conditions;
  } opening[MAXBRACES];
} open_braces;

static void convert (FILE* infp, FILE* outfp, const char* infilename)
{
  # Initialize input variables.
  infile = infp;
  input_line = 1;
  # Initialize output variables.
  outfile = outfp;
  # Initialize other variables.
  ifdef_stack = make_StackVectorString();
  ifdef_line_repeat = 0;
  # Go!
  if (infilename != NULL)
    fprintf(outfile,"#line 1 \"%s\"\n",infilename);
 {var boolean last_token_was_ident = FALSE;
#ifdef SPLIT_OBJECT_INITIALIZATIONS
  var boolean seen_var = FALSE;
  var boolean seen_var_object = FALSE;
  var boolean seen_var_object_ident = FALSE;
  var uintL last_ident_len = 0;
  var uintB last_ident_buf[256];
#endif
  loop {
    var Token token = next_token();
    switch (token.type) {
      case eof:
        if (open_braces.count > 0) {
          if (open_braces.count <= MAXBRACES) {
            fprintf(stderr,"Unclosed '%c' in line %lu\n",
                           open_braces.opening[open_braces.count-1].brace_type,
                           open_braces.opening[open_braces.count-1].input_line
                   );
          } else
            fprintf(stderr,"Unclosed '(' or '{' or '['\n");
        }
        return;
      case sep:
        switch (token.ch) {
          case '(': case '{': case '[':
            if (open_braces.count < MAXBRACES) {
              open_braces.opening[open_braces.count].brace_type = token.ch;
              open_braces.opening[open_braces.count].input_line = input_line;
              if (token.ch == '{') {
                open_braces.opening[open_braces.count].condition = current_condition();
                open_braces.opening[open_braces.count].pending_conditions = make_VectorVectorString();
              }
            }
            open_braces.count++;
            break;
          case ')': case '}': case ']':
            if (open_braces.count > 0) {
              open_braces.count--;
              if (open_braces.count < MAXBRACES) {
                var uintL opening_line = open_braces.opening[open_braces.count].input_line;
                var uintL closing_line = input_line;
                var uintB opening_ch = open_braces.opening[open_braces.count].brace_type;
                var uintB closing_ch = token.ch;
                if (!(   ((opening_ch == '(') && (closing_ch == ')'))
                      || ((opening_ch == '{') && (closing_ch == '}'))
                      || ((opening_ch == '[') && (closing_ch == ']'))
                   ) )
                  fprintf(stderr,"Opening delimiter '%c' in line %lu\n and closing delimiter '%c' in line %lu\n don't match.\n",
                                 opening_ch,opening_line,
                                 closing_ch,closing_line
                         );
                if ((opening_ch == '{') && (closing_ch == '}')) {
                  var const VectorString* opening_condition = open_braces.opening[open_braces.count].condition;
                  var const VectorString* closing_condition = current_condition();
                  if (VectorString_equals(opening_condition,closing_condition)) {
                    var const VectorVectorString* conditions = open_braces.opening[open_braces.count].pending_conditions;
                    var boolean did_newline = FALSE;
                    var boolean in_fresh_line = FALSE;
                    var uintL i;
                    for (i = VectorVectorString_length(conditions); i > 0; ) {
                      var const VectorString* condition = VectorVectorString_element(conditions,--i);
                      condition = modulo_current_condition(condition);
                      if (!is_true_condition_part(condition)) {
                        if (!in_fresh_line) {
                          fprintf(outfile,"\n");
                          in_fresh_line = TRUE;
                        }
                        fprintf(outfile,"#if ");
                        print_condition_part(outfile,condition);
                        fprintf(outfile,"\n}\n#endif\n");
                        did_newline = TRUE;
                      } else {
                        fprintf(outfile,"}");
                        in_fresh_line = FALSE;
                      }
                    }
                    if (did_newline) {
                      if (!in_fresh_line) {
                        fprintf(outfile,"\n");
                        in_fresh_line = TRUE;
                      }
                      line_emit();
                    }
                  } else {
                    fprintf(stderr,"Opening brace '%c' in line %lu: #if ",opening_ch,opening_line);
                    print_condition_part(stderr,opening_condition);
                    fprintf(stderr,"\n and closing brace '%c' in line %lu: #if ",closing_ch,closing_line);
                    print_condition_part(stderr,closing_condition);
                    fprintf(stderr,"\n don't match.\n");
                  }
                }
              }
            } else {
              fprintf(stderr,"No opening delimiter for closing delimiter '%c' in line %lu\n",
                             token.ch,input_line
                     );
            }
            break;
          default:
            break;
        }
#ifdef SPLIT_OBJECT_INITIALIZATIONS
        if (token.ch == '=' && seen_var_object_ident) {
          out.buffer[token.startindex] = ';';
          outbuffer_off();
          fwrite(last_ident_buf,1,last_ident_len,outfile);
          fputs(" =",outfile);
        }
        seen_var = FALSE;
        seen_var_object = FALSE;
        seen_var_object_ident = FALSE;
#endif
        break;
      case ident:
#ifdef SPLIT_OBJECT_INITIALIZATIONS
        if ((token.endindex - token.startindex == 3)
            && (out.buffer[token.startindex  ] == 'v')
            && (out.buffer[token.startindex+1] == 'a')
            && (out.buffer[token.startindex+2] == 'r')) {
          seen_var = TRUE;
          seen_var_object = FALSE;
          seen_var_object_ident = FALSE;
        } else if (seen_var
                   && (((token.endindex - token.startindex == 6)
                        && (out.buffer[token.startindex  ] == 'o')
                        && (out.buffer[token.startindex+1] == 'b')
                        && (out.buffer[token.startindex+2] == 'j')
                        && (out.buffer[token.startindex+3] == 'e')
                        && (out.buffer[token.startindex+4] == 'c')
                        && (out.buffer[token.startindex+5] == 't'))
                       || ((token.endindex - token.startindex == 5)
                           && (out.buffer[token.startindex  ] == 'c')
                           && (out.buffer[token.startindex+1] == 'h')
                           && (out.buffer[token.startindex+2] == 'a')
                           && (out.buffer[token.startindex+3] == 'r')
                           && (out.buffer[token.startindex+4] == 't')))) {
          seen_var = FALSE;
          seen_var_object = TRUE;
          seen_var_object_ident = FALSE;
        } else if (seen_var_object
                   && (token.endindex - token.startindex <= sizeof(last_ident_buf))) {
          seen_var = FALSE;
          seen_var_object = FALSE;
          seen_var_object_ident = TRUE;
          last_ident_len = token.endindex - token.startindex;
          memcpy(last_ident_buf,&out.buffer[token.startindex],last_ident_len);
        }
#endif
        if (!last_token_was_ident # to avoid cases like "local var x = ...;"
            && (token.endindex - token.startindex == 3)
            && (out.buffer[token.startindex  ] == 'v')
            && (out.buffer[token.startindex+1] == 'a')
            && (out.buffer[token.startindex+2] == 'r')
           ) {
          var uintL braceindex;
          for (braceindex = open_braces.count; braceindex > 0; ) {
            braceindex--;
            if (open_braces.opening[braceindex].brace_type == '{') {
              VectorVectorString_add(open_braces.opening[braceindex].pending_conditions,current_condition());
              outbuffer_off_insert(token.startindex,"{");
              break;
            }
          }
        }
        break;
      default:
#ifdef SPLIT_OBJECT_INITIALIZATIONS
        seen_var = FALSE;
        seen_var_object = FALSE;
        seen_var_object_ident = FALSE;
#endif
        break;
    }
    outbuffer_off();
    last_token_was_ident = (token.type == ident);
  }
}}

int main (int argc, char* argv[])
{
  var char* infilename;
  var FILE* infile;
  var FILE* outfile;
  # Argument parsing.
  if (argc == 2) {
    infilename = argv[1];
    infile = fopen(infilename,"r");
    if (infile == NULL)
      exit(1);
    input_filename = infilename;
  } else if (argc == 1) {
    infilename = NULL;
    infile = stdin;
  } else
    exit(1);
  outfile = stdout;
  # Main job.
  convert(infile,outfile,infilename);
  # Clean up.
  if (ferror(infile) || ferror(outfile) || fclose(outfile))
    exit(1);
  exit(0);
}

