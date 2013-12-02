/*
 * Export CLISP internals for modules
 * Bruno Haible 1994-2005
 * Sam Steingold 1998-2005
 */

#include "lispbibl.c"

/*
 * Printing of strings with embedded numbers, like with printf().
 * The major difference is that the numbers can also be of type
 * 'unsigned long long' (which printf() does not support in a portable way).
 * We don't even need to assume the existence of <stdarg.h>.
 */

typedef struct {
  char base; /* 'd' for decimal, 'x' for hexadecimal */
  int size;
  union {
    uint8 val8;
    uint16 val16;
    uint32 val32;
    #ifdef HAVE_LONGLONG
    uint64 val64;
    #endif
  } value;
} printf_arg;

#ifdef HAVE_LONGLONG
  #define fill_printf_arg(where,expr)  \
    where.size = sizeof(expr); \
    if (sizeof(expr) == sizeof(uint8)) { where.value.val8 = (uint8)(expr); } \
    else if (sizeof(expr) == sizeof(uint16)) { where.value.val16 = (uint16)(expr); } \
    else if (sizeof(expr) == sizeof(uint32)) { where.value.val32 = (uint32)(expr); } \
    else if (sizeof(expr) == sizeof(uint64)) { where.value.val64 = (uint64)(expr); } \
    else abort();
#else
  #define fill_printf_arg(where,expr)  \
    where.size = sizeof(expr); \
    if (sizeof(expr) == sizeof(uint8)) { where.value.val8 = (uint8)(expr); } \
    else if (sizeof(expr) == sizeof(uint16)) { where.value.val16 = (uint16)(expr); } \
    else if (sizeof(expr) == sizeof(uint32)) { where.value.val32 = (uint32)(expr); } \
    else abort();
#endif

static const char* Lsuffix = "L";
static const char* ULsuffix = "UL";
#ifdef HAVE_LONGLONG
static const char* ULLsuffix = "ULL";
#endif

static void print_printf_arg (const printf_arg* arg)
{
  switch (arg->size) {
    case sizeof(uint8):
      printf(arg->base=='d' ? "%u" : "0x%X", (unsigned int)(arg->value.val8));
      break;
    case sizeof(uint16):
      printf(arg->base=='d' ? "%u" : "0x%X", (unsigned int)(arg->value.val16));
      break;
    case sizeof(uint32):
      printf(arg->base=='d' ? "%lu%s" : "0x%lX%s", (unsigned long)(arg->value.val32), ULsuffix);
      break;
   #ifdef HAVE_LONGLONG
    case sizeof(uint64):
     #if (long_bitsize == 64)
      if (!(sizeof(uint64) == sizeof(unsigned long))) abort();
      printf("0x%lX%s", (unsigned long)(arg->value.val64), ULsuffix);
     #else
      if (!(sizeof(uint32) == sizeof(unsigned long))) abort();
      printf("0x%lX%08lX%s",
             (unsigned long)(arg->value.val64 >> 32),
             (unsigned long)(arg->value.val64 & 0xFFFFFFFFUL),
             ULLsuffix);
     #endif
      break;
   #endif
    default:
      abort();
  }
}

static void printf_with_args (const char* string, int argcount,
                              printf_arg* args)
{
  while (*string) {
    if (string[0]=='%') {
      if (!(string[1]=='d' || string[1]=='x')) abort();
      if (!(argcount > 0)) abort();
      args->base = string[1]; print_printf_arg(args);
      string+=2; args++; argcount--;
    } else {
      putchar(*string); string++;
    }
  }
}

#define printf0(string)  printf(string)
#define printf1(string,arg0)  \
  { var printf_arg args[1]; \
    fill_printf_arg(args[0],arg0); \
    printf_with_args(string,1,args); \
  }
#define printf2(string,arg0,arg1)  \
  { var printf_arg args[2]; \
    fill_printf_arg(args[0],arg0); \
    fill_printf_arg(args[1],arg1); \
    printf_with_args(string,2,args); \
  }
#define printf3(string,arg0,arg1,arg2)  \
  { var printf_arg args[3]; \
    fill_printf_arg(args[0],arg0); \
    fill_printf_arg(args[1],arg1); \
    fill_printf_arg(args[2],arg2); \
    printf_with_args(string,3,args); \
  }
#define printf4(string,arg0,arg1,arg2,arg3)  \
  { var printf_arg args[4]; \
    fill_printf_arg(args[0],arg0); \
    fill_printf_arg(args[1],arg1); \
    fill_printf_arg(args[2],arg2); \
    fill_printf_arg(args[3],arg3); \
    printf_with_args(string,4,args); \
  }
#define printf5(string,arg0,arg1,arg2,arg3,arg4)  \
  { var printf_arg args[5]; \
    fill_printf_arg(args[0],arg0); \
    fill_printf_arg(args[1],arg1); \
    fill_printf_arg(args[2],arg2); \
    fill_printf_arg(args[3],arg3); \
    fill_printf_arg(args[4],arg4); \
    printf_with_args(string,5,args); \
  }
#define printf6(string,arg0,arg1,arg2,arg3,arg4,arg5)  \
  { var printf_arg args[6]; \
    fill_printf_arg(args[0],arg0); \
    fill_printf_arg(args[1],arg1); \
    fill_printf_arg(args[2],arg2); \
    fill_printf_arg(args[3],arg3); \
    fill_printf_arg(args[4],arg4); \
    fill_printf_arg(args[5],arg5); \
    printf_with_args(string,6,args); \
  }
#define printf7(string,arg0,arg1,arg2,arg3,arg4,arg5,arg6)  \
  { var printf_arg args[7]; \
    fill_printf_arg(args[0],arg0); \
    fill_printf_arg(args[1],arg1); \
    fill_printf_arg(args[2],arg2); \
    fill_printf_arg(args[3],arg3); \
    fill_printf_arg(args[4],arg4); \
    fill_printf_arg(args[5],arg5); \
    fill_printf_arg(args[6],arg6); \
    printf_with_args(string,7,args); \
  }

static void print_file (const char* fname) {
  char buf[BUFSIZ];
  FILE* includefile = fopen(fname,"r");
  char* line;
  if (includefile == NULL) { perror(fname); exit(1); }
  while ((line = fgets(buf,BUFSIZ,includefile)) != NULL)
    fputs(line,stdout);
  if (ferror(includefile) || fclose(includefile)) { perror(fname); exit(1); }
}

static FILE *header_f = NULL, *test_f = NULL;
static unsigned int test_count = 0, typedef_count = 0, define_count = 0;

static void emit_typedef_test (const char *new_type) {
  fprintf(test_f,"  printf(\"sizeof(%s)=%%d\\n\",sizeof(%s));\n",
          new_type,new_type);
  test_count++;
}

static void emit_typedef (const char* def, const char* new_type) {
  fprintf(header_f,"typedef %s %s;\n",def,new_type);
  typedef_count++;
  if (test_f) emit_typedef_test(new_type);
}

static void emit_typedef_f (const char* format, const char* new_type) {
  fputs("typedef ",header_f);
  fprintf(header_f,format,new_type);
  fputs(";\n",header_f);
  typedef_count++;
  if (test_f) emit_typedef_test(new_type);
}

static void emit_define_test (const char* form, const char* definition) {
  fprintf(test_f,"  printf(\"%s=%%s\\n\",STRINGIFY(%s));\n",form,definition);
  test_count++;
}

static void emit_define (const char* form, const char* definition) {
  fprintf(header_f,"#define %s %s\n",form,definition);
  define_count++;
  if (test_f) emit_define_test(form,definition);
}

/* this cannot be used on X whose definition includes ## ! */
#define export_def(x)  puts("#define " #x "  " STRING(x))
#define export_literal(x)  puts(STRING(x))

int main(int argc, char* argv[])
{
  char buf[BUFSIZ];

  header_f = stdout;
  if (argc == 2) {              /* open the test file and start it */
    test_f = fopen(argv[1],"w");
    if (test_f == NULL) { perror(argv[1]); exit(1); }
    fprintf(stderr,"writing test file %s\n",argv[1]);
    fprintf(test_f,"/* generated by %s on %s %s */\n"
            "#if USE_CLISP_H\n#include \"clisp.h\"\n#else\n"
            "#include \"lispbibl.c\"\n#endif\n#include <stdio.h>\n\n"
            "int main () {\n",
            __FILE__,__DATE__,__TIME__);
  }

  printf("#define SAFETY %d\n",SAFETY);
 #if defined(UNICODE)
  printf("#define CLISP_UNICODE 1\n");
 #else
  printf("#define CLISP_UNICODE 0\n");
 #endif

  /* The definitions are extracted from lispbibl.d. */
#include "gen.lispbibl.c"

   printf("#define LISPFUNN(name,req_anz)  LISPFUN(name,sec,req_anz,0,norest,nokey,0,NIL)\n");
   /* In LISPFUN_B, emit the decl first, to avoid "gcc -missing-declarations" warnings. */
   printf("#define LISPFUN_B(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  Values C_##name subr_##rest_flag##_function_args; Values C_##name subr_##rest_flag##_function_args\n");
   printf("#define subr_norest_function_args  (void)\n");
   printf("#define subr_rest_function_args  (uintC argcount, object* rest_args_pointer)\n");
 #ifdef TYPECODES
  #ifdef DEBUG_GCSAFETY
   printf4("#define LISPFUN_F(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  { gcv_nullobj, %d,%d,%d,%d, gcv_nullobj, gcv_nullobj, (lisp_function_t)(&C_##name), 0, req_anz, opt_anz, (uintB)subr_##rest_flag, (uintB)subr_##key_flag, key_anz, sec},\n", Rectype_Subr, 0, subr_length, subr_xlength);
  #else
   printf4("#define LISPFUN_F(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  { { gcv_nullobj }, %d,%d,%d,%d, gcv_nullobj, gcv_nullobj, (lisp_function_t)(&C_##name), 0, req_anz, opt_anz, (uintB)subr_##rest_flag, (uintB)subr_##key_flag, key_anz, sec},\n", Rectype_Subr, 0, subr_length, subr_xlength);
  #endif
 #else
   printf1("#define LISPFUN_F(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)  { gcv_nullobj, %d, gcv_nullobj, gcv_nullobj, (lisp_function_t)(&C_##name), 0, req_anz, opt_anz, (uintB)subr_##rest_flag, (uintB)subr_##key_flag, key_anz, sec},\n", xrecord_tfl(Rectype_Subr,0,subr_length,subr_xlength));
 #endif
   printf("#define LISPFUN  LISPFUN_B\n");

  /* Note: The following inline/macro definitions are _not_ in lispbibl.d! */

  printf("#ifndef COMPILE_STANDALONE\n");
  printf("static inline unsigned int check_uint_defaulted (object obj, unsigned int defolt) {"
          " return missingp(obj) ? defolt : I_to_uint(check_uint(obj)); "
         "}\n");
  printf("#endif\n");
  printf("#define check_uint_default0(obj) check_uint_defaulted(obj,0)\n");

#if defined(UNIX_CYGWIN32)
  printf("#ifndef COMPILE_STANDALONE\n");
  printf("static inline object convert_time_to_universal_w32 (const FILETIME* w32_time) {\n");
  printf("  time_t unix_time = time_t_from_filetime(w32_time);\n");
  printf("  return convert_time_to_universal(&unix_time);\n");
  printf("}\n");
  printf("static inline void convert_time_from_universal_w32 (object universal, FILETIME* w32_time) {\n");
  printf("  time_t unix_time;\n");
  printf("  convert_time_from_universal(universal,&unix_time);");
  printf("  time_t_to_filetime(unix_time,w32_time);\n");
  printf("}\n");
  printf("#endif\n");
#endif
#if defined(WIN32_NATIVE)
  printf("#define convert_time_to_universal_w32 convert_time_to_universal\n");
  printf("#define convert_time_from_universal_w32 convert_time_from_universal\n");
#endif

  /* avoid some stupid warnings */
  printf("#undef PACKAGE_BUGREPORT\n");
  printf("#undef PACKAGE_NAME\n");
  printf("#undef PACKAGE_STRING\n");
  printf("#undef PACKAGE_TARNAME\n");
  printf("#undef PACKAGE_VERSION\n");
  /* Additional stuff for modules. */
  printf("#define DEFMODULE(module_name,package_name)\n");
  printf("#define DEFUN(funname,lambdalist,signature) LISPFUN signature\n");
  printf("#define DEFUNF DEFUN\n");
  printf("#define DEFUNN DEFUN\n");
  printf("#define DEFUNR DEFUN\n");
  printf("#define DEFUNW DEFUN\n");
  printf("#define DEFUND DEFUN\n");
  printf("#define DEFVAR(varname)\n");
  /* done - check for errors, close test files &c */
  if (ferror(stdout)) exit(1);
  if (ferror(header_f)) exit(1);
  if (test_f) {
    fprintf(test_f,"  return 0;\n}\n");
    if (ferror(test_f)) exit(1);
    fclose(test_f);
    fprintf(stderr,"wrote %d tests (%d typedefs, %d defines)\n",
            test_count,typedef_count,define_count);
  }
  exit(0);
}
