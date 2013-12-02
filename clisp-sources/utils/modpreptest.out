/* Sample -*- C -*- module */
#line 4
DEFMODULE(sample,"USER")
#define O(varname) module__sample__object_tab._##varname

struct {
#if ((defined(cond0)) && (defined(cond1))) || (cond2a ? cond2b : cond2c)
  object _object_Kgoto;
#endif
#if (defined(cond0)) && (defined(cond1))
  object _object__23_28_29;
#endif
#if (defined(cond0)) && (defined(cond1))
  object _object__28foo2_29;
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  object _object_Kotog;
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  object _object__28foo3_29;
#endif
  object _object_Ktest;
  object _object_Ktest_2Dnot;
#if (!(cond2a ? cond2b : cond2c)) && (cond3)
  object _object__23_5Cspace;
#endif
  object _var1;
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  object _var2;
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  object _var3;
#endif
} module__sample__object_tab;
#define object_Kgoto  module__sample__object_tab._object_Kgoto
#define object__23_28_29  module__sample__object_tab._object__23_28_29
#define object__28foo2_29  module__sample__object_tab._object__28foo2_29
#define object_Kotog  module__sample__object_tab._object_Kotog
#define object__28foo3_29  module__sample__object_tab._object__28foo3_29
#define object_Ktest  module__sample__object_tab._object_Ktest
#define object_Ktest_2Dnot  module__sample__object_tab._object_Ktest_2Dnot
#define object__23_5Cspace  module__sample__object_tab._object__23_5Cspace
uintC module__sample__object_tab_size = sizeof(module__sample__object_tab)/sizeof(object);

struct {
#if ((defined(cond0)) && (defined(cond1))) || (cond2a ? cond2b : cond2c)
  object_initdata_t _object_Kgoto;
#endif
#if (defined(cond0)) && (defined(cond1))
  object_initdata_t _object__23_28_29;
#endif
#if (defined(cond0)) && (defined(cond1))
  object_initdata_t _object__28foo2_29;
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  object_initdata_t _object_Kotog;
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  object_initdata_t _object__28foo3_29;
#endif
  object_initdata_t _object_Ktest;
  object_initdata_t _object_Ktest_2Dnot;
#if (!(cond2a ? cond2b : cond2c)) && (cond3)
  object_initdata_t _object__23_5Cspace;
#endif
  object_initdata_t _var1;
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  object_initdata_t _var2;
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  object_initdata_t _var3;
#endif
  int _dummy_to_avoid_trailing_comma_in_initializer;
} module__sample__object_tab_initdata = {
#if ((defined(cond0)) && (defined(cond1))) || (cond2a ? cond2b : cond2c)
  { ":GOTO" },
#endif
#if (defined(cond0)) && (defined(cond1))
  { "#()" },
#endif
#if (defined(cond0)) && (defined(cond1))
  { "(foo2)" },
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  { ":OTOG" },
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  { "(foo3)" },
#endif
  { ":test" },
  { ":test-not" },
#if (!(cond2a ? cond2b : cond2c)) && (cond3)
  { "#\\space" },
#endif
  { "NIL" },
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  { "NIL" },
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  { "NIL" },
#endif
  0
};

#line 10
 DEFVAR (var1)
#ifdef cond0
 DEFUN (mypack:fun1, x,(subr_mypack__fun1,1,0,norest,nokey,0,NIL))
#if defined(cond1)
   push( object_Kgoto);
 DEFUN (mypack:fun2, x,(subr_mypack__fun2,1,0,norest,nokey,0,NIL))
   push(object__23_28_29);
   push( object_Kgoto);
 DEFVAR (var2)
 DEFVAR (var3)
#else
 DEFUN (mypack:fun2, y,(subr_mypack__fun2,1,0,norest,nokey,0,NIL))
   push(object_Kotog);
 DEFVAR (var3)
 DEFVAR (var2)
#endif
#endif
DEFUN (mypack:fun3, x y &optional z,(subr_mypack__fun3,2,1,norest,nokey,0,NIL))
DEFUN (mypack:fun4, x y &key test test-not,(subr_mypack__fun4,2,0,norest,key,2,NIL))
DEFUN (fun5, x y &rest r,(subr_user__fun5,2,0,rest,nokey,0,NIL))
#if cond2a ? cond2b : cond2c
   push(object_Kgoto);
#elif cond3
   push(object__23_5Cspace);
#endif

struct {
  VAROBJECTS_ALIGNMENT_DUMMY_DECL
#if defined(cond0)
  subr_ _subr_mypack__fun1;
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  subr_t _subr_mypack__fun2;
#endif
  subr_t _subr_mypack__fun3;
  subr_t _subr_mypack__fun4;
  subr_t _subr_user__fun5;
  int _dummy_to_avoid_trailing_comma_in_initializer;
} module__sample__subr_tab = {
  #if varobjects_misaligned
  { 0 },
  #endif
  {
#if defined(cond0)
    LISPFUN_F(subr_mypack__fun1,1,0,norest,nokey,0,NIL)
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
    LISPFUN_F(subr_mypack__fun2,1,0,norest,nokey,0,NIL)
#endif
    LISPFUN_F(subr_mypack__fun3,2,1,norest,nokey,0,NIL)
    LISPFUN_F(subr_mypack__fun4,2,0,norest,key,2,NIL)
    LISPFUN_F(subr_user__fun5,2,0,rest,nokey,0,NIL)
    0
  }
};
uintC module__sample__subr_tab_size = (sizeof(struct module__sample__subr_tab_t)-sizeof(int))/sizeof(subr_t);

struct {
#if defined(cond0)
  subr_initdata_t _subr_mypack__fun1;
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  subr_initdata_t _subr_mypack__fun2;
#endif
  subr_initdata_t _subr_mypack__fun3;
  subr_initdata_t _subr_mypack__fun4;
  subr_initdata_t _subr_user__fun5;
  int _dummy_to_avoid_trailing_comma_in_initializer;
} module__sample__subr_tab_initdata = {
#if defined(cond0)
  { "mypack", "fun1" },
#endif
#if ((defined(cond0)) && (defined(cond1))) || ((defined(cond0)) && (!(defined(cond1))))
  { "mypack", "fun2" },
#endif
  { "mypack", "fun3" },
  { "mypack", "fun4" },
  { "USER", "fun5" },
  0
};

void module__sample__init_function_1 (module_t* module)
{
  pushSTACK(object_Ktest);
  pushSTACK(object_Ktest_2Dnot);
  module__sample__subr_tab._subr_mypack__fun4.keywords = vectorof(2);
  O(var1) = ( NIL);
#if (defined(cond0)) && (defined(cond1))
  O(var2) = ( object__28foo2_29);
#endif
#if (defined(cond0)) && (defined(cond1))
  O(var3) = ( O(var2));
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  O(var3) = ( object__28foo3_29);
#endif
#if (defined(cond0)) && (!(defined(cond1)))
  O(var2) = ( O(var3));
#endif
}

void module__sample__init_function_2 (module_t* module)
{
}
