/*
 * Special Forms, Control Structures, Evaluator Related Stuff for CLISP
 * Bruno Haible 1990-2005
 * Sam Steingold 1998-2005
 * German comments translated into English: Stefan Kain 2002-09-28
 */

#include "lispbibl.c"

/* (SYSTEM::%EXIT [errorp]) leaves the system */
LISPFUN(exit,seclass_default,0,1,norest,nokey,0,NIL) {
  var object errorp = STACK_0;
  final_exitcode = missingp(errorp) ? 0 :
                   (uint_p(errorp) ? I_to_uint(errorp) : 1);
  quit();
}

LISPSPECFORM(eval_when, 1,0,body)
{ /* (EVAL-WHEN ({situation}) {form}), CLTL p. 69 */
  var object situations = STACK_1; /* list of situations */
  /* search symbol EVAL or list (NOT COMPILE) in it: */
  while (consp(situations)) {
    var object situation = Car(situations);
    if (eq(situation,S(eval)) /* symbol EVAL found? */
        || eq(situation,S(Kexecute)))
      goto found;
    if (consp(situation) && eq(Car(situation),S(not))) {
      situation = Cdr(situation);
      if (consp(situation) && nullp(Cdr(situation))
          && (eq(Car(situation),S(compile)) /* list (NOT COMPILE) found? */
              || eq(Car(situation),S(Kcompile_toplevel))))
        goto found;
    }
    situations = Cdr(situations);
  }
  /* symbol EVAL not found */
  VALUES1(NIL);
  skipSTACK(2);
  return;
 found: { /* symbol EVAL found */
    var object body = popSTACK();
    skipSTACK(1);
    implicit_progn(body,NIL); /* evaluate body */
  }
}

LISPSPECFORM(quote, 1,0,nobody)
{ /* (QUOTE object) == 'object, CLTL p. 86 */
  VALUES1(popSTACK()); /* argument as value */
}

LISPSPECFORM(function, 1,1,nobody)
{ /* (FUNCTION funname), CLTL. p. 87
 either (FUNCTION symbol)
     or (FUNCTION (LAMBDA . lambdabody))
     or (FUNCTION name (LAMBDA . lambdabody)) */
  if (!boundp(STACK_0)) { /* 1 argument */
    var object name = STACK_1;
    if (consp(name) && eq(Car(name),S(lambda))) {
      VALUES1(get_closure(Cdr(name),S(Klambda),false,&aktenv));
    } else {
      STACK_1 = check_funname(source_program_error,S(function),STACK_1);
      var object fun = sym_function(STACK_1,aktenv.fun_env);
      if (!functionp(fun)) {
        if (functionmacrop(fun))
          fun = TheFunctionMacro(fun)->functionmacro_function;
        else
          fun = check_fdefinition(STACK_1,S(function));
      }
      VALUES1(fun);
    }
  } else { /* 2 arguments */
    STACK_1 = check_funname(source_program_error,S(function),STACK_1);
    while (!(consp(STACK_0) && eq(Car(STACK_0),S(lambda)))) {
      pushSTACK(NIL); /* no PLACE */
      pushSTACK(STACK_1); /* SOURCE-PROGRAM-ERROR slot DETAIL */
      pushSTACK(STACK_0); pushSTACK(S(function));
      check_value(source_program_error,
                  GETTEXT("~S: ~S should be a lambda expression"));
      STACK_0 = value1;
    }
    VALUES1(get_closure(Cdr(STACK_0),STACK_1,false,&aktenv));
  }
  skipSTACK(2);
}

/* error-message, if a symbol has no value.
 > symbol: symbol
 < value1: bound value
 can trigger GC */
local maygc void check_global_symbol_value (object symbol) {
  value1 = Symbol_value(symbol);
  if (!boundp(value1)) {
    do {
      pushSTACK(symbol); /* save */
      pushSTACK(symbol); /* PLACE */
      pushSTACK(symbol); /* CELL-ERROR Slot NAME */
      pushSTACK(symbol);
      pushSTACK(TheSubr(subr_self)->name);
      check_value(unbound_variable,GETTEXT("~S: ~S has no dynamic value"));
      symbol = popSTACK(); /* restore */
    } while (!boundp(value1));
    if (!nullp(value2)) /* STORE-VALUE */
      Symbol_value(symbol) = value1;
  }
}

LISPFUNNR(symbol_value,1)
{ /* (SYMBOL-VALUE symbol), CLTL p. 90 */
  var object symbol = check_symbol(popSTACK());
  check_global_symbol_value(symbol); /* value1 <- Symbol_value */
  mv_count=1;
}

LISPFUNNR(symbol_function,1)
{ /* (SYMBOL-FUNCTION symbol), CLTL p. 90 */
  var object symbol = check_symbol(popSTACK());
  var object val = Symbol_function(symbol);
  if (!boundp(val))
    val = check_fdefinition(symbol,S(symbol_function));
  VALUES1(val);
}

/* UP: just like GET-FUNNAME-SYMBOL (see init.lisp),
 except that it does not create the new symbol when there is none yet
 and does not issue a warning when the SETF symbol is shadowed
 can trigger GC */
local maygc object funname_to_symbol (object symbol) {
  if (!funnamep(symbol))
    symbol = check_funname_replacement(type_error,TheSubr(subr_self)->name,symbol);
  if (!symbolp(symbol)) /* (get ... 'SYS::SETF-FUNCTION) */
    symbol = get(Car(Cdr(symbol)),S(setf_function));
  return symbol;
}

LISPFUNNR(fdefinition,1)
{ /* (FDEFINITION funname), CLTL2 p. 120 */
  var object symbol = funname_to_symbol(STACK_0);
  if (!symbolp(symbol))
    VALUES1(check_fdefinition(STACK_0,TheSubr(subr_self)->name));
  else {
    var object val = Symbol_function(symbol);
    if (!boundp(val))
      val = check_fdefinition(STACK_0,TheSubr(subr_self)->name);
    VALUES1(val);
  }
  skipSTACK(1);
}

LISPFUNNR(boundp,1)
{ /* (BOUNDP symbol), CLTL p. 90 */
  var object symbol = check_symbol(popSTACK());
  VALUES_IF(boundp(Symbol_value(symbol)));
}

LISPFUNNR(fboundp,1)
{ /* (FBOUNDP symbol), CLTL p. 90, CLTL2 p. 120 */
  var object symbol = funname_to_symbol(popSTACK());
  VALUES_IF(symbolp(symbol) && /* should be a symbol */
            boundp(Symbol_function(symbol)));
}

LISPFUNNF(special_operator_p,1)
{ /* (SPECIAL-OPERATOR-P symbol), was (SPECIAL-FORM-P symbol), CLTL p. 91 */
  var object symbol = check_symbol(popSTACK());
  var object obj = Symbol_function(symbol);
  VALUES_IF(fsubrp(obj));
}

/* UP: Check the body of a SETQ- or PSETQ-form.
 > caller: Caller (a symbol)
 > STACK_0: Body
 < result: true if symbol-macros have to be expanded.
 can trigger GC */
local maygc bool check_setq_body (object caller) {
  pushSTACK(STACK_0); /* save body */
  while (consp(STACK_0)) {
    var object sym = check_symbol_non_constant(Car(STACK_0),caller);
    Car(STACK_0) = sym;
    if (sym_macrop(sym)) {
      skipSTACK(1); /* drop body */
      return true;
    }
    STACK_0 = Cdr(STACK_0);
    if (atomp(STACK_0)) {
      if (!nullp(STACK_0))
        goto fehler_dotted;
      /* STACK_0 == SOURCE-PROGRAM-ERROR slot DETAIL */
      pushSTACK(STACK_1); pushSTACK(TheFsubr(subr_self)->name);
      fehler(source_program_error,
             GETTEXT("~S: odd number of arguments: ~S"));
    }
    STACK_0 = Cdr(STACK_0);
  }
  /* body is finished. */
  if (!nullp(STACK_0)) {
   fehler_dotted: /* The whole body is still in STACK_0. */
    /* STACK_0 == SOURCE-PROGRAM-ERROR slot DETAIL */
    pushSTACK(STACK_1); pushSTACK(TheFsubr(subr_self)->name);
    fehler(source_program_error,GETTEXT("dotted list given to ~S : ~S"));
  }
  skipSTACK(1); /* drop body */
  return false;
}

LISPSPECFORM(setq, 0,0,body)
{ /* (SETQ {var form}), CLTL p. 91 */
  if (check_setq_body(S(setq))) {
    var object form = allocate_cons();
    Car(form) = S(setf); Cdr(form) = popSTACK(); /* turn SETQ into SETF */
    eval(form);
  } else {
    var object body = popSTACK();
    if (consp(body)) {
      do {
        var object symbol = Car(body); /* variable */
        body = Cdr(body);
        pushSTACK(Cdr(body)); /* save remaining list */
        pushSTACK(symbol); /* save symbol */
        eval(Car(body)); /* evaluate next form */
        symbol = popSTACK();
        value1 = setq(symbol,value1); /* execute assignment */
        body = popSTACK();
      } while (consp(body));
      /* value1 is the last evaluation result. */
    } else
      value1 = NIL; /* default value at (SETQ) */
    mv_count=1;
  }
}

LISPSPECFORM(psetq, 0,0,body)
{ /* (PSETQ {var form}), CLTL p. 92 */
  if (check_setq_body(S(psetq))) {
    var object form = allocate_cons();
    Car(form) = S(psetf); Cdr(form) = popSTACK(); /* turn PSETQ into PSETF */
    eval(form);
  } else {
    var object body = popSTACK();
    var uintL body_length = llength(body)/2; /* number of pairs (var form) */
    if (body_length > 0) {
      get_space_on_STACK(body_length*2*sizeof(gcv_object_t));
      {
        var uintL count = body_length;
        do {
          pushSTACK(Car(body)); /* push variable on stack */
          body = Cdr(body);
          pushSTACK(Cdr(body)); /* remaining list on stack */
          eval(Car(body)); /* evaluate next form */
          body = STACK_0;
          STACK_0 = value1; /* its result in the stack */
        } while (--count);
      }
      {
        var uintL count = body_length;
        do {
          var object val = popSTACK(); /* value */
          var object sym = popSTACK(); /* symbol */
          setq(sym,val); /* execute assignment */
        } while (--count);
      }
    }
    VALUES1(NIL);
  }
}

/* (SETF (SYMBOL-VALUE symbol) value) = (SET symbol value), CLTL p. 92 */
LISPFUNN(set,2)
{
  var object symbol = check_symbol_non_constant(STACK_1,S(set));
  VALUES1(Symbol_value(symbol) = STACK_0);
  skipSTACK(2);
}

LISPFUNN(makunbound,1)
{ /* (MAKUNBOUND symbol), CLTL p. 92 */
  var object symbol = check_symbol_non_constant(popSTACK(),S(makunbound));
  Symbol_value(symbol) = unbound;
  VALUES1(symbol);
}

LISPFUNN(fmakunbound,1)
{ /* (FMAKUNBOUND symbol), CLTL p. 92, CLTL2 p. 123 */
  var object symbol = funname_to_symbol(STACK_0);
  var object funname = popSTACK();
  if (!symbolp(symbol)) /* should be a symbol */
      goto undef; /* otherwise undefined */
  {
    var object obj = Symbol_function(symbol);
    if (fsubrp(obj)) {
      pushSTACK(symbol);
      pushSTACK(S(fmakunbound));
      fehler(error,GETTEXT("~S: the special operator definition of ~S must not be removed"));
    }
  }
  Symbol_function(symbol) = unbound;
 undef:
  VALUES1(funname);
}

LISPFUN(apply,seclass_default,2,0,rest,nokey,0,NIL)
{ /* (APPLY function {arg} arglist), CLTL p. 107 */
  BEFORE(rest_args_pointer);
  apply(Before(rest_args_pointer), /* function */
        argcount, /* number of {arg} on the stack */
        popSTACK()); /* arglist */
  skipSTACK(1); /* remove function from the stack */
}

LISPFUN(funcall,seclass_default,1,0,rest,nokey,0,NIL)
{ /* (FUNCALL function {arg}), CLTL p. 108 */
  funcall(Before(rest_args_pointer),argcount); skipSTACK(1);
}

LISPSPECFORM(progn, 0,0,body)
{ /* (PROGN {form}), CLTL p. 109 */
  implicit_progn(popSTACK(),NIL);
}

/* Macro: Evaluates the forms of a form list.
 implicit_prog();
 > -(STACK): form list
 increases STACK by 1
 can trigger GC */
#define implicit_prog()                               \
  do { while (mconsp(STACK_0)) {                      \
    var object forms = STACK_0;                       \
    STACK_0 = Cdr(forms);                             \
    eval(Car(forms)); /* evaluate next form */        \
    }                                                 \
    skipSTACK(1);                                     \
  } while(0)

LISPSPECFORM(prog1, 1,0,body)
{ /* (PROG1 form1 {form}), CLTL p. 109 */
  STACK_1 = (eval(STACK_1),value1); /* evaluate form1, save value */
  implicit_prog();
  VALUES1(popSTACK()); /* return saved value */
}

LISPSPECFORM(prog2, 2,0,body)
{ /* (PROG2 form1 form2 {form}), CLTL p. 109 */
  eval(STACK_2); /* evaluate form1 */
  eval(STACK_1); STACK_2 = value1; /* evaluate form2, save value */
  STACK_1 = STACK_0; skipSTACK(1);
  implicit_prog();
  VALUES1(popSTACK()); /* return saved value */
}

/* call parse_dd() and maybe complain about doc-string
 parse_doc_decl(body);
 > body: whole Body
 can trigger GC */
local maygc bool parse_doc_decl (object body, bool permit_doc_string) {
  pushSTACK(body);
  var bool to_compile = parse_dd(body);
  if (!permit_doc_string && !nullp(value3)) {
    pushSTACK(value1); pushSTACK(value2); pushSTACK(value3); /* save */
    pushSTACK(NIL); pushSTACK(STACK_(0+3+1));
    STACK_1 = CLSTEXT("doc-string is not allowed here and will be ignored: ~S");
    funcall(S(warn),2);
    value3 = popSTACK(); value2 = popSTACK(); value1 = popSTACK();
  }
  skipSTACK(1);
  return to_compile;
}

/* get the 5 environment objects to the stack
 adds 5 elements to the STACK */
local inline void aktenv_to_stack (void) {
  /* nest current environment, push on STACK */
  var gcv_environment_t* stack_env = nest_aktenv();
 #if !defined(STACK_UP)
  /* and transfer here */
  var object my_var_env = stack_env->var_env;
  var object my_fun_env = stack_env->fun_env;
  var object my_block_env = stack_env->block_env;
  var object my_go_env = stack_env->go_env;
  var object my_decl_env = stack_env->decl_env;
  skipSTACK(5); /* and take away from STACK again */
  pushSTACK(my_var_env); /* second argument */
  pushSTACK(my_fun_env); /* third argument */
  pushSTACK(my_block_env); /* fourth argument */
  pushSTACK(my_go_env); /* fifth argument */
  pushSTACK(my_decl_env); /* sixth argument */
 #endif
}

/* UP for LET, LET*, LOCALLY, MULTIPLE-VALUE-BIND, SYMBOL-MACROLET:
 Compiles the current form and executes it in compiled state.
 compile_form()
 > in STACK: EVAL-frame with the form
 < mv_count/mv_space: Values
 can trigger GC */
local maygc Values compile_eval_form (void)
{ /* execute (SYS::COMPILE-FORM form venv fenv benv genv denv) :
     get the whole form from the EVAL-frame in the stack: */
  pushSTACK(STACK_(frame_form)); /* as first argument */
  aktenv_to_stack();
  funcall(S(compile_form),6);
  /* call the freshly compiled closure with 0 arguments: */
  funcall(value1,0);
}

/* signal a correctable error for a broken LET variable spec
 can trigger GC */
local maygc object check_varspec (object varspec, object caller) {
  pushSTACK(NIL);     /* no PLACE */
  pushSTACK(varspec); /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(varspec); pushSTACK(caller);
  check_value(source_program_error,
              GETTEXT("~S: illegal variable specification ~S"));
  return value1;
}

/* the variables declared special appear on the stack twice:
   with binding SPECDECL (added when processing declarations)
   and the actual value (added when processing bindings).
 here we activate the SPECDECL bindings:
 Find the SPECDECL binding for the symbol
 > spec_pointer & spec_anz are returned by make_variable_frame()
 < return the pointer to the flags (or symbol+flags)
 i.e., something suitable to SET_BIT,
 or NULL if no such binding is found */
global gcv_object_t* specdecled_ (object symbol, gcv_object_t* spec_pointer,
                                  uintL spec_anz) {
  do {
    NEXT(spec_pointer);
   #ifdef NO_symbolflags
    if (eq(NEXT(spec_pointer),symbol)) {
      if (eq(NEXT(spec_pointer),Fixnum_0))
        return &Before(spec_pointer);
    } else {
      NEXT(spec_pointer);
    }
   #else
    if (eq(NEXT(spec_pointer),symbol))
      return &Before(spec_pointer);
   #endif
  } while (--spec_anz);
  return NULL;
}

/* UP for LET, LET*, LOCALLY, MULTIPLE-VALUE-BIND, SYMBOL-MACROLET:
 Analyzes the variables and declarations, builds up a variable binding-
 frame and extends VENV and poss. also DENV by a frame.
 make_variable_frame(caller,varspecs,&bind_ptr,&bind_count,&spec_ptr,&spec_count)
 > object caller: Caller, a symbol
 > object varspecs: list of variable-specifiers
 > object value2: list of declaration-specifiers
 > object value1: list ({form}) of forms
 < stack layout: variable binding frame, Env-binding-frame, ({form}).
 < gcv_object_t* bind_ptr: pointer to the first "genuine" binding.
 < uintC bind_count: number of "genuine" bindings.
 < gcv_object_t* spec_ptr: pointer to the first SPECDECL binding.
 < uintC spec_count: number of SPECDECL bindings.
 changes STACK
 can trigger GC */
local /*maygc*/ void make_variable_frame
(object caller, object varspecs, gcv_object_t** bind_ptr_, uintC* bind_count_,
 gcv_object_t** spec_ptr_, uintC* spec_count_)
{
  GCTRIGGER4(caller,varspecs,value1,value2);
  var object declarations = value2;
  { /* build up variable binding frame: */
    var gcv_object_t* top_of_frame = STACK; /* pointer to frame */
    /* first store the special-declared variables from
       declarations in the stack: */
    var gcv_object_t* spec_pointer = args_end_pointer;
    var uintL spec_anz = 0; /* number of SPECIAL-references */
    {
      var object declspecs = declarations;
      while (consp(declspecs)) {
        var object declspec = Car(declspecs); /* next declaration */
        if (consp(declspec) && eq(Car(declspec),S(special))) { /* (SPECIAL ) */
          while (consp( declspec = Cdr(declspec) )) {
            var object declsym = Car(declspec); /* next special item */
            if (!symbolp(declsym)) { /* should be a symbol */
              pushSTACK(value1); pushSTACK(value2);          /* save */
              pushSTACK(caller); pushSTACK(varspecs);        /* save */
              pushSTACK(declarations); pushSTACK(declspecs); /* save */
              pushSTACK(declspec);                           /* save */
              declsym = check_symbol_special(declsym,caller);
              declspec = popSTACK(); Car(declspec) = declspec;   /* restore */
              declspecs = popSTACK(); declarations = popSTACK(); /* restore */
              varspecs = popSTACK(); caller = popSTACK();        /* restore */
              value2 = popSTACK(); value1 = popSTACK();          /* restore */
            }
            /* store special-declared symbol in stack: */
            pushSTACK(specdecl); /* SPECDECL as "value" */
            pushSTACK_symbolwithflags(declsym,0); /* Symbol inactive */
            check_STACK();
            spec_anz++;
          }
        }
        declspecs = Cdr(declspecs);
      }
      *spec_count_ = spec_anz;
      *spec_ptr_ = spec_pointer;
    }
    *bind_ptr_ = args_end_pointer; /* pointer to first "genuine" binding */
    { /* Then store the "genuine" variable bindings (the variable
         and its unevaluated init at a time) in the stack: */
      var uintL var_anz = 0; /* number of variable bindings */
      {
        while (consp(varspecs)) {
          var object varspec = Car(varspecs); /* next varspec */
          /* split up in symbol and init: */
          var object symbol;
          var object init;
         retry_check_varspec:
          if (symbolp(varspec) && !eq(caller,S(symbol_macrolet))) {
            symbol = varspec; init = unbound;
          } else if /* one-/two-element list, with symbol as CAR ? */
            (consp(varspec)
             && !eq(caller, S(multiple_value_bind))
             && (symbol = Car(varspec), varspec = Cdr(varspec),
                 symbolp(symbol)
                 && ( /* two elements? */
                     (consp(varspec) && nullp(Cdr(varspec))
                      && (init = Car(varspec), true))
                     || /* one-element (allowed at LET, LET* according to X3J13 vote <182> ) */
                     (nullp(varspec) && !eq(caller,S(symbol_macrolet))
                      && (init = NIL, true))))) {
            /* now init = Car(varspec) or = NIL */
          } else {
            pushSTACK(value1); pushSTACK(value2);         /* save */
            pushSTACK(caller); pushSTACK(declarations);   /* save */
            pushSTACK(varspecs); /* save */
            varspec = check_varspec(Car(varspecs),caller);
            varspecs = popSTACK(); Car(varspecs) = varspec; /* restore */
            declarations = popSTACK(); caller = popSTACK(); /* restore */
            value2 = popSTACK(); value1 = popSTACK();       /* restore */
            goto retry_check_varspec;
          }
          pushSTACK(init); /* init and */
          pushSTACK_symbolwithflags(symbol,0); /* store variable */
          check_STACK();
          /* determine, if static or dynamic binding: */
          var bool specdecled = /* variable is declared special? */
            (specdecled_p(symbol,spec_pointer,spec_anz) != NULL);
          if (eq(caller,S(symbol_macrolet))) {
            if (special_var_p(TheSymbol(symbol))) {
              pushSTACK(symbol);
              pushSTACK(caller);
              fehler(program_error,
                     GETTEXT("~S: symbol ~S has been declared SPECIAL and may not be re-defined as a SYMBOL-MACRO"));
            }
            if (specdecled) {
              pushSTACK(symbol); /* SOURCE-PROGRAM-ERROR slot DETAIL */
              pushSTACK(symbol); pushSTACK(caller);
              fehler(source_program_error,
                     GETTEXT("~S: symbol ~S must not be declared SPECIAL and defined a SYMBOL-MACRO at the same time"));
            }
            /* static binding */
          } else {
            if (constant_var_p(TheSymbol(symbol))) {
              pushSTACK(value1); pushSTACK(value2);   /* save */
              pushSTACK(caller); pushSTACK(varspecs); /* save */
              pushSTACK(declarations);
              symbol = check_symbol_non_constant(symbol,caller);
              declarations = popSTACK(); varspecs = popSTACK(); /* restore */
              caller = popSTACK();
              value2 = popSTACK(); value1 = popSTACK();         /* restore */
              ASSERT(!constant_var_p(TheSymbol(symbol)));
              STACK_(varframe_binding_sym) = symbol;
            }
            if (specdecled || special_var_p(TheSymbol(symbol))) {
              /* bind dynamically */
              #if (varframe_binding_mark == varframe_binding_sym)
              STACK_(varframe_binding_mark) = SET_BIT(symbol,dynam_bit_o);
              #else
              STACK_(varframe_binding_mark) = SET_BIT(Fixnum_0,dynam_bit_o);
              #endif
            } else {
              /* bind statically */
            }
          }
          varspecs = Cdr(varspecs);
          var_anz++;
        }
      }
      *bind_count_ = var_anz;
      var_anz += spec_anz; /* total number of symbol/value pairs */
     #ifndef UNIX_DEC_ULTRIX_GCCBUG
      if (var_anz > (uintC)(~(uintC)0)) { /* does it fit into a uintC ? */
        pushSTACK(unbound);     /* SOURCE-PROGRAM-ERROR slot DETAIL */
        pushSTACK(caller);
        fehler(source_program_error,
               GETTEXT("~S: too many variables and/or declarations"));
      }
     #endif
      pushSTACK(aktenv.var_env); /* current VAR_ENV as NEXT_ENV */
      pushSTACK(fake_gcv_object(var_anz)); /* number of bindings */
      finish_frame(VAR);
    }
  }
  /* The variable binding frame is now finished. */
  var gcv_object_t* var_frame_ptr = STACK; /* pointer to variable binding frame */
  { /* build up VENV binding frame: */
    var gcv_object_t* top_of_frame = STACK; /* pointer to frame */
    /* first extend DENV by the necessary declspecs: */
    var object denv = aktenv.decl_env;
    pushSTACK(value1); /* save ({form}) */
    pushSTACK(declarations);
    while (mconsp(STACK_0)) {
      var object declspecs = STACK_0;
      STACK_0 = Cdr(declspecs);
      var object declspec = Car(declspecs); /* next Declspec */
      if (consp(declspec)) { /* should be a cons */
        if (!eq(Car(declspec),S(special))) /* we have treated (SPECIAL ...) already */
          denv = augment_decl_env(declspec,denv); /* process everything else */
      }
    }
    skipSTACK(1);
    var object forms = popSTACK();
    /* now build the frame: */
    if (eq(denv,aktenv.decl_env)) {
      pushSTACK(aktenv.var_env);
      finish_frame(ENV1V);
    } else {
      pushSTACK(aktenv.decl_env);
      pushSTACK(aktenv.var_env);
      finish_frame(ENV2VD);
      aktenv.decl_env = denv;
    }
    /* VENV-binding frame is finished. */
    aktenv.var_env = make_framepointer(var_frame_ptr); /* pointer to variable binding frame */
    pushSTACK(forms);
  }
}

/* activate the bindings created by make_variable_frame()
 > frame_pointer, count: values returned from make_variable_frame()
 count must be positive */
local void activate_bindings (gcv_object_t* frame_pointer, uintC count) {
  do {
    frame_pointer skipSTACKop -varframe_binding_size;
    var gcv_object_t* markptr = &Before(frame_pointer);
    if (as_oint(*markptr) & wbit(dynam_bit_o)) { /* binding dynamic? */
      var object symbol = *(markptr STACKop varframe_binding_sym); /* variable */
      var object newval = *(markptr STACKop varframe_binding_value); /* new value */
      *(markptr STACKop varframe_binding_value) = TheSymbolflagged(symbol)->symvalue; /* save old value in frame */
      TheSymbolflagged(symbol)->symvalue = newval; /* new value */
    }
    *markptr = SET_BIT(*markptr,active_bit_o); /* activate binding */
  } while (--count);
}
/* activate all SPECDECL declarations */
global void activate_specdecls (gcv_object_t* spec_ptr, uintC spec_count) {
  do {
    spec_ptr skipSTACKop -varframe_binding_size;
    var gcv_object_t* markptr = &Before(spec_ptr);
    *markptr = SET_BIT(*markptr,active_bit_o); /* activate binding */
  } while (--spec_count);
}

LISPSPECFORM(let, 1,0,body)
{ /* (LET ({varspec}) {decl} {form}), CLTL p. 110 */
  /* separate {decl} {form}: */
  if (parse_doc_decl(STACK_0,false)) { /* declaration (COMPILE) ? */
    /* yes -> compile form: */
    skipSTACK(2); return_Values compile_eval_form();
  } else {
    skipSTACK(1);
    /* build variable binding frame, extend VAR_ENV : */
    var gcv_object_t *bind_ptr, *spec_ptr;
    var uintC bind_count, spec_count;
    make_variable_frame(S(let),popSTACK(),&bind_ptr,&bind_count,
                        &spec_ptr,&spec_count);
    if (bind_count > 0) {
      { /* Then, evaluate the initialization forms: */
        var gcv_object_t* frame_pointer = bind_ptr;
        var uintC count = bind_count;
        do {
          var gcv_object_t* initptr = &NEXT(frame_pointer);
          var object init = *initptr; /* next init */
          *initptr = (!boundp(init) ? NIL : (eval(init),value1)); /* evaluate, NIL as default */
          frame_pointer skipSTACKop -(varframe_binding_size-1);
        } while (--count);
      }
      activate_bindings(bind_ptr,bind_count);
    }
    if (spec_count > 0) activate_specdecls(spec_ptr,spec_count);
    /* interpret body: */
    implicit_progn(popSTACK(),NIL);
    /* unwind frames: */
    unwind(); /* unwind VENV binding frame */
    unwind(); /* unwind variable binding frame */
  }
}

LISPSPECFORM(letstern, 1,0,body)
{ /* (LET* ({varspec}) {decl} {form}), CLTL p. 111 */
  /* separate {decl} {form} : */
  if (parse_doc_decl(STACK_0,false)) { /* declaration (COMPILE) ? */
    /* yes -> compile form: */
    skipSTACK(2); return_Values compile_eval_form();
  } else {
    skipSTACK(1);
    /* build variable binding frame, extend VAR_ENV : */
    var gcv_object_t *bind_ptr, *spec_ptr;
    var uintC bind_count, spec_count;
    make_variable_frame(S(letstern),popSTACK(),&bind_ptr,&bind_count,
                        &spec_ptr,&spec_count);
    /* Then, evaluate the initialization forms and activate the bindings */
    if (bind_count > 0) {
      var gcv_object_t* frame_pointer = bind_ptr;
      var uintC count = bind_count;
      do {
        var gcv_object_t* initptr = &Next(frame_pointer);
        frame_pointer skipSTACKop -varframe_binding_size;
        var gcv_object_t* markptr = &Before(frame_pointer);
        var object init = *initptr; /* next init */
        var object newval = (!boundp(init) ? NIL : (eval(init),value1)); /* evaluate, NIL as default */
        if (as_oint(*markptr) & wbit(dynam_bit_o)) { /* binding dynamic? */
          var object symbol = *(markptr STACKop varframe_binding_sym); /* variable */
          *initptr = TheSymbolflagged(symbol)->symvalue; /* save old value in frame */
          TheSymbolflagged(symbol)->symvalue = newval; /* new value */
          activate_specdecl(symbol,spec_ptr,spec_count);
        } else {
          *initptr = newval; /* new value into the frame */
        }
        *markptr = SET_BIT(*markptr,active_bit_o); /* activate binding */
      } while (--count);
    }
    if (spec_count > 0) activate_specdecls(spec_ptr,spec_count);
    /* interpret body: */
    implicit_progn(popSTACK(),NIL);
    /* unwind frames: */
    unwind(); /* unwind VENV-binding frame */
    unwind(); /* unwind variable binding frame */
  }
}

LISPSPECFORM(locally, 0,0,body)
{ /* (LOCALLY {decl} {form}), CLTL2 p. 221 */
  /* separate {decl} {form} : */
  var bool to_compile = parse_doc_decl(STACK_0,false);
  skipSTACK(1);
  if (to_compile) { /* declaration (COMPILE) ? */
    /* yes -> compile form: */
    return_Values compile_eval_form();
  } else {
    /* build variable binding frame, extend VAR_ENV : */
    var gcv_object_t *bind_ptr, *spec_ptr;
    var uintC bind_count, spec_count;
    make_variable_frame(S(locally),NIL,&bind_ptr,&bind_count,
                        &spec_ptr,&spec_count);
    if (bind_count) activate_bindings(bind_ptr,bind_count);
    if (spec_count) activate_specdecls(spec_ptr,spec_count);
    /* interpret body: */
    implicit_progn(popSTACK(),NIL);
    /* unwind frames: */
    unwind(); /* unwind VENV-binding frame */
    unwind(); /* unwind variable binding frame */
  }
}

LISPSPECFORM(compiler_let, 1,0,body)
{ /* (COMPILER-LET ({varspec}) {form}), CLTL p. 112 */
  var gcv_object_t* varspecs_ = &STACK_1;
  var object varspecs = *varspecs_; /* list of variables */
  var uintL varcount = llength(varspecs); /* number of variables */
  get_space_on_STACK(varcount*3*sizeof(gcv_object_t));
  /* evaluate varspecs: */
  var gcv_object_t* val_pointer = args_end_pointer; /* pointer to values */
  while (consp(varspecs)) {
    var object varspec = Car(varspecs);
    var object symbol;
   retry_check_varspec:
    if (consp(varspec)) { /* varspec is a Cons */
      pushSTACK(varspec); pushSTACK(varspecs); /* save */
      symbol = check_symbol_non_constant(Car(varspec),S(compiler_let));
      varspecs = popSTACK(); varspec = popSTACK(); /* restore */
      varspec = Cdr(varspec);
      if (consp(varspec) && nullp(Cdr(varspec))) {
        varspec = Car(varspec); /* Initform = second list element */
      } else if (nullp(varspec)) { /* allowed by X3J13 vote <182> */
        /* varspec = NIL; */ /* Initform = NIL */
      } else {
        pushSTACK(varspecs); /* save */
        varspec = check_varspec(Car(varspecs),S(compiler_let));
        varspecs = popSTACK(); Car(varspecs) = varspec; /* restore */
        goto retry_check_varspec;
      }
      pushSTACK(Cdr(varspecs));
      eval_noenv(varspec); /* evaluate initform */
      varspecs = STACK_0;
      STACK_0 = value1; /* and into the stack */
    } else {
      pushSTACK(varspecs); /* save */
      symbol = check_symbol_non_constant(varspec,S(compiler_let));
      varspecs = popSTACK(); /* restore */
      pushSTACK(NIL); /* NIL as value into the stack */
      varspecs = Cdr(varspecs);
    }
  }
  varspecs = *varspecs_;
  { /* build Frame: */
    var gcv_object_t* top_of_frame = STACK; /* pointer to frame */
    while (consp(varspecs)) {
      var object varspec = Car(varspecs);
      if (consp(varspec))
          varspec = Car(varspec);
      pushSTACK(Symbol_value(varspec)); /* old value of the variables */
      pushSTACK(varspec); /* variable */
      varspecs = Cdr(varspecs);
    }
    finish_frame(DYNBIND);
  }
  /* frame finished, now change the values of the variables: */
  varspecs = *varspecs_;
  {
    var gcv_object_t* valptr = val_pointer;
    while (consp(varspecs)) {
      var object varspec = Car(varspecs);
      if (consp(varspec))
        varspec = Car(varspec);
      Symbol_value(varspec) = NEXT(valptr); /* assign new value to the variables */
        varspecs = Cdr(varspecs);
    }
  }
  /* now evaluate the forms: */
  implicit_progn(*(varspecs_ STACKop -1),NIL);
  /* unwind binding frame: */
  unwind();
  /* clean up stack: */
  set_args_end_pointer(val_pointer);
  skipSTACK(2);
}

LISPSPECFORM(progv, 2,0,body)
{ /* (PROGV symbollist valuelist {form}), CLTL p. 112 */
  STACK_2 = (eval(STACK_2),value1); /* evaluate symbol list */
  var object valuelist = (eval(STACK_1),value1); /* evaluate value list */
  var object varlist = STACK_2;
  STACK_2 = STACK_0; /* save body */
  skipSTACK(2);
  var gcv_object_t *body_ = &STACK_0;
  progv(varlist,valuelist); /* build frame */
  implicit_progn(*body_,NIL); /* evaluate body */
  unwind(); /* unwind frame */
  skipSTACK(1); /* drop body */
}

/* error-message at FLET/LABELS, if there is no function specification.
 > caller: Caller, a symbol
 > obj: erroneous function specification */
nonreturning_function(local, fehler_funspec, (object caller, object obj)) {
  pushSTACK(obj);               /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(obj); pushSTACK(caller);
  fehler(source_program_error,GETTEXT("~S: ~S is not a function specification"));
}

/* skip all declarations from the body:
 descructively modifies BODY to remove (DECLARE ...)
 statements from its beginning */
local void skip_declarations (object* body) {
  while (consp(*body) && consp(Car(*body)) && eq(S(declare),Car(Car(*body))))
    *body = Cdr(*body);
}

/* UP: Finishes a FLET/MACROLET.
 finish_flet(top_of_frame,body);
 > stack layout: [top_of_frame] def1 name1 ... defn namen [STACK]
 > top_of_frame: pointer to frame
 > body: list of forms
 < mv_count/mv_space: values
 can trigger GC */
local maygc Values finish_flet (gcv_object_t* top_of_frame, object body) {
  {
    var uintL bindcount = /* number of bindings */
      STACK_item_count(STACK,top_of_frame) / 2;
      pushSTACK(aktenv.fun_env); /* current FUN_ENV as NEXT_ENV */
    pushSTACK(fake_gcv_object(bindcount));
    finish_frame(FUN);
  }
  /* function binding frame is finished.
     build FENV-binding frame: */
  {
    var gcv_object_t* top_of_frame = STACK; /* pointer to frame */
    pushSTACK(aktenv.fun_env);
    finish_frame(ENV1F);
    /* FENV-binding frame is finished.
       extend FUN_ENV:
       top_of_frame = pointer to the function binding frame */
    aktenv.fun_env = make_framepointer(top_of_frame);
  }
  /* allow declarations, as per ANSI CL */
  skip_declarations(&body);
  /* execute forms: */
  implicit_progn(body,NIL);
  unwind(); /* unwind FENV binding frame */
  unwind(); /* unwind function binding frame */
}

LISPSPECFORM(flet, 1,0,body)
{ /* (FLET ({funspec}) {form}), CLTL p. 113 */
  var object body = popSTACK(); /* ({form}) */
  var object funspecs = popSTACK(); /* ({funspec}) */
  /* build function binding frame: */
  var gcv_object_t* top_of_frame = STACK; /* pointer to frame */
  while (consp(funspecs)) {
    pushSTACK(body); /* save form list */
    pushSTACK(Cdr(funspecs)); /* remaining funspecs */
    funspecs = Car(funspecs); /* next funspec = (name . lambdabody) */
    /* should be a cons, whose CAR is a symbol and whose CDR is a cons: */
    if (!consp(funspecs)) {
     fehler_spec:
      fehler_funspec(S(flet),funspecs);
    }
    var object name = Car(funspecs);
    if (!funnamep(name)) {
      pushSTACK(funspecs);
      name = check_funname_replacement(source_program_error,S(flet),name);
      funspecs = popSTACK();
    }
    var object lambdabody = Cdr(funspecs);
    if (!consp(lambdabody))
      goto fehler_spec;
    pushSTACK(name); /* save name */
    /* turn lambdabody into a closure: */
    var object fun = get_closure(lambdabody,name,true,&aktenv);
    name = popSTACK();
    funspecs = popSTACK(); /* remaining funspecs */
    body = popSTACK();
    /* into the frame: */
    pushSTACK(fun); /* as "value" the closure */
    pushSTACK(name); /* name, binding is automatically active */
  }
  return_Values finish_flet(top_of_frame,body);
}

LISPSPECFORM(labels, 1,0,body)
{ /* (LABELS ({funspec}) {form}), CLTL p. 113 */
  /* We can dispense with the construction of a function binding frame,
     because when creating the first closure, the environment is nested anyway
     and thereby this function binding frame would be written into a vector.
     nest the current FUN_ENV: */
  pushSTACK(nest_fun(aktenv.fun_env));
  /* determine the number of funspecs and test the syntax: */
  var uintL veclength = 1; /* = 2 * (number of funspecs) + 1 */
  {
    pushSTACK(STACK_(1+1)); /* funspecs */
    while (consp(STACK_0)) {
      var object funspec = Car(STACK_0);
      /* should be a cons, whose CAR is a symbol and whose CDR is a cons: */
      if (!consp(funspec)) {
       fehler_spec:
        fehler_funspec(S(labels),funspec);
      }
      var object name = Car(funspec);
      if (!funnamep(name)) {
        pushSTACK(funspec);
        name = check_funname_replacement(source_program_error,S(labels),name);
        funspec = popSTACK();
      }
      var object lambdabody = Cdr(funspec);
      if (!consp(lambdabody))
        goto fehler_spec;
      STACK_0 = Cdr(STACK_0);
      veclength += 2;
    }
    skipSTACK(1); /* funspecs */
  }
  /* allocate vector of suitable length and store the names: */
  var object vec = allocate_vector(veclength);
  {
    var gcv_object_t* ptr = &TheSvector(vec)->data[0];
    var object funspecs = STACK_(1+1);
    while (consp(funspecs)) {
      *ptr++ = Car(Car(funspecs)); /* next name */
      ptr++; /* function remains NIL for the time being */
      funspecs = Cdr(funspecs);
    }
    *ptr++ = popSTACK(); /* nested FUN_ENV as last vector-element */
  }
  var object body = popSTACK(); /* form list */
  var object funspecs = popSTACK();
  { /* build FENV binding frame: */
    var gcv_object_t* top_of_frame = STACK; /* pointer to frame */
    pushSTACK(aktenv.fun_env);
    finish_frame(ENV1F);
  }
  /* extend FUN_ENV: */
  aktenv.fun_env = vec;
  /* create closures and put into the vector: */
  pushSTACK(body);
  pushSTACK(vec);
  {
    var uintL index = 1; /* index into the vector */
    while (consp(funspecs)) {
      pushSTACK(Cdr(funspecs)); /* remaining funspecs */
      var object funspec = Car(funspecs);
      /* create closure: */
      var object fun = get_closure(Cdr(funspec),Car(funspec),true,&aktenv);
      funspecs = popSTACK();
      TheSvector(STACK_0)->data[index] = fun; /* put into the vector */
      index += 2;
    }
  }
  skipSTACK(1); /* forget vector */
  body = popSTACK();
  /* allow declarations, as per ANSI CL */
  skip_declarations(&body);
  /* execute forms: */
  implicit_progn(body,NIL);
  unwind(); /* unwind FENV binding frame */
}

LISPSPECFORM(macrolet, 1,0,body)
{ /* (MACROLET ({macrodef}) {form}), CLTL p. 113 */
  var object body = popSTACK(); /* ({form}) */
  var object macrodefs = popSTACK(); /* ({macrodef}) */
  /* build macrobinding frame: */
  var gcv_object_t* top_of_frame = STACK; /* pointer to frame */
  while (consp(macrodefs)) {
    pushSTACK(body); /* save form list */
    pushSTACK(Cdr(macrodefs)); /* remaining macrodefs */
    macrodefs = Car(macrodefs); /* next macrodef = (name . lambdabody) */
    /* should be a cons, whose CAR is a symbol and whose CDR is a cons: */
    if (!consp(macrodefs)) {
     fehler_spec:
      pushSTACK(macrodefs);     /* SOURCE-PROGRAM-ERROR slot DETAIL */
      pushSTACK(macrodefs); pushSTACK(S(macrolet));
      fehler(source_program_error,
             GETTEXT("~S: ~S is not a macro specification"));
    }
    var object name = Car(macrodefs);
    if (!symbolp(name)) {
      pushSTACK(name);          /* SOURCE-PROGRAM-ERROR slot DETAIL */
      pushSTACK(name); pushSTACK(S(macrolet));
      fehler(source_program_error,
             GETTEXT("~S: macro name ~S should be a symbol"));
    }
    if (!mconsp(Cdr(macrodefs)))
      goto fehler_spec;
    pushSTACK(name); /* save */
    /* build macro-expander: (SYSTEM::MAKE-MACRO-EXPANDER macrodef nil env) */
    pushSTACK(macrodefs);
    pushSTACK(NIL);
    {
      aktenv_to_stack();
      { /* Add a MACROLET cons to the venv part of env: */
        var object new_cons = allocate_cons();
        Car(new_cons) = S(macrolet); Cdr(new_cons) = STACK_4;
        STACK_4 = new_cons;
      }
      { /* Add a MACROLET cons to the fenv part of env: */
        var object new_cons = allocate_cons();
        Car(new_cons) = S(macrolet); Cdr(new_cons) = STACK_3;
        STACK_3 = new_cons;
      }
      var object vec = vectorof(5);
      pushSTACK(vec);
    }
    funcall(S(make_macro_expander),3);
    name = popSTACK();
    macrodefs = popSTACK(); /* remaining macrodefs */
    body = popSTACK();
    /* into the frame: */
    pushSTACK(value1); /* as "value" the cons with the expander */
    pushSTACK(name); /* name, binding is automatically active */
  }
  return_Values finish_flet(top_of_frame,body);
}

LISPSPECFORM(function_macro_let, 1,0,body)
{ /* (SYSTEM::FUNCTION-MACRO-LET ({(name fun-lambdabody macro-full-lambdabody)})
        {form})
 is similar to FLET, except that alternative macro definitions
 are provided for every function. */
  var object body = popSTACK(); /* ({form}) */
  var object funmacspecs = popSTACK(); /* {(name fun-lambdabody macro-full-lambdabody)} */
  /* build FunctionMacro bindings frame : */
  var gcv_object_t* top_of_frame = STACK; /* pointer to frame */
  while (consp(funmacspecs)) {
    pushSTACK(body); /* save form list */
    pushSTACK(Cdr(funmacspecs)); /* remaining funmacspecs */
    funmacspecs = Car(funmacspecs);
    /* next (name fun-lambdabody macro-lambdabody) should be
       a list of length 3, whose CAR is a symbol
       and whose further list elements are conses: */
    if (!consp(funmacspecs)) {
     fehler_spec:
      pushSTACK(funmacspecs);   /* SOURCE-PROGRAM-ERROR slot DETAIL */
      pushSTACK(funmacspecs); pushSTACK(S(function_macro_let));
      fehler(source_program_error,
             GETTEXT("~S: ~S is not a function and macro specification"));
    }
    var object name = Car(funmacspecs);
    if (!symbolp(name)) {
      pushSTACK(name);          /* SOURCE-PROGRAM-ERROR slot DETAIL */
      pushSTACK(name); pushSTACK(S(function_macro_let));
      fehler(source_program_error,
             GETTEXT("~S: function and macro name ~S should be a symbol"));
    }
    if (!(mconsp(Cdr(funmacspecs)) && mconsp(Car(Cdr(funmacspecs)))
          && mconsp(Cdr(Cdr(funmacspecs)))
          && mconsp(Car(Cdr(Cdr(funmacspecs))))
          && nullp(Cdr(Cdr(Cdr(funmacspecs))))))
      goto fehler_spec;
    pushSTACK(name); /* save name */
    pushSTACK(Car(Cdr(funmacspecs))); /* fun-lambdabody */
    pushSTACK(Car(Cdr(Cdr(funmacspecs)))); /* macro-full-lambdabody */
    /* turn fun-lambdabody into a closure: */
    STACK_1 = get_closure(STACK_1,name,false,&aktenv);
    { /* build macro-expander:
         (SYSTEM::MAKE-FUNMACRO-EXPANDER name macro-full-lambdabody) */
      pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); funcall(S(make_funmacro_expander),2);
      pushSTACK(value1); C_macro_expander();
      STACK_0 = value1;
    }
    /* collect both: */
    C_make_function_macro();
    name = popSTACK();
    funmacspecs = popSTACK(); /* remaining funmacspecs */
    body = popSTACK();
    /* into the Frame: */
    pushSTACK(value1); /* as "value" the FunctionMacro */
    pushSTACK(name); /* name, binding is automatically active */
  }
  return_Values finish_flet(top_of_frame,body);
}

LISPSPECFORM(symbol_macrolet, 1,0,body)
{ /* (SYMBOL-MACROLET ({(var expansion)}) {decl} {form}), CLTL2 p. 155 */
  /* separate {decl} {form} : */
  if (parse_doc_decl(STACK_0,false)) { /* declaration (COMPILE) ? */
    /* yes -> compile form: */
    skipSTACK(2); return_Values compile_eval_form();
  } else {
    skipSTACK(1);
    /* build variable binding frame, extend VAR_ENV : */
    var gcv_object_t *bind_ptr, *spec_ptr;
    var uintC bind_count, spec_count;
    make_variable_frame(S(symbol_macrolet),popSTACK(),&bind_ptr,&bind_count,
                        &spec_ptr,&spec_count);
    /* then form the symbol-macros and activate the bindings: */
    if (bind_count > 0) {
      var gcv_object_t* frame_pointer = bind_ptr;
      var uintC count = bind_count;
      do {
        var gcv_object_t* initptr = &NEXT(frame_pointer);
        var object sm = allocate_symbolmacro();
        TheSymbolmacro(sm)->symbolmacro_expansion = *initptr;
        *initptr = sm;
        frame_pointer skipSTACKop -(varframe_binding_size-1);
        Before(frame_pointer) = SET_BIT(Before(frame_pointer),active_bit_o);
      } while (--count);
    }
    if (spec_count) activate_specdecls(spec_ptr,spec_count);
    /* interpret body: */
    implicit_progn(popSTACK(),NIL);
    /* unwind frames: */
    unwind(); /* unwind VENV-binding frame */
    unwind(); /* unwind variable-binding-frame */
  }
}

LISPSPECFORM(if, 2,1,nobody)
{ /* (IF test form1 [form2]), CLTL p. 115 */
  eval(STACK_2); /* evaluate condition */
  var object form;
  if (!nullp(value1)) {
    form = STACK_1; skipSTACK(3); /* evaluate form1 */
  } else {
    form = STACK_0; skipSTACK(3); /* evaluate form2 */
    if (!boundp(form)) {
      VALUES1(NIL); return; /* not supplied -> NIL */
    }
  }
  eval(form);
}

LISPSPECFORM(when, 1,0,body)
{ /* (WHEN test {form}), CLTL p. 115 */
  eval(STACK_1); /* evaluate condition */
  if (!nullp(value1)) {
    var object body = STACK_0;
    skipSTACK(2);
    implicit_progn(body,NIL);
  } else {
    skipSTACK(2);
    VALUES1(NIL);
  }
}

LISPSPECFORM(unless, 1,0,body)
{ /* (UNLESS test {form}), CLTL p. 115 */
  eval(STACK_1); /* evaluate condition */
  if (nullp(value1)) {
    var object body = STACK_0;
    skipSTACK(2);
    implicit_progn(body,NIL);
  } else {
    skipSTACK(2);
    VALUES1(NIL);
  }
}

LISPSPECFORM(cond, 0,0,body)
{ /* (COND {(bed {form})}), CLTL p. 116 */
  while (mconsp(STACK_0)) {
    var object clause = STACK_0; /* clause-list */
    STACK_0 = Cdr(clause); /* save remaining clauses */
    clause = Car(clause); /* next clause */
    if (!consp(clause)) { /* should be a cons */
      pushSTACK(clause);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
      pushSTACK(clause); pushSTACK(S(cond));
      fehler(source_program_error,GETTEXT("~S: clause ~S should be a list"));
    }
    pushSTACK(Cdr(clause)); /* save clause rest */
    eval(Car(clause)); /* evaluate condition */
    if (!nullp(value1))
      goto eval_clause;
    skipSTACK(1); /* try next */
  }
  /* no condition was satisfied. */
  VALUES1(NIL); skipSTACK(1); return;
 eval_clause: { /* found a true condition: */
    var object clause_rest = popSTACK(); /* clause rest */
    skipSTACK(1);
    implicit_progn(clause_rest,value1); /* evaluate */
  }
}

LISPSPECFORM(case, 1,0,body)
{ /* (CASE keyform {(keys {form})}), CLTL p. 117 */
  eval(STACK_1); /* evaluate keyform */
  var object value = value1;
  var object clauses = STACK_0;
  var object clause;
  skipSTACK(2);
  while (consp(clauses)) {
    clause = Car(clauses); /* next clause */
    clauses = Cdr(clauses);
    if (!consp(clause)) { /* should be a cons */
      pushSTACK(clause);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
      pushSTACK(clause); pushSTACK(S(case));
      fehler(source_program_error,GETTEXT("~S: missing key list: ~S"));
    }
    var object keys = Car(clause);
    if (eq(keys,T) || eq(keys,S(otherwise))) {
      if (nullp(clauses))
        goto eval_clause;
      pushSTACK(clauses);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
      pushSTACK(keys); pushSTACK(S(case));
      fehler(source_program_error,
             GETTEXT("~S: the ~S clause must be the last one"));
    } else {
      if (listp(keys)) {
        while (consp(keys)) {
          if (eql(Car(keys),value))
            goto eval_clause;
          keys = Cdr(keys);
        }
      } else {
        if (eql(keys,value))
          goto eval_clause;
      }
    }
  }
  /* no condition was satisfied. */
  VALUES1(NIL); return;
 eval_clause: { /* found a true condition: */
    var object clause_rest = Cdr(clause); /* clause-rest */
    implicit_progn(clause_rest,NIL); /* evaluate */
  }
}

LISPSPECFORM(block, 1,0,body)
{ /* (BLOCK name {form}), CLTL p. 119 */
  var object name = check_symbol(STACK_1);
  var object body = STACK_0; skipSTACK(2);
  var sp_jmp_buf returner; /* return point */
  { /* build block-frame: */
    var gcv_object_t* top_of_frame = STACK; /* pointer to frame */
    pushSTACK(name); /* block-name */
    pushSTACK(aktenv.block_env); /* current BLOCK_ENV as NEXT_ENV */
    finish_entry_frame(IBLOCK,returner,, goto block_return; );
  }
  { /* build BENV-frame: */
    var gcv_object_t* top_of_frame = STACK;
    pushSTACK(aktenv.block_env);
    finish_frame(ENV1B);
    /* extend BLOCK_ENV (top_of_frame = pointer to the block-frame) */
    aktenv.block_env = make_framepointer(top_of_frame);
  }
  /* execute body: */
  implicit_progn(body,NIL);
  unwind(); /* unwind BENV-binding frame */
 block_return: /* we jump to this label, if the BLOCK-Frame
                  has caught a RETURN-FROM. */
  unwind(); /* unwind BLOCK-frame */
}

/* error-message, if a block has already been left.
 fehler_block_left(name);
 > name: block-name */
nonreturning_function(global, fehler_block_left, (object name)) {
  pushSTACK(name);
  pushSTACK(S(return_from));
  fehler(control_error,
         GETTEXT("~S: the block named ~S has already been left"));
}

LISPSPECFORM(return_from, 1,1,nobody)
{ /* (RETURN-FROM name [result]), CLTL p. 120 */
  var object name = check_symbol(STACK_1);
  /* traverse BLOCK_ENV: */
  var object env = aktenv.block_env; /* current BLOCK_ENV */
  var gcv_object_t* FRAME;
  while (framepointerp(env)) {
    /* env is a frame-pointer to a IBLOCK-frame in the stack. */
    FRAME = TheFramepointer(env);
    if (framecode(FRAME_(0)) & bit(nested_bit_t)) {
      /* frame already nested */
      env = FRAME_(frame_next_env); break;
    }
    if (eq(FRAME_(frame_name),name))
      goto found;
    env = FRAME_(frame_next_env);
  }
  /* env is an Alist. */
  while (consp(env)) {
    var object block_cons = Car(env);
    if (eq(Car(block_cons),name)) {
      env = Cdr(block_cons);
      if (eq(env,disabled)) /* block still active? */
        fehler_block_left(name);
      goto found;
      }
    env = Cdr(env);
  }
  /* env is done. */
  pushSTACK(name);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(name); pushSTACK(S(return_from));
  fehler(source_program_error,
         GETTEXT("~S: no block named ~S is currently visible"));
  /* found block-frame: env */
 found:
  FRAME = uTheFramepointer(env); /* pointer to that frame */
  /* produce values, with which the block will be left: */
  var object result = popSTACK();
  skipSTACK(1);
  if (boundp(result)) { /* result supplied? */
    eval(result);
  } else {
      VALUES1(NIL);
  }
  /* jump to the found block-frame and unwind it: */
  unwind_upto(FRAME);
}

/* UP: append LIST to the "accumulation set" STACK_1 (head)/STACK_0 (tail)
 set_last_inplace() is "NCONC"
 set_last_copy()   is "APPEND" [can trigger GC]
 modifies */
local inline void set_last_inplace (object list) {
  if (!consp(STACK_0)) STACK_1=STACK_0=list; /* init */
  else Cdr(STACK_0) = list; /* insert as (cdr (last totallist)) */
  if (consp(list)) {
    var object list1;
    loop { /* list is a cons */
      list1 = Cdr(list);
      if (atomp(list1)) break;
      list = list1;
    }
    STACK_0 = list; /* (last totallist) <- (last list) */
  }
}
local inline maygc void set_last_copy (object list) {
  if (consp(list)) {
    pushSTACK(list);
    pushSTACK(allocate_cons());
    pushSTACK(STACK_0);
    /* stack layout: head, tail, list, copy, tail */
    Car(STACK_0) = Car(STACK_2);
    while (consp(Cdr(STACK_2))) {
      STACK_2 = Cdr(STACK_2);
      var object new_cons = allocate_cons();
      Cdr(STACK_0) = new_cons; STACK_0 = new_cons;
      Car(STACK_0) = Car(STACK_2);
    }
    Cdr(STACK_0) = Cdr(STACK_2); /* atom */
    if (!consp(STACK_(0+3))) {
      STACK_(1+3) = STACK_1; /* init head */
      STACK_(0+3) = STACK_0; /* init tail */
    } else {
      Cdr(STACK_(0+3)) = STACK_1; /* insert as (cdr (last totallist)) */
      STACK_(0+3) = STACK_0;
    }
    skipSTACK(3);
  } else {
    if (!consp(STACK_0)) STACK_1=STACK_0=list; /* init */
    else Cdr(STACK_0) = list; /* insert as (cdr (last totallist)) */
  }
}

/* We build the functions MAPCAR & MAPLIST in two versions:
 The first one builds the list in reversed order, then has to reverse it.
 The second one works in the forward direction. */
/* #define MAP_REVERSES */

#ifdef MAP_REVERSES

/* macro for MAPCAR and MAPLIST */
#define MAPCAR_MAPLIST_BODY(listaccess)                                 \
  { var gcv_object_t* args_pointer = rest_args_pointer STACKop 2;       \
    argcount++; /* argcount := number of lists on the stack */          \
    /* reserve space for the function call arguments: */                \
    get_space_on_STACK(sizeof(gcv_object_t)*(uintL)argcount);           \
    pushSTACK(NIL); /* start of the result list */                      \
   {var gcv_object_t* ergptr = &STACK_0; /* pointer to it */            \
    /* traverse all lists in parallel: */                               \
    loop { var gcv_object_t* argptr = args_pointer;                     \
      var object fun = NEXT(argptr);                                    \
      var uintC count = argcount;                                       \
      do {                                                              \
        var gcv_object_t* next_list_ = &NEXT(argptr);                   \
        var object next_list = *next_list_;                             \
        if (endp(next_list)) goto fertig; /* a list ended -> done */    \
        pushSTACK(listaccess(next_list)); /* as argument on the stack */ \
        *next_list_ = Cdr(next_list); /* shorten list */                \
      } while (--count);                                                \
      funcall(fun,argcount); /* call function */                        \
      pushSTACK(value1);                                                \
     {var object new_cons = allocate_cons(); /* new cons */             \
      Car(new_cons) = popSTACK(); Cdr(new_cons) = STACK_0;              \
      STACK_0 = new_cons; /* lengthen the result list */                \
     }}                                                                 \
    fertig:                                                             \
    VALUES1(nreverse(*ergptr)); /* reverse result list */               \
    set_args_end_pointer(args_pointer); /* clean up STACK */            \
   }}

#else

/* macro for MAPCAR and MAPLIST */
#define MAPCAR_MAPLIST_BODY(listaccess)                          \
  { var gcv_object_t* args_pointer = rest_args_pointer STACKop 2;       \
    argcount++; /* argcount := number of lists on the stack */          \
    /* reserve space for the function call arguments: */                \
    get_space_on_STACK(sizeof(gcv_object_t)*(uintL)argcount);           \
    /* start total list: */                                             \
    pushSTACK(NIL); /* total list */                                    \
    pushSTACK(NIL); /* (last totallist) */                              \
   {var gcv_object_t *ret=&STACK_1; /* remember the total list*/        \
    /* traverse all lists in parallel: */                               \
    loop { var gcv_object_t* argptr = args_pointer;                     \
      var object fun = NEXT(argptr);                                    \
      var uintC count = argcount;                                       \
      do {                                                              \
        var gcv_object_t* next_list_ = &NEXT(argptr);                   \
        var object next_list = *next_list_;                             \
        if (endp(next_list)) goto fertig; /* a list ended -> done */    \
        pushSTACK(listaccess(next_list)); /* as argument on the stack */ \
        *next_list_ = Cdr(next_list); /* shorten list */                \
      } while (--count);                                                \
      funcall(fun,argcount); /* call function */                        \
      pushSTACK(value1);                                                \
     {var object new_cons = allocate_cons(); /* new cons */             \
      Car(new_cons) = popSTACK(); /* new_cons = (LIST (FUNCALL ...)) */ \
      if (nullp(STACK_1)) STACK_1 = STACK_0 = new_cons; /* init */      \
      else { Cdr(STACK_0) = new_cons; STACK_0 = new_cons; } /* append */ \
    }}                                                                  \
   fertig:                                                              \
    VALUES1(*ret); /* result list-cons */                               \
    set_args_end_pointer(args_pointer); /* clean up STACK */            \
   }}

#endif

/* macro for MAPC and MAPL */
#define MAPC_MAPL_BODY(listaccess)                               \
  { var gcv_object_t* args_pointer = rest_args_pointer STACKop 2;       \
    argcount++; /* argcount := number of lists on the stack */          \
    /* reserve space for the function call arguments: */                \
    get_space_on_STACK(sizeof(gcv_object_t)*(uintL)argcount);           \
    pushSTACK(BEFORE(rest_args_pointer)); /* save first list argument */ \
   {var gcv_object_t* ergptr = &STACK_0; /* pointer to it */            \
    /* traverse all lists in parallel: */                               \
    loop { var gcv_object_t* argptr = args_pointer;                     \
      var object fun = NEXT(argptr);                                    \
      var uintC count = argcount;                                       \
      do {                                                              \
        var gcv_object_t* next_list_ = &NEXT(argptr);                   \
        var object next_list = *next_list_;                             \
        if (endp(next_list)) goto fertig; /* a list ended -> done */    \
        pushSTACK(listaccess(next_list)); /* as argument on the stack */ \
        *next_list_ = Cdr(next_list); /* shorten list */                \
      } while (--count);                                                \
      funcall(fun,argcount); /* call function */                        \
    }                                                                   \
    fertig:                                                             \
    VALUES1(*ergptr); /* first list as value */                         \
    set_args_end_pointer(args_pointer); /* clean up STACK */            \
   }}

/* macro for MAPCAN and MAPCON
 no MAP_REVERSES version is provided because NRECONC drops
 the last atom in dotted lists, e.g., (mapcan #'identity '(1))
 returns NIL when it should return 1:
 (apply (function nconc) (mapcar (function identity) (quote (1)))) => 1 */
#define MAPCAN_MAPCON_BODY(listaccess,append_function)                  \
  { var gcv_object_t* args_pointer = rest_args_pointer STACKop 2;       \
    argcount++; /* argcount := number of lists on the stack */          \
    /* reserve space for the function call arguments: */                \
    get_space_on_STACK(sizeof(gcv_object_t)*(uintL)argcount);           \
    /* start total list: */                                             \
    pushSTACK(NIL); /* total list */                                    \
    pushSTACK(NIL); /* (last totallist) */                              \
   {var gcv_object_t *ret=&STACK_1; /* remember the total list*/        \
    /* traverse all lists in parallel: */                               \
    loop { var gcv_object_t* argptr = args_pointer;                     \
      var object fun = NEXT(argptr);                                    \
      var uintC count = argcount;                                       \
      do {                                                              \
        var gcv_object_t* next_list_ = &NEXT(argptr);                   \
        var object next_list = *next_list_;                             \
        if (endp(next_list)) goto fertig; /* a list ended -> done */    \
        pushSTACK(listaccess(next_list)); /* as argument on the stack */ \
        *next_list_ = Cdr(next_list); /* shorten list */                \
      } while (--count);                                                \
      funcall(fun,argcount); /* call function */                        \
      append_function(value1);                                          \
    }                                                                   \
    fertig:                                                             \
    VALUES1(*ret); /* result list */                                    \
    set_args_end_pointer(args_pointer); /* clean up STACK */            \
   }}

#define Identity

LISPFUN(mapcar,seclass_default,2,0,rest,nokey,0,NIL)
/* (MAPCAR fun list {list}), CLTL p. 128 */
  MAPCAR_MAPLIST_BODY(Car)

LISPFUN(maplist,seclass_default,2,0,rest,nokey,0,NIL)
/* (MAPLIST fun list {list}), CLTL p. 128 */
  MAPCAR_MAPLIST_BODY(Identity)

LISPFUN(mapc,seclass_default,2,0,rest,nokey,0,NIL)
/* (MAPC fun list {list}), CLTL p. 128 */
  MAPC_MAPL_BODY(Car)

LISPFUN(mapl,seclass_default,2,0,rest,nokey,0,NIL)
/* (MAPL fun list {list}), CLTL p. 128 */
  MAPC_MAPL_BODY(Identity)

LISPFUN(mapcan,seclass_default,2,0,rest,nokey,0,NIL)
/* (MAPCAN fun list {list}), CLTL p. 128 */
  MAPCAN_MAPCON_BODY(Car,set_last_inplace)

LISPFUN(mapcon,seclass_default,2,0,rest,nokey,0,NIL)
/* (MAPCON fun list {list}), CLTL p. 128 */
  MAPCAN_MAPCON_BODY(Identity,set_last_inplace)

LISPFUN(mapcap,seclass_default,2,0,rest,nokey,0,NIL)
/* (EXT:MAPCAP fun list {list}) */
  MAPCAN_MAPCON_BODY(Car,set_last_copy)

LISPFUN(maplap,seclass_default,2,0,rest,nokey,0,NIL)
/* (EXT:MAPLAP fun list {list}) */
  MAPCAN_MAPCON_BODY(Identity,set_last_copy)

LISPSPECFORM(tagbody, 0,0,body)
{ /* (TAGBODY {tag | statement}), CLTL p. 130 */
  var object body = popSTACK();
  { /* build GENV-frame: */
    var gcv_object_t* top_of_frame = STACK; /* pointer to frame */
    pushSTACK(aktenv.go_env);
    finish_frame(ENV1G);
  }
  /* build TAGBODY-frame: */
  var gcv_object_t* top_of_frame = STACK; /* pointer to frame */
  /* parse body and store tags in stack: */
  var uintL tagcount = 0;
  {
    var object body_rest = body;
    while (consp(body_rest)) {
      var object item = Car(body_rest);
      body_rest = Cdr(body_rest);
      /* as tags are considered: symbols and numbers
         (like in compiler), Conses are statements. */
      if (atomp(item)) {
        if (numberp(item) || symbolp(item)) {
          /* store label in stack: */
          check_STACK();
          pushSTACK(body_rest); /* body-rest after the label */
          pushSTACK(item);
          tagcount++;
        } else {
          pushSTACK(item);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
          pushSTACK(item); pushSTACK(S(tagbody));
          fehler(source_program_error,
                 GETTEXT("~S: ~S is neither tag nor form"));
        }
      }
    }
  }
  if (tagcount>0) {
    var sp_jmp_buf returner; /* return point */
    pushSTACK(aktenv.go_env); /* current GO_ENV as NEXT_ENV */
    finish_entry_frame(ITAGBODY,returner,, goto go_entry; );
    /* extend GO_ENV: */
    aktenv.go_env = make_framepointer(STACK);
    if (false) {
     go_entry: /* we jump to this label, if this frame has caught a GO. */
      body = value1; /* the formlist is passed as value1. */
    }
    /* process statements: */
    pushSTACK(body);
    while (mconsp(STACK_0)) {
      var object body_rest = STACK_0;
      STACK_0 = Cdr(body_rest); /* remaining body */
      body_rest = Car(body_rest); /* next item */
      if (consp(body_rest)) {
        eval(body_rest); /* form -> evaluate */
      }
    }
    skipSTACK(1); /* forget body */
    unwind(); /* unwind TAGBODY-frame */
    unwind(); /* unwind GENV-frame */
  } else {
    /* body without -> only PROGN with value NIL */
    skipSTACK(2); /* unwind GENV-frame again, GENV is unchanged */
    pushSTACK(body); implicit_prog();
  }
  VALUES1(NIL);
}

LISPSPECFORM(go, 1,0,nobody)
{ /* (GO tag), CLTL p. 133 */
  var object tag = popSTACK();
  if (!(numberp(tag) || symbolp(tag))) {
    pushSTACK(tag);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
    pushSTACK(tag); pushSTACK(S(go));
    fehler(source_program_error,GETTEXT("~S: illegal tag ~S"));
  }
  /* peruse GO_ENV: */
  var object env = aktenv.go_env; /* current GO_ENV */
  var gcv_object_t* FRAME;
  while (framepointerp(env)) {
    /* env is a frame-pointer to a ITAGBODY-frame in the stack. */
    FRAME = uTheFramepointer(env);
    if (framecode(FRAME_(0)) & bit(nested_bit_t)) {
      /* frame already nested */
      env = FRAME_(frame_next_env); break;
    }
    /* search tags in  unnested ITAGBODY-frame: */
    var gcv_object_t* bind_ptr = &FRAME_(frame_bindings); /* pointer below the tag bindings */
    var gcv_object_t* bindend_ptr = STACKpointable(topofframe(FRAME_(0))); /* pointer above the tag bindings */
    do {
      if (eql(*bind_ptr,tag)) { /* tag found? */
        value1 = *(bind_ptr STACKop 1); /* fetch formlist from frame */
        goto found;
      }
      bind_ptr skipSTACKop 2;
    } while (bind_ptr != bindend_ptr);
    env = FRAME_(frame_next_env);
  }
  /* env is an Alist. */
  while (consp(env)) {
    var object tagbody_cons = Car(env);
    var object tagbody_vec = Car(tagbody_cons); /* tag-vector */
    var gcv_object_t* tagptr = &TheSvector(tagbody_vec)->data[0];
    var uintL index = 0;
    var uintL count = Svector_length(tagbody_vec);
    do {
      if (eql(*tagptr++,tag)) { /* tag found? */
        env = Cdr(tagbody_cons);
        if (eq(env,disabled)) { /* tagbody still active? */
          pushSTACK(tag);
          pushSTACK(S(go));
          fehler(control_error,
                 GETTEXT("~S: tagbody for tag ~S has already been left"));
        }
        FRAME = uTheFramepointer(env); /* pointer to the (still active!) frame */
        value1 = FRAME_(frame_bindings+2*index+1); /* formlist */
        goto found;
      }
      index++;
    } while (--count);
    env = Cdr(env);
  }
  /* env is finished. */
  pushSTACK(tag);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(tag); pushSTACK(S(go));
  fehler(source_program_error,
         GETTEXT("~S: no tag named ~S is currently visible"));
  /* tagbody-frame found. FRAME is pointing to it (without typeinfo),
     value1 is the liste of the forms to be executed. */
 found:
  mv_count=1; /* formlist value1 is passed */
  /* jump to the found tagbody-frame and continue there: */
  unwind_upto(FRAME);
}

/* error-message, when there are too many values
 fehler_mv_zuviel(caller);
 > caller: Caller, a symbol */
nonreturning_function(global, fehler_mv_zuviel, (object caller)) {
  pushSTACK(caller);
  fehler(error,GETTEXT("~S: too many values"));
}

LISPFUN(values,seclass_no_se,0,0,rest,nokey,0,NIL)
{ /* (VALUES {arg}), CLTL p. 134
     [not foldable, in order to avoid infinite loop!]*/
  if (argcount >= mv_limit)
    fehler_mv_zuviel(S(values));
  STACK_to_mv(argcount);
}

LISPFUNNR(values_list,1)
{ /* (VALUES-LIST list), CLTL p. 135 */
  list_to_mv(popSTACK(), fehler_mv_zuviel(S(values_list)); );
}

LISPSPECFORM(multiple_value_list, 1,0,nobody)
{ /* (MULTIPLE-VALUE-LIST form), CLTL p. 135 */
  eval(popSTACK()); /* evaluate form */
  mv_to_list(); /* pack values into list */
  VALUES1(popSTACK()); /* return list */
}

LISPSPECFORM(multiple_value_call, 1,0,body)
{ /* (MULTIPLE-VALUE-CALL fun {form}), CLTL p. 135 */
  var gcv_object_t* fun_ = &STACK_1;
  *fun_ = (eval(*fun_),value1); /* evaluate function */
  var object forms = popSTACK(); /* formlist */
  var uintL argcount = 0; /* number of arguments so far */
  while (consp(forms)) {
    pushSTACK(Cdr(forms)); /* remaining forms */
    eval(Car(forms)); /* evaluate next form */
    forms = popSTACK();
    /* put its values into the stack: */
    argcount += (uintL)mv_count;
    mv_to_STACK();
  }
  if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1)) {
    pushSTACK(*fun_);
    pushSTACK(S(multiple_value_call));
    fehler(program_error,GETTEXT("~S: too many arguments to ~S"));
  }
  funcall(*fun_,argcount); /* call function */
  skipSTACK(1);
}

LISPSPECFORM(multiple_value_prog1, 1,0,body)
{ /* (MULTIPLE-VALUE-PROG1 form {form}), CLTL p. 136 */
  eval(STACK_1); /* evaluate first form */
  var object body = popSTACK();
  skipSTACK(1);
  var uintC mvcount = mv_count; /* number of values */
  mv_to_STACK(); /* all values into the stack */
  pushSTACK(body); implicit_prog();
  STACK_to_mv(mvcount); /* fetch all values again from the stack */
}

LISPSPECFORM(multiple_value_bind, 2,0,body)
{ /* (MULTIPLE-VALUE-BIND ({var}) values-form {decl} {form}), CLTL p. 136 */
  /* separate {decl} {form} : */
  if (parse_doc_decl(STACK_0,false)) { /* declaration (COMPILE) ? */
    /* yes -> compile form: */
    skipSTACK(3); return_Values compile_eval_form();
  } else {
    var object varlist = STACK_2;
    STACK_2 = STACK_1;
    skipSTACK(2);
    /* build variable binding frame, extend VAR_ENV : */
    var gcv_object_t* form_ = &STACK_0;
    var gcv_object_t *bind_ptr, *spec_ptr;
    var uintC bind_count, spec_count;
    make_variable_frame(S(multiple_value_bind),varlist,&bind_ptr,&bind_count,
                        &spec_ptr,&spec_count);
    /* stack layout: values-form, variable binding frame,
                     env-binding-frame, ({form}).
       now evaluate values-form: */
    eval(*form_);
    /* Macro for binding variables in the variable-frame: binds
       the next variable to value, decreases frame_pointer by 2 resp. 3. */
  #define bind_next_var(value)                                          \
    { var gcv_object_t* valptr = &Next(frame_pointer);                  \
      frame_pointer skipSTACKop -varframe_binding_size;                 \
     {var gcv_object_t* markptr = &Before(frame_pointer);               \
       if (as_oint(*markptr) & wbit(dynam_bit_o)) { /* dynamic binding: */ \
        var object sym = *(markptr STACKop varframe_binding_sym); /* var */ \
        *valptr = TheSymbolflagged(sym)->symvalue; /* old val into the frame */ \
        TheSymbolflagged(sym)->symvalue = (value); /* new value into the value cell */ \
        activate_specdecl(sym,spec_ptr,spec_count);                     \
      } else /* static binding : */                                     \
        *valptr = (value); /* new value into the frame */               \
      *markptr = SET_BIT(*markptr,active_bit_o); /* activate binding */ \
     }}
    /* bind the r:=bind_count variables to the s:=mv_count values:
       (if there are not enough variables: discard remaining values;
       if there are not enough values:    fill with NIL.)
       here, r>=0 and s>=0. */
    {
      var gcv_object_t* frame_pointer = bind_ptr;
      var uintC r = bind_count;
      var object* mv_pointer;
      var uintC s = mv_count;
      if (r==0) goto ok; /* no variables? */
      if (s==0) goto fill; /* no values? */
      /* still min(r,s)>0 values to bind: */
     #if !defined(VALUE1_EXTRA)
      mv_pointer = &mv_space[0];
     #else
      bind_next_var(value1);
      if (--r == 0) goto ok; /* no more variables? */
      if (--s == 0) goto fill; /* no more values? */
      mv_pointer = &mv_space[1];
     #endif
      /* still min(r,s)>0 values to bind: */
      loop {
        bind_next_var(*mv_pointer++);
        if (--r == 0) goto ok; /* no more variables? */
        if (--s == 0) goto fill; /* no more values? */
      }
     fill: /* still bind r>0 variables to NIL */
      do { bind_next_var(NIL); } while (--r);
     ok: ;
    }
    if (spec_count > 0) activate_specdecls(spec_ptr,spec_count);
    /* interpret body: */
    implicit_progn(popSTACK(),NIL);
    /* unwind frames: */
    unwind(); /* unwind VENV binding frame */
    unwind(); /* unwind variable-binding-frame */
    skipSTACK(1);
  }
}
#undef bind_next_var

LISPSPECFORM(multiple_value_setq, 2,0,nobody)
{ /* (MULTIPLE-VALUE-SETQ ({var}) form), CLTL p. 136 */
  /* check variable list: */
  var gcv_object_t* firstvarptr = args_end_pointer;
  var uintL varcount = 0;
  {
    var gcv_object_t* varlistr_ = &STACK_1;
    while (consp(*varlistr_)) {
      var object symbol =   /* next variable */
        check_symbol_non_constant(Car(*varlistr_),S(multiple_value_setq));
      *varlistr_ = Cdr(*varlistr_);
      varcount++;
      pushSTACK(symbol);
      check_STACK();
      if (sym_macrop(symbol)) /* and not a symbol-macro */
        goto expand;
    }
    if (false) {
     expand: /* turn MULTIPLE-VALUE-SETQ into MULTIPLE-VALUE-SETF */
      dotimespL(varcount,varcount, {
        var object new_cons = allocate_cons();
        Car(new_cons) = popSTACK(); Cdr(new_cons) = *varlistr_;
        *varlistr_ = new_cons;
      });
      /* stack layout: varlist, form. */
      pushSTACK(STACK_0); STACK_1 = STACK_2; STACK_2 = S(multiple_value_setf);
      var object newform = listof(3);
      eval(newform);
      return;
    }
  }
  eval(Before(firstvarptr)); /* evaluate form */
  /* Write values into the stack (needed because setq() can trigger GC): */
  var gcv_object_t* mvptr = args_end_pointer;
  mv_to_STACK();
  /* Perform the assignments: */
  var uintL valcount = mv_count; /* number of values */
  {
    var uintL remaining = valcount; /* number of values that are still available */
    var gcv_object_t* varptr = firstvarptr;
    dotimesL(varcount,varcount, {
      var object value;
      if (remaining>0) {
        value = NEXT(mvptr); remaining--; /* next value */
      } else {
        value = NIL; /* NIL, if all values are consumed */
      }
      setq(NEXT(varptr),value); /* assign to the next variable */
    });
  }
  /* Return the first among the multiple values as the only value: */
  VALUES1(valcount > 0 ? (object)STACK_(valcount-1) : NIL);
  set_args_end_pointer(firstvarptr STACKop 2); /* clean up STACK */
}

LISPSPECFORM(catch, 1,0,body)
{ /* (CATCH tag {form}), CLTL p. 139 */
  STACK_1 = (eval(STACK_1),value1); /* evaluate tag */
  /* finish building of CATCH-frame: */
  var object body = popSTACK(); /* ({form}) */
  var gcv_object_t* top_of_frame = STACK STACKop 1; /* pointer above frame */
  var sp_jmp_buf returner; /* memorize return point */
  finish_entry_frame(CATCH,returner,, goto catch_return; );
  /* execute body: */
  implicit_progn(body,NIL);
 catch_return: /* we jump to this label, if the catch-frame built
                  above has caught a throw. */
  skipSTACK(3); /* unwind CATCH-frame */
}

LISPSPECFORM(unwind_protect, 1,0,body)
{ /* (UNWIND-PROTECT form {cleanup}), CLTL p. 140 */
  var object cleanup = popSTACK();
  var object form = popSTACK();
  /* build UNWIND-PROTECT-frame: */
  pushSTACK(cleanup);
  var gcv_object_t* top_of_frame = STACK;
  var sp_jmp_buf returner; /* return point */
  finish_entry_frame(UNWIND_PROTECT,returner,, goto throw_save; );
  /* evaluate protected form: */
  eval(form);
  { /* Cleanup after normal termination of the protected form: */
    /* unwind UNWIND-PROTECT-frame: */
    skipSTACK(2);
    cleanup = popSTACK();
    /* save values: */
    var uintC mvcount = mv_count;
    mv_to_STACK();
    /* process cleanup-forms: */
    pushSTACK(cleanup); implicit_prog();
    /* write back values: */
    STACK_to_mv(mvcount);
  }
  return;
 throw_save: /* we jump to this label, if the Unwind-Protect-Frame
                built above has kept a throw.
                save unwind_protect_to_save and jump to it in the end. */
  {
    var restartf_t fun = unwind_protect_to_save.fun;
    var gcv_object_t* arg = unwind_protect_to_save.upto_frame;
    /* Cleanup: */
    /* unwind UNWIND-PROTECT-frame: */
    skipSTACK(2);
    cleanup = popSTACK();
    /* save values: */
    var uintC mvcount = mv_count;
    mv_to_STACK();
    /* process cleanup-forms: */
    pushSTACK(cleanup); implicit_prog();
    /* write back values: */
    STACK_to_mv(mvcount);
    /* and jump further: */
    fun(arg);
  }
}

LISPSPECFORM(throw, 2,0,nobody)
{ /* (THROW tag result), CLTL p. 142 */
  STACK_1 = (eval(STACK_1),value1); /* evaluate tag */
  eval(popSTACK()); /* evaluate result */
  var object tag = popSTACK(); /* evaluated tag */
  throw_to(tag); /* try to throw to this tag */
  /* failed. */
  pushSTACK(tag);
  pushSTACK(S(throw));
  fehler(control_error,GETTEXT("~S: there is no CATCHer for tag ~S"));
}

LISPFUNN(driver,1)
{ /* (SYS::DRIVER fun) builds a driver-frame, that calls the function
 fun (with 0 arguments) each time. fun is executed in a endless loop
 that can be aborted with GO or THROW . */
  var gcv_object_t* top_of_frame = STACK; /* pointer above frame */
  var sp_jmp_buf returner; /* remember entry point */
  finish_entry_frame(DRIVER,returner,,;);
  /* this is the entry point. */
  loop { funcall(STACK_(0+2),0); } /* call fun, endless loop */
}

LISPFUNN(unwind_to_driver,1)
{ /* (SYS::UNWIND-TO-DRIVER top-p)
     UNWIND to the next Driver-Frame or to the top. */
  var object arg = popSTACK();
  if (nullp(arg))
    reset(1);
  else if (uint32_p(arg))
    reset(I_to_uint32(arg));
  else
    reset(0);
}

/* Checks an optional macroexpansion-environment in STACK_0.
 > STACK_0: argument
 < STACK_0: macroexpansions-environment #(venv fenv)
 can trigger GC */
local maygc void test_env (void) {
  var object arg = STACK_0;
  if (missingp(arg)) { /* required by ANSI CL sections 3.1.1.3.1, 3.1.1.4 */
    arg = allocate_vector(2); /* vector #(nil nil) as default */
  } else while (!(simple_vector_p(arg) && (Svector_length(arg) == 2))) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(arg); /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_svector2)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(arg);
    check_value(type_error,
                GETTEXT("Argument ~S is not a macroexpansion environment"));
    arg = value1;
  }
  STACK_0 = arg;
}

LISPFUN(macro_function,seclass_read,1,1,norest,nokey,0,NIL)
{ /* (MACRO-FUNCTION symbol [env]), CLTL p. 144;
     Issue MACRO-FUNCTION-ENVIRONMENT:YES */
  test_env();
  var object symbol = check_symbol(STACK_1);
  var object env = STACK_0; skipSTACK(2);
  var object fundef = sym_function(symbol,TheSvector(env)->data[1]);
  if (fsubrp(fundef)) {
    /* a FSUBR -> search in property list: (GET symbol 'SYS::MACRO) */
    var object got = get(symbol,S(macro)); /* search */
    if (!boundp(got)) /* not found? */
      goto nil;
    value1 = got;
  } else if (macrop(fundef)) { /* #<MACRO expander> ? */
    value1 = TheMacro(fundef)->macro_expander;
  } else { /* SUBR/Closure/FunctionMacro/#<UNBOUND> -> no macrodefinition */
   nil:
    value1 = NIL;
  }
  mv_count=1;
}

LISPFUN(macroexpand,seclass_default,1,1,norest,nokey,0,NIL)
{ /* (MACROEXPAND form [env]), CLTL p. 151 */
  test_env();
  var object env = popSTACK();
  var object form = STACK_0;
  STACK_0 = env; /* save env */
  macroexp0(form,env); /* expand */
  if (!nullp(value2)) { /* something happened? */
    /* yes -> expand to death: */
    do { macroexp0(value1,STACK_0);
    } while (!nullp(value2));
    value2 = T;
  }
  mv_count=2; skipSTACK(1);
}

LISPFUN(macroexpand_1,seclass_default,1,1,norest,nokey,0,NIL)
{ /* (MACROEXPAND-1 form [env]), CLTL p. 151 */
  test_env();
  var object env = popSTACK();
  var object form = popSTACK();
  macroexp0(form,env); /* expand one time */
  mv_count=2;
}

LISPSPECFORM(declare, 0,0,body)
{ /* (DECLARE {decl-spec}), CLTL p. 153 */
  /* ({decl-spec}) already in STACK_0 */
  pushSTACK(STACK_0);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
  fehler(source_program_error,
         GETTEXT("declarations ~S are not allowed here"));
}

LISPSPECFORM(the, 2,0,nobody)
{ /* (THE value-type form), CLTL p. 161 */
  eval(STACK_0); /* evaluate form */
  mv_to_list(); /* build value list and save */
  /* stack layout: value-type, form, values.
     call (SYS::%THE values (SYS::TYPE-FOR-DISCRIMINATION value-type))
     for type-check: */
  pushSTACK(STACK_0);
  pushSTACK(STACK_(2+1)); funcall(S(type_for_discrimination),1);
  pushSTACK(value1);
  funcall(S(pthe),2);
  if (nullp(value1)) {
    /* type-check failed */
    pushSTACK(STACK_(2+0)); /* value-type */
    pushSTACK(STACK_(0+1)); /* values */
    pushSTACK(STACK_(1+2)); /* form */
    pushSTACK(S(the));
    fehler(error, /* type_error ?? */
           GETTEXT("~S: ~S evaluated to the values ~S, not of type ~S"));
  }
  /* type-check OK -> return values: */
  list_to_mv(popSTACK(), { fehler_mv_zuviel(S(the)); } );
  skipSTACK(2);
}

LISPFUNN(proclaim,1)
{ /* (PROCLAIM decl-spec) */
  if (!consp(STACK_0/*declspec*/)) {
    pushSTACK(S(proclaim));
    fehler(error,GETTEXT("~S: bad declaration ~S"));
  }
  var object decltype = Car(STACK_0/*declspec*/); /* declaration type */
  if (eq(decltype,S(special))) { /* SPECIAL */
    while (!endp( STACK_0/*declspec*/ = Cdr(STACK_0/*declspec*/) )) {
      var object symbol = check_symbol(Car(STACK_0/*declspec*/));
      if (symmacro_var_p(TheSymbol(symbol))) {
        /* HyperSpec/Body/mac_define-symbol-macro.html says that making a
           global symbol-macro special is undefined. */
        pushSTACK(S(special)); pushSTACK(symbol); pushSTACK(TheSubr(subr_self)->name);
        fehler(program_error,
               GETTEXT("~S: attempting to turn ~S into a ~S variable, but it is already a global symbol-macro."));
      }
      if (!keywordp(symbol))
        clear_const_flag(TheSymbol(symbol));
      set_special_flag(TheSymbol(symbol));
    }
  } else if (eq(decltype,S(notspecial))) { /* NOTSPECIAL */
    while (!endp( STACK_0/*declspec*/ = Cdr(STACK_0/*declspec*/) )) {
      var object symbol = check_symbol(Car(STACK_0/*declspec*/));
      if (!keywordp(symbol)) clear_const_flag(TheSymbol(symbol));
      clear_special_flag(TheSymbol(symbol));
    }
  } else if (eq(decltype,S(declaration))) { /* DECLARATION */
    while (!endp( STACK_0/*declspec*/ = Cdr(STACK_0/*declspec*/) )) {
      var object symbol = check_symbol(Car(STACK_0/*declspec*/));
      /* (PUSHNEW symbol (cdr declaration-types)) : */
      if (nullp(memq(symbol,Cdr(O(declaration_types))))) {
        pushSTACK(symbol);
        {
          var object new_cons = allocate_cons();
          var object list = O(declaration_types);
          Car(new_cons) = popSTACK(); Cdr(new_cons) = Cdr(list);
          Cdr(list) = new_cons;
        }
      }
    }
  } else if (eq(decltype,S(inline)) || eq(decltype,S(notinline))) {
    pushSTACK(decltype); /* INLINE, NOTINLINE */
    while (!endp( STACK_1/*declspec*/ = Cdr(STACK_1/*declspec*/) )) {
      var object symbol = check_funname(source_program_error,S(proclaim),
                                        Car(STACK_1/*declspec*/));
      /*(SYS::%PUT (SYS::GET-FUNNAME-SYMBOL symbol) 'SYS::INLINABLE decltype)*/
      pushSTACK(symbol); funcall(S(get_funname_symbol),1); pushSTACK(value1);
      pushSTACK(S(inlinable)); pushSTACK(STACK_2)/*decltype*/;
      funcall(L(put),3);
    }
    skipSTACK(1); /*decltype*/
  } else if (eq(decltype,S(constant_inline))
             || eq(decltype,S(constant_notinline))) {
    pushSTACK(decltype); /* CONSTANT-INLINE, CONSTANT-NOTINLINE */
    while (!endp( STACK_1/*declspec*/ = Cdr(STACK_1/*declspec*/) )) {
      var object symbol = check_symbol(Car(STACK_1/*declspec*/));
      /* (SYS::%PUT symbol 'SYS::CONSTANT-INLINABLE decltype) : */
      pushSTACK(symbol); pushSTACK(S(constant_inlinable));
      pushSTACK(STACK_2)/*decltype*/; funcall(L(put),3);
    }
    skipSTACK(1); /*decltype*/
  } else if (eq(decltype,S(optimize))) {
    pushSTACK(Cdr(STACK_0)); funcall(S(note_optimize),1);
  } else {                /* check that the declspec is a proper list */
    pushSTACK(STACK_0/*declspec*/); funcall(L(list_length_proper),1);
  }
  VALUES1(NIL); skipSTACK(1);
}

LISPFUNN(eval,1)
{ /* (EVAL form), CLTL p. 321 */
  eval_noenv(popSTACK()); /* evaluate form in empty environment */
}

LISPSPECFORM(load_time_value, 1,1,nobody)
{ /* (LOAD-TIME-VALUE form [read-only-p]), CLTL2 p. 680 */
  var object form = STACK_1;
  skipSTACK(2); /* ignore read-only-p */
  eval_noenv(form); /* evaluate form in empty environment */
  mv_count=1;
}

/* UP: Checks an optional environment-argument for EVALHOOK and APPLYHOOK.
 test_optional_env_arg(&env5);
 < env5: 5 components of the environment
 increases STACK by 1 */
local void test_optional_env_arg (environment_t* env5) {
  var object env = popSTACK(); /* env-argument */
  if (!boundp(env)) { /* not supplied -> void environment */
    env5->var_env   = NIL;
    env5->fun_env   = NIL;
    env5->block_env = NIL;
    env5->go_env    = NIL;
    env5->decl_env  = O(top_decl_env);
  } else if (simple_vector_p(env) && (Svector_length(env) == 5)) {
    /* a simple-vector of length 5 */
    env5->var_env   = TheSvector(env)->data[0];
    env5->fun_env   = TheSvector(env)->data[1];
    env5->block_env = TheSvector(env)->data[2];
    env5->go_env    = TheSvector(env)->data[3];
    env5->decl_env  = TheSvector(env)->data[4];
  } else
    fehler_environment(env);
}

LISPFUN(evalhook,seclass_default,3,1,norest,nokey,0,NIL)
{ /* (EVALHOOK form evalhookfn applyhookfn [env]), CLTL p. 323 */
  var environment_t env5;
  test_optional_env_arg(&env5); /* env-argument after env5 */
  var object applyhookfn = popSTACK();
  var object evalhookfn = popSTACK();
  var object form = popSTACK();
  bindhooks(evalhookfn,applyhookfn); /* bind *EVALHOOK* and *APPLYHOOK* */
  /* build environment-frame: */
  make_ENV5_frame();
  /* set current environments: */
  aktenv.var_env   = env5.var_env  ;
  aktenv.fun_env   = env5.fun_env  ;
  aktenv.block_env = env5.block_env;
  aktenv.go_env    = env5.go_env   ;
  aktenv.decl_env  = env5.decl_env ;
  /* evaluate form bypassing *EVALHOOK* and *APPLYHOOK* : */
  eval_no_hooks(form);
  unwind(); /* unwind environment-frame */
  unwind(); /* unwind binding frame for *EVALHOOK* / *APPLYHOOK* */
}

LISPFUN(applyhook,seclass_default,4,1,norest,nokey,0,NIL)
{ /* (APPLYHOOK function args evalhookfn applyhookfn [env]), CLTL p. 323 */
  var environment_t env5;
  test_optional_env_arg(&env5); /* env-Argument after env5 */
  var object applyhookfn = popSTACK();
  var object evalhookfn = popSTACK();
  var object args = popSTACK();
  var object fun = popSTACK();
  bindhooks(evalhookfn,applyhookfn); /* bind *EVALHOOK* and *APPLYHOOK* */
  /* build environment-frame: */
  make_ENV5_frame();
  /* set current environments: */
  aktenv.var_env   = env5.var_env  ;
  aktenv.fun_env   = env5.fun_env  ;
  aktenv.block_env = env5.block_env;
  aktenv.go_env    = env5.go_env   ;
  aktenv.decl_env  = env5.decl_env ;
  { /* save fun: */
    pushSTACK(fun);
    var gcv_object_t* fun_ = &STACK_0;
    /* evaluate each argument and store on the stack: */
    var uintC argcount = 0;
    while (consp(args)) {
      pushSTACK(Cdr(args)); /* remaining argument list */
      eval_no_hooks(Car(args)); /* evaluate next arg */
      args = STACK_0; STACK_0 = value1; /* store value in stack */
      argcount++;
      if (argcount==0) { /* overflow? */
        pushSTACK(*fun_);
        pushSTACK(S(applyhook));
        fehler(program_error,GETTEXT("~S: too many arguments given to ~S"));
      }
    }
    funcall(*fun_,argcount); /* apply function */
    skipSTACK(1);
  }
  unwind(); /* unwind environment-frame */
  unwind(); /* unwind binding frame for *EVALHOOK* / *APPLYHOOK* */
}

/* check whether the form is a constant */
local bool form_constant_p (object form) {
  if (symbolp(form))
    return constant_var_p(TheSymbol(form));
  if (consp(form)) {
    var object head = Car(form);
    if (eq(head,S(quote))) return true;
    if (!funnamep(head)) return false;  /* what's this form? */
    /* cf. funname_to_symbol */
    var object fdef = head;
    if (!symbolp(fdef))
      /* (get ... 'SYS::SETF-FUNCTION) */
      fdef = get(Car(Cdr(fdef)),S(setf_function));
    if (!symbolp(fdef))
      /* Use of (setf foo) before it's defined. */
      return false;
    fdef = Symbol_function(fdef);
    if ((cclosurep(fdef) && (Cclosure_seclass(fdef) == seclass_foldable))
        || (subrp(fdef) && (TheSubr(fdef)->seclass == seclass_foldable))) {
      check_SP();
      loop {
        form = Cdr(form);
        if (nullp(form)) return true;  /* list is over */
        if (!consp(form)) return false;  /* invalid form */
        if (!form_constant_p(Car(form))) return false;
      }
    }
    return false;
  }
  /* self-evaluating objects, i.e., (NOT (OR symbol cons)), are constants */
  return true;
}

LISPFUN(constantp,seclass_read,1,1,norest,nokey,0,NIL)
{ /* (CONSTANTP expr [env]), CLTL p. 324 */
  skipSTACK(1); /* environment is not used */
  VALUES_IF(form_constant_p(popSTACK()));
}

/* (SYS::GLOBAL-SYMBOL-MACRO-P symbol) tests if the symbol is a global
   symbol macro, defined through DEFINE-SYMBOL-MACRO. */
LISPFUNNR(global_symbol_macro_p,1)
{
  var object symbol = check_symbol(popSTACK());
  VALUES_IF(symmacro_var_p(TheSymbol(symbol)));
}

/* (FUNCTION-SIDE-EFFECT fun) -> seclass, fdefinition, name */
LISPFUNNR(function_side_effect,1)
{ /* This function is called at compile time, so the argument does not have to
     be a function, it may be a variable name whose value will be some function
     at run time. Therefore we never signal errors, just return *SECLASS-DIRTY*. */
  var object fdef = popSTACK();
  var object name = unbound;
  if (consp(fdef) && consp(Cdr(fdef))
      && (eq(S(quote),Car(fdef)) || eq(S(function),Car(fdef))))
    fdef = Car(Cdr(fdef));
  if (funnamep(fdef)) {
    name = fdef;
    /* cf. funname_to_symbol */
    if (!symbolp(fdef))
      /* (get ... 'SYS::SETF-FUNCTION) */
      fdef = get(Car(Cdr(fdef)),S(setf_function));
  }
  if (symbolp(fdef))
    fdef = Symbol_function(fdef);
  /* If the argument was a function object, then we have it now. */
  var seclass_t seclass = seclass_default;
  if (subrp(fdef))
    seclass = (seclass_t)TheSubr(fdef)->seclass;
  else if (cclosurep(fdef))
    seclass = (seclass_t)Cclosure_seclass(fdef);
  if (!boundp(name) && boundp(fdef)) {
    if (subrp(fdef))
      name = TheSubr(fdef)->name;
    else if (closurep(fdef))
      name = Closure_name(fdef);
  }
  VALUES3(seclass_object(seclass),
          boundp(fdef) ? fdef : NIL,
          boundp(name) ? name : NIL);
}

LISPFUNNR(function_name_p,1)
{ /* (SYS::FUNCTION-NAME-P expr) recognizes function name */
  var object arg = popSTACK();
  VALUES_IF(funnamep(arg));
}

LISPFUNN(check_function_name,2)
{ /* (SYS::CHECK-FUNCTION-NAME funname caller)
 checks whether the funname argument is a function name, giving the user the
 opportunity to correct it if it is not. Returns the corrected function name. */
  VALUES1(check_funname(source_program_error,STACK_0,STACK_1));
  skipSTACK(2);
}

LISPFUNN(check_symbol,2)
{ /* (SYS::CHECK-SYMBOL symbol caller)
 checks whether the symbol argument is a symbol, giving the user the
 opportunity to correct it if it is not. Returns the corrected symbol. */
  var gcv_object_t *sym_ = &STACK_1;
  var gcv_object_t *caller_ = &STACK_0;
  while (!symbolp(*sym_)) {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(*sym_);           /* SOURCE-PROGRAM-ERROR slot DETAIL */
    pushSTACK(*sym_); pushSTACK(*caller_);
    check_value(source_program_error,GETTEXT("~S: ~S is not a symbol"));
    *sym_ = value1;
  }
  VALUES1(*sym_);
  skipSTACK(2);
}

LISPFUN(parse_body,seclass_default,1,1,norest,nokey,0,NIL)
{ /* (SYS::PARSE-BODY body [docstring-allowed])
 parses body, recognizes declarations, returns three values:
 1. body-rest, all forms after the declarations,
 2. list of occurred declspecs
 3. docstring (only if docstring-allowed=T ) or NIL.
 (docstring-allowed should be = NIL or T) */
  parse_doc_decl(STACK_1/*body*/,!missingp(STACK_0));
  /* got 3 values from parse_dd(): ({form}), declspecs, doc */
  mv_count = 3;
  skipSTACK(2);
}

LISPFUNN(keyword_test,2)
{ /* (SYSTEM::KEYWORD-TEST arglist kwlist)
 determines, if all keywords in the list kwlist occur
 in the argument-list arglist (a keyword/value - list) or if there
 is a keyword/value-pair :ALLOW-OTHER-KEYS with value /= NIL .
 if not, error. */
  var object arglist = STACK_1;
  { /* check number of arguments: */
    var uintL argcount = llength(arglist);
    if (argcount % 2) {
      pushSTACK(arglist);
      /* ANSI CL 3.5.1.6. wants a PROGRAM-ERROR here. */
      fehler(program_error,
             GETTEXT("keyword argument list ~S has an odd length"));
    }
  }
  { /* search, if there is :ALLOW-OTHER-KEYS : */
    var object arglistr = arglist;
    while (consp(arglistr)) {
      if (eq(Car(arglistr),S(Kallow_other_keys)) && !nullp(Car(Cdr(arglistr))))
        goto fertig;
      arglistr = Cdr(Cdr(arglistr));
    }
  }
  { /* check whether all specified keywords occur in kwlist: */
    var object arglistr = arglist;
    while (consp(arglistr)) {
      var object key = Car(arglistr); arglistr = Cdr(arglistr);
      var object val = Car(arglistr); arglistr = Cdr(arglistr);
      if (eq(key,S(Kallow_other_keys))) {
        if (nullp(val)) break;  /* need check */
        else goto fertig;       /* no check */
      }
    }
    for (arglistr = arglist; consp(arglistr); ) {
      var object key = Car(arglistr); arglistr = Cdr(arglistr);
      var object val = Car(arglistr); arglistr = Cdr(arglistr);
      if (!eq(key,S(Kallow_other_keys))
          && nullp(memq(key,STACK_0))) { /* not found */
        pushSTACK(key); /* KEYWORD-ERROR Slot DATUM */
        pushSTACK(key);
        pushSTACK(STACK_(0+2));
        pushSTACK(val);
        pushSTACK(key);
        { /* `(MEMBER ,@kwlist) = KEYWORD-ERROR Slot EXPECTED-TYPE */
          var object type = allocate_cons();
          Car(type) = S(member); Cdr(type) = STACK_(0+5);
          STACK_3 = type;
        }
        fehler(keyword_error,
               GETTEXT("Illegal keyword/value pair ~S, ~S in argument list.\n"
                       "The allowed keywords are ~S"));
      }
    }
  }
 fertig:
  skipSTACK(2);
  VALUES1(NIL);
}

LISPSPECFORM(and, 0,0,body)
{ /* (AND {form}), CLTL p. 82 */
  var object body = popSTACK();
  if (atomp(body)) {
    VALUES1(T); /* (AND) -> T */
  } else {
    loop {
      pushSTACK(Cdr(body));
      eval(Car(body)); /* evaluate form */
      body = popSTACK();
      if (atomp(body))
        break; /* at the end: return values of the last form */
      if (nullp(value1)) {
        mv_count=1; break; /* prematurely: 1 value NIL */
      }
    }
  }
}

LISPSPECFORM(or, 0,0,body)
{ /* (OR {form}), CLTL p. 83 */
  var object body = popSTACK();
  if (atomp(body)) {
    VALUES1(NIL); /* (OR) -> NIL */
  } else {
    loop {
      pushSTACK(Cdr(body));
      eval(Car(body)); /* evaluate form */
      body = popSTACK();
      if (atomp(body))
        break; /* at the end: return values of the last form */
      if (!nullp(value1)) {
        mv_count=1; break; /* prematurely: 1 value /=NIL */
      }
    }
  }
}

LISPFUN(xor,seclass_foldable,0,0,rest,nokey,0,NIL)
{ /* (XOR {form}) returns either 2 values: the unique non-NIL value
     and its index in the argument list; or NIL */
  VALUES1(NIL); /* for the case of all NILs*/
  while (argcount) {
    var object arg = popSTACK();
    if (!nullp(arg)) {
      if (!nullp(value1)) {
        VALUES1(NIL);
        skipSTACK(--argcount);
        return;
      } else
        VALUES2(arg,fixnum(argcount));
    }
    argcount--;
  }
}

/* From now on, the table macro has a different use: */
#undef LISPSPECFORM

/* table of all Fsubr-functions: */
global const struct fsubr_tab_ fsubr_tab = {
 #define LISPSPECFORM LISPSPECFORM_D
  #include "fsubr.c"
 #undef LISPSPECFORM
};
