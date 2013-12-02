/*
 * Error-Handling for CLISP
 * Bruno Haible 1990-2005
 * Marcus Daniels 8.4.1994
 * Sam Steingold 1998-2004
 * German comments translated into English: Stefan Kain 2002-09-11
 */

#include "lispbibl.c"

/* SYS::*RECURSIVE-ERROR-COUNT* =
   depth of recursion of the output of error messages */

local void cancel_interrupts (void) {
 #ifdef PENDING_INTERRUPTS
  interrupt_pending = false; /* Ctrl-C pending time is soon completed */
   #ifndef WIN32_NATIVE
    begin_system_call();
     #ifdef HAVE_UALARM
      ualarm(0,0); /* abort SIGALRM timer */
     #else
      alarm(0); /* abort SIGALRM timer */
     #endif
    end_system_call();
   #endif
 #endif
}

/* UP: Starts the output of an error message.
 begin_error()
 < STACK_0: Stream (in general *ERROR-OUTPUT*)
 < STACK_1: value of *error-handler*
 < STACK_2: list of arguments for *error-handler*
 < STACK_3: type of condition (in general SIMPLE-ERROR) or NIL
 decreases STACK by 7 */
local void begin_error (void)
{
  end_system_call(); /* there is no system call running anymore */
  cancel_interrupts();
 #if defined(HAVE_SIGNALS) && defined(SIGPIPE)
  writing_to_subprocess = false;
 #endif
  /* increase error-count, if >3 abort output: */
  dynamic_bind(S(recursive_error_count),
               fixnum_inc(Symbol_value(S(recursive_error_count)),1));
  if (!posfixnump(Symbol_value(S(recursive_error_count)))) /* should be a fixnum >=0 */
    Symbol_value(S(recursive_error_count)) = Fixnum_0; /* otherwise emergency correction */
  if (posfixnum_to_V(Symbol_value(S(recursive_error_count))) > 3) {
    /* multiple nested error message. */
    Symbol_value(S(recursive_error_count)) = Fixnum_0; /* delete error count */
    /* bind *PRINT-PRETTY* to NIL (in order to save memory): */
    dynamic_bind(S(print_pretty),NIL);
    fehler(serious_condition,
           /* Note: All translations of this error message should be in
              pure ASCII, to avoid endless recursion if *terminal-encoding*
              supports only ASCII characters. */
           GETTEXT("Unprintable error message"));
  }
  var object error_handler = Symbol_value(S(error_handler)); /* *ERROR-HANDLER* */
  if (!nullp(error_handler)) { /* *ERROR-HANDER* /= NIL */
    pushSTACK(NIL); pushSTACK(NIL); pushSTACK(error_handler);
    pushSTACK(make_string_output_stream()); /* String-Output-Stream */
  } else if (nullpSv(use_clcs)) { /* SYS::*USE-CLCS* */
    /* *ERROR-HANDER* = NIL, SYS::*USE-CLCS* = NIL */
    pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
    pushSTACK(var_stream(S(error_output),strmflags_wr_ch_B)); /* Stream *ERROR-OUTPUT* */
    fresh_line(&STACK_0); /* new line */
    write_sstring(&STACK_0,O(error_string1)); /* print "*** - " */
  } else { /* *ERROR-HANDER* = NIL, SYS::*USE-CLCS* /= NIL */
    pushSTACK(S(simple_error)); pushSTACK(NIL); pushSTACK(unbound);
    pushSTACK(make_string_output_stream()); /* String-Output-Stream */
  }
}

/* UP: output an error-object. */
local void write_errorobject (object obj) {
  if (nullp(STACK_1)) {
    dynamic_bind(S(prin_stream),unbound); /* bind SYS::*PRIN-STREAM* to #<UNBOUND> */
    dynamic_bind(S(print_escape),T); /* bind *PRINT-ESCAPE* to T */
    dynamic_bind(S(print_readably),NIL); /* bind *PRINT-READABLY* to NIL */
    prin1(&STACK_(0+3+3+3),obj); /* output directly */
    dynamic_unbind(S(print_readably));
    dynamic_unbind(S(print_escape));
    dynamic_unbind(S(prin_stream));
  } else {
    /* push obj onto the argument list: */
    pushSTACK(obj);
    obj = allocate_cons();
    Car(obj) = popSTACK();
    Cdr(obj) = STACK_2; STACK_2 = obj;
    /* and write "~S" into the format string: */
    write_ascii_char(&STACK_0,'~'); write_ascii_char(&STACK_0,'S');
  }
}

/* UP: outputs an error-character. */
local void write_errorchar (object obj) {
  if (nullp(STACK_1)) {
    write_char(&STACK_0,obj); /* write directly */
  } else { /* push obj on the argument list: */
    pushSTACK(obj);
    obj = allocate_cons();
    Car(obj) = popSTACK();
    Cdr(obj) = STACK_2; STACK_2 = obj;
    /* and write "~A" into the format string: */
    write_ascii_char(&STACK_0,'~'); write_ascii_char(&STACK_0,'A');
  }
}

/* UP: Outputs a piece of an error string without modifications.
 write_errorasciz_substring(start,end);
 > start, end: delimit an unmovable string in UTF-8 encoding */
local void write_errorasciz_substring (const uintB* start, const uintB* end)
{
 #ifdef UNICODE
  var object encoding = O(internal_encoding);
  var const uintB* bptr = start;
  var const uintB* bendptr = end;
  var uintL clen = Encoding_mblen(encoding)(encoding,bptr,bendptr);
  if (clen > 0) {
    var DYNAMIC_ARRAY(charbuf,chart,clen);
    {
      var chart* cptr = &charbuf[0];
      var chart* cendptr = cptr+clen;
      Encoding_mbstowcs(encoding)(encoding,nullobj,&bptr,
                                  bendptr,&cptr,cendptr);
      ASSERT(cptr == cendptr);
    }
    {
      var const chart* cptr = &charbuf[0];
      dotimespL(clen,clen, { write_code_char(&STACK_0,*cptr); cptr++; });
    }
    FREE_DYNAMIC_ARRAY(charbuf);
  }
 #else
  var const uintB* bptr = start;
  while (bptr != end) {
    write_code_char(&STACK_0,as_chart(*bptr));
    bptr++;
  }
 #endif
}

/* UP: Outputs an errorstring unchanged.
 write_errorasciz(asciz);
 > asciz: errorstring (a non-relocatable ASCIZ-string), in UTF-8 Encoding */
local void write_errorasciz (const char* asciz) {
  write_errorasciz_substring((const uintB*)asciz,
                             (const uintB*)(asciz + asciz_length(asciz)));
}

/* UP: Outputs an errorstring. At each tilde-S '~S' an object from the stack
 is printed, at each tilde-C '~C' a character from the stack is printed.
 write_errorstring(errorstring)
 > STACK_0: Stream etc.
 > errorstring: Errorstring (an non-relocatable ASCIZ-string),
                in UTF-8 Encoding
 > STACK_7, STACK_8, ...: arguments (for each '~S' resp. '~C' one argument),
   in reversed order as with FUNCALL !
 < result: STACK-value above the stream and the arguments */
local gcv_object_t* write_errorstring (const char* errorstring)
{
  var gcv_object_t* argptr = args_end_pointer STACKop 7; /* pointer above stream and frame */
  loop {
    var char ch = *errorstring; /* next character */
    if (ch==0) /* string finished? */
      break;
    if (ch=='~') { /* tilde? */
      if (errorstring[1]=='S') {
        /* print an object from stack: */
        write_errorobject(BEFORE(argptr));
        errorstring += 2;
        continue;
      }
      if (errorstring[1]=='C') {
        /* print a character from stack: */
        write_errorchar(BEFORE(argptr));
        errorstring += 2;
        continue;
      }
      pushSTACK(asciz_to_string(errorstring,Symbol_value(S(utf_8))));
      fehler(error,
             GETTEXT("internal error or error in message catalog: invalid low-level format string ~S"));
    }
    /* output all characters until the next special character */
    var const char* ptr = errorstring;
    loop {
      ptr++;
      ch = *ptr;
      if (ch==0 || ch=='~')
        break;
    }
    write_errorasciz_substring((const uintB*)errorstring,(const uintB*)ptr);
    errorstring = ptr;
  }
  return argptr;
}

/* SIGNAL the CONDITION and INVOKE the debugger */
nonreturning_function(local, signal_and_debug, (object condition)) {
  pushSTACK(condition); /* save condition */
  dynamic_bind(S(print_escape),T); /* bind *PRINT-ESCAPE* to NIL */
  dynamic_bind(S(print_readably),NIL); /* bind *PRINT-READABLY* to NIL */
  pushSTACK(condition); funcall(L(clcs_signal),1); /* (SIGNAL condition) */
  dynamic_bind(S(prin_stream),unbound); /* bind SYS::*PRIN-STREAM* to #<UNBOUND> */
  pushSTACK(STACK_(0+3+3+3)); /* condition */
  funcall(L(invoke_debugger),1); /* (INVOKE-DEBUGGER condition) */
  NOTREACHED;
}

/* finishes the output of an error message and starts a new driver,
 (when start_driver_p is true)
 can trigger GC */
local maygc void end_error (gcv_object_t* stackptr, bool start_driver_p) {
  elastic_newline(&STACK_0);
  if (nullp(STACK_1)) {
    /* *ERROR-HANDER* = NIL, SYS::*USE-CLCS* = NIL */
    skipSTACK(4); /* error message has already been printed */
    /* unbind binding frame for sys::*recursive-error-count*,
       because no error message output is active */
    dynamic_unbind(S(recursive_error_count));
    set_args_end_pointer(stackptr);
    break_driver(false); /* call break-driver (does not return) */
  } else {
    STACK_0 = get_output_stream_string(&STACK_0);
    var object arguments = nreverse(STACK_2);
    /* stack layout: type, args, handler, errorstring. */
    if (boundp(STACK_1)) {
      /* *ERROR-HANDER* /= NIL
         stack layout: nil, args, handler, errorstring.
         execute (apply *error-handler* nil errorstring args): */
      check_SP(); check_STACK();
      {
        var object error_handler = STACK_1; STACK_1 = NIL;
        apply(error_handler,2,arguments);
        skipSTACK(2);
      }
      /* unbind binding frame for sys::*recursive-error-count*,
         because no error message output is active */
      dynamic_unbind(S(recursive_error_count));
      set_args_end_pointer(stackptr);
      if (start_driver_p)
        break_driver(false); /* call break-driver (does not return) */
    } else {
      /* *ERROR-HANDER* = NIL, SYS::*USE-CLCS* /= NIL
         stack layout: type, args, --, errorstring. */
      var object type = STACK_3;
      var object errorstring = STACK_0;
      skipSTACK(4);
      dynamic_unbind(S(recursive_error_count));
      /* execute (APPLY #'coerce-to-condition errorstring args
                        'error type keyword-arguments) */
      pushSTACK(errorstring); pushSTACK(arguments);
      pushSTACK(S(error)); pushSTACK(type);
      var uintC argcount = 4;
      /* arithmetic-error, division-by-zero, floating-point-overflow,
         floating-point-underflow
         --> complete :operation :operands ??
         cell-error, uncound-variable, undefined-function, unbound-slot
         --> complete :name */
      if (eq(type,S(simple_cell_error))
          || eq(type,S(simple_unbound_variable))
          || eq(type,S(simple_undefined_function))
          || eq(type,S(simple_unbound_slot))) {
        pushSTACK(S(Kname)); pushSTACK(BEFORE(stackptr)); /* :name ... */
        argcount += 2;
      }
      /* unbound-slot --> complete :instance */
      if (eq(type,S(simple_unbound_slot))) {
        pushSTACK(S(Kinstance)); pushSTACK(BEFORE(stackptr)); /* :instance ... */
        argcount += 2;
      }
      /* type-error, keyword-error --> complete :datum, :expected-type */
      if (eq(type,S(simple_type_error))
          || eq(type,S(simple_keyword_error))
          || eq(type,S(simple_charset_type_error))) {
        pushSTACK(S(Kexpected_type)); pushSTACK(BEFORE(stackptr)); /* :expected-type ... */
        pushSTACK(S(Kdatum)); pushSTACK(BEFORE(stackptr)); /* :datum ... */
        argcount += 4;
      }
      /* argument-list-dotted --> complete :datum */
      if (eq(type,S(simple_argument_list_dotted))) {
        pushSTACK(S(Kexpected_type)); pushSTACK(S(list)); /* :expected-type 'LIST */
        pushSTACK(S(Kdatum)); pushSTACK(BEFORE(stackptr)); /* :datum ... */
        argcount += 4;
      }
      /* package-error --> complete :package */
      if (eq(type,S(simple_package_error))) {
        pushSTACK(S(Kpackage)); pushSTACK(BEFORE(stackptr)); /* :package ... */
        argcount += 2;
      }
      /* print-not-readable --> complete :object */
      if (eq(type,S(simple_print_not_readable))) {
        pushSTACK(S(Kobject)); pushSTACK(BEFORE(stackptr)); /* :object */
        argcount += 2;
      }
      /* stream-error, reader-error, end-of-file --> complete :stream */
      if (eq(type,S(simple_stream_error))
          || eq(type,S(simple_reader_error))
          || eq(type,S(simple_end_of_file))) {
        pushSTACK(S(Kstream)); pushSTACK(BEFORE(stackptr)); /* :stream ... */
        argcount += 2;
      }
      /* file-error --> complete :pathname */
      if (eq(type,S(simple_file_error))) {
        pushSTACK(S(Kpathname)); pushSTACK(BEFORE(stackptr)); /* :pathname ... */
        argcount += 2;
      }
      /* source-program-error --> complete :detail */
      if (eq(type,S(simple_source_program_error))) {
        pushSTACK(S(Kdetail)); pushSTACK(BEFORE(stackptr)); /* :detail ... */
        argcount += 2;
      }
      funcall(S(coerce_to_condition),argcount); /* SYS::COERCE-TO-CONDITION */
      set_args_end_pointer(stackptr);
      if (start_driver_p)
        signal_and_debug(value1);
    }
  }
}

/* helper -- see doc for fehler() */
local void prepare_error (condition_t errortype, const char* errorstring,
                          bool start_driver_p)
{ /* the common part of fehler(), check_value() &c */
  begin_error(); /* start error message */
  if (!nullp(STACK_3)) { /* *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ? */
    /* choose error-type-symbol for errortype: */
    var object sym = S(simple_condition); /* first error-type */
    sym = objectplus(sym,
                     (soint)(sizeof(*TheSymbol(sym))
                             <<(oint_addr_shift-addr_shift))
                     * (uintL)errortype);
    STACK_3 = sym;
  }
  end_error(write_errorstring(errorstring),start_driver_p); /* finish */
}

/* Error message with Errorstring. Does not return.
 fehler(errortype,errorstring);
 > errortype: condition type
 > errorstring: Constant ASCIZ-string, in UTF-8 Encoding.
   At each tilde-S a LISP-object is taken from STACK and printed instead of
   the tilde-S.
 > on the STACK: initialization values for the condition,
                 according to errortype */
nonreturning_function(global, fehler, (condition_t errortype,
                                       const char* errorstring)) {
  prepare_error(errortype,errorstring,true); /* finish error message */
  /* there is no point in using the condition system here:
     we will get into an infinite loop reporting the error */
  fprintf(stderr,"[%s:%d] cannot handle the fatal error due to a fatal error in the fatal error handler!\n",__FILE__,__LINE__);
  abort();
  /* NOTREACHED; */
}

/* Report an error and try to recover by asking the user to supply a value.
 check_value(errortype,errorstring);
 > errortype: condition-type
 > errorstring: constant ASCIZ-String, in UTF-8 Encoding.
   At every tilde-S, a LISP-object is taken from the STACK and printed
   instead of the tilde-S.
 > on the STACK: PLACE (form to be shown to the user) or NIL, then
   the initial values for the Condition, depending on error-type
 < value1, value2: return values from CHECK-VALUE:
   value1 = value supplied by the user,
   value2 = indicates whether PLACE should be filled
 < STACK: cleaned up
 can trigger GC */
global maygc void check_value (condition_t errortype, const char* errorstring)
{
  prepare_error(errortype,errorstring,nullpSv(use_clcs));
  /* if SYS::*USE-CLCS* /= NIL, use CHECK-VALUE */
  pushSTACK(value1); /* place is already on the stack! */
  funcall(S(check_value),2);
}

/* Report an error and try to recover by asking the user to choose among some
 alternatives.
 correctable_error(errortype,errorstring);
 > errortype: condition-type
 > errorstring: constant ASCIZ-String, in UTF-8 Encoding.
   At every tilde-S, a LISP-object is taken from the STACK and printed
   instead of the tilde-S.
 > on the STACK: list of alternatives
   ((restart-name restart-help-string . value-returned-by-the-restart)*), then
   the initial values for the Condition, depending on error-type
 < value1: return value from CORRECTABLE-ERROR, one of the CDDRs of the
   alternatives
 < STACK: cleaned up
 can trigger GC */
global maygc void correctable_error (condition_t errortype, const char* errorstring)
{
  prepare_error(errortype,errorstring,nullpSv(use_clcs));
  /* if SYS::*USE-CLCS* /= NIL, use CORRECTABLE-ERROR */
  pushSTACK(value1); /* options are already on the stack! */
  funcall(S(correctable_error),2);
}

#undef OS_error
#undef OS_file_error
#undef OS_filestream_error

#ifdef UNIX
  /* Define OS_error, OS_file_error. */
  #include "errunix.c"
#else
  /* Define just ANSIC_error. */
  #define OS_error ANSIC_error
  #define OS_error_internal ANSIC_error_internal
  #include "errunix.c"
  #undef OS_error_internal
  #undef OS_error
#endif /* UNIX */

#ifdef WIN32_NATIVE
  #include "errwin32.c"
#endif

/* Just like OS_error, but takes a channel stream and signals a FILE-ERROR.
 OS_filestream_error(stream);
 > stream: a channel stream
 > end_system_call() already called */
nonreturning_function(global, OS_filestream_error, (object stream)) {
  if (TheStream(stream)->strmtype == strmtype_file
      && !nullp(TheStream(stream)->strm_file_truename)) {
    OS_file_error(TheStream(stream)->strm_file_truename);
  } else {
    OS_error();
  }
}

LISPFUN(error,seclass_default,1,0,rest,nokey,0,NIL)
/* (ERROR errorstring {expr})
 Does not return.
 (defun error (errorstring &rest args)
   (if (or *error-handler* (not *use-clcs*))
     (progn
       (if *error-handler*
         (apply *error-handler* nil errorstring args)
         (progn
           (fresh-line *error-output*)
           (write-string "*** - " *error-output*)
           (apply #'format *error-output* errorstring args)
           (elastic-newline *error-output*)))
       (funcall *break-driver* nil))
     (let ((condition (coerce-to-condition errorstring args 'error
                                           'simple-error)))
       (signal condition)
       (invoke-debugger condition)))) */
{
  if (!nullpSv(error_handler) || nullpSv(use_clcs)) {
    begin_error(); /* start error message */
    rest_args_pointer skipSTACKop 1; /* pointer to the arguments */
    if (nullp(STACK_1)) {
      /* write error message:
         (FORMAT *ERROR-OUTPUT* errorstring {expr})
         (ELASTIC-NEWLINE *ERROR-OUTPUT*) */
      var object stream = STACK_0;
      skipSTACK(4);
      pushSTACK(stream);
      pushSTACK(stream);
      {
        var gcv_object_t* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,1+argcount, { pushSTACK(NEXT(ptr)); } );
      }
      funcall(S(format),2+argcount);
      funcall(L(elastic_newline),1);
    } else {
      /* write error message:
         ({handler} nil errorstring {expr}) */
      var object fun = STACK_1;
      skipSTACK(4);
      pushSTACK(NIL);
      {
        var gcv_object_t* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,1+argcount, { pushSTACK(NEXT(ptr)); } );
      }
      funcall(fun,2+argcount);
    }
    /* finish error message, cf. end_error(): */
    dynamic_unbind(S(recursive_error_count)); /* no error message output is active */
    set_args_end_pointer(rest_args_pointer); /* clean up STACK */
    break_driver(false); /* call break-driver (does not return) */
  } else {
    {
      var object arguments = listof(argcount);
      pushSTACK(arguments);
    }
    pushSTACK(S(error));
    pushSTACK(S(simple_error));
    funcall(S(coerce_to_condition),4); /* (SYS::COERCE-TO-CONDITION ...) */
    signal_and_debug(value1);
  }
  NOTREACHED;
}

/* (SYSTEM::%DEFCLCS error-types)
   sets the data needed for ERROR-OF-TYPE. */
LISPFUNN(defclcs,1)
{
  O(error_types) = popSTACK();
  VALUES0;
}

/* Converts a condition type into the corresponding Simple-Condition. */
local object convert_simple_condition (object type) {
  /* traverse vector O(error_types) like an Alist: */
  var object v = O(error_types);
  var uintL count = Svector_length(v);
  if (count > 0) {
    var gcv_object_t* ptr = &TheSvector(v)->data[0];
    dotimespL(count,count, {
      if (eq(type,Car(*ptr)))
        return Cdr(*ptr);
      ptr++;
    });
  }
  return type; /* not found -> leave type unchanged */
}

LISPFUN(cerror_of_type,seclass_default,3,0,rest,nokey,0,NIL)
/* (SYSTEM::CERROR-OF-TYPE continue-format-string type {keyword value}*
                           error-format-string {arg}*)
 (defun cerror-of-type (continue-format-string type &rest arguments)
   (let ((keyword-arguments '()))
     (loop
       (unless (and (consp arguments) (symbolp (car arguments))) (return))
       (push (pop arguments) keyword-arguments)
       (push (pop arguments) keyword-arguments))
     (setq keyword-arguments (nreverse keyword-arguments))
     (let ((error-format-string (first arguments))
           (args (rest arguments)))
       (apply #'cerror
         continue-format-string
         (if (or *error-handler* (not *use-clcs*))
           error-format-string
           (apply #'coerce-to-condition error-format-string args
                  'cerror (convert-simple-condition type) keyword-arguments))
         args)))) */
{
  var gcv_object_t* cfstring_ = &Next(rest_args_pointer STACKop 3);
  var uintC keyword_argcount = 0;
  rest_args_pointer skipSTACKop 1; /* pointer to the arguments behind type */
  while (argcount>=2) {
    var object next_arg = Next(rest_args_pointer); /* next argument */
    if (!symbolp(next_arg)) /* keyword? */
      break;
    rest_args_pointer skipSTACKop -2; argcount -= 2; keyword_argcount += 2;
  }
  /* next argument is hopefully a string. */
  if (!nullpSv(error_handler) || nullpSv(use_clcs)) {
    /* the type and the keyword-arguments are ignored. */
    BEFORE(rest_args_pointer) = *cfstring_;
    funcall(S(cerror),argcount+2);
    skipSTACK(keyword_argcount+1);
  } else {
    var object arguments = listof(argcount);
    /* stack layout: continue-format-string, type, {keyword, value}*,
                     errorstring.
      rearrange the stack a little bit: */
    var object errorstring = STACK_0;
    pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
    {
      var gcv_object_t* ptr2 = args_end_pointer;
      var gcv_object_t* ptr1 = ptr2 STACKop 4;
      var uintC count;
      dotimesC(count,keyword_argcount, { BEFORE(ptr2) = BEFORE(ptr1); } );
      BEFORE(ptr2) = convert_simple_condition(BEFORE(ptr1));
      BEFORE(ptr2) = S(cerror);
      BEFORE(ptr2) = arguments;
      BEFORE(ptr2) = errorstring;
      BEFORE(ptr2) = arguments;
    }
    /* stack layout: continue-format-string, arguments, errorstring, args,
                     CERROR, type, {keyword, value}*. */
    funcall(S(coerce_to_condition),4+keyword_argcount); /* (SYS::COERCE-TO-CONDITION ...) */
    /* stack layout: continue-format-string, arguments. */
    arguments = STACK_0;
    STACK_0 = value1;
    apply(S(cerror),2,arguments); /* (CERROR continue-format-string condition ...) */
  }
}

LISPFUN(error_of_type,seclass_default,2,0,rest,nokey,0,NIL)
/* (SYSTEM::ERROR-OF-TYPE type {keyword value}* errorstring {expr}*)
 does not return.
 (defun error-of-type (type &rest arguments)
   ;; split off keyword arguments from the &rest arguments:
   (let ((keyword-arguments '()))
     (loop
       (unless (and (consp arguments) (symbolp (car arguments))) (return))
       (push (pop arguments) keyword-arguments)
       (push (pop arguments) keyword-arguments))
     (setq keyword-arguments (nreverse keyword-arguments))
     (let ((errorstring (first arguments))
           (args (rest arguments)))
       (if (or *error-handler* (not *use-clcs*))
         (progn
           (if *error-handler*
             (apply *error-handler* nil errorstring args)
             (progn
               (fresh-line *error-output*)
               (write-string "*** - " *error-output*)
               (apply #'format *error-output* errorstring args)
               (elastic-newline *error-output*)))
           (funcall *break-driver* nil))
         (let ((condition
                 (apply #'coerce-to-condition errorstring args
                        'error (convert-simple-condition type)
                        keyword-arguments)))
           (signal condition)
           (invoke-debugger condition)))))) */
{
  var uintC keyword_argcount = 0;
  rest_args_pointer skipSTACKop 1; /* pointer to the arguments behind type */
  while (argcount>=2) {
    var object next_arg = Next(rest_args_pointer); /* next argument */
    if (!symbolp(next_arg)) /* keyword? */
      break;
    rest_args_pointer skipSTACKop -2; argcount -= 2; keyword_argcount += 2;
  }
  /* next argument is hopefully a string. */
  if (!nullpSv(error_handler) || nullpSv(use_clcs)) {
    /* the type and the keyword-arguments are ignored. */
    begin_error(); /* start error message */
    if (nullp(STACK_1)) {
      /* write error message:
         (FORMAT *ERROR-OUTPUT* errorstring {expr})
         (ELASTIC-NEWLINE *ERROR-OUTPUT*) */
      var object stream = STACK_0;
      skipSTACK(4);
      pushSTACK(stream);
      pushSTACK(stream);
      {
        var gcv_object_t* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,1+argcount, { pushSTACK(NEXT(ptr)); } );
      }
      funcall(S(format),2+argcount);
      funcall(L(elastic_newline),1);
    } else {
      /* write error message:
         ({handler} nil errorstring {expr}) */
      var object fun = STACK_1;
      skipSTACK(4);
      pushSTACK(NIL);
      {
        var gcv_object_t* ptr = rest_args_pointer;
        var uintC count;
        dotimespC(count,1+argcount, { pushSTACK(NEXT(ptr)); } );
      }
      funcall(fun,2+argcount);
    }
    /* finish error message, cf. end_error(): */
    dynamic_unbind(S(recursive_error_count)); /* no error message output is active */
    set_args_end_pointer(rest_args_pointer); /* clean up STACK */
    break_driver(false); /* call break-driver (does not return) */
  } else {
    var object arguments = listof(argcount);
    /* stack layout: type, {keyword, value}*, errorstring.
       rearrange the stack a little bit: */
    var object errorstring = STACK_0;
    pushSTACK(NIL); pushSTACK(NIL);
    {
      var gcv_object_t* ptr2 = args_end_pointer;
      var gcv_object_t* ptr1 = ptr2 STACKop 3;
      var uintC count;
      dotimesC(count,keyword_argcount, { BEFORE(ptr2) = BEFORE(ptr1); } );
      BEFORE(ptr2) = convert_simple_condition(BEFORE(ptr1));
      BEFORE(ptr2) = S(error);
      BEFORE(ptr2) = arguments;
      BEFORE(ptr2) = errorstring;
    }
    /* stack layout: errorstring, args, ERROR, type, {keyword, value}*. */
    funcall(S(coerce_to_condition),4+keyword_argcount); /* (SYS::COERCE-TO-CONDITION ...) */
    signal_and_debug(value1);
  }
  NOTREACHED;
}

LISPFUNN(invoke_debugger,1)
/* (INVOKE-DEBUGGER condition), CLtL2 p. 915
 does not return.
 (defun invoke-debugger (condition)
   (when *debugger-hook*
     (let ((debugger-hook *debugger-hook*)
           (*debugger-hook* nil))
       (funcall debugger-hook condition debugger-hook)))
   (funcall *break-driver* nil condition t)) */
{
  var object hook = Symbol_value(S(debugger_hook));
  if (!nullp(hook)) {
    var object condition = STACK_0;
    dynamic_bind(S(debugger_hook),NIL); /* bind *DEBUGGER-HOOK* to NIL */
    pushSTACK(condition); pushSTACK(hook); funcall(hook,2); /* call Debugger-Hook */
    dynamic_unbind(S(debugger_hook));
  }
  /* *BREAK-DRIVER* can be assumed here as /= NIL. */
  pushSTACK(NIL); pushSTACK(STACK_(0+1)); pushSTACK(T);
  funcall(Symbol_value(S(break_driver)),3); /* call break-driver */
  reset(1); /* returns unexpectedly -> back to the next loop */
  NOTREACHED;
}

/* UP: Executes a break-loop because of keyboard interrupt.
 > STACK_0 : calling function
 changes STACK, can trigger GC */
global maygc void tast_break (void)
{
  cancel_interrupts();
 #if defined(HAVE_SIGNALS) && defined(SIGPIPE)
  writing_to_subprocess = false;
 #endif
  if (!nullpSv(error_handler) || nullpSv(use_clcs)) {
    /* simulate begin_error(), 7 elements on the STACK: */
    pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
    pushSTACK(NIL); pushSTACK(NIL); pushSTACK(NIL);
    pushSTACK(var_stream(S(debug_io),strmflags_wr_ch_B)); /* Stream *DEBUG-IO* */
    fresh_line(&STACK_0); /* new line */
    write_sstring(&STACK_0,O(error_string1)); /* print "*** - " */
    /* print string, consume caller names, clean up STACK: */
    set_args_end_pointer(write_errorstring(GETTEXT("~S: User break")));
    break_driver(true); /* call break-driver */
  } else {
    pushSTACK(CLSTEXT("Continue execution"));
    pushSTACK(S(simple_interrupt_condition)); /* SYSTEM::[SIMPLE-]INTERRUPT-CONDITION */
    pushSTACK(CLSTEXT("~S: User break"));
    pushSTACK(STACK_(0+3)); /* caller */
    funcall(L(cerror_of_type),4); /* (SYS::CERROR-OF-TYPE "..." 'SYSTEM::[SIMPLE-]INTERRUPT-CONDITION "..." caller) */
    skipSTACK(1);
  }
}

LISPFUN(clcs_signal,seclass_default,1,0,rest,nokey,0,NIL)
/* (SIGNAL datum {arg}*), CLtL2 p. 888
 (defun signal (datum &rest arguments)
   (let ((condition ; CLtL2 p. 918 specifies this
           (coerce-to-condition datum arguments 'signal
                                'simple-condition)))
     (when (typep condition *break-on-signals*)
       ; Enter the debugger prior to signalling the condition
       (restart-case (invoke-debugger condition)
         (CONTINUE ())))
     (invoke-handlers condition)
     nil)) */
{
  {
    var object arguments = listof(argcount);
    pushSTACK(arguments);
  }
  pushSTACK(S(clcs_signal));
  pushSTACK(S(simple_condition));
  funcall(S(coerce_to_condition),4); /* (SYS::COERCE-TO-CONDITION ...) */
  pushSTACK(value1); /* save condition */
  pushSTACK(value1); pushSTACK(Symbol_value(S(break_on_signals)));
  funcall(S(safe_typep),2); /* (SYS::SAFE-TYPEP condition *BREAK-ON-SIGNALS*) */
  if (!nullp(value1)) {
    /* call break-driver: (funcall *break-driver* t condition t)
       *BREAK-DRIVER* can be assumed here as /= NIL . */
    pushSTACK(T); pushSTACK(STACK_(0+1)); pushSTACK(T);
    funcall(Symbol_value(S(break_driver)),3);
  }
  var object condition = popSTACK(); /* condition back */
  /* (CATCH 'SYS::DONE-SIGNALING ...). This can be used by handlers to override
     all other applicable handlers.
     Build CATCH frame: */
  pushSTACK(S(done_signaling));
  var gcv_object_t* top_of_frame = STACK STACKop 1; /* pointer above frame */
  var sp_jmp_buf returner; /* memorize return point */
  finish_entry_frame(CATCH,returner,, goto catch_return; );
  /* Call handlers: */
  invoke_handlers(condition);
 catch_return: /* we jump to this label, if the catch-frame built
                  above has caught a throw. */
  skipSTACK(3); /* unwind CATCH-frame */
  VALUES1(NIL);
}

/* check_classname(obj,type)
 > obj: an object
 > classname: a symbol expected to name a class with "proper name" classname
 < result: an object of the given type, either the same as obj or a replacement
 can trigger GC */
global maygc object check_classname (object obj, object type) {
  while (!typep_classname(obj,type)) {
    pushSTACK(type);            /* save type */
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(type);            /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(type); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not of type ~S"));
    obj = value1; type = popSTACK();
  }
  return obj;
}

#ifdef FOREIGN
/* check_fpointer_replacement(obj,restart_p)
 > obj: an object
 > restart_p: flag whether to allow entering a replacement
 < result: a valid foreign pointer, either the same as obj or a replacement
 can trigger GC */
global maygc object check_fpointer_replacement (object obj, bool restart_p) {
  for (;;) {
    if (!fpointerp(obj)) {
      pushSTACK(NIL);                /* no PLACE */
      pushSTACK(obj);                /* TYPE-ERROR slot DATUM */
      pushSTACK(S(foreign_pointer)); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(S(foreign_pointer)); pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      if (restart_p)
        check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
      else
        fehler(type_error,GETTEXT("~S: ~S is not a ~S"));
      obj = value1;
      continue;
    }
    if (!fp_validp(TheFpointer(obj))) {
      pushSTACK(NIL);                /* no PLACE */
      pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
      if (restart_p)
        check_value(error,GETTEXT("~S: ~S comes from a previous Lisp session and is invalid"));
      else
        fehler(error,GETTEXT("~S: ~S comes from a previous Lisp session and is invalid"));
      obj = value1;
      continue;
    }
    break;
  }
  return obj;
}
#endif

/* error-message, if an object is not a list.
 fehler_list(obj);
 > obj: non-list */
nonreturning_function(global, fehler_list, (object obj)) {
  pushSTACK(obj);     /* TYPE-ERROR slot DATUM */
  pushSTACK(S(list)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: ~S is not a list"));
}

/* check_list_replacement(obj)
 > obj: not a list
 < result: a list, a replacement
 can trigger GC */
global maygc object check_list_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);     /* TYPE-ERROR slot DATUM */
    pushSTACK(S(list)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a list"));
    obj = value1;
  } while (!listp(obj));
  return obj;
}

/* Error message, if an object isn't a proper list because it is dotted.
 fehler_proper_list_dotted(caller,obj);
 > caller: the caller (a symbol)
 > obj: end of the list, non-list */
nonreturning_function(global, fehler_proper_list_dotted, (object caller, object obj))
{
  pushSTACK(obj);                 /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_proper_list)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(caller);
  fehler(type_error,GETTEXT("~S: A proper list must not end with ~S"));
}

/* Error message, if an object isn't a proper list because it is circular.
 fehler_proper_list_circular(caller,obj);
 > caller: the caller (a symbol)
 > obj: circular list */
nonreturning_function(global, fehler_proper_list_circular, (object caller, object obj))
{
  dynamic_bind(S(print_circle),T); /* bind *PRINT-CIRCLE* to T */
  pushSTACK(obj);                 /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_proper_list)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(caller);
  fehler(type_error,GETTEXT("~S: A proper list must not be circular: ~S"));
}

/* check_symbol_replacement(obj)
 > obj: not a symbol
 < result: a symbol, a replacement
 can trigger GC */
global maygc object check_symbol_replacement (object obj) {
  do {
    var object caller = subr_self;
    caller = (subrp(caller) ? TheSubr(caller)->name : TheFsubr(caller)->name);
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);       /* TYPE-ERROR slot DATUM */
    pushSTACK(S(symbol)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(caller);
    check_value(type_error,GETTEXT("~S: ~S is not a symbol"));
    obj = value1;
  } while (!symbolp(obj));
  return obj;
}

/* check_symbol_non_constant_replacement(obj)
 > obj: not a non-constant symbol
 > caller: a symbol
 < result: a non-constant symbol, a replacement
 can trigger GC */
global maygc object check_symbol_non_constant_replacement (object obj, object caller)
{
  for (;;) {
    obj = check_symbol(obj);
    if (constant_var_p(TheSymbol(obj))) {
      pushSTACK(NIL); /* no PLACE */
      pushSTACK(obj); /* SOURCE-PROGRAM-ERROR slot DETAIL */
      pushSTACK(obj); pushSTACK(caller);
      check_value(source_program_error,
                  GETTEXT("~S: ~S is a constant, may not be used as a variable"));
      obj = value1;
    }
    break;
  }
  return obj;
}

/* UP: signal an error if a non-symbol was declared special
 returns the symbol
 can trigger GC */
global maygc object check_symbol_special (object obj, object caller)
{
  while (!symbolp(obj)) {
    pushSTACK(caller);
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* SOURCE-PROGRAM-ERROR slot DETAIL */
    pushSTACK(S(special)); pushSTACK(obj); pushSTACK(caller);
    check_value(source_program_error,
                GETTEXT("~S: ~S is not a symbol, cannot be declared ~S"));
    caller = popSTACK();
    obj = value1;
  }
  return obj;
}

/* error-message, if an object is not a simple-vector.
 fehler_kein_svector(caller,obj);
 > caller: caller (a symbol)
 > obj: non-Svector */
nonreturning_function(global, fehler_kein_svector, (object caller, object obj))
{
  pushSTACK(obj);              /* TYPE-ERROR slot DATUM */
  pushSTACK(S(simple_vector)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(S(simple_vector)); pushSTACK(obj); pushSTACK(caller);
  fehler(type_error,GETTEXT("~S: ~S is not a ~S"));
}

/* error-message, if an object is not a vector.
 fehler_vector(obj);
 > obj: non-vector */
nonreturning_function(global, fehler_vector, (object obj)) {
  pushSTACK(obj);       /* TYPE-ERROR slot DATUM */
  pushSTACK(S(vector)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: ~S is not a vector"));
}

/* check_array_replacement(obj)
 > obj: not an array
 < result: an array, a replacement
 can trigger GC */
global maygc object check_array_replacement (object obj) {
  do {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(S(array));        /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: argument ~S is not an array"));
    obj = value1;
  } while (!arrayp(obj));
  return obj;
}

/* check_vector_replacement(obj)
 > obj: not an vector
 < result: an vector, a replacement
 can trigger GC */
global maygc object check_vector_replacement (object obj) {
  do {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(S(vector));       /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: argument ~S is not a vector"));
    obj = value1;
  } while (!vectorp(obj));
  return obj;
}

/* check_byte_vector_replacement(obj)
 > obj: not an (ARRAY (UNSIGNED-BYTE 8) (*))
 < result: an (ARRAY (UNSIGNED-BYTE 8) (*)), a replacement
 can trigger GC */
global maygc object check_byte_vector_replacement (object obj) {
  do {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_uint8_vector)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(O(type_uint8_vector)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: argument ~S is not a vector of type ~S"));
    obj = value1;
  } while (!bit_vector_p(Atype_8Bit,obj));
  return obj;
}


/* error-message, if an object is not an environment.
 fehler_environment(obj);
 > obj: non-vector */
nonreturning_function(global, fehler_environment, (object obj)) {
  pushSTACK(obj);              /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_svector5)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: ~S may not be used as an environment"));
}

/* error-message, if an argument is not a Fixnum >=0 :
 fehler_posfixnum(obj);
 > obj: the erroneous argument */
nonreturning_function(global, fehler_posfixnum, (object obj)) {
  pushSTACK(obj);               /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_posfixnum)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: argument ~S should be a nonnegative fixnum"));
}

/* check_posfixnum_replacement(obj)
 > obj: not a fixnum >= 0
 < result: a fixnum >= 0, a replacement
 can trigger GC */
global maygc object check_posfixnum_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);               /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_posfixnum)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: argument ~S should be a nonnegative fixnum"));
    obj = value1;
  } while (!posfixnump(obj));
  return obj;
}

/* check_integer_replacement(obj)
 > obj: not an integer
 < result: an integer, a replacement
 can trigger GC */
global maygc object check_integer_replacement (object obj) {
  do {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(S(integer));      /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not an integer"));
    obj = value1;
  } while (!integerp(obj));
  return obj;
}

/* check_pos_integer_replacement(obj)
 > obj: not an integer >= 0
 < result: an integer >= 0, a replacement
 can trigger GC */
global maygc object check_pos_integer_replacement (object obj) {
  do {
    pushSTACK(NIL);                /* no PLACE */
    pushSTACK(obj);                /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_posinteger)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a non-negative integer"));
    obj = value1;
  } while (!(integerp(obj) && !R_minusp(obj)));
  return obj;
}

/* error-message, if an argument is not a Character:
 fehler_char(obj);
 > obj: the erroneous argument */
nonreturning_function(global, fehler_char, (object obj)) {
  pushSTACK(obj);          /* TYPE-ERROR slot DATUM */
  pushSTACK(S(character)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: argument ~S is not a character"));
}

/* check_char_replacement(obj)
 > obj: not a character
 < result: a character, a replacement
 can trigger GC */
global maygc object check_char_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);          /* TYPE-ERROR slot DATUM */
    pushSTACK(S(character)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: argument ~S is not a character"));
    obj = value1;
  } while (!charp(obj));
  return obj;
}

/* check_string_replacement(obj)
 > obj: not a string
 < result: a string, a replacement
 can trigger GC */
global maygc object check_string_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);       /* TYPE-ERROR slot DATUM */
    pushSTACK(S(string)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: argument ~S is not a string"));
    obj = value1;
  } while (!stringp(obj));
  return obj;
}

/* error-message, if an argument is not a Simple-String:
 > obj: the erroneous argument */
nonreturning_function(global, fehler_sstring, (object obj)) {
  pushSTACK(obj);              /* TYPE-ERROR slot DATUM */
  pushSTACK(S(simple_string)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(S(simple_string)); pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: argument ~S is not a ~S"));
}

/* error-message, if a Simple-String is immutable:
 fehler_sstring_immutable(obj);
 > obj: the String */
nonreturning_function(global, fehler_sstring_immutable, (object obj)) {
  pushSTACK(obj);
  fehler(error,GETTEXT("Attempt to modify a read-only string: ~S"));
}

/* Error message, if an argument is not of type (OR STRING INTEGER).
 fehler_string_integer(obj)  */
nonreturning_function(global, fehler_string_integer, (object obj)) {
  pushSTACK(obj);                    /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_string_integer)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,
         GETTEXT("~S: argument ~S is neither a string nor an integer"));
}

/* Error message, if a string size is too big.
 fehler_stringsize(size);
 > size: the desired string length  */
nonreturning_function(global, fehler_stringsize, (uintV size)) {
  var object obj = UV_to_I(size);
  pushSTACK(obj);                /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_stringsize)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj);
  fehler(type_error,
         GETTEXT("string too long: desired length ~S exceeds the supported maximum length"));
}

/* error message if an argument is not a class.
 fehler_class(caller,obj);
 > obj: the erroneous argument */
nonreturning_function(global, fehler_class, (object obj)) {
  pushSTACK(obj);      /* TYPE-ERROR slot DATUM */
  pushSTACK(S(class)); /* CLOS:CLASS, TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name); /* function name */
  fehler(type_error,GETTEXT("~S: ~S is not a class"));
}

/* error-message, if an argument is not a Stream:
 check_stream_replacement(obj);
 > obj: not a stream
 < obj: a stream
 can trigger GC */
global maygc object check_stream_replacement (object obj) {
  do {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(S(stream));       /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: argument ~S is not a stream"));
    obj = value1;
  } while (!streamp(obj));
  return obj;
}

/* Report an error when the argument is not an encoding:
 > obj: the (possibly) bad argument
 > default: what to return for :DEFAULT
 > keyword_p: true if the object comes from the :EXTERNAL-FORMAT argument
 < result: an encoding
 can trigger GC */
global maygc object check_encoding (object arg, const gcv_object_t *e_default,
                                    bool keyword_p) {
 restart:
  if (!boundp(arg) || eq(arg,S(Kdefault)))
    return *e_default;
  if (encodingp(arg))
    return arg;
 #ifdef UNICODE
  if (symbolp(arg) && constant_var_p(TheSymbol(arg))
      && encodingp(Symbol_value(arg)))
    return Symbol_value(arg);
  #ifdef HAVE_GOOD_ICONV
  if (stringp(arg)) {   /* (make-encoding :charset arg) */
    pushSTACK(arg);     /* :charset */
    pushSTACK(unbound); /* :line-terminator */
    pushSTACK(unbound); /* :input-error-action */
    pushSTACK(unbound); /* :output-error-action */
    pushSTACK(unbound); /* :if-does-not-exist */
    C_make_encoding();
    return value1;
  }
  #endif
 #else
  /* This is a hack to get away without an error. */
  if (symbolp(arg) && eq(Symbol_package(arg),O(charset_package)))
    return O(default_file_encoding);
 #endif
  if (eq(arg,S(Kunix)) || eq(arg,S(Kmac)) || eq(arg,S(Kdos))) {
    /* (make-encoding :charset default-file-encoding :line-terminator arg) */
    pushSTACK(*e_default); /* :charset */
    pushSTACK(arg);        /* :line-terminator */
    pushSTACK(unbound);    /* :input-error-action */
    pushSTACK(unbound);    /* :output-error-action */
    pushSTACK(unbound);    /* :if-does-not-exist */
    C_make_encoding();
    return value1;
  }
  pushSTACK(NIL); /* no PLACE */
  pushSTACK(arg);                     /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_external_format)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(arg);
  if (keyword_p) pushSTACK(S(Kexternal_format));
  pushSTACK(TheSubr(subr_self)->name);
  check_value(type_error,
              keyword_p ? GETTEXT("~S: illegal ~S argument ~S")
              : GETTEXT("~S: argument ~S is not a character set"));
  arg = value1;
  goto restart;
}

/* Error when the property list has odd length
 fehler_plist_odd(caller,plist);
 > plist: bad plist */
nonreturning_function(global, fehler_plist_odd, (object plist)) {
  pushSTACK(plist);             /* TYPE-ERROR slot DATUM */
  pushSTACK(S(plist));          /* TYPE-ERROR slot EXPECTED-TYPE*/
  pushSTACK(plist); pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: the property list ~S has an odd length"));
}

/* error-message for non-paired keyword-arguments
 fehler_key_odd(argcount,caller);
 > argcount: the number of arguments on the STACK
 > caller: function */
nonreturning_function(global, fehler_key_odd, (uintC argcount, object caller))
{
  var uintC count;
  pushSTACK(NIL); pushSTACK(NIL);
  for (count=0; count<argcount; count++) STACK_(count) = STACK_(count+2);
  STACK_(argcount) = caller;
  var object arglist = listof(argcount);
  STACK_1 = arglist;
  /* ANSI CL 3.5.1.6. wants a PROGRAM-ERROR here. */
  fehler(program_error,
         GETTEXT("~S: keyword arguments in ~S should occur pairwise"));
}

/* error-message for flawed keyword
 fehler_key_notkw(kw);
 > kw: Non-Symbol
 > caller: function */
nonreturning_function(global, fehler_key_notkw, (object kw, object caller)) {
  pushSTACK(kw);        /* KEYWORD-ERROR slot DATUM */
  pushSTACK(S(symbol)); /* KEYWORD-ERROR slot EXPECTED-TYPE */
  pushSTACK(kw); pushSTACK(S(LLkey)); pushSTACK(caller);
  fehler(keyword_error,
         GETTEXT("~S: ~S marker ~S is not a symbol"));
}

/* error-message for flawed keyword
 fehler_key_badkw(fun,kw,kwlist);
 > fun: function
 > key: illegal keyword
 > val: its value
 > kwlist: list of legal keywords */
nonreturning_function(global, fehler_key_badkw,
                      (object fun, object key, object val, object kwlist)) {
  pushSTACK(key); /* KEYWORD-ERROR slot DATUM */
  pushSTACK(kwlist);
  pushSTACK(kwlist);
  pushSTACK(val);
  pushSTACK(key);
  pushSTACK(fun);
  { /* `(MEMBER ,@kwlist) = KEYWORD-ERROR slot EXPECTED-TYPE */
    var object type = allocate_cons();
    Car(type) = S(member); Cdr(type) = STACK_4;
    STACK_4 = type;
  }
  fehler(keyword_error,
         GETTEXT("~S: illegal keyword/value pair ~S, ~S in argument list.\n"
                 "The allowed keywords are ~S"));
}

/* check_function_replacement(obj)
 > obj: not a function
 < result: a function, a replacement
 can trigger GC */
global maygc object check_function_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);         /* TYPE-ERROR slot DATUM */
    pushSTACK(S(function)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a function"));
    if (symbolp(value1))
      obj = Symbol_function(value1);
    else if (funnamep(value1)) {
      var object name = get(Car(Cdr(value1)),S(setf_function));
      if (symbolp(name)) obj = Symbol_function(name);
      else obj = value1;
    } else if (consp(value1) && eq(Car(value1),S(lambda))) {
      pushSTACK(value1); pushSTACK(S(function));
      funcall(L(coerce),2);
      obj = value1;
    } else obj = value1;
  } while (!functionp(obj));
  return obj;
}

/* error if funname does not have a function definition
 check_fdefinition(funname,caller)
 > funname: symbol or (setf symbol)
 > caller: symbol
 < a function object, possibly also installed as (FDEFINITION funname)
 can trigger GC */
global maygc object check_fdefinition (object funname, object caller)
{
  var object name = (symbolp(funname) ? funname
                     : get(Car(Cdr(funname)),S(setf_function)));
  var object def = (symbolp(name) ? (object)Symbol_function(name) : unbound);
  var bool store_p = false;
  while (!functionp(def)) {
    pushSTACK(caller); pushSTACK(funname); /* save */
    pushSTACK(S(quote)); pushSTACK(funname); def = listof(2);
    pushSTACK(S(fdefinition)); pushSTACK(def); def = listof(2);
    pushSTACK(def); /* PLACE */
    pushSTACK(STACK_1/*funname*/); /* CELL-ERROR Slot NAME */
    pushSTACK(STACK_0); /* funname */
    pushSTACK(STACK_4); /* caller */
    check_value(undefined_function,GETTEXT("~S: undefined function ~S"));
    /* value2 selects the restart: 0: CONTINUE, T: STORE-VALUE, else: USE-VALUE
       see also condition.lisp:check-value */
    store_p = eq(value2,T);
    /* this is the only place where check_value()'s second value is checked
       for something other than non-NIL */
    if (eq(value2,Fixnum_0)) { /* RETRY restart */
      funname = STACK_0;
      name = (symbolp(funname) ? funname
              : get(Car(Cdr(funname)),S(setf_function)));
      value1 = (symbolp(name) ? (object)Symbol_function(name) : unbound);
    }
    funname = popSTACK(); caller = popSTACK(); /* restore */
    def = value1;
  }
  if (store_p) {                /* STORE-VALUE restart */
    name = (symbolp(funname) ? funname
            : get(Car(Cdr(funname)),S(setf_function)));
    if (!symbolp(name)) {
      pushSTACK(Car(Cdr(funname))); /* the symbol in (setf symbol) */
      pushSTACK(def); /* save new function */
      pushSTACK(funname); funcall(S(get_funname_symbol),1);
      pushSTACK(value1); /* save new name */
      pushSTACK(value1); pushSTACK(S(setf_function)); pushSTACK(STACK_4);
      funcall(L(put),3); /* (put symbol 'setf-function name) */
      name = popSTACK(); def = popSTACK(); /* restore */
      skipSTACK(1); /* drop symbol in (setf symbol) */
    }
    Symbol_function(name) = def;
  }
  return def;
}

/* check_funname_replacement(obj)
 > errtype: type of condition to signal if obj is not a function name,
            either type_error or source_program_error
 > caller: a symbol
 > obj: not a function name
 < result: a function name, either the same as obj or a replacement
 can trigger GC */
global maygc object check_funname_replacement (condition_t errtype, object caller, object obj) {
  pushSTACK(caller); /* save */
  do {
    caller = STACK_0;
    pushSTACK(NIL); /* no PLACE */
    switch (errtype) {
      case type_error:
        pushSTACK(obj);                   /* TYPE-ERROR slot DATUM */
        pushSTACK(O(type_function_name)); /* slot EXPECTED-TYPE */
        break;
      case source_program_error:
        pushSTACK(obj);         /* SOURCE-PROGRAM-ERROR slot DETAIL */
        break;
      default: NOTREACHED;
    }
    pushSTACK(obj); pushSTACK(caller);
    check_value(errtype,GETTEXT("~S: ~S is not a function name; try using a symbol instead"));
    obj = value1;
  } while (!funnamep(obj));
  skipSTACK(1); /* drop caller */
  return obj;
}

/* error-message, if an argument is a lambda-expression instead of a function:
 caller: caller (a symbol)
 obj: the erroneous argument */
nonreturning_function(global, fehler_lambda_expression,
                      (object caller, object obj)) {
  pushSTACK(obj);         /* TYPE-ERROR slot DATUM */
  pushSTACK(S(function)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(obj); pushSTACK(caller);
  fehler(type_error,
         GETTEXT("~S: argument ~S is not a function.\n"
                 "To get a function in the current environment, write (FUNCTION ...).\n"
                 "To get a function in the global environment, write (COERCE '... 'FUNCTION)."));
}

/* too many arguments in a function call
 > caller : the function that is reporting the error
 > func   : the function being incorrectly called
 > ngiven : the number of arguments given
 < nmax   : the maximum number of arguments accepted */
nonreturning_function(global, fehler_too_many_args,
                      (object caller, object func, uintL ngiven, uintL nmax)) {
  pushSTACK(func);
  pushSTACK(fixnum(nmax));
  pushSTACK(fixnum(ngiven));
  /* ANSI CL 3.5.1.3. wants a PROGRAM-ERROR here. */
  if (!boundp(caller))
    fehler(program_error,GETTEXT("EVAL/APPLY: Too many arguments (~S instead of at most ~S) given to ~S"));
  else {
    pushSTACK(caller);
    fehler(program_error,GETTEXT("~S: Too many arguments (~S instead of at most ~S) given to ~S"));
  }
}

/* too few arguments in a function call
 > caller : the function that is reporting the error (unbound == EVAL/APPLY)
 > func   : the function being incorrectly called
 > ngiven : the number of arguments given
 < nmin   : the minimum number of arguments required */
nonreturning_function(global, fehler_too_few_args,
                      (object caller, object func, uintL ngiven, uintL nmin)) {
  pushSTACK(func);
  pushSTACK(fixnum(nmin));
  pushSTACK(fixnum(ngiven));
  /* ANSI CL 3.5.1.2. wants a PROGRAM-ERROR here. */
  if (!boundp(caller))
    fehler(program_error,GETTEXT("EVAL/APPLY: Too few arguments (~S instead of at least ~S) given to ~S"));
  else {
    pushSTACK(caller);
    fehler(program_error,GETTEXT("~S: Too few arguments (~S instead of at least ~S) given to ~S"));
  }
}

# Error message, if an argument isn't of a given elementary C type.
# fehler_<ctype>(obj);
# > obj: the faulty argument
nonreturning_function(global, fehler_uint8, (object obj)) {
  pushSTACK(obj);           /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_uint8)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(fixnum(8)); pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: argument ~S is not a nonnegative integer with at most ~S bits"));
}
nonreturning_function(global, fehler_sint8, (object obj)) {
  pushSTACK(obj);           /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_sint8)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(fixnum(8)); pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: argument ~S is not an integer with at most ~S bits (including the sign bit)"));
}
nonreturning_function(global, fehler_uint16, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_uint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(fixnum(16)); pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: argument ~S is not a nonnegative integer with at most ~S bits"));
}
nonreturning_function(global, fehler_sint16, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_sint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(fixnum(16)); pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: argument ~S is not an integer with at most ~S bits (including the sign bit)"));
}
nonreturning_function(global, fehler_uint32, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_uint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(fixnum(32)); pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: argument ~S is not a nonnegative integer with at most ~S bits"));
}
nonreturning_function(global, fehler_sint32, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_sint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(fixnum(32)); pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: argument ~S is not an integer with at most ~S bits (including the sign bit)"));
}
nonreturning_function(global, fehler_uint64, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_uint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(fixnum(64)); pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: argument ~S is not a nonnegative integer with at most ~S bits"));
}
nonreturning_function(global, fehler_sint64, (object obj)) {
  pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_sint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(fixnum(64)); pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: argument ~S is not an integer with at most ~S bits (including the sign bit)"));
}

/* error, if argument is not an integer in the range of the C type 'uint8'.
 check_uint8_replacement(obj)
 > obj: not an integer in the range of uint8
 < obj: an integer in the range of uint8
 can trigger GC */
global maygc object check_uint8_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);           /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_uint8)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not an 8-bit number"));
    obj = value1;
  } while (!uint8_p(obj));
  return obj;
}

/* error, if argument is not an integer in the range of the C type 'sint8'.
 check_sint8_replacement(obj)
 > obj: not an integer in the range of sint8
 < obj: an integer in the range of sint8
 can trigger GC */
global maygc object check_sint8_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);           /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_sint8)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not an 8-bit number"));
    obj = value1;
  } while (!sint8_p(obj));
  return obj;
}

/* error, if argument is not an integer in the range of the C type 'uint16'.
 check_uint16_replacement(obj)
 > obj: not an integer in the range of uint16
 < obj: an integer in the range of uint16
 can trigger GC */
global maygc object check_uint16_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_uint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a 16-bit number"));
    obj = value1;
  } while (!uint16_p(obj));
  return obj;
}

/* error, if argument is not an integer in the range of the C type 'sint16'.
 check_sint16_replacement(obj)
 > obj: not an integer in the range of sint16
 < obj: an integer in the range of sint16
 can trigger GC */
global maygc object check_sint16_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_sint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a 16-bit number"));
    obj = value1;
  } while (!sint16_p(obj));
  return obj;
}

/* error, if argument is not an integer in the range of the C type 'uint32'.
 check_uint32_replacement(obj)
 > obj: not an integer in the range of uint32
 < obj: an integer in the range of uint32
 can trigger GC */
global maygc object check_uint32_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_uint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a 32-bit number"));
    obj = value1;
  } while (!uint32_p(obj));
  return obj;
}

/* error, if argument is not an integer in the range of the C type 'sint32'.
 check_sint32_replacement(obj)
 > obj: not an integer in the range of sint32
 < obj: an integer in the range of sint32
 can trigger GC */
global maygc object check_sint32_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_sint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a 32-bit number"));
    obj = value1;
  } while (!sint32_p(obj));
  return obj;
}

/* error, if argument is not an integer in the range of the C type 'uint64'.
 check_uint64_replacement(obj)
 > obj: not an integer in the range of uint64
 < obj: an integer in the range of uint64
 can trigger GC */
global maygc object check_uint64_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_uint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a 64-bit number"));
    obj = value1;
  } while (!uint64_p(obj));
  return obj;
}

/* error, if argument is not an integer in the range of the C type 'sint64'.
 check_sint64_replacement(obj)
 > obj: not an integer in the range of sint64
 < obj: an integer in the range of sint64
 can trigger GC */
global maygc object check_sint64_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_sint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a 64-bit number"));
    obj = value1;
  } while (!sint64_p(obj));
  return obj;
}

/* error, if argument is not an integer in the range of the C type 'uint'.
 check_uint_replacement(obj)
 > obj: not an integer in the range of uint
 < obj: an integer in the range of uint
 can trigger GC */
global maygc object check_uint_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
   #if (int_bitsize==16)
    pushSTACK(O(type_uint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #else /* (int_bitsize==32) */
    pushSTACK(O(type_uint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #endif
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not an `unsigned int' number"));
    obj = value1;
  } while (!uint_p(obj));
  return obj;
}

/* error, if argument is not an integer in the range of the C type 'sint'.
 check_sint_replacement(obj)
 > obj: not an integer in the range of sint
 < obj: an integer in the range of sint
 can trigger GC */
global maygc object check_sint_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
   #if (int_bitsize==16)
    pushSTACK(O(type_sint16)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #else /* (int_bitsize==32) */
    pushSTACK(O(type_sint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #endif
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not an `int' number"));
    obj = value1;
  } while (!sint_p(obj));
  return obj;
}

/* error, if argument is not an integer in the range of the C type 'ulong'.
 check_ulong_replacement(obj)
 > obj: not an integer in the range of ulong
 < obj: an integer in the range of ulong
 can trigger GC */
global maygc object check_ulong_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
   #if (long_bitsize==32)
    pushSTACK(O(type_uint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #else /* (long_bitsize==64) */
    pushSTACK(O(type_uint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #endif
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a `unsigned long' number"));
    obj = value1;
  } while (!ulong_p(obj));
  return obj;
}

/* error, if argument is not an integer in the range of the C type 'slong'.
 check_slong_replacement(obj)
 > obj: not an integer in the range of slong
 < obj: an integer in the range of slong
 can trigger GC */
global maygc object check_slong_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);            /* TYPE-ERROR slot DATUM */
   #if (long_bitsize==32)
    pushSTACK(O(type_sint32)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #else /* (long_bitsize==64) */
    pushSTACK(O(type_sint64)); /* TYPE-ERROR slot EXPECTED-TYPE */
   #endif
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a `long' number"));
    obj = value1;
  } while (!slong_p(obj));
  return obj;
}

/* error, if argument is not a Single-Float.
 check_ffloat_replacement(obj)
 > obj: not a single-float
 < obj: a single-float
 can trigger GC */
global maygc object check_ffloat_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(S(single_float)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a single-float"));
    obj = value1;
  } while (!single_float_p(obj));
  return obj;
}

/* error, if argument is not a Double-Float.
 check_dfloat_replacement(obj)
 > obj: not a double-float
 < obj: a double-float
 can trigger GC */
global maygc object check_dfloat_replacement (object obj) {
  do {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(S(double_float)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a double-float"));
    obj = value1;
  } while (!double_float_p(obj));
  return obj;
}
