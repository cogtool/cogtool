/*
 * top level loop, aux functions for the debugger, stepper for CLISP
 * Bruno Haible 1990-2005
 * ILISP friendliness: Marcus Daniels 8.4.1994
 * Sam Steingold 2001-2004
 * German comments translated into English: Stefan Kain 2004-08-30
 */

#include "lispbibl.c"


/* ----------------------------------------------------------------------- */
/* Top-Level-Loop */

/* SYS::READ-FORM realizes the following features of the top-level REP loop
   and of the debug REP loop:

   - The prompt. When the input stream is interactive, a prompt is printed
     that reminds the user that the system is expecting a Lisp form.
     "> " or "1. Break> " or something like this.
     When the input stream is not interactive, such as in "clisp < in.lisp",
     no prompt is emitted, because these prompt would accumulate on standard
     output, without being useful.
     We use the criterion "interactive", not the criterion "output device =
     input device", because in situations like "clisp | tee logfile", which
     are interactive but with different devices for input and output, the
     prompt is desirable.

   - The continuation prompt [not yet implemented]. When the input stream is
     interactive a short prompt is printed after each newline that reminds
     the user if the reader is reading a string "..." or symbol |...| which
     is not yet complete.

   - The key/command bindings. The user can enter some special words like
     "help", ":q", "continue", "abort", which are recognized as commands.
     They can be given as argument or via SYS::*KEY-BINDINGS*. (The name of
     this variable comes from the Atari ST time, when most of the commands
     were accessible through function keys F1..F10.) Example:

     [1]> (/ 0)

     *** - division by zero
     The following restarts are available:
     ABORT          :R1      ABORT

     Break 1 [2]> abort

     [3]>

   - Support for paste: When a user pastes a couple of forms into the command
     line, all are executed.

     [1]> (setq x 3) (setq y 4) (setq z (sqrt (+ (* x x) (* y y))))
     3
     [2]>
     4
     [3]>
     5
     [4]>

   - Likewise for multiline paste:

     [1]> (setq x 3)
          (setq y 4)
          (setq z (sqrt (+ (* x x) (* y y))))
     3
     [2]>
     4
     [3]>
     5
     [4]>

   - Separation of form input and data input.

     Form input is not mistakenly considered as data. Example:

     [1]> (read-line) (cons 'a 'b)
     data
     "data" ;
     NIL
     [2]>
     (A . B)
     [3]>

     *not*

     [1]> (read-line) (cons 'a 'b)
     "(cons 'a 'b)" ;
     NIL
     [2]>

     Data input is not mistakenly considered as forms. Example:

     [1]> (read-char)
     abcdef
     #\a
     [2]>

     *not*

     [1]> (read-char)
     abcdef
     #\a
     [2]>
     *** - EVAL: variable BCDEF has no value

*/

/* (SYS::READ-FORM ostream istream prompt [commandlist])
 read one form (interactively) from the input stream.
 instead of the form, we also recognize special commands from commandlist
 (a fresh alist) or SYS::*KEY-BINDINGS*
 > STACK_1: prompt, a string
 > STACK_0: commandlist (fresh aliste) or #<UNBOUND>
 < STACK_1: Output-Stream *standard-output*
 < STACK_0: Input-Stream *standard-input*
 < mv_space/mv_count: value = form, NIL or (on EOF) T, T
 can trigger GC */
local maygc Values read_form(void)
{ /*
 (defun read-form (ostream istream prompt &optional (command-list nil))
   (loop
     (let ((raw (terminal-raw istream nil)))
       (when (interactive-stream-p istream)
         (fresh-line ostream)
         (write-string prompt ostream)
         (force-output ostream))
       (let* ((eof-value "EOF")
              (form (let ((*read-suppress* nil)
                          (*key-bindings* (nreconc command-list
                                                   *key-bindings*)))
                      (read istream nil eof-value nil))))
         (terminal-raw istream raw)
         (if (eql form eof-value)
           (progn (clear-input istream) (setq istream *debug-io*))
           (progn (clear-input-upto-newline istream)
                  (return (values form nil))))))))   */
 #if STACKCHECKR
  var gcv_object_t* STACKbefore = STACK; /* retain STACK for later */
 #endif
  pushSTACK(STACK_1); pushSTACK(STACK_1);
  STACK_3 = var_stream(S(standard_output),strmflags_wr_ch_B); /* ostream := *STANDARD-OUTPUT* */
  STACK_2 = var_stream(S(standard_input),strmflags_rd_ch_B); /* istream := *STANDARD-INPUT* */
  /* stack layout: ostream, istream, prompt, command-list. */
  pushSTACK(STACK_2);
  pushSTACK(STACK_3); pushSTACK(NIL); funcall(L(terminal_raw),2);
  pushSTACK(value1);
  /* stack layout: ostream, istream, prompt, command-list, inputstream, raw. */
  var signean status = listen_char(STACK_4);
  if (ls_eof_p(status) && !boundp(Symbol_value(S(terminal_read_stream))))
    goto eof;
  /* already have characters available (and not in ilisp_mode) -> no prompt */
  if (ilisp_mode || interactive_stream_p(STACK_4)) {
    /* interactive input-stream -> prompt output: */
   #if 0
    fresh_line(&STACK_5); /* (FRESH-LINE ostream) */
   #else
    /* the same, but avoiding infinite recursion
     (let ((*recurse-count-standard-output*
            (1+ *recurse-count-standard-output*)))
       (when (> *recurse-count-standard-output* 3)
         (setq *recurse-count-standard-output* 0)
         (makunbound (quote *standard-output*))
         (let ((*recurse-count-debug-io* (1+ *recurse-count-debug-io*)))
           (when (> *recurse-count-debug-io* 3)
             (setq *recurse-count-debug-io* 0)
             (makunbound (quote *debug-io*))
             (symbol-stream (quote *debug-io*) :io))
           (symbol-stream (quote *standard-output*) :output)))
       (fresh-line *standard-output*)) */
    /* (incf sys::*recurse-count-standard-output*) */
    dynamic_bind(S(recurse_count_standard_output),
                 fixnum_inc(Symbol_value(S(recurse_count_standard_output)),1));
    if (!posfixnump(Symbol_value(S(recurse_count_standard_output))))
      /* should be fixnum >=0; otherwise emergency correction */
      Symbol_value(S(recurse_count_standard_output)) = Fixnum_0;
    if (posfixnum_to_V(Symbol_value(S(recurse_count_standard_output))) > 3) {
      /* too many nested i/o errors. */
      Symbol_value(S(recurse_count_standard_output)) = Fixnum_0;
      Symbol_value(S(standard_output)) = unbound;
       /* (incf sys::*recurse-count-debug-io*): */
      dynamic_bind(S(recurse_count_debug_io),
                   fixnum_inc(Symbol_value(S(recurse_count_debug_io)),1));
      if (!posfixnump(Symbol_value(S(recurse_count_debug_io))))
        /* should be fixnum >=0; otherwise emergency correction */
        Symbol_value(S(recurse_count_debug_io)) = Fixnum_0;
      if (posfixnum_to_V(Symbol_value(S(recurse_count_debug_io))) > 3) {
        /* too many nested i/o errors. */
        Symbol_value(S(recurse_count_debug_io)) = Fixnum_0;
        Symbol_value(S(debug_io)) = unbound;
        var_stream(S(debug_io),strmflags_rd_ch_B|strmflags_wr_ch_B);
      }
      STACK_(5+3+3) = var_stream(S(standard_output),strmflags_wr_ch_B); /* ostream := *STANDARD-OUTPUT* */
      dynamic_unbind(S(recurse_count_debug_io));
    }
    fresh_line(&STACK_(5+3)); /* (FRESH-LINE ostream) */
    dynamic_unbind(S(recurse_count_standard_output));
   #endif
    write_string(&STACK_5,STACK_3); /* (WRITE-STRING prompt ostream) */
    force_output(STACK_5);
  } /* Prompt OK */
  {
    var gcv_object_t* inputstream_ = &STACK_1;
  #if 0
    /* That proves nevertheless awkward: If one presses CTRL-C during input,
       then one has some commands then in the BREAK loop doubly in the list */
    {
      var object list = Symbol_value(S(key_bindings)); /* old Key-Bindings */
      if (boundp(STACK_2)) /* command-list supplied? */
        list = nreconc(STACK_2,list); /* add in front */
      dynamic_bind(S(key_bindings),list); /* bind SYS::*KEY-BINDINGS* */
    }
   #else
    {
      var object list = (!boundp(STACK_2) ? NIL : (object)STACK_2);
      dynamic_bind(S(key_bindings),list); /* bind SYS::*KEY-BINDINGS* */
    }
   #endif
   #if !defined(TERMINAL_USES_KEYBOARD) /*  Atari - function keys */
    var bool terminal_read_stream_bound = false;
    if (interactive_stream_p(*(inputstream_ STACKop 3)) /* only for interactive input streams */
        && !boundp(Symbol_value(S(terminal_read_stream)))) {
      /* look for commands, not forms:
       (multiple-value-bind (line flag) (read-line istream)
         (let ((h (assoc line *key-bindings* :test (function string-equal))))
           (when h (funcall (cdr h)) (return t)))
         (setq istream
               (make-concatenated-stream
                (make-string-input-stream
                 (if flag line
                     (concatenate (quote string) line (string #\Newline))))
                istream))) */
      do {
        /* this loop is for win32 and its C-z RET abomination: after
           C-z (EOF) is processed, there is an empty line in the stream */
        pushSTACK(*inputstream_); pushSTACK(NIL); pushSTACK(NIL);
        funcall(L(read_line),3); /* (READ-LINE istream nil nil) */
        if (nullp(value1)) { /* EOF at line start? */
          dynamic_unbind(S(key_bindings));
          goto eof;
        }
      } while (Sstring_length(value1) == 0);
      var object line = value1; /* non-trivial line */
      /* NB: READ-LINE returns a SIMPLE-STRING in CLISP, so line is simple */
      { /* search for line in *KEY-BINDINGS*: */
        var object alist = Symbol_value(S(key_bindings));
        var uintL input_len = Sstring_length(line);
        for (;consp(alist);alist = Cdr(alist))
          if (mconsp(Car(alist)) && simple_string_p(Car(Car(alist)))) {
            var object key = Car(Car(alist));
            sstring_un_realloc(key);
            var uintL len = Sstring_length(key);
            /* check whether the line starts with the key and a whitespace */
            if ((len <= input_len) && string_eqcomp_ci(line,0,key,0,len)) {
              if (len == input_len) goto found;
              /* now len < input_len */
              { var chart ch = schar(line,len);
                if (cint_white_p(as_cint(ch))) goto found;
              }
              if (false) {
               found:
                funcall(Cdr(Car(alist)),0); /* call the appropriate function */
                dynamic_unbind(S(key_bindings));
                goto eof;
              }
            }
          }
      }
      /* create string-input-stream for this line: */
      if (nullp(value2)) {
        pushSTACK(line); pushSTACK(O(newline_string));
        line = string_concat(2); /* maybe need another Newline */
      }
      pushSTACK(line); funcall(L(make_string_input_stream),1);
      /* make concatenated-stream: */
      pushSTACK(value1); pushSTACK(*inputstream_);
      funcall(L(make_concatenated_stream),2);
      dynamic_bind(S(terminal_read_stream),value1);
      terminal_read_stream_bound = true;
      *inputstream_ = Symbol_value(S(terminal_read_stream));
    } else if (streamp(Symbol_value(S(terminal_read_stream)))) {
      var object stream = Symbol_value(S(terminal_read_stream));
      Symbol_value(S(terminal_read_stream)) = unbound;
      dynamic_bind(S(terminal_read_stream),stream);
      terminal_read_stream_bound = true;
      *inputstream_ = Symbol_value(S(terminal_read_stream));
    }
   #endif  /* !defined(TERMINAL_USES_KEYBOARD) */
    dynamic_bind(S(read_suppress),NIL); /* *READ-SUPPRESS* = NIL */
    /* read object (recursive-p=NIL, whitespace-p=NIL): */
    var object obj = stream_read(inputstream_,NIL,NIL);
    dynamic_unbind(S(read_suppress));
   #if !defined(TERMINAL_USES_KEYBOARD)
    if (terminal_read_stream_bound) {
      var object old_trs = Symbol_value(S(terminal_read_stream));
      dynamic_unbind(S(terminal_read_stream));
      if (streamp(old_trs)) {
        /* maybe need to process something from the first line? */
        var object strm_list = TheStream(old_trs)->strm_concat_list;
        pushSTACK(obj); /* save before PEEK-CHAR */
        pushSTACK(old_trs); /* save before PEEK-CHAR */
        Symbol_value(S(terminal_read_stream)) =
          (consp(strm_list) && !nullp(Cdr(strm_list))
           /* some input on the first line was not processed ? */
           && (pushSTACK(T), pushSTACK(Car(strm_list)),
               pushSTACK(NIL), pushSTACK(eof_value),
               funcall(L(peek_char),4), !eq(value1,eof_value)))
          ? STACK_0 : (gcv_object_t)unbound;
        skipSTACK(1); /* drop old_trs */
        obj = popSTACK();
      }
    }
   #endif
    dynamic_unbind(S(key_bindings));
    if (!eq(obj,eof_value)) { /* EOF test (after whitespace) */
      pushSTACK(obj);
      pushSTACK(STACK_(4+1)); pushSTACK(STACK_(0+1+1)); funcall(L(terminal_raw),2);
      /* If not at the beginning of a line, delete input till EOL: */
      if (interactive_stream_p(STACK_(4+1))
          && !eq(stream_get_lastchar(STACK_(4+1)),ascii_char(NL))) {
        while (ls_avail_p(listen_char(STACK_(4+1)))) {
          var object ch = peek_char(&STACK_(4+1));
          if (eq(ch,eof_value))
            break;
          read_char(&STACK_(4+1));
          if (eq(ch,ascii_char(NL)))
            break;
        }
      }
      VALUES2(popSTACK(), NIL); /* (values obj NIL) */
      skipSTACK(4);
     #if STACKCHECKR
      if (STACK != STACKbefore) /* verify if Stack is cleaned up */
        abort(); /* if not --> go to Debugger */
     #endif
      return;
    }
  }
 eof: /* reached EOF */
  pushSTACK(STACK_4); pushSTACK(STACK_(0+1)); funcall(L(terminal_raw),2);
  /* call (CLEAR-INPUT istream) to eat EOF from an interactive stream,
     because a continuable program could misunderstand the EOF: */
  clear_input(STACK_4);
  VALUES2(T,T); /* (values T T) */
  skipSTACK(4);
 #if STACKCHECKR
  if (STACK != STACKbefore) /* verify that STACK is cleaned up */
    abort(); /* if not --> go to Debugger */
 #endif
}

/* (SYS::READ-FORM prompt [commandlist])
 reads a form (interactively) from *standard-input*.
 prompt must be a String.
 A special key from commandlist (a fresh
 Alist) or SYS::*KEY-BINDINGS* can be entered instead of a form.
 Values: form, NIL or (on EOF) T, T */
LISPFUN(read_form,seclass_default,1,1,norest,nokey,0,NIL)
{ read_form(); skipSTACK(2); }

/* (SYS::READ-EVAL-PRINT prompt [commandlist])
 reads a form, evaluates it and prints the values.
 prompt must be a String.
 A special key from commandlist (a fresh
 Alist) or SYS::*KEY-BINDINGS* can be entered instead of a form.
 Values: NIL or (on special key or EOF) T */
LISPFUN(read_eval_print,seclass_default,1,1,norest,nokey,0,NIL)
/* (defun read-eval-print (prompt &optional (command-list nil))
   (multiple-value-bind (form flag)
       (read-form *standard-output* *standard-input* prompt command-list)
     (if flag
       form ; return T
       (progn
         (setq +++ ++ ++ + + - - form)
         (let ((vals (multiple-value-list (eval-env form [currentEnvironment]))))
           (setq /// // // / / vals)
           (setq *** ** ** * * (first vals))
           ; primitive:
         #|(do ((ostream *standard-output*)
                (L vals (cdr L)))
               ((atom L))
             (write (car L) ostream)
             (when (consp (cdr L))
               (write-string " ;" ostream)
               (terpri ostream)))
         |#; avoid unnecessary empty line between input and output:
           (let ((ostream *standard-output*))
             (fresh-line ostream)
             (when (consp vals)
               (write (car vals) ostream)
               (do ((L (cdr vals) (cdr L)))
                   ((atom L))
                 (write-string " ;" ostream)
                 (terpri ostream)
                 (write (car L) ostream)))
             (elastic-newline ostream)))
         nil)))) */
{
  read_form();                /* read form */
  /* stack layout: ostream, istream. */
  if (!nullp(value2)) {               /* flag ? */
    mv_count=1; skipSTACK(2); return; /* return T as value */
  }
  Symbol_value(S(plus3)) = Symbol_value(S(plus2)); /* (SETQ +++ ++) */
  Symbol_value(S(plus2)) = Symbol_value(S(plus));  /* (SETQ ++ +) */
  Symbol_value(S(plus)) = Symbol_value(S(minus));  /* (SETQ + -) */
  Symbol_value(S(minus)) = value1;                 /* (SETQ - form) */
  eval(value1);           /* evaluate form (in current environment) */
  pushSTACK(value1);          /* save a value */
  mv_to_list();               /* pack values into list */
  /* stack layout: ..., val1, vals. */
  Symbol_value(S(durch3)) = Symbol_value(S(durch2)); /* (SETQ /// //) */
  Symbol_value(S(durch2)) = Symbol_value(S(durch));  /* (SETQ // /) */
  Symbol_value(S(durch)) = STACK_0;                  /* (SETQ / vals) */
  Symbol_value(S(mal3)) = Symbol_value(S(mal2));     /* (SETQ *** **) */
  Symbol_value(S(mal2)) = Symbol_value(S(mal));      /* (SETQ ** *) */
  Symbol_value(S(mal)) = STACK_1;                    /* (SETQ * val1) */
  /* print values to ostream := value from *STANDARD-OUTPUT**/
  STACK_(1+2) = var_stream(S(standard_output),strmflags_wr_ch_B);
 #if 0
  if (mconsp(STACK_0)) {
    loop {
      var object valsr = STACK_0;
      STACK_0 = Cdr(valsr);
      terpri(&STACK_(1+2));
      prin1(&STACK_(1+2),Car(valsr)); /* print next value */
      /* ';' as separator before end of line: */
      if (matomp(STACK_0))
        break;
      write_ascii_char(&STACK_(1+2),' ');
      write_ascii_char(&STACK_(1+2),';');
    }
  }
 #else
  /* avoid unnecessary empty line between input and output:
   (There still appears an unnecessary empty line on the screen,
   if stdin is attached to the terminal and stdout is a pipe, that
   in the end goes to the terminal again - i.e. via '| tee logfile'.
   In this case we have to - because of 'logfile' - print an NL to
   stdout, and because stdin prints an NL at the end of line
   automatically, this new line really cannot be avoided.) */
  fresh_line(&STACK_(1+2));     /* (fresh-line ostream) */
  if (mconsp(STACK_0)) {
    loop {
      var object valsr = STACK_0;
      STACK_0 = Cdr(valsr);
      prin1(&STACK_(1+2),Car(valsr)); /* print next value */
      /* ';' as separator before end of line: */
      if (matomp(STACK_0))
        break;
      write_ascii_char(&STACK_(1+2),' ');
      write_ascii_char(&STACK_(1+2),';');
      terpri(&STACK_(1+2));
    }
  }
 #endif
  elastic_newline(&STACK_(1+2));
  skipSTACK(4);
  VALUES1(NIL);
}

/* Starts the default driver (Read-Eval-Print-Loop)
 driver(); */
global void driver (void)
{
  var p_backtrace_t bt_save = back_trace;
  var struct backtrace_t bt_here;
  bt_here.bt_next = back_trace;
  bt_here.bt_function = L(driver);
  bt_here.bt_stack = STACK STACKop -1;
  bt_here.bt_num_arg = -1;
  back_trace = &bt_here;
  loop {
    var object driverfun = Symbol_value(S(driverstern)); /* value of *DRIVER* */
    if (nullp(driverfun))
      break;
    funcall(driverfun,0);       /* call with 0 arguments */
  }
  /* Default-Driver: */
  Symbol_value(S(break_count)) = Fixnum_0; /* SYS::*BREAK-COUNT* := 0 */
  { /* then, build up the driver-frame: */
    var gcv_object_t* top_of_frame = STACK; /* pointer on top of frame */
    var sp_jmp_buf returner;                /* memorize return point */
    finish_entry_frame(DRIVER,returner,,;);
    /* this is the entry point. */
    loop {
      /* execute (SYS::READ-EVAL-PRINT "> "): */
      pushSTACK(O(prompt_string)); /* Prompt "> " */
      funcall(L(read_eval_print),1);
      if (eq(value1,T))        /* EOF has been read -> terminate loop */
        break;
    }
    skipSTACK(2);               /* skip driver-frame */
  }
  back_trace = bt_save;
}

/* Starts a secondary driver (Read-Eval-Print-Loop)
 break_driver(continuable_p);
 > continuable_p == can be continued after the driver finishes
 can trigger GC */
global maygc void break_driver (bool continuable_p) {
  var object driverfun = Symbol_value(S(break_driver)); /* *BREAK-DRIVER* */
  if (!nullp(driverfun)) {
    pushSTACK(continuable_p ? T : NIL);
    funcall(driverfun,1); /* call with CONTINUABLE argument */
    if (!continuable_p) /* not continuable? */
      reset(1); /* -> back to the previous REPLoop */
  } else {
    var p_backtrace_t bt_save = back_trace;
    var struct backtrace_t bt_here;
    bt_here.bt_next = back_trace;
    bt_here.bt_function = L(initial_break_driver);
    bt_here.bt_stack = STACK STACKop -1;
    bt_here.bt_num_arg = -1;
    back_trace = &bt_here;
    /* Default-Driver: (CLEAR-INPUT *DEBUG-IO*), since whatever has been
       typed so far, was not typed in anticipation of this error */
    Symbol_value(S(terminal_read_stream)) = unbound;
    Symbol_value(S(terminal_read_open_object)) = unbound;
    clear_input(var_stream(S(debug_io),strmflags_rd_ch_B|strmflags_wr_ch_B));
    /* SYS::*BREAK-COUNT* increase: */
    dynamic_bind(S(break_count),fixnum_inc(Symbol_value(S(break_count)),1));
    if (!posfixnump(Symbol_value(S(break_count)))) /* should be Fixnum >=0 */
      Symbol_value(S(break_count)) = Fixnum_0; /* oops - fix it! */
    { /* bind *STANDARD-INPUT* and *STANDARD-OUTPUT* to *DEBUG-IO* */
      var object stream =
        var_stream(S(debug_io),strmflags_rd_ch_B|strmflags_wr_ch_B);
      dynamic_bind(S(standard_input),stream);
      dynamic_bind(S(standard_output),stream);
    }
    dynamic_bind(S(print_escape),T);     /* bind *PRINT-ESCAPE* to T */
    dynamic_bind(S(print_readably),NIL); /* bind *PRINT-READABLY* to NIL */
    { /* make prompt:
         (format nil "~S. Break> " SYS::*BREAK-COUNT*)
         ==
         (with-output-to-string (s)
           (prin1 SYS::*BREAK-COUNT* s) (write-string ". Break> " s))
         ==
         (let ((s (make-string-output-stream)))
           (prin1 SYS::*BREAK-COUNT* s) (write-string ". Break> " s)
           (get-output-stream-string s)) */
      pushSTACK(make_string_output_stream());
      prin1(&STACK_0,Symbol_value(S(break_count)));
      write_sstring(&STACK_0,O(breakprompt_string));
      STACK_0 = get_output_stream_string(&STACK_0);
    }
    { /* make driver-frame: */
      var gcv_object_t* top_of_frame = STACK; /* pointer over frame */
      var sp_jmp_buf returner; /* return point */
      finish_entry_frame(DRIVER,returner,,;);
      /* re-entry point is here */
      loop {
        /* (SYS::READ-EVAL-PRINT Prompt) */
        pushSTACK(STACK_(0+2)); /* Prompt "nnn. Break> " */
        funcall(L(read_eval_print),1);
        if (eq(value1,T)) /* EOF -> finish loop */
          break;
      }
      if (!continuable_p) { /* not continuable? */
        back_trace = bt_save;
        unwind(); reset(1); /* -> back to the previous REPLoop */
      }
      skipSTACK(1+2); /* dissolve driver frame, forget prompt */
      dynamic_unbind(S(print_readably));
      dynamic_unbind(S(print_escape));
      dynamic_unbind(S(standard_output));
      dynamic_unbind(S(standard_input));
      dynamic_unbind(S(break_count));
    }
    back_trace = bt_save;
  }
}

LISPFUNN(initial_break_driver,1)
{
  break_driver(!nullp(popSTACK()));
  VALUES1(NIL);
}

LISPFUNN(load,1)
/* (LOAD filename), more primitive version than in CLTL p. 426
   method:
   (defun load (filename)
     (let ((stream (open filename))
           (end-of-file "EOF")) ; nonrecurring object
       (loop
         (let ((obj (read stream nil end-of-file)))
           (when (eql obj end-of-file) (return))
           (if (compiled-function-p obj) (funcall obj) (eval obj))))
       (close stream)
       t)) */
{
  funcall(L(open),1);           /* (OPEN filename) */
  pushSTACK(value1);            /* save stream */
  loop {
    var object obj = stream_read(&STACK_0,NIL,NIL); /* read object */
    if (eq(obj,eof_value))                          /* EOF -> done */
      break;
    if (closurep(obj)) {
      funcall(obj,0);     /* call closure (probably compiled closure) */
    } else {
      eval_noenv(obj);          /* evaluate other form */
    }
  }
  builtin_stream_close(&STACK_0,0); /* close stream */
  skipSTACK(1); VALUES1(T);
}

/* ----------------------------------------------------------------------- */
/* Auxiliary functions for debugger and stepper */

/* The following functions climb around in the stack, but will never
 trespass a driver-frame or the upper end of the stack.
 Valid "stackpointers" are in this context pointers to stack elements or
 frames, if there is neither the end of stack nor a driver-frame.
 Modus 1: all stack item
 Modus 2: frames
 Modus 3: lexical frames: frame-info has FRAME_BIT = 1 and
          (SKIP2_BIT = 1 or ENTRYPOINT_BIT = 0 or BLOCKGO_BIT = 1)
 Modus 4: EVAL- and APPLY-frames: frame-info = [TRAPPED_]EVAL/APPLY_FRAME_INFO
 Modus 5: APPLY-frames: frame-info = [TRAPPED_]APPLY_FRAME_INFO */

/* Macro: tests, if FRAME has reached stack end. */
#define stack_upend_p()  \
  (   eq(FRAME_(0),nullobj)           /* Nullword = upper stack end */ \
   || (framecode(FRAME_(0)) == DRIVER_frame_info) /* driver-frame = stack end */ \
   || ((framepointerp(Symbol_value(S(frame_limit2))))                        \
       && (uTheFramepointer(Symbol_value(S(frame_limit2))) cmpSTACKop FRAME) /* FRAME > *frame-limit2* ? */))
#define stack_downend_p()  \
  (   (framecode(FRAME_(0)) == DRIVER_frame_info) /* driver-frame = stack end */ \
   || ((framepointerp(Symbol_value(S(frame_limit1))))                        \
       && (FRAME cmpSTACKop uTheFramepointer(Symbol_value(S(frame_limit1)))) /* FRAME < *frame-limit1* ? */))

/* Macro: Tests, if FRAME points to a frame.
 first approximation:
 #define frame_p()  (!( (as_oint(FRAME_(0)) & wbit(frame_bit_o)) ==0))
 in second approximation, considering the frames with Skip2-bit: */
#define frame_p()  framep(FRAME)
local bool framep (gcv_object_t* FRAME)
{
  /* a normal lisp object is not a frame: */
  if ((as_oint(FRAME_(0)) & wbit(frame_bit_o)) ==0)
    return false;
  /* if a frame starts at FRAME_(-1) without Skip2-bit,
     then FRAME_(0) is part of this frame, which means,
     it is not itself the start of a frame: */
  if (   (!(FRAME==STACK))      /* do not trespass the STACK borders! */
      && ((as_oint(FRAME_(-1)) & wbit(skip2_bit_o)) == 0)
      && framep(FRAME STACKop -1))
    return false;
  return true;                  /* else, a frame starts here. */
}

/* Macro: decreases FRAME down to the next frame. */
#define next_frame_down()  do { FRAME skipSTACKop -1; } until (frame_p());

/* Macro: Tests, if the frame at FRAME is a lexical frame. */
#ifdef entrypoint_bit_t
#define lexical_frame_p()                                  \
  (   (!( (as_oint(FRAME_(0)) & wbit(skip2_bit_o)) ==0))   \
   || ( (as_oint(FRAME_(0)) & wbit(entrypoint_bit_o)) ==0) \
   || (!( (as_oint(FRAME_(0)) & wbit(blockgo_bit_o)) ==0)))
#else
#define lexical_frame_p()                                  \
  (/* (!( (as_oint(FRAME_(0)) & wbit(skip2_bit_o)) ==0))   \
   || */ (framecode(FRAME_(0)) >= entrypoint_limit_t)      \
   || (!( (as_oint(FRAME_(0)) & wbit(blockgo_bit_o)) ==0)) \
  )
#endif

/* Macro: Tests, if the frame at FRAME is an EVAL/APPLY frame. */
#define evalapply_frame_p()  \
  ((framecode(FRAME_(0)) & ~(bit(eval_bit_t)|bit(trapped_bit_t))) == \
   ((EVAL_frame_info|APPLY_frame_info) & ~(bit(eval_bit_t)|bit(trapped_bit_t))))

/* Macro: Tests, if the frame at FRAME is an APPLY frame. */
#define apply_frame_p()  \
  ((framecode(FRAME_(0)) & ~bit(trapped_bit_t)) == (APPLY_frame_info & ~bit(trapped_bit_t)))

/* UP: jumps up one stackitem */
local gcv_object_t* frame_up_1 (gcv_object_t* stackptr)
{
  var gcv_object_t* FRAME = stackptr;
  if (frame_p())
    FRAME = topofframe(FRAME_(0)); /* Pointer to top of frame */
  else
    FRAME skipSTACKop 1;        /* pointer to next object */
  return (stack_upend_p() ? stackptr : FRAME);
}

/* UP: jumpts down one stackitem */
local gcv_object_t* frame_down_1 (gcv_object_t* stackptr)
{
  var gcv_object_t* FRAME = stackptr;
  next_frame_down();          /* search next frame below */
  if (!(topofframe(FRAME_(0)) == stackptr)) /* not directly below stackptr? */
    FRAME = stackptr STACKop -1;
  return (stack_downend_p() ? stackptr : FRAME);
}

/* UP: jumps up to frame after next frame */
local gcv_object_t* frame_up_2 (gcv_object_t* stackptr)
{
  var gcv_object_t* FRAME = stackptr;
  if (frame_p())
    FRAME = topofframe(FRAME_(0)); /* pointer top of frame */
  else
    FRAME skipSTACKop 1;        /* pointer to next object */
  loop {
    if (stack_upend_p())
      return stackptr;
    if (as_oint(FRAME_(0)) & wbit(frame_bit_o))
      return FRAME;
    FRAME skipSTACKop 1;
  }
}

/* UP: jumps down to frame after next frame */
local gcv_object_t* frame_down_2 (gcv_object_t* stackptr)
{
  var gcv_object_t* FRAME = stackptr;
  next_frame_down();          /* search next frame below */
  return (stack_downend_p() ? stackptr : FRAME);
}

/* UP: jumps to next higher lexical frame */
local gcv_object_t* frame_up_3 (gcv_object_t* stackptr)
{
  var gcv_object_t* FRAME = stackptr;
  if (frame_p())
    FRAME = topofframe(FRAME_(0)); /* pointer top of frame */
  else
    FRAME skipSTACKop 1;      /* pointer to next object */
  loop {
    if (stack_upend_p())
      return stackptr;
    if (frame_p()) {
      if (lexical_frame_p())
        return FRAME;
      FRAME = topofframe(FRAME_(0)); /* pointer top of frame */
    } else {
      FRAME skipSTACKop 1;
    }
  }
}

/* UP: jumps to next lower lexical frame */
local gcv_object_t* frame_down_3 (gcv_object_t* stackptr)
{
  var gcv_object_t* FRAME = stackptr;
  loop {
    next_frame_down();        /* search next frame below */
    if (stack_downend_p())
      return stackptr;
    if (lexical_frame_p())
      break;
  }
  return FRAME;
}

/* UP: jumps to next higher EVAL/APPLY-frame */
local gcv_object_t* frame_up_4 (gcv_object_t* stackptr)
{
  var gcv_object_t* FRAME = stackptr;
  if (frame_p())
    FRAME = topofframe(FRAME_(0)); /* pointer top of frame */
  else
    FRAME skipSTACKop 1;      /* pointer to next object */
  loop {
    if (stack_upend_p())
      return stackptr;
    if (frame_p()) {
      if (evalapply_frame_p())
        return FRAME;
      FRAME = topofframe(FRAME_(0)); /* pointer top of frame */
    } else {
      FRAME skipSTACKop 1;
    }
  }
}

/* UP: jumpt to next lower EVAL/APPLY-frame */
local gcv_object_t* frame_down_4 (gcv_object_t* stackptr)
{
  var gcv_object_t* FRAME = stackptr;
  loop {
    next_frame_down();        /* search next frame below */
    if (stack_downend_p())
      return stackptr;
    if (evalapply_frame_p())
      break;
  }
  return FRAME;
}

/* UP: jumps to next higher APPLY-frame */
local gcv_object_t* frame_up_5 (gcv_object_t* stackptr)
{
  var gcv_object_t* FRAME = stackptr;
  if (frame_p())
    FRAME = topofframe(FRAME_(0)); /* pointer top of frame */
  else
    FRAME skipSTACKop 1;      /* pointer to next object */
  loop {
    if (stack_upend_p())
      return stackptr;
    if (frame_p()) {
      if (apply_frame_p())
        return FRAME;
      FRAME = topofframe(FRAME_(0)); /* pointer top of frame */
    } else {
      FRAME skipSTACKop 1;
    }
  }
}

/* UP: jumps to next lower APPLY-frame */
local gcv_object_t* frame_down_5 (gcv_object_t* stackptr)
{
  var gcv_object_t* FRAME = stackptr;
  loop {
    next_frame_down();        /* search next frame below */
    if (stack_downend_p())
      return stackptr;
    if (apply_frame_p())
      break;
  }
  return FRAME;
}

/* type of a pointer to a climb-up resp. climb-down routine: */
typedef gcv_object_t* (*climb_fun_t) (gcv_object_t* stackptr);

local const climb_fun_t frame_up_table[] =
  { &frame_up_1, &frame_up_2, &frame_up_3, &frame_up_4, &frame_up_5, };
local const climb_fun_t frame_down_table[] =
  { &frame_down_1, &frame_down_2, &frame_down_3, &frame_down_4, &frame_down_5, };

/* UP: checks and decodes the mode-argument.
 test_mode_arg(table)
 > STACK_0: mode
 > table: table of routines for climbing up resp. climbing down
 < result: routine for climbing up resp. climbing down
 increases STACK by 1 */
local climb_fun_t test_mode_arg (const climb_fun_t* table) {
  var object arg = popSTACK();
  var uintV mode;
  if (   !(posfixnump(arg)
      && ((mode = posfixnum_to_V(arg)) > 0)
      && (mode<=5))) {
    pushSTACK(arg);                /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_climb_mode)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(arg);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: bad frame climbing mode ~S"));
  }
  return table[mode-1];
}

/* UP: checks a frame-pointer-argument.
 test_framepointer_arg()
 > STACK_0: Lisp object, should be a frame-pointer
 < result: frame-pointer
 increases STACK by 1 */
local gcv_object_t* test_framepointer_arg (void)
{
  var object arg = popSTACK();
  if (!framepointerp(arg)) {
    pushSTACK(arg);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: ~S is not a stack pointer"));
  }
  return uTheFramepointer(arg);
}

LISPFUNN(frame_up_1,2)
{ /* (SYS::FRAME-UP-1 framepointer mode)
     returns the frame-pointer increased by 1. */
  var climb_fun_t frame_up_x = test_mode_arg(&frame_up_table[0]);
  var gcv_object_t* stackptr = test_framepointer_arg();
  stackptr = (*frame_up_x)(stackptr); /* climb up once */
  VALUES1(make_framepointer(stackptr));
}

LISPFUNN(frame_up,2)
{ /* (SYS::FRAME-UP framepointer mode) returns the frame-pointer at the top. */
  var climb_fun_t frame_up_x = test_mode_arg(&frame_up_table[0]);
  var gcv_object_t* stackptr = test_framepointer_arg();
  /* climb up as high as possible: */
  loop {
    var gcv_object_t* next_stackptr = (*frame_up_x)(stackptr);
    if (next_stackptr == stackptr)
      break;
    stackptr = next_stackptr;
  }
  VALUES1(make_framepointer(stackptr));
}

LISPFUNN(frame_down_1,2)
{ /* (SYS::FRAME-DOWN-1 framepointer mode)
     returns the frame-pointer 1 below. */
  var climb_fun_t frame_down_x = test_mode_arg(&frame_down_table[0]);
  var gcv_object_t* stackptr = test_framepointer_arg();
  stackptr = (*frame_down_x)(stackptr); /* climb down once */
  VALUES1(make_framepointer(stackptr));
}

LISPFUNN(frame_down,2)
{ /* (SYS::FRAME-DOWN framepointer mode)
     returns the frame-pointer at the bottom. */
  var climb_fun_t frame_down_x = test_mode_arg(&frame_down_table[0]);
  var gcv_object_t* stackptr = test_framepointer_arg();
  /* climb down as low as possible: */
  loop {
    var gcv_object_t* next_stackptr = (*frame_down_x)(stackptr);
    if (next_stackptr == stackptr)
      break;
    stackptr = next_stackptr;
  }
  VALUES1(make_framepointer(stackptr));
}

LISPFUNN(the_frame,0)
{ /* (SYS::THE-FRAME) returns the current stackpointer as frame-pointer. */
  var gcv_object_t* stackptr = STACK;
  stackptr = frame_up_2(stackptr); /* up to the next higher frame */
  VALUES1(make_framepointer(stackptr));
}

/* UP: activates the same lexical environment, that was active at
 framepointer STACK_0.
 same_env_as();
 increases STACK by 1, constructs an ENV5-Frame on top of the STACK */
local void same_env_as (void)
{
  var gcv_object_t* FRAME = test_framepointer_arg();
  /* 5 Environments still "empty": */
  var object found_var_env = nullobj;
  var object found_fun_env = nullobj;
  var object found_block_env = nullobj;
  var object found_go_env = nullobj;
  var object found_decl_env = nullobj;
  /* and fill: */
  loop {
    /* search at FRAME downwards for ENV-frames: */
    loop {
      FRAME skipSTACKop -1;
      if (FRAME==STACK)       /* done? */
        goto end;
      if (   frame_p()
          && (!( (as_oint(FRAME_(0)) & wbit(skip2_bit_o)) ==0))
          && (!( (as_oint(FRAME_(0)) & wbit(envbind_bit_o)) ==0)))
        break;
    }
    /* found next ENV-frame.
       its contents fills the empty components of env: */
    switch (framecode(FRAME_(0)) & envbind_case_mask_t) {
      case (ENV1V_frame_info & envbind_case_mask_t): /* 1 VAR_ENV */
        if (eq(found_var_env,nullobj)) { found_var_env = FRAME_(1); }
        break;
      case (ENV1F_frame_info & envbind_case_mask_t): /* 1 FUN_ENV */
        if (eq(found_fun_env,nullobj)) { found_fun_env = FRAME_(1); }
        break;
      case (ENV1B_frame_info & envbind_case_mask_t): /* 1 BLOCK_ENV */
        if (eq(found_block_env,nullobj)) { found_block_env = FRAME_(1); }
        break;
      case (ENV1G_frame_info & envbind_case_mask_t): /* 1 GO_ENV */
        if (eq(found_go_env,nullobj)) { found_go_env = FRAME_(1); }
        break;
      case (ENV1D_frame_info & envbind_case_mask_t): /* 1 DECL_ENV */
        if (eq(found_decl_env,nullobj)) { found_decl_env = FRAME_(1); }
        break;
      case (ENV2VD_frame_info & envbind_case_mask_t): /* 1 VAR_ENV and 1 DECL_ENV */
        if (eq(found_var_env,nullobj)) { found_var_env = FRAME_(1); }
        if (eq(found_decl_env,nullobj)) { found_decl_env = FRAME_(2); }
        break;
      case (ENV5_frame_info & envbind_case_mask_t): /* all 5 environments */
        if (eq(found_var_env,nullobj)) { found_var_env = FRAME_(1); }
        if (eq(found_fun_env,nullobj)) { found_fun_env = FRAME_(2); }
        if (eq(found_block_env,nullobj)) { found_block_env = FRAME_(3); }
        if (eq(found_go_env,nullobj)) { found_go_env = FRAME_(4); }
        if (eq(found_decl_env,nullobj)) { found_decl_env = FRAME_(5); }
        break;
      default: NOTREACHED;
    }
    /* if each single environment of env is filled (/=nullobj),
       the environment is done: */
    if (   (!eq(found_var_env,nullobj))
        && (!eq(found_fun_env,nullobj))
        && (!eq(found_block_env,nullobj))
        && (!eq(found_go_env,nullobj))
        && (!eq(found_decl_env,nullobj)))
      goto fertig;
  }
 end:                         /* end of stack is reached. */
  /* fetch the remaining environment-components from the current environment: */
  if (eq(found_var_env,nullobj)) { found_var_env = aktenv.var_env; }
  if (eq(found_fun_env,nullobj)) { found_fun_env = aktenv.fun_env; }
  if (eq(found_block_env,nullobj)) { found_block_env = aktenv.block_env; }
  if (eq(found_go_env,nullobj)) { found_go_env = aktenv.go_env; }
  if (eq(found_decl_env,nullobj)) { found_decl_env = aktenv.decl_env; }
 fertig:
  /* construct environment-frame: */
  make_ENV5_frame();
  /* set current environments: */
  aktenv.var_env   = found_var_env  ;
  aktenv.fun_env   = found_fun_env  ;
  aktenv.block_env = found_block_env;
  aktenv.go_env    = found_go_env   ;
  aktenv.decl_env  = found_decl_env ;
}

LISPFUNN(same_env_as,2)
{ /* (SYS::SAME-ENV-AS framepointer fun) activates the same lexical
     environment, that was active at framepointer, and then calls fun. */
  var object fun = popSTACK();
  same_env_as();              /* activate environment of framepointer */
  funcall(fun,0);               /* call fun */
  unwind();                     /* unwind environment-frame */
}

LISPFUNN(eval_at,2)
{ /* (SYS::EVAL-AT framepointer form) activates the same lexical
     environment, that was active at framepointer, and evaluates the form. */
  var object form = popSTACK();
  same_env_as();              /* activate environment of framepointer */
  eval(form);                   /* evaluate form */
  unwind();                     /* unwind environment-frame */
}

LISPFUNN(eval_frame_p,1)
{ /* (SYS::EVAL-FRAME-P framepointer)
     indicates, if framepointer points to an EVAL/APPLY-frame. */
  var gcv_object_t* FRAME = test_framepointer_arg();
  VALUES_IF(evalapply_frame_p());
}

LISPFUNN(driver_frame_p,1)
{ /* (SYS::DRIVER-FRAME-P framepointer)
     indicates, if framepointer points to a driver-frame. */
  var gcv_object_t* FRAME = test_framepointer_arg();
  VALUES_IF(framecode(FRAME_(0)) == DRIVER_frame_info);
}

/* error-message, if there is no EVAL/APPLY-frame-pointer.
 fehler_evalframe(obj);
 > obj: not an EVAL/APPLY-frame-pointer */
nonreturning_function(local, fehler_evalframe, (object obj)) {
  pushSTACK(obj);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(error,GETTEXT("~S: ~S is not a pointer to an EVAL/APPLY frame"));
}

LISPFUNN(trap_eval_frame,2)
{ /* (SYS::TRAP-EVAL-FRAME framepointer flag) switches the breakpoint at
     the specified EVAL/APPLY-frame on and off according to flag. */
  var object flag = popSTACK();
  var object frame = popSTACK();
  if (!framepointerp(frame))
    fehler_evalframe(frame);
  var gcv_object_t* FRAME = uTheFramepointer(frame);
  if (!evalapply_frame_p())
    fehler_evalframe(frame);
  /* FRAME points to the EVAL/APPLY-frame. */
  if (!nullp(flag)) {
    /* switch on breakpoint */
    *(oint*)(&FRAME_(0)) |= wbit(trapped_bit_o);
  } else {
    /* switch off breakpoint */
    *(oint*)(&FRAME_(0)) &= ~wbit(trapped_bit_o);
  }
  VALUES1(frame);
}

LISPFUNN(redo_eval_frame,1)
{ /* (SYS::REDO-EVAL-FRAME framepointer) unwinds up to the specified
     EVAL/APPLY-frame and restarts to execute it. */
  var object frame = popSTACK();
  if (!framepointerp(frame))
    fehler_evalframe(frame);
  var gcv_object_t* FRAME = uTheFramepointer(frame);
  if (!evalapply_frame_p())
    fehler_evalframe(frame);
  /* FRAME points to the EVAL/APPLY-frame. */
  VALUES0;
  /* unwind everything up to the EVAL/APPLY-frame, then jump there */
  unwind_upto(FRAME);
}

LISPFUNN(return_from_eval_frame,2)
{ /* (SYS::RETURN-FROM-EVAL-FRAME framepointer form)
     unwind up to the specified EVAL/APPLY-frame and return as its values
     all values of the evaluation of the specified form. */
  var object form = popSTACK();
  var object frame = popSTACK();
  if (!framepointerp(frame))
    fehler_evalframe(frame);
  var gcv_object_t* FRAME = uTheFramepointer(frame);
  if (!evalapply_frame_p())
    fehler_evalframe(frame);
  /* FRAME points to the EVAL/APPLY-frame. */
  VALUES1(form);
  /* unwind everything up to the EVAL/APPLY-frame, jump there */
  unwind_upto(FRAME);
}

/* ----------------------------------------------------------------------- */
/* Debug aux */

/* Returns the top-of-frame of a back_trace element. */
global gcv_object_t* top_of_back_trace_frame (const struct backtrace_t *bt) {
  var gcv_object_t* stack = bt->bt_stack;
  var object fun = bt->bt_function;
  if (fsubrp(fun)) {
    /* FSUBR */
    var uintW numreq;
    var uintW numopt;
    var uintW body_flag;
    switch ((uintW)posfixnum_to_V(TheFsubr(fun)->argtype)) {
      case fsubr_argtype_1_0_nobody: numreq = 1; numopt = 0; body_flag = 0; break;
      case fsubr_argtype_2_0_nobody: numreq = 2; numopt = 0; body_flag = 0; break;
      case fsubr_argtype_1_1_nobody: numreq = 1; numopt = 1; body_flag = 0; break;
      case fsubr_argtype_2_1_nobody: numreq = 2; numopt = 1; body_flag = 0; break;
      case fsubr_argtype_0_body: numreq = 0; numopt = 0; body_flag = 1; break;
      case fsubr_argtype_1_body: numreq = 1; numopt = 0; body_flag = 1; break;
      case fsubr_argtype_2_body: numreq = 2; numopt = 0; body_flag = 1; break;
      default: NOTREACHED;
    }
    return stack STACKop (numreq + numopt + body_flag);
  }
  if (subrp(fun))
    /* SUBR */
    return stack STACKop (TheSubr(fun)->req_anz + TheSubr(fun)->opt_anz
                          + TheSubr(fun)->key_anz);
  if (closurep(fun)) {
    if (simple_bit_vector_p(Atype_8Bit,TheClosure(fun)->clos_codevec)) {
      /* Compiled-Closure */
      var object codevec = TheClosure(fun)->clos_codevec;
      return stack STACKop (TheCodevec(codevec)->ccv_numreq
                            + TheCodevec(codevec)->ccv_numopt
                            + (TheCodevec(codevec)->ccv_flags & bit(0) ? 1 : 0)
                            + (TheCodevec(codevec)->ccv_flags & bit(7) ? TheCodevec(codevec)->ccv_numkey : 0));
    } else
      /* Interpreted-Closure */
      return stack;
  }
  /* Only SUBRs and functions occur as bt_function. */
  NOTREACHED;
}

local void print_back_trace (const gcv_object_t* stream_,
                             const struct backtrace_t *bt, int index) {
  write_ascii_char(stream_,'<');
  if (index >= 0)
    prin1(stream_,fixnum(index));
  else
    write_ascii_char(stream_,'#');
  write_ascii_char(stream_,'>');
  write_ascii_char(stream_,' ');
  prin1(stream_,bt->bt_function);
  if (bt->bt_num_arg >= 0) {
    write_ascii_char(stream_,' ');
    prin1(stream_,fixnum(bt->bt_num_arg));
  }
}

/* UP: prints the stackitem FRAME_(0) in detail to the stream
 and returns the next higher stackptr.
 print_stackitem(&stream,FRAME)
 can trigger GC */
local maygc gcv_object_t* print_stackitem (const gcv_object_t* stream_,
                                           gcv_object_t* FRAME)
{
  if (!frame_p()) {
    /* no frame, normal LISP-object */
    write_sstring(stream_,O(showstack_string_lisp_obj)); /* "- " */
    var object obj = FRAME_(0);
   #if !defined(NO_symbolflags)
    switch (typecode(obj)) {  /* poss. remove symbol-flags */
      case_symbolflagged: obj = symbol_without_flags(obj);
      default: break;
    }
   #endif
    prin1(stream_,obj);       /* print LISP-object */
    return FRAME STACKop 1;
  } else {
    /* met frame */
    var gcv_object_t* FRAME_top = topofframe(FRAME_(0)); /* top of frame */
    switch (framecode(FRAME_(0))) { /* according to frametype */
      case TRAPPED_APPLY_frame_info:
        write_sstring(stream_,CLSTEXT("APPLY frame with breakpoint for call "));
        goto APPLY_frame;
      case APPLY_frame_info:
        write_sstring(stream_,CLSTEXT("APPLY frame for call "));
       APPLY_frame:
        /* print function name and arguments: */
        write_ascii_char(stream_,'('); /* print '(' */
        prin1(stream_,TheIclosure(FRAME_(frame_closure))->clos_name); /* print name */
        {
          var gcv_object_t* argptr = FRAME_top;
          var uintL count = STACK_item_count(FRAME STACKop frame_args,FRAME_top);
          dotimesL(count,count, {
            write_ascii_char(stream_,' ');  /* print ' ' */
            write_ascii_char(stream_,'\''); /* print "'" */
            prin1(stream_,NEXT(argptr));    /* print next argument */
          });
        }
        write_ascii_char(stream_,')'); /* print ')' */
        break;
      case TRAPPED_EVAL_frame_info:
        write_sstring(stream_,CLSTEXT("EVAL frame with breakpoint for form "));
        goto EVAL_frame;
      case EVAL_frame_info:
        write_sstring(stream_,CLSTEXT("EVAL frame for form "));
      EVAL_frame:
        prin1(stream_,FRAME_(frame_form)); /* print form */
        break;
      case DYNBIND_frame_info: /* dynamic variable binding frames: */
        write_sstring(stream_,CLSTEXT("frame binding variables (~ = dynamically):"));
        /* print bindings: */
        FRAME skipSTACKop 1;
        while (FRAME != FRAME_top) {
          /* print binding of Symbol FRAME_(0) to value FRAME_(1): */
          write_sstring(stream_,O(showstack_string_bindung)); /* "␤  | " */
          write_ascii_char(stream_,'~'); /* print '~' */
          write_ascii_char(stream_,' '); /* print ' ' */
          prin1(stream_,FRAME_(0));      /* print symbol */
          write_sstring(stream_,O(showstack_string_zuord)); /* " <--> " */
          prin1(stream_,FRAME_(1)); /* print value */
          FRAME skipSTACKop 2;
        }
        break;
     #ifdef HAVE_SAVED_REGISTERS
      case CALLBACK_frame_info: /* callback-register-frames: */
        write_sstring(stream_,CLSTEXT("CALLBACK frame"));
        break;
     #endif
      /* variable- and function binding frames: */
      case VAR_frame_info:
        write_sstring(stream_,CLSTEXT("frame binding variables "));
       #ifdef NO_symbolflags
        prin1(stream_,make_framepointer(FRAME)); /* print frame-pointer */
        write_sstring(stream_,CLSTEXT(" binds (~ = dynamically):"));
        pushSTACK(FRAME_(frame_next_env)); /* save next environment */
        /* print bindings: */
        FRAME skipSTACKop frame_bindings;
        while (FRAME != FRAME_top) {
          if (as_oint(FRAME_(varframe_binding_mark)) & wbit(active_bit_o)) {
            /* print binding of symbol FRAME_(1) to value FRAME_(2): */
            write_sstring(stream_,O(showstack_string_bindung)); /* "␤  | " */
            if (as_oint(FRAME_(varframe_binding_mark)) & wbit(dynam_bit_o))
              /* dynamic binding? */
              write_ascii_char(stream_,'~'); /* yes -> print '~' */
            write_ascii_char(stream_,' ');   /* print ' ' */
            /* print symbol: */
            prin1(stream_,symbol_without_flags(FRAME_(varframe_binding_sym)));
            write_sstring(stream_,O(showstack_string_zuord)); /* " <--> " */
            prin1(stream_,FRAME_(varframe_binding_value)); /* print value */
          }
          FRAME skipSTACKop varframe_binding_size;
        }
        goto VARFUN_frame_next;
       #else
        goto VARFUN_frame;
       #endif
      case FUN_frame_info:
        write_sstring(stream_,CLSTEXT("frame binding functions "));
        goto VARFUN_frame;
      VARFUN_frame:
        prin1(stream_,make_framepointer(FRAME)); /* print frame-pointer */
        write_sstring(stream_,CLSTEXT(" binds (~ = dynamically):"));
        pushSTACK(FRAME_(frame_next_env)); /* save next environment */
        /* print bindings: */
        FRAME skipSTACKop frame_bindings;
        while (FRAME != FRAME_top) {
          if (as_oint(FRAME_(0)) & wbit(active_bit_o)) {
            /* print binding of symbol FRAME_(0) to value FRAME_(1): */
            write_sstring(stream_,O(showstack_string_bindung)); /* "␤  | " */
            if (as_oint(FRAME_(0)) & wbit(dynam_bit_o)) /* bindings dynamic? */
              write_ascii_char(stream_,'~'); /* yes -> print '~' */
            write_ascii_char(stream_,' ');   /* print ' ' */
            prin1(stream_,symbol_without_flags(FRAME_(0))); /* print symbol */
            write_sstring(stream_,O(showstack_string_zuord)); /* " <--> " */
            prin1(stream_,FRAME_(1)); /* print value */
          }
          FRAME skipSTACKop 2;
        }
      VARFUN_frame_next:
        /* print next environment: */
        terpri(stream_);
        write_sstring(stream_,CLSTEXT("  Next environment: "));
        {
          var object env = popSTACK(); /* next environment */
          if (!simple_vector_p(env)) {
            prin1(stream_,env);
          } else {
            /* next environment is a vector of length 2n+1 */
            do {
              pushSTACK(env);
              var uintL count = floor(Svector_length(env),2); /* = n = number of bindings */
              var uintL index = 0;
              dotimesL(count,count, {
                write_sstring(stream_,O(showstack_string_bindung)); /* "␤  | " */
                prin1(stream_,TheSvector(STACK_0)->data[index++]); /* print symbol */
                write_sstring(stream_,O(showstack_string_zuord)); /* " <--> " */
                prin1(stream_,TheSvector(STACK_0)->data[index++]); /* print symbol */
              });
              env = TheSvector(popSTACK())->data[index]; /* last vector-element */
            } while (simple_vector_p(env));
          }
        }
        break;
        /* compiled block/tagbody-frames: */
      case CBLOCK_CTAGBODY_frame_info:
        if (simple_vector_p(Car(FRAME_(frame_ctag)))) {
          /* compiled tagbody-frames: */
          write_sstring(stream_,CLSTEXT("compiled tagbody frame for "));
          prin1(stream_,Car(FRAME_(frame_ctag))); /* tag-vector */
        } else {
          /* compiled block-frames: */
          write_sstring(stream_,CLSTEXT("compiled block frame for "));
          prin1(stream_,Car(FRAME_(frame_ctag))); /* blockname */
        }
        break;
        /* interpreted block-frames: */
      case IBLOCK_frame_info:
        write_sstring(stream_,CLSTEXT("block frame "));
        goto IBLOCK_frame;
      case NESTED_IBLOCK_frame_info:
        write_sstring(stream_,CLSTEXT("nested block frame "));
        goto IBLOCK_frame;
      IBLOCK_frame:
        pushSTACK(FRAME_(frame_next_env));
        prin1(stream_,make_framepointer(FRAME)); /* print frame-pointer */
        write_sstring(stream_,CLSTEXT(" for "));
        prin1(stream_,FRAME_(frame_name)); /* blockname */
        goto NEXT_ENV;
        /* interpreted tagbody-frames: */
      case ITAGBODY_frame_info:
        write_sstring(stream_,CLSTEXT("tagbody frame "));
        goto ITAGBODY_frame;
      case NESTED_ITAGBODY_frame_info:
        write_sstring(stream_,CLSTEXT("nested tagbody frame "));
        goto ITAGBODY_frame;
      ITAGBODY_frame:
        pushSTACK(FRAME_(frame_next_env));
        prin1(stream_,make_framepointer(FRAME)); /* print frame-pointer */
        write_sstring(stream_,CLSTEXT(" for"));
        /* print tags/bodys: */
        FRAME skipSTACKop frame_bindings;
        while (FRAME != FRAME_top) {
          /* print binding of tag FRAME_(0) to body FRAME_(1): */
          write_sstring(stream_,O(showstack_string_bindung)); /* "␤  | " */
          prin1(stream_,FRAME_(0)); /* print tag */
          write_sstring(stream_,O(showstack_string_zuordtag)); /* " --> " */
          prin1(stream_,FRAME_(1)); /* print body */
          FRAME skipSTACKop 2;
        }
        goto NEXT_ENV;
      NEXT_ENV: /* printing of a block- or tagbody-environments STACK_0 */
        terpri(stream_);
        write_sstring(stream_,CLSTEXT("  Next environment: "));
        {
          var object env = popSTACK();
          if (!consp(env)) {
            prin1(stream_,env);
          } else {
            /* next environment is an Alist */
            do {
              pushSTACK(Cdr(env));
              env = Car(env);
              if (atomp(env)) {
                pushSTACK(S(show_stack));
                fehler(error,GETTEXT("~S: environment is not an alist"));
              }
              pushSTACK(Cdr(env));
              pushSTACK(Car(env));
              write_sstring(stream_,O(showstack_string_bindung)); /* "␤  | " */
              prin1(stream_,popSTACK());
              write_sstring(stream_,O(showstack_string_zuordtag)); /* " --> " */
              prin1(stream_,popSTACK());
              env = popSTACK();
            } while (consp(env));
          }
        }
        break;
      case CATCH_frame_info:
        /* catch-frames: */
        write_sstring(stream_,CLSTEXT("catch frame for tag "));
        prin1(stream_,FRAME_(frame_tag)); /* tag */
        break;
      case HANDLER_frame_info:
        /* handler-frames: */
        write_sstring(stream_,CLSTEXT("handler frame for conditions"));
        {
          var uintL m2 = Svector_length(Car(FRAME_(frame_handlers))); /* 2*m */
          var uintL i = 0;
          do {
            write_ascii_char(stream_,' '); /* print ' ' */
            prin1(stream_,TheSvector(Car(FRAME_(frame_handlers)))->data[i]); /* print type i */
            i += 2;
          } while (i < m2);
        }
        break;
      case UNWIND_PROTECT_frame_info:
        /* unwind-protect-frames: */
        write_sstring(stream_,CLSTEXT("unwind-protect frame"));
        break;
      case DRIVER_frame_info:
        /* driver-frames: */
        terpri(stream_); /* blank line */
        write_sstring(stream_,CLSTEXT("driver frame"));
        break;
        /* environment-frames: */
      case ENV1V_frame_info:
        write_sstring(stream_,CLSTEXT("frame binding environments"));
        write_sstring(stream_,O(showstack_string_VENV_frame)); /* "␤  VAR_ENV <--> " */
        prin1(stream_,FRAME_(1));
        break;
      case ENV1F_frame_info:
        write_sstring(stream_,CLSTEXT("frame binding environments"));
        write_sstring(stream_,O(showstack_string_FENV_frame)); /* "␤  FUN_ENV <--> " */
        prin1(stream_,FRAME_(1));
        break;
      case ENV1B_frame_info:
        write_sstring(stream_,CLSTEXT("frame binding environments"));
        write_sstring(stream_,O(showstack_string_BENV_frame)); /* "␤  BLOCK_ENV <--> " */
        prin1(stream_,FRAME_(1));
        break;
      case ENV1G_frame_info:
        write_sstring(stream_,CLSTEXT("frame binding environments"));
        write_sstring(stream_,O(showstack_string_GENV_frame)); /* "␤  GO_ENV <--> " */
        prin1(stream_,FRAME_(1));
        break;
      case ENV1D_frame_info:
        write_sstring(stream_,CLSTEXT("frame binding environments"));
        write_sstring(stream_,O(showstack_string_DENV_frame)); /* "␤  DECL_ENV <--> " */
        prin1(stream_,FRAME_(1));
        break;
      case ENV2VD_frame_info:
        write_sstring(stream_,CLSTEXT("frame binding environments"));
        write_sstring(stream_,O(showstack_string_VENV_frame)); /* "␤  VAR_ENV <--> " */
        prin1(stream_,FRAME_(1));
        write_sstring(stream_,O(showstack_string_DENV_frame)); /* "␤  DECL_ENV <--> " */
        prin1(stream_,FRAME_(2));
        break;
      case ENV5_frame_info:
        write_sstring(stream_,CLSTEXT("frame binding environments"));
        write_sstring(stream_,O(showstack_string_VENV_frame)); /* "␤  VAR_ENV <--> " */
        prin1(stream_,FRAME_(1));
        write_sstring(stream_,O(showstack_string_FENV_frame)); /* "␤  FUN_ENV <--> " */
        prin1(stream_,FRAME_(2));
        write_sstring(stream_,O(showstack_string_BENV_frame)); /* "␤  BLOCK_ENV <--> " */
        prin1(stream_,FRAME_(3));
        write_sstring(stream_,O(showstack_string_GENV_frame)); /* "␤  GO_ENV <--> " */
        prin1(stream_,FRAME_(4));
        write_sstring(stream_,O(showstack_string_DENV_frame)); /* "␤  DECL_ENV <--> " */
        prin1(stream_,FRAME_(5));
        break;
      default:
        pushSTACK(S(show_stack));
        fehler(serious_condition,GETTEXT("~S: unknown frame type"));
    }
    return FRAME_top;         /* pointer top of frame */
  }
}

LISPFUNN(describe_frame,2)
{ /* (SYS::DESCRIBE-FRAME stream framepointer) prints in detail the
     stackitem, that the pointer points to. */
  var gcv_object_t* FRAME = test_framepointer_arg(); /* pointer in the stack */
  STACK_0 = check_stream(STACK_0);
  fresh_line(&STACK_0);
  {
    var uintL count = 0;
    var p_backtrace_t bt = back_trace;
    unwind_back_trace(bt,FRAME STACKop -1);
    while (bt_beyond_stack_p(bt,FRAME)) {
      print_back_trace(&STACK_0,bt,++count);
      terpri(&STACK_0);
      bt = bt->bt_next;
    }
  }
  print_stackitem(&STACK_0,FRAME); /* print stack-item */
  elastic_newline(&STACK_0);
  skipSTACK(1); VALUES0; /* no values */
}

/* UP: print the stack (up to frame_limit frames, if that is non-0)
 frame by frame (moving using frame_up_x) or all stack items if that is NULL.
 starting with start_frame or STACK if that is NULL
 In debugger, use 'print show_stack(0,0,0)'.
 can trigger GC */
local inline maygc uintL show_stack (climb_fun_t frame_up_x, uintL frame_limit,
                                     gcv_object_t* start_frame)
{ /* run along the stack upwards */
  var gcv_object_t* FRAME = (start_frame == NULL ? STACK : start_frame);
  pushSTACK(var_stream(S(standard_output),strmflags_wr_ch_B));
  var gcv_object_t* stream_ = &STACK_0;
  var uintL count = 0;
  var p_backtrace_t bt = back_trace;
  while (!eq(FRAME_(0),nullobj) /* nullobj = stack end */
         && (frame_limit==0 || count<frame_limit)) {
    fresh_line(stream_);
    if (frame_up_x != NULL) {
      var gcv_object_t* next_frame = (*frame_up_x)(FRAME);
      if (next_frame == FRAME) break;
      FRAME = next_frame;
      while (bt_beyond_stack_p(bt,FRAME)) {
        print_back_trace(stream_,bt,++count);
        terpri(stream_);
        bt = bt->bt_next;
      }
      print_stackitem(stream_,FRAME);
    } else {
      while (bt_beyond_stack_p(bt,FRAME)) {
        print_back_trace(stream_,bt,++count);
        terpri(stream_);
        bt = bt->bt_next;
      }
      FRAME = print_stackitem(stream_,FRAME);
    }
    elastic_newline(stream_);
  }
  skipSTACK(1); /* drop *STANDARD-OUTPUT* */
  return count;
}

LISPFUN(show_stack,seclass_default,0,3,norest,nokey,0,NIL)
{ /* (SHOW-STACK mode limit start-frame) print the stack contents. */
  var gcv_object_t* start_frame = (missingp(STACK_0) ? (skipSTACK(1), &STACK_1)
                                   : test_framepointer_arg());
  var uintL frame_limit = (missingp(STACK_0) ? (skipSTACK(1), 0) :
                           uint32_p(STACK_0) ? I_to_uint32(popSTACK())
                           : (fehler_uint32(popSTACK()), 0));
  var climb_fun_t frame_up_x = (missingp(STACK_0)
                                ? (skipSTACK(1), (climb_fun_t) NULL)
                                : test_mode_arg(&frame_up_table[0]));
  VALUES1(UL_to_I(show_stack(frame_up_x,frame_limit,start_frame)));
}

/* For debugging: From within gdb, type: call ext_show_stack().
   Equivalent to (ext:show-stack) from the Lisp prompt. */
global void ext_show_stack (void) {
  pushSTACK(unbound); pushSTACK(unbound); pushSTACK(unbound); C_show_stack();
}

LISPFUNN(crash,0)
{ /* (SYSTEM::CRASH) jumps to the debugger sitting in the background. */
  abort();
  VALUES0;                      /* no values */
}

LISPFUNN(proom,0)
{ /* (SYSTEM::%ROOM), returns 3 values:
     - room occupied by LISP-objects
     - room free for LISP-objects
     - room statically occupied by LISP-objects
     do it in more detail at SPVW_PAGES?? */
  var uintM n1 = used_space();
  var uintM n2 = free_space();
  var uintM n3 = static_space();
  pushSTACK(uintM_to_I(n1));
  pushSTACK(uintM_to_I(n2));
  pushSTACK(uintM_to_I(n3));
  STACK_to_mv(3);
}

LISPFUNN(gc,0)
{ /* execute a GC and return the free space for LISP-objects (in bytes) */
  gar_col();                  /* execute GC */
  VALUES1(UL_to_I(free_space()));
}

/* rewrite read-form, in collaboration with the terminal-stream?? */

