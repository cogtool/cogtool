/* Handling of terminating signals. */

/* --------------------------- Specification ----------------------------- */

#if defined(HAVE_SIGNALS)
/* Installs the SIGTERM handler. */
local void install_sigterm_handler (void);
#endif

/* --------------------------- Implementation ---------------------------- */

#if defined(HAVE_SIGNALS)
/* print the "exiting" message and quit */
local void quit_on_signal (int sig) {
  pushSTACK(Symbol_value(S(error_output))); fresh_line(&STACK_0);
  pushSTACK(CLSTEXT("Exiting on signal ")); pushSTACK(STACK_1);
  funcall(L(write_string),2);   /* (write-line "exiting" stderr) */
  pushSTACK(sint_to_I(sig)); pushSTACK(STACK_1);
  funcall(L(prin1),2);            /* (prin1 signal stderr) */
  terpri(&STACK_0); skipSTACK(1); /* drop *error-output* */
  quit();
}

/* install error handlers for as many signals as possible */
local void install_sigterm_handler (void) {
#ifdef SIGHUP
  /* maybe ignore? - use nohup instead */
  SIGNAL(SIGHUP,&quit_on_signal);
#endif
#ifdef SIGQUIT
  SIGNAL(SIGQUIT,&quit_on_signal);
#endif
#ifdef SIGILL
  SIGNAL(SIGILL,&quit_on_signal);
#endif
#ifdef SIGABRT
  SIGNAL(SIGABRT,&quit_on_signal);
#endif
#ifdef SIGKILL
  SIGNAL(SIGKILL,&quit_on_signal);
#endif
#ifdef SIGTERM
  SIGNAL(SIGTERM,&quit_on_signal);
#endif
#ifdef SIGTTOU
  /* we must ignore SIGTTOU to avoid the following issue:
      - when CLISP is running in the background,
      - and its i/o is not redirected,
      - and CLISP receives a terminating signal,
     then CLISP will be stopped instead of being terminated
     when it will try to write the "exiting..." message:
  <http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap11.html>
         Attempts by a process in a background process group to write to
         its controlling terminal shall cause the process group to be
         sent a SIGTTOU signal */
  SIGNAL(SIGTTOU,SIG_IGN);
#endif
}
#endif
