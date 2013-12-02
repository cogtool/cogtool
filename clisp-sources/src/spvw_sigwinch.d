# Handling of signal SIGWINCH.

# ------------------------------ Specification --------------------------------

#if defined(HAVE_SIGNALS) && defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)
# Install a handler for SIGWINCH.
  local void install_sigwinch_handler (void);
#endif

#ifdef HAVE_SIGNALS
# Get the window width and adapt SYS::*PRIN-LINELENGTH* to it.
  local void update_linelength (void);
#endif

# Block signal SIGWINCH during GC.
# gc_signalblock_on(); ... gc_signalblock_off();

# ------------------------------ Implementation -------------------------------

#ifdef HAVE_SIGNALS

 #if defined(HAVE_READLINE)
  /* for rl_resize_terminal() */
  #include <readline/readline.h>
 #endif

# Adapts the value of SYS::*PRIN-LINELENGTH* to the current width of
# the terminal-window.
# update_linelength();
local void update_linelength (void) {
  # SYS::*PRIN-LINELENGTH* := width of the terminal-window - 1
 #if !defined(NEXTAPP)
  # [cf. 'term.c' in 'calc' by Hans-J. Boeh, Vernon Lee, Alan J. Demers]
  if (isatty(stdout_handle)) { # is standard-output a terminal?
    /* var int lines = 0; */
    var int columns = 0;
   #ifdef TIOCGWINSZ
    { # try ioctl first:
      var struct winsize stdout_window_size;
      if (!( ioctl(stdout_handle,TIOCGWINSZ,&stdout_window_size) <0)) {
        /* lines = stdout_window_size.ws_row; */
        columns = stdout_window_size.ws_col;
      }
    }
    # this can - contrary to the documentation - fail!
    if (/* (lines > 0) && */ (columns > 0))
      goto OK;
   #endif
   #if !defined(NO_TERMCAP_NCURSES)
    { # No let's try via termcap:
      var const char* term_name = getenv("TERM");
      if (term_name==NULL)
        term_name = "unknown";
      var char termcap_entry_buf[10000];
      if ( tgetent(termcap_entry_buf,term_name) ==1) {
        /* lines = tgetnum("li"); if (lines<0) { lines = 0; } */
        columns = tgetnum("co"); if (columns<0) { columns = 0; }
      }
    }
   #endif
    # Hopefully, columns contains now a sensible value.
    if (/* (lines > 0) && */ (columns > 0))
      goto OK;
    if (false) {
     OK:
      # change value of SYS::*PRIN-LINELENGTH* :
      Symbol_value(S(prin_linelength)) = fixnum(columns-1);
    }
  }
 #else # defined(NEXTAPP)
  if (nxterminal_line_length > 0) {
    # change value of SYS::*PRIN-LINELENGTH* :
    Symbol_value(S(prin_linelength)) = fixnum(nxterminal_line_length-1);
  }
 #endif
}

#endif

#if defined(HAVE_SIGNALS) && defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)

# signal-handler for signal SIGWINCH:
local void sigwinch_handler (int sig) { # sig = SIGWINCH
  inc_break_sem_5();
  signal_acknowledge(SIGWINCH,&sigwinch_handler);
  update_linelength();
 #if defined(HAVE_READLINE) && defined(RL_ISSTATE) && defined(RL_INITIALIZED)
  if (RL_ISSTATE(RL_INITIALIZED))
    rl_resize_terminal();
 #endif
  dec_break_sem_5();
}

#define install_sigwinch_handler()  \
  SIGNAL(SIGWINCH,&sigwinch_handler);

#endif

#if defined(HAVE_SIGNALS) && defined(SIGWINCH) && !defined(NO_ASYNC_INTERRUPTS)
  # block signal SIGWINCH, because we do not want the value
  # of SYS::*PRIN-LINELENGTH* to be changed during the GC.
  # Then allow signal SIGWINCH again.
  #define gc_signalblock_on()  signalblock_on(SIGWINCH)
  #define gc_signalblock_off()  signalblock_off(SIGWINCH)
#else
  #define gc_signalblock_on()
  #define gc_signalblock_off()
#endif
