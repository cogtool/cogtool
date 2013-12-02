# Handling of signal SIGCLD.

# ------------------------------ Specification ---------------------------------

#ifdef HAVE_SIGNALS

# Install the signal handler for SIGCLD.
  local void install_sigcld_handler (void);

#endif

# ------------------------------ Implementation --------------------------------

#ifdef HAVE_SIGNALS

# Our general policy with child processes - in particular child processes
# to which we are connected through pipes - is not to wait for them, but
# instead do what init(1) would do in case our process terminates before
# the child: perform a non-blocking waitpid() and ignore the child's
# termination status.
#   void handle_child () { while (waitpid(-1,NULL,WNOHANG) > 0); }
#   SIGNAL(SIGCLD,handle_child);
# The following is equivalent (but better, since it doesn't interrupt system
# calls):
#   SIGNAL(SIGCLD,SIG_IGN);

  local void install_sigcld_handler (void)
  {
    #if defined(SIGCLD)
      SIGNAL(SIGCLD,SIG_IGN);
    #endif
  }

  global void begin_want_sigcld ()
  {
    #if defined(SIGCLD)
      SIGNAL(SIGCLD,SIG_DFL);
    #endif
  }
  global void end_want_sigcld ()
  {
    #if defined(SIGCLD)
      SIGNAL(SIGCLD,SIG_IGN);
      # Try to remove zombies which may have been created since the last
      # begin_want_sigcld() call.
      while (waitpid(-1,NULL,WNOHANG) > 0);
    #endif
  }

#endif
