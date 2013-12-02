# Handling of signal SIGSEGV (or SIGBUS on some platforms).

# ------------------------------ Specification --------------------------------

#if defined(GENERATIONAL_GC)

# Install the signal handler for catching page faults.
  local void install_segv_handler (void);

#endif # GENERATIONAL_GC

# Install the stack overflow handler if possible.
# install_stackoverflow_handler(size);
# > size: size of substitute stack.
# This function must be called from main(); it allocates the substitute stack
# using alloca().
  local void install_stackoverflow_handler (uintL size);

# ------------------------------ Implementation -------------------------------

#if defined(GENERATIONAL_GC) || defined(NOCOST_SP_CHECK)
local void print_mem_stats (void) {
  fprintf(stderr,GETTEXTL("Permanently allocated: %lu bytes."),
          (unsigned long) static_space());
  fputs("\n",stderr);
  fprintf(stderr,GETTEXTL("Currently in use: %lu bytes."),
          (unsigned long) used_space());
  fputs("\n",stderr);
  fprintf(stderr,GETTEXTL("Free space: %lu bytes."),
          (unsigned long) free_space());
  fputs("\n",stderr);
}
#endif

#if defined(GENERATIONAL_GC)

# Put a breakpoint here if you want to catch CLISP just before it dies.
global void sigsegv_handler_failed (void* address) {
  fputs("\n",stderr);
  fprintf(stderr,GETTEXTL("SIGSEGV cannot be cured. Fault address = 0x%lx."),
          address);
  fputs("\n",stderr);
  print_mem_stats();
}

# Signal-Handler for signal SIGSEGV or similar:
local int sigsegv_handler (void* fault_address, int serious) {
  set_break_sem_0();
  switch (handle_fault((aint)fault_address,serious)) {
    case handler_done:
      # successful
      clr_break_sem_0();
      return 1;
    case handler_failed:
      # unsuccessful
      if (serious)
        sigsegv_handler_failed(fault_address);
      # the default-handler will lead us into the debugger.
    default:
      clr_break_sem_0();
      return 0;
  }
}

# install all signal-handlers:
local void install_segv_handler (void) {
  sigsegv_install_handler(&sigsegv_handler);
}

#endif # GENERATIONAL_GC

#ifdef NOCOST_SP_CHECK

local void stackoverflow_handler (int emergency, stackoverflow_context_t scp) {
  if (emergency) {
    fprintf(stderr,GETTEXTL("Apollo 13 scenario: Stack overflow handling failed. On the next stack overflow we will crash!!!"));
    fputs("\n",stderr);
    print_mem_stats();
  }
  sigsegv_leave_handler();
 #ifdef HAVE_SAVED_STACK
  # Assign a reasonable value to STACK:
  if (saved_STACK != NULL) {
    setSTACK(STACK = saved_STACK);
  } else { # This depends on STACK_register.
  #ifdef UNIX_LINUX
    # stackoverflow_context_t is actually `struct sigcontext *'.
    # What about MC680X0 and SPARC ??
   #ifdef I80386
    if (scp) { setSTACK(STACK = (gcv_object_t*)(scp->ebx)); }
   #endif
   #ifdef ARM
    if (scp) { setSTACK(STACK = (gcv_object_t*)(scp->uc_mcontext.gregs[R8])); }
   #endif
   #ifdef DECALPHA
    if (scp) { setSTACK(STACK = (gcv_object_t*)(scp->sc_regs[9])); }
   #endif
  #endif
  #ifdef UNIX_SUNOS5
    # stackoverflow_context_t is actually `ucontext_t *'.
   #ifdef SPARC
    if (scp) { setSTACK(STACK = (gcv_object_t*)(scp->uc_mcontext.gregs[REG_G5])); }
   #endif
   #ifdef I80386
    if (scp) { setSTACK(STACK = (gcv_object_t*)(scp->uc_mcontext.gregs[EBX])); }
   #endif
  #endif
  #ifdef UNIX_IRIX
    # stackoverflow_context_t is actually `struct sigcontext *'.
   #ifdef MIPS
    # no STACK_reg yet
   #endif
  #endif
  #ifdef UNIX_OSF
    # stackoverflow_context_t is actually `struct sigcontext *'.
   #ifdef DECALPHA
    if (scp) { setSTACK(STACK = (gcv_object_t*)(scp->sc_regs[9])); }
   #endif
  #endif
  #ifdef UNIX_HPUX
   #ifdef HPPA
    # stackoverflow_context_t is actually `struct sigcontext *'.
    #define USE_64BIT_REGS(mc) \
      (((mc).ss_flags & SS_WIDEREGS) && ((mc).ss_flags & SS_NARROWISINVALID))
    #define GET_R10(mc) \
      (USE_64BIT_REGS(mc) ? (mc).ss_wide.ss_64.ss_gr10 : (mc).ss_narrow.ss_gr10)
    if (scp) { setSTACK(STACK = (gcv_object_t*)GET_R10(scp->sc_ctxt.sl.sl_ss)); }
   #endif
  #endif
  #ifdef UNIX_FREEBSD
    # stackoverflow_context_t is actually `struct sigcontext *'.
   #ifdef I80386
    if (scp) { setSTACK(STACK = (gcv_object_t*)(scp->sc_ebx)); }
   #endif
   #ifdef DECALPHA
    if (scp) { setSTACK(STACK = (gcv_object_t*)(scp->sc_regs[9])); }
   #endif
  #endif
  #ifdef UNIX_OPENBSD
    # stackoverflow_context_t is actually `struct sigcontext *'.
   #ifdef I80386
    if (scp) { setSTACK(STACK = (gcv_object_t*)(scp->sc_ebx)); }
   #endif
  #endif
  #ifdef UNIX_NETBSD
    # stackoverflow_context_t is actually `struct sigcontext *'.
   #ifdef DECALPHA
    if (scp) { setSTACK(STACK = (gcv_object_t*)(scp->sc_regs[9])); }
   #endif
  #endif
  #if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
   #ifdef I80386
    if (scp) { setSTACK(STACK = (gcv_object_t*)(scp->Ebx)); }
   #endif
  #endif
  }
 #endif
  SP_ueber();
}

# Must allocate room for a substitute stack for the stack overflow
# handler itself. This cannot be somewhere in the regular stack,
# because we want to unwind the stack in case of stack overflow.
#define install_stackoverflow_handler(size)                                   \
  do { var void* room = alloca(size);                                         \
       stackoverflow_install_handler(&stackoverflow_handler,(void*)room,size);\
  } while(0)

#else

# A dummy that does nothing.
#define install_stackoverflow_handler(size)  (void)(size)

#endif
