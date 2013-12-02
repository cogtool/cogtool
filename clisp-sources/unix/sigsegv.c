/*
 * Catching SIGSEGV and similar signals.
 *
 * Since this is highly non-portable, we provide a sample test program.
 *
 * Note: CLISP needs this facility only if the system also provides
 *       getpagesize() and a working mprotect().
 * Once you have extended this file, build the knowledge into src/unix.d.
 */

/* How it works: We make a certain area read-only and write to it.
 * We expect a SIGSEGV or similar. Then look at the signal handler's
 * arguments, try to find the fault address.
 *
 * Sample session:
 *   gcc -O -g sigsegv.c
 *   gdb a.out
 *   (gdb) handle SIGSEGV nostop
 *   (gdb) break fault_handler
 *   (gdb) run
 *   (gdb) print fault_address
 *   (gdb) print sig
 *   (gdb) print arg1
 *   (gdb) print *arg1
 *   (gdb) print arg2
 *   (gdb) print *arg2
 *   (gdb) print arg3
 *   (gdb) print *arg3
 *
 */


#if 1
/* Put your pathname to unixconf.h here if you have already run configure. */
#include "unixconf.h"
#endif

#ifndef UNIXCONF

/* The following bits of information can be copied from unixconf.h. */

/* CL_STDC_HEADERS */
/* Define if you have the ANSI C header files
   <stdlib.h>, <stdarg.h>, <string.h>, <float.h>, <limits.h>. */
#undef STDC_HEADERS

/* CL_UNISTD_H */
/* Define if you have <unistd.h>. */
#undef HAVE_UNISTD_H

/* AC_SIZE_T */
#ifndef size_t
#undef size_t
#endif

/* AC_RETSIGTYPE */
/* Define as the return type of signal handlers (int or void). */
#define RETSIGTYPE void

#endif


/* Declarations. */

#ifdef STDC_HEADERS
#include <stdlib.h>
#endif

#ifdef HAVE_UNISTD_H
#include <sys/types.h>
#include <unistd.h>
#endif

#include <signal.h>
typedef RETSIGTYPE (*signal_handler_t) ();
extern signal_handler_t signal (int sig, signal_handler_t handler);
/* forward */ void install_signal (int sig, signal_handler_t handler);

#include <stdio.h>


/* This is the database how to get the fault address. */


#if (defined(i386) || defined(__i386) || defined(__i386__) || defined(_I386)) && defined(linux)
  /* This has been tested on Linux 1.1.38, but is likely to work for
   * most i386 Unices because of iBCS compatibility.
   *
   * On Linux, the correct way to declare this would be
   *   #include <asm/signal.h>
   *   #define FAULT_ADDRESS  ((struct sigcontext_struct *)(&more))->cr2
   */
#define FAULT_HANDLER_ARGLIST  sig, more
#define FAULT_HANDLER_ARGDECL  int sig; unsigned long more;
#define FAULT_ADDRESS  ((unsigned long *) &more) [21]
#define WP_SIGNAL  FAULT_HANDLER(SIGSEGV)
#define CAN_HANDLE_WP_FAULT
#endif

#if defined(linux) && defined(sparc) && 0 /* Sun4, Linux 2.0 */
#define FAULT_HANDLER_ARGLIST  sig, code, scp, addr
#define FAULT_HANDLER_ARGDECL  int sig; int code; void* scp; char* addr;
#define FAULT_ADDRESS  addr
#define WP_SIGNAL  FAULT_HANDLER(SIGSEGV)
#define CAN_HANDLE_WP_FAULT
#endif

#if defined(sun) && defined(sparc) && 0 /* Sun4, SunOS 4.1 */
#define FAULT_HANDLER_ARGLIST  sig, code, scp, addr
#define FAULT_HANDLER_ARGDECL  int sig; int code; void* scp; char* addr;
#define FAULT_ADDRESS  addr
#define WP_SIGNAL  FAULT_HANDLER(SIGBUS)
#define CAN_HANDLE_WP_FAULT
#endif

#if defined(sun) && defined(sparc) && 0 /* Sun4, SunOS 4.1.3 */
#define FAULT_HANDLER_ARGLIST  sig, code, scp, addr
#define FAULT_HANDLER_ARGDECL  int sig; int code; void* scp; char* addr;
#define FAULT_ADDRESS  addr
#define WP_SIGNAL  FAULT_HANDLER(SIGSEGV)
#define CAN_HANDLE_WP_FAULT
#endif

#if defined(__sgi) && defined(__mips) && 0 /* SGI Mips, Irix 5.2 */
#define FAULT_HANDLER_ARGLIST  sig, code, scp
#define FAULT_HANDLER_ARGDECL  int sig; int code; struct sigcontext *scp;
#define FAULT_ADDRESS  scp->sc_badvaddr
#define WP_SIGNAL  FAULT_HANDLER(SIGSEGV)
#define CAN_HANDLE_WP_FAULT
#endif

#if defined(__osf__) && defined(__alpha__) /* DEC Alpha, OSF/1 2.0 */
/* Note that this is not explicitly documented as being where the
   fault address lives, so watch out for new releases. */
#define FAULT_HANDLER_ARGLIST  sig, code, scp
#define FAULT_HANDLER_ARGDECL  int sig; int code; struct sigcontext *scp;
#define FAULT_ADDRESS  scp->sc_traparg_a0
#define WP_SIGNAL  FAULT_HANDLER(SIGSEGV)
#define CAN_HANDLE_WP_FAULT
#endif

#if defined(_AIX) && defined(_AIX32) /* AIX 3.2, RS/6000, gcc */
/* cf. AIX 3.2.5 Info Explorer,
 * section "sigaction, sigvec, or signal Subroutine"
 */
#define FAULT_HANDLER_ARGLIST  sig, code, scp
#define FAULT_HANDLER_ARGDECL  int sig; int code; struct sigcontext *scp;
/* "Saved vaddr for vmexception", cf. <sys/mstsave.h> */
#define FAULT_ADDRESS  scp->sc_jmpbuf.jmp_context.o_vaddr
#define WP_SIGNAL  FAULT_HANDLER(SIGSEGV)
#define CAN_HANDLE_WP_FAULT
#endif

/* Oliver Laumann says on 11.8.1994:
 *
 * Determining the faulting address in a SIGSEGV or SIGBUS handler is
 * definitely system dependent; it is even impossible on some platforms.
 *
 * In some UNIX variants, the third parameter passed to the signal handler
 * is a `struct sigcontext *scp'; the address is available either as
 * scp->sc_badvaddr (Ultrix), as scp->sc_jmpbuf.jmp_context.except[3]
 * (AIX), as scp->sc_sl.sl_ss.ss_cr21 (HP-UX), or as scp->sc_traparg_a0
 * (DEC/Alpha OSF/1).
 *
 * Or the address is passed to the handler as an additional, fourth
 * argument (BSD, SunOS 4.x); or you can set the SA_SIGINFO flag in the
 * sa_flags field of the `struct sigaction' argument to the sigaction()
 * syscall to receive a `siginfo_t *' argument with a si_addr field in
 * the handler (System V Release 4, SunOS 5.x, SGI Irix 5.1).
 */

#if defined(sun) && defined(sparc) && 0 /* Sun4, SunOS 5.3 */
#include <siginfo.h>
#define FAULT_HANDLER_ARGLIST  sig, sip, ucp
#define FAULT_HANDLER_ARGDECL  int sig; siginfo_t* sip; void* ucp;
/* ucp is really of type ucontext_t*, which is defined in <ucontext.h>. */
#define FAULT_ADDRESS  sip->si_addr
/* If SA_SIGINFO isn't specified, sip will be a NULL pointer! */
#define FAULT_ADDRESS_FROM_SIGINFO
#define WP_SIGNAL  FAULT_HANDLER(SIGSEGV)
#define CAN_HANDLE_WP_FAULT
/* They don't have getpagesize(). This thing expands into sysconf(11). */
#include <sys/param.h>
#define getpagesize() PAGESIZE
#endif

#if defined(__i386__) && defined(__NetBSD__) /* NetBSD 1.0 Beta */
/* Douglas Crosher <dtc@scrooge.ee.swin.oz.au> says on 29 August 1994:
 *
 * There is no current support in NetBSD to save the needed CR2 register
 * from which to obtain the fault address.
 */
#define WP_SIGNAL  FAULT_HANDLER(SIGBUS)
#endif

#if defined(NeXT) /* NextStep 3.2 */
/* The fault address is not passed to the signal handler. To get the fault
 * address, a Mach exception handler has to be set up, which runs in a separate
 * thread.
 */
#define WP_SIGNAL  FAULT_HANDLER(SIGBUS)
#define CAN_HANDLE_WP_FAULT
#endif

#if 0
/* Ultrix ?? */
#define FAULT_HANDLER_ARGLIST  sig, code, scp
#define FAULT_HANDLER_ARGDECL  int sig; int code; struct sigcontext *scp;
#define FAULT_ADDRESS  scp->sc_badvaddr
#define WP_SIGNAL  FAULT_HANDLER(SIGSEGV)
#define CAN_HANDLE_WP_FAULT
#endif

#if 0
/* HP-UX ?? */
#define FAULT_HANDLER_ARGLIST  sig, code, scp
#define FAULT_HANDLER_ARGDECL  int sig; int code; struct sigcontext *scp;
#define FAULT_ADDRESS  scp->sc_sl.sl_ss.ss_cr21
#define WP_SIGNAL  FAULT_HANDLER(SIGSEGV)
/* They don't have getpagesize(). */
#include <sys/param.h>
#define getpagesize() sysconf(_SC_PAGE_SIZE)
/* They don't have mprotect(). */
#define CAN_HANDLE_WP_FAULT
#endif

#if 0
/* 386BSD, FreeBSD ?? */
#define FAULT_HANDLER_ARGLIST  sig, code, scp, addr
#define FAULT_HANDLER_ARGDECL  int sig; int code; void* scp; char* addr;
#define FAULT_ADDRESS  addr
#define WP_SIGNAL  FAULT_HANDLER(SIGBUS)
#define CAN_HANDLE_WP_FAULT
#endif

#if 0
/* SysV R 4, Irix 5.1 ?? */
#include <siginfo.h>
#include <ucontext.h>
#define FAULT_HANDLER_ARGLIST  sig, sip, ucp
#define FAULT_HANDLER_ARGDECL  int sig; siginfo_t *sip; ucontext_t *ucp;
#define FAULT_ADDRESS  sip->si_addr
#endif


#if !defined(CAN_HANDLE_WP_FAULT)


/* A program for interactive debugging. */

char* fault_address;
typedef void* (*something) [20];
void do_nothing();

RETSIGTYPE fault_handler (sig, arg1, arg2, arg3, arg4, arg5, arg6)
  int sig;
  something arg1, arg2, arg3, arg4, arg5, arg6;
{ /* Set a breakpoint here! */
  printf("Got signal sig=%d", sig);
  if (sig==SIGSEGV)
    printf("=SIGSEGV");
#ifdef SIGBUS
  if (sig==SIGBUS)
    printf("=SIGBUS");
#endif
#ifdef SIGPROTV
  if (sig==SIGPROTV)
    printf("=SIGPROTV");
#endif
  printf(", with args 0x%lx, 0x%lx, 0x%lx, 0x%lx, 0x%lx, 0x%lx, ...\n", arg1, arg2, arg3, arg4, arg5, arg6);
  exit(0);
}

int main ()
{ fault_address = (char*) 0x12345678;
  printf("Look out for fault_address=0x%lx\n", (unsigned long)fault_address);

  install_signal(SIGSEGV,(signal_handler_t)fault_handler);
#ifdef SIGBUS
  install_signal(SIGBUS,(signal_handler_t)fault_handler);
#endif
#ifdef SIGPROTV
  install_signal(SIGPROTV,(signal_handler_t)fault_handler);
#endif

  do_nothing();
  *(char *) fault_address = 'z';
  do_nothing();

  printf("Got no exception!!\n");
  exit(1);
}

void do_nothing() { }


#else /* CAN_HANDLE_WP_FAULT */


/* Test the database entry. */


#ifndef UNIXCONF

/* More information from unixconf.h */

/* CL_CADDR_T */
#undef CADDR_T

/* CL_SIGNAL_REINSTALL */
/* Define if signal handlers need to be reinstalled when they are activated. */
#undef SIGNAL_NEED_REINSTALL

/* CL_GETPAGESIZE */
/* Define if you have getpagesize(). */
#undef HAVE_GETPAGESIZE
/* Define as the return type of getpagesize(). */
#undef RETGETPAGESIZETYPE

/* CL_MMAP */
/* Define if you have <sys/mman.h> and the mmap() function. */
#undef HAVE_MMAP

#endif


#ifndef getpagesize
#ifdef HAVE_GETPAGESIZE
extern RETGETPAGESIZETYPE getpagesize(/* void */);
#else
#error "Need getpagesize() for the test."
#endif
#endif

#ifdef NeXT /* NeXTstep has Mach VM. */
#include <sys/types.h>
#include <sys/resource.h>
#include <mach/mach.h>
#include <mach/machine/vm_param.h>
#define mmap(addr,len,prot,unused_flags,unused_fd,unused_off) \
  ({vm_address_t address = addr; \
    vm_allocate(task_self(),&address,len,!(addr)); \
    address; \
  })
#define mprotect(addr,len,prot) \
  (vm_protect(task_self(),addr,len,0,prot)==KERN_SUCCESS ? 0 : -1)
#define PROT_NONE 0
#define PROT_READ VM_PROT_READ
#define PROT_WRITE VM_PROT_WRITE
#define PROT_EXEC VM_PROT_EXECUTE
#else
#ifdef HAVE_MMAP
#include <sys/types.h>
#include <sys/mman.h>
extern void* mmap (/* void* addr, size_t len, int prot, int flags, int fd, off_t off */);
extern int mprotect (/* void* addr, size_t len, int prot */);
#else
#error "Need mprotect() for the test."
#endif
#endif

#ifndef PROT_READ_WRITE
#define PROT_READ_WRITE  PROT_READ | PROT_WRITE
#endif


unsigned long pagesize;
#define page_align(address)  (char*)((unsigned long)(address) & -pagesize)
char* fault_address;
int signalled;
int wp_sig_nr;
void do_nothing();

void fault_handler (FAULT_HANDLER_ARGLIST)
  FAULT_HANDLER_ARGDECL
{ char* address = (char*)(FAULT_ADDRESS);
  printf("Entering handler, sig=%d, address=0x%lx, fault_address=0x%lx.\n",
         sig, (unsigned long) address, (unsigned long) fault_address);
  if (address != fault_address)
    { printf("Doesn't work!!\n"); return; }
  signalled = 1; wp_sig_nr = sig;
  mprotect(page_align(address),pagesize,PROT_READ_WRITE);
  install_signal(sig,(signal_handler_t)fault_handler);
  printf("Exiting handler.\n");
  return;
}

int main ()
{ /* Get the page size. */
  pagesize = getpagesize();
  if ((pagesize-1) & pagesize)
    { printf("Pagesize=0x%lx is not a power of 2.\n", (unsigned long) pagesize);
      exit(1);
    }
  /* allocate some memory */
 {char* area;
#ifdef HAVE_MMAP
  /* Try to allocate mmap()ed memory. On AIX 3.2 (and maybe HP-UX 9.0 and OSF/1
   * as well) mprotect() does not work on normal malloc()ed memory.
   */
#ifdef _AIX
  area = mmap(0,6*pagesize, PROT_READ_WRITE, MAP_ANONYMOUS | MAP_VARIABLE, -1, 0);
#elif !defined(MAP_ANON)
  area = mmap(0,6*pagesize, PROT_READ_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
#else
  area = mmap(0,6*pagesize, PROT_READ_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
#endif
  if ((long)area < 0)
#endif
    { area = malloc(6*pagesize);
      if (!area)
        { printf("No memory.\n"); exit(1); }
    }
  fault_address = area + pagesize*7/2;
  /* make it read-only */
  if (mprotect(page_align(fault_address),pagesize,PROT_READ) < 0)
    { perror("mprotect failed:"); exit(1); }
  /* Install the handler */
#define FAULT_HANDLER(sig)  install_signal(sig,(signal_handler_t)fault_handler);
  WP_SIGNAL
#undef FAULT_HANDLER
  /* First test: write should provoke a signal */
  signalled = 0;
  do_nothing();
  *(char*)fault_address = 'z';
  do_nothing();
  if (!signalled)
    { printf("mprotect() didn't make the memory write-protected.\n"); exit(1); }
  if (*(char*)fault_address != 'z')
    { printf("Failed to resume write instruction correctly.\n"); exit(1); }
  /* Second test: no signal please, this time */
  signalled = 0;
  do_nothing();
  *(char*)fault_address = 'x';
  do_nothing();
  if (signalled)
    { printf("mprotect() didn't make the memory read-write.\n"); exit(1); }
  if (*(char*)fault_address != 'x')
    { printf("This shouldn't be.\n"); exit(1); }
  /* Everything worked. */
  printf("Seems to work.\n");
  exit(0);
}}

void do_nothing() { }


#endif /* CAN_HANDLE_WP_FAULT */


/* Subroutine for installing the handler. */
void install_signal (sig, handler)
  int sig;
  signal_handler_t handler;
{
#ifdef FAULT_ADDRESS_FROM_SIGINFO
  struct sigaction action, dummy_action;
  /* Get default action. */
  dummy_action.sa_handler = handler;
  dummy_action.sa_flags = 0;
  sigemptyset(&dummy_action.sa_mask);
  sigaction(sig,&dummy_action,&action);
  /* Modify it. */
  action.sa_handler = handler;
  action.sa_flags |= SA_SIGINFO;
  /* Activate it. */
  sigaction(sig,&action,(struct sigaction *)0);
#else
  signal(sig,handler);
#endif
}

