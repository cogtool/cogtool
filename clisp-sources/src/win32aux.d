/*
 * Auxiliary functions for CLISP on Win32
 * Bruno Haible 1997-2003
 * Sam Steingold 1999-2003
 */

#include "lispbibl.c"

/* File handles of standard input and standard output */
global Handle stdin_handle = INVALID_HANDLE_VALUE;
global Handle stdout_handle = INVALID_HANDLE_VALUE;
global Handle stderr_handle = INVALID_HANDLE_VALUE;

/* Auxiliary event for fd_read and fd_write. */
local HANDLE aux_event;

#ifndef UNICODE
/* when UNICODE is defined, console i/o is translated through
 the encoding mechanism.
 The encodings for *TERMINAL-IO* and *KEYBOARD-INPUT* should be
 set to the OEM codepage (see GetConsole[Output]CP() in Windows API) */

/* Character conversion table for OEM->ANSI. */
local char OEM2ANSI_table[256+1];

/* Character conversion table for ANSI->OEM. */
local char ANSI2OEM_table[256+1];

#endif

/* Auxiliary event for interrupt handling. */
local HANDLE sigint_event;
local HANDLE sigbreak_event;

/* Winsock library initialization flag */
local bool winsock_initialized = false;

/* COM library initialization flag */
local bool com_initialized = false;

/* Early/late error print function. The problem of early/late errors is
   complex, this is a simple kind of temporary solution */
local void earlylate_asciz_error (const char * description, bool fatal_p) {
  full_write(stderr_handle,description,strlen(description));
  if (fatal_p) _exit(1); /* FIXME: no finalization, no closing files! */
}

/* Initialization. */
global void init_win32 (void)
{
  /* Standard input/output handles. */
  stdin_handle = GetStdHandle(STD_INPUT_HANDLE);
  stdout_handle = GetStdHandle(STD_OUTPUT_HANDLE);
  stderr_handle = GetStdHandle(STD_ERROR_HANDLE);
  /* What to do if one of these is == INVALID_HANDLE_VALUE ?? */
  /* Auxiliary events. */
  aux_event = CreateEvent(NULL, true, false, NULL);
  sigint_event = CreateEvent(NULL, true, false, NULL);
  sigbreak_event = CreateEvent(NULL, true, false, NULL);
 #ifndef UNICODE
  { /* Translation table for console input. */
    var int i;
    for (i = 0; i < 256; i++)
      OEM2ANSI_table[i] = i;
    OEM2ANSI_table[i] = '\0';
    OemToChar(&OEM2ANSI_table[1],&OEM2ANSI_table[1]);
  }
  { /* Translation table for console output. */
    var int i;
    for (i = 0; i < 256; i++)
      ANSI2OEM_table[i] = i;
    ANSI2OEM_table[i] = '\0';
    CharToOem(&ANSI2OEM_table[1],&ANSI2OEM_table[1]);
  }
 #endif
  /* Initialize COM for shell link resolution */
  if (CoInitialize(NULL) == S_OK)
    com_initialized = true;
  { /* Winsock. */
    var WSADATA data;
    if (WSAStartup(MAKEWORD(1,1),&data)) {
      winsock_initialized = 0;
      earlylate_asciz_error("\n*** - Failed to initialize winsock library\n",0);
    } else winsock_initialized = true;
  }
}

global void done_win32 (void) {
  if (winsock_initialized && WSACleanup()) {
    earlylate_asciz_error("\n*** - Failed to shutdown winsock library\n",0);
  }
  winsock_initialized = 0;
  if (com_initialized) {
    CoUninitialize();
    com_initialized = false;
  }
}

/* Ctrl-C-interruptibility.
 We treat Ctrl-C as under Unix: Enter a break loop, continuable if possible.
 We treat Ctrl-Break as usual under Windows: abort the application, as if
 (exit t) was called, with exit code 130.
   Call fn(arg), being able to abort it if Ctrl-C is pressed.
   Not reentrant, hence fn() should be very simple and not invoke callbacks.
   fn() should return 0 once it terminated successfully.
   Returns true if successful, false if interrupted. */
local BOOL DoInterruptible (LPTHREAD_START_ROUTINE fn, LPVOID arg, BOOL socketp);
local BOOL interruptible_active;
local HANDLE interruptible_thread;
local BOOL interruptible_socketp;
local DWORD interruptible_abort_code;

local BOOL temp_interrupt_handler (DWORD CtrlType)
{
  var DWORD thread_exit_code = 0;
  if (CtrlType == CTRL_C_EVENT || CtrlType == CTRL_BREAK_EVENT) {
    /* Could invoke a signal handler at this point.?? */
    if (interruptible_active) {
      /* Set interruptible_active to false, so we won't get here a
       second time and try to terminate the same thread twice. */
      interruptible_active = false;
      /* Terminate the interruptible operation, set the exitcode to 1. */
      if (interruptible_socketp) {
       #ifndef __MINGW32__
        WSACancelBlockingCall();
       #endif
      }
      /* We treat error as nonexistent thread which shouldn't be closed */
      if (GetExitCodeThread(interruptible_thread,&thread_exit_code)
          && thread_exit_code == STILL_ACTIVE)
        if (!TerminateThread(interruptible_thread,0)) {
          OS_error();
        }
      interruptible_abort_code = 1+CtrlType;
    }
    /* Don't invoke the other handlers (in particular, the default handler) */
    return true;
  } else {
    /* Do invoke the other handlers. */
    return false;
  }
}

local BOOL DoInterruptible(LPTHREAD_START_ROUTINE fn, LPVOID arg, BOOL socketp)
{
  var HANDLE thread;
  var DWORD thread_id;
  thread = CreateThread(NULL,10000,fn,arg,0,&thread_id);
  if (thread==NULL) {
    OS_error();
  }
  interruptible_active = false;
  interruptible_thread = thread;
  interruptible_abort_code = 0;
  interruptible_socketp = socketp;
  SetConsoleCtrlHandler((PHANDLER_ROUTINE)temp_interrupt_handler,true);
  interruptible_active = true;
  WaitForSingleObject(interruptible_thread,INFINITE);
  interruptible_active = false;
  SetConsoleCtrlHandler((PHANDLER_ROUTINE)temp_interrupt_handler,false);
  CloseHandle(interruptible_thread);
  if (!interruptible_abort_code) {
    return true;                /* successful termination */
  } else {
    if (interruptible_abort_code == 1+CTRL_BREAK_EVENT) {
      final_exitcode = 130; quit(); /* aborted by Ctrl-Break */
    }
    return false;               /* aborted by Ctrl-C */
  }
}


/* Sleep a certain time.
 Return true after normal termination, false if interrupted by Ctrl-C. */
global BOOL msleep (DWORD milliseconds);
local DWORD WINAPI do_sleep (LPVOID arg)
{
  Sleep((DWORD)arg);
  return 0;
}
global BOOL msleep (DWORD milliseconds)
{
  return DoInterruptible(&do_sleep,(void*)milliseconds,false);
}

/* Sleep a certain time. */
global unsigned int sleep (unsigned int seconds)
{
  msleep(seconds*1000);
  return 0;                     /* the return value is wrong */
}


/* To catch Ctrl-C events, we use a separate thread which waits for an event,
 and install (using SetConsoleCtrlHandler()) a Ctrl-C handler which sends
 an event to this thread. The thread then waits for an appropriate moment
 to let the main thread hyperjump to the desired signal handler.
 We choose this approach because:
 - we don't know in which thread the handler installed via
   SetConsoleCtrlHandler() will be executed (main thread? separate thread?
   any other thread?),
 - the desired signal handler may longjmp(), hence we don't want it to
   be executed in any other thread than the main thread,
 - letting another thread hyperjump is feasible (through GetThreadContext()/
   SetThreadContext()), but a thread cannot hyperjump itself. */

local HANDLE main_thread = INVALID_HANDLE_VALUE;
local HANDLE sigint_thread = INVALID_HANDLE_VALUE;

/* Destination for hyperjump. Normally &interrupt_handler, but set to &quit
   when a Ctrl-Break was seen. */
local DWORD hyperjump_dest = (DWORD)&interrupt_handler;

local BOOL normal_interrupt_handler (DWORD CtrlType)
{
  if (CtrlType == CTRL_C_EVENT || CtrlType == CTRL_BREAK_EVENT) {
    /* Send an event to the sigint_thread. */
    interrupt_pending = true;
    if (CtrlType == CTRL_C_EVENT) {
      if (!PulseEvent(sigint_event)) {
        OS_error();
      }
    } elif (CtrlType == CTRL_BREAK_EVENT) {
      if (!PulseEvent(sigbreak_event)) {
        OS_error();
      }
    }
    /* Don't invoke the other handlers (in particular, the default handler) */
    return true;
  } else /* Do invoke the other handlers. */
    return false;
}

local DWORD WINAPI do_sigintwait (LPVOID arg)
{
  var int waitstate = 0;     /* 0: infinite, 1: 0.5 sec, 2: 0.05 sec. */
  var local DWORD wait_duration[3] = { INFINITE, 500, 50 };
  var HANDLE waitfor[2];
  waitfor[0] = sigint_event;
  waitfor[1] = sigbreak_event;
  for (;;) switch
    (WaitForMultipleObjects(2,&waitfor[0],false,wait_duration[waitstate])) {
      case WAIT_OBJECT_0+0:
        /* Got a sigint_event! */
        if (!interrupt_pending) { /* already being handled? */
          waitstate = 0; break;
        }
        if (waitstate==0) {
          /* Do not hyperjump right now. Wait a while - maybe
           interrupt_pending=true causes a continuable interruption. */
          waitstate = 1; break;
        }
        goto try_hyperjump;
      case WAIT_OBJECT_0+1:
        /* Got a sigbreak_event! */
        final_exitcode = 130; hyperjump_dest = (DWORD)&quit;
        goto try_hyperjump;
      case WAIT_TIMEOUT:
        if (!interrupt_pending) { /* already being handled? */
          waitstate = 0; break;
        }
        goto try_hyperjump;
      try_hyperjump:
        waitstate = 2;
        /* Stop the main thread so that it can't run while we hyperjump it. */
        SuspendThread(main_thread);
        if (break_sems_cleared()) {
          /* OK, the moment has come to hyperjump the main thread.
           printf("\nHyperjumping! interrupt_pending = %d, waitstate = %d\n",interrupt_pending,waitstate); */
          var CONTEXT context;
          context.ContextFlags = CONTEXT_CONTROL | CONTEXT_INTEGER;
          GetThreadContext(main_thread,&context);
          context.Eip = hyperjump_dest;
          SetThreadContext(main_thread,&context);
          /* Now it's ok to release the main thread. */
          waitstate = 0;
        }
        ResumeThread(main_thread);
        break;
      default: NOTREACHED;
    }
}

global void install_sigint_handler (void)
{
  ASSERT(main_thread==INVALID_HANDLE_VALUE &&
         sigint_thread==INVALID_HANDLE_VALUE);
  /* Get main_thread, assuming we are in the main thread. */
  if (!DuplicateHandle(GetCurrentProcess(),GetCurrentThread(),
                       GetCurrentProcess(),&main_thread,
                       0, false, DUPLICATE_SAME_ACCESS)) {
    OS_error();
  }
  /* Start sigint_thread. */
  {
    var DWORD thread_id;
    var HANDLE thread =
      CreateThread(NULL,10000,(LPTHREAD_START_ROUTINE)do_sigintwait,
                   (LPVOID)0,0,&thread_id);
    if (thread==NULL) {
      OS_error();
    }
    sigint_thread = thread;
  }
  /* Install normal_interrupt_handler. */
  if (!SetConsoleCtrlHandler((PHANDLER_ROUTINE)normal_interrupt_handler,true)) {
    OS_error();
  }
}


/* Limit for the size of a buffer we pass to WriteFile() and similar calls.
 Before introducing this, I have seen mem file corruption: The file produced
 by SAVEINITMEM contained wrong data. More exactly, inside a WriteFile()
 block of size about 75 KB, a block of exactly 8192 bytes, starting at
 an odd address, was shifted to the right by 3 bytes, with zeroes inserted
 at the beginning and three bytes shifted out and lost at the end of this
 8KB block. This problem sometimes disappeared spontaneously, sometimes by
 rebooting, and was sometimes 100% reproducible (the faulty block being
 always at the same address).
 This could be a bug in WinNT 3.51 or in the NFS client. In any case, it's
 better to work around it: Don't pass areas larger than 64 KB to WriteFile().
 Similarly for ReadFile() and similar calls, since we don't know exactly
 where the bug sits.
 PS: I know that this was necessary in VMS. But this is Win32, not VMS ... */
#define MAX_IO  32768

/* Reading from a console.
 Normally, ReadConsoleInput() waits until an keyboard event occurs. Ctrl-C
 is *not* a keyboard event. If the user presses Ctrl-C during a
 ReadConsoleInput() call, hyperjumping takes place, but the main thread
 is not scheduled until a keyboard event occurs. To avoid this, let
 the ReadConsoleInput() call be performed in a separate thread.
 It doesn't make sense to call this with Length > 1 (because some typed
 characters might get lost). */
struct ReadConsoleInput_params {
  HANDLE ConsoleInput; PINPUT_RECORD Buffer; LPDWORD NumberOfEventsRead;
  BOOL retval; DWORD errcode;
};
local DWORD WINAPI do_ReadConsoleInput (LPVOID arg)
{
  var struct ReadConsoleInput_params * params =
    (struct ReadConsoleInput_params *)arg;
  params->retval =
    ReadConsoleInput(params->ConsoleInput,params->Buffer,1,
                     params->NumberOfEventsRead);
  if (!params->retval)
    params->errcode = GetLastError();
  return 0;
}
/* Like ReadConsoleInput with Length==1, but is interruptible by Ctrl-C. */
global BOOL ReadConsoleInput1 (HANDLE ConsoleInput, PINPUT_RECORD Buffer,
                               LPDWORD NumberOfEventsRead)
{
  var struct ReadConsoleInput_params params;
  params.ConsoleInput       = ConsoleInput;
  params.Buffer             = Buffer;
  params.NumberOfEventsRead = NumberOfEventsRead;
  params.retval             = 0;
  params.errcode            = 0;
  if (DoInterruptible(&do_ReadConsoleInput,(void*)&params,false)) {
    if (!params.retval)
      SetLastError(params.errcode);
    return params.retval;
  } else {
    SetLastError(ERROR_SIGINT); return false;
  }
}


/* Determines whether ReadFile() on a file/pipe/console handle will hang.
   Returns 0 for yes, 1 for no (or 2 for known EOF or 3 for available byte),
   -1 for unknown. */
global int fd_read_wont_hang_p (HANDLE fd)
{
  # This is pretty complex. To test this, create a file "listen.lisp"
  # containing the code
  #   (tagbody 1 (prin1 (listen *terminal-io*)) (sys::%sleep 0 500) (go 1))
  # and execute "lisp.exe -q -i listen.lisp" with redirected standard input.
  switch (GetFileType(fd)) {
    case FILE_TYPE_CHAR:
      {
        var DWORD nevents;
        if (GetNumberOfConsoleInputEvents(fd,&nevents)) { # It's a console.
          if (nevents==0)
            return 0;
          var INPUT_RECORD* events =
            (INPUT_RECORD*)alloca(nevents*sizeof(INPUT_RECORD));
          var DWORD nevents_read;
          var DWORD mode;
          if (!PeekConsoleInput(fd,events,nevents,&nevents_read)) {
            OS_error();
          }
          if (nevents_read==0)
            return 0;
          if (!GetConsoleMode(fd,&mode)) {
            OS_error();
          }
          if (mode & ENABLE_LINE_INPUT) {
            # Look out for a Key-Down event corresponding to CR/LF.
            var DWORD i;
            for (i = 0; i < nevents_read; i++) {
              if (events[i].EventType == KEY_EVENT
                  && events[i].Event.KeyEvent.bKeyDown
                  && events[i].Event.KeyEvent.uAsciiChar == CR)
                # probably a byte available (except if it is Ctrl-Z)
                return -1;
            }
          } else { # Look out for any Key-Down event.
            var DWORD i;
            for (i = 0; i < nevents_read; i++) {
              if (events[i].EventType == KEY_EVENT
                  && events[i].Event.KeyEvent.bKeyDown
                  && events[i].Event.KeyEvent.uAsciiChar != 0)
                # probably a byte available (except if it is Ctrl-Z)
                return -1;
            }
          }
          return 0;
        } else if (!(GetLastError()==ERROR_INVALID_HANDLE)) {
          OS_error();
        }
      }
      # Not a console.
      switch (WaitForSingleObject(fd,0)) {
        case WAIT_OBJECT_0: # a byte is available, or EOF
          return 1;
        case WAIT_TIMEOUT:
          return 0;
        default:
          OS_error();
      }
    case FILE_TYPE_DISK:
      return 1;
    case FILE_TYPE_PIPE:
      {
        var DWORD nbytes;
        if (PeekNamedPipe(fd,NULL,0,NULL,&nbytes,NULL)) { # input pipe
          if (nbytes > 0)
            return 3;
          else
            return 0;
        } else if (GetLastError()==ERROR_BROKEN_PIPE) { # EOF reached
          return 2;
        } else if (GetLastError()==ERROR_ACCESS_DENIED) { # output pipe
          # => fake EOF.
          return 2;
        } else {
          OS_error();
        }
      }
    default: # It's a file (or something unknown).
      return -1;
  }
}

/* A wrapper around ReadFile() that supports different perseverances.
   Return value like read().
   When the return value is 0, it sets errno to indicate whether EOF has been
   seen (ERROR_HANDLE_EOF) or whether it is not yet known (ERROR_IO_PENDING). */
/* Reading from a file/pipe/console handle.
 This is the non-interruptible routine. */
local int lowlevel_fd_read (HANDLE fd, void* bufarea, size_t nbyte, perseverance_t persev) {
  if (nbyte == 0) {
    SetLastError(ERROR_IO_PENDING);
    return 0;
  }
 #if defined(GENERATIONAL_GC) && defined(SPVW_MIXED)
  handle_fault_range(PROT_READ_WRITE,(aint)bufarea,(aint)bufarea+nbyte);
 #endif
  var char* buf = (char*) bufarea;
  var int done = 0;
  do {
    if (persev == persev_immediate || persev == persev_bonus) {
      int wont_hang = fd_read_wont_hang_p(fd);
      if (wont_hang == 0) {
        SetLastError(ERROR_IO_PENDING);
        break;
      }
      if (wont_hang < 0) {
        if (persev == persev_bonus) {
          SetLastError(ERROR_IO_PENDING);
          break;
        }
      }
    }
    var int limited_nbyte = (nbyte <= MAX_IO ? nbyte : MAX_IO);
    var OVERLAPPED overlap;
    var DWORD nchars;
    var DWORD err;
    overlap.OffsetHigh = 0;
    overlap.Offset = SetFilePointer(fd, 0, (LONG*)&overlap.OffsetHigh,
                                    FILE_CURRENT);
    ResetEvent(aux_event);
    overlap.hEvent = aux_event;
    if (ReadFile(fd, buf, limited_nbyte, &nchars, &overlap))
      goto ok;
    /* Disk files (and maybe other handle types) don't support
       overlapped I/O on Win95. */
    err = GetLastError();
    if (err == ERROR_INVALID_PARAMETER) {
      if (ReadFile(fd, buf, limited_nbyte, &nchars, NULL))
        goto ok;
      err = GetLastError();
      /* On Win95, console handles need special handling. */
      if (err == ERROR_INVALID_PARAMETER) {
        if (ReadConsole(fd, buf, limited_nbyte, &nchars, NULL))
          goto ok;
        err = GetLastError();
      }
    }
    if (err == ERROR_HANDLE_EOF || err == ERROR_BROKEN_PIPE) {
      SetLastError(ERROR_HANDLE_EOF);
      break;
    }
    if (err != ERROR_IO_PENDING)
      return -1;
    if (!GetOverlappedResult(fd, &overlap, &nchars,
                             persev != persev_immediate && persev != persev_bonus)) {
      if (GetLastError() == ERROR_HANDLE_EOF) {
        SetLastError(ERROR_HANDLE_EOF);
        break;
      }
      return -1;
    }
    if (nchars == 0 && (persev == persev_immediate || persev == persev_bonus)) {
      SetLastError(ERROR_IO_PENDING);
      break;
    }
   ok:
    if (nchars == 0) {
      SetLastError(ERROR_HANDLE_EOF);
      break;
    }
    buf += nchars; done += nchars; nbyte -= nchars;
    if (persev != persev_full && nchars < MAX_IO)
      break;
    if (persev == persev_partial)
      persev = persev_bonus;
  } while (nbyte != 0);
 #ifndef UNICODE
  /* Possibly translate characters. */
  if (done > 0) {
    var int i;
    for (i = -done; i < 0; i++) {
      var unsigned char c = (unsigned char)buf[i];
      if (!(c == (unsigned char)OEM2ANSI_table[c]))
        goto maybe_translate;
    }
    /* No character found for which translation makes a difference,
     hence no need to translate. */
    if (false) {
     maybe_translate:
      var DWORD console_mode;
      if (GetConsoleMode(fd,&console_mode)) {
        /* It's a console, must really translate characters! */
        for (i = -done; i < 0; i++)
          buf[i] = OEM2ANSI_table[(unsigned char)buf[i]];
      }
    }
  }
 #endif
  return done;
}
/* Then we make it interruptible. */
struct fd_read_params {
  HANDLE fd; void* buf; size_t nbyte; perseverance_t persev;
  int retval; DWORD errcode;
};
local DWORD WINAPI do_fd_read (LPVOID arg) {
  var struct fd_read_params * params = (struct fd_read_params *)arg;
  params->retval = lowlevel_fd_read(params->fd,params->buf,params->nbyte,
                                    params->persev);
  if (params->retval <= 0)
    params->errcode = GetLastError();
  return 0;
}
global ssize_t fd_read (HANDLE fd, void* buf, size_t nbyte,
                        perseverance_t persev) {
  var struct fd_read_params params;
  params.fd      = fd;
  params.buf     = buf;
  params.nbyte   = nbyte;
  params.persev  = persev;
  params.retval  = 0;
  params.errcode = 0;
  if (DoInterruptible(&do_fd_read,(void*)&params,false)) {
    if (params.retval <= 0)
      SetLastError(params.errcode);
    return params.retval;
  } else {
    SetLastError(ERROR_SIGINT); return -1;
  }
}

/* Determines whether WriteFile() to a file/pipe/console handle will hang.
   Returns 1 for yes, 0 for no, -1 for unknown. */
local inline int fd_write_will_hang_p (HANDLE fd)
{
  switch (GetFileType(fd)) {
    case FILE_TYPE_DISK:
      return 0;
    default:
      return -1;
  }
}

/* A wrapper around WriteFile() that supports different perseverances.
   Return value like write().
   When the return value is 0, it sets errno to indicate whether EOWF has been
   seen (ERROR_HANDLE_EOF) or whether it is not yet known (ERROR_IO_PENDING). */
global ssize_t fd_write (HANDLE fd, const void* b, size_t nbyte,
                         perseverance_t persev) {
  if (nbyte == 0) {
    SetLastError(ERROR_IO_PENDING);
    return 0;
  }
#if defined(GENERATIONAL_GC) && defined(SPVW_MIXED)
  handle_fault_range(PROT_READ,(aint)b,(aint)b+nbyte);
#endif
  var const char* buf = (const char*) b;
#ifndef UNICODE
  /* Possibly translate characters. */
  if (nbyte > 0) {
    var int i;
    for (i = 0; i < nbyte; i++) {
      var unsigned char c = (unsigned char)buf[i];
      if (c != (unsigned char)ANSI2OEM_table[c])
        goto maybe_translate;
    }
    /* No character found for which translation makes a difference,
       hence no need to translate. */
    if (false) {
     maybe_translate:
      var DWORD console_mode;
      if (GetConsoleMode(fd,&console_mode)) {
        /* It's a console, must really translate characters! */
        var char* newbuf = alloca(nbyte);
        for (i = 0; i < nbyte; i++)
          newbuf[i] = ANSI2OEM_table[(unsigned char)buf[i]];
        buf = newbuf;
      }
    }
  }
#endif
  var int done = 0;
  do {
    /* Possibly check for Ctrl-C here ?? */
    if (persev == persev_immediate || persev == persev_bonus) {
      int will_hang = fd_write_will_hang_p(fd);
      if (will_hang > 0) {
        SetLastError(ERROR_IO_PENDING);
        break;
      }
      if (will_hang < 0) {
        if (persev == persev_bonus) {
          SetLastError(ERROR_IO_PENDING);
          break;
        }
      }
    }
    var int limited_nbyte = (nbyte <= MAX_IO ? nbyte : MAX_IO);
    var OVERLAPPED overlap;
    var DWORD nchars;
    var DWORD err;
    overlap.OffsetHigh = 0;
    overlap.Offset = SetFilePointer(fd, 0, (LONG*)&overlap.OffsetHigh,
                                    FILE_CURRENT);
    ResetEvent(aux_event);
    overlap.hEvent = aux_event;
    if (WriteFile(fd, buf, limited_nbyte, &nchars, &overlap))
      goto ok;
    /* Disk files (and maybe other handle types) don't support
       overlapped I/O on Win95. */
    err = GetLastError();
    if (err == ERROR_INVALID_PARAMETER) {
      if (WriteFile(fd, buf, limited_nbyte, &nchars, NULL))
        goto ok;
      err = GetLastError();
      /* On Win95, console handles need special handling. */
      if (err == ERROR_INVALID_PARAMETER) {
        if (WriteConsole(fd, buf, limited_nbyte, &nchars, NULL))
          goto ok;
        err = GetLastError();
      }
    }
    if (err != ERROR_IO_PENDING)
      return -1;
    if (!GetOverlappedResult(fd, &overlap, &nchars,
                             persev != persev_immediate && persev != persev_bonus))
      return -1;
    if (nchars == 0 && (persev == persev_immediate || persev == persev_bonus)) {
      SetLastError(ERROR_IO_PENDING);
      break;
    }
   ok:
    if (nchars == 0) {
      SetLastError(ERROR_HANDLE_EOF);
      break;
    }
    buf += nchars; done += nchars; nbyte -= nchars;
    if (persev != persev_full && nchars < MAX_IO)
      break;
    if (persev == persev_partial)
      persev = persev_bonus;
  } while (nbyte != 0);
  return done;
}

/* Determines whether recv() on a socket will hang.
   Returns 1 for yes, 0 for no, -1 for unknown. */
local inline int sock_read_will_hang_p (int fd)
{
  # Use select() with readfds = singleton set {fd}
  # and timeout = zero interval.
  var fd_set handle_set; # set of handles := {fd}
  var struct timeval zero_time; # time interval := 0
  FD_ZERO(&handle_set); FD_SET(fd,&handle_set);
 restart_select:
  zero_time.tv_sec = 0; zero_time.tv_usec = 0;
  var int result = select(FD_SETSIZE,&handle_set,NULL,NULL,&zero_time);
  if (result<0) {
    if (sock_errno_is(EINTR))
      goto restart_select;
    SOCK_error();
  } else {
    # result = number of handles in handle_set for which read() would
    # return without blocking.
    if (result==0)
      return 1;
  }
  # Now we know that recv() will return immediately.
  return 0;
}

/* A wrapper around recv() that supports different perseverances.
   Return value like read().
   When the return value is 0, it sets errno to indicate whether EOF has been
   seen (ENOENT) or whether it is not yet known (EAGAIN). */
/* Reading from a socket.
   This is the non-interruptible routine. */
local int lowlevel_sock_read (SOCKET fd, void* b, size_t nbyte, perseverance_t persev)
{
  if (nbyte == 0) {
    sock_set_errno(EAGAIN);
    return 0;
  }
#if defined(GENERATIONAL_GC) && defined(SPVW_MIXED)
  handle_fault_range(PROT_READ_WRITE,(aint)b,(aint)b+nbyte);
#endif
  var char* buf = (char*) b;
  var int done = 0;
  do {
    if (persev == persev_immediate || persev == persev_bonus) {
      int will_hang = sock_read_will_hang_p(fd);
      if (will_hang > 0) {
        sock_set_errno(EAGAIN);
        break;
      }
      if (will_hang < 0) {
        #if 1
          NOTREACHED;
        #else
          if (persev == persev_bonus) {
            sock_set_errno(EAGAIN);
            break;
          }
        #endif
      }
    }
    var int limited_nbyte = (nbyte <= MAX_IO ? nbyte : MAX_IO);
    var int retval = recv(fd,buf,limited_nbyte,0);
    if (retval == 0) {
      sock_set_errno(ENOENT);
      break;
    } else if (retval < 0)
      return retval;
    else {
      buf += retval; done += retval; nbyte -= retval;
      if (persev != persev_full && retval < MAX_IO)
        break;
      if (persev == persev_partial)
        persev = persev_bonus;
    }
  } while (nbyte != 0);
  return done;
}
/* Then we make it interruptible. */
struct sock_read_params {
  SOCKET fd; void* buf; size_t nbyte; perseverance_t persev;
  int retval; int errcode;
};
local DWORD WINAPI do_sock_read (LPVOID arg)
{
  var struct sock_read_params * params = (struct sock_read_params *)arg;
  params->retval = lowlevel_sock_read(params->fd,params->buf,params->nbyte,
                                      params->persev);
  if (params->retval <= 0)
    params->errcode = WSAGetLastError();
  return 0;
}
global int sock_read (SOCKET fd, void* buf, size_t nbyte, perseverance_t persev)
{
  var struct sock_read_params params;
  params.fd      = fd;
  params.buf     = buf;
  params.nbyte   = nbyte;
  params.persev  = persev;
  params.retval  = 0;
  params.errcode = 0;
  if (DoInterruptible(&do_sock_read,(void*)&params,true)) {
    if (params.retval <= 0)
      WSASetLastError(params.errcode);
    return params.retval;
  } else {
    WSASetLastError(WSAEINTR); return -1;
  }
}

/* Determines whether send() on a socket will hang.
   Returns 1 for yes, 0 for no, -1 for unknown. */
local inline int sock_write_will_hang_p (int fd)
{
  # Use select() with writefds = singleton set {fd}
  # and timeout = zero interval.
  var fd_set handle_set; # set of handles := {fd}
  var struct timeval zero_time; # time interval := 0
  FD_ZERO(&handle_set); FD_SET(fd,&handle_set);
 restart_select:
  zero_time.tv_sec = 0; zero_time.tv_usec = 0;
  var int result = select(FD_SETSIZE,NULL,&handle_set,NULL,&zero_time);
  if (result<0) {
    if (sock_errno_is(EINTR))
      goto restart_select;
    SOCK_error();
  } else {
    # result = number of handles in handle_set for which write() would
    # return without blocking.
    if (result==0)
      return 1;
  }
  # Now we know that send() will return immediately.
  return 0;
}

/* A wrapper around send() that supports different perseverances.
   Return value like write().
   When the return value is 0, it sets errno to indicate whether EOWF has been
   seen (ENOENT) or whether it is not yet known (EAGAIN). */
/* Writing to a socket.
   This is the non-interruptible routine. */
local int lowlevel_sock_write (SOCKET fd, const void* b, size_t nbyte, perseverance_t persev)
{
  if (nbyte == 0) {
    sock_set_errno(EAGAIN);
    return 0;
  }
#if defined(GENERATIONAL_GC) && defined(SPVW_MIXED)
  handle_fault_range(PROT_READ,(aint)b,(aint)b+nbyte);
#endif
  var const char* buf = (const char*) b;
  var int done = 0;
  do {
    if (persev == persev_immediate || persev == persev_bonus) {
      int will_hang = sock_write_will_hang_p(fd);
      if (will_hang > 0) {
        sock_set_errno(EAGAIN);
        break;
      }
      if (will_hang < 0) {
        #if 1
          NOTREACHED;
        #else
          if (persev == persev_bonus) {
            sock_set_errno(EAGAIN);
            break;
          }
        #endif
      }
    }
    var int limited_nbyte = (nbyte <= MAX_IO ? nbyte : MAX_IO);
    var int retval = send(fd,buf,limited_nbyte,0);
    if (retval == 0) {
      sock_set_errno(ENOENT);
      break;
    } else if (retval < 0)
      return retval;
    else {
      buf += retval; done += retval; nbyte -= retval;
      if (persev != persev_full && retval < MAX_IO)
        break;
      if (persev == persev_partial)
        persev = persev_bonus;
    }
  } while (nbyte != 0);
  return done;
}
/* Then we make it interruptible. */
struct sock_write_params {
  SOCKET fd; const void* buf; size_t nbyte; perseverance_t persev;
  int retval; int errcode;
};
local DWORD WINAPI do_sock_write (LPVOID arg)
{
  var struct sock_write_params * params = (struct sock_write_params *)arg;
  params->retval = lowlevel_sock_write(params->fd,params->buf,params->nbyte,
                                       params->persev);
  if (params->retval <= 0)
    params->errcode = WSAGetLastError();
  return 0;
}
global int sock_write (SOCKET fd, const void* buf, size_t nbyte, perseverance_t persev)
{
  var struct sock_write_params params;
  params.fd      = fd;
  params.buf     = buf;
  params.nbyte   = nbyte;
  params.persev  = persev;
  params.retval  = 0;
  params.errcode = 0;
  if (DoInterruptible(&do_sock_write,(void*)&params,true)) {
    if (params.retval <= 0)
      WSASetLastError(params.errcode);
    return params.retval;
  } else {
    WSASetLastError(WSAEINTR); return -1;
  }
}

/* interruptible win32 socket wait thread function */
struct socket_wait_params {
  socket_wait_event  event;
  SOCKET            handle;
  struct timeval * timeout;
  bool             success;
};

local DWORD WINAPI do_socket_wait (LPVOID arg) {
 restart_select:
  begin_system_call();
  {
    var struct socket_wait_params * params = (struct socket_wait_params *)arg;
    var int ret;
    var fd_set handle_set;
    FD_ZERO(&handle_set); FD_SET(params->handle,&handle_set);
    ret = select(FD_SETSIZE, /* 1st parameter doesnt really matters in winsock */
                 params->event==socket_wait_read?&handle_set:NULL,
                 params->event==socket_wait_write?&handle_set:NULL,
                 params->event==socket_wait_except?&handle_set:NULL,
                 params->timeout);
    if (ret < 0) {
      if (sock_errno_is(EINTR)) {
        end_system_call(); goto restart_select;
      }
      SOCK_error();
    }
      end_system_call();
      if (ret != SOCKET_ERROR && ret != 0) /* success */
        params->success = true;
      return 0;
  }
}

/* actual interface. return true on successfull wait, false when timeout
   expires or Ctrl-C or Ctrl-Break pressed */
global int interruptible_socket_wait (SOCKET socket_handle,
                                      socket_wait_event waitwhat,
                                      struct timeval * timeout_ptr) {
  var struct socket_wait_params params; /* parameters for interruptible function */
  params.event = waitwhat;              /* see select() description */
  params.handle = socket_handle;
  params.timeout = timeout_ptr;
  params.success = false;
  return DoInterruptible(&do_socket_wait,&params,true) && params.success;
}

/* Changing a file handle's position. */
global off_t lseek (HANDLE fd, off_t offset, DWORD mode)
{
  LONG offset_hi = offset >> 32;
  DWORD offset_lo = offset & 0xFFFFFFFF;
  offset_lo = SetFilePointer(fd,offset_lo,&offset_hi,mode);
  if (offset_lo == (DWORD)(-1) && GetLastError() != NO_ERROR)
    return -1;
  else
    return ((sint64)(sint32)offset_hi << 32) | (uint64)(uint32)offset_lo;
}

/* Testing for possibly interactive handle. */
global int isatty (HANDLE handle)
{
  var DWORD ftype = GetFileType(handle);
  return (ftype == FILE_TYPE_CHAR || ftype == FILE_TYPE_PIPE);
}


/* Create a new process, given a command line and two handles for standard
 input and standard output (both must be inheritable). */
global BOOL MyCreateProcess (LPTSTR CommandLine, HANDLE StdInput,
                             HANDLE StdOutput, HANDLE StdError,
                             LPPROCESS_INFORMATION ProcessInformation)
{
  var STARTUPINFO sinfo;
  sinfo.cb = sizeof(STARTUPINFO);
  sinfo.lpReserved = NULL;
  sinfo.lpDesktop = NULL;
  sinfo.lpTitle = NULL;
  sinfo.cbReserved2 = 0;
  sinfo.lpReserved2 = NULL;
  sinfo.dwFlags = STARTF_USESTDHANDLES;
  sinfo.hStdInput = StdInput;
  sinfo.hStdOutput = StdOutput;
  sinfo.hStdError = StdError;
  return CreateProcess(NULL, CommandLine, NULL, NULL, true, 0,
                       NULL, NULL, &sinfo, ProcessInformation);
}

/* I want to see a backtrace! */
int abort_dummy;
nonreturning_function(global, abort, (void))
{
#ifdef MICROSOFT
  /* This hack is necessary because if you write  1/0  the MSVC compiler
     signals an error at compilation time!! */
  var volatile int zero = 0;
  abort_dummy = 1/zero;
#else
  abort_dummy = 1/0;
#endif
  exit(-1);                     /* to turn off warning */
}


/* Print out the memory map of the process. */
global void DumpProcessMemoryMap (void)
{
  var MEMORY_BASIC_INFORMATION info;
  var aint address = 0;
  fputs("Memory dump:\n",stderr);
  while (VirtualQuery((void*)address,&info,sizeof(info)) == sizeof(info)) {
    /* Always info.BaseAddress = address. */
    switch (info.State) {
      case MEM_FREE:    fputs("-",stderr); break;
      case MEM_RESERVE: fputs("+",stderr); break;
      case MEM_COMMIT:  fputs("*",stderr); break;
      default: fputs("?",stderr); break;
    }
    fprintf(stderr," 0x%x - 0x%x",(aint)info.BaseAddress,
            (aint)info.BaseAddress+info.RegionSize-1);
    if (info.State != MEM_FREE) {
      fprintf(stderr," (0x%x) ",(aint)info.AllocationBase);
      /* info.AllocationProtect is apparently irrelevant. */
      switch (info.Protect & ~(PAGE_GUARD|PAGE_NOCACHE)) {
        case PAGE_READONLY:          fputs(" R  ",stderr); break;
        case PAGE_READWRITE:         fputs(" RW ",stderr); break;
        case PAGE_WRITECOPY:         fputs(" RWC",stderr); break;
        case PAGE_EXECUTE:           fputs("X   ",stderr); break;
        case PAGE_EXECUTE_READ:      fputs("XR  ",stderr); break;
        case PAGE_EXECUTE_READWRITE: fputs("XRW ",stderr); break;
        case PAGE_EXECUTE_WRITECOPY: fputs("XRWC",stderr); break;
        case PAGE_NOACCESS:          fputs("----",stderr); break;
        default: fputs("?",stderr); break;
      }
      if (info.Protect & PAGE_GUARD)
        fputs(" PAGE_GUARD",stderr);
      if (info.Protect & PAGE_NOCACHE)
        fputs(" PAGE_NOCACHE",stderr);
      fputs(" ",stderr);
      switch (info.Type) {
        case MEM_IMAGE:   fputs("MEM_IMAGE",stderr); break;
        case MEM_MAPPED:  fputs("MEM_MAPPED",stderr); break;
        case MEM_PRIVATE: fputs("MEM_PRIVATE",stderr); break;
        default:          fputs("MEM_?",stderr); break;
      }
    }
    fputs("\n",stderr);
    address = (aint)info.BaseAddress + info.RegionSize;
  }
  fputs("End of memory dump.\n",stderr);
}

/* file identification for check_file_re_open() */
/* if file NAMESTRING exists, fill file_id and call function on it,
   otherwise return NULL */
global void* with_file_id (char * namestring, void *data,
                           void* (*func) (struct file_id *, void *data)) {
  var HANDLE fh = CreateFile(namestring,0,FILE_SHARE_READ | FILE_SHARE_WRITE,
                             NULL,OPEN_EXISTING,OPEN_EXISTING,NULL);
  if (fh == INVALID_HANDLE_VALUE) return NULL;
  struct file_id fi;
  var errno_t status = handle_file_id(fh,&fi);
  var void* ret = status ? NULL : (*func)(&fi,data);
  CloseHandle(fh);
  return ret;
}

/* fill FI for an existing file handle */
global errno_t handle_file_id (HANDLE fh, struct file_id *fi) {
  var BY_HANDLE_FILE_INFORMATION info;
  if (!GetFileInformationByHandle(fh,&info)) return GetLastError();
  fi->nFileIndexLow = info.nFileIndexLow;
  fi->nFileIndexHigh = info.nFileIndexHigh;
  return 0;
}

/* if the file IDs are identical, return 1, otherwise return 0 */
global int file_id_eq (struct file_id *fi1, struct file_id *fi2)
{ return (fi1->nFileIndexLow == fi2->nFileIndexLow)
    && (fi1->nFileIndexHigh == fi2->nFileIndexHigh); }
