  # Handling of UNIX errors
  # OS_error();
  # > int errno: error code
    nonreturning_function(global, OS_error, (void));
  #ifdef UNIX
    nonreturning_function(global, OS_file_error, (object pathname));
  #endif

  # Problem: many different UNIX variants, each with its own set of error
  # messages.
  # Solution: The error names are quite portable. We take the english
  # error message from the system, and provide the translations ourselves.
  # French translations were provided by Tristan <marc@david.saclay.cea.fr>.

  #ifndef HAVE_STRERROR
    # Older systems have sys_nerr and sys_errlist instead of POSIX strerror().
    #if !defined(UNIX) # avoid conflict with unix.d, <stdlib.h>
      extern int sys_nerr; # Number of OS error messages
      extern char* sys_errlist[]; # Table of OS error messages
    #endif
    local const char* strerror (int errnum)
    {
      # Attention: There is no guarantee that all system error values are
      # less than sys_nerr. On IRIX 5.2, EDQUOT >= sys_nerr.
      if (errnum >= 0 && errnum < sys_nerr)
        return sys_errlist[errnum];
      else {
        errno = EINVAL;
        return NULL;
      }
    }
  #endif

  #ifdef GNU_GETTEXT
    # Let the caller of get_errormsg() translate the messages.
    #define clgettextl
  #endif

  # Returns a system error message.
  # get_errormsg(errcode,&os_error);
    typedef struct { const char* name; const char* msg; } os_error_t;
    local void get_errormsg (uintC errcode, os_error_t* errormsg)
    {
      errormsg->name = NULL;
      errormsg->msg = NULL;
      # Special case all the well-known error codes for which we have
      # good English messages and good translations. The if cascade
      # is not the most efficient code, but this function is not time
      # critical.
        # Errors in multibyte functions:
        # Test EILSEQ first, because some systems do not define it,
        # in which case iconv supplies a default value.  If that default
        # matches another errno, then we prefer to use the message for
        # that other errno rather than this message.
        #ifdef EILSEQ
        if (errcode == EILSEQ) {
          errormsg->name = "EILSEQ";
          errormsg->msg = GETTEXTL("Invalid multibyte or wide character");
        }
        #endif
        # Common UNIX errors:
        #ifdef EPERM
        if (errcode == EPERM) {
          errormsg->name = "EPERM";
          errormsg->msg = GETTEXTL("Operation not permitted");
        }
        #endif
        #ifdef ENOENT
        if (errcode == ENOENT) {
          errormsg->name = "ENOENT";
          errormsg->msg = GETTEXTL("No such file or directory");
        }
        #endif
        #ifdef ESRCH
        if (errcode == ESRCH) {
          errormsg->name = "ESRCH";
          errormsg->msg = GETTEXTL("No such process");
        }
        #endif
        #ifdef EINTR
        if (errcode == EINTR) {
          errormsg->name = "EINTR";
          errormsg->msg = GETTEXTL("Interrupted system call");
        }
        #endif
        #ifdef EIO
        if (errcode == EIO) {
          errormsg->name = "EIO";
          errormsg->msg = GETTEXTL("I/O error");
        }
        #endif
        #ifdef ENXIO
        if (errcode == ENXIO) {
          errormsg->name = "ENXIO";
          errormsg->msg = GETTEXTL("No such device or address");
        }
        #endif
        #ifdef E2BIG
        if (errcode == E2BIG) {
          errormsg->name = "E2BIG";
          errormsg->msg = GETTEXTL("Arg list too long");
        }
        #endif
        #ifdef ENOEXEC
        if (errcode == ENOEXEC) {
          errormsg->name = "ENOEXEC";
          errormsg->msg = GETTEXTL("Exec format error");
        }
        #endif
        #ifdef EBADF
        if (errcode == EBADF) {
          errormsg->name = "EBADF";
          errormsg->msg = GETTEXTL("Bad file number");
        }
        #endif
        #ifdef ECHILD
        if (errcode == ECHILD) {
          errormsg->name = "ECHILD";
          errormsg->msg = GETTEXTL("No child processes");
        }
        #endif
        #ifdef EAGAIN
        if (errcode == EAGAIN) {
          errormsg->name = "EAGAIN";
          errormsg->msg = GETTEXTL("No more processes");
        }
        #endif
        #ifdef ENOMEM
        if (errcode == ENOMEM) {
          errormsg->name = "ENOMEM";
          errormsg->msg = GETTEXTL("Not enough memory");
        }
        #endif
        #ifdef EACCES
        if (errcode == EACCES) {
          errormsg->name = "EACCES";
          errormsg->msg = GETTEXTL("Permission denied");
        }
        #endif
        #ifdef EFAULT
        if (errcode == EFAULT) {
          errormsg->name = "EFAULT";
          errormsg->msg = GETTEXTL("Bad address");
        }
        #endif
        #ifdef ENOTBLK
        if (errcode == ENOTBLK) {
          errormsg->name = "ENOTBLK";
          errormsg->msg = GETTEXTL("Block device required");
        }
        #endif
        #ifdef EBUSY
        if (errcode == EBUSY) {
          errormsg->name = "EBUSY";
          errormsg->msg = GETTEXTL("Device busy");
        }
        #endif
        #ifdef EEXIST
        if (errcode == EEXIST) {
          errormsg->name = "EEXIST";
          errormsg->msg = GETTEXTL("File exists");
        }
        #endif
        #ifdef EXDEV
        if (errcode == EXDEV) {
          errormsg->name = "EXDEV";
          errormsg->msg = GETTEXTL("Cross-device link");
        }
        #endif
        #ifdef ENODEV
        if (errcode == ENODEV) {
          errormsg->name = "ENODEV";
          errormsg->msg = GETTEXTL("No such device");
        }
        #endif
        #ifdef ENOTDIR
        if (errcode == ENOTDIR) {
          errormsg->name = "ENOTDIR";
          errormsg->msg = GETTEXTL("Not a directory");
        }
        #endif
        #ifdef EISDIR
        if (errcode == EISDIR) {
          errormsg->name = "EISDIR";
          errormsg->msg = GETTEXTL("Is a directory");
        }
        #endif
        #ifdef EINVAL
        if (errcode == EINVAL) {
          errormsg->name = "EINVAL";
          errormsg->msg = GETTEXTL("Invalid argument");
        }
        #endif
        #ifdef ENFILE
        if (errcode == ENFILE) {
          errormsg->name = "ENFILE";
          errormsg->msg = GETTEXTL("File table overflow");
        }
        #endif
        #ifdef EMFILE
        if (errcode == EMFILE) {
          errormsg->name = "EMFILE";
          errormsg->msg = GETTEXTL("Too many open files");
        }
        #endif
        #ifdef ENOTTY
        if (errcode == ENOTTY) {
          errormsg->name = "ENOTTY";
          errormsg->msg = GETTEXTL("Inappropriate ioctl for device");
        }
        #endif
        #ifdef ETXTBSY
        if (errcode == ETXTBSY) {
          errormsg->name = "ETXTBSY";
          errormsg->msg = GETTEXTL("Text file busy");
        }
        #endif
        #ifdef EFBIG
        if (errcode == EFBIG) {
          errormsg->name = "EFBIG";
          errormsg->msg = GETTEXTL("File too large");
        }
        #endif
        #ifdef ENOSPC
        if (errcode == ENOSPC) {
          errormsg->name = "ENOSPC";
          errormsg->msg = GETTEXTL("No space left on device");
        }
        #endif
        #ifdef ESPIPE
        if (errcode == ESPIPE) {
          errormsg->name = "ESPIPE";
          errormsg->msg = GETTEXTL("Illegal seek");
        }
        #endif
        #ifdef EROFS
        if (errcode == EROFS) {
          errormsg->name = "EROFS";
          errormsg->msg = GETTEXTL("Read-only file system");
        }
        #endif
        #ifdef EMLINK
        if (errcode == EMLINK) {
          errormsg->name = "EMLINK";
          errormsg->msg = GETTEXTL("Too many links");
        }
        #endif
        #ifdef EPIPE
        if (errcode == EPIPE) {
          errormsg->name = "EPIPE";
          errormsg->msg = GETTEXTL("Broken pipe, child process terminated or socket closed");
          # Note that these "translations" exploit that CLISP only catches
          # SIGPIPEs from subprocesses and sockets. Other pipes lead to a
          # deadly signal and never to this error message.
        }
        #endif
        # Errors in math functions:
        #ifdef EDOM
        if (errcode == EDOM) {
          errormsg->name = "EDOM";
          errormsg->msg = GETTEXTL("Argument out of domain");
        }
        #endif
        #ifdef ERANGE
        if (errcode == ERANGE) {
          errormsg->name = "ERANGE";
          errormsg->msg = GETTEXTL("Result too large");
        }
        #endif
        # Errors related to non-blocking I/O and interrupt I/O:
        #ifdef EWOULDBLOCK
        if (errcode == EWOULDBLOCK) {
          errormsg->name = "EWOULDBLOCK";
          errormsg->msg = GETTEXTL("Operation would block");
        }
        #endif
        #ifdef EINPROGRESS
        if (errcode == EINPROGRESS) {
          errormsg->name = "EINPROGRESS";
          errormsg->msg = GETTEXTL("Operation now in progress");
        }
        #endif
        #ifdef EALREADY
        if (errcode == EALREADY) {
          errormsg->name = "EALREADY";
          errormsg->msg = GETTEXTL("Operation already in progress");
        }
        #endif
        # Other common errors:
        #ifdef ELOOP
        if (errcode == ELOOP) {
          errormsg->name = "ELOOP";
          errormsg->msg = GETTEXTL("Too many levels of symbolic links");
        }
        #endif
        #ifdef ENAMETOOLONG
        if (errcode == ENAMETOOLONG) {
          errormsg->name = "ENAMETOOLONG";
          errormsg->msg = GETTEXTL("File name too long");
        }
        #endif
        #ifdef ENOTEMPTY
        if (errcode == ENOTEMPTY) {
          errormsg->name = "ENOTEMPTY";
          errormsg->msg = GETTEXTL("Directory not empty");
        }
        #endif
        # Errors relating to Network File System (NFS):
        #ifdef ESTALE
        if (errcode == ESTALE) {
          errormsg->name = "ESTALE";
          errormsg->msg = GETTEXTL("Stale NFS file handle");
        }
        #endif
        #ifdef EREMOTE
        if (errcode == EREMOTE) {
          errormsg->name = "EREMOTE";
          errormsg->msg = GETTEXTL("Too many levels of remote in path");
        }
        #endif
        # Errors relating to sockets, IPC and networking:
        #ifdef ENOTSOCK
        if (errcode == ENOTSOCK) {
          errormsg->name = "ENOTSOCK";
          errormsg->msg = GETTEXTL("Socket operation on non-socket");
        }
        #endif
        #ifdef EDESTADDRREQ
        if (errcode == EDESTADDRREQ) {
          errormsg->name = "EDESTADDRREQ";
          errormsg->msg = GETTEXTL("Destination address required");
        }
        #endif
        #ifdef EMSGSIZE
        if (errcode == EMSGSIZE) {
          errormsg->name = "EMSGSIZE";
          errormsg->msg = GETTEXTL("Message too long");
        }
        #endif
        #ifdef EPROTOTYPE
        if (errcode == EPROTOTYPE) {
          errormsg->name = "EPROTOTYPE";
          errormsg->msg = GETTEXTL("Protocol wrong type for socket");
        }
        #endif
        #ifdef ENOPROTOOPT
        if (errcode == ENOPROTOOPT) {
          errormsg->name = "ENOPROTOOPT";
          errormsg->msg = GETTEXTL("Option not supported by protocol");
        }
        #endif
        #ifdef EPROTONOSUPPORT
        if (errcode == EPROTONOSUPPORT) {
          errormsg->name = "EPROTONOSUPPORT";
          errormsg->msg = GETTEXTL("Protocol not supported");
        }
        #endif
        #ifdef ESOCKTNOSUPPORT
        if (errcode == ESOCKTNOSUPPORT) {
          errormsg->name = "ESOCKTNOSUPPORT";
          errormsg->msg = GETTEXTL("Socket type not supported");
        }
        #endif
        #ifdef EOPNOTSUPP
        if (errcode == EOPNOTSUPP) {
          errormsg->name = "EOPNOTSUPP";
          errormsg->msg = GETTEXTL("Operation not supported on socket");
        }
        #endif
        #ifdef EPFNOSUPPORT
        if (errcode == EPFNOSUPPORT) {
          errormsg->name = "EPFNOSUPPORT";
          errormsg->msg = GETTEXTL("Protocol family not supported");
        }
        #endif
        #ifdef EAFNOSUPPORT
        if (errcode == EAFNOSUPPORT) {
          errormsg->name = "EAFNOSUPPORT";
          errormsg->msg = GETTEXTL("Address family not supported by protocol family");
        }
        #endif
        #ifdef EADDRINUSE
        if (errcode == EADDRINUSE) {
          errormsg->name = "EADDRINUSE";
          errormsg->msg = GETTEXTL("Address already in use");
        }
        #endif
        #ifdef EADDRNOTAVAIL
        if (errcode == EADDRNOTAVAIL) {
          errormsg->name = "EADDRNOTAVAIL";
          errormsg->msg = GETTEXTL("Cannot assign requested address");
        }
        #endif
        #ifdef ENETDOWN
        if (errcode == ENETDOWN) {
          errormsg->name = "ENETDOWN";
          errormsg->msg = GETTEXTL("Network is down");
        }
        #endif
        #ifdef ENETUNREACH
        if (errcode == ENETUNREACH) {
          errormsg->name = "ENETUNREACH";
          errormsg->msg = GETTEXTL("Network is unreachable");
        }
        #endif
        #ifdef ENETRESET
        if (errcode == ENETRESET) {
          errormsg->name = "ENETRESET";
          errormsg->msg = GETTEXTL("Network dropped connection on reset");
        }
        #endif
        #ifdef ECONNABORTED
        if (errcode == ECONNABORTED) {
          errormsg->name = "ECONNABORTED";
          errormsg->msg = GETTEXTL("Software caused connection abort");
        }
        #endif
        #ifdef ECONNRESET
        if (errcode == ECONNRESET) {
          errormsg->name = "ECONNRESET";
          errormsg->msg = GETTEXTL("Connection reset by peer");
        }
        #endif
        #ifdef ENOBUFS
        if (errcode == ENOBUFS) {
          errormsg->name = "ENOBUFS";
          errormsg->msg = GETTEXTL("No buffer space available");
        }
        #endif
        #ifdef EISCONN
        if (errcode == EISCONN) {
          errormsg->name = "EISCONN";
          errormsg->msg = GETTEXTL("Socket is already connected");
        }
        #endif
        #ifdef ENOTCONN
        if (errcode == ENOTCONN) {
          errormsg->name = "ENOTCONN";
          errormsg->msg = GETTEXTL("Socket is not connected");
        }
        #endif
        #ifdef ESHUTDOWN
        if (errcode == ESHUTDOWN) {
          errormsg->name = "ESHUTDOWN";
          errormsg->msg = GETTEXTL("Cannot send after socket shutdown");
        }
        #endif
        #ifdef ETOOMANYREFS
        if (errcode == ETOOMANYREFS) {
          errormsg->name = "ETOOMANYREFS";
          errormsg->msg = GETTEXTL("Too many references: cannot splice");
        }
        #endif
        #ifdef ETIMEDOUT
        if (errcode == ETIMEDOUT) {
          errormsg->name = "ETIMEDOUT";
          errormsg->msg = GETTEXTL("Connection timed out");
        }
        #endif
        #ifdef ECONNREFUSED
        if (errcode == ECONNREFUSED) {
          errormsg->name = "ECONNREFUSED";
          errormsg->msg = GETTEXTL("Connection refused");
        }
        #endif
        #if 0
          errormsg->name = "";
          errormsg->msg = GETTEXTL("Remote peer released connection");
        #endif
        #ifdef EHOSTDOWN
        if (errcode == EHOSTDOWN) {
          errormsg->name = "EHOSTDOWN";
          errormsg->msg = GETTEXTL("Host is down");
        }
        #endif
        #ifdef EHOSTUNREACH
        if (errcode == EHOSTUNREACH) {
          errormsg->name = "EHOSTUNREACH";
          errormsg->msg = GETTEXTL("Host is unreachable");
        }
        #endif
        #if 0
          errormsg->name = "";
          errormsg->msg = GETTEXTL("Networking error");
        #endif
        # Quotas:
        #ifdef EPROCLIM
        if (errcode == EPROCLIM) {
          errormsg->name = "EPROCLIM";
          errormsg->msg = GETTEXTL("Too many processes");
        }
        #endif
        #ifdef EUSERS
        if (errcode == EUSERS) {
          errormsg->name = "EUSERS";
          errormsg->msg = GETTEXTL("Too many users");
        }
        #endif
        #ifdef EDQUOT
        if (errcode == EDQUOT) {
          errormsg->name = "EDQUOT";
          errormsg->msg = GETTEXTL("Disk quota exceeded");
        }
        #endif
        # Errors relating to STREAMS:
        #ifdef ENOSTR
        if (errcode == ENOSTR) {
          errormsg->name = "ENOSTR";
          errormsg->msg = GETTEXTL("Not a stream device");
        }
        #endif
        #ifdef ETIME
        if (errcode == ETIME) {
          errormsg->name = "ETIME";
          errormsg->msg = GETTEXTL("Timer expired");
        }
        #endif
        #ifdef ENOSR
        if (errcode == ENOSR) {
          errormsg->name = "ENOSR";
          errormsg->msg = GETTEXTL("Out of stream resources");
        }
        #endif
        #ifdef ENOMSG
        if (errcode == ENOMSG) {
          errormsg->name = "ENOMSG";
          errormsg->msg = GETTEXTL("No message of desired type");
        }
        #endif
        #ifdef EBADMSG
        if (errcode == EBADMSG) {
          errormsg->name = "EBADMSG";
          errormsg->msg = GETTEXTL("Not a data message");
        }
        #endif
        # Errors relating to SystemV IPC:
        #ifdef EIDRM
        if (errcode == EIDRM) {
          errormsg->name = "EIDRM";
          errormsg->msg = GETTEXTL("Identifier removed");
        }
        #endif
        # Errors relating to SystemV record locking:
        #ifdef EDEADLK
        if (errcode == EDEADLK) {
          errormsg->name = "EDEADLK";
          errormsg->msg = GETTEXTL("Resource deadlock would occur");
        }
        #endif
        #ifdef ENOLCK
        if (errcode == ENOLCK) {
          errormsg->name = "ENOLCK";
          errormsg->msg = GETTEXTL("No record locks available");
        }
        #endif
        # Errors for Remote File System (RFS):
        #ifdef ENONET
        if (errcode == ENONET) {
          errormsg->name = "ENONET";
          errormsg->msg = GETTEXTL("Machine is not on the network");
        }
        #endif
        #ifdef EREMOTE
        if (errcode == EREMOTE) {
          errormsg->name = "EREMOTE";
          errormsg->msg = GETTEXTL("Object is remote");
        }
        #endif
        #ifdef ERREMOTE
        if (errcode == ERREMOTE) {
          errormsg->name = "ERREMOTE";
          errormsg->msg = GETTEXTL("Object is remote");
        }
        #endif
        #ifdef ENOLINK
        if (errcode == ENOLINK) {
          errormsg->name = "ENOLINK";
          errormsg->msg = GETTEXTL("Link has been severed");
        }
        #endif
        #ifdef EADV
        if (errcode == EADV) {
          errormsg->name = "EADV";
          errormsg->msg = GETTEXTL("Advertise error");
        }
        #endif
        #ifdef ESRMNT
        if (errcode == ESRMNT) {
          errormsg->name = "ESRMNT";
          errormsg->msg = GETTEXTL("Srmount error");
        }
        #endif
        #ifdef ECOMM
        if (errcode == ECOMM) {
          errormsg->name = "ECOMM";
          errormsg->msg = GETTEXTL("Communication error on send");
        }
        #endif
        #ifdef EPROTO
        if (errcode == EPROTO) {
          errormsg->name = "EPROTO";
          errormsg->msg = GETTEXTL("Protocol error");
        }
        #endif
        #ifdef EMULTIHOP
        if (errcode == EMULTIHOP) {
          errormsg->name = "EMULTIHOP";
          errormsg->msg = GETTEXTL("Multihop attempted");
        }
        #endif
        #ifdef EDOTDOT
        if (errcode == EDOTDOT) {
          errormsg->name = "EDOTDOT";
          errormsg->msg = "";
        }
        #endif
        #ifdef EREMCHG
        if (errcode == EREMCHG) {
          errormsg->name = "EREMCHG";
          errormsg->msg = GETTEXTL("Remote address changed");
        }
        #endif
        # POSIX errors:
        #ifdef ENOSYS
        if (errcode == ENOSYS) {
          errormsg->name = "ENOSYS";
          errormsg->msg = GETTEXTL("Function not implemented");
        }
        #endif
      # If no error message known, default to the system's one.
      if (errormsg->name == NULL)
        errormsg->name = "";
      if (errormsg->msg == NULL || errormsg->msg[0] == '\0') {
        errno = 0;
        errormsg->msg = strerror(errcode);
        if (errno != 0 || errormsg->msg == NULL)
          errormsg->msg = "";
      }
    }

  #ifdef GNU_GETTEXT
    #undef clgettextl
  #endif

  #ifdef GNU_GETTEXT
    # Now translate after calling get_errormsg().
    #define translate(string)  clgettext(string)
  #else
    #define translate(string)  string
  #endif

local void OS_error_internal (uintC errcode)
{
  /* start error message: */
 #ifdef UNIX
  write_errorstring(GETTEXT("UNIX error "));
 #else
  write_errorstring(GETTEXT("POSIX library error "));
 #endif
  /* output errno: */
  write_errorobject(fixnum(errcode));
  /* output the error-specific message: */
  var os_error_t errormsg;
  get_errormsg(errcode,&errormsg);
  errormsg.msg = translate(errormsg.msg);
  if (!(errormsg.name[0] == 0)) { /* known name? */
    write_errorasciz(" (");
    write_errorasciz(errormsg.name);
    write_errorasciz(")");
  }
  if (!(errormsg.msg[0] == 0)) { /* non-empty message? */
    write_errorasciz(": ");
    write_errorasciz(errormsg.msg);
  }
}
nonreturning_function(global, OS_error, (void)) {
  var uintC errcode; /* positive error number */
  end_system_call(); /* just in case */
  begin_system_call();
  errcode = errno;
  errno = 0; /* reset for the next error */
  end_system_call();
  clr_break_sem_4(); /* no UNIX operation may be active */
  begin_error(); /* start error message */
  if (!nullp(STACK_3)) /* *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ? */
    STACK_3 = S(simple_os_error);
  OS_error_internal(errcode);
  end_error(args_end_pointer STACKop 7,true); /* finish error */
  NOTREACHED;
}
#ifdef UNIX
nonreturning_function(global, OS_file_error, (object pathname)) {
  var uintC errcode; /* positive error number */
  begin_system_call();
  errcode = errno;
  errno = 0; /* reset for the next error */
  end_system_call();
  clr_break_sem_4(); /* no UNIX operation may be active */
  pushSTACK(pathname); /* FILE-ERROR slot PATHNAME */
  begin_error(); /* start error message */
  if (!nullp(STACK_3)) /* *ERROR-HANDLER* = NIL, SYS::*USE-CLCS* /= NIL ? */
    STACK_3 = S(simple_file_error);
  OS_error_internal(errcode);
  end_error(args_end_pointer STACKop 7,true); /* finish error */
  NOTREACHED;
}
#endif

  #undef translate
  #ifdef GNU_GETTEXT
    # Now translate after calling get_errormsg().
    #define translate(string)  clgettextl(string)
  #else
    #define translate(string)  string
  #endif

#ifdef UNIX
/* print an error
 > int errorcode: error code (errno)
 > FILE: Filename (with quotation marks) as constant ASCIZ-String
 > LINE: line number */
global void errno_out_low (int errorcode, const char* file, uintL line) {
  fprintf(stderr,"\n[%s:%d] errno = ", file, line);
  var os_error_t errormsg;
  get_errormsg(errorcode,&errormsg);
  errormsg.msg = translate(errormsg.msg);
  if (errormsg.name[0] != 0 || errormsg.msg[0] != 0) {
    if (errormsg.name[0] != 0) /* known name? */
      fprintf(stderr,errormsg.name);
    else
      fprintf(stderr,"%d",errorcode);
    if (errormsg.msg[0] != 0) /* message? */
      fprintf(stderr,": %s",errormsg.msg);
  } else
    fprintf(stderr,"%d",errorcode);
  fprintf(stderr,".\n");
}
#endif
