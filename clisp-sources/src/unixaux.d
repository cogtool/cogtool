/*
 * Auxiliary functions for CLISP on UNIX
 * Bruno Haible 1990-2004
 * Sam Steingold 1998-2005
 */

#include "lispbibl.c"

/* ======================================================================== */

#ifdef NEED_OWN_UALARM
/* an emulation of ualarm(3). */
global unsigned int ualarm (unsigned int value, unsigned int interval) {
  var struct itimerval itimer;
  itimer.it_value.tv_sec = floor(value,1000000);
  itimer.it_value.tv_usec = value % 1000000;
  itimer.it_interval.tv_sec = floor(interval,1000000);
  itimer.it_interval.tv_usec = interval % 1000000;
  setitimer(ITIMER_REAL,&itimer,NULL);
  return 0;                     /* ignore the return value */
}
#endif

/* ======================================================================== */

#ifdef NEED_OWN_SELECT
/* an emulation of select(3). */
global int select (int width, fd_set* readfds, fd_set* writefds,
                   fd_set* exceptfds, struct timeval * timeout) {
  var struct pollfd pollfd_bag[FD_SETSIZE];
  var struct pollfd * pollfd_ptr = &pollfd_bag[0];
  var int pollfd_count = 0;
  if (width<0) {
    errno = EINVAL; return -1;
  }
  if (width>FD_SETSIZE)
    width = FD_SETSIZE;
  {
    var int fd;
    for (fd=0; fd<width; fd++) {
      var short events = 0;
      if (!(readfds==NULL) && FD_ISSET(fd,readfds))
        events |= POLLIN;
      if (!(writefds==NULL) && FD_ISSET(fd,writefds))
          events |= POLLOUT;
      if (!(exceptfds==NULL) && FD_ISSET(fd,exceptfds))
        events |= POLLPRI;
      if (events) {
        pollfd_ptr->fd = fd;
        pollfd_ptr->events = events;
        pollfd_ptr->revents = 0;
        pollfd_ptr++; pollfd_count++;
      }
    }
  }
  var int poll_timeout = timeout->tv_sec * 1000 + timeout->tv_usec / (1000000/1000);
  var int result = poll(&pollfd_bag[0],pollfd_count,poll_timeout);
  if (result>=0) {
    pollfd_ptr = &pollfd_bag[0];
    while (pollfd_count != 0) {
      var int fd = pollfd_ptr->fd;
      var short revents = pollfd_ptr->revents;
      if (!(readfds==NULL) && (revents & POLLIN))
        FD_SET(fd,readfds);
      if (!(writefds==NULL) && (revents & POLLOUT))
        FD_SET(fd,writefds);
      if (!(exceptfds==NULL) && (revents & (POLLPRI|POLLERR|POLLHUP)))
        FD_SET(fd,exceptfds);
      pollfd_ptr++; pollfd_count--;
    }
  }
  return result;
}
#endif

/* ======================================================================== */

#ifdef NEED_OWN_GETTIMEOFDAY
/* an emulation of gettimeofday(3). */
global int gettimeofday (struct timeval * tp, struct timezone * tzp) {
  if ((tp != NULL) || (tzp != NULL)) {
    var struct timeb timebuf;
    ftime(&timebuf);
    if (tp != NULL) {
      tp->tv_sec = timebuf.time;
      tp->tv_usec = (long)(timebuf.millitm) * (1000000/1000);
    }
    if (tzp != NULL) {
      tzp->tz_minuteswest = timebuf.timezone;
      tzp->tz_dsttime = 0;      /* ?? */
    }
  }
  return 0;
}
#endif

/* ======================================================================== */

#ifdef EINTR

#ifdef UNIX

/* a wrapper for open(). */
global int nonintr_open (const char* path, int flags, mode_t mode)
{
  var int retval;
  do {
    retval = open(path,flags,mode);
  } while ((retval < 0) && (errno == EINTR));
  return retval;
}

/* a wrapper for close(). */
global int nonintr_close (int fd) {
  var int retval;
  do {
    retval = close(fd);
  } while ((retval < 0) && (errno == EINTR));
  return retval;
}

/* a wrapper for ioctl(). */
#undef ioctl
global int nonintr_ioctl (int fd, IOCTL_REQUEST_T request, IOCTL_ARGUMENT_T arg) {
  var int retval;
  do {
    retval = ioctl(fd,request,arg);
  } while ((retval != 0) && (errno == EINTR));
  return retval;
}

#endif

#ifdef UNIX_TERM_TERMIOS

/* a wrapper for tcsetattr(). */
global int nonintr_tcsetattr (int fd, int optional_actions, struct termios * tp) {
  var int retval;
  do {
    retval = tcsetattr(fd,optional_actions,tp);
  } while ((retval != 0) && (errno == EINTR));
  return retval;
}

/* a wrapper for tcdrain(). */
global int nonintr_tcdrain (int fd) {
  var int retval;
  do {
    retval = tcdrain(fd);
    } while ((retval != 0) && (errno == EINTR));
  return retval;
}

/* a wrapper for tcflush(). */
global int nonintr_tcflush (int fd, int flag) {
  var int retval;
  do {
    retval = tcflush(fd,flag);
  } while ((retval != 0) && (errno == EINTR));
  return retval;
}

#endif

#ifdef NEED_OWN_SIGINTERRUPT

/* an emulation of siginterrupt(3). */
global int siginterrupt (int sig, int flag);
#if defined(HAVE_SIGACTION)
extern_C int sigaction (/* int sig, [const] struct sigaction * new, struct sigaction * old */);
#elif defined(HAVE_SIGVEC) && defined(SV_INTERRUPT)
extern_C int sigvec (/* int sig, [const] struct sigvec * new, struct sigvec * old */);
#endif
global int siginterrupt (int sig, int flag) {
 #if defined(HAVE_SIGACTION)
  var struct sigaction sa;
  sigaction(sig,(struct sigaction *)NULL,&sa);
 #ifdef SA_INTERRUPT
  if (flag) {
    if (sa.sa_flags & SA_INTERRUPT)
      return 0;
    sa.sa_flags |= SA_INTERRUPT; /* system calls will be interrupted */
  } else {
    if (!(sa.sa_flags & SA_INTERRUPT))
      return 0;
    sa.sa_flags &= ~ SA_INTERRUPT; /* system calls will be restarted */
  }
 #endif
 #ifdef SA_RESTART
  if (flag) {
    if (!(sa.sa_flags & SA_RESTART))
      return 0;
    sa.sa_flags &= ~ SA_RESTART; /* system calls will be interrupted */
  } else {
    if (sa.sa_flags & SA_RESTART)
      return 0;
    sa.sa_flags |= SA_RESTART;  /* system calls will be restarted */
  }
 #endif
  sigaction(sig,&sa,(struct sigaction *)NULL);
 #elif defined(HAVE_SIGVEC) && defined(SV_INTERRUPT)
  var struct sigvec sv;
  sigvec(sig,(struct sigvec *)NULL,&sv);
  if (flag) {
    if (sv.sv_flags & SV_INTERRUPT)
      return 0;
    sv.sv_flags |= SV_INTERRUPT; /* system calls will be interrupted */
  } else {
    if (!(sv.sv_flags & SV_INTERRUPT))
      return 0;
    sv.sv_flags &= ~ SV_INTERRUPT; /* system calls will be restarted */
  }
  sigvec(sig,&sv,(struct sigvec *)NULL);
 #endif
  return 0;                    /* the return value is always ignored. */
}

#endif

#endif

/* Determines whether read() on a file descriptor will hang.
   Returns 1 for yes, 0 for no, -1 for unknown. */
local inline int fd_read_will_hang_p (int fd)
{
  #if defined(HAVE_POLL) && (defined(HAVE_RELIABLE_POLL) || !defined(HAVE_RELIABLE_SELECT))
    var struct pollfd pollfd_bag[1];
    pollfd_bag[0].fd = fd;
    pollfd_bag[0].events = POLLIN;
    pollfd_bag[0].revents = 0;
   restart_poll:
    var int result = poll(&pollfd_bag[0],1,0);
    if (result<0) {
      if (errno==EINTR)
        goto restart_poll;
      OS_error();
    } else {
      # revents has POLLIN or some other bits set if read() would return
      # without blocking.
      if (pollfd_bag[0].revents == 0)
        #ifdef HAVE_RELIABLE_POLL
        return 1;
        #else
        return -1;
        #endif
    }
    # Now we know that read() will return immediately.
    return 0;
  #elif defined(HAVE_SELECT) && !defined(UNIX_BEOS)
    # Use select() with readfds = singleton set {fd}
    # and timeout = zero interval.
    var fd_set handle_set; # set of handles := {fd}
    var struct timeval zero_time; # time interval := 0
    FD_ZERO(&handle_set); FD_SET(fd,&handle_set);
   restart_select:
    zero_time.tv_sec = 0; zero_time.tv_usec = 0;
    var int result = select(FD_SETSIZE,&handle_set,NULL,NULL,&zero_time);
    if (result<0) {
      if (errno==EINTR)
        goto restart_select;
      if (!(errno==EBADF)) { OS_error(); } # UNIX_LINUX returns EBADF for files!
    } else {
      # result = number of handles in handle_set for which read() would
      # return without blocking.
      if (result==0)
        #ifdef HAVE_RELIABLE_SELECT
        return 1;
        #else
        return -1;
        #endif
    }
    # Now we know that read() will return immediately.
    return 0;
  #else
    return -1;
  #endif
}

/* A wrapper around read() that supports different perseverances.
   Return value like read().
   When the return value is 0, it sets errno to indicate whether EOF has been
   seen (ENOENT) or whether it is not yet known (EAGAIN). */
global ssize_t fd_read (int fd, void* bufarea, size_t nbyte, perseverance_t persev)
{
  var char* buf = (char*) bufarea;
  if (nbyte == 0) {
    errno = EAGAIN;
    return 0;
  }
 #if defined(GENERATIONAL_GC) && defined(SPVW_MIXED)
  /* Must adjust the memory permissions before calling read().
   - On SunOS4 a missing write permission causes the read() call to hang
     in an endless loop.
   - With Linux 2.2 the read call returns with errno=EFAULT, but with
     unpredictable side effects: If fd refers to a socket, some of
     the socket data gets lost.
   The SunOS4 behaviour is clearly a bug, but the Linux 2.2 behaviour is
   not. The POSIX spec says that read() returns with errno=EFAULT, but
   does not specify anything about possible side effects. */
  handle_fault_range(PROT_READ_WRITE,(aint)buf,(aint)buf+nbyte);
 #endif
  if (persev == persev_immediate || persev == persev_bonus) {
    int will_hang = fd_read_will_hang_p(fd);
    if (will_hang > 0) {
      errno = EAGAIN;
      return 0;
    }
    if (will_hang < 0) {
      #if (defined(HAVE_POLL) && defined(HAVE_RELIABLE_POLL)) || (defined(HAVE_SELECT) && !defined(UNIX_BEOS) && defined(HAVE_RELIABLE_SELECT))
        NOTREACHED;
      #else
        if (persev == persev_bonus) {
          # Non-blocking I/O is not worth it unless absolutely necessary.
          errno = EAGAIN;
          return 0;
        }
        # As a last resort, use non-blocking I/O.
        var ssize_t done = 0;
        NO_BLOCK_DECL(fd);
        START_NO_BLOCK(fd);
        do {
          var ssize_t retval = read(fd,buf,nbyte);
          if (retval == 0) {
            errno = EAGAIN;
            break;
          } else if (retval < 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
              errno = EAGAIN;
              break;
            }
           #ifdef EINTR
            if (errno != EINTR)
           #endif
              {
                done = retval; /* -1 */
                break;
              }
          } else {
            buf += retval; done += (size_t)retval; nbyte -= (size_t)retval;
            break;
          }
        } while (nbyte != 0);
        var int saved_errno = errno;
        END_NO_BLOCK(fd);
        errno = saved_errno;
        return done;
      #endif
    }
  }
  var ssize_t done = 0;
  do {
    var ssize_t retval = read(fd,buf,nbyte);
    if (retval == 0) {
      errno = ENOENT;
      break;
    } else if (retval < 0) {
     #ifdef EINTR
      if (errno != EINTR)
     #endif
        return retval; /* -1 */
    } else {
      buf += retval; done += (size_t)retval; nbyte -= (size_t)retval;
      if (persev != persev_full)
        break;
    }
  } while (nbyte != 0);
  return done;
}

/* Determines whether write() on a file descriptor will hang.
   Returns 1 for yes, 0 for no, -1 for unknown. */
local inline int fd_write_will_hang_p (int fd)
{
  #if defined(HAVE_POLL) && (defined(HAVE_RELIABLE_POLL) || !defined(HAVE_RELIABLE_SELECT))
    var struct pollfd pollfd_bag[1];
    pollfd_bag[0].fd = fd;
    pollfd_bag[0].events = POLLOUT;
    pollfd_bag[0].revents = 0;
   restart_poll:
    var int result = poll(&pollfd_bag[0],1,0);
    if (result<0) {
      if (errno==EINTR)
        goto restart_poll;
      OS_error();
    } else {
      # revents has POLLOUT or some other bits set if write() would return
      # without blocking.
      if (pollfd_bag[0].revents == 0)
        #ifdef HAVE_RELIABLE_POLL
        return 1;
        #else
        return -1;
        #endif
    }
    # Now we know that write() will return immediately.
    return 0;
  #elif defined(HAVE_SELECT) && !defined(UNIX_BEOS)
    # Use select() with writefds = singleton set {fd}
    # and timeout = zero interval.
    var fd_set handle_set; # set of handles := {fd}
    var struct timeval zero_time; # time interval := 0
    FD_ZERO(&handle_set); FD_SET(fd,&handle_set);
   restart_select:
    zero_time.tv_sec = 0; zero_time.tv_usec = 0;
    var int result = select(FD_SETSIZE,NULL,&handle_set,NULL,&zero_time);
    if (result<0) {
      if (errno==EINTR)
        goto restart_select;
      if (!(errno==EBADF)) { OS_error(); } # UNIX_LINUX returns EBADF for files!
    } else {
      # result = number of handles in handle_set for which write() would
      # return without blocking.
      if (result==0)
        #ifdef HAVE_RELIABLE_SELECT
        return 1;
        #else
        return -1;
        #endif
    }
    # Now we know that write() will return immediately.
    return 0;
  #else
    return -1;
  #endif
}

/* A wrapper around write() that supports different perseverances.
   Return value like write().
   When the return value is 0, it sets errno to indicate whether EOWF has been
   seen (ENOENT) or whether it is not yet known (EAGAIN). */
global ssize_t fd_write (int fd, const void* bufarea, size_t nbyte, perseverance_t persev)
{
  var const char* buf = (const char*) bufarea;
  if (nbyte == 0) {
    errno = EAGAIN;
    return 0;
  }
 #if defined(GENERATIONAL_GC) && defined(SPVW_MIXED)
  /* Must adjust the memory permissions before calling write(). */
  handle_fault_range(PROT_READ,(aint)buf,(aint)buf+nbyte);
 #endif
  if (persev == persev_immediate || persev == persev_bonus) {
    int will_hang = fd_write_will_hang_p(fd);
    if (will_hang > 0) {
      errno = EAGAIN;
      return 0;
    }
    if (will_hang < 0) {
      #if (defined(HAVE_POLL) && defined(HAVE_RELIABLE_POLL)) || (defined(HAVE_SELECT) && !defined(UNIX_BEOS) && defined(HAVE_RELIABLE_SELECT))
        NOTREACHED;
      #else
        if (persev == persev_bonus) {
          # Non-blocking I/O is not worth it unless absolutely necessary.
          errno = EAGAIN;
          return 0;
        }
        # As a last resort, use non-blocking I/O.
        var ssize_t done = 0;
        NO_BLOCK_DECL(fd);
        START_NO_BLOCK(fd);
        do {
          var ssize_t retval = write(fd,buf,nbyte);
          if (retval == 0) {
            errno = EAGAIN;
            break;
          } else if (retval < 0) {
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
              errno = EAGAIN;
              break;
            }
           #ifdef EINTR
            if (errno != EINTR)
           #endif
              {
                done = retval; /* -1 */
                break;
              }
          } else {
            buf += retval; done += (size_t)retval; nbyte -= (size_t)retval;
            break;
          }
        } while (nbyte != 0);
        var int saved_errno = errno;
        END_NO_BLOCK(fd);
        errno = saved_errno;
        return done;
      #endif
    }
  }
  var ssize_t done = 0;
  do {
    var ssize_t retval = write(fd,buf,nbyte);
    if (retval == 0) {
      errno = ENOENT;
      break;
    } else if (retval < 0) {
     #ifdef EINTR
      if (errno != EINTR)
     #endif
        return retval; /* -1 */
    } else {
      buf += retval; done += (size_t)retval; nbyte -= (size_t)retval;
      if (persev != persev_full)
        break;
    }
  } while (nbyte != 0);
  return done;
}

#ifdef UNIX_BEOS

/* BeOS 5 sockets cannot be used like file descriptors. */

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
    if (errno==EINTR)
      goto restart_select;
    OS_error();
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
global ssize_t sock_read (int fd, void* bufarea, size_t nbyte, perseverance_t persev) {
  var char* buf = (char*) bufarea;
  if (nbyte == 0) {
    errno = EAGAIN;
    return 0;
  }
 #if defined(GENERATIONAL_GC) && defined(SPVW_MIXED)
  /* Must adjust the memory permissions before calling recv(). */
  handle_fault_range(PROT_READ_WRITE,(aint)buf,(aint)buf+nbyte);
 #endif
  if (persev == persev_immediate || persev == persev_bonus) {
    int will_hang = sock_read_will_hang_p(fd);
    if (will_hang > 0) {
      errno = EAGAIN;
      return 0;
    }
    if (will_hang < 0) {
      #if 1
        NOTREACHED;
      #else
        if (persev == persev_bonus) {
          errno = EAGAIN;
          return 0;
        }
      #endif
    }
  }
  var ssize_t done = 0;
  do {
    var ssize_t retval = recv(fd,buf,nbyte,0);
    if (retval == 0) {
      errno = ENOENT;
      break;
    } else if (retval < 0) {
     #ifdef EINTR
      if (errno != EINTR)
     #endif
        return retval; /* -1 */
    } else {
      buf += retval; done += (size_t)retval; nbyte -= (size_t)retval;
      if (persev != persev_full)
        break;
    }
  } while (nbyte != 0);
  return done;
}

/* Determines whether send() on a socket will hang.
   Returns 1 for yes, 0 for no, -1 for unknown. */
local inline int sock_write_will_hang_p (int fd)
{
  #if 0 /* On BeOS, select() supports only readfds, not writefds. */
    # Use select() with writefds = singleton set {fd}
    # and timeout = zero interval.
    var fd_set handle_set; # set of handles := {fd}
    var struct timeval zero_time; # time interval := 0
    FD_ZERO(&handle_set); FD_SET(fd,&handle_set);
   restart_select:
    zero_time.tv_sec = 0; zero_time.tv_usec = 0;
    var int result = select(FD_SETSIZE,NULL,&handle_set,NULL,&zero_time);
    if (result<0) {
      if (errno==EINTR)
        goto restart_select;
      OS_error();
    } else {
      # result = number of handles in handle_set for which write() would
      # return without blocking.
      if (result==0)
        return 0;
    }
    # Now we know that send() will return immediately.
  #else
    return -1;
  #endif
}

/* A wrapper around send() that supports different perseverances.
   Return value like write().
   When the return value is 0, it sets errno to indicate whether EOWF has been
   seen (ENOENT) or whether it is not yet known (EAGAIN). */
global ssize_t sock_write (int fd, const void* bufarea, size_t nbyte, perseverance_t persev)
{
  var const char* buf = (const char*) bufarea;
  if (nbyte == 0) {
    errno = EAGAIN;
    return 0;
  }
 #if defined(GENERATIONAL_GC) && defined(SPVW_MIXED)
  /* Must adjust the memory permissions before calling send(). */
  handle_fault_range(PROT_READ,(aint)buf,(aint)buf+nbyte);
 #endif
  if (persev == persev_immediate || persev == persev_bonus) {
    int will_hang = sock_write_will_hang_p(fd);
    if (will_hang > 0) {
      errno = EAGAIN;
      return 0;
    }
    if (will_hang < 0) {
      if (persev == persev_bonus) {
        # Non-blocking I/O is not worth it unless absolutely necessary.
        errno = EAGAIN;
        return 0;
      }
      # As a last resort, use non-blocking I/O.
      var ssize_t done = 0;
      NO_BLOCK_DECL(fd);
      START_NO_BLOCK(fd);
      do {
        var ssize_t retval = send(fd,buf,nbyte,0);
        if (retval == 0) {
          errno = EAGAIN;
          break;
        } else if (retval < 0) {
          if (errno == EAGAIN || errno == EWOULDBLOCK) {
            errno = EAGAIN;
            break;
          }
         #ifdef EINTR
          if (errno != EINTR)
         #endif
            {
              done = retval; /* -1 */
              break;
            }
        } else {
          buf += retval; done += (size_t)retval; nbyte -= (size_t)retval;
          break;
        }
      } while (nbyte != 0);
      var int saved_errno = errno;
      END_NO_BLOCK(fd);
      errno = saved_errno;
      return done;
    }
  }
  var ssize_t done = 0;
  do {
    var ssize_t retval = send(fd,buf,nbyte,0);
    if (retval == 0) {
      errno = ENOENT;
      break;
    } else if (retval < 0) {
     #ifdef EINTR
      if (errno != EINTR)
     #endif
        return retval; /* -1 */
    } else {
      buf += retval; done += (size_t)retval; nbyte -= (size_t)retval;
      if (persev != persev_full)
        break;
    }
  } while (nbyte != 0);
  return done;
}

#endif

#ifdef PID_T

/* wait for termination of a child process: */
global int wait2 (PID_T child) {
  var int status = 0;
  /* WAIT(2V) and #include <sys/wait.h> :
     WIFSTOPPED(status)  ==  ((status & 0xFF) == 0177)
     WEXITSTATUS(status)  == ((status & 0xFF00) >> 8) */
  loop {
    var int ergebnis = waitpid(child,&status,0);
    if (ergebnis != child) {
      if (ergebnis<0) {
        if (errno==EINTR)
          continue;
       #ifdef ECHILD
        if (errno==ECHILD) { /* If the Child process is no longer there, */
          status = 0; break;  /* it was probably correctly terminated */
        }
       #endif
      }
      OS_error();
    }
    if (!((status & 0xFF) == 0177)) /* child process terminated? */
      break;
  }
  return status;
}

#endif

/* ======================================================================== */

#if defined(UNIX)

/* This is like the signal() function, except that
 - It uses sigaction() if needed in order to not block other signals,
 - It calls siginterrupt(sig,0) so that these signals avoid to interrupt
   system calls. */
global signal_handler_t install_signal_handler (int sig,
                                                signal_handler_t handler) {
  var signal_handler_t old_handler;
 #if defined(USE_SIGACTION)
  var struct sigaction old_sa;
  var struct sigaction new_sa;
  memset(&new_sa,0,sizeof(new_sa));
  new_sa.sa_handler = handler;
  /* Do not block other signals, except possibly SIGINT and SIGALRM
     (because our SIGINT/SIGALRM handlers expects the STACK_register
     to be valid). */
  sigemptyset(&new_sa.sa_mask);
 #ifdef HAVE_SAVED_STACK
  if (!(sig == SIGINT || sig == SIGALRM)) {
    sigaddset(&new_sa.sa_mask,SIGINT);
    sigaddset(&new_sa.sa_mask,SIGALRM);
  }
 #endif
  /* new_sa.sa_mask = 0; / * Do not block other signals. */
 #ifdef EINTR
  #ifdef SA_RESTART
  new_sa.sa_flags |= SA_RESTART; /* system calls will be restarted */
  #endif
 #endif
  if (sigaction(sig,&new_sa,&old_sa)<0)
    old_handler = (signal_handler_t)SIG_IGN;
  else
    old_handler = (signal_handler_t)old_sa.sa_handler;
 #else
  old_handler = signal(sig,handler);
  #ifdef EINTR
  siginterrupt(sig,0);
  #endif
 #endif
  return old_handler;
}
#endif

/* ======================================================================= */

#if defined(UNIX_CYGWIN32)

/* Prepare for <windows.h>. */
#define ULONG     OS_ULONG
#undef unused

/* ------------------------------------------------------------------------ */

/* The library's abort() function just makes the program exit.
 But I want to see a backtrace! */
int abort_dummy;
global void abort() {
  abort_dummy = 1/0;
}

/* ----------------------------------------------------------------------- */

/* Cygwin internal in <src/winsup/cygwin/times.cc>
 Convert a Win32 time to "UNIX" format.
 used by syscalls and dirkey modules */
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define FACTOR LL(0x19db1ded53ea710)
#define NSPERSEC LL(10000000)
global long time_t_from_filetime (const FILETIME * ptr) {
  /* A file time is the number of 100ns since jan 1 1601
     stuffed into two long words.
     A time_t is the number of seconds since jan 1 1970.  */
  long rem;
  long long x =
    ((long long) ptr->dwHighDateTime << 32) + ((unsigned) ptr->dwLowDateTime);
  /* pass "no time" as epoch */
  if (x == 0) return 0;
  x -= FACTOR;               /* number of 100ns between 1601 and 1970 */
  rem = x % ((long long) NSPERSEC);
  rem += (NSPERSEC/2);
  x /= (long long) NSPERSEC;    /* number of 100ns in a second */
  x += (long long) (rem/NSPERSEC);
  return x;
}
global void time_t_to_filetime (time_t time_in, FILETIME *out)
{
  long long x = time_in * NSPERSEC + FACTOR;
  out->dwHighDateTime = x >> 32;
  out->dwLowDateTime = x;
}
#undef FACTOR
#undef NSPERSEC

#endif

/* ======================================================================== */
/* close all file descriptors before exec()
 this is more reliable than FD_CLOEXEC because there are many places that
 can open file descriptors (Berkeley-DB, rawsock &c) that we do not control */
#define CLISP_OPEN_MAX_FALLBACK  256/* just a guess */
global void close_all_fd (void) {
#if defined(HAVE_SYSCONF) && defined(_SC_OPEN_MAX)
  int fd = sysconf(_SC_OPEN_MAX) - 1;
#elif defined(HAVE_GETDTABLESIZE)
  int fd = getdtablesize() - 1;
#elif defined(HAVE_GETRLIMIT) && defined(RLIMIT_NOFILE)
  int fd;
  struct rlimit rl;
  if (0 == getrlimit(RLIMIT_NOFILE,&rl)) fd = rl.rlim_cur;
  else fd = CLISP_OPEN_MAX_FALLBACK;
#elif defined(OPEN_MAX)
  int fd = OPEN_MAX;
#else
  int fd = CLISP_OPEN_MAX_FALLBACK;
#endif
  while (fd >= 3) close(fd--);
}

/* file identification for check_file_re_open() */
/* if file NAMESTRING exists, fill file_id and call function on it,
   otherwise return NULL */
global void* with_file_id (char * namestring, void *data,
                           void* (*func) (struct file_id *, void *data)) {
  var struct stat st;
  if (stat(namestring,&st)) return NULL;
  var struct file_id fi;
  fi.device = st.st_dev;
  fi.inode = st.st_ino;
  return (*func)(&fi,data);
}

/* fill FI for an existing file handle */
global errno_t handle_file_id (int fd, struct file_id *fi) {
  var struct stat st;
  if (fstat(fd,&st)) return errno;
  fi->device = st.st_dev;
  fi->inode = st.st_ino;
  return 0;
}

/* if the file IDs are identical, return 1, otherwise return 0 */
global int file_id_eq (struct file_id *fi1, struct file_id *fi2)
{ return (fi1->device == fi2->device) && (fi1->inode == fi2->inode); }
