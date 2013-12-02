# Cross-platform thread support
# Bruno Haible 1997-1999

# =============================================================================
# This part is heavily influenced by the file <X11/Xthreads.h> from X11R6,
# which carries the following copyright:
# -----------------------------------------------------------------------------
# Copyright (c) 1993  X Consortium
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
# X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
# AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
# Except as contained in this notice, the name of the X Consortium shall not be
# used in advertising or otherwise to promote the sale, use or other dealings
# in this Software without prior written authorization from the X Consortium.
# -----------------------------------------------------------------------------

# Upon entry to this file, one of these symbols shall be defined:
# POSIX_THREADS       POSIX.1c            pthread_*
# POSIXOLD_THREADS    POSIX.1c draft 4    pthread_*
# SOLARIS_THREADS     Solaris 2.4, 2.5    thr_*
# C_THREADS           Mach(?), AIX        cthread_*
# WIN32_THREADS       Win32               *Thread

# This file defines the following types:
#   xthread_t         Type of a thread
#   xcondition_t      Type of a condition (wait queue with signalling)
#   xmutex_t          Type of a mutex (mutually exclusive lock)
#   xthread_key_t     Type of a key for accessing (limited) thread-local storage
# and the following functions/macros:
# # Threads in general:
#   # Initialization of the thread subsystem.
#   extern void           xthread_init (void);
#   # Return the current thread.
#   extern xthread_t      xthread_self (void);
#   # Create a new thread.
#   extern int            xthread_create (xthread_t* thread, void* (*startroutine) (void*), void* arg);
#   # Terminate the current thread.
#   extern void           xthread_exit (void* retvalue);
#   # Give other threads a chance to run.
#   extern void           xthread_yield (void);
#   # Compare two threads for identity.
#   extern bool        xthread_equal (xthread_t thread1, xthread_t thread2);
# # Conditions:
#   # Initialize a wait queue.
#   extern int            xcondition_init (xcondition_t* c);
#   # Destroy a wait queue. The wait queue must be empty (noone waiting).
#   extern int            xcondition_destroy (xcondition_t* c);
#   # Release a mutex and put the current thread into the wait queue.
#   # When the wait ends, the mutex is acquired again.
#   extern int            xcondition_wait (xcondition_t* c, xmutex_t* m);
#   # Notify and unblock one thread in the wait queue.
#   extern int            xcondition_signal (xcondition_t* c);
#   # Notify and unblock all threads in the wait queue.
#   extern int            xcondition_broadcast (xcondition_t* c);
# # Mutexes:
#   # Initialize a mutex.
#   extern int            xmutex_init (xmutex_t* m);
#   # Destroy a mutex.
#   extern int            xmutex_destroy (xmutex_t* m);
#   # Lock a mutex.
#   extern int            xmutex_lock (xmutex_t* m);
#   # Unlock a mutex.
#   extern int            xmutex_unlock (xmutex_t* m);
# # Thread-local storage:
#   # (This is probably not useful at all. The number of thread-local storage
#   # words is limited: 512 on Win32, 128 with LinuxThreads. And it's probably
#   # much slower than my current_thread() function.)
#   # Create a word of thread-local storage, and return a key to it.
#   extern int            xthread_key_create (xthread_key_t* key);
#   # Delete a word of thread-local storage.
#   extern int            xthread_key_delete (xthread_key_t key);
#   # Get the value of the thread-local storage word for the current thread.
#   extern void*          xthread_key_get (xthread_key_t key);
#   # Set the value of the thread-local storage word for the current thread.
#   extern void           xthread_key_set (xthread_key_t key, void* value);


#if !(defined(POSIX_THREADS) || defined(POSIXOLD_THREADS) || defined(SOLARIS_THREADS) || defined(C_THREADS) || defined(WIN32_THREADS))
  #error "Define your flavour of multithreading"
#endif

# NOTE: This file is not yet finished. The primary target is POSIX_THREADS.
# For the other targets, the error checking needs to be improved.

# NOTE 2: Some of the macros in this file require gcc.

#if defined(POSIX_THREADS) || defined(POSIXOLD_THREADS)

#include <pthread.h>
#include <sched.h>

typedef pthread_t         xthread_t;
typedef pthread_cond_t    xcondition_t;
typedef pthread_mutex_t   xmutex_t;
typedef pthread_key_t     xthread_key_t;

#define xthread_init()
#define xthread_self()  pthread_self()
#ifdef POSIX_THREADS
#define xthread_create(thread,startroutine,arg)  \
  pthread_create(thread,NULL,startroutine,arg)
#endif
#ifdef POSIXOLD_THREADS
#define xthread_create(thread,startroutine,arg)  \
  pthread_create(thread,pthread_attr_default,startroutine,arg)
#endif
#define xthread_exit(v)  pthread_exit(v)
#define xthread_yield()  do { if (sched_yield() < 0) OS_error(); } while(0)
#define xthread_equal(t1,t2)  pthread_equal(t1,t2)
#define xthread_cancel(t) pthread_cancel(t)

#ifdef POSIX_THREADS
#define xcondition_init(c)  pthread_cond_init(c,NULL)
#endif
#ifdef POSIXOLD_THREADS
#define xcondition_init(c)  pthread_cond_init(c,pthread_condattr_default)
#endif
#define xcondition_destroy(c)  pthread_cond_destroy(c)
#define xcondition_wait(c,m)  pthread_cond_wait(c,m)
#define xcondition_timedwait(c,m,to)  pthread_cond_timedwait(c,m,to)
#define xcondition_signal(c)  pthread_cond_signal(c)
#define xcondition_broadcast(c)  pthread_cond_broadcast(c)

#ifdef POSIX_THREADS
#define xmutex_init(m)  pthread_mutex_init(m,NULL)
#endif
#ifdef POSIXOLD_THREADS
#define xmutex_init(m)  pthread_mutex_init(m,pthread_mutexattr_default)
#endif
#define xmutex_destroy(m)  pthread_mutex_destroy(m)
#define xmutex_lock(m)  pthread_mutex_lock(m)
#define xmutex_unlock(m)  pthread_mutex_unlock(m)

#ifdef POSIX_THREADS
#define xthread_key_create(key)  pthread_key_create(key,NULL)
#define xthread_key_delete(key)  pthread_key_delete(key)
#define xthread_key_get(key)  pthread_getspecific(key)
#endif
#ifdef POSIXOLD_THREADS
#define xthread_key_create(key)  pthread_keycreate(key,NULL)
#define xthread_key_delete(key)  0
#define xthread_key_get(key)  \
  ({ void* _tmp; pthread_getspecific(key,&_tmp); _tmp; })
#endif
#define xthread_key_set(key,val)  pthread_setspecific(key,val)

#endif # POSIX*_THREADS


#if defined(SOLARIS_THREADS)

#include <thread.h>
#include <synch.h>

typedef clisp_thread_t    xthread_t;
typedef cond_t            xcondition_t;
typedef mutex_t           xmutex_t;
typedef thread_key_t      xthread_key_t;

#define xthread_init()
#define xthread_self()  thr_self()
#define xthread_create(thread,startroutine,arg) thr_create(NULL,0,startroutine,arg,THR_NEW_LWP|THR_DETACHED,thread)
#define xthread_exit(v)  thr_exit(v)
#define xthread_yield()  thr_yield()
#define xthread_equal(t1,t2)  ((t1)==(t2))

#define xcondition_init(c)  cond_init(c,USYNC_THREAD,0)
#define xcondition_destroy(c)  cond_destroy(c)
#define xcondition_wait(c,m)  cond_wait(c,m)
#define xcondition_signal(c)  cond_signal(c)
#define xcondition_broadcast(c)  cond_broadcast(c)

#define xmutex_init(m)  mutex_init(m,USYNC_THREAD,0)
#define xmutex_destroy(m)  mutex_destroy(m)
#define xmutex_lock(m)  mutex_lock(m)
#define xmutex_unlock(m)  mutex_unlock(m)

#define xthread_key_create(key)  thr_keycreate(key,NULL)
#define xthread_key_delete(key)  0
#define xthread_key_get(key)  \
  ({ void* _tmp; thr_getspecific(key,&_tmp); _tmp; })
#define xthread_key_set(key,val)  thr_setspecific(key,val)

#endif # SOLARIS_THREADS


#if defined(C_THREADS)

#include <cthreads.h>

typedef cthread_t         xthread_t;
typedef struct condition  xcondition_t;
typedef struct mutex      xmutex_t;
# not available:          xthread_key_t;

#define xthread_init()  cthread_init()
#define xthread_self()  cthread_self()
#define xthread_create(thread,startroutine,arg)  *thread = ?? cthread_fork(startroutine,arg)
#define xthread_exit(v)  cthread_exit(v)
#define xthread_yield()  cthread_yield()
#define xthread_equal(t1,t2)  ((t1)==(t2))

#define xcondition_init(c)  condition_init(c)
#define xcondition_destroy(c)  condition_clear(c)
#define xcondition_wait(c,m)  condition_wait(c,m)
#define xcondition_signal(c)  condition_signal(c)
#define xcondition_broadcast(c)  condition_broadcast(c)

#define xmutex_init(m)  mutex_init(m)
#define xmutex_destroy(m)  mutex_clear(m)
#define xmutex_lock(m)  mutex_lock(m)
#define xmutex_unlock(m)  mutex_unlock(m)

#endif # C_THREADS


#if defined(WIN32_THREADS)

# include <windows.h>  # already included by win32.d

typedef DWORD              xthread_t;
struct _xthread_waiter {
  HANDLE sem;
  struct _xthread_waiter * next;
};
struct _xcondition {
  CRITICAL_SECTION cs;
  struct _xthread_waiter * waiters;
};
typedef struct _xcondition xcondition_t;
typedef CRITICAL_SECTION   xmutex_t;
typedef DWORD              xthread_key_t;

#define xthread_init()
#define xthread_self()  GetCurrentThreadId()
#define xthread_create(thread,startroutine,arg)  \
  CreateThread(NULL,0,(LPTHREAD_START_ROUTINE)startroutine,(LPVOID)arg,0,thread)
#define xthread_exit(v)  ExitThread((DWORD)(v))
#define xthread_yield()  Sleep(0)??
#define xthread_equal(t1,t2)  ((t1)==(t2))

#define xcondition_init(c)                                              \
  do { InitializeCriticalSection(&(c)->cs); (c)->waiters = NULL; } while(0)
#define xcondition_destroy(c)                   \
  DeleteCriticalSection(&(c)->cs);
#define xcondition_wait(c,m)                                        \
  do { struct _xthread_waiter self_waiting;                         \
    InitializeSemaphore(self_waiting.sem,??);                       \
    EnterCriticalSection(&(c)->cs);                                 \
    self_waiting.next = (c)->waiters; (c)->waiters = &self_waiting; \
    LeaveCriticalSection(&(c)->cs);                                 \
    LeaveCriticalSection(m);                                        \
    WaitForSingleObject(self_waiting.sem,INFINITE);                 \
    EnterCriticalSection(m);                                        \
    DeleteSemaphore(self_waiting.sem,??);                           \
  } while(0)
#define xcondition_signal(c)                                           \
  do { EnterCriticalSection(&(c)->cs);                                 \
    if ((c)->waiters != NULL) {                                        \
      ReleaseSemaphore((c)->waiters->sem,1,NULL);                      \
      (c)->waiters = (c)->waiters->next;                               \
    }                                                                  \
    LeaveCriticalSection(&(c)->cs);                                    \
  } while(0)
#define xcondition_broadcast(c)                                        \
  do { EnterCriticalSection(&(c)->cs);                                 \
    while ((c)->waiters != NULL) {                                     \
      ReleaseSemaphore((c)->waiters->sem,1,NULL);                      \
      (c)->waiters = (c)->waiters->next;                               \
    }                                                                  \
    LeaveCriticalSection(&(c)->cs);                                    \
  } while(0)

#define xmutex_init(m) (InitializeCriticalSection(m),GetLastError())
#define xmutex_destroy(m)  (DeleteCriticalSection(m),GetLastError())
#define xmutex_lock(m)      (EnterCriticalSection(m),GetLastError())
#define xmutex_unlock(m)    (LeaveCriticalSection(m),GetLastError())

#define xthread_key_create(key)  (*(key) = TlsAlloc())
#define xthread_key_delete(key)  TlsFree(key)
#define xthread_key_get(key)  TlsGetValue(key)
#define xthread_key_set(key,val)  TlsSetValue(key,val)

#endif # WIN32_THREADS


# =============================================================================

# Spin-locks.
# This is the most elementary kind of locks.
# Acquiring and releasing of a spin-lock can be considered an atomic operation.
# Differences between spin-locks and mutexes:
# - Spin-locks have to be locked only for a short time; no blocking system
#   calls must be performed with a spin-lock held; no other locks can be
#   acquired while a spin-lock is held.
# - Therefore spin-locks can be assumed to be unlocked "soon", without any
#   particular action to be performed. When trying to acquire a spin-lock which
#   is currently locked, all you can do is sit down and spin ("Däumchen drehen"
#   in German).
# - Acquiring a lock which is previously unlocked, and releasing a lock are
#   fast operations.

#if defined(GNU) && (defined(MC680X0) || defined(SPARC) || defined(MIPS) || defined(I80386) || defined(DECALPHA))

  typedef int spinlock_t;  # A value 0 means unlocked, != 0 means locked.

  # The following atomic operations are borrowed from LinuxThreads-0.6
  # and were mostly written by Richard Henderson <rth@tamu.edu>.

  # testandset(spinlock) tries to acquire the spinlock. It returns
  # 0 if it succeeded (i.e. the old value was 0, the new one is != 0).
  # It returns != 0 if it failed (i.e. the old value was != 0).

  extern inline void spinlock_init (int* spinlock)
  { *spinlock = 0; }
  #ifdef MC680X0
    extern inline int testandset (int* spinlock)
    { char ret;
      __asm__ __volatile__("tas %1; sne %0"
                           : "=g" (ret), "=m" (*spinlock)
                           : "1" (*spinlock)
                           : "cc"
                          );
      return ret;
    }
    extern inline void spinlock_release (int* spinlock)
    { *spinlock = 0; }
  #endif
  #ifdef SPARC
    extern inline int testandset (int* spinlock)
    { int ret;
      __asm__ __volatile__("ldstub %1,%0"
                           : "=r" (ret), "=m" (*spinlock)
                           : "1" (*spinlock)
                          );
      return ret;
    }
    extern inline void spinlock_release (int* spinlock)
    { __asm__ __volatile__("stbar; stb %1,%0"
                           : "=m" (*spinlock)
                           : "r" (0)
                          );
    }
  #endif
  #ifdef MIPS
    extern inline long testandset (int* spinlock)
    { long ret;
      long temp;
      __asm__ __volatile__("#Inline spinlock test & set"
                   "\n\t"  ".set mips2"
                   "\n" "1: ll %0,%2"
                   "\n\t"  "bnez %0,2f"
                   "\n\t"  ".set noreorder"
                   "\n\t"  "li %1,1"
                   "\n\t"  ".set reorder"
                   "\n\t"  "sc %1,%2"
                   "\n\t"  "beqz %1,1b"
                   "\n" "2: .set mips0"
                   "\n\t"  "#End spinlock test & set"
                           : "=&r" (ret), "=&r" (temp), "=m" (*spinlock)
                           : "2" (*spinlock)
                           : "memory"
                          );
      return ret;
    }
    extern inline void spinlock_release (int* spinlock)
    { *spinlock = 0; }
  #endif
  #ifdef I80386
    extern inline long testandset (int* spinlock)
    { int ret;
      __asm__ __volatile__("xchgl %0,%1"
                           : "=&r" (ret), "=m" (*spinlock)
                           : "0" (1), "1" (*spinlock)
                          );
      return ret;
    }
    extern inline void spinlock_release (int* spinlock)
    { *spinlock = 0; }
  #endif
  #ifdef DECALPHA
    extern inline long testandset (int* spinlock)
    { long ret;
      long temp;
      __asm__ __volatile__("/* Inline spinlock test & set */"
                   "\n" "1: ldl_l %0,%2"
                   "\n\t"  "bne %0,2f"
                   "\n\t"  "or $31,1,%1"
                   "\n\t"  "stl_c %1,%2"
                   "\n\t"  "beq %1,1b"
                   "\n" "2: mb"
                   "\n\t"  "/* End spinlock test & set */"
                           : "=&r" (ret), "=&r" (temp), "=m" (*spinlock)
                           : "2" (*spinlock)
                           : "memory"
                          );
      return ret;
    }
    extern inline void spinlock_release (int* spinlock)
    { __asm__ __volatile__("mb" : : : "memory"); *spinlock = 0; }
  #endif
  extern inline void spinlock_acquire (int* spinlock)
  { while (testandset(spinlock)) { xthread_yield(); } }
  extern inline void spinlock_destroy (int* spinlock)
  { unused spinlock; }

#elif defined(GNU) && defined(HPPA)

  # This is borrowed from glibc-2.0.4.

  typedef int spinlock_t __attribute__((__aligned__(16)));
  # A value -1 means unlocked, 0 means locked.

  # testandset(spinlock) tries to acquire the spinlock. It returns
  # 0 if it succeeded (i.e. the old value was -1, the new one is 0).
  # It returns != 0 if it failed (i.e. the old value was 0).

  extern inline void spinlock_init (int* spinlock)
  { *spinlock = -1; }
  extern inline int testandset (int* spinlock)
  { int ret;
    __asm__ __volatile__("ldcws %0,%1"
                         : "=m" (*spinlock), "=r" (ret)
                         : "m" (*spinlock)
                        );
    return ret;
  }
  extern inline void spinlock_acquire (int* spinlock)
  { while (testandset(spinlock)) { xthread_yield(); } }
  extern inline void spinlock_release (int* spinlock)
  { *spinlock = -1; }
  extern inline void spinlock_destroy (int* spinlock)
  { unused spinlock; }

#else

  # Slow, but portable.

  typedef xmutex_t spinlock_t;

  extern inline void spinlock_init (spinlock_t* spinlock)
  { int err = xmutex_init(spinlock); if (err) OS_error(); }
  extern inline void spinlock_acquire (spinlock_t* spinlock)
  { int err = xmutex_lock(spinlock); if (err) OS_error(); }
  extern inline void spinlock_release (spinlock_t* spinlock)
  { int err = xmutex_unlock(spinlock); if (err) OS_error(); }
  extern inline void spinlock_destroy (spinlock_t* spinlock)
  { int err = xmutex_destroy(spinlock); if (err) OS_error(); }

#endif


# =============================================================================
