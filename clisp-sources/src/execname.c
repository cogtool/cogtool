/*
 * Finding the full path of the executable.
 * Bruno Haible 20.12.1994
 * Sam Steingold 2004-2006
 */

/* This assumes that the executable is not removed or renamed while
   running. */

/* file name of the executable */
static char* executable_name = NULL;
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
/* note that up and including win2000, detaching from a process kills it
 <http://article.gmane.org/gmane.os.cygwin/32245>
 <http://article.gmane.org/gmane.os.cygwin/32246>
 <http://article.gmane.org/gmane.os.cygwin/32250> */
#define default_executable_name  "lisp.exe"
#else
#define default_executable_name  "lisp.run"
#endif

/* file descriptor of the executable
 (Only used to verify that we find the correct executable.) */
static int executable_fd = -1;

#if defined(UNIX)
/* maybe_executable(pathname)
 checks whether a given pathname may belong to the executable. */
static int maybe_executable (const char * filename) {
  struct stat statexe;
  struct stat statfile;
  if (access(filename,R_OK|X_OK) < 0)
    return 0/*false*/;
  if (executable_fd < 0)
    return 1/*true*/;
  /* If we already have an executable_fd, check that filename points to
   the same inode. */
  if (fstat(executable_fd,&statexe) < 0)
    return 1/*true*/;
  if (stat(filename,&statfile) < 0)
    return 0/*false*/;
  if (statfile.st_dev
      && statfile.st_dev == statexe.st_dev
      && statfile.st_ino == statexe.st_ino)
    return 1/*true*/;
  return 0/*false*/;
}
#endif

/* return the executable name */
char *get_executable_name (void);
char *get_executable_name (void) { return executable_name; }

/* find_executable(program_name)
 is to be called immediately after the program starts,
 with program_name = argv[0],
 before any chdir() operation and before any setenv("PATH",...).
 It determines the full program path and opens a file descriptor to
 the executable, for later use.
 Return value is 0 if successful, -1 and errno set if not. */
int find_executable (const char * program_name) {
  /* Do not need to execute this more than once. */
  if (executable_name != NULL) return 0;
#if defined(WIN32_NATIVE)
  { /* an illustration that win32 API can be sometimes useful */
    char execname[MAX_PATH];
    if (!GetModuleFileName(NULL,execname,MAX_PATH))
      goto notfound;
    executable_name = (char*)malloc(strlen(execname)+1);
    strcpy(executable_name,execname);
    return 0;  }
#elif defined(UNIX)
 #if defined(UNIX_LINUX) || defined(UNIX_CYGWIN32)
  { /* The executable is accessible as /proc/<pid>/exe. We try this first
   because it is safer: no race condition w.r.t. the file system. It may
   fail, however, if the user has not compiled /proc support into his
   kernel. */
    int fd = open("/proc/self/exe",O_RDONLY,my_open_mask);
    if (fd >= 0)
      executable_fd = fd;
  }
 #endif
  { /* Now we guess the executable's full path. We assume the executable
   has been called via execlp() or execvp() with properly set up argv[0].
   The login(1) convention to add a '-' prefix to argv[0] is not supported. */
    const char * p;
    for (p = program_name; *p; p++)
      if (*p == '/')
        goto has_slash;
  }
  { /* exec searches paths without slashes in the directory list given
       by $PATH. */
    const char * path = getenv("PATH");
    if (!(path==NULL)) {
      const char * p;
      const char * p_next;
      for (p = path; *p; p = p_next) {
        const char * q;
        unsigned long p_len;
        for (q = p; *q; q++) { if (*q == ':') break; }
        p_len = q-p; p_next = (*q=='\0' ? q : q+1);
        { /* We have a path item at p, of length p_len.
             Now concatenate the path item and program_name. */
          char * concat_name =
            (char*) malloc(p_len + strlen(program_name) + 2);
          if (concat_name == NULL) { errno = ENOMEM; goto notfound; }
          if (p_len == 0) {
            /* empty PATH element designates the current directory */
            strcpy(concat_name,program_name);
          } else {
            memcpy(concat_name, p, p_len);
            concat_name[p_len] = '/';
            strcpy(concat_name+p_len+1, program_name);
          }
          if (maybe_executable(concat_name)) {
            /* Assume we have found the executable */
            program_name = concat_name; goto resolve;
          }
          free(concat_name);
        }
      }
    }
    /* Not found in the PATH, assume the current directory. */
  }
 has_slash:
  /* exec treats paths containing slashes as relative to the current
     directory */
  if (maybe_executable(program_name)) {
   resolve:
    /* resolve program_name: */
#  if !defined(MAXPATHLEN)
#   define MAXPATHLEN 1024      /* see unix.d */
#  endif
    executable_name = (char*) malloc(MAXPATHLEN);
    if (executable_name == NULL) { errno = ENOMEM; goto notfound; }
    if (realpath(program_name,executable_name) == NULL) {
      free(executable_name); goto notfound;
    }
#if defined(UNIX_CYGWIN32)
    { /* cygwin does not append ".exe" on its own */
      int len = strlen(executable_name);
      if (!(len > 4 && (executable_name[len-4] == '.') &&
            (executable_name[len-1] == 'e' || executable_name[len-1] == 'E') &&
            (executable_name[len-2] == 'x' || executable_name[len-2] == 'X') &&
            (executable_name[len-3] == 'e' || executable_name[len-3] == 'E')))
        strcat(executable_name,".exe");
    }
#endif
    return 0;
  }
  errno = ENOENT;
#else
  #error "not implemented: find_executable()"
#endif
 notfound:
  executable_name = default_executable_name; return -1;
}
