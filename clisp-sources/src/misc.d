/*
 * Miscellaneous CLISP functions
 * Bruno Haible 1990-2005
 * Sam Steingold 1999-2005
 */

#include "lispbibl.c"

# Eigenwissen:

LISPFUN(lisp_implementation_type,seclass_no_se,0,0,norest,nokey,0,NIL)
{ /* (LISP-IMPLEMENTATION-TYPE), CLTL S. 447 */
  VALUES1(O(lisp_implementation_type_string));
}

LISPFUN(lisp_implementation_version,seclass_no_se,0,0,norest,nokey,0,NIL)
{ /* (LISP-IMPLEMENTATION-VERSION), CLTL S. 447 */
  value1 = O(lisp_implementation_version_string);
  if (nullp(value1)) { # noch unbekannt?
    var int count = 1;
    pushSTACK(O(lisp_implementation_package_version));
    funcall(L(machine_instance),0);
    if (nullp(O(memory_image_host)) || equal(value1,O(memory_image_host))) {
      /* the image was dumped on this machine - print time */
      if (!nullp(O(lisp_implementation_version_built_string))) {
        /* this is a gross and ugly hack!
           I have no idea about how to get the executable link time,
           let alone how to do this portably,
           so I rely on __DATE__ and __TIME__ CPP macros. */
        var uintL se,mi,ho,da,mo,ye;
        with_string_0(O(lisp_implementation_version_built_string),
                      O(misc_encoding),ztime,{
          /* Mmm dd yyyyhh:mm:ss
             0   4  7   11 14 17 */
          ztime[13] = ztime[16] = 0;
          se = atol(ztime+17);
          mi = atol(ztime+14);
          ho = atol(ztime+11);
          da = atol(ztime+4);
          mo = (strncmp("Jan",ztime,3) == 0 ? 1 :
                strncmp("Feb",ztime,3) == 0 ? 2 :
                strncmp("Mar",ztime,3) == 0 ? 3 :
                strncmp("Apr",ztime,3) == 0 ? 4 :
                strncmp("May",ztime,3) == 0 ? 5 :
                strncmp("Jun",ztime,3) == 0 ? 6 :
                strncmp("Jul",ztime,3) == 0 ? 7 :
                strncmp("Aug",ztime,3) == 0 ? 8 :
                strncmp("Sep",ztime,3) == 0 ? 9 :
                strncmp("Oct",ztime,3) == 0 ? 10 :
                strncmp("Nov",ztime,3) == 0 ? 11 :
                strncmp("Dec",ztime,3) == 0 ? 12 : 0);
          ztime[11]=0;
          ye = atol(ztime+7);
        });
        /* no month ==> l_i_v_b_s must have been converted already */
        if (mo != 0) { /* YYYY-MM-DD HH:MM:SS */
          var char build_time[4+1+2+1+2 +1+ 2+1+2+1+2+1];
          if (!boundp(Symbol_function(S(encode_universal_time)))) {
            sprintf(build_time,"%04u-%02u-%02u %02u:%02u:%02u",
                    (unsigned int) ye, (unsigned int) mo, (unsigned int) da,
                    (unsigned int) ho, (unsigned int) mi, (unsigned int) se);
          } else {
            pushSTACK(fixnum(se));
            pushSTACK(fixnum(mi));
            pushSTACK(fixnum(ho));
            pushSTACK(fixnum(da));
            pushSTACK(fixnum(mo));
            pushSTACK(fixnum(ye));
            funcall(S(encode_universal_time),6);
            sprintf(build_time,"%u", (unsigned int) I_to_UL(value1));
          }
          O(lisp_implementation_version_built_string) =
            ascii_to_string(build_time);
        }
        pushSTACK(ascii_to_string(" (built "));
        pushSTACK(O(lisp_implementation_version_built_string));
        pushSTACK(ascii_to_string(")"));
        count += 3;
      }
      if (!nullp(O(memory_image_timestamp))) {
        pushSTACK(ascii_to_string(" (memory "));
        pushSTACK(O(memory_image_timestamp));
        pushSTACK(ascii_to_string(")"));
        count += 3;
      }
    } else { /* this image was built on a different machine */
      pushSTACK(ascii_to_string(" (built on "));
      pushSTACK(O(memory_image_host));
      pushSTACK(ascii_to_string(")"));
      count += 3;
    }
    value1 = O(lisp_implementation_version_string) = string_concat(count);
  }
  mv_count=1;
}

LISPFUN(version,seclass_default,0,1,norest,nokey,0,NIL)
# (SYSTEM::VERSION) liefert die Version des Runtime-Systems,
# (SYSTEM::VERSION version) überprüft (am Anfang eines FAS-Files),
# ob die Versionen des Runtime-Systems übereinstimmen.
  {
    var object arg = popSTACK();
    if (!boundp(arg)) {
      VALUES1(O(version));
    } else {
      if (equal(arg,O(version)) /* || equal(arg,O(oldversion)) */) {
        VALUES0;
      } else {
        fehler(error,
               GETTEXT("This file was produced by another lisp version, must be recompiled.")
              );
      }
    }
  }

#ifdef MACHINE_KNOWN

LISPFUNN(machinetype,0)
# (MACHINE-TYPE), CLTL S. 447
  {
    var object erg = O(machine_type_string);
    if (nullp(erg)) { # noch unbekannt?
      # ja -> holen
      #ifdef UNIX
        #ifdef HAVE_SYS_UTSNAME_H
          var struct utsname utsname;
          begin_system_call();
          if ( uname(&utsname) <0) { OS_error(); }
          end_system_call();
          pushSTACK(asciz_to_string(utsname.machine,O(misc_encoding)));
          funcall(L(nstring_upcase),1); # in Großbuchstaben umwandeln
          erg = value1;
        #else
          # Betriebssystem-Kommando 'uname -m' bzw. 'arch' ausführen und
          # dessen Output in einen String umleiten:
          # (string-upcase
          #   (with-open-stream (stream (make-pipe-input-stream "/bin/arch"))
          #     (read-line stream nil nil)
          # ) )
          #if defined(UNIX_SUNOS4)
            pushSTACK(ascii_to_string("/bin/arch"));
          #elif defined(UNIX_NEXTSTEP)
            pushSTACK(ascii_to_string("/usr/bin/arch"));
          #else
            pushSTACK(ascii_to_string("uname -m"));
          #endif
          funcall(L(make_pipe_input_stream),1); # (MAKE-PIPE-INPUT-STREAM "/bin/arch")
          pushSTACK(value1); # Stream retten
          pushSTACK(value1); pushSTACK(NIL); pushSTACK(NIL);
          funcall(L(read_line),3); # (READ-LINE stream NIL NIL)
          pushSTACK(value1); # Ergebnis (kann auch NIL sein) retten
          builtin_stream_close(&STACK_1,0); /* close stream */
          if (!nullp(STACK_0))
            erg = string_upcase(STACK_0); # in Großbuchstaben umwandeln
          else
            erg = NIL;
          skipSTACK(2);
        #endif
      #endif
      #ifdef WIN32_NATIVE
        {
          var SYSTEM_INFO info;
          begin_system_call();
          GetSystemInfo(&info);
          end_system_call();
          if (info.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_INTEL) {
            erg = ascii_to_string("PC/386");
          }
        }
      #endif
      # Das Ergebnis merken wir uns für's nächste Mal:
      O(machine_type_string) = erg;
    }
    VALUES1(erg);
  }

LISPFUNN(machine_version,0)
# (MACHINE-VERSION), CLTL S. 447
  {
    var object erg = O(machine_version_string);
    if (nullp(erg)) { # noch unbekannt?
      # ja -> holen
      #ifdef UNIX
        #ifdef HAVE_SYS_UTSNAME_H
          var struct utsname utsname;
          begin_system_call();
          if ( uname(&utsname) <0) { OS_error(); }
          end_system_call();
          pushSTACK(asciz_to_string(utsname.machine,O(misc_encoding)));
          funcall(L(nstring_upcase),1); # in Großbuchstaben umwandeln
        #else
          # Betriebssystem-Kommando 'uname -m' bzw. 'arch -k' ausführen und
          # dessen Output in einen String umleiten:
          # (string-upcase
          #   (with-open-stream (stream (make-pipe-input-stream "/bin/arch -k"))
          #     (read-line stream nil nil)
          # ) )
          #if defined(UNIX_SUNOS4)
            pushSTACK(ascii_to_string("/bin/arch -k"));
          #else
            pushSTACK(ascii_to_string("uname -m"));
          #endif
          funcall(L(make_pipe_input_stream),1); # (MAKE-PIPE-INPUT-STREAM "/bin/arch -k")
          pushSTACK(value1); # Stream retten
          pushSTACK(value1); pushSTACK(NIL); pushSTACK(NIL);
          funcall(L(read_line),3); # (READ-LINE stream NIL NIL)
          pushSTACK(value1); # Ergebnis (kann auch NIL sein) retten
          builtin_stream_close(&STACK_1,0); /* close stream */
          funcall(L(string_upcase),1); skipSTACK(1); # in Großbuchstaben umwandeln
        #endif
        erg = value1;
      #endif
      #ifdef WIN32_NATIVE
        {
          var SYSTEM_INFO info;
          var OSVERSIONINFO v;
          begin_system_call();
          GetSystemInfo(&info);
          v.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
          if (!GetVersionEx(&v)) { OS_error(); }
          end_system_call();
          if (info.wProcessorArchitecture==PROCESSOR_ARCHITECTURE_INTEL) {
            erg = ascii_to_string("PC/386");
            # Check for Windows NT, since the info.wProcessorLevel is
            # garbage on Windows 95.
            if (v.dwPlatformId == VER_PLATFORM_WIN32_NT)
              TheS8string(erg)->data[3] = '0'+info.wProcessorLevel;
            else {
              if (info.dwProcessorType == PROCESSOR_INTEL_386)
                TheS8string(erg)->data[3] = '3';
              elif (info.dwProcessorType == PROCESSOR_INTEL_486)
                TheS8string(erg)->data[3] = '4';
              elif (info.dwProcessorType == PROCESSOR_INTEL_PENTIUM)
                TheS8string(erg)->data[3] = '5';
            }
          }
        }
      #endif
      # Das Ergebnis merken wir uns für's nächste Mal:
      O(machine_version_string) = erg;
    }
    VALUES1(erg);
  }

#endif # MACHINE_KNOWN

#if defined(HAVE_ENVIRONMENT)
/* declared in <stdlib.h> or <unistd.h> */
#if !HAVE_DECL_ENVIRON
extern_C char** environ;
#endif

/* push the (VAR . VALUE) on the STACK
 can trigger GC */
local inline maygc char* push_envar (char *env) {
  char *ep = env;
  while ((*ep != 0) && (*ep != '=')) ep++;
  pushSTACK(allocate_cons());
  var object var_string = n_char_to_string(env,ep-env,O(misc_encoding));
  Car(STACK_0) = var_string;
  if (*ep == '=') {
    var object val_string = asciz_to_string(ep+1,O(misc_encoding));
    Cdr(STACK_0) = val_string;
  }
  while (*ep != 0) ep++;
  return ep;
}

/* (EXT:GETENV string) return the string associated with the given string
 in the OS Environment or NIL if no value
 if STRING is NIL, return all the environment as an alist */
LISPFUN(get_env,seclass_default,0,1,norest,nokey,0,NIL) {
  var object arg = popSTACK();
  if (missingp(arg)) { /* return all the environment at once */
    var uintL count = 0;
   #if 0 && defined(WIN32_NATIVE)
    /* see below, clisp_setenv(), for references */
    var char* eblock_orig = GetEnvironmentStrings();
    var char* eblock = eblock_orig;
    for (; *eblock; eblock++, count++) eblock = push_envar(eblock);
   #else
    var char** epp;
    for (epp = environ; *epp; epp++, count++) push_envar(*epp);
   #endif
    VALUES1(listof(count));
   #if 0 && defined(WIN32_NATIVE)
    FreeEnvironmentStrings(eblock_orig);
   #endif
    return;
  }
  arg = check_string(arg);
  var const char* found;
  with_string_0(arg,O(misc_encoding),envvar, {
    begin_system_call();
    found = getenv(envvar);
    end_system_call();
  });
  if (found != NULL)
    VALUES1(asciz_to_string(found,O(misc_encoding)));
  else
    VALUES1(NIL);
}

/* Creates a string concatenating an environment variable and its value.
 Like sprintf(buffer, "%s=%s", name, value); */
local char * cat_env_var (char * buffer, const char * name, uintL namelen,
                          const char * value, uintL valuelen) {
  memcpy(buffer,name,namelen);
  if (value == NULL) abort();
  buffer[namelen++] = '=';
  memcpy(buffer+namelen,value,valuelen);
  buffer[namelen+valuelen] = 0;
  return buffer;
}

/* Uh oh, neither putenv() nor setenv(), have to frob the environment
   ourselves. Routine taken from glibc and fixed in several aspects. */
local int setenv_via_environ (const char * name, uintL namelen,
                              const char * value, uintL valuelen) {
  var char** epp;
  var char* ep;
  var uintL envvar_count = 0;
  for (epp = environ; (ep = *epp) != NULL; epp++) {
    var const char * np = name;
    /* Compare *epp and name: */
    while (*np != '\0' && *np == *ep) { np++; ep++; }
    if (*np == '\0' && *ep == '=')
      break;
    envvar_count++;
  }
  ep = *epp;
  if (ep == NULL) {
    if (value != NULL) {
      /* name not found in environ, add it.
         if value is NULL - nothing is to be done!
         Remember the environ, so that we can free it if
         we need to reallocate it again next time. */
      var static char** last_environ = NULL;
      var char** new_environ = (char**) malloc((envvar_count+2)*sizeof(char*));
      if (!new_environ)
        return -1; /* no need to set errno = ENOMEM */
      { /* copy environ */
        var uintL count;
        epp = environ;
        for (count = 0; count < envvar_count; count++)
          new_environ[count] = epp[count];
      }
      ep = (char*) malloc(namelen+1+valuelen+1);
      if (!ep) {
        free(new_environ); return -1; /* no need to set errno = ENOMEM */
      }
      new_environ[envvar_count] = cat_env_var(ep,name,namelen,value,valuelen);
      new_environ[envvar_count+1] = NULL;
      environ = new_environ;
      if (last_environ != NULL)
        free(last_environ);
      last_environ = new_environ;
    }
  } else {
    /* name found, replace its value.
       We could be tempted to overwrite name's value directly if
       the new value is not longer than the old value.
       But that's not a good idea - maybe someone still has a pointer to
       this area around.
       should we free() the old value?! */
    ep = (char*) malloc(namelen+1+valuelen+1);
    if (!ep)
      return -1; /* no need to set errno = ENOMEM */
    if (value == NULL) *epp = NULL;
    else *epp = cat_env_var(ep,name,namelen,value,valuelen);
  }
  return 0;
}

/* Modify the environment variables. putenv() is POSIX, but some BSD systems
 only have setenv(). Therefore (and because it's simpler to use) we
 implement this interface, but without the third argument.
 clisp_setenv(name,value) sets the value of the environment variable `name'
 to `value' and returns 0. Returns -1 if not enough memory. */
global int clisp_setenv (const char * name, const char * value) {
  var uintL namelen = asciz_length(name);
  var uintL valuelen = (value==NULL ? 0 : asciz_length(value));
#if defined(WIN32_NATIVE)
  /* On Woe32, each process has two copies of the environment variables,
     one managed by the OS and one managed by the C library. We set
     the value in both locations, so that other software that looks in
     one place or the other is guaranteed to see the value. Even if it's
     a bit slow. See also
     <http://article.gmane.org/gmane.comp.gnu.mingw.user/8272>
     <http://article.gmane.org/gmane.comp.gnu.mingw.user/8273>
     <http://www.cygwin.com/ml/cygwin/1999-04/msg00478.html> */
  if (!SetEnvironmentVariableA(name,value)
      && (value || GetLastError() != ERROR_ENVVAR_NOT_FOUND))
    return -1;
#endif
#if defined(HAVE_UNSETENV)
  if (value == NULL)
   #if UNSETENV_POSIX
    return unsetenv(name);
   #else
    { unsetenv(name); return 0; }
   #endif
#endif
#if defined(HAVE_PUTENV)
  if (value == NULL) {
    putenv((char*)name);
    if (getenv(name) != NULL)
      /* putenv(name) may silently fail - as on *BSD (which have unsetenv)
         Solaris & woe32 do not have unsetenv
         _and_ putenv(name) does not work */
      setenv_via_environ(name,namelen,NULL,0);
    return 0;
  } else
   #if defined(WIN32_NATIVE)
    /* putenv("FOO=") unsets FOO on woe32!! */
    if (valuelen == 0) return setenv_via_environ(name,namelen,"",0); else
   #endif
  { var char* buffer = (char*)malloc(namelen+1+valuelen+1);
    if (!buffer)
      return -1; /* no need to set errno = ENOMEM */
    return putenv(cat_env_var(buffer,name,namelen,value,valuelen));
  }
#elif defined(HAVE_SETENV)
  /* setenv(name,NULL,1) ==> segfault */
  if (value == NULL) return setenv_via_environ(name,namelen,NULL,0);
  else return setenv(name,value,1);
#else
  return setenv_via_environ(name,namelen,value,valuelen);
#endif
}

LISPFUNN(set_env,2)
{ /* (SYS::SETENV name value)
 define the OS Environment variable NAME to have VALUE (string or NIL) */
  STACK_1 = check_string(STACK_1);
  if (!nullp(STACK_0)) STACK_0 = check_string(STACK_0);
  var object value = popSTACK();
  var object name = popSTACK();
  var int ret;
  with_string_0(name,O(misc_encoding),namez, {
    begin_system_call();
    if (nullp(value)) {
      if (getenv(namez))
        ret = clisp_setenv(namez,NULL);
      else
        ret = 0;
    } else {
      with_string_0(value,O(misc_encoding),valuez, {
        ret = clisp_setenv(namez,valuez);
      });
    }
    end_system_call();
  });
  if (ret) {
    pushSTACK(value);
    pushSTACK(name);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S (~S ~S): out of memory"));
  }
  VALUES1(value);
}

#endif

#ifdef WIN32_NATIVE

LISPFUNN(registry,2)
{ /* (SYSTEM::REGISTRY path name) => the value of path\\name in the registry.
 Used to implement SHORT-SITE-NAME and LONG-SITE-NAME. */
  STACK_1 = check_string(STACK_1);
  STACK_0 = check_string(STACK_0);
  with_string_0(STACK_1,O(misc_encoding),pathz, {
    with_string_0(STACK_0,O(misc_encoding),namez, {
      LONG err;
      HKEY key;
      DWORD type;
      DWORD size;
      begin_system_call();
      err = RegOpenKeyEx(HKEY_LOCAL_MACHINE,pathz,0,KEY_READ, &key);
      if (!(err == ERROR_SUCCESS)) {
        if (err == ERROR_FILE_NOT_FOUND) {
          VALUES1(NIL);
          goto none;
        }
        SetLastError(err); OS_error();
      }
      err = RegQueryValueEx(key,namez,NULL,&type, NULL,&size);
      if (!(err == ERROR_SUCCESS)) {
        if (err == ERROR_FILE_NOT_FOUND) {
          RegCloseKey(key);
          VALUES1(NIL);
          goto none;
        }
        SetLastError(err); OS_error();
      }
      switch (type) {
        case REG_SZ: {
          var char* buf = (char*)alloca(size);
          err = RegQueryValueEx(key,namez,NULL,&type, (BYTE*)buf,&size);
          if (!(err == ERROR_SUCCESS)) { SetLastError(err); OS_error(); }
          err = RegCloseKey(key);
          if (!(err == ERROR_SUCCESS)) { SetLastError(err); OS_error(); }
          end_system_call();
          VALUES1(asciz_to_string(buf,O(misc_encoding)));
        }
          break;
        default: {
          var object path_name;
          pushSTACK(STACK_1); pushSTACK(O(backslash_string)); pushSTACK(STACK_(0+2));
          path_name = string_concat(3);
          pushSTACK(path_name);
          pushSTACK(TheSubr(subr_self)->name);
          fehler(error,GETTEXT("~S: type of attribute ~S is unsupported"));
        }
      }
    });
     none:;
  });
  skipSTACK(2);
}

#endif

LISPFUN(software_type,seclass_no_se,0,0,norest,nokey,0,NIL)
{ /* (SOFTWARE-TYPE), CLTL p. 448 */
  VALUES1(O(software_type));
}

LISPFUN(software_version,seclass_no_se,0,0,norest,nokey,0,NIL)
{ /* (SOFTWARE-VERSION), CLTL p. 448 */
#if defined(GNU)
 #if defined(__cplusplus)
  pushSTACK(CLSTEXT("GNU C++ "));
 #else
  pushSTACK(CLSTEXT("GNU C "));
 #endif
  pushSTACK(O(c_compiler_version));
  VALUES1(string_concat(2));
#else
 #if defined(__cplusplus)
  VALUES1(CLSTEXT("C++ compiler"));
 #else
  VALUES1(CLSTEXT("C compiler"));
 #endif
#endif
}

LISPFUNNF(identity,1)
{ /* (IDENTITY object), CLTL p. 448 */
  VALUES1(popSTACK());
}

LISPFUNN(address_of,1)
{ /* (SYS::ADDRESS-OF object) return the address of the object */
  var object arg = popSTACK();
  #ifdef TYPECODES
   #if defined(WIDE_HARD)
    VALUES1(UQ_to_I(untype(arg)));
   #elif defined(WIDE_SOFT)
    VALUES1(UL_to_I(untype(arg)));
   #else
    VALUES1(UL_to_I(as_oint(arg)));
   #endif
  #else /* HEAPCODES */
   #if defined(WIDE)
    VALUES1(UQ_to_I(as_oint(arg)));
   #else
    VALUES1(UL_to_I(as_oint(arg)));
   #endif
  #endif
}

LISPFUNN(code_address_of,1)
{ /* (SYS::CODE-ADDRESS-OF object) return the address of the machine codes
     of the object */
  var object obj = popSTACK();
  if (ulong_p(obj)) /* Number within the range of aint == ulong ==> self */
    VALUES1(obj);
  else if (subrp(obj))          /* SUBR ->  its address */
    VALUES1(ulong_to_I((aint)(TheSubr(obj)->function)));
  else if (fsubrp(obj))         /* FSUBR -> its address */
    VALUES1(ulong_to_I((aint)(TheFsubr(obj)->function)));
 #ifdef DYNAMIC_FFI
  else if (ffunctionp(obj)) {
    object fa = check_faddress_valid(TheFfunction(obj)->ff_address);
    VALUES1(ulong_to_I((uintP)Faddress_value(fa)));
  }
 #endif
  else
    VALUES1(NIL);
}

/* (SYS::PROCESS-ID) returns the pid */
LISPFUNN(process_id,0) {
  begin_system_call();
#if defined(UNIX)
  var int pid = getpid();
  end_system_call();
  VALUES1(sint32_to_I(pid));
#elif defined(WIN32_NATIVE)
  var DWORD pid = GetCurrentProcessId();
  end_system_call();
  VALUES1(uint32_to_I(pid));
#else
  #error "What is process-ID on your system?"
#endif
}

LISPFUNNF(ansi,0)
{ /* (SYS::ANSI) */
  VALUES1(O(ansi));
}

LISPFUNN(set_ansi,1)
{ /* (SYS::SET-ANSI ansi-p) */
  var object val = (nullp(popSTACK()) ? NIL : T);
  var object notval = (nullp(val) ? T : NIL);
  /* (SETQ *ANSI* val) */
  O(ansi) = val;
  /* (SETQ *FLOATING-POINT-CONTAGION-ANSI* val) */
  Symbol_value(S(floating_point_contagion_ansi)) = val;
  /* (SETQ *FLOATING-POINT-RATIONAL-CONTAGION-ANSI* val) */
  Symbol_value(S(floating_point_rational_contagion_ansi)) = val;
  /* (SETQ *PHASE-ANSI* val) */
  Symbol_value(S(phase_ansi)) = val;
  /* (SETQ *LOOP-ANSI* val) */
  Symbol_value(S(loop_ansi)) = val;
  /* (SETQ *MERGE-PATHNAMES-ANSI* val) */
  Symbol_value(S(merge_pathnames_ansi)) = val;
  /* (SETQ *PRINT-PATHNAMES-ANSI* val) */
  Symbol_value(S(print_pathnames_ansi)) = val;
  /* (SETQ *PRINT-SPACE-CHAR-ANSI* val) */
  Symbol_value(S(print_space_char_ansi)) = val;
  /* (SETQ *PARSE-NAMESTRING-ANSI* val) */
  Symbol_value(S(parse_namestring_ansi)) = val;
  /* (SETQ *SEQUENCE-COUNT-ANSI* val) */
  Symbol_value(S(sequence_count_ansi)) = val;
  /* (SETQ *COERCE-FIXNUM-CHAR-ANSI* val) */
  Symbol_value(S(coerce_fixnum_char_ansi)) = val;
  /* (SETQ *PRINT-EMPTY-ARRAYS-ANSI* val) */
  Symbol_value(S(print_empty_arrays_ansi)) = val;
  /* (SETQ *PRINT-UNREADABLE-ANSI* val) */
  Symbol_value(S(print_unreadable_ansi)) = val;
  /* (SETQ *DEFUN-ACCEPT-SPECIALIZED-LAMBDA-LIST* (not val)) */
  Symbol_value(S(defun_accept_specialized_lambda_list)) = notval;
  VALUES1(val);
}

LISPFUN(module_info,seclass_no_se,0,2,norest,nokey,0,NIL)
{ /* (EXT:MODULE-INFO)       ==> list of all module names
  (EXT:MODULE-INFO "name")   ==> "name", subr-count, obj-count
  (EXT:MODULE-INFO "name" t) ==> "name", s-count, s-list, o-count, o-list
  (EXT:MODULE-INFO :ffi)     ==> shared library names
  (EXT:MODULE-INFO :ffi t)   ==> DLLs & all associated objects */
  var object verbose = popSTACK();
  var bool verbosep = !missingp(verbose);
  var object arg = popSTACK();
  if (missingp(arg))
    VALUES1(listof(modules_names_to_stack()));
 #if defined(DYNAMIC_FFI) && (defined(WIN32_NATIVE) || defined(HAVE_DLOPEN))
  else if (eq(arg,S(Kffi))) {
    if (!verbosep) {            /* just the library names */
      pushSTACK(L(car)); pushSTACK(O(foreign_libraries));
      funcall(L(mapcar),2);
    } else {                      /* everything */
      pushSTACK(O(foreign_libraries));
      funcall(L(copy_tree),1);
    }
  }
 #endif
  else {
    arg = check_string(arg);
    var module_t *mod;
    with_string_0(arg,O(misc_encoding),mod_namez,
                  { mod = find_module(mod_namez); });
    if (mod == NULL) VALUES0;
    else if (verbosep) {
      pushSTACK(arg);
      var uintC count = *(mod->stab_size);
      while (count--) pushSTACK(mod->stab[count].name);
      { var object tmp = listof(*mod->stab_size); pushSTACK(tmp); }
      count = *(mod->otab_size);
      while (count--) pushSTACK(mod->otab[count]);
      value5 = listof(*mod->otab_size);
      value4 = fixnum(*(mod->otab_size));
      value3 = popSTACK();
      value2 = fixnum(*(mod->stab_size));
      value1 = popSTACK();
      mv_count = 5;
    } else
      VALUES3(arg,fixnum(*(mod->stab_size)),fixnum(*(mod->otab_size)));
  }
}

LISPFUN(argv,seclass_no_se,0,0,norest,nokey,0,NIL)
{
  VALUES1(copy_svector(O(argv)));
}

/* === a module facility to map CPP constants to Lisp symbols. ===
 return : alist of ((C lisp) ...)
 may trigger GC */
local maygc object map_to_alist (const c_lisp_map_t *map) {
  unsigned int index;
  for (index=0; index < map->size; index++) {
    pushSTACK(L_to_I(map->table[index].c_const));
    pushSTACK(*(map->table[index].l_const));
    { object tmp=listof(2); pushSTACK(tmp); }
  }
  return listof(map->size);
}

/* Lisp symbol ---> C number
 may trigger GC -- on error only */
global maygc long map_lisp_to_c (object obj, const c_lisp_map_t *map) {
  unsigned int index;
 restart_map_lisp_to_c:
  if (map->use_default_function_p && integerp(obj)) return I_to_L(obj);
  else if (map->have_default_value_p && missingp(obj))
    return map->default_value;
  else {
    for (index = 0; index < map->size; index++)
      if (eq(obj,*(map->table[index].l_const)))
        return map->table[index].c_const;
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    /* TYPE-ERROR slot EXPECTED-TYPE : (member [nil] l_const ...)
       or (or integer (member [nil] l_const ...)) */
    pushSTACK(S(member));
    if (map->have_default_value_p) pushSTACK(NIL);
    for (index = 0; index < map->size; index++)
      pushSTACK(*(map->table[index].l_const));
    var object tmp=listof(map->size+1+(map->have_default_value_p?1:0));
    pushSTACK(tmp);
    if (map->use_default_function_p) {
      pushSTACK(S(or));
      pushSTACK(S(integer));
      pushSTACK(STACK_2);       /* (member [nil] l_const ...) */
      var object tmp=listof(3);
      STACK_0 = tmp; /* replace (member ...) with (or integer (member ...)) */
    }
    pushSTACK(map_to_alist(map));
    pushSTACK(asciz_to_string(map->name,O(misc_encoding)));
    pushSTACK(STACK_3/*obj*/); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,
                GETTEXT("~S: Lisp value ~S is not found in table ~S: ~S"));
    obj = value1;
    goto restart_map_lisp_to_c;
  }
}

/* C number ---> Lisp symbol
 may trigger GC -- on error only */
global maygc object map_c_to_lisp (long val, const c_lisp_map_t *map) {
  unsigned int index;
 restart_map_c_to_lisp:
  for (index=0; index < map->size; index++)
    if (val == map->table[index].c_const)
      return *(map->table[index].l_const);
  if (map->have_default_value_p && val == map->default_value) return NIL;
  if (map->use_default_function_p) return L_to_I(val);
  pushSTACK(NIL);                   /* no PLACE */
  pushSTACK(L_to_I(val));           /* TYPE-ERROR slot DATUM */
  pushSTACK(S(member));
  for (index=0; index < map->size; index++)
    pushSTACK(L_to_I(map->table[index].c_const));
  var object tmp=listof(map->size+1);
  pushSTACK(tmp);               /*TYPE-ERROR slot EXPECTED-TYPE*/
  pushSTACK(map_to_alist(map));
  pushSTACK(asciz_to_string(map->name,O(misc_encoding)));
  pushSTACK(STACK_3/*val*/); pushSTACK(TheSubr(subr_self)->name);
  check_value(type_error,
              GETTEXT("~S: C value ~S is not found in table ~S: ~S"));
  val = I_to_L(check_slong(value1));
  goto restart_map_c_to_lisp;
}

/* C number ---> list of Lisp symbols, each denoting a bit
 may trigger GC */
global maygc object map_c_to_list (long val, const c_lisp_map_t *map) {
  unsigned int count = 0;
  unsigned int index = 0;
  for (; index < map->size; index++) {
    unsigned long c_const = map->table[index].c_const;
    if (c_const && (c_const == (val & c_const))) {
      pushSTACK(*(map->table[index].l_const));
      count++;
      val &= ~c_const;         /* clear this bit */
    }
  }
  /* not all bits accounted for: */
  if (val) { pushSTACK(L_to_I(val)); count++; }
  return listof(count);
}

/* list of Lisp symbols, each denoting a bit ---> C number
 may trigger GC -- on error only */
global maygc long map_list_to_c (object obj, const c_lisp_map_t *map) {
  if (listp(obj)) {
    long ret = 0;
    pushSTACK(obj);
    for (; !endp(STACK_0); STACK_0 = Cdr(STACK_0))
      ret |= map_lisp_to_c(Car(STACK_0),map);
    skipSTACK(1);
    return ret;
  } else
    return map_lisp_to_c(obj,map);
}
