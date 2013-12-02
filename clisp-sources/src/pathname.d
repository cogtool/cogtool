/*
 * Pathnames for CLISP
 * Bruno Haible 1990-2005
 * Logical Pathnames: Marcus Daniels 16.9.1994
 * ANSI compliance, bugs: Sam Steingold 1998-2006
 * German comments translated into English: Stefan Kain 2002-01-03
 */

#include "lispbibl.c"
#ifdef WIN32_NATIVE
#include "w32shell.c"
#endif
#include <string.h> /* declares strlen() */

/* enable the following #define to debug pathname translations
 setting DEBUG_TRANSLATE_PATHNAME to a larger value results in more output
 WARNING: PRIN1 can trigger GC! BEWARE!
 define DEBUG_TRANSLATE_PATHNAME 1 */
#if DEBUG_TRANSLATE_PATHNAME
#define string_concat(x) (printf("[%d]string_concat(%d)\n",__LINE__,x),(string_concat)(x))
/* define DOUT(l,o) printf("[%d] %s %s\n",__LINE__,l,#o);gar_col() */
#define DOUT(label,obj)  OBJECT_OUT(obj,label)
#define SDOUT(label,obj) printf("%d %s %s",__LINE__,label,STRING(obj));nobject_out(stdout,obj)
#else
#define DOUT(l,o)
#define SDOUT(l,o)
#endif

/* ========================================================================
                       Low level functions */

/* UP: Tests whether a pathname is possibly a symlink.
 possible_symlink(path) */
#ifdef UNIX_LINUX
local inline bool possible_symlink (const char* path) {
  /* In Linux 2.0.35, /proc/<pid>/{cwd,exe,root} and /proc/<pid>/fd/<n>
   are symlinks pointing to void. Treat them like non-symlinks, in order
   to avoid errors. */
  if (path[0]=='/'
      && path[1]=='p' && path[2]=='r' && path[3]=='o' && path[4]=='c'
      && path[5]=='/'
      && (path[6]>='0' && path[6]<='9'))
    return false;
  return true;
}
#else
  #define possible_symlink(path)  true
#endif

#ifdef UNIX_LINUX
/* The Linux /proc filesystem has some symlinks whose readlink value is
 zero-terminated: /proc/self in Linux 2.0.35, /proc/<pid>/fd/<n> in
 Linux 2.2.2. Remove this extraneous trailing zero byte. */
local inline int my_readlink (const char* path, char* buf, size_t bufsiz) {
  var int linklen = readlink(path,buf,bufsiz);
  if (linklen > 0 && buf[linklen-1] == '\0')
    linklen--;
  return linklen;
}
#define readlink  my_readlink
#endif

/* we need realpath() (declared in <stdlib.h>, included under STDC_HEADERS)
   http://www.opengroup.org/onlinepubs/009695399/functions/realpath.html
   which is alleged to be broken on some systems
   OTOH, on some other systems, notably on cygwin,
   we _do_ need the system implementation of realpath
   because otherwise we get screwed on /proc/self/exe -> lisp
   instead of lisp.exe and possibly other quirks */
#if defined(UNIX) && !defined(HAVE_REALPATH)
  /* library-function realpath implementation:
   [Copyright: SUN Microsystems, B. Haible]
   TITLE
     REALPATH(3)
   SYNOPSIS
     char* realpath (const char* path, char resolved_path[MAXPATHLEN]);
   DESCRIPTION
     realpath() expands all symbolic links  and  resolves  refer-
     ences  to '/./', '/../' and extra '/' characters in the null
     terminated string named by path and stores the canonicalized
     absolute pathname in the buffer named by resolved_path.  The
     resulting path will have no symbolic links  components,  nor
     any '/./' or '/../' components.
   RETURN VALUES
     realpath() returns a pointer to the  resolved_path  on  suc-
     cess.   On  failure, it returns NULL, sets errno to indicate
     the error, and places in resolved_path the absolute pathname
     of the path component which could not be resolved. */
#define realpath my_realpath /* avoid conflict with Consensys realpath declaration */
local char* realpath (const char* path, char* resolved_path) {
  /* Method: use getwd and readlink. */
  var char mypath[MAXPATHLEN];
  var int symlinkcount = 0; /* the number of symbolic links so far */
  var char* resolved_limit = &resolved_path[MAXPATHLEN-1];
  /* Valid pointers are those with resolved_path <= ptr <= resolved_limit.
   in *resolved_limit at most one null byte.
   (similarly with mypath.) */
  var char* resolve_start;
  {
    var char* resolved_ptr = resolved_path; /* always <= resolved_limit */
    /* poss. use Working-Directory: */
    if (!(path[0]=='/')) { /* not an absolute pathname? */
      if (getwd(resolved_path) == NULL)
        return NULL;
      resolved_ptr = resolved_path;
      while (*resolved_ptr) {
        resolved_ptr++;
      }
      if (resolved_ptr < resolved_limit) {
        *resolved_ptr++ = '/';
      }
      resolve_start = resolved_ptr;
    } else {
      resolve_start = resolved_ptr = &resolved_path[0];
    }
    /* copy the path: */
    var const char* path_ptr = path;
    while ((resolved_ptr < resolved_limit) && *path_ptr) {
      *resolved_ptr++ = *path_ptr++;
    }
    /* finish with '/' and a null: */
    if (resolved_ptr < resolved_limit) {
      *resolved_ptr++ = '/';
    }
    *resolved_ptr = 0;
  }
  /* Now start in resolved_path at resolve_start. */
  var char* from_ptr = resolve_start;
  var char* to_ptr = resolve_start;
  while ((to_ptr < resolved_limit) && (*from_ptr)) {
    /* so far the path in  resolved_path[0]...to_ptr[-1]
     has the shape '/subdir1/subdir2/.../txt',
     whereas 'txt' is poss. empty, but no subdir is empty. */
    var char next = *from_ptr++; *to_ptr++ = next;
    if ((next == '/') && (to_ptr > resolved_path+1)) {
      /* to_ptr[-1]='/'  ->  resolve Directory ...to_ptr[-2] : */
      var char* last_subdir_end = &to_ptr[-2];
      switch (*last_subdir_end) {
        case '/':
          #ifdef PATHNAME_UNIX_UNC
          if (to_ptr > resolved_path+2)
          #endif
            /* '//' is simplified to '/' : */
            to_ptr--;
          break;
        case '.':
          {
            var char* last_subdir_ptr = &last_subdir_end[-1];
            if (to_ptr > resolved_path+2) {
              if (*last_subdir_ptr == '.') {
                if ((to_ptr > resolved_path+4)
                    && (*--last_subdir_ptr == '/')) {
                  /* last subdir was '/../'
                   Therefore remove the subdir in front of it: */
                  while ((last_subdir_ptr > resolved_path)
                         && !(*--last_subdir_ptr == '/'));
                  to_ptr = last_subdir_ptr+1;
                }
              } else if (*last_subdir_ptr == '/') {
                /* last subdir was '/./'
                 remove: */
                to_ptr = last_subdir_end;
              }
            }
          }
          break;
        default:
          /* after a normal subdir */
          #ifdef HAVE_READLINK
          if (possible_symlink(resolved_path)) {
            /* read symbolic link: */
            to_ptr[-1]=0; /* replace '/' with 0 */
            #ifdef UNIX_CYGWIN32
            /* readlink() does not work right on NFS mounted directories
             (it returns -1,ENOENT or -1,EIO).
             So check for a directory first. */
            var struct stat statbuf;
            if (lstat(resolved_path,&statbuf) < 0)
              return NULL; /* error */
            if (S_ISDIR(statbuf.st_mode)) {
              /* directory, not a symbolic link */
              to_ptr[-1] = '/'; /* insert the '/' again */
            } else if (!S_ISLNK(statbuf.st_mode)) {
              /* something else, but not a directory or symbolic link. */
              errno = ENOTDIR;
              return NULL;
            } else
            #endif
              {
                var int linklen =
                  readlink(resolved_path,mypath,sizeof(mypath)-1);
                if (linklen >=0) { /* was a symbolic link */
                  if (++symlinkcount > MAXSYMLINKS) {
                    errno = ELOOP_VALUE; return NULL;
                  }
                  { /* append the still to be resolved part of path
                   to the link-content: */
                    var char* mypath_ptr = &mypath[linklen]; /* here is room */
                    var char* mypath_limit = &mypath[MAXPATHLEN-1]; /* up to here */
                    if (mypath_ptr < mypath_limit) { *mypath_ptr++ = '/'; } /* first, append a '/' */
                    /* then the rest: */
                    while ((mypath_ptr <= mypath_limit)
                           && (*mypath_ptr = *from_ptr++))
                      { mypath_ptr++; }
                    *mypath_ptr = 0; /* and conclude wit 0 */
                  }
                  /* this replaces resp. completes the path: */
                  if (mypath[0] == '/') { /* replaces the path: */
                    from_ptr = &mypath[0]; to_ptr = resolved_path;
                    while ((*to_ptr++ = *from_ptr++));
                    from_ptr = resolved_path;
                  } else { /* completes the path:
                     disrcard link-name. Therefore search for the last '/': */
                    {
                      var char* ptr = &to_ptr[-1];
                      while ((ptr > resolved_path) && !(ptr[-1] == '/')) { ptr--; }
                      from_ptr = ptr;
                    }
                    {
                      var char* mypath_ptr = &mypath[0]; to_ptr = from_ptr;
                      while ((to_ptr <= resolved_limit) && (*to_ptr++ = *mypath_ptr++));
                    }
                  }
                  to_ptr = from_ptr;
                } else {
                  #if defined(UNIX_IRIX)
                  if ((errno == EINVAL) || (errno == ENXIO))
                  #elif defined(UNIX_CYGWIN32)
                  if ((errno == EINVAL) || (errno == EACCES))
                  #else
                  if (errno == EINVAL)
                  #endif
                    /* no symbolic link */
                    to_ptr[-1] = '/'; /* insert the '/' again */
                  else
                    return NULL; /* error */
                }
              }
          }
          #endif
          break;
      }
    }
  } /* go for the next subdir */
  /* discard a '/' at the tail: */
  if ((to_ptr[-1] == '/')
      #ifdef PATHNAME_UNIX_UNC
      && (to_ptr > resolved_path+2)
      #else
      && (to_ptr > resolved_path+1)
      #endif
      )
    to_ptr--;
  to_ptr[0] = 0; /* conclude with 0 */
  return resolved_path; /* finished */
}
#endif

/* Creates a new subdirectory.
 make_directory(pathstring);
 > pathstring: result of shorter_directory(...)
 > STACK_0: pathname */
local inline void make_directory (char* pathstring) {
 #ifdef UNIX
  begin_system_call();
  if (mkdir(pathstring,0777)) { /* create sub-directory */
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
 #ifdef WIN32_NATIVE
  begin_system_call();
  if (! CreateDirectory(pathstring,NULL) ) { /* create sub-directory */
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
}

/* Deletes a subdirectory.
 delete_directory(pathstring);
 > pathstring: result of shorter_directory(...)
 > STACK_0: pathname */
local inline void delete_directory (char* pathstring) {
 #ifdef UNIX
  begin_system_call();
  if (rmdir(pathstring)) { /* delete sub-directory */
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
 #ifdef WIN32_NATIVE
  begin_system_call();
  if (! RemoveDirectory(pathstring) ) { /* delete sub-directory */
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
}

#ifdef WIN32_NATIVE
/* Changes the operating system's current directory.
 change_directory(pathstring);
 > pathstring: directory, ASCIZ-String
 > STACK_0: pathname */
local inline void change_current_directory (char* pathstring) {
  begin_system_call();
  if (!SetCurrentDirectory(pathstring)) {
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
}
#endif

/* Delete a file.
 delete_existing_file(pathstring);
 It is known that the file exists.
 > pathstring: file name, ASCIZ-String
 > STACK_0: pathname */
local inline void delete_existing_file (char* pathstring) {
 #ifdef UNIX
  begin_system_call();
  if (!( unlink(pathstring) ==0)) {
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
 #ifdef WIN32_NATIVE
  begin_system_call();
  if (! DeleteFile(pathstring) ) {
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
}

#ifdef WIN32_NATIVE
#define WIN32_ERROR_NOT_FOUND (GetLastError()==ERROR_FILE_NOT_FOUND || GetLastError()==ERROR_PATH_NOT_FOUND || GetLastError()==ERROR_BAD_NETPATH)
#endif

/* Delete a file.
 delete_file_if_exists(pathstring);
 No error is signaled if the file does not exist.
 > pathstring: file name, ASCIZ-String
 > STACK_0: pathname
 < result: whether the file existed */
local inline bool delete_file_if_exists (char* pathstring) {
  var bool exists = true;
 #ifdef UNIX
  begin_system_call();
  if (!( unlink(pathstring) ==0)) {
    if (!(errno==ENOENT)) { /* not found -> OK */
      end_system_call(); OS_file_error(STACK_0); /* report other error */
    }
    exists = false;
  }
  end_system_call();
 #endif
 #ifdef WIN32_NATIVE
  begin_system_call();
  if (! DeleteFile(pathstring) ) {
    if (!WIN32_ERROR_NOT_FOUND) {
      end_system_call(); OS_file_error(STACK_0);
    }
    exists = false;
  }
  end_system_call();
 #endif
  return exists;
}

/* Delete a file being the target of a subsequent rename.
 delete_file_before_rename(pathstring);
 No error is signaled if the file does not exist.
 > pathstring: file name, ASCIZ-String
 > STACK_0: pathname */
local inline void delete_file_before_rename (char* pathstring) {
 #if !defined(UNIX) /* rename() on Unix does it automatically */
  delete_file_if_exists(pathstring);
 #endif
}

/* Rename a file.
 rename_existing_file(old_pathstring,new_pathstring);
 It is known that the old_pathstring exists.
 On platforms except UNIX, it is known that new_pathstring does not exist.
 > old_pathstring: old file name, ASCIZ-String
 > new_pathstring: new file name, ASCIZ-String
 > STACK_0: pathname */
local inline void rename_existing_file (char* old_pathstring,
                                        char* new_pathstring) {
 #ifdef UNIX
  begin_system_call();
  if ( rename(old_pathstring,new_pathstring) <0) { /* rename file */
    end_system_call(); OS_file_error(STACK_0); /* report error */
  }
  end_system_call();
 #endif
 #ifdef WIN32_NATIVE
  begin_system_call();
  if (! MoveFile(old_pathstring,new_pathstring) ) {
    end_system_call(); OS_file_error(STACK_0);
  }
  end_system_call();
 #endif
}

/* ========================================================================
                         P A T H N A M E S */

/* All simple-strings occurring in pathnames are in fact
   normal-simple-strings. */

#ifdef PATHNAME_UNIX
/* Components:
 HOST          always NIL
 DEVICE        always NIL
 DIRECTORY     (Startpoint . Subdirs) whereas
                Startpoint = :RELATIVE | :ABSOLUTE
                Subdirs = () | (subdir . Subdirs)
                subdir = :WILD-INFERIORS (means "**" or "...", all subdirectories) or
                subdir = Simple-String, poss. with wildcard-character ? and *
 NAME          NIL or
               Simple-String, poss. with wildcard-character ? and *
               (also :WILD on input)
 TYPE          NIL or
               Simple-String, poss. with wildcard-character ? and *
               (also :WILD on input)
 VERSION       always NIL (also :WILD or :NEWEST on input)
 A UNIX-filename is split in Name and Type as follows:
   if there is no '.' in Filename: Name = everything, Type = NIL,
   if there is '.' in Filename: Name = everything in front of it, Type = everything behind the last '.' .
 If a pathname must be completely specified (no wildcards),
  :WILD, :WILD-INFERIORS are not allowed, no wildcard-characters in the
 Strings, at NAME poss. also not NIL.
 External Notation:  server:/sub1.typ/sub2.typ/name.typ
 with Defaults:             /sub1.typ/sub2.typ/name.typ
 or                                            name.typ
 or                         /sub1.typ/ ** /sub3.typ/x*.lisp  (without Spaces!)
 or similar.
 If NAME starts with a dot, (parse-namestring (namestring pathname)) will not
 be the same as pathname. */
#endif

#ifdef PATHNAME_WIN32
/* Components:
 HOST          NIL or Simple-String (Wildcard-Characters are without meaning)
 DEVICE        NIL or :WILD or "A"|...|"Z"
 DIRECTORY     (Startpoint . Subdirs) whereas
                Startpoint = :RELATIVE | :ABSOLUTE
                Subdirs = () | (subdir . Subdirs)
                subdir = :WILD-INFERIORS (means "**" or "...", all Subdirectories) or
                subdir = Simple-String, poss. with Wildcard-Character ? and *
 NAME          NIL or
               Simple-String, poss. with Wildcard-Character ? and *
               (also :WILD on input)
 TYPE          NIL or
               Simple-String, poss. with Wildcard-Character ? and *
               (also :WILD on input)
 VERSION       always NIL (also :WILD or :NEWEST on input)
 If HOST is non-NIL, DEVICE must be NIL.
 A WIN32-Filename is split into Name and Type as follows:
   if there is no '.' in Filename: Name = everything, Type = NIL,
   if there is a '.' in Filename: Name = everything in front of, Type = everything behind the last '.' .
 If a Pathname must be completely specified (no Wildcards),
 then :WILD, :WILD-INFERIORS are not allowed, no Wildcard-Characters in the
 Strings, at NAME poss. also not NIL.
 External notation:       A:\sub1.typ\sub2.typ\name.typ
 with Defaults:             \sub1.typ\sub2.typ\name.typ
 or                                            name.typ
 or                       *:\sub1.typ\**\sub3.typ\x*.lisp
 or similar.
 Instead of '\'  - traditionally on DOS - also '/' is allowed.
 If HOST is non-NIL and the DIRECTORY's Startpoint is not :ABSOLUTE,
 (parse-namestring (namestring pathname)) will not be the same as pathname. */
#endif

#ifdef LOGICAL_PATHNAMES
/* Components of Logical Pathnames:
 HOST          Simple-String or NIL
 DEVICE        always NIL
 DIRECTORY     (Startpoint . Subdirs) whereas
                Startpoint = :RELATIVE | :ABSOLUTE
                Subdirs = () | (subdir . Subdirs)
               subdir = :WILD-INFERIORS (means "**", all Subdirectories) or
               subdir = :WILD (means "*") or
               subdir = Simple-String, poss. with Wildcard-Character *
 NAME          NIL or
               :WILD (means "*") or
               Simple-String, poss. with Wildcard-Character *
 TYPE          NIL or
               :WILD (means "*") or
               Simple-String, poss. with Wildcard-Character *
 VERSION       NIL or :NEWEST or :WILD or Integer
 External Notation: see CLtl2 p. 628-629. */
#endif

/* access functions without case transforms:
 xpathname_host(logical,pathname)
 xpathname_device(logical,pathname)
 xpathname_directory(logical,pathname)
 xpathname_name(logical,pathname)
 xpathname_type(logical,pathname)
 xpathname_version(logical,pathname)
 > pathname: pathname or logical pathname
 > logical: flag = logpathnamep(pathname)
 < result: the value of the requested component
 pathname_*_maybe return the appropriate slot seen from the point of view of the
 underlying physical file system, therefore, ever though pathname has the slot
 version (for ANSI compliance reasons), pathname_version_maybe() returns NIL */
#if HAS_HOST
#define pathname_host_maybe(obj) (object)ThePathname(obj)->pathname_host
#else
#define pathname_host_maybe(obj) (unused(obj), NIL)
#endif
#if HAS_DEVICE
#define pathname_device_maybe(obj) (object)ThePathname(obj)->pathname_device
#else
#define pathname_device_maybe(obj) (unused(obj), NIL)
#endif
#if HAS_VERSION
#define pathname_version_maybe(obj) (object)ThePathname(obj)->pathname_version
#else
#define pathname_version_maybe(obj) (unused(obj), NIL)
#endif

#ifdef LOGICAL_PATHNAMES
#define xpathname_host(logical,pathname)                       \
  (logical ? (object)TheLogpathname(pathname)->pathname_host : \
             pathname_host_maybe(pathname))
#define xpathname_device(logical,pathname)  \
  (logical ? NIL : pathname_device_maybe(pathname))
#define xpathname_directory(logical,pathname)                       \
  (logical ? (object)TheLogpathname(pathname)->pathname_directory : \
                (object)ThePathname(pathname)->pathname_directory)
#define xpathname_name(logical,pathname)                       \
  (logical ? (object)TheLogpathname(pathname)->pathname_name : \
                (object)ThePathname(pathname)->pathname_name)
#define xpathname_type(logical,pathname)                       \
  (logical ? (object)TheLogpathname(pathname)->pathname_type : \
                (object)ThePathname(pathname)->pathname_type)
#define xpathname_version(logical,pathname)                       \
  (logical ? (object)TheLogpathname(pathname)->pathname_version : \
                (object)ThePathname(pathname)->pathname_version)
#else /* no logical pathnames */
#define xpathname_host(logical,pathname) \
  pathname_host_maybe(pathname)
#define xpathname_device(logical,pathname) \
  pathname_device_maybe(pathname)
#define xpathname_directory(logical,pathname) \
  ThePathname(pathname)->pathname_directory
#define xpathname_name(logical,pathname) \
  ThePathname(pathname)->pathname_name
#define xpathname_type(logical,pathname) \
  ThePathname(pathname)->pathname_type
#define xpathname_version(logical,pathname) \
  ThePathname(pathname)->pathname_version
#endif

#define SUBST_RECURSE(atom_form,self_call)                      \
  if (atomp(obj)) return atom_form;                             \
  check_STACK(); check_SP();                                    \
  pushSTACK(obj);                                               \
  { /* recursive call for CAR: */                               \
    object new_car = self_call(Car(obj));                       \
    pushSTACK(new_car);                                         \
  }                                                             \
  { /* recursive call for CDR: */                               \
    object new_cdr = self_call(Cdr(STACK_1));                   \
    if (eq(new_cdr,Cdr(STACK_1)) && eq(STACK_0,Car(STACK_1))) { \
      obj = STACK_1; skipSTACK(2); return obj;                  \
    } else { /* (CONS new_car new_cdr) */                       \
      STACK_1 = new_cdr;                                        \
     {object new_cons = allocate_cons();                        \
      Car(new_cons) = popSTACK(); Cdr(new_cons) = popSTACK();   \
      return new_cons;                                          \
    }}                                                          \
  }

/* Converts capital-/small letters between :LOCAL and :COMMON .
 common_case(string)
 > string: Normal-Simple-String or Symbol/Number
 < result: converted Normal-Simple-String or the same Symbol/Number
 can trigger GC
 Operating System with preference for small letters or Capitalize */
local maygc object common_case (object string) {
  if (!simple_string_p(string))
    return string;
  var uintL len = Sstring_length(string);
  /* Search, if capital- or small letters (or both) occur: */
  var bool all_upper = true;
  var bool all_lower = true;
  if (len > 0) {
    var object storage = string; sstring_un_realloc(storage);
    SstringDispatch(storage,X, {
      var const cintX* ptr = &((SstringX)TheVarobject(storage))->data[0];
      var uintL count;
      dotimespL(count,len, {
        var chart ch = as_chart(*ptr++);
        if (!chareq(ch,up_case(ch)))
          all_upper = false;
        if (!chareq(ch,down_case(ch)))
          all_lower = false;
        if (!all_upper && !all_lower)
          break;
      });
    });
  }
  if (all_upper == all_lower)
    /* all_upper = all_lower = true: Nothing to convert.
     all_upper = all_lower = false: "Mixed case represents itself." */
    return string;
  if (all_upper)
    /* all_upper = true, all_lower = false: STRING-DOWNCASE */
    return string_downcase(string);
  else
    /* all_upper = false, all_lower = true: STRING-UPCASE */
    return string_upcase(string);
}
/* the same, recursive like with SUBST: */
local object subst_common_case (object obj) {
  SUBST_RECURSE(common_case(obj),subst_common_case);
}

#ifdef LOGICAL_PATHNAMES

local bool legal_logical_word_char (chart ch) {
  ch = up_case(ch);
  var cint c = as_cint(ch);
  if (((c >= 'A') && (c <= 'Z'))
      || ((c >= '0') && (c <= '9'))
      || (c == '-'))
    return true;
  else
    return false;
}

#endif

#if HAS_HOST

/* UP: Determines, if a character is allowed as character in the host-part
 of a namestring.
 legal_hostchar(ch)
 > chart ch: Character-Code
 < result: true if allowed, else false
 NB: legal_logical_word_char(ch) implies legal_hostchar(ch). */
local bool legal_hostchar (chart ch) {
 #if defined(PATHNAME_WIN32)
  { /* This is just a guess. I do not know which characters are allowed in
       Windows host names. */
    var cint c = as_cint(ch);
    if ((c >= ' ') && (c <= '~')
        && (c != '"') && (c != '/') && (c != ':') && (c != '<') && (c != '>')
        && (c != '\\'))
      return true;
    else
      return false;
  }
 #else
  return alphanumericp(ch) || chareq(ch,ascii('-'));
 #endif
}

/* UP: check an optional HOST argument
 test_optional_host(host,convert)
 > host: Host-Argument
 > convert: Flag, if case-conversion is undesired
 < result: valid host-component
 can trigger GC */
local maygc object test_optional_host (object host, bool convert) {
  if (!boundp(host) || eq(host,S(Kunspecific)))
    return NIL;
  if (nullp(host))
    goto OK; /* NIL is OK */
  /* Else, host must be a String, whose characters are alphanumeric: */
  if (!stringp(host)) {
    pushSTACK(host);         /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_host)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(host);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: host should be NIL or a string, not ~S"));
  }
  host = coerce_normal_ss(host); /* as Normal-Simple-String */
  if (convert)
    host = common_case(host);
  {
    var uintL len = Sstring_length(host);
    if (len > 0) {
      var const chart* charptr = &TheSnstring(host)->data[0];
      dotimespL(len,len, {
        var chart ch = *charptr++;
        if (!legal_hostchar(ch))
          goto badhost;
      });
    }
  }
 OK: return host;
 badhost:
  pushSTACK(host);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(parse_error,GETTEXT("~S: illegal hostname ~S"));
}

#else

#ifdef LOGICAL_PATHNAMES

/* UP: check an optional HOST argument
 test_optional_host(host)
 > host: Host-Argument
 < result: valid host-component
 can trigger GC */
local maygc object test_optional_host (object host) {
  if (!boundp(host) || eq(host,S(Kunspecific)))
    return NIL; /* not specified -> NIL */
  if (nullp(host))
    goto OK; /* NIL is OK */
  /* Else, host must be a String, whose characters are alphanumeric: */
  if (!stringp(host)) {
    pushSTACK(host);         /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_host)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(host);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: host should be NIL or a string, not ~S"));
  }
  host = coerce_normal_ss(host); /* as Normal-Simple-String */
  {
    var uintL len = Sstring_length(host);
    if (len > 0) {
      var object storage = host; sstring_un_realloc(storage);
      SstringDispatch(storage,X, {
        var const cintX* ptr = &((SstringX)TheVarobject(storage))->data[0];
        dotimespL(len,len, {
          var chart ch = as_chart(*ptr++);
          if (!legal_logical_word_char(ch))
            goto badhost;
        });
      });
    }
  }
 OK: return host;
 badhost:
  pushSTACK(host);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(parse_error,GETTEXT("~S: illegal hostname ~S"));
}

#else

/* UP: check an optional HOST argument
 test_optional_host(host);
 > host: Host-Argument
 < result: valid host-component */
local object test_optional_host (object host) {
  if (boundp(host)       /* not specified -> OK */
      && !nullp(host)    /* specified -> should be NIL or :UNSPECIFIC */
      && !eq(host,S(Kunspecific))) {
    pushSTACK(host);     /* TYPE-ERROR slot DATUM */
    pushSTACK(S(null));  /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(host);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: host should be NIL, not ~S"));
  }
  return NIL;
}

#endif

#endif

/* Determines, if two characters count as equal characters in pathnames.
 equal_pathchar(ch1,ch2)
 > chart ch1,ch2: Character-Codes
 < result: true if equal, else false */
  #if !defined(PATHNAME_WIN32)
    #define equal_pathchar(ch1,ch2)  chareq(ch1,ch2)
  #else /* defined(PATHNAME_WIN32) */
    /* Case-insensitive, but normally without conversion */
    #define equal_pathchar(ch1,ch2)  chareq(up_case(ch1),up_case(ch2))
  #endif

/* UP: check whether a given byte is a valid element of NAME or TYPE
 component in a Namestring
 legal_namebyte(ch)
 > uintB: byte
 < return: true if valid, else false */
local inline bool legal_namebyte (uintB ch) {
  #ifdef VALID_FILENAME_CHAR # defined in unixconf.h
    return VALID_FILENAME_CHAR || (ch=='*') || (ch=='?');
  #else
    #ifdef PATHNAME_UNIX
      return ((ch>=' ') && (ch<='~') && !(ch=='/'));
    #endif
    #ifdef PATHNAME_WIN32
      return ((ch >= 1) && (ch <= 127)
              && (ch != '"') /*&& (ch != '*')*/
              && (ch != '/') && (ch != ':')
              && (ch != '<') && (ch != '>') /*&& (ch != '?')*/
              && (ch != '\\'))
             || (ch == 131)
             || (ch >= 160);
    #endif
  #endif
}

/* UP: check whether the character is a valid element of NAME or TYPE
 component in a Namestring
 legal_namechar(ch)
 > chart ch: character-code
 < return: true if valid, else false */
local bool legal_namechar (chart ch) {
  #ifdef UNICODE
    var uintB buf[4]; # are there characters longer than 4 bytes?!
    var uintL char_len = cslen(O(pathname_encoding),&ch,1);
    cstombs(O(pathname_encoding),&ch,1,buf,char_len);
    while (char_len > 0) {
      char_len--;
      if (!legal_namebyte(buf[char_len])) return false;
    }
    return true;
  #else
    return legal_namebyte(as_cint(ch));
  #endif
}

/* Determines, if a character is a wildcard for a single
 character.
 singlewild_char_p(ch)
 > chart ch: Character-Code
 < result: true if yes, else false */
#define singlewild_char_p(ch)  chareq(ch,ascii('?'))
#define multiwild_char_p(ch)  chareq(ch,ascii('*'))
#define wild_char_p(ch)   (multiwild_char_p(ch) || singlewild_char_p(ch))

/* Converts an object into a pathname. */
local object coerce_xpathname (object obj); /* later */

/* Converts an object into a non-logical pathname. */
local object coerce_pathname (object obj); /* later */
#if !defined(LOGICAL_PATHNAMES)
#define coerce_pathname(obj)  coerce_xpathname(obj)
#endif

/* Returns a default-pathname. */
local object defaults_pathname (void); /* later */

/* checks a default-pathname.
 test_default_pathname(defaults)
 > defaults: defaults-argument
 < result: value of the defaults-argument, a pathname
 can trigger GC */
local maygc object test_default_pathname (object defaults) {
  if (missingp(defaults))
    /* not specified -> take value of *DEFAULT-PATHNAME-DEFAULTS* : */
    return defaults_pathname();
  else
    /* specified -> turn into a pathname: */
    return coerce_xpathname(defaults);
}

/* <http://www.lisp.org/HyperSpec/Body/sec_19-2-3.html>:
 "for functions that manipulate or inquire about files in the file system,
  the pathname argument to such a function is merged with
  *DEFAULT-PATHNAME-DEFAULTS* before accessing the file system"
 When pathname comes from a file stream, this is NOT done because
 that pathname has already been "transfered from the world of the abstract
 Lisp pathname algebra to the real world of computer file system"
 Another option is to ensure that all slots of *DEFAULT-PATHNAME-DEFAULTS*
 are non-NIL (use :UNSPECIFIC instead): then merge_defaults() becomes
 an idempotent operation -- assuming trivial directory or non-ANSI merging.

 merge_defaults(pathname)
 > pathname: a pathname
 < result: a pathname derived from it, with *DEFAULT-PATHNAME-DEFAULTS* merged
           in.
 can trigger GC */
local maygc object merge_defaults (object pathname) {
  pushSTACK(pathname); pushSTACK(defaults_pathname());
  funcall(L(merge_pathnames),2);
  return value1;
}

/* error-message because of illegal pathname-argument.
 fehler_pathname_designator(thing); ( fehler_... = error_... )
 > thing: (erroneous) argument */
nonreturning_function(local, fehler_pathname_designator, (object thing)) {
  pushSTACK(thing);                       /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_designator_pathname)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(O(type_designator_pathname));
  pushSTACK(thing);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,
         GETTEXT("~S: argument ~S should be a pathname designator ~S"));
}

/* Tracks a chain of Synonym-Streams, so long as a File-Stream
 is reached.
 as_file_stream(stream)
 > stream: Builtin-Stream
 < stream: File-Stream */
local object as_file_stream (object stream) {
  var object s = stream;
  loop {
    if (TheStream(s)->strmtype == strmtype_file)
      return s;
    if (!(TheStream(s)->strmtype == strmtype_synonym))
      break;
    s = Symbol_value(TheStream(stream)->strm_synonym_symbol);
    if (!builtin_stream_p(s))
      break;
  }
  fehler_pathname_designator(stream);
}

/* Signal an error if a file-stream does not have
 a file-name associated with it.
 test_file_stream_named(stream)
 > stream: File-Stream */
#define test_file_stream_named(stream)  \
  do { if (nullp(TheStream(stream)->strm_file_truename)) \
         fehler_file_stream_unnamed(stream);             \
  } while(0)
nonreturning_function(local, fehler_file_stream_unnamed, (object stream)) {
  pushSTACK(stream); /* FILE-ERROR slot PATHNAME */
  pushSTACK(stream);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(file_error,GETTEXT("~S: filename for ~S is unknown"));
}

#if defined(UNIX) || defined(WIN32_NATIVE)

#ifdef UNIX
  #define slash  '/'
#endif
#ifdef WIN32_NATIVE
  #define slash  '\\'
#endif
/* physical slash */
#ifdef PATHNAME_WIN32
 #define pslashp(c)  (chareq(c,ascii('\\')) || chareq(c,ascii('/')))
 #define cpslashp(c) ((c) == '\\' || (c) == '/')
#else /* PATHNAME_UNIX */
 #define pslashp(c)  chareq(c,ascii(slash))
 #define cpslashp(c) ((c) == slash)
#endif
#define colonp(c)    chareq(c,ascii(':'))
#ifndef LOGICAL_PATHNAMES
#define lslashp(c)   pslashp(c)
#endif
#define dotp(c)      chareq(c,ascii('.'))
#define starp(c)     chareq(c,ascii('*'))

/* UP: add a character to an ASCII string and return as a Lisp string
 can trigger GC */
#ifdef UNICODE
local /*maygc*/ object asciz_add_char (const char* chars, uintL len, char ch,
                                       object encoding)
#else
#define asciz_add_char(chars,len,ch,encoding)  asciz_add_char_(chars,len,ch)
local /*maygc*/ object asciz_add_char_ (const char* chars, uintL len, char ch)
#endif
{
  #ifdef UNICODE
  GCTRIGGER1(encoding);
  #else
  GCTRIGGER();
  #endif
  var DYNAMIC_ARRAY(buf,char,len+1);
  begin_system_call(); memcpy(buf,chars,len); end_system_call();
  buf[len] = ch;
  var object s = n_char_to_string(buf,len+1,encoding);
  FREE_DYNAMIC_ARRAY(buf);
  return s;
}

/* UP: Converts a Unix-Directory-Specification into a pathname.
 asciz_dir_to_pathname(path,encoding)
 > const char* path: path as ASCIZ-String
 > encoding: Encoding
 < result: as a pathname without name and type
 can trigger GC */
#ifdef UNICODE
local /*maygc*/ object asciz_dir_to_pathname(const char* path, object encoding)
#else
#define asciz_dir_to_pathname(path,encoding)  asciz_dir_to_pathname_(path)
local /*maygc*/ object asciz_dir_to_pathname_(const char* path)
#endif
{
  #ifdef UNICODE
  GCTRIGGER1(encoding);
  #else
  GCTRIGGER();
  #endif
  var object pathname;
  var uintL len = asciz_length(path); /* string length */
  /* if the String does not end with a '/' already, a '/' is added: */
  if ((len>0) && cpslashp(path[len-1]))
    pathname = n_char_to_string(path,len,encoding);
  else
    pathname = asciz_add_char(path,len,slash,encoding);
  /* and convert into a pathname: */
  return coerce_pathname(pathname);
}

#endif

/* Type for PARSE-NAMESTRING:
 State while the string is being parsed character by character. */
typedef struct {
  uintL index;    /* index (incl. offset) */
  object FNindex; /* index as a fixnum */
  uintL count;    /* number of the remaining characters */
} zustand;        /* "state" */

/* Skip s characters. */
#define Z_SHIFT(z,s) \
 do { (z).index += (s); (z).FNindex = fixnum_inc((z).FNindex,(s)); (z).count -= (s); } while(0)

/* Tests whether the current character at Z satisfies pred. */
#define Z_AT_SLASH(z,pred,st) \
  (((z).count != 0) && pred(schar(st,(z).index)))

/* Replace this string with a substring. */
#define Z_SUB(z,s) ((s) = subsstring((s),(z).index,(z).index+(z).count), (z).index = 0)

#ifdef LOGICAL_PATHNAMES

/* Parsing of logical pathnames. */

/* separator between subdirs */
#define semicolonp(c)  (chareq(c,ascii(';')))
#define lslashp(c)     semicolonp(c)

/* Copy LEN characters in string ORIG starting at ORIG_OFFSET to string DEST,
   starting at DEST_OFFSET, up-casing all characters. LEN is > 0. */
local void copy_upcase (object dest, uintL dest_offset,
                        object orig, uintL orig_offset, uintL len) {
  sstring_un_realloc(orig);
  SstringDispatch(orig,X1, {
    var cintX1* ptr1 = &((SstringX1)TheVarobject(orig))->data[orig_offset];
    sstring_un_realloc(dest);
    SstringDispatch(dest,X2, {
      var cintX2* ptr2 = &((SstringX2)TheVarobject(dest))->data[dest_offset];
      dotimespL(len,len, { *ptr2++ = as_cint(up_case(as_chart(*ptr1++))); });
    });
  });
}

/* Parses the name/type/version part (if subdirp=false) or a subdir part
 (if subdirp=true) of a logical pathname.
 parse_logical_word(&z,subdirp)
 > STACK_2: storage vector, a normal-simple-string
 > zustand z: start state
 < zustand z: updated
 < result: a normal-simple-string or :WILD or :WILD-INFERIORS or NIL
 can trigger GC */
local maygc object parse_logical_word (zustand* z, bool subdirp) {
  ASSERT(sstring_normal_p(STACK_2));
  var zustand startz = *z; /* start-state */
  var chart ch;
  /* Is there a sequence of alphanumeric characters or '*',
   no two '*' adjacent (except "**", if subdirp),
   and, if subdirp, a ';' ? */
  var bool last_was_star = false;
  var bool seen_starstar = false;
  while (z->count) {
    ch = schar(STACK_2,z->index); /* next character */
    if (!legal_logical_word_char(ch)) {
      if (starp(ch)) {
        if (last_was_star) {
          if (subdirp && (z->index - startz.index == 1))
            seen_starstar = true;
          else
            break; /* adjacent '*' are forbidden */
        } else
          last_was_star = true;
      } else
        break;
    }
    /* skip character: */
    Z_SHIFT(*z,1);
  }
  var uintL len = z->index - startz.index;
  if (subdirp) {
    if ((z->count == 0) || !lslashp(ch)) {
      *z = startz; return NIL; /* no ';' -> no subdir */
    }
    /* skip character ';' : */
    Z_SHIFT(*z,1);
  }
  if (len==0)
    return NIL;
  else if ((len==1) && starp(schar(STACK_2,startz.index)))
    return S(Kwild);
  else if ((len==2) && seen_starstar)
    return S(Kwild_inferiors);
  else {
    var object result = allocate_string(len);
    copy_upcase(result,0,STACK_2,startz.index,len);
    return result;
  }
}

/* Test whether a string is a digit sequence.
 all_digits(string)
 > string: a normal-simple-string
 < true if the string consists entirely of digits, else false */
local bool all_digits (object string) {
  var uintL len = Sstring_length(string);
  if (len > 0) {
    var object storage = string; sstring_un_realloc(storage);
    SstringDispatch(storage,X, {
      var const cintX* ptr = &((SstringX)TheVarobject(storage))->data[0];
      dotimespL(len,len, {
        var cintX c = *ptr++;
        if (!((c >= '0') && (c <= '9')))
          return false;
      });
    });
  }
  return true;
}

/* test whether the string contains semicolons (and the rest being valid!),
 thus appearing to be a logical pathname
 > string: storage vector, a normal-simple-string
 < result: true if the string contains semicolons */
local bool looks_logical_p (object string) {
  var uintL len = Sstring_length(string);
  var bool logical_p = false;
  if (len > 0) {
    SstringDispatch(string,X, {
      var const cintX* charptr = &((SstringX)TheVarobject(string))->data[0];
      do {
        var chart ch = up_case(as_chart(*charptr++));
        if (!legal_logical_word_char(ch)) {
          if (semicolonp(ch))
            logical_p = true;
          else if (!colonp(ch) && !dotp(ch) && !starp(ch))
            return false; /* invalid logical pathname char */
        }
      } while (--len);
    });
  }
  return logical_p;
}

/* Attempt to parse a logical host name string, starting at a given state.
 parse_logical_host_prefix(&z,string)
 > string: storage vector, a normal-simple-string
 > state z: start state
 < state z: updated to point past the colon after the logical host
 < result: logical host, or NIL
 can trigger GC */
local maygc object parse_logical_host_prefix (zustand* zp, object string) {
  ASSERT(sstring_normal_p(string));
  var object host;
  var uintL startindex = zp->index;
  var chart ch;
  /* a sequence of alphanumeric characters and then ':' */
  loop {
    if (zp->count==0)
      return NIL; /* string already ended -> no host */
    ch = schar(string,zp->index); /* next character */
    if (!legal_logical_word_char(ch))
      break;
    /* go past alphanumeric character: */
    Z_SHIFT(*zp,1);
  }
  if (!colonp(ch))
    return NIL; /* no ':' -> no host */
  { /* make host-string: */
    var uintL len = zp->index - startindex;
    pushSTACK(string);
    host = allocate_string(len);
    string = popSTACK();
    /* and fill it: */
    if (len > 0)
      copy_upcase(host,0,string,startindex,len);
  }
  /* skip ':' */
  Z_SHIFT(*zp,1);
  return host;
}

/* CLHS for MAKE-PATHNAME: "Whenever a pathname is constructed the
 components may be canonicalized if appropriate."
 simplify the subdirectory list
 strings are coerced to normal simple strings
 the list should start with a valid startpoint (not checked!)
 > dir : pathname directory list
 < dir : the same list, destructively modified:
         ".." or :back         ==> :up
         ... x "foo" :up y ... ==> ... x y ...
         ... x  ""/"."   y ... ==> ... x y ...
         :absolute :up         ==> error
         :wild-inferiors :up   ==> error
 can trigger GC */
local maygc object simplify_directory (object dir) {
  if (!consp(dir)) return dir;
  DOUT("simplify_directory:< ",dir);
  pushSTACK(dir);
  { /* kill ".", ".."->:up, coerce to normal simple strings */
    var object curr = dir;
    while (consp(curr) && consp(Cdr(curr))) {
      var object next = Cdr(curr);
      var object here = Car(next);
      if (stringp(here)) {
        if (vector_length(here)==0 || string_equal(here,O(dot_string))) {
          Cdr(curr) = Cdr(next); /* drop "." and "" */
          continue;
        } else if (string_equal(here,O(wild_string))) {
          Car(next) = S(Kwild);
          curr = next;
          continue;
        } else if (string_equal(here,O(wildwild_string))) {
          Car(next) = S(Kwild_inferiors);
          curr = next;
          continue;
        } else if (!consp(next))
          break;
        if (string_equal(here,O(dotdot_string)))
          Car(next) = S(Kup); /* ".." --> :UP */
        else { /* coerce to normal */
          pushSTACK(next);
          var object element = coerce_normal_ss(here);
          next = popSTACK();
          Car(next) = element;
        }
      } else if (eq(here,S(Kback)))
        Car(next) = S(Kup); /* :BACK --> :UP (ANSI) */
      curr = next;
    }
  }
  dir = popSTACK();
  /* collapse "foo/../" (quadratic algorithm) */
  var bool changed_p;
  do {
    changed_p = false;
    var object curr = dir;
    while (consp(curr) && consp(Cdr(curr))) {
      var object next = Cdr(curr);
      var object here = Car(next);
      var object next_next = Cdr(next);
      if (consp(next_next)) {
        var object next_here = Car(next_next);
        /* :BACK has been converted to :UP */
        if (!eq(here,S(Kup)) && eq(next_here,S(Kup))) {
          if (eq(here,S(Kwild_inferiors)) || eq(here,S(Kabsolute))) {
            goto error_absolute_up;
          } else {
            Cdr(curr) = Cdr(next_next); /* collapse ( "foo" :UP ) */
            changed_p = true;
          }
        } else
          curr = next;
      } else
        curr = next;
    }
  } while (changed_p);
  if (eq(Car(dir),S(Kabsolute)) && consp(Cdr(dir)))
    if (eq(Car(Cdr(dir)),S(Kup)))
      goto error_absolute_up;
  DOUT("simplify_directory:> ",dir);
  return dir;
 error_absolute_up:
  /* <http://www.lisp.org/HyperSpec/Body/sec_19-2-2-4-3.html> */
  pushSTACK(O(empty_string)); /* FILE-ERROR slot PATHNAME */
  pushSTACK(dir); pushSTACK(S(Kdirectory));
  pushSTACK(TheSubr(subr_self)->name);
  fehler(file_error,GETTEXT("~S: illegal ~S argument ~S"));
}

/* Parses a logical pathname.
 parse_logical_pathnamestring(z)
 > STACK_1: storage vector, a normal-simple-string
 > STACK_0: freshly allocated logical pathname
 > state z: start state
 < STACK_0: same logical pathname, filled
 < result: number of remaining characters
 can trigger GC */
local maygc uintL parse_logical_pathnamestring (zustand z) {
  DOUT("parse_logical_pathnamestring:<0",STACK_0);
  DOUT("parse_logical_pathnamestring:<1",STACK_1);
  { /* parse Host-Specification: */
    var zustand startz = z;
    var object host = parse_logical_host_prefix(&z,STACK_1);
    if (nullp(host)) {
      z = startz; /* back to the start */
      host = STACK_(3+2); /* Default-Host */
    } else { /* enter host: */
      TheLogpathname(STACK_0)->pathname_host = host;
    }
  }
  { /* enter Directory-Start: */
    var object new_cons = allocate_cons(); /* new Cons for Startpoint */
    TheLogpathname(STACK_0)->pathname_directory = new_cons;
    pushSTACK(new_cons); /* new (last (pathname-directory Pathname)) */
  }
  /* stack layout:
   data-vector, pathname, (last (pathname-directory Pathname)).
   parse subdirectories:
   If ";" is the first char, it is turned into :RELATIVE
   (otherwise :ABSOLUTE) as the first subdir
   for a reason that escapes me, ANSI CL specifies that
   "foo:;bar;baz.zot" is a  :RELATIVE logical pathname while
   "foo:/bar/baz.zot" is an :ABSOLUTE physical pathname.
   see "19.3.1.1.3 The Directory part of a Logical Pathname Namestring"
   http://www.lisp.org/HyperSpec/Body/sec_19-3-1-1-3.html */
  if (Z_AT_SLASH(z,lslashp,STACK_2)) {
    Z_SHIFT(z,1);
    Car(STACK_0) = S(Krelative);
  } else {
    Car(STACK_0) = S(Kabsolute);
  }
  loop {
    /* try to parse the next subdir */
    var object subdir = parse_logical_word(&z,true);
    if (nullp(subdir))
      break;
    /* lengthen (pathname-directory pathname) by Subdir: */
    pushSTACK(subdir);
    var object new_cons = allocate_cons(); /* new Cons */
    Car(new_cons) = popSTACK(); /* = (cons subdir NIL) */
    Cdr(STACK_0) = new_cons; /* lengthens (pathname-directory Pathname) */
    STACK_0 = new_cons; /* new (last (pathname-directory Pathname)) */
  }
  { /* parse Name: */
    var object name = parse_logical_word(&z,false);
    TheLogpathname(STACK_1)->pathname_name = name;
    if ((z.count > 0) && dotp(schar(STACK_2,z.index))) {
      var zustand z_name = z;
      /* skip Character '.' : */
      Z_SHIFT(z,1);
      /* parse Type: */
      var object type = parse_logical_word(&z,false);
      TheLogpathname(STACK_1)->pathname_type = type;
      if (!nullp(type)) {
        if ((z.count > 0) && dotp(schar(STACK_2,z.index))) {
          var zustand z_type = z;
          /* skip Character '.' : */
          Z_SHIFT(z,1);
          /* parse Version: */
          var object version = parse_logical_word(&z,false);
          if (eq(version,S(Kwild))) {
          } else if (equal(version,Symbol_name(S(Knewest)))) {
            version = S(Knewest);
          } else if (stringp(version) && all_digits(version)) {
            pushSTACK(version); funcall(L(parse_integer),1);
            version = value1; /* version: string -> integer */
          } else {
            version = NIL;
          }
          TheLogpathname(STACK_1)->pathname_version = version;
          if (nullp(version))
            z = z_type; /* restore character '.' */
        } else {
          TheLogpathname(STACK_1)->pathname_version = NIL;
        }
      } else {
        z = z_name; /* restore character '.' */
        TheLogpathname(STACK_1)->pathname_version = NIL;
      }
    } else {
      TheLogpathname(STACK_1)->pathname_type = NIL;
      TheLogpathname(STACK_1)->pathname_version = NIL;
    }
  }
  skipSTACK(1);
  TheLogpathname(STACK_0)->pathname_directory =
    simplify_directory(TheLogpathname(STACK_0)->pathname_directory);
  DOUT("parse_logical_pathnamestring:>0",STACK_0);
  DOUT("parse_logical_pathnamestring:>1",STACK_1);
  return z.count;
}

/* recognition of a logical host, cf. CLtL2 p. 631
 (defun logical-host-p (host)
   (and (simple-string-p host)
        (gethash host sys::*logical-pathname-translations*) ; :test #'equalp !
        t)) */
local bool logical_host_p (object host) {
  return (simple_string_p(host)
          /* No need to string-upcase host, because it's tested via EQUALP. */
          && !eq(gethash(host,Symbol_value(S(logpathname_translations)),false),
                 nullobj));
}

#endif

#define string2wild(str)  (equal(str,O(wild_string)) ? S(Kwild) : (object)(str))
#define wild2string(obj)  (eq(obj,S(Kwild)) ? (object)O(wild_string) : (obj))

#ifdef PATHNAME_NOEXT
/* can trigger GC */
local maygc void fix_parse_namestring_dot_file (void)
{ /* make sure *PARSE-NAMESTRING-DOT-FILE* is valid */
  Symbol_value(S(parse_namestring_dot_file)) = S(Ktype); /*CLISP default*/
  pushSTACK(NIL);
  pushSTACK(S(parse_namestring_dot_file));
  pushSTACK(S(parse_namestring_dot_file));
  pushSTACK(Symbol_value(S(parse_namestring_dot_file)));
  STACK_3 = CLSTEXT("The variable ~S had an illegal value.\n"
                    "~S has been reset to ~S.");
  funcall(S(warn),4);
}

/* auxiliary function for PARSE-NAMESTRING:
 splits a string (at the last dot) into Name and Type.
 split_name_type(skip);
 > STACK_0: Normal-Simple-String
 > skip: 1 if a dot at the beginning should not trigger the splitting, else 0
 < STACK_1: Name
 < STACK_0: Type
 decrements STACK by 1
 can trigger GC */
local maygc void split_name_type (uintL skip) {
  if (skip == 0) {
    if (eq(Symbol_value(S(parse_namestring_dot_file)),S(Ktype))) { /* OK */
    } else if (eq(Symbol_value(S(parse_namestring_dot_file)),S(Kname))) {
      skip = 1; /* always have a name! */
    } else
      fix_parse_namestring_dot_file();
  }
  var object string = STACK_0;
  var uintL length = Sstring_length(string);
  /* Search for the last dot: */
  var uintL index = length;
  if (index > skip) {
    SstringDispatch(string,X, {
      var const cintX* ptr = &((SstringX)TheVarobject(string))->data[index];
      do {
        if (*--ptr == '.') goto punkt;
        index--;
      } while (index > skip);
    });
  }
  /* no dot found -> Type := NIL */
  pushSTACK(NIL);
  goto name_type_ok;
 punkt: /* dot found at index */
  /* type := (substring string index) */
  pushSTACK(subsstring(string,index,length));
  /* name := (substring string 0 (1- index)) */
  STACK_1 = subsstring(STACK_1,0,index-1);
 name_type_ok:
  STACK_0 = string2wild(STACK_0);
  STACK_1 = string2wild(STACK_1);
}
#endif

/* (PARSE-NAMESTRING thing [host [defaults [:start] [:end] [:junk-allowed]]]),
 CLTL p. 414 */
LISPFUN(parse_namestring,seclass_read,1,2,norest,key,3,
        (kw(start),kw(end),kw(junk_allowed)) ) {
  /* stack layout: thing, host, defaults, start, end, junk-allowed. */
  var bool junk_allowed;
  var bool parse_logical = false;
  DOUT("parse-namestring:[thng]",STACK_5);
  DOUT("parse-namestring:[host]",STACK_4);
  DOUT("parse-namestring:[dflt]",STACK_3);
  DOUT("parse-namestring:[beg]",STACK_2);
  DOUT("parse-namestring:[end]",STACK_1);
  DOUT("parse-namestring:[junk]",STACK_0);
  { /* 1. check junk-allowed: */
    var object obj = popSTACK(); /* junk-allowed-Argument */
    junk_allowed = !missingp(obj);
  }
  /* stack layout: thing, host, defaults, start, end.
   2. default-value for start is 0: */
  if (!boundp(STACK_1))
    STACK_1 = Fixnum_0;
  /* 3. check host: */
 #if HAS_HOST || defined(LOGICAL_PATHNAMES)
  {
    var object host = STACK_3;
   #if HAS_HOST
    host = test_optional_host(host,false);
   #else
    host = test_optional_host(host);
   #endif
    if (nullp(host)) {
      /* host := (PATHNAME-HOST defaults) */
      var object defaults = test_default_pathname(STACK_2);
     #ifdef LOGICAL_PATHNAMES
      if (logpathnamep(defaults))
        parse_logical = true;
     #endif
      host = xpathname_host(parse_logical,defaults);
    } else {
     #ifdef LOGICAL_PATHNAMES
      if (logical_host_p(host)) {
        parse_logical = true; host = string_upcase(host);
      }
     #endif
    }
    STACK_3 = host;
  }
 #else
  test_optional_host(STACK_3);
 #endif
  /* 4. thing must be a String: */
  DOUT("parse-namestring:[thng]",STACK_4);
  DOUT("parse-namestring:[host]",STACK_3);
  DOUT("parse-namestring:[dflt]",STACK_2);
  var object thing = STACK_4;
  if (xpathnamep(thing)) { /* Pathname? */
    value1 = thing; /* 1. value thing */
  fertig:
    DOUT("parse-namestring:[fertig]",value1);
    value2 = STACK_1; mv_count=2; /* 2. value start */
    skipSTACK(5); return;
  }
  if (builtin_stream_p(thing)) { /* Stream? */
    thing = as_file_stream(thing);
    test_file_stream_named(thing);
    value1 = TheStream(thing)->strm_file_name; /* 1. value: Filename */
    goto fertig; /* 2. value like above */
  }
  /* thing should now be at least a String or a Symbol: */
  var bool thing_symbol = false;
  if (!stringp(thing)) {
    if (!symbolp(thing) || !nullpSv(parse_namestring_ansi))
      fehler_pathname_designator(thing);
    thing = Symbol_name(thing); /* Symbol -> use symbol name */
    thing_symbol = true;
    STACK_4 = thing; /* and write back into the Stack */
  }
  /* thing = STACK_4 is now a String.
   it will be traversed. */
  var zustand z; /* running state */
  {
    var object string; /* String thing */
    { /* check boundaries, with thing, start, end as arguments: */
      var stringarg arg;
      pushSTACK(thing); pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
      test_string_limits_ro(&arg);
      string = arg.string;
      z.index = arg.offset+arg.index; /* z.index = start-argument, */
      z.count = arg.len;           /* z.count = number of characters. */
      z.FNindex = fixnum(arg.index); /* z.FNindex = start-Index as Fixnum. */
    }
   #ifdef LOGICAL_PATHNAMES
    if (!parse_logical) {
      /* Check whether *PARSE-NAMESTRING-ANSI* is true and the string
       starts with a logical hostname. */
      if (!nullpSv(parse_namestring_ansi)) {
        /* Coerce string to be a normal-simple-string. */
        #ifdef HAVE_SMALL_SSTRING
        SstringCase(string,{ Z_SUB(z,string); },{ Z_SUB(z,string); },{},{ Z_SUB(z,string); });
        #endif
        pushSTACK(string);
        var zustand tmp = z;
        var object host = parse_logical_host_prefix(&tmp,string);
        string = popSTACK();
        DOUT("parse-namestring:",string);
        DOUT("parse-namestring:",host);
        if (!nullp(host)
            /* Test whether the given hostname is valid. This is not
             strictly what ANSI specifies, but is better than giving
             an error for Win32 pathnames like "C:\\FOOBAR". */
            && logical_host_p(host))
          parse_logical = true;
        else
          /* ANSI CL specifies that we should look at the entire string, using
           parse_logical_pathnamestring, not only parse_logical_host_prefix. */
          parse_logical = looks_logical_p(string);
      }
    }
   #endif
    if (thing_symbol && !parse_logical) {
     #if defined(PATHNAME_UNIX) || defined(PATHNAME_WIN32)
      /* operating system with preference for small letters */
      Z_SUB(z,string); /* yes -> convert with STRING-DOWNCASE */
      pushSTACK(string);
      nstring_downcase(string,0,Sstring_length(string));
      string = popSTACK();
      sstring_un_realloc(string);
     #endif
    }
    /* Coerce string to be a normal-simple-string. */
    #ifdef HAVE_SMALL_SSTRING
    SstringCase(string,{ Z_SUB(z,string); },{ Z_SUB(z,string); },{},{ Z_SUB(z,string); });
    #endif
    pushSTACK(string);
  }
 #ifdef LOGICAL_PATHNAMES
  if (parse_logical) {
    pushSTACK(allocate_logpathname());
    /* stack layout: ..., data-vector, pathname. */
    var uintL remaining = parse_logical_pathnamestring(z);
    z.index += z.count-remaining; z.FNindex = fixnum_inc(z.FNindex,z.count-remaining); z.count = remaining;
  } else
 #endif
  {
    pushSTACK(allocate_pathname());
    /* stack layout: ..., data-vector, pathname.
     separator between subdirs is on WIN32 both '\' and '/': */
   #if HAS_HOST
    { /* parse Host-Specification: */
      var object host;
      {
        var zustand startz = z; /* start-state */
        var chart ch;
       #if defined(PATHNAME_WIN32)
        /* Look for two slashes, then a sequence of characters. */
        if (z.count==0) goto no_hostspec;
        ch = TheSnstring(STACK_1)->data[z.index];
        if (!pslashp(ch)) goto no_hostspec;
        Z_SHIFT(z,1);
        if (z.count==0) goto no_hostspec;
        ch = TheSnstring(STACK_1)->data[z.index];
        if (!pslashp(ch)) goto no_hostspec;
        Z_SHIFT(z,1);
        while (z.count) {
          ch = TheSnstring(STACK_1)->data[z.index];
          if (!legal_hostchar(ch))
            break;
          /* Skip past valid host char. */
          Z_SHIFT(z,1);
        }
        /* Create host string. */
        if (z.index - startz.index - 2 == 0)
          goto no_hostspec;
        host = subsstring(STACK_1,startz.index+2,z.index);
        /* Note: The next character in the string is not a letter or '*';
         therefore the device of the resulting pathname will be NIL. */
        goto hostspec_ok;
       #else
        /* is it a sequence of alphanumeric characters and then a ':' resp. '::' ? */
        loop {
          if (z.count==0)
            goto no_hostspec; /* string already through -> no Host */
          ch = TheSnstring(STACK_1)->data[z.index]; /* next character */
          if (!alphanumericp(ch))
            break;
          /* skip alphanumeric character: */
          Z_SHIFT(z,1);
        }
        if (!colonp(ch))
          goto no_hostspec; /* no ':' -> no host */
        /* build host-string: */
        host = subsstring(STACK_1,startz.index,z.index);
        /* skip character ':' : */
        Z_SHIFT(z,1);
        goto hostspec_ok;
       #endif
      no_hostspec: /* no host-specification */
        z = startz; /* back to start */
        host = STACK_(3+2); /* Default-Host */
      }
    hostspec_ok: /* enter host: */
      ThePathname(STACK_0)->pathname_host = host;
    }
   #endif /* HAS_HOST */
   #if HAS_DEVICE
    #ifdef PATHNAME_WIN32
    { /* parse one-letter Device-Specification: */
      var object device = NIL; /* Device := NIL */
      /* parse Drive-Specification:
       Is there a letter ('*','A'-'Z','a'-'z') and then a ':' ? */
      {
        var zustand startz = z; /* start-state */
        var chart ch;
        if (z.count==0)
          goto no_drivespec; /* string already through ? */
        ch = TheSnstring(STACK_1)->data[z.index]; /* next character */
        ch = up_case(ch); /* as capital letter */
        if (starp(ch)) {
          /* ch = '*' -> Device := :WILD */
          device = S(Kwild);
        } else if ((as_cint(ch) >= 'A') && (as_cint(ch) <= 'Z')) {
          /* 'A' <= ch <= 'Z' -> Device := "ch" */
          var object string = allocate_string(1); /* String of length 1 */
          TheSnstring(string)->data[0] = ch; /* with ch as sole letter */
          device = string;
        } else
          goto no_device;
        /* Device OK, skip character: */
        Z_SHIFT(z,1);
        if (z.count==0)
          goto no_drivespec; /* string already through ? */
        ch = TheSnstring(STACK_1)->data[z.index]; /* next character */
        ch = up_case(ch); /* as capital letter */
      no_device:
        /* concluded with colon? */
        if (!colonp(ch))
          goto no_drivespec;
        /* skip character: */
        Z_SHIFT(z,1);
        goto drivespec_ok;
      no_drivespec:
        /* parsing a Drive-Specification did not succeed. */
        z = startz; /* restore start-state */
        device = NIL; /* Device := NIL */
      }
    drivespec_ok: /* enter Device */
      ThePathname(STACK_0)->pathname_device = device;
    }
    #endif /* PATHNAME_WIN32 */
   #endif /* HAS_DEVICE */
    /* enter Directory-Start: */
    ThePathname(STACK_0)->pathname_directory = NIL;
    pushSTACK(NIL); /* new (last (pathname-directory Pathname)) */
    /* stack layout:
     ..., Datenvektor, Pathname, (last (pathname-directory Pathname)).
     parse subdirectories: */
    {
     #if defined(USER_HOMEDIR) && defined(PATHNAME_UNIX)
      /* if there is a '~' immediately, a username is read up to the next '/'
       or string-end and the Home-Directory of this user is inserted: */
      if ((z.count != 0) && chareq(schar(STACK_2,z.index),ascii('~'))) {
        /* there is a '~' immediately.
         skip character: */
        Z_SHIFT(z,1);
        var object userhomedir; /* Pathname of the User-Homedir */
        /* search next '/' : */
        var uintL charcount = 0;
        if (z.count > 0) {
          SstringDispatch(STACK_2,X, {
            var const cintX* charptr =
              &((SstringX)TheVarobject(STACK_2))->data[z.index];
            var uintL count;
            dotimespL(count,z.count, {
              if (*charptr++ == '/') break;
              charcount++;
            });
          });
        }
        /* Username has charcount characters */
        if (charcount==0) {
          userhomedir = O(user_homedir); /* only '~' -> User-Homedir */
        } else { /* build username: */
          var object username =
            subsstring(STACK_2,z.index,z.index+charcount);
          /* fetch his/her Home-Directory from the password-file: */
          with_sstring_0(username,O(misc_encoding),username_asciz, {
            begin_system_call();
            errno = 0;
            var struct passwd * userpasswd = getpwnam(username_asciz);
            if (userpasswd == (struct passwd *)NULL) { /* unsuccessful? */
              if (!(errno==0)) { OS_error(); } /* report error */
              end_system_call();
              /* else: error */
              pushSTACK(username);
              pushSTACK(S(parse_namestring));
              fehler(parse_error,GETTEXT("~S: there is no user named ~S"));
            }
            end_system_call();
            userhomedir = /* homedir as pathname */
              asciz_dir_to_pathname(userpasswd->pw_dir,O(misc_encoding));
          });
        }
        /* copy directory from the pathname userhomedir:
         (copy-list dir) = (nreconc (reverse dir) nil),
         after it memorize its last Cons. */
        userhomedir = reverse(ThePathname(userhomedir)->pathname_directory);
        userhomedir = nreconc(userhomedir,NIL);
        ThePathname(STACK_1)->pathname_directory = userhomedir;
        while (mconsp(Cdr(userhomedir))) { userhomedir = Cdr(userhomedir); }
        STACK_0 = userhomedir;
        /* skip username-characters: */
        Z_SHIFT(z,charcount);
        /* if the string is through: finished,
         otherwise a '/' follows immediately , it will be skipped: */
        if (z.count==0) { /* Name and Type := NIL */
          pushSTACK(NIL); pushSTACK(NIL); goto after_name_type;
        }
        /* skip character: */
        Z_SHIFT(z,1);
      } else
     #endif /* USER_HOMEDIR & PATHNAME_UNIX */
     #if defined(PATHNAME_UNIX) && 0
        /* What is this needed for, except for $HOME ?
         If a '$' follows immediately, an Environment-Variable is read up
         to the next '/' or string-end and its value is inserted: */
        if ((z.count != 0)
            && chareq(TheSnstring(STACK_2)->data[z.index],ascii('$'))) {
          /* A '$' follows immediately.
           skip character: */
          Z_SHIFT(z,1);
          var object envval_dir;
          /* search next '/' : */
          var uintL charcount = 0;
          {
            var const chart* charptr = &TheSnstring(STACK_2)->data[z.index];
            var uintL count;
            dotimesL(count,z.count, {
              if (chareq(*charptr++,ascii('/')))
                break;
              charcount++;
            });
          }
          { /* Environment-Variable has charcount characters. */
            var object envvar =
              subsstring(STACK_2,z.index,z.index+charcount);
            /* fetch its value: */
            with_sstring_0(envvar,O(misc_encoding),envvar_asciz, {
              begin_system_call();
              var const char* envval = getenv(envvar_asciz);
              end_system_call();
              if (envval==NULL) {
                pushSTACK(envvar);
                pushSTACK(S(parse_namestring));
                fehler(parse_error,
                       GETTEXT("~S: there is no environment variable ~S"));
              }
              envval_dir = /* value of the variable as pathname */
                asciz_dir_to_pathname(envval,O(misc_encoding));
            });
          }
          /* copy directory from the pathname envval_dir:
           (copy-list dir) = (nreconc (reverse dir) nil),
           afterwards memorize its last Cons. */
          envval_dir = reverse(ThePathname(envval_dir)->pathname_directory);
          envval_dir = nreconc(envval_dir,NIL);
          ThePathname(STACK_1)->pathname_directory = envval_dir;
          while (mconsp(Cdr(envval_dir))) { envval_dir = Cdr(envval_dir); }
          STACK_0 = envval_dir;
          /* skip envvar-characters: */
          Z_SHIFT(z,charcount);
          /* if the string is through: finished,
           otherwise a '/' follows immediately , it will be skipped: */
          if (z.count==0) { /* Name and Type := NIL */
            pushSTACK(NIL); pushSTACK(NIL); goto after_name_type;
          }
          /* skip character: */
          Z_SHIFT(z,1);
        } else
     #endif /* PATHNAME_UNIX & 0 */
     #if defined(PATHNAME_UNIX) || defined(PATHNAME_WIN32)
      #if defined(UNIX_CYGWIN32)
        if (z.count > 1 && !nullpSv(device_prefix)
            && colonp(schar(STACK_2,z.index+1))) {
          /* if string starts with 'x:', treat it as a device */
          var chart ch = down_case(schar(STACK_2,z.index));
          if ((as_cint(ch) >= 'a') && (as_cint(ch) <= 'z')) {
            pushSTACK(allocate_string(1)); /* drive */
            TheSnstring(STACK_0)->data[0] = ch;
            var object new_cons = allocate_cons();
            Car(new_cons) = popSTACK(); /* drive */
            ThePathname(STACK_1)->pathname_directory = new_cons;
            STACK_0 = new_cons;
            Z_SHIFT(z,2);
            if (Z_AT_SLASH(z,pslashp,STACK_2)) Z_SHIFT(z,1);
          } else goto continue_parsing_despite_colon;
        } else
        continue_parsing_despite_colon:
      #endif
        /* if 1st char is a slash, start with :ABSOLUTE (otherwise :RELATIVE): */
        if (Z_AT_SLASH(z,pslashp,STACK_2)) {
          Z_SHIFT(z,1);
          var object new_cons = allocate_cons();
          Car(new_cons) = S(Kabsolute);
          ThePathname(STACK_1)->pathname_directory = new_cons;
          STACK_0 = new_cons;
        }
     #endif
      loop {
        /* try to parse another subdirectory. */
       #ifdef PATHNAME_NOEXT
        {
          var uintL z_start_index = z.index; /* index at the start */
          loop {
            var chart ch;
            if (z.count == 0)
              break;
            ch = schar(STACK_2,z.index); /* next character */
            if (!legal_namechar(ch)) /* valid character ? */
              break;
            /* yes -> part of the name
             skip character: */
            Z_SHIFT(z,1);
          }
          /* reached end of the name.
           Name := substring of STACK_2 from z_start_index (inclusive)
                                          to z.index (exclusive). */
          var object string = subsstring(STACK_2,z_start_index,z.index);
          /* name finished. */
          pushSTACK(string);
        }
        /* if a '/' resp. '\' follows immediately, then it was a subdirectory,
         else the pathname is finished: */
        if (!Z_AT_SLASH(z,pslashp,STACK_3))
          /* no -> it was the name and no subdir. */
          break;
        /* a '/' resp. '\' follows. skip character: */
        Z_SHIFT(z,1);
        /* stack layout: ...,
           data-vector, pathname, (last (pathname-directory Pathname)),
           subdir. */
        /* was it '**' or '...' ? */
        if (equal(STACK_0,O(wildwild_string))
            || equal(STACK_0,O(dotdotdot_string))) {
          STACK_0 = S(Kwild_inferiors); /* replace with :WILD-INFERIORS */
        }
       #endif /* PATHNAME_NOEXT */
        if (nullp(STACK_1)) {
          var object new_cons = allocate_cons();
          Car(new_cons) = S(Krelative);
          ThePathname(STACK_2)->pathname_directory = new_cons;
          STACK_1 = new_cons;
        }
        /* lengthen (pathname-directory pathname) by subdir STACK_0: */
        var object new_cons = allocate_cons(); /* new Cons */
        Car(new_cons) = popSTACK(); /* = (cons subdir NIL) */
        Cdr(STACK_0) = new_cons; /* lengthened (pathname-directory Pathname) */
        STACK_0 = new_cons; /* new (last (pathname-directory Pathname)) */
      }
     #ifdef PATHNAME_NOEXT
      /* stack layout: ..., data-vector, pathname,
                   (last (pathname-directory Pathname)), string. */
      split_name_type(0); /* split string STACK_0 in name and type */
    after_name_type:
      /* stack layout: ..., data-vector, pathname,
                   (last (pathname-directory Pathname)), name, type. */
      { /* enter name and type in pathname: */
        var object type = popSTACK();
        var object name = popSTACK();
        skipSTACK(1); /* directory is already entered */
        /* replace name="" with name=NIL: */
        if (equal(name,O(empty_string)))
          name = NIL;
        var object pathname = STACK_0;
        ThePathname(pathname)->pathname_name = name;
        ThePathname(pathname)->pathname_type = type;
      }
     #endif
     #ifdef WIN32_NATIVE
      var object pathname = STACK_0;
      var object dir = ThePathname(pathname)->pathname_directory;
      var object dev = Symbol_value(S(device_prefix));
      if (nullp(ThePathname(pathname)->pathname_device)
          /* actually, we already know that dir is a cons */
          && consp(dir) && eq(Car(dir),S(Kabsolute))
          /* Cdr(dir) might not be a cons, e.g., "/foo" ==
           #S(pathname :directory (:absolute) :name "foo") */
          && consp(Cdr(dir)) && consp(Cdr(Cdr(dir)))
          && stringp(dev)
          && string_eqcomp_ci(Car(Cdr(dir)),0,dev,0,vector_length(dev))) {
        /* path = (:ABSOLUTE "cygdrive" "drive" "dir1" ...) ===>
           path = (:ABSOLUTE "dir1" ...); device = "DRIVE" */
        var object device = Car(Cdr(Cdr(dir)));
        Cdr(dir) = Cdr(Cdr(Cdr(dir)));
        device = string_upcase(device);
        ThePathname(STACK_0)->pathname_device = device;
      }
     #endif
     #ifdef UNIX_CYGWIN32
      var object dir = ThePathname(STACK_0)->pathname_directory;
      if (consp(dir) && stringp(Car(dir))) {
        /* dir = ("c" ...) --> (:absolute *device-prefix* "c" ...)*/
        pushSTACK(S(Kabsolute));
        pushSTACK(Symbol_value(S(device_prefix)));
        dir = listof(2);
        Cdr(Cdr(dir)) = ThePathname(STACK_0)->pathname_directory;
        ThePathname(STACK_0)->pathname_directory = dir;
      }
     #endif
      ThePathname(STACK_0)->pathname_directory =
        simplify_directory(ThePathname(STACK_0)->pathname_directory);
    }
  }
  /* Pathname is finished.
   stack layout: ..., data-vector, pathname. */
  if (!junk_allowed)
    /* Check whether no more characters remain */
    if (!(z.count == 0)) {
      pushSTACK(z.FNindex); /* last index */
      pushSTACK(STACK_(4+2+1)); /* thing */
      pushSTACK(S(parse_namestring));
      fehler(parse_error,
             GETTEXT("~S: syntax error in filename ~S at position ~S"));
    }
 #if HAS_HOST || defined(LOGICAL_PATHNAMES)
  /* Check that if a :host argument (or :host component of the :defaults
   argument) was present and the parsed pathname has a host component,
   they agree; and set the :host component of the result otherwise */
  if (!missingp(STACK_(3+2))) {
   #ifdef LOGICAL_PATHNAMES
    if (parse_logical) {
      var object parsed_host = TheLogpathname(STACK_0)->pathname_host;
      if (!nullp(parsed_host)) {
        if (!equal(STACK_(3+2),parsed_host)) {
          pushSTACK(STACK_0);
          pushSTACK(parsed_host);
          pushSTACK(STACK_(3+2+2));
          pushSTACK(S(parse_namestring));
          fehler(error,GETTEXT("~S: hosts ~S and ~S of ~S should coincide"));
        }
      } else
        TheLogpathname(STACK_0)->pathname_host = STACK_(3+2);
    } else
   #endif
    {
     #if HAS_HOST
      var object parsed_host = ThePathname(STACK_0)->pathname_host;
      if (!nullp(parsed_host)) {
        if (!equal(STACK_(3+2),parsed_host)) {
          pushSTACK(STACK_0);
          pushSTACK(parsed_host);
          pushSTACK(STACK_(3+2+2));
          pushSTACK(S(parse_namestring));
          fehler(error,GETTEXT("~S: hosts ~S and ~S of ~S should coincide"));
        }
      } else
        ThePathname(STACK_0)->pathname_host = STACK_(3+2);
     #endif
    }
  }
 #endif /* HAS_HOST || LOGICAL_PATHNAMES */
  value1 = STACK_0; /* pathname as 1. value */
  value2 = z.FNindex; /* index as 2. value */
  mv_count=2; /* 2 values */
  DOUT("parse-namestring:[end ret]",value1);
  skipSTACK(5+2); return;
}
#undef colonp
#undef Z_SUB
#undef Z_AT_SLASH
#undef Z_SHIFT

/* UP: Converts an object into a pathname.
 coerce_xpathname(object)
 > object: object
 < result: (PATHNAME Objekt)
 can trigger GC */
local maygc object coerce_xpathname (object obj) {
  if (xpathnamep(obj)) {
    /* nothing to do for pathnames. */
    return obj;
  } else {
    /* else: call PARSE-NAMESTRING: */
    pushSTACK(obj); funcall(L(parse_namestring),1);
    return value1;
  }
}

LISPFUNNR(pathname,1) { /* (PATHNAME pathname), CLTL p. 413 */
  VALUES1(coerce_xpathname(popSTACK()));
}

/* (PATHNAME-HOST pathname [:case]), CLTL p. 417, CLtL2 p. 644 */
LISPFUN(pathnamehost,seclass_read,1,0,norest,key,1, (kw(case))) {
  var object pathname = coerce_xpathname(STACK_1);
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(pathname)) {
    VALUES1(TheLogpathname(pathname)->pathname_host);
  } else
 #endif
  {
   #if HAS_HOST
    var object erg = ThePathname(pathname)->pathname_host;
    VALUES1(eq(STACK_0,S(Kcommon)) ? common_case(erg) : erg); /* host as value */
   #else
    VALUES1(NIL);
   #endif
  }
  skipSTACK(2);
}

/* (PATHNAME-DEVICE pathname [:case]), CLTL p. 417, CLtL2 p. 644 */
LISPFUN(pathnamedevice,seclass_read,1,0,norest,key,1, (kw(case))) {
  var object pathname = coerce_xpathname(STACK_1);
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(pathname)) {
    /* http://www.lisp.org/HyperSpec/Body/sec_19-3-2-1.html */
    VALUES1(S(Kunspecific));
  } else
 #endif
  {
   #if HAS_DEVICE
    var object erg = ThePathname(pathname)->pathname_device; /* device as value */
    VALUES1(eq(STACK_0,S(Kcommon)) ? common_case(erg) : erg);
   #else
    VALUES1(NIL);
   #endif
  }
  skipSTACK(2);
}

/* (PATHNAME-DIRECTORY pathname [:case]), CLTL p. 417, CLtL2 p. 644 */
LISPFUN(pathnamedirectory,seclass_read,1,0,norest,key,1, (kw(case))) {
  var object pathname = coerce_xpathname(STACK_1);
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(pathname)) {
    VALUES1(TheLogpathname(pathname)->pathname_directory);
  } else
 #endif
  {
    var object erg = ThePathname(pathname)->pathname_directory;
    VALUES1(eq(STACK_0,S(Kcommon)) ? subst_common_case(erg) : erg);
  }
  skipSTACK(2);
}

/* (PATHNAME-NAME pathname [:case]), CLTL p. 417, CLtL2 p. 644 */
LISPFUN(pathnamename,seclass_read,1,0,norest,key,1, (kw(case))) {
  var object pathname = coerce_xpathname(STACK_1);
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(pathname)) {
    value1 = TheLogpathname(pathname)->pathname_name;
  } else
 #endif
  {
    var object erg = ThePathname(pathname)->pathname_name;
    value1 = (eq(STACK_0,S(Kcommon)) ? common_case(erg) : erg);
  }
  mv_count=1; /* name as value */
  skipSTACK(2);
}

/* (PATHNAME-TYPE pathname [:case]), CLTL p. 417, CLtL2 p. 644 */
LISPFUN(pathnametype,seclass_read,1,0,norest,key,1, (kw(case))) {
  var object pathname = coerce_xpathname(STACK_1);
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(pathname)) {
    value1 = TheLogpathname(pathname)->pathname_type;
  } else
 #endif
  {
    var object erg = ThePathname(pathname)->pathname_type;
    value1 = (eq(STACK_0,S(Kcommon)) ? common_case(erg) : erg);
  }
  mv_count=1; /* type as value */
  skipSTACK(2);
}

/* (PATHNAME-VERSION pathname), CLTL p. 417, CLtL2 p. 644 */
LISPFUNNR(pathnameversion,1) {
  var object pathname = coerce_xpathname(popSTACK());
  VALUES1(xpathname_version(logpathnamep(pathname),pathname));
}

#ifdef LOGICAL_PATHNAMES

/* Converts obj to a pathname. If obj is a string, it is even converted to a
   logical pathname.
 can trigger GC */
local maygc object parse_as_logical (object obj) {
  /* The value of (PARSE-NAMESTRING obj nil empty-logical-pathname) is always
     a logical pathname, if obj is a string. (But not if it is a stream!) */
  pushSTACK(obj); pushSTACK(NIL);
  pushSTACK(O(empty_logical_pathname));
  funcall(L(parse_namestring),3);
  return value1;
}

/* Handler: Signals a TYPE-ERROR with the same error message as the current
   condition. */
local void signal_type_error (void* sp, gcv_object_t* frame, object label,
                              object condition) {
  var gcv_object_t* thing_ = (gcv_object_t*)sp;
  /* (SYS::ERROR-OF-TYPE 'TYPE-ERROR
        :DATUM thing
        :EXPECTED-TYPE '(AND STRING (SATISFIES SYSTEM::VALID-LOGICAL-PATHNAME-STRING-P))
        "~A" condition) */
  pushSTACK(S(type_error));
  pushSTACK(S(Kdatum)); pushSTACK(*thing_);
  pushSTACK(S(Kexpected_type)); pushSTACK(O(type_logical_pathname_string));
  pushSTACK(O(tildeA)); pushSTACK(condition);
  funcall(L(error_of_type),7);
}

LISPFUNNR(logical_pathname,1)
{ /* (LOGICAL-PATHNAME thing), CLtL2 p. 631 */
  var object thing = STACK_0;
  if (logpathnamep(thing)) {
    /* nothing to do for logical pathnames. */
    VALUES1(thing);
  } else if (pathnamep(thing)) {
    /* normal pathnames cannot be converted into logical pathnames. */
    pushSTACK(thing);                    /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_logical_pathname)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(thing);
    pushSTACK(S(logical_pathname));
    fehler(type_error,GETTEXT("~S: argument ~S is not a logical pathname, string, stream or symbol"));
  } else if (builtin_stream_p(thing)) { /* Stream? */
    thing = as_file_stream(thing);
    test_file_stream_named(thing);
    var object pathname = TheStream(thing)->strm_file_name;
    if (!logpathnamep(pathname)) {
      /* Normal pathnames cannot be converted into logical pathnames. */
      pushSTACK(pathname);                 /* TYPE-ERROR slot DATUM */
      pushSTACK(O(type_logical_pathname)); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(thing); pushSTACK(S(logical_pathname));
      fehler(type_error,GETTEXT("~S: the stream ~S was not opened with a logical pathname"));
    }
    VALUES1(pathname);
  } else {
    /* ANSI CL requires that we transform PARSE-ERROR into TYPE-ERROR. */
    var gcv_object_t* thing_ = &STACK_0;
    make_HANDLER_frame(O(handler_for_parse_error), &signal_type_error,thing_);
    var object pathname = parse_as_logical(thing);
    unwind_HANDLER_frame();
    /* Check that a host was given. This makes it hard to create relative
       logical pathnames, but it is what ANSI CL specifies. */
    if (nullp(TheLogpathname(pathname)->pathname_host)) {
      pushSTACK(TheLogpathname(pathname)->pathname_host); /* TYPE-ERROR slot DATUM */
      pushSTACK(S(string));                 /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(STACK_(0+2)); pushSTACK(S(logical_pathname));
      fehler(type_error,GETTEXT("~S: argument ~S does not contain a host specification"));
    }
    VALUES1(pathname);
  }
  skipSTACK(1);
}

/* forward declaration */
local object use_default_dir (object pathname);

/* (TRANSLATE-LOGICAL-PATHNAME pathname &key [:absolute]), CLtL2 p. 631 */
LISPFUN(translate_logical_pathname,seclass_default,1,0,norest,key,1,
        (kw(absolute))) {
  var bool absolute_p = !missingp(STACK_0);
  var object pathname;
  skipSTACK(1);                 /* drop :ABSOLUTE */
  /* It is not clear from the ANSI CL spec how the argument shall be coerced
   to a pathname. But the examples in the spec indicate that if the
   argument is a string, it should be converted to a logical pathname,
   by calling LOGICAL-PATHNAME, not by calling PATHNAME. */
  if (stringp(STACK_0)) {
    funcall(L(logical_pathname),1); pathname = value1;
  } else {
    pathname = coerce_xpathname(popSTACK());
  }
  if (logpathnamep(pathname)) {
    /* Conversion of a logical into a normal pathname:
     (let ((ht (make-hash-table :key-type 'logical-pathname :value-type '(eql t)
                                :test #'equal)))
       (loop
         (when (gethash pathname ht) (error "Translation loop"))
         (setf (gethash pathname ht) t)
         (let ((host (or (pathname-host pathname) "SYS")))
           (unless (logical-host-p host) (error "No translation for host"))
           (let* ((translations
                   (gethash host sys::*logical-pathname-translations*))
                  (translation
                   (assoc pathname translations :test #'pathname-match-p)))
             (unless (and translation (consp translation)
                          (consp (cdr translation)))
               (error "No translation for pathname"))
             (setq pathname (translate-pathname pathname (first translation)
                                                (second translation)))))
         (unless (sys::logical-pathname-p pathname) (return)))
       pathname) */
    pushSTACK(pathname);
    DOUT("translate-logical-pathname: <",pathname);
    pushSTACK(S(Ktest)); pushSTACK(L(equal)); funcall(L(make_hash_table),2);
    pushSTACK(value1);
    /* stack layout: pathname, ht. */
    loop {
      if (!nullp(shifthash(STACK_0,STACK_1,T,true))) {
        /* STACK_1 = pathname; -- FILE-ERROR slot PATHNAME */
        STACK_0 = STACK_1;
        pushSTACK(S(translate_logical_pathname));
        fehler(file_error,GETTEXT("~S: endless loop while resolving ~S"));
      }
      if (nullp(TheLogpathname(STACK_1)->pathname_host)) {
        /* replace host NIL with default-host: */
        var object newp = allocate_logpathname();
        var object oldp = STACK_1;
        TheLogpathname(newp)->pathname_host
          = O(default_logical_pathname_host); /* Default "SYS" */
        TheLogpathname(newp)->pathname_directory
          = TheLogpathname(oldp)->pathname_directory;
        TheLogpathname(newp)->pathname_name
          = TheLogpathname(oldp)->pathname_name;
        TheLogpathname(newp)->pathname_type
          = TheLogpathname(oldp)->pathname_type;
        TheLogpathname(newp)->pathname_version
          = TheLogpathname(oldp)->pathname_version;
        STACK_1 = newp;
      }
      var object host = TheLogpathname(STACK_1)->pathname_host;
      DOUT("translate-logical-pathname:",host);
      var object translations =
        gethash(host,Symbol_value(S(logpathname_translations)),false);
      if (eq(translations,nullobj)) {
        /* STACK_1 = pathname; -- FILE-ERROR slot PATHNAME */
        STACK_0 = STACK_1;
        pushSTACK(host);
        pushSTACK(S(translate_logical_pathname));
        fehler(file_error,GETTEXT("~S: unknown logical host ~S in ~S"));
      }
      /* (ASSOC pathname translations :test #'pathname-match-p): */
      pushSTACK(STACK_1); pushSTACK(translations);
      DOUT("translate-logical-pathname:[path_name_s1]",STACK_1);
      DOUT("translate-logical-pathname:",translations);
      pushSTACK(S(Ktest)); pushSTACK(L(pathname_match_p));
      funcall(L(assoc),4);
      if (atomp(value1) || matomp(Cdr(value1))) {
        /* STACK_1 = pathname; -- FILE-ERROR slot PATHNAME */
        STACK_0 = STACK_1;
        pushSTACK(S(translate_logical_pathname));
        fehler(file_error,GETTEXT("~S: No replacement rule for ~S is known."));
      }
      /* (TRANSLATE-PATHNAME pathname (first rule) (second rule) :MERGE NIL):*/
      pushSTACK(STACK_1); pushSTACK(Car(value1)); pushSTACK(Car(Cdr(value1)));
      pushSTACK(S(Kmerge)); pushSTACK(NIL);
      funcall(L(translate_pathname),5);
      STACK_1 = pathname = value1;
      DOUT("translate-logical-pathname:",pathname);
      if (!logpathnamep(pathname))
        break;
    }
    DOUT("translate-logical-pathname: >",pathname);
    skipSTACK(2);
  }
  if (absolute_p)
    pathname = use_default_dir(pathname); /* insert default-directory */
  VALUES1(pathname);
}

/* UP: Change an object into a non-logical pathname.
 coerce_pathname(object)
 > object: object
 < return: (TRANSLATE-LOGICAL-PATHNAME (PATHNAME Objekt))
 can trigger GC */
local maygc object coerce_pathname (object obj) {
  obj = coerce_xpathname(obj);
  if (pathnamep(obj)) {
    return obj;
  } else if (logpathnamep(obj)) {
    /* call TRANSLATE-LOGICAL-PATHNAME: */
    pushSTACK(obj); funcall(L(translate_logical_pathname),1);
    return value1;
  } else
    NOTREACHED;
}

#endif

/* UP: Pushes substrings for STRING_CONCAT on the STACK, that together yield
 the string for a subdirectory (car path) .
 subdir_namestring_parts(path,logicalp)
 > path: a Cons
 > logicalp: boolean
 < result: number of strings pushed on the stack
 changes STACK */

#define SUBDIR_PUSHSTACK(subdir)                                         \
  do { if (eq(subdir,S(Kwild_inferiors))) pushSTACK(O(wildwild_string)); \
       else if (eq(subdir,S(Kwild))) pushSTACK(O(wild_string));          \
       else if (eq(subdir,S(Kup)) || eq(subdir,S(Kback)))                \
         pushSTACK(O(dotdot_string));                                    \
       else if (stringp(subdir)) pushSTACK(subdir);                      \
       else NOTREACHED;                                                  \
  } while(0)

local uintC subdir_namestring_parts (object path,bool logp) {
  var object subdir = Car(path);
 #if defined(PATHNAME_UNIX) || defined(PATHNAME_WIN32)
  SUBDIR_PUSHSTACK(subdir); return 1;
 #endif
}

/* UP: Pushes substrings for STRING_CONCAT on the STACK, that together yield
 the String for the host of the Pathname pathname.
 host_namestring_parts(pathname)
 > pathname: non-logical pathname
 < result: number of strings pushed on the stack
 changes STACK */
#if HAS_HOST || defined(LOGICAL_PATHNAMES)
local uintC host_namestring_parts (object pathname) {
  var bool logp = logpathnamep(pathname);
  var object host = xpathname_host(logp,pathname);
  if (nullp(host)) {
    return 0; /* no String */
  } else {
   #ifdef PATHNAME_WIN32
    if (!logp) {
      pushSTACK(O(backslashbackslash_string));
      pushSTACK(host);
      return 2;
    }
   #endif
    pushSTACK(host);
    pushSTACK(O(colon_string)); /* ":" */
    return 2;
  }
}
#else
  #define host_namestring_parts(pathname)  (unused (pathname), 0)  /* no strings */
#endif

/* UP: Pushes substrings for STRING_CONCAT on the STACK, that together
 yield the String for the Device and Directory of the Pathname pathname.
 directory_namestring_parts(pathname)
 > pathname: non-logical pathname
 < result: number of strings pushed on the stack
 changes STACK */
local uintC directory_namestring_parts (object pathname) {
  var uintC stringcount = 0; /* number of strings so far = 0 */
  var bool logp = logpathnamep(pathname);
 #if defined(PATHNAME_WIN32)
  { /* Device: */
    var object device = xpathname_device(logp,pathname);
    if (!(nullp(device))) { /* NIL -> no string */
      var object string = wild2string(device);
      pushSTACK(string);
      stringcount++; /* and count */
      pushSTACK(O(colon_string));
      stringcount++; /* ":" */
    }
  }
 #endif
 #if defined(PATHNAME_WIN32) || defined(PATHNAME_UNIX)
  if (stringcount == 0) /* only if there's no device already */
    /* no check for both host and device being present:
       this can never happen in CLISP */
    stringcount += host_namestring_parts(pathname);
 #endif
  { /* Directory: */
    var object directory = xpathname_directory(logp,pathname);
   #if defined(LOGICAL_PATHNAMES)
    if (logp) {
      if (consp(directory) && eq(Car(directory),S(Krelative))) {
        pushSTACK(O(semicolon_string)); stringcount++; /* ";" on the Stack */
      }
    } else
   #endif
#if defined(PATHNAME_WIN32)
#define push_slash pushSTACK(O(backslash_string))
#elif defined(PATHNAME_UNIX)
#define push_slash pushSTACK(O(slash_string))
#else
#error "what is the directory separator on your platform?"
#endif
    {
      if (!mconsp(directory)) return stringcount; /* no directory */
      /* is the first subdir = :ABSOLUTE or = :RELATIVE ? */
      if (eq(Car(directory),S(Kabsolute))) {
        push_slash; stringcount++; /* "/" */
      } else if (nullp(Cdr(directory))) { /* (:RELATIVE) ==> "./" */
        pushSTACK(O(dot_string)); stringcount++; /* "." */
        push_slash; stringcount++; /* "/" */
        return stringcount;
    }}
    directory = Cdr(directory); /* skip */
    /* other subdirs on the stack: */
    while (consp(directory)) {
      stringcount += subdir_namestring_parts(directory,logp);
     #if defined(LOGICAL_PATHNAMES)
      if (logp) {
        pushSTACK(O(semicolon_string)); stringcount++; /* ";" */
      } else
     #endif
      {
       #ifdef PATHNAME_WIN32
        pushSTACK(O(backslash_string)); stringcount++; /* "\\" */
       #endif
       #ifdef PATHNAME_UNIX
        pushSTACK(O(slash_string)); stringcount++; /* "/" */
       #endif
      }
      directory = Cdr(directory);
    }
  }
#undef push_slash
  return stringcount;
}

/* UP: Pushes substrings for STRING_CONCAT on the STACK, that together yield
 the string for Name and Type of the pathname.
 nametype_namestring_parts(name,type,version)
 > name, type, poss. version: components of the pathname
 < result: number of the strings pushed on the stack
 can trigger GC
 changes STACK */
local maygc uintC nametype_namestring_parts (object name, object type, object version)
{
  var uintC stringcount = 0;
  /* Name: */
  if (!nullp(name)) { /* name=NIL -> do not print */
    var object string = wild2string(name);
    pushSTACK(string);
    stringcount++; /* and count */
  }
  /* Type: */
  if (!nullp(type)) { /* type=NIL -> do not print */
    pushSTACK(O(dot_string)); /* "." */
    stringcount++; /* and count */
    var object string = wild2string(type);
    pushSTACK(string);
    stringcount++; /* and count */
  }
  if (!nullp(version)) { /* version=NIL -> do not print */
    pushSTACK(O(dot_string)); /* "." */
    stringcount++; /* and count */
    if (eq(version,S(Knewest)))
      /* http://www.lisp.org/HyperSpec/Body/sec_19-3-1.html */
      pushSTACK(Symbol_name(S(Knewest))); /* :NEWEST -> "NEWEST" */
    else if (eq(version,S(Kwild)))
      pushSTACK(O(wild_string));
    else
      /* version (integer >0) ==> string: (sys::decimal-string version) */
      pushSTACK(decimal_string(version));
    stringcount++; /* and count */
  }
  return stringcount;
}

/* UP: Pushes substrings for STRING_CONCAT on the STACK, that together yield
 the string for name and type of the pathname.
 file_namestring_parts(pathname)
 > pathname: non-logical pathname
 < result: number of the strings pushed on the stack
 can trigger GC
 changes STACK */
local maygc uintC file_namestring_parts (object pathname) {
 #if defined(LOGICAL_PATHNAMES)
  if (logpathnamep(pathname))
    return nametype_namestring_parts
      (TheLogpathname(pathname)->pathname_name,
       TheLogpathname(pathname)->pathname_type,
       TheLogpathname(pathname)->pathname_version);
  else
 #endif
    /* do not print version when the underlying physical file system
       does not support it */
    return nametype_namestring_parts(ThePathname(pathname)->pathname_name,
                                     ThePathname(pathname)->pathname_type,
                                     pathname_version_maybe(pathname));
}

/* UP: Converts pathname into string.
 whole_namestring(pathname)
 > pathname: non-logical pathname
 < result: Normal-Simple-String
 can trigger GC */
local maygc object whole_namestring (object pathname) {
  var uintC stringcount = 0;
  stringcount += directory_namestring_parts(pathname);
  stringcount += file_namestring_parts(pathname);
  return string_concat(stringcount);
}

/* UP: Returns the string for the directory of a pathname.
 directory_namestring(pathname)
 > pathname: non-logical pathname
 < result: Normal-Simple-String
 can trigger GC */
local maygc object directory_namestring (object pathname) {
  /* The function DIRECTORY-NAMESTRING is totally underspecified.
   It could return
   a. just the string for the directory portion,
   b. the string for the device + directory portions,
   c. the string for the host + device + directory portions.
   Before we used hosts, we have traditionally returned (b).
   Now, with hosts, we still return (b) since HOST-NAMESTRING returns
   the host part, while there is no way to return just the device
   This makes most sense, given that CLHS says that programs
   should not attempt to concatenate the resulting string with anything. */
  return string_concat(directory_namestring_parts(pathname));
}

/* UP: Returns the string identifying a file in its directory.
 file_namestring(pathname)
 > pathname: non-logical pathname
 < result: normal-simple-string
 can trigger GC */
local maygc inline object file_namestring (object pathname) {
  return string_concat(file_namestring_parts(pathname));
}

LISPFUNNR(file_namestring,1)
{ /* (FILE-NAMESTRING pathname), CLTL p. 417 */
  var object pathname = coerce_xpathname(popSTACK());
  VALUES1(file_namestring(pathname));
}

LISPFUNNR(directory_namestring,1)
{ /* (DIRECTORY-NAMESTRING pathname), CLTL p. 417 */
  var object pathname = coerce_xpathname(popSTACK());
  VALUES1(directory_namestring(pathname));
}

LISPFUNNR(host_namestring,1)
{ /* (HOST-NAMESTRING pathname), CLTL p. 417 */
  var object pathname = coerce_xpathname(popSTACK());
  VALUES1(xpathname_host(logpathnamep(pathname),pathname));
}

/* UP: check an optional VERSION argument.
 test_optional_version(def);
 > STACK_0: VERSION-Argument
 > def: default value for it
 < result: valid version-component */
local object test_optional_version (object def) {
  var object version = STACK_0;
  if (!boundp(version)) {
    STACK_0 = def; /* not specified -> Default */
  } else if (nullp(version)) { /* NIL is OK */
  } else if (eq(version,S(Kwild))) { /* :WILD is OK */
  } else if (eq(version,S(Knewest))) { /* :NEWEST is OK */
  } else if (posfixnump(version) && !eq(version,Fixnum_0)) {/*Fixnum>0 is OK*/
  } else if (pathnamep(version)) { /* Pathname -> its Version */
    STACK_0 = ThePathname(version)->pathname_version;
  }
#ifdef LOGICAL_PATHNAMES
  else if (logpathnamep(version)) { /* Logical Pathname -> its Version */
    STACK_0 = TheLogpathname(version)->pathname_version;
  }
#endif
  else { /* None of the desired cases -> error: */
    pushSTACK(version);         /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_version)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(version);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: :VERSION-argument should be NIL or a positive fixnum or :WILD or :NEWEST, not ~S"));
  }
  return STACK_0;
}

#ifdef PATHNAME_WIN32

/* the operating system manages a default-drive.
 the operating system manages a default-directory on each drive. This
 can change, if another floppy disk is inserted. */

/* a default-drive is kept: DEFAULT_DRIVE = O(default_drive). */

/* the variable *DEFAULT-PATHNAME-DEFAULTS* contains (as pathname) the
 default value for each MERGE-operation. It is the one, which the system
 "interpretes into" the pathnames entered by the user.
 It is kept up to date with the DEFAULT_DRIVE: On
 initialization the current device (in terms of DOS), on
 change of DEFAULT_DRIVE via CD. */

#endif /* PATHNAME_WIN32 */

#ifdef PATHNAME_UNIX

/* The variable *DEFAULT-PATHNAME-DEFAULTS* contains (as pathname) the
 default value for each MERGE-operation. It is the one, which the system
 "interpretes into" the pathnames entered by the user. */

#endif

#ifdef UNIX

/* the operating system manages a default-directory ("working directory")
 for this process. It can be changed with chdir and queried with getwd.
 See CHDIR(2) and GETWD(3). */

#endif

/* UP: Re-calculation of *DEFAULT-PATHNAME-DEFAULTS* */
#ifdef PATHNAME_WIN32
/* from DEFAULT_DRIVE */
#endif
/* recalc_defaults_pathname();
 < result: value of *DEFAULT-PATHNAME-DEFAULTS*, a pathname
 can trigger GC */
local maygc object recalc_defaults_pathname (void) {
 #ifdef PATHNAME_WIN32
  /* execute (MAKE-PATHNAME :DEVICE default-drive) : */
  pushSTACK(S(Kdevice)); pushSTACK(O(default_drive));
  funcall(L(make_pathname),2);
 #endif
 #ifdef PATHNAME_UNIX
  /* execute (MAKE-PATHNAME) : */
  funcall(L(make_pathname),0);
 #endif
  /* and assign *DEFAULT-PATHNAME-DEFAULTS* : */
  return Symbol_value(S(default_pathname_defaults)) = value1;
}

/* UP: Returns the default-pathname.
 defaults_pathname()
 < result: value of *DEFAULT-PATHNAME-DEFAULTS*, a pathname
 can trigger GC */
local maygc object defaults_pathname (void) {
  var object pathname = Symbol_value(S(default_pathname_defaults)); /* value of *DEFAULT-PATHNAME-DEFAULTS* */
  if (xpathnamep(pathname)) { /* is a pathname -> OK */
    return pathname;
  } else { /* else warning: */
    pushSTACK(CLSTEXT("The value of ~S was not a pathname. ~:*~S is being reset."));
    pushSTACK(S(default_pathname_defaults));
    funcall(S(warn),2);
    /* and re-calculate: */
    return recalc_defaults_pathname();
  }
}

/* merge two directories
 > p_directory: pathname directory list
 > d_directory: defaults directory list
 > p_log: flag, whether pathname is logical
 > wildp: flag, from MERGE-PATHNAMES
 > called_from_make_pathname: flag, from MERGE-PATHNAMES
 < result: merges directory list
 can trigger GC */
local maygc object merge_dirs (object p_directory, object d_directory, bool p_log,
                               bool wildp, bool called_from_make_pathname) {
  var object new_subdirs = p_directory;
 #if DEBUG_TRANSLATE_PATHNAME
  printf("[%d] merge_dirs: log: %d; wild: %d; cfmp: %d\n",
         __LINE__,p_log,wildp,called_from_make_pathname);
 #endif
  SDOUT("merge_dirs:",p_directory);
  SDOUT("merge_dirs:",d_directory);
  if (called_from_make_pathname) {
    if (missingp(p_directory)) /* pathname-subdirs not given? */
      new_subdirs = d_directory; /* use defaults-subdirs */
  } else if (!wildp) {
    if (nullp(p_directory) /* is pathname-subdirs trivial? */
        || (eq(Car(p_directory),p_log ? S(Kabsolute) : S(Krelative))
            && matomp(Cdr(p_directory)))) {
      new_subdirs = d_directory; /* use defaults-subdirs */
    } else if (eq(Car(p_directory),S(Krelative))
               /* PATHNAME = :ABSOLUTE ==> merge is not needed */
               && consp(d_directory) /* DEFAULT = NIL ==> nothing to merge */
               && (eq(Car(d_directory),S(Kabsolute))
                   || !nullpSv(merge_pathnames_ansi))) {
      /* (append defaults-subdirs (cdr pathname-subdirs)) =
       (nreconc (reverse defaults-subdirs) (cdr pathname-subdirs)) : */
      pushSTACK(Cdr(p_directory));
      var object temp = reverse(d_directory);
      new_subdirs = simplify_directory(nreconc(temp,popSTACK()));
    }
  }
  return new_subdirs;
}

/* (MERGE-PATHNAMES pathname [defaults [default-version]] [:wild]), CLTL p. 415
 Definition assuming that HAS_HOST and HAS_DEVICE are exclusive:
 (defun merge-pathnames (pathname &optional (defaults *default-pathname-defaults*) default-version)
   (setq pathname (pathname pathname))
   (setq defaults (pathname defaults))
   (multiple-value-call #'make-pathname
#if HAS_HOST
     (if (or (equal (pathname-host pathname) (pathname-host defaults))
             (null (pathname-host pathname)))
       (values
         :host (or (pathname-host pathname) (pathname-host defaults))
#endif
#if HAS_DEVICE
     (if (or (equal (pathname-device pathname) (pathname-device defaults))
             (null (pathname-device pathname)))
       (values
         :device (or (pathname-device pathname) (pathname-device defaults))
#endif
         :directory
           (let ((pathname-dir (pathname-directory pathname))
                 (defaults-dir (pathname-directory defaults)))
             (if (eq (car pathname-dir) ':RELATIVE)
               (cond ((null (cdr pathname-dir)) defaults-dir)
                     ((or *merge-pathnames-ansi*
                          (not (eq (car defaults-dir) ':RELATIVE))) ; <----
                      (append defaults-dir (cdr pathname-dir)))
                     (t pathname-dir))
               pathname-dir)))
       (values
#if HAS_HOST
         :host (pathname-host pathname)
#endif
#if HAS_DEVICE
         :device (pathname-device pathname)
#endif
         :directory (pathname-directory pathname)))
     :name (or (pathname-name pathname) (pathname-name defaults))
     :type (or (pathname-type pathname) (pathname-type defaults))))

 If HAS_HOST and HAS_DEVICE are both true, the semantics are more
 complicated; see CLHS for details.

 If the :WILD argument is specified, :WILD components are replaced,
 instead of missing components.

 Explanation of the "<----" line:

 Roger Kehr <kehr@iti.informatik.th-darmstadt.de> asks why in CLISP

 (merge-pathnames (make-pathname :directory '(:relative "x"))
                  (make-pathname :directory '(:relative "y")))
 => #"x/"

 where he expects to get #"y/x/".

 Bruno: There are two reasons for this behaviour:

 1. An informal one: I found the latter behaviour confusing and changed
    CLISP to do it the former way. It seems to work better this way.

 2. A formal one: MERGE-PATHNAMES is used to specify default components
    for pathnames, so there is some analogy between (MERGE-PATHNAMES a b)
    and (or a b). Obviously putting in the same default a second time
    should do the same as putting it in once:

      (or a b b) is the same as (or a b), so

      (MERGE-PATHNAMES (MERGE-PATHNAMES a b) b) should be the same as
      (MERGE-PATHNAMES a b).

    (This question actually matters because in Common Lisp there is
    no distinction between "pathnames with defaults merged-in" and
    "pathnames with defaults not yet applied". For example, you do not
    know whether COMPILE-FILE will merge in some defaults.)

    Now, (MERGE-PATHNAMES (MERGE-PATHNAMES '#"x/" '#"y/") '#"y/")
    and  (MERGE-PATHNAMES '#"x/" '#"y/")
    are equal in CLISP's implementation, but not in implementations
    that strictly follow the Common Lisp spec. In fact, the above
    twice-default = once-default rule holds for all pathnames in CLISP. */
LISPFUN(merge_pathnames,seclass_read,1,2,norest,key,1, (kw(wild))) {
  /* :wild #'make-pathname causes NIL components to be considered specified,
   only #<unbound> components are considered unspecified. */
  var bool called_from_make_pathname = eq(STACK_0,L(make_pathname));
  /* :wild t causes only wild components to be considered unspecified. */
  var bool wildp = !missingp(STACK_0);
  skipSTACK(1);

#define SPECIFIED(obj)                               \
  !(called_from_make_pathname ? !boundp(obj) :       \
    (wildp ? eq(obj,S(Kwild)) : nullp(obj)))
#define NAMETYPE_MATCH(acc,slot)                                        \
  { var object tmp = x##slot(p_log,p);                                  \
    acc(newp)->slot = (SPECIFIED(tmp) ? tmp : (object)x##slot(d_log,d)); \
  }

  /* check pathname (STACK_2) and defaults (STACK_1):
   (coerce defaults 'pathname): */
  STACK_1 = test_default_pathname(STACK_1);
  /* (coerce pathname 'pathname): */
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(STACK_1)) {
    if (!xpathnamep(STACK_2)) { /* pathname */
      STACK_2 = parse_as_logical(STACK_2);
      DOUT("merge-pathnames:[log_pathname]",STACK_2);
    }
  } else
  #endif
  STACK_2 = coerce_xpathname(STACK_2); /* pathname */
  var bool d_log = logpathnamep(STACK_1);
  var bool p_log = logpathnamep(STACK_2);

  { /* check default-version (STACK_0): */
    var object v = test_optional_version(unbound);
    var object p_version = xpathname_version(p_log,STACK_2);
    var object d_version = xpathname_version(d_log,STACK_1);
    var object p_name = xpathname_name(p_log,STACK_2);
    if (SPECIFIED(p_version))
      v = p_version;
    if (missingp(v) && !SPECIFIED(p_name) && SPECIFIED(d_version))
      v = d_version;
    if (!boundp(v)) v = S(Knewest);
    STACK_0 = STACK_1; STACK_1 = STACK_2; STACK_2 = v;
    DOUT("merge-pathnames:",v);
  }
  /* stack layout: default-version, pathname, defaults. */

  /* do the merge */
 #ifdef LOGICAL_PATHNAMES
  DOUT("merge-pathnames:[defaults]",STACK_0);
  DOUT("merge-pathnames:[pathname]",STACK_1);
  if (d_log || p_log) {
    /* MERGE-PATHNAMES for logical pathnames */
    var object newp = allocate_logpathname(); /* fetch new pathname */
    var object d = popSTACK(); /* defaults */
    var object p = popSTACK(); /* pathname */
    { /* match hosts: */
      var object p_host = xpathname_host(p_log,p);
      var object d_host = xpathname_host(d_log,d);
      TheLogpathname(newp)->pathname_host = p_host; /* initially, new-host := pathname-host */
      if (equal(p_host,d_host))
        goto lmatch_directories;
      if (wildp ? !boundp(p_host) : nullp(p_host)) {
        /* pathname-host not specified, but defaults-host specified: */
        TheLogpathname(newp)->pathname_host = d_host; /* new-host := defaults-host */
        goto lmatch_directories;
      }
    }
    { /* directories do not match: new-directory := pathname-directory */
      var object dir = xpathname_directory(p_log,p);
      TheLogpathname(newp)->pathname_directory =
        (missingp(dir) ? xpathname_directory(d_log,d) : dir);
      goto ldirectories_OK;
    }
  lmatch_directories:
    { /* match directories: */
      pushSTACK(p); pushSTACK(d); pushSTACK(newp);
      TheLogpathname(STACK_0)->pathname_directory =
        merge_dirs(xpathname_directory(p_log,p),
                   xpathname_directory(d_log,d),
                   p_log,wildp,called_from_make_pathname);
      newp = popSTACK(); d = popSTACK(); p = popSTACK();
    }
  ldirectories_OK:
    /* the directories are OK now */
    NAMETYPE_MATCH(TheLogpathname,pathname_name);
    NAMETYPE_MATCH(TheLogpathname,pathname_type);
    TheLogpathname(newp)->pathname_version = popSTACK();
    DOUT("merge-pathnames:[ret]",newp);
    VALUES1(newp);
    return;
  }
  /* not both are logical pathnames -> first, convert into normal pathnames: */
  STACK_1 = coerce_pathname(STACK_1);
  STACK_0 = coerce_pathname(STACK_0);
 #endif
  var object newp = allocate_pathname(); /* fetch new pathname */
  var object d = popSTACK(); /* defaults */
  var object p = popSTACK(); /* pathname */
 #if HAS_HOST
  { /* match hosts: */
    var object p_host = ThePathname(p)->pathname_host;
    var object d_host = ThePathname(d)->pathname_host;
    ThePathname(newp)->pathname_host = p_host; /* initially, new-host := pathname-host */
    /* both hosts equal -> match devices: */
    if (equal(p_host,d_host))
      goto match_devices;
    if (!(wildp ? false : nullp(p_host)))
      goto notmatch_devices;
   #ifdef PATHNAME_WIN32
    var object p_device = ThePathname(p)->pathname_device;
    /* On Win32, a non-null p_device implicitly designates p_host as the
     local machine. It must not be overridden by d_host. */
    if (SPECIFIED(p_device))
      goto notmatch_devices;
   #endif
    /* pathname-host not specified, but defaults-host specified: */
    ThePathname(newp)->pathname_host = d_host; /* new-host := defaults-host */
    goto match_devices;
  }
 #endif /* HAS_HOST */
 match_devices:
 #if HAS_DEVICE
  { /* match devices: */
    var object p_device = ThePathname(p)->pathname_device;
    var object d_device = ThePathname(d)->pathname_device;
    ThePathname(newp)->pathname_device = p_device; /* initially, new-device := pathname-device */
    /* both devices equal -> match directories: */
    if (equal(p_device,d_device))
      goto match_directories;
    if (!SPECIFIED(p_device)) {
      /* pathname-device not given, but defaults-device is given: */
      ThePathname(newp)->pathname_device = d_device; /* new-device := defaults-device */
      goto match_directories;
    }
    goto notmatch_directories;
  }
 #endif /* HAS_DEVICE */
 match_directories: { /* match directories: */
    var object tmp;
    pushSTACK(p); pushSTACK(d); pushSTACK(newp);
    tmp = merge_dirs(ThePathname(p)->pathname_directory,
                     ThePathname(d)->pathname_directory,
                     false,wildp,called_from_make_pathname);
    newp = popSTACK(); d = popSTACK(); p = popSTACK();
    ThePathname(newp)->pathname_directory = tmp;
  }
  goto directories_OK;
  /* do not match devices: */
 notmatch_devices:
 #if HAS_DEVICE
  { /* new-device := pathname-device : */
    ThePathname(newp)->pathname_device = ThePathname(p)->pathname_device;
  }
 #endif
 notmatch_directories:
  { /* directories do not match: new-directory := pathname-directory */
    var object dir = xpathname_directory(p_log,p);
    ThePathname(newp)->pathname_directory =
      (missingp(dir) ? xpathname_directory(d_log,d) : dir);
  }
 directories_OK:
  /* the directories are OK now */
  NAMETYPE_MATCH(ThePathname,pathname_name);
  NAMETYPE_MATCH(ThePathname,pathname_type);
  ThePathname(newp)->pathname_version = popSTACK();
  DOUT("merge-pathnames:[ret]",newp);
  VALUES1(newp);
}
#undef SPECIFIED
#undef NAMETYPE_MATCH

/* (ENOUGH-NAMESTRING pathname [defaults]), CLTL p. 417
 Definition assuming that HAS_HOST and HAS_DEVICE are exclusive:
 (defun enough-namestring (pathname &optional (defaults *default-pathname-defaults*))
   (setq pathname (pathname pathname))
   (setq defaults (pathname defaults))
   (namestring
     (multiple-value-call #'make-pathname
#if HAS_HOST
       (if (equal (pathname-host pathname) (pathname-host defaults))
         (values
           :host nil
#endif
#if HAS_DEVICE
       (if (equal (pathname-device pathname) (pathname-device defaults))
         (values
           :device nil
#endif
           :directory
             (let ((pathname-dir (pathname-directory pathname))
                   (defaults-dir (pathname-directory defaults)))
               (if (equal pathname-dir defaults-dir)
                 (list ':RELATIVE)
                 (if (and (not (eq (car pathname-dir) ':RELATIVE))
                          (not (eq (car defaults-dir) ':RELATIVE))
                          (equal (subseq pathname-dir 0 (min (length pathname-dir) (length defaults-dir)))
                                 defaults-dir
                     )    )
                   (cons ':RELATIVE (nthcdr (length defaults-dir) pathname-dir))
                   pathname-dir
             ) ) )
         )
         (values
#if HAS_HOST
           :host (pathname-host pathname)
#endif
#if HAS_DEVICE
           :device (pathname-device pathname)
#endif
           :directory (pathname-directory pathname)))
       :name (if (equal (pathname-name pathname) (pathname-name defaults))
               nil
               (pathname-name pathname))
       :type (if (equal (pathname-type pathname) (pathname-type defaults))
               nil
               (pathname-type pathname)))))

 If HAS_HOST and HAS_DEVICE are both true, the semantics are more
 complicated; see CLHS for details. */
#define SET_NEWP(slot,value)                            \
      if (log2) TheLogpathname(newp)->slot = value;     \
      else ThePathname(newp)->slot = value;
LISPFUN(enough_namestring,seclass_read,1,1,norest,nokey,0,NIL) {
  /* check pathname and defaults:
   turn pathname into a Pathname: */
  STACK_1 = coerce_xpathname(STACK_1);
  var bool log2 = logpathnamep(STACK_1);
  /* turn defaults into a Pathname: */
  STACK_0 = test_default_pathname(STACK_0);
  var bool log1 = logpathnamep(STACK_0);
  /* fetch new Pathname: */
  var object newp = (log2 ? allocate_logpathname() : allocate_pathname());
  pushSTACK(newp);
  /* stack layout: pathname, defaults, new. */
 #if HAS_HOST
  { /* compare hosts: */
    var object p_host = xpathname_host(log2,STACK_2); /* pathname-host */
    var object d_host = xpathname_host(log1,STACK_1); /* defaults-host */
    if (equal(p_host,d_host)) { /* both hosts equal? */
      SET_NEWP(pathname_host,NIL); /* new-host := NIL */
 #endif
 #if HAS_DEVICE
    { /* compare devices: */
      var object p_device = xpathname_device(log2,STACK_2);
      var object d_device = xpathname_device(log1,STACK_1);
      if (equal(p_device,d_device)) { /* both devices equal? */
        if (!log2) ThePathname(newp)->pathname_device = NIL;
 #endif
        {
          var object p_directory = xpathname_directory(log2,STACK_2);
          var object d_directory = xpathname_directory(log1,STACK_1);
          var object new_subdirs;
          /* compare pathname-subdirs and defaults-subdirs: */
          if (equal(p_directory,d_directory)) { /* ==> use NIL : */
            new_subdirs = NIL;
          } else {
            /* Does neither pathname-subdirs nor defaults-subdirs
             start with :RELATIVE ? */
            if (   consp(p_directory) && (eq(Car(p_directory),S(Kabsolute)))
                && consp(d_directory) && (eq(Car(d_directory),S(Kabsolute)))) {
              /* yes -> test, if defaults-subdirs is a starting piece
               of the list pathname-subdirs: */
              var object Lp = p_directory;
              var object Ld = d_directory;
              /* Is Ld a starting piece of Lp ? */
              loop {
                if (atomp(Ld)) { /* Ld finished -> yes */
                  new_subdirs = Lp;
                  /* new-subdirs := (cons :RELATIVE new-subdirs) : */
                  pushSTACK(new_subdirs);
                  new_subdirs = allocate_cons();
                  Cdr(new_subdirs) = popSTACK();
                  Car(new_subdirs) = S(Krelative);
                  goto subdirs_ok;
                }
                if (atomp(Lp))
                  break; /* Lp finished -> no */
                if (!equal(Car(Ld),Car(Lp))) /* different list-elements? */
                  break; /* -> no */
                Ld = Cdr(Ld); Lp = Cdr(Lp); /* advance lists */
              }
            }
            new_subdirs = p_directory; /* new-subdirs := pathname-subdirs */
          }
         subdirs_ok: /* new-subdirs is the new subdir-list. */
          /* new-directory := new-subdirs : */
          newp = STACK_0;
          SET_NEWP(pathname_directory,new_subdirs);
        }
    #if HAS_DEVICE
      } else {
        /* different devices
         (Note for PATHNAME_WIN32: If we have different devices, the common
         host must have been NIL.)
         new-device := pathname-device
         new-directory := pathname-directory */
        if (log2) {
          TheLogpathname(newp)->pathname_directory =
            TheLogpathname(STACK_2)->pathname_directory;
        } else {
          ThePathname(newp)->pathname_device = p_device;
          ThePathname(newp)->pathname_directory =
            ThePathname(STACK_2)->pathname_directory;
        }
      }
    }
    #endif
    #if HAS_HOST
    } else { /* different hosts */
      /* new-host := pathname-host
       new-device := pathname-device
       new-directory := pathname-directory */
      if (log2) {
        TheLogpathname(newp)->pathname_host = p_host;
        TheLogpathname(newp)->pathname_directory =
          TheLogpathname(STACK_2)->pathname_directory;
      } else {
        ThePathname(newp)->pathname_host = p_host;
       #if HAS_DEVICE
        ThePathname(newp)->pathname_device =
          ThePathname(STACK_2)->pathname_device;
       #endif
        ThePathname(newp)->pathname_directory =
          ThePathname(STACK_2)->pathname_directory;
      }
    }
  }
 #endif
  { /* fill in name: */
    var object p_name = xpathname_name(log2,STACK_2); /* pathname-name */
    var object d_name = xpathname_name(log1,STACK_1); /* defaults-name */
    var object r_name = (equal(p_name,d_name) ? NIL : p_name);
    SET_NEWP(pathname_name,r_name);
  }
  { /* fill in type: */
    var object p_type = xpathname_type(log2,STACK_2); /* pathname-type */
    var object d_type = xpathname_type(log1,STACK_1); /* defaults-type */
    var object r_type = (equal(p_type,d_type) ? NIL : p_type);
    SET_NEWP(pathname_type,r_type);
  }
  skipSTACK(3);
  /* build (namestring new) : */
  with_saved_back_trace_subr(L(namestring),STACK STACKop -1,-1,
    VALUES1(whole_namestring(newp)); );
}
#undef SET_NEWP

#ifdef LOGICAL_PATHNAMES

/* UP: checks, if object is an admissible name:
 :WILD or a Simple-String made of valid characters, without adjacent '*'.
 legal_logical_word(object)
 > object: if a simple-string, a normal-simple-string */
local bool legal_logical_word (object obj) {
  if (eq(obj,S(Kwild)))
    return true;
  if (!simple_string_p(obj))
    return false;
  ASSERT(sstring_normal_p(obj));
  var uintL len = Sstring_length(obj);
  if (len==0)
    return false; /* empty word is forbidden */
  SstringDispatch(obj,X, {
    var const cintX* charptr = &((SstringX)TheVarobject(obj))->data[0];
    var bool last_was_star = false;
    dotimespL(len,len, {
      var chart cc = as_chart(*charptr++);
      if (!(legal_logical_word_char(cc) || starp(cc)))
        return false;
      if (starp(cc)) {
        if (last_was_star)
          return false; /* adjacent '*' are forbidden */
        last_was_star = true;
      } else {
        last_was_star = false;
      }
    });
  });
  return true;
}

#endif

#ifdef PATHNAME_NOEXT

/* UP: checks, if object is an admissible name:
 a Simple-String made of valid characters
 legal_name(object)
 > object: any object */
#define legal_name(obj)  check_name(obj,NULL)
/* also, return the _BASE ONE_ index of the first dot in the string */
local bool check_name (object obj, uintL *dot_pos_) {
  if (dot_pos_) *dot_pos_ = 0;
  if (!stringp(obj)) return false;
  var uintL len, offset;
  obj = unpack_string_ro(obj,&len,&offset);
  if (len > 0) {
    SstringDispatch(obj,X, {
      var const cintX* start = ((SstringX)TheVarobject(obj))->data + offset;
      var const cintX* charptr = start;
      do { var chart cc = as_chart(*charptr++);
        if (!legal_namechar(cc)) return false;
        if (dot_pos_ && *dot_pos_==0 && dotp(cc))
          *dot_pos_ = charptr - start;
      } while(--len);
    });
  }
  return true;
}


/* UP: checks, if object is an admissible name:
 a Simple-String made of valid characters, without '.'
 legal_type(object)
 > object: if a simple-string, a normal-simple-string */
local bool legal_type (object obj);
#ifdef PATHNAME_NOEXT
local bool legal_type (object obj) {
  if (!simple_string_p(obj))
    return false;
  ASSERT(sstring_normal_p(obj));
  var uintL len = Sstring_length(obj);
  if (len > 0) {
    SstringDispatch(obj,X, {
      var const cintX* charptr = &((SstringX)TheVarobject(obj))->data[0];
      dotimespL(len,len, {
        var chart cc = as_chart(*charptr++);
        if (dotp(cc) || !legal_namechar(cc))
          return false;
      });
    });
  }
  return true;
}

/* Check that the namestring for path will be parsed into a similar object
 used by pr_orecord() in io.d
 can trigger GC */
global maygc bool namestring_correctly_parseable_p (gcv_object_t *path_)
{
  /* #p".foo" can be either :name ".foo" or :type "foo" */
  var object name = ThePathname(*path_)->pathname_name;
  var object type = ThePathname(*path_)->pathname_type;
  var uintL dot_position;
  check_name(name,&dot_position); /* we know it's valid! */
  if (eq(Symbol_value(S(parse_namestring_dot_file)),S(Ktype))) {
   parse_namestring_dot_file_type: /* ".foo" ==> :type "foo" */
    if (nullp(type) && dot_position>0) return false; /* name has '.' => bad */
  } else if (eq(Symbol_value(S(parse_namestring_dot_file)),S(Kname))) {
    /* ".foo" ==> :name ".foo" */
    if (nullp(name) && !nullp(type)) return false;
    /* has dots _inside_ the name, and type=nil */
    if (nullp(type) && dot_position>1) return false;
  } else {
    fix_parse_namestring_dot_file(); /* set to :TYPE */
    name = ThePathname(*path_)->pathname_name; /* restore after posible GC */
    type = ThePathname(*path_)->pathname_type;
    goto parse_namestring_dot_file_type;
  }
  /* name cannot be "": it is replaced with NIL by MAKE-PATHNAME; */
 #if HAS_VERSION
  /* when the underlying physical file system DOES support version,
     we are confident - for no good reason so far! -
     that we will be able to print the pathname properly */
  return true;
 #else
  /* when the underlying physical file system does NOT support version,
     pathname version is not printed, so cannot be read back! */
  return nullp(ThePathname(*path_)->pathname_version);
 #endif
}
#endif

#endif /* PATHNAME_NOEXT */

local object copy_pathname (object pathname);

/* check whether the list is a valid directory list */
local bool directory_list_valid_p (bool logical, object dirlist) {
  { /* CAR must be either :RELATIVE or :ABSOLUTE ? */
    var object startpoint = Car(dirlist);
    if (!(eq(startpoint,S(Krelative)) || eq(startpoint,S(Kabsolute))))
      return false;
  }
  dirlist = Cdr(dirlist);
  /* check subdir list: */
  while (consp(dirlist)) {
    /* check the next subdir = POP(dirlist); */
    var object subdir = Car(dirlist); dirlist = Cdr(dirlist);
   #ifdef LOGICAL_PATHNAMES
    if (logical) {
      if (!(eq(subdir,S(Kwild_inferiors)) || eq(subdir,S(Kwild))
            || legal_logical_word(subdir) || eq(subdir,S(Kup))))
        return false;
    } else
   #endif
    {
     #ifdef PATHNAME_NOEXT
      #if defined(PATHNAME_UNIX) || defined(PATHNAME_WIN32)
      if (!(eq(subdir,S(Kwild_inferiors)) || eq(subdir,S(Kwild))
            || legal_name(subdir) || eq(subdir,S(Kup))))
        return false;
      #endif
     #endif
    }
  }
  return true;
}

#ifdef LOGICAL_PATHNAMES
 #define COERCE_PATHNAME_SLOT(slot,obj,stack_res)       \
   stack_res = ThePathname(coerce_pathname(obj))->pathname_##slot
#else
 #define COERCE_PATHNAME_SLOT(slot,obj,stack_res)       \
   stack_res = ThePathname(obj)->pathname_##slot
#endif

/* (MAKE-PATHNAME [:host] [:device] [:directory] [:name] [:type] [:version]
                [:defaults] [:case]),
 CLTL p. 416, CLtL2 p. 643 */
LISPFUN(make_pathname,seclass_read,0,0,norest,key,8,
        (kw(defaults),kw(case),kw(host),kw(device),kw(directory),
         kw(name),kw(type),kw(version)) )
{ /* stack layout: defaults, case, host, device, directory,
     name, type, version. */
  var bool logical = false;
  var bool convert = eq(STACK_6,S(Kcommon));
  /* 0. check defaults (STACK_7): */
  if (boundp(STACK_7)) {
   #ifdef LOGICAL_PATHNAMES
    if (!nullpSv(parse_namestring_ansi)
        && stringp(STACK_7) && looks_logical_p(STACK_7))
      STACK_7 = parse_as_logical(STACK_7);
    else
   #endif
      STACK_7 = coerce_xpathname(STACK_7);
  }
  /* 1. check host: */
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(STACK_5)) {
    STACK_5 = TheLogpathname(STACK_5)->pathname_host;
    logical = true;
  }
 #endif
  if (!boundp(STACK_5)) {
    var object d_path = defaults_pathname();
    STACK_5 = (!boundp(STACK_7) ?
               xpathname_host(logpathnamep(d_path),d_path) :
               xpathname_host(logpathnamep(STACK_7),STACK_7));
  } else {
   #if HAS_HOST
    STACK_5 = test_optional_host(STACK_5,convert);
   #else
    STACK_5 = test_optional_host(STACK_5);
   #endif
  }
 #ifdef LOGICAL_PATHNAMES
  if (!nullp(STACK_5) && logical_host_p(STACK_5)) {
    logical = true; STACK_5 = string_upcase(STACK_5);
  }
 #endif
  DOUT("make-pathname:[version]",STACK_0);
  DOUT("make-pathname:[type]",STACK_1);
  DOUT("make-pathname:[name]",STACK_2);
  DOUT("make-pathname:[directory]",STACK_3);
  DOUT("make-pathname:[device]",STACK_4);
  DOUT("make-pathname:[host]",STACK_5);
  DOUT("make-pathname:[case]",STACK_6);
  DOUT("make-pathname:[defaults]",STACK_7);
 #if HAS_DEVICE
  { /* 2. check device: */
    var object device = STACK_4;
    if (!boundp(device)) {
      if (!boundp(STACK_7)) /* no defaults? */
        STACK_4 = NIL; /* -> use NIL */
    } else {
      if (stringp(device))
        STACK_4 = device = coerce_normal_ss(device);
      if (convert)
        STACK_4 = device = common_case(device);
      if (nullp(device)) /* = NIL ? */
        goto device_ok;
     #ifdef LOGICAL_PATHNAMES
      else if (logical) {
        if (logpathnamep(device) /* Pathname -> its device */
            || (eq(device,S(Kunspecific)))) { /* :UNSPECIFIC -> NIL */
          STACK_4 = NIL; goto device_ok;
        }
      }
     #endif
     #ifdef PATHNAME_WIN32
      else if (eq(device,S(Kwild))) /* = :WILD ? */
        goto device_ok;
      else if (simple_string_p(device)) { /* Simple-String ? */
        if (Sstring_length(device) == 1) { /* of length 1 ? */
          var chart ch = schar(device,0);
          if ((as_cint(ch) >= 'A') && (as_cint(ch) <= 'Z')) /* with letters >='A' and <='Z' ? */
            goto device_ok;
        }
      }
     #endif
      else if (xpathnamep(device)) { /* Pathname -> its Device */
        COERCE_PATHNAME_SLOT(device,device,STACK_4);
        goto device_ok;
      }
      /* None of the desired cases -> error: */
      pushSTACK(STACK_4); pushSTACK(S(Kdevice)); goto fehler_arg;
       device_ok: ;
     #ifdef PATHNAME_WIN32
      if (!nullp(STACK_5) && !nullp(STACK_4)) {
        pushSTACK(STACK_4);
        pushSTACK(STACK_(5+1));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,
               GETTEXT("~S: on host ~S, device ~S is invalid, should be NIL"));
      }
     #endif
    }
  }
 #else /* HAS_DEVICE */
  {
    var object device = STACK_4;
    if (boundp(device)) { /* specified ? */
      if (!(nullp(device) || eq(device,S(Kunspecific))
            || xpathnamep(device))) { /* NIL or :UNSPECIFIC or Pathname -> OK */
        /* None of the desired cases -> error: */
        pushSTACK(STACK_4); pushSTACK(S(Kdevice)); goto fehler_arg;
      }
    }
  }
 #endif
  { /* 3. check directory: */
    DOUT("make-pathname:[directory]",STACK_3);
    var object directory = STACK_3;
    if (!boundp(directory) && boundp(STACK_7)) {
      /* not specified but defaults is supplied */
      goto directory_ok;
    } else if (missingp(directory)) { /* not specified or NIL */
      STACK_3 = NIL;                  /* default_directory == NIL */
      goto directory_ok;
    } else if (eq(directory,S(Kwild)) || eq(directory,S(Kwild_inferiors))) {
      directory = S(Kwild_inferiors);
      goto directory_add_absolute;
    } else if (stringp(directory)) {
      if (!legal_name(directory)) goto directory_bad;
      STACK_3 = directory = coerce_normal_ss(directory);
    directory_add_absolute:
      pushSTACK(S(Kabsolute));
      pushSTACK(directory);
      directory = listof(2); STACK_3 = directory;
      goto directory_ok;
    } else if (consp(directory)) { /* a Cons? */
      STACK_3 = directory = simplify_directory(copy_list(directory));
      if (convert)
        STACK_3 = directory = subst_common_case(directory);
      if (!directory_list_valid_p(logical,directory))
        goto directory_bad;
      else
        goto directory_ok;
    }
   #ifdef LOGICAL_PATHNAMES
    else if (logical) {
      if (logpathnamep(directory)) { /* Pathname -> its Directory */
        STACK_3 = TheLogpathname(directory)->pathname_directory;
        goto directory_ok;
      }
    }
   #endif
    else if (xpathnamep(directory)) { /* Pathname -> its Directory */
      COERCE_PATHNAME_SLOT(directory,directory,STACK_3);
      goto directory_ok;
    }
    /* None of the desired cases -> error: */
  directory_bad:
    pushSTACK(STACK_3); pushSTACK(S(Kdirectory)); goto fehler_arg;
  directory_ok: ;
  }
  { /* 4. check name: */
    DOUT("make-pathname:[name]",STACK_2);
    var object name = STACK_2;
    if (stringp(name))
      STACK_2 = name = coerce_normal_ss(name);
    if (convert)
      STACK_2 = name = common_case(name);
    if (!boundp(name)) { /* not specified */
        if (!boundp(STACK_7)) /* no defaults? */
          STACK_2 = NIL; /* -> use NIL */
    } else if (equal(name,O(empty_string))) { /* name = "" ? */
      STACK_2 = NIL; /* -> use NIL */
    } else if (nullp(name)) { /* NIL is OK */
    }
   #ifdef LOGICAL_PATHNAMES
    else if (logical) {
      if (legal_logical_word(name)) { /* OK */
      } else if (logpathnamep(name)) { /* Pathname -> its Name */
        STACK_2 = TheLogpathname(name)->pathname_name;
      } else { /* None of the desired cases -> error: */
        pushSTACK(STACK_2); pushSTACK(S(Kname)); goto fehler_arg;
      }
    }
   #endif
   #ifdef PATHNAME_NOEXT
    else if (eq(name,S(Kwild))) { /* :WILD is OK */
    }
   #endif
    else if (legal_name(name)) { /* admissible Name is OK */
      STACK_2 = name = coerce_normal_ss(name);
    } else if (xpathnamep(name)) { /* Pathname -> its Name */
      COERCE_PATHNAME_SLOT(name,name,STACK_2);
    } else { /* None of the desired cases -> error: */
      pushSTACK(STACK_2); pushSTACK(S(Kname)); goto fehler_arg;
    }
  }
  { /* 5. check type: */
    DOUT("make-pathname:[type]",STACK_1);
    var object type = STACK_1;
    if (stringp(type))
      STACK_1 = type = coerce_normal_ss(type);
    if (convert)
      STACK_1 = type = common_case(type);
    if (!boundp(type)) {
      if (!boundp(STACK_7)) /* no Defaults ? */
        STACK_1 = NIL; /* -> use NIL */
    } else if (nullp(type)) { /* NIL is OK */
    }
   #ifdef LOGICAL_PATHNAMES
    else if (logical) {
      if (legal_logical_word(type)) { /* OK */
      } else if (logpathnamep(type)) { /* Pathname -> its Type */
        STACK_1 = TheLogpathname(type)->pathname_type;
      } else { /* None of the desired cases -> error: */
        pushSTACK(STACK_1); pushSTACK(S(Ktype)); goto fehler_arg;
      }
    }
   #endif
   #ifdef PATHNAME_NOEXT
    else if (eq(type,S(Kwild))) { /* :WILD is OK */
    }
   #endif
    else if (legal_type(type)) {
    } else if (xpathnamep(type)) { /* Pathname -> its Type */
      COERCE_PATHNAME_SLOT(type,type,STACK_1);
    } else { /* None of the desired cases -> error: */
      pushSTACK(STACK_1); pushSTACK(S(Ktype)); goto fehler_arg;
    }
  }
  /* 6. check version: */
  STACK_0 = test_optional_version(!boundp(STACK_7) ? NIL : unbound);
  DOUT("make-pathname:[ver]",STACK_0);
  DOUT("make-pathname:[ver]",STACK_7);
  { /* 7. build Pathname: */
    var object pathname;
   #ifdef LOGICAL_PATHNAMES
    if (logical) {
      pathname = allocate_logpathname(); /* new Logical Pathname */
      TheLogpathname(pathname)->pathname_version   = popSTACK();
      TheLogpathname(pathname)->pathname_type      = popSTACK();
      TheLogpathname(pathname)->pathname_name      = popSTACK();
      TheLogpathname(pathname)->pathname_directory = popSTACK();
      skipSTACK(1);
      TheLogpathname(pathname)->pathname_host      = popSTACK();
    } else
   #endif
    {
      pathname = allocate_pathname(); /* new Pathname */
      ThePathname(pathname)->pathname_version   = popSTACK();
      ThePathname(pathname)->pathname_type      = popSTACK();
      ThePathname(pathname)->pathname_name      = popSTACK();
      ThePathname(pathname)->pathname_directory = popSTACK();
     #if HAS_DEVICE
      ThePathname(pathname)->pathname_device    = popSTACK();
     #else
      skipSTACK(1);
     #endif
     #if HAS_HOST
      ThePathname(pathname)->pathname_host      = popSTACK();
     #else
      skipSTACK(1);
     #endif
    }
    STACK_0 = pathname; /* forget case */
    DOUT("make-pathname:[pathname]",STACK_0);
    DOUT("make-pathname:[defaults]",STACK_1);
    pathname = popSTACK();
    /* 8. poss. merge in Defaults: */
    var object defaults = popSTACK();
    if (!boundp(defaults)) { /* no defaults given -> pathname is the value */
      value1 = pathname;
    } else {
      /* (MERGE-PATHNAMES pathname defaults [nil] :wild #'make-pathname) */
      pushSTACK(pathname); pushSTACK(defaults);
      pushSTACK(unbound); pushSTACK(S(Kwild)); pushSTACK(L(make_pathname));
      funcall(L(merge_pathnames),5);
    }
    mv_count=1;
    DOUT("make-pathname:[ret]",value1);
    return;
  }
 fehler_arg: /* error-message: */
  pushSTACK(TheSubr(subr_self)->name);
  fehler(error,GETTEXT("~S: illegal ~S argument ~S"));
}
#undef COERCE_PATHNAME_SLOT

#ifdef LOGICAL_PATHNAMES

/* (MAKE-LOGICAL-PATHNAME [:host] [:device] [:directory] [:name]
                          [:type] [:version] [:defaults] [:case]),
 like MAKE-PATHNAME, except that a Logical Pathname is built. */
LISPFUN(make_logical_pathname,seclass_read,0,0,norest,key,8,
        (kw(defaults),kw(case),kw(host),kw(device),
         kw(directory),kw(name),kw(type),kw(version)) )
{ /* A logical pathname as :HOST-Argument for MAKE-PATHNAME
   enforces a logical pathname as result. */
  if (boundp(STACK_5)) STACK_5 = string_upcase(STACK_5); /* host */
  else STACK_5 = NIL;
  { var object obj = allocate_logpathname();
    TheLogpathname(obj)->pathname_host = STACK_5;
    STACK_5 = obj; }
  /* PATHNAME-DEVICE for LOGICAL-PATHNAME returns :UNSPECIFIC, so
     MAKE-LOGICAL-PATHNAME must accept :DEVICE :UNSPECIFIC */
  if (eq(STACK_4,S(Kunspecific))) STACK_4 = NIL; /* device */
  /* continue at MAKE-PATHNAME. */
  C_make_pathname();
}

#endif

#ifdef USER_HOMEDIR
/* (USER-HOMEDIR-PATHNAME [host]), CLTL p. 418 */
LISPFUN(user_homedir_pathname,seclass_default,0,1,norest,nokey,0,NIL) {
  DOUT("user-homedir-pathname:[host]",STACK_0);
 #if HAS_HOST
  STACK_0 = test_optional_host(STACK_0,false); /* check Host */
  if (!nullp(STACK_0)) {
   #if defined(PATHNAME_WIN32)
    /* This is very primitive. Does Windows have the notion of homedirs on
     remote hosts?? */
    {
      var object pathname = allocate_pathname(); /* new Pathname */
      ThePathname(pathname)->pathname_host      = popSTACK();
     #if HAS_DEVICE
      ThePathname(pathname)->pathname_device    = NIL;
     #endif
      ThePathname(pathname)->pathname_directory = O(directory_absolute);
      ThePathname(pathname)->pathname_name      = NIL;
      ThePathname(pathname)->pathname_type      = NIL;
      ThePathname(pathname)->pathname_version   = NIL;
      VALUES1(pathname);
    }
   #else
      ??; /* FIXME for HAS_HOST & not WIN32 */
   #endif
  } else { /* no host given */
    skipSTACK(1);
    VALUES1(O(user_homedir)); /* User-Homedir-Pathname */
  }
 #else /* HAS_HOST */
  test_optional_host(popSTACK()); /* check Host and ignore */
  VALUES1(O(user_homedir)); /* User-Homedir-Pathname */
 #endif
  DOUT("user-homedir-pathname:[ret]",value1);
}
#endif

/* UP: copies a pathname.
 copy_pathname(pathname)
 > pathname: non-logical pathname
 < result: copy of the pathname, with the same components
 can trigger GC */
local maygc object copy_pathname (object pathname) {
  pushSTACK(pathname);
  var object newp = allocate_pathname();
  pathname = popSTACK();
 #if HAS_HOST
  ThePathname(newp)->pathname_host
    = ThePathname(pathname)->pathname_host;
 #endif
 #if HAS_DEVICE
  ThePathname(newp)->pathname_device
    = ThePathname(pathname)->pathname_device;
 #endif
  ThePathname(newp)->pathname_directory
    = ThePathname(pathname)->pathname_directory;
  ThePathname(newp)->pathname_name
    = ThePathname(pathname)->pathname_name;
  ThePathname(newp)->pathname_type
    = ThePathname(pathname)->pathname_type;
  ThePathname(newp)->pathname_version
    = ThePathname(pathname)->pathname_version;
  return newp;
}

/*
 * Wildcards
 * =========
 */

#ifdef PATHNAME_NOEXT
/* UP: check whether the object is wild
 wild_p(object)
 > object: normal simple-string or symbol
 < return: true when the object is wild */
local bool wild_p (object obj, bool dirp) {
  if (simple_string_p(obj)) {
    var uintL len = Sstring_length(obj);
    if (len > 0) {
      SstringDispatch(obj,X, {
        var const cintX* charptr = &((SstringX)TheVarobject(obj))->data[0];
        dotimespL(len,len, {
          var chart ch = as_chart(*charptr++);
          if (wild_char_p(ch))
            return true;
        });
      });
    }
    return false;
  } else
    return eq(obj,S(Kwild)) || (dirp && eq(obj,S(Kwild_inferiors)));
}
#endif

#ifdef LOGICAL_PATHNAMES
/* UP: check whether the obj is a string with '*' or a :WILD
 word_wild_p(object)
 > object: normal simple-string or symbol
 < return: true when the object is word-wild */
local bool word_wild_p (object obj, bool dirp) {
  if (simple_string_p(obj)) {
    var uintL len = Sstring_length(obj);
    if (len > 0) {
      SstringDispatch(obj,X, {
        var const cintX* charptr = &((SstringX)TheVarobject(obj))->data[0];
        dotimespL(len,len, {
          if (starp(as_chart(*charptr++)))
            return true;
        });
      });
    }
    return false;
  } else
    return eq(obj,S(Kwild)) || (dirp && eq(obj,S(Kwild_inferiors)));
}
#endif

/* UP: checks, if the host-component of a pathname contains wildcards.
 has_host_wildcards(pathname)
 > pathname: pathname
 < result: true if (PATHNAME-HOST pathname) contains wildcards. */
local bool has_host_wildcards (object pathname);
  /* host can not contain wildcards. */
#define has_host_wildcards(pathname)  (unused (pathname), false)

/* UP: checks, if the device-component of a pathname contains wildcards.
 has_device_wildcards(pathname)
 > pathname: pathname
 < result: true if (PATHNAME-DEVICE pathname) contains wildcards. */
local bool has_device_wildcards (object pathname) {
 #ifdef PATHNAME_WIN32
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(pathname))
    return false;
 #endif
  /* check device: = :WILD ? */
  return eq(ThePathname(pathname)->pathname_device,S(Kwild));
 #else
  return false;
 #endif
}

/* UP: checks, if the directory-component of a pathname contains wildcards.
 has_directory_wildcards(pathname)
 > pathname: pathname
 < result: true if (PATHNAME-DIRECTORY pathname) contains wildcards. */
local bool has_directory_wildcards (object pathname) {
  /* check directory: */
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(pathname)) {
    var object directory = TheLogpathname(pathname)->pathname_directory;
    for (;consp(directory); directory = Cdr(directory))
      if (word_wild_p(Car(directory),true))
        return true;
    return false;
  }
 #endif
  var object directory = ThePathname(pathname)->pathname_directory;
  for (;consp(directory); directory = Cdr(directory))
    if (wild_p(Car(directory),true))
      return true;
  return false;
}

/* UP: checks, if the name-component of a pathname contains wildcards.
 has_name_wildcards(pathname)
 > pathname: pathname
 < result: true if (PATHNAME-NAME pathname) contains wildcards. */
local bool has_name_wildcards (object pathname) {
  /* check name: */
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(pathname))
    return word_wild_p(TheLogpathname(pathname)->pathname_name,false);
 #endif
 #ifdef PATHNAME_NOEXT
  return wild_p(ThePathname(pathname)->pathname_name,false);
 #endif
  return false;
}

/* UP: checks, if the type-component of a pathname contains wildcards.
 has_type_wildcards(pathname)
 > pathname: pathname
 < result: true if (PATHNAME-TYPE pathname) contains wildcards. */
local bool has_type_wildcards (object pathname) {
  /* check type: */
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(pathname))
    return word_wild_p(TheLogpathname(pathname)->pathname_type,false);
 #endif
 #ifdef PATHNAME_NOEXT
  return wild_p(ThePathname(pathname)->pathname_type,false);
 #endif
  return false;
}

/* UP: checks, if the version-component of a pathname contains wildcards.
 has_version_wildcards(pathname)
 > pathname: pathname
 < result: true if (PATHNAME-VERSION pathname) contains wildcards. */
local bool has_version_wildcards (object pathname) {
  /* check version: */
  return eq(S(Kwild),xpathname_version(logpathnamep(pathname),pathname));
}

/* UP: checks, if any component of a pathname contains wildcards.
 has_some_wildcards(pathname)
 > pathname: pathname
 < result: true if pathname contains wildcards. */
local bool has_some_wildcards (object pathname) {
  if (has_host_wildcards(pathname)) return true;
  if (has_device_wildcards(pathname)) return true;
  if (has_directory_wildcards(pathname)) return true;
  if (has_name_wildcards(pathname)) return true;
  if (has_type_wildcards(pathname)) return true;
  if (has_version_wildcards(pathname)) return true;
  return false;
}

/* UP: checks, if a pathname contains no wildcards.
 check_no_wildcards(pathname);
 > pathname: pathname */
local void check_no_wildcards (object pathname) {
  if (!has_some_wildcards(pathname)) /* no wildcards found. */
    return;
  /* error-message, if the pathname contains wildcards: */
  pushSTACK(pathname); /* FILE-ERROR slot PATHNAME */
  pushSTACK(pathname);
  fehler(file_error,GETTEXT("wildcards are not allowed here: ~S"));
}

LISPFUN(wild_pathname_p,seclass_read,1,1,norest,nokey,0,NIL)
{ /* (WILD-PATHNAME-P pathname [field-key]), CLtL2 p. 623 */
  var object pathname = coerce_xpathname(STACK_1);
  var object key = STACK_0;
  var bool erg;
  if (missingp(key)) {
    erg = has_some_wildcards(pathname);
  } else if (eq(key,S(Khost))) {
    erg = has_host_wildcards(pathname);
  } else if (eq(key,S(Kdevice))) {
    erg = has_device_wildcards(pathname);
  } else if (eq(key,S(Kdirectory))) {
    erg = has_directory_wildcards(pathname);
  } else if (eq(key,S(Kname))) {
    erg = has_name_wildcards(pathname);
  } else if (eq(key,S(Ktype))) {
    erg = has_type_wildcards(pathname);
  } else if (eq(key,S(Kversion))) {
    erg = has_version_wildcards(pathname);
  } else {
    pushSTACK(key);                        /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_pathname_field_key)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(NIL);
    pushSTACK(S(Kversion));
    pushSTACK(S(Ktype));
    pushSTACK(S(Kname));
    pushSTACK(S(Kdirectory));
    pushSTACK(S(Kdevice));
    pushSTACK(S(Khost));
    pushSTACK(key);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~S: argument ~S should be ~S, ~S, ~S, ~S, ~S, ~S or ~S"));
  }
  VALUES_IF(erg); /* boolean value */
  skipSTACK(2);
}

/* Wildcard Matching
 ================= */

/* For the purposes of wildcard matching, according to CLHS, non-present
 components (i.e. NIL or a directory = (:RELATIVE)) are treated as wild. */

#if defined(PATHNAME_NOEXT) || defined(LOGICAL_PATHNAMES)

  /* UP: Matches a Wildcard-String ("Pattern") with a "Sample".
   > pattern : Normal-Simple-String, with wildcards
             '?' for exactly 1 character
             '*' for arbitrary many characters
   > sample  : Normal-Simple-String, that has to be matched
   recursive implementation because of backtracking: */
local bool wildcard_match_ab (uintL m_count, const chart* m_ptr,
                              uintL b_count, const chart* b_ptr);
local bool wildcard_match (object pattern, object sample) {
  if (eq(pattern,S(Kwild)) || eq(pattern,S(Kwild_inferiors)))
    return true;
  if (eq(pattern,S(Kup)) || eq(pattern,S(Kback)))
    return false;
  ASSERT(sstring_normal_p(pattern));
  ASSERT(sstring_normal_p(sample));
  return wildcard_match_ab(
                           /* m_count = */ Sstring_length(pattern),
                           /* m_ptr   = */ &TheSnstring(pattern)->data[0],
                           /* b_count = */ Sstring_length(sample),
                           /* b_ptr   = */ &TheSnstring(sample)->data[0]
                                           );
}
local bool wildcard_match_ab (uintL m_count, const chart* m_ptr,
                              uintL b_count, const chart* b_ptr) {
  var chart c;
  loop {
    if (m_count==0)
      return (b_count==0); /* "" matches only "" */
    m_count--;
    c = *m_ptr++; /* next match-character */
    if (chareq(c,ascii('?'))) { /* wildcard '?' */
      if (b_count==0) return false; /* at least one character still has to come */
      b_count--; b_ptr++; /* it will be ignored */
    } else if (starp(c))
      break; /* wildcard '*' later */
    else { /* everything else must match exactly: */
      if (b_count==0) return false;
      b_count--; if (!equal_pathchar(*b_ptr++,c)) return false;
    }
  }
  /* Wildcard '*': Search next non-wildcard-character and also count the '?'
   (because a sequence '*??*???***?' matches everything, that is as least as
   long as the sequence of question marks). The '?' can also be utilized
   immediately, because '*??*???***?' is equivalent to '??????*' . */
  loop {
    if (m_count==0) return true; /* wildcard at the end matches the rest. */
    m_count--;
    c = *m_ptr++; /* next match-character */
    if (chareq(c,ascii('?'))) {
      /* question mark: move forward, process instantly */
      if (b_count==0) return false;
      b_count--; b_ptr++;
    }
    else if (!starp(c))
      break;
  }
  /* c = next non-wildcard-character. Search it. */
  loop {
    if (b_count==0) return false; /* c not found */
    b_count--;
    if (equal_pathchar(*b_ptr++,c)) {
      if (wildcard_match_ab(m_count,m_ptr,b_count,b_ptr))
        return true;
    }
  }
}

#endif

/* UPs: matches a pathname-component ("Sample") and
 a pathname-component ("Pattern") at a time. */
local bool host_match (object pattern, object sample, bool logical);
local bool device_match (object pattern, object sample, bool logical);
local bool directory_match (object pattern, object sample, bool logical);
local bool nametype_match (object pattern, object sample, bool logical);
local bool version_match (object pattern, object sample, bool logical);
local bool host_match (object pattern, object sample, bool logical)
{/* logical is ignored */
 #if defined(LOGICAL_PATHNAMES) || HAS_HOST
  if (nullp(pattern)) return true;
  return equal(pattern,sample);
 #else
  return true;
 #endif
}
local bool device_match (object pattern, object sample, bool logical) {
 #if HAS_DEVICE
  #ifdef LOGICAL_PATHNAMES
  if (logical) {
    return true;
  }
  #endif
  if (nullp(pattern)) return true;
  #ifdef PATHNAME_WIN32
  if (eq(pattern,S(Kwild))) return true;
  if (eq(sample,S(Kwild))) return false;
  #endif
  #ifdef PATHNAME_WIN32
  return equalp(pattern,sample);
  #else
  return equal(pattern,sample);
  #endif
 #else
  return true;
 #endif
}
local bool nametype_match_aux (object pattern, object sample, bool logical)
{ /* logical is ignored */
 #if defined(LOGICAL_PATHNAMES) || defined(PATHNAME_NOEXT)
  if (eq(pattern,S(Kwild))) return true;
  if (eq(sample,S(Kwild))) return false;
  if (nullp(pattern)) {
    if (nullp(sample))
      return true;
    else
      return false;
  }
  if (nullp(sample))
    return false;
  return wildcard_match(pattern,sample);
 #else
  return true;                 /* when do we get here?! */
 #endif
}
local bool subdir_match (object pattern, object sample, bool logical)
{ /* logical is ignored */
  if (eq(pattern,sample)) return true;
 #if defined(LOGICAL_PATHNAMES) || defined(PATHNAME_NOEXT)
  if (eq(pattern,S(Kwild))) return true;
  if (!simple_string_p(pattern) || !simple_string_p(sample)) return false;
  return wildcard_match(pattern,sample);
 #else
  return true;                 /* when do we get here?! */
 #endif
}
/* recursive implementation because of backtracking: */
local bool directory_match_ab (object m_list, object b_list, bool logical);
local bool directory_match_ab (object m_list, object b_list, bool logical) {
  /* Algorithm analogous to wildcard_match_ab. */
  var object item;
  loop {
    if (atomp(m_list)) { return atomp(b_list); }
    item = Car(m_list); m_list = Cdr(m_list);
    if (eq(item,S(Kwild_inferiors))) break;
    if (atomp(b_list)) return false;
    if (!subdir_match(item,Car(b_list),logical)) return false;
    b_list = Cdr(b_list);
  }
  loop {
    if (atomp(m_list)) return true;
    item = Car(m_list); m_list = Cdr(m_list);
    if (!eq(item,S(Kwild_inferiors))) break;
  }
  loop {
    if (atomp(b_list)) return false;
    if (subdir_match(item,Car(b_list),logical)) {
      b_list = Cdr(b_list);
      if (directory_match_ab(m_list,b_list,logical)) return true;
    } else {
      b_list = Cdr(b_list);
    }
  }
}
local inline bool directory_trivial_p (object dir) {
  return nullp(dir)
    || (consp(dir) ? (eq(Car(dir),S(Krelative)) && nullp(Cdr(dir))) : false);
}
local bool directory_match (object pattern, object sample, bool logical) {
  if (nullp(pattern)) /* compare pattern with directory_default */
    return true;
  if (missingp(sample)) return true;
  /* match startpoint: */
  if (!eq(Car(pattern),Car(sample)))
    return false;
  pattern = Cdr(pattern); sample = Cdr(sample);
  /* match subdirs: */
  return directory_match_ab(pattern,sample,logical);
}
local bool nametype_match (object pattern, object sample, bool logical) {
  if (missingp(pattern)) return true;
  return nametype_match_aux(pattern,sample,logical);
}
local bool version_match (object pattern, object sample, bool logical)
{ /* logical is ignored */
  SDOUT("version_match:",pattern);
  SDOUT("version_match:",sample);
  if (!boundp(sample)) return true;
  if (nullp(pattern) || eq(pattern,S(Kwild))) return true;
  if (eq(sample,S(Kwild))) return false;
  return eql(pattern,sample);
}

LISPFUNNR(pathname_match_p,2)
{ /* (PATHNAME-MATCH-P pathname wildname), CLtL2 p. 623 */
  /* stack layout: pathname, wildname. */
  var bool logical = false;
  STACK_1 = coerce_xpathname(STACK_1);
  STACK_0 = coerce_xpathname(STACK_0);
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(STACK_1) && logpathnamep(STACK_0)) {
    logical = true;
  } else {
    /* not both logical pathnames -> first convert into normal pathnames: */
    STACK_1 = coerce_pathname(STACK_1);
    STACK_0 = coerce_pathname(STACK_0);
  }
 #endif
  DOUT("pathname-match-p:[s0]",STACK_0);
  DOUT("pathname-match-p:[s1]",STACK_1);
  var object wildname = popSTACK();
  var object pathname = popSTACK();
  if (!host_match(xpathname_host(logical,wildname),
                  xpathname_host(logical,pathname),
                  logical))
    goto no;
  if (!device_match(xpathname_device(logical,wildname),
                    xpathname_device(logical,pathname),
                    logical))
    goto no;
  if (!directory_match(xpathname_directory(logical,wildname),
                       xpathname_directory(logical,pathname),
                       logical))
    goto no;
  if (!nametype_match(xpathname_name(logical,wildname),
                      xpathname_name(logical,pathname),
                      logical))
    goto no;
  if (!nametype_match(xpathname_type(logical,wildname),
                      xpathname_type(logical,pathname),
                      logical))
    goto no;
  if (!version_match(xpathname_version(logical,wildname),
                     xpathname_version(logical,pathname),
                     logical))
    goto no;
 yes:
  VALUES1(T); return;
 no:
  VALUES1(NIL); return;
}

/* (TRANSLATE-PATHNAME sample pattern1 pattern2) implemented as follows:
 1. (PATHNAME-MATCH-P sample pattern1) while checking, extract
    text items from the substitution pattern (:WILD -> "*").
 2. Put the text items into pattern2 until pattern2 is full or all the
    text items are used up
 3. finally, (MERGE-PATHNAMES modified_pattern2 sample). */

  /* UP: Compare a wildcard string ("Pattern") with "Sample".
   wildcard_diff(pattern,sample,previous,solutions);
   > pattern: normal simple string, with substitution characters
             '?' for exactly 1 character
             '*' for as many characters as desired
   > sample: normal simple string, to compare with
   > previous: the already known result of comparison
               (reversed list of normal simple strings, NILs and lists)
   > solutions: address of a list in the STACK, onto which the results of
                the comparisons (reversed list of normal simple strings
                and lists) have to be consed
   can trigger GC */

/* Here you need not Lisp or C, but PROLOG!
 (PUSH previous solutions) */
#define push_solution()   do {                  \
      var object new_cons = allocate_cons();    \
      Car(new_cons) = *previous;                \
      Cdr(new_cons) = *solutions;               \
      *solutions = new_cons;                    \
    } while(0)
/* (PUSH (CONS new_piece previous) solutions) */
#define push_solution_with(new_piece)   do {                    \
      pushSTACK(new_piece);                                     \
     {var object new_cons = allocate_cons();                    \
      Car(new_cons) = STACK_0; Cdr(new_cons) = *previous;       \
      STACK_0 = new_cons;                                       \
      new_cons = allocate_cons();                               \
      Car(new_cons) = popSTACK(); Cdr(new_cons) = *solutions;   \
      *solutions = new_cons;                                    \
    }} while(0)

#if defined(PATHNAME_NOEXT) || defined(LOGICAL_PATHNAMES)

/* recursive implementation because of backtracking: */
local maygc void wildcard_diff_ab (object pattern, object sample,
                                   uintL m_index, uintL b_index,
                                   const gcv_object_t* previous,
                                   gcv_object_t* solutions) {
  var chart cc;
  loop {
    if (m_index == Sstring_length(pattern)) {
      if (b_index == Sstring_length(sample))
        push_solution();
      return;
    }
    cc = schar(pattern,m_index++);
    if (starp(cc))
      break;
    if (b_index == Sstring_length(sample))
      return;
    if (chareq(cc,ascii('?'))) {
      /* recursive call to wildcard_diff_ab(), with extended previous: */
      cc = schar(sample,b_index++);
      pushSTACK(pattern); pushSTACK(sample);
      {
        var object new_string = allocate_string(1);
        TheS32string(new_string)->data[0] = as_cint(cc);
        pushSTACK(new_string);
      }
      {
        var object new_cons = allocate_cons();
        Car(new_cons) = STACK_0; Cdr(new_cons) = *previous;
        STACK_0 = new_cons; /* (CONS ... previous) */
      }
      wildcard_diff_ab(STACK_2,STACK_1,m_index,b_index,&STACK_0,solutions);
      skipSTACK(3);
      return;
    } else {
      if (!equal_pathchar(schar(sample,b_index++),cc))
        return;
    }
  }
  var uintL b_start_index = b_index;
  loop {
    /* to reduce consing, intercept cases when wildcard_diff_ab()
       does nothing */
    if (m_index == Sstring_length(pattern)
        ? b_index == Sstring_length(sample)
        : (cc = schar(pattern,m_index),
           starp(cc) || chareq(cc,ascii('?'))
           || (b_index < Sstring_length(sample)
               && equal_pathchar(schar(sample,b_index),cc)))) {
      /* wildcard_diff_ab() recursive call, with extended previous: */
      pushSTACK(pattern); pushSTACK(sample);
      /* (SUBSTRING sample b_start_index b_index) */
      pushSTACK(subsstring(sample,b_start_index,b_index));
      var object new_cons = allocate_cons();
      Car(new_cons) = STACK_0; Cdr(new_cons) = *previous;
      STACK_0 = new_cons; /* (CONS ... previous) */
      wildcard_diff_ab(STACK_2,STACK_1,m_index,b_index,&STACK_0,solutions);
      skipSTACK(1);
      sample = popSTACK(); pattern = popSTACK();
    }
    if (b_index == Sstring_length(sample))
      break;
    b_index++;
  }
}

local maygc void wildcard_diff (object pattern, object sample,
                                const gcv_object_t* previous,
                                gcv_object_t* solutions) {
  ASSERT(sstring_normal_p(pattern));
  ASSERT(sstring_normal_p(sample));
  wildcard_diff_ab(pattern,sample,0,0,previous,solutions);
}

#endif

#if DEBUG_TRANSLATE_PATHNAME>1
/* all arguments to *_diff are on stack - this should be safe */
#define DEBUG_DIFF(f)                                         \
  printf("\n* " #f " [logical: %d]\n",logical);               \
  DOUT("",pattern); DOUT("",sample); DOUT("",*previous); DOUT("",*solutions)
#else
#define DEBUG_DIFF(f)
#endif
/* UPs: compares a pathname-component ("Sample") and
 a pathname-component ("Pattern") at a time.
 can trigger GC */
local maygc void host_diff      (object pattern, object sample, bool logical,
                                 const gcv_object_t* previous,
                                 gcv_object_t* solutions);
local maygc void device_diff    (object pattern, object sample, bool logical,
                                 const gcv_object_t* previous,
                                 gcv_object_t* solutions);
local maygc void directory_diff (object pattern, object sample, bool logical,
                                 const gcv_object_t* previous,
                                 gcv_object_t* solutions);
local maygc void nametype_diff  (object pattern, object sample, bool logical,
                                 const gcv_object_t* previous,
                                 gcv_object_t* solutions);
local maygc void version_diff   (object pattern, object sample, bool logical,
                                 const gcv_object_t* previous,
                                 gcv_object_t* solutions);
local maygc void host_diff (object pattern, object sample, bool logical,
                            const gcv_object_t* previous, gcv_object_t* solutions) {
  DEBUG_DIFF(host_diff);
 #ifdef LOGICAL_PATHNAMES
  if (logical) {
    if (nullp(pattern)) {
      push_solution_with(sample); return;
    }
    if (!equal(pattern,sample)) return;
  } else
 #endif
  {
 #if HAS_HOST
    if (nullp(pattern)) {
      push_solution_with(sample); return;
    }
    if (!equal(pattern,sample)) return;
 #endif
  }
 #if HAS_HOST
  push_solution_with(S(Khost));
 #else
  push_solution();
 #endif
}
local maygc void device_diff (object pattern, object sample, bool logical,
                              const gcv_object_t* previous, gcv_object_t* solutions) {
  DEBUG_DIFF(device_diff);
 #ifdef LOGICAL_PATHNAMES
  if (logical) {
   #if HAS_DEVICE
    push_solution_with(S(Kdevice));
   #else
    push_solution();
   #endif
    return;
  }
 #endif
 #if HAS_DEVICE
  #ifdef PATHNAME_WIN32
  if (nullp(pattern) || eq(pattern,S(Kwild))) {
    var object string = wild2string(sample);
    push_solution_with(string);
    return;
  }
  if (eq(sample,S(Kwild))) return;
  #endif
  #ifdef PATHNAME_WIN32
  if (nullp(pattern)) {
    var object string = wild2string(sample);
    push_solution_with(string);
    return;
  }
  if (!equalp(pattern,sample)) return;
  #else
  if (!equal(pattern,sample)) return;
  #endif
  push_solution_with(S(Kdevice));
 #else /* HAS_DEVICE */
  push_solution();
 #endif
}
local maygc void nametype_diff_aux (object pattern, object sample, bool logical,
                                    const gcv_object_t* previous,
                                    gcv_object_t* solutions) {
 #if defined(LOGICAL_PATHNAMES) || defined(PATHNAME_NOEXT)
  unused(logical);
  if (eq(pattern,S(Kwild))) {
    var object string = wild2string(sample);
    push_solution_with(string);
    return;
  }
  if (eq(sample,S(Kwild))) return;
  if (nullp(pattern)) {
    if (nullp(sample))
      push_solution();
    return;
  }
  if (nullp(sample))
    return;
  wildcard_diff(pattern,sample,previous,solutions);
 #endif
}
local maygc void subdir_diff (object pattern, object sample, bool logical,
                              const gcv_object_t* previous, gcv_object_t* solutions)
{
  DEBUG_DIFF(subdir_diff);
  if (eq(pattern,sample)) {
    if (eq(sample,S(Kwild)))
      push_solution_with(O(wild_string));
    else
      push_solution();
    return;
  }
 #if defined(LOGICAL_PATHNAMES) || defined(PATHNAME_NOEXT)
  unused(logical);
  if (eq(pattern,S(Kwild))) {
    var object string = wild2string(sample);
    push_solution_with(string);
    return;
  }
  if (eq(sample,S(Kwild))) return;
  if (!simple_string_p(pattern) || !simple_string_p(sample)) return;
  wildcard_diff(pattern,sample,previous,solutions);
 #endif
}
/* recursive implementation because of backtracking: */
local maygc void directory_diff_ab (object m_list, object b_list, bool logical,
                                    const gcv_object_t* previous,
                                    gcv_object_t* solutions) {
  /* algorithm analogous to wildcard_diff_ab. */
  var object item;
  if (atomp(m_list)) {
    if (atomp(b_list))
      push_solution();
    return;
  }
  item = Car(m_list); m_list = Cdr(m_list);
  if (!eq(item,S(Kwild_inferiors))) {
    if (atomp(b_list)) return;
    pushSTACK(NIL); pushSTACK(m_list); pushSTACK(Cdr(b_list));
    subdir_diff(item,Car(b_list),logical,previous,&STACK_2);
    /* call directory_diff_ab() recursively, with extended previous: */
    while (mconsp(STACK_2)) {
      pushSTACK(Car(STACK_2));
      directory_diff_ab(STACK_(1+1),STACK_(0+1),logical,&STACK_0,solutions);
      skipSTACK(1);
      STACK_2 = Cdr(STACK_2);
    }
    skipSTACK(3);
  } else {
    pushSTACK(b_list); /* b_start_list := b_list */
    loop {
      /* to reduce consing, intercept cases when directory_diff_ab()
       does nothing: */
      if (atomp(m_list)
          ? atomp(b_list)
          : (eq(Car(m_list),S(Kwild_inferiors)) || !atomp(b_list))) {
        /* call directory_diff_ab() recursively, with extended previous: */
        pushSTACK(m_list); pushSTACK(b_list);
        pushSTACK(STACK_2); pushSTACK(b_list);
        funcall(L(ldiff),2); /* (LDIFF b_start_list b_list) */
        pushSTACK(value1);
        { /* (:DIRECTORY subdir1 ... subdirn) */
          var object new_piece = allocate_cons();
          Car(new_piece) = S(Kdirectory); Cdr(new_piece) = STACK_0;
          STACK_0 = new_piece;
        }
        {
          var object new_cons = allocate_cons();
          Car(new_cons) = STACK_0; Cdr(new_cons) = *previous;
          STACK_0 = new_cons; /* (CONS ... previous) */
          directory_diff_ab(STACK_2,STACK_1,logical,&STACK_0,solutions);
          skipSTACK(1);
          b_list = popSTACK(); m_list = popSTACK();
        }
      }
      if (atomp(b_list))
        break;
      b_list = Cdr(b_list);
    }
    skipSTACK(1);
  }
}
local maygc void directory_diff (object pattern, object sample, bool logical,
                                 const gcv_object_t* previous,
                                 gcv_object_t* solutions) {
  DEBUG_DIFF(directory_diff);
  if (missingp(sample)) { push_solution_with(pattern); return; }
  if (directory_trivial_p(pattern)) { /* compare with directory_default */
    /* Augment the solution with the sample list - starting
     with :ABSOLUTE or :RELATIVE, it will not fit for "**". */
    push_solution_with(sample);
    return;
  }
  /* compare startpoint: */
  if (!eq(Car(pattern),Car(sample)))
    return;
  pattern = Cdr(pattern); sample = Cdr(sample);
  /* compare subdirs: */
  directory_diff_ab(pattern,sample,logical,previous,solutions);
}
local maygc void nametype_diff (object pattern, object sample, bool logical,
                                const gcv_object_t* previous,
                                gcv_object_t* solutions) {
  DEBUG_DIFF(nametype_diff);
  if (!boundp(sample)) { push_solution_with(pattern); return; }
  if (nullp(pattern)) {
    var object string = wild2string(sample);
    push_solution_with(string);
    return;
  }
  nametype_diff_aux(pattern,sample,logical,previous,solutions);
}
local maygc void version_diff (object pattern, object sample, bool logical,
                               const gcv_object_t* previous, gcv_object_t* solutions)
{ /* logical is ignored */
  DEBUG_DIFF(version_diff);
  if (!boundp(sample)) { push_solution_with(pattern); return; }
  if (nullp(pattern) || eq(pattern,S(Kwild))) {
    push_solution_with(sample);
    return;
  }
  if (eq(sample,S(Kwild))) return;
  if (!eql(pattern,sample)) return;
  push_solution();
}

#undef push_solution_with
#undef push_solution
#undef DEBUG_DIFF

/* Each substitution is a list of Normal-Simple-Strings or Lists.
 (The Lists come into being with :WILD-INFERIORS in directory_diff().)
 A Normal-Simple-String fits only with '?' or '*' or :WILD,
 A List fits only with :WILD-INFERIORS. */

#ifdef LOGICAL_PATHNAMES

/* On insertion of pieces of normal pathnames in logical pathnames:
 Conversion to capital letters.
 logical_case(string)
 > string: Normal-Simple-String or Symbol/Number
 < result: converted Normal-Simple-String or the same Symbol/Number
 can trigger GC */
local maygc object logical_case (object string) {
  if (!simple_string_p(string))
    return string;
  return string_upcase(string);
}
/* The same, recursive like with SUBST: */
local maygc object subst_logical_case (object obj) {
  SUBST_RECURSE(logical_case(obj),subst_logical_case);
}

/* On insertion of pieces of logical pathnames in normal pathnames:
 Conversion to capital letters.
 customary_case(string)
 > string: Normal-Simple-String or Symbol/Number
 < result: converted Normal-Simple-String or the same Symbol/Number
 can trigger GC */
local maygc object customary_case (object string) {
  if (!simple_string_p(string))
    return string;
 #if defined(PATHNAME_UNIX) || defined(PATHNAME_WIN32)
  /* operating system with preference for lowercase letters */
  return string_downcase(string);
 #endif
}
/* The same, recursive like with SUBST: */
local maygc object subst_customary_case (object obj) {
  SUBST_RECURSE(customary_case(obj),subst_customary_case);
}

#endif

#undef SUBST_RECURSE

/* Apply substitution SUBST to the PATTERN.
 translate_pathname(&subst,pattern) */
local object translate_pathname (object* subst, object pattern);
/* Pop the CAR of *subst and return it. */
#define RET_POP(subst)  \
  { var object ret = Car(*subst); *subst = Cdr(*subst); return ret; }
/* is the value trivial enough to ensure a trivial action? */
#define TRIVIAL_P(val) (simple_string_p(val)||nullp(val))
/* is the value simple enough to ensure a simple action? */
#define SIMPLE_P(val) (TRIVIAL_P(val)||eq(val,S(Kwild)))
/* translate_host(&subst,pattern,logical) etc.
 returns the appropriate replacement for host etc.; shortens subst;
 returns nullobj on failure
 can trigger GC */
local maygc object translate_host (gcv_object_t* subst, object pattern,
                                   bool logical);
local maygc object translate_device (gcv_object_t* subst, object pattern,
                                     bool logical);
local maygc object translate_subdir (gcv_object_t* subst, object pattern,
                                     bool logical);
local maygc object translate_directory (gcv_object_t* subst, object pattern,
                                        bool logical);
local maygc object translate_nametype (gcv_object_t* subst, object pattern,
                                       bool logical);
local maygc object translate_version (gcv_object_t* subst, object pattern,
                                      bool logical);
#if DEBUG_TRANSLATE_PATHNAME
/* all arguments to translate_* should be on stack - this should be safe */
#define DEBUG_TRAN(f)                                         \
  printf("\n* " #f " [logical: %d]\n",logical);               \
  DOUT("",*subst); DOUT("",pattern)
#else
#define DEBUG_TRAN(f)
#endif
local maygc object translate_host (gcv_object_t* subst, object pattern,
                                   bool logical) {
  DEBUG_TRAN(translate_host);
#define TRAN_HOST(subst,pattern)                        \
        if (nullp(pattern) && mconsp(*subst)) {         \
          if (TRIVIAL_P(Car(*subst))) {                 \
            RET_POP(subst);                             \
          } else if (eq(Car(*subst),S(Khost))) {        \
            *subst = Cdr(*subst);                       \
            return pattern;                             \
          } else                                        \
            return nullobj;                             \
        }
 #ifdef LOGICAL_PATHNAMES
  if (logical) {
    TRAN_HOST(subst,pattern);
  } else
 #endif
  {
 #if HAS_HOST
    TRAN_HOST(subst,pattern);
 #endif
  }
 #if HAS_HOST
  if (eq(Car(*subst),S(Khost)))
    *subst = Cdr(*subst);
 #endif
  return pattern;
 #undef TRAN_HOST
}
local maygc object translate_device (gcv_object_t* subst, object pattern,
                                     bool logical) {
  DEBUG_TRAN(translate_device);
 #if HAS_DEVICE
  #ifdef LOGICAL_PATHNAMES
  if (logical) {
    if (eq(Car(*subst),S(Kdevice)))
      { *subst = Cdr(*subst); }
    return pattern;
  }
  #endif
  #ifdef PATHNAME_WIN32
  if (nullp(pattern) && mconsp(*subst))
  #else
  if ((nullp(pattern) || eq(pattern,S(Kwild))) && mconsp(*subst))
  #endif
    {
      if (TRIVIAL_P(Car(*subst))) {
        RET_POP(subst);
      } else if (eq(Car(*subst),S(Kdevice))) {
        *subst = Cdr(*subst);
        return pattern;
      } else
        return nullobj;
    }
  if (eq(Car(*subst),S(Kdevice)))
    *subst = Cdr(*subst);
 #endif
  return pattern;
}
local maygc object translate_nametype_aux (gcv_object_t* subst, object pattern,
                                           bool logical) {
  DEBUG_TRAN(translate_nametype_aux);
  if (eq(pattern,S(Kwild)) && mconsp(*subst)) {
    if (TRIVIAL_P(Car(*subst))) {
      var object erg = Car(*subst); *subst = Cdr(*subst);
      return erg;
    } else
      return nullobj;
  }
  if (simple_string_p(pattern)) {
    pushSTACK(pattern); /* save pattern */
    var gcv_object_t* pattern_ = &STACK_0;
    var uintL len = Sstring_length(pattern);
    var uintL index = 0;
    var uintL stringcount = 0; /* number of strings on the stack */
    loop {
      var uintL last_index = index;
      var chart cc;
      /* search next wildcard-character: */
      pattern = *pattern_;
      while (index != len) {
        cc = schar(pattern,index);
        if ((starp(cc) /* wildcard for arbitrary many characters */
             || (!logical && singlewild_char_p(cc))) /* wildcard for exactly one character */
            && mconsp(*subst))
          break;
        index++;
      }
      /* Next (SUBSTRING pattern last_index index) on the stack: */
      pushSTACK(subsstring(pattern,last_index,index));
      stringcount++;
      /* finished? */
      if (index == len)
        break;
      /* replace wildcard: */
      if (TRIVIAL_P(Car(*subst))) {
        var object s = Car(*subst);
        pushSTACK(nullp(s) ? (object)O(empty_string) : s);
        *subst = Cdr(*subst); stringcount++;
      } else {
        skipSTACK(stringcount+1); return nullobj;
      }
      index++;
    }
    value1 = string_concat(stringcount);
    skipSTACK(1); /* skip pattern */
    return value1;
  }
  return pattern;
}
local maygc object translate_subdir (gcv_object_t* subst, object pattern,
                                     bool logical) {
  DEBUG_TRAN(translate_subdir);
 #if defined(LOGICAL_PATHNAMES) || defined(PATHNAME_NOEXT)
  return translate_nametype_aux(subst,pattern,logical);
 #endif
}
local maygc object translate_directory (gcv_object_t* subst, object pattern,
                                        bool logical) {
  DEBUG_TRAN(translate_directory);
  /* compare pattern with directory_default: */
  if (nullp(pattern) && mconsp(*subst)) {
    var object list = Car(*subst); *subst = Cdr(*subst);
    return listp(list) ? copy_list(list) : nullobj;
  }
  /* if subst is :relative while pattern is :absolute,
     nothing is to be done */
  if (eq(Car(pattern),S(Kabsolute)) && mconsp(*subst)
      && directory_trivial_p(Car(*subst))) {
    *subst = Cdr(*subst);
    return copy_list(pattern);
  }
  var uintL itemcount = 0; /* number of items on the stack */
  /* Startpoint: */
  pushSTACK(Car(pattern)); pattern = Cdr(pattern); itemcount++;
  /* subdirs: */
  while (consp(pattern)) {
    var object item = Car(pattern);
    pattern = Cdr(pattern);
    if (eq(item,S(Kwild_inferiors))) {
      if (mconsp(*subst)) {
        if (consp(Car(*subst)) && eq(Car(Car(*subst)),S(Kdirectory))) {
          var object list = Cdr(Car(*subst)); *subst = Cdr(*subst);
          while (consp(list)) {
            pushSTACK(Car(list)); list = Cdr(list); itemcount++;
          }
        } else {
          skipSTACK(itemcount); return nullobj;
        }
      } else {
        pushSTACK(item); itemcount++;
      }
    } else {
      pushSTACK(pattern); /* save pattern */
      item = translate_subdir(subst,item,logical);
      if (eq(item,nullobj)) { skipSTACK(itemcount+1); return nullobj; }
      pattern = STACK_0; STACK_0 = item; itemcount++;
    }
  }
  return listof(itemcount);
}
local maygc object translate_nametype (gcv_object_t* subst, object pattern,
                                       bool logical) {
  DEBUG_TRAN(translate_nametype);
  if (nullp(pattern) && mconsp(*subst)) {
    if (SIMPLE_P(Car(*subst))) {
      RET_POP(subst);
    } else
      return nullobj;
  }
  return translate_nametype_aux(subst,pattern,logical);
}
local object translate_version (gcv_object_t* subst, object pattern,
                                bool logical)
{ /* logical is ignored */
  DEBUG_TRAN(translate_version);
  if ((nullp(pattern) || eq(pattern,S(Kwild))) && mconsp(*subst)) {
    var object erg = Car(*subst);
    if (nullp(erg) || integerp(erg)
        || eq(erg,S(Kwild)) || eq(erg,S(Knewest))) {
      *subst = Cdr(*subst);
      return erg;
    } else
      return nullobj;
  }
  return pattern;
}
#undef SIMPLE_P
#undef TRIVIAL_P
#undef RET_POP
#undef DEBUG_TRAN
local maygc object translate_pathname (gcv_object_t* subst, object pattern) {
  var bool logical = false;
  var object item;
  pushSTACK(*subst); /* save subst for the error message */
  pushSTACK(pattern);
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(pattern))
    logical = true;
 #endif
#define GET_ITEM(what,xwhat,where,skip)                                     \
   item = translate_##what(subst,xpathname_##xwhat(logical,where),logical); \
   if (eq(item,nullobj)) { skipSTACK(skip); goto subst_error; }             \
   DOUT(#what " > ",item); pushSTACK(S(K##xwhat)); pushSTACK(item)
#define GET_ITEM_S(y,x,w) GET_ITEM(y,x,STACK_(w),w)
  /* build together arguments for MAKE-PATHNAME: */
  GET_ITEM(host,host,pattern,0);
 #if HAS_DEVICE
  GET_ITEM_S(device,device,2);
 #endif
  GET_ITEM_S(directory,directory,2+2*HAS_DEVICE);
  GET_ITEM_S(nametype,name,2+2*HAS_DEVICE+2);
  GET_ITEM_S(nametype,type,2+2*HAS_DEVICE+4);
  GET_ITEM_S(version,version,2+2*HAS_DEVICE+6);
  /* All replacement pieces must be consumed! */
  if (mconsp(*subst)) { skipSTACK(2+2*HAS_DEVICE+8); goto subst_error; }
  /* call (MAKE-PATHNAME ...) resp. (SYS::MAKE-LOGICAL-PATHNAME ...) : */
 #ifdef LOGICAL_PATHNAMES
  if (logical)
    funcall(L(make_logical_pathname),2+2*HAS_DEVICE+8);
  else
 #endif
    funcall(L(make_pathname),2+2*HAS_DEVICE+8);
  skipSTACK(2);
  return value1;
 subst_error: /* Error because of nullobj. */
  /* stack layout: subst, pattern. */
  pushSTACK(STACK_1);
  pushSTACK(S(translate_pathname));
  fehler(error,GETTEXT("~S: replacement pieces ~S do not fit into ~S"));
}
#undef GET_ITEM
#undef GET_ITEM_S

/* (TRANSLATE-PATHNAME sample pattern1 pattern2 [:all] [:merge] [:absolute]),
   CLtL2 p. 624
 :absolute = T --> convert the resulting pathnames to absolute
 :all = T --> return a list of all fitting pathnames
 :all = NIL --> Error, if more than one pathname fits
 :merge = NIL --> skip last MERGE-PATHNAMES step */
LISPFUN(translate_pathname,seclass_default,3,0,norest,key,3,
        (kw(all),kw(merge),kw(absolute)))
{ /* stack layout: sample, pattern1, pattern2, all, merge, absolute. */
  var bool absolute_p = !missingp(STACK_0);
  var bool logical = false;  /* sample and pattern are logical pathnames */
  var bool logical2 = false; /* pattern2 is a logical pathname */
  skipSTACK(1);              /* drop absolute */
  STACK_4 = coerce_xpathname(STACK_4);
  STACK_3 = coerce_xpathname(STACK_3);
  STACK_2 = coerce_xpathname(STACK_2);
 #ifdef LOGICAL_PATHNAMES
  if (logpathnamep(STACK_4) && logpathnamep(STACK_3)) {
    logical = true;
  } else {
    /* not both logical pathnames -> first convert into normal pathnames: */
    STACK_4 = coerce_pathname(STACK_4);
    STACK_3 = coerce_pathname(STACK_3);
  }
  if (logpathnamep(STACK_2))
    logical2 = true;
 #endif
  /* 1. step: construct list of all fitting substitutions. */
  pushSTACK(NIL); pushSTACK(NIL);
  host_diff(xpathname_host(logical,STACK_(3+2)),
            xpathname_host(logical,STACK_(4+2)),
            logical,&STACK_1,&STACK_0);
  while (mconsp(STACK_0)) {
    pushSTACK(Car(STACK_0)); pushSTACK(NIL);
    device_diff(xpathname_device(logical,STACK_(3+4)),
                xpathname_device(logical,STACK_(4+4)),
                logical,&STACK_1,&STACK_0);
    while (mconsp(STACK_0)) {
      pushSTACK(Car(STACK_0)); pushSTACK(NIL);
      directory_diff(xpathname_directory(logical,STACK_(3+6)),
                     xpathname_directory(logical,STACK_(4+6)),
                     logical,&STACK_1,&STACK_0);
      while (mconsp(STACK_0)) {
        pushSTACK(Car(STACK_0)); pushSTACK(NIL);
        nametype_diff(xpathname_name(logical,STACK_(3+8)),
                      xpathname_name(logical,STACK_(4+8)),
                      logical,&STACK_1,&STACK_0);
        while (mconsp(STACK_0)) {
          pushSTACK(Car(STACK_0)); pushSTACK(NIL);
          nametype_diff(xpathname_type(logical,STACK_(3+10)),
                        xpathname_type(logical,STACK_(4+10)),
                        logical,&STACK_1,&STACK_0);
          while (mconsp(STACK_0)) {
            pushSTACK(Car(STACK_0));
            version_diff(xpathname_version(logical,STACK_(3+11)),
                         xpathname_version(logical,STACK_(4+11)),
                         logical,&STACK_0,&STACK_10);
            skipSTACK(1);
            STACK_0 = Cdr(STACK_0);
          }
          skipSTACK(2);
          STACK_0 = Cdr(STACK_0);
        }
        skipSTACK(2);
        STACK_0 = Cdr(STACK_0);
      }
      skipSTACK(2);
      STACK_0 = Cdr(STACK_0);
    }
    skipSTACK(2);
    STACK_0 = Cdr(STACK_0);
  }
  skipSTACK(1);
  /* stack layout: ..., solutions. */
  if (matomp(STACK_0)) {
    pushSTACK(STACK_(3+1));
    pushSTACK(STACK_(4+1+1));
    pushSTACK(S(translate_pathname));
    fehler(error,GETTEXT("~S: ~S is not a specialization of ~S"));
  }
  /* 2.,3. step: */
  pushSTACK(NIL); /* pathnames := '() */
  while (mconsp(STACK_1)) { /* traverse solutions */
    var object solutions = STACK_1;
    STACK_1 = Cdr(solutions);
    { /* reverse list solution */
      var object solution = reverse(Car(solutions));
      /* 2. step: insert substitution in pattern2. */
     #ifdef LOGICAL_PATHNAMES
      /* convert capital-/small letters suitably: */
      if (!logical) {
        if (logical2)
          solution = subst_logical_case(solution);
      } else {
        if (!logical2)
          solution = subst_customary_case(solution);
      }
     #endif
      pushSTACK(solution);
      STACK_0 = translate_pathname(&STACK_0,STACK_(2+1+2));
    }
    /* 3. step: (MERGE-PATHNAMES modified_pattern2 sample :WILD T) */
    if (!nullp(STACK_(0+1+2)) /* query :MERGE-Argument */
        && has_some_wildcards(STACK_0)) {/*MERGE-PATHNAMES may be unnecessary*/
      pushSTACK(STACK_(4+1+2)); pushSTACK(unbound);
      pushSTACK(S(Kwild)); pushSTACK(T);
      funcall(L(merge_pathnames),5);
      pushSTACK(value1);
    }
    /* step 4: merge in default pathname */
   #if defined(PATHNAME_UNIX) || defined(PATHNAME_WIN32)
    if (absolute_p) {
      STACK_0 = use_default_dir(STACK_0); /* insert default-directory */
      /* (because Unix does not know the default-directory of LISP
         and Win32 is multitasking) */
    }
   #endif
    { /* (PUSH pathname pathnames) */
      var object new_cons = allocate_cons();
      Car(new_cons) = popSTACK(); Cdr(new_cons) = STACK_0;
      STACK_0 = new_cons;
    }
  }
  /* 4. step: (DELETE-DUPLICATES pathnames :TEST #'EQUAL) */
  pushSTACK(S(Ktest)); pushSTACK(L(equal));
  funcall(L(delete_duplicates),3);
  /* stack layout: ..., nil. */
  if (missingp(STACK_(1+1))) { /* query :ALL-Argument */
    if (mconsp(Cdr(value1))) {
      pushSTACK(value1);
      pushSTACK(STACK_(2+2));
      pushSTACK(STACK_(3+3));
      pushSTACK(STACK_(4+4));
      pushSTACK(S(translate_pathname));
      fehler(error,GETTEXT("(~S ~S ~S ~S) is ambiguous: ~S"));
    }
    value1 = Car(value1);
  }
  mv_count=1;
  skipSTACK(5+1);
}

/* (ABSOLUTE-PATHNAME pathname) converts pathname to a physical pathname, if
   necessary, and makes it absolute (using clisp's notion of default
   directory). */
LISPFUNN(absolute_pathname,1)
{
  var object thing = popSTACK();
  var object pathname = coerce_pathname(thing);
  pathname = use_default_dir(pathname); /* insert default-directory */
  VALUES1(pathname);
}

/* Converts an object into an absolute physical pathname and returns its
   namestring.
 physical_namestring(thing)
 > thing: an object
 < result: the namestring of the pathname denoted by thing
 can trigger GC */
global maygc object physical_namestring (object thing) {
  var object pathname = coerce_pathname(thing);
  pathname = use_default_dir(pathname); /* insert default-directory */
  return whole_namestring(pathname);
}

/* UP: tests, if the name of a pathname is =NIL.
 namenullp(pathname)
 > pathname: non-logical pathname
   local bool namenullp (object pathname);
   local bool namenullp(pathname)
     { return nullp(ThePathname(pathname)->pathname_name); } */
#define namenullp(path)  (nullp(ThePathname(path)->pathname_name))

/* error, if directory does not exist
 > obj: pathname or (better) erroneous component */
nonreturning_function(local, fehler_dir_not_exists, (object obj)) {
  pushSTACK(obj); /* FILE-ERROR slot PATHNAME */
  pushSTACK(obj);
  fehler(file_error,GETTEXT("nonexistent directory: ~S"));
}

/* error, if a file already exits
 > STACK_0: pathname */
nonreturning_function(local, fehler_file_exists, (void)) {
  /* STACK_0 = FILE-ERROR slot PATHNAME */
  pushSTACK(STACK_0); /* pathname */
  pushSTACK(TheSubr(subr_self)->name);
  fehler(file_error,GETTEXT("~S: file ~S already exists"));
}

#ifdef LOGICAL_PATHNAMES
/* An "absolute pathname" is always a non-logical pathname, poss.
 with further restrictions. */
#endif

#ifdef PATHNAME_WIN32

/* An "absolute pathname" is a pathname, whose device is a checked
 String and directory does not contain :RELATIVE, :CURRENT, :PARENT. */

/* UP: returns a namestring of a pathname for the operating system.
 OSnamestring(dir_namestring)
 > STACK_0: non-logical pathname
 > dir_namestring: directory-namestring (for DOS)
 < result: namestring (for DOS)
 can trigger GC */
local maygc object OSnamestring (object dir_namestring) {
  var uintC stringcount;
  pushSTACK(dir_namestring); /* Directory-Namestring as the first String */
  stringcount = file_namestring_parts(STACK_(0+1)); /* filename Strings */
  return string_concat(1+stringcount); /* concatenate */
}

/* UP: tests, if a drive exists.
 > uintB drive: drive-(capital-)letter
 < bool result: if this drive exists and is responsive */
local bool good_drive (uintB drive);
#ifdef WIN32_NATIVE
local bool good_drive (uintB drive) {
  var char rootpath[4];
  var DWORD result;
  rootpath[0] = drive;
  rootpath[1] = ':';
  rootpath[2] = '\\';
  rootpath[3] = '\0';
  begin_system_call();
  result = GetDriveType(rootpath);
  switch (result) {
    case DRIVE_UNKNOWN:
      end_system_call();
      return false;
    case DRIVE_NO_ROOT_DIR:
      /* Distinguish NFS mounts from nonassigned drive letters: */
      result = GetFileAttributes(rootpath);
      end_system_call();
      return !(result==0xFFFFFFFF);
    default:
      end_system_call();
      return true;
  }
}
#if 0
/* The following fails to recognize some (but not all) NFS mounts on WinNT. */
local bool good_drive_notsogood (uintB drive) {
  var DWORD drives_bitmask;
  begin_system_call();
  drives_bitmask = GetLogicalDrives();
  end_system_call();
  return ((drives_bitmask & ((DWORD)1 << (drive-'A'))) != 0);
}
#endif
#endif /* WIN32_NATIVE */

/* UP: returns the current drive.
 < char drive: drive-(capital-)letter */
local char default_drive (void) {
#ifdef WIN32_NATIVE
  var DWORD path_buflen = _MAX_PATH;
  var char* path_buffer = (char*)alloca(path_buflen);
  var DWORD result;
  begin_system_call();
  result = GetCurrentDirectory(path_buflen,path_buffer);
  if (!result) { OS_error(); }
  if (result >= path_buflen) {
    path_buflen = result; path_buffer = (char*)alloca(path_buflen);
    result = GetCurrentDirectory(path_buflen,path_buffer);
    if (!result) { OS_error(); }
  }
  end_system_call();
  if (path_buffer[1]==':') { /* local device */
    ASSERT(path_buffer[2]=='\\');
    return as_cint(up_case(as_chart(path_buffer[0])));
  } else if (path_buffer[0]=='\\') { /* network host */
    ASSERT(path_buffer[1]=='\\');
    return 0;
  } else NOTREACHED;
#endif
}

/* UP: returns the current directory on the given drive.
 > uintB drive: drive-(capital-)letter
 > object pathname: pathname (for error-reporting purposes)
 < result: current directory (as pathname)
 can trigger GC */
local maygc object default_directory_of (uintB drive, object pathname) {
/* working directory (of DOS) is the current directory: */
 #if defined(WIN32_NATIVE)
  var char currpath[4];
  var DWORD path_buflen = _MAX_PATH;
  var char* path_buffer = (char*)alloca(path_buflen+1);
  var char* dummy;
  var DWORD result;
  if (drive) {                  /* local disk */
    currpath[0] = drive;
    currpath[1] = ':';
    currpath[2] = '.'; /* this dot is actually not needed */
    currpath[3] = '\0';
    begin_system_call();
    result = GetFullPathName(currpath,path_buflen,path_buffer,&dummy);
    if (!result) { end_system_call(); OS_file_error(pathname); }
    if (result >= path_buflen) {
      path_buflen = result; path_buffer = (char*)alloca(path_buflen+1);
      result = GetFullPathName(currpath,path_buflen,path_buffer,&dummy);
      if (!result) { end_system_call(); OS_file_error(pathname); }
    }
    end_system_call();
  } else {                      /* network path */
    begin_system_call();
    result = GetCurrentDirectory(path_buflen,path_buffer);
    if (!result) { end_system_call(); OS_file_error(pathname); }
    if (result >= path_buflen) {
      path_buflen = result; path_buffer = (char*)alloca(path_buflen);
      result = GetCurrentDirectory(path_buflen,path_buffer);
      if (!result) { OS_file_error(pathname); }
    }
    end_system_call();
  }
  { /* poss. add a '\' at the end: */
    var char* path_end = &path_buffer[asciz_length(path_buffer)];
    if (!(path_end[-1]=='\\')) { path_end[0] = '\\'; path_end[1] = '\0'; }
  }
 #else
  var char path_buffer[3+MAXPATHLEN]; /* cf. GETWD(3) */
  path_buffer[0] = drive; path_buffer[1] = ':';
  /* file working directory in path_buffer: */
  begin_system_call();
  getwd_of(&path_buffer[2],drive);
  end_system_call();
 #endif
  /* Hack by DJ (see GO32/EXPHDLR.C) and EM (see LIB/MISC/_GETCWD1.C):
   converts all '\' to '/' and all captial- to small letters (only cosmetics,
   because DOS and our PARSE-NAMESTRING also understand filenames with '/'
   instead of '\').
   convert to pathname: */
  return asciz_dir_to_pathname(&path_buffer[0],O(pathname_encoding));
}

/* UP: Fills default-drive and default-directory into a pathname.
 use_default_dir(pathname)
 > pathname: non-logical pathname with Device /= :WILD
 < result: new absolute pathname
 can trigger GC */
local maygc object use_default_dir (object pathname) {
  /* first copy the pathname: */
  pathname = copy_pathname(pathname);
  pushSTACK(pathname);
  /* stack layout: pathname.
   default for the device: */
 #if HAS_HOST /* PATHNAME_WIN32 */
  if (nullp(ThePathname(pathname)->pathname_host))
 #endif
    if (nullp(ThePathname(pathname)->pathname_device)) {
      /* no device specified? --- take the default-drive instead: */
      ThePathname(pathname)->pathname_device = O(default_drive);
    }
  { /* Default for the directory: */
    var object subdirs = ThePathname(pathname)->pathname_directory;
    /* Does pathname-directory start with :RELATIVE ? */
    if (nullp(subdirs) || eq(Car(subdirs),S(Krelative))) {
      /* yes -> replace :RELATIVE with the default-directory: */
      pushSTACK(consp(subdirs) ? (object)Cdr(subdirs) : NIL);
     #if HAS_HOST /* PATHNAME_WIN32 */
      if (!nullp(ThePathname(pathname)->pathname_host)) {
        /* We do not have the concept of a current directory on a
         remote host. Simply use :ABSOLUTE instead of :RELATIVE. */
        subdirs = allocate_cons();
        Car(subdirs) = S(Kabsolute);
        Cdr(subdirs) = popSTACK();
      } else
     #endif
      { /* drive does not have to be present if we start on a network path */
        var object drive = ThePathname(pathname)->pathname_device;
        if (eq(drive,S(Kwild))) check_no_wildcards(pathname); /* error */
        var uintB dr = nullp(drive) ? 0 : as_cint(TheSnstring(drive)->data[0]);
        var object default_dir = default_directory_of(dr,pathname);
       #if HAS_HOST /* PATHNAME_WIN32 */
        ThePathname(STACK_1)->pathname_host = /* replace NIL in pathname ... */
          ThePathname(default_dir)->pathname_host; /* ... with default */
       #endif
        /* default_dir (a Pathname) is finished.
         Replace :RELATIVE with default-subdirs, i.e.
         form  (append default-subdirs (cdr subdirs))
              = (nreconc (reverse default-subdirs) (cdr subdirs)) */
        var object temp = ThePathname(default_dir)->pathname_directory;
        temp = reverse(temp);
        subdirs = nreconc(temp,popSTACK());
      }
    }
    /* traverse list and freshly cons up, thereby process '.\' and '..\'
     and '...\'  (do not leave it to DOS): */
    pushSTACK(subdirs);
    pushSTACK(NIL);
    /* stack layout: pathname, subdir-oldlist, subdir-newlist. */
    while (mconsp(STACK_1)) { /* until oldlist is finished: */
      var object subdir = Car(STACK_1); /* next subdir */
      if (equal(subdir,O(dot_string))) {
        /* = :CURRENT -> leave newlist unchanged */
      } else if (equal(subdir,O(dotdot_string))) {
        /* = :PARENT -> shorten newlist by one: */
        if (matomp(Cdr(STACK_0))) { /* newlist (except for :ABSOLUTE) empty ? */
          /* :PARENT from "\" returns Error */
          pushSTACK(STACK_2); /* FILE-ERROR slot PATHNAME */
          pushSTACK(O(backslash_string)); /* "\\" */
          pushSTACK(directory_namestring(STACK_(2+2))); /* directory of pathname */
          fehler(file_error,GETTEXT("no directory ~S above ~S"));
        }
        if (eq(Car(STACK_0),S(Kwild_inferiors))) { /* newlist starts with '...\' ? */
          /* :PARENT from "...\" returns Error */
          pushSTACK(STACK_2); /* FILE-ERROR slot PATHNAME */
          pushSTACK(directory_namestring(STACK_(2+1))); /* directory of pathname */
          fehler(file_error, /* '"..\\" after "...\\" is inadmissible: ~' */
                 GETTEXT("\"..\\\\\" after \"...\\\\\" is invalid: ~S"));
        }
        STACK_0 = Cdr(STACK_0);
      } else { /* (also if :ABSOLUTE !) */
        /* lengthen newlist by one: */
        pushSTACK(subdir);
        var object new_cons = allocate_cons();
        Car(new_cons) = popSTACK();
        Cdr(new_cons) = STACK_0;
        STACK_0 = new_cons;
      }
      STACK_1 = Cdr(STACK_1);
    }
    subdirs = nreverse(popSTACK()); /* newlist, reverse again */
    skipSTACK(1);
    /* stack layout: pathname. */
    ThePathname(STACK_0)->pathname_directory =
      simplify_directory(subdirs); /* enter into the pathname */
    pathname = popSTACK();
  }
  return pathname;
}

#ifdef WIN32_NATIVE

/* UP: translates short name to full name
 > shortname: old DOS 8.3 pathname
     wildcards aren't allowed. "." and ".." can be used.
 < fullname: buffer should be not less than MAX_PATH
 < result: true on success */
static BOOL FullName (LPCSTR shortname, LPSTR fullname) {
  var char current[_MAX_PATH];
  var char * rent = current;/* current+end-device-pos, rest after X: */
  var int state = 1;
  /* states for automata reading 'rent' pathname backward:
     0 - end
     1 - beginning
     2 - name component
     3 - slash component
     9,11,13... slash component after dots ("..").
       components to be skipped = (state - 9)/2
     10,12,14... name components after dots.
       components to be skipped = (state - 10)/2; */
  var enum {fn_eof, fn_name, fn_dots, fn_dot, fn_slash} symbol;
  /* symbol at the end of 'rent':
     1 - generic name
     2 - ".."
     3 - "."
     4 - slash
     0 - EOF i.e. beginning of 'rent' */
  var int pos;
  var int ops = 0;/* output position */
  strcpy(current,shortname);
  /* determine the end of device part */
  if (((current[0] >= 'a' && current[0] <= 'z')
    || (current[0] >= 'A' && current[0] <= 'Z'))
    && current[1] == ':') {
    rent = current+2;
  } else if (current[0]=='\\' && current[1]=='\\') {
    int i;rent = current;
    /* host */
    rent+=2;
    for (i=0;i<2;i++) {/* skip host and sharename */
      while (*rent && !cpslashp(*rent))
        rent++;
      if (*rent) rent++; else
        return FALSE;/*host and sharename don't end with slash*/
    }
  }
  pos = strlen(rent);
  do {
    rent[pos] = '\0';
    if (pos == 0) symbol = fn_eof; else
    if (cpslashp(rent[pos-1])) { pos--; symbol = fn_slash; } else
    { int dotcount = 0;/* < 0 -> not only dots */
      int wild = 0;
      while(pos > 0 && !cpslashp(rent[pos-1])) {
        if (rent[pos-1] == '.') dotcount++; else dotcount = -pos;
        if (rent[pos-1] == '*' || rent[pos-1] == '?') wild = 1;
        pos--;
      }
      if (wild) return FALSE;
      if (dotcount <= 0)  symbol = fn_name; else
      if (dotcount == 1)  symbol = fn_dot; else
      if (dotcount == 2)  symbol = fn_dots; else
        return FALSE; /* too many dots */
    }
    if (state == 1  /* beginning */
      || state == 2 /* name component */) {
      switch(symbol) {
      case fn_dot:  state = 3; break;  /* slash */
      case fn_dots: state = 11; break; /* dots-slash */
      case fn_name: {
        var WIN32_FIND_DATA wfd;
        var HANDLE h = NULL;
        h = FindFirstFile(current,&wfd);
        if (h != INVALID_HANDLE_VALUE) {
          strrev(wfd.cFileName);
          if (ops > 0 || wfd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
            fullname[ops++] = '\\';
          strcpy(fullname+ops,wfd.cFileName);
          ops+=strlen(wfd.cFileName);
          FindClose(h);
        } else return FALSE; /* file not found */
        state = 3;
      } break;
      case fn_slash:
        if (state == 1) state = 2;
        else return FALSE; /* two slashes in a row */
        break;
      case fn_eof:
        if (state == 1 && current == rent) return FALSE; /* D: */
        else state = 0;
        break;
      default:
        return FALSE;/* program error */
      }
    } else if (state == 3) {/* slash */
      switch(symbol) {
      case fn_slash: state = 2;break;
      case fn_eof:
        if (current == rent) state = 0; else return FALSE; /*D:FOO*/
        break;
      default: return FALSE; /* program error */
      }
    } else if (state % 2 == 1) {/* dots - slash 9, 11, 13 ... */
      switch(symbol) {
      case fn_slash:
        state += 1;
        if (state == 10) state = 2; /* zero depth */
        break; /* same depth */
      case fn_eof:
        return FALSE; /* too many ".." */
        break;
      default: return FALSE; /* program error */
      }
    } else {/* dots - name 10, 12, 14, ... */
      switch(symbol) {
      case fn_dot: state -= 1; break; /* same depth */
      case fn_dots: state += 1; break; /* increase depth */
      case fn_name: state -= 3; /* decrease depth */
      if (state < 9) return FALSE; /* program error */
      break;
      case fn_slash: return FALSE; /* two slashes */
      case fn_eof: return FALSE; /* too many ".."s */
      }
    }
  } while (state != 0);
  if (rent > current) fullname[ops++] = '\\';
  /* add device */
  while(rent > current)
    fullname[ops++] = (rent--)[-1];
  fullname[ops] = '\0';
  strrev(fullname);
  return TRUE;
}

#endif

/* UP: guarantees that the Directory of the Pathname exists
 (signals an error if it does not)
 assure_dir_exists(links_resolved,tolerantp)
 > STACK_0: absolute pathname without wildcards in directory
 > links_resolved: Flag, whether all links in the directory
                   of the pathname are already resolved
 > tolerantp: Flag, whether an error should be avoided
 < returns:
     if Name=NIL: Directory-Namestring (for DOS)
     if Name/=NIL: Namestring (for DOS)
     if tolerantp, maybe: nullobj
 can trigger GC */
#ifdef WIN32_NATIVE
local maygc object assure_dir_exists (bool links_resolved, bool tolerantp) {
  var bool nnullp = namenullp(STACK_0);
  if (nnullp && links_resolved) return directory_namestring(STACK_0);
  with_sstring_0(whole_namestring(STACK_0),O(pathname_encoding),path, {
    var char resolved[MAX_PATH];
    var bool substitute = false;
    var bool error = false;
    begin_system_call();
    if (links_resolved) { /* use light function */
      shell_shortcut_target_t rresolve = resolve_shell_symlink(path,resolved);
      if (rresolve != shell_shortcut_notresolved) {
        if (rresolve == shell_shortcut_notexists)
          error = true;
        else
          substitute = true;
      }
    } else {
      if (real_path(path,resolved))
        substitute = true;
      else { /* A file doesn't exist. Maybe dir does ? */
        error = true; /* let's be pessimistic */
        if (!nnullp) {
          var uintL lastslashpos = strlen(path) - 1;
          while (lastslashpos > 0 && path[lastslashpos]!=slash) lastslashpos--;
          if (path[lastslashpos]==slash) {
            path[lastslashpos + 1] = '\0'; /* leave only path without name */
            if (real_path(path,resolved)) {
              /* substitute only directory part */
              var DWORD fileattr = GetFileAttributes(resolved);
              /* resolved to a file ? Only directories allowed
                 - nonmaskable error */
              if (fileattr == 0xFFFFFFFF
                  || !(fileattr & FILE_ATTRIBUTE_DIRECTORY)) {
                SetLastError(ERROR_DIRECTORY);
                end_system_call(); OS_file_error(STACK_0);
              }
              pushSTACK(asciz_to_string(resolved,O(pathname_encoding)));
              /* substitute immediately - w/o substitute flag
               turn it into a pathname and use it with old name: */
              pushSTACK(coerce_pathname(STACK_0));
              /* save old pathname name and type components */
              pushSTACK(ThePathname(STACK_2)->pathname_name);
              pushSTACK(ThePathname(STACK_3)->pathname_type);
              STACK_4 = STACK_2;
              ThePathname(STACK_4)->pathname_name = STACK_1;
              ThePathname(STACK_4)->pathname_type = STACK_0;
              skipSTACK(4);
              error = false;
            }
          }
        }
      }
    }
    end_system_call();
    if (error) {
      if (tolerantp) return nullobj;
      pushSTACK(copy_pathname(STACK_0));
      ThePathname(STACK_0)->pathname_name = NIL;
      ThePathname(STACK_0)->pathname_type = NIL;
      fehler_dir_not_exists(popSTACK());
    }
    if (substitute) {
      var object resolved_string =
        asciz_to_string(resolved,O(pathname_encoding));
      STACK_0 = coerce_pathname(resolved_string);
      nnullp = namenullp(STACK_0);
    }
    /* merge in *DEFAULT-PATHNAME-DEFAULTS* & :VERSION :NEWEST:
       for cross-platform consistency, either all or no versions of
       assure_dir_exists() must call MERGE-PATHNAMES  */
    funcall(L(merge_pathnames),1); pushSTACK(value1);
    { var object dns = directory_namestring(STACK_0);
      return nnullp ? dns : OSnamestring(dns); }
  });
}
#endif

/* UP: returns the directory-namestring of a pathname under the assumption,
     that the directory of this pathname exists.
 assume_dir_exists()
 > STACK_0: absolute pathname without wildcards in the directory
 < result:
     if Name=NIL: directory-namestring (for DOS)
     if Name/=NIL: namestring (for DOS)
 can trigger GC */
global maygc object assume_dir_exists (void) {
  return assure_dir_exists(true,false);
}

#endif

#ifdef PATHNAME_UNIX

/* UP: Return the current Directory.
 < result: current Directory (as Pathname)
 can trigger GC */
local maygc object default_directory (void) {
  var char path_buffer[MAXPATHLEN]; /* cf. GETWD(3) */
  /* store Working Directory in path_buffer: */
  begin_system_call();
  if ( getwd(&path_buffer[0]) ==NULL) {
    end_system_call();
    pushSTACK(O(dot_string)); /* FILE-ERROR slot PATHNAME */
    pushSTACK(asciz_to_string(&path_buffer[0],O(pathname_encoding))); /* message */
    fehler(file_error,GETTEXT("UNIX error while GETWD: ~S"));
  }
  end_system_call();
  /* It must start with '/' : */
  if (!(path_buffer[0] == '/')) {
    pushSTACK(O(dot_string)); /* FILE-ERROR slot PATHNAME */
    pushSTACK(asciz_to_string(&path_buffer[0],O(pathname_encoding)));
    fehler(file_error,GETTEXT("UNIX GETWD returned ~S"));
  }
  /* convert to pathname: */
  return asciz_dir_to_pathname(&path_buffer[0],O(pathname_encoding));
}

/* UP: Fills Default-Directory into a pathname.
 use_default_dir(pathname)
 > pathname: non-logical pathname
 < result: new pathname, whose directory contains no :RELATIVE .
             (short: "absolute pathname")
 can trigger GC */
local maygc object use_default_dir (object pathname) {
  /* copy the pathname first: */
  pathname = copy_pathname(pathname);
  { /* then build the default-directory into the pathname: */
    var object subdirs = ThePathname(pathname)->pathname_directory;
    /* does pathname-directory start with :RELATIVE? */
    if (nullp(subdirs) || eq(Car(subdirs),S(Krelative))) {
      /* yes -> replace :RELATIVE with default-subdirs, i.e.
       form  (append default-subdirs (cdr subdirs))
            = (nreconc (reverse default-subdirs) (cdr subdirs)) */
      pushSTACK(pathname);
      pushSTACK(consp(subdirs) ? (object)Cdr(subdirs) : NIL);
      var object temp = default_directory();
      temp = ThePathname(temp)->pathname_directory;
      temp = reverse(temp);
      subdirs = nreconc(temp,popSTACK());
      subdirs = simplify_directory(subdirs);
      pathname = popSTACK();
      /* enter into the pathname: */
      ThePathname(pathname)->pathname_directory = subdirs;
    }
  }
  return pathname;
}

/* UP: Assures, that the directory of a pathname exists, and thereby resolves
 symbolic links.
 assure_dir_exists(tolerantp)
 > STACK_0: non-logical pathname, whose directory does not contain :RELATIVE.
 > links_resolved: Flag, if all links in the directory of the pathname
     are already resolved and if it is known to exist
 > tolerantp: flag, if an error is to be avoided
 < STACK_0: (poss. the same) pathname, whereas neither for the directory nor
            for the Filename a symbolic link is to be tracked.
 < result:
     if Name=NIL: directory-namestring (for UNIX, with '/' at the end)
     if Name/=NIL: namestring (for UNIX)
     if tolerantp poss.: nullobj
 < filestatus: if Name/=NIL: NULL if the file does not exist,
                                  else a pointer to a STAT-information.
 can trigger GC */

/* this has to be done this ugly way since C does not allow conditionals
 (like #ifdef HAVE_LSTAT) inside macros (like with_sstring_0) */
#ifdef HAVE_LSTAT
  #define if_HAVE_LSTAT(statement)  statement
#else
  #define if_HAVE_LSTAT(statement)
#endif

local var struct stat * filestatus;
local maygc object assure_dir_exists (bool links_resolved, bool tolerantp) {
  var uintC allowed_links = MAXSYMLINKS; /* number of allowed symbolic links */
  if (links_resolved)
    goto dir_exists;
  loop { /* loop over the symbolic links to be resolved */
    { /* determine Truepath of the directory: */
      var char path_buffer[MAXPATHLEN]; /* cf. REALPATH(3) */
      {
        var object pathname = STACK_0;
        var uintC stringcount = /* host and directory strings */
          directory_namestring_parts(pathname);
        pushSTACK(O(dot_string)); /* and "." */
        var object string = string_concat(stringcount+1); /* concatenate */
        /* resolve symbolic links therein: */
        with_sstring_0(string,O(pathname_encoding),string_asciz, {
          begin_system_call();
          if ( realpath(string_asciz,&path_buffer[0]) ==NULL) {
            if (errno!=ENOENT) { end_system_call(); OS_file_error(STACK_0); }
            end_system_call();
            if (!tolerantp)
              fehler_dir_not_exists(asciz_dir_to_pathname(&path_buffer[0],O(pathname_encoding))); /* erroneous component */
            end_system_call();
            FREE_DYNAMIC_ARRAY(string_asciz);
            return nullobj;
          }
          end_system_call();
        });
      }
      /* new Directory-Path must start with '/' : */
      if (!(path_buffer[0] == '/')) {
        /* STACK_0 = FILE-ERROR slot PATHNAME */
        pushSTACK(asciz_to_string(&path_buffer[0],O(pathname_encoding)));
        fehler(file_error,GETTEXT("UNIX REALPATH returned ~S"));
      }
      /* possibly add a '/' at the end: */
      var char* pathptr = &path_buffer[0];
      var uintL len = 0; /* string-length */
      until (*pathptr == 0) { pathptr++; len++; } /* search ASCIZ-string-end */
      if (!((len>0) && (pathptr[-1]=='/'))) {
        *pathptr = '/'; len++; /* add a '/' */
      }
      /* and convert to a string: */
      var object new_string = n_char_to_string(&path_buffer[0],len,O(pathname_encoding));
      /* turn it into a pathname and use its Directory: */
      var object new_pathname = coerce_pathname(new_string);
      ThePathname(STACK_0)->pathname_directory
        = ThePathname(new_pathname)->pathname_directory;
    }
  dir_exists:
    /* get information for the addressed file: */
    if (namenullp(STACK_0)) /* no file addressed? */
      return directory_namestring(STACK_0); /* yes -> finished */
    var object namestring = whole_namestring(STACK_0); /* concatenate */
    /* get information: */
    var local struct stat status;
    with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
      begin_system_call();
      if (!( lstat(namestring_asciz,&status) ==0)) {
        if (!(errno==ENOENT))
          { end_system_call(); OS_file_error(STACK_0); }
        /* file does not exist. */
        end_system_call();
        FREE_DYNAMIC_ARRAY(namestring_asciz);
        filestatus = (struct stat *)NULL; return namestring;
      }
      end_system_call();
      /* file exists. */
      if (S_ISDIR(status.st_mode)) { /* is it a directory? */
        /* STACK_0 = FILE-ERROR slot PATHNAME */
        pushSTACK(whole_namestring(STACK_0));
        pushSTACK(TheSubr(subr_self)->name);
        fehler(file_error,GETTEXT("~S: ~S names a directory, not a file"));
      }
     if_HAVE_LSTAT(
      else if (possible_symlink(namestring_asciz) && S_ISLNK(status.st_mode)) {
        /* is it a symbolic link?
           yes -> continue resolving: */
        if (allowed_links==0) { /* no more links allowed? */
          /* yes -> simulate UNIX-Error ELOOP */
          begin_system_call();
          errno = ELOOP_VALUE;
          end_system_call();
          OS_file_error(STACK_0);
        }
        allowed_links--; /* after that, one link less is allowed */
        var uintL linklen = status.st_size; /* presumed length of the link-content */
       retry_readlink: {
          var DYNAMIC_ARRAY(linkbuf,char,linklen+1); /* buffer for the Link-content */
          /* read link-content: */
          begin_system_call();
          {
            var int result = readlink(namestring_asciz,linkbuf,linklen);
            end_system_call();
            if (result<0)
              OS_file_error(STACK_0);
            if (!(result == (int)linklen)) { /* sometimes (AIX, NFS) status.st_size is incorrect */
              FREE_DYNAMIC_ARRAY(linkbuf); linklen = result; goto retry_readlink;
            }
          }
          /* turn it into a pathname:
             (MERGE-PATHNAMES (PARSE-NAMESTRING linkbuf) pathname-without-name&type) */
          pushSTACK(n_char_to_string(linkbuf,linklen,O(pathname_encoding)));
          FREE_DYNAMIC_ARRAY(linkbuf);
        }
        funcall(L(parse_namestring),1);
        pushSTACK(value1);
        var object pathname = copy_pathname(STACK_(0+1));
        ThePathname(pathname)->pathname_name = NIL;
        ThePathname(pathname)->pathname_type = NIL;
        pushSTACK(pathname);
        funcall(L(merge_pathnames),2);
        STACK_0 = value1;
      }
     ) /* HAVE_LSTAT */
      else { /* normal file */
        filestatus = &status; return namestring;
      }
    });
  }
}

/* the same under the assumption, that the directory already exists.
   (only a little simplification, as the file can be a symbolic link into a
   different directory, and this must be tested to exist.) */
global maygc object assume_dir_exists (void) {
  var object ret;
  with_saved_back_trace_subr(L(open),STACK STACKop -7,-1,
    ret = assure_dir_exists(true,false); );
  return ret;
}

#endif

#ifdef PATHNAME_WIN32
#if 0 /* unused */
/* UP: Turns a directory-namestring into one, that is suitably for DOS.
 OSdirnamestring(namestring)
 > namestring: newly created directory-namestring, with '\' at the end,
               a normal-simple-string
 < result: namestring for this directory, in DOS-Format: last '\'
             discarded, if superfluous, a normal-simple-string
 can trigger GC */
local maygc object OSdirnamestring (object namestring) {
  var uintL len = Sstring_length(namestring);
  if (len==0) goto ok; /* empty string -> do not discard anything */
  var chart ch = TheSnstring(namestring)->data[len-1];
  if (!chareq(ch,ascii('\\'))) /* no '\' at the end -> do not discard */
    goto ok;
  if (len==1) goto ok; /* "\" means Root -> do not discard */
  ch = TheSnstring(namestring)->data[len-2];
  if (chareq(ch,ascii('\\')) || colonp(ch)) /* '\' or ':' before it */
    goto ok; /* -> means parent -> do not discard */
  /* discard '\' at the end: */
  namestring = subsstring(namestring,0,len-1);
 ok: /* do not discard anything */
  return namestring;
}
#endif
/* UP: Changes the default-drive and its default-directory.
 change_default();
 > STACK_0: absolute pathname, whose device is a string and directory
     contains no :RELATIVE, :CURRENT, :PARENT, and name and type are =NIL.
 can trigger GC */
local maygc void change_default (void) {
  { /* change default-directory for this drive: */
    var object pathname = STACK_0;
    var uintC stringcount = directory_namestring_parts(pathname);
    /* no redundant '\' at the end */
    if (mconsp(Cdr(ThePathname(pathname)->pathname_directory))) {
      skipSTACK(1); stringcount--;
    }
    var object string = string_concat(stringcount); /* concatenate */
    with_sstring_0(string,O(pathname_encoding),asciz, {
      /* change default-directory: */
      change_current_directory(asciz);
    });
  }
  /* change default-drive: */
  O(default_drive) = ThePathname(STACK_0)->pathname_device;
  /* set *DEFAULT-PATHNAME-DEFAULTS* : */
  recalc_defaults_pathname();
}
#endif
#ifdef PATHNAME_UNIX
/* UP: changes the default-directory.
 change_default();
 > STACK_0: absolute pathname, whose directory contains no :RELATIVE,
      :CURRENT, :PARENT , and name and Type are =NIL.
 can trigger GC */
local maygc void change_default (void) {
  var object string = directory_namestring(STACK_0);
  with_sstring_0(string,O(pathname_encoding),asciz, {
    /* change default-directory: */
    begin_system_call();
    if (!( chdir(asciz) ==0)) { end_system_call(); OS_file_error(STACK_0); }
    end_system_call();
  });
}
#endif

LISPFUNNR(namestring,1) { /* (NAMESTRING pathname), CLTL p. 417 */
  var object pathname = coerce_xpathname(popSTACK());
  VALUES1(whole_namestring(pathname));
}

/* error-message because of missing file name
 fehler_noname(pathname);
 > pathname: pathname */
nonreturning_function(local, fehler_noname, (object pathname)) {
  pushSTACK(pathname); /* FILE-ERROR slot PATHNAME */
  pushSTACK(pathname);
  fehler(file_error,GETTEXT("no file name given: ~S"));
}
#define check_noname(pathname)                                          \
  do { if (namenullp(pathname)) { fehler_noname(pathname); } } while(0)

/* error-message because of illegal Name/Type-specification
 fehler_notdir(pathname);
 > pathname: pathname */
nonreturning_function(local, fehler_notdir, (object pathname)) {
  pushSTACK(pathname); /* FILE-ERROR slot PATHNAME */
  pushSTACK(pathname);
  fehler(file_error,GETTEXT("not a directory: ~S"));
}
#define check_notdir(pathname)                                  \
  do { if (!(nullp(ThePathname(pathname)->pathname_name)        \
             && nullp(ThePathname(pathname)->pathname_type)))   \
         fehler_notdir(pathname); } while(0)

/* test, if a file exists:
 file_exists(namestring)
 > led the way: assure_dir_exists()
 > STACK_0: pathname, the same as after execution of assure_dir_exists(), Name/=NIL
 > namestring: its namestring for the operating system */
#ifdef WIN32_NATIVE
  local inline int access0 (const char* path) {
    begin_system_call();
    var DWORD fileattr = GetFileAttributes(path);
    if (fileattr == 0xFFFFFFFF) {
      if (WIN32_ERROR_NOT_FOUND) {
        end_system_call(); return -1;
      }
      end_system_call(); OS_file_error(STACK_0);
    }
    end_system_call();
    return 0;
  }
  local bool file_exists (object namestring) {
    var bool exists;
    with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
      exists = (access0(namestring_asciz)==0);
    });
    return exists;
  }
#endif
#ifdef UNIX
  #define file_exists(namestring)  (!(filestatus == (struct stat *)NULL))
  #define FILE_EXISTS_TRIVIAL
#endif

/* error-message because of non-existent file
 fehler_file_not_exists();
 > STACK_0: pathname */
nonreturning_function(local, fehler_file_not_exists, (void)) {
  /* STACK_0 = FILE-ERROR slot PATHNAME */
  pushSTACK(STACK_0); /* pathname */
  pushSTACK(TheSubr(subr_self)->name);
  fehler(file_error,GETTEXT("~S: file ~S does not exist"));
}

/* TRUENAME for a pathname
   pushes pathname on the stack and
   returns the truename (filename for the operating system) or nullobj
   can trigger GC */
local maygc object true_namestring (object pathname, bool noname_p,
                                    bool tolerantp) {
  check_no_wildcards(pathname); /* with wildcards -> error */
  pathname = use_default_dir(pathname); /* insert default-directory */
  if (noname_p) check_noname(pathname);
  pushSTACK(pathname); /* directory must exist: */
  return assure_dir_exists(false,tolerantp);
}

LISPFUNNR(truename,1)
{ /* (TRUENAME pathname), CLTL p. 413 */
  var object pathname = popSTACK(); /* pathname-argument */
  if (builtin_stream_p(pathname)) { /* stream -> treat extra: */
    /* must be file-stream: */
    pathname = as_file_stream(pathname);
    test_file_stream_named(pathname);
    /* Streamtype File-Stream */
    VALUES1(TheStream(pathname)->strm_file_truename);
  } else {
    var object namestring =
      true_namestring(merge_defaults(coerce_pathname(pathname)),false,false);
    if (namenullp(STACK_0)) {
      /* no name specified */
      if (!nullp(ThePathname(STACK_0)->pathname_type)) {
        /* STACK_0 = FILE-ERROR slot PATHNAME */
        pushSTACK(STACK_0); /* pathname */
        pushSTACK(TheSubr(subr_self)->name);
        fehler(file_error,GETTEXT("~S: pathname with type but without name makes no sense: ~S"));
      }
      /* no name and no type specified -> pathname as result */
    } else {
      /* name specified.
       check, if the file exists: */
      if (!file_exists(namestring)) { fehler_file_not_exists(); }
      /* file exists -> pathname as value */
    }
    VALUES1(popSTACK());
  }
}

LISPFUNNR(probe_file,1)
{ /* (PROBE-FILE filename), CLTL p. 424 */
  var object pathname = popSTACK(); /* pathname-argument */
  if (builtin_stream_p(pathname)) { /* stream -> treat extra: */
    /* must be file-stream: */
    pathname = as_file_stream(pathname);
    test_file_stream_named(pathname);
    /* streamtype file-stream -> take truename: */
    var uintB flags = TheStream(pathname)->strmflags;
    pathname = TheStream(pathname)->strm_file_truename;
    if (flags & strmflags_open_B) { /* file opened? */
      /* yes -> truename instantly as result: */
      VALUES1(pathname); return;
    }
    /* no -> yet to test, if the file for the truename exists. */
  } else /* turn into a pathname */
    pathname = merge_defaults(coerce_pathname(pathname));
  /* pathname is now a Pathname. */
  var object namestring = true_namestring(pathname,true,true);
  if (eq(namestring,nullobj)) {
    /* path to the file does not exist -> NIL as value: */
    skipSTACK(1); VALUES1(NIL); return;
  }
  /* check, if the file exists: */
  if (file_exists(namestring)) {
    VALUES1(popSTACK()); /* file exists -> pathname as value */
  } else {
    skipSTACK(1); VALUES1(NIL); return; /* else NIL as value */
  }
}

/* tests, if a directory exists.
 directory_exists(pathname)
 > pathname: an absolute pathname without wildcards, with Name=NIL and Type=NIL
 < result: true, if it denotes an existing directory
 can trigger GC */
local maygc bool directory_exists (object pathname) {
  pushSTACK(pathname); /* save pathname */
  var object dir_namestring = directory_namestring(pathname);
  /* existence test, see assure_dir_exists(): */
  var bool exists = true;
 #ifdef WIN32_NATIVE
  with_sstring_0(dir_namestring,O(pathname_encoding),dir_namestring_asciz, {
    if (!nullp(Cdr(ThePathname(STACK_0)->pathname_directory))) {
      var uintL len = Sstring_length(dir_namestring);
      ASSERT((len > 0) && cpslashp(dir_namestring_asciz[len-1]));
      dir_namestring_asciz[len-1] = '\0'; /* replace '\' at the end with nullbyte */
    }
    begin_system_call();
    var DWORD fileattr = GetFileAttributes(dir_namestring_asciz);
    if (fileattr == 0xFFFFFFFF) {
      if (!WIN32_ERROR_NOT_FOUND) {
        end_system_call(); OS_file_error(STACK_0);
      }
      exists = false;
    } else {
      if (!(fileattr & FILE_ATTRIBUTE_DIRECTORY)) /* found file is no subdirectory ? */
        exists = false;
    }
    end_system_call();
  });
 #endif
 #ifdef PATHNAME_UNIX
  pushSTACK(dir_namestring);
  pushSTACK(O(dot_string)); /* and "." */
  dir_namestring = string_concat(2); /* concatenate */
  with_sstring_0(dir_namestring,O(pathname_encoding),dir_namestring_asciz, {
    var struct stat statbuf;
    begin_system_call();
    if (stat(dir_namestring_asciz,&statbuf) < 0) {
      if (!(errno==ENOENT)) { end_system_call(); OS_file_error(STACK_0); }
      exists = false;
    } else {
      if (!S_ISDIR(statbuf.st_mode)) /* found file is no subdirectory ? */
        exists = false;
    }
    end_system_call();
  });
 #endif
  skipSTACK(1);
  return exists;
}

LISPFUNNR(probe_directory,1)
{ /* (PROBE-DIRECTORY filename) tests, if a directory exists. */
  var object pathname = popSTACK(); /* pathname-argument */
  pathname = merge_defaults(coerce_pathname(pathname)); /* --> pathname */
  check_no_wildcards(pathname); /* with wildcards -> error */
  pathname = use_default_dir(pathname); /* insert default-directory */
  check_notdir(pathname); /* ensure that Name=NIL and Type=NIL */
  VALUES_IF(directory_exists(pathname));
}

/* Converts a directory pathname to an OS directory specification.
 > pathname: an object
 > use_default: whether to use the current default directory
 < result: a simple-bit-vector containing an ASCIZ string in OS format
 can trigger GC */
global maygc object pathname_to_OSdir (object pathname, bool use_default) {
  pathname = coerce_pathname(pathname); /* convert to pathname */
  check_no_wildcards(pathname); /* if it has wildcards -> error */
  if (use_default)
    pathname = use_default_dir(pathname); /* insert default directory */
  check_notdir(pathname); /* ensure that Name=NIL and Type=NIL */
  pushSTACK(pathname); /* save pathname */
  var object dir_namestring = directory_namestring(pathname);
  var object dir_namestring_asciz =
    string_to_asciz(dir_namestring,O(pathname_encoding));
  var char* asciz = TheAsciz(dir_namestring_asciz);
  var uintL len = asciz_length(asciz);
  #if defined(WIN32_NATIVE) || defined(UNIX)
    if (!nullp(Cdr(ThePathname(STACK_0)->pathname_directory))) {
      ASSERT((len > 0) && cpslashp(asciz[len-1]));
      asciz[len-1] = '\0';
    }
  #endif
  skipSTACK(1); /* forget pathname */
  return dir_namestring_asciz;
}

/* Converts an OS directory specification to a directory pathname.
 > path: a pathname referring to a directory
 < result: a pathname without name and type
 can trigger GC */
global maygc object OSdir_to_pathname (const char* path) {
  return asciz_dir_to_pathname(path,O(pathname_encoding));
}

/* UP: determines, if a file is opened.
 openp(pathname) */
#ifdef PATHNAME_WIN32
/* > pathname: absolute pathname, without wildcards. */
#endif
#ifdef PATHNAME_UNIX
/* > pathname: absolute pathname, without wildcards, after resolution
             of symbolic links */
#endif
/* < result: true, if an opened file-stream exits for this file. */
local bool openp (object pathname) {
  var object flist = O(open_files); /* traverse list of all open files */
  while (consp(flist)) {
    var object f = Car(flist); /* next open stream */
    if (TheStream(f)->strmtype == strmtype_file) { /* file-stream ? */
      if (equal(TheStream(f)->strm_file_truename,pathname))
        return true;
    }
    flist = Cdr(flist);
  }
  return false;
}

/* error-message because of deletion attempt on opened file
 fehler_delete_open(pathname);
 > pathname: truename of the file */
nonreturning_function(local, fehler_delete_open, (object pathname)) {
  pushSTACK(pathname); /* FILE-ERROR slot PATHNAME */
  pushSTACK(pathname);
  fehler(file_error,GETTEXT("cannot delete file ~S since there is a file stream open to it"));
}
#define check_delete_open(pathname)                                     \
 do { if (openp(pathname)) { fehler_delete_open(pathname); } } while(0)

/* (DELETE-FILE filename), CLTL p. 424 */
LISPFUNN(delete_file,1) {
  var object pathname = popSTACK(); /* pathname-argument */
  if (builtin_stream_p(pathname)) { /* stream -> treat extra: */
    var object stream = as_file_stream(pathname); /* must be file-stream */
    test_file_stream_named(stream);
    /* Streamtype file-stream.
     if file is opened, close file first: */
    if (TheStream(stream)->strmflags & strmflags_open_B) { /* file opened ? */
      pushSTACK(stream); builtin_stream_close(&STACK_0,0); stream = popSTACK();
    }
    /* then take the truename as file to be deleted: */
    pathname = TheStream(stream)->strm_file_truename;
  } else /* turn into a pathname */
    pathname = merge_defaults(coerce_pathname(pathname));
  /* pathname is now a pathname. */
  check_no_wildcards(pathname); /* with wildcards -> error */
  pathname = use_default_dir(pathname); /* insert default-directory */
  check_noname(pathname);
  pushSTACK(pathname); pushSTACK(pathname);
  var object namestring = assure_dir_exists(false,true);
  if (eq(namestring,nullobj)) {
    /* path to the file does not exist ==> return NIL */
    skipSTACK(2); VALUES1(NIL); return;
  }
  check_delete_open(STACK_0);
  /* delete the original filename - not the truename */
  namestring = whole_namestring(STACK_1);
  with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
    if (!delete_file_if_exists(namestring_asciz)) {
      /* file does not exist -> value NIL */
      FREE_DYNAMIC_ARRAY(namestring_asciz); skipSTACK(2);
      VALUES1(NIL); return;
    }
  });
  /* file existed, was deleted -> pathname (/=NIL) as value */
  VALUES1(nullp(O(ansi)) ? (object)STACK_1 : T); skipSTACK(2);
}

/* error-message because of renaming attempt of an opened file
 fehler_rename_open(pathname);
 > pathname: truename of the file */
nonreturning_function(local, fehler_rename_open, (object pathname)) {
  pushSTACK(pathname); /* FILE-ERROR slot PATHNAME */
  pushSTACK(pathname);
  fehler(file_error,GETTEXT("cannot rename file ~S since there is a file stream open to it"));
}

/* UP: Renames a file.
 rename_file();
 > stack layout: filename, newname, oldpathname.
 < stack layout: filename, newname, oldpathname, newpathname,
                oldtruename, oldnamestring, newtruename, newnamestring. */
local void rename_file (void) {
  { /* 1. newpathname := (MERGE-PATHNAMES newname oldpathname) */
    pushSTACK(STACK_1); /* newname as 1. argument */
    pushSTACK(STACK_(0+1)); /* oldpathname as 2. argument */
    funcall(L(merge_pathnames),2);
    pushSTACK(value1);
  }
  /* stack layout: filename, newname, oldpathname, newpathname. */
  { /* 2. check oldpathname: */
    var object oldpathname = STACK_1;
    var object old_namestring = true_namestring(oldpathname,true,false);
    if (openp(STACK_0)) /* do not rename open files! */
      { fehler_rename_open(STACK_0); }
    if (!file_exists(old_namestring))
      fehler_file_not_exists();
    pushSTACK(old_namestring);
  }
  /* stack layout: filename, newname, oldpathname, newpathname,
                oldtruename, oldnamestring. */
  { /* 3. check newpathname: */
    var object newpathname = coerce_pathname(STACK_2);
    var object new_namestring = true_namestring(newpathname,true,false);
    /* stack layout: filename, newname, oldpathname, newpathname,
                  oldtruename, oldnamestring, newtruename.
     4. rename file: */
    if (file_exists(new_namestring)) {
      /* file already exists -> do not delete without forewarn */
      fehler_file_exists();
    }
    pushSTACK(new_namestring);
  }
  /* stack layout: filename, newname, oldpathname, newpathname,
                oldtruename, oldnamestring, newtruename, newnamestring.
   now it can be renamed without risk: */
  with_sstring_0(STACK_2,O(pathname_encoding),oldnamestring_asciz, {
    with_sstring_0(STACK_0,O(pathname_encoding),newnamestring_asciz, {
      rename_existing_file(oldnamestring_asciz,newnamestring_asciz);
    });
  });
}

/* (RENAME-FILE filename newname), CLTL p. 423 */
LISPFUNN(rename_file,2) {
  var object filename = STACK_1; /* filename-argument */
  if (builtin_stream_p(filename)) { /* stream -> treat extra: */
    /* must be file-stream: */
    filename = as_file_stream(filename);
    test_file_stream_named(filename);
    /* streamtype file-stream -> use truename: */
    filename = TheStream(filename)->strm_file_truename;
    pushSTACK(filename);
    /* rename: */
    rename_file();
    /* update stream: */
    filename = STACK_7;
    TheStream(filename)->strm_file_name = STACK_4; /* newpathname as new name */
    TheStream(filename)->strm_file_truename = STACK_1; /* newtruename as new truename */
    /* leave handle etc. untouched */
  } else { /* turn into a pathname */
    filename = merge_defaults(coerce_pathname(filename));
    pushSTACK(filename);
    /* rename: */
    rename_file();
  }
  VALUES3(STACK_4, /* newpathname as 1st value */
          STACK_3, /* oldtruename as 2nd value */
          STACK_1); /* newtruename as 3rd value */
  skipSTACK(8);
}

/* Create a file.
 create_new_file(pathstring);
 It is known that the file does not already exist.
 > pathstring: file name, ASCIZ-String
 > STACK_0: pathname */
local inline void create_new_file (char* pathstring) {
 #ifdef WIN32_NATIVE
  begin_system_call();
  var Handle handle = CreateFile(pathstring, 0, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
  if (handle==INVALID_HANDLE_VALUE)
    { end_system_call(); OS_file_error(STACK_0); }
  /* file was created, handle is the Handle.
   close file again: */
  if (!CloseHandle(handle)) { end_system_call(); OS_file_error(STACK_0); }
  end_system_call();
 #endif
 #ifdef UNIX
  begin_system_call();
  var int result = OPEN(pathstring, O_WRONLY | O_BINARY | O_CREAT | O_TRUNC, my_open_mask);
  if (result<0) { end_system_call(); OS_file_error(STACK_0); } /* report error */
  /* file was created, result is the Handle.
   close file again: */
  if (!(CLOSE(result)==0)) { end_system_call(); OS_file_error(STACK_0); } /* report error */
  end_system_call();
 #endif
}

/* Open a file for input.
 open_input_file(pathstring,create_if_not_exists,&handle)
 > led the way: assure_dir_exists()
 > pathstring: file name, ASCIZ-String
 > create_if_not_exists: if true, the file must be created
 > STACK_0: pathname
 < handle: open file handle
 < result: whether the file could be opened (necessarily true if create_if_not_exists) */
local inline bool open_input_file (char* pathstring, bool create_if_not_exists,
                                   Handle* handle_) {
 #ifdef UNIX
  var int result;
  #ifdef FILE_EXISTS_TRIVIAL
  var int oflags = O_RDONLY | O_BINARY;
  if (!file_exists(_EMA_)) {
    /* file does not exist */
    if (!create_if_not_exists) return false;
    /* create file with open: */
    oflags |= O_CREAT;
  }
  begin_system_call();
  result = OPEN(pathstring,oflags,my_open_mask);
  end_system_call();
  if (result<0) { OS_file_error(STACK_0); }
  #else
  var int oflags = O_RDONLY | O_BINARY;
  if (create_if_not_exists) { oflags |= O_CREAT; }
  begin_system_call();
  result = OPEN(pathstring,oflags,my_open_mask);
  if (result<0) {
    if (errno == ENOENT) { /* not found? */
      /* file does not exist */
      if (!create_if_not_exists) { end_system_call(); return false; }
    }
    end_system_call(); OS_file_error(STACK_0); /* report error */
  }
  end_system_call();
  #endif
  *handle_ = result; return true;
 #endif
 #ifdef WIN32_NATIVE
  var Handle handle;
  #ifdef FILE_EXISTS_TRIVIAL
  var DWORD flag = OPEN_EXISTING;
  if (!file_exists(_EMA_)) { /* file does not exist */
    if (!create_if_not_exists) return false;
    /* create file with CreateFile: */
    flag = OPEN_ALWAYS;
  }
  begin_system_call();
  handle = CreateFile(pathstring, GENERIC_READ,
                      FILE_SHARE_READ | FILE_SHARE_WRITE,
                      NULL, flag, FILE_ATTRIBUTE_NORMAL, NULL);
  end_system_call();
  if (handle==INVALID_HANDLE_VALUE) { OS_file_error(STACK_0); }
  #else
  var DWORD flag = OPEN_EXISTING;
  if (create_if_not_exists) { flag = OPEN_ALWAYS; }
  begin_system_call();
  handle = CreateFile(pathstring, GENERIC_READ,
                      FILE_SHARE_READ | FILE_SHARE_WRITE,
                      NULL, flag, FILE_ATTRIBUTE_NORMAL, NULL);
  if (handle==INVALID_HANDLE_VALUE) {
    if (WIN32_ERROR_NOT_FOUND) { /* not found? */
      /* file does not exist */
      if (!create_if_not_exists) { end_system_call(); return false; }
    }
    end_system_call(); OS_file_error(STACK_0); /* report Error */
  }
  end_system_call();
  #endif
  *handle_ = handle; return true;
 #endif
}

#if defined(UNIX) || defined(WIN32_NATIVE)
/* Open a file for output.
 open_output_file(pathstring,truncate_if_exists)
 > pathstring: file name, ASCIZ-String
 > truncate_if_exists: if true, the file is truncated to zero size
 > STACK_0: pathname
 < result: open file handle */
local inline Handle open_output_file (char* pathstring, bool wronly,
                                      bool truncate_if_exists) {
 #ifdef UNIX
  begin_system_call();
  var int flags = O_BINARY | O_CREAT | (truncate_if_exists ? O_TRUNC : 0);
  /* regular file or !wronly => O_RDWR
   i.e., for the handle to be O_WRONLY, it must be opened :DIRECTION :OUTPUT
   AND the underlying file must be special (pipe &c)
   see bug #[ 1379620 ]: open FIFOs with write-only access for IPC
   see Stevens, UNIX Network Programming, vol 2 (IPC), ch 4 (pipes & FIFOs)*/
  if (wronly) { /* regular (regular_handle_p) => ignore wronly for buffering */
    var struct stat statbuf;
    if (stat(pathstring,&statbuf) ||
        S_ISREG(statbuf.st_mode) || S_ISBLK(statbuf.st_mode))
      flags |= O_RDWR;         /* not exists or regular => read-write */
    else flags |= O_WRONLY;     /* special => write-only */
  } else flags |= O_RDWR;
  var int result = OPEN(pathstring,flags,my_open_mask);
  end_system_call();
  if (result<0) { OS_file_error(STACK_0); } /* report error */
  return result;
 #endif
 #ifdef WIN32_NATIVE
  begin_system_call();
  var Handle handle = /* ignore wronly: no "special" files where it may hurt */
    CreateFile(pathstring, GENERIC_READ | GENERIC_WRITE,
               FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
               (truncate_if_exists ? CREATE_ALWAYS : OPEN_ALWAYS),
               FILE_ATTRIBUTE_NORMAL, NULL);
  end_system_call();
  if (handle==INVALID_HANDLE_VALUE) { OS_file_error(STACK_0); }
  return handle;
#endif
}
#endif

/* Create a backup file before opening a file for output.
 create_backup_file(pathstring,delete_backup_file);
 > led the way: assure_dir_exists()
 > pathstring: file name, ASCIZ-String
 > delete_backup_file: if true, delete the backup file
 > STACK_0: pathname
Can trigger GC */
local inline maygc void create_backup_file (char* pathstring,
                                            bool delete_backup_file) {
  var object filename = STACK_0;
  if (openp(filename))
    fehler_rename_open(filename); /* do not rename open files! */
  var object new_namestring;
 #if defined(UNIX) || defined(WIN32_NATIVE)
  /* extend truename with "%" resp. ".bak" resp. "~" :
   filename := (parse-namestring (concatenate 'string (namestring filename) "%")) : */
  filename = whole_namestring(filename); /* as String */
  pushSTACK(filename); pushSTACK(O(backupextend_string)); /* "%" */
  filename = string_concat(2); /* concatenate */
  pushSTACK(filename); /* save */
  pushSTACK(filename); /* save */
  filename = coerce_pathname(filename); /* again as filename */
  check_delete_open(filename);
  STACK_1 = filename;
  /* directory already exists. Do not resolve further links here. */
  new_namestring = popSTACK(); /* filename for the operating system */
 #endif
  with_sstring_0(new_namestring,O(pathname_encoding),new_namestring_asciz, {
    /* delete file (or link) with this name, if existing: */
    delete_file_before_rename(new_namestring_asciz);
    /* rename file from the old name to this name: */
    rename_existing_file(pathstring,new_namestring_asciz);
    if (delete_backup_file) { delete_existing_file(new_namestring_asciz); }
  });
  skipSTACK(1);
}

/* check the :DIRECTION argument */
global direction_t check_direction (object dir) {
  if (!boundp(dir) || eq(dir,S(Kinput)))
    return DIRECTION_INPUT;
  else if (eq(dir,S(Kinput_immutable)))
    return DIRECTION_INPUT_IMMUTABLE;
  else if (eq(dir,S(Koutput)))
    return DIRECTION_OUTPUT;
  else if (eq(dir,S(Kio)))
    return DIRECTION_IO;
  else if (eq(dir,S(Kprobe)))
    return DIRECTION_PROBE;
  else {
    pushSTACK(dir);               /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_direction)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(dir); pushSTACK(S(Kdirection));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: illegal ~S argument ~S"));
  }
}

local object direction_symbol (direction_t direction) {
  switch (direction) {
    case DIRECTION_INPUT: return S(Kinput);
    case DIRECTION_INPUT_IMMUTABLE: return S(Kinput_immutable);
    case DIRECTION_OUTPUT: return S(Koutput);
    case DIRECTION_IO: return S(Kio);
    case DIRECTION_PROBE: return S(Kprobe);
    default: NOTREACHED;
  }
}

/* check the :IF-DOES-NOT-EXIST argument
   check_if_does_not_exist(argument) */
global if_does_not_exist_t check_if_does_not_exist (object if_not_exist) {
  if (!boundp(if_not_exist))
    return IF_DOES_NOT_EXIST_UNBOUND;
  else if (eq(if_not_exist,S(Kerror)))
    return IF_DOES_NOT_EXIST_ERROR;
  else if (nullp(if_not_exist))
    return IF_DOES_NOT_EXIST_NIL;
  else if (eq(if_not_exist,S(Kcreate)))
    return IF_DOES_NOT_EXIST_CREATE;
  else {
    pushSTACK(if_not_exist);              /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_if_does_not_exist)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(if_not_exist); pushSTACK(S(Kif_does_not_exist));
    pushSTACK(S(open));
    fehler(type_error,GETTEXT("~S: illegal ~S argument ~S"));
  }
}

/* Converts a :IF-DOES-NOT-EXIST enum item to a symbol.
   if_does_not_exist_symbol(item)*/
global object if_does_not_exist_symbol (if_does_not_exist_t if_not_exist) {
  switch (if_not_exist) {
    case IF_DOES_NOT_EXIST_UNBOUND: return unbound;
    case IF_DOES_NOT_EXIST_ERROR: return S(Kerror);
    case IF_DOES_NOT_EXIST_NIL: return NIL;
    case IF_DOES_NOT_EXIST_CREATE: return S(Kcreate);
  }
  NOTREACHED;
}

/* check the :IF-EXISTS argument
   check_if_exists(argument) */
global if_exists_t check_if_exists (object if_exists) {
  if (!boundp(if_exists))
    return IF_EXISTS_UNBOUND;
  else if (eq(if_exists,S(Kerror)))
    return IF_EXISTS_ERROR;
  else if (nullp(if_exists))
    return IF_EXISTS_NIL;
  else if (eq(if_exists,S(Krename)))
    return IF_EXISTS_RENAME;
  else if (eq(if_exists,S(Krename_and_delete)))
    return IF_EXISTS_RENAME_AND_DELETE;
  else if (eq(if_exists,S(Knew_version)) || eq(if_exists,S(Ksupersede)))
    return IF_EXISTS_SUPERSEDE;
  else if (eq(if_exists,S(Kappend)))
    return IF_EXISTS_APPEND;
  else if (eq(if_exists,S(Koverwrite)))
    return IF_EXISTS_OVERWRITE;
  else {
    pushSTACK(if_exists);         /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_if_exists)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(if_exists); pushSTACK(S(Kif_exists)); pushSTACK(S(open));
    fehler(type_error,GETTEXT("~S: illegal ~S argument ~S"));
  }
}

/* Converts a :IF-EXISTS enum item to a symbol.
   if_exists_symbol(item) */
global object if_exists_symbol (if_exists_t if_exists) {
  switch (if_exists) {          /* :IF-EXISTS */
    case IF_EXISTS_UNBOUND: return unbound;
    case IF_EXISTS_ERROR: return S(Kerror);
    case IF_EXISTS_NIL: return NIL;
    case IF_EXISTS_RENAME: return S(Krename);
    case IF_EXISTS_RENAME_AND_DELETE: return S(Krename_and_delete);
    case IF_EXISTS_SUPERSEDE: return S(Ksupersede);
    case IF_EXISTS_APPEND: return S(Kappend);
    case IF_EXISTS_OVERWRITE: return S(Koverwrite);
  }
  NOTREACHED;
}

/* UP: check that the file we are about to open has not been opened yet
 > object truename - the name of the file that is being opened
 > direction_t direction - the direction of the pending OPEN
 can trigger GC - if CERROR is signaled */
extern void* find_open_file (struct file_id *fid, void* data);
local maygc void check_file_re_open (object truename, direction_t direction) {
  var uintB flags;
  switch (direction) {
    case DIRECTION_INPUT_IMMUTABLE: case DIRECTION_INPUT:
      flags = strmflags_wr_B;
      break;
    case DIRECTION_IO: case DIRECTION_OUTPUT:
      flags = (strmflags_rd_B | strmflags_wr_B);
      break;
    default: return;            /* PROBE: nothing to check */
  }
  var object bad_stream = nullobj;
  with_string_0(truename,O(pathname_encoding),namez, {
    begin_system_call();
    var void *ret = with_file_id(namez,(void*)&flags,&find_open_file);
    end_system_call();
    if (ret) bad_stream = popSTACK();
  });
  if (!eq(bad_stream,nullobj)) { /* found an existing open stream */
    pushSTACK(NIL);              /* 8: continue-format-string */
    pushSTACK(S(file_error));    /* 7: error type */
    pushSTACK(S(Kpathname));     /* 6: :PATHNAME */
    pushSTACK(truename);         /* 5: the offending pathname */
    pushSTACK(NIL);              /* 4: error-format-string */
    pushSTACK(TheSubr(subr_self)->name);       /* 3: caller */
    pushSTACK(bad_stream);                     /* 2: bad stream */
    pushSTACK(truename);                       /* 1: truename */
    pushSTACK(direction_symbol(direction));    /* 0: direction */
    STACK_8 = CLSTEXT("Open the file anyway"); /* continue-format-string */
    STACK_4 = CLSTEXT("~S: ~S already points to file ~S, opening the file again for ~S may produce unexpected results"); /* error-format-string */
    funcall(L(cerror_of_type),9);
  }
}

/* UP: create a file-stream
 open_file(filename,direction,if_exists,if_not_exists)
 > STACK_3: original filename (may be logical)
 > STACK_2: :BUFFERED argument
 > STACK_1: :EXTERNAL-FORMAT argument
 > STACK_0: :ELEMENT-TYPE argument
 > filename: filename, a pathname
 > direction: direction_t (see lispbibl.d)
 > if_exists: :IF-EXISTS argument if_exists_t (see lispbibl.d)
 > if_not_exists: :IF-DOES-NOT-EXIST argument (see lispbibl.d)
 < result: Stream or NIL
 < STACK: cleaned up
 can trigger GC */
local maygc object open_file (object filename, direction_t direction,
                              if_exists_t if_exists,
                              if_does_not_exist_t if_not_exists) {
  pushSTACK(STACK_3); /* save filename */
  /* Directory must exist: */
  var object namestring = /* File name for the operating system */
    /* tolerant only if :PROBE and if_not_exists = UNBOUND or NIL */
    true_namestring(filename,true,
                    ((direction == DIRECTION_PROBE)
                     && (if_not_exists == IF_DOES_NOT_EXIST_UNBOUND))
                    || (if_not_exists == IF_DOES_NOT_EXIST_NIL));
  if (eq(namestring,nullobj))
    /* path to the file does not exist,
     and :IF-DOES-NOT-EXIST = unbound or NIL */
    goto ergebnis_NIL;
  /* stack layout: Pathname, Truename.
   check filename and get the handle: */
  pushSTACK(namestring);        /* save */
  check_file_re_open(namestring,direction);
  namestring = popSTACK();      /* restore */
  var object handle;
 {var bool append_flag = false;
  var bool wronly_flag = false;
 {switch (direction) {
    case DIRECTION_PROBE:
      if (!file_exists(namestring)) { /* file does not exist */
        /* :IF-DOES-NOT-EXIST decides: */
        if (if_not_exists==IF_DOES_NOT_EXIST_ERROR)
          goto fehler_notfound;
        if (if_not_exists==IF_DOES_NOT_EXIST_UNBOUND
            || if_not_exists==IF_DOES_NOT_EXIST_NIL)
          goto ergebnis_NIL;
        /* :CREATE -> create the file using open and close: */
        with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
          create_new_file(namestring_asciz);
        });
      }
      handle = NIL; /* Handle := NIL */
      break;
    case DIRECTION_INPUT: case DIRECTION_INPUT_IMMUTABLE: { /* == :INPUT */
      var Handle handl;
      var bool result;
      with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
        result = open_input_file(namestring_asciz,
                                 if_not_exists==IF_DOES_NOT_EXIST_CREATE,
                                 &handl);
      });
      if (!result) {
        /* :IF-DOES-NOT-EXIST decides: */
        if (if_not_exists==IF_DOES_NOT_EXIST_NIL)
          goto ergebnis_NIL;
        else /* UNBOUND or :ERROR -> Error */
          goto fehler_notfound;
      }
      handle = allocate_handle(handl);
    }
      break;
    case DIRECTION_OUTPUT: wronly_flag = true; /*FALLTHROUGH*/
    case DIRECTION_IO:
      /* default for if_not_exists depends on if_exists: */
      if (if_not_exists==IF_DOES_NOT_EXIST_UNBOUND) {
        if (if_exists!=IF_EXISTS_APPEND && if_exists!=IF_EXISTS_OVERWRITE)
          /* (if_exists<IF_EXISTS_APPEND)
           if_exists = :APPEND or :OVERWRITE -> if_not_exists unchanged,
           otherwise :CREATE is the default */
          if_not_exists = IF_DOES_NOT_EXIST_CREATE;
      }
      /* default for if_exists is :SUPERSEDE (= :NEW-VERSION) : */
      if (if_exists==IF_EXISTS_UNBOUND)
        if_exists = IF_EXISTS_SUPERSEDE;
      #if defined(UNIX) || defined(WIN32_NATIVE)
      with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
        if (file_exists(namestring)) {
          /* file exists
           :IF-EXISTS decides: */
          switch (if_exists) {
            case IF_EXISTS_ERROR:
              goto fehler_exists;
            case IF_EXISTS_NIL:
              goto ergebnis_NIL;
            case IF_EXISTS_RENAME: case IF_EXISTS_RENAME_AND_DELETE:
              create_backup_file(namestring_asciz,
                                 if_exists==IF_EXISTS_RENAME_AND_DELETE);
              break;
            case IF_EXISTS_APPEND:
              append_flag = true; /* position at the end */
            default: ;
              /* :OVERWRITE -> use the existing file
               :NEW-VERSION, :SUPERSEDE -> truncate the file at 0 length */
          }
        } else {
          /* file does not exist
           :IF-DOES-NOT-EXIST decides: */
          if (if_not_exists==IF_DOES_NOT_EXIST_UNBOUND
              || if_not_exists==IF_DOES_NOT_EXIST_ERROR)
            goto fehler_notfound;
          if (if_not_exists==IF_DOES_NOT_EXIST_NIL)
            goto ergebnis_NIL;
          /* :CREATE */
        }
        /* open file:
         if-exists: if if_exists<IF_EXISTS_APPEND delete contents;
         othersise (with :APPEND, :OVERWRITE) preserve contents.
         if-not-exists: create new file. */
        var Handle handl =
          open_output_file(namestring_asciz,wronly_flag,
                           (if_exists!=IF_EXISTS_APPEND
                            && if_exists!=IF_EXISTS_OVERWRITE));
        handle = allocate_handle(handl);
      });
      #endif
      break;
    default: NOTREACHED;
      /* STACK_0 = Truename, FILE-ERROR slot PATHNAME */
  fehler_notfound: /* error: file not found */
      fehler_file_not_exists();
  fehler_exists: /* error: file already exists */
      fehler_file_exists();
 }}
 handle_ok:
  /* handle and append_flag are done with.
   make the Stream: */
  pushSTACK(STACK_4); /* :BUFFERED argument */
  pushSTACK(STACK_4); /* :EXTERNAL-FORMAT argument */
  pushSTACK(STACK_4); /* :ELEMENT-TYPE argument */
  pushSTACK(handle);
  {var object stream = make_file_stream(direction,append_flag,true);
   skipSTACK(4);
   return stream;
 }}
 ergebnis_NIL: /* return NIL */
  skipSTACK(6); /* forget both Pathnames and three arguments */
  return NIL;
}

/* (OPEN filename :direction :element-type :if-exists :if-does-not-exist
                :external-format :buffered) */
LISPFUN(open,seclass_default,1,0,norest,key,6,
        (kw(direction),kw(element_type),kw(if_exists),
         kw(if_does_not_exist),kw(external_format),kw(buffered)) ) {
  var object filename = STACK_6; /* filename */
  if (builtin_stream_p(filename)) {
    /* must be file-stream: */
    filename = as_file_stream(filename);
    test_file_stream_named(filename);
    /* streamtype file-stream -> use truename: */
    filename = TheStream(filename)->strm_file_truename;
    pushSTACK(filename);
  } else {
    filename = coerce_xpathname(filename); /* turn into a pathname */
    pushSTACK(filename);
   #ifdef LOGICAL_PATHNAMES
    /* Convert from logical to physical pathname: */
    if (logpathnamep(filename))
      filename = coerce_pathname(filename);
   #endif
    filename = merge_defaults(filename);
  }
  /* Stack layout: filename-arg, direction, element-type, if-exists,
                 if-does-not-exist, external-format, buffered, origpathname.
   filename is now a pathname. */
  var direction_t direction = check_direction(STACK_(5+1));
  var if_exists_t if_exists = check_if_exists(STACK_(3+1));
  var if_does_not_exist_t if_not_exists=check_if_does_not_exist(STACK_(2+1));
  /* :element-type is checked later.
   :external-format is checked later.
   :buffered is checked later.
   open file: */
  STACK_4 = STACK_5; STACK_5 = STACK_2; STACK_6 = STACK_1; STACK_7 = STACK_0;
  skipSTACK(4);
  VALUES1(open_file(filename,direction,if_exists,if_not_exists));
}

/* UP: Returns a list of all matching pathnames.
 directory_search(pathname,dir_search_param)
 > pathname: pathname with device /= :WILD
 > dir_search_param: :if-does-not-exist, :circle flag, :full flag
 < result:
     if name=NIL and type=NIL:     list of all matching directories,
     else (name=NIL -> name=:WILD):  list of all matching files.
     as absolute pathname without wildcards at a time,
     resp. for files and Full-Flag /=NIL as list
          (Pathname Write-Date Length)
          with Pathname without :WILD/:WILD-INFERIORS-components,
               Write-Date = Date of file creation (ss mm hh dd mm yy),
                 as Decoded-Time suitable for ENCODE-UNIVERSAL-TIME,
               Length = Length of the file (in Bytes).
 Method: Breadth-first-search (=> only one search operation runs at a time)
 can trigger GC */
typedef enum {
  DIR_IF_NONE_DISCARD, DIR_IF_NONE_ERROR, DIR_IF_NONE_KEEP, DIR_IF_NONE_IGNORE
} dir_search_if_none_t;
typedef struct {
  dir_search_if_none_t if_none;
  bool full_p;
  bool circle_p;
} dir_search_param_t;
local maygc object directory_search (object pathname, dir_search_param_t *dsp);

#ifdef WIN32_NATIVE
  /* Set of macros for directory search. */
  #define READDIR_wildnametype_suffix  O(wild_string) /* "*" */
  #define READDIR_var_declarations  \
    var WIN32_FIND_DATA filedata; \
    var HANDLE search_handle;
  #define READDIR_end_declarations
  #define READDIR_findfirst(pathstring,error_statement,done_statement) \
    if ((search_handle = FindFirstFile(pathstring,&filedata))          \
        == INVALID_HANDLE_VALUE) {                                     \
      if (!WIN32_ERROR_NOT_FOUND) { error_statement }                  \
      else { done_statement }                                          \
    }
  #define READDIR_findnext(error_statement,done_statement)    \
    if (!FindNextFile(search_handle,&filedata)) {             \
      if (!(GetLastError()==ERROR_NO_MORE_FILES)              \
            || !FindClose(search_handle))                     \
          { error_statement }                                 \
        else { done_statement }                               \
    }
  #define READDIR_entry_name()  (&filedata.cFileName[0])
  #define READDIR_entry_ISDIR()  (filedata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
  #define READDIR_entry_timedate(timepointp)  \
    { var FILETIME* pftimepoint = &filedata.ftLastWriteTime;               \
      if (pftimepoint->dwLowDateTime==0 && pftimepoint->dwHighDateTime==0) \
        pftimepoint = &filedata.ftCreationTime;                            \
      convert_time(pftimepoint,timepointp);                                \
    }
  #define READDIR_entry_size()  \
    (((uint64)filedata.nFileSizeHigh<<32)|filedata.nFileSizeLow)
#endif

#ifdef UNIX
/* Just like stat(), except that directories or files which would lead
 to problems are silently hidden. */
local inline int stat_for_search (char* pathstring, struct stat * statbuf) {
 #ifdef UNIX_LINUX
  /* Avoid searching /proc: It is a zoo containing strange animals:
   directories which go away constantly, pseudo-regular files which
   are really pipes, etc. */
  if (asciz_equal(pathstring,"/proc")) { errno = ENOENT; return -1; }
 #endif
  var int result = stat(pathstring,statbuf);
 #ifdef UNIX_CYGWIN32
  if ((result < 0) && (errno == EACCES)) { errno = ENOENT; }
 #endif
  return result;
}
#endif

#ifdef PATHNAME_NOEXT
/* UP: Extends the directory of a pathname by one component.
 > STACK_1: a pathname
 > STACK_0: new Subdir-component, a Simple-String
 < result: new pathname with directory lengthened by subdir
 removes the 2 arguments from the STACK
 can trigger GC */
local maygc object pathname_add_subdir (void) {
  /* copy pathname and lengthen its directory according to
   (append x (list y)) = (nreverse (cons y (reverse x))) : */
  var object pathname = copy_pathname(STACK_1);
  STACK_1 = pathname;
  pushSTACK(reverse(ThePathname(pathname)->pathname_directory));
  var object new_cons = allocate_cons();
  Cdr(new_cons) = popSTACK();
  Car(new_cons) = popSTACK();
  new_cons = nreverse(new_cons);
  pathname = popSTACK();
  ThePathname(pathname)->pathname_directory = new_cons;
  return pathname;
}

#ifdef UNIX
/* UP: extends a pathname by the file-information.
 > STACK_1: absolute pathname
 > STACK_0: absolute pathname, links resolved
 > *filestatus: its stat-info
 < STACK_0: list (Pathname Truename Write-Date Length [Comment])
            in :FULL-Format */
local void with_stat_info (void) {
  var object newlist;
  var off_t size = filestatus->st_size;
  { /* Pathname already in STACK_1, as 1. list element
   Truename already in STACK_0, as 2. list element */
    var decoded_time_t timepoint; /* Write-Date in decoded form */
    convert_time(&filestatus->st_mtime,&timepoint);
    pushSTACK(timepoint.Sekunden);
    pushSTACK(timepoint.Minuten);
    pushSTACK(timepoint.Stunden);
    pushSTACK(timepoint.Tag);
    pushSTACK(timepoint.Monat);
    pushSTACK(timepoint.Jahr);
    newlist = listof(6); /* build 6-element list */
  }
  pushSTACK(newlist); /* as 3. list element */
 #if SIZEOF_OFF_T > 4
  pushSTACK(UQ_to_I(size)); /* length as 4. list element */
 #else
  pushSTACK(UL_to_I(size)); /* length as 4. list element */
 #endif
  newlist = listof(4); /* build 4-element list */
  pushSTACK(Car(newlist)); /* pathname again in the stack */
  pushSTACK(newlist); /* list in the stack */
}
#endif

/* Search for a subdirectory with a given name.
 directory_search_1subdir(subdir,namestring);
 > STACK_0 = pathname
 > STACK_(3+1) = new-pathname-list
 > subdir: the new directory component to add to the pathname, if it exists
 > namestring: the namestring (for the OS)
 < STACK_0: replaced
 < STACK_(3+1): augmented
 can trigger GC */
local maygc void copy_pathname_and_add_subdir (object subdir)
{ /* copy pathname(STACK_0) and lengthen its directory by subdir: */
  pushSTACK(subdir);
  { var object pathname = pathname_add_subdir();
    pushSTACK(pathname);
  }
  { /* push this new pathname in front of new-pathname-list: */
    var object new_cons = allocate_cons();
    Car(new_cons) = STACK_0;
    Cdr(new_cons) = STACK_(3+1);
    STACK_(3+1) = new_cons;
  }
}

/* Check whether a directory exists and call copy_pathname_and_add_subdir()
   on it; if the directory does not exist or is a file, do nothing */
local maygc void check_sub_directory (object subdir, char* namestring_asciz) {
#if defined(UNIX)
  struct stat status;
  int ret;
  begin_system_call(); ret = stat(namestring_asciz,&status); end_system_call();
  if (ret) {
    if (errno != ENOENT)        /* subdirectory does not exist -> OK. */
      OS_file_error(STACK_0);
  } else {                       /* file exists. */
    if (S_ISDIR(status.st_mode)) /* is it a directory? */
      copy_pathname_and_add_subdir(subdir);
  }
#elif defined(WIN32_NATIVE)
  char resolved[MAX_PATH];
  if (real_path(namestring_asciz,resolved)) {
    DWORD fileattr;
    begin_system_call();
    fileattr = GetFileAttributes(resolved);
    end_system_call();
    if (fileattr == 0xFFFFFFFF) {
      /* you get ERROR_INVALID_NAME on GetFileAttributes("foo/")
         when file "foo" exists */
      if (!(WIN32_ERROR_NOT_FOUND || GetLastError() == ERROR_INVALID_NAME))
        OS_file_error(STACK_0);
    } else {                                   /* file exists. */
      if (fileattr & FILE_ATTRIBUTE_DIRECTORY) /* is it a directory? */
        copy_pathname_and_add_subdir(subdir);
    }
  }
#endif
}

local maygc void directory_search_1subdir (object subdir, object namestring) {
  with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
    check_sub_directory(subdir,namestring_asciz);
  });
}

#if defined(UNIX) || defined(WIN32_NATIVE)
/* Returns a truename dependent hash code for a directory.
 directory_search_hashcode()
 STACK_0 = dir_namestring
 STACK_1 = pathname
 < result: a hash code, or nullobj if the directory does not exist
 can trigger GC */

#ifdef UNIX
/* return (cons drive inode) */
local maygc object directory_search_hashcode (void) {
  pushSTACK(STACK_0); /* Directory-Name */
  pushSTACK(O(dot_string)); /* and "." */
  var object namestring = string_concat(2); /* concatenate */
  var struct stat status;
  with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
    begin_system_call();
    if (!( stat(namestring_asciz,&status) ==0)) { /* get information */
      end_system_call();
      FREE_DYNAMIC_ARRAY(namestring_asciz);
      return nullobj;
    }
    end_system_call();
  });
  /* entry exists (oh miracle...) */
  pushSTACK(UL_to_I(status.st_dev)); /* Device-Number and */
  #if SIZEOF_INO_T > 4
    pushSTACK(UQ_to_I(status.st_ino)); /* Inode-Number */
  #else
    pushSTACK(UL_to_I(status.st_ino)); /* Inode-Number */
  #endif
  var object new_cons = allocate_cons(); /* cons them together */
  Cdr(new_cons) = popSTACK(); Car(new_cons) = popSTACK();
  return new_cons;
}
#else
/* win32 - there is stat but no inodes
 using directory truenames as hashcodes */
local maygc object directory_search_hashcode (void) {
  return STACK_0;
}
#endif
#endif

#ifdef UNIX
/* Tests whether a directory entry actually exists.
 (It could be a link pointing to nowhere, or an undesired directory.)
 directory_search_direntry_ok(namestring,&statbuf)
 STACK_2 = pathname
 < result: true and statbuf filled, or false. */
local maygc bool directory_search_direntry_ok (object namestring,
                                               struct stat * statbuf) {
  var bool exists = true;
  with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
    begin_system_call();
    if (!( stat_for_search(namestring_asciz,statbuf) ==0)) {
      if (!((errno==ENOENT) || (errno==ELOOP_VALUE))) {
        end_system_call(); OS_file_error(STACK_2);
      }
      end_system_call();
      exists = false;
    }
    end_system_call();
  });
  return exists;
}
#endif

/* the version of files returned by DIRECTORY
 Since all pathnames returned by DIRECTORY must be truenames,
 this must be :NEWEST [but then they will not be printable readably!] */
#define DEFAULT_VERSION  S(Knewest)

/* Scans an entire directory.
 directory_search_scandir(recursively,next_task);
 stack layout: result-list, pathname, name&type, subdir-list, pathname-list,
              new-pathname-list, ht, pathname-list-rest, pathnames-to-insert,
              pathname, dir_namestring. */
local maygc void directory_search_scandir (bool recursively, signean next_task,
                                           dir_search_param_t *dsp) {
 #ifdef UNIX
  {
    var object namestring;
    pushSTACK(STACK_0); /* directory-name */
    pushSTACK(O(dot_string)); /* and "." */
    namestring = string_concat(2); /* concatenate */
    /* scan directory: */
    var DIR* dirp;
    set_break_sem_4();
    with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
      begin_system_call();
      dirp = opendir(namestring_asciz); /* open directory */
      end_system_call();
    });
    if (dirp == (DIR*)NULL) {
      if (dsp->if_none == DIR_IF_NONE_IGNORE) return;
      else OS_file_error(STACK_1);
    }
    loop {
      var SDIRENT* dp;
      begin_system_call();
      errno = 0;
      dp = readdir(dirp); /* fetch next directory-entry */
      if (dp == (SDIRENT*)NULL) { /* error or directory finished */
        if (!(errno==0)) { end_system_call(); OS_file_error(STACK_1); }
        end_system_call();
        break;
      }
      end_system_call();
      /* convert directory-entry into string: */
      var object direntry;
      {
        var uintL direntry_len;
       #if defined(UNIX_CYGWIN32)
        /* Neither d_reclen nor d_namlen present in DIR structure. */
        direntry_len = asciz_length(dp->d_name);
       #elif !defined(HAVE_STRUCT_DIRENT_D_NAMLEN) || defined(__USE_GNU)
        { /* On UNIX_LINUX direntry_len := dp->d_reclen was sufficient, but in
           general direntry_len := min(dp->d_reclen,asciz_length(dp->d_name))
           is necessary. The GNU libc is buggy: it does
           "#define d_namlen d_reclen", just as the Linux libc-5.0.9. */
          var const uintB* ptr = (const uintB*)(&dp->d_name[0]);
          var uintL count = dp->d_reclen;
          direntry_len = 0;
          while (count-- && *ptr++) direntry_len++;
        }
       #else
        direntry_len = dp->d_namlen;
       #endif
        direntry = n_char_to_string(&dp->d_name[0],direntry_len,O(pathname_encoding));
      }
      /* skip "." and ".." : */
      if (!(equal(direntry,O(dot_string))
            || equal(direntry,O(dotdot_string))))
        {
          pushSTACK(direntry);
          /* stack layout: ..., pathname, dir_namestring, direntry.
           determine, if it is a directory or a file: */
          pushSTACK(STACK_1); /* Directory-Namestring */
          SUBDIR_PUSHSTACK(direntry); /* direntry */
          var object namestring = string_concat(2); /* concatenate */
          /* get information: */
          var struct stat status;
         #if 1 /* just an optimization */
          if (!recursively) {
            /* Try to avoid calling directory_search_direntry_ok(),
             since it is an expensive operation (it calls stat()). */
            if (next_task < 0) {
              /* match (car subdir-list) with direntry: */
              if (wildcard_match(Car(STACK_(1+4+3)),STACK_0))
              if (directory_search_direntry_ok(namestring,&status)) {
                if (S_ISDIR(status.st_mode))
                  goto push_matching_subdir;
              } else
                switch (dsp->if_none) {
                  case DIR_IF_NONE_IGNORE: case DIR_IF_NONE_DISCARD: break;
                  case DIR_IF_NONE_ERROR:
                    pushSTACK(namestring);
                    fehler_file_not_exists();
                  case DIR_IF_NONE_KEEP:
                    goto push_matching_file;
                  default: NOTREACHED;
                }
            } else if (next_task > 0) {
              /* match name&type with direntry: */
              if (wildcard_match(STACK_(2+4+3),STACK_0))
              if (directory_search_direntry_ok(namestring,&status)) {
                if (!S_ISDIR(status.st_mode))
                  goto push_matching_file;
              } else
                switch (dsp->if_none) {
                  case DIR_IF_NONE_IGNORE: case DIR_IF_NONE_DISCARD: break;
                  case DIR_IF_NONE_ERROR:
                    pushSTACK(namestring);
                    fehler_file_not_exists();
                  case DIR_IF_NONE_KEEP:
                    goto push_matching_file;
                  default: NOTREACHED;
                }
            }
            goto done_direntry;
          }
         #endif
          if (directory_search_direntry_ok(namestring,&status)) {
            /* entry exists and is not unwanted. */
            if (S_ISDIR(status.st_mode)) { /* is it a directory? */
              /* entry is a directory. */
              if (recursively) { /* all recursive subdirectories wanted? */
                /* yes -> turn into a pathname and push to
                 pathnames-to-insert (is later insertet in front
                 of pathname-list-rest): */
                pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); /* pathname and direntry */
                {
                  var object pathname = pathname_add_subdir();
                  pushSTACK(pathname);
                }
                { /* push this new pathname in front of pathname-to-insert: */
                  var object new_cons = allocate_cons();
                  Car(new_cons) = popSTACK();
                  Cdr(new_cons) = STACK_(0+3);
                  STACK_(0+3) = new_cons;
                }
              }
              if (next_task<0) {
                /* match (car subdir-list) with direntry: */
                if (wildcard_match(Car(STACK_(1+4+3)),STACK_0))
                  {
                  push_matching_subdir:
                    /* subdirectory matches -> turn into a pathname
                     and push onto new-pathname-list: */
                    pushSTACK(STACK_2); pushSTACK(STACK_(0+1)); /* pathname and direntry */
                    {
                      var object pathname = pathname_add_subdir();
                      pushSTACK(pathname);
                    }
                    { /* push this new pathname
                         in front of new-pathname-list: */
                      var object new_cons = allocate_cons();
                      Car(new_cons) = popSTACK();
                      Cdr(new_cons) = STACK_(3+3);
                      STACK_(3+3) = new_cons;
                    }
                  }
              }
            } else {
              /* entry is a (halfway) normal File. */
              if (next_task>0) {
                /* match name&type with direntry: */
                if (wildcard_match(STACK_(2+4+3),STACK_0))
                  {
                  push_matching_file:
                    /* File matches -> turn into a pathname
                     and push onto result-list: */
                    pushSTACK(STACK_0); /* direntry */
                    split_name_type(1); /* split into Name and Type */
                    {
                      var object pathname = copy_pathname(STACK_(2+2));
                      ThePathname(pathname)->pathname_type = popSTACK(); /* insert type */
                      ThePathname(pathname)->pathname_name = popSTACK(); /* insert name */
                      ThePathname(pathname)->pathname_version = DEFAULT_VERSION;
                      pushSTACK(pathname);
                      pushSTACK(pathname);
                    }
                    /* form truename (resolve symbolic links): */
                    if (!eq(nullobj,assure_dir_exists(true,true))
                        && file_exists(_EMA_)) {
                      /* if file (still...) exists */
                      if (dsp->full_p) /* :FULL wanted? */
                        with_stat_info(); /* yes -> extend STACK_0 */
                      { /* and push STACK_0 in front of result-list: */
                        var object new_cons = allocate_cons();
                        Car(new_cons) = STACK_0;
                        Cdr(new_cons) = STACK_(4+4+3+2);
                        STACK_(4+4+3+2) = new_cons;
                      }
                    } else if (dsp->if_none == DIR_IF_NONE_KEEP) {
                      var object new_cons = allocate_cons();
                      Car(new_cons) = STACK_1; /* unresolved pathname */
                      Cdr(new_cons) = STACK_(4+4+3+2);
                      STACK_(4+4+3+2) = new_cons;
                    }
                    skipSTACK(2);
                  }
              }
            }
          } else
            switch (dsp->if_none) {
              case DIR_IF_NONE_IGNORE: case DIR_IF_NONE_DISCARD: break;
              case DIR_IF_NONE_ERROR:
                pushSTACK(namestring);
                fehler_file_not_exists();
              case DIR_IF_NONE_KEEP:
                goto push_matching_file;
              default: NOTREACHED;
            }
         done_direntry:
          skipSTACK(1); /* forget direntry */
        }
    }
    begin_system_call();
    if (CLOSEDIR(dirp)) { end_system_call(); OS_file_error(STACK_1); }
    end_system_call();
    clr_break_sem_4();
  }
 #endif
 #ifdef WIN32_NATIVE
  {
    SUBDIR_PUSHSTACK(STACK_0); /* Directory-Name */
    pushSTACK(READDIR_wildnametype_suffix); /* and concatenate */
    var object namestring = string_concat(2); /* "*.*" resp. "*" */
    with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
      /* scan directory, according to DOS- resp. Win32-convention: */
      READDIR_var_declarations;
      /* start of search, search for folders and normal files: */
      begin_system_call();
      do {
        /* readdir in resolved directory. directory was resolved earlier */
        READDIR_findfirst(namestring_asciz,{
          end_system_call();
          if (dsp->if_none == DIR_IF_NONE_IGNORE) {
            FREE_DYNAMIC_ARRAY(namestring_asciz); return;
          } else OS_file_error(STACK_1);
        }, break; );
        loop {
          end_system_call();
          /* convert directory-entry into string: */
          var object direntry = asciz_to_string(READDIR_entry_name(),O(pathname_encoding));
          /* skip "." and "..": */
          if (!(equal(direntry,O(dot_string))
                || equal(direntry,O(dotdot_string)))) {
            var shell_shortcut_target_t rresolved = shell_shortcut_notresolved;
            pushSTACK(direntry);
            /* stack layout: ..., pathname, dir_namestring, direntry. */
            pushSTACK(NIL);       /* will become found file full pathname, */
                                  /* changed with symbolic name for resolved (maybe nonfound) links */
            pushSTACK(NIL);       /* true pathname of it or whatever result to return */
            pushSTACK(direntry);  /* here will come filename to wildcard match */
            split_name_type(1);
            /* stack layout: ..., pathname, dir_namestring, direntry, NIL, NIL, direntry-name, direntry-type. */

            /* make full name of found file - dir + direntry
             TODO: optimize to not do it when it not needed */
            if (READDIR_entry_ISDIR()) {
              /* pathname and direntry: */
              pushSTACK(STACK_(6)); pushSTACK(STACK_(4+1));
              STACK_(3) = pathname_add_subdir();
            } else {
              STACK_(3) = copy_pathname(STACK_(6));
              ThePathname(STACK_(3))->pathname_type = STACK_0;
              ThePathname(STACK_(3))->pathname_name = STACK_1;
              ThePathname(STACK_(3))->pathname_version = DEFAULT_VERSION;
            }

            /* try to resolve .lnk files */
            if (!READDIR_entry_ISDIR() && !nullp(STACK_0)
                && string_equal(STACK_0,O(lnk_string)))
            {
              var char resolved[MAX_PATH];
              var char * full_resolved = resolved;
              with_sstring_0(whole_namestring(STACK_(3)),O(pathname_encoding),linkfile_asciiz, {
                rresolved =
                  resolve_shell_shortcut_more(linkfile_asciiz,resolved);
                if (rresolved != shell_shortcut_notresolved) {
                  var char resolved_f[MAX_PATH];
                  if (FullName(resolved,resolved_f))
                    full_resolved = resolved_f;
                  /* hack direntry-pathname to make it a symbolic name
                   symbolic link names are direntry-pathnames w/o ".lnk"
                   so split the name again
                   hack it in-place since lnk filename is not need anymore */
                  pushSTACK(STACK_1);
                  split_name_type(1);
                  ThePathname(STACK_(3+2))->pathname_name = STACK_1;
                  ThePathname(STACK_(3+2))->pathname_type = STACK_0;
                  ThePathname(STACK_(3+2))->pathname_version = DEFAULT_VERSION;
                  skipSTACK(2);
                  /* what to use as a result */
                  if (rresolved == shell_shortcut_notexists)
                    STACK_(2) = STACK_(3); /* use symbolic names as a result when target is not found */
                  else {
                    STACK_(2) = coerce_pathname(asciz_to_string(full_resolved,O(pathname_encoding)));
                    ThePathname(STACK_(2))->pathname_version = DEFAULT_VERSION;
                  }
                }
              });
            }

            if (rresolved == shell_shortcut_notresolved) {
              /* truename is the pathname itself */
              STACK_(2) = STACK_(3);
              /* nametomatch is direntry */
              STACK_(1) = STACK_(4);
            }

            skipSTACK(1); /* drop direntry-type */
            /* stack layout: ..., pathname, dir_namestring, direntry,
                direntry-pathname, true-pathname, direntry-name-to-check. */

            if (rresolved == shell_shortcut_notexists
                && dsp->if_none == DIR_IF_NONE_ERROR)
                  fehler_file_not_exists();

            if (rresolved != shell_shortcut_notexists
                || (dsp->if_none != DIR_IF_NONE_DISCARD
                    && dsp->if_none != DIR_IF_NONE_IGNORE)) {
              if (READDIR_entry_ISDIR() || rresolved == shell_shortcut_directory) {
                /* nonfound shortcuts are treated as shortcuts to files */
                if (recursively) { /* all recursive subdirectories wanted? */
                  /* yes -> push truename onto
                   pathnames-to-insert (is inserted in front of
                   pathname-list-rest later): */
                  var object new_cons = allocate_cons();
                  Car(new_cons) = STACK_(1);
                  Cdr(new_cons) = STACK_(0+6);
                  STACK_(0+6) = new_cons;
                }
                if (next_task<0) {
                  /* match (car subdir-list) with direntry: */
                  if (wildcard_match(Car(STACK_(1+4+6)),STACK_0)) {
                    /* Subdirectory matches -> push truename onto new-pathname-list: */
                    var object new_cons = allocate_cons();
                    Car(new_cons) = STACK_(1);
                    Cdr(new_cons) = STACK_(3+6);
                    STACK_(3+6) = new_cons;
                  }
                }
              } else {
                /* entry is a (halfway) normal file. */
                if (next_task>0) {
                  if (wildcard_match(STACK_(2+4+6),STACK_0)) {
                    /* stack layout: ..., pathname, dir_namestring, direntry,
                          direntry-maybhacked-pathname, true-pathname,
                          direntry-name-to-check.
                     test Full-Flag and poss. get more information: */

                    if (dsp->full_p      /* :FULL wanted? */
                        && rresolved != shell_shortcut_notexists) { /* treat nonexisting as :FULL NIL */
                      var decoded_time_t timepoint;
                      var off_t entry_size = 0;
                      /* get file attributes into these vars */
                      if (rresolved == shell_shortcut_file) {
                        /* need another readdir here */
                        READDIR_var_declarations;
                        with_sstring_0(whole_namestring(STACK_1),O(pathname_encoding),resolved_asciz, {
                          var bool notfound = false;
                          begin_system_call();
                          READDIR_findfirst(resolved_asciz, notfound = true; , notfound = true; );
                          end_system_call();
                          if (notfound || READDIR_entry_ISDIR()) {
                            /* just to be paranoid */
                            OS_file_error(STACK_1);
                          }
                          READDIR_entry_timedate(&timepoint);
                          entry_size = READDIR_entry_size();
                        });
                        READDIR_end_declarations;
                      } else {         /* easy way */
                        READDIR_entry_timedate(&timepoint);
                        entry_size = READDIR_entry_size();
                      }
                      pushSTACK(STACK_(2)); /* newpathname as 1st list element */
                      pushSTACK(STACK_(1+1)); /* resolved pathname as 2nd list element */
                      /* convert time and date from DOS-format to decoded-time: */
                      pushSTACK(timepoint.Sekunden);
                      pushSTACK(timepoint.Minuten);
                      pushSTACK(timepoint.Stunden);
                      pushSTACK(timepoint.Tag);
                      pushSTACK(timepoint.Monat);
                      pushSTACK(timepoint.Jahr);
                      { /* 6-element timestamp ==> the 3rd element */
                        object timestamp = listof(6); pushSTACK(timestamp); }
                      pushSTACK(off_to_I(entry_size)); /* length ==> 4th */
                      { object fullinfo = listof(4); STACK_1 = fullinfo; }
                    }
                    /* now STACK_1 can contain either truename or
                     list-of-file-information (both for insertion to result-list)
                     stack layout: ..., pathname, dir_namestring, direntry,
                           direntry-maybhacked-pathname,
                           true-pathname-or-list-of-info,
                           direntry-name-to-check. */
                    { /* push STACK_1 in front of result-list: */
                      var object new_cons = allocate_cons();
                      Car(new_cons) = STACK_1;
                      Cdr(new_cons) = STACK_(4+4+6);
                      STACK_(4+4+6) = new_cons;
                    }
                  }
                }
              }
            }
            skipSTACK(4); /* forget all up to dir_namestring */
          }
          /* next file: */
          begin_system_call();
          READDIR_findnext({ end_system_call(); OS_file_error(STACK_1); }, break; );
        }
      } while (false);
      end_system_call();
      READDIR_end_declarations;
    });
  }
 #endif
}

local maygc object directory_search (object pathname, dir_search_param_t *dsp) {
  pathname = use_default_dir(pathname); /* insert default-directory */
  /* pathname is now new and an absolute pathname. */
  pushSTACK(NIL); /* result-list := NIL */
  pushSTACK(pathname);
  /* if name=NIL and type/=NIL: set name := "*". */
  if (nullp(ThePathname(pathname)->pathname_name)
      && !nullp(ThePathname(pathname)->pathname_type))
    ThePathname(pathname)->pathname_name = S(Kwild);
  /* for matching: collect name and type into a string: */
  if (nullp(ThePathname(pathname)->pathname_name)) {
    pushSTACK(NIL); /* name=NIL -> also type=NIL -> do not search files */
  } else {
    var object nametype_string = file_namestring(pathname);
    pathname = STACK_0;
    pushSTACK(nametype_string);
  }
  pushSTACK(ThePathname(pathname)->pathname_directory); /* subdir-list */
  /* copy pathname and thereby discard name and type and
   shorten directory to (:ABSOLUTE) resp. (:ABSOLUTE :ROOT) : */
  pathname = copy_pathname(pathname);
  ThePathname(pathname)->pathname_name = NIL;
  ThePathname(pathname)->pathname_type = NIL;
  ThePathname(pathname)->pathname_version = NIL;
  ThePathname(pathname)->pathname_directory = O(directory_absolute);
  pushSTACK(pathname);
  { /* pack into one-element list: */
    var object new_cons = allocate_cons();
    Car(new_cons) = STACK_0;
    STACK_0 = new_cons;
  }
  var bool recursively = /* Flag, if the next operation has to be applied */
    false;               /* to all subdirectories. */
  loop {
    /* stack layout: result-list, pathname, name&type, subdir-list,
                   pathname-list.
     result-list = list of finished pathnames/lists, reversed.
     name&type = NIL or Normal-Simple-String,
       against which the filenames have to be matched.
     pathname-list = list of directories to be processed.
     the pathnames from pathname-list contain the directory
     only so deep, that afterwards work continues with (cdr subdir-list) .
     process next subdir-level: */
    STACK_1 = Cdr(STACK_1); /* shorten subdir-list */
    var signean next_task; /* what has to be done with the Dirs from pathname-list: */
    /* 0: nothing, finished
       1: look for a file of given namen/type
      -1: look for a subdirectory of given name
       2: look for all files, that match the given name/type
      -2: look for all subdirectories, that match the given name */
    if (matomp(STACK_1)) { /* subdir-list finished? */
      var object nametype = STACK_2;
      if (nullp(nametype)) /* name=NIL and type=NIL -> do not search files */
        next_task = 0;
     #if !defined(WIN32_NATIVE)
      else if (!wild_p(nametype,false) && (dsp->if_none != DIR_IF_NONE_IGNORE))
        /* === !(wild_p(name) || ((!nullp(type)) && wild_p(type))) */
        next_task = 1; /* search file */
     #endif
      else
        next_task = 2; /* search files with wildcards */
    } else {
      var object next_subdir = Car(STACK_1);
      if (eq(next_subdir,S(Kwild_inferiors))) { /* '...' ? */
        /* will be treated at the next run */
        recursively = true; goto passed_subdir;
      }
      if (!wild_p(next_subdir,false))
        next_task = -1; /* search subdir */
      else
        next_task = -2; /* search subdirs with wildcards */
    }
    /* traverse pathname-list and construct new list: */
    pushSTACK(NIL);
   #if defined(UNIX) || defined(WIN32_NATIVE)
    if (dsp->circle_p) { /* query :CIRCLE-Flag */
      /* maintain hash-table of all scanned directories so far (as
       cons (dev . ino)) : */
      /* (MAKE-HASH-TABLE :KEY-TYPE '(CONS INTEGER INTEGER) :VALUE-TYPE '(EQL T)
                          :TEST 'EQUAL) */
      pushSTACK(S(Ktest)); pushSTACK(S(equal));
      funcall(L(make_hash_table),2);
      pushSTACK(value1);
    } else
   #endif
      pushSTACK(NIL);
    pushSTACK(STACK_(0+2));
    loop {
      /* stack layout: ..., new-pathname-list, ht, pathname-list-rest. */
      var object pathname_list_rest = STACK_0;
      if (atomp(pathname_list_rest))
        break;
      STACK_0 = Cdr(pathname_list_rest); /* shorten list */
      pushSTACK(NIL); /* pathnames-to-insert := NIL */
      /* stack layout: ..., new-pathname-list, ht, pathname-list-rest,
                     pathnames-to-insert. */
      {
        var object pathname = Car(pathname_list_rest); /* next directory */
        pushSTACK(pathname); /* into the stack */
        /* try to shorten the task a little: */
        if (!recursively) {
          switch (next_task) {
            case 0: /* return this pathname */
             #if defined(UNIX) || defined(WIN32_NATIVE)
              assure_dir_exists(false,false); /* first resolve links */
             #endif
              { /* and push STACK_0 in front of result-list: */
                var object new_cons = allocate_cons();
                Car(new_cons) = popSTACK();
                Cdr(new_cons) = STACK_(4+4);
                STACK_(4+4) = new_cons;
              }
              goto next_pathname;
           #if !defined(WIN32_NATIVE)
            case 1: /* look in this pathname for a file */
              ThePathname(pathname)->pathname_name = /* insert name (/=NIL) */
                ThePathname(STACK_(3+4+1))->pathname_name;
              ThePathname(pathname)->pathname_type = /* insert type */
                ThePathname(STACK_(3+4+1))->pathname_type;
              ThePathname(pathname)->pathname_version =
                DEFAULT_VERSION; /* original may be :WILD! */
              pushSTACK(pathname);
              assure_dir_exists(true,false); /* resolve links, search file */
              if (file_exists(_EMA_)) { /* if file exists */
                /* extend result-list: */
                if (dsp->full_p) /* :FULL wanted? */
                  with_stat_info(); /* yes -> extend STACK_0 */
                /* and push STACK_0 in front of result-list: */
                var object new_cons = allocate_cons();
                Car(new_cons) = STACK_0;
                Cdr(new_cons) = STACK_(4+4+2);
                STACK_(4+4+2) = new_cons;
              }
              skipSTACK(2);
              goto next_pathname;
           #endif
            case -1: /* search for a subdirectory in this pathname */
              {
                var object namestring = assure_dir_exists(true,false); /* resolve links, directory-namestring */
                pushSTACK(namestring); /* directory-namestring */

                {
                  var object subdir = Car(STACK_(1+4+1+1)); /* (car subdir-list) */
                  SUBDIR_PUSHSTACK(subdir);
                }
               #if defined(WIN32_NATIVE)
                pushSTACK(O(backslash_string));
                namestring = string_concat(3); /* concatenate */
               #else
                namestring = string_concat(2);
               #endif
                /* get information: */
                directory_search_1subdir(Car(STACK_(1+4+1)),namestring);
              }
              skipSTACK(1);
              goto next_pathname;
          }
        }
        /* in order to finish the task, all entries in this directory
         have to be scanned: */
        {
          var object dir_namestring = assure_dir_exists(true,false); /* resolve links, form directory-name */
          pushSTACK(dir_namestring); /* save */
        }
        /* stack layout: ..., pathname, dir_namestring. */
       #if defined(UNIX) || defined(WIN32_NATIVE)
        if (dsp->circle_p) { /* query :CIRCLE flag */
          /* search pathname in the hash-table: */
          var object hashcode = directory_search_hashcode();
          if (eq(hashcode,nullobj)) {
            /* entry does not exist, however (this can happen to us
             only for symbolic links)
             -> will be skipped */
            skipSTACK(2); goto next_pathname;
          }
          /* and locate in the hash-table and store: */
          if (!nullp(shifthash(STACK_(2+2),hashcode,T,true))) {
            /* was already inside -> will be skipped */
            skipSTACK(2); goto next_pathname;
          }
        }
       #endif
        if (next_task==0) {
          /* push pathname STACK_1 in front of result-list: */
          var object new_cons = allocate_cons();
          Car(new_cons) = STACK_1;
          Cdr(new_cons) = STACK_(4+4+2);
          STACK_(4+4+2) = new_cons;
        }
        directory_search_scandir(recursively,next_task,dsp);
        skipSTACK(2); /* forget pathname and dir_namestring */
      next_pathname: ;
      }
      /* stack layout: ..., new-pathname-list, ht, pathname-list-rest, pathnames-to-insert.
       Before advancing with pathname-list-rest :
       pathname-list-rest := (nreconc pathnames-to-insert pathname-list-rest): */
      var object pathnames_to_insert = popSTACK();
      STACK_0 = nreconc(pathnames_to_insert,STACK_0);
    }
    skipSTACK(2); /* forget empty pathname-list-rest and hash-table */
    { /* reverse new-pathname-list, replaces the emptied pathname-list: */
      var object new_pathname_list = popSTACK();
      STACK_0 = nreverse(new_pathname_list); /* new pathname-list */
    }
    /* we are finished with this subdir-stage. */
    if (matomp(STACK_1))
      break; /* (atom subdir-list) -> finished. */
    recursively = false; /* the next (preliminarily) non-recursive */
  passed_subdir: ;
  }
  /* stack layout: result-list pathname name&type subdir-list pathname-list
   subdir-list became =NIL , also pathname-list = NIL (because at the last
   loop iteration next_task is always =0,1,2, so nothing
   was pushed on new-pathname-list). */
  skipSTACK(4);
  return popSTACK(); /* result-list as result */
}
#endif /* PATHNAME_NOEXT */

/* (DIRECTORY pathname [:circle] [:full] [:if-does-not-exist]),
   CLTL p. 427 */
LISPFUN(directory,seclass_read,1,0,norest,key,3,
        ( kw(if_does_not_exist),kw(circle),kw(full) ))
{ /* stack layout: pathname, if-does-not-exist, circle, full. */
  var dir_search_param_t dsp;
  if (!boundp(STACK_2) || eq(STACK_2,S(Kdiscard)))
    /* :IF-DOES-NOT-EXIST defaults to :DISCARD */
    dsp.if_none = DIR_IF_NONE_DISCARD;
  else if (eq(STACK_2,S(Kerror)))
    dsp.if_none = DIR_IF_NONE_ERROR;
  else if (eq(STACK_2,S(Kkeep)))
    dsp.if_none = DIR_IF_NONE_KEEP;
  else if (eq(STACK_2,S(Kignore)))
    dsp.if_none = DIR_IF_NONE_IGNORE;
  else {
    pushSTACK(STACK_2); /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_directory_not_exist)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(STACK_(2+2)); /* :IF-DOES-NOT-EXIST argument */
    pushSTACK(S(Kif_does_not_exist)); pushSTACK(S(directory));
    fehler(type_error,GETTEXT("~S: illegal ~S argument ~S"));
  }
  dsp.circle_p = !missingp(STACK_1); /* :CIRCLE argument defaults to NIL */
  dsp.full_p = !missingp(STACK_0); /* :FULL argument defaults to NIL */
  skipSTACK(3);
  /* check pathname-argument: */
  var object pathname = merge_defaults(coerce_pathname(STACK_0));
  /* let's go: */
 #ifdef PATHNAME_WIN32
  if (eq(ThePathname(pathname)->pathname_device,S(Kwild))) {
    /* Device = :WILD ? ==> scan all devices */
    STACK_0 = pathname;
    pushSTACK(NIL); /* pathname-list := NIL */
    { /* stack layout: pathname, pathname-list. */
      var char drive;
      for (drive='A'; drive<='Z'; drive++) /* traverse all drives */
        if (good_drive(drive)) {
          var object newpathname = copy_pathname(STACK_1);
          ThePathname(newpathname)->pathname_device =
            /* take over the device = one-element drive string */
            n_char_to_string(&drive,1,O(pathname_encoding));
          /* search within a drive: */
          var object newpathnames = directory_search(newpathname,&dsp);
          /* and attach pathname-list in front of STACK_0: */
          STACK_0 = nreconc(newpathnames,STACK_0);
        }
    }
    VALUES1(nreverse(STACK_0)); /* reverse pathname-list again */
    skipSTACK(2);
  } else
    /* only one device to scan */
 #endif
  {
    VALUES1(directory_search(pathname,&dsp)); /* form matching pathnames */
    skipSTACK(1);
  }
}

/* UP: make sure that the supposed directory namestring ends with a slash
 returns a new string with a slash appended or the same stirng
 can trigger GC */
local maygc object ensure_last_slash (object dir_string) {
  ASSERT(stringp(dir_string));
  var uintL len, offset;
  var object str = unpack_string_ro(dir_string,&len,&offset);
  var chart ch = schar(str,len+offset-1);
  if (!pslashp(ch) && !lslashp(ch)) {
    var char sl = (looks_logical_p(dir_string) ? ';' : slash);
    with_sstring_0(str,O(pathname_encoding),asciz, {
      dir_string = asciz_add_char(asciz,len,sl,O(pathname_encoding));
    });
  }
  return dir_string;
}

/* (CD [pathname]) sets the current drive and the current directory. */
LISPFUN(cd,seclass_default,0,1,norest,nokey,0,NIL) {
  var object pathname = popSTACK();
  if (!boundp(pathname)) { pathname = O(empty_string); } /* "" */
  else if (stringp(pathname)) /* make sure it ends with a slash */
    pathname = ensure_last_slash(pathname);
  pathname = merge_defaults(coerce_pathname(pathname)); /* --> pathname */
  /* no need to copy: merge_defaults produces a fresh pathname
     set name and type to NIL: */
  ThePathname(pathname)->pathname_name = NIL;
  ThePathname(pathname)->pathname_type = NIL;
  true_namestring(pathname,false,false); /* the directory must exist */
  change_default(); /* set default drive and default directory */
  VALUES1(popSTACK()); /* new pathname as the value */
}
#undef lslashp
#undef pslashp

/* UP: checks a pathname, if both name and type are =NIL ,
 and if the directory "almost" exists.
 shorter_directory(pathname,resolve_links)
 > pathname : Pathname-Argument
 > resolve_links : flag, if links have to be resolved (usually yes)
 < -(STACK) : absolute pathname */
#ifdef WIN32_NATIVE
/* < result: Directory-Namestring (for the OS, without '\' at the end, Normal-Simple-String) */
#endif
#ifdef UNIX
/* < result: Directory-Namestring (for the OS, without '/' at the end, Normal-Simple-String) */
#endif
/* decrements STACK by 1.
 can trigger GC */
local maygc object shorter_directory (object pathname, bool resolve_links) {
  pathname = merge_defaults(coerce_pathname(pathname)); /* --> pathname */
  check_no_wildcards(pathname); /* with wildcards -> error */
  pathname = use_default_dir(pathname); /* insert default-directory */
  check_notdir(pathname); /* ensure that Name=NIL and Type=NIL */
  pushSTACK(pathname); /* save new pathname */
  /* shorten the directory: */
  var object subdirs = ThePathname(pathname)->pathname_directory;
  if (nullp(Cdr(subdirs))) { /* root-directory ? */
    /* STACK_0 = pathname, FILE-ERROR slot PATHNAME */
    pushSTACK(STACK_0);
    fehler(file_error,GETTEXT("root directory not allowed here: ~S"));
  }
  subdirs = reverse(subdirs); /* copy list and reverse */
  pushSTACK(subdirs); /* save cons with last subdir as CAR */
  subdirs = Cdr(subdirs); /* all subdirs except for the last */
  subdirs = nreverse(subdirs); /* bring into right order again */
  pathname = STACK_1;
  ThePathname(pathname)->pathname_directory = subdirs; /* and store in the pathname */
  /* this directory must exist: */
  pushSTACK(pathname);
  /* stack layout: pathname, subdircons, pathname. */
  var object dir_namestring =
    (resolve_links ? assure_dir_exists(false,false) : assume_dir_exists());
  /* build subdir-string for the operating system: */
  STACK_0 = dir_namestring; /* directory-namestring so far as 1. String */
  var uintC stringcount =  /* the strings in the last subdir */
    subdir_namestring_parts(STACK_1,false);
  /* and no '\' at the end (for the OS)
   and no '/' at the end (for the OS) */
  var object dirstring = string_concat(1+stringcount); /* concatenate */
  skipSTACK(1);
  return dirstring;
}

LISPFUNN(make_dir,1)
{ /* (MAKE-DIR pathname) makes a new subdirectory pathname. */
  var object pathstring = shorter_directory(STACK_0,true);
  with_sstring_0(pathstring,O(pathname_encoding),pathstring_asciz, {
    make_directory(pathstring_asciz);
  });
  skipSTACK(2);
  VALUES1(T);
}

LISPFUNN(delete_dir,1)
{ /* (DELETE-DIR pathname) removes the subdirectory pathname. */
  var object pathstring = shorter_directory(STACK_0,true);
  with_sstring_0(pathstring,O(pathname_encoding),pathstring_asciz, {
    delete_directory(pathstring_asciz);
  });
  skipSTACK(2);
  VALUES1(T);
}

/* (defun ensure-directories-exist (pathspec &key verbose)
   (let* ((dir (pathname-directory pathspec))
          (path (make-pathname :host (pathname-host pathspec)
                               :device (pathname-device pathspec)
                               :directory dir)))
     (when (wild-pathname-p path)
       (error (make-condition (quote file-error) :pathname pathspec)))
     (if (directory path)
       (values pathspec nil)
       (loop for i from 1 upto (length dir)
         for newpath = (make-pathname :host (pathname-host pathspec)
                                      :device (pathname-device pathspec)
                                      :directory (subseq dir 0 i))
         unless (directory newpath)
         do (let ((namestring (namestring newpath)))
              (when verbose
                (format *standard-output* "~&Creating directory: ~A~%"
                        namestring))
              (ignore-errors (ext:make-dir namestring))
              (unless (directory newpath)
                 (error (make-condition (quote file-error)
                                        :pathname pathspec))))))
         finally (return (values pathspec t)))) */
LISPFUN(ensure_directories_exist,seclass_default,1,0,norest,key,1,
        (kw(verbose))) {
  var object pathname = coerce_pathname(STACK_1);
  pathname = merge_defaults(pathname); /* copy and discard name, type */
  ThePathname(pathname)->pathname_name = NIL;
  ThePathname(pathname)->pathname_type = NIL;
  check_no_wildcards(pathname); /* with wildcards -> error */
  pathname = use_default_dir(pathname); /* insert default-directory */
  pushSTACK(pathname); /* save new pathname */
  /* stack layout: pathspec, verbose, pathname. */
  if (directory_exists(pathname)) {
    skipSTACK(2); value2 = NIL; /* pathspec, NIL as values */
  } else {
    var object subdirs = copy_list(ThePathname(STACK_0)->pathname_directory);
    pushSTACK(subdirs); pushSTACK(Cdr(subdirs));
    Cdr(subdirs) = NIL;
    ThePathname(STACK_2)->pathname_directory = subdirs;
    /* stack layout: pathspec, verbose, pathname, (car (last subdirs)),
         remaining_subdirs. */
    while (mconsp(STACK_0)) {
      subdirs = STACK_0;
      Cdr(STACK_1) = subdirs; STACK_1 = subdirs; STACK_0 = Cdr(subdirs); Cdr(subdirs) = NIL;
      if (!directory_exists(STACK_2)) {
        if (!missingp(STACK_3)) { /* Verbose? */
          funcall(L(fresh_line),0); /* (FRESH-LINE [*standard-output*]) */
          pushSTACK(CLSTEXT("Creating directory: ")); funcall(L(write_string),1); /* (WRITE-STRING "..." [*standard-output*]) */
          pushSTACK(STACK_2); funcall(L(princ),1); /* (PRINC pathname [*standard-output*]) */
          funcall(L(terpri),0); /* (TERPRI [*standard-output*]) */
        }
        /* side remark: Do not need to resolve links here, because here we
         proceed step by step starting at the root, anyway. */
        var object pathstring = shorter_directory(STACK_2,false);
        with_sstring_0(pathstring,O(pathname_encoding),pathstring_asciz, {
          make_directory(pathstring_asciz);
        });
        skipSTACK(1);
      }
    }
    skipSTACK(4); value2 = T; /* pathspec, T as values */
  }
  value1 = popSTACK(); mv_count=2;
}

#ifdef UNIX
/* Returns the struct passwd entry for the current user.
 The return value points to static data, or is NULL upon failure. */
local struct passwd * unix_user_pwd (void) {
  var const char* username;
  var struct passwd * userpasswd = NULL;
  /* The manpage for GETLOGIN(3V) recommends
   first getpwnam(getlogin()), then getpwuid(getuid()). */
  begin_system_call();
  /* 1. attempt: getpwnam(getenv("USER")) */
  username = getenv("USER");
  if (username != NULL) {
    errno = 0; userpasswd = getpwnam(username);
    if (userpasswd != NULL) goto ok;
    if (errno != 0) { OS_error(); }
  }
  /* 2. attempt: getpwnam(getlogin()) */
  username = getlogin();
  if (username != NULL) {
    errno = 0; userpasswd = getpwnam(username);
    if (userpasswd != NULL) goto ok;
    if (errno != 0) { OS_error(); }
  }
  /* 3. attempt: getpwuid(getuid()) */
  errno = 0; userpasswd = getpwuid(getuid());
  if (userpasswd != NULL) goto ok;
  if (errno != 0) { OS_error(); }
  /* Everything fails, userpasswd == NULL. */
 ok:
  end_system_call();
  return userpasswd;
}
#endif

/* UP: Initializes the pathname-system.
 init_pathnames();
 can trigger GC */
global maygc void init_pathnames (void) {
 #ifdef PATHNAME_WIN32
  { /* initialize default-drive: */
    var char drive = default_drive();
    O(default_drive) =
      (drive == 0 ? NIL                  /* network */
       : n_char_to_string(&drive,1,O(pathname_encoding))); /* local device */
  }
 #endif
  /* initialize *DEFAULT-PATHNAME-DEFAULTS* : */
  recalc_defaults_pathname();
 #ifdef USER_HOMEDIR
  #ifdef UNIX
  /* we retrieve the home-directory and the usable shell from the
   environment. It contains (almost) always at least the following variables:
     LOGNAME = Username at the first login ("true" identity of the user)
     USER    = current username
     HOME    = current home-directory, fetched from /etc/passwd
     SHELL   = current standard-shell, fetched from /etc/passwd
     PATH    = search path for program call
     TERM    = terminal emulation
   we retrieve HOME (for "~" - translation) and SHELL (for EXECUTE).
   For "~username" we must scan the /etc/passwd - file. */
  { /* search in the environment for variable HOME: */
    begin_system_call();
    var const char* homedir = getenv("HOME");
    end_system_call();
    if (homedir != NULL) { /* found? */
      O(user_homedir) = asciz_dir_to_pathname(homedir,O(misc_encoding)); /* yes -> enter */
    } else { /* no -> get home-directory from the passwort-file: */
      var struct passwd * userpasswd = unix_user_pwd();
      if (userpasswd != NULL) { /* no -> enter homedir as pathname */
        O(user_homedir) = asciz_dir_to_pathname(userpasswd->pw_dir,O(misc_encoding));
      } else { /* no -> take current directory: */
        O(user_homedir) = default_directory();
      }
    }
  }
  #endif
  #ifdef WIN32
  /* WinNT defines HOMEDRIVE and HOMEPATH. Win95 (which is actually not a
   multiuser OS) lets the user set HOME himself.
   In any case, we give preference to HOME, because the user can change
   this. */
  {
    var const char * home;
    begin_system_call();
    home = getenv("HOME");
    if (home != NULL) {
      end_system_call();
      O(user_homedir) = asciz_dir_to_pathname(home,O(misc_encoding));
    } else {
      var const char * homedrive = getenv("HOMEDRIVE");
      var const char * homepath = getenv("HOMEPATH");
      end_system_call();
      if (homedrive!=NULL && homepath!=NULL) {
        var char* homeall = (char*)alloca(asciz_length(homedrive)+asciz_length(homepath)+1);
        var char* ptr = homeall;
        while ((*ptr = *homedrive) != '\0') { homedrive++; ptr++; }
        while ((*ptr = *homepath) != '\0') { homepath++; ptr++; }
        *ptr = '\0';
        O(user_homedir) = asciz_dir_to_pathname(homeall,O(misc_encoding));
      } else {
        O(user_homedir) = use_default_dir(asciz_dir_to_pathname(".",Symbol_value(S(ascii))));
      }
    }
  }
  #endif
 #endif
 #ifdef HAVE_SHELL
  #ifdef UNIX
  /* the command-shell O(command_shell) remains unchanged, otherwise
   we get too many portability problems. */
  { /* search the environment for variable SHELL: */
    begin_system_call();
    var const char* shell = getenv("SHELL");
    end_system_call();
    if (shell != NULL) { /* found? ==> enter */
      O(user_shell) = asciz_to_string(shell,O(misc_encoding));
    }
    /* else O(user_shell) remains on the default value "/bin/csh". */
  }
  #endif
  #ifdef WIN32_NATIVE
  { /* search in the environment for variable COMSPEC: */
    begin_system_call();
    var const char* shell = getenv("COMSPEC");
    if (!(shell==NULL)) {
      end_system_call();
      O(command_shell) = asciz_to_string(shell,O(misc_encoding)); /* enter */
    } else {
      var OSVERSIONINFO v;
      v.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
      if (!GetVersionEx(&v)) { OS_error(); }
      if (v.dwPlatformId == VER_PLATFORM_WIN32_NT) { /* Windows NT */
        shell = "cmd.exe";
      } else { /* Windows 95 or else */
        shell = "command.com";
      }
      end_system_call();
      O(command_shell) = ascii_to_string(shell); /* enter */
    }
  }
  #endif
 #endif
}

LISPFUNNR(file_write_date,1)
{ /* (FILE-WRITE-DATE file), CLTL p. 424 */
 #ifdef UNIX
  var time_t file_datetime; /* buffer for date/time of a file */
 #endif
 #ifdef WIN32_NATIVE
  var WIN32_FIND_DATA filedata;
 #endif
  var object pathname = popSTACK(); /* pathname-argument */
  if (builtin_stream_p(pathname)) { /* stream -> treat extra: */
    /* must be file-stream: */
    pathname = as_file_stream(pathname);
    /* streamtype file-stream */
    if ((TheStream(pathname)->strmflags & strmflags_open_B)
        && (!nullp(TheStream(pathname)->strm_buffered_channel))) {
      /* open file-stream
       work with the handle directly: */
     #ifdef UNIX
      var struct stat status;
      begin_system_call();
      if (!( fstat(TheHandle(TheStream(pathname)->strm_buffered_channel),&status) ==0)) {
        end_system_call(); OS_filestream_error(pathname);
      }
      end_system_call();
      file_datetime = status.st_mtime;
     #endif
     #ifdef WIN32_NATIVE
      var BY_HANDLE_FILE_INFORMATION fileinfo;
      var BOOL result;
      begin_system_call();
      result = GetFileInformationByHandle(TheHandle(TheStream(pathname)->strm_buffered_channel),&fileinfo);
      end_system_call();
      if (result) {
        filedata.ftCreationTime   = fileinfo.ftCreationTime;
        filedata.ftLastAccessTime = fileinfo.ftLastAccessTime;
        filedata.ftLastWriteTime  = fileinfo.ftLastWriteTime;
      } else { /* If that failed, try the full pathname. */
        test_file_stream_named(pathname);
        pathname = TheStream(pathname)->strm_file_truename;
        goto is_pathname;
      }
     #endif
    } else {
      /* closed file-stream -> use truename as pathname */
      test_file_stream_named(pathname);
      pathname = TheStream(pathname)->strm_file_truename;
      goto is_pathname;
    }
  } else { /* turn into a pathname */
    pathname = merge_defaults(coerce_pathname(pathname));
   is_pathname: { /* pathname is now really a pathname */
      var object namestring = true_namestring(pathname,true,false);
     #ifdef UNIX
      if (!file_exists(namestring)) { fehler_file_not_exists(); } /* file must exist */
      file_datetime = filestatus->st_mtime;
     #endif
     #ifdef WIN32_NATIVE
      /* Only a directory search gives us the times. */
      with_sstring_0(namestring,O(pathname_encoding),namestring_asciz, {
        var HANDLE search_handle;
        begin_system_call();
        search_handle = FindFirstFile(namestring_asciz,&filedata);
        if (search_handle==INVALID_HANDLE_VALUE) {
          if (WIN32_ERROR_NOT_FOUND) {
            end_system_call(); fehler_file_not_exists();
          }
          end_system_call(); OS_file_error(STACK_0);
        } else if (!FindClose(search_handle)) {
          end_system_call(); OS_file_error(STACK_0);
        }
        end_system_call();
      });
     #endif
      skipSTACK(1);
    }
  }
  /* date/time no is in the buffer file_datetime.
   convert into Universal-Time-Format: */
 #ifdef UNIX
  VALUES1(convert_time_to_universal(&file_datetime));
 #endif
 #ifdef WIN32_NATIVE
  var FILETIME* pftimepoint = &filedata.ftLastWriteTime;
  if (pftimepoint->dwLowDateTime==0 && pftimepoint->dwHighDateTime==0)
    pftimepoint = &filedata.ftCreationTime;
  VALUES1(convert_time_to_universal(pftimepoint));
 #endif
}

LISPFUNNR(file_author,1)
{ /* (FILE-AUTHOR file), CLTL p. 424 */
  var object pathname = popSTACK(); /* pathname-argument */
  if (builtin_stream_p(pathname)) { /* stream -> treat extra: */
    /* must be file-stream: */
    pathname = as_file_stream(pathname);
    /* streamtype file-stream */
    if (TheStream(pathname)->strmflags & strmflags_open_B) {
      /* open file-stream -> OK */
    } else { /* closed file-stream -> use truename as pathname */
      test_file_stream_named(pathname);
      pathname = TheStream(pathname)->strm_file_truename;
      goto is_pathname;
    }
  } else {
    pathname = merge_defaults(coerce_pathname(pathname)); /* --> pathname */
   is_pathname: { /* pathname is now really a pathname */
      var object namestring = true_namestring(pathname,true,false);
     #if defined(UNIX) || defined(WIN32_NATIVE)
      if (!file_exists(namestring)) { fehler_file_not_exists(); } /* file must exist */
     #endif
      skipSTACK(1);
    }
  }
  /* file exists -> NIL as value */
  VALUES1(NIL);
}

#ifdef UNIX

LISPFUN(execute,seclass_default,1,0,rest,nokey,0,NIL)
{ /* (EXECUTE file arg1 arg2 ...) calls a file with the given arguments. */
  var gcv_object_t* args_pointer = rest_args_pointer STACKop 1;
  {
    var gcv_object_t* argptr = args_pointer; /* Pointer to the arguments */
    { /* check file: */
      var gcv_object_t* file_ = &NEXT(argptr);
      var object namestring = true_namestring(coerce_pathname(*file_),
                                              true,false);
      /* check, if the file exists: */
      if (!file_exists(namestring)) { fehler_file_not_exists(); }
      *file_ = string_to_asciz(namestring,O(pathname_encoding)); /* save */
      skipSTACK(1);
    }
    { /* check the other arguments: */
      var uintC count;
      dotimesC(count,argcount, {
        var gcv_object_t* arg_ = &NEXT(argptr);
        pushSTACK(*arg_); funcall(L(string),1); /* convert next argument into a string */
        *arg_ = string_to_asciz(value1,O(misc_encoding)); /* and convert ASCIZ-string */
      });
    }
  }
  { /* build up argv-Array in stack and copy strings in the stack: */
    var uintL argvdata_length = 0;
    {
      var gcv_object_t* argptr = args_pointer;
      var uintC count;
      dotimespC(count,argcount+1, {
        var object arg = NEXT(argptr); /* next argument, ASCIZ-string */
        argvdata_length += Sbvector_length(arg);
      });
    }
    var DYNAMIC_ARRAY(argv,char*,1+(uintL)argcount+1);
    var DYNAMIC_ARRAY(argvdata,char,argvdata_length);
    {
      var gcv_object_t* argptr = args_pointer;
      var char** argvptr = &argv[0];
      var char* argvdataptr = &argvdata[0];
      var uintC count;
      dotimespC(count,argcount+1, {
        var object arg = NEXT(argptr); /* next argument, ASCIZ-string */
        var char* ptr = TheAsciz(arg);
        var uintL len = Sbvector_length(arg);
        *argvptr++ = argvdataptr; /* fill into argv */
        dotimespL(len,len, { *argvdataptr++ = *ptr++; } ); /* and copy */
      });
      *argvptr = NULL; /* and conclude with nullpointer */
    }
    { /* start a new process: */
      var int child;
      begin_system_call();
      begin_want_sigcld();
      if ((child = vfork()) ==0) {
        /* this program part is executed by the child-process: */
        close_all_fd();
        execv(argv[0],argv); /* call program */
        _exit(-1); /* if this fails, end the child-process */
      }
      /* this program part is executed by the caller: */
      if (child==-1) {
        /* something failed, either on vfork or on execv.
         in both cases errno was set. */
        end_want_sigcld(); OS_error();
      }
      /* wait, until the child-process is finished: */
      var int status = wait2(child);
      /* cf. WAIT(2V) and #include <sys/wait.h> :
         WIFSTOPPED(status)  ==  ((status & 0xFF) == 0177)
         WEXITSTATUS(status)  == ((status & 0xFF00) >> 8) */
      end_want_sigcld();
      end_system_call();
      /* finished. */
      set_args_end_pointer(args_pointer); /* clean up STACK */
      VALUES1(((status & 0xFF) == 0000) /* process ended normally (without signal, without core-dump) ? */
             ? fixnum((status & 0xFF00) >> 8) /* yes -> exit-status as value: */
             : NIL); /* no -> NIL as value */
    }
    FREE_DYNAMIC_ARRAY(argvdata);
    FREE_DYNAMIC_ARRAY(argv);
  }
}

#endif

/* Duplicate an open file handle.
 handle_dup(oldfd)
 Similar to dup(oldfd), with error checking.
 To be called only inside begin/end_system_call(). */
global Handle handle_dup (Handle old_handle) {
  Handle new_handle;
 #if defined(UNIX)
  new_handle = dup(old_handle);
  if (new_handle < 0) { OS_error(); }
 #elif defined(WIN32_NATIVE)
  new_handle = INVALID_HANDLE_VALUE;
  if (!DuplicateHandle(GetCurrentProcess(),old_handle,
                       GetCurrentProcess(),&new_handle,
                       0, true, DUPLICATE_SAME_ACCESS))
    OS_error();
 #else
  NOTREACHED;
 #endif
  return new_handle;
}

/* Duplicate an open file handle.
 handle_dup2(oldfd,newfd)
 Similar to dup2(oldfd,newfd), with error checking. The result may or may not
 be equal to newfd.
 To be called only inside begin/end_system_call(). */
global Handle handle_dup2 (Handle old_handle, Handle new_handle) {
 #if defined(UNIX)
  new_handle = dup2(old_handle,new_handle);
  if (new_handle < 0) { OS_error(); }
 #elif defined(WIN32_NATIVE)
  if (!DuplicateHandle(GetCurrentProcess(),old_handle,
                       GetCurrentProcess(),&new_handle,
                       0, true, DUPLICATE_SAME_ACCESS))
    OS_error();
 #else
  NOTREACHED;
 #endif
  return new_handle;
}

#ifdef HAVE_SHELL

/* (SHELL) calls a shell.
 (SHELL command) calls a shell and lets it execute a command. */

#if defined(WIN32_NATIVE)

/* (SYSTEM::SHELL-NAME) returns the name of the command shell. */
LISPFUNN(shell_name,0) {
  VALUES1(O(command_shell));
}

LISPFUN(shell,seclass_default,0,1,norest,nokey,0,NIL) {
  var object command = popSTACK();
  if (missingp(command))
    command = O(command_shell);
  command = check_string(command);
  var HANDLE prochandle;
  with_string_0(command,O(misc_encoding),command_asciz, {
    /* Start new process. */
    var HANDLE stdinput;
    var HANDLE stdoutput;
    var HANDLE stderror;
    var PROCESS_INFORMATION pinfo;
    begin_system_call();
    stdinput = GetStdHandle(STD_INPUT_HANDLE);
    if (stdinput == INVALID_HANDLE_VALUE) { OS_error(); }
    stdoutput = GetStdHandle(STD_OUTPUT_HANDLE);
    if (stdoutput == INVALID_HANDLE_VALUE) { OS_error(); }
    stderror = GetStdHandle(STD_ERROR_HANDLE);
    if (stderror == INVALID_HANDLE_VALUE) { OS_error(); }
    if (!MyCreateProcess(command_asciz,stdinput,stdoutput,stderror,&pinfo))
      { OS_error(); }
    if (pinfo.hThread && !CloseHandle(pinfo.hThread)) { OS_error(); }
    prochandle = pinfo.hProcess;
  });
  /* Wait until it terminates, get its exit status code. */
  var DWORD exitcode;
  switch (WaitForSingleObject(prochandle,INFINITE)) {
    case WAIT_FAILED:
      OS_error();
    case WAIT_OBJECT_0:
      break;
    default: NOTREACHED;
  }
  if (!GetExitCodeProcess(prochandle,&exitcode)) { OS_error(); }
  if (!CloseHandle(prochandle)) { OS_error(); }
  end_system_call();
  /* utilize return value: =0 (OK) -> T, >0 (not OK) -> NIL : */
  VALUES_IF(exitcode == 0);
}

#else /* UNIX || ... */

LISPFUN(shell,seclass_default,0,1,norest,nokey,0,NIL) {
  var object command = popSTACK();
  if (missingp(command)) {
    /* execute (EXECUTE shell) : */
    pushSTACK(O(user_shell)); /* Shell-Name */
    funcall(L(execute),1);
  } else {
    /* call (EXECUTE shell "-c" command): */
    pushSTACK(O(command_shell)); /* shell name */
    pushSTACK(O(command_shell_option)); /* shell option "-c" */
    pushSTACK(command);
    funcall(L(execute),3);
  }
}

#endif

#endif

/* stringlist_to_asciizlist (stringlist, encoding)
 convert a stringlist to list of asciz strings
 and places it on the stack.
 returns total length of all asciiz strings including zeros
   and listlength (if the pointer is not NULL)
 adds one element to STACK
 can trigger GC */
#if !defined(UNICODE)
#define stringlist_to_asciizlist(s,e,l) stringlist_to_asciizlist_(s,l)
local maygc int stringlist_to_asciizlist_ (object stringlist,uintL *listlength)
#else
local maygc int stringlist_to_asciizlist (object stringlist,
                                          gcv_object_t *encoding_,
                                          uintL *listlength)
#endif
{
  var int length = 0;
  var int listlen = 0;
  pushSTACK(NIL)/*result head*/; pushSTACK(NIL)/*result tail*/;
  pushSTACK(stringlist);
  while (consp(STACK_0/*stringlist tail*/)) {
    var object tmp = allocate_cons();
    if (nullp(STACK_2/*result*/)) STACK_1 = STACK_2 = tmp;
    else { Cdr(STACK_1/*result tail*/) = tmp; STACK_1 = tmp; }
    tmp = check_string(Car(STACK_0));
    tmp = string_to_asciz(tmp,*encoding_);
    length += Sbvector_length(tmp) + 1;
    Car(STACK_1) = tmp;
    STACK_0 = Cdr(STACK_0);
    listlen++;
  }
  if (listlength) *listlength = listlen;
  skipSTACK(2); /* drop stringlist and result tail */
  return length;
}

#ifdef WIN32_NATIVE

/* (SHELL-EXECUTE verb filename parameters defaultdir)
   ShellExecute wrapper
   See ShellExecute description at
   http://msdn.microsoft.com/library/default.asp?url=/library/en-us/shellcc/
     platform/Shell/reference/functions/shellexecute.asp
   verb: usually nil (for default),
         "edit", "explore", "open", "print", "properties"
   filename: filename or url to open
   parameters: list of arguments
   defaultdir: default directory for application (can be nil)
   returns: nil, but can signal an OS error*/
LISPFUN(shell_execute,seclass_default,0,4,norest,nokey,0,NIL) {
  var object verb_arg = STACK_3;
  var object filename_arg = check_string(STACK_2);
  var object parameters_arg = STACK_1;
  var object defaultdir_arg = STACK_0;
  var int verb_len = 0;
  if (nullp(verb_arg)) pushSTACK(S(nil));
  else {
    pushSTACK(string_to_asciz(check_string(verb_arg),O(misc_encoding)));
    verb_len = Sbvector_length(STACK_0);
  }
  var int filename_len = 0;
  pushSTACK(string_to_asciz(check_string(filename_arg),
      O(misc_encoding)));
  filename_len = Sbvector_length(STACK_0);
  var int parameters_len =
    stringlist_to_asciizlist(parameters_arg,&O(misc_encoding),NULL);
  /* list of asciiz strings is in the STACK */
  var DYNAMIC_ARRAY(parameters,char,parameters_len*2);
  var int parameter_pos = 0;
  while (!nullp(STACK_0)) {
    if (parameter_pos > 0) parameters[parameter_pos++] = ' ';
    parameter_pos +=
      shell_quote(parameters+parameter_pos,TheAsciz(Car(STACK_0)));
    ASSERT(parameter_pos < parameters_len*2);
    STACK_0 = Cdr(STACK_0);
  }
  skipSTACK(1);
  var int defaultdir_len = 0;
  if (nullp(defaultdir_arg)) pushSTACK(S(nil));
  else {
    pushSTACK(string_to_asciz(check_string(defaultdir_arg),
                              O(misc_encoding)));
    defaultdir_len = Sbvector_length(STACK_0);
  }
  /* STACK: verb/nil, filename, defaultdir/nil */
  var DYNAMIC_ARRAY(verb,char,1+verb_len);
  var DYNAMIC_ARRAY(filename,char,1+filename_len);
  var DYNAMIC_ARRAY(defaultdir,char,1+defaultdir_len);
  var char *sp, *dp;
  if (!nullp(STACK_2))
    for (sp=TheAsciz(STACK_2),dp=verb;(*dp = *sp);sp++,dp++);
  for (sp=TheAsciz(STACK_1),dp=filename;(*dp = *sp);sp++,dp++);
  if (!nullp(STACK_0))
    for (sp=TheAsciz(STACK_0),dp=defaultdir;(*dp = *sp);sp++,dp++);
  begin_system_call();
  var DWORD result = (DWORD) ShellExecute(NULL,
                                          nullp(STACK_2)?NULL:verb,
                                          filename,
                                          parameters_len?parameters:NULL,
                                          nullp(STACK_0)?NULL:defaultdir,
                                          SW_SHOWNORMAL);
  end_system_call();
  if (result <= 32) OS_error();
  FREE_DYNAMIC_ARRAY(defaultdir);
  FREE_DYNAMIC_ARRAY(filename);
  FREE_DYNAMIC_ARRAY(verb);
  FREE_DYNAMIC_ARRAY(parameters);
  skipSTACK(3+4);
  VALUES1(S(nil));
}

#endif

#if defined(UNIX) || defined (WIN32_NATIVE)

#ifdef UNIX

/* /dev/null handle. */
local Handle nullfile (void) {
  var Handle result;
  begin_system_call();
  result = open("/dev/null",O_RDWR);
  end_system_call();
  return result;
}

/* obtaining a pipe handle */
local void mkpipe (Handle * hin, bool dummy1, Handle * hout, bool dummy2) {
  var int handles[2];
  begin_system_call();
  if (pipe(handles)) OS_error();
  end_system_call();
  *hin = (Handle) handles[0];
  *hout = (Handle) handles[1];
}

#elif defined(WIN32_NATIVE)

/* /dev/null on NT/W95. */
local Handle nullfile (void) {
  var Handle result;
  begin_system_call();
  result = CreateFile("NUL", GENERIC_READ | GENERIC_WRITE,
                      FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                      OPEN_EXISTING, 0, NULL);
  end_system_call();
  return result;
}

/* obtaining pipe handle */
local void mkpipe (Handle * hin, bool dupinp, Handle * hout, bool dupoutp) {
  begin_system_call();
  if (!CreatePipe(hin,hout,NULL,0)) { OS_error(); }
  if (dupinp) {/* make it inheritable */
    var Handle hin1 = handle_dup(*hin);
    if (!CloseHandle(*hin)) { OS_error(); }
    *hin = hin1;
  }
  if (dupoutp) {
    var Handle hout1 = handle_dup(*hout);
    if (!CloseHandle(*hout)) { OS_error(); }
    *hout = hout1;
  }
  end_system_call();
}

#endif

local maygc bool init_launch_streamarg (int istack, bool child_inputp,
                                        Handle stdhandle, Handle * h, Handle * ph,
                                        Handle * hnull, bool * wait_p)
{
  var int handletype = 0;
  *h = INVALID_HANDLE;
  *ph = INVALID_HANDLE;
  if (boundp(STACK_(istack)) && eq(STACK_(istack),S(Kterminal))
      || !boundp(STACK_(istack)))
    *h = handle_dup(stdhandle);
  else if (nullp(STACK_(istack))) {
    if (*hnull == INVALID_HANDLE)
      *hnull = nullfile();
    *h = handle_dup(*hnull);
  } else if (eq(STACK_(istack),S(Kpipe))) {
    if (child_inputp)
      /* make an input pipe for child, ph = parent's handle */
      mkpipe(h,true,ph,false);
    else
      /* make an output pipe for child */
      mkpipe(ph,false,h,true);
    if (*ph == INVALID_HANDLE || *h == INVALID_HANDLE)
      return false;
    *wait_p = false; /* TODO: error when wait_p */
  } else {
    *h = handle_dup(stream_lend_handle(&(STACK_(istack)),
                                       child_inputp,/* child i/o direction is the same as lisp user i/o direction */
                                       &handletype));
    if (handletype != 1)
      return false;
  }
  return (*h != INVALID_HANDLE);
}

local maygc void make_launch_pipe (int istack, bool parent_inputp,
                                   Handle hparent_pipe, int childpid)
{
  if (hparent_pipe != INVALID_HANDLE) {
    pushSTACK(STACK_7);     /* encoding */
    pushSTACK(STACK_(8+1)); /* element-type */
    pushSTACK(STACK_(6+2)); /* buffered */
    (parent_inputp?mkips_from_handles:mkops_from_handles)
      (hparent_pipe,childpid); /* pufff */
    /* stack has been cleaned by callee */
    STACK_(istack+1) = STACK_0;/* replace :pipe with PIPE-x-STREAM */
    skipSTACK(1);
  }
}

/* on cygwin, <sigsegv.h> includes <windows.h> therefore *_PRIORITY_CLASS
   macros are already defined */
#if !defined(NORMAL_PRIORITY_CLASS)
  #define NORMAL_PRIORITY_CLASS 0
  #define HIGH_PRIORITY_CLASS -10
  #define IDLE_PRIORITY_CLASS  10
  #define MY_LOCAL_PRIORITY_CLASSES
#endif
#if defined(UNIX)
  #define CloseHandle(h) (close(h)==0)
#endif
/* paranoidal close */
#define ParaClose(h) if (!CloseHandle(h)) { end_system_call(); OS_error(); }

local maygc sintL interpret_launch_priority (void) {
  var sintL pry = NORMAL_PRIORITY_CLASS;
  if (!boundp(STACK_0)) return NORMAL_PRIORITY_CLASS;
  var object priority_arg = STACK_0;
 restart_priority:
  if (eq(priority_arg,S(Khigh))) return HIGH_PRIORITY_CLASS;
  else if (eq(priority_arg,S(Klow))) return IDLE_PRIORITY_CLASS;
  else if (eq(priority_arg,S(Knormal))) return NORMAL_PRIORITY_CLASS;
  else if (integerp(STACK_0)) return I_to_L(STACK_0);
  pushSTACK(NIL);              /* no PLACE */
  pushSTACK(priority_arg);     /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_priority)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(priority_arg);
  pushSTACK(S(Kpriority));
  pushSTACK(TheSubr(subr_self)->name);
  check_value(type_error,GETTEXT("~S: illegal ~S argument ~S"));
  priority_arg = value1;
  goto restart_priority;
}

/* (LAUNCH executable [:arguments] [:wait] [:input] [:output] [:error]
     [:element-type] [:external-format] [:buffered] [:priority])
 Launches a program.
 :arguments : a list of strings (*MISC-ENCODING* is used)
 :wait - nullp/not nullp - whether to wait for process to finish (default T)
 :input, :output, :error - i/o/e streams for process. basically file-streams,
   pipe streams or terminal-streams.
   see stream_lend_handle() in stream.d for full list of supported streams.
   Can be NIL (/dev/null), :pipe (pipe streams are created) or :terminal.
 :element-type, :external-format, :buffered : parameters for created
   pipe-stream if one or more of :input, :output, :error is :pipe.
 :priority : :HIGH/:LOW/:NORMAL or fixnum
   on UNIX - see nice(2)
   on Windows - see CreateProcess dwCreationFlags parameter.
 returns: value1: if wait exit code, child PID otherwise
          value2: NIL or created pipe-output-stream, input stream for child
          value3: NIL or created pipe-input-stream, output stream for child
          value4: NIL or created pipe-input-stream, error stream for child  */
LISPFUN(launch,seclass_default,1,0,norest,key,9,
        (kw(element_type),kw(external_format),kw(buffered),kw(arguments),
         kw(wait),kw(input),kw(output),kw(error),kw(priority))) {
  STACK_9 = check_string(STACK_9); /* command_arg */
  if (!boundp(STACK_5)) STACK_5 = NIL; /* arguments_arg */
  else STACK_5 = check_list(STACK_5);
  var long priority = interpret_launch_priority();/* from STACK_0 */
  var bool wait_p = !nullp(STACK_4); /* default: do wait! */
  var Handle hnull = INVALID_HANDLE;
  var Handle hinput;
  var Handle hparent_out; /* in case of pipe */
  /* STACK_3 == input_stream_arg */
  if (!init_launch_streamarg(3, true, stdin_handle, &hinput, &hparent_out,
                             &hnull,&wait_p))
    OS_error();
  var Handle houtput, hparent_in;
  /* STACK_2 == output_stream_arg */
  if (!init_launch_streamarg(2, false, stdout_handle, &houtput, &hparent_in,
                             &hnull,&wait_p)) {
    begin_system_call();
    if (hinput != INVALID_HANDLE && hinput != stdin_handle)
      ParaClose(hinput);
    if (hparent_out != INVALID_HANDLE)
      ParaClose(hparent_out);
    end_system_call();
    OS_error();
  }
  var Handle herror, hparent_errin;
  /* STACK_1 == error_stream_arg */
  if (!init_launch_streamarg(1, false, stderr_handle, &herror, &hparent_errin,
                        &hnull,&wait_p)) {
    begin_system_call();
    if (hinput != INVALID_HANDLE && hinput != stdin_handle)
      ParaClose(hinput);
    if (hparent_out != INVALID_HANDLE)
      ParaClose(hparent_out);
    if (houtput != INVALID_HANDLE && houtput != stdout_handle)
      ParaClose(houtput);
    if (hparent_in != INVALID_HANDLE)
      ParaClose(hparent_in);
    end_system_call();
    OS_error();
  }
  if (hnull != INVALID_HANDLE) {
    begin_system_call();
    ParaClose(hnull);
    end_system_call();
  }
  /* convert command and args to one asciiz string list */
  pushSTACK(allocate_cons());
  Car(STACK_0) = STACK_(9+1); /* command_arg */
  Cdr(STACK_0) = STACK_(5+1); /* arguments_arg */
  var uintL arglist_count = 0;
  var uintL argbuf_len = 1 +
    stringlist_to_asciizlist(STACK_0,&O(misc_encoding),&arglist_count);
  /* STACK: cmdlist, ascizcmdlist */
  STACK_1 = STACK_0;
  skipSTACK(1);
  /* STACK: ascizcmdlist */
  var int child_id = 0;
#ifdef UNIX
  var DYNAMIC_ARRAY(argv,char*,1+(uintL)arglist_count+1);
  var DYNAMIC_ARRAY(argvdata,char,argbuf_len);
  var object curcons = STACK_0;
  var char** argvptr = &argv[0];
  var char* argvdataptr = &argvdata[0];
  while (consp(curcons)) {
    var uintL len = Sbvector_length(Car(curcons));
    var char* ptr = TheAsciz(Car(curcons));
    *argvptr++ = argvdataptr; /* fill into argv */
    dotimespL(len,len, { *argvdataptr++ = *ptr++; } ); /* and copy */
    curcons = Cdr(curcons);
  };
  *argvptr = NULL; /* and conclude with null */
  skipSTACK(1);
  /* STACK: -- */
  begin_system_call();
  begin_want_sigcld();
  child_id = vfork();
  if (child_id == 0) {/* What ?! I am the clone ?! */
   /* TODO: close ALL unused opened handles since unclosed handles
      (to previously opened pipes) can prevent childs to end up properly */
   #define CHILD_DUP(from,to)                                            \
      if (dup2(from,to) < 0) {                                           \
          fprintf(stderr,"clisp/child: cannot duplicate %d to %d: %s\n", \
                  from,to,strerror(errno));                              \
          _exit(-1);                                                     \
        }                                                                \
      if (from>2)                                                        \
        close(from)
    CHILD_DUP(hinput,0);
    CHILD_DUP(houtput,1);
    CHILD_DUP(herror,2);
   #undef CHILD_DUP
    /* close child copies of parent's handles */
    if (hparent_out >= 0) close(hparent_out);
    if (hparent_in >= 0) close(hparent_in);
    if (hparent_errin >= 0) close(hparent_errin);
   #ifdef HAVE_NICE
    errno = 0; nice(priority);
    if (errno) {
      fprintf(stderr,"clisp/child: cannot set priority to %d: %s\n",
              priority,strerror(errno));
      _exit(-1);
    }
   #endif
    close_all_fd();
    execvp(*argv,argv);
    fprintf(stderr,"clisp/child: execvp failed: %s\n",strerror(errno));
    _exit(-1);
  } else if (child_id < 0) {
    /* TODO: FIXME: no easy way to be aware of dup2 or exec failures */
    end_want_sigcld();
    end_system_call();
    OS_error();
  }
  var int exit_code = 0;
  if (wait_p) {
    var int status = wait2(child_id);
    exit_code = WEXITSTATUS(status);
  }
  end_want_sigcld();
  /* close our copies of child's handles */
  if (hinput!=stdin_handle) ParaClose(hinput);
  if (houtput!=stdout_handle) ParaClose(houtput);
  if (herror!=stderr_handle) ParaClose(herror);
  end_system_call();
  FREE_DYNAMIC_ARRAY(argv);
  FREE_DYNAMIC_ARRAY(argvdata);
#else /* WIN32_NATIVE */
  var DYNAMIC_ARRAY(command_data,char,argbuf_len*2);
  /* argbuf_len is multiplied by 2 for quoting sake */
  var int command_pos = 0;
  while (!nullp(STACK_0)) {
    if (command_pos > 0) command_data[command_pos++] = ' ';
    command_pos += shell_quote(command_data+command_pos,
                               TheAsciz(Car(STACK_0)));
    ASSERT(command_pos < argbuf_len*2);
    STACK_0 = Cdr(STACK_0);
  }
  skipSTACK(1);
  /* STACK: -- */

  /* Start new process. */
  var HANDLE prochandle;
  var PROCESS_INFORMATION pinfo;
  var STARTUPINFO sinfo;
  sinfo.cb = sizeof(STARTUPINFO);
  sinfo.lpReserved = NULL;
  sinfo.lpDesktop = NULL;
  sinfo.lpTitle = NULL;
  sinfo.cbReserved2 = 0;
  sinfo.lpReserved2 = NULL;
  sinfo.dwFlags = STARTF_USESTDHANDLES;
  sinfo.hStdInput = hinput;
  sinfo.hStdOutput = houtput;
  sinfo.hStdError = herror;
  begin_system_call();
  if (!CreateProcess(NULL, command_data, NULL, NULL, true,
                     (DWORD)priority & 0x1E0,
                     NULL, NULL, &sinfo, &pinfo))
    { end_system_call(); OS_error(); }
  if (pinfo.hThread) /* zero for 16 bit programs in NT */
    ParaClose(pinfo.hThread);
  prochandle = pinfo.hProcess;
  child_id = pinfo.dwProcessId;
  FREE_DYNAMIC_ARRAY(command_data);
  var DWORD exit_code = 0;
  if (wait_p) {
    /* Wait until it terminates, get its exit status code. */
    switch (WaitForSingleObject(prochandle,INFINITE)) {
      case WAIT_FAILED:
        end_system_call(); OS_error();
      case WAIT_OBJECT_0:
        break;
      default: NOTREACHED;
    }
    if (!GetExitCodeProcess(prochandle,(DWORD*)&exit_code))
      { end_system_call(); OS_error(); }
  }
  /* we can safely close handle of a running process - it doesn't
     lead to process termination */
  ParaClose(prochandle);
  /* close our copies of child's handles */
  if (hinput!=stdin_handle) ParaClose(hinput);
  if (houtput!=stdout_handle) ParaClose(houtput);
  if (herror!=stderr_handle) ParaClose(herror);
  end_system_call();
#endif
  /* make pipe-streams */
  /* child's input stream, pipe-output from our side */
  make_launch_pipe (3, false, hparent_out, child_id);
  /* child's output stream, pipe-input from our side
     double analysis of buffered, eltype,encoding
     drawback: slow; advantage: simple iface with stream.d */
  make_launch_pipe (2, true, hparent_in, child_id);
  /* child's error stream, pipe-input from our side */
  make_launch_pipe (1, true, hparent_errin, child_id);

  value1 = wait_p ? fixnum(exit_code) : fixnum(child_id);
  value2 = (hparent_out   != INVALID_HANDLE) ? (object)STACK_3 : NIL; /* INPUT */
  value3 = (hparent_in    != INVALID_HANDLE) ? (object)STACK_2 : NIL; /* OUTPUT */
  value4 = (hparent_errin != INVALID_HANDLE) ? (object)STACK_1 : NIL; /* ERROR */
  mv_count = 4;

  skipSTACK(10);
}

#if defined(MY_LOCAL_PRIORITY_CLASSES)
  #undef MY_LOCAL_PRIORITY_CLASSES
  #undef NORMAL_PRIORITY_CLASS
  #undef HIGH_PRIORITY_CLASS
  #undef IDLE_PRIORITY_CLASS
#endif
#if defined(UNIX)
  #undef CloseHandle
#endif

#undef ParaClose

#endif

/* (SAVEMEM pathname exec-p) stores the memory image at pathname. */
LISPFUNN(savemem,2) {
  var bool exec_p = !nullp(popSTACK());
  /* execute (OPEN pathname :direction :output) :
   pathname as 1. argument */
  pushSTACK(S(Kdirection)); /* :DIRECTION as 2. Argument */
  pushSTACK(S(Koutput)); /* :OUTPUT as 3. Argument */
 #ifdef UNIX
  /* On Unix with mmap() existing .mem-Files may not be simply
   overwritten, because running Lisp-processes would crash due to this.
   So therefore :if-exists :rename-and-delete. */
  #if defined(UNIX_LINUX) && defined(SINGLEMAP_MEMORY)
  /* Under Linux 1.3.20, when the mem file to be saved is on an NFS volume
   and has the same filename as the mem file we started with, the GC
   done by savemem (once the new mem file has been created and still has
   size 0) will crash. Looks like a bug in the Linux NFS client, which
   causes random pages to be mapped in instead of pages from the renamed
   old mem file. Workaround: Do a full GC, forcing all the old mem file's
   contents into memory immediately. */
  gar_col();
  #endif
  pushSTACK(S(Kif_exists)); /* :IF-EXISTS as 4. Argument */
  pushSTACK(S(Krename_and_delete)); /* :RENAME-AND-DELETE as 5. Argument */
  funcall(L(open),5);
 #else
  funcall(L(open),3);
 #endif
  /* write memory image into the file:
   (the stream has to be closed by function savemem(),
   also in case of an error.) */
  savemem(value1,exec_p);
  VALUES1(T);
}

#ifdef DYNAMIC_MODULES

/* (SYSTEM::DYNLOAD-MODULES pathname stringlist)
 loads a shared library, containing a number of modules. */
LISPFUNN(dynload_modules,2) {
  /* convert pathname into string */
  STACK_1 = coerce_pathname(STACK_1);
  check_no_wildcards(STACK_1);
  STACK_1 = whole_namestring(use_default_dir(STACK_1));
  /* check strings and store in the stack: */
  var uintL stringcount = llength(STACK_0);
  var gcv_object_t* arg_ = &STACK_0;
  {
    var uintL count;
    dotimesL(count,stringcount, {
      Car(*arg_) = check_string(Car(*arg_));
      pushSTACK(string_to_asciz(Car(*arg_),Symbol_value(S(ascii))));
      *arg_ = Cdr(*arg_);
    });
    endp(*arg_); /* test for proper list */
  }
  {
    var const char * libpath = TheAsciz(string_to_asciz(*(arg_ STACKop 1),O(pathname_encoding)));
    var DYNAMIC_ARRAY(modnames,const char *,stringcount);
    if (stringcount > 0) {
      var uintL count;
      var gcv_object_t* ptr1 = STACK STACKop stringcount;
      var const char * * ptr2 = modnames;
      dotimespL(count,stringcount, { *ptr2++ = TheAsciz(NEXT(ptr1)); });
    }
    dynload_modules(libpath,stringcount,modnames);
    FREE_DYNAMIC_ARRAY(modnames);
  }
  skipSTACK(stringcount+1);
  VALUES1(popSTACK()); /* Library-Name as value */
}

#endif

/* =================================================================== */

#include "execname.c"
LISPFUNN(program_name,0)
{ /* (SYS::PROGRAM-NAME) returns the executable's name. */
  VALUES1(asciz_to_string(executable_name,O(pathname_encoding)));
}

LISPFUNN(lib_directory,0)
{ /* (SYS::LIB-DIRECTORY) returns CLISP's private library directory
 (called $(lisplibdir) in the Makefile). */
  if (!nullp(O(lib_dir))) {
    VALUES1(O(lib_dir));
  } else {
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: library directory is not known, use a command line option to specify it"));
  }
}

LISPFUNN(set_lib_directory,1)
{ /* (SYS::SET-LIB-DIRECTORY path) sets the CLISP's private library directory
  (called $(lisplibdir) in the Makefile) */
  var object path = popSTACK();
  if (stringp(path)) path = ensure_last_slash(path);
  VALUES1(O(lib_dir) = coerce_xpathname(path));
}

/* ===================================================================== */

#ifdef DEBUG_TRANSLATE_PATHNAME
#undef DEBUG_TRANSLATE_PATHNAME
#undef DOUT
#endif
