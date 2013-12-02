/*
 * CLISP: directory key: win32 registry, LDAP, Gnome-config
 * Copyright (C) 2000-2005 by Sam Steingold
 */

/* have to undefing UNICODE _here_ because clisp.h will #include <windows.h> */
#undef UNICODE
#include "clisp.h"

#include "config.h"

#if defined(__CYGWIN__)
# define UNIX_CYGWIN32
#endif

#define WIN32_LEAN_AND_MEAN  /* avoid including junk */
#if defined(UNIX_CYGWIN32) || defined(__MINGW32__)
/* `unused' is used in function declarations. */
# undef unused
# define ULONGLONG OS_ULONGLONG
# define ULONG OS_ULONG
# include <windows.h>
# undef ULONG
# undef ULONGLONG
# define unused (void)
#else
# undef unused
# include <windows.h>
# define unused
#endif

#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
# include <winreg.h>
# if !defined(__MINGW32__) && !defined(UNIX_CYGWIN32)
#  include <winldap.h>
/* #define ACCESS_LDAP *//* not yet! */
# endif
# define WIN32_REGISTRY 1
#elif defined(HAVE_LDAP_H)
# if defined(HAVE_LBER_H)
#  include <lber.h>        /* Solaris/cc requires this */
# endif
# include <ldap.h>
# define ACCESS_LDAP
#endif
#if defined(HAVE_GNOME_H)
# include <gnome.h>
#endif

/* as per RFC1777 and RFC2255 */
#ifndef LDAP_PORT
#define LDAP_PORT 389
#endif

/* :SCOPE */
typedef enum {
  SCOPE_SELF, SCOPE_KIDS, SCOPE_TREE
} scope_t;

#if defined(WIN32_REGISTRY)
# define SYSCALL_WIN32(call)     do {                                  \
    uintL status;                                                      \
    begin_system_call();                                               \
    status = call;                                                     \
    if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); } \
    end_system_call();                                                 \
  } while(0)
#endif

#if defined(ACCESS_LDAP)
#if defined(WIN32_NATIVE)
# define SYSCALL_LDAP(call) SYSCALL_WIN32(call)
#else
# define SYSCALL_LDAP(call)      do {                                  \
    uintL status;                                                      \
    begin_system_call();                                               \
    status = call;                                                     \
    if (status != LDAP_SUCCESS) { errno=status; OS_error(); }          \
    end_system_call();                                                 \
  } while(0)
#endif
#endif

/* #define DEBUG */
#if defined(DEBUG)
# include <stdio.h>
extern object nobject_out (FILE* stream, object obj);
# define XOUT(obj,label)                                                \
  (printf("[%s:%d] %s: %s:\n",__FILE__,__LINE__,STRING(obj),label),     \
   obj=nobject_out(stdout,obj), printf("\n"))
#else
# undef OBJECT_OUT
# define OBJECT_OUT(o,l)
# define XOUT(o,l)
#endif

DEFMODULE(dirkey,"LDAP")

enum { /* DIR-KEY slots */
  DK_TYPE=0,
  DK_DIR=1,
  DK_PATH=2,
  DK_OPEN=3,
  DK_HANDLE=4
};
#define SLOT_HANDLE(s)  (TheFpointer(s[DK_HANDLE])->fp_pointer)

/* check whether the OBJ is a DIR-KEY */
static gcv_object_t* dir_key_slots (object obj) {
  if (!typep_classname(obj,`LDAP::DIR-KEY`)) return NULL;
  return TheStructure(obj)->recdata+1; /* FIXME for derived structs! */
}

/* check that the OBJ is a DIR-KEY
 can trigger GC */
static object test_dir_key (object obj, bool check_open) {
 start: {
    gcv_object_t *slots = dir_key_slots(obj);
    if (NULL == slots) {
      pushSTACK(NIL); /* no PLACE */
      pushSTACK(obj); /* TYPE-ERROR slot DATUM */
      pushSTACK(`LDAP::DIR-KEY`); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(`LDAP::DIR-KEY`); pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
      obj = value1; goto start;
    }
    if (check_open && nullp(slots[DK_OPEN])) {
      pushSTACK(obj);
      pushSTACK(TheSubr(subr_self)->name);
      check_value(error,GETTEXT("~S on ~S is illegal"));
      obj = value1; goto start;
    }
    XOUT(obj,"directory key");
#  if SAFETY>=1
    ASSERT(symbolp(slots[DK_TYPE]));
    ASSERT(symbolp(slots[DK_DIR]));
    ASSERT(stringp(slots[DK_PATH])) ;
    ASSERT(eq(slots[DK_OPEN],NIL) || eq(slots[DK_OPEN],T));
    ASSERT(fpointerp(slots[DK_HANDLE]));
#  endif
  }
  return obj;
}

/* convert an array of char to a (VECTOR (UNSIGNED-BYTE 8))
 can trigger GC */
static object reg_val_to_vector (uintL size, const char* buffer) {
  object vec = allocate_bit_vector(Atype_8Bit,size);
  uintB* dat = TheSbvector(vec)->data;
  while(size--) *dat++ = *buffer++;
  return vec;
}

#if defined(WIN32_REGISTRY)
/* convert a registry value [type;size;buffer] to the appropriate Lisp object
 can trigger GC */
static object registry_value_to_object (DWORD type, DWORD size,
                                        const char* buffer) {
  switch (type) {
    case REG_NONE: return NIL;
    case REG_SZ:
    case REG_EXPAND_SZ: /* should we actually expand the env vars?! */
      return asciz_to_string(buffer,GLO(misc_encoding));
    case REG_DWORD_LITTLE_ENDIAN:
      if (REG_DWORD_LITTLE_ENDIAN != REG_DWORD)
        return UL_to_I(((unsigned char)buffer[3] << 24)+
                       ((unsigned char)buffer[2] << 16)+
                       ((unsigned char)buffer[1] <<  8)+
                       ((unsigned char)buffer[0]));
      else
        return UL_to_I(*(DWORD*)buffer);
    case REG_DWORD_BIG_ENDIAN:
      if (REG_DWORD_BIG_ENDIAN != REG_DWORD)
        return UL_to_I(((unsigned char)buffer[0] << 24)+
                       ((unsigned char)buffer[1] << 16)+
                       ((unsigned char)buffer[2] <<  8)+
                       ((unsigned char)buffer[3]));
      else
        return UL_to_I(*(DWORD*)buffer);
    case REG_MULTI_SZ: { /* multiple strings, separated by '\0'. */
      uintL ii;
      pushSTACK(NIL);
      for (ii = 0; ii < size; ) {
        uintL jj;
        for (jj = ii; (jj < size) && buffer[jj]; jj++);
        if (jj < size-1) { /* avoid last empty string */
          object new_cons = allocate_cons();
          Cdr(new_cons) = STACK_0;
          STACK_0 = new_cons;
          Car(STACK_0) = n_char_to_string(buffer+ii,jj-ii,GLO(misc_encoding));
        }
        ii = jj + 1;
      }
      return nreverse(popSTACK());
    }
    /* case REG_RESOURCE_LIST: */
    /* case REG_FULL_RESOURCE_DESCRIPTOR: */
    /* case REG_RESOURCE_REQUIREMENTS_LIST: */
    /* case REG_LINK: */
    /* case REG_BINARY: */
    default:
      return reg_val_to_vector(size,buffer);
  }
}

#endif

DEFUN(LDAP:DIR-KEY-CLOSE,dk)
{ /* close the supplied DIR-KEY */
  object dkey = popSTACK();
# if defined(WIN32_REGISTRY)
  if (fpointerp(dkey)) { /* an HKEY in an iterator_state */
    if (TheFpointer(dkey)->fp_pointer)
      SYSCALL_WIN32(RegCloseKey((HKEY)(TheFpointer(dkey)->fp_pointer)));
  } else
# endif
  {
    gcv_object_t *slots = dir_key_slots(dkey = test_dir_key(dkey,false));
    if (!nullp(slots[DK_OPEN])) {
#    if defined(WIN32_REGISTRY)
      if (eq(slots[DK_TYPE],`:WIN32`)) {
        SYSCALL_WIN32(RegCloseKey((HKEY)SLOT_HANDLE(slots)));
      } else
#    endif
#    if defined(ACCESS_LDAP)
      if (eq(slots[DK_TYPE],`:LDAP`)) {
        SYSCALL_LDAP(ldap_unbind((struct ldap*)SLOT_HANDLE(slots)));
      } else
#    endif
#    if defined(GNOME)
      if (eq(slots[DK_TYPE],`:GNOME`)) {
        with_string_0(slots[DK_PATH],GLO(pathname_encoding),pathz,{
          gnome_config_drop_file(pathz);
          gnome_config_sync_file(pathz);
        });
      } else
#    endif
        { /* noop */ ; }
      slots[DK_OPEN] = NIL;
    }
  }
  VALUES1(NIL);
}

#if defined(WIN32_REGISTRY)
struct root {
  const char *name;
  unsigned int namelen;
  HKEY hkey;
};
#define MKKEY(key)  { #key, sizeof(#key)-1, key }
static struct root roots[] = {
# if defined(HKEY_CLASSES_ROOT)
  MKKEY(HKEY_CLASSES_ROOT),
# endif
# if defined(HKEY_CURRENT_USER)
  MKKEY(HKEY_CURRENT_USER),
# endif
# if defined(HKEY_LOCAL_MACHINE)
  MKKEY(HKEY_LOCAL_MACHINE),
# endif
# if defined(HKEY_USERS)
  MKKEY(HKEY_USERS),
# endif
# if defined(HKEY_PERFORMANCE_DATA)
  MKKEY(HKEY_PERFORMANCE_DATA),
# endif
# if defined(HKEY_CURRENT_CONFIG)
  MKKEY(HKEY_CURRENT_CONFIG),
# endif
# if defined(HKEY_DYN_DATA)
  MKKEY(HKEY_DYN_DATA),
# endif
};
#undef MKKEY

static HKEY parse_registry_path (const char* path, const char** base_ret) {
  unsigned int ii;
  unsigned int len;
  HKEY hkey = NULL;
  char* base;
  char* host;
  /* Path syntax HOSTNAME\\... denotes a remote registry access. */
  host = NULL;
  begin_system_call();
  base = strstr(path,"\\\\");
  if (base == NULL) base = strstr(path,"//");
  if (base != NULL) {
    len = base-path;
    host = (char*)alloca(len+1);
    strncpy(host,path,len);
    host[len] = 0;
    path = base + 2;
  }
  /* Now look for the topmost directory component. */
  base = strchr(path,'\\');
  if (base == NULL) base = strchr(path,'/');
  if (base == NULL)
    len = strlen(path);
  else {
    len = base-path;
    base++;
  }
  /* Return the remainder. */
  *base_ret = base;
  /* Get the key for the topmost directory component. */
  for (ii = 0; ii < sizeof(roots)/sizeof(*roots); ii++)
    if (roots[ii].namelen == len && memcmp(roots[ii].name,path,len) == 0) {
      hkey = roots[ii].hkey;
      break;
    }
  end_system_call();
  if (hkey == NULL) { SetLastError(ERROR_PATH_NOT_FOUND); OS_error(); }
  if (host == NULL)
    return hkey;
  else {
    HKEY res;
    SYSCALL_WIN32(RegConnectRegistry(host,hkey,&res));
    return res;
  }
}

static void open_reg_key (HKEY hkey, char* path, direction_t dir,
                         if_does_not_exist_t if_not_exists, HKEY* p_hkey) {
  REGSAM perms;
  switch (dir) {
    case DIRECTION_OUTPUT: perms = KEY_WRITE; break;
    case DIRECTION_IO: perms = KEY_ALL_ACCESS; break;
    default: perms = KEY_READ; break;
  }
 {DWORD status;
  begin_system_call();
  status = RegOpenKeyEx(hkey,path,0,perms,p_hkey);
  if (status != ERROR_SUCCESS) {
    if ((if_not_exists == IF_DOES_NOT_EXIST_UNBOUND /*ignore*/)
        || ((status == ERROR_FILE_NOT_FOUND)
            && (if_not_exists != IF_DOES_NOT_EXIST_ERROR))) {
      switch (if_not_exists) {
        case IF_DOES_NOT_EXIST_NIL: case IF_DOES_NOT_EXIST_UNBOUND:
          *p_hkey = NULL; break;
        case IF_DOES_NOT_EXIST_CREATE:
          status = RegCreateKey(hkey,path,p_hkey);
          if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); }
          break;
        default: NOTREACHED;
      }
    } else { SetLastError(status); OS_error(); }
  }
  end_system_call();
}}
#endif

#if defined(ACCESS_LDAP)
nonreturning_function(static, fehler_ldap,
                      (object dk, object path, char* errmsg)) {
  end_system_call();
  pushSTACK(NIL); pushSTACK(path); pushSTACK(dk);
  pushSTACK(TheSubr(subr_self)->name);
  STACK_3 = CLSTEXT(errmsg);
  fehler(error,"~S(~S ~S): ~S");
}
#define LDAP_ERR2STR(d,p,status) fehler_ldap(d,p,ldap_err2string(status))
#define LDAP_RES2STR(d,p,ld,res) LDAP_ERR2STR(d,p,ldap_result2error(ld,res,1))
#endif

/* return the DIR-KEY object corresponding to the PATH under KEY
 PATH should be a string, like "HKEY_LOCAL_MACHINE\\Something"
 KEY can be either a DIR-KEY or a (MEMBER :win32 :gnome :ldap),
 in which case PATH must be absolute.
 :direction and :if-does-not-exist has the same meaning as for OPEN. */
DEFUN(LDAP::DIR-KEY-OPEN, key path &key DIRECTION IF-DOES-NOT-EXIST) {
  object path = check_string(STACK_2);
  object root = STACK_3;
  void* ret_handle;
  uintL status;
  direction_t direction = check_direction(STACK_1);
  if_does_not_exist_t if_not_exists = check_if_does_not_exist(STACK_0);
  gcv_object_t *slots = dir_key_slots(root);
  object type = (slots==NULL ? root : (object)slots[DK_TYPE]);
  if (if_not_exists == IF_DOES_NOT_EXIST_UNBOUND)
    if_not_exists = (direction == DIRECTION_INPUT ?
                     IF_DOES_NOT_EXIST_ERROR : IF_DOES_NOT_EXIST_CREATE);
  switch (direction) { /* reduce to :IO, :INPUT & :OUTPUT */
    case DIRECTION_OUTPUT: STACK_1 = S(Koutput); break;
    case DIRECTION_IO: STACK_1 = S(Kio); break;
    default: STACK_1 = S(Kinput); break;
  }
  /* create the key handle */
# if defined(WIN32_REGISTRY)
  if (eq(type,`:WIN32`)) {
    if (NULL != slots) {
      root = test_dir_key(root,true);
      slots = dir_key_slots(root);
      with_string_0(path,GLO(misc_encoding),pathz,{
        open_reg_key((HKEY)SLOT_HANDLE(slots),pathz,
                     direction,if_not_exists,(HKEY*)&ret_handle);
      });
    } else {
      with_string_0(path,GLO(misc_encoding),pathz,{
        char* base;
        HKEY hkey = parse_registry_path(pathz,(const char**)&base);
        open_reg_key(hkey,base,direction,if_not_exists,(HKEY*)&ret_handle);
      });
    }
  } else
# endif
# if defined(ACCESS_LDAP)
  if (eq(type,`:LDAP`)) {
    if (NULL != slots) {
      root = test_dir_key(root,true);
      begin_system_call();
      with_string_0(path,GLO(misc_encoding),pathz,{
        status = ldap_simple_bind_s((LDAP*)ret_handle,pathz,NULL);
      });
      if (status != LDAP_SUCCESS)
        LDAP_ERR2STR(root,path,status);
      end_system_call();
    } else { /* :LDAP */
      struct ldap_url_desc* ldap_url = NULL;
      begin_system_call();
      with_string_0(path,GLO(misc_encoding),pathz,
                    { status = ldap_url_parse(pathz,&ldap_url); });
      if (status != 0) {
        end_system_call();
        pushSTACK(path);
        pushSTACK(TheSubr(subr_self)->name);
        fehler(error,GETTEXT("~S: ~S is not an LDAP URL"));
      }
      ret_handle = (void*)ldap_open(ldap_url->lud_host,ldap_url->lud_port);
      if (ret_handle == NULL) OS_error();
      if ((status = ldap_simple_bind_s((LDAP*)ret_handle,ldap_url->lud_dn,
                                       NULL)) != LDAP_SUCCESS) {
        pushSTACK(path);
       {object dn = asciz_to_string(ldap_url->lud_dn,GLO(misc_encoding));
        path = popSTACK();
        LDAP_ERR2STR(path,dn,status);
      }}
      ldap_free_urldesc(ldap_url);
      end_system_call();
    }
  } else
# endif
# if defined(GNOME)
  if (eq(type,`:GNOME`)) {
    /* do nothing - gnome-conf is stateless */
  } else
# endif
  { /* invalid type */
    pushSTACK(type); /* TYPE-ERROR slot DATUM */
    pushSTACK(`(MEMBER :WIN32 :LDAP :GNOME)`);/*TYPE-ERROR slot EXPECTED-TYPE*/
    pushSTACK(`LDAP::DIR-KEY`);
    pushSTACK(type);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,GETTEXT("~S: ~S is not a ~S"));
  }
  /* create the DIR-KEY */
  pushSTACK(type);
  pushSTACK(STACK_(1+1)); /* direction */
  if (NULL != slots) {
    pushSTACK(slots[DK_PATH]);
#  if defined(WIN32_NATIVE)
    if (eq(slots[DK_TYPE],`:WIN32`))
      pushSTACK(`"\\"`);
    else
#  endif
      pushSTACK(`"/"`);
    pushSTACK(path);
   {object totalpath = string_concat(3);
    pushSTACK(totalpath);
  }} else
    pushSTACK(path);
  pushSTACK(T);
  pushSTACK(allocate_fpointer(ret_handle));
  /* make the DIR-KEY: stack: dir type path open-p handle*/
  funcall(`LDAP::MKDK`,5);
  pushSTACK(value1);
  /* Call (FINALIZE dir-key #'dir-key-close). */
  pushSTACK(STACK_0); /* dkey */
  pushSTACK(`LDAP::DIR-KEY-CLOSE`);
  funcall(L(finalize),2);
  /* Done. */
  VALUES1(STACK_0);
  skipSTACK(5);
}

/* The common code in DIR-KEY-SUBKEYS & DIR-KEY-ATTRIBUTES */
#define MAKE_OBJECT_LIST(COUNT_EXPR,GET_NEXT_OBJ_EXPR)                  \
  object dkey = test_dir_key(popSTACK(),true);                          \
  LONG status;                                                          \
  DWORD n_obj;                                                          \
  DWORD maxlen;                                                         \
  HKEY hkey = (HKEY)SLOT_HANDLE(dir_key_slots(dkey));                   \
  begin_system_call();                                                  \
  status = (COUNT_EXPR);                                                \
  if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); }    \
  end_system_call();                                                    \
  if (n_obj > 0) {                                                      \
    unsigned int ii;                                                    \
    char* buf = (char*)alloca(maxlen+1); /* one extra char for '\0' */  \
    maxlen++; pushSTACK(NIL);                                           \
    for (ii = 0; ii < n_obj; ii++) {                                    \
      DWORD len = maxlen;                                               \
      DWORD status;                                                     \
      begin_system_call();                                              \
      status = (GET_NEXT_OBJ_EXPR);                                     \
      if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); } \
      end_system_call();                                                \
      { object new_cons = allocate_cons();                              \
        Cdr(new_cons) = STACK_0;                                        \
        STACK_0 = new_cons; }                                           \
      { object string = asciz_to_string(buf,GLO(misc_encoding));        \
        Car(STACK_0) = string; }                                        \
    }                                                                   \
    value1 = nreverse(popSTACK());                                      \
  } else                                                                \
    value1 = NIL;                                                       \
  mv_count = 1

DEFUN(LDAP::DIR-KEY-SUBKEYS, key)
{ /* return the list of subkey names of the given KEY */
# if defined(WIN32_REGISTRY)
  if (eq(STACK_0,`:WIN32`)) { /* top-level keys */
    int ii, len = sizeof(roots)/sizeof(*roots);
    skipSTACK(1);
    for (ii=0; ii<len; ii++)
      pushSTACK(asciz_to_string(roots[ii].name,GLO(misc_encoding)));
    VALUES1(listof(len));
  } else
# endif
  {
    MAKE_OBJECT_LIST(RegQueryInfoKey(hkey,NULL,NULL,NULL,&n_obj,&maxlen,
                                     NULL,NULL,NULL,NULL,NULL,NULL),
                     RegEnumKey(hkey,ii,buf,len));
  }
}

DEFUN(LDAP:DIR-KEY-ATTRIBUTES,key)
{ /* return the list of attribute names of the given KEY */
  MAKE_OBJECT_LIST(RegQueryInfoKey(hkey,NULL,NULL,NULL,NULL,NULL,NULL,
                                   &n_obj,&maxlen,NULL,NULL,NULL),
                   RegEnumValue(hkey,ii,buf,&len,NULL,NULL,NULL,NULL));
}
#undef MAKE_OBJECT_LIST

/* iteration state access */
/* STACK = (NODE1 NODE2 ...) */
#define ITST_DKEY(state)   TheSvector(state)->data[0]
#define ITST_PATH(state)   TheSvector(state)->data[1]
#define ITST_SCOPE(state)  TheSvector(state)->data[2]
#define ITST_STACK(state)  TheSvector(state)->data[3]
#define ITST_NODE(state)   Car(ITST_STACK(state))
/* node to process */
#define NODE_HANDLE(node)  TheSvector(node)->data[0]
#define NODE_KEY(node)     TheSvector(node)->data[1]
#define NODE_KEY_S(node)   TheSvector(node)->data[2]
#define NODE_ATT(node)     TheSvector(node)->data[3]
#define NODE_ATT_S(node)   TheSvector(node)->data[4]
#define NODE_DAT_S(node)   TheSvector(node)->data[5]
#define NODE_NAME(node)    TheSvector(node)->data[6]

/* return the current path - concatenate the NAMEs of the NODEs of the STACK
 can trigger GC */
static object itst_current (object state) {
  object stack = ITST_STACK(state);
  uintL depth = 0;
  for (stack = nreverse(stack); !nullp(stack); stack = Cdr(stack)) {
    object name = NODE_NAME(Car(stack));
    if (Sstring_length(name) > 0) {
      if (depth) {
        depth++;
#      if defined(WIN32_NATIVE)
        pushSTACK(`"\\"`);
#      else
        pushSTACK(`"/"`);
#      endif
      }
      depth++;
      pushSTACK(name);
    }
  }
  ITST_STACK(state) = nreverse(ITST_STACK(state));
  return string_concat(depth);
}

static scope_t parse_scope (object scope) {
  if (eq(scope,`:SELF`))  return SCOPE_SELF;
  if (eq(scope,`:LEVEL`)) return SCOPE_KIDS;
  if (eq(scope,`:TREE`))  return SCOPE_TREE;
  pushSTACK(scope);         /* TYPE-ERROR slot DATUM */
  pushSTACK(`(MEMBER :SELF :LEVEL :TREE)`); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(`:SCOPE`);
  pushSTACK(scope);
  pushSTACK(TheSubr(subr_self)->name);
  fehler(type_error,GETTEXT("~S: ~S is not a ~S"));
}

/* return a simple vector with the iteration state
 < dkey path scope
 > #(dkey init-path scope (node ...)) */
DEFUN(LDAP::DKEY-SEARCH-ITERATOR, key path scope) {
  STACK_1 = check_string(STACK_1);
  parse_scope(STACK_0);
  pushSTACK(allocate_vector(4)); /* return value */
  ITST_DKEY(STACK_0)  = test_dir_key(STACK_3,true);
  ITST_PATH(STACK_0)  = STACK_2;
  ITST_SCOPE(STACK_0) = STACK_1;
  ITST_STACK(STACK_0) = T;
  VALUES1(STACK_0);
  skipSTACK(4);
}

/* open HANDLE to point to DKEY\\PATH
 compute KEY_S, ATT_S and DAT_S
 return the full current path (itst_current)
 and T/NIL indicating whether OPEN was successful
 can trigger GC */
static void init_iteration_node (object state, object subkey,
                                 object *new_path, object *failed_p) {
  XOUT(state,"init_iteration_node");
  pushSTACK(state);
  pushSTACK(subkey);
  ITST_PATH(STACK_1/*state*/) = check_string(ITST_PATH(STACK_1));
  pushSTACK(allocate_cons());         /* stack */
  pushSTACK(allocate_vector(7));      /* node */
  pushSTACK(allocate_fpointer(NULL)); /* handle */
  /* (push node stack) */
  if (eq(T,ITST_STACK(STACK_4/*state*/)))
    ITST_STACK(STACK_4/*state*/) = NIL;
  Car(STACK_2/*stack*/) = STACK_1/*node*/;
  Cdr(STACK_2/*stack*/) = ITST_STACK(STACK_4/*state*/);
  ITST_STACK(STACK_4/*state*/) = STACK_2/*stack*/;
  /* init node */
  NODE_KEY(STACK_1/*node*/) = fixnum(0);
  NODE_ATT(STACK_1/*node*/) = fixnum(0);
  NODE_NAME(STACK_1/*node*/) =
    (nullp(STACK_3/*subkey*/)
     ? ITST_PATH(STACK_4/*state*/) : STACK_3/*subkey*/);
  /* init handle */
  NODE_HANDLE(STACK_1/*node*/) = STACK_0/*handle*/;
  pushSTACK(STACK_0/*handle*/); pushSTACK(`LDAP::DIR-KEY-CLOSE`);
  funcall(L(finalize),2); /* (FINALIZE handle #'dir-key-close) */
  *new_path = itst_current(STACK_4/*state*/);
  test_dir_key(ITST_DKEY(STACK_4),true);
 {object dk = test_dir_key(ITST_DKEY(STACK_4/*state*/),true);
  gcv_object_t *slots = dir_key_slots(dk);
  FOREIGN *fp = &(TheFpointer(STACK_0/*handle*/)->fp_pointer);
  with_string_0(*new_path,GLO(misc_encoding),pathz,{
    open_reg_key((HKEY)SLOT_HANDLE(slots),pathz,check_direction(slots[DK_DIR]),
                 IF_DOES_NOT_EXIST_UNBOUND/*ignore*/,(HKEY*)fp);
  });
  if (*fp) {
    DWORD k_size, a_size, d_size;
    SYSCALL_WIN32(RegQueryInfoKey((HKEY)*fp,NULL,NULL,NULL,NULL,&k_size,
                                  NULL,NULL,&a_size,&d_size,NULL,NULL));
    NODE_KEY_S(STACK_1) = fixnum(k_size+1); /* node */
    NODE_ATT_S(STACK_1) = fixnum(a_size+1); /* node */
    NODE_DAT_S(STACK_1) = fixnum(d_size+1); /* node */
    *failed_p = NIL;
  } else {
    NODE_KEY_S(STACK_1) = Fixnum_0; /* node */
    NODE_ATT_S(STACK_1) = Fixnum_0; /* node */
    NODE_DAT_S(STACK_1) = Fixnum_0; /* node */
    *failed_p = T;
  }
  XOUT(STACK_4,"init_iteration_node -- state");
  skipSTACK(5);
}}

/* return the next key of the current state or NIL
 if the key is NIL, the HKEY is closed and the stack is popped
 can trigger GC */
static object state_next_key (object state) {
  object stack = ITST_STACK(state);
  if (!nullp(stack)) {
    object node = ITST_NODE(state);
    uintL keynum = posfixnum_to_V(NODE_KEY(node));
    uintL keylen = posfixnum_to_V(NODE_KEY_S(node));
    char* buffer = (char*)alloca(keylen);
    Fpointer fp  = TheFpointer(NODE_HANDLE(node));
    XOUT(state,"state_next_key");
    if (fp->fp_pointer) {
      DWORD status = RegEnumKey((HKEY)(fp->fp_pointer),
                                keynum,buffer,keylen);
      if (status == ERROR_SUCCESS) {
        NODE_KEY(node) = fixnum_inc(NODE_KEY(node),1);
        return asciz_to_string(buffer,GLO(misc_encoding));
      } else {
        SYSCALL_WIN32(RegCloseKey((HKEY)(fp->fp_pointer)));
        fp->fp_pointer = NULL;
        ITST_STACK(state) = Cdr(stack);
        return NIL;
      }
    } else {
      ITST_STACK(state) = Cdr(stack);
      return NIL;
    }
  } else
    return NIL;
}

DEFUN(LDAP::DKEY-SEARCH-NEXT-KEY,state)
{ /* return the next key of this iteration and T if open failed */
  object state = STACK_0;
  object dkey  = test_dir_key(ITST_DKEY(state),true);
  scope_t scope = parse_scope(ITST_SCOPE(state));
  object stack = ITST_STACK(state);
  XOUT(state,"LDAP::DKEY-SEARCH-NEXT-KEY");
  switch (scope) {
    case SCOPE_SELF:            /* just the top */
      if (eq(stack,T))
        init_iteration_node(state,NIL,&value1,&value2); /* first call */
      else
        value1 = value2 = NIL;
      break;
    case SCOPE_KIDS:            /* the children */
      if (eq(stack,T)) {
        init_iteration_node(state,NIL,&value1,&value2);
        if (nullp(value2))
          value1 = state_next_key(STACK_0); /* STACK_0 == state */
      } else {
        value1 = state_next_key(STACK_0);
        value2 = NIL;
      }
      break;
    case SCOPE_TREE:            /* the whole subtree */
      if (eq(stack,T))
        init_iteration_node(state,NIL,&value1,&value2); /* first call */
      else {                    /* find the next node and return it */
        pushSTACK(NIL); /* STACK_0 == subkey; STACK_1 == state */
        do STACK_0 = state_next_key(STACK_1);/* subkey==NIL ==> stack popped */
        while (nullp(STACK_0) && !nullp(ITST_STACK(STACK_1)));
        if (nullp(STACK_0)) value1 = NIL;
        else init_iteration_node(STACK_1,STACK_0,&value1,&value2);
        skipSTACK(1);
      }
      break;
    default: NOTREACHED;
  }
  skipSTACK(1);
  mv_count = 2;
}

DEFUN(LDAP::DKEY-SEARCH-NEXT-ATT,state)
{ /* return the next attribute and its value of this iteration */
  object state = STACK_0;
  object stack = ITST_STACK(state);
  if (!consp(stack)) {
    pushSTACK(`LDAP::DKEY-SEARCH-NEXT-KEY`);
    pushSTACK(state);
    pushSTACK(`LDAP::DKEY-SEARCH-NEXT-ATT`);
    fehler(error,GETTEXT("~S from ~S without ~S before it"));
  }
 {object node = Car(stack);
  Fpointer fp = TheFpointer(NODE_HANDLE(node));
  uintL attnum = posfixnum_to_V(NODE_ATT(node));
  uintL attlen = posfixnum_to_V(NODE_ATT_S(node));
  uintL datlen = posfixnum_to_V(NODE_DAT_S(node));
  char* att = (char*)alloca(attlen);
  char* dat = (char*)alloca(datlen);
  DWORD type;
  DWORD size;
  DWORD status = RegEnumValue((HKEY)(fp->fp_pointer),
                              attnum,att,&attlen,NULL,
                              &type,(BYTE*)dat,&size);
  XOUT(state,"LDAP::DKEY-SEARCH-NEXT-ATT");
  if (status == ERROR_SUCCESS) {
    NODE_ATT(node) = fixnum_inc(NODE_ATT(node),1);
    pushSTACK(asciz_to_string(att,GLO(misc_encoding)));
    pushSTACK(registry_value_to_object(type,size,dat));
  } else {
    pushSTACK(NIL);
    pushSTACK(NIL);
  }
  mv_count = 2;
  value1 = STACK_1;
  value2 = STACK_0;
  skipSTACK(3);
}}

#undef ITST_DKEY
#undef ITST_PATH
#undef ITST_SCOPE
#undef ITST_STACK
#undef ITST_NODE
#undef NODE_NAME
#undef NODE_KEY
#undef NODE_KEY_S
#undef NODE_ATT
#undef NODE_ATT_S
#undef NODE_DAT_S
#undef NODE_HANDLE

/* return the value of the given NAME in the KEY
 KEY must be an open DIR-KEY, NAME - a string
 if the name does not exists, return the DEFAULT (or signal an error)
 setf-able */
DEFUN(LDAP::DIR-KEY-VALUE, key name &optional default) {
  STACK_1 = check_string(STACK_1);
 {object dkey = test_dir_key(STACK_2,true);
  object name = STACK_1;
  object default_value = STACK_0;
  with_string_0(name,GLO(misc_encoding),namez,{
    DWORD status;
    DWORD type;
    DWORD size;
    char* buffer = NULL;
    HKEY hk = (HKEY)SLOT_HANDLE(dir_key_slots(dkey));
    begin_system_call();
    status = RegQueryValueEx(hk,namez,NULL,NULL,NULL,&size);
    if (status != ERROR_SUCCESS) {
      if ((status == ERROR_FILE_NOT_FOUND) && boundp(default_value)) {
        value1 = default_value;
        end_system_call();
        goto end;
      }
      SetLastError(status); OS_error();
    }
    ++size;                     /* one extra char for `\0' */
    buffer = (char*)alloca(size);
    status = RegQueryValueEx(hk,namez,NULL,&type,(BYTE*)buffer,&size);
    if (status != ERROR_SUCCESS) { SetLastError(status); OS_error(); }
    end_system_call();
    value1 = registry_value_to_object(type,size,buffer);
  end:;
  });
  mv_count = 1;
  skipSTACK(3);
}}

DEFUN(LDAP::SET-DKEY-VALUE, key name value)
{ /* set the given NAME in the KEY to the VALUE */
  STACK_1 = check_string(STACK_1);
 {object dkey = test_dir_key(STACK_2,true);
  object name = STACK_1;
  object value = STACK_0;
  HKEY hk = (HKEY)SLOT_HANDLE(dir_key_slots(dkey));
  with_string_0(name,GLO(misc_encoding),namez, {
    if (stringp(value)) {
      with_string_0(value,GLO(misc_encoding),valz, {
        SYSCALL_WIN32(RegSetValueEx(hk,namez,0,REG_SZ,
                                    (BYTE*)valz,strlen(valz)));
      });
    } else if (integerp(value)) {
      DWORD word = I_to_UL(value);
      SYSCALL_WIN32(RegSetValueEx(hk,namez,0,REG_DWORD,(BYTE*)&word,
                                  sizeof(DWORD)));
    } else if (bit_vector_p(Atype_8Bit,value)) {
      uintL idx = 0;
      uintL len = vector_length(value);
      object arr = (simple_vector_p(value) ? value :
                    array_displace_check(value,len,&idx));
      SYSCALL_WIN32(RegSetValueEx(hk,namez,0,REG_BINARY,
                                  TheSbvector(arr)->data+idx,len-idx));
    } else {
      pushSTACK(value);
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,GETTEXT("~S on ~S is illegal"));
    }
  });
  VALUES1(value);
  skipSTACK(3);
}}

#define REG_KEY_DEL(call)                                               \
  STACK_0 = check_string(STACK_0);                                      \
 {object dkey = test_dir_key(STACK_1,true);                             \
  object name = STACK_0; skipSTACK(2);                                  \
  with_string_0(name,GLO(misc_encoding),namez,{                         \
    SYSCALL_WIN32(call((HKEY)SLOT_HANDLE(dir_key_slots(dkey)),namez));  \
    });}                                                                \
  VALUES1(NIL)

DEFUN(LDAP::DIR-KEY-SUBKEY-DELETE, key name)
{ /* delete the specified subkey (and all its subkeys) */
  REG_KEY_DEL(RegDeleteKey);
}
DEFUN(LDAP::DIR-KEY-VALUE-DELETE, key name)
{ /* delete the specified value */
  REG_KEY_DEL(RegDeleteValue);
}
#undef REG_KEY_DEL

/* return all the info about the key, as 10 values:
 class; class_length;
 num_sub_keys; max_sub_key_length; max_class_length;
 num_values; max_value_name_length; max_value_length;
 security_descriptor; write_time */
DEFUN(LDAP::DKEY-INFO,key) {
  object dkey = test_dir_key(popSTACK(),true);
  HKEY hkey = (HKEY)SLOT_HANDLE(dir_key_slots(dkey));
  char* class_name = NULL;
  DWORD class_length;
  DWORD num_sub_keys;
  DWORD max_sub_key_length;
  DWORD max_class_length;
  DWORD num_values;
  DWORD max_value_name_length;
  DWORD max_value_length;
  DWORD security_descriptor;
  FILETIME write_time;
  SYSCALL_WIN32(RegQueryInfoKey(hkey,NULL,&class_length,NULL,NULL,
                                NULL,NULL,NULL,NULL,NULL,NULL,NULL));
  if (class_length > 0) {
    class_length++;
    class_name = (char*)alloca(class_length);
  }
  SYSCALL_WIN32(RegQueryInfoKey(hkey,class_name,&class_length,NULL,
                                &num_sub_keys,&max_sub_key_length,
                                &max_class_length,
                                &num_values,&max_value_name_length,
                                &max_value_length,
                                &security_descriptor,
                                &write_time));
  value1 = (class_name ? asciz_to_string(class_name,GLO(misc_encoding)) : NIL);
  value2 = L_to_I(num_sub_keys);
  value3 = L_to_I(max_sub_key_length);
  value4 = L_to_I(max_class_length);
  value5 = L_to_I(num_values);
  value6 = L_to_I(max_value_name_length);
  value7 = L_to_I(max_value_length);
  value8 = L_to_I(security_descriptor);
  value9 = convert_time_to_universal_w32(&write_time);
  mv_count = 9;
}

#undef SYSCALL_WIN32
#undef SYSCALL_LDAP
