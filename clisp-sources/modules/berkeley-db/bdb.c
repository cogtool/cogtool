/*
 * CLISP: Berkeley-DB <http://www.sleepycat.com/docs/api_c/>
 * Copyright (C) 2003-2005 by Sam Steingold
 */

/* have to undefing UNICODE _here_ because clisp.h will #include <windows.h> */
#undef UNICODE
#include "clisp.h"

#ifndef FOREIGN
# error "Berkeley-DB requires CLISP FOREIGN CPP macro"
#endif

#include "config.h"

#if defined(__CYGWIN__)
# define UNIX_CYGWIN32
#endif

#if defined(_WIN32) || defined(UNIX_CYGWIN32)
# define WIN32_LEAN_AND_MEAN  /* avoid including junk */
# if defined(UNIX_CYGWIN32) || defined(__MINGW32__)
/* `unused' is used in function declarations. */
#  undef unused
#  define ULONGLONG OS_ULONGLONG
#  define ULONG OS_ULONG
#  include <windows.h>
#  undef ULONG
#  undef ULONGLONG
#  define unused (void)
# else
#  undef unused
#  include <windows.h>
#  define unused
# endif
#endif

#if defined(TIME_WITH_SYS_TIME)
# include <sys/time.h>
# include <time.h>
#else
# if defined(HAVE_SYS_TIME_H)
#  include <sys/time.h>
# elif defined(HAVE_TIME_H)
#  include <time.h>
# endif
#endif

#include <string.h>             /* for memset() */
#include <stdio.h>              /* fopen/fclose for db->verify */

/* #define DEBUG */
#if defined(DEBUG)
# include <dmalloc.h>
# define my_malloc malloc
extern object nobject_out (FILE* stream, object obj);
# define XOUT(obj,label)                                                \
  (printf("[%s:%d] %s: %s:\n",__FILE__,__LINE__,STRING(obj),label),     \
   obj=nobject_out(stdout,obj), printf("\n"))
# define VECOUT(v,l)                            \
  (printf("[%s:%d] %d: ",__FILE__,__LINE__,l),  \
   vecout(v,l), printf("\n"))
static void vecout (unsigned char* v, int l) {
  int i; for (i=0; i<l; i++) printf(" %x",v[i]);
}
#else
# undef OBJECT_OUT
# define OBJECT_OUT(o,l)
# define XOUT(o,l)
# define VECOUT(v,l)
#endif

/* convert C string to Lisp string; NULL --> NIL  */
static inline object asciz_to_string0 (const char* a, object e)
{ return a ? asciz_to_string(a,e) : NIL; }

#include <db.h>

/* this has to be before DEFCHECKERs */
typedef enum { DBT_RAW, DBT_STRING, DBT_INTEGER } dbt_o_t;

DEFMODULE(bdb,"BDB")

DEFUN(BDB:DB-VERSION,&optional subsystems-p)
{ /* Berkeley-DB version */
  int major, minor, patch;
  char * version;
  begin_system_call();
  version = db_version(&major,&minor,&patch);
  end_system_call();
  if (major != DB_VERSION_MAJOR || minor != DB_VERSION_MINOR) {
    pushSTACK(fixnum(DB_VERSION_MINOR)); pushSTACK(fixnum(DB_VERSION_MAJOR));
    pushSTACK(fixnum(minor)); pushSTACK(fixnum(major));
    fehler(serious_condition,GETTEXT("Version mismatch: compile=~S.~S link=~S~S"));
  }
  if (patch != DB_VERSION_PATCH) {
    /* is this warning warranted? */
    pushSTACK(fixnum(DB_VERSION_PATCH)); pushSTACK(fixnum(patch));
    fehler(warning,GETTEXT("Patch level mismatch: compile=~S link=~S"));
  }
  value1 = asciz_to_string(version,GLO(misc_encoding));
  value2 = fixnum(major);
  value3 = fixnum(minor);
  value4 = fixnum(patch);
  if (missingp(STACK_0)) {
    mv_count = 4;
  } else {                      /* return subsystem versions too */
    int subsystem_count = 0;
    pushSTACK(value1);          /*  save */
#  if defined(DB_LOCKVERSION)
    pushSTACK(`:LOCK`); pushSTACK(fixnum(DB_LOCKVERSION)); subsystem_count++;
#  endif
#  if defined(DB_LOGVERSION)
    pushSTACK(`:LOG`); pushSTACK(fixnum(DB_LOGVERSION)); subsystem_count++;
#  endif
#  if defined(DB_TXNVERSION)
    pushSTACK(`:TXN`); pushSTACK(fixnum(DB_TXNVERSION)); subsystem_count++;
#  endif
#  if defined(DB_BTREEVERSION)
    pushSTACK(`:BTREE`); pushSTACK(fixnum(DB_BTREEVERSION)); subsystem_count++;
#  endif
#  if defined(DB_HASHVERSION)
    pushSTACK(`:HASH`); pushSTACK(fixnum(DB_HASHVERSION)); subsystem_count++;
#  endif
#  if defined(DB_QAMVERSION)
    pushSTACK(`:QAM`); pushSTACK(fixnum(DB_QAMVERSION)); subsystem_count++;
#  endif
#  if defined(DB_SEQUENCE_VERSION)
    pushSTACK(`:SEQUENCE`); pushSTACK(fixnum(DB_SEQUENCE_VERSION));
    subsystem_count++;
#  endif
    value5 = listof(2*subsystem_count);
    value1 = popSTACK();        /* restore */
    mv_count = 5;
  }
  skipSTACK(1);
}

static char *error_message = NULL;
#if defined(HAVE_DBE_SET_ERRCALL_ACCEPT_DBE)
static void error_callback (const DB_ENV* dbe,
                            const char *errpfx, const char *msg)
#else
static void error_callback (const char *errpfx, char *msg)
#endif
{
  char *data;
  if (error_message) { /* append the new message to the previous ones */
    int len = strlen(error_message);
    error_message = (char*)my_realloc(error_message,3 + strlen(msg) + len);
    error_message[len++] = ';';
    error_message[len++] = ' ';
    data = error_message + len;
  } else {
    int offset = errpfx ? strlen(errpfx)+4 : 0;
    data = error_message = (char*)my_malloc(1 + offset + strlen(msg));
    if (errpfx) {
      data[0] = '[';
      strcpy(data+1,errpfx);
      data[offset-3] = ']';
      data[offset-2] = ':';
      data[offset-1] = ' ';
    }
    data += offset;
  }
  strcpy(data,msg);
}
#define FREE_RESET(x) if (x) { free(x); x = NULL; }
static void error_message_reset (void) { FREE_RESET(error_message) }
nonreturning_function(static, error_bdb, (int status, char *caller)) {
  end_system_call();
  pushSTACK(`BDB::BDB-ERROR`);  /* error type */
  pushSTACK(`:ERRNO`); pushSTACK(fixnum(status));
  if (error_message)
    pushSTACK(`"~S (~S): ~S: ~S"`);
  else pushSTACK(`"~S (~S): ~S"`);
  pushSTACK(TheSubr(subr_self)->name);
  pushSTACK(asciz_to_string(caller,GLO(misc_encoding)));
  pushSTACK(asciz_to_string(db_strerror(status),GLO(misc_encoding)));
  if (error_message) {
    pushSTACK(asciz_to_string(error_message,GLO(misc_encoding)));
    free(error_message); error_message = NULL;
    funcall(L(error_of_type),8);
  } else funcall(L(error_of_type),7);
  NOTREACHED;
}

#if defined(HAVE_DB_ENV_SET_MSGCALL)
/* a collection of messages */
struct messages { int max; int len; char* msgs[unspecified]; };

/* allocate the struct+space for max pointers */
static struct messages * make_messages (int max) {
  struct messages * data = (struct messages*)
    my_malloc(sizeof(struct messages) + (max-unspecified)*sizeof(char*));
  if (max<1) abort();
  data->len = 0;
  data->max = max;
  while (max) data->msgs[--max] = NULL;
  return data;
}

/* release all messages and the structure itself */
static void free_messages (struct messages* data) {
  if (data) {
    while (data->len) free(data->msgs[--data->len]);
    free(data);
  }
}
#define close_messages(data) \
  do{ free_messages((struct messages*)data); data=NULL; }while(0)

/* convert all messages to a list of strings
 can trigger GC */
static object extract_messages (struct messages* data) {
  if (data && data->len) {
    int ii;
    for (ii=0; ii < data->len; ii++) {
      pushSTACK(asciz_to_string(data->msgs[ii],GLO(misc_encoding)));
      free(data->msgs[ii]);
    }
    data->len = 0;
    return listof(ii);
  } else return NIL;
}

/* add an extra message
 since we may need to reallocate, DATA is passed by reference */
static void add_message (struct messages* *data_, const char* msg) {
  if ((*data_) == NULL) (*data_) = make_messages(5);
  if ((*data_)->max = (*data_)->len) {  /* double the space */
    int new_max = 2*(*data_)->max;
    (*data_) = (struct messages*)
      my_realloc((*data_), (sizeof(struct messages) +
                            (new_max-unspecified)*sizeof(char*)));
    (*data_)->max = new_max;
  }
  { /* now max>len */
    int len = strlen(msg);
    (*data_)->msgs[++(*data_)->len] = (char*)my_malloc(len+1);
    strcpy((*data_)->msgs[(*data_)->len],msg);
  }
}

/* dbe is a const pointer, so we cannot change its slots explicitly
   (e.g., by assigning to app_private),
   so we have to pass an address to add_message() */
static void message_callback (const DB_ENV* dbe, const char *msg) {
  add_message((struct messages**)&(dbe->app_private),msg);
}
#else
#define close_messages(data)  data=NULL
#endif

#define SYSCALL1(caller,args,cleanup)     do {                  \
    int db_error_code;                                          \
    begin_system_call();                                        \
    db_error_code = caller args; cleanup                        \
    if (db_error_code) error_bdb(db_error_code,#caller);        \
    end_system_call();                                          \
  } while(0)
#define SYSCALL(caller,args)     SYSCALL1(caller,args,)

/* check whether the OBJ has type TYPE and return its handle
 can trigger GC */
typedef enum {
  BH_VALID,           /* return a valid handle */
  BH_INVALIDATE,      /* invalidate and return handle, NULL for invalid FP */
  BH_NIL_IS_NULL,     /* return either NULL for NIL or a valid handle */
  BH_INVALID_IS_NULL  /* return either NULL for invalid or a valid handle */
} bdb_handle_t;
static void* bdb_handle (object obj, object type, bdb_handle_t oh) {
 bdb_handle_restart:
  while (!typep_classname(obj,type)) {
    if (missingp(obj) && oh == BH_NIL_IS_NULL) return NULL;
    pushSTACK(type);            /* save */
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(type);            /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(type); pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not a ~S"));
    obj = value1; type = popSTACK(); /* restore */
  }
  { Fpointer fp = TheFpointer(TheStructure(obj)->recdata[1]);
    if (!fp_validp(fp)) {
      switch (oh) {
        case BH_INVALIDATE: case BH_INVALID_IS_NULL:
          return NULL;
        default:
          pushSTACK(type);            /* save */
          pushSTACK(NIL);             /* no PLACE */
          pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
          check_value(type_error,GETTEXT("~S: ~S has been closed or comes from a previous Lisp session"));
          obj = value1; type = popSTACK(); /* restore */
          goto bdb_handle_restart;
      }
    }
    if (oh == BH_INVALIDATE) mark_fp_invalid(fp);
    return fp->fp_pointer;
  }
}
/* access the parents & dependents fields in a bdb-handle structure */
#define Parents(x)     TheStructure(x)->recdata[2]
#define Dependents(x)  TheStructure(x)->recdata[3]
/* allocate a wrapper for the pointer and add a finalizer to it
 parent can be a bdb-object of a list thereof
 can trigger GC */
static void wrap_finalize (void* pointer, object parents,
                           object maker, object closer) {
  pushSTACK(maker); pushSTACK(parents); pushSTACK(closer);
  pushSTACK(allocate_fpointer(pointer)); funcall(`BDB::MKHANDLE`,4);
}

/* ===== Database Environment ===== */
/* not exported:
 DB_ENV->err	Error message with error string
 DB_ENV->errx	Error message
*/

DEFCHECKER(dbe_encryption_check, prefix=DB_ENCRYPT, default=0, AES)
/* set the password to perform encryption and decryption.
 can trigger GC */
static void dbe_set_encryption (DB_ENV *dbe, gcv_object_t *o_flags_,
                                gcv_object_t *o_password_) {
  u_int32_t flags = dbe_encryption_check(*o_flags_);
  *o_password_ = check_string(*o_password_);
  with_string_0(*o_password_,GLO(misc_encoding),password,
                { SYSCALL(dbe->set_encrypt,(dbe,password,flags)); });
}

DEFUN(BDB:DBE-CREATE,&key :PASSWORD :ENCRYPT    \
      :HOST :CLIENT-TIMEOUT :SERVER-TIMEOUT)
{ /* Create an environment handle */
  DB_ENV *dbe, *dbe_cl;
  bool remote_p = boundp(STACK_2); /* host ==> remote */
  int status, cl_timeout = 0, sv_timeout = 0;
# if defined(DB_RPCCLIENT)      /* 4.2 and later */
  SYSCALL(db_env_create,(&dbe,remote_p ? DB_RPCCLIENT : 0));
# elif defined(DB_CLIENT)       /* 4.1 and before */
  SYSCALL(db_env_create,(&dbe,remote_p ? DB_CLIENT : 0));
# else
#  error "how does your Berkeley DB create a remote client?"
# endif
  if (remote_p) {
    if (uint_p(STACK_0)) sv_timeout = I_to_uint(STACK_0);
    if (uint_p(STACK_1)) cl_timeout = I_to_uint(STACK_1);
   host_restart:
    if (stringp(STACK_2)) {     /* string host */
      with_string_0(STACK_2,GLO(misc_encoding),hostz, {
          begin_system_call();
          status = dbe->set_rpc_server(dbe,NULL,hostz,cl_timeout,sv_timeout,0);
          end_system_call();
        });
    } else if ((dbe_cl = (DB_ENV*)bdb_handle(STACK_2,`BDB::DBE`,
                                             BH_NIL_IS_NULL))) {
      /* reuse client */
      begin_system_call();
      status = dbe->set_rpc_server(dbe,dbe_cl->cl_handle,NULL,
                                   cl_timeout,sv_timeout,0);
      end_system_call();
    } else {                    /* bad host */
      pushSTACK(NIL);           /* no PLACE */
      pushSTACK(STACK_(2+1));   /* TYPE-ERROR slot DATUM */
      pushSTACK(`(OR STRING BDB::DBE)`); /* TYPE-ERROR slot EXPECTED-TYPE */
      pushSTACK(STACK_2);                /* host */
      pushSTACK(`BDB::DBE`); pushSTACK(S(string)); pushSTACK(`:HOST`);
      pushSTACK(TheSubr(subr_self)->name);
      check_value(type_error,GETTEXT("~S: ~S should be a ~S or a ~S, not ~S"));
      STACK_2 = value1;
      goto host_restart;
    }
    if (status) error_bdb(status,"set_rpc_server");
  }
  if (!missingp(STACK_4))       /* :PASSWD */
    dbe_set_encryption(dbe,&STACK_3,&STACK_4);
  skipSTACK(5);
  /* set error & message callbacks */
  begin_system_call(); dbe->set_errcall(dbe,&error_callback);
#if defined(HAVE_DB_ENV_SET_MSGCALL)
  dbe->set_msgcall(dbe,&message_callback);
#endif
  end_system_call();
  wrap_finalize(dbe,NIL,`BDB::MKDBE`,``BDB::DBE-CLOSE``);
}

static void time_stamp (FILE* out, char* prefix) {
  fputs(prefix,out);
#if defined(HAVE_GETTIMEOFDAY) && defined(HAVE_LOCALTIME) && defined(HAVE_STRFTIME)
  { char str[80]; struct timeval tv; gettimeofday(&tv,NULL);
    strftime(str,80," [%Y-%m-%d %a %H:%M:%S %Z]",localtime(&tv.tv_sec));
    fputs(str,out);
  }
#else
  fprintf(out," [%s:%d: FIXME time_stamp()]",__FILE__,__LINE__);
#endif
  fputs("\n",out);
}

/* open the C file and return it
 can trigger GC */
static FILE* my_fopen (object path) {
  FILE *ret;
  with_string_0(path=physical_namestring(path),GLO(pathname_encoding),pathz,{
      begin_system_call();
      ret = fopen(pathz,"w");
      if (ret == NULL) OS_file_error(path);
      time_stamp(ret,"opened");
      end_system_call();
    });
  return ret;
}

/* some DB parameters (errfile, errpfx) are actually kept in DB.dbenv field.
 http://groups.google.com/groups?&selm=adecb6f.0408050624.5ed0a11e@posting.google.com
 therefore, instead of calling db->get_errfile(db,...) we call
 db->dbenv(get_errfile(db->dbenv,...)).
 note that DB-CLOSE should close these parameters
 only if it is a standalone DB! */
#define CLOSE_FILE(which)                               \
  static void close_##which##file (DB_ENV *dbe) {       \
    FILE *file;                                         \
    begin_system_call();                                \
    dbe->get_##which##file(dbe,&file);                  \
    if (file && (file != stdout) && (file != stderr)) { \
      time_stamp(file,"closed");                        \
      fclose(file);                                     \
    }                                                   \
    end_system_call();                                  \
  }
CLOSE_FILE(err)
/* set :ERRFILE to STACK_0
 can trigger GC */
#define RESET_FILE(which)                                               \
  static void reset_##which##file (DB_ENV *dbe) {                       \
    close_##which##file(dbe);                                           \
    if (nullp(STACK_0)) {                                               \
      begin_system_call(); dbe->set_##which##file(dbe,NULL);            \
      end_system_call();                                                \
    } else {                                                            \
      FILE *file = my_fopen(STACK_0);                                   \
      begin_system_call(); dbe->set_##which##file(dbe,file);            \
      end_system_call();                                                \
    }                                                                   \
  }
RESET_FILE(err)
/* extract errfile */
#define EXTRACT_FILE(which)                             \
  static object dbe_get_##which##file (DB_ENV *dbe) {   \
    FILE* file;                                         \
    int fd = -1;                                        \
    begin_system_call();                                \
    dbe->get_##which##file(dbe,&file);                  \
    if (file) fd = fileno(file);                        \
    end_system_call();                                  \
    return fd >= 0 ? fixnum(fd) : NIL;                  \
  }
EXTRACT_FILE(err)
#if defined(HAVE_DB_ENV_SET_MSGCALL)
CLOSE_FILE(msg)
RESET_FILE(msg)
EXTRACT_FILE(msg)
#else
# define close_msgfile(dbe)   do{}while(0)
# define reset_msgfile(dbe)   do{}while(0)
# define dbe_get_msgfile(dbe) NIL
#endif
static void close_errpfx (DB_ENV *dbe) {
  const char *errpfx;
  begin_system_call();
  dbe->get_errpfx(dbe,&errpfx);
  if (errpfx) free((void*)errpfx);
  end_system_call();
}
DEFUN(BDB:DBE-CLOSE, dbe)
{ /* close DB environment */
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_0,`BDB::DBE`,BH_INVALIDATE);
  if (dbe) {
    funcall(`BDB::KILL-HANDLE`,1);
    close_errfile(dbe);
    close_errpfx(dbe);
    close_msgfile(dbe);
    close_messages(dbe->app_private);
    SYSCALL(dbe->close,(dbe,0));
    VALUES1(T);
  } else { skipSTACK(1); VALUES1(NIL); }
}

#if defined(HAVE_DB_ENV_SET_MSGCALL)
DEFUN(BDB:DBE-MESSAGES, dbe)
{ /* close DB environment */
  DB_ENV *dbe = (DB_ENV*)bdb_handle(popSTACK(),`BDB::DBE`,BH_VALID);
  VALUES1(extract_messages((struct messages*)dbe->app_private));
}
#endif

DEFFLAGSET(bdb_ac_flags, DB_AUTO_COMMIT)
DEFUN(BDB:DBE-DBREMOVE, dbe file database &key :TRANSACTION :AUTO-COMMIT)
{ /* remove DATABASE from FILE or the whole FILE */
  u_int32_t flags = bdb_ac_flags();
  DB_TXN *txn = (DB_TXN*)bdb_handle(popSTACK(),`BDB::TXN`,BH_NIL_IS_NULL);
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_2,`BDB::DBE`,BH_VALID);
  if (!nullp(STACK_2)) STACK_2 = check_string(STACK_0); /* DATABASE */
  STACK_1 = physical_namestring(STACK_1);               /* FILE */
  with_string_0(STACK_1,GLO(pathname_encoding),file, {
      if (stringp(STACK_0)) {
        with_string_0(STACK_0,GLO(misc_encoding),database, {
            SYSCALL(dbe->dbremove,(dbe,txn,file,database,flags));
          });
      } else SYSCALL(dbe->dbremove,(dbe,txn,file,NULL,flags));
    });
  VALUES0; skipSTACK(3);
}

DEFUN(BDB:DBE-DBRENAME, dbe file database newname       \
      &key :TRANSACTION :AUTO-COMMIT)
{ /* rename DATABASE to NEWNAME in FILE */
  u_int32_t flags = bdb_ac_flags();
  DB_TXN *txn = (DB_TXN*)bdb_handle(popSTACK(),`BDB::TXN`,BH_NIL_IS_NULL);
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_3,`BDB::DBE`,BH_VALID);
  with_string_0(physical_namestring(STACK_2),GLO(pathname_encoding),file, {
      with_string_0(check_string(STACK_1),GLO(misc_encoding),database, {
          with_string_0(check_string(STACK_0),GLO(misc_encoding),newname, {
              SYSCALL(dbe->dbrename,(dbe,txn,file,database,newname,flags));
            });
        });
    });
  VALUES0; skipSTACK(4);
}

DEFFLAGSET(dbe_open_flags, DB_JOINENV DB_INIT_CDB DB_INIT_LOCK DB_INIT_LOG \
           DB_INIT_MPOOL DB_INIT_TXN DB_RECOVER DB_RECOVER_FATAL        \
           DB_USE_ENVIRON DB_USE_ENVIRON_ROOT DB_CREATE DB_LOCKDOWN     \
           DB_PRIVATE DB_SYSTEM_MEM DB_THREAD)
DEFCHECKER(check_dbe_open_flags,prefix=DB,default=0,bitmasks=both,      \
           type=uint32_t, JOINENV INIT-CDB INIT-LOCK INIT-LOG           \
           INIT-MPOOL INIT-TXN RECOVER RECOVER-FATAL USE-ENVIRON        \
           USE-ENVIRON-ROOT CREATE LOCKDOWN PRIVATE SYSTEM-MEM THREAD)
DEFUN(BDB:DBE-OPEN, dbe &key :HOME :FLAGS :JOINENV :INIT-CDB :INIT-LOCK    \
      :INIT-LOG :INIT-MPOOL :INIT-TXN :RECOVER :RECOVER-FATAL :USE-ENVIRON \
      :USE-ENVIRON-ROOT :CREATE :LOCKDOWN :PRIVATE :SYSTEM-MEM :THREAD :MODE)
{ /* open DB environment */
  int mode = check_uint_default0(popSTACK());
  u_int32_t flags = dbe_open_flags()
    | check_dbe_open_flags_from_list(popSTACK());
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_1,`BDB::DBE`,BH_VALID);
  if (!missingp(STACK_0)) {
    with_string_0(physical_namestring(STACK_0),GLO(pathname_encoding),home,
                  { SYSCALL(dbe->open,(dbe,home,flags,mode)); });
  } else SYSCALL(dbe->open,(dbe,NULL,flags,mode));
  VALUES0; skipSTACK(2);
}

DEFFLAGSET(dbe_remove_flags, DB_FORCE DB_USE_ENVIRON DB_USE_ENVIRON_ROOT)
DEFUN(BDB:DBE-REMOVE, dbe &key :HOME :FORCE :USE-ENVIRON :USE-ENVIRON-ROOT)
{ /* destroy an environment */
  u_int32_t flags = dbe_remove_flags();
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_1,`BDB::DBE`,BH_INVALIDATE);
  if (!missingp(STACK_0)) {
    with_string_0(physical_namestring(STACK_0),GLO(pathname_encoding),home,
                  { SYSCALL(dbe->remove,(dbe,home,flags)); });
  } else SYSCALL(dbe->remove,(dbe,NULL,flags));
  VALUES0; skipSTACK(2);
}

/* ===== Environment Configuration ===== */

/* not exported:
 DB_ENV->set_app_dispatch	Configure application recovery interface
 DB_ENV->set_alloc	Set local space allocation functions
 DB_ENV->set_errcall	Set error message callback
 DB_ENV->set_feedback	Set feedback callback
 DB_ENV->set_paniccall	Set panic callback
*/

/* set :ERRPFX to STACK_0
 can trigger GC */
static void reset_errpfx (DB_ENV *dbe) {
  close_errpfx(dbe);
  if (nullp(STACK_0)) {
    begin_system_call(); dbe->set_errpfx(dbe,NULL); end_system_call();
  } else
    with_string_0(check_string(STACK_0),GLO(misc_encoding), prefix, {
        char *errpfx = (char*)my_malloc(prefix_bytelen+1);
        strcpy(errpfx,prefix);
        begin_system_call(); dbe->set_errpfx(dbe,errpfx); end_system_call();
      });
}
/* extract errpfx
 can trigger GC */
static object dbe_get_errpfx (DB_ENV *dbe) {
  const char* errpfx;
  begin_system_call();
  dbe->get_errpfx(dbe,&errpfx);
  end_system_call();
  return asciz_to_string0(errpfx,GLO(misc_encoding));
}
/* define a flag checker */
#define FLAG_EXTRACTOR(name,type)                       \
  static int name (type z) {                            \
    u_int32_t flags;                                    \
    SYSCALL(z->get_flags,(z,&flags));                   \
    return flags;                                       \
  }

/* at 4.2, http://www.sleepycat.com/docs/ref/upgrade.4.4/mutex.html
   get_tas_spins was renamed to mutex_get_tas_spins: */
#if defined(HAVE_DB_ENV_MUTEX_GET_TAS_SPINS)
# define get_tas_spins mutex_get_tas_spins
#endif
#if defined(HAVE_DB_ENV_MUTEX_SET_TAS_SPINS)
# define set_tas_spins mutex_set_tas_spins
#endif
/* todo: support mutexes properly */

static void set_flags (object arg, u_int32_t *flag_on, u_int32_t *flag_off,
                       u_int32_t values) {
  if (boundp(arg))
    *(nullp(arg) ? flag_off : flag_on) |= values;
}
static void my_set_verbose (DB_ENV *dbe, object arg, u_int32_t flag) {
  if (boundp(arg)) SYSCALL(dbe->set_verbose,(dbe,flag,!nullp(arg)));
}
DEFCHECKER(check_lk_detect,prefix=DB_LOCK, default=DB_LOCK_DEFAULT, NORUN \
           DEFAULT EXPIRE MAXLOCKS MINLOCKS MINWRITE OLDEST RANDOM YOUNGEST)
DEFUN(BDB:DBE-SET-OPTIONS, dbe &key                                     \
      :ERRFILE :ERRPFX :PASSWORD :ENCRYPT :LOCK-TIMEOUT :TXN-TIMEOUT :TIMEOUT \
      :SHM-KEY :TAS-SPINS :TX-TIMESTAMP :TX-MAX :DATA-DIR :TMP-DIR      \
      :LG-BSIZE :LG-DIR :LG-MAX :LG-REGIONMAX                           \
      :LK-CONFLICTS :LK-DETECT :LK-MAX-LOCKERS :LK-MAX-LOCKS :LK-MAX-OBJECTS \
      :AUTO-COMMIT :CDB-ALLDB :DIRECT-DB :DSYNC-LOG :LOG-AUTOREMOVE \
      :LOG-INMEMORY :DIRECT-LOG :NOLOCKING                              \
      :NOMMAP :NOPANIC :OVERWRITE :PANIC-ENVIRONMENT :REGION-INIT       \
      :TXN-NOSYNC :TXN-WRITE-NOSYNC :YIELDCPU                           \
      :VERB-CHKPOINT :VERB-DEADLOCK :VERB-RECOVERY :VERB-REPLICATION    \
      :VERB-WAITSFOR :VERBOSE :MSGFILE)
{ /* set many options */
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_(45),`BDB::DBE`,BH_VALID);
  if (!missingp(STACK_0)) reset_msgfile(dbe);
  skipSTACK(1);                 /* drop :MSGFILE */
  { object arg = popSTACK();
    my_set_verbose(dbe,arg,DB_VERB_WAITSFOR);
    my_set_verbose(dbe,arg,DB_VERB_REPLICATION);
#  if defined(DB_VERB_CHKPOINT)
    my_set_verbose(dbe,arg,DB_VERB_CHKPOINT);
#  endif
    my_set_verbose(dbe,arg,DB_VERB_RECOVERY);
    my_set_verbose(dbe,arg,DB_VERB_DEADLOCK);
  }
  my_set_verbose(dbe,popSTACK(),DB_VERB_WAITSFOR);
  my_set_verbose(dbe,popSTACK(),DB_VERB_REPLICATION);
  my_set_verbose(dbe,popSTACK(),DB_VERB_RECOVERY);
  my_set_verbose(dbe,popSTACK(),DB_VERB_DEADLOCK);
#if defined(DB_VERB_CHKPOINT)
  my_set_verbose(dbe,popSTACK(),DB_VERB_CHKPOINT);
#else
  skipSTACK(1);               /* drop :VERB-CHKPOINT */
#endif
  { /* flags */
    u_int32_t flags_on = 0, flags_off = 0;
    set_flags(popSTACK(),&flags_on,&flags_off,DB_YIELDCPU);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_TXN_WRITE_NOSYNC);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_TXN_NOSYNC);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_REGION_INIT);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_PANIC_ENVIRONMENT);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_OVERWRITE);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_NOPANIC);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_NOMMAP);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_NOLOCKING);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_DIRECT_LOG);
#  if defined(DB_LOG_INMEMORY)
    set_flags(popSTACK(),&flags_on,&flags_off,DB_LOG_INMEMORY);
#  else
    skipSTACK(1);               /* skip :LOG-INMEMORY */
#  endif
    set_flags(popSTACK(),&flags_on,&flags_off,DB_LOG_AUTOREMOVE);
#  if defined(DB_DSYNC_LOG)
    set_flags(popSTACK(),&flags_on,&flags_off,DB_DSYNC_LOG);
#  else
    skipSTACK(1);               /* skip :DSYNC-LOG */
#  endif
    set_flags(popSTACK(),&flags_on,&flags_off,DB_DIRECT_DB);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_CDB_ALLDB);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_AUTO_COMMIT);
    if (flags_off) SYSCALL(dbe->set_flags,(dbe,flags_off,0));
    if (flags_on)  SYSCALL(dbe->set_flags,(dbe,flags_on,1));
  }
#define DBE_SET1(what,type,get,how)    do {        \
    if (!missingp(STACK_0)) {                      \
      type what = get;                             \
      SYSCALL(dbe->set_##what,how);                \
    }                                              \
    skipSTACK(1);                                  \
  } while(0)
#define DBE_SET(what,type,get)   DBE_SET1(what,type,get,(dbe,what))
  DBE_SET(lk_max_objects,u_int32_t,I_to_uint32(check_uint32(STACK_0)));
  DBE_SET(lk_max_locks,u_int32_t,I_to_uint32(check_uint32(STACK_0)));
  DBE_SET(lk_max_lockers,u_int32_t,I_to_uint32(check_uint32(STACK_0)));
  DBE_SET(lk_detect,u_int32_t,check_lk_detect(STACK_0));
  if (!missingp(STACK_0)) {     /* LK_CONFLICTS */
    uintL dims[2];
   restart_LK_CONFLICTS:        /* check arguments */
    STACK_0 = check_array(STACK_0);
    if (2 != array_rank(STACK_0)
        || (get_array_dimensions(STACK_0,2,dims), dims[0] != dims[1])
        || (array_atype(STACK_0) != Atype_8Bit)) {
      pushSTACK(NIL);           /* no PLACE */
      pushSTACK(STACK_1);       /* TYPE-ERROR slot DATUM */
      pushSTACK(`(ARRAY (UNSIGNED-BYTE 8) (* *))`); /* EXPECTED-TYPE */
      pushSTACK(STACK_1); pushSTACK(`:LK-CONFLICTS`);
      pushSTACK(TheSubr(subr_self)->name);
      check_value(type_error,
                  GETTEXT("~S: ~S must be a square matrix of bytes, not ~S"));
      STACK_0 = value1;
      goto restart_LK_CONFLICTS;
    }
    { /* set the conflict matrix */
      uintL offset = 0;
      object data = array_displace_check(STACK_0,dims[0]*dims[1],&offset);
      SYSCALL(dbe->set_lk_conflicts,
              (dbe,TheSbvector(data)->data + offset,dims[0]));
    }
  }
  skipSTACK(1);
  DBE_SET(lg_regionmax,u_int32_t,I_to_uint32(check_uint32(STACK_0)));
  DBE_SET(lg_max,u_int32_t,I_to_uint32(check_uint32(STACK_0)));
  if (!missingp(STACK_0)) {     /* LG_DIR */
    with_string_0(physical_namestring(popSTACK()),GLO(pathname_encoding),dirz,
                  { SYSCALL(dbe->set_lg_dir,(dbe,dirz)); });
  } else skipSTACK(1);
  DBE_SET(lg_bsize,u_int32_t,I_to_uint32(check_uint32(STACK_0)));
  if (!missingp(STACK_0)) {     /* TMP_DIR */
    with_string_0(physical_namestring(popSTACK()),GLO(pathname_encoding),tmpz,
                  { SYSCALL(dbe->set_tmp_dir,(dbe,tmpz)); });
  } else skipSTACK(1);
  if (!missingp(STACK_0)) {     /* DATA_DIR */
    if (consp(STACK_0)) {
      do {
        with_string_0(physical_namestring(Car(STACK_0)),GLO(pathname_encoding),
                      dataz, { SYSCALL(dbe->set_data_dir,(dbe,dataz)); });
        STACK_0 = Cdr(STACK_0);
      } while (consp(STACK_0));
      skipSTACK(1);
    } else
      with_string_0(physical_namestring(popSTACK()),GLO(pathname_encoding),
                    dataz, { SYSCALL(dbe->set_data_dir,(dbe,dataz)); });
  } else skipSTACK(1);
  DBE_SET(tx_max,u_int32_t,I_to_uint32(check_uint32(STACK_0)));
  if (!missingp(STACK_0)) {     /* TX_TIMESTAMP */
    time_t timestamp;
    convert_time_from_universal(STACK_0,&timestamp);
    SYSCALL(dbe->set_tx_timestamp,(dbe,&timestamp));
  }
  skipSTACK(1);
  DBE_SET(tas_spins,u_int32_t,I_to_uint32(check_uint32(STACK_0)));
  DBE_SET(shm_key,long,I_to_sint32(check_sint32(STACK_0)));
  if (!missingp(STACK_0)) {     /* TIMEOUT = TXN_TIMEOUT & LOCK_TIMEOUT */
    STACK_0 = check_list(STACK_0);
    if (consp(STACK_0)) {
      db_timeout_t txn_timeout = I_to_uint(check_uint(Car(STACK_0)));
      SYSCALL(dbe->set_timeout,(dbe,txn_timeout,DB_SET_TXN_TIMEOUT));
      STACK_0 = check_list(Cdr(STACK_0));
      if (consp(STACK_0)) {
        db_timeout_t lock_timeout = I_to_uint(check_uint(Car(STACK_0)));
        SYSCALL(dbe->set_timeout,(dbe,lock_timeout,DB_SET_LOCK_TIMEOUT));
      }
    }
  }
  skipSTACK(1);
  DBE_SET1(timeout,db_timeout_t,I_to_uint(check_uint(STACK_0)),
           (dbe,timeout,DB_SET_TXN_TIMEOUT));
  DBE_SET1(timeout,db_timeout_t,I_to_uint(check_uint(STACK_0)),
           (dbe,timeout,DB_SET_LOCK_TIMEOUT));
  if (!missingp(STACK_1))       /* PASSWORD */
    dbe_set_encryption(dbe,&STACK_0,&STACK_1);
  skipSTACK(2);
  if (!missingp(STACK_0)) reset_errpfx(dbe);
  skipSTACK(1);
  if (!missingp(STACK_0)) reset_errfile(dbe);
  skipSTACK(1);
  VALUES0; skipSTACK(1);        /* skip dbe */
#undef DBE_SET
#undef DBE_SET1
}

/* get the list of verbosity options
 can trigger GC */
static object dbe_get_verbose (DB_ENV *dbe) {
  int count = 0, onoffp;
  SYSCALL(dbe->get_verbose,(dbe,DB_VERB_WAITSFOR,&onoffp));
  if (onoffp) { pushSTACK(`:VERB-WAITSFOR`); count++; }
  SYSCALL(dbe->get_verbose,(dbe,DB_VERB_REPLICATION,&onoffp));
  if (onoffp) { pushSTACK(`:VERB-REPLICATION`); count++;}
  SYSCALL(dbe->get_verbose,(dbe,DB_VERB_RECOVERY,&onoffp));
  if (onoffp) { pushSTACK(`:VERB-RECOVERY`); count++; }
  SYSCALL(dbe->get_verbose,(dbe,DB_VERB_DEADLOCK,&onoffp));
  if (onoffp) { pushSTACK(`:VERB-DEADLOCK`); count++; }
#if defined(DB_VERB_CHKPOINT)
  SYSCALL(dbe->get_verbose,(dbe,DB_VERB_CHKPOINT,&onoffp));
  if (onoffp) { pushSTACK(`:VERB-CHKPOINT`); count++; }
#endif
  return listof(count);
}
/* get the data directory list
 can trigger GC */
static object dbe_get_data_dirs (DB_ENV *dbe) {
  const char **dirs; int ii;
  SYSCALL(dbe->get_data_dirs,(dbe,&dirs));
  if (dirs) {
    for (ii=0; dirs[ii]; ii++)
      pushSTACK(asciz_to_string(dirs[ii],GLO(pathname_encoding)));
    return listof(ii);
  } else return NIL;
}
/* get the home directory
   return T when DBE is not yet open and a list otherwise
 can trigger GC */
static object dbe_get_home_dir (DB_ENV *dbe, int errorp) {
  const char *home;
  int status;
  begin_system_call();
  status = dbe->get_home(dbe,&home);
  end_system_call();
  if (status) {
    if (errorp) error_bdb(status,"dbe->get_home");
    error_message_reset(); return T;
  }
  if (home == NULL) return NIL;
  return asciz_to_string(home,GLO(pathname_encoding));
}
/* get the DBE open flags
   return T when DBE is not yet open and a list otherwise
 can trigger GC */
static object dbe_get_open_flags (DB_ENV *dbe, int errorp) {
  u_int32_t flags, status;
  begin_system_call();
  status = dbe->get_open_flags(dbe,&flags);
  end_system_call();
  if (status) {
    if (errorp) error_bdb(status,"dbe->get_open_flags");
    error_message_reset(); return T;
  }
  return check_dbe_open_flags_to_list(flags);
}
/* get the flags
 can trigger GC */
static object dbe_get_flags_list (DB_ENV *dbe) {
  u_int32_t count = 0, flags;
  SYSCALL(dbe->get_flags,(dbe,&flags));
  if (flags & DB_YIELDCPU) { pushSTACK(`:YIELDCPU`); count++; }
  if (flags & DB_TXN_WRITE_NOSYNC) { pushSTACK(`:TXN-WRITE-NOSYNC`);count++; }
  if (flags & DB_TXN_NOSYNC) { pushSTACK(`:TXN-NOSYNC`); count++; }
  if (flags & DB_REGION_INIT) { pushSTACK(`:REGION-INIT`); count++; }
  if (flags & DB_PANIC_ENVIRONMENT) {pushSTACK(`:PANIC-ENVIRONMENT`);count++;}
  if (flags & DB_OVERWRITE) { pushSTACK(`:OVERWRITE`); count++; }
  if (flags & DB_NOPANIC) { pushSTACK(`:NOPANIC`); count++; }
  if (flags & DB_NOMMAP) { pushSTACK(`:NOMMAP`); count++; }
  if (flags & DB_NOLOCKING) { pushSTACK(`:NOLOCKING`); count++; }
#if defined(DB_LOG_INMEMORY)
  if (flags & DB_LOG_INMEMORY) { pushSTACK(`:LOG-INMEMORY`); count++; }
#endif
  if (flags & DB_LOG_AUTOREMOVE) { pushSTACK(`:LOG-AUTOREMOVE`); count++; }
#if defined(DB_DSYNC_LOG)
  if (flags & DB_DSYNC_LOG) { pushSTACK(`:DSYNC-LOG`); count++; }
#endif
  if (flags & DB_DIRECT_LOG) { pushSTACK(`:DIRECT-LOG`); count++; }
  if (flags & DB_CDB_ALLDB) { pushSTACK(`:CDB-ALLDB`); count++; }
  if (flags & DB_AUTO_COMMIT) { pushSTACK(`:AUTO-COMMIT`); count++; }
  SYSCALL(dbe->get_encrypt_flags,(dbe,&flags));
  switch (flags) {
    case DB_ENCRYPT_AES: pushSTACK(`:ENCRYPT-AES`); count++; break;
    case 0: break;
    default: NOTREACHED;
  }
  return listof(count);
}
#define DEFINE_GETTER1(handle,handle_type,getter,type,finish)   \
  static object handle##_##getter (handle_type* handle) {       \
    type value;                                                 \
    SYSCALL(handle->getter,(handle,&value));                    \
    return finish;                                              \
  }
#define DEFINE_DBE_GETTER1(g,t,f)   DEFINE_GETTER1(dbe,DB_ENV,g,t,f)

DEFINE_DBE_GETTER1(get_tas_spins,u_int32_t,UL_to_I(value))
DEFINE_DBE_GETTER1(get_shm_key,long,value >= 0 ? fixnum(value) : NIL)
DEFINE_DBE_GETTER1(get_lk_detect,u_int32_t,check_lk_detect_reverse(value))
DEFINE_DBE_GETTER1(get_lk_max_lockers,u_int32_t,UL_to_I(value))
DEFINE_DBE_GETTER1(get_lk_max_locks,u_int32_t,UL_to_I(value))
DEFINE_DBE_GETTER1(get_lk_max_objects,u_int32_t,UL_to_I(value))
DEFINE_DBE_GETTER1(get_lg_bsize,u_int32_t,UL_to_I(value))
DEFINE_DBE_GETTER1(get_lg_dir,const char *,
                   asciz_to_string0(value,GLO(pathname_encoding)))
DEFINE_DBE_GETTER1(get_lg_max,u_int32_t,UL_to_I(value))
DEFINE_DBE_GETTER1(get_lg_regionmax,u_int32_t,UL_to_I(value))
DEFINE_DBE_GETTER1(get_tmp_dir,const char *,
                   asciz_to_string0(value,GLO(pathname_encoding)))
DEFINE_DBE_GETTER1(get_tx_max,u_int32_t,UL_to_I(value))
DEFINE_DBE_GETTER1(get_tx_timestamp,time_t,convert_time_to_universal(&value))

/* get timeout values for locks or transactions in the database environment */
static object dbe_get_timeout (DB_ENV *dbe, u_int32_t which) {
  db_timeout_t timeout;
  SYSCALL(dbe->get_timeout,(dbe,&timeout,which));
  return UL_to_I(timeout);
}
/* both timeouts as a list
 can trigger GC */
static object dbe_get_timeouts (DB_ENV *dbe) {
  pushSTACK(dbe_get_timeout(dbe,DB_SET_LOCK_TIMEOUT));
  pushSTACK(dbe_get_timeout(dbe,DB_SET_TXN_TIMEOUT));
  return listof(2);
}
/* conflicts table as a bit matrix
 can trigger GC */
static object dbe_get_lk_conflicts (DB_ENV *dbe) {
  int nmodes;
  const u_int8_t *conflicts;
  SYSCALL(dbe->get_lk_conflicts,(dbe,&conflicts,&nmodes));
  pushSTACK(fixnum(nmodes)); pushSTACK(fixnum(nmodes));
  value1 = listof(2); pushSTACK(value1);
  pushSTACK(S(Kelement_type)); pushSTACK(GLO(strmtype_ubyte8));
  funcall(L(make_array),3);
  {
    uintL size = nmodes*nmodes, index = 0;
    object data = array_displace_check(value1,size,&index);
    begin_system_call();
    memcpy(TheSbvector(data)->data+index,conflicts,size);
    end_system_call();
  }
  return value1;
}
FLAG_EXTRACTOR(dbe_get_flags_num,DB_ENV*)
DEFUNR(BDB:DBE-GET-OPTIONS, dbe &optional what) {
  object what = STACK_0;
  /* dbe may be NULL only for DB_XIDDATASIZE */
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_1,`BDB::DBE`,
                                    eq(what,`:DB-XIDDATASIZE`)
                                    ? BH_NIL_IS_NULL : BH_VALID);
  what = STACK_0; skipSTACK(2);
 restart_DBE_GET_OPTIONS:
  if (missingp(what)) {         /* get everything */
    uintL count = 0;
    pushSTACK(`:VERBOSE`); value1 = dbe_get_verbose(dbe);
    pushSTACK(value1); count++;
    pushSTACK(`:FLAGS`); value1 = dbe_get_flags_list(dbe);
    pushSTACK(value1); count++;
    pushSTACK(`:TIMESTAMP`); pushSTACK(dbe_get_tx_timestamp(dbe)); count++;
    pushSTACK(`:TX-MAX`); pushSTACK(dbe_get_tx_max(dbe)); count++;
    pushSTACK(`:TMP-DIR`); pushSTACK(dbe_get_tmp_dir(dbe)); count++;
    pushSTACK(`:DATA-DIR`); value1 = dbe_get_data_dirs(dbe);
    pushSTACK(value1); count++;
    pushSTACK(`:TAS-SPINS`); pushSTACK(dbe_get_tas_spins(dbe)); count++;
    pushSTACK(`:SHM-KEY`); pushSTACK(dbe_get_shm_key(dbe)); count++;
    pushSTACK(`:ERRPFX`); pushSTACK(dbe_get_errpfx(dbe)); count++;
    pushSTACK(`:ERRFILE`); pushSTACK(dbe_get_errfile(dbe)); count++;
    pushSTACK(`:MSGFILE`); pushSTACK(dbe_get_msgfile(dbe)); count++;
    pushSTACK(`:TIMEOUT`); value1 = dbe_get_timeouts(dbe);
    pushSTACK(value1); count++;
    pushSTACK(`:LG-BSIZE`); pushSTACK(dbe_get_lg_bsize(dbe)); count++;
    pushSTACK(`:LG-DIR`); pushSTACK(dbe_get_lg_dir(dbe)); count++;
    pushSTACK(`:LG-MAX`); pushSTACK(dbe_get_lg_max(dbe)); count++;
    pushSTACK(`:LG-REGIONMAX`); pushSTACK(dbe_get_lg_regionmax(dbe)); count++;
    pushSTACK(`:LK-CONFLICTS`); pushSTACK(dbe_get_lk_conflicts(dbe)); count++;
    pushSTACK(`:LK-DETECT`); pushSTACK(dbe_get_lk_detect(dbe)); count++;
    pushSTACK(`:LK-MAX-LOCKERS`);pushSTACK(dbe_get_lk_max_lockers(dbe));count++;
    pushSTACK(`:LK-MAX-LOCKS`); pushSTACK(dbe_get_lk_max_locks(dbe)); count++;
    pushSTACK(`:LK-MAX-OBJECTS`);pushSTACK(dbe_get_lk_max_objects(dbe));count++;
    pushSTACK(`:HOMEDIR`); pushSTACK(dbe_get_home_dir(dbe,false)); count++;
    pushSTACK(`:OPEN`); value1 = dbe_get_open_flags(dbe,false);
    pushSTACK(value1); count++;
    VALUES1(listof(count*2));
  } else if (eq(what,S(Kverbose))) {
    VALUES1(dbe_get_verbose(dbe));
  } else if (eq(what,`:FLAGS`)) {
    VALUES1(dbe_get_flags_list(dbe));
  } else if (eq(what,`:VERB-WAITSFOR`)) {
    int onoffp;
    SYSCALL(dbe->get_verbose,(dbe,DB_VERB_WAITSFOR,&onoffp));
    VALUES_IF(onoffp);
  } else if (eq(what,`:VERB-REPLICATION`)) {
    int onoffp;
    SYSCALL(dbe->get_verbose,(dbe,DB_VERB_REPLICATION,&onoffp));
    VALUES_IF(onoffp);
  } else if (eq(what,`:VERB-RECOVERY`)) {
    int onoffp;
    SYSCALL(dbe->get_verbose,(dbe,DB_VERB_RECOVERY,&onoffp));
    VALUES_IF(onoffp);
  } else if (eq(what,`:VERB-DEADLOCK`)) {
    int onoffp;
    SYSCALL(dbe->get_verbose,(dbe,DB_VERB_DEADLOCK,&onoffp));
    VALUES_IF(onoffp);
#if defined(DB_VERB_CHKPOINT)
  } else if (eq(what,`:VERB-CHKPOINT`)) {
    int onoffp;
    SYSCALL(dbe->get_verbose,(dbe,DB_VERB_CHKPOINT,&onoffp));
    VALUES_IF(onoffp);
#endif
  } else if (eq(what,`:YIELDCPU`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_YIELDCPU);
  } else if (eq(what,`:TXN-WRITE-NOSYNC`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_TXN_WRITE_NOSYNC);
  } else if (eq(what,`:TXN-NOSYNC`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_TXN_NOSYNC);
  } else if (eq(what,`:REGION-INIT`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_REGION_INIT);
  } else if (eq(what,`:PANIC-ENVIRONMENT`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_PANIC_ENVIRONMENT);
  } else if (eq(what,`:OVERWRITE`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_OVERWRITE);
  } else if (eq(what,`:NOPANIC`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_NOPANIC);
  } else if (eq(what,`:NOMMAP`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_NOMMAP);
  } else if (eq(what,`:NOLOCKING`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_NOLOCKING);
#if defined(DB_LOG_INMEMORY)
  } else if (eq(what,`:LOG-INMEMORY`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_LOG_INMEMORY);
#endif
  } else if (eq(what,`:LOG-AUTOREMOVE`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_LOG_AUTOREMOVE);
#if defined(DB_DSYNC_LOG)
  } else if (eq(what,`:DSYNC-LOG`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_DSYNC_LOG);
#endif
  } else if (eq(what,`:DIRECT-LOG`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_DIRECT_LOG);
  } else if (eq(what,`:CDB-ALLDB`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_CDB_ALLDB);
  } else if (eq(what,`:AUTO-COMMIT`)) {
    VALUES_IF(dbe_get_flags_num(dbe) & DB_AUTO_COMMIT);
  } else if (eq(what,`:LG-BSIZE`)) {
    VALUES1(dbe_get_lg_bsize(dbe));
  } else if (eq(what,`:LG-DIR`)) {
    VALUES1(dbe_get_lg_dir(dbe));
  } else if (eq(what,`:LG-MAX`)) {
    VALUES1(dbe_get_lg_max(dbe));
  } else if (eq(what,`:LG-REGIONMAX`)) {
    VALUES1(dbe_get_lg_regionmax(dbe));
  } else if (eq(what,`:LK-CONFLICTS`)) {
    VALUES1(dbe_get_lk_conflicts(dbe));
  } else if (eq(what,`:LK-DETECT`)) {
    VALUES1(dbe_get_lk_detect(dbe));
  } else if (eq(what,`:LK-MAX-LOCKERS`)) {
    VALUES1(dbe_get_lk_max_lockers(dbe));
  } else if (eq(what,`:LK-MAX-LOCKS`)) {
    VALUES1(dbe_get_lk_max_locks(dbe));
  } else if (eq(what,`:LK-MAX-OBJECTS`)) {
    VALUES1(dbe_get_lk_max_objects(dbe));
  } else if (eq(what,`:TX-TIMESTAMP`)) {
    VALUES1(dbe_get_tx_timestamp(dbe));
  } else if (eq(what,`:TX-MAX`)) {
    VALUES1(dbe_get_tx_max(dbe));
  } else if (eq(what,`:DATA-DIR`)) {
    VALUES1(dbe_get_data_dirs(dbe));
  } else if (eq(what,`:TMP-DIR`)) {
    VALUES1(dbe_get_tmp_dir(dbe));
  } else if (eq(what,`:TAS-SPINS`)) {
    VALUES1(dbe_get_tas_spins(dbe));
  } else if (eq(what,`:SHM-KEY`)) {
    VALUES1(dbe_get_shm_key(dbe));
  } else if (eq(what,`:LOCK-TIMEOUT`)) {
    VALUES1(dbe_get_timeout(dbe,DB_SET_LOCK_TIMEOUT));
  } else if (eq(what,`:TXN-TIMEOUT`)) {
    VALUES1(dbe_get_timeout(dbe,DB_SET_TXN_TIMEOUT));
  } else if (eq(what,`:TIMEOUT`)) {
    VALUES1(dbe_get_timeouts(dbe));
  } else if (eq(what,`:ENCRYPT`)) {
    u_int32_t flags;
    SYSCALL(dbe->get_encrypt_flags,(dbe,&flags));
    switch (flags) {
      case DB_ENCRYPT_AES: VALUES1(`:ENCRYPT-AES`);
      case 0: VALUES1(NIL);
      default: NOTREACHED;
    }
  } else if (eq(what,`:ERRPFX`)) {
    VALUES1(dbe_get_errpfx(dbe));
  } else if (eq(what,`:ERRFILE`)) {
    VALUES1(dbe_get_errfile(dbe));
  } else if (eq(what,`:MSGFILE`)) {
    VALUES1(dbe_get_msgfile(dbe));
  } else if (eq(what,`:DB-XIDDATASIZE`)) {
    VALUES1(fixnum(DB_XIDDATASIZE));
  } else if (eq(what,`:HOME`)) {
    VALUES1(dbe_get_home_dir(dbe,true));
  } else if (eq(what,`:OPEN`)) {
    VALUES1(dbe_get_open_flags(dbe,true));
  } else {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(what); pushSTACK(TheSubr(subr_self)->name);
    check_value(error,GETTEXT("~S: invalid argument ~S"));
    what = value1;
    goto restart_DBE_GET_OPTIONS;
  }
}

/* ===== Database Operations ===== */

/* not exported:
 DB->associate	Associate a secondary index
 DB->err	Error message with error string
 DB->errx	Error message
*/

DEFUN(BDB:DB-CREATE, dbe &key :XA)
{ /* create database */
  u_int32_t flags = missingp(STACK_0) ? 0 : DB_XA_CREATE;
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_1,`BDB::DBE`,BH_NIL_IS_NULL);
  DB *db;
  SYSCALL(db_create,(&db,dbe,flags));
  if (!dbe) {                   /* set error callback */
    begin_system_call();
    db->set_errcall(db,&error_callback);
    end_system_call();
  }
  wrap_finalize(db,STACK_1,`BDB::MKDB`,``BDB::DB-CLOSE``);
  skipSTACK(2);
}

DEFUN(BDB:DB-CLOSE, db &key :NOSYNC)
{ /* Close a database */
  u_int32_t flags = missingp(STACK_0) ? 0 : DB_NOSYNC;
  DB *db = (DB*)bdb_handle(STACK_1,`BDB::DB`,BH_INVALIDATE);
  if (db) {
    bool orphan_p = nullp(Parents(STACK_1));
    pushSTACK(STACK_1); funcall(`BDB::KILL-HANDLE`,1);
    if (orphan_p) {
      close_errfile(db->dbenv);
      close_errpfx(db->dbenv);
      close_msgfile(db->dbenv);
    }
    SYSCALL(db->close,(db,flags));
    VALUES1(T);
  } else VALUES1(NIL);
  skipSTACK(2);
}

/* zero-out DBT and set allocation */
static void init_dbt (DBT* p_dbt , u_int32_t flags) {
  begin_system_call(); memset(p_dbt,0,sizeof(DBT)); end_system_call();
  p_dbt->flags = flags;
}

/* ensure that the return value is a byte vector of specified length
 can trigger GC */
static object check_byte_vector_len (object obj, int length) {
 check_byte_vector_len_restart:
  obj = check_byte_vector(obj);
  if (length > 0 &&  length!=vector_length(obj)) {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(fixnum(length)); pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(error,GETTEXT("~S: byte vector ~S should have length ~S"));
    obj = value1;
    goto check_byte_vector_len_restart;
  }
  return obj;
}

DEFCHECKER(check_dbt_type,enum=dbt_o_t,default=DBT_RAW,prefix=DBT,\
           RAW STRING INTEGER)
/* check that the argument can be converted to a DBT
 can trigger GC */
static object check_dbt_object (object obj) {
  while (!bit_vector_p(Atype_8Bit,obj) && !stringp(obj)
         && !(integerp(obj) && positivep(obj))) {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(obj);             /* TYPE-ERROR slot DATUM */
    pushSTACK(`(OR STRING (INTEGER 0) (VECTOR (UNSIGNED-BYTE)))`);/*EXPECTED-TYPE*/
    pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is neither string, nonnegative integer nor byte vector"));
    obj = value1;
  }
  return obj;
}

#if SIZEOF_DB_RECNO_T == 8
# define db_recno_p(i)     uint64_p(i)
# define db_recno_to_I(r)  uint64_to_I(r)
# define I_to_db_recno(i)  I_to_uint64(check_uint64(i))
#elif SIZEOF_DB_RECNO_T == 4
# define db_recno_p(i)     uint32_p(i)
# define db_recno_to_I(r)  uint32_to_I(r)
# define I_to_db_recno(i)  I_to_uint32(check_uint32(i))
#else
# error "db_recno_t size if unknown!"
#endif
/* fill a DBT with contents of obj (a byte vector, string, or positive integer)
 dbt_type, when non-NULL, will contain the argument type
 when re_len is > 0, the database has fixed-record-length,
  so the integer will be padded on the left
  if re_len = -1, this is a key and it will be required to be a db_recno_t
   <http://www.sleepycat.com/docs/ref/am_conf/logrec.html>:
     In all cases for the Queue and Recno access methods, and when
     calling the Btree access method using the DB->get and
     DBcursor->c_get methods with the DB_SET_RECNO flag specified, the
     data field of the key DBT must be a pointer to a memory location of
     type db_recno_t <...>. The size field of the key DBT should be the
     size of that type (for example, "sizeof(db_recno_t)" in the C
     programming language). The db_recno_t type is a 32-bit unsigned
     type, which limits the number of logical records in a Queue or
     Recno database, and the maximum logical record which may be
     directly retrieved from a Btree database, to 4,294,967,295.
 can trigger GC */
static dbt_o_t fill_dbt (object obj, DBT* key, int re_len)
{
 restart_fill_dbt:
  obj = check_dbt_object(obj);
  init_dbt(key,DB_DBT_MALLOC);
  if (re_len==-1) {
    db_recno_t value = I_to_db_recno(obj);
    key->data = my_malloc(key->ulen = key->size = sizeof(db_recno_t));
    *(db_recno_t*)key->data = value;
    return DBT_INTEGER;
  } else if (stringp(obj)) {
    with_string_0(obj,GLO(misc_encoding),linez, {
        key->ulen = key->size = linez_bytelen;
        key->data = my_malloc(linez_bytelen);
        begin_system_call();
        memcpy(key->data,linez,linez_bytelen);
        end_system_call();
      });
    return DBT_STRING;
  } else if (bit_vector_p(Atype_8Bit,obj)) {
    uintL idx = 0;
    void *data_start;
    key->ulen = key->size = vector_length(obj);
    obj = array_displace_check(obj,key->size,&idx);
    data_start = TheSbvector(obj)->data + idx;
    handle_fault_range(PROT_READ,(aint)data_start,(aint)data_start + key->size);
    key->data = my_malloc(key->size);
    begin_system_call();
    memcpy(key->data,data_start,key->size);
    end_system_call();
    return DBT_RAW;
  } else if (integerp(obj)) { /* known to be positive from check_dbt_object() */
    unsigned long bitsize = 1+I_integer_length(obj); /* an extra sign bit */
    unsigned long bytesize = ceiling(bitsize,8);
    if (re_len) {
      if (bytesize > re_len) {
        pushSTACK(fixnum(bytesize)); pushSTACK(fixnum(re_len));
        pushSTACK(obj); pushSTACK(TheSubr(subr_self)->name);
        check_value(error,GETTEXT("~S: ~S does not fit into ~S bytes (it requires at least ~S bytes)"));
        obj = value1;
        goto restart_fill_dbt;
      } else bytesize = re_len;
    }
    key->ulen = key->size = bytesize;
    key->data = my_malloc(bytesize);
    if (I_to_LEbytes(obj,8*bytesize,(uintB*)key->data))
      NOTREACHED;               /* there must not be an overflow! */
#  if defined(DEBUG)
    ASSERT(eql(LEbytes_to_I(bytesize,(uintB*)key->data),obj));
#  endif
    return DBT_INTEGER;
  } else NOTREACHED;
}
static void free_dbt(DBT* dbt) {
  begin_system_call(); FREE_RESET(dbt->data); end_system_call();
}

/* convert a DBT to an object of the specified type
 p_dbt->data is free()d and set to NULL
 can trigger GC */
static object dbt_to_object (DBT *p_dbt, dbt_o_t type, int key_type) {
  if (p_dbt->data == NULL) return NIL;
  switch (type) {
    case DBT_RAW: {
      object vec = allocate_bit_vector(Atype_8Bit,p_dbt->size);
      void* data = TheSbvector(vec)->data;
      handle_fault_range(PROT_READ_WRITE,(aint)data,(aint)data + p_dbt->size);
      begin_system_call();
      memcpy(data,p_dbt->data,p_dbt->size);
      free(p_dbt->data); p_dbt->data = NULL;
      end_system_call();
      return vec;
    }
    case DBT_STRING: {
      object s = n_char_to_string((char*)p_dbt->data,p_dbt->size,
                                  GLO(misc_encoding));
      free_dbt(p_dbt);
      return s;
    }
    case DBT_INTEGER:
      if (key_type == -1) {
        if (p_dbt->size == sizeof(db_recno_t)) {
          db_recno_t res = *(db_recno_t*)p_dbt->data;
          free_dbt(p_dbt);
          return db_recno_to_I(res);
        } else {
          pushSTACK(`BDB::BDB-ERROR`);  /* error type */
          pushSTACK(`:ERRNO`); pushSTACK(Fixnum_0);
          pushSTACK(`"~S: bad logical record number size: ~S should be ~S"`);
          pushSTACK(TheSubr(subr_self)->name);
          pushSTACK(uint32_to_I(p_dbt->size));
          pushSTACK(uint32_to_I(sizeof(db_recno_t)));
          funcall(L(error_of_type),7);
        }
      } else {
        object ret = LEbytes_to_I(p_dbt->size,(uintB*)p_dbt->data);
#      if defined(DEBUG)
        uintB *tmp = (uintB*)alloca(p_dbt->size);
        memset(tmp,0,p_dbt->size);
        I_to_LEbytes(ret,1+I_integer_length(ret),tmp);
        ASSERT(!strncmp(tmp,p_dbt->data,p_dbt->size));
#      endif
        free_dbt(p_dbt);
        return ret;
      }
    default: NOTREACHED;
  }
}

/* extract record length, not errors */
static u_int32_t record_length (DB *db) {
  u_int32_t ret;
  int status;
  begin_system_call();
  status = db->get_re_len(db,&ret);
  end_system_call();
  if (status) {
    error_message_reset();
    return 0;
  } else return ret;
}
/* check whether the DB uses logical record numbers, see
 <http://www.sleepycat.com/docs/ref/am_conf/logrec.html> */
static inline int db_key_type (DB *db, u_int32_t action) {
  DBTYPE db_type;
  SYSCALL(db->get_type,(db,&db_type));
  switch (db_type) {
    case DB_QUEUE: case DB_RECNO: return -1;
    case DB_BTREE: switch (action) {
      case DB_GET_RECNO: case DB_SET_RECNO: return -1;
      default: return 0;
    }
    default: return 0;
  }
}

DEFUN(BDB:DB-DEL, dbe key &key :TRANSACTION :AUTO-COMMIT)
{ /* Delete items from a database */
  u_int32_t flags = bdb_ac_flags();
  DB_TXN *txn = (DB_TXN*)bdb_handle(popSTACK(),`BDB::TXN`,BH_NIL_IS_NULL);
  DB *db = (DB*)bdb_handle(STACK_1,`BDB::DB`,BH_VALID);
  DBT key;
  fill_dbt(STACK_0,&key,db_key_type(db,0));
  SYSCALL1(db->del,(db,txn,&key,flags),{free(key.data);});
  skipSTACK(2);
  VALUES0;
}

DEFUN(BDB:DB-FD, db)
{ /* Return a file descriptor from a database */
  DB *db = (DB*)bdb_handle(popSTACK(),`BDB::DB`,BH_VALID);
  int fd;
  SYSCALL(db->fd,(db,&fd));
  VALUES1(fixnum(fd));
}

DEFCHECKER(db_get_action,prefix=DB, default=0, \
           CONSUME CONSUME-WAIT GET-BOTH SET-RECNO)
DEFFLAGSET(db_get_options, DB_AUTO_COMMIT DB_DEGREE_2 DB_READ_COMMITTED \
           DB_DIRTY_READ DB_READ_UNCOMMITTED DB_MULTIPLE DB_RMW)
DEFUN(BDB:DB-GET, db key &key :ACTION :AUTO-COMMIT :DEGREE-2 :READ-COMMITTED \
      :DIRTY-READ :READ-UNCOMMITTED :MULTIPLE :RMW :TRANSACTION :ERROR :TYPE \
      :KEY-TYPE)
{ /* Get items from a database */
  dbt_o_t key_type = check_dbt_type(popSTACK());
  dbt_o_t out_type = check_dbt_type(popSTACK());
  int no_error = nullp(popSTACK());
  DB_TXN *txn = (DB_TXN*)bdb_handle(popSTACK(),`BDB::TXN`,BH_NIL_IS_NULL);
  u_int32_t flags = db_get_options();
  u_int32_t action = db_get_action(popSTACK());
  DB *db = (DB*)bdb_handle(STACK_1,`BDB::DB`,BH_VALID);
  DBT key, val;
  int status;
  fill_dbt(STACK_0,&key,db_key_type(db,action));
  init_dbt(&val,DB_DBT_MALLOC);
  skipSTACK(2);
  begin_system_call();
  status = db->get(db,txn,&key,&val,flags | action);
  end_system_call();
  if (status) {
    free_dbt(&key);
    if (no_error) {
      switch (status) {
        case DB_NOTFOUND: VALUES1(`:NOTFOUND`); error_message_reset(); return;
        case DB_KEYEMPTY: VALUES1(`:KEYEMPTY`); error_message_reset(); return;
      }
    }
    error_bdb(status,"db->get");
  }
  if (action == DB_SET_RECNO) {
    pushSTACK(dbt_to_object(&key,key_type,0));
    value2 = dbt_to_object(&val,out_type,0);
    value1 = popSTACK();
    mv_count = 2;
  } else
    VALUES1(dbt_to_object(&val,out_type,0));
}

DEFCHECKER(check_dbtype,enum=DBTYPE,default=DB_UNKNOWN,prefix=DB, \
           UNKNOWN BTREE HASH QUEUE RECNO)
DEFUN(BDB:DB-STAT, db &key :FAST-STAT :TRANSACTION)
{ /* Return database statistics */
  DB_TXN *txn = (DB_TXN*)bdb_handle(popSTACK(),`BDB::TXN`,BH_NIL_IS_NULL);
#if defined(HAVE_DB_STAT_ACCEPT_TXN)
# define DB_STAT(s)   SYSCALL(db->stat,(db,txn,&s,flags));
#else
# define DB_STAT(s)   SYSCALL(db->stat,(db,&s,flags));
#endif
  u_int32_t flags = missingp(STACK_0) ? 0 : DB_FAST_STAT;
  DB *db = (DB*)bdb_handle(STACK_1,`BDB::DB`,BH_VALID);
  int swapped_p;
  DBTYPE db_type;
  unsigned int count = 0;
  SYSCALL(db->get_type,(db,&db_type));
  pushSTACK(check_dbtype_reverse(db_type)); count++;
  SYSCALL(db->get_byteswapped,(db,&swapped_p));
  pushSTACK(swapped_p ? T : NIL); count++;
  switch (db_type) {
#define STAT_SLOT(slot)        pushSTACK(UL_to_I(slot)); count++
#define STAT_SLOT_FAST(slot)   pushSTACK(flags ? NIL : UL_to_I(slot)); count++
    case DB_HASH: {
      DB_HASH_STAT *hash_stat;
      DB_STAT(hash_stat);
      STAT_SLOT(hash_stat->hash_magic);
      STAT_SLOT(hash_stat->hash_version);
      STAT_SLOT(hash_stat->hash_nkeys);
      STAT_SLOT(hash_stat->hash_ndata);
      STAT_SLOT(hash_stat->hash_pagesize);
      STAT_SLOT(hash_stat->hash_ffactor);
      STAT_SLOT(hash_stat->hash_buckets);
      STAT_SLOT_FAST(hash_stat->hash_free);
      STAT_SLOT_FAST(hash_stat->hash_bfree);
      STAT_SLOT_FAST(hash_stat->hash_bigpages);
      STAT_SLOT_FAST(hash_stat->hash_big_bfree);
      STAT_SLOT_FAST(hash_stat->hash_overflows);
      STAT_SLOT_FAST(hash_stat->hash_ovfl_free);
      STAT_SLOT_FAST(hash_stat->hash_dup);
      STAT_SLOT_FAST(hash_stat->hash_dup_free);
      funcall(`BDB::MKDBSTAT-HASH`,count);
      begin_system_call(); free(hash_stat); end_system_call();
    } break;
    case DB_BTREE: case DB_RECNO: {
      DB_BTREE_STAT *btree_stat;
      DB_STAT(btree_stat);
      STAT_SLOT(btree_stat->bt_magic);
      STAT_SLOT(btree_stat->bt_version);
      STAT_SLOT(btree_stat->bt_nkeys);
      STAT_SLOT(btree_stat->bt_ndata);
      STAT_SLOT(btree_stat->bt_pagesize);
      STAT_SLOT(btree_stat->bt_minkey);
      STAT_SLOT(btree_stat->bt_re_len);
      STAT_SLOT(btree_stat->bt_re_pad);
      STAT_SLOT_FAST(btree_stat->bt_levels);
      STAT_SLOT_FAST(btree_stat->bt_int_pg);
      STAT_SLOT_FAST(btree_stat->bt_leaf_pg);
      STAT_SLOT_FAST(btree_stat->bt_dup_pg);
      STAT_SLOT_FAST(btree_stat->bt_over_pg);
      STAT_SLOT_FAST(btree_stat->bt_free);
      STAT_SLOT_FAST(btree_stat->bt_int_pgfree);
      STAT_SLOT_FAST(btree_stat->bt_leaf_pgfree);
      STAT_SLOT_FAST(btree_stat->bt_dup_pgfree);
      STAT_SLOT_FAST(btree_stat->bt_over_pgfree);
      funcall(`BDB::MKDBSTAT-BTREE`,count);
      begin_system_call(); free(btree_stat); end_system_call();
    } break;
    case DB_QUEUE: {
      DB_QUEUE_STAT *queue_stat;
      DB_STAT(queue_stat);
      STAT_SLOT(queue_stat->qs_magic);
      STAT_SLOT(queue_stat->qs_version);
      STAT_SLOT(queue_stat->qs_nkeys);
      STAT_SLOT(queue_stat->qs_ndata);
      STAT_SLOT(queue_stat->qs_pagesize);
      STAT_SLOT(queue_stat->qs_extentsize);
      STAT_SLOT_FAST(queue_stat->qs_pages);
      STAT_SLOT(queue_stat->qs_re_len);
      STAT_SLOT(queue_stat->qs_re_pad);
      STAT_SLOT_FAST(queue_stat->qs_pgfree);
      STAT_SLOT(queue_stat->qs_first_recno);
      STAT_SLOT(queue_stat->qs_cur_recno);
      funcall(`BDB::MKDBSTAT-QUEUE`,count);
      begin_system_call(); free(queue_stat); end_system_call();
    } break;
    default: NOTREACHED;
#undef STAT_SLOT
#undef STAT_SLOT_FAST
#undef DB_STAT
  }
  skipSTACK(2);
}

DEFFLAGSET(db_open_flags, DB_CREATE DB_DIRTY_READ DB_EXCL DB_NOMMAP \
           DB_RDONLY DB_THREAD DB_TRUNCATE DB_AUTO_COMMIT)
DEFCHECKER(check_db_open_flags,prefix=DB,default=0,bitmasks=both,type=uint32_t,\
           CREATE DIRTY-READ EXCL NOMMAP RDONLY THREAD TRUNCATE AUTO-COMMIT)
DEFUN(BDB:DB-OPEN, db file &key :DATABASE :TYPE :MODE :FLAGS      \
      :CREATE :DIRTY-READ :EXCL :NOMMAP :RDONLY :THREAD :TRUNCATE \
      :AUTO-COMMIT :TRANSACTION)
{ /* Open a database */
  DB_TXN *txn = (DB_TXN*)bdb_handle(popSTACK(),`BDB::TXN`,BH_NIL_IS_NULL);
  u_int32_t flags = db_open_flags() | check_db_open_flags_from_list(popSTACK());
  int mode = check_uint_defaulted(popSTACK(),0644);
  DBTYPE db_type = check_dbtype(popSTACK());
  DB *db = (DB*)bdb_handle(STACK_2,`BDB::DB`,BH_VALID);
  /* string is resolved by Berkeley-DB relative to data_dirs */
  with_string_0((stringp(STACK_1) ? (object)STACK_1
                 : physical_namestring(STACK_1)),
                GLO(pathname_encoding),file, {
      if (missingp(STACK_0)) {  /* no :DATABASE */
        SYSCALL(db->open,(db,txn,file,NULL,db_type,flags,mode));
      } else                    /* multiple databases in one file */
        with_string_0(check_string(STACK_0),GLO(misc_encoding),databse, {
            SYSCALL(db->open,(db,txn,file,databse,db_type,flags,mode));
          });
    });
  VALUES0;
  skipSTACK(3);
}

DEFUN(BDB:DB-SYNC, db)
{ /* Flush a database to stable storage */
  DB *db = (DB*)bdb_handle(popSTACK(),`BDB::DB`,BH_VALID);
  SYSCALL(db->sync,(db,0));
  VALUES0;
}

DEFUN(BDB:DB-TRUNCATE, db &key :TRANSACTION :AUTO-COMMIT)
{ /* Empty a database */
  u_int32_t flags = bdb_ac_flags();
  DB_TXN *txn = (DB_TXN*)bdb_handle(popSTACK(),`BDB::TXN`,BH_NIL_IS_NULL);
  DB *db = (DB*)bdb_handle(popSTACK(),`BDB::DB`,BH_VALID);
  u_int32_t count;
  SYSCALL(db->truncate,(db,txn,&count,flags));
  VALUES1(UL_to_I(count));
}

DEFUN(BDB:DB-UPGRADE, db file &key :DUPSORT)
{ /* Upgrade a database */
  u_int32_t flags = (missingp(STACK_0) ? 0 : DB_DUPSORT);
  DB *db = (DB*)bdb_handle(STACK_2,`BDB::DB`,BH_VALID);
  with_string_0(physical_namestring(STACK_1),GLO(pathname_encoding),file, {
      SYSCALL(db->upgrade,(db,file,flags));
    });
  VALUES0; skipSTACK(3);
}

DEFUN(BDB:DB-RENAME, db file database newname)
{ /* Rename a database */
  DB *db = (DB*)bdb_handle(STACK_3,`BDB::DB`,BH_VALID);
  with_string_0(physical_namestring(STACK_2),GLO(pathname_encoding),file, {
      with_string_0(check_string(STACK_0),GLO(misc_encoding),newname, {
          if (nullp(STACK_1))   /* database=NIL */
            SYSCALL(db->rename,(db,file,NULL,newname,0));
          else
            with_string_0(check_string(STACK_1),GLO(misc_encoding),database, {
                SYSCALL(db->rename,(db,file,database,newname,0));
              });
        });
    });
  VALUES0; skipSTACK(4);
}

DEFUN(BDB:DB-REMOVE, db file database)
{ /* Remove a database */
  DB *db = (DB*)bdb_handle(STACK_2,`BDB::DB`,BH_VALID);
  with_string_0(physical_namestring(STACK_1),GLO(pathname_encoding),file, {
      with_string_0(check_string(STACK_0),GLO(misc_encoding),database, {
          SYSCALL(db->remove,(db,file,database,0));
        });
    });
  VALUES0; skipSTACK(3);
}

DEFCHECKER(db_put_action,prefix=DB, default=0, APPEND NODUPDATA NOOVERWRITE)
DEFUN(BDB:DB-PUT, db key val &key :AUTO-COMMIT :ACTION :TRANSACTION)
{ /* Store items into a database */
  DB_TXN *txn = (DB_TXN*)bdb_handle(popSTACK(),`BDB::TXN`,BH_NIL_IS_NULL);
  u_int32_t action = db_put_action(popSTACK());
  u_int32_t flags = bdb_ac_flags();
  DB *db = (DB*)bdb_handle(STACK_2,`BDB::DB`,BH_VALID);
  DBT key, val;
  fill_dbt(STACK_0,&val,record_length(db));
  if (action == DB_APPEND) {     /* DB is :QUEUE or :RECNO */
    init_dbt(&key,DB_DBT_MALLOC); /* key is ignored - it will be returned */
    SYSCALL1(db->put,(db,txn,&key,&val,action | flags),{free(val.data);});
    VALUES1(dbt_to_object(&key,DBT_INTEGER,db_key_type(db,0)));
  } else {
    fill_dbt(STACK_1,&key,db_key_type(db,0));
    switch (action) {
      case DB_NODUPDATA: case DB_NOOVERWRITE: {
        int status;
        begin_system_call();
        status = db->put(db,txn,&key,&val,action | flags);
        free(val.data); free(key.data);
        end_system_call();
        switch (status) {
          case 0: VALUES0; break;
          case DB_KEYEXIST: VALUES1(`:KEYEXIST`); error_message_reset(); break;
          default: error_bdb(status,"db->put");
        }
        break;
      }
      default:
        SYSCALL1(db->put,(db,txn,&key,&val,action | flags),
                 {free(val.data);free(key.data);});
        VALUES0;
    }
  }
  skipSTACK(3);                 /* drop db key val */
}

DEFFLAGSET(db_join_flags, DB_JOIN_NOSORT)
DEFUN(BDB:DB-JOIN, db cursors &key :JOIN-NOSORT)
{ /* create a specialized join cursor */
  u_int32_t flags = db_join_flags(), length, pos;
  DB *db = (DB*)bdb_handle(STACK_1,`BDB::DB`,BH_VALID);
  DBC **curslist, *dbc;
  pushSTACK(STACK_0); funcall(L(length),1); length = posfixnum_to_V(value1);
  curslist = (DBC**)alloca((1+length)*sizeof(DBC*));
  if (curslist == NULL) {
    pushSTACK(TheSubr(subr_self)->name);
    fehler(storage_condition,GETTEXT("~S: alloca() failed"));
  }
  curslist[length] = 0;
  if (listp(STACK_0)) {         /* list */
    for (pos=0; pos<length; pos++, STACK_0 = Cdr(STACK_0))
      curslist[pos] = (DBC*)bdb_handle(Car(STACK_0),`BDB::DBC`,BH_VALID);
  } else {                      /* vector */
    for (pos=0; pos<length; pos++) {
      pushSTACK(STACK_0); pushSTACK(fixnum(pos)); funcall(L(aref),2);
      curslist[pos] = (DBC*)bdb_handle(value1,`BDB::DBC`,BH_VALID);
    }
  }
  SYSCALL(db->join,(db,curslist,&dbc,flags));
  { /* make parents */
    object parent_list = allocate_cons();
    Car(parent_list) = STACK_1;
    STACK_1 = parent_list;
    if (listp(STACK_0)) {       /*  list */
      Cdr(STACK_1) = copy_list(STACK_0);
    } else {                    /* vector */
      pushSTACK(STACK_0); pushSTACK(S(list)); funcall(L(coerce),2);
      Cdr(STACK_1) = value1;
    }
  }
  wrap_finalize(dbc,STACK_1,`BDB::MKDBC`,``BDB::DBC-CLOSE``);
  skipSTACK(2);
}

DEFUN(BDB:DB-KEY-RANGE, db key &key :TRANSACTION)
{ /* return an estimate of the proportion of keys that are less than,
     equal to, and greater than the specified key. The underlying
     database must be of type Btree. */
  DB_TXN *txn = (DB_TXN*)bdb_handle(popSTACK(),`BDB::TXN`,BH_NIL_IS_NULL);
  DBT key;
  DB_KEY_RANGE key_range;
  DB *db = (DB*)bdb_handle(STACK_1,`BDB::DB`,BH_VALID);
  fill_dbt(STACK_0,&key,db_key_type(db,0));
  SYSCALL1(db->key_range,(db,txn,&key,&key_range,0),{free(key.data);});
  pushSTACK(c_double_to_DF((dfloatjanus*)&(key_range.less)));
  pushSTACK(c_double_to_DF((dfloatjanus*)&(key_range.equal)));
  pushSTACK(c_double_to_DF((dfloatjanus*)&(key_range.greater)));
  VALUES3(STACK_0,STACK_1,STACK_2); skipSTACK(5);
}

DEFFLAGSET(db_verify_flags, DB_AGGRESSIVE DB_PRINTABLE DB_NOORDERCHK)
DEFUN(BDB:DB-VERIFY, db file &key :DATABASE :SALVAGE :AGGRESSIVE :PRINTABLE \
      :NOORDERCHK)
{ /* Verify/salvage a database */
  u_int32_t flags = db_verify_flags();
  DB *db = (DB*)bdb_handle(STACK_3,`BDB::DB`,BH_VALID);
  FILE *outfile = NULL;
  int status;
  if (!missingp(STACK_0)) {     /* SALVAGE */
    outfile = my_fopen(STACK_0);
    flags |= DB_SALVAGE;
  }
  STACK_2 = physical_namestring(STACK_2); /* FILE */
  if (!missingp(STACK_1)) {               /* DATABASE */
    STACK_1 = check_string(STACK_1);
    flags |= DB_ORDERCHKONLY;
  }
  with_string_0(STACK_2,GLO(pathname_encoding),file,{
      if (!missingp(STACK_1)) {
        with_string_0(STACK_1,GLO(misc_encoding),database,{
            begin_system_call();
            status = db->verify(db,file,database,outfile,flags);
            if (outfile) fclose(outfile);
            end_system_call();
          });
      } else {
        begin_system_call();
        status = db->verify(db,file,NULL,outfile,flags);
        if (outfile) fclose(outfile);
        end_system_call();
      }
    });
  if (status) error_bdb(status,"db->verify");
  VALUES0; skipSTACK(3);
}

/* ===== Database Configuration ===== */
/* not exported:
 DB->set_alloc		Set local space allocation functions
 DB->set_dup_compare	Set a duplicate comparison function
 DB->set_errcall	Set error message callback
 DB->set_errpfx		Set error message prefix
 DB->set_feedback	Set feedback callback
 DB->set_paniccall	Set panic callback
Btree/Recno Configuration
 DB->set_append_recno	Set record append callback
 DB->set_bt_compare	Set a Btree comparison function
 DB->set_bt_prefix	Set a Btree prefix comparison function
Hash Configuration
 DB->set_h_hash	Set a hashing function
*/

/* convert a Lisp integer to a pair of (Giga-bytes bytes)
 can trigger GC */
DEFVAR(giga_byte,`#.(BYTE 30 0)`)
static void size_to_giga_bytes (object size, u_int32_t *gb, u_int32_t *by) {
  pushSTACK(size); pushSTACK(negfixnum(-30));
  pushSTACK(O(giga_byte)); pushSTACK(size);
  funcall(L(ldb),2); *by = I_to_uint32(value1);
  funcall(L(ash),2); *gb = I_to_uint32(value1);
}

/* set the password to perform encryption and decryption.
 can trigger GC */
static void db_set_encryption (DB *db, gcv_object_t *o_flags_,
                               gcv_object_t *o_password_) {
  u_int32_t flags = dbe_encryption_check(*o_flags_);
  *o_password_ = check_string(*o_password_);
  with_string_0(*o_password_,GLO(misc_encoding),password,
                { SYSCALL(db->set_encrypt,(db,password,flags)); });
}
/* get all flags as a list
 can trigger GC */
static object db_get_flags_list (DB *db) {
  u_int32_t flags;
  int count = 0;
  SYSCALL(db->get_flags,(db,&flags));
  if (flags & DB_CHKSUM) { pushSTACK(`:CHKSUM`); count++; }
  if (flags & DB_DUP) { pushSTACK(`:DUP`); count++; }
  if (flags & DB_DUPSORT) { pushSTACK(`:DUPSORT`); count++; }
  if (flags & DB_ENCRYPT) { pushSTACK(`:ENCRYPT`); count++; }
#if defined(DB_INORDER)
  if (flags & DB_INORDER) { pushSTACK(`:INORDER`); count++; }
#endif
  if (flags & DB_RECNUM) { pushSTACK(`:RECNUM`); count++; }
  if (flags & DB_RENUMBER) { pushSTACK(`:RENUMBER`); count++; }
  if (flags & DB_REVSPLITOFF) { pushSTACK(`:REVSPLITOFF`); count++; }
  if (flags & DB_SNAPSHOT) { pushSTACK(`:SNAPSHOT`); count++; }
  if (flags & DB_TXN_NOT_DURABLE) { pushSTACK(`:TXN-NOT-DURABLE`); count++; }
  return listof(count);
}
/* get the DB open flags
   return T when DB is not yet open and a list otherwise
 can trigger GC */
static object db_get_open_flags (DB *db, int errorp) {
  u_int32_t flags, status;
  begin_system_call();
  status = db->get_open_flags(db,&flags);
  end_system_call();
  if (status) {
    if (errorp) error_bdb(status,"db->get_open_flags");
    error_message_reset(); return T;
  }
  return check_db_open_flags_to_list(flags);
}

DEFUN(BDB:DB-SET-OPTIONS, db &key :MSGFILE :ERRFILE :ERRPFX :PASSWORD \
      :ENCRYPTION :NCACHE :CACHESIZE :CACHE :LORDER :PAGESIZE :BT-MINKEY \
      :H-FFACTOR :H-NELEM :Q-EXTENTSIZE :RE-DELIM :RE-LEN :RE-PAD :RE-SOURCE \
      :CHKSUM :DUP :DUPSORT :ENCRYPT :INORDER :RECNUM :RENUMBER         \
      :REVSPLITOFF :SNAPSHOT :TXN-NOT-DURABLE)
{ /* set database options */
  DB *db = (DB*)bdb_handle(STACK_(28),`BDB::DB`,BH_VALID);
  { /* flags */
    u_int32_t flags_on = 0, flags_off = 0;
    set_flags(popSTACK(),&flags_on,&flags_off,DB_TXN_NOT_DURABLE);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_SNAPSHOT);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_REVSPLITOFF);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_RENUMBER);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_RECNUM);
#  if defined(DB_INORDER)
    set_flags(popSTACK(),&flags_on,&flags_off,DB_INORDER);
#  else
    skipSTACK(1);               /*  drop :INORDER */
#  endif
    set_flags(popSTACK(),&flags_on,&flags_off,DB_ENCRYPT);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_DUPSORT);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_DUP);
    set_flags(popSTACK(),&flags_on,&flags_off,DB_CHKSUM);
    if (flags_off || flags_on) {
      u_int32_t flags;
      SYSCALL(db->get_flags,(db,&flags));
      flags |= flags_on;
      flags &= ~flags_off;
      SYSCALL(db->set_flags,(db,flags));
    }
  }
  if (boundp(STACK_0)) {        /* RE_SOURCE */
    if (nullp(STACK_0))
      SYSCALL(db->set_re_source,(db,NULL));
    else
      with_string_0(check_string(STACK_0),GLO(pathname_encoding), re_source, {
          SYSCALL(db->set_re_source,(db,re_source));
        });
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* RE_PAD */
    int re_pad = I_to_uint8(check_uint8(STACK_0));
    SYSCALL(db->set_re_pad,(db,re_pad));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* RE_LEN */
    u_int32_t re_len = I_to_uint32(check_uint32(STACK_0));
    SYSCALL(db->set_re_len,(db,re_len));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* RE_DELIM */
    int re_delim = I_to_uint8(check_uint8(STACK_0));
    SYSCALL(db->set_re_delim,(db,re_delim));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* Q_EXTENTSIZE */
    u_int32_t q_extentsize = I_to_uint32(check_uint32(STACK_0));
    SYSCALL(db->set_q_extentsize,(db,q_extentsize));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* H_NELEM */
    u_int32_t h_nelem = I_to_uint32(check_uint32(STACK_0));
    SYSCALL(db->set_h_nelem,(db,h_nelem));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* H_FFACTOR */
    u_int32_t h_ffactor = I_to_uint32(check_uint32(STACK_0));
    SYSCALL(db->set_h_ffactor,(db,h_ffactor));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* BT_MINKEY */
    u_int32_t bt_minkey = I_to_uint32(check_uint32(STACK_0));
    SYSCALL(db->set_bt_minkey,(db,bt_minkey));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* PAGESIZE */
    u_int32_t pagesize = I_to_uint32(check_uint32(STACK_0));
    SYSCALL(db->set_pagesize,(db,pagesize));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* LORDER */
    int lorder = I_to_uint(check_uint(STACK_0));
    SYSCALL(db->set_lorder,(db,lorder));
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* CACHE = (ncache cachesize) */
    STACK_0 = check_list(STACK_0);
    if (consp(STACK_0)) {
      int ncache = I_to_uint(check_uint(Car(STACK_0)));
      u_int32_t gbytes=0, bytes=0;
      STACK_0 = check_list(Cdr(STACK_0));
      if (consp(STACK_0))
        size_to_giga_bytes(Car(STACK_0),&gbytes,&bytes);
      SYSCALL(db->set_cachesize,(db,gbytes,bytes,ncache));
    }
  }
  skipSTACK(1);
  if (!missingp(STACK_0)) {     /* CACHESIZE */
    int ncache = check_uint_default0(STACK_1);
    u_int32_t gbytes, bytes;
    size_to_giga_bytes(STACK_0,&gbytes,&bytes);
    SYSCALL(db->set_cachesize,(db,gbytes,bytes,ncache));
  }
  skipSTACK(2);
  if (!missingp(STACK_1))       /* PASSWORD */
    db_set_encryption(db,&STACK_0,&STACK_1);
  skipSTACK(2);
  if (!missingp(STACK_0)) reset_errpfx(db->dbenv);
  skipSTACK(1);
  if (!missingp(STACK_0)) reset_errfile(db->dbenv);
  skipSTACK(1);
  if (!missingp(STACK_0)) reset_msgfile(db->dbenv);
  skipSTACK(1);
  VALUES0; skipSTACK(1);        /* skip db */
}

/* get cache size and number of cashes
   value1 == cashesize, value2 = ncache
 can trigger GC */
static void db_get_cache (DB* db, int errorp) {
  u_int32_t gbytes, bytes;
  int ncache, status;
  begin_system_call();
  status = db->get_cachesize(db,&gbytes,&bytes,&ncache);
  end_system_call();
  if (status) {
    if (errorp) error_bdb(status,"db->get_cachesize");
    error_message_reset();
    value1 = value2 = NIL;
  } else {
    pushSTACK(UL_to_I(gbytes)); pushSTACK(fixnum(30)); funcall(L(ash),2);
    pushSTACK(value1); pushSTACK(UL_to_I(bytes)); funcall(L(plus),2);
    value2 = fixnum(ncache);
  }
}
/* get file name and database name
   value1 == file name, value2 = database name
 can trigger GC */
static void db_get_dbname (DB* db, int errorp) {
  const char *fname, *dbname;
  int status;
  begin_system_call();
  status = db->get_dbname(db,&fname,&dbname);
  end_system_call();
  if (status) {
    if (errorp) error_bdb(status,"db->get_cachesize");
    error_message_reset();
    value1 = value2 = NIL;
  } else {
    pushSTACK(asciz_to_string0(fname,GLO(pathname_encoding)));
    value2 = asciz_to_string0(dbname,GLO(misc_encoding));
    value1 = popSTACK();
  }
}

#define DEFINE_DB_GETTER1(g,t,f)   DEFINE_GETTER1(db,DB,g,t,f)
DEFINE_DB_GETTER1(get_lorder,int,L_to_I(value))
DEFINE_DB_GETTER1(get_pagesize,u_int32_t,UL_to_I(value))
#if defined(HAVE_DB_GET_TRANSACTIONAL_1ARG)
static object db_get_transactional (DB* db) {
  int val;
  begin_system_call(); val = db->get_transactional(db); end_system_call();
  return val ? T : NIL;
}
#else
DEFINE_DB_GETTER1(get_transactional,int,(value?T:NIL))
#endif
#define DEFINE_DB_GETTER2(getter,type,finish)           \
  static object db_##getter (DB* db,int errorp) {       \
    type value;                                         \
    int status;                                         \
    begin_system_call();                                \
    status = db->getter(db,&value);                     \
    end_system_call();                                  \
    if (status) {                                       \
      if (errorp) error_bdb(status,"db->" #getter);     \
      error_message_reset();                            \
      return NIL;                                       \
    } else                                              \
      return finish;                                    \
  }
DEFINE_DB_GETTER2(get_bt_minkey,u_int32_t,UL_to_I(value))
DEFINE_DB_GETTER2(get_h_ffactor,u_int32_t,UL_to_I(value))
DEFINE_DB_GETTER2(get_h_nelem,u_int32_t,UL_to_I(value))
DEFINE_DB_GETTER2(get_q_extentsize,u_int32_t,UL_to_I(value))
DEFINE_DB_GETTER2(get_re_delim,int,L_to_I(value))
DEFINE_DB_GETTER2(get_re_len,u_int32_t,UL_to_I(value))
DEFINE_DB_GETTER2(get_re_pad,int,L_to_I(value))
DEFINE_DB_GETTER2(get_re_source,const char*,
                  asciz_to_string0(value,GLO(pathname_encoding)))
FLAG_EXTRACTOR(db_get_flags_num,DB*)
DEFUNR(BDB:DB-GET-OPTIONS, db &optional what)
{ /* retrieve database options */
  DB *db = (DB*)bdb_handle(STACK_1,`BDB::DB`,BH_VALID);
  object what = STACK_0; skipSTACK(2);
 restart_DB_GET_OPTIONS:
  if (missingp(what)) {         /* get everything */
    uintL count = 0;
    pushSTACK(`:CACHE`); db_get_cache(db,false);
    pushSTACK(value1); pushSTACK(value2); value1 = listof(2);
    pushSTACK(value1); count++;
    pushSTACK(`:ERRPFX`); pushSTACK(dbe_get_errpfx(db->dbenv)); count++;
    pushSTACK(`:ERRFILE`); pushSTACK(dbe_get_errfile(db->dbenv)); count++;
    pushSTACK(`:MSGFILE`); pushSTACK(dbe_get_msgfile(db->dbenv)); count++;
    pushSTACK(`:FLAGS`); value1 = db_get_flags_list(db);
    pushSTACK(value1); count++;
    pushSTACK(`:LORDER`); pushSTACK(db_get_lorder(db)); count++;
    pushSTACK(`:PAGESIZE`); pushSTACK(db_get_pagesize(db)); count++;
    pushSTACK(`:BT-MINKEY`); pushSTACK(db_get_bt_minkey(db,false)); count++;
    pushSTACK(`:H-FFACTOR`); pushSTACK(db_get_h_ffactor(db,false)); count++;
    pushSTACK(`:H-NELEM`); pushSTACK(db_get_h_nelem(db,false)); count++;
    pushSTACK(`:Q-EXTENTSIZE`);pushSTACK(db_get_q_extentsize(db,false));count++;
    pushSTACK(`:RE-DELIM`); pushSTACK(db_get_re_delim(db,false)); count++;
    pushSTACK(`:RE-LEN`); pushSTACK(db_get_re_len(db,false)); count++;
    pushSTACK(`:RE-PAD`); pushSTACK(db_get_re_pad(db,false)); count++;
    pushSTACK(`:RE-SOURCE`); pushSTACK(db_get_re_source(db,false)); count++;
    pushSTACK(`:TRANSACTIONAL`); pushSTACK(db_get_transactional(db)); count++;
    pushSTACK(`:DBNAME`); db_get_dbname(db,false);
    pushSTACK(value1); pushSTACK(value2); value1 = listof(2);
    pushSTACK(value1); count++;
    pushSTACK(`:OPEN`); value1 = db_get_open_flags(db,false);
    pushSTACK(value1); count++;
    VALUES1(listof(2*count));
  } else if (eq(what,`:CACHE`)) {
    db_get_cache(db,true); mv_count = 2;
  } else if (eq(what,`:DBNAME`)) {
    db_get_dbname(db,true); mv_count = 2;
  } else if (eq(what,`:OPEN`)) {
    VALUES1(db_get_open_flags(db,true));
  } else if (eq(what,`:ENCRYPTION`)) {
    u_int32_t flags;
    SYSCALL(db->get_encrypt_flags,(db,&flags));
    switch (flags) {
      case DB_ENCRYPT_AES: VALUES1(`:ENCRYPT-AES`);
      case 0: VALUES1(NIL);
      default: NOTREACHED;
    }
  } else if (eq(what,`:ERRFILE`)) {
    VALUES1(dbe_get_errfile(db->dbenv));
  } else if (eq(what,`:MSGFILE`)) {
    VALUES1(dbe_get_msgfile(db->dbenv));
  } else if (eq(what,`:ERRPFX`)) {
    VALUES1(dbe_get_errpfx(db->dbenv));
  } else if (eq(what,`:PAGESIZE`)) {
    VALUES1(db_get_pagesize(db));
  } else if (eq(what,`:TRANSACTIONAL`)) {
    VALUES1(db_get_transactional(db));
  } else if (eq(what,`:BT-MINKEY`)) {
    VALUES1(db_get_bt_minkey(db,true));
  } else if (eq(what,`:H-FFACTOR`)) {
    VALUES1(db_get_h_ffactor(db,true));
  } else if (eq(what,`:H-NELEM`)) {
    VALUES1(db_get_h_nelem(db,true));
  } else if (eq(what,`:Q-EXTENTSIZE`)) {
    VALUES1(db_get_q_extentsize(db,true));
  } else if (eq(what,`:RE-DELIM`)) {
    VALUES1(db_get_re_delim(db,true));
  } else if (eq(what,`:RE-LEN`)) {
    VALUES1(db_get_re_len(db,true));
  } else if (eq(what,`:RE-PAD`)) {
    VALUES1(db_get_re_pad(db,true));
  } else if (eq(what,`:RE-SOURCE`)) {
    VALUES1(db_get_re_source(db,true));
  } else if (eq(what,`:LORDER`)) {
    VALUES1(db_get_lorder(db));
  } else if (eq(what,`:FLAGS`)) {
    VALUES1(db_get_flags_list(db));
  } else if (eq(what,`:TXN-NOT-DURABLE`)) {
    VALUES_IF(db_get_flags_num(db) & DB_TXN_NOT_DURABLE);
  } else if (eq(what,`:CHKSUM`)) {
    VALUES_IF(db_get_flags_num(db) & DB_CHKSUM);
  } else if (eq(what,`:DUP`)) {
    VALUES_IF(db_get_flags_num(db) & DB_DUP);
  } else if (eq(what,`:DUPSORT`)) {
    VALUES_IF(db_get_flags_num(db) & DB_DUPSORT);
  } else if (eq(what,`:ENCRYPT`)) {
    VALUES_IF(db_get_flags_num(db) & DB_ENCRYPT);
#if defined(DB_INORDER)
  } else if (eq(what,`:INORDER`)) {
    VALUES_IF(db_get_flags_num(db) & DB_INORDER);
#endif
  } else if (eq(what,`:RECNUM`)) {
    VALUES_IF(db_get_flags_num(db) & DB_RECNUM);
  } else if (eq(what,`:RENUMBER`)) {
    VALUES_IF(db_get_flags_num(db) & DB_RENUMBER);
  } else if (eq(what,`:REVSPLITOFF`)) {
    VALUES_IF(db_get_flags_num(db) & DB_REVSPLITOFF);
  } else if (eq(what,`:SNAPSHOT`)) {
    VALUES_IF(db_get_flags_num(db) & DB_SNAPSHOT);
  } else {
    pushSTACK(NIL);             /* no PLACE */
    pushSTACK(what); pushSTACK(TheSubr(subr_self)->name);
    check_value(error,GETTEXT("~S: invalid argument ~S"));
    what = value1;
    goto restart_DB_GET_OPTIONS;
  }
}

/* ===== cursors ===== */
DEFFLAGSET(make_dbc_flags, DB_DEGREE_2 DB_READ_COMMITTED DB_DIRTY_READ \
           DB_READ_UNCOMMITTED DB_WRITECURSOR)
DEFUN(BDB:MAKE-DBC,db &key :TRANSACTION :DEGREE-2 :READ-COMMITTED :DIRTY-READ \
      :READ-UNCOMMITTED :WRITECURSOR)
{ /* create a cursor */
  u_int32_t flags = make_dbc_flags();
  DB_TXN *txn = (DB_TXN*)bdb_handle(STACK_0,`BDB::TXN`,BH_NIL_IS_NULL);
  DB *db = (DB*)bdb_handle(STACK_1,`BDB::DB`,BH_VALID);
  DBC *cursor;
  SYSCALL(db->cursor,(db,txn,&cursor,flags));
  if (txn) {
    object parents = listof(2);
    pushSTACK(parents);
  } else skipSTACK(1);          /* drop TXN=NIL */
  wrap_finalize(cursor,popSTACK(),`BDB::MKDBC`,``BDB::DBC-CLOSE``);
}

DEFUN(BDB:DBC-CLOSE, cursor)
{ /* close a cursor */
  DBC *cursor = (DBC*)bdb_handle(STACK_0,`BDB::DBC`,BH_INVALIDATE);
  if (cursor) {
    funcall(`BDB::KILL-HANDLE`,1);
    SYSCALL(cursor->c_close,(cursor));
    VALUES1(T);
  } else { skipSTACK(1); VALUES1(NIL); }
}

DEFUN(BDB:DBC-COUNT, cursor)
{ /* return a count of the number of data items for the key to which
     the cursor refers */
  DBC *cursor = (DBC*)bdb_handle(popSTACK(),`BDB::DBC`,BH_VALID);
  db_recno_t count;
  SYSCALL(cursor->c_count,(cursor,&count,0));
  VALUES1(UL_to_I(count));
}

DEFUN(BDB:DBC-DEL, cursor)
{ /* delete the key/data pair to which the cursor refers */
  DBC *cursor = (DBC*)bdb_handle(popSTACK(),`BDB::DBC`,BH_VALID);
  SYSCALL(cursor->c_del,(cursor,0));
  VALUES0;
}

DEFFLAGSET(dbc_dup_flags, DB_POSITION)
DEFUN(BDB:DBC-DUP, cursor &key :POSITION)
{ /* create a new cursor that uses the same transaction and locker ID as
     the original cursor */
  u_int32_t flags = dbc_dup_flags();
  DBC *cursor = (DBC*)bdb_handle(STACK_0,`BDB::DBC`,BH_VALID);
  DBC *new_cursor;
  SYSCALL(cursor->c_dup,(cursor,&new_cursor,flags));
  wrap_finalize(cursor,Parents(STACK_0),`BDB::MKDBC`,``BDB::DBC-CLOSE``);
  skipSTACK(1);
}

static dbt_o_t fill_or_init (object datum, DBT *pdbt, int re_len) {
  if (symbolp(datum)) {       /* type spec for the return value */
    init_dbt(pdbt,DB_DBT_MALLOC);
    return check_dbt_type(datum);
  } else return fill_dbt(datum,pdbt,re_len); /* datum */
}
DEFCHECKER(dbc_get_action,prefix=DB,default=DB_CURRENT,                 \
           CURRENT FIRST GET-BOTH GET-BOTH-RANGE GET-RECNO JOIN-ITEM LAST \
           NEXT NEXT-DUP NEXT-NODUP PREV PREV-NODUP SET SET-RANGE SET-RECNO)
DEFFLAGSET(dbc_get_options, DB_DEGREE_2 DB_READ_COMMITTED DB_DIRTY_READ \
           DB_READ_UNCOMMITTED DB_MULTIPLE DB_MULTIPLE_KEY DB_RMW)
DEFUN(BDB:DBC-GET, cursor key data action &key :DEGREE-2 :DIRTY-READ \
      :MULTIPLE :MULTIPLE-KEY :RMW :ERROR)
{ /* retrieve key/data pairs from the database */
  int no_error = nullp(popSTACK());
  u_int32_t flag = dbc_get_options();
  u_int32_t action = dbc_get_action(popSTACK());
  DBC *cursor = (DBC*)bdb_handle(STACK_2,`BDB::DBC`,BH_VALID);
  DBT key, val;
  int db_log_key = db_key_type(cursor->dbp,action);
  dbt_o_t val_type = fill_or_init(popSTACK(),&val,
                                  (action==DB_GET_RECNO ? sizeof(db_recno_t)
                                   : record_length(cursor->dbp)));
  dbt_o_t key_type = fill_or_init(popSTACK(),&key,db_log_key);
  int status;
  skipSTACK(1);                 /* drop cursor */
  begin_system_call();
  status = cursor->c_get(cursor,&key,&val,flag | action);
  end_system_call();
  if (status) {
    free_dbt(&key); free_dbt(&val);
    if (no_error) {
      switch (status) {
        case DB_NOTFOUND: VALUES1(`:NOTFOUND`); error_message_reset(); return;
        case DB_KEYEMPTY: VALUES1(`:KEYEMPTY`); error_message_reset(); return;
      }
    }
    error_bdb(status,"dbc->c_get");
  }
  if (action == DB_GET_RECNO) { /* the only value is RECNO */
    VALUES1(dbt_to_object(&val,val_type,-1));
    free_dbt(&key);
  } else {                      /* two values: key & value */
    /* :SET-RECNO means that both key and value are returned as is */
    pushSTACK(dbt_to_object(&key,key_type,
                            action==DB_SET_RECNO ? 0 : db_log_key));
    value2 = dbt_to_object(&val,val_type,0);
    value1 = popSTACK();
    mv_count = 2;
  }
}

DEFCHECKER(dbc_put_flag,prefix=DB, default=DB_CURRENT,          \
           CURRENT AFTER BEFORE KEYFIRST KEYLAST NODUPDATA)
DEFUN(BDB:DBC-PUT, cursor key data flag)
{ /* retrieve key/data pairs from the database */
  u_int32_t flag = dbc_put_flag(popSTACK());
  DBC *cursor = (DBC*)bdb_handle(STACK_2,`BDB::DBC`,BH_VALID);
  DBT key, val;
  fill_dbt(STACK_1,&key,db_key_type(cursor->dbp,0));
  fill_dbt(STACK_0,&val,record_length(cursor->dbp));
  SYSCALL1(cursor->c_put,(cursor,&key,&val,flag),
           {free(val.data);free(key.data);});
  skipSTACK(3);
  VALUES0;
}

/* ===== locks ===== */
/* not exported
 DB_ENV->lock_vec	Acquire/release locks
*/

DEFUN(BDB:LOCK-DETECT, dbe action)
{ /* Perform deadlock detection */
  u_int32_t flags = 0;
  u_int32_t action = check_lk_detect(popSTACK());
  DB_ENV *dbe = (DB_ENV*)bdb_handle(popSTACK(),`BDB::DBE`,BH_VALID);
  int aborted = false;
  SYSCALL(dbe->lock_detect,(dbe,flags,action,&aborted));
  VALUES_IF(aborted);
}

DEFCHECKER(check_lockmode, enum=db_lockmode_t, prefix=DB_LOCK, default=, \
           NG READ WRITE WAIT IWRITE IREAD IWR DIRTY WWRITE)
DEFFLAGSET(lock_get_flags, DB_LOCK_NOWAIT)
DEFUN(BDB:LOCK-GET, dbe object locker mode &key :NOWAIT)
{ /* Acquire a lock */
  u_int32_t flags = lock_get_flags();
  db_lockmode_t mode = check_lockmode(popSTACK());
  u_int32_t locker = I_to_uint32(check_uint32(popSTACK()));
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_1,`BDB::DBE`,BH_VALID);
  DBT obj;
  DB_LOCK *dblock;
  int status;
  /* fill_dbt() might not return,
     so my_malloc() must be called after it to avoid a memory leak */
  fill_dbt(STACK_0,&obj,0);
  dblock = (DB_LOCK*)my_malloc(sizeof(DB_LOCK));
  begin_system_call();
  status = dbe->lock_get(dbe,locker,flags,&obj,mode,dblock);
  free(obj.data);
  if (status) {
    free(dblock);
    error_bdb(status,"dbe->lock_get");
  }
  end_system_call();
  pushSTACK(allocate_fpointer(dblock)); pushSTACK(STACK_2);
  funcall(`BDB::MKDBLOCK`,2); STACK_0 = STACK_1 = value1;
  pushSTACK(``BDB:LOCK-CLOSE``); funcall(L(finalize),2);
  VALUES1(popSTACK()); /* DBLOCK */
}

DEFUN(BDB:LOCK-ID, dbe)
{ /* Acquire a locker ID */
  DB_ENV *dbe = (DB_ENV*)bdb_handle(popSTACK(),`BDB::DBE`,BH_VALID);
  u_int32_t id;
  SYSCALL(dbe->lock_id,(dbe,&id));
  VALUES1(uint32_to_I(id));
}
DEFUN(BDB:LOCK-ID-FREE, dbe id)
{ /* Release a locker ID */
  u_int32_t id = I_to_uint32(check_uint32(popSTACK()));
  DB_ENV *dbe = (DB_ENV*)bdb_handle(popSTACK(),`BDB::DBE`,BH_VALID);
  SYSCALL(dbe->lock_id_free,(dbe,id));
  VALUES0;
}

DEFUN(BDB:LOCK-PUT, dbe lock)
{ /* Release a lock */
  DB_LOCK *lock = (DB_LOCK*)bdb_handle(popSTACK(),`BDB::DBLOCK`,BH_INVALIDATE);
  DB_ENV *dbe = (DB_ENV*)bdb_handle(popSTACK(),`BDB::DBE`,BH_VALID);
  SYSCALL1(dbe->lock_put,(dbe,lock),{free(lock);});
  VALUES0;
}
DEFUN(BDB:LOCK-CLOSE, lock)
{ /* Close a lock and free the memory */
  DB_LOCK *lock = (DB_LOCK*)bdb_handle(STACK_0,`BDB::DBLOCK`,
                                       BH_INVALID_IS_NULL);
  if (lock) {
    object parent = Parents(STACK_0); /* parent of DBLOCK is a single DBE! */
    DB_ENV *dbe = (DB_ENV*)bdb_handle(parent,`BDB::DBE`,BH_INVALID_IS_NULL);
    if (dbe == NULL) { /* the DBE has been closed */
      pushSTACK(`BDB::BDB-ERROR`);  /* error type */
      pushSTACK(`:ERRNO`); pushSTACK(Fixnum_0);
      pushSTACK(CLSTEXT("~S (~S): cannot close a lock whose environment has been already closed; you must re-open the environment and call ~S"));
      pushSTACK(TheSubr(subr_self)->name);
      pushSTACK(STACK_5);       /* lock */
      pushSTACK(`BDB::LOCK-PUT`);
      funcall(L(error_of_type),7);
    }
    pushSTACK(parent); pushSTACK(STACK_1); funcall(``BDB::LOCK-PUT``,2);
    VALUES1(T);
  } else VALUES1(NIL);
  skipSTACK(1);
}

DEFFLAGSET(stat_flags, DB_STAT_CLEAR)
DEFUN(BDB:LOCK-STAT,dbe &key :STAT-CLEAR)
{ /* Return lock subsystem statistics */
  u_int32_t flags = stat_flags();
  DB_ENV *dbe = (DB_ENV*)bdb_handle(popSTACK(),`BDB::DBE`,BH_VALID);
  DB_LOCK_STAT *ls;
  SYSCALL(dbe->lock_stat,(dbe,&ls,flags));
  pushSTACK(uint32_to_I(ls->st_id));
  pushSTACK(uint32_to_I(ls->st_cur_maxid));
  pushSTACK(uint32_to_I(ls->st_nmodes));
  pushSTACK(uint32_to_I(ls->st_maxlocks));
  pushSTACK(uint32_to_I(ls->st_maxlockers));
  pushSTACK(uint32_to_I(ls->st_maxobjects));
  pushSTACK(uint32_to_I(ls->st_nlocks));
  pushSTACK(uint32_to_I(ls->st_maxnlocks));
  pushSTACK(uint32_to_I(ls->st_nlockers));
  pushSTACK(uint32_to_I(ls->st_maxnlockers));
  pushSTACK(uint32_to_I(ls->st_nobjects));
  pushSTACK(uint32_to_I(ls->st_maxnobjects));
  pushSTACK(uint32_to_I(ls->st_nrequests));
  pushSTACK(uint32_to_I(ls->st_nreleases));
#if defined(HAVE_DB_LOCK_STAT_ST_NNOWAITS)
  pushSTACK(uint32_to_I(ls->st_nnowaits));
#elif defined(HAVE_DB_LOCK_STAT_ST_LOCK_NOWAIT)
  pushSTACK(uint32_to_I(ls->st_lock_nowait));
#else
# error neither st_lock_nowait nor st_nnowaits is found in DB_LOCK_STAT
#endif
#if defined(HAVE_DB_LOCK_STAT_ST_NCONFLICTS)
  pushSTACK(uint32_to_I(ls->st_nconflicts));
#elif defined(HAVE_DB_LOCK_STAT_ST_LOCK_WAIT)
  pushSTACK(uint32_to_I(ls->st_lock_wait));
#else
# error neither st_lock_wait nor st_nconflicts is found in DB_LOCK_STAT
#endif
  pushSTACK(uint32_to_I(ls->st_ndeadlocks));
  pushSTACK(uint32_to_I(ls->st_locktimeout));
  pushSTACK(uint32_to_I(ls->st_nlocktimeouts));
  pushSTACK(uint32_to_I(ls->st_txntimeout));
  pushSTACK(uint32_to_I(ls->st_ntxntimeouts));
  pushSTACK(uint32_to_I(ls->st_regsize));
  pushSTACK(uint32_to_I(ls->st_region_wait));
  pushSTACK(uint32_to_I(ls->st_region_nowait));
  funcall(`BDB::MKLOCKSTAT`,24);
  begin_system_call(); free(ls); end_system_call();
}

/* ===== logs ===== */

DEFFLAGSET(log_archive_flags,DB_ARCH_ABS DB_ARCH_DATA DB_ARCH_LOG \
           DB_ARCH_REMOVE)
DEFUN(BDB:LOG-ARCHIVE, dbe &key :ARCH-ABS :ARCH-DATA :ARCH-LOG :ARCH-REMOVE)
{ /* return a list of log or database filenames. */
  u_int32_t flags = log_archive_flags();
  DB_ENV *dbe = (DB_ENV*)bdb_handle(popSTACK(),`BDB::DBE`,BH_VALID);
  char **list = NULL;
  SYSCALL(dbe->log_archive,(dbe,&list,flags));
  if (list) {
    int count = 0;
    for (; *list; list++, count++)
      pushSTACK(asciz_to_string(*list,GLO(pathname_encoding)));
    begin_system_call(); free(list); end_system_call();
    VALUES1(listof(count));
  } else VALUES0;
}

/* extract the DB_LSN data from the object
 can trigger GC */
static void check_lsn (gcv_object_t *obj_, DB_LSN *lsn) {
  *obj_ = check_classname(*obj_,`BDB::LSN`);
  lsn->file = I_to_uint32(TheStructure(*obj_)->recdata[1]);
  lsn->offset = I_to_uint32(TheStructure(*obj_)->recdata[2]);
}

DEFUN(BDB:LOG-FILE, dbe lsn)
{ /* return the name of the file containing the record named by lsn. */
  DB_LSN lsn;
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_1,`BDB::DBE`,BH_VALID);
  char path[BUFSIZ];
  check_lsn(&STACK_0,&lsn);
  SYSCALL(dbe->log_file,(dbe,&lsn,path,BUFSIZ));
  VALUES1(asciz_to_string(path,GLO(pathname_encoding)));
  skipSTACK(2);
}

DEFUN(BDB:LOG-FLUSH, dbe lsn)
{ /* flush log records to disk */
  DB_LSN lsn;
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_1,`BDB::DBE`,BH_VALID);
  check_lsn(&STACK_0,&lsn);
  SYSCALL(dbe->log_flush,(dbe,&lsn));
  VALUES0;
  skipSTACK(2);
}

/* convert C srtuct DB_LSN to Lisp structure LSN
 can trigger GC */
static object make_lsn (DB_LSN *lsn) {
  pushSTACK(uint32_to_I(lsn->file));
  pushSTACK(uint32_to_I(lsn->offset));
  funcall(`BDB::MKLSN`,2);
  return value1;
}

DEFFLAGSET(log_put_flags, DB_FLUSH)
DEFUN(BDB:LOG-PUT, dbe data &key FLUSH)
{ /* write a log record */
  u_int32_t flags = log_put_flags();
  DB_LSN lsn;
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_1,`BDB::DBE`,BH_VALID);
  DBT data;
  fill_dbt(STACK_0,&data,0); skipSTACK(2);
  SYSCALL1(dbe->log_put,(dbe,&lsn,&data,flags),{free(data.data);});
  make_lsn(&lsn);
}

DEFUN(BDB:LOG-STAT, dbe &key :STAT-CLEAR)
{ /* logging subsystem statistics */
  u_int32_t flags = stat_flags();
  DB_ENV *dbe = (DB_ENV*)bdb_handle(popSTACK(),`BDB::DBE`,BH_VALID);
  DB_LOG_STAT *stat;
  SYSCALL(dbe->log_stat,(dbe,&stat,flags));
  pushSTACK(uint32_to_I(stat->st_magic));
  pushSTACK(uint32_to_I(stat->st_version));
  pushSTACK(uint32_to_I(stat->st_mode));
  pushSTACK(uint32_to_I(stat->st_lg_bsize));
  pushSTACK(uint32_to_I(stat->st_lg_size));
  pushSTACK(uint32_to_I(stat->st_w_mbytes));
  pushSTACK(uint32_to_I(stat->st_w_bytes));
  pushSTACK(uint32_to_I(stat->st_wc_mbytes));
  pushSTACK(uint32_to_I(stat->st_wc_bytes));
  pushSTACK(uint32_to_I(stat->st_wcount));
  pushSTACK(uint32_to_I(stat->st_wcount_fill));
  pushSTACK(uint32_to_I(stat->st_scount));
  pushSTACK(uint32_to_I(stat->st_cur_file));
  pushSTACK(uint32_to_I(stat->st_cur_offset));
  pushSTACK(uint32_to_I(stat->st_disk_file));
  pushSTACK(uint32_to_I(stat->st_disk_offset));
  pushSTACK(uint32_to_I(stat->st_maxcommitperflush));
  pushSTACK(uint32_to_I(stat->st_mincommitperflush));
  pushSTACK(uint32_to_I(stat->st_regsize));
  pushSTACK(uint32_to_I(stat->st_region_wait));
  pushSTACK(uint32_to_I(stat->st_region_nowait));
  funcall(`BDB::MKLOGSTAT`,21);
  begin_system_call(); free(stat); end_system_call();
}

DEFUN(BDB:LOG-CURSOR, dbe)
{ /* create a log cursor. */
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_0,`BDB::DBE`,BH_VALID);
  DB_LOGC *cursor;
  SYSCALL(dbe->log_cursor,(dbe,&cursor,0));
  wrap_finalize(cursor,STACK_0,`BDB::MKLOGC`,``BDB::LOGC-CLOSE``);
}

DEFUN(BDB:LOGC-CLOSE, logc)
{ /* discard the log cursor. */
  DB_LOGC *logc = (DB_LOGC*)bdb_handle(STACK_0,`BDB::LOGC`,BH_INVALIDATE);
  if (logc) {
    funcall(`BDB::KILL-HANDLE`,1);
    SYSCALL(logc->close,(logc,0));
    VALUES1(T);
  } else { skipSTACK(1); VALUES1(NIL); }
}

DEFCHECKER(logc_get_action,prefix=DB,default=DB_CURRENT,        \
           CURRENT FIRST LAST NEXT PREV)
DEFUN(BDB:LOGC-GET, logc action &key :TYPE :ERROR)
{ /* return records from the log. */
  int no_error = nullp(popSTACK());
  dbt_o_t out_type = check_dbt_type(popSTACK());
  DB_LOGC *logc = (DB_LOGC*)bdb_handle(STACK_1,`BDB::LOGC`,BH_VALID);
  DB_LSN lsn;
  u_int32_t action;
  DBT data;
  int status;
  if (symbolp(STACK_0) || fixnump(STACK_0)) {
    action = logc_get_action(STACK_0);
  } else {
    action = DB_SET;
    check_lsn(&STACK_0,&lsn);
  }
  init_dbt(&data,DB_DBT_MALLOC);
  begin_system_call();
  status = logc->get(logc,&lsn,&data,action);
  end_system_call();
  if (status) {
    if (no_error) {
      switch (status) {
        case DB_NOTFOUND: VALUES1(`:NOTFOUND`); error_message_reset(); return;
      }
    }
    error_bdb(status,"dbc->c_get");
  }
  if (action == DB_SET) {       /* STACK_0 is the LSN */
  } else STACK_0 = make_lsn(&lsn);
  VALUES2(dbt_to_object(&data,out_type,0),popSTACK());
  free_dbt(&data);
  skipSTACK(1);
}

DEFUN(BDB:LOG-COMPARE, lsn1 lsn2)
{ /* Compare two Log Sequence Numbers */
  DB_LSN lsn1, lsn2;
  int value;
  check_lsn(&STACK_1,&lsn1);
  check_lsn(&STACK_0,&lsn2);
  begin_system_call(); value = log_compare(&lsn1,&lsn2); end_system_call();
  VALUES1(fixnum(value));
  skipSTACK(2);
}

/* ===== transactions ===== */

DEFFLAGSET(txn_begin_flags, DB_DEGREE_2 DB_READ_COMMITTED DB_DIRTY_READ \
           DB_READ_UNCOMMITTED DB_TXN_NOSYNC DB_TXN_NOWAIT DB_TXN_SYNC)
DEFUN(BDB:TXN-BEGIN, dbe &key :PARENT :DEGREE-2 :READ-COMMITTED :DIRTY-READ \
      :READ-UNCOMMITTED :NOSYNC :NOWAIT :SYNC)
{ /* create a transaction */
  u_int32_t flags = txn_begin_flags();
  DB_TXN *parent = (DB_TXN*)bdb_handle(STACK_0,`BDB::TXN`,BH_NIL_IS_NULL), *ret;
  DB_ENV *dbe = (DB_ENV*)bdb_handle(STACK_1,`BDB::DBE`,BH_VALID);
  SYSCALL(dbe->txn_begin,(dbe,parent,&ret,flags));
  if (parent) {
    object parents = listof(2);
    pushSTACK(parents);
  } else skipSTACK(1);          /* drop TXN=NIL */
  wrap_finalize(ret,STACK_0,`BDB::MKTXN`,``BDB::TXN-COMMIT``);
  skipSTACK(1);
}

DEFUN(BDB:TXN-ABORT, txn)
{ /* Abort a transaction */
  DB_TXN *txn = (DB_TXN*)bdb_handle(STACK_0,`BDB::TXN`,BH_INVALIDATE);
  if (txn) {
    funcall(`BDB::KILL-HANDLE`,1);
    SYSCALL(txn->abort,(txn));
    VALUES1(T);
  } else { skipSTACK(1); VALUES1(NIL); }
}

DEFCHECKER(txn_check_sync,prefix=DB_TXN,default=0, NOSYNC SYNC)
DEFUN(BDB:TXN-COMMIT, txn &key :SYNC)
{ /* Commit a transaction */
  u_int32_t flags = txn_check_sync(popSTACK());
  DB_TXN *txn = (DB_TXN*)bdb_handle(STACK_0,`BDB::TXN`,BH_INVALIDATE);
  if (txn) {
    funcall(`BDB::KILL-HANDLE`,1);
    SYSCALL(txn->commit,(txn,flags));
    VALUES1(T);
  } else { skipSTACK(1); VALUES1(NIL); }
}

DEFUN(BDB:TXN-DISCARD, txn)
{ /* Discard a transaction */
  DB_TXN *txn = (DB_TXN*)bdb_handle(STACK_0,`BDB::TXN`,BH_INVALIDATE);
  if (txn) {
    funcall(`BDB::KILL-HANDLE`,1);
    SYSCALL(txn->discard,(txn,0));
    VALUES1(T);
  } else { skipSTACK(1); VALUES1(NIL); }
}

DEFUN(BDB:TXN-ID, txn)
{ /* Return the transaction's ID */
  DB_TXN *txn = (DB_TXN*)bdb_handle(popSTACK(),`BDB::TXN`,BH_VALID);
  u_int32_t id;
  begin_system_call(); id = txn->id(txn); end_system_call();
  VALUES1(UL_to_I(id));
}

DEFFLAGSET(txn_checkpoint_flags, DB_FORCE)
DEFUN(BDB:TXN-CHECKPOINT, dbe &key :KBYTE :MIN :FORCE)
{ /* flush the underlying memory pool, write a checkpoint record to the
     log, and then flush the log. */
  u_int32_t flags = txn_checkpoint_flags();
  u_int32_t min = check_uint_default0(popSTACK());
  u_int32_t kbyte = check_uint_default0(popSTACK());
  DB_ENV *dbe = (DB_ENV*)bdb_handle(popSTACK(),`BDB::DBE`,BH_VALID);
  SYSCALL(dbe->txn_checkpoint,(dbe,kbyte,min,flags));
  VALUES0;
}

/* return the pointer into the obj (which must be
   a (vector (unsigned-byte 8) DB_XIDDATASIZE))
 can trigger GC, the return value is invalidated by GC */
static u_int8_t* check_gid (gcv_object_t *obj_) {
  uintL idx = 0;
  object data_vector;
  *obj_ = check_byte_vector_len(*obj_,DB_XIDDATASIZE);
  data_vector = array_displace_check(*obj_,DB_XIDDATASIZE,&idx);
  return TheSbvector(data_vector)->data+idx;
}

DEFUN(BDB:TXN-PREPARE, txn gid)
{ /* initiate the beginning of a two-phase commit */
  DB_TXN *txn = (DB_TXN*)bdb_handle(STACK_1,`BDB::TXN`,BH_VALID);
  u_int8_t *gid = check_gid(&STACK_0);
  SYSCALL(txn->prepare,(txn,gid));
  VALUES0; skipSTACK(2);
}

/* allocate a (vector (unsigned-byte 8) DB_XIDDATASIZE) for this gid
 can trigger GC */
static object gid_to_vector (u_int8_t gid[DB_XIDDATASIZE]) {
  object vec = allocate_bit_vector(Atype_8Bit,DB_XIDDATASIZE);
  begin_system_call();
  memcpy(TheSbvector(vec)->data,gid,DB_XIDDATASIZE);
  end_system_call();
  return vec;
}

DEFFLAGSET(txn_recover_flags, DB_FIRST DB_NEXT)
DEFUN(BDB:TXN-RECOVER, dbe &key :FIRST :NEXT)
{ /* return a list of prepared but not yet resolved transactions */
  u_int32_t flags = txn_recover_flags();
  DB_ENV *dbe = (DB_ENV*)bdb_handle(popSTACK(),`BDB::DBE`,BH_VALID);
  u_int32_t tx_max;
  DB_PREPLIST *preplist;
  int status, ii;
  long retnum;
  SYSCALL(dbe->get_tx_max,(dbe,&tx_max));
  preplist = (DB_PREPLIST*)my_malloc(tx_max * sizeof(DB_PREPLIST));
  begin_system_call();
  status = dbe->txn_recover(dbe,preplist,tx_max,&retnum,flags);
  if (status) {
    free(preplist); end_system_call();
    error_bdb(status,"dbe->txn_recover");
  }
  end_system_call();
  for (ii=0; ii<retnum; ii++) {
    pushSTACK(allocate_fpointer(preplist[ii].txn));
    funcall(`BDB::MKTXN`,1); pushSTACK(value1);
    pushSTACK(gid_to_vector(preplist[ii].gid));
    value1 = allocate_cons();
    Cdr(value1) = popSTACK();   /* gid */
    Car(value1) = popSTACK();   /* txn */
    pushSTACK(value1);          /* (TXN . GID) */
  }
  VALUES1(listof(retnum));
}

DEFCHECKER(txn_timeout_check,prefix=DB_SET,default=, LOCK-TIMEOUT TXN-TIMEOUT)
DEFUN(BDB:TXN-SET-TIMEOUT, txn timeout which)
{ /* set timeout values for locks or transactions for the specified
     transaction */
  u_int32_t which = txn_timeout_check(popSTACK());
  db_timeout_t timeout = I_to_uint32(check_uint32(popSTACK()));
  DB_TXN *txn = (DB_TXN*)bdb_handle(popSTACK(),`BDB::TXN`,BH_VALID);
  SYSCALL(txn->set_timeout,(txn,timeout,which));
  VALUES0;
}

DEFUN(BDB:TXN-STAT, dbe &key :STAT-CLEAR)
{ /* transaction subsystem statistics */
  u_int32_t flags = stat_flags();
  DB_ENV *dbe = (DB_ENV*)bdb_handle(popSTACK(),`BDB::DBE`,BH_VALID);
  DB_TXN_STAT *stat;
  SYSCALL(dbe->txn_stat,(dbe,&stat,flags));
  pushSTACK(make_lsn(&(stat->st_last_ckp)));
  pushSTACK(convert_time_to_universal(&(stat->st_time_ckp)));
  pushSTACK(uint32_to_I(stat->st_last_txnid));
  pushSTACK(uint32_to_I(stat->st_maxtxns));
  pushSTACK(uint32_to_I(stat->st_nactive));
  pushSTACK(uint32_to_I(stat->st_maxnactive));
  pushSTACK(uint32_to_I(stat->st_nbegins));
  pushSTACK(uint32_to_I(stat->st_naborts));
  pushSTACK(uint32_to_I(stat->st_ncommits));
  pushSTACK(uint32_to_I(stat->st_nrestores));
  pushSTACK(uint32_to_I(stat->st_regsize));
  pushSTACK(uint32_to_I(stat->st_region_wait));
  pushSTACK(uint32_to_I(stat->st_region_nowait));
  { /* txnarray */
    int ii, size = stat->st_nactive;
    DB_TXN_ACTIVE *txn_active = stat->st_txnarray;
    for (ii=0; ii<size; ii++) {
      pushSTACK(uint32_to_I(txn_active->txnid));
      pushSTACK(uint32_to_I(txn_active->parentid));
      pushSTACK(make_lsn(&(txn_active->lsn)));
      pushSTACK(uint32_to_I(txn_active->xa_status));
      pushSTACK(gid_to_vector(txn_active->xid));
      funcall(`BDB::MKTXNACTIVE`,5); pushSTACK(value1);
    }
    value1 = vectorof(size); pushSTACK(value1);
  }
  funcall(`BDB::MKTXNSTAT`,14);
  begin_system_call(); free(stat); end_system_call();
}

void module__bdb__init_function_2 (module_t* module);
void module__bdb__init_function_2 (module_t* module)
{
#if defined(DEBUG)
  char *options = getenv("DMALLOC_OPTIONS");
  dmalloc_debug_setup(options);
  printf("dmalloc options: %s\n",options);
#endif
}
