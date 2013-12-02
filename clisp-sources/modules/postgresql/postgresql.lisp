;; CLISP interface to PostgreSQL <http://www.postgresql.org/>
;; Copyright (C) 1999-2006 Sam Steingold
;; This is free software, distributed under the GNU GPL 2

(pushnew :PostgreSQL *features*)

(defpackage "SQL"
  (:nicknames "POSTGRES" "POSTGRESQL")
  (:modern t)
  (:use "COMMON-LISP" "FFI")
  (:shadowing-import-from "EXPORTING"
           #:defconstant #:defun #:defmacro #:defvar
           #:def-c-type #:def-c-enum #:def-c-struct #:def-c-var #:def-call-out))

(in-package "SQL")

(setf (documentation (find-package "SQL") 'sys::impnotes) "postgresql")

(ffi:default-foreign-language :stdc)
(eval-when (compile) (setq *foreign-guard* t))

(c-lines "#include <config.h>~%") ; local PostgreSQL config

(c-lines "#if defined(HAVE_POSTGRES_EXT_H)
#  include <postgres_ext.h>
#elif defined(HAVE_POSTGRESQL_POSTGRES_EXT_H)
#  include <postgresql/postgres_ext.h>
#else
#  error \"PostgreSQL is not found\"
#endif~%")

(def-c-type Oid uint)

(eval-when (load compile eval)
  (defconstant NAMEDATALEN 64)
  (defconstant MAX-PARAM 64)
  (defconstant BUFSIZ 1024))

(c-lines "#if defined(HAVE_POSTGRES_EXT_H)
#  include <libpq-fe.h>
#elif defined(HAVE_POSTGRESQL_POSTGRES_EXT_H)
#  include <postgresql/libpq-fe.h>
#else
#  error \"PostgreSQL is not found\"
#endif~%")

(def-c-enum ConnStatusType
  CONNECTION_OK
  CONNECTION_BAD
  CONNECTION_STARTED          ; Waiting for connection to be made.
  CONNECTION_MADE             ; Connection OK; waiting to send.
  CONNECTION_AWAITING_RESPONSE ; Waiting for a response from the postmaster.
  CONNECTION_AUTH_OK ; Received authentication; waiting for backend startup.
  CONNECTION_SETENV           ; Negotiating environment.
  CONNECTION_SSL_STARTUP      ; Negotiating SSL.
  CONNECTION_NEEDED)          ; Internal state: connect() needed

(def-c-enum PostgresPollingStatusType
  (PGRES_POLLING_FAILED 0)
  PGRES_POLLING_READING         ; These two indicate that one may
  PGRES_POLLING_WRITING         ; use select before polling again.
  PGRES_POLLING_OK
  PGRES_POLLING_ACTIVE) ; unused; keep for awhile for backwards compatibility

(def-c-enum ExecStatusType
  (PGRES_EMPTY_QUERY 0)         ; empty query string was executed
  PGRES_COMMAND_OK              ; a query command that doesn't return anything
                                ; was executed properly by the backend
  PGRES_TUPLES_OK ; a query command that returns tuples was executed properly
                  ; by the backend, PGresult contains the result tuples
  PGRES_COPY_OUT                ; Copy Out data transfer in progress
  PGRES_COPY_IN                 ; Copy In data transfer in progress
  PGRES_BAD_RESPONSE ; an unexpected response was recv'd from the backend
  PGRES_NONFATAL_ERROR          ; notice or warning message
  PGRES_FATAL_ERROR)            ; query failed

(def-c-enum PGTransactionStatusType
  PQTRANS_IDLE                  ; connection idle
  PQTRANS_ACTIVE                ; command in progress
  PQTRANS_INTRANS               ; idle, within transaction block
  PQTRANS_INERROR               ; idle, within failed transaction
  PQTRANS_UNKNOW)               ; cannot determine status

(def-c-enum PGVerbosity
  PQERRORS_TERSE                ; single-line error messages
  PQERRORS_DEFAULT              ; recommended style
  PQERRORS_VERBOSE)             ; all the facts, ma'am

;;(def-c-type PGconn (c-struct vector)) ; components unknown
;;(def-c-type PGresult (c-struct vector)) ; components unknown
;;(def-c-struct PGconn) ; components unknown
;;(def-c-struct PGresult) ; components unknown
(def-c-type PGconn c-pointer) ; components unknown
(def-c-type PGresult c-pointer) ; components unknown
(def-c-type PGcancel c-pointer) ; components unknown

(def-c-struct PGnotify
  (result c-string)             ; (c-array character #.NAMEDATALEN)???
  (be_pid int)
  (extra c-string)
  ;; private:
  (next (c-pointer PGnotify)))

;; typedef void (*PQnoticeReceiver) (void *arg, const PGresult *res)
;; typedef void (*PQnoticeProcessor) (void *arg, const char *message)
(def-c-type PQnoticeReceiver
    (c-function (:arguments (p1 c-pointer) (res PGresult))
                (:return-type nil)))
(def-c-type PQnoticeProcessor
    (c-function (:arguments (p1 c-pointer) (message c-string))
                (:return-type nil)))

(def-c-type pqbool char)        ; for PQprint()

(def-c-struct PQprintOpt
  (header pqbool)           ; print output field headings and row count
  (align pqbool)                ; fill align the fields
  (standard pqbool)             ; old brain dead format
  (html3 pqbool)                ; output html tables
  (expanded pqbool)             ; expand tables
  (pager pqbool)                ; use pager for output if needed
  (fieldSep c-string)           ; field separator
  (tableOpt c-string)           ; insert to HTML <table ...>
  (caption c-string)            ; HTML <caption>
  (fieldName (c-array-ptr c-string))) ; null terminated array of repalceme field names

(def-c-struct PQconninfoOption
  (keyword c-string)        ; The keyword of the option
  (envvar c-string)                ; Fallback environment variable name
  (compiled c-string)              ; Fallback compiled in default value
  (val c-string)                   ; Option's current value, or NULL
  (label c-string)                 ; Label for field in connect dialog
  (dispchar c-string) ; Character to display for this field
                      ; a connect dialog. Values are: ""
                      ; Display entered value as is "*"
                      ; Password field - hide value "D"
                      ; Debug option - don't show by default
  (dispsize int))               ; Field size in characters for dialog

(def-c-struct PQArgBlock
  (len int)
  (isint int)
  (u c-pointer))                ; (c-union (ptr c-pointer) (integer int))

;; === fe-connect.c ===
;; make a new client connection to the backend
;; Asynchronous (non-blocking)
;;extern PGconn *PQconnectStart(const char *conninfo);
(def-call-out PQconnectStart (:return-type PGconn)
  (:arguments (conninfo c-string)))
;;extern PostgresPollingStatusType PQconnectPoll(PGconn *conn);
(def-call-out PQconnectPoll (:return-type PostgresPollingStatusType)
  (:arguments (conn PGconn)))

;; Synchronous (blocking)
(def-call-out PQconnectdb (:return-type PGconn)
  (:arguments (conninfo c-string)))
(def-call-out PQsetdbLogin (:return-type PGconn)
  (:arguments (pghost c-string) (pgport c-string) (pgoptions c-string)
              (pgtty c-string) (dbname c-string) (login c-string)
              (pwd c-string)))
(defmacro PQsetdb (a0 a1 a2 a3 a4) `(PQsetdbLogin ,a0 ,a1 ,a2 ,a3 ,a4 nil nil))

;; close the current connection and free the PGconn data structure
(def-call-out PQfinish (:arguments (conn PGconn)) (:return-type nil))

;; get info about connection options known to PQconnectdb
(def-call-out PQconndefaults (:return-type (c-ptr PQconninfoOption))
  (:arguments))
;; free the data structure returned by PQconndefaults()
(def-call-out PQconninfoFree (:return-type nil)
  (:arguments (connOptions (c-ptr PQconninfoOption))))

;; close the current connection and restablish a new one with the same
;; parameters:
;; Asynchronous (non-blocking)
;; extern int PQresetStart(PGconn *conn);
(def-call-out PQresetStart (:return-type int) (:arguments (conn PGconn)))
;; extern PostgresPollingStatusType PQresetPoll(PGconn *conn);
(def-call-out PQresetPoll (:return-type PostgresPollingStatusType)
  (:arguments (conn PGconn)))
;; Synchronous (blocking)
;; extern void PQreset(PGconn *conn);
(def-call-out PQreset (:return-type nil) (:arguments (conn PGconn)))

;; request a cancel structure
(def-call-out PQgetCancel (:return-type PGcancel) (:arguments (conn PGconn)))
;; free a cancel structure
(def-call-out PQfreeCancel (:return-type nil) (:arguments (conn PGcancel)))
;; issue a cancel request
(def-call-out PQcancel (:return-type int)
  (:arguments (cancel PGcancel)
              (errbuf (c-ptr (c-array-max char #.BUFSIZ)) :out :alloca)
              (errbufsize int))) ; pass BUFSIZ
;; backwards compatible version of PQcancel; not thread-safe
(def-call-out PQrequestCancel (:return-type int) (:arguments (conn PGconn)))

;; Accessor functions for PGconn objects
(def-call-out PQdb (:return-type c-string) (:arguments (conn PGconn)))
(def-call-out PQuser (:return-type c-string) (:arguments (conn PGconn)))
(def-call-out PQpass (:return-type c-string) (:arguments (conn PGconn)))
(def-call-out PQhost (:return-type c-string) (:arguments (conn PGconn)))
(def-call-out PQport (:return-type c-string) (:arguments (conn PGconn)))
(def-call-out PQtty (:return-type c-string) (:arguments (conn PGconn)))
(def-call-out PQoptions (:return-type c-string) (:arguments (conn PGconn)))
(def-call-out PQstatus (:return-type ConnStatusType) (:arguments (conn PGconn)))
(def-call-out PQtransactionStatus (:return-type PGTransactionStatusType)
  (:arguments (conn PGconn)))
(def-call-out PQparameterStatus (:return-type c-string)
  (:arguments (conn PGconn) (param-name c-string)))
(def-call-out PQprotocolVersion (:return-type int) (:arguments (conn PGconn)))
(def-call-out PQserverVersion (:return-type int) (:arguments (conn PGconn)))
(def-call-out PQerrorMessage (:return-type c-string) (:arguments (conn PGconn)))
(def-call-out PQsocket (:return-type int) (:arguments (conn PGconn)))
(def-call-out PQbackendPID (:return-type int) (:arguments (conn PGconn)))
(def-call-out PQclientEncoding (:return-type int) (:arguments (conn PGconn)))
(def-call-out PQsetClientEncoding (:return-type int)
  (:arguments (conn PGconn) (encoding c-string)))

;; ifdef USE_SSL
;; (def-call-out PQgetssl (:arguments (conn PGconn)) (:return-type SSL))
(def-call-out PQgetssl (:arguments (conn PGconn)) (:return-type c-pointer))
;; Tell libpq whether it needs to initialize OpenSSL (not in libpq 8.0)
(def-call-out PQinitSSL (:return-type nil) (:arguments (do_init int)))

(def-call-out PQsetErrorVerbosity (:return-type PGVerbosity)
  (:arguments (conn PGconn) (verbosity PGVerbosity)))

(def-call-out PQtrace (:return-type nil)
  (:arguments (conn PGconn) (debug_port c-pointer))) ; ?? FILE*
(def-call-out PQuntrace (:arguments (conn PGconn)) (:return-type nil))

(def-call-out PQsetNoticeReceiver (:return-type PQnoticeReceiver)
  (:arguments (conn PGconn) (proc PQnoticeProcessor)
              (arg c-pointer)))
(def-call-out PQsetNoticeProcessor (:return-type PQnoticeProcessor)
  (:arguments (conn PGconn) (proc PQnoticeProcessor)
              (arg c-pointer)))

;; Used to set callback that prevents concurrent access to
;; non-thread safe functions that libpq needs.
;; The default implementation uses a libpq internal mutex.
;; Only required for multithreaded apps that use kerberos
;; both within their app and for postgresql connections.
;;typedef void (*pgthreadlock_t) (int acquire);
;;extern pgthreadlock_t PQregisterThreadLock(pgthreadlock_t newhandler);

;; === fe-exec.c ===
;; Simple synchronous query
(def-call-out PQexec (:return-type PGresult)
  (:arguments (conn PGconn) (query c-string)))
(def-call-out PQexecParams (:return-type PGresult)
  (:arguments (conn PGconn) (command c-string) (nParams int)
              (paramTypes (c-ptr (c-array-max Oid #.MAX-PARAM)) :out :alloca)
              (paramValues (c-ptr (c-array-max c-string #.MAX-PARAM))
                           :out :alloca)
              (paramLengths (c-ptr (c-array-max int #.MAX-PARAM)) :out :alloca)
              (paramFormats (c-ptr (c-array-max int #.MAX-PARAM)) :out :alloca)
              (resultFormat int)))
(def-call-out PQprepare (:return-type PGresult)
  (:arguments (conn PGconn) (stmtName c-string) (query c-string) (nParams int)
              (paramTypes (c-ptr (c-array-max Oid #.MAX-PARAM)) :out :alloca)))
(def-call-out PQexecPrepared (:return-type PGresult)
  (:arguments (conn PGconn) (stmtName c-string) (nParams int)
              (paramValues (c-ptr (c-array-max c-string #.MAX-PARAM))
                           :out :alloca)
              (paramLengths (c-ptr (c-array-max int #.MAX-PARAM)) :out :alloca)
              (paramFormats (c-ptr (c-array-max int #.MAX-PARAM)) :out :alloca)
              (resultFormat int)))

;; Interface for multiple-result or asynchronous queries
(def-call-out PQsendQuery (:return-type int)
  (:arguments (conn PGconn) (query c-string)))
(def-call-out PQsendQueryParams (:return-type int)
  (:arguments (conn PGconn) (command c-string) (nParams int)
              (paramTypes (c-ptr (c-array-max Oid #.MAX-PARAM)) :out :alloca)
              (paramValues (c-ptr (c-array-max c-string #.MAX-PARAM))
                           :out :alloca)
              (paramLengths (c-ptr (c-array-max int #.MAX-PARAM)) :out :alloca)
              (paramFormats (c-ptr (c-array-max int #.MAX-PARAM)) :out :alloca)
              (resultFormat int)))
(def-call-out PQsendPrepare (:return-type int)
  (:arguments (conn PGconn) (stmtName c-string) (query c-string) (nParams int)
              (paramTypes (c-ptr (c-array-max Oid #.MAX-PARAM)) :out :alloca)))
(def-call-out PQsendQueryPrepared (:return-type int)
  (:arguments (conn PGconn) (stmtName c-string) (nParams int)
              (paramValues (c-ptr (c-array-max c-string #.MAX-PARAM))
                           :out :alloca)
              (paramLengths (c-ptr (c-array-max int #.MAX-PARAM)) :out :alloca)
              (paramFormats (c-ptr (c-array-max int #.MAX-PARAM)) :out :alloca)
              (resultFormat int)))
(def-call-out PQgetResult (:return-type PGresult) (:arguments (conn PGconn)))

;; Routines for managing an asynchronous query
(def-call-out PQisBusy (:return-type int) (:arguments (conn PGconn)))
(def-call-out PQconsumeInput (:return-type int) (:arguments (conn PGconn)))

;; LISTEN/NOTIFY support
(def-call-out PQnotifies (:return-type (c-ptr PGnotify))
  (:arguments (conn PGconn)))

;; Routines for copy in/out
(def-call-out PQputCopyData (:return-type int)
  (:arguments (conn PGconn) (buffer c-string) (nbytes int)))
(def-call-out PQputCopyEnd (:return-type int)
  (:arguments (conn PGconn) (errormsg c-string)))
(def-call-out PQgetCopyData (:return-type int)
  (:arguments (conn PGconn) (buffer (c-ptr c-string) :out) (async int)))

;; Deprecated routines for copy in/out
(def-call-out PQgetline (:return-type int)
  (:arguments (conn PGconn) (string c-string) (length int)))
(def-call-out PQputline (:return-type int)
  (:arguments (conn PGconn) (string c-string)))
(def-call-out PQgetlineAsync (:return-type int)
  (:arguments (conn PGconn) (buffer c-string) (bufsize int)))
(def-call-out PQputnbytes (:return-type int)
  (:arguments (conn PGconn) (buffer c-string) (nbytes int)))
(def-call-out PQendcopy (:arguments (conn PGconn)) (:return-type int))

;; Set blocking/nonblocking connection to the backend
(def-call-out PQsetnonblocking (:return-type int)
  (:arguments (conn PGconn) (arg int)))
(def-call-out PQisnonblocking (:return-type int) (:arguments (conn PGconn)))

;; Force the write buffer to be written (or at least try)
(def-call-out PQflush (:return-type int) (:arguments (conn PGconn)))

;; "Fast path" interface --- not really recommended for application use
(def-call-out PQfn (:return-type PGresult)
  (:arguments (conn PGconn) (fnid int) (result_buf (c-ptr int) :out)
              (result_len (c-ptr int) :out) (result_is_int int)
              (args (c-ptr (c-array-max PQArgBlock #.MAX-PARAM)) :out :alloca)
              (nargs int)))

;; Accessor functions for PGresult objects
(def-call-out PQresultStatus (:return-type ExecStatusType)
  (:arguments (res PGresult)))
(def-call-out PQresStatus (:return-type c-string)
  (:arguments (status ExecStatusType)))
(def-call-out PQresultErrorMessage (:return-type c-string)
  (:arguments (res PGresult)))
(def-call-out PQresultErrorField (:return-type c-string)
  (:arguments (res PGresult) (fieldcode int)))
(def-call-out PQntuples (:return-type int) (:arguments (res PGresult)))
(def-call-out PQnfields (:return-type int) (:arguments (res PGresult)))
(def-call-out PQbinaryTuples (:return-type int) (:arguments (res PGresult)))
(def-call-out PQfname (:return-type c-string)
  (:arguments (res PGresult) (field_num int)))
(def-call-out PQfnumber (:return-type int)
  (:arguments (res PGresult) (field_name c-string)))
(def-call-out PQftable (:return-type Oid)
  (:arguments (res PGresult) (field_num int)))
(def-call-out PQftablecol (:return-type int)
  (:arguments (res PGresult) (field_num int)))
(def-call-out PQfformat (:return-type int)
  (:arguments (res PGresult) (field_num int)))
(def-call-out PQftype (:return-type Oid)
  (:arguments (res PGresult) (field_num int)))
(def-call-out PQfsize (:return-type int)
  (:arguments (res PGresult) (field_num int)))
(def-call-out PQfmod (:return-type int)
  (:arguments (res PGresult) (field_num int)))
(def-call-out PQcmdStatus (:return-type c-string)
  (:arguments (res PGresult)))
(def-call-out PQoidStatus (:return-type c-string) ; old and ugly
  (:arguments (res PGresult)))
(def-call-out PQoidValue (:return-type Oid) ; new and improved
  (:arguments (res PGresult)))
(def-call-out PQcmdTuples (:return-type c-string)
  (:arguments (res PGresult)))
(def-call-out PQgetvalue (:return-type c-string)
  (:arguments (res PGresult) (tup_num int) (field_num int)))
(def-call-out PQgetlength (:return-type int)
  (:arguments (res PGresult) (tup_num int) (field_num int)))
(def-call-out PQgetisnull (:return-type int)
  (:arguments (res PGresult) (tup_num int) (field_num int)))

;; Delete a PGresult
(def-call-out PQclear (:return-type nil) (:arguments (res PGresult)))

;; For freeing other alloc'd results, such as PGnotify structs
(def-call-out PQfreemem (:return-type nil) (:arguments (res c-pointer)))

(def-call-out PQmakeEmptyPGresult (:return-type PGresult)
  (:arguments (conn PGconn) (status ExecStatusType)))

;; Quoting strings before inclusion in queries
(def-call-out PQescapeString (:return-type uint)
  (:arguments (to (c-ptr (c-array-max char #.BUFSIZ)) :out :alloca)
              (from c-string) (length uint)))
(def-call-out PQescapeBytea (:return-type c-string)
  (:arguments (bintext (c-ptr (c-array-max char #.BUFSIZ)) :in-out)
              (binlen uint) (bytealen (c-ptr uint) :out)))
(def-call-out PQunescapeBytea (:return-type c-string)
  (:arguments (strtext (c-ptr (c-array-max char #.BUFSIZ)) :in-out)
              (retbuflen (c-ptr uint) :out)))

;; === fe-print.c ===
(def-call-out PQprint (:return-type nil)
  (:arguments (fout c-pointer) ; ?? FILE*
              (res PGresult)
              (ps (c-ptr PQprintOpt))))
(def-call-out PQdisplayTuples (:return-type nil)
  (:arguments (res PGresult) (fp c-pointer) ; ?? FILE*
              (fillAlign int) (fieldSep c-string)
              (printHeader int) (quiet int)))
(def-call-out PQprintTuples (:return-type nil)
  (:arguments (res PGresult) (fout c-pointer) ; ?? FILE*
              (printAttName int) (terseOutput int) (width int)))

;; === fe-lobj.c ===
;; Large-object access routines
(def-call-out lo_open (:return-type int)
  (:arguments (conn PGconn) (lobjId Oid) (mode int)))
(def-call-out lo_close (:return-type int)
  (:arguments (conn PGconn) (fd int)))
(def-call-out lo_read (:return-type int)
  (:arguments (conn PGconn) (fd int) (buf c-string) (len int)))
(def-call-out lo_write (:return-type int)
  (:arguments (conn PGconn) (fd int) (buf c-string) (len int)))
(def-call-out lo_lseek (:return-type int)
  (:arguments (conn PGconn) (fd int) (offset int) (whence int)))
(def-call-out lo_creat (:return-type Oid)
  (:arguments (conn PGconn) (mode int)))
(def-call-out lo_tell (:return-type int)
  (:arguments (conn PGconn) (fd int)))
(def-call-out lo_unlink (:return-type int)
  (:arguments (conn PGconn) (lobjId Oid)))
(def-call-out lo_import (:return-type Oid)
  (:arguments (conn PGconn) (filename c-string)))
(def-call-out lo_export (:return-type int)
  (:arguments (conn PGconn) (lobjId Oid) (filename c-string)))

;; === fe-misc.c ===
;; Determine length of multibyte encoded char at *s
(def-call-out PQmblen (:return-type int)
  (:arguments (s (c-pointer uchar)) (encoding int)))
;; Determine display length of multibyte encoded char at *s
(def-call-out PQdsplen (:return-type int)
  (:arguments (s (c-pointer uchar)) (encoding int)))
;; Get encoding id from environment variable PGCLIENTENCODING
(def-call-out PQenv2encoding (:return-type int) (:arguments))

(provide "postgresql")
