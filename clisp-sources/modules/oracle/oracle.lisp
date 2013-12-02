;
; CLISP Oracle interface
;
; Copyright (C) 2002 Alma Mater Software, Inc., Tarrytown, NY, USA
;
; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License version 2 as
; published by the Free Software Foundation; see file GNU-GPL.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software Foundation,
; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;
; $Id: oracle.lisp,v 1.24 2005/11/17 23:09:56 sds Exp $

(defpackage "ORACLE"
  (:documentation
   "CLISP Oracle interface from Alma Mater Software, Inc.  Inquiries to: John Hinsdale <hin@alma.com>")
  (:use "LISP" "FFI")
  (:export "CONNECT" "DISCONNECT" "RUN-SQL"
           "DO-ROWS" "FETCH" "FETCH-ALL" "PEEK" "COLUMNS" "EOF"
           "INSERT-ROW" "UPDATE-ROW" "ROW-COUNT"
           "WITH-TRANSACTION" "COMMIT" "ROLLBACK" "AUTO-COMMIT"
	   "SQLCOL-NAME" "SQLCOL-TYPE" "SQLCOL-SIZE" "SQLCOL-SCALE" "SQLCOL-PRECISION" "SQLCOL-NULL_OK" ))

(in-package "ORACLE")
(pushnew :oracle *features*)

(setf (documentation (find-package "ORACLE") 'sys::impnotes) "oracle")

; Use "C" as foreign language
(default-foreign-language :stdc)

; Inline everything for speed
(proclaim '(inline
aref-null array-to-hash auto-commit auto-commit-nocheck cat
check-connection check-pairs check-success check-unique-elements
column-names columns comma-list-of-keys commit commit-nocheck connect
connection-key convert-type c-truth curconn disconnect do-rows-col
do-rows-index-of do-rows-var eof fetch flatten from-sqlval
gethash-required hash-combine hash-to-sqlparam-array if-null
insert-row is-select-query join lisp-truth nl out out-nl pairs-to-hash
peek rollback rollback-nocheck row-count row-to-result rowval run-sql
to-sqlval to-string update-row valid-symbol
))

;; GLOBALS

; Cached connections
(defvar *oracle-connection-cache* nil)  ; Table of established connections.  Maps keys constructed by
                                        ; CONNECTION-KEY to a "db" struct
(defvar *oracle-connection*       nil)  ; The current connection, a "db" struct, or NIL if none
(defvar *oracle-in-transaction*   nil)  ; Nesting guard for WITH-TRANSACTION macro

;; Lisp data types

; Cached per database connection
(defstruct db
  connection     ; "C" library handle for Oracle operations
  fetch-called   ; Flag: has fetch been called yet
  pending-row    ; Look-ahead row (for PEEK)
  colinfo        ; Cache the column info for speed
  hkey           ; Key in global hash (useful for removal)
)

; Shorthand for current library handle
(defun curconn ()
  (if (null *oracle-connection*)
      nil
    (db-connection *oracle-connection*)))

;  "C" DATA TYPES (oiface.h)

; Column info element
(def-c-struct sqlcol (name      c-string)
                     (type      c-string)
                     (size      int)
                     (scale     int)
                     (precision int)
                     (null_ok   int))

; Row data element
(def-c-struct sqlval (data      c-string)
                     (is_null   int))

; Bind parameter (name, value) pair
(def-c-struct sqlparam (name    c-string)
                       (value   sqlval))

;---------------------------------------------------------------------------------------

; EXPORTED LISP FUNCTIONS

;----------------------------------------------

; CONNECT

(defun connect (user password server &optional schema (auto-commit t) (prefetch-buffer-bytes 0) (long-len -1) truncate-ok connid)
"(ORACLE::CONNECT user password server &optional schema auto-commit prefetch-buffer-bytes long-len truncate-ok connid)

Connect to an Oracle database.  All subsequent operations will affect
this database.  A single program can access several different
databases by repeated calls to CONNECT.  Connections are cached: if
you call CONNECT again with the same user, schema and server, the
actual Oracle connection will be re-used.  This can be overriden by
using the CONNID parameter to identify multiple connections to the
same user, schema and server.  CONNECT may not be called inside the
WITH-TRANSACTION macro.

Required arguments:
    user         Oracle user ID
    password     Password for Oracle user, or NIL for no password (!)
    server       Oracle server ID (SID)

Optional arguments:
    schema       Oracle default schema (default: NIL).  If null, same as user.
                 This allows you to log on with one user's id/password but see
                 the database as if you were some other user.
    auto-commit  Commit after every operation (default: T).  Set this to NIL if
                 you intend to do transactions, and call call COMMIT explicitly.
    prefetch-    Number of bytes to cache from SELECT fetches (default: 64 Kbytes)
    buffer-      If you are very short of memory, or have a slow connection to Oracle,
    bytes        you can reduce this to 10,000 or so.  Alternatively, if you have a
                 fast connection to Oracle and regularly do large queries, you can
                 increase throughput by increasing this value.
    long-len     Number of bytes to fetch for \"long\" (LONG, [BC]LOB) types.
                 Long data that exceeds this size will raise an error,
                 or be truncated depending on the value of truncate-ok
                 (below).  Setting this parameter to zero and disallowing truncation
                 will disable long fetching entirely.  If NIL or negative, defaults to
                 500k bytes.
    truncate-ok  If set, allow truncation of LONG columns to long-len (default: NIL).
    connid       String assigning an ID making the connection unique among other
                 connections to with the same (user schema sever)

Returns: T if a cached connection was re-used (NIL if a new connection
         was created and cached).
"
  (when *oracle-in-transaction* (db-error "CONNECT not allowed inside WITH-TRANSACTION"))

  ; Default current schema
  (if (null schema) (setf schema user))
  (if (null long-len) (setf long-len -1))
  (if (null prefetch-buffer-bytes) (setf prefetch-buffer-bytes 0))

  ; Set up global connection cache
  (if (null *oracle-connection-cache*)
      (setf *oracle-connection-cache* (make-hash-table :test #'equal)))

  ; Construct key for connection cache
  (let* ((hkey (connection-key user schema server connid))
         (conn (gethash hkey *oracle-connection-cache*))
         (result t))

    ; If using cached connection, "ping" it with a query to make sure it's still alive
    ; If there's an error, disconnect and invalidate; fall through and retry
    (when conn
	  (let ((conn-handle (db-connection conn)))
	    (oracle_exec_sql conn-handle "SELECT 'x' FROM dual" (make-array 0) (c-truth nil))
	    (when (not (lisp-truth (oracle_success conn-handle)))
		  (oracle_disconnect conn-handle) ; Don't check for error here
		  (remhash hkey *oracle-connection-cache*)
		  (setf conn nil
				*oracle-connection* nil))))

    (when (null conn)
          ; Connect to database
          (let ((handle (oracle_connect user schema password server prefetch-buffer-bytes (c-truth auto-commit) long-len (c-truth truncate-ok))))
            (when (not (lisp-truth (oracle_success handle)))
                   ; Retry connection
                   ; ... TODO: implement retry logic here
                   ; Failed all attempts; give up
                  (db-error (oracle_last_error handle)))
            ; OK: cache the connection
            (setf conn (make-db :connection handle :hkey hkey)
				  (gethash hkey *oracle-connection-cache*) conn
				  result nil)))
    ; Set current connection
    (setf *oracle-connection* conn)
    result))

;----------------------------------------------

; DISCONNECT
(defun disconnect ()
"(ORACLE:DISCONNECT)

Disconnect from the database.  No more calls can be made until CONNECT
is called again.  The connection is closed and removed from the
connection cache.  Does nothing if there was no connection.
DISCONNECT may not be called inside the WITH-TRANSACTION macro.

Arguments: none.
Returns: NIL
"
  (when *oracle-in-transaction* (db-error "DISCONNECT not allowed inside WITH-TRANSACTION"))
  (when (curconn)
        (oracle_disconnect (curconn))
        (check-success)
        ; Remove connection from the hash table
        (remhash (db-hkey *oracle-connection*) *oracle-connection-cache*)
        (setf *oracle-connection* nil)))

;----------------------------------------------

; RUN-SQL
(defun run-sql (sql &optional params (is-select t is-select-given))
"(ORACLE::RUN-SQL sql &optional params is-select)

Run a SQL statement.  Must be connected to a database.
Required argument:
    sql        Text of SQL statement, as a string.  Statement may
               contain named parameters, e.g. \":myparam\" which
               whose values will be substituted from the parameters
               passed in in the next argument, \"params\"

Optional arguments:
    params     A mapping of the names of the bind-parameters in
               the query to their values.  The set of named parameters
               in the query must match they keys of the hash EXACTLY.
               The mapping may be passed as either (1) a hash table
               whose keys are the named parameters or (2) a list of
               pairs, ((name value) (name value) ...).

    is-select  (Boolean) Whether the statement is a SELECT query.
               You usually do not need to set this as it is detected
               by default based on the SQL text.  However, there are situations,
               such as when a SELECT query begins with comment, that you need
               to specify it explicitly.

Returns: the number of rows affected for non-SELECT statements, zero
for SELECT statements.

"
  (check-connection "run a SQL statement")
  ; Default statement type: query vs. command
  (when (not is-select-given)
        (setf is-select (is-select-query sql)))
  ; If pairs given, convert them to hash
  (setf params (check-pairs params))
  (oracle_exec_sql (curconn)
                   sql
                   (hash-to-sqlparam-array params)
                   (c-truth (not is-select)))
  (setf (db-fetch-called *oracle-connection*) nil
		(db-pending-row *oracle-connection*) nil
		(db-colinfo *oracle-connection*) nil)
  (check-success)

  ; Get the row count for the result
  (let ((result (row-count)))
    (check-success)
    result))

;----------------------------------------------

; DO-ROWS
(defmacro do-rows (vars &body body)
"(ORACLE:DO-ROWS (vars &body body)

Macro that extends Lisp's DO loop construct, binding database column
values to the symbols given in the first argument, which must be a
non-empty list of symbols matching columns of an active SELECT query.

It is allowed to call CONNECT in the body of the loop, but only to
switch the conneciton to a different database other than the one that
was used to do the SELECT.  This is useful for reading from one
database while writing to another.

When specifying variables to which to bind column values, instead of a
single symbol, a pair (bound-var \"column-name\") can be specified which
will cause values from the given column name to be bound to the given
variable.  This is for unusual cases where a Lisp symbol cannot be
created with the same for the column (e.g., a column names "T") and
when it is inconvenient of impossible to alias the column with
\"SELECT ... AS\"

"
  ; COMPILE TIME CHECKS
  ; Validate both variable list and column aliases are unique (at
  ; compile time)
  (check-unique-elements (map 'list #'do-rows-var vars))

  ; Conceivably the caller MIGHT want to bind two different loop
  ; variables to the same SELECTed column, but more likely that is a bug
  ; on his part, so don't allow it.
  (check-unique-elements (map 'list #'do-rows-col vars))

  ; Declare variables and bind to fetch from appropriate array index.
  ; Generate a map of the bound vars to gensyms which contain the index of that
  ; var into the fetched array.
  (let ((fetch-result (gensym))
        (saved-oracle-connection (gensym))
        (index-vars (make-hash-table)))
    (dolist (v vars)
            (setf (gethash (to-string (do-rows-var v)) index-vars) (gensym)))
    (list 'let
          ; Declare saved Oracle connection and calculated array indices OUTSIDE fetch loop
          (append `((,saved-oracle-connection *oracle-connection*))
                        (map 'list
                             #'(lambda (v) (list (gethash (to-string (do-rows-var v)) index-vars)
                                                 (list 'do-rows-index-of (list 'quote v))))
                             vars))
          ; Emit the DO loop itself
          (append (list
                   'do*
                   (append (list `(,fetch-result (fetch 'array) (fetch 'array)))
                             (map 'list
                                  #'(lambda (k)
                                      (let ((iter (list 'aref-null fetch-result (gethash (to-string (do-rows-var k)) index-vars))))
                                        (list (do-rows-var k) iter iter)))
                                  vars))
                   (list `(null ,fetch-result) '(row-count)))
                  (if (atom body) (list body) body)
                  (list `(setf *oracle-connection* ,saved-oracle-connection))))))

;----------------------------------------------

; FETCH
(defun fetch (&optional (result-type 'ARRAY))
"(ORACLE:FETCH (&optional (result-type 'ARRAY))))

Fetch a row of data.  Returns a row of values corresponding to the
columns of the SELECT statment.  The row data is returned in one of
three different forms, depending on the supplied result type:

    ARRAY: Values will be returned in an array with the same number of
           columns as in the SELECT statment, in the same order.
           This is the default.

    PAIRS: A list of pairs (column, value) will be returned.  The
           number and order of pairs is the same as the columns in the
           SELECT statement.

    HASH:  A hash table whose keys are the column names and whose values
           are the column values in the row.  The SELECT columns MUST
           BE UNIQUE and be valid Lisp symbols to use this option. If
           you are SELECT-ing an expression, you need to use a column
           alias: \"SELECT <expr> AS some_alias ...\"

Oracle data types are converted to Lisp datatypes as follows:
    - Numbers are converted to Lisp numeric types (fixnum/bignum/float)
    - NULL values are converetd to Lisp's NIL
    - Strings (char, varchar, varchar2) are left as Lisp strings
    - Dates are converted to strings of the form \"YYYY-MM-DD HH:MM:SS\"
      where HH is 24-hour form.
    - RAW and LONG RAW are converted to hexadecimal strings

Returns NIL if no rows are left (the EOF predicate can be before
a fetch to test this condition).

Arguments: none
"
  (check-connection "fetch a row of data")

  (let (result)
    ; Three cases: (1) have lookahead - use it (2) at EOF (3) Do a real fetch
    (cond
     ; Use pending look-ahead row and reset, else do a "real" fetch
     ((db-pending-row *oracle-connection*)
      (setf result (row-to-result (db-pending-row *oracle-connection*) result-type)
			(db-pending-row *oracle-connection*) nil))

     ; Check if already at EOF from previous fetches
     ((and (db-fetch-called *oracle-connection*)
           (let ((oracle-eof (lisp-truth (oracle_eof (curconn)))))
             (check-success)
             oracle-eof))
      ; Do nothing
      )

     ; Do a real fetch from Oracle
     (t (let ((fetch_status (oracle_fetch_row (curconn))))
          (cond ((not (lisp-truth fetch_status))
                 (db-error (oracle_last_error (curconn))))
                ((= fetch_status 2)
                 ; Newly arrived at EOF - do nothing
                 )
                (t ; Good fetch - get and convert row data
                 (setf result (oracle_row_values (curconn)))
                 (check-success)

                 ; Convert NULL values to NIL
                 (map-into result #'from-sqlval result)

                 ; Convert number and string types to Lisp based on Oracle type
                 (let ((colinfo (oracle_column_info (curconn))))
                   (check-success)
                   (map-into result #'convert-type result colinfo)
                   result))))))
    ; Set the flag that fetch was called at least once.
    ; This is to avoid further fetch calls to underlying Oracle library
    ; when we are already at EOF.
    (setf (db-fetch-called *oracle-connection*) t)
    (row-to-result result result-type)))


;----------------------------------------------

; FETCH-ALL
(defun fetch-all (&optional max-rows (result-type 'ARRAY) (item-type 'ARRAY))
"(ORACLE:FETCH-ALL(&optional max-rows (result-type 'ARRAY) (item-type 'ARRAY))

Fetch all rows from a query and return result as a sequence of
sequences.  Arguments (all optional) are:

   max-rows      Maximum number of rows to fetch
   result-type   Sequence type of row set 'ARRAY (default) or 'LIST
   item-type     Sequence type of columns per row,'ARRAY (default) or 'LIST
"
  (check-connection "fetch all rows of data")
  (when (not (find result-type '(array list)))
	(db-error (cat "Result type '" result-type "' should be 'ARRAY or 'LIST")))
  (when (not (find item-type '(array list)))
	(db-error (cat "Item type '" item-type "' should be 'ARRAY or 'LIST")))

  (do ((result (make-array 100 :element-type item-type
			   :fill-pointer 0 :adjustable t))
       (count 0 (1+ count))
       (row (oracle:fetch) (oracle:fetch)))
      ((or (null row) (when max-rows (>= count max-rows)))
       (coerce result result-type))
      (vector-push-extend (coerce row item-type)
				      result)))

;----------------------------------------------

; PEEK
(defun peek (&optional (result-type 'ARRAY))
"(ORACLE:PEEK (&optional (result-type 'ARRAY)))

Peek at next row of data (without fetching it).  Same as fetch, except
does not advance the database cursor to the next row.  Returns NIL if
at EOF.  If data is available, returns row data as FETCH (see FETCH
for data format and conversions done).

Arguments: none
"
  (check-connection "peek at next row of data")
  (cond ((not (db-fetch-called *oracle-connection*))
         (setf (db-pending-row *oracle-connection*) (fetch)))
        ((and (not (db-pending-row *oracle-connection*))
              (not (eof)))
         (setf (db-pending-row *oracle-connection*) (fetch))))
  (row-to-result (db-pending-row *oracle-connection*) result-type))


;----------------------------------------------

; COLUMNS - Return column info for most recent SELECT
(defun columns ()
"(ORACLE:COLUMNS)

Returns an array of column information structures, one for each
result column in the most recent SELECT statement.  Each structure has
slots:

   NAME      =  Oracle colume name, or the expression selected.  If the
                query used a column alias, \"SELECT <expr> AS <name>\" this
                alias will be returned.
   TYPE      =  Oracle data type (VARCHAR, NUMBER, DATE, ...)
   SIZE      =  Oracle data length (useful mostly for character types)
   SCALE     =  For numeric types, number of digits to right of decimal
   PRECISION =  For numeric types, total number of significant digits
   NULL_OK   =  T if NULLs allowed, NIL if nulls are not allowed.

Arguments: none
"
  (check-connection "get column information")
  (let ((cached-info (db-colinfo *oracle-connection*)))
    (if cached-info
        cached-info
      (let ((result (oracle_column_info (curconn))))
        (check-success)
        ; Convert C truth to Lisp for export
        (map-into result
                  #'(lambda (col)
					  ; Oracle identifies FLOAT using special value -127 for scale,
					  ; (which is irrelevant for floats).  In this case, map to "FLOAT"
					  ; for type name and NIL for scale.  Precision will be given in bits
					  ; as ANSI specifies
					  (when (and (equal (sqlcol-type col) "NUMBER") (= (sqlcol-scale col) -127) (not (= 0 (sqlcol-precision col))))
						    (setf (sqlcol-type col) "FLOAT"))
                      (setf (sqlcol-null_ok col) (lisp-truth (sqlcol-null_ok col)))
                      col)
                  result)
        (setf (db-colinfo *oracle-connection*) result)
        result))))

;----------------------------------------------

; EOF
(defun eof ()
"(ORACLE:EOF)

Returns EOF status.  A SELECT query is consdiered at EOF if the next
FETCH will return no data.  Must be connected to a database, and have
an active SELECT statement.

Arguments: none
"
  (check-connection "determine if at fetch EOF")
  (cond
   ((not (db-fetch-called *oracle-connection*))
    (null (peek)))
   ((db-pending-row *oracle-connection*)
    nil)
   (t (let ((oracle-eof (lisp-truth (oracle_eof (curconn)))))
        (check-success)
        (when (not oracle-eof)
              (setf (db-pending-row *oracle-connection*) (fetch)))
        (null (db-pending-row *oracle-connection*))))))

;----------------------------------------------

; INSERT-ROW
(defun insert-row (table vals)
"(ORACLE:INSERT-ROW table values)

Inserts a row into table.  First argument is a table name, second
argument is a map of column names to values, either a hash table or a
list of (name, value) pairs.  Columns missing from the map will be
given the default Oracle value, or NULL.

Returns: the number of rows inserted (i.e., 1).
"
  (when (null vals) (db-error "NULL name -> value map given"))
  (setf vals (check-pairs vals))
  (when (= 0 (hash-table-count vals)) (db-error "Empty column map given"))
  ; Build the INSERT statement
  (let ((sql (cat "INSERT INTO " table " ("
                  (comma-list-of-keys vals)
                  ") VALUES ("
                  (comma-list-of-keys vals t)
                  ")")))
    (run-sql sql vals)))

;----------------------------------------------

; UPDATE-ROW
(defun update-row (table condition vals &optional params)
"(ORACLE:UPDATE-ROW table condition values &optional params)

Updates rows in a table.  First argument is the table.  Second
argument is a condition expression for a WHERE clause (without the
\"WHERE\") which determines which rows are updated.  Third argument is
a map of columns to be updated to their new values.  The map may be
given as a hash or list of (name, value) pairs.  Last optional
argument is used to specify bind parameters that may occur in the
condition expression; this is most commonly done when the condition is
a match on a primary key, e.g.: \"pk_column = :pk_val\".

Returns: the number of rows updated.

"
  (when (null vals) (db-error "NULL name -> value map given"))
  (setf vals (check-pairs vals))
  (when (= 0 (hash-table-count vals)) (db-error "Empty column map given"))

  ; Build the UPDATE statement
  (let ((sql (cat "UPDATE " table " SET "))
        (plural nil))
    (loop for hkey being each hash-key of vals do
          (if plural
              (setf sql (cat sql ", "))
            (setf plural t))
          (setf sql (cat sql hkey " = :" hkey)))
    (setf sql (cat sql " WHERE " condition))
    ; Note we need to convert params to hash to combine it
    (run-sql sql (hash-combine (check-pairs params) vals))))

;----------------------------------------------

; ROW-COUNT
(defun row-count ()
"(ORACLE:ROW-COUNT)

For SELECT statements, returns the number of rows fetched (NOT peeked)
so far.  For other statements (INSERT/UPDATE/DELETE), returns the
number of rows inserted/updated/deleted.  Must be connected to a
database and have an active SQL statement.

Arguments: none
"
  (check-connection "get number of rows fetched or modified")
  (let ((rowcount (oracle_rows_affected (curconn))))
    (check-success)
    ; Maybe adjust downward to account for lookahead row
    (if (db-pending-row *oracle-connection*)
        (- rowcount 1)
      rowcount)))

;----------------------------------------------

; AUTO-COMMIT
(defun auto-commit (enable)
"(ORACLE:AUTO-COMMIT)

Enables or disables auto-commit.  When auto-commit is enabled,
modifications to the database are committed (made permanent) after
each call to RUN-SQL.  With auto-commit disabled, it is the callers
responsibility to explictly commit (or abort) changes by calling
COMMIT (or ROLLBACK), or to ensure transactional integrity by using
the WITH-TRANSACTION macro.  This function returns the previous status
of auto-commit.  This function may not be called inside the
WITH-TRANSACTION macro.

Arguments: (Boolean) Whether to enable auto-commit.
"
  (when *oracle-in-transaction* (db-error "Setting of AUTO-COMMIT not allowed inside WITH-TRANSACTION"))
  (auto-commit-nocheck enable))

; Private version that does not check if in transaction
(defun auto-commit-nocheck (enable)
  (check-connection "enable/disable auto-commit")
  (let ((old-value (lisp-truth (oracle_set_auto_commit (curconn) (c-truth enable)))))
    (check-success)
    old-value))

;----------------------------------------------

; WITH-TRANSACTION
(defmacro with-transaction (&body body)
"(ORACLE:WITH-TRANSACTION (&body body))

Executes Lisp code atomically as a transaction, ensuring that either
all the database operations complete successfully, or none of them do.
If there are any pending un-committed changes when this macro is
called, they are ROLLED BACK so that the database is affected only by
the updates inside the macro body.  Nesting of the macro is not
allowed and will produce an error.  There is no effect on the status
of auto-commit; it resumes its previous state when the macro exits.
The value returned by the macro is that of the last form in the macro
body.

"
  (let ((prev-auto-commit (gensym))
        (commit-ok (gensym))
        (result (gensym)))
    `(progn
       ; Check nesting
       (when *oracle-in-transaction* (db-error "Nesting of WITH-TRANSACTION is not allowed."))
       (let ((,prev-auto-commit t)
             (,commit-ok nil))
         (unwind-protect
             (progn ; Mark us as inside the macro
                    (setf *oracle-in-transaction* t)
                    ; Turn off auto-commit and save for later restore
                    (setf ,prev-auto-commit (auto-commit-nocheck nil))
                    ; Roll back any pending operation so that the database sees
                    ; only the effects of what's inside this macro
                    (if (not ,prev-auto-commit) (rollback-nocheck))
                    (setf ,result (progn ,@body))
                    (commit-nocheck)
                    (setf ,commit-ok t)
                    ,result)
           ; Cleanup
           (progn (when (not ,commit-ok) (rollback-nocheck)) ; Only roll back if need to
                  (auto-commit-nocheck ,prev-auto-commit)
                  (setf *oracle-in-transaction* nil)))))))


;----------------------------------------------

; COMMIT
(defun commit ()
"(ORACLE:COMMIT)

Commits (makes permanent) any pending changes to the database.  The
auto-commit feature must be OFF to use this function, nor can it be
called inside the WITH-TRANSACTION macro. Always returns NIL.

Argument: none
"
  (when *oracle-in-transaction* (db-error "COMMIT not allowed inside WITH-TRANSACTION"))
  (commit-nocheck))

(defun commit-nocheck ()
  (check-connection "commit transaction")
  (oracle_commit (curconn))
  (check-success)
  nil)

;----------------------------------------------

; ROLLBACK
(defun rollback ()
"(ORACLE:ROLLBACK)

Rolls back (abandons) any pending changes to the database.  The
auto-commit feature must be OFF to use this function, nor can it be
called insde the WITH-TRANSACTION macro.  Always returns NIL.

Argument: none
"
  (when *oracle-in-transaction* (db-error "ROLLBACK not allowed inside WITH-TRANSACTION"))
  (rollback-nocheck))

(defun rollback-nocheck ()
  (check-connection "rollback transaction")
  (oracle_rollback (curconn))
  (check-success)
  nil)


; =-=-=-=-=-=-=-   INTERNAL FUNCTIONS BELOW     =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

; Helper functions for DO-ROWS
(defun do-rows-var (v) (if (atom v) v (car v)))
(defun do-rows-col (v) (if (atom v) v (cadr v)))
(defun do-rows-index-of (v)
  (let ((i (position (to-string (do-rows-col v))
                     (map 'array
                          #'(lambda (x) (to-string (sqlcol-name x)))
                          (columns))
                     :test #'equal)))
    (when (null i)
          (db-error (cat "DO-ROWS: Column '" (do-rows-col v)
                      "' does not occur in query.  Allowed columns are:~%" (column-names))))
    i))
; COLUMN-NAMES
; Get list of column names, one per line.
(defun column-names ()
  (join "~%" (map 'list #'sqlcol-name (columns))))


; GETHASH-REQUIRED
; Get the value of a column - must exist if the hash table is non-empty
(defun gethash-required (key hash)
  (if (= 0 (hash-table-count hash))
      nil
    (multiple-value-bind
     (val exists) (gethash (to-string key) hash)
     (when (not exists) (db-error (cat "DO-ROWS: bound variable '" key "' does not occur in the query."
                                    "~%The allowed column/variable names are:~%~%" (column-names)
                                    "~%")))
     val)))

; ROW-TO-RESULT
; Convert fetched row array data to result type
(defun row-to-result (row result-type)
  (cond ((null row) nil)
        ((eq result-type 'ARRAY) row)
        ; ((eq result-type 'HASH) (pairs-to-hash (row-to-result row 'PAIRS)))
        ((eq result-type 'HASH) (array-to-hash (row-to-result row 'ARRAY)))
        ((eq result-type 'PAIRS)
         (let ((colinfo (oracle_column_info (curconn))))
           (check-success)
           (cond
            ((null row) nil)
            (t (map 'list
                    #'(lambda (col rowval)
                        (list (sqlcol-name col) rowval))
                    colinfo row)))))
        (t (db-error (cat "Invalid result type '" result-type "' given - should be 'ARRAY, 'PAIRS or 'HASH")))))


; CHECK-SUCCESS
; Check Oracle success code after calling a function.  Assumes (check-connection) was called!
(defun check-success ()
  (if (not (lisp-truth (oracle_success (curconn))))
      (db-error (oracle_last_error (curconn))))
  t)

; Convert Oracle type based on sqlcol data type.  Oracle numerics are converted
; to the appropriate internal Lisp type using READ-FROM-STRING.  NULL is retained
; as Lisp NIL, BLOB and BFILE are converted to array of bytes,
; and strings and dates are left as Lisp string.
(defun convert-type (val sc)
  (let ((dtype (sqlcol-type sc)))
    (cond ((null val) nil)
		  ((find dtype '("BLOB" "BFILE") :test #'equal)
		   (hex-to-byte-array val))
          ((find dtype '("NUMBER" "INTEGER" "FLOAT") :test #'equal)
		   (let ((old-default-format *read-default-float-format*))
			 ;; Adjust default float format, read, then restore old value
			 (unwind-protect
				 (progn
				   (setf *read-default-float-format* 'double-float)
				   (read-from-string val))
			   (setf *read-default-float-format* old-default-format))))
          ((find dtype '("VARCHAR" "DATE" "CHAR" "VARCHAR2" "LONG" "RAW" "LONG RAW" "CLOB" "ROWID DESC") :test #'equal)
           val)
          (t (db-error (cat "Unsupported data type '" dtype "'"))))))

; TO-SQLVAL
; Return a SQL val for LISP object, handling null case
(defun to-sqlval (x)
  (if (null x)
      (make-sqlval :data "" :is_null 1)
    (make-sqlval :data (to-string x) :is_null 0)))

; FROM-SQLVAL
; Return Lisp Object (string or NIL) for SQL val, handling null case
(defun from-sqlval (x)
  (if (lisp-truth (sqlval-is_null x))
      nil
    (sqlval-data x)))

; ROWVAL
; Return string value of an SQLVAL (row value), or "" if null
(defun rowval (row) (if (= 0 (sqlval-is_null row))
                        (sqlval-data row)
                      nil))

; HASH-TO-SQLPARAM-ARRAY
; Convert a hash table map of name->value strings to an array of SQL
; bind params suitable for passing to ORACLE_EXEC_SQL
(defun hash-to-sqlparam-array (h)
  (if (null h) (setf h (make-hash-table :test #'equal)))
  (let* ((count (hash-table-count h))
         (result (make-array count))
         (i 0))
    (loop for key being the hash-keys of h do
          (let ((val (gethash key h)))
            (when (not (atom key))
                  (db-error "Non-atom parameter name in bind-parameter hash"))
            (when (not (atom val))
                  (db-error "Non-atom parameter value in bind-parameter hash"))
            (setf (aref result i) (make-sqlparam :name (to-string key) :value (to-sqlval val)))
            (incf i)))
    result))

; CHECK-CONNECTION
; Check we are connected before doing an operation that requires a connection
(defun check-connection (&optional action)
  (if (null (curconn))
      (db-error (cat "Attempt to "
                  (if-null action "perform database operation")
                  " when not connected to any database"))))

; CONNECTION-KEY
; Construct key suitable for use in hash table keyed on
; unique triple of (user, schema, server) and optional connection ID
(defun connection-key (user schema server &optional connid)
  ; Use ~-delimited string - pretty disgusting, eh?
  (let ((result (cat (string-upcase user) "~" (string-upcase schema) "~" (string-upcase server))))
	(when connid
	  (setf result (cat result "~" connid)))
	result))

; PAIRS-TO-HASH
; Convert a list of pairs ((key1 val1) (key2 val2) ...) to hash, enforcing key uniqueness
(defun pairs-to-hash (plist)
  (if (null plist)
      nil
    (let ((result (make-hash-table :test #'equal)))
      (loop for p in plist do
            (let ((key (string-upcase (to-string (first p))))
                  (value (second p)))
              (when (not (valid-symbol key)) (db-error (cat "Column or parameter '" key "' is not a valid Lisp symbol name."
                                                         "~%Consider using SELECT ... " key " AS <column alias>")))
              ; Check uniqueness
              (multiple-value-bind
               (curval already-there) (gethash key result)
               (when already-there (db-error (cat "Column or parameter '" key
                                               "' appears twice in list of (name, value) pairs,~%first with value '"
                                               curval "' and again with value '" value "'.  Columns/parameters given were:~%"
                                               (join "~%" (map 'list #'car plist))
                                               (nl)))))
              (setf (gethash key result) value)))
      result)))


; CHECK-PAIRS
; Convert pairs to hash if needed
(defun check-pairs (p)
  (cond ((null p) (make-hash-table :test #'equal))
        ((eq (type-of p) 'HASH-TABLE) p)
        ((eq (type-of p) 'CONS) (pairs-to-hash p))
        (t (db-error (cat "Invalid type for name -> value map: '" (type-of p) "' - should be hash or list of pairs.")))))

; COMMA-LIST-OF-KEYS
; Return keys of hash table as comma-separated list.  If flag given,
; also pre-pend a colon to the name
(defun comma-list-of-keys (h &optional (colon nil))
  (let ((result "")
        (plural nil))
    (loop for hkey being each hash-key of h do
          (if plural
              (setf result (cat result ", "))
            (setf plural t))
          (when colon (setf result (cat result ":")))
          (setf result (cat result hkey)))
    result))

; IS-SELECT-QUERY
; Examine string to see if it begins with "SELECT".  Useful to auto-detect
; the mode (SELECT vs. non-SELECT for executing statements.
(defun is-select-query (s)
  (let ((start (string-trim '(#\Space #\Tab #\Newline) (string-upcase s))))
    (equal "SELECT" (subseq start 0 6))))

; =-=-=-=-=-=-=-   C WRAPPER FUNCTIONS  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

(c-lines "#include <oci.h>~%")
(c-lines "#include \"oiface.h\"~%")

; CONNECT
(def-call-out oracle_connect (:arguments (user           c-string)
                                         (schema         c-string)
                                         (password       c-string)
                                         (server         c-string)
                                         (prefetch_bytes int)
                                         (auto_commit    int)
                                         (long_len       int)
                                         (truncate_ok    int))
                             (:return-type c-pointer))

; DISCONNECT
(def-call-out oracle_disconnect (:arguments (db c-pointer))
                                (:return-type int  ))

; RUN SQL
(def-call-out oracle_exec_sql (:arguments (db         c-pointer)
                                          (sql        c-string)
                                          (params     (c-array-ptr (c-ptr sqlparam)))
                                          (is_command int))
                              (:return-type int))

; NO. OF COLUMNS
(def-call-out oracle_ncol (:arguments (db c-pointer))
                          (:return-type int))

; COLUMN INFO
(def-call-out oracle_column_info (:arguments (db c-pointer))
                                 (:return-type (c-array-ptr (c-ptr sqlcol))))

; FETCH
(def-call-out oracle_fetch_row (:arguments (db c-pointer))
                               (:return-type int))

; EOF
(def-call-out oracle_eof (:arguments (db c-pointer))
                         (:return-type int))

; SUCCESS
(def-call-out oracle_success (:arguments (db c-pointer))
                             (:return-type int))

; ROW VALUES
(def-call-out oracle_row_values (:arguments (db c-pointer))
                                (:return-type (c-array-ptr (c-ptr sqlval))))

; NO. ROWS AFFECTED
(def-call-out oracle_rows_affected (:arguments (db c-pointer))
                                   (:return-type int))

; COMMIT
(def-call-out oracle_commit (:arguments (db c-pointer))
                            (:return-type int))

(def-call-out oracle_rollback (:arguments (db c-pointer))
                              (:return-type int))

(def-call-out oracle_set_auto_commit (:arguments (db c-pointer)
                                                 (auto_commit int))
                                     (:return-type int))

; ERROR
(def-call-out oracle_last_error (:arguments (db c-pointer))
                                (:return-type c-string))


; =-=-=-=-=-=-=-   LOW LEVEL UTILITY FUNCTIONS  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

; IF-NULL
; Default a null value.  Is there a better Lisp built-in for this?
(defun if-null (value default) (if (null value) default value))

; AREF-NULL
; Do an AREF, but allow array to be null, in which case return NIL
(defun aref-null (a i) (if (null a) nil (aref a i)))

; HASH-COMBINE
; Combine two hash table.  Keys of the second hash will overwrite.
(defun hash-combine (h1 h2)
  (cond ((null h1) h2)
        ((null h2) h1)
        (t (loop for hkey being each hash-key of h2 do
                 (setf (gethash hkey h1)
                       (gethash hkey h2)))
           h1)))

; VALID-SYMBOL
; Test whether string is a valid Lisp symbol name
(defun valid-symbol (x)
  (equal (string-upcase (to-string x))
         (to-string (read-from-string x))))

; TO-STRING
; Convert object to a string; NIL -> ""
(defun to-string (s)
  (cond ((null s) "")
        ((stringp s) s)
        ((symbolp s) (symbol-name s))
        (t (format nil "~A" s))))

; CAT
; Concatenate strings
(defun cat (&rest args)
  (apply #'concatenate 'string (mapcar #'to-string (flatten args))))

; ARRAY-TO-HASH
; Convert array of row values to hash using column info
(defun array-to-hash (row)
  (if (null row)
      nil
    (let* ((cols (columns))
           (n (length row))
           (result (make-hash-table :test #'equal :size n)))
      (loop for i from 0 to (- n 1) do
            (setf (gethash (to-string (sqlcol-name (aref cols i))) result) (aref row i)))
      result)))

; CHECK-UNIQUE-ELEMENTS
; Does list consist of unqiue, non-null elements
(defun check-unique-elements (l)
  (let ((h (make-hash-table :test #'equal)))
    (dolist (elt l)
            (when (null elt)
                  (db-error "Null element in column/variable list"))
            (when (gethash (to-string elt) h)
                  (db-error (cat "DO-ROWS: Parameter/column '" elt "' occurs more than once in bound columns/variables:~%"
                              (join "~%" l))))
            (setf (gethash (to-string elt) h) t))
    t))

; JOIN
; Join a sequence of strings into one, separating with delimeter
; I'll probably get shot for this implementation.  Better way?
(defun join (delimiter seq)
  (let ((result ""))
    (loop for i from 0 to (- (length seq) 1) do
          (when (> i 0)
                (setf result (cat result delimiter)))
          (setf result (cat result (nth i seq))))
    result))

; WHILE (macro)
; While loop construct (lifted from Paul Graham)
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

; OUT
; Output functions
(defun out (&rest args)
  (format t "~A" (cat args)))
; OUT-NL
(defun out-nl (&rest args)
  (out args)
  (terpri))

; LISP-TRUTH
; Get Lisp truth of object, considering "C" 0/1 also.  Useful for
; taking booleans returned from "C"
(defun lisp-truth (x)
  (if (eq x 0) nil (not (null x))))

; C-TRUTH
; Get "C" truth of object (0 or 1).  Useful for passing args to "C"
(defun c-truth (x)
  (if (lisp-truth x) 1 0))

;; HEX-VALUE -- Get integer value of single upper-case hex digit
(defun hex-value (h)
  (cond ((and (char>= h #\A) (char<= h #\F))
         (+ 10 (- (char-code h) (char-code #\A))))
        ((and (char>= h #\a) (char<= h #\f))
         (+ 10 (- (char-code h) (char-code #\a))))
        ((and (char>= h #\0) (char<= h #\9))
         (- (char-code h) (char-code #\0)))
        (t (error "Invalid hex digit"))))

;; HEX-BYTE-VALUE -- Get byte value of pair of hex digits
(defun hex-byte-value (hh)
  (+ (* 16 (hex-value (elt hh 0)))
     (hex-value (elt hh 1))))

;; HEX-TO-BYTE-ARRAY -- Convert hex string to byte array
(defun hex-to-byte-array (h)
  (let* ((size (/ (length h) 2))
         (result (make-array size :element-type '(unsigned-byte 8))))
    (loop
     for i from 0 to (1- size) do
     (let ((offset (* 2 i)))
       (setf (elt result i)
             (coerce (hex-byte-value (subseq h offset (+ 2 offset)))
                     '(unsigned-byte 8)))))
    result))

; FLATTEN
; Flatten list (lifted from Paul Graham)
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

; NL
; Return newline
(defun nl () (format nil "~%"))

; DB-ERROR - Throw an error
(defun db-error (message)
  (error message))

; End of oracle.lisp
