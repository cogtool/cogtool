;;; PostgreSQL higher level functions
;;;
;;; Copyright (C) 1999-2005 by Sam Steingold
;;; Distributed under the GNU GPL2 <http://www.gnu.org/copyleft/gpl.html>:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.

(require "postgresql")

(in-package "SQL")

;;;
;;; Helper Functions
;;;

(defvar *sql-log* nil "The PostgreSQL log stream or NIL.")
(defvar *sql-login* "postgres" "The default PostgreSQL login.")
(defvar *sql-password* "postgres" "The default PostgreSQL passowrd.")

(define-condition sql-error (error)
  ((type :type symbol :reader sql-type :initarg :type)
   (mesg :type simple-string :reader sql-mesg :initarg :mesg))
  (:report (lambda (cc stream)
             (format stream "[~a] ~a" (sql-type cc) (sql-mesg cc)))))

(defun pq-finish (conn)
  "if you do `PQfinish' twice on the same object, you will get segfault!"
  (when (and conn (validp conn))
    (PQfinish conn)
    (setf (validp conn) nil)))

(defun pq-clear (res)
  "if you do `PQclear' twice on the same object, you will get segfault!"
  (when (and res (validp res))
    (PQclear res)
    (setf (validp res) nil)))

(defun sql-error (conn res format-string &rest args)
  (pq-clear res) (pq-finish conn)
  (error 'sql-error :mesg (apply #'format nil format-string args)
         :type (if res :request :connection)))

(defun sql-connect (&key host port options tty name
                    (login *sql-login*) (password *sql-password*))
  (let ((conn (PQsetdbLogin host port options tty name login password)))
    (when conn (set-foreign-pointer conn :copy))
    (unless (and conn (= (PQstatus conn) CONNECTION_OK))
      (sql-error conn nil "~S(~S,~S,~S,~S,~S,~S,~S): ~S"
                 'sql-connect host port options tty name login password
                 (PQerrorMessage conn)))
    (when *sql-log*
      (format *sql-log* "~&Connection(~S) OK:~% db name: ~S
 host:port[tty]: ~S:~S[~S]~% options: ~S~%"
              conn (PQdb conn) (PQhost conn) (PQport conn)
              (PQtty conn) (PQoptions conn)))
    conn))

(defmacro with-sql-connection ((conn &rest options &key (log '*sql-log*)
                                     &allow-other-keys) &body body)
  `(let* ((*sql-log* ,log)
          (,conn (sql-connect ,@(ext:remove-plist options :log))))
     (unwind-protect (progn ,@body)
       ;; close the connection to the database and cleanup
       (pq-finish ,conn))))

(defun sql-transaction (conn command status &optional (clear-p t))
  (let ((res (PQexec conn command)))
    (when res (set-foreign-pointer res :copy))
    (unless (and res (= status (PQresultStatus res)))
      (sql-error conn res command "~S(~S,~S): ~S" 'sql-transaction
                 conn command (PQresultErrorMessage res)))
    (when *sql-log*
      (format *sql-log* " * OK: ~a~%" command))
    (when clear-p (pq-clear res))
    res))

(defmacro with-sql-transaction ((res conn command status) &body body)
  `(let ((,res (sql-transaction ,conn ,command ,status nil)))
    (unwind-protect (progn ,@body)
      ;; avoid memory leaks
      (pq-clear ,res))))

(pushnew "SQL" custom:*system-package-list* :test #'string=)

;;; file sql.lisp ends here

