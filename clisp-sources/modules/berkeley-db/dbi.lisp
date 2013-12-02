;;; Copyright (C) 2003-2005 by Sam Steingold
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See <http://www.gnu.org/copyleft/gpl.html>

(defpackage "BDB"
  (:use "COMMON-LISP" "EXT")
  (:nicknames "BERKELEY-DB" "BERKDB")
  (:shadowing-import-from "EXPORTING" #:defstruct #:define-condition)
  (:export #:db-version
           #:bdb-handle #:bdb-handle-parents #:bdb-handle-dependents
           #:dbe #:db #:txn #:dbc #:logc #:mpoolfile #:dblock #:lsn
           #:dbe-create #:dbe-close #:dbe-dbremove #:dbe-dbrename #:dbe-open
           #:dbe-remove #:dbe-set-options #:dbe-get-options #:dbe-messages
           #:db-create #:db-close #:db-del #:db-fd #:db-get #:db-stat
           #:db-open #:db-sync #:db-truncate #:db-upgrade #:db-remove
           #:db-rename #:db-put #:db-join #:db-key-range #:db-verify
           #:db-set-options #:db-get-options
           #:make-dbc #:dbc-close #:dbc-count #:dbc-del
           #:dbc-dup #:dbc-get #:dbc-put
           #:lock-detect #:lock-get #:lock-id #:lock-id-free #:lock-put
           #:lock-close #:lock-stat
           #:log-archive #:log-file #:log-flush #:log-put #:log-stat
           #:log-compare #:log-cursor #:logc-close #:logc-get
           #:txn-begin #:txn-abort #:txn-commit #:txn-discard #:txn-id
           #:txn-checkpoint #:txn-prepare #:txn-recover #:txn-set-timeout
           #:txn-stat
           #:with-dbe #:with-db #:with-dbc))

(setf (package-lock "EXT") nil)
(use-package '("BDB") "EXT")
(ext:re-export "BDB" "EXT")
(pushnew :berkeley-db *features*)
(in-package "BDB")

(setf (documentation (find-package "BDB") 'sys::impnotes) "berkeley-db")

;;; objects
(cl:defstruct (bdb-handle (:constructor nil) (:copier nil))
  (handle nil :read-only t)
  (parents nil)       ; parents cannot be closed until this is closed
  (dependents nil))  ; cannot close this until all dependents are closed
(cl:defstruct (dbe (:include bdb-handle) (:copier nil)
                (:constructor mkdbe (handle parents))))
(cl:defstruct (db (:include bdb-handle) (:copier nil)
               (:constructor mkdb (handle parents))))
(cl:defstruct (dbc (:include bdb-handle) (:copier nil)
                   (:constructor mkdbc (handle parents))))
(cl:defstruct (txn (:include bdb-handle) (:copier nil)
                (:constructor mktxn (handle parents))))
(cl:defstruct (logc (:include bdb-handle) (:copier nil)
                 (:constructor mklogc (handle parents))))
(cl:defstruct (mpoolfile (:include bdb-handle) (:copier nil)
                      (:constructor mkmpoolfile (handle parents))))
(cl:defstruct (dblock (:copier nil) (:constructor mkdblock (handle parent)))
  (handle nil :read-only t) parent)

(defun mkhandle (maker parents closer handle)
  "make BDB-HANDLE, add it to the DEPENDETS of its PARENT, call FINALIZE"
  (unless (listp parents) (setq parents (list parents)))
  (let ((bdb-handle (funcall maker handle parents)))
    (dolist (parent parents)
      (push bdb-handle (bdb-handle-dependents parent)))
    (finalize bdb-handle closer)
    bdb-handle))
(defun kill-handle (handle)
  "close all dependents, remove from parents' dependents"
  (mapc #'close (bdb-handle-dependents handle))
  (dolist (p (bdb-handle-parents handle))
    (setf (bdb-handle-dependents p)
          (delete handle (bdb-handle-dependents p)))))

(defstruct (lsn (:constructor mklsn (file offset)))
  (file 0 :type (unsigned-byte 32) :read-only t)
  (offset 0 :type (unsigned-byte 32) :read-only t))

(defstruct (db-stat (:constructor nil))
  (type nil :read-only t)
  (byte-swapped nil :read-only t)
  (magic nil :read-only t)
  (version nil :read-only t)
  (num-keys nil :read-only t)
  (num-data nil :read-only t)
  (page-size nil :read-only t))
(defstruct (db-stat-hash (:include db-stat)
                         (:constructor mkdbstat-hash
                                       (type byte-swapped magic version
                                        num-keys num-data page-size
                                        fill-factor num-buckets free bfree
                                        big-pages big-bfree overflows
                                        overflows-free dup dup-free)))
  (fill-factor nil :read-only t)
  (num-buckets nil :read-only t)
  (free nil :read-only t)
  (bfree nil :read-only t)
  (big-pages nil :read-only t)
  (big-bfree nil :read-only t)
  (overflows nil :read-only t)
  (overflows-free nil :read-only t)
  (dup nil :read-only t)
  (dup-free nil :read-only t))

(defstruct (db-stat-btree (:include db-stat)
                          (:constructor mkdbstat-btree
                                        (type byte-swapped magic version
                                         num-keys num-data page-size
                                         min-key re-len re-pad levels
                                         internal-pages leaf-pages dup-pages
                                         overflow-pager
                                         free int-pgfree leaf-pgfree dup-pgfree
                                         over-pgfree)))
  (min-key nil :read-only t)
  (re-len nil :read-only t)
  (re-pad nil :read-only t)
  (levels nil :read-only t)
  (internal-pages nil :read-only t)
  (leaf-pages nil :read-only t)
  (dup-pages nil :read-only t)
  (overflow-pager nil :read-only t)
  (free nil :read-only t)
  (int-pgfree nil :read-only t)
  (leaf-pgfree nil :read-only t)
  (dup-pgfree nil :read-only t)
  (over-pgfree nil :read-only t))
(defstruct (db-stat-queue (:include db-stat)
                          (:constructor mkdbstat-queue
                                        (type byte-swapped magic version
                                         num-keys num-data page-size
                                         extent-size pages re-len re-pad
                                         pg-free first-recno curr-recno)))
  (extent-size nil :read-only t)
  (pages nil :read-only t)
  (re-len nil :read-only t)
  (re-pad nil :read-only t)
  (pg-free nil :read-only t)
  (first-recno nil :read-only t)
  (curr-recno nil :read-only t))

(defstruct (db-lock-stat (:constructor
                          mklockstat
                          (id cur_maxid nmodes maxlocks
                           maxlockers maxobjects nlocks maxnlocks
                           nlockers maxnlockers nobjects maxnobjects
                           nrequests nreleases nnowaits nconflicts
                           ndeadlocks locktimeout nlocktimeouts
                           txntimeout ntxntimeouts regsize
                           region_wait region_nowait)))
  ;; The last allocated locker ID.
  (id 0 :type (unsigned-byte 32) :read-only t)
  ;; The current maximum unused locker ID.
  (cur_maxid 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of lock modes.
  (nmodes 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of locks possible.
  (maxlocks 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of lockers possible.
  (maxlockers 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of lock objects possible.
  (maxobjects 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of current locks.
  (nlocks 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of locks at any one time.
  (maxnlocks 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of current lockers.
  (nlockers 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of lockers at any one time.
  (maxnlockers 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of current lock objects.
  (nobjects 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of lock objects at any one time.
  (maxnobjects 0 :type (unsigned-byte 32) :read-only t)
  ;; The total number of locks requested.
  (nrequests 0 :type (unsigned-byte 32) :read-only t)
  ;; The total number of locks released.
  (nreleases 0 :type (unsigned-byte 32) :read-only t)
  ;; The total number of lock requests failing because DB_LOCK_NOWAIT was set.
  (nnowaits 0 :type (unsigned-byte 32) :read-only t)
  ;; The total number of locks not immediately available due to conflicts.
  (nconflicts 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of deadlocks.
  (ndeadlocks 0 :type (unsigned-byte 32) :read-only t)
  ;; Lock timeout value.
  (locktimeout 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of lock requests that have timed out.
  (nlocktimeouts 0 :type (unsigned-byte 32) :read-only t)
  ;; Transaction timeout value.
  (txntimeout 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of transactions that have timed out. This value is also
  ;; a component of ndeadlocks, the total number of deadlocks detected.
  (ntxntimeouts 0 :type (unsigned-byte 32) :read-only t)
  ;; The size of the lock region.
  (regsize 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times that a thread of control was forced to wait
  ;; before obtaining the region lock.
  (region_wait 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times that a thread of control was able to obtain the
  ;; region lock without waiting.
  (region_nowait 0 :type (unsigned-byte 32) :read-only t))

(defstruct (db-log-stat (:constructor
                         mklogstat
                         (magic version mode lg_bsize lg_size w_mbytes w_bytes
                          wc_mbytes wc_bytes wcount wcount_fill scount cur_file
                          cur_offset disk_file disk_offset maxcommitperflush
                          mincommitperflush regsize region_wait region_nowait)))
  ;; The magic number that identifies a file as a log file.
  (magic 0 :type (unsigned-byte 32) :read-only t)
  ;; The version of the log file type.
  (version 0 :type (unsigned-byte 32) :read-only t)
  ;; The mode of any created log files.
  (mode 0 :type int :read-only t)
  ;; The in-memory log record cache size.
  (lg_bsize 0 :type (unsigned-byte 32) :read-only t)
  ;; The current log file size.
  (lg_size 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of megabytes written to this log.
  (w_mbytes 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of bytes over and above w_mbytes written to this log.
  (w_bytes 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of megabytes written to this log since the last checkpoint.
  (wc_mbytes 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of bytes over and above wc_mbytes written to this log
  ;; since the last checkpoint.
  (wc_bytes 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times the log has been written to disk.
  (wcount 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times the log has been written to disk because the
  ;; in-memory log record cache filled up.
  (wcount_fill 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times the log has been flushed to disk.
  (scount 0 :type (unsigned-byte 32) :read-only t)
  ;; The current log file number.
  (cur_file 0 :type (unsigned-byte 32) :read-only t)
  ;; The byte offset in the current log file.
  (cur_offset 0 :type (unsigned-byte 32) :read-only t)
  ;; The log file number of the last record known to be on disk.
  (disk_file 0 :type (unsigned-byte 32) :read-only t)
  ;; The byte offset of the last record known to be on disk.
  (disk_offset 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of commits contained in a single log flush.
  (maxcommitperflush 0 :type (unsigned-byte 32) :read-only t)
  ;; The minimum number of commits contained in a single log flush that
  ;; contained a commit.
  (mincommitperflush 0 :type (unsigned-byte 32) :read-only t)
  ;; The size of the region.
  (regsize 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times that a thread of control was forced to wait
  ;; before obtaining the region lock.
  (region_wait 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times that a thread of control was able to obtain the
  ;; region lock without waiting.
  (region_nowait 0 :type (unsigned-byte 32) :read-only t))

(defstruct (db-txn-active (:constructor mktxnactive
                                        (txnid parentid lsn xa_status xid)))
  ;; The transaction ID of the transaction.
  (txnid 0 :type (unsigned-byte 32) :read-only t)
  ;; The transaction ID of the parent transaction (or 0, if no parent).
  (parentid 0 :type (unsigned-byte 32) :read-only t)
  ;; The current log sequence number when the transaction was begun.
  (lsn nil :type lsn :read-only t)
  ;; If the transaction is an XA transaction, the status of the
  ;; transaction, otherwise 0.
  (xa_status 0 :type (unsigned-byte 32) :read-only t)
  ;; If the transaction is an XA transaction, the transaction's XA ID.
  (xid nil :type (vector (unsigned-byte 8)
                         #,(dbe-get-options nil :DB-XIDDATASIZE))
       :read-only t))

(defstruct (db-txn-stat (:constructor mktxnstat
                                      (last_ckp time_ckp last_txnid maxtxns
                                       nactive maxnactive nbegins naborts
                                       ncommits nrestores regsize region_wait
                                       region_nowait txnarray)))
  ;; The LSN of the last checkpoint.
  (last_ckp nil :type lsn :read-only t)
  ;; The time the last completed checkpoint finished
  (time_ckp 0 :type integer :read-only t)
  ;; The last transaction ID allocated.
  (last_txnid 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of active transactions configured.
  (maxtxns 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of transactions that are currently active.
  (nactive 0 :type (unsigned-byte 32) :read-only t)
  ;; The maximum number of active transactions at any one time.
  (maxnactive 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of transactions that have begun.
  (nbegins 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of transactions that have aborted.
  (naborts 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of transactions that have committed.
  (ncommits 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of transactions that have been restored.
  (nrestores 0 :type (unsigned-byte 32) :read-only t)
  ;; The size of the region.
  (regsize 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times that a thread of control was forced to wait
  ;; before obtaining the region lock.
  (region_wait 0 :type (unsigned-byte 32) :read-only t)
  ;; The number of times that a thread of control was able to obtain the
  ;; region lock without waiting.
  (region_nowait 0 :type (unsigned-byte 32) :read-only t)
  ;; an array of NACTIVE DB-TXN-ACTIVE structures, describing the
  ;; currently active transactions.
  (txnarray nil :type vector :read-only t))

;;; macros (see macros2.lisp for `with-open-file')
(defmacro with-dbe ((var &key create options) &body forms)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY forms)
    `(LET ((,var (BDB:DBE-CREATE ,@create)))
       (DECLARE (READ-ONLY ,var) ,@declarations)
       ,@(when options `((BDB:DBE-SET-OPTIONS ,var ,@options)))
       (UNWIND-PROTECT (PROGN ,@body-rest)
         (WHEN ,var (BDB:DBE-CLOSE ,var))))))
(defmacro with-db ((var dbe file &key create options open) &body forms)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY forms)
    `(LET ((,var (BDB:DB-CREATE ,dbe ,@create)))
       (DECLARE (READ-ONLY ,var) ,@declarations)
       ,@(when options `((BDB:DB-SET-OPTIONS ,var ,@options)))
       (BDB:DB-OPEN ,var ,file ,@open)
       (UNWIND-PROTECT (PROGN ,@body-rest)
         (WHEN ,var (BDB:DB-CLOSE ,var))))))
(defmacro with-dbc ((var &rest options) &body forms)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY forms)
    `(LET ((,var (BDB:MAKE-DBC ,@options)))
       (DECLARE (READ-ONLY ,var) ,@declarations)
       (UNWIND-PROTECT (PROGN ,@body-rest)
         (WHEN ,var (BDB:DBC-CLOSE ,var))))))

(ext:without-package-lock ("CL")
(defmethod close ((dbe dbe) &key abort)
  (declare (ignore abort))
  (dbe-close dbe))
(defmethod close ((db db) &key abort)
  (declare (ignore abort))
  (db-close db))
(defmethod close ((cu dbc) &key abort)
  (declare (ignore abort))
  (dbc-close cu))
(defmethod close ((lock dblock) &key abort)
  (declare (ignore abort))
  (lock-close lock))
(defmethod close ((lc logc) &key abort)
  (declare (ignore abort))
  (logc-close lc))
(defmethod close ((tx txn) &key abort)
  (if abort (txn-abort tx) (txn-commit tx)))
)

(define-condition bdb-error (simple-error)
  (($errno :initarg :errno :reader bdb-error-number)))

;;; restore locks
(pushnew "BDB" custom:*system-package-list* :test #'string=)
(setf (package-lock custom:*system-package-list*) t)
