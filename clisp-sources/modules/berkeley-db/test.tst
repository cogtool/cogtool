;; -*- Lisp -*-
;; some tests for Berkeley-DB
;; clisp -K full -E 1:1 -q -norc -i ../tests/tests -x '(run-test "berkeley-db/test")'

(listp (show (multiple-value-list (ext:module-info "bdb" t)) :pretty t)) T

(multiple-value-bind (ve ma mi pa subsystems) (bdb:db-version t)
  (format t "~&Version: ~S (~D.~D.~D) ~S~%" ve ma mi pa subsystems))
NIL

;;; --- helpers ---
(defun kill-down (name)
  (dolist (f (directory (ext:string-concat name "**")))
    (format t "~&removing ~S~%" f)
    (if (pathname-name f)
        (delete-file f)
        (ext:delete-dir f))))
kill-down
(defun rmrf (name)
  (ext:dir (ext:string-concat name "**"))
  (kill-down name)
  (format t "~&removing ~S~%" name)
  (ext:delete-dir name))
rmrf
(defun prepare-dir (name)
  (ensure-directories-exist name :verbose t)
  (kill-down name))
prepare-dir
(defun show-db (db)
  (let* ((*print-pretty* t) (stat (bdb:db-stat db))
         (file (and (eq :RECNO (bdb:db-stat-type stat))
                    (bdb:db-get-options db :RE-SOURCE))))
    (show (list db (bdb:db-fd db) stat (bdb:db-get-options db)))
    (when file (show-file file)))
  nil)
show-db
(defun show-dbe (dbe)
  (let ((*print-pretty* t))
    (show (list* dbe :archive (bdb:log-archive dbe)
                 (bdb:txn-stat dbe) (bdb:lock-stat dbe) (bdb:log-stat dbe)
                 (bdb:dbe-get-options dbe)
                 (when (fboundp 'bdb:dbe-messages)
                   (list :messages (bdb:dbe-messages dbe))))))
  nil)
show-dbe
(defun show-file (file)
  (with-open-file (st file :direction :input)
    (format t "~&~S: ~:D byte~:P:~%" file (file-length st))
    (loop :for l = (read-line st nil nil) :while l
      :do (format t "--> ~S~%" l))))
show-file
(defun finish-file (file)
  (when (probe-file file) (show-file file))
  (delete-file file)
  (probe-file file))
finish-file
(progn
  (defmethod close :before ((h bdb:bdb-handle) &key abort)
    (declare (ignore abort))
    (let ((*print-pretty* t))
      (format t "~&Closing ~S~%" h)))
  (defmethod close :after ((h bdb:bdb-handle) &key abort)
    (declare (ignore abort))
    (let ((*print-pretty* t))
      (format t "~&Closed ~S~%" h)))
  nil)
nil

;;; preparations

(prepare-dir "bdb-home/") NIL
(prepare-dir "bdb-data/") NIL
(progn (delete-file "bdb-errors") (delete-file "bdb-msg") NIL) NIL

;;; creation

(defvar *dbe* (show (bdb:dbe-create))) *dbe*

(bdb:dbe-set-options *dbe* :errfile "bdb-errors" :msgfile "bdb-msg"
                     :errpfx "zot" :data-dir "bdb-data/")
NIL

(bdb:dbe-set-options *dbe* :verbose t)
NIL

(bdb:dbe-get-options *dbe* :errpfx) "zot"

(bdb:dbe-open *dbe* :home "bdb-home/" :create t
              :init-mpool t :init-txn t :init-lock t :init-log t)
NIL

(show-dbe *dbe*) NIL

(defvar *db* (let ((*print-pretty* t)) (show (bdb:db-create *dbe*)))) *db*

;; the actual file goes to ./bdb-data/bazonk.db !
(bdb:db-open *db* "bazonk.db" :type :BTREE :create t) NIL

(null (probe-file "./bdb-data/bazonk.db")) NIL

(bdb:db-put *db* "foo" "bar")
NIL
(bdb:db-put *db* "fep" "blicket")
NIL

(bdb:db-sync *db*) NIL
(show-db *db*) NIL
(close *db*)   T

;;; recno with an underlying text file
(with-open-file (s "bdb-data/recno-source.txt" :direction :output
                   :external-format :unix :if-exists :supersede)
  (write-line "foo" s)
  (write-line "bar" s)
  (write-line "foobar" s)
  (file-length s))
15

(bdb:with-db (db *dbe* "recno-source.db"
                 :options (:RE-SOURCE "recno-source.txt")
                 :open (:type :RECNO :create T))
  (show-db db)
  (list (bdb:db-get db 1 :type :string)
        (bdb:db-get db 2 :type :string)
        (bdb:db-get db 3 :type :string)
        (bdb:db-get db 4 :error nil)))
("foo" "bar" "foobar" :NOTFOUND)

(bdb:with-db (db *dbe* "recno-source.db"
                 :options (:RE-SOURCE "recno-source.txt"))
  (show-db db)
  (bdb:db-put db 5 "bazonk"))
NIL

(bdb:with-db (db *dbe* "recno-source.db" :open (:rdonly t))
  (show-db db)
  (bdb:with-dbc (cu db)
    (list
     (loop :with key :and val
       :do (setf (values key val)
                 (bdb:dbc-get cu :INTEGER :STRING :NEXT :error nil))
       :until (eq key :notfound)
       :collect (list key val))
     (bdb:db-get db 4 :error nil))))
(((1 "foo") (2 "bar") (3 "foobar") (5 "bazonk")) :KEYEMPTY)

(with-open-file (s "bdb-data/recno-source.txt" :direction :input)
  (loop :for l = (read-line s nil nil) :while l :collect l))
("foo" "bar" "foobar" "" "bazonk")

;;; write factorials into (:BTREE :HASH)
(dolist (type '(:btree :hash))
  (show type)
  (bdb:with-db (db *dbe* (format nil "test-~A.db" type)
                   :open (:type type :create t))
    (show-db db)
    (show (loop :repeat 20 :for x = (random 30) :for x! = (! x)
            :collect (list x x! (bdb:db-put db x x!))))))
NIL

;;; write factorials into (:QUEUE :RECNO)
(dolist (type '(:queue :recno))
  (show type)
  (let ((max 30))
    (bdb:with-db (db *dbe* (format nil "test-~A.db" type)
                     :options (:RE-LEN (show (* 4 (ceiling (integer-length
                                                            (! max)) 32)))
                               :RE-PAD 0)
                     :open (:type type :create t))
      (show-db db)
      (show (loop :repeat 20 :for x = (random max) :collect
              (list (bdb:db-put db nil x :action :APPEND) x
                    (bdb:db-put db nil (! x) :action :APPEND) (! x))))
      (show db))))
NIL

;; locks - will NOT be automatically closed by DBE-CLOSE
(defparameter *locker* (show (bdb:lock-id *dbe*))) *locker*
(defparameter *lock* (show (bdb:lock-get *dbe* "foo" *locker* :READ)))
*lock*

(close *dbe*) T

(block nil
  (handler-bind ((bdb:bdb-error
                  (lambda (c)
                    (format t "~&~A~%" c)
                    (return (integerp (bdb:bdb-error-number c))))))
    (close *lock*)))
T

(ext:dir "bdb-home/**") NIL
(ext:dir "bdb-data/**") NIL
(finish-file "bdb-errors") NIL
(finish-file "bdb-msg") NIL

;;; access

(let ((*print-pretty* t)) (setq *dbe* (show (bdb:dbe-create))) nil) NIL

(bdb:dbe-set-options *dbe* :errfile "bdb-errors" :msgfile "bdb-msg"
                     :data-dir "bdb-data/")
NIL

(bdb:dbe-set-options *dbe* :verbose t)
NIL

(let ((arr #A((unsigned-byte 8) (6 6)
              ((0 0 0 0 0 0)
               (0 0 1 1 0 1)
               (0 1 1 1 1 1)
               (0 1 1 0 0 0)
               (0 0 1 0 0 0)
               (0 1 1 0 0 0)))))
  (bdb:dbe-set-options *dbe* :lk-conflicts arr)
  (equalp arr (bdb:dbe-get-options *dbe* :lk-conflicts)))
T

(bdb:dbe-open *dbe* :home "bdb-home/" :create t
              :init-mpool t :init-txn t :init-lock t :init-log t)
NIL

(show-dbe *dbe*) NIL

(let ((*print-pretty* t)) (setq *db* (show (bdb:db-create *dbe*))) nil) NIL

(bdb:db-open *db* "bazonk.db" :rdonly t) NIL

(show-db *db*) NIL

(= (bdb:db-get-options *db* :errfile) (bdb:dbe-get-options *dbe* :errfile)) T
(eql (bdb:db-get-options *db* :msgfile) (bdb:dbe-get-options *dbe* :msgfile)) T

(defvar *cursor* (show (bdb:make-dbc *db*))) *cursor*

(let ((li ()))
  (loop (multiple-value-bind (key val)
            (bdb:dbc-get *cursor* :STRING :STRING :NEXT :error nil)
          (when (eq key :notfound) (return li))
          (format t "~&=[count=~D]=> ~S -> ~S~%"
                  (bdb:dbc-count *cursor*) key val)
          (push (list key val) li))))
(("foo" "bar") ("fep" "blicket"))

(bdb:db-get *db* "bar" :error nil :type :raw)
:NOTFOUND

(bdb:db-get *db* "foo")
#(98 97 114)                    ; "bar"

(close *cursor*) T
(close *db*)     T

(let ((*print-pretty* t)) (setq *db* (show (bdb:db-create *dbe*))) nil) NIL
(bdb:db-open *db* "bazonk.db") NIL
(bdb:db-truncate *db*)      2   ; the number of records discarded
(multiple-value-list (bdb:db-get-options *db* :dbname)) ("bazonk.db" NIL)
(close *db*)         T

;;; read factorials from (:BTREE :HASH)
(let ((errors ()))
  (dolist (type '(:btree :hash) (nreverse errors))
    (show type)
    (push (list type) errors)
    (bdb:with-db (db *dbe* (format nil "test-~A.db" type) :open (:rdonly t))
      (show-db db)
      (bdb:with-dbc (cu db)
        (loop (multiple-value-bind (key val)
                  (bdb:dbc-get cu :INTEGER :INTEGER :NEXT :error nil)
                (when (eq key :notfound) (return))
                (format t "~&=[count=~D]=> ~S -> ~S~%"
                        (bdb:dbc-count cu) key val)
                (unless (= (! key) val)
                  (push (list :count (bdb:dbc-count cu) :key key :val val
                              :key! (! key))
                        (car errors)))))
        (setf (car errors) (nreverse (car errors)))))))
((:BTREE) (:HASH))

;;; read factorials from (:QUEUE :RECNO)
(let ((errors ()))
  (dolist (type '(:queue :recno) (nreverse errors))
    (show type)
    (push (list type) errors)
    (bdb:with-db (db *dbe* (format nil "test-~A.db" type) :open (:rdonly t))
      (show-db db)
      (bdb:with-dbc (cu db)
        (loop (multiple-value-bind (key val)
                  (bdb:dbc-get cu :INTEGER :INTEGER :NEXT :error nil)
                (when (eq key :notfound) (return))
                (format t "~&=[count=~D]=> ~S -> ~S~%"
                        (bdb:dbc-count cu) key val)
                (multiple-value-bind (key1 val1)
                    (bdb:dbc-get cu :INTEGER :INTEGER :NEXT)
                  (format t "~&=[count=~D]=> ~S -> ~S~%"
                          (bdb:dbc-count cu) key1 val1)
                  (unless (= (! val) val1)
                    (push (list :count (bdb:dbc-count cu) :key key :val val
                                :key1 key1 :val1 val1 :val! (! val))
                          (car errors))))))
        (setf (car errors) (nreverse (car errors)))))))
((:QUEUE) (:RECNO))

;; :BTREE/:SET-RECNO
(bdb:with-db (db *dbe* (format nil "test-~A-~A.db" :BTREE :RECNUM)
                 :open (:type :BTREE :create t) :options (:recnum t))
  (show-db db)
  (let ((*print-pretty* t))
    (show (loop :repeat 20 :for x = (random 30) :for x! = (! x)
            :collect (list x x! (bdb:db-put db x x!)))))
  (show-db db))
NIL

(bdb:with-db (db *dbe* (format nil "test-~A-~A.db" :BTREE :RECNUM)
                 :open (:rdonly t) :options (:recnum t))
  (show-db db)
  (loop :with key :and val
    :for n :from 1 :to (bdb:db-stat-num-keys (bdb:db-stat db))
    :do (setf (values key val) (bdb:db-get db n :action :SET-RECNO
                                           :type :INTEGER :key-type :INTEGER))
    (format t "~&=[~D]=> ~S -> ~S" n key val)
    :unless (= (! key) val) :collect (list n key val (! key))))
NIL

(bdb:with-db (db *dbe* (format nil "test-~A-~A.db" :BTREE :RECNUM)
                 :open (:rdonly t) :options (:recnum t))
  (show-db db)
  (bdb:with-dbc (cu db)
    (loop :with key :and val
      :for n :from 1 :to (bdb:db-stat-num-keys (bdb:db-stat db))
      :do (setf (values key val) (bdb:dbc-get cu n :INTEGER :SET-RECNO))
      (format t "~&=[~D/count=~D]=> ~S -> ~S" n (bdb:dbc-count cu) key val)
      :unless (= (! key) val) :collect (list n key val (! key)) :end
      :do (setq key (bdb:dbc-get cu :INTEGER :INTEGER :GET-RECNO))
      :unless (= key n) :collect (list n key) :end)))
NIL

;; transactions - will be automatically closed (committed) by DBE-CLOSE
(let ((txn (bdb:txn-begin *dbe*)) (*print-pretty* t))
  (show (list txn (bdb:txn-begin *dbe* :parent txn) *dbe*))
  nil)
nil

;; *locker* & *lock* come from a previous incarnation of *dbe*
(bdb:lock-put *dbe* (show *lock*)) NIL
(bdb:lock-id-free *dbe* *locker*) NIL

(show-dbe *dbe*) NIL
(close *dbe*)    T
(bdb:dbe-remove (show (bdb:dbe-create)) :home "bdb-home/") NIL

(finish-file "bdb-errors") NIL
(finish-file "bdb-msg") NIL
(rmrf "bdb-home/") T
(rmrf "bdb-data/") T
