;; -*- Lisp -*-
;; some tests for SYSCALLS
;; clisp -E 1:1 -q -norc -i ../tests/tests -x '(run-test "syscalls/test")'

(listp (show (multiple-value-list (ext:module-info "syscalls" t)) :pretty t)) T

(os:hostent-p (show (os:resolve-host-ipaddr "localhost")))
T

(listp (show (os:resolve-host-ipaddr) :pretty t)) T
(os:service-p (show (os:service "smtp"))) T
(os:service-p (show (os:service 25))) T
(listp (show (os:service) :pretty t)) T

#+unix
(let* ((fmt "%Y-%m-%d %T") (string (show (os:string-time fmt))))
  (string= string (os:string-time fmt (show (os:string-time fmt string)))))
#+unix T

#+unix
(when (fboundp 'os:getutxent)
  (not (integerp (show (length (loop :for utmpx = (os:getutxent) :while utmpx
                                 :collect (show utmpx :pretty t)))))))
#+unix NIL

(defparameter *tmp1* (os:mkstemp "syscalls-tests-")) *tmp1*
(defparameter *tmp2* (os:mkstemp "syscalls-tests-")) *tmp2*

(let ((*standard-output* (make-broadcast-stream
                          *standard-output* *tmp1* *tmp2*)))
  (show (write *tmp1* :stream *tmp1*)) (terpri *tmp1*)
  (show (write *tmp2* :stream *tmp2*)) (terpri *tmp2*)
  T)
T

#+unix (find :rdwr (show (os:stream-options *tmp1* :fl))) #+unix :RDWR
#+unix (ext:appease-cerrors
        (with-open-file (s *tmp1*)
          (find :rdonly (show (os:stream-options s :fl)))))
#+unix :RDONLY
#+unix (os:stream-options *tmp1* :fd) NIL
#+unix (os:stream-options *tmp1* :fd '(:cloexec)) NIL
#+unix (os:stream-options *tmp1* :fd) #+unix (:cloexec)
#+unix (os:stream-options *tmp1* :fd nil) NIL
#+unix (os:stream-options *tmp1* :fd) NIL

;; this may fail with ENOLCK - in which case we do not test locking
(handler-case (os:stream-lock *tmp1* t)
  (error (err)
    (format t "~S: ~A" 'os:stream-lock err)
    (pushnew :no-stream-lock *features*)
    T))
T
(os:stream-lock *tmp1* nil) NIL

(typep (show (os:priority (os:process-id))) '(or keyword (integer -20 20))) T

#+unix (let ((id (show (getuid)))) (= id (setf (getuid) id))) T
#+unix (let ((id (show (getgid)))) (= id (setf (getgid) id))) T
#+unix (let ((id (show (geteuid)))) (= id (setf (geteuid) id))) T
#+unix (let ((id (show (getegid)))) (= id (setf (getegid) id))) T

#+unix
(listp (show (if (fboundp 'os:sysconf) (os:sysconf) '(no os:sysconf)))) T
#+unix
(listp (show (if (fboundp 'os:confstr) (os:confstr) '(no os:confstr)))) T

#+unix
(listp (show (if (fboundp 'os:usage)
                 (multiple-value-list (os:usage)) '(no os:usage))))
T
#+unix
(listp (show (if (fboundp 'os:rlimit)
                 (multiple-value-list (os:rlimit)) '(no os:rlimit))))
T

#+unix (os:uname-p (show (os:uname))) #+unix T

#+unix (os:user-info-p (show (os:user-info :default))) T
#+unix (listp (show (os:user-info) :pretty t)) T
#+unix (os:group-info-p (show (os:group-info (os:user-info-gid
                                              (os:user-info :default))))) T
#+unix (listp (show (os:group-info) :pretty t)) T

(os:file-stat-p (show (os:file-stat *tmp1*))) T
(os:file-stat-p (show (os:file-stat (pathname *tmp1*)))) T

(os:convert-mode #o0666)
#+unix (:RUSR :WUSR :RGRP :WGRP :ROTH :WOTH)
#+win32 (:RUSR :WUSR 54)
#-(or unix win32) ERROR

(os:convert-mode '(:RWXU #+unix :RWXG #+unix :RWXO))
#+unix #o0777
#+win32 #o0700
#-(or unix win32) ERROR

(and (fboundp 'os:stat-vfs)
     (not (os:stat-vfs-p (show (os:stat-vfs *tmp2*)))))
NIL

(string= (show #+win32 (ext:string-concat (ext:getenv "USERDOMAIN") "\\"
                                          (ext:getenv "USERNAME"))
               #+unix (ext:getenv "USER")
               #-(or unix win32) ERROR)
         (show (os:file-owner *tmp1*)))
T

(progn (close *tmp1*) (close *tmp2*) T) T

(listp (show (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (show (os:copy-file *tmp2* *tmp1* :if-exists :append))) T
(listp (show (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (show (os:copy-file *tmp2* *tmp1* :if-exists :append))) T
(listp (show (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (show (os:copy-file *tmp2* *tmp1* :if-exists :append))) T
(listp (show (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (show (os:copy-file *tmp2* *tmp1* :if-exists :append))) T
(listp (show (os:copy-file *tmp1* *tmp2* :if-exists :append))) T
(listp (show (os:copy-file *tmp2* *tmp1* :if-exists :append))) T

(integerp (show (with-open-file (s *tmp1* :direction :input) (file-length s))))
T

(integerp (show (with-open-file (s *tmp2* :direction :input) (file-length s))))
T

;; win32 functions barf on cygwin pathnames
#+win32 (os:file-info-p (show (os:file-info *tmp2*) :pretty t)) T
#+win32 (listp (show (os:file-info (make-pathname :name "syscalls-tests-*"
                                                  :defaults *tmp2*)
                                   t)
                     :pretty t))
T

#+(or win32 cygwin)
(os:system-info-p (show (os:system-info)))
#+(or win32 cygwin) T
#+(or win32 cygwin)
(os:version-p (show (os:version)))
T
#+(or win32 cygwin)
(os:memory-status-p (show (os:memory-status)))
T

(let ((sysconf #+unix (show (os:sysconf)) #-unix nil))
  ;; guard against broken unixes, like FreeBSD 4.10-BETA
  (if #+unix (and (getf sysconf :PAGESIZE)
                  (getf sysconf :PHYS-PAGES)
                  (getf sysconf :AVPHYS-PAGES))
      #-unix T
      (listp (show (multiple-value-list (os:physical-memory))))
      T))
T

;; test file locking
(let ((buf (make-array 100 :fill-pointer t :adjustable t
                       :element-type 'character)))
  (defun flush-clisp (stream)
    (when #-:win32 (socket:socket-status (cons stream :input) 1)
          ;; select on win32 does not work with pipes
          #+:win32 (progn (sleep 1) (listen stream))
      ;; read from the clisp stream until the next prompt
      (setf (fill-pointer buf) 0)
      (loop :with pos-NL = 0 :for ch = (read-char stream)
        :until (and (char= ch #\Space) (char= #\[ (char buf pos-NL))
                    (let ((pos1 (position #\] buf :start pos-NL)))
                      (and pos1 (char= #\> (char buf (1+ pos1))))))
        :do (when (char= ch #\Newline) (setq pos-NL (1+ (length buf))))
        (vector-push-extend ch buf))
      (show buf))))
FLUSH-CLISP

(defun proc-send (proc fmt &rest args)
  (apply #'format proc fmt args)
  (terpri proc) (force-output proc)
  (flush-clisp proc))
PROC-SEND

(let* ((argv (ext:argv)) (run (aref argv 0))
       (args (list "-M" (aref argv (1+ (position "-M" argv :test #'string=)))
                   "-B" (aref argv (1+ (position "-B" argv :test #'string=)))
                   "-norc" "-q" "-on-error" "abort")))
  (show (cons run args))
  (defparameter *proc1* (ext:run-program run :arguments args
                                         :input :stream :output :stream))
  (defparameter *proc2* (ext:run-program run :arguments args
                                         :input :stream :output :stream))
  (flush-clisp *proc1*)
  (flush-clisp *proc2*)
  t)
T

(stringp
 (proc-send *proc1* "(setq s (open ~S :direction :output :if-exists :append))"
            (truename *tmp1*)))
T
(stringp
 (proc-send *proc2* "(setq s (open ~S :direction :output :if-exists :append))"
            (truename *tmp1*)))
T

#-:no-stream-lock (read-from-string (proc-send *proc1* "(stream-lock s t)"))
#-:no-stream-lock T
#-:no-stream-lock (proc-send *proc2* "(stream-lock s t)")
#-:no-stream-lock NIL           ; blocked

#-:no-stream-lock (read-from-string (proc-send *proc1* "(stream-lock s nil)"))
#-:no-stream-lock NIL           ; released
#-:no-stream-lock (read-from-string (flush-clisp *proc2*))
#-:no-stream-lock T             ; acquired

#-:no-stream-lock (read-from-string (proc-send *proc1* "(stream-lock s t :block nil)"))
#-:no-stream-lock NIL
#-:no-stream-lock (read-from-string (proc-send *proc2* "(stream-lock s nil)"))
#-:no-stream-lock NIL           ; released
#-:no-stream-lock (read-from-string (proc-send *proc1* "(stream-lock s t :block nil)"))
#-:no-stream-lock T
#-:no-stream-lock (read-from-string (proc-send *proc1* "(stream-lock s nil)"))
#-:no-stream-lock NIL           ; released

;; check :rename-and-delete
;; woe32 signals ERROR_SHARING_VIOLATION
;; when renaming a file opened by a different process
#-win32
(let ((inode (show (posix:file-stat-ino (posix:file-stat *tmp1*)))))
  (with-open-stream (s (ext:run-program
                        "tail" :arguments (list "-f" (namestring *tmp1*)
                                                (format nil "--pid=~D"
                                                        (os:process-id)))
                        :output :stream))
    (with-open-file (new *tmp1* :direction :output
                         :if-exists :rename-and-delete)
      (= inode (show (posix:file-stat-ino (posix:file-stat new)))))))
#-win32 NIL

(let ((file "foo.bar") (dates '(3141592653 3279321753)))
  (unwind-protect
       (progn (with-open-file (s file :direction :output) (write s :stream s))
              (loop :for d :in dates :do (posix:set-file-stat file :mtime d)
                :collect (= d (with-open-file (s file) (file-write-date s)))))
    (delete-file file)))
(T T)

(progn (proc-send *proc1* "(close s)~%(ext:quit)")
       (close (two-way-stream-input-stream *proc1*))
       (close (two-way-stream-output-stream *proc1*))
       (close *proc1*) (makunbound '*proc1*) (unintern '*proc1*)
       (proc-send *proc2* "(close s)~%(ext:quit)" )
       (close (two-way-stream-input-stream *proc2*))
       (close (two-way-stream-output-stream *proc2*))
       (close *proc2*) (makunbound '*proc2*) (unintern '*proc2*)
       (delete-file *tmp1*) (makunbound '*tmp1*) (unintern '*tmp1*)
       (delete-file *tmp2*) (makunbound '*tmp2*) (unintern '*tmp2*)
       (fmakunbound 'flush-clisp) (unintern 'flush-clisp)
       (fmakunbound 'proc-send) (unintern 'proc-send)
       (setq *features* (delete :no-stream-lock *features*))
       T)
T
