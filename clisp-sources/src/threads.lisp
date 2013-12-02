;; multithreading for CLISP

(defpackage "THREADS"
  (:nicknames "MT" "MP")
  (:use "COMMON-LISP" "EXT")
  (:export "MAKE-PROCESS" "PROCESS-WAIT"
           "WITHOUT-INTERRUPTS" "PROCESS-YIELD" "PROCESS-KILL"
           "PROCESS-INTERRUPT" "PROCESS-RESTART" "PROCESSP" "PROCESS-NAME"
           "PROCESS-ACTIVE-P" "PROCESS-STATE" "CURRENT-PROCESS" "LIST-PROCESSES"
           "MAKE-LOCK" "PROCESS-LOCK" "PROCESS-UNLOCK" "WITH-LOCK"
           "Y-OR-N-P-TIMEOUT" "WITH-TIMEOUT"))

(in-package "MT")

(use-package '("MT") "EXT")
(re-export "MT" "EXT")

;; definitions

(defmacro with-timeout ((seconds &body timeout-forms) &body body)
  "Execute BODY; if execution takes more than SECONDS seconds,
terminate and evaluate TIMEOUT-FORMS."
  `(call-with-timeout ,seconds (lambda () ,@timeout-forms) (lambda () ,@body)))

(defun y-or-n-p-timeout (seconds default &rest args)
  "Y-OR-N-P with timeout."
  (declare (ignorable seconds default))
  (with-timeout (seconds (format t "[Timed out] ~:[NO~;YES~]~%" default)
                         default)
    (apply #'y-or-n-p args)))

;;; locks

(defstruct (lock (:constructor make-lock (name)))
  name owner)

(defun process-lock (lock &optional whostate timeout)
  (process-wait whostate timeout lock)
  (setf (lock-owner lock) (current-process)))

(defun process-unlock (lock)
  (let ((self (current-process)) (owner (lock-owner lock)))
    (when owner
      (unless (eq owner self)
        (error (TEXT "~S: ~S does not own ~S" 'process-unlock self lock)))
      (setf (lock-owner lock) nil))))

(defmacro with-lock ((lock) &body body)
  "Execute BODY with LOCK locked."
  (let ((lk (gensym "WL-")))
    `(let ((,lk ,lock))
      (unwind-protect (progn (process-lock ,lk) ,@body)
        (process-unlock ,lk)))))
