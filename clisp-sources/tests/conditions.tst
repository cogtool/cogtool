;;;; -*- Lisp -*-
;;;; Test suite for the Common Lisp condition system
;;;; Written by David Gadbois <gadbois@cs.utexas.edu> 30.11.1993

;;;
;;; Helpers
;;;

#+CLISP
(defun my-cpl (class)
  (clos::class-precedence-list (clos:find-class class))
)
#+ALLEGRO
(defun my-cpl (class)
  (clos:finalize-inheritance (find-class class))
  (clos:class-precedence-list (find-class class))
)
#+CMU
(defun my-cpl (class)
  (pcl:class-precedence-list (find-class class))
)
#+SBCL
(defun my-cpl (class)
  (sb-pcl:class-precedence-list (find-class class))
)
#+LISPWORKS
(defun my-cpl (class)
  (class-precedence-list (find-class class))
)
MY-CPL

(defun check-superclasses (class expected)
  (let ((expected (list* class 't #+(or CLISP ALLEGRO SBCL LISPWORKS) 'standard-object #+CMU 'instance 'condition expected))
        (super (mapcar #' #+(or CLISP ALLEGRO SBCL LISPWORKS) class-name #+CMU pcl:class-name (my-cpl class))))
    (list (set-difference super expected)
          (set-difference expected super))))
CHECK-SUPERCLASSES

;;;
;;; IGNORE-ERRORS
;;;
;;; If this does not work, none of the tests that check for getting an error
;;; will.

;;; IGNORE-ERRORS should work.
(multiple-value-bind (value condition)
    (ignore-errors (error "Foo"))
  (list value (type-of condition)))
(nil simple-error)

;;; IGNORE-ERRORS should not interfere with values in non-error situations.
(multiple-value-list
    (ignore-errors (values 23 42)))
(23 42)

;;;
;;; Predefined condition types.
;;;

(check-superclasses 'warning '())
(nil nil)
(check-superclasses 'style-warning '(warning))
(nil nil)
(check-superclasses 'serious-condition '())
(nil nil)
(check-superclasses 'error '(serious-condition))
(nil nil)
(check-superclasses 'cell-error '(error serious-condition))
(nil nil)
(check-superclasses 'parse-error '(error serious-condition))
(nil nil)
(check-superclasses 'storage-condition '(serious-condition))
(nil nil)
(check-superclasses 'simple-error '(simple-condition error serious-condition))
(nil nil)
(check-superclasses 'simple-condition '())
(nil nil)
(check-superclasses 'simple-warning '(simple-condition warning))
(nil nil)
(check-superclasses 'file-error '(error serious-condition))
(nil nil)
(check-superclasses 'control-error '(error serious-condition))
(nil nil)
(check-superclasses 'program-error '(error serious-condition))
(nil nil)
(check-superclasses 'undefined-function '(cell-error error serious-condition))
(nil nil)
(check-superclasses 'arithmetic-error '(error serious-condition))
(nil nil)
(check-superclasses 'division-by-zero '(arithmetic-error error serious-condition))
(nil nil)
(check-superclasses 'floating-point-invalid-operation '(arithmetic-error error serious-condition))
(nil nil)
(check-superclasses 'floating-point-inexact '(arithmetic-error error serious-condition))
(nil nil)
(check-superclasses 'floating-point-overflow '(arithmetic-error error serious-condition))
(nil nil)
(check-superclasses 'floating-point-underflow '(arithmetic-error error serious-condition))
(nil nil)
(check-superclasses 'unbound-slot '(cell-error error serious-condition))
(nil nil)
(check-superclasses 'package-error '(error serious-condition))
(nil nil)
(check-superclasses 'print-not-readable '(error serious-condition))
(nil nil)
(check-superclasses 'reader-error '(parse-error stream-error error serious-condition))
(nil nil)
(check-superclasses 'stream-error '(error serious-condition))
(nil nil)
(check-superclasses 'end-of-file '(stream-error error serious-condition))
(nil nil)
(check-superclasses 'unbound-variable '(cell-error error serious-condition))
(nil nil)
(check-superclasses 'type-error '(error serious-condition))
(nil nil)
(check-superclasses 'simple-type-error '(simple-condition type-error error serious-condition))
(nil nil)

;;;
;;; Defining conditions.
;;;
(define-condition test () ())
TEST

(check-superclasses 'test '())
(nil nil)

(define-condition test2 (test) ())
TEST2

(check-superclasses 'test2 '(test))
(nil nil)

(define-condition test3 (test2 simple-condition) ())
TEST3

(check-superclasses 'test3 '(test2 test simple-condition))
(nil nil)

;;;
;;; Making conditions
;;;
(progn (make-condition 'test) t)
T

(ignore-errors (progn (make-condition 'integer) t))
NIL

;;;
;;; :REPORT option to DEFINE-CONDITION
;;;
(define-condition test4 (test3)
  ()
  (:report (lambda (condition stream)
             (format stream "Yow! -- ~S" (type-of condition)))))
TEST4

(with-output-to-string (s) (princ (make-condition 'test4) s))
"Yow! -- TEST4"

(define-condition test5 (test4) ())
TEST5

(with-output-to-string (s) (princ (make-condition 'test5) s))
"Yow! -- TEST5"

(with-output-to-string (s)
  (princ (make-condition 'test3
           :format-control "And How! -- ~S"
           :format-arguments '(23)) s))
"And How! -- 23"

;;;
;;; Condition slots.
;;;
(define-condition test6 (test4)
  ((foo :initarg :foo :initform 23 :accessor test6-foo))
  (:report (lambda (condition stream)
             (format stream "~S -- ~S"
                     (type-of condition)
                     (test6-foo condition)))))
TEST6

(test6-foo (make-condition 'test6))
23

(test6-foo (make-condition 'test6 :foo 42))
42

(setf (test6-foo (make-condition 'test6 :foo 42)) 17)
17

(with-output-to-string (s) (princ (make-condition 'test6 :foo 42) s))
"TEST6 -- 42"

(let ((type '(OR PROGRAM-ERROR TYPE-ERROR)))
  (typep (make-condition type) type))
T

(let ((type '(AND SIMPLE-ERROR TYPE-ERROR)))
  (typep (make-condition type) type))
T

;;;
;;; HANDLER-BIND
;;;

;;; You do not have to bind handlers.
(ignore-errors
 (handler-bind
     ()
   (error "Foo")))
nil

;;; Handlers should not interfere with values in non-error situations.
(multiple-value-list
    (block foo
      (handler-bind
          ((error #'(lambda (c)
                      (princ-error c)
                      (return-from foo 23))))
        (values 42 17))))
(42 17)

;;; Handlers should work.
(multiple-value-list
    (block foo
      (handler-bind
          ((error #'(lambda (c)
                      (princ-error c)
                      (return-from foo (values 23 17)))))
        (error "Foo"))))
(23 17)

;;; Only the appropriate handlers should be called.
(ignore-errors
 (block foo
   (handler-bind
       ((type-error #'(lambda (c)
                        (princ-error c)
                        (return-from foo 23))))
     (error "Foo"))))
nil

;;; Handlers can be specified type expressions.
(block foo
  (handler-bind
      (((or type-error error)
        #'(lambda (c)
            (princ-error c)
            (return-from foo 23))))
    (error "Foo")))
23

;;; Handlers should be undone.
(ignore-errors
 (block foo
   (let ((first-time t))
     (handler-bind
         ((error
           #'(lambda (c)
               (princ-error c)
               (if first-time
                   (progn
                     (setq first-time nil)
                     (error "Bar"))
                   (return-from foo 23)))))
       (error "Foo")))))
nil

;;; Handlers should be undone.
(block foo
  (let ((first-time t))
    (handler-bind
        ((error
          #'(lambda (c)
              (princ-error c)
              (return-from foo 23))))
      (handler-bind
          ((error
            #'(lambda (c)
                (princ-error c)
                (if first-time
                    (progn
                      (setq first-time nil)
                      (error "Bar"))
                    (return-from foo 42)))))
        (error "Foo")))))
23

;;; Handlers in the same cluster should be accessible.
(ignore-errors
 (block foo
   (handler-bind
       ((error
         #'(lambda (c) (princ-error c) nil))
        (error
         #'(lambda (c) (princ-error c) (return-from foo 23))))
     (error "Foo"))))
23

;;; Multiple handlers should work.
(block foo
  (handler-bind
      ((type-error
        #'(lambda (c)
            (princ-error c)
            (return-from foo 42)))
       (error
        #'(lambda (c)
            (princ-error c)
            (return-from foo 23))))
    (error "Foo")))
23

;;; Handlers should be undone.
(block foo
  (handler-bind
      ((error #'(lambda (c)
                  (princ-error c)
                  (return-from foo 23))))
    (block bar
      (handler-bind
          ((error #'(lambda (c)
                      (princ-error c)
                      (return-from foo 42))))
        (return-from bar)))
    (error "Foo")))
23

;;;
;;; HANDLER-CASE
;;;

;;; HANDLER-CASE should handle errors.
(multiple-value-list
    (handler-case
        (error "Foo")
      (error (c) (when (typep c 'error) (values 23 42)))))
(23 42)

;;; Except those it doesn't handle.
(ignore-errors
 (handler-case
     (error "Foo")
   (type-error () 23)))
NIL

;;; You don't have to specify handlers.
(ignore-errors
 (handler-case
     (error "Foo")))
NIL

;;; HANDLER-CASE should not interfere with values in non-error situations.
(multiple-value-list
    (handler-case
        (values 42 17)
      (error () 23)))
(42 17)

;;; :NO-ERROR should return values.
(multiple-value-list
    (handler-case
        (values 23 42)
      (:no-error (a b)
        (values b a))))
(42 23)

;;; Except when there is an error.
(handler-case
    (error "Foo")
  (error () 23)
  (:no-error (&rest args) (declare (ignore args)) 42))
23

;;; It does not have to be the last clause.
(handler-case
    23
  (:no-error (v) (1+ v))
  (error () 42))
24

;;; Multiple handlers should be OK.
(handler-case
    (error "Foo")
  (type-error () 23)
  (error () 42))
42

;;; Handlers should get undone.
(ignore-errors
 (progn
   (block foo
     (handler-case
         (return-from foo 23)
       (error () 42)))
   (error "Foo")))
NIL

;;; Ditto.
(ignore-errors
 (block foo
   (let ((first-time t))
     (handler-case
         (error "Foo")
       (error ()
         (if first-time
             (progn
               (setf first-time nil)
               (error "Bar"))
             (return-from foo 23)))))))
NIL

;;; from GCL ansi-test by Paul F. Dietz
(macrolet ((%m (&rest args) (cons 'error args)))
  (handler-bind ((error #'(lambda (c2)
                            (invoke-restart (find-restart 'foo c2)))))
    (handler-bind ((error #'(lambda (c) (princ-error c) (error "blah"))))
      (restart-case (restart-case (%m "boo!")
                      (foo () 'bad))
        (foo () 'good)))))
good

(symbol-macrolet ((%s (error "boo!")))
  (handler-bind ((error #'(lambda (c2)
                            (invoke-restart (find-restart 'foo c2)))))
    (handler-bind ((error #'(lambda (c) (princ-error c) (error "blah"))))
      (restart-case (restart-case %s
                      (foo () 'bad))
        (foo () 'good)))))
good

(macrolet ((%m2 (&rest args) (cons 'error args)))
  (macrolet ((%m (&rest args &environment env)
               (macroexpand (cons '%m2 args) env)))
    (handler-bind ((error #'(lambda (c2)
                              (invoke-restart (find-restart 'foo c2)))))
      (handler-bind ((error #'(lambda (c)
                                (princ-error c) (error "blah"))))
        (restart-case (restart-case (%m "boo!")
                        (foo () 'bad))
          (foo () 'good))))))
good

(macrolet ((%m2 (&rest args) (cons 'error args)))
  (macrolet ((%m (&rest args &environment env)
               (macroexpand (cons '%m2 args) env)))
    (handler-bind ((error #'(lambda (c2)
                              (invoke-restart (find-restart 'foo c2)))))
      (handler-bind ((error #'(lambda (c)
                                (princ-error c) (error "blah"))))
        (restart-case (with-restarts ((foo () 'bad))
                        (%m "boo!"))
          (foo () 'good))))))
good

(multiple-value-list
 (with-simple-restart (foo "zzz")
   (invoke-restart 'foo)))
(nil t)

(multiple-value-list
 (flet ((%f nil (invoke-restart 'foo)))
   (with-simple-restart (foo "zzz") (%f))))
(nil t)

(multiple-value-list
 (with-simple-restart (nil "")
   (invoke-restart (first (compute-restarts)))))
(nil t)

(restart-case
    (invoke-restart 'foo)
  (foo () :test (lambda (c) (princ-error c) nil) 'bad)
  (foo () 'good))
good

(block nil
  (handler-bind ((type-error (lambda (c)
                               (return (list (type-error-expected-type c)
                                             (type-error-datum c))))))
    (coerce '(1 2 3) 'integer)))
(INTEGER (1 2 3))

(block nil
  (handler-bind ((type-error (lambda (c)
                               (return (list (type-error-expected-type c)
                                             (type-error-datum c))))))
    (coerce '(1 2 3) '(integer 1))))
((INTEGER 1) (1 2 3))

;; Check that after a Ctrl-D (EOF), assert without places is not retried.
#+CLISP
(let ((done nil))
  (block test
    (sys::driver
      #'(lambda ()
          (when done (return-from test nil))
          (setq done t)
          (handler-bind ; override handler set by EXIT-ON-ERROR
              ((error (lambda (c) (throw 'sys::done-signaling nil))))
            (let ((*debug-io*
                    (make-two-way-stream (make-string-input-stream "")
                                         *terminal-io*)))
              (assert (= 1 2))))))))
#+CLISP
nil

(block nil
  (handler-bind ((unbound-variable (lambda (c) (princ-error c) (return :good))))
    (let ((foo (gensym "UNBOUND-")))
      (declare (compile) (optimize safety (debug 1)))
      (progn (symbol-value foo) :bad))))
:GOOD

(block nil
  (declaim (optimize safety (debug 1)))
  (unwind-protect
       (handler-bind ((unbound-variable
                       (lambda (c) (princ-error c) (return :good))))
         (let ((foo (gensym "UNBOUND-")))
           (declare (compile))
           (progn (symbol-value foo) :bad)))
    (declaim (optimize (safety 1)))))
:GOOD

(block nil
  (handler-bind ((unbound-variable (lambda (c) (princ-error c) (return :good))))
    (let ((foo (gensym "UNBOUND-")))
      (progn (symbol-value foo) :bad))))
:GOOD

(block nil
  (handler-bind ((reader-error (lambda (c)
                                 (princ-error c)
                                 (return (streamp (stream-error-stream c))))))
    (read-from-string ")")))
T

(block nil
  (handler-bind ((reader-error (lambda (c)
                                 (princ-error c)
                                 (return (streamp (stream-error-stream c))))))
    (read-from-string ",1")))
T
