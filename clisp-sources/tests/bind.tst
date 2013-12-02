;;; -*- Lisp -*-
;; test non-global special bindings

;; variable being declared special is bound - a "bound" declaration
(let ((x 5)) (let ((x (1+ x))) (declare (special x)) x))
6
(let ((x 5)) (let* ((x (1+ x))) (declare (special x)) x))
6
(let ((x 5)) (multiple-value-bind (x) (1+ x) (declare (special x)) x))
6
(let ((x 5)) ((lambda (x) (declare (special x)) x) (1+ x)))
6

;; similarly, without enclosing lexical binding
(block foo
  (handler-bind ((unbound-variable
                  (lambda (c) (princ-error c) (return-from foo 'good))))
    (let ((x (1+ x))) (declare (special x)) x)))
GOOD
(block foo
  (handler-bind ((unbound-variable
                  (lambda (c) (princ-error c) (return-from foo 'good))))
    (let* ((x (1+ x))) (declare (special x)) x)))
GOOD
(block foo
  (handler-bind ((unbound-variable
                  (lambda (c) (princ-error c) (return-from foo 'good))))
    (multiple-value-bind (x) (1+ x) (declare (special x)) x)))
GOOD
(block foo
  (handler-bind ((unbound-variable
                  (lambda (c) (princ-error c) (return-from foo 'good))))
    ((lambda (x) (declare (special x)) x) (1+ x))))
GOOD

;; variable being declared special is not being bound - a "free" declaration
(let ((x 5)) (let ((y (1+ x))) (declare (special x)) y))
6
(let ((x 5)) (let* ((y (1+ x))) (declare (special x)) y))
6
(let ((x 5)) (multiple-value-bind (y) (1+ x) (declare (special x)) y))
6
(let ((x 5)) ((lambda (y) (declare (special x)) y) (1+ x)))
6

;; variable is not being declared special
(let ((x 5)) (let ((x (1+ x))) x))
6
(let ((x 5)) (let* ((x (1+ x))) x))
6
(let ((x 5)) (multiple-value-bind (x) (1+ x) x))
6
(let ((x 5)) ((lambda (x) x) (1+ x)))
6

;; CLHS 3.3.4 - a "bound" declaration
(LET ((X 5))
  (PROGV '(X) '(20)
    (LET ((X (1+ X)) (Z (1+ X)))
      (DECLARE (SPECIAL X))
      Z)))
6
(LET ((X 5))
  (PROGV '(X) '(20)
    (LET* ((X (1+ X)) (Z (1+ X)))
      (DECLARE (SPECIAL X))
      Z)))
7
(LET ((X 5))
  (PROGV '(X) '(20)
    (MULTIPLE-VALUE-BIND (X Z) (VALUES (1+ X) (1+ X))
      (DECLARE (SPECIAL X))
      Z)))
6
(LET ((X 5))
  (PROGV '(X) '(20)
    ((LAMBDA (&OPTIONAL (X (1+ X)) (Z (1+ X)))
       (DECLARE (SPECIAL X))
       Z))))
7

;; same as above, with LOCALLY
;; Nice test: it triggers an ACL 6.2 compiler bug :-)
(let ((x 5))
  (progv '(x y) '(20 120)
    (let ((x (1+ x)) (y (1+ x)) (z (1+ x)))
      (declare (special x))
      (list z (locally (declare (special y)) y) x y))))
(6 120 6 6)
(let ((x 5))
  (progv '(x y) '(20 120)
    (let* ((x (1+ x)) (y (1+ x)) (z (1+ x)))
      (declare (special x))
      (list z (locally (declare (special y)) y) x y))))
(7 120 6 7)
(let ((x 5))
  (progv '(x y) '(20 120)
    (multiple-value-bind (x y z) (values (1+ x) (1+ x) (1+ x))
      (declare (special x))
      (list z (locally (declare (special y)) y) x y))))
(6 120 6 6)
(let ((x 5))
  (progv '(x y) '(20 120)
    ((lambda (&optional (x (1+ x)) (y (1+ x)) (z (1+ x)))
       (declare (special x))
       (list z (locally (declare (special y)) y) x y)))))
(7 120 6 7)

;; CLHS 3.3.4 - a "free" declaration
(LET ((X 5))
  (PROGV '(X) '(20)
    (LET ((Y (1+ X)) (Z (1+ X)))
      (DECLARE (SPECIAL X))
      Z)))
6
(LET ((X 5))
  (PROGV '(X) '(20)
    (LET* ((Y (1+ X)) (Z (1+ X)))
      (DECLARE (SPECIAL X))
      Z)))
6
(LET ((X 5))
  (PROGV '(X) '(20)
    (MULTIPLE-VALUE-BIND (Y Z) (VALUES (1+ X) (1+ X))
      (DECLARE (SPECIAL X))
      Z)))
6
(LET ((X 5))
  (PROGV '(X) '(20)
    ((LAMBDA (&OPTIONAL (Y (1+ X)) (Z (1+ X)))
       (DECLARE (SPECIAL X))
       Z))))
6

;; global special variable with extra redundant special declaration
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      (let ((*global-var-for-bind.tst* (1+ *global-var-for-bind.tst*)))
        (declare (special *global-var-for-bind.tst*))
        *global-var-for-bind.tst*)
      *global-var-for-bind.tst*)))
(6 5)
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      (let* ((*global-var-for-bind.tst* (1+ *global-var-for-bind.tst*)))
        (declare (special *global-var-for-bind.tst*))
        *global-var-for-bind.tst*)
      *global-var-for-bind.tst*)))
(6 5)
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      (multiple-value-bind (*global-var-for-bind.tst*)
          (1+ *global-var-for-bind.tst*)
        (declare (special *global-var-for-bind.tst*))
        *global-var-for-bind.tst*)
      *global-var-for-bind.tst*)))
(6 5)
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      ((lambda (*global-var-for-bind.tst*)
         (declare (special *global-var-for-bind.tst*))
         *global-var-for-bind.tst*)
       (1+ *global-var-for-bind.tst*))
      *global-var-for-bind.tst*)))
(6 5)

;; global special variable without special declaration
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      (let ((*global-var-for-bind.tst* (1+ *global-var-for-bind.tst*)))
        *global-var-for-bind.tst*)
      *global-var-for-bind.tst*)))
(6 5)
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      (let* ((*global-var-for-bind.tst* (1+ *global-var-for-bind.tst*)))
        *global-var-for-bind.tst*)
      *global-var-for-bind.tst*)))
(6 5)
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      (multiple-value-bind (*global-var-for-bind.tst*)
          (1+ *global-var-for-bind.tst*)
        *global-var-for-bind.tst*)
      *global-var-for-bind.tst*)))
(6 5)
(progn
  (defparameter *global-var-for-bind.tst* 123)
  (let ((*global-var-for-bind.tst* 5))
    (list
      ((lambda (*global-var-for-bind.tst*)
         *global-var-for-bind.tst*)
       (1+ *global-var-for-bind.tst*))
      *global-var-for-bind.tst*)))
(6 5)


;;; MACROLET and environments.

(eval-when (eval compile load)
  (defmacro chk-type (form type)
    (let ((obj (gensym)))
      `(LET ((,obj ,form)) (ASSERT (TYPEP ,obj ',type)) (THE ,type ,obj)))))
CHK-TYPE

;; Local macros may reference locally defined symbol macros.
(symbol-macrolet ((x (list 'symbol)))
  (macrolet ((foo (form) `(CHK-TYPE ,form ,@x)))
    (foo 'bar)))
BAR
(let ((x 5))
  (progv '(x) '(20)
    (symbol-macrolet ((x (list 'symbol)))
      (macrolet ((foo (form) `(CHK-TYPE ,form ,@x)))
        (foo 'bar)))))
BAR

(let ((x :good))
  (declare (special x))
  (let ((x :bad))
    (symbol-macrolet () (declare (special x)) x)))
:GOOD

;; Local macros may reference SPECIAL declarations.
(locally
  (declare (special symbol-type))
  (setq symbol-type (list 'symbol)))
(SYMBOL)
(locally
  (declare (special symbol-type))
  (macrolet ((foo (form) `(CHK-TYPE ,form ,@symbol-type)))
    (foo 'bar)))
BAR

;; Local macros may reference global variables.
;; Careful! In interpreted mode, the value from the dynamic binding is used;
;; in compiled mode, the original global value is used. (To change this,
;; one would have to use COMPILER-LET.)
(defparameter *symbol-type* (list 'symbol))
*SYMBOL-TYPE*
(let ((*symbol-type* (list 'symbol)))
  (macrolet ((foo (form) `(CHK-TYPE ,form ,@*symbol-type*)))
    (foo 'bar)))
BAR

;; Local macros must not reference lexical variable bindings.
(progv '(x) '((symbol))
  (let ((x (list 'symbol)))
    (macrolet ((foo (form) `(CHK-TYPE ,form ,@x)))
      (foo 'bar))))
ERROR
(progv '(x) '((symbol))
  (let ((x (list 'symbol)))
    (defun testfn ()
      (macrolet ((foo (form) `(CHK-TYPE ,form ,@x)))
        (foo 'bar))))
  (testfn))
ERROR
(progv '(x) '((symbol))
  (let ((x (list 'symbol)))
    (macrolet ((foo (form) `(CHK-TYPE ,form ,@x)))
      (defun testfn ()
        (foo 'bar))))
  (testfn))
ERROR

;; Local macros may reference locally defined macros.
(macrolet ((x () '(list 'symbol)))
  (macrolet ((foo (form) `(CHK-TYPE ,form ,@(x))))
    (foo 'bar)))
BAR

;; Local macros must not reference lexical function bindings.
(defun symbol-type-fn () (list 'symbol))
SYMBOL-TYPE-FN
(flet ((symbol-type-fn () (list 'symbol)))
  (macrolet ((foo (form) `(CHK-TYPE ,form ,@(symbol-type-fn))))
    (foo 'bar)))
ERROR
(progn
  (flet ((symbol-type-fn () (list 'symbol)))
    (defun testfn ()
      (macrolet ((foo (form) `(CHK-TYPE ,form ,@(symbol-type-fn))))
        (foo 'bar))))
  (testfn))
ERROR
(progn
  (flet ((symbol-type-fn () (list 'symbol)))
    (macrolet ((foo (form) `(CHK-TYPE ,form ,@(symbol-type-fn))))
      (defun testfn ()
        (foo 'bar))))
  (testfn))
ERROR
(fmakunbound 'symbol-type-fn)
SYMBOL-TYPE-FN

;; The scope of a "free" declaration in DO* contains the step-forms.
(block done
  (let ((x :good))
    (declare (special x))
    (let ((x :bad))
      (do* ((i 0 (return-from done x)))
           (nil nil)
        (declare (special x))))))
:GOOD

;; The scope of a "free" declaration in MULTIPLE-VALUE-BIND contains the body.
(let ((x :good))
  (declare (special x))
  (let ((x :bad))
    (multiple-value-bind (a b) (floor 13 4)
      (declare (special x))
      (list a b x))))
(3 1 :GOOD)

;; The scope of a "free" declaration in DEFUN contains only the body.
(let ((x 1))
  (declare (special x))
  (let ((x 2))
    (defun bind-test-function-1 (&optional (y x))
      (declare (special x))
      (list y x))
    (bind-test-function-1)))
(2 1)

(let ((x 5)) (let ((x (1+ x)) (z (1+ x))) (declare (special x)) z))
6

(let ((x 5)) (let* ((x (1+ x)) (z (1+ x))) (declare (special x)) z))
7

(let ((x 5)) (multiple-value-bind (x z) (values (1+ x) (1+ x)) (declare (special x)) z))
6

(let ((x 5)) ((lambda (x z) (declare (special x)) z) (1+ x) (1+ x)))
6

(let ((x 5)) ((lambda (&optional x z) (declare (special x)) z) (1+ x) (1+ x)))
6

;; macrolet.43
(let ((-x- nil))
  (declare (special -x-))
  (let ((-f- #'(lambda () -x-)))
    (declare (special -f-))
    (eval `(macrolet ((%m (-x-)
                        (declare (special -f-))
                        (funcall -f-)))
             (%m t)))))
nil
(let ((-x- nil))
  (declare (special -x-))
  (let ((-f- #'(lambda () -x-)))
    (declare (special -f-))
    (macrolet ((%m (-x-) '(funcall -f-)))
      (declare (special -f-))
      (%m t))))
nil

;; macrolet.44
(let ((-x- nil))
  (declare (special -x-))
  (let ((-f- #'(lambda () -x-)))
    (declare (special -f-))
    (eval `(macrolet ((%m (-x-)
                        (declare (special -f- -x-))
                        (funcall -f-)))
             (%m t)))))
t
(let ((-x- nil))
  (declare (special -x-))
  (let ((-f- #'(lambda () -x-)))
    (declare (special -f-))
    (macrolet ((%m (-x-) '(funcall -f-)))
      (declare (special -f- -x-))
      (%m t))))
nil

;; macrolet.45
(let ((-x- nil))
  (declare (special -x-))
  (let ((-f- #'(lambda () -x-)))
    (declare (special -f-))
    (eval `(macrolet ((%m ((-x-))
                        (declare (special -f- -x-))
                        (funcall -f-)))
             (%m (t))))))
t
(let ((-x- nil))
  (declare (special -x-))
  (let ((-f- #'(lambda () -x-)))
    (declare (special -f-))
    (macrolet ((%m ((-x-)) '(funcall -f-)))
      (declare (special -f- -x-))
      (%m (t)))))
nil
