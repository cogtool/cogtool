;; -*- Lisp -*-

#-(or GCL CMU SBCL OpenMCL LISPWORKS)
(use-package "CLOS")
#-(or GCL CMU SBCL OpenMCL LISPWORKS)
T

(unintern '<C1>)
T

(progn
(defclass <C1> ()
  ((x :initform 0 :accessor x-val :reader get-x :writer set-x :initarg :x)
   (y :initform 1 :accessor y-val :reader get-y :writer set-y :initarg :y)))
())
NIL

(progn
(defclass <C2> (<C1>)
  ((z :initform 0 :accessor z-val :reader get-z :writer set-z :initarg :z)))
())
NIL

(defparameter a (make-instance (find-class '<C1>) :x 10))
A

(let (cache)
  (defmethod slot-missing ((class t) (obj <C1>)
                           (slot-name t) (operation t)
                           &optional (new-value nil new-value-p))
    (setf cache
          (list slot-name operation new-value new-value-p)))
  (list (slot-boundp a 'abcd) cache
        (slot-value a 'abcd) cache))
(#+(or ALLEGRO CMU18 OpenMCL LISPWORKS) (ABCD SLOT-BOUNDP NIL NIL) #-(or ALLEGRO CMU18 OpenMCL LISPWORKS) T
 (ABCD SLOT-BOUNDP NIL NIL) (ABCD SLOT-VALUE NIL NIL) (ABCD SLOT-VALUE NIL NIL))

(x-val a)
10

(y-val a)
1

(setf (x-val a) 20)
20

(x-val a)
20

(get-x a)
20

(set-x 10 a)
10

(x-val a)
10

(with-slots (x y) a (+ x y))
11

(defun foo (z) (with-slots (x y) z (+ x y)))
foo

(foo a)
11

(compile 'foo)
foo

(foo a)
11

(fmakunbound 'foo)
foo

(x-val (reinitialize-instance a :x 20))
20

(x-val (reinitialize-instance a :x 30))
30

(x-val (reinitialize-instance a :x 50))
50

(x-val (reinitialize-instance a :x 80))
80

(x-val (reinitialize-instance a :y 20))
80

(y-val (reinitialize-instance a :x 30))
20

(x-val (reinitialize-instance a :y 50))
30

(y-val (reinitialize-instance a :x 80))
50

(defparameter b (make-instance (find-class '<C2>) :x 10 :y 20 :z 30))
B

(x-val b)
10

(y-val b)
20

(z-val b)
30

(let* ((fn (defgeneric f (x y)
             (:method ((x t) (y t))
               (list x y))))
       (meth1 (defmethod f ((i integer) (j number))
                (+ i j)))
       (meth2 (defmethod f ((s1 string) (s2 string))
                (concatenate 'string s1 s2))))
  (lambda () (defmethod f ((x list) (y list)) (append x y)))
  (list (eq meth1 (find-method #'f nil (list (find-class 'integer)
                                             (find-class 'number))))
        (eq meth2 (find-method #'f nil (list (find-class 'string)
                                             (find-class 'string))))))
(T T)

(f t t)
(T T)

(f 2 3)
5

(f 2 3.0)
5.0

(f 2.0 3)
(2.0 3)

(f "ab" "cd")
"abcd"

(f 1 "abc")
(1 "abc")

(progn
(defgeneric f (x y)
  (:method ((x t) (y t))
    (list x y))
  (:method ((i number) (j integer))
    (list (call-next-method) (- i j)))
  (:method ((i integer) (j number))
    (list (call-next-method) (+ i j))))
())
NIL

(f 'x 'y)
(X Y)

(f 1 2)
(((1 2) -1) 3)

(f 1 2.0)
((1 2.0) 3.0)

(f 1.0 2)
((1.0 2) -1.0)

(progn
(defgeneric g (x)
  (:method ((x null))
    (cons 'null (call-next-method)))
  (:method ((x list))
    (if (next-method-p) (cons 'list (call-next-method)) '(list$)))
  (:method ((x symbol))
    (if (next-method-p) (cons 'symbol (call-next-method)) '(symbol$))))
())
NIL

(g 'x)
(SYMBOL$)

(g '(x))
(LIST$)

(g '())
(NULL SYMBOL LIST$)

(defparameter *hl* nil)
*HL*

(progn
(defgeneric hgen (x)
  (:method ((x integer))
    (setf *hl* (cons 'i-primary-1 *hl*))
    (call-next-method)
    (setf *hl* (cons 'i-primary-2 *hl*)))
  (:method :before ((x integer))
    (setf *hl* (cons 'i-before *hl*)))
  (:method :after ((x integer))
    (setf *hl* (cons 'i-after *hl*)))
  (:method :around ((x integer))
    (setf *hl* (cons 'i-around-1 *hl*))
    (call-next-method)
    (setf *hl* (cons 'i-around-2 *hl*)))
  (:method ((x number))
    (setf *hl* (cons 'n-primary-1 *hl*))
    (call-next-method)
    (setf *hl* (cons 'n-primary-2 *hl*)))
  (:method :before ((x number))
    (setf *hl* (cons 'n-before *hl*)))
  (:method :after ((x number))
    (setf *hl* (cons 'n-after *hl*)))
  (:method :around ((x number))
    (setf *hl* (cons 'n-around-1 *hl*))
    (call-next-method)
    (setf *hl* (cons 'n-around-2 *hl*)))
  (:method ((x t))
    (setf *hl* (cons 'innermost *hl*))))
(defun h (x)
  (setf *hl* '()) (hgen x) (reverse *hl*))
)
H

(h 'abc)
(INNERMOST)

(h 3.14)
(N-AROUND-1 N-BEFORE N-PRIMARY-1 INNERMOST N-PRIMARY-2 N-AFTER N-AROUND-2)

(h 3)
(I-AROUND-1 N-AROUND-1 I-BEFORE N-BEFORE I-PRIMARY-1 N-PRIMARY-1 INNERMOST
  N-PRIMARY-2 I-PRIMARY-2 N-AFTER I-AFTER N-AROUND-2 I-AROUND-2
)

;; Keyword checking is enabled even when no method has &key.
(progn
  (defgeneric testgf00 (&rest args &key)
    (:method (&rest args)))
  (testgf00 'a 'b))
ERROR

;; Check that call-next-method functions have indefinite extent and can
;; be called in arbitrary order.
(let ((methods nil))
  (defgeneric foo136 (mode object))
  (defmethod foo136 (mode (object t))
    (if (eq mode 'store)
      (push #'call-next-method methods)
      (if (eq mode 'list)
        (list 't)
        (cons (list 't) (funcall mode)))))
  (defmethod foo136 (mode (object number))
    (if (eq mode 'store)
      (progn (push #'call-next-method methods) (call-next-method))
      (if (eq mode 'list)
        (cons 'number (call-next-method))
        (cons (cons 'number (call-next-method 'list object)) (funcall mode)))))
  (defmethod foo136 (mode (object real))
    (if (eq mode 'store)
      (progn (push #'call-next-method methods) (call-next-method))
      (if (eq mode 'list)
        (cons 'real (call-next-method))
        (cons (cons 'real (call-next-method 'list object)) (funcall mode)))))
  (defmethod foo136 (mode (object rational))
    (if (eq mode 'store)
      (progn (push #'call-next-method methods) (call-next-method))
      (if (eq mode 'list)
        (cons 'rational (call-next-method))
        (cons (cons 'rational (call-next-method 'list object)) (funcall mode)))))
  (defmethod foo136 (mode (object integer))
    (if (eq mode 'store)
      (progn (push #'call-next-method methods) (call-next-method))
      (if (eq mode 'list)
        (cons 'integer (call-next-method))
        (cons (cons 'integer (call-next-method 'list object)) (funcall mode)))))
  (foo136 'store 3)
  (multiple-value-bind (t-error-method
                        number-t-method
                        real-number-method
                        rational-real-method
                        integer-rational-method)
      (values-list methods)
    (foo136 #'(lambda ()
                (funcall number-t-method
                  #'(lambda ()
                      (funcall integer-rational-method
                        #'(lambda ()
                            (funcall real-number-method
                              #'(lambda () nil)
                              5))
                        5))
                  5))
            5)))
((INTEGER RATIONAL REAL NUMBER T)
 (T)
 (RATIONAL REAL NUMBER T)
 (NUMBER T))

(unintern '<C1>)
T

(progn
(defclass <C1> ()
  ((x :initform 0 :accessor x-val :initarg :x)
   (y :initform 1 :accessor y-val :initarg :y)))
())
NIL

(defparameter a (make-instance (find-class '<C1>) :x 10))
A

(defparameter b (make-instance (find-class '<C1>) :y 20 :x 10))
B

(defparameter c (make-instance (find-class '<C1>)))
C

(x-val a)
10

(y-val a)
1

(x-val b)
10

(y-val b)
20

(x-val c)
0

(y-val c)
1

(unintern '<C1>)
T

(let* ((c (defclass <C1> ()
            ((x :initform 0 :accessor x-val :initarg :x)
             (y :initform 1 :accessor y-val :initarg :y))))
       (m (defmethod initialize-instance :after ((instance <C1>)
                                                 &rest initvalues)
            (if (= (x-val instance) 0)
                (setf (x-val instance) (y-val instance))))))
  (eq m (find-method #'initialize-instance '(:after) (list c))))
T

(x-val (make-instance (find-class '<C1>)))
1

(x-val (make-instance (find-class '<C1>) :x 10))
10

(x-val (make-instance (find-class '<C1>) :y 20))
20

(x-val (make-instance (find-class '<C1>) :x 10 :y 20))
10

(let ((m (defmethod initialize-instance ((inst <C1>) &rest ignore)
           (call-next-method)
           123)))
  (eq m (find-method #'initialize-instance nil (list (find-class '<C1>)))))
T

(x-val (make-instance (find-class '<C1>) :x 101 :y 120))
101

(setf (find-class '<C1>) nil)
nil

(unintern '<C1>)
T

(eq (class-of ())               (find-class 'null))
T

(eq (class-of t)                (find-class 'symbol))
T

(eq (class-of 10)               (find-class #+(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) 'fixnum #-(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) 'integer))
T

(eq (class-of 10.0)             (find-class #+(or ALLEGRO CMU SBCL OpenMCL) 'single-float #-(or ALLEGRO CMU SBCL OpenMCL) 'float))
T

(eq (class-of '(a b))           (find-class 'cons))
T

(eq (class-of "abc")            (find-class #+CMU 'simple-string #+(or SBCL OpenMCL LISPWORKS) 'simple-base-string #-(or CMU SBCL OpenMCL LISPWORKS) 'string))
T

(eq (class-of '#(1 2))          (find-class #+(or CMU SBCL OpenMCL LISPWORKS) 'simple-vector #-(or CMU SBCL OpenMCL LISPWORKS) 'vector))
T

(eq (class-of #'car)            (find-class 'function))
T

(eq (class-of #'make-instance)  (find-class 'standard-generic-function))
T

(eq (class-of '#2a((a) (b)))    (find-class #+(or CMU SBCL LISPWORKS) 'simple-array #-(or CMU SBCL LISPWORKS) 'array))
T

(eq (class-of *standard-input*) (find-class 'stream))
NIL

(eq (class-of (lambda (x) x))   (find-class 'function))
T

(eq (class-of (find-class 't)) (find-class 'built-in-class))
T

(eq (class-of (make-array nil)) (find-class #+(or CMU SBCL LISPWORKS) 'simple-array #-(or CMU SBCL LISPWORKS) 'array))  T
(eq (class-of (make-array nil :element-type nil)) (find-class #+(or CMU SBCL) 'simple-array #-(or CMU SBCL) 'array)) T
(eq (class-of (make-array 10 :element-type nil)) (find-class #+CMU 'simple-string #+SBCL 'sb-kernel::simple-array-nil #-(or CMU SBCL) 'string)) T

(typep "abc" (find-class 't))
T

(typep "abc" (find-class 'array))
T

(typep "abc" (find-class 'vector))
T

(typep "abc" (find-class 'string))
T

(typep "abc" (find-class 'integer))
NIL

(typep 3 (find-class 't))
T

(typep 3 (find-class 'number))
T

(typep 3 (find-class 'float))
NIL

(typep 3 (find-class 'integer))
T

(typep 3 (find-class 'string))
NIL

(not (not (typep *standard-input* (find-class 'stream))))
T

#+CLISP
(defun subclassp (class1 class2)
  (clos::subclassp class1 class2)
)
#+ALLEGRO
(defun subclassp (class1 class2)
  (finalize-inheritance class1)
  (not (null (member class2 (class-precedence-list class1))))
)
#+CMU
(defun subclassp (class1 class2)
  (not (null (member (car (pcl:class-precedence-list class2))
                     (pcl:class-precedence-list class1)
) )    )     )
#+SBCL
(defun subclassp (class1 class2)
  (not (null (member (car (sb-pcl:class-precedence-list class2))
                     (sb-pcl:class-precedence-list class1)
) )    )     )
#+(or OpenMCL LISPWORKS)
(defun subclassp (class1 class2)
  (not (null (member class2 (class-precedence-list class1))))
)
#+(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) SUBCLASSP

(subclassp (find-class 'number)           (find-class 't))
T

(subclassp (find-class 'integer)          (find-class 'number))
T

(subclassp (find-class 'float)            (find-class 'number))
T

;; make-load-form
(defun mlf-tester (symbol &optional (lisp-file "make-load-form-demo.lisp")
                   &aux (compiled-file (compile-file-pathname lisp-file)))
  (unwind-protect
       (progn
         (with-open-file (stream lisp-file :direction :output #+(or CMU SBCL) :if-exists #+(or CMU SBCL) :supersede)
           (format stream "(in-package ~s)~%(defparameter ~S '#.~S)~%"
                   (package-name (symbol-package symbol))
                   symbol symbol))
         (compile-file lisp-file)
         (setf (symbol-value symbol) nil)
         (load compiled-file)
         (symbol-value symbol))
    (delete-file compiled-file)
    (delete-file lisp-file)
    #+clisp (delete-file (make-pathname :type "lib" :defaults lisp-file))))
MLF-TESTER

(defun mlf-kill (type)
  (let ((m (find-method #'make-load-form nil (list (find-class type)) nil)))
    (when m (remove-method #'make-load-form m)))
  (setf (find-class type) nil))
mlf-kill

;; from kmp
(progn
  (defclass test-class1 () ((foo :initarg :foo :accessor foo :initform 0)))
  (defclass test-class2 () ((foo :initarg :foo :accessor foo :initform 0)))
  (defmethod make-load-form ((obj test-class1) &optional environment)
    (declare (ignore environment))
    `(make-instance 'test-class1 :foo ',(foo obj)))
  (defmethod make-load-form ((obj test-class2) &optional environment)
    (declare (ignore environment))
    `(make-instance 'test-class2 :foo ',(foo obj)))
  (defparameter *t-list*
    (list (make-instance 'test-class1 :foo 100)
          (make-instance 'test-class2 :foo 200)))
  (mlf-tester '*t-list*)
  (mapcar #'foo *t-list*))
(100 200)

;; from Christophe Rhodes <csr21@cam.ac.uk>
(defstruct foo a)
FOO

#-OpenMCL ; Bug in OpenMCL
(progn
  (defmethod make-load-form ((x foo) &optional env)
    (make-load-form-saving-slots x :environment env))
  (defparameter *tmp-file* "mlf-tmp.lisp")
  (with-open-file (s *tmp-file* :direction :output #+(or CMU SBCL) :if-exists #+(or CMU SBCL) :supersede)
    (format s "(defparameter *foo* '#S(FOO :A BAR-CONST))~%"))
  (load (compile-file *tmp-file*))
  *foo*)
#-OpenMCL
#S(FOO :A BAR-CONST)

(progn
  (makunbound '*foo*)
  (defconstant bar-const 1)
  (unwind-protect (progn (load (compile-file *tmp-file*)) *foo*)
    (delete-file *tmp-file*)
    (delete-file (compile-file-pathname *tmp-file*))
    #+clisp (delete-file (make-pathname :type "lib" :defaults *tmp-file*))
    (mlf-kill 'foo)))
#S(FOO :A BAR-CONST)

#+SBCL (unintern 'foo) #+SBCL t
#+SBCL (unintern 'copy-foo) #+SBCL t
#+SBCL (unintern 'make-foo) #+SBCL t

;; <http://www.lisp.org/HyperSpec/Issues/iss215-writeup.html>
(progn
  (defclass pos ()
    ((x :initarg :x :reader pos-x)
     (y :initarg :y :reader pos-y)
     (r :accessor pos-r)))
  (defmethod shared-initialize :after ((self pos) ignore1 &rest ignore2)
    (declare (ignore ignore1 ignore2))
    (unless (slot-boundp self 'r)
      (setf (pos-r self) (sqrt (+ (* (pos-x self) (pos-x self))
                                  (* (pos-y self) (pos-y self)))))))
  (defmethod make-load-form ((self pos) &optional environment)
    (declare (ignore environment))
    `(make-instance ',(class-name (class-of self))
                    :x ',(pos-x self) :y ',(pos-y self)))
  (setq *foo* (make-instance 'pos :x 3.0 :y 4.0))
  (mlf-tester '*foo*)
  (list (pos-x *foo*) (pos-y *foo*) (pos-r *foo*)))
(3.0 4.0 5.0)

(progn
  (defclass tree-with-parent ()
    ((parent :accessor tree-parent)
     (children :initarg :children)))
  (defmethod make-load-form ((x tree-with-parent) &optional environment)
    (declare (ignore environment))
    (values
     ;; creation form
     `(make-instance ',(class-name (class-of x)))
     ;; initialization form
     `(setf (tree-parent ',x) ',(slot-value x 'parent)
            (slot-value ',x 'children) ',(slot-value x 'children))))
  (setq *foo* (make-instance 'tree-with-parent :children
                             (list (make-instance 'tree-with-parent
                                                  :children nil)
                                   (make-instance 'tree-with-parent
                                                  :children nil))))
  (setf (tree-parent *foo*) *foo*)
  (dolist (ch (slot-value *foo* 'children))
    (setf (tree-parent ch) *foo*))
  (mlf-tester '*foo*)
  (list (eq *foo* (tree-parent *foo*))
        (every (lambda (x) (eq x *foo*))
               (mapcar #'tree-parent (slot-value *foo* 'children)))
        (every #'null
               (mapcar (lambda (x) (slot-value x 'children))
                       (slot-value *foo* 'children)))))
(T T T)

;; <http://www.lisp.org/HyperSpec/Issues/iss237-writeup.html>
(progn
  (defparameter *initform-executed-counter* 0)
  (defstruct foo (slot-1 (incf *initform-executed-counter*)))
  (defparameter *foo* (make-foo)))
*FOO*
*foo*                           #S(FOO :SLOT-1 1)
*initform-executed-counter*     1
(progn
  (mapc #'eval (multiple-value-list (make-load-form-saving-slots *foo*)))
  *initform-executed-counter*)
1
(progn
  (defmethod print-object ((f foo) (o stream))
    (format o "~1t<~a>" (foo-slot-1 f)))
  (prin1-to-string (make-foo)))
" <2>"

(progn (mlf-kill 'foo) nil)
nil

#+SBCL (unintern 'foo) #+SBCL t
#+SBCL (unintern 'copy-foo) #+SBCL t
#+SBCL (unintern 'make-foo) #+SBCL t

(defstruct foo slot)
FOO

;; From: Kaz Kylheku <kaz@ashi.footprints.net>
;; Date: Sat, 3 Jan 2004 14:47:25 -0800 (PST)
;; <http://article.gmane.org/gmane.lisp.clisp.general:7853>
(let ((file "foo.lisp") c)
  (unwind-protect
       (progn
         (makunbound '*foo*)
         (with-open-file (f file :direction :output #+(or CMU SBCL) :if-exists #+(or CMU SBCL) :supersede)
           (format f "(eval-when (compile load eval) (defstruct foo slot))~@
                      (defparameter *foo* #.(make-foo))~%"))
         (load (setq c (compile-file file)))
         *foo*)
    (delete-file file)
    (delete-file c)
    #+clisp (delete-file (make-pathname :type "lib" :defaults file))))
#+(or CLISP GCL LISPWORKS) #S(FOO :SLOT NIL)
#+(or ALLEGRO CMU SBCL) ERROR
#-(or CLISP GCL ALLEGRO CMU SBCL LISPWORKS) UNKNOWN

;; The finalized-direct-subclasses list must be weak.
#+clisp
(flet ((weak-list-length (w)
         (if w (sys::%record-ref (sys::%record-ref w 0) 1) 0)))
  (let (old1-weakpointers-count old-subclasses-count old2-weakpointers-count
        new-subclasses-count new-weakpointers-count)
    (defclass foo64a () ())
    (defclass foo64b (foo64a) ())
    (let ((usymbol (gensym)))
      (eval `(defclass ,usymbol (foo64a) ()))
      (setq old1-weakpointers-count (weak-list-length (clos::class-finalized-direct-subclasses-table (find-class 'foo64a))))
      (setf (symbol-value usymbol) (1- (length (clos::list-all-finalized-subclasses (find-class 'foo64a)))))
      (setq old2-weakpointers-count (weak-list-length (clos::class-finalized-direct-subclasses-table (find-class 'foo64a))))
      (setq old-subclasses-count (symbol-value usymbol)))
    (gc)
    (setq new-subclasses-count (1- (length (clos::list-all-finalized-subclasses (find-class 'foo64a)))))
    (setq new-weakpointers-count (weak-list-length (clos::class-finalized-direct-subclasses-table (find-class 'foo64a))))
    (list old1-weakpointers-count old-subclasses-count old2-weakpointers-count
          new-subclasses-count new-weakpointers-count)))
#+clisp
(2 2 2 1 1)

;; The direct-subclasses list must be weak.
#+clisp
(let (old-weakpointers-count new-weakpointers-count)
  (defclass foo64c () ())
  (defclass foo64d (foo64c) ())
  (let ((usymbol (gensym)))
    (eval `(defclass ,usymbol (foo64c) ()))
    (setq old-weakpointers-count (length (class-direct-subclasses (find-class 'foo64c))))
    (setf (symbol-value usymbol) nil))
  (gc)
  (setq new-weakpointers-count (length (class-direct-subclasses (find-class 'foo64c))))
  (list old-weakpointers-count new-weakpointers-count))
#+clisp
(2 1)

;; change-class
;; <http://www.lisp.org/HyperSpec/Body/stagenfun_change-class.html>
(progn
  (defclass abstract-position () ())
  (defclass x-y-position (abstract-position)
    ((name :initarg :name)
     (x :initform 0 :initarg :x)
     (y :initform 0 :initarg :y)))
  (defclass rho-theta-position (abstract-position)
    ((name :initarg :name)
     (rho :initform 0)
     (theta :initform 0)))
  (defmethod update-instance-for-different-class :before
      ((old x-y-position) (new rho-theta-position) &key)
    ;; Copy the position information from old to new to make new
    ;; be a rho-theta-position at the same position as old.
    (let ((x (slot-value old 'x))
          (y (slot-value old 'y)))
      (setf (slot-value new 'rho) (sqrt (+ (* x x) (* y y)))
            (slot-value new 'theta) (atan y x))))
  (setq p1 (make-instance 'x-y-position :name 'foo :x 2 :y 0)
        p2 (make-instance 'x-y-position :name 'bar :x 1 :y 1))
  (change-class p1 'rho-theta-position)
  (change-class p2 'rho-theta-position)
  (list (slot-value p1 'name) (slot-value p1 'rho) (slot-value p1 'theta)
        (slot-value p2 'name) (slot-value p2 'rho) (slot-value p2 'theta)))
#+CLISP (FOO 2 0 BAR 1.4142135 0.7853981)
#+(or ALLEGRO CMU SBCL OpenMCL) (FOO 2.0 0.0 BAR 1.4142135 0.7853982)
#+GCL (FOO 2.0 0.0 BAR 1.4142135623730951 0.78539816339744828)
#+LISPWORKS (FOO 2.0 0.0 BAR 1.4142135623730951 0.7853981633974483)
#-(or CLISP GCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(progn
  (defclass c0 () (a b c))
  (defclass c1 () (b c a))
  (setq i (make-instance 'c0))
  (setf (slot-value i 'a) 1 (slot-value i 'b) 2 (slot-value i 'c) 3)
  (change-class i 'c1)
  (list (slot-value i 'a) (slot-value i 'b) (slot-value i 'c)))
(1 2 3)

;; <https://sourceforge.net/tracker/?func=detail&aid=869187&group_id=1355&atid=101355>
(progn
  (defclass c1 () ())
  (defclass c2 () ())
  (list
   (let ((c (make-instance 'c1)))
     (list (type-of (change-class c 'c2))
           (type-of (change-class c 'c1))))
   (let ((c (make-instance 'c1)))
     (list (type-of (change-class c 'c1))
           (type-of (change-class c 'c1))))))
((C2 C1) (C1 C1))

;; Check that change-class uses its initargs.
(progn
  (defclass c7 () ((name :initarg :name)))
  (defclass c8 () ((people :initarg :people) name))
  (let ((x (make-instance 'c7 :name 'queen-mary)))
    (change-class x 'c8 :people 700)
    (list (slot-value x 'name) (slot-value x 'people))))
(QUEEN-MARY 700)

;; Check that a GC collects the forward pointer left over by change-class.
#+CLISP
(progn
  (defclass c3 () (a b c))
  (defclass c4 () (b c d e))
  (let* ((i (make-instance 'c3))
         (nslots-before (sys::%record-length i)))
    (change-class i 'c4)
    (gc)
    (< nslots-before (sys::%record-length i))))
#+CLISP
T

;; Redefining a finalized class must not change its identity.
(let (c1 c2)
  (defclass foo60-b () ())
  (defclass foo60-a (foo60-b) ())
  (make-instance 'foo60-b)
  (setq c1 (find-class 'foo60-a))
  (defclass foo60-a () ())
  (setq c2 (find-class 'foo60-a))
  (eq c1 c2))
T

;; Redefining a non-finalized class must not change its identity.
(let (c1 c2)
  (defclass foo61-a (foo61-b) ())
  (setq c1 (find-class 'foo61-a))
  (defclass foo61-a () ())
  (setq c2 (find-class 'foo61-a))
  (eq c1 c2))
T

;; SUBTYPEP must work on finalized classes.
(progn
  (defclass foo62-b (foo62-a) ())
  (defclass foo62-c (foo62-b) ())
  (defclass foo62-a () ())
  (make-instance 'foo62-c)
  (list (subtypep 'foo62-b 'foo62-b)
        (subtypep 'foo62-c 'foo62-b)
        (subtypep 'foo62-b 'foo62-c)))
(T T NIL)

;; SUBTYPEP must work on non-finalized classes.
(progn
  (defclass foo63-b (foo63-a) ())
  (defclass foo63-c (foo63-b) ())
  (defclass foo63-a () ())
  (list (subtypep 'foo63-b 'foo63-b)
        (subtypep 'foo63-c 'foo63-b)
        (subtypep 'foo63-b 'foo63-c)))
(T T NIL)

;; Redefining a class can make it (and also its subclasses) non-finalized.
#+CLISP
(let (fa fb fc)
  (defclass foo65a () ())
  (defclass foo65b (foo65a) ())
  (defclass foo65c (foo65b) ())
  (setq fa (clos:class-finalized-p (find-class 'foo65a))
        fb (clos:class-finalized-p (find-class 'foo65b))
        fc (clos:class-finalized-p (find-class 'foo65c)))
  (defclass foo65b (foo65a foo65other) ())
  (list fa fb fc
        (clos:class-finalized-p (find-class 'foo65a))
        (clos:class-finalized-p (find-class 'foo65b))
        (clos:class-finalized-p (find-class 'foo65c))))
#+CLISP
(T T T T NIL NIL)

;; update-instance-for-redefined-class
;; <http://www.lisp.org/HyperSpec/Body/stagenfun_upd_efined-class.html>
(progn
  (defclass abstract-position () ())
  (defclass x-y-position (abstract-position)
    ((x :initform 0 :accessor position-x)
     (y :initform 0 :accessor position-y)))
  (setf i (make-instance 'x-y-position)
        (position-x i) 1d0
        (position-y i) 1d0)
  (type-of i))
x-y-position

(progn
  ;; It turns out polar coordinates are used more than Cartesian
  ;; coordinates, so the representation is altered and some new
  ;; accessor methods are added.
  (defmethod update-instance-for-redefined-class :before
      ((pos x-y-position) added deleted plist &key)
    ;; Transform the x-y coordinates to polar coordinates
    ;; and store into the new slots.
    (let ((x (getf plist 'x))
          (y (getf plist 'y)))
      (setf (position-rho pos) (sqrt (+ (* x x) (* y y)))
            (position-theta pos) (atan y x))))
  (defclass x-y-position (abstract-position)
    ((rho :initform 0 :accessor position-rho)
     (theta :initform 0 :accessor position-theta)))
  ;; All instances of the old x-y-position class will be updated
  ;; automatically.
  ;; The new representation is given the look and feel of the old one.
  (defmethod position-x ((pos x-y-position))
    (with-slots (rho theta) pos (* rho (cos theta))))
  (defmethod (setf position-x) (new-x (pos x-y-position))
    (with-slots (rho theta) pos
      (let ((y (position-y pos)))
        (setq rho (sqrt (+ (* new-x new-x) (* y y)))
              theta (atan y new-x))
        new-x)))
  (defmethod position-y ((pos x-y-position))
    (with-slots (rho theta) pos (* rho (sin theta))))
  (defmethod (setf position-y) (new-y (pos x-y-position))
    (with-slots (rho theta) pos
      (let ((x (position-x pos)))
        (setq rho (sqrt (+ (* x x) (* new-y new-y)))
              theta (atan new-y x))
        new-y)))
  (list (type-of i) (position-x i) (position-y i)
        (position-rho i) (position-theta i)))
#+OpenMCL (X-Y-POSITION 1.0d0 1.0000000000000002d0
                        1.4142135623730951d0 0.7853981633974483d0)
#-OpenMCL (X-Y-POSITION 1.0000000000000002d0 1.0d0
                        1.4142135623730951d0 0.7853981633974483d0)


;; 4.3.6. Redefining Classes

;; Newly added local slot.
;; 4.3.6.1.: "Local slots specified by the new class definition that are not
;;            specified as either local or shared by the old class are added."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo70 () ())
      (setq i (make-instance 'foo70))
      (defclass foo70 () ((size :initarg :size :initform 1) (other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(1 NIL)

;; Newly added shared slot.
;; 4.3.6.: "Newly added shared slots are initialized."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo71 () ())
      (setq i (make-instance 'foo71))
      (defclass foo71 () ((size :initarg :size :initform 1 :allocation :class) (other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(1 NIL)

;; Discarded local slot.
;; 4.3.6.1.: "Slots not specified as either local or shared by the new class
;;            definition that are specified as local by the old class are
;;            discarded."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo72 () ((size :initarg :size :initform 1)))
      (setq i (make-instance 'foo72 :size 5))
      (defclass foo72 () ((other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(NIL T)

;; Discarded shared slot.
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo73 () ((size :initarg :size :initform 1 :allocation :class)))
      (setq i (make-instance 'foo73))
      (defclass foo73 () ((other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(NIL T)

;; Shared slot remains shared.
;; 4.3.6.: "The value of a slot that is specified as shared both in the old
;;          class and in the new class is retained."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo74 () ((size :initarg :size :initform 1 :allocation :class)))
      (setq i (make-instance 'foo74))
      (defclass foo74 () ((size :initarg :size :initform 2 :allocation :class) (other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(1 NIL)

;; Shared slot becomes local.
;; 4.3.6.1.: "The value of a slot that is specified as shared in the old class
;;            and as local in the new class is retained."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo75 () ((size :initarg :size :initform 1 :allocation :class)))
      (setq i (make-instance 'foo75))
      (defclass foo75 () ((size :initarg :size :initform 2) (other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(1 NIL)

;; Local slot remains local.
;; 4.3.6.1.: "The values of local slots specified by both the new and old
;;            classes are retained."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo76 () ((size :initarg :size :initform 1)))
      (setq i (make-instance 'foo76 :size 5))
      (defclass foo76 () ((size :initarg :size :initform 2) (other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(5 NIL)

;; Local slot becomes shared.
;; 4.3.6.: "Slots that were local in the old class and that are shared in the
;;          new class are initialized."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo77 () ((size :initarg :size :initform 1)))
      (setq i (make-instance 'foo77 :size 5))
      (defclass foo77 () ((size :initarg :size :initform 2 :allocation :class) (other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(2 NIL)


;; Redefining the superclass of an instance

;; Newly added local slot.
;; 4.3.6.1.: "Local slots specified by the new class definition that are not
;;            specified as either local or shared by the old class are added."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo80a () ())
      (defclass foo80b (foo80a) ())
      (setq i (make-instance 'foo80b))
      (defclass foo80a () ((size :initarg :size :initform 1) (other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(1 NIL)

;; Newly added shared slot.
;; 4.3.6.: "Newly added shared slots are initialized."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo81a () ())
      (defclass foo81b (foo81a) ())
      (setq i (make-instance 'foo81b))
      (defclass foo81a () ((size :initarg :size :initform 1 :allocation :class) (other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(1 NIL)

;; Discarded local slot.
;; 4.3.6.1.: "Slots not specified as either local or shared by the new class
;;            definition that are specified as local by the old class are
;;            discarded."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo82a () ((size :initarg :size :initform 1)))
      (defclass foo82b (foo82a) ())
      (setq i (make-instance 'foo82b :size 5))
      (defclass foo82a () ((other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(NIL T)

;; Discarded shared slot.
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo83a () ((size :initarg :size :initform 1 :allocation :class)))
      (defclass foo83b (foo83a) ())
      (setq i (make-instance 'foo83b))
      (defclass foo83a () ((other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(NIL T)

;; Shared slot remains shared.
;; 4.3.6.: "The value of a slot that is specified as shared both in the old
;;          class and in the new class is retained."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo84a () ((size :initarg :size :initform 1 :allocation :class)))
      (defclass foo84b (foo84a) ())
      (setq i (make-instance 'foo84b))
      (defclass foo84a () ((size :initarg :size :initform 2 :allocation :class) (other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(1 NIL)

;; Shared slot becomes local.
;; 4.3.6.1.: "The value of a slot that is specified as shared in the old class
;;            and as local in the new class is retained."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo85a () ((size :initarg :size :initform 1 :allocation :class)))
      (defclass foo85b (foo85a) ())
      (setq i (make-instance 'foo85b))
      (defclass foo85a () ((size :initarg :size :initform 2) (other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(1 NIL)

;; Local slot remains local.
;; 4.3.6.1.: "The values of local slots specified by both the new and old
;;            classes are retained."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo86a () ((size :initarg :size :initform 1)))
      (defclass foo86b (foo86a) ())
      (setq i (make-instance 'foo86b :size 5))
      (defclass foo86a () ((size :initarg :size :initform 2) (other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(5 NIL)

;; Local slot becomes shared.
;; 4.3.6.: "Slots that were local in the old class and that are shared in the
;;          new class are initialized."
(multiple-value-bind (value condition)
    (ignore-errors
      (defclass foo87a () ((size :initarg :size :initform 1)))
      (defclass foo87b (foo87a) ())
      (setq i (make-instance 'foo87b :size 5))
      (defclass foo87a () ((size :initarg :size :initform 2 :allocation :class) (other)))
      (slot-value i 'size))
  (list value (typep condition 'error)))
(2 NIL)


;; The clos::list-finalized-direct-subclasses function lists only finalized
;; direct subclasses.
#+CLISP
(progn
  (defclass foo88b (foo88a) ((s :initarg :s)))
  (defclass foo88c (b) ())
  (defclass foo88a () ())
  ; Here foo88a is finalized, foo88b and foo88c are not.
  (list
    (length (clos::list-finalized-direct-subclasses (find-class 'foo88a)))
    (length (clos::list-finalized-direct-subclasses (find-class 'foo88b)))
    (length (clos::list-finalized-direct-subclasses (find-class 'foo88c)))))
#+CLISP
(0 0 0)
#+CLISP
(progn
  (defclass foo89b (foo89a) ((s :initarg :s)))
  (defclass foo89c (b) ())
  (defclass foo89a () ())
  (let ((x (make-instance 'foo89b :s 5)))
    ; Here foo89a and foo89b are finalized, foo89c is not.
    (list
      (length (clos::list-finalized-direct-subclasses (find-class 'foo89a)))
      (length (clos::list-finalized-direct-subclasses (find-class 'foo89b)))
      (length (clos::list-finalized-direct-subclasses (find-class 'foo89c))))))
#+CLISP
(1 0 0)

;; The clos::list-finalized-direct-subclasses function must notice when a
;; finalized direct subclass is redefined in such a way that it is no longer
;; a subclass.
#+CLISP
(progn
  (defclass foo90b (foo90a) ((s :initarg :s)))
  (defclass foo90c (foo90b) ())
  (defclass foo90a () ())
  (let ((x (make-instance 'foo90b :s 5)))
    ; Here foo90a and foo90b are finalized, foo90c is not.
    (defclass foo90b () (s))
    ; Now foo90b is no longer direct subclass of foo90a.
    (list
      (length (clos::list-finalized-direct-subclasses (find-class 'foo90a)))
      (length (clos::list-finalized-direct-subclasses (find-class 'foo90b)))
      (length (clos::list-finalized-direct-subclasses (find-class 'foo90c))))))
#+CLISP
(0 0 0)

;; The clos::list-finalized-direct-subclasses function must notice when a
;; finalized direct subclass is redefined in such a way that it is no longer
;; finalized.
#+CLISP
(progn
  (defclass foo91a () ())
  (defclass foo91b (foo91a) ())
  (defclass foo91c (foo91b) ())
  (defclass foo91b (foo91a foo91other) ())
  (list
    (length (clos::list-finalized-direct-subclasses (find-class 'foo91a)))
    (length (clos::list-finalized-direct-subclasses (find-class 'foo91b)))
    (length (clos::list-finalized-direct-subclasses (find-class 'foo91c)))))
#+CLISP
(0 0 0)

;; make-instances-obsolete causes update-instance-for-redefined-class to
;; be called on instances of current subclasses.
(progn
  (defclass foo92b (foo92a) ((s :initarg :s)))
  (defclass foo92a () ())
  (let ((x (make-instance 'foo92b :s 5)) (update-counter 0))
    (defclass foo92b (foo92a) ((s) (s1) (s2))) ; still subclass of foo92a
    (slot-value x 's)
    (defmethod update-instance-for-redefined-class ((object foo92b) added-slots discarded-slots property-list &rest initargs)
      (incf update-counter))
    (make-instances-obsolete 'foo92a)
    (slot-value x 's)
    update-counter))
1

;; make-instances-obsolete does not cause update-instance-for-redefined-class
;; to be called on instances of ancient subclasses.
(progn
  (defclass foo93b (foo93a) ((s :initarg :s)))
  (defclass foo93a () ())
  (let ((x (make-instance 'foo93b :s 5)) (update-counter 0))
    (defclass foo93b () ((s) (s1) (s2))) ; no longer a subclass of foo93a
    (slot-value x 's)
    (defmethod update-instance-for-redefined-class ((object foo93b) added-slots discarded-slots property-list &rest initargs)
      (incf update-counter))
    (make-instances-obsolete 'foo93a)
    (slot-value x 's)
    update-counter))
0

;; Redefining a class removes the slot accessors installed on behalf of the
;; old class.
(progn
  (defclass foo94 () ((a :reader foo94-get-a :writer foo94-set-a)
                      (b :reader foo94-get-b :writer foo94-set-b)
                      (c :accessor foo94-c)
                      (d :accessor foo94-d)
                      (e :accessor foo94-e)))
  (list* (not (null (find-method #'foo94-get-a '() (list (find-class 'foo94)) nil)))
         (not (null (find-method #'foo94-set-a '() (list (find-class 't) (find-class 'foo94)) nil)))
         (not (null (find-method #'foo94-get-b '() (list (find-class 'foo94)) nil)))
         (not (null (find-method #'foo94-set-b '() (list (find-class 't) (find-class 'foo94)) nil)))
         (not (null (find-method #'foo94-c '() (list (find-class 'foo94)) nil)))
         (not (null (find-method #'(setf foo94-c) '() (list (find-class 't) (find-class 'foo94)) nil)))
         (not (null (find-method #'foo94-d '() (list (find-class 'foo94)) nil)))
         (not (null (find-method #'(setf foo94-d) '() (list (find-class 't) (find-class 'foo94)) nil)))
         (not (null (find-method #'foo94-e '() (list (find-class 'foo94)) nil)))
         (not (null (find-method #'(setf foo94-e) '() (list (find-class 't) (find-class 'foo94)) nil)))
         (progn
           (defclass foo94 () ((a :reader foo94-get-a :writer foo94-set-a)
                               (b)
                               (c :accessor foo94-c)
                               (e :accessor foo94-other-e)))
           (list (not (null (find-method #'foo94-get-a '() (list (find-class 'foo94)) nil)))
                 (not (null (find-method #'foo94-set-a '() (list (find-class 't) (find-class 'foo94)) nil)))
                 (not (null (find-method #'foo94-get-b '() (list (find-class 'foo94)) nil)))
                 (not (null (find-method #'foo94-set-b '() (list (find-class 't) (find-class 'foo94)) nil)))
                 (not (null (find-method #'foo94-c '() (list (find-class 'foo94)) nil)))
                 (not (null (find-method #'(setf foo94-c) '() (list (find-class 't) (find-class 'foo94)) nil)))
                 (not (null (find-method #'foo94-d '() (list (find-class 'foo94)) nil)))
                 (not (null (find-method #'(setf foo94-d) '() (list (find-class 't) (find-class 'foo94)) nil)))
                 (not (null (find-method #'foo94-e '() (list (find-class 'foo94)) nil)))
                 (not (null (find-method #'(setf foo94-e) '() (list (find-class 't) (find-class 'foo94)) nil)))))))
(T T T T T T T T T T
 T T NIL NIL T T NIL NIL NIL NIL)

;; It is possible to redefine a class in a way that makes it non-finalized,
;; if it was not yet instantiated.
(progn
  (defclass foo95b () ((s :initarg :s :accessor foo95b-s)))
  (defclass foo95b (foo95a) ((s :accessor foo95b-s)))
  t)
T

;; When redefining a class in a way that makes it non-finalized, and it was
;; already instantiated, an error is signalled, and the instances survive it.
(let ((notes '()))
  (flet ((note (o) (setq notes (append notes (list o)))))
    (defclass foo96b () ((s :initarg :s :accessor foo96b-s)))
    (let ((x (make-instance 'foo96b :s 5)))
      (note (foo96b-s x))
      (note
        (typep
          (second
            (multiple-value-list
              (ignore-errors
                (defclass foo96b (foo96a) ((s :accessor foo96b-s))))))
          'error))
      (note (foo96b-s x))
      (note (slot-value x 's))
      (defclass foo96a () ((r :accessor foo96b-r)))
      (note (foo96b-s x))
      (note (slot-value x 's))
      (note (subtypep 'foo96b 'foo96a))
      notes)))
(5 T 5 5 5 5 NIL)
(let ((notes '()))
  (flet ((note (o) (setq notes (append notes (list o)))))
    (defclass foo97b () ((s :initarg :s :accessor foo97b-s)))
    (let ((x (make-instance 'foo97b :s 5)))
      (note (foo97b-s x))
      (note
        (typep
          (second
            (multiple-value-list
              (ignore-errors
                (defclass foo97b (foo97a) ((s :accessor foo97b-s))))))
          'error))
      (note (foo97b-s x))
      (note (slot-value x 's))
      (defclass foo97a () ((r :accessor foo97b-r)))
      (note (foo97b-s x))
      (note (slot-value x 's))
      (note (subtypep 'foo97b 'foo97a))
      notes)))
(5 T 5 5 5 5 NIL)


;; Test the :fixed-slot-location option.

; Single class.
#+CLISP
(progn
  (defclass foo100 () (a b c) (:fixed-slot-locations t))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo100))
                                :key #'clos:slot-definition-name)))
                (clos:slot-definition-location slot)))
          '(a b c)))
#+CLISP
(1 2 3)

; Simple subclass.
#+CLISP
(progn
  (defclass foo101a () (a b c) (:fixed-slot-locations t))
  (defclass foo101b (foo101a) (d e f) (:fixed-slot-locations t))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo101b))
                                :key #'clos:slot-definition-name)))
                (clos:slot-definition-location slot)))
          '(a b c d e f)))
#+CLISP
(1 2 3 4 5 6)

; Subclass with multiple inheritance.
#+CLISP
(progn
  (defclass foo102a () (a b c) (:fixed-slot-locations t))
  (defclass foo102b () (d e f))
  (defclass foo102c (foo102a foo102b) (g h i))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo102c))
                                :key #'clos:slot-definition-name)))
                (clos:slot-definition-location slot)))
          '(a b c d e f g h i)))
#+CLISP
(1 2 3 4 5 6 7 8 9)

; Subclass with multiple inheritance.
#+CLISP
(progn
  (defclass foo103a () (a b c))
  (defclass foo103b () (d e f) (:fixed-slot-locations t))
  (defclass foo103c (foo103a foo103b) (g h i))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo103c))
                                :key #'clos:slot-definition-name)))
                (clos:slot-definition-location slot)))
          '(a b c d e f g h i)))
#+CLISP
(4 5 6 1 2 3 7 8 9)

; Subclass with multiple inheritance and collision.
#+CLISP
(progn
  (defclass foo104a () (a b c) (:fixed-slot-locations t))
  (defclass foo104b () (d e f) (:fixed-slot-locations t))
  (defclass foo104c (foo104a foo104b) (g h i))
  t)
#+CLISP
ERROR

; Subclass with multiple inheritance and no collision.
#+CLISP
(progn
  (defclass foo105a () (a b c) (:fixed-slot-locations t))
  (defclass foo105b () () (:fixed-slot-locations t))
  (defclass foo105c (foo105a foo105b) (g h i))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo105c))
                                :key #'clos:slot-definition-name)))
                (clos:slot-definition-location slot)))
          '(a b c g h i)))
#+CLISP
(1 2 3 4 5 6)

; Subclass with multiple inheritance and no collision.
#+CLISP
(progn
  (defclass foo106a () () (:fixed-slot-locations t))
  (defclass foo106b () (d e f) (:fixed-slot-locations t))
  (defclass foo106c (foo106a foo106b) (g h i))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo106c))
                                :key #'clos:slot-definition-name)))
                (clos:slot-definition-location slot)))
          '(d e f g h i)))
#+CLISP
(1 2 3 4 5 6)

; Subclass with shared slots.
#+CLISP
(progn
  (defclass foo107a ()
    ((a :allocation :instance)
     (b :allocation :instance)
     (c :allocation :class)
     (d :allocation :class)
     (e :allocation :class))
    (:fixed-slot-locations t))
  (defclass foo107b (foo107a)
    ((b :allocation :class)))
  t)
#+CLISP
ERROR

; Subclass with shared slots and no collision.
#+CLISP
(progn
  (defclass foo108a ()
    ((a :allocation :instance)
     (b :allocation :instance)
     (c :allocation :class)
     (d :allocation :class)
     (e :allocation :class))
    (:fixed-slot-locations t))
  (defclass foo108b (foo108a)
    (; (b :allocation :class) ; gives error, see above
     (c :allocation :instance)
     (d :allocation :class)
     (f :allocation :instance)
     (g :allocation :class)))
  (mapcar #'(lambda (name)
              (let ((slot (find name (clos::class-slots (find-class 'foo108b))
                                :key #'clos:slot-definition-name)))
                (let ((location (clos:slot-definition-location slot)))
                  (if (consp location)
                    (class-name (clos::cv-newest-class (car location)))
                    location))))
          '(a b c d e f g)))
#+CLISP
(1 2 3 foo108b foo108a 4 foo108b)

;; Check that two classes with the same name can have different documentation
;; strings.
(let ((class1 (defclass foo109 () () (:documentation "first"))))
  (cons (documentation class1 't)
        (progn
          (setf (find-class 'foo109) nil)
          (let ((class2 (defclass foo109 () () (:documentation "second"))))
            (list (documentation class1 't)
                  (documentation class2 't))))))
("first" "first" "second")

;; Check that invalid class options are rejected.
(defclass foo116 () () (:name bar))
ERROR
(defclass foo117 () () (:direct-superclasses baz))
ERROR
(defclass foo118 () () (:direct-slots x))
ERROR
(defclass foo119 () () (:direct-default-initargs (:x 5)))
ERROR
(defclass foo120 () () (:other-option blabla))
ERROR

;; Check that invalid slot options are rejected.
(defclass foo121 () ((x :name bar)))
ERROR
(defclass foo122 () ((x :readers (bar))))
ERROR
(defclass foo123 () ((x :writers (bar))))
ERROR
(defclass foo124 () ((x :initargs (bar))))
ERROR
(defclass foo125 () ((x :initform 17 :initfunction (lambda () 42))))
ERROR


;;; Check that changing an object's class clears the effective-methods or
;;; discriminating-function cache of all affected generic functions.
(progn
  (defclass testclass31a () ())
  (defclass testclass31b (testclass31a) ())
  (defclass testclass31c (testclass31b) ())
  (let ((*p* (make-instance 'testclass31c)))
    (defgeneric testgf37 (x))
    (defmethod testgf37 ((x testclass31a)) (list 'a))
    (defmethod testgf37 ((x testclass31b)) (cons 'b (call-next-method)))
    (defmethod testgf37 ((x testclass31c)) (cons 'c (call-next-method)))
    (defmethod testgf37 ((x (eql *p*))) (cons '*p* (call-next-method)))
    (list
      (testgf37 *p*)
      (progn
        (change-class *p* 'testclass31b)
        (testgf37 *p*)))))
((*P* C B A) (*P* B A))


;;; Check that redefining a class with different class-precedence-list
;;; clears the effective-methods or discriminating-function cache of all
;;; affected generic functions.

;; Class specializers.

; Case 1: Adding a class to a CPL.
(progn
  (defclass testclass40a () ())
  (defclass testclass40b () ())
  (defclass testclass40c (testclass40b) ())
  (defgeneric testgf40 (x) (:method-combination list))
  (defmethod testgf40 list ((x standard-object)) 0)
  (defmethod testgf40 list ((x testclass40a)) 'a)
  (let ((inst (make-instance 'testclass40c)))
    (list
      (testgf40 inst)
      (progn
        (defclass testclass40b (testclass40a) ())
        (testgf40 inst)))))
((0) (A 0))

; Case 2: Removing a class from a CPL.
(progn
  (defclass testclass41a () ())
  (defclass testclass41b (testclass41a) ())
  (defclass testclass41c (testclass41b) ())
  (defgeneric testgf41 (x) (:method-combination list))
  (defmethod testgf41 list ((x standard-object)) 0)
  (defmethod testgf41 list ((x testclass41a)) 'a)
  (let ((inst (make-instance 'testclass41c)))
    (list
      (testgf41 inst)
      (progn
        (defclass testclass41b () ())
        (testgf41 inst)))))
((A 0) (0))

; Case 3: Reordering a CPL.
(progn
  (defclass testclass42a () ())
  (defclass testclass42b () ())
  (defclass testclass42c (testclass42a testclass42b) ())
  (defgeneric testgf42 (x))
  (defmethod testgf42 ((x testclass42a)) 'a)
  (defmethod testgf42 ((x testclass42b)) 'b)
  (let ((inst (make-instance 'testclass42c)))
    (list
      (testgf42 inst)
      (progn
        (defclass testclass42c (testclass42b testclass42a) ())
        (testgf42 inst)))))
(A B)

;; EQL specializers.

; Case 1: Adding a class to a CPL.
(progn
  (defclass testclass45a () ())
  (defclass testclass45b () ())
  (defclass testclass45c (testclass45b) ())
  (let ((inst (make-instance 'testclass45c)))
    (defgeneric testgf45 (x) (:method-combination list))
    (defmethod testgf45 list ((x testclass45a)) 'a)
    (defmethod testgf45 list ((x (eql inst))) 'inst)
    (list
      (testgf45 inst)
      (progn
        (defclass testclass45b (testclass45a) ())
        (testgf45 inst)))))
((INST) (INST A))

; Case 2: Removing a class from a CPL.
(progn
  (defclass testclass46a () ())
  (defclass testclass46b (testclass46a) ())
  (defclass testclass46c (testclass46b) ())
  (let ((inst (make-instance 'testclass46c)))
    (defgeneric testgf46 (x) (:method-combination list))
    (defmethod testgf46 list ((x testclass46a)) 'a)
    (defmethod testgf46 list ((x (eql inst))) 'inst)
    (list
      (testgf46 inst)
      (progn
        (defclass testclass46b () ())
        (testgf46 inst)))))
((INST A) (INST))

; Case 3: Reordering a CPL.
(progn
  (defclass testclass47a () ())
  (defclass testclass47b () ())
  (defclass testclass47c (testclass47a testclass47b) ())
  (let ((inst (make-instance 'testclass47c)))
    (defgeneric testgf47 (x))
    (defmethod testgf47 ((x testclass47a)) 'a)
    (defmethod testgf47 ((x testclass47b)) 'b)
    (defmethod testgf47 ((x (eql inst))) (list 'inst (call-next-method)))
    (list
      (testgf47 inst)
      (progn
        (defclass testclass47c (testclass47b testclass47a) ())
        (testgf47 inst)))))
((INST A) (INST B))

;; EQL specializers on change-class'ed instances.

; Case 1: Adding a class to a CPL.
(progn
  (defclass testclass48a () ())
  (defclass testclass48b () ())
  (defclass testclass48c (testclass48b) ())
  (let ((inst (make-instance 'standard-object)))
    (defgeneric testgf48 (x) (:method-combination list))
    (defmethod testgf48 list ((x testclass48a)) 'a)
    (defmethod testgf48 list ((x (eql inst))) 'inst)
    (change-class inst 'testclass48c)
    (list
      (testgf48 inst)
      (progn
        (defclass testclass48b (testclass48a) ())
        (testgf48 inst)))))
((INST) (INST A))

; Case 2: Removing a class from a CPL.
(progn
  (defclass testclass49a () ())
  (defclass testclass49b (testclass49a) ())
  (defclass testclass49c (testclass49b) ())
  (let ((inst (make-instance 'standard-object)))
    (defgeneric testgf49 (x) (:method-combination list))
    (defmethod testgf49 list ((x testclass49a)) 'a)
    (defmethod testgf49 list ((x (eql inst))) 'inst)
    (change-class inst 'testclass49c)
    (list
      (testgf49 inst)
      (progn
        (defclass testclass49b () ())
        (testgf49 inst)))))
((INST A) (INST))

; Case 3: Reordering a CPL.
(progn
  (defclass testclass50a () ())
  (defclass testclass50b () ())
  (defclass testclass50c (testclass50a testclass50b) ())
  (let ((inst (make-instance 'standard-object)))
    (defgeneric testgf50 (x))
    (defmethod testgf50 ((x testclass50a)) 'a)
    (defmethod testgf50 ((x testclass50b)) 'b)
    (defmethod testgf50 ((x (eql inst))) (list 'inst (call-next-method)))
    (change-class inst 'testclass50c)
    (list
      (testgf50 inst)
      (progn
        (defclass testclass50c (testclass50b testclass50a) ())
        (testgf50 inst)))))
((INST A) (INST B))


;;; ensure-generic-function
;;; <http://www.lisp.org/HyperSpec/Body/fun_ensure-ge_ric-function.html>
(ensure-generic-function 'car) error
(ensure-generic-function 'defclass) error
(ensure-generic-function 'tagbody) error

(let ((f 'egf-fun))
  (when (fboundp f) (fmakunbound f))
  (list
   (fboundp f)
   (typep (ensure-generic-function f) 'generic-function)
   (typep (ensure-generic-function f) 'generic-function)
   (typep (symbol-function f) 'generic-function)))
(nil t t t)

(let ((f 'egf-fun))
  (when (fboundp f) (fmakunbound f))
  (list
   (fboundp f)
   (typep (ensure-generic-function f :lambda-list '(a b c))
          'generic-function)
   ;; Test of incongruent generic function lambda list when no
   ;; methods exist
   (typep (ensure-generic-function f :lambda-list '(x y))
          'generic-function)
   (typep (symbol-function f) 'generic-function)))
(nil t t t)

(let ((f 'egf-fun))
  (when (fboundp f) (fmakunbound f))
  (list
   (fboundp f)
   (typep (ensure-generic-function f :lambda-list '(a b c))
          'generic-function)
   (typep (eval `(defmethod ,f ((a t)(b t)(c t)) (list a b c)))
          'standard-method)))
(nil t t)

;; Test of incongruent generic function lambda list when
;; some methods do exist
(ensure-generic-function 'egf-fun :lambda-list '(x y))
error

;; forward reference (GCL ansi test)
(let ((c1 (gensym)) (c2 (gensym)))
  (let ((class1 (eval `(defclass ,c1 (,c2) nil))))
    (if (not (typep class1 'class))
        1
        (let ((class2 (eval `(defclass ,c2 nil nil))))
          (if (not (typep class2 'class))
              2
              (let ((i1 (make-instance c1))
                    (i2 (make-instance c2)))
                (cond
                  ((not (typep i1 c1))     3)
                  ((not (typep i1 class1)) 4)
                  ((not (typep i1 c2))     5)
                  ((not (typep i1 class2)) 6)
                  ((typep i2 c1)           7)
                  ((typep i2 class1)       8)
                  ((not (typep i2 c2))     9)
                  ((not (typep i2 class2)) 10)
                  (t 'good))))))))
good

(let ((c1 (gensym)) (c2 (gensym)) (c3 (gensym)))
  (let ((class1 (eval `(defclass ,c1 (,c2 ,c3) nil))))
    (if (not (typep class1 'class))
        1
        (let ((class2 (eval `(defclass ,c2 nil nil))))
          (if (not (typep class2 'class))
              2
              (let ((class3 (eval `(defclass ,c3 nil nil))))
                (if (not (typep class3 'class))
                    3
                    (let ((i1 (make-instance c1))
                          (i2 (make-instance c2))
                          (i3 (make-instance c3)))
                      (cond
                        ((not (typep i1 c1))     4)
                        ((not (typep i1 class1)) 5)
                        ((not (typep i1 c2))     6)
                        ((not (typep i1 class2)) 7)
                        ((not (typep i1 c3))     8)
                        ((not (typep i1 class3)) 9)
                        ((typep i2 c1)           10)
                        ((typep i2 class1)       11)
                        ((typep i3 c1)           12)
                        ((typep i3 class1)       13)
                        ((not (typep i2 c2))     14)
                        ((not (typep i2 class2)) 15)
                        ((not (typep i3 c3))     16)
                        ((not (typep i3 class3)) 17)
                        ((typep i2 c3)           18)
                        ((typep i2 class3)       19)
                        ((typep i3 c2)           20)
                        ((typep i3 class2)       21)
                        (t 'good))))))))))
good

(let ((c1 (gensym)) (c2 (gensym)) (c3 (gensym)))
  (let ((class1 (eval `(defclass ,c1 (,c2) nil))))
    (if (not (typep class1 'class))
        1
        (let ((class2 (eval `(defclass ,c2 (,c3) nil))))
          (if (not (typep class2 'class))
              2
              (let ((class3 (eval `(defclass ,c3 nil nil))))
                (if (not (typep class3 'class))
                    3
                    (let ((i1 (make-instance c1))
                          (i2 (make-instance c2))
                          (i3 (make-instance c3)))
                      (cond
                        ((not (typep i1 c1))     4)
                        ((not (typep i1 class1)) 5)
                        ((not (typep i1 c2))     6)
                        ((not (typep i1 class2)) 7)
                        ((not (typep i1 c3))     8)
                        ((not (typep i1 class3)) 9)
                        ((typep i2 c1)           10)
                        ((typep i2 class1)       11)
                        ((typep i3 c1)           12)
                        ((typep i3 class1)       13)
                        ((not (typep i2 c2))     14)
                        ((not (typep i2 class2)) 15)
                        ((not (typep i3 c3))     16)
                        ((not (typep i3 class3)) 17)
                        ((not (typep i2 c3))     18)
                        ((not (typep i2 class3)) 19)
                        ((typep i3 c2)           20)
                        ((typep i3 class2)       21)
                        (t 'good))))))))))
good

(block nil
  (let ((c1 (gensym)) (c2 (gensym)) (c3 (gensym)) (c4 (gensym)) (c5 (gensym)))
    (unless (typep (eval `(defclass ,c4 nil nil)) 'class)
      (return 1))
    (unless (typep (eval `(defclass ,c5 nil nil)) 'class)
      (return 2))
    (unless (typep (eval `(defclass ,c1 (,c2 ,c3) nil)) 'class)
      (return 3))
    (unless (typep (eval `(defclass ,c2 (,c4 ,c5) nil)) 'class)
      (return 4))
    (eval `(progn
             (defclass ,c3 (,c5 ,c4) nil)
             (make-instance ',c1)))))
error

(progn
  (defclass class-0203 () ((a :allocation :class) (b :allocation :instance)))
  (defclass class-0204 (class-0203) (c d))
  (let ((c1 (make-instance 'class-0203)) (c2 (make-instance 'class-0204)))
    (list
     :bound (slot-boundp c1 'a) (slot-boundp c1 'b)
     (slot-boundp c2 'a) (slot-boundp c2 'b)
     (slot-boundp c2 'c) (slot-boundp c2 'd)
     (setf (slot-value c1 'a) 'x)
     :bound (slot-boundp c1 'a) (slot-boundp c1 'b)
     (slot-boundp c2 'a) (slot-boundp c2 'b)
     (slot-boundp c2 'c) (slot-boundp c2 'd)
     (slot-value c1 'a)
     (slot-value c2 'a)
     (eq (slot-makunbound c1 'a) c1)
     :bound (slot-boundp c1 'a) (slot-boundp c1 'b)
     (slot-boundp c2 'a) (slot-boundp c2 'b)
     (slot-boundp c2 'c) (slot-boundp c2 'd))))
(:bound nil nil nil nil nil nil
 x
 :bound t nil t nil nil nil
 x x
 t
 :bound nil nil nil nil nil nil)

(progn
  (defclass class-0206a () ((a :allocation :instance) (b :allocation :class)))
  (defclass class-0206b (class-0206a)
    ((a :allocation :class) (b :allocation :instance)))
  (let ((c1 (make-instance 'class-0206a)) (c2 (make-instance 'class-0206b)))
    (list
     :bound (slot-boundp c1 'a) (slot-boundp c1 'b)
     (slot-boundp c2 'a) (slot-boundp c2 'b)
     (setf (slot-value c1 'a) 'x)
     (setf (slot-value c1 'b) 'y)
     :bound (slot-boundp c1 'a) (slot-boundp c1 'b)
     (slot-boundp c2 'a) (slot-boundp c2 'b)
     :value-1
     (slot-value c1 'a) (slot-value c1 'b)
     (progn (slot-makunbound c1 'a)
            (slot-makunbound c1 'b)
            (setf (slot-value c2 'a) 'x))
     (setf (slot-value c2 'b) 'y)
     :bound (slot-boundp c1 'a) (slot-boundp c1 'b)
     (slot-boundp c2 'a) (slot-boundp c2 'b)
     :value-2
     (slot-value c2 'a) (slot-value c2 'b)
     (progn (slot-makunbound c2 'a)
            (slot-makunbound c2 'b)
            nil))))
(:bound nil nil nil nil
 x y
 :bound t t nil nil
 :value-1 x y
 x y
 :bound nil nil t t
 :value-2 x y
 nil)

(let* ((c (defclass reinit-class-01 ()
            ((a :initarg :a) (b :initarg :b))))
       (m (defmethod reinitialize-instance :after ((instance reinit-class-01)
                                                   &rest initargs
                                                   &key (x nil x-p))
            (declare (ignore initargs))
            (when x-p (setf (slot-value instance 'a) x))
            instance)))
  (eq m (find-method #'reinitialize-instance '(:after) (list c))))
T

(let* ((obj (make-instance 'reinit-class-01))
       (obj2 (reinitialize-instance obj :a 1 :b 3)))
  (list (eq obj obj2) (slot-value obj2 'a) (slot-value obj2 'b)))
(t 1 3)

(let* ((obj (make-instance 'reinit-class-01 :a 10 :b 20))
       (obj2 (reinitialize-instance obj :x 3)))
  (list (eq obj obj2) (slot-value obj2 'a) (slot-value obj2 'b)))
(t 3 20)

(let* ((obj (make-instance 'reinit-class-01 :a 10 :b 20))
       (obj2 (reinitialize-instance obj :x 3 :x 100)))
  (list (eq obj obj2) (slot-value obj2 'a) (slot-value obj2 'b)))
(t 3 20)

(let* ((obj (make-instance 'reinit-class-01 :a 10 :b 20))
       (obj2 (reinitialize-instance obj :x 3 :garbage 100)))
  (list (eq obj obj2) (slot-value obj2 'a) (slot-value obj2 'b)))
error

;; Check that invalid generic-function options are rejected.
(defgeneric foo126 (x y) (:lambda-list x))
ERROR
(defgeneric foo127 (x y) (:declarations (optimize (speed 3))))
ERROR

(let ((gf1 (defgeneric no-app-meth-gf-01 ()))
      (gf2 (defgeneric no-app-meth-gf-02 (x)))
      (gf3 (defgeneric no-app-meth-gf-03 (x y))))
  (defmethod no-applicable-method ((x (eql gf1)) &rest args)
    (list 'no-applicable-method args))
  (defmethod no-applicable-method ((x (eql gf2)) &rest args)
    (list 'no-applicable-method args))
  (defmethod no-applicable-method ((x (eql gf3)) &rest args)
    (list 'no-applicable-method args))
  (list (no-app-meth-gf-01)
        (no-app-meth-gf-02 (cons 'a 'b))
        (no-app-meth-gf-03 (cons 'a 'b) (cons 'c 'd))))
((NO-APPLICABLE-METHOD nil)
 (NO-APPLICABLE-METHOD ((A . B)))
 (NO-APPLICABLE-METHOD ((A . B) (C . D))))

#+CLISP
(let ((gf1 (defgeneric no-prim-meth-gf-01 ()))
      (gf2 (defgeneric no-prim-meth-gf-02 (x)))
      (gf3 (defgeneric no-prim-meth-gf-03 (x y))))
  (defmethod no-prim-meth-gf-01 :around ()
    (list :around (call-next-method)))
  (defmethod no-primary-method ((x (eql gf1)) &rest args)
    (list 'no-primary-method args))
  (defmethod no-prim-meth-gf-02 :around ((x t))
    (list :around x (call-next-method)))
  (defmethod no-primary-method ((x (eql gf2)) &rest args)
    (list 'no-primary-method args))
  (defmethod no-prim-meth-gf-03 :around ((x t) (y t))
    (list :around x y (call-next-method)))
  (defmethod no-primary-method ((x (eql gf3)) &rest args)
    (list 'no-primary-method args))
  (list (no-prim-meth-gf-01)
        (no-prim-meth-gf-02 (cons 'a 'b))
        (no-prim-meth-gf-03 (cons 'a 'b) (cons 'c 'd))))
#+CLISP
((NO-PRIMARY-METHOD nil)
 (NO-PRIMARY-METHOD ((A . B)))
 (NO-PRIMARY-METHOD ((A . B) (C . D))))


;;; Method combinations

;; Standard method combination

(progn
  (defgeneric test-mc-standard (x)
    (:method ((x string)) (cons 'string (call-next-method)))
    (:method ((x t)) x))
  (list (test-mc-standard 1)
        (test-mc-standard "a")))
(1 (STRING . "a"))

; See also the hgen test above.

(progn
  (defgeneric test-mc-standard-bad-qualifiers (x y))
  (defmethod test-mc-standard-bad-qualifiers ((x integer) (y integer)) (+ x y))
  (defmethod test-mc-standard-bad-qualifiers ((x float) (y float)) (+ x y))
  (defmethod test-mc-standard-bad-qualifiers :beffor ((x float) (y float))
    (format t "x = ~S, y = ~S~%" x y))
  t)
#+(or CLISP CMU LISPWORKS) ERROR #+(or GCL ALLEGRO SBCL OpenMCL) T #-(or CLISP GCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(progn
  (defgeneric test-mc-standard-bad1 (x y))
  (defmethod test-mc-standard-bad1 ((x real) (y real)) (+ x y))
  (defmethod test-mc-standard-bad1 :after :before ((x integer) (y integer))
    (* x y))
  t)
#+(or CLISP ALLEGRO CMU LISPWORKS) ERROR #+(or SBCL OpenMCL) T #-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(progn
  (defgeneric test-mc-standard-bad2 (x y))
  (defmethod test-mc-standard-bad2 ((x real) (y real)) (+ x y))
  (defmethod test-mc-standard-bad2 :before ((x integer) (y integer))
    (floor (call-next-method)))
  (test-mc-standard-bad2 3 4))
ERROR

(progn
  (defgeneric test-mc-standard-bad3 (x y))
  (defmethod test-mc-standard-bad3 ((x real) (y real)) (+ x y))
  (defmethod test-mc-standard-bad3 :after ((x integer) (y integer))
    (floor (call-next-method)))
  (test-mc-standard-bad3 3 4))
ERROR

(progn
  (defgeneric test-mc-standard-bad4 (x y)
    (:method-combination standard :most-specific-last)))
ERROR

;; Built-in method combination

(progn
  (defgeneric test-mc-progn (x s)
    (:method-combination progn)
    (:method progn ((x string) s) (vector-push-extend 'string s))
    (:method progn ((x t) s) (vector-push-extend 't s))
    (:method :around ((x number) s)
             (vector-push-extend 'number s) (call-next-method)))
  (list (let ((s (make-array 10 :adjustable t :fill-pointer 0)))
          (test-mc-progn 1 s)
          s)
        (let ((s (make-array 10 :adjustable t :fill-pointer 0)))
          (test-mc-progn "a" s)
          s)))
(#(NUMBER T) #(STRING T))

; Test checking of qualifiers.
(progn
  (defgeneric test-mc-append-1 (x)
    (:method-combination append)
    (:method ((x string)) (list (length x)))
    (:method ((x vector)) (list (array-element-type x))))
  t)
#+(or CLISP CMU LISPWORKS) ERROR #+(or ALLEGRO SBCL OpenMCL) T #-(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

; Test ANSI CL 7.6.6.4.
(progn
  (defgeneric test-mc-append-2 (x)
    (:method-combination append)
    (:method append ((x string)) (list (length x)))
    (:method append ((x vector)) (list (type-of (aref x 0))))
    (:method :around ((x string)) (list #\" (call-next-method) #\"))
    (:method :around ((x vector)) (coerce (call-next-method) 'vector)))
  (test-mc-append-2 "abc"))
(#\" #(3 STANDARD-CHAR) #\")

; Check that :most-specific-last affects only the order of the primary methods.
(progn
  (defgeneric test-mc-append-3 (x)
    (:method-combination append :most-specific-last)
    (:method append ((x string)) (list (length x)))
    (:method append ((x vector)) (list (type-of (aref x 0))))
    (:method :around ((x string)) (list #\" (call-next-method) #\"))
    (:method :around ((x vector)) (coerce (call-next-method) 'vector)))
  (test-mc-append-3 "abc"))
(#\" #(STANDARD-CHAR 3) #\")

;; Short-form method combination

; Syntax
(define-method-combination mc01 :documentation :operator)
ERROR

; Syntax
(define-method-combination mc02 :documentation nil)
ERROR

; Syntax
(define-method-combination mc03 :documentation "foo" :documentation "bar")
ERROR

; Syntax
(define-method-combination mc04
  :identity-with-one-argument nil :operator list :documentation)
ERROR

(define-method-combination mc05
  :identity-with-one-argument nil :operator list :documentation "test")
MC05

; Check that the operator is called.
(progn
  (defgeneric test-mc05-1 (x)
    (:method mc05 ((x real)) 'real)
    (:method mc05 ((x integer)) 'integer)
    (:method mc05 ((x number)) 'number)
    (:method-combination mc05))
  (test-mc05-1 3))
(INTEGER REAL NUMBER)

; Check that the method-combination arguments are unevaluated.
(progn
  (defgeneric test-mc05-2 (x)
    (:method mc05 ((x real)) 'real)
    (:method mc05 ((x integer)) 'integer)
    (:method mc05 ((x number)) 'number)
    (:method-combination mc05 (intern "MOST-SPECIFIC-LAST" "KEYWORD")))
  (test-mc05-2 3))
ERROR

; Check that passing :most-specific-last as method-combination argument works.
(progn
  (defgeneric test-mc05-3 (x)
    (:method mc05 ((x real)) 'real)
    (:method mc05 ((x integer)) 'integer)
    (:method mc05 ((x number)) 'number)
    (:method-combination mc05 :most-specific-last))
  (test-mc05-3 3))
(NUMBER REAL INTEGER)

; Check that the operator is also called if there is just one method.
(progn
  (defgeneric test-mc05-4 (x)
    (:method mc05 ((x real)) 'real)
    (:method-combination mc05 :most-specific-last))
  (test-mc05-4 3))
(REAL)

; Check that nil is an invalid method-combination argument.
(progn
  (defgeneric test-mc05-5 (x)
    (:method mc05 ((x real)) 'real)
    (:method-combination mc05 nil)))
ERROR

; Check that extra method-combination arguments are rejected.
(progn
  (defgeneric test-mc05-6 (x)
    (:method mc05 ((x real)) 'real)
    (:method-combination mc05 :most-specific-first junk)))
ERROR

(define-method-combination mc06
  :identity-with-one-argument t :operator list :documentation "test")
MC06

; Check that the operator is not called if there is just one method.
(progn
  (defgeneric test-mc06-1 (x)
    (:method mc06 ((x real)) 'real)
    (:method-combination mc06 :most-specific-last))
  (test-mc06-1 3))
REAL

;; Long-form method combination

; Example from CLHS
(progn
  (defun positive-integer-qualifier-p (method-qualifiers)
    (and (= (length method-qualifiers) 1)
         (typep (first method-qualifiers) '(integer 0 *))))
  (define-method-combination example-method-combination ()
    ((method-list positive-integer-qualifier-p))
    `(PROGN ,@(mapcar #'(lambda (method) `(CALL-METHOD ,method))
                      (stable-sort method-list #'<
                                   :key #'(lambda (method)
                                            (first (method-qualifiers
                                                    method)))))))
  (defgeneric mc-test-piq (p1 p2 s)
    (:method-combination example-method-combination)
    (:method 1 ((p1 t) (p2 t) s) (vector-push-extend (list 1 p1 p2) s))
    (:method 4 ((p1 t) (p2 t) s) (vector-push-extend (list 4 p1 p2) s))
    (:method 2 ((p1 t) (p2 t) s) (vector-push-extend (list 2 p1 p2) s))
    (:method 3 ((p1 t) (p2 t) s) (vector-push-extend (list 3 p1 p2) s)))
  (let ((s (make-array 10 :adjustable t :fill-pointer 0)))
    (mc-test-piq 1 2 s)
    s))
;#((1 1 2) (2 1 2) (3 1 2) (4 1 2))
; ANSI CL: "If the two methods play the same role and their order matters,
;           an error is signaled."
ERROR

; Example with :arguments.
(progn
  (define-method-combination w-args ()
    ((method-list *))
    (:arguments arg1 arg2 &aux (extra :extra))
    `(PROGN ,@(mapcar #'(lambda (method) `(CALL-METHOD ,method)) method-list)))
  (defgeneric mc-test-w-args (p1 p2 s)
    (:method-combination w-args)
    (:method ((p1 number) (p2 t) s)
      (vector-push-extend (list 'number p1 p2) s))
    (:method ((p1 string) (p2 t) s)
      (vector-push-extend (list 'string p1 p2) s))
    (:method ((p1 t) (p2 t) s) (vector-push-extend (list t p1 p2) s)))
  (let ((s (make-array 10 :adjustable t :fill-pointer 0)))
    (mc-test-w-args 1 2 s)
    s))
#((NUMBER 1 2) (T 1 2))

; Syntax
(define-method-combination mc11 ())
ERROR

; Syntax
(define-method-combination mc12 () ())
MC12

; Syntax
(define-method-combination mc13 () () (:arguments order &aux &key))
ERROR

; Syntax
(define-method-combination mc14 () () (:arguments &whole))
ERROR

(define-method-combination mc15 () () (:arguments order))
MC15

; Syntax
(define-method-combination mc16 () () (:generic-function))
ERROR

; Syntax
(define-method-combination mc17 () () (:generic-function gf1 gf2))
ERROR

; Syntax
(define-method-combination mc18 () () (:generic-function (gf)))
ERROR

(define-method-combination mc19 () () (:generic-function gf))
MC19

; Syntax
(define-method-combination mc20 () (a))
ERROR

; Syntax
(define-method-combination mc21 () ((3)))
ERROR

; Syntax
(define-method-combination mc22 () ((a)))
ERROR

(define-method-combination mc23 () ((a *)))
MC23

; Check that it's allowed (although redundant) to have multiple catch-all
; method groups.
(define-method-combination mc24 () ((a *) (b *))
  `(PROGN (CALL-METHOD ,(first a)) (CALL-METHOD ,(first b))))
MC24

; Check that an error is signaled if there is no applicable method.
(progn
  (define-method-combination mc25 () ((all ()))
    `(LIST 'RESULT ,@(mapcar #'(lambda (method) `(CALL-METHOD ,method)) all)))
  (defgeneric test-mc25 (x)
    (:method-combination mc25))
  (test-mc25 7))
ERROR

; Check that no error is signaled if there are applicable methods but the
; method combination chooses to ignore them.
(progn
  (define-method-combination mc26 () ((normal ()) (ignored (:ignore)))
    `(LIST 'RESULT ,@(mapcar #'(lambda (method) `(CALL-METHOD ,method)) normal)))
  (defgeneric test-mc26 (x)
    (:method-combination mc26)
    (:method :ignore ((x number)) (/ 0)))
  (test-mc26 7))
(RESULT)

; Check that a qualifier-pattern does not match qualifier lists that are
; subsets.
(progn
  (define-method-combination mc27 () ((normal ()) (ignored (:ignore :unused)))
    `(LIST 'RESULT ,@(mapcar #'(lambda (method) `(CALL-METHOD ,method)) normal)))
  (defgeneric test-mc27 (x)
    (:method-combination mc27)
    (:method :ignore ((x number)) (/ 0)))
  (test-mc27 7))
ERROR

; Check that multiple qualifier-patterns act as an OR.
(progn
  (define-method-combination mc28 () ((normal ()) (ignored (:ignore) (:unused)))
    `(LIST 'RESULT ,@(mapcar #'(lambda (method) `(CALL-METHOD ,method)) normal)))
  (defgeneric test-mc28 (x)
    (:method-combination mc28)
    (:method :ignore ((x number)) (/ 0)))
  (test-mc28 7))
(RESULT)

; Check that catch-all method groups don't comprise methods that are already
; matched by earlier method groups.
(progn
  (define-method-combination mc29 () ((ignored (:ignore) (:unused)) (other *))
    `(LIST 'RESULT ,@(mapcar #'(lambda (method) `(CALL-METHOD ,method)) other)))
  (defgeneric test-mc29 (x)
    (:method-combination mc29)
    (:method :ignore ((x number)) (/ 0)))
  (test-mc29 7))
(RESULT)

; Check the simultaneous presence of options and :arguments.
(define-method-combination mc50 (opt1 opt2) ((all *))
  (:arguments &whole whole arg1 arg2 &rest more-args)
  `(LIST ',opt1 ',opt2 'RESULT ,whole ,arg1 ,arg2 ,more-args))
MC50

(defgeneric test-mc50-1 (x)
  (:method-combination mc50 xyz))
ERROR

(progn
  (defgeneric test-mc50-2 (x)
    (:method-combination mc50 xyz "foo")
    (:method ((x integer)) (/ 0)))
  (test-mc50-2 7))
(XYZ "foo" RESULT (7) 7 NIL ())

(progn
  (defgeneric test-mc50-3 (x y z)
    (:method-combination mc50 xyz "bar")
    (:method ((x t) (y t) (z t)) (/ 0)))
  (test-mc50-3 'a 'b 'c))
(XYZ "bar" RESULT (A B C) A B NIL)

; Check the simultaneous presence of options (with &optional and &rest) and
; :arguments (with &key).
(define-method-combination mc51 (opt1 &optional opt2 &rest more-opts) ((all *))
  (:arguments &whole whole arg1 &key test test-not)
  `(LIST ',opt1 ',opt2 ',more-opts 'RESULT ,whole ,arg1 ,test ,test-not))
MC51

(defgeneric test-mc51-1 (x)
  (:method-combination mc51))
ERROR

(progn
  (defgeneric test-mc51-2 (x)
    (:method-combination mc51 "xyz")
    (:method ((x integer)) (/ 0)))
  (test-mc51-2 7))
("xyz" NIL NIL RESULT (7) 7 NIL NIL)

(progn
  (defgeneric test-mc51-3 (x)
    (:method-combination mc51 "xyz" "uvw")
    (:method ((x integer)) (/ 0)))
  (test-mc51-3 7))
("xyz" "uvw" NIL RESULT (7) 7 NIL NIL)

(progn
  (defgeneric test-mc51-4 (x)
    (:method-combination mc51 "xyz" "uvw" :foo :bar)
    (:method ((x integer)) (/ 0)))
  (test-mc51-4 7))
("xyz" "uvw" (:FOO :BAR) RESULT (7) 7 NIL NIL)

(progn
  (defgeneric test-mc51-5 (x &key test test-not key predicate)
    (:method-combination mc51 "xyz" "uvw" :foo :bar)
    (:method ((x integer) &key predicate test test-not key) (/ 0)))
  (test-mc51-5 7 :key 'FIRST :TEST-NOT 'EQUAL))
("xyz" "uvw" (:FOO :BAR) RESULT (7 :KEY FIRST :TEST-NOT EQUAL) 7 NIL EQUAL)

; Check :arguments with no arguments.
(define-method-combination mc60 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments)
  `(LIST ',opt1 ',opt2 'RESULT (CALL-METHOD ,(first all))))
MC60

(progn
  (defgeneric test-mc60-1 ()
    (:method-combination mc60 "xyz")
    (:method () '()))
  (test-mc60-1))
("xyz" "def" RESULT ())

(progn
  (defgeneric test-mc60-2 (x y)
    (:method-combination mc60 "xyz")
    (:method (x y) (list x y)))
  (test-mc60-2 'a 'b))
("xyz" "def" RESULT (A B))

(progn
  (defgeneric test-mc60-3 (&optional x y)
    (:method-combination mc60 "xyz")
    (:method (&optional x y) (list x y)))
  (test-mc60-3 'a))
("xyz" "def" RESULT (A NIL))

(progn
  (defgeneric test-mc60-4 (&rest x)
    (:method-combination mc60 "xyz")
    (:method (&rest x) x))
  (test-mc60-4 'a 'b))
("xyz" "def" RESULT (A B))

; Check :arguments with only required arguments.
(define-method-combination mc61 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments a1 a2)
  `(LIST ',opt1 ',opt2 'RESULT ,a1 ,a2 (CALL-METHOD ,(first all))))
MC61

(progn
  (defgeneric test-mc61-1 (x)
    (:method-combination mc61 "xyz")
    (:method (x) (list x)))
  (test-mc61-1 'a))
("xyz" "def" RESULT A NIL (A))

(progn
  (defgeneric test-mc61-2 (x y)
    (:method-combination mc61 "xyz")
    (:method (x y) (list x y)))
  (test-mc61-2 'a 'b))
("xyz" "def" RESULT A B (A B))

(progn
  (defgeneric test-mc61-3 (x y z)
    (:method-combination mc61 "xyz")
    (:method (x y z) (list x y z)))
  (test-mc61-3 'a 'b 'c))
("xyz" "def" RESULT A B (A B C))

(progn
  (defgeneric test-mc61-4 (x &optional y z)
    (:method-combination mc61 "xyz")
    (:method (x &optional y z) (list x y z)))
  (list (test-mc61-4 'a) (test-mc61-4 'a 'b) (test-mc61-4 'a 'b 'c)))
(("xyz" "def" RESULT A NIL (A NIL NIL))
 ("xyz" "def" RESULT A NIL (A B NIL))
 ("xyz" "def" RESULT A NIL (A B C)))

(progn
  (defgeneric test-mc61-5 (x y &optional z u)
    (:method-combination mc61 "xyz")
    (:method (x y &optional z u) (list x y z u)))
  (list (test-mc61-5 'a 'b) (test-mc61-5 'a 'b 'c) (test-mc61-5 'a 'b 'c 'd)))
(("xyz" "def" RESULT A B (A B NIL NIL))
 ("xyz" "def" RESULT A B (A B C NIL))
 ("xyz" "def" RESULT A B (A B C D)))

(progn
  (defgeneric test-mc61-6 (x y z &optional u v)
    (:method-combination mc61 "xyz")
    (:method (x y z &optional u v) (list x y z u v)))
  (list (test-mc61-6 'a 'b 'c) (test-mc61-6 'a 'b 'c 'd) (test-mc61-6 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A B (A B C NIL NIL))
 ("xyz" "def" RESULT A B (A B C D NIL))
 ("xyz" "def" RESULT A B (A B C D E)))

(progn
  (defgeneric test-mc61-7 (x &rest y)
    (:method-combination mc61 "xyz")
    (:method (x &rest y) (list* x y)))
  (list (test-mc61-7 'a) (test-mc61-7 'a 'b) (test-mc61-7 'a 'b 'c)))
(("xyz" "def" RESULT A NIL (A))
 ("xyz" "def" RESULT A NIL (A B))
 ("xyz" "def" RESULT A NIL (A B C)))

(progn
  (defgeneric test-mc61-8 (x y &rest z)
    (:method-combination mc61 "xyz")
    (:method (x y &rest z) (list* x y z)))
  (list (test-mc61-8 'a 'b) (test-mc61-8 'a 'b 'c) (test-mc61-8 'a 'b 'c 'd)))
(("xyz" "def" RESULT A B (A B))
 ("xyz" "def" RESULT A B (A B C))
 ("xyz" "def" RESULT A B (A B C D)))

(progn
  (defgeneric test-mc61-9 (x y z &rest u)
    (:method-combination mc61 "xyz")
    (:method (x y z &rest u) (list* x y z u)))
  (list (test-mc61-9 'a 'b 'c) (test-mc61-9 'a 'b 'c 'd) (test-mc61-9 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A B (A B C))
 ("xyz" "def" RESULT A B (A B C D))
 ("xyz" "def" RESULT A B (A B C D E)))

; Check :arguments with only optional arguments.
(define-method-combination mc62 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments &optional (o1 'def1) (o2 'def2))
  `(LIST ',opt1 ',opt2 'RESULT ,o1 ,o2 (CALL-METHOD ,(first all))))
MC62

(progn
  (defgeneric test-mc62-1 (x)
    (:method-combination mc62 "xyz")
    (:method (x) (list x)))
  (test-mc62-1 'a))
("xyz" "def" RESULT DEF1 DEF2 (A))

(progn
  (defgeneric test-mc62-2 (x &optional y)
    (:method-combination mc62 "xyz")
    (:method (x &optional y) (list x y)))
  (list (test-mc62-2 'a) (test-mc62-2 'a 'b)))
(("xyz" "def" RESULT DEF1 DEF2 (A NIL))
 ("xyz" "def" RESULT B DEF2 (A B)))

(progn
  (defgeneric test-mc62-3 (x &optional y z)
    (:method-combination mc62 "xyz")
    (:method (x &optional y z) (list x y z)))
  (list (test-mc62-3 'a) (test-mc62-3 'a 'b) (test-mc62-3 'a 'b 'c)))
(("xyz" "def" RESULT DEF1 DEF2 (A NIL NIL))
 ("xyz" "def" RESULT B DEF2 (A B NIL))
 ("xyz" "def" RESULT B C (A B C)))

(progn
  (defgeneric test-mc62-4 (x &optional y z u)
    (:method-combination mc62 "xyz")
    (:method (x &optional y z u) (list x y z u)))
  (list (test-mc62-4 'a) (test-mc62-4 'a 'b) (test-mc62-4 'a 'b 'c) (test-mc62-4 'a 'b 'c 'd)))
(("xyz" "def" RESULT DEF1 DEF2 (A NIL NIL NIL))
 ("xyz" "def" RESULT B DEF2 (A B NIL NIL))
 ("xyz" "def" RESULT B C (A B C NIL))
 ("xyz" "def" RESULT B C (A B C D)))

(progn
  (defgeneric test-mc62-5 (x &rest y)
    (:method-combination mc62 "xyz")
    (:method (x &rest y) (list* x y)))
  (list (test-mc62-5 'a) (test-mc62-5 'a 'b) (test-mc62-5 'a 'b 'c)))
(("xyz" "def" RESULT DEF1 DEF2 (A))
 ("xyz" "def" RESULT DEF1 DEF2 (A B))
 ("xyz" "def" RESULT DEF1 DEF2 (A B C)))

(progn
  (defgeneric test-mc62-6 (x &optional y &rest z)
    (:method-combination mc62 "xyz")
    (:method (x &optional y &rest z) (list* x y z)))
  (list (test-mc62-6 'a) (test-mc62-6 'a 'b) (test-mc62-6 'a 'b 'c)))
(("xyz" "def" RESULT DEF1 DEF2 (A NIL))
 ("xyz" "def" RESULT B DEF2 (A B))
 ("xyz" "def" RESULT B DEF2 (A B C)))

(progn
  (defgeneric test-mc62-7 (x &optional y z &rest u)
    (:method-combination mc62 "xyz")
    (:method (x &optional y z &rest u) (list* x y z u)))
  (list (test-mc62-7 'a) (test-mc62-7 'a 'b) (test-mc62-7 'a 'b 'c) (test-mc62-7 'a 'b 'c 'd)))
(("xyz" "def" RESULT DEF1 DEF2 (A NIL NIL))
 ("xyz" "def" RESULT B DEF2 (A B NIL))
 ("xyz" "def" RESULT B C (A B C))
 ("xyz" "def" RESULT B C (A B C D)))

; Check :arguments with only rest arguments.
(define-method-combination mc63 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments &rest r)
  `(LIST ',opt1 ',opt2 'RESULT ,r (CALL-METHOD ,(first all))))
MC63

(progn
  (defgeneric test-mc63-1 ()
    (:method-combination mc63 "xyz")
    (:method () '()))
  (test-mc63-1))
("xyz" "def" RESULT () ())

(progn
  (defgeneric test-mc63-2 (x y)
    (:method-combination mc63 "xyz")
    (:method (x y) (list x y)))
  (test-mc63-2 'a 'b))
("xyz" "def" RESULT () (A B))

(progn
  (defgeneric test-mc63-3 (&optional x y)
    (:method-combination mc63 "xyz")
    (:method (&optional x y) (list x y)))
  (test-mc63-3 'a))
("xyz" "def" RESULT () (A NIL))

(progn
  (defgeneric test-mc63-4 (&rest x)
    (:method-combination mc63 "xyz")
    (:method (&rest x) x))
  (test-mc63-4 'a 'b))
("xyz" "def" RESULT (A B) (A B))

; Check :arguments with required and optional arguments.
(define-method-combination mc64 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments a1 a2 &optional (o1 'def1) (o2 'def2))
  `(LIST ',opt1 ',opt2 'RESULT ,a1 ,a2 ,o1 ,o2 (CALL-METHOD ,(first all))))
MC64

(progn
  (defgeneric test-mc64-1 ()
    (:method-combination mc64 "xyz")
    (:method () '()))
  (test-mc64-1))
("xyz" "def" RESULT NIL NIL DEF1 DEF2 ())

(progn
  (defgeneric test-mc64-2 (x)
    (:method-combination mc64 "xyz")
    (:method (x) (list x)))
  (test-mc64-2 'a))
("xyz" "def" RESULT A NIL DEF1 DEF2 (A))

(progn
  (defgeneric test-mc64-3 (x y)
    (:method-combination mc64 "xyz")
    (:method (x y) (list x y)))
  (test-mc64-3 'a 'b))
("xyz" "def" RESULT A B DEF1 DEF2 (A B))

(progn
  (defgeneric test-mc64-4 (x y z)
    (:method-combination mc64 "xyz")
    (:method (x y z) (list x y z)))
  (test-mc64-4 'a 'b 'c))
("xyz" "def" RESULT A B DEF1 DEF2 (A B C))

(progn
  (defgeneric test-mc64-5 (x &optional y)
    (:method-combination mc64 "xyz")
    (:method (x &optional y) (list x y)))
  (list (test-mc64-5 'a) (test-mc64-5 'a 'b)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 (A NIL))
 ("xyz" "def" RESULT A NIL B DEF2 (A B)))

(progn
  (defgeneric test-mc64-6 (x y &optional z)
    (:method-combination mc64 "xyz")
    (:method (x y &optional z) (list x y z)))
  (list (test-mc64-6 'a 'b) (test-mc64-6 'a 'b 'c)))
(("xyz" "def" RESULT A B DEF1 DEF2 (A B NIL))
 ("xyz" "def" RESULT A B C DEF2 (A B C)))

(progn
  (defgeneric test-mc64-7 (x y z &optional u)
    (:method-combination mc64 "xyz")
    (:method (x y z &optional u) (list x y z u)))
  (list (test-mc64-7 'a 'b 'c) (test-mc64-7 'a 'b 'c 'd)))
(("xyz" "def" RESULT A B DEF1 DEF2 (A B C NIL))
 ("xyz" "def" RESULT A B D DEF2 (A B C D)))

(progn
  (defgeneric test-mc64-8 (x &optional y z)
    (:method-combination mc64 "xyz")
    (:method (x &optional y z) (list x y z)))
  (list (test-mc64-8 'a) (test-mc64-8 'a 'b) (test-mc64-8 'a 'b 'c)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 (A NIL NIL))
 ("xyz" "def" RESULT A NIL B DEF2 (A B NIL))
 ("xyz" "def" RESULT A NIL B C (A B C)))

(progn
  (defgeneric test-mc64-9 (x y &optional z u)
    (:method-combination mc64 "xyz")
    (:method (x y &optional z u) (list x y z u)))
  (list (test-mc64-9 'a 'b) (test-mc64-9 'a 'b 'c) (test-mc64-9 'a 'b 'c 'd)))
(("xyz" "def" RESULT A B DEF1 DEF2 (A B NIL NIL))
 ("xyz" "def" RESULT A B C DEF2 (A B C NIL))
 ("xyz" "def" RESULT A B C D (A B C D)))

(progn
  (defgeneric test-mc64-10 (x y z &optional u v)
    (:method-combination mc64 "xyz")
    (:method (x y z &optional u v) (list x y z u v)))
  (list (test-mc64-10 'a 'b 'c) (test-mc64-10 'a 'b 'c 'd) (test-mc64-10 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A B DEF1 DEF2 (A B C NIL NIL))
 ("xyz" "def" RESULT A B D DEF2 (A B C D NIL))
 ("xyz" "def" RESULT A B D E (A B C D E)))

(progn
  (defgeneric test-mc64-11 (x &optional y z u)
    (:method-combination mc64 "xyz")
    (:method (x &optional y z u) (list x y z u)))
  (list (test-mc64-11 'a) (test-mc64-11 'a 'b) (test-mc64-11 'a 'b 'c) (test-mc64-11 'a 'b 'c 'd)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 (A NIL NIL NIL))
 ("xyz" "def" RESULT A NIL B DEF2 (A B NIL NIL))
 ("xyz" "def" RESULT A NIL B C (A B C NIL))
 ("xyz" "def" RESULT A NIL B C (A B C D)))

(progn
  (defgeneric test-mc64-12 (x y &optional z u v)
    (:method-combination mc64 "xyz")
    (:method (x y &optional z u v) (list x y z u v)))
  (list (test-mc64-12 'a 'b) (test-mc64-12 'a 'b 'c) (test-mc64-12 'a 'b 'c 'd) (test-mc64-12 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A B DEF1 DEF2 (A B NIL NIL NIL))
 ("xyz" "def" RESULT A B C DEF2 (A B C NIL NIL))
 ("xyz" "def" RESULT A B C D (A B C D NIL))
 ("xyz" "def" RESULT A B C D (A B C D E)))

(progn
  (defgeneric test-mc64-13 (x y z &optional u v w)
    (:method-combination mc64 "xyz")
    (:method (x y z &optional u v w) (list x y z u v w)))
  (list (test-mc64-13 'a 'b 'c) (test-mc64-13 'a 'b 'c 'd) (test-mc64-13 'a 'b 'c 'd 'e) (test-mc64-13 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT A B DEF1 DEF2 (A B C NIL NIL NIL))
 ("xyz" "def" RESULT A B D DEF2 (A B C D NIL NIL))
 ("xyz" "def" RESULT A B D E (A B C D E NIL))
 ("xyz" "def" RESULT A B D E (A B C D E F)))

(progn
  (defgeneric test-mc64-14 (x &rest y)
    (:method-combination mc64 "xyz")
    (:method (x &rest y) (list* x y)))
  (list (test-mc64-14 'a) (test-mc64-14 'a 'b) (test-mc64-14 'a 'b 'c)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 (A))
 ("xyz" "def" RESULT A NIL DEF1 DEF2 (A B))
 ("xyz" "def" RESULT A NIL DEF1 DEF2 (A B C)))

(progn
  (defgeneric test-mc64-15 (x y &rest z)
    (:method-combination mc64 "xyz")
    (:method (x y &rest z) (list* x y z)))
  (list (test-mc64-15 'a 'b) (test-mc64-15 'a 'b 'c) (test-mc64-15 'a 'b 'c 'd)))
(("xyz" "def" RESULT A B DEF1 DEF2 (A B))
 ("xyz" "def" RESULT A B DEF1 DEF2 (A B C))
 ("xyz" "def" RESULT A B DEF1 DEF2 (A B C D)))

(progn
  (defgeneric test-mc64-16 (x y z &rest u)
    (:method-combination mc64 "xyz")
    (:method (x y z &rest u) (list* x y z u)))
  (list (test-mc64-16 'a 'b 'c) (test-mc64-16 'a 'b 'c 'd) (test-mc64-16 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A B DEF1 DEF2 (A B C))
 ("xyz" "def" RESULT A B DEF1 DEF2 (A B C D))
 ("xyz" "def" RESULT A B DEF1 DEF2 (A B C D E)))

(progn
  (defgeneric test-mc64-17 (x &optional y &rest z)
    (:method-combination mc64 "xyz")
    (:method (x &optional y &rest z) (list* x y z)))
  (list (test-mc64-17 'a) (test-mc64-17 'a 'b) (test-mc64-17 'a 'b 'c) (test-mc64-17 'a 'b 'c 'd)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 (A NIL))
 ("xyz" "def" RESULT A NIL B DEF2 (A B))
 ("xyz" "def" RESULT A NIL B DEF2 (A B C))
 ("xyz" "def" RESULT A NIL B DEF2 (A B C D)))

(progn
  (defgeneric test-mc64-18 (x &optional y z &rest u)
    (:method-combination mc64 "xyz")
    (:method (x &optional y z &rest u) (list* x y z u)))
  (list (test-mc64-18 'a) (test-mc64-18 'a 'b) (test-mc64-18 'a 'b 'c) (test-mc64-18 'a 'b 'c 'd) (test-mc64-18 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 (A NIL NIL))
 ("xyz" "def" RESULT A NIL B DEF2 (A B NIL))
 ("xyz" "def" RESULT A NIL B C (A B C))
 ("xyz" "def" RESULT A NIL B C (A B C D))
 ("xyz" "def" RESULT A NIL B C (A B C D E)))

(progn
  (defgeneric test-mc64-19 (x &optional y z u &rest v)
    (:method-combination mc64 "xyz")
    (:method (x &optional y z u &rest v) (list* x y z u v)))
  (list (test-mc64-19 'a) (test-mc64-19 'a 'b) (test-mc64-19 'a 'b 'c) (test-mc64-19 'a 'b 'c 'd) (test-mc64-19 'a 'b 'c 'd 'e) (test-mc64-19 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 (A NIL NIL NIL))
 ("xyz" "def" RESULT A NIL B DEF2 (A B NIL NIL))
 ("xyz" "def" RESULT A NIL B C (A B C NIL))
 ("xyz" "def" RESULT A NIL B C (A B C D))
 ("xyz" "def" RESULT A NIL B C (A B C D E))
 ("xyz" "def" RESULT A NIL B C (A B C D E F)))

; Check :arguments with required and rest arguments.
(define-method-combination mc65 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments a1 a2 &rest r)
  `(LIST ',opt1 ',opt2 'RESULT ,a1 ,a2 ,r (CALL-METHOD ,(first all))))
MC65

(progn
  (defgeneric test-mc65-1 ()
    (:method-combination mc65 "xyz")
    (:method () '()))
  (test-mc65-1))
("xyz" "def" RESULT NIL NIL () ())

(progn
  (defgeneric test-mc65-2 (x)
    (:method-combination mc65 "xyz")
    (:method (x) (list x)))
  (test-mc65-2 'a))
("xyz" "def" RESULT A NIL () (A))

(progn
  (defgeneric test-mc65-3 (x y)
    (:method-combination mc65 "xyz")
    (:method (x y) (list x y)))
  (test-mc65-3 'a 'b))
("xyz" "def" RESULT A B () (A B))

(progn
  (defgeneric test-mc65-4 (x y z)
    (:method-combination mc65 "xyz")
    (:method (x y z) (list x y z)))
  (test-mc65-4 'a 'b 'c))
("xyz" "def" RESULT A B () (A B C))

(progn
  (defgeneric test-mc65-5 (x &optional y)
    (:method-combination mc65 "xyz")
    (:method (x &optional y) (list x y)))
  (list (test-mc65-5 'a) (test-mc65-5 'a 'b)))
(("xyz" "def" RESULT A NIL () (A NIL))
 ("xyz" "def" RESULT A NIL () (A B)))

(progn
  (defgeneric test-mc65-6 (x y &optional z)
    (:method-combination mc65 "xyz")
    (:method (x y &optional z) (list x y z)))
  (list (test-mc65-6 'a 'b) (test-mc65-6 'a 'b 'c)))
(("xyz" "def" RESULT A B () (A B NIL))
 ("xyz" "def" RESULT A B () (A B C)))

(progn
  (defgeneric test-mc65-7 (x y z &optional u)
    (:method-combination mc65 "xyz")
    (:method (x y z &optional u) (list x y z u)))
  (list (test-mc65-7 'a 'b 'c) (test-mc65-7 'a 'b 'c 'd)))
(("xyz" "def" RESULT A B () (A B C NIL))
 ("xyz" "def" RESULT A B () (A B C D)))

(progn
  (defgeneric test-mc65-8 (x &optional y z)
    (:method-combination mc65 "xyz")
    (:method (x &optional y z) (list x y z)))
  (list (test-mc65-8 'a) (test-mc65-8 'a 'b) (test-mc65-8 'a 'b 'c)))
(("xyz" "def" RESULT A NIL () (A NIL NIL))
 ("xyz" "def" RESULT A NIL () (A B NIL))
 ("xyz" "def" RESULT A NIL () (A B C)))

(progn
  (defgeneric test-mc65-9 (x y &optional z u)
    (:method-combination mc65 "xyz")
    (:method (x y &optional z u) (list x y z u)))
  (list (test-mc65-9 'a 'b) (test-mc65-9 'a 'b 'c) (test-mc65-9 'a 'b 'c 'd)))
(("xyz" "def" RESULT A B () (A B NIL NIL))
 ("xyz" "def" RESULT A B () (A B C NIL))
 ("xyz" "def" RESULT A B () (A B C D)))

(progn
  (defgeneric test-mc65-10 (x y z &optional u v)
    (:method-combination mc65 "xyz")
    (:method (x y z &optional u v) (list x y z u v)))
  (list (test-mc65-10 'a 'b 'c) (test-mc65-10 'a 'b 'c 'd) (test-mc65-10 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A B () (A B C NIL NIL))
 ("xyz" "def" RESULT A B () (A B C D NIL))
 ("xyz" "def" RESULT A B () (A B C D E)))

(progn
  (defgeneric test-mc65-11 (x &optional y z u)
    (:method-combination mc65 "xyz")
    (:method (x &optional y z u) (list x y z u)))
  (list (test-mc65-11 'a) (test-mc65-11 'a 'b) (test-mc65-11 'a 'b 'c) (test-mc65-11 'a 'b 'c 'd)))
(("xyz" "def" RESULT A NIL () (A NIL NIL NIL))
 ("xyz" "def" RESULT A NIL () (A B NIL NIL))
 ("xyz" "def" RESULT A NIL () (A B C NIL))
 ("xyz" "def" RESULT A NIL () (A B C D)))

(progn
  (defgeneric test-mc65-12 (x y &optional z u v)
    (:method-combination mc65 "xyz")
    (:method (x y &optional z u v) (list x y z u v)))
  (list (test-mc65-12 'a 'b) (test-mc65-12 'a 'b 'c) (test-mc65-12 'a 'b 'c 'd) (test-mc65-12 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A B () (A B NIL NIL NIL))
 ("xyz" "def" RESULT A B () (A B C NIL NIL))
 ("xyz" "def" RESULT A B () (A B C D NIL))
 ("xyz" "def" RESULT A B () (A B C D E)))

(progn
  (defgeneric test-mc65-13 (x y z &optional u v w)
    (:method-combination mc65 "xyz")
    (:method (x y z &optional u v w) (list x y z u v w)))
  (list (test-mc65-13 'a 'b 'c) (test-mc65-13 'a 'b 'c 'd) (test-mc65-13 'a 'b 'c 'd 'e) (test-mc65-13 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT A B () (A B C NIL NIL NIL))
 ("xyz" "def" RESULT A B () (A B C D NIL NIL))
 ("xyz" "def" RESULT A B () (A B C D E NIL))
 ("xyz" "def" RESULT A B () (A B C D E F)))

(progn
  (defgeneric test-mc65-14 (x &rest y)
    (:method-combination mc65 "xyz")
    (:method (x &rest y) (list* x y)))
  (list (test-mc65-14 'a) (test-mc65-14 'a 'b) (test-mc65-14 'a 'b 'c)))
(("xyz" "def" RESULT A NIL () (A))
 ("xyz" "def" RESULT A NIL (B) (A B))
 ("xyz" "def" RESULT A NIL (B C) (A B C)))

(progn
  (defgeneric test-mc65-15 (x y &rest z)
    (:method-combination mc65 "xyz")
    (:method (x y &rest z) (list* x y z)))
  (list (test-mc65-15 'a 'b) (test-mc65-15 'a 'b 'c) (test-mc65-15 'a 'b 'c 'd)))
(("xyz" "def" RESULT A B () (A B))
 ("xyz" "def" RESULT A B (C) (A B C))
 ("xyz" "def" RESULT A B (C D) (A B C D)))

(progn
  (defgeneric test-mc65-16 (x y z &rest u)
    (:method-combination mc65 "xyz")
    (:method (x y z &rest u) (list* x y z u)))
  (list (test-mc65-16 'a 'b 'c) (test-mc65-16 'a 'b 'c 'd) (test-mc65-16 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A B () (A B C))
 ("xyz" "def" RESULT A B (D) (A B C D))
 ("xyz" "def" RESULT A B (D E) (A B C D E)))

(progn
  (defgeneric test-mc65-17 (x &optional y &rest z)
    (:method-combination mc65 "xyz")
    (:method (x &optional y &rest z) (list* x y z)))
  (list (test-mc65-17 'a) (test-mc65-17 'a 'b) (test-mc65-17 'a 'b 'c) (test-mc65-17 'a 'b 'c 'd)))
(("xyz" "def" RESULT A NIL () (A NIL))
 ("xyz" "def" RESULT A NIL () (A B))
 ("xyz" "def" RESULT A NIL (C) (A B C))
 ("xyz" "def" RESULT A NIL (C D) (A B C D)))

(progn
  (defgeneric test-mc65-18 (x &optional y z &rest u)
    (:method-combination mc65 "xyz")
    (:method (x &optional y z &rest u) (list* x y z u)))
  (list (test-mc65-18 'a) (test-mc65-18 'a 'b) (test-mc65-18 'a 'b 'c) (test-mc65-18 'a 'b 'c 'd) (test-mc65-18 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A NIL () (A NIL NIL))
 ("xyz" "def" RESULT A NIL () (A B NIL))
 ("xyz" "def" RESULT A NIL () (A B C))
 ("xyz" "def" RESULT A NIL (D) (A B C D))
 ("xyz" "def" RESULT A NIL (D E) (A B C D E)))

(progn
  (defgeneric test-mc65-19 (x &optional y z u &rest v)
    (:method-combination mc65 "xyz")
    (:method (x &optional y z u &rest v) (list* x y z u v)))
  (list (test-mc65-19 'a) (test-mc65-19 'a 'b) (test-mc65-19 'a 'b 'c) (test-mc65-19 'a 'b 'c 'd) (test-mc65-19 'a 'b 'c 'd 'e) (test-mc65-19 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT A NIL () (A NIL NIL NIL))
 ("xyz" "def" RESULT A NIL () (A B NIL NIL))
 ("xyz" "def" RESULT A NIL () (A B C NIL))
 ("xyz" "def" RESULT A NIL () (A B C D))
 ("xyz" "def" RESULT A NIL (E) (A B C D E))
 ("xyz" "def" RESULT A NIL (E F) (A B C D E F)))

; Check :arguments with optional and rest arguments.
(define-method-combination mc66 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments &optional (o1 'def1) (o2 'def2) &rest r)
  `(LIST ',opt1 ',opt2 'RESULT ,o1 ,o2 ,r (CALL-METHOD ,(first all))))
MC66

(progn
  (defgeneric test-mc66-1 ()
    (:method-combination mc66 "xyz")
    (:method () '()))
  (test-mc66-1))
("xyz" "def" RESULT DEF1 DEF2 () ())

(progn
  (defgeneric test-mc66-2 (x)
    (:method-combination mc66 "xyz")
    (:method (x) (list x)))
  (test-mc66-2 'a))
("xyz" "def" RESULT DEF1 DEF2 () (A))

(progn
  (defgeneric test-mc66-3 (x y)
    (:method-combination mc66 "xyz")
    (:method (x y) (list x y)))
  (test-mc66-3 'a 'b))
("xyz" "def" RESULT DEF1 DEF2 () (A B))

(progn
  (defgeneric test-mc66-4 (x y z)
    (:method-combination mc66 "xyz")
    (:method (x y z) (list x y z)))
  (test-mc66-4 'a 'b 'c))
("xyz" "def" RESULT DEF1 DEF2 () (A B C))

(progn
  (defgeneric test-mc66-5 (x &optional y)
    (:method-combination mc66 "xyz")
    (:method (x &optional y) (list x y)))
  (list (test-mc66-5 'a) (test-mc66-5 'a 'b)))
(("xyz" "def" RESULT DEF1 DEF2 () (A NIL))
 ("xyz" "def" RESULT B DEF2 () (A B)))

(progn
  (defgeneric test-mc66-6 (x y &optional z)
    (:method-combination mc66 "xyz")
    (:method (x y &optional z) (list x y z)))
  (list (test-mc66-6 'a 'b) (test-mc66-6 'a 'b 'c)))
(("xyz" "def" RESULT DEF1 DEF2 () (A B NIL))
 ("xyz" "def" RESULT C DEF2 () (A B C)))

(progn
  (defgeneric test-mc66-7 (x y z &optional u)
    (:method-combination mc66 "xyz")
    (:method (x y z &optional u) (list x y z u)))
  (list (test-mc66-7 'a 'b 'c) (test-mc66-7 'a 'b 'c 'd)))
(("xyz" "def" RESULT DEF1 DEF2 () (A B C NIL))
 ("xyz" "def" RESULT D DEF2 () (A B C D)))

(progn
  (defgeneric test-mc66-8 (x &optional y z)
    (:method-combination mc66 "xyz")
    (:method (x &optional y z) (list x y z)))
  (list (test-mc66-8 'a) (test-mc66-8 'a 'b) (test-mc66-8 'a 'b 'c)))
(("xyz" "def" RESULT DEF1 DEF2 () (A NIL NIL))
 ("xyz" "def" RESULT B DEF2 () (A B NIL))
 ("xyz" "def" RESULT B C () (A B C)))

(progn
  (defgeneric test-mc66-9 (x y &optional z u)
    (:method-combination mc66 "xyz")
    (:method (x y &optional z u) (list x y z u)))
  (list (test-mc66-9 'a 'b) (test-mc66-9 'a 'b 'c) (test-mc66-9 'a 'b 'c 'd)))
(("xyz" "def" RESULT DEF1 DEF2 () (A B NIL NIL))
 ("xyz" "def" RESULT C DEF2 () (A B C NIL))
 ("xyz" "def" RESULT C D () (A B C D)))

(progn
  (defgeneric test-mc66-10 (x y z &optional u v)
    (:method-combination mc66 "xyz")
    (:method (x y z &optional u v) (list x y z u v)))
  (list (test-mc66-10 'a 'b 'c) (test-mc66-10 'a 'b 'c 'd) (test-mc66-10 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT DEF1 DEF2 () (A B C NIL NIL))
 ("xyz" "def" RESULT D DEF2 () (A B C D NIL))
 ("xyz" "def" RESULT D E () (A B C D E)))

(progn
  (defgeneric test-mc66-11 (x &optional y z u)
    (:method-combination mc66 "xyz")
    (:method (x &optional y z u) (list x y z u)))
  (list (test-mc66-11 'a) (test-mc66-11 'a 'b) (test-mc66-11 'a 'b 'c) (test-mc66-11 'a 'b 'c 'd)))
(("xyz" "def" RESULT DEF1 DEF2 () (A NIL NIL NIL))
 ("xyz" "def" RESULT B DEF2 () (A B NIL NIL))
 ("xyz" "def" RESULT B C () (A B C NIL))
 ("xyz" "def" RESULT B C () (A B C D)))

(progn
  (defgeneric test-mc66-12 (x y &optional z u v)
    (:method-combination mc66 "xyz")
    (:method (x y &optional z u v) (list x y z u v)))
  (list (test-mc66-12 'a 'b) (test-mc66-12 'a 'b 'c) (test-mc66-12 'a 'b 'c 'd) (test-mc66-12 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT DEF1 DEF2 () (A B NIL NIL NIL))
 ("xyz" "def" RESULT C DEF2 () (A B C NIL NIL))
 ("xyz" "def" RESULT C D () (A B C D NIL))
 ("xyz" "def" RESULT C D () (A B C D E)))

(progn
  (defgeneric test-mc66-13 (x y z &optional u v w)
    (:method-combination mc66 "xyz")
    (:method (x y z &optional u v w) (list x y z u v w)))
  (list (test-mc66-13 'a 'b 'c) (test-mc66-13 'a 'b 'c 'd) (test-mc66-13 'a 'b 'c 'd 'e) (test-mc66-13 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT DEF1 DEF2 () (A B C NIL NIL NIL))
 ("xyz" "def" RESULT D DEF2 () (A B C D NIL NIL))
 ("xyz" "def" RESULT D E () (A B C D E NIL))
 ("xyz" "def" RESULT D E () (A B C D E F)))

(progn
  (defgeneric test-mc66-14 (x &rest y)
    (:method-combination mc66 "xyz")
    (:method (x &rest y) (list* x y)))
  (list (test-mc66-14 'a) (test-mc66-14 'a 'b) (test-mc66-14 'a 'b 'c)))
(("xyz" "def" RESULT DEF1 DEF2 () (A))
 ("xyz" "def" RESULT DEF1 DEF2 (B) (A B))
 ("xyz" "def" RESULT DEF1 DEF2 (B C) (A B C)))

(progn
  (defgeneric test-mc66-15 (x y &rest z)
    (:method-combination mc66 "xyz")
    (:method (x y &rest z) (list* x y z)))
  (list (test-mc66-15 'a 'b) (test-mc66-15 'a 'b 'c) (test-mc66-15 'a 'b 'c 'd)))
(("xyz" "def" RESULT DEF1 DEF2 () (A B))
 ("xyz" "def" RESULT DEF1 DEF2 (C) (A B C))
 ("xyz" "def" RESULT DEF1 DEF2 (C D) (A B C D)))

(progn
  (defgeneric test-mc66-16 (x y z &rest u)
    (:method-combination mc66 "xyz")
    (:method (x y z &rest u) (list* x y z u)))
  (list (test-mc66-16 'a 'b 'c) (test-mc66-16 'a 'b 'c 'd) (test-mc66-16 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT DEF1 DEF2 () (A B C))
 ("xyz" "def" RESULT DEF1 DEF2 (D) (A B C D))
 ("xyz" "def" RESULT DEF1 DEF2 (D E) (A B C D E)))

(progn
  (defgeneric test-mc66-17 (x &optional y &rest z)
    (:method-combination mc66 "xyz")
    (:method (x &optional y &rest z) (list* x y z)))
  (list (test-mc66-17 'a) (test-mc66-17 'a 'b) (test-mc66-17 'a 'b 'c) (test-mc66-17 'a 'b 'c 'd)))
(("xyz" "def" RESULT DEF1 DEF2 () (A NIL))
 ("xyz" "def" RESULT B DEF2 () (A B))
 ("xyz" "def" RESULT B DEF2 (C) (A B C))
 ("xyz" "def" RESULT B DEF2 (C D) (A B C D)))

(progn
  (defgeneric test-mc66-18 (x &optional y z &rest u)
    (:method-combination mc66 "xyz")
    (:method (x &optional y z &rest u) (list* x y z u)))
  (list (test-mc66-18 'a) (test-mc66-18 'a 'b) (test-mc66-18 'a 'b 'c) (test-mc66-18 'a 'b 'c 'd) (test-mc66-18 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT DEF1 DEF2 () (A NIL NIL))
 ("xyz" "def" RESULT B DEF2 () (A B NIL))
 ("xyz" "def" RESULT B C () (A B C))
 ("xyz" "def" RESULT B C (D) (A B C D))
 ("xyz" "def" RESULT B C (D E) (A B C D E)))

(progn
  (defgeneric test-mc66-19 (x &optional y z u &rest v)
    (:method-combination mc66 "xyz")
    (:method (x &optional y z u &rest v) (list* x y z u v)))
  (list (test-mc66-19 'a) (test-mc66-19 'a 'b) (test-mc66-19 'a 'b 'c) (test-mc66-19 'a 'b 'c 'd) (test-mc66-19 'a 'b 'c 'd 'e) (test-mc66-19 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT DEF1 DEF2 () (A NIL NIL NIL))
 ("xyz" "def" RESULT B DEF2 () (A B NIL NIL))
 ("xyz" "def" RESULT B C () (A B C NIL))
 ("xyz" "def" RESULT B C () (A B C D))
 ("xyz" "def" RESULT B C (E) (A B C D E))
 ("xyz" "def" RESULT B C (E F) (A B C D E F)))

; Check :arguments with required, optional and rest arguments.
(define-method-combination mc67 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments a1 a2 &optional (o1 'def1) (o2 'def2) &rest r)
  `(LIST ',opt1 ',opt2 'RESULT ,a1 ,a2 ,o1 ,o2 ,r (CALL-METHOD ,(first all))))
MC67

(progn
  (defgeneric test-mc67-1 ()
    (:method-combination mc67 "xyz")
    (:method () '()))
  (test-mc67-1))
("xyz" "def" RESULT NIL NIL DEF1 DEF2 () ())

(progn
  (defgeneric test-mc67-2 (x)
    (:method-combination mc67 "xyz")
    (:method (x) (list x)))
  (test-mc67-2 'a))
("xyz" "def" RESULT A NIL DEF1 DEF2 () (A))

(progn
  (defgeneric test-mc67-3 (x y)
    (:method-combination mc67 "xyz")
    (:method (x y) (list x y)))
  (test-mc67-3 'a 'b))
("xyz" "def" RESULT A B DEF1 DEF2 () (A B))

(progn
  (defgeneric test-mc67-4 (x y z)
    (:method-combination mc67 "xyz")
    (:method (x y z) (list x y z)))
  (test-mc67-4 'a 'b 'c))
("xyz" "def" RESULT A B DEF1 DEF2 () (A B C))

(progn
  (defgeneric test-mc67-5 (x &optional y)
    (:method-combination mc67 "xyz")
    (:method (x &optional y) (list x y)))
  (list (test-mc67-5 'a) (test-mc67-5 'a 'b)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 () (A NIL))
 ("xyz" "def" RESULT A NIL B DEF2 () (A B)))

(progn
  (defgeneric test-mc67-6 (x y &optional z)
    (:method-combination mc67 "xyz")
    (:method (x y &optional z) (list x y z)))
  (list (test-mc67-6 'a 'b) (test-mc67-6 'a 'b 'c)))
(("xyz" "def" RESULT A B DEF1 DEF2 () (A B NIL))
 ("xyz" "def" RESULT A B C DEF2 () (A B C)))

(progn
  (defgeneric test-mc67-7 (x y z &optional u)
    (:method-combination mc67 "xyz")
    (:method (x y z &optional u) (list x y z u)))
  (list (test-mc67-7 'a 'b 'c) (test-mc67-7 'a 'b 'c 'd)))
(("xyz" "def" RESULT A B DEF1 DEF2 () (A B C NIL))
 ("xyz" "def" RESULT A B D DEF2 () (A B C D)))

(progn
  (defgeneric test-mc67-8 (x &optional y z)
    (:method-combination mc67 "xyz")
    (:method (x &optional y z) (list x y z)))
  (list (test-mc67-8 'a) (test-mc67-8 'a 'b) (test-mc67-8 'a 'b 'c)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 () (A NIL NIL))
 ("xyz" "def" RESULT A NIL B DEF2 () (A B NIL))
 ("xyz" "def" RESULT A NIL B C () (A B C)))

(progn
  (defgeneric test-mc67-9 (x y &optional z u)
    (:method-combination mc67 "xyz")
    (:method (x y &optional z u) (list x y z u)))
  (list (test-mc67-9 'a 'b) (test-mc67-9 'a 'b 'c) (test-mc67-9 'a 'b 'c 'd)))
(("xyz" "def" RESULT A B DEF1 DEF2 () (A B NIL NIL))
 ("xyz" "def" RESULT A B C DEF2 () (A B C NIL))
 ("xyz" "def" RESULT A B C D () (A B C D)))

(progn
  (defgeneric test-mc67-10 (x y z &optional u v)
    (:method-combination mc67 "xyz")
    (:method (x y z &optional u v) (list x y z u v)))
  (list (test-mc67-10 'a 'b 'c) (test-mc67-10 'a 'b 'c 'd) (test-mc67-10 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A B DEF1 DEF2 () (A B C NIL NIL))
 ("xyz" "def" RESULT A B D DEF2 () (A B C D NIL))
 ("xyz" "def" RESULT A B D E () (A B C D E)))

(progn
  (defgeneric test-mc67-11 (x &optional y z u)
    (:method-combination mc67 "xyz")
    (:method (x &optional y z u) (list x y z u)))
  (list (test-mc67-11 'a) (test-mc67-11 'a 'b) (test-mc67-11 'a 'b 'c) (test-mc67-11 'a 'b 'c 'd)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 () (A NIL NIL NIL))
 ("xyz" "def" RESULT A NIL B DEF2 () (A B NIL NIL))
 ("xyz" "def" RESULT A NIL B C () (A B C NIL))
 ("xyz" "def" RESULT A NIL B C () (A B C D)))

(progn
  (defgeneric test-mc67-12 (x y &optional z u v)
    (:method-combination mc67 "xyz")
    (:method (x y &optional z u v) (list x y z u v)))
  (list (test-mc67-12 'a 'b) (test-mc67-12 'a 'b 'c) (test-mc67-12 'a 'b 'c 'd) (test-mc67-12 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A B DEF1 DEF2 () (A B NIL NIL NIL))
 ("xyz" "def" RESULT A B C DEF2 () (A B C NIL NIL))
 ("xyz" "def" RESULT A B C D () (A B C D NIL))
 ("xyz" "def" RESULT A B C D () (A B C D E)))

(progn
  (defgeneric test-mc67-13 (x y z &optional u v w)
    (:method-combination mc67 "xyz")
    (:method (x y z &optional u v w) (list x y z u v w)))
  (list (test-mc67-13 'a 'b 'c) (test-mc67-13 'a 'b 'c 'd) (test-mc67-13 'a 'b 'c 'd 'e) (test-mc67-13 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT A B DEF1 DEF2 () (A B C NIL NIL NIL))
 ("xyz" "def" RESULT A B D DEF2 () (A B C D NIL NIL))
 ("xyz" "def" RESULT A B D E () (A B C D E NIL))
 ("xyz" "def" RESULT A B D E () (A B C D E F)))

(progn
  (defgeneric test-mc67-14 (x &rest y)
    (:method-combination mc67 "xyz")
    (:method (x &rest y) (list* x y)))
  (list (test-mc67-14 'a) (test-mc67-14 'a 'b) (test-mc67-14 'a 'b 'c)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 () (A))
 ("xyz" "def" RESULT A NIL DEF1 DEF2 (B) (A B))
 ("xyz" "def" RESULT A NIL DEF1 DEF2 (B C) (A B C)))

(progn
  (defgeneric test-mc67-15 (x y &rest z)
    (:method-combination mc67 "xyz")
    (:method (x y &rest z) (list* x y z)))
  (list (test-mc67-15 'a 'b) (test-mc67-15 'a 'b 'c) (test-mc67-15 'a 'b 'c 'd)))
(("xyz" "def" RESULT A B DEF1 DEF2 () (A B))
 ("xyz" "def" RESULT A B DEF1 DEF2 (C) (A B C))
 ("xyz" "def" RESULT A B DEF1 DEF2 (C D) (A B C D)))

(progn
  (defgeneric test-mc67-16 (x y z &rest u)
    (:method-combination mc67 "xyz")
    (:method (x y z &rest u) (list* x y z u)))
  (list (test-mc67-16 'a 'b 'c) (test-mc67-16 'a 'b 'c 'd) (test-mc67-16 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A B DEF1 DEF2 () (A B C))
 ("xyz" "def" RESULT A B DEF1 DEF2 (D) (A B C D))
 ("xyz" "def" RESULT A B DEF1 DEF2 (D E) (A B C D E)))

(progn
  (defgeneric test-mc67-17 (x &optional y &rest z)
    (:method-combination mc67 "xyz")
    (:method (x &optional y &rest z) (list* x y z)))
  (list (test-mc67-17 'a) (test-mc67-17 'a 'b) (test-mc67-17 'a 'b 'c) (test-mc67-17 'a 'b 'c 'd)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 () (A NIL))
 ("xyz" "def" RESULT A NIL B DEF2 () (A B))
 ("xyz" "def" RESULT A NIL B DEF2 (C) (A B C))
 ("xyz" "def" RESULT A NIL B DEF2 (C D) (A B C D)))

(progn
  (defgeneric test-mc67-18 (x &optional y z &rest u)
    (:method-combination mc67 "xyz")
    (:method (x &optional y z &rest u) (list* x y z u)))
  (list (test-mc67-18 'a) (test-mc67-18 'a 'b) (test-mc67-18 'a 'b 'c) (test-mc67-18 'a 'b 'c 'd) (test-mc67-18 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 () (A NIL NIL))
 ("xyz" "def" RESULT A NIL B DEF2 () (A B NIL))
 ("xyz" "def" RESULT A NIL B C () (A B C))
 ("xyz" "def" RESULT A NIL B C (D) (A B C D))
 ("xyz" "def" RESULT A NIL B C (D E) (A B C D E)))

(progn
  (defgeneric test-mc67-19 (x &optional y z u &rest v)
    (:method-combination mc67 "xyz")
    (:method (x &optional y z u &rest v) (list* x y z u v)))
  (list (test-mc67-19 'a) (test-mc67-19 'a 'b) (test-mc67-19 'a 'b 'c) (test-mc67-19 'a 'b 'c 'd) (test-mc67-19 'a 'b 'c 'd 'e) (test-mc67-19 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 () (A NIL NIL NIL))
 ("xyz" "def" RESULT A NIL B DEF2 () (A B NIL NIL))
 ("xyz" "def" RESULT A NIL B C () (A B C NIL))
 ("xyz" "def" RESULT A NIL B C () (A B C D))
 ("xyz" "def" RESULT A NIL B C (E) (A B C D E))
 ("xyz" "def" RESULT A NIL B C (E F) (A B C D E F)))

; Check :arguments with required, optional and key arguments.
(define-method-combination mc68 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments a1 a2 &optional (o1 'def1) (o2 'def2) &key (test 'EQ) (test-not 'NEQ))
  `(LIST ',opt1 ',opt2 'RESULT ,a1 ,a2 ,o1 ,o2 ,test ,test-not (CALL-METHOD ,(first all))))
MC68

(progn
  (defgeneric test-mc68-1 (x &optional y)
    (:method-combination mc68 "xyz")
    (:method (x &optional y) (list x y)))
  (list (test-mc68-1 'a) (test-mc68-1 'a 'b)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 EQ NEQ (A NIL))
 ("xyz" "def" RESULT A NIL B DEF2 EQ NEQ (A B)))

(progn
  (defgeneric test-mc68-2 (x y z &optional u v w)
    (:method-combination mc68 "xyz")
    (:method (x y z &optional u v w) (list x y z u v w)))
  (list (test-mc68-2 'a 'b 'c) (test-mc68-2 'a 'b 'c 'd) (test-mc68-2 'a 'b 'c 'd 'e) (test-mc68-2 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT A B DEF1 DEF2 EQ NEQ (A B C NIL NIL NIL))
 ("xyz" "def" RESULT A B D DEF2 EQ NEQ (A B C D NIL NIL))
 ("xyz" "def" RESULT A B D E EQ NEQ (A B C D E NIL))
 ("xyz" "def" RESULT A B D E EQ NEQ (A B C D E F)))

(progn
  (defgeneric test-mc68-3 (x &rest y)
    (:method-combination mc68 "xyz")
    (:method (x &rest y) (list* x y)))
  (list (test-mc68-3 'a) (test-mc68-3 'a 'b 'c)
        (test-mc68-3 'a :test-not 'nequal :test 'eql :test-not 'nequalp)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 EQ NEQ (A))
 ("xyz" "def" RESULT A NIL DEF1 DEF2 EQ NEQ (A B C))
 ("xyz" "def" RESULT A NIL DEF1 DEF2 EQL NEQUAL (A :TEST-NOT NEQUAL :TEST EQL :TEST-NOT NEQUALP)))

(progn
  (defgeneric test-mc68-4 (x &rest y)
    (:method-combination mc68 "xyz")
    (:method (x &rest y) (list* x y)))
  (test-mc68-4 'a 'b))
ERROR

(progn
  (defgeneric test-mc68-5 (x y z &rest u)
    (:method-combination mc68 "xyz")
    (:method (x y z &rest u) (list* x y z u)))
  (list (test-mc68-5 'a :test 'eq) (test-mc68-5 'a :test 'eq 'd 'e)
        (test-mc68-5 'a :test 'eq :test-not 'nequal :test 'eql :test-not 'nequalp)))
(("xyz" "def" RESULT A :TEST DEF1 DEF2 EQ NEQ (A :TEST EQ))
 ("xyz" "def" RESULT A :TEST DEF1 DEF2 EQ NEQ (A :TEST EQ D E))
 ("xyz" "def" RESULT A :TEST DEF1 DEF2 EQL NEQUAL (A :TEST EQ :TEST-NOT NEQUAL :TEST EQL :TEST-NOT NEQUALP)))

(progn
  (defgeneric test-mc68-6 (x &optional y z u &rest v)
    (:method-combination mc68 "xyz")
    (:method (x &optional y z u &rest v) (list* x y z u v)))
  (list (test-mc68-6 'a) (test-mc68-6 'a 'b 'c)
        (test-mc68-6 'a :test 'eq 'd :test-not 'nequal :test 'eql :test-not 'nequalp)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 EQ NEQ (A NIL NIL NIL))
 ("xyz" "def" RESULT A NIL B C EQ NEQ (A B C NIL))
 ("xyz" "def" RESULT A NIL :TEST EQ EQL NEQUAL (A :TEST EQ D :TEST-NOT NEQUAL :TEST EQL :TEST-NOT NEQUALP)))

; Check :arguments with just &whole.
(define-method-combination mc69 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments &whole whole)
  `(LIST ',opt1 ',opt2 'RESULT ,whole (CALL-METHOD ,(first all))))
MC69

(progn
  (defgeneric test-mc69-1 ()
    (:method-combination mc69 "xyz")
    (:method () '()))
  (test-mc69-1))
("xyz" "def" RESULT () ())

(progn
  (defgeneric test-mc69-2 (x)
    (:method-combination mc69 "xyz")
    (:method (x) (list x)))
  (test-mc69-2 'a))
("xyz" "def" RESULT (A) (A))

(progn
  (defgeneric test-mc69-3 (x y)
    (:method-combination mc69 "xyz")
    (:method (x y) (list x y)))
  (test-mc69-3 'a 'b))
("xyz" "def" RESULT (A B) (A B))

(progn
  (defgeneric test-mc69-4 (x y z)
    (:method-combination mc69 "xyz")
    (:method (x y z) (list x y z)))
  (test-mc69-4 'a 'b 'c))
("xyz" "def" RESULT (A B C) (A B C))

(progn
  (defgeneric test-mc69-5 (x &optional y)
    (:method-combination mc69 "xyz")
    (:method (x &optional y) (list x y)))
  (list (test-mc69-5 'a) (test-mc69-5 'a 'b)))
(("xyz" "def" RESULT (A) (A NIL))
 ("xyz" "def" RESULT (A B) (A B)))

(progn
  (defgeneric test-mc69-6 (x y &optional z)
    (:method-combination mc69 "xyz")
    (:method (x y &optional z) (list x y z)))
  (list (test-mc69-6 'a 'b) (test-mc69-6 'a 'b 'c)))
(("xyz" "def" RESULT (A B) (A B NIL))
 ("xyz" "def" RESULT (A B C) (A B C)))

(progn
  (defgeneric test-mc69-7 (x y z &optional u)
    (:method-combination mc69 "xyz")
    (:method (x y z &optional u) (list x y z u)))
  (list (test-mc69-7 'a 'b 'c) (test-mc69-7 'a 'b 'c 'd)))
(("xyz" "def" RESULT (A B C) (A B C NIL))
 ("xyz" "def" RESULT (A B C D) (A B C D)))

(progn
  (defgeneric test-mc69-8 (x &optional y z)
    (:method-combination mc69 "xyz")
    (:method (x &optional y z) (list x y z)))
  (list (test-mc69-8 'a) (test-mc69-8 'a 'b) (test-mc69-8 'a 'b 'c)))
(("xyz" "def" RESULT (A) (A NIL NIL))
 ("xyz" "def" RESULT (A B) (A B NIL))
 ("xyz" "def" RESULT (A B C) (A B C)))

(progn
  (defgeneric test-mc69-9 (x y &optional z u)
    (:method-combination mc69 "xyz")
    (:method (x y &optional z u) (list x y z u)))
  (list (test-mc69-9 'a 'b) (test-mc69-9 'a 'b 'c) (test-mc69-9 'a 'b 'c 'd)))
(("xyz" "def" RESULT (A B) (A B NIL NIL))
 ("xyz" "def" RESULT (A B C) (A B C NIL))
 ("xyz" "def" RESULT (A B C D) (A B C D)))

(progn
  (defgeneric test-mc69-10 (x y z &optional u v)
    (:method-combination mc69 "xyz")
    (:method (x y z &optional u v) (list x y z u v)))
  (list (test-mc69-10 'a 'b 'c) (test-mc69-10 'a 'b 'c 'd) (test-mc69-10 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT (A B C) (A B C NIL NIL))
 ("xyz" "def" RESULT (A B C D) (A B C D NIL))
 ("xyz" "def" RESULT (A B C D E) (A B C D E)))

(progn
  (defgeneric test-mc69-11 (x &optional y z u)
    (:method-combination mc69 "xyz")
    (:method (x &optional y z u) (list x y z u)))
  (list (test-mc69-11 'a) (test-mc69-11 'a 'b) (test-mc69-11 'a 'b 'c) (test-mc69-11 'a 'b 'c 'd)))
(("xyz" "def" RESULT (A) (A NIL NIL NIL))
 ("xyz" "def" RESULT (A B) (A B NIL NIL))
 ("xyz" "def" RESULT (A B C) (A B C NIL))
 ("xyz" "def" RESULT (A B C D) (A B C D)))

(progn
  (defgeneric test-mc69-12 (x y &optional z u v)
    (:method-combination mc69 "xyz")
    (:method (x y &optional z u v) (list x y z u v)))
  (list (test-mc69-12 'a 'b) (test-mc69-12 'a 'b 'c) (test-mc69-12 'a 'b 'c 'd) (test-mc69-12 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT (A B) (A B NIL NIL NIL))
 ("xyz" "def" RESULT (A B C) (A B C NIL NIL))
 ("xyz" "def" RESULT (A B C D) (A B C D NIL))
 ("xyz" "def" RESULT (A B C D E) (A B C D E)))

(progn
  (defgeneric test-mc69-13 (x y z &optional u v w)
    (:method-combination mc69 "xyz")
    (:method (x y z &optional u v w) (list x y z u v w)))
  (list (test-mc69-13 'a 'b 'c) (test-mc69-13 'a 'b 'c 'd) (test-mc69-13 'a 'b 'c 'd 'e) (test-mc69-13 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT (A B C) (A B C NIL NIL NIL))
 ("xyz" "def" RESULT (A B C D) (A B C D NIL NIL))
 ("xyz" "def" RESULT (A B C D E) (A B C D E NIL))
 ("xyz" "def" RESULT (A B C D E F) (A B C D E F)))

(progn
  (defgeneric test-mc69-14 (x &rest y)
    (:method-combination mc69 "xyz")
    (:method (x &rest y) (list* x y)))
  (list (test-mc69-14 'a) (test-mc69-14 'a 'b) (test-mc69-14 'a 'b 'c)))
(("xyz" "def" RESULT (A) (A))
 ("xyz" "def" RESULT (A B) (A B))
 ("xyz" "def" RESULT (A B C) (A B C)))

(progn
  (defgeneric test-mc69-15 (x y &rest z)
    (:method-combination mc69 "xyz")
    (:method (x y &rest z) (list* x y z)))
  (list (test-mc69-15 'a 'b) (test-mc69-15 'a 'b 'c) (test-mc69-15 'a 'b 'c 'd)))
(("xyz" "def" RESULT (A B) (A B))
 ("xyz" "def" RESULT (A B C) (A B C))
 ("xyz" "def" RESULT (A B C D) (A B C D)))

(progn
  (defgeneric test-mc69-16 (x y z &rest u)
    (:method-combination mc69 "xyz")
    (:method (x y z &rest u) (list* x y z u)))
  (list (test-mc69-16 'a 'b 'c) (test-mc69-16 'a 'b 'c 'd) (test-mc69-16 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT (A B C) (A B C))
 ("xyz" "def" RESULT (A B C D) (A B C D))
 ("xyz" "def" RESULT (A B C D E) (A B C D E)))

(progn
  (defgeneric test-mc69-17 (x &optional y &rest z)
    (:method-combination mc69 "xyz")
    (:method (x &optional y &rest z) (list* x y z)))
  (list (test-mc69-17 'a) (test-mc69-17 'a 'b) (test-mc69-17 'a 'b 'c) (test-mc69-17 'a 'b 'c 'd)))
(("xyz" "def" RESULT (A) (A NIL))
 ("xyz" "def" RESULT (A B) (A B))
 ("xyz" "def" RESULT (A B C) (A B C))
 ("xyz" "def" RESULT (A B C D) (A B C D)))

(progn
  (defgeneric test-mc69-18 (x &optional y z &rest u)
    (:method-combination mc69 "xyz")
    (:method (x &optional y z &rest u) (list* x y z u)))
  (list (test-mc69-18 'a) (test-mc69-18 'a 'b) (test-mc69-18 'a 'b 'c) (test-mc69-18 'a 'b 'c 'd) (test-mc69-18 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT (A) (A NIL NIL))
 ("xyz" "def" RESULT (A B) (A B NIL))
 ("xyz" "def" RESULT (A B C) (A B C))
 ("xyz" "def" RESULT (A B C D) (A B C D))
 ("xyz" "def" RESULT (A B C D E) (A B C D E)))

(progn
  (defgeneric test-mc69-19 (x &optional y z u &rest v)
    (:method-combination mc69 "xyz")
    (:method (x &optional y z u &rest v) (list* x y z u v)))
  (list (test-mc69-19 'a) (test-mc69-19 'a 'b) (test-mc69-19 'a 'b 'c) (test-mc69-19 'a 'b 'c 'd) (test-mc69-19 'a 'b 'c 'd 'e) (test-mc69-19 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT (A) (A NIL NIL NIL))
 ("xyz" "def" RESULT (A B) (A B NIL NIL))
 ("xyz" "def" RESULT (A B C) (A B C NIL))
 ("xyz" "def" RESULT (A B C D) (A B C D))
 ("xyz" "def" RESULT (A B C D E) (A B C D E))
 ("xyz" "def" RESULT (A B C D E F) (A B C D E F)))

; Check :arguments with &whole and required, optional and rest arguments.
(define-method-combination mc70 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments &whole whole a1 a2 &optional (o1 'def1) (o2 'def2) &rest r)
  `(LIST ',opt1 ',opt2 'RESULT ,whole ,a1 ,a2 ,o1 ,o2 ,r (CALL-METHOD ,(first all))))
MC70

(progn
  (defgeneric test-mc70-1 ()
    (:method-combination mc70 "xyz")
    (:method () '()))
  (test-mc70-1))
("xyz" "def" RESULT () NIL NIL DEF1 DEF2 () ())

(progn
  (defgeneric test-mc70-2 (x)
    (:method-combination mc70 "xyz")
    (:method (x) (list x)))
  (test-mc70-2 'a))
("xyz" "def" RESULT (A) A NIL DEF1 DEF2 () (A))

(progn
  (defgeneric test-mc70-3 (x y)
    (:method-combination mc70 "xyz")
    (:method (x y) (list x y)))
  (test-mc70-3 'a 'b))
("xyz" "def" RESULT (A B) A B DEF1 DEF2 () (A B))

(progn
  (defgeneric test-mc70-4 (x y z)
    (:method-combination mc70 "xyz")
    (:method (x y z) (list x y z)))
  (test-mc70-4 'a 'b 'c))
("xyz" "def" RESULT (A B C) A B DEF1 DEF2 () (A B C))

(progn
  (defgeneric test-mc70-5 (x &optional y)
    (:method-combination mc70 "xyz")
    (:method (x &optional y) (list x y)))
  (list (test-mc70-5 'a) (test-mc70-5 'a 'b)))
(("xyz" "def" RESULT (A) A NIL DEF1 DEF2 () (A NIL))
 ("xyz" "def" RESULT (A B) A NIL B DEF2 () (A B)))

(progn
  (defgeneric test-mc70-6 (x y &optional z)
    (:method-combination mc70 "xyz")
    (:method (x y &optional z) (list x y z)))
  (list (test-mc70-6 'a 'b) (test-mc70-6 'a 'b 'c)))
(("xyz" "def" RESULT (A B) A B DEF1 DEF2 () (A B NIL))
 ("xyz" "def" RESULT (A B C) A B C DEF2 () (A B C)))

(progn
  (defgeneric test-mc70-7 (x y z &optional u)
    (:method-combination mc70 "xyz")
    (:method (x y z &optional u) (list x y z u)))
  (list (test-mc70-7 'a 'b 'c) (test-mc70-7 'a 'b 'c 'd)))
(("xyz" "def" RESULT (A B C) A B DEF1 DEF2 () (A B C NIL))
 ("xyz" "def" RESULT (A B C D) A B D DEF2 () (A B C D)))

(progn
  (defgeneric test-mc70-8 (x &optional y z)
    (:method-combination mc70 "xyz")
    (:method (x &optional y z) (list x y z)))
  (list (test-mc70-8 'a) (test-mc70-8 'a 'b) (test-mc70-8 'a 'b 'c)))
(("xyz" "def" RESULT (A) A NIL DEF1 DEF2 () (A NIL NIL))
 ("xyz" "def" RESULT (A B) A NIL B DEF2 () (A B NIL))
 ("xyz" "def" RESULT (A B C) A NIL B C () (A B C)))

(progn
  (defgeneric test-mc70-9 (x y &optional z u)
    (:method-combination mc70 "xyz")
    (:method (x y &optional z u) (list x y z u)))
  (list (test-mc70-9 'a 'b) (test-mc70-9 'a 'b 'c) (test-mc70-9 'a 'b 'c 'd)))
(("xyz" "def" RESULT (A B) A B DEF1 DEF2 () (A B NIL NIL))
 ("xyz" "def" RESULT (A B C) A B C DEF2 () (A B C NIL))
 ("xyz" "def" RESULT (A B C D) A B C D () (A B C D)))

(progn
  (defgeneric test-mc70-10 (x y z &optional u v)
    (:method-combination mc70 "xyz")
    (:method (x y z &optional u v) (list x y z u v)))
  (list (test-mc70-10 'a 'b 'c) (test-mc70-10 'a 'b 'c 'd) (test-mc70-10 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT (A B C) A B DEF1 DEF2 () (A B C NIL NIL))
 ("xyz" "def" RESULT (A B C D) A B D DEF2 () (A B C D NIL))
 ("xyz" "def" RESULT (A B C D E) A B D E () (A B C D E)))

(progn
  (defgeneric test-mc70-11 (x &optional y z u)
    (:method-combination mc70 "xyz")
    (:method (x &optional y z u) (list x y z u)))
  (list (test-mc70-11 'a) (test-mc70-11 'a 'b) (test-mc70-11 'a 'b 'c) (test-mc70-11 'a 'b 'c 'd)))
(("xyz" "def" RESULT (A) A NIL DEF1 DEF2 () (A NIL NIL NIL))
 ("xyz" "def" RESULT (A B) A NIL B DEF2 () (A B NIL NIL))
 ("xyz" "def" RESULT (A B C) A NIL B C () (A B C NIL))
 ("xyz" "def" RESULT (A B C D) A NIL B C () (A B C D)))

(progn
  (defgeneric test-mc70-12 (x y &optional z u v)
    (:method-combination mc70 "xyz")
    (:method (x y &optional z u v) (list x y z u v)))
  (list (test-mc70-12 'a 'b) (test-mc70-12 'a 'b 'c) (test-mc70-12 'a 'b 'c 'd) (test-mc70-12 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT (A B) A B DEF1 DEF2 () (A B NIL NIL NIL))
 ("xyz" "def" RESULT (A B C) A B C DEF2 () (A B C NIL NIL))
 ("xyz" "def" RESULT (A B C D) A B C D () (A B C D NIL))
 ("xyz" "def" RESULT (A B C D E) A B C D () (A B C D E)))

(progn
  (defgeneric test-mc70-13 (x y z &optional u v w)
    (:method-combination mc70 "xyz")
    (:method (x y z &optional u v w) (list x y z u v w)))
  (list (test-mc70-13 'a 'b 'c) (test-mc70-13 'a 'b 'c 'd) (test-mc70-13 'a 'b 'c 'd 'e) (test-mc70-13 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT (A B C) A B DEF1 DEF2 () (A B C NIL NIL NIL))
 ("xyz" "def" RESULT (A B C D) A B D DEF2 () (A B C D NIL NIL))
 ("xyz" "def" RESULT (A B C D E) A B D E () (A B C D E NIL))
 ("xyz" "def" RESULT (A B C D E F) A B D E () (A B C D E F)))

(progn
  (defgeneric test-mc70-14 (x &rest y)
    (:method-combination mc70 "xyz")
    (:method (x &rest y) (list* x y)))
  (list (test-mc70-14 'a) (test-mc70-14 'a 'b) (test-mc70-14 'a 'b 'c)))
(("xyz" "def" RESULT (A) A NIL DEF1 DEF2 () (A))
 ("xyz" "def" RESULT (A B) A NIL DEF1 DEF2 (B) (A B))
 ("xyz" "def" RESULT (A B C) A NIL DEF1 DEF2 (B C) (A B C)))

(progn
  (defgeneric test-mc70-15 (x y &rest z)
    (:method-combination mc70 "xyz")
    (:method (x y &rest z) (list* x y z)))
  (list (test-mc70-15 'a 'b) (test-mc70-15 'a 'b 'c) (test-mc70-15 'a 'b 'c 'd)))
(("xyz" "def" RESULT (A B) A B DEF1 DEF2 () (A B))
 ("xyz" "def" RESULT (A B C) A B DEF1 DEF2 (C) (A B C))
 ("xyz" "def" RESULT (A B C D) A B DEF1 DEF2 (C D) (A B C D)))

(progn
  (defgeneric test-mc70-16 (x y z &rest u)
    (:method-combination mc70 "xyz")
    (:method (x y z &rest u) (list* x y z u)))
  (list (test-mc70-16 'a 'b 'c) (test-mc70-16 'a 'b 'c 'd) (test-mc70-16 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT (A B C) A B DEF1 DEF2 () (A B C))
 ("xyz" "def" RESULT (A B C D) A B DEF1 DEF2 (D) (A B C D))
 ("xyz" "def" RESULT (A B C D E) A B DEF1 DEF2 (D E) (A B C D E)))

(progn
  (defgeneric test-mc70-17 (x &optional y &rest z)
    (:method-combination mc70 "xyz")
    (:method (x &optional y &rest z) (list* x y z)))
  (list (test-mc70-17 'a) (test-mc70-17 'a 'b) (test-mc70-17 'a 'b 'c) (test-mc70-17 'a 'b 'c 'd)))
(("xyz" "def" RESULT (A) A NIL DEF1 DEF2 () (A NIL))
 ("xyz" "def" RESULT (A B) A NIL B DEF2 () (A B))
 ("xyz" "def" RESULT (A B C) A NIL B DEF2 (C) (A B C))
 ("xyz" "def" RESULT (A B C D) A NIL B DEF2 (C D) (A B C D)))

(progn
  (defgeneric test-mc70-18 (x &optional y z &rest u)
    (:method-combination mc70 "xyz")
    (:method (x &optional y z &rest u) (list* x y z u)))
  (list (test-mc70-18 'a) (test-mc70-18 'a 'b) (test-mc70-18 'a 'b 'c) (test-mc70-18 'a 'b 'c 'd) (test-mc70-18 'a 'b 'c 'd 'e)))
(("xyz" "def" RESULT (A) A NIL DEF1 DEF2 () (A NIL NIL))
 ("xyz" "def" RESULT (A B) A NIL B DEF2 () (A B NIL))
 ("xyz" "def" RESULT (A B C) A NIL B C () (A B C))
 ("xyz" "def" RESULT (A B C D) A NIL B C (D) (A B C D))
 ("xyz" "def" RESULT (A B C D E) A NIL B C (D E) (A B C D E)))

(progn
  (defgeneric test-mc70-19 (x &optional y z u &rest v)
    (:method-combination mc70 "xyz")
    (:method (x &optional y z u &rest v) (list* x y z u v)))
  (list (test-mc70-19 'a) (test-mc70-19 'a 'b) (test-mc70-19 'a 'b 'c) (test-mc70-19 'a 'b 'c 'd) (test-mc70-19 'a 'b 'c 'd 'e) (test-mc70-19 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT (A) A NIL DEF1 DEF2 () (A NIL NIL NIL))
 ("xyz" "def" RESULT (A B) A NIL B DEF2 () (A B NIL NIL))
 ("xyz" "def" RESULT (A B C) A NIL B C () (A B C NIL))
 ("xyz" "def" RESULT (A B C D) A NIL B C () (A B C D))
 ("xyz" "def" RESULT (A B C D E) A NIL B C (E) (A B C D E))
 ("xyz" "def" RESULT (A B C D E F) A NIL B C (E F) (A B C D E F)))

; Check :arguments with only optional arguments but with svars.
(define-method-combination mc71 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments &optional (o1 'def1 os1) (o2 'def2 os2))
  `(LIST ',opt1 ',opt2 'RESULT ,o1 ,o2 ,os1 ,os2 (CALL-METHOD ,(first all))))
MC71

(progn
  (defgeneric test-mc71-1 (x)
    (:method-combination mc71 "xyz")
    (:method (x) (list x)))
  (test-mc71-1 'a))
("xyz" "def" RESULT DEF1 DEF2 NIL NIL (A))

(progn
  (defgeneric test-mc71-2 (x &optional y)
    (:method-combination mc71 "xyz")
    (:method (x &optional y) (list x y)))
  (list (test-mc71-2 'a) (test-mc71-2 'a 'b)))
(("xyz" "def" RESULT DEF1 DEF2 NIL NIL (A NIL))
 ("xyz" "def" RESULT B DEF2 T NIL (A B)))

(progn
  (defgeneric test-mc71-3 (x &optional y z)
    (:method-combination mc71 "xyz")
    (:method (x &optional y z) (list x y z)))
  (list (test-mc71-3 'a) (test-mc71-3 'a 'b) (test-mc71-3 'a 'b 'c)))
(("xyz" "def" RESULT DEF1 DEF2 NIL NIL (A NIL NIL))
 ("xyz" "def" RESULT B DEF2 T NIL (A B NIL))
 ("xyz" "def" RESULT B C T T (A B C)))

(progn
  (defgeneric test-mc71-4 (x &optional y z u)
    (:method-combination mc71 "xyz")
    (:method (x &optional y z u) (list x y z u)))
  (list (test-mc71-4 'a) (test-mc71-4 'a 'b) (test-mc71-4 'a 'b 'c) (test-mc71-4 'a 'b 'c 'd)))
(("xyz" "def" RESULT DEF1 DEF2 NIL NIL (A NIL NIL NIL))
 ("xyz" "def" RESULT B DEF2 T NIL (A B NIL NIL))
 ("xyz" "def" RESULT B C T T (A B C NIL))
 ("xyz" "def" RESULT B C T T (A B C D)))

(progn
  (defgeneric test-mc71-5 (x &rest y)
    (:method-combination mc71 "xyz")
    (:method (x &rest y) (list* x y)))
  (list (test-mc71-5 'a) (test-mc71-5 'a 'b) (test-mc71-5 'a 'b 'c)))
(("xyz" "def" RESULT DEF1 DEF2 NIL NIL (A))
 ("xyz" "def" RESULT DEF1 DEF2 NIL NIL (A B))
 ("xyz" "def" RESULT DEF1 DEF2 NIL NIL (A B C)))

(progn
  (defgeneric test-mc71-6 (x &optional y &rest z)
    (:method-combination mc71 "xyz")
    (:method (x &optional y &rest z) (list* x y z)))
  (list (test-mc71-6 'a) (test-mc71-6 'a 'b) (test-mc71-6 'a 'b 'c)))
(("xyz" "def" RESULT DEF1 DEF2 NIL NIL (A NIL))
 ("xyz" "def" RESULT B DEF2 T NIL (A B))
 ("xyz" "def" RESULT B DEF2 T NIL (A B C)))

(progn
  (defgeneric test-mc71-7 (x &optional y z &rest u)
    (:method-combination mc71 "xyz")
    (:method (x &optional y z &rest u) (list* x y z u)))
  (list (test-mc71-7 'a) (test-mc71-7 'a 'b) (test-mc71-7 'a 'b 'c) (test-mc71-7 'a 'b 'c 'd)))
(("xyz" "def" RESULT DEF1 DEF2 NIL NIL (A NIL NIL))
 ("xyz" "def" RESULT B DEF2 T NIL (A B NIL))
 ("xyz" "def" RESULT B C T T (A B C))
 ("xyz" "def" RESULT B C T T (A B C D)))

; Check :arguments with required, optional and key arguments and key-svars.
(define-method-combination mc72 (opt1 &optional (opt2 "def")) ((all *))
  (:arguments a1 a2 &optional (o1 'def1) (o2 'def2) &key (test 'EQ test-p) (test-not 'NEQ test-not-p))
  `(LIST ',opt1 ',opt2 'RESULT ,a1 ,a2 ,o1 ,o2 ,test ,test-not ,test-p ,test-not-p (CALL-METHOD ,(first all))))
MC72

(progn
  (defgeneric test-mc72-1 (x &optional y)
    (:method-combination mc72 "xyz")
    (:method (x &optional y) (list x y)))
  (list (test-mc72-1 'a) (test-mc72-1 'a 'b)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 EQ NEQ NIL NIL (A NIL))
 ("xyz" "def" RESULT A NIL B DEF2 EQ NEQ NIL NIL (A B)))

(progn
  (defgeneric test-mc72-2 (x y z &optional u v w)
    (:method-combination mc72 "xyz")
    (:method (x y z &optional u v w) (list x y z u v w)))
  (list (test-mc72-2 'a 'b 'c) (test-mc72-2 'a 'b 'c 'd) (test-mc72-2 'a 'b 'c 'd 'e) (test-mc72-2 'a 'b 'c 'd 'e 'f)))
(("xyz" "def" RESULT A B DEF1 DEF2 EQ NEQ NIL NIL (A B C NIL NIL NIL))
 ("xyz" "def" RESULT A B D DEF2 EQ NEQ NIL NIL (A B C D NIL NIL))
 ("xyz" "def" RESULT A B D E EQ NEQ NIL NIL (A B C D E NIL))
 ("xyz" "def" RESULT A B D E EQ NEQ NIL NIL (A B C D E F)))

(progn
  (defgeneric test-mc72-3 (x &rest y)
    (:method-combination mc72 "xyz")
    (:method (x &rest y) (list* x y)))
  (list (test-mc72-3 'a) (test-mc72-3 'a 'b 'c)
        (test-mc72-3 'a :test-not 'nequal)
        (test-mc72-3 'a :test 'eq :test-not 'nequal)
        (test-mc72-3 'a :test-not 'nequal :test 'eql :test-not 'nequalp)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 EQ NEQ NIL NIL (A))
 ("xyz" "def" RESULT A NIL DEF1 DEF2 EQ NEQ NIL NIL (A B C))
 ("xyz" "def" RESULT A NIL DEF1 DEF2 EQ NEQUAL NIL T (A :TEST-NOT NEQUAL))
 ("xyz" "def" RESULT A NIL DEF1 DEF2 EQ NEQUAL T T (A :TEST EQ :TEST-NOT NEQUAL))
 ("xyz" "def" RESULT A NIL DEF1 DEF2 EQL NEQUAL T T (A :TEST-NOT NEQUAL :TEST EQL :TEST-NOT NEQUALP)))

(progn
  (defgeneric test-mc72-4 (x &rest y)
    (:method-combination mc72 "xyz")
    (:method (x &rest y) (list* x y)))
  (test-mc72-4 'a 'b))
ERROR

(progn
  (defgeneric test-mc72-5 (x y z &rest u)
    (:method-combination mc72 "xyz")
    (:method (x y z &rest u) (list* x y z u)))
  (list (test-mc72-5 'a :test 'eq) (test-mc72-5 'a :test 'eq 'd 'e)
        (test-mc72-5 'a :test 'eq :test-not 'nequal)
        (test-mc72-5 'a :test 'eq :test 'eq :test-not 'nequal)
        (test-mc72-5 'a :test 'eq :test-not 'nequal :test 'eql :test-not 'nequalp)))
(("xyz" "def" RESULT A :TEST DEF1 DEF2 EQ NEQ NIL NIL (A :TEST EQ))
 ("xyz" "def" RESULT A :TEST DEF1 DEF2 EQ NEQ NIL NIL (A :TEST EQ D E))
 ("xyz" "def" RESULT A :TEST DEF1 DEF2 EQ NEQUAL NIL T (A :TEST EQ :TEST-NOT NEQUAL))
 ("xyz" "def" RESULT A :TEST DEF1 DEF2 EQ NEQUAL T T (A :TEST EQ :TEST EQ :TEST-NOT NEQUAL))
 ("xyz" "def" RESULT A :TEST DEF1 DEF2 EQL NEQUAL T T (A :TEST EQ :TEST-NOT NEQUAL :TEST EQL :TEST-NOT NEQUALP)))

(progn
  (defgeneric test-mc72-6 (x &optional y z u &rest v)
    (:method-combination mc72 "xyz")
    (:method (x &optional y z u &rest v) (list* x y z u v)))
  (list (test-mc72-6 'a) (test-mc72-6 'a 'b 'c)
        (test-mc72-6 'a :test 'eq 'd :test-not 'nequal)
        (test-mc72-6 'a :test 'eq 'd :test 'eq :test-not 'nequal)
        (test-mc72-6 'a :test 'eq 'd :test-not 'nequal :test 'eql :test-not 'nequalp)))
(("xyz" "def" RESULT A NIL DEF1 DEF2 EQ NEQ NIL NIL (A NIL NIL NIL))
 ("xyz" "def" RESULT A NIL B C EQ NEQ NIL NIL (A B C NIL))
 ("xyz" "def" RESULT A NIL :TEST EQ EQ NEQUAL NIL T (A :TEST EQ D :TEST-NOT NEQUAL))
 ("xyz" "def" RESULT A NIL :TEST EQ EQ NEQUAL T T (A :TEST EQ D :TEST EQ :TEST-NOT NEQUAL))
 ("xyz" "def" RESULT A NIL :TEST EQ EQL NEQUAL T T (A :TEST EQ D :TEST-NOT NEQUAL :TEST EQL :TEST-NOT NEQUALP)))

; Check that it's possible to provide 'redo' and 'return' restarts for each
; method invocation.
(progn
  (defun prompt-for-new-values ()
    (format *debug-io* "~&New values: ")
    (list (read *debug-io*)))
  (defun add-method-restarts (form method)
    (let ((block (gensym))
          (tag (gensym)))
      `(BLOCK ,block
         (TAGBODY
           ,tag
           (RETURN-FROM ,block
             (RESTART-CASE ,form
               (METHOD-REDO ()
                 :REPORT (LAMBDA (STREAM) (FORMAT STREAM "Try calling ~S again." ,method))
                 (GO ,tag))
               (METHOD-RETURN (L)
                 :REPORT (LAMBDA (STREAM) (FORMAT STREAM "Specify return values for ~S call." ,method))
                 :INTERACTIVE (LAMBDA () (PROMPT-FOR-NEW-VALUES))
                 (RETURN-FROM ,block (VALUES-LIST L)))))))))
  (defun convert-effective-method (efm)
    (if (consp efm)
      (if (eq (car efm) 'CALL-METHOD)
        (let ((method-list (third efm)))
          (if (or (typep (first method-list) 'method) (rest method-list))
            ; Reduce the case of multiple methods to a single one.
            ; Make the call to the next-method explicit.
            (convert-effective-method
              `(CALL-METHOD ,(second efm)
                 ((MAKE-METHOD
                    (CALL-METHOD ,(first method-list) ,(rest method-list))))))
            ; Now the case of at most one method.
            (if (typep (second efm) 'method)
              ; Wrap the method call in a RESTART-CASE.
              (add-method-restarts
                (cons (convert-effective-method (car efm))
                      (convert-effective-method (cdr efm)))
                (second efm))
              ; Normal recursive processing.
              (cons (convert-effective-method (car efm))
                    (convert-effective-method (cdr efm))))))
        (cons (convert-effective-method (car efm))
              (convert-effective-method (cdr efm))))
      efm))
  (define-method-combination standard-with-restarts ()
         ((around (:around))
          (before (:before))
          (primary () :required t)
          (after (:after)))
    (flet ((call-methods-sequentially (methods)
             (mapcar #'(lambda (method)
                         `(CALL-METHOD ,method))
                     methods)))
      (let ((form (if (or before after (rest primary))
                    `(MULTIPLE-VALUE-PROG1
                       (PROGN
                         ,@(call-methods-sequentially before)
                         (CALL-METHOD ,(first primary) ,(rest primary)))
                       ,@(call-methods-sequentially (reverse after)))
                    `(CALL-METHOD ,(first primary)))))
        (when around
          (setq form
                `(CALL-METHOD ,(first around)
                              (,@(rest around) (MAKE-METHOD ,form)))))
        (convert-effective-method form))))
  (defgeneric testgf16 (x) (:method-combination standard-with-restarts))
  (defclass testclass16a () ())
  (defclass testclass16b (testclass16a) ())
  (defclass testclass16c (testclass16a) ())
  (defclass testclass16d (testclass16b testclass16c) ())
  (defmethod testgf16 ((x testclass16a))
    (list 'a
          (not (null (find-restart 'method-redo)))
          (not (null (find-restart 'method-return)))))
  (defmethod testgf16 ((x testclass16b))
    (cons 'b (call-next-method)))
  (defmethod testgf16 ((x testclass16c))
    (cons 'c (call-next-method)))
  (defmethod testgf16 ((x testclass16d))
    (cons 'd (call-next-method)))
  (testgf16 (make-instance 'testclass16d)))
(D B C A T T)


;; Method combination with user-defined methods

(progn
  (defclass user-method (standard-method) (myslot))
  t)
T

(defmacro def-user-method (name &rest rest)
  (let* ((lambdalist-position (position-if #'listp rest))
         (qualifiers (subseq rest 0 lambdalist-position))
         (lambdalist (elt rest lambdalist-position))
         (body (subseq rest (+ lambdalist-position 1)))
         (required-part (subseq lambdalist 0 (or (position-if #'(lambda (x) (member x lambda-list-keywords)) lambdalist) (length lambdalist))))
         (specializers (mapcar #'find-class (mapcar #'(lambda (x) (if (consp x) (second x) 't)) required-part)))
         (unspecialized-required-part (mapcar #'(lambda (x) (if (consp x) (first x) x)) required-part))
         (unspecialized-lambdalist (append unspecialized-required-part (subseq lambdalist (length required-part)))))
    `(PROGN
       (ADD-METHOD #',name
         (MAKE-INSTANCE 'user-method
           :QUALIFIERS ',qualifiers
           :LAMBDA-LIST ',unspecialized-lambdalist
           :SPECIALIZERS ',specializers
           :FUNCTION
             #'(LAMBDA (ARGUMENTS NEXT-METHODS-LIST)
                 (FLET ((NEXT-METHOD-P () NEXT-METHODS-LIST)
                        (CALL-NEXT-METHOD (&REST NEW-ARGUMENTS)
                          (UNLESS NEW-ARGUMENTS (SETQ NEW-ARGUMENTS ARGUMENTS))
                          (IF (NULL NEXT-METHODS-LIST)
                            (ERROR "no next method for arguments ~:S" ARGUMENTS)
                            (FUNCALL (#+SBCL SB-PCL:METHOD-FUNCTION
                                      #+CMU MOP:METHOD-FUNCTION
                                      #-(or SBCL CMU) METHOD-FUNCTION
                                       (FIRST NEXT-METHODS-LIST))
                                     NEW-ARGUMENTS (REST NEXT-METHODS-LIST)))))
                   (APPLY #'(LAMBDA ,unspecialized-lambdalist ,@body) ARGUMENTS)))))
       ',name)))
DEF-USER-METHOD

; Single method.
(progn
  (defgeneric test-um01 (x y))
  (def-user-method test-um01 ((x symbol) (y symbol)) (list x y (next-method-p)))
  (test-um01 'a 'b))
(A B NIL)

; First among three primary methods.
(progn
  (defgeneric test-um02 (x))
  (def-user-method test-um02 ((x integer))
    (list* 'integer x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um02 ((x rational))
    (list* 'rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um02 ((x real))
    (list 'real x (not (null (next-method-p)))))
  (test-um02 17))
(INTEGER 17 T RATIONAL 17 T REAL 17 NIL)

; Second among three primary methods.
(progn
  (defgeneric test-um03 (x))
  (defmethod test-um03 ((x integer))
    (list* 'integer x (not (null (next-method-p))) (call-next-method)))
  (def-user-method test-um03 ((x rational))
    (list* 'rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um03 ((x real))
    (list 'real x (not (null (next-method-p)))))
  (test-um03 17))
(INTEGER 17 T RATIONAL 17 T REAL 17 NIL)

; Last among three primary methods.
(progn
  (defgeneric test-um04 (x))
  (defmethod test-um04 ((x integer))
    (list* 'integer x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um04 ((x rational))
    (list* 'rational x (not (null (next-method-p))) (call-next-method)))
  (def-user-method test-um04 ((x real))
    (list 'real x (not (null (next-method-p)))))
  (test-um04 17))
(INTEGER 17 T RATIONAL 17 T REAL 17 NIL)

; First among two before methods.
(let ((results nil))
  (defgeneric test-um05 (x))
  (defmethod test-um05 (x) (push 'PRIMARY results) (push x results))
  (def-user-method test-um05 :before ((x integer)) (push 'BEFORE-INTEGER results) (push x results))
  (defmethod test-um05 :before ((x real)) (push 'BEFORE-REAL results) (push x results))
  (test-um05 17)
  (nreverse results))
(BEFORE-INTEGER 17 BEFORE-REAL 17 PRIMARY 17)

; Last among two before methods.
(let ((results nil))
  (defgeneric test-um06 (x))
  (defmethod test-um06 (x) (push 'PRIMARY results) (push x results))
  (defmethod test-um06 :before ((x integer)) (push 'BEFORE-INTEGER results) (push x results))
  (def-user-method test-um06 :before ((x real)) (push 'BEFORE-REAL results) (push x results))
  (test-um06 17)
  (nreverse results))
(BEFORE-INTEGER 17 BEFORE-REAL 17 PRIMARY 17)

; First among two after methods.
(let ((results nil))
  (defgeneric test-um07 (x))
  (defmethod test-um07 (x) (push 'PRIMARY results) (push x results))
  (defmethod test-um07 :after ((x integer)) (push 'AFTER-INTEGER results) (push x results))
  (def-user-method test-um07 :after ((x real)) (push 'AFTER-REAL results) (push x results))
  (test-um07 17)
  (nreverse results))
(PRIMARY 17 AFTER-REAL 17 AFTER-INTEGER 17)

; Last among two after methods.
(let ((results nil))
  (defgeneric test-um08 (x))
  (defmethod test-um08 (x) (push 'PRIMARY results) (push x results))
  (def-user-method test-um08 :after ((x integer)) (push 'AFTER-INTEGER results) (push x results))
  (defmethod test-um08 :after ((x real)) (push 'AFTER-REAL results) (push x results))
  (test-um08 17)
  (nreverse results))
(PRIMARY 17 AFTER-REAL 17 AFTER-INTEGER 17)

; First among three around methods.
(progn
  (defgeneric test-um10 (x))
  (defmethod test-um10 ((x integer))
    (list* 'integer x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um10 ((x rational))
    (list* 'rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um10 ((x real))
    (list 'real x (not (null (next-method-p)))))
  (defmethod test-um10 :after ((x real)))
  (def-user-method test-um10 :around ((x integer))
    (list* 'around-integer x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um10 :around ((x rational))
    (list* 'around-rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um10 :around ((x real))
    (list* 'around-real x (not (null (next-method-p))) (call-next-method)))
  (test-um10 17))
(AROUND-INTEGER 17 T AROUND-RATIONAL 17 T AROUND-REAL 17 T INTEGER 17 T RATIONAL 17 T REAL 17 NIL)

; Second among three around methods.
(progn
  (defgeneric test-um11 (x))
  (defmethod test-um11 ((x integer))
    (list* 'integer x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um11 ((x rational))
    (list* 'rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um11 ((x real))
    (list 'real x (not (null (next-method-p)))))
  (defmethod test-um11 :after ((x real)))
  (defmethod test-um11 :around ((x integer))
    (list* 'around-integer x (not (null (next-method-p))) (call-next-method)))
  (def-user-method test-um11 :around ((x rational))
    (list* 'around-rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um11 :around ((x real))
    (list* 'around-real x (not (null (next-method-p))) (call-next-method)))
  (test-um11 17))
(AROUND-INTEGER 17 T AROUND-RATIONAL 17 T AROUND-REAL 17 T INTEGER 17 T RATIONAL 17 T REAL 17 NIL)

; Third among three around methods.
(progn
  (defgeneric test-um12 (x))
  (defmethod test-um12 ((x integer))
    (list* 'integer x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um12 ((x rational))
    (list* 'rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um12 ((x real))
    (list 'real x (not (null (next-method-p)))))
  (defmethod test-um12 :after ((x real)))
  (defmethod test-um12 :around ((x integer))
    (list* 'around-integer x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um12 :around ((x rational))
    (list* 'around-rational x (not (null (next-method-p))) (call-next-method)))
  (def-user-method test-um12 :around ((x real))
    (list* 'around-real x (not (null (next-method-p))) (call-next-method)))
  (test-um12 17))
(AROUND-INTEGER 17 T AROUND-RATIONAL 17 T AROUND-REAL 17 T INTEGER 17 T RATIONAL 17 T REAL 17 NIL)

; Second among three around methods, and also a user-defined primary method.
(progn
  (defgeneric test-um13 (x))
  (defmethod test-um13 ((x integer))
    (list* 'integer x (not (null (next-method-p))) (call-next-method)))
  (def-user-method test-um13 ((x rational))
    (list* 'rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um13 ((x real))
    (list 'real x (not (null (next-method-p)))))
  (defmethod test-um13 :after ((x real)))
  (defmethod test-um13 :around ((x integer))
    (list* 'around-integer x (not (null (next-method-p))) (call-next-method)))
  (def-user-method test-um13 :around ((x rational))
    (list* 'around-rational x (not (null (next-method-p))) (call-next-method)))
  (defmethod test-um13 :around ((x real))
    (list* 'around-real x (not (null (next-method-p))) (call-next-method)))
  (test-um13 17))
(AROUND-INTEGER 17 T AROUND-RATIONAL 17 T AROUND-REAL 17 T INTEGER 17 T RATIONAL 17 T REAL 17 NIL)


#|
;; Check that invalid print-object methods yield a warning.
(progn
  (defclass foo128 () ())
  (defmethod print-object ((object foo128) stream)
    (print-unreadable-object (object stream :type t :identity t)
      (write "BLABLA" :stream stream)))
  (block nil
    (handler-bind ((WARNING #'(lambda (w) (declare (ignore w)) (return 'WARNING))))
      (prin1-to-string (make-instance 'foo128)))
    nil))
#+CLISP WARNING
#-CLISP NIL
|#


;; Test against bug in clos::%call-next-method and FUNCALL&SKIP&RETGF.
(progn
  (defclass foo129 ()
    ((x :initarg :x)))
  (defparameter *foo129-counter* 0)
  (defmethod initialize-instance ((instance foo129) &rest initargs &key (x '()))
    (incf *foo129-counter*) ; (format t "~&Initializing ~S  ~:S~%" instance x)
    (apply #'call-next-method instance :x (cons 'a x) initargs))
  (make-instance 'foo129)
  *foo129-counter*)
1

(progn
  (defclass foo130 ()
    ((x :initarg :x)))
  (defparameter *foo130-counter* 0)
  (locally (declare (compile))
    (defmethod initialize-instance ((instance foo130) &rest initargs &key (x '()))
      (incf *foo130-counter*) ; (format t "~&Initializing ~S  ~:S~%" instance x)
      (apply #'call-next-method instance :x (cons 'a x) initargs)))
  (make-instance 'foo130)
  *foo130-counter*)
1

;; Check that undefined classes are treated as undefined, even though they
;; are represented by a FORWARD-REFERENCED-CLASS.
(progn
  #+CLISP (setq custom:*forward-referenced-class-misdesign* t)
  (defclass foo131 (forwardclass01) ())
  t)
T
(find-class 'forwardclass01)
ERROR
(find-class 'forwardclass01 nil)
NIL
(typep 1 'forwardclass01)
ERROR
(locally (declare (compile)) (typep 1 'forwardclass01))
ERROR
(type-expand 'forwardclass01)
ERROR
(subtypep 'forwardclass01 't)
ERROR
(subtypep 'nil 'forwardclass01)
ERROR
#+CLISP (sys::subtype-integer 'forwardclass01)
#+CLISP NIL ; should also be ERROR
#+CLISP (sys::subtype-sequence 'forwardclass01)
#+CLISP NIL ; should also be ERROR
(defstruct (foo131a (:include forwardclass01)))
ERROR
(defmethod foo131b ((x forwardclass01)))
ERROR
;; Same thing with opposite setting of *forward-referenced-class-misdesign*.
(progn
  #+CLISP (setq custom:*forward-referenced-class-misdesign* nil)
  (defclass foo132 (forwardclass02) ())
  t)
T
(find-class 'forwardclass02)
ERROR
(find-class 'forwardclass02 nil)
NIL
(typep 1 'forwardclass02)
ERROR
(locally (declare (compile)) (typep 1 'forwardclass02))
ERROR
(type-expand 'forwardclass02)
ERROR
(subtypep 'forwardclass02 't)
ERROR
(subtypep 'nil 'forwardclass02)
ERROR
#+CLISP (sys::subtype-integer 'forwardclass02)
#+CLISP NIL ; should also be ERROR
#+CLISP (sys::subtype-sequence 'forwardclass02)
#+CLISP NIL ; should also be ERROR
(defstruct (foo132a (:include forwardclass02)))
ERROR
(defmethod foo132b ((x forwardclass02)))
ERROR

;;; <http://article.gmane.org/gmane.lisp.clisp.general:9582>
(let ((ret '()))
  (defclass mixin-foo-144 () ())
  (defclass class-foo-144 (mixin-foo-144) ())
  (defgeneric fun-144 (x))
  (defmethod fun-144 ((x class-foo-144))
    (push 'class-foo-144 ret))
  (defclass class-bar-144 () ())
  (defmethod fun-144 :after ((x class-bar-144))
    (push 'class-bar-144-after ret))
  ;; redefine class class-foo
  (defclass mixin-foo-144 (class-bar-144) ())
  (fun-144 (make-instance 'class-foo-144))
  (nreverse ret))
(CLASS-FOO-144 CLASS-BAR-144-AFTER)

;; Similar, but call the generic function already before the redefinition.
(let ((ret ()))
  (defclass mixin-foo-145 () ())
  (defclass class-foo-145 (mixin-foo-145) ())
  (defgeneric fun-145 (x))
  (defmethod fun-145 ((x class-foo-145))
    (push 'class-foo-145 ret))
  (defclass class-bar-145 () ())
  (defmethod fun-145 :after ((x class-bar-145))
    (push 'class-bar-145-after ret))
  (let ((inst (make-instance 'class-foo-145)))
    (fun-145 inst)
    (setq ret '())
    ;; redefine class class-foo
    (defclass mixin-foo-145 (class-bar-145) ())
    (fun-145 inst)
    (nreverse ret)))
(CLASS-FOO-145 CLASS-BAR-145-AFTER)

(progn (load (make-pathname :name "listeners" :type nil
                            :defaults *run-test-truename*))
       (with-open-stream (s1 (make-string-input-stream "("))
         (with-open-stream (s2 (make-string-input-stream "())"))
           (with-open-stream (l (make-instance 'listener-input-stream
                                               :stream s2))
             (with-open-stream (c (make-concatenated-stream s1 l))
               (read c))))))
(NIL)
