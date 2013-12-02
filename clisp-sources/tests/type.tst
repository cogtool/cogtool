;; -*- Lisp -*-

(TYPEP (QUOTE A) (QUOTE SYMBOL))
T

(TYPEP (QUOTE NIL) (QUOTE SYMBOL))
T

(TYPEP (QUOTE (NIL)) (QUOTE SYMBOL))
NIL

(TYPEP 3 (QUOTE INTEGER))
T

(TYPEP 3 (QUOTE (INTEGER 0 4)))
T

(TYPEP 3 (QUOTE (INTEGER 0 3)))
T

(TYPEP 3 (QUOTE (INTEGER 0 2)))
NIL

(TYPEP 3 (QUOTE (FLOAT 0.0 2.0)))
NIL

(TYPEP 3 (QUOTE (FLOAT 0.0 2.0)))
NIL

(TYPEP 3 (QUOTE (FLOAT 0.0 4.0)))
NIL

(TYPEP 3.2 (QUOTE (FLOAT 0.0 4.0)))
T

(TYPEP 3.2 (QUOTE (FLOAT 0.0 3.2)))
T

(TYPEP 3.2 (QUOTE (FLOAT 0.0 (3.2))))
NIL

(TYPEP 3.2 (QUOTE (SHORT-FLOAT 0.0S0 3.2S0)))
#+(or ALLEGRO CMU SBCL ECL) T #-(or ALLEGRO CMU SBCL ECL) NIL

(TYPEP 3.2 (QUOTE (SINGLE-FLOAT 0.0F0 3.2F0)))
T

(TYPEP 3.2 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2S0 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2 (QUOTE (DOUBLE-FLOAT 0.0D0 3.2D0)))
NIL

(TYPEP 3.2 (QUOTE (FLOAT 0.0 3.2)))
T

(TYPEP 3.2S0 (QUOTE (FLOAT 0.0S0 3.2S0)))
T

(TYPEP 2.0S0 (QUOTE (SHORT-FLOAT 0.0S0 3.0S0)))
T

(TYPEP 2.0S0 (QUOTE (SINGLE-FLOAT 0.0F0 3.0F0)))
#+(or ALLEGRO CMU SBCL ECL) T #-(or ALLEGRO CMU SBCL ECL) NIL

(TYPEP 2.0 (QUOTE (SINGLE-FLOAT 0.0F0 3.0F0)))
T

(TYPEP 2.0D0 (QUOTE (DOUBLE-FLOAT 0.0D0 3.0D0)))
T

(TYPEP 3.0D0 (QUOTE (DOUBLE-FLOAT 0.0D0 3.0D0)))
T

(TYPEP 3.0D0 (QUOTE (DOUBLE-FLOAT 0.0D0 (3.0D0))))
NIL

(TYPEP 4 (QUOTE (MOD 4)))
NIL

(TYPEP 4 (QUOTE (MOD 5)))
T

(TYPEP 4 (QUOTE (RATIONAL 2 5)))
T

(TYPEP 4 (QUOTE (RATIONAL 2 7/2)))
NIL

(TYPEP 4 (QUOTE (RATIONAL 2 9/2)))
T

(TYPEP 4 (QUOTE (RATIONAL 2 4)))
T

(TYPEP 4/3 (QUOTE (RATIONAL 2 4)))
NIL

(TYPEP 2 (QUOTE (RATIONAL 2 4)))
T

(TYPEP "abcd" (QUOTE STRING))
T

(TYPEP "abcd" (QUOTE (STRING 4)))
T

(TYPEP "abcd" (QUOTE (STRING 43)))
NIL

(TYPEP '#(2 3) (QUOTE (COMPLEX INTEGER)))
NIL

(TYPEP '#(2 3) (QUOTE COMPLEX))
NIL

(TYPEP #C(2 3) (QUOTE COMPLEX))
T

(TYPEP #C(2 3) (QUOTE (COMPLEX INTEGER)))
T

;; depends on (UPGRADED-COMPLEX-PART-TYPE FLOAT)
(TYPEP #C(2.2 3) (QUOTE (COMPLEX FLOAT)))
#+CLISP NIL #-CLISP T

(TYPEP #C(2 3) (QUOTE (COMPLEX SYMBOL)))
ERROR

(TYPEP '#(A B C D) (QUOTE VECTOR))
T

(TYPEP '#(A B C D) (QUOTE (VECTOR * 4)))
T

;; depends on (UPGRADED-COMPLEX-PART-TYPE '(EQL 0))
(TYPEP #C(0 1) '(COMPLEX (EQL 0)))
#+(or CLISP GCL CMU19A OpenMCL) NIL #+(or CMU18 (and CMU19 (not CMU19A)) SBCL LISPWORKS) T #-(or CLISP GCL CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#| ; depends on (upgraded-array-element-type 'SYMBOL) !
 (TYPEP '#(A B C D) (QUOTE (VECTOR SYMBOL 4)))
 NIL
|#

(TYPEP (QUOTE A) (QUOTE (SYMBOL CONS)))
ERROR

(TYPEP (QUOTE A) (QUOTE (OR CONS SYMBOL)))
T

(TYPEP (QUOTE A) (QUOTE (OR CONS NUMBER)))
NIL

(TYPEP (QUOTE A) (QUOTE (OR ATOM NUMBER)))
T

(TYPEP (QUOTE A) (QUOTE (AND ATOM NUMBER)))
NIL

(TYPEP (QUOTE 2) (QUOTE (AND ATOM NUMBER)))
T

(NOT (NOT (TYPEP (QUOTE 2) (QUOTE (MEMBER 1 2 3)))))
T

(NOT (NOT (TYPEP (QUOTE 2) (QUOTE (MEMBER 1 3)))))
NIL

(TYPEP (QUOTE 2) (QUOTE (NOT (MEMBER 1 3))))
T

(TYPEP (QUOTE 2) (QUOTE (NOT (MEMBER 1 2 3))))
NIL

(TYPEP 2 (QUOTE (AND NUMBER (NOT SYMBOL))))
T

(TYPEP 2 (QUOTE (AND STRING (NOT SYMBOL))))
NIL

(TYPEP 2 (QUOTE (OR STRING (NOT SYMBOL))))
T

(TYPEP (QUOTE CONS) (QUOTE FUNCTION))
#-(or CLtL2 ANSI-CL CLISP) T #+(or CLtL2 ANSI-CL CLISP) NIL

(TYPEP (QUOTE CONS) (QUOTE (SATISFIES FUNCTIONP)))
#-(or CLtL2 ANSI-CL CLISP) T #+(or CLtL2 ANSI-CL CLISP) NIL

(TYPEP (QUOTE CONS) (QUOTE (SATISFIES NOT)))
NIL

(TYPEP (QUOTE NIL) (QUOTE (SATISFIES NOT)))
T

(TYPEP (QUOTE NIL) NIL)
NIL

(TYPEP (QUOTE T) NIL)
NIL

(SUBTYPEP (QUOTE CONS) T)
T

(SUBTYPEP NIL (QUOTE CONS))
T

(SUBTYPEP (QUOTE CONS) (QUOTE LIST))
T

(SUBTYPEP (QUOTE CONS) (QUOTE (OR ATOM CONS)))
T

(SUBTYPEP (QUOTE CONS) (QUOTE (AND ATOM CONS)))
NIL

(SUBTYPEP (QUOTE CONS) (QUOTE (NOT ATOM)))
#-(or AKCL ALLEGRO) T #+(or AKCL ALLEGRO) NIL

(SUBTYPEP (QUOTE LIST) (QUOTE (NOT ATOM)))
NIL

(SUBTYPEP (QUOTE (INTEGER 1 5)) (QUOTE (INTEGER 0 7)))
T

(SUBTYPEP (QUOTE (INTEGER 1 5)) (QUOTE (INTEGER 0 (5))))
NIL

(SUBTYPEP (QUOTE (INTEGER 1 5)) (QUOTE (INTEGER 0 5)))
T

(SUBTYPEP (QUOTE (INTEGER 1 5)) (QUOTE (MOD 5)))
NIL

(SUBTYPEP (QUOTE (INTEGER 1 (5))) (QUOTE (MOD 5)))
T

(SUBTYPEP (QUOTE (OR (INTEGER 1 (5) FLOAT)))
          (QUOTE (OR FLOAT (MOD 5))))
#+(or XCL ECL) T
#+(or CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) ERROR
#-(or XCL CLISP ALLEGRO CMU SBCL ECL OpenMCL LISPWORKS) UNKNOWN

(SUBTYPEP (QUOTE (OR (INTEGER 1 (5)) FLOAT)) (QUOTE (OR FLOAT (MOD 5))))
T

(SUBTYPEP (QUOTE (AND NUMBER (FLOAT 1.0 (5.0)))) (QUOTE (OR FLOAT (MOD 5))))
T

(SUBTYPEP (QUOTE (AND NUMBER (NOT (FLOAT 1.0 (5.0)))))
          (QUOTE (OR FLOAT (MOD 5))))
NIL

(SUBTYPEP (QUOTE (AND FLOAT (NOT (FLOAT 1.0 (5.0)))))
          (QUOTE (OR FLOAT (MOD 5))))
T

(SUBTYPEP (QUOTE (AND FLOAT (NOT (FLOAT 1.0 (5.0)))))
          (QUOTE (OR (FLOAT * 1.0) (FLOAT * 5.0))))
NIL

(SUBTYPEP (QUOTE (SATISFIES CONSP)) (QUOTE LIST))
NIL

(SUBTYPEP (QUOTE SIMPLE-STRING) (QUOTE ARRAY))
T

(DEFTYPE MOD1 (N) `(AND NUMBER (FLOAT 0.0 (,N))))
MOD1

(TYPEP 4.1 (QUOTE (MOD1 5.0)))
T

(TYPEP 4.1 (QUOTE (MOD1 4.1)))
NIL

(SUBTYPEP (QUOTE (FLOAT 2.3 6.7)) (QUOTE (MOD1 6.8)))
T

(SUBTYPEP (QUOTE (FLOAT 2.3 6.7)) (QUOTE (MOD1 6.7)))
NIL

(DEFUN BELIEBIGER-TEST (A) (MEMBER A (QUOTE (U I V X))))
BELIEBIGER-TEST

(NOT (NULL (TYPEP (QUOTE U) (QUOTE (SATISFIES BELIEBIGER-TEST)))))
T

(TYPEP (QUOTE A) (QUOTE (SATISFIES BELIEBIGER-TEST)))
NIL

(SUBTYPEP (QUOTE (MEMBER U I)) (QUOTE (SATISFIES BELIEBIGER-TEST)))
T

(SUBTYPEP (QUOTE (OR (MEMBER U I))) (QUOTE (SATISFIES BELIEBIGER-TEST)))
T

(SUBTYPEP (QUOTE (OR (MEMBER U I A))) (QUOTE (SATISFIES BELIEBIGER-TEST)))
NIL

(SUBTYPEP (QUOTE (SATISFIES BELIEBIGER-TEST)) (QUOTE (MEMBER U I V X Y)))
NIL

(DEFTYPE BELIEBIGER-TYP () (QUOTE (SATISFIES BELIEBIGER-TEST)))
BELIEBIGER-TYP

(NOT (NULL (TYPEP (QUOTE U) (QUOTE BELIEBIGER-TYP))))
T

(TYPEP (QUOTE A) (QUOTE BELIEBIGER-TYP))
NIL

#+(and CLISP FFI)
(TYPEP #\A 'FFI:FOREIGN-ADDRESS)
#+(and CLISP FFI)
NIL

(SUBTYPEP (QUOTE (MEMBER U I)) (QUOTE BELIEBIGER-TYP))
T

(SUBTYPEP (QUOTE BELIEBIGER-TYP) (QUOTE (MEMBER U I V X Y)))
NIL
(subtypep nil 'fixnum) t
(subtypep 'short-float 'float ) t
(subtypep 'single-float 'float ) t
(subtypep 'double-float 'float ) t
(subtypep 'long-float 'float ) t

(subtypep 'null 'symbol) t
(subtypep 'null 'list) t
(subtypep 'cons 'list) t

(subtypep 'standard-char #-(or CMU SBCL LISPWORKS) 'string-char #+(or CMU SBCL LISPWORKS) 'character) t

(subtypep #-(or CMU SBCL LISPWORKS) 'string-char #+(or CMU SBCL LISPWORKS) 'character 'character) t

(subtypep 'string 'vector) t

(subtypep 'bit-vector 'vector) t
(subtypep 'vector 'array) t

(subtypep 'simple-array 'array) t

(subtypep 'simple-vector 'simple-array) t
(subtypep 'simple-vector 'vector) t
(subtypep 'simple-string 'simple-array) t
(subtypep 'simple-bit-vector 'simple-array) t

(subtypep 'simple-string 'string) t
(subtypep 'simple-string 'vector) t
(subtypep 'simple-string 'simple-vector) nil
(subtypep 'simple-bit-vector 'bit-vector) t
(subtypep 'bit-vector 'vector) t
(subtypep 'simple-bit-vector 'simple-vector) nil

(subtypep 'unsigned-byte 'integer) t
(subtypep 'signed-byte 'integer) t

(subtypep 'integer '*) ERROR

(type-of (coerce '(1 2 3 4) '(simple-array (unsigned-byte 8))))
#-(or CMU SBCL LISPWORKS) (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (4)) #+(or CMU SBCL LISPWORKS) ERROR

(type-of (coerce '(1 2 3 4) '(simple-array *)))
#-(or CMU SBCL LISPWORKS) (SIMPLE-VECTOR 4) #+(or CMU SBCL LISPWORKS) ERROR

(type-of (coerce '(1 2 3 4) '(simple-array * (4))))
(SIMPLE-VECTOR 4)

;; these must be recognized correctly (see CLHS SUBTYPE and Figure 4-2)
(multiple-value-list (subtypep 'atom 'cons)) (nil t)
(multiple-value-list (subtypep 'atom 'list)) (nil t)
(multiple-value-list (subtypep 'cons 'atom)) (nil t)
(multiple-value-list (subtypep 'list 'atom)) (nil t)
(multiple-value-list (subtypep 'stream 'atom)) (t t)
(multiple-value-list (subtypep 'string 'atom)) (t t)
(multiple-value-list (subtypep 'vector 'atom)) (t t)
(multiple-value-list (subtypep nil nil))     (t t)
(multiple-value-list (subtypep 'extended-char 'character))     (t t)
;; but don't get fooled in saying "no" when there is a SATISFIES type
(multiple-value-list (subtypep 'atom '(or cons (satisfies unknown)))) (nil nil)
(multiple-value-list (subtypep '(vector nil) 'string)) (T T)
#+(and CLISP FFI)
(multiple-value-list (subtypep 'ffi:foreign-function 'function))
#+(and CLISP FFI)
(T T)

(multiple-value-list
 (subtypep '(and (not boolean) standard-char) 'standard-char))
(t t)

(let ((x 1)) (ctypecase x (t 'a)))  a
(let ((x 1)) (etypecase x (t 'a)))  a

(multiple-value-list (subtypep '(AND CONS (NOT (EQL 0))) 'CONS))  (t t)

(multiple-value-list (subtypep '(integer 1 2) '(real 1 2)))  (t t)
(multiple-value-list (subtypep '(integer 1 2) '(real (1) 2)))  (nil t)
(multiple-value-list (subtypep '(mod 10) '(or (mod 10) (mod 10))))  (t t)

;; http://www.lisp.org/HyperSpec/Body/fun_type-of.html
;; mandates that (TYPEP X Y) ==> (SUBTYPEP (TYPE-OF X) Y)
;; for all "built-in types" Y as listed in table 4-2 in
;; http://www.lisp.org/HyperSpec/Body/sec_4-2-3.html
(defun typeof-typep-subtype (x y)
  (list (type-of x) (typep x y) (subtypep (type-of x) y)))
TYPEOF-TYPEP-SUBTYPE

(typeof-typep-subtype #\Space 'standard-char)
(STANDARD-CHAR T T)

(typeof-typep-subtype #\Return 'standard-char)
(BASE-CHAR NIL NIL)

#+CLISP
(typeof-typep-subtype (symbol-function 'let) 'compiled-function)
#+CLISP
(SPECIAL-OPERATOR NIL NIL)

#+CLISP
(typeof-typep-subtype (symbol-function 'let) 'function)
#+CLISP
(SPECIAL-OPERATOR NIL NIL)

(typeof-typep-subtype #'car 'compiled-function)
#-(or SBCL CMU) (COMPILED-FUNCTION T T)
#+(or SBCL CMU) (FUNCTION T T)

(typeof-typep-subtype #'car 'function)
#-(or SBCL CMU) (COMPILED-FUNCTION T T)
#+(or SBCL CMU) (FUNCTION T T)

(typeof-typep-subtype #'car 'generic-function)
#-(or SBCL CMU) (COMPILED-FUNCTION NIL NIL)
#+(or SBCL CMU) (FUNCTION NIL NIL)

(typeof-typep-subtype #'compile 'compiled-function)
#-(or SBCL CMU) (COMPILED-FUNCTION T T)
#+(or SBCL CMU) (FUNCTION T T)

(typeof-typep-subtype #'compile 'function)
#-(or SBCL CMU) (COMPILED-FUNCTION T T)
#+(or SBCL CMU) (FUNCTION T T)

(typeof-typep-subtype #'compile 'generic-function)
#-(or SBCL CMU) (COMPILED-FUNCTION NIL NIL)
#+(or SBCL CMU) (FUNCTION NIL NIL)

(typeof-typep-subtype #'print-object 'compiled-function)
#-(or CMU SBCL LISPWORKS) (STANDARD-GENERIC-FUNCTION NIL NIL)
#+(or CMU SBCL LISPWORKS) (STANDARD-GENERIC-FUNCTION T T)

(typeof-typep-subtype #'print-object 'function)
(STANDARD-GENERIC-FUNCTION T T)

(typeof-typep-subtype #'print-object 'generic-function)
(STANDARD-GENERIC-FUNCTION T T)

(typeof-typep-subtype #'print-object 'standard-generic-function)
(STANDARD-GENERIC-FUNCTION T T)

(typeof-typep-subtype (make-array 0 :element-type nil) 'string)
((SIMPLE-ARRAY NIL (0)) T T)

(typeof-typep-subtype (make-array 0 :element-type nil) '(vector nil))
((SIMPLE-ARRAY NIL (0)) T T)

(subtypep '(member 0 1 2) '(mod 3)) t
(subtypep '(mod 3) '(member 0 1 2)) t
(subtypep '(member 0 1 2 4 8) '(mod 3)) nil
(subtypep '(mod 3) '(member 0 1 2 4 8)) t
(subtypep '(or (integer 0 999) (integer 1001 2000)) '(and (integer 0 2000) (not (eql 1000)))) t
(subtypep '(and (integer 0 2000) (not (eql 1000))) '(or (integer 0 999) (integer 1001 2000))) t
(subtypep '(or (integer 0 1000) (integer 1001 3000)) '(or (integer 0 2000) (integer 2001 3000))) t

(subtypep 'complex '(complex * #+CLISP *)) t
(subtypep '(complex * #+CLISP *) 'complex) t
(subtypep 'complex '(complex real #+CLISP real)) t
(subtypep '(complex real #+CLISP real) 'complex) t
(subtypep '(complex * #+CLISP *) '(complex real #+CLISP real)) t
(subtypep '(complex real #+CLISP real) '(complex * #+CLISP *)) t

(subtypep '(complex nil #+CLISP nil) 'nil) t

(multiple-value-list (subtypep '(not integer) '(or number (satisfies anything))))
(nil nil)

(multiple-value-list (subtypep '(and symbol number) 'nil))
(t t)

(multiple-value-list (subtypep '(array t (2 5)) '(or (array t (2 3 4)) (array t (2 4)))))
(nil t)

(multiple-value-list (subtypep '(array t (2 5)) '(not (or (array t (2 3 4)) (array t (2 5))))))
(nil t)

(multiple-value-list (subtypep '(array t (2 5)) '(not (or (array t (2 3 4)) (array t (2 4))))))
(t t)

(multiple-value-list (subtypep '(and (rational 2/3 1) ratio) '(and (rational 4/5 1) ratio)))
(nil t)

(multiple-value-list (subtypep '(rational 4/5 1) '(or (member 1) (rational 2/3 (1)))))
(t t)

(let ((l '(ARRAY BASE-CHAR BASE-STRING BIT-VECTOR BOOLEAN CHARACTER COMPLEX
           CONS FLOAT FUNCTION GENERIC-FUNCTION HASH-TABLE INTEGER LIST
           NULL NUMBER PACKAGE PATHNAME #+LOGICAL-PATHNAMES LOGICAL-PATHNAME
           RANDOM-STATE RATIONAL READTABLE REAL SEQUENCE
           STANDARD-GENERIC-FUNCTION STREAM STRING SYMBOL VECTOR))
      (failures '()))
  (dolist (a l)
    (dolist (b l)
      (unless (or (subtypep a b) (subtypep b a))
        ; See whether `(AND ,a ,b) is recognized as the null type.
        (let ((contains-NULL
                (and (member a '(BOOLEAN LIST NULL SEQUENCE SYMBOL))
                     (member b '(BOOLEAN LIST NULL SEQUENCE SYMBOL))))
              (contains-VECTOR
                (and (member a '(ARRAY VECTOR SEQUENCE))
                     (member b '(ARRAY VECTOR SEQUENCE)))))
          (unless (if contains-NULL
                    (and (equal (multiple-value-list (subtypep `(AND ,a ,b) 'NIL))
                                '(nil t))
                         (equal (multiple-value-list (subtypep `(AND ,a ,b) 'NULL))
                                '(t t)))
                    (if contains-VECTOR
                      (and (equal (multiple-value-list (subtypep `(AND ,a ,b) 'NIL))
                                  '(nil t))
                           (equal (multiple-value-list (subtypep `(AND ,a ,b) 'VECTOR))
                                  '(t t)))
                      (equal (multiple-value-list (subtypep `(AND ,a ,b) 'NIL))
                             '(t t))))
            (push (list a b) failures))))))
  failures)
#-(or CLISP CMU SBCL) NIL
#+(or CLISP CMU SBCL)
((STREAM STANDARD-GENERIC-FUNCTION)
 (STREAM GENERIC-FUNCTION)
 (STREAM FUNCTION)
 (STANDARD-GENERIC-FUNCTION STREAM)
 (GENERIC-FUNCTION STREAM)
 (FUNCTION STREAM))

;; from GCL ansi-test
(unintern 'bar) t
(unintern 'foo) t
#+SBCL (unintern 'copy-foo) #+SBCL t
#+SBCL (unintern 'make-foo) #+SBCL t
(progn
  (setq *DISJOINT-TYPES-LIST*
        '(CONS SYMBOL ARRAY NUMBER CHARACTER HASH-TABLE FUNCTION READTABLE
          PACKAGE PATHNAME STREAM RANDOM-STATE CONDITION RESTART))
  (defclass bar () ())
  (defstruct foo))
foo

(loop for type in *disjoint-types-list*
  unless (and (equal (multiple-value-list (subtypep type 'bar)) '(nil t))
              (equal (multiple-value-list (subtypep 'bar type)) '(nil t)))
  collect type)
nil

(loop for type in *disjoint-types-list*
  unless (and (equal (multiple-value-list (subtypep type 'foo)) '(nil t))
              (equal (multiple-value-list (subtypep 'foo type)) '(nil t)))
  collect type)
nil

(multiple-value-list (subtypep '(function (t t) cons) 'function))
(t t)
(multiple-value-list (subtypep 'function '(function (t t) cons)))
(nil nil)
(multiple-value-list (subtypep '(function (integer integer) cons) '(function (t t) cons)))
(nil nil)
(multiple-value-list (subtypep '(function (t t) cons) '(function (integer integer) cons)))
(nil nil)

(multiple-value-list (subtypep '(eql #.#'cons) '(function (integer integer) cons)))
(nil nil)

(multiple-value-list (subtypep '(and (eql #.#'cons) integer) 'character))
(t t)
(multiple-value-list (subtypep '(and (eql #.#'cons) (function (t t) cons)) 'character))
(nil nil)
(multiple-value-list (subtypep '(and (eql #.#'cons) function) 'character))
(nil t)

(multiple-value-list (subtypep '(and (eql #.#'cons) integer) '(function (integer integer) cons)))
(t t)
(multiple-value-list (subtypep '(and (eql #.#'cons) (function (t t) cons)) '(function (integer integer) cons)))
(nil nil)
(multiple-value-list (subtypep '(and (eql #.#'cons) function) '(function (integer integer) cons)))
(nil nil)

(multiple-value-list (subtypep '(and (eql #.#'cons) integer) 'function))
(t t)
(multiple-value-list (subtypep '(and (eql #.#'cons) (function (t t) cons)) 'function))
(t t)
(multiple-value-list (subtypep '(and (eql #.#'cons) function) 'function))
(t t)

(loop :with class = (find-class 'vector) :for x :in '((1 0) #(1 0) #*10)
  :for y = (coerce x class) :always (and (equalp y #(1 0)) (vectorp y)))
t

(coerce 1.0 'complex)
#C(1.0 0.0)

(deftype otherwise () nil)
otherwise

(typecase 'foo (otherwise :wrong) (t :right))
:right

(typecase 'foo (otherwise :wrong) (symbol :right) (t :wrong2))
:right

;; <http://www.lisp.org/HyperSpec/Body/speope_the.html>
(the fixnum (+ 5 7)) 12
(multiple-value-list (the (values) (truncate 3.2 2))) (1 1.2)
(multiple-value-list (the integer (truncate 3.2 2)))  (1 1.2)
(multiple-value-list (the (values integer) (truncate 3.2 2)))       (1 1.2)
(multiple-value-list (the (values integer float) (truncate 3.2 2))) (1 1.2)
(multiple-value-list (the (values integer float symbol) (truncate 3.2 2)))
(1 1.2)
(multiple-value-list (the (values integer float symbol t null list)
                       (truncate 3.2 2)))
(1 1.2)
(let ((i 100)) (declare (fixnum i)) (the fixnum (1+ i)))
101
(let* ((x (list 'a 'b 'c)) (y 5))
  (setf (the fixnum (car x)) y)
  x)
(5 B C)
(the (values) 'a) A
(multiple-value-list (the (values &rest symbol) (values 'a 'b))) (A B)

(type-of (make-array '(10 3) :element-type nil))
(SIMPLE-ARRAY NIL (10 3))
(type-of (make-array 10 :element-type nil))
(SIMPLE-ARRAY NIL (10))

(subtypep (type-of 123) 'unsigned-byte) t
(subtypep (type-of 12345678901234567890) 'unsigned-byte) t

(defstruct (foo (:type list)) a b)
foo
(make-foo :a 123)
(123 NIL)
#+CLISP (multiple-value-list (subtypep 'foo 'list))
#+CLISP (T T)
#+CLISP (multiple-value-list (subtypep 'list 'foo))
#+CLISP (NIL T)

(defstruct (foo (:type list) :named) a nil b)
foo
(let* ((y (make-foo :a 123))
       (z (copy-foo y)))
  (setf (foo-nil z) 321)
  (list y z))
((FOO 123 NIL NIL) (FOO 123 321 NIL))

(progn (mapc #'unintern '(foo70 foo71 foo72 foo73 foo74)) t) t

(defstruct (foo70 (:type (vector (unsigned-byte 8)))) x y)
foo70
(type-of (make-foo70 :x 12 :y 5))
(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (2))
(type-of (make-foo70 :x -1 :y 1))
ERROR
#+CLISP (typep (make-foo70 :x 12 :y 5) 'foo70)
#+CLISP t

(defstruct (foo71 (:type list) (:initial-offset 5)))
foo71
(defstruct (foo72 (:type list) (:initial-offset 2) (:include foo71)))
foo72
(list (length (make-foo71)) (length (make-foo72)))
(5 7)
#+CLISP (typep (make-list 6) 'foo71)
#+CLISP t
#+CLISP (typep (make-list 6) 'foo72)
#+CLISP nil
#+CLISP (multiple-value-list (subtypep '(cons t (cons t (cons t (cons t (cons t (cons t null)))))) 'foo71))
#+CLISP (t t)
#+CLISP (multiple-value-list (subtypep '(cons t (cons t (cons t (cons t (cons t (cons t null)))))) 'foo72))
#+CLISP (nil t)

(defstruct (foo73 (:type list) (:initial-offset 5) :named))
foo73
(defstruct (foo74 (:type list) (:initial-offset 2) :named (:include foo73)))
foo74
(list (length (make-foo73)) (length (make-foo74)))
(6 9)
#+CLISP (typep (list nil nil nil nil nil 'foo73 nil) 'foo73)
#+CLISP t
(foo73-p (list nil nil nil nil nil 'foo73 nil))
t
#+CLISP (typep (list nil nil nil nil nil 'foo73 nil nil 'foo74) 'foo73)
#+CLISP t
(foo74-p (list nil nil nil nil nil 'foo73 nil nil 'foo74))
t
#+CLISP (typep (list* nil nil nil nil nil 'foo73 nil 'tail) 'foo74)
#+CLISP nil
(foo74-p (list* nil nil nil nil nil 'foo73 nil 'tail))
nil
#+CLISP (multiple-value-list (subtypep '(cons t (cons t (cons t (cons t (cons t (cons (eql foo73) null)))))) 'foo73))
#+CLISP (t t)
#+CLISP (multiple-value-list (subtypep '(cons t (cons t (cons t (cons t (cons t (cons (eql foo73) null)))))) 'foo74))
#+CLISP (nil t)

(multiple-value-list (subtypep '(and simple-error type-error) 'error)) (T T)
(multiple-value-list (subtypep '(or simple-error type-error) 'condition)) (T T)

(progn
  (defstruct (foo129a (:type list))
    slot1
    (slot2 t)
    (slot3 (floor pi))
    (slot4 44))
  (defstruct (foo129b (:type list) (:include foo129a (slot4 -44)))
    slot5
    (slot6 t)
    (slot7 (floor (* pi pi)))
    (slot8 88))
  (let ((a (make-foo129b)))
    (list (foo129b-slot1 a)
          (foo129b-slot2 a)
          (foo129b-slot3 a)
          (foo129b-slot4 a)
          (foo129b-slot5 a)
          (foo129b-slot6 a)
          (foo129b-slot7 a)
          (foo129b-slot8 a))))
(nil t 3 -44 nil t 9 88)

(let ((*break-on-signals* t) a)
  (defstruct (foo139 (:predicate is-foo139)) p)
  (setq a (make-foo139 :p 10))
  (list (is-foo139 a) (foo139-p a)))
(T 10)

(let ((*break-on-signals* t) a)
  (defstruct (foo140 (:predicate is-foo140) (:type vector) :named) p)
  (setq a (make-foo140 :p 10))
  (list (is-foo140 a) (foo140-p a)))
(T 10)

(defmacro check-type-error (&body forms)
  `(block nil
     (handler-bind ((type-error
                     (lambda (c)
                       (return
                         (typep (type-error-datum c)
                                (type-error-expected-type c))))))
       ,@forms)))
CHECK-TYPE-ERROR

;; ansi tests GET-PROPERTIES.ERROR.6, GETF.ERROR.4
(check-type-error (GETF '(A . B) 'C))
NIL

(check-type-error (GET-PROPERTIES '(A 1 B 2 C . D) '(X Y)))
NIL

(check-type-error (FBOUNDP #'CAR))
NIL

#+CLISP
(typep '#1=(A 1 B 2 #1#) 'SYS::PLIST)
#+CLISP
NIL

(check-type-error (UNION NIL "A"))
NIL

#+clisp (multiple-value-list (subtypep charset:ucs-4 charset:utf-8)) (T T)
#+clisp (multiple-value-list (subtypep charset:utf-8 charset:ucs-4)) (T T)
