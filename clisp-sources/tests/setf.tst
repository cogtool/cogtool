;; -*- Lisp -*-

(SETf LI1 '(A (B) ((C) (D)) )  VEC1 '#(0 1 2 3))
#(0 1 2 3)

(setf pa 'old)
old

(psetf)
nil

(psetf pa 'new pao pa)
nil

pa
new

pao
old

(SETF (NTH 1 LI1) (QUOTE UU))
UU

(EVAL (QUOTE LI1))
(A UU ((C) (D)))

(SETF (ELT LI1 1) (QUOTE OO))
OO

(SETF (ELT VEC1 1) (QUOTE OO))
OO

(EVAL (QUOTE LI1))
(A OO ((C) (D)))

(EVAL (QUOTE VEC1))
#(0 OO 2 3)

(SETF (REST LI1) (QUOTE ((WW))))
((WW))

(EVAL (QUOTE LI1))
(A (WW))

(SETF (FIRST LI1) (QUOTE AA))
AA

(FIRST LI1)
AA

(SETF (SECOND LI1) (QUOTE BB))
BB

(EVAL (QUOTE LI1))
(AA BB)

(SETF (THIRD LI1) (QUOTE BB))
ERROR

(EVAL (QUOTE LI1))
(AA BB)


(SETF (REST LI1) (QUOTE (2 3 4 5 6 7 8 9 10)))
(2 3 4 5 6 7 8 9 10)

(SETF (SECOND LI1) 22)
22

(EVAL (QUOTE LI1))
(AA 22 3 4 5 6 7 8 9 10)

(SETF (THIRD LI1) (QUOTE 33))
33

(SETF (FOURTH LI1) (QUOTE 44))
44

(SETF (FIFTH LI1) (QUOTE 55))
55

(SETF (SIXTH LI1) (QUOTE 66))
66

(SETF (SEVENTH LI1) (QUOTE 77))
77

(SETF (EIGHTH LI1) (QUOTE 88))
88

(SETF (NINTH LI1) (QUOTE 99))
99

(SETF (TENTH LI1) (QUOTE 1010))
1010

(EVAL (QUOTE LI1))
(AA 22 33 44 55 66 77 88 99 1010)

(SETF (FIRST LI1) (QUOTE (((A)))))
(((A)))

(SETF (CAAAR LI1) (QUOTE UU))
UU

(CAAAR LI1)
UU

(CAR LI1)
((UU))

(SETF (CAAR LI1) (QUOTE OO))
OO

(EVAL (QUOTE LI1))
((OO) 22 33 44 55 66 77 88 99 1010)

(SETF (CAR LI1) (QUOTE II))
II

(EVAL (QUOTE LI1))
(II 22 33 44 55 66 77 88 99 1010)

(SETF (CDDDR LI1) (QUOTE PP))
PP

(EVAL (QUOTE LI1))
(II 22 33 . PP)

(SETF (CADDR LI1) (QUOTE 333))
333

(EVAL (QUOTE LI1))
(II 22 333 . PP)

(SETF (SVREF VEC1 2) (QUOTE KK))
KK

(EVAL (QUOTE VEC1))
#(0 OO KK 3)

(SETF (GET (QUOTE A) (QUOTE B)) (QUOTE UU))
UU

(GET (QUOTE A) (QUOTE B))
UU

(SETF (GETF (CADR (SETQ XX (QUOTE (AAA (I1 V1 I2 V2))))) (QUOTE I2))

(QUOTE V222))
V222

(EVAL (QUOTE XX))
(AAA (I1 V1 I2 V222))

(GETF (CADR XX) (QUOTE I2))
V222

(GETF (CADR XX) (QUOTE I1))
V1

(SETF (DOCUMENTATION (QUOTE BEISPIEL) (QUOTE TYP1)) "doc 1")
"doc 1"

(SETF (DOCUMENTATION (QUOTE BEISPIEL) (QUOTE TYP2)) "doc 2")
"doc 2"

(DOCUMENTATION (QUOTE BEISPIEL) (QUOTE TYP2))
#+XCL (TYP2 . "doc 2") #+SBCL NIL #-(or XCL SBCL) "doc 2"

(SETF (DOCUMENTATION (QUOTE BEISPIEL) (QUOTE TYP2)) "doc 3")
"doc 3"

(DOCUMENTATION (QUOTE BEISPIEL) (QUOTE TYP2))
#+XCL (TYP2 . "doc 3") #+SBCL NIL #-(or XCL SBCL) "doc 3"

(symbol-plist 'beispiel)
#+XCL (DOCUMENTATION ((TYP2 . "doc 3") (TYP1 . "doc 1")))
#+CLISP (SYSTEM::DOC (TYP2 "doc 3" TYP1 "doc 1"))
#+(or GCL) NIL
#+ALLEGRO (EXCL::%DOCUMENTATION ((TYP2 . "doc 3") (TYP1 . "doc 1")))
#+(or CMU SBCL OpenMCL) NIL
#+LISPWORKS (PKG::SYMBOL-NAME-STRING "BEISPIEL")
#-(or XCL CLISP GCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(SETF (SYMBOL-VALUE (QUOTE XX)) (QUOTE VOELLIGNEU))
VOELLIGNEU

(EVAL (QUOTE XX))
VOELLIGNEU

(PROGN
  (SETF (SYMBOL-FUNCTION (QUOTE FF))
        (COERCE (QUOTE (LAMBDA (X) (PRINT X) (QUOTE HELLO))) (QUOTE FUNCTION)))
  NIL)
NIL

(FF 5)
HELLO

(defun xx nil 'a)
xx

(progn (setf (symbol-function 'xx1) (symbol-function 'xx)) nil)
nil

(xx1)
a

(setq l '(a 1 c d))
(a 1 c d)

(setf (the integer (cadr l)) 100)
100

l
(a 100 c d)

(progn (setf a (make-hash-table)) t)
t

(setf (gethash 'color a) 'brown)
brown

(gethash 'color a)
brown

(defstruct schiff masse nil zot)
schiff

(setf s1 (make-schiff :nil 123))
#S(SCHIFF :MASSE NIL :NIL 123 :ZOT NIL)

(schiff-nil s1)  123

(documentation s1 'type)
nil

(setf (schiff-masse s1) 500)
500

(schiff-masse s1)
500

(defmacro setf-test (v) `(svref ,v 3))
setf-test

(progn (setf (macro-function 'setf-test1) (macro-function 'setf-test)) nil)
nil

(setf (setf-test vec1) 'oho)
oho

(eval 'vec1)
#(0 OO KK oho)

(setf (setf-test1 vec1) 'hihi)
hihi

(eval 'vec1)
#(0 OO KK hihi)

; (setf (displace ?? (svref vec1 3)) "aha")
; aha

; (eval 'vec1)
; #(0 oo KK aha)

(progn (setf a (make-array '(4 3))) nil)
nil

(aref a 2 2)
#+(or XCL CMU SBCL OpenMCL) 0 #+(or CLISP AKCL ALLEGRO LISPWORKS) NIL #-(or XCL CLISP AKCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(setf (apply #'aref a '(2 2)) 'xxxx)
xxxx

(aref a 2 2)
xxxx

(SETF (AREF '#(A B C) 1) (QUOTE II))
II

(setf b #*101010)
#*101010

(bit b 2)
1

(setf (bit b 2) 0)
0

(bit b 2)
0

(setf (sbit b 2) 1)
1

(sbit b 2)
1

(progn (setf a (make-array 5 :fill-pointer t)) t)
t

(fill-pointer a)
5

(setf (fill-pointer a) 3)
3

(fill-pointer a)
3

(setf str "hose")
"hose"

(setf (char str 0) #\d)
#\d

str
"dose"

(setf str "aaaxxxccc")
"aaaxxxccc"

(setf (subseq str 3 6) "bbb")
"bbb"

str
"aaabbbccc"

(setq x (list 'a 'b 'c))
(a b c)

(shiftf (cadr x) 'z)
b

x
(a z c)

(shiftf (cadr x) (cddr x) 'q)
z

x
(a (c) . q)

(progn
 (defun ad (x) (values (car x) (cdr x)))
 (defsetf ad (x) (a b) `(setf (values (car ,x) (cdr ,x)) (values ,a ,b)))
 (setq x (cons 1 2) y 3 z 4 w 5 v 6 u 7))
7

(rotatef (ad x) (values y z) (values w v u))
nil

x
(3 . 4)

(list y z w v u)
(5 6 1 2 nil)

(multiple-value-list (shiftf (ad x) (values y z w) (values v u) (floor 89 10)))
(3 4)

x
(5 . 6)

(list y z w v u)
(2 nil nil 8 9)

(progn
  (defsetf my-subseq (sequence start &optional end) (new-sequence)
    `(progn (replace ,sequence ,new-sequence :start1 ,start :end1 ,end)
      ,new-sequence)) t)
t

(setf s "asdfg" (my-subseq s 1 3) "xy")
"xy"

s
"axyfg"

; defsetf supports &environment
(progn
  (defsetf my-subseq-env (sequence start &optional end &environment env) (new-sequence)
    `(progn (replace ,sequence ,new-sequence :start1 ,start :end1 ,end)
      ,new-sequence)) t)
t

; defsetf required arguments are really required
(progn
  (defsetf test-setf-01 (a b &optional c) (newval)
    `(progn (print (list ',a ',b ',c ',newval)) ,newval))
  (setf (test-setf-01) 3))
ERROR

; defsetf lambda-lists don't allow destructuring
(progn
  (defsetf test-setf-02 (a ((b))) (newval))
  t)
ERROR

;; property lists
(setf pl (list 'a 10 'b 11 'c 12 'd 13 'a 14 'b 15 'c 16 'd 17))
(a 10 b 11 c 12 d 13 a 14 b 15 c 16 d 17)

(getf pl 'a)
10

(getf pl 'z 1)
1

(getf pl 'u)
nil

(setf (getf pl 'z) 125)
125

(remf pl 'z)
t

(remf pl 'z)
nil

(remf pl 'c)
t

(getf pl 'c)
16

(remf pl 'c)
t

(remf pl 'c)
nil

(getf pl 'd)
13

(setf (getf pl 'd) 100)
100

(getf pl 'd)
100

(remf pl 'd)
t

(remf pl 'd)
t

(getf pl 'b)
11

pl
(a 10 b 11 a 14 b 15)

;; <http://article.gmane.org/gmane.lisp.clisp.general:7646>
(unintern 'foo) t
#+SBCL (unintern 'copy-foo) #+SBCL t
#+SBCL (unintern 'make-foo) #+SBCL t
#+SBCL (unintern 'foo-a) #+SBCL t
(unintern 'bar) t
(unwind-protect
     (let ((forms
            '((defstruct foo a b)
              (defstruct (bar (:include foo) (:conc-name foo-)) c)
              (defun quux (x) (foo-a x))
              (defun frobozz (x y) (setf (foo-a x) y))
              (list (quux (make-foo :a 1))
               (quux (make-bar :a 2))
               (frobozz (make-foo) 10)
               (frobozz (make-bar) 20)))))
       (list (eval `(progn ,@forms))
             (funcall (compile nil `(lambda () ,@forms)))))
  (fmakunbound 'quux)
  (fmakunbound 'frobozz)
  (fmakunbound 'foo-a) (fmakunbound 'foo-b) (fmakunbound 'foo-c))
((1 2 10 20) (1 2 10 20))

(unwind-protect
     (progn
       (defstruct (foo (:type list)) a b c)
       (funcall
        (compile nil (lambda (f) (setf (foo-c f) (+ (foo-a f) (foo-b f))) f))
        (make-foo :a 10 :b 100)))
  (fmakunbound 'foo-a) (fmakunbound 'foo-b) (fmakunbound 'foo-c))
(10 100 110)

;; Check that the compiler can inline (setf foo) functions.
(progn
  (proclaim '(inline (setf foo21)))
  (defun (setf foo21) (x y) (+ x y))
  (defun bar21 (x y) ((setf foo21) x y))
  (compile 'bar21)
  (defun (setf foo21) (x y) (error "Not inlined"))
  (bar21 1 2))
3

;; <http://article.gmane.org/gmane.lisp.clisp.general:9034>
(defsetf foo22 () () '(values))
FOO22

;; Check that dotted lists are rejected.
(get-setf-expansion '(nth x y . z))
ERROR

(get-setf-expansion '(ldb x . y))
ERROR

(get-setf-expansion '(ldb x y . z))
ERROR

;; Check that some macroexpansions are as optimized as they can be.
;; This is important for good bytecode generation.

(macroexpand-1 '(push (foo) l))
(SETQ L (CONS (FOO) L))

(macroexpand-1 '(pop l))
(PROG1 (CAR L) (SETQ L (CDR L)))

#|
(macroexpand-1 '(psetf l (foo)))
(PROGN (SETQ L (FOO)) NIL)
|#

(macroexpand-1 '(pushnew (foo) l))
(SETQ L (ADJOIN (FOO) L))

(macroexpand-1 '(incf x))
(SETQ X (+ X 1))

(macroexpand-1 '(setf l (foo)))
(SETQ L (FOO))

#|
(macroexpand-1 '(setf (values l) (foo)))
(VALUES (SETQ L (FOO)))
|#

(macroexpand-1 '(setf (values-list l) (foo)))
#-LISPWORKS (VALUES-LIST (SETF L (MULTIPLE-VALUE-LIST (FOO))))
#+LISPWORKS ERROR

;; Check that the PUSH macroexpander doesn't blindly call subst or sublis.

(define-setf-expander bothvars (x y)
  (let ((g (gensym)))
    (values '() '() (list g) `(progn (setq ,x ,g ,y ,g)) x)))
BOTHVARS
(let (a b)
  (setf (bothvars a b) '())
  (push (make-array 2) (bothvars a b))
  (eq a b))
T

;; Check that DOCUMENTATION's value from different anonymous lambdas are
;; independent.

(setf (documentation (lambda () 'abazonk) 'function) "abazonk doc")
"abazonk doc"

(documentation (lambda () 'bazonk) 'function)
NIL

(setf (documentation (compile nil (lambda () 'abazonk)) 'function)
      "abazonk doc compiled")
"abazonk doc compiled"

(documentation (compile nil (lambda () 'bazonk)) 'function)
NIL

;; Check that DOCUMENTATION on anonymous lambdas works.

(documentation (lambda () "interpreted anonymous doc" 42) 'function)
"interpreted anonymous doc"

#| NYI
 (documentation (compile nil (lambda () "compiled anonymous doc" 42)) 'function)
"compiled anonymous doc"
|#

;; Check that (SETF DOCUMENTATION) on anonymous lambdas works.

(let ((f (lambda () "interpreted anonymous doc" 42)))
  (setf (documentation f 'function) "new doc")
  (documentation f 'function))
"new doc"

#| NYI
 (let ((f (compile nil (lambda () "interpreted anonymous doc" 42))))
  (setf (documentation f 'function) "new doc")
  (documentation f 'function))
"new doc"
|#

;; Check that DOCUMENTATION and SETF DOCUMENTATION work on the function object.

(progn
  (defun func01 () "interpreted doc" 42)
  (let ((old-func #'func01)
        (new-func (lambda () "new interpreted doc" 43)))
    (setf (fdefinition 'func01) new-func)
    (list* (documentation old-func 'function)
           (documentation new-func 'function)
           (documentation 'func01 'function)
           (progn
             (setf (documentation 'func01 'function) "replaced doc")
             (list
               (documentation old-func 'function)
               (documentation new-func 'function)
               (documentation 'func01 'function))))))
("interpreted doc" "new interpreted doc" "new interpreted doc"
 "interpreted doc" "replaced doc" "replaced doc")

#| NYI
 (progn
  (defun func02 () "compiled doc" 42)
  (let ((old-func #'func02)
        (new-func (compile nil (lambda () "new compiled doc" 43))))
    (setf (fdefinition 'func02) new-func)
    (list* (documentation old-func 'function)
           (documentation new-func 'function)
           (documentation 'func02 'function)
           (progn
             (setf (documentation 'func02 'function) "replaced doc")
             (list
               (documentation old-func 'function)
               (documentation new-func 'function)
               (documentation 'func02 'function))))))
 ("compiled doc" "new compiled doc" "new compiled doc"
 "compiled doc" "replaced doc" "replaced doc")
|#

(progn
  (defmacro func03 () "macro doc" 42)
  (let ((old-func (macro-function 'func03))
        (new-func (lambda (form env) "new macro doc" 43)))
    (setf (macro-function 'func03) new-func)
    (list* (documentation old-func 'function)
           (documentation new-func 'function)
           (documentation 'func03 'function)
           (progn
             (setf (documentation 'func03 'function) "replaced doc")
             (list
               (documentation old-func 'function)
               (documentation new-func 'function)
               (documentation 'func03 'function))))))
("macro doc" "new macro doc" "new macro doc"
 "macro doc" "replaced doc" "replaced doc")

;; user may pass env=NIL to get-setf-expansion to mean null lexical environment
(length (multiple-value-list (get-setf-expansion '(x) nil)))
5

; Clean up.
(unintern 'x)
T
