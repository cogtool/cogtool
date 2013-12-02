;; -*- Lisp -*-
;;*****************************************************************************
;;*                    short test      XCL                                    *
;;*****************************************************************************

;; Chapter 1  Introduction
;; -----------------------

;; Chapter 2  Data Types
;; ---------------------

;; Chapter 3  Valid Values
;; -----------------------

;; Chapter 4  Type specifiers
;; --------------------------
;;
;; deftype, COERCE, TYPE-OF

;; Chapter 5  Program Structure
;; ----------------------------

;; lambda lists
((LAMBDA (A B) (+ A (* B 3))) 4 5)
19

((LAMBDA (A &OPTIONAL (B 2)) (+ A (* B 3))) 4 5)
19

((LAMBDA (&OPTIONAL (A 2 B) (C 3 D) &REST X) (LIST A B C D X)))
(2 NIL 3 NIL NIL)

((LAMBDA (A B &KEY C D) (LIST A B C D)) 1 2)
(1 2 NIL NIL)

((LAMBDA (A &OPTIONAL (B 3) &REST X &KEY C (D A)) (LIST A B C D X))
1)
(1 3 NIL 1 NIL)

((LAMBDA (X &AUX (A 3) (B 4)) (+ X (* A B))) 2)
14

((LAMBDA (X Y &OPTIONAL A B &REST Z &KEY C (D Y) &AUX (U 3) (V 4))

(+ X Y A (* B (CAR Z)) C (* D U) V)) 3 4 5 2 7 :C 6 :D 8)
ERROR

((LAMBDA (X Y) ((LAMBDA (A B) (LIST A B)) (QUOTE U) (QUOTE V))) 5 6)

(U V)

((LAMBDA (X &ALLOW-OTHER-KEYS) (LIST X Y)) 2 :Y 3)
ERROR

lambda-list-keywords
#+XCL (&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX &BODY &WHOLE SYSTEM::&ENVIRONMENT)
#+CLISP (&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX &BODY &WHOLE &ENVIRONMENT)
#+AKCL (&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX &WHOLE &ENVIRONMENT &BODY)
#+(or ALLEGRO CMU) (&OPTIONAL &REST &KEY &AUX &BODY &WHOLE &ALLOW-OTHER-KEYS &ENVIRONMENT #+CMU19 EXT:&PARSE-BODY #+CMU19 C:&MORE)
#+SBCL (&ALLOW-OTHER-KEYS &AUX &BODY &ENVIRONMENT &KEY SB-INT:&MORE &OPTIONAL &REST &WHOLE)
#+OpenMCL (&OPTIONAL &REST &AUX &KEY &ALLOW-OTHER-KEYS &BODY &ENVIRONMENT &WHOLE)
#+LISPWORKS (&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX &STACK &BODY &WHOLE &ENVIRONMENT)
#-(or XCL CLISP AKCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(let ((s (prin1-to-string LAMBDA-PARAMETERS-LIMIT )))
  (or #+XCL (equal s "128")
      #+CLISP (equal s "65536") #+CLISP (equal s "4294967296") #+CLISP (equal s "4096")
      #+AKCL (equal s "64")
      #+ALLEGRO (equal s "16384")
      #+(or CMU SBCL) (equal s "536870911")
      #+OpenMCL (equal s "8192")
      #+LISPWORKS (equal s "255")
) )
T

;DEFVAR, DEFCONSTANT, DEFPARAMETER, eval-when

;; Chapter 6  Predicates
;; ---------------------

(TYPEP (QUOTE NIL) (QUOTE NULL))
T

(TYPEP (QUOTE (A B C)) (QUOTE NULL))
NIL

(TYPEP (QUOTE ABC) (QUOTE SYMBOL))
T

(TYPEP 4 (QUOTE ATOM))
T

(TYPEP 55 (QUOTE CONS))
NIL

(TYPEP (QUOTE (A (B C))) (QUOTE LIST))
T

(TYPEP 5/8 (QUOTE NUMBER))
T

(TYPEP -800 (QUOTE INTEGER))
T

(TYPEP 5/7 (QUOTE RATIONAL))
T

(TYPEP 2.718 (QUOTE FLOAT))
T

(TYPEP #C(1.23 3.56) (QUOTE FLOAT))
NIL

(TYPEP #\a (QUOTE CHARACTER))
T

(TYPEP "abc" (QUOTE STRING))
T

(TYPEP '#(1 2 3) (QUOTE STRING))
NIL

(TYPEP '#(A B C) (QUOTE BIT-VECTOR))
NIL

(TYPEP '#(A B C) (QUOTE VECTOR))
T

(TYPEP "abc" (QUOTE VECTOR))
T

(TYPEP '#(1 2 3 4) (QUOTE SIMPLE-VECTOR))
T

(TYPEP 3 (QUOTE SIMPLE-VECTOR))
NIL

(TYPEP "a b cd" (QUOTE SIMPLE-STRING))
T

(TYPEP (QUOTE ABC) (QUOTE SIMPLE-STRING))
NIL

(TYPEP #*1101 (QUOTE SIMPLE-BIT-VECTOR))
T

(TYPEP '#(1 0 0 1) (QUOTE SIMPLE-BIT-VECTOR))
NIL

(TYPEP '#2A((A B)(C D)) (QUOTE ARRAY))
T

(SETQ X 7)
7

(TYPEP X (QUOTE COMPILED-FUNCTION))
NIL

(TYPEP X (QUOTE COMMON))
ERROR

(SUBTYPEP (QUOTE CHARACTER) (QUOTE NUMBER))
NIL

(SUBTYPEP (QUOTE NUMBER) (QUOTE CHARACTER))
NIL

(SUBTYPEP (QUOTE STRING) (QUOTE NUMBER))
NIL

(SUBTYPEP (QUOTE COMPLEX) (QUOTE NUMBER))
T

(SUBTYPEP (QUOTE FLOAT) (QUOTE NUMBER))
T

(SUBTYPEP (QUOTE FIXNUM) (QUOTE NUMBER))
T

(SUBTYPEP (QUOTE RATIONAL) (QUOTE NUMBER))
T

(SUBTYPEP (QUOTE FLOAT) (QUOTE COMPLEX))
NIL

(SUBTYPEP (QUOTE INTEGER) (QUOTE RATIONAL))
T

(SUBTYPEP (QUOTE NUMBER) (QUOTE VECTOR))
NIL

(SUBTYPEP (QUOTE VECTOR) (QUOTE ARRAY))
T

(SUBTYPEP (QUOTE NUMBER) (QUOTE ARRAY))
NIL

(NULL (QUOTE NIL))
T

(SYMBOLP *STANDARD-INPUT*)
NIL

(SYMBOLP (QUOTE CAR))
T

(ATOM (QUOTE ABC))
T

(CONSP (ACONS (QUOTE X) (QUOTE Y) (QUOTE A)))
#+XCL ERROR
#+(or CLISP AKCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) T
#-(or XCL CLISP AKCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(LISTP (QUOTE (((A . B) . C))))
T

(LISTP 'A)
NIL

(LISTP NIL)
T

(LISTP '(A B C))
T

(NUMBERP #*101)
NIL

(NUMBERP -5)
T

(INTEGERP 5)
T

(INTEGERP #\+)
NIL

(RATIONALP 0)
T

(FLOATP -5)
NIL

(FLOATP (READ-FROM-STRING "1.0e30"))
T

(FLOATP 123.4)
T

(COMPLEXP 1/2)
NIL

(COMPLEXP #C(2 3))
T

(CHARACTERP #\1)
T

(STRINGP "abc")
T

(STRINGP :+*/-)
NIL

(BIT-VECTOR-P (READ-FROM-STRING "#5*01110"))
T

(VECTORP "abc")
T

(SIMPLE-VECTOR-P #*101)
NIL

(SIMPLE-STRING-P "abc")
T

(SIMPLE-STRING-P :+*/-)
NIL

(SIMPLE-BIT-VECTOR-P #*101)
T

(ARRAYP (READ-FROM-STRING "#7(2 4 3)"))
T

(ARRAYP (QUOTE (READ-FROM-STRING "#1a 5.77")))
NIL

(PACKAGEP (READ-FROM-STRING "#5*01110"))
NIL

(PACKAGEP *PACKAGE*)
T

(FUNCTIONP (QUOTE ATOM))
#-(or CLtL2 ANSI-CL CLISP) T #+(or CLtL2 ANSI-CL CLISP) NIL

(COMPILED-FUNCTION-P (QUOTE DO))
NIL

;COMMONP

(EQ (QUOTE (1 2 3 4 5)) (COPY-LIST (QUOTE (1 2 3 4 5))))
NIL

(SETQ X '((1 . A) (2 . B) (3 . C)) )
((1 . A) (2 . B) (3 . C))

(EQ (CADR X) (CADR (COPY-ALIST X)))
NIL

(EQ #\A #\A)
T

(EQ "Foo" "Foo")
NIL

(EQ "Foo" (COPY-SEQ "Foo"))
NIL

(EQL #C(3.0 -4.0) #C(3 -4))
nil

(EQL (CONS (QUOTE A) (QUOTE B)) (CONS (QUOTE A) (QUOTE C)))
NIL

(EQUAL (QUOTE (1 2 3 4 5)) (COPY-LIST (QUOTE (1 2 3 4 5))))
T

(EQUAL X (COPY-ALIST X))
T

(EQUAL 3 3)
T

(EQUAL 3 3.0)
NIL

(EQUAL 3.0 3.0)
T

(EQUAL #C(3 -4) #C(3 -4))
T

(EQUALP (QUOTE (1 2 3 4 5)) (COPY-LIST (QUOTE (1 2 3 4 5))))
T

(EQUALP "            foo" "            FOO")
T

(EQUALP "            fou" "            FOO")
NIL

(EQUALP '(0 1) '(#P""))   NIL

(NOT 1)
NIL

(NOT NIL)
T

(AND (EQ 1 2) (EQ 2 3) (EQ 3 4) (EQ 4 4))
NIL

(AND (EQ 1 2) (EQ 3 3) (EQ 3 4) (EQ 4 4))
NIL

(OR (EQ 2 2) (EQ 3 3) (EQ 3 4) (EQ 4 4))
T

(OR (EQ 1 2) (EQ 2 3) (EQ 3 4) (EQ 4 5))
NIL

;Kap 7 KONTROLLSTRUCTUREN
;-------------------------------------------------------------------------------

; quote, FUNCTION, SYMBOL-VALUE, SYMBOL-FUNCTION, BOUNDP, FBOUNDP,
; SPECIAL-FORM-P, SETQ, PSETQ, SET, MAKUNBOUND, FMAKUNBOUND,

(SETQ LI1 (QUOTE (A (B) ((C) (D)))))
(A (B) ((C) (D)))

(SETQ VEC1 '#(0 1 2 3))
#(0 1 2 3)

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
#+XCL (TYP2 . "doc 2") #+(or SBCL LISPWORKS) NIL #-(or XCL SBCL LISPWORKS) "doc 2"

(SETF (DOCUMENTATION (QUOTE BEISPIEL) (QUOTE TYP2)) "doc 3")
"doc 3"

(DOCUMENTATION (QUOTE BEISPIEL) (QUOTE TYP2))
#+XCL (TYP2 . "doc 3") #+(or SBCL LISPWORKS) NIL #-(or XCL SBCL LISPWORKS) "doc 3"

(symbol-plist 'beispiel)
#+XCL (DOCUMENTATION ((TYP2 . "doc 3") (TYP1 . "doc 1")))
#+ALLEGRO (EXCL::%DOCUMENTATION ((TYP2 . "doc 3") (TYP1 . "doc 1")))
#+LISPWORKS (PKG::SYMBOL-NAME-STRING "BEISPIEL")
#+CLISP (SYSTEM::DOC (TYP2 "doc 3" TYP1 "doc 1"))
#+(or CMU SBCL OpenMCL) NIL
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(SETF (SYMBOL-VALUE (QUOTE XX)) (QUOTE VOELLIGNEU))
VOELLIGNEU

(EVAL (QUOTE XX))
VOELLIGNEU

;PSETF, SHIFTF, ROTATEF, DEFINE-MODIFY-MACRO, DEFSETF, DEFINE-SETF-METHOD,
;GET-SETF-METHOD, GET-SETF-METHOD-MULTIPLE-VALUE, APPLY, FUNCALL, PROGN,
;PROG1, PROG2,

(LET ((X (LIST (QUOTE A) (QUOTE B) (QUOTE C)))) (RPLACD (LAST X) X)

(LIST-LENGTH X))
NIL

;LET*, COMPILER-LET, PROGV, FLET, LABELS, MACROLET, IF, WHEN, UNLESS, COND,
;CASE, TYPECASE, BLOCK, LOOP, DO, DO*, DOLIST, DOTIMES,

(MAPCAR (FUNCTION (LAMBDA (X) (LIST X))) (QUOTE (A B C)))
((A) (B) (C))

(MAPC (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
(1 2 3))
(QUOTE (U I V)))
(A B C)

(MAPL (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
(1 2 3))
(QUOTE (U I V)))
(A B C)

(MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C))
(QUOTE (1 2 3)) (QUOTE (U I V)))
(((A B C) (1 2 3) (U I V)) ((B C) (2 3) (I V)) ((C) (3) (V)))

(MAPCON (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B)) (QUOTE
(1 2 3))
(QUOTE (U I V)))
((A B) (1 2 3) (U I V) (B) (2 3) (I V))

(MAPCAN (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
(1 2 3))
(QUOTE (U I V)))
(A 1 U B 2 I C 3 V)

(funcall (compile nil (lambda (x) (flet ((foo (y) (+ y 1))) (foo (* 2 x))))) 3)
7

;; <http://www.lisp.org/HyperSpec/Body/speope_progv.html>
(let ((not-a-globally-special-var 3))
  (progv '(not-a-globally-special-var) '(4)
    (list not-a-globally-special-var
          (symbol-value 'not-a-globally-special-var))))
(3 4)

;; <http://www.lisp.org/HyperSpec/Body/fun_funcall.html>
(flet ((cons (x y) `(kons ,x ,y)))
  (let ((cons (symbol-function '+)))
    (funcall #'cons
             (funcall 'cons 1 2)
             (funcall cons 1 2))))
(KONS (1 . 2) 3)

(or #+win32 (string= "g++" (software-type) :end2 3)
    (let* ((n (min lambda-parameters-limit 1024))
           (vars (loop repeat n collect (gensym))))
      (eval
       `(= ,n (flet ((%f ,vars (+ ,@vars)))
                (%f ,@(loop for e in vars collect 1)))))))
T

;TAGBODY, GO, MULTIPLE-VALUE-LIST, MULTIPLE-VALUE-CALL, MULTIPLE-VALUE-PROG1,
;MULTIPLE-VALUE-BIND, MULTIPLE-VALUE-SETQ, VALUES, VALUES-LIST, CATCH,

(let ((ls (loop
             for i from 1 to (min (1- multiple-values-limit) 100000)
             collect i)))
  (equal ls (multiple-value-list (values-list ls))))
t

;UNWIND-PROTECT, THROW,

;Kap 8 MACROS
;-------------------------------------------------------------------------------

;MACRO-FUNCTION, DEFMACRO, MACROEXPAND, MACROEXPAND-1,

;Kap 9 DECLARATIONEN
;-------------------------------------------------------------------------------

;DECLARE, LOCALLY, PROCLAIM, THE,

;Kap 10 SYMBOLE
;-------------------------------------------------------------------------------

;GET, REMPROP, SYMBOL-PLIST, GETF, REMF, GET-PROPERTIES, SYMBOL-NAME,

;MAKE-SYMBOL, COPY-SYMBOL, GENSYM, GENTEMP, SYMBOL-PACKAGE,

(KEYWORDP 36)
NIL

(KEYWORDP :RENAME)
T

;Kap 11 PAKETE
;-------------------------------------------------------------------------------

;FIND-PACKAGE, IN-PACKAGE, LIST-ALL-PACKAGES, MAKE-PACKAGE, PACKAGE-NAME,
;PACKAGE-NICKNAMES, PACKAGE-SHADOWING-SYMBOLS, PACKAGE-USE-LIST,
;PACKAGE-USED-BY-LIST, RENAME-PACKAGE, UNUSE-PACKAGE, USE-PACKAGE, INTERN,
;UNINTERN, FIND-SYMBOL, EXPORT, UNEXPORT, IMPORT, SHADOWING-IMPORT, SHADOW,
;FIND-ALL-SYMBOLS, DO-SYMBOLS, DO-EXTERNAL-SYMBOLS, DO-ALL-SYMBOLS,
;PROVIDE, REQUIRE,

;Kap 12 ZAHLEN
;-------------------------------------------------------------------------------

(ZEROP -456)
NIL

(ZEROP 0)
T

(PLUSP 3)
T

(PLUSP 3453786543987565)
T

(MINUSP -456)
T

(ODDP -1)
T

(ODDP 0)
NIL

(EVENP -456)
T

(EVENP -345)
NIL

(= 5/2 2.5)
T

(/= 3.0 3)
NIL

(/= 3.0 #C(3.0 1.0))
T

(< 3.0 3)
NIL

(< 3 3.0 3 #C(3.0 0.0))
#+(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) NIL
#-(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) ERROR

(< -5 -4 -2 0 4 5)
T

(> 8 7 6 5 4)
T

(> 3 3.0 3 #C(3.0 0.0))
#+(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) NIL
#-(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) ERROR

(<= 3.0 3)
T

(<= 3 3)
T

(<= 1 3 3 2 5)
NIL

(<= 5/2 2.5)
T

(>= -5 -4 -2 0 4 5)
NIL

(MAX 1 3 2 -7)
3

;MIN,

(+ 1 1/2 0.5 #C(3.0 5.5))
#C(5.0 5.5)

(- 3 0 3 5 -6)
1

(- #C(0 6) 1/4 0.5 7)
#C(-7.75 6.0)

(* 7 6 5 4 3 2 1)
5040

(* 2 2 2.0 2)
16.0

(/ -8)
-1/8

(/ 4 2)
2

(1+ 0)
1

(1+ #C(0 1))
#C(1 1)

(1- 5.0)
4.0

;INCF, DECF,

(CONJUGATE #C(3/5 4/5))
#C(3/5 -4/5)

(GCD 91 -49)
7

(LCM 14 35)
70

(PRIN1-TO-STRING (EXP 1) )
"2.7182817" ; "2.718282"

(EXPT #C(0 1) 2)
-1

(PRIN1-TO-STRING (EXPT 2 #C(0 1)) )
"#C(0.7692389 0.63896126)" ; "#C(0.7692389 0.6389612)"

(PRIN1-TO-STRING (LOG -3 10) )
"#C(0.47712126 1.3643764)" ; "#C(0.4771213 1.364376)"

(LOG 3 0)
#+(or XCL CMU OpenMCL LISPWORKS) 0 #+(or ALLEGRO SBCL) 0.0 #-(or XCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) ERROR

(SQRT 9)
3.0

(SQRT -9.0)
#C(0.0 3.0)

(ISQRT 9)
3

(ISQRT 26)
5

(ABS 6)
6

(ABS -6)
6

;PHASE,

(SIGNUM 0)
0

(SIGNUM -4)
-1

(SIGNUM 4)
1

(PRIN1-TO-STRING (SIN (* 8 (/ PI 2))) )
#+XCL "-4.576950980887866D-17"
#+CLISP "2.0066230454737344098L-19"
#+(or AKCL LISPWORKS) "-4.898425415289509E-16"
#+(or ALLEGRO CMU SBCL) "-4.898425415289509d-16"
#+OpenMCL "-4.898587196589413D-16"
#-(or XCL CLISP AKCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(let* ((fudge 2)
       (val (tan (/ pi 2)))
       (ref (cos (/ pi 2)))
       (rel-err (abs (/ (- (/ val) ref) ref))))
  (< rel-err (* fudge long-float-epsilon)))
t

(PRIN1-TO-STRING (SIN (EXPT 10 3)) )
"0.82687956" ; "0.8268796"

(COS 0)
1.0

(PRIN1-TO-STRING (COS (/ PI 2)) )
#+XCL "5.721188726109832D-18"
#+CLISP "-2.5082788068421680123L-20"
#+AKCL "6.1230317691118863E-17"
#+(or ALLEGRO CMU SBCL) "6.123031769111886d-17"
#+OpenMCL "6.123233995736766D-17"
#+LISPWORKS "6.123031769111886E-17"
#-(or XCL CLISP AKCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(PRIN1-TO-STRING (TAN 1) )
"1.5574077" ; "1.557408"

(PRIN1-TO-STRING (TAN (/ PI 2)) )
#+XCL "1.747888503373944D17"
#+CLISP "-3.9867976290042641156L19"
#+(or AKCL LISPWORKS) "1.6331778728383844E16"
#+(or ALLEGRO CMU SBCL) "1.6331778728383844d+16"
#+OpenMCL "1.633123935319537D+16"
#-(or XCL CLISP AKCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(prin1-to-string (CIS -1) )
"#C(0.5403023 -0.84147096)" ; "#C(0.5403023 -0.8414709)"

(CIS 2.5)
#C(-0.8011436 0.5984721)

(prin1-to-string (ASIN -1) )
"-1.5707964" ; "-1.570796"

(ASIN 0)
0.0

(ASIN 2)
#+(or CMU SBCL) #C(1.5707964 -1.3169578)
#-(or CMU SBCL) #C(1.5707964 -1.316958)

(prin1-to-string (ACOS 0) )
"1.5707964" ; "1.570796"

(prin1-to-string (ACOS -1) )
"3.1415927" ; "3.141593"

(prin1-to-string (ACOS 2) )
#+XCL "#C(0.0 1.316958)" #+CLISP "#C(0 1.316958)" #+(or ALLEGRO OpenMCL) "#c(0.0 1.316958)"
#+(or CMU SBCL) "#C(0.0 1.3169578)"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(ACOS 1.00001)
#+ganz-korrekt #C(0.0 0.0044721322)
#+XCL #C(0.0 0.004475157)
#+CLISP-korrekt #C(0.0 0.0044751678) ; da schon 1.00001 gerundet wurde
#+CLISP         #C(0.0 0.0044751023) ; i * ln(x+sqrt(x^2-1))
#+CLISP-anders  #C(0.0 0.0044752206) ; i * ln(x+sqrt((x-1)*(x+1)))
#+(or ALLEGRO SBCL) #C(0.0 0.004475168)
#+CMU #C(0.0 0.0044751678)
#+OpenMCL #C(0.0 0.0044751023)
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL) #C(0.0 0.0044721322)

(ATAN 1)
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 0.7853982 #+CLISP 0.7853981
#-(or XCL ALLEGRO CLISP CMU SBCL OpenMCL) UNKNOWN

(prin1-to-string PI )
#+(or XCL OpenMCL) "3.141592653589793D0"
#+CLISP "3.1415926535897932385L0"
#+(or ALLEGRO CMU SBCL) "3.141592653589793d0"
#+LISPWORKS "3.141592653589793"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(SINH 0)
0.0

(prin1-to-string (SINH #C(5.0 -9.6)) )
#+(or CMU SBCL) "#C(-73.06699 12.936809)"
#-(or CMU SBCL) "#C(-73.06699 12.93681)"

(COSH 0)
1.0

(prin1-to-string (COSH 1) )
"1.5430807"

(TANH 50)
1.0

(prin1-to-string (TANH 0.00753) )
#-ALLEGRO "0.0075298576" #+ALLEGRO "0.0075298795" ; "0.007529857"

(prin1-to-string (ASINH 0.5) )
#-(or ALLEGRO CMU SBCL OpenMCL) "0.48121184" #+(or ALLEGRO CMU SBCL OpenMCL) "0.4812118" ; "0.4812118"

(prin1-to-string (ASINH 3/7) )
#-(or CLISP ALLEGRO CMU SBCL OpenMCL) "0.4164308"
#+CLISP "0.4164307" ; Rundungsfehler
#+(or ALLEGRO CMU SBCL OpenMCL) "0.41643077"

(ACOSH 0)
#C(0 1.5707964)

(ACOSH 1)
0

(ACOSH -1)
#C(0 3.1415927)

(prin1-to-string (ATANH 0.5) )
"0.54930615" ; "0.5493061"

(prin1-to-string (ATANH 3/7) )
#-(or CLISP ALLEGRO CMU SBCL OpenMCL) "0.4581454"
#+CLISP "0.4581453" ; Rundungsfehler
#+(or ALLEGRO CMU SBCL OpenMCL) "0.45814538"

(= (SIN (* #C(0 1) 5)) (* #C(0 1) (SINH 5)))
T

(= (COS (* #C(0 1) 5)) (COSH 5))
T

(= (TAN (* #C(0 1) 5)) (* #C(0 1) (TANH 5)))
T

(= (SINH (* #C(0 1) 5)) (* #C(0 1) (SIN 5)))
T

(= (COSH (* #C(0 1) 5)) (COS 5))
T

(= (TANH (* #C(0 1) 5)) (* #C(0 1) (TAN 5)))
T

(FLOAT 1)
1.0

(FLOAT 0.5)
0.5

(RATIONAL 2)
2

(RATIONAL 2.0)
2

(RATIONAL 2.5)
5/2

(RATIONALIZE 2.5)
5/2

(RATIONALIZE 7/3)
7/3

(RATIONALIZE PI)
#+XCL 28296953155597409/9007199254740992
#+CLISP 8717442233/2774848045
#+(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) 245850922/78256779
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(NUMERATOR 5/2)
5

(NUMERATOR (/ 8 -6))
-4

(DENOMINATOR 5/2)
2

(DENOMINATOR (/ 8 -6))
3

(GCD (NUMERATOR 7/9) (DENOMINATOR 7/9))
1

(FLOOR 2.6)
2

(FLOOR 2.5)
2

(CEILING 2.6)
3

(CEILING 2.5)
3

(CEILING 2.4)
3

(TRUNCATE 2.6)
2

(TRUNCATE 2.5)
2

(TRUNCATE 2.4)
2

(ROUND 2.6)
3

(ROUND 2.5)
2

(ROUND 2.4)
2

(MOD 13 4)
1

(MOD -13 4)
3

(prin1-to-string (REM 13.4 1) )
#-(or CLISP ALLEGRO CMU SBCL OpenMCL) "0.4" ; #+XCL "0.3999996"
#+(or CLISP ALLEGRO CMU SBCL OpenMCL) "0.39999962" ; Rundungsfehler

(FFLOOR 2.6)
2

(FFLOOR 2.5)
2

(FFLOOR 2.4)
2

(FCEILING -0.3)
0

(FCEILING -0.7)
0

(FCEILING -2.4)
-2

(FTRUNCATE 2.5)
2.0

(FTRUNCATE 2.4)
2.0

(FROUND -0.7)
-1.0

(FROUND -2.4)
-2.0

(DECODE-FLOAT 35.0)
0.546875

(DECODE-FLOAT 3.5S0)
0.875S0

(SCALE-FLOAT 2.5 5)
80.0

(SCALE-FLOAT 0.7541 2)
3.0164

(FLOAT-RADIX 2.5)
2

(FLOAT-RADIX 3.5D0)
2

;FLOAT-DIGITS, FLOAT-PRECISION, FLOAT-SIGN, INTEGER-DECODE-FLOAT,

(COMPLEX 1/4 7.3)
#C(0.25 7.3)

(COMPLEX 1 0)
1

(REALPART 5)
5

(REALPART #C(1.4 0.0))
1.4

(IMAGPART 5)
0

(IMAGPART #C(1.4 0.0))
0.0

;LOGAND, LOGANDC1, LOGANDC2, LOGEQV, LOGIOR, LOGNAND, LOGNOR, LOGNOT,
;LOGORC1, LOGORC2, LOGTEST, LOGXOR, LOGBITP, ASH,

(LOGCOUNT 13)
3

(LOGCOUNT -13)
2

(INTEGER-LENGTH 0)
0

(INTEGER-LENGTH 1)
1

;BYTE, BYTE-POSITION, BYTE-SIZE, LDB, LDB-TEST, MASK-FIELD, DPB, DEPOSIT-FIELD,

;RANDOM,

#+XCL (RANDOM-STATE-P
(EVAL (READ-FROM-STRING "(sys::%set-type-pointer sys::%type-random-state 1)")))
#+XCL T

;MAKE-RANDOM-STATE,

BOOLE-CLR
0

BOOLE-SET
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 1 #+CLISP 15 #+LISPWORKS 15 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-1
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 2 #+CLISP 10 #+LISPWORKS 3 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-2
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 3 #+CLISP 12 #+LISPWORKS 5 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-C1
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 4 #+CLISP 5 #+LISPWORKS 12 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-C2
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 5 #+CLISP 3 #+LISPWORKS 10 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-AND
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 6 #+CLISP 8 #+LISPWORKS 1 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-IOR
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 7 #+CLISP 14 #+LISPWORKS 7 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-XOR
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 8 #+CLISP 6 #+LISPWORKS 6 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-EQV
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 9 #+CLISP 9 #+LISPWORKS 9 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-NAND
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 10 #+CLISP 7 #+LISPWORKS 14 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-NOR
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 11 #+CLISP 1 #+LISPWORKS 8 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-ANDC1
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 12 #+CLISP 4 #+LISPWORKS 4 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-ANDC2
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 13 #+CLISP 2 #+LISPWORKS 2 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-ORC1
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 14 #+CLISP 13 #+LISPWORKS 13 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

BOOLE-ORC2
#+(or XCL ALLEGRO CMU SBCL OpenMCL) 15 #+CLISP 11 #+LISPWORKS 11 #-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(let ((s (prin1-to-string MOST-POSITIVE-FIXNUM )))
  (or #+(or XCL CLISP) (equal s "16777215")
      #+CLISP (equal s "33554431") #+CLISP (equal s "67108863")
      #+CLISP (equal s "4294967295") #+CLISP (equal s "1099511627775")
      #+CLISP (equal s "281474976710655")
      #+(or ALLEGRO CMU SBCL OpenMCL) (equal s "536870911")
      #+LISPWORKS (equal s "8388607")
) )
T

(let ((s (prin1-to-string MOST-NEGATIVE-FIXNUM )))
  (or #+(or XCL CLISP) (equal s "-16777216")
      #+CLISP (equal s "-33554432") #+CLISP (equal s "-67108864")
      #+CLISP (equal s "-4294967296") #+CLISP (equal s "-1099511627776")
      #+CLISP (equal s "-281474976710656")
      #+(or ALLEGRO CMU SBCL OpenMCL) (equal s "-536870912")
      #+LISPWORKS (equal s "-8388608")
) )
T

(prin1-to-string MOST-POSITIVE-SHORT-FLOAT )
#+XCL "1.701S38"
#+CLISP "3.4028s38"
#+ALLEGRO "3.4028232e+38"
#+(or CMU SBCL) "3.4028235e+38"
#+OpenMCL "3.4028235E+38"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(prin1-to-string LEAST-POSITIVE-SHORT-FLOAT )
#+XCL "2.939S-39"
#+CLISP "1.1755s-38"
#+(or ALLEGRO CMU SBCL) "1.4012985e-45"
#+OpenMCL "1.4012985E-45"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(prin1-to-string LEAST-NEGATIVE-SHORT-FLOAT )
#+XCL "-2.939S-39"
#+CLISP "-1.1755s-38"
#+(or ALLEGRO CMU SBCL) "-1.4012985e-45"
#+OpenMCL "-1.4012985E-45"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(prin1-to-string MOST-NEGATIVE-SHORT-FLOAT )
#+XCL "-1.701S38"
#+CLISP "-3.4028s38"
#+ALLEGRO "-3.4028232e+38"
#+(or CMU SBCL) "-3.4028235e+38"
#+OpenMCL "-3.4028235E+38"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(let ((s (prin1-to-string MOST-POSITIVE-SINGLE-FLOAT )))
  (or #+XCL (equal s "1.701411E38")
      #+CLISP (equal s "1.7014117E38") #+CLISP (equal s "3.4028235E38")
      #+ALLEGRO (equal s "3.4028232e+38")
      #+(or CMU SBCL) (equal s "3.4028235e+38")
      #+OpenMCL (equal s "3.4028235E+38")
) )
T

(let ((s (prin1-to-string LEAST-POSITIVE-SINGLE-FLOAT )))
  (or #+(or XCL CLISP) (equal s "2.938736E-39")
      #+CLISP (equal s "1.1754944E-38")
      #+(or ALLEGRO CMU SBCL) (equal s "1.4012985e-45")
      #+OpenMCL (equal s "1.4012985E-45")
) )
T

(let ((s (prin1-to-string LEAST-NEGATIVE-SINGLE-FLOAT )))
  (or #+(or XCL CLISP) (equal s "-2.938736E-39")
      #+CLISP (equal s "-1.1754944E-38")
      #+(or ALLEGRO CMU SBCL) (equal s "-1.4012985e-45")
      #+OpenMCL (equal s "-1.4012985E-45")
) )
T

(let ((s (prin1-to-string MOST-NEGATIVE-SINGLE-FLOAT )))
  (or #+XCL (equal s "-1.701411E38")
      #+CLISP (equal s "-1.7014117E38") #+CLISP (equal s "-3.4028235E38")
      #+ALLEGRO (equal s "-3.4028232e+38")
      #+(or CMU SBCL) (equal s "-3.4028235e+38")
      #+OpenMCL (equal s "-3.4028235E+38")
) )
T

(let ((s (prin1-to-string MOST-POSITIVE-DOUBLE-FLOAT )))
  (or #+XCL (equal s "1.701411834604692D38")
      #+CLISP (equal s "8.988465674311579d307")
      #+CLISP (equal s "1.7976931348623157d308")
      #+ALLEGRO (equal s "4.494232837155787d+307")
      #+(or CMU SBCL) (equal s "1.7976931348623157d+308")
      #+OpenMCL (equal s "1.7976931348623157D+308")
      #+LISPWORKS (equal s "1.7976931348623157E308")
) )
T

(let ((s (prin1-to-string LEAST-POSITIVE-DOUBLE-FLOAT )))
  (or #+XCL (equal s "2.938735877055719D-39")
      #+CLISP (equal s "5.562684646268004d-309")
      #+CLISP (equal s "2.2250738585072014d-308")
      #+ALLEGRO (equal s "4.9406564584124657d-324")
      #+(or CMU18 SBCL) (equal s "4.940656458412465d-324")
      #+CMU19 (equal s "4.9406564584124654d-324")
      #+OpenMCL (equal s "5.0D-324")
      #+LISPWORKS (equal s "4.9406564584124646E-324")
) )
T

(let ((s (prin1-to-string LEAST-NEGATIVE-DOUBLE-FLOAT )))
  (or #+XCL (equal s "-2.938735877055719D-39")
      #+CLISP (equal s "-5.562684646268004d-309")
      #+CLISP (equal s "-2.2250738585072014d-308")
      #+ALLEGRO (equal s "-4.9406564584124657d-324")
      #+(or CMU18 SBCL) (equal s "-4.940656458412465d-324")
      #+CMU19 (equal s "-4.9406564584124654d-324")
      #+OpenMCL (equal s "-5.0D-324")
      #+LISPWORKS (equal s "-4.9406564584124646E-324")
) )
T

(let ((s (prin1-to-string MOST-NEGATIVE-DOUBLE-FLOAT )))
  (or #+XCL (equal s "-1.701411834604692D38")
      #+CLISP (equal s "-8.988465674311579d307")
      #+CLISP (equal s "-1.7976931348623157d308")
      #+ALLEGRO (equal s "-4.494232837155787d+307")
      #+(or CMU SBCL) (equal s "-1.7976931348623157d+308")
      #+OpenMCL (equal s "-1.7976931348623157D+308")
      #+LISPWORKS (equal s "-1.7976931348623157E308")
) )
T

(prin1-to-string MOST-POSITIVE-LONG-FLOAT )
#+XCL "1.701411834604692D38"
#+CLISP "8.8080652584198167656L646456992"
#+ALLEGRO "4.494232837155787d+307"
#+(or CMU SBCL) "1.7976931348623157d+308"
#+OpenMCL "1.7976931348623157D+308"
#+LISPWORKS "1.7976931348623157E308"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(prin1-to-string LEAST-POSITIVE-LONG-FLOAT )
#+XCL "2.938735877055719D-39"
#+CLISP "5.676615526003731344L-646456994"
#+ALLEGRO "4.9406564584124657d-324"
#+(or CMU18 SBCL) "4.940656458412465d-324"
#+CMU19 "4.9406564584124654d-324"
#+OpenMCL "5.0D-324"
#+LISPWORKS "4.9406564584124646E-324"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(prin1-to-string LEAST-NEGATIVE-LONG-FLOAT )
#+XCL "-2.938735877055719D-39"
#+CLISP "-5.676615526003731344L-646456994"
#+ALLEGRO "-4.9406564584124657d-324"
#+(or CMU18 SBCL) "-4.940656458412465d-324"
#+CMU19 "-4.9406564584124654d-324"
#+OpenMCL "-5.0D-324"
#+LISPWORKS "-4.9406564584124646E-324"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(prin1-to-string MOST-NEGATIVE-LONG-FLOAT )
#+XCL "-1.701411834604692D38"
#+CLISP "-8.8080652584198167656L646456992"
#+ALLEGRO "-4.494232837155787d+307"
#+(or CMU SBCL) "-1.7976931348623157d+308"
#+OpenMCL "-1.7976931348623157D+308"
#+LISPWORKS "-1.7976931348623157E308"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(prin1-to-string SHORT-FLOAT-EPSILON )
#+XCL "1.526S-5"
#+CLISP "7.6295s-6"
#+ALLEGRO "1.1920929e-7"
#+(or CMU SBCL) "5.960465e-8"
#+OpenMCL "5.960465E-8"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(prin1-to-string SINGLE-FLOAT-EPSILON )
#+XCL "5.960464E-8"
#+CLISP "5.960465E-8"
#+ALLEGRO "1.1920929e-7"
#+(or CMU SBCL) "5.960465e-8"
#+OpenMCL "5.960465E-8"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(let ((s (prin1-to-string DOUBLE-FLOAT-EPSILON )))
  (or #+XCL (equal s "1.387778780781446D-17")
      ;; #+    CLISP      (equal s "1.1107651257113995d-16") ; linux/i386
      #+(or CLISP CMU SBCL) (equal s "1.1102230246251568d-16")
      #+OpenMCL (equal s "1.1102230246251568D-16")
      #+ALLEGRO (equal s "2.220446049250313d-16")
      #+LISPWORKS (equal s "1.1102230246251568E-16")))
T

(prin1-to-string LONG-FLOAT-EPSILON )
#+XCL "1.387778780781446D-17"
#+CLISP "5.4210108624275221706L-20"
#+ALLEGRO "2.220446049250313d-16"
#+(or CMU SBCL) "1.1102230246251568d-16"
#+OpenMCL "1.1102230246251568D-16"
#+LISPWORKS "1.1102230246251568E-16"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(prin1-to-string SHORT-FLOAT-NEGATIVE-EPSILON )
#+XCL "1.526S-5"
#+CLISP "3.81476s-6"
#+ALLEGRO "1.1920929e-7"
#+(or CMU18 SBCL) "2.9802325e-8"
#+CMU19 "2.9802326e-8"
#+OpenMCL "2.9802326E-8"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(prin1-to-string SINGLE-FLOAT-NEGATIVE-EPSILON )
#+XCL "5.960464E-8"
#+CLISP "2.9802326E-8"
#+ALLEGRO "1.1920929e-7"
#+(or CMU18 SBCL) "2.9802325e-8"
#+CMU19 "2.9802326e-8"
#+OpenMCL "2.9802326E-8"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL) UNKNOWN

(let ((s (prin1-to-string DOUBLE-FLOAT-NEGATIVE-EPSILON )))
  (or #+XCL (equal s "1.387778780781446D-17")
      ;; #+    CLISP      (equal s "5.553825628556998d-17") ; linux/i386
      #+(or CLISP CMU SBCL) (equal s "5.551115123125784d-17")
      #+OpenMCL (equal s "5.551115123125784D-17")
      #+ALLEGRO (equal s "2.220446049250313d-16")
      #+LISPWORKS (equal s "5.551115123125784E-17")))
T

(prin1-to-string LONG-FLOAT-NEGATIVE-EPSILON )
#+XCL "1.387778780781446D-17"
#+CLISP "2.7105054312137610853L-20"
#+ALLEGRO "2.220446049250313d-16"
#+(or CMU SBCL) "5.551115123125784d-17"
#+OpenMCL "5.551115123125784D-17"
#+LISPWORKS "5.551115123125784E-17"
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(/ 1 0)
ERROR

(/ 1 0.0s0)
ERROR

(/ 1 0.0f0)
ERROR

(/ 1 0.0d0)
ERROR

(/ 1 0.0L0)
ERROR

(expt 10.0s0 1000)
ERROR

(expt 10.0f0 1000)
ERROR

(expt 10.0d0 1000)
ERROR

(expt 10.0L0 1000000000)
ERROR

;Kap 13 ZEICHEN
;-------------------------------------------------------------------------------

(STANDARD-CHAR-P #\a)
T

(STANDARD-CHAR-P 1)
ERROR

(GRAPHIC-CHAR-P #\a)
T

(GRAPHIC-CHAR-P 1)
ERROR

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\a)
T

(#-(or CMU SBCL OpenMCL LISPWORKS) STRING-CHAR-P #+(or CMU SBCL OpenMCL LISPWORKS) CHARACTERP #\1)
T

#-(or CMU SBCL OpenMCL)
(STRING-CHAR-P "")
#-(or CMU SBCL OpenMCL)
ERROR

(ALPHA-CHAR-P #\a)
T

(ALPHA-CHAR-P #\$)
NIL

(UPPER-CASE-P #\a)
NIL

(LOWER-CASE-P #\A)
NIL

(BOTH-CASE-P #\a)
T

(BOTH-CASE-P #\$)
NIL

(DIGIT-CHAR-P #\a)
NIL

(DIGIT-CHAR-P #\5)
5

(ALPHANUMERICP #\a)
T

(ALPHANUMERICP #\$)
NIL

(CHAR= #\d #\d)
T

(CHAR/= #\d #\d)
NIL

(CHAR< #\z #\0)
NIL

;CHAR>, CHAR>=, CHAR<=,

(CHAR-EQUAL #\d #\d)
T

(CHAR-NOT-EQUAL #\d #\d)
NIL

(CHAR-LESSP #\d #\x)
T

(CHAR-LESSP #\d #\d)
NIL

(CHAR-NOT-GREATERP #\d #\d)
T

(CHAR-GREATERP #\e #\d)
T

(CHAR-NOT-LESSP #\e #\d)
T

;CHAR-CODE, CODE-CHAR, CHARACTER,

(CHAR-UPCASE #\a)
#\A

(CHAR-UPCASE #\=)
#\=

(CHAR= (CHAR-DOWNCASE (CHAR-UPCASE #\x)) #\x)
T

(CHAR-DOWNCASE #\A)
#\a

(CHAR= (CHAR-UPCASE (CHAR-DOWNCASE #\X)) #\X)
T

(DIGIT-CHAR 7)
#\7

(DIGIT-CHAR 12)
NIL

;CHAR-INT, INT-CHAR, CHAR-NAME, NAME-CHAR,

CHAR-CODE-LIMIT
#+XCL 128 #+(or (and CLISP (not UNICODE)) AKCL CMU SBCL OpenMCL) 256 #+(or ALLEGRO LISPWORKS) 65536 #+(and CLISP UNICODE) 1114112
#-(or XCL CLISP AKCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

;Kap 14 SEQUENZEN
;-------------------------------------------------------------------------------

(ELT (SYMBOL-NAME (QUOTE ABC)) 0)
#\A

(SUBSEQ (QUOTE (A B C D E)) 2)
(C D E)

(COPY-SEQ '#(A B C))
#(A B C)

(COPY-SEQ (QUOTE ((A B) C (D E))))
((A B) C (D E))

(LENGTH (QUOTE #(A B C D E F)))
6

(LENGTH (QUOTE (A B C D E F)))
6

(NREVERSE (QUOTE (A (B (C) D))))
((B (C) D) A)

(REVERSE (QUOTE (1 2 3 4)))
(4 3 2 1)

(MAKE-SEQUENCE (QUOTE VECTOR) 4 :INITIAL-ELEMENT (QUOTE O))
#(O O O O)

(MAKE-SEQUENCE (QUOTE LIST) 4 :INITIAL-ELEMENT (QUOTE O))
(O O O O)

(equalp (make-sequence 'string 5 :initial-element #\a)
        (make-string 5 :initial-element #\a))
t

(CONCATENATE (QUOTE LIST) (QUOTE (A B C)) (QUOTE (1 2)))
(A B C 1 2)

(MAP (QUOTE LIST) (QUOTE LIST) (QUOTE (#\a #\b #\c)) (QUOTE (#\1 #\2
#\3)))
((#\a #\1) (#\b #\2) (#\c #\3))

(MAP (QUOTE LIST) (QUOTE LIST) (QUOTE (A B C)) (QUOTE (1 2 3)))
((A 1) (B 2) (C 3))

;; Check that at construction of a sequence, a length constraint implicitly
;; contained in the desired sequence type is respected, i.e. an error is
;; signalled if it cannot be fulfilled.
;
; MAKE-SEQUENCE
(make-sequence '(vector t 5) 5 :initial-element 'a)
#(A A A A A)
(make-sequence '(vector t 5) 6 :initial-element 'a)
ERROR
(make-sequence '(or (vector t 5) (vector t 10)) 5 :initial-element 'a)
#(A A A A A)
(make-sequence '(or (vector t 5) (vector t 10)) 6 :initial-element 'a)
ERROR
#+CLISP
(make-sequence '(vector t 5) 5
  :initial-element #\A :update #'(lambda (c) (code-char (1+ (char-code c)))))
#+CLISP
#(#\A #\B #\C #\D #\E)
#+CLISP
(make-sequence '(vector t 5) 6
  :initial-element #\A :update #'(lambda (c) (code-char (1+ (char-code c)))))
#+CLISP
ERROR
#+CLISP
(make-sequence '(or (vector t 5) (vector t 10)) 5
  :initial-element #\A :update #'(lambda (c) (code-char (1+ (char-code c)))))
#+CLISP
#(#\A #\B #\C #\D #\E)
#+CLISP
(make-sequence '(or (vector t 5) (vector t 10)) 6
  :initial-element #\A :update #'(lambda (c) (code-char (1+ (char-code c)))))
#+CLISP
ERROR
;
; COERCE
(coerce #(a b c d e) '(vector t 5))
#(A B C D E)
(coerce #(a b c d e f) '(vector t 5))
ERROR
(coerce #(a b c d e) '(or (vector t 5) (vector t 10)))
#(A B C D E)
(coerce #(a b c d e f) '(or (vector t 5) (vector t 10)))
ERROR
(coerce '(a b c d e) '(vector t 5))
#(A B C D E)
(coerce '(a b c d e f) '(vector t 5))
ERROR
(coerce '(a b c d e) '(or (vector t 5) (vector t 10)))
#(A B C D E)
(coerce '(a b c d e f) '(or (vector t 5) (vector t 10)))
ERROR
;
; SYS::COERCED-SUBSEQ
#+CLISP (sys::coerced-subseq #(a b c d e) '(vector t 5))
#+CLISP #(A B C D E)
#+CLISP (sys::coerced-subseq #(a b c d e f) '(vector t 5))
#+CLISP ERROR
#+CLISP (sys::coerced-subseq #(a b c d e) '(or (vector t 5) (vector t 10)))
#+CLISP #(A B C D E)
#+CLISP (sys::coerced-subseq #(a b c d e f) '(or (vector t 5) (vector t 10)))
#+CLISP ERROR
#+CLISP (sys::coerced-subseq '(a b c d e) '(vector t 5))
#+CLISP #(A B C D E)
#+CLISP (sys::coerced-subseq '(a b c d e f) '(vector t 5))
#+CLISP ERROR
#+CLISP (sys::coerced-subseq '(a b c d e) '(or (vector t 5) (vector t 10)))
#+CLISP #(A B C D E)
#+CLISP (sys::coerced-subseq '(a b c d e f) '(or (vector t 5) (vector t 10)))
#+CLISP ERROR
;
; CONCATENATE
(concatenate '(vector t 5) '(a b c) '(d e))
#(A B C D E)
(concatenate '(vector t 5) '(a b c) '(d e f))
ERROR
(concatenate '(or (vector t 5) (vector t 10)) '(a b c) '(d e))
#(A B C D E)
(concatenate '(or (vector t 5) (vector t 10)) '(a b c) '(d e f))
ERROR
;
; MAP
(map '(vector t 5) #'identity '(a b c d e))
#(A B C D E)
(map '(vector t 5) #'identity '(a b c d e f))
ERROR
(map '(or (vector t 5) (vector t 10)) #'identity '(a b c d e))
#(A B C D E)
(map '(or (vector t 5) (vector t 10)) #'identity '(a b c d e f))
ERROR
;
; MERGE
(merge '(vector t 5) '(a b c d e) '() #'<)
#(A B C D E)
(merge '(vector t 5) '(a b c d e f) '() #'<)
ERROR
(merge '(or (vector t 5) (vector t 10)) '(a b c d e) '() #'<)
#(A B C D E)
(merge '(or (vector t 5) (vector t 10)) '(a b c d e f) '() #'<)
ERROR

(SOME (QUOTE NULL) (QUOTE (A B NIL T E)))
T

(EVERY (QUOTE ATOM) (QUOTE (A 8 #(A B))))
T

(NOTANY (QUOTE EQ) (QUOTE (A B C D E 4)) (QUOTE (I J K L M 4)))
nil ;? T

(NOTEVERY (QUOTE EQ) '#(U) (QUOTE (A X U)))
T

(REDUCE (QUOTE LIST) (QUOTE (A)) :FROM-END NIL :INITIAL-VALUE NIL)

(NIL A)

(REDUCE (QUOTE LIST) (QUOTE (A B C D)) :FROM-END NIL :INITIAL-VALUE
(QUOTE III))
((((III A) B) C) D)

(REDUCE (QUOTE LIST) (QUOTE (A B C D)) :FROM-END T)
(A (B (C D)))

(FILL '#(A B C D) (QUOTE I) :START 1 :END 3)
#(A I I D)

(REPLACE '#(A B C D) '#(I J) :START1 1)
#(A I J D)

(REMOVE (QUOTE NUMBERP) '#(Y A 4 A C 9 A D 2 3) :COUNT 1 :FROM-END T)
#(Y A 4 A C 9 A D 2 3)

(REMOVE (QUOTE A) (QUOTE (A 1 B A 2 A)) :START 1)
(A 1 B 2)

(REMOVE-DUPLICATES (QUOTE (A B C A D A)) :START 1)
(A B C D A)

(REMOVE-IF (QUOTE NUMBERP) '#(Y A 4 A C 9 A D 2 3))
#(Y A A C A D)

(REMOVE-IF-NOT (QUOTE NUMBERP) '#(Y A 4 A C 9 A D 2 3))
#(4 9 2 3)

(REMOVE-IF-NOT (QUOTE NUMBERP) '#(Y A 4 A C 9 A D 2 3) :COUNT 2 :FROM-END NIL)
#(4 A C 9 A D 2 3)

(DELETE (QUOTE (A)) (QUOTE ((A B) (C D) (A))) :TEST (QUOTE EQUAL))

((A B) (C D))

(DELETE-IF (FUNCTION (LAMBDA (X) (EQ (CAR X) (QUOTE A))))
(QUOTE ((A B) (C D) (A))))
((C D))

(DELETE-IF-NOT (QUOTE NUMBERP) (QUOTE (A 3 B 4)))
(3 4)

;DELETE-DUPLICATES,

(NSUBSTITUTE (QUOTE NEW) (QUOTE (1 OLD)) (QUOTE ((0 OLD) (1 OLD) (2 OLD)))
:TEST-NOT (QUOTE EQUAL) :FROM-END T)
(NEW (1 OLD) NEW)

(NSUBSTITUTE (QUOTE NEW) (QUOTE OLD) (QUOTE (0 OLD 1 OLD 2 OLD)) :END 2)
(0 NEW 1 OLD 2 OLD)

(NSUBSTITUTE-IF (QUOTE NEW) (QUOTE NUMBERP) (QUOTE (0 A 1 B 2 C 3 D))
:COUNT 2
:END 5)
(NEW A NEW B 2 C 3 D)

(NSUBSTITUTE-IF-NOT (QUOTE NEW) (QUOTE NUMBERP) (QUOTE (0 A 1 B 2 C
3 D)) :COUNT
2 :FROM-END T)
(0 A 1 B 2 NEW 3 NEW)

(SUBSTITUTE (QUOTE NEW) (QUOTE (2 OLD))
(QUOTE ((1 OLD) (2 OLD) (3 OLD) (4 OLD))) :TEST (QUOTE EQUAL) :START
3)
((1 OLD) (2 OLD) (3 OLD) (4 OLD))

(SUBSTITUTE-IF (QUOTE NEW) (QUOTE NUMBERP) (QUOTE (A 1 B 2 D 3)))
(A NEW B NEW D NEW)

(SUBSTITUTE-IF-NOT (QUOTE NEW) (QUOTE NUMBERP) (QUOTE (A 1 B 2 D 3))
:COUNT 2
:FROM-END T)
(A 1 NEW 2 NEW 3)

(FIND (QUOTE 0) (QUOTE ((0 A) (1 A) (2 A) (0 B))) :TEST (QUOTE =) :FROM-END
T
:KEY (QUOTE CAR) :START 1)
(0 B)

(FIND-IF (QUOTE NUMBERP) (QUOTE ((A 0) (B 1) (C 2))) :KEY (QUOTE CADR)
:START 3)
NIL

;FIND-IF-NOT,

(POSITION (QUOTE A) (QUOTE ((0 A) (1 B) (2 A) (3 B))) :TEST
(FUNCTION (LAMBDA (X Y) (EQ X (CADR Y)))) :START 1)
2

(POSITION (QUOTE A) (QUOTE ((0 A) (1 B) (2 A) (3 B))) :KEY (QUOTE CADR))

0

(POSITION-IF (QUOTE NUMBERP) (QUOTE ((0 X) (1 7.0) (2 8))) :FROM-END
T :START 1
:KEY (QUOTE CADR))
2

;POSITION-IF-NOT,

(COUNT (QUOTE (A)) (QUOTE (A (A) A (A) A B)) :TEST-NOT (QUOTE EQUAL)
:KEY
(FUNCTION (LAMBDA (X) (IF (ATOM X) (LIST X)))))
3

(COUNT-IF-NOT (QUOTE NUMBERP) '#(A 3 B 5 7 C D) :START 2 :END 5)
1

;COUNT-IF-NOT,

(MISMATCH (QUOTE (A B C 3 4 5)) (QUOTE (A B X 3 4 B)) :START1 1 :START2 5 :END1
2 :TEST-NOT (QUOTE EQ))
1

(MISMATCH (QUOTE (A B C 3 4 5)) (QUOTE (U B X 3 4 5)) :FROM-END T)
#+XCL 2 #-XCL 3

(SEARCH "ABCD" "0ABIABJBCBC" :END1 3 :START1 1 :START2 0 :FROM-END
T)
9

(SEARCH (QUOTE (#\A #\B #\C #\D)) "0ABIABJBCBC" :END1 2 :START2 0 :FROM-END
T)
4

(SEARCH (QUOTE (A B C D)) (QUOTE (0 A B I A B J B C B C)) :END1 2 :START2
2)
4

(SORT (QUOTE ((U 3) (I 1) (A 7) (K 3) (C 4) (B 6))) (QUOTE <) :KEY
(QUOTE CADR))
((I 1) (U 3) (K 3) (C 4) (B 6) (A 7))

(STABLE-SORT (QUOTE ((B 4) (A 3) (A 2) (B 1) (C 9) (B 2))) (QUOTE STRING<)
:KEY
(QUOTE CAR))
((A 3) (A 2) (B 4) (B 1) (B 2) (C 9))

(MERGE (QUOTE LIST) (QUOTE (5 1 4 4 7)) (QUOTE (2 3 5 6 8 9)) (QUOTE
<))
(2 3 5 1 4 4 5 6 7 8 9) ;? error

(MERGE (QUOTE LIST) (QUOTE (1 4 4 7)) (QUOTE (2 3 5 6 8 9)) (QUOTE
<))
(1 2 3 4 4 5 6 7 8 9) ;? error

;Kap 15 LISTEN
;-------------------------------------------------------------------------------

(CAR (QUOTE (A B C D E F G)))
A

(CDR (QUOTE (A B C D E F G)))
(B C D E F G)

(CADR (QUOTE (A B C D E F G)))
B

(CDDR (QUOTE (A B C D E F G)))
(C D E F G)

(CADDR (QUOTE (A B C D E F G)))
C

(CDDDR (QUOTE (A B C D E F G)))
(D E F G)

(CADDDR (QUOTE (A B C D E F G)))
D

(CDDDDR (QUOTE (A B C D E F G)))
(E F G)

(CAADR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
((U V W) X)

(CADAR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
(6 7)

(CDAAR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
(5)

(CDADR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
(Y)

(CDDAR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
NIL

(CAAAAR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
(1 2 3)

(CAADAR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
6

(CAADDR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
(Q W E)

(CADAAR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
5

(CADADR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
Y

(CADDAR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
NIL

(CADDDR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
(A B C)

(CDAAAR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
(4)

(CDAADR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
(X)

(CDADAR
(QUOTE (((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G)))
(7)

(CONS 1 2)
(1 . 2)

(CONS (QUOTE A) (CONS (QUOTE B) (CONS (QUOTE C) (QUOTE NIL))))
(A B C)

(CONS (QUOTE A) (QUOTE (B C D)))
(A B C D)

(TREE-EQUAL 5 (+ 2 3) :TEST (FUNCTION EQL))
T

(ENDP (QUOTE NIL))
T

(ENDP (QUOTE (A . B)))
NIL

(LIST-LENGTH (QUOTE (A B C D)))
4

(LET ((X (LIST (QUOTE A) (QUOTE B) (QUOTE C)))) (RPLACD (LAST X) X)

(LIST-LENGTH X))
NIL

(NTH 0 (QUOTE (A B C D)))
A

(FIRST (QUOTE (1 2 3 4 5 6 7 8 9 10 11)))
1

(SECOND (QUOTE (1 2 3 4 5 6 7 8 9 10 11)))
2

(THIRD (QUOTE (1 2 3 4 5 6 7 8 9 10 11)))
3

(FOURTH (QUOTE (1 2 3 4 5 6 7 8 9 10 11)))
4

(FIFTH (QUOTE (1 2 3 4 5 6 7 8 9 10 11)))
5

(SIXTH (QUOTE (1 2 3 4 5 6 7 8 9 10 11)))
6

(SEVENTH (QUOTE (1 2 3 4 5 6 7 8 9 10 11)))
7

(EIGHTH (QUOTE (1 2 3 4 5 6 7 8 9 10 11)))
8

(NINTH (QUOTE (1 2 3 4 5 6 7 8 9 10 11)))
9

(TENTH (QUOTE (1 2 3 4 5 6 7 8 9 10 11)))
10

(REST (QUOTE (A . B)))
B

(NTHCDR 1 (QUOTE (A B C D)))
(B C D)

(LAST (QUOTE (1 2 3 4 5)))
(5)

(LAST (QUOTE (1 2 3 . 4)))
(3 . 4)

(LIST (QUOTE A) (QUOTE B) (QUOTE C) (QUOTE D))
(A B C D)

(LIST* (QUOTE A) (QUOTE B) (QUOTE C) (QUOTE D))
(A B C . D)

(MAKE-LIST 4 :INITIAL-ELEMENT (QUOTE O))
(O O O O)

(MAKE-LIST 3 :INITIAL-ELEMENT (QUOTE RAH))
(RAH RAH RAH)

(APPEND (QUOTE (A B C)) (QUOTE (D E F)) (QUOTE NIL) (QUOTE (G)))
(A B C D E F G)

(COPY-LIST (QUOTE (1 2 3 4 5)))
(1 2 3 4 5)

(COPY-LIST (QUOTE (1 2 3 . 4)))
(1 2 3 . 4)

(COPY-ALIST (QUOTE (A B)))
(A B)

(COPY-ALIST (QUOTE ((1 . A) (2 . B) (3 . C))))
((1 . A) (2 . B) (3 . C))

(COPY-ALIST (QUOTE ((A B) C (D E))))
((A B) C (D E))

(COPY-TREE (QUOTE (A B (C (D) (E F)) G)))
(A B (C (D) (E F)) G)

(REVAPPEND (QUOTE (A B C)) (QUOTE (D E F)))
(C B A D E F)

(REVAPPEND (QUOTE (A B C)) (QUOTE I))
(C B A . I) ;? error

(NRECONC (QUOTE (A B C)) (QUOTE (I J)))
(C B A I J)

;NRECONC

(SETQ AA NIL)
NIL

(PUSH (QUOTE 1) AA)
(1)

(PUSH (QUOTE 2) AA)
(2 1)

(POP AA)
2

(POP AA)
1

(POP AA)
NIL

(SETQ AA (QUOTE (B A)))
(B A)

(PUSHNEW (QUOTE A) AA)
(B A)

(PUSHNEW (QUOTE C) AA)
(C B A)

(PUSHNEW (QUOTE U) (CAR (SETQ XX (QUOTE (NIL KKK)))))
(U)

(PUSHNEW (QUOTE U) (CAR XX))
(U)

(PUSHNEW (QUOTE V) (CAR XX))
(V U)

(EVAL (QUOTE XX))
((V U) KKK)

(BUTLAST (QUOTE (A B C)) 2)
(A)

(NBUTLAST (QUOTE (A B C D)) 6)
NIL

(NBUTLAST (QUOTE (A B C D)) 1)
(A B C)

(LDIFF (SETQ XX (QUOTE (A B C D E))) (CDDR XX))
(A B)

(LDIFF (SETQ XX (QUOTE (A B C D . E))) (CDDR XX))
(A B)

(LDIFF (QUOTE (A B C D . E)) (QUOTE E))
(A B C D)

(LDIFF (QUOTE (1 . 2)) 3)
(1 . 2)

(let ((lists '#((a b c) (a b c . d)))
      (ld-res #(#(nil (a b) (a b c) (a b c) (a b c) (a b c) (a b c))
                #(nil (a b) (a b c . d) (a b c . d) (a b c . d) (a b c)
                  (a b c . d))))
      (tp-res #(#(t t nil nil t nil nil) #(t t nil nil nil t nil))))
  (dotimes (i (length lists))
    (let* ((list (aref lists i)) (l-r (aref ld-res i)) (t-r (aref tp-res i))
           (objects (vector list (cddr list) (copy-list (cddr list))
                            '(f g h) '() 'd 'x)))
      (dotimes (j (length objects))
        (let ((object (aref objects j)))
          (unless (equal (tailp object list) (aref t-r j))
            (error "(tailp ~s ~s): ~s; should be: ~s"
                   object list (tailp object list) (aref t-r j)))
          (unless (equal (ldiff list object) (aref l-r j))
            (error "(ldiff ~s ~s): ~s; should be: ~s"
                   list object (ldiff list object) (aref l-r j))))))))
nil

(TAILP 10203040506070 (LIST* 'A 'B (1- 10203040506071)))
T

;RPLACA, RPLACD

(NSUBST (QUOTE A) (QUOTE B) (QUOTE (U B (B) C)) :TEST-NOT
        (FUNCTION (LAMBDA (X Y) (NOT (EQL X Y)))))
(U A (A) C)

(NSUBST-IF (QUOTE OO) (QUOTE NUMBERP) (QUOTE (A B C (3 (4) 0))))
(A B C (OO (OO) OO))

(NSUBST-IF-NOT (QUOTE OO) (FUNCTION (LAMBDA (X) (OR (LIST X) (SYMBOLP X))))
               (QUOTE (A B C (3 (4) 0))))
(A B C (3 (4) 0))

(SUBST (QUOTE A) (QUOTE B) (QUOTE (U B (B) C))
       :TEST-NOT (FUNCTION (LAMBDA (X Y) (NOT (EQL X Y))))
       :KEY (FUNCTION (LAMBDA (U) (IF (LISTP U) (CAR U)))))
(U . A)

(SUBST-IF (QUOTE NUMMMER) (QUOTE NUMBERP) (QUOTE ((A (7 (V 6))))))

((A (NUMMMER (V NUMMMER))))

(SUBST-IF-NOT (QUOTE NUMMMER)
              (FUNCTION (LAMBDA (X) (OR (LISTP X) (NUMBERP X))))
              (QUOTE ((A (7 (V 6))))))
((NUMMMER (7 (NUMMMER 6))))

(NSUBLIS (QUOTE (((A) . UU) (A . II))) (QUOTE (I (A) A))
         :TEST (FUNCTION (LAMBDA (X Y) (IF (LISTP Y) (EQL X (CAR Y))))))
#+(or XCL ALLEGRO)                            (I II . II) ; X aus der Aliste, Y ein Blatt des Baumes
#+(or CLISP CMU SBCL LUCID OpenMCL LISPWORKS) (I (UU) UU) ; X ein Blatt, Y aus der Aliste
#-(or XCL CLISP CMU SBCL LUCID ALLEGRO OpenMCL LISPWORKS) UNKNOWN

(SUBLIS (QUOTE (((A) . UU) (A . II))) (QUOTE (I (A) A))
        :TEST (FUNCTION (LAMBDA (X Y) (IF (LISTP Y) (EQL X (CAR Y))))))
#+(or XCL ALLEGRO LUCID)                (I II . II) ; X aus der Aliste, Y ein Blatt des Baumes
#+(or CLISP CMU SBCL OpenMCL LISPWORKS) (I (UU) UU) ; X ein Blatt, Y aus der Aliste
#-(or XCL CLISP CMU SBCL LUCID ALLEGRO OpenMCL LISPWORKS) UNKNOWN

(MEMBER (QUOTE A) (QUOTE ((A) (B) (A) (C))) :KEY (QUOTE CAR))
((A) (B) (A) (C))

(MEMBER-IF (QUOTE NUMBERP) (QUOTE ((A) (B) (3) (C))) :KEY (QUOTE CAR))

((3) (C))

(MEMBER-IF-NOT (QUOTE NUMBERP) (QUOTE ((8) (A) (B) (3) (C)))
               :KEY (QUOTE CAR))
((A) (B) (3) (C))

(TAILP (CDDR (SETQ XX (QUOTE (U I A B)))) XX)
T

(TAILP (QUOTE D) (QUOTE (A B C . D)))
T

(ADJOIN (QUOTE A) (QUOTE ((A) B C)) :TEST (QUOTE EQUAL))
(A (A) B C)

(NUNION (QUOTE (A B C D)) (QUOTE (U I B A)))
#+XCL (A B C D U I)
#+CLISP (C D U I B A)
#+(or ALLEGRO CMU SBCL OpenMCL) (D C U I B A)
#+LISPWORKS (I U A B C D)
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(UNION (QUOTE (A B C D)) (QUOTE (A D I V)))
#+(or XCL LISPWORKS) (V I A B C D)
#+CLISP (B C A D I V)
#+(or ALLEGRO CMU SBCL OpenMCL) (C B A D I V)
#-(or XCL CLISP ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(INTERSECTION (QUOTE ((A 1) (A 2) (A 3))) (QUOTE ((A 4) (A 2) (B 6) (C 7)))
:TEST (QUOTE EQUAL))
((A 2))

(NINTERSECTION (QUOTE (A B C D)) (QUOTE (C D E F G)) :TEST-NOT (QUOTE EQL))
#-(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) (A B C D)
#+(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) (D C B A)

(NSET-DIFFERENCE (QUOTE (A B C D)) (QUOTE (I J C)))
#-(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) (A B D)
#+(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) (D B A)

(NSET-EXCLUSIVE-OR (QUOTE (A B C)) (QUOTE (I A D C)))
#-OpenMCL (B I D) #+OpenMCL (D I B)

(SET-DIFFERENCE (QUOTE (ANTON BERTA AUTO BERLIN)) (QUOTE (AMERILLA))
                :TEST #'(LAMBDA (X Y) (EQL (ELT (SYMBOL-NAME X) 0)
                                           (ELT (SYMBOL-NAME Y) 0))))
#+(or XCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) (BERLIN BERTA)
#+(or CLISP AKCL) (BERTA BERLIN)
#-(or XCL CLISP AKCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(SET-EXCLUSIVE-OR (QUOTE (ANTON ANNA EMIL)) (QUOTE (BERTA AUTO AUGUST))
                  :TEST #'(LAMBDA (X Y) (EQL (ELT (SYMBOL-NAME X) 0)
                                             (ELT (SYMBOL-NAME Y) 0))))
#-(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) (EMIL BERTA)
#+(or ALLEGRO CMU SBCL OpenMCL LISPWORKS) (BERTA EMIL)

(SUBSETP (QUOTE (A B)) (QUOTE (B A U I C D)))
T

(ACONS (QUOTE A) (QUOTE B) (QUOTE ((C . D))))
((A . B) (C . D))

(ACONS (QUOTE A) (QUOTE B) NIL)
((A . B))

(ASSOC (QUOTE A) (QUOTE ((B C) A ((A) U) (A I)))
       :TEST-NOT (FUNCTION (LAMBDA (X Y) (IF (ATOM Y) (EQL Y X)))))
#+ALLEGRO ERROR #-ALLEGRO (B C)

(ASSOC-IF (QUOTE SYMBOLP) (QUOTE ((A . 3) (3 . A))))
(A . 3)

(ASSOC-IF-NOT (QUOTE NUMBERP) (QUOTE ((A . 3) (3 . A))))
(A . 3)

(PAIRLIS (QUOTE (A B C)) (QUOTE (1 2 3)))
((C . 3) (B . 2) (A . 1))

(RASSOC (QUOTE A) (QUOTE ((1 . B) (2 . A))))
(2 . A)

(RASSOC-IF (QUOTE SYMBOLP) (QUOTE ((1 . 3) (2 . A))))
(2 . A)

(RASSOC-IF-NOT (QUOTE SYMBOLP) (QUOTE ((1 . 3) (2 . A))))
(1 . 3)

;Kap 16 Hash-Tabellen
;-------------------------------------------------------------------------------

(HASH-TABLE-P (MAKE-HASH-TABLE :TEST (FUNCTION EQL) :REHASH-SIZE 2 :SIZE 20))
T
(HASH-TABLE-P (MAKE-HASH-TABLE :TEST (FUNCTION EQL) :REHASH-SIZE 1.1 :SIZE 20))
T
;CLRHASH, GETHASH, HASH-TABLE-COUNT, MAPHASH, REMHASH, SXHASH,

;; <HS>/Body/mac_with-hash_ble-iterator.html
(defun test-hash-table-iterator (hash-table)
  (let ((all-entries '())
        (generated-entries '())
        (unique (list nil)))
    (maphash #'(lambda (key value) (push (list key value) all-entries))
             hash-table)
    (with-hash-table-iterator (generator-fn hash-table)
      (loop
        (multiple-value-bind (more? key value) (generator-fn)
          (unless more? (return))
          (unless (eql value (gethash key hash-table unique))
             (error "Key ~S not found for value ~S" key value))
          (push (list key value) generated-entries))))
    (unless (= (length all-entries)
               (length generated-entries)
               (length (union all-entries generated-entries
                              :key #'car :test (hash-table-test hash-table))))
      (error "Generated entries and Maphash entries don't correspond"))
    t))
test-hash-table-iterator

(let ((tab (make-hash-table :test #'equal)))
  (setf (gethash "Richard" tab) "Gabriel")
  (setf (gethash "Bruno" tab) "Haible")
  (setf (gethash "Michael" tab) "Stoll")
  (setf (gethash "Linus" tab) "Torvalds")
  (setf (gethash "Richard" tab) "Stallman")
  (test-hash-table-iterator tab)
)
T

#+CLISP
(gethash "foo" (read-from-string
                (prin1-to-string
                 (make-hash-table :test 'equalp :initial-contents
                                  '(("FOO" . "bar"))))))
#+CLISP
"bar"

;Kap 17 Felder
;-------------------------------------------------------------------------------

;MAKE-ARRAY, VECTOR, AREF, SVREF, ARRAY-ELEMENT-TYPE, ARRAY-RANK,
;ARRAY-DIMENSION, ARRAY-DIMENSIONS, ARRAY-TOTAL-SIZE, ARRAY-IN-BOUNDS-P,
;ARRAY-ROW-MAJOR-INDEX, ADJUSTABLE-ARRAY-P,

;array-rank-limit, array-dimension-limit, array-total-size-limit,


;BIT, SBIT, BIT-AND, BIT-ANDC1, BIT-ANDC2, BIT-EQV, BIT-IOR, BIT-NAND, BIT-NOR,
;BIT-NOT, BIT-ORC1, BIT-ORC2, BIT-XOR,

;ARRAY-HAS-FILL-POINTER-P, FILL-POINTER, VECTOR-POP, VECTOR-PUSH,
;VECTOR-PUSH-EXTEND, ADJUST-ARRAY,

;Kap 18 Strings
;-------------------------------------------------------------------------------

;CHAR, SCHAR, STRING=, STRING-EQUAL, STRING/=, STRING<, STRING<=, STRING>,

;STRING>=, STRING-GREATERP, STRING-LESSP, STRING-NOT-EQUAL,
;STRING-NOT-GREATERP, STRING-NOT-LESSP, MAKE-STRING, STRING-LEFT-TRIM,

;STRING-RIGHT-TRIM, STRING-TRIM, STRING-UPCASE, STRING-CAPITALIZE,

;STRING-DOWNCASE, NSTRING-CAPITALIZE, NSTRING-DOWNCASE, NSTRING-UPCASE, STRING,

;;Kap 19 Structures
;;-----------------------------------------------------------------------------

;DEFSTRUCT,

(defstruct (ice-cream-factory
            (:constructor make-factory)
            (:constructor fabricate-factory
              (&key (capacity 5)
                    location
                    (local-flavors
                     (case location
                       ((hawaii) '(pineapple macadamia guava))
                       ((massachusetts) '(lobster baked-bean))
                       ((california) '(ginger lotus avocado bean-sprout garlic))
                       ((texas) '(jalapeno barbecue))))
                    (flavors
                     (subseq (append local-flavors
                                     '(vanilla chocolate strawberry pistachio
                                       maple-walnut peppermint))
                             0 capacity))
                    ((:own owner)))))
  (capacity 3)
  (flavors '(vanilla chocolate strawberry mango))
  (owner 'me))
ICE-CREAM-FACTORY

(let ((houston (fabricate-factory :capacity 4 :location 'texas)))
  (ice-cream-factory-flavors houston)
)
(JALAPENO BARBECUE VANILLA CHOCOLATE)

(let ((cambridge (fabricate-factory :location 'massachusetts)))
  (ice-cream-factory-flavors cambridge)
)
(LOBSTER BAKED-BEAN VANILLA CHOCOLATE STRAWBERRY)

(let ((seattle (fabricate-factory :local-flavors '(salmon))))
  (ice-cream-factory-flavors seattle)
)
(SALMON VANILLA CHOCOLATE STRAWBERRY PISTACHIO)

(let ((wheaton (fabricate-factory :capacity 4 :location 'illinois)))
  (ice-cream-factory-flavors wheaton)
)
(VANILLA CHOCOLATE STRAWBERRY PISTACHIO)

(let ((pittsburgh (fabricate-factory :capacity 4)))
  (ice-cream-factory-flavors pittsburgh)
)
(VANILLA CHOCOLATE STRAWBERRY PISTACHIO)

(let ((cleveland (make-factory :capacity 4)))
  (ice-cream-factory-flavors cleveland)
)
(VANILLA CHOCOLATE STRAWBERRY MANGO)

;; <http://www.lisp.org/HyperSpec/Issues/iss111-writeup.html>
(progn
  (defvar *x* 'global-x)
  (let ((y 'local-y))
    (defstruct baz (*x* 'x-init) (y *x*) (z y))))
baz
(make-baz)
#S(BAZ :*X* X-INIT :Y GLOBAL-X :Z LOCAL-Y)

;Kap 20 EVAL
;-------------------------------------------------------------------------------

;EVAL, EVALHOOK, *evalhook*, APPLYHOOK, *applyhook*,

(CONSTANTP -5)
T

(CONSTANTP (READ-FROM-STRING "1.0e30"))
T

;Kap 21 Streams
;-------------------------------------------------------------------------------

;MAKE-SYNONYM-STREAM, MAKE-BROADCAST-STREAM, MAKE-CONCATENATED-STREAM,
;MAKE-TWO-WAY-STREAM, MAKE-ECHO-STREAM, MAKE-STRING-INPUT-STREAM,
;MAKE-STRING-OUTPUT-STREAM, GET-OUTPUT-STREAM-STRING, with-input-from-string,
;with-open-stream, with-output-to-string,

(STREAMP *STANDARD-INPUT*)
T

(INPUT-STREAM-P *TERMINAL-IO*)
T

;OUTPUT-STREAM-P, STREAM-ELEMENT-TYPE, CLOSE,

;Kap 22 Ein- und Ausgabe
;-------------------------------------------------------------------------------

(READTABLEP *READTABLE*)
T

(READTABLEP (QUOTE PROGN))
NIL

;COPY-READTABLE, READ, *read-base*, READ-BYTE, READ-CHAR, READ-CHAR-NO-HANG,

;*read-default-float-format*, READ-DELIMITED-LIST, READ-FROM-STRING, READ-LINE,
;READ-PRESERVING-WHITESPACE, *read-suppress*, *readtable*, UNREAD-CHAR,

;GET-DISPATCH-MACRO-CHARACTER, GET-MACRO-CHARACTER,
;SET-DISPATCH-MACRO-CHARACTER, SET-MACRO-CHARACTER, SET-SYNTAX-FROM-CHAR,
;MAKE-DISPATCH-MACRO-CHARACTER,

(GET-DISPATCH-MACRO-CHARACTER #\# #\0)
NIL

;PPRINT, PRIN1, PRIN1-TO-STRING, PRINC, PRINC-TO-STRING, PRINT, *print-array*,
;*print-base*, *print-case*, *print-circle*, *print-escape*, *print-gensym*,

;*print-length*, *print-level*, *print-pretty*, *print-radix*,

;PEEK-CHAR, LISTEN, CLEAR-INPUT, CLEAR-OUTPUT, PARSE-INTEGER,

;WRITE, WRITE-BYTE, WRITE-CHAR, WRITE-LINE, WRITE-STRING, WRITE-TO-STRING,
;Y-OR-N-P, YES-OR-NO-P,

;TERPRI, FINISH-OUTPUT, FORCE-OUTPUT, FORMAT, FRESH-LINE,

;Kap 23 File-Interface
;-------------------------------------------------------------------------------

;PATHNAME, TRUENAME, PARSE-NAMESTRING, MERGE-PATHNAMES,
;*default-pathname-defaults*, MAKE-PATHNAME, PATHNAMEP, PATHNAME-DEVICE,
;PATHNAME-DIRECTORY, PATHNAME-HOST, PATHNAME-NAME, PATHNAME-TYPE,
;PATHNAME-VERSION, NAMESTRING, FILE-NAMESTRING, DIRECTORY-NAMESTRING,

;HOST-NAMESTRING, ENOUGH-NAMESTRING, user-homedir-pathname, OPEN,
;with-open-file, RENAME-FILE, DELETE-FILE, PROBE-FILE, FILE-WRITE-DATE,

;FILE-AUTHOR, FILE-LENGTH, FILE-POSITION, LOAD, *load-verbose*, DIRECTORY

;Kap 24 Fehler
;-------------------------------------------------------------------------------

;CERROR, ERROR, *break-on-warnings*, WARN, BREAK, check-type, assert, etypecase,
;ecase, ctypecase, ccase

;Kap 25 Erweiterungen
;-------------------------------------------------------------------------------

;COMPILE, DISASSEMBLE, COMPILE-FILE, DOCUMENTATION, trace, untrace, step, time,
;DESCRIBE, INSPECT, room, ed, DRIBBLE, APROPOS, APROPOS-LIST,
;GET-DECODED-TIME, GET-INTERNAL-REAL-TIME, GET-INTERNAL-RUN-TIME,
;GET-UNIVERSAL-TIME, DECODE-UNIVERSAL-TIME, ENCODE-UNIVERSAL-TIME,

;internal-time-units-per-second, SLEEP, LISP-IMPLEMENTATION-TYPE,
;LISP-IMPLEMENTATION-VERSION, MACHINE-INSTANCE, MACHINE-TYPE, MACHINE-VERSION,

;SOFTWARE-TYPE, SOFTWARE-VERSION, SHORT-SITE-NAME, LONG-SITE-NAME, *features*,
;IDENTITY

;Kap I Systeminterne Praedikate
;-------------------------------------------------------------------------------

;? (SEQUENCEP (TYPE-SPECIFIER-P (BIT-ARRAY-P
;? (ADJUSTABLE-VECTOR-WITH-FILL-POINTER-P (ALISTP (DECLARATION-SPECIFIER-P


#+(or XCL CLISP CMU)
(SYS::FIXNUMP 10) ;?
#+(or XCL CLISP CMU)
T ;?

;Kap II Systeminterne Atome
;-------------------------------------------------------------------------------

;case-every, comment, cond-every, displace, return, return-from, ACCESS, BOOLE,
;call-arguments-limit, DEFUN, errset, *errset*, *macroexpand-hook*, *package*,
;*random-state*, *SAVE-OLD-DEFINITION-WHEN-REDEFINED*,

#+clisp (ext:module-info "clisp" t)
#+clisp "clisp"

; Clean up.
(UNINTERN 'X)
T
