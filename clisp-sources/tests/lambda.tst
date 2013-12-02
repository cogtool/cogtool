;; -*-Lisp -*-
(makunbound 'b) B
(makunbound 'e) E

(SETQ Z 2) 2

((LAMBDA (Z) (DECLARE (SPECIAL Z)) (LIST Z (SYMBOL-VALUE 'Z))) 3)
(3 3)

(MAKUNBOUND 'Z) Z

((LAMBDA (A B) (+ A (* B 3))) 4 5)
19

((LAMBDA (A &OPTIONAL (B 2)) (+ A (* B 3))) 4 5)
19

((LAMBDA (A &OPTIONAL (B 2)) (+ A (* B 3))) 4)
10

((LAMBDA (&OPTIONAL (A 2 B) (C 3 D) &REST X) (LIST A B C D X)))
(2 NIL 3 NIL NIL)

((LAMBDA (&OPTIONAL (A 2 B) (C 3 D) &REST X) (LIST A B C D X)) 6)
(6 T 3 NIL NIL)

((LAMBDA (&OPTIONAL (A 2 B) (C 3 D) &REST X) (LIST A B C D X)) 6 3)
(6 T 3 T NIL)

((LAMBDA (&OPTIONAL (A 2 B) (C 3 D) &REST X) (LIST A B C D X)) 6 3 8)
(6 T 3 T (8))

((LAMBDA (&OPTIONAL (A 2 B) (C 3 D) &REST X) (LIST A B C D X)) 6 3 8 9 10 11)
(6 T 3 T (8 9 10 11))

((LAMBDA (A B &KEY C D) (LIST A B C D)) 1 2)
(1 2 NIL NIL)

((LAMBDA (A B &KEY C D) (LIST A B C D)) 1 2 :C 6)
(1 2 6 NIL)

((LAMBDA (A B &KEY C D) (LIST A B C D)) 1 2 :D 8)
(1 2 NIL 8)

((LAMBDA (A B &KEY C D) (LIST A B C D)) 1 2 :C 6 :D 8)
(1 2 6 8)

((LAMBDA (A B &KEY C D) (LIST A B C D)) 1 2 :D 8 :C 6)
(1 2 6 8)

((LAMBDA (A B &KEY C D) (LIST A B C D)) :A 1 :D 8 :C 6)
(:A 1 6 8)

((LAMBDA (A B &KEY C D) (LIST A B C D)) :A :B :C :D)
(:A :B :D NIL)

((LAMBDA (A &OPTIONAL (B 3) &REST X &KEY C (D A)) (LIST A B C D X)) 1)
(1 3 NIL 1 NIL)

((LAMBDA (A &OPTIONAL (B 3) &REST X &KEY C (D A)) (LIST A B C D X)) 1 2)
(1 2 NIL 1 NIL)

((LAMBDA (A &OPTIONAL (B 3) &REST X &KEY C (D A)) (LIST A B C D X)) :C 7)
(:C 7 NIL :C NIL)

((LAMBDA (A &OPTIONAL (B 3) &REST X &KEY C (D A)) (LIST A B C D X)) 1 6 :C 7)
(1 6 7 1 (:C 7))

((LAMBDA (A &OPTIONAL (B 3) &REST X &KEY C (D A)) (LIST A B C D X)) 1 6 :D 8)
(1 6 NIL 8 (:D 8))

((LAMBDA (A &OPTIONAL (B 3) &REST X &KEY C (D A)) (LIST A B C D X))
 1 6 :D 8 :C 9 :D 10)
(1 6 9 8 (:D 8 :C 9 :D 10))

((LAMBDA (X &AUX (A 3) (B 4)) (+ X (* A B))) 2)
14

((LAMBDA (X Y &OPTIONAL A B &REST Z &KEY C (D Y) &AUX (U 3) (V 4))
   (+ X Y A (* B (CAR Z)) C (* D U) V))
  3 4 5 2 7 :C 6 :D 8)
ERROR

((LAMBDA (X Y &OPTIONAL A B &REST Z &KEY C (D Y) &AUX (U 3) (V 4))

(+ X Y A (* B (CAR Z)) C (* D U) V)) 3 4 5 2 7 :C 6)
ERROR

((LAMBDA (X &AUX C) (CONS X C)) (QUOTE A))
(A)

((LAMBDA (X &REST Y Z) (LIST X Y Z)) 1 2 3)
ERROR

((LAMBDA (5 A B) (LIST A B)) 1 2)
ERROR

((LAMBDA ((LENGTH (QUOTE (A B))) C) (LIST C)) 1)
ERROR

((LAMBDA (X &KEY :Y :Z) (LIST X Y Z)) 1 :Y 2 :Z 3)
ERROR

((LAMBDA (X Y) (LIST X Y Z)) 1 2)
ERROR

((LAMBDA (X Y) (LIST X Y Z)) 1 2 3)
ERROR

((LAMBDA (&OPTIONAL) (LIST A B C)) 1)
ERROR

((LAMBDA (&OPTIONAL (A)) (LIST A)) 1)
(1)

((LAMBDA (&OPTIONAL (A B)) (LIST A B)) 1)
ERROR

((LAMBDA (&OPTIONAL (A 3 B)) (LIST A B)) 1)
(1 T)

((LAMBDA (&OPTIONAL (A 3)) (LIST A)) 1)
(1)

((LAMBDA (&OPTIONAL (A 3 B 4)) (LIST A B)) 1)
#+XCL (1 T)
#-XCL ERROR

((LAMBDA (X) (LIST X Y)) 1 2)
ERROR

((LAMBDA (X) (LIST X)) 1 2)
ERROR

((LAMBDA (#\a) (LIST A)) 1)
ERROR

((LAMBDA (#*10) (LIST 1 2 3)))
ERROR

((LAMBDA (X Y) ((LAMBDA (A B) (LIST A B)) (QUOTE U) (QUOTE V))) 5 6)
(U V)

((LAMBDA (X Y) (LIST X Y)) 1)
ERROR

((LAMBDA (X &REST Y &OPTIONAL (Z 5)) (LIST X Y Z)) 1 3)
ERROR

((LAMBDA (X &X) (LIST X)) 7)
ERROR

((LAMBDA (X &AUX) (LIST X)) 6)
(6)

((LAMBDA (X &AUX Y) (LIST X Y)) 6)
(6 NIL)

((LAMBDA (X &AUX (Y)) (LIST X Y)) 6)
(6 NIL)

((LAMBDA (X &REST) (LIST X)) 2)
ERROR

((LAMBDA (X &KEY) (LIST X)) 3)
(3)

((LAMBDA (X &KEY Y) (LIST X)) 3)
(3)

((LAMBDA (X &KEY Y) (LIST X)) 3 :Y)
ERROR

((LAMBDA (X &KEY Y) (LIST X)) :\3)
(:\3)

((LAMBDA NIL (LIST 1 2 3)))
(1 2 3)

((LAMBDA NIL (LIST 1 2 3)) 4 5)
ERROR

((LAMBDA (LIST 1 2 3)))
ERROR

((LAMBDA (X)))
ERROR

((LAMBDA (&AUX &KEY &REST &OPTIONAL)))
ERROR

((LAMBDA (A B &KEY C D &ALLOW-OTHER-KEYS) (LIST A B C D E F)) 1 2 :C
6 :D 8 :E 5
:F 7)
ERROR

((LAMBDA (X &ALLOW-OTHER-KEYS) (LIST X Y)) 2 :Y 3)
ERROR

((LAMBDA))
ERROR

;; CLHS 3.4.1.4.1.1

((LAMBDA (&KEY X) X) :X 1 :Y 2 :ALLOW-OTHER-KEYS T)
1

((LAMBDA (&KEY X) X) :X 1 :Y 2 :ALLOW-OTHER-KEYS T :ALLOW-OTHER-KEYS NIL)
1

((LAMBDA (&KEY X) X) :X 1 :Y 2 :ALLOW-OTHER-KEYS NIL :ALLOW-OTHER-KEYS T)
ERROR

((LAMBDA (&KEY X &ALLOW-OTHER-KEYS) X) :X 1 :Y 2)
1

((LAMBDA (&KEY X &ALLOW-OTHER-KEYS) X)
 :X 1 :Y 2 :ALLOW-OTHER-KEYS T :ALLOW-OTHER-KEYS NIL)
1

((LAMBDA (&KEY X &ALLOW-OTHER-KEYS) X)
 :X 1 :Y 2 :ALLOW-OTHER-KEYS NIL :ALLOW-OTHER-KEYS T)
1

((LAMBDA (&KEY X) X) :X 1 :ALLOW-OTHER-KEYS NIL)
1

((LAMBDA (&KEY X) X) :X 1 :ALLOW-OTHER-KEYS NIL :ALLOW-OTHER-KEYS NIL)
1

;; function-lambda-expression:
(defun foo (x) (list x))  foo

(multiple-value-list (function-lambda-expression #'foo))
#+CLISP
((LAMBDA (X) (DECLARE (SYSTEM::IN-DEFUN FOO)) (BLOCK FOO (LIST X)))
 #(NIL NIL NIL NIL ((DECLARATION OPTIMIZE DECLARATION)))
 foo)
#+CMU
((lambda (x) (block foo (list x))) nil foo)
#+SBCL
(NIL T FOO)
#+OpenMCL
(NIL NIL FOO)
#+LISPWORKS
((LAMBDA (X) (DECLARE (LAMBDA-NAME FOO)) (BLOCK FOO (LIST X))) NIL FOO)
#-(or CLISP CMU SBCL OpenMCL LISPWORKS)
UNKNOWN

(compile 'foo) foo

(multiple-value-list (function-lambda-expression #'foo))
#+CLISP
((lambda (x) (list x)) t foo)
#+CMU
((lambda (x) (block foo (list x))) nil foo)
#+SBCL
(NIL T FOO)
#+OpenMCL
(NIL NIL FOO)
#+LISPWORKS
(NIL NIL FOO)
#-(or CLISP CMU SBCL OpenMCL LISPWORKS)
UNKNOWN

(fmakunbound 'foo) foo

(defun (setf foo) (v a) (setf (car a) v)) (setf foo)

(multiple-value-list (function-lambda-expression #'(setf foo)))
#+CLISP
((LAMBDA (V A) (DECLARE (SYSTEM::IN-DEFUN (SETF FOO)))
  (BLOCK FOO (SETF (CAR A) V)))
 #(NIL NIL NIL NIL ((DECLARATION OPTIMIZE DECLARATION)))
 (SETF FOO))
#+CMU
((lambda (v a) (block foo (setf (car a) v))) nil (setf foo))
#+SBCL
(NIL T (SETF FOO))
#+OpenMCL
(NIL NIL (SETF FOO))
#+LISPWORKS
((LAMBDA (X) (DECLARE (LAMBDA-NAME (SETF FOO)))
  (BLOCK FOO (SETF (CAR A) V))) NIL (SETF FOO))
#-(or CLISP CMU SBCL OpenMCL LISPWORKS)
UNKNOWN

(compile '(setf foo)) (setf foo)

(multiple-value-list (function-lambda-expression #'(setf foo)))
#+CLISP
((LAMBDA (V A) (SETF (CAR A) V)) T (SETF FOO))
#+CMU
((lambda (v a) (block foo (setf (car a) v))) nil (setf foo))
#+SBCL
(NIL T (SETF FOO))
#+OpenMCL
(NIL NIL (SETF FOO))
#+LISPWORKS
(NIL NIL (SETF FOO))
#-(or CLISP CMU SBCL OpenMCL LISPWORKS)
UNKNOWN

(fmakunbound '(setf foo)) (setf foo)

;; disassemble
#+clisp (setf (getenv "PAGER") "cat") #+clisp "cat"
#-(and clisp (or win32 cygwin BeOS)) (disassemble 'car) nil
#-(and clisp (or win32 cygwin BeOS)) (disassemble #'cdr) nil
(disassemble 'disassemble) nil
(disassemble #'set-difference) nil
