;;; -*- lisp -*-
;;; test file of examples from Steele
;;;

;; 7.3

(let ((f '+))
  (apply f '(1 2)))
3

(let ((f #'-))
  (apply f '(1 2)))
-1

(apply #'max 3 5 '(2 7 3))
7

(apply 'cons '((+ 2 3) 4))
((+ 2 3) . 4)

(apply #'+ '())
0

(apply #'(lambda (&key a b) (list a b)) '(:b 3))
(nil 3)

(funcall '+ 2 3)
5

(let ((c (symbol-function '+)))
  (funcall c 1 2 3 4))
10

;; 7.4

;;progn
(progn 1 2 3)
3

(progn (+ 2 1) 2)
2

(progn 1 2 (values  2 3))
2

(progn)
nil

;;prog1
(prog1 1 2 3)
1

(prog1 3 (+ 1 2) 2)
3

(prog1 (values  2 3) 1 2 )
2

(let ((x '(a b c)))
(prog1 (car x) (rplaca x 'foo)))
a

;;prog2
(prog2 1 2 3)
2

(prog2  (+ 1 2) 2 3)
2

(prog2 1 (values  2 3) 4)
2

;; 7.5

;;let
(setf a 0)
0

(let ((a 1) (b 2) c )
  (declare (integer a b))
  (list a b c))
(1 2 nil)


(let ((a 1) (b a))
  (declare (integer a b))
  (list a b))
(1 0)

(let (x239)
  (declare (special x239))
  (symbol-value 'x239))
nil

;let*
(let* ((a 1) (b 2) c )
  (declare (integer a b))
  (list a b c))
(1 2 nil)


(let* ((a 1) (b a))
  (declare (integer a b))
  (list a b))
(1 1)

;;compiler-let (?)

;;progv

(progv '(a b c) '(1 2 3) (+ a b c))
6

(progv '(a b c) '(1 2) (list a b c))
error

(let ((v '(a b c))
      (val '(3 2 1)))
  (progv v val (mapcar #'eval v)))
(3 2 1)

;;flet

(defun plus (&rest args) (apply #'+ args))
plus

(flet ((plus (a b) (+ a b))
       (minus (a b) (- a b)))
  (list (plus 1 2) (minus 1 2)))
(3 -1)

(list (flet ((plus (a b) (- a b))) (plus 3 2)) (+ 3 2))
(1 5)

(flet ((plus (a b) (plus (plus a b a) b))) (plus 3 2))
10

;;Labels
(labels ((queue (l) (if (car l) (queue (cdr l))'ende))) (queue '(1 2 3)))
ENDE

(labels ((plus (a b) (* a (plus a a b)))) (plus 1 2 3))
ERROR

;;macrolet ?

;; 7.6

;;if

(let ((a t) (b nil)) (list (if a 1 2) (if b 1 2) (if a 1) (if b 1)))
(1 2 1 nil)

;;when
(let ((a t) (b nil)) (list (when a 1 2) (when b 1 2) (when a 1)))
(2 nil 1)

;;unless
(let ((a t) (b nil)) (list (unless a 1 2) (unless b 1 2) (unless a 1)))
(nil 2 nil)

;;cond
(let ((a t) (b 10) (c nil))
  (list (cond (a 1) (t 'END)) (cond (b) (t 'END)) (cond (c 1) (t 'END))))
(1 10 END)

;;case
(case (+ 1 2)
  (1 -1)
  (2 -2)
  (3 -3))
-3

(case (+ 1 2)
  (1 -1)
  (2 -2))
nil

;;(case (+ 1 2)
;; (1 -1)
;; (2 -2)
;; (1 -1)
;; (3 -3))
;;ERROR ; because a key may appear only once

(case (+  1 2)
  ((1 3) -1)
  (2 -2)
  (otherwise 100))
-1

;;(case (+  1 2)
;; ((1 3) -1)
;; ((2 1) -2)
;; (t 100))
;;ERROR          ; because a key may appear only once

;;typecase
(typecase (+  1 2)
  (list -2)
  (null -3)
  (integer -1))
-1

;; 7.7

;;block
(block blocktest (if t (return 0) ) 1)
error

(block blocktest (if t (return-from blocktest 0) ) 1)
0

(block blocktest (if nil (return-from blocktest 0) ) 1)
1

(block blocktest (catch 'catcher
                   (if t (throw 'catcher 0) ) 1))
0

;; 7.8

;; 7.8.1

;;loop
(let ((i 10))
  (loop (if (< (decf i) 1) (return i))))
0

(let ((i 10))
  (catch 'catcher
    (loop (if (< (decf i) 1) (return i)))))
0

;; 7.8.2

;;do,do*
(setf a 0)
0

(do ((a 1 (+ a 1)) (b a))
    ((> a 9) (list b c))
  (setf c (+ a b)))
(0 9)

(do* ((a 1 (+ a 1)) (b a))
     ((> a 9) b))
1

(let ((a 0))
  (do* ((a 1 (+ a 1)) (b a))
       ((> a 9) a) (declare (integer a b)))
  a)
0

;; 7.8.3

;;dolist
(let ((l '(1 2 3))
      (r 0))
  (dolist (x l r)
    (setf r (+ r  x)) ))
6

;;dolist
(let ((l '(1 2 3)))
  (dolist (x l) (if (> 0 x) (incf x) (return 10))))
10

(let ((l '(1 2 3)))
  (dolist (x l ) (incf x)))
nil

;;dotimes
(let ((s 0))
  (dotimes (i (+ 1 9)s) (setf s (+ s i))))
45

;; 7.8.4

;;mapcar
(mapcar #'abs '(3 -4 2 -5 -6))
(3 4 2 5 6)

(mapcar #'cons '(a b c) '(1 2 3))
((a . 1) (b . 2) (c . 3))

;;maplist
(maplist #'(lambda (x) (cons 'foo x))'(a b c d))
((foo a b c d) (foo b c d) (foo c d) (foo d))

(maplist #'(lambda (x) (if (member (car x) (cdr x)) 0 1))
         '(a b a c d b c))
(0 0 1 0 1 1 1)

;;mapc
(mapc #'abs '(3 -4 2 -5 -6))
(3 -4 2 -5 -6)

;;mapl
(mapl #'(lambda (x y) (cons x y))'(a b c d)'(1 2 3 4))
(a b c d)

;;mapcan
(mapcan #'(lambda (x) (and (numberp x) (list x)))'(a 1 b c 3 4 d 5))
(1 3 4 5)

;;mapcon
(mapcon #'(lambda (x) (and (oddp (car x)) (list (car x))))'(5 4 3 2 1))
(5 3 1)

;; 7.8.5

;;tagbody
(let ((a 0))
  (tagbody (if nil (go tag0) (go tag1))
    (this will never be reached)
   tag0
    (setf a 1)
   tag1
    (setf a 2))
  a)
2

(let ((a 0))
  (tagbody (if t (go tag0) (go tag1))
    (this will never be reached)
   tag0
    (setf a 1))
  a)
;; CMUCL compiles on the fly and therefore signals an error
#-(or CMU LISPWORKS) 1
#+(or CMU LISPWORKS) ERROR

;;prog*
(let ((z '(1 0)))
  (prog* ((y z) (x (car y)))
    (return x)))
1

(prog (a (b 1))
  (if a (go tag0) (go tag1))
  (this will never be reached)
  tag0
  (setf a 1)
  (this will never be reached)
  tag1
  (setf a 2))
nil

(prog (a (b 1))
  (if a (return nil) (go tag1))
  (this will never be reached)
  tag0
  (return (list a 1))
  tag1
  (setf a 2)
  (go tag0))
(2 1)

;; 7.9

;;multiple-value-bind
(defun adder (x y) (values (+ 1 x) (+ 1 y) ) )
adder

(multiple-value-bind (a b) (adder 1 2) (+ a b))
5

(defun adder (x y) (values-list (list  (+ 1 x) (+ 1 y))))
adder

(multiple-value-bind (a b) (adder 1 2) (+ a b))
5

(multiple-value-list (floor -3 4))
(-1 1)

(multiple-value-call #'+ (floor 5 3) (floor 19 4))
10

(multiple-value-bind (c d)
    (multiple-value-prog1 (floor -3 4) (+ 1 2))
  (list c d))
(-1 1)

(multiple-value-bind (x) (floor 5 3) (list x))
(1)

(multiple-value-bind (x y) (floor 5 3) (list x y))
(1 2)

(multiple-value-bind (x y z) (floor 5 3) (list x y z))
(1 2 nil)

(multiple-value-setq (a b) (values 10 20))
10

b
20

;; 7.10

;;catch/throw/unwind-protect

;;; <https://sourceforge.net/tracker/?func=detail&aid=858011&group_id=1355&atid=101355>

(funcall (compile nil (lambda (x) (flet ((z (x) (return-from z x))) (z x)))) 7)
7

(flet ((z () (return-from z 6))) (z))
6

(funcall (compile nil (lambda () (labels ((z () (return-from z 5))) (z)))))
5

(labels ((z () (return-from z 4))) (z))
4

