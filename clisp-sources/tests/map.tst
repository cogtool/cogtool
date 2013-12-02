;; -*- Lisp -*-

(setf a-vector (make-array 10))
#+(or XCL CMU SBCL OpenMCL) #(0 0 0 0 0 0 0 0 0 0)
#+(or CLISP AKCL ECL ALLEGRO LISPWORKS) #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
#-(or XCL CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(do ((i 0 (1+ i))
     (n (length a-vector)))
    ((= i n))
    (when (null (aref a-vector i))
          (setf (aref a-vector i) 0)))
nil

(setq liste '(a b c d))
(a b c d)

(setq x 'anfangswert-von-x)
anfangswert-von-x

(do ((x liste (cdr x))
     (oldx x x))
    ((null x))
    (print oldx) (print x))
nil

(defun list-reverse (list)
  (do ((x list (cdr x))
       (y '() (cons (car x) y)))
      ((endp x) y)))
list-reverse

(list-reverse '(a b c d))
(d c b a)

(setq foo '(a b c d))
(a b c d)

(setq bar '(1 2 3 4))
(1 2 3 4)

(defun fkt(a b) (cons a b))
fkt

;; mapcar

(mapcar #'abs '(3 -4 2 -5 -6))
(3 4 2 5 6)

(mapcar #'cons '(a b c) '(1 2 3))
((a . 1) (b . 2) (c . 3))


(mapcar #'fkt foo bar)
((a . 1)(b . 2)(c . 3)(d . 4))

(do ((x foo (cdr x))
     (y bar (cdr y))
     (z '() (cons (fkt (car x) (car y)) z)))
    ((or (null x) (null y))
     (nreverse z)))
((a . 1)(b . 2)(c . 3)(d . 4))

;; dolist
(let ((l '(1 2 3))
      (r 0))
  (dolist (x l r)
    (setf r (+ r  x)) ))
6


;; dolist
(let ((l '(1 2 3)))
  (dolist (x l)(if (> 0 x)(incf x)(return 10))))
10

(let ((l '(1 2 3)))
  (dolist (x l )(incf x)))
nil

;; dotimes
(let ((s 0))
  (dotimes (i (+ 1 9)s)(setf s (+ s i))))
45

(dolist (x '(a b c d)) (prin1 x) (princ " "))
nil

(defun palindromep (string &optional
                    (start 0)
                    (end (length string)))
  (dotimes (k (floor (- end start) 2) t)
    (unless (char-equal (char string (+ start k))
                        (char string (- end k 1)))
      (return nil))))
palindromep

(palindromep "Able was I ere I saw Elba")
t

(palindromep "einnegermitgazellezagtimregennie")
t

(palindromep "eisgekuehlter bommerlunder")
nil

(palindromep (remove-if-not #'alpha-char-p
                            "A man, a plan, a canal -- Panama"))
t

(MAPCAR (FUNCTION (LAMBDA (X) (LIST X)))
        (QUOTE (A B C)))
((A) (B) (C))

(MAPCAR (FUNCTION (LAMBDA (X Y) (LIST X Y)))
        (QUOTE (A B C)) (QUOTE (1 2 3)))
((A 1) (B 2) (C 3))

(MAPCAR (FUNCTION (LAMBDA (X Y) (LIST X Y)))
        (QUOTE (A B C)) (QUOTE (1 2)))
((A 1) (B 2))

(MAPCAR (FUNCTION (LAMBDA (X Y) (LIST X Y)))
        (QUOTE (C)) (QUOTE (1 2)))
((C 1))

(MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y)))
        (QUOTE (C)) (QUOTE (1 2)) (U V W))
ERROR

(MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y)))
        (QUOTE (C)) (QUOTE (1 2)) (QUOTE (U V W)))
((C 1))

(MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y)))
        (QUOTE (A B C)) (QUOTE (1 2 3)) (QUOTE (U V W)))
((A 1) (B 2) (C 3))

(MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z)))
        (QUOTE (A B C)) (QUOTE (1 2 3)) (QUOTE (U V W)))
((A 1 U) (B 2 V) (C 3 W))

(MAPCAR #'(LAMBDA (X Y Z) (LIST X Y Z)) '(A B C) '(1 2 3) '(U V W . X))
((A 1 U) (B 2 V) (C 3 W))

(MAPCAR #'(LAMBDA (X Y Z) (LIST X Y Z)) '(A B C) '(1 2 3) '(U V . W))
ERROR

(funcall (compile nil (lambda ()
                        (mapcar #'(LAMBDA (X Y Z) (LIST X Y Z))
                                '(A B C) '(1 2 3) '(U V . W)))))
ERROR

;; mapc
(mapc #'abs '(3 -4 2 -5 -6))
(3 -4 2 -5 -6)

(MAPC (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z)))
      (QUOTE (A B C)) (QUOTE (1 2 3)) (QUOTE (U I V)))
(A B C)

(MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z)))
        (QUOTE (A B C)) (QUOTE (1 2 3)) (QUOTE (U I V)))
((A 1 U) (B 2 I) (C 3 V))

(mapl #'(lambda (x y) (cons x y)) '(a b c d) '(1 2 3 4))
(a b c d)

(MAPL (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z)))
      (QUOTE (A B C)) (QUOTE (1 2 3)) (QUOTE (U I V)))
(A B C)

;; maplist

(maplist #'(lambda (x) (cons 'foo x)) '(a b c d))
((foo a b c d)(foo b c d)(foo c d)(foo d))

(maplist #'(lambda (x) (if (member (car x) (cdr x)) 0 1))
         '(a b a c d b c))
(0 0 1 0 1 1 1)

(MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z)))
         (QUOTE (A B C)) (QUOTE (1 2 3)) (QUOTE (U I V)))
(((A B C) (1 2 3) (U I V)) ((B C) (2 3) (I V)) ((C) (3) (V)))

(MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z)))
         (QUOTE (A B C)) (QUOTE (1 2 3)) (QUOTE (U I)))
(((A B C) (1 2 3) (U I)) ((B C) (2 3) (I)))

(MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z)))
         (QUOTE (A B C)) (QUOTE (1 2)) (QUOTE (U I V)))
(((A B C) (1 2) (U I V)) ((B C) (2) (I V)))

(MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z)))
         (QUOTE (A B)) (QUOTE (1 2 3)) (QUOTE (U I V)))
(((A B) (1 2 3) (U I V)) ((B) (2 3) (I V)))

;; mapcon

(mapcon #'(lambda (x)(and (oddp (car x))(list (car x))))'(5 4 3 2 1))
(5 3 1)

(MAPCON (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z)))
        (QUOTE (A B)) (QUOTE (1 2 3)) (QUOTE (U I V)))
((A B) (1 2 3) (U I V) (B) (2 3) (I V))

(MAPCON (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z)))
        (QUOTE (A B C)) (QUOTE (1 2 3)) (QUOTE (U I V)))
((A B C) (1 2 3) (U I V) (B C) (2 3) (I V) (C) (3) (V))

(mapcon (constantly 1) (list 2))
1

#+CLISP
(maplap #'list '(a b) '(1 2 3) '(u i v))
#+CLISP
((A B) (1 2 3) (U I V) (B) (2 3) (I V))

#+CLISP
(maplap #'list '(a b c) '(1 2 3) '(u i v))
#+CLISP
((A B C) (1 2 3) (U I V) (B C) (2 3) (I V) (C) (3) (V))

;; mapcan

(mapcan #'(lambda (x) (and (numberp x) (list x))) '(a 1 b c 3 4 d 5))
(1 3 4 5)

(MAPCAN (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z)))
        (QUOTE (A B C)) (QUOTE (1 2 3)) (QUOTE (U I V)))
(A 1 U B 2 I C 3 V)

(MAPCAN (FUNCTION (LAMBDA (X Y) (LIST X Y)))
        (QUOTE (A B C)) (QUOTE (1 2 3)))
(A 1 B 2 C 3)

(MAPCAN (FUNCTION (LAMBDA (X) (LIST X))) (QUOTE (A B C)))
(A B C)

(MAPCON (FUNCTION (LAMBDA (X) (LIST X))) (QUOTE (A B C)))
((A B C) (B C) (C))

(MAPCON (FUNCTION (LAMBDA (X Y) (LIST X Y)))
        (QUOTE (A B C)) (QUOTE (1 2)))
((A B C) (1 2) (B C) (2))

(MAPCON (FUNCTION (LAMBDA (X) (LIST X))) (QUOTE (A B C)))
((A B C) (B C) (C))

(mapcan #'identity '(1))
1

(mapcan #'identity '(1 2 3))
#+(or CLISP ALLEGRO CMU SBCL OpenMCL) 3
#+(or AKCL ECL GCL LISPWORKS) ERROR
#-(or CLISP AKCL ECL GCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

#+CLISP
(mapcap #'list '(a b) '(1 2 3) '(u i v))
#+CLISP
(A 1 U B 2 I)

#+CLISP
(mapcap #'list '(a b c) '(1 2 3) '(u i v))
#+CLISP
(A 1 U B 2 I C 3 V)


; Clean up.
(unintern 'x)
T
