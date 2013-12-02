;; -*- Lisp -*-

#+ALLEGRO
(progn
  (defun make-weak-pointer (v)
    (make-array 1 :weak t :initial-contents (list v)))
  (defun weak-pointer-value (wv)
    (let ((v (aref wv 0))) (values v (not (null v)))))
  (defun set-weak-pointer-value (wv v) (setf (aref wv 0) v))
  (defsetf weak-pointer-value set-weak-pointer-value)
  t)
#+ALLEGRO
T

#+LISPWORKS
(progn
  (defun gc () (mark-and-sweep 3))
  (defun make-weak-pointer (v)
    (let ((wp (make-array 1)))
      (setf (aref wp 0) v)
      (set-array-weak wp t)
      wp))
  (defun weak-pointer-value (wv)
    (let ((v (aref wv 0))) (values v (not (null v)))))
  (defun set-weak-pointer-value (wv v) (setf (aref wv 0) v))
  (defsetf weak-pointer-value set-weak-pointer-value)
  t)
#+LISPWORKS
T

(defmacro weakptr-test (&body body)
  `(progn (make-list 100) ,@body (make-array 200)
          (list (eq co (weak-pointer-value wp))
                (multiple-value-list (weak-pointer-value wp))
                (multiple-value-bind (v p) (weak-pointer-value wpp)
                  (list #+(or ALLEGRO LISPWORKS) (if (arrayp v) 'WEAK-POINTER (type-of v))
                        #-(or ALLEGRO LISPWORKS) (type-of v)
                        p)))))
weakptr-test

(weakptr-test (setq co (cons 1 2) wp (make-weak-pointer co)
                    wpp (make-weak-pointer wp)))
(T ((1 . 2) T) (WEAK-POINTER T))

(weakptr-test (gc))
(T ((1 . 2) T) (WEAK-POINTER T))

(weakptr-test (setq co nil) (gc))
(T (NIL NIL) (WEAK-POINTER T))

(weakptr-test (setq co (cons 1 2) wp (make-weak-pointer 1)))
(NIL (1 T) (WEAK-POINTER T))

(weakptr-test (setf (weak-pointer-value wp) co) (gc))
(T ((1 . 2) T) (NULL NIL))

(weakptr-test (setf (weak-pointer-value wp) 2 (weak-pointer-value wpp) co)
              (gc))
(NIL (2 T) (CONS T))

(weakptr-test (setf (weak-pointer-value wp) co (weak-pointer-value wpp) wp)
              (gc))
(T ((1 . 2) T) (WEAK-POINTER T))

(weakptr-test (setf (weak-pointer-value wp) 3 co nil) (gc))
(NIL (3 T) (WEAK-POINTER T))

(weakptr-test (setf (weak-pointer-value wp) (cons 1 2)) (gc))
(T (NIL NIL) (WEAK-POINTER T))

(let ((*print-circle* t))
  (setf (weak-pointer-value wp) wpp)
  (prin1-to-string wp))
#+CLISP "#1=#<WEAK-POINTER #<WEAK-POINTER #1#>>"
#+CMU "#1=#<Weak Pointer: #<Weak Pointer: #1#>>"
#+LISPWORKS "#1=#(#(#1#))"
#-(or CLISP CMU LISPWORKS) UNKNOWN

(progn (makunbound 'co) (makunbound 'wp) (makunbound 'wpp) (gc)
       (fmakunbound 'weakptr-test))
weakptr-test
