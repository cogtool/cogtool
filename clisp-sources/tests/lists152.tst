;; -*- Lisp -*-

(ENDP 'NIL)
T

(ENDP '(A . B))
NIL

(ENDP '(A B . C))
NIL

(ENDP '(A B C))
NIL

(ENDP '(A B C D))
NIL

(ENDP '(A B C . D))
NIL

(ENDP '('NIL 'NIL))
NIL

(LIST-LENGTH 'NIL)
0

(LIST-LENGTH '(A . B))
#+XCL 1 #+(or CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) ERROR
#-(or XCL CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(LIST-LENGTH '(A B C D))
4

(LIST-LENGTH '(A (B C) D))
3

(LET ((X (LIST 'A 'B 'C)))
  (RPLACD (LAST X) X)
  (LIST-LENGTH X))
NIL

(NTH 0 '(A B C D))
A

(NTH 1 '(A B C D))
B

(NTH 3 '(A B C D))
D

(NTH 5 '(A B C D))
NIL

(NTH -2 '(A B C D))
ERROR

(NTH 0 'NIL)
NIL

(NTH 2 'NIL)
NIL

(FIRST '(1 2 3 4 5 6 7 8 9 10 11))
1

(SECOND '(1 2 3 4 5 6 7 8 9 10 11))
2

(THIRD '(1 2 3 4 5 6 7 8 9 10 11))
3

(FOURTH '(1 2 3 4 5 6 7 8 9 10 11))
4

(FIFTH '(1 2 3 4 5 6 7 8 9 10 11))
5

(SIXTH '(1 2 3 4 5 6 7 8 9 10 11))
6

(SEVENTH '(1 2 3 4 5 6 7 8 9 10 11))
7

(EIGHTH '(1 2 3 4 5 6 7 8 9 10 11))
8

(NINTH '(1 2 3 4 5 6 7 8 9 10 11))
9

(TENTH '(1 2 3 4 5 6 7 8 9 10 11))
10

(FIRST '(1 2 3))
1

(SECOND '(1 2 3))
2

(THIRD '(1 2 3))
3

(FOURTH '(1 2 3))
NIL

(FIFTH '(1 2 3))
NIL

(SIXTH '(1 2 3))
NIL

(SEVENTH '(1 2 3))
NIL

(EIGHTH '(1 2 3))
NIL

(NINTH '(1 2 3))
NIL

(TENTH '(1 2 3))
NIL

(FIRST 'NIL)
NIL

(SECOND 'NIL)
NIL

(THIRD 'NIL)
NIL

(FOURTH 'NIL)
NIL

(FIFTH 'NIL)
NIL

(SIXTH 'NIL)
NIL

(SEVENTH 'NIL)
NIL

(EIGHTH 'NIL)
NIL

(NINTH 'NIL)
NIL

(TENTH 'NIL)
NIL

(REST '(1 2 3 4 5))
(2 3 4 5)

(REST 'NIL)
NIL

(REST '(A . B))
B

(REST '(1 2 3 . 4))
(2 3 . 4)

(NTHCDR 0 '(A B C D))
(A B C D)

(NTHCDR 1 '(A B C D))
(B C D)

(NTHCDR 3 '(A B C D))
(D)

(NTHCDR 5 '(A B C D))
NIL

(NTHCDR -2 '(A B C D))
ERROR

(NTHCDR 0 'NIL)
NIL

(NTHCDR 2 'NIL)
NIL

(LAST '(1 2 3 4 5))
(5)

(LAST 'NIL)
NIL

(LAST '(A . B))
(A . B)

(LAST '(1 2 3 . 4))
(3 . 4)

(LIST 'A 'B 'C 'D)
(A B C D)

(LIST 'A)
(A)

(LIST '(A B) '(C D))
((A B) (C D))

(LIST 'A 'NIL)
(A NIL)

(LIST 'NIL 'A)
(NIL A)

(LIST 'NIL 'NIL)
(NIL NIL)

(LIST)
NIL

(LIST 3 4 'A
      (CAR '(B . C))
      (+ 6 -2))
(3 4 A B 4)

(LIST* 'A 'B 'C 'D)
(A B C . D)

(LIST* 'A)
A

(LIST* '(A B)
       '(C D))
((A B)
 C D)

(LIST* 'A 'NIL)
(A)

(LIST* 'NIL 'A)
(NIL . A)

(LIST* 'NIL 'NIL)
(NIL)

(LIST*)
ERROR

(LIST* 3 4 'A
       (CAR '(B . C))
       (+ 6 -2))
(3 4 A B . 4)

(LIST* 'A 'B 'C
       '(D E F))
(A B C D E F)

(LIST* X)
ERROR

(LIST* 'NIL)
NIL

(MAKE-LIST 5)
(NIL NIL NIL NIL NIL)

(MAKE-LIST 5 :INITIAL-ELEMENT)
ERROR

(MAKE-LIST 3 :INITIAL-ELEMENT 'RAH)
(RAH RAH RAH)

(MAKE-LIST 0)
NIL

(MAKE-LIST 0 :INITIAL-ELEMENT 'AAA)
NIL

(MAKE-LIST 5 :INITIAL-ELEMENT 'NIL)
(NIL NIL NIL NIL NIL)

(MAKE-LIST)
ERROR

(APPEND '(A B C)
        '(D E F)
        'NIL
        '(G))
(A B C D E F G)

(APPEND '(A B C)
       'D)
(A B C . D)

(APPEND 'A 'B)
ERROR

(APPEND 'A 'NIL)
ERROR

(APPEND 'NIL 'NIL)
NIL

(APPEND 'NIL 'A)
#+XCL ERROR
#-XCL A

(APPEND 'NIL
       '(A B C))
(A B C)

(SETQ X '(A B C))
(A B C)

(SETQ Y '(D E F))
(D E F)

(SETQ R (APPEND X Y))
(A B C D E F)

X
(A B C)

Y
(D E F)

(EQ (CDDDR R) Y)
T

(COPY-LIST '(1 2 3 4 5))
(1 2 3 4 5)

(COPY-LIST 'NIL)
NIL

(COPY-LIST '(A . B))
(A . B)

(COPY-LIST '(1 2 3 . 4))
(1 2 3 . 4)

(SETQ L '(1 2 3 4 5))
(1 2 3 4 5)

(EQ L (COPY-LIST L))
NIL

(EQL L (COPY-LIST L))
NIL

(EQUAL L (COPY-LIST L))
T

(EQUALP L (COPY-LIST L))
T

(COPY-ALIST 'A)
ERROR

(COPY-ALIST 'NIL)
NIL

(COPY-ALIST 5)
ERROR

(COPY-ALIST '(A B))
#+(or XCL CLISP ECL ALLEGRO CMU SBCL LISPWORKS) (A B) #+(or GCL OpenMCL) ERROR
#-(or XCL CLISP ECL GCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(COPY-ALIST '((1 . A)
              (2 . B)
              (3 . C)))
((1 . A)
 (2 . B)
 (3 . C))

(SETQ X '((1 . A)
          (2 . B)
          (3 . C)))
((1 . A)
 (2 . B)
 (3 . C))

(EQ X (COPY-ALIST X))
NIL

(EQL X (COPY-ALIST X))
NIL

(EQUAL X (COPY-ALIST X))
T

(EQ (CADR X)
    (CADR (COPY-ALIST X)))
NIL

(EQL (CADR X)
     (CADR (COPY-ALIST X)))
NIL

(EQUAL (CADR X)
       (CADR (COPY-ALIST X)))
T

(COPY-ALIST '((1 . 2))
            '((A . B)))
ERROR

(COPY-ALIST '((A B)
              C
              (D E)))
#+(or XCL CLISP ECL ALLEGRO CMU SBCL LISPWORKS) ((A B) C (D E)) #+(or GCL OpenMCL) ERROR
#-(or XCL CLISP ECL GCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(COPY-TREE 'X)
X

(COPY-TREE 5)
5

(COPY-TREE '(A B))
(A B)

(COPY-TREE '(A B
               (C (D)
                  (E F))
               G))
(A B
   (C (D)
      (E F))
   G)

(COPY-TREE '((1 . E)
             (2 . F)))
((1 . E)
 (2 . F))

(COPY-TREE #*001)
#*001

(SETQ X
      '(A B
          (C D)
          E))
(A B
   (C D)
   E)

(EQ X
    (COPY-TREE X))
NIL

(EQL X
     (COPY-TREE X))
NIL

(EQUAL X
       (COPY-TREE X))
T

(EQ (CDADDR X)
    (CDADDR (COPY-TREE X)))
NIL

(EQL (CDADDR X)
     (CDADDR (COPY-TREE X)))
NIL

(EQUAL (CDADDR X)
       (CDADDR (COPY-TREE X)))
T

(REVAPPEND '(A B C)
       '(D E F)
       'NIL
       '(G))
ERROR

(REVAPPEND '(A B C)
       'D)
(C B A . D)

(REVAPPEND 'A 'B)
ERROR

(REVAPPEND 'A 'NIL)
ERROR

(REVAPPEND 'NIL 'NIL)
NIL

(REVAPPEND 'NIL 'A)
A

(REVAPPEND 'NIL
           '(A B C))
(A B C)

(REVAPPEND '(A B C)
           '(D E F))
(C B A D E F)

(REVAPPEND '(D E F)
           '(A B C))
(F E D A B C)

(EQL (REVAPPEND '(A B C)
            '(D E F))
     (APPEND (REVERSE '(A B C))
            '(D E F)))
NIL

(EQUAL (REVAPPEND '(A B C)
                  '(D E F))
       (APPEND (REVERSE '(A B C))
               '(D E F)))
T

(SETQ X '(A B C))
(A B C)

(SETQ Y '(D E F))
(D E F)

(SETQ R (REVAPPEND X Y))
(C B A D E F)

X
(A B C)

Y
(D E F)

(EQ (CDDDR R)
    Y)
T

(SETQ X
      '(A B C)
      Y
      '(D E F))
(D E F)

(NCONC X Y)
(A B C D E F)

X
(A B C D E F)

(EQ (CDDDR X)
    Y)
T

(SETQ X
      '(A B C)
      Y
      '(D E F)
      Z
      '(G H I))
(G H I)

(NCONC)
NIL

(NCONC X)
(A B C)

(NCONC NIL)
NIL

(NCONC NIL NIL)
NIL

(NCONC X NIL)
(A B C)

(NCONC NIL NIL NIL NIL)
NIL

(NCONC NIL NIL X NIL)
(A B C)

(NCONC X NIL Y NIL Z NIL)
(A B C D E F G H I)

X
(A B C D E F G H I)

Y
(D E F G H I)

Z
(G H I)

(EQ (CDDDR X)
    Y)
T

(EQ (CDDDR Y)
    Z)
T

(NCONC '(1 2) 'A)
#+XCL ERROR #+(or CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) (1 2 . A)
#-(or XCL CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(NCONC 'A)
#+XCL ERROR #+(or CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) A
#-(or XCL CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(SETQ X
      '(A B C)
      Y
      '(D E F))
(D E F)

(NRECONC X Y)
(C B A D E F)

X
#+XCL WAS-DESTROYED ; wo kommt denn so was her?
#+CLISP (C B A D E F)
#+(or AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) (A D E F)
#-(or XCL CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(TAILP Y X)
T

(SETQ X
      '(A B C)
      Y
      '(D E F)
      Z
      '(G H I))
(G H I)

(NRECONC)
ERROR

(NRECONC X)
ERROR

(NRECONC NIL)
ERROR

(NRECONC NIL NIL)
NIL

(NRECONC X NIL)
(C B A)

X
#+XCL WAS-DESTROYED
#+CLISP (C B A)
#+(or AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) (A)
#-(or XCL CLISP AKCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(NRECONC NIL NIL NIL NIL)
ERROR

(NCONC NIL 'X)
#+XCL ERROR
#+(or CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) X
#-(or XCL CLISP AKCL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

(SETQ AA NIL)
NIL

(PUSH '1 AA)
(1)

(PUSH '2 AA)
(2 1)

(PUSH '2 AA)
(2 2 1)

(SETQ AA
      '(B A))
(B A)

(PUSHNEW 'A AA)
(B A)

(PUSHNEW 'C AA)
(C B A)

(SETQ XXX NIL)
NIL

(PUSHNEW 'C XXX :TEST 'EQUAL)
(C)

(PUSHNEW 'C XXX :TEST 'EQUAL)
(C)

(PUSHNEW '(C) XXX :TEST 'EQUAL)
((C) C)

XXX
((C) C)

(SETQ XX '(NIL KKK))
(NIL KKK)

(PUSHNEW 'U (CAR XX))
(U)

(PUSHNEW 'U (CAR XX))
(U)

(PUSHNEW 'V (CAR XX))
(V U)

XX
((V U) KKK)

(PUSHNEW '(W) (CAR XX))
((W)
 V U)

(PUSHNEW '(W) (CAR XX))
((W)
 (W)
 V U)

(PUSHNEW '(W) (CAR XX)
         :TEST 'EQUAL)
((W)
 (W)
 V U)

(PUSHNEW '(W) (CAR XX)
         :TEST-NOT 'EQUAL)
((W)
 (W)
 V U)

(SETQ AA '(1 2 3))
(1 2 3)

(POP AA)
1

AA
(2 3)

(POP AA)
2

(POP AA)
3

(POP AA)
NIL

(POP AA)
NIL

(BUTLAST '(A B C))
(A B)

(BUTLAST '(A B C)
       2)
(A)

(NBUTLAST '(A B C D)
       3)
(A)

(NBUTLAST '(A B C D)
       1)
(A B C)

(NBUTLAST '(A B C D)
       0)
(A B C D)

(NBUTLAST '(A B C D)
       4)
NIL

(NBUTLAST '(A B C D)
       6)
NIL

(butlast '#1=(1 2 3 . #1#) 3)
ERROR

;; <http://www.lisp.org/HyperSpec/Body/fun_revappendcm_nreconc.html>
(let ((list-1 (list 1 2 3))
      (list-2 (list 'a 'b 'c)))
  (list (revappend list-1 list-2)
        (equal list-1 '(1 2 3))
        (equal list-2 '(a b c))))
((3 2 1 A B C) T T)

(revappend '(1 2 3) '())
(3 2 1)

(revappend '(1 2 3) '(a . b))
(3 2 1 A . B)

(revappend '() '(a b c))
(A B C)

(revappend '(1 2 3) 'a)
(3 2 1 . A)

(revappend '() 'a)
A   ;degenerate case

(let ((list-1 (list '1 '2 '3))
      (list-2 (list 'a 'b 'c)))
  (list (nreconc list-1 list-2)
        (equal list-1 '(1 2 3))
        (equal list-2 '(a b c))))
((3 2 1 A B C) NIL T)

;; <http://www.lisp.org/HyperSpec/Body/fun_nconc.html>
(nconc)
NIL

(setq x '(a b c))
(A B C)

(setq y '(d e f))
(D E F)

(nconc x y)
(A B C D E F)

x
(A B C D E F)

(setq foo (list 'a 'b 'c 'd 'e)
      bar (list 'f 'g 'h 'i 'j)
      baz (list 'k 'l 'm))
(K L M)

(setq foo (nconc foo bar baz))
(A B C D E F G H I J K L M)

foo     (A B C D E F G H I J K L M)
bar     (F G H I J K L M)
baz     (K L M)

(setq foo (list 'a 'b 'c 'd 'e)
      bar (list 'f 'g 'h 'i 'j)
      baz (list 'k 'l 'm))
(K L M)

(setq foo (nconc nil foo bar nil baz))
(A B C D E F G H I J K L M)

foo     (A B C D E F G H I J K L M)
bar     (F G H I J K L M)
baz     (K L M)

;; <http://www.lisp.org/HyperSpec/Body/fun_append.html>
(append '(a b c) '(d e f) '() '(g))
(A B C D E F G)

(append '(a b c) 'd)
(A B C . D)

(setq lst '(a b c))
(A B C)

(append lst '(d))
(A B C D)

lst
(A B C)

(append)
NIL

(append 'a)
A


; Clean up.
(unintern 'x)
T
