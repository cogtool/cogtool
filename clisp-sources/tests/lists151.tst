
(MAKUNBOUND 'A)
A

(MAKUNBOUND 'X)
X

(CAR '(A B C D E F G))
A

(CDR '(A B C D E F G))
(B C D E F G)

(CAAR '((A) B C D E F G))
A

(CADR '(A B C D E F G))
B

(CDAR '((A B) C D E F G))
(B)

(CDDR '(A B C D E F G))
(C D E F G)

(CAAAR '(((A)) B C D E F G))
A

(CAADR '(A (B) C D E F G))
B

(CADAR '((A B) C D E F G))
B

(CADDR '(A B C D E F G))
C

(CDAAR '(((A B)) C D E F G))
(B)

(CDADR '(A (B C) D E F G))
(C)

(CDDAR '((A B C) D E F G))
(C)

(CDDDR '(A B C D E F G))
(D E F G)

(CAAAAR '((((A))) B C D E F G))
A

(CAAADR '(A ((B)) C D E F G))
B

(CAADAR '((A (B)) C D E F G))
B

(CAADDR '(A B (C) D E F G))
C

(CADAAR '(((A B)) C D E F G))
B

(CADADR '(A (B C) D E F G))
C

(CADDAR '((A B C) D E F G))
C

(CADDDR '(A B C D E F G))
D

(CDAAAR '((((A B))) C D E F G))
(B)

(CDAADR '(A ((B C)) D E F G))
(C)

(CDADAR '((A (B C)) D E F G))
(C)

(CDADDR '(A B (C D) E F G))
(D)

(CDDAAR '(((A B C)) D E F G))
(C)

(CDDADR '(A (B C D) E F G))
(D)

(CDDDAR '((A B C D) E F G))
(D)

(CDDDDR '(A B C D E F G))
(E F G)

(CAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
((((1 2 3) 4) 5) (6 7))

(CDR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
((((U V W) X) Y) ((Q W E) R) (A B C) E F G)

(CAAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
(((1 2 3) 4) 5)

(CADR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
(((U V W) X) Y)

(CDAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
((6 7))

(CDDR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
(((Q W E) R) (A B C) E F G)

(CAAAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
((1 2 3) 4)

(CAADR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
((U V W) X)

(CADAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
(6 7)

(CADDR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
((Q W E) R)

(CDAAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
(5)

(CDADR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
(Y)

(CDDAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
NIL

(CDDDR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B C)
E F G))
((A B C) E F G)

(CAAAAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
(1 2 3)

(CAAADR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
(U V W)

(CAADAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
6

(CAADDR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
(Q W E)

(CADAAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
5

(CADADR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
Y

(CADDAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
NIL

(CADDDR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
(A B C)

(CDAAAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
(4)

(CDAADR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
(X)

(CDADAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
(7)

(CDADDR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
(R)

(CDDAAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
NIL

(CDDADR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
NIL

(CDDDAR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
NIL

(CDDDDR '(((((1 2 3) 4) 5) (6 7)) (((U V W) X) Y) ((Q W E) R) (A B
C) E F G))
(E F G)

(CAR 'NIL)
NIL

(CDR 'NIL)
NIL

(CAAR 'NIL)
NIL

(CADR 'NIL)
NIL

(CDAR 'NIL)
NIL

(CDDR 'NIL)
NIL

(CAAAR 'NIL)
NIL

(CAADR 'NIL)
NIL

(CADAR 'NIL)
NIL

(CADDR 'NIL)
NIL

(CDAAR 'NIL)
NIL

(CDADR 'NIL)
NIL

(CDDAR 'NIL)
NIL

(CDDDR 'NIL)
NIL

(CAAAAR 'NIL)
NIL

(CAAADR 'NIL)
NIL

(CAADAR 'NIL)
NIL

(CAADDR 'NIL)
NIL

(CADAAR 'NIL)
NIL

(CADADR 'NIL)
NIL

(CADDAR 'NIL)
NIL

(CADDDR 'NIL)
NIL

(CDAAAR 'NIL)
NIL

(CDAADR 'NIL)
NIL

(CDADAR 'NIL)
NIL

(CDADDR 'NIL)
NIL

(CDDAAR 'NIL)
NIL

(CDDADR 'NIL)
NIL

(CDDDAR 'NIL)
NIL

(CDDDDR 'NIL)
NIL

(CAR '(A B C D E F G))
A

(CDR '(A B C D E F G))
(B C D E F G)

(CAAR '(A B C D E F G))
ERROR

(CADR '(A B C D E F G))
B

(CDAR '(A B C D E F G))
ERROR

(CDDR '(A B C D E F G))
(C D E F G)

(CAAAR '(A B C D E F G))
ERROR

(CAADR '(A B C D E F G))
ERROR

(CADAR '(A B C D E F G))
ERROR

(CADDR '(A B C D E F G))
C

(CDAAR '(A B C D E F G))
ERROR

(CDADR '(A B C D E F G))
ERROR

(CDDAR '(A B C D E F G))
ERROR

(CDDDR '(A B C D E F G))
(D E F G)

(CAAAAR '(A B C D E F G))
ERROR

(CAAADR '(A B C D E F G))
ERROR

(CAADAR '(A B C D E F G))
ERROR

(CAADDR '(A B C D E F G))
ERROR

(CADAAR '(A B C D E F G))
ERROR

(CADADR '(A B C D E F G))
ERROR

(CADDAR '(A B C D E F G))
ERROR

(CADDDR '(A B C D E F G))
D

(CDAAAR '(A B C D E F G))
ERROR

(CDAADR '(A B C D E F G))
ERROR

(CDADAR '(A B C D E F G))
ERROR

(CDADDR '(A B C D E F G))
ERROR

(CDDAAR '(A B C D E F G))
ERROR

(CDDADR '(A B C D E F G))
ERROR

(CDDDAR '(A B C D E F G))
ERROR

(CDDDDR '(A B C D E F G))
(E F G)

(CAR '(A))
A

(CDR '(A))
NIL

(CAAR '(A))
ERROR

(CADR '(A))
NIL

(CDAR '(A))
ERROR

(CDDR '(A))
NIL

(CAAAR '(A))
ERROR

(CAADR '(A))
NIL

(CADAR '(A))
ERROR

(CADDR '(A))
NIL

(CDAAR '(A))
ERROR

(CDADR '(A))
NIL

(CDDAR '(A))
ERROR

(CDDDR '(A))
NIL

(CAAAAR '(A))
ERROR

(CAAADR '(A))
NIL

(CAADAR '(A))
ERROR

(CAADDR '(A))
NIL

(CADAAR '(A))
ERROR

(CADADR '(A))
NIL

(CADDAR '(A))
ERROR

(CADDDR '(A))
NIL

(CDAAAR '(A))
ERROR

(CDAADR '(A))
NIL

(CDADAR '(A))
ERROR

(CDADDR '(A))
NIL

(CDDAAR '(A))
ERROR

(CDDADR '(A))
NIL

(CDDDAR '(A))
ERROR

(CDDDDR '(A))
NIL

(CONS 1 2)
(1 . 2)

(CONS 'A 'B)
(A . B)

(CONS 'A 'B 'C)
ERROR

(CONS 'A)
ERROR

(CONS)
ERROR

(CONS 'A 'NIL)
(A)

(CONS 'NIL 'A)
(NIL . A)

(CONS 'A (CONS 'B (CONS 'C 'NIL)))
(A B C)

(CONS 'A '(B C D))
(A B C D)

(TREE-EQUAL 1 1)
T

(TREE-EQUAL 'WORD 'WORD)
T

(TREE-EQUAL 'WORD1 'WORD2)
NIL

(TREE-EQUAL '(A B) '(A B))
T

(TREE-EQUAL '(A (B C)) '((A B) C))
NIL

(TREE-EQUAL 5 (+ 2 3))
T

(TREE-EQUAL '(A (B QUOTE NIL)) '(A (B)))
NIL

(TREE-EQUAL '(A (B . 1.0)) '(A (B #C(1.0 0.0))))
NIL

(TREE-EQUAL 1 1 :TEST #'EQ)
T

(TREE-EQUAL 'WORD 'WORD :TEST #'EQ)
T

(TREE-EQUAL 'WORD1 'WORD2 :TEST #'EQ)
NIL

(TREE-EQUAL '(A B) '(A B) :TEST #'EQ)
T

(TREE-EQUAL '(A (B C)) '((A B) C) :TEST #'EQ)
NIL

(TREE-EQUAL 5 (+ 2 3) :TEST #'EQ)
T

(TREE-EQUAL '(A (B)) '(A (B)) :TEST #'EQ)
T

(TREE-EQUAL '(A (B . 1.0)) '(A (B #C(1.0 0.0))) :TEST #'EQ)
NIL

(TREE-EQUAL 1 1 :TEST #'EQL)
T

(TREE-EQUAL 'WORD 'WORD :TEST #'EQL)
T

(TREE-EQUAL 'WORD1 'WORD2 :TEST #'EQL)
NIL

(TREE-EQUAL '(A B) '(A B) :TEST #'EQL)
T

(TREE-EQUAL '(A (B C)) '((A B) C) :TEST #'EQL)
NIL

(TREE-EQUAL 5 (+ 2 3) :TEST #'EQL)
T

(TREE-EQUAL '(A (B)) '(A (B)) :TEST #'EQL)
T

(TREE-EQUAL '(A (B . 1.0)) '(A (B #C(1.0 0.0))) :TEST #'EQL)
NIL

(TREE-EQUAL 1 1 :TEST #'EQUAL)
T

(TREE-EQUAL 'WORD 'WORD :TEST #'EQUAL)
T

(TREE-EQUAL 'WORD1 'WORD2 :TEST #'EQUAL)
NIL

(TREE-EQUAL '(A B) '(A B) :TEST #'EQUAL)
T

(TREE-EQUAL '(A (B C)) '((A B) C) :TEST #'EQUAL)
NIL

(TREE-EQUAL 5 (+ 2 3) :TEST #'EQUAL)
T

(TREE-EQUAL '(A (B)) '(A (B)) :TEST #'EQUAL)
T

(TREE-EQUAL '(A (B . 1.0)) '(A (B #C(1.0 0.0))) :TEST #'EQUAL)
NIL

(TREE-EQUAL 1 1 :TEST-NOT #'EQ)
NIL

(TREE-EQUAL 'WORD 'WORD :TEST-NOT #'EQ)
NIL

(TREE-EQUAL 'WORD1 'WORD2 :TEST-NOT #'EQ)
T

(TREE-EQUAL '(A B) '(A B) :TEST-NOT #'EQ)
NIL

(TREE-EQUAL '(A (B C)) '((A B) C) :TEST-NOT #'EQ)
NIL

(TREE-EQUAL 5 (+ 2 3) :TEST-NOT #'EQ)
NIL

(TREE-EQUAL '(A (B)) '(A (B)) :TEST-NOT #'EQ)
NIL

(TREE-EQUAL '(A (B . 1.0)) '(A (B #C(1.0 0.0))) :TEST-NOT #'EQ)
NIL

