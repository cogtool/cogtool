;; -*- Lisp -*-

(SUBST 'A 'B
       '(U B (B) C))
(U A (A) C)

(SUBST 'A 'B
       '(U B (B) C)
       :TEST-NOT
       #'(LAMBDA (X Y)
           (IF (ATOM Y)
               (EQL X Y)
               T)))
(A B (B . A) A . A)

(SUBST 'A 'B
       '(U B (B) C)
       :TEST
       #'(LAMBDA (X Y)
           (NOT (EQL X Y))))
A

(SUBST 'A 'B
       '(U B (B) C)
       :TEST-NOT
       #'(LAMBDA (X Y)
           (NOT (EQL X Y))))
(U A (A) C)

(SUBST 'A 'B
       '(U B (B) C)
       :TEST-NOT
       #'(LAMBDA (X Y)
           (NOT (EQL X Y)))
       :KEY
       #'(LAMBDA (U)
           (IF (LISTP U)
               (CAR U))))
(U . A)

(SUBST-IF 'NUMMMER 'NUMBERP
          '((A (7 (V 6)))))
((A (NUMMMER (V NUMMMER))))

(SUBST-IF-NOT 'NUMMMER 'NUMBERP
              '((A (7 (V 6)))))
NUMMMER

(SUBST-IF-NOT 'NUMMMER
              #'(LAMBDA (X)
                  (AND (LISTP X)
                       (NUMBERP X)))
              '((A (7 (V 6)))))
NUMMMER

(SUBST-IF-NOT 'NUMMMER
              #'(LAMBDA (X)
                  (OR (LISTP X)
                    (NUMBERP X)))
              '((A (7 (V 6)))))
((NUMMMER (7 (NUMMMER 6))))

(NSUBST 'A 'B
        '(U B (B) C)
        :TEST-NOT
        #'(LAMBDA (X Y)
            (IF (ATOM Y)
                (EQL X Y)
                T)))
(A B (B . A) A . A)

(NSUBST 'A 'B
        '(U B (B) C)
        :TEST-NOT
        #'(LAMBDA (X Y)
            (NOT (EQL X Y))))
(U A (A) C)

(NSUBST 'A 'B
        '(U B (B) C)
        :TEST
        #'(LAMBDA (X Y)
            (NOT (EQL X Y))))
A

(NSUBST-IF 'OO 'NUMBERP
           '(A B C (3 (4) 0)))
(A B C (OO (OO) OO))

(NSUBST-IF-NOT 'OO 'NUMBERP
               '(A B C (3 (4) 0)))
OO

(NSUBST-IF-NOT 'OO
               #'(LAMBDA (X)
                   (OR (ATOM X)
                       (NUMBERP X)))
               '(A B C (3 (4) 0)))
OO

(NSUBST-IF-NOT 'OO
               #'(LAMBDA (X)
                   (AND (ATOM X)
                        (NUMBERP X)))
               '(A B C (3 (4) 0)))
OO

(NSUBST-IF-NOT 'OO
       #'(LAMBDA (X)
           (OR (LIST X)
               (NUMBERP X)))
       '(A B C (3 (4) 0)))
(A B C (3 (4) 0))

(NSUBST-IF-NOT 'OO
               #'(LAMBDA (X)
                   (OR (LIST X)
                       (SYMBOLP X)))
               '(A B C (3 (4) 0)))
(A B C (3 (4) 0))

(SUBLIS '((A . A1) (B . B1))
        '(A B))
(A1 B1)

(SUBLIS '((A . A1)
          (B . B1))
        '(A B
          (B . C)))
(A1 B1
    (B1 . C))

(SUBLIS '((A . A1) (B . B1) (NIL . NIL1))
       '(A B (B . C)))
(A1 B1 (B1 . C) . NIL1)

(SUBLIS '((A . A1) (B . B1) (NIL . NIL1))
        '(A B (B C)))
(A1 B1 (B1 C . NIL1) . NIL1)

(SUBLIS '((A . A1) (B . B1) (NIL . NIL1))
        '(A B (B C))
        :TEST-NOT 'EQL)
A1

(SUBLIS '((A . A1) (B . B1) (NIL . NIL1))
        '(A B (B C))
        :TEST-NOT
        #'(LAMBDA (X Y)
            (IF (ATOM Y)
                (EQL X Y))))
A1

(SUBLIS '(((A) . UU) (A . II))
        '(I (A) A))
(I (II) II)

(SUBLIS '(((A) . UU) (A . II))
        '(I (A) A)
        :KEY #'(LAMBDA (X) (IF (LISTP X) (CAR X))))
(I II . II) ; KEY wird angewandt auf: X ein Blatt des Baumes

(SUBLIS '((1 . 2) (2 . 4) (3 . 6) (a . aa) (b . bb) (c . cc) (d . dd))
        '((a b (c (d 1) 2 (3)))))
((aa bb (cc (dd 2) 4 (6))))

(SUBLIS '((1 . 2) (2 . 4) (3 . 6) (a . aa) (b . bb) (c . cc) (d . dd))
        '((a b (c (d 1) 2 (3))))
        :test #'(lambda (x y) (and (numberp x) (numberp y) (= x y))))
((a b (c (d 2) 4 (6))))

(SUBLIS '((1 . 2) (2 . 4) (3 . 6) (a . aa) (b . bb) (c . cc) (d . dd))
        '((a b (c (d 1) 2 (3))))
        :test #'equalp :key #'(lambda (x) (and (symbolp x) x)))
((aa bb (cc (dd 1) 2 (3))))

(SUBLIS '(((A) . UU) (A . II))
        '(I (A) A)
        :TEST #'(LAMBDA (X Y) (IF (LISTP Y) (EQL X (CAR Y)))))
#+(or XCL AKCL ECL LUCID ALLEGRO)       (I II . II) ; X aus der Aliste, Y ein Blatt des Baumes
#+(or CLISP CMU SBCL OpenMCL LISPWORKS) (I (UU) UU) ; X ein Blatt, Y aus der Aliste
#-(or XCL CLISP AKCL ECL CMU SBCL LUCID ALLEGRO OpenMCL LISPWORKS) UNKNOWN

(NSUBLIS '(((A) . UU) (A . II))
         '(I (A) A)
         :KEY #'(LAMBDA (X) (IF (LISTP X) (CAR X))))
(I II . II) ; KEY wird angewandt auf: X ein Blatt des Baumes

(NSUBLIS '(((A) . UU) (A . II))
         '(I (A) A)
         :TEST #'(LAMBDA (X Y) (IF (LISTP X) (EQUAL X Y))))
(I UU . UU)

(NSUBLIS '(((A) . UU) (A . II))
         '(I (A) A)
         :TEST #'(LAMBDA (X Y) (IF (LISTP Y) (EQUAL X Y))))
(I UU . UU)

(NSUBLIS '(((A) . UU) (A . II))
         '(I (A) A)
         :TEST #'(LAMBDA (X Y) (IF (LISTP Y) (EQL X (CAR Y)))))
#+(or XCL AKCL ECL ALLEGRO)                   (I II . II) ; X aus der Aliste, Y ein Blatt des Baumes
#+(or CLISP CMU SBCL LUCID OpenMCL LISPWORKS) (I (UU) UU) ; X ein Blatt, Y aus der Aliste
#-(or XCL CLISP AKCL ECL CMU SBCL LUCID ALLEGRO OpenMCL LISPWORKS) UNKNOWN

;; <http://www.lisp.org/HyperSpec/Body/fun_subliscm_nsublis.html>
(sublis '((x . 100) (z . zprime))
        '(plus x (minus g z x p) 4 . x))
(PLUS 100 (MINUS G ZPRIME 100 P) 4 . 100)

(sublis '(((+ x y) . (- x y)) ((- x y) . (+ x y)))
        '(* (/ (+ x y) (+ x p)) (- x y))
        :test #'equal)
(* (/ (- X Y) (+ X P)) (+ X Y))

(setq tree1 '(1 (1 2) ((1 2 3)) (((1 2 3 4)))))
(1 (1 2) ((1 2 3)) (((1 2 3 4))))

(sublis '((3 . "three")) tree1)
(1 (1 2) ((1 2 "three")) (((1 2 "three" 4))))

(sublis '((t . "string"))
        (sublis '((1 . "") (4 . 44)) tree1)
        :key #'stringp)
("string" ("string" 2) (("string" 2 3)) ((("string" 2 3 44))))

tree1
(1 (1 2) ((1 2 3)) (((1 2 3 4))))

(setq tree2 '("one" ("one" "two") (("one" "Two" "three"))))
("one" ("one" "two") (("one" "Two" "three")))

(sublis '(("two" . 2)) tree2)
("one" ("one" "two") (("one" "Two" "three")))

tree2
("one" ("one" "two") (("one" "Two" "three")))

(sublis '(("two" . 2)) tree2 :test 'equal)
("one" ("one" 2) (("one" "Two" "three")))

(nsublis '((t . 'temp))
         tree1
         :key #'(lambda (x) (or (atom x) (< (list-length x) 3))))
((QUOTE TEMP) (QUOTE TEMP) QUOTE TEMP)

(nthcdr (1+ most-positive-fixnum) '(1 2 3))
NIL
