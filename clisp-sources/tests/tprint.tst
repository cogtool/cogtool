;******************************************************************************
;*      ROSENMUELLER    TPRINT.QUE                                            *
;******************************************************************************

;;** displace zu displac0 umbenannt, weil paket gedruckt wird 22.08.1990 
**;;

(write-to-string 
        '(let ((a a1)
               (b b1))
              1 2
              (3 4)
              (5 6)
              7 8) :pretty t)
"(LET ((A A1) 
      (B B1)) 
     1 2 
     (3 4) 
     (5 6) 
     7 8)"

(write-to-string 
        '(prog (1 2)
               (3 4)
          a    (5 6)
               (8 9)
          x    (zzz)) :pretty t)
"(PROG (1 2)
      (3 4)
 A    (5 6)
      (8 9)
 X    (ZZZ))"

(write-to-string 
        '(do ((l '(1 2 3)
                 (cdr l)))
             ((null l) 
              (print 'a1)
              (print 'a2))
             (print l)) :pretty t)
"(DO ((L '(1 2 3) 
        (CDR L))) 
    ((NULL L) 
     (PRINT 'A1) 
     (PRINT 'A2)) 
    (PRINT L))"

(setq *print-level* nil *print-length* nil *print-pretty* t)
T

(WRITE-TO-STRING
'(123 
"das ist der 1. string laenger als 80 zeichen aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
456
"das ist der 2. bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
789)
)
"(123 \"das ist der 1. string laenger als 80 zeichen aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\" 

     456 
     \"das ist der 2. bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\" 

     789)"

(setq tarray
        #4a((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))
             ((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))
             ((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 58 59 60)))
            (((61 62 63 64 65)(66 67 68 69 70)(11 12 13 14 15)(16 17 18 19 20))
             ((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))
             ((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 58 59 60)))
           ))
#4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 
25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 
49 50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(11 
12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 35)(
36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 58 59 
60))))

(setq tal
        '((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))
             ((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))
             ((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 58 59 60)))
            (((61 62 63 64 65)(66 67 68 69 70)(11 12 13 14 15)(16 17 18 19 20))
             ((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))
             ((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 58 59 60)))
           ))
((((1 2 3 4 5) 
   (6 7 8 9 10) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))) 
 (((61 62 63 64 65) 
   (66 67 68 69 70) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))))

(setq tlist (list 'a 'b 'c tarray))
(A B C 
   #4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 
   25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 
   48 49 50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(
   11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 
   34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 
   57 58 59 60)))))

(setq *print-length* 6)
6

tarray
#4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(
26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 
50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(11 12 
13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 35)(36 
37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 58 59 60))))

(WRITE-TO-STRING tal)
"((((1 2 3 4 5) 
   (6 7 8 9 10) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))) 
 (((61 62 63 64 65) 
   (66 67 68 69 70) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))))"

(setq *print-length* 5)
5

tarray
#4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(
26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 
50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(11 12 
13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 35)(36 
37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 58 59 60))))

(WRITE-TO-STRING tal)
"((((1 2 3 4 5) 
   (6 7 8 9 10) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))) 
 (((61 62 63 64 65) 
   (66 67 68 69 70) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))))"

(setq *print-length* 4)
4

(WRITE-TO-STRING tarray)
"#4A((((1 2 3 4 ...)(6 7 8 9 ...)(11 12 13 14 ...)(16 17 18 19 ...))((21 22 23 24 ...)(
26 27 28 29 ...)(31 32 33 34 ...)(36 37 38 39 ...))((41 42 43 44 ...)(46 47 48 
49 ...)(51 52 53 54 ...)(56 57 58 59 ...)))(((61 62 63 64 ...)(66 67 68 69 ...)(
11 12 13 14 ...)(16 17 18 19 ...))((21 22 23 24 ...)(26 27 28 29 ...)(31 32 33 
34 ...)(36 37 38 39 ...))((41 42 43 44 ...)(46 47 48 49 ...)(51 52 53 54 ...)(56 
57 58 59 ...))))"
;;"#4A((((1 2 3 4 ...)(6 7 8 9 ...)(11 12 13 14 ...)(16 17 18 19 ...))((21 22 23 
;;24 ...)(26 27 28 29 ...)(31 32 33 34 ...)(36 37 38 39 ...))((41 42 43 44 ...)(
;;46 47 48 49 ...)(51 52 53 54 ...)(56 57 58 59 ...)))(((61 62 63 64 ...)(66 67 
;;68 69 ...)(11 12 13 14 ...)(16 17 18 19 ...))((21 22 23 24 ...)(26 27 28 29 ...)(
;;31 32 33 34 ...)(36 37 38 39 ...))((41 42 43 44 ...)(46 47 48 49 ...)(51 52 53 
;;54 ...)(56 57 58 59 ...))))"

(WRITE-TO-STRING tal)
"((((1 2 3 4 ...) 
   (6 7 8 9 ...) 
   (11 12 13 14 ...) 
   (16 17 18 19 ...)) 
  ((21 22 23 24 ...) 
   (26 27 28 29 ...) 
   (31 32 33 34 ...) 
   (36 37 38 39 ...)) 
  ((41 42 43 44 ...) 
   (46 47 48 49 ...) 
   (51 52 53 54 ...) 
   (56 57 58 59 ...))) 
 (((61 62 63 64 ...) 
   (66 67 68 69 ...) 
   (11 12 13 14 ...) 
   (16 17 18 19 ...)) 
  ((21 22 23 24 ...) 
   (26 27 28 29 ...) 
   (31 32 33 34 ...) 
   (36 37 38 39 ...)) 
  ((41 42 43 44 ...) 
   (46 47 48 49 ...) 
   (51 52 53 54 ...) 
   (56 57 58 59 ...))))"

(setq *print-length* 3)
3

(WRITE-TO-STRING tarray)
"#4A((((1 2 3 ...)(6 7 8 ...)(11 12 13 ...)...)((21 22 23 ...)(26 27 28 ...)(31 
32 33 ...)...)((41 42 43 ...)(46 47 48 ...)(51 52 53 ...)...))(((61 62 63 ...)(
66 67 68 ...)(11 12 13 ...)...)((21 22 23 ...)(26 27 28 ...)(31 32 33 ...)...)((
41 42 43 ...)(46 47 48 ...)(51 52 53 ...)...)))"

(WRITE-TO-STRING tal)
"((((1 2 3 ...) 
   (6 7 8 ...) 
   (11 12 13 ...) ...) 
  ((21 22 23 ...) 
   (26 27 28 ...) 
   (31 32 33 ...) ...) 
  ((41 42 43 ...) 
   (46 47 48 ...) 
   (51 52 53 ...) ...)) 
 (((61 62 63 ...) 
   (66 67 68 ...) 
   (11 12 13 ...) ...) 
  ((21 22 23 ...) 
   (26 27 28 ...) 
   (31 32 33 ...) ...) 
  ((41 42 43 ...) 
   (46 47 48 ...) 
   (51 52 53 ...) ...)))"

(setq *print-length* 2)
2

(WRITE-TO-STRING tarray)
"#4A((((1 2 ...)(6 7 ...)...)((21 22 ...)(26 27 ...)...)...)(((61 62 ...)(66 67 ...)...)((
21 22 ...)(26 27 ...)...)...))"

(WRITE-TO-STRING tal)
"((((1 2 ...) 
   (6 7 ...) ...) 
  ((21 22 ...) 
   (26 27 ...) ...) ...) 
 (((61 62 ...) 
   (66 67 ...) ...) 
  ((21 22 ...) 
   (26 27 ...) ...) ...))"

(setq *print-length* 1)
1

(WRITE-TO-STRING tarray)
"#4A((((1 ...)...)...)...)"

(WRITE-TO-STRING tal)
"((((1 ...) 
   ...) 
  ...) 
 ...)"
;;"((((1 ...) ...) ...) ...)"

(setq *print-length* 0)
0

(WRITE-TO-STRING tarray)
"#4A(...)"

(WRITE-TO-STRING tal)
"(...)"

;***1

(setq *print-length* 6)
6

tarray   
#4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(
26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 
50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(11 12 
13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 35)(36 
37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 58 59 60))))

(WRITE-TO-STRING tal)
"((((1 2 3 4 5) 
   (6 7 8 9 10) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))) 
 (((61 62 63 64 65) 
   (66 67 68 69 70) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))))"

(setq *print-level* 5)
5

tarray
#4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(
26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 
50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(11 12 
13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 35)(36 
37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 58 59 60))))

(WRITE-TO-STRING tal)
"((((1 2 3 4 5) 
   (6 7 8 9 10) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))) 
 (((61 62 63 64 65) 
   (66 67 68 69 70) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))))"

(setq *print-level* 4)
4

tarray
#4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(
26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 
50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(11 12 
13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 35)(36 
37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 58 59 60))))

(WRITE-TO-STRING tal)
"((((1 2 3 4 5) 
   (6 7 8 9 10) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))) 
 (((61 62 63 64 65) 
   (66 67 68 69 70) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))))"

(setq *print-level* 3)
3

(WRITE-TO-STRING tarray)
"#4A(((# # # #)(# # # #)(# # # #))((# # # #)(# # # #)(# # # #)))"

(WRITE-TO-STRING tal)
"(((# 
   # 
   # 
   #) 
  (# 
   # 
   # 
   #) 
  (# 
   # 
   # 
   #)) 
 ((# 
   # 
   # 
   #) 
  (# 
   # 
   # 
   #) 
  (# 
   # 
   # 
   #)))"

(setq *print-level* 2)
2

(WRITE-TO-STRING tarray)
"#4A((# # #)(# # #))"

(WRITE-TO-STRING tal)
"((# 
  # 
  #) 
 (# 
  # 
  #))"

(setq *print-level* 1)
1

(WRITE-TO-STRING tarray)
"#4A(# #)"

(WRITE-TO-STRING tal)
"(# 
 #)"

(setq *print-level* 0)
0

(WRITE-TO-STRING tarray)
"#4A#"

(WRITE-TO-STRING tal)
"#"

(setq *print-level* 6)
6

tarray
#4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(
26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 
50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(11 12 
13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 35)(36 
37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 58 59 60))))

(WRITE-TO-STRING tal)
"((((1 2 3 4 5) 
   (6 7 8 9 10) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))) 
 (((61 62 63 64 65) 
   (66 67 68 69 70) 
   (11 12 13 14 15) 
   (16 17 18 19 20)) 
  ((21 22 23 24 25) 
   (26 27 28 29 30) 
   (31 32 33 34 35) 
   (36 37 38 39 40)) 
  ((41 42 43 44 45) 
   (46 47 48 49 50) 
   (51 52 53 54 55) 
   (56 57 58 59 60))))"

(setq *print-length* 6)
6

(WRITE-TO-STRING tlist)
"(A B C 
   #4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 
   25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 
   48 49 50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(
   11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 
   35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 
   58 59 60)))))"
;;"(A B C 
;;   #4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 
;;   25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 
;;   48 49 50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(
;;   11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 
;;   34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 
;;   57 58 59 60)))))"

(setq *print-length* 5)
5

(WRITE-TO-STRING tlist)
"(A B C 
   #4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 
   25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 
   48 49 50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(
   11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 
   35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 
   58 59 60)))))"
;;"(A B C 
;;   #4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 
;;   25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 
;;   48 49 50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(
;;   11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 
;;   34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 
;;   57 58 59 60)))))"

(setq *print-length* 4)
4

(WRITE-TO-STRING tlist)
"(A B C 
   #4A((((1 2 3 4 ...)(6 7 8 9 ...)(11 12 13 14 ...)(16 17 18 19 ...))((21 22 23 
   24 ...)(26 27 28 29 ...)(31 32 33 34 ...)(36 37 38 39 ...))((41 42 43 44 ...)(
   46 47 48 49 ...)(51 52 53 54 ...)(56 57 58 59 ...)))(((61 62 63 64 ...)(66 67 
   68 69 ...)(11 12 13 14 ...)(16 17 18 19 ...))((21 22 23 24 ...)(26 27 28 29 ...)(
   31 32 33 34 ...)(36 37 38 39 ...))((41 42 43 44 ...)(46 47 48 49 ...)(51 52 
   53 54 ...)(56 57 58 59 ...)))))"
;;"(A B C #4A((((1 2 3 4 ...)(6 7 8 9 ...)(11 12 13 14 ...)(16 17 18 19 ...))((21 
;;   22 23 24 ...)(26 27 28 29 ...)(31 32 33 34 ...)(36 37 38 39 ...))((41 42 43 
;;   44 ...)(46 47 48 49 ...)(51 52 53 54 ...)(56 57 58 59 ...)))(((61 62 63 64 ...)(
;;   66 67 68 69 ...)(11 12 13 14 ...)(16 17 18 19 ...))((21 22 23 24 ...)(26 27 
;;   28 29 ...)(31 32 33 34 ...)(36 37 38 39 ...))((41 42 43 44 ...)(46 47 48 49 ...)(
;;   51 52 53 54 ...)(56 57 58 59 ...)))))"

(setq *print-length* 3)
3

(WRITE-TO-STRING tlist)
"(A B C ...)"


(setq *print-length* 2)
2

(WRITE-TO-STRING tlist)
"(A B ...)"


(setq *print-length* 1)
1

(WRITE-TO-STRING tlist)
"(A ...)"


(setq *print-length* 0)
0

(WRITE-TO-STRING tlist)
"(...)"

(setq *print-length* 6)
6

(WRITE-TO-STRING tlist)
"(A B C 
   #4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 
   25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 
   48 49 50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(
   11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 
   35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 
   58 59 60)))))"
;;"(A B C 
;;   #4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 
;;   25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 
;;   48 49 50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(
;;   11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 
;;   34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 
;;   57 58 59 60)))))"

(setq *print-level* 5)
5

(WRITE-TO-STRING tlist)
"(A B C 
   #4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 
   25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 
   48 49 50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(
   11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 
   35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 
   58 59 60)))))"
;;"(A B C 
;;   #4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 
;;   25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 
;;   48 49 50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(
;;   11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 
;;   34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 
;;   57 58 59 60)))))"

(setq *print-level* 4)
4

(WRITE-TO-STRING tlist)
"(A B C #4A(((# # # #)(# # # #)(# # # #))((# # # #)(# # # #)(# # # 
#))))"

(setq *print-level* 3)
3

(WRITE-TO-STRING tlist)
"(A B C #4A((# # #)(# # #)))"


(setq *print-level* 2)
2

(WRITE-TO-STRING tlist)
"(A B C #4A(# #))"

(setq *print-level* 1)
1

(WRITE-TO-STRING tlist)
"(A B C #4A#)"

(setq *print-level* 0)
0

(WRITE-TO-STRING tlist)
"#"

(setq *print-level* 6)
6

(WRITE-TO-STRING tlist)
"(A B C 
   #4A((((1 2 3 4 5)(6 7 8 9 10)(11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 
   25)(26 27 28 29 30)(31 32 33 34 35)(36 37 38 39 40))((41 42 43 44 45)(46 47 
   48 49 50)(51 52 53 54 55)(56 57 58 59 60)))(((61 62 63 64 65)(66 67 68 69 70)(
   11 12 13 14 15)(16 17 18 19 20))((21 22 23 24 25)(26 27 28 29 30)(31 32 33 34 
   35)(36 37 38 39 40))((41 42 43 44 45)(46 47 48 49 50)(51 52 53 54 55)(56 57 
   58 59 60)))))"

;***2

(setq *print-pretty* t)
t

(setq *print-length* 10.)
10

(setq *print-level* 3.)
3

(setq *print-circle* nil)
NIL

(setq a '(10 11 12))
(10 11 12)

(WRITE-TO-STRING (rplacd (cdr a) a))
"(11 10 11 10 11 10 11 10 11 10 ...)"

(setq b '(10 11 12))
(10 11 12)

(WRITE-TO-STRING (rplaca (cddr b) b))
"((10 11 
     (10 11 
         #)))"

(setq c '(10 11 12 13))
(10 11 12 13)

(WRITE-TO-STRING (rplacd (cddr c) (cdr c)))
"(12 11 12 11 12 11 12 11 12 11 ...)"

(WRITE-TO-STRING (setq d (list 'a 'b a 'c b c 'a )))
"(A B 
   (10 11 10 11 10 11 10 11 10 11 ...) 
   C 
   (10 11 
       (10 11 
           #)) 
   (10 11 12 11 12 11 12 11 12 11 ...) 
   A)"

(setq *print-length* nil)
NIL

(setq *print-level* nil)
NIL

(setq *print-circle* t)
T

(setq a '(10 11 12))
(10 11 12)

(WRITE-TO-STRING (rplacd (cdr a) a))
"#2=(11 10 . #2#)"

(setq b '(10 11 12))
(10 11 12)

(WRITE-TO-STRING (rplaca (cddr b) b))
"(#1=(10 11 
        #1#))"
;;"(#1=(10 11 
;;     #1#))"

(setq c '(10 11 12 13))
(10 11 12 13)

(WRITE-TO-STRING (rplacd (cddr c) (cdr c)))
"#2=(12 11 . #2#)"

(WRITE-TO-STRING (setq d (list 'a 'b a 'c b c 'a )))
"(A B 
   #3=(10 11 . #3#) 
   C 
   #6=(10 11 
          #6#) 
   (10 . #8=( 11 12 . #8#)) 
   A)"
;;"(A B 
;;   #3=(10 11 . #3#) 
;;   C 
;;   #6=(10 11 
;;       #6#) 
;;   (10 . #8=( 11 12 . #8#)) 
;;   A)"

(setq *print-circle* nil)
NIL

;***3

(setq sys::*pprint-max-indentation* 5)
5

;       (a b c )  ==>  (a b c)
(WRITE-TO-STRING        '(a b c))
"(A B C)"

;       (aaaaaaaaaaa.. bbbbbbb.. cccc..) lang 
;               ==> (aaaaa... bbbbb.. 
;                             ccc...)
(WRITE-TO-STRING        '(aaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb 
cc
         bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb dd
         cccccccccccccccccccccccccccccccccccccccccccccc eeeeeee))
"(AAAAAAAAA BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB CC 
       BBBBBBBBBBBBBBBBBBBBBBBBBBBBBB DD 
       CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC EEEEEEE)"

;       ((a)(b)(c)) ==> ((a) (b) (c))
(WRITE-TO-STRING        '((a) (b) (c)))
"((A) 
 (B) 
 (C))"

;       ((aaaa..) (bbbb..) (cccc..)) lang
;               ==> ((aaaa....)
;                    (bbbb...)
;                    (ccc..))
(WRITE-TO-STRING        '((aaaaaaaaaaaaaaaaaaaaaaaaaa)
          (bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb)
          (ccccccccccccccccccccc)
          (dddddddddddddddddddddddddddddddddd)))
"((AAAAAAAAAAAAAAAAAAAAAAAAAA) 
 (BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB) 
 (CCCCCCCCCCCCCCCCCCCCC) 
 (DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD))"

;       (abcde (a)(b)(c)) ==> (abcde (a) (b) (c))
(WRITE-TO-STRING        '(abcde (a)(b)(c)))
"(ABCDE (A) 
       (B) 
       (C))"

;       (abcde (aaaaaa..) (bbb..) ...) lang
;               ==> (abcde (aaaa....)
;                          (bbbb...)
;                          (ccc.......))
(WRITE-TO-STRING        '(abcde (aaaaaaaaaaaaaaaaaaaaaaaaaaaaa) (bbbbbbbbbbbbbbbbbbbbb)
                (cccccccccccccccccccccccccccccccc)
                (dddddddddddddddddddddddddddddddddddddd)))
"(ABCDE (AAAAAAAAAAAAAAAAAAAAAAAAAAAAA) 
       (BBBBBBBBBBBBBBBBBBBBB) 
       (CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC) 
       (DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD))"

;       (form1 form2 ...)   eine Zeile
;               ==> (form1 form2 ...)
(WRITE-TO-STRING        '((a 1 2) (b 1 2) (c 1 2)))
"((A 1 2) 
 (B 1 2) 
 (C 1 2))"

;       (form1 form2 ...) lang
;               ==> (form1
;                    form2 ...)
(WRITE-TO-STRING        '((aaaaaaaaaa 1111111111 2222222222)
         (bbbbbbbbbb 1111111111 2222222222)
         (cccccccccc 1111111111 2222222222)))
"((AAAAAAAAAA 1111111111 2222222222) 
 (BBBBBBBBBB 1111111111 2222222222) 
 (CCCCCCCCCC 1111111111 2222222222))"

;       (atom form1 form2 ....)
;               ==> (atom form1
;                         form2..)
(WRITE-TO-STRING        '(abcde (a 1 2) (b 1 2)))
"(ABCDE (A 1 2) 
       (B 1 2))"

(WRITE-TO-STRING        '(abcde (aaaaaaaaaaaaaaaaa 111111111111 22222222222)
                (bbbbbbbbbbbbbbbbb 111111111111 22222222222)
                (ccccccccccccccccc 111111111111 22222222222)))
"(ABCDE (AAAAAAAAAAAAAAAAA 111111111111 22222222222) 
       (BBBBBBBBBBBBBBBBB 111111111111 22222222222) 
       (CCCCCCCCCCCCCCCCC 111111111111 22222222222))"

;       (prog form1 form2 atom form3 ...)
;               ==> (prog form1
;                         form2
;                    atom form3 ..)
;; 16.8.1990 ab hier alle progs ausgetauscht, da alte am zeilenende zwei space?
(WRITE-TO-STRING        '(prog (a 1 2) (b 1 2) at (c 1 2) (d 1 2)))
"(PROG (A 1 2)
      (B 1 2)
 AT   (C 1 2)
      (D 1 2))"

(WRITE-TO-STRING        '(prog (a 1 2) (b 1 2) atommarke (c 1 2) (d 1 2)))
"(PROG (A 1 2)
      (B 1 2)
 ATOMMARKE
      (C 1 2)
      (D 1 2))"

(WRITE-TO-STRING        '(tagbody (a) (b) at (c) (d)))
"(TAGBODY 
      (A)
      (B)
 AT   (C)
      (D))"
;;"(TAGBODY (A) 
;;       (B) ........)

;       (form1 atom1 atom2 form2..)
;               ==> (form1
;                    atom1 atom2
;                    form2..)
(WRITE-TO-STRING        '((a 1 2) at1 at2 (b 1 2) (c 1 2)))
"((A 1 2) 
 AT1 AT2 
 (B 1 2) 
 (C 1 2))"

(WRITE-TO-STRING        '((aaaaaaaaaaaa 11111111111 22222222222) atom1 atom2
          (bbbbbbbbbbbb 11111111111 222222222222)
          (ccccc 1111 2222 3333)))
"((AAAAAAAAAAAA 11111111111 22222222222) 
 ATOM1 ATOM2 
 (BBBBBBBBBBBB 11111111111 222222222222) 
 (CCCCC 1111 2222 3333))"

;       (atom1 atom2 atom3 .. ) lang
;               ==> (atom1 atom2 ..
;                    atomn..)
(WRITE-TO-STRING        '(aaaaaaaaaaaaaaaa b ccccccccccccccccc dddddd eeeeeeeeeeeeeeee 

          ffffffffffffffffffffffffffffffffffffffff ggg hhhh iiiii
          jjjjjjjjjjjjjjjjjj k l m n ooooooooooooooo))
"(AAAAAAAAAAAAAAAA B CCCCCCCCCCCCCCCCC DDDDDD EEEEEEEEEEEEEEEE 
       FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF GGG HHHH IIIII 
       JJJJJJJJJJJJJJJJJJ K L M N OOOOOOOOOOOOOOO)"

;       Test der Sonderfunktionen
;; ab hier alle gequoteten objekte mit terpri !
(WRITE-TO-STRING        '(a '1 b))
"(A '1 B)"

(WRITE-TO-STRING        '(a `'b c))
"(A `'B 
   C)"

(WRITE-TO-STRING        '(a '''''''''b c))
"(A '''''''''B 
   C)"

(WRITE-TO-STRING        '(a b 'c d))
"(A B 'C D)"

(WRITE-TO-STRING        '(aaaaaaaaaa bbbbbbbbbbbb 'c 
        ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd))
"(AAAAAAAAAA BBBBBBBBBBBB 'C 
       DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD)"

(WRITE-TO-STRING        '(a (b) 'c d))
"(A (B) 
   'C D)"

(WRITE-TO-STRING        '(aaaaaaaaaa (bbbbbbbbbbbb) 'c 
        dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd))
"(AAAAAAAAAA (BBBBBBBBBBBB) 
       'C DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD)"

(WRITE-TO-STRING        '(a #'b c))
"(A #'B 
   C)"
;;"(A #'B C)"

(WRITE-TO-STRING        '(a `b c))
"(A `B 
   C)"
;;"(A `B C)"

(WRITE-TO-STRING        '(a ,b c))
"(A ,B 
   C)"
;;"(A ,B C)"

(WRITE-TO-STRING        '(a ,@b c))
"(A ,@B 
   C)"
;;"(A ,@B C)"

(WRITE-TO-STRING        '(a ,.b c))
"(A ,.B 
   C)"
;;"(A ,.B C)"

(WRITE-TO-STRING        '(displac0 (liste1)(liste2)))
"(displac0 (LISTE1) 
       (LISTE2))"

(WRITE-TO-STRING         
'(lambda (form vare lenv benv)
                        (progn (mapcon '(lambda (x)
                                                (if (typep 'list x)
                                                   (compile-exp x vare 
                                                            lenv benv)
                                                   '((lbl ,(cdr(assoc x 
                                                                lenv ))))))))))
"(LAMBDA (FORM VARE LENV BENV) 
       (PROGN (MAPCON '(LAMBDA (X) 
                              (IF (TYPEP 'LIST X) 
                                  (COMPILE-EXP X VARE LENV BENV) 
                                  '((LBL ,(CDR (ASSOC X LENV)))))))))"

(WRITE-TO-STRING         
'(lambda ()
                        ((c) nil "12345678901234567890123456789012")))
"(LAMBDA NIL 
       ((C) 
        NIL \"12345678901234567890123456789012\"))"

(WRITE-TO-STRING         '(lambda ()
        ((cccccccccccccccccccccccccccccccccccccccccccc) nil 
        "12345678901234567890123456789012")))
"(LAMBDA NIL 
       ((CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC) 
        NIL \"12345678901234567890123456789012\"))"

(WRITE-TO-STRING         '(defun ed-search-all
                        (cl)
                        (cond ((null (consp cl)) nil)
                                ((ed-search cl) t)
                                (t nil))))
"(DEFUN ED-SEARCH-ALL 
       (CL) 
       (COND ((NULL (CONSP CL)) 
              NIL) 
             ((ED-SEARCH CL) 
              T) 
             (T NIL)))"

(WRITE-TO-STRING         '(defun ed-search-all
                        (cl)
                        (cond ((null (consp ccccccccclllllllllll)) nil)
                                ((ed-search ccccccccclllllllllll) t)
                                (t nil))))
"(DEFUN ED-SEARCH-ALL 
       (CL) 
       (COND ((NULL (CONSP CCCCCCCCCLLLLLLLLLLL)) 
              NIL) 
             ((ED-SEARCH CCCCCCCCCLLLLLLLLLLL) 
              T) 
             (T NIL)))"

(WRITE-TO-STRING         '(a b c 
                    (d)
                    'e f g h i ))
"(A B C 
   (D) 
   'E F G H I)"

(WRITE-TO-STRING         '(aaaaaaaaaaa bbbbbbbbbb cccccccccc 
                    (dddddddddd)
                    'eeeeeeeeee fffffffffff gggggggggg hhhhhhhhhh iiiiiiiiii ))
"(AAAAAAAAAAA BBBBBBBBBB CCCCCCCCCC 
       (DDDDDDDDDD) 
       'EEEEEEEEEE FFFFFFFFFFF GGGGGGGGGG HHHHHHHHHH IIIIIIIIII)"

(WRITE-TO-STRING '(DEFMACRO DEFSTRUCT 
       (LET ((TYPE (CAR TYPE-OPTIONS)) (OPTIONS (CDR TYPE-OPTIONS))) 
            (LET ((DEFSTRUCT-TYPE (ANY-KNOWN-TYPE OPTIONS))) 
                 `(PROGN 'COMPILE 
                      (PUT ',TYPE 
                      (MAKE-DEFSTRUCT-DESCRIPTION NAME 
                         (LET ((I (1- (DEFSTRUCT-TYPE-DESCRIPTION-OVERHEAD (
                                             GET ',DEFSTRUCT-TYPE 
                                             'DEFSTRUCT-TYPE-DESCRIPTION)))))))))))))
"(DEFMACRO DEFSTRUCT 
       (LET ((TYPE (CAR TYPE-OPTIONS)) 
             (OPTIONS (CDR TYPE-OPTIONS))) 
            (LET ((DEFSTRUCT-TYPE (ANY-KNOWN-TYPE OPTIONS))) 
                 `(PROGN 'COMPILE 
                         (PUT ',TYPE 
                              (MAKE-DEFSTRUCT-DESCRIPTION NAME 
                                     (LET ((I (1- 
                                           (DEFSTRUCT-TYPE-DESCRIPTION-OVERHEAD 
                                                  (GET ',DEFSTRUCT-TYPE 
                                                    'DEFSTRUCT-TYPE-DESCRIPTION))))))))))))"

(WRITE-TO-STRING '(SETQ (COND ((SETQ (CATCH (MAPCAN '(LAMBDA (SPEC) 

              (CASE (T (COND ((SETQ (COND ((MINUSP SPEC))))))))))))))))
"(SETQ (COND ((SETQ (CATCH (MAPCAN '(LAMBDA (SPEC) 
                                          (CASE (T (COND ((SETQ (COND ((MINUSP 
                                                                           SPEC)))))))))))))))"

(WRITE-TO-STRING '((SETQ (CATCH (MAPCAN '(LAMBDA (SPEC) 
              (CASE (T (COND ((COND (EVENT1 (COND ((AND (EQCAR INPU 
))))))))))))))))
"((SETQ (CATCH (MAPCAN '(LAMBDA (SPEC) 
                              (CASE (T (COND ((COND (EVENT1 (COND ((AND (EQCAR 
                                                                           INPU)))))))))))))))"
(WRITE-TO-STRING '(case a
       (otto (aaaaaaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb)
             (ccccccccccccccccccccc ddddddddddddddddddddddddddddddddddddd))
       (:otto (aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb)
              (ccccccccccccccccccccccccc ddddddddddddddddddddddddd))
       (12 (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbbbb)
           (ccccccccccccccccccccccccccccccccc  ddddddddddddddddddddddddddd))
       (#\n (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbbbbb)
            (ccccccccccccccccccccccccccccccccccccccc ddddddddddddddddddddddd))))
"(CASE A 
      (OTTO (AAAAAAAAAAAAAAAAAAAAAAAAAAAAA BBBBBBBBBBBBBBBBBBBBBB) 
            (CCCCCCCCCCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD)) 
      (:OTTO (AAAAAAAAAAAAAAAAAAAAAA BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB) 
             (CCCCCCCCCCCCCCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDD)) 
      (12 (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA BBBBBBBBBBBBBBBBBBBBBBBBBBB) 
          (CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDD)) 
      (#\\n (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA BBBBBBBBBBBBBBBBBBBBBBBBB) 
           (CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDD)))"

(WRITE-TO-STRING '(DEFMACRO DEFSTRUCT-TYPE-DESCRIPTION-OVERHEAD (A_STRUCTURE) 

(LET ((TYPE (QUOTE DEFSTRUCT-TYPE-DESCRIPTION)) (SLOT-NAME (QUOTE OVERHEAD))) 

(EVAL (DEFSTRUCT-TYPE-DESCRIPTION-REF 
(GET (QUOTE LIST) (QUOTE DEFSTRUCT-TYPE-DESCRIPTION)))))))
"(DEFMACRO DEFSTRUCT-TYPE-DESCRIPTION-OVERHEAD 
       (A_STRUCTURE) 
       (LET ((TYPE 'DEFSTRUCT-TYPE-DESCRIPTION) 
             (SLOT-NAME 'OVERHEAD)) 
            (EVAL (DEFSTRUCT-TYPE-DESCRIPTION-REF (GET 'LIST 
                                                    'DEFSTRUCT-TYPE-DESCRIPTION)))))"

(WRITE-TO-STRING '(defmacro aaa
(let--eval--defstruct-type-description-ref 
(get lista qdefstruct-type-description))))
"(DEFMACRO AAA 
       (LET--EVAL--DEFSTRUCT-TYPE-DESCRIPTION-REF (GET LISTA 
                                                    QDEFSTRUCT-TYPE-DESCRIPTION)))"

(WRITE-TO-STRING 
      '(UN-MAC-EXPAND DM-DF KWOTE MOVD SP VIRGINFN OK EF EXPAND-MCALL 
              EXPANDMACROS IS-displac0 IS-MACRO MAC-EXPAND R-EXPANDMACROS 
              PUTPROPS1 *UNDIS *DIS COPY PUTPROPS *CHE *CL *CLE *TEST *UNBLOCK 
        CH 
              EDITCOMS EDREPLACE PRINTLEVEL NEQ *UNDO ABFRAGE ATOML CONS0 CONT 
        EDFIND1ST 
              EDITF ))
"(UN-MAC-EXPAND DM-DF KWOTE MOVD SP VIRGINFN OK EF EXPAND-MCALL EXPANDMACROS 
       IS-displac0 IS-MACRO MAC-EXPAND R-EXPANDMACROS PUTPROPS1 *UNDIS *DIS 
       COPY PUTPROPS *CHE *CL *CLE *TEST *UNBLOCK CH EDITCOMS EDREPLACE 
       PRINTLEVEL NEQ *UNDO ABFRAGE ATOML CONS0 CONT EDFIND1ST EDITF)"

(WRITE-TO-STRING '(DEFUN MAKEFILE 
       (LET 
            (PRINT (displac0 (LIST 'PRINT (LIST 'QUOTE (LIST 'LAST 'UPDATE 
        (DATE-TIME)))) `(PRINT '(LAST UPDATE ,(DATE-TIME)))) (OR ECHO STREAM)) 
            (PRINT (displac0 (LIST 'PRINT (LIST 'QUOTE (LIST 'VERSION GENV))) 
        `(PRINT '(VERSION ,GENV))) (OR ECHO STREAM)) 
            (WHEN FNSV (PRINT (displac0 (LIST 'SETQ FNS (LIST 'QUOTE FNSV)) 
        `(SETQ ,FNS ',FNSV)) (OR ECHO STREAM))) 
            (WHEN VARSV (PRINT (displac0 (LIST 'SETQ VARS (LIST 'QUOTE VARSV)) 
        `(SETQ ,VARS ',VARSV)) (OR ECHO STREAM))) 
            (PRINT (displac0 (LIST 'SETQ COMS (LIST 'QUOTE COMSV)) `(SETQ 
        ,COMS ',COMSV)) (OR ECHO STREAM)) 
            (PRINT (LIST 'SETQ GEN GENV) (OR ECHO STREAM)) )))
"(DEFUN MAKEFILE 
       (LET (PRINT (displac0 (LIST 'PRINT 
                                   (LIST 'QUOTE 
                                         (LIST 'LAST 'UPDATE 
                                               (DATE-TIME)))) 
                          `(PRINT '(LAST UPDATE 
                                         ,(DATE-TIME)))) 
                   (OR ECHO STREAM)) 
            (PRINT (displac0 (LIST 'PRINT 
                                   (LIST 'QUOTE 
                                         (LIST 'VERSION GENV))) 
                          `(PRINT '(VERSION ,GENV))) 
                   (OR ECHO STREAM)) 
            (WHEN FNSV 
                  (PRINT (displac0 (LIST 'SETQ FNS 
                                         (LIST 'QUOTE FNSV)) 
                                `(SETQ ,FNS 
                                       ',FNSV)) 
                         (OR ECHO STREAM))) 
            (WHEN VARSV 
                  (PRINT (displac0 (LIST 'SETQ VARS 
                                         (LIST 'QUOTE VARSV)) 
                                `(SETQ ,VARS 
                                       ',VARSV)) 
                         (OR ECHO STREAM))) 
            (PRINT (displac0 (LIST 'SETQ COMS 
                                   (LIST 'QUOTE COMSV)) 
                          `(SETQ ,COMS 
                                 ',COMSV)) 
                   (OR ECHO STREAM)) 
            (PRINT (LIST 'SETQ GEN GENV) 
                   (OR ECHO STREAM))))"

(write-to-string  '(DEFUN EVENT-SPEC 
       (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa (
                                                                       (NUMBERP 
                                                                           SPEC) 
                                                                       (SETQ 
                                                                         EVENT1 
                                                                         (COND (
                                                                        (MINUSP 
                                                                           SPEC) 
                                                                      (NTH-BACK 
                                                                        HISTORY 
                                                                        (1- 
                                                                           SPEC))) 
))))))
"(DEFUN EVENT-SPEC 
       (AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA (
                                                                       (NUMBERP 
                                                                           SPEC) 
                                                                         (SETQ 
                                                                         EVENT1 
                                                                         (COND (
                                                                        (MINUSP 
                                                                           SPEC) 
                                                                      (NTH-BACK 
                                                                        HISTORY 
                                                                        (1- 
                                                                           SPEC))))))))"

(WRITE-TO-STRING '(DEFUN EVENT-SPEC 
       ((LAMBDAxSETQxxCONDxxxSETQxxCATCHxxMAPCANxxxxxCASE (TxxCOND 
                                        (COND (EVENT1 
                                            (COND (E (LIST EVENT1)) 
                                                  ((AND (CONSP INPU)        ;!!!
                                                        (EQCAR INPU '&GROUP)) 
                                                   (APPEND (CDR INPU) NIL)) ;!!!
                                                  (T)))))))))
"(DEFUN EVENT-SPEC 
       ((LAMBDAXSETQXXCONDXXXSETQXXCATCHXXMAPCANXXXXXCASE (TXXCOND (COND 
                                                                        (EVENT1 
                                                                          (COND 
                                                                             (E 
                                                                          (LIST 
                                                                         EVENT1)) 
                                                                                (
                                                                           (AND 
                                                                         (CONSP 
                                                                           INPU) 
                                                                         (EQCAR 
                                                                           INPU 
                                                                        '&GROUP)) 
                                                                        (APPEND 
                                                                           (CDR 
                                                                           INPU) 
                                                                            NIL)) 
                                                                             (T))))))))"
(WRITE-TO-STRING 
'(DEFUN EVENT-SPEC 
   ((LAMBDA (EVENT V INPUT EVENT1 I E) 
      (SETQ INPUT 
        (COND ((SETQ INPUT 
          (CATCH 'TAG 
            (MAPCAN '(LAMBDA (SPEC) 
              (CASE SPEC 
                (T (COND ((NUMBERP SPEC) 
                          (SETQ EVENT1 
                                (COND ((MINUSP SPEC) 
                                       (NTH-BACK HISTORY 
                                                 (1- SPEC))))) 
                          (COND (EVENT1 (COND ((AND (EQCAR INPU '&GROUP))))))) 
            (T (PRINTL-SP "illegale specification" SPEC 'IN L)))))) 
L))))))))))
"(DEFUN EVENT-SPEC 
       ((LAMBDA (EVENT V INPUT EVENT1 I E) 
               (SETQ INPUT 
                     (COND ((SETQ INPUT 
                                  (CATCH 'TAG 
                                         (MAPCAN '(LAMBDA (SPEC) 
                                                         (CASE SPEC 
                                                               (T (COND (
                                                                       (NUMBERP 
                                                                           SPEC) 
                                                                         (SETQ 
                                                                         EVENT1 
                                                                         (COND (
                                                                        (MINUSP 
                                                                           SPEC) 
                                                                      (NTH-BACK 
                                                                        HISTORY 
                                                                        (1- 
                                                                           SPEC))))) 
                                                                         (COND 
                                                                        (EVENT1 
                                                                          (COND (
                                                                           (AND 
                                                                         (EQCAR 
                                                                           INPU 
                                                                        '&GROUP))))))) 
                                                                        (T 
                                                                     (PRINTL-SP 
                                                       \"illegale specification\" 
                                                       SPEC 'IN L)))))) 
                                                L)))))))))"

(WRITE-TO-STRING '(DEFUN EVENT-SPEC 
       (HISTORY &OPTIONAL L) 
       ((LAMBDA (EVENT V INPUT EVENT1 I E) 
               (SETQ INPUT 
                     (COND ((NULL L) 
                            (CAR (CDR (CAR (CAR (CDDDR (CAR HISTORY))))))) 
                           ((SETQ INPUT 
                                  (CATCH 'TAG 
                                         (MAPCAN '(LAMBDA (SPEC) 
                                                         (CASE SPEC 
                                                               (V (SETQ V T) 
                                                                  (SETQ E NIL) 
                                                                  NIL) 
                                                               (I (SETQ V NIL) 
                                                                  (SETQ E NIL)) 
                                                               (E (SETQ E T) 
                                                                  (SETQ V NIL) 
                                                                  (SETQ I NIL)) 
                                                               (T (COND (
                                                                       (NUMBERP 
                                                                           SPEC) 
                                                                       (SETQ 
                                                                         EVENT1 
                                                                         (COND (
                                                                        (MINUSP 
                                                                           SPEC) 
                                                                      (NTH-BACK 
                                                                        HISTORY 
                                                                        (1- 
                                                                           SPEC))) 
                                                                               (
                                                                     (ASSQ-HOLD 
                                                                           SPEC 
                                                                        HISTORY)))) 
                                                                       (COND 
                                                                        (EVENT1 
                                                                          (SETQ 
                                                                           INPU 
                                                                           (CAR 
                                                                           (CDR 
                                                                         EVENT1))) 
                                                                          (COND 
                                                                             (V 
                                                                          (LIST 
                                                                           (CAR 
                                                                          (CDDR 
                                                                         EVENT1)))) 
                                                                             (E 
                                                                          (LIST 
                                                                         EVENT1)) 
                                                                             (
                                                                           (AND 
                                                                         (CONSP 
                                                                           INPU) 
                                                                         (EQCAR 
                                                                           INPU 
                                                                        '&GROUP)) 
                                                                        (APPEND 
                                                                           (CDR 
                                                                           INPU) 
                                                                           NIL)) 
                                                                             (T 
                                                                          (LIST 
                                                                           INPU)))))) 
                                                                        (T 
                                                                     (PRINTL-SP 
                                                                            "illegale specification" 
                                                                           SPEC 
                                                                      'IN L) )))))))))))))))
"(DEFUN EVENT-SPEC 
       (HISTORY &OPTIONAL L) 
       ((LAMBDA (EVENT V INPUT EVENT1 I E) 
               (SETQ INPUT 
                     (COND ((NULL L) 
                            (CAR (CDR (CAR (CAR (CDDDR (CAR HISTORY))))))) 
                           ((SETQ INPUT 
                                  (CATCH 'TAG 
                                         (MAPCAN '(LAMBDA (SPEC) 
                                                         (CASE SPEC 
                                                               (V (SETQ V T) 
                                                                  (SETQ E NIL) 
                                                                  NIL) 
                                                               (I (SETQ V NIL) 
                                                                  (SETQ E NIL)) 
                                                               (E (SETQ E T) 
                                                                  (SETQ V NIL) 
                                                                  (SETQ I NIL)) 
                                                               (T (COND (
                                                                       (NUMBERP 
                                                                           SPEC) 
                                                                         (SETQ 
                                                                         EVENT1 
                                                                         (COND (
                                                                        (MINUSP 
                                                                           SPEC) 
                                                                      (NTH-BACK 
                                                                        HISTORY 
                                                                        (1- 
                                                                           SPEC))) 
                                                                               (
                                                                     (ASSQ-HOLD 
                                                                           SPEC 
                                                                        HISTORY)))) 
                                                                         (COND 
                                                                        (EVENT1 
                                                                          (SETQ 
                                                                           INPU 
                                                                           (CAR 
                                                                           (CDR 
                                                                         EVENT1))) 
                                                                          (COND 
                                                                             (V 
                                                                          (LIST 
                                                                           (CAR 
                                                                          (CDDR 
                                                                         EVENT1)))) 
                                                                             (E 
                                                                          (LIST 
                                                                         EVENT1)) 
                                                                             (
                                                                           (AND 
                                                                         (CONSP 
                                                                           INPU) 
                                                                         (EQCAR 
                                                                           INPU 
                                                                        '&GROUP)) 
                                                                        (APPEND 
                                                                           (CDR 
                                                                           INPU) 
                                                                            NIL)) 
                                                                             (T 
                                                                          (LIST 
                                                                           INPU)))))) 
                                                                        (T 
                                                                     (PRINTL-SP 
                                                       \"illegale specification\" 
                                                       SPEC 'IN L)))))))))))))))"

;***4

(WRITE-TO-STRING '(LAMBDA (PP-CALL &OPTIONAL ENV) 
       (LET ((SYMB (CADR PP-CALL))) 
            (IF (SYMBOLP SYMB) 
                (LIST 'PROGN 
                      (IF (BOUNDP SYMB) 
                          (LIST 'PPRINT SYMB)) 
                      (IF (FBOUNDP SYMB) 
                          (LET ((FUN (SYMBOL-FUNCTION SYMB))) 
                               (COND ((EQ (SYSTEM::%GET-TYPE FUN) 
                                          SYSTEM::%TYPE-CLOSURE) 
                                      (LIST 'PPRINT 
                                            (LIST 'QUOTE 
                                                  (LIST* 'DEFUN SYMB 
                                                         (CDR (SYSTEM::%P-GET-CONTENT 
                                                               (SYSTEM::%INDEX-LOCATION 
                                                                FUN 3))))))) 
                                     ((EQ (SYSTEM::%GET-TYPE FUN) 
                                          SYSTEM::%TYPE-CONS) 
                                      (LIST 'PPRINT 
                                            (LIST 'QUOTE 
                                                  (LIST* 'DEFUN SYMB 
                                                         (CDR FUN))))) 
                                     ((EQ (SYSTEM::%GET-TYPE FUN) 
                                          SYSTEM::%TYPE-MACRO-FUNCTION) 
                                      (LET ((FUNLIS (SYSTEM::%SET-TYPE-POINTER 
                                                           SYSTEM::%TYPE-CONS 
                                                           FUN))) 
                                           (LIST 'PPRINT 
                                                 (LIST 'QUOTE 
                                                       (COND ((NULL (CDR FUNLIS)) 
                                                              (CAR FUNLIS)) 
                                                             (T (CADR FUNLIS)))))))))) 
                      "ok")))))
"(LAMBDA (PP-CALL &OPTIONAL ENV) 
       (LET ((SYMB (CADR PP-CALL))) 
            (IF (SYMBOLP SYMB) 
                (LIST 'PROGN 
                      (IF (BOUNDP SYMB) 
                          (LIST 'PPRINT SYMB)) 
                      (IF (FBOUNDP SYMB) 
                          (LET ((FUN (SYMBOL-FUNCTION SYMB))) 
                               (COND ((EQ (SYSTEM::%GET-TYPE FUN) 
                                          SYSTEM::%TYPE-CLOSURE) 
                                      (LIST 'PPRINT 
                                            (LIST 'QUOTE 
                                                  (LIST* 'DEFUN SYMB 
                                                         (CDR 
                                                        (SYSTEM::%P-GET-CONTENT 
                                                       (SYSTEM::%INDEX-LOCATION 
                                                              FUN 3))))))) 
                                     ((EQ (SYSTEM::%GET-TYPE FUN) 
                                          SYSTEM::%TYPE-CONS) 
                                      (LIST 'PPRINT 
                                            (LIST 'QUOTE 
                                                  (LIST* 'DEFUN SYMB 
                                                         (CDR FUN))))) 
                                     ((EQ (SYSTEM::%GET-TYPE FUN) 
                                          SYSTEM::%TYPE-MACRO-FUNCTION) 
                                      (LET ((FUNLIS (SYSTEM::%SET-TYPE-POINTER 
                                                           SYSTEM::%TYPE-CONS 
                                                           FUN))) 
                                           (LIST 'PPRINT 
                                                 (LIST 'QUOTE 
                                                       (COND ((NULL (CDR FUNLIS)) 
                                                              (CAR FUNLIS)) 
                                                             (T (CADR FUNLIS)))))))))) 
                      \"ok\"))))"

(WRITE-TO-STRING '
(LAMBDA (PP-CALL &OPTIONAL ENV) 
       (LET ((SYMB (CADR PP-CALL))) 
            (IF (SYMBOLP SYMB) 
                (LIST 'PROGN 
                      (IF (BOUNDP SYMB) 
                          (LIST 'PPRINT SYMB)) 
                      (IF (FBOUNDP SYMB) 
                          (LET ((FUN (SYMBOL-FUNCTION SYMB))) 
                               (COND ((EQ (SYSTEM::%GET-TYPE FUN) 
                                          SYSTEM::%TYPE-CLOSURE) 
                                      (LIST 'PPRINT 
                                            (LIST 'QUOTE 
                                                  (LIST* 'DEFUN SYMB 
                                                         (CDR (SYSTEM::%P-GET-CONTENT 
                                                               (SYSTEM::%INDEX-LOCATION 
                                                                FUN 3))))))) ))))))))
"(LAMBDA (PP-CALL &OPTIONAL ENV) 
       (LET ((SYMB (CADR PP-CALL))) 
            (IF (SYMBOLP SYMB) 
                (LIST 'PROGN 
                      (IF (BOUNDP SYMB) 
                          (LIST 'PPRINT SYMB)) 
                      (IF (FBOUNDP SYMB) 
                          (LET ((FUN (SYMBOL-FUNCTION SYMB))) 
                               (COND ((EQ (SYSTEM::%GET-TYPE FUN) 
                                          SYSTEM::%TYPE-CLOSURE) 
                                      (LIST 'PPRINT 
                                            (LIST 'QUOTE 
                                                  (LIST* 'DEFUN SYMB 
                                                         (CDR 
                                                        (SYSTEM::%P-GET-CONTENT 
                                                       (SYSTEM::%INDEX-LOCATION 
                                                              FUN 3))))))))))))))"
(WRITE-TO-STRING '
(LAMBDA (PP-CALL &OPTIONAL ENV) 
       (LET (IF (LIST (IF (LET (COND ((LIST (LIST (LIST* (CDR (SYSTEM::%P-GET-CONTENT 
                                                               (SYSTEM::%INDEX-LOCATION 
                                                                FUN 3))))))) ))))))))
"(LAMBDA (PP-CALL &OPTIONAL ENV) 
       (LET (IF (LIST (IF (LET (COND ((LIST (LIST (LIST* (CDR 
                                                        (SYSTEM::%P-GET-CONTENT 
                                                       (SYSTEM::%INDEX-LOCATION 
                                                              FUN 3))))))))))))))"
(WRITE-TO-STRING '
(LAMBDA (PP-CALL &OPTIONAL ENV) 
       (letttttttttttttttttttttttttttttttttttttttttttttt (CDR (SYSTEM::%P-GET-CONTENT 
                                                               (SYSTEM::%INDEX-LOCATION 
                                                                FUN 3))))))
"(LAMBDA (PP-CALL &OPTIONAL ENV) 
       (LETTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT (CDR 
                                                        (SYSTEM::%P-GET-CONTENT 
                                                       (SYSTEM::%INDEX-LOCATION 
                                                              FUN 3)))))"

(WRITE-TO-STRING '
(lambdaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa (CDR (SYSTEM::%P-GET-CONTENT 
                                                               (SYSTEM::%INDEX-LOCATION 
                                                                FUN 3) ))))
"(LAMBDAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA (CDR 
                                                        (SYSTEM::%P-GET-CONTENT 
                                                       (SYSTEM::%INDEX-LOCATION 
                                                              FUN 3))))"

(WRITE-TO-STRING '
(laaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa (SYSTEM::%P-GET-CONTENT 
                                                               (SYSTEM::%INDEX-LOCATION 
                                                                FUN 3) )))
"(LAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA 
       (SYSTEM::%P-GET-CONTENT (SYSTEM::%INDEX-LOCATION FUN 3)))"

(SETQ *PRINT-PRETTY* NIL)
NIL

;***5

(defstruct name a b c d e)
NAME

(progn (setq val '(name 1 2 3 4 5))
       (setq st (sys::%set-type-pointer sys::%type-named-structure val))
        T)
T

(WRITE-TO-STRING st)
"#S(NAME A 1 B 2 C 3 D 4 E 5)"

(defstruct (pcvar (:print-function print-pcvar)) id)
pcvar

(defun print-pcvar (var str dep)
 (format str "?~s" (pcvar-id var)))
print-pcvar

(prin1-to-string (make-pcvar :id 'otto))
"?OTTO"

#|-------------------------------------------------------------------------

-------------------------------------------------------------------------|#

