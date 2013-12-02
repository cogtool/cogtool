 ;       ***********************************************************
 ;       *  ROSENMUELLER  143   Tel.340   TREAD.dat   04.11.1987   *
 ;       ***********************************************************
;&----- Tread0.tst -------------------------------------------------------------

 ;      Testdaten aus den Beispielen der Sprachbeschreibung
 ;
 ;      Eingabe der Zahl 27 und der Liste (a b)         Seite 334


     27
   27
     27.
   27
     #o33 
   27
       #x1b 
   27
       #b11011
   27
        #.(* 3 3 3)
   27
        81/3
   27
        '(A B)
   (A B)
        '(a b)
   (A B)
        '( a b )
   (A B)
        '(\A |B|)
   (A B)

           '(|\A|
           B
        )
   (A B)


 ;      Test auf Gleichheit                             Seite 338
        '|foo||bar| 
   |foobar|
       '|foobar|

   |foobar|
 ;      Test auf Gleichheit
        '|foo|bar|baz| 
   |foo|BAR|baz|
       '|fooBARbaz|

   |foo|BAR|baz|
 ;      Test auf Ungleichheit


(progn (unless (find-package '|foo| )(make-package '|foo|)) t)
   T

        '|foo:bar| 
   |foo:bar|
      
        (read-from-string "|foo|:|bar|")

   ERROR



 ;      reserved token : potential numbers              Seite 341
 ;              eingelesen als Symbol und 
 ;              als solches gedruckt (dh. mit '\' als 1.Zeichen)
        '1b5000 
   \1B5000
       '777777q 
   \777777Q
       '1.7J 
   \1.7J
       '-3/4+6.7J

   \-3/4+6.7J
        '12/25/83 
   \12/25/83
     '27^19 
   \27^19
         '3^4/5 
   \3^4/5
      '6//7

   \6//7
        '3.1.2.6 
   \3.1.2.6
      '^-43^ 
   \^-43^
         '3.141_592_653_589_793_238_4

   \3.141_592_653_589_793_238_4
        '-3.7+2.6i-6.17j+19.6k

   \-3.7+2.6I-6.17J+19.6K

 ;      not potential numbers
 ;          sind immer Symbole :
        '/ 
   /
         '/5 
   /5
       '+ 
   +
       '1+ 
   1+
         '1-

   1-
        'foo+ 
   FOO+
      'ab.cd 
   AB.CD
    '_ 
   _
       '^ 
   ^
          '^/-

   ^/-

 ;      die folgenden token sind Symbole , wenn *read-base* == 10 :
        'bad-face 
   BAD-FACE
        '25-sec-83 
   25-SEC-83
       'a/b 
   A/B
       'fad_cafe

   FAD_CAFE
        'f^

   F^

(progn  (setq *print-base* 16)t)
T

 ;      und sind reserved token, wenn *read-base* == 16
 ;      und werden mit '\' gedruckt, falls *print-base* == 16 :
        'bad-face 
   \BAD-FACE
        '25-sec-83 
   \25-SEC-83
       'a/b 
   \A/B
       'fad_cafe

   \FAD_CAFE
        'f^

   \F^

(progn (setq *print-base* 10)t)
T


 ;      folgende "Zahlen"-Atome muessen mit '\' gedruckt werden :
        '\123 
   \123
       '\123E12 
   \123E12
     '\123S12 
   \123S12
      '\123L12

   \123L12
        '\123D12 
   \123D12
    '\123F12 
   \123F12
     '\123/12

   \123/12


;
 ;        Test der Punktsyntax
        '(a . b)
   (A . B)
        '(a.b)
   (A.B)
                '(a. b)
   (A. B)
                
        '(a \. b c)
   (A \. B C)
        '(a |.| b c)
   (A \. B C)
        '(a \... b c)
   (A \... B C)

        '(a |...| b c)
   (A \... B C)
        '(a b . c)
   (A B . C)
        '.ab

   .AB
        (read-from-string "(. b)")
   ERROR
        (read-from-string "(a .. b)")
   ERROR
        
        (read-from-string "(a . . 'b   )")
   ERROR
        ; Fehler
        (read-from-string "(a b c ...)")
   ERROR

;-------------------------------------------------------------------------------
;(progn (print "etwa ein achtel") t) T
;-------------------------------------------------------------------------------

 ;        test zeichen einlesen
     #\control-meta-a

   #\M-C-A
     #\control-meta-\a
   #\M-C-\a

     #\linefeed

   #\NEWLINE
     #\newline

   #\NEWLINE
     #\space

   #\SPACE
     #\rubout

   #\RUBOUT
     #\page

   #\PAGE
     #\tab

   #\TAB
     #\backspace

   #\BACKSPACE
     #\return

   #\RETURN
     #\control-space

   #\C-SPACE
     #\c-m-return

   #\M-C-RETURN
     #\control-meta-tab

   #\M-C-TAB
     #\h-s-m-c-rubout

   #\H-S-M-C-RUBOUT
     #\control-%

   #\C-%
     #\control-\"
   #\C-\"

     #\meta->

   #\M->

 ;        Test *read-base*
(setq *read-base* 2)
2

(setq *print-radix* t)
T

#+XCL (prin1-to-string 4/3.) #+XCL "4/3."
#-XCL (prin1-to-string #10r4/3) #-XCL "#10r4/3"

(setq *print-base* 2.) 2.

#+XCL (prin1-to-string 4/3.) #+XCL "#2r100/11"
#-XCL (prin1-to-string #10r4/3) #-XCL "#b100/11"

(setq *print-base* 10.) 10.

       '123 
   \123
        123. 
   123.
       312.

   312.

(progn (setq *read-base* 10. ) t)
T

(setq *print-radix* nil)
NIL

 ;        Test floatings extrema 
        .1      
   0.1
+.1     
   0.1
-.1     
   -0.1
.1e1    
   1.0
+.1e1   
   1.0
-.1e1   
   -1.0
1e1     
   10.0
+1e1    
   10.0
-1e1

   -10.0
        1.e1    
   10.0
+1.e1   
   10.0
-1.e1

   -10.0

(setq *read-base* 16)
16

        '1a1.   
   \1A1.
; fehler symbol
        '1r1.   
   \1R1.
; fehler symbol

(progn (setq *read-base* 10. ) t)
T

 ;        Test *print-level*
(setq *print-level* 10)
10

(setq *print-circle* t) 
T

;-- NSUBST ging in Zyklus ! 
(prin1-to-string '(#1=(a b #1#)) ) 
"(#1=(a b #1#))"  

(prin1-to-string '(#1=(#1#)))
"(#1=(#1#))"

(prin1-to-string '(#1=(a b #1#) (c d #1#)))
"(#1=(a b #1#) (c d #1#))"

(setq *print-level* nil)
NIL

 ;        Test *print-length*
(setq *print-length* 11)
11

(prin1-to-string '(1 2 3 4 (5 5) 6 7 8 9 10 11 12 13))
"(1 2 3 4 (5 5) 6 7 8 9 10 11 ...)"

;;;; NSUBST  
(prin1-to-string '#1=(a b . #1#))
#+XCL "#2=(a b . #2#)"
#-XCL "#1=(A B . #1#)"

(setq *print-length* nil)
NIL

 ;        Test packagemarker
        ':abc

   :ABC
 ;        zur Zeit noch nicht implementiert :
(progn (unless (find-package 'abc)(make-package 'abc)) t)
   T

        (read-from-string "abc:de")
   ERROR
        'abc::de
   ABC::DE
        (read-from-string "::111")
   #+XCL ERROR #+CLISP :|111|
        (read-from-string ":abc:")
   ERROR
        (read-from-string ":abc:00")
   ERROR
       ;Fehler
        (read-from-string "ab:cd:2")
   ERROR
        (read-from-string "ab:")
   ERROR
        (read-from-string "ab:cd:")
   ERROR
        (read-from-string "abc::")
   #+XCL ERROR #+CLISP abc::||
 ;

;&----- Tread1.tst -------------------------------------------------------------

 ;        Test array
        '#0a7
   #0A7
        '#1a(1 2 3 4 5 6 7 8)
   #(1 2 3 4 5 6 7 8)

        '#2a((1 2 3)(3 2 1))
   #2A((1 2 3)(3 2 1))

        '#3a(((1 2 3 ) (4 5 6))
             ((1 2 3 ) (4 5 6)))
   #3A(((1 2 3)(4 5 6))((1 2 3)(4 5 6)))

   #+XCL '#a7
   #+XCL #0A7
   #+CLISP '#a7
   #+CLISP #(NIL NIL NIL NIL NIL NIL NIL)

        '#2a((1 2)(3 4)(5 6))
   #2A((1 2)(3 4)(5 6))

        '#3a(((1 2)(3 4)(5 6))
             ((7 8)(9 10)(11 12)))
   #3A(((1 2)(3 4)(5 6))((7 8)(9 10)(11 12)))

 ;
 ;        Test ehemaliger Fehler
        (multiple-value-list (read-from-string "1(123)"))
   (1 1)


        (read-from-string "#0*111") 
   ERROR
                               ; Fehler
        (read-from-string "#0(1 1 1 )")
   ERROR
                        ; Fehler
        (read-from-string "#0r111")
   ERROR
        (read-from-string "#1r111")
   ERROR
               ; Fehler
        #2r111 
   7
               #36r111

   1333
        (read-from-string "#37r111")
   ERROR
                               ; Fehler
        (read-from-string "#011010")
ERROR

        -01     
   -1
-001    
   -1
-01.    
   -1
-0.     
   0
-0/12   
   0
-00.    
   0
-00/12  
   0
;zahlen
        '-12./12        
   \-12./12
'-0./12 
   \-0./12
                                ;symbole

        (list 1 2 #\a)
   (1 2 #\a)


;;        '{1 2 3 { 4 5 6) (7 8 9 }
;;   (1 2 3 (4 5 6) (7 8 9))

;-------------------------------------------------------------------------------
;(progn (print "etwa ein viertel") t) T
;-------------------------------------------------------------------------------

 ;
 ;        Test der Fehlerbehandlung
 ;
 ;        -2        <esc> als 1. zeichen
          (read-from-string "")
   ERROR
 
 ;        -3       <esc> im token
          (read-from-string "abc")
   ERROR


 
 ;        -4       <esc> innerhalb von |.....|
          (read-from-string "a|bc")
   ERROR

 ;        -5         zu langer vektor
          (read-from-string "#2(a b c)")
   ERROR

 ;        -6         packagezeichenfehler
          (read-from-string "::")
   #+XCL ERROR #+CLISP :||
 ;        -7         zu langer bitvektor
          (read-from-string "#2*1010")
   ERROR

;        -9         labelfehler
          (read-from-string "#1#")
   ERROR

 ;        -10        punkttoken
        (READ-FROM-STRING "... ")
   ERROR

;        -11        unzulaessige *read-base*
;                wird auf 10 gesetzt durch Fehlerbehandlung
        (progn (setq *read-base* 40) (read-from-string "123"))
   ERROR

;        Bindung nach Rueckkehr aus Debug
*read-base*
10.

(progn (setq *read-base* 10.) t)
T

*read-base*
10

;        -12        falsche printbase
(progn (setq *print-base* nil) (prin1-to-string *print-base*))
ERROR

(progn  (setq *print-base* 10.)t)
T

*print-base*
10

;        -14        feldfehler
        (read-from-string "#2a((1 2 3)(1 2)")
   ERROR
        (read-from-string "#2a((1 2 3)(1 2))")
   ERROR

;;        (read-from-string "#2a(1 2 3}")
;;   ERROR

;        -16        unzulaessige printlevel
(setq *print-level* t)
T

*print-level*
T

(setq *print-level* nil)
NIL

*print-level*
NIL

;        -17        unzulaessige printlength
(setq *print-length* t)
T

*print-length*
T

(setq *print-length* nil)
NIL

*print-length*
NIL

;&----- Tread2.tst -------------------------------------------------------------

 ;        -22        kein Atom fuer synonym
;;;;; typsystem notwendig        
(make-synonym-stream 123)
ERROR

 ;        -23        kein stream als wert 
        (prin1-to-string (make-synonym-stream '*read-base*))
   #+XCL "#<SYSTEM::%TYPE-SYNONYM-STREAM *READ-BASE*>"
   #+CLISP "#<SYNONYM-STREAM *READ-BASE*>"

 ;        -24        falscher abschluss fuer vektor

        (read-from-string "#(1 2 3 .               )")
   ERROR

 ;      -29     falsch gepunktete liste
        (read-from-string "(a .)")
   ERROR


        (read-from-string "(. a)")
   ERROR


 ;      -30     kein stream als argument
        (make-echo-stream pi *standard-output*)
   ERROR


        (make-two-way-stream pi *standard-output*)
   ERROR

 ;      -31     kein stream als argument
        (make-echo-stream *standard-input* pi)
   ERROR


        (make-two-way-stream *standard-input* pi)
   ERROR


 ;      -33     illegale readtable
(progn (setq *readtable* t) (read-from-string "123"))
ERROR

(progn (setq *readtable* (copy-readtable nil)) t)
T

 ;      -35     illegale readbase in #xxR
        (read-from-string "#37R")
   ERROR

 ;      -36     illegale ziffern in #xxR
        (read-from-string "#10rabc")
   ERROR

        (read-from-string "#10ra/b")
   ERROR
 ;      -37     illegaler #\-string
        (read-from-string "#\nocontrol-")
   ERROR
        (read-from-string "#\control-")
   ERROR
 ;      -38     ????????
;;        }
;;   ERROR

 ;
 ;      test print-radix
 ;
(setq *print-radix* t)
T

(WRITE-TO-STRING 11111 )
   "11111."

(progn  (setq *print-base* 2)t)
T

(WRITE-TO-STRING 11111 )
   #+XCL "#2r10101101100111"
   #-XCL "#b10101101100111"

(progn  (setq *print-base* 8)t)
T

(WRITE-TO-STRING 11111 )
   "#o25547"

(progn  (setq *print-base* 16)t)
T

(WRITE-TO-STRING 11111 )
   "#x2B67"

(progn  (setq *print-base* 22)t)
T

(WRITE-TO-STRING 11111 )
   "#22r10L1"

(progn  (setq *print-base* 10)t)
T

(setq *print-radix* nil)
NIL

(WRITE-TO-STRING 11111 )
   "11111"

(progn  (setq *print-base* 2)t)
T

(WRITE-TO-STRING 11111 )
   "10101101100111"

(progn  (setq *print-base* 8)t)
T

(WRITE-TO-STRING 11111 )
   "25547"

(progn  (setq *print-base* 16)t)
T

(WRITE-TO-STRING 11111 )
   "2B67"

(progn  (setq *print-base* 22)t)
T

(WRITE-TO-STRING 11111 )
   "10L1"

(progn  (setq *print-base* 10)t)
T

 ;
 ;      test read-default-float-format (besser write-to-string ?)
 ;
        0.00999998

   0.00999998
        0.009999998

   0.009999998
        0.0099999997

   0.01
        1.23

   1.23
(prin1-to-string        1.23e20)
   "1.23E20" ; "1.229999E20"

(prin1-to-string        1.23f20)
   "1.23E20" ; "1.229999E20"

(prin1-to-string        1.23s20)
   #+XCL "1.229S20" #+CLISP "1.23s20"

        1.23d20
   1.23D20
        1.23l20
   1.23D20
(prin1-to-string        123456789.123456789)
   "1.2345679E8" ; "1.234567E8"

(prin1-to-string        12345678.12345678)
   "1.2345678E7" ; "1.234567E7"

(prin1-to-string        1234567.1234567)
   "1234567.1" ; "1234567.0"

(prin1-to-string        123456.123456)
   #-CLISP "123456.12" ; "123456.1"
   #+CLISP "123456.125" ; Anzeigefehler

(prin1-to-string        12345.12345)
   "12345.123" ; "12345.12"

(prin1-to-string        1234.1234)
   "1234.1234" ; "1234.123"

        123.123
   123.123
        12.12
   12.12
        1.1
   1.1

(setq *read-default-float-format* 'short-float)
SHORT-FLOAT

        1.23
   1.23
(prin1-to-string        1.23e20)
   "1.23E20" ; "1.229E20"

(prin1-to-string        1.23f20)
   "1.23f20" ; "1.229999F20"

(prin1-to-string        1.23s20)
   "1.23E20" ; "1.229E20"

        1.23d20
   1.23D20
        1.23l20
   1.23D20
(prin1-to-string        123456789.123456789)
   "1.23457E8" ; "1.234E8"

(prin1-to-string        12345678.12345678)
   "1.23457E7" ; "1.234E7"

(prin1-to-string        1234567.1234567)
   "1234560.0"

(prin1-to-string        123456.123456)
   "123456.0"

(prin1-to-string        12345.12345)
   "12345.1" ; "12345.0"

(prin1-to-string        1234.1234)
   #-CLISP "1234.12" ; "1234.0"
   #+CLISP "1234.13" ; Anzeigefehler

(prin1-to-string        123.123)
   "123.123" ; "123.1"

        12.12
   12.12
        1.1
   1.1

(setq *read-default-float-format* 'double-float)
DOUBLE-FLOAT

        1.23
   1.23
        1.23e20
   1.23E20
(prin1-to-string        1.23f20)
   "1.23f20" ; "1.229999F20"

(prin1-to-string        1.23s20)
   "1.23s20" ; "1.229S20"

        1.23d20
   1.23E20
        1.23l20
   1.23E20
(prin1-to-string        123456789.123456789)
   "1.2345678912345679E8" ; "1.234567891234568E8"

        12345678.12345678
   1.234567812345678E7
        1234567.1234567
   1234567.1234567
        123456.123456
   123456.123456
        12345.12345
   12345.12345
        1234.1234
   1234.1234
        123.123
   123.123
        12.12
   12.12
        1.1
   1.1

(setq *read-default-float-format* 'long-float)
LONG-FLOAT

        1.23
   #+XCL 1.23D0 #-XCL 1.23
        1.23e20
   #+XCL 1.23D20 #-XCL 1.23E20
(prin1-to-string        1.23f20)
   "1.23f20" ; "1.229999F20"

(prin1-to-string        1.23s20)
   "1.23s20" ; "1.229S20"

        1.23d20
   1.23D20
        1.23l20
   #+XCL 1.23D20 #-XCL 1.23E20
(prin1-to-string        123456789.123456789)
   "1.23456789123456789E8" ; "1.234567891234568D8"

        12345678.12345678
   #+XCL 1.234567812345678D7 #-XCL 1.234567812345678E7
        1234567.1234567
   #+XCL 1234567.1234567D0 #-XCL 1234567.1234567
        123456.123456
   #+XCL 123456.123456D0 #-XCL 123456.123456
        12345.12345
   #+XCL 12345.12345D0 #-XCL 12345.12345
        1234.1234
   #+XCL 1234.1234D0 #-XCL 1234.1234
        123.123
   #+XCL 123.123D0 #-XCL 123.123
        12.12
   #+XCL 12.12D0 #-XCL 12.12
        1.1
   #+XCL 1.1D0 #-XCL 1.1

(setq *read-default-float-format* 'single-float)
SINGLE-FLOAT

 ;
 ;      test print-case
 ;
(WRITE-TO-STRING 'abcde)
   "ABCDE"

(WRITE-TO-STRING        'abcde-efghij)
   "ABCDE-EFGHIJ"

(WRITE-TO-STRING        'abcde1efghij)
   "ABCDE1EFGHIJ"

(WRITE-TO-STRING        'abcde-1efgh)
   "ABCDE-1EFGH"

(setq *print-case* :downcase)
:downcase

(WRITE-TO-STRING        'abcde)
   "abcde"

(WRITE-TO-STRING        'abcde-efghij)
   "abcde-efghij"

(WRITE-TO-STRING        'abcde1efghij)
   "abcde1efghij"

(WRITE-TO-STRING        'abcde-1efgh)
   "abcde-1efgh"

(setq *print-case* :capitalize)
:Capitalize

(WRITE-TO-STRING        'abcde)
   "Abcde"

(WRITE-TO-STRING        'abcde-efghij)
   "Abcde-Efghij"

(WRITE-TO-STRING        'abcde1efghij)
   "Abcde1efghij"

(WRITE-TO-STRING        'abcde-1efgh)
   "Abcde-1efgh"

(setq *print-case* :upcase)
:UPCASE

 ;
 ;      test print-gensym
 ;
(progn (gensym "test")(gensym 77) t)
   T

(setq *print-gensym* nil)
NIL

(WRITE-TO-STRING        (gensym))
   #+XCL "|test|78" #+CLISP "|test78|"

(setq *print-gensym* t)
T

(WRITE-TO-STRING        (gensym))
   #+XCL "#:|test|79" #+CLISP "#:|test79|"

 ;
 ;      test print-escape-pretty NIL
 ;
(WRITE-TO-STRING        nil)
   "NIL"

(WRITE-TO-STRING        '(a b nil c))
   "(A B NIL C)"

(WRITE-TO-STRING        '(a b (nil) c))
   "(A B (NIL) C)"

(WRITE-TO-STRING        '(a b (nil c) d))
   "(A B (NIL C) D)"

(WRITE-TO-STRING '#(a b nil c (a b nil c) (nil)))
"#(A B NIL C (A B NIL C) (NIL))"

(setq *print-pretty* t)
T

(WRITE-TO-STRING        nil)
   "NIL"

(WRITE-TO-STRING        '(a b nil c))
   "(A B NIL C)"

(WRITE-TO-STRING        '(a b (nil) c))
#+XCL
"(A B 
   (NIL) 
   C)"
#+CLISP "(A B (NIL) C)"

(WRITE-TO-STRING        '(a b (nil c) d))
#+XCL
"(A B 
   (NIL C) 
   D)"
#+CLISP "(A B (NIL C) D)"

(WRITE-TO-STRING '#(a b nil c (a b nil c) (nil)))
"#(A B NIL C (A B NIL C) (NIL))"
;;"#(A B NIL C 
;;(A B NIL C) 
;;(NIL))"

;-------------------------------------------------------------------------------
;(progn (print "etwa mitte") t) T
;-------------------------------------------------------------------------------

(setq *print-pretty* nil)
NIL

 ;
 ;      test read-suppress
 ;
(let ((*read-suppress* t)) (read-from-string "#o33"))
NIL

(let ((*read-suppress* t)) (read-from-string "#x1b"))
NIL

(let ((*read-suppress* t)) (read-from-string "#b11011"))
NIL

(let ((*read-suppress* t)) (read-from-string "#.(* 3 3 3)"))
NIL

(let ((*read-suppress* t)) (read-from-string "(a . b)"))
#+XCL NIL #-XCL (NIL NIL NIL)

(let ((*read-suppress* t)) (read-from-string "(a.b)"))
#+XCL (A.B) #-XCL (NIL)

(let ((*read-suppress* t)) (read-from-string "(a. b)"))
#+XCL (A. B) #-XCL (NIL NIL)
                
(let ((*read-suppress* t)) (read-from-string "(a \. b c)"))
#+XCL (A \. B C) #-XCL (NIL NIL NIL NIL)

(let ((*read-suppress* t)) (read-from-string "(a |.| b c)"))
#+XCL (A \. B C) #-XCL (NIL NIL NIL NIL)

(let ((*read-suppress* t)) (read-from-string "(a \... b c)"))
#+XCL (A \... B C) #-XCL (NIL NIL NIL NIL)

(let ((*read-suppress* t)) (read-from-string "(a |...| b c)"))
#+XCL (A \... B C) #-XCL (NIL NIL NIL NIL)

(let ((*read-suppress* t)) (read-from-string "(a b . c)"))
#+XCL NIL #-XCL (NIL NIL NIL NIL)

(let ((*read-suppress* t)) (read-from-string ".ab"))
#+XCL .AB #-XCL NIL

(let ((*read-suppress* t)) (read-from-string "(. b)"))
#+XCL NIL #-XCL (NIL NIL)

(let ((*read-suppress* t)) (read-from-string "(a .. b)"))
#+XCL (A NIL B) #-XCL (NIL NIL NIL)
        
(let ((*read-suppress* t)) (read-from-string "(a . . 'b   )"))
#+XCL NIL #-XCL (NIL NIL NIL 'NIL)

(let ((*read-suppress* t)) (read-from-string "(a b c ...)"))
#+XCL (A B C NIL) #-XCL (NIL NIL NIL NIL)

(let ((*read-suppress* t)) (read-from-string "#\\control-meta-a"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\control-meta-\\a"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\linefeed"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\newline"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\space"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\rubout"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\page"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\tab"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\backspace"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\return"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\control-space"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\c-m-return"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\control-meta-tab"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\h-s-m-c-rubout"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\control-%"))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\control-\""))
NIL

(let ((*read-suppress* t)) (read-from-string "#\\meta->"))
NIL

(let ((*read-suppress* t)) (read-from-string "xx:xx")) nil

(let ((*read-suppress* t)) (read-from-string "xx::xx")) nil

(setq *print-level* 10)
10

        (let ((*read-suppress* t)) (read-from-string "(#1=(a b #1#))"))
   #+XCL ((A B NIL)) #-XCL ((NIL NIL NIL))
        (let ((*read-suppress* t)) (read-from-string "(#1=(#1#))"))
   ((NIL))

        (let ((*read-suppress* t)) (read-from-string "(#1=(a b #1#) (c d #1#))"))
   #+XCL ((A B NIL) (C D NIL)) #-XCL ((NIL NIL NIL) (NIL NIL NIL))

(setq *print-level* nil)
NIL

(setq *print-length* 11)
11

        (let ((*read-suppress* t)) (read-from-string "#1=(a b . #1#)"))
   #+XCL NIL #-XCL (NIL NIL NIL NIL)

(setq *print-length* nil)
NIL


        (let ((*read-suppress* t)) (read-from-string "#0a7"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#1a(1 2 3 4 5 6 7 8)"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#2a((1 2 3)(3 2 1))"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#3a(((1 2 3 ) (4 5 6))
                                                          ((1 2 3 ) (4 5 6)))"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#a7"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#2a((1 2)(3 4)(5 6))"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#3a(((1 2)(3 4)(5 6))
                                                          ((7 8)(9 10)(11 12)))"))
   NIL

        (let ((*read-suppress* t)) (read-from-string "#0*111"))
   NIL
   
        (let ((*read-suppress* t)) (read-from-string "#0(1 1 1 )"))
   #+XCL ERROR #-XCL NIL

        (let ((*read-suppress* t)) (read-from-string "#0r111"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#1r111"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#2r111"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#36r111"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#37r111"))
   NIL

;        (list 1 2 #\a)
;   (1 2 NIL)

        (let ((*read-suppress* t)) (read-from-string "#2(a b c)"))
   #+XCL ERROR #-XCL NIL
        (let ((*read-suppress* t)) (read-from-string "#2*1010"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#1#"))
   NIL

;;        #2a((1 2 3)(1 2)
;;        #2a(1 2 3}
;;   NIL

        (let ((*read-suppress* t)) (read-from-string "#(1 2 3 .               )"))
   #+XCL ERROR #-XCL NIL

        (let ((*read-suppress* t)) (read-from-string "(a .)"))
   #+XCL NIL #-XCL (NIL NIL)
        (let ((*read-suppress* t)) (read-from-string "(. a)"))
   #+XCL NIL #-XCL (NIL NIL)

        (let ((*read-suppress* t)) (read-from-string "#37R"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#10rabc"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#10ra/b"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#\\nocontrol-"))
   NIL
        (let ((*read-suppress* t)) (read-from-string "#\\control-"))
   NIL


;&----- Tread3.tst -------------------------------------------------------------

;******** Readfehler ***********************************************************
(read-from-string "#\\break")
ERROR

(read-from-string "#\\home-up")
ERROR

(read-from-string "#\\escape")
#+XCL ERROR #+CLISP #\Escape

(#+xx 1 #+xx 2) nil
(#+xx xx:xx)    nil

;;;; typsystem notwendig
(read-from-string "#c(pi 0)")
ERROR

(read-from-string "#c(0 pi)")
ERROR

#|
;******** test fehlerbehandlung ***********************************************
(setq lisptest::*error-message* t)
T


;;;;    test fehlerbehandlung in COMREAD                                   ;;;;
;;;;    --------------------------------                                   ;;;;
;;;     test fehlerbehandlung in Shift                                      ;;;
;;;     ------------------------------                                      ;;;
;;      1.zeichen                                                            ;;
;;      ---------                                                            ;;
;       EOF

(read-from-string "")
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"\"))"
;;"(LT-ERROR LISP::E-READ-TOKEN 4 (\"\"))"

;       constituent/illegal2                            ; nicht moeglich
;sinnlos !      (setq illegal2 (code-char 8))
;   #\BACKSPACE
        ; 127 ?         ; illegal2 wird nicht
;       (set-syntax-from-char #\A illegal2)
;   T
                ; kopiert !
;       'Abc

;   ABC
;       : nach #:
        #::abc

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"#::ABC\"))"
;       unbekannte typ oder illegal1
(setq illegal1 (code-char 7))
#\

        (set-syntax-from-char #\B illegal1)
   T

        B
   "(LT-ERROR LISP::E-READ-TOKEN 5 (\"\" 66))"
;cd

;   "(LT-ERROR LISP::UNBOUND-VARIABLE 1 CD)"
;;      token 1                                                              ;;
;;      -------                                                              ;;
;       EOF
        (read-from-string "ab")
   ab

;       constituent/illegal2
;       'abAcd  
;   ABACD
                                        ; s.o.
;       unzulaessige :
        ::abc

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"<:>:ABC\"))"
        #::abc

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"#::ABC\"))"
        :::abc

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"<:>::ABC\"))"
        #:::abc

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"#:::ABC\"))"
        :a:bc

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"A<:>:BC\"))"
        #:a:bc

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"A<#:>:BC\"))"
        :a::bc

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"A<:>::BC\"))"
        #:a::bc

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"A<#:>::BC\"))"
        :abc:

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"ABC<:>:\"))"
        #:abc:

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"ABC<#:>:\"))"
        ab:

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"AB:
\"))"
        ab:cd:

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"AB:CD:\"))"
        ab::

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"AB::
\"))"
        a:b:c

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"A:B:C\"))"
        a:b::c

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"A:B::C\"))"
        a::b:c

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"A::B:C\"))"
        a::b::c

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"A::B::C\"))"

;       EOF nach \
(read-from-string "ab\\")
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"AB\\\\\"))"
;;"(LT-ERROR LISP::E-READ-TOKEN 4 (\"AB\\\\\"))"

;       unbekannte typ oder illegal1
        abB
   "(LT-ERROR LISP::E-READ-TOKEN 5 (\"AB\" 66))"
        ;cd
;;      token 2                                                              ;;
;;      -------                                                              ;;
;       EOF
(read-from-string "xx::a|bc")
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"Abc<|>\"))"
;;"(LT-ERROR LISP::E-READ-TOKEN 4 (\"Abc<|>\"))"
        
(read-from-string "xx::a\\")
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"A\\\\\"))"
;;"(LT-ERROR LISP::E-READ-TOKEN 4 (\"A\\\\\"))"

(read-from-string "xx:a\\")
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"A\\\\\"))"
;;"(LT-ERROR LISP::E-READ-TOKEN 4 (\"A\\\\\"))"

(read-from-string "ab|cd")
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"ABcd<|>\"))"
;;"(LT-ERROR LISP::E-READ-TOKEN 4 (\"ABcd<|>\"))"

(read-from-string "ab|")
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"AB<|>\"))"
;;"(LT-ERROR LISP::E-READ-TOKEN 4 (\"AB<|>\"))"

;       EOF nach \
(read-from-string "ab|c\\")
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"ABc<|>\\\\\"))"
;;"(LT-ERROR LISP::E-READ-TOKEN 4 (\"ABc<|>\\\\\"))"

;       unbekannter typ oder illegal1
(read-from-string "ab|cdB")
"(LT-ERROR LISP::E-READ-TOKEN 5 (\"ABcd<|>\" 66))"
        ;ef
;;      ende                                                                 ;;
;;      ----                                                                 ;;
;       : leer
        (read-from-string ":")
   "(LT-ERROR LISP::E-READ-TOKEN 3 (\":*\"))"
        
        (read-from-string ": ")
   "(LT-ERROR LISP::E-READ-TOKEN 3 (\": \"))"
        
        (read-from-string "#:")
   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"#:\"))"

        (read-from-string "#: ")
   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"#: \"))"
        
        (read-from-string "a:")
   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"A:*\"))"
        
        (read-from-string "a: ")
   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"A: \"))"
        
        (read-from-string "a::")
   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"A::*\"))"
        
        (read-from-string "a:: ")
   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"A:: \"))"
        
;       punkttoken
        ....

   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"....\"))"
;;;     fehlerbehandlung in scanobj                                         ;;;
;;;     ---------------------------                                         ;;;
;;      listen                                                               ;;
;;      ------                                                               ;;
;       EOF 
        (read-from-string "(a b" )
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((A B)) LISP::E-READ-TOKEN 4 
(\"\"))"

        (read-from-string "(a b " )
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((A B)) LISP::E-READ-TOKEN 4 
(\"\"))"

        (read-from-string "(a b" nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((A B)))"

        (read-from-string "(a b " nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((A B)))"

;       EOF nach .
        (read-from-string "(a b . ")
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((A B #\\.)) LISP::E-READ-TOKEN 
4 (\"\"))"

        (read-from-string "(a b ." nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((A B)))"

        (read-from-string "(a b . "  nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((A B)))"

        (read-from-string "(a b .")
;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((A B #\\.)))"
    "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((A B)))"

        (read-from-string "(a b . c")
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((A B C)) LISP::E-READ-TOKEN 
4 (\"\"))"

;       2. .obj
        (a b . . c d)
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((A B #\\. #\\. C D)))"

;       nur 1 obj fuer gep. paar
        (a .)
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((A #\\.)))"

        (. a)
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((A)))"

;       keine klammer nach 2.obj
        (a . b c d)
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((A B C D)))"

        (a . )
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((A #\\.)))"

;-------------------------------------------------------------------------------
;(progn (print "etwa drei viertel") t) T
;-------------------------------------------------------------------------------

;;      macros                                                               ;;
;;      ------                                                               ;;
;       EOF nach backquotekomma
        (read-from-string ",")
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((UNQUOTE)))"

        (read-from-string ", ")
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((UNQUOTE)) LISP::E-READ-TOKEN 
4 (\"\"))"

        (read-from-string "," nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((UNQUOTE)))"

        (read-from-string ", " nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((UNQUOTE)))"

;       . nach backquotekomma
        (read-from-string ",.")
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((LISP::UNQUOTE-NSPLICING)) 

LISP::E-READ-TOKEN 4 (\"\"))"

        (read-from-string ",. ")
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((LISP::UNQUOTE-NSPLICING)) 

LISP::E-READ-TOKEN 4 (\"\"))"

        (read-from-string ",." nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((LISP::UNQUOTE-NSPLICING)))"

        (read-from-string ",. " nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((LISP::UNQUOTE-NSPLICING)))"

;       ) nach backquotekomma
        (read-from-string ",)")
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((UNQUOTE)))"

        (read-from-string ",) ")
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((UNQUOTE)))"

        (read-from-string ",)" nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((UNQUOTE)))"

        (read-from-string ",) " nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((UNQUOTE)))"

;       } nach backquotekomma
;;        (read-from-string ",}")
;;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((UNQUOTE)))"

;;        (read-from-string ",} ")
;;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((UNQUOTE)))"

;;        (read-from-string ",}" nil 'dummy)
;;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((UNQUOTE)))"

;;        (read-from-string ",} " nil 'dummy)
;;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((UNQUOTE)))"

;       EOF nach quote
        (read-from-string "'")
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((QUOTE)) LISP::E-READ-TOKEN 
4 (\"\"))"

        (read-from-string "' ")
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((QUOTE)) LISP::E-READ-TOKEN 
4 (\"\"))"

        (read-from-string "'" nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 (NIL))"

        (read-from-string "' " nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 (NIL))"

;       . nach quote
        (read-from-string "'.")
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((QUOTE)))"

        (read-from-string "'. ")
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUOTE)))"

        (read-from-string "'." nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((QUOTE)))"

        (read-from-string "'. " nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUOTE)))"

;       ) nach quote
        (read-from-string "')")
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUOTE)))"

        (read-from-string "') ")
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUOTE)))"

        (read-from-string "')" nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUOTE)))"

        (read-from-string "') " nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUOTE)))"

;       } nach quote
;;        (read-from-string "'}")
;;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUOTE)))"

;;        (read-from-string "'} ")
;;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUOTE)))"

;;        (read-from-string "'}" nil 'dummy)
;;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUOTE)))"

;;        (read-from-string "'} " nil 'dummy)
;;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUOTE)))"


;&----- Tread4.tst -------------------------------------------------------------
;(setq lisptest::*error-message* t)
;   T
;(setq illegal1 (code-char 7))
;   #\
;(set-syntax-from-char #\B illegal1)
;   T
;       EOF nach backquote
        (read-from-string "`")
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((QUASIQUOTE)) LISP::E-READ-TOKEN 
4 (\"\"))"

        (read-from-string "` ")
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((QUASIQUOTE)) LISP::E-READ-TOKEN 
4 (\"\"))"

        (read-from-string "`" nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 (NIL))"

        (read-from-string "` " nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 (NIL))"

;       . nach backquote
        (read-from-string "`.")
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((QUASIQUOTE)))"

        (read-from-string "`. ")
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUASIQUOTE)))"

        (read-from-string "`." nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 0 ((QUASIQUOTE)))"

        (read-from-string "`. " nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUASIQUOTE)))"

;       ) nach backquote
        (read-from-string "`)")
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUASIQUOTE)))"

        (read-from-string "`) ")
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUASIQUOTE)))"

        (read-from-string "`)" nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUASIQUOTE)))"

        (read-from-string "`) " nil 'dummy)
   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUASIQUOTE)))"

;       } nach backquote
;;        (read-from-string "`}")
;;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUASIQUOTE)))"

;;        (read-from-string "`} ")
;;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUASIQUOTE)))"

;;        (read-from-string "`}" nil 'dummy)
;;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUASIQUOTE)))"

;;        (read-from-string "`} " nil 'dummy)
;;   "(LT-ERROR LISP::E-READ-STRUCTURE 1 ((QUASIQUOTE)))"

;       EOF im string
        (read-from-string "\"a")
   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"a<\\\">\"))"

        (read-from-string "\"")
   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"<\\\">\"))"

        (read-from-string "\"a" nil 'dummy)
   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"a<\\\">\"))"

        (read-from-string "\"" nil 'dummy)
   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"<\\\">\"))"

;       EOF nach \ im string
        (read-from-string "\"a\\")
   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"a<\\\">\\\\\"))"

        (read-from-string "\"\\")
   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"<\\\">\\\\\"))"

        (read-from-string "\"a\\" nil 'dummy)
   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"a<\\\">\\\\\"))"

        (read-from-string "\"\\" nil 'dummy)
   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"<\\\">\\\\\"))"

;;      disp-macros                                                          ;;
;;      -----------                                                          ;;
;       EOF in infixarg
        (read-from-string "#12")
   "(LT-ERROR E-READ-MACRO 0 (#\\# 12 #\\SPACE NIL))"

        (read-from-string "#")
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\SPACE NIL))"

        (read-from-string "#12" nil 'dummy)
   "(LT-ERROR E-READ-MACRO 0 (#\\# 12 #\\SPACE NIL))"

        (read-from-string "#" nil 'dummy)
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\SPACE NIL))"

;       EOF oder illegale zeichenfolge in char
        (read-from-string "#\\")
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\\\ \"\"))"

        (read-from-string "#\\" nil 'dummy)
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\\\ \"\"))"

        (read-from-string "#\\con")
   "(LT-ERROR E-READ-MACRO 0 (#\\# 256 #\\\\ \"\"))"

        (read-from-string "#\\con" nil 'dummy)
   "(LT-ERROR E-READ-MACRO 0 (#\\# 256 #\\\\ \"\"))"

        (read-from-string "#\\m-com-a")
   "(LT-ERROR E-READ-MACRO 0 (#\\# 512 #\\\\ \"COM-a\"))"


;       keine Zahl in #R
        #10R1a2

   "(LT-ERROR E-READ-MACRO 1 (#\\# 10 #\\R \"1A2\"))"
;       keine zahlensyntax in #R
        #10Ra12

   "(LT-ERROR E-READ-MACRO 1 (#\\# 10 #\\R \"A12\"))"
;       keine Readbase in #R
        #40R123

   "(LT-ERROR E-READ-MACRO 1 (#\\# 40 #\\R \"123\"))"
;       EOF oder nichtconstituent nach #:
        (read-from-string "#:")
   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"#:\"))"

        (read-from-string "#:" nil 'dummy)
   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"#:\"))"

        (read-from-string "#:B")
   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"#:B\"))"

;       kein Atom nach #:
        (read-from-string "#:( a b )")
   "(LT-ERROR LISP::E-READ-TOKEN 3 (\"#:(\"))"

#:123   
"(LT-ERROR UNBOUND-VARIABLE 1 #:\\123)"

;       kein subchar
#0111010
"(LT-ERROR E-READ-MACRO 2 (#\\# 111010 #\\NEWLINE NIL))"

;;;;    test fehlerbehandlung in COMREAD1                                  ;;;;
;;;;    ---------------------------------                                  ;;;;
;;      kommentar                                                            ;;
;;      ---------                                                            ;;
;       EOF
        (read-from-string "#|")  ;; |#
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\| NIL))"

        (read-from-string "#|abc")  ;; |#
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\| NIL))"

        (read-from-string "#|abc|")  ;; |#
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\| NIL))"

        (read-from-string "#|abc#")  ;; |#
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\| NIL))"

        (read-from-string "#|abc#|de#xyz")  ;; |# |#
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\| NIL))"

;;      vektor                                                              ;;
;;      ------                                                              ;;
;       EOF
        (read-from-string "#(a b c")
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\( (A B C)) LISP::E-READ-TOKEN 
4 (\"\"))"

;       gelesene anzahl zu gross
        #3(a b c d e f)
   "(LT-ERROR E-READ-MACRO 1 (#\\# 3 #\\( (A B C D E F)))"

;       falscher abschluss fuer vector
        (read-from-string "#(a b c . d e f)")
   "(LT-ERROR E-READ-MACRO 1 (#\\# 0 #\\( (A B C #\\. D E F)))"

;;      Zeichen                                                             ;;
;;      -------                                                             ;;
;       EOF als 1.zeichen
        (read-from-string "#\\")
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\\\ \"\"))"

        (read-from-string "#\\a")
   #\a

        #\a\abc-de

   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\\\ \"a\\\\abc-de
\"))"
        (read-from-string "#\\a|abc-de")

   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\\\ \"a|abc-de
\"))"
        #\a#abc-de

   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\\\ \"a#abc-de
\"))"
        #\con\abc-de

   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\\\ \"CON\\\\abc-de
\"))"
        (read-from-string "#\\\\")
   #\\

        (read-from-string "#\\con-\\")
   "(LT-ERROR E-READ-MACRO 0 (#\\# 256 #\\\\ \"\\\\\"))"

        (read-from-string "#\\con|abc-de")

   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\\\ \"CON|abc-de
\"))"
        #\con#abc-de

   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\\\ \"CON#abc-de
\"))"
        #\con-abc

   "(LT-ERROR E-READ-MACRO 0 (#\\# 256 #\\\\ \"ABC\"))"

;;;     test fehlerbehandlung in COMREAD2                                 ;;;
;;;     ---------------------------------                                 ;;;
;;      Searcalm                                                           ;;
;;      --------                                                           ;;
;       keine funktionsliste
(make-dispatch-macro-character #\X t)
T

Xa
"(LT-ERROR E-READ-MACRO 2 (#\\X NIL #\\a NIL))"

;       keine dispatchmacroliste am zeichen
(set-macro-character #\Y #'cdr t)
T

(make-dispatch-macro-character #\Y t)
T

Ya
"(LT-ERROR E-READ-MACRO 2 (#\\Y NIL #\\a NIL))"

;       subchar nicht in liste
(set-dispatch-macro-character #\Y #\. #'car)
T

Y,
"(LT-ERROR E-READ-MACRO 2 (#\\Y NIL #\\, NIL))"

(progn (SETQ *READTAbLE* (COPY-READTAbLE nil)) t) 
T

;;      Makearray                                                         ;;
;;      ---------                                                         ;;
;       dimension = 0 und EOF
        (read-from-string "#0a")
   "(LT-ERROR E-READ-MACRO 1 (#\\# 0 #\\a NIL) LISP::E-READ-TOKEN 4 
(\"\"))"

        (read-from-string "#0a" nil 'dummy)
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\a NIL))"

;       dimension = 0 und ERROR
        #0a.

   "(LT-ERROR E-READ-MACRO 1 (#\\# 0 #\\a #\\.))"
        (read-from-string "#0a)")
   "(LT-ERROR E-READ-MACRO 1 (#\\# 0 #\\a #\\)))"

;;        (read-from-string "#0a}")
;;   "(LT-ERROR E-READ-MACRO 1 (#\\# 0 #\\a #\\}))"

;       klammereinlesen EOF
        (read-from-string "#3a(((1)(2))(" )
   "(LT-ERROR E-READ-MACRO 0 (#\\# 3 #\\a (1 2)))"

        (read-from-string "#3a(((1)(2))((3" )
   "(LT-ERROR E-READ-MACRO 0 (#\\# 3 #\\a (1 2 3)) LISP::E-READ-TOKEN 
4 (\"\"))"

;       klammereinlesen falsche termacro 
        (multiple-value-list (read-from-string "#3a(((1)(2))(;xyz"))
   "(LT-ERROR E-READ-MACRO 1 (#\\# 3 #\\a (1 2 #\\;)))"

        (multiple-value-list (read-from-string "#3a(((1)(2))((3;xyz"))
   "(LT-ERROR E-READ-MACRO 0 (#\\# 3 #\\a (1 2 3)) LISP::E-READ-TOKEN 
4 (\"\"))"

;       klammereinlesen falsche zeichen
(read-from-string "#3a(((1)(2))(axyz")
"(LT-ERROR E-READ-MACRO 1 (#\\# 3 #\\a (1 2 #\\a)))"


;&----- Tread5.tst -------------------------------------------------------------
(setq lisptest::*error-message* t)
T

(read-from-string "#3a(((1)(2))((3 axyz")
"(LT-ERROR E-READ-MACRO 0 (#\\# 3 #\\a (1 2 3 AXYZ)) LISP::E-READ-TOKEN 
4 (\"\"))"

;       EOF in obj
        (read-from-string "#3a(((1)(2))((" )
   "(LT-ERROR E-READ-MACRO 0 (#\\# 3 #\\a (1 2)) LISP::E-READ-TOKEN 
4 (\"\"))"

        (read-from-string "#3a(((1)(2))((" nil 'dummy)
   "(LT-ERROR E-READ-MACRO 0 (#\\# 3 #\\a (1 2)))"

;       falsche zeichen fuer klammer zu
        #3a(((1)(2))((3 )(4))((5)(6)))
   #3A(((1)(2))((3)(4))((5)(6)))

;;#3a(((1)(2))((3}
;;"(LT-ERROR E-READ-MACRO 1 (#\\# 3 #\\a (1 2 3)))"

xyz
"(LT-ERROR UNBOUND-VARIABLE 1 XYZ)"

(read-from-string "#3a(((1)(2))((3(xyz")
"(LT-ERROR LISP::E-READ-STRUCTURE 0 ((XYZ)) LISP::E-READ-TOKEN 4 (\"\"))"

        (read-from-string "#3a(((1)(2))((3" )
   "(LT-ERROR E-READ-MACRO 0 (#\\# 3 #\\a (1 2 3)) LISP::E-READ-TOKEN 
4 (\"\"))"

;       falsche klammerung
        #3a(((1) (2))((3 4)
   "(LT-ERROR E-READ-MACRO 1 (#\\# 3 #\\a (1 2 3 4)))"

;;      Mbitvector                                                           ;;
;;      ----------                                                           ;;
;       falsche zeichen
        #*11001a11

   "(LT-ERROR E-READ-MACRO 1 (#\\# 0 #\\* (49 49 48 48 49 97 49 49)))"
;       zu viele elemente
        #5*111001101

   "(LT-ERROR E-READ-MACRO 1 (#\\# 5 #\\* (49 49 49 48 48 49 49 48 
49)))"
;;      Makemacro                                                            ;;
;;      ---------                                                            ;;
;       complex-obj nicht 2elementige liste

;;;declare ?    
#cab
"(LT-ERROR E-READ-MACRO 1 (#\\# NIL #\\c AB))"
;;;declare ?
#c(1)
"(LT-ERROR E-READ-MACRO 1 (#\\# NIL #\\c (1)))"

#c(1 2 3)
"(LT-ERROR E-READ-MACRO 1 (#\\# NIL #\\c (1 2 3)))"

(read-from-string "#c")
;alt   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\c NIL) LISP::E-READ-TOKEN 
4 (\"\"))"
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"\"))"

;       EOF nach #'
        (read-from-string "#'" )
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\' NIL) LISP::E-READ-TOKEN 4 
(\"\"))"

        (read-from-string "#'" nil 'dummy)
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\' NIL))"

;       ERROR nach #'
        #')
   "(LT-ERROR E-READ-MACRO 1 (#\\# 0 #\\' #\\)))"

;;        #'}
;;   "(LT-ERROR E-READ-MACRO 1 (#\\# 0 #\\' #\\}))"

        #'.

   "(LT-ERROR E-READ-MACRO 1 (#\\# 0 #\\' #\\.))"
;       EOF nach #+
        (read-from-string "#+" )
;alt   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"\"))"
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"\"))"

        (read-from-string "#+" nil 'dummy)
;falsch   DUMMY
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"\"))"


;       ERROR nach #+
        (READ-FROM-STRING "#+)")
   "(LT-ERROR E-READ-MACRO 3 (#\\)))"

;;        (READ-FROM-STRING "#+}")
;;   "(LT-ERROR E-READ-MACRO 3 (#\\}))"

        (READ-FROM-STRING "#+.")
   "(LT-ERROR E-READ-MACRO 3 (#\\.))"

;       kein symbol oder liste nach #+
        (READ-FROM-STRING "#+123")
;alt   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"\"))"
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"\"))"

;       EOF nach #-
        (read-from-string "#-" )
;alt   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"\"))"
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"\"))"

        (read-from-string "#-" nil 'dummy)
;fatsch   DUMMY
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"\"))"

;       ERROR nach #-
        (read-from-string "#-)")
   "(LT-ERROR E-READ-MACRO 3 (#\\)))"

;;        (read-from-string "#-}")
;;   "(LT-ERROR E-READ-MACRO 3 (#\\}))"

        (read-from-string "#-.")
   "(LT-ERROR E-READ-MACRO 3 (#\\.))"

;       kein symbol oder liste nach #-
        (read-from-string "#-123")
;alt   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"\"))"
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"\"))"

;       EOF nach #.
        (read-from-string "#." )
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\. NIL) LISP::E-READ-TOKEN 4 
(\"\"))"

        (read-from-string "#." nil 'dummy)
   "(LT-ERROR E-READ-MACRO 0 (#\\# 0 #\\. NIL))"

;       ERROR nach #.
        #.)
   "(LT-ERROR E-READ-MACRO 1 (#\\# 0 #\\. #\\)))"

;;        #.}
;;   "(LT-ERROR E-READ-MACRO 1 (#\\# 0 #\\. #\\}))"

        #..

   "(LT-ERROR E-READ-MACRO 1 (#\\# 0 #\\. #\\.))"
;       nach #1a keine (
        #1ax
   "(LT-ERROR E-READ-MACRO 1 (#\\# 1 #\\a #\\x))"

        (read-from-string "#1a")
   "(LT-ERROR E-READ-MACRO 1 (#\\# 1 #\\a #\\*))"       
;       EOF nach #n=
        (read-from-string "#1=" )
;alt   "(LT-ERROR LISP::E-READ-TOKEN 4 (\"\"))"
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"\"))"

        (read-from-string "#1=" nil 'dummy)
;falsch   DUMMY
"(LT-ERROR E-READ-MACRO 3 (NIL) LISP::E-READ-TOKEN 4 (\"\"))"

;       ERROR nach #1=
        (READ-FROM-STRING "#1=)")
   "(LT-ERROR E-READ-MACRO 3 (#\\)))"

;;        (READ-FROM-STRING "#1=}")
;;   "(LT-ERROR E-READ-MACRO 3 (#\\}))"

        (READ-FROM-STRING "#1=.")
   "(LT-ERROR E-READ-MACRO 3 (#\\.))"

;       label nicht gefunden
        (READ-FROM-STRING "#11111#")
   "(LT-ERROR E-READ-MACRO 1 (#\\# 11111 #\\# NIL))"

(progn
(makunbound 'illegal1)
(setq lisptest::*error-message* nil)
(setq *print-circle* t) 
T)
T
|#

