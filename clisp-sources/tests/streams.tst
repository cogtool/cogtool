;; -*- lisp -*-
(progn (in-package "COMMON-LISP-USER") t) t

#-(or AKCL ECL ALLEGRO SBCL OpenMCL) (PRIN1-TO-STRING (MAKE-BROADCAST-STREAM))
#+XCL "#<%TYPE-STRUCTURE-STREAM NIL>"
#+CLISP "#<OUTPUT BROADCAST-STREAM>"
#+CMU "#<Broadcast Stream>"
#+LISPWORKS "#<Broadcast stream to ()>"
#-(or XCL CLISP AKCL ECL ALLEGRO CMU SBCL OpenMCL LISPWORKS) UNKNOWN

;; make sure that DESCRIBE does not try to look up CLHS documentation
#+clisp (defun custom:clhs-root () nil) #+clisp ext:clhs-root

;; CLOSE should not delete information about
;; element type, direction, and external format
;; Note that CLHS <http://www.lisp.org/HyperSpec/Body/sec_21-1-3.html>
;; 21.1.3 "Stream Arguments to Standardized Functions"
;; says that INPUT-STREAM-P &c do not operate on closed streams while
;; the comp.lang.lisp thread "CLOSE and OUTPUT-STREAM-P"
;; <http://groups.google.com/groups?hl=en&lr=&ie=UTF-8&th=e0c06a88910db64b&rnum=1>
;; appears to imply otherwise; we follow the opinion of the users.
(defun close-1 (s)
  #-(or CMU SBCL)
  (let* ((i (input-stream-p s))
         (o (output-stream-p s))
         (e (stream-element-type s))
         (f (stream-external-format s))
         (c (close s)))
    (and (eq i (input-stream-p s))
         (eq o (output-stream-p s))
         (equal e (stream-element-type s))
         (equal f (stream-external-format s))
         c))
  #+(or CMU SBCL)
  (close s))
close-1

(PROGN (SETQ S1 (OPEN "d1.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE))
(SETQ S2 (OPEN "d2.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE))
(SETQ S3 (OPEN "d3.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE))
(SETQ B1 (MAKE-BROADCAST-STREAM S1 S2 S3 *STANDARD-OUTPUT*)) T)   T

(PRINT "test broadcast satz 1" B1)   "test broadcast satz 1"

(PRINT "test broadcast satz 2" B1)   "test broadcast satz 2"

(PRINT "test broadcast satz 3" B1)   "test broadcast satz 3"

(CLOSE-1 S1)   T

(CLOSE-1 S2)   T

(CLOSE-1 S3)   T

(PROGN (SETQ S (OPEN "d1.plc")) T)   T

(READ S)   "test broadcast satz 1"

(READ S)   "test broadcast satz 2"

(READ S)   "test broadcast satz 3"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "d2.plc")) T)   T

(READ S)   "test broadcast satz 1"

(READ S)   "test broadcast satz 2"

(READ S)   "test broadcast satz 3"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "d3.plc")) T)   T

(READ S)   "test broadcast satz 1"

(READ S)   "test broadcast satz 2"

(READ S)   "test broadcast satz 3"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "t0.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE)) T)   T

(PRINT (QUOTE READ1) S)   READ1

(PRINT (QUOTE READ2) S)   READ2

(CLOSE-1 S)   T

(PROGN (SETQ INPTW (OPEN "t0.plc"))
(SETQ S1 (OPEN "d1.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE))
(SETQ S2 (OPEN "d2.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE))
(SETQ SY (MAKE-SYNONYM-STREAM (QUOTE S2)))
(SETQ S3 (OPEN "d3.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE))
(SETQ TW (MAKE-TWO-WAY-STREAM INPTW S3))
(SETQ S4 (OPEN "d4.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE))
(SETQ EC (MAKE-ECHO-STREAM INPTW S4))
(SETQ S5 (OPEN "d5.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE))
(SETQ S6 (OPEN "d6.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE))
(SETQ B1 (MAKE-BROADCAST-STREAM S5 S6))
(SETQ S7 (OPEN "d7.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE))
(SETQ B2 (MAKE-BROADCAST-STREAM S1 SY TW EC B1 S7)) T)   T

(PRINT "w to b2 1.satz" B2)   "w to b2 1.satz"

(PRINT "w to sy" SY)   "w to sy"

(PRINT "w to b2 2.satz" B2)   "w to b2 2.satz"

(PRINT "w to tw" TW)   "w to tw"

(PRINT "w to b2 3.satz" B2)   "w to b2 3.satz"

(PRINT "w to ec" EC)   "w to ec"

(PRINT "w to b2 4.satz" B2)   "w to b2 4.satz"

(PRINT "w to b1" B1)   "w to b1"

(PRINT "w to b2 5.satz" B2)   "w to b2 5.satz"

(PRINT "w to s7" S7)   "w to s7"

(PRINT "w to b2 6.satz" B2)   "w to b2 6.satz"

(READ TW)   READ1

(READ EC)   READ2

(PRINT "w to b2 7.satz" B2)   "w to b2 7.satz"

(PRINT "w to b2 8.satz" B2)   "w to b2 8.satz"

(CLOSE-1 INPTW)   T

(CLOSE-1 S1)   T

(CLOSE-1 S2)   T

(CLOSE-1 S3)   T

(CLOSE-1 S4)   T

(CLOSE-1 S5)   T

(CLOSE-1 S6)   T

(CLOSE-1 S7)   T

(PROGN (SETQ S (OPEN "d1.plc")) T)   T

(READ S)   "w to b2 1.satz"

(READ S)   "w to b2 2.satz"

(READ S)   "w to b2 3.satz"

(READ S)   "w to b2 4.satz"

(READ S)   "w to b2 5.satz"

(READ S)   "w to b2 6.satz"

(READ S)   "w to b2 7.satz"

(READ S)   "w to b2 8.satz"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "d2.plc")) T)   T

(READ S)   "w to b2 1.satz"

(READ S)   "w to sy"

(READ S)   "w to b2 2.satz"

(READ S)   "w to b2 3.satz"

(READ S)   "w to b2 4.satz"

(READ S)   "w to b2 5.satz"

(READ S)   "w to b2 6.satz"

(READ S)   "w to b2 7.satz"

(READ S)   "w to b2 8.satz"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "d3.plc")) T)   T

(READ S)   "w to b2 1.satz"

(READ S)   "w to b2 2.satz"

(READ S)   "w to tw"

(READ S)   "w to b2 3.satz"

(READ S)   "w to b2 4.satz"

(READ S)   "w to b2 5.satz"

(READ S)   "w to b2 6.satz"

(READ S)   "w to b2 7.satz"

(READ S)   "w to b2 8.satz"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "d4.plc")) T)   T

(READ S)   "w to b2 1.satz"

(READ S)   "w to b2 2.satz"

(READ S)   "w to b2 3.satz"

(READ S)   "w to ec"

(READ S)   "w to b2 4.satz"

(READ S)   "w to b2 5.satz"

(READ S)   "w to b2 6.satz"

(READ S)   READ2

(READ S)   "w to b2 7.satz"

(READ S)   "w to b2 8.satz"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "d5.plc")) T)   T

(READ S)   "w to b2 1.satz"

(READ S)   "w to b2 2.satz"

(READ S)   "w to b2 3.satz"

(READ S)   "w to b2 4.satz"

(READ S)   "w to b1"

(READ S)   "w to b2 5.satz"

(READ S)   "w to b2 6.satz"

(READ S)   "w to b2 7.satz"

(READ S)   "w to b2 8.satz"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "d6.plc")) T)   T

(READ S)   "w to b2 1.satz"

(READ S)   "w to b2 2.satz"

(READ S)   "w to b2 3.satz"

(READ S)   "w to b2 4.satz"

(READ S)   "w to b1"

(READ S)   "w to b2 5.satz"

(READ S)   "w to b2 6.satz"

(READ S)   "w to b2 7.satz"

(READ S)   "w to b2 8.satz"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "d7.plc")) T)   T

(READ S)   "w to b2 1.satz"

(READ S)   "w to b2 2.satz"

(READ S)   "w to b2 3.satz"

(READ S)   "w to b2 4.satz"

(READ S)   "w to b2 5.satz"

(READ S)   "w to s7"

(READ S)   "w to b2 6.satz"

(READ S)   "w to b2 7.satz"

(READ S)   "w to b2 8.satz"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "t1.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE)) T)   T

(PRINT "1.satz t1" S)   "1.satz t1"

(PRINT "2.satz t1" S)   "2.satz t1"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "t2.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE)) T)   T

(PRINT "1.satz t2" S)   "1.satz t2"

(PRINT "2.satz t2" S)   "2.satz t2"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "t3.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE)) T)   T

(PRINT "1.satz t3" S)   "1.satz t3"

(PRINT "2.satz t3" S)   "2.satz t3"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "t4.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE)) T)   T

(PRINT "1.satz t4" S)   "1.satz t4"

(PRINT "2.satz t4" S)   "2.satz t4"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "t5.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE)) T)   T

(PRINT "1.satz t5" S)   "1.satz t5"

(PRINT "2.satz t5" S)   "2.satz t5"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "t6.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE)) T)   T

(PRINT "1.satz t6" S)   "1.satz t6"

(PRINT "2.satz t6" S)   "2.satz t6"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "t7.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE)) T)   T

(PRINT "1.satz t7" S)   "1.satz t7"

(PRINT "2.satz t7" S)   "2.satz t7"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "t8.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE)) T)   T

(PRINT "1.satz t8" S)   "1.satz t8"

(PRINT "2.satz t8" S)   "2.satz t8"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "t9.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE)) T)   T

(PRINT "1.satz t9" S)   "1.satz t9"

(PRINT "2.satz t9" S)   "2.satz t9"

(CLOSE-1 S)   T

(PROGN (SETQ S (OPEN "t10.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE)) T)   T

(PRINT "1.satz t10" S)   "1.satz t10"

(PRINT "2.satz t10" S)   "2.satz t10"

(CLOSE-1 S)   T

(PROGN (SETQ S1 (OPEN "t1.plc")) (SETQ S2 (OPEN "t2.plc"))
(SETQ S3 (OPEN "t3.plc")) (SETQ S4 (OPEN "t4.plc")) (SETQ S5 (OPEN
"t5.plc"))
(SETQ C1 (MAKE-CONCATENATED-STREAM S1 S2 S3))
(SETQ C2 (MAKE-CONCATENATED-STREAM S4 S5)) T)   T

(READ C1)   "1.satz t1"

(READ C2)   "1.satz t4"

(READ C1)   "2.satz t1"

(READ C1)   "1.satz t2"

(READ C2)   "2.satz t4"

(READ C2)   "1.satz t5"

(READ C1)   "2.satz t2"

(READ C1)   "1.satz t3"

(READ C1)   "2.satz t3"

(READ C2)   "2.satz t5"

(CLOSE-1 S1)   T

(CLOSE-1 S2)   T

(CLOSE-1 S3)   T

(CLOSE-1 S4)   T

(CLOSE-1 S5)   T

(PROGN (SETQ S1 (OPEN "t1.plc")) (SETQ S2 (OPEN "t2.plc"))
(SETQ S3 (OPEN "t3.plc")) (SETQ S4 (OPEN "t4.plc")) (SETQ S5 (OPEN
"t5.plc"))
(SETQ S6 (OPEN "t6.plc")) (SETQ S7 (OPEN "t7.plc")) (SETQ S8 (OPEN
"t8.plc"))
(SETQ S9 (OPEN "t9.plc")) (SETQ S10 (OPEN "t10.plc"))
(SETQ C1 (MAKE-CONCATENATED-STREAM S1 S2))
(SETQ C2 (MAKE-CONCATENATED-STREAM S3))
(SETQ C3 (MAKE-CONCATENATED-STREAM C1 C2 S4))
(SETQ C4 (MAKE-CONCATENATED-STREAM S5 S6 S7 S8 S9 S10)) T)   T

(READ C4)   "1.satz t5"

(READ C3)   "1.satz t1"

(READ C4)   "2.satz t5"

(READ C4)   "1.satz t6"

(READ C3)   "2.satz t1"

(READ C3)   "1.satz t2"

(READ C4)   "2.satz t6"

(READ C4)   "1.satz t7"

(READ C4)   "2.satz t7"

(READ C3)   "2.satz t2"

(READ C3)   "1.satz t3"

(READ C3)   "2.satz t3"

(READ C4)   "1.satz t8"

(READ C4)   "2.satz t8"

(READ C4)   "1.satz t9"

(READ C4)   "2.satz t9"

(READ C3)   "1.satz t4"

(READ C3)   "2.satz t4"

(READ C4)   "1.satz t10"

(READ C4)   "2.satz t10"

(CLOSE-1 S1)   T

(CLOSE-1 S2)   T

(CLOSE-1 S3)   T

(CLOSE-1 S4)   T

(CLOSE-1 S5)   T

(CLOSE-1 S6)   T

(CLOSE-1 S7)   T

(CLOSE-1 S8)   T

(CLOSE-1 S9)   T

(CLOSE-1 S10)   T

(SETQ STR1 "test 123456")   "test 123456"

(PROGN (SETQ S1 (MAKE-STRING-INPUT-STREAM STR1)) T)   T

(READ S1)   TEST

(READ-CHAR S1)   #\1

(READ-CHAR S1)   #\2

(UNREAD-CHAR #\2 S1)   NIL

(READ-CHAR S1)   #\2

(READ-CHAR S1)   #\3

(READ-CHAR S1)   #\4

(UNREAD-CHAR #\A S1)   ERROR

(READ-CHAR S1)   #\5

(READ-CHAR S1)   #\6

(CLOSE-1 S1)   T

STR1   "test 123456"

(multiple-value-list (READ-FROM-STRING "012345 789"))   (12345 7)

(multiple-value-list (READ-FROM-STRING "012345 789" T NIL
                :PRESERVE-WHITESPACE T))   (12345 6)

(multiple-value-list (READ-FROM-STRING "012345 789" T NIL :END 4))
(123 4)

(multiple-value-list (READ-FROM-STRING "012345 789" T NIL :START 2))
(2345 7)

(PROGN (SETQ STRGSTREAM (MAKE-STRING-INPUT-STREAM "0123456789" 5 8)) T)
T

(READ STRGSTREAM)   567

(let* ((s "0123456789")
       (d (make-array 5 :displaced-to s :displaced-index-offset 3
                        :element-type (array-element-type s)))
       (i (make-string-input-stream d 2 5)))
  (read i))
567

(PROGN (SETQ STRGSTREAM
(MAKE-STRING-INPUT-STREAM "wenn alles gut geht ist das ein stream 012")) T)
T

(READ STRGSTREAM)   WENN

(READ STRGSTREAM)   ALLES

(READ STRGSTREAM)   GUT

(READ STRGSTREAM)   GEHT

(READ STRGSTREAM)   IST

(READ STRGSTREAM)   DAS

(READ STRGSTREAM)   EIN

(READ STRGSTREAM)   STREAM

(READ STRGSTREAM)   12

(PROGN (SETQ STRGSTREAM (MAKE-STRING-OUTPUT-STREAM)) T)   T

(PRINC "das " STRGSTREAM)   "das "

(PRINC "ist " STRGSTREAM)   "ist "

(PRINC "ein " STRGSTREAM)   "ein "

(PRINC "string " STRGSTREAM)   "string "

(PRINC "output " STRGSTREAM)   "output "

(PRINC "stream " STRGSTREAM)   "stream "

(GET-OUTPUT-STREAM-STRING STRGSTREAM)   "das ist ein string output stream "

(GET-OUTPUT-STREAM-STRING STRGSTREAM)   ""

(PRINC "das ist ein neuer string output stream" STRGSTREAM)
"das ist ein neuer string output stream"

(GET-OUTPUT-STREAM-STRING STRGSTREAM)
"das ist ein neuer string output stream"

(SETQ *PRINT-LENGTH* 50)   50

(WRITE-TO-STRING 123456789)   "123456789"

"(write-to-string '#1=(123456789 . #1#))"
"(write-to-string '#1=(123456789 . #1#))"

(PRIN1-TO-STRING "abc")   "\"abc\""

(PRINC-TO-STRING "abc")   "abc"

(PROGN (SETQ OS (MAKE-STRING-OUTPUT-STREAM)) T)   T

(SETQ S50 "123456789A123456789B123456789C123456789D12345678
E")   "123456789A123456789B123456789C123456789D12345678
E"

(SETQ S49 "123456789A123456789B123456789C123456789D1234567
*")   "123456789A123456789B123456789C123456789D1234567
*"

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S49 OS)   "123456789A123456789B123456789C123456789D1234567
*"

(PRINC "A" OS)   "A"

(PRINC "B" OS)   "B"

(PRINC "C" OS)   "C"

(LENGTH (PRINC (GET-OUTPUT-STREAM-STRING OS)))   402

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S50 OS)   "123456789A123456789B123456789C123456789D12345678
E"

(PRINC S49 OS)   "123456789A123456789B123456789C123456789D1234567
*"

(PRINC S49 OS)   "123456789A123456789B123456789C123456789D1234567
*"

(PRINC S49 OS)   "123456789A123456789B123456789C123456789D1234567
*"

(PRINC S49 OS)   "123456789A123456789B123456789C123456789D1234567
*"

(LENGTH (PRINC (GET-OUTPUT-STREAM-STRING OS)))   496

(PROGN (SETQ OS (OPEN "d0.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE))
(SETQ OS1 (OPEN "d1.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE))
(SETQ IS (OPEN "t0.plc" :DIRECTION :OUTPUT #+(or CMU SBCL LISPWORKS) :IF-EXISTS #+(or CMU SBCL LISPWORKS) :SUPERSEDE)) T)   T

(PRINC "'(a b #.(print \"1.zwischenwert\" os1) c d)" IS)
"'(a b #.(print \"1.zwischenwert\" os1) c d)"

(PRINC "'(a b #.(prin1-to-string \"2.zwischenwert\") c d)" IS)
"'(a b #.(prin1-to-string \"2.zwischenwert\") c d)"

(PRINC "'(a b #.(format nil  \"3.zwischenwert\") c d)" IS)
"'(a b #.(format nil  \"3.zwischenwert\") c d)"

(CLOSE-1 IS)   T

(PROGN (SETQ IS (OPEN "t0.plc")) (SETQ ES (MAKE-ECHO-STREAM IS OS))
T)   T

(PRINT "ausgabe os1" OS1)   "ausgabe os1"

(READ ES)   (QUOTE (A B "1.zwischenwert" C D))

(PRINT "ausgabe os1" OS1)   "ausgabe os1"

(READ ES)   (QUOTE (A B "\"2.zwischenwert\"" C D))

(PRINT "ausgabe os1" OS1)   "ausgabe os1"

(READ ES)   (QUOTE (A B "3.zwischenwert" C D))

(PRINT "ausgabe os1" OS1)   "ausgabe os1"

(CLOSE-1 IS)   T

(CLOSE-1 OS)   T

(PROGN (SETQ IS (OPEN "d0.plc")) T)   T

(READ IS)   (QUOTE (A B "1.zwischenwert" C D))

(READ IS)   (QUOTE (A B "\"2.zwischenwert\"" C D))

(READ IS)   (QUOTE (A B "3.zwischenwert" C D))

(CLOSE-1 IS)   T

(CLOSE-1 OS1)   T

(PROGN (SETQ IS (OPEN "d1.plc")) T)   T

(READ IS)   "ausgabe os1"

(READ IS)   "1.zwischenwert"

(READ IS)   "ausgabe os1"

(READ IS)   "ausgabe os1"

(READ IS)   "ausgabe os1"

(READ IS)   "1.zwischenwert"

(CLOSE-1 IS)   T

(progn (mapc #'delete-file (directory "*.plc")) t)
T

#+clisp (progn
(setq s1 (make-instance 'fill-stream :stream *standard-output*)
      s2 (make-synonym-stream 's1)
      s3 (make-broadcast-stream s1 s2))
(list (stream-element-type s1)
      (stream-element-type s2)
      (stream-element-type s3)))
#+clisp
(CHARACTER CHARACTER CHARACTER)

#+clisp
(progn
  (defvar *my-indent-level*)
  (with-output-to-string (out)
    (let ((*print-right-margin* 20)
          (*print-pretty* t)
          (*my-indent-level* 2))
      (with-fill-stream (fill out :text-indent '*my-indent-level*)
        (format fill "~%this is some long sentence which will      be broken at spaces")
        (force-output fill)
        (let ((*my-indent-level* 5))
          (format fill "~%and    properly indented to the level specified by the ~S argument which can be a ~S or an ~S - cool!" :TEXT-INDENT 'symbol 'integer))
        (format fill "~%Don't forget  to call ~S on it, and/or use ~S   Pretty formatting of the  S-expressions    printed with ~~S is  preserved: ~S" 'force-output 'with-fill-stream '(defun qu (x y z) (if x (+ y z) (* y z)))))))
)
#+clisp "
  this is some long
  sentence which
  will be broken at
  spaces
     and properly
     indented to
     the level
     specified by
     the
      :TEXT-INDENT
     argument which
     can be a
      SYMBOL
     or an INTEGER
     - cool!
  Don't forget to
  call FORCE-OUTPUT
  on it, and/or use
   WITH-FILL-STREAM
  Pretty formatting
  of the
  S-expressions
  printed with ~S
  is preserved:
   (DEFUN QU (X Y Z)
    (IF X (+ Y Z)
     (* Y Z)))
"


#+clisp
(with-output-to-string (*error-output*)
  (letf ((*current-language* 'english))
    (loop :with *print-pretty* = t :with *print-right-margin* = 60
      :for *fill-indent-sexp*
      :in (list 3 20 nil t #'1+ #'1- (lambda (x) (* x 2)))
      :do (format *error-output* "~3%~S~2%" *fill-indent-sexp*)
      (loop :for i :from 1 :to 12 :by 5 :do (warn "This form contains an error, a mistake, a bug, a blunder, a bungle, a blooper: ~S and can therefore not be correctly interpreted, neither today nor tomorrow nor next week nor next month nor next year" (make-list i))))))
#+clisp "


3

WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper: (NIL) and can
         therefore not be correctly interpreted, neither
         today nor tomorrow nor next week nor next month
         nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
   (NIL NIL NIL NIL NIL NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
   (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year


20

WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper: (NIL) and can
         therefore not be correctly interpreted, neither
         today nor tomorrow nor next week nor next month
         nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
                    (NIL NIL NIL NIL NIL NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
                    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
                     NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year


NIL

WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper: (NIL) and can
         therefore not be correctly interpreted, neither
         today nor tomorrow nor next week nor next month
         nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
\(NIL NIL NIL NIL NIL NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
\(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year


T

WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper: (NIL) and can
         therefore not be correctly interpreted, neither
         today nor tomorrow nor next week nor next month
         nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
         (NIL NIL NIL NIL NIL NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
         (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year


#<SYSTEM-FUNCTION 1+>

WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper: (NIL) and can
         therefore not be correctly interpreted, neither
         today nor tomorrow nor next week nor next month
         nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
          (NIL NIL NIL NIL NIL NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
          (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year


#<SYSTEM-FUNCTION 1->

WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper: (NIL) and can
         therefore not be correctly interpreted, neither
         today nor tomorrow nor next week nor next month
         nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
        (NIL NIL NIL NIL NIL NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
        (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year


#<FUNCTION :LAMBDA (X) (* X 2)>

WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper: (NIL) and can
         therefore not be correctly interpreted, neither
         today nor tomorrow nor next week nor next month
         nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
                  (NIL NIL NIL NIL NIL NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year
WARNING: This form contains an error, a mistake, a bug, a
         blunder, a bungle, a blooper:
                  (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
                   NIL)
         and can therefore not be correctly interpreted,
         neither today nor tomorrow nor next week nor next
         month nor next year
"


(let ((f "foo.bar") fwd1 fwd2)
  (unwind-protect
       (progn ; FILE-WRITE-DATE should work on :PROBE streams
         (with-open-file (s f :direction :output
                            #+(or CMU SBCL) :if-exists
                            #+(or CMU SBCL) :supersede)
           (write f :stream s)
           (setq fwd1 (file-write-date s)))
         (with-open-file (s f :direction :probe)
           (list (or (= fwd1 (setq fwd2 (file-write-date s)))
                     (list fwd1 fwd2))
                 ;; PROBE streams are closed!
                 (open-stream-p s))))
    (delete-file f)))
(T NIL)

#+clisp
(let ((f "foo.bar") fwd size dir decoded)
  (unwind-protect
       (progn (with-open-file (s f :direction :output)
                (write s :stream s)
                (setq fwd (file-write-date s)
                      size (file-length s)))
              (setq dir (first (directory f :full t))
                    decoded (subseq (multiple-value-list
                                     (decode-universal-time fwd))
                                    0 6))
              (list (or (equal (third dir) decoded)
                        (list dir fwd decoded))
                    (or (= (fourth dir) size)
                        (list dir size))))
    (delete-file f)))
#+clisp (T T)

;; http://sourceforge.net/tracker/index.php?func=detail&aid=1399709&group_id=1355&atid=101355
;; test :if-exists :append
(let ((f "foo.bar") pos)
  (unwind-protect
       (progn (with-open-file (s f :direction :output :if-exists :supersede
                                 #+clisp :external-format #+clisp :unix)
                (write-line "foo" s)
                (push (file-position s) pos))
              (with-open-file (s f :direction :output :if-exists :append
                                 #+clisp :external-format #+clisp :unix)
                (push (file-position s) pos)
                (write-line "bar" s)
                (push (file-position s) pos))
              (list (nreverse pos)
                    (with-open-file (s f :direction :input)
                      (list (read-line s) (read-line s)))))
    (delete-file f)))
((4 4 8) ("foo" "bar"))

(stringp (with-output-to-string (s)
           (describe (make-array nil) s)))
T

(stringp (with-output-to-string (s)
           (describe (make-array 1 :element-type nil) s)))
T

(stringp (with-output-to-string (s)
           (describe (make-array nil :element-type nil) s)))
T

(stringp (with-output-to-string (s) (describe nil s)))
T

(WITH-INPUT-FROM-STRING (*S* "abcde")
  (DECLARE (SPECIAL *S*))
  (LET ((SS (MAKE-SYNONYM-STREAM '*S*)))
    (ASSERT (TYPEP SS 'STREAM)) (ASSERT (TYPEP SS 'SYNONYM-STREAM))
    (ASSERT (INPUT-STREAM-P SS)) (ASSERT (NOT (OUTPUT-STREAM-P SS)))
    (ASSERT (OPEN-STREAM-P SS)) (ASSERT (STREAMP SS))
    (ASSERT (STREAM-ELEMENT-TYPE SS))
    (list (READ-CHAR *S*) (READ-CHAR SS) (READ-CHAR *S*) (READ-CHAR SS)
          (READ-CHAR SS))))
(#\a #\b #\c #\d #\e)

(WITH-OUTPUT-TO-STRING (*S*)
  (DECLARE (SPECIAL *S*))
  (LET ((SS (MAKE-SYNONYM-STREAM '*S*)))
    (ASSERT (TYPEP SS 'STREAM)) (ASSERT (TYPEP SS 'SYNONYM-STREAM))
    (ASSERT (OUTPUT-STREAM-P SS)) (ASSERT (NOT (INPUT-STREAM-P SS)))
    (ASSERT (OPEN-STREAM-P SS)) (ASSERT (STREAMP SS))
    (ASSERT (STREAM-ELEMENT-TYPE SS))
    (WRITE-CHAR #\a *S*) (WRITE-CHAR #\b SS)
    (WRITE-CHAR #\x *S*) (WRITE-CHAR #\y SS)))
"abxy"

;; <http://www.lisp.org/HyperSpec/Body/syscla_broadcast-stream.html>
(stream-external-format (make-broadcast-stream))   :default
(file-length (make-broadcast-stream))              0
(file-position (make-broadcast-stream))            0
(file-string-length (make-broadcast-stream) "foo") 1
(stream-element-type (make-broadcast-stream))      T

(let ((o (open "foo.bin" :direction :output #+(or CMU SBCL LISPWORKS) :if-exists #+(or CMU SBCL LISPWORKS) :supersede :element-type '(unsigned-byte 8)))
      (i (make-string-input-stream "foo")))
  (unwind-protect (stream-element-type (make-two-way-stream i o))
    (close o) (delete-file o)
    (close i)))
(OR CHARACTER (UNSIGNED-BYTE 8))

(stream-element-type (make-concatenated-stream
                      (make-string-input-stream "foo")
                      (make-string-input-stream "bar")
                      (make-string-input-stream "baz")
                      (make-string-input-stream "zot")))
#-(or CMU SBCL LISPWORKS) CHARACTER
#+(or CMU SBCL LISPWORKS) BASE-CHAR

(let ((s (make-string-output-stream :element-type nil)))
  (list (typep #\z (stream-element-type s))
        (typep #\a (array-element-type (get-output-stream-string s)))))
(NIL NIL)

;; [ 1412268 ] Buffer overflow in stream handling code
;; https://sourceforge.net/tracker/?func=detail&atid=101355&aid=1412268&group_id=1355
(open (make-string 5000 :initial-element #\x)) ERROR

;; composite streams operate on their constituent streams, not themselves
(let ((cs (make-concatenated-stream (make-string-input-stream "a"))))
  (unread-char (read-char cs) cs)
  (eql (peek-char nil cs)
       (peek-char nil (first (concatenated-stream-streams cs)) nil nil)))
T

(let ((2w (make-two-way-stream (make-string-input-stream "a")
                               *standard-output*)))
  (unread-char (read-char 2w) 2w)
  (eql (peek-char nil 2w)
       (peek-char nil (two-way-stream-input-stream 2w) nil nil)))
T

(let* ((so (make-string-output-stream))
       (ve (make-array 1))
       (ec (make-echo-stream (make-string-input-stream "a") so)))
  (unread-char (read-char ec) ec)
  (unread-char (read-char ec) ec)
  (unread-char (read-char ec) ec)
  (read-sequence ve ec)
  (unread-char (aref ve 0) ec)
  (list (eql (peek-char nil ec)
             (peek-char nil (echo-stream-input-stream ec) nil nil))
        (get-output-stream-string so)))
(T "a")

(let* ((so1 (make-string-output-stream))
       (so2 (make-string-output-stream))
       (ec1 (make-echo-stream (make-string-input-stream "a") so1))
       (ec2 (make-echo-stream ec1 so2)))
  (unread-char (read-char ec2) ec2)
  (unread-char (read-char ec2) ec2)
  (unread-char (read-char ec2) ec2)
  (list (eql (peek-char nil ec2)
             (peek-char nil ec1 nil nil))
        (get-output-stream-string so1)
        (get-output-stream-string so2)))
(T "a" "a")

(clear-input *debug-io*) nil
(clear-input *query-io*) nil
(clear-input *standard-input*) nil
(clear-input *terminal-io*) nil
(finish-output *debug-io*) nil
(finish-output *query-io*) nil
(finish-output *standard-output*) nil
(finish-output *terminal-io*) nil
(force-output *debug-io*) nil
(force-output *query-io*) nil
(force-output *standard-output*) nil
(force-output *terminal-io*) nil
(clear-output *debug-io*) nil
(clear-output *query-io*) nil
(clear-output *standard-output*) nil
(clear-output *terminal-io*) nil

(progn
(makunbound 's)
(makunbound 's1)
(makunbound 's2)
(makunbound 's3)
(makunbound 's4)
(makunbound 's5)
(makunbound 's6)
(makunbound 's7)
(makunbound 's8)
(makunbound 's9)
(makunbound 's10)
(makunbound 'b1)
(makunbound 'b2)
(makunbound 'c1)
(makunbound 'c2)
(makunbound 'c3)
(makunbound 'c4)
(makunbound 'inptw)
(makunbound 'sy)
(makunbound 'tw)
(makunbound 'ec)
(makunbound 'str1)
(makunbound 'strgstream)
(makunbound 'os)
(makunbound 'os1)
(makunbound 'is)
(makunbound 'es)
(makunbound 's50)
(makunbound 's49)
(setq *print-length* nil)
t)
T

