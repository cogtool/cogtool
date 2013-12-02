;; -*- Lisp -*-

(loop for x from 1 to 9
      for y = nil then x
      collect (list x y))
((1 NIL) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9))

(loop for x from 1 to 9
      and y = nil then x
      collect (list x y))
((1 NIL) (2 1) (3 2) (4 3) (5 4) (6 5) (7 6) (8 7) (9 8))

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-2-1-1-1.html>
(with-output-to-string (*standard-output*)
  (loop as i from 1 to 5
        do (print i)))
"
1 
2 
3 
4 
5 "

(with-output-to-string (*standard-output*)
  (loop for i from 10 downto 1 by 3
        do (print i)))
"
10 
7 
4 
1 "

(with-output-to-string (*standard-output*)
  (loop as i below 5
        do (print i)))
"
0 
1 
2 
3 
4 "

(with-output-to-string (*standard-output*)
  (loop for item in '(1 2 3 4 5)
        do (print item)))
"
1 
2 
3 
4 
5 "

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-2-1-2-1.html>
(with-output-to-string (*standard-output*)
  (loop for item in '(1 2 3 4 5) by #'cddr
        do (print item)))
"
1 
3 
5 "

(loop for (item . x) (t . fixnum) in '((A . 1) (B . 2) (C . 3))
      unless (eq item 'B) sum x)
4

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-2-1-3-1.html>
(loop for sublist on '(a b c d)
      collect sublist)
((A B C D) (B C D) (C D) (D))

(with-output-to-string (*standard-output*)
  (loop for (item) on '(1 2 3)
        do (print item)))
"
1 
2 
3 "

(with-output-to-string (*standard-output*)
  (loop for item in '(1 2 3)
        do (print item)))
"
1 
2 
3 "

;; ON ends on ATOM, not ENDP
(loop for x on '(1 2 . 3) collect x)
((1 2 . 3) (2 . 3))

(loop for i below 5
      for j = 10 then i
      collect j)
(10 1 2 3 4)

(loop for i below 5
      for j = i
      collect j)
(0 1 2 3 4)

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-2-1-4-1.html>
(loop for item = 1 then (+ item 10)
      repeat 5
      collect item)
(1 11 21 31 41)

(loop for char across (the simple-string "Hello")
      collect char)
(#\H #\e #\l #\l #\o)

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-4-1.html>
(with-output-to-string (*standard-output*)
  (loop repeat 3
        do (write-line "What I say three times is true")))
"What I say three times is true
What I say three times is true
What I say three times is true
"

(with-output-to-string (*standard-output*)
  (loop repeat -15
        do (write-line "What you see is what you expect")))
""

#| ;; FOR clauses should come before WHILE clauses
 (let ((stack '(a b c d e f)))
  (loop while stack
        for item = (length stack) then (pop stack)
        collect item))
 (6 A B C D E F)
|#

(loop for i fixnum from 3
      when (oddp i) collect i
      while (< i 5))
(3 5)

(loop for i from 0 to 10
      always (< i 11))
T

(loop for i from 0 to 10
      never (> i 11))
T

(loop for i from 0
      thereis (when (> i 10) i))
11

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-4-2.html>
(with-output-to-string (*standard-output*)
  (loop for i from 0 to 10
        always (< i 9)
        finally (print "You won't see this")))
""

(with-output-to-string (*standard-output*)
  (loop never t
        finally (print "You won't see this")))
""

(with-output-to-string (*standard-output*)
  (loop thereis "Here is my value"
        finally (print "You won't see this")))
""

(loop thereis "Here is my value"
      finally (print "You won't see this"))
"Here is my value"

(with-output-to-string (*standard-output*)
  (loop for i from 1 to 10
        thereis (> i 11)
        finally (print i)))
"
11 "

(let (everest chocorua sahara)
  (defstruct mountain  height difficulty (why "because it is there"))
  (setq everest (make-mountain :height '(2.86e-13 parsecs)))
  (setq chocorua (make-mountain :height '(1059180001 microns)))
  (defstruct desert  area (humidity 0))
  (setq sahara (make-desert :area '(212480000 square furlongs)))
  (loop for x in (list everest sahara chocorua)
        thereis (and (mountain-p x) (mountain-height x))))
(2.86e-13 parsecs)

(with-output-to-string (*standard-output*)
  (loop for (month date-list) in '((january (24 28)) (february (17 29 12)))
        do (loop for date in date-list
                 do (case date
                      (29 (when (eq month 'february) (loop-finish))))
                 do (format t "~:(~A~) ~A~%" month date))))
"January 24
January 28
February 17
"

(loop for i in '(1 2 3 stop-here 4 5 6)
      when (symbolp i) do (loop-finish)
      count i)
3

(loop for i in '(1 2 3 stop-here 4 5 6)
      until (symbolp i)
      count i)
3

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-3.html>
(loop for name in '(fred sue alice joe june)
      for kids in '((bob ken) () () (kris sunshine) ())
      collect name
      append kids)
(FRED BOB KEN SUE ALICE JOE KRIS SUNSHINE JUNE)

(multiple-value-list
  (loop for name in '(fred sue alice joe june)
        as age in '(22 26 19 20 10)
        append (list name age) into name-and-age-list
        count name into name-count
        sum age into total-age
        finally
          (return (values (round total-age name-count) name-and-age-list))))
(19 (FRED 22 SUE 26 ALICE 19 JOE 20 JUNE 10))

(LOOP FOR X FROM 1 TO 20 WHEN (EQL (MOD X 5) 0) COLLECT X INTO FOO
  WHEN (EQL (MOD X 5) 2) COLLECT X INTO FOO FINALLY (RETURN FOO))
(2 5 7 10 12 15 17 20)

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-3-1.html>
(loop for i in '(bird 3 4 turtle (1 . 4) horse cat)
      when (symbolp i) collect i)
(BIRD TURTLE HORSE CAT)

(loop for i from 1 to 10 if (oddp i) collect i)
(1 3 5 7 9)

(with-output-to-string (*standard-output*)
  (loop for i in '(a b c d) by #'cddr
        collect i into my-list
        finally (print my-list)))
"
(A C) "

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-3-2.html>
(loop for x in '((a) (b) ((c)))
      append x)
(A B (C))

(loop for i upfrom 0
      as x in '(a b (c))
      nconc (if (evenp i) (list x) '()))
(A (C))

;; GCL ansi-test (Paul F. Dietz <dietz@dls.net>)
(LOOP FOR X IN '((A) (B) (C . WHATEVER)) APPENDING X)
(A B C . WHATEVER)

(LOOP FOR X IN '((A) (B) (C . WHATEVER)) NCONC X)
(A B C . WHATEVER)

(LOOP FOR X IN '(A B C D E) NCONC (CONS X 'FOO))
(A B C D E . FOO)

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-3-3.html>
(loop for i in '(a b nil c nil d e)
      count i)
5

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-3-5.html>
(loop for i fixnum in '(1 2 3 4 5)
      sum i)
15

(let ((series '(1.2 4.3 5.7)))
  (loop for v in series
        sum (* 2.0 v)))
22.4

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-3-4.html>
(loop for i in '(2 1 5 3 4)
      maximize i)
5

(loop for i in '(2 1 5 3 4)
      minimize i)
1

(let ((series '(1.2 4.3 5.7)))
  (loop for v in series
        maximize (round v) fixnum))
6

(let ((series '(1.2 4.3 5.7)))
  (loop for v in series
        minimize (round v) into result fixnum
        finally (return result)))
1

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-2-2.html>
;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-2-2-1.html>
(loop with a = 1
      with b = (+ a 2)
      with c = (+ b 3)
      with d = (+ c 4)
      return (list a b c d))
(1 3 6 10)

(loop with a = 1
       and b = 2
       and c = 3
       and d = 4
      return (list a b c d))
(1 2 3 4)

(let ((a 5) (b 10) (c 1729))
  (loop with a = 1
         and b = (+ a 2)
         and c = (+ b 3)
         and d = (+ c 4)
        return (list a b c d)))
(1 7 13 1733)

(loop with (a b c) (float integer float)
      return (format nil "~A ~A ~A" a b c))
"0.0 0 0.0"

(loop with (a b c) float
      return (format nil "~A ~A ~A" a b c))
"0.0 0.0 0.0"

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-8-1.html>
(let ((numbers-list '(3 2 4 6 1 7 8)) (results nil))
  (cons
    (with-output-to-string (*standard-output*)
      (loop for i in numbers-list
            when (oddp i)
              do (print i)
              and collect i into odd-numbers
              and do (terpri)
              else
              collect i into even-numbers
            finally (setq results (list odd-numbers even-numbers))))
    results))
("
3 

1 

7 
"
(3 1 7) (2 4 6 8))

(loop for i in '(1 2 3 4 5 6)
      when (and (> i 3) i)
        collect it)
(4 5 6)

(loop for i in '(1 2 3 4 5 6)
      when (and (> i 3) i)
        return it)
4

;; Find a number in a list.
(loop for i in '(1 2 3 4 5 6)
  when (and (> i 3) i)
  return it)
4

;; The above example is similar to the following one.
(loop for i in '(1 2 3 4 5 6)
  thereis (and (> i 3) i))
4

;; Nest conditional clauses.
(let ((list '(0 3.0 apple 4 5 9.8 orange banana)))
  (loop for i in list
    when (numberp i)
     when (floatp i)
      collect i into float-numbers
     else                       ; Not (floatp i)
      collect i into other-numbers
    else                        ; Not (numberp i)
     when (symbolp i)
      collect i into symbol-list
     else                       ; Not (symbolp i)
      do (error "found a funny value in list ~S, value ~S~%" list i)
    finally (return (list float-numbers other-numbers symbol-list))))
((3.0 9.8) (0 4 5) (APPLE ORANGE BANANA))

(LET ((IT 'Z)) (LOOP FOR X IN '(A NIL B C D) IF X COLLECT IT END COLLECT IT))
(A Z Z B Z C Z D Z)

(LET ((IT 'Z)) (LOOP FOR X IN '(A NIL B C D) WHEN X COLLECT IT AND COLLECT IT))
(A Z B Z C Z D Z)


(loop for i in '(1 2 3 4 5 6)
      thereis (and (> i 3) i))
4

;; Without the END preposition, the last AND would apply to the
;; inner IF rather than the outer one.
(with-output-to-string (*standard-output*)
  (loop for x from 0 to 3
        do (print x)
        if (zerop (mod x 2))
          do (write-string " a")
          and
          if (zerop (floor x 2))
            do (write-string " b")
            and
            do (write-string " c")))
"
0  a b c
1 
2  a
3 "

(with-output-to-string (*standard-output*)
  (loop for x from 0 to 3
        do (print x)
        if (zerop (mod x 2))
          do (write-string " a")
          and
          if (zerop (floor x 2))
            do (write-string " b")
            end
          and
          do (write-string " c")))
"
0  a b c
1 
2  a c
3 "

(with-output-to-string (*standard-output*)
  (loop for i from 1 to 5
        do (print i)))
"
1 
2 
3 
4 
5 "

(with-output-to-string (*standard-output*)
  (loop for i from 1 to 4
        do (print i)
           (print (* i i))))
"
1 
1 
2 
4 
3 
9 
4 
16 "

(loop for item in '(1 2 3 a 4 5)
      when (not (numberp item))
        return (format nil "non-numeric value: ~S" item))
"non-numeric value: A"

(loop for item in '(1 2 3 a 4 5)
      when (not (numberp item))
        do (return (format nil "non-numeric value: ~S" item)))
"non-numeric value: A"

(loop for numlist in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
      for a integer = (first numlist)
      for b integer = (second numlist)
      for c float = (third numlist)
      collect (list c b a))
((4.0 2 1) (8.3 6 5) (10.4 9 8))

;; According to the BNF syntax, "and" must not be followed by "for". But
;; ANSI CL section 6.1.1.5.1 contains ambiguous wording, and this example
;; appears in CLtL2 p. 743, we keep it.
(loop for numlist in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
      for a integer = (first numlist)
      and for b integer = (second numlist)
      and for c float = (third numlist)
      collect (list c b a))
#-(or CMU SBCL) ((4.0 2 1) (8.3 6 5) (10.4 9 8))
#+(or CMU SBCL) ERROR

(loop for numlist in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
      for a integer = (first numlist)
      and b integer = (second numlist)
      and c float = (third numlist)
      collect (list c b a))
((4.0 2 1) (8.3 6 5) (10.4 9 8))

(loop for (a b c) (integer integer float) in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
      collect (list c b a))
((4.0 2 1) (8.3 6 5) (10.4 9 8))

(loop for (a b c) float in '((1.0 2.0 4.0) (5.0 6.0 8.3) (8.0 9.0 10.4))
      collect (list c b a))
((4.0 2.0 1.0) (8.3 6.0 5.0) (10.4 9.0 8.0))

(loop with (a b) float = '(1.0 2.0)
      and (c d) integer = '(3 4)
      and (e f)
      return (list a b c d e f))
(1.0 2.0 3 4 NIL NIL)

(loop for (a nil b) = '(1 2 3)
      do (return (list a b)))
(1 3)

(loop for (x . y) = '(1 . 2)
      do (return y))
2

(loop for ((a . b) (c . d)) of-type ((float . float) (integer . integer))
                            in '(((1.2 . 2.4) (3 . 4)) ((3.4 . 4.6) (5 . 6)))
      collect (list a b c d))
((1.2 2.4 3 4) (3.4 4.6 5 6))

(loop for buffer in '("\"Hello\"" "\"unterminated" "nothing")
      collect
        (loop initially (unless (char= (char buffer 0) #\") (loop-finish))
              for i fixnum from 1 below (length buffer)
              when (char= (char buffer i) #\")
                return i))
(6 NIL NIL)

(let (result)
  (list
    (with-output-to-string (*standard-output*)
      (setq result
        (loop for i from 1 to 10
              when (> i 5)
                collect i
              finally (print i))))
    result))
("
11 " (6 7 8 9 10))

(multiple-value-list
  (loop for i from 1 to 10
        when (> i 5)
          collect i into number-list
          and count i into number-count
        finally (return (values number-count number-list))))
(5 (6 7 8 9 10))

(LET (Z)
  (list (LOOP FOR X FROM 1 TO 10 COUNT (< X 5) INTO FOO FLOAT
          FINALLY (SETQ Z FOO))
        Z))
(NIL 4.0)

(let (result)
  (list
    (with-output-to-string (*standard-output*)
      (setq result
        (loop named max
              for i from 1 to 10
              do (print i)
              do (return-from max 'done))))
    result))
("
1 " DONE)

;;; The following tests are not mandatory according to dpANS or ANSI CL,
;;; but that's how users expect the LOOP macro to work, so we check them.

(loop for i = 0
      for j to 2
      collect j)
(0 1 2)

(loop for i in '(1 2)
      for j = i
      for k = j
      collect (list i j k))
((1 1 1) (2 2 2))

(loop for idx upfrom 0 below 5
      for char = (aref "Error" idx)
      collect char)
(#\E #\r #\r #\o #\r)

(let ((hash-table (make-hash-table)))
  (setf (gethash 1 hash-table) 100)
  (setf (gethash 2 hash-table) 200)
  (sort
    (loop for key being each hash-key in hash-table using (hash-value val)
          for key+1 = (1+ key)
          collect (list key key+1 val))
    #'<
    :key #'car))
((1 2 100) (2 3 200))

(loop for i across '#(1 2 3 4)
      for j = (1+ i)
      collect (list i j))
((1 2) (2 3) (3 4) (4 5))

(loop for i in '()
      for j = (1+ i)
      collect j)
nil

(loop for i across '#()
      for j = (1+ i)
      collect j)
nil

(loop for x = t
      for y in '(A B C)
      for z = t
      collect y)
(A B C)

(loop for x = t
      for y across '#(A B C)
      for z = t
      collect y)
(A B C)

(loop for x = t
      for y in ()
      for z = t
      collect y)
nil

(loop for x = t
      for y across '#()
      for z = t
      collect y)
nil

(let ((hash-table (make-hash-table)))
  (setf (gethash 1 hash-table) 100)
  (setf (gethash 2 hash-table) 200)
  (sort
    (loop for x = t
          for key being each hash-key in hash-table using (hash-value val)
          for key+1 = (1+ key)
          for z = t
          collect (list key key+1 val))
    #'<
    :key #'car))
((1 2 100) (2 3 200))

(loop for i from 1 to 0
      collect i)
nil

(let ((hash-table (make-hash-table)))
  (setf (gethash 1 hash-table) 100)
  (setf (gethash 2 hash-table) 200)
  (sort
    (loop for val being each hash-value in hash-table
          collect val)
    #'<))
(100 200)

(let ((hash-table (make-hash-table)))
  (setf (gethash 1 hash-table) 100)
  (setf (gethash 2 hash-table) 200)
  (sort
    (loop for val being each hash-value in hash-table
          for deriv-val = (/ 1 val)
          collect deriv-val)
    #'<))
(1/200 1/100)

(let ((hash-table (make-hash-table)))
  (setq i 123456789)
  (setf (gethash 1 hash-table) 100)
  (setf (gethash 2 hash-table) 200)
  (loop for i across '#(1 2 3 4 5 6)
        collect i)
  (loop for i in '(1 2 3 4 5 6)
        collect i)
  (loop for i being each hash-key of hash-table
        collect i)
  (loop for i being each present-symbol of *package*
        collect i)
  i)
123456789

(loop for x on '(3 4 5)
      for y = (car x)
      for z in '(a b c)
      collect z)
(a b c)

(loop for x across '#(3 4 5)
      for y = (1+ x)
      for z across '#(a b c)
      collect (list x y z))
((3 4 a) (4 5 b) (5 6 c))

(loop for x across '#()
      for y = x
      for z across '#(a b c)
      collect (list x y z))
nil

(loop for x across '#(1 2 3)
      for y = x
      for z across '#()
      collect (list x y z))
nil

(loop for x across '#(1 2 3)
      for y = (1+ x)
      for z across '#(a b)
      collect (list x y z))
((1 2 a) (2 3 b))

(loop for x across '#(1 2)
      for y = (1+ x)
      for z across '#(a b c)
      collect (list x y z))
((1 2 a) (2 3 b))

(let ((package (make-package "LOOP-TEST")))
  (intern "blah" package)
  (let ((blah2 (intern "blah2" package)))
    (export blah2 package))
  (list
    (sort
      (loop for sym being each present-symbol of package
            for sym-name = (symbol-name sym)
            collect sym-name)
      #'string<)
    (sort
      (loop for sym being each external-symbol of package
            for sym-name = (symbol-name sym)
            collect sym-name)
      #'string<)))
(("blah" "blah2") ("blah2"))

(let ((ht (make-hash-table)))
  (loop for key being each hash-key of ht
        for value = (gethash key ht)
        collect (list key value)))
nil

(let ((ht (make-hash-table)))
  (loop for dummy = (+ 1 2)
        for key being each hash-key of ht
        collect (list key)))
nil

;;; Three more tests, found by Russell Senior.
;;; They are justified by ANSI CL 6.1.1.4 and 6.1.2.1.5.

(let ((list '(1 2 3)))
  (loop for x in list
        and y = nil then x
        collect (list x y)))
((1 NIL) (2 1) (3 2))

(let ((list '(1 2 3)))
  (loop for x in list
        for y = nil then x
        collect (list x y)))
((1 NIL) (2 2) (3 3))

(let ((list '(1 2 3)))
  (loop for x in list
        for y = nil then x
        and z = nil then y
        collect (list x y z)))
((1 NIL NIL) (2 2 NIL) (3 3 2))

;;; One more test, found by Lennart Staflin.

(loop repeat 4 for x = (+ 1 1) collect x)
(2 2 2 2)

;;; Tests from ANSI CL section 6.1.2.1.1.
;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-2-1-1.html>
(let ((x 1)) (loop for i from x by (incf x) to 10 collect i))
(1 3 5 7 9)

(let ((x 1)) (loop for i by (incf x) from x to 10 collect i))
(2 4 6 8 10)

(loop for i from 1 to 5 collect i into c collect (copy-list c))
((1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5))

(let ((rem 55)) (loop for i below 3 with num = (* 10 rem) and rem collect rem))
(nil nil nil)

;; Clean up.
(progn (delete-package "LOOP-TEST") t)
T

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-2-1-7-1.html>
;; package defaults to *package*
(unwind-protect
     (let ((*package* (make-package "LOOP-TEST-PACKAGE-1")))
       ;; For effect, intern some symbols
       (read-from-string "(THIS IS A TEST)")
       (export (intern "THIS"))
       (set-exclusive-or
        '("THIS" "IS" "A" "TEST")
        (loop for x being each present-symbol collect x)
        :test #'string=))
  (delete-package "LOOP-TEST-PACKAGE-1"))
nil

;; <https://sourceforge.net/tracker/?func=detail&atid=101355&aid=618428&group_id=1355>
(let ((alist '(1 2 3 4 5 6)))
  (loop for value in alist
    if (oddp value) collect value into alist
    else collect value into blist
    finally (return (list alist blist))))
((1 3 5) (2 4 6))

;; reported by "Thomas F. Burdick" <tfb@OCF.Berkeley.EDU>
;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-7-2.html>
;; According to the HyperSpec 6.1.2.1.4, in for-as-equals-then, var is
;; initialized to the result of evaluating form1.  6.1.7.2 says that
;; initially clauses are evaluated in the loop prologue, which precedes all
;; loop code except for the initial settings provided by with, for, or as.
(loop :for x = 0 :then (1+ x) :for y = (1+ x) :then (ash y 1)
  :for z :across #(1 3 9 27 81 243) :for w = (+ x y z)
  :initially (assert (zerop x)) :initially (assert (= 2 w))
  :until (>= w 100) :collect w)
(2 6 15 38)

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-8.html>
(let ((i 0))                    ; no loop keywords are used
  (loop (incf i) (if (= i 3) (return i))))
3

(let ((i 0)(j 0))
  (tagbody
    (loop (incf j 3) (incf i) (if (= i 3) (go exit)))
   exit)
  j)
9

(loop for x from 1 to 10
  for y = nil then x
  collect (list x y))
((1 NIL) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9) (10 10))

(loop for x from 1 to 10
  and y = nil then x
  collect (list x y))
((1 NIL) (2 1) (3 2) (4 3) (5 4) (6 5) (7 6) (8 7) (9 8) (10 9))

;; not really required, but many people want to access
;; iteration variables in the finally clauses
(loop for x in '(1 2 3) for y in '(4 5 6) sum (* x y) into z
  finally (return (+ x y z)))
41

(cdr (multiple-value-list
      (compile nil
               (lambda (x)
                 (loop :with (a . b) = x :repeat 10
                   :finally (pprint (list a b)))))))
(NIL NIL)

(LET ((I 0)) (LOOP FOR NIL FROM 10 BELOW 15 COLLECT (INCF I)))
(1 2 3 4 5)

(LOOP FOR NIL ON NIL DO (RETURN T))
NIL

;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-3.html>
(handler-case
    (eval (macroexpand '(loop :for i :from 1 :to 20 :sum i :maximize i)))
  (program-error (c) (princ c) (values '(correct program-error))))
(correct program-error)

(handler-case
    (eval (macroexpand '(loop :for i :from 1 :to 20 :sum i :always (evenp i))))
  (program-error (c) (princ c) (values '(correct program-error))))
(correct program-error)

;; local variables:
;; eval: (make-local-variable 'write-file-functions)
;; eval: (remove-hook 'write-file-functions 'delete-trailing-whitespace t)
;; end:
