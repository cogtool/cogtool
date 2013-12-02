;; -*- Lisp -*-
;; restarts
(defmacro check-use-value (fun good bad &key (type 'type-error) (test 'eql))
  `(handler-bind ((,type (lambda (c) (princ-error c) (use-value ',good))))
     (,test (,fun ',good) (,fun ',bad))))
check-use-value

(check-use-value char-code #\1 12 :test =) t
(check-use-value symbol-name good "bad" :test string=) t
(check-use-value intern "BAR" bar :test eq) t
(check-use-value fboundp cons "CONS") t
(check-use-value fdefinition cons "CONS") t
(check-use-value string "123" 123) t

(check-use-value (lambda (a) (aref a 2)) #(a b c d) 1) t
(check-use-value (lambda (a) (setf (aref a 2) 'x)) #(a b c d) 1) t
(check-use-value (lambda (a) (row-major-aref a 3)) #2A((a b) (c d)) 1) t
(check-use-value (lambda (a) (setf (row-major-aref a 3) 'x)) #2A((a b) (c d)) 1) t
(check-use-value array-element-type #*1001 1) t
(check-use-value array-rank #(a b c d) 1) t
(check-use-value (lambda (a) (array-dimension a 1)) #2A((a b) (c d)) 1) t
(check-use-value array-dimensions #2A((a b) (c d)) 1 :test equal) t
(check-use-value (lambda (a) (array-in-bounds-p a 1 2)) #2A((a b) (c d)) 1) t
(check-use-value (lambda (a) (array-row-major-index a 2)) #(a b c d) 1) t
(check-use-value (lambda (a) (array-row-major-index a 1 1)) #2A((a b) (c d)) 1) t
(check-use-value adjustable-array-p #2A((a b) (c d)) 1) t
(check-use-value (lambda (a) (bit a 2)) #*1011 1) t
(check-use-value (lambda (a) (sbit a 2)) #*1011 1) t
(check-use-value array-has-fill-pointer-p #2A((a b) (c d)) 1) t

(let ((bs (make-broadcast-stream)))
  (handler-bind ((type-error (lambda (c) (princ-error c) (use-value bs))))
    (broadcast-stream-streams 10)))
NIL

(handler-bind ((error (lambda (c) (princ-error c) (use-value #\#))))
  (eq (get-dispatch-macro-character #\a #\()
      (get-dispatch-macro-character #\# #\()))
T

(with-output-to-string (o)
  (handler-bind ((type-error (lambda (c) (princ-error c) (use-value o))))
    (princ "no error!" 123)))
"no error!"

(handler-bind ((type-error (lambda (c) (princ-error c) (use-value 16))))
  (parse-integer "ABC" :radix 'lambda))
2748

(with-input-from-string (s "bazonk")
  (handler-bind ((type-error (lambda (c) (princ-error c) (use-value s))))
    (list (read-char 123) (read-char 1) (read-char 'read-char))))
(#\b #\a #\z)

(handler-bind
    ((type-error
      (lambda (c)
        (princ-error c)
        (use-value
         (case (type-error-datum c)
           (1 *readtable*)
           (2 :upcase)
           (t (error "huh?")))))))
  (setf (readtable-case 1) 2))
:UPCASE

(handler-bind
    ((type-error
      (lambda (c)
        (princ-error c)
        (use-value
         (case (type-error-datum c)
           (1 #\#)
           (2 *readtable*)
           (t (error "huh?")))))))
  (nth-value 1 (get-macro-character 1 2)))
T

(handler-bind ((type-error (lambda (c) (princ-error c) (use-value 7))))
  (list (digit-char-p #\3 300)
        (digit-char-p #\8 'digit-char-p)))
(3 NIL)

(handler-bind ((type-error
                (lambda (c)
                  (princ-error c)
                  (use-value (char (type-error-datum c) 0)))))
  (list (char= "abc" "a")
        (char-equal "ABC" "a")))
(T T)

(handler-bind ((type-error
                (lambda (c)
                  (princ-error c)
                  (use-value (string (type-error-datum c))))))
  (ext:string-concat "foo-" 'bar "-baz"))
"foo-BAR-baz"

(handler-bind ((undefined-function
                (lambda (c) (princ-error c)
                        (store-value
                         (lambda (new-car pair)
                           (setf (car pair) new-car))))))
  (let ((a '(1 . 2)))
    (setf (zz a) 12)
    a))
(12 . 2)
(fmakunbound '(setf zz)) (setf zz)

(handler-bind ((undefined-function
                (lambda (c) (princ-error c) (store-value #'car))))
  (zz '(1 . 2)))
1
(fmakunbound 'zz) zz

(defun use-value-read (c)
  (princ-error c)
  (use-value (read-from-string
              (etypecase c
                (sys::source-program-error (sys::source-program-error-form c))
                (type-error (type-error-datum c))
                (cell-error (cell-error-name c))))))
use-value-read

(let ((table (copy-readtable nil)))
  (and (eq :upcase (readtable-case table))
       (setf (readtable-case table) :invert)
       (let ((copy (copy-readtable table)))
         (and (not (eq table copy))
              (eq (readtable-case copy) :invert)))))
T

(handler-bind ((type-error #'use-value-read))
  (setf (readtable-case (copy-readtable nil)) ":UPCASE"))
:UPCASE

(handler-bind ((error (lambda (c) (princ-error c) (use-value '+))))
  (eval '(function "+")))
#.#'+

(handler-bind ((error #'use-value-read))
  (funcall "+" 1 2 3))
6

;; progv
(handler-bind ((type-error #'use-value-read))
  (progv '("foo") '(123) foo))
123

(handler-bind ((program-error (lambda (c) (princ-error c) (use-value 'zz))))
  (progv '(:const-var) '(123) zz))
123

(let ((form '(progv '("foo" :const) '(123 321) (+ foo zz))))
  (handler-bind ((type-error #'use-value-read)
                 (program-error (lambda (c) (princ-error c) (use-value 'zz))))
    (list (eval form) form)))
(444 (progv '("foo" :const) '(123 321) (+ foo zz)))

(handler-bind ((type-error #'use-value-read))
  (multiple-value-setq (a "foo") (values 123 321))
  (list foo a))
(321 123)

(handler-bind ((program-error (lambda (c) (princ-error c) (use-value 'zz))))
  (setq :const-var 125)
  zz)
125

(handler-bind ((program-error
                (lambda (c) (princ-error c) (use-value '(zz 48)))))
  (let (("foo" 32)) zz))
48

;; This test reflects only the current CLISP behaviour:
;; - It can be argued that zz should be bound statically (since zz
;;   is not declared special) or should be bound dynamically (since :const-var
;;   would be bound dynamically and zz replaces just the symbol).
;; - It can be argued that later zz should be evaluated statically (because
;;   that's what normal EVAL in the interpreter would do) or should be
;;   evaluated to lookup (symbol-value 'zz) - since that's what the compiler
;;   would make from the code.
(handler-bind ((program-error (lambda (c) (princ-error c) (use-value 'zz))))
  (let ((:const-var 64)) zz))
64

;; either TYPE-ERROR or SOURCE-PROGRAM-ERROR is reasonable here
;; (handler-bind ((source-program-error #'use-value-read)
;;                (type-error #'use-value-read))
;;   ((lambda (x "y") (+ x y)) 1 3))
;; 4

;; (handler-bind ((source-program-error #'use-value-read)
;;                (type-error #'use-value-read))
;;   ((lambda (x &optional ("y" 10)) (+ x y)) 1 3))
;; 4

;; (handler-bind ((source-program-error #'use-value-read)
;;                (type-error #'use-value-read))
;;   ((lambda (x &key ("y" 10)) (+ x y)) 1 :y 3))
;; 4

;; (handler-bind ((source-program-error #'use-value-read)
;;                (type-error #'use-value-read))
;;   ((lambda (x &aux ("y" 10)) (+ x y)) 1))
;; 11

;; (handler-bind ((source-program-error #'use-value-read)
;;                (type-error #'use-value-read))
;;   (let ((f (lambda ("a" &optional "b" ("c" 1) &rest "d"
;;                     &key "e" ("f" 2) ("g" 3 "gp") (("hk" "ha") 4 "hp")
;;                     ("i" 5 "ip")
;;                     &aux ("j" 6))
;;              (list a b c '&rest d 'e e 'f f 'g g gp 'h ha hp 'i i ip 'j j))))
;;     (print f)
;;     (funcall f 11 22 33 :e 44 :g 55 'hk 66)))
;; (11 22 33 &REST (:E 44 :G 55 HK 66) E 44 F 2 G 55 T H 66 T I 5 NIL J 6)

(handler-bind ((type-error #'use-value-read)
               (source-program-error #'use-value-read))
  (funcall "CAR" '(1 . 1)))
1

(handler-bind ((type-error #'use-value-read)
               (source-program-error #'use-value-read))
  (setq "FOO" 1)
  (symbol-value 'foo))
1

;; make-hash-table
(flet ((mht (test) (make-hash-table :test test)))
  (check-use-value mht eql bazonk :test equalp)) t
(flet ((mht (w) (make-hash-table :weak w)))
  (check-use-value mht nil bazonk :test equalp)) t
(flet ((mht (s) (make-hash-table :size s)))
  (check-use-value mht 10 bazonk :test equalp)) t
(flet ((mht (rs) (make-hash-table :rehash-size rs)))
  (check-use-value mht 2d0 bazonk :test equalp)) t
(flet ((mht (tr) (make-hash-table :rehash-threshold tr)))
  (check-use-value mht 5d-1 bazonk :test equalp)) t

(handler-bind ((program-error (lambda (c) (princ-error c) (use-value '1+)))
               (type-error (lambda (c) (princ-error c) (use-value '1-))))
  (list (eval '(1 10)) (funcall 1 100) (apply 1 '(1000))))
(11 99 999)

(progn (makunbound 'bar)
(handler-bind ((unbound-variable
                (lambda (c) (princ-error c) (store-value 41))))
  (1+ bar)))
42

bar 41

(progn
 (defclass zot () (zot-foo))
 (setq bar (make-instance 'zot))
 (handler-bind ((unbound-slot
                 (lambda (c) (princ-error c) (store-value 41))))
   (1+ (slot-value bar 'zot-foo))))
42

(slot-value bar 'zot-foo) 41

(progn
  (define-condition xyzzy ()
    ((f1 :accessor my-f1 :initarg :f1-is))
    (:report (lambda (c s)
               (format s "~1Txyzzy: My f1 is ~A" (my-f1 c)))))
  (princ-to-string (make-condition 'xyzzy :f1-is "a silly string")))
" xyzzy: My f1 is a silly string"

;; check all invocations of correctable-error in package.d
(let* ((p1 (make-package "PACK-1" :use nil))
       (p2 (make-package "PACK-2" :use nil))
       (p3 (make-package "PACK-3" :use nil))
       (p4 (make-package "PACK-4" :use nil))
       (p5 (make-package "PACK-5" :use nil))
       (bar-name (symbol-name (gensym "BAR-")))
       (foo1 (intern "FOO" p1)) (foo2 (intern "FOO" p2))
       (bar1 (intern bar-name p1)) (bar2 (intern bar-name p2))
       (bar3 (intern bar-name p3)) (bar4 (intern bar-name p4))
       (s12 (intern "SYM-1" p2)) (s22 (intern "SYM-2" p2))
       (s13 (intern "SYM-1" p3)) (s23 (intern "SYM-2" p3))
       (s14 (intern "SYM-1" p4)) (s24 (intern "SYM-2" p4))
       (s15 (intern "SYM-1" p5)) (s25 (intern "SYM-2" p5)))
  (export (list s12 s22) p2)
  (export (list s13 s23) p3)
  (export (list s14 s24) p4)
  (handler-bind ((package-error
                  (lambda (c) (princ-error c) (invoke-restart :pack-3))))
    (use-package (list p2 p3 p4) p1))
  (assert (null (set-exclusive-or (list p2 p3 p4) (package-use-list p1))))
  (assert (eq (find-symbol "SYM-1" p1) s13))
  (assert (eq (find-symbol "SYM-2" p1) s23))
  (handler-bind ((package-error
                  (lambda (c) (princ-error c) (invoke-restart 'import))))
    (export s15 p1))
  (assert (eq (find-symbol "SYM-1" p1) s15))
  (handler-bind ((package-error
                  (lambda (c) (princ-error c) (invoke-restart :pack-2))))
    (export foo2 p2))
  (assert (eq (find-symbol "FOO" p1) foo2))
  (assert (null (set-exclusive-or (list bar1 bar2 bar3 bar4)
                                  (find-all-symbols bar-name))))
  (handler-bind ((package-error
                  (lambda (c) (princ-error c) (invoke-restart :pack-1))))
    (export bar2 p2))
  (assert (eq (find-symbol bar-name p1) bar1))
  (export bar3 p3)
  (export bar4 p4)
  (handler-bind ((package-error
                  (lambda (c) (princ-error c) (invoke-restart :pack-4))))
    (unintern bar1 p1))
  (assert (eq (find-symbol bar-name p1) bar4))
  (delete-package p5)
  (handler-bind ((package-error (lambda (c) (princ-error c) (continue c))))
    (delete-package p2) (delete-package p3) (delete-package p4))
  (delete-package p1))
T

(let ((p1 (make-package "PACK" :use nil)) p2 p3 p4
      (bar-name (symbol-name (gensym "BAR-"))))
  (handler-bind ((package-error
                  (lambda (c) (princ-error c) (invoke-restart 'continue))))
    (assert (eq p1 (make-package "PACK"))))
  (handler-bind ((package-error
                  (lambda (c) (princ-error c) (invoke-restart 'read "KCAP"))))
    (setq p2 (make-package "PACK")))
  (assert (string= "KCAP" (package-name p2)))
  (handler-bind ((package-error
                  (lambda (c) (princ-error c) (invoke-restart 'continue))))
    (setq p3 (make-package "FOO" :nicknames (list "CL" bar-name "KCAP"))))
  (assert (equal (list bar-name) (package-nicknames p3)))
  (handler-bind ((package-error
                  (lambda (c) (princ-error c) (invoke-restart 'read "ZOT"))))
    (setq p4 (make-package "QUUX" :nicknames (list "CL" bar-name "KCAP"))))
  (assert (equal (list "ZOT") (package-nicknames p4)))
  (delete-package p1) (delete-package p2)
  (delete-package p3) (delete-package p4))
T

(handler-bind ((error (lambda (c) (princ-error c) (use-value '(9 8 7 6)))))
  (list (butlast 123 2)
        (butlast '#1=(1 2 3 . #1#) 2)
        (last 123 2)
        (last '#1# 2)))
((9 8) (9 8) (7 6) (7 6))

(handler-bind ((error (lambda (c) (princ c) (use-value 'doc-restart))))
  (setf (documentation '(doc-restart) 'function)
        "docstring for doc-restart")
  (documentation 'doc-restart 'function))
"docstring for doc-restart"

(unintern 'doc-restart)
T
