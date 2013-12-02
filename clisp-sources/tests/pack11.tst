;; -*- Lisp -*-
;; packages-test
;; -------------

;;test file for chapter 11

(packagep  *package*)
T

;; `list-all-packages' and type test
(every #'packagep (list-all-packages))
T

;;11.6 obligate Paketnamen u. deren Abkuerzungen

;;vorhandensein der standardpakete und find-package dafuer

(and (find-package 'lisp) t)
T
(and (find-package 'user) t)
T
(and (find-package 'keyword) t)
T
(and (find-package 'system) t)
T
(and (find-package 'sys) t)
T
(and (find-package "sys") t)
NIL
(and (find-package "sys") t)
NIL
(and (find-package "system") t)
NIL
(and (find-package "SYSTEM") t)
T
(and (find-package "SYS") t)
T

(eq (find-package (find-package "CL")) (find-package "CL"))
t

;nicknames
(find "SYS" (package-nicknames 'sys) :test #'string=)
"SYS"

;package-name
(package-name 'sys)
"SYSTEM"
(package-name 'system)
"SYSTEM"
(package-name "COMMON-LISP-USER")
"COMMON-LISP-USER"
(package-name "SYS")
"SYSTEM"

(let ((p (make-package #\p)))
  (prog1 (eq p (find-package #\p))
    (delete-package p)))
t

(progn (make-package #\p) (delete-package #\p))
T

;;; 11.7 anlegen von paketen, export import ...

  ;package-funktionen mit nutzerdefinierten paketen

;falls test1 bereits existiert
(and (find-package 'test1)
     (in-package "TEST1")
     (rename-package (find-package 'test1) 'test1-old)
     nil)
nil

;make-package
(package-name (make-package 'test1 :nicknames '(t1 tst1)))
"TEST1"

(package-name (rename-package (find-package "TEST1") (find-package "TEST1")))
"TEST1"

;package-use-list
;(package-use-list (find-package 'test1))
;("LISP")


(and (in-package "TEST1") T)
T


(export '(test1::test1-y test1::test1-z)
        (find-package '"TEST1"))
T

(export '(test1::test1-a test1::test1-b test1::test1-c)
        (find-package 'test1))
T

(setf test1-a -2
      test1-b -1
      test1-c  0
      test1-x  1
      test1-y  2
      test1-z  3)
3

;falls test2 bereits existiert
(and
        (find-package 'test2)
        (rename-package (find-package 'test2) 'test2-old)
        nil)
nil

(package-name (defpackage test2 (:nicknames "T2" "TST2") (:use test1)))
"TEST2"

(progn (in-package "TEST2") t)
LISP:T

(cl:package-name (cl:find-package 'test2))
"TEST2"

(cl:package-name cl:*package*)
"TEST2"

(cl:import '(cl:error) (cl:find-package 'test2))
CL:T

(cl:and (cl:boundp 'test1-x) test1-x)
CL:NIL

(cl:unintern 'test1-x)
CL:T

(eval (read-from-string "(cl:and (cl:boundp 'test1:test1-x) test1:test1-x)"))
#+XCL 1 #-XCL ERROR

(cl:and (cl:boundp 'test1::test1-x) test1::test1-x)
1

(cl:and (cl:boundp 'test1-y) test1-y)
#+XCL CL:NIL #-XCL 2

(cl:unintern 'test1-y)
#+XCL CL:T #-XCL CL:NIL

(cl:and (cl:boundp 'test1:test1-y) test1:test1-y)
#+XCL ERROR #-XCL 2

(cl:and (cl:boundp 'test1::test1-y) test1::test1-y)
2

(cl:import  '(test1::test1-x test1::test1-y) (cl:find-package 'test2))
CL:T

(cl:and (cl:boundp 'test1-x) test1-x)
1

(eval (read-from-string "(cl:and (cl:boundp 'test1:test1-x) test1:test1-x)"))
#+XCL 1 #-XCL ERROR

(cl:and (cl:boundp 'test1::test1-x) test1::test1-x)
1

(cl:and (cl:boundp 'test1-z) test1-z)
#+XCL CL:NIL #-XCL 3

(cl:unintern 'test1-z (cl:find-package 'test2))
#+XCL CL:T #-XCL CL:NIL

(cl:and (cl:boundp 'test1:test1-z) test1:test1-z)
#+XCL ERROR #-XCL 3

test1::test1-z
3

(cl:unexport  '(test1::test1-x test1::test1-y) (cl:find-package 'test1))
CL:T

(cl:and (cl:boundp 'test1-x) test1-x)
1

(cl:and (cl:boundp 'test1-y) test1-y)
#+XCL CL:NIL #-XCL 2

(cl:unintern 'test1-x (cl:find-package 'test2))
CL:T

(eval (read-from-string "test1:test1-x"))
ERROR

test1::test1-x
1

test1-z
3

(cl:unintern 'test1-z (cl:find-package 'test2))
#+XCL CL:T #-XCL CL:NIL

test1:test1-z
3

test1::test1-z
3

(cl:import 'test1::test1-z (cl:find-package 'test2))
CL:T

test1-z
3

test1:test1-z
3

test1::test1-z
3

test1-c
#+XCL ERROR #-XCL 0

(cl:unintern 'test-c (cl:find-package 'test2))
CL:T

test1:test1-c
0

test1::test1-c
0

(cl:import '(test1::test1-a test1::test1-b test1::test1-c)
             (cl:find-package 'test2))
CL:T

test1-c
0

test1:test1-c
0

test1::test1-c
0

(cl:eq 'test1-c 'test1::test1-c)
CL:T

  ;Ende nutzerdefinierte Pakete

;; test in standardmaessig vorgegebenen paketen

; export | import | unintern

(cl:and (cl:in-package "CL-USER") cl:T)
CL:T

(setf x 1 y 2 z 3)
3

(and (defpackage "EDITOR") T)
T

(and (in-package "EDITOR") T)
T

(unintern 'x)
T

(unintern 'y)
T

(unintern 'z)
T

cl-user::x
1

(eval (read-from-string "cl-user:x"))
ERROR

x
error

(eq 'x 'cl-user::x)
NIL

(unintern 'x)
T

(export '(cl-user::x cl-user::y) (find-package 'cl-user))
T

cl-user::x
1

cl-user:x
1

x
error

(unintern 'x)
T

(import 'cl-user:x (find-package 'editor))
T

x
1

(eq 'x 'cl-user::x)
t

(eq 'x 'cl-user:x)
t

(eq 'editor::x 'cl-user::x)
t

;; unexport

(and (in-package "CL-USER") T)
T

(unexport 'y)
T

(and (in-package "EDITOR") T)
T

y
ERROR

(eval (read-from-string "cl-user:y"))
ERROR

cl-user::y
2

(and (in-package "CL-USER") (package-name *package*))
"COMMON-LISP-USER"

(ext:appease-cerrors
 (let ((*package* "not a package - just a string"))
   (multiple-value-list (read-from-string "READ-FROM-STRING"))))
(READ-FROM-STRING 16)

;; http://www.lisp.org/HyperSpec/Issues/iss194-writeup.html
(let ((tmp-sym (make-symbol "FOO"))
      (old-sym (find-symbol "FOO" "CL-USER")))
  (when old-sym (unintern old-sym "CL-USER"))
  (list (import tmp-sym "CL-USER")
        (package-name (symbol-package tmp-sym))
        (unintern tmp-sym "CL-USER")
        (find-symbol "FOO" "CL-USER")))
(T "COMMON-LISP-USER" T NIL)

;; shadowing-import -- zunaechst ohne geerbte symbole!!

(setf d 4 e 5 f 6 y 111 x 222)
222

(export '(cl-user::a cl-user::b cl-user::c cl-user::y cl-user::x)
        (find-package 'cl-user))
T

(import '(cl-user::a cl-user::b cl-user::c cl-user::y) (find-package 'editor))
ERROR

(and (make-package 'shadow-test) (in-package "SHADOW-TEST") t)
T

(setf x 'shadow-test)
shadow-test

(shadowing-import '(cl-user::d cl-user::e cl-user::f cl-user::x)
                  (find-package 'shadow-test))
T

x
222

(eq cl-user::x x)
T

; shadow

(shadow '(e #\F) (find-package 'shadow-test))
t

(setf e 'shadow-test-e)
shadow-test-e

(eq 'e 'cl-user::e)
#+XCL nil #-XCL t

e
shadow-test-e

(eval (read-from-string "cl-user:e"))
error

cl-user::e
#+XCL 5 #-XCL shadow-test-e

;; unintern a shadowing symbol
(progn
  (setq pg3 (make-package "G3") pg1 (make-package "G1" :use (list pg3))
        pg2 (make-package "G2" :use (list pg3))
        ph (make-package "H" :use (list pg1 pg2)))
  (shadow "FOO" ph))
t

(setq gsym (intern "FOO" pg3))   g3::foo

(export gsym pg3)                t
(export gsym pg1)                t
(export gsym pg2)                t

(multiple-value-list (setf (values sym access) (find-symbol "FOO" ph)))
(h::foo :internal)

(package-shadowing-symbols ph)   (h::foo)
(eq sym gsym)                    nil
(equal (symbol-package sym) ph)  t

(unintern sym ph)                t

(delete-package ph)              t
(delete-package pg1)             t
(delete-package pg2)             t
(delete-package pg3)             t
(delete-package "TEST2")         t
(delete-package "TEST1")         t

; use-package | unuse-package

(and (make-package 'use-test) (in-package "USE-TEST") t)
t

(use-package '(cl-user))
T

cl-user::d
4

(eval (read-from-string "cl-user:d"))
#+XCL 4 #-XCL ERROR

d
ERROR

(unuse-package 'cl-user)
T

cl-user::d
4

(eval (read-from-string "cl-user:d"))
ERROR

d
ERROR

;make-package mit beutzung eines paketes, dass geerbte symbole enthaelt

(and (make-package 'inherit :nicknames '(inh i)) (in-package "INHERIT") T)
T

(setf a 'inherita b 'inheritb)
inheritb

(export '(a b) (find-package 'inherit))
T

(and (make-package 'inherit1 :use '(inherit)) (in-package "INHERIT1") T)
T

a
inherit::inherita

b
inherit::inheritb

(cl:setf c 'inherit1c)
inherit1c

(cl:and (cl:make-package 'inherit2 :use '(inherit1))
        (cl:in-package "INHERIT2") cl:T)
CL:T

a
#+XCL inherita #-XCL CL:ERROR

b
#+XCL inheritb #-XCL CL:ERROR

c
#+XCL inherit1c #-XCL CL:ERROR

(eval (read-from-string "(cl:eq 'c 'inherit1:c)"))
#+XCL CL:T #-XCL CL:ERROR

(eval (read-from-string "(cl:eq 'a 'inherit:a)"))
#+XCL CL:T #-XCL CL:ERROR

(eval (read-from-string "(cl:eq 'b 'inherit:b)"))
#+XCL CL:T #-XCL CL:ERROR

(cl:eq 'c 'inherit1::c)
#+XCL CL:T #-XCL CL:NIL

(cl:eq 'a 'inherit::a)
#+XCL CL:T #-XCL CL:NIL

(cl:eq 'b 'inherit::b)
#+XCL CL:T #-XCL CL:NIL

;find-all-symbols

(cl:and (cl:in-package "CL-USER") cl:T)
CL:T

(delete-package "EDITOR")      T
(delete-package "SHADOW-TEST") T
(delete-package "USE-TEST")    T
(delete-package "INHERIT2")    T
(delete-package "INHERIT1")    T
(delete-package "INHERIT")     T

; find-all-symbols fehlerhaft
(and (member 'cl-user::x (setf s (find-all-symbols 'x)))T)
T

(eval (read-from-string "(and (member 'editor:x s) t)"))
#+XCL T #-XCL ERROR

(and (member 'cl-user::x (setf s1 (find-all-symbols 'x)))T)
T

(set-difference s s1)
nil                              ;Ende Kommentar

;do-symbols | do-external-symbols | do-all-symbols

(setf sym nil
      esym nil
      asym nil
)
nil

(do-symbols (s (find-package 'cl-user))(push (symbol-name s) sym))
nil

(do-external-symbols (s (find-package 'cl-user))(push (symbol-name s) esym))
nil

(do-all-symbols (s)(push (symbol-name s) asym))
nil

(find "ESYM" sym :test #'string=)
"ESYM"

(find "ESYM" esym :test #'string=)
nil

(find "LAMBDA-LIST-KEYWORDS" esym :test #'string=)
#+XCL "LAMBDA-LIST-KEYWORDS" #-XCL NIL

;(count "LAMBDA-LIST-KEYWORDS" asym :test #'string=)
;T                                                  ;viel zu lang

; modules | provide | (require nicht getestet !)

(and *modules* T)
#+(or XCL ECL LISPWORKS) T #+CLISP NIL #-(or XCL CLISP ECL LISPWORKS) UNKNOWN

(let ((*modules* *modules*))
  (provide 'provide-test)
  (find "PROVIDE-TEST" *modules* :test #'string=))
"PROVIDE-TEST"

;; from ansi-tests
(unless (member "z" *modules* :test #'string=)
  (let ((*modules* *modules*)) (provide "z") (require #\z) nil))
NIL

(let ((*modules* *modules*))
  (provide :ABAZONK)
  (not (member :ABAZONK *modules* :test #'string=)))
NIL

;; <HS>/Body/mac_with-package-iterator.html
(defun test-package-iterator (package)
  (unless (packagep package)
    (setq package (find-package package)))
  (let ((all-entries '())
        (generated-entries '()))
    (do-symbols (x package)
      (multiple-value-bind (symbol accessibility)
          (find-symbol (symbol-name x) package)
        (push (list symbol accessibility) all-entries)))
    (with-package-iterator (generator-fn package
                                         :internal :external :inherited)
      (loop
        (multiple-value-bind (more? symbol accessibility pkg)
            (generator-fn)
          (declare (ignore pkg))
          (unless more? (return))
          (let ((l (multiple-value-list (find-symbol (symbol-name symbol)
                                                     package))))
            (unless (equal l (list symbol accessibility))
              (error "Symbol ~S not found as ~S in package ~A [~S]"
                     symbol accessibility (package-name package) l))
            (push l generated-entries)))))
    (unless (and (subsetp all-entries generated-entries :test #'equal)
                 (subsetp generated-entries all-entries :test #'equal))
      (error "Generated entries and Do-Symbols entries do not correspond"))
    t))
test-package-iterator

(compile 'test-package-iterator) test-package-iterator

(test-package-iterator :common-lisp-user) t

(test-package-iterator :common-lisp)      t

(progn ; from gcl/ansi-test
(defconstant +fail-count-limit+ 20)
(defmacro test-with-package-iterator (package-list-expr &rest symbol-types)
  "Build an expression that tests the with-package-iterator form."
  (let ((name (gensym))
        (cht-var (gensym))
        (pkg-list-var (gensym)))
    `(let ((,cht-var (make-hash-table))
           (,pkg-list-var ,package-list-expr)
           (fail-count 0))
       (with-package-iterator (,name ,pkg-list-var
                                     ,@(copy-list symbol-types))
         ;; For each symbol, check that name is returning appropriate things
         (loop
           (block fail
             (multiple-value-bind (more sym access pkg)
                 (,name)
               (unless more (return nil))
               (setf (gethash sym ,cht-var) t)  ;; note presence of symbol
               ;; Check that its access status is in the list,
               ;;  that pkg is a package,
               ;;  that the symbol is in the package,
               ;;  and that (in the package) it has the correct access type
               (unless (member access (quote ,(copy-list symbol-types)))
                 (unless (> fail-count +fail-count-limit+)
                   (format t "Bad access type: ~S ==> ~A~%" sym access))
                 (when (= fail-count +fail-count-limit+)
                   (format t "Further messages suppressed~%"))
                 (incf fail-count)
                 (return-from fail nil))

               (unless (packagep pkg)
                 (unless (> fail-count +fail-count-limit+)
                   (format t "Not a package: ~S ==> ~S~%" sym pkg))
                 (when (= fail-count +fail-count-limit+)
                   (format t "Further messages suppressed~%"))
                 (incf fail-count)
                 (return-from fail nil))
               (multiple-value-bind (sym2 access2)
                   (find-symbol (symbol-name sym) pkg)
                 (unless (or (eq sym sym2)
                             (member sym2 (package-shadowing-symbols pkg)))
                   (unless (> fail-count +fail-count-limit+)
                     (format t "Not same symbol: ~S ~S~%" sym sym2))
                   (when (= fail-count +fail-count-limit+)
                     (format t "Further messages suppressed~%"))
                   (incf fail-count)
                   (return-from fail nil))
                 (unless (eq access access2)
                   (unless (> fail-count +fail-count-limit+)
                     (format t "Not same access type: ~S ~S ~S~%"
                             sym access access2))
                   (when (= fail-count +fail-count-limit+)
                     (format t "Further messages suppressed~%"))
                   (incf fail-count)
                   (return-from fail nil)))))))
       ;; now, check that each symbol in each package has
       ;; been properly found
       (loop
         for p in ,pkg-list-var do
           (block fail
             (do-symbols (sym p)
               (multiple-value-bind (sym2 access)
                   (find-symbol (symbol-name sym) p)
                 (unless (eq sym sym2)
                   (unless (> fail-count +fail-count-limit+)
                     (format t "Not same symbol (2): ~S ~S~%"
                             sym sym2))
                   (when (= fail-count +fail-count-limit+)
                     (format t "Further messages suppressed~%"))
                   (incf fail-count)
                   (return-from fail nil))
                 (unless (or (not (member access
                                          (quote ,(copy-list symbol-types))))
                             (gethash sym ,cht-var))
                   (format t "Symbol not found: ~S~%" sym)
                   (incf fail-count)
                   (return-from fail nil))))))
       (or (zerop fail-count) fail-count))))
(defun with-package-iterator-internal (packages)
  (test-with-package-iterator packages :internal))
(compile 'with-package-iterator-internal)
(defun with-package-iterator-external (packages)
  (test-with-package-iterator packages :external))
(compile 'with-package-iterator-external)
(defun with-package-iterator-inherited (packages)
  (test-with-package-iterator packages :inherited))
(compile 'with-package-iterator-inherited)
(defun with-package-iterator-all (packages)
  (test-with-package-iterator packages :internal :external :inherited))
(compile 'with-package-iterator-all)
t)
T

(with-package-iterator-internal (list (find-package "COMMON-LISP-USER"))) t
(with-package-iterator-external (list (find-package "COMMON-LISP-USER"))) t
(with-package-iterator-inherited (list (find-package "COMMON-LISP-USER"))) t
(with-package-iterator-all (list (find-package "COMMON-LISP-USER"))) t

(with-package-iterator-internal (list (find-package "COMMON-LISP"))) t
(with-package-iterator-external (list (find-package "COMMON-LISP"))) t
(with-package-iterator-inherited (list (find-package "COMMON-LISP"))) t
(with-package-iterator-all (list (find-package "COMMON-LISP"))) t

(map nil #'print (list-all-packages))
nil

#+CLISP
(ext:appease-cerrors
 (let (SYSTEM::*COUTPUT-STREAM*)
   (setq SYSTEM::*COUTPUT-STREAM* 123)))
#+CLISP 123

#+CLISP
(package-case-sensitive-p
 (make-package "TEST-PACKAGE-CASE" :case-sensitive t :case-inverted t))
#+CLISP
T

#+CLISP
(package-case-inverted-p
 (defpackage "TEST-PACKAGE-CASE" (:case-sensitive nil) (:case-inverted nil)))
#+CLISP
NIL

#+CLISP
(setf (package-case-inverted-p "TEST-PACKAGE-CASE") t
      (package-case-sensitive-p "TEST-PACKAGE-CASE") t)
#+CLISP
T

#+CLISP
(let ((*break-on-signals* 'warning))
  (package-case-sensitive-p
    (defpackage "TEST-PACKAGE-CASE" (:case-inverted t) (:case-sensitive t))))
#+CLISP
T

#+CLISP
(delete-package "TEST-PACKAGE-CASE")
#+CLISP
T


; Clean up.
(unintern 'x)
T
