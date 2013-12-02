;;; -*- Lisp -*-
;;; Test "Exceptional situations" as specified by CLHS

;; NB: CLHS section 1.4.2 implies that we have to verify only those
;; specifications which have the wording "an error is signalled" or
;; "an error should be signalled".

(invoke-restart 'nonexistent-restart)
control-error

(acos 'x)
type-error

(acosh 'x)
type-error

(progn
  (defgeneric foo01 (x))
  (defmethod foo01 ((x number)) t)
  (let ((m (find-method #'foo01 nil (list (find-class 'number)))))
    (remove-method #'foo01 m)
    (defgeneric foo01 (x y))
    (add-method #'foo01 m)
) )
error

#-CLISP ; documented behaviour of ADD-METHOD
(progn
  (defgeneric foo02 (x))
  (defmethod foo02 ((x number)) t)
  (let ((m (find-method #'foo02 nil (list (find-class 'number)))))
    (remove-method #'foo02 m)
    (defgeneric foo03 (x))
    (add-method #'foo03 m)
) )
#-CLISP
error

(let ((a (make-array 5 :adjustable t)))
  (adjust-array a 4 :fill-pointer 1)
)
error

(adjustable-array-p '(x))
type-error

(alpha-char-p 33)
type-error

(alphanumericp 33)
type-error

(array-dimensions '(x))
type-error

(array-displacement '(x))
type-error

(array-element-type '(x))
type-error

(array-has-fill-pointer-p '(x))
type-error

(array-rank '(x))
type-error

(array-total-size '(x))
type-error

(ash 3/4 2)
type-error

(ash 3 4.0)
type-error

(asin 'x)
type-error

(asinh 'x)
type-error

(atan 'x)
type-error

(atan #c(0 0.4) 3.4)
type-error

(atan -4 #c(3 4))
type-error

(atanh 'x)
type-error

(boole 'x 3 4)
type-error

(boole boole-and 3/4 -7)
type-error

(boole boole-set 5 #c(-3 4))
type-error

(both-case-p 33)
type-error

(boundp 47)
type-error

(butlast '(a b c) -1)
type-error

(butlast '#(a b c))
type-error

(car 'x)
type-error

(cdr '#(a b c))
type-error

(cdadar '((x y)))
type-error

(progn
  (defgeneric foo04 (x))
  (defmethod foo04 ((x real)) 'ok)
  (defmethod foo04 ((x integer)) (call-next-method (sqrt x)))
  (foo04 -1))
error

(progn
  (defgeneric foo041 (x))
  (defmethod foo041 ((x real)) 'ok)
  (defmethod foo041 ((x integer)) (call-next-method (sqrt x)))
  (foo04 2))
error

(ccase 'x)
type-error

(char-code 33)
type-error

(char-downcase 33)
type-error

(char-equal)
program-error

(char-greaterp)
program-error

(char-lessp)
program-error

(char-name 33)
type-error

(char-not-equal)
program-error

(char-not-greaterp)
program-error

(char-not-lessp)
program-error

(char-upcase 33)
type-error

(char/=)
program-error

(char<)
program-error

(char<=)
program-error

(char=)
program-error

(char>)
program-error

(char>=)
program-error

(character "abc")
type-error

(character "")
type-error

#-CLISP ; see custom:*coerce-fixnum-char-ansi* in impnotes
(character 33)
#-CLISP
type-error

(clear-input '*terminal-io*)
type-error

(clear-output '*terminal-io*)
type-error

(coerce '(a b c) '(vector * 4))
type-error

(coerce '#(a b c) '(vector * 4))
type-error

(coerce '(a b c) '(vector * 2))
type-error

(coerce '#(a b c) '(vector * 2))
type-error

(coerce "foo" '(string 2))
type-error

(coerce '#(#\a #\b #\c) '(string 2))
type-error

(coerce '(0 1) '(simple-bit-vector 3))
type-error

(coerce nil 'nil)
type-error

(coerce '#:nonexistent 'function)
error

(coerce 'and 'function)
error

(coerce -1 '(integer 0 100))
type-error

(compile-file "./12836123.lisp")
file-error

(concatenate 'symbol)
error

(concatenate '(string 3) "ab" "cd")
type-error

(copy-pprint-dispatch 'x)
type-error

(copy-seq 'x)
type-error

(copy-symbol #\x)
type-error

(cos 'x)
type-error

(cosh 'x)
type-error

(count #\x 'x)
type-error

(let ((x nil)) (ctypecase x))
type-error

(decode-float 2/3)
type-error

(defclass foo05 () (a b a))
program-error

(defclass foo06 () (a b) (:default-initargs x a x b))
program-error

(defclass foo07 () ((a :allocation :class :allocation :class)))
program-error

(defclass foo08 () ((a :initform 42 :initform 42)))
program-error

(defclass foo09 () ((a :type real :type real)))
program-error

(defclass foo10 () ((a :documentation "bla" :documentation "blabla")))
program-error

(defgeneric if (x))
program-error

(progn
  (defmacro foo11 (x) x)
  (defgeneric foo11 (x)))
program-error

;; redefinition
(defun ext:! (a b c) (+ a b c))
PACKAGE-ERROR

(defun foo11 ((x y 1) z) (list x y z))
program-error

(lambda ((x y 1) z) (list x y z))
program-error

(progn
  (defun foo12 (x) x)
  (defgeneric foo12 (x)))
program-error

(defgeneric foo13 (x y &rest l)
  (:method (x y))
)
error

(defgeneric foo14 (x)
  (:documentation "bla")
  (:documentation "blabla")
)
program-error

(defgeneric foo15 (x)
  (:my-option t))
program-error

; define-method-combination is too complicated

(progn
  (defvar foo16)
  (define-symbol-macro foo16 t))
program-error

(defmethod if (x) nil)
error

(progn
  (defmacro foo17 (x) x)
  (defmethod foo17 (x) nil))
error

(progn
  (defun foo18 (x) x)
  (defmethod foo18 (x) nil))
error

(progn
  (defgeneric foo19 (x))
  (defmethod foo19 (x y) nil))
error

(progn
  (defpackage "FOO20")
  (defpackage "FOO21" (:nicknames "FOO20")))
package-error

(defpackage "FOO22" (:size 20) (:size 20))
program-error

(defpackage "FOO23" (:documentation "bla") (:documentation "blabla"))
program-error

(defpackage "FOO24" (:my-option t))
program-error

(defpackage "FOO25" (:shadow "IF") (:intern "IF"))
program-error

(defpackage "FOO26" (:shadow "IF") (:import-from "USER" "IF"))
program-error

(defpackage "FOO27" (:shadow "IF") (:shadowing-import-from "USER" "IF"))
program-error

(defpackage "FOO28" (:intern "IF") (:import-from "USER" "IF"))
program-error

(defpackage "FOO29" (:intern "IF") (:shadowing-import-from "USER" "IF"))
program-error

(defpackage "FOO30" (:import-from "USER" "IF") (:shadowing-import-from "USER" "IF"))
program-error

(defpackage "FOO31" (:export "IF") (:intern "IF"))
program-error

(defstruct foo32 a foo20::a)
program-error

(progn
  (defstruct foo33 a)
  (defstruct (foo34 (:include foo33)) foo20::a))
program-error

(delete #\x 'x)
type-error

(delete-duplicates 'abba)
type-error

;; This test fails on BeOS. The system does not give the ENOTDIR error code
;; that should be given according to POSIX. I'm too lazy to put a workaround
;; into pathname.d.
#-BeOS
(progn
  (with-open-file (s "./foo35.tmp" :direction :output #+(or CMU SBCL) :if-exists #+(or CMU SBCL) :supersede))
  (delete-file "./foo35.tmp/bar"))
#-BeOS
file-error

(destructuring-bind (a) '(1 2) a)
error

(destructuring-bind ((a b c)) '((1)) `(,a ,b ,b))
error

; directory - no way to make a directory search fail

#-CLISP ; documented behaviour of DISASSEMBLE
(disassemble #x123456)
#-CLISP
type-error

; dribble - no way to force a file-error

(ecase 'x)
type-error

(elt 'x 0)
type-error

(elt "abc" 4)
type-error

(elt '(a b c) 4)
type-error

(elt '#(a b c) 4)
type-error

(elt (make-array 3 :fill-pointer 3 :adjustable t) 4)
type-error

(endp 'x)
type-error

(ensure-directories-exist "/*/")
file-error

(error 42)
type-error

(let ((x nil)) (etypecase x))
type-error

(every '(lambda (x) x) nil)
type-error

(every #'identity 'x)
type-error

(fboundp '(psetf aref))
type-error

(fdefinition '(psetf aref))
type-error

(fdefinition '#:nonexistent)
undefined-function

(file-author "*")
file-error

(file-length *terminal-io*)
type-error

(with-open-file (s "./foo35.tmp" :direction :output #+(or CMU SBCL) :if-exists #+(or CMU SBCL) :supersede)
  (file-position s 0.0))
error

(with-open-file (s "./foo35.tmp" :direction :output #+(or CMU SBCL) :if-exists #+(or CMU SBCL) :supersede)
  (file-position s -1))
error

(with-open-file (s "./foo35.tmp" :direction :input)
  (file-position s (+ (file-length s) 1000)))
error

(delete-file "./foo35.tmp")
null

(file-write-date "*")
file-error

(fill 'x #\x)
type-error

(fill (make-list 3) 'x :start nil)
type-error

(fill (make-list 3) 'x :start -1)
type-error

(fill (make-list 3) 'x :start 1 :end -1)
type-error

(fill-pointer "abc")
type-error

(find #\x 'x)
type-error

(find-class '#:nonexistent t)
error

(progn
  (defgeneric foo36 (x y))
  (find-method #'foo36 nil (list (find-class 'number))))
error

(progn
  (defgeneric foo37 (x))
  (find-method #'foo37 nil (list (find-class 'number))))
error

(finish-output '*terminal-io*)
type-error

(float-digits 2/3)
type-error

(float-precision 2/3)
type-error

(float-radix 2/3)
type-error

(float-sign 2/3)
type-error

(float-sign -4.5 2/3)
type-error

(fmakunbound '(psetf aref))
type-error

(force-output '*terminal-io*)
type-error

(funcall 'foo38)
undefined-function

(funcall 'and)
undefined-function

(gcd 4 3/4)
type-error

(gensym #\x)
type-error

(gentemp 't)
type-error

(gentemp "X" 24)
type-error

(get "a" 'x)
type-error

(get-dispatch-macro-character #\0 #\#)
error

(graphic-char-p 33)
type-error

(hash-table-rehash-size *readtable*)
type-error

(hash-table-rehash-threshold *package*)
type-error

(hash-table-size *random-state*)
type-error

(hash-table-test '#(a b c))
type-error

(imagpart #\c)
type-error

(in-package "FOO39")
package-error

(input-stream-p (pathname "abc"))
type-error

(integer-decode-float 2/3)
type-error

(integer-length 0.0)
type-error

(interactive-stream-p (pathname "abc"))
type-error

(invoke-restart 'foo40)
control-error

(invoke-restart-interactively 'foo41)
control-error

(isqrt -1)
type-error

(isqrt #c(3 4))
type-error

(last '(a b c) -1)
type-error

(lcm 4/7 8)
type-error

(length 'x)
type-error

(list-length 'x)
type-error

(list-length '(x . y))
type-error

(load "./128347234.lsp")
file-error

(load "*.lsp")
file-error

(load-logical-pathname-translations "FOO41")
error

(logand -3 2.3)
type-error

(logbitp -1 5)
type-error

(logbitp 2 3/7)
type-error

(logcount #*01010011)
type-error

(logical-pathname '#(#\A #\B))
type-error

(logical-pathname-translations '#(#\A #\B))
type-error

(lower-case-p 33)
type-error

(make-broadcast-stream (make-string-input-stream "abc"))
type-error

(make-concatenated-stream (make-string-output-stream))
type-error

(progn
  (defclass foo42 () ())
  (make-instance 'foo42 :x 1))
error

(make-list -1)
type-error

(progn
  (defstruct foo43)
  (make-load-form (make-foo43)))
error

(make-random-state 'x)
type-error

(make-sequence 'x 5)
type-error

(make-sequence 'sequence 5)
type-error

(make-sequence '(string 3) 4)
type-error

(make-sequence 'null 5)
type-error

(make-sequence 'cons 0)
type-error

(make-symbol 'x)
type-error

(make-synonym-stream *terminal-io*)
type-error

(make-two-way-stream (make-string-input-stream "abc") (make-string-input-stream "def"))
type-error

(make-two-way-stream (make-string-output-stream) (make-string-output-stream))
type-error

(makunbound "xx")
type-error

(map 'x #'identity "abc")
type-error

(map '(string 3) #'identity "ab")
type-error

(max 3 #c(4 0.0))
type-error

(map 'list #'identity '(a b . c))
type-error

(length '(a b . c))
type-error

(reduce #'+ '(1 2 . 3))
type-error

(merge '(vector * 5) '(3 1) '(2 4) #'<)
type-error

(min 3 #c(4 0.0))
type-error

(minusp #c(4 -3/4))
type-error

(muffle-warning)
control-error

(name-char '#(#\N #\u #\l))
type-error

(nbutlast '(a b c) -1)
type-error

(nbutlast '#(a b c))
type-error

(no-applicable-method #'cons)
error

(no-next-method #'print-object (find-method #'print-object nil (list (find-class 'standard-object) (find-class 't))))
error

(notany '(lambda (x) x) nil)
type-error

(notany #'identity 'x)
type-error

(notevery '(lambda (x) x) nil)
type-error

(notevery #'identity 'x)
type-error

(nthcdr 2 '(a . b))
type-error

(oddp 3.5)
type-error

#+(and UNIX (not BeOS))
(progn (open "/etc/passwd" :direction :input :if-exists :error) (/ 0))
#+(and UNIX (not BeOS))
division-by-zero

#+UNIX
(progn (open "/etc/nonexistent" :direction :input :if-exists :error) (/ 0))
#+UNIX
file-error

(open "./foo44nonexistent" :direction :input :if-does-not-exist :error)
file-error

(open "./*" :direction :input)
file-error

#+UNIX
(open "/etc/mtab" :direction :input :external-format 'mtab-entries)
#+UNIX
error

(open-stream-p (pathname "foo45"))
type-error

(output-stream-p (pathname "foo46"))
type-error

(package-name 47)
type-error

(package-nicknames (pathname "foo47"))
type-error

(package-shadowing-symbols (vector 'a 'b 'c))
type-error

(package-use-list (list 'a 'b 'c))
type-error

(package-used-by-list (list 'a 'b 'c))
type-error

(parse-integer "x-y")
error

(parse-namestring (coerce (list #\f #\o #\o (code-char 0) #\4 #\8) 'string))
parse-error

(parse-namestring "foo48:a" (logical-pathname "foo49:"))
error

(parse-namestring "sys:foo.bar." "sys")
parse-error

(pathname-match-p 34 "*")
type-error

(pathname-match-p "x" 34)
type-error

(peek-char nil (make-string-input-stream "") t)
end-of-file

(peek-char #\space (make-string-input-stream "") t)
end-of-file

#| ; It's not clear why peek-char should signal an error, where read-char and
   ; read-line don't. Kent Pitman says: "Sounds like a mess."
 (peek-char nil (make-string-input-stream "") nil nil t)
end-of-file
|#

(phase 'x)
type-error

(plusp #c(0 4.2))
type-error

(pprint-dispatch nil t)
type-error

(pprint-exit-if-list-exhausted)
error

(pprint-indent nil 2)
error

(let ((x (make-string-output-stream)))
  (pprint-logical-block (x nil :prefix 24)))
type-error

(let ((x (make-string-output-stream)))
  (pprint-logical-block (x nil :prefix "a" :per-line-prefix "b")))
error

(pprint-newline :fresh)
type-error

(pprint-pop)
error

(pprint-tab :paragraph 0 1)
error

(let ((*print-readably* t)) (print-unreadable-object (nil *standard-output*)))
print-not-readable

#+CLISP
(let ((ext:*print-pathnames-ansi* t) (ext:*parse-namestring-dot-file* :name))
  (write-to-string (make-pathname :name "foo.bar") :readably t))
#+CLISP
print-not-readable

#+CLISP
(let ((ext:*print-pathnames-ansi* t) (ext:*parse-namestring-dot-file* :type))
  (write-to-string (make-pathname :name "foo.bar") :readably t))
#+CLISP
print-not-readable

#+CLISP
(let ((ext:*print-pathnames-ansi* t) (ext:*parse-namestring-dot-file* :name))
  (write-to-string (make-pathname :type "bar") :readably t))
#+CLISP
print-not-readable

#+CLISP
(let ((ext:*print-pathnames-ansi* t) (ext:*parse-namestring-dot-file* :type))
  (write-to-string (make-pathname :name "foo.bar") :readably t))
#+CLISP
print-not-readable

(print 1 2)
type-error

(probe-file "*")
file-error

(provide 25)
type-error

(random -2.3)
type-error

(random 1/2)
type-error

(rational #c(2.4 -0.3))
type-error

(rationalize #c(2.4 -0.3))
type-error

(read (make-string-input-stream "((a b)") nil)
end-of-file

(read (make-string-input-stream " ") t)
end-of-file

(read-byte (pathname "foo50"))
type-error

(read-byte (make-string-input-stream "abc"))
error

(let ((filename "./foo51.bin"))
  (with-open-file (s filename :direction :output #+(or CMU SBCL) :if-exists #+(or CMU SBCL) :supersede
                              :if-exists :overwrite
                              :if-does-not-exist :create))
  (with-open-file (s filename :direction :input
                              :element-type '(unsigned-byte 8))
    (read-byte s t)))
end-of-file
(delete-file "./foo51.bin")
null

(let ((filename "./foo52.txt"))
  (with-open-file (s filename :direction :output #+(or CMU SBCL) :if-exists #+(or CMU SBCL) :supersede
                              :if-exists :overwrite
                              :if-does-not-exist :create))
  (with-open-file (s filename :direction :input)
    (read-char s t)))
end-of-file
(delete-file "./foo52.txt")
null

(let ((filename "./foo53.txt"))
  (with-open-file (s filename :direction :output #+(or CMU SBCL) :if-exists #+(or CMU SBCL) :supersede
                              :if-exists :overwrite
                              :if-does-not-exist :create))
  (with-open-file (s filename :direction :input)
    (read-char-no-hang s t)))
end-of-file
(delete-file "./foo53.txt")
null

(read-from-string "((a b))" nil nil :end 6)
end-of-file

(read-from-string " () () " t nil :start 3 :end 4)
end-of-file

(read-line (make-string-input-stream "") t)
end-of-file

(read-sequence (list 1 2 3) (make-string-input-stream "") :start nil)
type-error

(read-sequence (list 1 2 3) (make-string-input-stream "") :end -1)
type-error

(readtable-case nil)
type-error

(setf (readtable-case *readtable*) ':unknown)
type-error

(realpart #\c)
type-error

(progn
  (defclass foo54 () ())
  (reinitialize-instance (make-instance 'foo54) :dummy 0))
error

(remove #\x 'x)
type-error

(remove-duplicates 'abba)
type-error

(remprop 55 'abc)
type-error

(rplaca nil 5)
type-error

(rplacd nil 5)
type-error

(scale-float 2/3 -1)
type-error

(scale-float 3.4 1.0)
type-error

(set-dispatch-macro-character #\0 #\# #'(lambda (s c n) (loop)))
error

(set-pprint-dispatch '(vector * 2) nil #c(3 4))
error

(sin 'x)
type-error

(sinh 'x)
type-error

(sleep -1)
type-error

(progn
  (defclass foo55 () (a))
  (slot-boundp (make-instance 'foo55) ':a))
error

(progn
  (defclass foo56 () (a))
  (slot-makunbound (make-instance 'foo56) ':a))
error

(slot-missing (find-class 't) nil ':a 'setf)
error

(slot-unbound (find-class 't) nil ':a)
unbound-slot

(progn
  (defclass foo57 () (a))
  (slot-value (make-instance 'foo57) ':a))
error

(some '(lambda (x) x) nil)
type-error

(some #'identity 'x)
type-error

(special-operator-p '(and x y))
type-error

(special-operator-p '(setf aref))
type-error

(sqrt 'x)
type-error

(standard-char-p 33)
type-error

(stream-element-type '*terminal-io)
type-error

(string 33)
type-error

(symbol-function 33)
type-error

(symbol-function ':compile)
undefined-function

(symbol-macrolet ((t true)))
program-error

(symbol-macrolet ((*print-pretty* (stream-print-pretty *standard-output*))))
program-error

(symbol-macrolet ((foo58 t)) (declare (special foo58)))
program-error

(symbol-name '(setf foo59))
type-error

(symbol-package '(setf foo59))
type-error

(symbol-plist '(setf foo59))
type-error

(symbol-value '(setf foo59))
type-error

(symbol-value '#:nonexistent)
unbound-variable

(tan 'x)
type-error

(tanh 'x)
type-error

(throw '#:nonexistent nil)
control-error

(translate-logical-pathname (make-broadcast-stream))
type-error

(translate-logical-pathname (logical-pathname "foo61:"))
file-error

#-CLISP ; clisp explicitly allows symbols as pathnames
(translate-pathname 'x "x" "y")
#-CLISP
type-error

#-CLISP ; clisp explicitly allows symbols as pathnames
(translate-pathname "a" '* '*)
#-CLISP
type-error

(translate-pathname "x" "y" "z")
error

(truename "./foo62nonexistent")
file-error

(truename "./*/x")
file-error

;; <http://www.lisp.org/HyperSpec/Body/sec_19-2-2-4-3.html>
(make-pathname :directory '(:absolute :up))   file-error
(make-pathname :directory '(:absolute :back)) file-error
(make-pathname :directory '(:absolute :wild-inferiors :up))   file-error
(make-pathname :directory '(:relative :wild-inferiors :back)) file-error

(typep nil 'values)
error

(typep #'cons '(values t))
error

(typep #'cons '(function (t t) list))
error

(unexport ':foo63)
package-error

(unwind-protect (progn
  (defpackage "FOO64" (:export "XYZ"))
  (defpackage "FOO65" (:export "XYZ"))
  (defpackage "FOO66" (:use "FOO64" "FOO65") (:shadow "XYZ"))
  (unintern (find-symbol "XYZ" (find-package "FOO66")) (find-package "FOO66")))
  (delete-package "FOO20")
  (delete-package "FOO66")
  (delete-package "FOO65")
  (delete-package "FOO64"))
error

; update-instance-for-different-class too complicated

; update-instance-for-redefined-class too complicated

(upper-case-p 33)
type-error

(values-list '(a b . c))
type-error

(values-list (make-list multiple-values-limit))
error

(vector-pop "foo67")
type-error

(vector-pop (make-array 10 :fill-pointer 0))
error

(vector-push 'x (make-array 10))
error

(let ((a (make-array 5 :fill-pointer 0)))
  (dotimes (i 100) (vector-push-extend 'x a)))
error

(warn (make-condition 'error))
type-error

(warn (make-condition 'warning) "x")
type-error

(warn 'error)
type-error

(wild-pathname-p #\x)
type-error

(write-byte 1 (pathname "foo67"))
type-error

(write-byte 1 (make-string-output-stream))
error

(write-sequence '(#\1 #\2 #\3) (make-string-output-stream) :start nil)
type-error

(write-sequence '(#\1 #\2 #\3) (make-string-output-stream) :end -1)
type-error

(zerop 'x)
type-error

; section 2.3.1.1
(read-from-string "-35/000")
reader-error                  ; not division-by-zero!
(read-from-string "31e300")
reader-error                  ; not floating-point-overflow!

(WITH-STANDARD-IO-SYNTAX
  (LET ((*READ-SUPPRESS* T)) (READ-FROM-STRING "')")))
READER-ERROR

(WITH-STANDARD-IO-SYNTAX
  (LET ((*READ-SUPPRESS* T)) (READ-FROM-STRING "#<")))
READER-ERROR

(WITH-STANDARD-IO-SYNTAX
  (LET ((*READ-SUPPRESS* T)) (READ-FROM-STRING "# ")))
READER-ERROR

(WITH-STANDARD-IO-SYNTAX
  (LET ((*READ-SUPPRESS* T)) (READ-FROM-STRING "#)")))
READER-ERROR

;; loop: redeclaration of a variable
(loop for x in '(1 2 3) for x in '(4 5 6) collect x)
program-error

(loop for (x . x) in '((1 2) (3 4) (5 6)) collect x)
program-error

(loop for e across (vector '(x . y) '(u . v)) for e from 1 to 5 collect e)
program-error

(loop for x = '(nil nil nil) for x = 1 count x until t)
program-error

(loop with foo = '(a b) for x in '(c d) collecting x into foo
  finally (return foo))
program-error

(loop 42)
program-error

(fdefinition (list 'setf (gensym)))
undefined-function

(proclaim '(integer . foo))
type-error
