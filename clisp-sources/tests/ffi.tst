;; -*- lisp -*-
;; (ext:cd "../tests/") (load "tests") (run-test "ffi")
;; ./clisp -E utf-8 -norc -i tests/tests -x '(run-test "tests/ffi")'

(progn (defpackage "FTEST" (:use "FFI" "COMMON-LISP")) (in-package "FTEST") T)
T

(multiple-value-list (sizeof 'uint8))
(1 1)

(bitsizeof 'sint32)
32

(multiple-value-list (sizeof '(c-pointer int)))
#.(multiple-value-list (sizeof '(c-ptr int)))

(foreign-address-unsigned (unsigned-foreign-address 3))
3

(def-call-out c-self
  (:name "ffi_identity")
  (:documentation "return the pointer argument as is")
  (:arguments (obj c-pointer))
  (:return-type c-pointer) (:language :stdc))
C-SELF

(stringp (documentation 'c-self 'function))
T

(typep #'c-self 'function)
T

(typep #'c-self 'foreign-function)
T

(subtypep 'foreign-function 'function)
T

(check-type #'c-self foreign-function)
nil

(integerp (foreign-address-unsigned #'c-self))
T

(functionp (setq parse-c-type-optimizer
                 (compiler-macro-function 'parse-c-type)))
T

(funcall parse-c-type-optimizer '(parse-c-type 'c-pointer) nil)
'C-POINTER

(funcall parse-c-type-optimizer '(parse-c-type 'c-pointer 'opaque) nil)
(PARSE-C-TYPE 'C-POINTER 'OPAQUE)

(def-c-type opaque c-pointer)
OPAQUE

(funcall parse-c-type-optimizer '(parse-c-type 'opaque) nil)
'C-POINTER

(funcall parse-c-type-optimizer '(parse-c-type '(c-ptr uint8)) nil)
(PARSE-C-TYPE '(c-ptr uint8))

(car (funcall parse-c-type-optimizer '(parse-c-type `(c-array uint8 ,l)) nil))
VECTOR

(car (funcall parse-c-type-optimizer '(parse-c-type `(c-array ,type ,l)) nil))
PARSE-C-TYPE

(let () (declare (compile)) (with-c-var (place 'long -12345678) place))
-12345678

(let () (declare (compile))
  (with-foreign-object (fv 'long -12345678) (foreign-value fv)))
-12345678

(with-c-var (place '(c-array sint8 (2 3))
                   #2a((-1 -2 -3) (-9 -8 -7)))
  place)
#2A((-1 -2 -3) (-9 -8 -7))

(with-c-var (place '(c-array sint8 (2 3))
                   #(#(-1 -2 -3) #(-9 -8 -7)))
  place)
ERROR

(with-c-var (place '(c-array sint8 (2 3))
                   #2a((-1 -2 -3) (-9 -8 -7)))
  (cast place '(c-array sint8 (3 2))))
#2A((-1 -2) (-3 -9) (-8 -7))

(with-foreign-object (a '(c-array sint32 4)
                        #(122222 928389716 -1987234239 -123141))
  (memory-as a 'sint32 8))
-1987234239

(with-c-var (a '(c-array sint32 4)
               #(122222 928389716 -19 -123141))
  (setf (memory-as (c-var-address a) 'sint32 8) 478798798) a)
#(122222 928389716 478798798 -123141)

(with-c-var (a '(c-array sint32 4)
               #(122222 928389716 -19 -123141))
  (setf (memory-as (c-var-address a) 'sint32 8) 478798798))
478798798

(with-foreign-object (a '(c-array double-float 2)
                        #(9.05d12 -12.765d-13))
  (memory-as a 'double-float 0))
9.05d12

(with-foreign-object (a '(c-array single-float 2)
                        #(9.05f12 -12.765f-13))
  (memory-as a 'single-float 0))
9.05f12

(with-foreign-object (x 'single-float)
  (list (setf (memory-as x 'single-float) -28.23f-15)
        (foreign-value x)))
(-28.23f-15 -28.23f-15)

(with-c-var (p '(c-ptr sint32) -823498)
  (= (foreign-address-unsigned
      (memory-as (c-var-address p) 'c-pointer))
     (foreign-address-unsigned (c-var-address (deref p)))))
t

(with-foreign-object (p '(c-ptr sint32) -823498)
  (= (foreign-address-unsigned (memory-as p 'c-pointer))
     (foreign-address-unsigned p)))
nil

(with-foreign-object (p '(c-ptr sint16))
  (with-foreign-object (i 'sint16 -32765)
    (list (eq (setf (memory-as p 'c-pointer) i) i)
          (foreign-value p))))
(t -32765)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-pointer short)))
    (:return-type (c-ptr-null short)) (:language :stdc))
  (c-self nil))
nil

(with-foreign-object (x 'short -29876) (c-self x))
-29876

(with-foreign-object (x 'short -19635) (c-self (foreign-address x)))
-19635

(with-foreign-object (x 'character #\t) (c-self x))
ERROR

(type-of (foreign-function #'c-self
  (parse-c-type '(c-function (:arguments (obj (c-pointer short)))
                             (:return-type (c-ptr-null short))
                             (:language :stdc)))))
foreign-function

(funcall (foreign-function #'c-self
  (parse-c-type '(c-function (:arguments (obj long))
                             (:return-type long)
                             (:language :stdc))) :name "foo1")
         #x67676767)
#x67676767

(funcall (foreign-function (foreign-address #'c-self)
  (parse-c-type '(c-function (:arguments (obj long))
                             (:return-type long)
                             (:language :stdc))) :name "foo2")
         #x76767676)
#x76767676

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj c-pointer))
    (:return-type (c-pointer char)) (:language :stdc))
  (c-self nil))
nil

(with-c-var (x 'char -112)
  (let ((ref (c-self (c-var-address x))))
    (list
     (typep ref 'foreign-variable)
     (foreign-value ref))))
(T -112)

;; <http://sourceforge.net/tracker/index.php?func=detail&aid=679661&group_id=1355&atid=101355>
(def-c-struct triv (i int))
TRIV

(def-call-out trigger (:arguments (struct_array (c-array-ptr (c-ptr triv))))
  (:name "ffi_identity") (:language :stdc)
  (:return-type (c-array-ptr (c-ptr triv))))
TRIGGER

(trigger (vector (make-triv :i 0) (make-triv :i 1) (make-triv :i 3)
                 (make-triv :i 4) (make-triv :i 5) (make-triv :i 6)))
#(#S(TRIV :I 0) #S(TRIV :I 1) #S(TRIV :I 3)
  #S(TRIV :I 4) #S(TRIV :I 5) #S(TRIV :I 6))

(with-foreign-object (x '(c-array-ptr int) (vector -4 6 7))
  (foreign-value x))
#(-4 6 7)

(let ((v (allocate-deep 'triv (make-triv :i 42))))
  (prog1
      (list (typeof (foreign-value v))
            (slot (foreign-value v) 'i))
    (foreign-free v)))
((C-STRUCT TRIV (I INT)) 42)

(def-call-out c-self (:name "ffi_identity")
  (:arguments (obj c-pointer))
  (:return-type (c-pointer triv)) (:language :stdc))
C-SELF

(with-c-var (v 'triv (make-triv :i 8476272))
  (with-c-place (w (c-self (c-var-object v)))
    (setf (slot v 'i) -74590302)
    (list
     (typeof w)
     (slot w 'i))))
((C-STRUCT TRIV (I INT)) -74590302)

(list
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-pointer triv)))
    (:return-type c-pointer) (:language :stdc))
  (c-self nil))
(C-SELF NIL)

#+UNICODE
(type-of (setq orig-encoding custom:*foreign-encoding*))
#+UNICODE EXT:ENCODING

#+UNICODE
(typep (setf custom:*foreign-encoding*
             (ext:make-encoding :charset 'charset:iso-8859-1))
       'ext:encoding)
#+UNICODE T

#+UNICODE
(typep (setf custom:*foreign-encoding* (ext:make-encoding :charset 'charset:utf-8))
       'ext:encoding)
#+UNICODE T

(typep (ffi::lookup-foreign-variable "ffi_user_pointer"
                                     (ffi::parse-c-type 'ffi:c-pointer))
       'foreign-variable)
T

(ffi::lookup-foreign-variable "ffi_user_pointer" (parse-c-type 'uint64))
ERROR

(typep (ffi::lookup-foreign-variable "ffi_user_pointer"
                                     (parse-c-type '(c-array-ptr sint8)))
       'foreign-variable)
T

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr-null (c-array character 3))))
    (:return-type (c-ptr (c-array uint8 3))) (:language :stdc))
  (c-self "@A0"))
#(64 65 48)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr-null (c-array sint8 4))))
    (:return-type (c-array-ptr uint8)) (:language :stdc))
  (list (c-self #(127 64 63 0)) (c-self nil)))
(#(127 64 63) NIL)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr-null (c-array uint8 5))))
    (:return-type (c-array-ptr sint8)) (:language :stdc))
  (c-self (make-array 5 :element-type '(unsigned-byte 8)
                        :initial-contents '(127 63 64 0 6))))
#(127 63 64)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj c-string))
    (:return-type (c-array-ptr uint8)) (:language :stdc))
  (c-self (coerce '(#\@ #\A #\Newline #\2) 'string)))
#(64 65 10 50)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr (c-array-max uint16 4)) :in-out))
    (:return-type nil) (:language :stdc))
  (c-self (make-array 4 :element-type '(unsigned-byte 16)
                        :initial-contents '(128 255 0 127))))
#(128 255)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (a1 (c-ptr (c-array-max uint32 4)))
                (a2 (c-ptr (c-array-max uint8 4)))
                (a3 (c-ptr (c-array-max uint8 4)))
                (a4 (c-ptr (c-array     uint32 2))))
    (:return-type (c-ptr (c-array-max sint32 4))) (:language :stdc))
  (c-self (make-array 3 :element-type '(unsigned-byte 32)
                        :initial-contents '(128 0 127))
          (vector 1 2 3)
          (make-array 2 :element-type '(unsigned-byte 8)
                        :initial-contents '(241 17))
          (make-array 2 :element-type '(unsigned-byte 32)
                        :initial-contents '(1299 192225))))
#(128)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr (c-array sint8 4)) :in-out))
    (:return-type nil) (:language :stdc))
  (c-self #(-128 -99 0 127)))
#(-128 -99 0 127)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr (c-array uint16 4)) :in-out))
    (:return-type nil) (:language :stdc))
  (c-self (make-array 4 :element-type '(unsigned-byte 16)
                        :initial-contents '(128 255 0 127))))
#(128 255 0 127)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first boolean)
                (obj (c-ptr (c-array uint16 4)) :in-out))
    (:return-type nil) (:language :stdc))
  (c-self T #(1000 #xff 0 127)))
#(1000 255 0 127)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr (c-union (c1 character)
                                     (s (c-array-ptr character))))))
    (:return-type (c-ptr character)) (:language :stdc))
  (c-self #\w))
#\w

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first boolean)
                (obj (c-ptr (c-union (c character)
                                     (b boolean)
                                     (p c-pointer)))
                     :in-out))
    (:return-type nil) (:language :stdc))
  (c-self t #\j))
#\j

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first c-string))
    (:return-type (c-ptr (c-array character 4))) (:language :stdc))
  (c-self "zrewp"))
"zrew"

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr uint8)))
    (:return-type (c-ptr (c-array character 4))) (:language :stdc))
  (c-self #(64 65 66 67 68)))
"@ABC"


;; Test convert_from_foreign of UTF-8 strings.
;; C-STRING, C-ARRAY dim1, C-ARRAY-MAX, C-ARRAY-PTR use UTF-8 encoding,
;; the others use ASCII encoding.

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr uint8)))
    (:return-type c-string) (:language :stdc))
  (c-self #(#xE6 #x97 #xA5  #xE6 #x9C #xAC  #xE8 #xAA #x9E)))
"日本語"

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr uint8)))
    (:return-type (c-ptr (c-array character 9))) (:language :stdc))
  (c-self #(#xE6 #x97 #xA5  #xE6 #x9C #xAC  #xE8 #xAA #x9E)))
"日本語"

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr uint8)))
    (:return-type (c-ptr (c-array-max character 20))) (:language :stdc))
  (c-self #(#xE6 #x97 #xA5  #xE6 #x9C #xAC  #xE8 #xAA #x9E  #x00
            #x9E #xAA #xE8 #xAC #x9C #xE6 #xA5 #x97 #xE6 #x0A)))
"日本語"

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr uint8)))
    (:return-type (c-array-ptr character)) (:language :stdc))
  (c-self #(#xE6 #x97 #xA5  #xE6 #x9C #xAC  #xE8 #xAA #x9E  #x00
            #x9E #xAA #xE8 #xAC #x9C #xE6 #xA5 #x97 #xE6 #x0A)))
"日本語"

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first uint8))
    (:return-type character) (:language :stdc))
  (c-self #x61))
#\a

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first uint8))
    (:return-type character) (:language :stdc))
  (c-self #x9E))
ERROR

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr uint8)))
    (:return-type (c-ptr (c-array character (3 3)))) (:language :stdc))
  (c-self #(#x61 #x62 #x63  #x64 #x65 #x66  #x67 #x68 #x69)))
#2A((#\a #\b #\c) (#\d #\e #\f) (#\g #\h #\i))

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr uint8)))
    (:return-type (c-ptr (c-array character (3 3)))) (:language :stdc))
  (c-self #(#xE6 #x97 #xA5  #xE6 #x9C #xAC  #xE8 #xAA #x9E)))
ERROR

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr uint8)))
    (:return-type (c-ptr character)) (:language :stdc))
  (c-self #(#x61 #x62 #x63  #x64 #x65 #x66  #x67 #x68 #x69)))
#\a

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr uint8)))
    (:return-type (c-ptr character)) (:language :stdc))
  (c-self #(#xE6 #x97 #xA5  #xE6 #x9C #xAC  #xE8 #xAA #x9E)))
ERROR

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr uint8)))
    (:return-type (c-ptr-null character)) (:language :stdc))
  (c-self #(#x61 #x62 #x63  #x64 #x65 #x66  #x67 #x68 #x69)))
#\a

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr uint8)))
    (:return-type (c-ptr-null character)) (:language :stdc))
  (c-self #(#xE6 #x97 #xA5  #xE6 #x9C #xAC  #xE8 #xAA #x9E)))
ERROR

;; Test convert_to_foreign of UTF-8 strings.
;; C-STRING, C-ARRAY dim1, C-ARRAY-MAX, C-ARRAY-PTR use UTF-8 encoding,
;; the others use ASCII encoding.

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first c-string))
    (:return-type (c-array-ptr uint8)) (:language :stdc))
  (c-self "日本語"))
#(#xE6 #x97 #xA5  #xE6 #x9C #xAC  #xE8 #xAA #x9E)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-ptr (c-array character 9))))
    (:return-type (c-ptr (c-array uint8 9))) (:language :stdc))
  (c-self "日本語"))
#(#xE6 #x97 #xA5  #xE6 #x9C #xAC  #xE8 #xAA #x9E)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-ptr (c-array-max character 20))))
    (:return-type (c-array-ptr uint8)) (:language :stdc))
  (c-self "日本語"))
#(#xE6 #x97 #xA5  #xE6 #x9C #xAC  #xE8 #xAA #x9E)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-ptr (c-array-max character 7))))
    (:return-type (c-array-ptr uint8)) (:language :stdc))
  (c-self "日本語"))
#(#xE6 #x97 #xA5  #xE6 #x9C #xAC)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-array-ptr character)))
    (:return-type (c-array-ptr uint8)) (:language :stdc))
  (c-self "日本語"))
#(#xE6 #x97 #xA5  #xE6 #x9C #xAC  #xE8 #xAA #x9E)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first character))
    (:return-type uint8) (:language :stdc))
  (c-self #\a))
#x61

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first character))
    (:return-type uint8) (:language :stdc))
  (c-self #\ø))
ERROR

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-ptr (c-array character (3 3)))))
    (:return-type (c-ptr (c-array uint8 9))) (:language :stdc))
  (c-self #2A("abc" "def" "ghi")))
#(#x61 #x62 #x63  #x64 #x65 #x66  #x67 #x68 #x69)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-ptr (c-array character (3 3)))))
    (:return-type (c-ptr (c-array uint8 9))) (:language :stdc))
  (c-self #2A("日本語" "Tür" "kçe")))
ERROR

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-ptr (c-array character (3 3)))))
    (:return-type (c-ptr (c-array uint8 9))) (:language :stdc))
  (c-self #2A("日" "本" "語")))
ERROR

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-ptr character)))
    (:return-type (c-ptr (c-array uint8 1))) (:language :stdc))
  (c-self #\a))
#(#x61)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-ptr character)))
    (:return-type (c-ptr (c-array uint8 1))) (:language :stdc))
  (c-self #\ø))
ERROR

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-ptr-null character)))
    (:return-type (c-ptr (c-array uint8 1))) (:language :stdc))
  (c-self #\a))
#(#x61)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first (c-ptr-null character)))
    (:return-type (c-ptr (c-array uint8 1))) (:language :stdc))
  (c-self #\ø))
ERROR


(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first c-string)
                (obj (c-ptr (c-array sint16 4)) :in-out))
    (:return-type nil) (:language :stdc))
  (c-self "abc" #(-32768 -255 0 -256)))
#(-32768 -255 0 -256)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (first c-string)
                (obj (c-ptr (c-array uint32 4)) :in-out))
    (:return-type nil) (:language :stdc))
  (c-self nil #(#xffffffff #xffffff 0 127)))
#(#xffffffff #xffffff 0 127)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-ptr (c-array-max sint16 17)) :out))
    (:return-type nil) (:language :stdc))
  (c-self))
#()

(with-foreign-object (fv 'long -12345678) (typep fv 'foreign-variable))
T

(progn
  (defparameter *x* 0)
  (defun callback (x)
    (the (unsigned-byte 16) x)
    (setf *x* x)
    (the (unsigned-byte 16) (1+ (* 2 x))))
  *x*)
0

(def-c-type idfun
 (c-function (:arguments (x uint)) (:return-type uint)
   (:language :stdc)))
IDFUN

;; convert forth and back
(type-of (setq callbackf (with-c-var (x 'idfun #'callback) x)))
FOREIGN-FUNCTION

(list (funcall callbackf 32767) *x*)
(65535 32767)

(with-foreign-object (x '(c-function
    (:arguments (x uint)) (:return-type uint)(:language :stdc)) callbackf))
NIL

(with-foreign-object (x '(c-function
    (:arguments (x int)) (:return-type uint)(:language :stdc)) callbackf))
ERROR

(progn (foreign-free callbackf) (makunbound 'callbackf))
CALLBACKF

(progn
  (defparameter *x* 0)
  (defun pass-float (x)
    (the double-float x)
    (setf *x* x)
    (the single-float (float x 1f0)))
  *x*)
0

(def-c-type idfpfun
 (c-function (:arguments (x double-float)) (:return-type single-float)
   (:language :stdc)))
IDFPFUN

(type-of (setq fpcallback (with-c-var (x 'idfpfun #'pass-float) x)))
FOREIGN-FUNCTION

(list (funcall fpcallback 3.5d0) *x*)
(3.5f0 3.5d0)

(progn (foreign-free fpcallback) 4)
4

(funcall fpcallback -7.5d0)
ERROR

(with-c-var (x 'sint64) x)
0

(with-c-var (x 'sint64 #x1111111111111111) t)
t

(with-c-var (x 'uint64 #x1111111111111111) (offset x 0 'uint32))
#x11111111
(with-c-var (x 'uint64 #x2222222222222222) (offset x 4 'uint32))
#x22222222

(with-c-var (f '(c-function (:return-type uint64) (:language :stdc))
               #'(lambda () #xAAAABBBB77773333))
  ;; TODO? foreign-free such callbacks
  (funcall f))
#xAAAABBBB77773333

(progn
 (def-call-out c-self (:name "ffi_identity")
   (:arguments (p (c-ptr-null sint64)))
   (:return-type (c-ptr sint64)) (:language :stdc))
 (c-self -1311768467284833366))
-1311768467284833366

(with-c-var (f '(c-function (:arguments (n sint64))
                            (:return-type uint64) (:language :stdc))
               #'(lambda (x) (- x)))
  ;; TODO? foreign-free such callbacks
  (funcall f #x-43219876fedcba98))
#x43219876fedcba98

(with-c-var (s '(c-struct list (c character) (d sint64))
               '(#\a -7378753924192827255))
            s)
(#\a -7378753924192827255)

(progn
 (def-call-out c-self (:name "ffi_identity")
   (:arguments (p sint64))
   (:return-type nil) (:language :stdc))
 (multiple-value-list (c-self -1311768467284833366)))
nil

(def-call-out foreign-as-string (:name "ffi_identity")
  (:arguments (obj c-pointer))
  (:return-type c-string) (:language :stdc))
FOREIGN-AS-STRING

(with-foreign-string (fv e b "ABC" #+UNICODE :encoding #+UNICODE charset:ascii
                         :null-terminated-p nil)
  (list e b))
(3 3)

(with-foreign-string (fv e b "ABC" #+UNICODE :encoding #+UNICODE charset:ascii
                         :null-terminated-p t)
  (list e b))
(4 4)

(with-foreign-string (f e b "abc" :start 1 :end 2)
  (foreign-as-string (foreign-address f)))
"b"

(with-foreign-string (f e b "abcd" :start 1 :end 3
                        #+UNICODE :encoding #+UNICODE charset:ascii)
  (memory-as f 'character 1))
#\c

(with-foreign-string (f e b "abcde" :start 1 :end 4
                        #+UNICODE :encoding #+UNICODE charset:ascii)
  (memory-as f 'string 1))
"cd"

;#+UNICODE ; clisp writes a BOM at start and I don't want to depend on that
;(with-foreign-string (f e b "abcde" :start 1 :end 4
;                        :encoding charset:utf-16)
;  (memory-as f 'string 2))
;#+UNICODE "cd"

(let ((f (with-foreign-string (fv e b "ABC") fv)))
  (validp f))
NIL

(block abort
  (with-foreign-string (fv e b "ABC")
    (setq fm fv) (return-from abort 123)))
123

(validp fm)
NIL

(block abort
  (with-foreign-object (fv 'sint16 -563)
    (setq fm fv) (return-from abort 246)))
246

(validp fm)
NIL

(foreign-value fm)
ERROR

(with-c-var (x '(c-array-max character 32) "") x)
""

(type-of (setq fm (allocate-deep 'c-string "abc" :read-only t)))
FOREIGN-VARIABLE

(foreign-value fm)
"abc"

(with-c-place (x fm) x)
"abc"

(with-c-place (x fm) (setf x "xyz"))
ERROR

(foreign-value (ffi::%cast fm (ffi::parse-c-type
                               '(c-ptr (c-array-max character 20)))))
"abc"

(with-c-place (x fm) (cast x '(c-ptr (c-array-max character 2))))
"ab"

(type-of (foreign-variable fm (parse-c-type '(c-ptr (c-array uint8 2)))))
FOREIGN-VARIABLE

(foreign-value (foreign-variable fm (parse-c-type '(c-ptr (c-array uint8 2)))))
#(97 98)

(foreign-value (foreign-variable (foreign-address fm)
  (parse-c-type '(c-ptr (c-array uint8 2))) :name "conversion"))
#(97 98)

(foreign-variable "abc" (parse-c-type 'c-pointer))
ERROR

(foreign-variable fm (parse-c-type 'c-pointer) :name 123)
ERROR

(foreign-variable #'c-self (parse-c-type 'c-pointer))
ERROR

(progn (foreign-free fm) 0)
0

(type-of (setq fa (foreign-address fm)))
FOREIGN-ADDRESS

(eq (foreign-pointer fa) (foreign-pointer fm))
T

(type-of (setq fv (unsigned-foreign-address (foreign-address-unsigned fm))))
FOREIGN-ADDRESS

(eq (foreign-pointer fv) (foreign-pointer fm))
NIL

(progn (set-foreign-pointer fv fm) 1)
1

(eq (foreign-pointer fm) (foreign-pointer fv))
T

(eq (foreign-pointer fm) (foreign-pointer fa))
T

(type-of (setq fp (foreign-pointer fa)))
FOREIGN-POINTER

(eq (foreign-pointer fa) fp)
T

(eq (set-foreign-pointer fa :copy) fp)
NIL

(eq (foreign-pointer fm) (foreign-pointer fp))
NIL

(progn (setf (validp fm) nil) 1)
1

(progn (setf (validp fm) nil) 2)
2

(let ((restarts (list (unsigned-foreign-address 123450))))
  (foreign-address-unsigned
   (handler-bind
    ((type-error (lambda (c &aux (retry (pop restarts)))
                   (declare (ignore c))
                   (when retry (use-value retry)))))
    (foreign-variable "abc" (parse-c-type 'char)))))
123450

(let ((restarts (list #'c-self)))
  (foreign-address-unsigned
   (handler-bind
    ((type-error (lambda (c &aux (retry (pop restarts)))
                   (declare (ignore c))
                   (when retry (use-value retry)))))
    (foreign-function "abc"
      (parse-c-type '(c-function (:language :stdc)))))))
#.(foreign-address-unsigned #'c-self)

(progn (setq fm (allocate-deep 'character "abc" :count 5)) (type-of fm))
FOREIGN-VARIABLE

(with-c-place (x fm) (identity (typeof x)))
(C-ARRAY-MAX CHARACTER 5)

(with-c-place (x fm) (typeof x))
(C-ARRAY-MAX CHARACTER 5)

(with-foreign-object (fv `(c-array-max character ,5) "abc")
  (with-c-place (x fv) (typeof x)))
(C-ARRAY-MAX CHARACTER 5)

(let () (declare (compile))
  (with-foreign-object (fv `(c-array-max character ,5) "abc")
    (with-c-place (x fv) (typeof x))))
(C-ARRAY-MAX CHARACTER 5)

(let () (declare (compile))
  (with-c-var (x `(c-array uint32 ,1))
    (typeof (cast x `(c-array uint8 ,4)))))
(C-ARRAY UINT8 4)

(let () (declare (compile))
  (with-c-var (x `(c-array-max uint32 ,1) #(#x11111111))
    (cast x `(c-array uint8 ,4))))
#(#x11 #x11 #x11 #x11)

(with-c-place (x fm) (setf (element x 1) #\Z))
#\Z

(foreign-value fm)
"aZc"

(with-c-place (x fm) (cast x '(c-array character 3)))
ERROR

(with-c-place (x fm) (offset x 1 '(c-array character 2)))
"Zc"

(with-c-place (x fm)
  (slot (cast x '(c-union (s (c-array character 5))
                          (c character))) 'c))
#\a

(progn (foreign-free fm) (validp fm))
T

(progn (setf (validp fm) nil) (validp fm))
NIL

(makunbound 'fm)
FM

(with-c-var (place 'long -12345678)  place)
-12345678

(with-c-var (place 'c-string "abc")  place)
"abc"

(with-c-var (place `(c-array character ,6) "abcdef")  place)
"abcdef"

(with-c-var (place `(c-array character ,6) "abcdefghi")  place)
ERROR

(with-c-var (place '(c-array character 7) "abc")  place)
ERROR

(with-c-var (place `(c-array-max character ,6) "abcdefgh")  place)
"abcdef"

(with-c-var (place `(c-array-max character ,7) "abc")  place)
"abc"

(progn (setq fm (allocate-shallow 'character :count 3)) (type-of fm))
FOREIGN-VARIABLE

(foreign-value fm)
""

(progn (foreign-free fm) T)
T

(allocate-shallow 'ulong :count 0)
ERROR

(progn (setq fm (allocate-shallow 'long :count 2)) (type-of fm))
FOREIGN-VARIABLE

(foreign-value fm)
#(0 0)

(with-c-place (a fm)
  (dotimes (i 3) (setf (element a i) (+ 100000000 i))))
ERROR

(with-c-place (a fm) a)
#(100000000 100000001)

(def-call-out c-self (:name "ffi_identity")
  (:arguments (obj (c-pointer (c-array long 2))))
  (:return-type (c-pointer (c-array long 2))) (:language :stdc))
C-SELF

(foreign-value (c-self fm))
#(100000000 100000001)

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-pointer (c-array long 2))))
    (:return-type (c-ptr (c-array long 2))) (:language :stdc))
  (c-self fm))
#(100000000 100000001)

(progn (foreign-free fm) (makunbound 'fm))
FM

(with-c-var (place '(c-ptr (c-struct vector
                             (a (c-array long 2))
                             (s (c-array character 3))))
                   '#(#(-2000000000 -1000333111) "abc"))
  (slot (deref place) 'a))
#(-2000000000 -1000333111)

(with-c-var (place '(c-ptr (c-struct list
                             (a (c-array long 2))
                             (s (c-array character 3))))
                   '(#(-3 -1) #(#\a #\b #\c)))
  (slot (deref place) 's))
"abc"

(progn (foreign-free (allocate-deep
    '(c-ptr (c-struct list (a (c-array long 2)) (s (c-array character 3))))
    '(#(-2 -3) "abc")) :full t) nil)
NIL

(with-foreign-object (x '(c-array-ptr int) (vector -4 6 7)) (foreign-value x))
#(-4 6 7)

(with-foreign-object (x '(c-struct list
                          (a (c-array-ptr ulong))
                          (b (c-array-ptr ulong))
                          (c (c-array-ptr ulong))
                          (d (c-ptr (c-array ulong 2))))
                        '(#(123456789) #(987654321) #(543235263)
                          #(936272894 1333222444)))
  (foreign-value x))
(#(123456789) #(987654321) #(543235263) #(936272894 1333222444))

;; UTF-16 is not in every CLISP with #+UNICODE
#+UNICODE
(let ((sy (find-symbol "UTF-16" "CHARSET")))
  (if (and sy (boundp sy) (sys::encodingp (symbol-value sy)))
      (with-foreign-string (fv e b "ABC" :encoding (symbol-value sy))
        (list e b))
      '(4 10)))
#+UNICODE (4 10) ; #\uFEFF is added upfront

;; prevent the user from shooting himself in the foot
(setf (validp (unsigned-foreign-address 4)) nil)
ERROR

(with-c-var (place '(c-ptr (c-struct list
                            (a (c-array long 2))
                            (s (c-array character 3))))
                   '(#(-3 -1) #(#\a #\b #\c)))
  (slot (deref place) 's))
"abc"

(with-c-var (place '(c-ptr (c-struct vector
                            (a (c-array long 2))
                            (s (c-array character 3))))
                   '#(#(-3 -1) #(#\a #\b #\c)))
  place)
#(#(-3 -1) "abc")

(def-call-out make-foreign-string
  (:arguments (s c-string :in :malloc-free))
  (:name "ffi_identity") (:language :stdc)
  (:return-type c-pointer))
MAKE-FOREIGN-STRING

(progn (setf *x* (make-foreign-string "abcd"))
       (with-c-var (p 'c-pointer *x*) (cast p '(c-ptr (c-array uint8 4)))))
#(97 98 99 100)

(with-c-var (p 'c-pointer *x*) (cast p '(c-ptr (c-array character 4))))
"abcd"

(progn (foreign-free *x*) (makunbound '*x*))
*x*

(def-c-type link-node (c-struct list
  (x ulong) (s (c-ptr (c-array-max character 3)))
  (p (c-pointer link-node))))
LINK-NODE

(type-of (setq fm (allocate-deep 'link-node '(834687632 "Ef" nil))))
FOREIGN-VARIABLE

(progn
  (def-call-out c-self (:name "ffi_identity")
    (:arguments (obj (c-pointer link-node)))
    (:return-type (c-pointer link-node)) (:language :stdc))
  (foreign-value (c-self fm)))
(834687632 "Ef" NIL)

(with-c-var (p 'link-node (list 298734222 "IyH" fm))
  (cast p '(c-struct vector
             (x ulong) (s (c-ptr (c-array-max character 3)))
             (p (c-ptr-null link-node)))))
#(298734222 "IyH" (834687632 "Ef" NIL))

(progn (foreign-free fm :full t) (makunbound 'fm))
FM

#+UNICODE
(type-of (setq custom:*foreign-encoding* orig-encoding))
#+UNICODE EXT:ENCODING

#+win32
(progn
  (def-call-out command-line
      (:name "GetCommandLineA") (:library "kernel32.dll")
      (:arguments) (:return-type ffi:c-string) (:language :stdc))
  (stringp (command-line)))
#+win32 T

#-BeOS ; FFI::FOREIGN-LIBRARY-FUNCTION not defined on BeOS
(list
 (def-call-out c-malloc (:arguments (l long))
   (:name "malloc") (:language :stdc) (:return-type c-pointer)
   (:library :default)) ; use allocate-shallow or allocate-deep instead!
 (def-call-out c-free (:arguments (p c-pointer))
   (:name "free") (:language :stdc) (:return-type nil)
   (:library :default))) ; use foreign-free instead!
#-BeOS
(c-malloc c-free)

;; this is ugly and inefficient; if you find yourself doing this,
;; you probably want to do it in C
#-BeOS
(let ((m (c-malloc 4)) ret)
  (unwind-protect
       (with-c-var (v '(c-ptr (c-array uint8 4)))
         (setf (cast v 'c-pointer) m)
         (with-c-var (i '(c-ptr uint32))
           (setf (cast i 'c-pointer) m)
           (setq i 0)
           (push v ret)
           (setq i (1- (ash 1 32)))
           (push v ret)
           (setq v #A((unsigned-byte 8) (4) (1 2 3 4)))
           ;; I depends on endianness!
           (assert (or (= i (+ (ash 4 24) (ash 3 16) (ash 2 8) 1))
                       (= i (+ (ash 1 24) (ash 2 16) (ash 3 8) 4)))))
         (nreverse ret))
    (c-free m)))
#-BeOS
(#A((unsigned-byte 8) (4) (0 0 0 0))
 #A((unsigned-byte 8) (4) (255 255 255 255)))

(integerp (sys::code-address-of #'c-malloc)) T

(listp (macroexpand '(def-c-var foo-var (:type int)))) T

(listp (macroexpand '(def-c-const foo-const))) T

(progn (in-package "USER") (delete-package "FTEST") T) T
