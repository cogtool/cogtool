;; Sample Lisp code, used as a test for clisp-indent.el
;; Contains at least one sample form for every special form or macro.
;; The list is sorted by exporting package.

(and (cond1) (cond2)
     (cond3)
     (cond4))

(and
  (cond1) (cond2)
  (cond3)
  (cond4))

(appease-cerrors
   (form1)
   (form2))

(assert (form1)
  "error")

(block nil
  (form1)
  (form2))

(block
    nil
  (form1)
  (form2))

(case foo
  ((T)
    (form1)
    (form2)))

(catch 'foo
  (form1)
  (form2))

(ccase foo
  ((T)
    (form1)
    (form2)))

(check-type x integer
  "error")

(compiler-let
    ((*load-pathname* nil))
  (form1))

(cond
  ((cond1)
    (form1)
    (form2)
    (form3)))

(cond ((cond1)
        (form1)
        (form2)
        (form3)))

(ctypecase x
  (integer
    (isqrt x)))

(decf
  (aref a i)
  (aref b i))

(declaim
  (optimize (safety 3)
            (speed 1)))

(declare
  (integer x))

(defclass fundamental-stream
          (stream clos:standard-object
           t)
  (($open :type boolean :initform t) ; whether the stream is open
   ($reval :type boolean :initform nil)) ; whether read-eval is allowed
  (:metaclass standard-class))

(defconstant pi
  3.14
  "Archimedes")

(defgeneric foobar (x y)
  "foo goes bar")

(define-condition arithmetic-error (error)
  (($operation :initarg :operation :reader arithmetic-error-operation)
   ($operands  :initarg :operands  :reader arithmetic-error-operands)))

(define-modify-macro decf (x)
  -
  "decrement")

(define-modify-macro decf (x) -
  "decrement")

(define-setf-expander subseq (seq start end)
  (compute-expansion))

(define-setf-method subseq (seq start end)
  (compute-expansion))

(define-symbol-macro *ansi*
  (get-ansi))

(definternational date-format
  (t ENGLISH))

(deflanguage ENGLISH)

(deflocalized date-format ENGLISH
  "~1{~5@*~D-~4@*~2,'0D-~3@*~2,'0D ~2@*~2,'0D:~1@*~2,'0D:~0@*~2,'0D~:}")

(defmacro incf (place &optional (delta '1))
  `(setf ,place (+ ,place ,delta)))

(defmethod foo ((x integer)
                y
                (z t))
  (bar x y z))

(defpackage regexp
  (:nicknames re)
  (:documentation
   "POSIX Regular Expressions - matching, compiling, executing.")
  (:use lisp ffi)
  (:export match match-start match-end match-string regexp-quote
           regexp-compile regexp-exec regexp-split with-loop-split))

(defparameter pi
  3.14
  "Archimedes")

(defsetf nth SYSTEM::%SETNTH
  "set nth element")

(defsetf aref (array &rest indices) (value)
  `(SYSTEM::STORE ,array ,@indices ,value))

(defsetf aref (array &rest indices)
              (value)
  `(SYSTEM::STORE ,array ,@indices ,value))

(defstruct (control-string-directive
             (:copier nil)
             (:conc-name "CSD-")
             (:predicate nil)
             (:constructor make-csd ()))
  (type         0 :type fixnum)
  (cs-index     0 :type fixnum)
  (parm-list    nil :type list)
  (v-or-#-p     nil :type symbol)
  (colon-p      nil :type symbol)
  (atsign-p     nil :type symbol)
  (data         nil)
  (clause-chain nil))

(deftype designator (thing)
  (cond ((symbolp thing)
         ...)))

(defun foo
       (&optional x
        &key y)

(defvar pi
  3.14
  "Archimedes")

(destructuring-bind (&optional x
                     &rest y)
    (foobar)
  (form1)
  (form2))

(deutsch
  "Beispiel"
 english
  "Sample")

(do ((x 1 (1+ x))
     (y (init-y)
        (inc y)))
    ((endp l))
  (form1))

(do* ((x 1 (1+ x))
      (y (init-y)
         (inc y)))
     ((endp l))
  (form1))

(do-all-symbols (s (find-package "NAME")
                 nil)
  (foobar))

(do-external-symbols (s (find-package "NAME")
                      nil)
  (foobar))

(do-symbols (s)
  (foobar))

(dohash (key val)
    (get-hash-table)
  (form1)
  (form2))

(dolist (x
         (list)
         nil)
  (form1))

(doseq (x
        (list)
        nil)
  (form1))

(dotimes (x
          (count)
          nil)
  (form1))

(ecase foo
  ((T)
    (form1)
    (form2)))

(english
  "Sample"
 deutsch
  "Beispiel")

(etypecase (form1)
  (integer
    (form2) (form3)
    (form3)))

(eval-when
    (compile)
  (do-something))

(exit-on-error
  (form1))

(flet ((mark-used
           (blockname
            &optional x
            &key y)
         (setf (get blockname 'used) t)
         (do ((L1 *format-uwps* (cdr L1))
              (L2 (get blockname 'uwps)))
             ((eq L1 L2))
           (setf (car L1) 'T))
         blockname))
  (mark-used nil))

(formatter
  "error: ~S")

(francais
  "exemple"
 english
  "Sample"
 deutsch
  "Beispiel")

(function
  random)

(function (lambda (x y)
            (+ x y)))

(function
  (lambda (x y)
    (+ x y)))

(function plus (lambda (x y)
                 (+ x y)))

(function plus
          (lambda (x y)
            (+ x y)))

(function
  plus
  (lambda (x y)
    (+ x y)))

(generic-flet
    ((dump (x)
       (:method ((x character))
         (format t "~C" x))
       (:method ((x integer))
         (format t "~D" x))))
  (dump obj))

(generic-function (x)
  (:method ((x character))
    (format t "~C" x))
  (:method ((x integer))
    (format t "~D" x)))

(generic-labels ((fib (x)
                   (:method ((x (eql 0)))
                     0)
                   (:method ((x (eql 1)))
                     1)
                   (:method ((x integer))
                     (if (minusp x)
                       (- (fib (+ x 2)) (fib (+ x 1)))
                       (+ (fib (- x 2)) (fib (- x 1)))))))
  (fib n))

(go
  nil)

(handler-bind
  ((simple-error
    #'(lambda (c) (throw 'exit (values nil c))))
   (error
    #'(lambda (c) (throw 'exit (values nil c)))))
  (form1))

(handler-case
  (push-object (part-ref object index) index)
  (part-ref-error ()
    (format t "~D does not refer to a selectable part." index))
  (:no-error (&optional x
              &key y)
    (f x y)))

(if (minusp x)
  (- (fib (+ x 2)) (fib (+ x 1)))
  (+ (fib (- x 2)) (fib (- x 1))))

(ignore-errors
  (/ x y))

(in-package "FOO"
            :use '("BAR"))

(in-package
  "FOO"
  :use '("BAR"))

(incf
  (aref a i)
  (aref b i))

(labels ((mark-used
             (blockname
              &optional x
              &key y)
           (setf (get blockname 'used) t)
           (do ((L1 *format-uwps* (cdr L1))
                (L2 (get blockname 'uwps)))
               ((eq L1 L2))
             (setf (car L1) 'T))
           blockname))
  (mark-used nil))

(lambda (x y)
  (+ x y))

(let ((x (i-x))
      (y
        (long-init-y)))
  (foobar))

(let* ((x (i-x))
       (y
         (long-init-y)))
  (foobar))


(load-time-value
  (foo)
  t)

(locally
  (declare (compile))
  (foo))

(loop
  (incf x)
  (return))

(loop-finish)

(macrolet ((Monat->Jahrtag (Monat) ; 0 <= Monat < 12, 0=März,...,11=Februar
             `(svref '#(0 31 61 92 122 153 184 214 245 275 306 337) ,Monat)))
  (form1))

(muffle-cerrors
  (form1))

(multiple-value-bind (x y)
    (floor a b)
  (values y x))

(multiple-value-call #'%expand-cons (rest form)
  (second form) nil
  (%expand-list (cddr form))

(multiple-value-list
  (floor a b))

(multiple-value-prog1
    (floor a b)
  (foobar))

(multiple-value-setq
    (SM1 SM2 SM3 SM4 SM5)
  (get-setf-method (car form)))

(nth-value 1
  (floor a b))

(or (cond1) (cond2)
    (cond3)
    (cond4))

(or
  (cond1) (cond2)
  (cond3)
  (cond4))

(pop
  (form1))

(print-unreadable-object (class stream :type t)
  (write (class-classname class) :stream stream))

(prog (a b
       c d)
  retry
  (multiple-value-setq (c d)
    (floor a b))
  (if (zerop d)
    (return))
  (go retry))

(prog* (a b
        c d)
  retry
  (multiple-value-setq (c d)
    (floor a b))
  (if (zerop d)
    (return))
  (go retry))

(prog1
  (form1)
  (form2))

(prog2
  (form1)
  (form2)
  (form3))

(progn (form1)
       (form2))

(progn
  (form1)
  (form2))

(progv
    (vars)
    (vals)
  (form1)
  (form2))

(progv (vars) (vals)
  (form1)
  (form2))

(psetf a (get-a)
       b (get-b))

(psetf a
         (get-a)
       b
         (get-b))

(psetq a (get-a)
       b (get-b))

(psetq a
         (get-a)
       b
         (get-b))

(push (form1)
      (form2))

(push
  (form1)
  (form2))

(pushnew (form1)
         (form2))

(pushnew
  (form1)
  (form2))

(quote
  #(a b c))

(remf 'x
  'y)

(restart-bind ((nil *fun1*
                    :interactive-function *fun2*
                    :report-function *fun3*
                    :test-function *fun4*))
  (form1)
  (form2))

(restart-bind
    ((nil *fun1*
          :interactive-function *fun2*
          :report-function *fun3*
          :test-function *fun4*))
  (form1)
  (form2))

(restart-case
    (invoke-debugger condition)
  (continue (&optional x
             &aux y)
    (form1)
    (form2)))

(return
  (form1))

(return-from
  nil
  (form1))

(rotatef (aref x i)
         (aref y i))

(rotatef
  (aref x i)
  (aref y i))

(setf a (get-a)
      b (get-b))

(setf a
        (get-a)
      b
        (get-b))

(setq a (get-a)
      b (get-b))

(setq a
        (get-a)
      b
        (get-b))

(shiftf (aref x i)
        (aref y i)
        (aref z i))

(shiftf
  (aref x i)
  (aref y i)
  (aref z i))

(space
  (form1))

(step
  (form1))

(symbol-macrolet
    ((x (slot-value obj 'x))
     (y
       (slot-value obj 'y)))
  (form1))

(symbol-macrolet ((x (slot-value obj 'x))
                  (y
                    (slot-value obj 'y)))
  (form1))

(tagbody
  retry
  (multiple-value-setq (c d)
    (floor a b))
  (if (zerop d)
    (return))
  (go retry))

(the integer
  (form1))

(the-environment)

(throw
    'exit
  (form1))

(time
  (form1))

(trace foo
       bar)

(trace
  foo
  bar)

(typecase (form1)
  (integer
    (form2) (form3)
    (form3)))

(unless
    (cond1)
  (form1)
  (form2))

(untrace foo
         bar)

(untrace
  foo
  bar)

(unwind-protect
  (foo)
  (close s))

(when
    (cond1)
  (form1)
  (form2))

(with-accessors ((x1 thing-x) (y1 thing-y)
                 (z1 thing-z))
    thing1
  (form1))

(with-condition-restarts
    (conds)
    (restarts)
  (form1))

(with-hash-table-iterator
    (name table)
  (form1))

(with-hash-table-iterator (name table)
  (form1))

(with-input-from-string
    (x s :start 3)
  (form1))

(with-keyboard
  (form1))

(with-open-file (s "foobar"
                 :direction :input)
  (form1))

(with-open-stream (s stream)
  (form1))

(with-output-to-printer (s
                         :external-format charset:iso-8859-1)
  (form1))

(with-output-to-string (s str
                        :element-type 'character)
  (form1))

(with-package-iterator (name pack)
  (form1))

(with-restarts
    ((continue (&optional x
                &aux y)
       (form1)
       (form2)))
  (invoke-debugger condition))

(with-simple-restart
    (x error
     "error")
  (form1))

(with-slots ((x1 x) (y1 y)
             (z1 z))
    thing1
  (form1))

(with-standard-io-syntax
  (form1))

(without-floating-point-underflow
  (expt 10 x))

;; Package SYSTEM
constant-eql
defformat-simple 
ds-slot-default
ds-slot-initer
ds-slot-offset
ds-slot-readonly
ds-slot-type
formatter-bind-args
formatter-bind-terminator
formatter-bind-terminators 
macro-expander
memq
multiple-value-setf
spd

;; Package FFI

bitsizeof
c-lines
cast
def-c-call-in
def-c-call-out
def-c-enum
def-c-struct
def-c-type
def-c-var
def-call-in
def-call-out
deref
element
sizeof
slot
typeof

;; Package SCREEN

with-window
