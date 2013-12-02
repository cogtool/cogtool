;;; Condition System for CLISP
;;; David Gadbois <gadbois@cs.utexas.edu> 30.11.1993
;;; Bruno Haible 24.11.1993, 2.12.1993 -- 2005
;;; Sam Steingold 1998-2005

(in-package "COMMON-LISP")
;;; exports:
(export
 '(;; types:
   restart condition serious-condition error program-error control-error
   arithmetic-error division-by-zero floating-point-overflow
   floating-point-underflow floating-point-inexact
   floating-point-invalid-operation
   cell-error unbound-variable undefined-function unbound-slot
   type-error package-error print-not-readable parse-error stream-error
   end-of-file reader-error file-error storage-condition warning
   style-warning simple-condition simple-error simple-type-error simple-warning
   ;; macros:
   define-condition handler-bind ignore-errors handler-case
   with-condition-restarts restart-bind restart-case
   with-simple-restart check-type assert etypecase ctypecase ecase ccase
   ;; functions:
   make-condition arithmetic-error-operation arithmetic-error-operands
   cell-error-name unbound-slot-instance type-error-datum
   type-error-expected-type package-error-package print-not-readable-object
   stream-error-stream file-error-pathname
   simple-condition-format-control simple-condition-format-arguments
   signal restart-name compute-restarts find-restart invoke-restart
   invoke-restart-interactively invoke-debugger break error cerror warn
   ;; functions and restart names:
   abort continue muffle-warning store-value use-value
   ;; variables:
   *break-on-signals* *debugger-hook*))
;; extensions:
(in-package "EXT")
(export
 '(muffle-cerrors appease-cerrors exit-on-error with-restarts os-error
   abort-on-error set-global-handler without-global-handlers
   source-program-error source-program-error-form source-program-error-detail
   simple-condition-format-string simple-charset-type-error retry)
 "EXT")
(in-package "CUSTOM")
(common-lisp:export
 '(*break-on-warnings* *report-error-print-backtrace*)
 "CUSTOM")
(ext:re-export "CUSTOM" "EXT")
(common-lisp:in-package "SYSTEM")

;;; Overview of Concepts

;; A condition is some information about an exceptional situation the program
;; cannot or does not want handle locally.
;; A handler is some code that tries to do recovery from exceptional situations
;; that happen elsewhere, or that decides to transfer control.
;; A restart is a point where control may be transferred to, together with a
;; description what is about to happen in this case.

;;; The CONDITION type

; The condition type system is integrated with CLOS.
(clos:defclass condition () ())

;; 29.3.18. Printing Conditions when *print-escape*
;;          and *print-readably* are NIL.
;; <http://www.lisp.org/HyperSpec/Body/sec_9-1-3.html>
(definternational print-condition-format
  (t ENGLISH))
(deflocalized print-condition-format ENGLISH
  (formatter "Condition of type ~S."))
(clos:defmethod clos:print-object ((condition condition) stream)
  (if (or *print-escape* *print-readably*)
    (clos:call-next-method)
    (progn
      (format stream (localized 'print-condition-format) (type-of condition))
      condition)))
; Entry-points used by other parts of CLISP.
(defun print-condition (condition stream)
  (let ((*print-escape* nil)
        (*print-readably* nil))
    (print-object condition stream)))
(defun pretty-print-condition (condition stream &key (text-indent 6))
  (with-fill-stream (out stream :text-indent text-indent)
    (print-condition condition out)))

;;; 29.4.5. Defining Conditions
;;; <http://www.lisp.org/HyperSpec/Body/sec_9-1-2.html>
;; DEFINE-CONDITION, CLtL2 p. 898
(defmacro define-condition (&whole whole-form
                            name parent-types slot-specs &rest options)
  (unless (symbolp name)
    (error-of-type 'source-program-error
      :form whole-form
      :detail name
      (TEXT "~S: the name of a condition must be a symbol, not ~S")
      'define-condition name))
  (unless (and (listp parent-types) (every #'symbolp parent-types))
    (error-of-type 'source-program-error
      :form whole-form
      :detail parent-types
      (TEXT "~S: the parent-type list must be a list of symbols, not ~S")
      'define-condition parent-types))
  (unless (listp slot-specs)
    (error-of-type 'source-program-error
      :form whole-form
      :detail slot-specs
      (TEXT "~S: the slot description list must be a list, not ~S")
      'define-condition slot-specs))
  (let ((default-initargs-option nil)
        (docstring-option nil)
        (report-function nil))
    (dolist (option options)
      (if (listp option)
        (cond ((and (eq (car option) ':DEFAULT-INITARGS) (oddp (length option)))
               (setq default-initargs-option option))
              ((and (keywordp (car option)) (eql (length option) 2))
               (case (first option)
                 (:DOCUMENTATION (setq docstring-option option))
                 (:REPORT (setq report-function (rest option)))
                 (T (error-of-type 'source-program-error
                      :form whole-form
                      :detail (first option)
                      (TEXT "~S ~S: unknown option ~S")
                      'define-condition name (first option)))))
              (t
               (error-of-type 'source-program-error
                 :form whole-form
                 :detail option
                 (TEXT "~S ~S: invalid syntax in ~S option: ~S")
                 'define-condition name 'define-condition option)))
        (error-of-type 'source-program-error
          :form whole-form
          :detail option
          (TEXT "~S ~S: not a ~S option: ~S")
          'define-condition name 'define-condition option)))
    (let ((defclass-form
            `(CLOS:DEFCLASS ,name
               ,(or parent-types '(CONDITION))
               ,slot-specs
               ,@(if docstring-option `(,docstring-option))
               ,@(if default-initargs-option `(,default-initargs-option)))))
      `(PROGN
         ,defclass-form
         ,@(when report-function
             `((CLOS:DEFMETHOD PRINT-OBJECT ((CONDITION ,name) STREAM)
                 (IF (OR *PRINT-ESCAPE* *PRINT-READABLY*)
                   (CLOS:CALL-NEXT-METHOD)
                   (PROGN
                     ,(if (stringp (first report-function))
                       `(WRITE-STRING ,(first report-function) STREAM)
                       `(FUNCALL (FUNCTION ,@report-function) CONDITION STREAM))
                     CONDITION)))))
         ',name))))

;;; 29.4.6. Creating Conditions

(defun find-subclasses-of-type (type class)
  "Find all subclasses of CLASS that are subtypes of the given TYPE."
  (if (subtypep class type)
      (list class)
      (delete-duplicates
       (loop :for c :in (clos:class-direct-subclasses class)
         :nconc (find-subclasses-of-type type c)))))

#+(or) ; not used
(defun prune-subclasses (classes)
  "Delete classes that are subclasses of other classes."
  (do ((tail classes (cdr tail)) this)
      ((endp tail) (delete nil classes))
    (setq this (car tail))
    (when (loop :for c :in classes
            ;; when THIS is a subclass of C, remove THIS
            :thereis (and c (not (eq this c)) (clos::subclassp this c)))
      (setf (car tail) nil))))

;; MAKE-CONDITION, CLtL2 p. 901
(defun make-condition (type &rest slot-initializations)
  (unless (subtypep type 'condition)
    (error-of-type 'error
      (TEXT "~S: type ~S is not a subtype of ~S")
      'make-condition type 'condition))
  (let ((class (or (and (symbolp type) (find-class type nil))
                   ;; not a specific class - find a maximal subclass of
                   ;; CONDITION that has the given TYPE
                   (car (last (sort (find-subclasses-of-type
                                     type (find-class 'condition))
                                    #'clos::subclassp))))))
    (if class
        (apply #'clos:make-instance class slot-initializations)
        (error-of-type 'error
          (TEXT "~S: cannot find a ~S class that is a subtype of ~S")
                'make-condition 'condition type))))

;; canonicalize a condition argument, CLtL2 p. 888

(defun try-coerce-to-condition (datum arguments
                                caller-name
                                default-type &rest more-initargs)
  (typecase datum
    (condition
      (when arguments
        (unless (eq caller-name 'cerror)
          (error-of-type 'type-error
            :datum arguments :expected-type 'null
            (TEXT "~S ~S: superfluous arguments ~S")
            caller-name datum arguments)))
      datum)
    (symbol
      (apply #'make-condition datum arguments))
    ((or string function) ; only this case uses default-type and more-initargs
      (apply #'make-condition default-type
             :format-control datum
             :format-arguments arguments
             more-initargs))
    (t
      (error-of-type 'type-error
        :datum datum :expected-type '(or condition symbol string function)
        (TEXT "~S: the condition argument must be a string, a symbol or a condition, not ~S")
        caller-name datum))))

(defun valid-condition-designator-p (datum+arguments)
  (handler-case
      (try-coerce-to-condition (car datum+arguments) (cdr datum+arguments)
                               'coerce-to-condition 'simple-error) ; hmmpf
    (ERROR () nil)
    (:NO-ERROR (&rest values) (declare (ignore values)) t)))

(defun coerce-to-condition (datum arguments
                            caller-name
                            default-type &rest more-initargs)
  (handler-case
      (apply #'try-coerce-to-condition datum arguments
             caller-name default-type more-initargs)
    (TYPE-ERROR (error) (signal error))
    (ERROR (error)
      ;; ANSI CL wants a type error here.
      (error-of-type 'type-error
        :datum (cons datum arguments)
        :expected-type '(satisfies valid-condition-designator-p)
        "~A" error))))

;;; 29.5. Predefined Condition Types

; Hierarchy:
;
;   condition
;   |
;   |-- simple-condition
;   |
;   |-- serious-condition
;   |   |
;   |   |-- error
;   |   |   |
;   |   |   |-- simple-error
;   |   |   |
;   |   |   |-- arithmetic-error
;   |   |   |   |
;   |   |   |   |-- division-by-zero
;   |   |   |   |
;   |   |   |   |-- floating-point-overflow
;   |   |   |   |
;   |   |   |   |-- floating-point-underflow
;   |   |   |   |
;   |   |   |   |-- floating-point-inexact
;   |   |   |   |
;   |   |   |   |-- floating-point-invalid-operation
;   |   |   |
;   |   |   |-- cell-error
;   |   |   |   |
;   |   |   |   |-- unbound-variable
;   |   |   |   |
;   |   |   |   |-- undefined-function
;   |   |   |   |
;   |   |   |   |-- unbound-slot
;   |   |   |
;   |   |   |-- control-error
;   |   |   |
;   |   |   |-- file-error
;   |   |   |
;   |   |   |-- os-error
;   |   |   |
;   |   |   |-- package-error
;   |   |   |
;   |   |   |-- print-not-readable
;   |   |   |
;   |   |   |-- program-error
;   |   |   |   |
;   |   |   |   +---------------------------------------+
;   |   |   |                                           |
;   |   |   |-- parse-error                             |
;   |   |   |   |                                       |
;   |   |   |   +-------------------+                   |
;   |   |   |                       |                   |
;   |   |   |-- stream-error        |                   |
;   |   |   |   |                   |                   |
;   |   |   |   |-- end-of-file     |                   |
;   |   |   |   |                   |                   |
;   |   |   |   +-------------------+-- reader-error    |
;   |   |   |                                           |
;   |   |   |-- type-error                              |
;   |   |       |                                       |
;   |   |       |-- simple-type-error                   |
;   |   |       |                                       |
;   |   |       +---------------------------------------+-- argument-list-dotted
;   |   |
;   |   |-- storage-condition
;   |   |
;   |   |-- interrupt-condition
;   |
;   |-- warning
;       |
;       |-- simple-warning
;       |
;       |-- style-warning
;

;; X3J13 writeup <CONDITION-SLOTS:HIDDEN> wants the slot names to be hidden,
;; (e.g. no slot named `package', `stream', `pathname'), hence we prepend $.

; conditions that require interactive intervention
(define-condition serious-condition () ())

  ; serious conditions that occur deterministically
  (define-condition error (serious-condition) ())

    ; mostly statically detectable errors of a program
    (define-condition program-error (error) ())
    ; all the other errors must be detected by the runtime system

      ; statically detectable errors of a program, source available
      (define-condition source-program-error (program-error)
        (;; The "outer-most" bad form, i.e. the list whose first element is the
         ;; macro or special-operator name.
         ($form   :initarg :form   :reader source-program-error-form)
         ;; The "inner-most" detail of the bad form, e.g., when a string is
         ;; given as a variable name, this is the string, not the whole form.
         ($detail :initarg :detail :reader source-program-error-detail)))
      ; CLISP specific

    ; not statically detectable errors in program control
    (define-condition control-error (error) ())

    ; errors that occur while doing arithmetic operations
    (define-condition arithmetic-error (error)
      (($operation :initarg :operation :reader arithmetic-error-operation)
       ($operands  :initarg :operands  :reader arithmetic-error-operands)))

      ; trying to evaluate a mathematical function at a singularity
      (define-condition division-by-zero (arithmetic-error) ())

      ; trying to get too close to infinity in the floating point domain
      (define-condition floating-point-overflow (arithmetic-error) ())

      ; trying to get too close to zero in the floating point domain
      (define-condition floating-point-underflow (arithmetic-error) ())

      (define-condition floating-point-inexact (arithmetic-error) ())

      (define-condition floating-point-invalid-operation (arithmetic-error) ())

    ; trying to access a location which contains #<UNBOUND>
    (define-condition cell-error (error)
      (($name :initarg :name :reader cell-error-name)))

      ; trying to get the value of an unbound variable
      (define-condition unbound-variable (cell-error) ())

      ; trying to get the global function definition of an undefined function
      (define-condition undefined-function (cell-error) ())

      ; trying to get the value of an unbound slot
      (define-condition unbound-slot (cell-error)
        (($instance :initarg :instance :reader unbound-slot-instance)))

    ; when some datum does not belong to the expected type
    (define-condition type-error (error)
      (($datum         :initarg :datum         :reader type-error-datum)
       ($expected-type :initarg :expected-type :reader type-error-expected-type)))

      ; when some keyword does not belong to one of the allowed keywords
      ; ANSI CL 3.5.1.4., 3.5.1.5. want this to be a subclass of PROGRAM-ERROR.
      (define-condition keyword-error (program-error type-error) ())
      ; CLISP specific

      ; when some character does not belong to a given character set
      (define-condition charset-type-error (type-error) ())
      ; CLISP specific

      ; when an argument list in APPLY is dotted
      ; ANSI CL 3.5.1.2. want this to be a subclass of PROGRAM-ERROR.
      ; For the use of APPLY in the expansion of the FORMATTER macro, this
      ; must also be a subclass of TYPE-ERROR.
      (define-condition argument-list-dotted (program-error type-error) ())
      ; CLISP specific

    ; errors during operation on packages
    (define-condition package-error (error)
      (($package :initarg :package :reader package-error-package)))

    ; attempted violation of *PRINT-READABLY*
    (define-condition print-not-readable (error)
      (($object :initarg :object :reader print-not-readable-object)))

    ; errors related to parsing
    (define-condition parse-error (error) ())

    ; errors while doing stream I/O
    (define-condition stream-error (error)
      (($stream :initarg :stream :reader stream-error-stream)))

      ; unexpected end of stream
      (define-condition end-of-file (stream-error) ())

      ; parsing/tokenization error during READ
      (define-condition reader-error (parse-error stream-error) ())

    ; errors with pathnames, OS level errors with streams
    (define-condition file-error (error)
      (($pathname :initarg :pathname :reader file-error-pathname)))

    ; general OS errors
    (define-condition os-error (error) ())
    ; CLISP specific

  ; "Virtual memory exhausted"
  (define-condition storage-condition (serious-condition) ())

  ; "User break"
  (define-condition interrupt-condition (serious-condition) ())
  ; CLISP specific

; conditions for which user notification is appropriate
(define-condition warning () ())

  ; conditions which are a matter of programming style (not serious)
  (define-condition style-warning (warning) ())

;; These shouldn't be separate types but we cannot adjoin slots without
;; defining subtypes.

; conditions usually created by SIGNAL
(define-condition simple-condition ()
  (($format-control :initarg :format-control :initform nil
                    :reader simple-condition-format-string ; for CLtL2 backward compatibility
                    :reader simple-condition-format-control)
   ($format-arguments :initarg :format-arguments :initform nil
                      :reader simple-condition-format-arguments))
  #|
  (:report
    (lambda (condition stream)
      (let ((fstring (simple-condition-format-control condition)))
        (when fstring
          (apply #'format stream fstring
                 (simple-condition-format-arguments condition))))))
  |#
)
;; We don't use the :report option here. Instead we define a print-object
;; method which will be executed regardless of the condition type's CPL.
(clos:defmethod print-object :around ((condition simple-condition) stream)
  (if (or *print-escape* *print-readably*)
    (clos:call-next-method)
    (let ((fstring (simple-condition-format-control condition)))
      (if fstring
        (apply #'format stream fstring (simple-condition-format-arguments condition))
        (clos:call-next-method))))
  condition)

;; conditions usually created by ERROR or CERROR
(define-condition simple-error (simple-condition error) ())

;; conditions usually created by CHECK-TYPE
(define-condition simple-type-error (simple-condition type-error) ())

;; conditions usually created by WARN
(define-condition simple-warning (simple-condition warning) ())

;; All conditions created by the C runtime code are of type simple-condition.
;; Need the following types. Don't use them for discrimination.
(define-condition simple-serious-condition (simple-condition serious-condition) ())
(define-condition simple-program-error (simple-error program-error) ())
(define-condition simple-source-program-error (simple-error source-program-error) ())
(define-condition simple-control-error (simple-error control-error) ())
(define-condition simple-arithmetic-error (simple-error arithmetic-error) ())
(define-condition simple-division-by-zero (simple-error division-by-zero) ())
(define-condition simple-floating-point-overflow (simple-error floating-point-overflow) ())
(define-condition simple-floating-point-underflow (simple-error floating-point-underflow) ())
(define-condition simple-cell-error (simple-error cell-error) ())
(define-condition simple-unbound-variable (simple-error unbound-variable) ())
(define-condition simple-undefined-function (simple-error undefined-function) ())
(define-condition simple-unbound-slot (simple-error unbound-slot) ())
(define-condition simple-keyword-error (simple-error keyword-error) ())
(define-condition simple-charset-type-error (simple-error charset-type-error) ())
(define-condition simple-argument-list-dotted (simple-error argument-list-dotted) ())
(define-condition simple-package-error (simple-error package-error) ())
(define-condition simple-print-not-readable (simple-error print-not-readable) ())
(define-condition simple-parse-error (simple-error parse-error) ())
(define-condition simple-stream-error (simple-error stream-error) ())
(define-condition simple-end-of-file (simple-error end-of-file) ())
(define-condition simple-reader-error (simple-error reader-error) ())
(define-condition simple-file-error (simple-error file-error) ())
(define-condition simple-os-error (simple-error os-error) ())
(define-condition simple-storage-condition (simple-condition storage-condition) ())
(define-condition simple-interrupt-condition (simple-condition interrupt-condition) ())

;; for NO-APPLICABLE-METHOD, NO-PRIMARY-METHOD, NO-NEXT-METHOD
(define-condition method-call-error (simple-error)
  (($gf :initarg :generic-function :reader method-call-error-generic-function)
   ($method :initarg :method :reader method-call-error-method)
   ($args :initarg :argument-list :reader method-call-error-argument-list)))
(define-condition method-call-type-error
    (simple-type-error method-call-error) ())

;; Bootstrapping
(%defclcs
 ;; The order of the types in this vector must be the same as in lispbibl.d.
 '#((condition                . simple-condition)
    (serious-condition        . simple-serious-condition)
    (error                    . simple-error)
    (program-error            . simple-program-error)
    (source-program-error     . simple-source-program-error)
    (control-error            . simple-control-error)
    (arithmetic-error         . simple-arithmetic-error)
    (division-by-zero         . simple-division-by-zero)
    (floating-point-overflow  . simple-floating-point-overflow)
    (floating-point-underflow . simple-floating-point-underflow)
    (cell-error               . simple-cell-error)
    (unbound-variable         . simple-unbound-variable)
    (undefined-function       . simple-undefined-function)
    (unbound-slot             . simple-unbound-slot)
    (type-error               . simple-type-error)
    (keyword-error            . simple-keyword-error)
    (charset-type-error       . simple-charset-type-error)
    (argument-list-dotted     . simple-argument-list-dotted)
    (package-error            . simple-package-error)
    (print-not-readable       . simple-print-not-readable)
    (parse-error              . simple-parse-error)
    (stream-error             . simple-stream-error)
    (end-of-file              . simple-end-of-file)
    (reader-error             . simple-reader-error)
    (file-error               . simple-file-error)
    (os-error                 . simple-os-error)
    (storage-condition        . simple-storage-condition)
    (interrupt-condition      . simple-interrupt-condition)
    (warning                  . simple-warning)))


;;; Handling and Signalling - Primitives

(defvar *break-on-signals* nil)

#|
;; This would be a possible implementation. However, it forces too many
;; variables into closures although in the most frequent case - no condition
;; at all - they won't be needed. Furthermore, it conses too much.

;; List of active invocations of HANDLER-BIND.
 (defvar *handler-clusters* '())

;; HANDLER-BIND, CLtL2 p. 898
 (defmacro handler-bind (clauses &body body)
  `(LET ((*HANDLER-CLUSTERS*
           (CONS
             (LIST ,@(mapcar #'(lambda (clause)
                                 (let ((type (first clause))
                                       (function-form (second clause)))
                                   `(CONS ',type ,function-form)))
                             clauses))
             *HANDLER-CLUSTERS*)))
     (PROGN ,@body)))

;; SIGNAL, CLtL2 p. 888
 (defun signal (datum &rest arguments)
  (let ((condition
         ;; CLtL2 p. 918 specifies this
         (coerce-to-condition datum arguments 'signal 'simple-condition)))
    (when (typep condition *break-on-signals*)
      ; Enter the debugger prior to signalling the condition
      (restart-case (invoke-debugger condition)
        (CONTINUE ())))
    ; CLtL2 p. 884: "A handler is executed in the dynamic context of the
    ; signaler, except that the set of available condition handlers will
    ; have been rebound to the value that was active at the time the condition
    ; handler was made active."
    (let ((*handler-clusters* *handler-clusters*))
      (loop
        (when (null *handler-clusters*) (return))
        (dolist (handler (pop *handler-clusters*))
          (when (typep condition (car handler))
            (funcall (cdr handler) condition)
            (return)))))
    nil))

|#

;; HANDLER-BIND, CLtL2 p. 898
;; Since we can build handler frames only in compiled code
;; there is SYS::%HANDLER-BIND which is synonymous to HANDLER-BIND except
;; that SYS::%HANDLER-BIND only occurs in compiled code.
(defmacro handler-bind (clauses &body body)
  (let* ((typespecs (mapcar #'first clauses))
         (handlers (nconc (mapcar #'rest clauses) (list body)))
         (handler-vars (gensym-list (length handlers))))
    `(LET ,(mapcar #'list
                   handler-vars
                   (mapcar #'(lambda (handler)
                               `(FUNCTION (LAMBDA () (PROGN ,@handler))))
                           handlers))
       (LOCALLY (DECLARE (COMPILE))
         (SYS::%HANDLER-BIND
          ,(car (last handler-vars))
          ,@(mapcan #'(lambda (typespec handler-var)
                        `(',typespec #'(LAMBDA (CONDITION)
                                         (FUNCALL (FUNCALL ,handler-var)
                                                  CONDITION))))
                    typespecs handler-vars))))))

;; SIGNAL, CLtL2 p. 888
;; is in error.d


;;; Handling and Signalling - Part 2

;; IGNORE-ERRORS, CLtL2 p. 897
(defmacro ignore-errors (&body body)
  (let ((blockname (gensym)))
    `(BLOCK ,blockname
       (HANDLER-BIND
         ((ERROR #'(LAMBDA (CONDITION)
                     (RETURN-FROM ,blockname (VALUES NIL CONDITION)))))
         ,@body))))

;; HANDLER-CASE, CLtL2 p. 895
(defmacro handler-case (&whole whole-form
                        form &rest clauses)
  ;; split off the :NO-ERROR clause and
  ;; add a GO tag to the other clauses (type varlist . body)
  (let ((no-error-clause nil) ; the :no-error clause, if found
        (extended-clauses '())) ; ((tag type varlist . body) ...)
    (do ()
        ((endp clauses))
      (let ((clause (pop clauses)))
        (block check-clause
          (unless (and (consp clause) (consp (cdr clause))
                       (listp (second clause)))
            (error-of-type 'source-program-error
              :form whole-form
              :detail clause
              (TEXT "~S: illegal syntax of clause ~S")
              'handler-case clause))
          (when (eq (first clause) ':no-error)
            (if (null no-error-clause)
              (setq no-error-clause clause)
              (warn (TEXT "~S: multiple ~S clauses: ~S and ~S")
                    'handler-case ':no-error clause no-error-clause))
            (return-from check-clause))
          (let ((varlist (second clause))) ; known to be a list
            (unless (null (cdr varlist))
              (error-of-type 'source-program-error
                :form whole-form
                :detail varlist
                (TEXT "~S: too many variables ~S in clause ~S")
                'handler-case varlist clause)))
          (push (cons (gensym) clause) extended-clauses))))
    (setq extended-clauses (nreverse extended-clauses))
    (let ((blockname (gensym))
          (tempvar (gensym)))
      `(BLOCK ,blockname
         (LET (,tempvar) ; tempvar is IGNORABLE since it is a gensym
           (TAGBODY
             (RETURN-FROM ,blockname
               ,(let ((main-form
                        `(HANDLER-BIND
                           ,(mapcar #'(lambda (xclause)
                                        (let ((tag (first xclause))
                                              (type (first (rest xclause)))
                                              (varlist (second (rest xclause))))
                                          `(,type
                                            #'(LAMBDA (CONDITION)
                                                ,(if (null varlist)
                                                   `(DECLARE (IGNORE CONDITION))
                                                   `(SETQ ,tempvar CONDITION))
                                                (GO ,tag)))))
                                    extended-clauses)
                           ,form)))
                  (if no-error-clause
                    `(MULTIPLE-VALUE-CALL #'(LAMBDA ,@(rest no-error-clause))
                       ,main-form)
                    main-form)))
             ,@(mapcap #'(lambda (xclause)
                           (let ((tag (first xclause))
                                 (varlist (second (rest xclause)))
                                 (body (cddr (rest xclause)))) ; may contain declarations
                             `(,tag
                               (RETURN-FROM ,blockname
                                 (LET ,(if (null varlist)
                                         '() `((,@varlist ,tempvar)))
                                   ,@body)))))
                       extended-clauses)))))))


;;; Restarts

;; This stuff is needed only once an exception has already occurred. No need
;; to optimize the hell out of it.

; The default test function for restarts always returns T. See CLtL2 p. 905,909.
(defun default-restart-test (condition)
  (declare (ignore condition))
  t)

; The default interactive function for restarts returns the empty argument list.
(defun default-restart-interactive ()
  '())

;; The RESTART type, CLtL2 p. 916
;; Also defines RESTART-NAME, CLtL2 p. 911
;; Also defines MAKE-RESTART, ABI
(defstruct restart
  name             ; its name, or NIL if it is not named
  (test #'default-restart-test) ; function that tests whether this restart
                                ; applies to a given condition
  (invoke-tag nil) ; tag used to invoke the restart, or nil
  invoke-function  ; function used to invoke the restart, if invoke-tag is nil
  (report nil)     ; function used to print a description of the restart
  (interactive #'default-restart-interactive)
                   ; function used to gather additional data from the user
                   ; before invoking the restart
  (meaningfulp t)  ; whether it makes sense to invoke this restart without
                   ; prior corrective action
)
#| ; We could also define it as a CLOS class:
 (clos:defclass restart ()
  (name            :initarg :name            :reader restart-name)
  (test            :initarg :test            :reader restart-test
                   :initform #'default-restart-test)
  (invoke-tag      :initarg :invoke-tag      :reader restart-invoke-tag
                   :initform nil)
  (invoke-function :initarg :invoke-function :reader restart-invoke-function)
  (report          :initarg :report          :reader restart-report
                   :initform nil)
  (interactive     :initarg :interactive     :reader restart-interactive
                   :initform #'default-restart-interactive)
  (meaningfulp     :initarg :meaningfulp     :reader restart-meaningfulp
                   :initform t))
|#

;; Printing restarts
(clos:defmethod clos:print-object ((restart restart) stream)
  (if (or *print-escape* *print-readably*)
    (clos:call-next-method)
    (let ((report-function (restart-report restart)))
      (if report-function
        (funcall report-function stream)
        (prin1 (restart-name restart) stream))))
  restart)

;; Expands to the equivalent of `(MAKE-RESTART :NAME name ...)
;; but makes intelligent use of the defaults to reduce code size.
(defun make-restart-form (name test invoke-tag invoke-function report interactive meaningfulp)
  `(MAKE-RESTART
     :NAME ,name
     ,@(if (not (equal test '(FUNCTION DEFAULT-RESTART-TEST)))
         `(:TEST ,test))
     ,@(if (not (equal invoke-tag 'NIL))
         `(:INVOKE-TAG ,invoke-tag))
     :INVOKE-FUNCTION ,invoke-function
     ,@(if (not (equal report 'NIL))
         `(:REPORT ,report))
     ,@(if (not (equal interactive '(FUNCTION DEFAULT-RESTART-INTERACTIVE)))
         `(:INTERACTIVE ,interactive))
     ,@(if (not (equal meaningfulp 'T))
         `(:MEANINGFULP ,meaningfulp))))

;; The list of active restarts.
(defvar *active-restarts* nil) ; ABI

;; A list of pairs of conditions and restarts associated with them. We have to
;; keep the associations separate because there can be a many-to-many mapping
;; between restarts and conditions, and this mapping has dynamic extent.
(defvar *condition-restarts* nil) ; ABI

; Add an association between a condition and a couple of restarts.
(defun add-condition-restarts (condition restarts) ; ABI
  (dolist (restart restarts)
    (push (cons condition restart) *condition-restarts*)))

;; WITH-CONDITION-RESTARTS, CLtL2 p. 910
(defmacro with-condition-restarts (condition-form restarts-form &body body)
  `(LET ((*CONDITION-RESTARTS* *CONDITION-RESTARTS*))
     (ADD-CONDITION-RESTARTS ,condition-form ,restarts-form)
     ;; ANSI CL does not allow declarations at the beginning of the body, but
     ;; we do, as an extension.
     (LET () ,@body)))

;;; 29.4.8. Finding and Manipulating Restarts

; Tests whether a given restart is applicable to a given condition
(defun applicable-restart-p (restart condition)
  (and
   (or (null condition)
       ;; A restart is applicable if it is associated to that condition
       ;; or if it is not associated to any condition.
       (let ((not-at-all t))
         (dolist (asso *condition-restarts* not-at-all)
           (when (eq (cdr asso) restart)
             (if (eq (car asso) condition)
                 (return t)
                 (setq not-at-all nil))))))
   ;; Call the restart's test function:
   (funcall (restart-test restart) condition)))

;; COMPUTE-RESTARTS, CLtL2 p. 910
(defun compute-restarts (&optional condition)
  (remove-if-not #'(lambda (restart) (applicable-restart-p restart condition))
                 *active-restarts*))

;; FIND-RESTART, CLtL2 p. 911
; returns a restart or nil
(defun find-restart (restart-identifier &optional condition)
  ;; cannot use (E)TYPECASE for bootstrapping reasons
  (cond ((null restart-identifier)
         (error-of-type 'error
           (TEXT "~S: ~S is not a valid restart name here. Use ~S instead.")
           'find-restart restart-identifier 'compute-restarts))
        ((symbolp restart-identifier)
         ;;(find restart-identifier *active-restarts*
         ;;      :test (lambda (ri restart)
         ;;              (and (eq (restart-name restart) ri)
         ;;                   (applicable-restart-p restart condition))))
         (dolist (restart *active-restarts*)
           (when (and (eq (restart-name restart) restart-identifier)
                      (applicable-restart-p restart condition))
             (return restart))))
        ((typep restart-identifier 'restart)
         ;;(find restart-identifier *active-restarts*
         ;;      :test (lambda (ri restart)
         ;;              (and (eq restart ri)
         ;;                   (applicable-restart-p restart condition))))
         (dolist (restart *active-restarts*)
           (when (and (eq restart restart-identifier)
                      (applicable-restart-p restart condition))
             (return restart))))
        (t (error-of-type 'type-error
             :datum restart-identifier :expected-type '(or symbol restart)
             (TEXT "~S: invalid restart name ~S")
             'find-restart restart-identifier))))

(defun restart-not-found (restart-identifier)
  (error-of-type 'control-error
    (TEXT "~S: No restart named ~S is visible.")
    'invoke-restart restart-identifier))

(defun %invoke-restart (restart arguments)
  (if (restart-invoke-tag restart)
    (throw (restart-invoke-tag restart) arguments)
    (apply (restart-invoke-function restart) arguments)
    ;; This may return normally, the restart need not transfer control.
    ;; (See CLtL2 p. 880.)
    ))

;; INVOKE-RESTART, CLtL2 p. 911
(defun invoke-restart (restart-identifier &rest arguments)
  (let ((restart (find-restart restart-identifier)))
    (unless restart (restart-not-found restart-identifier))
    (%invoke-restart restart arguments)))

(defun invoke-restart-condition (restart-identifier condition &rest arguments)
  (let ((restart (find-restart restart-identifier condition)))
    (unless restart (restart-not-found restart-identifier))
    (%invoke-restart restart arguments)))

(defun invoke-restart-condition-if-exists (restart-identifier condition &rest arguments)
  (let ((restart (find-restart restart-identifier condition)))
    (when restart
      (%invoke-restart restart arguments))))

;; INVOKE-RESTART-INTERACTIVELY, CLtL2 p. 911
(defun invoke-restart-interactively (restart-identifier)
  (let ((restart (find-restart restart-identifier)))
    (unless restart (restart-not-found restart-identifier))
    (let ((arguments (funcall (restart-interactive restart))))
      (%invoke-restart restart arguments))))

;;; 29.4.7. Establishing Restarts

;; This conses out the wazoo, but there seems to be no good way around it short
;; of special casing things a zillion ways.  The main problem is that someone
;; could write:
;;
;; (restart-bind ((nil *fun-1*
;;                     :interactive-function *fun-2*
;;                     :report-function *fun-3*
;;                     :test-function *fun-4*
;;                 )) ...)
;;
;; and it is supposed to work.

;; RESTART-BIND, CLtL2 p. 909
(defmacro restart-bind (&whole whole-form
                        restart-specs &body body)
  (setq body `(PROGN ,@body))
  (unless (listp restart-specs)
    (error-of-type 'source-program-error
      :form whole-form
      :detail restart-specs
      (TEXT "~S: not a list: ~S")
      'restart-bind restart-specs))
  (if restart-specs
    `(LET ((*ACTIVE-RESTARTS*
            (LIST*
             ,@(mapcar #'(lambda (spec)
                           (unless (and (listp spec) (consp (cdr spec))
                                        (symbolp (first spec)))
                             (error-of-type 'source-program-error
                               :form whole-form
                               :detail spec
                               (TEXT "~S: invalid restart specification ~S")
                               'restart-bind spec))
                           (apply #'(lambda (name function
                                             &key (test-function '(FUNCTION DEFAULT-RESTART-TEST))
                                                  (interactive-function '(FUNCTION DEFAULT-RESTART-INTERACTIVE))
                                                  (report-function 'NIL)
                                                  ((meaningfulp meaningfulp) 'T))
                                      (when (and (null name) (eq report-function 'NIL))
                                        ;; CLtL2 p. 906: "It is an error if an unnamed restart is used
                                        ;; and no report information is provided."
                                        (error-of-type 'source-program-error
                                          :form whole-form
                                          :detail spec
                                          (TEXT "~S: unnamed restarts require ~S to be specified: ~S")
                                          'restart-bind ':REPORT-FUNCTION spec))
                                      (make-restart-form
                                        `',name
                                        test-function
                                        'NIL
                                        function
                                        report-function
                                        interactive-function
                                        meaningfulp))
                                  spec))
                       restart-specs)
             *ACTIVE-RESTARTS*)))
       ,body)
    body))

;; RESTART-CASE, CLtL2 p. 903
;; WITH-RESTARTS
;; Syntax: (RESTART-CASE form {restart-clause}*)
;;         (WITH-RESTARTS ({restart-clause}*) {form}*)
;; restart-clause ::=   (restart-name arglist {keyword value}* {form}*)
;;                    | (restart-name {keyword value}* arglist {form}*)

;; There are a number of special cases we could optimize for. If we
;; can determine that we will not have to cons any closures at
;; runtime, then we could statically allocate the list of restarts.
;;
;; Since we have to deal with the wacky way RESTART-CASE interacts with
;; WITH-CONDITION-RESTARTS, we do not go through RESTART-BIND.

(eval-when (compile load eval)
  (defun expand-restart-case (caller whole-form restart-clauses form)
    (unless (listp restart-clauses)
      (error-of-type 'source-program-error
        :form whole-form
        :detail restart-clauses
        (TEXT "~S: not a list: ~S")
        caller restart-clauses))
    (let ((xclauses ;; list of expanded clauses
           ;; ((tag name test interactive report lambdalist . body) ...)
           (mapcar
            #'(lambda (restart-clause &aux (clause restart-clause))
                (unless (and (consp clause) (consp (cdr clause))
                             (symbolp (first clause)))
                  (error-of-type 'source-program-error
                    :form whole-form
                    :detail clause
                    (TEXT "~S: invalid restart specification ~S")
                    caller clause))
                (let ((name (pop clause))
                      (passed-arglist nil)
                      (passed-keywords nil)
                      arglist
                      (keywords '()))
                  (loop
                    (when (endp clause) (return))
                    (cond ((and (not passed-arglist) (listp (first clause)))
                           (setq arglist (pop clause))
                           (setq passed-arglist t)
                           (when keywords (setq passed-keywords t)))
                          ((and (not passed-keywords) (consp (cdr clause))
                                (symbolp (first clause)))
                           (push (pop clause) keywords)
                           (push (pop clause) keywords))
                          (t (return))))
                  (unless passed-arglist
                    (error-of-type 'source-program-error
                      :form whole-form
                      :detail clause
                      (TEXT "~S: missing lambda list in restart specification ~S")
                      caller clause))
                  (multiple-value-bind (test interactive report meaningfulp)
                      (apply #'(lambda (&key (test 'DEFAULT-RESTART-TEST)
                                             (interactive 'DEFAULT-RESTART-INTERACTIVE)
                                             (report 'DEFAULT-RESTART-REPORT)
                                             ((meaningfulp meaningfulp) 'T))
                                 (values test interactive report meaningfulp))
                             (nreverse keywords))
                    (when (and (null name) (eq report 'DEFAULT-RESTART-REPORT))
                      ;; CLtL2 p. 906: "It is an error if an unnamed restart
                      ;; is used and no report information is provided."
                      (error-of-type 'source-program-error
                        :form whole-form
                        :detail restart-clause
                        (TEXT "~S: unnamed restarts require ~S to be specified: ~S")
                        caller ':REPORT restart-clause))
                    (when (and (consp arglist) (not (member (first arglist) lambda-list-keywords))
                               (eq interactive 'DEFAULT-RESTART-INTERACTIVE))
                      ;; restart takes required arguments but does not have an
                      ;; interactive function that will prompt for them.
                      (warn (TEXT "~S: restart cannot be invoked interactively because it is missing a ~S option: ~S")
                            caller ':INTERACTIVE restart-clause))
                    `(,(gensym) ,name ,test ,interactive ,report ,meaningfulp
                       ,arglist ,@clause))))
            restart-clauses))
          (blockname (gensym))
          (arglistvar (gensym))
          (associate
           ;; Yick.  As a pretty lame way of allowing for an association
           ;; between conditions and restarts, RESTART-CASE has to
           ;; notice if its body is signalling a condition, and, if so,
           ;; associate the restarts with the condition.
           (and (consp form)
                (case (first form)
                  ((SIGNAL ERROR CERROR WARN ERROR-OF-TYPE) t)
                  (t nil))
                (gensym))))
      `(BLOCK ,blockname
         (LET (,arglistvar) ; arglistvar is IGNORABLE since it is a gensym
           (TAGBODY
             ,(let ((restart-forms
                     (mapcar #'(lambda (xclause)
                                 (let ((tag (first xclause))
                                       (name (second xclause))
                                       (test (third xclause))
                                       (interactive (fourth xclause))
                                       (report (fifth xclause))
                                       (meaningfulp (sixth xclause)))
                                   (make-restart-form
                                     `',name
                                     `(FUNCTION ,test)
                                     'NIL
                                     `(FUNCTION (LAMBDA (&REST ARGUMENTS)
                                        (SETQ ,arglistvar ARGUMENTS)
                                        (GO ,tag)))
                                     (if (eq report 'DEFAULT-RESTART-REPORT)
                                       `NIL
                                       `(FUNCTION
                                          ,(if (stringp report)
                                             `(LAMBDA (STREAM)
                                                (WRITE-STRING ,report STREAM))
                                             report)))
                                     `(FUNCTION ,interactive)
                                     meaningfulp)))
                             xclauses))
                    (form `(RETURN-FROM ,blockname ,form)))
                 `(LET* ,(if associate
                           `((,associate (LIST ,@restart-forms))
                             (*ACTIVE-RESTARTS*
                              (APPEND ,associate *ACTIVE-RESTARTS*))
                             (*CONDITION-RESTARTS* *CONDITION-RESTARTS*))
                           `((*ACTIVE-RESTARTS*
                              (LIST* ,@restart-forms *ACTIVE-RESTARTS*))))
                    ,(if associate
                       #| ; This code resignals the condition in a different dynamic context!
                       `(LET ((CONDITION
                               (HANDLER-CASE ,form ; evaluate the form
                                (CONDITION (C) C)))) ; catch the condition
                         (WITH-CONDITION-RESTARTS CONDITION ,associate ; associate the condition with the restarts
                           (SIGNAL CONDITION))) ; resignal the condition
                         |#
                       #| ; This code invokes the debugger even if it shouldn't!
                       `(HANDLER-BIND
                         ((CONDITION ; catch the condition
                           #'(LAMBDA (CONDITION)
                              (WITH-CONDITION-RESTARTS CONDITION ,associate  ; associate the condition with the restarts
                               (SIGNAL CONDITION) ; resignal the condition
                               (INVOKE-DEBUGGER CONDITION))))) ; this is weird!
                         ,form)
                       |#
                       `(HANDLER-BIND
                            ((CONDITION ; catch the condition
                              #'(LAMBDA (CONDITION)
                                  (ADD-CONDITION-RESTARTS CONDITION ,associate) ; associate the condition with the restarts
                                  (SIGNAL CONDITION)))) ; resignal the condition
                         ,form)
                       form)))
             ,@(mapcap #'(lambda (xclause)
                           (let ((tag (first xclause))
                                 (lambdabody (cddddr (cddr xclause))))
                             `(,tag
                               (RETURN-FROM ,blockname
                                 (APPLY (FUNCTION (LAMBDA ,@lambdabody))
                                        ,arglistvar)))))
                       xclauses))))))
)

(defmacro restart-case (&whole whole-form
                        form &rest restart-clauses &environment env)
  (expand-restart-case 'restart-case whole-form restart-clauses
                       (macroexpand form env)))

(defmacro with-restarts (&whole whole-form
                         restart-clauses &body body &environment env)
  (expand-restart-case 'with-restarts whole-form restart-clauses
                       (if (cdr body)
                         (cons 'PROGN body)
                         (macroexpand (car body) env))))

;; WITH-SIMPLE-RESTART, CLtL2 p. 902
(defmacro with-simple-restart ((name format-string &rest format-arguments) &body body)
  (if (or format-arguments (not (constantp format-string)))
    `(WITH-RESTARTS
         ((,name
           :REPORT (LAMBDA (STREAM) (FORMAT STREAM ,format-string ,@format-arguments))
           () (VALUES NIL T)))
       ,@body)
    ;; Here's an example of how we can easily optimize things. There is no
    ;; need to refer to anything in the lexical environment, so we can avoid
    ;; consing a restart at run time.
    (let ((blockname (gensym))
          (tag (gensym)))
      `(BLOCK ,blockname
         (CATCH ',tag
           (LET ((*ACTIVE-RESTARTS*
                  (CONS
                   (LOAD-TIME-VALUE
                    (MAKE-RESTART :NAME ',name
                                  :INVOKE-TAG ',tag
                                  :REPORT #'(LAMBDA (STREAM)
                                              (FORMAT STREAM ,format-string))))
                   *ACTIVE-RESTARTS*)))
             (RETURN-FROM ,blockname (PROGN ,@body))))
         (VALUES NIL T)))))


;;; 29.4.10. Restart Functions

;; These functions are customary way to pass control from a handler to a
;; restart. They just invoke the restart of the same name.

;; ABORT, CLtL2 p. 913
(defun abort (&optional condition)
  (invoke-restart-condition 'ABORT condition))

;; CONTINUE, CLtL2 p. 913
(defun continue (&optional condition)
  (invoke-restart-condition-if-exists 'CONTINUE condition))

;; MUFFLE-WARNING, CLtL2 p. 913
(defun muffle-warning (&optional condition)
  (invoke-restart-condition 'MUFFLE-WARNING condition))

;; STORE-VALUE, CLtL2 p. 913
(defun store-value (value &optional condition)
  (invoke-restart-condition-if-exists 'STORE-VALUE condition value))

;; USE-VALUE, CLtL2 p. 914
(defun use-value (value &optional condition)
  (invoke-restart-condition-if-exists 'USE-VALUE condition value))

;; like CONTINUE but is not triggered by ^D
(defun retry (&optional condition)
  (invoke-restart-condition-if-exists 'RETRY condition))

;;; 29.4.2. Assertions

;; These macros supersede the corresponding ones from macros2.lisp.

;; Queries the user for the values to put into the given place.
;; Returns a fresh list of length place-numvalues.
(defun prompt-for-new-value (place place-numvalues &optional instead-p) ; ABI
  (cond ((= place-numvalues 1)
         (format *debug-io*
                 (if instead-p
                   (concatenate 'string "~&"
                     (TEXT "Use instead~@[ of ~S~]: "))
                   (prompt-for-new-value-string))
                 place)
         (list (read *debug-io*)))
        ((do ((ii 1 (1+ ii)) res)
             ((> ii place-numvalues) (nreverse res))
           (fresh-line *debug-io*)
           (format *debug-io*
                   (if instead-p
                     (TEXT "Use instead of ~S [value ~D of ~D]: ")
                     (TEXT "New ~S [value ~D of ~D]: "))
                   place ii place-numvalues)
           (push (read *debug-io*) res)))))

;; CHECK-TYPE, CLtL2 p. 889
(defmacro check-type (place typespec &optional (string nil) &environment env)
  (let ((tag1 (gensym))
        (tag2 (gensym))
        (var (gensym)))
    `(TAGBODY
       ,tag1
       (LET ((,var ,place))
         (WHEN (TYPEP ,var ',typespec) (GO ,tag2))
         (CHECK-TYPE-FAILED ',place ,var
                            #'(LAMBDA (NEW-VALUE) (SETF ,place NEW-VALUE))
                            ,(length (nth-value 2 (get-setf-expansion place env)))
                            ,string ',typespec))
       (GO ,tag1)
       ,tag2)))
(defun check-type-failed (place place-oldvalue place-setter place-numvalues string typespec) ; ABI
  (restart-case
    (error-of-type 'type-error
      :datum place-oldvalue :expected-type typespec
      (type-error-string)
      (check-type-error-string place string typespec)
      place-oldvalue)
    ;; Only one restart. Call it STORE-VALUE, not CONTINUE, so that it's not
    ;; chosen by "continue".
    (STORE-VALUE
      :report (lambda (stream)
                (format stream (report-one-new-value-string) place))
      :interactive (lambda () (prompt-for-new-value place place-numvalues))
      (new-value) (funcall place-setter new-value))))

;; ASSERT, CLtL2 p. 891
(defmacro assert (test-form &optional (place-list nil) (datum nil) &rest args
                  &environment env)
  (let ((tag1 (gensym))
        (tag2 (gensym)))
    `(TAGBODY
       ,tag1
       (WHEN ,test-form (GO ,tag2))
       (,@(if place-list
            (let ((all-numvalues '())
                  (all-setter-vars '())
                  (all-setter-forms '()))
              (do ((pl place-list (cdr pl)))
                  ((endp pl))
                (multiple-value-bind (temps subforms stores setterform getterform)
                    (get-setf-expansion (car pl) env)
                  (declare (ignore getterform))
                  (push (length stores) all-numvalues)
                  (setq all-setter-vars
                        (revappend stores all-setter-vars))
                  (push (wrap-let* (mapcar #'list temps subforms) setterform)
                        all-setter-forms)))
              (setq all-numvalues (nreverse all-numvalues))
              (setq all-setter-vars (nreverse all-setter-vars))
              (setq all-setter-forms (nreverse all-setter-forms))
              `(ASSERT-FAILED ',place-list ',all-numvalues
                              #'(LAMBDA ,all-setter-vars ,@all-setter-forms)))
            `(SIMPLE-ASSERT-FAILED))
        ,@(if datum
            `(NIL ,datum ,@args) ; use coerce-to-condition??
            `((ASSERT-ERROR-STRING ',test-form))))
       (GO ,tag1)
       ,tag2)))
(defun assert-failed (place-list place-numvalues-list places-setter error-string &rest condition-datum+args) ; ABI
  (restart-case
    ;; No need for explicit association, see APPLICABLE-RESTART-P.
    (if error-string
      (error ; of-type ??
        "~A" error-string)
      (apply #'error condition-datum+args)) ; use coerce-to-condition??
    ;; Only one restart: CONTINUE.
    (CONTINUE
      :REPORT (lambda (stream)
                (apply #'format stream
                       (if (= (length place-list) 1)
                         (report-one-new-value-string)
                         (report-new-values-string))
                       place-list))
      :INTERACTIVE (lambda ()
                     (mapcan #'(lambda (place place-numvalues)
                                 (prompt-for-new-value place place-numvalues))
                             place-list place-numvalues-list))
      (&rest new-values) (apply places-setter new-values))))
(defun simple-assert-failed (error-string &rest condition-datum+args) ; ABI
  (restart-case
    ;; No need for explicit association, see APPLICABLE-RESTART-P.
    (if error-string
      (error ; of-type ??
        "~A" error-string)
      (apply #'error condition-datum+args)) ; use coerce-to-condition??
    ;; Only one restart: CONTINUE.
    ;; But mark it as not meaningful, because it leads to user frustration.
    (CONTINUE
      :REPORT (lambda (stream) (format stream (report-no-new-value-string)))
      MEANINGFULP nil
      ())))

(defun correctable-error (options condition)
  (let ((restarts
         (mapcar (lambda (option)
                   (destructuring-bind (name report . return) option
                     (make-restart
                      :name (etypecase name
                              (string (intern name *keyword-package*))
                              (symbol name))
                      :report (lambda (s) (princ report s))
                      :interactive (if (consp return)
                                     (lambda ()
                                       (apply (car return) (cdr return)))
                                     #'default-restart-interactive)
                      :invoke-function
                        (if (consp return)
                          (lambda (value) ; get `value' from :INTERACTIVE
                            (return-from correctable-error value))
                          (lambda ()
                            (return-from correctable-error return))))))
                 options)))
    (with-condition-restarts condition restarts
      (let ((*active-restarts* (nconc restarts *active-restarts*)))
        (error condition)))))

;; Report an error and try to recover by asking the user to supply a value.
;; Returns
;; 1. value supplied by the user,
;; 2. a boolean indicating whether PLACE should be filled, or 0 for FDEFINITION
(defun check-value (place condition)
  (let ((restarts
         (nconc
          (list (make-restart
                 :name 'USE-VALUE
                 :report
                   (lambda (stream)
                     (format stream (report-one-new-value-string-instead)
                             place))
                 :interactive (lambda () (prompt-for-new-value place 1 t))
                 :invoke-function
                   (lambda (val) (return-from check-value (values val nil)))))
          (when (and (consp place) (eq 'fdefinition (car place)))
            (list (make-restart ; for check_fdefinition() only!
                   :name 'RETRY
                   :report (lambda (stream)
                             (format stream (report-no-new-value-string)))
                   :invoke-function
                     (lambda ()
                       (return-from check-value (values nil 0))))))
          (when place
            (list (make-restart
                   :name 'STORE-VALUE
                   :report
                     (lambda (stream)
                       (format stream (report-one-new-value-string) place))
                   :interactive (lambda () (prompt-for-new-value place 1))
                   :invoke-function
                     (lambda (val)
                       (return-from check-value (values val t)))))))))
    (with-condition-restarts condition restarts
      (let ((*active-restarts* (nconc restarts *active-restarts*)))
        (error condition)))))

(defun retry-function-call (condition function arguments)
  (with-restarts ((retry
                   :report (lambda (out)
                             (format out (TEXT "try calling ~S again")
                                     (function-name function)))
                   () (return-from retry-function-call
                        (apply function arguments)))
                  (return
                   :report (lambda (out)
                             (format out (TEXT "specify return values")))
                   :interactive (lambda () (prompt-for-new-value 'VALUES 1))
                   (l) (return-from retry-function-call (values-list l))))
    (with-condition-restarts condition
        (list (find-restart 'RETRY) (find-restart 'RETURN))
      (error condition))))

;; redefine the function in init.lisp, used by LOAD
(eval-when (compile) (fmakunbound 'eval-loaded-form)) ; avoid a warning
;; FILE is the *LOAD-TRUENAME* of the file being loaded
;; we cannot use *LOAD-TRUENAME* because when the error is in a nested LOAD,
;; we will get the truename of the inner-most file instead of the current one
(defun eval-loaded-form (obj file)
  (restart-case (eval-loaded-form-low obj)
    (skip
        :report (lambda (out)
                  (format out (TEXT "skip "))
                  (if (compiled-function-p obj)
                      (write (sys::closure-name obj) :stream out
                             :pretty nil :escape nil)
                      (write obj :stream out :pretty nil :escape nil
                             :level 2 :length 3)))
        :interactive default-restart-interactive
        () (return-from eval-loaded-form 'skip))
    (stop
        :report (lambda (out) (format out (TEXT "stop loading file ~A") file))
        :interactive default-restart-interactive
        () (return-from eval-loaded-form 'stop))))

;;; 29.4.3. Exhaustive Case Analysis

;; These macros supersede the corresponding ones from macros2.lisp.
(flet ((parenthesize-keys (clauses)
         ;; PARENTHESIZE-KEYS is necessary to avoid confusing
         ;; the symbols OTHERWISE and T used as keys, with the same
         ;; symbols used in the syntax of the non exhaustive CASE.
         (mapcar #'(lambda (c)
                     (cond ((or (eq (car c) 't)
                                (eq (car c) 'otherwise))
                            (warn (TEXT "~S used as a key in ~S, it would be better to use parentheses.")
                                  (car c) c)
                            (cons (list (car c)) (cdr c)))
                           (t c)))
                 clauses)))
  (flet ((typecase-errorstring (keyform keyclauselist)
           (let ((typelist (mapcar #'first keyclauselist)))
             `(TYPECASE-ERROR-STRING ',keyform ',typelist)))
         (typecase-expected-type (keyclauselist)
           `(OR ,@(mapcar #'first keyclauselist)))
         (case-errorstring (keyform keyclauselist)
           (let ((caselist
                   (mapcap #'(lambda (keyclause)
                               (setq keyclause (car keyclause))
                               (if (listp keyclause) keyclause (list keyclause)))
                           keyclauselist)))
             `(CASE-ERROR-STRING ',keyform ',caselist)))
         (case-expected-type (keyclauselist)
           `(MEMBER ,@(mapcap #'(lambda (keyclause)
                                  (setq keyclause (car keyclause))
                                  (if (listp keyclause)
                                      keyclause (list keyclause)))
                              keyclauselist)))
         (simply-error (casename form clauselist errorstring expected-type)
           (let ((var (gensym)))
             `(LET ((,var ,form))
                (,casename ,var ,@(parenthesize-keys clauselist)
                  ;; if a clause contains an OTHERWISE or T key,
                  ;; it is treated as a normal key, as per CLHS.
                  (OTHERWISE
                    (ETYPECASE-FAILED ,var ,errorstring ',expected-type))))))
         (retry-loop (casename place clauselist errorstring expected-type env)
           (let ((g (gensym))
                 (h (gensym)))
             `(BLOCK ,g
                (TAGBODY
                  ,h
                  (RETURN-FROM ,g
                    (,casename ,place ,@(parenthesize-keys clauselist)
                      ;; if a clause contains an OTHERWISE or T key,
                      ;; it is treated as a normal key, as per CLHS.
                      (OTHERWISE
                        (CTYPECASE-FAILED ',place ,place
                                          #'(LAMBDA (NEW-VALUE) (SETF ,place NEW-VALUE))
                                          ,(length (nth-value 2 (get-setf-expansion place env)))
                                          ,errorstring ',expected-type)
                        (GO ,h)))))))))
    (defmacro etypecase (keyform &rest keyclauselist)
      (if (assoc t keyclauselist)
        `(TYPECASE ,keyform ,@keyclauselist)
        (simply-error 'TYPECASE keyform keyclauselist
                      (typecase-errorstring keyform keyclauselist)
                      (typecase-expected-type keyclauselist))))
    (defmacro ctypecase (keyplace &rest keyclauselist &environment env)
      (if (assoc t keyclauselist)
        `(TYPECASE ,keyplace ,@keyclauselist)
        (retry-loop 'TYPECASE keyplace keyclauselist
                    (typecase-errorstring keyplace keyclauselist)
                    (typecase-expected-type keyclauselist)
                    env)))
    (defmacro ecase (keyform &rest keyclauselist)
      (simply-error 'CASE keyform keyclauselist
                    (case-errorstring keyform keyclauselist)
                    (case-expected-type keyclauselist)))
    (defmacro ccase (keyform &rest keyclauselist &environment env)
      (retry-loop 'CASE keyform keyclauselist
                  (case-errorstring keyform keyclauselist)
                  (case-expected-type keyclauselist)
                  env))
) )
(defun etypecase-failed (value errorstring expected-type) ; ABI
  (error-of-type 'type-error
    :datum value :expected-type expected-type
    (type-error-string)
    errorstring value))
(defun ctypecase-failed (place place-oldvalue place-setter place-numvalues errorstring expected-type) ; ABI
  (restart-case
    (progn ; no need for explicit association, see applicable-restart-p
      (error-of-type 'type-error
        :datum place-oldvalue :expected-type expected-type
        (type-error-string)
        errorstring
        place-oldvalue))
    ;; Only one restart. Call it STORE-VALUE, not CONTINUE, so that it's not
    ;; chosen by "continue".
    (STORE-VALUE
      :report (lambda (stream)
                (format stream (report-one-new-value-string) place))
      :interactive (lambda () (prompt-for-new-value place place-numvalues))
      (new-value) (funcall place-setter new-value))))

;;; 29.4.11. Debugging Utilities

(defvar *debugger-hook* nil)

;; INVOKE-DEBUGGER, CLtL2 p. 915
; is in error.d

;; BREAK, CLtL2 p. 914
; (BREAK [format-string {arg}*])
; we call INVOKE-DEBUGGER and therefore need a condition.
(defun break (&optional (format-string "Break") &rest args)
  (if (not *use-clcs*)
    (progn
      (fresh-line *error-output*)
      (apply #'format *error-output*
                      (concatenate 'string "*** - " format-string)
                      args)
      (elastic-newline *error-output*)
      (funcall *break-driver* t))
    (let ((condition
           (make-condition 'simple-condition
                           :format-control format-string
                           :format-arguments args))
          (*debugger-hook* nil)) ; Issue 91
      (with-restarts
          ((CONTINUE
            :report (lambda (stream)
                      (format stream (TEXT "Return from ~S loop")
                                     'break))
            ()))
        (with-condition-restarts condition (list (find-restart 'CONTINUE))
          (invoke-debugger condition)))))
  nil)

;;; 29.4.1. Signaling Conditions

;; ERROR, CLtL2 p. 886
#| ; is in error.d
 (defun error (errorstring &rest args)
  (if (or *error-handler* (not *use-clcs*))
    (progn
      (if *error-handler*
        (apply *error-handler* nil errorstring args)
        (progn
          (fresh-line *error-output*)
          (write-string "*** - " *error-output*)
          (apply #'format *error-output* errorstring args)
          (elastic-newline *error-output*)))
      (funcall *break-driver* nil))
    (let ((condition (coerce-to-condition errorstring args 'error 'simple-error)))
      (signal condition)
      (invoke-debugger condition))))
|#

;; CERROR, CLtL2 p. 887
(defun cerror (continue-format-string error-format-string &rest args)
  (if *error-handler*
    (apply *error-handler*
           (or continue-format-string t) error-format-string args)
    (if (not *use-clcs*)
      (progn
        (fresh-line *error-output*)
        (write-string "** - " *error-output*)
        (write-string (TEXT "Continuable Error") *error-output*)
        (terpri *error-output*)
        (apply #'format *error-output* error-format-string args)
        (elastic-newline *error-output*)
        (fresh-line *debug-io*)
        (if (interactive-stream-p *debug-io*)
          (progn
            (write-string (TEXT "If you continue (by typing 'continue'): ")
                          *debug-io*)
            (apply #'format *debug-io* continue-format-string args)
            (elastic-newline *debug-io*)
            (funcall *break-driver* t))
          (progn
            (apply #'format *debug-io* continue-format-string args)
            (elastic-newline *debug-io*))))
      (let ((condition (coerce-to-condition error-format-string args
                                            'cerror 'simple-error)))
        (with-restarts
            ((CONTINUE
              :report (lambda (stream)
                        (apply #'format stream continue-format-string args))
              ()))
          (with-condition-restarts condition (list (find-restart 'CONTINUE))
            (signal condition)
            (invoke-debugger condition))))))
  nil)

;;; 29.4.9. Warnings

(defvar *break-on-warnings* nil)

;; WARN, CLtL2 p. 912
;; (WARN format-string {arg}*)
(defun warn (format-string &rest args)
  (if (not *use-clcs*)
    (progn
      (fresh-line *error-output*)
      (write-string (TEXT "WARNING:") *error-output*)
      (terpri *error-output*)
      (apply #'format *error-output* format-string args)
      (elastic-newline *error-output*)
      (when *break-on-warnings* (funcall *break-driver* t)))
    (block warn
      (let ((condition (coerce-to-condition format-string args 'warn 'simple-warning)))
        (unless (typep condition 'warning)
          (error-of-type 'type-error
            :datum condition :expected-type 'warning
            (TEXT "~S: This is more serious than a warning: ~A")
            'warn condition))
        (with-restarts ((MUFFLE-WARNING () (return-from warn)))
          (with-condition-restarts condition (list (find-restart 'MUFFLE-WARNING))
            (signal condition)))
        (fresh-line *error-output*)
        (let ((first-line-prefix (TEXT "WARNING: ")))
          (write-string first-line-prefix *error-output*)
          (pretty-print-condition
           condition *error-output*
           :text-indent (string-width first-line-prefix)))
        (elastic-newline *error-output*)
        (when *break-on-warnings*
          (with-restarts
              ((CONTINUE
                :report (lambda (stream)
                          (format stream (TEXT "Return from ~S loop") 'break))
                () (return-from warn)))
            (with-condition-restarts condition (list (find-restart 'CONTINUE))
              ;; We don't call  (invoke-debugger condition)  because that
              ;; would tell the user about a "Continuable error". Actually,
              ;; it is only a warning!
              (funcall *break-driver* nil condition nil)))))))
  nil)


#|
Todo:
29.3.6 29.3.7 29.3.8 29.3.9 29.3.10
      29.3.11 29.3.12 29.3.13 29.3.14 29.3.15 29.3.16 29.3.17 29.3.18
29.4. 29.4.9 29.4.11
29.5.
|#


;; Miscellaneous functions that use condition macros.

#+LOGICAL-PATHNAMES
(defun valid-logical-pathname-string-p (string)
  (handler-case (logical-pathname string)
    (TYPE-ERROR () nil)
    (:NO-ERROR (&rest values) (declare (ignore values)) t)))


;; Extensions. They assume *USE-CLCS* is T.

; Which restarts are suitable for automatic invocation?
; - Only CONTINUE restarts.
;   E.g. (check-type x float) has no CONTINUE restart.
; - Only meaningful CONTINUE restarts.
;   E.g. (assert (= 3 4)) has a CONTINUE restart, but it is not meaningful.
; - Only non-interactive CONTINUE restarts.
;   E.g. (assert (>= i 0) (i)) has a CONTINUE restart, but it prompts the user
;   for a new value of i.
(defun find-noninteractively-invokable-continue-restart (condition)
  (let ((restart (find-restart 'CONTINUE condition)))
    (and restart
         (restart-meaningfulp restart)
         (eq (restart-interactive restart) #'default-restart-interactive)
         restart)))

(defun muffle-cerror (condition) ; ABI
  (let ((restart (find-noninteractively-invokable-continue-restart condition)))
    (when restart
      (invoke-restart restart))))
(defmacro muffle-cerrors (&body body)
  "(MUFFLE-CERRORS {form}*) executes the forms, but when a continuable
error occurs, the CONTINUE restart is silently invoked."
  `(HANDLER-BIND ((ERROR #'MUFFLE-CERROR))
     ,@body))
#|| ; This works as well, but looks more like a hack.
 (defmacro muffle-cerrors (&body body)
  (let ((old-debugger-hook (gensym)))
    `(LET* ((,old-debugger-hook *DEBUGGER-HOOK*)
            (*DEBUGGER-HOOK*
             (LAMBDA (CONDITION DEBUGGER-HOOK)
               (CONTINUE CONDITION)
               (WHEN ,old-debugger-hook
                 (FUNCALL ,old-debugger-hook CONDITION ,old-debugger-hook)))))
      (PROGN ,@body))))
||#

(defun appease-cerror (condition) ; ABI
  (let ((restart (find-noninteractively-invokable-continue-restart condition)))
    (when restart
      (warn #'(lambda (stream &rest arguments)
                (print-condition condition stream)
                (let ((report-function (restart-report restart)))
                  (when report-function
                    (terpri stream)
                    (funcall report-function stream)))
                arguments))
      (invoke-restart restart))))
(defmacro appease-cerrors (&body body)
  "(APPEASE-CERRORS {form}*) executes the forms, but turns continuable errors
into warnings. A continuable error is signalled again as a warning, then
its CONTINUE restart is invoked."
  `(HANDLER-BIND ((ERROR #'APPEASE-CERROR))
     ,@body))

(defvar *report-error-print-backtrace* nil)
(defun report-error (condition)
  (when *report-error-print-backtrace*
    (print-backtrace :out *error-output*))
  (fresh-line *error-output*)
  (write-string "*** - " *error-output*)
  (pretty-print-condition condition *error-output*)
  (elastic-newline *error-output*))

(defun exitunconditionally (condition) ; ABI
  (report-error condition)
  (exit t))                     ; exit Lisp with error
(defun exitonerror (condition) ; ABI
  (unless (find-noninteractively-invokable-continue-restart condition)
    (exitunconditionally condition)))
(defmacro exit-on-error (&body body)
  "(EXIT-ON-ERROR {form}*) executes the forms, but exits Lisp if a
non-continuable error or a Ctrl-C interrupt occurs."
  `(HANDLER-BIND ((INTERRUPT-CONDITION #'EXITUNCONDITIONALLY)
                  (SERIOUS-CONDITION #'EXITONERROR))
    ,@body))

(defun abortonerror (condition) ; ABI
  (report-error condition)
  (invoke-restart (find-restart 'abort condition)))

(defmacro abort-on-error (&body body)
  "(ABORT-ON-ERROR {form}*) executes the forms and aborts all errors."
  `(HANDLER-BIND ((SERIOUS-CONDITION #'ABORTONERROR))
     ,@body))

(defgeneric global-handler (condition)
  (:method-combination progn)
  (:documentation "the global error handler, methods should not return")
  (:method progn ((condition t)) nil))

(defun set-global-handler (condition-name handler)
  "Make HANDLER handle CONDITION globally.
HANDLER should be funcallable (symbol or function).
If it returns, the next applicable error handler is invoked.
When HANDLER is nil, remove the global handler for CONDITION.
Returns the added or removed method(s)."
  (let ((clos::*warn-if-gf-already-called* nil)
        (clos::*gf-warn-on-replacing-method* nil))
    (cond (handler              ; install handler
           (clos::do-defmethod 'global-handler
             (lambda (backpointer)
               (declare (ignore backpointer)) ; we do not need CALL-NEXT-METHOD
               (list
                (lambda (condition)
                  ;; avoid infinite recursion by disabling the handler
                  (let ((clos::*gf-warn-on-replacing-method* nil)
                        (clos::*warn-if-gf-already-called* nil)
                        (old-handler (set-global-handler condition-name nil)))
                    (unwind-protect (funcall handler condition)
                      (when old-handler
                        (clos:add-method #'global-handler old-handler)))))
                t))             ; wants-next-method-p == NIL
             (list :qualifiers #1='(progn) :lambda-list '(condition)
                   'clos::signature #(1 0 NIL NIL NIL NIL)
                   :specializers (list (find-class condition-name)))))
          ((consp condition-name) ; install all these handlers
           (dolist (handler condition-name)
             (clos:add-method #'global-handler handler)))
          ((null condition-name) ; remove all global handlers
           (let ((handlers '()))
             (dolist (method (clos::generic-function-methods #'global-handler))
               (unless (equal '#,(list (find-class 't))
                              (clos::method-specializers method))
                 (push method handlers)
                 (clos:remove-method #'global-handler method)))
             handlers))
          ((symbolp condition-name) ; remove handler for this condition
           (let ((method (find-method #'global-handler #1#
                                      (list (find-class condition-name)) nil)))
             (when method
               (clos:remove-method #'global-handler method)
               method)))
          (t (error "~S(~S ~S): invalid arguments"
                    'set-global-handler condition-name handler)))))

(defmacro without-global-handlers (&body body)
  "Remove all global handlers, execute BODY, restore the handlers."
  (let ((handlers (gensym "HANDLERS-")))
    `(let ((,handlers (set-global-handler nil nil)))
       (unwind-protect (progn ,@body)
         (set-global-handler ,handlers nil)))))
