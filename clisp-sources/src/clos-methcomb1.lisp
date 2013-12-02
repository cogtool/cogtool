;;;; Common Lisp Object System for CLISP: Method Combination
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08
;;;; James Anderson 2003

(in-package "CLOS")

;;; ===========================================================================

;;; Global management of method-combinations and their names:

;; Mapping from name, a symbol, to method-combination instance.
;; If the caller is non-nil, an error is signalled if the method-combination
;; does not exist. Otherwise nil is returned.
;; All method-combination objects used here have an empty options list;
;; method-combination objects with options are stored in generic functions.
(defun get-method-combination (name caller)
  (or (get name '%method-combination)
      (and caller
           (error (TEXT "~S: The method combination ~S is not defined.")
                  caller name))))
(defun (setf get-method-combination) (new-value name)
  (setf (get name '%method-combination) new-value))

;;; ===========================================================================

;;; The method-combination class definition.
;; A method-combination is used 1) without options when defined and attached
;; to a symbol, 2) with options when applied to a particular generic function.
;; Strange design... but ANSI CL specifies it this way.
;; A structure definition is to be preferred, otherwise the compiled
;; load fails on type tests as the class can't be defined early enough
;; in the file.
(defparameter <method-combination>
  (defclass method-combination (metaobject)
    ((name                      ; a symbol naming the method combination
       :type symbol
       :accessor method-combination-name)
     (documentation             ; an optional documentation string
       :type (or null string)
       :accessor method-combination-documentation)
     (check-options             ; A function of 3 arguments
                                ; (function-name method-combination options)
                                ; that checks the syntax of arguments to the
                                ; method combination
       :type function
       :accessor method-combination-check-options)
     (expander                  ; A function of 4 arguments
                                ; (function method-combination options methods)
                                ; which computes two values: 1. the inner body
                                ; of the effective method, as a form containing
                                ; (CALL-METHOD ...) forms, 2. a list of
                                ; options describing the wrapper, such as
                                ; (:ARGUMENTS ...) or (:GENERIC-FUNCTION ...).
       :type function
       :accessor method-combination-expander)
     (check-method-qualifiers   ; A function of 3 arguments
                                ; (function method-combination method)
                                ; that checks whether the method's qualifiers
                                ; are compatible with the method-combination.
       :type function
       :accessor method-combination-check-method-qualifiers)
     (call-next-method-allowed  ; A function of 3 arguments
                                ; (function method-combination method)
                                ; telling whether call-next-method is allowed
                                ; in the particular method.
       :type function
       :accessor method-combination-call-next-method-allowed)
     (declarations              ; list to be prepended to the effective method
                                ; body
       :type list
       :accessor method-combination-declarations)

     ;; The following slots apply only to standard and short form
     ;; method-combination.
     (qualifiers                ; the allowed list of qualifiers
       :type list
       :accessor method-combination-qualifiers)

     ;; The following slots apply only to short form method-combination.
     (operator                  ; a symbol
       :type symbol
       :accessor method-combination-operator)
     (identity-with-one-argument ; true if `(operator ,x) should be replaced
                                ; with x
       :type boolean
       :accessor method-combination-identity-with-one-argument)

     ;; The following slots apply only to long form method-combination.
     (long-expander             ; A function of 2+n variables
                                ; (function methods . options)
                                ; which computes the inner body of the effective
                                ; method, as a form containing (CALL-METHOD ...)
                                ; forms
       :type function
       :accessor method-combination-long-expander)
     (arguments-lambda-list     ; The :arguments option of the defined method
                                ; combination for inclusion in the effective
                                ; method function.
       :type list
       :accessor method-combination-arguments-lambda-list)

     ;; The following slots depend on the particular generic function.
     (options                   ; arguments for the method combination
       :type list
       :accessor method-combination-options))

    (:fixed-slot-locations t)
    (:generic-accessors nil)))

(defun initialize-instance-<method-combination> (combination &rest args
                                                 &key name
                                                      (documentation nil)
                                                      check-options
                                                      expander
                                                      check-method-qualifiers
                                                      call-next-method-allowed
                                                      (declarations '())
                                                      qualifiers
                                                      operator
                                                      (identity-with-one-argument nil)
                                                      long-expander
                                                      arguments-lambda-list
                                                      (options '()))
  (when *classes-finished*
    (apply #'%initialize-instance combination args)) ; == (call-next-method)
  (setf (method-combination-name combination) name)
  (setf (method-combination-documentation combination) documentation)
  (setf (method-combination-check-options combination) check-options)
  (setf (method-combination-expander combination) expander)
  (setf (method-combination-check-method-qualifiers combination) check-method-qualifiers)
  (setf (method-combination-call-next-method-allowed combination) call-next-method-allowed)
  (setf (method-combination-declarations combination) declarations)
  (setf (method-combination-qualifiers combination) qualifiers)
  (setf (method-combination-operator combination) operator)
  (setf (method-combination-identity-with-one-argument combination) identity-with-one-argument)
  (setf (method-combination-long-expander combination) long-expander)
  (setf (method-combination-arguments-lambda-list combination) arguments-lambda-list)
  (setf (method-combination-options combination) options)
  combination)

(defun make-instance-<method-combination> (class &rest args
                                           &key &allow-other-keys)
  ;; class = <method-combination>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore class))
  (let ((combination (%allocate-instance <method-combination>)))
    (apply #'initialize-instance-<method-combination> combination args)))

(defun copy-method-combination (combination)
  (make-instance-<method-combination> <method-combination>
    :name (method-combination-name combination)
    :documentation (method-combination-documentation combination)
    :check-options (method-combination-check-options combination)
    :expander (method-combination-expander combination)
    :check-method-qualifiers (method-combination-check-method-qualifiers combination)
    :call-next-method-allowed (method-combination-call-next-method-allowed combination)
    :declarations (method-combination-declarations combination)
    :qualifiers (method-combination-qualifiers combination)
    :operator (method-combination-operator combination)
    :identity-with-one-argument (method-combination-identity-with-one-argument combination)
    :long-expander (method-combination-long-expander combination)
    :arguments-lambda-list (method-combination-arguments-lambda-list combination)
    :options (method-combination-options combination)))

(defun print-object-<method-combination> (object stream)
  (print-unreadable-object (object stream :identity t :type t)
    (write (method-combination-name object) :stream stream)))
