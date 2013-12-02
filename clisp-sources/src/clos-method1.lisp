;;;; Common Lisp Object System for CLISP: Methods
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;;; ---------------------------------------------------------------------------

(defparameter <method>
  (defclass method (standard-stablehash metaobject)
    (($from-defgeneric     ; flag, if this method comes from a DEFGENERIC
       :type boolean
       :accessor method-from-defgeneric))
    (:fixed-slot-locations t)
    (:generic-accessors nil)))

(defun initialize-instance-<method> (method &rest args
                                     &key ((from-defgeneric from-defgeneric) nil)
                                          ((backpointer backpointer) nil backpointer-p)
                                     &allow-other-keys)
  (if *classes-finished*
    (apply #'%initialize-instance method args) ; == (call-next-method)
    ; Bootstrapping: Simulate the effect of #'%initialize-instance.
    (apply #'shared-initialize-<standard-stablehash> method 't args))
  ; Fill the slots.
  (setf (method-from-defgeneric method) from-defgeneric)
  ; Fill the backpointer. This is needed for NO-NEXT-METHOD to work: When
  ; CALL-NEXT-METHOD is called from within the method function without a next
  ; method being available, the method function must call NO-NEXT-METHOD with
  ; the method object as argument. But since the method function is called
  ; with the argument list and the remaining methods list as arguments, it
  ; cannot know about the method object to which it belongs. We solve this
  ; paradox by constructing a backpointer cons that the method function
  ; has access to and that points back to the method object after it has been
  ; initialized.
  (when backpointer-p
    (setf (car backpointer) method))
  method)

;;; ---------------------------------------------------------------------------

(defparameter <standard-method> ; ABI
  (defclass standard-method (method)
    (($fast-function       ; the function with fast calling conventions, i.e.
                           ; argument list (&rest arguments) or
                           ; (next-methods-function &rest arguments), depending
                           ; on wants-next-method-p
       :type (or null function)
       :accessor std-method-fast-function)
     ($wants-next-method-p ; flag, if the NEXT-METHOD (as function with all
                           ; arguments) resp. NIL is to be passed as first
                           ; argument (= NIL for :BEFORE- and :AFTER-methods)
       :type boolean
       :accessor std-method-wants-next-method-p)
     ($function            ; the function with slow calling conventions, i.e.
                           ; argument list (arguments next-methods-list)
       :type (or null function)
       :accessor std-method-function)
     ($specializers        ; list of specializers, e.g. classes or
                           ; eql-specializers
       :type list
       :accessor std-method-specializers)
     ($qualifiers          ; list of non-NIL atoms, e.g. (:before)
       :type list
       :accessor std-method-qualifiers)
     ($lambda-list         ; lambda list without specializers
       :type list
       :accessor std-method-lambda-list)
     ($signature           ; signature struct (see functions.lisp)
       :type (simple-vector 6)
       :accessor std-method-signature)
     ($documentation       ; string or NIL
       :type (or string null)
       :accessor std-method-documentation)
     ($gf                  ; the generic function, which this method belongs to
                           ; (only for the purpose of CALL-NEXT-METHOD and
                           ; NO-NEXT-METHOD)
       :type (or null generic-function)
       :accessor std-method-generic-function))
    (:fixed-slot-locations t)
    (:generic-accessors nil)))

;; Note about the argument passing convention for methods:
;; 1) The MOP description of COMPUTE-EFFECTIVE-METHOD and MAKE-METHOD-LAMBDA
;;    says that a method function takes 2 arguments: the list of arguments (!)
;;    and the list of next methods. This is awfully inefficient, and useless
;;    (since MAKE-METHOD-LAMBDA is ill-designed anyway). Therefore here we
;;    pass to the function the arguments as-is, and the next methods as
;;    inserted first argument, if needed.
;; 2) Instead of the list of next methods, we pass an effective method that
;;    consists of these next methods. This is more efficient (saves a FUNCALL)
;;    for the simple case of a single applicable method, but is less
;;    efficient (a FUNCALL instead of just a CAR) for longer lists of methods.
;; 3) We don't explicitly pass the generic function to the method during the
;;    invocation. However, for CALL-NEXT-METHOD, NO-NEXT-METHOD and
;;    METHOD-GENERIC-FUNCTION the generic function must be known. So we have
;;    to store a generic function backpointer in the method.

(defun method-lambda-list-to-signature (lambda-list errfunc)
  (multiple-value-bind (reqvars optvars optinits optsvars rest
                        keyp keywords keyvars keyinits keysvars
                        allowp auxvars auxinits)
      (analyze-lambdalist lambda-list errfunc)
    (declare (ignore optinits optsvars keyvars keyinits keysvars
                     auxvars auxinits))
    (make-signature
      :req-num (length reqvars) :opt-num (length optvars)
      :rest-p (or keyp (not (eql rest 0))) :keys-p keyp
      :keywords keywords :allow-p allowp)))

(defun initialize-instance-<standard-method> (method &rest args
                                              &key (qualifiers '())
                                                   (lambda-list nil lambda-list-p)
                                                   (specializers nil specializers-p)
                                                   (function nil function-p)
                                                   (documentation nil)
                                                   ((fast-function fast-function) nil fast-function-p)
                                                   ((wants-next-method-p wants-next-method-p) nil)
                                                   ((signature signature) nil signature-p)
                                                   ((gf gf) nil)
                                                   ((from-defgeneric from-defgeneric) nil)
                                                   ((backpointer backpointer) nil)
                                              &allow-other-keys)
  (declare (ignore from-defgeneric backpointer))
  (apply #'initialize-instance-<method> method args) ; == (call-next-method)
  ; Check the qualifiers.
  (unless (proper-list-p qualifiers)
    (error (TEXT "(~S ~S): The ~S argument should be a proper list, not ~S")
           'initialize-instance 'standard-method ':qualifiers qualifiers))
  (unless (notany #'listp qualifiers)
    (error (TEXT "(~S ~S): The qualifiers list should consist of non-NIL atoms, not ~S")
           'initialize-instance 'standard-method qualifiers))
  ; Check the lambda-list and compute the signature from it.
  (unless lambda-list-p
    (error (TEXT "(~S ~S): Missing ~S argument.")
           'initialize-instance 'standard-method ':lambda-list))
  (let ((sig (method-lambda-list-to-signature lambda-list
               #'(lambda (detail errorstring &rest arguments)
                   (declare (ignore detail))
                   (error (TEXT "(~S ~S): Invalid ~S argument: ~A")
                          'initialize-instance 'standard-method ':lambda-list
                          (apply #'format nil errorstring arguments))))))
    ; Check the signature argument. It is optional; specifying it only has
    ; the purpose of saving memory allocation (by sharing the same signature
    ; for all reader methods and the same signature for all writer methods).
    (if signature-p
      (unless (equalp sig signature)
        (error (TEXT "(~S ~S): Lambda-list ~S and signature ~S are inconsistent.")
               'initialize-instance 'standard-method lambda-list signature))
      (setq signature sig)))
  ; Check the specializers.
  (unless specializers-p
    (error (TEXT "(~S ~S): Missing ~S argument.")
           'initialize-instance 'standard-method ':specializers))
  (unless (proper-list-p specializers)
    (error (TEXT "(~S ~S): The ~S argument should be a proper list, not ~S")
           'initialize-instance 'standard-method ':specializers specializers))
  (dolist (x specializers)
    (unless (or (defined-class-p x) (eql-specializer-p x))
      (if (typep x 'specializer)
        (error (TEXT "(~S ~S): The element ~S of the ~S argument is not yet defined.")
               'initialize-instance 'standard-method x ':specializers)
        (error (TEXT "(~S ~S): The element ~S of the ~S argument is not of type ~S.")
               'initialize-instance 'standard-method x ':specializers 'specializer))))
  (unless (= (length specializers) (sig-req-num signature))
    (error (TEXT "(~S ~S): The lambda list ~S has ~S required arguments, but the specializers list ~S has length ~S.")
           'initialize-instance 'standard-method lambda-list (sig-req-num signature)
           specializers (length specializers)))
  ; Check the function, fast-function and wants-next-method-p.
  (unless (or function-p fast-function-p)
    (error (TEXT "(~S ~S): Missing ~S argument.")
           'initialize-instance 'standard-method ':function))
  (when function-p
    (unless (functionp function)
      (error (TEXT "(~S ~S): The ~S argument should be a function, not ~S")
             'initialize-instance 'standard-method ':function function)))
  (when fast-function-p
    (unless (functionp fast-function)
      (error (TEXT "(~S ~S): The ~S argument should be a function, not ~S")
             'initialize-instance 'standard-method 'fast-function fast-function)))
  (unless (typep wants-next-method-p 'boolean)
    (error (TEXT "(~S ~S): The ~S argument should be a NIL or T, not ~S")
           'initialize-instance 'standard-method 'wants-next-method-p  wants-next-method-p))
  (when function-p
    ;; :function overrides fast-function and wants-next-method-p, because it is
    ;; the standardized way (employed by user-defined method classes) to define
    ;; the behaviour of a method.
    (setq fast-function nil
          wants-next-method-p t))
  ; Check the documentation.
  (unless (or (null documentation) (stringp documentation))
    (error (TEXT "(~S ~S): The ~S argument should be a string or NIL, not ~S")
           'initialize-instance 'standard-method ':documentation documentation))
  ; Fill the slots.
  (setf (std-method-fast-function method) fast-function)
  (setf (std-method-wants-next-method-p method) wants-next-method-p)
  (setf (std-method-function method) function)
  (setf (std-method-specializers method) specializers)
  (setf (std-method-qualifiers method) qualifiers)
  (setf (std-method-lambda-list method) lambda-list)
  (setf (std-method-signature method) signature)
  (setf (std-method-documentation method) documentation)
  (setf (std-method-generic-function method) gf)
  method)

(defun make-instance-<standard-method> (class &rest args
                                        &key &allow-other-keys)
  ;; class = <standard-method>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore class))
  (let ((method (%allocate-instance <standard-method>)))
    (apply #'initialize-instance-<standard-method> method args)))

(defun print-object-<standard-method> (method stream)
  (print-unreadable-object (method stream :type t)
    (if (and (not (eq (std-method-qualifiers method) (sys::%unbound)))
             (not (eq (std-method-specializers method) (sys::%unbound))))
      (progn
        (dolist (q (std-method-qualifiers method))
          (write q :stream stream)
          (write-char #\Space stream))
        (write (mapcar #'specializer-pretty (std-method-specializers method))
               :stream stream))
      (write :uninitialized :stream stream))))

;; Preliminary.
;; During bootstrapping, only <standard-method> instances are used.
(defun make-method-instance (class &rest args ; ABI
                             &key &allow-other-keys)
  (apply #'make-instance-<standard-method> class args))
(defun method-function (method)
  (std-method-function-or-substitute method))
(defun method-qualifiers (method)
  (std-method-qualifiers method))
(defun method-lambda-list (method)
  (std-method-lambda-list method))
(defun method-signature (method)
  (std-method-signature method))
(defun method-specializers (method)
  (std-method-specializers method))
(defun method-generic-function (method)
  (std-method-generic-function method))
(defun (setf method-generic-function) (new-gf method)
  (setf (std-method-generic-function method) new-gf))

;;; ---------------------------------------------------------------------------

(defparameter <standard-accessor-method>
  (defclass standard-accessor-method (standard-method)
    (($slot-definition     ; direct slot definition responsible for this method
       :type direct-slot-definition
       :accessor %accessor-method-slot-definition))
    (:fixed-slot-locations t)
    (:generic-accessors nil)))

(defun initialize-instance-<standard-accessor-method> (method &rest args
                                                       &key (slot-definition nil slot-definition-p)
                                                       &allow-other-keys)
  (apply #'initialize-instance-<standard-method> method args) ; == (call-next-method)
  ; Check the slot-definition.
  (unless slot-definition-p
    (error (TEXT "(~S ~S): Missing ~S argument.")
           'initialize-instance 'standard-accessor-method ':slot-definition))
  (unless (typep slot-definition 'direct-slot-definition)
    (error (TEXT "(~S ~S): The slot-definition argument is not of type ~S.")
           'initialize-instance 'standard-accessor-method 'direct-slot-definition))
  ; Fill the slots.
  (setf (%accessor-method-slot-definition method) slot-definition)
  method)

;;; ---------------------------------------------------------------------------

(defparameter <standard-reader-method>
  (defclass standard-reader-method (standard-accessor-method)
    ()
    (:fixed-slot-locations t)))

(defun make-instance-<standard-reader-method> (class &rest args
                                               &key &allow-other-keys)
  ;; class = <standard-reader-method>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore class))
  (let ((method (%allocate-instance <standard-reader-method>)))
    (apply #'initialize-instance-<standard-accessor-method> method args)))

;;; ---------------------------------------------------------------------------

(defparameter <standard-writer-method>
  (defclass standard-writer-method (standard-accessor-method)
    ()
    (:fixed-slot-locations t)))

(defun make-instance-<standard-writer-method> (class &rest args
                                               &key &allow-other-keys)
  ;; class = <standard-writer-method>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore class))
  (let ((method (%allocate-instance <standard-writer-method>)))
    (apply #'initialize-instance-<standard-accessor-method> method args)))

;;; ---------------------------------------------------------------------------
