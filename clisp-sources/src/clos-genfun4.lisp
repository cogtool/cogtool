;;;; Common Lisp Object System for CLISP
;;;; Generic Functions
;;;; Part n-2: make/initialize-instance methods, generic functions.
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2005
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; ----------------------------------------------------------------------------

;;; Lift the initialization protocol.

(defmethod shared-initialize ((gf generic-function) situation &rest args
                              &key name)
  (declare (ignore name))
  (apply #'shared-initialize-<generic-function> gf situation args))

(defmethod shared-initialize ((gf standard-generic-function) situation &rest args
                              &key name
                                   lambda-list
                                   argument-precedence-order
                                   method-class
                                   method-combination
                                   documentation
                                   declarations
                                   declare)
  (declare (ignore name lambda-list argument-precedence-order method-class
                   method-combination documentation declarations declare))
  (apply #'shared-initialize-<standard-generic-function> gf situation args))

(defmethod initialize-instance ((gf generic-function) &rest args
                                &key name
                                     lambda-list
                                     argument-precedence-order
                                     method-class
                                     method-combination
                                     documentation
                                     declarations
                                     declare
                                     ((methods methods) nil)) ; from DEFGENERIC
  (declare (ignore name lambda-list argument-precedence-order method-class
                   method-combination documentation declarations declare
                   methods))
  (apply #'initialize-instance-<generic-function> gf args))

(defmethod reinitialize-instance ((gf generic-function) &rest args
                                  &key name
                                       lambda-list
                                       argument-precedence-order
                                       method-class
                                       method-combination
                                       documentation
                                       declarations
                                       declare
                                       ((methods methods) nil) ; from DEFGENERIC
                                  &allow-other-keys)
  (declare (ignore name lambda-list argument-precedence-order method-class
                   method-combination documentation declarations declare
                   methods))
  (apply #'reinitialize-instance-<generic-function> gf args))

;; ----------------------------------------------------------------------------

;; An argument is called "dispatching" if not all the corresponding parameter
;; specializers are <t>.
(defun dispatching-arg-p (index methods)
  (notevery #'(lambda (method)
                (eq (nth index (method-specializers method)) <t>))
            methods))
(defun single-dispatching-arg (reqanz methods)
  (let ((first-dispatching-arg
         (dotimes (i reqanz nil)
           (when (dispatching-arg-p i methods) (return i)))))
    (and first-dispatching-arg
         (do ((i (1+ first-dispatching-arg) (1+ i)))
             ((>= i reqanz) first-dispatching-arg)
           (when (dispatching-arg-p i methods) (return nil))))))
(defun dispatching-arg-type (index methods)
  `(OR ,@(remove-duplicates
           (mapcar #'(lambda (method)
                       (nth index (method-specializers method)))
                   methods)
           :test #'same-specializers-p)))

(defgeneric no-applicable-method (gf &rest args)
  (:method ((gf t) &rest args)
    (let* ((methods (safe-gf-methods gf))
           (dispatching-arg
             (if (safe-gf-undeterminedp gf)
               nil
               (let ((reqnum (sig-req-num (safe-gf-signature gf))))
                 (single-dispatching-arg reqnum methods)))))
      (sys::retry-function-call
        (if dispatching-arg
          (make-condition 'method-call-type-error
            :datum (nth dispatching-arg args)
            :expected-type (dispatching-arg-type dispatching-arg methods)
            :generic-function gf :argument-list args
            :format-control (TEXT "~S: When calling ~S with arguments ~S, no method is applicable.")
            :format-arguments (list 'no-applicable-method gf args))
          (make-condition 'method-call-error
            :generic-function gf :argument-list args
            :format-control (TEXT "~S: When calling ~S with arguments ~S, no method is applicable.")
            :format-arguments (list 'no-applicable-method gf args)))
        gf args))))

(defgeneric missing-required-method (gf combination group-name group-filter &rest args) ; ABI
  (:method ((gf t) (combination method-combination) (group-name symbol) (group-filter function) &rest args)
    (let* ((methods (remove-if-not group-filter (safe-gf-methods gf)))
           (dispatching-arg
             (if (safe-gf-undeterminedp gf)
               nil
               (let ((reqnum (sig-req-num (safe-gf-signature gf))))
                 (single-dispatching-arg reqnum methods)))))
      (if dispatching-arg
        (error-of-type 'method-call-type-error
          :datum (nth dispatching-arg args)
          :expected-type (dispatching-arg-type dispatching-arg methods)
          :generic-function gf :argument-list args
          (TEXT "~S: When calling ~S with arguments ~S, no method of group ~S (from ~S) is applicable.")
          'missing-required-method gf args group-name combination)
        (error-of-type 'method-call-error
          :generic-function gf :argument-list args
          (TEXT "~S: When calling ~S with arguments ~S, no method of group ~S (from ~S) is applicable.")
          'missing-required-method gf args group-name combination)))))

;; Special case of missing-required-method for STANDARD method combination
;; and the PRIMARY method group.
(defgeneric no-primary-method (gf &rest args)
  (:method ((gf t) &rest args)
    (let* ((methods (remove-if-not #'null (safe-gf-methods gf)
                                   :key #'method-qualifiers))
           (dispatching-arg
             (if (safe-gf-undeterminedp gf)
               nil
               (let ((reqnum (sig-req-num (safe-gf-signature gf))))
                 (single-dispatching-arg reqnum methods)))))
      (sys::retry-function-call
        (if dispatching-arg
          (make-condition 'method-call-type-error
            :datum (nth dispatching-arg args)
            :expected-type (dispatching-arg-type dispatching-arg methods)
            :generic-function gf :argument-list args
            :format-control (TEXT "~S: When calling ~S with arguments ~S, no primary method is applicable.")
            :format-arguments (list 'no-primary-method gf args))
          (make-condition 'method-call-error
            :generic-function gf :argument-list args
            :format-control (TEXT "~S: When calling ~S with arguments ~S, no primary method is applicable.")
            :format-arguments (list 'no-primary-method gf args)))
        gf args))))

(defun %no-next-method (backpointer &rest args) ; ABI
  (let ((method (car backpointer)))
    (apply #'no-next-method (method-generic-function method) method args)))
(defgeneric no-next-method (gf method &rest args)
  (:method ((gf standard-generic-function) (method method) &rest args
            &aux (cont-mesg (format nil (TEXT "ignore ~S") 'CALL-NEXT-METHOD)))
    (if (let ((method-combo (safe-gf-method-combination gf)))
          (funcall (method-combination-call-next-method-allowed method-combo)
                   gf method-combo method))
      (cerror cont-mesg 'method-call-error
        :generic-function gf :method method :argument-list args
        :format-control (TEXT "~S: When calling ~S with arguments ~S, there is no next method after ~S, and ~S was called.")
        :format-arguments (list 'no-next-method gf args method
                                '(call-next-method)))
      (let ((qualifiers (method-qualifiers method)))
        (if qualifiers
          (cerror cont-mesg 'program-error
            :format-control (TEXT "~S: ~S is invalid within ~{~S~^ ~} methods")
            :format-arguments (list gf 'CALL-NEXT-METHOD qualifiers))
          (cerror cont-mesg 'program-error
            :format-control (TEXT "~S: ~S is invalid within primary methods")
            :format-arguments (list gf 'CALL-NEXT-METHOD)))))))

;; ----------------------------------------------------------------------------

(defgeneric find-method (gf qualifiers specializers &optional errorp)
  (:method ((gf standard-generic-function) qualifiers specializers &optional (errorp t))
    (std-find-method gf qualifiers specializers errorp)))

;; MOP p. 33
(let ((*allow-making-generic* t))
  (defgeneric add-method (gf method)
    (:method ((gf standard-generic-function) (method method))
      (std-add-method gf method))))
; No extended method check because this GF is specified in ANSI CL.
;(initialize-extended-method-check #'add-method)

;; MOP p. 91
(fmakunbound 'remove-method)
(defgeneric remove-method (gf method)
  (:method ((gf standard-generic-function) (method method))
    (std-remove-method gf method)))
; No extended method check because this GF is specified in ANSI CL.
;(initialize-extended-method-check #'remove-method)

;; MOP p. 40
(fmakunbound 'compute-discriminating-function)
(defgeneric compute-discriminating-function (gf)
  (:method ((gf generic-function))
    (compute-discriminating-function-<generic-function> gf)))
(setq |#'compute-discriminating-function| #'compute-discriminating-function)

;; MOP p. 35
(fmakunbound 'compute-applicable-methods)
(defgeneric compute-applicable-methods (gf args)
  (:method ((gf generic-function) args)
    (compute-applicable-methods-<generic-function> gf args)))
(setq |#'compute-applicable-methods| #'compute-applicable-methods)

;; MOP p. 36
(fmakunbound 'compute-applicable-methods-using-classes)
(defgeneric compute-applicable-methods-using-classes (gf req-arg-classes)
  (:method ((gf generic-function) req-arg-classes)
    (compute-applicable-methods-using-classes-<generic-function> gf req-arg-classes)))
(setq |#'compute-applicable-methods-using-classes| #'compute-applicable-methods-using-classes)

;; MOP p. 41
(fmakunbound 'compute-effective-method)
(defgeneric compute-effective-method (gf combination methods)
  (:method ((gf generic-function) combination methods)
    (compute-effective-method-<generic-function> gf combination methods)))
(setq |#'compute-effective-method| #'compute-effective-method)

;; ----------------------------------------------------------------------------

;; MOP p. 10
;;
;; "Portable programs may define methods that override specified methods
;;  only when the description of the specified method explicitly allows this."
;; (Note: "override" means a method that does not (call-next-method).)
;;
;; "Portable programs may define methods that extend specified methods
;;  unless the description of the specified method explicitly prohibits this.
;;  Unless there is a specific statement to the contrary, these extending
;;  methods must return whatever value was returned by the call to
;;  call-next-method."
;; (Note: "extend" means a method that does (call-next-method).)
;; There are no explicit prohibitions. So this rule is applicable to all
;; MOP generic functions that are not part of a protocol.

;; Signal an error if the first MOP-standardized method was not called, or
;; if the returned values are different ones.
(defun extended-method-check (called original-values extended-values name)
  (unless called
    (error (TEXT "~S: Overriding a standardized method is not allowed. You need to call ~S.")
           name 'call-next-method))
  (unless (and (eql (length original-values) (length extended-values))
               (every #'eql original-values extended-values))
    (error (if (and (eql (length original-values) 1) (eql (length extended-values) 1))
             (TEXT "~S: Extending a standardized method is only allowed if it returns the same values as the next method.~%Original value: ~{~S~^, ~}~%Value returned by the extending method: ~{~S~^, ~}")
             (TEXT "~S: Extending a standardized method is only allowed if it returns the same values as the next method.~%Original values: ~{~S~^, ~}~%Values returned by the extending method: ~{~S~^, ~}"))
           name original-values extended-values))
  (values-list extended-values))

;; The list of packages whose classes are considered MOP-standardized.
(defvar *mop-standardized-packages*
        (list (find-package "COMMON-LISP") (find-package "CLOS")))

;; Tests whether a method is considered MOP-standardized.
(defun mop-standardized-p (method)
  ;; Careful! Don't use the generic function method-specializers here,
  ;; otherwise we get an infinite recursion.
  (and (typep method 'standard-method)
       (every #'(lambda (specializer)
                  (and (defined-class-p specializer)
                       (let ((name (class-name specializer)))
                         (and (symbolp name)
                              (memq (symbol-package name)
                                    *mop-standardized-packages*)))))
              (std-method-specializers method))))

;; Rewrite an effective-method, adding a check that
;; 1. the first MOP-standardized method in the list is really called,
;; 2. the returned values are identical to the values of this call.
(defun add-extended-method-check (efm gf)
  (let ((name (generic-function-name gf)))
    (flet ((add-outer-wrapper (form)
             `(LET ((STANDARDIZED-METHOD-CALLED NIL)
                    (STANDARDIZED-METHOD-VALUES NIL))
                (LET ((EXTENDED-VALUES (MULTIPLE-VALUE-LIST ,form)))
                  (EXTENDED-METHOD-CHECK STANDARDIZED-METHOD-CALLED
                                         STANDARDIZED-METHOD-VALUES
                                         EXTENDED-VALUES
                                         ',name))))
           (add-method-call-wrapper (form)
             `(PROGN
                (SETQ STANDARDIZED-METHOD-VALUES (MULTIPLE-VALUE-LIST ,form))
                (SETQ STANDARDIZED-METHOD-CALLED T)
                (VALUES-LIST STANDARDIZED-METHOD-VALUES))))
      (labels ((convert-effective-method (efm)
                 (if (consp efm)
                   (if (eq (car efm) 'CALL-METHOD)
                     (let ((method-list (third efm)))
                       (if (or (typep (first method-list) 'method) (rest method-list))
                         ; Reduce the case of multiple methods to a single one.
                         ; Make the call to the next-method explicit.
                         (convert-effective-method
                           `(CALL-METHOD ,(second efm)
                              ((MAKE-METHOD
                                 (CALL-METHOD ,(first method-list) ,(rest method-list))))))
                         ; Now the case of at most one method.
                         (if (and (typep (second efm) 'method)
                                  (member (method-qualifiers (second efm)) '((:before) (:after))
                                          :test #'equal))
                           ; Don't recurse into :before/:after methods since they
                           ; cannot call CALL-NEXT-METHOD and their values are ignored.
                           efm
                           (if (and (typep (second efm) 'method)
                                    (mop-standardized-p (second efm)))
                             ; Wrap the method call. Don't need to recurse into efm
                             ; because we are only interested in the outermost call
                             ; to a MOP-standardized method.
                             (add-method-call-wrapper efm)
                             ; Normal recursive processing.
                             (cons (convert-effective-method (car efm))
                                   (convert-effective-method (cdr efm)))))))
                     (cons (convert-effective-method (car efm))
                           (convert-effective-method (cdr efm))))
                   efm)))
        (add-outer-wrapper (convert-effective-method efm))))))

(defparameter *extended-method-check-method*
  ;; This method is added for each MOP-standardized generic function.
  (defmethod compute-effective-method ((gf (eql nil)) method-combination methods)
    (declare (ignore method-combination))
    (if (or (every #'mop-standardized-p methods)
            (notany #'mop-standardized-p methods))
      (call-next-method)
      (add-extended-method-check (call-next-method) gf))))

(defun initialize-extended-method-check (gf)
  (add-method |#'compute-effective-method|
    (make-instance-<standard-method> <standard-method>
      :qualifiers          (std-method-qualifiers *extended-method-check-method*)
      :lambda-list         (std-method-lambda-list *extended-method-check-method*)
      :specializers        (list (intern-eql-specializer gf) <t> <t>)
      :documentation       (std-method-documentation *extended-method-check-method*)
      'fast-function       (std-method-fast-function *extended-method-check-method*)
      'wants-next-method-p (std-method-wants-next-method-p *extended-method-check-method*)
      'signature           (std-method-signature *extended-method-check-method*))))

;; ----------------------------------------------------------------------------

(defun check-generic-function-initialized (gf)
  (unless (std-gf-initialized gf)
    (error (TEXT "The generic function ~S has not yet been initialized.")
           gf)))

;; MOP p. 80
(defgeneric generic-function-name (generic-function)
  (:method ((gf standard-generic-function))
    (check-generic-function-initialized gf)
    (funcallable-name gf)))
(initialize-extended-method-check #'generic-function-name)
;; MOP p. 92
(defgeneric (setf generic-function-name) (new-value generic-function)
  (:method (new-value (gf standard-generic-function))
    (unless (sys::function-name-p new-value)
      (error-of-type 'type-error
        :datum new-value :expected-type '(or symbol (cons (eql setf) (cons symbol null)))
        (TEXT "~S: The name of a generic function must be a function name, not ~S")
        '(setf generic-function-name) new-value))
    (reinitialize-instance gf :name new-value)
    new-value))
(initialize-extended-method-check #'(setf generic-function-name))

;; MOP p. 80
(let ((*allow-making-generic* t))
  (defgeneric generic-function-methods (generic-function)
    (:method ((gf standard-generic-function))
      (check-generic-function-initialized gf)
      (std-gf-methods gf))))
(setq |#'generic-function-methods| #'generic-function-methods)
(initialize-extended-method-check #'generic-function-methods)

;; MOP p. 80
(let ((*allow-making-generic* t))
  (defgeneric generic-function-method-class (generic-function)
    (:method ((gf standard-generic-function))
      (check-generic-function-initialized gf)
      (std-gf-default-method-class gf))))
(setq |#'generic-function-method-class| #'generic-function-method-class)
(initialize-extended-method-check #'generic-function-method-class)

;; MOP p. 79
(defgeneric generic-function-lambda-list (generic-function)
  (:method ((gf standard-generic-function))
    (check-generic-function-initialized gf)
    (when (eq (std-gf-signature gf) (sys::%unbound))
      (error (TEXT "~S: the lambda-list of ~S is not yet known")
             'generic-function-lambda-list gf))
    (std-gf-lambda-list gf)))
(initialize-extended-method-check #'generic-function-lambda-list)

;; Not in MOP.
(let ((*allow-making-generic* t))
  (defgeneric generic-function-signature (generic-function)
    (:method ((gf generic-function))
      (let ((lambdalist (generic-function-lambda-list gf)))
        (generic-function-lambda-list-to-signature lambdalist
          #'(lambda (detail errorstring &rest arguments)
              (declare (ignore detail))
              (error (TEXT "Invalid ~S result ~S: ~A")
                     'generic-function-lambda-list lambdalist
                     (apply #'format nil errorstring arguments))))))
    (:method ((gf standard-generic-function))
      (check-generic-function-initialized gf)
      (when (eq (std-gf-signature gf) (sys::%unbound))
        (error (TEXT "~S: the lambda-list of ~S is not yet known")
               'generic-function-lambda-list gf))
      (std-gf-signature gf))))
(setq |#'generic-function-signature| #'generic-function-signature)

;; Not in MOP.
(let ((*allow-making-generic* t))
  (defgeneric generic-function-undeterminedp (generic-function)
    (:method ((gf generic-function))
      ;; It's a pity that this is not a MOP function. So we have to catch errors
      ;; in order to peek into the state of a generic function.
      (block nil
        (sys::%handler-bind
         #'(lambda () (generic-function-lambda-list gf) nil) 'ERROR
         #'(lambda (condition) (declare (ignore condition)) (return t)))))
    (:method ((gf standard-generic-function))
      (check-generic-function-initialized gf)
      (std-gf-undeterminedp gf))))
(setq |#'generic-function-undeterminedp| #'generic-function-undeterminedp)

;; MOP p. 80
(let ((*allow-making-generic* t))
  (defgeneric generic-function-method-combination (generic-function)
    (:method ((gf standard-generic-function))
      (check-generic-function-initialized gf)
      (std-gf-method-combination gf))))
(setq |#'generic-function-method-combination| #'generic-function-method-combination)
(initialize-extended-method-check #'generic-function-method-combination)

;; MOP p. 79
(defgeneric generic-function-argument-precedence-order (generic-function)
  (:method ((gf standard-generic-function))
    (check-generic-function-initialized gf)
    (when (eq (std-gf-signature gf) (sys::%unbound))
      (error (TEXT "~S: the lambda-list of ~S is not yet known")
             'generic-function-argument-precedence-order gf))
    (let ((argorder (std-gf-argorder gf))
          (lambdalist (std-gf-lambda-list gf)))
      (mapcar #'(lambda (i) (nth i lambdalist)) argorder))))
(initialize-extended-method-check #'generic-function-argument-precedence-order)

;; Not in MOP.
(fmakunbound 'generic-function-argorder)
(defgeneric generic-function-argorder (generic-function)
  (:method ((gf generic-function))
    (let* ((lambdalist (generic-function-lambda-list gf))
           (signature (generic-function-signature gf))
           (reqnum (sig-req-num signature))
           (reqvars (subseq lambdalist 0 reqnum))
           (argument-precedence-order (generic-function-argument-precedence-order gf)))
      (generic-function-argument-precedence-order-to-argorder
        argument-precedence-order reqnum reqvars
        #'(lambda (detail errorstring &rest arguments)
            (declare (ignore detail))
            (error (TEXT "Invalid ~S result ~S: ~A")
                   'generic-function-argument-precedence-order argument-precedence-order
                   (apply #'format nil errorstring arguments))))))
  (:method ((gf standard-generic-function))
    (check-generic-function-initialized gf)
    (when (eq (std-gf-signature gf) (sys::%unbound))
      (error (TEXT "~S: the lambda-list of ~S is not yet known")
             'generic-function-argument-precedence-order gf))
    (std-gf-argorder gf)))
(setq |#'generic-function-argorder| #'generic-function-argorder)

;; MOP p. 79
(fmakunbound 'generic-function-declarations)
(defgeneric generic-function-declarations (generic-function)
  (:method ((gf standard-generic-function))
    (check-generic-function-initialized gf)
    (std-gf-declspecs gf)))
(setq |#'generic-function-declarations| #'generic-function-declarations)
(initialize-extended-method-check #'generic-function-declarations)
