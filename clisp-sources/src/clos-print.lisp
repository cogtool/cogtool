;;;; Common Lisp Object System for CLISP: Classes
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


(defgeneric print-object (object stream)
  (:method ((object standard-object) stream)
    (if *print-readably*
      (let ((form (make-init-form object)))
        (if form
          (write (sys::make-load-time-eval form) :stream stream)
          (print-unreadable-object (object stream :type t :identity t))))
      (print-unreadable-object (object stream :type t :identity t)))
    object)
  (:method ((object structure-object) stream)
    (system::print-structure object stream)
    object)
  (:method ((object potential-class) stream)
    (print-object-<potential-class> object stream)
    object)
  (:method ((object forward-reference-to-class) stream)
    (print-object-<forward-reference-to-class> object stream)
    object)
  (:method ((object slot-definition) stream)
    (print-object-<slot-definition> object stream)
    object)
  (:method ((object eql-specializer) stream)
    (print-object-<eql-specializer> object stream)
    object)
  (:method ((object method-combination) stream)
    (print-object-<method-combination> object stream)
    object)
  (:method ((object standard-method) stream)
    (print-object-<standard-method> object stream)
    object)
  (:method ((object funcallable-standard-object) stream)
    (print-object-<funcallable-standard-object> object stream)
    object))

#| ;; Commented out because the example in the CLHS description of
   ;; PRINT-UNREADABLE-OBJECT leaves doubts about whether the
   ;;   "print-object object stream => object"
   ;; specification was meant as it is.
   ;; CLISP's printer ignores the value of PRINT-OBJECT anyway.

;; Check that all user-defined print-object methods return the object.
(defparameter *print-object-method-warning* t)
(defun print-object-method-warning (method object result)
  (when *print-object-method-warning*
    (let ((*print-object-method-warning* nil))
      (warn (TEXT "~S: invalid method ~S. ANSI CL requires that every ~S method returns the object as value. Expected ~S, but it returned ~S.")
            'print-object method 'print-object object result))))
(defmethod compute-effective-method ((gf (eql #'print-object))
                                     method-combination methods)
  (declare (ignore method-combination))
  (multiple-value-bind (form options) (call-next-method)
    (let ((object-var (gensym))
          (result-var (gensym)))
      (values `(LET ((,result-var ,form))
                 (UNLESS (EQL ,result-var ,object-var)
                   (PRINT-OBJECT-METHOD-WARNING ',(first methods) ,object-var ,result-var))
                 ,object-var)
              (cons `(:ARGUMENTS ,object-var) options)))))
|#

;; Another DEFSTRUCT hook.
(defun defstruct-remove-print-object-method (name) ; ABI
  (let ((method (find-method #'print-object nil
                             (list (find-class name) <t>) nil)))
    (when method (remove-method #'print-object method))))
