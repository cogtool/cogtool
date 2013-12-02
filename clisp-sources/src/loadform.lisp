;;; MAKE-LOAD-FORM for CLISP
;;; Sam Steingold 2001-2004
;;; Bruno Haible 2004

;; this could have been placed in in clos.lisp,
;; but `make-init-form' uses conditions

(in-package "CLOS")

(defun make-load-form-saving-slots
    (object &key environment
     (slot-names
      (let ((slots (class-slots (class-of object))))
        (etypecase object
          (standard-object
           (mapcan (lambda (slot)
                     (when (eq (slot-definition-allocation slot) ':instance)
                       (list (slot-definition-name slot))))
                   slots))
          (structure-object (mapcar #'slot-definition-name slots))))))
  (declare (ignore environment))
  (values `(allocate-instance (find-class ',(class-name (class-of object))))
          `(progn
            (setf ,@(mapcan (lambda (slot)
                              (when (slot-boundp object slot)
                                `((slot-value ,object ',slot)
                                  ',(slot-value object slot))))
                            slot-names))
            (initialize-instance ,object))))

;; Condition type thrown by make-load-form.
;; It's not sufficient to rely on method-call-error because a test like
;;    (eq (method-call-error-generic-function err) #'make-load-form)
;; doesn't work when make-load-form is traced.
(define-condition missing-load-form (error)
  (($object :initarg :object :reader missing-load-form-object)))
(define-condition simple-missing-load-form (simple-error missing-load-form) ())

(defun signal-missing-load-form (object)
  (let ((class (class-name (class-of object))))
    (error-of-type 'simple-missing-load-form :object object
      (TEXT "A method on ~S for class ~S is necessary for externalizing the object ~S, according to ANSI CL 3.2.4.4, but no such method is defined.")
      'make-load-form class object)))

(defgeneric make-load-form (object &optional environment)
  ;; <http://www.lisp.org/HyperSpec/Body/stagenfun_make-load-form.html>
  ;; "The methods specialized on standard-object, structure-object, and
  ;;  condition all signal an error of type error."
  (:method ((object standard-object) &optional environment)
    (declare (ignore environment))
    (signal-missing-load-form object))
  (:method ((object structure-object) &optional environment)
    (declare (ignore environment))
    (signal-missing-load-form object))
  (:method ((object condition) &optional environment)
    (declare (ignore environment))
    (signal-missing-load-form object))
  (:method ((object defined-class) &optional environment)
    (declare (ignore environment))
    ;; TODO: Implement as described in CLHS
    `(find-class ',(class-name object))))

(defun mlf-init-function (object)
  (multiple-value-bind (creation-form initialization-form)
      (make-load-form object)
    (let ((funname
            (gensym
              (sys::string-concat
                "CREATE-INSTANCE-OF-<"
                (write-to-string (class-name (class-of object)) :readably nil)
                ">-"))))
      `(FUNCTION ,funname
         (LAMBDA ()
           ,@(if (and compiler::*compiling* compiler::*compiling-from-file*)
               '((DECLARE (COMPILE)))
               '())
           ,(if initialization-form
              (let ((var (gensym "MAKE-LOAD-FORM-")))
                `(LET ((,var ,creation-form))
                   ,(sublis `((',object . ,var) (,object . ,var))
                            initialization-form
                            :test #'equal)
                   ,var))
              creation-form))))))

(defun make-init-form (object)
  (when compiler::*load-forms*
    (multiple-value-bind (form found-p)
        (gethash object compiler::*load-forms*)
      (if found-p
        form
        (setf (gethash object compiler::*load-forms*)
              (block compute-init-form
                (handler-bind
                  ((missing-load-form
                     #'(lambda (err)
                         (when (eql (missing-load-form-object err) object)
                           (return-from compute-init-form nil)))))
                  `(funcall ,(eval (mlf-init-function object))))))))))
