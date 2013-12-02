;;;; Common Lisp Object System for CLISP
;;;; Specializers
;;;; Part n-2: Final class definitions, make/initialize-instance methods.
;;;; Bruno Haible 2004-05-15

(in-package "CLOS")

;;; ===========================================================================

(defmethod shared-initialize ((specializer specializer) situation &rest args)
  (apply #'shared-initialize-<specializer> specializer situation args))

(defmethod reinitialize-instance ((instance specializer) &rest initargs)
  (declare (ignore initargs))
  (error (TEXT "~S: The MOP does not allow reinitializing ~S")
         'reinitialize-instance instance))

;;; ===========================================================================

(defmethod shared-initialize ((specializer eql-specializer) situation &rest args
                              &key ((singleton singleton) nil))
  (declare (ignore singleton))
  (apply #'shared-initialize-<eql-specializer> specializer situation args))

;;; ===========================================================================
