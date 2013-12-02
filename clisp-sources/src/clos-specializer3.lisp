;;;; Common Lisp Object System for CLISP
;;;; Specializers
;;;; Part n-1: Generic functions specified in the MOP.
;;;; Bruno Haible 2004-05-15

(in-package "CLOS")


;; Make creation of <specializer> instances customizable.
(setf (fdefinition 'initialize-instance-<eql-specializer>) #'initialize-instance)
(setf (fdefinition 'make-instance-<eql-specializer>) #'make-instance)


;; Optimized accessors, with type checking.

(defun specializer-direct-methods-table (specializer)
  (accessor-typecheck specializer 'specializer 'specializer-direct-methods-table)
  (sys::%record-ref specializer *<specializer>-direct-methods-location*))
(defun (setf specializer-direct-methods-table) (new-value specializer)
  (accessor-typecheck specializer 'specializer '(setf specializer-direct-methods-table))
  (setf (sys::%record-ref specializer *<specializer>-direct-methods-location*) new-value))

(defun eql-specializer-singleton (specializer)
  (accessor-typecheck specializer 'eql-specializer 'eql-specializer-singleton)
  (sys::%record-ref specializer *<eql-specializer>-singleton-location*))
(defun (setf eql-specializer-singleton) (new-value specializer)
  (accessor-typecheck specializer 'eql-specializer '(setf eql-specializer-singleton))
  (setf (sys::%record-ref specializer *<eql-specializer>-singleton-location*) new-value))


;; MOP p. 103
(defgeneric specializer-direct-generic-functions (specializer)
  (:method ((specializer specializer))
    (compute-direct-generic-functions specializer)))

;; MOP p. 31
(defgeneric add-direct-method (specializer method)
  (:method ((specializer specializer) (method method))
    (add-direct-method-<specializer>-<method> specializer method)))

;; MOP p. 89
(defgeneric remove-direct-method (specializer method)
  (:method ((specializer specializer) (method method))
    (remove-direct-method-internal specializer method)))

;; MOP p. 103
(defgeneric specializer-direct-methods (specializer)
  (:method ((specializer specializer))
    (list-direct-methods specializer)))
