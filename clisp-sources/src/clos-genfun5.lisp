;;;; Common Lisp Object System for CLISP
;;;; Generic Functions
;;;; Part n-1: Generic functions specified in the MOP.
;;;; Bruno Haible 2004-06-13

(in-package "CLOS")


;; Make creation of <standard-generic-function> instances customizable.
(setf (fdefinition 'make-instance-<standard-generic-function>) #'make-instance)

;; Make creation of generic-function instances customizable.
(setf (fdefinition 'allocate-generic-function-instance) #'allocate-instance) ; ABI
(setf (fdefinition 'make-generic-function-instance) #'make-instance) ; ABI

;;; ===========================================================================

;;; Generic function definition customization

;; MOP p. 50
(defgeneric ensure-generic-function-using-class (gf funname
                                                 &key generic-function-class
                                                      lambda-list
                                                      argument-precedence-order
                                                      method-class
                                                      method-combination
                                                      documentation
                                                      declarations
                                                      declare
                                                      environment
                                                 &allow-other-keys)
  (:method ((gf generic-function) funname &rest args)
    (apply #'ensure-generic-function-using-class-<t> gf funname args))
  (:method ((gf null) funname &rest args)
    (apply #'ensure-generic-function-using-class-<t> gf funname args)))
