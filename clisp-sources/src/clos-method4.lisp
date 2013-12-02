;;;; Common Lisp Object System for CLISP
;;;; Methods
;;;; Part n-1: Generic functions specified in the MOP.
;;;; Bruno Haible 2004-06-01

(in-package "CLOS")


;; Make creation of <standard-method> instances customizable.
(setf (fdefinition 'make-instance-<standard-method>) #'make-instance)

;; Make creation of <standard-reader-method>, <standard-writer-method>
;; instances customizable.
(setf (fdefinition 'make-instance-<standard-reader-method>) #'make-instance)
(setf (fdefinition 'make-instance-<standard-writer-method>) #'make-instance)

;; Make creation of method instances customizable.
(setf (fdefinition 'make-method-instance) #'make-instance) ; ABI
