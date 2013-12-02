;;;; Common Lisp Object System for CLISP
;;;; Method Combination
;;;; Part n-1: Generic functions specified in the MOP.
;;;; Bruno Haible 2004-06-10

(in-package "CLOS")


;; Make creation of <method-combination> instances customizable.
(setf (fdefinition 'make-instance-<method-combination>) #'make-instance)

;; MOP p. 54
(defgeneric find-method-combination (generic-function name options)
  (:method ((gf generic-function) (name symbol) options)
    (find-method-combination-<generic-function>-<symbol> gf name options)))
(initialize-extended-method-check #'find-method-combination)
