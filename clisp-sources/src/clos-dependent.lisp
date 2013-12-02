;;;; Common Lisp Object System for CLISP
;;;; Dependent Protocol
;;;; Bruno Haible 2004-07-29

(in-package "CLOS")

;; ----------------------------------------------------------------------------

;; MOP p. 30
(defgeneric add-dependent (metaobject dependent)
  (:method ((metaobject defined-class) dependent)
    (pushnew dependent (class-listeners metaobject) :test #'eq))
  (:method ((metaobject generic-function) dependent)
    (pushnew dependent (gf-listeners metaobject) :test #'eq)))

;; MOP p. 87
(defgeneric remove-dependent (metaobject dependent)
  (:method ((metaobject defined-class) dependent)
    (setf (class-listeners metaobject)
          (delete dependent (the list (class-listeners metaobject)) :test #'eq)))
  (:method ((metaobject generic-function) dependent)
    (setf (gf-listeners metaobject)
          (delete dependent (the list (gf-listeners metaobject)) :test #'eq))))

;; MOP p. 73
(defgeneric map-dependents (metaobject function)
  (:method ((metaobject defined-class) function)
    (map-dependents-<defined-class> metaobject function))
  (:method ((metaobject generic-function) function)
    (map-dependents-<generic-function> metaobject function)))

;; MOP p. 101
(defgeneric update-dependent (metaobject dependent &rest initargs))
