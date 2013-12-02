;;;; Common Lisp Object System for CLISP
;;;; Class metaobjects
;;;; Part n-2: Final class definitions, make/initialize-instance methods.
;;;; Bruno Haible 2004-05-25
;;;; Sam Steingold 2005

(in-package "CLOS")

;;; ===========================================================================

;;; Lift the initialization protocol.

(defmethod shared-initialize ((class potential-class) situation &rest args
                              &key name)
  (declare (ignore name))
  (apply #'shared-initialize-<potential-class> class situation args))

;;; ===========================================================================

(defmethod shared-initialize ((class defined-class) situation &rest args
                              &key name direct-superclasses direct-slots
                                   direct-default-initargs documentation)
  (declare (ignore name direct-superclasses direct-slots
                   direct-default-initargs documentation))
  (apply #'shared-initialize-<defined-class> class situation args))

(defmethod reinitialize-instance ((class defined-class) &rest args
                                  &key &allow-other-keys)
  (apply #'reinitialize-instance-<defined-class> class args))

;;; ===========================================================================

(defmethod shared-initialize ((class built-in-class) situation &rest args
                              &key name direct-superclasses
                                   ((prototype prototype) nil))
  (declare (ignore name direct-superclasses prototype))
  (apply #'shared-initialize-<built-in-class> class situation args))

;;; ===========================================================================

(defmethod shared-initialize ((class structure-class) situation &rest args
                              &key name direct-superclasses direct-slots
                                   direct-default-initargs documentation
                                   (generic-accessors t)
                                   ((direct-slots direct-slots-as-metaobjects) '())
                                   ((names names) nil)
                                   ((kconstructor kconstructor) nil)
                                   ((boa-constructors boa-constructors) '())
                                   ((copier copier) nil)
                                   ((predicate predicate) nil)
                                   ((slots slots) '()) ((size size) 1))
  (declare (ignore name direct-superclasses direct-slots
                   direct-default-initargs documentation generic-accessors
                   direct-slots-as-metaobjects names kconstructor
                   boa-constructors copier predicate slots size))
  (apply #'shared-initialize-<structure-class> class situation args))

;;; ===========================================================================

(defmethod shared-initialize ((class standard-class) situation &rest args
                              &key name direct-superclasses direct-slots
                                   direct-default-initargs documentation
                                   (generic-accessors t)
                                   (fixed-slot-locations nil))
  (declare (ignore name direct-superclasses direct-slots
                   direct-default-initargs documentation generic-accessors
                   fixed-slot-locations))
  (apply #'shared-initialize-<standard-class> class situation args))

;;; ===========================================================================

(defmethod shared-initialize ((class funcallable-standard-class) situation &rest args
                              &key name direct-superclasses direct-slots
                                   direct-default-initargs documentation
                                   (generic-accessors t)
                                   (fixed-slot-locations nil))
  (declare (ignore name direct-superclasses direct-slots
                   direct-default-initargs documentation generic-accessors
                   fixed-slot-locations))
  (apply #'shared-initialize-<funcallable-standard-class> class situation args))

;;; ===========================================================================

;; Now that all the predefined subclasses of <defined-class> have been defined,
;; CLASS-OF can work on all existing <defined-class> instances. Therefore now,
;; not earlier, it's possible to pass these <defined-class> instances to
;; generic functions.
