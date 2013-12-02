;;;; Common Lisp Object System for CLISP
;;;; Class metaobjects
;;;; Part n-1: Generic functions specified in the MOP.
;;;; Bruno Haible 2004-05-25
;;;; Sam Steingold 2005

(in-package "CLOS")

;;; ===========================================================================

;; Make creation of <defined-class> instances customizable.

;; Installing the accessor methods can only be done after a class has been
;; initialized, but must be done in a _primary_ initialize-instance method,
;; so that it doesn't interfere with :after/:around methods that a user could
;; install. See MOP p. 60.
(defmethod initialize-instance ((class defined-class) &rest args)
  (declare (ignore args))
  (call-next-method) ; == (apply #'shared-initialize class 't args)
  (install-class-direct-accessors class)
  class)

(defmethod initialize-instance ((class structure-class) &rest args
                                &key ((defclass-form defclass-form))
                                &allow-other-keys)
  (if (eq defclass-form 'defstruct) ; called from DEFINE-STRUCTURE-CLASS
      ;; we do not (CALL-NEXT-METHOD) because the
      ;; INITIALIZE-INSTANCE@DEFINED-CLASS method calls
      ;; INSTALL-CLASS-DIRECT-ACCESSORS which installs slot accessors
      ;; immediately overwritten by the accessors defined by DEFSTRUCT
      (apply #'shared-initialize class 't args)
      (call-next-method))       ; initialize-instance@defined-class
  class)

(setf (fdefinition 'initialize-instance-<built-in-class>) #'initialize-instance)
(setf (fdefinition 'make-instance-<built-in-class>) #'make-instance)
(setf (fdefinition 'initialize-instance-<structure-class>) #'initialize-instance)
(setf (fdefinition 'make-instance-<structure-class>) #'make-instance)
(setf (fdefinition 'initialize-instance-<standard-class>) #'initialize-instance)
(setf (fdefinition 'make-instance-<standard-class>) #'make-instance)
(setf (fdefinition 'initialize-instance-<funcallable-standard-class>) #'initialize-instance)
(setf (fdefinition 'make-instance-<funcallable-standard-class>) #'make-instance)

;;; ===========================================================================

;;; Optimized class-xxx accessors.
;;; These are possible thanks to the :fixed-slot-locations class option.

(defun check-class-initialized (class level)
  (unless (>= (class-initialized class) level)
    (error (TEXT "The class ~S has not yet been initialized.")
           class)))

(defun check-class-finalized (class level)
  (check-class-initialized class 2)
  (unless (>= (class-initialized class) level)
    (error (TEXT "The class ~S has not yet been finalized.")
           class)))

;; Not in MOP.
(defun class-classname (class)
  (accessor-typecheck class 'potential-class 'class-classname)
  (sys::%record-ref class *<potential-class>-classname-location*))
(defun (setf class-classname) (new-value class)
  (accessor-typecheck class 'potential-class '(setf class-classname))
  (setf (sys::%record-ref class *<potential-class>-classname-location*) new-value))
;; MOP p. 76
(defgeneric class-name (class)
  (:method ((class defined-class))
    (check-class-initialized class 1)
    (class-classname class))
  (:method ((class forward-reference-to-class))
    (slot-value class '$classname)))
; No extended method check because this GF is specified in ANSI CL.
;(initialize-extended-method-check #'class-name)
;; MOP p. 92
(defgeneric (setf class-name) (new-value class)
  (:method (new-value (class potential-class))
    (unless (symbolp new-value)
      (error-of-type 'type-error
        :datum new-value :expected-type 'symbol
        (TEXT "~S: The name of a class must be a symbol, not ~S")
        '(setf class-name) new-value))
    (when (built-in-class-p class)
      (error-of-type 'error
        (TEXT "~S: The name of the built-in class ~S cannot be modified")
        '(setf class-name) class))
    (reinitialize-instance class :name new-value)
    new-value))
(initialize-extended-method-check #'(setf class-name))

;; Not in MOP.
(defun class-direct-subclasses-table (class)
  (accessor-typecheck class 'super-class 'class-direct-subclasses-table)
  (if (potential-class-p class)
    (sys::%record-ref class *<potential-class>-direct-subclasses-location*)
    (slot-value class '$direct-subclasses)))
(defun (setf class-direct-subclasses-table) (new-value class)
  (accessor-typecheck class 'super-class '(setf class-direct-subclasses-table))
  (if (potential-class-p class)
    (setf (sys::%record-ref class *<potential-class>-direct-subclasses-location*) new-value)
    (setf (slot-value class '$direct-subclasses) new-value)))
;; MOP p. 76
(defgeneric class-direct-subclasses (class)
  (:method ((class defined-class))
    (check-class-initialized class 2)
    (list-direct-subclasses class))
  (:method ((class forward-reference-to-class))
    (list-direct-subclasses class)))

;; MOP p. 76
(defgeneric class-direct-superclasses (class)
  (:method ((class defined-class))
    (check-class-initialized class 2)
    (sys::%record-ref class *<defined-class>-direct-superclasses-location*))
  (:method ((class forward-reference-to-class))
    ;; Broken MOP. Any use of this method is a bug.
    (warn (TEXT "~S being called on ~S, but class ~S is not yet defined.")
          'class-direct-superclasses class (class-name class))
    '()))
(initialize-extended-method-check #'class-direct-superclasses)
;; Not in MOP.
(defun (setf class-direct-superclasses) (new-value class)
  (accessor-typecheck class 'defined-class '(setf class-direct-superclasses))
  (setf (sys::%record-ref class *<defined-class>-direct-superclasses-location*) new-value))

;; Not in MOP.
(defun class-all-superclasses (class)
  (accessor-typecheck class 'defined-class 'class-all-superclasses)
  (sys::%record-ref class *<defined-class>-all-superclasses-location*))
(defun (setf class-all-superclasses) (new-value class)
  (accessor-typecheck class 'defined-class '(setf class-all-superclasses))
  (setf (sys::%record-ref class *<defined-class>-all-superclasses-location*) new-value))

;; MOP p. 76
(defgeneric class-precedence-list (class)
  (:method ((class defined-class))
    (check-class-finalized class 3)
    (sys::%record-ref class *<defined-class>-precedence-list-location*)))
(initialize-extended-method-check #'class-precedence-list)
;; Not in MOP.
(defun (setf class-precedence-list) (new-value class)
  (accessor-typecheck class 'defined-class '(setf class-precedence-list))
  (setf (sys::%record-ref class *<defined-class>-precedence-list-location*) new-value))

;; MOP p. 75
(defgeneric class-direct-slots (class)
  (:method ((class defined-class))
    (check-class-initialized class 2)
    (sys::%record-ref class *<defined-class>-direct-slots-location*))
  (:method ((class forward-reference-to-class))
    ;; Broken MOP. Any use of this method is a bug.
    (warn (TEXT "~S being called on ~S, but class ~S is not yet defined.")
          'class-direct-slots class (class-name class))
    '()))
(initialize-extended-method-check #'class-direct-slots)
;; Not in MOP.
(defun (setf class-direct-slots) (new-value class)
  (accessor-typecheck class 'defined-class '(setf class-direct-slots))
  (setf (sys::%record-ref class *<defined-class>-direct-slots-location*) new-value))

;; MOP p. 77
(defgeneric class-slots (class)
  (:method ((class defined-class))
    (check-class-finalized class 5)
    (sys::%record-ref class *<defined-class>-slots-location*)))
(initialize-extended-method-check #'class-slots)
;; Not in MOP.
(defun (setf class-slots) (new-value class)
  (accessor-typecheck class 'defined-class '(setf class-slots))
  (setf (sys::%record-ref class *<defined-class>-slots-location*) new-value))

;; Not in MOP.
(defun class-slot-location-table (class)
  (accessor-typecheck class 'defined-class 'class-slot-location-table)
  (sys::%record-ref class *<defined-class>-slot-location-table-location*))
(defun (setf class-slot-location-table) (new-value class)
  (accessor-typecheck class 'defined-class '(setf class-slot-location-table))
  (setf (sys::%record-ref class *<defined-class>-slot-location-table-location*) new-value))

;; MOP p. 75
(defgeneric class-direct-default-initargs (class)
  (:method ((class defined-class))
    (check-class-initialized class 2)
    (sys::%record-ref class *<defined-class>-direct-default-initargs-location*))
  (:method ((class forward-reference-to-class))
    ;; Broken MOP. Any use of this method is a bug.
    (warn (TEXT "~S being called on ~S, but class ~S is not yet defined.")
          'class-direct-default-initargs class (class-name class))
    '()))
(initialize-extended-method-check #'class-direct-default-initargs)
;; Not in MOP.
(defun (setf class-direct-default-initargs) (new-value class)
  (accessor-typecheck class 'defined-class '(setf class-direct-default-initargs))
  (setf (sys::%record-ref class *<defined-class>-direct-default-initargs-location*) new-value))

;; MOP p. 75
(defgeneric class-default-initargs (class)
  (:method ((class defined-class))
    (check-class-finalized class 6)
    (sys::%record-ref class *<defined-class>-default-initargs-location*)))
(initialize-extended-method-check #'class-default-initargs)
;; Not in MOP.
(defun (setf class-default-initargs) (new-value class)
  (accessor-typecheck class 'defined-class '(setf class-default-initargs))
  (setf (sys::%record-ref class *<defined-class>-default-initargs-location*) new-value))

;; Not in MOP.
(defun class-documentation (class)
  (accessor-typecheck class 'defined-class 'class-documentation)
  (sys::%record-ref class *<defined-class>-documentation-location*))
(defun (setf class-documentation) (new-value class)
  (accessor-typecheck class 'defined-class '(setf class-documentation))
  (setf (sys::%record-ref class *<defined-class>-documentation-location*) new-value))

;; Not in MOP.
(defun class-listeners (class)
  (accessor-typecheck class 'defined-class 'class-listeners)
  (sys::%record-ref class *<defined-class>-listeners-location*))
(defun (setf class-listeners) (new-value class)
  (accessor-typecheck class 'defined-class '(setf class-listeners))
  (setf (sys::%record-ref class *<defined-class>-listeners-location*) new-value))

;; Not in MOP.
(defun class-initialized (class)
  (accessor-typecheck class 'defined-class 'class-initialized)
  (sys::%record-ref class *<defined-class>-initialized-location*))
(defun (setf class-initialized) (new-value class)
  (accessor-typecheck class 'defined-class '(setf class-initialized))
  (setf (sys::%record-ref class *<defined-class>-initialized-location*) new-value))

;; Not in MOP.
(defun class-subclass-of-stablehash-p (class)
  (accessor-typecheck class 'slotted-class 'class-subclass-of-stablehash-p)
  (sys::%record-ref class *<slotted-class>-subclass-of-stablehash-p-location*))
(defun (setf class-subclass-of-stablehash-p) (new-value class)
  (accessor-typecheck class 'slotted-class '(setf class-subclass-of-stablehash-p))
  (setf (sys::%record-ref class *<slotted-class>-subclass-of-stablehash-p-location*) new-value))

;; Not in MOP.
(defun class-generic-accessors (class)
  (accessor-typecheck class 'slotted-class 'class-generic-accessors)
  (sys::%record-ref class *<slotted-class>-generic-accessors-location*))
(defun (setf class-generic-accessors) (new-value class)
  (accessor-typecheck class 'slotted-class '(setf class-generic-accessors))
  (setf (sys::%record-ref class *<slotted-class>-generic-accessors-location*) new-value))

;; Not in MOP.
(defun class-direct-accessors (class)
  (accessor-typecheck class 'slotted-class 'class-direct-accessors)
  (sys::%record-ref class *<slotted-class>-direct-accessors-location*))
(defun (setf class-direct-accessors) (new-value class)
  (accessor-typecheck class 'slotted-class '(setf class-direct-accessors))
  (setf (sys::%record-ref class *<slotted-class>-direct-accessors-location*) new-value))

;; Not in MOP.
(defun class-valid-initargs-from-slots (class)
  (accessor-typecheck class 'slotted-class 'class-valid-initargs-from-slots)
  (sys::%record-ref class *<slotted-class>-valid-initargs-from-slots-location*))
(defun (setf class-valid-initargs-from-slots) (new-value class)
  (accessor-typecheck class 'slotted-class '(setf class-valid-initargs-from-slots))
  (setf (sys::%record-ref class *<slotted-class>-valid-initargs-from-slots-location*) new-value))

;; Not in MOP.
(defun class-instance-size (class)
  (accessor-typecheck class 'slotted-class 'class-instance-size)
  (sys::%record-ref class *<slotted-class>-instance-size-location*))
(defun (setf class-instance-size) (new-value class)
  (accessor-typecheck class 'slotted-class '(setf class-instance-size))
  (setf (sys::%record-ref class *<slotted-class>-instance-size-location*) new-value))

;; Not in MOP.
(defun class-names (class)
  (accessor-typecheck class 'structure-class 'class-names)
  (sys::%record-ref class *<structure-class>-names-location*))
(defun (setf class-names) (new-value class)
  (accessor-typecheck class 'structure-class '(setf class-names))
  (setf (sys::%record-ref class *<structure-class>-names-location*) new-value))

;; Not in MOP.
(defun class-kconstructor (class)
  (accessor-typecheck class 'structure-class 'class-kconstructor)
  (sys::%record-ref class *<structure-class>-kconstructor-location*))
(defun (setf class-kconstructor) (new-value class)
  (accessor-typecheck class 'structure-class '(setf class-kconstructor))
  (setf (sys::%record-ref class *<structure-class>-kconstructor-location*) new-value))

;; Not in MOP.
(defun class-boa-constructors (class)
  (accessor-typecheck class 'structure-class 'class-boa-constructors)
  (sys::%record-ref class *<structure-class>-boa-constructors-location*))
(defun (setf class-boa-constructors) (new-value class)
  (accessor-typecheck class 'structure-class '(setf class-boa-constructors))
  (setf (sys::%record-ref class *<structure-class>-boa-constructors-location*) new-value))

;; Not in MOP.
(defun class-copier (class)
  (accessor-typecheck class 'structure-class 'class-copier)
  (sys::%record-ref class *<structure-class>-copier-location*))
(defun (setf class-copier) (new-value class)
  (accessor-typecheck class 'structure-class '(setf class-copier))
  (setf (sys::%record-ref class *<structure-class>-copier-location*) new-value))

;; Not in MOP.
(defun class-predicate (class)
  (accessor-typecheck class 'structure-class 'class-predicate)
  (sys::%record-ref class *<structure-class>-predicate-location*))
(defun (setf class-predicate) (new-value class)
  (accessor-typecheck class 'structure-class '(setf class-predicate))
  (setf (sys::%record-ref class *<structure-class>-predicate-location*) new-value))

;; Not in MOP.
(defun class-current-version (class)
  (accessor-typecheck class 'semi-standard-class 'class-current-version)
  (sys::%record-ref class *<semi-standard-class>-current-version-location*))
(defun (setf class-current-version) (new-value class)
  (accessor-typecheck class 'semi-standard-class '(setf class-current-version))
  (setf (sys::%record-ref class *<semi-standard-class>-current-version-location*) new-value))

;; Not in MOP.
(defun class-funcallablep (class)
  (accessor-typecheck class 'semi-standard-class 'class-funcallablep)
  (sys::%record-ref class *<semi-standard-class>-funcallablep-location*))
(defun (setf class-funcallablep) (new-value class)
  (accessor-typecheck class 'semi-standard-class '(setf class-funcallablep))
  (setf (sys::%record-ref class *<semi-standard-class>-funcallablep-location*) new-value))

;; Not in MOP.
(defun class-fixed-slot-locations (class)
  (accessor-typecheck class 'semi-standard-class 'class-fixed-slot-locations)
  (sys::%record-ref class *<semi-standard-class>-fixed-slot-locations-location*))
(defun (setf class-fixed-slot-locations) (new-value class)
  (accessor-typecheck class 'semi-standard-class '(setf class-fixed-slot-locations))
  (setf (sys::%record-ref class *<semi-standard-class>-fixed-slot-locations-location*) new-value))

;; Not in MOP.
(defun class-instantiated (class)
  (accessor-typecheck class 'semi-standard-class 'class-instantiated)
  (sys::%record-ref class *<semi-standard-class>-instantiated-location*))
(defun (setf class-instantiated) (new-value class)
  (accessor-typecheck class 'semi-standard-class '(setf class-instantiated))
  (setf (sys::%record-ref class *<semi-standard-class>-instantiated-location*) new-value))

;; Not in MOP.
(defun class-direct-instance-specializers-table (class)
  (accessor-typecheck class 'semi-standard-class 'class-direct-instance-specializers-table)
  (sys::%record-ref class *<semi-standard-class>-direct-instance-specializers-location*))
(defun (setf class-direct-instance-specializers-table) (new-value class)
  (accessor-typecheck class 'semi-standard-class '(setf class-direct-instance-specializers-table))
  (setf (sys::%record-ref class *<semi-standard-class>-direct-instance-specializers-location*) new-value))

;; Not in MOP.
(defun class-finalized-direct-subclasses-table (class)
  (accessor-typecheck class 'semi-standard-class 'class-finalized-direct-subclasses-table)
  (sys::%record-ref class *<semi-standard-class>-finalized-direct-subclasses-location*))
(defun (setf class-finalized-direct-subclasses-table) (new-value class)
  (accessor-typecheck class 'semi-standard-class '(setf class-finalized-direct-subclasses-table))
  (setf (sys::%record-ref class *<semi-standard-class>-finalized-direct-subclasses-location*) new-value))

;; MOP p. 77
(defgeneric class-prototype (class)
  (:method ((class semi-standard-class))
    (check-class-finalized class 6)
    (or (sys::%record-ref class *<semi-standard-class>-prototype-location*)
        (setf (sys::%record-ref class *<semi-standard-class>-prototype-location*)
              (let ((old-instantiated (class-instantiated class)))
                (prog1
                  (clos::%allocate-instance class)
                  ;; The allocation of the prototype doesn't need to flag the
                  ;; class as being instantiated, because 1. the prototype is
                  ;; thrown away when the class is redefined, 2. we don't want
                  ;; a redefinition with nonexistent or non-finalized
                  ;; superclasses to succeed despite of the prototype.
                  (setf (class-instantiated class) old-instantiated))))))
  (:method ((class built-in-class))
    (let ((prototype (sys::%record-ref class *<built-in-class>-prototype-location*)))
      (if (eq prototype (sys::%unbound))
        (error (TEXT "~S: ~S is an abstract class and therefore does not have a direct instance")
               'class-prototype class)
        prototype)))
  ;; CLISP extension:
  (:method ((class structure-class))
    (or (sys::%record-ref class *<structure-class>-prototype-location*)
        (setf (sys::%record-ref class *<structure-class>-prototype-location*)
              (clos::%allocate-instance class)))))
(initialize-extended-method-check #'class-prototype)
;; Not in MOP.
(defun (setf class-prototype) (new-value class)
  (accessor-typecheck class 'semi-standard-class '(setf class-prototype))
  (setf (sys::%record-ref class *<semi-standard-class>-prototype-location*) new-value))

;;; ===========================================================================

;;; Class Specification Protocol

;; Not in MOP.
(defgeneric compute-direct-slot-definition-initargs (class &rest slot-spec)
  (:method ((class defined-class) &rest slot-spec)
    slot-spec))

;;; ===========================================================================

;;; Class Finalization Protocol

;; MOP p. 76
(defgeneric class-finalized-p (class)
  (:method ((class defined-class))
    (= (class-initialized class) 6))
  (:method ((class forward-reference-to-class))
    nil)
  ;; CLISP extension: Convenience method on symbols.
  (:method ((name symbol))
    (class-finalized-p (find-class name))))
(initialize-extended-method-check #'class-finalized-p)

;; MOP p. 54
(defgeneric finalize-inheritance (class)
  (:method ((class semi-standard-class))
    (finalize-inheritance-<semi-standard-class> class))
  ;; CLISP extension: No-op method on other classes.
  (:method ((class defined-class))
    class)
  ;; CLISP extension: Convenience method on symbols.
  (:method ((name symbol))
    (finalize-inheritance (find-class name))))
(initialize-extended-method-check #'finalize-inheritance)

;; MOP p. 38
(defgeneric compute-class-precedence-list (class)
  (:method ((class defined-class))
    (compute-class-precedence-list-<defined-class> class)))

;; Not in MOP.
(defgeneric compute-effective-slot-definition-initargs (class direct-slot-definitions)
  (:method ((class defined-class) direct-slot-definitions)
    (compute-effective-slot-definition-initargs-<defined-class> class direct-slot-definitions)))

;; MOP p. 42
(defgeneric compute-effective-slot-definition (class slotname direct-slot-definitions)
  (:method ((class defined-class) slotname direct-slot-definitions)
    (compute-effective-slot-definition-<defined-class> class slotname direct-slot-definitions)))

;; MOP p. 43
(defgeneric compute-slots (class)
  (:method ((class semi-standard-class))
    (compute-slots-<defined-class>-primary class))
  (:method :around ((class semi-standard-class))
    (compute-slots-<slotted-class>-around class
      #'(lambda (c) (call-next-method c)))))

;; MOP p. 39
(defgeneric compute-default-initargs (class)
  (:method ((class defined-class))
    (compute-default-initargs-<defined-class> class)))

;;; ===========================================================================

;;; Class definition customization

;; MOP p. 47
(defgeneric ensure-class-using-class (class name
                                      &key metaclass
                                           direct-superclasses
                                           direct-slots
                                           direct-default-initargs
                                           documentation
                                           ; CLISP specific extension:
                                           fixed-slot-locations
                                      &allow-other-keys)
  (:method ((class potential-class) name &rest args)
    (apply #'ensure-class-using-class-<t> class name args))
  (:method ((class null) name &rest args)
    (apply #'ensure-class-using-class-<t> class name args)))

;; MOP p. 102
(defgeneric validate-superclass (class superclass)
  (:method ((class potential-class) (superclass potential-class))
    (or (eq superclass <t>)
        (eq (class-of class) (class-of superclass))
        (and (eq (class-of class) <funcallable-standard-class>)
             (eq (class-of superclass) <standard-class>))
        ;; This makes no sense: If the superclass is a
        ;; funcallable-standard-class, it is a subclass of FUNCTION,
        ;; therefore class will become a subclass of FUNCTION too, but there
        ;; is no way to FUNCALL or APPLY it. Where did the MOP authors have
        ;; their brain here?
        (and (eq (class-of class) <standard-class>)
             (eq (class-of superclass) <funcallable-standard-class>))
        ;; Needed for clos-genfun1.lisp:
        (and (eq superclass <function>)
             (eq (class-classname class) 'funcallable-standard-object))
        ;; CLISP specific extension:
        (subclassp (class-of class) (class-of superclass)))))

;;; ===========================================================================

;;; Subclass relationship change notification

;; MOP p. 32
(defgeneric add-direct-subclass (class subclass)
  (:method ((class super-class) (subclass potential-class))
    (add-direct-subclass-internal class subclass)))

;; MOP p. 90
(defgeneric remove-direct-subclass (class subclass)
  (:method ((class super-class) (subclass potential-class))
    (remove-direct-subclass-internal class subclass)))

;;; ===========================================================================

;;; Accessor definition customization

;; MOP p. 86
(defgeneric reader-method-class (class direct-slot &rest initargs)
  (:method ((class defined-class) direct-slot &rest initargs)
    (declare (ignore direct-slot initargs))
    <standard-reader-method>))

;; MOP p. 103
(defgeneric writer-method-class (class direct-slot &rest initargs)
  (:method ((class defined-class) direct-slot &rest initargs)
    (declare (ignore direct-slot initargs))
    <standard-writer-method>))
