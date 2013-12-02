;;;; Common Lisp Object System for CLISP
;;;; Slot Definition metaobjects
;;;; Part n-2: Final class definitions, make/initialize-instance methods.
;;;; Bruno Haible 2004-04-18

(in-package "CLOS")

;;; ===========================================================================

;;; Lift the initialization protocol.

(defmethod initialize-instance ((slotdef slot-definition) &rest args
                                &key name initform initfunction initargs
                                     type allocation documentation
                                     ((inheritable-initer inheritable-initer))
                                     ((inheritable-doc inheritable-doc)))
  (declare (ignore name initform initfunction initargs type allocation
                   documentation inheritable-initer inheritable-doc))
  (apply #'initialize-instance-<slot-definition> slotdef args))

(defmethod initialize-instance ((slotdef direct-slot-definition) &rest args
                                &key name initform initfunction initargs
                                     type allocation documentation
                                     ((inheritable-initer inheritable-initer))
                                     ((inheritable-doc inheritable-doc))
                                     readers writers
                                     ((defclass-form defclass-form)))
  (declare (ignore name initform initfunction initargs type allocation
                   documentation inheritable-initer inheritable-doc readers
                   writers defclass-form))
  (apply #'initialize-instance-<direct-slot-definition> slotdef args))

(defmethod initialize-instance ((slotdef effective-slot-definition) &rest args
                                &key name initform initfunction initargs
                                     type allocation documentation
                                     ((inheritable-initer inheritable-initer))
                                     ((inheritable-doc inheritable-doc)))
  (declare (ignore name initform initfunction initargs type allocation
                   documentation inheritable-initer inheritable-doc))
  (apply #'initialize-instance-<effective-slot-definition> slotdef args))

(defmethod reinitialize-instance ((instance slot-definition) &rest initargs)
  (declare (ignore initargs))
  (error (TEXT "~S: The MOP does not allow reinitializing ~S")
         'reinitialize-instance instance))


;;; ===========================================================================

;;; Now the concrete classes for <standard-class> and <structure-class> slots.

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance ((slotdef standard-direct-slot-definition) &rest args)
  (apply #'initialize-instance-<standard-direct-slot-definition> slotdef args))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance ((slotdef standard-effective-slot-definition) &rest args)
  (apply #'initialize-instance-<standard-effective-slot-definition> slotdef args))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance ((slotdef structure-direct-slot-definition) &rest args)
  (apply #'initialize-instance-<structure-direct-slot-definition> slotdef args))

;;; ---------------------------------------------------------------------------

(defun structure-effective-slot-definition-readonly (slotdef)
  (slot-value slotdef '$readonly))
(defun (setf structure-effective-slot-definition-readonly) (new-value slotdef)
  (setf (slot-value slotdef '$readonly) new-value))
(defmethod initialize-instance ((slotdef structure-effective-slot-definition) &rest args)
  (apply #'initialize-instance-<structure-effective-slot-definition> slotdef args))

;;; ===========================================================================

;; Now that all the predefined subclasses of <slot-definition> have been
;; defined, CLASS-OF can work on all existing <slot-definition> instances.
;; Therefore now, not earlier, it's possible to pass these <slot-definition>
;; instances to generic functions.
