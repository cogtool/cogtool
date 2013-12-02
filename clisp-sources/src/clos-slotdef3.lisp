;;;; Common Lisp Object System for CLISP
;;;; Slot Definition metaobjects
;;;; Part n-1: Generic functions specified in the MOP.
;;;; Bruno Haible 2004-04-18

(in-package "CLOS")


;; Make creation of <slot-definition> instances customizable.
(setf (fdefinition 'make-instance-<standard-direct-slot-definition>) #'make-instance)
(setf (fdefinition 'make-instance-<standard-effective-slot-definition>) #'make-instance)
(setf (fdefinition 'make-instance-<structure-direct-slot-definition>) #'make-instance)
(setf (fdefinition 'make-instance-<structure-effective-slot-definition>) #'make-instance)


#| ;;; Unoptimized slot-definition-xxx accessors.

;; MOP p. 84
(defgeneric slot-definition-name (slotdef)
  (:method ((slotdef slot-definition))
    (slot-value slotdef '$name)))
(initialize-extended-method-check #'slot-definition-name)
(defun (setf slot-definition-name) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-name))
  (setf (slot-value slotdef '$name) new-value))

(defun slot-definition-inheritable-initer (slotdef)
  (accessor-typecheck slotdef 'slot-definition 'slot-definition-inheritable-initer)
  (slot-value slotdef '$inheritable-initer))

;; MOP p. 84
(defgeneric slot-definition-initform (slotdef)
  (:method ((slotdef slot-definition))
    (inheritable-slot-definition-initform (slot-value slotdef '$inheritable-initer))))
(initialize-extended-method-check #'slot-definition-initform)
(defun (setf slot-definition-initform) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-initform))
  (setf (inheritable-slot-definition-initform (slot-value slotdef '$inheritable-initer)) new-value))

;; MOP p. 84
(defgeneric slot-definition-initfunction (slotdef)
  (:method ((slotdef slot-definition))
    (inheritable-slot-definition-initfunction (slot-value slotdef '$inheritable-initer))))
(initialize-extended-method-check #'slot-definition-initfunction)
(defun (setf slot-definition-initfunction) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-initfunction))
  (setf (inheritable-slot-definition-initfunction (slot-value slotdef '$inheritable-initer)) new-value))

;; MOP p. 84
(defgeneric slot-definition-initargs (slotdef)
  (:method ((slotdef slot-definition))
    (slot-value slotdef '$initargs)))
(initialize-extended-method-check #'slot-definition-initargs)
(defun (setf slot-definition-initargs) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-initargs))
  (setf (slot-value slotdef '$initargs) new-value))

;; MOP p. 85
(defgeneric slot-definition-type (slotdef)
  (:method ((slotdef slot-definition))
    (slot-value slotdef '$type)))
(initialize-extended-method-check #'slot-definition-type)
(defun (setf slot-definition-type) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-type))
  (setf (slot-value slotdef '$type) new-value))

;; MOP p. 84
(defgeneric slot-definition-allocation (slotdef)
  (:method ((slotdef slot-definition))
    (slot-value slotdef '$allocation)))
(initialize-extended-method-check #'slot-definition-allocation)
(defun (setf slot-definition-allocation) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-allocation))
  (setf (slot-value slotdef '$allocation) new-value))

(defun slot-definition-inheritable-doc (slotdef)
  (accessor-typecheck slotdef 'slot-definition 'slot-definition-inheritable-doc)
  (slot-value slotdef '$inheritable-doc))

(defun slot-definition-documentation (slotdef)
  (accessor-typecheck slotdef 'slot-definition 'slot-definition-documentation)
  (inheritable-slot-definition-documentation (slot-value slotdef '$inheritable-doc)))
(defun (setf slot-definition-documentation) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-documentation))
  (setf (inheritable-slot-definition-documentation (slot-value slotdef '$inheritable-doc)) new-value))

;; MOP p. 85
(defgeneric slot-definition-readers (slotdef)
  (:method ((slotdef direct-slot-definition))
    (slot-value slotdef '$readers)))
(initialize-extended-method-check #'slot-definition-readers)
(defun (setf slot-definition-readers) (new-value slotdef)
  (accessor-typecheck slotdef 'direct-slot-definition '(setf slot-definition-readers))
  (setf (slot-value slotdef '$readers) new-value))

;; MOP p. 85
(defgeneric slot-definition-writers (slotdef)
  (:method ((slotdef direct-slot-definition))
    (slot-value slotdef '$writers)))
(initialize-extended-method-check #'slot-definition-writers)
(defun (setf slot-definition-writers) (new-value slotdef)
  (accessor-typecheck slotdef 'direct-slot-definition '(setf slot-definition-writers))
  (setf (slot-value slotdef '$writers) new-value))

;; MOP p. 86
(defgeneric slot-definition-location (slotdef)
  (:method ((slotdef effective-slot-definition))
    (slot-value slotdef '$location)))
(initialize-extended-method-check #'slot-definition-location)
(defun (setf slot-definition-location) (new-value slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition '(setf slot-definition-location))
  (setf (slot-value slotdef '$location) new-value))

(defun slot-definition-efm-svuc (slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition 'slot-definition-efm-svuc)
  (slot-value slotdef '$efm-svuc))
(defun (setf slot-definition-efm-svuc) (new-value slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition '(setf slot-definition-efm-svuc))
  (setf (slot-value slotdef '$efm-svuc) new-value))

(defun slot-definition-efm-ssvuc (slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition 'slot-definition-efm-ssvuc)
  (slot-value slotdef '$efm-ssvuc))
(defun (setf slot-definition-efm-ssvuc) (new-value slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition '(setf slot-definition-efm-ssvuc))
  (setf (slot-value slotdef '$efm-ssvuc) new-value))

(defun slot-definition-efm-sbuc (slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition 'slot-definition-efm-sbuc)
  (slot-value slotdef '$efm-sbuc))
(defun (setf slot-definition-efm-sbuc) (new-value slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition '(setf slot-definition-efm-sbuc))
  (setf (slot-value slotdef '$efm-sbuc) new-value))

(defun slot-definition-efm-smuc (slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition 'slot-definition-efm-smuc)
  (slot-value slotdef '$efm-smuc))
(defun (setf slot-definition-efm-smuc) (new-value slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition '(setf slot-definition-efm-smuc))
  (setf (slot-value slotdef '$efm-smuc) new-value))

|#

;;; Optimized slot-definition-xxx accessors.
;;; These are possible thanks to the :fixed-slot-locations class option.

;; MOP p. 84
(defgeneric slot-definition-name (slotdef)
  (:method ((slotdef slot-definition))
    (sys::%record-ref slotdef *<slot-definition>-name-location*)))
(initialize-extended-method-check #'slot-definition-name)
;; Not in MOP.
(defun (setf slot-definition-name) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-name))
  (setf (sys::%record-ref slotdef *<slot-definition>-name-location*) new-value))

;; Not in MOP.
(defun slot-definition-inheritable-initer (slotdef)
  (accessor-typecheck slotdef 'slot-definition 'slot-definition-inheritable-initer)
  (sys::%record-ref slotdef *<slot-definition>-inheritable-initer-location*))
(defun (setf slot-definition-inheritable-initer) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-inheritable-initer))
  (setf (sys::%record-ref slotdef *<slot-definition>-inheritable-initer-location*) new-value))

;; MOP p. 84
(defgeneric slot-definition-initform (slotdef)
  (:method ((slotdef slot-definition))
    (inheritable-slot-definition-initform (sys::%record-ref slotdef *<slot-definition>-inheritable-initer-location*))))
(initialize-extended-method-check #'slot-definition-initform)
;; Not in MOP.
(defun (setf slot-definition-initform) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-initform))
  (setf (inheritable-slot-definition-initform (sys::%record-ref slotdef *<slot-definition>-inheritable-initer-location*)) new-value))

;; MOP p. 84
(defgeneric slot-definition-initfunction (slotdef)
  (:method ((slotdef slot-definition))
    (inheritable-slot-definition-initfunction (sys::%record-ref slotdef *<slot-definition>-inheritable-initer-location*))))
(initialize-extended-method-check #'slot-definition-initfunction)
;; Not in MOP.
(defun (setf slot-definition-initfunction) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-initfunction))
  (setf (inheritable-slot-definition-initfunction (sys::%record-ref slotdef *<slot-definition>-inheritable-initer-location*)) new-value))

;; MOP p. 84
(defgeneric slot-definition-initargs (slotdef)
  (:method ((slotdef slot-definition))
    (sys::%record-ref slotdef *<slot-definition>-initargs-location*)))
(initialize-extended-method-check #'slot-definition-initargs)
;; Not in MOP.
(defun (setf slot-definition-initargs) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-initargs))
  (setf (sys::%record-ref slotdef *<slot-definition>-initargs-location*) new-value))

;; MOP p. 85
(defgeneric slot-definition-type (slotdef)
  (:method ((slotdef slot-definition))
    (sys::%record-ref slotdef *<slot-definition>-type-location*)))
(initialize-extended-method-check #'slot-definition-type)
;; Not in MOP.
(defun (setf slot-definition-type) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-type))
  (setf (sys::%record-ref slotdef *<slot-definition>-type-location*) new-value))

;; MOP p. 84
(defgeneric slot-definition-allocation (slotdef)
  (:method ((slotdef slot-definition))
    (sys::%record-ref slotdef *<slot-definition>-allocation-location*)))
(initialize-extended-method-check #'slot-definition-allocation)
;; Not in MOP.
(defun (setf slot-definition-allocation) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-allocation))
  (setf (sys::%record-ref slotdef *<slot-definition>-allocation-location*) new-value))

;; Not in MOP.
(defun slot-definition-inheritable-doc (slotdef)
  (accessor-typecheck slotdef 'slot-definition 'slot-definition-inheritable-doc)
  (sys::%record-ref slotdef *<slot-definition>-inheritable-doc-location*))
(defun (setf slot-definition-inheritable-doc) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-inheritable-doc))
  (setf (sys::%record-ref slotdef *<slot-definition>-inheritable-doc-location*) new-value))

;; Not in MOP.
(defun slot-definition-documentation (slotdef)
  (accessor-typecheck slotdef 'slot-definition 'slot-definition-documentation)
  (inheritable-slot-definition-documentation (sys::%record-ref slotdef *<slot-definition>-inheritable-doc-location*)))
(defun (setf slot-definition-documentation) (new-value slotdef)
  (accessor-typecheck slotdef 'slot-definition '(setf slot-definition-documentation))
  (setf (inheritable-slot-definition-documentation (sys::%record-ref slotdef *<slot-definition>-inheritable-doc-location*)) new-value))

;; MOP p. 85
(defgeneric slot-definition-readers (slotdef)
  (:method ((slotdef direct-slot-definition))
    (sys::%record-ref slotdef *<direct-slot-definition>-readers-location*)))
(initialize-extended-method-check #'slot-definition-readers)
;; Not in MOP.
(defun (setf slot-definition-readers) (new-value slotdef)
  (accessor-typecheck slotdef 'direct-slot-definition '(setf slot-definition-readers))
  (setf (sys::%record-ref slotdef *<direct-slot-definition>-readers-location*) new-value))

;; MOP p. 85
(defgeneric slot-definition-writers (slotdef)
  (:method ((slotdef direct-slot-definition))
    (sys::%record-ref slotdef *<direct-slot-definition>-writers-location*)))
(initialize-extended-method-check #'slot-definition-writers)
;; Not in MOP.
(defun (setf slot-definition-writers) (new-value slotdef)
  (accessor-typecheck slotdef 'direct-slot-definition '(setf slot-definition-writers))
  (setf (sys::%record-ref slotdef *<direct-slot-definition>-writers-location*) new-value))

;; MOP p. 86
(defgeneric slot-definition-location (slotdef)
  (:method ((slotdef effective-slot-definition))
    (sys::%record-ref slotdef *<effective-slot-definition>-location-location*)))
(initialize-extended-method-check #'slot-definition-location)
;; Not in MOP.
(defun (setf slot-definition-location) (new-value slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition '(setf slot-definition-location))
  (setf (sys::%record-ref slotdef *<effective-slot-definition>-location-location*) new-value))

;; Not in MOP.
(defun slot-definition-efm-svuc (slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition 'slot-definition-efm-svuc)
  (sys::%record-ref slotdef *<effective-slot-definition>-efm-svuc-location*))
(defun (setf slot-definition-efm-svuc) (new-value slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition '(setf slot-definition-efm-svuc))
  (setf (sys::%record-ref slotdef *<effective-slot-definition>-efm-svuc-location*) new-value))

;; Not in MOP.
(defun slot-definition-efm-ssvuc (slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition 'slot-definition-efm-ssvuc)
  (sys::%record-ref slotdef *<effective-slot-definition>-efm-ssvuc-location*))
(defun (setf slot-definition-efm-ssvuc) (new-value slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition '(setf slot-definition-efm-ssvuc))
  (setf (sys::%record-ref slotdef *<effective-slot-definition>-efm-ssvuc-location*) new-value))

;; Not in MOP.
(defun slot-definition-efm-sbuc (slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition 'slot-definition-efm-sbuc)
  (sys::%record-ref slotdef *<effective-slot-definition>-efm-sbuc-location*))
(defun (setf slot-definition-efm-sbuc) (new-value slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition '(setf slot-definition-efm-sbuc))
  (setf (sys::%record-ref slotdef *<effective-slot-definition>-efm-sbuc-location*) new-value))

;; Not in MOP.
(defun slot-definition-efm-smuc (slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition 'slot-definition-efm-smuc)
  (sys::%record-ref slotdef *<effective-slot-definition>-efm-smuc-location*))
(defun (setf slot-definition-efm-smuc) (new-value slotdef)
  (accessor-typecheck slotdef 'effective-slot-definition '(setf slot-definition-efm-smuc))
  (setf (sys::%record-ref slotdef *<effective-slot-definition>-efm-smuc-location*) new-value))


;; MOP p. 45
(defgeneric direct-slot-definition-class (class &rest initargs)
  (:method ((class semi-standard-class) &rest initargs)
    (declare (ignore initargs))
    <standard-direct-slot-definition>)
  (:method ((class structure-class) &rest initargs)
    (declare (ignore initargs))
    <structure-direct-slot-definition>))

;; MOP p. 45
(defgeneric effective-slot-definition-class (class &rest initargs)
  (:method ((class semi-standard-class) &rest initargs)
    (declare (ignore initargs))
    <standard-effective-slot-definition>)
  (:method ((class structure-class) &rest initargs)
    (declare (ignore initargs))
    <structure-effective-slot-definition>))

;; Customizable function used to compare two slots of given objects belonging
;; to the same class.
;; Arguments: class is a subclass of <direct-slot-definition>,
;;            (class-of object1) = class,
;;            (class-of object2) = class,
;;            slot is a slot of class,
;;            value1 = (slot-value object1 (slot-definition-name slot)),
;;            value2 = (slot-value object2 (slot-definition-name slot)).
(defgeneric slot-equal-using-class (class object1 object2 slot value1 value2)
  (:method (class (object1 standard-direct-slot-definition) (object2 standard-direct-slot-definition) slot value1 value2)
    (declare (ignore class object1 object2 slot))
    (equal value1 value2)))

;; Test two direct slots for equality, except for the inheritable slots,
;; where only the presence is compared.
(defun equal-direct-slot (slot1 slot2 &aux slot-class)
  (and (eq (setq slot-class (class-of slot1)) (class-of slot2))
       (eq (slot-definition-name slot1) (slot-definition-name slot2))
       (eq (null (slot-definition-initfunction slot1)) (null (slot-definition-initfunction slot2)))
       (eq (null (slot-definition-documentation slot1)) (null (slot-definition-documentation slot2)))
       ;; The MOP doesn't specify an equality method that the user could define,
       ;; therefore we use the generic "compare all slots" approach.
       (dolist (s (class-slots slot-class) t)
         (let ((n (slot-definition-name s)))
           ;; $inheritable-initer covers the :initform :initfunction slot options.
           ;; $inheritable-doc covers the :documentation slot option.
           (unless (memq n '($inheritable-initer $inheritable-doc))
             (let ((unboundp1 (not (slot-boundp slot1 n)))
                   (unboundp2 (not (slot-boundp slot2 n))))
               (unless (and (eq unboundp1 unboundp2)
                            (or unboundp1
                                (slot-equal-using-class slot-class slot1 slot2 s
                                                        (slot-value slot1 n)
                                                        (slot-value slot2 n))))
                 (return nil))))))))

#|
;; Tell the compiler how to serialize <structure-effective-slot-definition>
;; instances. This is needed for DEFSTRUCT.
(defmethod make-load-form ((object structure-effective-slot-definition) &optional environment)
  (declare (ignore environment))
  (make-load-form-<structure-effective-slot-definition> object))
|#
