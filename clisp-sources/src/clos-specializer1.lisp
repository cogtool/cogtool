;;;; Common Lisp Object System for CLISP
;;;; Specializers
;;;; Part 1: Class definitions, preliminary accessors, utility functions.
;;;; Bruno Haible 2004-05-15

(in-package "CLOS")

;;; ===========================================================================

;;; The abstract class <specializer> allows specializers for methods in
;;; generic functions (i.e. classes and EQL-specializers) to be treated in a
;;; homogenous way.

(defvar *<specializer>-defclass*
  '(defclass specializer (standard-stablehash metaobject)
     (($direct-methods          ; weak-list or weak-hash-table of methods that
                                ; use this specializer
       :initform nil))
    (:fixed-slot-locations t)))

;; Fixed slot locations.
(defconstant *<specializer>-direct-methods-location* 2)

;; Preliminary accessors.
(predefun specializer-direct-methods-table (object)
  (sys::%record-ref object *<specializer>-direct-methods-location*))
(predefun (setf specializer-direct-methods-table) (new-value object)
  (setf (sys::%record-ref object *<specializer>-direct-methods-location*) new-value))

;; Initialization of a <specializer> instance.
(defun shared-initialize-<specializer> (specializer situation &rest args
                                        &key &allow-other-keys)
  (apply #'shared-initialize-<standard-stablehash> specializer situation args)
  (unless *classes-finished*
    ; Bootstrapping: Simulate the effect of #'%shared-initialize.
    (when (eq situation 't) ; called from initialize-instance?
      (setf (specializer-direct-methods-table specializer) nil)))
  specializer)

;;; ===========================================================================

;;; The class <eql-specializer> represents an EQL-specializer.

(defvar <eql-specializer> 'eql-specializer)
(defvar *<eql-specializer>-defclass*
  '(defclass eql-specializer (specializer)
     (($singleton :initarg singleton))
     (:fixed-slot-locations t)))
(defvar *<eql-specializer>-class-version* (make-class-version))

;; Fixed slot locations.
(defconstant *<eql-specializer>-singleton-location* 3)

;; Preliminary accessors.
(predefun eql-specializer-singleton (object)
  (sys::%record-ref object *<eql-specializer>-singleton-location*))
(predefun (setf eql-specializer-singleton) (new-value object)
  (setf (sys::%record-ref object *<eql-specializer>-singleton-location*) new-value))

(defconstant *<eql-specializer>-instance-size* 4)

;; Initialization of an <eql-specializer> instance.
(defun shared-initialize-<eql-specializer> (specializer situation &rest args
                                            &key ((singleton singleton) nil singleton-p)
                                            &allow-other-keys)
  (apply #'shared-initialize-<specializer> specializer situation args)
  (unless *classes-finished*
    ; Bootstrapping: Simulate the effect of #'%shared-initialize.
    (when singleton-p
      (setf (eql-specializer-singleton specializer) singleton)))
  specializer)

(defun initialize-instance-<eql-specializer> (specializer &rest args
                                              &key &allow-other-keys)
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'initialize-instance later.
  (apply #'shared-initialize-<eql-specializer> specializer 't args))

(defun make-instance-<eql-specializer> (class &rest args
                                        &key &allow-other-keys)
  ;; class = <eql-specializer>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore class))
  (let ((specializer (allocate-metaobject-instance *<eql-specializer>-class-version* *<eql-specializer>-instance-size*)))
    (apply #'initialize-instance-<eql-specializer> specializer args)))

;; Type test.
(defun eql-specializer-p (object)
  (and (std-instance-p object)
       (let ((cv (sys::%record-ref object 0)))
         ; Treat the most frequent case first, for bootstrapping.
         (or (eq cv *<eql-specializer>-class-version*)
             (gethash <eql-specializer>
                      (class-all-superclasses (class-of object)))))))

;;; ===========================================================================

;; We don't store the list of generic functions that use a given specializer
;; in the specializer, but instead compute it on the fly, because
;; 1. For good asymptotic performance the generic-functions list would have to
;;    be stored as a weak set or a weak multiset, thus requiring that
;;    <generic-function> inherits from <standard-stable-hash> - but this gives
;;    a collision with <funcallable-instance>.
;; 2. The generic-functions list of a specializer is generally not much
;;    shorter than the methods list of the specializer, and is redundant.

(defun compute-direct-generic-functions (specializer)
  (let* ((methods (specializer-direct-methods specializer))
         (gfs (delete-duplicates (mapcar #'method-generic-function methods) :test #'eq)))
    (when (memq nil gfs)
      (error (TEXT "~S: Some methods have been removed from their generic function, but the list in the ~S specializer was not updated.")
             'specializer-direct-generic-functions specializer))
    gfs))

;; MOP p. 103
(predefun specializer-direct-generic-functions (specializer)
  (compute-direct-generic-functions specializer))

#|
;; Adds a method to the list of direct methods.
(defun add-direct-method (specializer method) ...)
;; Removes a method from the list of direct methods.
(defun remove-direct-method (specializer method) ...)
;; Returns the currently existing direct methods, as a freshly consed list.
(defun list-direct-methods (specializer) ...)
|#
(def-weak-set-accessors specializer-direct-methods-table method
  add-direct-method-internal
  remove-direct-method-internal
  list-direct-methods)

(defun add-direct-method-<specializer>-<method> (specializer method)
  (add-direct-method-internal specializer method)
  (when (eql-specializer-p specializer)
    (let ((its-class (class-of (eql-specializer-singleton specializer))))
      (when (semi-standard-class-p its-class)
        (add-direct-instance-specializer its-class specializer)))))

;; Preliminary.
(predefun add-direct-method (specializer method)
  (add-direct-method-<specializer>-<method> specializer method))

(predefun remove-direct-method (specializer method)
  (remove-direct-method-internal specializer method))

;; MOP p. 103
(predefun specializer-direct-methods (specializer)
  (list-direct-methods specializer))

;;; ===========================================================================

;; EQL-specializers for numbers.
(defvar *eql-specializer-table*
  (make-hash-table :key-type 'number :value-type 'eql-specializer
                   :test 'ext:fasthash-eql :warn-if-needs-rehash-after-gc t))

;; EQL-specializers for other kinds of objects.
(defvar *eq-specializer-table*
  (make-hash-table :key-type '(not number) :value-type 'eql-specializer
                   :test 'ext:stablehash-eq
                   :weak :key))

;; MOP p. 70
(defun intern-eql-specializer (object)
  (let ((table (if (numberp object) *eql-specializer-table* *eq-specializer-table*)))
    (or (gethash object table)
        (setf (gethash object table)
              (make-instance-<eql-specializer> <eql-specializer>
                                               'singleton object)))))

;; Returns the eql-specializer for the given object only if it already exists,
;; otherwise nil.
(defun existing-eql-specializer (object)
  (let ((table (if (numberp object) *eql-specializer-table* *eq-specializer-table*)))
    (gethash object table)))

;; MOP p. 52
(defun eql-specializer-object (specializer)
  (eql-specializer-singleton specializer))

(defun print-object-<eql-specializer> (specializer stream)
  (print-unreadable-object (specializer stream :type t)
    (write (eql-specializer-object specializer) :stream stream)))

;;; ===========================================================================

;; Converts a specializer to a pretty printing type.
(defun specializer-pretty (specializer)
  (if (eql-specializer-p specializer)
    `(EQL ,(eql-specializer-object specializer))
    specializer))
