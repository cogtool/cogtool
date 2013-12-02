;;;; Common Lisp Object System for CLISP: Slots
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")

;;; ===========================================================================

(defgeneric slot-missing (class instance slot-name operation
                          &optional new-value)
  (:method ((class t) instance slot-name operation &optional new-value)
    (declare (ignore instance new-value))
    (error-of-type 'error
      (TEXT "~S: The class ~S has no slot named ~S")
      operation class slot-name)))

(defgeneric slot-unbound (class instance slot-name)
  (:method ((class t) instance slot-name)
    (declare (ignore class))
    (multiple-value-bind (new-value store-p)
        (sys::check-value `(slot-value ,instance ',slot-name)
          (make-condition 'sys::simple-unbound-slot
            :name slot-name
            :instance instance
            :format-control (TEXT "~S: The slot ~S of ~S has no value")
            :format-arguments (list 'slot-value slot-name instance)))
      (when store-p
        (setf (slot-value instance slot-name) new-value))
      new-value)))

;;; ===========================================================================

;; Optimization of SLOT-VALUE and its brothers.
;; They are optimized to work as follows:
;; Step 1: (class-of instance) -> a class object. Take its slot-location-table.
;; Step 2: Lookup the slot name in the table. The result is an object that
;; encodes the behaviour of
;;   (slot-value-using-class this-class any-direct-instance slot)
;;   ((setf slot-value-using-class) any-new-value this-class any-direct-instance slot)
;;   (slot-boundp-using-class this-class any-direct-instance slot)
;;   (slot-makunbound-using-class this-class any-direct-instance slot)
;; for the slot with the given name. In the particular case that these generic
;; functions' effective methods are the predefined ones, the object is just the
;; slot location (a cons or nonnegative fixnum).

(defun invalidate-slot-value-info (class-specializer instance-specializer slot-specializer)
  ;; Step 1: Determine all affected classes that satisfy the instance-specializer.
  (let ((affected-classes
          (if (defined-class-p instance-specializer)
            (if (= (class-initialized instance-specializer) 6) ; finalized?
              (list-all-finalized-subclasses instance-specializer)
              '())
            (list (class-of (eql-specializer-object instance-specializer))))))
    ;; Step 2: Filter out those that don't satisfy the class-specializer.
    (setq affected-classes
          (remove-if-not #'(lambda (c) (typep c class-specializer))
                         affected-classes))
    ;; Step 3: Iterate over the affected classes and recompute the relevant
    ;; entries in the slot-location-table.
    (dolist (class affected-classes)
      (let ((ht (class-slot-location-table class)))
        (dolist (slot (class-slots class))
          (when (typep slot slot-specializer)
            (setf (gethash (slot-definition-name slot) ht)
                  (compute-slot-location-table-entry class slot))))))))

(defun note-svuc-change (method)
  (apply #'invalidate-slot-value-info (method-specializers method)))

(defun note-ssvuc-change (method)
  (apply #'invalidate-slot-value-info (cdr (method-specializers method))))

(defun note-sbuc-change (method)
  (apply #'invalidate-slot-value-info (method-specializers method)))

(defun note-smuc-change (method)
  (apply #'invalidate-slot-value-info (method-specializers method)))

;; MOP p. 97
(defgeneric slot-value-using-class (class object slot))
(setq |#'slot-value-using-class| #'slot-value-using-class)
#|
(defmethod slot-value-using-class ((class semi-standard-class) instance (slot standard-effective-slot-definition))
  (let ((slot-name (slot-definition-name slot))
        (slot-location (slot-definition-location slot)))
    ((lambda (value)
       (if (eq value unbound)
         (slot-unbound class instance slot-name)
         value))
     (cond ((null slot-location)
            (slot-missing class instance slot-name 'slot-value))
           ((atom slot-location) ; access local slot
            (sys::%record-ref instance slot-location))
           (t ; access shared slot
            (svref (cv-shared-slots (car slot-location))
                   (cdr slot-location)))))))
|#
;; The main work is done by a SUBR:
(do-defmethod 'slot-value-using-class
  (make-instance-<standard-method> <standard-method>
    :specializers (list (find-class 'semi-standard-class)
                        (find-class 't)
                        (find-class 'standard-effective-slot-definition))
    'fast-function #'clos::%slot-value-using-class
    'wants-next-method-p nil
    :qualifiers '()
    :lambda-list '(class instance slot)
    'signature #s(compiler::signature :req-num 3)))

;; MOP p. 93
(defgeneric (setf slot-value-using-class) (new-value class object slot))
(setq |#'(setf slot-value-using-class)| #'(setf slot-value-using-class))
#|
(defmethod (setf slot-value-using-class) (new-value (class semi-standard-class) instance (slot standard-effective-slot-definition))
  (let ((slot-name (slot-definition-name slot))
        (slot-location (slot-definition-location slot)))
    (cond ((null slot-location)
           (slot-missing class instance slot-name 'setf new-value))
          ((atom slot-location) ; access local slot
           (sys::%record-store instance slot-location new-value))
          (t ; access shared slot
           (setf (svref (cv-shared-slots (car slot-location))
                        (cdr slot-location))
                 new-value)))))
|#
;; The main work is done by a SUBR:
(do-defmethod '(setf slot-value-using-class)
  (make-instance-<standard-method> <standard-method>
    :specializers (list (find-class 't)
                        (find-class 'semi-standard-class)
                        (find-class 't)
                        (find-class 'standard-effective-slot-definition))
    'fast-function #'clos::%set-slot-value-using-class
    'wants-next-method-p nil
    :qualifiers '()
    :lambda-list '(new-value class instance slot)
    'signature #s(compiler::signature :req-num 4)))

;; MOP p. 94
(defgeneric slot-boundp-using-class (class object slot))
(setq |#'slot-boundp-using-class| #'slot-boundp-using-class)
#|
(defmethod slot-boundp-using-class ((class semi-standard-class) instance (slot standard-effective-slot-definition))
  (let ((slot-name (slot-definition-name slot))
        (slot-location (slot-definition-location slot)))
    (cond ((null slot-location)
           (slot-missing class instance slot-name 'slot-boundp))
          ((atom slot-location) ; access local slot
           (not (eq (sys::%record-ref instance slot-location) unbound)))
          (t ; access shared slot
           (not (eq (svref (cv-shared-slots (car slot-location))
                           (cdr slot-location))
                    unbound))))))
|#
;; The main work is done by a SUBR:
(do-defmethod 'slot-boundp-using-class
  (make-instance-<standard-method> <standard-method>
    :specializers (list (find-class 'semi-standard-class)
                        (find-class 't)
                        (find-class 'standard-effective-slot-definition))
    'fast-function #'clos::%slot-boundp-using-class
    'wants-next-method-p nil
    :qualifiers '()
    :lambda-list '(class instance slot)
    'signature #s(compiler::signature :req-num 3)))

;; MOP p. 96
(defgeneric slot-makunbound-using-class (class object slot))
(setq |#'slot-makunbound-using-class| #'slot-makunbound-using-class)
#|
(defmethod slot-makunbound-using-class ((class semi-standard-class) instance (slot standard-effective-slot-definition))
  (let ((slot-name (slot-definition-name slot))
        (slot-location (slot-definition-location slot)))
    (cond ((null slot-location)
           (slot-missing class instance slot-name 'slot-makunbound))
          ((atom slot-location) ; access local slot
           (sys::%record-store instance slot-location unbound))
          (t ; access shared slot
           (setf (svref (cv-shared-slots (car slot-location))
                        (cdr slot-location))
                 unbound)))))
|#
;; The main work is done by a SUBR:
(do-defmethod 'slot-makunbound-using-class
  (make-instance-<standard-method> <standard-method>
    :specializers (list (find-class 'semi-standard-class)
                        (find-class 't)
                        (find-class 'standard-effective-slot-definition))
    'fast-function #'clos::%slot-makunbound-using-class
    'wants-next-method-p nil
    :qualifiers '()
    :lambda-list '(class instance slot)
    'signature #s(compiler::signature :req-num 3)))
