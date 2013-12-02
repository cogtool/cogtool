;; http://sourceforge.net/tracker/index.php?func=detail&aid=1359066&group_id=1355&atid=101355
;; user-defined :allocation :hash

(defpackage #:hash-classes
  (:use #:common-lisp
        #+allegro #:clos
        #+clisp #:clos
        #+cmu #:clos-mop
        #+lispworks #:clos
        #+(and mcl (not openmcl)) #:mcl-mop
        #+openmcl #:openmcl-mop
        #+sbcl #:sb-mop)
  (:export #:hash-class))

(in-package #:hash-classes)

(defclass hash-class (standard-class)
  ())

(defclass hash-object (standard-object)
  ((hash-slots :initform (make-hash-table :test #'eq))))

(defmethod validate-superclass ((class hash-class) (superclass standard-class))
  t)

(defmethod initialize-instance :around
    ((class hash-class) &rest initargs &key direct-superclasses)
  (declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses
        thereis (subtypep class (find-class 'hash-object)))
      (call-next-method)
      (apply #'call-next-method
             class
             :direct-superclasses
             (append direct-superclasses
                     (list (find-class 'hash-object)))
             initargs)))

(defmethod reinitialize-instance :around
    ((class hash-class) &rest initargs
     &key (direct-superclasses '() direct-superclasses-p))
  (declare (dynamic-extent initargs))
  (if (or (not direct-superclasses-p)
          (loop for class in direct-superclasses
            thereis (subtypep class (find-class 'hash-object))))
      (call-next-method)
      (apply #'call-next-method
             class
             :direct-superclasses
             (append direct-superclasses
                     (list (find-class 'hash-object)))
             initargs)))

(defclass hash-direct-slot-definition (standard-direct-slot-definition)
  ())

(defmethod direct-slot-definition-class ((class hash-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'hash-direct-slot-definition))

(defclass hash-effective-slot-definition (standard-effective-slot-definition)
  ())

(defvar *effective-slot-definition-class*)

(defmethod compute-effective-slot-definition
    ((class hash-class) (name t) direct-slot-definitions)
  (let ((*effective-slot-definition-class*
         (if (eq (slot-definition-allocation (first direct-slot-definitions))
                 :hash)
             (find-class 'hash-effective-slot-definition)
             (find-class 'standard-effective-slot-definition))))
    (call-next-method)))

(defmethod effective-slot-definition-class ((class hash-class) &rest initargs)
  (declare (ignore initargs))
  *effective-slot-definition-class*)

(defmethod shared-initialize :before
    ((object hash-object) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (unless (slot-boundp object 'hash-slots)
    (setf (slot-value object 'hash-slots)
          (make-hash-table :test #'eq))))

(defmethod slot-value-using-class
    ((class hash-class) object (slot hash-effective-slot-definition))
  (multiple-value-bind (value present-p)
      (gethash (slot-definition-name slot)
               (slot-value object 'hash-slots))
    (if present-p value
        (slot-unbound class object (slot-definition-name slot)))))

(defmethod (setf slot-value-using-class)
    (value (class hash-class) object (slot hash-effective-slot-definition))
  (setf (gethash (slot-definition-name slot)
                 (slot-value object 'hash-slots))
        value))

(defmethod slot-boundp-using-class
    ((class hash-class) object (slot hash-effective-slot-definition))
  (nth-value 1 (gethash (slot-definition-name slot)
                        (slot-value object 'hash-slots))))

(defmethod slot-makunbound-using-class
    ((class hash-class) object (slot hash-effective-slot-definition))
  (remhash (slot-definition-name slot)
           (slot-value object 'hash-slots)))
