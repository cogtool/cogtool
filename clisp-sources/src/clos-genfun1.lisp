;;;; Common Lisp Object System for CLISP: Generic Functions
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; ============================================================================

(defparameter <funcallable-standard-class>
  (defclass funcallable-standard-class (semi-standard-class)
    ()
    (:fixed-slot-locations t)))
(defparameter *<funcallable-standard-class>-class-version*
  (class-current-version <funcallable-standard-class>))

(defconstant *<funcallable-standard-class>-instance-size* 28)

;; For DEFCLASS macro expansions.
(defconstant *<funcallable-standard-class>-valid-initialization-keywords* ; ABI
             *<standard-class>-valid-initialization-keywords*)

(defun make-instance-<funcallable-standard-class> (metaclass &rest args
                                                   &key name
                                                        (direct-superclasses '())
                                                        (direct-slots '())
                                                        (direct-default-initargs '())
                                                   &allow-other-keys)
  ;; metaclass = <funcallable-standard-class>
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'make-instance later.
  (declare (ignore metaclass name direct-superclasses direct-slots
                   direct-default-initargs))
  (let ((class (allocate-metaobject-instance *<funcallable-standard-class>-class-version*
                                             *<funcallable-standard-class>-instance-size*)))
    (apply #'initialize-instance-<funcallable-standard-class> class args)))

(defun initialize-instance-<funcallable-standard-class> (class &rest args
                                                         &key &allow-other-keys)
  ;; Don't add functionality here! This is a preliminary definition that is
  ;; replaced with #'initialize-instance later.
  (apply #'shared-initialize-<funcallable-standard-class> class 't args)
  (install-class-direct-accessors class)
  class)

(defun shared-initialize-<funcallable-standard-class> (class situation &rest args
                                                       &key (direct-superclasses '() direct-superclasses-p)
                                                            ((:direct-slots direct-slots-as-lists) '() direct-slots-as-lists-p)
                                                            ((direct-slots direct-slots-as-metaobjects) '() direct-slots-as-metaobjects-p)
                                                            (direct-default-initargs '() direct-default-initargs-p)
                                                            (documentation nil documentation-p)
                                                            (generic-accessors t generic-accessors-p)
                                                            (fixed-slot-locations nil fixed-slot-locations-p)
                                                       &allow-other-keys)
  (declare (ignore direct-superclasses direct-superclasses-p
                   direct-slots-as-lists direct-slots-as-lists-p
                   direct-slots-as-metaobjects direct-slots-as-metaobjects-p
                   direct-default-initargs direct-default-initargs-p
                   documentation documentation-p generic-accessors
                   generic-accessors-p fixed-slot-locations
                   fixed-slot-locations-p))
  (apply #'shared-initialize-<semi-standard-class> class situation args)
  class)

;; ----------------------------------------------------------------------------

;; Low-level representation of funcallable instances:
;; Funcallable instances are Closures with a certain bit set in the
;; closure_flags. They always the following shape:
;; - recdata[0] = clos_name_or_class_version is a semi-class-version,
;;   like for instances,
;; - recdata[1] = clos_codevec is a simple-8bit-vector, like for compiled
;;   functions,
;; - recdata[2] = clos_venv is reserved,
;; - recdata[3] is the first slot, the name,
;; - then come additional slots, as described by the class.

(defparameter <funcallable-standard-object>
  (ext:compiler-let ((*allow-mixing-metaclasses* t))
    (let ((*allow-mixing-metaclasses* t))
      (defclass funcallable-standard-object (function standard-object)
        ;; The MOP p. 7 specifies a superclass list (standard-object function),
        ;; but then generic-function and standard-generic-function would have a
        ;; class-precedence-list that contains standard-object before function,
        ;; which contradicts the last sentence of ANSI CL 4.2.2. Possible
        ;; workarounds are: 1. reversed order (function standard-object),
        ;; 2. use a twin superclass or subclass of standard-object instead of
        ;; standard-object itself, 3. override compute-class-precedence-list
        ;; for this class. We choose solution 1 because it is the one a user
        ;; will most easily understand.
        (($name ; The function name is present as first CLOS slot. The macro
                ; Closure_name in lispbibl.d refers to it. Therefore this slot
                ; must not be changed after initialization, since this could
                ; interfere with the forwarded-instance mechanism.
           :accessor funcallable-name))
        (:metaclass funcallable-standard-class)
        (:fixed-slot-locations t)
        (:generic-accessors nil)))))

(defun print-object-<funcallable-standard-object> (object stream)
  (print-unreadable-object (object stream :type t)
    (write (funcallable-name object) :stream stream)))

;; Preliminary.
;; Now we can at least print classes and generic-functions.
(defun print-object (object stream)
  (cond ((potential-class-p object) (format stream "#<CLASS ~S>" (class-classname object)))
        ((funcallable-instance-p object) (print-object-<funcallable-standard-object> object stream))
        (t (write-string "#<UNKNOWN>" stream))))

;; ============================================================================

;; low-level-representation:
;; Compiled functions (Cclosures), for which a certain bit is set in
;; the flag-byte of the code-vector.

;; The compiler uses (at GENERIC-FLET, GENERIC-LABELS) and the evaluator
;; presupposes likewise, that a generic function does not change its
;; calling convention.
;; A generic function with signature (reqnum optnum restp keywords allowp)
;; is from the very beginning (!) a compiled function with
;;         reqnum  required parameters
;;         0       optional parameters
;;         &rest if and only if (or (> optnum 0) restp),
;;         without &key.
(defun callinfo (reqnum optnum restp keywords allowp)
  (declare (ignore keywords allowp))
  (list reqnum 0 (or (> optnum 0) restp) nil nil nil))

;; ----------------------------------------------------------------------------

(defparameter <generic-function>
  (defclass generic-function (metaobject funcallable-standard-object)
    (($listeners          ; list of objects to be notified upon a change
       :type list
       :accessor gf-listeners))
    (:metaclass funcallable-standard-class)
    (:fixed-slot-locations t)
    (:generic-accessors nil)))

;; Initialization of a <generic-function> instance.
(defun shared-initialize-<generic-function> (gf situation &rest args
                                             &key (name nil name-p)
                                             &allow-other-keys)
  (when *classes-finished*
    (apply #'%shared-initialize gf situation args)) ; == (call-next-method)
  (when (or (eq situation 't) name-p)
    (setf (funcallable-name gf) name))
  (when (eq situation 't)
    (setf (gf-listeners gf) nil))
  gf)

;; ----------------------------------------------------------------------------

(defparameter <standard-generic-function>
  (defclass standard-generic-function (generic-function)
    (($signature           ; a signature struct
       :type (simple-vector 6)
       :accessor std-gf-signature)
     ($argorder            ; the argument-precedence-order, as a list of
                           ; numbers from 0 to reqnum-1,
       :type list
       :accessor std-gf-argorder)
     ($methods             ; the list of all methods
       :type list
       :accessor std-gf-methods)
     ($method-combination  ; a method-combination object
       :type method-combination
       :accessor std-gf-method-combination)
     ($default-method-class ; default class for newly added methods
       :type class
       :accessor std-gf-default-method-class)
     ($lambda-list         ; a redundant non-canonical encoding of the
                           ; signature
       :type list
       :accessor std-gf-lambda-list)
     ($documentation
       :type (or null string)
       :accessor std-gf-documentation)
     ($declspecs           ; a list of declaration-specifiers
       :type list
       :accessor std-gf-declspecs)
     ($effective-method-cache ; an alist mapping a list of methods to the
                           ; effective method as function
       :type list
       :accessor std-gf-effective-method-cache)
     ($initialized         ; true if an instance has already been created
       :type boolean
       :accessor std-gf-initialized))
    (:metaclass funcallable-standard-class)
    (:fixed-slot-locations t)
    (:generic-accessors nil)))

(defun std-gf-undeterminedp (gf)
  (eq (std-gf-signature gf) (sys::%unbound)))

;; Preliminary.
;; During bootstrapping, only <standard-generic-function> instances are used.
(defun generic-function-methods (gf)
  (std-gf-methods gf))
(defun generic-function-method-class (gf)
  (std-gf-default-method-class gf))
(defun generic-function-signature (gf)
  (std-gf-signature gf))
(defun generic-function-undeterminedp (gf)
  (std-gf-undeterminedp gf))
(defun generic-function-method-combination (gf)
  (std-gf-method-combination gf))
(defun generic-function-argorder (gf)
  (std-gf-argorder gf))
(defun generic-function-declarations (gf)
  (std-gf-declspecs gf))

;; ============================================================================
