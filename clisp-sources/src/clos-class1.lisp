;;;; Common Lisp Object System for CLISP
;;;; Class metaobjects
;;;; Part 1: Class definitions, preliminary accessors.
;;;; Bruno Haible 2004-05-25
;;;; Sam Steingold 2005

(in-package "CLOS")


;;; Low-level representation:

;; In the runtime-system, the type "CLOS instance" exists.
;; The first component is the class-version, the rest are the local slot
;; values.

;; Classes are instances of type CLASS,

;; The "value" of a slot that is unbound, is #<UNBOUND> - what else?

;;; see RECORD.D :
;; (STD-INSTANCE-P obj) tests, if an object is a CLOS-instance.
;; (ALLOCATE-STD-INSTANCE class n) returns a non-funcallable CLOS-instance
;; with Class class and n-1 additional slots.
;; (ALLOCATE-FUNCALLABLE-INSTANCE class n) returns a funcallable CLOS-instance
;; with Class class and n-3 additional slots.

;;; see IO.D :
;; CLOS-instances are printed via (PRINT-OBJECT object stream).

;; (CLASS-OF object) see PREDTYPE.D, uses property CLOSCLASS.

;;; ===========================================================================

;;; Auxiliary stuff.

;; An empty hash table.
(defconstant empty-ht
             (make-hash-table :key-type 'symbol :value-type 't
                              :test 'eq :warn-if-needs-rehash-after-gc t
                              :size 0))

;;; ===========================================================================

;;; The abstract class <super-class> allows defined classes and
;;; forward-references to classes to be treated in a homogenous way.

(defvar *<super-class>-defclass*
  '(defclass super-class (standard-stablehash metaobject)
     (($classname          ; (class-name class) = (class-classname class),
                           ; a symbol
        :type symbol
        :initarg :name)
      ($direct-subclasses  ; set of all direct subclasses, as a weak-list or
                           ; weak-hash-table or NIL
        :type (or hash-table weak-list null)
        :initform nil))
     (:fixed-slot-locations nil)))

;;; ===========================================================================

;;; The abstract class <potential-class> is the abstract base class of all
;;; classes.

(defvar *<potential-class>-defclass*
  '(defclass potential-class (specializer super-class)
     ()
     (:fixed-slot-locations t)))

;; Fixed slot locations.
(defconstant *<potential-class>-classname-location* 3)
(defconstant *<potential-class>-direct-subclasses-location* 4)

;; Preliminary accessors.
(predefun class-classname (object)
  (sys::%record-ref object *<potential-class>-classname-location*))
(predefun (setf class-classname) (new-value object)
  (setf (sys::%record-ref object *<potential-class>-classname-location*) new-value))
(predefun class-direct-subclasses-table (object)
  (if (potential-class-p object)
    (sys::%record-ref object *<potential-class>-direct-subclasses-location*)
    (slot-value object '$direct-subclasses)))
(predefun (setf class-direct-subclasses-table) (new-value object)
  (if (potential-class-p object)
    (setf (sys::%record-ref object *<potential-class>-direct-subclasses-location*) new-value)
    (setf (slot-value object '$direct-subclasses) new-value)))

;; Initialization of a <potential-class> instance.
(defun shared-initialize-<potential-class> (class situation &rest args
                                            &key (name nil name-p)
                                            &allow-other-keys)
  (apply #'shared-initialize-<specializer> class situation args)
  (unless *classes-finished*
    ; Bootstrapping: Simulate the effect of #'%shared-initialize.
    (when (eq situation 't) ; called from initialize-instance?
      (setf (class-direct-subclasses-table class) nil)))
  (when (or (eq situation 't) name-p)
    ; No need to check the name: any name is valid.
    (setf (class-classname class) name))
  class)

;;; ===========================================================================

;;; The class <forward-referenced-class> allows forward-references to classes
;;; to collect their direct subclasses already before they are defined:
;;;     (defclass b (a) ())
;;;     (defclass a () ())
;;;     (class-direct-subclasses (find-class 'a)) => (#<STANDARD-CLASS B>)

;;; A forward-referenced-class's name is always a symbol that cannot be
;;; changed, and the forward-referenced-class is available as
;;; (get name 'CLOSCLASS), until it is replaced with the defined class.

;;; The MOP specification regarding <forward-referenced-class> is severely
;;; misdesigned. The actual meaning of a <forward-referenced-class> is a
;;; forward-reference to (= placeholder for) a not yet defined class. The only
;;; place where it is used is in the direct-superclasses list of some classes
;;; that are not yet finalized.
;;;
;;; Putting it under <class> is a mistake because:
;;;   1. Classes fundamentally describe the slots and operations available
;;;      on its (direct and indirect) instances. But a forward-referenced
;;;      class can never have (direct and indirect) instances, since the
;;;      slots and operations are not yet known.
;;;   2. All the generic functions on <class>, such as class-precedence-list
;;;      or class-direct-default-initargs, make no sense on a
;;;      <forward-referenced-class> - since the real information is not yet
;;;      available.
;;;   3. <class> inherits from <specializer>, but it makes no sense to use
;;;      a <forward-referenced-class> as a specializer in a method or as a
;;;      type in TYPEP or SUBTYPEP.
;;;
;;; This is also backed by the fact that this MOP implementation has three
;;; times more tests for <defined-class> (i.e. for <class> without
;;; <forward-referenced-class>) than for <potential-class>.
;;;
;;; A better design would be to define an abstract class <superclass> and
;;; let <forward-referenced-class> inherit from it:
;;;   (defclass super-class () ...)
;;;   (defclass class (super-class specializer) ...)
;;;   (defclass forward-referenced-class (super-class) ...)
;;; and (class-direct-superclasses class) would simply be a list of
;;; <super-class> instances.

;; The proper <forward-referenced-class> inherits from <super-class> but
;; not from <specializer>.
(defvar *<forward-reference-to-class>-defclass*
  '(defclass forward-reference-to-class (super-class)
     ()
     (:fixed-slot-locations nil)))

;; The crappy <forward-referenced-class> from the MOP is subclass of
;; <potential-class> and thus also of <specializer>.
(defvar *<misdesigned-forward-referenced-class>-defclass*
  '(defclass misdesigned-forward-referenced-class (forward-reference-to-class potential-class)
     ()
     (:fixed-slot-locations nil)))

;;; ===========================================================================

;;; The abstract class <defined-class> allows built-in objects, user-defined
;;; objects and proxies to external worlds to be treated in a homogenous way.

(defvar *<defined-class>-defclass*
  '(defclass defined-class (potential-class)
     (($direct-superclasses ; list of all direct superclasses (or their names,
                           ; while the class is waiting to be finalized)
        :type list
        :initarg :direct-superclasses)
      ($all-superclasses   ; hash table of all superclasses (including
                           ; the class itself)
        :type hash-table)
      ($precedence-list    ; ordered list of all superclasses (with the class
                           ; itself first), or NIL while the class is waiting
                           ; to be finalized
        :type list)
      ($direct-slots       ; list of all freshly added slots (as
                           ; direct-slot-definition instances)
        :type list
        :initarg :direct-slots)
      ($slots              ; list of all slots (as effective-slot-definition
                           ; instances)
        :type list)
      ($slot-location-table ; hash table slotname -> descriptor
                           ; where the descriptor is either
                           ; - the location of the slot (a fixnum or cons), or
                           ; - its effective slot definition
        :type hash-table
        :initform empty-ht)
      ($direct-default-initargs ; freshly added default-initargs
                           ; (as alist initarg -> (form function))
        :type list
        :initarg :direct-default-initargs)
      ($default-initargs   ; default-initargs
                           ; (as alist initarg -> (form function))
        )
      ($documentation      ; string or NIL
        :type (or string null)
        :initarg :documentation)
      ($listeners          ; list of objects to be notified upon a change
        :type list
        :initform nil)
      ($initialized        ; describes which parts of the class are initialized
        :type (integer 0 6) ; 0 = nothing
                            ; 1 = name
                            ; 2 = likewise, plus direct-... info
                            ; 3 = likewise, plus class-precedence-list
                            ; 4 = likewise, plus class-all-superclasses
                            ; 5 = likewise, plus class-slots
                            ; 6 = likewise, plus slot-location-table, default-initargs
        :initform 0))
     (:fixed-slot-locations t)))

;; Fixed slot locations.
(defconstant *<defined-class>-direct-superclasses-location* 5)
(defconstant *<defined-class>-all-superclasses-location* 6)
(defconstant *<defined-class>-precedence-list-location* 7)
(defconstant *<defined-class>-direct-slots-location* 8)
(defconstant *<defined-class>-slots-location* 9)
(defconstant *<defined-class>-slot-location-table-location* 10)
(defconstant *<defined-class>-direct-default-initargs-location* 11)
(defconstant *<defined-class>-default-initargs-location* 12)
(defconstant *<defined-class>-documentation-location* 13)
(defconstant *<defined-class>-listeners-location* 14)
(defconstant *<defined-class>-initialized-location* 15)

;; Preliminary accessors.
(predefun class-direct-superclasses (object)
  (sys::%record-ref object *<defined-class>-direct-superclasses-location*))
(predefun (setf class-direct-superclasses) (new-value object)
  (setf (sys::%record-ref object *<defined-class>-direct-superclasses-location*) new-value))
(predefun class-all-superclasses (object)
  (sys::%record-ref object *<defined-class>-all-superclasses-location*))
(predefun (setf class-all-superclasses) (new-value object)
  (setf (sys::%record-ref object *<defined-class>-all-superclasses-location*) new-value))
(predefun class-precedence-list (object)
  (sys::%record-ref object *<defined-class>-precedence-list-location*))
(predefun (setf class-precedence-list) (new-value object)
  (setf (sys::%record-ref object *<defined-class>-precedence-list-location*) new-value))
(predefun class-direct-slots (object)
  (sys::%record-ref object *<defined-class>-direct-slots-location*))
(predefun (setf class-direct-slots) (new-value object)
  (setf (sys::%record-ref object *<defined-class>-direct-slots-location*) new-value))
(predefun class-slots (object)
  (sys::%record-ref object *<defined-class>-slots-location*))
(predefun (setf class-slots) (new-value object)
  (setf (sys::%record-ref object *<defined-class>-slots-location*) new-value))
(predefun class-slot-location-table (object)
  (sys::%record-ref object *<defined-class>-slot-location-table-location*))
(predefun (setf class-slot-location-table) (new-value object)
  (setf (sys::%record-ref object *<defined-class>-slot-location-table-location*) new-value))
(predefun class-direct-default-initargs (object)
  (sys::%record-ref object *<defined-class>-direct-default-initargs-location*))
(predefun (setf class-direct-default-initargs) (new-value object)
  (setf (sys::%record-ref object *<defined-class>-direct-default-initargs-location*) new-value))
(predefun class-default-initargs (object)
  (sys::%record-ref object *<defined-class>-default-initargs-location*))
(predefun (setf class-default-initargs) (new-value object)
  (setf (sys::%record-ref object *<defined-class>-default-initargs-location*) new-value))
(predefun class-documentation (object)
  (sys::%record-ref object *<defined-class>-documentation-location*))
(predefun (setf class-documentation) (new-value object)
  (setf (sys::%record-ref object *<defined-class>-documentation-location*) new-value))
(predefun class-listeners (object)
  (sys::%record-ref object *<defined-class>-listeners-location*))
(predefun (setf class-listeners) (new-value object)
  (setf (sys::%record-ref object *<defined-class>-listeners-location*) new-value))
(predefun class-initialized (object)
  (sys::%record-ref object *<defined-class>-initialized-location*))
(predefun (setf class-initialized) (new-value object)
  (setf (sys::%record-ref object *<defined-class>-initialized-location*) new-value))

(defun canonicalized-slot-p (x)
  ; A "canonicalized slot specification" is a special kind of property list.
  ; See MOP p. 13-15.
  (and (proper-list-p x)
       (evenp (length x))
       (let ((default '#:default))
         (not (eq (getf x ':name default) default)))))

(defun canonicalized-default-initarg-p (x)
  ; A "canonicalized default initarg" is an element of an alist mapping
  ; a slot name (a symbol) to a list of the form (form function).
  ; See MOP p. 16.
  (and (consp x) (symbolp (first x))
       (consp (cdr x)) (consp (cddr x)) (functionp (third x))
       (null (cdddr x))))

;; Initialization of a <defined-class> instance.
(defun shared-initialize-<defined-class> (class situation &rest args
                                          &key (name nil)
                                               (direct-superclasses nil direct-superclasses-p)
                                               ((:direct-slots direct-slots-as-lists) '() direct-slots-as-lists-p)
                                               ((direct-slots direct-slots-as-metaobjects) '() direct-slots-as-metaobjects-p)
                                               (direct-default-initargs nil direct-default-initargs-p)
                                               (documentation nil documentation-p)
                                          &allow-other-keys
                                          &aux old-direct-superclasses)
  (setq old-direct-superclasses
        (if (eq situation 't) ; called from initialize-instance?
          '()
          (sys::%record-ref class *<defined-class>-direct-superclasses-location*)))
  (apply #'shared-initialize-<potential-class> class situation args)
  (unless *classes-finished*
    ; Bootstrapping: Simulate the effect of #'%shared-initialize.
    (when (eq situation 't) ; called from initialize-instance?
      (setf (class-slot-location-table class) empty-ht)
      (setf (class-listeners class) nil)
      (setf (class-initialized class) 0)))
  (when (eq situation 't)
    ; shared-initialize-<potential-class> has initialized the name.
    (setf (class-initialized class) 1))
  ; Get the name, for error message purposes.
  (setq name (class-classname class))
  (when (or (eq situation 't) direct-superclasses-p)
    ; Check the direct-superclasses.
    (unless (proper-list-p direct-superclasses)
      (error #1=(TEXT "(~S ~S) for class ~S: The ~S argument should be a proper list, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'class name ':direct-superclasses direct-superclasses))
    (unless (every #'(lambda (x)
                       (or (defined-class-p x)
                           (forward-reference-to-class-p x)))
                   direct-superclasses)
      (error (TEXT "(~S ~S) for class ~S: The direct-superclasses list should consist of classes, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'class name direct-superclasses))
    (when (and (> (length direct-superclasses) 1)
               (typep class <structure-class>))
      (error (TEXT "(~S ~S) for class ~S: The metaclass ~S forbids more than one direct superclass: It does not support multiple inheritance.")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'class name (class-of class)))
    (dolist (sc direct-superclasses)
      (when (defined-class-p sc)
        (check-allowed-superclass class sc)))
    (when (null direct-superclasses)
      (setq direct-superclasses (default-direct-superclasses class))))
  (when (or (eq situation 't) direct-slots-as-lists-p)
    ; Check the direct-slots.
    (unless (proper-list-p direct-slots-as-lists)
      (error #1# (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'class name ':direct-slots direct-slots-as-lists))
    (dolist (sl direct-slots-as-lists)
      (unless (canonicalized-slot-p sl)
        (error (TEXT "(~S ~S) for class ~S: The direct slot specification ~S is not in the canonicalized form (slot-name initform initfunction).")
               (if (eq situation 't) 'initialize-instance 'shared-initialize)
               'class name sl))))
  (when (or (eq situation 't) direct-default-initargs-p)
    ; Check the direct-default-initargs.
    (unless (proper-list-p direct-default-initargs)
      (error #1# (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'class name ':direct-default-initargs direct-default-initargs))
    (dolist (definitarg direct-default-initargs)
      (unless (canonicalized-default-initarg-p definitarg)
        (error (TEXT "(~S ~S) for class ~S: The direct default initarg ~S is not in canonicalized form (a property list).")
               (if (eq situation 't) 'initialize-instance 'shared-initialize)
               'class name definitarg))))
  (when (or (eq situation 't) documentation-p)
    ; Check the documentation.
    (unless (or (null documentation) (stringp documentation))
      (error (TEXT "(~S ~S) for class ~S: The ~S argument should be a string or NIL, not ~S")
             (if (eq situation 't) 'initialize-instance 'shared-initialize)
             'class name :documentation documentation)))
  ; Fill the slots.
  (when (or (eq situation 't) direct-superclasses-p)
    (setf (class-direct-superclasses class) (copy-list direct-superclasses))
    (update-subclasses-sets class old-direct-superclasses direct-superclasses))
  (when (or (eq situation 't) direct-slots-as-lists-p direct-slots-as-metaobjects-p)
    (setf (class-direct-slots class)
          (if direct-slots-as-metaobjects-p
            direct-slots-as-metaobjects
            (convert-direct-slots class direct-slots-as-lists))))
  (when (or (eq situation 't) direct-default-initargs-p)
    (setf (class-direct-default-initargs class) direct-default-initargs))
  (when (or (eq situation 't) documentation-p)
    (setf (class-documentation class) documentation))
  ; The following slots are initialized by the subclass' shared-initialize:
  ;   all-superclasses
  ;   precedence-list
  ;   slots
  ;   slot-location-table
  ;   default-initargs
  ; Now allow the user to call some class-xxx accessor functions.
  (when (eq situation 't)
    (setf (class-initialized class) 2))
  class)

;;; ===========================================================================

;;; The class <built-in-class> represents those classes for which the user
;;; cannot create subclasses.

(defvar <built-in-class> 'built-in-class)
(defvar *<built-in-class>-defclass*
  '(defclass built-in-class (defined-class)
     (($prototype          ; class prototype - an instance
        :type t))
     (:fixed-slot-locations t)))
(defvar *<built-in-class>-class-version* (make-class-version))

;; Fixed slot locations.
(defconstant *<built-in-class>-prototype-location* 16)

(defconstant *<built-in-class>-instance-size* 17)

;;; ===========================================================================

;;; The class <slotted-class> represents those classes for which the local
;;; slot values are stored in the instance. It also represents common
;;; behaviour of <standard-class> and <structure-class>.

(defvar *<slotted-class>-defclass*
  '(defclass slotted-class (defined-class)
     (($subclass-of-stablehash-p ; true if <standard-stablehash> or
                           ; <structure-stablehash> is among the superclasses
        :type boolean)
      ($generic-accessors  ; flag whether to create the accessors as methods;
                           ; if false, regular functions are used
        :initform t)
      ($direct-accessors   ; automatically generated accessor methods
                           ; (as plist)
        :type list
        :initform '())
      ($valid-initargs-from-slots ; list of valid initargs, computed from slots
        :type list)        ; (not including those declared valid by methods!)
      ($instance-size      ; number of local slots of the direct instances + 1
        :type (integer 1 *)))
     (:fixed-slot-locations t)))

;; Fixed slot locations.
(defconstant *<slotted-class>-subclass-of-stablehash-p-location* 16)
(defconstant *<slotted-class>-generic-accessors-location* 17)
(defconstant *<slotted-class>-direct-accessors-location* 18)
(defconstant *<slotted-class>-valid-initargs-from-slots-location* 19)
(defconstant *<slotted-class>-instance-size-location* 20)

;; Preliminary accessors.
(predefun class-subclass-of-stablehash-p (object)
  (sys::%record-ref object *<slotted-class>-subclass-of-stablehash-p-location*))
(predefun (setf class-subclass-of-stablehash-p) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-subclass-of-stablehash-p-location*) new-value))
(predefun class-generic-accessors (object)
  (sys::%record-ref object *<slotted-class>-generic-accessors-location*))
(predefun (setf class-generic-accessors) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-generic-accessors-location*) new-value))
(predefun class-direct-accessors (object)
  (sys::%record-ref object *<slotted-class>-direct-accessors-location*))
(predefun (setf class-direct-accessors) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-direct-accessors-location*) new-value))
(predefun class-valid-initargs-from-slots (object)
  (sys::%record-ref object *<slotted-class>-valid-initargs-from-slots-location*))
(predefun (setf class-valid-initargs-from-slots) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-valid-initargs-from-slots-location*) new-value))
(predefun class-instance-size (object)
  (sys::%record-ref object *<slotted-class>-instance-size-location*))
(predefun (setf class-instance-size) (new-value object)
  (setf (sys::%record-ref object *<slotted-class>-instance-size-location*) new-value))

;; Initialization of a <slotted-class> instance.
(defun shared-initialize-<slotted-class> (class situation &rest args
                                          &key (generic-accessors t generic-accessors-p)
                                          &allow-other-keys)
  (apply #'shared-initialize-<defined-class> class situation args)
  (unless *classes-finished*
    ; Bootstrapping: Simulate the effect of #'%shared-initialize.
    (when (eq situation 't) ; called from initialize-instance?
      (setf (class-direct-accessors class) '())))
  (when (or (eq situation 't) generic-accessors-p)
    (setf (class-generic-accessors class) generic-accessors))
  ; The following slots are initialized by the subclass' shared-initialize:
  ;   subclass-of-stablehash-p
  ;   valid-initargs-from-slots
  ;   instance-size
  class)

;;; ===========================================================================

;;; The class <structure-class> represents classes like those defined through
;;; DEFSTRUCT.

(defvar <structure-class> 'structure-class)
(defvar *<structure-class>-defclass*
  '(defclass structure-class (slotted-class)
     (($names              ; encoding of the include-nesting, a list
                           ; (name_1 ... name_i-1 name_i) with name=name_1,
        :type cons)        ; name_1 contains name_2, ..., name_i-1 contains name_i.
      ($kconstructor       ; name of keyword constructor function
        :type symbol)
      ($boa-constructors   ; list of all BOA constructor function names
       :type list)
      ($copier             ; name of the copier function
       :type symbol)
      ($predicate          ; name of the predicate function
       :type symbol)
      ($prototype          ; class prototype - an instance or NIL
        :type (or structure-object null)))
     (:fixed-slot-locations t)))
(defvar *<structure-class>-class-version* (make-class-version))

;; Fixed slot locations.
(defconstant *<structure-class>-names-location* 21)
(defconstant *<structure-class>-kconstructor-location* 22)
(defconstant *<structure-class>-boa-constructors-location* 23)
(defconstant *<structure-class>-copier-location* 24)
(defconstant *<structure-class>-predicate-location* 25)
(defconstant *<structure-class>-prototype-location* 26)

;; Preliminary accessors.
(predefun class-names (object)
  (sys::%record-ref object *<structure-class>-names-location*))
(predefun (setf class-names) (new-value object)
  (setf (sys::%record-ref object *<structure-class>-names-location*) new-value))
(predefun class-kconstructor (object)
  (sys::%record-ref object *<structure-class>-kconstructor-location*))
(predefun (setf class-kconstructor) (new-value object)
  (setf (sys::%record-ref object *<structure-class>-kconstructor-location*) new-value))
(predefun class-boa-constructors (object)
  (sys::%record-ref object *<structure-class>-boa-constructors-location*))
(predefun (setf class-boa-constructors) (new-value object)
  (setf (sys::%record-ref object *<structure-class>-boa-constructors-location*) new-value))
(predefun class-copier (object)
  (sys::%record-ref object *<structure-class>-copier-location*))
(predefun (setf class-copier) (new-value object)
  (setf (sys::%record-ref object *<structure-class>-copier-location*) new-value))
(predefun class-predicate (object)
  (sys::%record-ref object *<structure-class>-predicate-location*))
(predefun (setf class-predicate) (new-value object)
  (setf (sys::%record-ref object *<structure-class>-predicate-location*) new-value))

(defconstant *<structure-class>-instance-size* 27)

;;; ===========================================================================

;;; The class <semi-standard-class> is a common superclass of <standard-class>
;;; and <funcallable-standard-class>. Both implement the "default" CLOS
;;; behaviour.

(defvar <semi-standard-class> 'semi-standard-class)
(defvar *<semi-standard-class>-defclass*
  '(defclass semi-standard-class (slotted-class)
     (($current-version    ; most recent class-version, points back to this
                           ; class
        :type simple-vector)
      ($funcallablep       ; flag whether direct instances are funcallable
        :type boolean)
      ($fixed-slot-locations ; flag whether to guarantee same slot locations
                           ; in all subclasses
        :initarg :fixed-slot-locations
        )
      ($instantiated       ; true if an instance has already been created
        :type boolean
        :initform nil)
      ($direct-instance-specializers ; set of all eql-specializers of direct
                           ; instances that may be used in methods, as a
                           ; weak-list or weak-hash-table or NIL
        :type (or hash-table weak-list null)
        :initform nil)
      ($finalized-direct-subclasses ; set of all finalized direct subclasses,
                           ; as a weak-list or weak-hash-table or NIL
        :type (or hash-table weak-list null)
        :initform '())
      ($prototype          ; class prototype - an instance or NIL
        :type (or standard-object null)))
     (:default-initargs :fixed-slot-locations nil)
     (:fixed-slot-locations t)))

;; Fixed slot locations.
(defconstant *<semi-standard-class>-current-version-location* 21)
(defconstant *<semi-standard-class>-funcallablep-location* 22)
(defconstant *<semi-standard-class>-fixed-slot-locations-location* 23)
(defconstant *<semi-standard-class>-instantiated-location* 24)
(defconstant *<semi-standard-class>-direct-instance-specializers-location* 25)
(defconstant *<semi-standard-class>-finalized-direct-subclasses-location* 26)
(defconstant *<semi-standard-class>-prototype-location* 27)

;; Preliminary accessors.
(predefun class-current-version (object)
  (sys::%record-ref object *<semi-standard-class>-current-version-location*))
(predefun (setf class-current-version) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-current-version-location*) new-value))
(predefun class-funcallablep (object)
  (sys::%record-ref object *<semi-standard-class>-funcallablep-location*))
(predefun (setf class-funcallablep) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-funcallablep-location*) new-value))
(predefun class-fixed-slot-locations (object)
  (sys::%record-ref object *<semi-standard-class>-fixed-slot-locations-location*))
(predefun (setf class-fixed-slot-locations) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-fixed-slot-locations-location*) new-value))
(predefun class-instantiated (object)
  (sys::%record-ref object *<semi-standard-class>-instantiated-location*))
(predefun (setf class-instantiated) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-instantiated-location*) new-value))
(predefun class-direct-instance-specializers-table (object)
  (sys::%record-ref object *<semi-standard-class>-direct-instance-specializers-location*))
(predefun (setf class-direct-instance-specializers-table) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-direct-instance-specializers-location*) new-value))
(predefun class-finalized-direct-subclasses-table (object)
  (sys::%record-ref object *<semi-standard-class>-finalized-direct-subclasses-location*))
(predefun (setf class-finalized-direct-subclasses-table) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-finalized-direct-subclasses-location*) new-value))
(predefun class-prototype (object)
  (sys::%record-ref object *<semi-standard-class>-prototype-location*))
(predefun (setf class-prototype) (new-value object)
  (setf (sys::%record-ref object *<semi-standard-class>-prototype-location*) new-value))

;;; ===========================================================================

;;; The class <standard-class> represents classes with the "default" CLOS
;;; behaviour.

(defvar <standard-class> 'standard-class) ; ABI
(defvar *<standard-class>-defclass*
  '(defclass standard-class (semi-standard-class)
     ()
     (:fixed-slot-locations t)))
(defvar *<standard-class>-class-version* (make-class-version))

(defconstant *<standard-class>-instance-size* 28)

;; For DEFCLASS macro expansions.
(defconstant *<standard-class>-valid-initialization-keywords* ; ABI
             '(:name :direct-superclasses :direct-slots :direct-default-initargs
               :documentation :generic-accessors :fixed-slot-locations))
(defconstant *<standard-class>-default-initargs* '(:fixed-slot-locations nil))

;;; ===========================================================================

;;; The classes <funcallable-standard-class> and <funcallable-standard-object>
;;; can be defined later.

(defvar <funcallable-standard-class> nil)
(defvar *<funcallable-standard-class>-class-version* nil)
(defvar <funcallable-standard-object> nil)

;;; ===========================================================================

;;; Type tests.

(defun built-in-class-p (object) ; ABI
  (and (std-instance-p object)
       (let ((cv (sys::%record-ref object 0)))
         ; Treat the most frequent cases first, for speed and bootstrapping.
         (cond ((eq cv *<standard-class>-class-version*) nil)
               ((eq cv *<structure-class>-class-version*) nil)
               ((eq cv *<built-in-class>-class-version*) t)
               (t ; Now a slow, but general instanceof test.
                 (gethash <built-in-class>
                          (class-all-superclasses (class-of object))))))))

(defun structure-class-p (object) ; ABI
  (and (std-instance-p object)
       (let ((cv (sys::%record-ref object 0)))
         ; Treat the most frequent cases first, for speed and bootstrapping.
         (cond ((eq cv *<standard-class>-class-version*) nil)
               ((eq cv *<structure-class>-class-version*) t)
               ((eq cv *<built-in-class>-class-version*) nil)
               (t ; Now a slow, but general instanceof test.
                 (gethash <structure-class>
                          (class-all-superclasses (class-of object))))))))

(defun semi-standard-class-p (object)
  (and (std-instance-p object)
       (let ((cv (sys::%record-ref object 0)))
         ; Treat the most frequent cases first, for speed and bootstrapping.
         (cond ((eq cv *<standard-class>-class-version*) t)
               ((eq cv *<structure-class>-class-version*) nil)
               ((eq cv *<built-in-class>-class-version*) nil)
               (t ; Now a slow, but general instanceof test.
                 (gethash <semi-standard-class>
                          (class-all-superclasses (class-of object))))))))

(defun standard-class-p (object) ; ABI
  (and (std-instance-p object)
       (let ((cv (sys::%record-ref object 0)))
         ; Treat the most frequent cases first, for speed and bootstrapping.
         (cond ((eq cv *<standard-class>-class-version*) t)
               ((eq cv *<structure-class>-class-version*) nil)
               ((eq cv *<built-in-class>-class-version*) nil)
               (t ; Now a slow, but general instanceof test.
                 (gethash <standard-class>
                          (class-all-superclasses (class-of object))))))))

(sys::def-atomic-type potential-class potential-class-p)
(sys::def-atomic-type defined-class defined-class-p)
(sys::def-atomic-type built-in-class built-in-class-p)
(sys::def-atomic-type structure-class structure-class-p)
(sys::def-atomic-type standard-class standard-class-p)

(defun forward-reference-to-class-p (object)
  (and (std-instance-p object)
       (gethash <forward-reference-to-class>
                (class-all-superclasses (class-of object)))))

;;; ===========================================================================

;;; Copying.
(defun copy-standard-class (class)
  (let* ((n (sys::%record-length class))
         (copy (allocate-metaobject-instance (sys::%record-ref class 0) n)))
    (dotimes (i n) (setf (sys::%record-ref copy i) (sys::%record-ref class i)))
    copy))

(defun print-object-<potential-class> (object stream)
  (if (and *print-readably* (defined-class-p object))
    ; Only defined-class instances can be restored through FIND-CLASS.
    (write (sys::make-load-time-eval `(FIND-CLASS ',(class-classname object)))
           :stream stream)
    (print-unreadable-object (object stream :type t)
      (let ((name (class-classname object)))
        ;; The class <string> has two names: cl:string and cs-cl:string.
        ;; Which one we show, depends on *package*.
        (when (and (eq name 'string)
                   (eq (find-symbol "STRING" *package*) 'cs-cl:string))
          (setq name 'cs-cl:string))
        (write name :stream stream))
      (when (semi-standard-class-p object)
        (if (and (slot-boundp object '$current-version)
                 (class-version-p (class-current-version object))
                 (slot-boundp object '$precedence-list))
          (progn
            (when (< (class-initialized object) 3) ; not yet finalized?
              (write-string " " stream)
              (write :incomplete :stream stream))
            ;; FIXME: Overhaul this questionable and confusing feature.
            (let ((serial (cv-serial (class-current-version object))))
              (unless (eql serial 0)
                (write-string " " stream)
                (write :version :stream stream)
                (write-string " " stream)
                (write serial :stream stream))))
          (progn
            (write-string " " stream)
            (write :uninitialized :stream stream)))))))

(defun print-object-<forward-reference-to-class> (object stream)
  (print-unreadable-object (object stream :type t)
    (write (slot-value object '$classname) :stream stream)))

;; Preliminary.
;; Now we can at least print classes.
(predefun print-object (object stream)
  (cond ((potential-class-p object) (format stream "#<CLASS ~S>" (class-classname object)))
        (t (write-string "#<UNKNOWN>" stream))))
