;;;; Common Lisp Object System for CLISP
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08
;;;; method combinations: James Anderson 2003

(in-package "CLOS")

;;; preliminary remarks:

;; abbreviations:
;; std = standard
;; gf = generic function
;; <...> = (class ...), mostly = (find-class '...)
;; em = effective method

; Now DEFCLASS works (except for accessor methods).
(load "clos-slots1")
(load "clos-method1")
(load "clos-methcomb1")
(load "clos-genfun1")
(load "clos-genfun2a")
(load "clos-methcomb2")
(load "clos-genfun2b")
(load "clos-method2")
(load "clos-genfun3")
; Now DEFGENERIC, DEFMETHOD work. DEFCLASS works fully.
(load "clos-dependent")
(load "clos-genfun4")
(load "clos-method3")
(load "clos-methcomb3")
(load "clos-slots2")
(load "clos-slotdef2")
(load "clos-stablehash2")
(load "clos-specializer2")
(load "clos-class4")
(load "clos-class5")
; Now instance creation works. Instances can be passed to generic functions.
(setq *classes-finished* t)
(setq *allow-making-generic* t)
(load "clos-slotdef3")
(load "clos-specializer3")
(load "clos-class6")
(load "clos-method4")
(load "clos-methcomb4")
(load "clos-genfun5")
(load "clos-print")
(load "clos-custom")
(setq *allow-making-generic* nil)
(load "documentation")


;; Bootstrapping techniques

; Due to the extreme self-circularity of CLOS - classes are CLOS instances
; and therefore described by CLOS classes, generic functions are customizable
; through generic functions, CLOS instances are constructed through
; make-instance which is a generic function, etc. - several techniques need
; to be used for bootstrapping.

; 1) class-version
; Since each CLOS instance points to its class, and CLOS classes are
; themselves CLOS instances - how can the first CLOS class be created?
; In particular, the class STANDARD-CLASS is a direct instance of itself.
; But it also inherits from the classes CLASS, STANDARD-OBJECT and T, which
; are themselves also instances of CLASS.
; The solution is to realize that each instance points to a CLASS-VERSION
; which itself points to the CLASS. These CLASS-VERSION objects are ideal
; points for breaking the circularity, because they are just plain
; simple-vectors and because they are small in number: just one per concrete
; class.
; So for a concrete class <foo> we pre-allocate a class-version:
;
;   (defvar *<foo>-class-version* (make-class-version))
;
; and use it during bootstrapping:
;
;   (defun make-instance-<foo> (class &rest args &key &allow-other-keys)
;     (declare (ignore class))
;     (let ((instance (allocate-metaobject-instance *<foo>-class-version* n)))
;       (apply #'initialize-instance-<foo> instance args)))
;
; Later, immediately after the class is really defined (which creates a new
; class-version for this class) we only need to connect the old class-version
; with the new class:
;
;   (defparameter <foo> (defclass foo ...))
;   (replace-class-version <foo> *<foo>-class-version*)
;
; No need to update already created <foo> instances!

; 2) fixed-slot-locations
; Each local slot value of a standard-object instance is stored at an index
; called "slot location". Each slot value access has, to determine it, to
; look it up in the class' slot-location-table (or similar). But how can it
; know where to find the slot-location-table? In the presence of multiple
; inheritance - which is explicitly allowed for subclasses of CLASS - the
; slot-location-table's index could depend on the class, thus again requiring
; a slot-location-table access in the metaclass, etc. ad infinitum.
; There are two ways to resolve this:
; a) Note that every chain x -> (CLASS-OF x) -> (CLASS-OF (CLASS-OF x)) -> ...
;    must eventually end at the STANDARD-CLASS class. This is because user
;    defined classes must have a metaclass that was defined before them, and
;    among the predefined classes, the STANDARD-CLASS class is the only object
;    for which (EQ x (CLASS-OF x)).
; b) The MOP has a restriction that does not allow programs to create classes
;    that inherit from two categories of metaobjects (classes, slot-definitions,
;    generic functions, methods, method-combinations) simultaneously. See
;    MOP p. 3: "A metaobject class is a subclass of exactly one of these
;    classes. The results are undefined if an attempt is made to define a
;    class that is a subclass of more than one basic metaobject class."
;    This allows the Lisp implementation to fix the index of every slot of the
;    built-in metaobject classes.
; We use approach b), since it obviously leads to more efficient operations.
; (In particular, it allows the operations in record.d to use fixed indices
; for the slot-location-table.)
; To this effect, DEFCLASS with metaclass STANDARD-CLASS supports an option
; (:FIXED-SLOT-LOCATIONS T) that specifies that all local slots listed in this
; class will be accessible under the same slot locations in _all_ subclasses.
; Effectively this disallows some kinds of multiple inheritance; an error
; is signalled during the computation of the slot locations of subclasses if
; the constraints cannot be fulfilled.
; If a class has (:FIXED-SLOT-LOCATIONS T), it makes sense to mark all its
; superclasses that have slots as (:FIXED-SLOT-LOCATIONS T) as well.
; So, after
;
;   (defclass foo ()
;     (slot1 slot2 ... slotN)
;     (:fixed-slot-locations t))
;
; we know that slot1 will be at index 1, slot2 at index 2, etc. in all
; subclasses.

; 3) initialize-instance and make-instance methods
; make-instance and initialize-instance are generic functions used for the
; construction of instances. But they are themselves built from methods,
; which are CLOS instances, and whose specializers are CLOS classes, therefore
; also CLOS instances. How can the first CLOS instances be created then?
; The solution is to use plain non-generic functions for creating and
; initializing all CLOS instances, until the CLOS is sufficiently complete
; for executing generic functions like make-instance and initialize-instance.
; But we want 1. to avoid code duplication, 2. to avoid differences in
; behaviour between the incomplete CLOS and the complete CLOS.
; The way to achieve this is to define the bulk of the built-in methods of
; user-extensible generic functions in plain functions, which then get called
; or replaced by method definitions.
;
; Idiom 1, that can be used for any normal or user-extensible generic function:
;   (defclass foo ...)
;   (defun something-<foo> (a b c) ...)
;   (defun something (a b c) (something-<foo> a b c))
;   (defun other-code ... (something ...))
;   ; and then after CLOS is complete:
;   (fmakunbound 'something)
;   (defgeneric something (a b c)
;     (:method ((a foo) b c)
;       (something-<foo> a b c)))
;
; Idiom 2, that can be used for methods with only the standard behaviour:
;   (defclass foo ...)
;
;   (defun initialize-instance-<foo> (instance &rest args &key &allow-other-keys)
;     (apply #'shared-initialize instance 't args))
;   (defun other-code ... (initialize-instance-<foo> ...))
;   ; and then after CLOS is complete:
;   (setf (fdefinition 'initialize-instance-<foo>) #'initialize-instance)
;
;   (defun make-instance-<foo> (class &rest args &key &allow-other-keys)
;     (declare (ignore class))
;     (let ((instance (allocate-metaobject-instance *<foo>-class-version* n)))
;       (apply #'initialize-instance-<foo> instance args)))
;   (defun other-code ... (make-instance-<foo> ...))
;   ; and then after CLOS is complete:
;   (setf (fdefinition 'make-instance-<foo>) #'make-instance)

; 4) *classes-finished* and %initialize-instance
; When the user creates a subclass of a metaobject class, instances of his
; class should be initialized correctly:
;   (defclass my-method (method)
;     ((slot1 :initform (slot1-init) ...) ...))
; It is the last-priority applicable method of initialize-instance, called
; clos::%initialize-instance, that calls
;   (setf (slot-value new-instance 'slot1) (slot1-init))
; So we have to call clos::%initialize-instance at the beginning of
; initialize-instance-<method>. But we cannot do this while the CLOS is not
; complete - because clos::%initialize-instance does a CLASS-OF of the instance
; and then traverses the slot-location-table of the class. Both of these (the
; pointer from the class-version to the class, and the slot-location-table
; in the class) don't exist at this moment. The fact that CLOS is complete or
; not is indicated by the variable *classes-finished*. So what we do is
;
;   (defun initialize-instance-<method> (instance &rest args &key &allow-other-keys)
;     (when *classes-finished*
;       (apply #'%initialize-instance instance args)) ; == (call-next-method)
;     ...)
;
; But to avoid behaviour differences between *classes-finished* = nil and = t,
; we have simulate the task of %initialize-instance during bootstrapping. In
; the initialize-instance-<xyz> function of each subclass we have to fill the
; slots with the initforms. Example:
;
;   (defun initialize-instance-<foo-method> (instance &rest args &key &allow-other-keys)
;     (apply #'initialize-instance-<method> instance args) ; == (call-next-method)
;     (unless *classes-finished*
;       ; Bootstrapping: Simulate the effect of #'%initialize-instance.
;       (setf (foo-method-slot1) (foo-method-slot1-initform))
;       ...)
;     ...)
;
; For subclasses of <class>, the same holds for shared-initialize instead of
; initialize-instance.

; 5) generic-accessors
; Once CLASS and SLOT-DEFINITION and its subclasses are defined, we can already
; use DEFCLASS to define classes like METHOD or METHOD-COMBINATION. The use of
; (:FIXED-SLOT-LOCATIONS T) guarantees fast accessors if we write them by hand,
; one by one. As a shorthand, to automate the creation of accessors at a moment
; when generic functions don't yet work, the DEFCLASS option
; (:GENERIC-ACCESSORS NIL) allows to create the accessors as plain functions.
; This is like DEFSTRUCT does for STRUCTURE-CLASS/STRUCTURE-OBJECT instance
; accessors, except that it also works for STANDARD-CLASS/STANDARD-OBJECT
; accessors. Thus,
;
;   (defclass foo ()
;     ((slot1 :accessor foo-slot1) ...)
;     (:generic-accessors nil))
;
; is equivalent to
;
;   (defclass foo ()
;     ((slot1) ...)
;   (defun foo-slot1 (instance)
;     (accessor-typecheck instance 'foo 'foo-slot1)
;     (slot-value instance 'slot1))
;   (defun (setf foo-slot1) (new-value instance)
;     (accessor-typecheck instance 'foo '(setf foo-slot1))
;     (setf (slot-value instance 'slot1) new-value))
;
; This can be combined with (:FIXED-SLOT-LOCATIONS T):
;
;   (defclass foo ()
;     ((slot1 :accessor foo-slot1) ...)
;     (:fixed-slot-locations t)
;     (:generic-accessors nil))
;
; is equivalent to
;
;   (defclass foo ()
;     ((slot1) ...)
;   (defun foo-slot1 (instance)
;     (accessor-typecheck instance 'foo 'foo-slot1)
;     (sys::%record-ref instance 1))
;   (defun (setf foo-slot1) (new-value instance)
;     (accessor-typecheck instance 'foo '(setf foo-slot1))
;     (setf (sys::%record-ref instance 1) new-value))


;; Debugging Techniques

; Some generic functions, like compute-effective-method or method-specializers,
; are used to implement the behaviour of generic functions. We have a boot-
; strapping problem here.
; Since we try to call the generic function whenever possible, and the non-
; generic particular method only if absolutely necessary, the technique is to
; write generic code and break the metacircularity at selected points.
; In practice this means you get a "*** - Lisp stack overflow. RESET" message
; and have to break the endless recursion. How to detect the metacircularity
; that causes the endless recursion?
; 1. Start "./lisp.run -i init.lisp | tee log". You get "Lisp stack overflow".
; 2. Find the source form which causes the endless recursion. If you are
;    unsure, add a ":echo t" to the LOAD form loading the particular form in
;    init.lisp or clos.lisp and go back to step 1.
; 3. At the prompt, type  (in-package "CLOS")  and the problematic source form.
; 4. While it is executing, eating more and more stack, interrupt it through a
;    "kill -2 <pid>" from a nearby shell window, where <pid> is the lisp.run's
;    process id. (Just pressing Ctrl-C would kill the lisp.run and tee
;    processes.)
; 5. Type  (ext:show-stack).
; 6. Analyze the resulting log file. To find the loop in the stack trace,
;    concentrate on the middle. You have to skip the first 500 or 1000 lines
;    of stack trace. To get a quick overview, look at only the lines
;    starting with "APPLY frame".
; 7. Think about the ideal point for breaking the loop.


;; Extension Protocols

; The MOP specifies the following individual protocols.
; The "checks" column tells whether the CLISP code contains verifications
; that the user added code is fulfilling the required constraints.
; The "testsuite" column tells whether the mop.tst contains interesting
; testcases.
;                                                     checks  testsuite
; add-dependent remove-dependent map-dependents         --      OK
; add-direct-method remove-direct-method \
;   specializer-direct-generic-functions \
;   specializer-direct-methods                          --      OK
; add-direct-subclass remove-direct-subclass \
;   class-direct-subclasses                             OK      OK
; compute-applicable-methods \
;   compute-applicable-methods-using-classes            OK      OK
; compute-class-precedence-list                         OK      OK
; compute-default-initargs                              OK      OK
; compute-direct-slot-definition-initargs               OK      OK
; compute-discriminating-function                       OK      OK
; compute-effective-method                              --      OK
; compute-effective-slot-definition                     OK      OK
; compute-effective-slot-definition-initargs            OK      OK
; compute-slots                                         OK      OK
; direct-slot-definition-class                          OK      OK
; effective-slot-definition-class                       OK      OK
; ensure-class-using-class                              OK      OK
; ensure-generic-function-using-class                   OK      OK
; make-method-lambda
; reader-method-class                                   OK      OK
; slot-value-using-class \
;   (setf slot-value-using-class) \
;   slot-boundp-using-class \
;   slot-makunbound-using-class                         --      OK
; validate-superclass                                   OK      OK
; writer-method-class                                   OK      OK
