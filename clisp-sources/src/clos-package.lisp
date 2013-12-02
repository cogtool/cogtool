;;;; Common Lisp Object System for CLISP
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2005

;; to use it: (USE-PACKAGE "CLOS").

(in-package "COMMON-LISP")
(pushnew ':mop *features*)
(pushnew ':clos *features*)

(in-package "SYSTEM") ; necessary despite DEFPACKAGE!

;; Defined later, in functions.lisp.
(import 'make-signature)
(import 'sig-req-num)
(import 'sig-opt-num)
(import 'sig-rest-p)
(import 'sig-keys-p)
(import 'sig-keywords)
(import 'sig-allow-p)
(import 'check-function-name)

;; Defined later, in compiler.lisp.
(import 'compiler::%generic-function-lambda)
(import 'compiler::%optimize-function-lambda)

(defpackage "CLOS"
  (:nicknames "MOP")
  (:documentation "http://www.lisp.org/HyperSpec/Body/chap-7.html")
  (:import-from "EXT" ext:mapcap ext:proper-list-p)
  (:import-from "SYSTEM"
    ;; Import:
    sys::text                   ; for error messages (i18n.d)
    sys::error-of-type          ; defined in error.d
    sys::check-function-name    ; defined in control.d
    sys::function-name-p        ; defined in control.d
    sys::function-block-name    ; defined in eval.d
    sys::memq                   ; defined in list.d
    sys::predefun               ; defined in init.lisp
    sys::gensym-list            ; defined in macros2.lisp
    sys::analyze-lambdalist     ; defined in lambdalist.lisp
    sys::make-signature         ; defined in functions.lisp
    sys::sig-req-num sys::sig-opt-num sys::sig-rest-p ; likewise
    sys::sig-keys-p sys::sig-keywords sys::sig-allow-p ; likewise
    ;; clos::class-p clos::defined-class-p ; defined in predtype.d
    ;; clos:class-of clos:find-class ; defined in predtype.d
    ;; clos::typep-class        ; defined in predtype.d
    ;; clos::structure-object-p ; defined in record.d
    ;; clos::std-instance-p clos::allocate-std-instance ; defined in record.d
    ;; clos::%allocate-instance ; defined in record.d
    ;; clos:slot-value clos::set-slot-value ; defined in record.d
    ;; clos:slot-boundp clos:slot-makunbound ; defined in record.d
    ;; clos:slot-exists-p ; defined in record.d
    ;; clos::class-gethash clos::class-tuple-gethash ; defined in hashtabl.d
    compiler::%generic-function-lambda ; defined in compiler.lisp
    compiler::%optimize-function-lambda ; defined in compiler.lisp
    ;; clos:generic-flet clos:generic-labels ; treated in compiler.lisp
    ;; Export:
    ;; clos::closclass ; property in predtype.d, type.lisp, compiler.lisp
    ;; clos:class      ; used in record.d
    ;; clos:generic-function ; used in type.lisp, compiler.lisp
    ;; clos:standard-generic-function ; used in predtype.d, type.lisp, compiler.lisp
    ;; clos:slot-missing clos:slot-unbound  ; called by record.d
    ;; clos::*make-instance-table*          ; used in record.d
    ;; clos::*reinitialize-instance-table*  ; used in record.d
    ;; clos::initial-reinitialize-instance  ; called by record.d
    ;; clos::initial-initialize-instance    ; called by record.d
    ;; clos::initial-make-instance          ; called by record.d
    ;; clos:print-object                    ; called by io.d
    ;; clos:describe-object                 ; called by user2.lisp
    ;; clos::define-structure-class         ; called by defstruct.lisp
    ;; clos::defstruct-remove-print-object-method ; called by defstruct.lisp
    ;; clos::built-in-class-p               ; called by type.lisp
    ;; clos::subclassp  ; called by type.lisp, used in compiler.lisp
    ;; clos:class-name                      ; used in type.lisp, compiler.lisp
    ;; clos:find-class                      ; used in compiler.lisp
    ;; clos::defgeneric-lambdalist-callinfo ; called by compiler.lisp
    ;; clos::make-generic-function-form     ; called by compiler.lisp
    )) ; defpackage

(in-package "CLOS")

;;; exports: ** also in init.lisp ** !
(export
 '(;; names of functions and macros:
   slot-value slot-boundp slot-makunbound slot-exists-p with-slots
   with-accessors documentation
   find-class class-of defclass defmethod call-next-method next-method-p
   defgeneric generic-function generic-flet generic-labels
   class-name no-applicable-method no-next-method no-primary-method
   find-method add-method remove-method
   compute-applicable-methods method-qualifiers function-keywords
   slot-missing slot-unbound
   print-object describe-object
   make-instance allocate-instance initialize-instance reinitialize-instance
   shared-initialize ensure-generic-function
   make-load-form make-load-form-saving-slots
   change-class update-instance-for-different-class
   update-instance-for-redefined-class make-instances-obsolete
   ;; names of classes:
   class standard-class structure-class built-in-class
   standard-object structure-object
   generic-function standard-generic-function method standard-method
   ;; method combinations
   standard method-combination define-method-combination
   method-combination-error invalid-method-error
   call-method make-method))

;;; MOP exports: ** also in init.lisp ** !
(export
        '(metaobject
          ;; MOP for dependents
          add-dependent remove-dependent map-dependents update-dependent
          ;; MOP for slot definitions
          slot-definition standard-slot-definition
          direct-slot-definition standard-direct-slot-definition
          effective-slot-definition standard-effective-slot-definition
          slot-definition-name
          slot-definition-initform slot-definition-initfunction
          slot-definition-type slot-definition-allocation
          slot-definition-initargs
          slot-definition-readers slot-definition-writers
          slot-definition-location
          ;; MOP for slot access
          slot-value-using-class slot-boundp-using-class
          slot-makunbound-using-class
          standard-instance-access funcallable-standard-instance-access
          ;; MOP for classes
          class forward-referenced-class
          built-in-class structure-class standard-class
          class-name class-direct-superclasses class-precedence-list
          class-direct-subclasses class-direct-slots class-slots
          class-direct-default-initargs class-default-initargs class-prototype
          class-finalized-p finalize-inheritance
          compute-direct-slot-definition-initargs direct-slot-definition-class
          compute-class-precedence-list
          compute-slots compute-effective-slot-definition
          compute-effective-slot-definition-initargs
          effective-slot-definition-class
          compute-default-initargs
          validate-superclass add-direct-subclass remove-direct-subclass
          standard-accessor-method
          standard-reader-method standard-writer-method
          reader-method-class writer-method-class
          ensure-class ensure-class-using-class
          ;; MOP for specializers
          specializer eql-specializer
          specializer-direct-generic-functions specializer-direct-methods
          add-direct-method remove-direct-method
          eql-specializer-object intern-eql-specializer
          ;; MOP for methods
          method standard-method
          method-function method-generic-function method-lambda-list
          method-specializers method-qualifiers accessor-method-slot-definition
          extract-lambda-list extract-specializer-names
          ;; MOP for method combinations
          find-method-combination compute-effective-method
          ;; MOP for generic functions
          funcallable-standard-class funcallable-standard-object
          set-funcallable-instance-function
          generic-function-name generic-function-methods
          generic-function-method-class generic-function-lambda-list
          generic-function-method-combination
          generic-function-argument-precedence-order
          generic-function-declarations
          compute-discriminating-function
          compute-applicable-methods compute-applicable-methods-using-classes
          compute-effective-method-as-function
          ensure-generic-function ensure-generic-function-using-class
          ;; CLISP specific symbols
          generic-flet generic-labels no-primary-method
          method-call-error method-call-type-error
          method-call-error-generic-function
          method-call-error-method method-call-error-argument-list
          standard-stablehash structure-stablehash
)        )
