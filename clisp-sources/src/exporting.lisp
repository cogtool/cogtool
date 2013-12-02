;;; Macros that export their definienda
;;; Bruno Haible 2004-12-15
;;; Sam Steingold 2005

(defpackage "EXPORTING"
  (:use "COMMON-LISP")
  (:documentation "Replacements for the standard macros with the versions that export their definienda.")
  (:shadow . #1=(defconstant defparameter defvar define-symbol-macro
                 defun defgeneric defmethod define-compiler-macro defsetf
                 define-setf-expander defmacro define-modify-macro
                 deftype defstruct defclass define-condition
                 define-method-combination
                 #+FFI def-c-type #+FFI def-c-enum #+FFI def-c-struct
                 #+FFI def-c-var #+FFI def-c-const
                 #+FFI def-c-call-out #+FFI def-call-out
                 #+AFFI def-lib-call-out))
  (:export . #1#))

(in-package "EXPORTING")
(setf (documentation (find-package "EXPORTING") 'sys::impnotes) "exporting")

;; Macros for the variable namespace.

(cl:defmacro defconstant (&whole whole
                          name initial-value &optional documentation)
  (declare (ignore initial-value documentation))
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFCONSTANT ,name ,@(cddr whole))))

(cl:defmacro defparameter (&whole whole
                           name initial-value &optional documentation)
  (declare (ignore initial-value documentation))
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFPARAMETER ,name ,@(cddr whole))))

(cl:defmacro defvar (&whole whole
                     name &optional initial-value documentation)
  (declare (ignore initial-value documentation))
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFVAR ,name ,@(cddr whole))))

(cl:defmacro define-symbol-macro (symbol expansion)
  `(PROGN
     (EXPORT ',(or symbol '(NIL)))
     (CL:DEFINE-SYMBOL-MACRO ,symbol ,expansion)))

;; Macros for the function namespace.

(cl:defmacro defun (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or (sys::function-block-name name) '(NIL)))
     (CL:DEFUN ,name ,lambda-list ,@body)))

(cl:defmacro defgeneric (name lambda-list &rest options)
  `(PROGN
     (EXPORT ',(or (sys::function-block-name name) '(NIL)))
     (CL:DEFGENERIC ,name ,lambda-list ,@options)))

(cl:defmacro defmethod (name &rest definition)
  `(PROGN
     (EXPORT ',(or (sys::function-block-name name) '(NIL)))
     (CL:DEFMETHOD ,name ,@definition)))

(cl:defmacro define-compiler-macro (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or (sys::function-block-name name) '(NIL)))
     (CL:DEFINE-COMPILER-MACRO ,name ,lambda-list ,@body)))

(cl:defmacro defsetf (name &rest definition)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFSETF ,name ,@definition)))

(cl:defmacro define-setf-expander (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFINE-SETF-EXPANDER ,name ,lambda-list ,@body)))

(cl:defmacro defmacro (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFMACRO ,name ,lambda-list ,@body)))

(cl:defmacro define-modify-macro (&whole whole
                                  name lambda-list function
                                  &optional documentation)
  (declare (ignore lambda-list function documentation))
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFINE-MODIFY-MACRO ,name ,@(cddr whole))))

;; Macros for the type namespace.

(cl:defmacro deftype (name lambda-list &body body)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFTYPE ,name ,lambda-list ,@body)))

(cl:defun slot-definition-accessor-symbols (slot)
  (mapcar #'sys::function-block-name
          (append (clos:slot-definition-readers slot)
                  (clos:slot-definition-writers slot))))

(cl:defun all-accessor-symbols (direct-slot-list)
  (mapcan #'slot-definition-accessor-symbols direct-slot-list))

(cl:defun class-accessor-symbols (class) ; ABI
  (all-accessor-symbols (clos:class-direct-slots class)))

(cl:defun export-structure-accessories (name) ; ABI
  (export name)
  (export (sys::structure-kconstructor name))
  (export (sys::structure-boa-constructors name))
  (export (sys::structure-copier name))
  (export (sys::structure-predicate name))
  (dolist (slot (sys::structure-direct-slots name))
    (export (slot-definition-accessor-symbols slot))))

(cl:defmacro defstruct (name+options &rest slots)
  `(LET ((NAME (CL:DEFSTRUCT ,name+options ,@slots)))
     (EXPORT-STRUCTURE-ACCESSORIES NAME)
     NAME))

(cl:defmacro defclass (name superclasses slot-specs &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (LET ((C (CL:DEFCLASS ,name ,superclasses ,slot-specs ,@options)))
       (EXPORT (CLASS-ACCESSOR-SYMBOLS C))
       C)))

(cl:defmacro define-condition (name parent-types slot-specs &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (LET ((C (CL:DEFINE-CONDITION ,name ,parent-types ,slot-specs ,@options)))
       (EXPORT (CLASS-ACCESSOR-SYMBOLS (FIND-CLASS C)))
       C)))

;; Macros for the method-combination namespace.

(cl:defmacro define-method-combination (name &rest definition)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (CL:DEFINE-METHOD-COMBINATION ,name ,@definition)))

;; FFI.

#+FFI
(cl:defmacro def-c-type (name typespec)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-C-TYPE ,name ,typespec)))

#+FFI
(cl:defmacro def-c-enum (name &rest items)
  `(PROGN
     (EXPORT '(,name ,@(mapcar
                         #'(lambda (item) (if (consp item) (first item) item))
                         items)))
     (FFI:DEF-C-ENUM ,name ,@items)))

#+FFI
(cl:defmacro def-c-struct (name+options &rest slots)
  `(LET ((NAME (FFI:DEF-C-STRUCT ,name+options ,@slots)))
     (EXPORT-STRUCTURE-ACCESSORIES NAME)
     NAME))

#+FFI
(cl:defmacro def-c-const (name &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-C-CONST ,name ,@options)))

#+FFI
(cl:defmacro def-c-var (name &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-C-VAR ,name ,@options)))

#+FFI
(cl:defmacro def-c-call-out (name &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-C-CALL-OUT ,name ,@options)))

#+FFI
(cl:defmacro def-call-out (name &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-CALL-OUT ,name ,@options)))

#+AFFI
(cl:defmacro def-lib-call-out (name library &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-LIB-CALL-OUT ,name ,library ,@options)))

#| ;; def-c-call-in and def-call-in don't actually define anything;
   ;; they are more like declarations.

#+FFI
 (cl:defmacro def-c-call-in (name &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-C-CALL-IN ,name ,@options)))

#+FFI
 (cl:defmacro def-call-in (name &rest options)
  `(PROGN
     (EXPORT ',(or name '(NIL)))
     (FFI:DEF-CALL-IN ,name ,@options)))

|#

(pushnew "EXPORTING" custom:*system-package-list* :test #'string=)
