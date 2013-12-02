;;; Foreign function interface for CLISP
;;; Bruno Haible 19.2.1995
;;; Sam Steingold 1998-2005

#+UNICODE
(progn
  (in-package "EXT")
  (export '(custom::*foreign-encoding*) "CUSTOM")
  (export '(custom::*foreign-encoding*) "EXT"))

(use-package '("COMMON-LISP" "EXT") "FFI")
(in-package "FFI")

(export '(def-c-type def-c-var def-c-const parse-c-type deparse-c-type
          def-c-call-out def-call-out #+AFFI def-lib-call-out
          def-c-call-in def-call-in default-foreign-language
          c-lines *output-c-functions* *output-c-variables* *foreign-guard*
          nil boolean character char uchar short ushort int uint long ulong
          uint8 sint8 uint16 sint16 uint32 sint32 uint64 sint64
          single-float double-float
          c-pointer c-string c-struct c-union c-array c-array-max
          c-function c-ptr c-ptr-null c-array-ptr
          def-c-enum def-c-struct element deref slot cast typeof
          sizeof bitsizeof c-var-object c-var-address offset
          validp with-c-place foreign-value enum-from-value enum-to-value
          foreign-address foreign-address-unsigned unsigned-foreign-address
          with-foreign-object with-c-var with-foreign-string
          foreign-allocate allocate-deep allocate-shallow foreign-free
          foreign-pointer set-foreign-pointer
          close-foreign-library memory-as
          foreign-variable foreign-function))

(eval-when (load compile eval)
  (import (intern "*COUTPUT-FILE*" "COMPILER"))
  (import (intern "*COUTPUT-STREAM*" "COMPILER"))
  (import (intern "*FFI-MODULE*" "COMPILER"))
  (import (intern "FINALIZE-COUTPUT-FILE" "COMPILER"))
  (import (intern "TEXT" "SYSTEM")) ; messages
  (import (intern "SYMBOL-TO-KEYWORD" "SYSTEM"))
  (import (intern "CHECK-SYMBOL" "SYSTEM")) ; error checking
  (import (intern "FOREIGN-FUNCTION-IN-ARG-COUNT" "SYSTEM")) ; called by SYS::FUNCTION-SIGNATURE
)

;; These constants are defined in spvw.d.
;; We declare them here only to avoid warnings.
#-FFI
(progn
  (defvar fv-flag-readonly)
  (defvar fv-flag-malloc-free)
  (defvar ff-flag-alloca)
  (defvar ff-flag-malloc-free)
  (defvar ff-flag-out)
  (defvar ff-flag-in-out)
  (defvar ff-language-asm)
  (defvar ff-language-c)
  (defvar ff-language-ansi-c)
  (defvar ff-language-stdcall)
)

;; ===========================================================================

#+UNICODE
(progn
  (define-symbol-macro *foreign-encoding* (system::foreign-encoding))
  (defsetf system::foreign-encoding system::set-foreign-encoding))

;; ============================ helper functions ============================

; Determines whether a name is a valid C identifier.
(defun c-ident-p (name)
  (and (> (length name) 0)
       (every #'(lambda (c)
                 ;(and (standard-char-p ch)
                 ;     (or (alphanumericp ch) (eql ch #\_)) ; don't allow #\$
                 ;)
                  (or (char<= #\A c #\Z) (char<= #\a c #\z) (char<= #\0 c #\9)
                      (char= #\_ c) (char= #\@ c)
                ) )
              name
       )
       (not (char<= #\0 (char name 0) #\9))
       ; must not be a reserved word:
       (not (gethash name
                     (load-time-value
                      (let* ((reserved-list
                               '("auto" "break" "case" "char" "continue"
                                 "default" "do" "double" "else" "enum" "extern"
                                 "float" "for" "goto" "if" "int" "long"
                                 "register" "return" "short" "sizeof" "static"
                                 "struct" "switch" "typedef" "union" "unsigned"
                                 "void" "while"))
                             (reserved-table
                               (make-hash-table :key-type 'string :value-type '(eql t)
                                                :test #'equal)))
                        (dolist (w reserved-list)
                          (setf (gethash w reserved-table) 'T))
                        reserved-table))))))

; Given a string, return it in C syntax.
(defun to-c-string (string)
  (with-output-to-string (s)
    (write-char #\" s)
    (map nil #'(lambda (c)
                 (cond ((eql c #\Null)
                        (error (TEXT "Cannot map string ~S to C since it contains a character ~S")
                               string c))
                       ((eq c #\Newline)
                        (write-char #\\ s) (write-char #\n s))
                       ((or (eql c #\") (eql c #\\))
                        (write-char #\\ s) (write-char c s))
                       (t (write-char c s))))
             string)
    (write-char #\" s)))

;; ============================ C types ============================

;: The table of C types.
(defvar *c-type-table*
  (make-hash-table :key-type 'symbol :test 'stablehash-eq))

; simple C types
(dolist (c-type
          '(nil boolean character char uchar short ushort int uint long ulong
            uint8 sint8 uint16 sint16 uint32 sint32 uint64 sint64
            single-float double-float
            c-pointer c-string))
  (setf (gethash c-type *c-type-table*) c-type))

;; parse the components of a C-STRUCT or C-UNION
(defun parse-components (typespec)
  (let* ((form-type (first typespec))
         (spec-list
           (ecase form-type
             (C-STRUCT (cddr typEspec))
             (C-UNION (cdr typespec)))))
    (mapcar (lambda (subspec)
              (unless (and (consp subspec)
                           (eql (length subspec) 2)
                           (symbolp (first subspec)))
                (error (TEXT "Invalid ~S component: ~S")
                       form-type subspec))
              (parse-c-type (second subspec)))
            spec-list)))

;; return the constructor function for this structure
(defun c-struct-constructor (typespec)
  (let ((class (second typespec)))
    (when (consp class) (setq class (car class)))  ; name+options -> name
    (case class
      (VECTOR #'vector)
      (LIST #'list)
      (t (let* ((slots (mapcar #'first (cddr typespec)))
                (vars (mapcar #'(lambda (x) (gensym (symbol-name x))) slots))
                h)
           (eval `(FUNCTION
                   (LAMBDA ,vars
                    (DECLARE (COMPILE))
                    ,(if (and (setq h (get class 'clos::closclass))
                              (typep h clos::<structure-class>)
                              (setq h (clos::class-kconstructor h)))
                       ;; h is the keyword constructor for the structure
                       `(,h ,@(mapcan #'(lambda (s v)
                                          (list (symbol-to-keyword s) v))
                                      slots vars))
                       ;; no keyword constructor found ->
                       ;; use CLOS:SLOT-VALUE instead
                       (let ((ivar (gensym)))
                         `(LET ((,ivar (CLOS:MAKE-INSTANCE ',class)))
                           ,@(mapcar #'(lambda (s v)
                                         `(SETF (CLOS:SLOT-VALUE ,ivar ', s)
                                           ,v))
                                     slots vars)
                           ,ivar)))))))))))

(defmacro with-name/options ((name options name+options) &body body)
  (multiple-value-bind (body-rest declarations) (system::parse-body body)
    (let ((no (gensym "NAME+OPTIONS-")))
      `(LET ((,no ,name+options))
         (MULTIPLE-VALUE-BIND (,name ,options)
             (IF (CONSP ,no) (VALUES (FIRST ,no) (REST ,no)) (VALUES ,no NIL))
           (DECLARE (IGNORABLE ,name ,options) ,@declarations)
           ,@body-rest)))))

(defmacro with-defining-c-type ((name c-type len) &body body)
  (multiple-value-bind (body-rest declarations) (system::parse-body body)
    `(LET ((,c-type (MAKE-ARRAY ,len)))
       ,@(if declarations `((DECLARE ,@declarations)))
       (UNWIND-PROTECT
           (PROGN
             (WHEN ,name
               (SETF (GETHASH ,name *C-TYPE-TABLE*) ,c-type))
             ,@body-rest)
         (WHEN ,name
           (SETF (GETHASH ,name *C-TYPE-TABLE*) NIL)))
       (WHEN ,name
         (SETF (GETHASH ,name *C-TYPE-TABLE*) ,c-type))
       ,c-type)))

;; Parse a C type specification.
;; If name is non-NIL, it will be assigned to name.
(defun parse-c-type (typespec &optional (name nil))
  (if (atom typespec)
    (if (symbolp typespec)
      (multiple-value-bind (c-type found) (gethash typespec *c-type-table*)
        (unless found
          (error (TEXT "Incomplete FFI type ~S is not allowed here.")
                 typespec))
        (when name (setf (gethash name *c-type-table*) c-type))
        c-type)
      (error (TEXT "FFI type should be a symbol, not ~S")
             typespec))
    (flet ((invalid (typespec)
             (error (TEXT "Invalid FFI type: ~S")
                    typespec))
           (dimp (dim) (typep dim '(integer 0 *))))
      (case (first typespec)
        (C-STRUCT
         (with-defining-c-type (name c-type (max 5 (+ 3 (length typespec))))
           (setf (svref c-type 0) (first typespec)) ; c-struct
           (with-name/options (name options (second typespec))
             (setf (svref c-type 1) name)
             (setf (svref c-type 2) options))
           (setf (svref c-type 3) (map 'vector #'first (cddr typespec)))
           (setf (svref c-type 4) ; constructor
                 (c-struct-constructor typespec))
           (setf (subseq c-type 5) (parse-components typespec))))
        (C-UNION
         (with-defining-c-type (name c-type (1+ (length typespec)))
           (setf (svref c-type 0) (first typespec))
           (setf (svref c-type 1) (map 'vector #'first (rest typespec)))
           (setf (subseq c-type 2) (parse-components typespec))))
        (C-ARRAY
          (unless (eql (length typespec) 3) (invalid typespec))
          (let ((dimensions (third typespec)))
            (unless (listp dimensions) (setq dimensions (list dimensions)))
            (unless (every #'dimp dimensions)
              (invalid typespec))
            (with-defining-c-type (name c-type (+ 2 (length dimensions)))
              (setf (svref c-type 0) 'C-ARRAY)
              (setf (svref c-type 1) (parse-c-type (second typespec)))
              (setf (subseq c-type 2) dimensions))))
        (C-ARRAY-MAX
          (unless (eql (length typespec) 3) (invalid typespec))
          (let ((maxdim (third typespec)))
            (unless (dimp maxdim) (invalid typespec))
            (with-defining-c-type (name c-type 3)
              (setf (svref c-type 0) 'C-ARRAY-MAX)
              (setf (svref c-type 1) (parse-c-type (second typespec)))
              (setf (svref c-type 2) maxdim))))
        (C-FUNCTION
         (let ((c-type
                 (parse-c-function
                   (parse-options (rest typespec)
                                  '(:arguments :return-type :language)
                                  typespec)
                   typespec)))
            (when name (setf (gethash name *c-type-table*) c-type))
            c-type))
        (C-PTR
         (unless (eql (length typespec) 2) (invalid typespec))
         (with-defining-c-type (name c-type 2)
           (setf (svref c-type 0) 'C-PTR)
           (setf (svref c-type 1) (parse-c-type (second typespec)))))
        (C-PTR-NULL
          (unless (eql (length typespec) 2) (invalid typespec))
          (with-defining-c-type (name c-type 2)
            (setf (svref c-type 0) 'C-PTR-NULL)
            (setf (svref c-type 1) (parse-c-type (second typespec)))))
        (C-POINTER
          (unless (eql (length typespec) 2) (invalid typespec))
          (with-defining-c-type (name c-type 2)
            (setf (svref c-type 0) 'C-POINTER)
            (setf (svref c-type 1) (parse-c-type (second typespec)))))
        (C-ARRAY-PTR
          (unless (eql (length typespec) 2) (invalid typespec))
          (with-defining-c-type (name c-type 2)
            (setf (svref c-type 0) 'C-ARRAY-PTR)
            (setf (svref c-type 1) (parse-c-type (second typespec)))))
        (t (invalid typespec))))))

;; Primitive types (symbols) can be safely inlined at compile-time.
;; Handle (parse-c-type 'uint8) as well as
;; (def-c-type PGconn c-pointer) (parse-c-type 'pgconn)
;; In addition, `(c-array uint8 ,(length foo))
;;  -> (vector 'c-array (parse-c-type 'uint8) (length foo))
;;  -> (vector 'c-array 'uint8 (length foo))
;; As a result, execution time is nearly halved.
(define-compiler-macro parse-c-type (&whole form typespec &optional name)
  (unless name
    (cond
      ((typep typespec '(CONS (EQL QUOTE) (CONS SYMBOL NULL)))
       (let* ((typespec (second typespec))
              (internal (gethash typespec *c-type-table* 0)))
         (when (symbolp internal)
           (return-from parse-c-type `',internal))))
      ;; Under Kaz Kylheku's backquote reader, `(c-array x ,l) yields
      ;;(SYSTEM::BACKQUOTE (C-ARRAY X (SYSTEM::UNQUOTE L)))
      ;; which is optimized because it is costly, yet a common pattern.
      ((typep typespec
              '(CONS (EQL SYS::BACKQUOTE)
                (CONS
                 (CONS (MEMBER C-ARRAY C-ARRAY-MAX)
                  (CONS ATOM ; do not match (SYSTEM::UNQUOTE #) here
                        (CONS (CONS (EQL SYS::UNQUOTE)
                                    (CONS * NULL)) NULL))) NULL)))
       (return-from parse-c-type
         (let ((typespec (second typespec)))
           `(VECTOR
             ',(first typespec)
             (PARSE-C-TYPE ',(second typespec))
             (EXT:ETHE UNSIGNED-BYTE ,(second (third typespec)))))))))
  form)

(defun parse-options (options keywords whole)
  (let ((alist '()))
    (dolist (option options)
      (unless (and (consp option) (member (first option) keywords))
        (error (TEXT "Invalid option in ~S: ~S")
               whole option))
      (when (assoc (first option) alist)
        (error (TEXT "Only one ~S option is allowed: ~S")
              (first option) whole))
      (push option alist))
    alist))

;; check whether C-TYPE is a C type spec and return the type
(defun ctype-type (c-type)
  (and (simple-vector-p c-type) (plusp (length c-type)) (svref c-type 0)))

;; check whether the flag is set in the variable
(defun flag-set-p (var flag) (not (zerop (logand var flag))))

(defun flag-to-language (flag)
  (append (if (flag-set-p flag ff-language-c) '(:C) '())
          (if (flag-set-p flag ff-language-ansi-c)
              (if (flag-set-p flag ff-language-stdcall)
                '(:STDC-STDCALL)
                '(:STDC))
              '())))

(defun language-to-flag (lang)
  (ecase lang
    (:C ff-language-c)
    (:STDC ff-language-ansi-c)
    (:STDC-STDCALL (+ ff-language-ansi-c ff-language-stdcall))))

;; the default foreign language
(defvar *foreign-language* nil) ; ABI

(defmacro default-foreign-language (lang)
  (language-to-flag lang)       ; error checking
  `(eval-when (load compile eval)
     (without-package-lock ("FFI") (setq *foreign-language* ',lang))))

;; get the even (start=0) or odd (start=1) elements of the simple vector
(defun split-c-fun-arglist (args start)
  (do ((ii start (+ ii 2)) (res '()))
      ((>= ii (length args)) (nreverse res))
    (push (svref args ii) res)))

(defun parse-c-function (alist whole)
  (vector
    'C-FUNCTION
    (parse-c-type (or (second (assoc ':return-type alist)) 'nil))
    (coerce (mapcap (lambda (argspec)
                      (unless (and (listp argspec)
                                   (symbolp (first argspec))
                                   (<= 2 (length argspec) #-AFFI 4 #+AFFI 5))
                        (error (TEXT "Invalid parameter specification in ~S: ~S")
                               whole argspec))
                      (let* ((argtype (parse-c-type (second argspec)))
                             (argmode (if (cddr argspec) (third argspec) ':IN))
                             (argalloc (if (cdddr argspec)
                                         (fourth argspec)
                                         (if (or (eq argtype 'C-STRING)
                                                 (case (ctype-type argtype) ((C-PTR C-PTR-NULL C-ARRAY-PTR) t))
                                                 (eq argmode ':OUT))
                                           ':ALLOCA
                                           ':NONE))))
                        ;; see FOREIGN-CALL-OUT in foreign.d
                        (when (and (or (eq argmode :OUT) (eq argmode :IN-OUT))
                                   (not (eq (ctype-type argtype) 'C-PTR)))
                          (warn (TEXT "~s argument ~s is not a pointer in ~s")
                                argmode argtype whole))
                        (list argtype
                              (+ (ecase argmode
                                   ((:IN :READ-ONLY) 0)
                                   ((:OUT :WRITE-ONLY) ff-flag-out)
                                   ((:IN-OUT :READ-WRITE) ff-flag-in-out))
                                 (ecase argalloc
                                   (:NONE 0)
                                   (:ALLOCA ff-flag-alloca)
                                   (:MALLOC-FREE ff-flag-malloc-free))
                                 #+AFFI
                                 (if (cddddr argspec)
                                   (ash (1+ (position (fifth argspec) *registers*)) 8)
                                   0)))))
                    (or (rest (assoc ':arguments alist)) '()))
            'simple-vector)
    (+ (let ((rettype (assoc ':return-type alist)))
         (if (cddr rettype)
           (ecase (third rettype)
             (:NONE 0)
             (:MALLOC-FREE ff-flag-malloc-free))
           0))
       (let ((languages (assoc ':language alist)))
         (if languages
           (reduce #'+ (rest languages) :key #'language-to-flag)
           (language-to-flag
            (or *foreign-language*
                (progn
                  (warn (TEXT "~s: No ~s argument and no ~s form in this compilation unit; ~s assumed now and for the rest of this unit")
                        whole :language 'default-foreign-language :stdc)
                  (setq *foreign-language* :STDC))))))))) ; Default is ANSI C

(defun parse-foreign-name (name)
  (unless (stringp name)
    (error (TEXT "The name must be a string, not ~S")
           name))
  (if (c-ident-p name)
    name
    (error (TEXT "The name ~S is not a valid C identifier")
           name)))

(defmacro DEF-C-TYPE (&whole whole-form name typespec)
  (setq name (check-symbol name (first whole-form)))
  `(EVAL-WHEN (LOAD COMPILE EVAL)
     (PARSE-C-TYPE ',typespec ',name)
     ',name))

;; Convert back a C type from internal (vector) to external (list)
;; representation. Both representations may be circular.
(defun deparse-c-type (ctype)
  (let ((alist '()))
    (labels ((new-type (ctype typespec)
               (setq alist (acons ctype typespec alist))
               ctype)
             (deparse-slot (slot slottype)
               (list slot (deparse slottype)))
             (deparse (ctype)
               (or (cdr (assoc ctype alist :test #'eq))
                   (if (symbolp ctype)
                     ;; <simple-c-type>, c-pointer, c-string
                     (new-type ctype ctype)
                     (let ((typespec (list (svref ctype 0))))
                       (new-type ctype typespec)
                       (setf (rest typespec) ; fill the rest
                             (ecase (svref ctype 0)
                               ;; #(c-struct name options slots
                               ;;            constructor <c-type>*)
                               (C-STRUCT
                                (let* ((constructor (svref ctype 4))
                                       (options (svref ctype 2))
                                       (name (cond ((eql constructor #'vector)
                                                    'vector)
                                                   ((eql constructor #'list)
                                                    'list)
                                                   (t (svref ctype 1)))))
                                  (cons (if options (cons name options) name)
                                        (map 'list #'deparse-slot
                                             (svref ctype 3)
                                             (subseq ctype 5)))))
                               ;; #(c-union alternatives <c-type>*)
                               (C-UNION
                                (map 'list #'deparse-slot
                                     (svref ctype 1) (subseq ctype 2)))
                               ;; #(c-array <c-type> number*)
                               (C-ARRAY
                                (list (deparse (svref ctype 1))
                                      (let ((dimensions (subseq ctype 2)))
                                        (if (eql (length dimensions) 1)
                                          (elt dimensions 0)
                                          (coerce dimensions 'list)))))
                               ;; #(c-array-max <c-type> number)
                               (C-ARRAY-MAX
                                (list (deparse (svref ctype 1))
                                      (svref ctype 2)))
                               ;; #(c-function <c-type> #({<c-type> flags}*)
                               ;;               flags)
                               (C-FUNCTION
                                (list (list ':arguments
                                            (do ((args (coerce (svref ctype 2) 'list) (cddr args))
                                                 (i 1 (+ i 1))
                                                 (argspecs '()))
                                                ((null args) (nreverse argspecs))
                                              (let ((argtype (first args))
                                                    (argflags (second args)))
                                                (push `(,(intern (format nil "arg~D" i) compiler::*keyword-package*)
                                                        ,(deparse argtype)
                                                        ,(cond ((flag-set-p argflags ff-flag-out) ':OUT)
                                                               ((flag-set-p argflags ff-flag-in-out) ':IN-OUT)
                                                               (t ':IN))
                                                        ,(cond ((flag-set-p argflags ff-flag-alloca) ':ALLOCA)
                                                               ((flag-set-p argflags ff-flag-malloc-free) ':MALLOC-FREE)
                                                               (t ':NONE))
                                                        #+AFFI
                                                        ,@(let ((h (logand (ash argflags -8) #xF)))
                                                            (if (not (zerop h))
                                                              (list (svref *registers* (- h 1)))
                                                              '())))
                                                      argspecs))))
                                      (list ':return-type
                                            (deparse (svref ctype 1))
                                            (if (flag-set-p (svref ctype 3) ff-flag-malloc-free) ':MALLOC-FREE ':NONE))
                                      (cons ':language
                                            (flag-to-language (svref ctype 3)))))
                               ;; #(c-ptr <c-type>), #(c-ptr-null <c-type>)
                               ;; #(c-array-ptr <c-type>), #(c-pointer <c-type>)
                               ((C-PTR C-PTR-NULL C-POINTER C-ARRAY-PTR)
                                (list (deparse (svref ctype 1))))))
                       typespec)))))
      (deparse ctype))))

;; ============================ module ============================

; Data belonging to the FFI module being compiled:
(defvar *ffi-module* nil)

; We put everything into a structure, so that COMPILE-FILE needs to bind only
; a single variable at compile time.
(defstruct ffi-module
  name
  c-name
  (object-table (make-hash-table :key-type '(or string symbol) :value-type '(cons string fixnum)
                                 :test 'stablehash-equal :warn-if-needs-rehash-after-gc t))
  (type-table (make-hash-table :key-type 'symbol :value-type '(or null string)
                               :test 'stablehash-eq :warn-if-needs-rehash-after-gc t))
  (init-once '())
  (init-always '())
  (fini '())
  ;; type -> (function-name . #(const1 const2 const3 ...))
  (constant-table (make-hash-table :test 'stablehash-eq
                                   :key-type 'symbol :value-type 'string
                                   :warn-if-needs-rehash-after-gc t))
  (variable-list '())
  (function-list '()))
(define-symbol-macro *name*
    (ffi-module-name *ffi-module*))
(define-symbol-macro *c-name*
    (ffi-module-c-name *ffi-module*))
(define-symbol-macro *object-table*
    (ffi-module-object-table *ffi-module*))
(define-symbol-macro *type-table*
    (ffi-module-type-table *ffi-module*))
(define-symbol-macro *init-once*
    (ffi-module-init-once *ffi-module*))
(define-symbol-macro *init-always*
    (ffi-module-init-always *ffi-module*))
(define-symbol-macro *fini*
    (ffi-module-fini *ffi-module*))
(define-symbol-macro *constant-table*
    (ffi-module-constant-table *ffi-module*))
(define-symbol-macro *variable-list*
    (ffi-module-variable-list *ffi-module*))
(define-symbol-macro *function-list*
    (ffi-module-function-list *ffi-module*))

; Convert a file name to a C module name.
; This must agree with some sed command in clisp-link.in.
(defun to-module-name (name)
  (map 'string #'(lambda (c)
                   (if (or (char<= #\A c #\Z) (char<= #\a c #\z) (char<= #\0 c #\9) (char= c #\_))
                     c
                     #\_
                 ) )
       name
) )

; Convert a Lisp name to a C name.
; (Doesn't really matter how. This must just be a deterministic function.)
(defun to-c-name (name)
  (setq name (if (and (symbolp name)
                      (symbol-package name)
                      (package-case-inverted-p (symbol-package name)))
               (cs-cl:string name)
               (let ((nm (string name)))
                 (if (some #'lower-case-p nm)
                     nm (string-downcase name)))))
  (if (c-ident-p name)
    name
    (with-output-to-string (s)
      (format s "_lisp__")
      (map nil
           #'(lambda (ch)
               (if (and (standard-char-p ch) (alphanumericp ch))
                 (write-char ch s)
                 (format s "_~2X" (char-code ch))))
           name))))

; Prepare the conversion of a C type to its C representation.
; Calling this will generate a "typedef" declaration for some C types.
; This is needed if you want to call `to-c-typedecl' more than once on
; the same type.
; This must be called before `to-c-typedecl', at a point where global
; declarations in the *coutput-stream* are acceptable.
(defun prepare-c-typedecl (c-type)
  (unless (gethash c-type *type-table*)
    (case (ctype-type c-type)
      ((C-STRUCT C-UNION C-ARRAY C-ARRAY-MAX)
       (let ((new-typename (symbol-name (gensym "g"))))
         (format *coutput-stream* "~%typedef ~A;~%"
                 (to-c-typedecl c-type new-typename))
         (setf (gethash c-type *type-table*) new-typename))))))

; Convert a C type to its C representation.
(defun to-c-typedecl (c-type name)
  (case c-type
    ((nil) (format nil "void ~A" name))
    ((boolean int) (format nil "int ~A" name))
    (character (format nil "char ~A" name))
    ((char sint8) (format nil "sint8 ~A" name))
    ((uchar uint8) (format nil "uint8 ~A" name))
    ((short sint16) (format nil "sint16 ~A" name))
    ((ushort uint16) (format nil "uint16 ~A" name))
    (uint (format nil "unsigned int ~A" name))
    (long (format nil "long ~A" name))
    (ulong (format nil "unsigned long ~A" name))
    (sint32 (format nil "sint32 ~A" name))
    (uint32 (format nil "uint32 ~A" name))
    (sint64 (format nil "sint64 ~A" name))
    (uint64 (format nil "uint64 ~A" name))
    (single-float (format nil "float ~A" name))
    (double-float (format nil "double ~A" name))
    (c-string (format nil "char* ~A" name))
    (c-pointer (format nil "void* ~A" name))
    (t (if (gethash c-type *type-table*)
         (format nil "~A ~A" (gethash c-type *type-table*) name)
         (macrolet ((with-to-c ((class typename &key (tname '(gensym "t")))
                                &body body)
                      `(let ((,typename (format nil "~A ~A" ,class ,tname)))
                         (unwind-protect
                              (progn (setf (gethash c-type *type-table*)
                                           ,typename)
                                     ,@body)
                           (setf (gethash c-type *type-table*) nil)))))
           (case (ctype-type c-type)
             (c-struct
              (cond ((sys::memq :typedef (svref c-type 2))
                     (format nil "~A ~A" (svref c-type 1) name))
                    ((sys::memq :external (svref c-type 2))
                     (format nil "struct ~A ~A" (svref c-type 1) name))
                    (t (with-to-c ("struct" type :tname (svref c-type 1))
                         (format nil "~a { ~{~A; ~}} ~A"
                                 type (map 'list #'to-c-typedecl
                                           (subseq c-type 5)
                                           (svref c-type 3))
                                 name)))))
             (c-union
              (with-to-c ("union" type)
                (format nil "~A { ~{~A; ~}} ~A"
                        type (map 'list #'to-c-typedecl
                                  (cddr (coerce c-type 'list))
                                  (svref c-type 1))
                        name)))
             (c-array
              (to-c-typedecl (svref c-type 1)
                             (format nil "(~A)~{[~D]~}" name
                                     (cddr (coerce c-type 'list)))))
             (c-array-max
              (to-c-typedecl (svref c-type 1)
                             (format nil "(~A)[~D]" name (svref c-type 2))))
             ((c-ptr c-ptr-null c-pointer c-array-ptr)
              (to-c-typedecl (svref c-type 1) (format nil "* ~A" name)))
             (c-function
              (to-c-typedecl (svref c-type 1)
                             (format nil "(~A) (~{~A~^,~})" name
                                     (mapcar (lambda (c-t)
                                               (to-c-typedecl
                                                c-t (gensym "arg")))
                                             (split-c-fun-arglist
                                              (svref c-type 2) 0)))))
             (t (error (TEXT "illegal foreign data type ~S")
                       c-type))))))))

(defvar *output-c-functions* nil)
(defvar *output-c-variables* nil)
(defvar *foreign-guard* nil)

(defun prepare-module ()
  (unless *ffi-module*
    (setq *ffi-module*
          (let ((module-name (pathname-name *coutput-file*)))
            (make-ffi-module :name module-name
                             :c-name (to-module-name module-name))))
    (format *coutput-stream* "extern object module__~A__object_tab[];~%"
            *c-name*)))
(defun finalize-coutput-file ()
  (when *ffi-module*
    (format *coutput-stream* "~%subr_t module__~A__subr_tab[1];~%~
            uintC module__~A__subr_tab_size = 0;~%~
            subr_initdata_t module__~A__subr_tab_initdata[1];~2%"
            *c-name* *c-name* *c-name*)
    (let ((count (hash-table-count *object-table*)))
      (if (zerop count)
        (format *coutput-stream* "object module__~A__object_tab[1];~%~
                object_initdata_t module__~A__object_tab_initdata[1];~%"
                *c-name* *c-name*)
        (let ((v (make-array count)))
          (format *coutput-stream* "object module__~A__object_tab[~D];~%~
                  object_initdata_t module__~A__object_tab_initdata[~D] = {~%"
                  *c-name* count *c-name* count)
          (dohash (key value *object-table*)
            (declare (ignore key))
            (setf (svref v (cdr value)) (car value)))
          (map nil #'(lambda (initstring)
                       (format *coutput-stream* "  { ~A },~%"
                               (to-c-string initstring)))
               v)
          (format *coutput-stream* "};~%")))
      (format *coutput-stream* "uintC module__~A__object_tab_size = ~D;~%"
              *c-name* count))
    (format *coutput-stream* "~%")
    (maphash (lambda (type fun-vec)
               (let* ((fun (car fun-vec)) (vec (cdr fun-vec))
                      (c-decl (to-c-typedecl type fun)))
                 (format *coutput-stream* "~A (int number, int *definedp);~%~
                                           ~A (int number, int *definedp) {
  *definedp=1;~%  switch (number) {~%"
                         c-decl c-decl)
                 (dotimes (num (length vec))
                   (let ((const (aref vec num)))
                     (format *coutput-stream* "#  if defined(~A)
    case ~D: return ~A;~%#  endif~%"
                             const num const)))
                 (format *coutput-stream* "    default: *definedp=0; return 0;
  }~%}~%#define HAVE_~A~%" (string-upcase (symbol-name fun)))))
             *constant-table*)
    (setq *variable-list*
          (nreverse (delete-duplicates
                     *variable-list* :key #'first :test #'equal)))
    (when *output-c-variables*
      (dolist (variable *variable-list*)
        ;;(prepare-c-typedecl (second variable))
        (format *coutput-stream* "extern ~A;~%"
                (to-c-typedecl (second variable) (first variable)))))
    (setq *function-list*
          (nreverse (delete-duplicates
                     *function-list* :key #'first :test #'equal)))
    (when *output-c-functions*
      (dolist (function *function-list*)
        ;;(prepare-c-typedecl (svref (second function) 1))
        (format *coutput-stream* "extern ~A"
                (to-c-typedecl (svref (second function) 1)
                               (format nil "(~A)(" (first function))))
        (when (third function)    ; built-in, requires full arglist
          (do* ((parameters (svref (second function) 2))
                (length (length parameters)) (i 0 (+ 2 i))
                (parameter (svref parameters i)))
               ((>= i length))
            (unless (zerop i)
              (write-string ", " *coutput-stream*))
            (write-string (to-c-typedecl parameter "")
                          *coutput-stream*)))
        (format *coutput-stream* ");~%")))
    (format *coutput-stream*
            "~%void module__~A__init_function_1 (module_t* module);~%~
            ~%void module__~A__init_function_2 (module_t* module);~%~
            ~%void module__~A__fini_function (module_t* module);~%~
            ~2%void module__~A__init_function_1 (module_t* module)~%~
            {~{~%~A~}~%}~2%~
            void module__~A__init_function_2 (module_t* module)~%~
            {~{~%~A~}~%"
            *c-name* *c-name* *c-name*
            *c-name* *init-once* *c-name* *init-always*)
    (dolist (variable *variable-list*)
      (let ((c-name (first variable)))
        (when *foreign-guard*
          (format *coutput-stream* "# if HAVE_DECL_~A~%"
                  (string-upcase c-name)))
        (format *coutput-stream*
                "  register_foreign_variable((void*)&~A,~A,~D,sizeof(~A));~%"
                c-name (to-c-string c-name) (third variable) (first variable))
        (when *foreign-guard* (format *coutput-stream* "# endif~%"))))
    (dolist (function *function-list*)
      (let ((c-name (first function)))
        (when *foreign-guard*
          (format *coutput-stream* "# if defined(HAVE_~A)~%"
                  (string-upcase c-name)))
        (format *coutput-stream*
                "  register_foreign_function((void*)&~A,~A,~D);~%"
                c-name (to-c-string c-name) (svref (second function) 3))
        (when *foreign-guard* (format *coutput-stream* "# endif~%"))))
    (format *coutput-stream*
            "}~2%void module__~A__fini_function (module_t* module)~%~
            {~{~%~A~}~%}~%"
            *c-name* *fini*)))

; Allocate a new object in the module's object_tab.
(defun new-object (read-only-p initstring)
  (when read-only-p
    (let ((h (gethash initstring *object-table*)))
      (when h
        (return-from new-object (cdr h))))) ; no need to allocate a new one
  (let ((index (hash-table-count *object-table*)))
    (setf (gethash (if read-only-p initstring (gensym)) *object-table*)
          (cons initstring index))
    index))

; Pass an object from the compilation environment to the module.
(defun pass-object (object)
  (new-object t
              (let ((*package* compiler::*keyword-package*))
                (write-to-string object :readably t :pretty nil))))

; Convert an object's index to a C lvalue.
(defun object-to-c-value (index)
  (format nil "module__~A__object_tab[~D]" *c-name* index))

; Output some C text literally.
(defmacro C-LINES (format-string &rest args)
  `(EVAL-WHEN (COMPILE)
     (DO-C-LINES ,format-string ,@args)))
(defun do-c-lines (format-string &rest args) ; ABI
  (when (compiler::prepare-coutput-file)
    (prepare-module)
    (etypecase format-string
      ((or string function)
       (apply #'format *coutput-stream* format-string args))
      ((member :init-always :init-once :fini)
       (let ((code (apply #'format nil args)))
         (ecase format-string
           (:init-always (push code *init-always*))
           (:init-once (push code *init-once*))
           (:fini (push code *fini*))))))))

;; ============================ named C variables ============================

(defun foreign-name (lisp-name name-option)
  (if name-option
    (parse-foreign-name (second name-option))
    (to-c-name lisp-name)))

;; CPP consts (#define'd in an *.h file):
;; for each type (int, string, pointer) there is a C function (and a
;;  foreign-function created when the first constant of this type is
;;  encountered) mapping a number (assigned at _compile_ time) into the value
;; when loading a compiled file, the C function has already been writted
;;  into the C file and compiled, so the numbers have to be pre-assigned
(defmacro DEF-C-CONST (&whole whole-form name &rest options)
  (setq name (check-symbol name (first whole-form)))
  (prepare-module)
  (let* ((alist (parse-options options '(:name :type :documentation)
                               whole-form))
         (doc (cdr (assoc ':documentation alist))) ; ("doc string") or NIL
         (c-type (or (second (assoc ':type alist)) 'ffi:int))
         (c-name (foreign-name name (assoc ':name alist)))
         f-name c-number)
    (check-type c-type (member ffi:int ffi:c-string ffi:c-pointer)
                "A constant must be either an integer, a string or a pointer")
    (setq f-name (intern
                  (format nil "module__~A__constant_map_~A" *name*
                          (nstring-downcase
                           (nsubstitute #\_ #\-
                                        (copy-seq (symbol-name c-type))))))
          c-number
          (vector-push-extend
           c-name (cdr (or (gethash c-type *constant-table*)
                           (setf (gethash c-type *constant-table*)
                                 (cons f-name (make-array 10 :adjustable t
                                                          :fill-pointer 0)))))))
    `(progn
       ;; c-number == 0 ==> need to output the def-call-out form
       ,@(when (zerop c-number)
           `((ffi:def-call-out ,f-name
                 (:arguments (number ffi:int)
                             (defined-p (ffi:c-ptr ffi:int) :out))
               (:return-type ,c-type))))
       (defconstant ,name (c-const-value ',f-name ,c-number ',name ',c-name)
         ,@doc))))

(defun c-const-value (f-name c-number name c-name) ; ABI
  (multiple-value-bind (value value-p) (funcall f-name c-number)
    (if value-p value
        (progn
          (warn (TEXT "~S(~S): CPP constant ~A is not defined")
                'def-c-const name c-name)
          (sys::%unbound)))))

(defmacro DEF-C-VAR (&whole whole-form
                     name &rest options)
  (setq name (check-symbol name (first whole-form)))
  (let* ((alist (parse-options options '(:name :type :read-only :alloc
                                         :library :documentation)
                               whole-form))
         (doc (assoc ':documentation alist))
         (c-name (foreign-name name (assoc ':name alist)))
         (type (second (or (assoc ':type alist)
                           (sys::error-of-type 'ext:source-program-error
                             :form whole-form
                             :detail whole-form
                             (TEXT "~S: ~S option missing in ~S")
                             'def-c-var ':type whole-form))))
         (read-only (second (assoc ':read-only alist)))
         (flags (+ (if read-only fv-flag-readonly 0)
                   (let ((alloc (assoc ':alloc alist)))
                     (if (cdr alloc)
                       (ecase (second alloc)
                         (:NONE 0)
                         (:MALLOC-FREE fv-flag-malloc-free))
                       0))))
         (library  (second (assoc :library alist)))
         #|
         (getter-function-name (sys::symbol-suffix name "%GETTER%"))
         (setter-function-name (sys::symbol-suffix name "%SETTER%"))
         |#
         (def (gensym "DEF-C-VAR-")))
    `(LET ((,def (LOAD-TIME-VALUE
                  ,(if library
                       `(FFI::FOREIGN-LIBRARY-VARIABLE
                         ',c-name (FFI::FOREIGN-LIBRARY ,library)
                         nil (PARSE-C-TYPE ',type))
                       `(LOOKUP-FOREIGN-VARIABLE
                         ',c-name (PARSE-C-TYPE ',type))))))
       #|
       (LET ((FVAR (LOOKUP-FOREIGN-VARIABLE ',c-name (PARSE-C-TYPE ',type))))
         (DEFUN ,getter-function-name () (FOREIGN-VALUE FVAR))
         ; Install a setter even if the variable is read-only.
         ; When called, it will print a comprehensible error message.
         (DEFUN ,setter-function-name (VALUE) (SET-FOREIGN-VALUE FVAR VALUE))
       )
       (DEFSETF ,getter-function-name ,setter-function-name)
       (DEFINE-SYMBOL-MACRO ,name (,getter-function-name))
       |#
       ,(unless library
          `(EVAL-WHEN (COMPILE) (NOTE-C-VAR ',c-name ',type ',flags)))
       (when ,def
         (SYSTEM::%PUT ',name 'FOREIGN-VARIABLE ,def)
         ,@(when doc `((SETF (DOCUMENTATION ',name 'VARIABLE) ',(second doc))))
         (DEFINE-SYMBOL-MACRO ,name
             (FOREIGN-VALUE (LOAD-TIME-VALUE (GET ',name 'FOREIGN-VARIABLE)))))
       ',name)))

(defun note-c-var (c-name type flags) ; ABI
  (when (compiler::prepare-coutput-file)
    (prepare-module)
    (push (list c-name (parse-c-type type) flags) *variable-list*)))

(defsetf foreign-value set-foreign-value)
;(defsetf foreign-pointer set-foreign-pointer) ; no, incompatible with SETF
(defsetf validp set-validp)

(defmacro with-c-place ((var fvar) &body body)
  (let ((fv (gensym (symbol-name var))))
    `(LET ((,fv ,fvar))
       (SYMBOL-MACROLET ((,var (FOREIGN-VALUE ,fv)))
         ,@body))))

;; ============================ Stack allocation ============================

;; Allocate arbitrarily complex structure on the stack,
;; but allocate only (sizeof c-type) bytes when called without initarg!
;; C-ARRAY-MAX, "" and #() are thus your friends for creation of empty arrays.
;; (with-c-var (v '(c-ptr  (c-array     uint8 32)))     (cast v 'c-pointer))
;;                -> null-pointer, 4 bytes
;; (with-c-var (v '(c-ptr  (c-array-max uint8 32)) #()) (cast v 'c-pointer))
;;                -> non-null,  4+32 bytes
;; (with-c-var (v '(c-ptr-null (c-array uint8 32)) nil) (cast v 'c-pointer))
;;                -> null-pointer, 4 bytes

;; c-type is evaluated. This is particularly useful for variable sized arrays
;; using: `(c-array uint8 ,(length foo))
(defmacro with-foreign-object ((var c-type &optional (init nil init-p))
                               &body body)
  `(EXEC-ON-STACK
     #'(LAMBDA (,var) ,@body)
     (PARSE-C-TYPE ,c-type)
     ,@(if init-p `(,init))))

;; symbol-macro based interface (like DEF-C-VAR)
;; WITH-C-VAR appears as a composition of WITH-FOREIGN-OBJECT and WITH-C-PLACE
(defmacro with-c-var ((var c-type &optional (init nil init-p)) &body body)
  (let ((fv (gensym (symbol-name var))))
    `(EXEC-ON-STACK
       #'(LAMBDA (,fv)
           (SYMBOL-MACROLET ((,var (FOREIGN-VALUE ,fv)))
             ,@body))
       (PARSE-C-TYPE ,c-type)
       ,@(if init-p `(,init)))))

(defun exec-with-foreign-string (thunk string ; ABI
                                 &key (encoding #+UNICODE custom:*foreign-encoding*
                                                #-UNICODE custom:*default-file-encoding*)
                                      (null-terminated-p t) (start 0) (end nil))
  (call-with-foreign-string thunk encoding string start end
                            (if null-terminated-p
                                (sys::encoding-zeroes encoding)
                                0)))

(defmacro with-foreign-string ((foreign-variable char-count byte-count string
                                &rest keywords
                                &key encoding null-terminated-p start end)
                               &body body)
  (declare (ignore encoding null-terminated-p start end)) ; get them via keywords
  `(EXEC-WITH-FOREIGN-STRING
     #'(LAMBDA (,foreign-variable ,char-count ,byte-count) ,@body)
     ,string ,@keywords))

;; ============================ heap allocation ============================

(defmacro allocate-deep (ffi-type initval &rest keywords &key count read-only)
  (declare (ignore count read-only)) ; to be accessed via keywords
  `(foreign-allocate (parse-c-type ,ffi-type)
                     :initial-contents ,initval
                     ,@keywords))

(defmacro allocate-shallow (ffi-type &rest keywords &key count read-only)
  (declare (ignore count read-only)) ; to be accessed via keywords
  `(foreign-allocate (parse-c-type ,ffi-type) ,@keywords))

;; =========================== low-level interface ===========================

(sys::def-setf-alias memory-as write-memory-as) ; ABI

;; ============================ named C functions ============================

(defmacro DEF-C-CALL-OUT (name &rest options)
  (warn (TEXT "~s is deprecated, use ~s instead")
        'def-c-call-out 'def-call-out)
  `(DEF-CALL-OUT ,name ,@options (:LANGUAGE :STDC)))

(defmacro DEF-CALL-OUT (&whole whole-form name &rest options)
  (setq name (check-symbol name (first whole-form)))
  (let* ((alist
          (parse-options options '(:name :arguments :return-type :language
                                   :built-in :library :documentation)
                         whole-form))
         (def (gensym "DEF-CALL-OUT-"))
         (doc (assoc ':documentation alist))
         (parsed-function (parse-c-function alist whole-form))
         (signature (argvector-to-signature (svref parsed-function 2)))
         (library (second (assoc :library alist)))
         (c-name (foreign-name name (assoc :name alist))))
    (setq alist (remove-if (lambda (el) (sys::memq (car el) '(:name :library)))
                           alist))
    `(LET ((,def ,(if library
                      `(FFI::FOREIGN-LIBRARY-FUNCTION
                        ',c-name (FFI::FOREIGN-LIBRARY ,library)
                        nil ,parsed-function)
                      `(LOOKUP-FOREIGN-FUNCTION ',c-name ,parsed-function))))
       ,(unless library
          `(EVAL-WHEN (COMPILE) (NOTE-C-FUN ',c-name ',alist ',whole-form)))
       (when ,def
         (SYSTEM::REMOVE-OLD-DEFINITIONS ',name)
         (COMPILER::EVAL-WHEN-COMPILE (COMPILER::C-DEFUN ',name ',signature))
         ,@(when doc `((SETF (DOCUMENTATION ',name 'FUNCTION) ',(second doc))))
         (SYSTEM::%PUTD ',name ,def))
       ',name)))

(defun note-c-fun (c-name alist whole) ; ABI
  (when (compiler::prepare-coutput-file)
    (prepare-module)
    (push (list c-name (parse-c-function alist whole)
                (cadr (assoc :built-in alist)))
          *function-list*)))

#+AFFI
(defmacro DEF-LIB-CALL-OUT (&whole whole-form name library &rest options)
  (setq name (check-symbol name (first whole-form)))
  (let* ((alist (parse-options options
                               '(:name :offset :arguments :return-type)
                               whole-form))
         (parsed-function
           (parse-c-function (remove (assoc ':name alist) alist) whole-form))
         (signature (argvector-to-signature (svref parsed-function 2)))
         (c-name (foreign-name name (assoc ':name alist)))
         (offset (second (assoc ':offset alist))))
    `(LET ()
       (SYSTEM::REMOVE-OLD-DEFINITIONS ',name)
       (COMPILER::EVAL-WHEN-COMPILE (COMPILER::C-DEFUN ',name ',signature))
       (SYSTEM::%PUTD ',name
         (FOREIGN-LIBRARY-FUNCTION ',c-name
           (FOREIGN-LIBRARY ',library)
           ',offset
           (PARSE-C-FUNCTION ',(remove (assoc ':name alist) alist) ',whole-form)))
       ',name)))

(defmacro DEF-C-CALL-IN (name &rest options)
  (warn (TEXT "~s is deprecated, use ~s instead")
        'def-c-call-in 'def-call-in)
  `(DEF-CALL-IN ,name ,@options (:LANGUAGE :STDC)))

(defmacro DEF-CALL-IN (&whole whole-form name &rest options)
  (setq name (check-symbol name (first whole-form)))
  (let* ((alist (parse-options options
                               '(:name :arguments :return-type :language)
                               whole-form))
         (c-name (foreign-name name (assoc ':name alist))))
    (setq alist (remove (assoc ':name alist) alist))
    `(PROGN
       (EVAL-WHEN (COMPILE)
         (NOTE-C-CALL-IN ',name ',c-name ',alist ',whole-form))
       ',name)))

(defun convert-to-foreign-C (flags)
  (if (flag-set-p flags ff-flag-malloc-free)
      "convert_to_foreign_mallocing"
      "convert_to_foreign_nomalloc"))

(defun note-c-call-in (name c-name alist whole) ; ABI
  (when (compiler::prepare-coutput-file)
    (prepare-module)
    (let* ((fvd (parse-c-function alist whole))
           (rettype (svref fvd 1))
           (args (svref fvd 2))
           (flags (svref fvd 3))
           (argtypes (split-c-fun-arglist args 0))
           (argflags (split-c-fun-arglist args 1))
           (argnames (mapcar #'(lambda (argtype) (declare (ignore argtype))
                                 (symbol-name (gensym "g")))
                             argtypes))
           (par-list (mapcar #'to-c-typedecl argtypes argnames))
           (par-string (format nil "(~{~A~^, ~})" (or par-list '("void"))))
           (fun+type (to-c-typedecl rettype (format nil "(~A)" c-name))))
      (prepare-c-typedecl rettype)
      ;(mapc #'prepare-c-typedecl argtypes)
      ;; prototype + start
      (format *coutput-stream* "~%~A ~A;~%~A " fun+type par-string fun+type)
      (if (flag-set-p flags ff-language-ansi-c)
        ;; ANSI C parameter declarations
        (format *coutput-stream* "~A" par-string)
        ;; K&R C parameter declarations
        (progn
          (format *coutput-stream* "(")
          (do ((argnamesr argnames (cdr argnamesr)))
              ((null argnamesr))
            (format *coutput-stream* "~A" (car argnamesr))
            (when (cdr argnamesr) (format *coutput-stream* ", ")))
          (format *coutput-stream* ")")
          (dolist (par par-list)
            (format *coutput-stream* "~%  ~A;" par))))
      (format *coutput-stream* "~%{~%  begin_callback();~%")
      (let ((inargcount 0) (outargcount (if (eq rettype 'NIL) 0 1))
            (flag-output (logior ff-flag-out ff-flag-in-out)))
        (mapc #'(lambda (argtype argflag argname)
                  (unless (flag-set-p argflag ff-flag-out)
                    (format *coutput-stream*
                            "  pushSTACK(convert_from_foreign(~A,&~A));~%"
                            (object-to-c-value (pass-object argtype)) argname)
                    (incf inargcount))
                  (when (flag-set-p argflag flag-output)
                    (incf outargcount)))
              argtypes argflags argnames)
        (format *coutput-stream* "  funcall(~A,~D);~%"
                (object-to-c-value (pass-object name)) inargcount)
        (unless (eq rettype 'NIL)
          (format *coutput-stream* " {~%  var ~A;~%~:
  ~A(~A,value1,&retval);~%"
                  (to-c-typedecl rettype "retval")
                  (convert-to-foreign-C flags)
                  (object-to-c-value (pass-object rettype))))
        (let ((outargcount (if (eq rettype 'NIL) 0 1)))
          (mapc #'(lambda (argtype argflag argname)
                    (when (flag-set-p argflag flag-output)
                      (unless (eq (ctype-type argtype) 'C-PTR)
                        (error (TEXT "~S: :OUT argument is not a pointer: ~S")
                               'DEF-CALL-IN argtype))
                      (format *coutput-stream* "  ~A~A(~A,~A,~A);~%"
                              (if (eql outargcount 0) ""
                                (format nil "if (mv_count >= ~D) "
                                        (+ outargcount 1)))
                              (convert-to-foreign-C argflag)
                              (object-to-c-value
                                (pass-object (svref argtype 1)))
                              (if (eql outargcount 0)
                                "value1"
                                (format nil "mv_space[~D]" outargcount))
                              argname)
                      (incf outargcount)))
                argtypes argflags argnames))
        (format *coutput-stream* "  end_callback();~%")
        (unless (eq rettype 'NIL)
          (format *coutput-stream* "  return retval;~% }~%")))
      (format *coutput-stream* "}~%"))))

;; ===========================================================================

(defun argvector-to-signature (argvector)
  (sys::make-signature :req-num (count-inarguments argvector)))

(defun count-inarguments (arg-vector)
  (do* ((l (length arg-vector))
        (inargcount 0)
        (i 1 (+ i 2)))
       ((>= i l)
        inargcount)
    (unless (flag-set-p ff-flag-out (svref arg-vector i))
      (incf inargcount))))

; Called by SYS::FUNCTION-SIGNATURE.
(defun foreign-function-in-arg-count (obj)
  (count-inarguments (sys::%record-ref obj 3)))

(defmacro def-c-enum (&whole whole-form name &rest items)
  (setq name (check-symbol name (first whole-form)))
  (let ((forms '()) (ht (make-hash-table :key-type 'fixnum :value-type 'symbol))
        (next-value 0) (this-val 0))
    (dolist (item items)
      (when (consp item)
        (when (rest item)
          (setq next-value (second item)
                this-val (second item)))
        (setq item (first item)))
      (push `(DEFCONSTANT ,item ,next-value) forms)
      (when (gethash this-val ht)
        (warn (TEXT "~S (~S): value ~S will be assigned to both ~S and ~S")
              'def-c-enum name this-val (gethash this-val ht) item))
      (setf (gethash this-val ht) item)
      (setq next-value `(1+ ,item) this-val (1+ this-val)))
    `(PROGN ,@(nreverse forms) (setf (get ',name 'def-c-enum) ,ht)
            (def-c-type ,name int))))

(defun enum-table (enum)
  (or (get enum 'def-c-enum)
      (error (TEXT "~S does not name a C enum type") enum)))
(defun enum-to-value (enum symbol)
  (let ((val (dohash (k v (enum-table enum)
                      (error (TEXT "~S is not of C enum type ~S") symbol enum))
               (when (eq v symbol) (return k)))))
    (unless (= val (symbol-value symbol))
      (error
       (TEXT "~S symbol value (~S) does not match its table value (~S) in ~S")
       symbol (symbol-value symbol) val enum))
    val))
(defun enum-from-value (enum value)
  (or (gethash value (enum-table enum))
      (error (TEXT "~S is not a valid value of type ~S") value enum)))

(defmacro def-c-struct (name+options &rest slots)
  (with-name/options (name options name+options)
    `(PROGN
       (DEFSTRUCT ,name ,@(mapcar #'first slots))
       (DEF-C-TYPE ,name (C-STRUCT ,name+options ,@slots)))))

;; In order for ELEMENT, DEREF, SLOT to be SETFable, I make them macros.
;; (element (foreign-value x) ...) --> (foreign-value (%element x ...))
;; (deref (foreign-value x))       --> (foreign-value (%deref x))
;; (slot (foreign-value x) ...)    --> (foreign-value (%slot x ...))
;; (cast (foreign-value x) ...)    --> (foreign-value (%cast x ...))
;; (offset (foreign-value x) ...)  --> (foreign-value (%offset x ...))
(flet ((err (whole-form reconsed-form)
         (sys::error-of-type 'ext:source-program-error
           :form whole-form
           :detail reconsed-form
           (TEXT "~S is only allowed after ~S: ~S")
           (first whole-form) 'FOREIGN-VALUE whole-form))
       (foreign-place-p (place type)
         (and (consp place) (eq (first place) type) (eql (length place) 2))))

  (defmacro element (&whole whole-form
                     place &rest indices &environment env)
    (setq place (macroexpand place env))
    (if (foreign-place-p place 'FOREIGN-VALUE)
      `(FOREIGN-VALUE (%ELEMENT ,(second place) ,@indices))
      (err whole-form `(element ,place ,@indices))))

  (defmacro deref (&whole whole-form
                   place &environment env)
    (setq place (macroexpand place env))
    (if (foreign-place-p place 'FOREIGN-VALUE)
      `(FOREIGN-VALUE (%DEREF ,(second place)))
      (err whole-form `(deref ,place))))

  (defmacro slot (&whole whole-form
                  place slotname &environment env)
    (setq place (macroexpand place env))
    (if (foreign-place-p place 'FOREIGN-VALUE)
      `(FOREIGN-VALUE (%SLOT ,(second place) ,slotname))
      (err whole-form `(slot ,place ,slotname))))

  (defmacro cast (&whole whole-form
                  place type &environment env)
    (setq place (macroexpand place env))
    (if (foreign-place-p place 'FOREIGN-VALUE)
      `(FOREIGN-VALUE (%CAST ,(second place) (PARSE-C-TYPE ,type)))
      (err whole-form `(cast ,place ,type))))

  (defmacro offset (&whole whole-form
                    place offset type &environment env)
    (setq place (macroexpand place env))
    (if (foreign-place-p place 'FOREIGN-VALUE)
      `(FOREIGN-VALUE (%OFFSET ,(second place) ,offset (PARSE-C-TYPE ,type)))
      (err whole-form `(offset ,place ,offset ,type))))

  ;; Extract FOREIGN-VARIABLE object underlying the place
  (defmacro c-var-object (&whole whole-form
                          place &environment env)
    (setq place (macroexpand place env))
    (if (foreign-place-p place 'FOREIGN-VALUE)
      (second place)
      (err whole-form `(c-var-object ,place))))

  ;; The equivalent of (FOREIGN-ADDRESS fvar) for c-places:
  ;; (c-var-address (foreign-value x)) --> (foreign-address x)
  (defmacro c-var-address (&whole whole-form
                           place &environment env)
    (setq place (macroexpand place env))
    (if (foreign-place-p place 'FOREIGN-VALUE)
      `(FOREIGN-ADDRESS ,(second place))
      (err whole-form `(c-var-address ,place))))

  ;; Similarly for TYPEOF.
  ;; (typeof (foreign-value x)) --> (deparse-c-type (foreign-type x))
  (defmacro typeof (&whole whole-form
                    place &environment env)
    (setq place (macroexpand place env))
    (if (foreign-place-p place 'FOREIGN-VALUE)
      `(DEPARSE-C-TYPE (FOREIGN-TYPE ,(second place)))
      (err whole-form `(typeof ,place))))

  ;; Similar tricks are being played for SIZEOF, BITSIZEOF.
  ;; They are macros which work on <c-place>s.
  ;; If the argument is not a <c-place>, they behave like
  ;; ordinary functions.
  ;; (sizeof (foreign-value x))  --> (sizeof (typeof (foreign-value x)))
  ;;                             --> (sizeof (deparse-c-type (foreign-type x)))
  ;;                             --> (%sizeof (foreign-type x))
  ;; (sizeof (deparse-c-type y)) --> (%sizeof y)
  ;; (sizeof z)                  --> (%sizeof (parse-c-type z))
  (defmacro sizeof (place &environment env)
    (setq place (macroexpand place env))
    (if (foreign-place-p place 'FOREIGN-VALUE)
      `(%SIZEOF (FOREIGN-TYPE ,(second place)))
      (if (foreign-place-p place 'DEPARSE-C-TYPE)
        `(%SIZEOF ,(second place))
        `(%SIZEOF (PARSE-C-TYPE ,place)))))

  (defmacro bitsizeof (place &environment env)
    (setq place (macroexpand place env))
    (if (foreign-place-p place 'FOREIGN-VALUE)
      `(%BITSIZEOF (FOREIGN-TYPE ,(second place)))
      (if (foreign-place-p place 'DEPARSE-C-TYPE)
        `(%BITSIZEOF ,(second place))
        `(%BITSIZEOF (PARSE-C-TYPE ,place))))))

;; ===========================================================================

;;; for self-test
;;; commented out since CLISP does not need them;
;;; they are defined by the test suite.
;; (def-call-out ffi_identity (:arguments (obj int))
;;   (:return-type int) (:language :stdc))
;; (def-c-var ffi_user_pointer (:type c-pointer))
