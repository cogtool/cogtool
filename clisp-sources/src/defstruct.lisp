;;; Sources for CLISP DEFSTRUCT macro
;;; Bruno Haible 1988-2005
;;; Sam Steingold 1998-2005
;;; German comments translated into English: Stefan Kain 2003-01-14

(in-package "SYSTEM")

#| Explanation of the appearing data types:

   For structure types (but not for structure classes!):

   (get name 'DEFSTRUCT-DESCRIPTION) =
     #(type size keyword-constructor effective-slotlist direct-slotlist
       boa-constructors copier predicate defaultfun0 defaultfun1 ...)

   type (if the type of the whole structure is meant):
      = LIST                   storage as list
      = VECTOR                 storage as (simple-)vector
      = (VECTOR element-type)  storage as vector with element-type

   size is the list length / vector length.

   keyword-constructor = NIL or the name of the keyword-constructor
   boa-constructors = list of names of BOA constructors
   copier = NIL or the name of the copier function
   predicate = NIL or the name of the predicate function

   effective-slotlist is a packed description of the slots of a structure:
   effective-slotlist = ({slot}*)
   slot = an instance of structure-effective-slot-definition, containing:
         name - the slotname,
         initargs - a list containing the initialization argument,
              or NIL for the pseudo-slot containing the structure name in
              named structures,
         offset - the location of the slot in any instance,
         initer = (initform . initfunction) - as usual,
         init-function-form -
              a form (a symbol or a list (SVREF ...)), that yields
              upon evaluation in an arbitrary environment a function,
              that returns the default value, when called.
         type - the declared type for this slot,
         readonly = NIL or = T specifying, if this slot is readonly, i.e.
              after the construction of the Structure the slot cannot be
              changed with (setf ...) anymore.
         (See also pr_structure_default() in io.d.)
   direct-slotlist is the list of slots defined together with the structure:
   direct-slotlist = ({slot*})
   slot = an instance of structure-direct-slot-definition, containing:
         name, initform, initfunction, initargs, type, initer - see above
         writers - list of setters: ((setf struct-slot-name))
         readers - list of getters: (struct-slot-name)
   The initializations are specified as follows:
     - not real slot (i.e. initargs = ()):
       initform           = `(QUOTE ,name)
       initfunction       = a constant-initfunction for name
       init-function-form = `(MAKE-CONSTANT-INITFUNCTION ',name)
     - real slot with constant initform:
       initform           = as specified by the user
       initfunction       = a constant-initfunction for the initform's value
       init-function-form = `(MAKE-CONSTANT-INITFUNCTION ,initform)
     - real slot with non-constant initform:
       initform           = as specified by the user
       initfunction       = a closure taking 0 arguments, or nil
       init-function-form = for inherited slots: `(SVREF ...)
                            for direct slots: `(FUNCTION (LAMBDA () ,initform))
                            In both cases, after some processing: a gensym
                            referring to a binding.

   For structure classes, i.e. if type = T, all this information is contained
   in the CLOS class (get name 'CLOS::CLOSCLASS). In this case, all slots are
   real slots: the names list is stored in the first memory word already by
   ALLOCATE-INSTANCE, without need for corresponding effective-slot-definition.
|#

;; Indices of the fixed elements of a defstruct-description:
;; if you add a slot, you need to modify io.d:SYS::STRUCTURE-READER
(defconstant *defstruct-description-type-location* 0)
(defconstant *defstruct-description-size-location* 1)
(defconstant *defstruct-description-kconstructor-location* 2)
(defconstant *defstruct-description-slots-location* 3)
(defconstant *defstruct-description-direct-slots-location* 4)
(defconstant *defstruct-description-boa-constructors-location* 5)
(defconstant *defstruct-description-copier-location* 6)
(defconstant *defstruct-description-predicate-location* 7)
(proclaim '(constant-inline *defstruct-description-type-location*
                            *defstruct-description-size-location*
                            *defstruct-description-kconstructor-location*
                            *defstruct-description-slots-location*
                            *defstruct-description-direct-slots-location*
                            *defstruct-description-boa-constructors-location*
                            *defstruct-description-copier-location*
                            *defstruct-description-predicate-location*))

(defun make-ds-slot (name initargs offset initer type readonly)
  (clos::make-instance-<structure-effective-slot-definition>
    clos::<structure-effective-slot-definition>
    :name name
    :initargs initargs
    :initform (car initer) :initfunction (cdr initer) 'clos::inheritable-initer initer
    :type type
    'clos::readonly readonly
    'clos::location offset))
(defun copy-<structure-effective-slot-definition> (slot)
  (make-ds-slot
    (clos:slot-definition-name slot)
    (clos:slot-definition-initargs slot)
    (clos:slot-definition-location slot)
    (clos::slot-definition-inheritable-initer slot)
    (clos:slot-definition-type slot)
    (clos::structure-effective-slot-definition-readonly slot)))
(defmacro ds-real-slot-p (slot)
  `(not (null (clos:slot-definition-initargs ,slot))))
(defmacro ds-pseudo-slot-default (slot)
  ;; The pseudo-slots have an initform = (QUOTE name) and an initfunction which
  ;; returns the name.
  `(funcall (clos:slot-definition-initfunction ,slot)))

#| The type test comes in 4 variants. Keep them in sync! |#

#| Type test, for TYPEP.
   Must be equivalent to (typep object (ds-canonicalize-type symbol)).
|#
(defun ds-typep (object symbol desc)
  (declare (ignore symbol))
  (let ((type (svref desc *defstruct-description-type-location*))
        (size (svref desc *defstruct-description-size-location*)))
    (if (eq type 'LIST)
      (and (conses-p size object)
           (dolist (slot (svref desc *defstruct-description-slots-location*) t)
             (unless (ds-real-slot-p slot)
               (unless (eq (nth (clos:slot-definition-location slot) object)
                           (ds-pseudo-slot-default slot))
                 (return nil)))))
      (and (vectorp object) (simple-array-p object)
           (>= (length object) size)
           (equal (array-element-type object)
                  (if (consp type)
                    (upgraded-array-element-type (second type))
                    'T))
           (dolist (slot (svref desc *defstruct-description-slots-location*) t)
             (unless (ds-real-slot-p slot)
               (unless (and (simple-vector-p object)
                            (eq (svref object (clos:slot-definition-location slot))
                                (ds-pseudo-slot-default slot)))
                 (return nil))))))))

#| Type test expansion, for TYPEP compiler macro. |#
(defun ds-typep-expansion (objform symbol desc)
  (declare (ignore symbol))
  (let ((type (svref desc *defstruct-description-type-location*))
        (size (svref desc *defstruct-description-size-location*))
        (tmp (gensym)))
    `(LET ((,tmp ,objform))
       ,(if (eq type 'LIST)
          `(AND ,@(case size
                    (0 '())
                    (1 `((CONSP ,tmp)))
                    (t `((CONSES-P ,size ,tmp))))
                ,@(mapcan #'(lambda (slot)
                              (unless (ds-real-slot-p slot)
                                `((EQ (NTH ,(clos:slot-definition-location slot) ,tmp)
                                      ',(ds-pseudo-slot-default slot)))))
                          (svref desc *defstruct-description-slots-location*)))
          (let ((eltype (if (consp type)
                          (upgraded-array-element-type (second type))
                          'T)))
            `(AND ,@(if (eq eltype 'T)
                      `((SIMPLE-VECTOR-P ,tmp))
                      `((VECTORP ,tmp)
                        (SIMPLE-ARRAY-P ,tmp)
                        (EQUAL (ARRAY-ELEMENT-TYPE ,tmp) ',eltype)))
                  ,(case size
                     (0 'T)
                     (t `(>= (LENGTH ,tmp) ,size)))
                  ,@(mapcan #'(lambda (slot)
                                (unless (ds-real-slot-p slot)
                                  `((EQ (SVREF ,tmp ,(clos:slot-definition-location slot))
                                        ',(ds-pseudo-slot-default slot)))))
                            (svref desc *defstruct-description-slots-location*))))))))

#| Type canonicalization, for SUBTYPEP. |#
(defun ds-canonicalize-type (symbol)
  (let ((desc (get symbol 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (let ((type (svref desc *defstruct-description-type-location*))
            (size (svref desc *defstruct-description-size-location*))
            (slotlist (svref desc *defstruct-description-slots-location*)))
        (if (eq type 'LIST)
          (let ((resulttype 'T))
            ;; Start with T, not (MEMBER NIL), because of the possibility
            ;; of subclasses.
            (dotimes (i size) (setq resulttype (list 'CONS 'T resulttype)))
            (dolist (slot slotlist)
              (unless (ds-real-slot-p slot)
                (let ((resttype resulttype))
                  (dotimes (j (clos:slot-definition-location slot))
                    (setq resttype (third resttype)))
                  (setf (second resttype) `(EQL ,(ds-pseudo-slot-default slot))))))
            resulttype)
          `(AND (SIMPLE-ARRAY ,(if (consp type) (second type) 'T) (*))
                ;; Constraints that cannot be represented through ANSI CL
                ;; type specifiers. We use SATISFIES types with uninterned
                ;; symbols. This is possible because this function is only
                ;; used for SUBTYPEP.
                ,@(when (or (plusp size)
                            (some #'(lambda (slot) (not (ds-real-slot-p slot)))
                                  slotlist))
                    (let ((constraint-name (gensym)))
                      (setf (symbol-function constraint-name)
                            #'(lambda (x) (typep x symbol)))
                      `((SATISFIES ,constraint-name)))))))
      ; The DEFSTRUCT-DESCRIPTION was lost.
      'NIL)))

#| (ds-make-pred predname type name slotlist size)
   returns the form, that creates the type-test-predicate for
   the structure name.

   type         the type of the structure,
   name         the name of the structure,
   predname     the name of the type-test-predicate,
   slotlist     (only used when type /= T) list of slots
   size         instance size
|#
(defun ds-make-pred (predname type name slotlist size)
  `(,@(if (eq type 'T) `((PROCLAIM '(INLINE ,predname))) '())
    (DEFUN ,predname (OBJECT)
      ,(if (eq type 'T)
         `(%STRUCTURE-TYPE-P ',name OBJECT)
         (let ((max-offset -1)
               (max-name-offset -1))
           (dolist (slot+initff slotlist)
             (let ((slot (car slot+initff)))
               (setq max-offset (max max-offset (clos:slot-definition-location slot)))
               (unless (ds-real-slot-p slot)
                 (setq max-name-offset (max max-name-offset (clos:slot-definition-location slot))))))
           ; This code is only used when there is at least one named slot.
           (assert (<= 0 max-name-offset max-offset))
           (assert (< max-offset size))
           (if (eq type 'LIST)
             `(AND ,@(case size
                       (0 '())
                       (1 `((CONSP OBJECT)))
                       (t `((CONSES-P ,size OBJECT))))
                   ,@(mapcan #'(lambda (slot+initff)
                                 (let ((slot (car slot+initff)))
                                   (unless (ds-real-slot-p slot)
                                     `((EQ (NTH ,(clos:slot-definition-location slot) OBJECT)
                                           ',(ds-pseudo-slot-default slot))))))
                             slotlist))
             ; This code is only used when there is at least one named slot.
             ; Therefore the vector's upgraded element type must contain
             ; SYMBOL, i.e. it must be a general vector.
             `(AND (SIMPLE-VECTOR-P OBJECT)
                   (>= (LENGTH OBJECT) ,size)
                   ,@(mapcan #'(lambda (slot+initff)
                                 (let ((slot (car slot+initff)))
                                   (unless (ds-real-slot-p slot)
                                     `((EQ (SVREF OBJECT ,(clos:slot-definition-location slot))
                                           ',(ds-pseudo-slot-default slot))))))
                             slotlist))))))))

#| auxiliary function for both constructors:
   (ds-arg-default arg slot+initff)
   returns for an argument arg (part of the argument list) the part of
   the argument list, that binds this argument with the default for slot.
|#

(defun ds-arg-default (arg slot+initff)
  (let* ((slot (car slot+initff))
         (initer (clos::slot-definition-inheritable-initer slot))
         (initfunction (clos::inheritable-slot-definition-initfunction initer)))
    `(,arg
      ;; Initial value: If it is not a constant form, must funcall the
      ;; initfunction. If it is a constant, we can use the initform directly.
      ;; If no initform has been provided, ANSI CL says that "the consequences
      ;; are undefined if an attempt is later made to read the slot's value
      ;; before a value is explicitly assigned", i.e. we could leave the slot
      ;; uninitialized (= #<UNBOUND> in the structure case). But CLtL2 says
      ;; "the element's initial value is undefined", which implies that the
      ;; slot is initialized to an arbitrary value. We use NIL as this value.
      ,(if ; equivalent to (constantp (clos::inheritable-slot-definition-initform initer))
           (or (null initfunction) (constant-initfunction-p initfunction))
         (clos::inheritable-slot-definition-initform initer)
         `(FUNCALL ,(cdr slot+initff))))))

#| auxiliary function for both constructors:
   (ds-make-constructor-body type name names size slotlist get-var)
   returns the expression, that creates and fills a structure
   of given type.
|#
(defun ds-make-constructor-body (type name names size slotlist varlist)
  (if (and (or (eq type 'VECTOR) (eq type 'LIST))
           (do ((slotlistr slotlist (cdr slotlistr))
                (index 0 (1+ index)))
               ((null slotlistr) (eql index size))
             (let* ((slot+initff (car slotlistr))
                    (slot (car slot+initff)))
               (unless (eq (clos:slot-definition-location slot) index)
                 (return nil)))))
    ;; optimize the simple case
    `(,type ,@(mapcar #'(lambda (slot+initff var)
                          (let ((slot (car slot+initff)))
                            (if (ds-real-slot-p slot)
                              `(THE ,(clos:slot-definition-type slot) ,var)
                              `(QUOTE ,(ds-pseudo-slot-default slot)))))
                       slotlist varlist))
    `(LET ((OBJECT
             ,(cond ((eq type 'T) `(%MAKE-STRUCTURE ,names ,size))
                    ((eq type 'LIST) `(MAKE-LIST ,size))
                    ((consp type)
                     `(MAKE-ARRAY ,size :ELEMENT-TYPE ',(second type)))
                    (t `(MAKE-ARRAY ,size)))))
       ,@(mapcar
           #'(lambda (slot+initff var)
               (let* ((slot (car slot+initff))
                      (offset (clos:slot-definition-location slot)))
                 `(SETF
                    ,(cond ((eq type 'T)
                            `(%STRUCTURE-REF ',name OBJECT ,offset))
                           ((eq type 'LIST)
                            `(NTH ,offset OBJECT))
                           ((eq type 'VECTOR)
                            `(SVREF OBJECT ,offset))
                           (t `(AREF OBJECT ,offset)))
                    ,(if (or (eq type 'T) (ds-real-slot-p slot))
                       `(THE ,(clos:slot-definition-type slot) ,var)
                       `(QUOTE ,(ds-pseudo-slot-default slot))))))
           slotlist varlist)
       OBJECT)))

#| auxiliary function for ds-make-boa-constructor:

   (ds-arg-with-default arg slotlist)
   returns for an argument arg (part of the argument list) the part of
   the argument list, that binds this argument with the correct default value.
|#

(defun ds-arg-with-default (arg slotlist)
  (if (and (listp arg) (consp (cdr arg)))
    ;; default value is already supplied
    arg
    ;; no default value in the lambda-list
    (let* ((var (if (listp arg) (first arg) arg))
           (slot+initff (find (if (consp var) (second var) var) slotlist
                              :key #'(lambda (slot+initff)
                                       (clos:slot-definition-name (car slot+initff)))
                              :test #'eq)))
      (if slot+initff
        ;; slot found -> take its default value
        (ds-arg-default var slot+initff)
        ;; slot not found, no default value
        arg))))

#| (ds-make-boa-constructor descriptor type name names size slotlist whole-form)
   returns the form that defines the BOA-constructor.
|#
(defun ds-make-boa-constructor (descriptor type name names size slotlist whole-form)
  (let ((constructorname (first descriptor))
        (arglist (second descriptor)))
    (multiple-value-bind (reqs optvars optinits optsvars rest
                          keyflag keywords keyvars keyinits keysvars
                          allow-other-keys auxvars auxinits)
        (analyze-lambdalist arglist
          #'(lambda (detail errorstring &rest arguments)
              (error-of-type 'source-program-error
                :form whole-form
                :detail detail
                (TEXT "~S ~S: In ~S argument list: ~A")
                'defstruct name ':constructor
                (apply #'format nil errorstring arguments))))
      (let* ((argnames
               ; The list of all arguments that are already supplied with
               ; values through the parameter list.
               (append reqs optvars (if (not (eql rest 0)) (list rest))
                       keyvars auxvars))
             (new-arglist ; new argument list
               `(;; required args:
                 ,@reqs
                 ;; optional args:
                 ,@(if optvars
                     (cons '&optional
                           (mapcar #'(lambda (arg var init svar)
                                       (declare (ignore var init svar))
                                       (ds-arg-with-default arg slotlist))
                                   (cdr (memq '&optional arglist))
                                   optvars optinits optsvars))
                     '())
                 ;; &rest arg:
                 ,@(if (not (eql rest 0))
                     (list '&rest rest)
                     '())
                 ;; &key args:
                 ,@(if keyflag
                     (cons '&key
                           (append
                             (mapcar #'(lambda (arg symbol var init svar)
                                         (declare (ignore symbol var init svar))
                                         (ds-arg-with-default arg slotlist))
                                     (cdr (memq '&key arglist))
                                     keywords keyvars keyinits keysvars)
                             (if allow-other-keys '(&allow-other-keys) '())))
                     '())
                 ;; &aux args:
                 &aux
                 ,@(mapcar #'(lambda (arg var init)
                               (declare (ignore var init))
                               (ds-arg-with-default arg slotlist))
                           (cdr (memq '&aux arglist))
                           auxvars auxinits)
                 ,@(let ((slotinitlist nil))
                     (dolist (slot+initff slotlist)
                       (let ((slot (car slot+initff)))
                         (when (or (eq type 'T) (ds-real-slot-p slot))
                           (unless (memq (clos:slot-definition-name slot) argnames)
                             (push (ds-arg-with-default
                                     (clos:slot-definition-name slot) slotlist)
                                   slotinitlist)))))
                     (nreverse slotinitlist)))))
        `(DEFUN ,constructorname ,new-arglist
           ,(ds-make-constructor-body type name names size slotlist
              (mapcar #'(lambda (slot+initff)
                          (clos:slot-definition-name (car slot+initff)))
                      slotlist)))))))

#| (ds-make-keyword-constructor descriptor type name names size slotlist)
   returns the form, that defines the keyword-constructor. |#
(defun ds-make-keyword-constructor (descriptor type name names size slotlist)
  (let ((varlist
          (mapcar #'(lambda (slot+initff)
                      (let ((slot (car slot+initff)))
                        (if (or (eq type 'T) (ds-real-slot-p slot))
                          (make-symbol
                            (symbol-name (clos:slot-definition-name slot)))
                          nil)))
                  slotlist)))
    `(DEFUN ,descriptor
       (&KEY
        ,@(mapcan
            #'(lambda (slot+initff var)
                (let ((slot (car slot+initff)))
                  (if (or (eq type 'T) (ds-real-slot-p slot))
                    (list (ds-arg-default var slot+initff))
                    '())))
            slotlist varlist))
       ,(ds-make-constructor-body type name names size slotlist varlist))))

(defun ds-make-copier (copiername name type)
  (declare (ignore name))
  `(,@(if (or (eq type 'T) (eq type 'LIST))
        `((PROCLAIM '(INLINE ,copiername)))
        '())
    (DEFUN ,copiername (STRUCTURE)
      ,(if (eq type 'T)
         '(COPY-STRUCTURE STRUCTURE)
         (if (eq type 'LIST)
           '(COPY-LIST STRUCTURE)
           (if (consp type)
             `(LET* ((OBJ-LENGTH (ARRAY-TOTAL-SIZE STRUCTURE))
                     (OBJECT (MAKE-ARRAY OBJ-LENGTH :ELEMENT-TYPE
                                         (QUOTE ,(second type)))))
                (DOTIMES (I OBJ-LENGTH OBJECT)
                  (SETF (AREF OBJECT I) (AREF STRUCTURE I))))
             '(%COPY-SIMPLE-VECTOR STRUCTURE)))))))

(defun ds-accessor-name (slotname concname)
  (if concname
    (concat-pnames concname slotname)
    slotname))

(defun ds-make-readers (name names type concname slotlist)
  (mapcap
    #'(lambda (slot+initff)
        (let ((slot (car slot+initff)))
          (when (or (eq type 'T) (ds-real-slot-p slot))
            (let ((accessorname (ds-accessor-name (clos:slot-definition-name slot) concname))
                  (offset (clos:slot-definition-location slot))
                  (slottype (clos:slot-definition-type slot)))
              ;; This makes the macroexpansion depend on the current state
              ;; of the compilation environment, but it doesn't hurt because
              ;; the included structure's definition must already be
              ;; present in the compilation environment anyway. We don't expect
              ;; people to re-DEFUN defstruct accessors.
              (unless (memq (get accessorname 'SYSTEM::DEFSTRUCT-READER name)
                            (cdr names))
                `((PROCLAIM '(FUNCTION ,accessorname (,name) ,slottype))
                  (PROCLAIM '(INLINE ,accessorname))
                  (DEFUN ,accessorname (OBJECT)
                    (THE ,slottype
                      ,(cond ((eq type 'T)
                              `(%STRUCTURE-REF ',name OBJECT ,offset))
                             ((eq type 'LIST) `(NTH ,offset OBJECT))
                             ((consp type) `(AREF OBJECT ,offset))
                             (t `(SVREF OBJECT ,offset)))))
                  (SYSTEM::%PUT ',accessorname 'SYSTEM::DEFSTRUCT-READER
                                ',name)))))))
    slotlist))

(defun ds-make-writers (name names type concname slotlist)
  (mapcap
    #'(lambda (slot+initff)
        (let ((slot (car slot+initff)))
          (when (and (or (eq type 'T) (ds-real-slot-p slot))
                     (not (clos::structure-effective-slot-definition-readonly slot)))
            (let ((accessorname (ds-accessor-name (clos:slot-definition-name slot) concname))
                  (offset (clos:slot-definition-location slot))
                  (slottype (clos:slot-definition-type slot)))
              ;; This makes the macroexpansion depend on the current state
              ;; of the compilation environment, but it doesn't hurt because
              ;; the included structure's definition must already be
              ;; present in the compilation environment anyway. We don't expect
              ;; people to re-DEFUN or re-DEFSETF defstruct accessors.
              (unless (memq (get accessorname 'SYSTEM::DEFSTRUCT-WRITER name)
                            (cdr names))
                `((PROCLAIM '(FUNCTION (SETF ,accessorname) (,slottype ,name) ,slottype))
                  (PROCLAIM '(INLINE (SETF ,accessorname)))
                  (DEFUN (SETF ,accessorname) (VALUE OBJECT)
                    ,(if (eq type 'T)
                       `(%STRUCTURE-STORE ',name
                          OBJECT
                          ,offset
                          ,(if (eq 'T slottype)
                             `VALUE
                             `(THE ,slottype VALUE)))
                       (if (eq type 'LIST)
                         `(SETF (NTH ,offset OBJECT) VALUE)
                         (if (consp type)
                           `(SETF (AREF OBJECT ,offset) VALUE)
                           `(SETF (SVREF OBJECT ,offset) VALUE)))))
                  (SYSTEM::%PUT ',accessorname 'SYSTEM::DEFSTRUCT-WRITER
                                ',name)))))))
    slotlist))

(defun find-structure-class-slot-initfunction (classname slotname) ; ABI
  (let ((class (find-class classname)))
    (unless (clos::structure-class-p class)
      (error (TEXT "The class ~S is not a structure class: ~S")
             classname class))
    (let* ((slots (clos:class-slots class))
           (slot
             ; (find slotname (the list) slots :test #'clos:slot-definition-name)
             (dolist (s slots)
               (when (eql (clos:slot-definition-name s) slotname) (return s)))))
      (unless slot
        (error (TEXT "The class ~S has no slot named ~S.")
               classname slotname))
      (clos:slot-definition-initfunction slot))))

(defun find-structure-slot-initfunction (name slotname) ; ABI
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (unless desc
      (if (clos::defined-class-p (get name 'CLOS::CLOSCLASS))
        (error (TEXT "The structure type ~S has been defined as a class.")
               name)
        (error (TEXT "The structure type ~S has not been defined.")
               name)))
    (let* ((slots (svref desc *defstruct-description-slots-location*))
           (slot
             ; (find slotname (the list) slots :test #'clos:slot-definition-name)
             (dolist (s slots)
               (when (eql (clos:slot-definition-name s) slotname) (return s)))))
      (unless slot
        (error (TEXT "The structure type ~S has no slot named ~S.")
               name slotname))
      (clos:slot-definition-initfunction slot))))

(defun ds-initfunction-fetcher (name type slotname)
  (if (eq type 'T)
    `(FIND-STRUCTURE-CLASS-SLOT-INITFUNCTION ',name ',slotname)
    `(FIND-STRUCTURE-SLOT-INITFUNCTION ',name ',slotname)))

;; A hook for CLOS
(predefun clos::defstruct-remove-print-object-method (name) ; preliminary
  (declare (ignore name))
  nil)

(defun make-load-form-slot-list (slotlist default-slots default-vars mlf)
  (mapcar #'(lambda (slot+initff)
              (let ((slot (car slot+initff)))
                (funcall mlf
                         slot
                         (let ((i (position slot+initff default-slots)))
                           (if i (nth i default-vars) (cdr slot+initff))))))
          slotlist))

(defmacro defstruct (&whole whole-form
                     name-and-options . docstring-and-slotargs)
  (let ((name                              name-and-options)
        (options                           nil)
        (conc-name-option                  t)
        (constructor-option-list           nil)
        (keyword-constructor               nil)
        (boa-constructors                  '())
        (copier-option                     t)
        (predicate-option                  0)
        (include-option                    nil)
         names
         namesform
        (namesbinding                      nil)
        (print-object-option               nil)
        (type-option                       t)
        (named-option                      0)
        (initial-offset-option             0)
        (initial-offset                    0)
        (docstring                         nil)
        (slotargs                          docstring-and-slotargs)
        (directslotlist                    nil) ; list of (slot . initff)
         size
        (include-skip                      0)
        (inherited-slot-count              0)
        (slotlist                          nil) ; list of (slot . initff)
        (slotdefaultvars                   nil)
        (slotdefaultfuns                   nil)
        (slotdefaultslots                  nil) ; list of (slot . initff)
        (slotdefaultdirectslots            nil) ; list of (slot . initff)
         constructor-forms                      )
    ;; check name-and-options:
    (when (listp name-and-options)
      (setq name (first name-and-options))
      (setq options (rest name-and-options)))
    ;; otherwise, name and options are already correct.
    (unless (symbolp name)
      (error-of-type 'source-program-error
        :form whole-form
        :detail name
        (TEXT "~S: invalid syntax for name and options: ~S")
        'defstruct name-and-options))
    ;; name is a symbol, options is the list of options.
    ;; processing the options:
    (dolist (option options)
      (when (keywordp option) (setq option (list option))) ; option without arguments
      (if (listp option)
        (if (keywordp (car option))
          (case (first option)
            (:CONC-NAME
             (setq conc-name-option (second option)))
            (:CONSTRUCTOR
               (if (atom (cdr option))
                 ;; default-keyword-constructor
                 (push (concat-pnames "MAKE-" name) constructor-option-list)
                 (let ((arg (second option)))
                   (setq arg (check-symbol arg 'defstruct))
                   (push
                     (if (atom (cddr option))
                       arg ; keyword-constructor
                       (if (not (listp (third option)))
                         (error-of-type 'source-program-error
                           :form whole-form
                           :detail (third option)
                           (TEXT "~S ~S: argument list should be a list: ~S")
                           'defstruct name (third option))
                         (rest option))) ; BOA-constructor
                     constructor-option-list))))
            (:COPIER
               (when (consp (cdr option))
                 (let ((arg (second option)))
                   (setq arg (check-symbol arg 'defstruct))
                   (setq copier-option arg))))
            (:PREDICATE
               (when (consp (cdr option))
                 (let ((arg (second option)))
                   (setq arg (check-symbol arg 'defstruct))
                   (setq predicate-option arg))))
            ((:INCLUDE :INHERIT)
               (if (null include-option)
                 (setq include-option option)
                 (error-of-type 'source-program-error
                   :form whole-form
                   :detail options
                   (TEXT "~S ~S: At most one :INCLUDE argument may be specified: ~S")
                   'defstruct name options)))
            ((:PRINT-FUNCTION :PRINT-OBJECT)
               (if (null (cdr option))
                 (setq print-object-option '(PRINT-STRUCTURE STRUCT STREAM))
                 (let ((arg (second option)))
                   (when (and (consp arg) (eq (first arg) 'FUNCTION))
                     (warn (TEXT "~S: Use of ~S implicitly applies FUNCTION.~@
                                     Therefore using ~S instead of ~S.")
                           'defstruct (first option) (second arg) arg)
                     (setq arg (second arg)))
                   (setq print-object-option
                         `(,arg STRUCT STREAM
                           ,@(if (eq (first option) ':PRINT-FUNCTION)
                                 '(*PRIN-LEVEL*) '()))))))
            (:TYPE (setq type-option (second option)))
            (:NAMED (setq named-option t))
            (:INITIAL-OFFSET (setq initial-offset-option (or (second option) 0)))
            (T (error-of-type 'source-program-error
                 :form whole-form
                 :detail (first option)
                 (TEXT "~S ~S: unknown option ~S")
                 'defstruct name (first option))))
          (error-of-type 'source-program-error
            :form whole-form
            :detail option
            (TEXT "~S ~S: invalid syntax in ~S option: ~S")
            'defstruct name 'defstruct option))
        (error-of-type 'source-program-error
          :form whole-form
          :detail option
          (TEXT "~S ~S: not a ~S option: ~S")
          'defstruct name 'defstruct option)))
    ;;; conc-name-option is either T or NIL or the :CONC-NAME argument.
    ;; constructor-option-list is a list of all :CONSTRUCTOR-arguments,
    ;;   each in the form  symbol  or  (symbol arglist . ...).
    ;; copier-option is either T or the :COPIER-argument.
    ;; predicate-option is either 0 or the :PREDICATE-argument.
    ;; include-option is either NIL or the entire
    ;;   :INCLUDE/:INHERIT-option.
    ;; print-object-option is NIL or a form for the body of the PRINT-OBJECT
    ;;   method.
    ;; type-option is either T or the :TYPE-argument.
    ;; named-option is either 0 or T.
    ;; initial-offset-option is either 0 or the :INITIAL-OFFSET-argument.
    ;;; inspection of the options:
    (setq named-option (or (eq type-option 'T) (eq named-option 'T)))
    ;; named-option (NIL or T) specifies, if the name is in the structure.
    (if named-option
      (when (eql predicate-option 0)
        (setq predicate-option (concat-pnames name "-P"))) ; defaultname
      (if (or (eql predicate-option 0) (eq predicate-option 'NIL))
        (setq predicate-option 'NIL)
        (error-of-type 'source-program-error
          :form whole-form
          :detail predicate-option
          (TEXT "~S ~S: There is no ~S for unnamed structures.")
          'defstruct name :predicate)))
    ;; predicate-option is
    ;;   if named-option=T: either NIL or the name of the type-test-predicate,
    ;;   if named-option=NIL meaningless.
    (when (eq conc-name-option 'T)
      (setq conc-name-option (string-concat (string name) "-")))
    ;; conc-name-option is the name prefix.
    (if constructor-option-list
      (setq constructor-option-list (remove 'NIL constructor-option-list))
      (setq constructor-option-list (list (concat-pnames "MAKE-" name))))
    ;; constructor-option-list is a list of all constructors that have to be
    ;; created, each in the form  symbol  or  (symbol arglist . ...).
    (if (eq copier-option 'T)
      (setq copier-option (concat-pnames "COPY-" name)))
    ;; copier-option is either NIL or the name of the copy function.
    (unless (or (eq type-option 'T)
                (eq type-option 'VECTOR)
                (eq type-option 'LIST)
                (and (consp type-option) (eq (first type-option) 'VECTOR)))
      (error-of-type 'source-program-error
        :form whole-form
        :detail type-option
        (TEXT "~S ~S: invalid :TYPE option ~S")
        'defstruct name type-option))
    ;; type-option is either T or LIST or VECTOR or (VECTOR ...)
    (unless (and (integerp initial-offset-option) (>= initial-offset-option 0))
      (error-of-type 'source-program-error
        :form whole-form
        :detail initial-offset-option
        (TEXT "~S ~S: The :INITIAL-OFFSET must be a nonnegative integer, not ~S")
        'defstruct name initial-offset-option))
    ;; initial-offset-option is an Integer >=0.
    (when (and (plusp initial-offset-option) (eq type-option 'T))
      (error-of-type 'source-program-error
        :form whole-form
        :detail options
        (TEXT "~S ~S: :INITIAL-OFFSET must not be specified without :TYPE : ~S")
        'defstruct name options))
    ;; if type-option=T, then initial-offset-option=0.
    (when (eq type-option 'T) (setq include-skip 1))
    ;; if type-option=T, include-skip is 1, else 0.
    (when (stringp (first docstring-and-slotargs))
      (setq docstring (first docstring-and-slotargs))
      (setq slotargs (rest docstring-and-slotargs)))
    ;; else, docstring and slotargs are already correct.
    ;; docstring is either NIL or a String.
    ;; slotargs are the remaining arguments.
    (if include-option
      (let* ((option (rest include-option))
             (subname (first option))
             (incl-class (get subname 'CLOS::CLOSCLASS))
             (incl-desc (get subname 'DEFSTRUCT-DESCRIPTION)))
        (unless (clos::defined-class-p incl-class)
          (setq incl-class nil))
        (when (and (null incl-class) (null incl-desc))
          (error-of-type 'source-program-error
            :form whole-form
            :detail subname
            (TEXT "~S ~S: included structure ~S has not been defined.")
            'defstruct name subname))
        (when (and incl-class (not (clos::structure-class-p incl-class)))
          (error-of-type 'source-program-error
            :form whole-form
            :detail subname
            (TEXT "~S ~S: included structure ~S is not a structure type.")
            'defstruct name subname))
        (when incl-class
          (setq names (cons name (clos::class-names incl-class)))
          (setq namesbinding
                (list
                  (list
                    (setq namesform (gensym))
                    `(CONS ',name (CLOS::CLASS-NAMES (GET ',subname 'CLOS::CLOSCLASS)))))))
        (unless (equalp (if incl-class 't (svref incl-desc *defstruct-description-type-location*)) type-option)
          (error-of-type 'source-program-error
            :form whole-form
            :detail subname
            (TEXT "~S ~S: included structure ~S must be of the same type ~S.")
            'defstruct name subname type-option))
        (setq slotlist
          (nreverse
            (mapcar #'(lambda (slot)
                        (cons (copy-<structure-effective-slot-definition> slot)
                              (ds-initfunction-fetcher subname type-option
                                (clos:slot-definition-name slot))))
                    (if incl-class
                      (clos:class-slots incl-class)
                      (svref incl-desc *defstruct-description-slots-location*)))))
        ;; slotlist is the reversed list of the inherited slots.
        (setq include-skip (if incl-class
                             (clos::class-instance-size incl-class)
                             (svref incl-desc *defstruct-description-size-location*)))
        (when slotlist
          (assert (> include-skip (clos:slot-definition-location (car (first slotlist))))))
        ;; include-skip >=0 is the number of slots that are already consumend
        ;;    by the substructure, the "size" of the substructure.
        ;; Process further arguments of the :INCLUDE-option:
        (dolist (slotarg (rest option))
          (let* ((slotname (if (atom slotarg) slotarg (first slotarg)))
                 (slot+initff (find slotname slotlist
                                    :key #'(lambda (slot+initff)
                                             (clos:slot-definition-name (car slot+initff)))
                                    :test #'eq)))
            (when (null slot+initff)
              (error-of-type 'source-program-error
                :form whole-form
                :detail slotname
                (TEXT "~S ~S: included structure ~S has no component with name ~S.")
                'defstruct name subname slotname))
            (let ((slot (car slot+initff)))
              (if (atom slotarg)
                ; overwrite default to NIL
                (progn
                  (setf (clos::slot-definition-inheritable-initer slot)
                        (cons 'NIL (make-constant-initfunction 'NIL)))
                  (setf (cdr slot+initff) `(MAKE-CONSTANT-INITFUNCTION NIL)))
                (progn
                  (let ((initform (second slotarg)))
                    (if (constantp initform)
                      (progn
                        (setf (clos::slot-definition-inheritable-initer slot)
                              (cons initform (make-constant-initfunction (eval initform))))
                        (setf (cdr slot+initff) `(MAKE-CONSTANT-INITFUNCTION ,initform)))
                      (progn
                        (setf (clos::slot-definition-inheritable-initer slot)
                              (cons initform nil)) ; FIXME
                        (setf (cdr slot+initff)
                              `(FUNCTION ,(concat-pnames "DEFAULT-" slotname)
                                 (LAMBDA () ,initform))))))
                  ;; Process the slot-options of this Slot-Specifier:
                  (do ((slot-arglistr (cddr slotarg) (cddr slot-arglistr)))
                      ((endp slot-arglistr))
                    (let ((slot-keyword (first slot-arglistr))
                          (slot-key-value (second slot-arglistr)))
                      (cond ((eq slot-keyword ':READ-ONLY)
                             (if slot-key-value
                               (setf (clos::structure-effective-slot-definition-readonly slot) t)
                               (if (clos::structure-effective-slot-definition-readonly slot)
                                 (error-of-type 'source-program-error
                                   :form whole-form
                                   :detail subname
                                   (TEXT "~S ~S: The READ-ONLY slot ~S of the included structure ~S must remain READ-ONLY in ~S.")
                                   'defstruct name slotname subname name)
                                 (setf (clos::structure-effective-slot-definition-readonly slot) nil))))
                            ((eq slot-keyword ':TYPE)
                             (unless
                                 (subtypep
                                   (type-for-discrimination slot-key-value)
                                   (type-for-discrimination (clos:slot-definition-type slot)))
                               (error-of-type 'source-program-error
                                 :form whole-form
                                 :detail subname
                                 (TEXT "~S ~S: The type ~S of slot ~S should be a subtype of the type defined for the included strucure ~S, namely ~S.")
                                 'defstruct name slot-key-value slotname subname
                                 (clos:slot-definition-type slot)))
                             (setf (clos:slot-definition-type slot) slot-key-value))
                            (t (error-of-type 'source-program-error
                                 :form whole-form
                                 :detail slot-keyword
                                 (TEXT "~S ~S: ~S is not a slot option.")
                                 'defstruct name slot-keyword)))))))
              (push (cons
                      (clos::make-instance-<structure-direct-slot-definition>
                        clos::<structure-direct-slot-definition>
                        :name slotname
                        :initform (clos:slot-definition-initform slot)
                        :initfunction (clos:slot-definition-initfunction slot)
                        :initargs (clos:slot-definition-initargs slot)
                        :type (clos:slot-definition-type slot)
                        'clos::inheritable-initer (clos::slot-definition-inheritable-initer slot)
                        ;; no readers/writers: these are inherited slots
                        :readers '()
                        :writers '())
                      (cdr slot+initff))
                    directslotlist))))
        (dolist (slot+initff slotlist)
          (let* ((slot (car slot+initff))
                 (initfunction (clos:slot-definition-initfunction slot)))
            (unless (or (null initfunction) (constant-initfunction-p initfunction))
              (let ((variable (gensym)))
                (push (cdr slot+initff) slotdefaultfuns)
                (push variable slotdefaultvars)
                (push slot+initff slotdefaultslots)
                (push nil slotdefaultdirectslots)
                (setf (cdr slot+initff) variable)))))
        (when (eq (first include-option) ':INHERIT)
          (setq inherited-slot-count (length slotlist))))
      (if (eq name 'STRUCTURE-OBJECT)
        (setq names (list name)
              namesform `',names)
        (setq names (cons name (clos::class-names (get 'STRUCTURE-OBJECT 'CLOS::CLOSCLASS)))
              namesbinding
              (list
                (list
                  (setq namesform (gensym))
                  `(CONS ',name (CLOS::CLASS-NAMES (GET 'STRUCTURE-OBJECT 'CLOS::CLOSCLASS))))))))
    ;; names is the include-nesting, namesform is the form belonging to it.
    ;; slotlist is the former slot list, reversed.
    ;; inherited-slot-count is the number of slots, that have to be ignored
    ;; when the accessors are created.
    (when (and named-option ; named structure
               (consp type-option) ; of type (VECTOR ...)
               ;; must be able to contain the name(s):
               (not (typep names (type-for-discrimination (second type-option)))))
      (error-of-type 'source-program-error
        :form whole-form
        :detail type-option
        (TEXT "~S ~S: structure of type ~S cannot hold the name.")
        'defstruct name type-option))
    ;; layout of the structure:
    ;; names, poss. include-slots, initial-offset-option times NIL, slots.
    ;; layout of vector or list:
    ;; include-part, initial-offset-option times NIL, poss. name, slots.
    (setq initial-offset (+ include-skip initial-offset-option))
    (unless (eq type-option 'T)
      (when named-option
        (push
          ; the type recognition pseudo-slot
          (cons
            (make-ds-slot nil
                          '()
                          initial-offset
                          (cons `(QUOTE ,name) (make-constant-initfunction name))
                          'SYMBOL ; type = symbol
                          T)      ; read-only
            `(MAKE-CONSTANT-INITFUNCTION ',name))
          slotlist)
        (incf initial-offset)))
    ;; the slots are situated behind initial-offset.
    ;; If type/=T (i.e vector or list) and named-option, the name is situated
    ;;   in Slot number  (1- initial-offset).
    ;; processing the slots:
    (let ((offset initial-offset))
      (dolist (slotarg slotargs)
        (let (slotname
              initform
              initfunction
              initfunctionform)
          (if (atom slotarg)
            (setq slotname slotarg  initform nil)
            (setq slotname (first slotarg)  initform (second slotarg)))
          ;; Here we compare slot names through their symbol-names, not through
          ;; #'eq, because if we have two slots P::X and Q::X, the two accessor
          ;; functions would have the same name FOO-X.
          (when (find (symbol-name slotname) slotlist
                      :test #'(lambda (name slot+initff)
                                (let ((slot (car slot+initff)))
                                  (and (or (eq type-option 'T) (ds-real-slot-p slot))
                                       (string= (clos:slot-definition-name slot) name)))))
            (error-of-type 'source-program-error
              :form whole-form
              :detail slotname
              (TEXT "~S ~S: There may be only one slot with the name ~S.")
              'defstruct name slotname))
          (let ((type t) (read-only nil))
            (when (consp slotarg)
              (do ((slot-arglistr (cddr slotarg) (cddr slot-arglistr)))
                  ((endp slot-arglistr))
                (let ((slot-keyword (first slot-arglistr))
                      (slot-key-value (second slot-arglistr)))
                  (cond ((eq slot-keyword ':READ-ONLY)
                         (setq read-only (if slot-key-value t nil)))
                        ((eq slot-keyword ':TYPE) (setq type slot-key-value))
                        (t (error-of-type 'source-program-error
                             :form whole-form
                             :detail slot-keyword
                             (TEXT "~S ~S: ~S is not a slot option.")
                             'defstruct name slot-keyword))))))
            (if (constantp initform)
              (setq initfunction (make-constant-initfunction (eval initform))
                    initfunctionform `(MAKE-CONSTANT-INITFUNCTION ,initform))
              (let ((variable (gensym)))
                (push
                  `(FUNCTION ,(concat-pnames "DEFAULT-" slotname)
                     (LAMBDA () ,initform))
                  slotdefaultfuns)
                (push variable slotdefaultvars)
                (setq initfunction nil ; FIXME
                      initfunctionform variable)))
            (let ((initer (cons initform initfunction))
                  (initargs (list (symbol-to-keyword slotname)))
                  (accessorname (ds-accessor-name slotname conc-name-option)))
              (when (eq predicate-option accessorname)
                (warn
                 (TEXT "~S ~S: Slot ~S accessor will shadow the predicate ~S.")
                 'defstruct name slotname predicate-option)
                (setq predicate-option nil))
              (push (cons
                      (clos::make-instance-<structure-direct-slot-definition>
                        clos::<structure-direct-slot-definition>
                        :name slotname
                        :initform initform
                        :initfunction initfunction
                        :initargs initargs
                        :type type
                        'clos::inheritable-initer initer
                        ;; we cannot recover accessor names later
                        ;; because of the :CONC-NAME option
                        :writers (if read-only '() (list `(SETF ,accessorname)))
                        :readers (list accessorname))
                      initfunctionform)
                    directslotlist)
              (push (cons
                      (make-ds-slot slotname
                                    initargs
                                    offset ; location
                                    initer
                                    ;; The following are defstruct specific.
                                    type read-only)
                      initfunctionform)
                    slotlist)
              (unless (constantp initform)
                (push (car slotlist) slotdefaultslots)
                (push (car directslotlist) slotdefaultdirectslots)))))
        (incf offset))
      (setq size offset))
    ;; size = total length of the structure
    (setq slotlist (nreverse slotlist))
    (setq directslotlist (nreverse directslotlist))
    (setq slotdefaultfuns (nreverse slotdefaultfuns))
    (setq slotdefaultvars (nreverse slotdefaultvars))
    (setq slotdefaultslots (nreverse slotdefaultslots))
    (setq slotdefaultdirectslots (nreverse slotdefaultdirectslots))
    ;; the slots in slotlist are now sorted in ascending order again.
    (setq constructor-forms
      (mapcar
        #'(lambda (constructor-option)
            (if (consp constructor-option)
              (ds-make-boa-constructor
                constructor-option type-option name namesform size slotlist whole-form)
              (progn
                (when (null keyword-constructor)
                  (setq keyword-constructor constructor-option))
                (ds-make-keyword-constructor
                  constructor-option type-option name namesform size
                  slotlist))))
        constructor-option-list))
    (setq boa-constructors
          (mapcan #'(lambda (constructor-option)
                      (when (consp constructor-option)
                        (list (first constructor-option))))
                  constructor-option-list))
    ;; constructor-forms = list of forms, that define the constructors.
    (mapc #'(lambda (slot+initff directslot+initff)
              (let* ((slot (car slot+initff))
                     (initfunctionform
                       (ds-initfunction-fetcher name type-option (clos:slot-definition-name slot))))
                (setf (cdr slot+initff) initfunctionform)
                (when directslot+initff
                  (setf (cdr directslot+initff) initfunctionform))))
          slotdefaultslots slotdefaultdirectslots)
    ;; now, slotlist contains no more slotdefaultvars.
    `(EVAL-WHEN (LOAD COMPILE EVAL)
       (LET ()
         (LET ,(append namesbinding (mapcar #'list slotdefaultvars slotdefaultfuns))
           ;; ANSI CL doesn't specify what happens when a structure is
           ;; redefined with different specification. We do here what DEFCLASS
           ;; also does: remove the accessory functions defined by the previous
           ;; specification.
           (STRUCTURE-UNDEFINE-ACCESSORIES ',name)
           ,(if (eq type-option 'T)
              `(REMPROP ',name 'DEFSTRUCT-DESCRIPTION)
              `(%PUT ',name 'DEFSTRUCT-DESCRIPTION
                     (VECTOR ',type-option
                             ,size
                             ',keyword-constructor
                             (LIST ,@(make-load-form-slot-list
                                      slotlist slotdefaultslots slotdefaultvars
                                      'clos::make-load-form-<structure-effective-slot-definition>))
                             (LIST ,@(make-load-form-slot-list
                                      directslotlist slotdefaultdirectslots
                                      slotdefaultvars
                                      'clos::make-load-form-<structure-direct-slot-definition>))
                             ',boa-constructors
                             ',copier-option
                             ',predicate-option)))
           ,(if (eq type-option 'T)
              `(CLOS::DEFINE-STRUCTURE-CLASS ',name
                 ,namesform
                 ',keyword-constructor
                 ',boa-constructors
                 ',copier-option
                 ',predicate-option
                 (LIST ,@(make-load-form-slot-list
                          slotlist slotdefaultslots slotdefaultvars
                          'clos::make-load-form-<structure-effective-slot-definition>))
                 (LIST ,@(make-load-form-slot-list
                          directslotlist slotdefaultdirectslots slotdefaultvars
                          'clos::make-load-form-<structure-direct-slot-definition>)))
              `(CLOS::UNDEFINE-STRUCTURE-CLASS ',name))
           ,@constructor-forms)
         ,@(if (and named-option predicate-option)
             (ds-make-pred predicate-option type-option name slotlist size))
         ,@(if copier-option (ds-make-copier copier-option name type-option))
         ,@(let ((directslotlist (nthcdr inherited-slot-count slotlist)))
             `(,@(ds-make-readers name names type-option conc-name-option
                                  directslotlist)
               ,@(ds-make-writers name names type-option conc-name-option
                                  directslotlist)))
         ;; see documentation.lisp: we map STRUCTURE to TYPE
         (sys::%set-documentation ',name 'TYPE ,docstring)
         ,@(when (eq type-option 'T)
             (list
               (if print-object-option
                 `(CLOS:DEFMETHOD CLOS:PRINT-OBJECT ((STRUCT ,name) STREAM)
                    (PROGN ,print-object-option))
                 `(CLOS::DEFSTRUCT-REMOVE-PRINT-OBJECT-METHOD ',name))))
         ',name))))


;; A kind of Meta-Object Protocol for structures.
;; These function apply to structures of any representation
;; (structure classes as well as subtypes of LIST or VECTOR).
;; This differs from the CLOS MOP
;;   1. in the use of a structure name (symbol) instead of a class,
;;   2. in the different set of available operations: classes in general
;;      don't have kconstructors, boa-constructors, copier, predicate,
;;      whereas on the other hand structures in general don't have a prototype
;;      and finalization.

(defun structure-slots (name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (svref desc *defstruct-description-slots-location*)
      (let ((class (find-class name)))
        (clos::accessor-typecheck class 'structure-class 'structure-slots)
        (clos::class-slots class)))))
#|
 (defun (setf structure-slots) (new-value name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (setf (svref desc *defstruct-description-slots-location*) new-value)
      (let ((class (find-class name)))
        (clos::accessor-typecheck class 'structure-class '(setf structure-slots))
        (setf (clos::class-slots class) new-value)))))
|#

(defun structure-direct-slots (name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (svref desc *defstruct-description-direct-slots-location*)
      (let ((class (find-class name)))
        (clos::accessor-typecheck class 'structure-class 'structure-direct-slots)
        (clos::class-direct-slots class)))))
#|
 (defun (setf structure-slots) (new-value name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (setf (svref desc *defstruct-description-direct-slots-location*) new-value)
      (let ((class (find-class name)))
        (clos::accessor-typecheck class 'structure-class '(setf structure-direct-slots))
        (setf (clos::class-direct-slots class) new-value)))))
|#

(defun structure-instance-size (name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (svref desc *defstruct-description-size-location*)
      (let ((class (find-class name)))
        (clos::accessor-typecheck class 'structure-class 'structure-instance-size)
        (clos::class-instance-size class)))))
#|
 (defun (setf structure-instance-size) (new-value name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (setf (svref desc *defstruct-description-size-location*) new-value)
      (let ((class (find-class name)))
        (clos::accessor-typecheck class 'structure-class '(setf structure-instance-size))
        (setf (clos::class-instance-size class) new-value)))))
|#

(defun structure-kconstructor (name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (svref desc *defstruct-description-kconstructor-location*)
      (clos::class-kconstructor (find-class name)))))
#|
 (defun (setf structure-kconstructor) (new-value name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (setf (svref desc *defstruct-description-kconstructor-location*) new-value)
      (setf (clos::class-kconstructor (find-class name)) new-value))))
|#

(defun structure-boa-constructors (name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (svref desc *defstruct-description-boa-constructors-location*)
      (clos::class-boa-constructors (find-class name)))))
#|
 (defun (setf structure-boa-constructors) (new-value name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (setf (svref desc *defstruct-description-boa-constructors-location*) new-value)
      (setf (clos::class-boa-constructors (find-class name)) new-value))))
|#

(defun structure-copier (name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (svref desc *defstruct-description-copier-location*)
      (clos::class-copier (find-class name)))))
#|
 (defun (setf structure-copier) (new-value name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (setf (svref desc *defstruct-description-copier-location*) new-value)
      (setf (clos::class-copier (find-class name)) new-value))))
|#

(defun structure-predicate (name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (svref desc *defstruct-description-predicate-location*)
      (clos::class-predicate (find-class name)))))
#|
 (defun (setf structure-predicate) (new-value name)
  (let ((desc (get name 'DEFSTRUCT-DESCRIPTION)))
    (if desc
      (setf (svref desc *defstruct-description-predicate-location*) new-value)
      (setf (clos::class-predicate (find-class name)) new-value))))
|#

(defun structure-undefine-accessories (name) ; ABI
  (when (or (get name 'DEFSTRUCT-DESCRIPTION)
            (clos::structure-class-p (find-class name nil)))
    (macrolet ((fmakunbound-if-present (symbol-form)
                 `(let ((symbol ,symbol-form))
                    (when symbol (fmakunbound symbol)))))
      (fmakunbound-if-present (structure-kconstructor name))
      (mapc #'fmakunbound (structure-boa-constructors name))
      (fmakunbound-if-present (structure-copier name))
      (fmakunbound-if-present (structure-predicate name))
      (dolist (slot (structure-direct-slots name))
        (mapc #'fmakunbound (clos::slot-definition-readers slot))
        (mapc #'fmakunbound (clos::slot-definition-writers slot))))))
