;;;; Common Lisp Object System for CLISP: Generic Functions
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")


;; ============================= Runtime support =============================

;; Runtime support for CALL-NEXT-METHOD.
(defun %call-next-method (backpointer next-methods original-args new-args) ; ABI
  (let* ((method (car backpointer))
         (gf (method-generic-function method)))
    (when (gf-never-called-p gf)
      ;; Oops, gf still contains a prototype dispatch which only calls
      ;; initial-funcall-gf. This can really happen, because make-instance
      ;; can call initial-make-instance, which calls initialize-instance's
      ;; effective method without going through initialize-instance itself.
      ;; Similarly for initial-initialize-instance and
      ;; initial-reinitialize-instance, which call shared-initialize's
      ;; effective method without going through shared-initialize's dispatch.
      ;; Similarly for slot-value, which call slot-value-using-class's
      ;; effective method without going through slot-value-using-class.
      ;; Remedy by calling the prototype dispatch once.
      (apply (sys::generic-function-effective-method-function gf) original-args))
    ;; Now we can assume that the real dispatch is installed.
    (let* ((emf (sys::generic-function-effective-method-function gf))
           (original-em (apply emf original-args))
           (new-em (apply emf new-args)))
      ;; Protect against the case that emf is a prototype dispatch which only
      ;; calls initial-funcall-gf.
      (when (or (eq original-em gf) (eq new-em gf))
        (error-of-type 'error
          (TEXT "~S in ~S: bug in determination of effective methods")
          'call-next-method gf))
      (if (eq original-em new-em)
        (if next-methods
          (apply next-methods new-args)
          (apply #'%no-next-method backpointer new-args))
        (error-of-type 'error
          (TEXT "~S in ~S: the new arguments ~S have a different effective method than the old arguments ~S")
          'call-next-method gf new-args original-args)))))


;; =================== Initialization and Reinitialization ===================

(defun make-generic-function (generic-function-class caller whole-form funname lambda-list argument-precedence-order method-combination method-class method-class-p declspecs declspecs-p documentation documentation-p user-defined-args ; ABI
                              &rest methods)
  (when user-defined-args
    ;; Provide good error messages. The error message from
    ;; MAKE-INSTANCE later is unintelligible.
    (let ((valid-keywords (class-valid-initialization-keywords generic-function-class)))
      (unless (eq valid-keywords 'T)
        (dolist (option user-defined-args)
          (unless (member (first option) valid-keywords)
            (error-of-type 'ext:source-program-error
              :form whole-form
              :detail option
              (TEXT "~S ~S: invalid ~S option ~S")
              caller funname 'defgeneric option))))))
  (let ((gf (apply #'make-generic-function-instance generic-function-class
              :name funname
              :lambda-list lambda-list
              :argument-precedence-order argument-precedence-order
              (append
                (if method-class-p (list :method-class method-class))
                (list :method-combination method-combination)
                (if declspecs-p (list :declarations declspecs))
                (if documentation-p (list :documentation documentation))
                (mapcan #'(lambda (option) (list (first option) (rest option)))
                        user-defined-args)
                (unless method-class-p (list :method-class method-class))
                (unless declspecs-p (list :declarations declspecs))
                (unless documentation-p (list :documentation documentation))))))
    (dolist (method methods) (add-method gf method))
    gf))

;; When this is true, it is possible to replace a non-generic function with
;; a generic function through DEFGENERIC or ENSURE-GENERIC-FUNCTION.
(defparameter *allow-making-generic* nil)

(defun ensure-generic-function-using-class-<t> (gf funname &rest all-keys
                                                &key (generic-function-class <standard-generic-function>)
                                                     lambda-list
                                                     argument-precedence-order
                                                     (method-class nil method-class-p)
                                                     method-combination
                                                     documentation
                                                     declarations
                                                     declare
                                                     environment
                                                     ((methods methods) nil) ; from DEFGENERIC
                                                &allow-other-keys)
  (declare (ignore lambda-list argument-precedence-order method-combination
                   documentation declarations declare environment methods))
  ;; Argument checks.
  (unless (function-name-p funname)
    (error-of-type 'program-error
      (TEXT "~S: the name of a function must be a symbol, not ~S")
      'ensure-generic-function-using-class funname))
  (unless (defined-class-p generic-function-class)
    (if (symbolp generic-function-class)
      (setq generic-function-class (find-class generic-function-class))
      (error (TEXT "~S for generic-function ~S: generic-function-class ~S is neither a class or a symbol")
             'ensure-generic-function-using-class funname generic-function-class)))
  (unless (subclassp generic-function-class <generic-function>)
    (error (TEXT "~S for generic-function ~S: generic-function-class ~S is not a subclass of GENERIC-FUNCTION")
           'ensure-generic-function-using-class funname generic-function-class))
  ;; Preparation of initialization arguments.
  (setq all-keys (copy-list all-keys))
  (remf all-keys ':generic-function-class)
  (when method-class-p
    (unless (defined-class-p method-class)
      (if (symbolp method-class)
        (setq method-class (find-class method-class))
        (error (TEXT "~S for generic-function ~S: method-class ~S is neither a class or a symbol")
               'ensure-generic-function-using-class funname method-class)))
    (setf (getf all-keys ':method-class) method-class))
  (if gf
    ;; Redefinition of a generic function.
    (progn
      ;; Take into account the new generic-function-class.
      (unless (eq (class-of gf) generic-function-class)
        ;; MOP p. 51 says that an error should be signalled in this case,
        ;; but ANSI CL says that CHANGE-CLASS is used to modify the GF.
        (change-class gf generic-function-class))
      (warn-if-gf-already-called gf)
      (apply #'reinitialize-instance gf all-keys)
      gf)
    ;; First definition of a generic function.
    (setf (fdefinition funname)
          (apply #'make-generic-function-instance
                 generic-function-class
                 :name funname
                 all-keys))))

;; Preliminary.
(defun ensure-generic-function-using-class (gf funname &rest args
                                            &key generic-function-class
                                                 lambda-list
                                                 argument-precedence-order
                                                 method-class
                                                 method-combination
                                                 documentation
                                                 declarations
                                                 declare
                                                 environment
                                            &allow-other-keys)
  (declare (ignore generic-function-class lambda-list argument-precedence-order
                   method-class method-combination documentation declarations
                   declare environment))
  (apply #'ensure-generic-function-using-class-<t> gf funname args))

;; MOP p. 49
(defun ensure-generic-function (funname &rest args
                                &key generic-function-class
                                     lambda-list
                                     argument-precedence-order
                                     method-class
                                     method-combination
                                     documentation
                                     declarations
                                     declare
                                     environment
                                &allow-other-keys)
  (declare (ignore generic-function-class lambda-list argument-precedence-order
                   method-class method-combination documentation declarations
                   declare environment))
  (unless (function-name-p funname)
    (error-of-type 'program-error
      (TEXT "~S: the name of a function must be a symbol, not ~S")
      'ensure-generic-function funname))
  (let ((result
          (apply #'ensure-generic-function-using-class
                 (if (fboundp funname)
                   (let ((gf (fdefinition funname)))
                     (if (typep-class gf <generic-function>)
                       gf
                       (if (not *allow-making-generic*)
                         (error-of-type 'program-error
                           (TEXT "~S: ~S does not name a generic function")
                           'ensure-generic-function funname)
                         nil)))
                   nil)
                 funname
                 args)))
    ; A check, to verify that user-defined methods on
    ; ensure-generic-function-using-class work as they should.
    (unless (typep-class result <generic-function>)
      (error (TEXT "Wrong ~S result for ~S: not a generic-function: ~S")
             'ensure-generic-function-using-class funname result))
    result))

#||
;; For GENERIC-FLET, GENERIC-LABELS
;; like make-generic-function, only that the dispatch-code is
;; installed immediately.
(defun make-generic-function-now (generic-function-class caller whole-form funname lambda-list argument-precedence-order method-combination method-class method-class-p declspecs declspecs-p documentation documentation-p user-defined-args
                                  &rest methods)
  (let ((gf (apply #'make-generic-function generic-function-class caller whole-form funname lambda-list argument-precedence-order method-combination method-class method-class-p declspecs declspecs-p documentation documentation-p user-defined-args methods)))
    (install-dispatch gf)
    gf))
||#


;; ================================ DEFMETHOD ================================

(defmacro defmethod (&whole whole-form
                     funname &rest method-description)
  (setq funname (sys::check-function-name funname 'defmethod))
  (multiple-value-bind (fast-function-factory-lambda method-initargs-forms signature)
      (analyze-method-description 'defmethod whole-form funname method-description)
    `(LET ()
       (COMPILER::EVAL-WHEN-COMPILE
         (COMPILER::C-DEFUN ',funname ,signature nil 'DEFMETHOD))
       (WHEN (GET ,(if (atom funname) `',funname `(SYS::GET-SETF-SYMBOL ',(second funname))) 'SYS::TRACED-DEFINITION)
         (SYS::UNTRACE1 ',funname))
       (DO-DEFMETHOD ',funname (FUNCTION ,fast-function-factory-lambda)
                     (LIST ,@method-initargs-forms)))))

;; Installs a method. Can be called in two ways:
;; (do-defmethod funname method)
;; (do-defmethod funname fast-function-factory method-initargs)
(defun do-defmethod (funname arg1 &optional (arg2 nil must-build-method)) ; ABI
  (let* ((gf
           (if (fboundp funname)
             (let ((gf (fdefinition funname)))
               (if (typep-class gf <generic-function>)
                 gf
                 (error-of-type 'error
                   (TEXT "~S: ~S does not name a generic function")
                   'defmethod funname)))
             (setf (fdefinition funname)
                   ;; Create a GF compatible with the given method signature.
                   (multiple-value-bind (m-lambdalist m-signature)
                       (if must-build-method
                         (values (getf arg2 ':lambda-list)
                                 (getf arg2 'signature))
                         (values (method-lambda-list arg1)
                                 (method-signature arg1)))
                     (let ((gf-lambdalist
                             (gf-lambdalist-from-first-method m-lambdalist m-signature)))
                       (make-generic-function-instance <standard-generic-function>
                         :name funname
                         :lambda-list gf-lambdalist
                         :method-class <standard-method>))))))
         (method
           (if must-build-method
             (let ((method-class (safe-gf-default-method-class gf))
                   (backpointer (list nil)))
               (apply #'make-method-instance method-class
                      'backpointer backpointer
                      (nconc (method-function-initargs method-class
                                                       (funcall arg1 backpointer))
                             arg2)))
             arg1)))
    (add-method gf method)
    method))

;;; (DECLAIM-METHOD function-name qualifier* spec-lambda-list)
;; does the compile-time side effects of
;; (DEFMETHOD function-name qualifier* spec-lambda-list ...)

(defmacro declaim-method (&whole whole-form
                          funname &rest method-description)
  (setq funname (sys::check-function-name funname 'declaim-method))
  (multiple-value-bind (fast-function-factory-lambda method-initargs-forms signature)
      (analyze-method-description 'defmethod whole-form funname method-description)
    (declare (ignore fast-function-factory-lambda method-initargs-forms))
    `(COMPILER::EVAL-WHEN-COMPILE
       (COMPILER::C-DEFUN ',funname ,signature nil 'DEFMETHOD))))


;; ====================== DEFGENERIC and similar Macros ======================

;;; For DEFGENERIC, GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS,
;;;     WITH-ADDED-METHODS
;; caller: symbol
;; whole-form: whole source form
;; funname: function name, symbol or (SETF symbol)
;; lambdalist: lambdalist of the generic function
;; options: (option*)
;; Returns 8 values:
;; 1. generic-function-class-form,
;; 2. signature,
;; 3. argument-precedence-order,
;; 4. method-combination-lambda, to be applied to the generic-function-class,
;; 5. method-class-form,
;; 6. method-class-p,
;; 7. declspecs,
;; 8. declspecs-p,
;; 9. docstring,
;; 10. docstring-p,
;; 11. user-defined-args,
;; 12. method-forms.
(defun analyze-defgeneric (caller whole-form funname lambdalist options)
  (setq funname (sys::check-function-name funname caller))
  ;; Parse the lambdalist:
  (analyze-defgeneric-lambdalist caller whole-form funname lambdalist)
  ;; Process the options:
  (let ((generic-function-classes nil)
        (method-forms '())
        (method-combinations nil)
        (method-classes nil)
        (argorders nil)
        (declares nil)
        (docstrings nil)
        (user-defined-args))
    (dolist (option options)
      (unless (listp option)
        (error-of-type 'ext:source-program-error
          :form whole-form
          :detail option
          (TEXT "~S ~S: not a ~S option: ~S")
          caller funname 'defgeneric option))
      (case (first option)
        (DECLARE
         ;; The DEFGENERIC description in ANSI CL is inconsistent. According to
         ;; the BNF syntax, multiple DECLARE options cannot be passed in a
         ;; single DEFGENERIC forms. However, it also says explicitly "The
         ;; declare option may be specified more than once..."
         #|
         (when declares
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail options
             (TEXT "~S ~S: ~S may only be specified once.")
             caller funname 'declare))
         |#
         (check-gf-declspecs (rest option) 'declare
           #'(lambda (errorstring &rest arguments)
               (error "~S ~S: ~A" caller funname
                      (apply #'format nil errorstring arguments))))
         (setq declares
               (if declares
                 `(DECLARE ,@(append (rest declares) (rest option)))
                 option)))
        (:ARGUMENT-PRECEDENCE-ORDER
         (when argorders
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail options
             #1=(TEXT "~S ~S: ~S may only be specified once.")
             caller funname ':argument-precedence-order))
         (setq argorders option))
        (:DOCUMENTATION
         (unless (and (eql (length option) 2) (stringp (second option)))
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail option
             (TEXT "~S ~S: A string must be specified after ~S : ~S")
             caller funname ':documentation option))
         (when docstrings
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail options
             (TEXT "~S ~S: Only one ~S string is allowed.")
             caller funname ':documentation))
         (setq docstrings (rest option)))
        (:METHOD-COMBINATION
         (when method-combinations
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail options
             #1# caller funname ':method-combination))
         ;; The syntax for this option is
         ;;   (method-combination-name method-combination-argument*)
         ;; CLHS writes "method-combination" here instead of
         ;; "method-combination-name" but this is most probably a typo.
         ;; To be liberal, we also allow a method-combination instead of a
         ;; method-combination-name.
         (unless (>= (length option) 2)
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail option
             (TEXT "~S ~S: A method combination type name must be specified after ~S : ~S")
             caller funname ':method-combination option))
         (let ((method-combination-name (second option)))
           (unless (or (symbolp method-combination-name)
                       (typep-class method-combination-name <method-combination>))
             (error-of-type 'ext:source-program-error
               :form whole-form
               :detail method-combination-name
               (TEXT "~S ~S: Invalid method combination specification: ~S")
               caller funname option))
           (setq method-combinations (rest option))))
        (:GENERIC-FUNCTION-CLASS
         (unless (and (eql (length option) 2) (symbolp (second option)))
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail option
             #2=(TEXT "~S ~S: A class name must be specified after ~S : ~S")
             caller funname ':generic-function-class option))
         (when generic-function-classes
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail options
             #3=(TEXT "~S ~S: Only one ~S option is allowed.")
             caller funname ':generic-function-class))
         (setq generic-function-classes (rest option)))
        (:METHOD-CLASS
         (unless (and (eql (length option) 2) (symbolp (second option)))
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail option
             #2# caller funname ':method-class option))
         (when method-classes
           (error-of-type 'ext:source-program-error
             :form whole-form
             :detail options
             #3# caller funname ':method-class))
         (setq method-classes (rest option)))
        (:METHOD
         (multiple-value-bind (fast-function-factory-lambda method-initargs-forms)
             (analyze-method-description caller whole-form funname (rest option))
           (push (cons fast-function-factory-lambda method-initargs-forms)
                 method-forms)))
        ((:LAMBDA-LIST :DECLARATIONS)
         (error-of-type 'ext:source-program-error
           :form whole-form
           :detail option
           (TEXT "~S ~S: invalid ~S option: ~S")
           caller funname 'defgeneric option))
        (t
         (let ((optionkey (first option)))
           (if (symbolp optionkey)
             (if (assoc optionkey user-defined-args)
               (error-of-type 'ext:source-program-error
                 :form whole-form
                 :detail options
                 #3# caller funname optionkey)
               (push option user-defined-args))
             (error-of-type 'ext:source-program-error
               :form whole-form
               :detail option
               (TEXT "~S ~S: invalid syntax in ~S option: ~S")
               caller funname 'defgeneric option))))))
    ;; Check :argument-precedence-order :
    (multiple-value-bind (signature argument-precedence-order argorder)
        (check-gf-lambdalist+argorder lambdalist (rest argorders) argorders
          #'(lambda (detail errorstring &rest arguments)
              (error-of-type 'ext:source-program-error
                :form whole-form
                :detail detail
                (TEXT "~S ~S: ~A")
                caller funname (apply #'format nil errorstring arguments))))
      (declare (ignore argorder))
      ;; Default :method-combination is STANDARD:
      (unless method-combinations
        (setq method-combinations '(STANDARD)))
      (let ((generic-function-class-form
              (if generic-function-classes
                `(FIND-CLASS ',(first generic-function-classes))
                '<STANDARD-GENERIC-FUNCTION>))
            (method-class-form
              (if method-classes
                `(FIND-CLASS ',(first method-classes))
                '<STANDARD-METHOD>)))
        (values generic-function-class-form
                signature
                argument-precedence-order
                (let ((method-combination-name (car method-combinations))
                      (options (cdr method-combinations)))
                  `(LAMBDA (GF-CLASS)
                     ,(if (symbolp method-combination-name)
                        ;; We need a preliminary generic function because the
                        ;; method combination must be passed to
                        ;; ensure-generic-function, and the GF has not yet been
                        ;; created and initialized at this moment.
                        `(FIND-METHOD-COMBINATION
                           (MAKE-GENERIC-FUNCTION-PROTOTYPE GF-CLASS :NAME ',funname)
                           ',method-combination-name ',options)
                        `(METHOD-COMBINATION-WITH-OPTIONS ',funname ',method-combination-name ',options))))
                method-class-form
                (not (null method-classes))
                ;; list of declspecs
                (cdr declares)
                (not (null declares))
                ;; docstring or nil
                (car docstrings)
                (not (null docstrings))
                (nreverse user-defined-args)
                ;; list of the method-forms
                (mapcar #'(lambda (method-cons)
                            (let ((fast-function-factory-lambda (car method-cons))
                                  (method-initargs-forms (cdr method-cons)))
                              `(LET ((METHOD-CLASS ,method-class-form)
                                     (BACKPOINTER (LIST NIL)))
                                 (APPLY #'MAKE-METHOD-INSTANCE METHOD-CLASS
                                   ,@method-initargs-forms
                                   'BACKPOINTER BACKPOINTER
                                   (METHOD-FUNCTION-INITARGS METHOD-CLASS
                                                             (,fast-function-factory-lambda BACKPOINTER))))))
                  (nreverse method-forms)))))))

;; Parse a DEFGENERIC lambdalist:
;; lambdalist --> reqnum, req-vars, optnum, restp, keyp, keywords, allowp
(defun analyze-defgeneric-lambdalist (caller whole-form funname lambdalist)
  (multiple-value-bind (reqvars optvars rest keyp keywords keyvars allowp)
      (sys::analyze-generic-function-lambdalist lambdalist
        #'(lambda (detail errorstring &rest arguments)
            (error-of-type 'ext:source-program-error
              :form whole-form
              :detail detail
              (TEXT "~S ~S: invalid generic function lambda-list: ~A")
              caller funname (apply #'format nil errorstring arguments))))
    (declare (ignore keyvars))
    (values (length reqvars) reqvars (length optvars)
            (or (not (eql rest 0)) keyp) ; &key implies &rest
            keyp keywords allowp)))


;;; DEFGENERIC

(defmacro defgeneric (&whole whole-form
                      funname lambda-list &rest options)
  (multiple-value-bind (generic-function-class-form signature argument-precedence-order method-combination-lambda method-class-form method-class-p declspecs declspecs-p docstring docstring-p user-defined-args method-forms)
      (analyze-defgeneric 'defgeneric whole-form funname lambda-list options)
    (let ((generic-function-class-var (gensym))
          (generic-function-class-keywords-var (gensym)))
      `(LET ()
         (DECLARE (SYS::IN-DEFUN ,funname))
         (COMPILER::EVAL-WHEN-COMPILE
           (COMPILER::C-DEFUN ',funname ',signature nil 'DEFGENERIC))
         (WHEN (GET ,(if (atom funname) `',funname `(SYS::GET-SETF-SYMBOL ',(second funname))) 'SYS::TRACED-DEFINITION)
           (SYS::UNTRACE1 ',funname))
         ;; NB: no (SYSTEM::REMOVE-OLD-DEFINITIONS ',funname)
         (LET* ((,generic-function-class-var ,generic-function-class-form)
                ,@(if user-defined-args
                    `((,generic-function-class-keywords-var
                        (CLASS-VALID-INITIALIZATION-KEYWORDS ,generic-function-class-var)))))
           ;; Provide good error messages. The error message from
           ;; ENSURE-GENERIC-FUNCTION (actually MAKE-INSTANCE) later is unintelligible.
           ,@(if user-defined-args
               `((UNLESS (EQ ,generic-function-class-keywords-var 'T)
                   ,@(mapcar #'(lambda (option)
                                 `(UNLESS (MEMBER ',(first option) ,generic-function-class-keywords-var)
                                    (ERROR-OF-TYPE 'EXT:SOURCE-PROGRAM-ERROR
                                      :FORM ',whole-form
                                      :DETAIL ',option
                                      (TEXT "~S ~S: invalid option ~S")
                                      'DEFGENERIC ',funname ',option)))
                             user-defined-args))))
           (APPLY #'ENSURE-GENERIC-FUNCTION ',funname
             ;; Here we pass a default for :GENERIC-FUNCTION-CLASS because
             ;; defaulting it in the context of ENSURE-GENERIC-FUNCTION is
             ;; less straightforward than defaulting it here.
             :GENERIC-FUNCTION-CLASS ,generic-function-class-var
             :LAMBDA-LIST ',lambda-list
             ;; Here we pass a default for :ARGUMENT-PRECEDENCE-ORDER because
             ;; it's error-prone to pass :LAMBDA-LIST without
             ;; :ARGUMENT-PRECEDENCE-ORDER.
             :ARGUMENT-PRECEDENCE-ORDER ',argument-precedence-order
             ,@(if method-class-p `(:METHOD-CLASS ,method-class-form))
             :METHOD-COMBINATION (,method-combination-lambda ,generic-function-class-var)
             ,@(if docstring-p `(:DOCUMENTATION ',docstring))
             ,@(if declspecs-p `(:DECLARATIONS ',declspecs))
             'METHODS (LIST ,@method-forms)
             ;; Pass user-defined initargs of the generic-function-class.
             ,@(mapcan #'(lambda (option)
                           (list `',(first option) `',(rest option)))
                       user-defined-args)
             (APPEND
               ;; Pass the default initargs of the generic-function class, in
               ;; order to erase leftovers from the previous definition.
               (MAPCAN #'(LAMBDA (X) (LIST (FIRST X) (FUNCALL (THIRD X))))
                       (CLASS-DEFAULT-INITARGS ,generic-function-class-var))
               (LIST
                 ;; Here we pass defaults for :METHOD-CLASS, :DOCUMENTATION,
                 ;; :DECLARATIONS if the corresponding option wasn't specified
                 ;; in the DEFGENERIC form, because when a generic function is
                 ;; being redefined, passing :DOCUMENTATION NIL to
                 ;; ENSURE-GENERIC-FUNCTION means to erase the documentation
                 ;; string, while nothing means to keep it! See MOP p. 61.
                 ,@(unless method-class-p '(:METHOD-CLASS <STANDARD-METHOD>))
                 ,@(unless docstring-p '(:DOCUMENTATION NIL))
                 ,@(unless declspecs-p '(:DECLARATIONS NIL))))))))))

;; ============================================================================

;; For GENERIC-FUNCTION, GENERIC-FLET, GENERIC-LABELS

;; Transform lambdalist into calling convention for the compiler:
(defun defgeneric-lambdalist-callinfo (caller whole-form funname lambdalist)
  (multiple-value-bind (reqnum req-vars optnum restp keyp keywords allowp)
      (analyze-defgeneric-lambdalist caller whole-form funname lambdalist)
    (declare (ignore req-vars keyp))
    (callinfo reqnum optnum restp keywords allowp)))

(defun make-generic-function-form (caller whole-form funname lambda-list options)
  (multiple-value-bind (generic-function-class-form signature argument-precedence-order method-combination-lambda method-class-form method-class-p declspecs declspecs-p docstring docstring-p user-defined-args method-forms)
      (analyze-defgeneric caller whole-form funname lambda-list options)
    (declare (ignore signature))
    (let ((generic-function-class-var (gensym)))
      `(LET ((,generic-function-class-var ,generic-function-class-form))
         (MAKE-GENERIC-FUNCTION ,generic-function-class-var ',caller ',whole-form ',funname ',lambda-list ',argument-precedence-order (,method-combination-lambda ,generic-function-class-var) ,method-class-form ,method-class-p ',declspecs ,declspecs-p ',docstring ,docstring-p ',user-defined-args
                                ,@method-forms)))))

#| GENERIC-FUNCTION is a TYPE (and a COMMON-LISP symbol) in ANSI CL,
 but not a macro, so this definition violates the standard
(defmacro generic-function (&whole whole-form
                            lambda-list &rest options)
  (make-generic-function-form 'generic-function whole-form 'LAMBDA
                              lambda-list options))
|#

;; For GENERIC-FLET, GENERIC-LABELS
(defun analyze-generic-fundefs (caller whole-form fundefs)
  (let ((names '())
        (funforms '()))
    (dolist (fundef fundefs)
      (unless (and (consp fundef) (consp (cdr fundef)))
        (error-of-type 'ext:source-program-error
          :form whole-form
          :detail fundef
          (TEXT "~S: ~S is not a generic function specification")
          caller fundef))
      (push (first fundef) names)
      (push (make-generic-function-form
             caller whole-form (first fundef) (second fundef) (cddr fundef))
            funforms))
    (values (nreverse names) (nreverse funforms))))


;;; GENERIC-FLET

(defmacro generic-flet (&whole whole-form
                        fundefs &body body)
  (multiple-value-bind (funnames funforms)
      (analyze-generic-fundefs 'generic-flet whole-form fundefs)
    (let ((varnames (gensym-list funnames)))
      `(LET ,(mapcar #'list varnames funforms)
         (FLET ,(mapcar #'(lambda (varname funname)
                            `(,funname (&rest args) (apply ,varname args)))
                        varnames funnames)
           ,@body)))))

;;; GENERIC-LABELS

(defmacro generic-labels (&whole whole-form
                          fundefs &body body)
  (multiple-value-bind (funnames funforms)
      (analyze-generic-fundefs 'generic-labels whole-form fundefs)
    (let ((varnames (gensym-list funnames)))
      `(LET ,varnames
         (FLET ,(mapcar #'(lambda (varname funname)
                            `(,funname (&rest args) (apply ,varname args)))
                        varnames funnames)
           ,@(mapcar #'(lambda (varname funform) `(SETQ ,varname ,funform))
                     varnames funforms)
           ,@body)))))

;;; WITH-ADDED-METHODS
;; is screwed up and therefore will not be implemented.

;; ============================================================================
