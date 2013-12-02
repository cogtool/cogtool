;;; CLISP - PLACES.LSP
;;; CLISP-specific: string-concat, %rplaca, %rplacd, store, setelt, ...

(in-package "SYSTEM")

;;;----------------------------------------------------------------------------
;;; Funktionen zur Definition und zum Ausnutzen von places:
;;;----------------------------------------------------------------------------
;; Return a symbol for SYSTEM::SETF-FUNCTION
;; the returned symbol will be interned iff the argument is.
(defun setf-symbol (symbol)
  (let* ((pack (symbol-package symbol))
         (name (string-concat "(SETF " (if pack (package-name pack) "#") ":"
                              (symbol-name symbol) ")")))
    (if pack
        (intern name pack)
        (make-symbol name))))
;;;----------------------------------------------------------------------------
;; Returns the symbol which is on the property list at SYSTEM::SETF-FUNCTION
(defun get-setf-symbol (symbol) ; ABI
  (or (get symbol 'SYSTEM::SETF-FUNCTION)
      (progn
        (when (get symbol 'SYSTEM::SETF-EXPANDER)
          (warn (TEXT "The function (~S ~S) is hidden by a SETF expander.")
                'setf symbol))
        (setf (get symbol 'SYSTEM::SETF-FUNCTION) (setf-symbol symbol)))))
;;;----------------------------------------------------------------------------
;; Returns 5 values:
;;   SM1  temps       variables to bind
;;   SM2  subforms    values to bind to
;;   SM3  stores      variables whose values are used by the setter form
;;   SM4  setterform  setter form
;;   SM5  getterform  getter form
(defun get-setf-expansion (form &optional env)
  (unless env ; user may pass env=NIL to mean "null lexical environment"
    (setq env (vector nil nil)))
  (loop
    ; 1. Schritt: nach globalen SETF-Definitionen suchen:
    (when (and (consp form) (symbolp (car form)))
      (when (global-in-fenv-p (car form) (svref env 1))
        ; Operator nicht lokal definiert
        (let ((plist-info (get (first form) 'SYSTEM::SETF-EXPANDER)))
          (when plist-info
            (return-from get-setf-expansion
              (if (symbolp plist-info) ; Symbol kommt von kurzem DEFSETF
                (do* ((storevar (gensym))
                      (tempvars nil (cons (gensym) tempvars))
                      (tempforms nil (cons (car formr) tempforms))
                      (formr (cdr form) (cdr formr)))
                     ((endp formr)
                      (setq tempforms (nreverse tempforms))
                      (values tempvars
                              tempforms
                              `(,storevar)
                              `(,plist-info ,@tempvars ,storevar)
                              `(,(first form) ,@tempvars)
                     ))
                )
                (let ((argcount (car plist-info)))
                  (if (eql argcount -5)
                    ; (-5 . fun) kommt von DEFINE-SETF-METHOD
                    (funcall (cdr plist-info) form env)
                    ; (argcount storevarcount . fun) kommt von langem DEFSETF
                    (let ((access-form form)
                          (tempvars '())
                          (tempforms '())
                          (new-access-form '()))
                      (let ((i 0)) ; Argumente-Zähler
                        ; argcount = -1 falls keine Keyword-Argumente existieren
                        ; bzw.     = Anzahl der einzelnen Argumente vor &KEY,
                        ;          = nil nachdem diese abgearbeitet sind.
                        (dolist (argform (cdr access-form))
                          (when (eql i argcount) (setq argcount nil i 0))
                          (if (and (null argcount) (evenp i))
                            (if (keywordp argform)
                              (push argform new-access-form)
                              (error-of-type 'source-program-error
                                :form form
                                :detail argform
                                (TEXT "The argument ~S to ~S should be a keyword.")
                                argform (car access-form)
                            ) )
                            (let ((tempvar (gensym)))
                              (push tempvar tempvars)
                              (push argform tempforms)
                              (push tempvar new-access-form)
                          ) )
                          (incf i)
                      ) )
                      (setq new-access-form (nreverse new-access-form))
                      (let ((newval-vars (gensym-list (cadr plist-info))))
                        (values
                          (nreverse tempvars)
                          (nreverse tempforms)
                          newval-vars
                          (apply (cddr plist-info) env (append newval-vars new-access-form))
                          (cons (car access-form) new-access-form)
                ) ) ) ) )
            ) )
    ) ) ) )
    ; 2. Schritt: macroexpandieren
    (when (eq form (setq form (macroexpand-1 form env)))
      (return)
  ) )
  ; 3. Schritt: Default-SETF-Methoden
  (cond ((symbolp form)
         (return-from get-setf-expansion
           (let ((storevar (gensym)))
             (values nil
                     nil
                     `(,storevar)
                     `(SETQ ,form ,storevar)
                     `,form
        )) ) )
        ((and (consp form) (symbolp (car form)))
         (return-from get-setf-expansion
           (do* ((storevar (gensym))
                 (tempvars nil (cons (gensym) tempvars))
                 (tempforms nil (cons (car formr) tempforms))
                 (formr (cdr form) (cdr formr)))
                ((endp formr)
                 (setq tempforms (nreverse tempforms))
                 (values tempvars
                         tempforms
                         `(,storevar)
                         ;; this is identical to CLISP-specific
                         ;; ((SETF ,(first form)) ,storevar ,@tempvars)
                         ;; but the we will return the form
                         ;; that will not confuse 3rd party code walkers
                         `(FUNCALL #'(SETF ,(first form)) ,storevar ,@tempvars)
                         `(,(first form) ,@tempvars)
                ))
        )) )
        (t (error-of-type 'source-program-error
             :form form
             :detail form
             (TEXT "~S: Argument ~S is not a SETF place.")
             'get-setf-expansion form))))
;;;----------------------------------------------------------------------------
(defun get-setf-method (form &optional (env (vector nil nil)))
  (multiple-value-bind (temps subforms stores setterform getterform)
      (get-setf-expansion form env)
    (unless (and (consp stores) (null (cdr stores)))
      (error-of-type 'source-program-error
        :form form
        :detail form
        (TEXT "SETF place ~S should produce exactly one store variable.")
        form
    ) )
    (values temps subforms stores setterform getterform)
) )
;;;----------------------------------------------------------------------------
;; Auxiliary functions for simplifying bindings and setterforms.

; Like (subst newitem olditem form), except that it works on forms and
; doesn't look inside quoted literals.
; FIXME: This is still not correct: The form can contain macros or THE.
(defun subst-in-form (newitem olditem form)
  ; Don't use subst here, since a form can contain circular lists hidden behind
  ; QUOTE.
  (if (atom form)
    (if (eql form olditem) newitem form)
    (if (eq (car form) 'QUOTE)
      form
      (let ((modified nil)
            (new-form-reversed '()))
        (do ((formr form (cdr formr)))
            ((atom formr)
             (unless (eql formr (setq formr (subst-in-form newitem olditem formr)))
               (setq modified t))
             (if modified (nreconc new-form-reversed formr) form))
          (let ((new-subform (subst-in-form newitem olditem (car formr))))
            (unless (eql (car formr) new-subform)
              (setq modified t))
            (setq new-form-reversed (cons new-subform new-form-reversed))))))))

; Like (sublis alist form), except that it works on forms and
; doesn't look inside quoted literals.
; FIXME: This is still not correct: The form can contain macros or THE.
(defun sublis-in-form (alist form)
  ; Don't use sublis here, since a form can contain circular lists hidden behind
  ; QUOTE.
  (if (atom form)
    (let ((h (assoc form alist))) (if h (cdr h) form))
    (if (eq (car form) 'QUOTE)
      form
      (let ((modified nil)
            (new-form-reversed '()))
        (do ((formr form (cdr formr)))
            ((atom formr)
             (unless (eql formr (setq formr (sublis-in-form alist formr)))
               (setq modified t))
             (if modified (nreconc new-form-reversed formr) form))
          (let ((new-subform (sublis-in-form alist (car formr))))
            (unless (eql (car formr) new-subform)
              (setq modified t))
            (setq new-form-reversed (cons new-subform new-form-reversed))))))))

; An empty binding list can be optimized away.
(defun wrap-let* (bindlist form)
  (if (and (null bindlist)
           ; But don't optimize the LET* away if the form is a PROGN form,
           ; because when it occurs as a top-level form in a file and refers
           ; to uninterned symbols, compiling the elements of the PROGN
           ; separately leads to problems.
           (not (and (consp form) (eq (first form) 'PROGN))))
    form
    `(LET* ,bindlist ,form)))

; In simple assignments like (SETQ foo #:G0) the #:G0 can be replaced directly.
(defun simple-assignment-p (env store-form stores)
  (and (= (length stores) 1)
       (consp store-form)
       (eq (first store-form) 'SETQ)
       (= (length store-form) 3)
       (symbolp (second store-form))
       (not (nth-value 1 (macroexpand-1 (second store-form) env)))
       (simple-use-p (third store-form) (first stores))
) )
(defun simple-use-p (form var)
  (or (eq form var)
      (and (consp form) (eq (first form) 'THE) (= (length form) 3)
           (simple-use-p (third form) var)
) )   )

; Tests whether a variable (a gensym) occurs in the given form.
; FIXME: This is still not correct: The form can contain macros or THE.
(defun occurs-in-p (form var)
  ; Don't use (tree-equal form form ...) here, since a form can contain
  ; circular lists hidden behind QUOTE.
  (if (atom form)
    (eq form var)
    (if (eq (car form) 'QUOTE)
      nil
      (do ((formr form (cdr formr)))
          ((atom formr) (eq formr var))
        (when (occurs-in-p (car formr) var) (return t))))))

; Tests whether two forms are guaranteed to commute. The first is assumed to
; be a symbol, the second one can be more complex.
(defun commuting-forms-p (var form env)
  (and (symbolp var)
       (not (nth-value 1 (macroexpand-1 var env)))
       (not (nth-value 1 (macroexpand-1 form env)))
       (if (atom form)
         (not (eq form var))
         (let ((funname (first form))
               (argforms (rest form)))
           (and (function-name-p funname) ; we don't handle LAMBDAs here
                (if (symbolp funname)
                  (and (not (special-operator-p funname))
                       (null (macro-function funname env)))
                  t)
                (not (compiler::fenv-search funname (and env (svref env 1))))
                (every #'(lambda (argform) (commuting-forms-p var argform env))
                       argforms))))))
; For bootstrapping.
(predefun compiler::fenv-search (funname fenv)
  (declare (ignore funname fenv))
  nil)

; In simple function calls like (SYSTEM::%RPLACA foo #:G0) the #:G0 can be
; replaced directly if it occurs only once, as an argument, and the earlier
; arguments commute with the value.
(defun simple-occurrence-in-basic-block-p (env form var valform)
  (if (atom form)
    (eq form var)
    (case (first form)
      (SETQ
        (and (= (length form) 3)
             (symbolp (second form))
             (not (nth-value 1 (macroexpand-1 (second form) env)))
             (not (eq (second form) var))
             (simple-occurrence-in-basic-block-p env (third form) var valform)))
      (THE
        (and (= (length form) 3)
             (simple-occurrence-in-basic-block-p env (third form) var valform)))
      (t
        (let ((funname (first form))
              (argforms (rest form)))
          (and (function-name-p funname) ; we don't handle LAMBDAs here
               (if (symbolp funname)
                 (and (not (special-operator-p funname))
                      (null (macro-function funname env)))
                 t)
               ; At this point we know it's a function call.
               ; We assume the value to be put in for var does not change
               ; funname's function definition,
               (do ((earlier-argforms (reverse argforms) (cdr earlier-argforms)))
                   ((null earlier-argforms) nil)
                 (when (occurs-in-p (car earlier-argforms) var)
                   ; Found the last argument form that refers to var.
                   (return
                     (and (simple-occurrence-in-basic-block-p env (car earlier-argforms) var valform)
                          (every #'(lambda (argform)
                                     (and (symbolp argform)
                                          (not (nth-value 1 (macroexpand-1 argform env)))
                                          (not (ext:special-variable-p argform env))
                                          (not (eq argform var))
                                          (commuting-forms-p argform valform env)))
                                 (cdr earlier-argforms))))))))))))

(defun optimized-wrap-let* (env bindlist form) ; ABI
  (if (null bindlist)
    form
    (let* ((last-binding (car (last bindlist)))
           (last-var (first last-binding))
           (last-valform (second last-binding)))
      (if (simple-occurrence-in-basic-block-p env form last-var last-valform)
        (optimized-wrap-let* env (butlast bindlist)
          (subst-in-form last-valform last-var form))
        (wrap-let* bindlist form)))))

(defun optimized-wrap-multiple-value-bind (env varlist valuesform form)
  (cond ((null varlist)
         `(PROGN ,valuesform ,form))
        ((null (cdr varlist))
         (optimized-wrap-let* env (list (list (car varlist) valuesform)) form))
        (t `(MULTIPLE-VALUE-BIND ,varlist ,valuesform ,form))))

;;;----------------------------------------------------------------------------
(defmacro push (item place &environment env)
  (multiple-value-bind (temps subforms stores setterform getterform)
      (get-setf-expansion place env)
    (let ((itemtemps (gensym-list (length stores)))
          (bindlist (mapcar #'list temps subforms))
          (oldtemps (gensym-list (length stores))))
      (optimized-wrap-multiple-value-bind env itemtemps item
        (wrap-let* bindlist
          (optimized-wrap-multiple-value-bind env oldtemps getterform
            ;; We're not blindly optimizing this to
            ;;   (sublis-in-form
            ;;     (mapcar #'(lambda (storevar itemvar oldvar)
            ;;                 (cons storevar `(CONS ,itemvar ,oldvar)))
            ;;             stores itemtemps oldtemps)
            ;;     setterform)
            ;; because we don't want the CONS forms to be evaluated multiple
            ;; times. Instead we rely on simple-occurrence-in-basic-block-p for
            ;; doing the analysis.
            (optimized-wrap-let* env
              (mapcar #'(lambda (storevar itemvar oldvar)
                          (list storevar `(CONS ,itemvar ,oldvar)))
                      stores itemtemps oldtemps)
              setterform)))))))
;;;----------------------------------------------------------------------------
(eval-when (load compile eval)
  (defun check-accessor-name (accessfn whole-form)
    (unless (symbolp accessfn)
      (error-of-type 'source-program-error
        :form whole-form
        :detail accessfn
        (TEXT "The name of the accessor must be a symbol, not ~S")
        accessfn))))
(defmacro define-setf-expander (&whole whole-form
                                accessfn lambdalist &body body)
  (check-accessor-name accessfn whole-form)
  (multiple-value-bind (body-rest declarations docstring)
      (system::parse-body body t)
    (if (null body-rest) (setq body-rest '(NIL)))
    (let ((name (make-symbol (string-concat "SETF-" (symbol-name accessfn))))
          (SYSTEM::%WHOLE-FORM whole-form))
      (multiple-value-bind (newlambdalist envvar)
          (remove-env-arg lambdalist name)
        (let ((SYSTEM::%ARG-COUNT 0)
              (SYSTEM::%MIN-ARGS 0)
              (SYSTEM::%RESTP nil)
              (SYSTEM::%NULL-TESTS nil)
              (SYSTEM::%LET-LIST nil)
              (SYSTEM::%KEYWORD-TESTS nil)
              (SYSTEM::%DEFAULT-FORM nil)
             )
          (SYSTEM::ANALYZE1 newlambdalist '(CDR SYSTEM::%LAMBDA-LIST)
                            name 'SYSTEM::%LAMBDA-LIST
          )
          (if (null newlambdalist)
            (push `(IGNORE SYSTEM::%LAMBDA-LIST) declarations)
          )
          (let ((lengthtest (sys::make-length-test 'SYSTEM::%LAMBDA-LIST 1 nil))
                (mainform
                  `(LET* ,(nreverse SYSTEM::%LET-LIST)
                     ,@(if declarations `(,(cons 'DECLARE declarations)))
                     ,@(nreverse SYSTEM::%NULL-TESTS)
                     ,@(nreverse SYSTEM::%KEYWORD-TESTS)
                     (BLOCK ,accessfn ,@body-rest)
                   )
               ))
            (if lengthtest
              (setq mainform
                `(IF ,lengthtest
                   (ERROR-OF-TYPE 'PROGRAM-ERROR
                     (TEXT "The SETF expander for ~S may not be called with ~S arguments.")
                     (QUOTE ,accessfn) (1- (LENGTH SYSTEM::%LAMBDA-LIST))
                   )
                   ,mainform
              )  )
            )
            `(EVAL-WHEN (LOAD COMPILE EVAL)
               (LET ()
                 (REMPROP ',accessfn 'SYSTEM::DEFSTRUCT-WRITER)
                 (DEFUN ,name (SYSTEM::%LAMBDA-LIST ,(or envvar 'SYSTEM::ENV))
                   ,@(if envvar '() '((DECLARE (IGNORE SYSTEM::ENV))))
                   ,mainform
                 )
                 (sys::check-redefinition
                  ',accessfn 'define-setf-expander
                  (and (get ',accessfn 'SYSTEM::SETF-EXPANDER)
                       'SYSTEM::SETF-EXPANDER))
                 (SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-EXPANDER
                   (CONS -5 (FUNCTION ,name))
                 )
                 (SYSTEM::%SET-DOCUMENTATION ',accessfn 'SETF ',docstring)
                 ',accessfn
             ) )
) ) ) ) ) )
;;;----------------------------------------------------------------------------
(defmacro defsetf (&whole whole-form
                   accessfn &rest args)
  (check-accessor-name accessfn whole-form)
  (cond ((and (consp args) (not (listp (first args))) (symbolp (first args)))
         `(EVAL-WHEN (LOAD COMPILE EVAL)
            (LET ()
              (REMPROP ',accessfn 'SYSTEM::DEFSTRUCT-WRITER)
              (SYS::CHECK-REDEFINITION
               ',accessfn 'DEFSETF
               (and (get ',accessfn 'SYSTEM::SETF-EXPANDER)
                    'SYSTEM::SETF-EXPANDER))
              (SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-EXPANDER ',(first args))
              (SYSTEM::%SET-DOCUMENTATION ',accessfn 'SETF
                ,(if (and (null (cddr args))
                          (or (null (second args)) (stringp (second args)))
                     )
                   (second args)
                   (if (cddr args)
                     (error-of-type 'source-program-error
                       :form whole-form
                       :detail (cdr args)
                       (TEXT "~S: Too many arguments: ~S")
                       'defsetf (cdr args))
                     (error-of-type 'source-program-error
                       :form whole-form
                       :detail (second args)
                       (TEXT "~S: The documentation string must be a string: ~S")
                       'defsetf (second args))))
              )
              ',accessfn
          ) )
        )
        ((and (consp args) (listp (first args)) (consp (cdr args)) (listp (second args)))
         (multiple-value-bind (body-rest declarations docstring)
             (system::parse-body (cddr args) t)
           (let* ((storevars (second args))
                  arg-count
                  (setter
                    (let ((lambdalist (first args)))
                      (multiple-value-bind (reqvars optvars optinits optsvars rest
                                            keyp keywords keyvars keyinits keysvars
                                            allowp env)
                          (analyze-defsetf-lambdalist lambdalist
                            #'(lambda (detail errorstring &rest arguments)
                                (error-of-type 'source-program-error
                                  :form whole-form
                                  :detail detail
                                  (TEXT "~S ~S: invalid ~S lambda-list: ~A")
                                  'defsetf accessfn 'defsetf
                                  (apply #'format nil errorstring arguments))))
                        (declare (ignore optinits optsvars rest keywords keyvars
                                         keyinits keysvars allowp))
                        (setq arg-count (if keyp (+ (length reqvars) (length optvars)) -1))
                        (if (eql env 0)
                          (setq env (gensym)
                                declarations (cons `(IGNORE ,env) declarations))
                          (setq lambdalist
                                (let ((lr (memq '&ENVIRONMENT lambdalist)))
                                  (append (ldiff lambdalist lr) (cddr lr)))))
                        (when declarations (setq declarations `((DECLARE ,@declarations))))
                        `(LAMBDA (,env ,@storevars ,@lambdalist)
                           ,@declarations
                           (BLOCK ,accessfn ,@body-rest)
                         )
                 )) ) )
             `(EVAL-WHEN (LOAD COMPILE EVAL)
                (LET ()
                  (REMPROP ',accessfn 'SYSTEM::DEFSTRUCT-WRITER)
                  (SYS::CHECK-REDEFINITION
                    ',accessfn 'DEFSETF
                    (AND (GET ',accessfn 'SYSTEM::SETF-EXPANDER)
                         'SYSTEM::SETF-EXPANDER))
                  (SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-EXPANDER
                    (LIST* ,arg-count ,(length storevars)
                           (FUNCTION ,(concat-pnames "SETF-" accessfn) ,setter)
                  ) )
                  (SYSTEM::%SET-DOCUMENTATION ',accessfn 'SETF ,docstring)
                  ',accessfn
              ) )
        )) )
        (t (error-of-type 'source-program-error
             :form whole-form
             :detail args
             (TEXT "(~S ~S): Illegal syntax.")
             'defsetf accessfn))))
;;;----------------------------------------------------------------------------
;; Redirects #'(SETF accessfn) to be the same as setterfn.
(defmacro def-setf-alias (accessfn setterfn)
  `(SYSTEM::%PUT ',accessfn 'SYSTEM::SETF-FUNCTION ',setterfn))
;;;----------------------------------------------------------------------------
;;; Definition of places:
;;;----------------------------------------------------------------------------
(def-setf-alias system::package-documentation SYSTEM::|(SETF PACKAGE-DOCUMENTATION)|)
(def-setf-alias package-case-inverted-p SYSTEM::|(SETF PACKAGE-CASE-INVERTED-P)|)
(def-setf-alias package-case-sensitive-p SYSTEM::|(SETF PACKAGE-CASE-SENSITIVE-P)|)
(def-setf-alias package-lock SYSTEM::|(SETF PACKAGE-LOCK)|)
(def-setf-alias hash-table-weak-p SYSTEM::|(SETF HASH-TABLE-WEAK-P)|)
(def-setf-alias hash-table-warn-if-needs-rehash-after-gc SYSTEM::|(SETF HASH-TABLE-WARN-IF-NEEDS-REHASH-AFTER-GC)|)
(def-setf-alias weak-pointer-value SYSTEM::|(SETF WEAK-POINTER-VALUE)|)
(def-setf-alias weak-list-list SYSTEM::|(SETF WEAK-LIST-LIST)|)
(def-setf-alias weak-mapping-value SYSTEM::|(SETF WEAK-MAPPING-VALUE)|)
(def-setf-alias weak-and-mapping-value SYSTEM::|(SETF WEAK-AND-MAPPING-VALUE)|)
(def-setf-alias weak-or-mapping-value SYSTEM::|(SETF WEAK-OR-MAPPING-VALUE)|)
(def-setf-alias weak-alist-contents SYSTEM::|(SETF WEAK-ALIST-CONTENTS)|)
(def-setf-alias weak-alist-value SYSTEM::|(SETF WEAK-ALIST-VALUE)|)
;;;----------------------------------------------------------------------------
(defsetf aref (array &rest indices) (value)
  `(SYSTEM::STORE ,array ,@indices ,value))
;;;----------------------------------------------------------------------------
(defun SYSTEM::%SETNTH (index list value) ; ABI
  (let ((pointer (nthcdr index list)))
    (if (null pointer)
      (error-of-type 'error
        (TEXT "~S: index ~S is too large for ~S")
        '(setf nth) index list)
      (rplaca pointer value)
    )
    value
) )
(defsetf nth SYSTEM::%SETNTH)
;;;----------------------------------------------------------------------------
(def-setf-alias elt SYSTEM::|(SETF ELT)|)
;;;----------------------------------------------------------------------------
(defsetf rest SYSTEM::%RPLACD)
(defsetf first SYSTEM::%RPLACA)
(defsetf second (list) (value) `(SYSTEM::%RPLACA (CDR ,list) ,value))
(defsetf third (list) (value) `(SYSTEM::%RPLACA (CDDR ,list) ,value))
(defsetf fourth (list) (value) `(SYSTEM::%RPLACA (CDDDR ,list) ,value))
(defsetf fifth (list) (value) `(SYSTEM::%RPLACA (CDDDDR ,list) ,value))
(defsetf sixth (list) (value) `(SYSTEM::%RPLACA (CDR (CDDDDR ,list)) ,value))
(defsetf seventh (list) (value) `(SYSTEM::%RPLACA (CDDR (CDDDDR ,list)) ,value))
(defsetf eighth (list) (value) `(SYSTEM::%RPLACA (CDDDR (CDDDDR ,list)) ,value))
(defsetf ninth (list) (value) `(SYSTEM::%RPLACA (CDDDDR (CDDDDR ,list)) ,value))
(defsetf tenth (list) (value) `(SYSTEM::%RPLACA (CDR (CDDDDR (CDDDDR ,list))) ,value))

(defsetf car SYSTEM::%RPLACA)
(defsetf cdr SYSTEM::%RPLACD)
(defsetf caar (list) (value) `(SYSTEM::%RPLACA (CAR ,list) ,value))
(defsetf cadr (list) (value) `(SYSTEM::%RPLACA (CDR ,list) ,value))
(defsetf cdar (list) (value) `(SYSTEM::%RPLACD (CAR ,list) ,value))
(defsetf cddr (list) (value) `(SYSTEM::%RPLACD (CDR ,list) ,value))
(defsetf caaar (list) (value) `(SYSTEM::%RPLACA (CAAR ,list) ,value))
(defsetf caadr (list) (value) `(SYSTEM::%RPLACA (CADR ,list) ,value))
(defsetf cadar (list) (value) `(SYSTEM::%RPLACA (CDAR ,list) ,value))
(defsetf caddr (list) (value) `(SYSTEM::%RPLACA (CDDR ,list) ,value))
(defsetf cdaar (list) (value) `(SYSTEM::%RPLACD (CAAR ,list) ,value))
(defsetf cdadr (list) (value) `(SYSTEM::%RPLACD (CADR ,list) ,value))
(defsetf cddar (list) (value) `(SYSTEM::%RPLACD (CDAR ,list) ,value))
(defsetf cdddr (list) (value) `(SYSTEM::%RPLACD (CDDR ,list) ,value))
(defsetf caaaar (list) (value) `(SYSTEM::%RPLACA (CAAAR ,list) ,value))
(defsetf caaadr (list) (value) `(SYSTEM::%RPLACA (CAADR ,list) ,value))
(defsetf caadar (list) (value) `(SYSTEM::%RPLACA (CADAR ,list) ,value))
(defsetf caaddr (list) (value) `(SYSTEM::%RPLACA (CADDR ,list) ,value))
(defsetf cadaar (list) (value) `(SYSTEM::%RPLACA (CDAAR ,list) ,value))
(defsetf cadadr (list) (value) `(SYSTEM::%RPLACA (CDADR ,list) ,value))
(defsetf caddar (list) (value) `(SYSTEM::%RPLACA (CDDAR ,list) ,value))
(defsetf cadddr (list) (value) `(SYSTEM::%RPLACA (CDDDR ,list) ,value))
(defsetf cdaaar (list) (value) `(SYSTEM::%RPLACD (CAAAR ,list) ,value))
(defsetf cdaadr (list) (value) `(SYSTEM::%RPLACD (CAADR ,list) ,value))
(defsetf cdadar (list) (value) `(SYSTEM::%RPLACD (CADAR ,list) ,value))
(defsetf cdaddr (list) (value) `(SYSTEM::%RPLACD (CADDR ,list) ,value))
(defsetf cddaar (list) (value) `(SYSTEM::%RPLACD (CDAAR ,list) ,value))
(defsetf cddadr (list) (value) `(SYSTEM::%RPLACD (CDADR ,list) ,value))
(defsetf cdddar (list) (value) `(SYSTEM::%RPLACD (CDDAR ,list) ,value))
(defsetf cddddr (list) (value) `(SYSTEM::%RPLACD (CDDDR ,list) ,value))
;;;----------------------------------------------------------------------------
(defsetf svref SYSTEM::SVSTORE)
(defsetf row-major-aref system::row-major-store)
;;;----------------------------------------------------------------------------
;; Simplify a form, when its values are not needed, only its side effects.
;; Returns a list of subforms.
;;   (values x y z) --> (x y z)
;;   x --> (x)
(defun devalue-form (form)
  (if (eq (car form) 'VALUES) (cdr form) (list form))
)
;;;----------------------------------------------------------------------------
(defmacro pop (place &environment env)
  (multiple-value-bind (temps subforms stores setterform getterform)
      (get-setf-expansion place env)
    ;; Be sure to call the CARs before the CDRs - it matters in case
    ;; not all of the places evaluate to lists.
    (let* ((bindlist (mapcar #'list temps subforms))
           (oldtemps (gensym-list (length stores)))
           (advance-and-set-form
             ;; We're not blindly optimizing this to
             ;;   (sublis-in-form
             ;;     (mapcar #'(lambda (storevar oldvar) (cons storevar `(CDR ,oldvar)))
             ;;             stores oldtemps)
             ;;     setterform)
             ;; because some part of the setterform could make side-effects that
             ;; affect the value of the CDRs. Instead we rely on
             ;; simple-occurrence-in-basic-block-p for doing the analysis.
             (optimized-wrap-let* env
               (mapcar #'(lambda (storevar oldvar)
                           (list storevar `(CDR ,oldvar)))
                       stores oldtemps)
               setterform)))
      (if (= (length stores) 1)
        (let ((prog1-form
                `(PROG1
                   (CAR ,(car oldtemps))
                   ,@(devalue-form advance-and-set-form))))
          (if (and (symbolp getterform)
                   (not (nth-value 1 (macroexpand-1 getterform env)))
                   (simple-occurrence-in-basic-block-p env advance-and-set-form
                     (car oldtemps) getterform))
            ;; getterform can be evaluated multiple times instead of once, and
            ;; nothing in the setterform interferes with its value. => Optimize
            ;; away the binding of the oldtemps.
            (optimized-wrap-let* env bindlist
              (subst-in-form getterform (car oldtemps)
                prog1-form))
            (optimized-wrap-let* env (nconc bindlist (list (list (car oldtemps) getterform)))
              prog1-form)))
        (optimized-wrap-let* env bindlist
          (optimized-wrap-multiple-value-bind env oldtemps getterform
            `(MULTIPLE-VALUE-PROG1
               (VALUES ,@(mapcar #'(lambda (oldvar) `(CAR ,oldvar)) oldtemps))
               ,@(devalue-form advance-and-set-form))))))))
;----------------------------------------------------------------------------
(defmacro psetf (&whole whole-form
                 &rest args &environment env)
  (labels ((recurse (args)
             (multiple-value-bind (temps subforms stores setterform getterform)
                 (get-setf-expansion (car args) env)
               (declare (ignore getterform))
               (when (atom (cdr args))
                 (error-of-type 'source-program-error
                   :form whole-form
                   :detail whole-form
                   (TEXT "~S called with an odd number of arguments: ~S")
                   'psetf whole-form))
               (wrap-let* (mapcar #'list temps subforms)
                 `(MULTIPLE-VALUE-BIND ,stores ,(second args)
                    ,@(when (cddr args) (list (recurse (cddr args))))
                    ,@(devalue-form setterform))))))
    (when args `(,@(recurse args) NIL))))
;;;----------------------------------------------------------------------------
(defmacro pushnew (item place &rest keylist &environment env)
  (multiple-value-bind (temps subforms stores setterform getterform)
      (get-setf-expansion place env)
    (let ((itemtemps (gensym-list (length stores)))
          (bindlist (mapcar #'list temps subforms))
          (oldtemps (gensym-list (length stores))))
      (optimized-wrap-multiple-value-bind env itemtemps item
        (wrap-let* bindlist
          (optimized-wrap-multiple-value-bind env oldtemps getterform
            ;; We're not blindly optimizing this to
            ;;   (sublis-in-form
            ;;     (mapcar #'(lambda (storevar itemvar oldvar)
            ;;                 (cons storevar `(ADJOIN ,itemvar ,oldvar ,@keylist)))
            ;;             stores itemtemps oldtemps)
            ;;     setterform)
            ;; because we don't want the ADJOIN forms to be evaluated multiple
            ;; times. Instead we rely on simple-occurrence-in-basic-block-p for
            ;; doing the analysis.
            (optimized-wrap-let* env
              (mapcar #'(lambda (storevar itemvar oldvar)
                          (list storevar `(ADJOIN ,itemvar ,oldvar ,@keylist)))
                      stores itemtemps oldtemps)
              setterform)))))))
;;;----------------------------------------------------------------------------
(defmacro remf (place indicator &environment env)
  (multiple-value-bind (temps subforms stores setterform getterform)
      (get-setf-method place env)
    (let* ((indicatorvar (gensym))
           (oldtemps (gensym-list (length stores)))
           (bindlist
             ;; The order of the bindings is a not strictly left-to-right here,
             ;; but that's how ANSI CL 5.1.3 specifies it.
             `(,@(mapcar #'list temps subforms)
               (,indicatorvar ,indicator)
               (,(first oldtemps) ,getterform)))
           (removed-p (gensym)))
      (wrap-let* bindlist
        `(MULTIPLE-VALUE-BIND (,(first stores) ,removed-p)
             (SYSTEM::%REMF ,(first oldtemps) ,indicatorvar)
           (WHEN (AND ,removed-p (ATOM ,(first stores)))
             ,setterform)
           ,removed-p)))))
;;;----------------------------------------------------------------------------
(export 'ext::remove-plist "EXT")
;; Remove the keys from the plist.
;; Useful for re-using the &REST arg after removing some options.
(defun remove-plist (plist &rest keys)
  ;; This implementation is
  ;; 1. minimal-consing, non-consing if possible,
  ;; 2. O(L*K) where L = (length plist), K = (length keys).
  ;; Remove the first occurring key first, then the second occurring key, etc.
  ;; This allows us to use the built-in GET-PROPERTIES function.
  ;; Another O(L*K) algorithm is remove the keys in the order in which they
  ;; occur in keys, keeping track how much of the list has already been copied.
  (do ((copy '()))
      (nil)
    (let ((rest (nth-value 2 (get-properties plist keys))))
      (unless rest (return (nreconc copy plist)))
      (setq copy (nreconc (ldiff plist rest) copy))
      (setq plist (cddr rest)))))
;;;----------------------------------------------------------------------------
(defmacro rotatef (&rest args &environment env)
  (when (null args) (return-from rotatef NIL))
  (when (null (cdr args)) (return-from rotatef `(PROGN ,(car args) NIL)))
  (do* ((arglist args (cdr arglist))
        (res (list 'LET* nil nil))
        last-setterform
        (tail (cdr res))
        (bindlist '())
        (all-stores '())
        last-stores
        first-stores)
       ((null arglist)
        (setf (second res) (nreverse bindlist)
              (second (third res)) last-stores
              (cdr tail) (nconc (nreverse all-stores) (devalue-form last-setterform))
              (cdr (last res)) (list nil))
        res)
    (multiple-value-bind (temps subforms stores setterform getterform)
        (get-setf-expansion (first arglist) env)
      (setq bindlist (nreconc (mapcar #'list temps subforms) bindlist))
      (setf (cadr tail) (list 'MULTIPLE-VALUE-BIND last-stores getterform nil))
      (setq tail (cddadr tail))
      (if (null first-stores)
        (setq first-stores stores)
        (setq all-stores (revappend (devalue-form last-setterform) all-stores))
      )
      (setq last-stores stores last-setterform setterform))))
;;;----------------------------------------------------------------------------
(defmacro define-modify-macro (&whole whole-form
                               name lambdalist function &optional docstring)
  (multiple-value-bind (reqvars optvars optinits optsvars rest)
      (analyze-modify-macro-lambdalist lambdalist
        #'(lambda (detail errorstring &rest arguments)
            (error-of-type 'source-program-error
              :form whole-form
              :detail detail
              (TEXT "~S ~S: invalid ~S lambda-list: ~A")
              'define-modify-macro name 'define-modify-macro
              (apply #'format nil errorstring arguments))))
    (declare (ignore optinits optsvars))
    (let ((varlist (append reqvars optvars))
          (restvar (if (not (eql rest 0)) rest nil)))
      `(DEFMACRO ,name (PLACE ,@lambdalist &ENVIRONMENT ENV) ,docstring
         (MULTIPLE-VALUE-BIND (TEMPS SUBFORMS STORES SETTERFORM GETTERFORM)
             (GET-SETF-METHOD PLACE ENV)
           ;; ANSI CL 5.1.3. mandates the following evaluation order:
           ;; First the SUBFORMS,
           ;; then the varlist and restvar, then the GETTERFORM,
           ;; then the SETTERFORM.
           (LET ((LET-LIST (MAPCAR #'LIST TEMPS SUBFORMS)))
             (IF (AND ,@(mapcar #'(lambda (var) `(CONSTANTP ,var)) varlist)
                      ,@(when restvar `((EVERY #'CONSTANTP ,restvar))))
               ;; The varlist and restvar forms are constant forms, therefore
               ;; may be evaluated after the GETTER instead of before.
               (LET ((FUNCTION-APPLICATION
                       (LIST* ',function GETTERFORM ,@varlist ,restvar)))
                 (OPTIMIZED-WRAP-LET* ENV
                   (NCONC LET-LIST
                          (LIST (LIST (CAR STORES) FUNCTION-APPLICATION)))
                   SETTERFORM))
               ;; General case.
               (LET* ((ARGVARS
                        (MAPCAR #'(LAMBDA (VAR) (DECLARE (IGNORE VAR)) (GENSYM))
                                (LIST* ,@varlist ,restvar)))
                      (FUNCTION-APPLICATION
                        (LIST* ',function GETTERFORM ARGVARS)))
                 (OPTIMIZED-WRAP-LET* ENV
                   (NCONC LET-LIST
                          (MAPCAR #'LIST ARGVARS (LIST* ,@varlist ,restvar))
                          (LIST (LIST (CAR STORES) FUNCTION-APPLICATION)))
                   SETTERFORM))
       ) ) ) )
) ) )
;;;----------------------------------------------------------------------------
(define-modify-macro decf (&optional (delta 1)) -)
;;;----------------------------------------------------------------------------
(define-modify-macro incf (&optional (delta 1)) +)
;;;----------------------------------------------------------------------------
(defmacro setf (&whole whole-form
                &rest args &environment env)
  (let ((argcount (length args)))
    (cond ((eql argcount 2)
           (let* ((place (first args))
                  (value (second args)))
             (loop
               ;; 1. Schritt: nach globalen SETF-Definitionen suchen:
               (when (and (consp place) (symbolp (car place)))
                 (when (global-in-fenv-p (car place) (svref env 1))
                   ; Operator nicht lokal definiert
                   (if (and (eq (first place) 'VALUES-LIST) (eql (length place) 2))
                     (return-from setf
                       `(VALUES-LIST
                          (SETF ,(second place) (MULTIPLE-VALUE-LIST ,value))
                        )
                     )
                     (let ((plist-info (get (first place) 'SYSTEM::SETF-EXPANDER)))
                       (when plist-info
                         (return-from setf
                           (cond ((symbolp plist-info) ; Symbol kommt von kurzem DEFSETF
                                  `(,plist-info ,@(cdr place) ,value)
                                 )
                                 ((and (eq (first place) 'THE) (eql (length place) 3))
                                  `(SETF ,(third place) (THE ,(second place) ,value))
                                 )
                                 (t
                                  (multiple-value-bind (temps subforms stores setterform getterform)
                                      (get-setf-expansion place env)
                                    (declare (ignore getterform))
                                    (let ((bindlist (mapcar #'list temps subforms)))
                                      (if (= (length stores) 1)
                                        ;; 1 store variable
                                        (wrap-let* (nconc bindlist
                                                     (list `(,(first stores) ,value))
                                                   )
                                          setterform
                                        )
                                        ;; mehrere Store-Variable
                                        (if ;; Hat setterform die Gestalt
                                            ;; (VALUES (SETQ v1 store1) ...) ?
                                          (and (consp setterform)
                                               (eq (car setterform) 'VALUES)
                                               (do ((str stores (cdr str))
                                                    (sqr (cdr setterform) (cdr sqr)))
                                                   ((or (null str) (null sqr))
                                                    (and (null str) (null sqr)))
                                                 (unless (simple-assignment-p env (car sqr) (list (car str)))
                                                   (return nil)
                                          )    ) )
                                          (let ((vlist (mapcar #'second (rest setterform))))
                                            `(LET* ,bindlist
                                               (MULTIPLE-VALUE-SETQ ,vlist ,value)
                                               (VALUES ,@vlist)
                                             )
                                          )
                                          (wrap-let* bindlist
                                            `(MULTIPLE-VALUE-BIND ,stores ,value
                                               ,setterform
                                          )  )
                                 )) ) ) )
               ) ) ) ) ) ) )
               ;; 2. Schritt: macroexpandieren
               (when (eq place (setq place (macroexpand-1 place env)))
                 (return)
             ) )
             ;; 3. Schritt: Default-SETF-Methoden
             (cond ((symbolp place)
                    `(SETQ ,place ,value)
                   )
                   ((and (consp place) (symbolp (car place)))
                    (multiple-value-bind (temps subforms stores setterform getterform)
                        (get-setf-expansion place env)
                      (declare (ignore getterform))
                      ; setterform probably looks like
                      ;   `((SETF ,(first place)) ,@stores ,@temps).
                      ; stores are probably superfluous and get optimized away.
                      (optimized-wrap-let* env
                        (nconc (mapcar #'list temps subforms)
                               (list (list (first stores) value)))
                        setterform
                      )
                   ))
                   (t (error-of-type 'source-program-error
                        :form whole-form
                        :detail (first args)
                        (TEXT "~S: Illegal place: ~S")
                        'setf (first args))))))
          ((oddp argcount)
           (error-of-type 'source-program-error
             :form whole-form
             :detail whole-form
             (TEXT "~S called with an odd number of arguments: ~S")
             'setf whole-form))
          (t (do* ((arglist args (cddr arglist))
                   (L nil))
                  ((null arglist) `(LET () (PROGN ,@(nreverse L))))
               (push `(SETF ,(first arglist) ,(second arglist)) L)
          )  )
) ) )
;;;----------------------------------------------------------------------------
(defmacro shiftf (&whole whole-form
                  &rest args &environment env)
  (when (< (length args) 2)
    (error-of-type 'source-program-error
      :form whole-form
      :detail args
      (TEXT "~S: too few arguments: ~S")
      'shiftf whole-form))
  (do* ((arglist args (cdr arglist))
        (res (list 'LET* nil nil))
        last-setterform
        first-getterform
        (tail (cdr res))
        (bindlist '())
        (all-stores '())
        last-stores
        first-stores)
       ((null (cdr arglist))
        (setf (second res) (nreverse bindlist)
              (cadr tail) (list 'MULTIPLE-VALUE-BIND last-stores (car (last args)) nil)
              tail (cddadr tail)
              (cdr tail) (nconc (nreverse all-stores) (devalue-form last-setterform))
              (third res) (list 'MULTIPLE-VALUE-BIND first-stores first-getterform (third res)
                                (cons 'values first-stores)))
        res)
    (multiple-value-bind (temps subforms stores setterform getterform)
        (get-setf-expansion (first arglist) env)
      (setq bindlist (nreconc (mapcar #'list temps subforms) bindlist))
      (if first-stores
        (setf all-stores (revappend (devalue-form last-setterform) all-stores)
              (cadr tail) (list 'MULTIPLE-VALUE-BIND last-stores getterform nil)
              tail (cddadr tail))
        (setq first-stores stores first-getterform getterform))
      (setq last-stores stores last-setterform setterform))))
;;;----------------------------------------------------------------------------
;;; more places
;;;----------------------------------------------------------------------------
(defsetf GET (symbol indicator &optional default) (value)
  (let ((storeform `(SYSTEM::%PUT ,symbol ,indicator ,value)))
    (if default
      `(PROGN ,default ,storeform) ; default wird nur zum Schein ausgewertet
      `,storeform
) ) )
;;;----------------------------------------------------------------------------
;;; Schreibt zu einem bestimmten Indicator einen Wert in eine gegebene
;;; Propertyliste. Wert ist NIL falls erfolgreich getan oder die neue
;;; (erweiterte) Propertyliste.
(define-setf-expander getf (place indicator &optional default &environment env)
  (multiple-value-bind (temps subforms stores setterform getterform)
      (get-setf-method place env)
    (let* ((storevar (gensym))
           (indicatorvar (gensym))
           (defaultvar-list (if default (list (gensym)) `()))
          )
      (values
        `(,@temps    ,indicatorvar ,@defaultvar-list)
        `(,@subforms ,indicator    ,@(if default `(,default) `()))
        `(,storevar)
        `(LET ((,(first stores) (SYS::%PUTF ,getterform ,indicatorvar ,storevar)))
           ,@defaultvar-list ; defaultvar zum Schein auswerten
           (WHEN ,(first stores) ,setterform)
           ,storevar
         )
        `(GETF ,getterform ,indicatorvar ,@defaultvar-list)
) ) ) )
;;;----------------------------------------------------------------------------
(defsetf GETHASH (key hashtable &optional default) (value)
  (let ((storeform `(SYSTEM::PUTHASH ,key ,hashtable ,value)))
    (if default
      `(PROGN ,default ,storeform) ; default wird nur zum Schein ausgewertet
      `,storeform
) ) )
;;;----------------------------------------------------------------------------
(defsetf fill-pointer SYSTEM::SET-FILL-POINTER)
;;;----------------------------------------------------------------------------
(defsetf readtable-case SYSTEM::SET-READTABLE-CASE)
;;;----------------------------------------------------------------------------
(defsetf SYMBOL-VALUE SYSTEM::SET-SYMBOL-VALUE)
(sys::%putd 'SET #'SYSTEM::SET-SYMBOL-VALUE) ; deprecated alias
;;;----------------------------------------------------------------------------
(defsetf SYMBOL-FUNCTION SYSTEM::%PUTD)
;;;----------------------------------------------------------------------------
(defsetf SYMBOL-PLIST SYSTEM::%PUTPLIST)
;;;----------------------------------------------------------------------------
(defun SYSTEM::SET-FDEFINITION (name value) ; ABI
  (setf (symbol-function (get-funname-symbol name)) value)
)
(defsetf FDEFINITION SYSTEM::SET-FDEFINITION)
;;;----------------------------------------------------------------------------
(defsetf MACRO-FUNCTION (symbol &optional env) (value)
  (declare (ignore env))
  `(PROGN
     (SETF (SYMBOL-FUNCTION ,symbol) (SYSTEM::MAKE-MACRO ,value))
     (REMPROP ,symbol 'SYSTEM::MACRO)
     ,value
   )
)
;;;----------------------------------------------------------------------------
(defsetf CHAR SYSTEM::STORE-CHAR)
(defsetf SCHAR SYSTEM::STORE-SCHAR)
(defsetf BIT SYSTEM::STORE)
(defsetf SBIT SYSTEM::STORE)
(defsetf SUBSEQ (sequence start &optional end) (value)
  `(PROGN (REPLACE ,sequence ,value :START1 ,start :END1 ,end) ,value)
)
;;;----------------------------------------------------------------------------
(define-setf-expander char-bit (char name &environment env)
  (multiple-value-bind (temps subforms stores setterform getterform)
      (get-setf-method char env)
    (let* ((namevar (gensym))
           (storevar (gensym)))
      (values `(,@temps    ,namevar)
              `(,@subforms ,name)
              `(,storevar)
              `(LET ((,(first stores) (SET-CHAR-BIT ,getterform ,namevar ,storevar)))
                 ,setterform
                 ,storevar
               )
              `(CHAR-BIT ,getterform ,namevar)
) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander LDB (bytespec integer &environment env)
  (multiple-value-bind (temps subforms stores setterform getterform)
      (get-setf-method integer env)
    (let* ((bytespecvar (gensym))
           (storevar (gensym)))
      (values (cons bytespecvar temps)
              (cons bytespec subforms)
              `(,storevar)
              `(LET ((,(first stores) (DPB ,storevar ,bytespecvar ,getterform)))
                 ,setterform
                 ,storevar
               )
              `(LDB ,bytespecvar ,getterform)
) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander MASK-FIELD (bytespec integer &environment env)
  (multiple-value-bind (temps subforms stores setterform getterform)
      (get-setf-method integer env)
    (let* ((bytespecvar (gensym))
           (storevar (gensym)))
      (values (cons bytespecvar temps)
              (cons bytespec subforms)
              `(,storevar)
              `(LET ((,(first stores) (DEPOSIT-FIELD ,storevar ,bytespecvar ,getterform)))
                 ,setterform
                 ,storevar
               )
              `(MASK-FIELD ,bytespecvar ,getterform)
) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander THE (type place &environment env)
  (multiple-value-bind (temps subforms stores setterform getterform) (get-setf-expansion place env)
    (values temps subforms stores
            (sublis-in-form (mapcar #'(lambda (storevar simpletype)
                                        (cons storevar `(THE ,simpletype ,storevar))
                                      )
                                    stores
                                    (if (and (consp type) (eq (car type) 'VALUES))
                                      (cdr type)
                                      (list type)
                                    )
                            )
                            setterform
            )
            `(THE ,type ,getterform)
) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander APPLY (&whole whole-form
                             fun &rest args &environment env)
  (if (and (listp fun)
           (eq (list-length fun) 2)
           (eq (first fun) 'FUNCTION)
           (symbolp (second fun))
      )
    (setq fun (second fun))
    (error-of-type 'source-program-error
      :form whole-form
      :detail fun
      (TEXT "~S is only defined for functions of the form #'symbol.")
      '(setf apply)))
  (multiple-value-bind (temps subforms stores setterform getterform)
      (get-setf-expansion (cons fun args) env)
    (unless (eq (car (last args)) (car (last subforms)))
      (error-of-type 'source-program-error
        :form whole-form
        :detail (cons fun args)
        (TEXT "~S on ~S is not a SETF place.")
        'apply fun))
    (let ((item (car (last temps)))) ; 'item' steht für eine Argumentliste!
      (labels ((splice (arglist)
                 ; Würde man in (LIST . arglist) das 'item' nicht als 1 Element,
                 ; sondern gespliced, sozusagen als ',@item', haben wollen, so
                 ; bräuchte man die Form, die (splice arglist) liefert.
                 (if (endp arglist)
                   'NIL
                   (let ((rest (splice (cdr arglist))))
                     (if (eql (car arglist) item)
                       ; ein (APPEND item ...) davorhängen, wie bei Backquote
                       (backquote-append item rest)
                       ; ein (CONS (car arglist) ...) davorhängen, wie bei Backquote
                       (backquote-cons (car arglist) rest)
              )) ) ) )
        (flet ((call-splicing (form)
                 ; ersetzt einen Funktionsaufruf form durch einen, bei dem
                 ; 'item' nicht 1 Argument, sondern eine Argumentliste liefert
                 (let ((fun (first form))
                       (argform (splice (rest form))))
                   ; (APPLY #'fun argform) vereinfachen:
                   ; (APPLY #'fun NIL) --> (fun)
                   ; (APPLY #'fun (LIST ...)) --> (fun ...)
                   ; (APPLY #'fun (CONS x y)) --> (APPLY #'fun x y)
                   ; (APPLY #'fun (LIST* ... z)) --> (APPLY #'fun ... z)
                   (if (or (null argform)
                           (and (consp argform) (eq (car argform) 'LIST))
                       )
                     (cons fun (cdr argform))
                     (list* 'APPLY
                            (list 'FUNCTION fun)
                            (if (and (consp argform)
                                     (or (eq (car argform) 'LIST*)
                                         (eq (car argform) 'CONS)
                                )    )
                              (cdr argform)
                              (list argform)
              )) ) ) )      )
          (values temps subforms stores
                  (call-splicing setterform)
                  (call-splicing getterform)
) ) ) ) ) )
;;;----------------------------------------------------------------------------
;;; Zusätzliche Definitionen von places
;;;----------------------------------------------------------------------------
(define-setf-expander funcall (&whole whole-form
                               fun &rest args &environment env)
  (unless (and (listp fun)
               (eq (list-length fun) 2)
               (let ((fun1 (first fun)))
                 (or (eq fun1 'FUNCTION) (eq fun1 'QUOTE))
               )
               (symbolp (second fun))
               (setq fun (second fun))
          )
    (error-of-type 'source-program-error
      :form whole-form
      :detail (cons fun args)
      (TEXT "~S is only defined for functions of the form #'symbol.")
      '(setf funcall)))
  (get-setf-expansion (cons fun args) env)
)
;;;----------------------------------------------------------------------------
(define-setf-expander PROGN (&rest forms &environment env)
  (let ((last (last forms)))
    (multiple-value-bind (temps subforms stores setterform getterform)
        (get-setf-expansion (car last) env)
      (if (eq forms last)
        (values temps subforms stores setterform getterform)
        (let ((dummyvar (gensym)))
          (values
            `(,dummyvar                    ,@temps)
            `((PROGN ,@(ldiff forms last)) ,@subforms)
            stores
            `(PROGN
               ,dummyvar ; avoid warning about unused temporary variable
               ,setterform
             )
            getterform
) ) ) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander LOCALLY (&rest body &environment env)
  (multiple-value-bind (body-rest declspecs) (system::parse-body body)
    (multiple-value-bind (temps subforms stores setterform getterform)
        (get-setf-expansion `(PROGN ,@body-rest) env)
      (if declspecs
        (let ((declarations `(DECLARE ,@declspecs)))
          (values
            temps
            (mapcar #'(lambda (x) `(LOCALLY ,declarations ,x)) subforms)
            stores
           `(LOCALLY ,declarations ,setterform)
           `(LOCALLY ,declarations ,getterform)
        ) )
        (values temps subforms stores setterform getterform)
) ) ) )
;;;----------------------------------------------------------------------------
(define-setf-expander IF (&whole whole-form
                          condition t-form f-form &environment env)
  (let ((conditionvar (gensym)))
    (multiple-value-bind (T-temps T-subforms T-stores T-setterform T-getterform)
        (get-setf-expansion t-form env)
      (multiple-value-bind (F-temps F-subforms F-stores F-setterform F-getterform)
          (get-setf-expansion f-form env)
        (unless (eql (length T-stores) (length F-stores))
          (error-of-type 'source-program-error
            :form whole-form
            :detail whole-form
            (TEXT "SETF place ~S expects different numbers of values in the true and false branches (~D vs. ~D values).")
            (list 'IF condition t-form f-form) (length T-stores) (length F-stores)))
        (values
          `(,conditionvar
            ,@T-temps
            ,@F-temps
           )
          `(,condition
            ,@(mapcar #'(lambda (x) `(IF ,conditionvar ,x)) T-subforms)
            ,@(mapcar #'(lambda (x) `(IF (NOT ,conditionvar) ,x)) F-subforms)
           )
          T-stores
          `(IF ,conditionvar ,T-setterform ,(sublis-in-form (mapcar #'cons F-stores T-stores) F-setterform))
          `(IF ,conditionvar ,T-getterform ,F-getterform)
) ) ) ) )
;;;----------------------------------------------------------------------------
(defsetf GET-DISPATCH-MACRO-CHARACTER
         (disp-char sub-char &optional (readtable '*READTABLE*)) (value)
  `(PROGN (SET-DISPATCH-MACRO-CHARACTER ,disp-char ,sub-char ,value ,readtable) ,value)
)
;;;----------------------------------------------------------------------------
(def-setf-alias long-float-digits SYSTEM::|(SETF LONG-FLOAT-DIGITS)|)
;;;----------------------------------------------------------------------------
(defsetf system::%record-ref system::%record-store)
;;;----------------------------------------------------------------------------
(defsetf system::%structure-ref system::%structure-store)
;;;----------------------------------------------------------------------------
(def-setf-alias clos::standard-instance-access CLOS::|(SETF STANDARD-INSTANCE-ACCESS)|)
;;;----------------------------------------------------------------------------
(def-setf-alias system::closure-name SYSTEM::|(SETF CLOSURE-NAME)|)
;;;----------------------------------------------------------------------------
#+LOGICAL-PATHNAMES
(defsetf logical-pathname-translations set-logical-pathname-translations)
;;;----------------------------------------------------------------------------
(defsetf stream-external-format system::set-stream-external-format)
;;;----------------------------------------------------------------------------
;;; Handhabung von (SETF (VALUES place1 ... placek) form)
;;; --> (MULTIPLE-VALUE-BIND (dummy1 ... dummyk) form
;;;       (SETF place1 dummy1 ... placek dummyk)
;;;       (VALUES dummy1 ... dummyk)
;;;     )
(define-setf-expander values (&rest subplaces &environment env)
  (multiple-value-bind (temps subforms stores setterforms getterforms)
      (setf-VALUES-aux subplaces env)
    (values temps
            subforms
            stores
            `(VALUES ,@setterforms)
            `(VALUES ,@getterforms)
) ) )
(defun setf-VALUES-aux (places env)
  (do ((temps nil)
       (subforms nil)
       (stores nil)
       (setterforms nil)
       (getterforms nil)
       (placesr places))
      ((endp placesr)
       (setq temps (nreverse temps))
       (setq subforms (nreverse subforms))
       (setq stores (nreverse stores))
       (setq setterforms (nreverse setterforms))
       (setq getterforms (nreverse getterforms))
       (values temps subforms stores setterforms getterforms)
      )
    (multiple-value-bind (SM-temps SM-subforms SM-stores SM-setterform SM-getterform)
        (get-setf-expansion (pop placesr) env)
      (setq temps (revappend SM-temps temps))
      (setq subforms (revappend SM-subforms subforms))
      (when SM-stores
        ;; See ANSI CL 5.1.2.3.
        (dolist (extra-store (rest SM-stores))
          (push extra-store temps)
          (push 'NIL subforms))
        (push (first SM-stores) stores))
      (setq setterforms (cons SM-setterform setterforms))
      (setq getterforms (cons SM-getterform getterforms))
) ) )
;;;----------------------------------------------------------------------------
;;; Analog zu (MULTIPLE-VALUE-SETQ (var1 ... vark) form) :
;;; (MULTIPLE-VALUE-SETF (place1 ... placek) form)
;;; --> (VALUES (SETF (VALUES place1 ... placek) form))
;;; --> (MULTIPLE-VALUE-BIND (dummy1 ... dummyk) form
;;;       (SETF place1 dummy1 ... placek dummyk)
;;;       dummy1
;;;     )
(defmacro multiple-value-setf (places form &environment env)
  (multiple-value-bind (temps subforms stores setterforms getterforms)
      (setf-VALUES-aux places env)
    (declare (ignore getterforms))
    (wrap-let* (mapcar #'list temps subforms)
      `(MULTIPLE-VALUE-BIND ,stores ,form
         ,@setterforms
         ,(first stores) ; (null stores) -> NIL -> Wert NIL
    )  )
) )
;;;----------------------------------------------------------------------------
;;;                              Symbol-macros
(define-symbol-macro *ansi* (sys::ansi))
(defsetf sys::ansi sys::set-ansi)
(system::%set-documentation '*ansi* 'variable
 "This symbol-macro modifies some variables for maximum ANSI CL compliance.
Variables affected: `custom:*floating-point-contagion-ansi*',
 `custom:*floating-point-rational-contagion-ansi*', `custom:*phase-ansi*',
 `custom:*merge-pathnames-ansi*', `custom:*print-pathnames-ansi*',
 `custom:*print-space-char-ansi*', `custom:*parse-namestring-ansi*',
 `custom:*print-empty-arrays-ansi*', `custom:*print-unreadable-ansi*',
 `custom:*sequence-count-ansi*', `custom:*coerce-fixnum-char-ansi*',
 `custom:*defun-accept-specialized-lambda-list*', `custom:*loop-ansi*'.
Invoking CLISP with `-ansi' sets this to T.
Invoking CLISP with `-traditional' sets this to NIL.")

(define-symbol-macro *current-language* (sys::current-language))
(defsetf sys::current-language sys::set-current-language)
(system::%set-documentation '*current-language* 'variable
 "This symbol-macro determines the current language used for UI.")

(define-symbol-macro *lib-directory* (sys::lib-directory))
(defsetf sys::lib-directory sys::set-lib-directory)
(system::%set-documentation '*lib-directory* 'variable
 "This symbol-macro determines the location where CLISP finds its data files.")

(define-symbol-macro *default-file-encoding*
  (system::default-file-encoding))
(defsetf system::default-file-encoding system::set-default-file-encoding)
#+UNICODE
(progn
  (define-symbol-macro *pathname-encoding* (system::pathname-encoding))
  (defsetf system::pathname-encoding system::set-pathname-encoding)
  (define-symbol-macro *terminal-encoding* (system::terminal-encoding))
  (defsetf system::terminal-encoding system::set-terminal-encoding)
  (define-symbol-macro *misc-encoding* (system::misc-encoding))
  (defsetf system::misc-encoding system::set-misc-encoding)
)
(when (fboundp 'sys::setenv)
  (defsetf ext:getenv sys::setenv))
