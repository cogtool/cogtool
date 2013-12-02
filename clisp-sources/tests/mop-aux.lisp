;;; Auxiliary functions for MOP.
;;;
;;; Copyright (C) 2004 Bruno Haible
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html

#-(or CLISP ALLEGRO LISPWORKS)
(let ((packname
         #+SBCL "SB-PCL" ; or "SB-MOP"?
         #+CMU "PCL" ; or "MOP"?
         #+OpenMCL "OPENMCL-MOP" ; or "CCL" ?
         ))
  #+SBCL (unlock-package packname)
  (rename-package packname packname (cons "CLOS" (package-nicknames packname)))
  t)

;; ============================================================================
;;                    compute-effective-method-as-function

#+SBCL
(defun clos::compute-effective-method-as-function (gf methods args)
  (declare (ignore args))
  (if methods
    (let ((emf (clos::get-effective-method-function gf methods)))
      #'(lambda (&rest args) (clos::invoke-emf emf args)))
    #'(lambda (&rest args) (apply 'no-applicable-method gf args))))

#-(or CLISP SBCL)
(#+ALLEGRO without-package-locks
 #-ALLEGRO progn

(macrolet ((err-misplaced (item)
             `(funcall errfunc ,item
                       "Lambda list marker ~S not allowed here."
                       ,item))
           (err-invalid (item)
             `(funcall errfunc ,item
                       (if (or (symbolp ,item) (listp ,item))
                         "Invalid lambda list element ~S"
                         "Invalid lambda list element ~S. A lambda list may only contain symbols and lists.")
                       ,item))
           (check-item (item permissible)
             `(if (member ,item ,permissible :test #'eq)
                (return)
                (err-misplaced ,item)))
           (skip-L (lastseen items)
             `(loop
                (when (atom L) (return))
                (let ((item (car L)))
                  (if (member item lambda-list-keywords :test #'eq)
                    (check-item item ,items)
                    (funcall errfunc item
                             ,(case lastseen
                                (&REST "Lambda list element ~S is superfluous. Only one variable is allowed after &REST.")
                                (&ALLOW-OTHER-KEYS "Lambda list element ~S is superfluous. No variable is allowed right after &ALLOW-OTHER-KEYS.")
                                (&ENVIRONMENT "Lambda list element ~S is superfluous. Only one variable is allowed after &ENVIRONMENT.")
                                (t "Lambda list element ~S is superfluous."))
                             item)))
                (setq L (cdr L)))))

;;; Analyzes a lambda-list of a function (CLtL2 p. 76, ANSI CL 3.4.1.).
;;; Reports errors through errfunc (a function taking a detail object, an
;;; error format string and format string arguments).
;; Returns 13 values:
;; 1. list of required parameters
;; 2. list of optional parameters
;; 3. list of init-forms of the optional parameters
;; 4. list of supplied-vars for the optional parameters (0 for the missing)
;; 5. &rest parameter or 0
;; 6. flag, if keywords are allowed
;; 7. list of keywords
;; 8. list of keyword parameters
;; 9. list of init-forms of the keyword parameters
;; 10. list of supplied-vars for the keyword parameters (0 for the missing)
;; 11. flag, if other keywords are allowed
;; 12. list of &aux variables
;; 13. list of init-forms of the &aux variables
  (defun analyze-lambdalist (lambdalist errfunc)
    (let ((L lambdalist) ; rest of the lambda-list
          (reqvar nil)
          (optvar nil)
          (optinit nil)
          (optsvar nil)
          (rest 0)
          (keyflag nil)
          (keyword nil)
          (keyvar nil)
          (keyinit nil)
          (keysvar nil)
          (allow-other-keys nil)
          (auxvar nil)
          (auxinit nil))
      ;; The lists are all accumulated in reversed order.
      ;; Required parameters:
      (loop
        (if (atom L) (return))
        (let ((item (car L)))
          (if (symbolp item)
            (if (member item lambda-list-keywords :test #'eq)
              (check-item item '(&optional &rest &key &aux))
              (push item reqvar))
            (err-invalid item)))
        (setq L (cdr L)))
      ;; Now (or (atom L) (member (car L) '(&optional &rest &key &aux))).
      ;; Optional parameters:
      (when (and (consp L) (eq (car L) '&optional))
        (setq L (cdr L))
        (macrolet ((note-optional (var init svar)
                     `(progn
                        (push ,var optvar)
                        (push ,init optinit)
                        (push ,svar optsvar))))
          (loop
            (if (atom L) (return))
            (let ((item (car L)))
              (if (symbolp item)
                (if (member item lambda-list-keywords :test #'eq)
                  (check-item item '(&rest &key &aux))
                  (note-optional item nil 0))
                (if (and (consp item) (symbolp (car item)))
                  (if (null (cdr item))
                    (note-optional (car item) nil 0)
                    (if (consp (cdr item))
                      (if (null (cddr item))
                        (note-optional (car item) (cadr item) 0)
                        (if (and (consp (cddr item)) (symbolp (caddr item))
                                 (null (cdddr item)))
                          (note-optional (car item) (cadr item) (caddr item))
                          (err-invalid item)))
                      (err-invalid item)))
                  (err-invalid item))))
            (setq L (cdr L)))))
      ;; Now (or (atom L) (member (car L) '(&rest &key &aux))).
      ;; &rest parameters:
      (when (and (consp L) (eq (car L) '&rest))
        (setq L (cdr L))
        (macrolet ((err-norest ()
                     `(funcall errfunc lambdalist
                               "Missing &REST parameter in lambda list ~S"
                               lambdalist)))
          (if (atom L)
            (err-norest)
            (prog ((item (car L)))
              (if (symbolp item)
                (if (member item lambda-list-keywords :test #'eq)
                  (progn (err-norest) (return))
                  (setq rest item))
                (err-invalid item))
              (setq L (cdr L)))))
        ;; Move forward to the next &KEY or &AUX:
        (skip-L &rest '(&key &aux)))
      ;; Now (or (atom L) (member (car L) '(&key &aux))).
      ;; Keyword parameters:
      (when (and (consp L) (eq (car L) '&key))
        (setq L (cdr L))
        (setq keyflag t)
        (loop
          (if (atom L) (return))
          (let ((item (car L)))
            (if (symbolp item)
              (if (member item lambda-list-keywords :test #'eq)
                (check-item item '(&allow-other-keys &aux))
                (progn
                  (push (intern (symbol-name item) (find-package "KEYWORD")) keyword)
                  (push item keyvar) (push nil keyinit) (push 0 keysvar)))
              (if (and (consp item)
                       (or (symbolp (car item))
                           (and (consp (car item))
                                (symbolp (caar item))
                                (consp (cdar item))
                                (symbolp (cadar item))
                                (null (cddar item))))
                       (or (null (cdr item))
                           (and (consp (cdr item))
                                (or (null (cddr item))
                                    (and (consp (cddr item))
                                         (symbolp (caddr item))
                                         (null (cdddr item)))))))
                (progn
                  (if (consp (car item))
                    (progn
                      (push (caar item) keyword)
                      (push (cadar item) keyvar))
                    (progn
                      (push (intern (symbol-name (car item)) (find-package "KEYWORD"))
                            keyword)
                      (push (car item) keyvar)))
                  (if (consp (cdr item))
                    (progn
                      (push (cadr item) keyinit)
                      (if (consp (cddr item))
                        (push (caddr item) keysvar)
                        (push 0 keysvar)))
                    (progn (push nil keyinit) (push 0 keysvar))))
                (err-invalid item))))
          (setq L (cdr L)))
        ;; Now (or (atom L) (member (car L) '(&allow-other-keys &aux))).
        (when (and (consp L) (eq (car L) '&allow-other-keys))
          (setq allow-other-keys t)
          (setq L (cdr L))
          ;; Move forward  to the next &AUX:
          (skip-L &allow-other-keys '(&aux))))
      ;; Now (or (atom L) (member (car L) '(&aux))).
      ;; &aux variables:
      (when (and (consp L) (eq (car L) '&aux))
        (setq L (cdr L))
        (loop
          (if (atom L) (return))
          (let ((item (car L)))
            (if (symbolp item)
              (if (member item lambda-list-keywords :test #'eq)
                (err-misplaced item)
                (progn (push item auxvar) (push nil auxinit)))
              (if (and (consp item) (symbolp (car item)))
                (if (null (cdr item))
                  (progn (push (car item) auxvar) (push nil auxinit))
                  (if (and (consp (cdr item)) (null (cddr item)))
                    (progn (push (car item) auxvar) (push (cadr item) auxinit))
                    (err-invalid item)))
                (err-invalid item))))
          (setq L (cdr L))))
      ;; Now (atom L).
      (if L
        (funcall errfunc lambdalist
                 "Lambda lists with dots are only allowed in macros, not here: ~S"
                 lambdalist))
      (values
        (nreverse reqvar)
        (nreverse optvar) (nreverse optinit) (nreverse optsvar)
        rest
        keyflag
        (nreverse keyword) (nreverse keyvar) (nreverse keyinit) (nreverse keysvar)
        allow-other-keys
        (nreverse auxvar) (nreverse auxinit))))

;;; Analyzes a lambda-list of a generic function (ANSI CL 3.4.2.).
;;; Reports errors through errfunc (a function taking a detail object, an
;;; error format string and format string arguments).
;; Returns 7 values:
;; 1. list of required parameters
;; 2. list of optional parameters
;; 3. &rest parameter or 0
;; 4. flag, if keywords are allowed
;; 5. list of keywords
;; 6. list of keyword parameters
;; 7. flag, if other keywords are allowed
  (defun analyze-generic-function-lambdalist (lambdalist errfunc)
    (let ((L lambdalist) ; rest of the lambda-list
          (reqvar nil)
          (optvar nil)
          (rest 0)
          (keyflag nil)
          (keyword nil)
          (keyvar nil)
          (allow-other-keys nil))
      ;; The lists are all accumulated in reversed order.
      ;; Required parameters:
      (loop
        (if (atom L) (return))
        (let ((item (car L)))
          (if (symbolp item)
            (if (member item lambda-list-keywords :test #'eq)
              (check-item item '(&optional &rest &key))
              ;; Need to check for duplicates here because otherwise the
              ;; :arguments-precedence-order makes no sense.
              (if (member item reqvar :test #'eq)
                (funcall errfunc item "Duplicate variable name ~S" item)
                (push item reqvar)))
            (err-invalid item)))
        (setq L (cdr L)))
      ;; Now (or (atom L) (member (car L) '(&optional &rest &key))).
      ;; Optional parameters:
      (when (and (consp L) (eq (car L) '&optional))
        (setq L (cdr L))
        (loop
          (if (atom L) (return))
          (let ((item (car L)))
            (if (symbolp item)
              (if (member item lambda-list-keywords :test #'eq)
                (check-item item '(&rest &key))
                (push item optvar))
              (if (and (consp item) (symbolp (car item)))
                (if (null (cdr item))
                  (push (car item) optvar)
                  (funcall errfunc item
                           "Invalid lambda list element ~S. Optional parameters cannot have default value forms in generic function lambda lists."
                           item))
                (err-invalid item))))
          (setq L (cdr L))))
      ;; Now (or (atom L) (member (car L) '(&rest &key))).
      ;; &rest parameters:
      (when (and (consp L) (eq (car L) '&rest))
        (setq L (cdr L))
        (macrolet ((err-norest ()
                     `(funcall errfunc lambdalist
                               "Missing &REST parameter in lambda list ~S"
                               lambdalist)))
          (if (atom L)
            (err-norest)
            (prog ((item (car L)))
              (if (symbolp item)
                (if (member item lambda-list-keywords :test #'eq)
                  (progn (err-norest) (return))
                  (setq rest item))
                (err-invalid item))
              (setq L (cdr L)))))
        ;; Move forward to the next &KEY:
        (skip-L &rest '(&key)))
      ;; Now (or (atom L) (member (car L) '(&key))).
      ;; Keyword parameters:
      (when (and (consp L) (eq (car L) '&key))
        (setq L (cdr L))
        (setq keyflag t)
        (loop
          (if (atom L) (return))
          (let ((item (car L)))
            (if (symbolp item)
              (if (member item lambda-list-keywords :test #'eq)
                (check-item item '(&allow-other-keys))
                (progn
                  (push (intern (symbol-name item) (find-package "KEYWORD")) keyword)
                  (push item keyvar)))
              (if (and (consp item)
                       (or (symbolp (car item))
                           (and (consp (car item))
                                (symbolp (caar item))
                                (consp (cdar item))
                                (symbolp (cadar item))
                                (null (cddar item)))))
                (if (null (cdr item))
                  (if (consp (car item))
                    (progn
                      (push (caar item) keyword)
                      (push (cadar item) keyvar))
                    (progn
                      (push (intern (symbol-name (car item)) (find-package "KEYWORD"))
                            keyword)
                      (push (car item) keyvar)))
                  (funcall errfunc item
                           "Invalid lambda list element ~S. Keyword parameters cannot have default value forms in generic function lambda lists."
                           item))
                (err-invalid item))))
          (setq L (cdr L)))
        ;; Now (or (atom L) (member (car L) '(&allow-other-keys))).
        (when (and (consp L) (eq (car L) '&allow-other-keys))
          (setq allow-other-keys t)
          (setq L (cdr L))
          ;; Move forward to the end:
          (skip-L &allow-other-keys '())))
      ;; Now (atom L).
      (if L
        (funcall errfunc lambdalist
                 "Lambda lists with dots are only allowed in macros, not here: ~S"
                 lambdalist))
      (values
        (nreverse reqvar)
        (nreverse optvar)
        rest
        keyflag
        (nreverse keyword) (nreverse keyvar)
        allow-other-keys)))

;;; Analyzes a define-method-combination lambda-list (ANSI CL 3.4.10.).
;;; Reports errors through errfunc (a function taking a detail object, an
;;; error format string and format string arguments).
;; Returns 14 values:
;; 1. &whole parameter or 0
;; 2. list of required parameters
;; 3. list of optional parameters
;; 4. list of init-forms of the optional parameters
;; 5. list of supplied-vars for the optional parameters (0 for the missing)
;; 6. &rest parameter or 0
;; 7. flag, if keywords are allowed
;; 8. list of keywords
;; 9. list of keyword parameters
;; 10. list of init-forms of the keyword parameters
;; 11. list of supplied-vars for the keyword parameters (0 for the missing)
;; 12. flag, if other keywords are allowed
;; 13. list of &aux variables
;; 14. list of init-forms of the &aux variables
  (defun analyze-method-combination-lambdalist (lambdalist errfunc)
    (let ((L lambdalist) ; rest of the lambda-list
          (whole 0))
      ;; The lists are all accumulated in reversed order.
      ;; &whole parameter:
      (when (and (consp L) (eq (car L) '&whole))
        (setq L (cdr L))
        (macrolet ((err-nowhole ()
                     `(funcall errfunc lambdalist
                               "Missing &WHOLE parameter in lambda list ~S"
                               lambdalist)))
          (if (atom L)
            (err-nowhole)
            (prog ((item (car L)))
              (if (symbolp item)
                (if (member item lambda-list-keywords :test #'eq)
                  (progn (err-nowhole) (return))
                  (setq whole item))
                (err-invalid item))
              (setq L (cdr L))))))
      ;; The rest should be an ordinary lambda-list.
      (multiple-value-bind (reqvar optvar optinit optsvar rest
                            keyflag keyword keyvar keyinit keysvar allow-other-keys
                            auxvar auxinit)
          (analyze-lambdalist L errfunc)
        (values
          whole
          reqvar
          optvar optinit optsvar
          rest
          keyflag
          keyword keyvar keyinit keysvar
          allow-other-keys
          auxvar auxinit))))

) ; macrolet

(defun %no-next-method (method &rest args)
  (apply #'no-next-method (clos:method-generic-function method) method args))

;; Adds the function definitions of CALL-NEXT-METHOD and NEXT-METHOD-P.
(defun add-next-method-local-functions (cont req-dummies rest-dummy body)
  ;; FIXME: Where does the method argument for %NO-NEXT-METHOD come from?
  `(FLET ((CALL-NEXT-METHOD (&REST NEW-ARGS)
            (IF NEW-ARGS
              ;; argument checking in the interpreter only
              (IF ,cont
                (APPLY ,cont NEW-ARGS)
                (APPLY #'%NO-NEXT-METHOD NIL NEW-ARGS))
              ,(if rest-dummy
                 `(IF ,cont
                    (APPLY ,cont ,@req-dummies ,rest-dummy)
                    (APPLY #'%NO-NEXT-METHOD NIL ,@req-dummies ,rest-dummy))
                 `(IF ,cont
                    (FUNCALL ,cont ,@req-dummies)
                    (%NO-NEXT-METHOD NIL ,@req-dummies)))))
          (NEXT-METHOD-P () ,cont))
     ,@body))

(defun make-method-error (whole-form)
  (error
    (make-condition 'program-error
      :format-control "~S cannot be used here: ~S"
      :format-arguments (list 'make-method whole-form))))

(defun callable-method-form-p (form)
  (or (typep form 'method)
      (and (consp form) (eq (car form) 'MAKE-METHOD)
           (consp (cdr form)) (null (cddr form)))))

(defun call-method-arg1-error (whole-form)
  (error
    (make-condition 'program-error
      :format-control "~S: The first argument is neither a method nor a (MAKE-METHOD ...) form: ~S"
      :format-arguments (list 'call-method whole-form))))

(defun call-method-arg2-error (whole-form)
  (error
    (make-condition 'program-error
      :format-control "~S: The second argument is not a list: ~S"
      :format-arguments (list 'call-method whole-form))))

(defun call-method-arg2elements-error (whole-form)
  (error
    (make-condition 'program-error
      :format-control "~S: The second argument is not a list of methods or (MAKE-METHOD ...) forms: ~S"
      :format-arguments (list 'call-method whole-form))))

;; The signature of a function object.
(defstruct (signature (:type vector) (:conc-name sig-))
  (req-num 0    :type fixnum)
  (opt-num 0    :type fixnum)
  (rest-p nil   :type boolean)
  (keys-p nil   :type boolean)
  (keywords nil :type list)
  (allow-p nil  :type boolean))

;; Checks a generic-function lambda-list and converts it to a signature.
;; Reports errors through errfunc (a function taking a detail object, an
;; error format string and format string arguments).
(defun generic-function-lambda-list-to-signature (lambdalist errfunc)
  (multiple-value-bind (reqvars optvars rest keyp keywords keyvars allowp)
      (analyze-generic-function-lambdalist lambdalist errfunc)
    (declare (ignore keyvars)) 
    (let ((reqnum (length reqvars))
          (optnum (length optvars))
          (restp (or (not (eql rest 0)) keyp))) ; &key implies &rest
      (make-signature :req-num reqnum :opt-num optnum
                      :rest-p restp :keys-p keyp
                      :keywords keywords :allow-p allowp))))

(defun generic-function-signature (gf)
  (let ((lambdalist (clos:generic-function-lambda-list gf)))
    (generic-function-lambda-list-to-signature lambdalist 
      #'(lambda (detail errorstring &rest arguments)
          (declare (ignore detail))
          (error "Invalid ~S result ~S: ~A"
                 'generic-function-lambda-list lambdalist
                 (apply #'format nil errorstring arguments))))))

(defun gf-sig-restp (sig)
  (or (sig-rest-p sig) (> (sig-opt-num sig) 0)))

(defun method-lambda-list-to-signature (lambda-list errfunc)
  (multiple-value-bind (reqvars optvars optinits optsvars rest
                        keyp keywords keyvars keyinits keysvars
                        allowp auxvars auxinits)
      (analyze-lambdalist lambda-list errfunc)
    (declare (ignore optinits optsvars keyvars keyinits keysvars
                     auxvars auxinits))
    (make-signature
      :req-num (length reqvars) :opt-num (length optvars)
      :rest-p (or keyp (not (eql rest 0))) :keys-p keyp
      :keywords keywords :allow-p allowp)))

(defun method-signature (method)
  (let ((lambda-list (clos:method-lambda-list method))) 
    (method-lambda-list-to-signature lambda-list
      #'(lambda (detail errorstring &rest arguments)
          (declare (ignore detail))
          (error "Invalid ~S result for ~S: ~:S: ~A"
                 'method-lambda-list method lambda-list
                 (apply #'format nil errorstring arguments))))))

(defun gensym-list (how-many)
  (map-into (make-list how-many) #'gensym))

(defun gf-keyword-arguments (restp signature methods)
  ;; CLtL2 28.1.6.4., 28.1.6.5., ANSI CL 7.6.4., 7.6.5.
  ;; Keyword Arguments in Generic Functions
  (when restp
    ;; The generic function has &REST or &KEY, thus try all methods.
    ;; "If the lambda-list of ... the generic function definition
    ;;  contains &allow-other-keys, all keyword arguments are accepted."
    (unless (sig-allow-p signature)
      ;; "The specific set of keyword arguments accepted ...
      ;;  varies according to the applicable methods."
      (let ((signatures (mapcar #'method-signature methods)))
        ;; "A method that has &rest but not &key does not affect the
        ;;   set of acceptable keyword arguments."
        (setq signatures (delete-if-not #'sig-keys-p signatures))
        ;; No &key in the generic function, and no method with &key ==>
        ;; no restriction on the arguments.
        (when (or (sig-keys-p signature) signatures)
          ;; "If the lambda-list of any applicable method ... contains
          ;;  &allow-other-keys, all keyword arguments are accepted."
          (unless (some #'sig-allow-p signatures)
            ;; "The set of keyword arguments accepted for a
            ;;  particular call is the union of the keyword
            ;;  arguments accepted by all applicable methods and
            ;;  the keyword arguments mentioned after &key in the
            ;;  generic function definition."
            (let* ((keywords
                    (remove-duplicates
                     (append (sig-keywords signature)
                             (reduce #'append (mapcar #'sig-keywords signatures)))
                     :from-end t))
                   (opt-vars (gensym-list (sig-opt-num signature)))
                   (key-vars (gensym-list (length keywords)))
                   (lambdalist-keypart
                     `(&KEY    ; lambdalist-keypart
                       ,@(mapcar #'(lambda (kw var) `((,kw ,var)))
                                 keywords key-vars))))
              (values opt-vars key-vars lambdalist-keypart))))))))

;; Returns pieces of code to be used in the expansion of the effective-method.
;; 1. the lambda-list of the effective-method.
;; 2. the part of the lambda-list responsible for keyword checking.
;; 3. a declarations/forms list to use right after the lambda-list.
;; 4. an application primitive to use with argument lists for the methods.
;; 5. a list of forms representing the arguments to pass to methods.
;; 6. a set of macro definitions that defines local macros.
(defun effective-method-code-bricks (gf methods duplicates)
  (let* ((signature (generic-function-signature gf))
         (req-num (sig-req-num signature))
         (req-vars (gensym-list req-num))
         (restp (gf-sig-restp signature))
         (rest-var (if restp (gensym)))
         (apply-fun (if restp 'APPLY 'FUNCALL))
         (apply-args `(,@req-vars ,@(if restp `(,rest-var) '())))
         (lambdalist `(,@req-vars ,@(if restp `(&REST ,rest-var) '()))))
    (multiple-value-bind (opt-vars key-vars lambdalist-keypart)
        (gf-keyword-arguments restp signature methods)
      (values
        ;; 1. lambda-list
        (if (null opt-vars)
          (append lambdalist lambdalist-keypart)
          lambdalist)
        ;; 2. lambda-list &key part
        lambdalist-keypart
        ;; 3. declarations and first forms
        (if (null opt-vars)
          (if key-vars `((DECLARE (IGNORE ,@key-vars))) '())
          `((APPLY #'(LAMBDA (&OPTIONAL ,@opt-vars ,@lambdalist-keypart)
                       (DECLARE (IGNORE ,@opt-vars ,@key-vars)))
                   ,rest-var)))
        ;; 4. application primitive
        apply-fun
        ;; 5. list of forms representing the argument
        apply-args
        ;; 6. macro definitions
        `((MAKE-METHOD (&WHOLE WHOLE FORM)
            (DECLARE (IGNORE FORM))
            (MAKE-METHOD-ERROR WHOLE))
          (CALL-METHOD (&WHOLE WHOLE METHOD &OPTIONAL NEXT-METHODS-LIST)
            (UNLESS (CALLABLE-METHOD-FORM-P METHOD)
              (CALL-METHOD-ARG1-ERROR WHOLE))
            (UNLESS (LISTP NEXT-METHODS-LIST)
              (CALL-METHOD-ARG2-ERROR WHOLE))
            ,@(when duplicates
                `((LET ((METHOD+GROUPNAME (ASSOC METHOD ',duplicates :TEST #'EQ)))
                    (WHEN METHOD+GROUPNAME
                      (CALL-METHOD-DUPLICATES-ERROR ',gf METHOD+GROUPNAME)))))
            (LET ((NEXT-METHODS-EM-FORM
                    (IF NEXT-METHODS-LIST
                      (LIST 'FUNCTION
                        (LIST 'LAMBDA ',lambdalist
                          (LIST 'CALL-METHOD (CAR NEXT-METHODS-LIST)
                            (CDR NEXT-METHODS-LIST))))
                      'NIL)))
              (IF (TYPEP METHOD 'METHOD)
                (PROGN
                  (UNLESS (EVERY #'CALLABLE-METHOD-FORM-P NEXT-METHODS-LIST)
                    (CALL-METHOD-ARG2ELEMENTS-ERROR WHOLE))
                  (LIST 'FUNCALL (LIST 'QUOTE (CLOS:METHOD-FUNCTION METHOD))
                    ',(cons (ecase apply-fun (APPLY 'LIST*) (FUNCALL 'LIST))
                            apply-args)
                    (LIST* 'LIST
                      (MAPCAR #'(LAMBDA (NEXT-METHOD)
                                  (IF (TYPEP NEXT-METHOD 'METHOD)
                                    NEXT-METHOD ; no need to quote, since self-evaluating
                                    (LIST 'LET
                                      (LIST (LIST 'METHOD-CLASS
                                                  '',(clos:generic-function-method-class gf)))
                                      (LIST 'APPLY
                                            '#'MAKE-INSTANCE
                                            'METHOD-CLASS
                                            ':LAMBDA-LIST '',lambdalist
                                            ':SPECIALIZERS '',(make-list req-num :initial-element (find-class 't))
                                            ':FUNCTION
                                              (LET ((CONT (GENSYM)))
                                                (LIST 'FUNCTION
                                                  (LIST 'LAMBDA (CONS CONT ',lambdalist)
                                                    (LIST 'DECLARE (LIST 'IGNORABLE CONT))
                                                    (ADD-NEXT-METHOD-LOCAL-FUNCTIONS CONT ',req-vars ',rest-var
                                                      (CDR NEXT-METHOD)))))))))
                              NEXT-METHODS-LIST))))
                (LET ((CONT (GENSYM)))
                  (LIST 'LET (LIST (LIST CONT NEXT-METHODS-EM-FORM))
                    (LIST 'DECLARE (LIST 'IGNORABLE CONT))
                    (ADD-NEXT-METHOD-LOCAL-FUNCTIONS CONT ',req-vars ',rest-var
                      (CDR METHOD))))))))))))

(defun proper-list-p (l)
  (and (listp l)
       (list-length l)
       (null (cdr (last l)))))

; Check the effective-method option (:ARGUMENTS ...).
; Returns two values:
; 1. the arguments-lambda-list,
; 2. the list of variables contained therein.
(defun check-em-arguments-option (option caller name)
  (let ((arguments-lambda-list (cdr option)))
    (multiple-value-bind (whole reqvars optvars optinits optsvars rest
                          keyp keywords keyvars keyinits keysvars allowp
                          auxvars auxinits)
        (analyze-method-combination-lambdalist arguments-lambda-list
          #'(lambda (detail errorstring &rest arguments)
              (declare (ignore detail))
              (error
                (make-condition 'program-error
                  :format-control "~S ~S: invalid ~S lambda-list: ~A"
                  :format-arguments (list caller name ':arguments
                                          (apply #'format nil errorstring arguments))))))
      (declare (ignore optinits keyp keywords keyinits allowp auxinits))
      (values
       arguments-lambda-list
       (remove 0 (append (list whole) reqvars optvars optsvars (list rest)
                         keyvars keysvars auxvars))))))

; Check the effective-method option (:GENERIC-FUNCTION ...).
; Returns the generic-function variable contained therein.
(defun check-em-generic-function-option (option caller name)
  (unless (and (consp (cdr option)) (symbolp (cadr option)) (null (cddr option)))
    (error
      (make-condition 'program-error
        :format-control "~S ~S: Invalid syntax for ~S option: ~S"
        :format-arguments (list caller name ':generic-function option))))
  (cadr option))

; Check the effective-method option (:DUPLICATES ...).
; Returns an alist of methods and its method group names.
(defun check-em-duplicates-option (option caller name)
  (unless (and (proper-list-p (cdr option))
               (every #'(lambda (x)
                          (and (consp x)
                               (typep (car x) 'method)
                               (symbolp (cdr x))))
                      (cdr option)))
    (error
      (make-condition 'program-error
        :format-control "~S ~S: Invalid syntax for ~S option: ~S"
        :format-arguments (list caller name ':duplicates option))))
  (cdr option))

;; Given the generic function, its combination, and the effective method form
;; and the arguments-lambda-list specifying variables for it, constructs the
;; function form for the effective method, including correct arguments and with
;; the next-method support.
(defun build-effective-method-function-form (generic-function combination methods
                                             effective-method-form
                                             combination-arguments-lambda-list
                                             generic-function-variable
                                             duplicates)
  (multiple-value-bind (lambdalist lambdalist-keypart firstforms apply-fun apply-args macrodefs)
      (effective-method-code-bricks generic-function methods duplicates)
    (declare (ignore lambdalist-keypart))
    (let* ((declarations #+CLISP (clos::method-combination-declarations combination)
                         #-CLISP '())
           (ef-fun
             (let ((wrapped-ef-form
                     `(MACROLET ,macrodefs
                        ,effective-method-form)))
               (when combination-arguments-lambda-list
                 ;; Use an inline lambda to assign values to the variables
                 ;; of the combination-arguments-lambda-list.
                 (multiple-value-bind (whole reqvars optvars optinits optsvars rest
                                       keyp keywords keyvars keyinits keysvars
                                       allowp auxvars auxinits)
                     (analyze-method-combination-lambdalist combination-arguments-lambda-list
                       #'(lambda (detail errorstring &rest arguments)
                           (declare (ignore detail))
                           (error "In ~S ~S lambda list: ~A"
                                  combination ':arguments
                                  (apply #'format nil errorstring arguments))))
                   (declare (ignore optinits optsvars
                                    keywords keyvars keyinits keysvars
                                    allowp auxvars auxinits))
                   (let ((whole-var nil)
                         (whole-form nil))
                     (unless (eql whole 0)
                       (setq whole-var whole)
                       (setq whole-form (list* (ecase apply-fun
                                                 (APPLY 'LIST*)
                                                 (FUNCALL 'LIST))
                                               apply-args))
                       (setq combination-arguments-lambda-list
                             (cddr combination-arguments-lambda-list)))
                     ;; The combination-arguments-lambda-list has an implicit
                     ;; &ALLOW-OTHER-KEYS.
                     (when (and (member '&KEY combination-arguments-lambda-list :test #'eq)
                                (not (member '&ALLOW-OTHER-KEYS combination-arguments-lambda-list :test #'eq)))
                       (let ((i (or (position '&AUX combination-arguments-lambda-list)
                                    (length combination-arguments-lambda-list))))
                         (setq combination-arguments-lambda-list
                               (append (subseq combination-arguments-lambda-list 0 i)
                                       '(&ALLOW-OTHER-KEYS)
                                       (subseq combination-arguments-lambda-list i)))))
                     (let* ((ll-req-num (length reqvars))
                            (ll-opt-num (length optvars))
                            (signature (generic-function-signature generic-function))
                            (gf-req-num (sig-req-num signature))
                            (gf-opt-num (sig-opt-num signature)))
                       ;; "If the section of the :arguments lambda-list is
                       ;;  shorter, extra arguments are ignored."
                       (when (< ll-req-num gf-req-num)
                         (setq apply-args (append (subseq apply-args 0 ll-req-num)
                                                  (subseq apply-args gf-req-num))))
                       ;; "If the section of the :arguments lambda-list is
                       ;;  longer, excess required parameters are bound to
                       ;;  forms that evaluate to nil and excess optional
                       ;;  parameters are bound to their initforms."
                       (when (> ll-req-num gf-req-num)
                         (setq apply-args (append (subseq apply-args 0 gf-req-num)
                                                  (make-list (- ll-req-num gf-req-num)
                                                             :initial-element 'NIL)
                                                  (subseq apply-args gf-req-num))))
                       ;; Now the required parameters section of apply-args
                       ;; has length ll-req-num.
                       ;; Likewise for the &optional section.
                       (when (< ll-opt-num gf-opt-num)
                         (let* ((has-&optional (eq (nth ll-req-num combination-arguments-lambda-list) '&OPTIONAL))
                                (i (+ ll-req-num (if has-&optional 1 0) ll-opt-num)))
                           (setq combination-arguments-lambda-list
                                 (append (subseq combination-arguments-lambda-list 0 i)
                                         (if has-&optional '() '(&OPTIONAL))
                                         (gensym-list (- gf-opt-num ll-opt-num))
                                         (subseq combination-arguments-lambda-list i)))))
                       (when (> ll-opt-num gf-opt-num)
                         ;; In this case we have to split the one lambda into
                         ;; two or three ones.
                         ;; Outermost lambda: the required and present optional
                         ;;                   variables.
                         ;; Inner lambda: The missing optional variables.
                         ;; Innermost lambda: The &rest/&key variables.
                         (let ((combination-arguments-rest
                                 (subseq combination-arguments-lambda-list (+ ll-req-num 1 ll-opt-num)))
                               (apply-args-rest (subseq apply-args ll-req-num)))
                           (when (member (first combination-arguments-rest) '(&REST &KEY) :test #'eq)
                             (setq wrapped-ef-form
                                   `(,apply-fun #'(LAMBDA ,(append (if (> gf-opt-num 0) '(&OPTIONAL) '())
                                                                   (gensym-list gf-opt-num)
                                                                   combination-arguments-rest)
                                                    ,@declarations
                                                    ,wrapped-ef-form)
                                                ,@apply-args-rest))
                             (setq combination-arguments-lambda-list
                                   (subseq combination-arguments-lambda-list 0 (+ ll-req-num 1 ll-opt-num))))
                           (setq wrapped-ef-form
                                 `(FUNCALL #'(LAMBDA (&OPTIONAL ,@(subseq combination-arguments-lambda-list (+ ll-req-num 1 gf-opt-num)))
                                               ,@declarations
                                               ,wrapped-ef-form)))
                           (setq combination-arguments-lambda-list
                                 (subseq combination-arguments-lambda-list 0 (+ ll-req-num 1 gf-opt-num)))
                           (when (member (first combination-arguments-rest) '(&REST &KEY) :test #'eq)
                             (setq combination-arguments-lambda-list
                                   (append combination-arguments-lambda-list `(&REST ,(gensym)))))))
                       ;; When lambdalist has &rest or &key but combination-arguments-lambda-list
                       ;; doesn't, add a dummy &rest variable to it.
                       (when (and (eq apply-fun 'APPLY)
                                  (not (or (not (eql rest 0)) keyp)))
                         (let ((i (or (position '&AUX combination-arguments-lambda-list)
                                      (length combination-arguments-lambda-list))))
                           (setq combination-arguments-lambda-list
                                 (append (subseq combination-arguments-lambda-list 0 i)
                                         `(&REST ,(gensym))
                                         (subseq combination-arguments-lambda-list i)))))
                       ;; "&whole var can be placed first in the :arguments lambda-list."
                       (when whole-form
                         (setq combination-arguments-lambda-list
                               (cons whole-var combination-arguments-lambda-list))
                         (setq apply-args (cons whole-form apply-args)))
                       (setq wrapped-ef-form
                             `(,apply-fun #'(LAMBDA ,combination-arguments-lambda-list
                                              ,@declarations
                                              ,wrapped-ef-form)
                                          ,@apply-args))))))
               (when generic-function-variable
                 (setq wrapped-ef-form
                       `(LET ((,generic-function-variable ',generic-function))
                          ,@declarations
                          ,wrapped-ef-form)))
               `#'(LAMBDA ,lambdalist
                    ,@declarations
                    ,@firstforms
                    ,wrapped-ef-form))))
      ef-fun)))

(defun compute-effective-method-as-function-form (gf combination methods)
  ;; Call the customizable compute-effective-method from the MOP. (No need to
  ;; verify that it produces exactly two values: Many user-defined methods
  ;; probably return just the first value, letting the second value default
  ;; to empty.)
  (multiple-value-bind (effective-method-form effective-method-options)
      (clos:compute-effective-method gf combination methods)
    ;; Build a function form around the inner form:
    (build-effective-method-function-form gf combination methods
      effective-method-form
      (let ((option (assoc ':ARGUMENTS effective-method-options)))
        (if option
          (check-em-arguments-option option 'compute-discriminating-function gf)
          '()))
      ;; Supporting the :GENERIC-FUNCTION effective-method option here is
      ;; is useless, since COMPUTE-EFFECTIVE-METHOD has been passed the
      ;; generic function as argument, and COMPUTE-EFFECTIVE-METHOD could just
      ;; use this generic function object (quoted or not, doesn't matter, since
      ;; it's self-evaluating) instead of introducing a variable. But the MOP
      ;; p. 42 talks about it, and it increases consistency with the
      ;; DEFINE-METHOD-COMBINATION macro, so let's support it.
      (let ((option (assoc ':GENERIC-FUNCTION effective-method-options)))
        (if option
          (check-em-generic-function-option option 'compute-discriminating-function gf)
          nil))
      ;; :DUPLICATES is a CLISP specific extension.
      #+CLISP
      (let ((option (assoc ':DUPLICATES effective-method-options)))
        (if option
          (check-em-duplicates-option option 'compute-discriminating-function gf)
          '()))
      #-CLISP '())))

(defun no-applicable-method-caller (gf)
  #'(lambda (&rest args) (apply #'no-applicable-method gf args)))

(defun clos::compute-effective-method-as-function (gf methods args)
  (declare (ignore args))
  (when (null methods)
    (return-from clos::compute-effective-method-as-function
      (no-applicable-method-caller gf)))
  ;; Apply method combination:
  (let ((ef-fun (compute-effective-method-as-function-form gf (clos:generic-function-method-combination gf) methods)))
    ;; Evaluate or compile the resulting form:
    (if (constantp ef-fun) ; constant or self-evaluating form?
      ;; No need to invoke the compiler for a constant form.
      ef-fun
      ;; For a general form:
      ;; (eval ef-fun)                                 ; interpreted
      ;; (eval `(LOCALLY (DECLARE (COMPILE)) ,ef-fun)) ; compiled
      (funcall
        (compile nil
          `(LAMBDA ()
             (DECLARE (INLINE FUNCALL APPLY)
                      #+SBCL (SB-EXT:DISABLE-PACKAGE-LOCKS CALL-METHOD))
             ,ef-fun))))))

) ; progn

#| ;; Test case:
(defgeneric foo (x))
(defmethod foo ((x integer)) (* x x))
(funcall (clos::compute-effective-method-as-function #'foo (clos:generic-function-methods #'foo) '(10)) 10)
=> 100
|#

;; ============================================================================
