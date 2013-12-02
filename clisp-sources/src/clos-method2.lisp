;;;; Common Lisp Object System for CLISP: Methods
;;;; Bruno Haible 21.8.1993 - 2004
;;;; Sam Steingold 1998 - 2004
;;;; German comments translated into English: Stefan Kain 2002-04-08

(in-package "CLOS")

;; ANSI CL 3.4.3. Specialized Lambda Lists
;; Decompose a specialized lambda list into
;; 1. an ordinary lambda list,
;; 2. a list of specializers for the required arguments,
;; 3. a list of ignorable required parameters.
(defun decompose-specialized-lambda-list (specialized-lambda-list errfunc)
  (let ((remaining-lambda-list specialized-lambda-list)
        (req-vars '())
        (ignorable-req-vars '())
        (spec-list '()))
    (do ()
        ((or (atom remaining-lambda-list)
             (memq (car remaining-lambda-list) lambda-list-keywords)))
      (let ((item (pop remaining-lambda-list)))
        ;; item can be a variable or of the form (variable specializer-name).
        ;; ANSI CL 3.4.3. also allows the syntax (variable), but the DEFMETHOD
        ;; description forbids it, and the DEFGENERIC description refers to it.
        (if (atom item)
          (progn
            (push item req-vars)
            (push 't spec-list))
          (if (and (consp (cdr item)) (null (cddr item)))
            (progn
              (push (first item) req-vars)
              (push (first item) ignorable-req-vars) ; CLtL2 p. 840 top
              (push (second item) spec-list))
            (funcall errfunc item
              (TEXT "Invalid specialized parameter in method lambda list ~S: ~S")
              specialized-lambda-list item)))))
    (let ((lambda-list (nreconc req-vars remaining-lambda-list)))
      (analyze-lambdalist lambda-list errfunc)
      (values lambda-list (nreverse spec-list) (nreverse ignorable-req-vars)))))

;; helper
(defmacro program-error-reporter (caller)
  `#'(lambda (detail errorstring &rest arguments)
       (declare (ignore detail))
       (error-of-type 'program-error
         (TEXT "~S: ~A") ,caller
         (apply #'format nil errorstring arguments))))

;; MOP p. 52
(defun extract-lambda-list (specialized-lambda-list)
  (nth-value 0
    (decompose-specialized-lambda-list
      specialized-lambda-list
      (program-error-reporter 'extract-lambda-list))))

;; MOP p. 53
(defun extract-specializer-names (specialized-lambda-list)
  (nth-value 1
    (decompose-specialized-lambda-list
      specialized-lambda-list
      (program-error-reporter 'extract-specializer-names))))

;;; For DEFMETHOD, DEFGENERIC, GENERIC-FUNCTION, GENERIC-FLET,
;;;     GENERIC-LABELS, WITH-ADDED-METHODS
;; caller: symbol
;; whole-form: whole source form
;; funname: function name, symbol or (SETF symbol)
;; description: (qualifier* spec-lambda-list {declaration|docstring}* form*)
;; ==>
;; 1. a function lambda, to be applied to a backpointer cons (that will later
;;    point back to the method object), that returns a cons h with
;;    (car h) = fast-function,
;;    (cadr h) = true if the compiler could optimize away the ",cont" variable.
;; 2. method-initargs-forms,
;; 3. signature
(defun analyze-method-description (caller whole-form funname description)
  ;; Collect the qualifiers:
  (let ((qualifiers nil))
    (loop
      (when (atom description)
        (error-of-type 'ext:source-program-error
          :form whole-form
          :detail description
          (TEXT "~S ~S: missing lambda list")
          caller funname))
      (when (listp (car description)) (return))
      (push (pop description) qualifiers))
    (setq qualifiers (nreverse qualifiers))
    ;; Build lambdalist, extract parameter-specializers and signature:
    (let ((specialized-lambda-list (car description))
          (body (cdr description)))
      (multiple-value-bind (lambda-list spec-list ignorable-req-vars)
          (decompose-specialized-lambda-list specialized-lambda-list
            #'(lambda (detail errorstring &rest arguments)
                (error-of-type 'ext:source-program-error
                  :form whole-form
                  :detail detail
                  (TEXT "~S ~S: ~A")
                  caller funname
                  (apply #'format nil errorstring arguments))))
        (let ((req-specializer-forms
                (mapcar #'(lambda (specializer-name)
                            (cond ((defined-class-p specializer-name)
                                   `',specializer-name)
                                  ((symbolp specializer-name)
                                   `(FIND-CLASS ',specializer-name))
                                  ((and (consp specializer-name)
                                        (eq (car specializer-name) 'EQL)
                                        (consp (cdr specializer-name))
                                        (null (cddr specializer-name)))
                                   `(INTERN-EQL-SPECIALIZER ,(second specializer-name)))
                                  (t (error-of-type 'ext:source-program-error
                                       :form whole-form
                                       :detail specializer-name
                                       (TEXT "~S ~S: Invalid specializer ~S in lambda list ~S")
                                       caller funname specializer-name specialized-lambda-list))))
                        spec-list)))
          (sys::check-redefinition
            (list* funname qualifiers spec-list) caller
            ;; do not warn about redefinition when no method was defined
            (and (fboundp 'find-method) (fboundp funname)
                 (typep-class (fdefinition funname) <generic-function>)
                 (not (safe-gf-undeterminedp (fdefinition funname)))
                 (eql (sig-req-num (safe-gf-signature (fdefinition funname))) (length spec-list))
                 (find-method (fdefinition funname) qualifiers spec-list nil)
                 "method"))
          (multiple-value-bind (reqvars optvars optinits optsvars rest
                                keyp keywords keyvars keyinits keysvars
                                allowp auxvars auxinits)
              (analyze-lambdalist lambda-list
                #'(lambda (detail errorstring &rest arguments)
                    (error-of-type 'ext:source-program-error
                      :form whole-form
                      :detail detail
                      (TEXT "~S ~S: ~A")
                      caller funname
                      (apply #'format nil errorstring arguments))))
            (declare (ignore optinits optsvars keyvars keyinits keysvars
                             auxvars auxinits))
            (let ((reqnum (length reqvars))
                  (optnum (length optvars))
                  (restp (or keyp (not (eql rest 0))))
                  (weakened-lambda-list lambda-list))
              ;; Methods have an implicit &allow-other-keys (CLtL2 28.1.6.4., ANSI CL 7.6.4.):
              (when (and keyp (not allowp))
                (let ((index (+ (position '&KEY lambda-list :test #'eq) 1 (length keywords))))
                  (setq weakened-lambda-list
                    `(,@(subseq lambda-list 0 index) &ALLOW-OTHER-KEYS
                      ,@(subseq lambda-list index)))))
              (let* ((backpointer (gensym))
                     (compile nil)
                     (documentation nil)
                     (lambdabody
                       (multiple-value-bind (body-rest declarations docstring)
                           (sys::parse-body body t)
                         (setq compile (member '(COMPILE) declarations :test #'equal))
                         (setq documentation docstring)
                         (when ignorable-req-vars
                           (push `(IGNORABLE ,@ignorable-req-vars) declarations))
                         (let ((lambdabody-part1
                                `(,weakened-lambda-list
                                  ,@(if declarations `((DECLARE ,@declarations)))))
                               (lambdabody-part2
                                 (if (eq caller 'generic-function)
                                   body-rest
                                   ;; implicit block
                                   `((BLOCK ,(function-block-name funname)
                                       ,@body-rest)))))
                           (let ((cont (gensym)) ; variable for the continuation
                                 (req-dummies (gensym-list reqnum))
                                 (rest-dummy (if (or restp (> optnum 0)) (gensym)))
                                 (lambda-expr `(LAMBDA ,@lambdabody-part1 ,@lambdabody-part2)))
                             `(; new lambda-list:
                               (,cont
                                ,@req-dummies
                                ,@(if rest-dummy `(&REST ,rest-dummy) '()))
                               ,(add-next-method-local-functions
                                  backpointer cont req-dummies rest-dummy
                                  ;; new body:
                                  (list
                                    (if rest-dummy
                                      `(APPLY (FUNCTION ,lambda-expr)
                                              ,@req-dummies ,rest-dummy)
                                      `(,lambda-expr ,@req-dummies)))))))))
                     (sig (make-signature :req-num reqnum :opt-num optnum
                                          :rest-p restp :keys-p keyp
                                          :keywords keywords :allow-p allowp)))
                (values
                  `(LAMBDA (,backpointer)
                     ,@(if compile '((DECLARE (COMPILE))))
                     (%OPTIMIZE-FUNCTION-LAMBDA (T) ,@lambdabody))
                  `(:QUALIFIERS ',qualifiers
                    :LAMBDA-LIST ',lambda-list
                    'SIGNATURE ,sig
                    :SPECIALIZERS (LIST ,@req-specializer-forms)
                    ,@(if documentation `(:DOCUMENTATION ',documentation))
                    ,@(if (eq caller 'DEFGENERIC) `('FROM-DEFGENERIC T)))
                  sig)))))))))
