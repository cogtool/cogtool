(common-lisp:in-package "SYSTEM")
;; ----------------------------------------------------------------------------
(defmacro typecase (&whole whole-form
                    keyform &rest typeclauselist)
  (let* ((tempvar (gensym))
         (condclauselist nil))
    (do ((typeclauselistr typeclauselist (cdr typeclauselistr)) spec)
        ((atom typeclauselistr))
      (cond ((atom (car typeclauselistr))
             (error-of-type 'source-program-error
               :form whole-form
               :detail (car typeclauselistr)
               (TEXT "Invalid clause in ~S: ~S")
               'typecase (car typeclauselistr)))
            ((eq (setq spec (caar typeclauselistr)) T)
             (push `(T ,@(or (cdar typeclauselistr) '(NIL))) condclauselist)
             (return))
            ((and (atom (cdr typeclauselistr)) (eq spec 'OTHERWISE))
             (push `(T ,@(or (cdar typeclauselistr) '(NIL))) condclauselist))
            (t (push `((TYPEP ,tempvar (QUOTE ,spec))
                       ,@(or (cdar typeclauselistr) '(NIL)))
                     condclauselist))))
    `(LET ((,tempvar ,keyform)) (COND ,@(nreverse condclauselist)))))
;; ----------------------------------------------------------------------------
(defun type-error-string () ; ABI
  (TEXT "~A~%The value is: ~S"))
(defun check-type-error-string (place string typespec) ; ABI
  (format nil
    (TEXT "The value of ~S should be ~:[of type ~S~;~:*~A~].")
    place string typespec))
(defun report-one-new-value-string () ; ABI
  (TEXT "You may input a new value for ~S."))
(defun report-one-new-value-string-instead ()
  (TEXT "You may input a value to be used instead~@[ of ~S~]."))
(defun prompt-for-new-value-string () ; ABI
  (concatenate 'string "~&" (TEXT "New ~S: ")))
(predefmacro check-type (place typespec &optional (string nil))
  (let ((tag1 (gensym))
        (tag2 (gensym)))
    `(TAGBODY
       ,tag1
       (WHEN (TYPEP ,place ',typespec) (GO ,tag2))
       (CERROR (REPORT-ONE-NEW-VALUE-STRING)
         (TYPE-ERROR-STRING)
         (CHECK-TYPE-ERROR-STRING ',place ,string ',typespec)
         ,place)
       (FORMAT *QUERY-IO* (PROMPT-FOR-NEW-VALUE-STRING) ',place)
       (SETF ,place (READ *QUERY-IO*))
       (GO ,tag1)
       ,tag2)))
;; ----------------------------------------------------------------------------
(defun report-no-new-value-string () ; ABI
  (TEXT "Retry"))
(defun report-new-values-string () ; ABI
  (TEXT "You may input new values for ~@{~S~^, ~}."))
(defun assert-error-string (test-form) ; ABI
  (format nil
    (TEXT "~S must evaluate to a non-NIL value.")
    test-form))
(predefmacro assert (test-form &optional (place-list nil) (string nil) &rest args)
  (let ((tag1 (gensym))
        (tag2 (gensym)))
    `(TAGBODY
       ,tag1
       (WHEN ,test-form (GO ,tag2))
       (CERROR ,(case (length place-list)
                  (0 `(REPORT-NO-NEW-VALUE-STRING))
                  (1 `(APPLY #'FORMAT NIL (REPORT-ONE-NEW-VALUE-STRING) ',place-list))
                  (t `(APPLY #'FORMAT NIL (REPORT-NEW-VALUES-STRING) ',place-list)))
               ',(or string "~A")
               ,@(if string
                   args
                   (list `(ASSERT-ERROR-STRING ',test-form))))
       ,@(mapcan
           #'(lambda (place)
               (list `(FORMAT *QUERY-IO* (PROMPT-FOR-NEW-VALUE-STRING) ',place)
                     `(SETF ,place (READ *QUERY-IO*))))
           place-list)
       (GO ,tag1)
       ,tag2)))
;; ----------------------------------------------------------------------------
(defun typecase-error-string (keyform typelist) ; ABI
  (format nil
    (TEXT "The value of ~S must be of one of the types ~{~S~^, ~}")
    keyform typelist))
(defun case-error-string (keyform caselist) ; ABI
  (format nil
    (TEXT "The value of ~S must be one of ~{~S~^, ~}")
    keyform caselist))

;;; Exhaustive Case Analysis

;; These macros are superseded by the corresponding ones from condition.lisp
;; when the condition system is available.

(flet ((parenthesize-keys (clauses)
         ;; PARENTHESIZE-KEYS is necessary to avoid confusing
         ;; the symbols OTHERWISE and T used as keys, with the same
         ;; symbols used in the syntax of the non exhaustive CASE.
         (mapcar #'(lambda (c)
                     (cond ((or (eq (car c) 't)
                                (eq (car c) 'otherwise))
                            (warn (TEXT "~S used as a key in ~S, it would be better to use parentheses.")
                                  (car c) c)
                            (cons (list (car c)) (cdr c)))
                           (t c)))
                 clauses))
       (case-list (clause-list)
         (mapcap #'(lambda (key-clause)
                     (setq key-clause (car key-clause))
                     (if (listp key-clause) key-clause (list key-clause)))
                 clause-list)))
  (flet ((typecase-errorstring (keyform keyclauselist)
           (let ((typelist (mapcar #'first keyclauselist)))
             `(TYPECASE-ERROR-STRING ',keyform ',typelist)))
         (typecase-expected-type (keyclauselist)
           `(OR ,@(mapcar #'first keyclauselist)))
         (case-errorstring (keyform keyclauselist)
           (let ((caselist (case-list keyclauselist)))
             `(CASE-ERROR-STRING ',keyform ',caselist)))
         (case-expected-type (keyclauselist)
           `(MEMBER ,@(case-list keyclauselist)))
         (simply-error (casename form clauselist errorstring expected-type)
           (let ((var (gensym)))
             `(LET ((,var ,form))
                (,casename ,var
                  ,@(parenthesize-keys clauselist)
                  (OTHERWISE
                    (ERROR-OF-TYPE 'TYPE-ERROR
                      :DATUM ,var :EXPECTED-TYPE ',expected-type
                      (TYPE-ERROR-STRING)
                      ,errorstring ,var))))))
         (retry-loop (casename place clauselist errorstring)
           (let ((g (gensym))
                 (h (gensym)))
             `(BLOCK ,g
                (TAGBODY
                  ,h
                  (RETURN-FROM ,g
                    (,casename ,place
                      ,@(parenthesize-keys clauselist)
                      (OTHERWISE
                        (CERROR (REPORT-ONE-NEW-VALUE-STRING)
                                (TYPE-ERROR-STRING)
                                ,errorstring
                                ,place)
                        (FORMAT *QUERY-IO* (PROMPT-FOR-NEW-VALUE-STRING)
                                ',place)
                        (SETF ,place (READ *QUERY-IO*))
                        (GO ,h)))))))))
    (predefmacro etypecase (keyform &rest keyclauselist)
      (if (assoc t keyclauselist)
          `(typecase ,keyform ,@keyclauselist)
          (simply-error 'TYPECASE keyform keyclauselist
                        (typecase-errorstring keyform keyclauselist)
                        (typecase-expected-type keyclauselist))))
    (predefmacro ctypecase (keyplace &rest keyclauselist)
      (if (assoc t keyclauselist)
          `(typecase ,keyplace ,@keyclauselist)
          (retry-loop 'TYPECASE keyplace keyclauselist
                      (typecase-errorstring keyplace keyclauselist))))
    (predefmacro ecase (keyform &rest keyclauselist)
      (simply-error 'CASE keyform keyclauselist
                    (case-errorstring keyform keyclauselist)
                    (case-expected-type keyclauselist)))
    (predefmacro ccase (keyform &rest keyclauselist)
      (retry-loop 'CASE keyform keyclauselist
                  (case-errorstring keyform keyclauselist)))))
;; ----------------------------------------------------------------------------
(defmacro deftype (&whole whole-form
                   name lambdalist &body body)
  (unless (symbolp name)
    (error-of-type 'source-program-error
      :form whole-form
      :detail name
      (TEXT "type name should be a symbol, not ~S")
      name))
  (if (or (get name 'TYPE-SYMBOL) (get name 'TYPE-LIST))
    (error-of-type 'source-program-error
      :form whole-form
      :detail name
      (TEXT "~S is a built-in type and may not be redefined.")
      name))
  (multiple-value-bind (body-rest declarations docstring)
      (SYSTEM::PARSE-BODY body t)
    (if declarations (setq declarations (list (cons 'DECLARE declarations))))
    (let ((%whole-form whole-form)
          (%arg-count 0) (%min-args 0) (%restp nil) (%null-tests nil)
          (%let-list nil) (%keyword-tests nil) (%default-form '(QUOTE *)))
      (analyze1 lambdalist '(CDR <DEFTYPE-FORM>) name '<DEFTYPE-FORM>)
      (let ((lengthtest (make-length-test '<DEFTYPE-FORM>))
            (mainform `(LET* ,(nreverse %let-list)
                         ,@declarations
                         ,@(nreverse %null-tests)
                         ,@(nreverse %keyword-tests)
                         (BLOCK ,name ,@body-rest))))
        (if lengthtest
          (setq mainform
            `(IF ,lengthtest
               (TYPE-CALL-ERROR <DEFTYPE-FORM>)
               ,mainform)))
        `(EVAL-WHEN (COMPILE LOAD EVAL)
           (LET ()
             (%PUT ',name 'DEFTYPE-EXPANDER
               (FUNCTION ,(make-symbol (string-concat "DEFTYPE-" (string name)))
                 (LAMBDA (<DEFTYPE-FORM>) ,mainform)))
             (SYS::%SET-DOCUMENTATION ',name 'TYPE ',docstring)
             ',name))))))
(defun type-call-error (deftype-form) ; ABI
  (error-of-type 'error
    (TEXT "The deftype expander for ~S may not be called with ~S arguments.")
    (car deftype-form) (1- (length deftype-form))))
;; ----------------------------------------------------------------------------
;; cf. X3J13 vote <173>
(defmacro define-symbol-macro (&whole whole-form
                               symbol expansion)
  (unless (symbolp symbol)
    (error-of-type 'source-program-error
      :form whole-form
      :detail symbol
      (TEXT "~S: the name of a symbol macro must be a symbol, not ~S")
      'define-symbol-macro symbol))
  `(LET ()
     (EVAL-WHEN (COMPILE LOAD EVAL)
       (CHECK-NOT-SPECIAL-VARIABLE-P ',symbol)
       (SYSTEM::%PUT ',symbol 'SYSTEM::SYMBOLMACRO
                     (SYSTEM::MAKE-GLOBAL-SYMBOL-MACRO ',expansion))
       (SYSTEM::%PROCLAIM-SYMBOL-MACRO ',symbol))
     ',symbol))
(defun check-not-special-variable-p (symbol) ; ABI
  (when (special-variable-p symbol)
    (error-of-type 'program-error
      (TEXT "~S: the symbol ~S names a global variable")
      'define-symbol-macro symbol)))
;; ----------------------------------------------------------------------------
;; X3J13 vote <123>
;; Macro (nth-value n form) == (nth n (multiple-value-list form)), CLtL2 S. 184
(defmacro nth-value (n form)
  (if (and (integerp n) (>= n 0))
    (if (< n (1- multiple-values-limit))
      (if (= n 0)
        `(PROG1 ,form)
        (let* ((resultvar (gensym))
               (vars (list resultvar))
               (ignores '()))
          (do ((i n (1- i)))
              ((zerop i))
            (let ((g (gensym)))
              (setq vars (cons g vars))
              (setq ignores (cons g ignores))
          ) )
          `(MULTIPLE-VALUE-BIND ,vars ,form
             (DECLARE (IGNORE ,@ignores))
             ,resultvar
           )
      ) )
      `(PROGN ,form NIL)
    )
    `(NTH ,n (MULTIPLE-VALUE-LIST ,form))
) )
;; ----------------------------------------------------------------------------
(defun gensym-list (how-many)
  (map-into (make-list (if (numberp how-many) how-many (length how-many)))
            #'gensym))
;; ----------------------------------------------------------------------------
(defmacro time (form)
  (let ((vars (gensym-list 9)))
    `(MULTIPLE-VALUE-BIND ,vars (%%TIME)
      ;; this construction uses stack space at run time only
      (UNWIND-PROTECT ,form (MULTIPLE-VALUE-CALL #'%TIME (%%TIME) ,@vars)))))
;; ----------------------------------------------------------------------------
(defmacro times (form)
  (let ((var1 (gensym))
        (var2 (gensym))
        (var3 (gensym))
        (var4 (gensym))
        (timevars1 (gensym-list 9))
        (timevars2 (gensym-list 9)))
    (setq form
          `(PROGN
             (MULTIPLE-VALUE-SETQ ,timevars1 (%%TIME))
             (UNWIND-PROTECT
               ,form
               (MULTIPLE-VALUE-SETQ ,timevars2 (%%TIME)))))
    `(MULTIPLE-VALUE-BIND (,var1 ,var2 ,var3 ,var4 ,@timevars1 ,@timevars2)
         (%SPACE1)
       ;; this construction uses stack space at run time only
       (UNWIND-PROTECT
         (LET ((*GC-STATISTICS* (1+ (MAX *GC-STATISTICS* 0))))
           (UNWIND-PROTECT
             (SETQ ,var3 (MULTIPLE-VALUE-LIST ,form))
             (SETQ ,var4 (%SPACE2))))
         (%SPACE ,var1 ,var2 ,var3 ,var4)
         (%TIME ,@timevars2 ,@timevars1))
       (VALUES-LIST ,var3))))
;; ----------------------------------------------------------------------------
(defmacro with-input-from-string
    ((var string &key (index nil sindex) (start '0 sstart) (end 'NIL send))
     &body body)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body)
    `(LET ((,var (MAKE-STRING-INPUT-STREAM ,string
                   ,@(if (or sstart send)
                       `(,start ,@(if send `(,end) '()))
                       '()))))
       (DECLARE (READ-ONLY ,var) ,@declarations)
       (UNWIND-PROTECT
         (PROGN ,@body-rest)
         ,@(if sindex
             `((SETF ,index (SYSTEM::STRING-INPUT-STREAM-INDEX ,var))) '())
         (CLOSE ,var)))))
;; ----------------------------------------------------------------------------
(defmacro with-open-file ((stream &rest options) &body body)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body)
    `(LET ((,stream (OPEN ,@options)))
       (DECLARE (READ-ONLY ,stream) ,@declarations)
       (UNWIND-PROTECT (MULTIPLE-VALUE-PROG1 (PROGN ,@body-rest)
                         (WHEN ,stream (CLOSE ,stream)))
         (WHEN ,stream (CLOSE ,stream :ABORT T))))))
;; ----------------------------------------------------------------------------
(defmacro with-open-stream ((var stream) &body body)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body)
    `(LET ((,var ,stream))
       (DECLARE (READ-ONLY ,var) ,@declarations)
       (UNWIND-PROTECT (MULTIPLE-VALUE-PROG1 (PROGN ,@body-rest)
                         (when ,var (CLOSE ,var)))
         (WHEN ,var (CLOSE ,var :ABORT T))))))
;; ----------------------------------------------------------------------------
(defmacro with-output-to-string ((var &optional (string nil)
                                  &key (element-type ''CHARACTER))
                                 &body body)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body)
    (if string
      (let ((ignored-var (gensym)))
        `(LET ((,var (SYS::MAKE-STRING-PUSH-STREAM ,string))
               (,ignored-var ,element-type))
           (DECLARE (READ-ONLY ,var) (IGNORE ,ignored-var) ,@declarations)
           (UNWIND-PROTECT
             (PROGN ,@body-rest)
             (CLOSE ,var))))
      `(LET ((,var (MAKE-STRING-OUTPUT-STREAM :ELEMENT-TYPE ,element-type)))
         (DECLARE (READ-ONLY ,var) ,@declarations)
         (UNWIND-PROTECT
           (PROGN ,@body-rest (GET-OUTPUT-STREAM-STRING ,var))
           (CLOSE ,var))))))
;; ----------------------------------------------------------------------------
;; X3J13 vote <40>
(defmacro print-unreadable-object
          ((&whole args object stream &key type identity) &body body)
  (declare (ignore object stream type identity))
  `(SYSTEM::WRITE-UNREADABLE
     ,(if body `(FUNCTION (LAMBDA () ,@body)) 'NIL)
     ,@args))
;; ----------------------------------------------------------------------------
(in-package "EXT")
(export '(space with-output-to-printer))
(in-package "SYSTEM")
(defmacro with-output-to-printer ((var &rest options &key external-format)
                                  &body body)
  (declare (ignore external-format))
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body)
    (if declarations
      (setq declarations (list (cons 'DECLARE declarations))))
    `(LET ((,var (SYS::MAKE-PRINTER-STREAM ,@options)))
       ,@declarations
       (UNWIND-PROTECT
         (PROGN ,@body-rest)
         (CLOSE ,var)))))
#+UNIX
(defun make-printer-stream (&key (external-format :default)) ; ABI
  (make-pipe-output-stream "lpr" :external-format external-format))
#+WIN32
(defun make-printer-stream (&key (external-format :default)) ; ABI
  (open "prn" :direction :output :external-format external-format))
;; ----------------------------------------------------------------------------
(in-package "EXT")
(export 'without-floating-point-underflow)
(in-package "SYSTEM")
(defmacro without-floating-point-underflow (&body body)
  `(let ((SYS::*INHIBIT-FLOATING-POINT-UNDERFLOW* T))
    ;; need `progn' to signal an error when `body' starts with a declaration
    (progn ,@body)))
;; ----------------------------------------------------------------------------

