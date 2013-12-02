;;; ANSI-compatible definitions
;;; Bruno Haible 21.7.1994
;;; Sam Steingold 1999-2004

;; ============================================================================

(in-package "COMMON-LISP")
(export '(declaim destructuring-bind complement
          constantly with-standard-io-syntax with-hash-table-iterator
          read-sequence write-sequence))
(in-package "SYSTEM")

;; ----------------------------------------------------------------------------

;; X3J13 vote <144>

(defmacro declaim (&rest decl-specs)
  `(PROGN
     ,@(mapcar #'(lambda (decl-spec) `(PROCLAIM (QUOTE ,decl-spec))) decl-specs)
   )
)

;; ----------------------------------------------------------------------------

;; X3J13 vote <64>

(defmacro destructuring-bind (&whole whole-form
                              lambdalist form &body body)
  (multiple-value-bind (body-rest declarations) (system::parse-body body)
    (if declarations (setq declarations `((DECLARE ,@declarations))))
    (let ((%whole-form whole-form)
          (%arg-count 0) (%min-args 0) (%restp nil) (%ignored nil)
          (%let-list nil) (%keyword-tests nil) (%default-form nil))
      (analyze1 lambdalist '<DESTRUCTURING-FORM> 'destructuring-bind '<DESTRUCTURING-FORM>)
      (let ((lengthtest (make-length-test '<DESTRUCTURING-FORM> 0))
            (mainform `(LET* ,(nreverse %let-list)
                         ,@declarations
                         ,@(if %ignored `((DECLARE (IGNORE ,%ignored))))
                         ,@(nreverse %keyword-tests)
                         ,@body-rest
           ))          )
        (if lengthtest
          (setq mainform
            `(IF ,lengthtest
               (DESTRUCTURING-ERROR <DESTRUCTURING-FORM>
                                    '(,%min-args . ,(if %restp nil %arg-count))
               )
               ,mainform
        ) )  )
        `(LET ((<DESTRUCTURING-FORM> ,form)) ,mainform)
) ) ) )

(defun destructuring-error (destructuring-form min.max) ; ABI
  (let ((min (car min.max))
        (max (cdr min.max)))
    (error-of-type 'program-error
      (TEXT "The object to be destructured should be a list with ~:[at least ~*~S~;~:[from ~S to ~S~;~S~]~] elements, not ~4@*~S.")
      max (eql min max) min max destructuring-form
) ) )

;; Like destructuring-bind, except that it works only with ordinary
;; lambda-lists and generates more efficient code.
;; Compare   (disassemble #'(lambda (l) (destructuring-bind (&optional (o 'def)) l o)))
;; with      (disassemble #'(lambda (l) (simple-destructuring-bind (&optional (o 'def)) l o)))
(defmacro simple-destructuring-bind (lambdalist form &body body)
  `(APPLY #'(LAMBDA ,lambdalist ,@body) ,form))

;; ----------------------------------------------------------------------------

;; X3J13 vote <87>

(defun complement (fun)
  #'(lambda (&rest arguments) (not (apply fun arguments)))
)

;; ANSI-CL

(defun constantly (object)
  #'(lambda (&rest arguments) (declare (ignore arguments)) object)
)

;; ----------------------------------------------------------------------------

;; part of X3J13 vote <40>

(defconstant *common-lisp-user-package* (find-package "COMMON-LISP-USER")) ; ABI

(defmacro with-standard-io-syntax (&body body)
  ;; ANSI CL does not allow declarations at the beginning of the body, but
  ;; we do, as an extension.
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body)
    ;; It would be possible to put all these bindings into a single function,
    ;; but this would force variables into closures.
    `(LET (;; printer/reader variables:
           (*PACKAGE*                   *COMMON-LISP-USER-PACKAGE*)
           ;; printer variables:
           (*PRINT-ARRAY*               T)
           (*PRINT-BASE*                10)
           (*PRINT-CASE*                ':UPCASE)
           (*PRINT-CIRCLE*              NIL)
           (*PRINT-ESCAPE*              T)
           (*PRINT-GENSYM*              T)
           (*PRINT-LENGTH*              NIL)
           (*PRINT-LEVEL*               NIL)
           (*PRINT-LINES*               NIL)
           (*PRINT-MISER-WIDTH*         NIL)
           (*PRINT-PPRINT-DISPATCH*     NIL)
           (*PRINT-PRETTY*              NIL)
           (*PRINT-RADIX*               NIL)
           (*PRINT-READABLY*            T)
           (*PRINT-RIGHT-MARGIN*        NIL)
           (*PRINT-CLOSURE*             NIL) ; CLISP specific
           (*PRINT-RPARS*               nil) ; CLISP specific
           (*PRINT-INDENT-LISTS*        1)   ; CLISP specific
           (SYSTEM::*PRIN-STREAM*       NIL) ; CLISP specific
           (SYSTEM::*PRIN-LINELENGTH*   79)  ; CLISP specific
           (SYSTEM::*PRIN-LINE-PREFIX*  NIL) ; CLISP specific
           (COMPILER::*LOAD-FORMS*      NIL) ; CLISP specific
           ;; reader variables:
           (*READ-BASE*                 10)
           (*READ-DEFAULT-FLOAT-FORMAT* 'SINGLE-FLOAT)
           (*READ-EVAL*                 T)
           (*READ-SUPPRESS*             NIL)
           (*READTABLE*                 (COPY-READTABLE NIL)))
       ,@(if declarations `((DECLARE ,@declarations)))
       ,@body-rest)))

;; ----------------------------------------------------------------------------

;; part of X3J13 vote <98>

(defmacro with-hash-table-iterator ((macroname hashtable) &body body)
  (unless (symbolp macroname)
    (error (TEXT "~S: macro name should be a symbol, not ~S")
           'with-hash-table-iterator macroname))
  (let ((var (gensym)))
    `(LET ((,var (SYS::HASH-TABLE-ITERATOR ,hashtable)))
       (MACROLET ((,macroname () '(SYS::HASH-TABLE-ITERATE ,var) ))
         ,@body))))

;; ----------------------------------------------------------------------------

;; ANSI-CL

(defmacro lambda (&whole whole-form lambdalist &body body)
  (declare (ignore lambdalist body))
  `(FUNCTION ,whole-form))

;; ----------------------------------------------------------------------------

;; Make GET-MACRO-CHARACTER work on dispatch macro characters.
(let ((vector
        (let ((vector '#()))
          (declare (compile))
          ; This code must be in accordance with io.d:read_macro().
          (defun dispatch-reader (stream ch)
            (let ((arg 0)
                  subch)
              (let ((flag nil))
                (loop
                  (let ((nextch (read-char stream nil nil)))
                    (unless nextch
                      (error-of-type 'end-of-file
                        :stream stream
                        (TEXT "~S: input stream ~S ends within read macro beginning to ~S")
                        'read stream ch
                    ) )
                    (unless (characterp nextch)
                      (error-of-type 'stream-error
                        :stream stream
                        (TEXT "~S from ~S: character read should be a character: ~S")
                        'read stream ch
                    ) )
                    (unless (char<= #\0 nextch #\9)
                      (setq subch nextch)
                      (return)
                    )
                    (setq arg (+ (* 10 arg) (digit-char-p nextch)))
                    (setq flag t)
                ) )
                (unless flag (setq arg nil))
              )
              (let* ((subc (char-upcase subch))
                     (macrodef
                       (if (< (char-code subc) #x100)
                         (svref vector (char-code subc))
                         (gethash subc (svref vector #x100))
                    )) )
                (unless macrodef
                  (error-of-type 'stream-error
                    :stream stream
                    (TEXT "~S from ~S: After ~S is ~S an undefined dispatch macro character")
                    'read stream ch subch
                ) )
                (funcall macrodef stream subch arg)
          ) ) )
          vector
     )) )
  (let ((vector-index
          (do ((i 0 (1+ i)))
               (nil)
            (when (eq (%record-ref #'dispatch-reader i) vector) (return i)))))
    (%defio #'dispatch-reader vector-index)
  )
)

;; -------------------------------------------------------------------------
;; READ-SEQUENCE and WRITE-SEQUENCE are badly specified because they assume
;; that the stream has a unique element type, either subtype of CHARACTER or
;; subtype of INTEGER. But some streams (esp. generic-streams) have a type
;; of (OR CHARACTER INTEGER).

;; This is a little hack to get the non-ambigouous cases right.
(defun stream-input-element-type (stream)
  (loop
    (typecase stream
      (SYNONYM-STREAM
       (setq stream (symbol-value (synonym-stream-symbol stream))))
      (ECHO-STREAM
       (setq stream (echo-stream-input-stream stream)))
      (TWO-WAY-STREAM
       (setq stream (two-way-stream-input-stream stream)))
      (T (return))))
  (stream-element-type stream))

(defun stream-output-element-type (stream)
  (loop
    (typecase stream
      (SYNONYM-STREAM
       (setq stream (symbol-value (synonym-stream-symbol stream))))
      (ECHO-STREAM
       (setq stream (echo-stream-output-stream stream)))
      (TWO-WAY-STREAM
       (setq stream (two-way-stream-output-stream stream)))
      (T (return))))
  (stream-element-type stream))

(defun read-sequence (sequence stream &rest rest &key (start 0) (end nil))
  (declare (ignore start end))
  (let ((seltype (stream-input-element-type stream))
        (veltype (if (vectorp sequence) (array-element-type sequence) t)))
    (cond ((or (eq seltype 'NIL) (eq seltype 'CHARACTER)
               (eq veltype 'CHARACTER))
           (apply #'read-char-sequence sequence stream rest))
          ((or (subtypep seltype 'INTEGER) (subtypep veltype 'INTEGER))
           (apply #'read-byte-sequence sequence stream rest))
          (t
           (error (TEXT "~S: element types of ~S and ~S are ambiguous. Please use ~S or ~S.")
                  'read-sequence sequence stream
                  'read-char-sequence 'read-byte-sequence)))))

(defun write-sequence (sequence stream &rest rest &key (start 0) (end nil))
  (declare (ignore start end))
  (let ((seltype (stream-output-element-type stream))
        (veltype (if (vectorp sequence) (array-element-type sequence) t)))
    (cond ((or (eq seltype 'NIL) (eq seltype 'CHARACTER)
               (eq veltype 'CHARACTER))
           (apply #'write-char-sequence sequence stream rest))
          ((or (subtypep seltype 'INTEGER) (subtypep veltype 'INTEGER))
           ;; `write-byte-sequence' accepts :NO-HANG and returns an extra value
           ;; since making `write-char-sequence' do the same would be hairy,
           ;; we ignore this second return value for the sake of ANSI
           (values (apply #'write-byte-sequence sequence stream rest)))
          (t
           (error (TEXT "~S: element types of ~S and ~S are ambiguous. Please use ~S or ~S.")
                  'write-sequence sequence stream
                  'write-char-sequence 'write-byte-sequence)))))

;; ----------------------------------------------------------------------------

;; ANSI-CL specifies TYPE-ERRORs in many places.
;; Here are the corresponding types.

;; (DESIGNATOR thing) is an abbreviation for many terms seen in the CLHS
;; glossary.

;; bounding index sequence    (START-INDEX sequence), (END-INDEX sequence)
;; character                  CHARACTER, BASE-CHAR
;; class                      CLASS
;; condition                  ---
;; extended function          EXTENDED-FUNCTION
;; external file format       ---
;; file position              FILE-POSITION
;; function                   FUNCTION
;; interval                   ---
;; list                       LIST
;; logical-host               LOGICAL-HOST
;; package                    PACKAGE
;; pathname                   PATHNAME
;; readtable                  READTABLE
;; restart                    RESTART
;; spreadable argument list   ---
;; stream                     STREAM
;; stream variable            ---
;; string                     STRING, (STRING length)

(deftype designator (thing)
  (cond ((symbolp thing)
         (case thing
;          (STRING
;            `(OR CHARACTER STRING SYMBOL)
;          )
           (CHARACTER
             `(OR CHARACTER
                  ,@(if (not *ansi*) `((INTEGER 0 ,(1- char-code-limit))))
                  (DESIGNATOR (STRING 1))
           )  )
           (BASE-CHAR
             `(OR BASE-CHAR
                  ,@(if (not *ansi*) `((INTEGER 0 ,(1- base-char-code-limit))))
                  #+BASE-CHAR=CHARACTER
                  (DESIGNATOR (STRING 1))
                  #-BASE-CHAR=CHARACTER
                  (AND (DESIGNATOR (STRING 1)) (SATISFIES BASE-CHAR-DESIGNATOR-P))
           )  )
;          (CLASS `(OR CLOS:CLASS (AND SYMBOL (SATISFIES CLASS-DESIGNATOR-P))))
;          (EXTENDED-FUNCTION
;            `(OR (AND (OR SYMBOL CONS) (SATISFIES FUNCTION-NAME-P)) FUNCTION)
;          )
;          (FILE-POSITION
;            `(OR (MEMBER :START :END) (INTEGER 0 *))
;          )
;          (FUNCTION
;            `(OR SYMBOL FUNCTION)
;          )
;          (LIST
;            `T
;          )
;          (LOGICAL-HOST
;            #-LOGICAL-PATHNAMES `NIL
;            #+LOGICAL-PATHNAMES `(OR STRING LOGICAL-PATHNAME)
;          )
;          (PACKAGE
;            `(OR (DESIGNATOR STRING) PACKAGE)
;          )
;          (PATHNAME
;            `(OR STRING FILE-STREAM PATHNAME)
;          )
;          (READTABLE
;            `(OR NULL READTABLE)
;          )
;          (RESTART
;            `(OR (AND SYMBOL (NOT NULL)) RESTART)
;          )
;          (STREAM
;            `(OR BOOLEAN STREAM)
;          )
           (t thing)
        ))
        ((consp thing)
         (case (first thing)
;          (START-INDEX
;            (let ((seq (second thing)))
;              (assert (typep seq 'SEQUENCE))
;              `(INTEGER 0 ,(length seq))
;          ) )
;          (END-INDEX
;            (let ((seq (second thing)))
;              (assert (typep seq 'SEQUENCE))
;              `(OR (INTEGER 0 ,(length (second thing))) NULL)
;          ) )
           (STRING
             (let ((n (second thing)))
               (assert (typep n '(INTEGER 0 *)))
               (let ((fun (intern (format nil "SYMBOL-OF-LENGTH-~D" n)
                                  (find-package "SYSTEM"))))
                 (unless (fboundp fun)
                   (setf (symbol-function fun)
                         #'(lambda (s)
                             (and (symbolp s) (eql (length (symbol-name s)) n))
                           )
                 ) )
                 `(OR ,@(if (eql n 1) '(CHARACTER) '())
                      (STRING ,n)
                      (AND SYMBOL (SATISFIES ,fun))
                  )
           ) ) )
           (t thing)
        ))
        (t (typespec-error 'designator thing))
) )

#-BASE-CHAR=CHARACTER
(defun base-char-designator-p (obj) ; ABI
  (base-char-p (char (coerce obj 'string) 0))
)

;(defun class-designator-p (sym &aux f)
;  (and (setq f (get sym 'CLOS::CLOSCLASS))
;       (clos::defined-class-p f)
;       (eq (clos:class-name f) sym)
;) )

(defun recognizable-sequence-type-p (typespec)
  (or (subtypep typespec 'LIST) (subtypep typespec 'VECTOR))
)

;; ----------------------------------------------------------------------------

(defmacro define-hash-table-test (name test hash)
  (loop (when (symbolp name) (return))
    ;; check-value will be defined in condition.lisp
    (setq name (check-value
                nil (make-condition 'simple-type-error
                      :format-control (TEXT "~S: ~S should be a symbol")
                      :format-arguments (list 'define-hash-table-test name)
                      :datum name :expected-type 'symbol))))
  `(progn (setf (get ',name 'hash-table-test) (cons #',test #',hash)) ',name))
