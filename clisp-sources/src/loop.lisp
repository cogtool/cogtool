;; ANSI CL Loop
;; (LOOP {loop-clause}*), CLtL2 p. 163,709-747
;; <http://www.lisp.org/HyperSpec/Body/sec_6-1.html>
;; <http://www.lisp.org/HyperSpec/Body/mac_loop.html>
;; Bruno Haible 1991-2002
;; Sam Steingold 1999-2004

(in-package "COMMON-LISP")
(export '(loop loop-finish))
(pushnew ':loop *features*)

(in-package "SYSTEM")

;; Parser auxiliary functions

;; (loop-keywordp obj) determines whether OBJ is a loop keyword,
;; and then returns the appropriate unique symbol, otherwise NIL.
(defun loop-keywordp (obj)
  (and (symbolp obj)
       (gethash (symbol-name obj)
         (load-time-value
           (make-hash-table
             :key-type 'string :value-type 'symbol
             :test 'fasthash-equal :warn-if-needs-rehash-after-gc t
             :initial-contents
               (mapcar #'(lambda (s) (cons (symbol-name s) s))
                 '(named
                   for as and from downfrom upfrom to downto upto below
                   above by in on = then across being each the hash-key
                   hash-keys hash-value hash-values of using symbol
                   present-symbol internal-symbol external-symbol symbols
                   present-symbols internal-symbols external-symbols
                   repeat
                   while until always never thereis
                   collect collecting append appending nconc nconcing
                   count counting sum summing maximize maximizing
                   minimize minimizing into
                   with
                   if when unless else end it
                   do doing return
                   of-type
                   initially finally)))))))

(defvar *whole*)                ; the entire form (LOOP ...)

;; (loop-syntax-error loop-keyword) reports a syntax error.
(defun loop-syntax-error (loop-keyword)
  (error-of-type 'source-program-error
    :form *whole*
    :detail loop-keyword        ; FIXME: should be something more useful
    (TEXT "~S: syntax error after ~A in ~S")
    'loop (symbol-name loop-keyword) *whole*))

;; destructuring:

;; (destructure-vars pattern) returns the list of variables occuring
;; in the pattern.
(defun destructure-vars (pattern)
  (let ((vars '()))
    (labels ((accumulate (pattern)
               (cond ((null pattern))
                     ((atom pattern) (push pattern vars))
                     (t
                       (accumulate (car pattern))
                       (accumulate (cdr pattern))))))
      (accumulate pattern))
    (nreverse vars)))

;; (empty-tree-p pattern) determine whether the pattern has no variables
;; at all.
(defun empty-tree-p (pattern)
  (cond ((null pattern) t)
        ((atom pattern) nil)
        (t (and (empty-tree-p (car pattern))
                (empty-tree-p (cdr pattern))))))

;; (destructure-type pattern type) returns the list of declaration
;; specifiers, that declare that each variable in 'pattern' is of the
;; corresponding type in 'type'.
(defun destructure-type (pattern type)
  (let ((declspecs '()))
    (labels ((accumulate (pattern type)
               (cond ((null pattern))
                     ((atom pattern)
                       (push `(TYPE ,type ,pattern) declspecs))
                     ((consp type)
                       (accumulate (car pattern) (car type))
                       (accumulate (cdr pattern) (cdr type)))
                     (t
                       (let ((vars (destructure-vars pattern)))
                         (when vars
                           (push `(TYPE ,type ,@vars) declspecs)))))))
      (accumulate pattern type))
    (nreverse declspecs)))

;; (simple-type-p type) determines whether 'type' contains, after
;; destructuring, only NIL, T, FIXNUM, FLOAT, and therefore can be
;; used without OF-TYPE.
(defun simple-type-p (type)
  (if (atom type)
    (case type
      ((NIL T FIXNUM FLOAT) t)
      (t nil))
    (and (simple-type-p (car type))
         (simple-type-p (cdr type)))))

(defvar *helpvars*) ;; vector of auxiliary variables for destructuring

;; (helpvar n) returns the (n+1)-st auxiliary variable (n>=0).
;; At least n auxiliary variable must already have been used.
(defun helpvar (n)
  ;; '*helpvars*' is extended if necessary.
  (when (= n (fill-pointer *helpvars*))
    (vector-push-extend (gensym) *helpvars*))
  (aref *helpvars* n))

;; (destructure pattern form) returns a list of lists (variable_i form_i).
;; variable_i is a variable from 'pattern', form_i is a form, whose
;; result must be bound or assigned to variable_i. The order of the
;; bindings/assignments doesn't matter, i.e. both LET and LET*, or
;; both PSETQ and SETQ are possible.
(defun destructure (pattern form)
  (labels ((destructure-tree (pattern form helpvar-count)
             ; helpvar-count = Anzahl der belegten Hilfsvariablen
             (cond ((empty-tree-p pattern) nil)
                   ((atom pattern) (list (list pattern form)))
                   ((empty-tree-p (car pattern))
                    (destructure-tree (cdr pattern) `(CDR ,form) helpvar-count))
                   ((empty-tree-p (cdr pattern))
                    (destructure-tree (car pattern) `(CAR ,form) helpvar-count))
                   (t ; muss form zwischendurch einer Hilfsvariablen zuweisen
                     (let ((helpvar (helpvar helpvar-count)))
                       (nconc (destructure-tree (car pattern) `(CAR (SETQ ,helpvar ,form)) (1+ helpvar-count))
                              (destructure-tree (cdr pattern) `(CDR ,helpvar) helpvar-count)))))))
    (or (destructure-tree pattern form 0)
        ; no variables -> must nevertheless evaluate form!
        (list (list (helpvar 0) form)))))

;; (default-bindings vars declspecs).
;; vars = (var ...) is a list of variables without init forms.
;; Returns the binding list ((var var-init) ...), where var-init is
;; compatible with the declspecs.
(defun default-bindings (vars declspecs)
  ; Use NIL or 0 or 0.0 if it fits the declarations.
  ; Otherwise use NIL and extend the type declarations.
  (let ((bindings (mapcar #'(lambda (var) (list var 'NIL)) vars)))
    (dolist (declspec declspecs)
      (when (eq (first declspec) 'TYPE)
        ; declspec is of form (TYPE type . vars)
        (let* ((type (second declspec))
               (dtype (type-for-discrimination type))
               h)
          (cond ((typep 'NIL dtype)) ; OK
                ((or (typep (setq h '0) dtype) (typep (setq h '0.0) dtype))
                 (dolist (var (cddr declspec))
                   (setf (second (find var bindings :key #'first)) h)))
                (t (setf (second declspec) `(OR NULL ,type)))))))
    bindings))

;; A loop-initialization describes at macro expansion time the task
;; to initialise one or more variables. The initialization may end up
;; generating code in the prologue or in the inner loop.
(defstruct (loop-initialization
             (:copier nil)
             (:conc-name "LI-")
             (:predicate nil)
             (:constructor make-loop-init))
  ;; How to generate the Lisp code.
  specform           ; special form: LET or MULTIPLE-VALUE-BIND or PROGN
  bindings           ; for LET: list of bindings,
                     ; for MULTIPLE-VALUE-BIND: varlist and form
  declspecs          ; list of declspecs
  (endtest-forms nil) ; more forms to be inserted after the declarations,
                      ; within the tagbody.
  ;; Properties of this initialization.
  everytime ; If the assignment has to be evaluated in the prologue only: NIL.
            ; If the assignment has to be evaluated once for each iteration:
            ; a cons, pointing at the right place in the stepafter-code.
  (requires-stepbefore nil) ; True if the variables can get their values only
                            ; in the stepbefore-code or initially-code,
                            ; false if the first assignment can be merged
                            ; with the initial binding.
  (depends-preceding nil) ; True if everytime=NIL and the values may depend
                          ; on preceding variables, so that these preceding
                          ; variables must get their values no later than in
                          ; the initially-code.
  (later-depend nil)) ; True if some later variables depend on these values,
                      ; so that these values
                      ; must be computed no later than in the initially-code.

(proclaim '(inline li-vars))
(defun li-vars (li)
  (case (li-specform li)
    ((MULTIPLE-VALUE-BIND) (first (li-bindings li)))
    ((LET) (mapcar #'first (li-bindings li)))))

; (wrap-initializations initializations form) wickelt eine (umgedrehte!)
; Liste von Initialisierungen um form herum und liefert die neue Form.
(defun wrap-initializations (initializations form)
  (dolist (initialization initializations)
    (let ((name (li-specform initialization))
          (bindings (li-bindings initialization))
          (declarations (li-declspecs initialization)))
      (setq form
            `(,name
              ,@(case name (MULTIPLE-VALUE-BIND bindings) (LET `(,bindings)))
              ,@(if declarations `((DECLARE ,@declarations)))
              ,@(li-endtest-forms initialization)
              ,form))))
  form)

;; Variable containing the last test result, called "it".
(defvar *last-it*)
;; Flag whether this variable is used.
(defvar *used-it*)

;; The bulk of the expander.
(defun expand-loop (*whole* body)
  (let ((body-rest body) ; alle Parse-Funktionen verkürzen body-rest
        (block-name 'NIL) ; Name des umgebenden BLOCKs
        (already-within-main nil) ; im zweiten Teil von {variables}* {main}* ?
        (*helpvars* (make-array 1 :fill-pointer 0 :adjustable t))
        (*last-it* nil)
        (var-list nil)          ; all variables seen so far
        (acculist-var nil) ; Akkumulationsvariable für collect, append etc.
        (accuvar-tailvar-alist nil) ; alist of (accu-var . tail-var)
        (accunum-var nil) ; Akkumulationsvariable für count, sum etc.
        (accu-vars-nil nil) ; Akkumulationsvariablen mit Initialwert NIL
        (accu-vars-0 nil) ; Akkumulationsvariablen mit Initialwert 0
        (accu-table (make-hash-table :warn-if-needs-rehash-after-gc t
                                     :test 'stablehash-eq)) ; var --> clauses
        (accu-declarations nil) ; Typdeklarationen (umgedrehte Liste von declspecs)
        (initializations nil) ; Bindungen: (init ...) (umgedrehte Liste)
        (seen-for-as-= nil) ; schon eine FOR-AS-= Klausel gesehen?
        (seen-endtest nil) ; schon eine FOR-AS Klausel mit Abbruchbedingung gesehen?
        (initially-code nil) ; initially-Code (umgedrehte Liste)
        (startup-code nil) ; forms from for ... = ... then and end conditions
        (stepbefore-code nil) ; Code zum Abbruch vor dem Schleifendurchlauf (umgedrehte Liste)
        (main-code nil) ; Code im Hauptteil der Schleife (umgedrehte Liste)
        (stepafter-code nil) ; Code zur Vorbereitung des nächsten Schleifendurchlaufs (umgedrehte Liste)
        (accu-vars-nreverse nil) ; Akkumulationsvariablen, die am Schluss umzudrehen sind
        (finally-code nil) ; finally-Code (umgedrehte Liste)
        (backward-consing-p     ; is backward-consing possible?
         (do ((rest *whole* (cdr rest)))
             ((endp rest) t)
           (case (loop-keywordp (car rest))
             ((NCONC NCONCING APPEND APPENDING)
              (unless (eq (loop-keywordp (caddr rest)) 'INTO)
                (return nil))))))
        (results nil)) ; alist (value-form . (clause list))
    (labels
      ((next-kw () ; Schaut, ob als nächstes ein Keyword kommt.
                   ; Wenn ja, wird es geliefert. Wenn nein, Ergebnis NIL.
         (and (consp body-rest) (loop-keywordp (first body-rest))))
       (cons-forward (form accuvar accufuncsym)
         (let ((tailvar
                (cdr (or (assoc accuvar accuvar-tailvar-alist)
                         (car (setq accuvar-tailvar-alist
                                    (acons accuvar
                                           (gensym (symbol-name accuvar))
                                           accuvar-tailvar-alist))))))
               (incrementvar (gensym)))
           (push accuvar accu-vars-nil)
           (push tailvar accu-vars-nil)
           `(LET ((,incrementvar
                   ,(ecase accufuncsym
                      (CONS `(LIST ,form))
                      (REVAPPEND `(COPY-LIST ,form))
                      (NRECONC `,form))))
              (IF ,accuvar
                ,(case accufuncsym
                   (CONS `(SETF ,tailvar (SETF (CDR ,tailvar) ,incrementvar)))
                   (t `(SETF ,tailvar (LAST (RPLACD ,tailvar ,incrementvar)))))
                ,(case accufuncsym
                   (CONS `(SETF ,tailvar (SETF ,accuvar ,incrementvar)))
                   (t `(SETF ,tailvar (LAST (SETF ,accuvar
                                                  ,incrementvar)))))))))
       (compatible-p (kw1 kw2)
         ;; <http://www.lisp.org/HyperSpec/Body/sec_6-1-3.html>
         (let ((ht #,(make-hash-table
                      :warn-if-needs-rehash-after-gc t
                      :test 'stablehash-eq
                      :initial-contents
                      '((collect . list) (collecting . list)
                        (append . list) (appending . list)
                        (nconc . list) (nconcing . list)
                        (sum . sum-count) (summing . sum-count)
                        (count . sum-count) (counting . sum-count)
                        (maximize . max-min) (maximizing . max-min)
                        (minimize . max-min) (minimizing . max-min)))))
           (eq (gethash kw1 ht) (gethash kw2 ht))))
       (new-accu-var (var clause)
         (let ((others (gethash var accu-table)) bad)
           (when (setq bad (find (first clause) others
                                 :key #'first :test-not #'compatible-p))
             (error-of-type 'source-program-error
               :form *whole* :detail clause
               (TEXT "~S: variable ~S is used in incompatible clauses~{ ~A ~S~} and~{ ~A ~S~}")
               *whole* var clause bad))
           (setf (gethash var accu-table) (cons clause others))))
       (new-result (form clause)
         (let ((pair (assoc form results :test #'equal)))
           (if pair
               (push clause (cdr pair))
               (push (list form clause) results))
           results))
       (acculist-var (keyword form)
         (or acculist-var
             (progn (setq acculist-var (gensym "ACCULIST-VAR-"))
                    (push acculist-var accu-vars-nil)
                    (let ((clause (list keyword form)))
                      (new-accu-var acculist-var clause)
                      (unless backward-consing-p
                        (new-result acculist-var clause)))
                    acculist-var)))
       (cons-backward (keyword form) ; accuvar is NIL, accufuncsym is CONS
         (let ((accuvar (acculist-var keyword form)))
           (new-result `(SYS::LIST-NREVERSE ,accuvar) `(,keyword ,form))
           `(SETQ ,accuvar (CONS ,form ,accuvar))))
       (parse-kw-p (kw) ; Schaut, ob als nächstes das Keyword kw kommt.
                        ; Wenn ja, wird es übergangen. Wenn nein, Ergebnis NIL.
         (and (consp body-rest) (eq (loop-keywordp (first body-rest)) kw)
              (progn (pop body-rest) t)))
       (parse-form (kw) ; Nach kw: parst expr
         (unless (consp body-rest) (loop-syntax-error kw))
         (pop body-rest))
       (parse-form-or-it (kw) ; Nach kw: parst expr, das auch 'it' sein kann
         (unless (consp body-rest) (loop-syntax-error kw))
         (let ((form (pop body-rest)))
           (if (eq (loop-keywordp form) 'it)
             (if *last-it*
               (progn (setq *used-it* t) *last-it*)
               form)
             form)))
       (parse-var-typespec () ;; parse var [typespec]
         ;; return the variable pattern and the list of declspecs
         (unless (consp body-rest)
           (error-of-type 'source-program-error
             :form *whole* :detail body-rest
             (TEXT "~S: missing variable.") 'loop))
         (let ((pattern (pop body-rest))
               (typedecl nil))
           (block nil
             (unless (consp body-rest) (return))
             (case (loop-keywordp (first body-rest))
               ((NIL)           ; no loop keyword ->interpret as typespec
                (setq typedecl (pop body-rest))
                (unless (simple-type-p typedecl)
                  (warn (TEXT "~S: After ~S, ~S is interpreted as a type specification")
                        'loop pattern typedecl)))
               ((OF-TYPE)       ; OF-TYPE -> expect a typespec
                (pop body-rest)
                (setq typedecl (parse-form 'of-type)))
               (T (return)))    ; other
             (setq typedecl (destructure-type pattern typedecl)))
           (values pattern typedecl)))
       (parse-progn () ;; parses {expr}* and return the list of forms
         (let ((list nil))
           (loop
            (unless (and (consp body-rest)
                         (not (loop-keywordp (first body-rest))))
              (return))
            (push (pop body-rest) list))
           (nreverse list)))
       (parse-nonempty-progn (kw) ;; after kw: [CLTS] {expr}+ or [CLTL2] {expr}*
         (let ((exprs (parse-progn)))
           (unless exprs
             (if *loop-ansi*
               (loop-syntax-error kw)
               (warn (TEXT "~S: missing forms after ~A: permitted by CLtL2, forbidden by ANSI CL.") 'loop (symbol-name kw))))
           exprs))
       (parse-unconditional () ;; parse an unconditional
         ;; unconditional ::= {do | doing} {expr}*
         ;; unconditional ::= return expr
         ;; Returns a lisp form or NIL when no unconditional was parsed.
         (let ((kw (next-kw)))
           (case kw
             ((DO DOING)
              (pop body-rest)
              `(PROGN ,@(parse-nonempty-progn kw)))
             ((RETURN)
              (pop body-rest)
              `(RETURN-FROM ,block-name ,(parse-form-or-it kw)))
             (t 'NIL))))
       (parse-clause () ;; parses a clause
         ;; clause ::= accumulation | conditional | unconditional
         ;; accumulation ::= {collect | collecting | append | appending |
         ;;                   nconc | nconcing} expr [into var]
         ;; accumulation ::= {count | counting | sum | summing |
         ;;                   maximize | maximizing | minimize |
         ;;                   minimizing} expr [into var] [typespec]
         ;; conditional ::= {if | when | unless} expr clause {and clause}*
         ;;                 [else clause {and clause}*] [end]
         ;; Returns a lisp form or NIL when no clause was parsed.
         (or (parse-unconditional)
             (let ((kw (next-kw)))
               (case kw
                 ((COLLECT COLLECTING APPEND APPENDING NCONC NCONCING)
                  (pop body-rest)
                  ;; It seems permitted to write
                  ;;   (loop ...  collect i into c collect (copy-list c))
                  ;; Therefore we must use forward-consing collection
                  ;; (keeping the tail in a separate variable) if the
                  ;; accumulation variable is named, and can use the more
                  ;; efficient backward-consing (with nreverse at the end)
                  ;; only for unnamed accumulation.
                  ;; Also, APPEND/NCONC require forward-consing because
                  ;; REVAPPEND/NRECONC drop the last atom in dotted lists
                  (let ((form (parse-form-or-it kw))
                        (accuvar nil)
                        (accufuncsym
                          (case kw
                            ((COLLECT COLLECTING) 'CONS)
                            ((APPEND APPENDING) 'REVAPPEND)
                            ((NCONC NCONCING) 'NRECONC))))
                    (when (parse-kw-p 'into)
                      (unless (and (consp body-rest)
                                   (symbolp (setq accuvar (pop body-rest))))
                        (loop-syntax-error 'into)))
                    (cond (accuvar ; named acc var -> forward-consing.
                           (cons-forward form accuvar accufuncsym))
                          ((or (eq accufuncsym 'REVAPPEND)
                               (eq accufuncsym 'NRECONC)
                               (null backward-consing-p))
                           ;; REVAPPEND/NRECONC now or before
                           (when backward-consing-p
                             (error "~s: internal error: backward consing should be illegal!" *whole*))
                           (cons-forward form (acculist-var kw form)
                                         accufuncsym))
                          (t ; Unnamed acc var & CONS -> cons-backward
                           (cons-backward kw form)))))
                 ((COUNT COUNTING SUM SUMMING MAXIMIZE MAXIMIZING
                   MINIMIZE MINIMIZING)
                  (pop body-rest)
                  (let* ((form (parse-form-or-it kw)) (type 'fixnum)
                         (accuvar nil) (clause (list kw form)))
                    (when (parse-kw-p 'into)
                      (unless (and (consp body-rest)
                                   (symbolp (setq accuvar (pop body-rest))))
                        (loop-syntax-error 'into)))
                    (unless accuvar
                      (setq accuvar
                            (or accunum-var
                                (setq accunum-var (gensym "ACCUNUM-VAR-"))))
                      (new-result accuvar clause))
                    (new-accu-var accuvar clause)
                    (when (consp body-rest)
                      (let ((kw2 (loop-keywordp (first body-rest))))
                        (when (or (not kw2) (eq kw2 'of-type))
                          (setq type
                                (if (not kw2) (pop body-rest)
                                  (progn (pop body-rest)
                                         (parse-form 'of-type))))
                          (case kw
                            ((MAXIMIZE MAXIMIZING MINIMIZE MINIMIZING)
                             (setq type `(OR NULL ,type)))) ; wegen Startwert NIL
                          (push `(TYPE ,type ,accuvar) accu-declarations))))
                    (case kw
                      ((MAXIMIZE MAXIMIZING MINIMIZE MINIMIZING)
                       (push accuvar accu-vars-nil))
                      ((COUNT COUNTING SUM SUMMING)
                       (push (list accuvar (coerce 0 type)) accu-vars-0)))
                    (case kw
                      ((COUNT COUNTING) `(WHEN ,form (INCF ,accuvar)))
                      ((SUM SUMMING) `(SETQ ,accuvar (+ ,accuvar ,form)))
                      ((MAXIMIZE MAXIMIZING) `(SETQ ,accuvar (MAX-IF ,form ,accuvar)))
                      ((MINIMIZE MINIMIZING) `(SETQ ,accuvar (MIN-IF ,form ,accuvar))))))
                 ((IF WHEN UNLESS)
                  (pop body-rest)
                  (let* ((condition (parse-form kw))
                         (it-var (gensym))
                         used-it
                         (true-form
                           (let ((*last-it* it-var) (*used-it* nil))
                             (prog1
                               (parse-clauses kw)
                               (setq used-it *used-it*))))
                         (false-form 'NIL))
                    (when (parse-kw-p 'else)
                      (setq false-form
                        (let ((*last-it* it-var) (*used-it* nil))
                          (prog1
                            (parse-clauses 'else)
                            (setq used-it (or used-it *used-it*))))))
                    (parse-kw-p 'end)
                    (when used-it
                      (psetq it-var `((,it-var ,condition))
                             condition it-var))
                    (let ((form
                            `(IF ,(if (eq kw 'UNLESS)
                                    `(NOT ,condition) ; UNLESS
                                    `,condition) ; IF, WHEN
                               ,true-form
                               ,false-form)))
                      (if used-it `(LET ,it-var ,form) `,form))))
                 (t 'NIL)))))
       (parse-clauses (kw) ; Nach kw: parst  clause {and clause}*
                           ; oder kurz       {clause}+{and}
         ; Liefert eine Lisp-Form.
         (let ((clauses nil))
           (loop
             (let ((clause (parse-clause)))
               (unless clause (loop-syntax-error kw))
               (push clause clauses))
             (unless (parse-kw-p 'and) (return))
             (setq kw 'and)
             (setq *last-it* nil)) ; 'it' ist nur in der ersten Klausel gültig
           `(PROGN ,@(nreverse clauses))))
       ; Binden und Initialisieren von Variablen:
       ; Nach ANSI-CL 6.1.1.4 gelten zwei Grundregeln:
       ; - Beim Initialisieren von FOR-AS Variablen (außer FOR-AS-=) sind
       ;   mindestens alle vorherigen FOR-AS Variablen sichtbar.
       ; - Beim Initialisieren von FOR-AS-= Variablen sind alle FOR-AS Variablen
       ;   sichtbar.
       ; Zusätzlich ist die folgende Grundregel wünschenswert:
       ; - Beim Initialisieren von FOR-AS-= Variablen sind mindestens alle
       ;   vorherigen FOR-AS Variablen initialisiert und deren Abbruch-
       ;   bedingungen abgeprüft.
       ; Man könnte erst alle Variablen binden und dann im initially-code
       ; die Initialisierungen durchführen. Wir führen demgegenüber zwei
       ; Optimierungen durch:
       ; - Falls vor der FOR-AS Variablen keine FOR-AS-= Klausel kommt,
       ;   braucht die Variable zum Zeitpunkt ihrer Initialisierung nicht
       ;   sichtbar zu sein, und wir verlagern ihre Initialisierung nach
       ;   vorne, zur Bindung. Das geht aber nur, wenn vor der FOR-AS Variablen
       ;   keine FOR-AS Klausel mit Abbruchbedingung kommt.
       ; - Falls eine Variable gar nicht sichtbar zu sein braucht, weil keine
       ;   FOR-AS-= Klausel vorkommt und hinter ihr auch keine andere FOR-AS
       ;   Klausel stört, können die Bindung und die Initialiserung der
       ;   Variablen ins Schleifeninnere verschoben werden.
       (note-initialization (initialization)
         (when (or (li-bindings initialization)
                   (li-declspecs initialization)
                   (li-endtest-forms initialization))
           (when seen-for-as-=
             (setf (li-requires-stepbefore initialization) t))
           (when (li-endtest-forms initialization)
             (setq seen-endtest t))
           (dolist (var (li-vars initialization))
             (when (memq var var-list)
               (error-of-type 'source-program-error
                 :form *whole* :detail var
                 (TEXT "~S: duplicate iteration variable ~S") *whole* var))
             (push var var-list))
           (push initialization initializations)))
       (make-endtest (endtest-form)
         (make-loop-init
           :specform 'PROGN
           :bindings nil
           :declspecs nil
           :endtest-forms (list endtest-form)
           :everytime (setq stepafter-code (cons 'NIL stepafter-code))
           :requires-stepbefore seen-endtest)))
      ;; Los geht's!
      ; parst: [named name]
      (when (parse-kw-p 'named)
        (unless (and (consp body-rest) (symbolp (first body-rest)))
          (loop-syntax-error 'named))
        (setq block-name (pop body-rest)))
      (loop
        ; main ::= clause | termination | initially | finally |
        ;          with | for-as | repeat
        ; termination ::= {while | until | always | never | thereis} expr
        ; initially ::= initially {expr}*
        ; finally ::= finally { unconditional | {expr}* }
        ; with ::= with {var-typespec [= expr]}+{and}
        ; for-as ::= {for | as} {var-typespec ...}+{and}
        ; repeat ::= repeat expr
        (unless (consp body-rest) (return))
        (let ((clause (parse-clause)))
          (if clause
            (progn (setq already-within-main t) (push clause main-code))
            (let ((kw (loop-keywordp (first body-rest))))
              (case kw
                ((WHILE UNTIL ALWAYS NEVER THEREIS)
                 (pop body-rest)
                 (setq already-within-main t)
                 (let ((form (parse-form kw)))
                   (push (case kw
                           (WHILE `(UNLESS ,form (LOOP-FINISH)))
                           (UNTIL `(WHEN ,form (LOOP-FINISH)))
                           (ALWAYS
                             (new-result 'T (list kw form))
                             `(UNLESS ,form (RETURN-FROM ,block-name 'NIL)))
                           (NEVER
                             (new-result 'T (list kw form))
                             `(WHEN ,form (RETURN-FROM ,block-name 'NIL)))
                           (THEREIS
                             (let ((dummy (gensym)))
                               (new-result 'NIL (list kw form))
                               `(BLOCK ,dummy
                                  (RETURN-FROM ,block-name
                                    (OR ,form (RETURN-FROM ,dummy NIL)))))))
                         main-code)))
                ((INITIALLY)
                 (pop body-rest)
                 (push `(PROGN ,@(parse-nonempty-progn kw)) initially-code))
                ((FINALLY)
                 (pop body-rest)
                 (push (let ((form (parse-unconditional)))
                         (if form
                           (if *loop-ansi*
                             (loop-syntax-error 'FINALLY)
                             (warn (TEXT "~S: loop keyword immediately after ~A: permitted by CLtL2, forbidden by ANSI CL.") 'loop (symbol-name kw)))
                           (setq form `(PROGN ,@(parse-nonempty-progn kw))))
                          form)
                       finally-code))
                ((WITH FOR AS REPEAT)
                 (pop body-rest)
                 (when already-within-main
                   (warn (TEXT "~S: ~A clauses should occur before the loop's main body")
                         'loop (symbol-name kw)))
                 (case kw
                   ((WITH)
                    (let ((bindings nil)
                          (declspecs nil))
                      (loop
                        (let (new-bindings)
                          (multiple-value-bind (pattern new-declspecs) (parse-var-typespec)
                            (if (parse-kw-p '=)
                              ; Initialisierungsform angegeben.
                              (let ((form (parse-form '=)))
                                (setq new-bindings (destructure pattern form)))
                              ; keine Initialisierungsform angegeben.
                              (setq new-bindings (default-bindings (destructure-vars pattern) new-declspecs)))
                            (setq bindings (revappend new-bindings bindings))
                            (setq declspecs (revappend new-declspecs declspecs))))
                        (unless (parse-kw-p 'and) (return))
                        (setq kw 'and))
                      (note-initialization
                        (make-loop-init
                          :specform 'LET
                          :bindings (nreverse bindings)
                          :declspecs (nreverse declspecs)
                          :everytime nil
                          ;; WITH vars should always be bound on top
                          :requires-stepbefore nil ; seen-endtest
                          :depends-preceding t))))
                   ((FOR AS)
                    ; for-as ::= {for | as} for-as-clause {and [{for | as}] for-as-clause}*
                    ; for-as-clause ::= var-typespec
                    ;                   [{from | downfrom | upfrom} expr]
                    ;                   [{to | downto | upto | below | above} expr]
                    ;                   [by expr]
                    ; for-as-clause ::= var-typespec {in | on} expr [by expr]
                    ; for-as-clause ::= var-typespec = expr [then expr]
                    ; for-as-clause ::= var-typespec across expr
                    ; for-as-clause ::= var-typespec being {each | the}
                    ;                   {hash-key[s] | hash-value[s]}
                    ;                   {in | of} expr
                    ;                   [using ( {hash-value | hash-key} var ) ]
                    ; for-as-clause ::= var-typespec being {each | the}
                    ;                   {symbol[s] | present-symbol[s] | internal-symbol[s] | external-symbol[s]}
                    ;                   {in | of} expr
                    (let ((bindings nil)
                          (declspecs nil)
                          (initializations nil)
                          (stepafter nil)
                          (old-seen-endtest seen-endtest)
                          (depends-preceding nil))
                      (flet ((note-initialization (initialization)
                               ;; supersedes the outer definition!
                               ;; Calls to note-initialization must temporarily be suspended.
                               (when (li-endtest-forms initialization)
                                 (setq seen-endtest t))
                               (push initialization initializations)))
                        (loop
                          (multiple-value-bind (pattern new-declspecs) (parse-var-typespec)
                            (let ((preposition (next-kw)))
                              (case preposition
                                ((IN ON)
                                 (pop body-rest)
                                 (let ((start-form (parse-form preposition))
                                       (step-function-form '(FUNCTION CDR))
                                       (step-function-var nil))
                                   (when (parse-kw-p 'by)
                                     (setq step-function-form (parse-form 'by))
                                     (unless (and (function-form-p step-function-form)
                                                  (function-name-p (second step-function-form)))
                                       (setq step-function-var (gensym))))
                                   (let ((var (if (and pattern (symbolp pattern)
                                                       (eq preposition 'ON))
                                                  pattern (gensym))))
                                     (push `(,var ,start-form) bindings)
                                     (when step-function-var
                                       (push `(,step-function-var ,step-function-form)
                                             bindings))
                                     (note-initialization
                                      (make-endtest
                                       `(WHEN (,(if (eq preposition 'IN)
                                                    'ENDP 'ATOM)
                                                ,var)
                                          (LOOP-FINISH))))
                                     (unless (eq var pattern)
                                       (note-initialization
                                        (make-loop-init
                                         :specform 'LET
                                         :bindings (destructure pattern (if (eq preposition 'IN) `(CAR ,var) var))
                                         :declspecs new-declspecs
                                         :everytime t
                                         :requires-stepbefore seen-endtest)))
                                     (push
                                       (list var
                                             (if step-function-var
                                               `(FUNCALL ,step-function-var ,var)
                                               `(,(second step-function-form) ,var)))
                                       stepafter))))
                                (=
                                 (pop body-rest)
                                 (let* ((first-form (parse-form 'preposition))
                                        (then-form first-form))
                                   (when (parse-kw-p 'then)
                                     (setq then-form (parse-form 'then)))
                                   (setq bindings
                                     (revappend (destructure pattern first-form)
                                                bindings))
                                   (setq declspecs (revappend new-declspecs declspecs))
                                   (unless (and (constantp first-form)
                                                (constantp then-form))
                                     (setq seen-for-as-= t)
                                     ;; Even when `first-form' is constant but
                                     ;; `then-form' is not, we must set
                                     ;; `depends-preceding', because the
                                     ;; `stepafter-code' depends on the order of
                                     ;; the steppings, which forbids moving
                                     ;; some code from `initially-code' +
                                     ;; `stepafter-code' to `stepbefore-code.'
                                     (setq depends-preceding t))
                                   (setq stepafter (revappend (destructure pattern then-form) stepafter))))
                                (ACROSS
                                 (pop body-rest)
                                 (let ((vector-form (parse-form preposition))
                                       (vector-var (gensym))
                                       (index-var (gensym)))
                                   (push `(,vector-var ,vector-form) bindings)
                                   (push `(,index-var 0) bindings)
                                   (note-initialization
                                     (make-endtest `(WHEN (>= ,index-var (LENGTH ,vector-var)) (LOOP-FINISH))))
                                   (note-initialization
                                     (make-loop-init
                                       :specform 'LET
                                       :bindings (destructure pattern `(AREF ,vector-var ,index-var))
                                       :declspecs new-declspecs
                                       :everytime t
                                       :requires-stepbefore seen-endtest))
                                   (push (list index-var `(1+ ,index-var)) stepafter)))
                                (BEING
                                 (pop body-rest)
                                 (let ((plural (next-kw)))
                                   (case plural
                                     ((EACH THE))
                                     (t (loop-syntax-error 'being)))
                                   (pop body-rest)
                                   (let ((preposition (next-kw)))
                                     (case preposition
                                       ((HASH-KEY HASH-VALUE
                                         SYMBOL PRESENT-SYMBOL INTERNAL-SYMBOL EXTERNAL-SYMBOL)
                                        (when (eq plural 'THE)
                                          (warn (TEXT "~S: After ~S a plural loop keyword is required, not ~A")
                                                'loop plural (symbol-name preposition))))
                                       ((HASH-KEYS HASH-VALUES
                                         SYMBOLS PRESENT-SYMBOLS INTERNAL-SYMBOLS EXTERNAL-SYMBOLS)
                                        (when (eq plural 'EACH)
                                          (warn (TEXT "~S: After ~S a singular loop keyword is required, not ~A")
                                                'loop plural (symbol-name preposition))))
                                       (t (loop-syntax-error plural)))
                                     (pop body-rest)
                                     (case preposition
                                       ((HASH-KEY HASH-KEYS HASH-VALUE HASH-VALUES)
                                        (let ((other-pattern nil)
                                              (form
                                               (case (next-kw)
                                                 ((IN OF) (pop body-rest)
                                                  (parse-form preposition))
                                                 (t (loop-syntax-error
                                                      preposition)))))
                                          (when (parse-kw-p 'using)
                                            (unless (and (consp body-rest)
                                                         (consp (car body-rest))
                                                         (consp (cdar body-rest))
                                                         (null (cddar body-rest))
                                                         (case (loop-keywordp (caar body-rest))
                                                           ((HASH-KEY HASH-KEYS)
                                                            (case preposition
                                                              ((HASH-VALUE HASH-VALUES) t) (t nil)))
                                                           ((HASH-VALUE HASH-VALUES)
                                                            (case preposition
                                                              ((HASH-KEY HASH-KEYS) t) (t nil)))))
                                              (loop-syntax-error 'using))
                                            (setq other-pattern (second (pop body-rest))))
                                          (let ((state-var (gensym))
                                                (nextp-var (gensym))
                                                (nextkey-var (gensym))
                                                (nextvalue-var (gensym)))
                                            (multiple-value-bind (nextmain-var nextother-var)
                                                (case preposition
                                                  ((HASH-KEY HASH-KEYS) (values nextkey-var nextvalue-var))
                                                  ((HASH-VALUE HASH-VALUES) (values nextvalue-var nextkey-var)))
                                              (push `(,state-var (SYS::HASH-TABLE-ITERATOR ,form)) bindings)
                                              (note-initialization
                                               (make-loop-init
                                                :specform 'MULTIPLE-VALUE-BIND
                                                :bindings `((,nextp-var ,nextkey-var ,nextvalue-var)
                                                            (SYS::HASH-TABLE-ITERATE ,state-var))
                                                :declspecs (unless other-pattern `((IGNORE ,nextother-var)))
                                                :endtest-forms `((UNLESS ,nextp-var (LOOP-FINISH)))
                                                :everytime t
                                                :requires-stepbefore seen-endtest))
                                              (note-initialization
                                               (make-loop-init
                                                :specform 'LET
                                                :bindings (destructure pattern nextmain-var)
                                                :declspecs new-declspecs
                                                :everytime t
                                                :requires-stepbefore seen-endtest))
                                              (when other-pattern
                                                (note-initialization
                                                 (make-loop-init
                                                  :specform 'LET
                                                  :bindings (destructure other-pattern nextother-var)
                                                  :declspecs nil
                                                  :everytime t
                                                  :requires-stepbefore seen-endtest)))))))
                                       ((SYMBOL SYMBOLS PRESENT-SYMBOL PRESENT-SYMBOLS
                                         INTERNAL-SYMBOL INTERNAL-SYMBOLS EXTERNAL-SYMBOL EXTERNAL-SYMBOLS)
                                        (let ((flags (case preposition
                                                       ((SYMBOL SYMBOLS) '(:internal :external :inherited))
                                                       ((PRESENT-SYMBOL PRESENT-SYMBOLS) '(:internal :external))
                                                       ((INTERNAL-SYMBOL INTERNAL-SYMBOLS) '(:internal))
                                                       ((EXTERNAL-SYMBOL EXTERNAL-SYMBOLS) '(:external))))
                                              (state-var (gensym))
                                              (nextp-var (gensym))
                                              (nextsym-var (gensym))
                                              (form
                                               (case (next-kw)
                                                 ((IN OF) (pop body-rest)
                                                  (parse-form preposition))
                                                 (t '*package*))))
                                          (push `(,state-var (SYS::PACKAGE-ITERATOR ,form ',flags))
                                                bindings)
                                          (note-initialization
                                           (make-loop-init
                                            :specform 'MULTIPLE-VALUE-BIND
                                            :bindings `((,nextp-var ,nextsym-var)
                                                        (SYS::PACKAGE-ITERATE ,state-var))
                                            :declspecs nil
                                            :endtest-forms `((UNLESS ,nextp-var (LOOP-FINISH)))
                                            :everytime t
                                            :requires-stepbefore seen-endtest))
                                          (note-initialization
                                           (make-loop-init
                                            :specform 'LET
                                            :bindings (destructure pattern nextsym-var)
                                            :declspecs new-declspecs
                                            :everytime t
                                            :requires-stepbefore seen-endtest))))))))
                                (t
                                 (unless (symbolp pattern) (loop-syntax-error kw))
                                 (unless pattern (setq pattern (gensym)))
                                 ;; ANSI CL 6.1.2.1.1 implies that the
                                 ;; start/end/by clauses can come in any
                                 ;; order, but only one of each kind.
                                 (let ((step-start-p nil)
                                       (step-end-p nil)
                                       (step-by-p nil)
                                       step-start-form
                                       step-end-form
                                       step-by-form
                                       step-end-preposition
                                       dir)
                                   (loop
                                     (cond ((and (not step-start-p)
                                                 (setq dir (case preposition
                                                             (FROM 't)
                                                             (UPFROM 'up)
                                                             (DOWNFROM 'down)
                                                             (t nil))))
                                            (setq step-start-p dir)
                                            (pop body-rest)
                                            (setq step-start-form (parse-form preposition))
                                            (push `(,pattern ,step-start-form) bindings))
                                           ((and (not step-end-p)
                                                 (setq dir (case preposition
                                                             (TO 't)
                                                             ((UPTO BELOW) 'up)
                                                             ((DOWNTO ABOVE) 'down)
                                                             (t nil))))
                                            (setq step-end-p dir)
                                            (setq step-end-preposition preposition)
                                            (pop body-rest)
                                            (setq step-end-form (parse-form preposition))
                                            (unless (constantp step-end-form)
                                              (let ((step-end-var (gensym)))
                                                (push `(,step-end-var ,step-end-form) bindings)
                                                (setq step-end-form step-end-var))))
                                           ((and (not step-by-p)
                                                 (eq preposition 'BY))
                                            (setq step-by-p t)
                                            (pop body-rest)
                                            (setq step-by-form (parse-form 'by))
                                            (unless (constantp step-by-form)
                                              (let ((step-by-var (gensym)))
                                                (push `(,step-by-var ,step-by-form) bindings)
                                                (setq step-by-form step-by-var))))
                                           (t (return)))
                                     (setq preposition (next-kw)))
                                   ;; All parsing done, gather the declarations:
                                   (setq declspecs (revappend new-declspecs declspecs))
                                   ;; Determine the direction of iteration:
                                   (let ((step-direction
                                           (if (or (eq step-start-p 'down) (eq step-end-p 'down))
                                             (if (or (eq step-start-p 'up) (eq step-end-p 'up))
                                               (error-of-type 'source-program-error
                                                 :form *whole* :detail kw
                                                 (TEXT "~S: questionable iteration direction after ~A")
                                                 'loop (symbol-name kw))
                                               'down)
                                             'up)))
                                     ;; Determine start, unless given:
                                     (unless step-start-p
                                       (when (eq step-direction 'down)
                                         ; Abwärtsiteration ohne Startwert ist nicht erlaubt.
                                         ; Die zweite optionale Klausel (d.h. preposition) muss abwärts zeigen.
                                         (error-of-type 'source-program-error
                                           :form *whole* :detail preposition
                                           (TEXT "~S: specifying ~A requires FROM or DOWNFROM")
                                           'loop (symbol-name preposition)))
                                       ; Aufwärtsiteration -> Startwert 0
                                       (setq step-start-form '0)
                                       (push `(,pattern ,step-start-form) bindings))
                                     ; Determine step, unless given:
                                     (unless step-by-p (setq step-by-form '1))
                                     ; Determine end test:
                                     (when step-end-p
                                       (let* ((compfun
                                                (if (eq step-direction 'up)
                                                  (if (eq step-end-preposition 'below) '>= '>) ; up
                                                  (if (eq step-end-preposition 'above) '<= '<))) ; down
                                              (endtest
                                                (if (and (constantp step-end-form) (zerop (eval step-end-form)))
                                                  (case compfun
                                                    (>= `(NOT (MINUSP ,pattern)))
                                                    (> `(PLUSP ,pattern))
                                                    (<= `(NOT (PLUSP ,pattern)))
                                                    (< `(MINUSP ,pattern)))
                                                  `(,compfun ,pattern ,step-end-form))))
                                         (note-initialization
                                           (make-endtest `(WHEN ,endtest (LOOP-FINISH))))))
                                     (push
                                       (list pattern `(,(if (eq step-direction 'up) '+ '-) ,pattern ,step-by-form))
                                       stepafter)))))))
                          (unless (parse-kw-p 'and) (return))
                          (setq kw 'and)
                          (case (next-kw) ((FOR AS) (pop body-rest)))))
                      (when (setq stepafter (apply #'append (nreverse stepafter)))
                        (push `(PSETQ ,@stepafter) stepafter-code))
                      (push 'NIL stepafter-code) ; Markierung für spätere Initialisierungen
                      (note-initialization ; outer `note-initialization'!
                        (make-loop-init
                          :specform 'LET
                          :bindings (nreverse bindings)
                          :declspecs (nreverse declspecs)
                          :everytime nil
                          :requires-stepbefore old-seen-endtest
                          :depends-preceding depends-preceding))
                      (dolist (initialization (nreverse initializations))
                        (when (li-everytime initialization)
                          (setf (li-everytime initialization) stepafter-code))
                        (note-initialization initialization))))
                   ((REPEAT)
                    (let ((form (parse-form kw))
                          (var (gensym)))
                      (note-initialization
                        (make-loop-init
                          :specform 'LET
                          :bindings `((,var ,form))
                          :declspecs nil
                          :everytime nil
                          :requires-stepbefore seen-endtest
                          :depends-preceding t))
                      (push `(SETQ ,var (1- ,var)) stepafter-code)
                      (note-initialization
                       (make-endtest `(UNLESS (PLUSP ,var) (LOOP-FINISH))))))))
                (t (error-of-type 'source-program-error
                     :form *whole* :detail *whole*
                     (TEXT "~S: illegal syntax near ~S in ~S")
                     'loop (first body-rest) *whole*)))))))
      ; Noch einige semantische Tests:
      (when (> (length results) 1)
        (error-of-type 'source-program-error
          :form *whole* :detail *whole*
          (TEXT "~S: ambiguous result:~:{~%~S from ~@{~{~A ~S~}~^, ~}~}")
          *whole* results))
      (unless (null results)
        (push `(RETURN-FROM ,block-name ,(caar results)) finally-code))
      ; Initialisierungen abarbeiten und optimieren:
      (let ((initializations1
             (unless (zerop (length *helpvars*))
               ;; `*helpvars*' must be bound first thing
               (list (make-loop-init
                      :specform 'LET
                      :bindings
                      (map 'list #'(lambda (var) `(,var NIL)) *helpvars*))))))
        ; `depends-preceding' backpropagation:
        (let ((later-depend nil))
          (dolist (initialization initializations)
            (when later-depend (setf (li-later-depend initialization) t))
            (when (li-depends-preceding initialization)
              (setq later-depend t))))
        (dolist (initialization (nreverse initializations))
          (let* ((everytime (li-everytime initialization))
                 (requires-stepbefore (li-requires-stepbefore initialization))
                 (name (li-specform initialization))
                 (bindings (li-bindings initialization))
                 (declarations (li-declspecs initialization))
                 (vars (case name
                         (MULTIPLE-VALUE-BIND (first bindings))
                         (LET (mapcar #'first bindings))))
                 (initforms
                   (case name
                     (MULTIPLE-VALUE-BIND `((MULTIPLE-VALUE-SETQ ,@bindings)))
                     (LET `((SETQ ,@(apply #'append bindings))))
                     (t '())))
                 (endtest-forms (li-endtest-forms initialization)))
            (if requires-stepbefore
              ; wegen seen-for-as-= oder AREF nicht optimierbar
              (progn
                (push
                  (make-loop-init
                    :specform 'LET
                    :bindings (default-bindings vars declarations)
                    :declspecs declarations)
                  initializations1)
                (if everytime
                  (if (li-later-depend initialization)
                    ;; double code: initially-code and stepafter-code
                    (setf startup-code (revappend endtest-forms (revappend initforms startup-code))
                          (cdr everytime) (revappend endtest-forms (revappend initforms (cdr everytime))))
                    (setq stepbefore-code (revappend endtest-forms (revappend initforms stepbefore-code))))
                  (setq startup-code (revappend endtest-forms (revappend initforms startup-code)))))
              ; Initialisierungsklausel nach initializations1 schaffen:
              (progn
                (push
                  (make-loop-init
                    :specform name
                    :bindings bindings
                    :declspecs declarations)
                  initializations1)
                (if everytime
                  (progn
                    ; put the initforms into the stepafter-code only.
                    (setf (cdr everytime) (revappend initforms (cdr everytime)))
                    ; handle the endtest-forms.
                    (if (li-later-depend initialization)
                      ;; double endtest: initially-code and stepafter-code
                      (setf startup-code (revappend endtest-forms startup-code)
                            (cdr everytime) (revappend endtest-forms (cdr everytime)))
                      (setq stepbefore-code (revappend endtest-forms stepbefore-code))))
                  (setq startup-code (revappend endtest-forms startup-code)))))))
        (flet ((check-accu-var (var)
                 (when (memq var var-list)
                   (error-of-type 'source-program-error
                     :form *whole* :detail var
                     (TEXT "~S: accumulation variable ~S is already bound")
                     *whole* var))))
          (push
           (make-loop-init
            :specform 'LET
            :bindings
            `(,@(mapcar #'(lambda (var) (check-accu-var var) `(,var NIL))
                        (delete-duplicates accu-vars-nil))
              ,@(mapcar #'(lambda (var) (check-accu-var (car var)) var)
                        (delete-duplicates accu-vars-0 :key #'car)))
            :declspecs (nreverse accu-declarations))
           initializations1))
        ;; Remove the NIL placeholders in stepafter-code.
        (setq stepafter-code (delete 'NIL stepafter-code))
        ;; If initially-code and stepafter-code both end in the same
        ;; forms, drag these forms across the label to stepbefore-code.
        (unless initially-code
          (flet ((form-eq (form1 form2) ; Calling EQUAL on user-given forms would be wrong
                   (or (eql form1 form2)
                       (and (consp form1) (consp form2)
                            (eql (length form1) (length form2))
                            (or (eq (car form1) (car form2))
                                (and (case (length form1) ((1 3) t))
                                     (case (car form1) ((SETQ PSETQ) t))
                                     (case (car form2) ((SETQ PSETQ) t))))
                            (every #'eq (cdr form1) (cdr form2))))))
            (loop
              (unless (and (consp startup-code) (consp stepafter-code)
                           (form-eq (car startup-code) (car stepafter-code)))
                (return))
              (setq stepbefore-code
                    (nconc stepbefore-code (list (pop stepafter-code))))
              (pop startup-code))))
        ;; Final macroexpansion.
        `(MACROLET ((LOOP-FINISH () (LOOP-FINISH-ERROR)))
           (BLOCK ,block-name
             ,(wrap-initializations initializations1
                `(MACROLET ((LOOP-FINISH () '(GO END-LOOP)))
                   (TAGBODY
                     ,@(if startup-code (nreverse startup-code))
                     ,@(if initially-code (nreverse initially-code))
                     BEGIN-LOOP
                     ,@(if stepbefore-code (nreverse stepbefore-code))
                     ,(cons 'PROGN (nreverse main-code))
                     ,@(if stepafter-code (nreverse stepafter-code))
                     (GO BEGIN-LOOP)
                     END-LOOP
                     ,@(mapcar #'(lambda (var)
                                   `(SETQ ,var (SYS::LIST-NREVERSE ,var)))
                               accu-vars-nreverse)
                     (MACROLET ((LOOP-FINISH () (LOOP-FINISH-WARN)
                                  '(GO END-LOOP)))
                       ,@(nreverse finally-code)))))))))))

;; Der eigentliche Macro:
(defmacro loop (&whole whole &body body)
  (if (some #'atom body)
    ;; "extended" loop form
    (expand-loop whole body)
    ;; "simple" loop form
    (let ((tag (gensym)))
      `(BLOCK NIL (TAGBODY ,tag ,@body (GO ,tag))))))
(defmacro loop-finish (&whole whole)
  (error (TEXT "~S is possible only from within ~S")
         whole 'loop))
(defun loop-finish-warn ()
  (warn (TEXT "Use of ~S in FINALLY clauses is deprecated because it can lead to infinite loops.")
        '(loop-finish)))
(defun loop-finish-error ()
  (error (TEXT "~S is not possible here")
         '(loop-finish)))

;; Run-Time-Support:

(defun max-if (x y)
  (if y (max x y) x))
(defun min-if (x y)
  (if y (min x y) x))
