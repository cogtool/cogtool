;; FORMAT - and company.
;; Bruno Haible 22.06.1988
;; CLISP-Version 16.08.1988, 03.09.1988, 04.08.1989
;; Major revision by Bruno Haible  14.02.1990-15.02.1990
;; Further revised and wrote FORMATTER 9.4.1995-11.4.1995
;; German comments translated into English: Stefan Kain 2001-09-09
;; pprint-logical-block ~:> support: John Boyland 2003

;; FORMAT is a mechanism for producing string output conveniently by,
;; basically, taking a pre-determined string with placeholders and
;; substituting computed values or strings for those placeholders --
;; though it became much more complex than this because the placeholders
;; included iteration primitives for producing lists of results,
;; plurals, and other such exotica.  It may be loosely characterized as
;; FORTRAN FORMAT statements gone berserk.
;; -- Guy L. Steele Jr. and Richard P. Gabriel in "The Evolution of Lisp"

(in-package "SYSTEM")

;;; ---------------------------------------------------------------------------

;; data-structure of control-string-directives:
(defstruct (control-string-directive
             (:copier nil)
             (:conc-name "CSD-")
             (:predicate nil)
             (:constructor make-csd ()))
  (type         0 :type fixnum)
  (cs-index     0 :type fixnum)
  (parm-list    nil :type list)
  (v-or-#-p     nil :type symbol)
  (colon-p      nil :type symbol)
  (atsign-p     nil :type symbol)
  (data         nil)
  (clause-chain nil))
;; Explanation:
;; type=0: directive ~<Newline>, nothing to print.
;;         further components are meaningless
;; type=1: String to be printed,
;;         from *FORMAT-CS* the portion :START cs-index :END data.
;;         further components are meaningless
;; type=2: execute format-directive.
;;         data = name of directive (Symbol),
;;         colon-p states, if there was a ':' ,
;;         atsign-p states, if there was a '@' ,
;;         parm-list = parameter-list for the directive,
;;         v-or-#-p states, if parm-list is to be processed before the call.
;;         clause-chain is a chain of pointers: e.g.  ~[...~;...~;...~]
;;         pointer from the ~[-directive to the list at the first ~;-directive,
;;         from there to the list at the next ~;-directive and so on.
;;         until eventually to the list at the ~]-directive.

;; check whether the character is a whitespace character. -- see io.d
;; (defun whitespacep (char)
;;   (member char '(#\Space #\Newline #\Linefeed #\Tab #\Return #\Page))
;;   (case char
;;     ((#\Space #\Newline #\Linefeed #\Tab #\Return #\Page) t)
;;     (t nil)))

;; (FORMAT-PARSE-CS control-string startindex csdl stop-at)
;; parses a control-string (exactly: (subseq control-string startindex))
;; and stores the resulting control-string-directive list in (cdr csdl) .
;; The parsing must end with the directive stop-at (a Character, or NIL
;; for the end of the String).
;; If stop-at /= NIL, a pointer to the sublist at the next separator is to
;; be stored in (csd-clause-chain (car csdl)). These pointers form
;; a simple list within csdl: from one Separator to the
;; next, finally to the end of the Clause.
(defun format-parse-cs (control-string startindex csdl stop-at)
  (declare (fixnum startindex))
  (macrolet ((errorstring ()
               (TEXT "The control string terminates within a format directive.")))
    (prog* ((index startindex)  ; cs-index of the next character
            ch                  ; current character
            intparam            ; Integer-Parameter
            newcsd              ; current CSD
            (last-separator-csd (car csdl)))
      (declare (type simple-string control-string) (type fixnum index))
      (loop                     ; new directive altogether
        (tagbody
          (when (>= index (length control-string))
            (go string-ended))
          (setq ch (schar control-string index))
          (unless (eql ch #\~)
            ;; possibly transform part of string into a separate directive,
            (setq csdl (setf (cdr csdl) (list (setq newcsd (make-csd)))))
            (setf (csd-type     newcsd) 1)
            (setf (csd-cs-index newcsd) index)
            (setq index (position #\~ control-string :start index))
            (unless index
              (setf (csd-data newcsd) (setq index (length control-string)))
              (go string-ended))
            (setf (csd-data newcsd) index))
          (setq csdl (setf (cdr csdl) (list (setq newcsd (make-csd)))))
          (setf (csd-type         newcsd) 2)
          (setf (csd-cs-index     newcsd) index)
          (setf (csd-parm-list    newcsd) nil)
          (setf (csd-v-or-#-p     newcsd) nil)
          (setf (csd-colon-p      newcsd) nil)
          (setf (csd-atsign-p     newcsd) nil)
          (setf (csd-data         newcsd) nil)
          (setf (csd-clause-chain newcsd) nil)

          param                 ; parameter of a directive may begin
          (incf index)
          (when (>= index (length control-string))
            (format-error 'error control-string index (errorstring))
            (go string-ended))
          (setq ch (schar control-string index))
          (when (digit-char-p ch) (go num-param))
          (case ch
            ((#\+ #\-) (go num-param))
            (#\' (go quote-param))
            ((#\V #\v #\#)
             (push (if (eql ch #\#) ':ARG-COUNT ':NEXT-ARG)
                   (csd-parm-list newcsd))
             (setf (csd-v-or-#-p newcsd) T)
             (go param-ok-1))
            (#\, (push nil (csd-parm-list newcsd)) (go param))
            (#\: (go colon-modifier))
            (#\@ (go atsign-modifier))
            (T (go directive)))

          num-param             ; numerical parameter
          (multiple-value-setq (intparam index)
            (parse-integer control-string :start index :junk-allowed t))
          (unless intparam
            (format-error 'error control-string index
                          (TEXT "~A must introduce a number.")
                          ch))
          (push intparam (csd-parm-list newcsd))
          (go param-ok-2)

          quote-param           ; Quote-Parameter-Treatment
          (incf index)
          (when (>= index (length control-string))
            (format-error 'error control-string index
              (TEXT "The control string terminates in the middle of a parameter."))
            (go string-ended))
          (setq ch (schar control-string index))
          (push ch (csd-parm-list newcsd))

          param-ok-1            ; Parameter OK
          (incf index)
          param-ok-2            ; Parameter OK
          (when (>= index (length control-string))
            (format-error 'error control-string index (errorstring))
            (go string-ended))
          (setq ch (schar control-string index))
          (case ch
            (#\, (go param))
            (#\: (go colon-modifier))
            (#\@ (go atsign-modifier))
            (T (go directive)))

          colon-modifier        ; after :
          (setf (csd-colon-p newcsd) T)
          (go passed-modifier)

          atsign-modifier       ; after @
          (setf (csd-atsign-p newcsd) T)
          (go passed-modifier)

          passed-modifier       ; after : or @
          (incf index)
          (when (>= index (length control-string))
            (format-error 'error control-string index (errorstring))
            (go string-ended))
          (setq ch (schar control-string index))
          (case ch
            (#\: (go colon-modifier))
            (#\@ (go atsign-modifier))
            (T (go directive)))

          directive             ; directive (its Name) reached
          (setf (csd-parm-list newcsd) (nreverse (csd-parm-list newcsd)))
          (let ((directive-name
                  (cdr (assoc (char-upcase ch)
                           ; with function-definition     ; without function-definition
                         '((#\A . FORMAT-ASCII)
                           (#\S . FORMAT-S-EXPRESSION)
                           (#\W . FORMAT-WRITE)
                           (#\D . FORMAT-DECIMAL)
                           (#\B . FORMAT-BINARY)
                           (#\O . FORMAT-OCTAL)
                           (#\X . FORMAT-HEXADECIMAL)
                           (#\R . FORMAT-RADIX)
                           (#\P . FORMAT-PLURAL)
                           (#\C . FORMAT-CHARACTER)
                           (#\F . FORMAT-FIXED-FLOAT)
                           (#\E . FORMAT-EXPONENTIAL-FLOAT)
                           (#\G . FORMAT-GENERAL-FLOAT)
                           (#\$ . FORMAT-DOLLARS-FLOAT)
                           (#\% . FORMAT-TERPRI)
                           (#\_ . FORMAT-PPRINT-NEWLINE)
                           (#\I . FORMAT-PPRINT-INDENT)
                           (#\& . FORMAT-FRESH-LINE)      (#\Newline . #\Newline)
                           (#\| . FORMAT-PAGE)
                           (#\~ . FORMAT-TILDE)
                           (#\T . FORMAT-TABULATE)
                           (#\* . FORMAT-GOTO)
                           (#\? . FORMAT-INDIRECTION)
                           (#\/ . FORMAT-CALL-USER-FUNCTION)
                           (#\( . FORMAT-CASE-CONVERSION) (#\) . FORMAT-CASE-CONVERSION-END)
                           (#\[ . FORMAT-CONDITIONAL)     (#\] . FORMAT-CONDITIONAL-END)
                           (#\{ . FORMAT-ITERATION)       (#\} . FORMAT-ITERATION-END)
                           (#\< . FORMAT-JUSTIFICATION)   (#\> . FORMAT-JUSTIFICATION-END)
                           (#\^ . FORMAT-UP-AND-OUT)      (#\; . FORMAT-SEPARATOR)
                           (#\! . FORMAT-CALL)
                           (#\. . FORMAT-ELASTIC-NEWLINE))))))
            (if directive-name
              (setf (csd-data newcsd) directive-name)
              (format-error 'error control-string index
                (TEXT "Non-existent format directive"))))
          (incf index)
          (case ch
            (#\/
             (let* ((start index)
                    (end (or (position #\/ control-string :start start)
                             (format-error 'error control-string index
                               (TEXT "Closing '/' is missing"))))
                    (pos (position #\: control-string :start start :end end))
                    (name (string-upcase
                            (subseq control-string
                                    (if pos
                                      (if (char= #\: (char control-string (1+ pos))) (+ 2 pos) (1+ pos))
                                      start)
                                    end)))
                    (pack (if pos
                            (let ((packname
                                    (string-upcase
                                      (subseq control-string start pos))))
                              (or (find-package packname)
                                  (format-error 'error control-string index
                                    (TEXT "There is no package with name ~S")
                                    packname)))
                            *common-lisp-user-package*)))
               (push (list (intern name pack)) (csd-parm-list newcsd))
               (setq index (1+ end))))
            (( #\( #\[ #\{)
             (multiple-value-setq (index csdl)
               (format-parse-cs control-string index csdl
                 (case ch (#\( #\)) (#\[ #\]) (#\{ #\}) ))))
            (#\<
             (multiple-value-setq (index csdl)
               (format-parse-cs control-string index csdl #\>))
             ;; (assert (eq (csd-data (car csdl)) 'FORMAT-JUSTIFICATION-END))
             (when (csd-colon-p (car csdl))
               (setf (csd-data newcsd) 'FORMAT-LOGICAL-BLOCK)))
            (( #\) #\] #\} #\> )
             (unless stop-at
               (format-error 'error control-string index
                 (TEXT "The closing format directive '~A' does not have a corresponding opening one.")
                 ch))
             (unless (eql ch stop-at)
               (format-error 'error control-string index
                 (TEXT "The closing format directive '~A' does not match the corresponding opening one. It should read '~A'.")
                 ch stop-at))
             (setf (csd-clause-chain last-separator-csd) csdl)
             (go end))
            (#\;
             (unless (or (eql stop-at #\]) (eql stop-at #\>))
               (format-error 'error control-string index
                 (TEXT "The ~~; format directive is not allowed at this point.")))
             (setf (csd-clause-chain last-separator-csd) csdl)
             (setq last-separator-csd newcsd))
            (#\Newline
             (setf (csd-type newcsd) 0)
             (if (csd-colon-p newcsd)
               (if (csd-atsign-p newcsd)
                 (format-error 'error control-string index
                   (TEXT "The ~~newline format directive cannot take both modifiers."))
                 nil) ; ~:<newline> -> ignore Newline, retain Whitespace
               (progn
                 (when (csd-atsign-p newcsd)
                   ;; ~@<newline> -> part of String with Newline for output
                   (setf (csd-type newcsd) 1)
                   (setf (csd-cs-index newcsd) (1- index))
                   (setf (csd-data newcsd) index))
                 (setq index
                   (or (position-if-not #'whitespacep control-string :start index)
                       (length control-string)))))))
        ) ; tagbody finished
      )   ; loop finished

      string-ended
      (when stop-at
        (format-error 'error control-string index
          (TEXT "An opening format directive is never closed; expecting '~A'.")
          stop-at))

      end
      (return (values index csdl)))))

;;; ---------------------------------------------------------------------------

(defvar *FORMAT-CS*)            ; control-string
(defvar *FORMAT-CSDL*)          ; control-string directive list
(defvar *FORMAT-ARG-LIST*)      ; argument-list
(defvar *FORMAT-NEXT-ARG*)   ; pointer to next argument in argument-list
(defvar *FORMAT-NEXT-ARGLIST*) ; pointer to next sublist in ~:{ iteration
(defvar *FORMAT-UP-AND-OUT* nil) ; reason for up-and-out

;; (format-error type {keyword value}* control-string errorpos errorstring . arguments)
;; signals an Error of the given type, that occurred in FORMAT. The position
;; in the Control-string is marked with an arrow.
(defun format-error (type &rest arguments)
  (let ((type-initargs '()))
    (loop
      (unless (keywordp (car arguments)) (return))
      (push (pop arguments) type-initargs)
      (push (pop arguments) type-initargs))
    (let* ((control-string (pop arguments))
           (errorpos (pop arguments))
           (errorstring (pop arguments)))
      (when control-string
        (unless errorpos (setq errorpos (csd-cs-index (car *FORMAT-CSDL*))))
        (setq errorstring
          (string-concat errorstring "~%"
            (TEXT "Current point in control string:")))
        (let ((pos1 0) (pos2 0))
          (declare (simple-string errorstring) (fixnum pos1 pos2))
          (loop
            (setq pos2 (or (position #\Newline control-string :start pos1)
                           (length control-string)))
            (setq errorstring (string-concat errorstring "~%  ~A"))
            (setq arguments
              (nconc arguments (list (substring control-string pos1 pos2))))
            (when (<= pos1 errorpos pos2)
              (setq errorstring (string-concat errorstring "~%~VT" "|"))
              (setq arguments (nconc arguments (list (+ (- errorpos pos1) 2)))))
            (when (= pos2 (length control-string)) (return))
            (setq pos1 (+ pos2 1)))))
      (apply #'error-of-type
             type (nreconc type-initargs (list* errorstring arguments))))))

;;; ---------------------------------------------------------------------------

(defun format (destination control-string &rest arguments)
  (unless (or (stringp control-string) (functionp control-string))
    (format-cs-error control-string))
  (cond ((null destination)
         (let ((stream (make-string-output-stream)))
           (format-apply stream control-string arguments)
           (get-output-stream-string stream)))
        ((eq destination 'T)
         (format-apply *standard-output* control-string arguments)
         nil)
        ((streamp destination)
         (format-apply destination control-string arguments)
         nil)
        ((stringp destination)
         (if (array-has-fill-pointer-p destination)
           (let ((stream (sys::make-string-push-stream destination)))
             (format-apply stream control-string arguments))
           (error-of-type 'error
             (TEXT "The destination string ~S should have a fill pointer.")
             destination))
         nil)
        (t (error-of-type 'type-error
             :datum destination :expected-type '(or boolean stream string)
             (TEXT "The destination argument ~S is invalid (not NIL or T or a stream or a string).")
             destination))))

(defun format-apply (stream control-string arguments
                     &optional (whole-arguments arguments))
  (cond ((stringp control-string)
         ;; possibly convert control-string into Simple-String ??
         (let ((node (list control-string)))
           (format-parse-cs control-string 0 node nil)
           (let* ((*FORMAT-CS*         (car node))
                  (*FORMAT-CSDL*       (cdr node))
                  (*FORMAT-ARG-LIST*   whole-arguments)
                  (*FORMAT-NEXT-ARG*   arguments)
                  (*FORMAT-NEXT-ARGLIST* nil)
                  (*FORMAT-UP-AND-OUT* nil))
             (format-interpret stream)
             *FORMAT-NEXT-ARG*)))
        ((functionp control-string)
         (let ((*FORMAT-CS* nil)) ; format-error cannot point to the position anymore
           (apply control-string stream arguments)))
        (t (format-cs-error control-string))))

(defun format-cs-error (control-string)
  (error-of-type 'type-error
    :datum control-string :expected-type '(or string function)
    (TEXT "~S: The control-string must be a string, not ~S")
    'format control-string))

;;; ---------------------------------------------------------------------------

;; (next-arg) returns (and consumes) the next argument from the argument-
;; list *FORMAT-NEXT-ARG*.
(defun next-arg ()
  (if (atom *FORMAT-NEXT-ARG*)
    (if (null *FORMAT-NEXT-ARG*)
      (format-error 'error *FORMAT-CS* nil
        (TEXT "There are not enough arguments left for this format directive."))
      (format-error 'type-error :datum *FORMAT-NEXT-ARG* :expected-type 'LIST
        *FORMAT-CS* nil
        (TEXT "The argument list is a dotted list: ~S")
        *FORMAT-ARG-LIST*))
    (pop *FORMAT-NEXT-ARG*)))

;; (format-interpret stream [endmarker]) interprets *FORMAT-CSDL* .
;; Fluid vars:
;;   *FORMAT-ARG-LIST*
;;   *FORMAT-NEXT-ARG*
;;   *FORMAT-NEXT-ARGLIST*
;;   *FORMAT-CS*
;;   *FORMAT-CSDL*
;;   *FORMAT-UP-AND-OUT*
;; Stop interpretation when arriving at the directive endmarker
;; or the directive ~; .
(defun format-interpret (stream &optional (endmarker nil))
  (loop
    (when *FORMAT-UP-AND-OUT* (return))
    (when (endp *FORMAT-CSDL*) (return))
    (let ((csd (car *FORMAT-CSDL*)))
      (case (csd-type csd)
        (0 )
        (1 (write-string *FORMAT-CS* stream
             :start (csd-cs-index csd) :end (csd-data csd)))
        (2 (let ((directive-name (csd-data csd)))
             (if (eq directive-name endmarker) (return))
             (if (eq directive-name 'FORMAT-SEPARATOR) (return))
             (apply directive-name
               stream
               (csd-colon-p csd)
               (csd-atsign-p csd)
               (format-resolve-parms csd))))))
    (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))))

;; returns the correct argument-list of a CSD, possibly with substituted
;; parameters: V (as :NEXT-ARG) and # (as :ARG-COUNT) are resolved.
(defun format-resolve-parms (csd)
  (let ((arglist (csd-parm-list csd)))
    (if (csd-v-or-#-p csd)
      (mapcar #'(lambda (arg)
                  (case arg
                    (:NEXT-ARG (next-arg))
                    (:ARG-COUNT (list-length *FORMAT-NEXT-ARG*))
                    (T arg)))
              arglist)
      arglist)))

;; Defines a simple FORMAT-subfunction, i.e. a function that consumes
;; exactly one argument.
(defmacro defformat-simple (name (stream colon atsign . optionals-with-defaults)
                            (arg) &body body)
  (multiple-value-bind (body-rest declarations) (sys::parse-body body)
    (let ((name2 (concat-pnames "DO-" name)) ; in #<PACKAGE SYSTEM>
          (optionals (mapcar #'(lambda (opt) (if (consp opt) (first opt) opt))
                             optionals-with-defaults)))
      `(PROGN
         (DEFUN ,name (,stream ,colon ,atsign &OPTIONAL ,@optionals)
           (,name2 ,stream ,colon ,atsign ,@optionals (next-arg)))
         (DEFUN ,name2 (,stream ,colon ,atsign ,@optionals ,arg)
           ,@(if declarations `((DECLARE ,@declarations)))
           ,@(mapcap #'(lambda (opt)
                         (if (and (consp opt) (not (null (second opt))))
                           `((IF (NULL ,(first opt))
                               (SETQ ,(first opt) ,(second opt))))
                           '()))
                     optionals-with-defaults)
           ,@body-rest)))))

;; Moves the value of "Pointers into the argument-list" in one direction.
(defun format-goto-new-arg (backwardp index)
  (if backwardp
    ;; backwards
    (setq *FORMAT-NEXT-ARG*
          (nthcdr (max (- (list-length *FORMAT-ARG-LIST*)
                          (list-length *FORMAT-NEXT-ARG*)
                          index)
                       0)
                  *FORMAT-ARG-LIST*))
    ;; forwards is easier:
    (setq *FORMAT-NEXT-ARG* (nthcdr index *FORMAT-NEXT-ARG*))))

;; prints arg as old-Roman number to stream, e.g. 4 as IIII.
(defun format-old-roman (arg stream)
  (unless (and (integerp arg) (<= 1 arg 4999))
    (format-error 'type-error :datum arg :expected-type '(INTEGER 1 4999)
      *FORMAT-CS* nil
      (TEXT "The ~~:@R format directive requires an integer in the range 1 - 4999, not ~S")
      arg))
  (do ((charlistr  '(#\M  #\D #\C #\L #\X #\V #\I) (cdr charlistr))
       (valuelistr '(1000 500 100 50  10   5   1) (cdr valuelistr))
       (value arg (multiple-value-bind (multiplicity restvalue)
                      (floor value (first valuelistr))
                    (dotimes (i multiplicity)
                      (write-char (first charlistr) stream))
                    restvalue)))
      ((zerop value))))

;; prints arg as new-Roman number to stream, e.g. 4 as IV.
(defun format-new-roman (arg stream)
  (unless (and (integerp arg) (<= 1 arg 3999))
    (format-error 'type-error :datum arg :expected-type '(INTEGER 1 3999)
      *FORMAT-CS* nil
      (TEXT "The ~~@R format directive requires an integer in the range 1 - 3999, not ~S")
      arg))
  (do ((charlistr       '(#\M #\D #\C #\L #\X #\V #\I) (cdr charlistr))
       (valuelistr     '(1000 500 100 50  10   5   1 ) (cdr valuelistr))
       (lowercharlistr  '(#\C #\C #\X #\X #\I #\I    ) (cdr lowercharlistr))
       (lowervaluelistr '(100 100 10  10   1   1   0 ) (cdr lowervaluelistr))
       (value arg
         (multiple-value-bind (multiplicity restvalue)
             (floor value (first valuelistr))
           (dotimes (i multiplicity) (write-char (first charlistr) stream))
           (let ((loweredvalue (- (first valuelistr) (first lowervaluelistr))))
             (if (>= restvalue loweredvalue)
               (progn
                 (write-char (first lowercharlistr) stream)
                 (write-char (first charlistr) stream)
                 (- restvalue loweredvalue))
               restvalue)))))
      ((zerop value))))

(defconstant FORMAT-CARDINAL-ONES
  '#(NIL "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
     "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen"
     "seventeen" "eighteen" "nineteen"))

(defconstant FORMAT-CARDINAL-TENS
  '#(NIL NIL "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty"
     "ninety"))

;; (format-small-cardinal arg stream) prints an 0< integer <1000 in
;; plain English to stream. (arg=0 -> on output.)
(defun format-small-cardinal (arg stream)
  (multiple-value-bind (hundreds tens-and-ones) (truncate arg 100)
    (when (> hundreds 0)
      (write-string (svref FORMAT-CARDINAL-ONES hundreds) stream)
      (write-string " hundred" stream))
    (when (> tens-and-ones 0)
      (when (> hundreds 0) (write-string " and " stream))
      (multiple-value-bind (tens ones) (truncate tens-and-ones 10)
        (if (< tens 2)
          (write-string (svref FORMAT-CARDINAL-ONES tens-and-ones) stream)
          (progn
            (write-string (svref FORMAT-CARDINAL-TENS tens) stream)
            (when (> ones 0)
              (write-char #\- stream)
              (write-string (svref FORMAT-CARDINAL-ONES ones) stream))))))))

;; (format-cardinal arg stream) prints the integer arg in plain English
;; to stream.
(defun format-cardinal (arg stream) ; arg Integer
  (if (zerop arg)
    (write-string "zero" stream)
    (progn
      (when (minusp arg) (write-string "minus " stream) (setq arg (- arg)))
      (labels ((blocks1000 (illions-list arg) ; decomposition in 1000er-Blocks
                 (when (null illions-list)
                   (format-error 'type-error :datum arg :expected-type '(INTEGER 0 999999999999999999999999999999999999999999999999999999999999999999)
                     *FORMAT-CS* nil
                     (TEXT "The argument for the ~~R format directive is too large.")))
                 (multiple-value-bind (thousands small) (truncate arg 1000)
                   (when (> thousands 0)
                     (blocks1000 (cdr illions-list) thousands))
                   (when (> small 0)
                     (when (> thousands 0)
                       (write-string ", " stream))
                     (format-small-cardinal small stream)
                     (write-string (car illions-list) stream)))))
        (blocks1000
          ; American (billion=10^9)
          '("" " thousand" " million" " billion" " trillion" " quadrillion"
            " quintillion" " sextillion" " septillion" " octillion"
            " nonillion" " decillion" " undecillion" " duodecillion"
            " tredecillion" " quattuordecillion" " quindecillion"
            " sexdecillion" " septendecillion" " octodecillion"
            " novemdecillion" " vigintillion")
          arg)))))

(defconstant FORMAT-ORDINAL-ONES
  '#(NIL "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth"
     "ninth" "tenth" "eleventh" "twelfth" "thirteenth" "fourteenth"
     "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"))

;; (format-ordinal arg stream) prints an integer arg as an ordinal number in
;; plain English to stream.
(defun format-ordinal (arg stream) ; arg Integer
  (if (zerop arg)
    (write-string "zeroth" stream)
    (progn
      (when (minusp arg) (write-string "minus " stream) (setq arg (- arg)))
      (multiple-value-bind (hundreds tens-and-ones) (floor arg 100)
        (when (> hundreds 0) (format-cardinal (* hundreds 100) stream))
        (if (zerop tens-and-ones)
          (write-string "th" stream)
          (multiple-value-bind (tens ones) (floor tens-and-ones 10)
            (when (> hundreds 0) (write-char #\Space stream))
            (cond ((< tens 2)
                   (write-string (svref FORMAT-ORDINAL-ONES tens-and-ones) stream))
                  ((zerop ones)
                   (write-string
                     (svref '#(NIL "tenth" "twentieth" "thirtieth" "fortieth"
                               "fiftieth" "sixtieth" "seventieth" "eightieth"
                               "ninetieth")
                            tens)
                     stream))
                  (t (write-string (svref FORMAT-CARDINAL-TENS tens) stream)
                     (write-char #\- stream)
                     (write-string (svref FORMAT-ORDINAL-ONES ones)
                                   stream)))))))))

;; (format-padding count char stream) prints count (a Fixnum >=0)
;; characters char to stream.
(defun format-padding (count char stream)
  (dotimes (i count) (write-char char stream)))

;; prints to Stream stream:
;; the String str, possibly filled with padding characters padchar.
;; width is at least mincol. In order to achieve that,
;; at least minpad characters are inserted, possibly additional ones in
;; Blocks of colinc characters. if padleftflag, they are inserted on the left,
;; else right of the String.
(defun format-padded-string (mincol colinc minpad padchar padleftflag
                             str stream)
  (let* ((need (+ (string-width str) minpad)) ; it least that number of columns
         (auxpad (if (< need mincol)
                   (* (ceiling (- mincol need) colinc) colinc)
                   0))) ; this many additional characters
    (unless padleftflag (write-string str stream))
    (format-padding (+ minpad auxpad) padchar stream)
    (when padleftflag (write-string str stream))))

;; prints the Integer arg to Stream:
;; in Base base, with sign (+ only if >=0 and positive-sign-flag), with
;; commaflag, every three digits are separated by the character
;; commachar.  fill on the left with padchar's, so that the total width
;; is at least mincol.
(defun format-integer (base
                       mincol
                       padchar
                       commachar
                       commainterval
                       commaflag
                       positive-sign-flag
                       arg
                       stream)
  (let* ((*print-base* base)
         (*print-radix* nil)
         (*print-readably* nil))
    (if (and (zerop mincol) (not commaflag) (not positive-sign-flag))
      (princ arg stream)        ; normal output does the job
      (let* ((oldstring (princ-to-string arg))
             (oldstring-length (length oldstring))
             (number-of-digits
               (if (minusp arg) (1- oldstring-length) oldstring-length) )
             (number-of-commas
               (if commaflag (floor (1- number-of-digits) commainterval) 0) )
             (positive-sign (and positive-sign-flag (>= arg 0)))
             (newstring-length
               (+ (if positive-sign 1 0) ; sign
                  oldstring-length number-of-commas)) ; digits, commas
             (newstring (make-string newstring-length)) )
        ;; first the sign +:
        (when positive-sign (setf (schar newstring 0) #\+))
        ;; Then convert oldstring in newstring, skipping the commas:
        (let ((oldpos oldstring-length) (newpos newstring-length))
          (loop
            (decf oldpos)
            (when (minusp oldpos) (return))
            (decf newpos)
            (setf (schar newstring newpos) (schar oldstring oldpos))
            (when (and (plusp number-of-commas) ; insert a comma?
                       (zerop (mod (- oldstring-length oldpos) commainterval)))
              (decf newpos)
              (setf (schar newstring newpos) commachar)
              (decf number-of-commas))))
        (if (zerop mincol)
          (write-string newstring stream) ; faster
          (format-padded-string mincol 1 0 padchar t newstring stream))))))

;; non-numeric argument for numeric format instruction is printed with ~A
(defun format-ascii-decimal (arg stream)
  (let ((*print-base* 10.)
        (*print-radix* nil)
        (*print-readably* nil))
    (princ arg stream)))

;; subroutine for ~D, ~B, ~O, ~X:
(defun format-base (base stream colon-modifier atsign-modifier
                    mincol padchar commachar commainterval
                    arg)
  (if (or (and (zerop mincol) (not colon-modifier) (not atsign-modifier))
          (not (integerp arg)))
    (let ((*print-base* base)
          (*print-radix* nil)
          (*print-readably* nil))
      (if (integerp arg)
        (princ arg stream)
        (format-padded-string mincol 1 0 padchar t
                              (princ-to-string arg) stream)))
    (format-integer base mincol padchar commachar commainterval
                    colon-modifier atsign-modifier arg stream)))

;; (format-scale-exponent-aux arg zero one ten tenth lg2)
;; returns two values for the Floating-Point-Number arg >= 0 and
;; zero = 0.0, one = 1.0, ten = 10.0, tenth = 0.1, lg2 = log(2)/log(10)
;; (the four in the same floating point precision as arg):
;; mantissa and n, with n being an integer
;; and mantissa being a floating-point-number, 0.1 <= mantissa < 1,
;; arg = mantissa * 10^n (also 10^(n-1) <= arg < 10^n ).
;; (for arg=zero: zero and n=0.)
(defun format-scale-exponent-aux (arg zero one ten tenth lg2)
  (multiple-value-bind (significand expon) (decode-float arg)
    (declare (ignore significand))
    (if (zerop arg)
      (values zero 0)
      (let* ((expon10a (truncate (* expon lg2))) ; round is not used, in order to avoid overflow
             (signif10a (/ arg (expt ten expon10a))))
        (do ((ten-power ten (* ten-power ten))
             (signif10b signif10a (/ signif10a ten-power))
             (expon10b expon10a (1+ expon10b)))
            ((< signif10b one)
             (do ((ten-power ten (* ten-power ten))
                  (signif10c signif10b (* signif10b ten-power))
                  (expon10c expon10b (1- expon10c)))
                 ((>= signif10c tenth)
                  (values signif10c expon10c)))))))))

;; (format-scale-exponent arg)
;; returns two values for floating point number arg >= 0:
;; mantissa and n, with integer n
;; and floating-point mantissa, 0.1 <= mantissa < 1,
;; arg = mantissa * 10^n (also 10^(n-1) <= arg < 10^n ).
;; (for arg=zero: 0.0 and n=0.)
(defun format-scale-exponent (arg)
  (format-scale-exponent-aux arg 0 1 10 1/10 (log 2 (float 10 arg)))
  #+(or)
  (cond ((short-float-p arg)
         (format-scale-exponent-aux arg 0.0s0 1.0s0 10.0s0 0.1s0 0.30103s0))
        ((single-float-p arg)
         (format-scale-exponent-aux arg 0.0f0 1.0f0 10.0f0 0.1f0 0.30103s0))
        ((double-float-p arg)
         (format-scale-exponent-aux arg 0.0d0 1.0d0 10.0d0 0.1d0 0.30103s0))
        ((long-float-p arg)
         (format-scale-exponent-aux arg
           (float 0 arg) (float 1 arg) (float 10 arg) (float 1/10 arg)
           0.30102999566d0))))  ; lg2 is needed with 32 Bit-Precision

;; (format-float-to-string arg width d k dmin)
;; returns a String for Floating-point arg:
;; it has the value of (* (abs arg) (expt 10 k)), with at least d digits behind
;; the decimal point and at most the length width (width=nil -> no limitation).
;; Nevertheless there is no rounding to less than dmin digits.
(let ((digit-string (make-array 20 :element-type 'character
                                :adjustable t :fill-pointer t)))
(defun format-float-to-string (arg width d k dmin)
  (if (zerop arg)
    (let ((places (max (or d 0) (or dmin 0))))
      (when width ; width specified -> places := (min places (1- width))
        (when (>= places width) (setq places (1- width))))
      (values
        (let ((str (make-string (1+ places) :initial-element #\0)))
          (setf (schar str 0) #\.)
          str)                  ; one decimal point and places zeros
        (1+ places)             ; number of digits
        t                       ; decimal point in front
        (zerop places)          ; decimal point at the end?
        0))                     ; position of the decimal point
    (multiple-value-bind (significand expon) (integer-decode-float arg)
      ;; significand : Integer >0
      ;; expon : Integer
      ;; mantprec : number of real Mantissa-bits of significand
      ;; (so 2^mantprec <= significand < 2^(mantprec+1))
      ;; width : number of digits, that the number (including decimal point)
      ;;         is not to overshoot, or NIL
      ;;   at least 2: a digit and the decimal point
      (when width (setq width (max width 2)))
      ;; d : minimum number of digits behind the decimal point or NIL
      ;; k : scaling factor (ref. CLTL p.394)
      ;; dmin : minimum number of digits, that
      ;;        may not be rounded (despite the specification of width or d).
      ;;        (only interesting, if d <= dmin <= (precision of number).)
      ;; converts the number significand*2^expon into a decimal-string.
      ;; There is no exponent present.
      (let* ((mantprec (1- (float-digits arg)))
             (numerator significand)
             (denominator 1)
             (round-down-1 1) ; rounding-off unit:
             ;; rounding off by 1 in the last digit that can be rounded off
             ;; corresponds to the decrease of the numerator by round-down-1.
             (round-up-1 1) ; rounding-up unit:
             ;; rounding up by 1 in the last digit that can be rounded up
             ;; corresponds to the increase of the numerator by round-up-1.
             ;; positions: 0 = 1st. digit in front of the decimal point,
             ;; -1 = 1st. digit behind the decimal point.
             (posn 0) ; position of the next digit to be printed
             (digit-count 0) ; number of the printed digits so far in
                             ; digit-string (excluding the decimal point)
             (point-pos 0) ; decimal-point-position = number of leading digits
                           ; = number of digits in front of the decimal point
             (last-pos nil) ; NIL or position of the last significant digit
                            ;  (if d or width were specified)
             (halbzahlig nil) ; indicates, if 0.50000 can be dropped at the end
             digit              ; the current digit, >=0, <10
             (round-down-p nil) ; T if last digit is to be rounded off
             (round-up-p nil)) ; T if last digit is to be rounded up
        (setf (fill-pointer digit-string) 0) ; empty the digit-string
        (cond ((> expon 0)
               (setq numerator (ash significand expon))
               (setq round-up-1 (setq round-down-1 (ash 1 expon))))
              ((< expon 0)          ; round-up-1 = round-down-1 = 1
               (setq denominator (ash 1 (- expon)))))
        ;; number = numerator/denominator
        (when (= significand (ash 1 mantprec))
          ;; If Significand=2^mantprec, round-down-1 can be halved.
          ;; Instead, the other three items can be doubled:
          (setq round-up-1 (ash round-up-1 1))
          (setq numerator (ash numerator 1))
          (setq denominator (ash denominator 1)))
        ;; default behavior: rounding-unit = one unit in the last
        ;; BINARY-digit.
        ;; number = numerator/denominator
        ;; work scaling factor k into the number (ref. CLTL p.394)
        ;; k<0 -> divide mantissa by 10^(abs k)
        ;; k>0 -> multiply mantissa with 10^k
        ;; Retain ratio between round-up-1/round-down-1 and numerator.
        (when k
          (if (< k 0)
            (let ((scal-factor (expt 10 (- k))))
              (setq denominator (* denominator scal-factor)))
            (let ((scal-factor (expt 10 k)))
              (setq numerator (* numerator scal-factor))
              (setq round-up-1 (* round-up-1 scal-factor))
              (setq round-down-1 (* round-down-1 scal-factor)))))
        ;; adjust to >= 1/10 : (multiply numerator with 10 at a time and
        ;; plan for an additional leading 0)
        (do ()
            ((>= (* numerator 10) denominator))
          (setq posn (1- posn))
          (setq numerator (* numerator 10))
          (setq round-down-1 (* round-down-1 10))
          (setq round-up-1 (* round-up-1 10)))
        ;; posn = position of the final leading 0
        ;;        = 1 + position of the 1st. significant digit
        ;;        or =0, if k>=0
        ;; implementation of the rounding:
        (loop
          ;; so long as the result stays >= 1 even after rounding up,
          ;; plan for one more digit in front of the decimal point:
          (do ()
              ((< (+ (ash numerator 1) round-up-1) (ash denominator 1)))
            (setq denominator (* denominator 10))
            (setq posn (1+ posn)))
          ;; if d or width is specified: calculate last-pos
          (if d
            ;; if dmin is specified: (min (- d) (- dmin)) = (- (max d dmin)).
            ;; else (- d).
            (progn
              (setq last-pos (- d))
              (when (and dmin (> last-pos (- dmin)))
                (setq last-pos (- dmin))))
            ;; if not d, only specify width:
            (when width
              (if (< posn 0)
                ;; leading zeros behind the decimal point -> d:=(1- width)
                (setq last-pos (- 1 width))
                ;; no leading zeros behind the decimal point -> there will
                ;; be posn digits in front of the point, d:=(- (1- width) posn)
                (setq last-pos (1+ (- posn width))))
              ;; last-pos = (- (- (1- width) (max posn 0)))
              ;; take dmin into account again
              (when (and dmin (> last-pos (- dmin)))
                (setq last-pos (- dmin)))))
          (when (or d width)
            (let* ((ziffernzahl (- last-pos posn))
                            ; = - number of significant digits or >=0.
                   (decimal-1 denominator))
                            ; := (ceiling (* decimal-1 (expt 10 ziffernzahl)))
              (if (>= ziffernzahl 0)
                (dotimes (i ziffernzahl)
                  (setq decimal-1 (* decimal-1 10)))
                (dotimes (i (- ziffernzahl))
                  (setq decimal-1 (ceiling decimal-1 10))))
              ;; decimal-1 = amount by which numerator has to be increased
              ;; resp. decreased, therewith the decimal representation is
              ;; changed by exactly 1 at the position last-pos
              (setq round-down-1 (max decimal-1 round-down-1))
              (setq round-up-1 (max decimal-1 round-up-1))
              ;; now rounding my take place by one (half) decimal-1.
              (when (= round-up-1 decimal-1) (setq halbzahlig T))))
          (when (< (+ (ash numerator 1) round-up-1) (ash denominator 1))
            (return)))
        ;; posn = position of the first significant digit + 1
        ;; print leading point and consecutive zeros:
        (when (< posn 0)
          (setq point-pos digit-count)
          (vector-push-extend #\. digit-string)
          (dotimes (i (- posn))
            (incf digit-count)
            (vector-push-extend #\0 digit-string)))
        ;; print digits of the mantissa:
        (loop
          (when (zerop posn)
            (vector-push-extend #\. digit-string)
            (setq point-pos digit-count))
          (decf posn)
          (multiple-value-setq (digit numerator)
            (truncate (* numerator 10) denominator))
          (setq round-down-1 (* round-down-1 10))
          (setq round-up-1 (* round-up-1 10))
          (setq round-down-p (< (ash numerator 1) round-down-1))
          (if halbzahlig
            (setq round-up-p
                  (>= (ash numerator 1) (- (ash denominator 1) round-up-1)))
            (setq round-up-p
                  (>= (ash numerator 1) (- (ash denominator 1) round-up-1))))
          (when (or round-down-p round-up-p
                    (and last-pos (<= posn last-pos)))
            (return))
          (vector-push-extend (schar #1="0123456789" digit) digit-string)
          (incf digit-count))
        ;; print last significant digit:
        (when (or (null last-pos) (>= posn last-pos))
          (vector-push-extend
           (schar #1#
                  (cond ((and round-down-p (not round-up-p)) digit)
                        ((and round-up-p (not round-down-p)) (1+ digit))
                        ((<= (ash numerator 1) denominator) digit)
                        (t (1+ digit))))
           digit-string)
          (incf digit-count))
        ;; print consecutive zeros and point
        (when (>= posn 0)
          (dotimes (i posn)
            (incf digit-count)
            (vector-push-extend #\0 digit-string))
          (vector-push-extend #\. digit-string)
          (setq point-pos digit-count))
        (when d
          (dotimes (i (- d (- digit-count point-pos)))
            (incf digit-count)
            (vector-push-extend #\0 digit-string)))
        (values                     ; 5 values
          digit-string              ; digits
          (1+ digit-count)          ; number of digits
          (= point-pos 0)           ; leading point?
          (= point-pos digit-count) ; trailing point?
          point-pos)))))            ; position of the decimal point
) ; let

;; (format-float-for-f w d k overflowchar padchar plus-sign-flag arg stream)
;; prints the Floating-Point-Number arg in Fix-Comma-Representation to stream.
(defun format-float-for-f (w d k overflowchar padchar plus-sign-flag
                           arg stream)
  (let ((width (if w (if (or plus-sign-flag (minusp arg)) (1- w) w) nil)))
    ;; width = available characters without sign
    (multiple-value-bind (digits digitslength leadingpoint trailingpoint)
        (format-float-to-string arg width d k 0)
      (when (eql d 0)
        (setq trailingpoint nil)) ; d=0 -> no additional zero behind
      (when w
        (setq width (- width digitslength))
        (when leadingpoint      ; plan possibly additional zero ahead
          (if (> width 0) (setq width (1- width)) (setq leadingpoint nil)))
        (when trailingpoint     ; plan possibly additional zero behind
          (if (> width 0) (setq width (1- width)) (setq trailingpoint nil))))
      ;; width characters still remain.
      (if (and overflowchar w (minusp width))
        (format-padding w overflowchar stream) ; not enough room -> overflow
        (progn
          (when (and w (> width 0)) (format-padding width padchar stream))
          (if (minusp arg)
            (write-char #\- stream)
            (if plus-sign-flag (write-char #\+ stream)))
          (when leadingpoint (write-char #\0 stream))
          (write-string digits stream)
          (when trailingpoint (write-char #\0 stream)))))))

;; (format-float-for-e w d e k overflowchar padchar exponentchar plus-sign-flag
;;                     arg stream)
;; prints the Floating-Point-Number arg
;; in Exponential representation to stream.
;; (compare CLTL p.392-394)
;; partitioning of Mantissa:
;;   if k<=0,    first 1 zero (if fits in width), then the point,
;;               then |k| zeros, then d-|k| significant digits;
;;               which is d digits behind the point, altogether.
;;   Falls k>0,  first k significant digits, then the point,
;;               then d-k+1 further significant digits;
;;               which is d+1 significant digits, altogether.
;;               no zeros in front.
;;   (The default in FORMAT-EXPONENTIAL-FLOAT is k=1.)
;; the sign in front of the Mantissa (a + only if arg>=0 and plus-sign-flag).
;; then the Exponent, prefaced by exponentchar, then sign of the
;; Exponent (always + or -), then e digits for the Exponent.
;; Then fill the whole thing with padchars to w characters.
;; If this results (even after possible suppression of a leading zero) in
;; more than w characters, print w overflowchars instead, or
;; (if overflowchar = nil) print the number with as many digits
;; as necessary.
(defun format-float-for-e (w d e k
       overflowchar padchar exponentchar plus-sign-flag arg stream)
  (multiple-value-bind (mantissa oldexponent) (format-scale-exponent (abs arg))
    (let* ((exponent (if (zerop arg) 0 (- oldexponent k))) ; Exponent to be printed
           (expdigits (write-to-string (abs exponent) :base 10.
                                       :radix nil :readably nil))
           ;; expdigitsneed = number of digits, that are necessary
           ;; for the Exponent.
           (expdigitsneed (if e (max (length expdigits) e) (length expdigits)))
           ;; mantd = number of Mantissa-Digits behind the point
           (mantd (if d (if (> k 0) (1+ (- d k)) d) nil))
           ;; no rounding takes place within the first (+ 1 (abs k)) digits.
           (dmin (if (minusp k) (- 1 k) nil)) ; hereafter: demand, that
           ;; mantwidth = number of available characters (or nil)
           ;; for the Mantissa (incl. sign, point)
           (mantwidth (if w (- w 2 expdigitsneed) nil)))
      (declare (simple-string expdigits) (fixnum exponent expdigitsneed))
      (if (and overflowchar w e (> expdigitsneed e))
        ;; if Overflowchar and w and e being stated, Exponent needs more room:
        (format-padding w overflowchar stream)
        (progn
          (if w
            (if (or plus-sign-flag (minusp arg))
              (setq mantwidth (1- mantwidth))))
          ;; mantwidth = number of available characters (or nil)
          ;;  for the Mantissa (without sign,including point)
          (multiple-value-bind (mantdigits mantdigitslength
                                leadingpoint trailingpoint)
              (format-float-to-string mantissa mantwidth mantd k dmin)
            (when w
              (setq mantwidth (- mantwidth mantdigitslength))
              (if trailingpoint
                (if (or (null mantd) (> mantd 0))
                  (setq mantwidth (- mantwidth 1))
                  (setq trailingpoint nil)))
              (if leadingpoint
                (if (> mantwidth 0)
                  (setq mantwidth (- mantwidth 1))
                  (setq leadingpoint nil))))
            ;; mantwidth characters remain.
            (if (and overflowchar w (minusp mantwidth))
              (format-padding w overflowchar stream) ; not enough room -> overflow
              (progn
                (when (and w (> mantwidth 0))
                  (format-padding mantwidth padchar stream))
                (if (minusp arg)
                  (write-char #\- stream)
                  (if plus-sign-flag (write-char #\+ stream)))
                (if leadingpoint (write-char #\0 stream))
                (write-string mantdigits stream)
                (if trailingpoint (write-char #\0 stream))
                (write-char
                  (cond (exponentchar)
                        ((and (not *PRINT-READABLY*)
                              (typep arg *READ-DEFAULT-FLOAT-FORMAT*))
                         #\E)
                        ((short-float-p arg) #\s)
                        ((single-float-p arg) #\f)
                        ((double-float-p arg) #\d)
                        ((long-float-p arg) #\L))
                  stream)
                (write-char (if (minusp exponent) #\- #\+) stream)
                (when (and e (> e (length expdigits)))
                  (format-padding (- e (length expdigits)) #\0 stream))
                (write-string expdigits stream)))))))))

;; advances *FORMAT-CSDL* until the end of the current ~[ resp. ~{ resp. ~< .
(defun format-skip-to-end ()
  (do ()
      ((null (csd-clause-chain (car *FORMAT-CSDL*))))
    (setq *FORMAT-CSDL* (csd-clause-chain (car *FORMAT-CSDL*)))))

;; (format-justified-segments mincol colinc minpad justify-left justify-right
;;                            piecelist)
;; calculates the positions and the number of spaces between
;; the various Strings in piecelist.
;; Between the various Strings in piecelist (also ahead, if justify-left;
;; also behind, if justify-right) at least minpad padding-characters
;; are inserted. Then, further padding-characters are added, for the
;; total width to become >= mincol. If width > mincol, more
;; padding-characters are added until the width is of the form
;; mincol + k * colinc.
;; These padding-characters are distributed to the various places evenly.
;; 1st value: A vector, that specifies for each position, how many
;;            padding-characters have to be inserted (NIL = none).
;;            First Element: leftmost, Second Element: behind the 1st String,
;;            ..., last: rightmost.
;; 2. value: The resulting total width.
(defun format-justified-segments
       (mincol colinc minpad justify-left justify-right piecelist)
  (declare (fixnum mincol colinc minpad))
  (let ((piecesnumber 0)
        (pieceswidth 0))
    (dolist (piece piecelist)
      (declare (simple-string piece))
      (incf piecesnumber)
      (incf pieceswidth (string-width piece)))
    (let* ((new-justify-left
             (or justify-left (and (= piecesnumber 1) (not justify-right))))
           (padblocks (+ piecesnumber -1       ; number of insertion-points
                         (if new-justify-left 1 0) (if justify-right 1 0)))
           (width-need (+ pieceswidth (* padblocks minpad)))
           (width (+ mincol
                     (if (<= width-need mincol)
                       0
                       (* (ceiling (- width-need mincol) colinc) colinc)))))
      (declare (fixnum piecesnumber pieceswidth padblocks width-need width))
      (multiple-value-bind (padwidth rest)
          (floor (- width pieceswidth) padblocks)
        (let ((padblock-lengths
                (make-array (1+ piecesnumber) :initial-element padwidth)))
          (unless new-justify-left (setf (svref padblock-lengths 0) nil))
          (unless justify-right
            (setf (svref padblock-lengths piecesnumber) nil))
          (do ((i 0 (1+ i)))
              ((zerop rest))
            (when (svref padblock-lengths i)
              (incf (svref padblock-lengths i))
              (decf rest)))
          (values padblock-lengths width))))))

;;; ---------------------------------------------------------------------------

;; ~A, CLTL p.387-388, CLtL2 p. 584, ABI
(defformat-simple format-ascii (stream colon-modifier atsign-modifier
                  (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
                  (arg)
  (when (and colon-modifier (null arg)) (setq arg "()"))
  (if (and (zerop mincol) (zerop minpad))
    (princ arg stream)
    (format-padded-string mincol colinc minpad padchar
      atsign-modifier ; =: padleftflag
      (princ-to-string arg)
      stream)))

;; preliminary, see fill-out.lisp
(defun stream-start-s-expression (stream)
  (declare (ignore stream)) *print-right-margin*)
(defun stream-end-s-expression (stream) (declare (ignore stream)))

;; ~S, CLTL p.388, CLtL2 p. 584, ABI
(defformat-simple format-s-expression (stream colon-modifier atsign-modifier
                  (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
                  (arg)
  (let ((*print-right-margin* (stream-start-s-expression stream)))
    (if (and (zerop mincol) (zerop minpad))
      (if (and colon-modifier (null arg))
        (write-string #1="()" stream)
        (prin1 arg stream))
      (format-padded-string mincol colinc minpad padchar
        atsign-modifier ; =: padleftflag
        (if (and colon-modifier (null arg)) #1# (prin1-to-string arg))
        stream)))
  (stream-end-s-expression stream))

;; ~W, ABI
(defformat-simple format-write (stream colon-modifier atsign-modifier
                  (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
                  (arg)
  (declare (ignore colon-modifier))
  (if (and (zerop mincol) (zerop minpad))
    (write arg :stream stream)
    (format-padded-string mincol colinc minpad padchar
      atsign-modifier ; =: padleftflag
      (write-to-string arg)
      stream)))

;; ~D, CLTL p.388, CLtL2 p. 585, ABI
(defformat-simple format-decimal (stream colon-modifier atsign-modifier
                  (mincol 0) (padchar #\Space) (commachar #\,)
                  (commainterval 3))
                  (arg)
  (format-base 10 stream colon-modifier atsign-modifier mincol
               padchar commachar commainterval arg))

;; ~B, CLTL p.388, CLtL2 p. 585, ABI
(defformat-simple format-binary (stream colon-modifier atsign-modifier
                  (mincol 0) (padchar #\Space) (commachar #\,)
                  (commainterval 3))
                  (arg)
  (format-base 2 stream colon-modifier atsign-modifier mincol
               padchar commachar commainterval arg))

;; ~O, CLTL p.388, CLtL2 p. 585, ABI
(defformat-simple format-octal (stream colon-modifier atsign-modifier
                  (mincol 0) (padchar #\Space) (commachar #\,)
                  (commainterval 3))
                  (arg)
  (format-base 8 stream colon-modifier atsign-modifier mincol
               padchar commachar commainterval arg))

;; ~X, CLTL p.388-389, CLtL2 p. 586, ABI
(defformat-simple format-hexadecimal (stream colon-modifier atsign-modifier
                  (mincol 0) (padchar #\Space) (commachar #\,)
                  (commainterval 3))
                  (arg)
  (format-base 16 stream colon-modifier atsign-modifier mincol
               padchar commachar commainterval arg))

;; ~R, CLTL p.389, CLtL2 p. 586-587, ABI
(defformat-simple format-radix (stream colon-modifier atsign-modifier
                  (radix nil) (mincol 0) (padchar #\Space) (commachar #\,)
                  (commainterval 3))
                  (arg)
  (if radix
    (format-integer radix mincol padchar commachar commainterval
                    colon-modifier atsign-modifier
                    arg stream)
    (if atsign-modifier
      (if (integerp arg)
        (if colon-modifier
          (format-old-roman arg stream)
          (format-new-roman arg stream))
        (format-error 'type-error :datum arg :expected-type 'INTEGER
          *FORMAT-CS* nil
          (TEXT "The ~~R and ~~:R format directives require an integer argument, not ~S")
          arg))
      (if colon-modifier
        (format-ordinal arg stream)
        (format-cardinal arg stream)))))

;; ~P, CLTL p. 389, CLtL2 p. 587-588, ABI
(defun format-plural (stream colon-modifier atsign-modifier)
  (when colon-modifier (format-goto-new-arg t 1))
  (let ((singular (eql (next-arg) 1)))
    (if atsign-modifier
      (write-string (if singular "y" "ies") stream)
      (unless singular (write-char #\s stream)))))

;; ~C, CLTL p.389-390, CLtL2 p. 588, ABI
(defformat-simple format-character (stream colon-modifier atsign-modifier)
                  (arg)
  (unless (characterp arg)
    (format-error 'type-error :datum arg :expected-type 'CHARACTER
      *FORMAT-CS* nil
      (TEXT "The ~~C format directive requires a character argument, not ~S")
      arg))
  (if (not colon-modifier)
    (if (not atsign-modifier)
      ;; ~C
      (write-char arg stream)
      ;; ~@C
      (prin1 arg stream))
    ;; ~:C prints the name of non-printing characters.
    ;; ~:@C prints instructions how to type the character.
    ;; Since characters don't have attributes any more, both do the same.
    (if (and (graphic-char-p arg) (not (eql arg #\Space)))
      (write-char arg stream)
      (let ((name (char-name arg)))
        (if name
          (write-string name stream)
          (write-char arg stream))))))

;; ~F, CLTL p.390-392, CLtL2 p. 588-590, ABI
(defformat-simple format-fixed-float (stream colon-modifier atsign-modifier
                  (w nil) (d nil) (k 0) (overflowchar nil) (padchar #\Space))
                  (arg)
  (declare (ignore colon-modifier))
  (when (rationalp arg) (setq arg (float arg)))
  (if (floatp arg)
    (format-float-for-f w d k overflowchar padchar atsign-modifier arg stream)
    (if w
      (format-padded-string w 1 0 padchar t (princ-to-string arg) stream)
      (format-ascii-decimal arg stream))))

;; ~E, CLTL p.392-395, CLtL2 p. 590-593, ABI
(defformat-simple format-exponential-float (stream
                  colon-modifier atsign-modifier
                  (w nil) (d nil) (e nil) (k 1)
                  (overflowchar nil) (padchar #\Space) (exponentchar nil))
                  (arg)
  (declare (ignore colon-modifier))
  (when (rationalp arg) (setq arg (float arg)))
  (if (floatp arg)
    (format-float-for-e w d e k overflowchar padchar exponentchar
                        atsign-modifier arg stream)
    (if w
      (format-padded-string w 1 0 padchar t (princ-to-string arg) stream)
      (format-ascii-decimal arg stream))))

;; ~G, CLTL p.395-396, CLtL2 p. 594-595, ABI
(defformat-simple format-general-float (stream colon-modifier atsign-modifier
                  (w nil) (d nil) (e nil) (k 1)
                  (overflowchar nil) (padchar #\Space) (exponentchar nil))
                  (arg)
  (declare (ignore colon-modifier))
  (if (rationalp arg) (setq arg (float arg)))
  (if (floatp arg)
    (multiple-value-bind (mantissa n) (format-scale-exponent (abs arg))
      (declare (ignore mantissa))
      (if (null d)
        (setq d
          (multiple-value-bind (digits digitslength)
              (format-float-to-string (abs arg) nil nil nil nil)
            (declare (ignore digits))
            (max (1- digitslength) 1 (min n 7)))))
      (let* ((ee (if e (+ 2 e) 4))
             (dd (- d n)))
        (if (<= 0 dd d)
          (progn
            (format-float-for-f
              (if w (- w ee) nil)
              dd 0
              overflowchar padchar atsign-modifier arg stream)
            (format-padding ee #\Space stream))
          (format-float-for-e w d e k overflowchar padchar exponentchar
                              atsign-modifier arg stream))))
    (if w
      (format-padded-string w 1 0 padchar t (princ-to-string arg) stream)
      (format-ascii-decimal arg stream))))

;; ~$, CLTL p.396-397, CLtL2 p. 595-596, ABI
(defformat-simple format-dollars-float (stream colon-modifier atsign-modifier
                  (d 2) (n 1) (w 0) (padchar #\Space))
                  (arg)
  (when (rationalp arg) (setq arg (float arg)))
  (if (floatp arg)
    (multiple-value-bind (digits digitslength
                          leadingpoint trailingpoint leadings)
        (format-float-to-string arg nil d 0 nil)
      (declare (ignore digitslength leadingpoint trailingpoint))
      (let* ((lefts (max leadings n))
             (totalwidth (+ (if (or atsign-modifier (minusp arg)) 1 0)
                            lefts 1 d))
             (padcount (max (- w totalwidth) 0)))
        (if (not colon-modifier) (format-padding padcount padchar stream))
        (if (minusp arg)
          (write-char #\- stream)
          (if atsign-modifier (write-char #\+ stream)))
        (if colon-modifier (format-padding padcount padchar stream))
        (format-padding (- lefts leadings) #\0 stream)
        (write-string digits stream)))
    (if d
      (format-padded-string d 1 0 padchar t (princ-to-string arg) stream)
      (format-ascii-decimal arg stream))))

;; ~%, CLTL p.397, CLtL2 p. 596, ABI
(defun format-terpri (stream colon-modifier atsign-modifier &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (dotimes (i count) (terpri stream)))

(defun format-pprint-newline (stream colon-modifier atsign-modifier)
  (pprint-newline (if colon-modifier
                    (if atsign-modifier :mandatory :fill)
                    (if atsign-modifier :miser :linear))
                  stream))

(defun format-pprint-indent (stream colon-modifier atsign-modifier
                             &optional (count 1))
  (declare (ignore atsign-modifier))
  (pprint-indent (if colon-modifier :current :block) (or count 1) stream))

;; ~&, CLTL p.397, CLtL2 p. 596, ABI
(defun format-fresh-line (stream colon-modifier atsign-modifier
                          &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (when (plusp count)
    (fresh-line stream)
    (dotimes (i (1- count)) (terpri stream))))

;; ~.
(defun format-elastic-newline (stream colon-modifier atsign-modifier
                               &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (when (plusp count)
    (dotimes (i (1- count)) (terpri stream))
    (ext:elastic-newline stream)))

;; ~|, CLTL p.397, CLtL2 p. 596, ABI
(defun format-page (stream colon-modifier atsign-modifier &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (dotimes (i count) (write-char #\Page stream)))

;; ~~, CLTL p.397, CLtL2 p. 596, ABI
(defun format-tilde (stream colon-modifier atsign-modifier &optional (count 1))
  (declare (ignore colon-modifier atsign-modifier))
  (if (null count) (setq count 1))
  (dotimes (i count) (write-char #\~ stream)))

#| have to support PPHELP streams separately, so this is in io.d now
;; ~T, CLTL p.398-399, CLtL2 p. 597-598, ABI
 (defun format-tabulate (stream colon-modifier atsign-modifier
                        &optional (colnum 1) (colinc 1))
  (if (null colnum) (setq colnum 1))
  (if (null colinc) (setq colinc 1))
  (let* ((new-colnum (+ (max colnum 0)
                        (if (and colon-modifier (boundp '*prin-indentation*))
                          *prin-indentation*
                          0)))
         (new-colinc (max colinc 1)) ; >0
         (pos (sys::line-position stream))) ; actual position, fixnum>=0 or NIL
    (if atsign-modifier
      (format-padding
        (if pos
          (+ new-colnum (mod (- (+ pos new-colnum)) new-colinc))
          new-colnum)
        #\Space stream)
      (if pos
        (if (< pos new-colnum)
          (format-padding (- new-colnum pos) #\Space stream)
          (unless (zerop colinc)
            (format-padding (+ colinc (mod (- new-colnum pos) (- colinc)))
                            #\Space stream)))
        (format-padding 2 #\Space stream)))))
|#

;; ~*, CLTL p.399, CLtL2 p. 598
(defun format-goto (stream colon-modifier atsign-modifier
                    &optional (index nil))
  (declare (ignore stream))
  (if atsign-modifier
    (setq *FORMAT-NEXT-ARG* (nthcdr (or index 0) *FORMAT-ARG-LIST*))
    (format-goto-new-arg colon-modifier (or index 1))))

;; ~?, CLTL p.399-401, CLtL2 p. 598-599
(defun format-indirection (stream colon-modifier atsign-modifier)
  (declare (ignore colon-modifier))
  (let* ((csarg (next-arg))
         (node (do-format-indirection-1 csarg)))
    (if atsign-modifier
      (if (consp node)
        (let ((*FORMAT-CS* (car node))
              (*FORMAT-CSDL* (cdr node))
             ;(*FORMAT-ARG-LIST* *FORMAT-NEXT-ARG*) ; ??
              (*FORMAT-UP-AND-OUT* nil))
          (format-interpret stream))
        (setq *FORMAT-NEXT-ARG*
          (let ((*FORMAT-CS* nil))
            (apply node stream *FORMAT-NEXT-ARG*))))
      (let ((arglistarg (next-arg)))
        (do-format-indirection-2 stream node arglistarg arglistarg)))))
(defun do-format-indirection (stream csarg arguments) ; ABI
  (unless (or (stringp csarg) (functionp csarg))
    (format-indirection-cserror csarg))
  (unless (listp arguments) (format-indirection-lerror arguments))
  (format-apply stream csarg arguments))
(defun do-format-indirection-1 (csarg) ; ABI
  (cond ((stringp csarg)
         (let ((node (list csarg)))
           (format-parse-cs csarg 0 node nil)
           node))
        ((functionp csarg)
         csarg)
        (t (format-indirection-cserror csarg))))
(defun do-format-indirection-2 (stream node arglistarg wholelistarg) ; ABI
  (unless (listp arglistarg) (format-indirection-lerror arglistarg))
  (if (consp node)
    (let* ((*FORMAT-CS*         (car node))
           (*FORMAT-CSDL*       (cdr node))
           (*FORMAT-ARG-LIST*   wholelistarg)
           (*FORMAT-NEXT-ARG*   arglistarg)
           (*FORMAT-NEXT-ARGLIST* nil)
           (*FORMAT-UP-AND-OUT* nil))
      (format-interpret stream)
      *FORMAT-NEXT-ARG*)
    (let ((*FORMAT-CS* nil))
      (apply node stream arglistarg)))) ; wholelistarg??
(defun format-indirection-cserror (csarg)
  (format-error 'type-error :datum csarg :expected-type '(OR STRING FUNCTION)
    *FORMAT-CS* nil
    (TEXT "The control string argument for the ~~? format directive is invalid: ~S")
    csarg))
(defun format-indirection-lerror (arguments)
  (format-error 'type-error :datum arguments :expected-type 'LIST
    *FORMAT-CS* nil
    (TEXT "The argument list argument for the ~~? format directive is invalid: ~S")
    arguments))

;;; ~// ANSI CL 22.3.5.4 Tilde Slash: Call Function
(defun FORMAT-CALL-USER-FUNCTION (stream colon-p atsign-p symbol-list
                                  &rest more-args)
  (apply (car symbol-list) stream (next-arg) colon-p atsign-p more-args))

;; ~(, CLTL p.401, CLtL2 p. 600-601
(defun format-case-conversion (stream colon-modifier atsign-modifier)
  (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
  (let ((tempstr
          (let ((tempstream (make-string-output-stream
                              :line-position (sys::line-position stream))))
            (format-interpret tempstream 'FORMAT-CASE-CONVERSION-END)
            ;; what does UP-AND-OUT effectuate in ~{...~(...~^...~)...~} ??
            (get-output-stream-string tempstream))))
    (if colon-modifier
      (if atsign-modifier
        (write-string (nstring-upcase tempstr) stream)
        (write-string (nstring-capitalize tempstr) stream))
      (if atsign-modifier
        (write-string (nstring-capitalize1 tempstr) stream)
        (write-string (nstring-downcase tempstr) stream)))))
(defun nstring-capitalize1 (string) ; ABI
  (setq string (nstring-downcase string))
  (dotimes (i (length string)) ; make first character Upcase
    (when (both-case-p (schar string i))
      (setf (schar string i) (char-upcase (schar string i)))
      (return)))
  string)

;; ~[, CLTL p.402-403, CLtL2 p. 601-602
(defun format-conditional (stream colon-modifier atsign-modifier
                           &optional (prefix nil))
  (if colon-modifier
    (if atsign-modifier
      (format-conditional-error)
      (progn
        (when (next-arg)
          (setq *FORMAT-CSDL* (csd-clause-chain (car *FORMAT-CSDL*))))
        (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
        (format-interpret stream 'FORMAT-CONDITIONAL-END)))
    (if atsign-modifier
      (when (next-arg)
        (format-goto-new-arg t 1)
        (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
        (format-interpret stream 'FORMAT-CONDITIONAL-END)
        (unless (null (csd-clause-chain (car *FORMAT-CSDL*)))
          (format-error 'error *FORMAT-CS* nil
            (TEXT "The ~~; format directive is not allowed at this point."))))
      (let ((index (or prefix (next-arg))))
        (unless (integerp index)
          (format-error 'type-error :datum index :expected-type 'INTEGER
            *FORMAT-CS* nil
            (TEXT "The ~~[ parameter must be an integer, not ~S")
            index))
        (dotimes (i (if (minusp index) most-positive-fixnum index))
          (when (eq (csd-data (car *FORMAT-CSDL*)) 'FORMAT-CONDITIONAL-END)
            (return))
          (setq *FORMAT-CSDL* (csd-clause-chain (car *FORMAT-CSDL*)))
          (when (csd-colon-p (car *FORMAT-CSDL*)) (return)))
        (unless (eq (csd-data (car *FORMAT-CSDL*)) 'FORMAT-CONDITIONAL-END)
          (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*)))
        (format-interpret stream 'FORMAT-CONDITIONAL-END))))
  (format-skip-to-end)) ; skip to the end of ~[...~]-Directive

(defun format-conditional-error ()
  (format-error 'error *FORMAT-CS* nil
    (TEXT "The ~~[ format directive cannot take both modifiers.")))

; ~{, CLTL p.403-404, CLtL2 p. 602-604
(defun format-iteration (stream colon-modifier atsign-modifier
                         &optional (prefix nil))
  (let* ((total-csdl *FORMAT-CSDL*)
         (max-iteration-count prefix))
    (format-skip-to-end) ; skip to the end of ~{...~}-Directive
    (let* ((min-1-iteration (csd-colon-p (car *FORMAT-CSDL*)))
           (inner-cs (if (eq (cdr total-csdl) *FORMAT-CSDL*)
                       (next-arg)
                       *FORMAT-CS*))
           (inner-csdl (if (stringp inner-cs)
                         (if (eq (cdr total-csdl) *FORMAT-CSDL*)
                           (let ((node (list inner-cs)))
                             (format-parse-cs inner-cs 0 node nil)
                             (cdr node))
                           (cdr total-csdl))))
           (arg-list-rest (if (not atsign-modifier)
                            (let ((arg (next-arg)))
                              (unless (listp arg)
                                (format-error 'type-error :datum arg :expected-type 'LIST
                                  *FORMAT-CS* nil
                                  (TEXT "The ~~{ format directive requires a list argument, not ~S")
                                  arg))
                              arg))))
      (do* ((iteration-count 0 (1+ iteration-count)))
           ((or (and max-iteration-count
                     (>= iteration-count max-iteration-count))
                (let ((remaining (if atsign-modifier
                                   *FORMAT-NEXT-ARG*
                                   arg-list-rest)))
                  (if min-1-iteration
                    (and (plusp iteration-count) (null remaining))
                    (null remaining)))))
        (if (stringp inner-cs)
          (if colon-modifier
            (let* ((*FORMAT-ARG-LIST*
                     (if atsign-modifier (next-arg) (pop arg-list-rest)))
                   (*FORMAT-NEXT-ARGLIST* ; for ~:^
                     (if atsign-modifier *FORMAT-NEXT-ARG* arg-list-rest))
                   (*FORMAT-NEXT-ARG* *FORMAT-ARG-LIST*)
                   (*FORMAT-CS* inner-cs)
                   (*FORMAT-CSDL* inner-csdl)
                   (*FORMAT-UP-AND-OUT* nil))
              (format-interpret stream 'FORMAT-ITERATION-END)
              (when (eq *FORMAT-UP-AND-OUT* ':TERMINATE-ALL) (return))
            )
            (if atsign-modifier
              ;; CLtL2 p. 598: "When within a ~{ construct, the "goto" is
              ;; relative to the list of arguments being processed by the
              ;; iteration." Does that mean, that for ~@{, *FORMAT-ARG-LIST*
              ;; has to be freshly bound at the beginning of each Iteration??
              ;; (*FORMAT-ARG-LIST* *FORMAT-NEXT-ARG*) ??
              (let* ((*FORMAT-CS* inner-cs)
                     (*FORMAT-CSDL* inner-csdl)
                     (*FORMAT-UP-AND-OUT* nil))
                (format-interpret stream 'FORMAT-ITERATION-END)
                (when *FORMAT-UP-AND-OUT* (return)))
              (let* ((*FORMAT-ARG-LIST* arg-list-rest)
                     (*FORMAT-NEXT-ARG* *FORMAT-ARG-LIST*)
                     (*FORMAT-CS* inner-cs)
                     (*FORMAT-CSDL* inner-csdl)
                     (*FORMAT-UP-AND-OUT* nil))
                (format-interpret stream 'FORMAT-ITERATION-END)
                (setq arg-list-rest *FORMAT-NEXT-ARG*)
                (when *FORMAT-UP-AND-OUT* (return)))))
          ;; inner-cs may be a function in the ~{~} case
          (if (functionp inner-cs)
            (if colon-modifier
              (let* ((arglist
                       (if atsign-modifier (next-arg) (pop arg-list-rest)))
                     (*FORMAT-CS* nil))
                (apply inner-cs stream arglist))
              (if atsign-modifier
                (setq *FORMAT-NEXT-ARG*
                  (let ((*FORMAT-CS* nil))
                    (apply inner-cs stream *FORMAT-NEXT-ARG*)))
                (setq arg-list-rest
                  (let ((*FORMAT-CS* nil))
                    (apply inner-cs stream arg-list-rest)))))
            (format-indirection-cserror inner-cs)))))))

;; ~<, CLTL p.404-406, CLtL2 p. 604-605
(defun format-justification (stream colon-modifier atsign-modifier
       &optional (mincol 0) (colinc 1) (minpad 0) (padchar #\Space))
  (let* ((saved-csdl *FORMAT-CSDL*)
         (pos (sys::line-position stream))
         (tempstream (make-string-output-stream :line-position pos))
         (check-on-line-overflow nil)
         supplementary-need
         line-length
         (old-piecelist
           (let ((pieces nil))
             (do ((first-piece-flag t nil))
                 ((eq (csd-data (car *FORMAT-CSDL*))
                      'FORMAT-JUSTIFICATION-END))
               (setq *FORMAT-CSDL* (cdr *FORMAT-CSDL*))
               (let ((*FORMAT-UP-AND-OUT* nil))
                 (format-interpret tempstream 'FORMAT-JUSTIFICATION-END)
                 (when (and first-piece-flag
                            (eq (csd-data (car *FORMAT-CSDL*))
                                'FORMAT-SEPARATOR))
                   (when (setq check-on-line-overflow
                               (csd-colon-p (car *FORMAT-CSDL*)))
                     (multiple-value-setq (supplementary-need line-length)
                       (values-list
                         (format-resolve-parms (car *FORMAT-CSDL*))))))
                 (when *FORMAT-UP-AND-OUT*
                   (setq *FORMAT-CSDL* saved-csdl)
                   (format-skip-to-end)
                   (return))
                 (push (get-output-stream-string tempstream) pieces)))
             (nreverse pieces))))
    (do-format-justification stream colon-modifier atsign-modifier
                             mincol colinc minpad padchar
                             pos check-on-line-overflow
                             (if check-on-line-overflow (car old-piecelist))
                             supplementary-need line-length
                             (if check-on-line-overflow
                               (cdr old-piecelist)
                               old-piecelist))))
(defun do-format-justification (stream colon-modifier atsign-modifier
                                mincol colinc minpad padchar
                                pos check-on-line-overflow firstpiece
                                supplementary-need line-length piecelist) ; ABI
  (if (null mincol) (setq mincol 0))
  (if (null colinc) (setq colinc 1))
  (if (null minpad) (setq minpad 0))
  (if (null padchar) (setq padchar #\Space))
  (if piecelist
    (multiple-value-bind (padblocklengths width)
      (format-justified-segments mincol colinc minpad
        colon-modifier atsign-modifier piecelist)
      (when (and check-on-line-overflow
                 (> (+ (or pos 0) width (or supplementary-need 0))
                    (or line-length #|(sys::line-length stream)|# 72)))
        (write-string firstpiece stream))
      (do ((i 0 (1+ i)))
          (nil)
        (when (svref padblocklengths i)
          (format-padding (svref padblocklengths i) padchar stream))
        (when (null piecelist) (return))
        (write-string (pop piecelist) stream)))
    (format-padding mincol padchar stream)))

;; CLtL2 p. 762-763
(defun format-logical-block (stream colon-modifier atsign-modifier)
  ;; (format-error 'error *FORMAT-CS* nil (TEXT "~~<...~~:> not implemented yet"))
  (format-justification stream colon-modifier atsign-modifier))

;; parse the CSDL and return the following values
;; prefix string (or nil)
;; suffix string (or nil)
;; per-line-p non-nil or nil
;; body-csdl csdl
;; add-fill-p t or nil
;; last-csdl csdl
(defun format-logical-block-parse (csdl)
  (let (prefix suffix per-line-p body-csdl add-fill-p temp)
    (when (csd-colon-p (car csdl))
      (setq prefix "(")
      (setq suffix ")"))
    (setq temp (car (csd-clause-chain (car csdl))))
    (unless (eq (csd-data temp) 'FORMAT-JUSTIFICATION-END)
      (pop csdl)
      (setq per-line-p (csd-atsign-p temp))
      (cond ((eq (car csdl) temp)
             (setq prefix ""))
            ((and (eq (car (cdr csdl)) temp)
                  (eql (csd-type (car csdl)) 1))
             (setq prefix (subseq *FORMAT-CS* (csd-cs-index (car csdl))
                                  (csd-data (car csdl))))
             (pop csdl))
            (t (format-error 'error *FORMAT-CS* (csd-cs-index (car csdl))
                 (TEXT "Prefix for logical block must be constant")))))
    (setq body-csdl (cdr csdl))
    (setq temp (csd-clause-chain (car csdl)))
    (unless (eq (csd-data (car temp)) 'FORMAT-JUSTIFICATION-END)
      (pop temp)
      (cond ((eql (csd-type (car temp)) 1)
             (setq suffix (subseq *FORMAT-CS* (csd-cs-index (car temp))
                                              (csd-data (car temp))))
             (pop temp))))
    (unless (and (eql (csd-type (car temp)) 2)
                 (eq (csd-data (car temp)) 'FORMAT-JUSTIFICATION-END))
      (format-error 'error *FORMAT-CS* (csd-cs-index (car temp))
        (TEXT "Logical block suffix must be constant")))
    (setq add-fill-p (csd-atsign-p (car temp)))
    (values prefix suffix per-line-p body-csdl add-fill-p temp)))

;; ~^, CLTL p.406-407, CLtL2 p. 605-606
(defun format-up-and-out (stream colon-modifier atsign-modifier
                          &optional (a nil) (b nil) (c nil))
  (declare (ignore stream atsign-modifier))
  (if (up-and-out-p a b c
        (if colon-modifier *FORMAT-NEXT-ARGLIST* *FORMAT-NEXT-ARG*))
    (setq *FORMAT-UP-AND-OUT* (if colon-modifier ':TERMINATE-ALL ':TERMINATE))))
(defun up-and-out-p (a b c &optional args) ; ABI
  (cond ((and (null a) (null b) (null c)) ; no parameters
         (null args))
        ((and (null b) (null c)) (eql a 0)) ; one parameter
        ((null c) (eql a b)) ; two parameters
        ((and (integerp a) (integerp b) (integerp c)) (<= a b c))
        ((and (characterp a) (characterp b) (characterp c)) (char<= a b c))))

;; ~!, a CLISP extension as a replacement for badly designed ~/.../
(defun format-call (stream colon-modifier atsign-modifier &rest more-args)
  (apply (next-arg) stream (next-arg) colon-modifier atsign-modifier
         more-args))

;;; ---------------------------------------------------------------------------
;;; FORMATTER - Compilation of FORMAT-Strings.


;; Fall-back function if control-string cannot be compiled.
(defun formatter-hairy (control-string) ; ABI
  ;; control-string is known to be a string
  #'(lambda (stream &rest args)
      (let ((node (list control-string)))
        (format-parse-cs control-string 0 node nil)
        (let* ((*FORMAT-CS*         (car node))
               (*FORMAT-CSDL*       (cdr node))
               (*FORMAT-ARG-LIST*   args)
               (*FORMAT-NEXT-ARG*   *FORMAT-ARG-LIST*)
               (*FORMAT-NEXT-ARGLIST* nil)
               (*FORMAT-UP-AND-OUT* nil))
          (format-interpret stream)
          *FORMAT-NEXT-ARG*))))


;; Block for ~^
(defvar *format-terminate*)
;; Block for ~:^
(defvar *format-terminate-all*)

;; the block is only provided on demand.
;; In order to avoid unnecessary UNWIND-PROTECTs, a list of the pending
;; UNWIND-PROTECTs is maintained.
;; Each block-name (a Gensym) contains a reference
;; to this list at the moment of its creation.

;; list of pending UNWIND-PROTECTs
(defvar *format-uwps*)

(defun formatter-block (prefix)
  (let ((sym (gensym prefix)))
    (setf (get sym 'uwps) *format-uwps*)
    sym))

(flet ((mark-used (blockname)
         ;; mark the block for not being optimized away.
         (setf (get blockname 'used) t)
         ;; mark all skipped UNWIND-PROTECTs for not being
         ;; optimized away.
         (do ((L1 *format-uwps* (cdr L1))
              (L2 (get blockname 'uwps)))
             ((eq L1 L2))
           (setf (car L1) 'T))
         blockname))
  (defun formatter-terminate ()
    (mark-used *format-terminate*))
  (defun formatter-terminate-all ()
    (mark-used *format-terminate-all*)))

(defmacro formatter-bind-terminator (&body body)
  `(let ((*format-terminate* (formatter-block "TERMINATE-")))
     (formatter-bind-terminator-1 (progn ,@body))))
(defun formatter-bind-terminator-1 (forms)
  (when (get *format-terminate* 'used)
    (setq forms `((BLOCK ,*format-terminate* ,@forms))))
  forms)

(defmacro formatter-bind-terminators (&body body)
  `(let ((*format-terminate* (formatter-block "TERMINATE-"))
         (*format-terminate-all* (formatter-block "TERMINATE-ALL-")))
     (formatter-bind-terminators-1 (progn ,@body))))
(defun formatter-bind-terminators-1 (forms)
  (when (get *format-terminate* 'used)
    (setq forms `((BLOCK ,*format-terminate* ,@forms))))
  (when (get *format-terminate-all* 'used)
    (setq forms `((BLOCK ,*format-terminate-all* ,@forms))))
  forms)


;; Flag, if within ~(...~)
(defvar *format-case*)

;; the argument-list cannot always have the same name ARGS because of ~:^ .
;; your name.
(defvar *args*)

;; name of the argument-list of the enclosing ~:{ Iteration.
(defvar *iterargs*)

;; access to the normal argument-list:
;; normal case:
;;   argument-list &REST ARGS,
;;   access to the next element is (POP ARGS),
;;   ~# is (LENGTH ARGS),
;;   total list for ~:* is WHOLE-ARGS.
;; optimized, if no (LENGTH ARGS) and no WHOLE-ARGS is necessary:
;;   argument-list #:ARG1 #:ARG2 ... &REST ARGS
;;   access to the next element is #:ARGi or (POP ARGS).

;; flag, that indicates, if we are still in the linear processing phase of
;; the arguments (each one exactly once, known position).
(defvar *formatter-linear-args*)

;; number of arguments, that belong to the linear processing phase, so far.
;; Important: It can be decreased afterwards!!
(defvar *formatter-linear-argcount*)

;; position in the argument-list during the linear processing phase.
;; always <= *formatter-linear-argcount*.
(defvar *formatter-linear-position*)

;; flag, if WHOLE-ARGS is to be bound.
(defvar *formatter-whole-args*)

;; starts an iteration, that binds ARGS and possibly WHOLE-ARGS.
(defmacro formatter-bind-args (&body body)
  `(let ((*args* (gensym "ARGS"))
         (*formatter-linear-args* t)
         (*formatter-linear-argcount* 0)
         (*formatter-linear-position* 0)
         (*formatter-whole-args* nil))
     (formatter-bind-args-1 (progn ,@body))))
(defun formatter-bind-args-1 (forms)
  (when *formatter-whole-args*
    (subst-if-then #'(lambda (x) ; x = `(WHOLE-ARGS ,i)
                       (setq *formatter-linear-argcount*
                             (min *formatter-linear-argcount* (second x))))
                   #'(lambda (x) ; x = `(WHOLE-ARGS ,i) ?
                       (and (consp x) (eq (car x) 'WHOLE-ARGS)
                            (consp (cdr x)) (numberp (cadr x)) (null (cddr x))))
                   forms))
  (let ((argsyms nil))
    (dotimes (i *formatter-linear-argcount*) (push (gensym "ARG") argsyms))
    (setq argsyms (nreverse argsyms))
    (setq forms
      (subst-if-then #'(lambda (x) ; x = `(ARG ,i)
                         (if (< (second x) *formatter-linear-argcount*)
                           (nth (second x) argsyms)
                           `(POP ,*args*)))
                     #'(lambda (x) ; x = `(ARG ,i) ?
                         (and (consp x) (eq (car x) 'ARG) (consp (cdr x)) (null (cddr x))))
                     forms))
    (setq forms
      (subst-if-then #'(lambda (x) ; x = `(SETQ-ARGS-WHOLE-ARGS ,old-pos ,new-pos)
                         (let ((old-pos (second x)) (new-pos (third x)))
                           (if (<= old-pos *formatter-linear-argcount*)
                             ; no need for WHOLE-ARGS since ARGS = WHOLE-ARGS at this point
                             (if (<= new-pos *formatter-linear-argcount*)
                               `(PROGN)
                               `(SETQ ,*args* (NTHCDR ,(- new-pos *formatter-linear-argcount*) ,*args*)))
                             (progn
                               (setq *formatter-whole-args* t)
                               `(SETQ ,*args* (WHOLE-ARGS ,(max new-pos *formatter-linear-argcount*)))))))
                     #'(lambda (x) ; x = `(SETQ-ARGS-WHOLE-ARGS ,i ,j) ?
                         (and (consp x) (eq (car x) 'SETQ-ARGS-WHOLE-ARGS)
                              (consp (cdr x)) (consp (cddr x)) (null (cdddr x))))
                     forms))
    (when *formatter-whole-args*
      (setq forms
        (subst-if-then #'(lambda (x) ; x = `(WHOLE-ARGS ,i)
                           (let ((i (- (second x)
                                       *formatter-linear-argcount*)))
                             (if (zerop i)
                               `WHOLE-ARGS
                               `(NTHCDR ,i WHOLE-ARGS))))
                       #'(lambda (x) ; x = `(WHOLE-ARGS ,i) ?
                           (and (consp x) (eq (car x) 'WHOLE-ARGS)
                                (consp (cdr x)) (numberp (cadr x)) (null (cddr x))))
                       forms))
      (setq forms `((LET ((WHOLE-ARGS ,*args*)) ,@forms))))
    (values `(,@argsyms &REST ,*args*)
            `((DECLARE (IGNORABLE ,@argsyms ,*args*)) ,@forms))))

(defmacro formatter-bind-block (&body body)
  `(let ((*args* (gensym "ARGS")) ; not used inside the pprint-logical-block
         (*format-terminate* (formatter-block "TERMINATE-"))
         (*formatter-linear-args* nil)
         (*formatter-whole-args* nil))
     (formatter-bind-block-1 (progn ,@body))))
(defun formatter-bind-block-1 (forms)
  ;; inside the pprint-logical-block, we use the "secret"
  ;; variable for accessing the list as *args*
  ;; rather than try to find and fix all the places
  ;; that use *args* in various ways.
  (setq forms
        (subst-if-then #'(lambda (x) ; x = `(POP OBJ)
                           (declare (ignore x))
                           `(PPRINT-POP))
                       #'(lambda (x) ; x = `(POP OBJ)
                           (and (consp x) (eq (car x) 'POP)
                                (consp (cdr x)) (eq 'OBJ (cadr x))
                                (null (cddr x))))
                       forms))
  (when *formatter-whole-args*
    (setq forms
          (subst-if-then #'(lambda (x) ; x = `(WHOLE-ARGS ,i)
                             `(NTHCDR ,(second x) WHOLE-ARGS))
                         #'(lambda (x) ; x = `(WHOLE-ARGS ,i) ?
                             (and (consp x) (eq (car x) 'WHOLE-ARGS)
                                  (consp (cdr x)) (numberp (cadr x))
                                  (null (cddr x))))
                         forms)))
  (setq forms
        (subst-if-then #'(lambda (x) ; x = `(IF (ENDP OBJ)
                                     ;        (RETURN-FROM ,*format-terminate*))
                           (declare (ignore x))
                           `(PPRINT-EXIT-IF-LIST-EXHAUSTED))
                       #'(lambda (x) ; x = `(IF (ENDP OBJ) (RETURN-FROM ,*format-terminate*))
                           (and (consp x)
                                (eq (car x) 'IF)
                                (consp (cdr x)) (consp (cadr x))
                                (eq (caadr x) 'ENDP) (consp (cdadr x))
                                (eq 'OBJ (cadadr x)) (null (cddadr x))
                                (consp (cddr x)) (consp (caddr x))
                                (eq (caaddr x) 'RETURN-FROM)
                                (eq (car (cdaddr x)) *format-terminate*)
                                (null (cdr (cdaddr x)))
                                (null (cdddr x))))
                       forms))
  ;; the terminate won't be used (I think....)
  (values `(,*args* ,@(if *formatter-whole-args*
                        `(&aux (WHOLE-ARGS ,*args*))))
          `((DECLARE (IGNORABLE ,*args*)) ,@forms)))

;; terminates the linear mode.
;; Hence the argument-list can be accessed as ARGS.
(defun formatter-stop-linear ()
  (when *formatter-linear-args*
    (setq *formatter-linear-argcount*
          (min *formatter-linear-argcount* *formatter-linear-position*))
    ;; Now *formatter-linear-argcount* = *formatter-linear-position*.
    (setq *formatter-linear-args* nil)))

;; Fetches a Form, that returns the length of the argument-list.
(defun formatter-length-args ()
  (formatter-stop-linear)
  `(LENGTH ,*args*))

;; Fetches a Form for the next argument.
;; This form must be substituted with SUBST afterwards.
(defun formatter-next-arg ()
  (if *formatter-linear-args*
    (prog1
      `(ARG ,*formatter-linear-position*)
      (incf *formatter-linear-position*)
      (setq *formatter-linear-argcount*
            (max *formatter-linear-argcount* *formatter-linear-position*)))
    `(POP ,*args*)))

;; Fetches a Form, that returns an nthcdr of the whole argument-list.
;; This form must be substituted with SUBST afterwards.
(defun formatter-whole-args (n)
  (formatter-stop-linear)
  (setq *formatter-whole-args* t)
  `(WHOLE-ARGS ,n))

;; Return a form to get all the rest of the remaining arguments.
(defun formatter-whole-args* ()
  (cond (*formatter-linear-args*
         (formatter-stop-linear)
         (setq *formatter-whole-args* t)
         `(WHOLE-ARGS ,*formatter-linear-position*))
        (t *args*)))

;; Fetches a Form-list for the skipping (forwards/backwards) of arguments.
(defun formatter-goto-arg (absolute-p backward-p n)
  (if absolute-p
    ;; the simplest case: (setq args (nthcdr n whole-args))
    (if (numberp n)
      (progn
        (setq n (max n 0))
        (if *formatter-linear-args*
          (if (< n *formatter-linear-position*)
            (prog1
              `((SETQ-ARGS-WHOLE-ARGS ,*formatter-linear-position* ,n))
              (setq *formatter-linear-position* n))
            ;; n >= *formatter-linear-position*
            (formatter-goto-arg nil nil (- n *formatter-linear-position*)))
          (progn
            (formatter-stop-linear)
            `((SETQ ,*args* ,(formatter-whole-args n))))))
      (let ((n `(OR ,n 0)))
        (formatter-stop-linear)
        `((SETQ ,*args* (NTHCDR ,n ,(formatter-whole-args 0))))))
    (if backward-p
      ;; the simplest case:
      ;; (setq args (nthcdr (max (- (length whole-args) (length args) n) 0) whole-args))
      (if (and (numberp n) *formatter-linear-args*)
        (formatter-goto-arg t nil (- *formatter-linear-position* n))
        (let ((n (if (numberp n) n `(OR ,n 1))))
          (formatter-stop-linear)
          `((SETQ ,*args* ,(if *formatter-linear-args*
                             `(NTHCDR (MAX (- ,*formatter-linear-position* ,n) 0) ,(formatter-whole-args 0))
                             `(LIST-BACKWARD ,n ; first evaluate n, because it can contain (POP ARGS)
                                ,(formatter-whole-args 0) ,*args*))))))
      ;; the simplest case: (setq args (nthcdr n args))
      (if (and (numberp n) (<= n 100) *formatter-linear-args*)
        (do ((l '() (cons (formatter-next-arg) l)) (i 0 (1+ i)))
            ((>= i n) (nreverse l)))
        (let ((n (if (numberp n) n `(OR ,n 1))))
          (formatter-stop-linear)
          `((SETQ ,*args* (NTHCDR ,n ,*args*))))))))
(defun list-backward (n whole-list list) ; ABI
  (nthcdr (max (- (length whole-list) (length list) n) 0) whole-list))

;; Fetches a Form, that returns a Directive-Argument.
(defun formatter-arg (arg)
  (case arg
    (:NEXT-ARG (formatter-next-arg))
    (:ARG-COUNT (formatter-length-args))
    (T     ; arg is NIL or Integer or Character, needs not to be quoted.
     arg)))

;; Main-Compilation-Function. Returns a Form-List.
;; return Fluid: *format-cs* and *format-csdl* (will be advanced).
(defun formatter-main-1 (&optional (endmarker nil))
  (let ((forms '()))
    (loop
      (when (endp *format-csdl*) (return))
      (let ((csd (car *format-csdl*)))
        (case (csd-type csd)
          (0 )
          (1 (push (subseq *format-cs* (csd-cs-index csd) (csd-data csd))
                   forms))
          (2 (let ((directive-name (csd-data csd)))
               (if (eq directive-name endmarker) (return))
               (if (eq directive-name 'FORMAT-SEPARATOR) (return))
               (let ((colon-p (csd-colon-p csd))
                     (atsign-p (csd-atsign-p csd))
                     (arglist (mapcar #'formatter-arg (csd-parm-list csd))))
                 (labels ((simple-arglist (n)
                            (unless (<= (length arglist) n)
                              (format-error 'error *format-cs* nil
                                 (TEXT "Too many arguments for this format directive")))
                            (setq arglist
                                  (append arglist
                                          (make-list (- n (length arglist))
                                                     :initial-element 'NIL))))
                          (trivial-call ()
                            (push `(,directive-name
                                    STREAM
                                    ,colon-p
                                    ,atsign-p
                                    ,@arglist)
                                   forms))
                           (trivial (n)
                             (simple-arglist n)
                             (trivial-call))
                          (simple-call ()
                            (push `(,(intern (string-concat
                                               "DO-" (string directive-name))
                                             (find-package "SYSTEM"))
                                    STREAM
                                    ,colon-p
                                    ,atsign-p
                                    ,@arglist
                                    ;; Pass the actual argument at last because
                                    ;; ,@arglist may contain `(POP ,*args*)
                                    ;; as well.
                                    ,(formatter-next-arg))
                                  forms))
                           (simple (n)
                             (simple-arglist n)
                             (simple-call)))
                   (case directive-name
                     (FORMAT-ASCII                  ; #\A
                      (simple-arglist 4)
                      (if (and (member (first arglist) '(nil 0)) ; mincol
                               (member (third arglist) '(nil 0))) ; minpad
                        (progn
                          (setq forms (revappend (remove 'NIL arglist) forms))
                          (push `(PRINC ,(if colon-p
                                           `(OR ,(formatter-next-arg) "()")
                                           (formatter-next-arg))
                                          STREAM)
                                forms))
                        (simple-call)))
                     (FORMAT-S-EXPRESSION           ; #\S
                      (simple-arglist 4)
                      (if (and (member (first arglist) '(nil 0)) ; mincol
                               (member (third arglist) '(nil 0)) ; minpad
                               (not colon-p))
                        (progn
                          (setq forms (revappend (remove 'NIL arglist) forms))
                          (push `(let ((*print-right-margin*
                                        (STREAM-START-S-EXPRESSION STREAM)))
                                   (PRIN1 ,(formatter-next-arg) STREAM))
                                forms)
                          (push '(STREAM-END-S-EXPRESSION STREAM) forms))
                        (simple-call)))
                     (FORMAT-WRITE                  ; #\W
                      (simple-arglist 4)
                      (if (and (member (first arglist) '(nil 0)) ; mincol
                               (member (third arglist) '(nil 0))) ; minpad
                        (progn
                          (setq forms (revappend (remove 'NIL arglist) forms))
                          (push `(WRITE ,(formatter-next-arg) :STREAM STREAM)
                                forms))
                        (simple-call)))
                     (FORMAT-DECIMAL                ; #\D
                      (simple 4))
                     (FORMAT-BINARY                 ; #\B
                      (simple 4))
                     (FORMAT-OCTAL                  ; #\O
                      (simple 4))
                     (FORMAT-HEXADECIMAL            ; #\X
                      (simple 4))
                     (FORMAT-RADIX                  ; #\R
                      (simple-arglist 5)
                      (if (and (null (first arglist)) (not atsign-p))
                        (progn
                          (setq forms (revappend (remove 'NIL arglist) forms))
                          (push `(,(if colon-p 'FORMAT-ORDINAL 'FORMAT-CARDINAL)
                                  ,(formatter-next-arg) STREAM)
                                forms))
                        (simple-call)))
                     (FORMAT-PLURAL                 ; #\P
                      (simple-arglist 0)
                      (when colon-p
                        (setq forms
                              (revappend (formatter-goto-arg nil t 1) forms)))
                      (push (if atsign-p
                              `(WRITE-STRING
                                 (IF (EQL ,(formatter-next-arg) 1) "y" "ies")
                                 STREAM)
                              `(UNLESS (EQL ,(formatter-next-arg) 1)
                                 (WRITE-CHAR #\s STREAM)))
                            forms))
                     (FORMAT-CHARACTER              ; #\C
                      (simple 0))
                     (FORMAT-FIXED-FLOAT            ; #\F
                      (simple 5))
                     (FORMAT-EXPONENTIAL-FLOAT      ; #\E
                      (simple 7))
                     (FORMAT-GENERAL-FLOAT          ; #\G
                      (simple 7))
                     (FORMAT-DOLLARS-FLOAT          ; #\$
                      (simple 4))
                     (FORMAT-TERPRI                 ; #\%
                      (simple-arglist 1)
                      (if (member (first arglist) '(nil 1))
                        (push #\Newline forms) ; equiv. to `(TERPRI STREAM)
                        (trivial-call)))
                     (FORMAT-PPRINT-NEWLINE         ; #\_
                      (simple-arglist 0)
                      (push `(PPRINT-NEWLINE ,(if colon-p
                                                (if atsign-p :mandatory :fill)
                                                (if atsign-p :miser :linear))
                                             STREAM)
                            forms))
                     (FORMAT-PPRINT-INDENT          ; #\I
                      (simple-arglist 1)
                      (push `(PPRINT-INDENT ,(if colon-p :current :block)
                                            ,(or (first arglist) 0)
                                            STREAM)
                            forms))
                     (FORMAT-FRESH-LINE             ; #\&
                      (simple-arglist 1)
                      (if (member (first arglist) '(nil 1))
                        (push `(FRESH-LINE STREAM) forms)
                        (trivial-call)))
                     (FORMAT-PAGE                   ; #\|
                      (simple-arglist 1)
                      (if (member (first arglist) '(nil 1))
                        (push #\Page forms)
                        (trivial-call)))
                     (FORMAT-TILDE                  ; #\~
                      (simple-arglist 1)
                      (if (member (first arglist) '(nil 1))
                        (push #\~ forms)
                        (trivial-call)))
                     (FORMAT-TABULATE               ; #\T
                      (trivial 2))
                     (FORMAT-GOTO                   ; #\*
                      (simple-arglist 1)
                      (setq forms
                            (revappend (formatter-goto-arg atsign-p colon-p
                                         (or (first arglist) (if atsign-p 0 1)))
                                       forms)))
                     (FORMAT-INDIRECTION            ; #\?
                      (simple-arglist 0)
                      (if atsign-p
                        (push `(SETQ ,*args*
                                 (DO-FORMAT-INDIRECTION STREAM
                                   ,(formatter-next-arg)
                                   ,(progn (formatter-stop-linear) `,*args*)))
                              forms)
                        (push `(DO-FORMAT-INDIRECTION
                                 STREAM ,(formatter-next-arg)
                                 ,(formatter-next-arg))
                              forms)))
                     (FORMAT-CALL-USER-FUNCTION     ; #\/
                      (let* ((func (car (pop arglist)))
                             (argsvars (gensym-list arglist))
                             (inner-form
                               `(,func STREAM ,(formatter-next-arg) ,colon-p
                                       ,atsign-p ,@argsvars)))
                        (push (if argsvars
                                `(LET ,(mapcar #'list argsvars arglist)
                                   ,inner-form)
                                inner-form)
                              forms)))
                     (FORMAT-CASE-CONVERSION        ; #\(
                      (simple-arglist 0)
                      (setq *format-csdl* (cdr *format-csdl*))
                      (if *format-case*
                        ;; Richard Waters notes: It is possible for ~(...~) to
                        ;; be nested in a format string, but note that inner
                        ;; nested modes never have any effect. You can just
                        ;; ignore them.
                        (let ((inner-forms
                                ;; no need to bind *format-case* to t here
                                (formatter-main-1 'FORMAT-CASE-CONVERSION-END)))
                          (setq forms (revappend inner-forms forms)))
                        (push `(LET ((ORIG-STREAM STREAM)
                                     (STREAM (MAKE-STRING-OUTPUT-STREAM
                                               :LINE-POSITION (SYS::LINE-POSITION STREAM))))
                                 ,@(let* ((*format-uwps*
                                            (cons 'NIL *format-uwps*))
                                          (inner-forms
                                            (let ((*format-case* t))
                                              (formatter-main
                                                'FORMAT-CASE-CONVERSION-END)))
                                          (cleanup-forms
                                            `((WRITE-STRING
                                                (,(if colon-p
                                                    (if atsign-p
                                                      'NSTRING-UPCASE
                                                      'NSTRING-CAPITALIZE)
                                                    (if atsign-p
                                                      'SYS::NSTRING-CAPITALIZE1
                                                      'NSTRING-DOWNCASE))
                                                 (GET-OUTPUT-STREAM-STRING STREAM))
                                                ORIG-STREAM))))
                                     (if (car *format-uwps*)
                                       `((UNWIND-PROTECT (PROGN ,@inner-forms) ,@cleanup-forms))
                                       `(,@inner-forms ,@cleanup-forms))))
                              forms)))
                     (FORMAT-CONDITIONAL            ; #\[
                      (if colon-p
                        (if atsign-p
                          (format-conditional-error)
                          (progn
                            (simple-arglist 0)
                            (push `(IF (NOT ,(formatter-next-arg))
                                     (PROGN ,@(progn
                                                (formatter-stop-linear)
                                                (setq *format-csdl*
                                                      (cdr *format-csdl*))
                                                (formatter-main
                                                  'FORMAT-CONDITIONAL-END)))
                                     (PROGN ,@(progn
                                                (formatter-stop-linear)
                                                (setq *format-csdl*
                                                      (cdr *format-csdl*))
                                                (formatter-main
                                                  'FORMAT-CONDITIONAL-END))))
                                  forms)))
                        (if atsign-p
                          (progn
                            (simple-arglist 0)
                            (formatter-stop-linear)
                            (push `(IF (CAR ,*args*)
                                     (PROGN ,@(progn
                                                (setq *format-csdl*
                                                      (cdr *format-csdl*))
                                                (formatter-main
                                                  'FORMAT-CONDITIONAL-END)))
                                     (SETQ ,*args* (CDR ,*args*)))
                                  forms)
                            (unless (null (csd-clause-chain (car *format-csdl*)))
                              (format-error 'error *format-cs* nil
                                (TEXT "The ~~; format directive is not allowed at this point."))))
                          (progn
                            (simple-arglist 1)
                            (push `(CASE ,(or (first arglist)
                                              (formatter-next-arg))
                                     ,@(let ((index 0) (cases '()))
                                         (formatter-stop-linear)
                                         (loop
                                           (when (null (csd-clause-chain
                                                         (car *format-csdl*)))
                                             (return))
                                           (when (csd-colon-p (car *format-csdl*))
                                             (setq index 'T))
                                           (setq *format-csdl* (cdr *format-csdl*))
                                           (push `(,index ,@(formatter-main 'FORMAT-CONDITIONAL-END))
                                                 cases)
                                           (if (eq index 'T) (return) (incf index)))
                                         (nreverse cases)))
                                  forms)))))
                     (FORMAT-ITERATION              ; #\{
                      (simple-arglist 1)
                      (setq *format-csdl* (cdr *format-csdl*))
                      (let ((max-n-iterations (first arglist))
                            (min-1-iteration (csd-colon-p (car (csd-clause-chain csd))))
                            (indirect (eq (csd-clause-chain csd) *format-csdl*)))
                        (flet ((compute-innermost ()
                                 (if indirect
                                   (progn
                                     (formatter-stop-linear)
                                     `((SETQ ,*args*
                                             (DO-FORMAT-INDIRECTION-2 STREAM NODE
                                                                      ,*args*
                                                                      ,(formatter-whole-args 0)))))
                                   (formatter-main 'FORMAT-ITERATION-END))))
                          (flet ((compute-inner ()
                                   (if colon-p
                                     (let ((*iterargs* *args*))
                                       (formatter-bind-terminator
                                         (multiple-value-bind (lambdalist innermost)
                                             (formatter-bind-args (compute-innermost))
                                           `((APPLY #'(LAMBDA ,lambdalist ,@innermost)
                                               ,(formatter-next-arg))))))
                                     (let ((*iterargs* nil))
                                       ;; CLtL2 p. 598: "When within a ~{ construct, the "goto" is
                                       ;; relative to the list of arguments being processed by the
                                       ;; iteration." Does that mean, that for ~@{, WHOLE-ARGS has to
                                       ;; be freshly bound to ARGS at the beginning of each Iteration??
                                       ;; (if atsign-p
                                       ;;   (progn (formatter-stop-linear)
                                       ;;     `((LET ((WHOLE-ARGS ,*args*)) ,@(compute-innermost)))
                                       ;;   )
                                       ;;   (compute-innermost)
                                       ;; )
                                       (compute-innermost)))))
                            (flet ((compute-middle ()
                                     (if (eql max-n-iterations 0)
                                       '()
                                       (progn
                                         (unless (and (eql max-n-iterations 1) min-1-iteration)
                                           (formatter-stop-linear))
                                         (if (eql max-n-iterations 1)
                                           (if min-1-iteration
                                             (compute-inner)
                                             `((UNLESS (ENDP ,*args*) ,@(compute-inner))))
                                           `((BLOCK NIL
                                               (TAGBODY
                                                 L
                                                 ,@(if max-n-iterations
                                                     `((WHEN ,(if (numberp max-n-iterations)
                                                                `(>= I N)
                                                                `(AND N (>= I N)))
                                                         (RETURN))
                                                       (INCF I)))
                                                 ,@(if (not min-1-iteration)
                                                     `((WHEN (ENDP ,*args*) (RETURN))))
                                                 ,@(compute-inner)
                                                 ,@(if min-1-iteration
                                                     `((WHEN (ENDP ,*args*) (RETURN))))
                                                 (GO L)))))))))
                              (flet ((compute-outer ()
                                       (formatter-bind-terminators
                                         ;; *format-terminate-all* and *format-terminate* will be
                                         ;; bound, but if colon-p, *format-terminate* will be
                                         ;; shadowed further inside (see above).
                                         (if atsign-p
                                           (compute-middle)
                                           (multiple-value-bind (lambdalist inner-forms)
                                               (formatter-bind-args (compute-middle))
                                             `((APPLY #'(LAMBDA ,lambdalist ,@inner-forms)
                                                 ,(formatter-next-arg))))))))
                                (flet ((compute-outermost ()
                                         (if indirect
                                           `((LET ((NODE (DO-FORMAT-INDIRECTION-1 ,(formatter-next-arg))))
                                               ,@(compute-outer)))
                                           (compute-outer))))
                                  (let ((new-forms
                                          (if (and max-n-iterations (not (member max-n-iterations '(0 1))))
                                            `((LET ((N ,(first arglist)) (I 0))
                                                ,@(compute-outermost)))
                                            (compute-outermost))))
                                    (setq forms
                                          (revappend new-forms forms))))))))))
                     (FORMAT-JUSTIFICATION          ; #\<
                      (simple-arglist 4)
                      (let* ((firstseparator (car (csd-clause-chain csd)))
                             (check-on-line-overflow
                               (and (eq (csd-data firstseparator)
                                        'FORMAT-SEPARATOR)
                                    (csd-colon-p firstseparator)))
                             (bindings
                               `((POS (SYS::LINE-POSITION STREAM))
                                 (ORIG-STREAM STREAM)
                                 (STREAM (MAKE-STRING-OUTPUT-STREAM
                                           :LINE-POSITION POS))))
                             (justify-args
                               `(ORIG-STREAM
                                 ,colon-p
                                 ,atsign-p
                                 ,@arglist
                                 POS
                                 ,check-on-line-overflow
                                 ,(when check-on-line-overflow
                                    (setq *format-csdl* (cdr *format-csdl*))
                                    `(PROGN
                                       ,@(formatter-main 'FORMAT-JUSTIFICATION-END)
                                       (GET-OUTPUT-STREAM-STRING STREAM)))
                                 ,(when check-on-line-overflow
                                    (formatter-arg (first (csd-parm-list firstseparator))))
                                 ,(when check-on-line-overflow
                                    (formatter-arg (second (csd-parm-list firstseparator))))))
                             (new-forms
                               (formatter-bind-terminator
                                 (let* ((*format-uwps* (cons 'NIL *format-uwps*))
                                        (pieces-forms '()))
                                   (loop
                                     (when (null (csd-clause-chain (car *format-csdl*))) (return))
                                     (setq *format-csdl* (cdr *format-csdl*))
                                     (push (formatter-main 'FORMAT-JUSTIFICATION-END) pieces-forms))
                                   (setq pieces-forms (nreverse pieces-forms))
                                   (list
                                     (if (car *format-uwps*)
                                       `(LET* (,@bindings
                                               (JARGS (LIST ,@justify-args))
                                               (PIECES '()))
                                          (UNWIND-PROTECT
                                              (PROGN
                                                ,@(mapcap #'(lambda (piece-forms)
                                                              `(,@piece-forms
                                                                (PUSH (GET-OUTPUT-STREAM-STRING STREAM) PIECES)))
                                                          pieces-forms))
                                            (APPLY #'DO-FORMAT-JUSTIFICATION
                                                   (NCONC JARGS (LIST (SYS::LIST-NREVERSE PIECES))))))
                                       `(LET* (,@bindings)
                                          (DO-FORMAT-JUSTIFICATION
                                            ,@justify-args
                                            (LIST
                                              ,@(mapcar #'(lambda (piece-forms)
                                                            `(PROGN ,@piece-forms (GET-OUTPUT-STREAM-STRING STREAM)))
                                                        pieces-forms))))))))))
                        (setq forms (revappend new-forms forms))))
                     (FORMAT-LOGICAL-BLOCK          ; #\< ending with ~:>
                      (simple-arglist 0)
                      (multiple-value-bind (prefix suffix per-line-p
                                            body-csdl add-fill last-csdl)
                          (format-logical-block-parse *FORMAT-CSDL*)
                       ;(when add-fill
                       ;  (format-error 'error *FORMAT-CS*
                       ;      (csd-cs-index (car *FORMAT-CSDL*))
                       ;    (TEXT "Error: ~~:@> not implemented")))
                       (setq *FORMAT-CSDL* body-csdl)
                       (labels ((compute-inner ()
                                  `((PPRINT-LOGICAL-BLOCK
                                      ;; *args* refers to things *after*
                                      ;; anything used in the body.
                                      ;; I need some way to refer to
                                      ;; all the list.
                                      (STREAM
                                       ,*args*
                                       ,@(and prefix
                                           (if per-line-p
                                             `(:per-line-prefix ,prefix)
                                             `(:prefix ,prefix)))
                                       ,@(and suffix `(:suffix ,suffix)))
                                       ,@(let ((*args* 'OBJ))
                                           (formatter-main
                                             'FORMAT-JUSTIFICATION-END)))))
                                (compute-outer ()
                                  (multiple-value-bind (lambdalist inner)
                                      (formatter-bind-block (compute-inner))
                                    `(((LAMBDA ,lambdalist ,@inner)
                                         ,(if atsign-p
                                            (formatter-whole-args*)
                                            (formatter-next-arg)))))))
                         (let ((body (compute-outer)))
                           (setq *format-csdl* last-csdl)
                           (setq forms (append body forms))))))
                     (FORMAT-UP-AND-OUT             ; #\^
                      (simple-arglist 3)
                      (formatter-stop-linear)
                      (let ((argsvar (if colon-p *iterargs* *args*)))
                        (push `(IF ,(if (some #'(lambda (x) (and (constantp x) x)) arglist)
                                      `(UP-AND-OUT-P ,@arglist)
                                      (if (and (null (second arglist)) (null (third arglist)))
                                        (let ((first-arg (first arglist)))
                                          (if (null first-arg)
                                            `(ENDP ,argsvar)
                                            (if (and (consp first-arg) (eq (car first-arg) 'LENGTH))
                                              `(ENDP ,(second first-arg)) ; (EQL (LENGTH x) 0) == (ENDP x)
                                              `(CASE ,first-arg ((NIL) (ENDP ,argsvar)) ((0) T) (T NIL)))))
                                          `(UP-AND-OUT-P ,@arglist ,argsvar)))
                                 (RETURN-FROM ,(if colon-p (formatter-terminate-all) (formatter-terminate))))
                              forms)))
                     (FORMAT-CALL                   ; #\!
                      (let* ((argsvars (gensym-list arglist))
                             (inner-form
                               `(FUNCALL ,(formatter-next-arg)
                                  STREAM ,(formatter-next-arg) ,colon-p ,atsign-p
                                  ,@argsvars)))
                        (push (if argsvars
                                `(LET ,(mapcar #'list argsvars arglist)
                                   ,inner-form)
                                inner-form)
                              forms)))
                     (FORMAT-ELASTIC-NEWLINE        ; #\.
                      (simple-arglist 1)
                      (if (member (first arglist) '(nil 1))
                        (push `(EXT:ELASTIC-NEWLINE STREAM) forms)
                        (trivial-call)))
                     (t ;; Huh? Someone implemented a new format directive,
                        ;; but forgot it here! Bail out.
                      (throw 'formatter-hairy nil)))))))))
      (setq *format-csdl* (cdr *format-csdl*)))
    ;; Combine adjacent strings:
    (let ((new-forms '()))
      (dolist (form forms)
        (when (characterp form) (setq form (string form)))
        (if (and (consp new-forms) (stringp (car new-forms)) (stringp form))
          (setf (car new-forms) (string-concat form (car new-forms)))
          (push form new-forms)))
      new-forms)))
(defun formatter-main (&optional (endmarker nil))
  (let ((new-forms (formatter-main-1 endmarker)))
    ;; Convert strings to WRITE-STRING forms:
    (mapcap #'(lambda (form)
                (if (stringp form)
                  (case (length form)
                    (0 )
                    (1 (setq form (char form 0))
                       `(,(if (eq form #\Newline)
                            `(TERPRI STREAM)
                            `(WRITE-CHAR ,form STREAM))))
                    (t `((WRITE-STRING ,form STREAM))))
                  (list form)))
            new-forms)))

;; FORMATTER, CLtL2 p. 764
(defmacro formatter (control-string)
  (unless (stringp control-string)
    (error-of-type 'type-error
      :datum control-string :expected-type 'string
      (TEXT "~S: The control-string must be a string, not ~S")
      'formatter control-string))
  ;; possibly convert control-string to Simple-String ??
  (or
    (catch 'formatter-hairy
      (let ((node (list control-string)))
        (format-parse-cs control-string 0 node nil)
        (let ((*FORMAT-CS* (car node))
              (*FORMAT-CSDL* (cdr node))
              (*format-case* nil)
              (*format-uwps* '())
              (*iterargs* nil))
          (multiple-value-bind (lambdalist forms)
              (formatter-bind-args
                `(,@(formatter-bind-terminators
                      (formatter-main))
                  ,(progn (formatter-stop-linear) `,*args*)))
            `(FUNCTION
               (LAMBDA (STREAM ,@lambdalist)
                 (DECLARE (IGNORABLE STREAM))
                 ,@forms))))))
    `(FORMATTER-HAIRY ,(coerce control-string 'simple-string))))

;;; ---------------------------------------------------------------------------

;; (FORMAT-QUOTE string)
;; returns a format-string that yields exactly the given string.
(defun format-quote (string)
  (let ((qstring (make-array 10 :element-type 'character
                                :adjustable t :fill-pointer 0)))
    (map nil #'(lambda (c)
                 (when (eql c #\~) (vector-push-extend #\~ qstring))
                 (vector-push-extend c qstring))
         string)
    qstring))
