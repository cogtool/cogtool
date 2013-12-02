;;;; Apropos, Describe

(common-lisp:in-package "SYSTEM")
(export
 '(custom::*apropos-matcher* custom::*apropos-do-more*)
 "CUSTOM")
(re-export "CUSTOM" "EXT")

;;-----------------------------------------------------------------------------
;; APROPOS

(defvar *apropos-matcher* nil
  "A function of one argument, a pattern (a string),
returning a new function of one argument, a symbol name (also a string),
which returns non-NIL when the symbol name matches the pattern
for the purposes of APROPOS.
When this variable is NIL, SEARCH is used.")

(defun apropos-list (string &optional (package nil))
  (let* ((L nil)
         (matcher (and *apropos-matcher* (funcall *apropos-matcher* string)))
         (fun (if matcher
                  (lambda (sym)
                    (when (funcall matcher (symbol-name sym))
                      (push sym L)))
                  (lambda (sym)
                    (when
                        ;; (search string (symbol-name sym) :test #'char-equal)
                        (sys::search-string-equal string sym) ; 15x faster!
                      (push sym L))))))
    (if package
      (system::map-symbols fun package)
      (system::map-all-symbols fun))
    (stable-sort (delete-duplicates L :test #'eq :from-end t)
                 #'string< :key #'symbol-name)))

(defvar *apropos-do-more* nil
  "Print values of the symbols in `apropos'.
It can be a list of :FUNCTION, :VARIABLE, :TYPE, :CLASS
to print the corresponding values, or T for all of them.")

(defun apropos-do-more (what)
  (or (eq t *apropos-do-more*) (memq what *apropos-do-more*)))

(defun apropos (string &optional (package nil))
  (dolist (sym (apropos-list string package))
    (format t "~&~s~40t" sym)
    (when (fboundp sym)
      #1=(write-string "   ")   ; spacer
      (write-string (fbound-string sym))
      (when (apropos-do-more :function)
        (format t " [~s]" (fdefinition sym))))
    (when (symbol-macro-expand sym)
      #1# (write-string (TEXT "symbol-macro")))
    (when (boundp sym)
      #1# (if (constantp sym)
            (write-string (TEXT "constant"))
            (write-string (TEXT "variable")))
      (when (apropos-do-more :variable)
        (format t " [~s]" (symbol-value sym))))
    (let ((type (or (get sym 'system::type-symbol)
                    (get sym 'system::defstruct-description))))
      (when type
        #1# (write-string (TEXT "type"))
        (when (apropos-do-more :type)
          (format t " [~s]" type))))
    (let ((class (get sym 'clos::closclass)))
      (when class
        #1# (write-string (TEXT "class"))
        (when (apropos-do-more :class)
          (format t " [~s]" class))))
    (elastic-newline))
  (values))

;;-----------------------------------------------------------------------------
;; DESCRIBE

;; Number of recursive calls since the top-level call.
(defvar *describe-nesting*)

; List of objects which have been described during the current top-level call.
(defvar *describe-done*)

(defun describe-slotted-object (object stream)
  (let ((slotnames (clos::slot-names object)))
    (if slotnames
      (let* ((slotstrings (mapcar #'write-to-string slotnames)) more
             (tabpos (+ *print-indent-lists* 4 (reduce #'max (mapcar #'length slotstrings)))))
        (terpri stream)
        (format stream (TEXT "Slots:"))
        (mapc #'(lambda (slotname slotstring)
                  (format stream "~%~V,0T  ~A~VT"
                          *print-indent-lists* slotstring tabpos)
                  (cond ((clos:slot-boundp object slotname)
                         (format stream "=  ~S" (clos:slot-value object slotname))
                         (pushnew (clos:slot-value object slotname) more))
                        ((format stream (TEXT "unbound")))))
              slotnames slotstrings)
        (dolist (vv (nreverse more)) (describe vv stream)))
      (progn
        (terpri stream)
        (format stream (TEXT "No slots."))))))

(defun launch-doc (obj type stream name)
  (when (zerop *describe-nesting*)
    (let ((doc (documentation obj type)))
      (when doc
        (if *browser*
            (ext::browse-url doc :out stream)
            (format stream name doc))))))

(clos:defgeneric describe-object (obj stream)
  (:method ((obj t) (stream stream))
    (ecase (type-of obj)
      #+(or UNIX FFI WIN32)
      (EXT::FOREIGN-POINTER
       (format stream (TEXT "a foreign pointer.")))
      #+FFI
      (FFI::FOREIGN-ADDRESS
       (format stream (TEXT "a foreign address.")))
      #+FFI
      (FFI::FOREIGN-VARIABLE
       (format stream (TEXT "a foreign variable of foreign type ~S.")
               (ffi::deparse-c-type (sys::%record-ref obj 3))))
      #+SOCKETS
      (SOCKET-SERVER
       (format stream (TEXT "a server socket accepting connections.")))
      (BYTE
       (format stream (TEXT "a byte specifier, denoting the ~S bits starting at bit position ~S of an integer.")
               (byte-size obj) (byte-position obj)))
      (EXT:SPECIAL-OPERATOR
       (format stream (TEXT "a special form handler.")))
      (EXT:LOAD-TIME-EVAL
       (format stream (TEXT "a load-time evaluation promise.")))
      (EXT:SYMBOL-MACRO
       (format stream (TEXT "a symbol macro handler.")))
      (SYS::MACRO
       (format stream (TEXT "a macro expander."))
       (terpri stream)
       (format stream (TEXT "For more information, evaluate ~{~S~^ or ~}.")
               `((DISASSEMBLE (MACRO-FUNCTION
                               ',(sys::closure-name
                                  (sys::macro-expander obj)))))))
      (EXT:FUNCTION-MACRO
       (format stream (TEXT "a function with alternative macro expander.")))
      (EXT:ENCODING
       (format stream (TEXT "an encoding.")))
      (EXT:WEAK-POINTER
       (multiple-value-bind (value validp) (weak-pointer-value obj)
         (if validp
           (progn
             (format stream (TEXT "a GC-invisible pointer to ~S.")
                     value)
             (describe value stream))
           (format stream (TEXT "a GC-invisible pointer to a now defunct object.")))))
      (EXT:WEAK-LIST
       (let ((remaining (weak-list-list obj)))
         (if remaining
           (format stream (TEXT "a list of GC-invisible pointers to ~{~S~^, ~}.")
                   remaining)
           (format stream (TEXT "a list of GC-invisible pointers, all defunct by now.")))))
      (EXT:WEAK-AND-RELATION
       (let ((remaining (weak-and-relation-list obj)))
         (if remaining
           (format stream (TEXT "a weak \"and\" relation between ~{~S~^, ~}.")
                   remaining)
           (format stream (TEXT "a weak \"and\" relation, no longer referring to its objects.")))))
      (EXT:WEAK-OR-RELATION
       (let ((remaining (weak-or-relation-list obj)))
         (if remaining
           (format stream (TEXT "a weak \"or\" relation between ~{~S~^, ~}.")
                   remaining)
           (format stream (TEXT "a weak \"or\" relation, all elements defunct by now.")))))
      (EXT:WEAK-MAPPING
       (multiple-value-bind (key value alive) (weak-mapping-pair obj)
         (if alive
           (format stream (TEXT "a weak association from ~S to ~S.") key value)
           (format stream (TEXT "a weak association, the key value being defunct by now.")))))
      (EXT:WEAK-AND-MAPPING
       (multiple-value-bind (keys value alive) (weak-and-mapping-pair obj)
         (if alive
           (format stream (TEXT "a weak \"and\" mapping from ~:S to ~S.") keys value)
           (format stream (TEXT "a weak \"and\" mapping, some key value being defunct by now.")))))
      (EXT:WEAK-OR-MAPPING
       (multiple-value-bind (keys value alive) (weak-or-mapping-pair obj)
         (if alive
           (format stream (TEXT "a weak \"or\" mapping from ~:S to ~S.") keys value)
           (format stream (TEXT "a weak \"or\" mapping, all keys being defunct by now.")))))
      (EXT:WEAK-ALIST
       (let ((type (weak-alist-type obj))
             (remaining (weak-alist-contents obj)))
         (format stream (TEXT "a weak association list, of type ~S ") type)
         (ecase type
           (:KEY    (format stream (TEXT "(i.e. a list of EXT:WEAK-MAPPING key/value pairs)")))
           (:VALUE  (format stream (TEXT "(i.e. a list of EXT:WEAK-MAPPING value/key pairs)")))
           (:KEY-AND-VALUE (format stream (TEXT "(i.e. a list of (key . value) pairs each combined into a EXT:WEAK-AND-RELATION)")))
           (:KEY-OR-VALUE  (format stream (TEXT "(i.e. a list of (key . value) pairs each combined into a EXT:WEAK-OR-RELATION)"))))
         (if remaining
           (format stream (TEXT ", containing ~S.") remaining)
           (format stream (TEXT ", no longer referring to any pairs.")))))
      (SYS::READ-LABEL
       (format stream (TEXT "a label used for resolving #~D# references during READ.")
               (logand (sys::address-of obj)
                       (load-time-value (ash most-positive-fixnum -1)))))
      (FRAME-POINTER
       (format stream (TEXT "a pointer into the stack. It points to:"))
       (sys::describe-frame stream obj))
      (SYSTEM-INTERNAL
       (format stream (TEXT "a special-purpose object.")))
      (ADDRESS
       (format stream (TEXT "a machine address.")))))
  (:method ((obj clos:standard-object) (stream stream))
    (format stream (TEXT "an instance of the CLOS class ~S.")
            (clos:class-of obj))
    (describe-slotted-object obj stream))
  (:method ((obj clos:funcallable-standard-object) (stream stream))
    (format stream (TEXT "an instance of the CLOS class ~S, can be used as a function.")
            (clos:class-of obj))
    (describe-slotted-object obj stream))
  (:method ((obj structure-object) (stream stream)) ; CLISP specific
    (format stream (TEXT "a structure of type ~S.")
            (type-of obj))
    (let ((types (butlast (cdr (sys::%record-ref obj 0)))))
      (when types
        (terpri stream)
        (format stream (TEXT "As such, it is also a structure of type ~{~S~^, ~}.")
                types)))
    (describe-slotted-object obj stream))
  (:method ((obj cons) (stream stream))
    (multiple-value-bind (len dotted-p) (list-length-dotted obj)
      (if len
        (if dotted-p
          (if (> len 1)
            (format stream (TEXT "a dotted list of length ~S.")
                    len)
            (progn
              (format stream (TEXT "a cons."))
              (describe (car obj) stream)
              (describe (cdr obj) stream)))
          (format stream (TEXT "a list of length ~S.")
                  len))
        (format stream (TEXT "a cyclic list.")))))
  (:method ((obj null) (stream stream))
    (format stream (TEXT "the empty list, "))
    (clos:call-next-method))
  (:method ((obj symbol) (stream stream))
    (format stream (TEXT "the symbol ~S, ")
            obj)
    (let ((home (symbol-package obj)) mored moree)
      (cond (home
             (format stream (TEXT "lies in ~S")
                     home)
             (pushnew home mored))
            (t (format stream (TEXT "is uninterned"))))
      (let ((accessible-packs nil))
        (let ((*print-escape* t) (*print-readably* nil))
          (let ((normal-printout
                 (if home
                   (let ((*package* home)) (prin1-to-string obj))
                   (let ((*print-gensym* nil)) (prin1-to-string obj)))))
            (dolist (pack (list-all-packages))
              (when ; obj in pack accessible?
                  (string=
                   (let ((*package* pack)) (prin1-to-string obj))
                   normal-printout)
                (push pack accessible-packs)))))
        (when accessible-packs
          (format stream (TEXT ", is accessible in ~:d package~:p ~{~A~^, ~}")
                  (length accessible-packs)
                  (sort (mapcar #'package-name accessible-packs)
                        #'string<))))
      (when (keywordp obj)
        (format stream (TEXT ", is a keyword")))
      (when (symbol-macro-expand obj)
        (format stream (TEXT ", symbol-macro expanding to: ~S")
                (macroexpand-1 obj))
        (push `(macroexpand-1 ',obj) moree))
      (cond ((boundp obj)
             (if (constantp obj)
               (format stream (TEXT ", a constant"))
               (if (sys::special-variable-p obj)
                 (format stream (TEXT ", a variable declared SPECIAL"))
                 (format stream (TEXT ", a variable"))))
             (format stream (TEXT ", value: ~s") (symbol-value obj))
             (pushnew (symbol-value obj) mored))
            ((sys::special-variable-p obj)
             (format stream (TEXT ", an unbound variable declared SPECIAL"))))
      (when (fboundp obj)
        (format stream (TEXT ", names "))
        (cond ((special-operator-p obj)
               (format stream (TEXT "a special operator"))
               (when (macro-function obj)
                 (format stream (TEXT " with macro definition"))))
              ((functionp (symbol-function obj))
               (format stream (TEXT "a~:[~; deprecated~] function")
                       (assoc obj *deprecated-functions-alist* :test #'eq)))
              (t ; (macro-function obj)
               (format stream (TEXT "a~:[~; deprecated~] macro")
                       (assoc obj *deprecated-functions-alist* :test #'eq))))
        (let ((dep (get obj 'deprecated)))
          (when dep
            (format stream (TEXT " (use ~s instead)") dep)))
        (pushnew (symbol-function obj) mored))
      (when (or (get obj 'system::type-symbol)
                (get obj 'system::defstruct-description)
                (get obj 'system::deftype-expander))
        (format stream (TEXT ", names a type"))
        (when (get obj 'system::deftype-expander)
          (push `(type-expand ',obj t) moree)))
      (when (clos::defined-class-p (get obj 'clos::closclass))
        (format stream (TEXT ", names a class")))
      (when (symbol-plist obj)
        (let ((properties
               (do ((l nil) (pl (symbol-plist obj) (cddr pl)))
                   ((null pl) (nreverse l))
                 (push (car pl) l))))
          (format stream (TEXT ", has ~:D propert~@:P ~{~S~^, ~}")
                  (length properties) properties))
        (push `(symbol-plist ',obj) moree))
      (format stream (TEXT "."))
      (dolist (ty '(compiler-macro setf structure type variable function))
        (let ((doc (documentation obj ty)))
          (when doc
            (terpri stream)
            (format stream (TEXT "Documentation as a ~a:") ty)
            (terpri stream)
            (princ doc stream))))
      (launch-doc obj 'ext::clhs ; change to sys::clhs when ext:clhs is finally removed
                  stream (TEXT "~%ANSI Documentation is at~% ~S"))
      (launch-doc obj 'sys::impnotes stream
                  (TEXT "~%CLISP Documentation is at~% ~S"))
      (when moree
        (terpri stream)
        (format stream (TEXT "For more information, evaluate ~{~S~^ or ~}.")
                moree))
      (dolist (zz (nreverse mored)) (describe zz stream))))
  (:method ((obj integer) (stream stream))
    (format stream (TEXT "an integer, uses ~S bit~:p, is represented as a ~:[bignum~;fixnum~].")
            (integer-length obj) (sys::fixnump obj)))
  (:method ((obj ratio) (stream stream))
    (format stream (TEXT "a rational, not integral number.")))
  (:method ((obj float) (stream stream))
    (format stream (TEXT "a float with ~S bits of mantissa (~(~A~)).")
            (float-digits obj) (type-of obj)))
  (:method ((obj complex) (stream stream))
    (format stream (TEXT "a complex number "))
    (let ((x (realpart obj))
          (y (imagpart obj)))
      (if (zerop y)
        (if (zerop x)
          (format stream (TEXT "at the origin"))
          (format stream (TEXT "on the ~:[posi~;nega~]tive real axis")
                  (minusp x)))
        (if (zerop x)
          (format stream (TEXT "on the ~:[posi~;nega~]tive imaginary axis")
                  (minusp y))
          (format stream (TEXT "in the ~:[~:[first~;fourth~]~;~:[second~;third~]~] quadrant")
                  (minusp x) (minusp y)))))
    (format stream (TEXT " of the Gaussian number plane.")))
  (:method ((obj character) (stream stream))
    (format stream (TEXT "a character"))
    (format stream (TEXT "."))
    #+UNICODE
    (let ((unicode-name (unicode-attributes obj)))
      (terpri stream)
      (if unicode-name
        (format stream (TEXT "Unicode name: ~A") unicode-name)
        (format stream (TEXT "It is not defined by the Unicode standard."))))
    (terpri stream)
    (format stream (TEXT "It is a ~:[non-~;~]printable character.")
            (graphic-char-p obj))
    (unless (standard-char-p obj)
      (terpri stream)
      (format stream (TEXT "Its use is non-portable."))))
  (:method ((obj stream) (stream stream))
    (format stream (TEXT "a~:[~:[ closed ~;n output-~]~;~:[n input-~;n input/output-~]~]stream.")
            (and (input-stream-p obj) (open-stream-p obj))
            (and (output-stream-p obj) (open-stream-p obj))))
  (:method ((obj package) (stream stream))
    (if (package-name obj)
      (progn
        (format stream (TEXT "the package named ~A")
                (package-name obj))
        (let ((nicknames (package-nicknames obj)))
          (when nicknames
            (format stream (TEXT ". It has ~:d nickname~:p ~{~A~^, ~}")
                    (length nicknames) nicknames)))
        (format stream (TEXT "."))
        (let ((use-list (package-use-list obj))
              (used-by-list (package-used-by-list obj)))
          (terpri stream)
          (format stream (TEXT "It "))
          (when use-list
            (format stream (TEXT "imports the external symbols of ~:d package~:p ~{~A~^, ~} and ")
                    (length use-list)
                    (mapcar #'package-name use-list)))
          (let ((L nil) (count 0)) ; maybe list all exported symbols
            (do-external-symbols (s obj) (push s L) (incf count))
            (format stream
                    (TEXT "exports ~[no symbols~:;~:*~:d symbol~:p~]")
                    count)
            (when (zerop *describe-nesting*)
              (format stream (TEXT "~{ ~S~^,~}")
                      (sort L #'string< :key #'symbol-name))))
          (if used-by-list
            (format stream (TEXT " to ~:d package~:p ~{~A~^, ~}")
                    (length used-by-list)
                    (mapcar #'package-name used-by-list))
            (format stream (TEXT ", but no package uses these exports")))
          (format stream (TEXT "."))
          (let ((case-inverted-p (package-case-inverted-p obj))
                (case-sensitive-p (package-case-sensitive-p obj))
                (uses-cl (memq (find-package "COMMON-LISP") use-list))
                (uses-cs-cl (memq (find-package "CS-COMMON-LISP") use-list)))
            (cond ((and (not case-inverted-p) (not case-sensitive-p)
                        (not uses-cs-cl)))
                  ((and case-inverted-p case-sensitive-p (not uses-cl))
                   (terpri stream)
                   (format stream (TEXT "It is a modern case-sensitive package.")))
                  (t
                   (terpri stream)
                   (format stream (TEXT "ATTENTION: "))
                   (if case-inverted-p
                     (if case-sensitive-p
                       (format stream (TEXT "It is a modern case-sensitive package, but uses the symbols from the traditional ~S!")
                               (find-package "COMMON-LISP"))
                       (format stream (TEXT "It is case-inverted, but not case-sensitive!")))
                     (if case-sensitive-p
                       (format stream (TEXT "It is case-sensitive, but not case-inverted!"))
                       (format stream (TEXT "It is a traditional ANSI CL compatible package, but uses the symbols from the modern ~S!")
                               (find-package "CS-COMMON-LISP"))))))))
        (launch-doc obj 'sys::impnotes stream
                    (TEXT "~%CLISP Documentation is at~% ~S")))
      (format stream (TEXT "a deleted package."))))
  (:method ((obj hash-table) (stream stream))
    (let ((count (hash-table-count obj)))
      (format stream (TEXT "an ~s hash table with ~[no entries~:;~:*~:d entr~:*~[~;y~:;ies~]~].")
              (hash-table-test obj) count)))
  (:method ((obj readtable) (stream stream))
    (format stream (TEXT "~:[a~;the Common Lisp~] readtable.")
            (equalp obj (copy-readtable))))
  (:method ((obj pathname) (stream stream))
    (format stream (TEXT "a ~:[~;portable ~]pathname~:[.~;~:*, with the following components:~{~A~}~]")
            (sys::logical-pathname-p obj)
            (mapcan #'(lambda (kw component)
                        (case component
                          ((nil :unspecific)) ; ignore
                          (t (list (format nil "~%~A = ~S" kw component)))))
                    '(:host :device :directory :name :type :version)
                    (list (pathname-host obj)
                          (pathname-device obj)
                          (pathname-directory obj)
                          (pathname-name obj)
                          (pathname-type obj)
                          (pathname-version obj)))))
  (:method ((obj random-state) (stream stream))
    (format stream (TEXT "a random-state.")))
  (:method ((obj array) (stream stream))
    (let ((rank (array-rank obj))
          (eltype (array-element-type obj)))
      (format stream (TEXT "a~:[~; simple~] ~A dimensional array")
              (simple-array-p obj) rank)
      (when (eql rank 1)
        (format stream (TEXT " (vector)")))
      (case eltype
        ((T))
        ((NIL) (write-string (TEXT " with no storage") stream))
        (OTHERWISE (format stream (TEXT " of ~As") eltype)))
      (when (adjustable-array-p obj)
        (format stream (TEXT ", adjustable")))
      (when (plusp rank)
        (format stream (TEXT ", of size ~{~S~^ x ~}")
                (array-dimensions obj))
        (when (array-has-fill-pointer-p obj)
          (format stream (TEXT " and current length (fill-pointer) ~S")
                  (fill-pointer obj))))
      (when (and (stringp obj) (not (eq eltype 'NIL)))
        #-UNICODE
        (format stream (TEXT " (a string)"))
        #+UNICODE
        (multiple-value-bind (bits ro-p realloc) (sys::string-info obj)
          (format stream (TEXT " (a~:[~;n immutable~] ~:[~;reallocated ~]~A string)")
                  ro-p realloc
                  (case bits (8 "ISO-8859-1") (16 "UCS-2") (32 "UCS-4")))))
      (format stream (TEXT "."))))
  (:method ((obj generic-function) (stream stream))
    (format stream (TEXT "a generic function."))
    (unless (clos::generic-function-undeterminedp obj)
      (terpri stream)
      (format stream (TEXT "Argument list: ~A")
              (clos:generic-function-lambda-list obj)))
    (let ((mc (clos::method-combination-name (clos:generic-function-method-combination obj))))
      (unless (eq mc 'STANDARD)
        (terpri stream)
        (format stream (TEXT "Method combination: ~S") mc)))
    (let ((methods (clos:generic-function-methods obj)))
      (terpri stream)
      (if methods
        (progn
          (format stream (TEXT "Methods:"))
          (dolist (meth (clos:generic-function-methods obj))
            (format stream "~%  ~{~S ~}~S" (clos:method-qualifiers meth)
                    (mapcar #'(lambda (specializer)
                                (setq specializer (clos::specializer-pretty specializer))
                                (if (and (clos::defined-class-p specializer)
                                         (eq (find-class (class-name specializer) nil)
                                             specializer))
                                  (class-name specializer)
                                  specializer))
                            (clos:method-specializers meth)))))
        (format stream (TEXT "No methods.")))))
  (:method ((obj function) (stream stream))
    (ecase (type-of obj)        ; not etypecase!
      #+FFI
      (FFI::FOREIGN-FUNCTION
       (format stream (TEXT "a foreign function of foreign type ~S.")
               (ffi::deparse-c-type (vector 'ffi::c-function
                                            (sys::%record-ref obj 2)
                                            (sys::%record-ref obj 3)
                                            (sys::%record-ref obj 4)))))
      (COMPILED-FUNCTION
       (multiple-value-bind (name req opt rest-p keywords other-keys)
           (sys::subr-info obj)
         (if (and name req)
           (progn
             (format stream (TEXT "a built-in system function."))
             (sys::describe-signature stream req opt rest-p
                                      keywords keywords other-keys))
           (progn
             (format stream (TEXT "a compiled function."))
             (multiple-value-bind (req opt rest-p key-p keywords other-keys-p)
                 (sys::signature obj)
               (sys::describe-signature stream req opt rest-p key-p keywords
                                        other-keys-p)
               (let* ((name (sys::closure-name obj))
                      (funform (cond ((and (symbolp name) (macro-function name))
                                      `(MACRO-FUNCTION ',name))
                                     ((fboundp name) `(FUNCTION ,name)))))
                 (when funform
                   (terpri stream)
                   (format stream
                           (TEXT "For more information, evaluate ~{~S~^ or ~}.")
                           `((DISASSEMBLE ,funform))))))))))
      (FUNCTION
       ;; we do not use ETYPECASE here to ensure that if we do get here,
       ;; we are dealing with an Iclosure object
       (format stream (TEXT "an interpreted function."))
       (let ((doc (sys::%record-ref obj 2)))
         (terpri stream)
         (format stream (TEXT "Argument list: ~:S")
                 (car (sys::%record-ref obj 1)))
         (when doc
           (terpri stream)
           (format stream (TEXT "Documentation: ~A") doc)))))))

(defun describe1 (obj stream)
  (let ((objstring (sys::write-to-short-string
                    obj (or *print-right-margin* sys::*prin-linelength*))))
    (fresh-line stream)
    (terpri stream) ; blank line
    (if (memq obj *describe-done*)
      (format stream (TEXT "~A [see above]") objstring)
      (let ((doc (and (symbolp obj) (get obj 'sys::doc))))
        (push obj *describe-done*)
        (format stream (TEXT "~A is ") objstring)
        (describe-object obj stream)
        (fresh-line stream)
        (when doc
          (terpri stream) ; blank line
          (format stream (TEXT "Documentation:"))
          (do ((tail doc (cddr tail)))
              ((endp tail))
            (format stream "~&~s:~%~s" (car tail) (cadr tail)))
          (fresh-line stream)))))
  (finish-output stream))

;; A private class through which we can distinguish recursive describe calls
;; from top-level calls.
(defclass describe-stream (fill-stream)
  ())

(defun describe (obj &optional stream)
  (cond ((typep stream 'describe-stream) ; Recursive call
         ;; flush the pending output _before_ increasing indentation
         (force-output stream)
         (let ((*describe-nesting* (1+ *describe-nesting*))
               (*print-right-margin*
                (max (- (or *print-right-margin* sys::*prin-linelength*)
                        *print-indent-lists*)
                     1)))
           (describe1 obj stream)))
        (t                      ; Top-level call
         (cond ((eq stream 'nil) (setq stream *standard-output*))
               ((eq stream 't) (setq stream *terminal-io*)))
         (let ((*print-circle* t)
               (*describe-nesting* 0)
               (*describe-done* nil))
           (describe1 obj (clos:make-instance 'describe-stream
                           :stream stream
                           :text-indent '*describe-nesting*)))))
  (values))

;;-----------------------------------------------------------------------------
;; auxiliary functions for DESCRIBE of FUNCTION

(defun arglist (func)
  (setq func (coerce func 'function))
  (if (typep func 'generic-function)
    ; Generic functions store the lambda-list. It has meaningful variable names.
    (clos:generic-function-lambda-list func)
    ; Normal functions store only the signature, no variable names.
    (sig-to-list (get-signature func))))

(defun describe-signature (stream req-anz opt-anz rest-p keyword-p keywords
                           allow-other-keys)
  (terpri stream)
  (format stream (TEXT "Argument list: ~A.")
          (format nil "(~{~A~^ ~})"
                  (signature-to-list req-anz opt-anz rest-p keyword-p keywords
                                     allow-other-keys))))

;;-----------------------------------------------------------------------------
;; auxiliary functions for CLISP metadata

(defun clisp-data-file (name)
  ;; [ $(lisplibdir) == *lib-directory* ]/data/name
  (let ((*merge-pathnames-ansi* t) path
        (data (make-pathname :directory (list :relative "data"))))
    (assert (probe-file
             (setq path (merge-pathnames
                         name (merge-pathnames data *lib-directory*))))
            (*lib-directory*) "~s: file ~s does not exist - adjust ~s"
            'clisp-data-file path '*lib-directory*)
    path))

;;-----------------------------------------------------------------------------
;; auxiliary functions for DESCRIBE of CHARACTER

#+UNICODE (progn

;; Return the line associated with a Unicode code in the Unicode data file.
;; Returns a simple-string or nil. Also used by the CHAR-NANE function.
(defun unicode-attributes-line (code)
  (with-open-file (f (clisp-data-file "UnicodeDataFull.txt")
                     :direction :input
                     :element-type 'character
                     :external-format 'charset:ascii)
    ;; We know that the file's lines are sorted according to the code, and
    ;; we use this fact to perform a fast binary search.
    (flet ((code-at-pos ()
             ; Returns the code of the first line contained after the current
             ; position.
             (if (read-line f nil nil)
               (let ((c1 (read-char f nil nil))
                     (c2 (read-char f nil nil))
                     (c3 (read-char f nil nil))
                     (c4 (read-char f nil nil)))
                 (if (and c1 c2 c3 c4)
                   (let ((c5 (read-char f nil nil)))
                     (if (and c5 (digit-char-p c5 16))
                       (let ((c6 (read-char f nil nil)))
                         (if (and c6 (digit-char-p c6 16))
                           (parse-integer
                             (coerce (list c1 c2 c3 c4 c5 c6) 'string)
                             :radix 16
                           )
                           (parse-integer
                             (coerce (list c1 c2 c3 c4 c5) 'string)
                             :radix 16
                       ) ) )
                       (parse-integer
                         (coerce (list c1 c2 c3 c4) 'string)
                         :radix 16
                   ) ) )
                   #x1000000
               ) )
               #x1000000
          )) )
      (let* ((blocksize 1024)
             (f-size (progn (file-position f :end) (file-position f)))
             (lblock 0) ; lower bound for block number
             (rblock (1+ (floor f-size blocksize))))
        ; The block number where the line starts is >= lblock, < rblock.
        (loop
          (when (= (- rblock lblock) 1) (return))
          (let ((mblock (ash (+ lblock rblock) -1)))
            (file-position f (* mblock blocksize))
            (if (> (code-at-pos) code)
              (setq rblock mblock)
              (setq lblock mblock)
        ) ) )
        ; Sequentially read the block, searching for the first line whose
        ; number is >= code.
        (file-position f (* lblock blocksize))
        (when (plusp code) ; hack to make code=0000 work
          (read-line f nil nil)
        )
        (loop
          (let ((line (read-line f nil nil)))
            (unless line (return nil))
            (let* ((four (position #\; line)) ; 4 or 5 or 6
                   (c (parse-integer line :end four :radix 16)))
              ; Treat the range start/end lines specially.
              (when (eql (char line four) #\;)
                (when (eql (char line (+ four 1)) #\<)
                  (let* ((piece1 (subseq line (+ four 1)
                                              (or (position #\; line
                                                            :start (+ four 1))
                                                  (length line))))
                         (n (length piece1)))
                    (cond ((and (= c code)
                                (>= n 8)
                                (string= piece1 ", First>" :start1 (- n 8))
                           )
                           (return
                             (concatenate 'string
                               (format nil "~4,'0X" code)
                               (subseq line four (+ four n -8 1))
                               (subseq line (+ four n))
                          )) )
                          ((and (>= c code)
                                (>= n 7)
                                (string= piece1 ", Last>" :start1 (- n 7))
                           )
                           (return
                             (concatenate 'string
                               (format nil "~4,'0X" code)
                               (subseq line four (+ four n -7 1))
                               (subseq line (+ four n))
                          )) )
              ) ) ) )
              (if (= c code) (return line))
              (if (> c code) (return nil))
        ) ) )
) ) ) )

;; Return the Unicode attributes of a character, as 14 values,
;; or NIL if the character is not defined in the Unicode standard.
;; The values are:
;;  1. Character name.
;;  2. General category.
;;  3. Canonical combining classes.
;;  4. Bidirectional category.
;;  5. Character decomposition.
;;  6. Decimal digit value.
;;  7. Digit value.
;;  8. Numeric value.
;;  9. mirrored-p.
;; 10. Old Unicode 1.0 name.
;; 11. Comment.
;; 12. Upper case equivalent mapping.
;; 13. Lower case equivalent mapping.
;; 14. Title case equivalent mapping.
(defun unicode-attributes (ch)
  (let ((line (unicode-attributes-line (char-code ch))))
    (when line
      (let ((pieces
             (loop :for pos = 0 :then (1+ semicolon-pos)
               :for semicolon-pos = (position #\; line :start pos)
               :collect (subseq line pos (or semicolon-pos (length line)))
               :do (unless semicolon-pos (loop-finish)))))
        (assert (= (parse-integer (nth 0 pieces) :radix 16) (char-code ch)))
        (values-list
         (append
          (mapcar #'(lambda (x) (if (equal x #1="") nil x))
                  (subseq pieces 1 12))
          (mapcar #'(lambda (x)
                      (if (equal x #1#) nil
                          (code-char (parse-integer x :radix 16))))
                  (subseq pieces 12 15))))))))

) ; #+UNICODE

;;-----------------------------------------------------------------------------

;; DOCUMENTATION mit abfragen und ausgeben??
;; function, variable, type, structure, setf
