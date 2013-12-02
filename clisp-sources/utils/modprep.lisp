;;; MODPREP - CLISP module preprocessor
;;;
;;; Copyright (C) 1998 Bruno Haible (20.9.1998, 10.-11.10.1998) [C]
;;; Copyright (C) 2003-2005 by Sam Steingold [lisp]
;;; This is Free Software, covered by the GNU GPL (v2)
;;; See http://www.gnu.org/copyleft/gpl.html
#| This preprocessor generates all necessary tables for a CLISP module.
The input file is normal C code, modified like this:
- It includes "clisp.h".
- There is one (and only one) declaration
    DEFMODULE(module_name,"PACKAGE-NAME")
  module_name is the clisp module name.
  PACKAGE-NAME is the default package name (in upper case) for Lisp
  functions.
- Constant Lisp objects can be referred to using the backquote syntax:
    pushSTACK(`:TEST`);
    value1 = `#()`;
  The backquoted strings are read in at module load time.
- The module SUBRs can be referred to using double backquote syntax:
    pushSTACK(``FOO:BAR``);
  means the same as L(bar) in the CLISP sources
- The definition of Lisp functions is done using the macro
    DEFUN(function_name, lambda_list)
  for example
    DEFUN(foo::bar, x y [&optional z] [&rest foo | &key a b c] )
  &rest and &key cannot be combined (this is a restriction for SUBRs).
  &key requires at least one keyword (this is a restriction for SUBRs too).
- Variables containing Lisp objects (known to the garbage collector) are
  defined using the macro
    DEFVAR(variable_name, initform)
  where initform is a C form. (You can also specify a Lisp initform, by
  using the backquote syntax.) The variable can be referred to as
    O(variable_name)
  These variables are private to the module.
- DEFFLAGSET(c_name, C_CONST1 C_CONST2 C_CONST3)
  is converted to
  static uint32 c_name (void) {
   uint32 ret = 0
    #if defined(C_CONST3)
      | (missingp(STACK_0) ? 0 : C_CONST3)
    #endif
    #if defined(C_CONST2)
      | (missingp(STACK_1) ? 0 : C_CONST2)
    #endif
    #if defined(C_CONST1)
      | (missingp(STACK_2) ? 0 : C_CONST1)
    #endif
      ;
    skipSTACK(3);
    return ret;
  }
  it is convenient for parsing flag arguments to DEFUNs
- DEFCHECKER(c_name, [enum|type]=..., suffix= ..., prefix=..., default=...,
             bitmasks= ..., reverse= ..., delim= ...,
             C_CONST1 C_CONST2 C_CONST3)
  is converted to
  static struct { int c_const, gcv_object_t *l_const; } c_name_table[] = ...
  static [enum_]type c_name (object arg) {
    unsigned int index;
   restart_c_name:
    if (integerp(arg)) return I_to_L(arg);
    else if (missingp(arg)) return default;
    else {
      for (index = 0; index < c_name_table_size; index++)
        if (eq(a,*c_name_table[index].l_const))
          return c-name_table[index].c_const;
      pushSTACK(NIL); pushSTACK(arg);
      pushSTACK(the_appropriate_error_type);
      pushSTACK(the_appropriate_error_type); pushSTACK(arg);
      pushSTACK(TheSubr(subr_self)->name);
      check_value(type_error,GETTEXT("~S: ~S is not of type ~S"));
      arg = value1;
      goto restart_c_name;
    }
  }
  static object c_name_reverse (enum_type a) {
    unsigned int index;
    for (index = 0; index < c_name_table_size; index++)
      if (a == c_name_table[index].c_const)
        return *c_name_table[index].l_const;
    if (a == default) return NIL;
    ERROR; /* or "return reverse(a);" if reverse is supplied */
  }
 enum means no #ifdef
 prefix, suffix default to ""
 reverse defaults to "" and means ERROR
 bitmasks means additional *_to_list and *_from_list functions are defined
 delim defaults to "_" and separates prefix and suffix

Restrictions and caveats:
- A module should consist of a single file.
- #line lines should not be separated into multiple lines using
  backslash-newline.
- No multi-line comments should start in a preprocessor line.
- #if conditions are assumed to be constant from the DEFMODULE call to
  the end of the file. All necessary #define's should therefore be done
  before the DEFMODULE call.
- DEFUN and DEFVAR statements must take one CPP line
  (can be continued with backslashes):
     DEFUN (FOO:BAR,               ; BAD
             zot quux)
     DEFUN (FOO:BAR,             \
             zot quux)             ; GOOD

|#

(in-package "CL-USER")

(defvar *input-file*)
(defvar *lineno*)               ; current line position in file
(defvar *lines*)                ; list of all lines

(defun next-non-blank (line beg)
  (position-if-not #'sys::whitespacep line :start beg))
(defun prev-non-blank (line end)
  (let ((pos (position-if-not #'sys::whitespacep line :from-end t :end end)))
    (and pos (1+ pos))))
(defun next-blank (line beg) (position-if #'sys::whitespacep line :start beg))

(defun decode-directive (line)
  "return :if/:elif/:else/:endif/:line or nil;
 and the start of the first non-blank after the instruction"
  (declare (string line))
  (let ((pos (next-non-blank line 0)))
    (and pos (char= #\# (aref line pos))
         (setq pos (next-non-blank line (1+ pos)))
         (values (intern (subseq line pos (setq pos (next-blank line pos)))
                         #.(find-package "KEYWORD"))
                 (and pos (next-non-blank line pos))))))

(defun decode-line-directive (line)
  "Decode a #line directive. If the line represents a #line directive,
return the line number, else NIL."
  (declare (string line))
  (multiple-value-bind (directive pos) (decode-directive line)
    (when (eq :|line| directive)
      (parse-integer line :start pos :end (next-blank line pos)))))

(defstruct line number contents)

(defun read-all-input (stream)
  (loop :for in = (read-line stream nil nil) :and lineno :upfrom 1
    :while in
    :do (loop :for len = (length in)
          :while (and (/= 0 len) (char= #\\ (aref in (1- len))))
          :do (setf (aref in (1- len)) #\Space ; remove #\\
                    in (ext:string-concat in (read-line stream nil nil)))
          (incf lineno))
    :do (let ((n (decode-line-directive in)))
          (when (and n (/= n lineno))
            ;; (warn "fixed line number: ~S --> ~S" lineno n)
            (setq lineno n)))
    :collect (make-line :number lineno :contents in)))

(defun vector-starts-with (v0 v1)
  (and (>= (length v0) (length v1))
       (every #'string= v0 v1)))

;; Push elt, and optimize: elt can be removed if it starts with an
;; already present string sequence. If another element starts with elt,
;; that one can be removed
(defun stack-push-optimize (stack elt)
  (unless (some (lambda (vec) (vector-starts-with elt vec)) stack)
    (delete-if (lambda (vec) (vector-starts-with vec elt)) stack)
    (vector-push-extend elt stack))
  stack)

;; The #if[def] stack. All the conditions are implicitly combined by &&.
;; For every #if we start a new entry in the stack, which is popped when
;; we see the corresponding #endif. This is a stack of vector of string,
;; not a stack of string, because when a #elif is seen, we add an
;; element to the stack without popping the previous one.
(defvar *if-stack* (make-array 10 :adjustable t :fill-pointer 0))

(defun sharp-if (condition)
  (let ((vec (make-array 5 :adjustable t :fill-pointer 0)))
    (vector-push condition vec)
    (vector-push-extend vec *if-stack*)
    vec))
(defun sharp-else ()
  (let* ((top (aref *if-stack* (1- (length *if-stack*))))
         (pos (1- (length top))))
    (setf (aref top pos) (ext:string-concat "!(" (aref top pos) ")"))
    top))
(defun sharp-elif (condition)
  (vector-push-extend condition (sharp-else)))
(defun sharp-endif () (vector-pop *if-stack*))

(defun current-condition ()
  "Returns the current #if condition.
It is a vector of strings, implicitly combined by &&.
The vector is freshly constructed, but the strings are shared"
  (let ((vec (make-array 5 :adjustable t :fill-pointer 0)))
    (loop :for cc :across *if-stack* :do
      (dotimes (ii (length cc))
        (vector-push-extend (aref cc ii) vec)))
    vec))

(defun string-rest (line pos)
  (unless pos
    (error "~S:~D: incomplete instruction ~S" *input-file* *lineno* line))
  (subseq line pos (prev-non-blank line nil)))

(defun if-p (line)
  "#if/#ifdef/#ifndef"
  (declare (string line))
  (multiple-value-bind (directive pos) (decode-directive line)
    (case directive
      ;; FIXME: what about a comment starting on a CPP line?!
      (:|if| (string-rest line pos))
      (:|ifndef| (ext:string-concat "!defined(" (string-rest line pos) ")"))
      (:|ifdef| (ext:string-concat "defined(" (string-rest line pos) ")")))))

(defun else-p (line) (eq :|else| (decode-directive line)))

(defun elif-p (line)
  (multiple-value-bind (directive pos) (decode-directive line)
    (and (eq :|elif| directive) (string-rest line pos))))

(defun endif-p (line) (eq :|endif| (decode-directive line)))

;; DEFMODULE(name,package)
(defvar *module-name*)
(defvar *module-line*)
(defvar *module-package*)
(defvar *module-all-packages*)
(defvar *init-1-name* nil "Did the module define its own init1?")
(defvar *init-2-name* nil "Did the module define its own init2?")
(defvar *fini-name* nil "Did the module define its own fini?")

(defconstant *commands*
  '("DEFMODULE" "DEFUN" "DEFVAR" "DEFCHECKER" "DEFFLAGSET"))

(defun split-option (argument)
  "foo=bar ==> (:foo bar)"
  (let ((= (position #\= argument)))
    (if =
        (list (intern (nstring-upcase
                       (subseq argument 0 (prev-non-blank argument =)))
                      #.(find-package "KEYWORD"))
              (let ((start (next-non-blank argument (1+ =))))
                (if start (subseq argument start) T)))
        argument)))
(defun split-command (line &key (start 0) (end (length line)))
  "parse command line into command name and arguments:
FOO(bar,baz,zot) ==> FOO; (bar baz zot); end-position"
  (setq start (next-non-blank line start))
  (unless start (return-from split-command nil)) ; blank line
  (setq end (position #\) line :start start :end end :from-end t))
  (let* ((last (or (prev-non-blank line end)         ;nothing before #\)
                   (return-from split-command nil))) ; => no command
         (paren (position #\( line :start start :end last)) args
         (name (subseq line start
                       (or (prev-non-blank line paren) ; nothing before #\(
                           (return-from split-command nil))))) ; => no command
    (unless (member name *commands* :test #'string=)
      (return-from split-command nil))
    ;; valid command in LINE
    (unless end (error "~S:~D: no closing paren in ~S[~:D;~:D]"
                       *input-file* *lineno* line start end))
    (unless paren (error "~S:~D: no opening paren in ~S[~:D;~:D]"
                         *input-file* *lineno* line start last))
    (setq start (next-non-blank line (1+ paren)))
    (do ((comma (position #\, line :end last :start start)))
        ((null comma) (push (subseq line start last) args))
      (push (subseq line start (prev-non-blank line comma)) args)
      (setq start (next-non-blank line (1+ comma))
            comma (position #\, line :end last :start (1+ comma))))
    (values name (nreverse args) (1+ end))))
(defun string-object-p (obj)
  "check whether the object is a string"
  (let ((len (1- (length obj))))
    (and (char= #\" (aref obj 0))
         (char= #\" (aref obj len)))))
(defun extract-argument-string (arg)
  (if (string-object-p arg)
      (subseq arg 1 (1- (length arg)))
      (error "~S:~D: expected a string, got ~A" *input-file* *lineno* arg)))

(defun defmodule-p (line)
  (multiple-value-bind (name args) (split-command line)
    (unless (string= "DEFMODULE" name) (return-from defmodule-p nil))
    (assert (= 2 (length args)) () "~S: requires 2 arguments, got ~S"
            name args)
    (setq *module-name* (first args)
          *init-1-name* (format nil "module__~A__init_function_1"
                                    *module-name*)
          *init-2-name* (format nil "module__~A__init_function_2"
                                *module-name*)
          *fini-name* (format nil "module__~A__fini_function"
                              *module-name*)
          *module-package* (extract-argument-string (second args))
          *module-all-packages* (list *module-package*))
    (values *module-name* *module-package*)))

(defstruct objdef
  ;; init is either a string or a list of either text-strings or pairs
  ;; (condition-string . text-string)
  init tag
  (cond-stack (make-array 5 :adjustable t :fill-pointer 0)))
(defvar *objdefs* (make-array 10 :adjustable t :fill-pointer 0))
(defun tag-to-objdef (tag)
  (find tag *objdefs* :test #'string= :key #'objdef-tag))
(defun write-string-c-style (string out)
  (loop :for cc :across string :and ii :upfrom 0
    :do (cond ((alphanumericp cc) (write-char (char-downcase cc) out))
              ((char= cc #\:) (write-char (if (zerop ii) #\K #\_) out))
              ((or (char= cc #\_) (char= cc #\-)) (write-char #\_ out))
              (t (format out "_~2,'0x" (char-code cc))))))

(defun init-to-tag (init already-present-p &optional (prefix "object_"))
  (let ((base
         (with-output-to-string (s)
           (when prefix (write-string prefix s))
           (etypecase init
             (string (write-string-c-style init s))
             (list (dolist (el init)
                     (write-char #\_ s)
                     (etypecase el
                       (string (write-string-c-style el s))
                       (cons (write-string-c-style (car el) s)
                             (write-char #\_ s)
                             (write-string-c-style (cdr el) s)))))))))
    (when (funcall already-present-p base)
      (loop :for ii :upfrom 0 :for new = (format nil "~a_~d" base ii)
        :while (funcall already-present-p new)
        :finally (setq base new)))
    base))

(defun string-upcase-verbose (string)
  (when (and (some #'lower-case-p string)    ; upcasing will modify
             (not (string-object-p string))) ; this is not a string object
    (warn "~S:~D: fixed object case ~S" *input-file* *lineno* string)
    (setq string (string-upcase string)))
  string)

(defun new-objdef (init)
  (etypecase init
    (string (setq init (string-upcase-verbose init)))
    (cons (loop :for tail :on init :do
            (etypecase (car tail)
              (string (setf (car tail) (string-upcase-verbose (car tail))))
              (cons (setf (cdar tail) (string-upcase-verbose (cdar tail))))))))
  (let ((od (make-objdef :init init :tag (init-to-tag init #'tag-to-objdef))))
    (vector-push-extend od *objdefs*)
    od))

(defun init-to-objdef (init &optional (condition (current-condition)))
  "Looks up or creates an Objdef for a given initstring"
  (let ((odef (or (find init *objdefs* :test #'equal :key #'objdef-init)
                  (new-objdef init))))
    (stack-push-optimize (objdef-cond-stack odef) condition)
    odef))

(defconstant *seclass*
  #("seclass_foldable" "seclass_no_se" "seclass_read"
    "seclass_write" "seclass_default")
  "must be in sync with src/lispbibl.d:seclass_t")
(defstruct signature
  (seclass (1- (length *seclass*)))
  req opt rest-p key-p keywords
  (cond-stack (make-array 5 :adjustable t :fill-pointer 0)))

(defconstant *valid-signatures*
  (vector
   (make-signature :req 0 :opt 0)
   (make-signature :req 1 :opt 0)
   (make-signature :req 2 :opt 0)
   (make-signature :req 3 :opt 0)
   (make-signature :req 4 :opt 0)
   (make-signature :req 5 :opt 0)
   (make-signature :req 6 :opt 0)
   (make-signature :req 0 :opt 1)
   (make-signature :req 1 :opt 1)
   (make-signature :req 2 :opt 1)
   (make-signature :req 3 :opt 1)
   (make-signature :req 4 :opt 1)
   (make-signature :req 0 :opt 2)
   (make-signature :req 1 :opt 2)
   (make-signature :req 2 :opt 2)
   (make-signature :req 3 :opt 2)
   (make-signature :req 0 :opt 3)
   (make-signature :req 1 :opt 3)
   (make-signature :req 2 :opt 3)
   (make-signature :req 0 :opt 4)
   (make-signature :req 0 :opt 5)
   (make-signature :req 0 :opt 0 :rest-p t)
   (make-signature :req 1 :opt 0 :rest-p t)
   (make-signature :req 2 :opt 0 :rest-p t)
   (make-signature :req 3 :opt 0 :rest-p t)
   (make-signature :req 0 :opt 0 :key-p t)
   (make-signature :req 1 :opt 0 :key-p t)
   (make-signature :req 2 :opt 0 :key-p t)
   (make-signature :req 3 :opt 0 :key-p t)
   (make-signature :req 4 :opt 0 :key-p t)
   (make-signature :req 0 :opt 1 :key-p t)
   (make-signature :req 1 :opt 1 :key-p t)
   (make-signature :req 1 :opt 2 :key-p t))
  "must be in sync with src/lispbibl.d:subr_argtype_t")

(defvar *must-close-next-defun* nil
  "set to T when emulating the signature")
(defvar *in-defun* nil "set to T when entering a defun")
(defvar *emulation-count* 0)

(defun parse-signature (fname line &key (start 0) (end (length line)))
  (unless end
    (error "~S:~D: unterminated signature ~S" *input-file* *lineno* line))
  (loop :with seen-opt :and seen-key :and seen-rest :and keys
    :and opt = 0 :and req = 0 :and pos2 = start
    :for pos1 = (next-non-blank line pos2)
    :while (and pos1 (< pos1 end))
    :do (setq pos2 (min end (or (next-blank line pos1) end)))
    (cond ((string-equal line "&optional" :start1 pos1 :end1 pos2)
           (when (or seen-opt seen-key seen-rest)
             (error "~S:~D: bad signature ~S between ~S and ~S"
                    *input-file* *lineno* line pos1 pos2))
           (setq seen-opt t))
          ((string-equal line "&key" :start1 pos1 :end1 pos2)
           (when (or seen-key seen-rest)
             (error "~S:~D: bad signature ~S between ~S and ~S"
                    *input-file* *lineno* line pos1 pos2))
           (setq seen-key t))
          ((string-equal line "&rest" :start1 pos1 :end1 pos2)
           (when (or seen-key seen-rest)
             (error "~S:~D: bad signature ~S between ~S and ~S"
                    *input-file* *lineno* line pos1 pos2))
           (setq seen-rest 0))
          (seen-rest
           (if (zerop seen-rest) (setq seen-rest t)
               (error "~S:~D: bad signature ~S between ~S and ~S"
                      *input-file* *lineno* line pos1 pos2)))
          (seen-key
           (push (init-to-objdef (if (char= (aref line pos1) #\:)
                                     (subseq line pos1 pos2)
                                     (ext:string-concat
                                      ":" (subseq line pos1 pos2))))
                 keys))
          (seen-opt (incf opt))
          ((incf req)))
    :finally (return (check-signature fname
                      (make-signature
                       :req req :opt opt :rest-p seen-rest
                       :key-p seen-key :keywords (nreverse keys))))))

(defun check-signature (fname sig)
  (if (find sig *valid-signatures* :test #'signature-match)
    (values sig nil)
    (values (load-time-value (make-signature :req 0 :opt 0 :rest-p t))
      (with-output-to-string (out) ; emulate signature
        (warn "~S:~D:~A: emulating signature (~A ~A~:[~; &rest~]~:[~; &key~])"
              *input-file* *lineno* fname
              (signature-req sig) (signature-opt sig)
              (signature-rest-p sig) (signature-key-p sig))
        (incf *emulation-count*)
        (when (signature-rest-p sig) ; why?!
          (error "~S:~D:~A: cannot emulate &rest" *input-file* *lineno* fname))
        (let* ((min-arg (signature-req sig))
               (req+opt (+ min-arg (signature-opt sig)))
               (max-arg
                (unless (or (signature-rest-p sig) (signature-key-p sig))
                  req+opt))
               (kwds (signature-keywords sig)) (n-kwds (length kwds))
               (kwd-list
                 (with-output-to-string (kwd-s)
                   (write-char #\( kwd-s)
                   (loop :for k :in kwds :and firstp = t :then nil :do
                     (unless firstp (write-char #\Space kwd-s))
                     ;; cannot have conditionals in this init
                     (write-string (objdef-init k) kwd-s))
                   (write-char #\) kwd-s))))
          (format out "{ if (argcount < ~D) { pushSTACK(TheSubr(subr_self)->name); fehler(source_program_error,GETTEXT(\"EVAL/APPLY: too few arguments given to ~~S\")); } " min-arg)
          (when max-arg (format out "if (argcount > ~D) { pushSTACK(TheSubr(subr_self)->name); fehler(source_program_error,GETTEXT(\"EVAL/APPLY: too many arguments given to ~~S\")); } " max-arg))
          (unless (zerop (signature-opt sig)) (format out "for (;argcount < ~D; argcount++) pushSTACK(unbound); " req+opt))
          (when (signature-key-p sig)
            (format out "if ((argcount-~D)%2) fehler_key_odd(argcount,TheSubr(subr_self)->name); " req+opt)
            (when (zerop n-kwds) (warn "~A: &key without any keywords" fname))
            (format out "{ uintC i; skipSTACK((-~D)); ~
  for (i = 0; i<argcount-~D; i++) STACK_(i) = STACK_(i+~D); "
                    n-kwds req+opt n-kwds)
            (dotimes (i n-kwds)
              (format out "STACK_(argcount-~D+~D) = unbound; " req+opt i))
            (format out "for (i = argcount-~D; i > 0; i -= 2) " req+opt)
            (loop :for k :in kwds :and i :upfrom 0 :do
              (format out "~[~:;else ~]if (eq (STACK_(i-1),O(~A))) ~
 STACK_(argcount-~D+~D) = STACK_(i-2); "
                      i (objdef-tag k) req+opt (- n-kwds i 1)))
            (format out "else fehler_key_badkw(TheSubr(subr_self)->name,STACK_(i-1),STACK_(i-2),O(~A)); skipSTACK(argcount-~D); }"
                    (objdef-tag (init-to-objdef kwd-list)) req+opt))
          (setq *must-close-next-defun* t))))))

(defun signature-match (s1 s2)
  (and (= (signature-req s1) (signature-req s2))
       (= (signature-opt s1) (signature-opt s2))
       (eq (signature-rest-p s1) (signature-rest-p s2))
       (eq (signature-key-p s1) (signature-key-p s2))))

(defun signature= (s1 s2)
  (and (signature-match s1 s2)
       (= (signature-seclass s1) (signature-seclass s2))
       (equal (signature-keywords s1) (signature-keywords s2))))

(defstruct fundef
  pack      ;; The symbol's package name
  name      ;; The symbol's print name
  tag       ;; The struct tag we use for this function
  ;; The total #if condition of this function.
  (cond-stack (make-array 5 :adjustable t :fill-pointer 0))
  signatures;; The function's possible signatures,
  ;; together with their individual #if conditions.
)
(defvar *fundefs* (make-array 10 :adjustable t :fill-pointer 0))
(defun tag-to-fundef (tag)
  (find tag *fundefs* :test #'string= :key #'fundef-tag))

(defun new-fundef (name pack)
  (let ((fd (make-fundef :pack pack :name name :tag
                         (init-to-tag (ext:string-concat pack ":" name)
                                      #'tag-to-fundef "subr_"))))
    (when (some #'lower-case-p pack)
      (warn "~S:~D: fixed package case: ~S" *input-file* *lineno* pack)
      (setq pack (string-upcase pack)))
    (unless (every (lambda (c) (or (not (alpha-char-p c)) (upper-case-p c)))
                   name)
      (warn "~S:~D: fixed function name case ~S" *input-file* *lineno* name)
      (setq name (string-upcase name)))
    (pushnew pack *module-all-packages* :test #'string=)
    (vector-push-extend fd *fundefs*)
    fd))

(defun find-fundef (funname &optional (condition (current-condition)))
  "find the FDEF object corresponding to the given FUNNAME or create a new one"
  (let ((pack *module-package*) (name funname) (pos (position #\: funname)))
    (when pos
      (setq pack (subseq funname 0 pos)) (incf pos)
      (when (char= #\: (aref funname pos)) (incf pos))
      (when (find #\: funname :start pos)
        (error "~S:~D: too many package markers in ~S"
               *input-file* *lineno* funname))
      (setq name (subseq funname pos)))
    (unless pack
      (error "~S:~D: function name ~S needs a package prefix"
             *input-file* *lineno* funname))
    (let ((fdef (or (find-if (lambda (fd)
                               (and (string= name (fundef-name fd))
                                    (string= pack (fundef-pack fd))))
                             *fundefs*)
                    (new-fundef name pack))))
      (stack-push-optimize (fundef-cond-stack fdef) condition)
      fdef)))

(defun funname-to-fundef (funname sig &optional
                          (condition (current-condition)))
  (let* ((fdef (find-fundef funname condition))
         (sig-old (find sig (fundef-signatures fdef) :test #'signature=)))
    (if sig-old
        (stack-push-optimize (signature-cond-stack sig-old) condition)
        (progn
          (stack-push-optimize (signature-cond-stack sig) condition)
          (push sig (fundef-signatures fdef))))
    fdef))

(defun fundef-lispfun (fundef sig)
  "Print a signature in a form suitable as argument list for LISPFUN."
  (format nil "(~a,~a,~d,~d,~:[no~;~]rest,~:[no~;~]key,~d,NIL)"
          (fundef-tag fundef) (aref *seclass* (signature-seclass sig))
          (signature-req sig) (signature-opt sig)
          (signature-rest-p sig) (signature-key-p sig)
          (length (signature-keywords sig))))

(defvar *brace-depth* 0)
(defun parse-name (line end form &aux pos comma)
  "parse the LINE DEF...(name,...), END is the end of DEF...
Return the position of the first comma, position of the closing paren,
 and the name of the form."
  (unless (zerop *brace-depth*)
    (error "~S:~D: ~A must be at the top level (depth ~D): ~S"
           *input-file* *lineno* form *brace-depth* line))
  (setq pos (next-non-blank line end))
  (unless (and pos (char= #\( (aref line pos)))
    (error "~S:~D:~D: invalid ~A syntax in ~S"
           *input-file* *lineno* pos form line))
  (unless (setq comma (position #\, line :start pos))
    (error "~S:~D: too few arguments to ~A in ~S"
           *input-file* *lineno* form line))
  (values comma (position #\) line :start comma)
          (subseq line (next-non-blank line (1+ pos))
                  (prev-non-blank line comma))))

(defun defun-p (line)
  "Parse a DEFUN(funname,lambdalist) line,
and turn it into DEFUN(funname,lambdalist,signature)."
  (let* ((pos (next-non-blank line 0)) (sec "seclass_default") cc sig fname
         (len (length line)) (end (and pos (+ pos #.(length "DEFUN")))) comma)
    (when (and pos (< end len) (string= "DEFUN" line :start2 pos :end2 end)
               (case (setq cc (aref line end))
                 (#\F (setq sec "seclass_foldable") (incf end))
                 (#\N (setq sec "seclass_no_se") (incf end))
                 (#\R (setq sec "seclass_read") (incf end))
                 (#\W (setq sec "seclass_write") (incf end))
                 (#\D (setq sec "seclass_default") (incf end))
                 (#\( t)
                 (t (sys::whitespacep cc))))
      (multiple-value-setq (comma end fname) (parse-name line end "DEFUN"))
      (multiple-value-setq (sig cc)
        (parse-signature fname line :start (1+ comma) :end end))
      (let* ((rest (subseq line end))
             (all (ext:string-concat
                   (subseq line 0 end) ","
                   (fundef-lispfun (funname-to-fundef fname sig) sig)
                   (or cc "") rest)))
        (values all (- (length all) (length rest)))))))

(defstruct cpp-helper
  name cpp-names
  (cond-stack (make-array 5 :adjustable t :fill-pointer 0)))

(defstruct (flag-set (:include cpp-helper)))
(defvar *flag-sets* (make-array 5 :adjustable t :fill-pointer 0))
(defun new-flag-set (name cpp-names &key (condition (current-condition)))
  ;; must nreverse cpp-names to match the order of keywords
  (let ((fs (make-flag-set :name name :cpp-names (nreverse cpp-names))))
    (vector-push-extend fs *flag-sets*)
    (stack-push-optimize (flag-set-cond-stack fs) condition)
    fs))

;; type is the enum type name (if it is an enum typedef) and NIL otherwise
;; since enum constants cannot be checked by CPP, we do not ifdef them
(defstruct (checker (:include cpp-helper))
  enum-p type prefix suffix delim reverse bitmasks default cpp-odefs type-odef)
(defvar *checkers* (make-array 5 :adjustable t :fill-pointer 0))
(defun to-C-name (name prefix suffix delim)
  (etypecase name
    (string
     (setq name (substitute #\_ #\- name))
     (when prefix (setq name (ext:string-concat prefix delim name)))
     (when suffix (setq name (ext:string-concat name delim suffix))))
    (cons (setq name (second name))))
  name)
(defun new-checker (name cpp-names &key type prefix suffix reverse default enum
                    bitmasks (delim "_") (condition (current-condition)))
  "NAME is the name of the checker function
CPP-NAMES is the list of possible values, either strings or
 pairs (:KEYWORD VALUE), value is not #ifdef'ed"
  (setq default (and (not (eq default T)) default
                     (or (parse-integer default :junk-allowed t) default))
        bitmasks (and (not (eq bitmasks T)) bitmasks)
        delim (if (eq delim T) "" delim))
  (when (and type enum)
    (error "~S:~D:~S(~S): cannot specify both ~A=~S and ~A=~S"
           *input-file* *lineno* 'new-checker name :type type :enum enum))
  (let ((ch (make-checker :type (or type enum) :name name :reverse reverse
                          :prefix prefix :suffix suffix :bitmasks bitmasks
                          :enum-p (not (null enum)) :cpp-names cpp-names
                          :delim delim :default default))
        (type-odef (if default
                       ;; note that if DEFAULT is an undefined CPP constant
                       ;; NIL will not be a valid argument
                       "(OR NULL INTEGER (MEMBER"
                       "(OR INTEGER (MEMBER"))
        cpp-odefs)
    (vector-push-extend ch *checkers*)
    (stack-push-optimize (checker-cond-stack ch) condition)
    (cond (enum
           (dolist (name cpp-names)
             (push (init-to-objdef (ext:string-concat ":" name) condition)
                   cpp-odefs)
             (setq type-odef (ext:string-concat type-odef " :" name)))
           (setf (checker-type-odef ch)
                 (init-to-objdef (ext:string-concat type-odef "))"))))
          (t
           (setq type-odef (list type-odef))
           (dolist (name cpp-names)
             (etypecase name
               (string
                (let ((co (ext:string-concat
                           "defined(" (to-C-name name prefix suffix delim)
                           ")")))
                  (push (init-to-objdef (ext:string-concat ":" name)
                                        (concatenate 'vector condition
                                                     (list co)))
                        cpp-odefs)
                  (push (cons co (ext:string-concat " :" name)) type-odef)))
               (cons
                (let ((nm (symbol-name (car name))))
                  (push (init-to-objdef (ext:string-concat ":" nm) condition)
                        cpp-odefs)
                  (push (ext:string-concat " :" nm) type-odef)))))
           (setf (checker-type-odef ch)
                 (init-to-objdef (nreconc type-odef (list "))"))))))
    (setf (checker-cpp-odefs ch) (nreverse cpp-odefs))
    ch))

(defun word-list (line)
  (loop :with len = (length line) :with pos2 = -1
    :for pos1 = (and (< pos2 len) (next-non-blank line (1+ pos2)))
    :while (and pos1 (< pos1 len))
    :do (setq pos2 (min len (or (next-blank line pos1) len)))
    :collect (split-option (subseq line pos1 pos2))))
(defun def-something-p (line command-alist)
  "Parse a COMMAND(c_name,[options]CPP_CONST...) line."
  (multiple-value-bind (name args last constructor) (split-command line)
    (setq constructor (cdr (assoc name command-alist :test #'string=)))
    (unless constructor (return-from def-something-p nil))
    (apply constructor (first args) (word-list (car (last args)))
           (mapcan #'split-option (cdr (nbutlast args))))
    (subseq line last)))

(defstruct vardef
  tag (cond-stack (make-array 5 :adjustable t :fill-pointer 0)))
(defvar *vardefs* (make-array 10 :adjustable t :fill-pointer 0))
(defun tag-to-vardef (tag)
  (find tag *vardefs* :test #'string= :key #'vardef-tag))
(defun varname-to-vardef (varname &optional (condition (current-condition)))
  (let ((vdef (or (tag-to-vardef varname)
                  (let ((vd (make-vardef :tag varname)))
                    (vector-push-extend vd *vardefs*)
                    vd))))
    (stack-push-optimize (vardef-cond-stack vdef) condition)
    vdef))

;; Variable initializers. (We treat them separately from the variables
;; themselves, so that they are executed in order.)
(defstruct varinit tag init condition)
(defvar *varinits* (make-array 10 :adjustable t :fill-pointer 0))

(defun lexical-analysis (line &key (start 0) (end (length line)) in-comment)
  "Return the new line (with substitutions expanded) and the end position.
Also return status: NIL for parsing until the end and
  ';' when a comment was started but not ended;
  ')' when a closing parenthesis was encountered;
  ',' when a comma was encountered."
  (do ((paren-depth 0) in-string in-char in-subst subst-start in-func
       (ii start (1+ ii)) cc (done nil))
      ((or done (>= ii end))
       (when (= ii end)
         (when in-string
           (error "~S:~D: string not terminated: ~S"
                  *input-file* *lineno* line))
         (when in-char
           (error "~S:~D: character not terminated: ~S"
                  *input-file* *lineno* line))
         (when in-subst
           (error "~S:~D: backquote not terminated: ~S"
                  *input-file* *lineno* line)))
       (values line (1- ii) (cond (done done) (in-comment #\;))))
    (setq cc (aref line ii))
    (cond ((and (not in-comment) (not in-char) (not in-string) (not in-subst)
                (char= cc #\())
           (incf paren-depth))
          ((and (not in-comment) (not in-char) (not in-string) (not in-subst)
                (char= cc #\{))
           (when *must-close-next-defun* (setq *in-defun* t))
           (incf *brace-depth*))
          ((and (not in-comment) (not in-char) (not in-string) (not in-subst)
                (char= cc #\}))
           (decf *brace-depth*))
          ((and (not in-comment) (not in-char) (not in-string) (not in-subst)
                (zerop paren-depth) (char= cc #\,))
           (setq done #\,))
          ((and (not in-comment) (not in-char) (not in-string) (not in-subst)
                (char= cc #\)))
           (if (plusp paren-depth)
               (decf paren-depth)
               (setq done #\))))
          ((and (not in-char) (not in-string) (not in-subst)
                (< (1+ ii) end) (char= cc #\/)
                (char= (aref line (1+ ii)) #\*))
           (when in-comment
             (error "~S:~D: nested comments in ~S at ~D"
                    *input-file* *lineno* line ii))
           (setq in-comment t)
           (incf ii))
          ((and (not in-char) (not in-string) (not in-subst)
                (< (1+ ii) end) (char= cc #\*)
                (char= (aref line (1+ ii)) #\/))
           (unless in-comment
             (error "~S:~D: comment terminator outside comment in ~S at ~D"
                    *input-file* *lineno* line ii))
           (setq in-comment nil)
           (incf ii))
          ((and (not in-comment) (not in-char) (not in-subst)
                (char= cc #\"))
           (setq in-string (not in-string)))
          ((and (not in-comment) (not in-string) (not in-subst)
                (char= cc #\'))
           (setq in-char (not in-char)))
          ((and (not in-comment) (not in-string) (not in-char)
                (char= cc #\`))
           (if in-subst
               (if (= subst-start (1- ii))
                   (if in-func
                       (error "~S:~D: too many backquotes in ~S at ~D"
                              *input-file* *lineno* line ii)
                       (setq in-func t subst-start ii))
                   (if in-func
                       (let* ((id (subseq line (1+ subst-start) ii))
                              (def (find-fundef id))
                              (tag (fundef-tag def))
                              (tlen (1- (length tag))))
                         (if (and (< (1+ ii) end)
                                  (char= (aref line (1+ ii)) #\`))
                             (incf ii)
                             (error "~S:~D: bad SUBR reference in ~S at ~D"
                                    *input-file* *lineno* line ii))
                         (setq line (ext:string-concat
                                     (subseq line 0 (1- subst-start))
                                     "F(" tag ")" (subseq line (1+ ii)))
                               end (+ end tlen (- subst-start ii) 3 -1)
                               ii (+ subst-start tlen 3)
                               subst-start nil in-subst nil))
                       (let* ((id (subseq line (1+ subst-start) ii))
                              (def (init-to-objdef id))
                              (tag (objdef-tag def))
                              (tlen (1- (length tag))))
                         (setq line (ext:string-concat
                                     (subseq line 0 subst-start)
                                     "O(" tag ")" (subseq line (1+ ii)))
                               end (+ end tlen (- subst-start ii) 3)
                               ii (+ subst-start tlen 3)
                               subst-start nil in-subst nil))))
               (setq in-subst t subst-start ii))))))

(defun defvar-p (line)
  "Parse a DEFVAR(varname,initform) line, and turn it into DEFVAR(varname).
We remove the initform because it might contain backquoted stuff including
commas and parentheses."
  (let* ((pos (next-non-blank line 0)) cc vdef
         (len (length line)) (end (and pos (+ pos #.(length "DEFVAR")))) comma)
    (when (and pos (< end len) (string= "DEFVAR" line :start2 pos :end2 end)
               (or (char= (setq cc (aref line end)) #\()
                   (sys::whitespacep cc)))
      (setq pos (next-non-blank line end))
      (unless (and pos (char= #\( (aref line pos)))
        (error "~S:~D: invalid DEFVAR syntax in ~S"
               *input-file* *lineno* line))
      (unless (setq comma (position #\, line :start pos))
        (error "~S:~D: too few arguments to DEFVAR in ~S"
               *input-file* *lineno* line))
      (multiple-value-setq (line end cc)
        (lexical-analysis line :start (1+ comma) :end len))
      (cond ((eql cc #\;)
             (error "~S:~D: comment not terminated: ~S"
                    *input-file* *lineno* line))
            ((eql cc #\,)
             (error  "~S:~D: too many arguments to DEFVAR in ~S"
                     *input-file* *lineno* line))
            ((not (eql cc #\)))
             (error "~S:~D: DEFMACRO not terminated: ~S"
                    *input-file* *lineno* line)))
      (setq vdef (varname-to-vardef
                  (subseq line (next-non-blank line (1+ pos))
                          (prev-non-blank line comma))))
      (vector-push-extend
       (make-varinit :tag (vardef-tag vdef) :condition (current-condition)
                     :init (subseq line (next-non-blank line (1+ comma)) end))
       *varinits*)
      ;; return value from defvar-p: DEFVAR(varname)
      (ext:string-concat (subseq line 0 comma) (subseq line end)))))

(defun parse (&optional *lines*)
  "Parse the entire input"
  (loop :with in-comment :and condition :and status
    :for ln :in *lines* :and idx :upfrom 0 :and end = -1
    :for line = (line-contents ln) :do (setq *lineno* (line-number ln))
    (unless in-comment
      (when (defmodule-p line) (setq *module-line* idx))
      (when (setq condition (if-p line)) (sharp-if condition))
      (when (else-p line) (sharp-else))
      (when (setq condition (elif-p line)) (sharp-elif condition))
      (when (endif-p line) (sharp-endif))
      (setq line (or (defvar-p line)
                     (def-something-p line
                         `(("DEFFLAGSET" . ,#'new-flag-set)
                           ("DEFCHECKER" . ,#'new-checker)))
                     line))
      (multiple-value-bind (l p) (defun-p line)
        (when l (setq line l end (1- p)))))
    (loop (multiple-value-setq (line end status)
            (lexical-analysis line :start (1+ end) :in-comment in-comment))
      (setq in-comment (eql status #\;))
      (when (and *must-close-next-defun* *in-defun* (= *brace-depth* 0))
        (setq line (ext:string-concat line "}")
              *must-close-next-defun* nil *in-defun* nil))
      (when (and *init-1-name* (search *init-1-name* line)
                 (not (search "__modprep" *init-1-name*)))
        ;; module defines its own init1
        ;; the user is expected to call our init1 from his
        (setq *init-1-name* (ext:string-concat *init-1-name* "__modprep")))
      (when (and *init-2-name* (search *init-2-name* line))
        (setq *init-2-name* nil)) ; module defines its own init2
      (when (and *fini-name* (search *fini-name* line))
        (setq *fini-name* nil)) ; module defines its own fini-func
      (when (or (null status) (eql status #\;)) (return)))
    (setf (line-contents ln) line)))

;; *** output ***

(defun print-condition-part (out condition)
  "Print a list (cond1 && cond2 && ...) to a stream"
  (case (length condition)
    (0 (write-char #\1 out))
    (1 (write (aref condition 0) :stream out :escape nil))
    (t (dotimes (i (length condition))
         (when (plusp i) (write-string " && " out))
         (format out "(~A)" (aref condition i))))))

(defun print-condition-whole (out cond-st)
  "Print a list of lists (cond1 || cond2 || ...) to a stream"
  (case (length cond-st)
    (0 (write-char #\0 out))
    (1 (print-condition-part out (aref cond-st 0)))
    (t (dotimes (i (length cond-st))
         (when (plusp i) (write-string " || " out))
         (write-char #\( out)
         (print-condition-part out (aref cond-st i))
         (write-char #\) out)))))

(declaim (inline condition-part-true condition-stack-false newline))
(defun condition-part-true (condition) (zerop (length condition)))
(defun condition-stack-false (cond-st) (zerop (length cond-st)))
(defun condition-stack-true (cond-st) (every #'condition-part-true cond-st))
(defun newline (out) (terpri out) (incf *lineno*))
(defmacro formatln (out string &rest args)
  `(progn (format ,out ,string ,@args) (newline ,out)))

(defmacro with-conditional ((out cond-stack) &body body)
  (let ((cs (gensym "WC-")) (always-true (gensym "WC-")))
    `(let ((,cs ,cond-stack))
       (unless (condition-stack-false ,cs)
         (let ((,always-true (condition-stack-true ,cs)))
           (unless ,always-true
             (write-string "#if " ,out)
             (print-condition-whole ,out ,cs)
             (newline ,out))
           ,@body
           (newline ,out)
           (unless ,always-true
             (formatln ,out "#endif")))))))

(defun print-tables-1 (out)
  "Output the tables just after the DEFMODULE line"
  (let ((object-tab-initdata
         (ext:string-concat "module__" *module-name* "__object_tab_initdata"))
        (object-tab
         (ext:string-concat "module__" *module-name* "__object_tab"))
        (subr-tab (ext:string-concat "module__" *module-name* "__subr_tab")))
    (newline out)
    (formatln out "#define O(varname) ~a._##varname" object-tab)
    (formatln out "#define F(varname) subr_tab_ptr_as_object(&(~a._##varname))"
              subr-tab)
    (newline out)
    (formatln out "struct ~A_t {" object-tab)
    (setq *objdefs* (sort *objdefs* #'string-lessp :key #'objdef-tag)
          *fundefs* (sort *fundefs* #'string-lessp :key #'fundef-tag))
    (loop :for od :across *objdefs*
      :do (with-conditional (out (objdef-cond-stack od))
            (format out "  gcv_object_t _~A;" (objdef-tag od))))
    (loop :for vd :across *vardefs*
      :do (with-conditional (out (vardef-cond-stack vd))
            (format out "  gcv_object_t _~A;" (vardef-tag vd))))
    (formatln out "} ~A;" object-tab)
    (formatln out "uintC ~A_size = sizeof(~A)/sizeof(gcv_object_t);"
              object-tab object-tab)
    (newline out)
    (formatln out "struct ~A_t {" object-tab-initdata)
    (loop :for od :across *objdefs*
      :do (with-conditional (out (objdef-cond-stack od))
            (format out "  object_initdata_t _~A;" (objdef-tag od))))
    (loop :for vd :across *vardefs*
      :do (with-conditional (out (vardef-cond-stack vd))
            (format out "  object_initdata_t _~A;"  (vardef-tag vd))))
    (formatln out "  int _dummy_to_avoid_trailing_comma_in_initializer;")
    (formatln out "} ~A = {" object-tab-initdata)
    (loop :for od :across *objdefs*
      :do (with-conditional (out (objdef-cond-stack od))
            (let ((init (objdef-init od)))
              (etypecase init
                (string (format out "  { ~S }," init))
                (cons (format out "  {")
                      (dolist (el init)
                        (etypecase el
                          (string (formatln out " ~S" el))
                          (cons (formatln out "#  if ~A" (car el))
                                (formatln out "    ~S" (cdr el))
                                (formatln out "#  endif") )))
                      (format out "  },"))))))
    (loop :for vd :across *vardefs*
      :do (with-conditional (out (vardef-cond-stack vd))
            (write-string "  { \"NIL\" }," out)))
    (formatln out "  0")
    (formatln out "};") (newline out)
    (formatln out "struct ~A_t {" subr-tab)
    (formatln out "  VAROBJECTS_ALIGNMENT_DUMMY_DECL")
    (loop :for fd :across *fundefs*
      :do (with-conditional (out (fundef-cond-stack fd))
            (format out "  subr_t _~A;" (fundef-tag fd))))
    (formatln out "  int _dummy_to_avoid_trailing_comma_in_initializer;")
    (formatln out "};")
    (formatln out "extern struct ~A_t ~A;" subr-tab subr-tab)
    (newline out)
    (loop :for fs :across *flag-sets*
      :do (with-conditional (out (flag-set-cond-stack fs))
            (formatln out "static uintL ~A (void) {" (flag-set-name fs))
            (formatln out "  uintL flags = 0")
            (loop :for cpp-name :in (flag-set-cpp-names fs) :for nn :upfrom 0
              :do (formatln out "#  ifdef ~A" cpp-name)
                  (formatln out "    | (missingp(STACK_(~D)) ? 0 : ~A)"
                            nn cpp-name)
                  (formatln out "#  endif")
              :finally (progn (formatln out "   ;")
                              (formatln out "  skipSTACK(~D);" nn)))
            (formatln out "  return flags;")
            (formatln out "}")))
    (newline out)
    (loop :for ch :across *checkers* :for default = (checker-default ch)
      :for prefix = (checker-prefix ch) :for suffix = (checker-suffix ch)
      :for delim = (checker-delim ch)
      :for reverse = (checker-reverse ch) :for bitmasks = (checker-bitmasks ch)
      :for type-tag = (objdef-tag (checker-type-odef ch))
      :for c-name = (checker-name ch) :for c-type = (or (checker-type ch) "int")
      :for enum-p = (checker-enum-p ch)
      :do (with-conditional (out (checker-cond-stack ch))
            (formatln out "static const c_lisp_pair_t ~A_table[] = {" c-name)
            (loop :for name :in (checker-cpp-names ch)
              :for name-C = (to-C-name name prefix suffix delim)
              :for odef :in (checker-cpp-odefs ch)
              :for need-ifdef = (and (not enum-p) (stringp name))
              :do (when need-ifdef (formatln out " #ifdef ~A" name-C))
              (formatln out "  { ~A, &(O(~A)) }," name-C (objdef-tag odef))
              (when need-ifdef (formatln out " #endif")))
            (formatln out "  { 0, NULL }")
            (formatln out "};")
            (formatln out "static const c_lisp_map_t ~A_map = {" c-name)
            (formatln out "  ~A_table," c-name)
            (formatln out "  (sizeof(~A_table)/sizeof(c_lisp_pair_t))-1,"
                      c-name)
            (cond ((and (not enum-p) (stringp default))
                   (formatln out "# if defined(~A)" default)
                   (formatln out "  ~A,true," default)
                   (formatln out "# else")
                   (formatln out "  0,false,")
                   (formatln out "# endif"))
                  (t (formatln out "  ~A,true," (or default 0))))
            (formatln out "  true,") ; use_default_function_p
            (formatln out "  ~S" c-name)
            (formatln out "};")
            (formatln out "#define ~A(a) (~A)map_lisp_to_c(a,&~A_map)"
                      c-name c-type c-name)
            (formatln out "#define ~A_reverse(a) map_c_to_lisp(a,&~A_map)"
                      c-name c-name)
            (when bitmasks
              (formatln out "#define ~A_to_list(a) map_c_to_list(a,&~A_map)"
                        c-name c-name)
              (formatln out "#define ~A_from_list(a) map_list_to_c(a,&~A_map)"
                        c-name c-name))))
    (newline out)))

(defun print-tables-2 (out)
  "Output the tables at the end of the file"
  (let ((subr-tab-initdata
         (ext:string-concat "module__" *module-name* "__subr_tab_initdata"))
        (subr-tab
         (ext:string-concat "module__" *module-name* "__subr_tab")))
    (newline out) (newline out)
    (formatln out "struct ~A_t ~A" subr-tab subr-tab)
    (formatln out "  #if defined(HEAPCODES) && (alignment_long < varobject_alignment) && defined(__GNUC__)")
    (formatln out "    __attribute__ ((aligned (varobject_alignment)))")
    (formatln out "  #endif")
    (formatln out "  = {")
    (formatln out "  #if varobjects_misaligned")
    (formatln out "  { 0 },")
    (formatln out "  #endif")
    (loop :for fd :across *fundefs* :do
      (setf (fundef-signatures fd) (nreverse (fundef-signatures fd)))
      (loop :for sig :in (fundef-signatures fd) :do
        (with-conditional (out (signature-cond-stack sig))
          (format out "  LISPFUN_F~A" (fundef-lispfun fd sig)))))
    (formatln out "  0")
    (formatln out "};")
    (formatln out "uintC ~A_size = (sizeof(struct ~A_t)-varobjects_misaligned-sizeof(int))/sizeof(subr_t);" subr-tab subr-tab)
    (newline out)
    (formatln out "struct ~A_t {" subr-tab-initdata)
    (loop :for fd :across *fundefs*
      :do (with-conditional (out (fundef-cond-stack fd))
            (format out "  subr_initdata_t _~A;" (fundef-tag fd))))
    (formatln out "  int _dummy_to_avoid_trailing_comma_in_initializer;")
    (formatln out "} ~A = {" subr-tab-initdata)
    (loop :for fd :across *fundefs*
      :do (with-conditional (out (fundef-cond-stack fd))
            (format out "  { ~S, ~S }," (fundef-pack fd) (fundef-name fd))))
    (formatln out "  0")
    (formatln out "};") (newline out)
    ;; Emit the decl first, to avoid "gcc -missing-declarations" warnings.
    (formatln out "void ~A (module_t* module);" *init-1-name*)
    (formatln out "void ~A (module_t* module)" *init-1-name*)
    (formatln out "{")
    (loop :for fd :across *fundefs* :for tag = (fundef-tag fd) :do
      (loop :for sig :in (fundef-signatures fd) :do
        (when (signature-key-p sig)
          (with-conditional (out (signature-cond-stack sig))
            (dolist (kw (signature-keywords sig))
              (formatln out "  pushSTACK(O(~A));" (objdef-tag kw)))
            (format out "  ~A._~A.keywords = vectorof(~D);" subr-tab tag
                    (length (signature-keywords sig)))))))
    (loop :for vi :across *varinits*
      :do (with-conditional (out (vector (varinit-condition vi)))
            (format out "  O(~A) = (~A);" (varinit-tag vi) (varinit-init vi))))
    (formatln out "}")
    (when *init-2-name*         ; no init2 => define a dummy
      (newline out)
      ;; Emit the decl first, to avoid "gcc -missing-declarations" warnings.
      (formatln out "void ~A (module_t* module);" *init-2-name*)
      (formatln out "void ~A (module_t* module)" *init-2-name*)
      (formatln out "{")
      (formatln out "}"))
    (when *fini-name*           ; no fini-func => define a dummy
      (newline out)
      ;; Emit the decl first, to avoid "gcc -missing-declarations" warnings.
      (formatln out "void ~A (module_t* module);" *fini-name*)
      (formatln out "void ~A (module_t* module)" *fini-name*)
      (formatln out "{")
      (formatln out "}"))))

(defun output-all (out input-file &optional *lines* &aux (*lineno* 1))
  (format out "#line 1 ~S~%" input-file)
  (loop :for ln :in *lines* :and idx :upfrom 0 :do
    (when (/= *lineno* (line-number ln))
      (format out "#line ~D~%" (setq *lineno* (line-number ln))))
    (write-string (line-contents ln) out) (newline out)
    (when (position #\newline (line-contents ln)) (break "~s" ln))
    (when (= idx *module-line*)
      (print-tables-1 out)))
  (print-tables-2 out))

(defun mod-file (input)
  (make-pathname :name (ext:string-concat (pathname-name input) ".m")
                 :defaults input))

(defun modprep (*input-file* &optional (output (mod-file *input-file*)))
  (format t "~&;; ~S: ~S --> ~S~%" 'modprep *input-file* output)
  (with-open-file (in *input-file* :direction :input
                      #+UNICODE :external-format #+UNICODE charset:utf-8)
    (format t ";; ~S: reading ~S: ~:D byte~:P, "
            'modprep *input-file* (file-length in))
    (force-output)
    (setq *lines* (read-all-input in)))
  (format t "~:D line~:P~%" (length *lines*))
  (parse *lines*)
  (format t "~&;; ~S: ~:D object~:P, ~:D DEFUN~:P~[~:;~:* (~:d emulated)~]~
~[~*~:;~:*, ~:D DEFVAR~:P (~:D init~:P)~]~%;; packages: ~S~%"
          'modprep (length *objdefs*) (length *fundefs*)
          *emulation-count* (length *vardefs*) (length *varinits*)
          *module-all-packages*) ; should we write preload.lisp?
  (with-open-file (out output :direction :output :if-exists :supersede
                       #+UNICODE :external-format #+UNICODE charset:utf-8)
    (output-all out *input-file* *lines*)
    (format t "~&~S: wrote ~A (~:D byte~:P)~&"
            'modprep output (file-length out))))

(modprep (first *args*) (or (second *args*) (mod-file (first *args*))))

;;; file modprep.lisp ends here
