;;; Backquote Implementation for CLISP

;;; Parts of this file
;;; Copyright 2003 Kaz Kylheku <kaz@ashi.footprints.net>
;;; Dedicated to Pei-Yin Lin
;;;
;;; LIBERAL FREEWARE LICENSE: This Lisp source code document may be used
;;; by anyone for any purpose, and freely redistributed alone or in
;;; combination with other software, provided that the license is not
;;; violated.  The only possible way to violate the license is to
;;; redistribute this code in source form, with the copyright notice or
;;; license removed or altered.  This license applies to this document
;;; only, not any other software that it is combined with.

;;; Parts of this file
;;; Copyright (C) 1988, 1989, 1992-2004 Michael Stoll, Bruno Haible
;;; Copyright (C) 2002-2003, 2005 Sam Steingold
;;; This is Free Software, covered by the GNU GPL.

(in-package "SYSTEM")

;;; ============================== Reader Macro ==============================

;;; At this level, we do only parsing, not simplification, so that things
;;; like `,A print as they were input. Parsing returns
;;; the following                  for
;;;   (BACKQUOTE x)                `x
;;;   (UNQUOTE x)                  ,x
;;;   (SPLICE x)                   ,@x
;;;   (NSPLICE x)                  ,.x

;;; *unquote-occurred* flips to T when a sub-reader encounters the unquote
;;; syntax. This variable is the basis for a trick by which we diagnose uses of
;;; the unquote syntax inside forms which are not vectors or lists, such as:
;;; #`#s(foo-structure :bar #,z) without an cooperation from the readers of
;;; these forms. In some cases, however, such unquote syntax will cause the
;;; reader of the subform to raise an error before we catch it.
(proclaim '(special *unquote-occurred*))

;;; *backquote-level* measures the level of backquote nesting that the reader
;;; is entangled in. It increases by one when reading the backquoted object,
;;; and decreases by one over reading an unquoted form.
(proclaim '(special *backquote-level*))

;;; *reading-array* is normally unbound. When the reader is running, it
;;; is dynamically bound to NIL, and when a #A array is being read,
;;; it is bound to T. This lets the comma-reader signal an error when
;;; unquotes occur in an array.
(proclaim '(special *reading-array*))

;;; *reading-struct* is analogous to *reading-array*, but for structs.
(proclaim '(special *reading-struct*))

;;; Handle the ` read syntax.
(defun backquote-reader (stream char)
  (declare (ignore char))
  (let* ((*unquote-occurred* nil)
         (*reading-array* nil)
         (*reading-struct* nil)
         (*backquote-level* (1+ (or *backquote-level* 0)))
         (object (read stream t nil t)))
    (unless (or (and (vectorp object) (eq (array-element-type object) 'T))
                (listp object))
      (when *unquote-occurred*
        (error-of-type 'reader-error
          :stream stream
          (TEXT "~S: unquotes may occur only in (...) or #(...) forms")
          'read)))
    (when (consp object)
      (let ((head (first object)))
        (when (or (eq head 'SPLICE) (eq head 'NSPLICE))
          (bq-non-list-splice-error head stream)))
      (when (bq-member 'SPLICE object)
        (bq-dotted-splice-error 'SPLICE stream))
      (when (bq-member 'NSPLICE object)
        (bq-dotted-splice-error 'NSPLICE stream)))
    (list 'BACKQUOTE object)))

;;; Handle the read syntax ,
(defun comma-reader (stream char)
  (declare (ignore char))
  (when (null *backquote-level*)
    (error-of-type 'reader-error
      :stream stream
      (TEXT "~S: comma is illegal outside of backquote")
      'read))
  (when (zerop *backquote-level*)
    (error-of-type 'reader-error
      :stream stream
      (TEXT "~S: more commas out than backquotes in, is illegal")
      'read))
  (when *reading-struct*
    (error-of-type 'reader-error
      :stream stream
      (TEXT "~S: unquotes may not occur in structures")
      'read))
  (when *reading-array*
    (error-of-type 'reader-error
      :stream stream
      (TEXT "~S: unquotes may not occur in arrays")
      'read))
  (setq *unquote-occurred* t)
  (let ((*backquote-level* (1- *backquote-level*))
        (next (peek-char nil stream)))
    (cond ((char= next #\@)
           (read-char stream)
           (list 'SPLICE (read stream t nil t)))
          ((char= next #\.)
           (read-char stream)
           (list 'NSPLICE (read stream t nil t)))
          (t (list 'UNQUOTE (read stream t nil t))))))

;;; Like MEMBER but handles improper lists without error.
(defun bq-member (elem list &key (test #'eql))
  (do ((list list (rest list)))
      ((atom list) nil)
   (when (funcall test (first list) elem)
     (return list))))

;;; ----------------------------- Error Messages -----------------------------

;;; Used by the Reader Macro and the Macroexpander.

;;; Signal error for `,.form or `,@form.
;;; It's undefined behaviour; we signal an error for it.
;;; If stream is non-NIL, then add the prefix "READ: ", to flag the error as
;;; coming from the reader.
(defun bq-non-list-splice-error (sym &optional stream)
  (let ((errmsg
          (if (eq sym 'SPLICE)
            (TEXT "the syntax `,@form is invalid")
            (TEXT "the syntax `,.form is invalid"))))
    (if stream
      (error-of-type 'reader-error
        :stream stream
        (TEXT "READ: ~@?")
        errmsg)
      (error-of-type 'error
        "~@?"
        errmsg))))

;;; Signal error for `(... . ,@form) or `(... . ,.form).
;;; It's undefined behaviour; we signal an error for it.
(defun bq-dotted-splice-error (sym &optional stream)
  (let ((errmsg
          (if (eq sym 'SPLICE)
            (TEXT "the syntax `( ... . ,@form) is invalid")
            (TEXT "the syntax `( ... . ,.form) is invalid"))))
    (if stream
      (error-of-type 'reader-error
        :stream stream
        (TEXT "READ: ~@?")
        errmsg)
      (error-of-type 'error
        "~@?"
        errmsg))))

;;; ============================== Macroexpander ==============================

;;; The macroexpander consists of three parts:
;;; - The backquote macro: the entry point, taking the form as provided by
;;;   the reader, and producing an evaluatable form.
;;; - The recursive expansion engine. Its behaviour is dictated by the
;;;   HyperSpec's general description of backquote.
;;; - The expansion optimizer. Its purpose is to use the Lisp's list/vector
;;;   construction primitives as efficiently as possible. It can run in a
;;;   non-optimized mode, where lists are always constructed through LIST and
;;;   APPEND; or optimization can be performed that reduce run-time consing or
;;;   otherwise simplify the macroexpansion.

;;; ------------------------------ Entry-points ------------------------------

;; The BACKQUOTE macro just calls the backquote expander on its argument.
(sys::%putd 'BACKQUOTE
  (sys::make-macro
    (function BACKQUOTE
      (lambda (form env)
        (declare (ignore env))
        (bq-expand (second form))))))

;;; ----------------------- Recursive Expansion Engine -----------------------

;;; Top level cases
;;;
;;; `()        -->  ()
;;; `cons      -->  result of (bq-expand-cons cons)
;;; `#( ... )  -->  result of (bq-expand-vector #( ... ))
;;;  other     -->  'other
(defun bq-expand (form)
  ;; we don't have TYPECASE at this stage
  (cond
    ((null form) nil)
    ((consp form)
      ;; Handle base cases as described by HyperSpec, plus nested backquote:
      ;;
      ;; `,form     -->  form
      ;; `,@form    -->  error
      ;; ``form     -->  `form-expanded
      ;; list-form  -->  (append f1 f2 f3 ...) where (f1 f2 f3 ...)
      ;;                 is the output of (bq-expand-list list-form).
      (case (first form)
        ((UNQUOTE)
          (second form))
        ((SPLICE NSPLICE)
          (bq-non-list-splice-error (second form)))
        ((BACKQUOTE)
          (list 'BACKQUOTE (bq-expand (second form))))
        (otherwise
          (let ((expansion (bq-expand-list form)))
            (bq-append-multiple expansion)))))
    ((and (vectorp form) (eq (array-element-type form) 'T))
      ;; Handle vector expansion, along the lines suggested by HyperSpec.
      (let ((expansion (bq-expand-list (map 'list #'identity form))))
        (bq-optimize-for-vector expansion (bq-append-multiple expansion))))
    (t (list 'quote form))))

;;; Handle the transformation of `(x1 x2 x3 ...) as described by HyperSpec
;;; to produce a list of forms that can be combined into an APPEND form.
;;; There is one deviation from the HyperSpec: namely, in the case
;;; `(x1 x2 ... xn . atom) the atom is translated to (BACKQUOTE atom)
;;; rather than (QUOTE atom). This allows for the atom to be a vector
;;; with embedded unquotes, an apparently common extension.
(defun bq-expand-list (forms)
  (let ((expanded '())) ; reversed list of expanded forms to be APPENDed
    (do ()
        ((null forms))
      (let ((expanded-car (bq-transform (first forms))))
        (setq expanded (cons expanded-car expanded)))
      (let ((tail (rest forms)))
        (cond ((null tail) (return-from nil))
              ((consp tail)
               (case (first tail)
                 ;; well-defined dotted unquote `( ... . ,form)
                 ((UNQUOTE)
                  (setq expanded (cons (second tail) expanded))
                  (return-from nil))
                 ;; undefined dotted splice: `( ... . ,@form)
                 ((SPLICE NSPLICE)
                  (bq-dotted-splice-error (first tail)))
                 (otherwise
                  (setq forms tail))))
              (t (setq expanded (cons (list 'BACKQUOTE tail) expanded))
                 (return-from nil)))))
    (nreverse expanded)))

;;; Handle the transformation of [x1] [x2] ... forms as described
;;; in the HyperSpec. In addition, nested backquotes are handled here.
(defun bq-transform (form)
  (if (consp form)
    (case (first form)
      ((UNQUOTE) (bq-list (second form)))
      ((SPLICE) (second form))
      ;; (BQ-NCONCABLE FORM) serves as a parse tree annotation which
      ;; tells the optimizer that FORM may be destructively manipulated.
      ((NSPLICE) (list 'BQ-NCONCABLE (second form)))
      ((BACKQUOTE) (bq-list (list 'BACKQUOTE (bq-expand (second form)))))
      (otherwise (bq-list (bq-expand form))))
    (bq-list (bq-expand form))))

;;; --------------------------- Expansion Optimizer ---------------------------

;;; Naive expansion produces inefficient construction forms, e.g.
;;;   `(,(foo) ,(bar)) => (APPEND (LIST (foo)) (LIST (bar)))
;;;   instead of          (LIST (foo) (bar))
;;; Backquote expansion optimizers are enabled by default, but can be turned
;;; off for debugging.
(proclaim '(special *backquote-optimize-cons*))
(setq *backquote-optimize-cons* t)
(proclaim '(special *backquote-optimize-list*))
(setq *backquote-optimize-list* t)
(proclaim '(special *backquote-optimize-append*))
(setq *backquote-optimize-append* t)
(proclaim '(special *backquote-optimize-nconc*))
(setq *backquote-optimize-nconc* t)
(proclaim '(special *backquote-optimize-vector*))
(setq *backquote-optimize-vector* t)

;;; This simplifes CONS, LIST, APPEND, NCONC calls that are emitted by the
;;; backquote expander. We are *not* allowed to collapse CONS or LIST calls
;;; given by the user.
;;; E.g. `((,a ,b 3) (,a ,@(list b 3)))
;;; can be simplified to (LIST (LIST* A B '(3)) (LIST A B 3))
;;; but not to           (LIST (LIST* A B '(3)) (LIST* A B '(3))).

;;; The set of simplifications has been chosen in a minimal way, to satisfy
;;; the testsuite. Don't add additional simplifications if the testsuite
;;; doesn't show a need for it - otherwise such simplifications would just
;;; be no-ops that slow down the macroexpander.

;;; Some optimizations have to be inhibited on splicing forms because while
;;; primitives LIST and APPEND handle splicing automatically correctly,
;;; primitives like CONS and QUOTE don't.

;;; Tests whether the given form may yield multiple list elements.
;;; Not only ,@x and ,.x are splicing, but also ,,@x !
(defun bq-splicing-p (form)
  (and (consp form)
       (case (first form)
         ((UNQUOTE) (bq-splicing-p (second form)))
         ((SPLICE NSPLICE) t)
         (t nil))))

;;; To convert a splicing form to a non-splicing one.
(defun bq-non-splicing (form)
  (if (bq-splicing-p form) (list 'APPEND form) form))

;;; BQ-CONS returns a form that returns a CONS of the results of the two
;;; given forms. Assumes that form2 is not splicing.
(defun bq-cons (form1 form2)
  (let ((operator (if (bq-splicing-p form1) 'LIST* 'CONS)))
    (if *backquote-optimize-cons*
      ; Simplify `(CONS ,form1 ,form2) or `(LIST* ,form1... ,form2):
      (cond #|
            ((and (not (bq-splicing-p form1)) (constantp form1)
                  (constantp form2))
             ; Both parts constant -> combine immediately.
             (list 'QUOTE (cons (eval form1) (eval form2))))
            ((null form2)
             ; (CONS form1 NIL) -> (LIST form1)
             ; (LIST* form1... NIL) -> (LIST form1...)
             (list 'LIST form1))
            |#
            ((atom form2)
             ; Cannot simplify.
             (list operator form1 form2))
            ((eq (first form2) 'LIST)
             ; (CONS form1 (LIST . more)) -> (LIST form1 . more)
             ; (LIST* form1... (LIST . more)) -> (LIST form1... . more)
             ; Test case: `(,(f1) ,(f2))
             (list* 'LIST form1 (rest form2)))
            #|
            ((or (eq (first form2) 'LIST*) (eq (first form2) 'CONS))
             ; (CONS form1 (LIST* . more)) -> (LIST* form1 . more)
             ; (CONS form1 (CONS . more)) -> (LIST* form1 . more)
             ; (LIST* form1... (LIST* . more)) -> (LIST* form1... . more)
             ; (LIST* form1... (CONS . more)) -> (LIST* form1... . more)
             (list 'LIST* form1 (rest form2)))
            |#
            ((and (consp form2) (eq (first form2) 'QUOTE)
                  (consp (cdr form2)) (null (cddr form2))
                  (not (bq-splicing-p (second form2)))
                  (consp form1) (eq (first form1) 'QUOTE)
                  (consp (cdr form1)) (null (cddr form1))
                  (not (bq-splicing-p (second form1))))
             ; Test case: `(c1 c2)
             (list 'QUOTE (cons (second form1) (second form2))))
            (t (list operator form1 form2)))
      (list operator form1 form2))))

;;; BQ-LIST returns a form that returns a list of the result of the given form.
(defun bq-list (form1)
  ; Equivalent to (bq-cons form1 'NIL).
  (if *backquote-optimize-list*
    (cond ((and (not (bq-splicing-p form1)) (constantp form1))
           ; Test case: `(c1)
           (list 'QUOTE (list (eval form1))))
          (t (list 'LIST form1)))
    (list 'LIST form1)))

;;; BQ-APPEND returns a form that returns the nondestructive concatenation of
;;; the results of the given forms.
(defun bq-append (form1 form2)
  (if *backquote-optimize-append*
    ; Simplify `(APPEND ,form1 ,form2):
    (cond ((null form1)
           ; (APPEND NIL form2) -> (APPEND form2) -> form2
           ; Test case: `(,@() ,@(f1))
           form2)
          ((null form2)
           ; (APPEND form1 NIL) -> (APPEND form1) -> form1
           ; Test case: `(,@(f1) ,@())
           form1)
          ((and (consp form1) (eq (first form1) 'LIST)
                (null (cdr (last form1))))
           ; (APPEND (LIST x1 ... xn) form2) -> (LIST* x1 ... xn form2),
           ; or (CONS x1 form2) if n = 1, or form2 if n = 0.
           ; Test cases: `(,(f1) c2) and `(,(f1) ,(f2))
           (setq form2 (bq-non-splicing form2))
           (cond ((null (cdr form1)) form2)
                 ((null (cddr form1)) (bq-cons (second form1) form2))
                 (t (cons 'LIST* (append (rest form1) (list form2))))))
          ((and (consp form1) (eq (first form1) 'QUOTE)
                (consp (cdr form1)) (null (cddr form1))
                (not (bq-splicing-p (second form1))) ; too hairy
                (listp (second form1)) (null (cdr (last (second form1)))))
           ; (APPEND (QUOTE l) form2) -> (LIST* ... form2)
           ; Test cases: `(c1 c2) and `(c1 ,(f2))
           (setq form2 (bq-non-splicing form2))
           (let ((result form2))
             (do ((l (reverse (second form1)) (cdr l)))
                 ((endp l))
               (setq result (bq-cons (list 'QUOTE (car l)) result)))
             result))
          ((and (consp form2) (eq (first form2) 'APPEND))
           ; (APPEND form1 (APPEND . more)) -> (APPEND form1 . more)
           ; Test case: `(,@(f1) ,@(f2) ,@(f3))
           (list* 'APPEND form1 (rest form2)))
          (t (list 'APPEND form1 form2)))
    (list 'APPEND form1 form2)))

;;; BQ-NCONC returns a form that returns the destructive concatenation of the
;;; results of the given forms.
(defun bq-nconc (form1 form2)
  (if *backquote-optimize-nconc*
    ; Simplify `(NCONC ,form1 ,form2):
    (cond ((null form1)
           ; (NCONC NIL form2) -> (NCONC form2) -> form2
           ; Test case: `(,.() ,.(f1))
           form2)
          ((null form2)
           ; (NCONC form1 NIL) -> (NCONC form1) -> form1
           ; Test case: `(,.(f1) ,.())
           form1)
          ((and (consp form2) (eq (first form2) 'NCONC))
           ; (NCONC form1 (NCONC . more)) -> (NCONC form1 . more)
           ; Test case: `(,.(f1) ,.(f2) ,@(f3))
           (list* 'NCONC form1 (rest form2)))
          (t (list 'NCONC form1 form2)))
    (list 'NCONC form1 form2)))

;;; BQ-APPEND-MULTIPLE returns a form that returns the concatenation of the
;;; results of the given forms. It uses destructive concatenation for forms
;;; that start with BQ-NCONCABLE.
(defun bq-append-multiple (forms)
  (setq forms (reverse forms))
  (if (endp forms)
    'NIL
    (let (result nconcable)
      (let ((form (car forms)))
        (if (and (consp form) (eq (first form) 'BQ-NCONCABLE))
          (setq result (second form) nconcable t)
          (setq result form nconcable nil)))
      (setq forms (cdr forms))
      (do ()
          ((endp forms))
        (let ((form (car forms)))
          (setq result
                (if (and (consp form) (eq (first form) 'BQ-NCONCABLE))
                  ; Must wrap the already constructed result in an APPEND to
                  ; prevent it from splicing, since destructive modifications
                  ; are allowed on form, but not on result.
                  (bq-nconc (second form)
                            (if (and (bq-splicing-p result) (not nconcable))
                              (list 'APPEND result)
                              result))
                  (bq-append form
                             (if (and (bq-splicing-p result) nconcable)
                               (list 'NCONC result)
                               result))))
          (setq nconcable nil))
        (setq forms (cdr forms)))
      ;; The caller cannot handle a naked (SPLICE ...) or (NSPLICE ...) form,
      ;; so wrap it in an APPEND.
      (bq-non-splicing result))))

;;; BQ-OPTIMIZE-FOR-VECTOR generates a better translation for a backquoted
;;; vector. The vector has been already converted to a list, which
;;; was subject to unoptimized backquote expansion. That resulting
;;; list of append arguments is what is passed to this function.
;;; The expansion is optimized and then converted to a vector
;;; form according to these rules:
;;;
;;; '(...)  -> #(...)
;;; (list ...) -> (vector ...)
;;; (append ...) -> (multiple-value-call #'vector ...)
;;;
;;; The (append ...) case is based on the original unoptimized
;;; append args. The arguments are each treated as follows:
;;;
;;; (list ...) -> (values ...)
;;; (splice ...) -> (values-list (append ...))
;;; (nsplice ...) -> (values-list (nconc ...))
;;; other -> (values-list other)
(defun bq-optimize-for-vector (unoptimized optimized)
  (if *backquote-optimize-vector*
    (cond ((or (eq optimized 'NIL)
               (and (consp optimized) (eq (first optimized) 'QUOTE)
                    (consp (cdr optimized)) (null (cddr optimized))
                    (not (bq-splicing-p (second optimized)))))
           ; Create the vector at macroexpansion time.
           (apply #'vector (eval optimized)))
          ((not (consp optimized))
           (list 'APPLY '#'VECTOR optimized))
          ((eq (first optimized) 'LIST)
           ; Test cases: `#(,(f1) ,(f2)) and ``#(,,@(f1) ,,@(f2))
           (cons 'VECTOR (rest optimized)))
          (t (list* 'MULTIPLE-VALUE-CALL '#'VECTOR
                    (mapcan #'(lambda (form &aux (nconcable nil))
                                (when (and (consp form) (eq (first form) 'BQ-NCONCABLE))
                                  (setq form (second form) nconcable t))
                                (cond ((atom form)
                                       (list (list 'VALUES-LIST form)))
                                      ((memq (first form) '(SPLICE NSPLICE))
                                       ; Test case: ``#(,.,.(f1) ,.,@(f2) ,@,.(f3) ,@,@(f4))
                                       (list
                                         (list 'VALUES-LIST
                                           (list (if nconcable 'NCONC 'APPEND)
                                                 form))))
                                      ((eq (first form) 'LIST)
                                       ; Test case: `#(,(f1) ,@(f2))
                                       (mapcar #'(lambda (f)
                                                   (if (bq-splicing-p f)
                                                     (list 'VALUES-LIST (list 'LIST f))
                                                     (list 'VALUES f)))
                                               (rest form)))
                                      ((and (eq (first form) 'QUOTE)
                                            (consp (cdr form)) (null (cddr form))
                                            (not (bq-splicing-p (second form))))
                                       ; Test case: `#(c1 ,@(f2))
                                       (mapcar #'(lambda (x) (list 'QUOTE x))
                                               (second form)))
                                      (t (list (list 'VALUES-LIST form)))))
                             unoptimized))))
    (list 'APPLY '#'VECTOR optimized)))

;;; =========================== Other Entry-points ===========================

;;; Interfaces used by other modules within CLISP, and possibly
;;; by CLISP applications.
;;;
;;; Note: to dynamically add a variable number of unquotes to a nested
;;; backquote, consider using the ,,@ syntax:
;;;
;;;   (let ((unquote-these-forms '((list 1 2) (list 3 4)))
;;;     `(outer `(inner ,,@unquote-these-forms))
;;;
;;; rather than ADD-BACKQUOTE and ADD-UNQUOTE:
;;;
;;;   (let ((unquote-these-forms '((list 1 2) (list 3 4))))
;;;     `(outer ,(system::add-backquote
;;;                `(inner ,@(mapcar #'system::add-unquote
;;;                                  unquote-these-forms)))))
;;;
;;; The effect is like `(outer ,(inner ,(list 1 2) ,(list 3 4)))
;;;
;;; If you want the effect `(outer ,(inner ,@(list 1 2) ,@(list 3 4)))
;;; then substitute `(outer `(inner ,@,@unquote-these-forms))
;;;
;;; If you think you need ADD-BACKQUOTE and ADD-UNQUOTE, or even
;;; the nonexistent ADD-SPLICE, it's likely that your requirements
;;; may be satisfied by the ,,@ and ,@,@ syntax. The distributive
;;; rule is simple: the right ,@ splices the forms into the list, and the
;;; left , or ,@ distributes itself over those forms before the next
;;; expansion round.
;;;
;;; There are exceptions, like the more complicated situation in CLISP's
;;; giant defstruct macro, which prepares a list of nested lists each
;;; containing a buried unquote forms, and then later encloses it in a
;;; backquote.

(defun add-backquote (skel)
  (list 'BACKQUOTE skel))

(defun add-unquote (skel)
  (list 'UNQUOTE skel))

(defun backquote-cons (car cdr)
  (bq-cons car cdr))

(defun backquote-append (left right)
  (bq-append left right))
