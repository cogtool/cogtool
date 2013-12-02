;;; 22.2 The Lisp Pretty Printer
;;; Sam Steingold 2001-07-26 - 2005

(in-package "LISP")
(export
 '(*print-pprint-dispatch* pprint-dispatch copy-pprint-dispatch
   set-pprint-dispatch
   pprint-logical-block pprint-pop pprint-exit-if-list-exhausted
   pprint-fill pprint-linear pprint-tabular)
 "LISP")

(in-package "EXT")

;; used here and in inspect.lisp
(defmacro with-gensyms ((title &rest names) &body body)
  "Bind symbols in NAMES to gensyms.  TITLE is a string - `gensym' prefix.
Inspired by Paul Graham, <On Lisp>, p. 145."
  `(let (,@(mapcar (lambda (sy)
                     `(,sy (gensym ,(concatenate 'string title
                                                 (symbol-name sy) "-"))))
                   names))
     ,@body))

(export '(with-gensyms) "EXT")

(in-package "SYS")

;; ---------------------- Dispatch Tables ----------------------
;; a Dispatch Table is a cons of `*PRINT-PPRINT-DISPATCH*'
;; and an alist of (type priority function)
;; if you modify the structure of Dispatch Tables,
;; you have to change DISPATCH_TABLE_VALID_P in pretty_print_call() in io.d
;; since it checks whether the Dispatch Table contains any valid entries

(defun pprint-dispatch-p (obj)
  (and (consp obj) (eq (car obj) '*print-pprint-dispatch*)))

(defparameter *print-pprint-dispatch* (list '*print-pprint-dispatch*))

(defun default-print-dispatch-function (stream object)
  (print-object object stream))

(defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
  ;; object   ---an object.
  ;; table    ---a pprint dispatch table, or nil.
  ;;            The default is the value of *print-pprint-dispatch*.
  ;; values:
  ;;  function---a function designator.
  ;;  found-p ---a generalized boolean.
  (let ((tail (cdr table)) top)
    (loop (setq tail (member object tail :test #'typep :key #'car))
          (when (endp tail)
            (return (if top
                        (values (third top) t)
                        (values #'default-print-dispatch-function nil))))
          (when (or (null top) (> (cadar tail) (second top)))
            (setq top (car tail)))
          (pop tail))))

(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  ;; table     ---a pprint dispatch table, or nil.
  ;; value:
  ;;  new-table---a fresh pprint dispatch table.
  (unless (or (null table) (pprint-dispatch-p table))
    (error-of-type 'type-error
      :datum table :expected-type '(satisfies pprint-dispatch-p)
      (TEXT "~S: ~S is not a valid print dispatch table")
      'copy-pprint-dispatch table))
  (if table
      (copy-alist table)
      (list '*print-pprint-dispatch*)))

(defun set-pprint-dispatch (type-specifier function &optional (priority 0)
                            (table *print-pprint-dispatch*))
  ;; type-specifier---a type specifier.
  ;; function      ---a function, a function name, or nil.
  ;; priority      ---a real. The default is 0.
  ;; table         ---a pprint dispatch table.
  ;;                  The default is the value of *print-pprint-dispatch*.
  (unless (realp priority)
    (error-of-type 'type-error
      :datum priority :expected-type 'real
      (TEXT "~S: priority must be a real number, not ~S")
      'set-pprint-dispatch priority))
  (let ((rec (member type-specifier (cdr table) :test #'equal :key #'car)))
    (if rec
        (if function
            ;; replace record
            (setf (cdar rec) (list priority function))
            ;; remove record
            (if (cdr rec)
                (setf (car rec) (cadr rec)
                      (cdr rec) (cddr rec))
                (setf (cdr table)
                      (delete type-specifier (cdr table) :test #'equal
                              :key #'car))))
        (when function
          (setf (cdr table)
                (acons type-specifier (list priority function)
                       (cdr table)))))
    (values nil)))

;; ---------------------- pprint-logical-block ----------------------

(defmacro pprint-logical-block ((stream-symbol object
                                 &key prefix per-line-prefix suffix)
                                &body body)
  (let ((out (case stream-symbol
               ((t) '*terminal-io*)
               ((nil) '*standard-output*)
               (otherwise stream-symbol)))
        (idx (gensym "PPLB-IDX-"))
        (pre (gensym "PPLB-PREF-"))
        (suf (gensym "PPLB-SUFF-")))
    `(let ((,pre ,prefix)
           (,suf ,suffix)
           (*prin-line-prefix* ,per-line-prefix)
           (*prin-miserp*
            (and *print-miser-width*
                 (> (line-position ,out)
                    (- (or *print-right-margin* *prin-linelength*)
                       *print-miser-width*))))
           (*prin-indentation*
            (if (boundp '*prin-indentation*)
                (+ *prin-indentation* *print-indent-lists*)
                0)))
      (when (and ,pre *prin-line-prefix*)
        (pprint-logical-block-both-error ,pre))
      (when (and ,pre (not (stringp ,pre)))
        (pprint-logical-block-prefix-not-string-error ,pre))
      (when (and ,suf (not (stringp ,suf)))
        (pprint-logical-block-suffix-not-string-error ,suf))
      (when (and *prin-line-prefix* (not (stringp *prin-line-prefix*)))
        (pprint-logical-block-prefix-not-string-error *prin-line-prefix*))
      (%pprint-logical-block
       (lambda (,out obj)
         (declare (ignorable obj))
         (let ((,idx 0) (*prin-level* (1+ *prin-level*)))
           (macrolet ((pprint-pop ()
                        '(cond
                          ((and *print-length* (>= ,idx *print-length*))
                           (write-string "..." ,out)
                           (go pprint-logical-block-end))
                          ((and (/= 0 ,idx) (%circlep obj ,out))
                           (go pprint-logical-block-end))
                          ((listp obj) (incf ,idx) (pop obj))
                          (t (write-string ". " ,out)
                           (write obj :stream ,out)
                           (go pprint-logical-block-end))))
                      (pprint-exit-if-list-exhausted ()
                        '(unless obj (go pprint-logical-block-end))))
             (when ,pre
               (write-string ,pre ,out)
               (pprint-indent :current 0 ,out))
             (tagbody ,@body
              pprint-logical-block-end
                (when ,suf
                  ;; to avoid suffix being attached to the last string
                  (pprint-newline :fill ,out)
                  (write-string ,suf ,out))))))
       ,object ,out))))
(defun pprint-logical-block-both-error (prefix) ; ABI
  (error (TEXT "~S: cannot supply both ~S (~S) and ~S (~S)")
         'pprint-logical-block ':prefix prefix
         ':per-line-prefix *prin-line-prefix*))
(defun pprint-logical-block-prefix-not-string-error (prefix) ; ABI
  (error-of-type 'type-error
    :datum prefix :expected-type 'string
    (TEXT "~S: ~S must be a string, not ~S")
    'pprint-logical-block ':prefix prefix))
(defun pprint-logical-block-suffix-not-string-error (suffix) ; ABI
  (error-of-type 'type-error
    :datum suffix :expected-type 'string
    (TEXT "~S: ~S must be a string, not ~S")
    'pprint-logical-block ':suffix suffix))

;; ---------------------- utilities ----------------------

(defun pprint-tab (kind colnum colinc &optional stream)
  ;; kind  ---one of :line, :section, :line-relative, or :section-relative.
  ;; colnum---a non-negative integer.
  ;; colinc---a non-negative integer.
  ;; stream---an output stream designator.
  (format-tabulate stream
                   (ecase kind
                     ((:line :line-relative) nil)
                     ((:section :section-relative) t))
                   (ecase kind
                     ((:line :section) nil)
                     ((:line-relative :section-relative) t))
                   colnum colinc))

;; ---------------------- list printing ----------------------

(defun pprint-fill (out list &optional (colon-p t) at-sign-p)
  ;; out      ---an output stream designator.
  ;; list     ---an object.
  ;; colon-p  ---a generalized boolean. The default is true.
  ;; at-sign-p---a generalized boolean. ignored
  (declare (ignore at-sign-p))
  (pprint-logical-block (out list :prefix (if colon-p "(" "")
                             :suffix (if colon-p ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write (pprint-pop) :stream out)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space out)
          (pprint-newline :fill out))))

(defun pprint-linear (out list &optional (colon-p t) at-sign-p)
  ;; out      ---an output stream designator.
  ;; list     ---an object.
  ;; colon-p  ---a generalized boolean. The default is true.
  ;; at-sign-p---a generalized boolean. ignored
  (declare (ignore at-sign-p))
  (pprint-logical-block (out list :prefix (if colon-p "(" "")
                             :suffix (if colon-p ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write (pprint-pop) :stream out)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space out)
          (pprint-newline :linear out))))

;; lifted verbatim from CLHS
(defun pprint-tabular (out list &optional (colon-p t) at-sign-p (tabsize nil))
  ;; out      ---an output stream designator.
  ;; list     ---an object.
  ;; colon-p  ---a generalized boolean. The default is true.
  ;; at-sign-p---a generalized boolean. ignored
  ;; tabsize  ---a non-negative integer. The default is 16.
  (declare (ignore at-sign-p))
  (when (null tabsize) (setq tabsize 16))
  (pprint-logical-block (out list :prefix (if colon-p "(" "")
                             :suffix (if colon-p ")" ""))
    (pprint-exit-if-list-exhausted)
    (loop (write (pprint-pop) :stream out)
          (pprint-exit-if-list-exhausted)
          (write-char #\Space out)
          (pprint-tab :section-relative 0 tabsize out)
          (pprint-newline :fill out))))
