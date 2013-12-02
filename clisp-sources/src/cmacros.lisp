;;; CLISP Compiler Macros
;;; Sam Steingold 2001-05-09
;;; Bruno Haible 2005
;;; CLHS 3.2.2.1 http://www.lisp.org/HyperSpec/Body/sec_3-2-2-1.html

(in-package "SYSTEM")

;; a legitimate option is to keep the `compiler-macro' definition of the
;; symbol in a global hash-table instead of the `symbol-plist'.
;; the reason we use plists is that
;; * this performance issue is related only to the compilation speed,
;;   not the execution speed
;; * the plists are actually quite short:
;;   [non-standard functions & macros used in this snippet are in CLOCC:
;;    compose:            <http://clocc.sf.net/clocc/src/port/ext.lisp>
;;    standard-deviation: <http://clocc.sf.net/clocc/src/cllib/math.lisp>
;;    top-bottom-ui:      <http://clocc.sf.net/clocc/src/cllib/sorted.lisp>
;;    CLOCC is available at <http://clocc.sf.net>]
;; (let ((al nil)
;;       (acc (compose length symbol-plist)))
;;   (do-all-symbols (sy) (push sy al))
;;   (delete-duplicates al :test #'eq)
;;   (format t "~&none:~10t ~5:d~%" (count-if #'zerop al :key acc))
;;   (multiple-value-bind (de me le) (standard-deviation al :key acc)
;;     (format t "std dev:~10t ~5f~%mean:~10t ~5f~%length:~10t ~5:d~%"
;;             de me le))
;;   (top-bottom-ui al 5 nil nil :key acc))
;; none:      4,206
;; std dev:   1.874
;; mean:      .6492
;; length:    5,089
;; Top/Bottom: list: 5,089 records.
;; Top (5):
;;   1: hostent-addrtype    ==> 10
;;   2: hostent-aliases     ==> 10
;;   3: hostent-addr-list   ==> 10
;;   4: hostent-name        ==> 10
;;   5: dir-key-info-type   ==> 10
;; also, compiler macros are probably not used often anyway.
;; At any rate, if someone will want to switch to a global hash-table,
;; one needs to change only the following two functions:
;;    compiler-macro-function and
;;    (setf compiler-macro-function)

(defun compiler-macro-function (name &optional environment)
  (declare (ignore environment))
  (setq name (check-function-name name 'compiler-macro-function))
  (if (symbolp name) (get name 'compiler-macro)
      (get (second name) 'compiler-macro-setf)))

(defun (setf compiler-macro-function) (newf name &optional environment) ; ABI
  (declare (ignore environment))
  (setq name (check-function-name name '(setf compiler-macro-function)))
  (if (symbolp name) (setf (get name 'compiler-macro) newf)
      (setf (get (second name) 'compiler-macro-setf) newf)))

;; (proclaim '(inline function-form-p simple-function-form-p))

;; Test whether the form is (FUNCTION ...).
(defun function-form-p (form)
  (and (consp form) (eq (car form) 'FUNCTION)
       (consp (cdr form)) (null (cddr form))))

;; Test whether the form is #'symbol or #'(SETF symbol).
(defun simple-function-form-p (form)
  (and (function-form-p form) (function-name-p (second form))))

;; (funcall (function foo) ...) ==> (foo ...)
(defun strip-funcall-form (form) ; ABI
  (if (and (eq (car form) 'funcall) (simple-function-form-p (second form)))
    (cons (second (second form)) (cddr form))
    form))

(defmacro define-compiler-macro (&whole whole-form
                                 name args &body body)
  (declare (ignore name args body))
  (multiple-value-bind (expansion expansion-lambdabody name lambdalist docstring)
      (sys::make-macro-expansion (cdr whole-form) whole-form
                                 #'function-name-p 'strip-funcall-form)
    (declare (ignore expansion-lambdabody lambdalist))
    (sys::check-redefinition name 'define-compiler-macro
                             (and (compiler-macro-function name)
                                  "compiler macro"))
    `(EVAL-WHEN (COMPILE LOAD EVAL)
       ,@(when docstring
           `((SYSTEM::%SET-DOCUMENTATION ',name 'COMPILER-MACRO ,docstring)))
       (SETF (COMPILER-MACRO-FUNCTION ',name) ,expansion)
       ',name)))
