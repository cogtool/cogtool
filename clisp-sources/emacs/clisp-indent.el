;;; Indentation style for CLISP sources                      -*- Emacs-Lisp -*-
;;; Load this file from ~/.emacs or ~/.emacs.el
;;; Tested with XEmacs 21.1.7, Emacs 20,21

;;; General

;; Tabs are anachronistic.
(setq-default indent-tabs-mode nil)


;;; Common Lisp

; Don't use autoloading.
;(autoload 'common-lisp-indent-function "cl-indent" "Common Lisp indent.")
; Load cl-indent.el now. Loading it later would clobber our
; common-lisp-indent-function property settings.
(load-library "cl-indent")

(setq lisp-indent-function 'common-lisp-indent-function)

; Each operator's indentation specification can be
; - a symbol, must be an Elisp function,
; - a list (x1 x2 ...), where x1 specifies the indentation of the first
;   subform, x2 the indentation of the second subform, and so on.
;   ... nil ...          = default indentation
;   ... &lambda ...      = (&whole 4 (&rest 1))
;   ... &body)           = &rest ,lisp-body-indent
;   ... &rest x)         = infinite repetition x x x x x ...
;   ... n ...            = explicit indentation by n spaces, if the
;                          first subform is already on the second line
; - an integer n, denoting (4 4 ... &body).
;   => Special case: 0 is equivalent to (&body), i.e. equivalent to (&rest 2).
; Note a subtle difference between (&rest 2) and (2 &rest 2): If the first
; subform is on the same line as the operator, (&rest 2) indents the second
; subform under it, whereas (2 &rest 2) indents the second subform by 2 spaces
; only.

(defmacro defindent (operator indentation)
  `(put ',operator 'common-lisp-indent-function ',indentation))

(setq lisp-body-indent 2)       ; default
(setq lisp-indent-maximum-backtracking 4) ; needed for flet/labels
(setq lisp-tag-body-indentation 2) ; for tagbody/prog/prog*
(setq lisp-tag-indentation 2)   ; for tagbody/prog/prog*

(defindent and (&rest 2))
(defindent appease-cerrors (&rest 2))
(defindent assert (&rest 2))
(defindent block (4 &rest 2))                                    ; default
(defindent case (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
(defindent catch (4 &rest 2))                                    ; default
(defindent ccase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
(defindent check-type (2 2 &rest 2))
(defindent compiler-let ((&whole 4 &rest (&whole 1 1 2)) &body)) ; default
(defindent cond (&rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
(defindent ctypecase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
(defindent decf (2 2))
(defindent declaim (&rest 2))
(defindent declare (&rest 2))
(defindent defclass (10 (&whole 10 1 &rest 1) &rest (&whole 2 &rest 1)))
(defindent defconstant (4 2 2))                                  ; default
(defindent defgeneric (4 (&whole 4 1 &rest 1) &body))
(defindent define-condition (18 (&whole 18 1 &rest 1) &rest (&whole 2 &rest 1)))
(defindent define-modify-macro (4 (&whole 4 1 &rest 1) 4 &body))
(defindent define-setf-expander (4 (&whole 4 1 &rest 1) &body))
(defindent define-setf-method (4 (&whole 4 1 &rest 1) &body))
(defindent define-symbol-macro (4 &body))
(defindent definternational (4 &body))
(defindent deflanguage (4))
(defindent deflocalized (4 4 &body))
(defindent defmacro (4 (&whole 4 1 &rest 1) &body))
(defindent defmethod lisp-indent-defmethod)                      ; default
(defindent defpackage (4 &rest 2))
(defindent defparameter (4 2 2))                                 ; default
; FIXME: How to deal with both short and long forms of defsetf?
;(defindent defsetf (4 (&whole 4 1 &rest 1) 2 &body))
;(defindent defsetf (14 (&whole 14 1 &rest 1) (&whole 14 1 &rest 1) &body))
(defindent defstruct ((&whole 4 &rest (&whole 2 &rest 1)) &rest (&whole 2 &rest 1))) ; default
(defindent deftype (9 (&whole 9 1 &rest 1) &body))
(defindent defun (7 (&whole 7 1 &rest 1) &body))
(defindent defvar (4 2 2))                                       ; default
(defindent destructuring-bind ((&whole 6 1 &rest 1) 4 &body))
(defindent deutsch (2 1 2 1 2))
(defindent do lisp-indent-do)                                    ; default
(defindent do* lisp-indent-do)                                   ; default
(defindent do-all-symbols ((&whole 4 1 &rest 1) &body))
(defindent do-external-symbols ((&whole 4 1 &rest 1) &body))
(defindent do-symbols ((&whole 4 1 &rest 1) &body))
(defindent dohash ((&whole 4 1 &rest 1) (&whole 4 1 &rest 1) &body))
(defindent dolist ((&whole 4 1 1) &body))
(defindent doseq ((&whole 4 1 1) &body))
(defindent dotimes ((&whole 4 1 1) &body))
(defindent ecase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
(defindent english (2 1 2 1 2))
(defindent etypecase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
(defindent eval-when (4 &body))                                  ; default
(defindent exit-on-error (&body))
(defindent fcase '(6 4 &rest (&whole 2 &rest 1)))
(defindent flet ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
(defindent formatter (&body))
(defindent francais (2 1 2 1 2))
(defindent function (&body))
(defindent generic-flet ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
(defindent generic-function ((&whole 4 1 &rest 1) &body))
(defindent generic-labels ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
(defindent go (2))
(defindent handler-bind (2 &body))
(defindent handler-case (2 &rest (&whole 2 (&whole 4 1 &rest 1) &body)))
; CLISP source indents the two branchs of an 'if' form equally.
(defindent if (4 2 2))
(defindent ignore-errors (&body))
(defindent in-package (&body))
(defindent incf (2 2))
(defindent labels ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
(defindent lambda ((&whole 4 1 &rest 1) &body))
(defindent let ((&whole 4 &rest (&whole 1 2 2)) &body))
(defindent let* ((&whole 4 &rest (&whole 1 2 2)) &body))
(defindent load-time-value (&body))
(defindent locally (2 &body))
; CLISP sources don't use the "big" LOOP - its semantics is too unreliable.
(defindent loop (&body))
(defindent loop-finish ())
(defindent macrolet ((&whole 4 &rest (&whole 1 (&whole 4 1 &rest 1) &body)) &body))
(defindent :method ((&whole 4 1 &rest 1) &body))          ; for defgeneric etc.
(defindent muffle-cerrors (&rest 2))
(defindent multiple-value-bind ((&whole 6 &rest 1) 4 2 &rest 2))
(defindent multiple-value-call (4 2 &rest 2))
(defindent multiple-value-list (2))
(defindent multiple-value-prog1 (2 &rest 2))
(defindent multiple-value-setq (4 2))                            ; default
(defindent nth-value (2 2))
(defindent optimize (&rest 2))
(defindent or (&rest 2))
(defindent pop (2))
(defindent print-unreadable-object ((&whole 4 1 &rest 1) &body))
(defindent prog ((&whole 4 1 &rest 1) &rest lisp-indent-tagbody))
(defindent prog* ((&whole 4 1 &rest 1) &rest lisp-indent-tagbody))
(defindent prog1 (2 &body))
(defindent prog2 (2 2 &body))
(defindent progn (&body))                                        ; default
(defindent progv (4 4 &body))                                    ; default
(defindent psetf (7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7))
(defindent psetq (7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7 9 7))
(defindent push (&body))
(defindent pushnew (&body))
(defindent quote (2))
(defindent remf (2 2))
(defindent restart-bind ((&whole 4 &rest 1) &body))
(defindent restart-case (4 &rest (&whole 2 (&whole 4 1 &rest 1) &body)))
(defindent return (&body))                                       ; default
(defindent return-from (2 &body))
(defindent rotatef (&body))
(defindent setf (6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6))
(defindent setq (6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6 8 6))
(defindent shiftf (&body))
(defindent space (2))
(defindent step (2))
(defindent symbol-macrolet ((&whole 4 &rest (&whole 1 2 &rest 2)) &body))
(defindent tagbody lisp-indent-tagbody)                          ; default
(defindent the (4 2))
(defindent the-environment ())
(defindent throw (4 &body))                                      ; default
(defindent time (2))
(defindent trace (&body))
(defindent typecase (4 &rest (&whole 2 2 2 2 2 2 2 2 2 2 2)))
(defindent unless (4 &body))                                     ; default
(defindent untrace (&body))
(defindent unwind-protect (2 &body))
(defindent when (4 &body))                                       ; default
(defindent with-accessors ((&whole 4 1 &rest 1) 4 &body))
(defindent with-condition-restarts (4 4 &body))
(defindent with-hash-table-iterator (4 &body))
(defindent with-input-from-string ((&whole 4 1 &rest 1) &body))
(defindent with-keyboard (&body))
(defindent with-open-file ((&whole 4 1 &rest 1) &body))
(defindent with-open-stream (4 &body))
(defindent with-output-to-printer ((&whole 4 1 &rest 1) &body))
(defindent with-output-to-string ((&whole 4 1 &rest 1) &body))
(defindent with-package-iterator ((&whole 4 1 &rest 1) &body))
(defindent with-restarts ((&whole 4 &rest (&whole 2 (&whole 4 1 &rest 1) &body)) &body))
(defindent with-simple-restart ((&whole 4 1 &rest 1) &body))
(defindent with-slots ((&whole 4 1 &rest 1) 4 &body))
(defindent with-standard-io-syntax (&body))
(defindent without-floating-point-underflow (&body))

;;; Permit the user to load this file using (require 'clisp-indent).
(provide 'clisp-indent)
