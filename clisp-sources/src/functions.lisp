;;; Utilities for function objects
;;; Sam Steingold 2001-2005
;;; Bruno Haible 2004

(in-package "COMMON-LISP")
(export '(function-lambda-expression))
(in-package "SYSTEM")

;; The signature of a function object.
(defstruct (signature (:type vector) (:conc-name sig-))
  ;; (name nil     :type (or symbol cons))
  (req-num 0    :type fixnum)
  (opt-num 0    :type fixnum)
  (rest-p nil   :type boolean)
  (keys-p nil   :type boolean)
  (keywords nil :type list)
  (allow-p nil  :type boolean))

;; X3J13 vote <88>
;; function --> lambda expression, CLtL2 p. 682
(defun function-lambda-expression (obj)
  (setq obj (coerce obj 'function))
  (cond #+FFI
        ((eq (type-of obj) 'FFI::FOREIGN-FUNCTION)
         (values nil nil (sys::%record-ref obj 0)))
        ((sys::subr-info obj)
         (values nil nil (sys::subr-info obj)))
        ((sys::%compiled-function-p obj) ; compiled closure?
         (let* ((name (sys::closure-name obj))
                (def (get (if (symbolp name)
                              name (get (second name) 'sys::setf-function))
                          'sys::definition)))
           (values (when def (cons 'LAMBDA (cddar def))) t name)))
        ((sys::closurep obj) ; interpreted closure?
         (values (cons 'LAMBDA (sys::%record-ref obj 1)) ; lambda-expression without docstring
                 (vector ; environment
                         (sys::%record-ref obj 4) ; venv
                         (sys::%record-ref obj 5) ; fenv
                         (sys::%record-ref obj 6) ; benv
                         (sys::%record-ref obj 7) ; genv
                         (sys::%record-ref obj 8)); denv
                 (sys::closure-name obj))))) ; name

(defun function-name (obj)
  ;; Equivalent to (nth-value 2 (function-lambda-expression obj))
  (setq obj (coerce obj 'function))
  (cond #+FFI
        ((eq (type-of obj) 'FFI::FOREIGN-FUNCTION)
         (sys::%record-ref obj 0))
        ((sys::subr-info obj))
        ((sys::%compiled-function-p obj) ; compiled closure?
         (sys::closure-name obj))
        ((sys::closurep obj) ; interpreted closure?
         (sys::closure-name obj))))

;; Returns the function definition of a function name, ignoring wrappers
;; installed by TRACE, profilers etc.
(defun unwrapped-fdefinition (funname)
  (let* ((sym (get-funname-symbol funname))
         (def (or (get sym 'sys::traced-definition)
                  (symbol-function sym))))
    (if (macrop def)
      (macro-expander def)
      def)))
