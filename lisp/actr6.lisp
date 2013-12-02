;; Set clisp options 

(setf *load-verbose* nil)
(setf *compile-verbose nil)
(setf *load-print* nil)
(setf *compile-print nil)
(setf *load-compiling* t)
(setf *floating-point-contagion-ansi* t)
(setf *warn-on-floating-point-contagion* nil)

;; Load ACT-R 6 and EMMA

(load "actr6/load-act-r-6.lisp")
