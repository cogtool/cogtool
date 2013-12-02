;; CLISP disassembler
;; Sam Steingold: converted to CLOS 2001-06-16

(in-package "COMPILER")

(defun orig-fundef (object)
  (unless (fboundp object)
    (error-of-type 'undefined-function
      :name object (TEXT "Undefined function ~S") object))
  (let* ((name (get-funname-symbol object))
         (def (or (get name 'sys::traced-definition)
                  (symbol-function name))))
    (if (macrop def) (macro-expander def) def)))

(defgeneric disassemble (object &key qualifiers specializers)
  (:documentation "disassemble the OBJECT, which should be a function.
if QUALIFIERS or SPECIALIZERS is given, OBJECT should be a generic function.")
  #+UNIX (:method ((object string) &rest junk)
           (declare (ignore junk))
           (disassemble-machine-code
            (sys::program-name) (sys::process-id) nil object))
  (:method ((object method) &key &allow-other-keys)
    (disassemble (method-function object)))
  (:method ((object clos::standard-method) &key &allow-other-keys)
    (disassemble (or (clos::std-method-fast-function object)
                     (clos::std-method-function object))))
  (:method ((object standard-generic-function) &key qualifiers specializers)
    (if (or qualifiers specializers)
        (disassemble (find-method object qualifiers
                                  (mapcar #'find-class specializers)))
        (sys::disassemble-closure object)))
  (:method ((object symbol) &rest opts)
    (apply #'disassemble
           (if (ext:symbol-macro-expand object)
               (coerce `(lambda () ,object) 'function)
               (orig-fundef object))
           opts))
  (:method ((object cons) &rest opts)
    (apply #'disassemble
           (if (function-name-p object)
               (orig-fundef object)
               (coerce (if (eq 'lambda (car object))
                           object `(lambda () ,object))
                       'function))
           opts))
  (:method ((object t) &rest opts)
    (disassemble (coerce object 'function) opts))
  (:method ((object function) &rest junk)
    (declare (ignore junk))
    #+UNIX (when (sys::code-address-of object)
             (return-from disassemble
               (disassemble-machine-code
                (sys::program-name) (sys::process-id) object
                (format nil "0x~X" (sys::code-address-of object)))))
    (unless (sys::closurep object)
      (warn (TEXT "Cannot disassemble natively compiled function ~S") object)
      (return-from disassemble (describe object)))
    ;; the object is a closure.
    (unless (sys::%compiled-function-p object)
      (setq object
            (compile-lambda (sys::closure-name object) ; name
                            (sys::%record-ref object 1) ; lambdabody
                            (sys::%record-ref object 4) ; venv
                            (sys::%record-ref object 5) ; fenv
                            (sys::%record-ref object 6) ; benv
                            (sys::%record-ref object 7) ; genv
                            (sys::%record-ref object 8) ; denv
                            nil)))  ; no error even on failure
    ;; object is a compiled closure.
    (sys::disassemble-closure object) ; disassemble
    nil))

;; Disassemble machine code.
;; Bruno Haible 1995
;; you may customize it to your needs.
#+UNIX
(defun disassemble-machine-code (program-name pid function address)
  ;; This uses gdb.
  (unless (= (shell "gdb --version > /dev/null 2>&1") 0)
    (when function
      ;; Show at least some basic information about the function.
      (describe function))
    (fresh-line)
    (format t (TEXT "Cannot show machine instructions: gdb not found."))
    (return-from disassemble-machine-code nil))
  (unless (stringp address) (setq address (format nil "~A" address)))
  (let ((tempfilename (format nil "/tmp/gdbcomm~D" pid))
        (outfilename (format nil "/tmp/gdbdis~D" pid))
        ;; On Windows older than Windows XP, we cannot use gdb on the live
        ;; process, due to a limitation of the Win32 API.
        ;; See http://sources.redhat.com/ml/cygwin/2003-06/msg00933.html
        (use-live-process *disassemble-use-live-process*))
    (with-open-file (f tempfilename :direction :output)
      ;; inhibit pausing after every 23 lines
      ;; (remove this if your gdb doesn't understand it)
      (format f "set height 100000~%")
      ;; inhibit line breaking (because we filter the lines later)
      (format f "set width 1000~%")
      (when use-live-process
        ;; attach to the lisp.run process
        (format f "attach ~D~%" pid))
      (if (digit-char-p (char address 0))
          ;; disassemble at numerical address
          (format f "disassemble ~A~%" address)
          ;; disassemble at symbolic address (the "disassemble" command
          ;; does not always work for symbolic arguments)
          (format f "x/10000i ~A~%" address))
      (when use-live-process
        ;; let lisp.run continue
        (format f "detach~%"))
      ;; quit the debugger
      (format f "quit~%"))
    ;; Run gdb, capture only the lines beginning with 0x.
    ;; Let lisp.run continue (in case the debugger did not detach properly)
    (shell
      (if use-live-process
        (format nil "~A -n -batch -x ~A ~A < /dev/null | grep '^0' > ~A ; kill -CONT ~D"
                "gdb" tempfilename program-name outfilename pid)
        (format nil "~A -n -batch -x ~A ~A < /dev/null | grep '^0' > ~A"
                "gdb" tempfilename program-name outfilename)))
    (delete-file tempfilename)
    ;; Now let the user view the listing.
    (if (or (string= (getenv "TERM") "dumb")
            (string= (getenv "TERM") "emacs"))
      ;; do not call a pager when running under Emacs
      (with-open-file (in outfilename :direction :input)
        (do ((line (read-line in nil nil) (read-line in nil nil)))
            ((null line))
          (format t "~a~%" line)))
      (shell (format nil "~A ~A" (or (getenv "PAGER") "more") outfilename)))
    (delete-file outfilename))
  #| ;; This uses SunOS dbx. (Untested.)
  (let ((tempfilename (format nil "/tmp/dbxcomm~D" pid)))
    (with-open-file (f tempfilename :direction :output)
      (format f "~A/100i~%" address) ; disassemble
      (format f "detach~%")          ; let lisp.run continue
      (format f "quit~%"))           ; quit the debugger
    (shell (format nil "~A -s ~A ~A ~D" "dbx"
                   tempfilename program-name pid))) |#
  #| ;; This uses AIX dbx. (Untested.)
  (let ((tempfilename (format nil "/tmp/dbxcomm~D" pid)))
    (with-open-file (f tempfilename :direction :output)
      (format f "~A/100i~%" address) ; disassemble
      (format f "detach~%")          ; let lisp.run continue
      (format f "quit~%"))           ; quit the debugger
    (shell (format nil "~A -c ~A -a ~D ~A" "dbx"
                   tempfilename pid program-name))) |#
  nil)
