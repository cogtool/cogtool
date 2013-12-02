;; SCREEN is actually conditionally defined in constpack.d,
;; but the condition (in lispbibl.d) is too hairy
;; to duplicate it in makemake.in, so this file is always compiled
;; (even when it is not subsequently loaded by init.lisp),
;; thus we have to use DEFPACKAGE here just in case

(defpackage "SCREEN"
  (:documentation "http://clisp.cons.org/impnotes/screen.html")
  (:use "COMMON-LISP" "EXT")
  (:export ;; exported functions and macros:
   #:make-window #:window-size
   #:window-cursor-position #:set-window-cursor-position
   #:clear-window #:clear-window-to-eot #:clear-window-to-eol
   #:delete-window-line #:insert-window-line
   #:highlight-on #:highlight-off #:window-cursor-on #:window-cursor-off
   #:with-window #:*window*))

(in-package "SCREEN")

(defvar *window*) ; ABI

(defmacro with-window (&body body)
  `(LET ((*WINDOW* (MAKE-WINDOW)))
     (UNWIND-PROTECT (PROGN ,@body) (CLOSE *WINDOW*))))
