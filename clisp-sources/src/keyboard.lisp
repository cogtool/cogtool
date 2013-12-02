;; Keyboard stream

(in-package "EXT")
(export '(with-keyboard *keyboard-input*))
(in-package "SYSTEM")

;;;--------------------------------------------------------------------------

(defvar *keyboard-input*)
(defmacro with-keyboard (&body body)
  `(SYS::EXEC-WITH-KEYBOARD (FUNCTION (LAMBDA () (PROGN ,@body))))
)
(defun exec-with-keyboard (fun) ; ABI
  #+WIN32 ; *keyboard-input* existiert schon
    (funcall fun)
  #+UNIX
    (let ((mode nil))
      (unwind-protect
        (progn
          (unless *keyboard-input*
            (setq *keyboard-input* (sys::make-keyboard-stream))
          )
          (setq mode (sys::terminal-raw *terminal-io* t))
          (funcall fun)
        )
        (sys::terminal-raw *terminal-io* mode)
    ) )
)

; Used by spvw.d.
(defun wait-keypress ()
  (with-keyboard (read-char *keyboard-input*))
)

