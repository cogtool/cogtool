;; -*- Lisp -*-
;; some tests for READLINE
;; clisp -q -norc -i ../tests/tests -x '(run-test "readline/test")'

(let ((ver-num (and (boundp 'readline:readline-version)
                    readline:readline-version)))
  (format t "~&readline version ~S (~D=0~O=x~X)~%"
          (and (boundp 'readline:library-version) readline:library-version)
          ver-num ver-num ver-num))
nil

(if (boundp 'readline:editing-mode) readline:editing-mode 1) 1
(if (boundp 'readline:insert-mode)  readline:insert-mode 1)  1
(if (boundp 'readline:readline-name) readline:readline-name "CLISP") "CLISP"
(setq readline:readline-name "abazonk") "abazonk"
readline:readline-name                  "abazonk"

(readline:history-stifled-p)    0
(readline:stifle-history 100) NIL
(readline:history-stifled-p)    1
(abs (readline:unstifle-history)) 100 ; 5=>100, 4.2=>-100
(readline:history-stifled-p)    0

(readline:where-history)       0
(readline:history-total-bytes) 0

(defparameter *history-file* "readline-history-file") *history-file*
(readline:write-history *history-file*)            0
(readline:append-history 1000 *history-file*)      0
(readline:read-history *history-file*)             0
(readline:read-history-range *history-file* 0 -1)  0
(readline:history-truncate-file *history-file* 10) 0
(probe-file (delete-file *history-file*))          NIL

(when (zerop (logand readline:readline-state readline:STATE-INITIALIZED))
  (not (zerop (readline:initialize))))
NIL

(readline:resize-terminal) NIL

(multiple-value-bind (rows cols) (readline:get-screen-size)
  (show (list rows cols))
  (readline:set-screen-size rows cols))
NIL

;;; This tests readline-from-string, and indirectly getc-function
(progn
  (defun stuff-string (string)
    "Stuff a string (with NewLine added) to readline buffer"
    (assert (< (length string) 255)) ; stuff-char limit
    (map 'nil (lambda (char) (readline:stuff-char (char-code char))) string)
    (readline:stuff-char (char-code #\NewLine)))
  (defun readline-from-string (string)
    "Run readline:readline, with fake input."
    (stuff-string string)
    (readline:readline ""))
  (readline-from-string "test")) "test"

;;; Bind key and test that function works
(let ((a 0))
  (readline:bind-key (char-code #\t) (lambda (? ??) (incf a)))
  (readline-from-string "test")
  a) 2

;;; Now key is  unbound, but still ignored
(readline:unbind-key (char-code #\t)) 0
(readline-from-string "test") "es"

;;; Bind it back to insert-self
(progn
  (readline:parse-and-bind "\"t\": self-insert")
  (readline-from-string "test"))
"test"

(progn
  (stuff-string "(1 2")
  (stuff-string "3 4) 5")
  (read readline:*readline-input-stream*)) (1 2 3 4)

(read readline:*readline-input-stream*) 5
