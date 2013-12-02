;;; Common stuff for the demos
;;; Copyright (C) 1999-2005 by Sam Steingold (sds@gnu.org)
;;; GPL2 is applicable

(defpackage "CLX-DEMOS"
  (:use "COMMON-LISP" "XLIB" "EXT")
  (:shadowing-import-from "XLIB" "CHAR-WIDTH") ; EXT has CHAR-WIDTH
  (:export "KOCH" "QIX" "SOKOBAN"))

(in-package :clx-demos)

(defun x-host-display (&optional (disp (getenv "DISPLAY")))
  "Parse the DISPLAY environment variable.
Return 3 values: host, server, screen."
  (if disp
      (let* ((pos1 (position #\: disp))
             (pos2 (and pos1 (position #\. disp :start pos1))))
        (values (subseq disp 0 pos1)
                (if pos1 (parse-integer (subseq disp (1+ pos1) pos2)) 0)
                (if pos2 (parse-integer (subseq disp (1+ pos2))) 0)))
      (values "" 0 0)))

(defun x-open-display ()
  "Open the appropriate X display."
  (multiple-value-bind (host di) (x-host-display)
    (xlib:open-display host :display di)))

(require "koch" (list (make-pathname :name "koch" :defaults *load-truename*)))
(require "qix" (list (make-pathname :name "qix" :defaults *load-truename*)))
(require "sokoban" (list (make-pathname :name "sokoban" :defaults *load-truename*)))
