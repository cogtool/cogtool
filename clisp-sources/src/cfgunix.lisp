#0Y UTF-8 ;;;  This file is Unicode/UTF-8 encoded.  -*- coding: utf-8 -*-

;;; Site specific definitions, to be modified on installation

(in-package "EXT")
(mapcar #'fmakunbound '(short-site-name long-site-name))

(let ((cache nil))
(defun short-site-name ()
  (if cache cache
      (setq cache
            (or (getenv "ORGANIZATION")
                (with-open-stream (s (make-pipe-input-stream "uname -n"))
                  (read-line s)))))))

(let ((cache nil))
(defun long-site-name ()
  (if cache cache
      (setq cache
            (or (getenv "ORGANIZATION")
                (with-open-stream (s (make-pipe-input-stream "uname -a"))
                  (read-line s)))))))

(defparameter *editor* "vi" "The name of the editor.")
(defun editor-name () (or (getenv "EDITOR") *editor*))

(defun edit-file (file)
  "(edit-file file) edits a file."
  (open file :direction :probe :if-does-not-exist :create)
  (shell (format nil "~A ~A" (editor-name) (truename file))))

(defun editor-tempfile ()
  "The temporary file LISP creates for editing."
  (merge-pathnames "lisptemp.lisp" (user-homedir-pathname)))

(defparameter *load-paths*
  '(#"./"           ; in the current directory
    "~/lisp/**/")   ; in all directories below $HOME/lisp
  "The list of directories where programs are searched on LOAD etc.")

;; This makes screen output prettier:
(setq *print-pretty* t)

;; This perhaps makes pathname parsing more intuitive:
;;  ".clisprc" --> #S(pathname :name ".clisprc" :type nil)
(setq *parse-namestring-dot-file* :name)

;; which browser do you use? (see `*browsers*' in clhs.lisp)
;; (setq *browser* :mozilla-remote)

(defvar *impnotes-root-default* "http://clisp.cons.org/impnotes/")
(defun impnotes-root ()
  "This returns the root URL for the CLISP implementation notes.
You can set the environment variable `IMPNOTES' or redefine this function
in ~/.clisprc.  On win32 you can also use the Registry."
  (or (getenv "IMPNOTES") *impnotes-root-default*))

;; Common Lisp HyperSpec access
(defvar *clhs-root-default*)
(defun clhs-root ()
  "This returns the root URL for the Common Lisp HyperSpec.
You can set the environment variable `CLHSROOT' or redefine this function
in ~/.clisprc.  On win32 you can also use the Registry."
  (or (getenv "CLHSROOT") *clhs-root-default*))
