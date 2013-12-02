;;; Automatic choice of encoding for CLISP sources           -*- Emacs-Lisp -*-
;;; Load this file from ~/.emacs or ~/.emacs.el
;;; Tested with Emacs 20 with Mule-UCS, Emacs 21

;; All the *.d and *.lisp sources are in UTF-8 encoding.
(modify-coding-system-alist 'file "\\.\\(d\\|lisp\\)\\'" 'utf-8)

;; For CLISP in `inferior-lisp-mode' under win32
(modify-coding-system-alist 'process "lisp" 'unix)
