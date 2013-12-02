;; Module for wildcard matching in CLISP
;; Bruno Haible 18.4.1995
;; Sam Steingold 2001-2005

(defpackage "WILDCARD"
  (:modern t)
  (:use "COMMON-LISP" "FFI")
  (:export #:match #:wildcard-matcher))
(in-package "WILDCARD")
(pushnew "WILDCARD" custom:*system-package-list* :test #'string=)
(pushnew :wildcard *features*)
(setf (documentation (find-package "WILDCARD") 'sys::impnotes) "wildcard")

(default-foreign-language :stdc)

(c-lines "#include <config.h>~%")
(c-lines "#include <fnmatch.h>~%")

(def-call-out fnmatch
    (:arguments (pattern c-string)
                (string c-string)
                (flags int))
  (:return-type int))

;; flags values
(def-c-const FNM_PATHNAME)
(def-c-const FNM_FILE_NAME)
(def-c-const FNM_NOESCAPE)
(def-c-const FNM_PERIOD)
(def-c-const FNM_LEADING_DIR)
(def-c-const FNM_CASEFOLD)

(defun match (pattern string &key (start 0) (end nil) (case-insensitive nil))
  ;; Prepare the string.
  (unless (and (eql start 0) (null end))
    (unless end (setq end (length string)))
    (setq string (make-array (- end start) :element-type 'character
                                           :displaced-to string
                                           :displaced-index-offset start)))
  ;; Match.
  (zerop
    (fnmatch pattern string
             (logior FNM_PATHNAME (if case-insensitive FNM_CASEFOLD 0)))))

(defun wildcard-matcher (pattern)
  "A valid value for *APROPOS-MATCHER*."
  (lambda (name) (zerop (fnmatch pattern name FNM_CASEFOLD))))
