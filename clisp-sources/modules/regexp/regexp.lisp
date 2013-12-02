;; Module for regular expression searching/matching in CLISP
;; Bruno Haible 14.4.1995, 18.4.1995 -- 2003
;; Sam Steingold 1999-10-28 -- 2005

(defpackage "REGEXP"
  (:documentation
   "POSIX Regular Expressions - matching, compiling, executing.")
  (:use "COMMON-LISP")
  (:import-from "SYS" #:text)
  (:export #:match #:match-start #:match-end #:match-string #:regexp-quote
           #:regexp-matcher
           #:regexp-compile #:regexp-exec #:regexp-split #:with-loop-split))

(in-package "REGEXP")
(pushnew "REGEXP" custom:*system-package-list* :test #'string=)
(pushnew :regexp *features*)

(setf (documentation (find-package "REGEXP") 'sys::impnotes) "regexp-mod")

(defstruct (match (:constructor make-match-boa (start end))
                  (:constructor ))
  start end)

;; The following implementation of MATCH compiles the pattern
;; once for every search.
(defun match-once (pattern string &key (start 0) (end nil)
                                       (extended nil) (ignore-case nil)
                                       (newline nil) (nosub nil)
                                       (notbol nil) (noteol nil))
  (regexp-exec (regexp-compile pattern :extended extended
                                       :ignore-case ignore-case
                                       :newline newline :nosub nosub)
               string :start start :end end :notbol notbol :noteol noteol))

;; The following implementation of MATCH compiles the pattern
;; only once per Lisp session, if it is a literal string.
(defmacro match (pattern string &rest more-forms)
  (if (stringp pattern)
    `(%MATCH (MATCHER ,pattern) ,string ,@more-forms)
    `(MATCH-ONCE ,pattern ,string ,@more-forms)))

(defmacro matcher (pattern)
  (declare (string pattern))
  `(LOAD-TIME-VALUE (%MATCHER ,pattern)))
(defun %matcher (pattern)
  (cons pattern (make-array '(2 2 2 2))))

(defun %match (patternbox string &key (start 0) (end nil)
                                      (extended nil) (ignore-case nil)
                                      (newline nil) (nosub nil)
                                      (notbol nil) (noteol nil))
  ;; Compile the pattern, if not already done.
  (let ((compiled-pattern
          (aref (cdr patternbox) (if extended 0 1) (if ignore-case 0 1)
                                 (if newline 0 1) (if nosub 0 1))))
    (unless (and compiled-pattern #+ffi (ffi:validp compiled-pattern))
      (setq compiled-pattern (regexp-compile (car patternbox)
                                             :extended extended
                                             :ignore-case ignore-case
                                             :newline newline :nosub nosub))
      (setf (aref (cdr patternbox) (if extended 0 1) (if ignore-case 0 1)
                                   (if newline 0 1) (if nosub 0 1))
            compiled-pattern))
    (regexp-exec compiled-pattern string :start start :end end
                                         :notbol notbol :noteol noteol)))

;; Convert a match (of type MATCH) to a substring.
(defun match-string (string match)
  (let ((start (match-start match))
        (end (match-end match)))
    (make-array (- end start)
                :element-type 'character
                :displaced-to string
                :displaced-index-offset start)))

;; Utility function
(defun regexp-quote (string &optional extended)
  (let ((qstring (make-array 10 :element-type 'character
                                :adjustable t :fill-pointer 0)))
    (map nil (if extended
               (lambda (c)
                 (case c
                   ((#\$ #\^ #\. #\* #\[ #\] #\\ #\+ #\?)
                    (vector-push-extend #\\ qstring)))
                 (vector-push-extend c qstring))
               (lambda (c)
                 (case c
                   ((#\$ #\^ #\. #\* #\[ #\] #\\)
                    (vector-push-extend #\\ qstring)))
                 (vector-push-extend c qstring)))
         string)
    qstring))

(defun regexp-split (pattern string &key (start 0) (end nil)
                                         (extended nil) (ignore-case nil)
                                         (newline nil) (nosub nil)
                                         (notbol nil) (noteol nil))
  "Split the STRING by the regexp PATTERN.
Return a list of substrings of STRINGS."
  (loop
    :with compiled =
            (if (stringp pattern)
              (regexp-compile pattern :extended extended
                                      :ignore-case ignore-case
                                      :newline newline :nosub nosub)
              pattern)
    :for match = (regexp-exec compiled string :start start :end end
                                              :notbol notbol :noteol noteol)
    :collect
      (make-array (- (if match (match-start match) (length string)) start)
                  :element-type 'character
                  :displaced-to string
                  :displaced-index-offset start)
    :while match
    :do (let ((new-start (match-end match)))
          (when (= start new-start)
            (error (TEXT "~S: ~S matches an empty string ~S at ~S:~D")
                   'regexp-split pattern match string start))
          (setq start new-start))))

(defmacro with-loop-split ((var stream pattern
                            &key (start 0) end
                            (extended nil) (ignore-case nil)
                            (newline nil) (nosub nil)
                            (notbol nil) (noteol nil))
                           &body forms)
  "Read from STREAM one line at a time, binding VAR to the split line.
The line is split with REGEXP-SPLIT using PATTERN."
  (ext:with-gensyms ("WLS-" compiled-pattern line nb ne st be en)
    `(LOOP
       :WITH ,compiled-pattern =
         (IF (STRINGP ,pattern)
           (REGEXP-COMPILE ,pattern :EXTENDED ,extended
                                    :IGNORE-CASE ,ignore-case
                                    :NEWLINE ,newline :NOSUB ,nosub)
           ,pattern)
       :AND ,ne = ,noteol
       :AND ,nb = ,notbol
       :AND ,st = ,stream
       :AND ,be = ,start
       :AND ,en = ,end
       :AND ,var
       :FOR ,line = (READ-LINE ,st NIL NIL)
       :WHILE ,line
       :DO (SETQ ,var
             (REGEXP-SPLIT ,compiled-pattern ,line :START ,be :END ,en
                                                   :NOTBOL ,nb :NOTEOL ,ne))
      ,@forms)))

(defun regexp-matcher (pattern)
  "A valid value for *APROPOS-MATCHER* in the UTF-8 locale."
  (let ((compiled (regexp-compile pattern :extended t :ignore-case t)))
    (lambda (name) (regexp-exec compiled name :boolean t))))
