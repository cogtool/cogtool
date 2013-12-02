;;;; Querying the user

(in-package "SYSTEM")

; -----------------------------------------------------------------------------

; (Y-OR-N-P [format-string {arg}*]), CLTL S. 407
(defun y-or-n-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    (write-string (TEXT " (y/n) ") *query-io*)
  )
  (let ((localinfo (localized 'y-or-n)))
    (loop
      (let ((line (string-left-trim " " (read-line *query-io*))))
        (when (plusp (length line))
          (let ((first-char (char-upcase (char line 0))))
            (when (member first-char (car localinfo)) (return nil))
            (when (member first-char (cdr localinfo)) (return t))
      ) ) )
      (write-string (TEXT "Please answer with y or n : ") *query-io*)
) ) )
(definternational y-or-n (t ENGLISH))
(deflocalized y-or-n ENGLISH '((#\N) . (#\Y)))

; (YES-OR-NO-P [format-string {arg}*]), CLTL S. 408
(defun yes-or-no-p (&optional format-string &rest args)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string args)
    (write-string (TEXT " (yes/no) ") *query-io*)
  )
  (let ((localinfo (localized 'yes-or-no)))
    (loop
      (clear-input *query-io*)
      (let ((line (string-trim " " (read-line *query-io*))))
        (when (member line (car localinfo) :test #'string-equal) (return nil))
        (when (member line (cdr localinfo) :test #'string-equal) (return t))
      )
      (write-string (TEXT "Please answer with yes or no : ") *query-io*)
) ) )
(definternational yes-or-no (t ENGLISH))
(deflocalized yes-or-no ENGLISH '(("no" "nah") . ("yes" "yup" "yep" "yeah")))

