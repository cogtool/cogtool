;;;; Command-line completion hook

(in-package "SYSTEM")

;;-----------------------------------------------------------------------------
;; Completing routine for the GNU Readline library.
;; Input: string (the input line), and the boundaries of the text to be
;; completed:  (subseq string start end)
;; Output: a list of simple-strings. empty, when no meaningful completions.
;; otherwise, CDR = list of all meaningful completions,
;; CAR = the immediate replacement
(let ((state nil))
(defun completion (string start end)
  (let* ((quotedp (and (>= start 1) ; quoted completion?
                       (member (char string (- start 1)) '(#\" #\|))))
         (start1 (if quotedp (1- start) start))
         (functionalp1 (and (>= start1 1)
                            (equal (subseq string (- start1 1) start1) "(")))
         (functionalp2 (and (>= start1 2)
                            (equal (subseq string (- start1 2) start1) "#'")))
         ;; completion of a function or of any symbol?
         (functionalp (or (= start end) functionalp1 functionalp2))
         ;; test for special case: nothing was entered to be completed,
         ;; so we try to DESCRIBE the last function symbol entered
         (void-completion
           (and (= start end)
                (or (>= start (length string))
                    (whitespacep (schar string start))))))
    ;; If nothing useful was entered (just whitespace), print help.
    (when void-completion
      (do ((pos (min end (1- (length string))) (1- pos))
           (depth 0)
           (white end))
          ((or (minusp pos) (plusp depth))
           (setq start (+ pos 2) end white))
        (cond ((char= #\( (schar string pos)) (incf depth))
              ((char= #\) (schar string pos)) (decf depth))
              ((whitespacep (schar string pos)) (setq white pos))))
      (when (< end start)       ; nothing useful was entered - just whitespace
        (sys::help) (terpri)    ; print help
        (return-from completion 0))) ; redraw the prompt
    ;; FIXME: If quotedp is true due to #\", we should better collect matching
    ;;        filenames, not symbols, I think.
    ;; Collect matching symbols.
    (let ((new-state (list* string start end))
          (package *package*)
          (mapfun #'sys::map-symbols)
          (prefix nil))
      ;; Extract the package name:
      (unless quotedp
        (let ((colon (position #\: string :start start :end end)))
          (when colon
            (let ((packname (subseq string start colon)))
              (case (readtable-case *readtable*)
                (:UPCASE (setq packname (string-upcase packname)))
                (:DOWNCASE (setq packname (string-downcase packname)))
                (:INVERT
                  (setq packname
                    (map 'string
                         #'(lambda (c)
                             (cond ((lower-case-p c) (char-upcase c))
                                   ((upper-case-p c) (char-downcase c))
                                   (t c)))
                         packname))))
              (when (equal packname "") (setq packname "KEYWORD"))
              (setq package (find-package packname)))
            (unless package
              (return-from completion nil))
            (incf colon)
            (if (and (< colon end) (eql (char string colon) #\:))
              (incf colon)
              (setq mapfun #'sys::map-external-symbols))
            (setq prefix (subseq string start colon))
            (setq start colon))))
      (let* ((case-sensitive-p
               (or quotedp
                   (package-case-sensitive-p package)
                   (case (readtable-case *readtable*)
                     ((:UPCASE :DOWNCASE) nil)
                     ((:PRESERVE :INVERT) t))))
             ;; FIXME: Handling of (readtable-case *readtable*) = :INVERT is
             ;;        incomplete.
             (case-inverted-p (package-case-inverted-p package))
             (known-part (subseq string start end))
             (known-len (length known-part))
             (char-cmp (if case-sensitive-p #'char= #'char-equal))
             (string-cmp (if case-sensitive-p #'string= #'string-equal))
             (return-list '())
             (gatherer
               (if functionalp
                 #'(lambda (sym)
                     (when (fboundp sym)
                       (let ((name (symbol-name sym)))
                         (when (>= (length name) known-len)
                           (when case-inverted-p
                             (setq name (string-invertcase name)))
                           (when (funcall string-cmp name known-part
                                          :end1 known-len)
                             (push name return-list))))))
                 #'(lambda (sym)
                     (let ((name (symbol-name sym)))
                       (when (>= (length name) known-len)
                         (when case-inverted-p
                           (setq name (string-invertcase name)))
                         (when (funcall string-cmp name known-part
                                        :end1 known-len)
                           (push name return-list))))))))
        (funcall mapfun gatherer package)
        ;; Now react depending on the list of matching symbols.
        (when (null return-list)
          (return-from completion nil))
        (when (and void-completion
                   (< end (length string)) (whitespacep (schar string end)))
          (let ((first-matching-name
                  (find known-part return-list :test string-cmp)))
            (when case-inverted-p
              (setq first-matching-name (string-invertcase first-matching-name)))
            (let ((first-matching-sym (find-symbol first-matching-name package)))
              (return-from completion
                (when (and first-matching-sym (fboundp first-matching-sym))
                      ;; FIXME: why not test (null (cdr return-list)) ?
                  (cond ((equalp state new-state)
                         (describe first-matching-sym) (terpri) (terpri))
                        (t (setq state new-state)))
                  0)))))               ; redraw the prompt
        ;; For a function without arguments, append a closing paren.
        (when (and functionalp1
                   (not quotedp)    ; readline will close the quote after #\) !
                   (null (cdr return-list))
                   (let ((name (car return-list)))
                     (when case-inverted-p
                       (setq name (string-invertcase name)))
                     (let ((sym (find-symbol name package)))
                       (and sym (fboundp sym) (functionp (symbol-function sym))
                            (multiple-value-bind (name req-num opt-num rest-p key-p)
                                (function-signature (symbol-function sym))
                              (declare (ignore name))
                              (and (eql req-num 0) (eql opt-num 0)
                                   (not rest-p) (not key-p)))))))
          (setf (car return-list) (string-concat (car return-list) ")")))
        ;; Downcase a function name.
        (when (and (not quotedp) (not case-sensitive-p))
          (map-into return-list #'string-downcase return-list))
        ;; Sort the return-list.
        (setq return-list (sort return-list #'string<))
        ;; Look for the largest common initial piece.
        (let ((imax (reduce #'min return-list :key #'length)))
          (do ((i 0 (1+ i)))
              ((or (= i imax)
                   (let ((c (char (first return-list) i)))
                     (dolist (s (rest return-list) nil)
                       (unless (funcall char-cmp (char s i) c) (return t)))))
               (push (subseq (first return-list) 0 i) return-list))))
        ;; Reattach prefix consisting of package name and colons.
        (when prefix
          (mapl #'(lambda (l) (setf (car l) (string-concat prefix (car l))))
                return-list))
        return-list))))
)
