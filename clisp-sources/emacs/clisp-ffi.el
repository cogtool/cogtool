;;; syntax highlighting for CLISP FFI forms
;;; Load this file from ~/.emacs or ~/.emacs.el

(font-lock-add-keywords
 'lisp-mode
 '(("(\\(def-\\(\\(call\\(\\s_\\|\\sw\\)*\\)\\|\\(c-const\\)\\|\\(c-var\\)\\|\\(c-enum\\|c-struct\\|c-type\\)\\)\\)\\>[ \t'\(]*\\(\\(\\s_\\|\\sw\\)*\\)"
    (1 font-lock-keyword-face)
    (8 (cond ((match-beginning 3) font-lock-function-name-face)
             ((match-beginning 5) font-lock-variable-name-face)
             (t font-lock-type-face)) nil t))))

;; convert between lisp-style-symbols and ICantReadThis

(defun cantread-to-lisp-1 (name)
  "Convert ICantReadThis to i-cant-read-this"
  (let ((case-fold-search nil))
    (while (string-match "\\([a-z]\\)\\([A-Z]\\)" name)
      (setq name (replace-match "\\1-\\2" t nil name))))
  (downcase name))
(defun cantread-to-lisp (&optional point)
  "Convert the symbol-at-point from ICantReadThis to i-cant-read-this"
  (interactive "d")
  (goto-char point)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (name (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (delete-region (car bounds) (cdr bounds))
    (insert (cantread-to-lisp-1 name))))

(defun lisp-to-cantread-1 (name)
  "Convert i-cant-read-this to ICantReadThis"
  (delete ?\- (capitalize name)))
(defun lisp-to-cantread (&optional point)
  "Convert the symbol-at-point from i-cant-read-this to ICantReadThis"
  (interactive "d")
  (goto-char point)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (name (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (delete-region (car bounds) (cdr bounds))
    (insert (lisp-to-cantread-1 name))))
