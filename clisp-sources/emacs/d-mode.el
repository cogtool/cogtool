;;; Copyright (C) 2001-2004 by Sam Steingold
;;; released under the GNU GPL <http://www.gnu.org/copyleft/gpl.html>
;;; as a part of CLISP <http://clisp.cons.org>
;;;
;;; the mode for editing CLISP *.d files
;;; add the following to your ~/.emacs.el
;;; (setq auto-mode-alist (cons '("\\.d\\'" . d-mode) auto-mode-alist))
;;; (autoload 'd-mode "/usr/local/src/clisp/emacs/d-mode")

(require 'cc-mode)
(require 'cc-fonts)
(require 'cl)                   ; `subst'

(defun d-mode-translate-word (word)
  "translate the word at point from German into English"
  (interactive (list (read-string "translate: " (thing-at-point 'word))))
  (shell-command (format "wordtrans -d de-en -i %s" word)))

(defun d-mode-find-C-def (sym)
  (interactive
   (let ((sym (or (thing-at-point 'symbol)
                  (save-excursion
                    (skip-syntax-backward "^w")
                    (unless (bobp) (backward-char 1))
                    (thing-at-point 'symbol) ""))))
     (list (read-string
            "function name: "
            (if (string-match ":\\([^:]+\\)$" sym)
                (match-string 1 sym) sym)))))
  (let* ((c-name
          (with-current-buffer (find-file-noselect "constsym.d")
            (goto-char (point-min))
            (search-forward (concat "\"" (upcase sym) "\""))
            (buffer-substring-no-properties
             (scan-sexps (point) -2) (1- (scan-sexps (point) -1)))))
         (c-def
          (with-current-buffer (find-file-noselect "subr.d")
            (goto-char (point-min))
            (search-forward (concat "(" c-name ","))
            (buffer-substring-no-properties
             (line-beginning-position) (point)))))
    (message "C-name: %s; c-def: %s" c-name c-def)
    (tags-search (concat "^" c-def))))

(defun d-mode-current-defun-function ()
  "Return the name of the current function."
  (save-excursion
    (d-mode-beg-of-defun)
    (cond ((looking-at "LISP")
           (search-forward "(")
           (let ((c-name (buffer-substring-no-properties
                          (point) (scan-sexps (point) 1))))
             (set-buffer (find-file-noselect "constsym.d"))
             (save-excursion
               (goto-char (point-min))
               (search-forward (concat "LISPSYM(" c-name ","))
               (buffer-substring-no-properties
                (1+ (point)) (1- (scan-sexps (point) 1))))))
          ((looking-at "DEFUN")
           (re-search-forward "([-A-Za-Z]*::?\\([^ ,]+\\)")
           (match-string 1))
          ((looking-at "\\(local\\|global\\)")
           (search-forward "(") (forward-char -1)
           (let ((beg (scan-sexps (point) -1)))
             (buffer-substring-no-properties beg (scan-sexps beg 1))))
          ((looking-at "nonreturning_function")
           (search-forward "(") (search-forward "(") (forward-char -1)
           (let ((beg (scan-sexps (point) -1)))
             (buffer-substring-no-properties beg (scan-sexps beg 1))))
          (t ;; (or (looking-at "struct ") (looking-at "#define ")
             ;;     (looking-at "typedef "))
           (let ((add-log-current-defun-function nil))
             (add-log-current-defun))))))

(defun d-mode-beg-of-defun ()
  "A valid value for `beginning-of-defun-function' for `d-mode'."
  (re-search-backward
   (eval-when-compile
    (concat "^" (regexp-opt '("LISPFUN" "LISPSPECFORM" "local " "global "
                              "#define " "nonreturning_function"
                              "typedef " "struct " "DEFUN")
                            t)))
   nil 1))                      ; move to the limit

(defun d-mode-convert-function ()
  "Convert from the old-style to ANSI function definition.
The point should be on the prototype and the definition should follow."
  (interactive)
  (let ((beg (point)))
    (end-of-line 1)
    (delete-region (1- (point)) (- (search-forward "{" nil nil) 2))
    (forward-char -1) (insert "\n")
    (c-indent-region beg (progn (backward-char 2) (forward-sexp) (point)))))

(defun d-mode-convert-lispfun ()
  "Convert the LISPFUN to the new indentation."
  (interactive)
  (search-forward "LISPFUN")
  (beginning-of-line)
  (let ((beg (point)))
    (forward-sexp 2) (insert "\n{ ") (delete-char 1)
    (re-search-forward "^ *{")
    (delete-region (line-beginning-position) (1+ (point)))
    (goto-char beg)
    (c-indent-region beg (progn (forward-sexp 3) (point)))))

(defvar d-comment-start "# ") ; for C++: "// ?"
(defconst d-comment-start-block (concat "[ \t]*\\(" d-comment-start "\\)"))

(defun d-mode-convert-comment ()
  "Comvert the current comment line from # to /**/"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward d-comment-start (line-end-position) t)
      (replace-match "/* ") (end-of-line)
      (if (/= ?\\ (char-before)) (insert " */")
        (forward-char -1) (just-one-space) (insert "*/ ")))))

(defun d-mode-convert-block-comment ()
  "Comvert the current comment block from # to /**/"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (looking-at d-comment-start-block)
      (forward-line -1))
    (replace-match "/* " t t nil 1)
    (forward-line 1)
    (while (looking-at d-comment-start-block)
      (replace-match " " t t nil 1) (forward-line 1))
    (forward-char -1) (skip-chars-backward "\\\\") (just-one-space)
    (insert "*/ ")))

(defun d-mode-convert-next-comment ()
  "Convert the next comment appropriately"
  (interactive)
  (re-search-forward d-comment-start)
  (if (progn (beginning-of-line) (looking-at d-comment-start-block))
      (d-mode-convert-block-comment)
      (d-mode-convert-comment) (comment-indent)))

(defun d-mode-wrap-do-while ()
  "Wrap this block in do/while(0) [for CPP macros]."
  (interactive)
  (insert "do") (just-one-space) (forward-sexp 1) (just-one-space)
  (insert "while(0)"))

(defun d-mode-indent-sharp (s-element)
  "Check whether a macro or a comment and indent accordingly."
  (save-excursion (back-to-indentation)
                  (if (looking-at d-comment-start) 0 [0])))

(defvar d-font-lock-extra-types
  '(nconc (list "bool" "object" "chart" "[otac]int" "signean" "\\sw+_T"
           "s[aco]int" "[csu]?int[BCDLPQWX0-9]*" "hfint" "fcint" "SPint"
           "[SU]LONG" "[SU]BYTE" "[DSU]WORD" "[SU]LONGLONG" "RET\\sw*TYPE"
           "\\sw+_Pseudofun"
           "Values" "SOCKET" "Handle" "stringarg" "FILETIME")
    c-font-lock-extra-types)
  "Extra types to be fontified as such.")

(defun d-mode-modify-font-lock (form)
  "Modify the font locking spec appropriately."
  (subst d-font-lock-extra-types 'c-font-lock-extra-types
         ;; `d-mode' should highlight #foo not only at the beginning-of-line
         (if (and (consp form) (stringp (car form))
                  (= ?^ (aref (car form) 0))
                  (= ?# (aref (car form) 1)))
             (cons (concat "^[ \t]*" (substring (car form) 1)) (cdr form))
             form)))

(defvar d-extra-keywords
  (eval-when-compile
   (regexp-opt '("var" "local" "global" "true" "false" "NIL" "T" "loop"
                 "inline" "NULL" "nullobj" "maygc"
                 "popSTACK" "pushSTACK" "skipSTACK" "skipSTACKop" "STACKop"
                 "dotimespC" "dotimesC" "dotimespL" "dotimesL" "dotimespW"
                 "dotimesW" "nonreturning_function" "return_Values"
                 "SstringDispatch" "SstringCase")
               'words)))

(defvar d-font-lock-keywords-1
  (mapcar #'d-mode-modify-font-lock c-font-lock-keywords-1))

(defvar d-font-lock-keywords-2
  (cons d-extra-keywords
        (mapcar #'d-mode-modify-font-lock c-font-lock-keywords-2)))

(defvar d-font-lock-keywords-3
  (cons d-extra-keywords
        (mapcar #'d-mode-modify-font-lock c-font-lock-keywords-3)))

(defvar d-font-lock-keywords d-font-lock-keywords-1)

(defun d-mode-add-font-locking (default)
  (cons (list 'd-font-lock-keywords 'd-font-lock-keywords-1
              'd-font-lock-keywords-2 'd-font-lock-keywords-3)
        (cdr default)))

(defvar d-mode-font-lock-defaults
  (d-mode-add-font-locking
   (if (boundp 'running-xemacs) (get 'c-mode 'font-lock-defaults)
       (cdr (assq 'c-mode font-lock-defaults-alist))))
  "The `font-lock-defaults' for `d-mode'.")

(defvar d-mode-build-dir "../build/"
  "*The build directory to look at when there is not makefile in src.")

;; from Martin Stjernholm <mast@lysator.liu.se>
;; Date: 26 May 2002 17:34:21 +0200
;; restore the indentation to pre-e21.4
(defun d-indent-to-boi (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (back-to-indentation)
    (vector (current-column))))

(defun d-indent-to-boi+offset (langelem)
  (save-excursion
    (goto-char (cdr langelem))
    (back-to-indentation)
    (vector (+ (current-column) c-basic-offset))))

(define-derived-mode d-mode c-mode "D"
  "Major mode for editing CLISP source code.
Special commands:
\\{d-mode-map}
Turning on D mode calls the value of the variable `d-mode-hook',
if that value is non-nil.
If you are using Emacs 20.2 or earlier (including XEmacs) and want to
use fontifications, you have to (require 'font-lock) first.  Sorry.
Beware - this will modify the original C-mode too!"
  (set (make-local-variable 'compile-command)
       (let* ((target (if (eq window-system 'w32) "lisp.exe" "lisp.run"))
              build-dir
              (make (if (eq window-system 'w32) "nmake" "make"))
              (makefile
               (cond ((file-readable-p "Makefile") nil)
                     ((file-readable-p "makefile") nil)
                     ((file-readable-p "makefile-msvc") "makefile-msvc")
                     ((file-readable-p "makefile.msvc") "makefile.msvc")
                     ((file-readable-p "makefile.msvc5") "makefile.msvc5")
                     ((file-readable-p "Makefile.msvc5") "Makefile.msvc5")
                     ((file-readable-p "makefile-msvs") "makefile-msvs")
                     ((file-readable-p "makefile-gcc")
                      (setq make "make") "makefile-gcc")
                     ((file-directory-p d-mode-build-dir)
                      (setq build-dir d-mode-build-dir make "make")
                      "Makefile")
                     (t nil))))
         (if build-dir
             (concat make " -C " build-dir " -f " makefile " " target)
             (if makefile
                 (concat make " -f " makefile " " target)
                 (concat make " " target)))))
  (set (make-local-variable 'add-log-current-defun-function)
       'd-mode-current-defun-function)
  (c-set-offset 'cpp-macro 'd-mode-indent-sharp)
  (c-set-offset 'block-close 'd-indent-to-boi)
  (c-set-offset 'statement-block-intro 'd-indent-to-boi+offset)
  ;; (setq defun-prompt-regexp
  ;; "^\\(LISPFUNN?(.*) \\|\\(local\\|global\\|nonreturning_function\\) .*\\)")
  (set (make-local-variable 'beginning-of-defun-function)
       'd-mode-beg-of-defun)
  (when (<= 21 emacs-major-version)
    (set (make-local-variable 'font-lock-defaults)
         d-mode-font-lock-defaults)))

(when window-system
  ;; enable font locking
  (if (boundp 'running-xemacs)
      (put 'd-mode 'font-lock-defaults d-mode-font-lock-defaults)
      (when (and (> 21 emacs-major-version)
                 (null (assq 'd-mode font-lock-defaults-alist)))
        (setq font-lock-defaults-alist
              (cons (cons 'd-mode d-mode-font-lock-defaults)
                    font-lock-defaults-alist)))))

;; enable CLISP "# foo" comments
(modify-syntax-entry ?# ". 1b" d-mode-syntax-table)
(modify-syntax-entry 32 ; space
                     (if (boundp 'running-xemacs) " 2b" "- 2b")
                     d-mode-syntax-table)
(modify-syntax-entry ?\n "> b" d-mode-syntax-table)
(modify-syntax-entry ?\f "> b" d-mode-syntax-table)

;; put D buffers along with the C buffers in the menus
(when (boundp 'mouse-buffer-menu-mode-groups)
  (push '("\\<D\\>" . "C") mouse-buffer-menu-mode-groups))

;; treat D files like C files for add-log
(eval-after-load "add-log"
  '(add-to-list 'add-log-c-like-modes 'd-mode))

;; some keybindings
(define-key d-mode-map (kbd "<f5>") 'd-mode-convert-next-comment)

;; update the dates in headers
(defvar clisp-home-dir "d:/gnu/clisp/sf/clisp/"
  "*the location of clisp sources for `clisp-update-dates'")
(defvar clisp-update-dates-user user-full-name
  "*default argument for `clisp-update-dates'")
(defun clisp-update-dates (&optional user)
  "Update the dates in file header for the user.
Look at the files that are mentioned in `clisp-home-dir'/src/ChangeLog
as changed by the `user' and check that this date is in that file's header."
  (interactive (list (read-from-minibuffer "User: " clisp-update-dates-user)))
  (setq clisp-update-dates-user user)
  (message "clisp-update-dates: %s" user)
  (let* ((c-l (find-file-noselect (expand-file-name "src/ChangeLog"
                                                    clisp-home-dir)))
         (year (format-time-string "%Y")) start all bad
         (re (concat "^" year "-.*" user)))
    (with-current-buffer c-l
      (save-excursion
        (goto-char 0)
        (while (setq start (re-search-forward re nil t))
          (let ((end (re-search-forward "^[0-9]")))
            (goto-char start)
            (while (re-search-forward "^\t\\* " end t)
              (setq all (nconc (split-string
                                (buffer-substring-no-properties
                                 (point) (re-search-forward
                                          "[:()]" (line-end-position) t))
                                "[ ,():]+" t)
                               all)))))))
    (setq all (delete-dups all))
    (message "clisp-update-dates: %d files: %s" (length all) all)
    (dolist (file all)
      (let ((buf (find-file-noselect
                  (expand-file-name
                   (if (string-match "/" file) file (concat "src/" file))
                   clisp-home-dir))))
        (with-current-buffer buf
          (save-excursion
            (goto-char 0)
            (cond ((null (search-forward user nil t))
                   (message "clisp-update-dates: %s does not mention %s"
                            file user)
                   (push file bad))
                  ((progn (beginning-of-line)
                          (search-forward year (line-end-position) t))
                   (message "clisp-update-dates: %s is good!" file)
                   (kill-buffer buf))
                  (t (message "clisp-update-dates: %s needs updating" file)
                     (push file bad)))))))
    (message "clisp-update-dates: %d files need updating: %s" (length bad) bad)
    (values bad all)))

(provide 'd-mode)
