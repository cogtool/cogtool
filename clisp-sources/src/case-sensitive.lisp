;;; Case-Sensitive Packages for CLISP
;;; Bruno Haible 2004-07-14
;;; Sam Steingold 2005

(in-package "SYSTEM")

;; From CS-COMMON-LISP export all standard symbols which don't have a
;; case-sensitive variant (like SYMBOL-NAME etc).
(let ((cs-cl-package (find-package "CS-COMMON-LISP")))
  (do-external-symbols (standard-sym "COMMON-LISP")
    (let ((cs-sym (find-symbol (symbol-name standard-sym) cs-cl-package)))
      (if cs-sym
        ;; Copy the property list (important for STRING et al.).
        (setf (symbol-plist cs-sym) (copy-list (symbol-plist standard-sym)))
        ;; Use the standard-sym unmodified.
        (progn
          (import (list standard-sym) cs-cl-package)
          (setq cs-sym standard-sym)))
      (export (list cs-sym) cs-cl-package))))

;; #<PACKAGE CS-COMMON-LISP-USER> is the default case-sensitive user package.
(use-package '("CS-COMMON-LISP" "EXT") "CS-COMMON-LISP-USER")
(pushnew "CS-COMMON-LISP" custom:*system-package-list* :test #'string=)
