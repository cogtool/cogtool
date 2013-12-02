;; -*- Lisp -*-
;; some tests for I18N
;; clisp -q -norc -i ../tests/tests -x '(run-test "i18n/test")'

(listp (show (multiple-value-list (ext:module-info "i18n" t)) :pretty t)) T

(i18n:gettext "foo") "foo"
(i18n:ngettext "abazonk" "abazonk" 12) "abazonk"
(typep (show (i18n:textdomain)) '(or null string)) T
(setf (i18n:textdomain) "zot") "zot"
(typep (show (i18n:textdomaindir "foo")) '(or null pathname)) T
(pathnamep (setf (i18n:textdomaindir "foo") (car (directory "./")))) T

(listp (show (i18n:set-locale) :pretty t)) T

(if (fboundp 'i18n:locale-conv)
    (i18n:locale-conv-p (show (i18n:locale-conv) :pretty t))
    t)
T

(if (fboundp 'i18n:language-information)
    (listp (show (i18n:language-information) :pretty t))
    t)
T
