;;;; Common Lisp Object System for CLISP
;;;; Generic metaobjects
;;;; Bruno Haible 2004-05-29

(in-package "CLOS")

;;; The metaobject abstract class.

(defvar <metaobject> 'metaobject)
(defvar *<metaobject>-defclass*
  '(defclass metaobject ()
     ()))
