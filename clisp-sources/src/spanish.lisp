;;; Spanish translations of DEFINTERNATIONALed values.
;;; Bruno Haible, Carlos Linares

(in-package "I18N")
(common-lisp:export 'ESPAÑOL)
(common-lisp:import 'ESPAÑOL "EXT")
(common-lisp:export 'ESPAÑOL "EXT")

(common-lisp:in-package "SYSTEM")

(deflanguage ESPAÑOL)

(deflocalized y-or-n ESPAÑOL '((#\N) . (#\S #\Y)))
(deflocalized yes-or-no ESPAÑOL '(("no") . ("si")))

