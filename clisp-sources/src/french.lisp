;;; French translations of DEFINTERNATIONALed values.
;;; Bruno Haible, Jörg Höhle

(in-package "I18N")
(common-lisp:export 'FRANÇAIS)
(common-lisp:import 'FRANÇAIS "EXT")
(common-lisp:export 'FRANÇAIS "EXT")

(common-lisp:in-package "SYSTEM")

(deflanguage FRANÇAIS)

(deflocalized date-format FRANÇAIS
  (formatter "~1{~3@*~D/~4@*~D/~5@*~D ~2@*~2,'0D:~1@*~2,'0D:~0@*~2,'0D~:}")
)
(deflocalized room-format FRANÇAIS
  (list (formatter "Classe~VT instances  taille (octets)  t. moyenne~%")
        (formatter "------~VT ---------  ---------------  ----------~%")
        (formatter       "~VT~8D     ~9D  ~13,3F~%")
) )
(deflocalized space-format FRANÇAIS
  (list (formatter       "~VT     permanent            temporaire~%")
        (formatter "Classe~VTinstances   octets   instances   octets~%")
        (formatter "------~VT--------- ---------  --------- ---------~%")
        (formatter       "~VT~9D ~9D  ~9D ~9D~%")
) )
(deflocalized y-or-n FRANÇAIS '((#\N) . (#\O #\Y)))
(deflocalized yes-or-no FRANÇAIS '(("non") . ("oui")))
(deflocalized print-condition-format FRANÇAIS
  (formatter "Condition exceptionnelle de type ~S.")
)

