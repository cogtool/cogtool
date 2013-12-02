;;; Dutch translations of DEFINTERNATIONALed values.
;;; Bruno Haible, Tijs van Bakel

(in-package "I18N")
(common-lisp:export 'NEDERLANDS)
(common-lisp:import 'NEDERLANDS "EXT")
(common-lisp:export 'NEDERLANDS "EXT")

(common-lisp:in-package "SYSTEM")

(deflanguage NEDERLANDS)

(deflocalized date-format NEDERLANDS
  (formatter "~1{~3@*~D-~4@*~D-~5@*~D ~2@*~2,'0D:~1@*~2,'0D:~0@*~2,'0D~:}")
)
(deflocalized room-format NEDERLANDS
  (list (formatter "Klasse~VT instanties   grootte (bytes)  gemiddeld~%")
        (formatter "------~VT ----------   ---------------  ---------~%")
        (formatter       "~VT~9D     ~11D  ~13,3F~%")
) )
(deflocalized space-format NEDERLANDS
  (list (formatter       "~VT      permanent             tijdelijk~%")
        (formatter "Klasse~VTinstanties    bytes  instanties    bytes~%")
        (formatter "------~VT---------- --------  ---------- --------~%")
        (formatter       "~VT~9D ~9D  ~9D ~9D~%")
) )
(deflocalized y-or-n NEDERLANDS '((#\N) . (#\J #\Y)))
(deflocalized yes-or-no NEDERLANDS '(("neen" "nee" "no") . ("ja" "yes")))
(deflocalized print-condition-format NEDERLANDS
  (formatter "Uitzonderingsgeval van type ~S.")
)

