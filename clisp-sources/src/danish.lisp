;;; Danish translations of DEFINTERNATIONALed values.
;;; Dennis Decker Jensen

(in-package "I18N")
(common-lisp:export 'DANSK)
(common-lisp:import 'DANSK "EXT")
(common-lisp:export 'DANSK "EXT")

(common-lisp:in-package "SYSTEM")

(deflanguage DANSK)

(deflocalized date-format DANSK
  (formatter "~1{~3@*~D.~4@*~D.~5@*~D ~2@*~2,'0D:~1@*~2,'0D:~0@*~2,'0D~:}"))
(deflocalized room-format DANSK
  (list (formatter "klasse~VT instanser   str. (byte)    middelstr.~%")
        (formatter "------~VT ---------   -------------  ---------~%")
        (formatter       "~VT~8D     ~9D  ~13,3F~%")))
(deflocalized space-format DANSK
  (list (formatter       "~VT     permanent             midlertidigt~%")
        (formatter "klasse~VTinstanser byte       instanser byte~%")
        (formatter "------~VT--------- ---------  --------- ---------~%")
        (formatter       "~VT~9D ~9D  ~9D ~9D~%")))
(deflocalized y-or-n DANSK '((#\N) . (#\J)))
(deflocalized yes-or-no DANSK '(("nej" "niks") . ("ja" "jo")))
(deflocalized print-condition-format DANSK
  (formatter "Undtagelsestilfælde af type ~S."))

