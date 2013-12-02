;;; Russian translations of DEFINTERNATIONALed values.
;;; Sam Steingold

(in-package "I18N")
(common-lisp:export 'РУССКИЙ)
(common-lisp:import 'РУССКИЙ "EXT")
(common-lisp:export 'РУССКИЙ "EXT")

(common-lisp:in-package "SYSTEM")

(deflanguage РУССКИЙ)

(deflocalized date-format РУССКИЙ
  (formatter "~1{~3@*~D/~4@*~D/~5@*~D ~2@*~2,'0D:~1@*~2,'0D:~0@*~2,'0D~:}"))
(deflocalized room-format РУССКИЙ
  (list (formatter "Класс~VT экземпляры размер (байты)  средний размер~%")
        (formatter "-----~VT ---------  ---------------  ------------~%")
        (formatter      "~VT~8D     ~9D  ~13,3F~%")))
(deflocalized space-format РУССКИЙ
  (list (formatter      "~VT    постоянно             временно~%")
        (formatter "Класс~VTэкземпляры   байты   экземпляры  байты~%")
        (formatter "-----~VT---------- ---------  --------- ---------~%")
        (formatter      "~VT~9D ~9D  ~9D ~9D~%")))
(deflocalized y-or-n РУССКИЙ '((#\N #\Н) . (#\Y #\Д)))
(deflocalized yes-or-no РУССКИЙ '(("нет") . ("да")))
(deflocalized print-condition-format РУССКИЙ
  (formatter "Исключение типа ~S."))
