(in-package "EXT")
(export '(ethe letf letf* with-collect compiled-file-p))
(in-package "SYSTEM")
;;; ---------------------------------------------------------------------------
;;; Wie THE, nur dass auch im compilierten Code der Typtest durchgeführt wird.
(defmacro ethe (typespec form)
  (let ((g (gensym)))
    `(THE ,typespec
       (LET ((,g (MULTIPLE-VALUE-LIST ,form)))
         (IF (SYS::%THE ,g ',(type-for-discrimination typespec))
           (VALUES-LIST ,g)
           (ERROR-OF-TYPE 'ERROR ; 'TYPE-ERROR ??
             (TEXT "The form ~S yielded ~:[no values~;~:*~{~S~^ ; ~}~] ,~@
                       that's not of type ~S.")
             ',form ,g ',typespec
) )  ) ) ) )
;;; ---------------------------------------------------------------------------
; Macro LETF / LETF* wie LET, LET*, nur dass als "Variable" beliebige Places
; (wie bei SETF) zugelassen sind, inklusive VALUES, VALUES-LIST.

; (LETF ((A form)) ...) --> (LET ((A form)) ...)

; (LETF (((CAR A) form)) ...)
;   --> (LET* ((#:G1 A)
;              (#:G2 (CAR #:G1))
;              (#:G3 form))
;         (UNWIND-PROTECT
;           (PROGN (SYSTEM::%RPLACA #:G1 #:G3) ...)
;           (SYSTEM::%RPLACA #:G1 #:G2)
;       ) )

; (LETF (((VALUES A B) form)) ...) --> (MULTIPLE-VALUE-BIND (A B) form ...)

; (LETF (((VALUES (CAR A) (CDR B)) form)) ...)
;   --> (LET* ((#:G1 A)
;              (#:G2 (CAR #:G1))
;              (#:G3 B)
;              (#:G4 (CDR #:G3)))
;         (MULTIPLE-VALUE-BIND (#:G5 #:G6) form
;           (UNWIND-PROTECT
;             (PROGN (SYSTEM::%RPLACA #:G1 #:G5) (SYSTEM::%RPLACD #:G3 #:G6)
;                    ...
;             )
;             (SYSTEM::%RPLACA #:G1 #:G2) (SYSTEM::%RPLACA #:G3 #:G4)
;       ) ) )

; (LETF (((VALUES-LIST A) form)) ...)
;   --> (LET ((A (MULTIPLE-VALUE-LIST form))) ...)

(defmacro LETF* (&whole whole-form
                 bindlist &body body &environment env)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body)
    (let ((declare (if declarations `((DECLARE ,@declarations)) '())))
      (values (expand-LETF* bindlist declare body-rest whole-form env)))))

; expandiert ein LETF*, liefert die Expansion und
; T, falls diese Expansion mit einem LET* anfängt, dessen Bindungsliste
; erweitert werden darf.
(defun expand-LETF* (bindlist declare body whole-form env)
  (if (atom bindlist)
    (if bindlist
      (error-of-type 'source-program-error
        :form whole-form
        :detail bindlist
        (TEXT "LETF* code contains a dotted list, ending with ~S")
        bindlist
      )
      (values `(LET* () ,@declare ,@body) t)
    )
    (let ((bind (car bindlist)) place form)
      (if (atom bind)
        (setq place bind form nil)
        (if (and (consp (cdr bind)) (null (cddr bind)))
          (progn
            (setq place (car bind) form (cadr bind))
            (when (and (consp place) (eq (car place) 'VALUES-LIST) (eql (length place) 2))
              (setq place (second place) form `(MULTIPLE-VALUE-LIST ,form))
            )
            (loop
              (if (and (consp place) (eq (car place) 'THE) (eql (length place) 3))
                (setq place (third place) form `(THE ,(second place) ,form))
                (return)
          ) ) )
          (error-of-type 'source-program-error
            :form whole-form
            :detail bind
            (TEXT "illegal syntax in LETF* binding: ~S")
            bind
      ) ) )
      (multiple-value-bind (rest-expanded flag)
          (expand-LETF* (cdr bindlist) declare body whole-form env)
        (if (and (atom place)
                 (not (and (symbolp place) (nth-value 1 (macroexpand-1 place))))
            )
          (values
            (if flag
              `(LET* ,(cons (list place form) (cadr rest-expanded))
                 ,@(cddr rest-expanded)
               )
              `(LET* ((,place ,form)) ,@declare ,rest-expanded)
            )
            t
          )
          (if (and (consp place) (eq (car place) 'VALUES))
            (if (every #'(lambda (subplace)
                           (and (symbolp subplace)
                                (not (nth-value 1 (macroexpand-1 subplace)))
                         ) )
                       place)
              (values
                `(MULTIPLE-VALUE-BIND ,(cdr place) ,form ,@declare ,rest-expanded)
                nil
              )
              (values
                (do ((bindlist nil)
                     (storetemps nil)
                     (stores1 nil)
                     (stores2 nil)
                     (subplacesr (cdr place)))
                    ((atom subplacesr)
                     `(LET* ,(nreverse bindlist)
                        ,@declare
                        (MULTIPLE-VALUE-BIND ,(nreverse storetemps) ,form
                          ,@declare
                          (UNWIND-PROTECT
                            (PROGN ,@(nreverse stores1) ,rest-expanded)
                            ,@(nreverse stores2)
                    ) ) ) )
                  (multiple-value-bind (SM-temps SM-subforms SM-stores SM-setterform SM-getterform)
                      (get-setf-method (pop subplacesr))
                    (setq bindlist
                      (cons (list (first SM-stores) SM-getterform)
                            (nreconc (mapcar #'list SM-temps SM-subforms) bindlist)
                    ) )
                    (let ((storetemp (gensym)))
                      (setq storetemps (cons storetemp storetemps))
                      ; We can use subst-in-form here, because storetemp is just a variable reference.
                      (setq stores1 (cons (subst-in-form storetemp (first SM-stores) SM-setterform) stores1))
                    )
                    (setq stores2 (cons SM-setterform stores2))
                ) )
                t
            ) )
            (multiple-value-bind (SM-temps SM-subforms SM-stores SM-setterform SM-getterform)
                (get-setf-method place)
              (let ((formvar (gensym)))
                (values
                  `(LET* (,.(mapcar #'list SM-temps SM-subforms)
                          (,(first SM-stores) ,SM-getterform)
                          (,formvar ,form))
                     ,@declare
                     (UNWIND-PROTECT
                       ; We can use subst-in-form here, because formvar is just a variable reference.
                       (PROGN ,(subst-in-form formvar (first SM-stores) SM-setterform) ,rest-expanded)
                       ,SM-setterform
                   ) )
                  t
            ) ) )
) ) ) ) ) )

(defmacro LETF (&whole whole-form
                bindlist &body body &environment env)
  (multiple-value-bind (body-rest declarations) (SYSTEM::PARSE-BODY body)
    (let ((declare (if declarations `((DECLARE ,@declarations)) '()))
          (let-list nil))
      (multiple-value-bind (let*-list let/let*-list uwp-store1 uwp-store2)
          (expand-LETF bindlist whole-form env)
        ; mehrfach folgendes anwenden:
        ; endet let*-list mit (#:G form) und kommt in let/let*-list (var #:G)
        ; vor, so dürfen beide gestrichen werden, und dafür kommt (var form)
        ; an den Anfang von let-list.
        (setq let*-list (nreverse let*-list))
        (loop
          (unless (and (consp let*-list)
                       (let ((last (caar let*-list)))
                         (and (symbolp last) (null (symbol-package last))
                              (dolist (bind let/let*-list nil)
                                (when (eq (second bind) last)
                                  (push (list (first bind) (second (car let*-list)))
                                        let-list
                                  )
                                  (setq let/let*-list
                                    (delete last let/let*-list :key #'second
                                            :test #'eq :count 1
                                  ) )
                                  (setq let*-list (cdr let*-list))
                                  (return t)
                  )    ) )    ) )
            (return)
        ) )
        (setq let*-list (nreverse let*-list))
        ; Nun muss folgendes gemacht werden:
        ; 1. Die Bindungen von let*-list mit LETF* aktivieren,
        ; 2. die Bindungen von let-list mit LET aktivieren,
        ; 3. in beliebiger Reihenfolge:
        ;    a. die Bindungen von let/let*-list mit LET oder LET* aktivieren,
        ;    b. die Bindungen von uwp-store1 mit UNWIND-PROTECT aktivieren
        ;       und danach mit uwp-store2 deaktivieren.
        ; Beispielsweise:
#|      `(LETF* ,let*-list
           ,@declare
           (LET ,let-list
             ,@declare
             (LET* ,let/let*-list
               ,@declare
               `(UNWIND-PROTECT (PROGN ,@uwp-store1 ,@body-rest) ,@uwp-store2)
         ) ) )
|#
        (let ((body body-rest) ; eine Formenliste ohne Deklarationen
              (1form nil)) ; zeigt an, ob body aus einer einzigen Form besteht
          (when uwp-store1
            (setq body `((UNWIND-PROTECT (PROGN ,@uwp-store1 ,@body) ,@uwp-store2))
                  1form t
          ) )
          (when let/let*-list
            (setq body `((LET* ,let/let*-list ,@declare ,@body)) 1form t)
          )
          (when let-list
            (setq body `((LET ,let-list ,@declare ,@body)) 1form t)
          )
          (when let*-list
            (setq body `((LETF* ,let*-list ,@declare ,@body)) 1form t)
          )
          (if (and 1form (or (null declare) (not (eq (caar body) 'unwind-protect))))
            ; eine Form, keine Deklarationen oder fängt mit letf*/let/let* an
            (car body)
            ; allgemein
            `(LET () ,@declare (PROGN ,@body))
) ) ) ) ) )

; expandiert ein LETF, liefert:
; 1. eine Bindungsliste für LETF*,
; 2. eine Bindungsliste für LET/LET* (Reihenfolge der Bindung darin beliebig),
; 3. eine Liste von Bindungsanweisungen,
; 4. eine Liste von Entbindungsanweisungen
; (beide gleich lang).
(defun expand-LETF (bindlist whole-form env)
  (if (atom bindlist)
    (if bindlist
      (error-of-type 'source-program-error
        :form whole-form
        :detail bindlist
        (TEXT "LETF code contains a dotted list, ending with ~S")
        bindlist
      )
      (values '() '() '() '())
    )
    (let ((bind (car bindlist)) place form)
      (if (atom bind)
        (setq place bind form nil)
        (if (and (consp (cdr bind)) (null (cddr bind)))
          (progn
            (setq place (car bind) form (cadr bind))
            (when (and (consp place) (eq (car place) 'VALUES-LIST) (eql (length place) 2))
              (setq place (second place) form `(MULTIPLE-VALUE-LIST ,form))
            )
            (loop
              (if (and (consp place) (eq (car place) 'THE) (eql (length place) 3))
                (setq place (third place) form `(THE ,(second place) ,form))
                (return)
          ) ) )
          (error-of-type 'source-program-error
            :form whole-form
            :detail bind
            (TEXT "illegal syntax in LETF binding: ~S")
            bind
      ) ) )
      (multiple-value-bind (L1 L2 L3 L4) (expand-LETF (cdr bindlist) whole-form env)
        (if (and (atom place)
                 (not (and (symbolp place) (nth-value 1 (macroexpand-1 place))))
            )
          (let ((g (gensym)))
            (values (cons (list g form) L1) (cons (list place g) L2) L3 L4)
          )
          (if (and (consp place) (eq (car place) 'VALUES))
            (if (every #'(lambda (subplace)
                           (and (symbolp subplace)
                                (not (nth-value 1 (macroexpand-1 subplace)))
                         ) )
                       place)
              (let ((gs (mapcar #'(lambda (subplace)
                                    (declare (ignore subplace))
                                    (gensym)
                                  )
                                (cdr place)
                   ))   )
                (values
                  (cons (list (cons 'VALUES gs) form) L1)
                  (nconc (mapcar #'list (cdr place) gs) L2)
                  L3
                  L4
              ) )
              (do ((bindlist nil)
                   (storetemps nil)
                   (stores1 nil)
                   (stores2 nil)
                   (subplacesr (cdr place)))
                  ((atom subplacesr)
                   (values
                     (nreconc bindlist
                              (cons (list (cons 'VALUES storetemps) form) L1)
                     )
                     L2
                     (nreconc stores1 L3)
                     (nreconc stores2 L4)
                  ))
                (multiple-value-bind (SM-temps SM-subforms SM-stores SM-setterform SM-getterform)
                    (get-setf-method (pop subplacesr))
                  (setq bindlist
                    (cons (list (first SM-stores) SM-getterform)
                          (nreconc (mapcar #'list SM-temps SM-subforms) bindlist)
                  ) )
                  (let ((storetemp (gensym)))
                    (setq storetemps (cons storetemp storetemps))
                    ; We can use subst-in-form here, because storetemp is just a variable reference.
                    (setq stores1 (cons (subst-in-form storetemp (first SM-stores) SM-setterform) stores1))
                  )
                  (setq stores2 (cons SM-setterform stores2))
            ) ) )
            (multiple-value-bind (SM-temps SM-subforms SM-stores SM-setterform SM-getterform)
                (get-setf-method place)
              (let ((g (gensym)))
                (values
                  `(,.(mapcar #'list SM-temps SM-subforms)
                    (,(first SM-stores) ,SM-getterform)
                    (,g ,form))
                  L2
                  ; We can use subst-in-form here, because g is just a variable reference.
                  (cons (subst-in-form g (first SM-stores) SM-setterform) L3)
                  (cons SM-setterform L4)
            ) ) )
) ) ) ) ) )

;;; ---------------------------------------------------------------------------
(defmacro with-collect ((&rest collectors) &body forms)
  "Evaluate forms, collecting objects into lists.
Within the FORMS, you can use local macros listed among collectors,
they are returned as multiple values.
E.g., (with-collect (c1 c2) (dotimes (i 10) (if (oddp i) (c1 i) (c2 i))))
 ==> (1 3 5 7 9); (0 2 4 6 8) [2 values]
In CLISP, push/nreverse is about 1.25 times as fast as pushing into the
tail, so this macro uses push/nreverse on CLISP and push into the tail
on other lisps (which is 1.5-2 times as fast as push/nreverse there)."
  #+clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~:@(~s~)-RET-" cc)))
                     collectors)))
    `(let (,@ret)
      (declare (list ,@ret))
      (macrolet ,(mapcar (lambda (co re) `(,co (form) `(push ,form ,',re)))
                         collectors ret)
        ,@forms
        (values ,@(mapcar (lambda (re) `(sys::list-nreverse ,re)) ret)))))
  #-clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~:@(~s~)-RET-" cc)))
                     collectors))
        (tail (mapcar (lambda (cc) (gensym (format nil "~:@(~s~)-TAIL-" cc)))
                      collectors))
        (tmp (mapcar (lambda (cc) (gensym (format nil "~:@(~s~)-TMP-" cc)))
                     collectors)))
    `(let (,@ret ,@tail)
      (declare (list ,@ret ,@tail))
      (macrolet ,(mapcar (lambda (co re ta tm)
                           `(,co (form)
                             `(let ((,',tm (list ,form)))
                               (if ,',re (setf (cdr ,',ta) (setf ,',ta ,',tm))
                                   (setf ,',re (setf ,',ta ,',tm))))))
                         collectors ret tail tmp)
        ,@forms
        (values ,@ret)))))

;;; ---------------------------------------------------------------------------
(defun compiled-file-p (file-name)
  "Return non-NIL is FILE-NAME names a CLISP-compiled file
with compatible bytecodes."
  (with-open-file (in file-name :direction :input :if-does-not-exist nil)
    (handler-bind ((error (lambda (c) (declare (ignore c))
                                  (return-from compiled-file-p nil))))
      (and in (char= #\( (peek-char nil in nil #\a))
           (let ((form (read in nil nil)))
             (and (consp form)
                  (eq (car form) 'SYSTEM::VERSION)
                  (null (eval form))))))))
