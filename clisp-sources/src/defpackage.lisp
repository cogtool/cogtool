;;; ANSI-compatible definitions
;;; Bruno Haible 21.7.1994
;;; Sam Steingold 1999-2005

;; ============================================================================

(in-package "COMMON-LISP")
(export '(defpackage))
(in-package "SYSTEM")

;; ----------------------------------------------------------------------------

;; X3J13 vote <52>

;; Package-Definition und -Installation, CLtL2 S. 270
(defmacro defpackage (&whole whole-form
                      packname &rest options)
  (setq packname (string packname))
  ;; Process important options:
  (let ((case-sensitive nil) ; flag for :CASE-SENSITIVE
        (case-inverted nil)  ; flag for :CASE-INVERTED
        (modern :DEFAULT))   ; flag for :MODERN
    ;; Process :MODERN first, because it specifies some defaults.
    (dolist (option options)
      (when (listp option)
        (case (first option)
          (:MODERN ; CLISP extension
           (setq modern (second option))
           (setq case-inverted (setq case-sensitive (not (null modern))))))))
    (dolist (option options)
      (when (listp option)
        (case (first option)
          (:CASE-SENSITIVE ; CLISP extension
           (setq case-sensitive (not (null (second option)))))
          (:CASE-INVERTED ; CLISP extension
           (setq case-inverted (not (null (second option))))))))
    (let ((to-string (if case-inverted #'cs-cl:string #'cl:string)))
      ;; Process options:
      (let ((size nil) ; :SIZE has been supplied
            (documentation nil) ; :DOCUMENTATION string
            (nickname-list '()) ; list of nicknames
            (shadow-list '()) ; list of symbol names to shadow
            (shadowing-list '()) ; list of pairs (symbol-name . package-name) for shadowing-import
            (use-list '()) ; list of package-names for use-package
            (use-default "COMMON-LISP") ; default use-list
            (import-list '()) ; list of (symbol-name . package-name) for import
            (intern-list '()) ; list of symbol-names for intern
            (symname-list '()) ; list of all symbol names specified so far
            (export-list '())) ; liste of symbol-names for export
        (flet ((record-symname (name)
                 (if (member name symname-list :test #'string=)
                   (error-of-type 'source-program-error
                     :form whole-form
                     :detail name
                     (TEXT "~S ~A: the symbol ~A must not be specified more than once")
                     'defpackage packname name)
                   (push name symname-list)))
               (modernize (name)
                 ;; MODERN: CL ==> CS-CL
                 (let ((pack (if (packagep name) name
                                 (sys::%find-package (string name)))))
                   (ecase modern
                     ((t) (if (eq pack #.(find-package "COMMON-LISP"))
                              "CS-COMMON-LISP" (package-name pack)))
                     ((nil) (if (eq pack #.(find-package "CS-COMMON-LISP"))
                                "COMMON-LISP" (package-name pack)))
                     ((:DEFAULT) (package-name pack))))))
          (setq use-default (modernize use-default))
          (dolist (option options)
            (if (listp option)
              (if (keywordp (car option))
                (case (first option)
                  (:SIZE
                   (if size
                     (error-of-type 'source-program-error
                       :form whole-form
                       :detail options
                       (TEXT "~S ~A: the ~S option must not be given more than once")
                       'defpackage packname ':SIZE)
                     (setq size t))) ; ignored
                  (:DOCUMENTATION ; ANSI-CL
                   (if documentation
                     (error-of-type 'source-program-error
                       :form whole-form
                       :detail options
                       (TEXT "~S ~A: the ~S option must not be given more than once")
                       'defpackage packname ':DOCUMENTATION)
                     (setq documentation (second option))))
                  (:NICKNAMES
                   (dolist (name (rest option))
                     (push (string name) nickname-list)))
                  (:SHADOW
                   (dolist (name (rest option))
                     (setq name (funcall to-string name))
                     (unless (member name shadow-list :test #'string=)
                       (push name shadow-list)
                       (record-symname name))))
                  (:SHADOWING-IMPORT-FROM
                   (let ((pack (modernize (second option))))
                     (dolist (name (cddr option))
                       (setq name (funcall to-string name))
                       (let ((name+pack (cons name pack)))
                         (unless (member name+pack shadowing-list :test #'equal) ; #'string= on car and cdr
                           (push name+pack shadowing-list)
                           (record-symname name))))))
                  (:USE
                   (dolist (name (rest option))
                     (push (modernize name) use-list))
                   (setq use-default nil))
                  (:IMPORT-FROM
                   (let ((pack (modernize (second option))))
                     (dolist (name (cddr option))
                       (setq name (funcall to-string name))
                       (let ((name+pack (cons name pack)))
                         (unless (member name+pack import-list :test #'equal) ; #'string= on car and cdr
                           (push name+pack import-list)
                           (record-symname name))))))
                  (:INTERN
                   (dolist (name (rest option))
                     (setq name (funcall to-string name))
                     (unless (member name intern-list :test #'string=)
                       (push name intern-list)
                       (record-symname name))))
                  (:EXPORT
                   (dolist (name (rest option))
                     (setq name (funcall to-string name))
                     (unless (member name export-list :test #'string=)
                       (push name export-list))))
                  (:CASE-SENSITIVE) ; CLISP extension, already handled above
                  (:CASE-INVERTED) ; CLISP extension, already handled above
                  (:MODERN) ; CLISP extension, already handled above
                  (T (error-of-type 'source-program-error
                       :form whole-form
                       :detail (first option)
                       (TEXT "~S ~A: unknown option ~S")
                       'defpackage packname (first option))))
                (error-of-type 'source-program-error
                  :form whole-form
                  :detail option
                  (TEXT "~S ~A: invalid syntax in ~S option: ~S")
                  'defpackage packname 'defpackage option))
              (error-of-type 'source-program-error
                :form whole-form
                :detail option
                (TEXT "~S ~A: not a ~S option: ~S")
                'defpackage packname 'defpackage option)))
          ;; Check for overlaps between intern-list and export-list:
          (setq symname-list intern-list)
          (mapc #'record-symname export-list))
        ;; Reverse lists and apply default values:
        (setq nickname-list (nreverse nickname-list))
        (setq shadow-list (nreverse shadow-list))
        (setq shadowing-list (nreverse shadowing-list))
        (setq use-list (if use-default (list use-default) (nreverse use-list)))
        (setq import-list (nreverse import-list))
        (setq intern-list (nreverse intern-list))
        (setq export-list (nreverse export-list))
        ;; Produce the expansion:
        `(EVAL-WHEN (LOAD COMPILE EVAL)
           (SYSTEM::%IN-PACKAGE ,packname :NICKNAMES ',nickname-list :USE '()
                                :CASE-SENSITIVE ,case-sensitive
                                :CASE-INVERTED ,case-inverted)
           ;; Step 0
           ,@(ecase modern
               ((t)
                `((when (find "COMMON-LISP" (package-use-list ,packname)
                              :test #'string= :key #'package-name)
                    (unuse-package "COMMON-LISP" ,packname)
                    (use-package "CS-COMMON-LISP" ,packname))))
               ((nil)
                `((when (find "CS-COMMON-LISP" (package-use-list ,packname)
                              :test #'string= :key #'package-name)
                    (unuse-package "CS-COMMON-LISP" ,packname)
                    (use-package "COMMON-LISP" ,packname))))
               ((:DEFAULT) '()))
           ;; Step 1
           ,@(if shadow-list
               `((,(if case-inverted 'CS-CL:shadow 'CL:SHADOW)
                   ',shadow-list ,packname)))
           ,@(mapcar #'(lambda (pair)
                         `(SHADOWING-IMPORT-CERROR ,(car pair) ,(cdr pair)
                                                   ,case-inverted ,packname))
                     shadowing-list)
           ;; Step 2
           ,@(if use-list `((USE-PACKAGE ',use-list ,packname)))
           ;; Step 3
           ,@(mapcar #'(lambda (pair)
                         `(IMPORT-CERROR ,(car pair) ,(cdr pair)
                                         ,case-inverted ,packname))
                     import-list)
           ,@(mapcar #'(lambda (symname)
                         `(,(if case-inverted 'CS-CL:intern 'CL:INTERN)
                            ,symname ,packname))
                     intern-list)
           ;; Step 4
           ,@(if export-list
               `((INTERN-EXPORT ',export-list ,packname ,case-inverted)))
           ;; Step 5
           ,@(if documentation
               `((SETF (SYS::PACKAGE-DOCUMENTATION (FIND-PACKAGE ,packname))
                       ,documentation)))
           (FIND-PACKAGE ,packname))))))

; Hilfsfunktionen:
(defun find-symbol-cerror (string packname invert calling-packname)
  (multiple-value-bind (sym found)
      (if invert
        (cs-cl:find-symbol string packname)
        (cl:find-symbol string packname))
    (unless found
      (cerror ; 'package-error ??
              (TEXT "This symbol will be created.")
              (TEXT "~S ~A: There is no symbol ~A::~A .")
              'defpackage calling-packname packname string)
      (setq sym (if invert
                  (cs-cl:intern string packname)
                  (cl:intern string packname))))
    sym))
(defun shadowing-import-cerror (string packname invert calling-packname) ; ABI
  (let ((sym (find-symbol-cerror string packname invert calling-packname)))
    (shadowing-import (or sym '(NIL)) calling-packname)))
(defun import-cerror (string packname invert calling-packname) ; ABI
  (let ((sym (find-symbol-cerror string packname invert calling-packname)))
    (import (or sym '(NIL)) calling-packname)))
(defun intern-export (string-list packname invert) ; ABI
  (export (mapcar #'(lambda (string)
                      (if invert
                        (cs-cl:intern string packname)
                        (cl:intern string packname)))
                  string-list)
          packname))
