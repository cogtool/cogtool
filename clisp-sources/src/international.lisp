;;;                           INTERNATIONALIZATION

(in-package "I18N")
(cl:export '(deflanguage definternational deflocalized localized english))

(common-lisp:in-package "SYSTEM")
(use-package '("I18N") "EXT")
(ext:re-export "I18N" "EXT")

;; ----------------------------------------------------------------------------
;; Languages and Internationalization:
;; (sys::current-language)
;;   returns the current language, a symbol.
;; (deflanguage lang [parent-lang])
;;   defines a language, being a descendant of parent-lang.
;; (sys::assert-language lang)
;;   asserts that lang is a valid language.
;; (definternational symbol
;;   { (lang value-form) }*
;;   [ (t lang) | (t (var) value-form*) ]
;; )
;;   defines an internationalized object, with some predefined values in
;;   the specified languages, and a default language or default function for
;;   unspecified languages.
;; (deflocalized symbol lang value-form)
;;   enhances an internationalized object.
;; (localized symbol [lang])
;;   looks up the value of an internationalized object in lang, which defaults
;;   to the current language.
;;
;; There is an analogy between
;;         deflanguage        --   defclass
;;         definternational   --   defgeneric
;;         deflocalized       --   defmethod
;;         localized          --   funcall
;; If you need something like "definternational with arguments", then use
;; defgeneric with EQL methods for the language argument. (Well, language
;; inheritance doesn't work with EQL methods.)
;;

(defvar *all-languages* nil)
(defun assert-language (lang) ; ABI
  (let ((h (assoc lang *all-languages*)))
    (unless h
      (error-of-type 'error
        (TEXT "Language ~S is not defined")
        lang
    ) )
    (cdr h)
) )
(defun ensure-language (lang parent-lang) ; ABI
  (let ((h (assoc lang *all-languages*)))
    (if h
      (unless (eq (cdr h) parent-lang)
        (error-of-type 'error
          (TEXT "Language ~S inherits from ~S")
          lang (cdr h)
      ) )
      (progn
        (or (null parent-lang) (assert-language parent-lang))
        (setq *all-languages*
              (nconc *all-languages* (list (cons lang parent-lang)))
  ) ) ) )
  lang
)
(defmacro deflanguage (lang &optional parent-lang)
  `(SYSTEM::ENSURE-LANGUAGE ',lang ',parent-lang)
)
(deflanguage ENGLISH)
(defmacro definternational (symbol &rest options)
  `(PROGN
     ,@(mapcap #'(lambda (option)
                   (let ((lang (first option)))
                     (if (eq lang 'T)
                       `((SYS::%PUT ',symbol 'SYS::OTHER-LANGUAGE
                           ,(if (listp (second option))
                              `(FUNCTION (LAMBDA ,@(cdr option)))
                              `(SYSTEM::DEFINTERNATIONAL-DEFAULT
                                 ',symbol ',(second option)
                               )
                            )
                        ))
                       `((ASSERT-LANGUAGE ',lang)
                         (SYS::%PUT ',symbol ',lang ,(second option))
                        )
                 ) ) )
               options
       )
     ',symbol
   )
)
(defmacro deflocalized (symbol lang form)
  `(PROGN
     (ASSERT-LANGUAGE ',lang)
     (SYS::%PUT ',symbol ',lang ,form)
     ',symbol
   )
)
(defun localized (symbol &optional (language (sys::current-language)))
  (let ((notfound '#:notfound)
        (lang language))
    (loop
      (let ((h (assoc lang *all-languages*)))
        (unless h
          (error-of-type 'error
            (TEXT "~S: Language ~S is not defined")
            'localized lang
        ) )
        (let ((value (get symbol lang notfound)))
          (unless (eq value notfound) (return-from localized value))
        )
        ; not found -> search parent language
        (setq lang (cdr h))
        ; no parent language -> lookup default function
        (unless lang (return))
  ) ) )
  (let ((h (get symbol 'SYS::OTHER-LANGUAGE)))
    (if h
      (funcall h language)
      ; no more parent language and no lookup default -> return nil
      nil
) ) )
;; Default defaulter: Look up for another language.
(defvar *localized-recursion* nil)
(defun definternational-default (symbol default-language) ; ABI
  #'(lambda (language)
      (if (eq *localized-recursion* symbol) ; catch endless recursion
        (error-of-type 'error
          (TEXT "~S ~S: no value for default language ~S")
          'localized symbol language
        )
        (let ((*localized-recursion* symbol))
          (localized symbol default-language)
    ) ) )
)
