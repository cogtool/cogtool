;;; CLISP i18n module
;;; Copyright (C) 1990-2004 Bruno Haible
;;; Copyright (C) 1998-2005 Sam Steingold
;;
;; Interface to GNU gettext
;;
;; (gettext msgid [domain [category]]) returns the translation of
;; msgid in the given domain, depending on the given category.
;;
;; (ngettext msgid msgid_plural n [domain [category]]) returns the plural
;; form of the translation for of msgid and n in the given domain, depending
;; on the given category.
;;
;; (textdomain) returns the current default domain.
;;
;; (textdomaindir domain) returns the message catalog directory
;; for the given domain.

(eval-when (compile load eval)
  (setf (package-lock custom:*system-package-list*) nil))

(export
 '(i18n::gettext i18n::ngettext i18n::textdomain i18n::textdomaindir
   i18n::set-locale i18n::language-information
   i18n::locale-conv i18n::locale-conv-p
   i18n::locale-conv-decimal_point i18n::locale-conv-thousands_sep
   i18n::locale-conv-grouping  i18n::locale-conv-int_curr_symbol
   i18n::locale-conv-currency_symbol i18n::locale-conv-mon_decimal_point
   i18n::locale-conv-mon_thousands_sep i18n::locale-conv-mon_grouping
   i18n::locale-conv-positive_sign i18n::locale-conv-negative_sign
   i18n::locale-conv-int_frac_digits i18n::locale-conv-frac_digits
   i18n::locale-conv-p_cs_precedes i18n::locale-conv-p_sep_by_space
   i18n::locale-conv-n_cs_precedes i18n::locale-conv-n_sep_by_space
   i18n::locale-conv-p_sign_posn i18n::locale-conv-n_sign_posn
   i18n::locale-conv-int_p_cs_precedes i18n::locale-conv-int_n_cs_precedes
   i18n::locale-conv-int_p_sep_by_space i18n::locale-conv-int_n_sep_by_space
   i18n::locale-conv-int_p_sign_posn i18n::locale-conv-int_n_sign_posn)
 "I18N")
(ext:re-export "I18N" "EXT")

(defsetf i18n::textdomain i18n::set-textdomain)
(defsetf i18n::textdomaindir i18n::set-textdomaindir)

(defstruct (i18n::locale-conv
             (:constructor i18n::mk-locale-conv
                           (i18n::decimal_point      i18n::thousands_sep
                            i18n::grouping           i18n::int_curr_symbol
                            i18n::currency_symbol    i18n::mon_decimal_point
                            i18n::mon_thousands_sep  i18n::mon_grouping
                            i18n::positive_sign      i18n::negative_sign
                            i18n::int_frac_digits    i18n::frac_digits
                            i18n::p_cs_precedes      i18n::p_sep_by_space
                            i18n::n_cs_precedes      i18n::n_sep_by_space
                            i18n::p_sign_posn        i18n::n_sign_posn
                            i18n::int_p_cs_precedes  i18n::int_n_cs_precedes
                            i18n::int_p_sep_by_space i18n::int_n_sep_by_space
                            i18n::int_p_sign_posn    i18n::int_n_sign_posn)))
  ;; <http://www.opengroup.org/onlinepubs/009695399/functions/localeconv.html>
  i18n::decimal_point i18n::thousands_sep i18n::grouping i18n::int_curr_symbol
  i18n::currency_symbol i18n::mon_decimal_point i18n::mon_thousands_sep
  i18n::mon_grouping i18n::positive_sign i18n::negative_sign
  i18n::int_frac_digits i18n::frac_digits i18n::p_cs_precedes
  i18n::p_sep_by_space i18n::n_cs_precedes i18n::n_sep_by_space
  i18n::p_sign_posn i18n::n_sign_posn i18n::int_p_cs_precedes
  i18n::int_n_cs_precedes i18n::int_p_sep_by_space i18n::int_n_sep_by_space
  i18n::int_p_sign_posn i18n::int_n_sign_posn)

(pushnew "I18N" custom:*system-package-list* :test #'string=)
(setf (package-lock custom:*system-package-list*) t)
(pushnew :i18n *features*)
