/*
 * Internationalization for CLISP
 * Bruno Haible 1990-2004
 * Sam Steingold 1998-2004
 */

#include "lispbibl.c"

#include <locale.h>

# =========== Old-style internationalization, for CLISP itself only ===========

# (SYS::CURRENT-LANGUAGE) returns the current language.
LISPFUNNR(current_language,0) {
  VALUES1(O(current_language));
}

# (SYS::SET-CURRENT-LANGUAGE LANG) ==
# (SETF (SYS::CURRENT-LANGUAGE) LANG) ==
# (SETQ *CURRENT-LANGUAGE* LANG)
# LANG is either LANGUAGE or (LANGUAGE . LOCALE-DIRECTORY)
LISPFUNN(set_current_language,1) {
 #ifndef LANGUAGE_STATIC
  if (consp(STACK_0)) {
    pushSTACK(check_symbol(Car(STACK_0)));
    pushSTACK(check_string(Cdr(STACK_(0+1))));
    with_sstring_0(Symbol_name(STACK_1),O(misc_encoding),lang,{
      with_string_0(STACK_0,O(misc_encoding),localedir,
                    { init_language(lang,localedir); });
    });
    skipSTACK(2);
  } else {
    STACK_0 = check_symbol(STACK_0);
    with_sstring_0(Symbol_name(STACK_0),O(misc_encoding),lang,
                   { init_language(lang,NULL); });
  }
 #else
  if (!eq(STACK_0,O(current_language))) {
    pushSTACK(STACK_0);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: cannot set language to ~S"));
  }
 #endif
  VALUES1(O(current_language)); skipSTACK(1);
}

LISPFUNNR(text,1)
{ /* (SYS::TEXT english) returns the message in the current language */
 #ifndef GNU_GETTEXT
  VALUES1(ENGLISH ? (object)STACK_0 : NIL);
 #else
  STACK_0 = check_string(STACK_0);
  with_string_0(STACK_0,Symbol_value(S(ascii)),asciz, {
    VALUES1(CLSTEXT(asciz));
  });
 #endif
  skipSTACK(1);
}
