/*
 * ======= General internationalization, for Lisp programs too =======
 * Copyright (C) 1990-2005 Bruno Haible
 * Copyright (C) 1998-2006 Sam Steingold
 * GPL2
 */

#if defined(_WIN32)
/* get ASCII functions */
# undef UNICODE
#endif

#include "clisp.h"
#include "config.h"

#include <string.h>             /* strncpy() */
#include <locale.h>
#if defined(HAVE_LANGINFO_H)
# include <langinfo.h>
#endif
#include <limits.h>            /* for CHAR_MAX */

#ifdef CLISP_UNICODE
# define if_UNICODE(statement)  statement
#else
# define if_UNICODE(statement)  /*nothing*/
#endif

DEFMODULE(i18n,"I18N")

/* Returns the <locale.h> value corresponding to a LC_... constant. */
DEFCHECKER(check_locale_category,prefix=LC,default=LC_MESSAGES, \
           ALL COLLATE CTYPE MESSAGES MONETARY NUMERIC TIME \
           PAPER NAME ADDRESS TELEPHONE MEASUREMENT IDENTIFICATION)

#ifdef GNU_GETTEXT

static inline object do_gettext (const char* msgid,
                                 const char* domain, int category)
{
  const char* translated_msg;
  if (msgid[0] == '\0') {
    translated_msg = "";  /* Don't return the catalog's header entry. */
  } else {
    begin_system_call();
#  ifdef CLISP_UNICODE
    if (domain != NULL)
      bind_textdomain_codeset(domain,"UTF-8");
#  endif
    translated_msg = dcgettext(domain,msgid,category);
    end_system_call();
  }
  return asciz_to_string(translated_msg,Symbol_value(S(utf_8)));
}

static inline object do_ngettext (const char* msgid, const char* msgid_plural,
                                  const char* domain, uint32 n, int category)
{
  const char* translated_msg;
  begin_system_call();
# ifdef CLISP_UNICODE
  if (domain != NULL)
    bind_textdomain_codeset(domain,"UTF-8");
# endif
  translated_msg = dcngettext(domain,msgid,msgid_plural,n,category);
  end_system_call();
  return asciz_to_string(translated_msg,Symbol_value(S(utf_8)));
}

#endif

DEFUNR(I18N:GETTEXT, msgid &optional domain category)
{ /* returns the translation of msgid in the given domain,
     depending on the given category. */
  object msgid = check_string(STACK_2);
 #ifdef GNU_GETTEXT
  with_string_0(msgid,Symbol_value(S(ascii)),msgid_asciz, {
    object domain = STACK_1;
    if (missingp(domain)) {
      int category = check_locale_category(STACK_0);
      VALUES1(do_gettext(msgid_asciz,NULL,category));
    } else {
      domain = check_string(domain);
      with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
        int category = check_locale_category(STACK_0);
        VALUES1(do_gettext(msgid_asciz,domain_asciz,category));
      });
    }
  });
 #else
  VALUES1(msgid);
 #endif
  skipSTACK(3);
}

DEFUNR(I18N:NGETTEXT,msgid msgid_plural n &optional domain category)
{ /* returns the plural form of the translation for of msgid and n in
     the given domain, depending on the given category. */
  STACK_4 = check_string(STACK_4); /* msgid */
  STACK_3 = check_string(STACK_3); /* msgid_plural */
  {
    object arg = (STACK_2 = check_pos_integer(STACK_2));
    uint32 n;
    if (uint32_p(arg))
      n = I_to_uint32(arg);
    else {
      /* arg is a Bignum. Plural form depends only on (mod arg 1000000). */
      pushSTACK(arg); pushSTACK(fixnum(1000000)); funcall(L(mod),2);
      n = 1000000 + (uint32)posfixnum_to_V(value1);
    }
    {
      object msgid = STACK_4;
      object msgid_plural = STACK_3;
     #ifdef GNU_GETTEXT
      with_string_0(msgid,Symbol_value(S(ascii)),msgid_asciz, {
        with_string_0(msgid_plural,Symbol_value(S(ascii)),msgid_plural_asciz, {
          object domain = STACK_1;
          if (missingp(domain)) {
            int category = check_locale_category(STACK_0);
            VALUES1(do_ngettext(msgid_asciz,msgid_plural_asciz,NULL,
                                n,category));
          } else {
            domain = check_string(domain);
            with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
              int category = check_locale_category(STACK_0);
              VALUES1(do_ngettext(msgid_asciz,msgid_plural_asciz,domain_asciz,
                                  n,category));
              });
          }
          });
        });
     #else
      VALUES1(n == 1 ? msgid : msgid_plural);
     #endif
    }
  }
  skipSTACK(5);
}

DEFUNR(I18N:TEXTDOMAIN,)
{ /* returns the current default domain. */
 #ifdef GNU_GETTEXT
  const char* domain;
  begin_system_call();
  domain = textdomain(NULL);
  end_system_call();
  VALUES1(asciz_to_string(domain,Symbol_value(S(ascii))));
 #else
  VALUES1(NIL);
 #endif
}

DEFUN(I18N:SET-TEXTDOMAIN, domain)
{ /* sets the default domain. */
  object domain = check_string(popSTACK());
 #ifdef GNU_GETTEXT
  with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
    begin_system_call();
    textdomain(domain_asciz);
    if_UNICODE(bind_textdomain_codeset(domain_asciz,"UTF-8"));
    end_system_call();
  });
 #endif
  VALUES1(domain);
}

DEFUN(I18N:TEXTDOMAINDIR, domain)
{ /* returns the message catalog directory for the given domain. */
  object domain = check_string(popSTACK());
 #ifdef GNU_GETTEXT
  const char* dir;
  with_string_0(domain,Symbol_value(S(ascii)),domain_asciz, {
    begin_system_call();
    dir = bindtextdomain(domain_asciz,NULL);
    end_system_call();
  });
  VALUES1(dir != NULL ? OSdir_to_pathname(dir) : (object)NIL);
 #else
  VALUES1(NIL);
 #endif
}

DEFUN(I18N:SET-TEXTDOMAINDIR, domain directory)
{ /* sets the message catalog directory for the given domain. */
  object domain = (STACK_1=check_string(STACK_1));
 #ifdef GNU_GETTEXT
  /* Check and use default directory, because the bindtextdomain()
     documentation recommends that the argument be an absolute pathname,
     to protect against later chdir() calls. */
  object directory = pathname_to_OSdir(STACK_0,true);
  with_string_0(STACK_1/*domain*/,Symbol_value(S(ascii)),domain_asciz, {
    begin_system_call();
    bindtextdomain(domain_asciz,TheAsciz(directory));
    end_system_call();
  });
 #endif
  VALUES1(STACK_0);
  skipSTACK(2);
}


/* ======================== locale ======================== */

DEFUN(I18N:SET-LOCALE, &optional category locale)
{ /* call setlocale(3) */
  gcv_object_t *category = &STACK_1;
  gcv_object_t *locale = &STACK_0;
  char* res;
  if (missingp(*category)) {
    int pos = 0;
    if (missingp(*locale)) {
      for (; pos < check_locale_category_map.size; pos++) {
        begin_system_call();
        res = setlocale(check_locale_category_map.table[pos].c_const,NULL);
        end_system_call();
        pushSTACK(*check_locale_category_map.table[pos].l_const);
        pushSTACK(res ? asciz_to_string(res,GLO(misc_encoding)) : NIL);
      }
    } else {
      *locale = check_string(*locale);
      with_string_0(*locale,GLO(misc_encoding),loc_z,{
          for (; pos < check_locale_category_map.size; pos++) {
            begin_system_call();
            res = setlocale(check_locale_category_map.table[pos].c_const,loc_z);
            end_system_call();
            pushSTACK(*check_locale_category_map.table[pos].l_const);
            pushSTACK(res ? asciz_to_string(res,GLO(misc_encoding)) : NIL);
          }
        });
    }
    VALUES1(listof(2*check_locale_category_map.size));
  } else {
    int cat_value = check_locale_category(*category);
    if (missingp(*locale)) {
      begin_system_call();
      res = setlocale(cat_value,NULL);
      end_system_call();
    } else {
      *locale = check_string(*locale);
      with_string_0(*locale,GLO(misc_encoding),loc_z,{
          begin_system_call();
          res = setlocale(cat_value,loc_z);
          end_system_call();
        });
    }
    VALUES1(res ? asciz_to_string(res,GLO(misc_encoding)) : NIL);
  }
  skipSTACK(2);
}

#if defined(WIN32_NATIVE)
/* call GetLocaleInfo.
 res is a malloc'ed area of size res_size
 it may be increased as a result of calling this function */
# define GET_LOCALE_INFO_BUF_SIZE 256
static void get_locale_info (int what, char**res, int *res_size) {
  int val;
  begin_system_call();
  val = GetLocaleInfo(LOCALE_SYSTEM_DEFAULT,what,*res,*res_size);
  end_system_call();
  if (val == 0) OS_error();
  if (val > *res_size) {
    *res = (char*)my_realloc(*res,val);
    if (*res == NULL) OS_error();
    *res_size = val;
    begin_system_call();
    GetLocaleInfo(LOCALE_SYSTEM_DEFAULT,what,*res,*res_size);
    end_system_call();
  }
}
#endif

#if defined(HAVE_LOCALECONV)
static void thousands_sep_to_STACK (char* sep1000) {
  int ii;
  for (ii=0; sep1000[ii]; ii++) pushSTACK(fixnum(sep1000[ii]));
  pushSTACK(Fixnum_0); value1 = vectorof(ii+1); pushSTACK(value1);
}
static /*maygc*/ object bool_char_lconv(char val) {
  switch (val) {
    case 0: return NIL;
    case 1: return T;
    case CHAR_MAX: return S(Kunspecific);
    default:
      pushSTACK(CLSTEXT("~S: localeconv() returned an invalid value ~S (should be one of ~S, ~S, CHAR_MAX=~S)"));
      pushSTACK(TheSubr(subr_self)->name);
      pushSTACK(fixnum(val));
      pushSTACK(Fixnum_0); pushSTACK(Fixnum_1); pushSTACK(fixnum(CHAR_MAX));
      funcall(S(warn),6);
      return fixnum(val);
  }
}
static object int_char_lconv(char val) {
  switch (val) {
    case CHAR_MAX: return S(Kunspecific);
    default: return fixnum(val);
  }
}
DEFUN(I18N:LOCALE-CONV,)
{ /* call localeconv(3) */
  struct lconv *lc;
  begin_system_call(); lc = localeconv(); end_system_call();
  pushSTACK(asciz_to_string(lc->decimal_point,GLO(misc_encoding)));
  pushSTACK(asciz_to_string(lc->thousands_sep,GLO(misc_encoding)));
  thousands_sep_to_STACK(lc->grouping);
  pushSTACK(asciz_to_string(lc->int_curr_symbol,GLO(misc_encoding)));
  pushSTACK(asciz_to_string(lc->currency_symbol,GLO(misc_encoding)));
  pushSTACK(asciz_to_string(lc->mon_decimal_point,GLO(misc_encoding)));
  pushSTACK(asciz_to_string(lc->mon_thousands_sep,GLO(misc_encoding)));
  thousands_sep_to_STACK(lc->mon_grouping);
  pushSTACK(asciz_to_string(lc->positive_sign,GLO(misc_encoding)));
  pushSTACK(asciz_to_string(lc->negative_sign,GLO(misc_encoding)));
  pushSTACK(int_char_lconv(lc->int_frac_digits));
  pushSTACK(int_char_lconv(lc->frac_digits));
  pushSTACK(bool_char_lconv(lc->p_cs_precedes));
  pushSTACK(int_char_lconv(lc->p_sep_by_space));
  pushSTACK(bool_char_lconv(lc->n_cs_precedes));
  pushSTACK(int_char_lconv(lc->n_sep_by_space));
  pushSTACK(int_char_lconv(lc->p_sign_posn));
  pushSTACK(int_char_lconv(lc->n_sign_posn));
#if HAVE_STRUCT_LCONV_INT_P_CS_PRECEDES
  pushSTACK(bool_char_lconv(lc->int_p_cs_precedes));
#else
  pushSTACK(S(Kunspecific));
#endif
#if HAVE_STRUCT_LCONV_INT_N_CS_PRECEDES
  pushSTACK(bool_char_lconv(lc->int_n_cs_precedes));
#else
  pushSTACK(S(Kunspecific));
#endif
#if HAVE_STRUCT_LCONV_INT_P_SEP_BY_SPACE
  pushSTACK(int_char_lconv(lc->int_p_sep_by_space));
#else
  pushSTACK(S(Kunspecific));
#endif
#if HAVE_STRUCT_LCONV_INT_N_SEP_BY_SPACE
  pushSTACK(int_char_lconv(lc->int_n_sep_by_space));
#else
  pushSTACK(S(Kunspecific));
#endif
#if HAVE_STRUCT_LCONV_INT_P_SIGN_POSN
  pushSTACK(int_char_lconv(lc->int_p_sign_posn));
#else
  pushSTACK(S(Kunspecific));
#endif
#if HAVE_STRUCT_LCONV_INT_N_SIGN_POSN
  pushSTACK(int_char_lconv(lc->int_n_sign_posn));
#else
  pushSTACK(S(Kunspecific));
#endif
  funcall(`I18N::MK-LOCALE-CONV`,24);
}
#elif defined(WIN32_NATIVE)
static int my_atoi (char *res) {
  int val;
  begin_system_call(); val = atoi(res); end_system_call();
  return val;
}
static void thousands_sep_to_STACK (int what, char** gres, int* res_size) {
  /* "1;2;3" ==> #(1 2 3) */
  int start = 0, end = 0, count = 0, limit;
  char *res;
  get_locale_info(what,gres,res_size);
  res = *gres; limit = *res_size;
  while (res[end] && (end < limit)) {
    while (res[end] && (res[end] != ';') && (end < limit)) end++;
    pushSTACK(fixnum(my_atoi(res+start))); count++;
    if (!res[end]) break;
    start = ++end;
  }
  value1 = vectorof(count); pushSTACK(value1);
}
static void locale_string_to_STACK (int what, char**res, int* res_size) {
  get_locale_info(what,res,res_size);
  pushSTACK(asciz_to_string(*res,GLO(misc_encoding)));
}
static void locale_int_to_STACK (int what, char**res, int* res_size) {
  get_locale_info(what,res,res_size);
  pushSTACK(fixnum(my_atoi(*res)));
}
static void locale_bool_to_STACK (int what, char**res, int* res_size) {
  get_locale_info(what,res,res_size);
  pushSTACK(my_atoi(*res) ? T : NIL);
}
DEFUN(I18N:LOCALE-CONV,)
{ /* call GetLocaleInfo(3) */
  int res_size = GET_LOCALE_INFO_BUF_SIZE;
  char *res = my_malloc(res_size);
  locale_string_to_STACK(LOCALE_SDECIMAL,&res,&res_size);
  locale_string_to_STACK(LOCALE_STHOUSAND,&res,&res_size);
  thousands_sep_to_STACK(LOCALE_SGROUPING,&res,&res_size);
  locale_string_to_STACK(LOCALE_SINTLSYMBOL,&res,&res_size);
  locale_string_to_STACK(LOCALE_SCURRENCY,&res,&res_size);
  locale_string_to_STACK(LOCALE_SMONDECIMALSEP,&res,&res_size);
  locale_string_to_STACK(LOCALE_SMONTHOUSANDSEP,&res,&res_size);
  thousands_sep_to_STACK(LOCALE_SMONGROUPING,&res,&res_size);
  locale_string_to_STACK(LOCALE_SPOSITIVESIGN,&res,&res_size);
  locale_string_to_STACK(LOCALE_SNEGATIVESIGN,&res,&res_size);
  locale_int_to_STACK(LOCALE_IINTLCURRDIGITS,&res,&res_size);
  locale_int_to_STACK(LOCALE_ICURRDIGITS,&res,&res_size);
  locale_bool_to_STACK(LOCALE_IPOSSYMPRECEDES,&res,&res_size);
  locale_bool_to_STACK(LOCALE_IPOSSEPBYSPACE,&res,&res_size);
  locale_bool_to_STACK(LOCALE_INEGSYMPRECEDES,&res,&res_size);
  locale_bool_to_STACK(LOCALE_INEGSEPBYSPACE,&res,&res_size);
  locale_bool_to_STACK(LOCALE_IPOSSIGNPOSN,&res,&res_size);
  locale_bool_to_STACK(LOCALE_INEGSIGNPOSN,&res,&res_size);
  pushSTACK(S(Kunspecific));
  pushSTACK(S(Kunspecific));
  pushSTACK(S(Kunspecific));
  pushSTACK(S(Kunspecific));
  pushSTACK(S(Kunspecific));
  pushSTACK(S(Kunspecific));
  funcall(`I18N::MK-LOCALE-CONV`,24);
  begin_system_call(); free(res); end_system_call();
}
#endif

#if defined(HAVE_NL_LANGINFO) || defined(WIN32_NATIVE)
DEFCHECKER(check_nl_item,CODESET                                        \
           D_T_FMT D_FMT T_FMT T_FMT_AMPM AM_STR PM_STR                 \
           DAY_1 DAY_2 DAY_3 DAY_4 DAY_5 DAY_6 DAY_7                    \
           ABDAY_1 ABDAY_2 ABDAY_3 ABDAY_4 ABDAY_5 ABDAY_6 ABDAY_7      \
           MON_1 MON_2 MON_3 MON_4 MON_5 MON_6                          \
           MON_7 MON_8 MON_9 MON_10 MON_11 MON_12                       \
           ABMON_1 ABMON_2 ABMON_3 ABMON_4 ABMON_5 ABMON_6              \
           ABMON_7 ABMON_8 ABMON_9 ABMON_10 ABMON_11 ABMON_12           \
           ERA ERA_D_FMT ERA_D_T_FMT ERA_T_FMT                          \
           ALT_DIGITS RADIXCHAR THOUSEP                                 \
           YESEXPR NOEXPR YESSTR NOSTR CRNCYSTR                         \
           D_MD_ORDER                                                   \
           LOCALE_FONTSIGNATURE LOCALE_ICALENDARTYPE LOCALE_ICENTURY    \
           LOCALE_ICOUNTRY LOCALE_ICURRDIGITS LOCALE_ICURRENCY          \
           LOCALE_IDATE LOCALE_IDAYLZERO                                \
           LOCALE_IDEFAULTANSICODEPAGE LOCALE_IDEFAULTCODEPAGE          \
           LOCALE_IDEFAULTCOUNTRY LOCALE_IDEFAULTEBCDICCODEPAGE         \
           LOCALE_IDEFAULTLANGUAGE LOCALE_IDEFAULTMACCODEPAGE           \
           LOCALE_IDIGITS LOCALE_IDIGITSUBSTITUTION                     \
           LOCALE_IFIRSTDAYOFWEEK LOCALE_IFIRSTWEEKOFYEAR               \
           LOCALE_IINTLCURRDIGITS LOCALE_ILANGUAGE                      \
           LOCALE_ILDATE LOCALE_ILZERO LOCALE_IMEASURE LOCALE_IMONLZERO \
           LOCALE_INEGCURR LOCALE_INEGNUMBER LOCALE_INEGSEPBYSPACE      \
           LOCALE_INEGSIGNPOSN LOCALE_INEGSYMPRECEDES LOCALE_IOPTIONALCALENDAR \
           LOCALE_IPAPERSIZE LOCALE_IPOSSEPBYSPACE LOCALE_IPOSSIGNPOSN  \
           LOCALE_IPOSSYMPRECEDES LOCALE_ITIMEMARKPOSN LOCALE_ITLZERO   \
           LOCALE_S1159 LOCALE_S2359                                    \
           LOCALE_SABBREVCTRYNAME LOCALE_SABBREVDAYNAME1                \
           LOCALE_SABBREVDAYNAME2 LOCALE_SABBREVDAYNAME3                \
           LOCALE_SABBREVDAYNAME4 LOCALE_SABBREVDAYNAME5                \
           LOCALE_SABBREVDAYNAME6 LOCALE_SABBREVDAYNAME7                \
           LOCALE_SABBREVLANGNAME LOCALE_SABBREVMONTHNAME1              \
           LOCALE_SABBREVMONTHNAME2 LOCALE_SABBREVMONTHNAME3            \
           LOCALE_SABBREVMONTHNAME4 LOCALE_SABBREVMONTHNAME5            \
           LOCALE_SABBREVMONTHNAME6 LOCALE_SABBREVMONTHNAME7            \
           LOCALE_SABBREVMONTHNAME8 LOCALE_SABBREVMONTHNAME9            \
           LOCALE_SABBREVMONTHNAME10 LOCALE_SABBREVMONTHNAME11          \
           LOCALE_SABBREVMONTHNAME12 LOCALE_SABBREVMONTHNAME13          \
           LOCALE_SCOUNTRY LOCALE_SCURRENCY                             \
           LOCALE_SDATE LOCALE_SDAYNAME1 LOCALE_SDAYNAME2 LOCALE_SDAYNAME3 \
           LOCALE_SDAYNAME4 LOCALE_SDAYNAME5 LOCALE_SDAYNAME6 LOCALE_SDAYNAME7 \
           LOCALE_SDECIMAL LOCALE_SENGCOUNTRY LOCALE_SENGCURRNAME       \
           LOCALE_SENGLANGUAGE LOCALE_SGROUPING LOCALE_SINTLSYMBOL      \
           LOCALE_SISO3166CTRYNAME LOCALE_SISO639LANGNAME               \
           LOCALE_SLANGUAGE LOCALE_SLIST LOCALE_SLONGDATE               \
           LOCALE_SMONDECIMALSEP LOCALE_SMONGROUPING LOCALE_SMONTHNAME1 \
           LOCALE_SMONTHNAME2 LOCALE_SMONTHNAME3 LOCALE_SMONTHNAME4     \
           LOCALE_SMONTHNAME5 LOCALE_SMONTHNAME6 LOCALE_SMONTHNAME7     \
           LOCALE_SMONTHNAME8 LOCALE_SMONTHNAME9 LOCALE_SMONTHNAME10    \
           LOCALE_SMONTHNAME11 LOCALE_SMONTHNAME12 LOCALE_SMONTHNAME13  \
           LOCALE_SMONTHOUSANDSEP LOCALE_SNATIVECTRYNAME                \
           LOCALE_SNATIVECURRNAME LOCALE_SNATIVEDIGITS LOCALE_SNATIVELANGNAME \
           LOCALE_SNEGATIVESIGN LOCALE_SPOSITIVESIGN                    \
           LOCALE_SSHORTDATE LOCALE_SSORTNAME LOCALE_STHOUSAND          \
           LOCALE_STIME LOCALE_STIMEFORMAT LOCALE_SYEARMONTH)
#if defined(HAVE_NL_LANGINFO)
# define get_lang_info(what)                                            \
  begin_system_call(); res = nl_langinfo(what); end_system_call()
# define res_to_obj() (res ? asciz_to_string(res,GLO(misc_encoding)) : NIL)
# define DECLARE_RES  char* res
# define FINISH_RES
#elif defined(WIN32_NATIVE)
# define get_lang_info(what)  get_locale_info(what,&res,&res_size)
# define res_to_obj() (asciz_to_string(res,GLO(misc_encoding)))
# define DECLARE_RES  int res_size=GET_LOCALE_INFO_BUF_SIZE; char *res=(char*)my_malloc(res_size)
# define FINISH_RES   begin_system_call(); free(res); end_system_call()
#endif
DEFUNR(I18N:LANGUAGE-INFORMATION,&optional item)
{ /* call nl_langinfo(3) or GetLocaleInfo() */
  object what = popSTACK();
  if (missingp(what)) {         /* everything */
    int pos = 0;
    DECLARE_RES;
    for (; pos < check_nl_item_map.size; pos++) {
      get_lang_info(check_nl_item_map.table[pos].c_const);
      pushSTACK(*check_nl_item_map.table[pos].l_const);
      pushSTACK(res_to_obj());
    }
    FINISH_RES;
    VALUES1(listof(2*check_nl_item_map.size));
  } else {
    int item = check_nl_item(what);
    DECLARE_RES;
    get_lang_info(item);
    VALUES1(res_to_obj());
    FINISH_RES;
  }
}
#endif
