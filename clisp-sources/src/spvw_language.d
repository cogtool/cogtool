# Choice of user interface language, and internationalization.

# ------------------------------ Specification --------------------------------

#ifndef LANGUAGE_STATIC

  # The current user interface language.
    extern uintL language;

  # Initializes the current interface language, according to the given
  # arguments, getting the defaults from environment variables.
    global void init_language (const char* argv_language, const char* argv_localedir);

  # Returns the translation of msgid according to the current interface
  # language.
    global const char * clgettext (const char * msgid);

#endif

# Returns the translation of string according to the current interface
# language. A string is returned.
# can trigger GC
global object CLSTEXT (const char*);

# Returns the translated value of obj. obj is translated, then
# READ-FROM-STRING is applied to the result.
# can trigger GC
global object CLOTEXT (const char*);


# ------------------------------ Implementation -------------------------------

#ifndef LANGUAGE_STATIC

# language, that is used for communication with the user:
global uintL language;

# Initializes the language, given the language name.
local bool init_language_from (const char* langname);
#ifdef GNU_GETTEXT
  #define language_deutsch   1
  #define language_francais  2
  #define language_spanish   3
  #define language_dutch     4
  #define language_russian   5
  #define language_danish    6
#endif

local object current_language_o (uintL lang) {
  switch (lang) {
    case language_english: return S(english);
   #ifdef GNU_GETTEXT
    case language_deutsch:  return S(german);
    case language_francais: return S(french);
    case language_spanish:  return S(spanish);
    case language_dutch:    return S(dutch);
    case language_russian:  return S(russian);
    case language_danish:   return S(danish);
   #endif
    default: NOTREACHED;
  }
}

local bool init_language_from (const char* langname) {
  if (asciz_equal(langname,"ENGLISH") || asciz_equal(langname,"english")) {
    language = language_english; return true;
  }
 #ifdef GNU_GETTEXT
  if (asciz_equal(langname,"DEUTSCH") || asciz_equal(langname,"deutsch")
      || asciz_equal(langname,"GERMAN") || asciz_equal(langname,"german")) {
    language = language_deutsch; return true;
  }
  if (asciz_equal(langname,"FRANCAIS") || asciz_equal(langname,"francais")
     #ifndef ASCII_CHS
      || asciz_equal(langname,"FRAN\307AIS") || asciz_equal(langname,"FRAN\303\207AIS") # FRENCH
      || asciz_equal(langname,"fran\347ais") || asciz_equal(langname,"fran\303\247ais") # french
     #endif
      || asciz_equal(langname,"FRENCH") || asciz_equal(langname,"french")) {
    language = language_francais; return true;
  }
  if (asciz_equal(langname,"ESPANOL") || asciz_equal(langname,"espanol")
     #ifndef ASCII_CHS
      || asciz_equal(langname,"ESPA\321OL") || asciz_equal(langname,"ESPA\303\221OL") # SPANISH
      || asciz_equal(langname,"espa\361ol") || asciz_equal(langname,"espa\303\261ol") # spanish
     #endif
      || asciz_equal(langname,"SPANISH") || asciz_equal(langname,"spanish")) {
    language = language_spanish; return true;
  }
  if (asciz_equal(langname,"russian") || asciz_equal(langname,"RUSSIAN")
     #ifndef ASCII_CHS
      || asciz_equal(langname,"\320\240\320\243\320\241\320\241\320\232\320\230\320\231")
      || asciz_equal(langname,"\321\200\321\203\321\201\321\201\320\272\320\270\320\271")
      || asciz_equal(langname,"\240\243\241\241\232\230\231")
      || asciz_equal(langname,"\200\203\201\201\272\270\271")
     #endif
      ) {
    language = language_russian; return true;
  }
  if (asciz_equal(langname,"NEDERLANDS") || asciz_equal(langname,"nederlands")
      || asciz_equal(langname,"DUTCH") || asciz_equal(langname,"dutch")) {
    language = language_dutch; return true;
  }
  if (asciz_equal(langname,"DANSK") || asciz_equal(langname,"dansk")
      || asciz_equal(langname,"DANISH") || asciz_equal(langname,"danish")) {
    language = language_danish; return true;
  }
 #endif
  return false;
}

# Initializes the language.
global void init_language (const char* argv_language,
                           const char* argv_localedir) {
  # language is set with priorities in this order:
  #   1. built in fix, LANGUAGE_STATIC
  #   2. -L command line argument
  #   3. environment-variable CLISP_LANGUAGE
  #   4. environment-variable LANG
  #   5. default: English
  if (argv_language) {
    if (init_language_from(argv_language))
      goto chosen1;
  }
 #ifdef HAVE_ENVIRONMENT
  {
    var const char* langname = getenv("CLISP_LANGUAGE");
    if (langname) {
      if (init_language_from(langname))
        goto chosen1;
    }
  }
 #endif
 #ifdef GNU_GETTEXT
  # The analysis of getenv("LANG") below will be done - in more detail -
  # by bindtextdomain() and textdomain(). No need to do it ourselves.
  # Do we need to call setlocale(LC_MESSAGES,"") or not??
  goto chosen2;
 #else
  #ifdef HAVE_ENVIRONMENT
   #define ascii_alphanumericp(c)  \
     ((c>='A' && c<='Z') || (c>='a' && c<='z') || (c>='0' && c<='9'))
  {
    var const char* lang = getenv("LANG");
    if (lang) {
      # LANG has in general the syntax language[_country][.charset]
      if (lang[0]=='e' && lang[1]=='n'
          && !ascii_alphanumericp(lang[2])) { # "en"
        language = language_english; goto chosen2;
      }
    }
  }
  #endif
  # Default: english
  language = language_english; goto chosen2;
 #endif
 chosen1:
  # At this point we have chosen the language based upon the
  # command-line option or the clisp-specific environment variables.
 #ifdef GNU_GETTEXT
  # GNU gettext chooses the message catalog based upon:
  # 1. environment variable LANGUAGE [only if dcgettext.c, not with
  #    cat-compat.c],
  # 2. environment variable LC_ALL,
  # 3. environment variable LC_MESSAGES,
  # 4. environment variable LANG.
  # We clobber LC_MESSAGES and unset the earlier two variables.
  {
    var const char * locale;
    switch (language) {
      case language_english:  locale = "en_US"; break;
      case language_deutsch:  locale = "de_DE"; break;
      case language_francais: locale = "fr_FR"; break;
      case language_spanish:  locale = "es_ES"; break;
      case language_dutch:    locale = "nl_NL"; break;
      case language_russian:  locale = "ru_RU"; break;
      case language_danish:   locale = "da_DA"; break;
      default:                locale = "";
    }
    if (getenv("LANGUAGE")) clisp_setenv("LANGUAGE",NULL);
    if (getenv("LC_ALL")) clisp_setenv("LC_ALL",NULL);
    clisp_setenv("LC_MESSAGES",locale);
    # Given the above, the following line is only needed for those
    # platforms for which gettext is compiled with HAVE_LOCALE_NULL
    # defined.
    setlocale(LC_MESSAGES,locale);
    # Invalidate the gettext internal caches.
    textdomain(textdomain(NULL));
  }
 #endif
 chosen2: ;
  # At this point we have chosen the language based upon an
  # environment variable GNU gettext knows about.
 #ifdef GNU_GETTEXT
  {
    # We apparently don't need to check whether argv_localedir is
    # not NULL and a valid directory. But since we may call chdir()
    # before the gettext library opens the catalog file, we have to
    # convert argv_localedir to be an absolute pathname, if possible.
    var bool must_free_argv_localedir = false;
    #ifdef UNIX
    if (argv_localedir != NULL)
      if (argv_localedir[0] != '\0' && argv_localedir[0] != '/') {
        var char currdir[MAXPATHLEN];
        if (!(getwd(currdir) == NULL)) {
          var uintL currdirlen = asciz_length(currdir);
          if (currdirlen > 0 && currdir[0] == '/') {
            var uintL len = currdirlen + 1 + asciz_length(argv_localedir) + 1;
            var char* abs_localedir = (char*)malloc(len*sizeof(char));
            if (!(abs_localedir == NULL)) {
              must_free_argv_localedir = true;
              # Append currdir, maybe '/', and argv_localedir into abs_localedir:
              var char* ptr = abs_localedir;
              {
                var const char * srcptr = currdir;
                var uintL count;
                dotimespL(count,currdirlen, { *ptr++ = *srcptr++; });
              }
              if (ptr[-1] != '/')
                *ptr++ = '/';
              {
                var const char * srcptr = argv_localedir;
                while ((*ptr++ = *srcptr++) != '\0') continue;
              }
              argv_localedir = abs_localedir;
            }
          }
        }
      }
    #endif
    # If argv_localedir is NULL, the user is not interested in localized
    # messages. Therefore use a directory that doesn't contain message
    # catalogs.
    if (argv_localedir == NULL)
      argv_localedir = "/nonexistent";
    bindtextdomain("clisp",argv_localedir);
    bindtextdomain("clisplow",argv_localedir);
    if (must_free_argv_localedir) free((void*)argv_localedir);
   #ifdef UNICODE
    bind_textdomain_codeset("clisp","UTF-8");
   #endif
  }
 #endif
}

 #ifdef GNU_GETTEXT

  global const char * clgettext (const char * msgid) {
    var const char * translated_msg;
    if (msgid[0] == '\0') {
      # If you ask gettext to translate the empty string, it returns
      # the catalog's header (containing meta information)!
      translated_msg = msgid;
    } else {
      begin_system_call();
      translated_msg = dgettext("clisp",msgid);
      end_system_call();
    }
    return translated_msg;
  }

  # Low-level messages, which are output through fprintf(3), are
  # stored in a separate catalog and returned in locale encoding.
  global const char * clgettextl (const char * msgid) {
    var const char * translated_msg;
    if (msgid[0] == '\0') {
      # If you ask gettext to translate the empty string, it returns
      # the catalog's header (containing meta information)!
      translated_msg = msgid;
    } else {
      begin_system_call();
      translated_msg = dgettext("clisplow",msgid);
      end_system_call();
    }
    return translated_msg;
  }

  #endif

#endif

# FIXME: Don't hardwire ISO-8859-1. The catalog's character set is
# given by the "Content-Type:" line in the meta information.
# in anticipation of this fix, CLSTEXT is a function, not a macro

global maygc object CLSTEXT (const char* asciz) {
 #ifdef GNU_GETTEXT
  return asciz_to_string(clgettext(asciz),Symbol_value(S(utf_8)));
 #else
  return ascii_to_string(asciz);
 #endif
}

global maygc object CLOTEXT (const char* asciz) {
  dynamic_bind(S(packagestern),O(default_package)); # bind *PACKAGE*
  pushSTACK(CLSTEXT(asciz)); funcall(L(read_from_string),1);
  dynamic_unbind(S(packagestern));
  return value1;
}
