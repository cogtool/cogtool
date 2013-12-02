# Choice of character set, and internationalization.

# ------------------------------ Specification --------------------------------

local void init_ctype (void);

# ------------------------------ Implementation -------------------------------

local void init_ctype() {
  # When you call setlocale(LC_CTYPE,""), it examines the environment
  # variables:
  # 1. environment variable LC_ALL - an override for all LC_* variables,
  # 2. environment variable LC_CTYPE,
  # 3. environment variable LANG - a default for all LC_* variables.
  var const char * locale;
  locale = getenv("CLISP_LC_CTYPE");
  if (!locale || !*locale) {
    locale = getenv("LC_ALL");
    if (!locale || !*locale) {
      locale = getenv("LC_CTYPE");
      if (!locale || !*locale)
        locale = getenv("LANG");
    }
  }
  # Make the <ctype.h> functions 8-bit clean, if the LC_CTYPE environment
  # variable is set accordingly.
  # (We don't use these functions directly, but additional modules like
  # regexp use them and profit from this.)
  if (locale && *locale)
    setlocale(LC_CTYPE,locale);

  # Also set the other locale facets that may be used by i18n.d.
  #if HAVE_LC_MESSAGES
  setlocale(LC_MESSAGES,"");
  #endif
  setlocale(LC_CTYPE,"");
  setlocale(LC_TIME,"");
  setlocale(LC_COLLATE,"");
  setlocale(LC_MONETARY,"");
}

