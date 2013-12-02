/*
 * Encodings (character sets and conversions) for CLISP
 * Bruno Haible 1998-2005
 * Sam Steingold 1998-2005
 */

#include "lispbibl.c"

#include <string.h>             /* declares memcpy() */
#include <stdio.h>              /* declares fprintf() */

#ifdef UNICODE
#include "libcharset.h"
#endif

/* =========================================================================
 * Individual encodings */

#ifdef UNICODE

/* NOTE 1! The mblen function has to be consistent with the mbstowcs function
 (when called with stream = nullobj).
 The wcslen function has to be consistent with the wcstombs function (when
 called with stream = nullobj). */

/* NOTE 2! The conversion from bytes to characters (mbstowcs function) is
 subject to the following restriction: At most one byte lookahead is needed.
 This means, when someone calls mbstowcs for converting one character, and
 he tries it with 1 byte, then with one more byte, then with one more byte,
 and so on: when the conversion succeeds for the first time, it will leave at
 most one byte in the buffer. stream.d (rd_ch_buffered, rd_ch_array_buffered)
 heavily depend on this. */

/* --------------------------------------------------------------------------
 * base64 http://rfc.net/rfc2045.html */

global uintL base64_mblen (object encoding, const uintB* src,
                           const uintB* srcend);
global void base64_mbstowcs (object encoding, object stream,
                             const uintB* *srcp, const uintB* srcend,
                             chart* *destp, chart* destend);
global uintL base64_wcslen (object encoding, const chart* src,
                            const chart* srcend);
global void base64_wcstombs (object encoding, object stream,
                             const chart* *srcp, const chart* srcend,
                             uintB* *destp, uintB* destend);
global object base64_range (object encoding, uintL start, uintL end,
                            uintL maxintervals);

static const char base64_table[64] = {
  'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P',
  'Q','R','S','T','U','V','W','X','Y','Z','a','b','c','d','e','f',
  'g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v',
  'w','x','y','z','0','1','2','3','4','5','6','7','8','9','+','/'
};
static const signed char table_base64[128] = {
  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,      /*  -1-  9 */
  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,      /*  10- 19 */
  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,      /*  20- 29 */
  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,  -1,      /*  30- 39 */
  -1,  -1,  -1,  62,  -1,  -1,  -1,  63,  52,  53,      /*  40- 49 */
  54,  55,  56,  57,  58,  59,  60,  61,  -1,  -1,      /*  50- 59 */
  -1,  -1,  -1,  -1,  -1,   0,   1,   2,   3,   4,      /*  60- 69 */
   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,      /*  70- 79 */
  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,      /*  80- 89 */
  25,  -1,  -1,  -1,  -1,  -1,  -1,  26,  27,  28,      /*  90- 99 */
  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,      /* 100-109 */
  39,  40,  41,  42,  43,  44,  45,  46,  47,  48,      /* 110-119 */
  49,  50,  51,  -1,  -1,  -1,  -1,  -1                 /* 120-127 */
};
/* alternatively:
    var int pos = 0;
    while (pos < sizeof(table_base64)) table_base64[pos++] = -1;
    for (pos = 0; pos < sizeof(base64_table); pos++)
      table_base64[base64_table[pos]] = pos;
*/

#define MIME_LINE_LENGTH  76

typedef enum { le_unix, le_mac, le_dos } line_end_t;
local line_end_t enc_eol_to_le (object enc_eol) {
  if (eq(enc_eol,S(Kunix))) return le_unix;
  if (eq(enc_eol,S(Kdos))) return le_dos;
  if (eq(enc_eol,S(Kmac))) return le_mac;
  NOTREACHED;
}

local uintL base64_to_chars (line_end_t le, const uintB* src,
                             const uintB* srcend, chart *dest) {
  var uintL pos = 0;
  var uintL counter = 0;
  var uintL num_chars = 0;
  while (src < srcend) {
    int c = *src++;
    if (counter < MIME_LINE_LENGTH/4) counter++;
    else { /* Wrap line every 76 characters. */
      counter = 1;
      switch (le) {
        case le_unix:
          if (dest) *dest++ = ascii(LF); num_chars++;
          break;
        case le_dos:
          if (dest) { *dest++ = ascii(CR); *dest++ = ascii(LF); }
          num_chars += 2;
          break;
        case le_mac:
          if (dest) *dest++ = ascii(CR); num_chars++;
          break;
      }
    }
    /* Process first byte of a triplet.  */
    if (dest) *dest++ = ascii(base64_table[0x3f & c >> 2]); num_chars++;
    var int value = (0x03 & c) << 4;
     /* Process second byte of a triplet.  */
    if (src == srcend) {
      if (dest) {
        *dest++ = ascii(base64_table[value]);
        *dest++ = ascii('=');
        *dest++ = ascii('=');
      }
      num_chars += 3;
      break;
    }
    c = *src++;
    if (dest) *dest++ = ascii(base64_table[value | (0x0f & c >> 4)]);
    num_chars++;
    value = (0x0f & c) << 2;
    /* Process third byte of a triplet.  */
    if (src == srcend) {
      if (dest) {
        *dest++ = ascii(base64_table[value]);
        *dest++ = ascii('=');
      }
      num_chars += 2;
      break;
    }
    c = *src++;
    if (dest) {
      *dest++ = ascii(base64_table[value | (0x03 & c >> 6)]);
      *dest++ = ascii(base64_table[0x3f & c]);
    }
    num_chars += 2;
  }
  return num_chars;
}

global uintL base64_mblen (object encoding, const uintB* src,
                           const uintB* srcend) {
  return base64_to_chars(enc_eol_to_le(TheEncoding(encoding)->enc_eol),
                         src,srcend,NULL);
}

/* see emacs/src/fns.c */
global void base64_mbstowcs (object encoding, object stream,
                             const uintB* *srcp, const uintB* srcend,
                             chart* *destp, chart* destend) {
  *destp += base64_to_chars(enc_eol_to_le(TheEncoding(encoding)->enc_eol),
                            *srcp,srcend,*destp);
  *srcp = srcend;
}

#define BASE64_P(c) (c<sizeof(table_base64) && table_base64[c]!=-1)
#define BASE64_IGNORABLE_P(ch)                                          \
  (chareq(ch,ascii(' ')) || chareq(ch,ascii('\t')) || chareq(ch,ascii('\n')) \
   || chareq(ch,ascii('\f')) || chareq(ch,ascii('\r')))

/* see emacs/src/fns.c */
#define READ_QUADRUPLET_BYTE(endform) do {                      \
  if (src == srcend) { endform; }                               \
  ch = *src++;                                                  \
 } while (BASE64_IGNORABLE_P(ch));                              \
  c = as_cint(ch)

/* convert ascii src to bytes dest - when destp is given
 return the number of bytes
 the final bad characted position is retutned in error_p */
local uintL base64_to_bytes (const chart *src, const chart* srcend,
                             uintB* destp, const chart* *error_p) {
  var unsigned char c;
  var chart ch;
  var unsigned long value;
  var uintB *dest = destp;
  var uintL num_bytes = 0;

  while (1) {
    /* Process first byte of a quadruplet. */
    READ_QUADRUPLET_BYTE(return num_bytes);
    if (!BASE64_P(c)) { *error_p = src-1; return num_bytes; }
    value = table_base64[c] << 18;

    /* Process second byte of a quadruplet.  */
    READ_QUADRUPLET_BYTE(*error_p = src; return num_bytes);
    if (!BASE64_P(c)) { *error_p = src-1; return num_bytes; }
    value |= table_base64[c] << 12;

    if (dest) *dest++ = (unsigned char) (value >> 16);
    num_bytes++;

    /* Process third byte of a quadruplet.  */
    READ_QUADRUPLET_BYTE(*error_p = src; return num_bytes);
    if (c == '=') {
      READ_QUADRUPLET_BYTE(*error_p = src; return num_bytes);
      if (c != '=') { *error_p = src-1; return num_bytes; }
      continue;
    }

    if (!BASE64_P(c)) { *error_p = src-1; return num_bytes; }
    value |= table_base64[c] << 6;

    if (dest) *dest++ = (unsigned char) (0xff & value >> 8);
    num_bytes++;

    /* Process fourth byte of a quadruplet.  */
    READ_QUADRUPLET_BYTE(*error_p = src-1; return num_bytes);
    if (c == '=')
      continue;
    if (!BASE64_P(c)) { *error_p = src-1; return num_bytes; }
    value |= table_base64[c];

    if (dest) *dest++ = (unsigned char) (0xff & value);
    num_bytes++;
  }
}

global uintL base64_wcslen (object encoding, const chart* src,
                            const chart* srcend) {
  var const chart *error_p = NULL;
  return base64_to_bytes(src,srcend,NULL,&error_p)
    + (error_p ? 1 : 0);        /* space for errors */
}

global void base64_wcstombs (object encoding, object stream,
                             const chart* *srcp, const chart* srcend,
                             uintB* *destp, uintB* destend) {
  var const chart *error_p = NULL;
  *destp += base64_to_bytes(*srcp,srcend,*destp,&error_p);
  if (error_p) {
    pushSTACK(fixnum(srcend-*srcp));
    pushSTACK(fixnum(error_p-*srcp));
    pushSTACK(code_char(*error_p));
    fehler(charset_type_error,GETTEXT("Invalid base64 encoding at ~S (character ~S of ~S)"));
  }
  *srcp = srcend;
}

global object base64_range (object encoding, uintL start, uintL end,
                            uintL maxintervals) {
  var uintL count = 0; /* number of intervals already on the STACK */
  if (end >= sizeof(table_base64)) end = sizeof(table_base64) - 1;
  for (;start <= end && count < maxintervals; count++) {
    while ((start <= end) && (table_base64[start] == -1)) start++;
    if (start > end) break;
    pushSTACK(code_char(as_chart(start)));
    while ((start <= end) && (table_base64[start] != -1)) start++;
    pushSTACK(code_char(as_chart(start-1)));
  }
  return stringof(count << 1);
}

local char const hex_table[] = "0123456789ABCDEF";

/* Error, when a character cannot be converted to an encoding.
 fehler_unencodable(encoding,ch); */
nonreturning_function(global, fehler_unencodable,
                      (object encoding, chart ch)) {
  pushSTACK(code_char(ch)); /* CHARSET-TYPE-ERROR slot DATUM */
  pushSTACK(encoding); /* CHARSET-TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(TheEncoding(encoding)->enc_charset);
  pushSTACK(ascii_char(hex_table[as_cint(ch)&0x0F]));
  pushSTACK(ascii_char(hex_table[(as_cint(ch)>>4)&0x0F]));
  pushSTACK(ascii_char(hex_table[(as_cint(ch)>>8)&0x0F]));
  pushSTACK(ascii_char(hex_table[(as_cint(ch)>>12)&0x0F]));
  if (as_cint(ch) < 0x10000)
    fehler(charset_type_error,
           GETTEXT("Character #\\u~C~C~C~C cannot be represented in the character set ~S"));
  else {
    pushSTACK(ascii_char(hex_table[(as_cint(ch)>>16)&0x0F]));
    pushSTACK(ascii_char(hex_table[(as_cint(ch)>>20)&0x0F]));
    fehler(charset_type_error,
           GETTEXT("Character #\\u00~C~C~C~C~C~C cannot be represented in the character set ~S"));
  }
}

/* odd buffer for ucs-2 and such */
nonreturning_function(local, fehler_buffer_parity, (object encoding)) {
  pushSTACK(TheEncoding(encoding)->enc_charset);
  fehler(error,GETTEXT("incomplete byte sequence at end of buffer for ~S"));
}

/* The range function for an encoding covering all of Unicode. */
global object all_range (object encoding, uintL start, uintL end,
                         uintL maxintervals) {
  var uintL count = 0;
  if (maxintervals > 0) {
    pushSTACK(code_char(as_chart(start))); pushSTACK(code_char(as_chart(end)));
    count = 2;
  }
  return stringof(count);
}

/* The range function for an encoding covering the BMP of Unicode. */
global object bmp_range (object encoding, uintL start, uintL end,
                         uintL maxintervals) {
  var uintL count = 0;
  if (maxintervals > 0 && start < 0x10000) {
    if (end >= 0x10000)
      end = 0xFFFF;
    pushSTACK(code_char(as_chart(start))); pushSTACK(code_char(as_chart(end)));
    count = 2;
  }
  return stringof(count);
}

/* --------------------------------------------------------------------------
 * Unicode-16 encoding */

/* Unicode-16 encoding in two flavours:
 The big-endian format (files starting with 0xFE 0xFF),
 the little-endian format (files starting with 0xFF 0xFE). */

/* min. bytes per character = 2
   max. bytes per character = 2 */

global uintL uni16_mblen (object encoding, const uintB* src,
                          const uintB* srcend);
global void uni16be_mbstowcs (object encoding, object stream,
                              const uintB* *srcp, const uintB* srcend,
                              chart* *destp, chart* destend);
global void uni16le_mbstowcs (object encoding, object stream,
                              const uintB* *srcp, const uintB* srcend,
                              chart* *destp, chart* destend);
global uintL uni16_wcslen (object encoding, const chart* src,
                           const chart* srcend);
global void uni16be_wcstombs (object encoding, object stream,
                              const chart* *srcp, const chart* srcend,
                              uintB* *destp, uintB* destend);
global void uni16le_wcstombs (object encoding, object stream,
                              const chart* *srcp, const chart* srcend,
                              uintB* *destp, uintB* destend);

/* Bytes to characters. */

global uintL uni16_mblen (object encoding, const uintB* src,
                          const uintB* srcend) {
  var uintL len = srcend-src;
  var bool error_p = len & 1; /* odd-p */
  var uintL count = len >> 1; /* mod 2 */
  if (error_p && !eq(TheEncoding(encoding)->enc_towcs_error,S(Kignore)))
    return count+1;
  else return count;
}

global void uni16be_mbstowcs (object encoding, object stream,
                              const uintB* *srcp, const uintB* srcend,
                              chart* *destp, chart* destend) {
  var const uintB* src = *srcp;
  var chart* dest = *destp;
  var uintL len = srcend-src;
  var bool error_p = len & 1; /* odd-p */
  var uintL count = len >> 1; /* mod 2 */
  if (count > destend-dest)
    count = destend-dest;
  if (count > 0) {
    do {
      *dest++ = as_chart(((cint)src[0] << 8) | (cint)src[1]);
      src += 2;
    } while (--count);
    *srcp = src;
    *destp = dest;
  }
  if (error_p) {
    var object action = TheEncoding(encoding)->enc_towcs_error;
    if (eq(action,S(Kignore))) {
    } else if (eq(action,S(Kerror))) {
      fehler_buffer_parity(encoding);
    } else
      *(*destp)++ = char_code(action);
  }
}

global void uni16le_mbstowcs (object encoding, object stream,
                              const uintB* *srcp, const uintB* srcend,
                              chart* *destp, chart* destend) {
  var const uintB* src = *srcp;
  var chart* dest = *destp;
  var uintL len = srcend-src;
  var bool error_p = len & 1; /* odd-p */
  var uintL count = len >> 1; /* mod 2 */
  if (count > destend-dest)
    count = destend-dest;
  if (count > 0) {
    do {
      *dest++ = as_chart((cint)src[0] | ((cint)src[1] << 8));
      src += 2;
    } while (--count);
    *srcp = src;
    *destp = dest;
  }
  if (error_p) {
    var object action = TheEncoding(encoding)->enc_towcs_error;
    if (eq(action,S(Kignore))) {
    } else if (eq(action,S(Kerror))) {
      fehler_buffer_parity(encoding);
    } else
      *(*destp)++ = char_code(action);
  }
}

/* Characters to bytes. */

global uintL uni16_wcslen (object encoding, const chart* src,
                           const chart* srcend) {
  var uintL count = srcend-src;
  var uintL result = 0;
  while (count--) {
    var chart ch = *src++;
    if (as_cint(ch) < 0x10000)
      result += 2;
    else {
      var object action = TheEncoding(encoding)->enc_tombs_error;
      if (eq(action,S(Kignore))) {
      } else if (uint8_p(action)) {
        result++;
      } else if (!eq(action,S(Kerror))) {
        var chart c = char_code(action);
        if (as_cint(c) < 0x10000)
          result += 2;
      } else
        fehler_unencodable(encoding,ch);
    }
  }
  return result;
}

global void uni16be_wcstombs (object encoding, object stream,
                              const chart* *srcp, const chart* srcend,
                              uintB* *destp, uintB* destend) {
  var const chart* src = *srcp;
  var uintB* dest = *destp;
  var uintL scount = srcend-src;
  var uintL dcount = destend-dest;
  if (scount > 0 && dcount > 0) {
    do {
      var cint ch = as_cint(*src++); scount--;
      if (ch < 0x10000) {
        if (dcount < 2) break;
        dest[0] = (uintB)(ch>>8); dest[1] = (uintB)ch;
        dest += 2; dcount -= 2;
      } else {
        var object action = TheEncoding(encoding)->enc_tombs_error;
        if (eq(action,S(Kignore))) {
        } else if (uint8_p(action)) {
          *dest++ = I_to_uint8(action); dcount--;
        } else if (!eq(action,S(Kerror))) {
          var cint c = char_int(action);
          if (c < 0x10000) {
            if (dcount < 2) break;
            dest[0] = (uintB)(c>>8); dest[1] = (uintB)c;
            dest += 2; dcount -= 2;
          } else
            fehler_unencodable(encoding,as_chart(ch));
        } else
          fehler_unencodable(encoding,as_chart(ch));
      }
    } while (scount > 0 && dcount > 0);
    *srcp = src;
    *destp = dest;
  }
}

global void uni16le_wcstombs (object encoding, object stream,
                              const chart* *srcp, const chart* srcend,
                              uintB* *destp, uintB* destend) {
  var const chart* src = *srcp;
  var uintB* dest = *destp;
  var uintL scount = srcend-src;
  var uintL dcount = destend-dest;
  if (scount > 0 && dcount > 0) {
    do {
      var cint ch = as_cint(*src++); scount--;
      if (ch < 0x10000) {
        if (dcount < 2) break;
        dest[0] = (uintB)ch; dest[1] = (uintB)(ch>>8);
        dest += 2; dcount -= 2;
      } else {
        var object action = TheEncoding(encoding)->enc_tombs_error;
        if (eq(action,S(Kignore))) {
        } else if (uint8_p(action)) {
          *dest++ = I_to_uint8(action); dcount--;
        } else if (!eq(action,S(Kerror))) {
          var cint c = char_int(action);
          if (c < 0x10000) {
            if (dcount < 2) break;
            dest[0] = (uintB)c; dest[1] = (uintB)(c>>8);
            dest += 2; dcount -= 2;
          } else
            fehler_unencodable(encoding,as_chart(ch));
        } else
          fehler_unencodable(encoding,as_chart(ch));
      }
    } while (scount > 0 && dcount > 0);
    *srcp = src;
    *destp = dest;
  }
}

/* -------------------------------------------------------------------------
 * Unicode-32 encoding */

/* Unicode-32 encoding in two flavours:
 The big-endian format,
 the little-endian format. */

/* min. bytes per character = 4
   max. bytes per character = 4 */

global uintL uni32be_mblen (object encoding, const uintB* src,
                            const uintB* srcend);
global uintL uni32le_mblen (object encoding, const uintB* src,
                            const uintB* srcend);
global void uni32be_mbstowcs (object encoding, object stream,
                              const uintB* *srcp, const uintB* srcend,
                              chart* *destp, chart* destend);
global void uni32le_mbstowcs (object encoding, object stream,
                              const uintB* *srcp, const uintB* srcend,
                              chart* *destp, chart* destend);
global uintL uni32_wcslen (object encoding, const chart* src,
                           const chart* srcend);
global void uni32be_wcstombs (object encoding, object stream,
                              const chart* *srcp, const chart* srcend,
                              uintB* *destp, uintB* destend);
global void uni32le_wcstombs (object encoding, object stream,
                              const chart* *srcp, const chart* srcend,
                              uintB* *destp, uintB* destend);

/* Bytes to characters. */

/* Error when an invalid character was encountered.
 fehler_uni32_invalid(encoding,code); */
nonreturning_function(local, fehler_uni32_invalid,
                      (object encoding, uint32 code)) {
  var uintC count;
  pushSTACK(TheEncoding(encoding)->enc_charset);
  dotimespC(count,8, {
    pushSTACK(ascii_char(hex_table[code&0x0F]));
    code = code>>4;
  });
  fehler(error,
         GETTEXT("character #x~C~C~C~C~C~C~C~C in ~S conversion, not an UTF-32 character"));
}

global uintL uni32be_mblen (object encoding, const uintB* src,
                            const uintB* srcend) {
  var uintL len = srcend-src;
  var bool error_p = ((len & 3) != 0); /* rem 4 */
  var uintL count = len >> 2; /* mod 4 */
  if (!eq(TheEncoding(encoding)->enc_towcs_error,S(Kignore)))
    return count + error_p;
  else {
    var uintL result = 0;
    dotimesL(count,count, {
      var uint32 ch =
        ((uint32)src[0] << 24) | ((uint32)src[1] << 16)
        | ((uint32)src[2] << 8) | (uint32)src[3];
      if (ch < char_code_limit)
        result++;
      src += 4;
    });
    return result;
  }
}

global uintL uni32le_mblen (object encoding, const uintB* src,
                            const uintB* srcend) {
  var uintL len = srcend-src;
  var bool error_p = ((len & 3) != 0); /* rem 4 */
  var uintL count = len >> 2; /* mod 4 */
  if (!eq(TheEncoding(encoding)->enc_towcs_error,S(Kignore)))
    return count + error_p;
  else {
    var uintL result = 0;
    dotimesL(count,count, {
      var uint32 ch =
        (uint32)src[0] | ((uint32)src[1] << 8)
        | ((uint32)src[2] << 16) | ((uint32)src[3] << 24);
      if (ch < char_code_limit)
        result++;
      src += 4;
    });
    return result;
  }
}

global void uni32be_mbstowcs (object encoding, object stream,
                              const uintB* *srcp, const uintB* srcend,
                              chart* *destp, chart* destend) {
  var const uintB* src = *srcp;
  var chart* dest = *destp;
  var uintL len = srcend-src;
  var bool error_p = ((len & 3) != 0); /* rem 4 */
  var uintL scount = len >> 2; /* mod 4 */
  var uintL dcount = destend-dest;
  if (scount > 0 && dcount > 0) {
    do {
      var uint32 ch =
        ((uint32)src[0] << 24) | ((uint32)src[1] << 16)
        | ((uint32)src[2] << 8) | (uint32)src[3];
      if (ch < char_code_limit) {
        *dest++ = as_chart(ch); dcount--;
      } else {
        var object action = TheEncoding(encoding)->enc_towcs_error;
        if (eq(action,S(Kignore))) {
        } else if (eq(action,S(Kerror))) {
          fehler_uni32_invalid(encoding,ch);
        } else {
          *dest++ = char_code(action); dcount--;
        }
      }
      src += 4; scount--;
    } while (scount > 0 && dcount > 0);
    *srcp = src;
    *destp = dest;
  }
  if (error_p) {
    var object action = TheEncoding(encoding)->enc_towcs_error;
    if (eq(action,S(Kignore))) {
    } else if (eq(action,S(Kerror))) {
      fehler_buffer_parity(encoding);
    } else
      *(*destp)++ = char_code(action);
  }
}

global void uni32le_mbstowcs (object encoding, object stream,
                              const uintB* *srcp, const uintB* srcend,
                              chart* *destp, chart* destend) {
  var const uintB* src = *srcp;
  var chart* dest = *destp;
  var uintL len = srcend-src;
  var bool error_p = ((len & 3) != 0); /* rem 4 */
  var uintL scount = len >> 2; /* mod 4 */
  var uintL dcount = destend-dest;
  if (scount > 0 && dcount > 0) {
    do {
      var uint32 ch =
        (uint32)src[0] | ((uint32)src[1] << 8)
        | ((uint32)src[2] << 16) | ((uint32)src[3] << 24);
      if (ch < char_code_limit) {
        *dest++ = as_chart(ch); dcount--;
      } else {
        var object action = TheEncoding(encoding)->enc_towcs_error;
        if (eq(action,S(Kignore))) {
        } else if (eq(action,S(Kerror))) {
          fehler_uni32_invalid(encoding,ch);
        } else {
          *dest++ = char_code(action); dcount--;
        }
      }
      src += 4; scount--;
    } while (scount > 0 && dcount > 0);
    *srcp = src;
    *destp = dest;
  }
  if (error_p) {
    var object action = TheEncoding(encoding)->enc_towcs_error;
    if (eq(action,S(Kignore))) {
    } else if (eq(action,S(Kerror))) {
      fehler_buffer_parity(encoding);
    } else
      *(*destp)++ = char_code(action);
  }
}

/* Characters to bytes. */

global uintL uni32_wcslen (object encoding, const chart* src,
                           const chart* srcend) {
  return (srcend-src)*4;
}

global void uni32be_wcstombs (object encoding, object stream,
                              const chart* *srcp, const chart* srcend,
                              uintB* *destp, uintB* destend) {
  var const chart* src = *srcp;
  var uintB* dest = *destp;
  var uintL count = floor(destend-dest,4);
  if (count > srcend-src)
    count = srcend-src;
  if (count > 0) {
    dotimespL(count,count, {
      var cint ch = as_cint(*src++);
      dest[0] = 0; dest[1] = (uintB)(ch>>16);
      dest[2] = (uintB)(ch>>8); dest[3] = (uintB)ch;
      dest += 4;
    });
    *srcp = src;
    *destp = dest;
  }
}

global void uni32le_wcstombs (object encoding, object stream,
                              const chart* *srcp, const chart* srcend,
                              uintB* *destp, uintB* destend) {
  var const chart* src = *srcp;
  var uintB* dest = *destp;
  var uintL count = floor(destend-dest,4);
  if (count > srcend-src)
    count = srcend-src;
  if (count > 0) {
    dotimespL(count,count, {
      var cint ch = as_cint(*src++);
      dest[0] = (uintB)ch; dest[1] = (uintB)(ch>>8);
      dest[2] = (uintB)(ch>>16); dest[3] = 0;
      dest += 4;
    });
    *srcp = src;
    *destp = dest;
  }
}

/* -------------------------------------------------------------------------
 * UTF-8 encoding */

/* See http://www.stonehand.com/unicode/standard/fss-utf.html
 or  Linux 2.0.x, file linux/fs/nls.c
                   cmask  cval  shift     maxval           bits
  1 byte sequence   0x80  0x00   0*6         0x7F  0XXXXXXX
  2 byte sequence   0xE0  0xC0   1*6        0x7FF  110XXXXX 10XXXXXX
  3 byte sequence   0xF0  0xE0   2*6       0xFFFF  1110XXXX 10XXXXXX 10XXXXXX
  4 byte sequence   0xF8  0xF0   3*6     0x1FFFFF  11110XXX 10XXXXXX 10XXXXXX 10XXXXXX
  5 byte sequence   0xFC  0xF8   4*6    0x3FFFFFF  111110XX 10XXXXXX 10XXXXXX 10XXXXXX 10XXXXXX
  6 byte sequence   0xFE  0xFC   5*6   0x7FFFFFFF  1111110X 10XXXXXX 10XXXXXX 10XXXXXX 10XXXXXX 10XXXXXX

 We support only 21-bit Unicode characters, i.e. those which can be encoded
 with at most 4 bytes. Characters outside this range give an error.
 Spurious bytes of the form 10XXXXXX are ignored.
 (This resync feature is one of the benefits of the UTF encoding.) */

/* min. bytes per character = 1
   max. bytes per character = 4 */

global uintL utf8_mblen (object encoding, const uintB* src,
                         const uintB* srcend);
global void utf8_mbstowcs (object encoding, object stream, const uintB* *srcp,
                           const uintB* srcend, chart* *destp, chart* destend);
global uintL utf8_wcslen (object encoding, const chart* src,
                          const chart* srcend);
global void utf8_wcstombs (object encoding, object stream, const chart* *srcp,
                           const chart* srcend, uintB* *destp, uintB* destend);

/* Bytes to characters. */

/* Error when an invalid 1-byte sequence was encountered.
 fehler_utf8_invalid1(encoding,b1); */
nonreturning_function(local, fehler_utf8_invalid1,
                      (object encoding, uintB b1)) {
  pushSTACK(TheEncoding(encoding)->enc_charset);
  pushSTACK(ascii_char(hex_table[b1&0x0F]));
  pushSTACK(ascii_char(hex_table[(b1>>4)&0x0F]));
  fehler(error,GETTEXT("invalid byte #x~C~C in ~S conversion, not a Unicode-16"));
}

/* Error when an invalid 2-byte sequence was encountered.
 fehler_utf8_invalid2(encoding,b1,b2); */
nonreturning_function(local, fehler_utf8_invalid2,
                      (object encoding, uintB b1, uintB b2)) {
  pushSTACK(TheEncoding(encoding)->enc_charset);
  pushSTACK(ascii_char(hex_table[b2&0x0F]));
  pushSTACK(ascii_char(hex_table[(b2>>4)&0x0F]));
  pushSTACK(ascii_char(hex_table[b1&0x0F]));
  pushSTACK(ascii_char(hex_table[(b1>>4)&0x0F]));
  fehler(error,GETTEXT("invalid byte sequence #x~C~C #x~C~C in ~S conversion"));
}

/* Error when an invalid 3-byte sequence was encountered.
 fehler_utf8_invalid3(encoding,b1,b2,b3); */
nonreturning_function(local, fehler_utf8_invalid3,
                      (object encoding, uintB b1, uintB b2, uintB b3)) {
  pushSTACK(TheEncoding(encoding)->enc_charset);
  pushSTACK(ascii_char(hex_table[b3&0x0F]));
  pushSTACK(ascii_char(hex_table[(b3>>4)&0x0F]));
  pushSTACK(ascii_char(hex_table[b2&0x0F]));
  pushSTACK(ascii_char(hex_table[(b2>>4)&0x0F]));
  pushSTACK(ascii_char(hex_table[b1&0x0F]));
  pushSTACK(ascii_char(hex_table[(b1>>4)&0x0F]));
  fehler(error,
         GETTEXT("invalid byte sequence #x~C~C #x~C~C #x~C~C in ~S conversion"));
}

/* Error when an invalid 4-byte sequence was encountered.
 fehler_utf8_invalid4(encoding,b1,b2,b3,b4); */
nonreturning_function(local, fehler_utf8_invalid4,
                      (object encoding, uintB b1, uintB b2, uintB b3, uintB b4)) {
  pushSTACK(TheEncoding(encoding)->enc_charset);
  pushSTACK(ascii_char(hex_table[b4&0x0F]));
  pushSTACK(ascii_char(hex_table[(b4>>4)&0x0F]));
  pushSTACK(ascii_char(hex_table[b3&0x0F]));
  pushSTACK(ascii_char(hex_table[(b3>>4)&0x0F]));
  pushSTACK(ascii_char(hex_table[b2&0x0F]));
  pushSTACK(ascii_char(hex_table[(b2>>4)&0x0F]));
  pushSTACK(ascii_char(hex_table[b1&0x0F]));
  pushSTACK(ascii_char(hex_table[(b1>>4)&0x0F]));
  fehler(error,
         GETTEXT("invalid byte sequence #x~C~C #x~C~C #x~C~C #x~C~C in ~S conversion"));
}

global uintL utf8_mblen (object encoding, const uintB* src,
                         const uintB* srcend) {
  var uintL count = 0;
  while (src < srcend) {
    var uintB c = src[0];
    if (c < 0x80) { /* 1 byte sequence */
      src += 1;
      count++;
      continue;
    }
    if (c < 0xC0) {
      src++; continue; /* skip spurious 10XXXXXX byte */
    }
    if (c < 0xE0) { /* 2 byte sequence */
      if (src+2 > srcend) break;
      if (((src[1] ^ 0x80) < 0x40)
          && (c >= 0xC2)) {
        src += 2;
        count++;
        continue;
      }
      {
        var object action = TheEncoding(encoding)->enc_towcs_error;
        if (eq(action,S(Kignore))) {
          src += 2; continue;
        } else if (eq(action,S(Kerror))) {
          fehler_utf8_invalid2(encoding,c,src[1]);
        } else {
          src += 2; count++; continue;
        }
      }
    }
    if (c < 0xF0) { /* 3 byte sequence */
      if (src+3 > srcend) break;
      if (((src[1] ^ 0x80) < 0x40) && ((src[2] ^ 0x80) < 0x40)
          && (c >= 0xE1 || src[1] >= 0xA0)
          && (c != 0xED || src[1] < 0xA0)) {
        src += 3;
        count++;
        continue;
      }
      {
        var object action = TheEncoding(encoding)->enc_towcs_error;
        if (eq(action,S(Kignore))) {
          src += 3; continue;
        } else if (eq(action,S(Kerror))) {
          fehler_utf8_invalid3(encoding,c,src[1],src[2]);
        } else {
          src += 3; count++; continue;
        }
      }
    }
    if (c < 0xF8) { /* 4 byte sequence */
      if (src+4 > srcend) break;
      if (((src[1] ^ 0x80) < 0x40) && ((src[2] ^ 0x80) < 0x40)
          && ((src[3] ^ 0x80) < 0x40)
          && (c >= 0xF1 || src[1] >= 0x90)) {
        src += 4;
        count++;
        continue;
      }
      {
        var object action = TheEncoding(encoding)->enc_towcs_error;
        if (eq(action,S(Kignore))) {
          src += 4; continue;
        } else if (eq(action,S(Kerror))) {
          fehler_utf8_invalid4(encoding,c,src[1],src[2],src[3]);
        } else {
          src += 4; count++; continue;
        }
      }
    }
    {
      var object action = TheEncoding(encoding)->enc_towcs_error;
      if (eq(action,S(Kignore))) {
        src += 1; continue;
      } else if (eq(action,S(Kerror))) {
        fehler_utf8_invalid1(encoding,c);
      } else {
        src += 1; count++; continue;
      }
    }
  }
  return count;
}

global void utf8_mbstowcs (object encoding, object stream, const uintB* *srcp,
                           const uintB* srcend, chart* *destp,
                           chart* destend) {
  var const uintB* src = *srcp;
  var chart* dest = *destp;
  while (src < srcend) {
    var uintB c = src[0];
    if (c < 0x80) { /* 1 byte sequence */
      if (dest == destend) break;
      *dest++ = as_chart((cint)c);
      src += 1;
      continue;
    }
    if (c < 0xC0) {
      src++; continue; /* skip spurious 10XXXXXX byte */
    }
    if (dest == destend) break;
    if (c < 0xE0) { /* 2 byte sequence */
      if (src+2 > srcend) break;
      if (((src[1] ^ 0x80) < 0x40)
          && (c >= 0xC2)) {
        *dest++ = as_chart(((cint)(c & 0x1F) << 6) | (cint)(src[1] ^ 0x80));
        src += 2;
        continue;
      }
      {
        var object action = TheEncoding(encoding)->enc_towcs_error;
        if (eq(action,S(Kignore))) {
          src += 2; continue;
        } else if (eq(action,S(Kerror))) {
          fehler_utf8_invalid2(encoding,c,src[1]);
        } else {
          src += 2; *dest++ = char_code(action); continue;
        }
      }
    }
    if (c < 0xF0) { /* 3 byte sequence */
      if (src+3 > srcend) break;
      if (((src[1] ^ 0x80) < 0x40) && ((src[2] ^ 0x80) < 0x40)
          && (c >= 0xE1 || src[1] >= 0xA0)
          && (c != 0xED || src[1] < 0xA0)) {
        *dest++ = as_chart(((cint)(c & 0x0F) << 12)
                           | ((cint)(src[1] ^ 0x80) << 6)
                           | (cint)(src[2] ^ 0x80));
        src += 3;
        continue;
      }
      {
        var object action = TheEncoding(encoding)->enc_towcs_error;
        if (eq(action,S(Kignore))) {
          src += 3; continue;
        } else if (eq(action,S(Kerror))) {
          fehler_utf8_invalid3(encoding,c,src[1],src[2]);
        } else {
          src += 3; *dest++ = char_code(action); continue;
        }
      }
    }
    if (c < 0xF8) { /* 4 byte sequence */
      if (src+4 > srcend) break;
      if (((src[1] ^ 0x80) < 0x40) && ((src[2] ^ 0x80) < 0x40)
          && ((src[3] ^ 0x80) < 0x40)
          && (c >= 0xF1 || src[1] >= 0x90)) {
        var cint ch = ((cint)(c & 0x07) << 18)
                      | ((cint)(src[1] ^ 0x80) << 12)
                      | ((cint)(src[2] ^ 0x80) << 6)
                      | (cint)(src[3] ^ 0x80);
        if (ch < char_code_limit) {
          *dest++ = as_chart(ch);
          src += 4;
          continue;
        }
      }
      {
        var object action = TheEncoding(encoding)->enc_towcs_error;
        if (eq(action,S(Kignore))) {
          src += 4; continue;
        } else if (eq(action,S(Kerror))) {
          fehler_utf8_invalid4(encoding,c,src[1],src[2],src[3]);
        } else {
          src += 4; *dest++ = char_code(action); continue;
        }
      }
    }
    {
      var object action = TheEncoding(encoding)->enc_towcs_error;
      if (eq(action,S(Kignore))) {
        src += 1; continue;
      } else if (eq(action,S(Kerror))) {
        fehler_utf8_invalid1(encoding,c);
      } else {
        src += 1; *dest++ = char_code(action); continue;
      }
    }
  }
  *srcp = src;
  *destp = dest;
}

/* Characters to bytes. */

global uintL utf8_wcslen (object encoding, const chart* src,
                          const chart* srcend) {
  var uintL destlen = 0;
  while (src < srcend) {
    var cint ch = as_cint(*src++);
    destlen += (ch < 0x80 ? 1 : ch < 0x800 ? 2 : 3);
  }
  return destlen;
}

global void utf8_wcstombs (object encoding, object stream, const chart* *srcp,
                           const chart* srcend, uintB* *destp,
                           uintB* destend) {
  var const chart* src = *srcp;
  var uintB* dest = *destp;
  while (src < srcend) {
    var cint ch = as_cint(*src);
    var uintL count = (ch < 0x80 ? 1 : ch < 0x800 ? 2 : 3);
    if (dest+count > destend) break;
    src++;
    if (ch < 0x80) { /* 1 byte sequence */
      *dest++ = ch;
    } else if (ch < 0x800) { /* 2 byte sequence */
      *dest++ = 0xC0 | (ch >> 6);
      *dest++ = 0x80 | (ch & 0x3F);
    } else if (ch < 0x10000) { /* 3 byte sequence */
      *dest++ = 0xE0 | (ch >> 12);
      *dest++ = 0x80 | ((ch >> 6) & 0x3F);
      *dest++ = 0x80 | (ch & 0x3F);
    } else { /* ch < 0x110000, 4 byte sequence */
      *dest++ = 0xF0 | (ch >> 18);
      *dest++ = 0x80 | ((ch >> 12) & 0x3F);
      *dest++ = 0x80 | ((ch >> 6) & 0x3F);
      *dest++ = 0x80 | (ch & 0x3F);
    }
  }
  *srcp = src;
  *destp = dest;
}

/* -------------------------------------------------------------------------
 * Java encoding */

/* This is ISO 8859-1 with \uXXXX escape sequences,
   denoting Unicode characters.
 See the Java Language Specification.
 Characters outside the BMP are represented by two consecutive
 \uXXXX escape sequences, like UTF-16. Example:
   $ printf '\U00102345\n' | native2ascii -encoding UTF-8
   \udbc8\udf45

 This is quick&dirty: The text is supposed not to contain \u except as part
 of \uXXXX escape sequences. */

/* min. bytes per character = 1
   max. bytes per character = 12 */

global uintL java_mblen (object encoding, const uintB* src,
                         const uintB* srcend);
global void java_mbstowcs (object encoding, object stream, const uintB* *srcp,
                           const uintB* srcend, chart* *destp, chart* destend);
global uintL java_wcslen (object encoding, const chart* src,
                          const chart* srcend);
global void java_wcstombs (object encoding, object stream, const chart* *srcp,
                           const chart* srcend, uintB* *destp, uintB* destend);

/* Bytes to characters. */

global uintL java_mblen (object encoding, const uintB* src,
                         const uintB* srcend) {
  var uintL count = 0;
  while (src < srcend) {
    var uintB c;
    var cint ch;
    if (src[0] != '\\') {
      src += 1;
      count++;
      continue;
    }
    if (src+2 > srcend) break;
    if (src[1] != 'u') {
      src += 1;
      count++;
      continue;
    }
    if (src+3 > srcend) break;
    c = src[2];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 2; /* skip incomplete \u sequence */
      continue;
    }
    ch = (cint)c << 12;
    if (src+4 > srcend) break;
    c = src[3];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 3; /* skip incomplete \u sequence */
      continue;
    }
    ch |= (cint)c << 8;
    if (src+5 > srcend) break;
    c = src[4];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 4; /* skip incomplete \u sequence */
      continue;
    }
    ch |= (cint)c << 4;
    if (src+6 > srcend) break;
    c = src[5];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 5; /* skip incomplete \u sequence */
      continue;
    }
    ch |= (cint)c;
    if (ch < char_code_limit && !(ch >= 0xd800 && ch < 0xe000)) {
      src += 6; /* complete \u sequence */
      count++;
      continue;
    }
    if (!(ch >= 0xd800 && ch < 0xdc00)) {
      src += 6; /* skip invalid \u sequence */
      continue;
    }
    var cint ch1 = ch;
    if (src+7 > srcend) break;
    if (src[6] != '\\') {
      src += 6; /* skip incomplete \u sequence */
      continue;
    }
    if (src+8 > srcend) break;
    if (src[7] != 'u') {
      src += 6; /* skip incomplete \u sequence */
      continue;
    }
    if (src+9 > srcend) break;
    c = src[8];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 8; /* skip incomplete \u sequence */
      continue;
    }
    ch = (cint)c << 12;
    if (src+10 > srcend) break;
    c = src[9];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 9; /* skip incomplete \u sequence */
      continue;
    }
    ch |= (cint)c << 8;
    if (src+11 > srcend) break;
    c = src[10];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 10; /* skip incomplete \u sequence */
      continue;
    }
    ch |= (cint)c << 4;
    if (src+12 > srcend) break;
    c = src[11];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 11; /* skip incomplete \u sequence */
      continue;
    }
    ch |= (cint)c;
    if (ch >= 0xdc00 && ch < 0xe000) {
      ch = 0x10000 + ((ch1 - 0xd800) << 10) + (ch - 0xdc00);
      if (ch < char_code_limit) {
        src += 12; /* complete \u sequence */
        count++;
        continue;
      }
    }
    src += 6; /* skip invalid \u sequence */
    continue;
  }
  return count;
}

global void java_mbstowcs (object encoding, object stream, const uintB* *srcp,
                           const uintB* srcend, chart* *destp,
                           chart* destend) {
  var const uintB* src = *srcp;
  var chart* dest = *destp;
  while (src < srcend) {
    var uintB c;
    var cint ch;
    c = src[0];
    if (c != '\\') {
      if (dest==destend) break;
      *dest++ = as_chart((cint)c);
      src += 1;
      continue;
    }
    if (src+2 > srcend) break;
    if (src[1] != 'u') {
      if (dest==destend) break;
      *dest++ = as_chart((cint)c);
      src += 1;
      continue;
    }
    if (src+3 > srcend) break;
    c = src[2];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 2; /* skip incomplete \u sequence */
      continue;
    }
    ch = (cint)c << 12;
    if (src+4 > srcend) break;
    c = src[3];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 3; /* skip incomplete \u sequence */
      continue;
    }
    ch |= (cint)c << 8;
    if (src+5 > srcend) break;
    c = src[4];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 4; /* skip incomplete \u sequence */
      continue;
    }
    ch |= (cint)c << 4;
    if (src+6 > srcend) break;
    c = src[5];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 5; /* skip incomplete \u sequence */
      continue;
    }
    ch |= (cint)c;
    if (ch < char_code_limit && !(ch >= 0xd800 && ch < 0xe000)) {
      if (dest==destend) break;
      *dest++ = as_chart(ch);
      src += 6; /* complete \u sequence */
      continue;
    }
    if (!(ch >= 0xd800 && ch < 0xdc00)) {
      src += 6; /* skip invalid \u sequence */
      continue;
    }
    var cint ch1 = ch;
    if (src+7 > srcend) break;
    if (src[6] != '\\') {
      src += 6; /* skip incomplete \u sequence */
      continue;
    }
    if (src+8 > srcend) break;
    if (src[7] != 'u') {
      src += 6; /* skip incomplete \u sequence */
      continue;
    }
    if (src+9 > srcend) break;
    c = src[8];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 8; /* skip incomplete \u sequence */
      continue;
    }
    ch = (cint)c << 12;
    if (src+10 > srcend) break;
    c = src[9];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 9; /* skip incomplete \u sequence */
      continue;
    }
    ch |= (cint)c << 8;
    if (src+11 > srcend) break;
    c = src[10];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 10; /* skip incomplete \u sequence */
      continue;
    }
    ch |= (cint)c << 4;
    if (src+12 > srcend) break;
    c = src[11];
    if (c >= '0' && c <= '9') { c -= '0'; }
    else if (c >= 'A' && c <= 'F') { c -= 'A'-10; }
    else if (c >= 'a' && c <= 'f') { c -= 'a'-10; }
    else {
      src += 11; /* skip incomplete \u sequence */
      continue;
    }
    ch |= (cint)c;
    if (ch >= 0xdc00 && ch < 0xe000) {
      ch = 0x10000 + ((ch1 - 0xd800) << 10) + (ch - 0xdc00);
      if (ch < char_code_limit) {
        if (dest==destend) break;
        *dest++ = as_chart(ch);
        src += 12; /* complete \u sequence */
        continue;
      }
    }
    src += 6; /* skip invalid \u sequence */
    continue;
  }
  *srcp = src;
  *destp = dest;
}

/* Characters to bytes. */

global uintL java_wcslen (object encoding, const chart* src,
                          const chart* srcend) {
  var uintL destlen = 0;
  while (src < srcend) {
    var cint ch = as_cint(*src++);
    destlen += (ch < 0x80 ? 1 : ch < 0x10000 ? 6 : 12);
  }
  return destlen;
}

global void java_wcstombs (object encoding, object stream, const chart* *srcp,
                           const chart* srcend, uintB* *destp,
                           uintB* destend) {
  var const chart* src = *srcp;
  var uintB* dest = *destp;
  while (src < srcend) {
    local char const hex_table_lc[] = "0123456789abcdef"; /* lowercase! */
    var cint ch = as_cint(*src);
    var uintL count = (ch < 0x80 ? 1 : ch < 0x10000 ? 6 : 12);
    if (dest+count > destend) break;
    src++;
    if (ch < 0x80) { /* 1 byte sequence */
      *dest++ = ch;
    } else if (ch < 0x10000) { /* 6 byte sequence */
      *dest++ = '\\';
      *dest++ = 'u';
      *dest++ = hex_table_lc[(ch>>12)&0x0F];
      *dest++ = hex_table_lc[(ch>>8)&0x0F];
      *dest++ = hex_table_lc[(ch>>4)&0x0F];
      *dest++ = hex_table_lc[ch&0x0F];
    } else { /* 12 byte sequence */
      var cint ch1 = 0xD800 + ((ch - 0x10000) >> 10);
      var cint ch2 = 0xDC00 + ((ch - 0x10000) & 0x3FF);
      *dest++ = '\\';
      *dest++ = 'u';
      *dest++ = hex_table_lc[(ch1>>12)&0x0F];
      *dest++ = hex_table_lc[(ch1>>8)&0x0F];
      *dest++ = hex_table_lc[(ch1>>4)&0x0F];
      *dest++ = hex_table_lc[ch1&0x0F];
      *dest++ = '\\';
      *dest++ = 'u';
      *dest++ = hex_table_lc[(ch2>>12)&0x0F];
      *dest++ = hex_table_lc[(ch2>>8)&0x0F];
      *dest++ = hex_table_lc[(ch2>>4)&0x0F];
      *dest++ = hex_table_lc[ch2&0x0F];
    }
  }
  *srcp = src;
  *destp = dest;
}

/* -------------------------------------------------------------------------
 * 8-bit NLS characters sets */

/* min. bytes per character = 1
   max. bytes per character = 1 */

typedef struct nls_table_t {
  const char* charset;
  const unsigned char* const* page_uni2charset;  /* UCS-2 to 8-bit table */
  const unsigned short* charset2uni;             /* 8-bit to UCS-2 table */
  int is_ascii_extension;
}
#if defined(HEAPCODES) && (alignment_long < 4) && defined(GNU)
/* Force all XPSEUDODATAs to be allocated with a 4-byte alignment.
   GC needs this. */
  __attribute__ ((aligned (4)))
#endif
       nls_table_t;

static const unsigned char nopage[256] = {
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x00-0x07 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x08-0x0f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x10-0x17 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x18-0x1f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x20-0x27 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x28-0x2f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x30-0x37 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x38-0x3f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x40-0x47 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x48-0x4f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x50-0x57 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x58-0x5f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x60-0x67 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x68-0x6f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x70-0x77 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x78-0x7f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x80-0x87 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x88-0x8f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x90-0x97 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0x98-0x9f */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xa0-0xa7 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xa8-0xaf */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xb0-0xb7 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xb8-0xbf */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xc0-0xc7 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xc8-0xcf */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xd0-0xd7 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xd8-0xdf */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xe0-0xe7 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xe8-0xef */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* 0xf0-0xf7 */
  0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00  /* 0xf8-0xff */
};

#include "nls_ascii.c"
#include "nls_iso8859_1.c"
#include "nls_iso8859_2.c"
#include "nls_iso8859_3.c"
#include "nls_iso8859_4.c"
#include "nls_iso8859_5.c"
#include "nls_iso8859_6.c"
#include "nls_iso8859_7.c"
#include "nls_iso8859_8.c"
#include "nls_iso8859_9.c"
#include "nls_iso8859_10.c"
#include "nls_iso8859_13.c"
#include "nls_iso8859_14.c"
#include "nls_iso8859_15.c"
#include "nls_iso8859_16.c"
#include "nls_koi8_r.c"
#include "nls_koi8_u.c"
#include "nls_mac_arabic.c"
#include "nls_mac_centraleurope.c"
#include "nls_mac_croatian.c"
#include "nls_mac_cyrillic.c"
#include "nls_mac_dingbat.c"
#include "nls_mac_greek.c"
#include "nls_mac_hebrew.c"
#include "nls_mac_iceland.c"
#include "nls_mac_roman.c"
#include "nls_mac_romania.c"
#include "nls_mac_symbol.c"
#include "nls_mac_thai.c"
#include "nls_mac_turkish.c"
#include "nls_mac_ukraine.c"
#include "nls_cp437_ms.c"
#include "nls_cp437_ibm.c"
#include "nls_cp737.c"
#include "nls_cp775.c"
#include "nls_cp850.c"
#include "nls_cp852_ms.c"
#include "nls_cp852_ibm.c"
#include "nls_cp855.c"
#include "nls_cp857.c"
#include "nls_cp860_ms.c"
#include "nls_cp860_ibm.c"
#include "nls_cp861_ms.c"
#include "nls_cp861_ibm.c"
#include "nls_cp862_ms.c"
#include "nls_cp862_ibm.c"
#include "nls_cp863_ms.c"
#include "nls_cp863_ibm.c"
#include "nls_cp864_ms.c"
#include "nls_cp864_ibm.c"
#include "nls_cp865_ms.c"
#include "nls_cp865_ibm.c"
#include "nls_cp866.c"
#include "nls_cp869_ms.c"
#include "nls_cp869_ibm.c"
#include "nls_cp874_ms.c"
#include "nls_cp874_ibm.c"
#include "nls_cp1250.c"
#include "nls_cp1251.c"
#include "nls_cp1252.c"
#include "nls_cp1253.c"
#include "nls_cp1254.c"
#include "nls_cp1256.c"
#include "nls_cp1257.c"
#include "nls_hp_roman8.c"
#include "nls_nextstep.c"
#include "nls_jisx0201.c"

#define nls_first_sym  S(ascii)
#define nls_last_sym  S(jisx0201)
#define nls_num_encodings  (&symbol_tab_data.S_jisx0201 - &symbol_tab_data.S_ascii + 1)

static const nls_table_t * const nls_tables[] = {
  &nls_ascii_table,
  &nls_iso8859_1_table,
  &nls_iso8859_2_table,
  &nls_iso8859_3_table,
  &nls_iso8859_4_table,
  &nls_iso8859_5_table,
  &nls_iso8859_6_table,
  &nls_iso8859_7_table,
  &nls_iso8859_8_table,
  &nls_iso8859_9_table,
  &nls_iso8859_10_table,
  &nls_iso8859_13_table,
  &nls_iso8859_14_table,
  &nls_iso8859_15_table,
  &nls_iso8859_16_table,
  &nls_koi8_r_table,
  &nls_koi8_u_table,
  &nls_mac_arabic_table,
  &nls_mac_centraleurope_table,
  &nls_mac_croatian_table,
  &nls_mac_cyrillic_table,
  &nls_mac_dingbat_table,
  &nls_mac_greek_table,
  &nls_mac_hebrew_table,
  &nls_mac_iceland_table,
  &nls_mac_roman_table,
  &nls_mac_romania_table,
  &nls_mac_symbol_table,
  &nls_mac_thai_table,
  &nls_mac_turkish_table,
  &nls_mac_ukraine_table,
  &nls_cp437_ms_table,
  &nls_cp437_ibm_table,
  &nls_cp737_table,
  &nls_cp775_table,
  &nls_cp850_table,
  &nls_cp852_ms_table,
  &nls_cp852_ibm_table,
  &nls_cp855_table,
  &nls_cp857_table,
  &nls_cp860_ms_table,
  &nls_cp860_ibm_table,
  &nls_cp861_ms_table,
  &nls_cp861_ibm_table,
  &nls_cp862_ms_table,
  &nls_cp862_ibm_table,
  &nls_cp863_ms_table,
  &nls_cp863_ibm_table,
  &nls_cp864_ms_table,
  &nls_cp864_ibm_table,
  &nls_cp865_ms_table,
  &nls_cp865_ibm_table,
  &nls_cp866_table,
  &nls_cp869_ms_table,
  &nls_cp869_ibm_table,
  &nls_cp874_ms_table,
  &nls_cp874_ibm_table,
  &nls_cp1250_table,
  &nls_cp1251_table,
  &nls_cp1252_table,
  &nls_cp1253_table,
  &nls_cp1254_table,
  &nls_cp1256_table,
  &nls_cp1257_table,
  &nls_hp_roman8_table,
  &nls_nextstep_table,
  &nls_jisx0201_table,
};

global uintL nls_mblen (object encoding, const uintB* src,
                        const uintB* srcend);
global void nls_mbstowcs (object encoding, object stream, const uintB* *srcp,
                          const uintB* srcend, chart* *destp, chart* destend);
global uintL nls_asciiext_mblen (object encoding, const uintB* src,
                                 const uintB* srcend);
global void nls_asciiext_mbstowcs (object encoding, object stream,
                                   const uintB* *srcp, const uintB* srcend,
                                   chart* *destp, chart* destend);
global uintL nls_wcslen (object encoding, const chart* src,
                         const chart* srcend);
global void nls_wcstombs (object encoding, object stream, const chart* *srcp,
                          const chart* srcend, uintB* *destp, uintB* destend);
global uintL nls_asciiext_wcslen (object encoding, const chart* src,
                                  const chart* srcend);
global void nls_asciiext_wcstombs (object encoding, object stream,
                                   const chart* *srcp, const chart* srcend,
                                   uintB* *destp, uintB* destend);
global object nls_range (object encoding, uintL start, uintL end, uintL maxintervals);

/* Bytes to characters. */

/* Error when an invalid byte was encountered.
 fehler_nls_invalid(encoding,b); */
nonreturning_function(local, fehler_nls_invalid, (object encoding, uintB b)) {
  pushSTACK(TheEncoding(encoding)->enc_charset);
  pushSTACK(ascii_char(hex_table[b&0x0F]));
  pushSTACK(ascii_char(hex_table[(b>>4)&0x0F]));
  fehler(error,GETTEXT("invalid byte #x~C~C in ~S conversion"));
}

global uintL nls_mblen (object encoding, const uintB* src,
                        const uintB* srcend) {
  if (!eq(TheEncoding(encoding)->enc_towcs_error,S(Kignore)))
    return (srcend-src);
  else {
    var uintL count = srcend-src;
    var uintL result = 0;
    if (count > 0) {
      var const nls_table_t* table =
        (const nls_table_t*) TheMachine(TheEncoding(encoding)->enc_table);
      var const unsigned short* cvtable = table->charset2uni;
      dotimespL(count,count, {
        if (!(cvtable[*src++] == 0xFFFD))
          result++;
      });
    }
    return result;
  }
}

global void nls_mbstowcs (object encoding, object stream, const uintB* *srcp,
                          const uintB* srcend, chart* *destp, chart* destend) {
  var const uintB* src = *srcp;
  var chart* dest = *destp;
  var uintL count = destend-dest;
  if (count > srcend-src)
    count = srcend-src;
  if (count > 0) {
    var const nls_table_t* table =
      (const nls_table_t*) TheMachine(TheEncoding(encoding)->enc_table);
    var const unsigned short* cvtable = table->charset2uni;
    dotimespL(count,count, {
      var uintB b = *src++;
      var cint ch = cvtable[b];
      if (!(ch == 0xFFFD)) {
        *dest++ = as_chart(ch);
      } else {
        var object action = TheEncoding(encoding)->enc_towcs_error;
        if (eq(action,S(Kignore))) {
        } else if (eq(action,S(Kerror))) {
          fehler_nls_invalid(encoding,b);
        } else {
          *dest++ = char_code(action);
        }
      }
    });
    *srcp = src;
    *destp = dest;
  }
}

/* Same thing, specially optimized for ASCII extensions. */

global uintL nls_asciiext_mblen (object encoding, const uintB* src,
                                 const uintB* srcend) {
  if (!eq(TheEncoding(encoding)->enc_towcs_error,S(Kignore)))
    return (srcend-src);
  else {
    var uintL count = srcend-src;
    var uintL result = 0;
    if (count > 0) {
      var const nls_table_t* table =
        (const nls_table_t*) TheMachine(TheEncoding(encoding)->enc_table);
      var const unsigned short* cvtable = table->charset2uni;
      dotimespL(count,count, {
        var uintB b = *src++;
        if ((b < 0x80) || !(cvtable[b] == 0xFFFD))
          result++;
      });
    }
    return result;
  }
}

global void nls_asciiext_mbstowcs (object encoding, object stream,
                                   const uintB* *srcp, const uintB* srcend,
                                   chart* *destp, chart* destend) {
  var const uintB* src = *srcp;
  var chart* dest = *destp;
  var uintL count = destend-dest;
  if (count > srcend-src)
    count = srcend-src;
  if (count > 0) {
    var const nls_table_t* table =
      (const nls_table_t*) TheMachine(TheEncoding(encoding)->enc_table);
    var const unsigned short* cvtable = table->charset2uni;
    dotimespL(count,count, {
      var uintB b = *src++;
      if (b < 0x80) { /* avoid memory reference (big speedup!) */
        *dest++ = as_chart((cint)b);
      } else {
        var cint ch = cvtable[b];
        if (!(ch == 0xFFFD)) {
          *dest++ = as_chart(ch);
        } else {
          var object action = TheEncoding(encoding)->enc_towcs_error;
          if (eq(action,S(Kignore))) {
          } else if (eq(action,S(Kerror))) {
            fehler_nls_invalid(encoding,b);
          } else {
            *dest++ = char_code(action);
          }
        }
      }
    });
    *srcp = src;
    *destp = dest;
  }
}

/* Characters to bytes. */

global uintL nls_wcslen (object encoding, const chart* src,
                         const chart* srcend) {
  var uintL count = srcend-src;
  var uintL result = 0;
  if (count > 0) {
    var const nls_table_t* table = (const nls_table_t*)
      TheMachine(TheEncoding(encoding)->enc_table);
    var const unsigned char* const* cvtable = table->page_uni2charset;
    dotimespL(count,count, {
      var chart ch = *src++;
      if (as_cint(ch) < 0x10000
          && (cvtable[as_cint(ch)>>8][as_cint(ch)&0xFF] != 0
              || chareq(ch,ascii(0))))
        result++;
      else {
        var object action = TheEncoding(encoding)->enc_tombs_error;
        if (eq(action,S(Kignore))) {
        } else if (uint8_p(action)) {
          result++;
        } else if (!eq(action,S(Kerror))) {
          var chart c = char_code(action);
          if (as_cint(c) < 0x10000
              && (cvtable[as_cint(c)>>8][as_cint(c)&0xFF] != 0
                  || chareq(c,ascii(0))))
            result++;
        } else
          fehler_unencodable(encoding,ch);
      }
     });
  }
  return result;
}

global void nls_wcstombs (object encoding, object stream,
                          const chart* *srcp, const chart* srcend,
                          uintB* *destp, uintB* destend) {
  var const chart* src = *srcp;
  var uintB* dest = *destp;
  var uintL scount = srcend-src;
  var uintL dcount = destend-dest;
  if (scount > 0 && dcount > 0) {
    var const nls_table_t* table = (const nls_table_t*)
      TheMachine(TheEncoding(encoding)->enc_table);
    var const unsigned char* const* cvtable = table->page_uni2charset;
    do {
      var chart ch = *src++; scount--;
      var uintB b;
      if (as_cint(ch) < 0x10000
          && (b = cvtable[as_cint(ch)>>8][as_cint(ch)&0xFF],
              b != 0 || chareq(ch,ascii(0)))) {
        *dest++ = b; dcount--;
      } else {
        var object action = TheEncoding(encoding)->enc_tombs_error;
        if (eq(action,S(Kignore))) {
        } else if (uint8_p(action)) {
          *dest++ = I_to_uint8(action); dcount--;
        } else if (!eq(action,S(Kerror))) {
          var chart c = char_code(action);
          if (as_cint(c) < 0x10000
              && (b = cvtable[as_cint(c)>>8][as_cint(c)&0xFF],
                  b != 0 || chareq(c,ascii(0)))) {
            *dest++ = b; dcount--;
          } else
            fehler_unencodable(encoding,ch);
        } else
          fehler_unencodable(encoding,ch);
      }
    } while (scount > 0 && dcount > 0);
    *srcp = src;
    *destp = dest;
  }
}

/* Same thing, specially optimized for ASCII extensions. */

global uintL nls_asciiext_wcslen (object encoding, const chart* src,
                                  const chart* srcend) {
  var uintL count = srcend-src;
  var uintL result = 0;
  if (count > 0) {
    var const nls_table_t* table = (const nls_table_t*)
      TheMachine(TheEncoding(encoding)->enc_table);
    var const unsigned char* const* cvtable = table->page_uni2charset;
    dotimespL(count,count, {
      var chart ch = *src++;
      if (as_cint(ch) < 0x80
          || (as_cint(ch) < 0x10000
              && cvtable[as_cint(ch)>>8][as_cint(ch)&0xFF] != 0))
        result++;
      else {
        var object action = TheEncoding(encoding)->enc_tombs_error;
        if (eq(action,S(Kignore))) {
        } else if (uint8_p(action)) {
          result++;
        } else if (!eq(action,S(Kerror))) {
          var chart c = char_code(action);
          if (as_cint(c) < 0x10000
              && (cvtable[as_cint(c)>>8][as_cint(c)&0xFF] != 0
                  || chareq(c,ascii(0))))
            result++;
        } else
          fehler_unencodable(encoding,ch);
      }
    });
  }
  return result;
}

global void nls_asciiext_wcstombs (object encoding, object stream,
                                   const chart* *srcp, const chart* srcend,
                                   uintB* *destp, uintB* destend) {
  var const chart* src = *srcp;
  var uintB* dest = *destp;
  var uintL scount = srcend-src;
  var uintL dcount = destend-dest;
  if (scount > 0 && dcount > 0) {
    var const nls_table_t* table = (const nls_table_t*)
      TheMachine(TheEncoding(encoding)->enc_table);
    var const unsigned char* const* cvtable = table->page_uni2charset;
    do {
      var chart ch = *src++; scount--;
      if (as_cint(ch) < 0x80) { /* avoid memory reference (big speedup!) */
        *dest++ = (uintB)as_cint(ch);
        dcount--;
      } else {
        var uintB b;
        if (as_cint(ch) < 0x10000
            && (b = cvtable[as_cint(ch)>>8][as_cint(ch)&0xFF],
                b != 0)) {
          *dest++ = b; dcount--;
        } else {
          var object action = TheEncoding(encoding)->enc_tombs_error;
          if (eq(action,S(Kignore))) {
          } else if (uint8_p(action)) {
            *dest++ = I_to_uint8(action); dcount--;
          } else if (!eq(action,S(Kerror))) {
            var chart c = char_code(action);
            if (as_cint(c) < 0x10000
                && (b = cvtable[as_cint(c)>>8][as_cint(c)&0xFF],
                    b != 0 || chareq(c,ascii(0)))) {
              *dest++ = b; dcount--;
            } else
              fehler_unencodable(encoding,ch);
          } else
            fehler_unencodable(encoding,ch);
        }
      }
    } while (scount > 0 && dcount > 0);
    *srcp = src;
    *destp = dest;
  }
}

/* Determining the range of encodable characters. */
global object nls_range (object encoding, uintL start, uintL end,
                         uintL maxintervals) {
  var uintL count = 0; /* number of intervals already on the STACK */
  /* The range lies in the BMP; no need to look beyond U+FFFF. */
  if (maxintervals > 0 && start < 0x10000) {
    if (end >= 0x10000)
      end = 0xFFFF;
    var const nls_table_t* table =
      (const nls_table_t*) TheMachine(TheEncoding(encoding)->enc_table);
    var const unsigned char* const* cvtable = table->page_uni2charset;
    var uintL i1;
    var uintL i2;
    var bool have_i1_i2 = false; /* [i1,i2] = interval being built */
    var uintL i;
    for (i = start;;) {
      /* Here i < 0x10000 and count < maxintervals. */
      var chart ch = as_chart(i);
      if (cvtable[as_cint(ch)>>8][as_cint(ch)&0xFF] != 0
          || chareq(ch,ascii(0))) {
        /* ch encodable -> extend the interval */
        if (!have_i1_i2) {
          have_i1_i2 = true; i1 = i;
        }
        i2 = i;
      } else {
        /* ch not encodable -> finish the interval */
        if (have_i1_i2) {
          pushSTACK(code_char(as_chart(i1)));
          pushSTACK(code_char(as_chart(i2)));
          check_STACK(); count++;
          have_i1_i2 = false;
          /* If we have already produced the maximum number of intervals
             requested by the caller, it's of no use to search further. */
          if (count == maxintervals)
            break;
        }
      }
      if (i == end)
        break;
      i++;
    }
    if (have_i1_i2) {
      pushSTACK(code_char(as_chart(i1))); pushSTACK(code_char(as_chart(i2)));
      check_STACK(); count++;
    }
  }
  return stringof(2*count);
}

/* -------------------------------------------------------------------------
 * iconv-based encodings */

/* They are defined in stream.d because they need to access internals of
   the ChannelStream. */

#ifdef HAVE_GOOD_ICONV

extern uintL iconv_mblen (object encoding, const uintB* src,
                          const uintB* srcend);
extern void iconv_mbstowcs (object encoding, object stream, const uintB* *srcp,
                            const uintB* srcend, chart* *destp,
                            chart* destend);
extern uintL iconv_wcslen (object encoding, const chart* src,
                           const chart* srcend);
extern void iconv_wcstombs (object encoding, object stream, const chart* *srcp,
                            const chart* srcend, uintB* *destp,
                            uintB* destend);
extern object iconv_range (object encoding, uintL start, uintL end,
                           uintL maxintervals);

#endif /* HAVE_GOOD_ICONV */

#if defined(GNU_LIBICONV) || (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 2))

#ifdef GNU_LIBICONV
#define iconv_first_sym  S(koi8_ru)
#define iconv_last_sym  S(utf_7)
#define iconv_num_encodings  (&symbol_tab_data.S_utf_7 - &symbol_tab_data.S_koi8_ru + 1)
#else
#define iconv_first_sym  S(cp1255)
#define iconv_last_sym  S(utf_7)
#define iconv_num_encodings  (&symbol_tab_data.S_utf_7 - &symbol_tab_data.S_cp1255 + 1)
#endif

#endif

/* ----------------------------------------------------------------------- */

#endif /* UNICODE */

/* =========================================================================
 * General functions */

/* (MAKE-ENCODING [:charset] [:line-terminator] [:input-error-action]
                  [:output-error-action] [:if-does-not-exist])
 creates a new encoding. */
LISPFUN(make_encoding,seclass_read,0,0,norest,key,5,
        (kw(charset),kw(line_terminator),
         kw(input_error_action),kw(output_error_action)
         kw(if_does_not_exist))) {
  var object arg = popSTACK(); /* :if-does-not-exist */
  var bool ignore_not_exist = nullp(arg); /* no error */
  /* Check the :CHARSET argument. */
  arg = STACK_3;
  /* string -> symbol in CHARSET */
  if (!boundp(arg) || eq(arg,S(Kdefault))) {
    arg = O(default_file_encoding);
   #ifndef UNICODE
    if (nullp(arg)) /* initialization */
      goto create_new_encoding;
   #endif
  } else if (encodingp(arg)) {
  }
 #ifdef UNICODE
  else if (symbolp(arg) && constant_var_p(TheSymbol(arg))
           && encodingp(Symbol_value(arg))) {
    arg = Symbol_value(arg);
  } else if (stringp(arg)) {
    var object arg_upcase = string_upcase(arg);
    var object sym;
    arg = STACK_3; /* refetch */
    if (find_external_symbol(arg_upcase,false,O(charset_package),&sym)
        && constant_var_p(TheSymbol(sym)) && encodingp(Symbol_value(sym)))
      arg = Symbol_value(sym);
    #ifdef HAVE_GOOD_ICONV
    else {
      var bool valid_encoding_p = true;
      with_string_0(arg,Symbol_value(S(ascii)),charset_ascii,{
        valid_encoding_p =
          check_charset(charset_ascii,ignore_not_exist ? nullobj : arg);
      });
      /* if :IF-DOES-NOT-EXIST was non-NIL and the encoding was invalid,
         check_charset() would have signalled an error */
      if (valid_encoding_p) {
        pushSTACK(coerce_ss(arg));
        var object encoding = allocate_encoding();
        TheEncoding(encoding)->enc_eol = S(Kunix);
        TheEncoding(encoding)->enc_towcs_error = S(Kerror);
        TheEncoding(encoding)->enc_tombs_error = S(Kerror);
        TheEncoding(encoding)->enc_charset = popSTACK();
        TheEncoding(encoding)->enc_mblen    = P(iconv_mblen);
        TheEncoding(encoding)->enc_mbstowcs = P(iconv_mbstowcs);
        TheEncoding(encoding)->enc_wcslen   = P(iconv_wcslen);
        TheEncoding(encoding)->enc_wcstombs = P(iconv_wcstombs);
        TheEncoding(encoding)->enc_range    = P(iconv_range);
        TheEncoding(encoding)->min_bytes_per_char = 1;
        TheEncoding(encoding)->max_bytes_per_char = max_bytes_per_chart; /* wild assumption */
        arg = encoding;
      } else {
        ASSERT(ignore_not_exist);
        arg = NIL;
      }
    }
    #else
    else
      goto bad_arg;
    #endif
  }
 #endif
  else {
   bad_arg:
    pushSTACK(arg); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(encoding)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(arg); pushSTACK(S(Kcharset)); pushSTACK(S(make_encoding));
    fehler(type_error,GETTEXT("~S: illegal ~S argument ~S"));
  }
  STACK_3 = arg;
  /* Check the :LINE-TERMINATOR argument. */
  arg = STACK_2;
  if (!(!boundp(arg)
        || eq(arg,S(Kunix)) || eq(arg,S(Kmac)) || eq(arg,S(Kdos)))) {
    pushSTACK(arg); /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_line_terminator)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(arg); pushSTACK(S(Kline_terminator));
    pushSTACK(S(make_encoding));
    fehler(type_error,GETTEXT("~S: illegal ~S argument ~S"));
  }
  /* Check the :INPUT-ERROR-ACTION argument. */
  arg = STACK_1;
  if (!(!boundp(arg)
        || eq(arg,S(Kerror)) || eq(arg,S(Kignore)) || charp(arg))) {
    pushSTACK(arg); /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_input_error_action)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(arg); pushSTACK(S(Kinput_error_action));
    pushSTACK(S(make_encoding));
    fehler(type_error,GETTEXT("~S: illegal ~S argument ~S"));
  }
  /* Check the :OUTPUT-ERROR-ACTION argument. */
  arg = STACK_0;
  if (!(!boundp(arg)
        || eq(arg,S(Kerror)) || eq(arg,S(Kignore))
        || charp(arg) || uint8_p(arg))) {
    pushSTACK(arg); /* TYPE-ERROR slot DATUM */
    pushSTACK(O(type_output_error_action)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(arg); pushSTACK(S(Koutput_error_action));
    pushSTACK(S(make_encoding));
    fehler(type_error,GETTEXT("~S: illegal ~S argument ~S"));
  }
  /* Create a new encoding. */
  if (nullp(STACK_3) /* illegal charset & :IF-DOES-NOT-EXIST NIL */
      || ((!boundp(STACK_2) || eq(STACK_2,TheEncoding(STACK_3)->enc_eol))
          && (!boundp(STACK_1)
              || eq(STACK_1,TheEncoding(STACK_3)->enc_towcs_error))
          && (!boundp(STACK_0)
              || eq(STACK_0,TheEncoding(STACK_3)->enc_tombs_error)))) {
    VALUES1(STACK_3);
  } else create_new_encoding: {
    var object encoding = allocate_encoding();
    var object old_encoding = STACK_3;
    if (encodingp(old_encoding)) {
      var const gcv_object_t* ptr1 = &TheRecord(old_encoding)->recdata[0];
      var gcv_object_t* ptr2 = &TheRecord(encoding)->recdata[0];
      var uintC count = encoding_length;
      do { *ptr2++ = *ptr1++; } while (--count);
      if (encoding_xlength > 0)
        copy_mem_b(ptr2,ptr1,encoding_xlength);
    }
    if (boundp(STACK_2))
      TheEncoding(encoding)->enc_eol = STACK_2;
    if (boundp(STACK_1))
      TheEncoding(encoding)->enc_towcs_error = STACK_1;
    if (boundp(STACK_0))
      TheEncoding(encoding)->enc_tombs_error = STACK_0;
    VALUES1(encoding);
  }
  skipSTACK(4);
}

/* (SYSTEM::ENCODINGP object) */
LISPFUNNF(encodingp,1) {
  var object arg = popSTACK();
  VALUES_IF(encodingp(arg));
}

#ifdef UNICODE
#define DEFAULT_ENC &O(misc_encoding)
#else
#define DEFAULT_ENC &O(default_file_encoding)
#endif

/* (SYSTEM::CHARSET-TYPEP object encoding) tests whether the object
   is a character belonging to the given character set. */
LISPFUNNR(charset_typep,2) {
  var object encoding = check_encoding(STACK_0,DEFAULT_ENC,false);
  var object obj = STACK_1;
  if (charp(obj)) {
   #ifdef UNICODE
    var uintL i = as_cint(char_code(obj));
    obj = Encoding_range(encoding)(encoding,i,i,1);
    VALUES_IF(Sstring_length(obj));
   #else
    VALUES1(T);
   #endif
  } else {
    VALUES1(NIL);
  }
  skipSTACK(2);
}

/* (EXT:ENCODING-LINE-TERMINATOR encoding) --> :UNIX/:DOS/:MAC */
LISPFUNNF(encoding_line_terminator,1) {
  var object encoding = check_encoding(popSTACK(),DEFAULT_ENC,false);
  VALUES1(TheEncoding(encoding)->enc_eol);
}

#ifdef UNICODE

/* (EXT:ENCODING-CHARSET encoding) --> charset */
LISPFUNNF(encoding_charset,1) {
  var object encoding = check_encoding(popSTACK(),DEFAULT_ENC,false);
  VALUES1(TheEncoding(encoding)->enc_charset);
}

/* (SYSTEM::CHARSET-RANGE encoding char1 char2 [maxintervals])
 returns the range of characters in [char1,char2] encodable in the encoding. */
LISPFUN(charset_range,seclass_read,3,1,norest,nokey,0,NIL) {
  var object encoding = check_encoding(STACK_3,DEFAULT_ENC,false);
  if (!charp(STACK_2)) STACK_2 = check_char(STACK_2);
  if (!charp(STACK_1)) STACK_1 = check_char(STACK_1);
  var uintL i1 = as_cint(char_code(STACK_2));
  var uintL i2 = as_cint(char_code(STACK_1));
  var uintL maxintervals;
  if (missingp(STACK_0))
    maxintervals = ~(uintL)0;
  else if (uint32_p(STACK_0))
    maxintervals = I_to_uint32(STACK_0);
  else
    fehler_uint32(STACK_0);
  VALUES1(i1 <= i2 ?
          Encoding_range(encoding)(encoding,i1,i2,maxintervals) :
          (object)O(empty_string));
  skipSTACK(4);
}

#endif

/* -------------------------------------------------------------------------
 * Elementary string functions */

/* UP: return a LISP string the given contents.
 n_char_to_string(charptr,len,encoding)
 > char* charptr: the address of the character sequence
 > uintL len: its length
 > object encoding: Encoding
 < return: normal-simple-string with len characters as content
 can trigger GC */
#ifdef UNICODE
global maygc object n_char_to_string (const char* srcptr, uintL blen,
                                      object encoding) {
  var const uintB* bptr = (const uintB*)srcptr;
  var const uintB* bendptr = bptr+blen;
  var uintL clen = Encoding_mblen(encoding)(encoding,bptr,bendptr);
  pushSTACK(encoding);
  check_stringsize(clen);
  var object obj = allocate_string(clen);
  encoding = popSTACK();
  {
    var chart* cptr = &TheSnstring(obj)->data[0];
    var chart* cendptr = cptr+clen;
    Encoding_mbstowcs(encoding)(encoding,nullobj,&bptr,bendptr,&cptr,cendptr);
    ASSERT(cptr == cendptr);
  }
  return obj;
}
#else
global maygc object n_char_to_string_ (const char* srcptr, uintL len) {
  var const uintB* bptr = (const uintB*)srcptr;
  check_stringsize(len);
  var object obj = allocate_string(len);
  if (len > 0) {
    var chart* ptr = &TheSnstring(obj)->data[0];
    /* copy bptr to ptr as characters: */
    dotimespL(len,len, { *ptr++ = as_chart(*bptr++); } );
  }
  return obj;
}
#endif

/* UP: Convert an ASCIZ string to a LISP string
 asciz_to_string(asciz,encoding)
 ascii_to_string(asciz)
 > char* asciz: ASCIZ-String (NULL-terminated)
 > object encoding: Encoding
 < return: normal-simple-string the same string (without NULL)
 can trigger GC */
#ifdef UNICODE
global maygc object asciz_to_string (const char * asciz, object encoding) {
  return n_char_to_string(asciz,asciz_length(asciz),encoding);
}
#else
global maygc object asciz_to_string_ (const char * asciz) {
  return n_char_to_string_(asciz,asciz_length(asciz));
}
#endif
global maygc object ascii_to_string (const char * asciz) {
  var const uintB* bptr = (const uintB*)asciz;
  var uintL len = asciz_length(asciz);
  check_stringsize(len);
  var object obj = allocate_s8string(len); /* allocate string */
  if (len > 0) {
    var cint8* ptr = &TheS8string(obj)->data[0];
    /* copy as characters bptr --> ptr: */
    dotimespL(len,len, {
      var uintB b = *bptr++;
      ASSERT(b < 0x80);
      *ptr++ = (cint8)b;
    });
  }
  DBGREALLOC(obj);
  return obj;
}

/* UP: Convert a LISP string to an ASCIZ string.
 string_to_asciz(obj,encoding)
 > object obj: String
 > object encoding: Encoding
 < return: simple-bit-vector with the bytes<==characters and a NULL at the end
 < TheAsciz(ergebnis): address of the byte sequence contain therein
 can trigger GC */
#ifdef UNICODE
global maygc object string_to_asciz (object obj, object encoding) {
  var uintL len;
  var uintL offset;
  var object string = unpack_string_ro(obj,&len,&offset);
  var const chart* srcptr;
  unpack_sstring_alloca(string,len,offset, srcptr=);
  var uintL bytelen = cslen(encoding,srcptr,len);
  pushSTACK(encoding);
  pushSTACK(string);
  var object newasciz = allocate_bit_vector(Atype_8Bit,bytelen+1);
  string = popSTACK();
  encoding = popSTACK();
  unpack_sstring_alloca(string,len,offset, srcptr=);
  cstombs(encoding,srcptr,len,&TheSbvector(newasciz)->data[0],bytelen);
  TheSbvector(newasciz)->data[bytelen] = '\0';
  return newasciz;
}
#else
global maygc object string_to_asciz_ (object obj) {
  pushSTACK(obj); /* save string */
  var object newasciz = allocate_bit_vector(Atype_8Bit,vector_length(obj)+1);
  obj = popSTACK(); /* restore string */
  {
    var uintL len;
    var uintL offset;
    var object string = unpack_string_ro(obj,&len,&offset);
    var const chart* sourceptr;
    unpack_sstring_alloca(string,len,offset, sourceptr=);
    /* source-string: length in len, bytes at sourceptr */
    var uintB* destptr = &TheSbvector(newasciz)->data[0];
    /* destination-string: bytes at destptr */
    { /* copy loop: */
      var uintL count;
      dotimesL(count,len, { *destptr++ = as_cint(*sourceptr++); } );
      *destptr++ = '\0'; /* append NULL byte */
    }
  }
  return newasciz;
}
#endif

/* =========================================================================
 * Initialization */

/* Initialize the encodings.
 init_encodings(); */
global void init_encodings_1 (void) {
  /* Compile-time checks: */
  ASSERT(sizeof(chart) == sizeof(cint));
 #ifdef UNICODE
  {
    var object symbol = S(base64);
    var object encoding = allocate_encoding();
    TheEncoding(encoding)->enc_eol = S(Kunix);
    TheEncoding(encoding)->enc_towcs_error = S(Kerror);
    TheEncoding(encoding)->enc_tombs_error = S(Kerror);
    TheEncoding(encoding)->enc_charset = symbol;
    TheEncoding(encoding)->enc_mblen    = P(base64_mblen);
    TheEncoding(encoding)->enc_mbstowcs = P(base64_mbstowcs);
    TheEncoding(encoding)->enc_wcslen   = P(base64_wcslen);
    TheEncoding(encoding)->enc_wcstombs = P(base64_wcstombs);
    TheEncoding(encoding)->enc_range    = P(base64_range);
    TheEncoding(encoding)->min_bytes_per_char = 2; /* ?? */
    TheEncoding(encoding)->max_bytes_per_char = 2; /* ?? */
    define_constant(symbol,encoding);
  }
  {
    var object symbol = S(unicode_16_big_endian);
    var object encoding = allocate_encoding();
    TheEncoding(encoding)->enc_eol = S(Kunix);
    TheEncoding(encoding)->enc_towcs_error = S(Kerror);
    TheEncoding(encoding)->enc_tombs_error = S(Kerror);
    TheEncoding(encoding)->enc_charset = symbol;
    TheEncoding(encoding)->enc_mblen    = P(uni16_mblen);
    TheEncoding(encoding)->enc_mbstowcs = P(uni16be_mbstowcs);
    TheEncoding(encoding)->enc_wcslen   = P(uni16_wcslen);
    TheEncoding(encoding)->enc_wcstombs = P(uni16be_wcstombs);
    TheEncoding(encoding)->enc_range    = P(bmp_range);
    TheEncoding(encoding)->min_bytes_per_char = 2;
    TheEncoding(encoding)->max_bytes_per_char = 2;
    define_constant(symbol,encoding);
  }
  {
    var object symbol = S(unicode_16_little_endian);
    var object encoding = allocate_encoding();
    TheEncoding(encoding)->enc_eol = S(Kunix);
    TheEncoding(encoding)->enc_towcs_error = S(Kerror);
    TheEncoding(encoding)->enc_tombs_error = S(Kerror);
    TheEncoding(encoding)->enc_charset = symbol;
    TheEncoding(encoding)->enc_mblen    = P(uni16_mblen);
    TheEncoding(encoding)->enc_mbstowcs = P(uni16le_mbstowcs);
    TheEncoding(encoding)->enc_wcslen   = P(uni16_wcslen);
    TheEncoding(encoding)->enc_wcstombs = P(uni16le_wcstombs);
    TheEncoding(encoding)->enc_range    = P(bmp_range);
    TheEncoding(encoding)->min_bytes_per_char = 2;
    TheEncoding(encoding)->max_bytes_per_char = 2;
    define_constant(symbol,encoding);
  }
  {
    var object symbol = S(unicode_32_big_endian);
    var object encoding = allocate_encoding();
    TheEncoding(encoding)->enc_eol = S(Kunix);
    TheEncoding(encoding)->enc_towcs_error = S(Kerror);
    TheEncoding(encoding)->enc_tombs_error = S(Kerror);
    TheEncoding(encoding)->enc_charset = symbol;
    TheEncoding(encoding)->enc_mblen    = P(uni32be_mblen);
    TheEncoding(encoding)->enc_mbstowcs = P(uni32be_mbstowcs);
    TheEncoding(encoding)->enc_wcslen   = P(uni32_wcslen);
    TheEncoding(encoding)->enc_wcstombs = P(uni32be_wcstombs);
    TheEncoding(encoding)->enc_range    = P(all_range);
    TheEncoding(encoding)->min_bytes_per_char = 4;
    TheEncoding(encoding)->max_bytes_per_char = 4;
    define_constant(symbol,encoding);
  }
  {
    var object symbol = S(unicode_32_little_endian);
    var object encoding = allocate_encoding();
    TheEncoding(encoding)->enc_eol = S(Kunix);
    TheEncoding(encoding)->enc_towcs_error = S(Kerror);
    TheEncoding(encoding)->enc_tombs_error = S(Kerror);
    TheEncoding(encoding)->enc_charset = symbol;
    TheEncoding(encoding)->enc_mblen    = P(uni32le_mblen);
    TheEncoding(encoding)->enc_mbstowcs = P(uni32le_mbstowcs);
    TheEncoding(encoding)->enc_wcslen   = P(uni32_wcslen);
    TheEncoding(encoding)->enc_wcstombs = P(uni32le_wcstombs);
    TheEncoding(encoding)->enc_range    = P(all_range);
    TheEncoding(encoding)->min_bytes_per_char = 4;
    TheEncoding(encoding)->max_bytes_per_char = 4;
    define_constant(symbol,encoding);
  }
  {
    var object symbol = S(utf_8);
    var object encoding = allocate_encoding();
    TheEncoding(encoding)->enc_eol = S(Kunix);
    TheEncoding(encoding)->enc_towcs_error = S(Kerror);
    TheEncoding(encoding)->enc_tombs_error = S(Kerror);
    TheEncoding(encoding)->enc_charset = symbol;
    TheEncoding(encoding)->enc_mblen    = P(utf8_mblen);
    TheEncoding(encoding)->enc_mbstowcs = P(utf8_mbstowcs);
    TheEncoding(encoding)->enc_wcslen   = P(utf8_wcslen);
    TheEncoding(encoding)->enc_wcstombs = P(utf8_wcstombs);
    TheEncoding(encoding)->enc_range    = P(all_range);
    TheEncoding(encoding)->min_bytes_per_char = 1;
    TheEncoding(encoding)->max_bytes_per_char = 4;
    define_constant(symbol,encoding);
  }
  {
    var object symbol = S(java);
    var object encoding = allocate_encoding();
    TheEncoding(encoding)->enc_eol = S(Kunix);
    TheEncoding(encoding)->enc_towcs_error = S(Kerror);
    TheEncoding(encoding)->enc_tombs_error = S(Kerror);
    TheEncoding(encoding)->enc_charset = symbol;
    TheEncoding(encoding)->enc_mblen    = P(java_mblen);
    TheEncoding(encoding)->enc_mbstowcs = P(java_mbstowcs);
    TheEncoding(encoding)->enc_wcslen   = P(java_wcslen);
    TheEncoding(encoding)->enc_wcstombs = P(java_wcstombs);
    TheEncoding(encoding)->enc_range    = P(all_range);
    TheEncoding(encoding)->min_bytes_per_char = 1;
    TheEncoding(encoding)->max_bytes_per_char = 12;
    define_constant(symbol,encoding);
  }
 #endif
}
global void init_encodings_2 (void) {
 #ifdef UNICODE
  {
    var object symbol = nls_first_sym;
    var const nls_table_t * const * ptr = &nls_tables[0];
    var uintC count = sizeof(nls_tables)/sizeof(nls_tables[0]);
    ASSERT(nls_num_encodings == count);
    for (; count > 0; count--) {
      var object encoding = allocate_encoding();
      TheEncoding(encoding)->enc_eol = S(Kunix);
      TheEncoding(encoding)->enc_towcs_error = S(Kerror);
      TheEncoding(encoding)->enc_tombs_error = S(Kerror);
      TheEncoding(encoding)->enc_charset = symbol;
      if ((*ptr)->is_ascii_extension) {
        TheEncoding(encoding)->enc_mblen    = P(nls_asciiext_mblen);
        TheEncoding(encoding)->enc_mbstowcs = P(nls_asciiext_mbstowcs);
        TheEncoding(encoding)->enc_wcslen   = P(nls_asciiext_wcslen);
        TheEncoding(encoding)->enc_wcstombs = P(nls_asciiext_wcstombs);
      } else {
        TheEncoding(encoding)->enc_mblen    = P(nls_mblen);
        TheEncoding(encoding)->enc_mbstowcs = P(nls_mbstowcs);
        TheEncoding(encoding)->enc_wcslen   = P(nls_wcslen);
        TheEncoding(encoding)->enc_wcstombs = P(nls_wcstombs);
      }
      TheEncoding(encoding)->enc_range    = P(nls_range);
      TheEncoding(encoding)->enc_table    = make_machine(*ptr);
      TheEncoding(encoding)->min_bytes_per_char = 1;
      TheEncoding(encoding)->max_bytes_per_char = 1;
      define_constant(symbol,encoding);
      symbol = objectplus(symbol,(soint)sizeof(*TheSymbol(symbol))<<(oint_addr_shift-addr_shift));
      ptr++;
    }
  }
  /* Now some aliases. */
  define_constant(S(unicode_16),Symbol_value(S(unicode_16_big_endian))); /* network byte order = big endian */
  define_constant(S(unicode_32),Symbol_value(S(unicode_32_big_endian))); /* network byte order = big endian */
  define_constant(S(ucs_2),Symbol_value(S(unicode_16)));
  define_constant(S(ucs_4),Symbol_value(S(unicode_32)));
  define_constant(S(macintosh),Symbol_value(S(mac_roman)));
  define_constant(S(windows_1250),Symbol_value(S(cp1250)));
  define_constant(S(windows_1251),Symbol_value(S(cp1251)));
  define_constant(S(windows_1252),Symbol_value(S(cp1252)));
  define_constant(S(windows_1253),Symbol_value(S(cp1253)));
  define_constant(S(windows_1254),Symbol_value(S(cp1254)));
  define_constant(S(windows_1256),Symbol_value(S(cp1256)));
  define_constant(S(windows_1257),Symbol_value(S(cp1257)));
 #if defined(GNU_LIBICONV) || (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 2))
  {
    var object symbol = iconv_first_sym;
    var uintC count = iconv_num_encodings;
    for (; count > 0; count--) {
      pushSTACK(Symbol_name(symbol)); pushSTACK(unbound);
      pushSTACK(unbound); pushSTACK(unbound); pushSTACK(NIL);
      C_make_encoding(); /* cannot use funcall yet */
      if (nullp(value1)) {
        pushSTACK(symbol); pushSTACK(O(charset_package)); C_unintern();
      } else define_constant(symbol,value1);
      symbol = objectplus(symbol,(soint)sizeof(*TheSymbol(symbol))<<(oint_addr_shift-addr_shift));
    }
  }
  /* Now some aliases, to be defined only if their targets still exit. */
  if (!boundp(Symbol_value(S(cp1255)))) {
    pushSTACK(S(windows_1255)); pushSTACK(O(charset_package)); C_unintern();
  } else define_constant(S(windows_1255),Symbol_value(S(cp1255)));
  if (!boundp(Symbol_value(S(cp1258)))) {
    pushSTACK(S(windows_1258)); pushSTACK(O(charset_package)); C_unintern();
  } else define_constant(S(windows_1258),Symbol_value(S(cp1258)));
 #endif
  /* Initialize O(internal_encoding): */
  pushSTACK(Symbol_value(S(utf_8))); /* :charset */
  pushSTACK(S(Kunix));          /* :line-terminator */
  pushSTACK(unbound);           /* :input-error-action */
  pushSTACK(unbound);           /* :output-error-action */
  pushSTACK(unbound);           /* :if-does-not-exist */
  C_make_encoding();
  O(internal_encoding) = value1;
 #endif
  /* Initialize locale dependent encodings: */
  init_dependent_encodings();
}

#ifdef UNICODE
/* convert the encoding name to its canonical form
 at this time, just upper-case the encoding name
 in the future, might insert/delete `-' &c
 (do not use toupper() because we know that encoding name is ASCII) */
local char *canonicalize_encoding (char *encoding) {
  var char* p;
  for (p = encoding; *p != '\0'; p++)
    if (*p >= 'a' && *p <= 'z')
      *p += 'A' - 'a';
  return encoding;
}
#endif

/* The reasonable 1:1 default.
 Rationale: this avoids random encoding errors,
   e.g., when DIRECTORY is called on startup and the use home dir
   contains files with non-ASCII names
 The right default encoding is the one defined by the CLISP
   CODE-CHAR/CHAR-CODE conversion on the first 255 bytes, i.e., ISO-8859-1 */
#define DEFAULT_1_1_ENCODING  Symbol_value(S(iso8859_1))
#define DEFAULT_1_1_ENCODING_NAME "ISO-8859-1"

/* Returns an encoding specified by a name.
 The line-termination is OS dependent.
 encoding_from_name(name,context)
 > char* name: Any of the canonical names returned by the locale_charset()
               function.
 > char* context: for warnings
 > STACK_0 : if context /= locale and name does not make an encoding
             - the default locale encoding
 can trigger GC */
local maygc object encoding_from_name (const char* name, const char* context) {
 #ifdef UNICODE
  /* Attempt to use the character set implicitly specified by the locale. */
  name = canonicalize_encoding((char*)name); /* FIXME: dangerous cast */
  if (asciz_equal(name,"US-ASCII") || asciz_equal(name,"ANSI_X3.4-1968"))
    pushSTACK(Symbol_value(S(ascii)));
  #if defined(GNU_LIBICONV) || (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 2))
  else if (asciz_equal(name,"GB2312"))
    pushSTACK(Symbol_value(S(euc_cn)));
  else if (asciz_equal(name,"SJIS"))
    pushSTACK(Symbol_value(S(shift_jis)));
  #endif
  else if (asciz_equal(name,"1:1") || asciz_equal(name,"8BIT"))
    pushSTACK(DEFAULT_1_1_ENCODING);
  else {
    pushSTACK(asciz_to_string(name,Symbol_value(S(ascii))));
    pushSTACK(O(charset_package));
    C_find_symbol();
    if (!nullp(value2) && encodingp(Symbol_value(value1)))
      pushSTACK(Symbol_value(value1));
    else if (asciz_equal(context,"locale")) { /* e.g., name=ISO8859-1 */
      fprintf(stderr,GETTEXT("WARNING: %s: no encoding %s, using %s"),
              context,name,DEFAULT_1_1_ENCODING_NAME);
      fputs("\n",stderr);
      pushSTACK(DEFAULT_1_1_ENCODING);
    } else  {
      fprintf(stderr,GETTEXT("WARNING: %s: no encoding %s, using %s"),
              context,name,"locale encoding");
      fputs("\n",stderr);
      pushSTACK(STACK_0);
    }
  }
 #else
  unused name; unused context;
  pushSTACK(unbound);           /* :charset */
 #endif /* UNICODE */
 #if defined(WIN32) || (defined(UNIX) && (O_BINARY != 0))
  pushSTACK(S(Kdos));           /* :line-terminator */
 #else
  pushSTACK(S(Kunix));          /* :line-terminator */
 #endif
  pushSTACK(unbound);           /* :input-error-action */
  pushSTACK(unbound);           /* :output-error-action */
  pushSTACK(unbound);           /* :if-does-not-exist */
  C_make_encoding();
  return value1;
}

/* Initialize the encodings which depend on environment variables.
 init_dependent_encodings(); */
global void init_dependent_encodings(void) {
#ifdef UNICODE
  extern const char* locale_encoding; /* GNU locale encoding canonical name */
  extern const char* argv_encoding_file; /* override *DEFAULT-FILE-ENCODING* */
  extern const char* argv_encoding_pathname; /* override *PATHNAME-ENCODING* */
  extern const char* argv_encoding_terminal; /* override *terminal-encoding* */
  extern const char* argv_encoding_foreign; /* override *foreign-encoding* */
  extern const char* argv_encoding_misc; /* override *misc-encoding* */
  begin_system_call();
  locale_encoding = locale_charset(); /* depends on environment variables */
  end_system_call();
  pushSTACK(encoding_from_name(locale_encoding,"locale"));
  /* Initialize each encoding as follows: If the corresponding -E....
     option was not given, use the locale dependent locale_charset().
     If it was given, use that, and if the specified encoding was invalid,
     use a default encoding that does not depend on the locale. */
  O(default_file_encoding) =
    (argv_encoding_file == NULL ? (object)STACK_0
     : encoding_from_name(argv_encoding_file,"*DEFAULT-FILE-ENCODING*"));
  O(pathname_encoding) =
    (argv_encoding_pathname == NULL ? (object)STACK_0
     : encoding_from_name(argv_encoding_pathname,"*PATHNAME-ENCODING*"));
 #if defined(WIN32_NATIVE)
  /* cf libiconv/libcharset/lib/localcharset.c locale_charset() */
  if (argv_encoding_terminal == NULL) {
    var char buf[2+10+1];
    sprintf(buf,"CP%u",GetOEMCP());
    O(terminal_encoding) = encoding_from_name(buf,"*TERMINAL-ENCODING*");
  } else
    O(terminal_encoding) =
      encoding_from_name(argv_encoding_terminal,"*TERMINAL-ENCODING*");
 #else
  O(terminal_encoding) =
    (argv_encoding_terminal == NULL ? (object)STACK_0
     : encoding_from_name(argv_encoding_terminal,"*TERMINAL-ENCODING*"));
 #endif
 #if defined(HAVE_FFI) || defined(HAVE_AFFI)
  O(foreign_encoding) =
    (argv_encoding_foreign == NULL ? (object)STACK_0
     : encoding_from_name(argv_encoding_foreign,"*FOREIGN-ENCODING*"));
  O(foreign_8bit_encoding) =
    (TheEncoding(O(foreign_encoding))->max_bytes_per_char == 1
     ? O(foreign_encoding)
     /* not DEFAULT_1_1_ENCODING because foreign_8bit_encoding must agree
        with foreign_encoding and only ASCII agrees with _all_ encodings */
     : Symbol_value(S(ascii)));
 #endif
  O(misc_encoding) =
    (argv_encoding_misc == NULL ? (object)STACK_0
     : encoding_from_name(argv_encoding_misc,"*MISC-ENCODING*"));
  skipSTACK(1);
#else /* no UNICODE */
  O(default_file_encoding) = encoding_from_name(NULL,NULL);
  O(terminal_encoding) = encoding_from_name(NULL,NULL);
#endif
}

/* =========================================================================
 * Accessors */

/* (SYSTEM::DEFAULT-FILE-ENCODING) */
LISPFUNNR(default_file_encoding,0) {
  VALUES1(O(default_file_encoding));
}

/* (SYSTEM::SET-DEFAULT-FILE-ENCODING encoding) */
LISPFUNN(set_default_file_encoding,1) {
  var object encoding =
    check_encoding(popSTACK(),&O(default_file_encoding),false);
  VALUES1(O(default_file_encoding) = encoding);
}

#ifdef UNICODE

/* (SYSTEM::PATHNAME-ENCODING) */
LISPFUNNR(pathname_encoding,0) {
  VALUES1(O(pathname_encoding));
}

/* (SYSTEM::SET-PATHNAME-ENCODING encoding) */
LISPFUNN(set_pathname_encoding,1) {
  var object encoding = check_encoding(popSTACK(),&O(pathname_encoding),false);
  VALUES1(O(pathname_encoding) = encoding);
}

/* (SYSTEM::TERMINAL-ENCODING) */
LISPFUNNR(terminal_encoding,0) {
  VALUES1(O(terminal_encoding));
}

/* (SYSTEM::SET-TERMINAL-ENCODING encoding) */
LISPFUNN(set_terminal_encoding,1) {
  var object encoding = check_encoding(STACK_0,&O(terminal_encoding),false);
  /* Ensure O(terminal_encoding) = (STREAM-EXTERNAL-FORMAT *TERMINAL-IO*).
     But first modify (STREAM-EXTERNAL-FORMAT *TERMINAL-IO*): */
  set_terminalstream_external_format(var_stream(S(terminal_io),0),encoding);
  VALUES1(O(terminal_encoding) = popSTACK());
}

#if defined(HAVE_FFI) || defined(HAVE_AFFI)

/* (SYSTEM::FOREIGN-ENCODING) */
LISPFUNNR(foreign_encoding,0) {
  VALUES1(O(foreign_encoding));
}

/* (SYSTEM::SET-FOREIGN-ENCODING encoding) */
LISPFUNN(set_foreign_encoding,1) {
  var object encoding = check_encoding(popSTACK(),&O(foreign_encoding),false);
  O(foreign_encoding) = encoding;
  O(foreign_8bit_encoding) =
    (TheEncoding(O(foreign_encoding))->max_bytes_per_char == 1
     ? O(foreign_encoding)
     : Symbol_value(S(ascii)));
  VALUES1(encoding);
}

#endif /* HAVE_FFI || HAVE_AFFI */

/* (SYSTEM::MISC-ENCODING) */
LISPFUNNR(misc_encoding,0) {
  VALUES1(O(misc_encoding));
}

/* (SYSTEM::SET-MISC-ENCODING encoding) */
LISPFUNN(set_misc_encoding,1) {
  var object encoding = check_encoding(popSTACK(),&O(misc_encoding),false);
  VALUES1(O(misc_encoding) = encoding);
}

#endif /* UNICODE */

/* =========================================================================
 * More functions */

/* (CONVERT-STRING-FROM-BYTES byte-array encoding [:start] [:end]) */
LISPFUN(convert_string_from_bytes,seclass_read,2,0,norest,key,2,
        (kw(start),kw(end)) ) {
  /* Stack layout: array, encoding, start, end. */
  STACK_3 = check_vector(STACK_3); /* check array */
  STACK_2 = check_encoding(STACK_2,DEFAULT_ENC,false);
  var object array = STACK_3;
  STACK_3 = STACK_2; /* encoding */
  STACK_2 = array; /* array */
  /* Stack layout: encoding, array, start, end. */
  if (!boundp(STACK_1))
    STACK_1 = Fixnum_0; /* check start */
  if (missingp(STACK_0)) /* check end */
    STACK_0 = fixnum(vector_length(array));
  /* Convert array to a vector with element type (UNSIGNED-BYTE 8): */
  if (!bit_vector_p(Atype_8Bit,array)) {
    /* (SYS::COERCED-SUBSEQ array '(ARRAY (UNSIGNED-BYTE 8) (*))
                            [:start] [:end]) */
    pushSTACK(array); pushSTACK(O(type_uint8_vector));
    pushSTACK(S(Kstart)); pushSTACK(STACK_(1+3));
    pushSTACK(S(Kend)); pushSTACK(STACK_(0+5));
    funcall(L(coerced_subseq),6);
    if (!bit_vector_p(Atype_8Bit,value1)) { NOTREACHED; }
    STACK_2 = value1;
    STACK_0 = I_I_minus_I(STACK_0,STACK_1); /* end := (- end start) */
    STACK_1 = Fixnum_0; /* start := 0 */
    array = STACK_2;
  }
  /* Determine size of result string: */
  var stringarg sa;
  sa.offset = 0; sa.len = vector_length(array);
  sa.string = array_displace_check(array,sa.len,&sa.offset);
  test_vector_limits(&sa);
  array = sa.string;
  pushSTACK(array);
  /* stack layout: encoding, array */
  var uintL start = sa.offset + sa.index;
  var uintL end = start + sa.len;
 #ifdef UNICODE
  var uintL clen =
    Encoding_mblen(STACK_1)(STACK_1,&TheSbvector(array)->data[start],
                            &TheSbvector(array)->data[end]);
 #else
  var uintL clen = end-start;
 #endif
  /* Allocate and fill the result string: */
  check_stringsize(clen);
  var object string = allocate_string(clen);
  if (clen > 0) {
    array = STACK_0;
    var chart* cptr = &TheSnstring(string)->data[0];
    var const uintB* bptr = &TheSbvector(array)->data[start];
   #ifdef UNICODE
    var const uintB* bendptr = &TheSbvector(array)->data[end];
    var chart* cendptr = cptr+clen;
    Encoding_mbstowcs(STACK_1)(STACK_1,nullobj,&bptr,bendptr,&cptr,cendptr);
    ASSERT(cptr == cendptr);
   #else
    dotimespL(clen,clen, { *cptr++ = as_chart(*bptr++); } );
   #endif
  }
  VALUES1(string); skipSTACK(2);
}

/* (CONVERT-STRING-TO-BYTES string encoding [:start] [:end]) */
LISPFUN(convert_string_to_bytes,seclass_read,2,0,norest,key,2,
        (kw(start),kw(end)) ) {
  /* Stack layout: string, encoding, start, end. */
  STACK_2 = check_encoding(STACK_2,DEFAULT_ENC,false);
  var object string = STACK_3;
  STACK_3 = STACK_2; /* encoding */
  STACK_2 = string; /* string */
  /* Stack layout: encoding, string, start, end. */
  var stringarg sa;
  test_string_limits_ro(&sa); /* check string */
  string = sa.string;
  pushSTACK(string);
  /* Stack layout: encoding, string */
  var const chart* srcptr;
  unpack_sstring_alloca(string,sa.len,sa.offset+sa.index, srcptr=);
  var uintL blen = cslen(STACK_1,srcptr,sa.len);
  /* Allocate and fill the result vector: */
  var object array = allocate_bit_vector(Atype_8Bit,blen);
  if (blen > 0) {
    string = STACK_0;
    unpack_sstring_alloca(string,sa.len,sa.offset+sa.index, srcptr=);
    cstombs(STACK_1,srcptr,sa.len,&TheSbvector(array)->data[0],blen);
  }
  VALUES1(array); skipSTACK(2);
}
