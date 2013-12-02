/*
 * List of all relocatable machine pointers
 * Bruno Haible 1990-2004
 * Sam Steingold 2005
 */

/* There are three kinds of relocatable pointers:
   LPSEUDOCODE(fun)                    local C function defined in stream.d
   XPSEUDOCODE(rettype,name,arglist)   external C function
   XPSEUDODATA(type,name)              external C variable

 The macro PSEUDO, defined in the including file, determines the macro
 expansion for these macros.
   #define PSEUDO PSEUDO_A   for the declaration of the code table
   #define PSEUDO PSEUDO_B   for the declaration of the data table
   #define PSEUDO PSEUDO_C   for the declaration of both tables' elements
   #define PSEUDO PSEUDO_D   for the initialization of the code table
   #define PSEUDO PSEUDO_E   for the initialization of the data table */

#define LPSEUDOCODE CONCAT(LCODE_,PSEUDO)
#define XPSEUDOCODE CONCAT(XCODE_,PSEUDO)
#define XPSEUDODATA CONCAT(XDATA_,PSEUDO)

#define LCODE_PSEUDO_A(fun)  Pseudofun pseudo_##fun;
#define LCODE_PSEUDO_B(fun)
#define LCODE_PSEUDO_C(fun)
#define LCODE_PSEUDO_D(fun)  (Pseudofun)(&fun),
#define LCODE_PSEUDO_E(fun)
#define XCODE_PSEUDO_A(rettype,name,arglist)  Pseudofun pseudo_##name;
#define XCODE_PSEUDO_B(rettype,name,arglist)
#define XCODE_PSEUDO_C(rettype,name,arglist)  extern rettype name arglist;
#define XCODE_PSEUDO_D(rettype,name,arglist)  (Pseudofun)(&name),
#define XCODE_PSEUDO_E(rettype,name,arglist)
#define XDATA_PSEUDO_A(type,name)
#define XDATA_PSEUDO_B(type,name)  Pseudofun pseudo_##name;
#define XDATA_PSEUDO_C(type,name)  extern type name;
#define XDATA_PSEUDO_D(type,name)
#define XDATA_PSEUDO_E(type,name)  (Pseudofun)(&name),


/* For hashtabl.d. */
XPSEUDOCODE(bool, eql, (object obj1, object obj2))
XPSEUDOCODE(bool, equal, (object obj1, object obj2))
XPSEUDOCODE(bool, equalp, (object obj1, object obj2))
XPSEUDOCODE(uint32, hashcode1stable, (object obj))
XPSEUDOCODE(uint32, hashcode2, (object obj))
XPSEUDOCODE(uint32, hashcode2stable, (object obj))
XPSEUDOCODE(uint32, hashcode3, (object obj))
XPSEUDOCODE(uint32, hashcode3stable, (object obj))
XPSEUDOCODE(uint32, hashcode4, (object obj))
XPSEUDOCODE(bool, gcinvariant_hashcode1_p, (object obj))
XPSEUDOCODE(bool, gcinvariant_hashcode1stable_p, (object obj))
XPSEUDOCODE(bool, gcinvariant_hashcode2_p, (object obj))
XPSEUDOCODE(bool, gcinvariant_hashcode2stable_p, (object obj))
XPSEUDOCODE(bool, gcinvariant_hashcode3_p, (object obj))
XPSEUDOCODE(bool, gcinvariant_hashcode3stable_p, (object obj))
XPSEUDOCODE(bool, gcinvariant_hashcode4_p, (object obj))
XPSEUDOCODE(bool, hash_lookup_builtin, (object ht, object obj, bool allowgc, gcv_object_t** KVptr_, gcv_object_t** Iptr_))
#ifndef GENERATIONAL_GC
XPSEUDOCODE(bool, hash_lookup_builtin_with_rehash, (object ht, object obj, bool allowgc, gcv_object_t** KVptr_, gcv_object_t** Iptr_))
#endif
XPSEUDOCODE(bool, hash_lookup_user, (object ht, object obj, bool allowgc, gcv_object_t** KVptr_, gcv_object_t** Iptr_))

LPSEUDOCODE(rd_by_error) LPSEUDOCODE(rd_by_array_error) LPSEUDOCODE(rd_by_array_dummy)
LPSEUDOCODE(wr_by_error) LPSEUDOCODE(wr_by_array_error) LPSEUDOCODE(wr_by_array_dummy)
LPSEUDOCODE(rd_ch_error) LPSEUDOCODE(pk_ch_dummy) LPSEUDOCODE(rd_ch_array_error) LPSEUDOCODE(rd_ch_array_dummy)
LPSEUDOCODE(wr_ch_error) LPSEUDOCODE(wr_ch_array_error) LPSEUDOCODE(wr_ch_array_dummy)
LPSEUDOCODE(wr_ch_pending_newline) LPSEUDOCODE(wr_ch_array_pending_newline)

LPSEUDOCODE(rd_by_synonym) LPSEUDOCODE(rd_by_array_synonym) LPSEUDOCODE(wr_by_synonym) LPSEUDOCODE(wr_by_array_synonym) LPSEUDOCODE(rd_ch_synonym) LPSEUDOCODE(pk_ch_synonym) LPSEUDOCODE(rd_ch_array_synonym) LPSEUDOCODE(wr_ch_synonym) LPSEUDOCODE(wr_ch_array_synonym)
LPSEUDOCODE(wr_by_broad) LPSEUDOCODE(wr_by_array_broad) LPSEUDOCODE(wr_ch_broad) LPSEUDOCODE(wr_ch_array_broad)
LPSEUDOCODE(rd_by_concat) LPSEUDOCODE(rd_by_array_concat) LPSEUDOCODE(rd_ch_concat) LPSEUDOCODE(pk_ch_concat) LPSEUDOCODE(rd_ch_array_concat)
LPSEUDOCODE(wr_by_twoway) LPSEUDOCODE(wr_by_array_twoway) LPSEUDOCODE(wr_ch_twoway) LPSEUDOCODE(wr_ch_array_twoway)
LPSEUDOCODE(rd_by_twoway) LPSEUDOCODE(rd_by_array_twoway) LPSEUDOCODE(rd_ch_twoway) LPSEUDOCODE(pk_ch_twoway) LPSEUDOCODE(rd_ch_array_twoway)
LPSEUDOCODE(rd_by_echo) LPSEUDOCODE(rd_by_array_echo) LPSEUDOCODE(rd_ch_echo) LPSEUDOCODE(rd_ch_array_echo)
LPSEUDOCODE(rd_ch_str_in) LPSEUDOCODE(rd_ch_array_str_in)
LPSEUDOCODE(wr_ch_str_out) LPSEUDOCODE(wr_ch_array_str_out)
LPSEUDOCODE(wr_ch_str_push)
LPSEUDOCODE(wr_ch_pphelp) LPSEUDOCODE(wr_ch_array_pphelp)
LPSEUDOCODE(rd_ch_buff_in)
LPSEUDOCODE(wr_ch_buff_out)
#ifdef GENERIC_STREAMS
LPSEUDOCODE(rd_ch_generic) LPSEUDOCODE(pk_ch_generic) LPSEUDOCODE(wr_ch_generic) LPSEUDOCODE(wr_ch_array_generic) LPSEUDOCODE(rd_by_generic) LPSEUDOCODE(wr_by_generic)
#endif

LPSEUDOCODE(rd_by_iau_unbuffered) LPSEUDOCODE(rd_by_ias_unbuffered) LPSEUDOCODE(rd_by_iau8_unbuffered) LPSEUDOCODE(rd_by_array_iau8_unbuffered)
LPSEUDOCODE(wr_by_iau_unbuffered) LPSEUDOCODE(wr_by_ias_unbuffered) LPSEUDOCODE(wr_by_iau8_unbuffered) LPSEUDOCODE(wr_by_array_iau8_unbuffered)
LPSEUDOCODE(rd_ch_unbuffered) LPSEUDOCODE(rd_ch_array_unbuffered)
LPSEUDOCODE(wr_ch_unbuffered_unix) LPSEUDOCODE(wr_ch_array_unbuffered_unix)
LPSEUDOCODE(wr_ch_unbuffered_mac) LPSEUDOCODE(wr_ch_array_unbuffered_mac)
LPSEUDOCODE(wr_ch_unbuffered_dos) LPSEUDOCODE(wr_ch_array_unbuffered_dos)
LPSEUDOCODE(rd_ch_buffered) LPSEUDOCODE(rd_ch_array_buffered)
LPSEUDOCODE(wr_ch_buffered_unix) LPSEUDOCODE(wr_ch_array_buffered_unix)
LPSEUDOCODE(wr_ch_buffered_mac) LPSEUDOCODE(wr_ch_array_buffered_mac)
LPSEUDOCODE(wr_ch_buffered_dos) LPSEUDOCODE(wr_ch_array_buffered_dos)
LPSEUDOCODE(rd_by_iau_buffered) LPSEUDOCODE(rd_by_ias_buffered) LPSEUDOCODE(rd_by_ibu_buffered) LPSEUDOCODE(rd_by_ibs_buffered) LPSEUDOCODE(rd_by_icu_buffered) LPSEUDOCODE(rd_by_ics_buffered) LPSEUDOCODE(rd_by_iau8_buffered)
LPSEUDOCODE(rd_by_array_iau8_buffered)
LPSEUDOCODE(wr_by_iau_buffered) LPSEUDOCODE(wr_by_ias_buffered) LPSEUDOCODE(wr_by_ibu_buffered) LPSEUDOCODE(wr_by_ibs_buffered) LPSEUDOCODE(wr_by_icu_buffered) LPSEUDOCODE(wr_by_ics_buffered) LPSEUDOCODE(wr_by_iau8_buffered)
LPSEUDOCODE(wr_by_array_iau8_buffered)
#if defined(KEYBOARD) || defined(MAYBE_NEXTAPP)
LPSEUDOCODE(rd_ch_keyboard)
#endif
#if defined(MAYBE_NEXTAPP)
LPSEUDOCODE(wr_ch_terminal) LPSEUDOCODE(rd_ch_terminal)
#endif
#ifdef UNIX
LPSEUDOCODE(wr_ch_terminal1) LPSEUDOCODE(rd_ch_terminal1) LPSEUDOCODE(wr_ch_array_terminal1)
#if defined(GNU_READLINE) || defined(MAYBE_NEXTAPP)
LPSEUDOCODE(wr_ch_terminal3) LPSEUDOCODE(rd_ch_terminal3) LPSEUDOCODE(wr_ch_array_terminal3)
#endif
#endif
#ifdef SCREEN
LPSEUDOCODE(wr_ch_window)
#endif
#ifdef PRINTER
LPSEUDOCODE(wr_ch_printer)
#endif

/* External definitions from ENCODING.D: */
#ifdef UNICODE
XPSEUDOCODE(object, base64_range, (object encoding, uintL start, uintL end, uintL maxintervals))
XPSEUDOCODE(uintL, base64_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDOCODE(void, base64_mbstowcs, (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDOCODE(uintL, base64_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDOCODE(void, base64_wcstombs, (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDOCODE(object, all_range, (object encoding, uintL start, uintL end, uintL maxintervals))
XPSEUDOCODE(object, bmp_range, (object encoding, uintL start, uintL end, uintL maxintervals))
XPSEUDOCODE(uintL, uni16_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDOCODE(void, uni16be_mbstowcs, (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDOCODE(void, uni16le_mbstowcs, (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDOCODE(uintL, uni16_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDOCODE(void, uni16be_wcstombs, (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDOCODE(void, uni16le_wcstombs, (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDOCODE(uintL, uni32be_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDOCODE(uintL, uni32le_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDOCODE(void, uni32be_mbstowcs, (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDOCODE(void, uni32le_mbstowcs, (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDOCODE(uintL, uni32_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDOCODE(void, uni32be_wcstombs, (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDOCODE(void, uni32le_wcstombs, (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDOCODE(uintL, utf8_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDOCODE(void, utf8_mbstowcs, (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDOCODE(uintL, utf8_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDOCODE(void, utf8_wcstombs, (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDOCODE(uintL, java_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDOCODE(void, java_mbstowcs, (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDOCODE(uintL, java_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDOCODE(void, java_wcstombs, (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDOCODE(uintL, nls_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDOCODE(void, nls_mbstowcs, (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDOCODE(uintL, nls_asciiext_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDOCODE(void, nls_asciiext_mbstowcs, (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDOCODE(uintL, nls_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDOCODE(void, nls_wcstombs, (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDOCODE(uintL, nls_asciiext_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDOCODE(void, nls_asciiext_wcstombs, (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDOCODE(object, nls_range, (object encoding, uintL start, uintL end, uintL maxintervals))
XPSEUDODATA(struct nls_table_t, nls_ascii_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_1_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_2_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_3_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_4_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_5_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_6_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_7_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_8_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_9_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_10_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_13_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_14_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_15_table)
XPSEUDODATA(struct nls_table_t, nls_iso8859_16_table)
XPSEUDODATA(struct nls_table_t, nls_koi8_r_table)
XPSEUDODATA(struct nls_table_t, nls_koi8_u_table)
XPSEUDODATA(struct nls_table_t, nls_mac_arabic_table)
XPSEUDODATA(struct nls_table_t, nls_mac_centraleurope_table)
XPSEUDODATA(struct nls_table_t, nls_mac_croatian_table)
XPSEUDODATA(struct nls_table_t, nls_mac_cyrillic_table)
XPSEUDODATA(struct nls_table_t, nls_mac_dingbat_table)
XPSEUDODATA(struct nls_table_t, nls_mac_greek_table)
XPSEUDODATA(struct nls_table_t, nls_mac_hebrew_table)
XPSEUDODATA(struct nls_table_t, nls_mac_iceland_table)
XPSEUDODATA(struct nls_table_t, nls_mac_roman_table)
XPSEUDODATA(struct nls_table_t, nls_mac_romania_table)
XPSEUDODATA(struct nls_table_t, nls_mac_symbol_table)
XPSEUDODATA(struct nls_table_t, nls_mac_thai_table)
XPSEUDODATA(struct nls_table_t, nls_mac_turkish_table)
XPSEUDODATA(struct nls_table_t, nls_mac_ukraine_table)
XPSEUDODATA(struct nls_table_t, nls_cp437_ms_table)
XPSEUDODATA(struct nls_table_t, nls_cp437_ibm_table)
XPSEUDODATA(struct nls_table_t, nls_cp737_table)
XPSEUDODATA(struct nls_table_t, nls_cp775_table)
XPSEUDODATA(struct nls_table_t, nls_cp850_table)
XPSEUDODATA(struct nls_table_t, nls_cp852_ms_table)
XPSEUDODATA(struct nls_table_t, nls_cp852_ibm_table)
XPSEUDODATA(struct nls_table_t, nls_cp855_table)
XPSEUDODATA(struct nls_table_t, nls_cp857_table)
XPSEUDODATA(struct nls_table_t, nls_cp860_ms_table)
XPSEUDODATA(struct nls_table_t, nls_cp860_ibm_table)
XPSEUDODATA(struct nls_table_t, nls_cp861_ms_table)
XPSEUDODATA(struct nls_table_t, nls_cp861_ibm_table)
XPSEUDODATA(struct nls_table_t, nls_cp862_ms_table)
XPSEUDODATA(struct nls_table_t, nls_cp862_ibm_table)
XPSEUDODATA(struct nls_table_t, nls_cp863_ms_table)
XPSEUDODATA(struct nls_table_t, nls_cp863_ibm_table)
XPSEUDODATA(struct nls_table_t, nls_cp864_ms_table)
XPSEUDODATA(struct nls_table_t, nls_cp864_ibm_table)
XPSEUDODATA(struct nls_table_t, nls_cp865_ms_table)
XPSEUDODATA(struct nls_table_t, nls_cp865_ibm_table)
XPSEUDODATA(struct nls_table_t, nls_cp866_table)
XPSEUDODATA(struct nls_table_t, nls_cp869_ms_table)
XPSEUDODATA(struct nls_table_t, nls_cp869_ibm_table)
XPSEUDODATA(struct nls_table_t, nls_cp874_ms_table)
XPSEUDODATA(struct nls_table_t, nls_cp874_ibm_table)
XPSEUDODATA(struct nls_table_t, nls_cp1250_table)
XPSEUDODATA(struct nls_table_t, nls_cp1251_table)
XPSEUDODATA(struct nls_table_t, nls_cp1252_table)
XPSEUDODATA(struct nls_table_t, nls_cp1253_table)
XPSEUDODATA(struct nls_table_t, nls_cp1254_table)
XPSEUDODATA(struct nls_table_t, nls_cp1256_table)
XPSEUDODATA(struct nls_table_t, nls_cp1257_table)
XPSEUDODATA(struct nls_table_t, nls_hp_roman8_table)
XPSEUDODATA(struct nls_table_t, nls_nextstep_table)
XPSEUDODATA(struct nls_table_t, nls_jisx0201_table)
#ifdef HAVE_GOOD_ICONV
XPSEUDOCODE(uintL, iconv_mblen, (object encoding, const uintB* src, const uintB* srcend))
XPSEUDOCODE(void, iconv_mbstowcs, (object encoding, object stream, const uintB* *srcp, const uintB* srcend, chart* *destp, chart* destend))
XPSEUDOCODE(uintL, iconv_wcslen, (object encoding, const chart* src, const chart* srcend))
XPSEUDOCODE(void, iconv_wcstombs, (object encoding, object stream, const chart* *srcp, const chart* srcend, uintB* *destp, uintB* destend))
XPSEUDOCODE(object, iconv_range, (object encoding, uintL start, uintL end, uintL maxintervals))
#endif
#endif
