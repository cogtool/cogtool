/*
 * List of all symbols known to the C-program ("program constants")
 * Bruno Haible 1990-2005
 * Sam Steingold 1998-2006
 */

/* The macro LISPSYM declares a LISP symbol.
 LISPSYM(name,printname,package)
 > name: C-name of the symbol.
 > printname: the printname of the symbol (a C string),
     if the string is not an ASCII string, the first character must be a space
     (thus no symbol name can start with a space -- see init_symbol_tab_*())
 > package: home-package of the symbol, either lisp or system or keyword.
 >          it is exported automatically from package lisp. */

/* expander for the declaration of the symbol table: */
#define LISPSYM_A(name,printname,package)       \
  symbol_ S_##name;

/* expander for the initialization of the symbol table: */
#ifdef TYPECODES
  #ifdef DEBUG_GCSAFETY
    #define LISPSYM_B(name,printname,package)  \
      { S(name), unbound, unbound, unbound, NIL, NIL, NIL, },
  #else
    #define LISPSYM_B(name,printname,package)  \
      { {S(name)}, unbound, unbound, unbound, NIL, NIL, NIL, },
  #endif
#else
  #if defined(LINUX_NOEXEC_HEAPCODES) && 0
    #define LISPSYM_B(name,printname,package)  \
      { S(name), xrecord_tfl(Rectype_Symbol,0,symbol_length,0), \
        unbound, unbound, unbound, NIL, NIL, NIL, unbound, },
  #else
    #define LISPSYM_B(name,printname,package)  \
      { S(name), xrecord_tfl(Rectype_Symbol,0,symbol_length,0), \
        unbound, unbound, unbound, NIL, NIL, NIL, },
  #endif
#endif
#define LISPSYM_C(name,printname,package)  printname,
#define LISPSYM_D(name,printname,package)  (uintB)enum_##package##_index,

/* which expander is used must be specified in the main file. */

/* The marker 'ABI' means that although the symbol is not exported from one
   of the public packages (COMMON-LISP, CLOS, EXT, FFI), it can be referred
   to from user compiled .fas files, because it occurs as part of macro
   expansions. */

LISPSYM(nil,"NIL",lisp)
LISPSYM(t,"T",lisp)

/* FSUBRs in CONTROL: */
LISPSYM(eval_when,"EVAL-WHEN",lisp)
LISPSYM(quote,"QUOTE",lisp)
LISPSYM(function,"FUNCTION",lisp)
LISPSYM(setq,"SETQ",lisp)
LISPSYM(psetq,"PSETQ",lisp)
LISPSYM(progn,"PROGN",lisp)
LISPSYM(prog1,"PROG1",lisp)
LISPSYM(prog2,"PROG2",lisp)
LISPSYM(let,"LET",lisp)
LISPSYM(letstern,"LET*",lisp)
LISPSYM(locally,"LOCALLY",lisp)
LISPSYM(compiler_let,"COMPILER-LET",ext)
LISPSYM(progv,"PROGV",lisp)
LISPSYM(flet,"FLET",lisp)
LISPSYM(labels,"LABELS",lisp)
LISPSYM(macrolet,"MACROLET",lisp)
LISPSYM(function_macro_let,"FUNCTION-MACRO-LET",system)
LISPSYM(symbol_macrolet,"SYMBOL-MACROLET",lisp)
LISPSYM(if,"IF",lisp)
LISPSYM(when,"WHEN",lisp)
LISPSYM(unless,"UNLESS",lisp)
LISPSYM(cond,"COND",lisp)
LISPSYM(case ,"CASE",lisp)
LISPSYM(block,"BLOCK",lisp)
LISPSYM(return_from,"RETURN-FROM",lisp)
LISPSYM(tagbody,"TAGBODY",lisp)
LISPSYM(go,"GO",lisp)
LISPSYM(multiple_value_list,"MULTIPLE-VALUE-LIST",lisp)
LISPSYM(multiple_value_call,"MULTIPLE-VALUE-CALL",lisp)
LISPSYM(multiple_value_prog1,"MULTIPLE-VALUE-PROG1",lisp)
LISPSYM(multiple_value_bind,"MULTIPLE-VALUE-BIND",lisp)
LISPSYM(multiple_value_setq,"MULTIPLE-VALUE-SETQ",lisp)
LISPSYM(catch,"CATCH",lisp)
LISPSYM(unwind_protect,"UNWIND-PROTECT",lisp)
LISPSYM(throw,"THROW",lisp)
LISPSYM(declare,"DECLARE",lisp)
LISPSYM(the,"THE",lisp)
LISPSYM(load_time_value,"LOAD-TIME-VALUE",lisp)
LISPSYM(and,"AND",lisp)
LISPSYM(or,"OR",lisp)

/* SUBRs: */
/* ---------- SPVW ---------- */
/* no SUBRs */
/* ---------- EVAL ---------- */
LISPSYM(funtabref,"%FUNTABREF",system)
LISPSYM(subr_info,"SUBR-INFO",system)
LISPSYM(add_implicit_block,"ADD-IMPLICIT-BLOCK",system)
LISPSYM(function_block_name,"FUNCTION-BLOCK-NAME",system)
/* ---------- ARRAY ---------- */
LISPSYM(copy_simple_vector,"%COPY-SIMPLE-VECTOR",system) /* ABI */
LISPSYM(vector,"VECTOR",lisp)
LISPSYM(aref,"AREF",lisp)
LISPSYM(store,"STORE",system) /* ABI */
LISPSYM(svref,"SVREF",lisp)
LISPSYM(svstore,"SVSTORE",system) /* ABI */
LISPSYM(psvstore,"%SVSTORE",system) /* ABI */
LISPSYM(row_major_aref,"ROW-MAJOR-AREF",lisp)
LISPSYM(row_major_store,"ROW-MAJOR-STORE",system) /* ABI */
LISPSYM(array_element_type,"ARRAY-ELEMENT-TYPE",lisp)
LISPSYM(array_rank,"ARRAY-RANK",lisp)
LISPSYM(array_dimension,"ARRAY-DIMENSION",lisp)
LISPSYM(array_dimensions,"ARRAY-DIMENSIONS",lisp)
LISPSYM(array_total_size,"ARRAY-TOTAL-SIZE",lisp)
LISPSYM(array_in_bounds_p,"ARRAY-IN-BOUNDS-P",lisp)
LISPSYM(array_row_major_index,"ARRAY-ROW-MAJOR-INDEX",lisp)
LISPSYM(adjustable_array_p,"ADJUSTABLE-ARRAY-P",lisp)
LISPSYM(array_displacement,"ARRAY-DISPLACEMENT",lisp)
LISPSYM(bit,"BIT",lisp)
LISPSYM(sbit,"SBIT",lisp)
LISPSYM(bit_and,"BIT-AND",lisp)
LISPSYM(bit_ior,"BIT-IOR",lisp)
LISPSYM(bit_xor,"BIT-XOR",lisp)
LISPSYM(bit_eqv,"BIT-EQV",lisp)
LISPSYM(bit_nand,"BIT-NAND",lisp)
LISPSYM(bit_nor,"BIT-NOR",lisp)
LISPSYM(bit_andc1,"BIT-ANDC1",lisp)
LISPSYM(bit_andc2,"BIT-ANDC2",lisp)
LISPSYM(bit_orc1,"BIT-ORC1",lisp)
LISPSYM(bit_orc2,"BIT-ORC2",lisp)
LISPSYM(bit_not,"BIT-NOT",lisp)
LISPSYM(array_has_fill_pointer_p,"ARRAY-HAS-FILL-POINTER-P",lisp)
LISPSYM(fill_pointer,"FILL-POINTER",lisp)
LISPSYM(set_fill_pointer,"SET-FILL-POINTER",system) /* ABI */
LISPSYM(vector_push,"VECTOR-PUSH",lisp)
LISPSYM(vector_pop,"VECTOR-POP",lisp)
LISPSYM(vector_push_extend,"VECTOR-PUSH-EXTEND",lisp)
LISPSYM(make_array,"MAKE-ARRAY",lisp)
LISPSYM(adjust_array,"ADJUST-ARRAY",lisp)
LISPSYM(vector_init,"VECTOR-INIT",system)
LISPSYM(vector_upd,"VECTOR-UPD",system)
LISPSYM(vector_endtest,"VECTOR-ENDTEST",system)
LISPSYM(vector_fe_init,"VECTOR-FE-INIT",system)
LISPSYM(vector_fe_upd,"VECTOR-FE-UPD",system)
LISPSYM(vector_fe_endtest,"VECTOR-FE-ENDTEST",system)
LISPSYM(vector_length,"VECTOR-LENGTH",system)
LISPSYM(vector_init_start,"VECTOR-INIT-START",system)
LISPSYM(vector_fe_init_end,"VECTOR-FE-INIT-END",system)
LISPSYM(make_bit_vector,"MAKE-BIT-VECTOR",system)
/* ---------- CHARSTRG ---------- */
LISPSYM(string_info,"STRING-INFO",system)
LISPSYM(standard_char_p,"STANDARD-CHAR-P",lisp)
LISPSYM(graphic_char_p,"GRAPHIC-CHAR-P",lisp)
LISPSYM(char_width,"CHAR-WIDTH",ext)
LISPSYM(string_char_p,"STRING-CHAR-P",ext)
#if (base_char_code_limit < char_code_limit)
LISPSYM(base_char_p,"BASE-CHAR-P",system) /* ABI */
#endif
LISPSYM(alpha_char_p,"ALPHA-CHAR-P",lisp)
LISPSYM(upper_case_p,"UPPER-CASE-P",lisp)
LISPSYM(lower_case_p,"LOWER-CASE-P",lisp)
LISPSYM(both_case_p,"BOTH-CASE-P",lisp)
LISPSYM(digit_char_p,"DIGIT-CHAR-P",lisp)
LISPSYM(alphanumericp,"ALPHANUMERICP",lisp)
LISPSYM(char_gleich,"CHAR=",lisp)
LISPSYM(char_ungleich,"CHAR/=",lisp)
LISPSYM(char_kleiner,"CHAR<",lisp)
LISPSYM(char_groesser,"CHAR>",lisp)
LISPSYM(char_klgleich,"CHAR<=",lisp)
LISPSYM(char_grgleich,"CHAR>=",lisp)
LISPSYM(char_equal,"CHAR-EQUAL",lisp)
LISPSYM(char_not_equal,"CHAR-NOT-EQUAL",lisp)
LISPSYM(char_lessp,"CHAR-LESSP",lisp)
LISPSYM(char_greaterp,"CHAR-GREATERP",lisp)
LISPSYM(char_not_greaterp,"CHAR-NOT-GREATERP",lisp)
LISPSYM(char_not_lessp,"CHAR-NOT-LESSP",lisp)
LISPSYM(char_code,"CHAR-CODE",lisp)
LISPSYM(code_char,"CODE-CHAR",lisp)
LISPSYM(character,"CHARACTER",lisp)
LISPSYM(char_upcase,"CHAR-UPCASE",lisp)
LISPSYM(char_downcase,"CHAR-DOWNCASE",lisp)
LISPSYM(digit_char,"DIGIT-CHAR",lisp)
LISPSYM(char_int,"CHAR-INT",lisp)
LISPSYM(int_char,"INT-CHAR",ext)
LISPSYM(char_name,"CHAR-NAME",lisp)
LISPSYM(char_invertcase,"CHAR-INVERTCASE",ext)
LISPSYM(string_invertcase,"STRING-INVERTCASE",ext)
LISPSYM(nstring_invertcase,"NSTRING-INVERTCASE",ext)
LISPSYM(char,"CHAR",lisp)
LISPSYM(schar,"SCHAR",lisp)
LISPSYM(store_char,"STORE-CHAR",system) /* ABI */
LISPSYM(store_schar,"STORE-SCHAR",system) /* ABI */
LISPSYM(string_gleich,"STRING=",lisp)
LISPSYM(cs_string_gleich,"STRING=",cs_lisp)
LISPSYM(string_ungleich,"STRING/=",lisp)
LISPSYM(cs_string_ungleich,"STRING/=",cs_lisp)
LISPSYM(string_kleiner,"STRING<",lisp)
LISPSYM(cs_string_kleiner,"STRING<",cs_lisp)
LISPSYM(string_groesser,"STRING>",lisp)
LISPSYM(cs_string_groesser,"STRING>",cs_lisp)
LISPSYM(string_klgleich,"STRING<=",lisp)
LISPSYM(cs_string_klgleich,"STRING<=",cs_lisp)
LISPSYM(string_grgleich,"STRING>=",lisp)
LISPSYM(cs_string_grgleich,"STRING>=",cs_lisp)
LISPSYM(string_equal,"STRING-EQUAL",lisp)
LISPSYM(string_not_equal,"STRING-NOT-EQUAL",lisp)
LISPSYM(string_lessp,"STRING-LESSP",lisp)
LISPSYM(string_greaterp,"STRING-GREATERP",lisp)
LISPSYM(string_not_greaterp,"STRING-NOT-GREATERP",lisp)
LISPSYM(string_not_lessp,"STRING-NOT-LESSP",lisp)
LISPSYM(search_string_gleich,"SEARCH-STRING=",system)
LISPSYM(search_string_equal,"SEARCH-STRING-EQUAL",system)
LISPSYM(make_string,"MAKE-STRING",lisp)
LISPSYM(string_both_trim,"STRING-BOTH-TRIM",system)
LISPSYM(string_width,"STRING-WIDTH",ext)
LISPSYM(nstring_upcase,"NSTRING-UPCASE",lisp)
LISPSYM(string_upcase,"STRING-UPCASE",lisp)
LISPSYM(nstring_downcase,"NSTRING-DOWNCASE",lisp)
LISPSYM(string_downcase,"STRING-DOWNCASE",lisp)
LISPSYM(nstring_capitalize,"NSTRING-CAPITALIZE",lisp)
LISPSYM(string_capitalize,"STRING-CAPITALIZE",lisp)
LISPSYM(string,"STRING",lisp)
LISPSYM(cs_string,"STRING",cs_lisp)
LISPSYM(name_char,"NAME-CHAR",lisp)
LISPSYM(substring,"SUBSTRING",ext)
LISPSYM(string_concat,"STRING-CONCAT",ext)
/* ---------- CONTROL ---------- */
LISPSYM(exit,"%EXIT",system)
LISPSYM(symbol_value,"SYMBOL-VALUE",lisp)
LISPSYM(symbol_function,"SYMBOL-FUNCTION",lisp)
LISPSYM(fdefinition,"FDEFINITION",lisp)
LISPSYM(boundp,"BOUNDP",lisp)
LISPSYM(fboundp,"FBOUNDP",lisp)
LISPSYM(special_operator_p,"SPECIAL-OPERATOR-P",lisp)
LISPSYM(set,"SET-SYMBOL-VALUE",system) /* ABI */
LISPSYM(makunbound,"MAKUNBOUND",lisp)
LISPSYM(fmakunbound,"FMAKUNBOUND",lisp)
LISPSYM(apply,"APPLY",lisp)
LISPSYM(funcall,"FUNCALL",lisp)
LISPSYM(mapcar,"MAPCAR",lisp)
LISPSYM(maplist,"MAPLIST",lisp)
LISPSYM(mapc,"MAPC",lisp)
LISPSYM(mapl,"MAPL",lisp)
LISPSYM(mapcan,"MAPCAN",lisp)
LISPSYM(mapcon,"MAPCON",lisp)
LISPSYM(mapcap,"MAPCAP",ext)
LISPSYM(maplap,"MAPLAP",ext)
LISPSYM(values,"VALUES",lisp)
LISPSYM(values_list,"VALUES-LIST",lisp)
LISPSYM(driver,"DRIVER",system)
LISPSYM(unwind_to_driver,"UNWIND-TO-DRIVER",system)
LISPSYM(macro_function,"MACRO-FUNCTION",lisp)
LISPSYM(macroexpand,"MACROEXPAND",lisp)
LISPSYM(macroexpand_1,"MACROEXPAND-1",lisp)
LISPSYM(proclaim,"PROCLAIM",lisp)
LISPSYM(eval,"EVAL",lisp)
LISPSYM(evalhook,"EVALHOOK",ext)
LISPSYM(applyhook,"APPLYHOOK",ext)
LISPSYM(constantp,"CONSTANTP",lisp)
LISPSYM(global_symbol_macro_p,"GLOBAL-SYMBOL-MACRO-P",system)
LISPSYM(function_side_effect,"FUNCTION-SIDE-EFFECT",system)
LISPSYM(function_name_p,"FUNCTION-NAME-P",system)
LISPSYM(check_function_name,"CHECK-FUNCTION-NAME",system)
LISPSYM(check_symbol,"CHECK-SYMBOL",system)
LISPSYM(parse_body,"PARSE-BODY",system)
LISPSYM(keyword_test,"KEYWORD-TEST",system) /* ABI */
LISPSYM(xor,"XOR",ext)
/* ---------- DEBUG ---------- */
LISPSYM(read_form,"READ-FORM",system)
LISPSYM(read_eval_print,"READ-EVAL-PRINT",system)
LISPSYM(initial_break_driver,"INITIAL-BREAK-DRIVER",system)
LISPSYM(load,"LOAD",lisp)
LISPSYM(frame_up_1,"FRAME-UP-1",system)
LISPSYM(frame_up,"FRAME-UP",system)
LISPSYM(frame_down_1,"FRAME-DOWN-1",system)
LISPSYM(frame_down,"FRAME-DOWN",system)
LISPSYM(the_frame,"THE-FRAME",system)
LISPSYM(same_env_as,"SAME-ENV-AS",system)
LISPSYM(eval_at,"EVAL-AT",system)
LISPSYM(eval_frame_p,"EVAL-FRAME-P",system)
LISPSYM(driver_frame_p,"DRIVER-FRAME-P",system)
LISPSYM(trap_eval_frame,"TRAP-EVAL-FRAME",system)
LISPSYM(redo_eval_frame,"REDO-EVAL-FRAME",system)
LISPSYM(return_from_eval_frame,"RETURN-FROM-EVAL-FRAME",system)
LISPSYM(describe_frame,"DESCRIBE-FRAME",system)
LISPSYM(show_stack,"SHOW-STACK",ext)
LISPSYM(crash,"CRASH",system)
LISPSYM(proom,"%ROOM",system)
LISPSYM(gc,"GC",ext)
/* ---------- ENCODING ---------- */
LISPSYM(make_encoding,"MAKE-ENCODING",ext)
LISPSYM(encodingp,"ENCODINGP",system) /* ABI */
LISPSYM(charset_typep,"CHARSET-TYPEP",system)
LISPSYM(encoding_line_terminator,"ENCODING-LINE-TERMINATOR",ext)
#ifdef UNICODE
LISPSYM(encoding_charset,"ENCODING-CHARSET",ext)
LISPSYM(charset_range,"CHARSET-RANGE",system)
#endif
LISPSYM(default_file_encoding,"DEFAULT-FILE-ENCODING",system) /* ABI */
LISPSYM(set_default_file_encoding,"SET-DEFAULT-FILE-ENCODING",system) /* ABI */
#ifdef UNICODE
LISPSYM(pathname_encoding,"PATHNAME-ENCODING",system) /* ABI */
LISPSYM(set_pathname_encoding,"SET-PATHNAME-ENCODING",system) /* ABI */
LISPSYM(terminal_encoding,"TERMINAL-ENCODING",system) /* ABI */
LISPSYM(set_terminal_encoding,"SET-TERMINAL-ENCODING",system) /* ABI */
#if defined(HAVE_FFI) || defined(HAVE_AFFI)
LISPSYM(foreign_encoding,"FOREIGN-ENCODING",system) /* ABI */
LISPSYM(set_foreign_encoding,"SET-FOREIGN-ENCODING",system) /* ABI */
#endif
LISPSYM(misc_encoding,"MISC-ENCODING",system) /* ABI */
LISPSYM(set_misc_encoding,"SET-MISC-ENCODING",system) /* ABI */
#endif
LISPSYM(convert_string_from_bytes,"CONVERT-STRING-FROM-BYTES",ext)
LISPSYM(convert_string_to_bytes,"CONVERT-STRING-TO-BYTES",ext)
/* ---------- ERROR ---------- */
LISPSYM(error,"ERROR",lisp)
LISPSYM(defclcs,"%DEFCLCS",system)
LISPSYM(cerror_of_type,"CERROR-OF-TYPE",system)
LISPSYM(error_of_type,"ERROR-OF-TYPE",system) /* ABI */
LISPSYM(invoke_debugger,"INVOKE-DEBUGGER",lisp)
LISPSYM(clcs_signal,"SIGNAL",lisp)
/* ---------- HASHTABL ---------- */
LISPSYM(make_hash_table,"MAKE-HASH-TABLE",lisp)
LISPSYM(gethash,"GETHASH",lisp)
LISPSYM(puthash,"PUTHASH",system) /* ABI */
LISPSYM(remhash,"REMHASH",lisp)
LISPSYM(maphash,"MAPHASH",lisp)
LISPSYM(clrhash,"CLRHASH",lisp)
LISPSYM(hash_table_count,"HASH-TABLE-COUNT",lisp)
LISPSYM(hash_table_rehash_size,"HASH-TABLE-REHASH-SIZE",lisp)
LISPSYM(hash_table_rehash_threshold,"HASH-TABLE-REHASH-THRESHOLD",lisp)
LISPSYM(hash_table_size,"HASH-TABLE-SIZE",lisp)
LISPSYM(hash_table_test,"HASH-TABLE-TEST",lisp)
LISPSYM(fasthash_stable_p,"FASTHASH-STABLE-P",system)
LISPSYM(stablehash_stable_p,"STABLEHASH-STABLE-P",system)
LISPSYM(hash_table_iterator,"HASH-TABLE-ITERATOR",system) /* ABI */
LISPSYM(hash_table_iterate,"HASH-TABLE-ITERATE",system) /* ABI */
LISPSYM(hash_table_weak_p,"HASH-TABLE-WEAK-P",ext)
LISPSYM(set_hash_table_weak_p,"(SETF HASH-TABLE-WEAK-P)",system) /* ABI */
LISPSYM(hash_table_warn_if_needs_rehash_after_gc,"HASH-TABLE-WARN-IF-NEEDS-REHASH-AFTER-GC",ext)
LISPSYM(set_hash_table_warn_if_needs_rehash_after_gc,"(SETF HASH-TABLE-WARN-IF-NEEDS-REHASH-AFTER-GC)",system) /* ABI */
LISPSYM(class_gethash,"CLASS-GETHASH",clos)
LISPSYM(class_tuple_gethash,"CLASS-TUPLE-GETHASH",clos)
LISPSYM(sxhash,"SXHASH",lisp)
/* ---------- IO ---------- */
LISPSYM(defio,"%DEFIO",system)
LISPSYM(copy_readtable,"COPY-READTABLE",lisp)
LISPSYM(set_syntax_from_char,"SET-SYNTAX-FROM-CHAR",lisp)
LISPSYM(set_macro_character,"SET-MACRO-CHARACTER",lisp)
LISPSYM(get_macro_character,"GET-MACRO-CHARACTER",lisp)
LISPSYM(make_dispatch_macro_character,"MAKE-DISPATCH-MACRO-CHARACTER",lisp)
LISPSYM(set_dispatch_macro_character,"SET-DISPATCH-MACRO-CHARACTER",lisp)
LISPSYM(get_dispatch_macro_character,"GET-DISPATCH-MACRO-CHARACTER",lisp)
LISPSYM(readtable_case,"READTABLE-CASE",lisp)
LISPSYM(set_readtable_case,"SET-READTABLE-CASE",system) /* ABI */
LISPSYM(lpar_reader,"LPAR-READER",system)
LISPSYM(rpar_reader,"RPAR-READER",system)
LISPSYM(quote_reader,"QUOTE-READER",system)
LISPSYM(function_reader,"FUNCTION-READER",system)
LISPSYM(string_reader,"STRING-READER",system)
LISPSYM(line_comment_reader,"LINE-COMMENT-READER",system)
LISPSYM(comment_reader,"COMMENT-READER",system)
LISPSYM(char_reader,"CHAR-READER",system)
LISPSYM(binary_reader,"BINARY-READER",system)
LISPSYM(octal_reader,"OCTAL-READER",system)
LISPSYM(hexadecimal_reader,"HEXADECIMAL-READER",system)
LISPSYM(radix_reader,"RADIX-READER",system)
LISPSYM(complex_reader,"COMPLEX-READER",system)
LISPSYM(uninterned_reader,"UNINTERNED-READER",system)
LISPSYM(bit_vector_reader,"BIT-VECTOR-READER",system)
LISPSYM(vector_reader,"VECTOR-READER",system)
LISPSYM(array_reader,"ARRAY-READER",system)
LISPSYM(read_eval_reader,"READ-EVAL-READER",system)
LISPSYM(load_eval_reader,"LOAD-EVAL-READER",system)
LISPSYM(label_definition_reader,"LABEL-DEFINIION-READER",system)
LISPSYM(label_reference_reader,"LABEL-REFERENCE-READER",system)
LISPSYM(not_readable_reader,"NOT-READABLE-READER",system)
LISPSYM(syntax_error_reader,"SYNTAX-ERROR-READER",system)
LISPSYM(featurep,"FEATUREP",ext)
LISPSYM(feature_reader,"FEATURE-READER",system)
LISPSYM(not_feature_reader,"NOT-FEATURE-READER",system)
LISPSYM(structure_reader,"STRUCTURE-READER",system)
LISPSYM(closure_reader,"CLOSURE-READER",system)
LISPSYM(clisp_pathname_reader,"CLISP-PATHNAME-READER",system)
LISPSYM(ansi_pathname_reader,"ANSI-PATHNAME-READER",system)
#if defined(UNIX) || defined(WIN32_NATIVE)
LISPSYM(unix_executable_reader,"UNIX-EXECUTABLE-READER",system)
#endif
LISPSYM(read,"READ",lisp)
LISPSYM(read_preserving_whitespace,"READ-PRESERVING-WHITESPACE",lisp)
LISPSYM(read_delimited_list,"READ-DELIMITED-LIST",lisp)
LISPSYM(read_line,"READ-LINE",lisp)
LISPSYM(read_char,"READ-CHAR",lisp)
LISPSYM(unread_char,"UNREAD-CHAR",lisp)
LISPSYM(peek_char,"PEEK-CHAR",lisp)
LISPSYM(listen,"LISTEN",lisp)
LISPSYM(read_char_will_hang_p,"READ-CHAR-WILL-HANG-P",ext)
LISPSYM(read_char_no_hang,"READ-CHAR-NO-HANG",lisp)
LISPSYM(clear_input,"CLEAR-INPUT",lisp)
LISPSYM(read_from_string,"READ-FROM-STRING",lisp)
LISPSYM(parse_integer,"PARSE-INTEGER",lisp)
LISPSYM(print_structure,"PRINT-STRUCTURE",system) /* ABI */
LISPSYM(write,"WRITE",lisp)
LISPSYM(prin1,"PRIN1",lisp)
LISPSYM(print,"PRINT",lisp)
LISPSYM(pprint,"PPRINT",lisp)
LISPSYM(pprint_dispatch,"PPRINT-DISPATCH",lisp) /* used in io.d */
LISPSYM(pprint_indent,"PPRINT-INDENT",lisp)
LISPSYM(pprint_newline,"PPRINT-NEWLINE",lisp)
LISPSYM(ppprint_logical_block,"%PPRINT-LOGICAL-BLOCK",system) /* ABI */
LISPSYM(pcirclep,"%CIRCLEP",system) /* for PPRINT-POP */
LISPSYM(princ,"PRINC",lisp)
LISPSYM(write_to_string,"WRITE-TO-STRING",lisp)
LISPSYM(prin1_to_string,"PRIN1-TO-STRING",lisp)
LISPSYM(princ_to_string,"PRINC-TO-STRING",lisp)
LISPSYM(write_char,"WRITE-CHAR",lisp)
LISPSYM(write_string,"WRITE-STRING",lisp)
LISPSYM(write_line,"WRITE-LINE",lisp)
LISPSYM(terpri,"TERPRI",lisp)
LISPSYM(fresh_line,"FRESH-LINE",lisp)
LISPSYM(elastic_newline,"ELASTIC-NEWLINE",ext)
LISPSYM(finish_output,"FINISH-OUTPUT",lisp)
LISPSYM(force_output,"FORCE-OUTPUT",lisp)
LISPSYM(clear_output,"CLEAR-OUTPUT",lisp)
LISPSYM(write_unreadable,"WRITE-UNREADABLE",system) /* ABI */
LISPSYM(line_position,"LINE-POSITION",system) /* ABI */
LISPSYM(whitespacep,"WHITESPACEP",system)
LISPSYM(write_spaces,"WRITE-SPACES",system)
/* ---------- LIST ---------- */
LISPSYM(car,"CAR",lisp)
LISPSYM(cdr,"CDR",lisp)
LISPSYM(caar,"CAAR",lisp)
LISPSYM(cadr,"CADR",lisp)
LISPSYM(cdar,"CDAR",lisp)
LISPSYM(cddr,"CDDR",lisp)
LISPSYM(caaar,"CAAAR",lisp)
LISPSYM(caadr,"CAADR",lisp)
LISPSYM(cadar,"CADAR",lisp)
LISPSYM(caddr,"CADDR",lisp)
LISPSYM(cdaar,"CDAAR",lisp)
LISPSYM(cdadr,"CDADR",lisp)
LISPSYM(cddar,"CDDAR",lisp)
LISPSYM(cdddr,"CDDDR",lisp)
LISPSYM(caaaar,"CAAAAR",lisp)
LISPSYM(caaadr,"CAAADR",lisp)
LISPSYM(caadar,"CAADAR",lisp)
LISPSYM(caaddr,"CAADDR",lisp)
LISPSYM(cadaar,"CADAAR",lisp)
LISPSYM(cadadr,"CADADR",lisp)
LISPSYM(caddar,"CADDAR",lisp)
LISPSYM(cadddr,"CADDDR",lisp)
LISPSYM(cdaaar,"CDAAAR",lisp)
LISPSYM(cdaadr,"CDAADR",lisp)
LISPSYM(cdadar,"CDADAR",lisp)
LISPSYM(cdaddr,"CDADDR",lisp)
LISPSYM(cddaar,"CDDAAR",lisp)
LISPSYM(cddadr,"CDDADR",lisp)
LISPSYM(cdddar,"CDDDAR",lisp)
LISPSYM(cddddr,"CDDDDR",lisp)
LISPSYM(cons,"CONS",lisp)
LISPSYM(tree_equal,"TREE-EQUAL",lisp)
LISPSYM(endp,"ENDP",lisp)
LISPSYM(list_length,"LIST-LENGTH",lisp)
LISPSYM(list_length_dotted,"LIST-LENGTH-DOTTED",ext)
LISPSYM(list_length_proper,"LIST-LENGTH-PROPER",ext)
LISPSYM(nth,"NTH",lisp)
LISPSYM(first,"FIRST",lisp)
LISPSYM(second,"SECOND",lisp)
LISPSYM(third,"THIRD",lisp)
LISPSYM(fourth,"FOURTH",lisp)
LISPSYM(fifth,"FIFTH",lisp)
LISPSYM(sixth,"SIXTH",lisp)
LISPSYM(seventh,"SEVENTH",lisp)
LISPSYM(eighth,"EIGHTH",lisp)
LISPSYM(ninth,"NINTH",lisp)
LISPSYM(tenth,"TENTH",lisp)
LISPSYM(rest,"REST",lisp)
LISPSYM(nthcdr,"NTHCDR",lisp)
LISPSYM(conses_p,"CONSES-P",system) /* ABI */
LISPSYM(last,"LAST",lisp)
LISPSYM(list,"LIST",lisp)
LISPSYM(liststern,"LIST*",lisp)
LISPSYM(make_list,"MAKE-LIST",lisp)
LISPSYM(append,"APPEND",lisp)
LISPSYM(copy_list,"COPY-LIST",lisp)
LISPSYM(copy_alist,"COPY-ALIST",lisp)
LISPSYM(copy_tree,"COPY-TREE",lisp)
LISPSYM(revappend,"REVAPPEND",lisp)
LISPSYM(nconc,"NCONC",lisp)
LISPSYM(nreconc,"NRECONC",lisp)
LISPSYM(list_nreverse,"LIST-NREVERSE",system) /* ABI */
LISPSYM(butlast,"BUTLAST",lisp)
LISPSYM(nbutlast,"NBUTLAST",lisp)
LISPSYM(ldiff,"LDIFF",lisp)
LISPSYM(rplaca,"RPLACA",lisp)
LISPSYM(prplaca,"%RPLACA",system) /* ABI */
LISPSYM(rplacd,"RPLACD",lisp)
LISPSYM(prplacd,"%RPLACD",system) /* ABI */
LISPSYM(subst,"SUBST",lisp)
LISPSYM(subst_if,"SUBST-IF",lisp)
LISPSYM(subst_if_not,"SUBST-IF-NOT",lisp)
LISPSYM(nsubst,"NSUBST",lisp)
LISPSYM(nsubst_if,"NSUBST-IF",lisp)
LISPSYM(nsubst_if_not,"NSUBST-IF-NOT",lisp)
LISPSYM(sublis,"SUBLIS",lisp)
LISPSYM(nsublis,"NSUBLIS",lisp)
LISPSYM(memq,"MEMQ",system) /* ABI */
LISPSYM(member,"MEMBER",lisp)
LISPSYM(member_if,"MEMBER-IF",lisp)
LISPSYM(member_if_not,"MEMBER-IF-NOT",lisp)
LISPSYM(tailp,"TAILP",lisp)
LISPSYM(adjoin,"ADJOIN",lisp)
LISPSYM(acons,"ACONS",lisp)
LISPSYM(pairlis,"PAIRLIS",lisp)
LISPSYM(assoc,"ASSOC",lisp)
LISPSYM(assoc_if,"ASSOC-IF",lisp)
LISPSYM(assoc_if_not,"ASSOC-IF-NOT",lisp)
LISPSYM(rassoc,"RASSOC",lisp)
LISPSYM(rassoc_if,"RASSOC-IF",lisp)
LISPSYM(rassoc_if_not,"RASSOC-IF-NOT",lisp)
LISPSYM(list_upd,"LIST-UPD",system)
LISPSYM(list_endtest,"LIST-ENDTEST",system)
LISPSYM(list_fe_init,"LIST-FE-INIT",system)
LISPSYM(list_access,"LIST-ACCESS",system)
LISPSYM(list_access_set,"LIST-ACCESS-SET",system)
LISPSYM(list_elt,"LIST-ELT",system)
LISPSYM(list_set_elt,"LIST-SET-ELT",system)
LISPSYM(list_init_start,"LIST-INIT-START",system)
LISPSYM(list_fe_init_end,"LIST-FE-INIT-END",system)
/* ---------- MISC ---------- */
LISPSYM(lisp_implementation_type,"LISP-IMPLEMENTATION-TYPE",lisp)
LISPSYM(lisp_implementation_version,"LISP-IMPLEMENTATION-VERSION",lisp)
LISPSYM(version,"VERSION",system)
#ifdef MACHINE_KNOWN
LISPSYM(machinetype,"MACHINE-TYPE",lisp)
LISPSYM(machine_version,"MACHINE-VERSION",lisp)
#endif
#ifdef HAVE_ENVIRONMENT
LISPSYM(get_env,"GETENV",ext)
LISPSYM(set_env,"SETENV",system)
#endif
#ifdef WIN32_NATIVE
LISPSYM(registry,"REGISTRY",system)
#endif
LISPSYM(software_type,"SOFTWARE-TYPE",lisp)
LISPSYM(software_version,"SOFTWARE-VERSION",lisp)
LISPSYM(identity,"IDENTITY",lisp)
LISPSYM(address_of,"ADDRESS-OF",system)
LISPSYM(code_address_of,"CODE-ADDRESS-OF",system)
LISPSYM(process_id,"PROCESS-ID",system)
LISPSYM(ansi,"ANSI",system) /* ABI */
LISPSYM(set_ansi,"SET-ANSI",system) /* ABI */
LISPSYM(module_info,"MODULE-INFO",ext)
#if defined(DYNAMIC_FFI) && (defined(WIN32_NATIVE) || defined(HAVE_DLOPEN))
LISPSYM(Kffi,"FFI",keyword)
#endif
LISPSYM(argv,"ARGV",ext)
/* ---------- I18N ---------- */
LISPSYM(current_language,"CURRENT-LANGUAGE",system) /* ABI */
LISPSYM(set_current_language,"SET-CURRENT-LANGUAGE",system) /* ABI */
LISPSYM(text,"TEXT",system)
/* ---------- SOCKET ---------- */
#ifdef MACHINE_KNOWN
LISPSYM(machine_instance,"MACHINE-INSTANCE",lisp)
#endif
/* ---------- TIME ---------- */
LISPSYM(get_internal_real_time,"GET-INTERNAL-REAL-TIME",lisp)
LISPSYM(get_internal_run_time,"GET-INTERNAL-RUN-TIME",lisp)
LISPSYM(get_universal_time,"GET-UNIVERSAL-TIME",lisp)
#if defined(UNIX) || defined(WIN32)
LISPSYM(default_time_zone,"DEFAULT-TIME-ZONE",system)
#endif
LISPSYM(sleep,"%SLEEP",system)
LISPSYM(time,"%%TIME",system) /* ABI */
LISPSYM(delta4,"DELTA4",system)
/* ---------- PACKAGE ---------- */
LISPSYM(make_symbol,"MAKE-SYMBOL",lisp)
LISPSYM(find_package,"FIND-PACKAGE",lisp)
LISPSYM(pfind_package,"%FIND-PACKAGE",system) /* ABI */
LISPSYM(package_name,"PACKAGE-NAME",lisp)
LISPSYM(package_nicknames,"PACKAGE-NICKNAMES",lisp)
LISPSYM(rename_package,"RENAME-PACKAGE",lisp)
LISPSYM(package_use_list,"PACKAGE-USE-LIST",lisp)
LISPSYM(package_used_by_list,"PACKAGE-USED-BY-LIST",lisp)
LISPSYM(package_shadowing_symbols,"PACKAGE-SHADOWING-SYMBOLS",lisp)
LISPSYM(package_lock,"PACKAGE-LOCK",ext)
LISPSYM(package_shortest_name,"PACKAGE-SHORTEST-NAME",ext)
LISPSYM(package_case_sensitive_p,"PACKAGE-CASE-SENSITIVE-P",ext)
LISPSYM(package_case_inverted_p,"PACKAGE-CASE-INVERTED-P",ext)
LISPSYM(package_documentation,"PACKAGE-DOCUMENTATION",system) /* ABI */
LISPSYM(set_package_documentation,"(SETF PACKAGE-DOCUMENTATION)",system) /* ABI */
LISPSYM(set_package_lock,"(SETF PACKAGE-LOCK)",system) /* ABI */
LISPSYM(set_package_case_inverted_p,"(SETF PACKAGE-CASE-INVERTED-P)",system)
LISPSYM(set_package_case_sensitive_p,"(SETF PACKAGE-CASE-SENSITIVE-P)",system)
LISPSYM(symbol_value_lock,"SYMBOL-VALUE-LOCK",system)
LISPSYM(check_package_lock,"CHECK-PACKAGE-LOCK",system)
LISPSYM(list_all_packages,"LIST-ALL-PACKAGES",lisp)
LISPSYM(intern,"INTERN",lisp)
LISPSYM(cs_intern,"INTERN",cs_lisp)
LISPSYM(find_symbol,"FIND-SYMBOL",lisp)
LISPSYM(cs_find_symbol,"FIND-SYMBOL",cs_lisp)
LISPSYM(unintern,"UNINTERN",lisp)
LISPSYM(export,"EXPORT",lisp)
LISPSYM(unexport,"UNEXPORT",lisp)
LISPSYM(re_export,"RE-EXPORT",ext)
LISPSYM(import,"IMPORT",lisp)
LISPSYM(shadowing_import,"SHADOWING-IMPORT",lisp)
LISPSYM(shadow,"SHADOW",lisp)
LISPSYM(cs_shadow,"SHADOW",cs_lisp)
LISPSYM(use_package,"USE-PACKAGE",lisp)
LISPSYM(unuse_package,"UNUSE-PACKAGE",lisp)
LISPSYM(make_package,"MAKE-PACKAGE",lisp)
LISPSYM(cs_make_package,"MAKE-PACKAGE",cs_lisp)
LISPSYM(pin_package,"%IN-PACKAGE",system) /* ABI */
/* LISPSYM(in_package,"IN-PACKAGE",lisp) */
LISPSYM(delete_package,"DELETE-PACKAGE",lisp)
LISPSYM(find_all_symbols,"FIND-ALL-SYMBOLS",lisp)
LISPSYM(cs_find_all_symbols,"FIND-ALL-SYMBOLS",cs_lisp)
LISPSYM(map_symbols,"MAP-SYMBOLS",system) /* ABI */
LISPSYM(map_external_symbols,"MAP-EXTERNAL-SYMBOLS",system) /* ABI */
LISPSYM(map_all_symbols,"MAP-ALL-SYMBOLS",system) /* ABI */
LISPSYM(package_iterator,"PACKAGE-ITERATOR",system) /* ABI */
LISPSYM(package_iterate,"PACKAGE-ITERATE",system) /* ABI */
/* ---------- PATHNAME ---------- */
LISPSYM(parse_namestring,"PARSE-NAMESTRING",lisp)
LISPSYM(pathname,"PATHNAME",lisp)
LISPSYM(pathnamehost,"PATHNAME-HOST",lisp)
LISPSYM(pathnamedevice,"PATHNAME-DEVICE",lisp)
LISPSYM(pathnamedirectory,"PATHNAME-DIRECTORY",lisp)
LISPSYM(pathnamename,"PATHNAME-NAME",lisp)
LISPSYM(pathnametype,"PATHNAME-TYPE",lisp)
LISPSYM(pathnameversion,"PATHNAME-VERSION",lisp)
#ifdef LOGICAL_PATHNAMES
LISPSYM(logical_pathname,"LOGICAL-PATHNAME",lisp)
LISPSYM(translate_logical_pathname,"TRANSLATE-LOGICAL-PATHNAME",lisp)
#endif
LISPSYM(file_namestring,"FILE-NAMESTRING",lisp)
LISPSYM(directory_namestring,"DIRECTORY-NAMESTRING",lisp)
LISPSYM(host_namestring,"HOST-NAMESTRING",lisp)
LISPSYM(merge_pathnames,"MERGE-PATHNAMES",lisp)
LISPSYM(enough_namestring,"ENOUGH-NAMESTRING",lisp)
LISPSYM(make_pathname,"MAKE-PATHNAME",lisp)
#ifdef LOGICAL_PATHNAMES
LISPSYM(make_logical_pathname,"MAKE-LOGICAL-PATHNAME",system)
#endif
#ifdef USER_HOMEDIR
LISPSYM(user_homedir_pathname,"USER-HOMEDIR-PATHNAME",lisp)
#endif
LISPSYM(wild_pathname_p,"WILD-PATHNAME-P",lisp)
LISPSYM(pathname_match_p,"PATHNAME-MATCH-P",lisp)
LISPSYM(translate_pathname,"TRANSLATE-PATHNAME",lisp)
LISPSYM(absolute_pathname,"ABSOLUTE-PATHNAME",ext)
LISPSYM(namestring,"NAMESTRING",lisp)
LISPSYM(truename,"TRUENAME",lisp)
LISPSYM(probe_file,"PROBE-FILE",lisp)
LISPSYM(probe_directory,"PROBE-DIRECTORY",ext)
LISPSYM(delete_file,"DELETE-FILE",lisp)
LISPSYM(rename_file,"RENAME-FILE",lisp)
LISPSYM(file_error,"FILE-ERROR",lisp)
LISPSYM(open,"OPEN",lisp)
LISPSYM(directory,"DIRECTORY",lisp)
LISPSYM(cd,"CD",ext)
LISPSYM(make_dir,"MAKE-DIR",ext)
LISPSYM(delete_dir,"DELETE-DIR",ext)
LISPSYM(ensure_directories_exist,"ENSURE-DIRECTORIES-EXIST",lisp)
LISPSYM(file_write_date,"FILE-WRITE-DATE",lisp)
LISPSYM(file_author,"FILE-AUTHOR",lisp)
#ifdef UNIX
LISPSYM(execute,"EXECUTE",ext)
#endif
#ifdef HAVE_SHELL
#ifdef WIN32_NATIVE
LISPSYM(shell_name,"SHELL-NAME",system)
#endif
LISPSYM(shell,"SHELL",ext)
#endif
#if defined(UNIX) || defined(WIN32_NATIVE)
LISPSYM(launch,"LAUNCH",ext)
#endif
#if defined(WIN32_NATIVE)
LISPSYM(shell_execute,"SHELL-EXECUTE",ext)
#endif
LISPSYM(savemem,"SAVEMEM",system)
#if defined(WIN32_NATIVE) || defined(UNIX_CYGWIN32)
LISPSYM(device_prefix,"*DEVICE-PREFIX*",custom)
#endif
#ifdef DYNAMIC_MODULES
LISPSYM(dynload_modules,"DYNLOAD-MODULES",system)
#endif
LISPSYM(program_name,"PROGRAM-NAME",system)
LISPSYM(lib_directory,"LIB-DIRECTORY",system) /* ABI */
LISPSYM(set_lib_directory,"SET-LIB-DIRECTORY",system) /* ABI */
/* ---------- PREDTYPE ---------- */
LISPSYM(eq,"EQ",lisp)
LISPSYM(eql,"EQL",lisp)
LISPSYM(equal,"EQUAL",lisp)
LISPSYM(equalp,"EQUALP",lisp)
LISPSYM(consp,"CONSP",lisp)
LISPSYM(atom,"ATOM",lisp)
LISPSYM(symbolp,"SYMBOLP",lisp)
LISPSYM(stringp,"STRINGP",lisp)
LISPSYM(numberp,"NUMBERP",lisp)
LISPSYM(compiled_function_p,"COMPILED-FUNCTION-P",lisp)
LISPSYM(pcompiled_function_p,"%COMPILED-FUNCTION-P",system)
LISPSYM(null,"NULL",lisp)
LISPSYM(not,"NOT",lisp)
LISPSYM(closurep,"CLOSUREP",system)
LISPSYM(listp,"LISTP",lisp)
LISPSYM(proper_list_p,"PROPER-LIST-P",ext)
LISPSYM(integerp,"INTEGERP",lisp)
LISPSYM(fixnump,"FIXNUMP",system) /* ABI */
LISPSYM(rationalp,"RATIONALP",lisp)
LISPSYM(floatp,"FLOATP",lisp)
LISPSYM(short_float_p,"SHORT-FLOAT-P",system) /* ABI */
LISPSYM(single_float_p,"SINGLE-FLOAT-P",system) /* ABI */
LISPSYM(double_float_p,"DOUBLE-FLOAT-P",system) /* ABI */
LISPSYM(long_float_p,"LONG-FLOAT-P",system) /* ABI */
LISPSYM(realp,"REALP",lisp)
LISPSYM(complexp,"COMPLEXP",lisp)
LISPSYM(streamp,"STREAMP",lisp)
LISPSYM(built_in_stream_p,"BUILT-IN-STREAM-P",system)
LISPSYM(random_state_p,"RANDOM-STATE-P",lisp)
LISPSYM(readtablep,"READTABLEP",lisp)
LISPSYM(hash_table_p,"HASH-TABLE-P",lisp)
LISPSYM(pathnamep,"PATHNAMEP",lisp)
LISPSYM(logical_pathname_p,"LOGICAL-PATHNAME-P",system) /* ABI */
LISPSYM(characterp,"CHARACTERP",lisp)
LISPSYM(functionp,"FUNCTIONP",lisp)
LISPSYM(packagep,"PACKAGEP",lisp)
LISPSYM(arrayp,"ARRAYP",lisp)
LISPSYM(simple_array_p,"SIMPLE-ARRAY-P",system) /* ABI */
LISPSYM(bit_vector_p,"BIT-VECTOR-P",lisp)
LISPSYM(vectorp,"VECTORP",lisp)
LISPSYM(simple_vector_p,"SIMPLE-VECTOR-P",lisp)
LISPSYM(simple_string_p,"SIMPLE-STRING-P",lisp)
LISPSYM(simple_bit_vector_p,"SIMPLE-BIT-VECTOR-P",lisp)
LISPSYM(type_of,"TYPE-OF",lisp)
LISPSYM(defclos,"%DEFCLOS",clos)
LISPSYM(potential_class_p,"POTENTIAL-CLASS-P",clos) /* ABI */
LISPSYM(defined_class_p,"DEFINED-CLASS-P",clos) /* ABI */
LISPSYM(class_of,"CLASS-OF",clos)
LISPSYM(find_class,"FIND-CLASS",clos)
LISPSYM(typep_class,"TYPEP-CLASS",clos) /* ABI */
LISPSYM(expand_deftype,"EXPAND-DEFTYPE",system)
LISPSYM(coerce,"COERCE",lisp)
LISPSYM(note_new_structure_class,"NOTE-NEW-STRUCTURE-CLASS",system)
LISPSYM(note_new_standard_class,"NOTE-NEW-STANDARD-CLASS",system)
LISPSYM(heap_statistics,"HEAP-STATISTICS",system)
LISPSYM(gc_statistics,"GC-STATISTICS",system) /* ABI */
LISPSYM(list_statistics,"LIST-STATISTICS",system)
LISPSYM(heap_statistics_statistics,"HEAP-STATISTICS-STATISTICS",system)
LISPSYM(gc_statistics_statistics,"GC-STATISTICS-STATISTICS",system)
/* ---------- RECORD ---------- */
LISPSYM(record_ref,"%RECORD-REF",system)
LISPSYM(record_store,"%RECORD-STORE",system)
LISPSYM(record_length,"%RECORD-LENGTH",system)
LISPSYM(pstructure_ref,"%%STRUCTURE-REF",system)
LISPSYM(structure_ref,"%STRUCTURE-REF",system) /* ABI */
LISPSYM(structure_store,"%STRUCTURE-STORE",system) /* ABI */
LISPSYM(make_structure,"%MAKE-STRUCTURE",system) /* ABI */
LISPSYM(copy_structure,"COPY-STRUCTURE",lisp)
LISPSYM(structure_type_p,"%STRUCTURE-TYPE-P",system) /* ABI */
LISPSYM(closure_name,"CLOSURE-NAME",system)
LISPSYM(set_closure_name,"(SETF CLOSURE-NAME)",system)
LISPSYM(closure_codevec,"CLOSURE-CODEVEC",system)
LISPSYM(closure_consts,"CLOSURE-CONSTS",system)
LISPSYM(make_code_vector,"MAKE-CODE-VECTOR",system)
LISPSYM(make_closure,"%MAKE-CLOSURE",system)
LISPSYM(make_constant_initfunction,"MAKE-CONSTANT-INITFUNCTION",system) /* ABI */
LISPSYM(constant_initfunction_p,"CONSTANT-INITFUNCTION-P",system)
LISPSYM(closure_set_seclass,"CLOSURE-SET-SECLASS",system)
LISPSYM(set_funcallable_instance_function,"SET-FUNCALLABLE-INSTANCE-FUNCTION",clos)
LISPSYM(copy_generic_function,"%COPY-GENERIC-FUNCTION",system) /* ABI */
LISPSYM(generic_function_effective_method_function,"GENERIC-FUNCTION-EFFECTIVE-METHOD-FUNCTION",system)
LISPSYM(make_load_time_eval,"MAKE-LOAD-TIME-EVAL",system)
LISPSYM(make_symbol_macro,"MAKE-SYMBOL-MACRO",system) /* ABI */
LISPSYM(symbol_macro_p,"SYMBOL-MACRO-P",system)
LISPSYM(symbol_macro_expand,"SYMBOL-MACRO-EXPAND",ext)
LISPSYM(make_global_symbol_macro,"MAKE-GLOBAL-SYMBOL-MACRO",system)
LISPSYM(global_symbol_macro_definition,"GLOBAL-SYMBOL-MACRO-DEFINITION",system)
LISPSYM(make_macro,"MAKE-MACRO",system) /* ABI */
LISPSYM(macrop,"MACROP",system)
LISPSYM(macro_expander,"MACRO-EXPANDER",system)
LISPSYM(make_function_macro,"MAKE-FUNCTION-MACRO",system)
LISPSYM(function_macro_p,"FUNCTION-MACRO-P",system)
LISPSYM(function_macro_function,"FUNCTION-MACRO-FUNCTION",system) /* ABI */
LISPSYM(function_macro_expander,"FUNCTION-MACRO-EXPANDER",system)
LISPSYM(finalize,"FINALIZE",ext)
LISPSYM(structure_object_p,"STRUCTURE-OBJECT-P",clos) /* ABI */
LISPSYM(std_instance_p,"STD-INSTANCE-P",clos) /* ABI */
LISPSYM(funcallable_instance_p,"FUNCALLABLE-INSTANCE-P",clos)
LISPSYM(allocate_metaobject_instance,"ALLOCATE-METAOBJECT-INSTANCE",clos)
LISPSYM(allocate_std_instance,"ALLOCATE-STD-INSTANCE",clos)
LISPSYM(allocate_funcallable_instance,"ALLOCATE-FUNCALLABLE-INSTANCE",clos)
LISPSYM(pallocate_instance,"%ALLOCATE-INSTANCE",clos)
LISPSYM(pslot_value_using_class,"%SLOT-VALUE-USING-CLASS",clos)
LISPSYM(pset_slot_value_using_class,"%SET-SLOT-VALUE-USING-CLASS",clos)
LISPSYM(pslot_boundp_using_class,"%SLOT-BOUNDP-USING-CLASS",clos)
LISPSYM(pslot_makunbound_using_class,"%SLOT-MAKUNBOUND-USING-CLASS",clos)
LISPSYM(slot_value,"SLOT-VALUE",clos)
LISPSYM(set_slot_value,"SET-SLOT-VALUE",clos)
LISPSYM(slot_boundp,"SLOT-BOUNDP",clos)
LISPSYM(slot_makunbound,"SLOT-MAKUNBOUND",clos)
LISPSYM(slot_exists_p,"SLOT-EXISTS-P",clos)
LISPSYM(standard_instance_access,"STANDARD-INSTANCE-ACCESS",clos)
LISPSYM(set_standard_instance_access,"(SETF STANDARD-INSTANCE-ACCESS)",clos)
LISPSYM(punbound,"%UNBOUND",system)
LISPSYM(pshared_initialize,"%SHARED-INITIALIZE",clos)
LISPSYM(preinitialize_instance,"%REINITIALIZE-INSTANCE",clos)
LISPSYM(pinitialize_instance,"%INITIALIZE-INSTANCE",clos)
LISPSYM(pmake_instance,"%MAKE-INSTANCE",clos)
LISPSYM(pchange_class,"%CHANGE-CLASS",clos)
/* ---------- WEAK ---------- */
LISPSYM(make_weak_pointer,"MAKE-WEAK-POINTER",ext)
LISPSYM(weak_pointer_p,"WEAK-POINTER-P",ext)
LISPSYM(weak_pointer_value,"WEAK-POINTER-VALUE",ext)
LISPSYM(set_weak_pointer_value,"(SETF WEAK-POINTER-VALUE)",system) /* ABI */
LISPSYM(make_weak_list,"MAKE-WEAK-LIST",ext)
LISPSYM(weak_list_p,"WEAK-LIST-P",ext)
LISPSYM(weak_list_list,"WEAK-LIST-LIST",ext)
LISPSYM(set_weak_list_list,"(SETF WEAK-LIST-LIST)",system) /* ABI */
LISPSYM(make_weak_and_relation,"MAKE-WEAK-AND-RELATION",ext)
LISPSYM(weak_and_relation_p,"WEAK-AND-RELATION-P",ext)
LISPSYM(weak_and_relation_list,"WEAK-AND-RELATION-LIST",ext)
LISPSYM(make_weak_or_relation,"MAKE-WEAK-OR-RELATION",ext)
LISPSYM(weak_or_relation_p,"WEAK-OR-RELATION-P",ext)
LISPSYM(weak_or_relation_list,"WEAK-OR-RELATION-LIST",ext)
LISPSYM(make_weak_mapping,"MAKE-WEAK-MAPPING",ext)
LISPSYM(weak_mapping_p,"WEAK-MAPPING-P",ext)
LISPSYM(weak_mapping_pair,"WEAK-MAPPING-PAIR",ext)
LISPSYM(weak_mapping_value,"WEAK-MAPPING-VALUE",ext)
LISPSYM(set_weak_mapping_value,"(SETF WEAK-MAPPING-VALUE)",system) /* ABI */
LISPSYM(make_weak_and_mapping,"MAKE-WEAK-AND-MAPPING",ext)
LISPSYM(weak_and_mapping_p,"WEAK-AND-MAPPING-P",ext)
LISPSYM(weak_and_mapping_pair,"WEAK-AND-MAPPING-PAIR",ext)
LISPSYM(weak_and_mapping_value,"WEAK-AND-MAPPING-VALUE",ext)
LISPSYM(set_weak_and_mapping_value,"(SETF WEAK-AND-MAPPING-VALUE)",system) /* ABI */
LISPSYM(make_weak_or_mapping,"MAKE-WEAK-OR-MAPPING",ext)
LISPSYM(weak_or_mapping_p,"WEAK-OR-MAPPING-P",ext)
LISPSYM(weak_or_mapping_pair,"WEAK-OR-MAPPING-PAIR",ext)
LISPSYM(weak_or_mapping_value,"WEAK-OR-MAPPING-VALUE",ext)
LISPSYM(set_weak_or_mapping_value,"(SETF WEAK-OR-MAPPING-VALUE)",system) /* ABI */
LISPSYM(make_weak_alist,"MAKE-WEAK-ALIST",ext)
LISPSYM(weak_alist_p,"WEAK-ALIST-P",ext)
LISPSYM(weak_alist_type,"WEAK-ALIST-TYPE",ext)
LISPSYM(weak_alist_contents,"WEAK-ALIST-CONTENTS",ext)
LISPSYM(set_weak_alist_contents,"(SETF WEAK-ALIST-CONTENTS)",system) /* ABI */
LISPSYM(weak_alist_assoc,"WEAK-ALIST-ASSOC",ext)
LISPSYM(weak_alist_rassoc,"WEAK-ALIST-RASSOC",ext)
LISPSYM(weak_alist_value,"WEAK-ALIST-VALUE",ext)
LISPSYM(set_weak_alist_value,"(SETF WEAK-ALIST-VALUE)",system) /* ABI */
/* ---------- SEQUENCE ---------- */
LISPSYM(sequencep,"SEQUENCEP",system) /* ABI */
LISPSYM(defseq,"%DEFSEQ",system)
LISPSYM(elt,"ELT",lisp)
LISPSYM(setelt,"(SETF ELT)",system) /* ABI */
LISPSYM(subseq,"SUBSEQ",lisp)
LISPSYM(copy_seq,"COPY-SEQ",lisp)
LISPSYM(length,"LENGTH",lisp)
LISPSYM(reverse,"REVERSE",lisp)
LISPSYM(nreverse,"NREVERSE",lisp)
LISPSYM(make_sequence,"MAKE-SEQUENCE",lisp)
LISPSYM(coerced_subseq,"COERCED-SUBSEQ",system)
LISPSYM(concatenate,"CONCATENATE",lisp)
LISPSYM(map,"MAP",lisp)
LISPSYM(map_into,"MAP-INTO",lisp)
LISPSYM(some,"SOME",lisp)
LISPSYM(every,"EVERY",lisp)
LISPSYM(notany,"NOTANY",lisp)
LISPSYM(notevery,"NOTEVERY",lisp)
LISPSYM(reduce,"REDUCE",lisp)
LISPSYM(fill,"FILL",lisp)
LISPSYM(replace,"REPLACE",lisp)
LISPSYM(remove,"REMOVE",lisp)
LISPSYM(remove_if,"REMOVE-IF",lisp)
LISPSYM(remove_if_not,"REMOVE-IF-NOT",lisp)
LISPSYM(delete,"DELETE",lisp)
LISPSYM(delete_if,"DELETE-IF",lisp)
LISPSYM(delete_if_not,"DELETE-IF-NOT",lisp)
LISPSYM(remove_duplicates,"REMOVE-DUPLICATES",lisp)
LISPSYM(delete_duplicates,"DELETE-DUPLICATES",lisp)
LISPSYM(substitute,"SUBSTITUTE",lisp)
LISPSYM(substitute_if,"SUBSTITUTE-IF",lisp)
LISPSYM(substitute_if_not,"SUBSTITUTE-IF-NOT",lisp)
LISPSYM(nsubstitute,"NSUBSTITUTE",lisp)
LISPSYM(nsubstitute_if,"NSUBSTITUTE-IF",lisp)
LISPSYM(nsubstitute_if_not,"NSUBSTITUTE-IF-NOT",lisp)
LISPSYM(find,"FIND",lisp)
LISPSYM(find_if,"FIND-IF",lisp)
LISPSYM(find_if_not,"FIND-IF-NOT",lisp)
LISPSYM(position,"POSITION",lisp)
LISPSYM(position_if,"POSITION-IF",lisp)
LISPSYM(position_if_not,"POSITION-IF-NOT",lisp)
LISPSYM(count,"COUNT",lisp)
LISPSYM(count_if,"COUNT-IF",lisp)
LISPSYM(count_if_not,"COUNT-IF-NOT",lisp)
LISPSYM(mismatch,"MISMATCH",lisp)
LISPSYM(search,"SEARCH",lisp)
LISPSYM(sort,"SORT",lisp)
LISPSYM(stable_sort,"STABLE-SORT",lisp)
LISPSYM(merge,"MERGE",lisp)
LISPSYM(read_char_sequence,"READ-CHAR-SEQUENCE",ext)
LISPSYM(write_char_sequence,"WRITE-CHAR-SEQUENCE",ext)
LISPSYM(read_byte_sequence,"READ-BYTE-SEQUENCE",ext)
LISPSYM(write_byte_sequence,"WRITE-BYTE-SEQUENCE",ext)
LISPSYM(sequence_count_ansi,"*SEQUENCE-COUNT-ANSI*",custom)
/* ---------- STREAM ---------- */
LISPSYM(symbol_stream,"SYMBOL-STREAM",system)
LISPSYM(make_synonym_stream,"MAKE-SYNONYM-STREAM",lisp)
LISPSYM(synonym_stream_p,"SYNONYM-STREAM-P",system) /* ABI */
LISPSYM(synonym_stream_symbol,"SYNONYM-STREAM-SYMBOL",lisp)
LISPSYM(make_broadcast_stream,"MAKE-BROADCAST-STREAM",lisp)
LISPSYM(broadcast_stream_p,"BROADCAST-STREAM-P",system) /* ABI */
LISPSYM(broadcast_stream_streams,"BROADCAST-STREAM-STREAMS",lisp)
LISPSYM(make_concatenated_stream,"MAKE-CONCATENATED-STREAM",lisp)
LISPSYM(concatenated_stream_p,"CONCATENATED-STREAM-P",system) /* ABI */
LISPSYM(concatenated_stream_streams,"CONCATENATED-STREAM-STREAMS",lisp)
LISPSYM(make_two_way_stream,"MAKE-TWO-WAY-STREAM",lisp)
LISPSYM(two_way_stream_p,"TWO-WAY-STREAM-P",system) /* ABI */
LISPSYM(two_way_stream_input_stream,"TWO-WAY-STREAM-INPUT-STREAM",lisp)
LISPSYM(two_way_stream_output_stream,"TWO-WAY-STREAM-OUTPUT-STREAM",lisp)
LISPSYM(make_echo_stream,"MAKE-ECHO-STREAM",lisp)
LISPSYM(echo_stream_p,"ECHO-STREAM-P",system) /* ABI */
LISPSYM(echo_stream_input_stream,"ECHO-STREAM-INPUT-STREAM",lisp)
LISPSYM(echo_stream_output_stream,"ECHO-STREAM-OUTPUT-STREAM",lisp)
LISPSYM(make_string_input_stream,"MAKE-STRING-INPUT-STREAM",lisp)
LISPSYM(string_input_stream_index,"STRING-INPUT-STREAM-INDEX",system) /* ABI */
LISPSYM(make_string_output_stream,"MAKE-STRING-OUTPUT-STREAM",lisp)
LISPSYM(get_output_stream_string,"GET-OUTPUT-STREAM-STRING",lisp)
LISPSYM(make_string_push_stream,"MAKE-STRING-PUSH-STREAM",system) /* ABI */
LISPSYM(string_stream_p,"STRING-STREAM-P",system) /* ABI */
LISPSYM(make_buffered_input_stream,"MAKE-BUFFERED-INPUT-STREAM",ext)
LISPSYM(buffered_input_stream_index,"BUFFERED-INPUT-STREAM-INDEX",system)
LISPSYM(make_buffered_output_stream,"MAKE-BUFFERED-OUTPUT-STREAM",ext)
#ifdef GENERIC_STREAMS
LISPSYM(generic_stream_controller,"GENERIC-STREAM-CONTROLLER",gstream)
LISPSYM(make_generic_stream,"MAKE-GENERIC-STREAM",gstream)
LISPSYM(generic_stream_p,"GENERIC-STREAM-P",gstream)
#endif
LISPSYM(file_stream_p,"FILE-STREAM-P",system) /* ABI */
#ifdef KEYBOARD
LISPSYM(make_keyboard_stream,"MAKE-KEYBOARD-STREAM",system)
#endif
LISPSYM(terminal_raw,"TERMINAL-RAW",system)
#ifdef SCREEN
LISPSYM(make_window,"MAKE-WINDOW",screen) /* ABI */
LISPSYM(window_size,"WINDOW-SIZE",screen)
LISPSYM(window_cursor_position,"WINDOW-CURSOR-POSITION",screen)
LISPSYM(set_window_cursor_position,"SET-WINDOW-CURSOR-POSITION",screen)
LISPSYM(clear_window,"CLEAR-WINDOW",screen)
LISPSYM(clear_window_to_eot,"CLEAR-WINDOW-TO-EOT",screen)
LISPSYM(clear_window_to_eol,"CLEAR-WINDOW-TO-EOL",screen)
LISPSYM(delete_window_line,"DELETE-WINDOW-LINE",screen)
LISPSYM(insert_window_line,"INSERT-WINDOW-LINE",screen)
LISPSYM(highlight_on,"HIGHLIGHT-ON",screen)
LISPSYM(highlight_off,"HIGHLIGHT-OFF",screen)
LISPSYM(window_cursor_on,"WINDOW-CURSOR-ON",screen)
LISPSYM(window_cursor_off,"WINDOW-CURSOR-OFF",screen)
#endif
#ifdef PIPES
LISPSYM(make_pipe_input_stream,"MAKE-PIPE-INPUT-STREAM",ext)
LISPSYM(make_pipe_output_stream,"MAKE-PIPE-OUTPUT-STREAM",ext)
#ifdef PIPES2
LISPSYM(make_pipe_io_stream,"MAKE-PIPE-IO-STREAM",ext)
#endif
#endif
#ifdef X11SOCKETS
LISPSYM(make_x11socket_stream,"MAKE-SOCKET-STREAM",system)
LISPSYM(read_n_bytes,"READ-N-BYTES",system)
LISPSYM(write_n_bytes,"WRITE-N-BYTES",system)
LISPSYM(x11_socket_stream,"X11-SOCKET-STREAM",system)
#endif
#ifdef SOCKET_STREAMS
LISPSYM(socket_server,"SOCKET-SERVER",socket)
LISPSYM(socket_server_close,"SOCKET-SERVER-CLOSE",socket)
LISPSYM(socket_server_port,"SOCKET-SERVER-PORT",socket)
LISPSYM(socket_server_host,"SOCKET-SERVER-HOST",socket)
LISPSYM(socket_accept,"SOCKET-ACCEPT",socket)
LISPSYM(socket_wait,"SOCKET-WAIT",socket)
LISPSYM(socket_status,"SOCKET-STATUS",socket)
LISPSYM(socket_connect,"SOCKET-CONNECT",socket)
LISPSYM(socket_stream_port,"SOCKET-STREAM-PORT",socket)
LISPSYM(socket_stream_host,"SOCKET-STREAM-HOST",socket)
LISPSYM(socket_stream_peer,"SOCKET-STREAM-PEER",socket)
LISPSYM(socket_stream_local,"SOCKET-STREAM-LOCAL",socket)
LISPSYM(socket_options,"SOCKET-OPTIONS",socket)
#ifdef HAVE_SHUTDOWN
LISPSYM(socket_stream_shutdown,"SOCKET-STREAM-SHUTDOWN",socket)
#endif
LISPSYM(make_stream,"MAKE-STREAM",ext)
LISPSYM(stream_handles,"STREAM-HANDLES",socket)
#endif
LISPSYM(built_in_stream_open_p,"BUILT-IN-STREAM-OPEN-P",system)
LISPSYM(input_stream_p,"INPUT-STREAM-P",lisp)
LISPSYM(output_stream_p,"OUTPUT-STREAM-P",lisp)
LISPSYM(stream_element_type_eq,"STREAM-ELEMENT-TYPE-EQ",system)
LISPSYM(built_in_stream_element_type,"BUILT-IN-STREAM-ELEMENT-TYPE",system)
LISPSYM(built_in_stream_set_element_type,"BUILT-IN-STREAM-SET-ELEMENT-TYPE",system)
LISPSYM(stream_external_format,"STREAM-EXTERNAL-FORMAT",lisp)
LISPSYM(set_stream_external_format,"SET-STREAM-EXTERNAL-FORMAT",system) /* ABI */
LISPSYM(interactive_stream_p,"INTERACTIVE-STREAM-P",lisp)
LISPSYM(built_in_stream_close,"BUILT-IN-STREAM-CLOSE",system)
LISPSYM(read_byte,"READ-BYTE",lisp)
LISPSYM(read_byte_lookahead,"READ-BYTE-LOOKAHEAD",ext)
LISPSYM(read_byte_will_hang_p,"READ-BYTE-WILL-HANG-P",ext)
LISPSYM(read_byte_no_hang,"READ-BYTE-NO-HANG",ext)
LISPSYM(read_integer,"READ-INTEGER",ext)
LISPSYM(read_float,"READ-FLOAT",ext)
LISPSYM(write_byte,"WRITE-BYTE",lisp)
LISPSYM(write_integer,"WRITE-INTEGER",ext)
LISPSYM(write_float,"WRITE-FLOAT",ext)
LISPSYM(file_position,"FILE-POSITION",lisp)
LISPSYM(file_length,"FILE-LENGTH",lisp)
LISPSYM(file_string_length,"FILE-STRING-LENGTH",lisp)
LISPSYM(line_number,"LINE-NUMBER",system)
LISPSYM(allow_read_eval,"ALLOW-READ-EVAL",system)
LISPSYM(defgray,"%DEFGRAY",system)
/* ---------- SYMBOL ---------- */
LISPSYM(putd,"%PUTD",system) /* ABI */
LISPSYM(find_subr,"%FIND-SUBR",system)
LISPSYM(proclaim_constant,"%PROCLAIM-CONSTANT",system) /* ABI */
LISPSYM(proclaim_symbol_macro,"%PROCLAIM-SYMBOL-MACRO",system) /* ABI */
LISPSYM(get,"GET",lisp)
LISPSYM(getf,"GETF",lisp)
LISPSYM(putf,"%PUTF",system) /* ABI */
LISPSYM(remf,"%REMF",system) /* ABI */
LISPSYM(get_properties,"GET-PROPERTIES",lisp)
LISPSYM(putplist,"%PUTPLIST",system) /* ABI */
LISPSYM(put,"%PUT",system) /* ABI */
LISPSYM(remprop,"REMPROP",lisp)
LISPSYM(symbol_package,"SYMBOL-PACKAGE",lisp)
LISPSYM(symbol_plist,"SYMBOL-PLIST",lisp)
LISPSYM(symbol_name,"SYMBOL-NAME",lisp)
LISPSYM(cs_symbol_name,"SYMBOL-NAME",cs_lisp)
LISPSYM(keywordp,"KEYWORDP",lisp)
LISPSYM(special_variable_p,"SPECIAL-VARIABLE-P",ext)
LISPSYM(gensym,"GENSYM",lisp)
LISPSYM(plist,"PLIST",system) /* type in type.lisp */
/* ---------- LISPARIT ---------- */
LISPSYM(decimal_string,"DECIMAL-STRING",system)
LISPSYM(zerop,"ZEROP",lisp)
LISPSYM(plusp,"PLUSP",lisp)
LISPSYM(minusp,"MINUSP",lisp)
LISPSYM(oddp,"ODDP",lisp)
LISPSYM(evenp,"EVENP",lisp)
LISPSYM(gleich,"=",lisp)
LISPSYM(ungleich,"/=",lisp)
LISPSYM(kleiner,"<",lisp)
LISPSYM(groesser,">",lisp)
LISPSYM(klgleich,"<=",lisp)
LISPSYM(grgleich,">=",lisp)
LISPSYM(max,"MAX",lisp)
LISPSYM(min,"MIN",lisp)
LISPSYM(plus,"+",lisp)
LISPSYM(minus,"-",lisp)
LISPSYM(mal,"*",lisp)
LISPSYM(durch,"/",lisp)
LISPSYM(einsplus,"1+",lisp)
LISPSYM(einsminus,"1-",lisp)
LISPSYM(conjugate,"CONJUGATE",lisp)
LISPSYM(gcd,"GCD",lisp)
LISPSYM(xgcd,"XGCD",ext)
LISPSYM(lcm,"LCM",lisp)
LISPSYM(exp,"EXP",lisp)
LISPSYM(expt,"EXPT",lisp)
LISPSYM(log,"LOG",lisp)
LISPSYM(sqrt,"SQRT",lisp)
LISPSYM(isqrt,"ISQRT",lisp)
LISPSYM(abs,"ABS",lisp)
LISPSYM(phase,"PHASE",lisp)
LISPSYM(signum,"SIGNUM",lisp)
LISPSYM(sin,"SIN",lisp)
LISPSYM(cos,"COS",lisp)
LISPSYM(tan,"TAN",lisp)
LISPSYM(cis,"CIS",lisp)
LISPSYM(asin,"ASIN",lisp)
LISPSYM(acos,"ACOS",lisp)
LISPSYM(atan,"ATAN",lisp)
LISPSYM(sinh,"SINH",lisp)
LISPSYM(cosh,"COSH",lisp)
LISPSYM(tanh,"TANH",lisp)
LISPSYM(asinh,"ASINH",lisp)
LISPSYM(acosh,"ACOSH",lisp)
LISPSYM(atanh,"ATANH",lisp)
LISPSYM(float,"FLOAT",lisp)
LISPSYM(rational,"RATIONAL",lisp)
LISPSYM(rationalize,"RATIONALIZE",lisp)
LISPSYM(numerator,"NUMERATOR",lisp)
LISPSYM(denominator,"DENOMINATOR",lisp)
LISPSYM(floor,"FLOOR",lisp)
LISPSYM(ceiling,"CEILING",lisp)
LISPSYM(truncate,"TRUNCATE",lisp)
LISPSYM(round,"ROUND",lisp)
LISPSYM(mod,"MOD",lisp)
LISPSYM(rem,"REM",lisp)
LISPSYM(ffloor,"FFLOOR",lisp)
LISPSYM(fceiling,"FCEILING",lisp)
LISPSYM(ftruncate,"FTRUNCATE",lisp)
LISPSYM(fround,"FROUND",lisp)
LISPSYM(decode_float,"DECODE-FLOAT",lisp)
LISPSYM(scale_float,"SCALE-FLOAT",lisp)
LISPSYM(float_radix,"FLOAT-RADIX",lisp)
LISPSYM(float_sign,"FLOAT-SIGN",lisp)
LISPSYM(float_digits,"FLOAT-DIGITS",lisp)
LISPSYM(float_precision,"FLOAT-PRECISION",lisp)
LISPSYM(integer_decode_float,"INTEGER-DECODE-FLOAT",lisp)
LISPSYM(complex,"COMPLEX",lisp)
LISPSYM(realpart,"REALPART",lisp)
LISPSYM(imagpart,"IMAGPART",lisp)
LISPSYM(logior,"LOGIOR",lisp)
LISPSYM(logxor,"LOGXOR",lisp)
LISPSYM(logand,"LOGAND",lisp)
LISPSYM(logeqv,"LOGEQV",lisp)
LISPSYM(lognand,"LOGNAND",lisp)
LISPSYM(lognor,"LOGNOR",lisp)
LISPSYM(logandc1,"LOGANDC1",lisp)
LISPSYM(logandc2,"LOGANDC2",lisp)
LISPSYM(logorc1,"LOGORC1",lisp)
LISPSYM(logorc2,"LOGORC2",lisp)
LISPSYM(boole,"BOOLE",lisp)
LISPSYM(lognot,"LOGNOT",lisp)
LISPSYM(logtest,"LOGTEST",lisp)
LISPSYM(logbitp,"LOGBITP",lisp)
LISPSYM(ash,"ASH",lisp)
LISPSYM(logcount,"LOGCOUNT",lisp)
LISPSYM(integer_length,"INTEGER-LENGTH",lisp)
LISPSYM(byte,"BYTE",lisp)
LISPSYM(bytesize,"BYTE-SIZE",lisp)
LISPSYM(byteposition,"BYTE-POSITION",lisp)
LISPSYM(ldb,"LDB",lisp)
LISPSYM(ldb_test,"LDB-TEST",lisp)
LISPSYM(mask_field,"MASK-FIELD",lisp)
LISPSYM(dpb,"DPB",lisp)
LISPSYM(deposit_field,"DEPOSIT-FIELD",lisp)
LISPSYM(random,"RANDOM",lisp)
LISPSYM(random_posfixnum,"RANDOM-POSFIXNUM",system)
LISPSYM(make_random_state,"MAKE-RANDOM-STATE",lisp)
LISPSYM(fakultaet,"!",ext)
LISPSYM(exquo,"EXQUO",ext)
LISPSYM(mod_expt,"MOD-EXPT",ext)
LISPSYM(long_float_digits,"LONG-FLOAT-DIGITS",ext)
LISPSYM(set_long_float_digits,"(SETF LONG-FLOAT-DIGITS)",system) /* ABI */
LISPSYM(log2,"LOG2",system)
LISPSYM(log10,"LOG10",system)
/* ---------- FOREIGN ---------- */
#ifdef DYNAMIC_FFI
LISPSYM(validp,"VALIDP",ffi)
LISPSYM(set_validp,"SET-VALIDP",ffi)
LISPSYM(set_foreign_pointer,"SET-FOREIGN-POINTER",ffi)
LISPSYM(sizeof,"%SIZEOF",ffi) /* ABI */
LISPSYM(bitsizeof,"%BITSIZEOF",ffi) /* ABI */
LISPSYM(lookup_foreign_variable,"LOOKUP-FOREIGN-VARIABLE",ffi) /* ABI */
LISPSYM(unsigned_foreign_address,"UNSIGNED-FOREIGN-ADDRESS",ffi)
LISPSYM(foreign_address_unsigned,"FOREIGN-ADDRESS-UNSIGNED",ffi)
LISPSYM(foreign_value,"FOREIGN-VALUE",ffi)
LISPSYM(set_foreign_value,"SET-FOREIGN-VALUE",ffi)
LISPSYM(foreign_type,"FOREIGN-TYPE",ffi) /* ABI */
LISPSYM(element,"%ELEMENT",ffi) /* ABI */
LISPSYM(deref,"%DEREF",ffi) /* ABI */
LISPSYM(slot,"%SLOT",ffi) /* ABI */
LISPSYM(cast,"%CAST",ffi) /* ABI */
LISPSYM(offset,"%OFFSET",ffi) /* ABI */
LISPSYM(read_memory_as,"MEMORY-AS",ffi)
LISPSYM(write_memory_as,"WRITE-MEMORY-AS",ffi) /* ABI */
LISPSYM(exec_on_stack,"EXEC-ON-STACK",ffi) /* ABI */
LISPSYM(call_with_foreign_string,"CALL-WITH-FOREIGN-STRING",ffi)
LISPSYM(foreign_allocate,"FOREIGN-ALLOCATE",ffi)
LISPSYM(foreign_free,"FOREIGN-FREE",ffi)
LISPSYM(lookup_foreign_function,"LOOKUP-FOREIGN-FUNCTION",ffi) /* ABI */
LISPSYM(foreign_call_out,"FOREIGN-CALL-OUT",ffi)
#if defined(WIN32_NATIVE) || defined(HAVE_DLOPEN)
LISPSYM(foreign_library,"FOREIGN-LIBRARY",ffi) /* ABI */
LISPSYM(close_foreign_library,"CLOSE-FOREIGN-LIBRARY",ffi)
LISPSYM(foreign_library_variable,"FOREIGN-LIBRARY-VARIABLE",ffi) /* ABI */
LISPSYM(foreign_library_function,"FOREIGN-LIBRARY-FUNCTION",ffi) /* ABI */
#endif  /* WIN32_NATIVE || HAVE_DLOPEN */
#endif  /* DYNAMIC_FFI */
/* ---------- ZTHREAD ---------- */
#ifdef MULTITHREAD
LISPSYM(make_process,"MAKE-PROCESS",mt)
LISPSYM(process_wait,"PROCESS-WAIT",mt)
LISPSYM(call_with_timeout,"CALL-WITH-TIMEOUT",mt) /* ABI */
LISPSYM(process_yield,"PROCESS-YIELD",mt)
LISPSYM(process_kill,"PROCESS-KILL",mt)
LISPSYM(process_interrupt,"PROCESS-INTERRUPT",mt)
LISPSYM(process_restart,"PROCESS-RESTART",mt)
LISPSYM(processp,"PROCESSP",mt)
LISPSYM(process_name,"PROCESS-NAME",mt)
LISPSYM(process_active_p,"PROCESS-ACTIVE-P",mt)
LISPSYM(process_state,"PROCESS-STATE",mt)
LISPSYM(process_whostate,"PROCESS-WHOSTATE",mt)
LISPSYM(current_process,"CURRENT-PROCESS",mt)
LISPSYM(list_processes,"LIST-PROCESSES",mt)
LISPSYM(make_lock,"MAKE-LOCK",mt)
LISPSYM(process_lock,"PROCESS-LOCK",mt)
LISPSYM(process_unlock,"PROCESS-UNLOCK",mt)
#endif

/* Keywords: */
LISPSYM(Kallow_other_keys,"ALLOW-OTHER-KEYS",keyword)
LISPSYM(Kadjustable,"ADJUSTABLE",keyword)
LISPSYM(Kelement_type,"ELEMENT-TYPE",keyword)
LISPSYM(Kinitial_element,"INITIAL-ELEMENT",keyword)
LISPSYM(Kinitial_contents,"INITIAL-CONTENTS",keyword)
LISPSYM(Kfill_pointer,"FILL-POINTER",keyword)
LISPSYM(Kdisplaced_to,"DISPLACED-TO",keyword)
LISPSYM(Kdisplaced_index_offset,"DISPLACED-INDEX-OFFSET",keyword)
LISPSYM(Kstart1,"START1",keyword)
LISPSYM(Kend1,"END1",keyword)
LISPSYM(Kstart2,"START2",keyword)
LISPSYM(Kend2,"END2",keyword)
LISPSYM(Kstart,"START",keyword)
LISPSYM(Kend,"END",keyword)
LISPSYM(Kno_hang,"NO-HANG",keyword)
LISPSYM(Kinteractive,"INTERACTIVE",keyword)
LISPSYM(Kpreserve_whitespace,"PRESERVE-WHITESPACE",keyword)
LISPSYM(Kradix,"RADIX",keyword)
LISPSYM(Kjunk_allowed,"JUNK-ALLOWED",keyword)
LISPSYM(Kcase,"CASE",keyword)
LISPSYM(Klevel,"LEVEL",keyword)
LISPSYM(Klength,"LENGTH",keyword)
LISPSYM(Klines,"LINES",keyword)
LISPSYM(Kmiser_width,"MISER-WIDTH",keyword)
LISPSYM(Kpprint_dispatch,"PPRINT-DISPATCH",keyword)
LISPSYM(Klinear,"LINEAR",keyword)       /* PPRINT-NEWLINE */
LISPSYM(Kfill,"FILL",keyword)           /* PPRINT-NEWLINE */
LISPSYM(Kmiser,"MISER",keyword)         /* PPRINT-NEWLINE */
LISPSYM(Kmandatory,"MANDATORY",keyword) /* PPRINT-NEWLINE */
LISPSYM(Kblock,"BLOCK",keyword)     /* PPRINT-INDENT */
LISPSYM(Kcurrent,"CURRENT",keyword) /* PPRINT-INDENT */
LISPSYM(Kgensym,"GENSYM",keyword)
LISPSYM(Kescape,"ESCAPE",keyword)
LISPSYM(Kbase,"BASE",keyword)
LISPSYM(Karray,"ARRAY",keyword)
LISPSYM(Kcircle,"CIRCLE",keyword)
LISPSYM(Kpretty,"PRETTY",keyword)
LISPSYM(Kclosure,"CLOSURE",keyword)
LISPSYM(Kreadably,"READABLY",keyword)
LISPSYM(Kright_margin,"RIGHT-MARGIN",keyword)
LISPSYM(Kstream,"STREAM",keyword)
LISPSYM(Kidentity,"IDENTITY",keyword)
LISPSYM(Ktest,"TEST",keyword)
LISPSYM(Ktest_not,"TEST-NOT",keyword)
LISPSYM(Kkey,"KEY",keyword)
LISPSYM(Knicknames,"NICKNAMES",keyword)
LISPSYM(Kuse,"USE",keyword)
LISPSYM(Kcase_sensitive,"CASE-SENSITIVE",keyword)
LISPSYM(Kcase_inverted,"CASE-INVERTED",keyword)
LISPSYM(Kupdate,"UPDATE",keyword)
LISPSYM(Kup,"UP",keyword)     /* 19.2.2.4.3 - directory component */
LISPSYM(Kback,"BACK",keyword) /* (see MAKE-PATHNAME in pathname.d) */
LISPSYM(Kfrom_end,"FROM-END",keyword)
LISPSYM(Kinitial_value,"INITIAL-VALUE",keyword)
LISPSYM(Kcount,"COUNT",keyword)
LISPSYM(Ksize,"SIZE",keyword)
LISPSYM(Krehash_size,"REHASH-SIZE",keyword)
LISPSYM(Krehash_threshold,"REHASH-THRESHOLD",keyword)
LISPSYM(Kkey_type,"KEY-TYPE",keyword)
LISPSYM(Kvalue_type,"VALUE-TYPE",keyword)
LISPSYM(Kwarn_if_needs_rehash_after_gc,"WARN-IF-NEEDS-REHASH-AFTER-GC",keyword)
LISPSYM(Kweak,"WEAK",keyword)
LISPSYM(Kkey_and_value,"KEY-AND-VALUE",keyword)
LISPSYM(Kkey_or_value,"KEY-OR-VALUE",keyword)
LISPSYM(Kvalue,"VALUE",keyword)
LISPSYM(Kdefaults,"DEFAULTS",keyword)
LISPSYM(Kdevice,"DEVICE",keyword)
LISPSYM(Kdirectory,"DIRECTORY",keyword)
LISPSYM(Kname,"NAME",keyword)
LISPSYM(Ktype,"TYPE",keyword)
LISPSYM(Kversion,"VERSION",keyword)
LISPSYM(Khost,"HOST",keyword)
LISPSYM(Kall,"ALL",keyword)
LISPSYM(Kmerge,"MERGE",keyword)
LISPSYM(Kdirection,"DIRECTION",keyword)
LISPSYM(Kif_exists,"IF-EXISTS",keyword)
LISPSYM(Kif_does_not_exist,"IF-DOES-NOT-EXIST",keyword)
LISPSYM(Kkeep,"KEEP",keyword)   /* DIRECTORY :IF-DOES-NOT-EXIST argument */
LISPSYM(Kdiscard,"DISCARD",keyword) /* DIRECTORY :IF-DOES-NOT-EXIST argument */
LISPSYM(Kexternal_format,"EXTERNAL-FORMAT",keyword)
LISPSYM(Kbuffered,"BUFFERED",keyword)
LISPSYM(Kfull,"FULL",keyword)
LISPSYM(Kabort,"ABORT",keyword)
LISPSYM(Kverbose,"VERBOSE",keyword)
LISPSYM(Kexecute,"EXECUTE",keyword)
LISPSYM(Kcompile_toplevel,"COMPILE-TOPLEVEL",keyword)
LISPSYM(Kload_toplevel,"LOAD-TOPLEVEL",keyword)
LISPSYM(Keof,"EOF",keyword)
LISPSYM(Kinput_available,"INPUT-AVAILABLE",keyword)
LISPSYM(Kline_position,"LINE-POSITION",keyword)
LISPSYM(Klittle,"LITTLE",keyword)
LISPSYM(Kbig,"BIG",keyword)
LISPSYM(Kcharset,"CHARSET",keyword)
LISPSYM(Kline_terminator,"LINE-TERMINATOR",keyword)
LISPSYM(Kunix,"UNIX",keyword)
LISPSYM(Kmac,"MAC",keyword)
LISPSYM(Kdos,"DOS",keyword)
LISPSYM(Kinput_error_action,"INPUT-ERROR-ACTION",keyword)
LISPSYM(Koutput_error_action,"OUTPUT-ERROR-ACTION",keyword)
LISPSYM(Kansi_cl,"ANSI-CL",keyword)
LISPSYM(Kextra_file_types,"EXTRA-FILE-TYPES",keyword)
#if defined(UNIX) || defined (WIN32_NATIVE)
LISPSYM(Kwait,"WAIT",keyword)
LISPSYM(Kterminal,"TERMINAL",keyword)
LISPSYM(Kpipe,"PIPE",keyword)
LISPSYM(Karguments,"ARGUMENTS",keyword)
LISPSYM(Kpriority,"PRIORITY",keyword)
LISPSYM(Khigh,"HIGH",keyword)
LISPSYM(Knormal,"NORMAL",keyword)
LISPSYM(Klow,"LOW",keyword)
#endif
#ifdef SOCKET_STREAMS
LISPSYM(Ktimeout,"TIMEOUT",keyword)
LISPSYM(Kinterface,"INTERFACE",keyword)
LISPSYM(Kbacklog,"BACKLOG",keyword)
LISPSYM(Kso_debug,"SO-DEBUG",keyword)
LISPSYM(Kso_acceptconn,"SO-ACCEPTCONN",keyword)
LISPSYM(Kso_broadcast,"SO-BROADCAST",keyword)
LISPSYM(Kso_reuseaddr,"SO-REUSEADDR",keyword)
LISPSYM(Kso_dontroute,"SO-DONTROUTE",keyword)
LISPSYM(Kso_keepalive,"SO-KEEPALIVE",keyword)
LISPSYM(Kso_error,"SO-ERROR",keyword)
LISPSYM(Kso_linger,"SO-LINGER",keyword)
LISPSYM(Kso_oobinline,"SO-OOBINLINE",keyword)
LISPSYM(Kso_type,"SO-TYPE",keyword)
LISPSYM(Kso_rcvbuf,"SO-RCVBUF",keyword)
LISPSYM(Kso_sndbuf,"SO-SNDBUF",keyword)
LISPSYM(Kso_rcvlowat,"SO-RCVLOWAT",keyword)
LISPSYM(Kso_sndlowat,"SO-SNDLOWAT",keyword)
LISPSYM(Kso_rcvtimeo,"SO-RCVTIMEO",keyword)
LISPSYM(Kso_sndtimeo,"SO-SNDTIMEO",keyword)
#endif
#if defined(WIN32_NATIVE)
LISPSYM(Kwin32,"WIN32",keyword)
#endif
LISPSYM(Kread_only,"READ-ONLY",keyword)

/* other symbols: */
LISPSYM(standard_char,"STANDARD-CHAR",lisp) /* type in PREDTYPE */
LISPSYM(string_char,"STRING-CHAR",ext) /* type in PREDTYPE */
LISPSYM(base_char,"BASE-CHAR",lisp) /* type in PREDTYPE */
LISPSYM(array_rank_limit,"ARRAY-RANK-LIMIT",lisp) /* constant in ARRAY */
LISPSYM(array_dimension_limit,"ARRAY-DIMENSION-LIMIT",lisp) /* constant in ARRAY */
LISPSYM(string_dimension_limit,"STRING-DIMENSION-LIMIT",system) /* constant in ARRAY */
LISPSYM(array_total_size_limit,"ARRAY-TOTAL-SIZE-LIMIT",lisp) /* constant in ARRAY */
LISPSYM(subtype_integer,"SUBTYPE-INTEGER",system) /* function for ARRAY */
LISPSYM(char_cod_limit,"CHAR-CODE-LIMIT",lisp) /* constant in CHARSTRG */
LISPSYM(base_char_cod_limit,"BASE-CHAR-CODE-LIMIT",ext) /* constant in CHARSTRG */
#if defined(UNICODE) && defined(AWFULLY_SLOW)
LISPSYM(unicode_attributes_line,"UNICODE-ATTRIBUTES-LINE",system) /* function in CHARSTRG */
#endif
LISPSYM(designator,"DESIGNATOR",ext) /* type for CHARSTRG */
LISPSYM(make_trampoline,"MAKE-TRAMPOLINE",system) /* function for RECORD */
LISPSYM(symbolmacro,"SYMBOLMACRO",system) /* property in RECORD */
LISPSYM(make_instance,"MAKE-INSTANCE",clos) /* error reporting in record.d */
LISPSYM(shared_initialize,"SHARED-INITIALIZE",clos) /* ditto */
LISPSYM(reinitialize_instance,"REINITIALIZE-INSTANCE",clos) /* ditto */
LISPSYM(initialize_instance,"INITIALIZE-INSTANCE",clos) /* ditto */
LISPSYM(update_instance_frc,"UPDATE-INSTANCE-FOR-REDEFINED-CLASS",clos)
LISPSYM(class_slots,"CLASS-SLOTS",clos) /* function for RECORD */
LISPSYM(slot_definition_location,"SLOT-DEFINITION-LOCATION",clos) /* function for RECORD, IO */
LISPSYM(slot_definition_name,"SLOT-DEFINITION-NAME",clos) /* function for RECORD, IO */
LISPSYM(structure_object,"STRUCTURE-OBJECT",lisp) /* type for RECORD */
LISPSYM(class,"CLASS",clos) /* type for RECORD */
LISPSYM(slot_missing,"SLOT-MISSING",clos) /* function for RECORD */
LISPSYM(slot_unbound,"SLOT-UNBOUND",clos) /* function for RECORD */
LISPSYM(standard_object,"STANDARD-OBJECT",lisp) /* type for RECORD */
LISPSYM(reinitialize_instance_table,"*REINITIALIZE-INSTANCE-TABLE*",clos) /* variable for RECORD */
LISPSYM(make_instance_table,"*MAKE-INSTANCE-TABLE*",clos) /* variable for RECORD */
LISPSYM(initial_reinitialize_instance,"INITIAL-REINITIALIZE-INSTANCE",clos) /* function for RECORD */
LISPSYM(initial_initialize_instance,"INITIAL-INITIALIZE-INSTANCE",clos) /* function for RECORD */
LISPSYM(initial_make_instance,"INITIAL-MAKE-INSTANCE",clos) /* function for RECORD */
LISPSYM(allocate_instance,"ALLOCATE-INSTANCE",clos) /* function for RECORD */
LISPSYM(finalize_inheritance,"FINALIZE-INHERITANCE",clos) /* function for RECORD */
LISPSYM(class_version_compute_slotlists,"CLASS-VERSION-COMPUTE-SLOTLISTS",clos) /* function for RECORD */
LISPSYM(constant_initfunction,"CONSTANT-INITFUNCTION",clos) /* marker in RECORD */
LISPSYM(fasthash_eq,"FASTHASH-EQ",ext) /* test for HASHTABL */
LISPSYM(stablehash_eq,"STABLEHASH-EQ",ext) /* test for HASHTABL */
LISPSYM(eq_hashfunction,"*EQ-HASHFUNCTION*",custom) /* variable for HASHTABL */
LISPSYM(fasthash_eql,"FASTHASH-EQL",ext) /* test for HASHTABL */
LISPSYM(stablehash_eql,"STABLEHASH-EQL",ext) /* test for HASHTABL */
LISPSYM(eql_hashfunction,"*EQL-HASHFUNCTION*",custom) /* variable for HASHTABL */
LISPSYM(fasthash_equal,"FASTHASH-EQUAL",ext) /* test for HASHTABL */
LISPSYM(stablehash_equal,"STABLEHASH-EQUAL",ext) /* test for HASHTABL */
LISPSYM(equal_hashfunction,"*EQUAL-HASHFUNCTION*",custom) /* variable for HASHTABL */
LISPSYM(structure_stablehash,"STRUCTURE-STABLEHASH",clos) /* class for HASHTABL */
LISPSYM(warn_on_hashtable_needing_rehash_after_gc,"*WARN-ON-HASHTABLE-NEEDING-REHASH-AFTER-GC*",custom) /* variable for HASHTABL */
LISPSYM(simple_vector,"SIMPLE-VECTOR",lisp) /* type in SEQUENCE, PREDTYPE */
LISPSYM(simple_string,"SIMPLE-STRING",lisp) /* type in SEQUENCE, PREDTYPE */
LISPSYM(base_string,"BASE-STRING",lisp) /* type in SEQUENCE, PREDTYPE */
LISPSYM(simple_base_string,"SIMPLE-BASE-STRING",lisp) /* type in SEQUENCE, PREDTYPE */
LISPSYM(bit_vector,"BIT-VECTOR",lisp) /* type in SEQUENCE, PREDTYPE */
LISPSYM(simple_bit_vector,"SIMPLE-BIT-VECTOR",lisp) /* type in SEQUENCE, PREDTYPE */
LISPSYM(array,"ARRAY",lisp) /* type in SEQUENCE, PREDTYPE */
LISPSYM(simple_array,"SIMPLE-ARRAY",lisp) /* type in SEQUENCE, PREDTYPE */
LISPSYM(sequence,"SEQUENCE",lisp) /* type for SEQUENCE */
LISPSYM(subtype_sequence,"SUBTYPE-SEQUENCE",system) /* function for SEQUENCE */
LISPSYM(package_error,"PACKAGE-ERROR",lisp) /* type for PACKAGE */
LISPSYM(Kinternal,"INTERNAL",keyword) /* INTERN result in PACKAGE */
LISPSYM(Kexternal,"EXTERNAL",keyword) /* INTERN result in PACKAGE */
LISPSYM(Kinherited,"INHERITED",keyword) /* INTERN result in PACKAGE */
LISPSYM(do_symbols,"DO-SYMBOLS",lisp) /* error reporter in PACKAGE */
LISPSYM(do_external_symbols,"DO-EXTERNAL-SYMBOLS",lisp) /* error reporter in PACKAGE */
LISPSYM(packagestern,"*PACKAGE*",lisp) /* variable in PACKAGE */
LISPSYM(internal_time_units_per_second,"INTERNAL-TIME-UNITS-PER-SECOND",lisp) /* constant in TIME */
LISPSYM(encode_universal_time,"ENCODE-UNIVERSAL-TIME",lisp) /* function in TIME */
LISPSYM(use_clcs,"*USE-CLCS*",system) /* variable in ERROR */
LISPSYM(recursive_error_count,"*RECURSIVE-ERROR-COUNT*",system) /* variable in ERROR */
LISPSYM(error_handler,"*ERROR-HANDLER*",custom) /* variable in ERROR */
LISPSYM(simple_condition,"SIMPLE-CONDITION",lisp) /* type for ERROR                                 --+ */
LISPSYM(simple_serious_condition,"SIMPLE-SERIOUS-CONDITION",system) /* type for ERROR                 | */
LISPSYM(simple_error,"SIMPLE-ERROR",lisp) /* type for ERROR                                           | */
LISPSYM(simple_program_error,"SIMPLE-PROGRAM-ERROR",system) /* type for ERROR                         | */
LISPSYM(simple_source_program_error,"SIMPLE-SOURCE-PROGRAM-ERROR",system) /* type for ERROR           | */
LISPSYM(simple_control_error,"SIMPLE-CONTROL-ERROR",system) /* type for ERROR                         | */
LISPSYM(simple_arithmetic_error,"SIMPLE-ARITHMETIC-ERROR",system) /* type for ERROR                   | */
LISPSYM(simple_division_by_zero,"SIMPLE-DIVISION-BY-ZERO",system) /* type for ERROR                   | */
LISPSYM(simple_floating_point_overflow,"SIMPLE-FLOATING-POINT-OVERFLOW",system) /* type for ERROR     | order is co-ordinated */
LISPSYM(simple_floating_point_underflow,"SIMPLE-FLOATING-POINT-UNDERFLOW",system) /* type for ERROR   | with ERROR.D, */
LISPSYM(simple_cell_error,"SIMPLE-CELL-ERROR",system) /* type for ERROR                               | LISPBIBL.D, */
LISPSYM(simple_unbound_variable,"SIMPLE-UNBOUND-VARIABLE",system) /* type for ERROR                   | CONDITION.LISP! */
LISPSYM(simple_undefined_function,"SIMPLE-UNDEFINED-FUNCTION",system) /* type for ERROR               | */
LISPSYM(simple_unbound_slot,"SIMPLE-UNBOUND-SLOT",system) /* type for ERROR                           | */
LISPSYM(simple_type_error,"SIMPLE-TYPE-ERROR",lisp) /* type for ERROR                                 | */
LISPSYM(simple_keyword_error,"SIMPLE-KEYWORD-ERROR",system) /* type for ERROR                         | */
LISPSYM(simple_charset_type_error,"SIMPLE-CHARSET-TYPE-ERROR",ext) /* type for ERROR                  | */
LISPSYM(simple_argument_list_dotted,"SIMPLE-ARGUMENT-LIST-DOTTED",system) /* type for ERROR           | */
LISPSYM(simple_package_error,"SIMPLE-PACKAGE-ERROR",system) /* type for ERROR                         | */
LISPSYM(simple_print_not_readable,"SIMPLE-PRINT-NOT-READABLE",system) /* type for ERROR               | */
LISPSYM(simple_parse_error,"SIMPLE-PARSE-ERROR",system) /* type for ERROR                             | */
LISPSYM(simple_stream_error,"SIMPLE-STREAM-ERROR",system) /* type for ERROR                           | */
LISPSYM(simple_end_of_file,"SIMPLE-END-OF-FILE",system) /* type for ERROR                             | */
LISPSYM(simple_reader_error,"SIMPLE-READER-ERROR",system) /* type for ERROR                           | */
LISPSYM(simple_file_error,"SIMPLE-FILE-ERROR",system) /* type for ERROR                               | */
LISPSYM(simple_os_error,"SIMPLE-OS-ERROR",system) /* type for ERROR                                   | */
LISPSYM(simple_storage_condition,"SIMPLE-STORAGE-CONDITION",system) /* type for ERROR                 | */
LISPSYM(simple_interrupt_condition,"SIMPLE-INTERRUPT-CONDITION",system) /* type for ERROR             | */
LISPSYM(simple_warning,"SIMPLE-WARNING",lisp) /* type for ERROR                                     --+ */
LISPSYM(Kinstance,"INSTANCE",keyword) /* make-condition-argument for ERROR */
LISPSYM(Kdatum,"DATUM",keyword) /* make-condition-argument for ERROR */
LISPSYM(Kexpected_type,"EXPECTED-TYPE",keyword) /* make-condition-argument for ERROR */
LISPSYM(Kpackage,"PACKAGE",keyword) /* make-condition-argument for ERROR */
LISPSYM(Kobject,"OBJECT",keyword) /* make-condition-argument for ERROR */
LISPSYM(Kpathname,"PATHNAME",keyword) /* make-condition-argument for ERROR */
LISPSYM(Kdetail,"DETAIL",keyword) /* make-condition-argument for ERROR */
LISPSYM(format,"FORMAT",lisp) /* function in ERROR */
LISPSYM(debugger_hook,"*DEBUGGER-HOOK*",lisp) /* variable in ERROR */
LISPSYM(coerce_to_condition,"COERCE-TO-CONDITION",system) /* function for ERROR */
LISPSYM(cerror,"CERROR",lisp) /* function for ERROR */
LISPSYM(check_value,"CHECK-VALUE",system) /* function for ERROR */
LISPSYM(correctable_error,"CORRECTABLE-ERROR",system) /* function for ERROR */
LISPSYM(prompt_for_new_value,"PROMPT-FOR-NEW-VALUE",system) /* function for ERROR */ /* ABI */
LISPSYM(continue,"CONTINUE",lisp) /* restart for ERROR */
LISPSYM(break_on_signals,"*BREAK-ON-SIGNALS*",lisp) /* variable for ERROR */
LISPSYM(safe_typep,"SAFE-TYPEP",system) /* function for ERROR */
LISPSYM(done_signaling,"DONE-SIGNALING",system) /* catch-tag for ERROR */
LISPSYM(penl,"$PENL",system) /* slotname in STREAM */
LISPSYM(stream_read_byte,"STREAM-READ-BYTE",gray)
LISPSYM(stream_read_byte_lookahead,"STREAM-READ-BYTE-LOOKAHEAD",gray)
LISPSYM(stream_read_byte_sequence,"STREAM-READ-BYTE-SEQUENCE",gray)
LISPSYM(stream_write_byte,"STREAM-WRITE-BYTE",gray)
LISPSYM(stream_write_byte_sequence,"STREAM-WRITE-BYTE-SEQUENCE",gray)
LISPSYM(stream_read_char,"STREAM-READ-CHAR",gray)
LISPSYM(stream_unread_char,"STREAM-UNREAD-CHAR",gray)
LISPSYM(stream_peek_char,"STREAM-PEEK-CHAR",gray)
LISPSYM(stream_read_char_sequence,"STREAM-READ-CHAR-SEQUENCE",gray)
LISPSYM(stream_write_char,"STREAM-WRITE-CHAR",gray)
LISPSYM(stream_write_char_sequence,"STREAM-WRITE-CHAR-SEQUENCE",gray)
LISPSYM(stream_read_line,"STREAM-READ-LINE",gray)
LISPSYM(stream_read_char_will_hang_p,"STREAM-READ-CHAR-WILL-HANG-P",gray)
LISPSYM(stream_clear_input,"STREAM-CLEAR-INPUT",gray)
LISPSYM(stream_finish_output,"STREAM-FINISH-OUTPUT",gray)
LISPSYM(stream_force_output,"STREAM-FORCE-OUTPUT",gray)
LISPSYM(stream_clear_output,"STREAM-CLEAR-OUTPUT",gray)
LISPSYM(stream_line_column,"STREAM-LINE-COLUMN",gray)
LISPSYM(stream_position,"STREAM-POSITION",gray)
#ifdef GENERIC_STREAMS
LISPSYM(generic_stream_rdch,"GENERIC-STREAM-READ-CHAR",gstream)
LISPSYM(generic_stream_pkch,"GENERIC-STREAM-PEEK-CHAR",gstream)
LISPSYM(generic_stream_read_char_will_hang_p,"GENERIC-STREAM-READ-CHAR-WILL-HANG-P",gstream)
LISPSYM(generic_stream_clear_input,"GENERIC-STREAM-CLEAR-INPUT",gstream)
LISPSYM(generic_stream_wrch,"GENERIC-STREAM-WRITE-CHAR",gstream)
LISPSYM(generic_stream_wrss,"GENERIC-STREAM-WRITE-STRING",gstream)
LISPSYM(generic_stream_finish_output,"GENERIC-STREAM-FINISH-OUTPUT",gstream)
LISPSYM(generic_stream_force_output,"GENERIC-STREAM-FORCE-OUTPUT",gstream)
LISPSYM(generic_stream_clear_output,"GENERIC-STREAM-CLEAR-OUTPUT",gstream)
LISPSYM(generic_stream_rdby,"GENERIC-STREAM-READ-BYTE",gstream)
LISPSYM(generic_stream_wrby,"GENERIC-STREAM-WRITE-BYTE",gstream)
LISPSYM(generic_stream_close,"GENERIC-STREAM-CLOSE",gstream)
LISPSYM(generic_stream,"GENERIC-STREAM",gstream)
#endif
#ifdef KEYBOARD
LISPSYM(Kchar,"CHAR",keyword) /* make-input-character-argument for STREAM */
LISPSYM(Kbits,"BITS",keyword) /* make-input-character-argument for STREAM */
LISPSYM(make_input_character,"MAKE-INPUT-CHARACTER",system) /* function for STREAM */
LISPSYM(make_char,"MAKE-CHAR",ext) /* function for STREAM */
LISPSYM(keyboard_input,"*KEYBOARD-INPUT*",ext) /* variable in STREAM */
LISPSYM(input_character,"INPUT-CHARACTER",system) /* type for CHARSTRG */
LISPSYM(input_character_char,"INPUT-CHARACTER-CHAR",system) /* function for CHARSTRG */
#endif
LISPSYM(completion,"COMPLETION",system) /* function in STREAM when GNU_READLINE is used */
#if defined(GNU_READLINE) || defined(NEXTAPP)
LISPSYM(conversion_failure,"CONVERSION-FAILURE",system) /* CATCH-tag in STREAM */
#endif
LISPSYM(terminal_io,"*TERMINAL-IO*",lisp) /* variable in STREAM */
LISPSYM(key_bindings,"*KEY-BINDINGS*",system) /* variable in STREAM */
LISPSYM(query_io,"*QUERY-IO*",lisp) /* variable in STREAM */
LISPSYM(debug_io,"*DEBUG-IO*",lisp) /* variable in STREAM */
LISPSYM(standard_input,"*STANDARD-INPUT*",lisp) /* variable in STREAM */
LISPSYM(standard_output,"*STANDARD-OUTPUT*",lisp) /* variable in STREAM */
LISPSYM(error_output,"*ERROR-OUTPUT*",lisp) /* variable in STREAM */
LISPSYM(trace_output,"*TRACE-OUTPUT*",lisp) /* variable in STREAM */
LISPSYM(stream_element_type,"STREAM-ELEMENT-TYPE",lisp) /* function in STREAM */
LISPSYM(lastchar,"$LASTCHAR",system) /* slotname in STREAM */
LISPSYM(reval,"$REVAL",system) /* slotname in STREAM */
LISPSYM(default_pathname_defaults,"*DEFAULT-PATHNAME-DEFAULTS*",lisp) /* variable in PATHNAME */
LISPSYM(merge_pathnames_ansi,"*MERGE-PATHNAMES-ANSI*",custom) /* variable in PATHNAME */
LISPSYM(print_pathnames_ansi,"*PRINT-PATHNAMES-ANSI*",custom) /* variable in IO */
LISPSYM(print_space_char_ansi,"*PRINT-SPACE-CHAR-ANSI*",custom) /* variable in IO */
LISPSYM(print_empty_arrays_ansi,"*PRINT-EMPTY-ARRAYS-ANSI*",custom) /* variable in IO */
LISPSYM(print_unreadable_ansi,"*PRINT-UNREADABLE-ANSI*",custom) /* variable in IO */
LISPSYM(parse_namestring_ansi,"*PARSE-NAMESTRING-ANSI*",custom) /* variable in PATHNAME */
#ifdef PATHNAME_NOEXT
LISPSYM(parse_namestring_dot_file,"*PARSE-NAMESTRING-DOT-FILE*",custom) /* variable in PATHNAME */
#endif
#ifdef LOGICAL_PATHNAMES
LISPSYM(type_error,"TYPE-ERROR",lisp) /* type for PATHNAME */
LISPSYM(logpathname_translations,"*LOGICAL-PATHNAME-TRANSLATIONS*",system) /* variable in PATHNAME */
#endif
LISPSYM(Kwild,"WILD",keyword) /* pathname component in PATHNAME */
LISPSYM(Kwild_inferiors,"WILD-INFERIORS",keyword) /* pathname component in PATHNAME */
LISPSYM(Krelative,"RELATIVE",keyword) /* pathname component in PATHNAME */
LISPSYM(Kabsolute,"ABSOLUTE",keyword) /* pathname component in PATHNAME */
LISPSYM(Knewest,"NEWEST",keyword) /* pathname component in PATHNAME */
LISPSYM(Kunspecific,"UNSPECIFIC",keyword) /* argument in PATHNAME */
LISPSYM(Kcommon,"COMMON",keyword) /* argument in PATHNAME */
/* LISPSYM(Klocal,"LOCAL",keyword) / * argument in PATHNAME */
LISPSYM(Kinput,"INPUT",keyword) /* argument in PATHNAME */
LISPSYM(Kinput_immutable,"INPUT-IMMUTABLE",keyword) /* argument in PATHNAME */
LISPSYM(Koutput,"OUTPUT",keyword) /* argument in PATHNAME */
LISPSYM(Kio,"IO",keyword) /* argument in PATHNAME */
LISPSYM(Kprobe,"PROBE",keyword) /* argument in PATHNAME */
LISPSYM(unsigned_byte,"UNSIGNED-BYTE",lisp) /* argument in PATHNAME */
LISPSYM(signed_byte,"SIGNED-BYTE",lisp) /* argument in PATHNAME */
LISPSYM(Kdefault,"DEFAULT",keyword) /* argument in PATHNAME & FOREIGN */
LISPSYM(Knext,"NEXT",keyword) /* argument in FOREIGN */
LISPSYM(canonicalize_type,"CANONICALIZE-TYPE",system) /* function for PATHNAME */
LISPSYM(subtypep,"SUBTYPEP",lisp) /* function for PATHNAME */
LISPSYM(Kerror,"ERROR",keyword) /* argument in PATHNAME, ENCODING */
LISPSYM(Knew_version,"NEW-VERSION",keyword) /* argument in PATHNAME */
LISPSYM(Krename,"RENAME",keyword) /* argument in PATHNAME */
LISPSYM(Krename_and_delete,"RENAME-AND-DELETE",keyword) /* argument in PATHNAME */
LISPSYM(Koverwrite,"OVERWRITE",keyword) /* argument in PATHNAME */
LISPSYM(Kappend,"APPEND",keyword) /* argument in PATHNAME */
LISPSYM(Ksupersede,"SUPERSEDE",keyword) /* argument in PATHNAME */
LISPSYM(Kcreate,"CREATE",keyword) /* argument in PATHNAME */
#if defined(DYNAMIC_FFI)
LISPSYM(Kcopy,"COPY",keyword) /* SET-FOREIGN-POINTER */
#endif
LISPSYM(warn,"WARN",lisp) /* function in STREAM, PATHNAME */
LISPSYM(Kignore,"IGNORE",keyword) /* argument in ENCODING, PATHNAME */
LISPSYM(with_output_to_string,"WITH-OUTPUT-TO-STRING",lisp) /* error reporter in STREAM */
LISPSYM(integer,"INTEGER",lisp) /* type in STREAM */
LISPSYM(hash_table,"HASH-TABLE",lisp) /* type in IO, PREDTYPE */
LISPSYM(random_state,"RANDOM-STATE",lisp) /* type in IO, PREDTYPE */
LISPSYM(reader_error,"READER-ERROR",lisp) /* type for IO */
LISPSYM(read_base,"*READ-BASE*",lisp) /* IO variable */
LISPSYM(read_suppress,"*READ-SUPPRESS*",lisp) /* IO variable */
LISPSYM(read_eval,"*READ-EVAL*",lisp) /* IO variable */
LISPSYM(readtablestern,"*READTABLE*",lisp) /* IO variable */
LISPSYM(features,"*FEATURES*",lisp) /* IO variable */
LISPSYM(read_preserve_whitespace,"*READ-PRESERVE-WHITESPACE*",system) /* IO variable */
LISPSYM(read_line_number,"*READ-LINE-NUMBER*",system) /* IO variable */
LISPSYM(read_recursive_p,"*READ-RECURSIVE-P*",system) /* IO variable */
LISPSYM(read_reference_table,"*READ-REFERENCE-TABLE*",system) /* IO variable */
LISPSYM(backquote_level,"*BACKQUOTE-LEVEL*",system) /* IO variable */
LISPSYM(backquote_reader,"BACKQUOTE-READER",system) /* function for IO */
LISPSYM(comma_reader,"COMMA-READER",system) /* function for IO */
LISPSYM(reading_array,"*READING-ARRAY*",system) # IO variable
LISPSYM(reading_struct,"*READING-STRUCT*",system) # IO variable
LISPSYM(compiling,"*COMPILING*",system) /* IO variable */
LISPSYM(make_init_form,"MAKE-INIT-FORM",clos) /* a function for io.d */
LISPSYM(make_byte,"MAKE-BYTE",system) /* function for IO */
LISPSYM(Kupcase,"UPCASE",keyword) /* *PRINT-CASE* - value in IO */
LISPSYM(Kdowncase,"DOWNCASE",keyword) /* *PRINT-CASE* - value in IO */
LISPSYM(Kcapitalize,"CAPITALIZE",keyword) /* *PRINT-CASE* - value in IO */
                           /* Must be in the same order as in io.d! */
LISPSYM(print_case,"*PRINT-CASE*",lisp) /* --------- IO variable -+ */
LISPSYM(print_level,"*PRINT-LEVEL*",lisp) /*                      | */
LISPSYM(print_length,"*PRINT-LENGTH*",lisp) /*                    | */
LISPSYM(print_gensym,"*PRINT-GENSYM*",lisp) /*                    | */
LISPSYM(print_escape,"*PRINT-ESCAPE*",lisp) /*                    | */
LISPSYM(print_radix,"*PRINT-RADIX*",lisp) /*                      | */
LISPSYM(print_base,"*PRINT-BASE*",lisp) /*                        | */
LISPSYM(print_array,"*PRINT-ARRAY*",lisp) /*                      | */
LISPSYM(print_circle,"*PRINT-CIRCLE*",lisp) /*                    | */
LISPSYM(print_pretty,"*PRINT-PRETTY*",lisp) /*                    | */
LISPSYM(print_closure,"*PRINT-CLOSURE*",custom) /*                | */
LISPSYM(print_readably,"*PRINT-READABLY*",lisp) /*                | */
LISPSYM(print_lines,"*PRINT-LINES*",lisp) /*                      | */
LISPSYM(print_miser_width,"*PRINT-MISER-WIDTH*",lisp) /*          | */
LISPSYM(print_pprint_dispatch,"*PRINT-PPRINT-DISPATCH*",lisp) /*  | */
LISPSYM(print_right_margin,"*PRINT-RIGHT-MARGIN*",lisp) /* -------+ */
LISPSYM(print_rpars,"*PRINT-RPARS*",custom) /* IO variable */
LISPSYM(print_indent_lists,"*PRINT-INDENT-LISTS*",custom) /* IO variable */
LISPSYM(print_pretty_fill,"*PRINT-PRETTY-FILL*",custom) /* IO variable */
LISPSYM(print_circle_table,"*PRINT-CIRCLE-TABLE*",system) /* IO variable */
LISPSYM(print_symbol_package_prefix_shortest,"*PRINT-SYMBOL-PACKAGE-PREFIX-SHORTEST*",custom) /* IO variable */
LISPSYM(prin_level,"*PRIN-LEVEL*",system) /* IO variable */ /* ABI */
LISPSYM(prin_lines,"*PRIN-LINES*",system) /* IO variable */
LISPSYM(prin_line_prefix,"*PRIN-LINE-PREFIX*",system) /* IO variable */ /* ABI */
LISPSYM(prin_miserp,"*PRIN-MISERP*",system) /* IO variable */ /* ABI */
LISPSYM(prin_pprinter,"*PRIN-PPRINTER*",system) /* IO variable */
LISPSYM(prin_indentation,"*PRIN-INDENTATION*",system) /* IO variable */ /* ABI */
LISPSYM(prin_bqlevel,"*PRIN-BQLEVEL*",system) /* IO variable */
LISPSYM(prin_stream,"*PRIN-STREAM*",system) /* IO variable */ /* ABI */
LISPSYM(prin_linelength,"*PRIN-LINELENGTH*",system) /* IO variable */ /* ABI */
LISPSYM(prin_l1,"*PRIN-L1*",system) /* IO variable */
LISPSYM(prin_lm,"*PRIN-LM*",system) /* IO variable */
LISPSYM(prin_rpar,"*PRIN-RPAR*",system) /* IO variable */
LISPSYM(prin_traillength,"*PRIN-TRAILLENGTH*",system) /* IO variable */
LISPSYM(prin_prev_traillength,"*PRIN-PREV-TRAILLENGTH*",system) /* IO variable */
LISPSYM(prin_jblocks,"*PRIN-JBLOCKS*",system) /* IO variable */
LISPSYM(prin_jbstrings,"*PRIN-JBSTRINGS*",system) /* IO variable */
LISPSYM(prin_jbmodus,"*PRIN-JBMODUS*",system) /* IO variable */
LISPSYM(prin_jblpos,"*PRIN-JBLPOS*",system) /* IO variable */
LISPSYM(format_tabulate,"FORMAT-TABULATE",system) /* see io.d and format.lisp */ /* ABI */
LISPSYM(load_forms,"*LOAD-FORMS*",system) /* see IO, COMPILER, LOADFORM */ /* ABI */
LISPSYM(terminal_read_open_object,"*TERMINAL-READ-OPEN-OBJECT*",system) /* IO */
LISPSYM(terminal_read_stream,"*TERMINAL-READ-STREAM*",system) /* IO */
LISPSYM(backquote,"BACKQUOTE",system) /* marker in IO */
LISPSYM(splice,"SPLICE",system) /* marker in IO */
LISPSYM(nsplice,"NSPLICE",system) /* marker in IO */
LISPSYM(unquote,"UNQUOTE",system) /* marker in IO */
LISPSYM(structure_print,"STRUCTURE-PRINT",system) /* property in IO */
LISPSYM(defstruct_description,"DEFSTRUCT-DESCRIPTION",system) /* property in IO */
LISPSYM(print_object,"PRINT-OBJECT",clos) /* function for IO */
LISPSYM(trace_values,"*TRACE-VALUES*",ext) /* variable in EVAL */
LISPSYM(setf_function,"SETF-FUNCTION",system) /* property in EVAL */
LISPSYM(lambda,"LAMBDA",lisp) /* marker in EVAL */
LISPSYM(LLoptional,"&OPTIONAL",lisp) /* lambda list marker in EVAL */
LISPSYM(LLkey,"&KEY",lisp) /* lambda list marker in EVAL */
LISPSYM(LLallow_other_keys,"&ALLOW-OTHER-KEYS",lisp) /* lambda list marker in EVAL */
LISPSYM(LLrest,"&REST",lisp) /* lambda list marker in EVAL */
LISPSYM(LLaux,"&AUX",lisp) /* lambda list marker in EVAL */
LISPSYM(LLbody,"&BODY",lisp) /* lambda list marker in EVAL */
LISPSYM(macro,"MACRO",system) /* marker in EVAL, type in PREDTYPE */
LISPSYM(special,"SPECIAL",lisp) /* declaration-specifier in EVAL */
LISPSYM(notspecial,"NOTSPECIAL",ext) /* declaration-specifier in EVAL */
LISPSYM(source,"SOURCE",system) /* declaration-specifier in EVAL */
LISPSYM(optimize,"OPTIMIZE",lisp) /* declaration-specifier in EVAL */
LISPSYM(declaration,"DECLARATION",lisp) /* declaration-specifier in EVAL */
LISPSYM(note_optimize,"NOTE-OPTIMIZE",system) /* function for CONTROL */
LISPSYM(compile_lambda,"COMPILE-LAMBDA",system) /* function for EVAL */
LISPSYM(expand_lambdabody_main,"%EXPAND-LAMBDABODY-MAIN",system) /* function for EVAL */
LISPSYM(compile,"COMPILE",lisp) /* declaration-specifier and function for EVAL */
#ifdef DEBUG_EVAL
LISPSYM(funcall_trace_output,"*FUNCALL-TRACE-OUTPUT*",system) /* variable in EVAL */
#endif
LISPSYM(evalhookstern,"*EVALHOOK*",custom) /* variable in EVAL */
LISPSYM(applyhookstern,"*APPLYHOOK*",custom) /* variable in EVAL */
LISPSYM(macroexpand_hook,"*MACROEXPAND-HOOK*",lisp) /* variable in EVAL */
LISPSYM(lambda_parameters_limit,"LAMBDA-PARAMETERS-LIMIT",lisp) /* constant in EVAL */
LISPSYM(call_arguments_limit,"CALL-ARGUMENTS-LIMIT",lisp) /* constant in EVAL */
LISPSYM(multiple_values_limit,"MULTIPLE-VALUES-LIMIT",lisp) /* constant in EVAL */
LISPSYM(jmpbuf_size,"*JMPBUF-SIZE*",system) /* constant in EVAL for COMPILER */
LISPSYM(big_endian,"*BIG-ENDIAN*",system) /* constant in EVAL for COMPILER */
LISPSYM(Klambda,"LAMBDA",keyword) /* marker in EVAL */
LISPSYM(keyword,"KEYWORD",lisp) /* type for EVAL */
LISPSYM(plus2,"++",lisp) /* variable in DEBUG */
LISPSYM(plus3,"+++",lisp) /* variable in DEBUG */
LISPSYM(mal2,"**",lisp) /* variable in DEBUG */
LISPSYM(mal3,"***",lisp) /* variable in DEBUG */
LISPSYM(durch2,"//",lisp) /* variable in DEBUG */
LISPSYM(durch3,"///",lisp) /* variable in DEBUG */
LISPSYM(driverstern,"*DRIVER*",ext) /* variable in DEBUG */
LISPSYM(break_driver,"*BREAK-DRIVER*",ext) /* variable in DEBUG */
LISPSYM(break_count,"*BREAK-COUNT*",system) /* variable in DEBUG */
LISPSYM(recurse_count_standard_output,"*RECURSE-COUNT-STANDARD-OUTPUT*",system) /* variable in DEBUG */
LISPSYM(recurse_count_debug_io,"*RECURSE-COUNT-DEBUG-IO*",system) /* variable in DEBUG */
LISPSYM(frame_limit1,"*FRAME-LIMIT1*",system) /* variable in DEBUG */
LISPSYM(frame_limit2,"*FRAME-LIMIT2*",system) /* variable in DEBUG */
LISPSYM(setf,"SETF",lisp) /* marker in CONTROL */
LISPSYM(psetf,"PSETF",lisp) /* marker in CONTROL */
LISPSYM(multiple_value_setf,"MULTIPLE-VALUE-SETF",system) /* marker in CONTROL */
LISPSYM(make_macro_expander,"MAKE-MACRO-EXPANDER",system) /* function for CONTROL */
LISPSYM(make_funmacro_expander,"MAKE-FUNMACRO-EXPANDER",system) /* function for CONTROL */
LISPSYM(type_for_discrimination,"TYPE-FOR-DISCRIMINATION",system) /* function for CONTROL */
LISPSYM(pthe,"%THE",system) /* function for CONTROL */
LISPSYM(compile_form,"COMPILE-FORM",system) /* function for CONTROL */
LISPSYM(otherwise,"OTHERWISE",lisp) /* marker in CONTROL */
LISPSYM(inline,"INLINE",lisp) /* declaration-specifier in CONTROL */
LISPSYM(notinline,"NOTINLINE",lisp) /* declaration-specifier in CONTROL */
LISPSYM(get_funname_symbol,"GET-FUNNAME-SYMBOL",system) /* function for CONTROL */
LISPSYM(inlinable,"INLINABLE",system) /* property in CONTROL */
LISPSYM(constant_inline,"CONSTANT-INLINE",ext) /* declaration-specifier in CONTROL */
LISPSYM(constant_notinline,"CONSTANT-NOTINLINE",ext) /* declaration-specifier in CONTROL */
LISPSYM(constant_inlinable,"CONSTANT-INLINABLE",system) /* property in CONTROL */
LISPSYM(boolean,"BOOLEAN",lisp) /* type in PREDTYPE */
LISPSYM(symbol,"SYMBOL",lisp) /* type in PREDTYPE */
LISPSYM(address,"ADDRESS",ext) /* type in PREDTYPE */
LISPSYM(file_stream,"FILE-STREAM",lisp) /* type in PREDTYPE */
LISPSYM(synonym_stream,"SYNONYM-STREAM",lisp) /* type in PREDTYPE */
LISPSYM(broadcast_stream,"BROADCAST-STREAM",lisp) /* type in PREDTYPE */
LISPSYM(concatenated_stream,"CONCATENATED-STREAM",lisp) /* type in PREDTYPE */
LISPSYM(two_way_stream,"TWO-WAY-STREAM",lisp) /* type in PREDTYPE */
LISPSYM(echo_stream,"ECHO-STREAM",lisp) /* type in PREDTYPE */
LISPSYM(string_stream,"STRING-STREAM",lisp) /* type in PREDTYPE */
LISPSYM(stream,"STREAM",lisp) /* type in PREDTYPE */
LISPSYM(package,"PACKAGE",lisp) /* type in PREDTYPE */
LISPSYM(readtable,"READTABLE",lisp) /* type in PREDTYPE */
LISPSYM(special_operator,"SPECIAL-OPERATOR",ext) /* type in PREDTYPE */
LISPSYM(load_time_eval,"LOAD-TIME-EVAL",ext) /* type in PREDTYPE */
LISPSYM(symbol_macro,"SYMBOL-MACRO",ext) /* type in PREDTYPE */
LISPSYM(global_symbol_macro,"GLOBAL-SYMBOL-MACRO",ext) /* type in PREDTYPE */
LISPSYM(function_macro,"FUNCTION-MACRO",ext) /* type in PREDTYPE */
LISPSYM(encoding,"ENCODING",ext) /* type in PREDTYPE */
#ifdef FOREIGN
LISPSYM(foreign_pointer,"FOREIGN-POINTER",ext) /* type in PREDTYPE */
#endif
#ifdef DYNAMIC_FFI
LISPSYM(foreign_address,"FOREIGN-ADDRESS",ffi) /* type in PREDTYPE */
LISPSYM(foreign_variable,"FOREIGN-VARIABLE",ffi) /* type in PREDTYPE */
LISPSYM(foreign_function,"FOREIGN-FUNCTION",ffi) /* type in PREDTYPE */
#endif
LISPSYM(weak_pointer,"WEAK-POINTER",ext) /* type in PREDTYPE */
LISPSYM(weak_list,"WEAK-LIST",ext) /* type in PREDTYPE */
LISPSYM(weak_alist,"WEAK-ALIST",ext) /* type in PREDTYPE */
LISPSYM(weak_mapping,"WEAK-MAPPING",ext) /* type in PREDTYPE */
LISPSYM(finalizer,"FINALIZER",ext) /* type in PREDTYPE */
#ifdef YET_ANOTHER_RECORD
LISPSYM(yet_another,"YET-ANOTHER",ext) /* type in PREDTYPE */
#endif
LISPSYM(internal_weak_list,"INTERNAL-WEAK-LIST",system) /* type in PREDTYPE */
LISPSYM(weak_and_relation,"WEAK-AND-RELATION",ext) /* type in PREDTYPE */
LISPSYM(weak_or_relation,"WEAK-OR-RELATION",ext) /* type in PREDTYPE */
LISPSYM(weak_and_mapping,"WEAK-AND-MAPPING",ext) /* type in PREDTYPE */
LISPSYM(weak_or_mapping,"WEAK-OR-MAPPING",ext) /* type in PREDTYPE */
LISPSYM(internal_weak_alist,"INTERNAL-WEAK-ALIST",system) /* type in PREDTYPE */
LISPSYM(internal_weak_hashed_alist,"INTERNAL-WEAK-HASHED-ALIST",system) /* type in PREDTYPE */
LISPSYM(compiled_function,"COMPILED-FUNCTION",lisp) /* type in PREDTYPE */
LISPSYM(frame_pointer,"FRAME-POINTER",system) /* type in PREDTYPE */
LISPSYM(read_label,"READ-LABEL",system) /* type in PREDTYPE */
LISPSYM(system_internal,"SYSTEM-INTERNAL",system) /* type in PREDTYPE */
LISPSYM(fixnum,"FIXNUM",lisp) /* type in PREDTYPE */
LISPSYM(bignum,"BIGNUM",lisp) /* type in PREDTYPE */
LISPSYM(ratio,"RATIO",lisp) /* type in PREDTYPE */
LISPSYM(short_float,"SHORT-FLOAT",lisp) /* type in PREDTYPE */
LISPSYM(single_float,"SINGLE-FLOAT",lisp) /* type in PREDTYPE */
LISPSYM(double_float,"DOUBLE-FLOAT",lisp) /* type in PREDTYPE */
LISPSYM(long_float,"LONG-FLOAT",lisp) /* type in PREDTYPE */
LISPSYM(standard_generic_function,"STANDARD-GENERIC-FUNCTION",clos) /* type in PREDTYPE */
LISPSYM(closclass,"CLOSCLASS",clos) /* marker in PREDTYPE */
LISPSYM(typep,"TYPEP",lisp) /* function for PREDTYPE */
LISPSYM(deftype_expander,"DEFTYPE-EXPANDER",system) /* property in PREDTYPE */
LISPSYM(deftype_depth_limit,"*DEFTYPE-DEPTH-LIMIT*",custom) /* PREDTYPE variable */
LISPSYM(coerce_fixnum_char_ansi,"*COERCE-FIXNUM-CHAR-ANSI*",custom) /* variable for PREDTYPE */
LISPSYM(gc_statistics_stern,"*GC-STATISTICS*",system) /* variable for PREDTYPE */
LISPSYM(recurse_count_gc_statistics,"*RECURSE-COUNT-GC-STATISTICS*",system) /* variable in PREDTYPE */
LISPSYM(traced_definition,"TRACED-DEFINITION",system) /* property in SYMBOL */
LISPSYM(gensym_counter,"*GENSYM-COUNTER*",lisp) /* variable in SYMBOL */
LISPSYM(pprint_first_newline,"*PPRINT-FIRST-NEWLINE*",custom) /* io.d:pr_enter_1() */
LISPSYM(inhibit_floating_point_underflow,"*INHIBIT-FLOATING-POINT-UNDERFLOW*",system) /* variable in LISPARIT */ /* ABI */
LISPSYM(warn_on_floating_point_contagion,"*WARN-ON-FLOATING-POINT-CONTAGION*",custom)
LISPSYM(floating_point_contagion_ansi,"*FLOATING-POINT-CONTAGION-ANSI*",custom)
LISPSYM(warn_on_floating_point_rational_contagion,"*WARN-ON-FLOATING-POINT-RATIONAL-CONTAGION*",custom)
LISPSYM(floating_point_rational_contagion_ansi,"*FLOATING-POINT-RATIONAL-CONTAGION-ANSI*",custom)
LISPSYM(phase_ansi,"*PHASE-ANSI*",custom)
LISPSYM(loop_ansi,"*LOOP-ANSI*",custom)
LISPSYM(defun_accept_specialized_lambda_list,"*DEFUN-ACCEPT-SPECIALIZED-LAMBDA-LIST*",custom)
LISPSYM(specialized_lambda_list_to_ordinary,"SPECIALIZED-LAMBDA-LIST-TO-ORDINARY",system)
LISPSYM(pi,"PI",lisp) /* variable in LISPARIT */
LISPSYM(number,"NUMBER",lisp) /* type for LISPARIT */
LISPSYM(real,"REAL",lisp) /* type for LISPARIT */
LISPSYM(most_positive_fixnum,"MOST-POSITIVE-FIXNUM",lisp) /* constant in LISPARIT */
LISPSYM(most_negative_fixnum,"MOST-NEGATIVE-FIXNUM",lisp) /* constant in LISPARIT */
LISPSYM(most_positive_short_float,"MOST-POSITIVE-SHORT-FLOAT",lisp) /* constant in LISPARIT */
LISPSYM(least_positive_short_float,"LEAST-POSITIVE-SHORT-FLOAT",lisp) /* constant in LISPARIT */
LISPSYM(least_negative_short_float,"LEAST-NEGATIVE-SHORT-FLOAT",lisp) /* constant in LISPARIT */
LISPSYM(most_negative_short_float,"MOST-NEGATIVE-SHORT-FLOAT",lisp) /* constant in LISPARIT */
LISPSYM(most_positive_single_float,"MOST-POSITIVE-SINGLE-FLOAT",lisp) /* constant in LISPARIT */
LISPSYM(least_positive_single_float,"LEAST-POSITIVE-SINGLE-FLOAT",lisp) /* constant in LISPARIT */
LISPSYM(least_negative_single_float,"LEAST-NEGATIVE-SINGLE-FLOAT",lisp) /* constant in LISPARIT */
LISPSYM(most_negative_single_float,"MOST-NEGATIVE-SINGLE-FLOAT",lisp) /* constant in LISPARIT */
LISPSYM(most_positive_double_float,"MOST-POSITIVE-DOUBLE-FLOAT",lisp) /* constant in LISPARIT */
LISPSYM(least_positive_double_float,"LEAST-POSITIVE-DOUBLE-FLOAT",lisp) /* constant in LISPARIT */
LISPSYM(least_negative_double_float,"LEAST-NEGATIVE-DOUBLE-FLOAT",lisp) /* constant in LISPARIT */
LISPSYM(most_negative_double_float,"MOST-NEGATIVE-DOUBLE-FLOAT",lisp) /* constant in LISPARIT */
LISPSYM(most_positive_long_float,"MOST-POSITIVE-LONG-FLOAT",lisp) /* variable in LISPARIT */
LISPSYM(least_positive_long_float,"LEAST-POSITIVE-LONG-FLOAT",lisp) /* variable in LISPARIT */
LISPSYM(least_negative_long_float,"LEAST-NEGATIVE-LONG-FLOAT",lisp) /* variable in LISPARIT */
LISPSYM(most_negative_long_float,"MOST-NEGATIVE-LONG-FLOAT",lisp) /* variable in LISPARIT */
LISPSYM(least_positive_normalized_long_float,"LEAST-POSITIVE-NORMALIZED-LONG-FLOAT",lisp) /* variable in LISPARIT */
LISPSYM(least_negative_normalized_long_float,"LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT",lisp) /* variable in LISPARIT */
LISPSYM(short_float_epsilon,"SHORT-FLOAT-EPSILON",lisp) /* constant in LISPARIT */
LISPSYM(single_float_epsilon,"SINGLE-FLOAT-EPSILON",lisp) /* constant in LISPARIT */
LISPSYM(double_float_epsilon,"DOUBLE-FLOAT-EPSILON",lisp) /* constant in LISPARIT */
LISPSYM(long_float_epsilon,"LONG-FLOAT-EPSILON",lisp) /* variable in LISPARIT */
LISPSYM(short_float_negative_epsilon,"SHORT-FLOAT-NEGATIVE-EPSILON",lisp) /* constant in LISPARIT */
LISPSYM(single_float_negative_epsilon,"SINGLE-FLOAT-NEGATIVE-EPSILON",lisp) /* constant in LISPARIT */
LISPSYM(double_float_negative_epsilon,"DOUBLE-FLOAT-NEGATIVE-EPSILON",lisp) /* constant in LISPARIT */
LISPSYM(long_float_negative_epsilon,"LONG-FLOAT-NEGATIVE-EPSILON",lisp) /* variable in LISPARIT */
LISPSYM(default_float_format,"*DEFAULT-FLOAT-FORMAT*",custom) /* variable in LISPARIT */
LISPSYM(read_default_float_format,"*READ-DEFAULT-FLOAT-FORMAT*",lisp) /* variable in LISPARIT */
LISPSYM(write_float_decimal,"WRITE-FLOAT-DECIMAL",system) /* function for LISPARIT */
LISPSYM(random_state_stern,"*RANDOM-STATE*",lisp) /* variable in LISPARIT */
#ifdef UNICODE
LISPSYM(base64,"BASE64",charset)
LISPSYM(unicode_16,"UNICODE-16",charset)
LISPSYM(unicode_16_big_endian,"UNICODE-16-BIG-ENDIAN",charset)
LISPSYM(unicode_16_little_endian,"UNICODE-16-LITTLE-ENDIAN",charset)
LISPSYM(unicode_32,"UNICODE-32",charset)
LISPSYM(unicode_32_big_endian,"UNICODE-32-BIG-ENDIAN",charset)
LISPSYM(unicode_32_little_endian,"UNICODE-32-LITTLE-ENDIAN",charset)
LISPSYM(utf_8,"UTF-8",charset)
LISPSYM(java,"JAVA",charset)
/* Natively supported 8-bit encodings. */
LISPSYM(ascii,"ASCII",charset) /* ----------------------+ These must be */
LISPSYM(iso8859_1,"ISO-8859-1",charset) /*              | in the same order */
LISPSYM(iso8859_2,"ISO-8859-2",charset) /*              | as in encoding.d */
LISPSYM(iso8859_3,"ISO-8859-3",charset)
LISPSYM(iso8859_4,"ISO-8859-4",charset)
LISPSYM(iso8859_5,"ISO-8859-5",charset)
LISPSYM(iso8859_6,"ISO-8859-6",charset)
LISPSYM(iso8859_7,"ISO-8859-7",charset)
LISPSYM(iso8859_8,"ISO-8859-8",charset)
LISPSYM(iso8859_9,"ISO-8859-9",charset)
LISPSYM(iso8859_10,"ISO-8859-10",charset)
LISPSYM(iso8859_13,"ISO-8859-13",charset)
LISPSYM(iso8859_14,"ISO-8859-14",charset)
LISPSYM(iso8859_15,"ISO-8859-15",charset)
LISPSYM(iso8859_16,"ISO-8859-16",charset)
LISPSYM(koi8_r,"KOI8-R",charset)
LISPSYM(koi8_u,"KOI8-U",charset)
LISPSYM(mac_arabic,"MAC-ARABIC",charset)
LISPSYM(mac_centraleurope,"MAC-CENTRAL-EUROPE",charset)
LISPSYM(mac_croatian,"MAC-CROATIAN",charset)
LISPSYM(mac_cyrillic,"MAC-CYRILLIC",charset)
LISPSYM(mac_dingbat,"MAC-DINGBAT",charset)
LISPSYM(mac_greek,"MAC-GREEK",charset)
LISPSYM(mac_hebrew,"MAC-HEBREW",charset)
LISPSYM(mac_iceland,"MAC-ICELAND",charset)
LISPSYM(mac_roman,"MAC-ROMAN",charset)
LISPSYM(mac_romania,"MAC-ROMANIA",charset)
LISPSYM(mac_symbol,"MAC-SYMBOL",charset)
LISPSYM(mac_thai,"MAC-THAI",charset)
LISPSYM(mac_turkish,"MAC-TURKISH",charset)
LISPSYM(mac_ukraine,"MAC-UKRAINE",charset)
LISPSYM(cp437_ms,"CP437",charset)
LISPSYM(cp437_ibm,"CP437-IBM",charset)
LISPSYM(cp737,"CP737",charset)
LISPSYM(cp775,"CP775",charset)
LISPSYM(cp850,"CP850",charset)
LISPSYM(cp852_ms,"CP852",charset)
LISPSYM(cp852_ibm,"CP852-IBM",charset)
LISPSYM(cp855,"CP855",charset)
LISPSYM(cp857,"CP857",charset)
LISPSYM(cp860_ms,"CP860",charset)
LISPSYM(cp860_ibm,"CP860-IBM",charset)
LISPSYM(cp861_ms,"CP861",charset)
LISPSYM(cp861_ibm,"CP861-IBM",charset)
LISPSYM(cp862_ms,"CP862",charset)
LISPSYM(cp862_ibm,"CP862-IBM",charset)
LISPSYM(cp863_ms,"CP863",charset)
LISPSYM(cp863_ibm,"CP863-IBM",charset)
LISPSYM(cp864_ms,"CP864",charset)
LISPSYM(cp864_ibm,"CP864-IBM",charset)
LISPSYM(cp865_ms,"CP865",charset)
LISPSYM(cp865_ibm,"CP865-IBM",charset)
LISPSYM(cp866,"CP866",charset)
LISPSYM(cp869_ms,"CP869",charset)
LISPSYM(cp869_ibm,"CP869-IBM",charset)
LISPSYM(cp874_ms,"CP874",charset)
LISPSYM(cp874_ibm,"CP874-IBM",charset)
LISPSYM(cp1250,"CP1250",charset)
LISPSYM(cp1251,"CP1251",charset)
LISPSYM(cp1252,"CP1252",charset)
LISPSYM(cp1253,"CP1253",charset)
LISPSYM(cp1254,"CP1254",charset)
LISPSYM(cp1256,"CP1256",charset)
LISPSYM(cp1257,"CP1257",charset)
LISPSYM(hp_roman8,"HP-ROMAN8",charset) /*               | */
LISPSYM(nextstep,"NEXTSTEP",charset) /*                 | */
LISPSYM(jisx0201,"JIS_X0201",charset) /* ---------------+ */
/* Aliases. */
LISPSYM(ucs_2,"UCS-2",charset)
LISPSYM(ucs_4,"UCS-4",charset)
LISPSYM(macintosh,"MACINTOSH",charset)
LISPSYM(windows_1250,"WINDOWS-1250",charset)
LISPSYM(windows_1251,"WINDOWS-1251",charset)
LISPSYM(windows_1252,"WINDOWS-1252",charset)
LISPSYM(windows_1253,"WINDOWS-1253",charset)
LISPSYM(windows_1254,"WINDOWS-1254",charset)
LISPSYM(windows_1256,"WINDOWS-1256",charset)
LISPSYM(windows_1257,"WINDOWS-1257",charset)
#if defined(GNU_LIBICONV) || (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 2))
/* All documented encodings of libiconv, except for those which are already
   builtin without libiconv. */
#ifdef GNU_LIBICONV
LISPSYM(koi8_ru,"KOI8-RU",charset) /* ------------------+ This block is */
#endif
LISPSYM(cp1255,"CP1255",charset) /*                     | referenced in */
LISPSYM(cp1258,"CP1258",charset) /*                     | encoding.d */
LISPSYM(euc_jp,"EUC-JP",charset)
LISPSYM(shift_jis,"SHIFT-JIS",charset)
LISPSYM(cp932,"CP932",charset)
LISPSYM(iso_2022_jp,"ISO-2022-JP",charset)
#if defined(GNU_LIBICONV) || (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 3))
LISPSYM(iso_2022_jp_2,"ISO-2022-JP-2",charset)
#endif
#ifdef GNU_LIBICONV
LISPSYM(iso_2022_jp_1,"ISO-2022-JP-1",charset)
#endif
LISPSYM(euc_cn,"EUC-CN",charset)
#ifdef GNU_LIBICONV
LISPSYM(hz,"HZ",charset)
#endif
LISPSYM(gbk,"GBK",charset)
LISPSYM(cp936,"CP936",charset)
LISPSYM(gb18030,"GB18030",charset)
LISPSYM(euc_tw,"EUC-TW",charset)
LISPSYM(big5,"BIG5",charset)
LISPSYM(cp950,"CP950",charset)
LISPSYM(big5_hkscs,"BIG5-HKSCS",charset)
LISPSYM(iso_2022_cn,"ISO-2022-CN",charset)
LISPSYM(iso_2022_cn_ext,"ISO-2022-CN-EXT",charset)
LISPSYM(euc_kr,"EUC-KR",charset)
LISPSYM(cp949,"CP949",charset)
LISPSYM(iso_2022_kr,"ISO-2022-KR",charset)
LISPSYM(johab,"JOHAB",charset)
#if defined(GNU_LIBICONV) || (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 3))
LISPSYM(armscii_8,"ARMSCII-8",charset)
#endif
LISPSYM(georgian_academy,"GEORGIAN-ACADEMY",charset)
LISPSYM(georgian_ps,"GEORGIAN-PS",charset)
LISPSYM(tis_620,"TIS-620",charset)
#ifdef GNU_LIBICONV
LISPSYM(mulelao_1,"MULELAO-1",charset)
#endif
LISPSYM(cp1133,"CP1133",charset)
LISPSYM(viscii,"VISCII",charset)
#if defined(GNU_LIBICONV) || (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 3))
LISPSYM(tcvn,"TCVN",charset)
#endif
#if defined(GNU_LIBICONV) && defined(UNIX_AIX)
LISPSYM(cp856,"CP856",charset)
LISPSYM(cp922,"CP922",charset)
LISPSYM(cp943,"CP943",charset)
LISPSYM(cp1046,"CP1046",charset)
LISPSYM(cp1124,"CP1124",charset)
LISPSYM(cp1129,"CP1129",charset)
#endif
LISPSYM(utf_16,"UTF-16",charset) /*                     | */
LISPSYM(utf_7,"UTF-7",charset) /* ----------------------+ */
/* Aliases. */
#if defined(GNU_LIBICONV) || (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 2))
LISPSYM(windows_1255,"WINDOWS-1255",charset)
LISPSYM(windows_1258,"WINDOWS-1258",charset)
#endif
#endif
#endif
LISPSYM(english,"ENGLISH",i18n) /* a language for MISC */
#ifdef GNU_GETTEXT
LISPSYM(danish,"DANSK",i18n) /* a language for MISC */
LISPSYM(german,"DEUTSCH",i18n) /* a language for MISC */
LISPSYM(french," FRAN\303\207AIS",i18n) /* a language for MISC [' ' => utf8] */
LISPSYM(spanish," ESPA\303\221OL",i18n) /* a language for MISC [' ' => utf8] */
LISPSYM(dutch,"NEDERLANDS",i18n) /* a language for MISC */
LISPSYM(russian," \320\240\320\243\320\241\320\241\320\232\320\230\320\231",i18n) /* a language for MISC [' ' => utf8] */
#endif
LISPSYM(init_hooks,"*INIT-HOOKS*",custom) /* variable for SPVW */
LISPSYM(fini_hooks,"*FINI-HOOKS*",custom) /* variable for SPVW */
LISPSYM(quiet,"*QUIET*",system) /* variable for SPVW */
LISPSYM(norc,"*NORC*",system) /* variable for SPVW */
LISPSYM(Klisting,"LISTING",keyword) /* argument for SPVW */
LISPSYM(Koutput_file,"OUTPUT-FILE",keyword) /* argument for SPVW */
LISPSYM(compile_file,"COMPILE-FILE",lisp) /* function for SPVW */
LISPSYM(load_compiling,"*LOAD-COMPILING*",custom) /* variable for SPVW */
LISPSYM(load_verbose,"*LOAD-VERBOSE*",lisp) /* variable for SPVW */
LISPSYM(load_print,"*LOAD-PRINT*",lisp) /* variable for SPVW */
LISPSYM(load_echo,"*LOAD-ECHO*",custom) /* variable for SPVW */
LISPSYM(compile_print,"*COMPILE-PRINT*",lisp) /* variable for SPVW */
LISPSYM(compile_verbose,"*COMPILE-VERBOSE*",lisp) /* variable for SPVW */
LISPSYM(report_error_print_backtrace,"*REPORT-ERROR-PRINT-BACKTRACE*",custom) /* variable for SPVW */
LISPSYM(args,"*ARGS*",ext) /* variable in SPVW */
LISPSYM(appease_cerror,"APPEASE-CERROR",system) /* function for SPVW */
LISPSYM(exitunconditionally,"EXITUNCONDITIONALLY",system) /* function for SPVW*/
LISPSYM(exitonerror,"EXITONERROR",system) /* function for SPVW */
LISPSYM(abortonerror,"ABORTONERROR",system) /* function for SPVW */
LISPSYM(interrupt_condition,"INTERRUPT-CONDITION",system) /*condition for SPVW*/
LISPSYM(serious_condition,"SERIOUS-CONDITION",lisp) /* condition for SPVW */
LISPSYM(set_global_handler,"SET-GLOBAL-HANDLER",ext) /* function for SPVW */
LISPSYM(global_handler,"GLOBAL-HANDLER",system) /* gf for EVAL */
LISPSYM(wait_keypress,"WAIT-KEYPRESS",system) /* function for SPVW */
#ifdef UNIX
LISPSYM(disassemble_use_live_process,"*DISASSEMBLE-USE-LIVE-PROCESS*",system) /* variable in SPVW */
#endif
/* ---------- FFI ---------- */
#ifdef DYNAMIC_FFI
/* LISPSYM(boolean,"BOOLEAN",ffi) */
/* LISPSYM(char,"CHAR",ffi) */
LISPSYM(uchar,"UCHAR",ffi)
LISPSYM(short,"SHORT",ffi)
LISPSYM(ushort,"USHORT",ffi)
LISPSYM(int,"INT",ffi)
LISPSYM(uint,"UINT",ffi)
LISPSYM(long,"LONG",ffi)
LISPSYM(ulong,"ULONG",ffi)
LISPSYM(uint8,"UINT8",ffi)
LISPSYM(sint8,"SINT8",ffi)
LISPSYM(uint16,"UINT16",ffi)
LISPSYM(sint16,"SINT16",ffi)
LISPSYM(uint32,"UINT32",ffi)
LISPSYM(sint32,"SINT32",ffi)
LISPSYM(uint64,"UINT64",ffi)
LISPSYM(sint64,"SINT64",ffi)
/* LISPSYM(single_float,"SINGLE-FLOAT",ffi) */
/* LISPSYM(double_float,"DOUBLE-FLOAT",ffi) */
LISPSYM(c_pointer,"C-POINTER",ffi)
LISPSYM(c_string,"C-STRING",ffi)
LISPSYM(c_struct,"C-STRUCT",ffi)
LISPSYM(c_union,"C-UNION",ffi)
LISPSYM(c_array,"C-ARRAY",ffi)
LISPSYM(c_array_max,"C-ARRAY-MAX",ffi)
LISPSYM(c_function,"C-FUNCTION",ffi)
LISPSYM(c_ptr,"C-PTR",ffi)
LISPSYM(c_ptr_null,"C-PTR-NULL",ffi)
LISPSYM(c_array_ptr,"C-ARRAY-PTR",ffi)
LISPSYM(fv_flag_readonly,"FV-FLAG-READONLY",ffi) /* constant in FFI */
LISPSYM(fv_flag_malloc_free,"FV-FLAG-MALLOC-FREE",ffi) /* constant in FFI */
LISPSYM(ff_flag_alloca,"FF-FLAG-ALLOCA",ffi) /* constant in FFI */
LISPSYM(ff_flag_malloc_free,"FF-FLAG-MALLOC-FREE",ffi) /* constant in FFI */
LISPSYM(ff_flag_out,"FF-FLAG-OUT",ffi) /* constant in FFI */
LISPSYM(ff_flag_in_out,"FF-FLAG-IN-OUT",ffi) /* constant in FFI */
LISPSYM(ff_language_asm,"FF-LANGUAGE-ASM",ffi) /* constant in FFI */
LISPSYM(ff_language_c,"FF-LANGUAGE-C",ffi) /* constant in FFI */
LISPSYM(ff_language_ansi_c,"FF-LANGUAGE-ANSI-C",ffi) /* constant in FFI */
LISPSYM(ff_language_stdcall,"FF-LANGUAGE-STDCALL",ffi) /* constant in FFI */
LISPSYM(foreign_call_in,"FOREIGN-CALL-IN",ffi) /* error message in FFI */
#endif
#ifdef HAVE_AFFI
LISPSYM(affi_libcall,"%LIBCALL",system)
LISPSYM(mem_read,"MEM-READ",system)
LISPSYM(mem_write,"MEM-WRITE",system)
LISPSYM(mem_write_vector,"MEM-WRITE-VECTOR",system)
LISPSYM(affi_nonzerop,"NZERO-POINTER-P",system)
#endif
