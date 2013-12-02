/*
 * list of all objects known to the C-program ("program-constants")
 * Bruno Haible 1990-2005
 * Sam Steingold 1998-2005
 * German comments translated into English: Stefan Kain 2002-02-20

 The symbols are already treated specially in CONSTSYM.
 A table of all other objects known to the C-program is maintained here.

 The macro LISPOBJ declares a LISP object.
 LISPOBJ(name,initstring)
 > name: object is addressable as object_tab.name or as O(name)
 > initstring: initialization-string in LISP syntax */

# expander for the declaration of the object-table:
  #define LISPOBJ_A(name,initstring)  \
    gcv_object_t name;

# expander for the initialization of the object-table:
  #define LISPOBJ_B(name,initstring)  \
    NIL,
  #define LISPOBJ_C(name,initstring)  \
    initstring,

# Which expander is used, must be configured by the main file.

# The macro LISPOBJ_S declares a LISP string.
# > name: object is addressable as object_tab.name or as O(name)
# > initstring: initialization-string in C-syntax, may not contain
#               backslashes
  #define LISPOBJ_S(name,initstring)  \
    LISPOBJ(name,"\"" initstring "\"")

# for SPVW.D:
  # chained list of all active weak-pointers:
  LISPOBJ(all_weakpointers,"0")
  # list of all finalizers:
  LISPOBJ(all_finalizers,"0")
  # During GC: the list of finalizers to be processed after the GC:
  LISPOBJ(pending_finalizers,"0")
# for ENCODING.D:
  # Encodings for which both the charset and the line-terminator matter:
  # The default encoding for file streams, pipe streams, socket streams.
  LISPOBJ(default_file_encoding,".")
  # The encoding of the terminal stream.
  LISPOBJ(terminal_encoding,".")
  # Encodings for which only the charset matters:
  #ifdef UNICODE
    # The encoding of the C strings compiled into the executable.
    LISPOBJ(internal_encoding,".")
    # The encoding of pathnames on the file system.
    LISPOBJ(pathname_encoding,".")
    #if defined(HAVE_FFI) || defined(HAVE_AFFI)
      # The encoding of strings passed through the FFI.
      LISPOBJ(foreign_encoding,".")
      # The encoding of characters passed through the FFI.
      # Must be 1:1, i.e. one of the nls_* encodings.
      LISPOBJ(foreign_8bit_encoding,".")
    #endif
    # The encoding for everything else (environment variables, command-line
    # options etc.)
    LISPOBJ(misc_encoding,".")
  #endif
  LISPOBJ(type_line_terminator,"(MEMBER :DEFAULT :UNIX :MAC :DOS)")
  LISPOBJ(type_input_error_action,"(OR (MEMBER :ERROR :IGNORE) CHARACTER)")
  LISPOBJ(type_output_error_action,"(OR (MEMBER :ERROR :IGNORE) CHARACTER (UNSIGNED-BYTE 8))")
# for CHARSTRG.D:
  # On change of character-names except of CONSTOBJ.D, also
  # readjust CHARSTRG.D, FORMAT.LISP, IMPNOTES.HTML!
  #ifdef WIN32_CHARNAMES
    # names of characters with codes 0,7,...,13,26,27,32,8,10:
    LISPOBJ(charname_0,"\"Null\"")
    LISPOBJ(charname_7,"\"Bell\"")
    LISPOBJ(charname_8,"\"Backspace\"")
    LISPOBJ(charname_9,"\"Tab\"")
    LISPOBJ(charname_10,"\"Newline\"")
    LISPOBJ(charname_11,"\"Code11\"")
    LISPOBJ(charname_12,"\"Page\"")
    LISPOBJ(charname_13,"\"Return\"")
    LISPOBJ(charname_26,"\"Code26\"")
    LISPOBJ(charname_27,"\"Escape\"")
    LISPOBJ(charname_32,"\"Space\"")
    LISPOBJ(charname_8bis,"\"Rubout\"")
    LISPOBJ(charname_10bis,"\"Linefeed\"")
  #endif
  #ifdef UNIX_CHARNAMES
    LISPOBJ(charname_0bis,"\"Null\"")
    LISPOBJ(charname_7bis,"\"Bell\"")
    LISPOBJ(charname_8bis,"\"Backspace\"")
    LISPOBJ(charname_9bis,"\"Tab\"")
    LISPOBJ(charname_10bis,"\"Newline\"")
    LISPOBJ(charname_10tris,"\"Linefeed\"")
    LISPOBJ(charname_12bis,"\"Page\"")
    LISPOBJ(charname_13bis,"\"Return\"")
    LISPOBJ(charname_27bis,"\"Escape\"")
    LISPOBJ(charname_32bis,"\"Space\"")
    LISPOBJ(charname_127bis,"\"Rubout\"")
    LISPOBJ(charname_127tris,"\"Delete\"")
    LISPOBJ(charname_0,"\"Nul\"")
    LISPOBJ(charname_1,"\"Soh\"")
    LISPOBJ(charname_2,"\"Stx\"")
    LISPOBJ(charname_3,"\"Etx\"")
    LISPOBJ(charname_4,"\"Eot\"")
    LISPOBJ(charname_5,"\"Enq\"")
    LISPOBJ(charname_6,"\"Ack\"")
    LISPOBJ(charname_7,"\"Bel\"")
    LISPOBJ(charname_8,"\"Bs\"")
    LISPOBJ(charname_9,"\"Ht\"")
    LISPOBJ(charname_10,"\"Nl\"")
    LISPOBJ(charname_11,"\"Vt\"")
    LISPOBJ(charname_12,"\"Np\"")
    LISPOBJ(charname_13,"\"Cr\"")
    LISPOBJ(charname_14,"\"So\"")
    LISPOBJ(charname_15,"\"Si\"")
    LISPOBJ(charname_16,"\"Dle\"")
    LISPOBJ(charname_17,"\"Dc1\"")
    LISPOBJ(charname_18,"\"Dc2\"")
    LISPOBJ(charname_19,"\"Dc3\"")
    LISPOBJ(charname_20,"\"Dc4\"")
    LISPOBJ(charname_21,"\"Nak\"")
    LISPOBJ(charname_22,"\"Syn\"")
    LISPOBJ(charname_23,"\"Etb\"")
    LISPOBJ(charname_24,"\"Can\"")
    LISPOBJ(charname_25,"\"Em\"")
    LISPOBJ(charname_26,"\"Sub\"")
    LISPOBJ(charname_27,"\"Esc\"")
    LISPOBJ(charname_28,"\"Fs\"")
    LISPOBJ(charname_29,"\"Gs\"")
    LISPOBJ(charname_30,"\"Rs\"")
    LISPOBJ(charname_31,"\"Us\"")
    LISPOBJ(charname_32,"\"Sp\"")
    # The proposal to add:
    #  constobj.d (UNIX_CHARNAMES): #\Erik is a synonym for #\Null.
    #  LISPOBJ(charname_0tris,"\"Erik\"") # special "honour" for Mr. Nutgum
    # has been rejected because of a seriousness attack.
    LISPOBJ(charname_127,"\"Del\"")
  #endif
# for ARRAY.D:
  LISPOBJ(type_vector_with_fill_pointer,"(AND VECTOR (SATISFIES ARRAY-HAS-FILL-POINTER-P))") # type for error message
# for HASHTABL.D:
 #ifdef GENERATIONAL_GC
  LISPOBJ(gc_count,"0")
 #endif
  LISPOBJ(type_eq_hashfunction,"(MEMBER EXT::FASTHASH-EQ EXT::STABLEHASH-EQ)")
  LISPOBJ(type_eql_hashfunction,"(MEMBER EXT::FASTHASH-EQL EXT::STABLEHASH-EQL)")
  LISPOBJ(type_equal_hashfunction,"(MEMBER EXT::FASTHASH-EQUAL EXT::STABLEHASH-EQUAL)")
  LISPOBJ(type_weak_ht,"(MEMBER :KEY :VALUE :KEY-AND-VALUE :KEY-OR-VALUE NIL)")
# for RECORD.D:
  LISPOBJ(constant_initfunction_code,".")
  LISPOBJ(endless_loop_code,".")
# for WEAK.D:
  LISPOBJ(type_weak_alist,"(MEMBER :KEY :VALUE :KEY-AND-VALUE :KEY-OR-VALUE)")
# for SEQUENCE.D:
  # internal list of all defined sequence-types:
  LISPOBJ(seq_types,"NIL")
  LISPOBJ(type_recognizable_sequence_type,"(SATISFIES SYSTEM::RECOGNIZABLE-SEQUENCE-TYPE-P)") # type for error message
  # keyword-pairs for test_start_end (do not separate pairs!):
  LISPOBJ(kwpair_start,":START")
  LISPOBJ(kwpair_end,":END")
  LISPOBJ(kwpair_start1,":START1")
  LISPOBJ(kwpair_end1,":END1")
  LISPOBJ(kwpair_start2,":START2")
  LISPOBJ(kwpair_end2,":END2")
# for PREDTYPE.D:
  # distinctive marks for classes, are filled by CLOS::%DEFCLOS
  LISPOBJ(class_version_standard_class,"#()")
  LISPOBJ(class_version_structure_class,"#()")
  LISPOBJ(class_version_built_in_class,"#()")
  LISPOBJ(class_defined_class,"CLOS::DEFINED-CLASS")
  LISPOBJ(class_potential_class,"CLOS::POTENTIAL-CLASS")
  # some built-in-classes, are filled by CLOS::%DEFCLOS
  LISPOBJ(class_array,"ARRAY")             # ---+
  LISPOBJ(class_bit_vector,"BIT-VECTOR")   #    |   order
  LISPOBJ(class_character,"CHARACTER")     #    |   coordinated
  LISPOBJ(class_complex,"COMPLEX")         #    |   with clos.lisp!
  LISPOBJ(class_cons,"CONS")
  LISPOBJ(class_float,"FLOAT")
  LISPOBJ(class_function,"FUNCTION")
  LISPOBJ(class_hash_table,"HASH-TABLE")
  LISPOBJ(class_integer,"INTEGER")
  LISPOBJ(class_list,"LIST")
  LISPOBJ(class_null,"NULL")
  LISPOBJ(class_package,"PACKAGE")
  LISPOBJ(class_pathname,"PATHNAME")
  #ifdef LOGICAL_PATHNAMES
  LISPOBJ(class_logical_pathname,"LOGICAL-PATHNAME")
  #endif
  LISPOBJ(class_random_state,"RANDOM-STATE")
  LISPOBJ(class_ratio,"RATIO")
  LISPOBJ(class_readtable,"READTABLE")
  LISPOBJ(class_stream,"STREAM")
  LISPOBJ(class_file_stream,"FILE-STREAM")
  LISPOBJ(class_synonym_stream,"SYNONYM-STREAM")
  LISPOBJ(class_broadcast_stream,"BROADCAST-STREAM")
  LISPOBJ(class_concatenated_stream,"CONCATENATED-STREAM")
  LISPOBJ(class_two_way_stream,"TWO-WAY-STREAM")
  LISPOBJ(class_echo_stream,"ECHO-STREAM")
  LISPOBJ(class_string_stream,"STRING-STREAM")
  LISPOBJ(class_string,"STRING")
  LISPOBJ(class_symbol,"SYMBOL")           #    |
  LISPOBJ(class_t,"T")                     #    |
  LISPOBJ(class_vector,"VECTOR")           # ---+
  LISPOBJ(type_designator_character,"(EXT::DESIGNATOR CHARACTER)")
  #if (base_char_code_limit < char_code_limit)
  LISPOBJ(type_designator_base_char,"(EXT::DESIGNATOR BASE-CHAR)")
  #endif
  LISPOBJ(type_designator_function,"(OR FUNCTION SYMBOL (CONS (EQL SETF) (CONS SYMBOL NULL)) (CONS (EQL LAMBDA)))")
  # Upper bound for the number of structure classes present in the system:
  LISPOBJ(structure_class_count_max,"0")
  # Upper bound for the number of standard classes present in the system:
  LISPOBJ(standard_class_count_max,"0")
  # built-in-types for HEAP-STATISTICS
  LISPOBJ(hs_t,"T")                                 # ---+
  LISPOBJ(hs_cons,"CONS")                           #    |  order
  LISPOBJ(hs_null,"NULL")                           #    |  coordinated
  LISPOBJ(hs_symbol,"SYMBOL")                       #    |  with enum_hs_...
  LISPOBJ(hs_simple_bit_vector,"SIMPLE-BIT-VECTOR") #    |  in predtype.d!
  LISPOBJ(hs_simple_2bit_vector,"EXT::SIMPLE-2BIT-VECTOR")
  LISPOBJ(hs_simple_4bit_vector,"EXT::SIMPLE-4BIT-VECTOR")
  LISPOBJ(hs_simple_8bit_vector,"EXT::SIMPLE-8BIT-VECTOR")
  LISPOBJ(hs_simple_16bit_vector,"EXT::SIMPLE-16BIT-VECTOR")
  LISPOBJ(hs_simple_32bit_vector,"EXT::SIMPLE-32BIT-VECTOR")
  LISPOBJ(hs_simple_nilvector,"EXT::SIMPLE-NIL-VECTOR")
  LISPOBJ(hs_simple_string,"SIMPLE-STRING")
  LISPOBJ(hs_simple_vector,"SIMPLE-VECTOR")
  LISPOBJ(hs_bit_vector,"BIT-VECTOR")
  LISPOBJ(hs_2bit_vector,"EXT::2BIT-VECTOR")
  LISPOBJ(hs_4bit_vector,"EXT::4BIT-VECTOR")
  LISPOBJ(hs_8bit_vector,"EXT::8BIT-VECTOR")
  LISPOBJ(hs_16bit_vector,"EXT::16BIT-VECTOR")
  LISPOBJ(hs_32bit_vector,"EXT::32BIT-VECTOR")
  LISPOBJ(hs_nilvector,"EXT::NIL-VECTOR")
  LISPOBJ(hs_string,"STRING")
  LISPOBJ(hs_vector,"VECTOR")
  LISPOBJ(hs_simple_array,"SIMPLE-ARRAY")
  LISPOBJ(hs_array,"ARRAY")
  LISPOBJ(hs_function,"FUNCTION")
  LISPOBJ(hs_file_stream,"FILE-STREAM")
  LISPOBJ(hs_synonym_stream,"SYNONYM-STREAM")
  LISPOBJ(hs_broadcast_stream,"BROADCAST-STREAM")
  LISPOBJ(hs_concatenated_stream,"CONCATENATED-STREAM")
  LISPOBJ(hs_two_way_stream,"TWO-WAY-STREAM")
  LISPOBJ(hs_echo_stream,"ECHO-STREAM")
  LISPOBJ(hs_string_stream,"STRING-STREAM")
  LISPOBJ(hs_stream,"STREAM")
  LISPOBJ(hs_hash_table,"HASH-TABLE")
  LISPOBJ(hs_package,"PACKAGE")
  LISPOBJ(hs_readtable,"READTABLE")
  LISPOBJ(hs_pathname,"PATHNAME")
  #ifdef LOGICAL_PATHNAMES
  LISPOBJ(hs_logical_pathname,"LOGICAL-PATHNAME")
  #endif
  LISPOBJ(hs_random_state,"RANDOM-STATE")
  LISPOBJ(hs_byte,"BYTE")
  LISPOBJ(hs_special_operator,"EXT::SPECIAL-OPERATOR")
  LISPOBJ(hs_load_time_eval,"EXT::LOAD-TIME-EVAL")
  LISPOBJ(hs_symbol_macro,"EXT::SYMBOL-MACRO")
  LISPOBJ(hs_global_symbol_macro,"EXT::GLOBAL-SYMBOL-MACRO")
  LISPOBJ(hs_macro,"SYS::MACRO")
  LISPOBJ(hs_function_macro,"EXT::FUNCTION-MACRO")
  LISPOBJ(hs_big_read_label,"SYS::READ-LABEL")
  LISPOBJ(hs_encoding,"EXT::ENCODING")
  #ifdef FOREIGN
  LISPOBJ(hs_foreign_pointer,"EXT::FOREIGN-POINTER")
  #endif
  #ifdef DYNAMIC_FFI
  LISPOBJ(hs_foreign_address,"FFI::FOREIGN-ADDRESS")
  LISPOBJ(hs_foreign_variable,"FFI::FOREIGN-VARIABLE")
  LISPOBJ(hs_foreign_function,"FFI::FOREIGN-FUNCTION")
  #endif
  #ifdef HAVE_SMALL_SSTRING
  LISPOBJ(hs_realloc_simple_string,"EXT::FORWARD-POINTER-SIMPLE-STRING")
  #endif
  LISPOBJ(hs_realloc_instance,"EXT::FORWARD-POINTER-INSTANCE")
  LISPOBJ(hs_weakpointer,"EXT::WEAK-POINTER")
  LISPOBJ(hs_weak_list,"EXT::WEAK-LIST")
  LISPOBJ(hs_weak_alist,"EXT::WEAK-ALIST")
  LISPOBJ(hs_weakmapping,"EXT::WEAK-MAPPING")
  LISPOBJ(hs_finalizer,"EXT::FINALIZER")
  #ifdef SOCKET_STREAMS
  LISPOBJ(hs_socket_server,"SOCKET::SOCKET-SERVER")
  #endif
  #ifdef YET_ANOTHER_RECORD
  LISPOBJ(hs_yetanother,"SYS::YETANOTHER")
  #endif
  LISPOBJ(hs_internal_weak_list,"SYS::INTERNAL-WEAK-LIST")
  LISPOBJ(hs_weak_and_relation,"EXT::WEAK-AND-RELATION")
  LISPOBJ(hs_weak_or_relation,"EXT::WEAK-OR-RELATION")
  LISPOBJ(hs_weak_and_mapping,"EXT::WEAK-AND-MAPPING")
  LISPOBJ(hs_weak_or_mapping,"EXT::WEAK-OR-MAPPING")
  LISPOBJ(hs_internal_weak_alist,"SYS::INTERNAL-WEAK-ALIST")
  LISPOBJ(hs_internal_weak_hashed_alist,"SYS::INTERNAL-WEAK-HASHED-ALIST")
  LISPOBJ(hs_system_function,"EXT::SYSTEM-FUNCTION")
  LISPOBJ(hs_bignum,"BIGNUM")
  LISPOBJ(hs_ratio,"RATIO")
  #ifndef IMMEDIATE_FFLOAT
  LISPOBJ(hs_single_float,"SINGLE-FLOAT")
  #endif                                            #    |
  LISPOBJ(hs_double_float,"DOUBLE-FLOAT")           #    |
  LISPOBJ(hs_long_float,"LONG-FLOAT")               #    |
  LISPOBJ(hs_complex,"COMPLEX")                     # ---+
  LISPOBJ(gc_statistics_list,"NIL")
# for PACKAGE.D:
  # internal list of all packages:
  LISPOBJ(all_packages,".")
  # the keyword-package:
  LISPOBJ(keyword_package,".")
  # the charset-package:
  LISPOBJ(charset_package,".")
  # the default-package for *PACKAGE*:
  LISPOBJ(default_package,".")
  # default-use-list:
  LISPOBJ(use_default,"(\"COMMON-LISP\")")
  # default-package for ANSI-CL-compliance:
  LISPOBJ(ansi_user_package_name,"\"COMMON-LISP-USER\"")
# for SYMBOL.D:
  LISPOBJ(gensym_prefix,"\"G\"") # prefix for gensym, a string
# for MISC.D:
  # basic knowledge:
  LISPOBJ_S(lisp_implementation_type_string,"CLISP")
  LISPOBJ_S(lisp_implementation_package_version,PACKAGE_VERSION)
  # we want here the _LINK_ time, but I have no idea about how to get it
#ifdef __DATE__
  LISPOBJ_S(lisp_implementation_version_built_string,__DATE__ __TIME__)
#else
  LISPOBJ(lisp_implementation_version_built_string,"NIL")
#endif
  LISPOBJ(lisp_implementation_version_string,"NIL") # cache
  LISPOBJ(memory_image_timestamp,"NIL") # the dump date of the current image
  LISPOBJ(memory_image_host,"NIL") # the host on which this image was dumped
  /* The date of the last change of the bytecode interpreter
     or the arglist of any built-in function in FUNTAB */
  LISPOBJ(version,"(20050505)")
#ifdef MACHINE_KNOWN
  LISPOBJ(machine_type_string,"NIL")
  LISPOBJ(machine_version_string,"NIL")
  LISPOBJ(machine_instance_string,"NIL")
#endif
  LISPOBJ(software_type,"NIL") # initialized later
 #if defined(GNU)
  LISPOBJ_S(c_compiler_version,__VERSION__)
 #endif
  LISPOBJ(argv,"NIL")
# for I18N.D:
  LISPOBJ(current_language,".")
  LISPOBJ(ansi,"NIL")
# for TIME.D:
 #ifdef TIME_RELATIVE
  # start-universal-time:
  LISPOBJ(start_UT,"NIL")
 #endif
# for ERROR.D:
  # error-message-startstring:
  LISPOBJ_S(error_string1,"*** - ")
  # vector with conditions and simple-conditions:
  LISPOBJ(error_types,"#()")
  # for errors of type TYPE-ERROR:
  LISPOBJ(type_function_name,"(OR SYMBOL (CONS (EQL SETF) (CONS SYMBOL NULL)))")
  LISPOBJ(type_uint8,"(INTEGER 0 255)") # or "(UNSIGNED-BYTE 8)"
  LISPOBJ(type_sint8,"(INTEGER -128 127)") # or "(SIGNED-BYTE 8)"
  LISPOBJ(type_uint16,"(INTEGER 0 65535)") # or "(UNSIGNED-BYTE 16)"
  LISPOBJ(type_sint16,"(INTEGER -32768 32767)") # or "(SIGNED-BYTE 16)"
  LISPOBJ(type_uint32,"(INTEGER 0 4294967295)") # or "(UNSIGNED-BYTE 32)"
  LISPOBJ(type_sint32,"(INTEGER -2147483648 2147483647)") # or "(SIGNED-BYTE 32)"
  LISPOBJ(type_uint64,"(INTEGER 0 18446744073709551615)") # or "(UNSIGNED-BYTE 64)"
  LISPOBJ(type_sint64,"(INTEGER -9223372036854775808 9223372036854775807)") # or "(SIGNED-BYTE 64)"
  LISPOBJ(type_array_index,"(INTEGER 0 (#.ARRAY-DIMENSION-LIMIT))")
  LISPOBJ(type_array_length,"(INTEGER 0 #.ARRAY-DIMENSION-LIMIT)")
  LISPOBJ(type_array_bit,"(ARRAY BIT)")
  LISPOBJ(type_posfixnum,"(INTEGER 0 #.MOST-POSITIVE-FIXNUM)")
  LISPOBJ(type_negfixnum,"(INTEGER #.MOST-NEGATIVE-FIXNUM (0))")
  LISPOBJ(type_posbignum,"(INTEGER (#.MOST-POSITIVE-FIXNUM))")
  LISPOBJ(type_negbignum,"(INTEGER * (#.MOST-NEGATIVE-FIXNUM))")
  LISPOBJ(type_posfixnum1,"(INTEGER (0) #.MOST-POSITIVE-FIXNUM)")
  LISPOBJ(type_array_rank,"(INTEGER 0 (#.ARRAY-RANK-LIMIT))")
  LISPOBJ(type_radix,"(INTEGER 2 36)")
  LISPOBJ(type_end_index,"(OR NULL INTEGER)")
  LISPOBJ(type_posinteger,"(INTEGER 0 *)")
  LISPOBJ(type_stringsymchar,"(OR STRING SYMBOL CHARACTER)")
  LISPOBJ(type_proper_list,"(SATISFIES EXT::PROPER-LIST-P)")
  LISPOBJ(type_svector2,"(SIMPLE-VECTOR 2)")
  LISPOBJ(type_svector5,"(SIMPLE-VECTOR 5)")
  LISPOBJ(type_climb_mode,"(INTEGER 1 5)")
  LISPOBJ(type_hashtable_test,"(MEMBER EQ EQL EQUAL EQUALP #.#'EQ #.#'EQL #.#'EQUAL #.#'EQUALP)")
  LISPOBJ(type_hashtable_size,"(INTEGER 0 #.(ASH MOST-POSITIVE-FIXNUM -1))")
  LISPOBJ(type_hashtable_rehash_size,"(OR (INTEGER 1 *) (FLOAT (1.0) *))")
  LISPOBJ(type_hashtable_rehash_threshold,"(REAL 0 1)")
  LISPOBJ(type_boole,"(INTEGER 0 15)")
  LISPOBJ(type_not_digit,"(AND CHARACTER (NOT (SATISFIES DIGIT-CHAR-P)))")
  LISPOBJ(type_rtcase,"(MEMBER :UPCASE :DOWNCASE :PRESERVE :INVERT)")
  LISPOBJ(type_peektype,"(OR BOOLEAN CHARACTER)")
  LISPOBJ(type_printcase,"(MEMBER :UPCASE :DOWNCASE :CAPITALIZE)")
  LISPOBJ(type_pprint_newline,"(MEMBER :LINEAR :FILL :MISER :MANDATORY)")
  LISPOBJ(type_pprint_indent,"(MEMBER :BLOCK :CURRENT)")
  LISPOBJ(type_random_arg,"(OR (INTEGER (0) *) (FLOAT (0.0) *))")
  LISPOBJ(type_packname,"(OR PACKAGE STRING SYMBOL CHARACTER)")
  LISPOBJ(type_posint16,"(INTEGER (0) (65536))")
  LISPOBJ(type_string_integer,"(OR STRING INTEGER)")
  LISPOBJ(type_stringsize,"(INTEGER 0 (#.SYS::STRING-DIMENSION-LIMIT))")
  LISPOBJ(type_uint8_vector,"(ARRAY (UNSIGNED-BYTE 8) (*))")
  LISPOBJ(type_position,"(OR (MEMBER :START :END) (INTEGER 0 4294967295))")
 #if HAS_HOST || defined(LOGICAL_PATHNAMES)
  LISPOBJ(type_host,"(OR NULL STRING)")
 #endif
  LISPOBJ(type_version,"(OR (MEMBER NIL :WILD :NEWEST) (INTEGER (0) #.MOST-POSITIVE-FIXNUM) PATHNAME)")
  LISPOBJ(type_direction,"(MEMBER :INPUT :INPUT-IMMUTABLE :OUTPUT :IO :PROBE)")
  LISPOBJ(type_if_exists,"(MEMBER :ERROR :NEW-VERSION :RENAME :RENAME-AND-DELETE :OVERWRITE :APPEND :SUPERSEDE NIL)")
  LISPOBJ(type_if_does_not_exist,"(MEMBER :ERROR :CREATE NIL)")
  LISPOBJ(type_directory_not_exist,"(MEMBER :DISCARD :ERROR :KEEP :IGNORE)")
  LISPOBJ(type_external_format,"(OR (MEMBER :DEFAULT) EXT::ENCODING (MEMBER :UNIX :MAC :DOS))")
  LISPOBJ(type_pathname_field_key,"(MEMBER :HOST :DEVICE :DIRECTORY :NAME :TYPE :VERSION NIL)")
 #ifdef SOCKET_STREAMS
  LISPOBJ(type_socket_option,"(MEMBER :SO-DEBUG : SO-ACCEPTCONN :SO-BROADCAST :SO-REUSEADDR :SO-DONTROUTE :SO-KEEPALIVE :SO-ERROR :SO-LINGER :SO-OOBINLINE :SO-TYPE :SO-RCVBUF :SO-SNDBUF :SO-RCVLOWAT :SO-SNDLOWAT :SO-RCVTIMEO :SO-SNDTIMEO)")
 #endif
 #ifdef LOGICAL_PATHNAMES
  LISPOBJ(type_logical_pathname,"(OR LOGICAL-PATHNAME STRING STREAM SYMBOL)")
 #endif
  LISPOBJ(type_builtin_stream,"(SATISFIES SYSTEM::BUILT-IN-STREAM-P)")
# for PATHNAME.D:
  LISPOBJ(lib_dir,"NIL") # must be set via a command line option
  LISPOBJ(type_designator_pathname,"(OR STRING FILE-STREAM PATHNAME)")
 #if defined(UNIX) || defined (WIN32_NATIVE)
  LISPOBJ(type_priority,"(OR (MEMBER :HIGH :NORMAL :LOW) INTEGER)")
 #endif
 #ifdef LOGICAL_PATHNAMES
  LISPOBJ(empty_logical_pathname,".") # (already initialized)
  LISPOBJ(handler_for_parse_error,"(#(PARSE-ERROR NIL))")
  LISPOBJ(type_logical_pathname_string,"(AND STRING (SATISFIES SYSTEM::VALID-LOGICAL-PATHNAME-STRING-P))")
  LISPOBJ(default_logical_pathname_host,"\"SYS\"")
 #endif
  LISPOBJ_S(empty_string,"")
  LISPOBJ_S(wild_string,"*")
  LISPOBJ_S(colon_string,":")
  LISPOBJ_S(semicolon_string,";")
 #ifdef PATHNAME_WIN32
  LISPOBJ(backslash_string,"\"\\\\\"")
 #endif
 #if defined(PATHNAME_WIN32)
  LISPOBJ(backslashbackslash_string,"\"\\\\\\\\\"")
  LISPOBJ_S(lnk_string,"lnk") /* for woe32 shell link resolution */
 #endif
 #if defined(PATHNAME_UNIX)
  LISPOBJ_S(slash_string,"/")
 #endif
  LISPOBJ_S(dot_string,".")
 #if defined(PATHNAME_WIN32) || defined(PATHNAME_UNIX)
  LISPOBJ_S(dotdot_string,"..")
  LISPOBJ_S(dotdotdot_string,"...")
 #endif
 #ifdef PATHNAME_WIN32
  LISPOBJ_S(backupextend_string,".bak") # name-extension of backupfiles
 #endif
 #ifdef PATHNAME_UNIX
  LISPOBJ_S(backupextend_string,"%") # name-extension of backupfiles
 #endif
 #ifdef PATHNAME_WIN32
  # default-drive (as string of length 1):
  LISPOBJ(default_drive,"NIL")
 #endif
 #if defined(PATHNAME_UNIX) || defined(PATHNAME_WIN32)
  LISPOBJ_S(wildwild_string,"**")
  LISPOBJ(directory_absolute,"(:ABSOLUTE)") # directory of the empty absolute pathname
 #endif
 #ifdef USER_HOMEDIR
  LISPOBJ(user_homedir,"#\".\"") # user-homedir-pathname
 #endif
 #ifdef HAVE_SHELL
 #ifdef UNIX
  LISPOBJ(command_shell,"\""SHELL"\"") # command-shell as string
  LISPOBJ(command_shell_option,"\"-c\"") # command-shell-option for command
  LISPOBJ(user_shell,"\"/bin/csh\"") # user-shell as string
 #endif
 #ifdef WIN32_NATIVE
  LISPOBJ(command_shell,"NIL") # command-interpreter as string
 #endif
 #endif
  # list of all open channel-streams, terminal-streams:
  LISPOBJ(open_files,"NIL")
 #ifdef GC_CLOSES_FILES
  # During the GC: the list of file-streams to be closed after the GC:
  LISPOBJ(files_to_close,"NIL")
 #endif
  # argumentlist for WRITE-TO-STRING :
  LISPOBJ(base10_radixnil,"(:BASE 10 :RADIX NIL)")
  # defaults for COMPILE-FILE-call in SPVW:
  LISPOBJ(source_file_type,"#\".lisp\"")
  LISPOBJ(compiled_file_type,"#\".fas\"")
  LISPOBJ(listing_file_type,"#\".lis\"")
# for STREAM.D:
  #if defined(SPVW_PURE) || ((((STACK_ADDRESS_RANGE << addr_shift) >> garcol_bit_o) & 1) != 0)
  LISPOBJ(dynamic_8bit_vector,"NIL") # cache for macro DYNAMIC_8BIT_VECTOR
  LISPOBJ(dynamic_string,"NIL") # cache for macro DYNAMIC_STRING
  #endif
  LISPOBJ(class_fundamental_stream,"NIL") # #<STANDARD-CLASS FUNDAMENTAL-STREAM>
  LISPOBJ(class_fundamental_input_stream,"NIL") # #<STANDARD-CLASS FUNDAMENTAL-INPUT-STREAM>
  LISPOBJ(class_fundamental_output_stream,"NIL") # #<STANDARD-CLASS FUNDAMENTAL-OUTPUT-STREAM>
  LISPOBJ(type_input_stream,"(SATISFIES INPUT-STREAM-P)") # type for error-message
  LISPOBJ(type_output_stream,"(SATISFIES OUTPUT-STREAM-P)") # type for error-message
  LISPOBJ(type_string_with_fill_pointer,"(AND STRING (SATISFIES ARRAY-HAS-FILL-POINTER-P))") # type for error-message
#if defined(GNU_READLINE) || defined(NEXTAPP)
  LISPOBJ(handler_for_charset_type_error,"(#(SYSTEM::CHARSET-TYPE-ERROR NIL))")
#endif
  LISPOBJ(setf_stream_element_type,"(SETF STREAM-ELEMENT-TYPE)")
  LISPOBJ(type_endianness,"(MEMBER :LITTLE :BIG)") # type for error-message
  LISPOBJ(type_open_file_stream,"(AND FILE-STREAM (SATISFIES OPEN-STREAM-P))") # type for error-message
  LISPOBJ(strmtype_ubyte8,"(UNSIGNED-BYTE 8)") # as stream-element-type
  LISPOBJ(standard_input_file_stream,"NIL")
  LISPOBJ(standard_output_file_stream,"NIL")
  LISPOBJ(standard_error_file_stream,"NIL")
# for IO.D:
  # four readtable-case-values:
  LISPOBJ(rtcase_0,":UPCASE")
  LISPOBJ(rtcase_1,":DOWNCASE")
  LISPOBJ(rtcase_2,":PRESERVE")
  LISPOBJ(rtcase_3,":INVERT")
 # for reader:
  # standard-readtable of Common Lisp
  LISPOBJ(standard_readtable,".")
  # prototype of the dispatch-reader-functions
  LISPOBJ(dispatch_reader,"NIL")
  LISPOBJ(dispatch_reader_index,"0")
  # prefix for character-names:
  LISPOBJ(charname_prefix,"\"Code\"")
  # internal variables of the reader:
  LISPOBJ(token_buff_1,".")
  LISPOBJ(token_buff_2,".")
  LISPOBJ(displaced_string,".")
  # handler-types:
  LISPOBJ(handler_for_arithmetic_error,"(#(ARITHMETIC-ERROR NIL))")
  LISPOBJ_S(tildeA,"~A")
 # for printer:
  # substrings used for output of objects:
  LISPOBJ_S(printstring_array,"ARRAY")
  LISPOBJ_S(printstring_fill_pointer,"FILL-POINTER=")
  LISPOBJ_S(printstring_address,"ADDRESS")
  LISPOBJ_S(printstring_system,"SYSTEM-POINTER")
  LISPOBJ_S(printstring_frame_pointer,"FRAME-POINTER")
  LISPOBJ_S(printstring_read_label,"READ-LABEL")
  LISPOBJ_S(printstring_unbound,"#<UNBOUND>")
  LISPOBJ_S(printstring_unbound_readably,"#.(SYS::%UNBOUND)")
  LISPOBJ_S(printstring_special_reference,"#<SPECIAL REFERENCE>")
  LISPOBJ_S(printstring_disabled_pointer,"#<DISABLED POINTER>")
  LISPOBJ_S(printstring_dot,"#<DOT>")
  LISPOBJ_S(printstring_eof,"#<END OF FILE>")
  LISPOBJ_S(printstring_deleted,"DELETED ")
  LISPOBJ_S(printstring_package,"PACKAGE")
  LISPOBJ_S(printstring_readtable,"READTABLE")
  LISPOBJ(pathname_slotlist,"#.(list (cons :HOST #'pathname-host) (cons :DEVICE #'pathname-device) (cons :DIRECTORY #'pathname-directory) (cons :NAME #'pathname-name) (cons :TYPE #'pathname-type) (cons :VERSION #'pathname-version))")
  LISPOBJ(byte_slotlist,"#.(list (cons :SIZE #'byte-size) (cons :POSITION #'byte-position))")
  LISPOBJ_S(printstring_symbolmacro,"SYMBOL-MACRO")
  LISPOBJ_S(printstring_globalsymbolmacro,"GLOBAL SYMBOL-MACRO")
  LISPOBJ_S(printstring_macro,"MACRO")
  LISPOBJ_S(printstring_functionmacro,"FUNCTION-MACRO")
  LISPOBJ_S(printstring_encoding,"ENCODING")
  #ifdef FOREIGN
  LISPOBJ_S(printstring_invalid,"INVALID ")
  LISPOBJ_S(printstring_fpointer,"FOREIGN-POINTER")
  #endif
  #ifdef DYNAMIC_FFI
  LISPOBJ_S(printstring_faddress,"FOREIGN-ADDRESS")
  LISPOBJ_S(printstring_fvariable,"FOREIGN-VARIABLE")
  LISPOBJ_S(printstring_ffunction,"FOREIGN-FUNCTION")
  #endif
  LISPOBJ_S(printstring_weakpointer,"WEAK-POINTER")
  LISPOBJ_S(printstring_broken_weakpointer,"#<BROKEN WEAK-POINTER>")
  LISPOBJ_S(printstring_weak_list,"WEAK-LIST")
  LISPOBJ_S(printstring_weak_alist,"WEAK-ALIST")
  LISPOBJ_S(printstring_weakmapping,"WEAK-MAPPING")
  LISPOBJ_S(printstring_broken_weakmapping,"#<BROKEN WEAK-MAPPING>")
  LISPOBJ_S(printstring_finalizer,"#<FINALIZER>")
  #ifdef SOCKET_STREAMS
  LISPOBJ_S(printstring_socket_server,"SOCKET-SERVER")
  #endif
  #ifdef YET_ANOTHER_RECORD
  LISPOBJ_S(printstring_yetanother,"YET-ANOTHER")
  #endif
  LISPOBJ_S(printstring_internal_weak_list,"#<INTERNAL-WEAK-LIST>")
  LISPOBJ_S(printstring_weak_and_relation,"WEAK-AND-RELATION")
  LISPOBJ_S(printstring_broken_weak_and_relation,"#<BROKEN WEAK-AND-RELATION>")
  LISPOBJ_S(printstring_weak_or_relation,"WEAK-OR-RELATION")
  LISPOBJ_S(printstring_broken_weak_or_relation,"#<BROKEN WEAK-OR-RELATION>")
  LISPOBJ_S(printstring_weak_and_mapping,"WEAK-AND-MAPPING")
  LISPOBJ_S(printstring_broken_weak_and_mapping,"#<BROKEN WEAK-AND-MAPPING>")
  LISPOBJ_S(printstring_weak_or_mapping,"WEAK-OR-MAPPING")
  LISPOBJ_S(printstring_broken_weak_or_mapping,"#<BROKEN WEAK-OR-MAPPING>")
  LISPOBJ_S(printstring_internal_weak_alist,"#<INTERNAL-WEAK-ALIST>")
  LISPOBJ_S(printstring_internal_weak_hashed_alist,"#<INTERNAL-WEAK-HASHED-ALIST>")
  LISPOBJ_S(printstring_closure,"FUNCTION")
  LISPOBJ_S(printstring_compiled_closure,"COMPILED-FUNCTION")
  LISPOBJ_S(printstring_subr,"SYSTEM-FUNCTION")
  LISPOBJ_S(printstring_addon_subr,"ADD-ON-SYSTEM-FUNCTION")
  LISPOBJ_S(printstring_fsubr,"SPECIAL-OPERATOR")
  LISPOBJ_S(printstring_closed,"CLOSED ")
  LISPOBJ_S(printstring_input,"INPUT ")
  LISPOBJ_S(printstring_output,"OUTPUT ")
  LISPOBJ_S(printstring_io,"IO ")
  # Buffering mode, addressed by
  # (bit(1) if input-buffered) | (bit(0) if output-buffered).
  LISPOBJ_S(printstring_buffered_00,"UNBUFFERED ")
  LISPOBJ_S(printstring_buffered_01,"OUTPUT-BUFFERED ")
  LISPOBJ_S(printstring_buffered_10,"INPUT-BUFFERED ")
  LISPOBJ_S(printstring_buffered_11,"BUFFERED ")
    # name-string for each streamtype, addressed by streamtype:
    LISPOBJ_S(printstring_strmtype_synonym,"SYNONYM")
    LISPOBJ_S(printstring_strmtype_broad,"BROADCAST")
    LISPOBJ_S(printstring_strmtype_concat,"CONCATENATED")
    LISPOBJ_S(printstring_strmtype_twoway,"TWO-WAY")
    LISPOBJ_S(printstring_strmtype_echo,"ECHO")
    LISPOBJ_S(printstring_strmtype_str_in,"STRING-INPUT")
    LISPOBJ_S(printstring_strmtype_str_out,"STRING-OUTPUT")
    LISPOBJ_S(printstring_strmtype_str_push,"STRING-PUSH")
    LISPOBJ_S(printstring_strmtype_pphelp,"PRETTY-PRINTER-HELP")
    LISPOBJ_S(printstring_strmtype_buff_in,"BUFFERED-INPUT")
    LISPOBJ_S(printstring_strmtype_buff_out,"BUFFERED-OUTPUT")
    #ifdef GENERIC_STREAMS
    LISPOBJ_S(printstring_strmtype_generic,"GENERIC")
    #endif
    LISPOBJ_S(printstring_strmtype_file,"FILE")
    #ifdef KEYBOARD
    LISPOBJ_S(printstring_strmtype_keyboard,"KEYBOARD")
    #endif
    LISPOBJ_S(printstring_strmtype_terminal,"TERMINAL")
    #ifdef SCREEN
    LISPOBJ_S(printstring_strmtype_window,"WINDOW")
    #endif
    #ifdef PRINTER
    LISPOBJ_S(printstring_strmtype_printer,"PRINTER")
    #endif
    #ifdef PIPES
    LISPOBJ_S(printstring_strmtype_pipe_in,"PIPE-INPUT")
    LISPOBJ_S(printstring_strmtype_pipe_out,"PIPE-OUTPUT")
    #endif
    #ifdef X11SOCKETS
    LISPOBJ_S(printstring_strmtype_x11socket,"X11-SOCKET")
    #endif
    #ifdef SOCKET_STREAMS
    LISPOBJ_S(printstring_strmtype_socket,"SOCKET")
    LISPOBJ_S(printstring_strmtype_twoway_socket,"SOCKET")
    #endif
  LISPOBJ_S(printstring_stream,"-STREAM")
# for LISPARIT.D:
  # various constant numbers:
  #ifndef IMMEDIATE_FFLOAT
  LISPOBJ(FF_zero,"0.0F0")
  LISPOBJ(FF_one,"1.0F0")
  LISPOBJ(FF_minusone,"-1.0F0")
  #endif
  LISPOBJ(DF_zero,"0.0D0")
  LISPOBJ(DF_one,"1.0D0")
  LISPOBJ(DF_minusone,"-1.0D0")
  # defaultlength for reading of long-floats (Integer >=LF_minlen, <2^intWCsize):
  LISPOBJ(LF_digits,".") # (already initialized)
  # variable long-floats: (already initialized)
  LISPOBJ(SF_pi,".")   # value of pi as Short-Float
  LISPOBJ(FF_pi,".")   # value of pi as Single-Float
  LISPOBJ(DF_pi,".")   # value of pi as Double-Float
  LISPOBJ(pi,".")      # value of pi, Long-Float of defaultlenght
  LISPOBJ(LF_pi,".")   # value of pi, so exact as known
  LISPOBJ(LF_ln2,".")  # value of ln 2, so exact as known
  LISPOBJ(LF_ln10,".") # value of ln 10, so exact as known
# for EVAL.D:
  # toplevel-declaration-environment:
  LISPOBJ(top_decl_env,"(NIL)") # list of O(declaration_types) (is initialized later)
  # decl-spec with list of declaration-types to be recognized:
  LISPOBJ(declaration_types,"(DECLARATION OPTIMIZE DECLARATION)")
  # name of the common-lisp-package:
  LISPOBJ_S(common_lisp_string,"COMMON-LISP")
# for DEBUG.D:
  LISPOBJ_S(newline_string,NLstring)
  # prompts:
  LISPOBJ_S(prompt_string,"> ")
  LISPOBJ_S(breakprompt_string,". Break> ")
  # various strings for description of the stack:
  LISPOBJ_S(showstack_string_lisp_obj,"- ")
  LISPOBJ_S(showstack_string_bindung,NLstring "  | ")
  LISPOBJ_S(showstack_string_zuord," <--> ")
  LISPOBJ_S(showstack_string_zuordtag," --> ")
  LISPOBJ_S(showstack_string_VENV_frame,NLstring "  VAR_ENV <--> ")
  LISPOBJ_S(showstack_string_FENV_frame,NLstring "  FUN_ENV <--> ")
  LISPOBJ_S(showstack_string_BENV_frame,NLstring "  BLOCK_ENV <--> ")
  LISPOBJ_S(showstack_string_GENV_frame,NLstring "  GO_ENV <--> ")
  LISPOBJ_S(showstack_string_DENV_frame,NLstring "  DECL_ENV <--> ")
# for SPVW.D:
 #ifdef WIN32_NATIVE
  LISPOBJ(load_extra_file_types,"(\".BAT\")")
 #endif
/* for control & io, function seclass_object(): */
  LISPOBJ(seclass_no_se,"(NIL NIL NIL)")
  LISPOBJ(seclass_read,"(T NIL NIL)")
  LISPOBJ(seclass_write,"(NIL T T)")
  LISPOBJ(seclass_default,"(T T T)")
# for FOREIGN.D:
 #ifdef DYNAMIC_FFI
  LISPOBJ(fp_zero,"NIL")
  LISPOBJ(foreign_variable_table,"#.(make-hash-table :test #'equal)")
  LISPOBJ(foreign_function_table,"#.(make-hash-table :test #'equal)")
  LISPOBJ(type_foreign_variable,"(OR FFI::FOREIGN-VARIABLE FFI::FOREIGN-ADDRESS)")
  LISPOBJ(type_foreign_function,"(OR FFI::FOREIGN-FUNCTION FFI::FOREIGN-ADDRESS)")
 #if defined(WIN32_NATIVE) || defined(HAVE_DLOPEN)
  LISPOBJ(foreign_libraries,"NIL")
 #endif
  LISPOBJ(foreign_callin_table,"#.(make-hash-table :test #'eq)")
  LISPOBJ(foreign_callin_vector,"#.(let ((array (make-array 1 :adjustable t :fill-pointer 1))) (sys::store array 0 0) array)")
 #endif
