/*
 * EVAL, APPLY and bytecode interpreter for CLISP
 * Bruno Haible 1990-2005
 * Sam Steingold 1998-2005
 * German comments translated into English: Stefan Kain 2001-08-13
 */
#include "lispbibl.c"


/* function-table:
 In this table only SUBRS are listed, which may be inlined by the compiler.
 In FUNTAB1 and FUNTAB2 SUBRs without Rest-Parameter (i.e. with
 fixed number of arguments known at compile-time) are listed.
 In FUNTABR SUBRs with Rest-Parameter are listed. */
#define _(name)  &subr_tab.D_##name  /* address of SUBR name, like L(name) */
/* FUNTAB1 and FUNTAB2, first: */
local const Subr FUNTAB[] = {
  /* SPVW : 0 SUBRs */
  /* EVAL : 3 SUBRs */
  _(funtabref), _(subr_info), _(special_variable_p),
  /* ARRAY : 30-2 SUBRs */
  _(copy_simple_vector), /* _(svref), _(psvstore), */ _(row_major_aref),
  _(row_major_store), _(array_element_type), _(array_rank),
  _(array_dimension), _(array_dimensions), _(array_total_size),
  _(adjustable_array_p), _(bit_and), _(bit_ior), _(bit_xor), _(bit_eqv),
  _(bit_nand), _(bit_nor), _(bit_andc1), _(bit_andc2), _(bit_orc1),
  _(bit_orc2), _(bit_not), _(array_has_fill_pointer_p), _(fill_pointer),
  _(set_fill_pointer), _(vector_push), _(vector_pop), _(vector_push_extend),
  _(make_array), _(adjust_array),
  /* CHARSTRG : 47 SUBRs */
  _(standard_char_p), _(graphic_char_p), _(string_char_p), _(alpha_char_p),
  _(upper_case_p), _(lower_case_p), _(both_case_p), _(digit_char_p),
  _(alphanumericp), _(char_code), _(code_char), _(character), _(char_upcase),
  _(char_downcase), _(digit_char), _(char_int), _(int_char), _(char_name),
  _(char), _(schar), _(store_char), _(store_schar), _(string_gleich),
  _(string_ungleich), _(string_kleiner), _(string_groesser),
  _(string_klgleich), _(string_grgleich), _(string_equal),
  _(string_not_equal), _(string_lessp), _(string_greaterp),
  _(string_not_greaterp), _(string_not_lessp), _(search_string_gleich),
  _(search_string_equal), _(make_string), _(string_both_trim),
  _(nstring_upcase), _(string_upcase), _(nstring_downcase),
  _(string_downcase), _(nstring_capitalize), _(string_capitalize),
  _(string), _(name_char), _(substring),
  /* CONTROL : 23-2 SUBRs */
  _(symbol_value), /* _(symbol_function), */ _(boundp), _(fboundp),
  _(special_operator_p), _(set), _(makunbound), _(fmakunbound),
  /* _(values_list), */ _(driver), _(unwind_to_driver), _(macro_function),
  _(macroexpand), _(macroexpand_1), _(proclaim), _(eval),
  _(evalhook), _(applyhook), _(constantp), _(function_side_effect),
  _(function_name_p),_(parse_body), _(keyword_test),
  /* DEBUG : 0 SUBRs */
  /* ERROR : 1 SUBR */
  _(invoke_debugger),
  /* HASHTABL : 11 SUBRs */
  _(make_hash_table), _(gethash), _(puthash), _(remhash), _(maphash),
  _(clrhash), _(hash_table_count), _(hash_table_iterator),
  _(hash_table_iterate), _(class_gethash), _(sxhash),
  /* IO : 36 SUBRs */
  _(copy_readtable), _(set_syntax_from_char), _(set_macro_character),
  _(get_macro_character), _(make_dispatch_macro_character),
  _(set_dispatch_macro_character), _(get_dispatch_macro_character),
  _(read), _(read_preserving_whitespace), _(read_delimited_list),
  _(read_line), _(read_char), _(unread_char), _(peek_char), _(listen),
  _(read_char_no_hang), _(clear_input), _(read_from_string),
  _(parse_integer), _(write), _(prin1), _(print), _(pprint), _(princ),
  _(write_to_string), _(prin1_to_string), _(princ_to_string), _(write_char),
  _(write_string), _(write_line), _(terpri), _(fresh_line),
  _(finish_output), _(force_output), _(clear_output), _(line_position),
  /* LIST : 84-36=48 SUBRs */
  /* _(car), _(cdr), _(caar), _(cadr), _(cdar), _(cddr), _(caaar), _(caadr),
     _(cadar), _(caddr), _(cdaar), _(cdadr), _(cddar), _(cdddr), _(caaaar),
     _(caaadr), _(caadar), _(caaddr), _(cadaar), _(cadadr), _(caddar),
     _(cadddr), _(cdaaar), _(cdaadr), _(cdadar), _(cdaddr), _(cddaar),
     _(cddadr), _(cdddar), _(cddddr), _(cons), */ _(tree_equal), _(endp),
  _(list_length), _(nth), /* _(first), _(second), _(third), _(fourth), */
  _(fifth), _(sixth), _(seventh), _(eighth), _(ninth), _(tenth), /* _(rest), */
  _(nthcdr), _(last), _(make_list), _(copy_list), _(copy_alist), _(memq),
  _(copy_tree), _(revappend), _(nreconc), _(list_nreverse), _(butlast),
  _(nbutlast), _(ldiff), _(rplaca), _(prplaca), _(rplacd), _(prplacd),
  _(subst), _(subst_if), _(subst_if_not), _(nsubst), _(nsubst_if),
  _(nsubst_if_not), _(sublis), _(nsublis), _(member), _(member_if),
  _(member_if_not), _(tailp), _(adjoin), _(acons), _(pairlis), _(assoc),
  _(assoc_if), _(assoc_if_not), _(rassoc), _(rassoc_if), _(rassoc_if_not),
  /* MISC : 10 SUBRs */
  _(lisp_implementation_type), _(lisp_implementation_version),
  _(software_type), _(software_version), _(identity), _(get_universal_time),
  _(get_internal_run_time), _(get_internal_real_time), _(sleep), _(time),
  /* PACKAGE : 26 SUBRs */
  _(make_symbol), _(find_package), _(package_name), _(package_nicknames),
  _(rename_package), _(package_use_list), _(package_used_by_list),
  _(package_shadowing_symbols), _(list_all_packages), _(intern),
  _(find_symbol), _(unintern), _(export), _(unexport), _(import),
  _(shadowing_import), _(shadow), _(use_package), _(unuse_package),
  _(make_package), _(pin_package), _(find_all_symbols),
  _(map_symbols), _(map_external_symbols), _(map_all_symbols),
  _(pfind_package), _(re_export),
  /* PATHNAME : 27 SUBRs */
  _(parse_namestring), _(pathname), _(pathnamehost), _(pathnamedevice),
  _(pathnamedirectory), _(pathnamename), _(pathnametype),
  _(pathnameversion), _(file_namestring), _(directory_namestring),
  _(host_namestring), _(merge_pathnames), _(enough_namestring),
  _(make_pathname), _(namestring), _(truename), _(probe_file),
  _(delete_file), _(rename_file), _(open), _(directory), _(cd),
  _(make_dir), _(delete_dir), _(file_write_date), _(file_author),
  _(savemem),
  /* PREDTYPE : 44-3 SUBRs */
  /* _(eq), */ _(eql), _(equal), _(equalp), _(consp), _(atom), _(symbolp),
  _(stringp), _(numberp), _(compiled_function_p), /* _(null), _(not), */
  _(closurep), _(listp), _(integerp), _(fixnump), _(rationalp), _(floatp),
  _(short_float_p), _(single_float_p), _(double_float_p), _(long_float_p),
  _(realp), _(complexp), _(streamp), _(random_state_p), _(readtablep),
  _(hash_table_p), _(pathnamep), _(logical_pathname_p), _(characterp),
  _(functionp), _(packagep), _(arrayp), _(simple_array_p), _(bit_vector_p),
  _(vectorp), _(simple_vector_p), _(simple_string_p), _(simple_bit_vector_p),
  _(type_of), _(class_of), _(find_class), _(coerce),
  /* RECORD : 23 SUBRs */
  _(record_ref), _(record_store), _(record_length), _(structure_ref),
  _(structure_store), _(make_structure), _(copy_structure),
  _(structure_type_p), _(closure_name), _(closure_codevec),
  _(closure_consts), _(make_code_vector), _(make_closure),
  _(copy_generic_function), _(make_load_time_eval),
  _(function_macro_function), _(structure_object_p), _(std_instance_p),
  _(slot_value), _(set_slot_value), _(slot_boundp), _(slot_makunbound),
  _(slot_exists_p),
  /* SEQUENCE : 40 SUBRs */
  _(sequencep), _(elt), _(setelt), _(subseq), _(copy_seq), _(length),
  _(reverse), _(nreverse), _(make_sequence), _(reduce), _(fill),
  _(replace), _(remove), _(remove_if), _(remove_if_not), _(delete),
  _(delete_if), _(delete_if_not), _(remove_duplicates),
  _(delete_duplicates), _(substitute), _(substitute_if),
  _(substitute_if_not), _(nsubstitute), _(nsubstitute_if),
  _(nsubstitute_if_not), _(find), _(find_if), _(find_if_not), _(position),
  _(position_if), _(position_if_not), _(count), _(count_if),
  _(count_if_not), _(mismatch), _(search), _(sort), _(stable_sort),
  _(merge),
  /* STREAM : 24 SUBRs */
  _(file_stream_p), _(make_synonym_stream), _(synonym_stream_p),
  _(broadcast_stream_p), _(concatenated_stream_p), _(make_two_way_stream),
  _(two_way_stream_p), _(make_echo_stream), _(echo_stream_p),
  _(make_string_input_stream), _(string_input_stream_index),
  _(make_string_output_stream), _(get_output_stream_string),
  _(make_string_push_stream), _(string_stream_p), _(input_stream_p),
  _(output_stream_p), _(built_in_stream_element_type),
  _(stream_external_format), _(built_in_stream_close), _(read_byte),
  _(write_byte), _(file_position), _(file_length),
  /* SYMBOL : 14 SUBRs */
  _(putd), _(proclaim_constant), _(get), _(getf), _(get_properties),
  _(putplist), _(put), _(remprop), _(symbol_package), _(symbol_plist),
  _(symbol_name), _(keywordp), _(gensym), _(gensym),
  /* LISPARIT : 84 SUBRs */
  _(decimal_string), _(zerop), _(plusp), _(minusp), _(oddp), _(evenp),
  _(einsplus), _(einsminus), _(conjugate), _(exp), _(expt), _(log),
  _(sqrt), _(isqrt), _(abs), _(phase), _(signum), _(sin), _(cos), _(tan),
  _(cis), _(asin), _(acos), _(atan), _(sinh), _(cosh), _(tanh), _(asinh),
  _(acosh), _(atanh), _(float), _(rational), _(rationalize), _(numerator),
  _(denominator), _(floor), _(ceiling), _(truncate), _(round), _(mod),
  _(rem), _(ffloor), _(fceiling), _(ftruncate), _(fround), _(decode_float),
  _(scale_float), _(float_radix), _(float_sign), _(float_digits),
  _(float_precision), _(integer_decode_float), _(complex), _(realpart),
  _(imagpart), _(lognand), _(lognor), _(logandc1), _(logandc2), _(logorc1),
  _(logorc2), _(boole), _(lognot), _(logtest), _(logbitp), _(ash),
  _(logcount), _(integer_length), _(byte), _(bytesize), _(byteposition),
  _(ldb), _(ldb_test), _(mask_field), _(dpb), _(deposit_field), _(random),
  _(make_random_state), _(fakultaet), _(exquo), _(long_float_digits),
  _(set_long_float_digits), _(log2), _(log10),
  /* other: */
}; /* that were 529-43 SUBRs. */
/* Now FUNTABR : */
local const Subr FUNTABR[] = {
  /* SPVW : 0 SUBRs */
  /* EVAL : 0 SUBRs */
  /* ARRAY : 7 SUBRs */
  _(vector), _(aref), _(store), _(array_in_bounds_p),
  _(array_row_major_index), _(bit), _(sbit),
  /* CHARSTRG : 13 SUBRs */
  _(char_gleich), _(char_ungleich), _(char_kleiner), _(char_groesser),
  _(char_klgleich), _(char_grgleich), _(char_equal), _(char_not_equal),
  _(char_lessp), _(char_greaterp), _(char_not_greaterp), _(char_not_lessp),
  _(string_concat),
  /* CONTROL : 9 SUBRs */
  _(apply), _(funcall), _(mapcar), _(maplist), _(mapc),
  _(mapl), _(mapcan), _(mapcon), _(values),
  /* DEBUG : 0 SUBRs */
  /* ERROR : 2 SUBRs */
  _(error), _(error_of_type),
  /* HASHTABL : 1 SUBR */
  _(class_tuple_gethash),
  /* IO : 0 SUBRs */
  /* LIST : 4 SUBRs */
  _(list), _(liststern), _(append), _(nconc),
  /* MISC : 0 SUBRs */
  /* PACKAGE : 0 SUBRs */
  /* PATHNAME : 0 SUBRs */
  /* PREDTYPE : 0 SUBRs */
  /* RECORD : 1 SUBR */
  _(pallocate_instance),
  /* SEQUENCE : 6 SUBRs */
  _(concatenate), _(map), _(some), _(every), _(notany), _(notevery),
  /* STREAM : 2 SUBRs */
  _(make_broadcast_stream), _(make_concatenated_stream),
  /* SYMBOL : 0 SUBRs */
  /* LISPARIT : 18 SUBRs */
  _(gleich), _(ungleich), _(kleiner), _(groesser), _(klgleich),
  _(grgleich), _(max), _(min), _(plus), _(minus), _(mal), _(durch), _(gcd),
  _(lcm), _(logior), _(logxor), _(logand), _(logeqv),
}; /* That were 63 SUBRs. */
#undef _
#define FUNTAB1  (&FUNTAB[0])
#define FUNTAB2  (&FUNTAB[256])
#define FUNTAB_length  (sizeof(FUNTAB)/sizeof(Subr))
#define FUNTABR_length  (sizeof(FUNTABR)/sizeof(Subr))

/* argument-type-tokens for compiled closures: */
typedef enum {
  cclos_argtype_default,
  cclos_argtype_0_0,
  cclos_argtype_1_0,
  cclos_argtype_2_0,
  cclos_argtype_3_0,
  cclos_argtype_4_0,
  cclos_argtype_5_0,
  cclos_argtype_0_1,
  cclos_argtype_1_1,
  cclos_argtype_2_1,
  cclos_argtype_3_1,
  cclos_argtype_4_1,
  cclos_argtype_0_2,
  cclos_argtype_1_2,
  cclos_argtype_2_2,
  cclos_argtype_3_2,
  cclos_argtype_0_3,
  cclos_argtype_1_3,
  cclos_argtype_2_3,
  cclos_argtype_0_4,
  cclos_argtype_1_4,
  cclos_argtype_0_5,
  cclos_argtype_0_0_rest,
  cclos_argtype_1_0_rest,
  cclos_argtype_2_0_rest,
  cclos_argtype_3_0_rest,
  cclos_argtype_4_0_rest,
  cclos_argtype_0_0_key,
  cclos_argtype_1_0_key,
  cclos_argtype_2_0_key,
  cclos_argtype_3_0_key,
  cclos_argtype_4_0_key,
  cclos_argtype_0_1_key,
  cclos_argtype_1_1_key,
  cclos_argtype_2_1_key,
  cclos_argtype_3_1_key,
  cclos_argtype_0_2_key,
  cclos_argtype_1_2_key,
  cclos_argtype_2_2_key,
  cclos_argtype_0_3_key,
  cclos_argtype_1_3_key,
  cclos_argtype_0_4_key,
  cclos_argtype_for_broken_compilers_that_dont_like_trailing_commas
} cclos_argtype_t;

/* Call of the bytecode-interpreter:
 interpretes the bytecode of a compiled closure.
 interpret_bytecode(closure,codevec,index);
 > closure: compiled closure
 > codevec: its codevector, a Simple-Bit-Vector
 > index: Start-Index
 < mv_count/mv_space: values
 changes STACK, can trigger GC
 local Values interpret_bytecode (object closure, object codevec, uintL index);
*/
local /*maygc*/ Values interpret_bytecode_ (object closure, Sbvector codeptr,
                                            const uintB* byteptr);
#define interpret_bytecode(closure,codevec,index)                       \
  with_saved_back_trace_cclosure(closure,                               \
    interpret_bytecode_(closure,TheSbvector(codevec),&TheSbvector(codevec)->data[index]); )

/* GCC2 can jump directly to labels.
   This results in faster code than switch(). */
#if defined(GNU) && !(__APPLE_CC__ > 1)
  #if (__GNUC__ >= 2) && !defined(UNIX_HPUX) && !defined(NO_FAST_DISPATCH) /* work around HP-UX Linker Bug */
    #define FAST_DISPATCH
    #if (__GNUC__ >= 3) || (__GNUC_MINOR__ >= 7) /* work around gcc-2.6.3 Bug (-fno-defer-pop ginge auch) */
      #define FAST_DISPATCH_THREADED
    #endif
  #endif
#endif

/* Values of the bytecodes (256 totally): */
typedef enum {
 #define BYTECODE(code)  code,
  #include "bytecode.c"
 #undef BYTECODE
  cod_for_broken_compilers_that_dont_like_trailing_commas
} bytecode_enum_t;


/* ---------------------- LISP-FUNCTIONS ----------------------- */

/* (SYS::%FUNTABREF i) returns the name of function Nr. i from the function-
 table (a symbol), resp. NIL if i is not in the right range. */
LISPFUNNF(funtabref,1)
{
  var object arg = popSTACK(); /* argument */
  var uintV i;
  if (posfixnump(arg) /* should be Fixnum >=0 */
      && (i = posfixnum_to_V(arg),
          i < FUNTAB_length+FUNTABR_length)) { /* and < table-length */
    /* Name of the indexed element of the table: */
    value1 = (i < FUNTAB_length
              ? FUNTAB[i]                /* from FUNTAB1/2 */
              : FUNTABR[i-FUNTAB_length] /* resp. from FUNTABR */
              )->name;
  } else {
    value1 = NIL; /* or NIL */
  }
  mv_count=1; /* as value */
}

/* (SYS::SUBR-INFO obj) returns information for this SUBR, if obj is a SUBR
   (or a Symbol with a SUBR as global function definition),
 6 values:
   name              Name,
   req-anz           number of required parameters,
   opt-anz           number of optional parameters,
   rest-p            flag, if &rest is specified,
   keywords          list of admissible keywords (empty: no &key specified),
   allow-other-keys  flag, if additional keywords are allowed,
 otherwise NIL. */
LISPFUNNR(subr_info,1)
{
  var object obj = popSTACK();
  if (!subrp(obj)) {
    if (!(symbolp(obj) && subrp(Symbol_function(obj)))) {
      VALUES0; return; /* no SUBR -> no value */
    }
    obj = Symbol_function(obj);
  }
  /* obj is a SUBR */
  pushSTACK(TheSubr(obj)->name); /* Name */
  pushSTACK(fixnum(TheSubr(obj)->req_anz)); /* req-anz (req-nr) */
  pushSTACK(fixnum(TheSubr(obj)->opt_anz)); /* opt-anz (opt-nr) */
  pushSTACK(TheSubr(obj)->rest_flag == subr_norest ? NIL : T); /* rest-p */
  /* during bootstrap, before defseq.lisp is loaded, this may fail: */
  coerce_sequence(TheSubr(obj)->keywords,S(list),false);
  /* keyword-vector as list (during bootstrap: vector) */
  pushSTACK(eq(value1,nullobj) ? (object)TheSubr(obj)->keywords : value1);
  pushSTACK(TheSubr(obj)->key_flag == subr_key_allow ? T : NIL); /* allow-other-keys */
  funcall(L(values),6); /* 6 values */
}


/* ----------------------- SUBROUTINES ----------------------- */

/* UP: unwinds a frame, which is pointed at by STACK.
 unwind();
 The values mv_count/mv_space remain unmodified.
 If it is no Unwind-Protect-Frame: return normally.
 If it is a  Unwind-Protect-Frame:
   save the values, climbs(?) up STACK and SP
   and then calls unwind_protect_to_save.fun .
 changes STACK
 can trigger GC */
global /*maygc*/ void unwind (void)
{
  var fcint frame_info = framecode(STACK_0);
  GCTRIGGER_IF(frame_info == APPLY_frame_info || frame_info == TRAPPED_APPLY_frame_info
               || frame_info == EVAL_frame_info || frame_info == TRAPPED_EVAL_frame_info,
               GCTRIGGER1(mv_space));
 #ifdef unwind_bit_t
  if (frame_info & bit(unwind_bit_t)) /* anything to do? */
 #else
  if (frame_info >= unwind_limit_t) /* anything to do? */
 #endif
    { /* (no at APPLY, EVAL ungetrapped, CATCH, HANDLER,
         IBLOCK and ITAGBODY unnested) */
      if ((frame_info & bit(skip2_bit_t)) == 0) { /* ENV- or DYNBIND-Frame? */
       #ifdef entrypoint_bit_t
        if (frame_info & bit(entrypoint_bit_t)) /* BLOCK, TAGBODY, CATCH etc. ? */
       #else
        if (frame_info < entrypoint_limit_t) /* BLOCK, TAGBODY, CATCH etc. ? */
       #endif
          /* Frame with Exitpoint */
          if (frame_info & bit(blockgo_bit_t)) { /* BLOCK or TAGBODY? */
            /* BLOCK_FRAME or TAGBODY_FRAME */
            if (frame_info & bit(cframe_bit_t)) { /* compiled? */
              /* CBLOCK_FRAME or CTAGBODY_FRAME
                 In Cons (NAME/Tags . <Framepointer>) */
              Cdr(STACK_(frame_ctag)) = disabled; /* disable Exit/Tags */
            } else {
              /* IBLOCK_FRAME or ITAGBODY_FRAME, nested
                 In Cons (NAME/Tags . <Framepointer>)
                 (first pair of alist next_env) */
              Cdr(Car(STACK_(frame_next_env))) = disabled; /* disable Exit/Tags */
            }
          } else {
            /* UNWIND_PROTECT_FRAME DRIVER_FRAME or trapped APPLY/EVAL_FRAME */
            if (frame_info & bit(dynjump_bit_t)) {
              /* UNWIND_PROTECT_FRAME or DRIVER_FRAME */
              if (frame_info & bit(driver_bit_t)) {
                /* DRIVER_FRAME */
              } else {
                /* UNWIND_PROTECT_FRAME */
                enter_frame_at_STACK();
              }
            } else {
              /* trapped APPLY/EVAL_FRAME
                 like in the tracer: */
              var object values;
              mv_to_list(); values = popSTACK(); /* pack values into list */
              dynamic_bind(S(trace_values),values); /* bind *TRACE-VALUES* */
              break_driver(true); /* call break-driver */
              list_to_mv(Symbol_value(S(trace_values)), /* build values again */
                         fehler_mv_zuviel(framecode(STACK_(0+3))==
                                          TRAPPED_EVAL_frame_info
                                          ? S(eval)
                                          : S(apply)););
              dynamic_unbind(S(trace_values)); /* unbind */
            }
          }
        else {
         #ifdef HAVE_SAVED_REGISTERS
          if ((frame_info & bit(callback_bit_t)) == 0) {
            /* CALLBACK_FRAME */
            var gcv_object_t* new_STACK = topofframe(STACK_0); /* Pointer to Frame */
            /* set callback_saved_registers: */
            callback_saved_registers = (struct registers *)(aint)as_oint(STACK_1);
            /* set STACK, thus unwind frame: */
            setSTACK(STACK = new_STACK);
            goto fertig;
          } else
         #endif
          {
            /* VAR_FRAME or FUN_FRAME */
            var gcv_object_t* new_STACK = topofframe(STACK_0); /* Pointer to Frame */
            if (frame_info & bit(fun_bit_t)) {
              /* for functions: do nothing */
            } else {
              /* VAR_FRAME, bindingptr iterates over the bindungs */
              var gcv_object_t* frame_end = STACKpointable(new_STACK);
              var gcv_object_t* bindingptr = &STACK_(frame_bindings); /* start of the variable-/functionbindings */
              while (bindingptr != frame_end) {
                if (as_oint(*(bindingptr STACKop 0)) & wbit(dynam_bit_o))
                  if (as_oint(*(bindingptr STACKop 0)) & wbit(active_bit_o)) {
                    /* binding static or inactive -> nothing to do
                       binding dynamic and active -> write back value: */
                    TheSymbolflagged(*(bindingptr STACKop varframe_binding_sym))->symvalue =
                      *(bindingptr STACKop varframe_binding_value);
                  }
                bindingptr skipSTACKop varframe_binding_size; /* next binding */
              }
            }
            /* set STACK, thus unwind frame: */
            setSTACK(STACK = new_STACK);
            goto fertig;
          }
        }
      } else {
        /* DYNBIND_FRAME or CALLBACK_FRAME or ENV_FRAME */
        if (frame_info & bit(envbind_bit_t)) {
          /* ENV_FRAME */
          var gcv_object_t* ptr = &STACK_1;
          switch (frame_info & envbind_case_mask_t) {
            case (ENV1V_frame_info & envbind_case_mask_t): /* 1 VAR_ENV */
              aktenv.var_env = *ptr; ptr skipSTACKop 1; break;
            case (ENV1F_frame_info & envbind_case_mask_t): /* 1 FUN_ENV */
              aktenv.fun_env = *ptr; ptr skipSTACKop 1; break;
            case (ENV1B_frame_info & envbind_case_mask_t): /* 1 BLOCK_ENV */
              aktenv.block_env = *ptr; ptr skipSTACKop 1; break;
            case (ENV1G_frame_info & envbind_case_mask_t): /* 1 GO_ENV */
              aktenv.go_env = *ptr; ptr skipSTACKop 1; break;
            case (ENV1D_frame_info & envbind_case_mask_t): /* 1 DECL_ENV */
              aktenv.decl_env = *ptr; ptr skipSTACKop 1; break;
            case (ENV2VD_frame_info & envbind_case_mask_t): /* 1 VAR_ENV and 1 DECL_ENV */
              aktenv.var_env = *ptr; ptr skipSTACKop 1;
                  aktenv.decl_env = *ptr; ptr skipSTACKop 1;
                  break;
            case (ENV5_frame_info & envbind_case_mask_t): /* all 5 Environments */
              aktenv.var_env = *ptr; ptr skipSTACKop 1;
              aktenv.fun_env = *ptr; ptr skipSTACKop 1;
              aktenv.block_env = *ptr; ptr skipSTACKop 1;
              aktenv.go_env = *ptr; ptr skipSTACKop 1;
              aktenv.decl_env = *ptr; ptr skipSTACKop 1;
              break;
            default: NOTREACHED;
          }
        } else {
          /* DYNBIND_FRAME */
          var gcv_object_t* new_STACK = topofframe(STACK_0); /* Pointer to Frame */
          var gcv_object_t* frame_end = STACKpointable(new_STACK);
          var gcv_object_t* bindingptr = &STACK_1; /* start of the bindings */
          /* bindingptr iterates through the bindings */
          until (bindingptr == frame_end) {
            Symbol_value(*(bindingptr STACKop 0)) = *(bindingptr STACKop 1);
            bindingptr skipSTACKop 2; /* next binding */
          }
          /* set STACK, thus unwind frame: */
          setSTACK(STACK = new_STACK);
          goto fertig;
        }
      }
    }
  /* set STACK, thus unwind frame: */
  setSTACK(STACK = topofframe(STACK_0));
 fertig: ;
}

/* UP: "unwinds" the STACK up to the next DRIVER_FRAME and
 jumps into the corresponding top-level-loop.
 if count=0, unwind to TOP; otherwise reset that many times */
nonreturning_function(global, reset, (uintL count)) {
  /* when unwinding UNWIND-PROTECT-frames, don't save values: */
  bool top_p = (count==0);
  VALUES0;
  unwind_protect_to_save.fun = (restartf_t)&reset;
  loop {
    /* does STACK end here? */
    if (eq(STACK_0,nullobj) && eq(STACK_1,nullobj)) {
      driver(); quit(); /* STACK completely gone -> restart */
    }
    if (framecode(STACK_0) & bit(frame_bit_t)) {
      /* at STACK_0: beginning of a frame */
      if (framecode(STACK_0) == DRIVER_frame_info /* DRIVER_FRAME ? */
          && !top_p && --count==0) /* done count resets */
        break; /* yes -> found */
      unwind(); /* unwind frame */
    } else {
      /* STACK_0 contains a normal LISP-object */
      skipSTACK(1);
    }
  }
  /* At STACK_0 a new Driver-Frame starts. */
  enter_frame_at_STACK();
}

/* UP: dynamically binds the symbols of list symlist
 to the the values of list vallist.
 progv(symlist,vallist);
 > symlist, vallist: two lists
 Exactly one variable binding frame is constructed.
 changes STACK
 can trigger GC */
global maygc void progv (object symlist, object vallist) {
  /* check symlist */
  var uintL llen = 0;
  var bool need_new_symlist = true;
  pushSTACK(symlist); pushSTACK(vallist);
  for (pushSTACK(symlist); consp(STACK_0); STACK_0 = Cdr(STACK_0), llen++) {
    var object sym = check_symbol_non_constant(Car(STACK_0),S(progv));
    if (!eq(sym,Car(STACK_0))) { /* changed symbol ==> must copy symlist */
      if (need_new_symlist) {    /* have not copied symlist yet */
        pushSTACK(sym);          /* save sym */
        STACK_1 = STACK_3 = copy_list(STACK_3); /* copy symlist */
        var uintL pos = llen;                 /* skip copy ... */
        while (pos--) STACK_1 = Cdr(STACK_1); /* ... to the right position */
        need_new_symlist = false; /* do not copy symlist twice */
        sym = popSTACK();         /* restore sym */
      }
      Car(STACK_0) = sym;
    }
  }
  skipSTACK(1); vallist = popSTACK(); symlist = popSTACK();
  /* demand room on STACK: */
  get_space_on_STACK(llen * 2 * sizeof(gcv_object_t));
  /* build frame: */
  var gcv_object_t* top_of_frame = STACK; /* Pointer to Frame */
  var object symlistr = symlist;
  while (consp(symlistr)) { /* loop over symbol list */
    var object sym = Car(symlistr);
    pushSTACK(Symbol_value(sym)); /* old value of the variables */
    pushSTACK(sym); /* variable */
    symlistr = Cdr(symlistr);
  }
  finish_frame(DYNBIND);
  /* building of frame completed, now change the values of the variables: */
  while (consp(symlist)) {
    if (atomp(vallist)) {
      /* value list shorter than symbol list
         -> all further "values" are #<UNBOUND> */
      do {
        Symbol_value(Car(symlist)) = unbound;
        symlist = Cdr(symlist);
      } while (consp(symlist));
      break;
    }
    /* symbol obtains new value: */
    Symbol_value(Car(symlist)) = Car(vallist);
    symlist = Cdr(symlist); vallist = Cdr(vallist);
  }
}

/* UP: unwinds the dynamic nesting in STACK up to the frame
 (exclusively), which is pointed to by upto, and then jumps to it.
 unwind_upto(upto);
 > upto: pointer to a frame (into the stack, without typinfo).
 saves the values mv_count/mv_space.
 changes STACK,SP
 can trigger GC
 then jumps to the frame, which was found. */
nonreturning_function(global /*maygc*/, unwind_upto, (gcv_object_t* upto_frame)) {
  GCTRIGGER1(mv_space);
  unwind_protect_to_save.fun        = &unwind_upto;
  unwind_protect_to_save.upto_frame = upto_frame;
  until (STACK == upto_frame) { /* arrived at target-frame? */
    if (framecode(STACK_0) & bit(frame_bit_t)) { /* is it a frame? */
      unwind(); /* yes -> unwind */
      /* (if this is a Unwind-Protect-Frame, then
         unwind_upto(upto_frame) is called again, and we are again here.) */
    } else {
      skipSTACK(1); /* no -> simply go ahead */
    }
  }
  /* now STACK points to the FRAME found. */
  enter_frame_at_STACK();
}

/* UP: throws to the Tag tag and passes the values mv_count/mv_space.
 returns only, if there is no CATCH-Frame for this tag.
 throw_to(tag); */
global void throw_to (object tag) {
  /* search for Catch-Frame with Tag = tag: */
  var gcv_object_t* FRAME = STACK;
  loop { /* search in the Stack starting at FRAME
            for a CATCH-Frame with the same Tag: */
    if (eq(FRAME_(0),nullobj)) /* end of Stack? */
      return; /* yes -> no suitable Catch there -> jump back */
    if (framecode(FRAME_(0)) & bit(frame_bit_t)) {
      /* found frame */
      if ((framecode(FRAME_(0)) == CATCH_frame_info) /* Catch-Frame? */
          && eq(FRAME_(frame_tag),tag)) /* with the same tag? */
        break; /* yes -> search-loop finished */
      /* skip Frame: */
      FRAME = topofframe(FRAME_(0));
    } else {
      FRAME skipSTACKop 1;
    }
  }
  /* FRAME points to the lowest CATCH-Frame with the same Tag */
  unwind_upto(FRAME); /* unwind upto there, then jump */
}

/* UP: Invokes all handlers for condition cond. Returns only, if none
 of these handlers feels responsible (i.e. if each handler returns).
 invoke_handlers(cond);
 can trigger GC
 This deactivates the handler, that is called right now,
 and all newer handlers. */
global maygc void invoke_handlers (object cond) {
  /* Also deactivates the handler being called, and all newer handlers.
     the handler-ranges, which are screened off: */
  var stack_range_t* other_ranges = inactive_handlers;
  var stack_range_t new_range;
  /* Search for Handler-Frame, that handles a Type with (TYPEP cond type): */
  var gcv_object_t* FRAME = STACK;
  loop { /* search in Stack starting at FRAME for a suitable HANDLER-Frame: */
    if (!(other_ranges == NULL) && (FRAME == other_ranges->low_limit)) {
      FRAME = other_ranges->high_limit;
      other_ranges = other_ranges->next;
    } else if (eq(FRAME_(0),nullobj)) { /* End of Stack? */
      break; /* yes -> finised, jump back */
    } else if (framecode(FRAME_(0)) & bit(frame_bit_t)) {
      /* found frame */
      if (framecode(FRAME_(0)) == HANDLER_frame_info) { /* Handler-Frame? */
        /* loop over types of the vectors #(type1 label1 ... typem labelm): */
        var uintL m2 = Svector_length(Car(FRAME_(frame_handlers))); /* 2*m */
        var uintL i = 0;
        do {
          pushSTACK(cond); /* save cond */
          pushSTACK(cond);
          pushSTACK(TheSvector(Car(FRAME_(frame_handlers)))->data[i]); /* typei */
          funcall(S(safe_typep),2); /* execute (SYS::SAFE-TYPEP cond typei) */
          if (!nullp(value1)) { /* found a suitable handler */
            /* CLtL2 S. 873, 884:
               "A handler is executed in the dynamic context
               of the signaler, except that the set of available condition
               handlers will have been rebound to the value that was active
               at the time the condition handler was made active."
               we make the whole thing bullet-proof by an
               Unwind-Protect-Frame: */
            var stack_range_t* saved_inactive_handlers = inactive_handlers;
            new_range.low_limit = STACK;
            new_range.high_limit = topofframe(FRAME_(0));
            new_range.next = other_ranges;
            var gcv_object_t* top_of_frame = STACK;
            var sp_jmp_buf returner; /* return point */
            finish_entry_frame(UNWIND_PROTECT,returner,, {
              var restartf_t fun = unwind_protect_to_save.fun;
              var gcv_object_t* arg = unwind_protect_to_save.upto_frame;
              skipSTACK(2); /* unwind Unwind-Protect-Frame */
              /* Cleanup: reactivate Handler: */
              inactive_handlers = saved_inactive_handlers;
              /* and jump ahead: */
              fun(arg);
              NOTREACHED;
            });
            /* deactivate Handler: */
            inactive_handlers = &new_range;
            if (!nullp(Cdr(FRAME_(frame_handlers)))) {
              /* make information available for Handler: */
              handler_args.condition = STACK_(0+2);
              handler_args.stack = FRAME STACKop 4;
              handler_args.sp = (SPint*)(aint)as_oint(FRAME_(frame_SP));
              handler_args.spdepth = Cdr(FRAME_(frame_handlers));
              /* call Handler: */
              var object closure = FRAME_(frame_closure);
              var object codevec = TheCclosure(closure)->clos_codevec;
              var uintL index = (TheCodevec(codevec)->ccv_flags & bit(7) ? CCV_START_KEY : CCV_START_NONKEY)
                + (uintL)posfixnum_to_V(TheSvector(Car(FRAME_(frame_handlers)))->data[i+1]);
              interpret_bytecode(closure,codevec,index);
            } else {
              /* call C-Handler: */
              void* handler_fn = TheMachineCode(FRAME_(frame_closure));
              ((void (*) (void*, gcv_object_t*, object, object)) handler_fn)
                ((void*)(aint)as_oint(FRAME_(frame_SP)),FRAME,
                 TheSvector(Car(FRAME_(frame_handlers)))->data[i+1],
                 STACK_(0+2));
            }
            skipSTACK(2); /* unwind Unwind-Protect-Frame */
            /* reactivate Handler: */
            inactive_handlers = saved_inactive_handlers;
          }
          cond = popSTACK(); /* cond back */
          i += 2;
        } while (i < m2);
      }
      /* skip Frame: */
      FRAME = topofframe(FRAME_(0));
    } else {
      FRAME skipSTACKop 1;
    }
  }
  var object handler = Symbol_function(S(global_handler));
  if (boundp(handler)) {                 /* unbound during bootstrap */
    pushSTACK(cond); funcall(handler,1); /* (GLOBAL-HANDLER cond) */
  }
}

/* UP: finds out, if an object is a function name, i.e. a Symbol or
 a list of the form (SETF symbol).
 funnamep(obj)
 > obj: Object
 < result: true if function name */
global bool funnamep (object obj) {
  if (symbolp(obj))
    return true;
  if (consp(obj) && eq(Car(obj),S(setf))) {
    obj = Cdr(obj);
    if (consp(obj) && nullp(Cdr(obj)) && symbolp(Car(obj)))
      return true;
  }
  return false;
}

/* UP: find whether the symbol is bound in the environment */
local inline gcv_object_t* symbol_env_search (object sym, object venv)
{
  /* Does the binding at bindptr bind the symbol sym? */
#ifdef NO_symbolflags
  #define binds_sym_p(bindingptr)  \
    (eq(*(bindingptr STACKop 1),sym) /* the right symbol? */ \
     && eq(*(bindingptr STACKop 0),fixnum(bit(active_bit)))) /* active & static? */
#else
  var object cmp = SET_BIT(sym,active_bit_o); /* for comparison: binding must be active */
  #define binds_sym_p(bindingptr)  \
    (eq(*(bindingptr STACKop 0),cmp)) /* right symbol & active & static? */
#endif
 next_env:
  if (framepointerp(venv)) {
    /* Environment is a Pointer to a variable-binding-frame */
    var gcv_object_t* FRAME = TheFramepointer(venv);
    var uintL count = as_oint(FRAME_(frame_anz)); /* number of bindings */
    if (count > 0) {
      var gcv_object_t* bindingsptr = &FRAME_(frame_bindings); /* 1st binding */
      do {
        if (binds_sym_p(bindingsptr)) /* right symbol & active & static? */
          return bindingsptr STACKop varframe_binding_value;
        bindingsptr skipSTACKop varframe_binding_size; /* no: next binding */
      } while (--count);
    }
    venv = FRAME_(frame_next_env);
    goto next_env;
  }
  var bool from_inside_macrolet = false;
  for (;;) {
    if (simple_vector_p(venv)) {
      /* environment is a simple-vector */
      var uintL count = floor(Svector_length(venv),2); /* number of bindings */
      var gcv_object_t* ptr = &TheSvector(venv)->data[0];
      dotimesL(count,count, {
        if (eq(*ptr,sym)) { /* right symbol? */
          if (from_inside_macrolet && !eq(*(ptr+1),specdecl)
              && !symbolmacrop(*(ptr+1)))
            goto macrolet_error;
          return ptr+1;
        }
        ptr += 2; /* next binding */
      });
      venv = *ptr; /* next environment */
      continue;
    } else if (consp(venv)) {
      /* environment is a MACROLET capsule */
      ASSERT(eq(Car(venv),S(macrolet)));
      from_inside_macrolet = true;
      venv = Cdr(venv);
      continue;
    } else
      break;
  }
  /* Environment is NIL */
  return NULL;
#undef binds_sym_p
 macrolet_error:
  pushSTACK(sym); /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(S(macrolet)); pushSTACK(sym);
  fehler(program_error,
         GETTEXT("Invalid access to the value of the lexical variable ~S from within a ~S definition"));
}

/* (SYS::SPECIAL-VARIABLE-P symbol &optional environment)
   tests whether the symbol is a special variable or a constant.
   A missing or NIL environment means the global environment. */
LISPFUN(special_variable_p,seclass_read,1,1,norest,nokey,0,NIL)
{
  var object symbol = check_symbol(STACK_1);
  var object env = STACK_0; skipSTACK(2);
  if (special_var_p(TheSymbol(symbol))) {
    value1 = T;
  } else if (missingp(env)) {
    value1 = NIL;
  } else {
    if (simple_vector_p(env)) {
      var uintL len = Svector_length(env);
      if (len == 2 || len == 5)
        env = TheSvector(env)->data[0]; /* venv */
      else
        fehler_environment(env);
    }
    var gcv_object_t *binding = symbol_env_search(symbol,env);
    if ((binding != NULL) && eq(*binding,specdecl))
      value1 = T;
    else
      value1 = NIL;
  }
  mv_count = 1;
}

/* UP: returns the value of a symbol in an environment.
 sym_value(symbol,venv,&symbolmacro)
 > symbol: Symbol
 > venv: a Variable- and Symbolmacro-Environment
 < symbolmacro: symbol-macro definition, or nullobj if not a symbol-macro
 < result: value of the symbol in this environment, or
           nullobj if a symbol-macro */
local gcv_object_t sym_value (object sym, object env, object* symbolmacro_)
{
  if (special_var_p(TheSymbol(sym))) {
    /* Constants and symbols declared special have only global values. */
    goto global_value;
  } else {
    var gcv_object_t* binding = symbol_env_search(sym,env);
    if (binding != NULL) {
      var object val = *binding;
      if (eq(val,specdecl))
        goto global_value;
      if (symbolmacrop(val)) {
        *symbolmacro_ = val;
        return nullobj;
      }
      *symbolmacro_ = nullobj;
      return val;
    }
    if (symmacro_var_p(TheSymbol(sym))) {
      /* Fetch the symbol-macro definition from the property list: */
      var object symbolmacro = get(sym,S(symbolmacro));
      if (!eq(symbolmacro,unbound)) {
        ASSERT(globalsymbolmacrop(symbolmacro));
        *symbolmacro_ = TheGlobalSymbolmacro(symbolmacro)->globalsymbolmacro_definition;
        return nullobj;
      }
      /* Huh? The symbol-macro definition got lost. */
      clear_symmacro_flag(TheSymbol(sym));
    }
  }
 global_value: /* the global (dynamic) value of the Symbol */
  *symbolmacro_ = nullobj;
  return Symbol_value(sym);
}

/* UP: determines, if a Symbol is a Macro in the current environment.
 sym_macrop(symbol)
 > symbol: Symbol
 < result: true if sym is a Symbol-Macro */
global bool sym_macrop (object sym) {
  var object symbolmacro;
  sym_value(sym,aktenv.var_env,&symbolmacro);
  return !eq(symbolmacro,nullobj);
}

/* UP: Sets the value of a Symbol in the current Environment.
 setq(symbol,value);
 > symbol: Symbol, no constant, not a symbol-macro in the current Environment
 > value: desired value of the Symbols in the current Environment
 < result: value
 can trigger GC */
global maygc object setq (object sym, object value)
{
  if (special_var_p(TheSymbol(sym))) {
    /* Constants and symbols declared special have only global values. */
    goto global_value;
  } else {
    var gcv_object_t* binding = symbol_env_search(sym,aktenv.var_env);
    if (binding != NULL) {
      var object val = *binding;
      if (eq(val,specdecl))
        goto global_value;
      ASSERT(!symbolmacrop(val));
      return *binding = value;
    }
    ASSERT(!symmacro_var_p(TheSymbol(sym)));
  }
 global_value: /* the global (dynamic) value of the Symbol */
  pushSTACK(value); pushSTACK(sym);
  symbol_value_check_lock(S(setq),sym);
  Symbol_value(STACK_0) = STACK_1;
  skipSTACK(1);
  return popSTACK();
}

# UP: returns for a Symbol its function definition in an Environment
# sym_function(sym,fenv)
# > sym: function name (e.g. Symbol)
# > fenv: a function- and macro-bindung-environment
# < result: function definition, either unbound (if undefined function)
#             or Closure/SUBR/FSUBR/Macro/FunctionMacro.
  global object sym_function (object sym, object env)
  {
    var object value;
    {
     next_env:
      if (framepointerp(env)) {
        # Environment is a Pointer to a function-binding-frame
        var gcv_object_t* FRAME = TheFramepointer(env);
        var uintL count = as_oint(FRAME_(frame_anz)); # number of bindings
        if (count > 0) {
          var gcv_object_t* bindingsptr = &FRAME_(frame_bindings); # pointer to the first binding
          dotimespL(count,count, {
            if (equal(*(bindingsptr STACKop 0),sym)) { # right Symbol?
              value = *(bindingsptr STACKop 1); goto fertig;
            }
            bindingsptr skipSTACKop 2; # no: next binding
          });
        }
        env = FRAME_(frame_next_env);
        goto next_env;
      }
      var bool from_inside_macrolet = false;
      for (;;) {
        if (simple_vector_p(env)) {
          # Environment is a Simple-Vector
          var uintL count = floor(Svector_length(env),2); # number of bindings
          var gcv_object_t* ptr = &TheSvector(env)->data[0];
          dotimesL(count,count, {
            if (equal(*ptr,sym)) { # right Symbol?
              value = *(ptr+1);
              if (from_inside_macrolet && !macrop(value))
                goto macrolet_error;
              goto fertig;
            }
            ptr += 2; # next binding
          });
          env = *ptr; # next Environment
          continue;
        } else if (consp(env)) {
          /* environment is a MACROLET capsule */
          ASSERT(eq(Car(env),S(macrolet)));
          from_inside_macrolet = true;
          env = Cdr(env);
          continue;
        } else
          # Environment is NIL
          goto global_value;
      }
    }
   global_value: # global function-definition
    if (!symbolp(sym)) {
      sym = get(Car(Cdr(sym)),S(setf_function)); # (get ... 'SYS::SETF-FUNCTION)
      if (!symbolp(sym)) # should be (uninterned) Symbol
        return unbound; # else undefined
    }
    return Symbol_function(sym);
   fertig: # Symbol found active in Environment, "Value" value
    # (a Closure or Macro or FunctionMacro or NIL)
    # if Definition = NIL (during LABELS), the function is passed for
    # as undefined:
    if (nullp(value))
      value = unbound;
    return value;
   macrolet_error:
    pushSTACK(sym); /* SOURCE-PROGRAM-ERROR slot DETAIL */
    pushSTACK(S(macrolet)); pushSTACK(sym);
    fehler(source_program_error,
           GETTEXT("Invalid access to the local function definition of ~S from within a ~S definition"));
  }

# UP: evaluates a Form in a given Environment.
# eval_5env(form,var,fun,block,go,decl);
# > var_env: value for VAR_ENV
# > fun_env: value for FUN_ENV
# > block_env: value for BLOCK_ENV
# > go_env: value for GO_ENV
# > decl_env: value for DECL_ENV
# > form: Form
# < mv_count/mv_space: values
# can trigger GC
global maygc Values eval_5env (object form, object var_env, object fun_env,
                               object block_env, object go_env, object decl_env) {
  # bind Environments:
  make_ENV5_frame();
  # set current Environments:
  aktenv.var_env = var_env;
  aktenv.fun_env = fun_env;
  aktenv.block_env = block_env;
  aktenv.go_env = go_env;
  aktenv.decl_env = decl_env;
  # evaluate Form:
  eval(form);
  # unwind Environment-Frame:
  unwind();
  return; # finished
}

# UP: evaluates a form in an empty environment.
# eval_noenv(form);
# > form: Form
# < mv_count/mv_space: values
# can trigger GC
global maygc Values eval_noenv (object form) {
  return_Values eval_5env(form,NIL,NIL,NIL,NIL,O(top_decl_env));
}

# UP: "nests" a FUN-Environment, i.e. writes all active bindings
# from the Stack into freshly allocated vectors.
# nest_fun(env)
# > env: FUN-Env
# < result: same environment, no Pointer into the Stack
# can trigger GC
  global maygc object nest_fun (object env)
  {
    var uintL depth = 0; # recursion counter := 0
    # Pseudorecursion with Input env, Output env.
   nest_start: # start of recursion
    if (framepointerp(env)) {
      # env is a pointer to a STACK-Frame.
      check_STACK();
      pushSTACK(env); # save env
      # execute nest_fun(NEXT_ENV(env)) "disrecursified" :-) :
      {
        var gcv_object_t* FRAME = TheFramepointer(env);
        env = FRAME_(frame_next_env); depth++; goto nest_start;
      }
     nest_reentry: depth--;
      # NEXT_ENV is now nested.
      {
        var gcv_object_t* FRAME = TheFramepointer(STACK_0); # next STACK-Frame to be nested
        STACK_0 = env; # bisher genestetes Environment
        var uintL anzahl = as_oint(FRAME_(frame_anz)); # number of not yet netsted bindings
        if (anzahl == 0) {
          # no bindings -> unnecessary, to create a vector.
          env = popSTACK();
        } else {
          # create vector for anzahl bindings:
          env = allocate_vector(2*anzahl+1);
          # and fill:
          {
            var gcv_object_t* ptr = &TheSvector(env)->data[0];
            var gcv_object_t* bindingsptr = &FRAME_(frame_bindings); # Pointer to the first binding
            # put anzahl bindings starting at bindingsptr into the vector at ptr:
            dotimespL(anzahl,anzahl, {
              *ptr++ = *(bindingsptr STACKop 0); # copy binding into the vector
              *ptr++ = *(bindingsptr STACKop 1);
              bindingsptr skipSTACKop 2;
            });
            *ptr++ = popSTACK(); # put nested NEXT_ENV into vector
          }
          FRAME_(frame_next_env) = env; # Vector as NEXT_ENV into the Frame
          FRAME_(frame_anz) = as_object(0); # new number of not yet nested bindings
        }
      }
    }
    # finished with this Nest-substep.
    if (depth>0) # end of recursion
      goto nest_reentry;
    return env;
  }

# UP: "nests" a VAR-Environment, i.e. writes all active bindings
# from the Stack in freshly allocated vectors.
# nest_var(env)
# > env: VAR-Env
# < result: same Environment, no Pointer in the Stack
# can trigger GC
  local maygc object nest_var (object env)
  {
    var uintL depth = 0; # Recursion counter := 0
    # Pseudorecursion with Input env, Output env.
   nest_start: # start of Recursion
    if (framepointerp(env)) {
      # env is a Pointer to a STACK-Frame.
      check_STACK();
      pushSTACK(env); # save env
      # execute nest_var(NEXT_ENV(env)) "disrecursified" :-) :
      {
        var gcv_object_t* FRAME = TheFramepointer(env);
        env = FRAME_(frame_next_env); depth++; goto nest_start;
      }
     nest_reentry: depth--;
      # NEXT_ENV is now nested.
      {
        var gcv_object_t* FRAME = TheFramepointer(STACK_0); # next STACK-Frame to be nested
        STACK_0 = env; # formerly nested Environment
        # Search (from bottom) the first active among the not yet
        # nested bindings:
        var uintL anzahl = as_oint(FRAME_(frame_anz)); # number of not yet nested bindings
        var uintL count = 0;
        var gcv_object_t* bindingsptr = &FRAME_(frame_bindings); # Pointer to the first binding
        until ((count>=anzahl) # all unnested bindings through?
               || (as_oint(*(bindingsptr STACKop 0)) & wbit(active_bit_o))) { # discovered active binding?
          # no -> continue search:
          bindingsptr skipSTACKop varframe_binding_size;
          count++;
        }
        # Below bindingsptr are count inactive bindings.
        # From bindingsptr on there are anzahl-count active, to be nested bindings.
        anzahl = anzahl-count; # number of bindings to be nested
        if (anzahl == 0) {
          # no bindings -> creating a vector is unnecessary.
          env = popSTACK();
        } else {
          # create vector for anzahl bindings:
          env = allocate_vector(2*anzahl+1);
          # and fill:
          {
            var gcv_object_t* ptr = &TheSvector(env)->data[0];
            # put bindungs starting at bindingsptr in the vector at ptr:
            dotimespL(anzahl,anzahl, {
              if (as_oint(*(bindingsptr STACKop varframe_binding_mark)) & wbit(dynam_bit_o)) { # binding dynamic?
                # dynamic binding, lexical scope
                *ptr++ = symbol_without_flags(*(bindingsptr STACKop varframe_binding_sym)); # put Symbol without Flag-Bits in the Vector
                *ptr++ = specdecl; # mark as special reference
                # binding stays active in the Frame
              } else {
                # static binding, lexical scope
                *(bindingsptr STACKop varframe_binding_mark) =
                  CLR_BIT(*(bindingsptr STACKop varframe_binding_mark),active_bit_o); /* deactivate binding */
                *ptr++ = *(bindingsptr STACKop varframe_binding_sym); # copy binding in the vector
                *ptr++ = *(bindingsptr STACKop varframe_binding_value);
              }
              bindingsptr skipSTACKop varframe_binding_size;
            });
            *ptr++ = popSTACK(); # put nested NEXT_ENV in the vector
          }
          FRAME_(frame_next_env) = env; # vector as NEXT_ENV in the Frame
          FRAME_(frame_anz) = fake_gcv_object(count); # new number of not yet nested bindings
        }
      }
    }
    # finished with this Nest-substep.
    if (depth>0) # end of recursion
      goto nest_reentry;
    return env;
  }

/* UP: Nests the Environments in *env (i.e. writes all information in
 Stack-independent structures) and pushes them onto the STACK.
 (The values VAR_ENV, FUN_ENV, BLOCK_ENV, GO_ENV, DECL_ENV will not
 be changed, because inactive bindings might poss. still sit in the frames.
 It has to be feasible, to activate these bindings without change of VAR_ENV.)
 nest_env(env)
 > gcv_environment_t* env: Pointer to five Environments
 < gcv_environment_t* result: Pointer to the Environments in the STACK
 changes STACK, can trigger GC */
global maygc gcv_environment_t* nest_env (gcv_environment_t* env5)
{
  /* First copy all Environments in the STACK: */
  make_STACK_env(env5->var_env,env5->fun_env,env5->block_env,env5->go_env,
                 env5->decl_env,env5 = );
  /* DECL_ENV: Not to be changed. */
  { /* GO_ENV: */
    var object env = env5->go_env;
    var uintL depth = 0; /* recursion depth := 0 */
    /* pseudo-recursion: nests a GO_ENV. */
    /* Input: env, a GO_ENV. Output: env, with Alist. */
   nest_go_start: { /* start of recursion */
      var gcv_object_t* FRAME;
      if (framepointerp(env)) {
        /* env is a pointer into the STACK to a ITAGBODY-frame. */
        check_STACK();
        FRAME = TheFramepointer(env);
        if (framecode(FRAME_(0)) & bit(nested_bit_t)) { /* frame already nested? */
          env = FRAME_(frame_next_env); /* yes -> fetch former Alist */
        } else {
          pushSTACK(env); /* save env */
          /* execute nest_go(NEXT_ENV(env)) "disrecursivied": */
          env = FRAME_(frame_next_env); depth++; goto nest_go_start;
         nest_go_reentry: depth--;
          { /* NEXT_ENV is now nested. */
            var object frame = STACK_0; /* next to be nested STACK-Frame */
            FRAME = uTheFramepointer(frame);
            STACK_0 = env; /* so far nested Environment */
            var gcv_object_t* tagsptr = &FRAME_(frame_bindings); /* Pointer to the bottom Tag */
            var gcv_object_t* frame_end = STACKpointable(topofframe(FRAME_(0))); /* Pointer to Frame */
            var uintL count = /* number of tags */
              /* subtract the pointers tagsptr and frame_end (both without Typinfo!): */
              STACK_item_count(tagsptr,frame_end) / 2;
              { /* create vector for count tags: */
                var object tagvec = allocate_vector(count);
                /* and fill: */
                if (count > 0) {
                  var gcv_object_t* ptr = &TheSvector(tagvec)->data[0];
                  /* put tags starting at tagsptr in the vector at ptr: */
                  dotimespL(count,count, {
                    *ptr++ = *(tagsptr STACKop 0);
                    tagsptr skipSTACKop 2;
                  });
                }
                pushSTACK(tagvec); /* and save */
              }
            { /* create next Alist Cons (cons tag-vector frame-pointer) : */
              var object new_cons = allocate_cons();
              Car(new_cons) = STACK_0; /* tagvec */
              Cdr(new_cons) = frame;
              STACK_0 = new_cons;
            }
            /* and prepend to Alist: */
            env = allocate_cons();
            Car(env) = popSTACK(); /* new_cons */
            Cdr(env) = popSTACK(); /* previous Alist */
            FRAME_(frame_next_env) = env; /* store new NEXT_ENV */
            *(oint*)(&FRAME_(0)) |= wbit(nested_bit_o); /* this frame is now nested. */
          }
        }
      }
      /* finished with this Nest-Substep. */
      if (depth>0) /* end of Recursion */
        goto nest_go_reentry;
      env5->go_env = env; /* file nested GO_ENV */
    }
  }
  { /* BLOCK_ENV: */
    var object env = env5->block_env;
    var uintL depth = 0; /* recursion depth := 0 */
    /* Pseudo-Recursion: nests a BLOCK_ENV. */
    /* Input: env, a BLOCK_ENV. Output: env, with Aliste. */
   nest_block_start: { /* start of recursion */
      var gcv_object_t* FRAME;
      if (framepointerp(env)) {
        /* env is a pointer into the STACK to a IBLOCK-Frame. */
        check_STACK();
        FRAME = TheFramepointer(env);
        if (framecode(FRAME_(0)) & bit(nested_bit_t)) { /* Frame already nested? */
          env = FRAME_(frame_next_env); /* yes -> fetch previous Alist */
        } else {
          pushSTACK(env); /* save env */
          /* execute nest_block(NEXT_ENV(env)) "disrecursified": */
          env = FRAME_(frame_next_env); depth++; goto nest_block_start;
         nest_block_reentry: depth--;
          { /* NEXT_ENV is now nested. */
            var object frame = STACK_0; /* next to be nested STACK-Frame */
            FRAME = TheFramepointer(frame);
            STACK_0 = env; /* so far nested Environment */
            { /* create next Alist Cons (cons Block-Name Frame-Pointer) : */
              var object new_cons = allocate_cons();
              Car(new_cons) = FRAME_(frame_name);
              Cdr(new_cons) = frame;
              pushSTACK(new_cons);
            }
            /* and prepend to the Aliste: */
            env = allocate_cons();
            Car(env) = popSTACK(); /* new_cons */
            Cdr(env) = popSTACK(); /* previous Alist */
            FRAME_(frame_next_env) = env; /* store new NEXT_ENV */
            *(oint*)(&FRAME_(0)) |= wbit(nested_bit_o); /* this frame is now nested. */
          }
        }
      }
    }
    /* finished with this Nest-Substep. */
    if (depth>0) /* end of recursion */
      goto nest_block_reentry;
    env5->block_env = env; /* file nested BLOCK_ENV */
  }
  /* FUN_ENV: */
  env5->fun_env = nest_fun(env5->fun_env);
  /* VAR_ENV: */
  env5->var_env = nest_var(env5->var_env);
  /* done */
  return env5;
}

# UP: Nests the current environments (i.e. writes all Information in
# Stack-independent Structures) and pushes them onto the STACK.
# (The values VAR_ENV, FUN_ENV, BLOCK_ENV, GO_ENV, DECL_ENV are not
# modified, because inactive bindings might poss. still sit in the Frames.
# It has to be feasible, to activate these bindings without change of VAR_ENV.)
# nest_aktenv()
# < gcv_environment* result: Pointer to the Environments in the STACK
# changes STACK, can trigger GC
#define nest_aktenv()  nest_env(&aktenv)

# UP: augments a Declaration-Environment with a decl-spec.
# augment_decl_env(declspec,env)
# > declspec: Declaration-Specifier, a Cons
# > env: Declaration-Environment
# < result: new (poss. augmented) Declaration-Environment
# can trigger GC
  global maygc object augment_decl_env (object new_declspec, object env)
  {
    var object decltype = Car(new_declspec); # Declaration-Type
    # Is this a declaration type to be payed attention to?
    # Is there a Decl-Spec of the form (DECLARATION ... decltype ...) in env?
    # Aside: The List O(declaration_types) is the last Decl-Spec in env.
    if (symbolp(decltype)) {
      # loop over all local to be respected Declaration-Types:
      var object declspecs = env;
      while (consp(declspecs)) { # loop over all declspecs from env
        var object declspec = Car(declspecs);
        if (eq(Car(declspec),S(declaration)) # (DECLARATION ...) ?
            && !nullp(memq(decltype,Cdr(declspec))))
          goto beachten;
        declspecs = Cdr(declspecs);
      }
    }
    # not to be respected Declaration.
    return env; # leave env unchanged
   beachten:
    # a to be respected Declaration -> env := (cons new_declspec env)
    pushSTACK(env); pushSTACK(new_declspec);
    env = allocate_cons();
    Car(env) = popSTACK(); Cdr(env) = popSTACK();
    return env;
  }

# UP: expands a form, if possible, (however it doesn't, if FSUBR-Call
# or Symbol or FunctionMacro-Call) in an Environment
# macroexp(form,venv,fenv);
# > form: Form
# > venv: a Variable- and Symbolmacro-Environment
# > fenv: a Function- and Macrobinding-Environment
# < value1: the expansion
# < value2: NIL, if not expanded,
#           T, if expansion has taken place
# can trigger GC
  global maygc void macroexp (object form, object venv, object fenv)
  {
    if (consp(form)) { # only lists can be a macro-call
      var object funname = Car(form); # function name
      if (symbolp(funname)) {
        var object fdef = sym_function(funname,fenv); # fetch function definition
        # is it a #<MACRO expander> ?
        if (macrop(fdef)) {
          # yes -> expand:
          # execute (FUNCALL *MACROEXPAND-HOOK* expander form env) :
          pushSTACK(TheMacro(fdef)->macro_expander); # expander as first argument
          pushSTACK(form); # form as second argument
          pushSTACK(fenv);
          pushSTACK(nest_var(venv)); # nested Variable- and Symbolmacro-Environment
          STACK_1 = nest_fun(STACK_1); # nested Functions- and Macrobinding-Environment
          var object env = allocate_vector(2); # Environment for both
          TheSvector(env)->data[0] = popSTACK(); # venv as 1. component
          TheSvector(env)->data[1] = STACK_0;    # fenv as 2. component
          STACK_0 = env; # Environment as third Argument
          funcall(Symbol_value(S(macroexpand_hook)),3);
          value2 = T; # expanded Form as 1. value, T as 2. value
          return;
        }
      }
    }
    # else, don't expand:
    value1 = form; value2 = NIL;
  }

# UP: expands a form, if possible, (also, when FSUBR-Call or
# Symbol, however not, when FunctionMacro-Call) in an Environment
# macroexp0(form,env);
# > form: Form
# > env: a Macroexpansion-Environment
# < value1: the Expansion
# < value2: NIL, if not expanded,
#           T, if expansion has taken place
# can trigger GC
  global maygc void macroexp0 (object form, object env)
  {
    if (consp(form)) { # only lists can be a macro-call
      var object funname = Car(form); # function name
      if (symbolp(funname)) {
        var object fdef = sym_function(funname,TheSvector(env)->data[1]); # fetch function definition
        if (fsubrp(fdef)) {
          # fdef is a FSUBR, so the global function definition was valid.
          # loop up, if the property list contains a macro definition:
          var object expander = get(funname,S(macro)); # search for Property SYS::MACRO
          if (boundp(expander)) {
            # found. Expand with th Expander from the property list:
            # execute (FUNCALL *MACROEXPAND-HOOK* expander form env) :
            pushSTACK(expander); # expander as first argument
            pushSTACK(form); # form as second Argument
            pushSTACK(env); # environment as third argument
            funcall(Symbol_value(S(macroexpand_hook)),3);
            value2 = T; # expanded form as 1. value, t as 2. value
            return;
          }
        } else {
          # 4 possibilities:
          # #UNBOUND/SUBR/Closure (global or lexical function def.)
          #   -> don't expand
          # #<MACRO expander> (lexical macro definition)
          #   -> expand (call expander)
          # #<FUNCTION-MACRO function expander> (lexical FunctionMacro-
          #   Definition) -> don't expand, because
          #   (MACRO-FUNCTION funname) => NIL
          # Symbol (lexical function definition during SYS::%EXPAND)
          # expand: (list* 'FUNCALL Symbol (cdr form))
          if (macrop(fdef)) {
            # #<MACRO expander> -> expand:
            # execute (FUNCALL *MACROEXPAND-HOOK* expander form env) :
            pushSTACK(TheMacro(fdef)->macro_expander); # Expander as first Argument
            pushSTACK(form); # Form as second Argument
            pushSTACK(env); # Environment as third Argument
            funcall(Symbol_value(S(macroexpand_hook)),3);
            value2 = T; # expanded Form as 1. value, T as 2. value
            return;
          } else if (symbolp(fdef)) {
            # fdef a Symbol
            # Must be expanded to (FUNCALL fdef ...) :
            pushSTACK(Cdr(form)); # (cdr form)
            pushSTACK(fdef); # Symbol
            {
              var object new_cons = allocate_cons();
              Car(new_cons) = popSTACK(); Cdr(new_cons) = STACK_0;
              STACK_0 = new_cons; # (cons Symbol (cdr form))
            }
            {
              var object new_cons = allocate_cons();
              Car(new_cons) = S(funcall); Cdr(new_cons) = popSTACK();
              value1 = new_cons; # (cons 'FUNCALL (cons Symbol (cdr form)))
            }
            value2 = T; return; # expansion has taken place.
          }
        }
      }
    } else if (symbolp(form)) {
      var object symbolmacro;
      var object val = sym_value(form,TheSvector(env)->data[0],&symbolmacro);
      if (!eq(symbolmacro,nullobj)) { # found Symbol-Macro?
        # yes -> expand
        value1 = TheSymbolmacro(symbolmacro)->symbolmacro_expansion; value2 = T;
        return;
      }
    }
    # else, don't expand:
    value1 = form; value2 = NIL;
  }

/* UP: Parse-Declarations-Docstring. Detaches those from a list of forms,
 that have to be viewed as declarations resp. documentation string.
 parse_dd(formlist)
 > formlist: ( {decl|doc-string} . body )
 < value1: body
 < value2: List of decl-specs
 < value3: Doc-String or NIL
 < result: true if one (COMPILE)-declaration occurred, else false
 can trigger GC */
global maygc bool parse_dd (object formlist)
{
  pushSTACK(formlist); /* store formlist for error message */
  pushSTACK(NIL); /* preliminary Doc-String */
  pushSTACK(NIL); /* start of decl-spec-Liste */
  /* stack layout: formlist, docstring, declspecs. */
  var bool compile_decl = false; /* flag: (COMPILE)-declaration occurred */
  var object body = formlist; /* rest of the form-list */
  while (consp(body)) {
    var object form = Car(body); /* next form */
    var object body_rest = Cdr(body); /* shorten body */
    if (stringp(form)) { /* found Doc-String? */
      if (atomp(body_rest)) /* at the last position of the form list? */
        goto fertig; /* yes -> last form can't be a Doc-String! */
      if (!nullp(STACK_1)) { /* preceding Doc-String? */
        /* yes -> more than one Doc-String is too much: */
        pushSTACK(STACK_2);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
        pushSTACK(STACK_0);
        fehler(source_program_error,
               GETTEXT("Too many documentation strings in ~S"));
      }
      STACK_1 = form; /* new Doc-String */
      body = body_rest;
    } else if (consp(form) && eq(Car(form),S(declare))) {/* (DECLARE ...) */
      /* cons decl-specs one by one onto STACK_0: */
      pushSTACK(body_rest); /* save body_rest */
      pushSTACK(Cdr(form)); /* list of the new decl-specs */
      while (mconsp(STACK_0)) {
        var object declspec = Car(STACK_0); /* next decl-spec */
        { /* check for (COMPILE) */
          /* Test: (EQUAL d '(COMPILE)) =
               (and (consp d) (eq (car d) 'COMPILE) (null (cdr d))) */
          if (consp(declspec)
              && eq(Car(declspec),S(compile))
              && nullp(Cdr(declspec)))
            compile_decl = true;
          else if (consp(declspec) && eq(Car(declspec),S(optimize))) {
            pushSTACK(Cdr(declspec)); funcall(S(note_optimize),1);
            pushSTACK(value1);
            declspec = allocate_cons();
            Car(declspec) = S(optimize);
            Cdr(declspec) = popSTACK(); /* value1 */
          }
        }
        { /* push this declaration onto STACK_(0+2) : */
          pushSTACK(declspec);
          var object new_cons = allocate_cons();
          Car(new_cons) = popSTACK(); /* declspec */
          Cdr(new_cons) = STACK_(0+2);
          STACK_(0+2) = new_cons;
        }
        /* go to next decl-spec: */
        STACK_0 = Cdr(STACK_0);
      }
      skipSTACK(1);
      body = popSTACK(); /* body := old body_rest */
    } else {
     fertig: /* finished with loop over the form list */
      break;
    }
  }
  value1 = body;
  value2 = nreverse(popSTACK()); /* decl-spec-Liste */
  value3 = popSTACK(); /* Doc-String */
  skipSTACK(1);
  return compile_decl;
}

# UP: binds *EVALHOOK* and *APPLYHOOK* dynamically to the specified values.
# bindhooks(evalhook_value,applyhook_value);
# > evalhook_value: value for *EVALHOOK*
# > applyhook_value: value for *APPLYHOOK*
# changes STACK
global void bindhooks (object evalhook_value, object applyhook_value) {
  # build frame:
  {
    var gcv_object_t* top_of_frame = STACK; # Pointer to Frame
    pushSTACK(Symbol_value(S(evalhookstern)));  # old value of *EVALHOOK*
    pushSTACK(S(evalhookstern));                # *EVALHOOK*
    pushSTACK(Symbol_value(S(applyhookstern))); # old value of *APPLYHOOK*
    pushSTACK(S(applyhookstern));               # *APPLYHOOK*
    finish_frame(DYNBIND);
  }
  # Frame got ready, now change the values of the variables:
  Symbol_value(S(evalhookstern)) = evalhook_value; # (SETQ *EVALHOOK* evalhook_value)
  Symbol_value(S(applyhookstern)) = applyhook_value; # (SETQ *APPLYHOOK* applyhook_value)
}

# UP: binds *EVALHOOK* and *APPLYHOOK* dynamically to NIL.
# bindhooks_NIL();
# changes STACK
#define bindhooks_NIL()  bindhooks(NIL,NIL)

# UP: Determines the source-lambdabody of a lambda body.
# lambdabody_source(lambdabody)
# > lambdabody: Lambdabody (a Cons)
# < result: Source-Lambdabody (unbound if no source specified)
local object lambdabody_source (object lambdabody) {
  var object body = Cdr(lambdabody);
  # body = ((DECLARE (SOURCE ...) ...) ...) ?
  if (consp(body)) {
    var object form = Car(body); # first Form
    # form = (DECLARE (SOURCE ...) ...) ?
    if (consp(form) && eq(Car(form),S(declare))) {
      var object declspecs = Cdr(form);
      # declspecs = ((SOURCE ...) ...) ?
      if (consp(declspecs)) {
        var object declspec = Car(declspecs);
        # declspec = (SOURCE ...) ?
        if (consp(declspec) && eq(Car(declspec),S(source))) {
          var object declspecr = Cdr(declspec);
          if (consp(declspecr))
            # found Source
            return Car(declspecr);
        }
      }
    }
  }
  return unbound;
}

/* UP: Inserts an implicit BLOCK in a lambda body.
 add_implicit_block();
 > STACK_1: function name
 > STACK_0: lambda body
 > value1: body
 > value2: list of decl-specs
 > value3: Doc-String or NIL
 < STACK_0: new lambda body
 can trigger GC */
local /*maygc*/ void add_implicit_block (void)
{
  GCTRIGGER3(value1,value2,value3);
  /* Replace lambdabody with
 (cons (car lambdabody) ; lambda list
       (multiple-value-bind (body-rest declarations docstring)
           (sys::parse-body (cdr lambdabody) t) ; body
         (append (if declarations (list (cons 'DECLARE declarations)))
                 (if docstring (list docstring))
                 (list (list* 'BLOCK (function-block-name name)
                       body-rest))))) */
  var object new_body;
  pushSTACK(value2);            /* declarations */
  pushSTACK(value3);            /* docstring */
  pushSTACK(funname_blockname(STACK_(1+2))); /* blockname */
  pushSTACK(value1);                         /* body-rest */
  { /* stack layout: name, lambdabody, declarations, docstring,
                     blockname, body-rest. */
    var object tmp = allocate_cons();
    Cdr(tmp) = popSTACK(); Car(tmp) = STACK_0;
    STACK_0 = tmp;
  }
  {
    var object tmp = allocate_cons();
    Car(tmp) = S(block); Cdr(tmp) = STACK_0;
    STACK_0 = tmp;
  }
  { /* stack layout: name, lambdabody, declarations, docstring, block-form. */
    var object tmp = allocate_cons();
    Car(tmp) = popSTACK();
    new_body = tmp;
  }
  /* stack layout: name, lambdabody, declarations, docstring. */
  if (nullp(STACK_0)) {
    skipSTACK(1);
  } else {
    pushSTACK(new_body);
    var object tmp = allocate_cons();
    Cdr(tmp) = popSTACK(); Car(tmp) = popSTACK();
    new_body = tmp;
  }
  /* stack layout: name, lambdabody, declarations. */
  if (nullp(STACK_0)) {
    STACK_0 = new_body;
  } else {
    pushSTACK(new_body);
    {
      var object tmp = allocate_cons();
      Car(tmp) = S(declare); Cdr(tmp) = STACK_1;
      STACK_1 = tmp;
    }
    {
      var object tmp = allocate_cons();
      Cdr(tmp) = popSTACK(); Car(tmp) = STACK_0;
      STACK_0 = tmp;
    }
  }
  { /* stack layout: name, lambdabody, new-body. */
    var object tmp = allocate_cons();
    Cdr(tmp) = popSTACK(); Car(tmp) = Car(STACK_0);
    STACK_0 = tmp;
  }
}

LISPFUNNR(add_implicit_block,2)
{ /* (ADD-IMPLICIT-BLOCK name (lambda-list . lambda-body))
     inserts an implicit BLOCK in the BODY */
  parse_dd(Cdr(STACK_0));       /* just the lambda-body */
  add_implicit_block();
  VALUES1(STACK_0);
  skipSTACK(2);
}

LISPFUNNR(function_block_name,1)
{ /* returns the name of the implicit block for a function-name */
  var object funname =
    check_funname(type_error,S(function_block_name),popSTACK());
  VALUES1(funname_blockname(funname));
}

/* UP: Creates the corresponding Closure for a Lambdabody by decomposition
 of the lambda list and poss. macro-expansion of all forms.
 get_closure(lambdabody,name,blockp,env)
 > lambdabody: (lambda-list {decl|doc} {form})
 > name: Name, a Symbol or (SETF symbol)
 > blockp: if an implicit BLOCK has to be inserted
 > env: Pointer to the five distinct environments:
        env->var_env = VENV, env->fun_env = FENV,
        env->block_env = BENV, env->go_env = GENV,
        env->decl_env = DENV.
 < result: Closure
 can trigger GC */
global maygc object get_closure (object lambdabody, object name, bool blockp,
                                 gcv_environment_t* env)
{
  /* Lambdabody must be a Cons: */
  if (atomp(lambdabody)) {
    pushSTACK(lambdabody);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
    pushSTACK(name);
    fehler(source_program_error,GETTEXT("~S: lambda-list for ~S is missing"));
  }
  { /* and the CAR must be a List: */
    var object lambdalist = Car(lambdabody);
    if (!listp(lambdalist)) {
      pushSTACK(lambdalist);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
      pushSTACK(lambdalist); pushSTACK(name); pushSTACK(S(function));
      fehler(source_program_error,
             GETTEXT("~S: lambda-list for ~S should be a list, not ~S"));
    }
  }
  pushSTACK(name);
  pushSTACK(lambdabody);
  /* stack layout: name, lambdabody.
     decompose ({decl|doc} {form}) */
  if (parse_dd(Cdr(lambdabody))) {
    /* A (COMPILE)-Declaration occurred.
       replace Lambdabody by its source (because some Macros
       can be compiled more efficiently than their Macro-Expansion): */
    { var object source = lambdabody_source(STACK_0);
      if (!boundp(source)) {
        if (blockp)
          add_implicit_block();
      } else {
        STACK_0 = source;
      }
    }
    { /* nest environments: */
      var gcv_environment_t* stack_env = nest_env(env); /* push on STACK */
     #if !defined(STACK_UP)
      /* and transfer over here */
      var object my_var_env = stack_env->var_env;
      var object my_fun_env = stack_env->fun_env;
      var object my_block_env = stack_env->block_env;
      var object my_go_env = stack_env->go_env;
      var object my_decl_env = stack_env->decl_env;
      skipSTACK(5); /* and pop from STACK again */
      pushSTACK(my_var_env);
      pushSTACK(my_fun_env);
      pushSTACK(my_block_env);
      pushSTACK(my_go_env);
      pushSTACK(my_decl_env);
     #endif
      /* stack layout: name, lambdabody, venv, fenv, benv, genv, denv. */
    }
    /* (SYS::COMPILE-LAMBDA name lambdabody venv fenv benv genv denv t) : */
    pushSTACK(T); funcall(S(compile_lambda),8);
    return value1; /* compiled Closure as value */
  }
  { /* build Interpreted Closure: */
    var object source = lambdabody_source(STACK_0);
    if (!boundp(source)) { /* no source specified -> expand Lambdabody: */
      if (blockp)
        add_implicit_block();
      /* call (SYS::%EXPAND-LAMBDABODY-MAIN lambdabody venv fenv) : */
      pushSTACK(STACK_0); /* Lambdabody */
      pushSTACK(nest_var(env->var_env)); /* nested Variable Environment */
      pushSTACK(nest_fun(env->fun_env)); /* nested Function Environment */
      funcall(S(expand_lambdabody_main),3);
      lambdabody = value1; /* expanded Lambdabody */
    } else { /* Source specified -> it replaces the old Lambdabody: */
      lambdabody = STACK_0; /* Lambdabody */
      STACK_0 = source; /* Source-Lambdabody */
    }
  }
  /* Now  STACK_0      is the Source-Lambdabody,
          lambdabody   is the Lambdabody to be used. */
  pushSTACK(Car(lambdabody)); /* Lambdalist */
  /* decompose ({decl|doc} {form}) : */
  parse_dd(Cdr(lambdabody));
  pushSTACK(value1); /* Body */
  pushSTACK(value2); /* Declarations */
  pushSTACK(value3); /* Doc-String or NIL */
  var gcv_object_t* closure_; /* Pointer to the Closure in the STACK */
  { /* create Closure (filled with NIL): */
    var object closure = allocate_closure(iclos_length,seclass_default<<4);
    /* and fill partially: */
    TheIclosure(closure)->clos_docstring = popSTACK(); /* Doc-String */
    var object declarations              = popSTACK(); /* Declarations */
    TheIclosure(closure)->clos_body      = popSTACK(); /* Body */
    var object lambdalist                = popSTACK(); /* Lambda-List */
    TheIclosure(closure)->clos_form      = popSTACK(); /* Source-Lambdabody */
    TheIclosure(closure)->clos_name      = STACK_0;    /* Name */
    /* and save: */
    STACK_0 = closure;
    /* stack layout: closure. */
    closure_ = &STACK_0; /* Pointer to the Closure in the STACK */
    if (!nullpSv(defun_accept_specialized_lambda_list)
        && functionp(Symbol_function(S(specialized_lambda_list_to_ordinary)))) {
      /* convert lambda list to ordinary */
      pushSTACK(declarations); /* save */
      pushSTACK(lambdalist); pushSTACK(S(function));
      funcall(S(specialized_lambda_list_to_ordinary),2);
      lambdalist = value1;       /* new ordinary lambda list */
      declarations = popSTACK(); /* restore */
      if (!nullp(value2))        /* merge in declarations */
        declarations = nreconc(value2,declarations);
    }
    pushSTACK(lambdalist); pushSTACK(lambdalist); pushSTACK(lambdalist);
    pushSTACK(declarations);
  }
  { /* nest Environments and put them nested in the closure: */
    var gcv_environment_t* stack_env = nest_env(env);
    var object closure = *closure_;
    TheIclosure(closure)->clos_var_env   = stack_env->var_env  ;
    TheIclosure(closure)->clos_fun_env   = stack_env->fun_env  ;
    TheIclosure(closure)->clos_block_env = stack_env->block_env;
    TheIclosure(closure)->clos_go_env    = stack_env->go_env   ;
    TheIclosure(closure)->clos_decl_env  = stack_env->decl_env ;
    skipSTACK(5);
    /* keywords:=0, as long as &KEY is missing: */
    TheIclosure(closure)->clos_keywords = Fixnum_0;
  }
  /* stack layout:
     closure, lambdalist, lalist-save, lalist-rest, declarations */
  var uintL spec_count = 0; /* number of dynamic references */
  var uintL req_count  = 0; /* number of required-parameters */
  var uintL opt_count  = 0; /* number of optional-parameters */
  var uintL key_count  = 0; /* number of keyword-parameters */
  var uintL aux_count  = 0; /* number of &AUX-variables */
  var uintL var_count  = 0; /* total number of the variables on the STACK */
  { /* process declarations:
       read dynamically referenced variables from the decl-spec-list
       declarations and push them on STACK. Other to be respected
       declarations change the declarations-environment of the Closure. */
    var object declarations = popSTACK();
    while (consp(declarations)) { /* all decl-specs processed? */
      var object declspec = Car(declarations);
      /* declspec must be a List: */
      if (atomp(declspec)) {
        pushSTACK(declspec);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
        pushSTACK(declspec); pushSTACK(S(function));
        fehler(source_program_error,
               GETTEXT("~S: illegal declaration ~S"));
      }
      /* process SPECIAL-declaration: */
      if (eq(Car(declspec),S(special))) { /* SPECIAL-declaration ? */
        var object declspecrest = Cdr(declspec);
        while (consp(declspecrest)) {
          var object sym = Car(declspecrest);
          if (!symbolp(sym)) {
            pushSTACK(declarations); pushSTACK(declspec); /* save */
            pushSTACK(declspecrest);
            sym = check_symbol_special(sym,S(function));
            declspecrest = popSTACK(); Car(declspecrest) = sym;
            declspec = popSTACK(); declarations = popSTACK(); /* restore */
          }
          /* push Symbol on STACK: */
          check_STACK(); pushSTACK(sym); spec_count++; var_count++;
          declspecrest = Cdr(declspecrest);
        }
      }
      /* process other declaration: */
      pushSTACK(Cdr(declarations)); /* shorten and save declarations */
      {
        var object denv = TheIclosure(*closure_)->clos_decl_env;
        denv = augment_decl_env(declspec,denv);
        TheIclosure(*closure_)->clos_decl_env = denv;
      }
      declarations = popSTACK();
    }
  }
  /* stack layout:
     closure, lambdalist, lalist-save, lalist-rest [special symbols]* */
  var gcv_object_t *lalist_ = closure_ STACKop -2; /* remaining lambda list */
  var gcv_object_t *lalist_save_ = closure_ STACKop -3; /* save fixed items */
  var object item; /* element of the lambda list */
  /* Macro:
     NEXT_ITEM(&OPTIONAL_label,&REST_label,&KEY_label,
               &ALLOW-OTHER-KEYS_label,&AUX_label,Ende_label)
     shortens the rest of the lambda list, moves the next Element to "item"
     and in case of one of the 6 specified lambda-list-markers, it jumps to
     the respective locations. */
  #define NEXT_ITEM(opt_label,rest_label,key_label,allow_label,aux_label,end_label) \
    { if (atomp(*lalist_)) goto end_label; /* Lambda-List finished? */  \
      item = Car(*lalist_); *lalist_save_ = *lalist_; /* next element */ \
      *lalist_ = Cdr(*lalist_); /* shorten List */                      \
      if (eq(item,S(LLoptional)))         goto opt_label;   /* &OPTIONAL ? */ \
      if (eq(item,S(LLrest)))             goto rest_label;  /* &REST ? */ \
      if (eq(item,S(LLkey)))              goto key_label;   /* &KEY ? */ \
      if (eq(item,S(LLallow_other_keys))) goto allow_label; /* &ALLOW-OTHER-KEYS ? */ \
      if (eq(item,S(LLaux)))              goto aux_label;   /* &AUX ? */ \
      if (eq(item,S(LLbody)))             goto badLLkey;    /* &BODY ? */ \
    }
 req: /* process required-parameter push on STACK: */
  while (1) {
    NEXT_ITEM(opt,rest,key,badLLkey,aux,ende);
    item = check_symbol_non_constant(item,S(function));
    Car(*lalist_save_) = item;
    /* push Variable on STACK: */
    check_STACK();
    pushSTACK(item); pushSTACK(Fixnum_0); req_count++; var_count++;
  }
 opt: /* process &OPTIONAL-parameter, push on STACK and
         put Init-Forms into the Closure: */
  while(1) {
    NEXT_ITEM(badLLkey,rest,key,badLLkey,aux,ende);
    var object init_form;
    /* Parse variable spezification in item:
         var  or  (var [init [svar]])
       push var and poss. svar on STACK, set in var poss.
       the svar_bit. Returns also init (or NIL) in init_form. */
    check_STACK();
    if (atomp(item)) {
      item = check_symbol_non_constant(item,S(function));
      Car(*lalist_save_) = item;
      /* push variable on STACK: */
      pushSTACK(item); pushSTACK(Fixnum_0); opt_count++; var_count++;
      init_form = NIL; /* Default-Init */
    } else {
      var object item_rest = item;
      /* first list-element: var */
      pushSTACK(item_rest);
      item = check_symbol_non_constant(Car(item),S(function));
      item_rest = popSTACK(); Car(item_rest) = item;
      item_rest = Cdr(item_rest);
      /* push variable on STACK: */
      pushSTACK(item); pushSTACK(Fixnum_0); opt_count++; var_count++;
      if (consp(item_rest)) {
        init_form = Car(item_rest); /* second list-element: init */
        item_rest = Cdr(item_rest);
        if (consp(item_rest)) {
          if (mconsp(Cdr(item_rest))) { /* varspec is too lang */
            pushSTACK(item_rest);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
            pushSTACK(*(closure_ STACKop -1)); /* entire Lambda-List */
            pushSTACK(S(LLoptional)); pushSTACK(S(function));
            fehler(source_program_error,
                   GETTEXT("~S: variable specification after ~S too long: ~S"));
          }
          /* third list-element: svar */
          pushSTACK(init_form); pushSTACK(item_rest);
          item = check_symbol_non_constant(Car(item_rest),S(function));
          item_rest = popSTACK(); Car(item_rest) = item;
          init_form = popSTACK();
          /* set svar-bit for var: */
          STACK_0 = fixnum_inc(STACK_0,bit(svar_bit));
          /* push variable on STACK: */
          pushSTACK(item); pushSTACK(Fixnum_0); var_count++;
        }
      } else
        init_form = NIL; /* Default-Init */
    }
    /* push init_form in front of (clos_opt_inits closure) : */
    pushSTACK(init_form);
    {
      var object new_cons = allocate_cons();
      Car(new_cons) = popSTACK();
      var object closure = *closure_;
      Cdr(new_cons) = TheIclosure(closure)->clos_opt_inits;
      TheIclosure(closure)->clos_opt_inits = new_cons;
    }
  }
 rest: /* process &REST-parameter and push on Stack: */
  NEXT_ITEM(badrest,badrest,badrest,badrest,badrest,badrest);
  item = check_symbol_non_constant(item,S(function));
  Car(*lalist_save_) = item;
  /* push variable on STACK: */
  pushSTACK(item); pushSTACK(Fixnum_0); var_count++;
  /* set Rest-Flag to T: */
  TheIclosure(*closure_)->clos_rest_flag = T;
  NEXT_ITEM(badLLkey,badLLkey,key,badLLkey,aux,ende);
  pushSTACK(item);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(*(closure_ STACKop -1)); /* entire Lambda-List */
  pushSTACK(S(LLaux)); pushSTACK(S(LLkey));
  pushSTACK(S(LLrest)); pushSTACK(S(function));
  fehler(source_program_error,
         GETTEXT("~S: ~S var must be followed by ~S or ~S or end of list: ~S"));
 badrest:
  pushSTACK(*(closure_ STACKop -1)); /* entire Lambda-List */
  pushSTACK(STACK_0);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(S(LLrest)); pushSTACK(S(function));
  fehler(source_program_error,
         GETTEXT("~S: ~S must be followed by a variable: ~S"));
 key: /* process &KEY-Parameter, push on STACK
         and put Init-Forms in the Closure: */
  TheIclosure(*closure_)->clos_keywords = NIL; /* keywords:=NIL */
  while(1) {
    NEXT_ITEM(badLLkey,badLLkey,badLLkey,allow,aux,ende);
    var object keyword;
    var object init_form;
    /* Parse variable-spezification in item:
         var  or  (var [init [svar]])  or ((key var) [init [svar]])
       push var and poss. svar on STACK, set in var poss.
       the svar_bit. Returns also the Keyword in keyword and
       init (or NIL) in init_form. */
    check_STACK();
    if (atomp(item)) {
      item = check_symbol_non_constant(item,S(function));
      Car(*lalist_save_) = item;
      /* push variable on STACK: */
      pushSTACK(item); pushSTACK(Fixnum_0); key_count++; var_count++;
      /* fetch Keyword: */
      keyword = intern_keyword(Symbol_name(item));
      /* Default-Init: */
      init_form = NIL;
    } else {
      var object item_rest = item; /* (item [init [svar]]) */
      item = Car(item); /* first list-element: var or (key var) */
      pushSTACK(item_rest); /* save */
      if (atomp(item)) {
        item = check_symbol_non_constant(item,S(function)); /* item = var */
        /* push variable on STACK: */
        item_rest = popSTACK(); /* restore */
        Car(item_rest) = item; item_rest = Cdr(item_rest); /*([init [svar]])*/
        pushSTACK(item); pushSTACK(Fixnum_0); key_count++; var_count++;
        /* fetch Keyword: */
        pushSTACK(item_rest); /* save */
        keyword = intern_keyword(Symbol_name(item));
        item_rest = popSTACK(); /* restore */
      } else {
        pushSTACK(item);
        /* item = (key var) */
        keyword = check_symbol(Car(item)); /* key */
        item = popSTACK(); Car(item) = keyword;
        item = Cdr(item); /* (var) */
        if (!(consp(item) && matomp(Cdr(item))))
          goto fehler_keyspec;
        pushSTACK(keyword); pushSTACK(item); /* save */
        item = check_symbol_non_constant(Car(item),S(function)); /* var */
        Car(popSTACK()) = item; keyword = popSTACK(); /* restore */
        item_rest = popSTACK(); item_rest = Cdr(item_rest); /*([init [svar]])*/
        /* push variable on STACK: */
        pushSTACK(item); pushSTACK(Fixnum_0); key_count++; var_count++;
      }
      if (consp(item_rest)) {
        init_form = Car(item_rest); /* second list-element: init */
        item_rest = Cdr(item_rest); /* ([svar]) */
        if (consp(item_rest)) {
          if (mconsp(Cdr(item_rest)))
            goto fehler_keyspec;
          /* third list-element: svar */
          pushSTACK(init_form); pushSTACK(keyword); pushSTACK(item_rest);
          item = check_symbol_non_constant(Car(item_rest),S(function));
          item_rest = popSTACK(); Car(item_rest) = item;
          keyword = popSTACK(); init_form = popSTACK(); /* restore */
          /* set svar-Bit in var: */
          STACK_0 = fixnum_inc(STACK_0,bit(svar_bit));
          /* push variable on STACK: */
          pushSTACK(item); pushSTACK(Fixnum_0); var_count++;
        }
      } else
        init_form = NIL; /* Default-Init */
    }
    /* push keyword in front of (clos_keywords closure) and
       push init_form in front of (clos_key_inits closure) : */
    pushSTACK(init_form); pushSTACK(keyword);
    {
      var object new_cons = allocate_cons();
      Car(new_cons) = popSTACK();
      var object closure = *closure_;
      Cdr(new_cons) = TheIclosure(closure)->clos_keywords;
      TheIclosure(closure)->clos_keywords = new_cons;
    }
    {
      var object new_cons = allocate_cons();
      Car(new_cons) = popSTACK();
      var object closure = *closure_;
      Cdr(new_cons) = TheIclosure(closure)->clos_key_inits;
      TheIclosure(closure)->clos_key_inits = new_cons;
    }
  }
 fehler_keyspec:
  pushSTACK(*(closure_ STACKop -1)); /* entire Lambda-List */
  pushSTACK(STACK_0);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(S(LLkey)); pushSTACK(S(function));
  fehler(source_program_error,
         GETTEXT("~S: incorrect variable specification after ~S: ~S"));
 allow: /* process &ALLOW-OTHER-KEYS: */
  TheIclosure(*closure_)->clos_allow_flag = T; /* set Flag to T */
  NEXT_ITEM(badLLkey,badLLkey,badLLkey,badLLkey,aux,ende);
  pushSTACK(*(closure_ STACKop -1)); /* entire Lambda-List */
  pushSTACK(STACK_0);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(S(LLaux)); pushSTACK(S(LLallow_other_keys));
  pushSTACK(S(function));
  fehler(source_program_error,
         GETTEXT("~S: ~S must be followed by ~S or end of list: ~S"));
 aux: /* process &AUX-Parameter, push on STACK and
         put Init-Forms in the Closure: */
  while(1) {
    NEXT_ITEM(badLLkey,badLLkey,badLLkey,badLLkey,badLLkey,ende);
    var object init_form;
    /* Parse variable-spezification in item:
         var  or  (var [init])
       push var on STACK.
       Returns also init (or NIL) in init_form. */
    check_STACK();
    if (atomp(item)) {
      item = check_symbol_non_constant(item,S(function));
      Car(*lalist_save_) = item;
      /* push variable on STACK: */
      pushSTACK(item); pushSTACK(Fixnum_0); aux_count++; var_count++;
      init_form = NIL; /* Default-Init */
    } else {
      var object item_rest = item; pushSTACK(item_rest);
      /* first list-element: var */
      item = check_symbol_non_constant(Car(item),S(function));
      item_rest = popSTACK(); Car(item_rest) = item; item_rest = Cdr(item_rest);
      /* push variable on STACK: */
      pushSTACK(item); pushSTACK(Fixnum_0); aux_count++; var_count++;
      if (consp(item_rest)) {
        init_form = Car(item_rest); /* second list-element: init */
        if (mconsp(Cdr(item_rest))) { /* varspec too long */
          pushSTACK(item_rest);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
          pushSTACK(*(closure_ STACKop -1)); /* entire Lambda-List */
          pushSTACK(S(LLaux)); pushSTACK(S(function));
          fehler(source_program_error,
                 GETTEXT("~S: variable specification after ~S too long : ~S"));
        }
      } else
        init_form = NIL; /* Default-Init */
    }
    /* push init_form in front of (clos_aux_inits closure) : */
    pushSTACK(init_form);
    {
      var object new_cons = allocate_cons();
      Car(new_cons) = popSTACK();
      var object closure = *closure_;
      Cdr(new_cons) = TheIclosure(closure)->clos_aux_inits;
      TheIclosure(closure)->clos_aux_inits = new_cons;
    }
  }
  /* Collected error messages: */
 badLLkey:
  pushSTACK(item);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(*(closure_ STACKop -1)); /* entire Lambda-List */
  pushSTACK(item); pushSTACK(S(function));
  fehler(source_program_error,
         GETTEXT("~S: badly placed lambda-list keyword ~S: ~S"));
 ende: /* reached list-end */
#undef NEXT_ITEM
  if (((uintL)~(uintL)0 > lp_limit_1) && (var_count > lp_limit_1)) {
    /* too many parameters? */
    pushSTACK(*(closure_ STACKop -1)); /* entire Lambda-List */
    pushSTACK(STACK_0);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
    pushSTACK(S(function));
    fehler(source_program_error,
           GETTEXT("~S: too many parameters in the lambda-list ~S"));
  }
  /* var_count <= lp_limit_1, therefore all counts fit in an uintC. */
  if (!nullp(*lalist_)) { /* is Lambda-List a Dotted List? */
    pushSTACK(*lalist_);  /* SOURCE-PROGRAM-ERROR slot DETAIL */
    pushSTACK(*(closure_ STACKop -1)); /* entire Lambda-List */
    pushSTACK(S(function));
    fehler(source_program_error,
           GETTEXT("~S: a dot in a lambda-list is allowed only for macros, not here: ~S"));
  }
  /* Collect variables into a vector and put it into the Closure,
     Collect variable-flags into a Byte-Vector and put it into the Closure: */
  pushSTACK(allocate_bit_vector(Atype_8Bit,var_count-spec_count)); /* create Byte-Vector */
  var object vars = allocate_vector(var_count); /* create Vector */
  var object varflags = popSTACK();
  { /* write variables in the Vector (last one to the back,
       leading ones in front): */
    var gcv_object_t* ptr = &TheSvector(vars)->data[var_count];
    var uintB* ptrflags = &TheSbvector(varflags)->data[var_count-spec_count];
    var uintC count = var_count-spec_count;
    while (count--) {
      *--ptrflags = (uintB)posfixnum_to_V(popSTACK());
      *--ptr = popSTACK();
    }
    for (count = spec_count; count--;)
      *--ptr = popSTACK();
  }
  var object closure = *closure_;
  TheIclosure(closure)->clos_vars     = vars;
  TheIclosure(closure)->clos_varflags = varflags;
  /* write counts in the Closure: */
  TheIclosure(closure)->clos_spec_anz = fixnum(spec_count);
  TheIclosure(closure)->clos_req_anz  = fixnum(req_count);
  TheIclosure(closure)->clos_opt_anz  = fixnum(opt_count);
  TheIclosure(closure)->clos_key_anz  = fixnum(key_count);
  TheIclosure(closure)->clos_aux_anz  = fixnum(aux_count);
  /* In the Variable-Vector the first spec_count variables are the
     SPECIAL-declared ones. In each remaining variable the DYNAM_BIT is
     set, if it occurs among the SPECIAL-declared one. */
  if (spec_count) { /* loop over the remaining variables: */
    if (var_count-spec_count > 0) {
      var gcv_object_t* othervarptr = &TheSvector(vars)->data[spec_count];
      var uintB* othervarflagsptr = &TheSbvector(varflags)->data[0];
      var uintC count1 = var_count-spec_count;
      do {
        var object othervar = *othervarptr++; /* next variable */
        { /* Search for it among the SPECIAL-declared variables: */
          var gcv_object_t* specvarptr = &TheSvector(vars)->data[0];
          var uintC count2 = spec_count;
          do {
            if (eq(*specvarptr++,othervar)) { /* found? */
              /* yes -> so the variable othervar is to be bound dynamically. */
              *othervarflagsptr |= bit(dynam_bit); break;
            }
          } while (--count2);
        }
        othervarflagsptr++;
      } while (--count1);
    }
  }
  /* Finally reverse the accumulated lists in the Closure: */
  nreverse(TheIclosure(closure)->clos_opt_inits);
  nreverse(TheIclosure(closure)->clos_keywords);
  nreverse(TheIclosure(closure)->clos_key_inits);
  nreverse(TheIclosure(closure)->clos_aux_inits);
  /* stack layout: closure, lambdalist, lalist-save, lalist-rest */
  skipSTACK(4);
  return closure;
}

# error, if symbol to be called is a special form.
# fehler_specialform(caller,funname);  (transl.: error_specialfor(...);)
# > caller: caller (a symbol)
# > funname: a symbol
nonreturning_function(local, fehler_specialform, (object caller, object funname)) {
  pushSTACK(funname); # CELL-ERROR slot NAME
  pushSTACK(funname);
  pushSTACK(caller);
  fehler(undefined_function,
         GETTEXT("~S: ~S is a special operator, not a function"));
}

# error, if symbol to be called is a macro.
# fehler_macro(caller,funname);
# > caller: caller (a symbol)
# > funname: a symbol
nonreturning_function(local, fehler_macro, (object caller, object funname)) {
  pushSTACK(funname); # CELL-ERROR slot NAME
  pushSTACK(funname);
  pushSTACK(caller);
  fehler(undefined_function,GETTEXT("~S: ~S is a macro, not a function"));
}

/* UP: Alters argument to a function.
 coerce_function(obj)
 > obj: object
 < result: object as function (SUBR or Closure)
 can trigger GC */
global maygc object coerce_function (object obj)
{
  /* obj should be a symbol, a SUBR or a Closure. */
  if (functionp(obj)) {
    return obj; /* function is OK */
  } else if (symbolp(obj)) {
    var object fdef = Symbol_function(obj);
    if (functionp(fdef))
      return fdef;
    else if (orecordp(fdef)) {
      switch (Record_type(fdef)) {
        case Rectype_Fsubr:
          fehler_specialform(TheSubr(subr_self)->name,obj);
        case Rectype_Macro:
          fehler_macro(TheSubr(subr_self)->name,obj);
        default: NOTREACHED;
      }
    } else
      return check_fdefinition(obj,TheSubr(subr_self)->name);
  } else if (funnamep(obj)) {
    /* this could have be done easier but we inline the checks -
       symbolp and functionp for performance reasons */
    var object symbol = get(Car(Cdr(obj)),S(setf_function)); /* (get ... 'SYS::SETF-FUNCTION) */
    if (!symbolp(symbol)) { /* should be symbol */
      pushSTACK(obj); symbol = check_symbol(symbol); obj = popSTACK();
    }
    var object fdef = Symbol_function(symbol);
    if (functionp(fdef))
      return fdef;
    else
      return check_fdefinition(obj,TheSubr(subr_self)->name);
  } else if (consp(obj) && eq(Car(obj),S(lambda))) { /* (LAMBDA . ...) ? */
    fehler_lambda_expression(TheSubr(subr_self)->name,obj);
  } else
    return check_function(obj);
}

#ifdef DEBUG_EVAL
/* Emit some trace output for a function call, to *funcall-trace-output*.
 trace_call(fun,type_of_call,caller_type);
 > object fun: function being called, a SUBR/FSUBR/Closure
 > uintB type_of_call: 'A' for apply, 'F' for funcall, 'B' for bytecode
 > uintB caller_type: 'F' for fsubr, 'S' for subr,
                      'C' for cclosure, 'I' for iclosure
 can trigger GC */
local maygc void trace_call (object fun, uintB type_of_call, uintB caller_type)
{
  var object stream = Symbol_value(S(funcall_trace_output)); /* SYS::*FUNCALL-TRACE-OUTPUT* */
  /* No output until *funcall-trace-output* has been initialized: */
  if (!streamp(stream))
    return;
  pushSTACK(stream);
  if (cclosurep(fun)) {
    pushSTACK(Closure_name(fun));
    write_ascii_char(&STACK_1,'c');
  } else if (closurep(fun)) {
    pushSTACK(TheIclosure(fun)->clos_name);
    write_ascii_char(&STACK_1,'C');
  } else if (subrp(fun)) {
    pushSTACK(TheSubr(fun)->name);
    write_ascii_char(&STACK_1,'S');
  } else if (fsubrp(fun)) {
    pushSTACK(TheFsubr(fun)->name);
    write_ascii_char(&STACK_1,'F');
  } else {
    pushSTACK(NIL);
    write_ascii_char(&STACK_1,'?');
  }
  write_ascii_char(&STACK_1,type_of_call); /* output type of call */
  write_ascii_char(&STACK_1,caller_type);  /* output caller */
  write_ascii_char(&STACK_1,'[');
  prin1(&STACK_1,STACK_0);    /* output function name */
  write_ascii_char(&STACK_1,']');
  terpri(&STACK_1);
  skipSTACK(2);
}
#define TRACE_CALL(fu,tc,ct)                                            \
  if (streamp(Symbol_value(S(funcall_trace_output)))) {                 \
    pushSTACK(fu); trace_call(fu,tc,ct); fu = popSTACK();               \
  }
#else
#define TRACE_CALL(fu,tc,ct)
#endif

# Test for illegal keywords
# check_for_illegal_keywords(allow_flag,fehler_statement);
# > uintC argcount: Number of Keyword/Value-pairs
# > gcv_object_t* rest_args_pointer: Pointer to the 2*argcount remaining arguments
# > bool allow_flag: Flag, if &ALLOW-OTHER-KEYS was specified
# > for_every_keyword: Macro, which loops over all Keywords and assigns
#                      them to 'keyword'.
# > fehler_statement: Statement, that reports, that bad_keyword is illegal.
#define check_for_illegal_keywords(allow_flag_expr,caller,fehler_statement)   \
    { var gcv_object_t* argptr = rest_args_pointer; # Pointer to the arguments \
      var object bad_keyword = nullobj; # first illegal keyword or nullobj  \
      var object bad_value = nullobj; /* its value */                       \
      var bool allow_flag = # Flag for allow-other-keys (if                 \
        # &ALLOW-OTHER-KEYS was specified or ':ALLOW-OTHER-KEY T' occurred) \
        (allow_flag_expr);                                                  \
      # But ':ALLOW-OTHER-KEYS NIL' hides a subsequent ':ALLOW-OTHER-KEYS T' \
      # (see CLHS 3.4.1.4.1.1).                                             \
      var bool allow_hidden = false; # true if seen ':ALLOW-OTHER-KEYS NIL' \
      var uintC check_count=argcount;                                       \
      while (check_count--) {                                               \
        var object kw = NEXT(argptr); # next Argument                       \
        var object val = NEXT(argptr); # and value for it                   \
        # must be a symbol, should be a keyword:                            \
        if (!symbolp(kw)) fehler_key_notkw(kw,caller);                      \
        if (!allow_flag) { # other keywords allowed? yes -> ok              \
          if (eq(kw,S(Kallow_other_keys))) {                                \
            if (!allow_hidden) {                                            \
              if (!nullp(val))                                              \
                allow_flag = true;                                          \
              else                                                          \
                allow_hidden = true;                                        \
            }                                                               \
          } else {                                                          \
            # up to now :ALLOW-OTHER-KEYS was not there, and NOALLOW        \
            if (eq(bad_keyword,nullobj)) { # all Keywords ok so far?        \
              # must test, if the keyword kw is allowed.                    \
              for_every_keyword({ if (eq(keyword,kw)) goto kw_ok; });       \
              # keyword kw was not allowed.                                 \
              bad_keyword = kw;                                             \
              bad_value = val;                                              \
              kw_ok: ;                                                      \
            }                                                               \
          }                                                                 \
        }                                                                   \
      };                                                                    \
      if (!allow_flag)                                                      \
        if (!eq(bad_keyword,nullobj)) {                                     \
          # wrong keyword occurred                                          \
          fehler_statement                                                  \
        }                                                                   \
    }

# For a Keyword 'keyword' search the pair Key.Value:
# find_keyword_value( notfound_statement, found_statement );
# > keyword: Keyword
# > uintC argcount: Number of Keyword/Value-Pairs
# > gcv_object_t* rest_args_pointer: Pointer to the 2*argcount remaining Arguments
# > notfound_statement: what is to be done, if not found
# > found_statement: what is to be done, if value found
  #define find_keyword_value(notfound_statement,found_statement)  \
    { var gcv_object_t* argptr = rest_args_pointer;                       \
      var uintC find_count;                                               \
      dotimesC(find_count,argcount, {                                     \
        if (eq(NEXT(argptr),keyword)) goto kw_found; # right keyword?     \
        NEXT(argptr);                                                     \
      });                                                                 \
      if (true) {                                                         \
        # not found                                                       \
        notfound_statement                                                \
      } else {                                                            \
       kw_found: # found                                                  \
       {var object value = NEXT(argptr);                                  \
        found_statement                                                   \
      }}                                                                  \
    }

/* UP: Applies an interpreted closure to arguments.
 funcall_iclosure(closure,args_pointer,argcount);
 > closure: Closure
 > args_pointer: Pointer to the arguments (in Stack)
 > argcount: Number of Arguments
 < mv_count/mv_space: values
 < STACK: cleaned up, = args_pointer
 can trigger GC */
local maygc Values funcall_iclosure (object closure, gcv_object_t* args_pointer,
                                     uintC argcount)
{
  /* 1st step: finish building of APPLY-frame: */
  var sp_jmp_buf my_jmp_buf;
  TRACE_CALL(closure,'F','I');
  {
    var gcv_object_t* top_of_frame = args_pointer; /* Pointer to frame */
    pushSTACK(closure);
    finish_entry_frame(APPLY,my_jmp_buf,,{
      if (mv_count==0) { /* after reentry: pass form? */
        closure = STACK_(frame_closure); /* try the same APPLY again */
        args_pointer = topofframe(STACK_0);
        argcount = STACK_item_count(STACK STACKop frame_args,args_pointer);
      } else {
        setSTACK(STACK = topofframe(STACK_0)); /* clean STACK ?or unwind()?*/
        eval_noenv(value1); return; /* evaluate passed form */
      }
    });
  }
  var gcv_object_t* closure_ = &STACK_(frame_closure); /* &closure */
  var gcv_object_t* frame_pointer; /* pointer to the frame */
  var uintC spec_count = posfixnum_to_V(TheIclosure(closure)->clos_spec_anz);
  var gcv_object_t *spec_ptr;
  { /* 2nd step: build variable-binding-frame: */
    var gcv_object_t* top_of_frame = STACK; /* Pointer to Frame */
    var object vars = TheIclosure(closure)->clos_vars; /* Vector of variable-names */
    var uintL var_count = Svector_length(vars); /* number of variables */
    get_space_on_STACK(var_count*varframe_binding_size*sizeof(gcv_object_t));
    {
      var gcv_object_t* varptr = &TheSvector(vars)->data[0]; /* Pointer to variables in vector */
      var uintC count;
      /* the special-references first: */
      spec_ptr = args_end_pointer;
      dotimesC(count,spec_count, {
        pushSTACK(specdecl); /* SPECDECL as "value" */
        pushSTACK_symbolwithflags(*varptr++,0); /* INactive */
      });
      frame_pointer = args_end_pointer;
      if (var_count-spec_count > 0) {
        var uintB* varflagsptr = &TheSbvector(TheIclosure(closure)->clos_varflags)->data[0];
        dotimespC(count,var_count-spec_count, {
          pushSTACK(NIL); /* NIL as preliminary value */
          var object next_var = *varptr++; /* next variable */
          var oint next_varflags = (oint)(*varflagsptr++)<<oint_symbolflags_shift; /* with poss. dynam_bit, svar_bit */
          if (special_var_p(TheSymbol(next_var))) /* proclaimed SPECIAL? */
            next_varflags |= wbit(dynam_bit_o); /* -> bind dynamically */
          pushSTACK_symbolwithflags(next_var,next_varflags);
        });
      }
    }
    /* VAR_ENV of closure becomes NEXT_ENV in frame: */
    pushSTACK(TheIclosure(closure)->clos_var_env);
    pushSTACK(fake_gcv_object(var_count)); /* var_count bindungs, all still un-nested */
    finish_frame(VAR);
  }
  /* STACK now points below the variable-binding-frame.
     frame_pointer = Pointer in the variable-binding-frame, above the first
     still inactive binding, below the already active SPECIAL-references. */
  { /* 3rd step: bind current environments: */
    var object new_var_env = make_framepointer(STACK);
    /* this frame will become the new VAR_ENV later. */
    make_ENV5_frame();
    /* activate the closure-environment: */
    aktenv.var_env   = new_var_env; /* variable-binding-frame */
    aktenv.fun_env   = TheIclosure(closure)->clos_fun_env;
    aktenv.block_env = TheIclosure(closure)->clos_block_env;
    aktenv.go_env    = TheIclosure(closure)->clos_go_env;
    aktenv.decl_env  = TheIclosure(closure)->clos_decl_env;
  }
  /* stack layout: APPLY-frame, variable-binding-frame, ENV-frame */
  { /* 4th step: process parameters: */
    check_SP();
    /* Macro for binding of variables in variable-frame:
       binds the next variable to value, decreases frame_pointer by 2 resp. 3.
       (takes advantage of varframe_binding_mark = 0 !) */
   #define bind_next_var(value,markptr_zuweisung)                       \
    { frame_pointer skipSTACKop -varframe_binding_size;                 \
     {var gcv_object_t* markptr = markptr_zuweisung &Before(frame_pointer); \
      if (as_oint(*markptr) & wbit(dynam_bit_o)) {                      \
        /* activate dynamic Binding: */                                 \
        var object sym = *(markptr STACKop varframe_binding_sym); /* var */ \
        *(markptr STACKop varframe_binding_value) = /* old value in frame */ \
          TheSymbolflagged(sym)->symvalue;                              \
        /* new value in value-cell: */                                  \
        TheSymbolflagged(sym)->symvalue = (value);                      \
        activate_specdecl(sym,spec_ptr,spec_count);                     \
      } else { /* activate static binding: */                           \
        /* new value in frame: */                                       \
        *(markptr STACKop varframe_binding_value) = (value);            \
      }                                                                 \
      *markptr = SET_BIT(*markptr,active_bit_o);/* activate binding */  \
     }}
    { /* process required parameters: fetch next argument and bind in stack */
      var uintC count = posfixnum_to_V(TheIclosure(closure)->clos_req_anz);
      if (count>0) {
        if (argcount < count) {
          pushSTACK(TheIclosure(closure)->clos_name);
          /* ANSI CL 3.5.1.2. wants a PROGRAM-ERROR here. */
          fehler(program_error,
                 GETTEXT("EVAL/APPLY: too few arguments given to ~S"));
        }
        argcount -= count;
        dotimespC(count,count, {
          var object next_arg = NEXT(args_pointer); /* next argument */
          bind_next_var(next_arg,); /* bind next variable */
        });
      }
    }
    { /* process optional parameters:
         fetch next argument; if there is none,
         execute an Init-form; then bind in stack. */
      var uintC count = posfixnum_to_V(TheIclosure(closure)->clos_opt_anz);
      if (count==0)
        goto optional_ende;
      {
        var object inits = TheIclosure(closure)->clos_opt_inits; /*init forms*/
        do {
          if (argcount==0)
            goto optional_aus;
          argcount--;
          var object next_arg = NEXT(args_pointer); /* next argument */
          var gcv_object_t* optmarkptr;
          bind_next_var(next_arg,optmarkptr=); /* bind next variable */
          if (as_oint(*optmarkptr) & wbit(svar_bit_o)) {
            /* supplied-p-Parameter follows? */
            *optmarkptr = CLR_BIT(*optmarkptr,svar_bit_o);
            bind_next_var(T,); /* yes -> bind to T */
          }
          inits = Cdr(inits); /* shorten Init-Forms-List */
          count--;
        } while (count);
        goto optional_ende;
       optional_aus: /* no more optional arguments here. */
        pushSTACK(inits);
      }
      /* execute all Init-forms of the optional parameters here: */
      dotimespC(count,count, {
        var object inits = STACK_0; /* remaining Initforms */
        STACK_0 = Cdr(inits);
        inits = (eval(Car(inits)),value1); /* next Initform, evaluated */
        var gcv_object_t* optmarkptr;
        bind_next_var(inits,optmarkptr=); /* bind next variable */
        if (as_oint(*optmarkptr) & wbit(svar_bit_o)) {
          /* supplied-p-Parameter follows? */
          *optmarkptr = CLR_BIT(*optmarkptr,svar_bit_o);
          bind_next_var(NIL,); /* yes -> bind to NIL */
        }
      });
      closure = *closure_;
      /* initialize &REST-parameters without arguments: */
      if (!nullp(TheIclosure(closure)->clos_rest_flag)) /* Rest-Flag? */
        bind_next_var(NIL,); /* yes -> bind to NIL */
      /* initialize &KEY-parameters without arguments : */
      count = posfixnum_to_V(TheIclosure(closure)->clos_key_anz); /* number of Keyword-parameters */
      if (count>0) {
        STACK_0 = TheIclosure(closure)->clos_key_inits; /* their Init-forms */
        dotimespC(count,count, {
          var object inits = STACK_0; /* remaining Initforms */
          STACK_0 = Cdr(inits);
          inits = (eval(Car(inits)),value1); /* next Initform, evaluated */
          var gcv_object_t* keymarkptr;
          bind_next_var(inits,keymarkptr=); /* bind next Variable */
          if (as_oint(*keymarkptr) & wbit(svar_bit_o)) {
            /* supplied-p-Parameter follows? */
            *keymarkptr = CLR_BIT(*keymarkptr,svar_bit_o);
            bind_next_var(NIL,); /* yes -> bind to NIL */
          }
        });
        closure = *closure_;
      }
      skipSTACK(1); /* remaining Init-forms forgotten */
      goto aux; /* go to the AUX-variables */
    }
   optional_ende:
    /* prepare &KEY-parameters and &REST-parameters: */
    if (numberp(TheIclosure(closure)->clos_keywords) /* is keyword a number? */
        && nullp(TheIclosure(closure)->clos_rest_flag)) { /* and no Rest-parameter? */
      /* yes -> neither &KEY nor &REST specified */
      if (argcount>0) { /* still arguments there? -> Error */
        pushSTACK(TheIclosure(closure)->clos_name);
        /* ANSI CL 3.5.1.3. wants a PROGRAM-ERROR here. */
        fehler(program_error,
               GETTEXT("EVAL/APPLY: too many arguments given to ~S"));
      }
    } else { /* &KEY or &REST present. */
      /* process &REST-parameters: */
      if (!nullp(TheIclosure(closure)->clos_rest_flag)) { /* &rest? */
        /* yes -> collect residual arguments in a list: */
        pushSTACK(NIL); /* start of list */
        if (argcount>0) {
          var gcv_object_t* ptr = args_pointer STACKop -(uintP)argcount;
          var uintC count;
          dotimespC(count,argcount, {
            var object new_cons = allocate_cons();
            Car(new_cons) = BEFORE(ptr);
            Cdr(new_cons) = STACK_0;
            STACK_0 = new_cons;
          });
          closure = *closure_;
        }
        var object list = popSTACK(); /* entire list */
        bind_next_var(list,); /* bind &REST-parameter to this list */
      }
      /* process &KEY-parameters: */
      if (!numberp(TheIclosure(closure)->clos_keywords)) {
        /* Keyword-parameters present */
        var gcv_object_t* rest_args_pointer = args_pointer;
        /* argcount = number of remaining arguments */
        /* halve argcount --> number of pairs Key.Value: */
        if (argcount%2) { /* number was odd -> not paired: */
          var uintC count = 0;
          while (count<argcount) pushSTACK(rest_args_pointer[count++]);
          fehler_key_odd(argcount,TheIclosure(closure)->clos_name);
        }
        argcount = argcount/2;
        { /* test for illegal keywords: */
          var object keywords = TheIclosure(closure)->clos_keywords;
         #define for_every_keyword(statement)           \
            { var object keywordsr = keywords;          \
              while (consp(keywordsr)) {                \
                var object keyword = Car(keywordsr);    \
                statement;                              \
                keywordsr = Cdr(keywordsr);             \
              }}
          check_for_illegal_keywords
            (!nullp(TheIclosure(closure)->clos_allow_flag),
             TheIclosure(closure)->clos_name,
             { fehler_key_badkw(TheIclosure(closure)->clos_name,
                                bad_keyword,bad_value,
                                TheIclosure(closure)->clos_keywords);});
         #undef for_every_keyword
          /* Now assign the Key-values and evaluate the Key-Inits: */
          var uintC count = posfixnum_to_V(TheIclosure(closure)->clos_key_anz);
          if (count > 0) {
            var object key_inits = TheIclosure(closure)->clos_key_inits;
            dotimespC(count,count, {
              var object keyword = Car(keywords); /* Keyword */
              var object var_value;
              var object svar_value;
              /* Find the pair Key.Value for Keyword: */
              find_keyword_value({ /* not found, must evaluate the Init: */
                pushSTACK(keywords); pushSTACK(key_inits);
                var_value = (eval(Car(key_inits)),value1);
                key_inits = popSTACK(); keywords = popSTACK();
                svar_value = NIL; /* NIL for poss. supplied-p-Parameter */
              },{ /* found -> take value: */
                var_value = value;
                svar_value = T; /* T for poss. supplied-p-Parameter */
              });
              {
                var gcv_object_t* keymarkptr;
                bind_next_var(var_value,keymarkptr=); /* bind keyword-var */
                if (as_oint(*keymarkptr) & wbit(svar_bit_o)) { /* supplied-p-Parameter follows? */
                  *keymarkptr = CLR_BIT(*keymarkptr,svar_bit_o);
                  bind_next_var(svar_value,); /* yes -> bind to NIL resp. T */
                }
              }
              keywords = Cdr(keywords);
              key_inits = Cdr(key_inits);
            });
          }
        }
        closure = *closure_;
      }
    }
   aux: { /* process &AUX-parameter: */
      var uintC count = posfixnum_to_V(TheIclosure(closure)->clos_aux_anz);
      if (count>0) {
        pushSTACK(TheIclosure(closure)->clos_aux_inits); /* Init-forms for &AUX-variables */
        dotimespC(count,count, {
          var object inits = STACK_0;
          STACK_0 = Cdr(inits);
          inits = (eval(Car(inits)),value1); /* evaluate nnext Init */
          bind_next_var(inits,); /* and bind next variable to it */
        });
        skipSTACK(1); /* forget remaining Init-forms */
        closure = *closure_;
      }
    }
   #undef bind_next_var
  }
  if (spec_count > 0) activate_specdecls(spec_ptr,spec_count);
  /* 5th step: evaluate Body: */
  implicit_progn(TheIclosure(closure)->clos_body,NIL);
  unwind(); /* unwind ENV-frame */
  unwind(); /* unwind variable-binding-frame */
  unwind(); /* unwind APPLY-frame */
}

# UP: provides the assignment of the Key-arguments for SUBRs.
# call only, if key_flag /= subr_nokey.
# > fun: function, a SUBR
# > argcount: number of arguments after optional ones
# > STACK_(argcount-1),...,STACK_0: the argcount arguments after the optional ones
# > key_args_pointer: Pointer to the Key-parameters in the STACK
# > rest_args_pointer: Pointer to the remaining arguments in the STACK
# < STACK: set correctly
# changes STACK
  local void match_subr_key (object fun, uintL argcount, gcv_object_t* key_args_pointer, gcv_object_t* rest_args_pointer)
   {
    /* halve argcount --> the number of pairs Key.Value: */
    if (argcount%2) /* number was odd -> not paired: */
      fehler_key_odd(argcount,TheSubr(fun)->name);
    if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
      fehler_too_many_args(unbound,fun,argcount,ca_limit_1);
    # Due to argcount <= ca_limit_1, all count's fit in a uintC.
    argcount = argcount/2;
    # test for illegal Keywords:
    {
      var gcv_object_t* keywords_pointer = &TheSvector(TheSubr(fun)->keywords)->data[0];
      var uintC key_anz = TheSubr(fun)->key_anz;
      #define for_every_keyword(statement)                    \
        if (key_anz > 0) {                                    \
          var gcv_object_t* keywordptr = keywords_pointer;    \
          var uintC count = key_anz;                          \
          do {                                                \
            var object keyword = *keywordptr++;               \
            statement;                                        \
          } while (--count);                                  \
        }
      check_for_illegal_keywords
        (TheSubr(fun)->key_flag == subr_key_allow,TheSubr(fun)->name,
         { pushSTACK(bad_keyword); /* save bad keyword */
           pushSTACK(bad_value);  /* save bad value */
           pushSTACK(fun); /* save the function */
           # convert Keyword-Vector to a List:
           # (SYS::COERCE-SEQUENCE kwvec 'LIST)
           coerce_sequence(TheSubr(fun)->keywords,S(list),true);
           fun = popSTACK(); bad_value = popSTACK();
           bad_keyword = popSTACK();
           fehler_key_badkw(TheSubr(fun)->name,bad_keyword,
                            bad_value,value1);
         });
      #undef for_every_keyword
    # now assign Arguments and Parameters:
      if (key_anz > 0) {
        var gcv_object_t* keywordptr = keywords_pointer;
        var gcv_object_t* key_args_ptr = key_args_pointer;
        var uintC count;
        dotimespC(count,key_anz, {
          var object keyword = *keywordptr++; # Keyword
          # find the pair Key.Value for this Keyword:
          find_keyword_value(
            # not found -> value remains #<UNBOUND> :
            { NEXT(key_args_ptr); },
            # found -> save value:
            { NEXT(key_args_ptr) = value; }
            );
        });
      }
    }
    # poss. process Rest-Parameters:
    if (TheSubr(fun)->rest_flag == subr_norest) {
      # SUBR without &REST-Flag: forget remaining Arguments:
      set_args_end_pointer(rest_args_pointer);
    }
    # SUBR with &REST-Flag: leave remaining Arguments in Stack
  }

# UP: provides the assignment between Argument-list and Keyword-parameters
# and poss. Rest-parameters of a compiled Closure.
# > closure: compiled Closure with &KEY-parameters
# > argcount: number of arguments after optional ones
# > STACK_(argcount-1),...,STACK_0: the argcount arguments after the optional ones
# > key_args_pointer: Pointer to the Key-parameters in the STACK
#                     (poss. also Pointer beneath the Rest-parameters in the STACK,
#                      which is #<UNBOUND>, if it is still to be supplied with)
# > rest_args_pointer: Pointer to the remaining Arguments in the STACK
# < STACK: set correctly
# < ergebnis: closure
# changes STACK
# can trigger GC
  local maygc object match_cclosure_key (object closure, uintL argcount, gcv_object_t* key_args_pointer, gcv_object_t* rest_args_pointer)
  {
    /* halve argcount --> the number of pairs Key.Value: */
    if (argcount%2) /* number was odd -> not paired: */
      fehler_key_odd(argcount,Closure_name(closure));
    if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
      fehler_too_many_args(unbound,closure,argcount,ca_limit_1);
    # Due to argcount <= ca_limit_1, all count's fit in a uintC.
    argcount = argcount/2;
    var object codevec = TheCclosure(closure)->clos_codevec; # Code-Vector
    {
      var uintC key_anz = TheCodevec(codevec)->ccv_numkey; # number of Keywords
      var uintL keywords_offset = TheCodevec(codevec)->ccv_keyconsts; # Offset of Keywords in FUNC
      var gcv_object_t* keywords_pointer = # points to the first Keyword
        (TheCodevec(codevec)->ccv_flags & bit(4) # generic function?
         ? &TheSvector(TheCclosure(closure)->clos_consts[0])->data[keywords_offset]
         : &TheCclosure(closure)->clos_consts[keywords_offset]
        );
    # test for illegal Keywords:
      #define for_every_keyword(statement)                    \
        if (key_anz > 0) {                                    \
          var gcv_object_t* keywordptr = keywords_pointer;    \
          var uintC count = key_anz;                          \
          do {                                                \
            var object keyword = *keywordptr++;               \
            statement;                                        \
          } while (--count);                                  \
        }
      check_for_illegal_keywords
        (!((TheCodevec(codevec)->ccv_flags & bit(6)) == 0),
         Closure_name(closure),
         { pushSTACK(bad_keyword); /* save */
           pushSTACK(bad_value);  /* save */
           pushSTACK(closure); /* save the closure */
           /* build list of legal Keywords: */
           for_every_keyword( { pushSTACK(keyword); } );
          {var object kwlist = listof(key_anz);
           closure = popSTACK(); bad_value = popSTACK();
           bad_keyword = popSTACK(); /* report errors: */
           fehler_key_badkw(Closure_name(closure),
                            bad_keyword,bad_value,kwlist);}});
      #undef for_every_keyword
    # now assign Arguments and Parameters:
      if (key_anz > 0) {
        var gcv_object_t* keywordptr = keywords_pointer;
        var gcv_object_t* key_args_ptr = key_args_pointer;
        var uintC count;
        dotimespC(count,key_anz, {
          var object keyword = *keywordptr++; # Keyword
          # find the pair Key.value for this keyword:
          find_keyword_value(
            # not found -> Wert remains #<UNBOUND> :
            { NEXT(key_args_ptr); },
            # found -> save value:
            { NEXT(key_args_ptr) = value; }
            );
        });
      }
    }
    # poss. process Rest-parameters:
    if (TheCodevec(codevec)->ccv_flags & bit(0)) { # Rest-Flag?
      # Closure with Keywords and &REST-Flag:
      var gcv_object_t* rest_arg_ = &BEFORE(key_args_pointer); # Pointer to the REST-Parameter
      if (!boundp(*rest_arg_)) {
        # must still be filed: handicraft list
        *rest_arg_ = closure; # save Closure
        var object rest_arg = NIL;
        until (args_end_pointer == rest_args_pointer) {
          pushSTACK(rest_arg);
          rest_arg = allocate_cons();
          Cdr(rest_arg) = popSTACK();
          Car(rest_arg) = popSTACK();
        }
        closure = *rest_arg_; # return Closure
        *rest_arg_ = rest_arg;
      } else {
        # forget remaining arguments:
        set_args_end_pointer(rest_args_pointer);
      }
    } else {
      # Closure without &REST-Flag: forget remaining arguments:
      set_args_end_pointer(rest_args_pointer);
    }
    return closure;
  }


#           ----------------------- E V A L -----------------------

# later:
local Values eval1 (object form);
local Values eval_fsubr (object fun, object args);
local Values eval_applyhook (object fun);
local Values eval_subr (object fun);
local Values eval_closure (object fun);
#ifdef DYNAMIC_FFI
local Values eval_ffunction (object fun);
#endif

# UP: evaluates a form in the current environment.
# eval(form);
# > form: form
# < mv_count/mv_space: values
# can trigger GC
  global maygc Values eval (object form)
  {
   start:
    # Test for Keyboard-Interrupt:
    interruptp({
      pushSTACK(form); # save form
      pushSTACK(S(eval)); tast_break(); # call break-loop
      form = popSTACK();
      goto start;
    });
    var sp_jmp_buf my_jmp_buf;
    # build EVAL-frame:
    {
      var gcv_object_t* top_of_frame = STACK; # Pointer to Frame
      pushSTACK(form); # Form
      finish_entry_frame(EVAL,my_jmp_buf,,
        {
          if (mv_count==0) { # after reentry: Form passed over?
            form = STACK_(frame_form); # evaluate the same form again
          } else {
            form = STACK_(frame_form) = value1; # evaluate form passed over
          }
        });
    }
    # Test for *EVALHOOK*:
    {
      var object evalhook_value = Symbol_value(S(evalhookstern)); # *EVALHOOK*
      if (nullp(evalhook_value)) { # *EVALHOOK* = NIL ?
        # yes -> continue evaluation normally
        pushSTACK(Symbol_value(S(applyhookstern))); eval1(form);
      } else {
        # bind *EVALHOOK*, *APPLYHOOK* to NIL:
        bindhooks_NIL();
        # execute (FUNCALL *EVALHOOK* form env) :
        pushSTACK(form); # Form as 1. Argument
        pushSTACK(evalhook_value); # save Function
        var gcv_environment_t* stack_env = nest_aktenv(); # Environments in the Stack,
        var object env = allocate_vector(5); # in newly allocated Vector
        *(gcv_environment_t*)(&TheSvector(env)->data[0]) = *stack_env; # push in
        skipSTACK(5);
        evalhook_value = popSTACK(); # return Function
        pushSTACK(env); # entire Environment as 2. Argument
        funcall(evalhook_value,2);
        # restore old values of *EVALHOOK*, *APPLYHOOK* :
        unwind();
        # unwind EVAL-Frame:
        unwind();
      }
    }
  }

# UP: evaluates a form in the current Environment. Does not take
# *EVALHOOK* and *APPLYHOOK* into consideration.
# eval_no_hooks(form);
# > form: Form
# < mv_count/mv_space: values
# can trigger GC
global maygc Values eval_no_hooks (object form) {
  var sp_jmp_buf my_jmp_buf;
  # build EVAL-Frame:
  {
    var gcv_object_t* top_of_frame = STACK; # Pointer to Frame
    pushSTACK(form); # Form
    finish_entry_frame(EVAL,my_jmp_buf,,
    {
      if (mv_count==0) { # after reentry: Form passed over?
        form = STACK_(frame_form); # evaluate the same form again
      } else {
        form = STACK_(frame_form) = value1; # evaluate form passed over
      }
    });
  }
  # continue evaluation, consider *APPLYHOOK* as being NIL:
  pushSTACK(NIL); eval1(form);
}

/* UP: evaluates a form in the current environment.
 Does not take the value of *EVALHOOK* into consideration and expects the value of
 *APPLYHOOK*.
 the EVAL-frame must already have been built; it will then be unwound.
 eval1(form);
 > form: form
 > STACK_3..STACK_1: EVAL-Frame, with form in STACK_3
 > STACK_0: value of *APPLYHOOK*
 < mv_count/mv_space: values
 changes STACK
 can trigger GC */
local maygc Values eval1 (object form)
{
  if (atomp(form)) {
    if (symbolp(form)) { /* Form is a Symbol */
      /* value1 = value in the current Environment - not unbound! */
      var object symbolmacro;
      value1 = sym_value(form,aktenv.var_env,&symbolmacro);
      if (!eq(symbolmacro,nullobj)) { /* Symbol-Macro? */
        /* yes -> expand and evaluate again: */
        skipSTACK(1); /* forget value of *APPLYHOOK* */
        check_SP(); check_STACK();
        eval(TheSymbolmacro(symbolmacro)->symbolmacro_expansion); /* evaluate Expansion */
        unwind(); /* unwind EVAL-Frame */
      } else {
        if (!boundp(value1)) {
          do {
            pushSTACK(form); /* PLACE */
            pushSTACK(form); /* CELL-ERROR slot NAME */
            pushSTACK(form);
            check_value(unbound_variable,GETTEXT("EVAL: variable ~S has no value"));
            form = STACK_(frame_form+1);
          } while (!boundp(value1));
          if (!nullp(value2)) /* STORE-VALUE */
            value1 = setq(form,value1);
        }
        mv_count=1; /* value1 as value */
        skipSTACK(1);
        unwind(); /* unwind EVAL-Frame */
      }
    } else {
      /* self-evaluating form */
      VALUES1(form);
      skipSTACK(1);
      unwind(); /* unwind EVAL-Frame */
    }
  } else { /* Form is a Cons */
   eval_cons:
    /* determine, if Macro-call, poss. expand: */
    macroexp(form,aktenv.var_env,aktenv.fun_env); form = value1;
    if (!nullp(value2)) { /* expanded ? */
      /* now really evaluate: */
      skipSTACK(1); /* forget value of *APPLYHOOK* */
      check_SP(); check_STACK();
      eval(form); /* evaluate expanded form */
      unwind(); /* unwind EVAL-Frame */
    } else {
      var object fun = Car(form); /* function designation */
      if (funnamep(fun)) {
        /* fetch function-definition in the environment: */
        fun = sym_function(fun,aktenv.fun_env);
       fun_dispatch:
        /* branch according to type of function:
           unbound / SUBR/FSUBR/Closure / FunctionMacro / Macro */
       #ifdef TYPECODES
        switch (typecode(fun))
       #else
        if (immsubrp(fun))
          goto case_subr;
        else if (orecordp(fun))
          goto case_orecord;
        else
          switch (0)
       #endif
        {
         case_subr: /* SUBR */
          pushSTACK(Cdr(form)); /* argument list */
          if (!nullp(STACK_1))
            goto applyhook;
          eval_subr(fun);
          break;
         case_closure: /* closure */
          pushSTACK(Cdr(form)); /* argument list */
         closure: /* fun is a closure */
          if (!nullp(STACK_1))
            goto applyhook;
          eval_closure(fun);
          break;
         applyhook: /* value of *APPLYHOOK* is /= NIL. */
          eval_applyhook(fun);
          break;
         case_orecord:
          switch (Record_type(fun)) {
            case_Rectype_Closure_above;
            case_Rectype_Subr_above;
            case Rectype_Fsubr: /* Fsubr */
              eval_fsubr(fun,Cdr(form));
              break;
           #ifdef DYNAMIC_FFI
            case Rectype_Ffunction: /* Foreign-Function */
              pushSTACK(Cdr(form)); /* argument list */
              if (!nullp(STACK_1))
                goto applyhook;
              eval_ffunction(fun);
              break;
           #endif
            case Rectype_FunctionMacro:
              /* FunctionMacro -> treat like a function */
              fun = TheFunctionMacro(fun)->functionmacro_function;
              goto fun_dispatch;
            default:
              goto undef;
          }
          break;
          default: undef: {
            pushSTACK(form);
            fun = check_fdefinition(Car(form),S(eval));
            form = popSTACK();
            goto fun_dispatch;
          }
        }
      } else if (consp(fun) && eq(Car(fun),S(lambda))) {
        /* lambda-expression? */
        pushSTACK(Cdr(form)); /* Argument list */
        fun = get_closure(Cdr(fun),S(Klambda),false,&aktenv); /* create closure in current environment */
        goto closure; /* und apply it to the arguments, as above */
      } else {
        pushSTACK(Cdr(form));
        fun = check_funname_replacement(source_program_error,S(eval),fun);
        pushSTACK(fun);
        form = allocate_cons();
        Car(form) = popSTACK(); /* fun */
        Cdr(form) = popSTACK(); /* Cdr(form) */
        goto eval_cons;
      }
    }
  }
}

# In EVAL: Applies a FSUBR to an argument-list, cleans up STACK
# and returns the values.
# eval_fsubr(fun,args);
# > fun: a FSUBR
# > args: argument-list
# > STACK-layout: EVAL-Frame, *APPLYHOOK*.
# < STACK: cleaned up
# < mv_count/mv_space: values
# changes STACK
# can trigger GC
  local maygc Values eval_fsubr (object fun, object args)
  {
    skipSTACK(1); # forget value of *APPLYHOOK*
    check_SP(); check_STACK();
    #if STACKCHECKS
    var gcv_object_t* STACKbefore = STACK;
    #endif
    # put arguments in the STACK:
    switch ((uintW)posfixnum_to_V(TheFsubr(fun)->argtype)) {
      # Macro for 1 required-Parameter:
      #define REQ_PAR()  \
        { if (atomp(args)) goto fehler_zuwenig;                   \
          pushSTACK(Car(args)); # next parameter in the STACK \
          args = Cdr(args);                                       \
        }
      case (uintW)fsubr_argtype_2_0_nobody:
        # FSUBR with 2 required-Parameters
        REQ_PAR();
      case (uintW)fsubr_argtype_1_0_nobody:
        # FSUBR with 1 required-Parameter
        REQ_PAR();
        if (!nullp(args)) goto fehler_zuviel;
        break;
      case (uintW)fsubr_argtype_2_1_nobody:
        # FSUBR with 2 required-Parameters and 1 optional-Parameter
        REQ_PAR();
      case (uintW)fsubr_argtype_1_1_nobody:
        # FSUBR with 1 required-Parameter and 1 optional-Parameter
        REQ_PAR();
        if (consp(args)) {
          pushSTACK(Car(args)); # optional parameter into STACK
          args = Cdr(args);
          if (!nullp(args)) goto fehler_zuviel;
        } else {
          pushSTACK(unbound); # unbound into STACK instead
          if (!nullp(args)) goto fehler_dotted;
        }
        break;
      case (uintW)fsubr_argtype_2_body:
        # FSUBR with 2 required-Parameters and Body-Parameter
        REQ_PAR();
      case (uintW)fsubr_argtype_1_body:
        # FSUBR with 1 required-Parameter and Body-Parameter
        REQ_PAR();
      case (uintW)fsubr_argtype_0_body:
        # FSUBR with 0 required-Parameters and Body-Parameter
        pushSTACK(args); # remaining body into STACK
        break;
      default: NOTREACHED;
      fehler_zuwenig: # argument-list args is an atom, prematurely
        if (!nullp(args)) goto fehler_dotted;
        # clean up STACK up to the calling EVAL-Frame:
        until (framecode(STACK_0) & bit(frame_bit_t)) {
          skipSTACK(1);
        }
        {
          var object form = STACK_(frame_form); # Form from EVAL-Frame
          pushSTACK(form); /* SOURCE-PROGRAM-ERROR slot DETAIL */
          pushSTACK(form); pushSTACK(Car(form));
          fehler(source_program_error,
                 GETTEXT("EVAL: too few parameters for special operator ~S: ~S"));
        }
      fehler_zuviel: # argument-list args is not NIL at the tail
        if (atomp(args)) goto fehler_dotted;
        # clean up STACK up to the calling EVAL-Frame:
        until (framecode(STACK_0) & bit(frame_bit_t)) {
          skipSTACK(1);
        }
        {
          var object form = STACK_(frame_form); # Form from EVAL-Frame
          pushSTACK(form); /* SOURCE-PROGRAM-ERROR slot DETAIL */
          pushSTACK(form); pushSTACK(Car(form));
          fehler(source_program_error,
                 GETTEXT("EVAL: too many parameters for special operator ~S: ~S"));
        }
      fehler_dotted: # argument-list args ends with Atom /= NIL
        # clean up STACK up to the calling EVAL-Frame:
        until (framecode(STACK_0) & bit(frame_bit_t)) {
          skipSTACK(1);
        }
        {
          var object form = STACK_(frame_form); # Form from EVAL-Frame
          pushSTACK(form); /* SOURCE-PROGRAM-ERROR slot DETAIL */
          pushSTACK(form); pushSTACK(Car(form));
          fehler(source_program_error,
                 GETTEXT("EVAL: dotted parameter list for special operator ~S: ~S"));
        }
      #undef REQ_PAR
    }
    # Now STACK = STACKbefore STACKop - (req + opt + (body-flag ? 1 : 0)).
    # Call FSUBR:
    with_saved_back_trace_fsubr(fun,
      (*(fsubr_function_t*)(TheFsubr(fun)->function))(); );
    #if STACKCHECKS
     if (!(STACK == STACKbefore)) # STACK as before?
       abort(); # no -> go to Debugger
    #endif
    unwind(); # unwind EVAL-Frame
  }

# In EVAL: Applies *APPLYHOOK* to a function (SUBR or Closure) and
# an argument-list, cleans up the STACK and returns the values.
# eval_applyhook(fun);
# > fun: function, a SUBR or a closure
# > STACK-layout: EVAL-Frame, *APPLYHOOK* (/= NIL), argument-list.
# < STACK: cleaned up
# < mv_count/mv_space: values
# changes STACK
# can trigger GC
local maygc Values eval_applyhook(object fun) {
  var object args = popSTACK(); # argument-list
  var object applyhook_value = popSTACK(); # value of *APPLYHOOK*
  check_SP();
  # bind *EVALHOOK*, *APPLYHOOK* to NIL:
  bindhooks_NIL();
  #ifndef X3J13_005
  # execute (FUNCALL *APPLYHOOK* fun args env) :
  pushSTACK(fun); # Funktion as 1. Argument
  pushSTACK(args); # argument-list as 2. Argument
  pushSTACK(applyhook_value); # save function
  {
    var gcv_environment_t* stack_env = nest_aktenv(); # Environments into Stack,
    var object env = allocate_vector(5); # in newly allocated Vector
    *(gcv_environment_t*)(&TheSvector(env)->data[0]) = *stack_env; # push in
    skipSTACK(5);
  }
  applyhook_value = popSTACK(); # function back
  pushSTACK(env); # entire Environment as 3. Argument
  funcall(applyhook_value,3);
  #else
  # execute (FUNCALL *APPLYHOOK* fun args) :
  pushSTACK(fun); # function as 1. Argument
  pushSTACK(args); # argument-list as 2. Argument
  funcall(applyhook_value,2);
  #endif
  # old values of *EVALHOOK*, *APPLYHOOK* back:
  unwind();
  # unwind EVAL-Frame:
  unwind();
}

# In EVAL: error, if too few arguments
nonreturning_function(local, fehler_eval_zuwenig, (object fun)) {
  var object form = STACK_(frame_form); # Form
  pushSTACK(form); /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(form); pushSTACK(fun);
  /* ANSI CL 3.5.1.2. wants a PROGRAM-ERROR here. */
  fehler(source_program_error,
         GETTEXT("EVAL: too few arguments given to ~S: ~S"));
}

# In EVAL: error, if too many arguments
nonreturning_function(local, fehler_eval_zuviel, (object fun)) {
  var object form = STACK_(frame_form); # Form
  pushSTACK(form); /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(form); pushSTACK(fun);
  /* ANSI CL 3.5.1.3. wants a PROGRAM-ERROR here. */
  fehler(source_program_error,
         GETTEXT("EVAL: too many arguments given to ~S: ~S"));
}

# In EVAL: error, if dotted argument-list
nonreturning_function(local, fehler_eval_dotted, (object fun)) {
  var object form = STACK_(frame_form); # Form
  pushSTACK(form); /* SOURCE-PROGRAM-ERROR slot DETAIL */
  pushSTACK(form); pushSTACK(fun);
  fehler(source_program_error,
         GETTEXT("EVAL: argument list given to ~S is dotted: ~S"));
}

# In EVAL: Applies an SUBR to an argument-list, cleans up STACK
# and returns the values.
# eval_subr(fun);
# > fun: function, a SUBR
# > STACK-layout: EVAL-Frame, *APPLYHOOK*, argument-list.
# < STACK: cleaned up
# < mv_count/mv_space: values
# changes STACK
# can trigger GC
  local maygc Values eval_subr (object fun)
  {
    var object args = popSTACK(); # argument-list
    skipSTACK(1); # forget value of *APPLYHOOK*
    check_SP(); check_STACK();
    var gcv_object_t* args_pointer = args_end_pointer; # Pointer to the arguments
    var gcv_object_t* rest_args_pointer; # Pointer to the remaining arguments
    var uintL argcount; # number of remaining arguments
    # push arguments evaluated in the STACK:
    # first a Dispatch for most important cases:
    switch (TheSubr(fun)->argtype) {
      # Macro for a required-argument:
      #define REQ_ARG()  \
        { if (atomp(args)) goto fehler_zuwenig;                \
          pushSTACK(Cdr(args)); # remaining arguments          \
          eval(Car(args)); # evaluate next argument            \
          args = STACK_0; STACK_0 = value1; # and into STACK   \
        }
      # Macro for the n-th last optional-argument:
      #define OPT_ARG(n)  \
        { if (atomp(args)) goto unbound_optional_##n ;         \
          pushSTACK(Cdr(args)); # remaining arguments          \
          eval(Car(args)); # evaluate next argument            \
          args = STACK_0; STACK_0 = value1; # and into STACK   \
        }
      case (uintW)subr_argtype_6_0:
        # SUBR with 6 required-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_5_0:
        # SUBR with 5 required-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_4_0:
        # SUBR with 4 required-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_3_0:
        # SUBR with 3 required-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_2_0:
        # SUBR with 2 required-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_1_0:
        # SUBR with 1 required-Argument
        REQ_ARG();
      case (uintW)subr_argtype_0_0:
        # SUBR without Arguments
        if (!nullp(args)) goto fehler_zuviel;
        goto apply_subr_norest;
      case (uintW)subr_argtype_4_1:
        # SUBR with 4 required-Arguments and 1 optional-Argument
        REQ_ARG();
      case (uintW)subr_argtype_3_1:
        # SUBR with 3 required-Arguments and 1 optional-Argument
        REQ_ARG();
      case (uintW)subr_argtype_2_1:
        # SUBR with 2 required-Arguments and 1 optional-Argument
        REQ_ARG();
      case (uintW)subr_argtype_1_1:
        # SUBR with 1 required-Argument and 1 optional-Argument
        REQ_ARG();
      case (uintW)subr_argtype_0_1:
        # SUBR with 1 optional-Argument
        OPT_ARG(1);
        if (!nullp(args)) goto fehler_zuviel;
        goto apply_subr_norest;
      case (uintW)subr_argtype_3_2:
        # SUBR with 3 required-Arguments and 2 optional-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_2_2:
        # SUBR with 2 required-Arguments and 2 optional-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_1_2:
        # SUBR with 1 required-Argument and 2 optional-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_0_2:
        # SUBR with 2 optional-Arguments
        OPT_ARG(2);
        OPT_ARG(1);
        if (!nullp(args)) goto fehler_zuviel;
        goto apply_subr_norest;
      case (uintW)subr_argtype_2_3:
        # SUBR with 2 required-Arguments and 3 optional-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_1_3:
        # SUBR with 1 required-Argument and 3 optional-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_0_3:
        # SUBR with 3 optional-Arguments
        OPT_ARG(3);
        OPT_ARG(2);
        OPT_ARG(1);
        if (!nullp(args)) goto fehler_zuviel;
        goto apply_subr_norest;
      case (uintW)subr_argtype_0_5:
        # SUBR with 5 optional-Arguments
        OPT_ARG(5);
      case (uintW)subr_argtype_0_4:
        # SUBR with 4 optional-Arguments
        OPT_ARG(4);
        OPT_ARG(3);
        OPT_ARG(2);
        OPT_ARG(1);
        if (!nullp(args)) goto fehler_zuviel;
        goto apply_subr_norest;
      unbound_optional_5: # Still 5 optional Arguments, but atomp(args)
        pushSTACK(unbound);
      unbound_optional_4: # Still 4 optional Arguments, but atomp(args)
        pushSTACK(unbound);
      unbound_optional_3: # Still 3 optional Arguments, but atomp(args)
        pushSTACK(unbound);
      unbound_optional_2: # Still 2 optional Arguments, but atomp(args)
        pushSTACK(unbound);
      unbound_optional_1: # Still 1 optional Argument, but atomp(args)
        pushSTACK(unbound);
        if (!nullp(args)) goto fehler_dotted;
        goto apply_subr_norest;
      case (uintW)subr_argtype_3_0_rest:
        # SUBR with 3 required-Arguments and further Arguments
        REQ_ARG();
      case (uintW)subr_argtype_2_0_rest:
        # SUBR with 2 required-Arguments and further Arguments
        REQ_ARG();
      case (uintW)subr_argtype_1_0_rest:
        # SUBR with 1 required-Argument and further Arguments
        REQ_ARG();
      case (uintW)subr_argtype_0_0_rest:
        # SUBR with further Arguments
        rest_args_pointer = args_end_pointer; # Pointer to the remaining arguments
        # evaluate all further arguments and into Stack:
        argcount = 0; # counter for the remaining arguments
        while (consp(args)) {
          check_STACK();
          pushSTACK(Cdr(args)); # remaining arguments
          eval(Car(args)); # evaluate next argument
          args = STACK_0; STACK_0 = value1; # and into STACK
          argcount++;
        }
        goto apply_subr_rest;
      case (uintW)subr_argtype_4_0_key:
        # SUBR with 4 required-Arguments and Keyword-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_3_0_key:
        # SUBR with 3 required-Arguments and Keyword-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_2_0_key:
        # SUBR with 2 required-Arguments and Keyword-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_1_0_key:
        # SUBR with 1 required-Argument and Keyword-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_0_0_key:
        # SUBR with Keyword-Arguments
        if (atomp(args)) goto unbound_optional_key_0;
        goto apply_subr_key;
      case (uintW)subr_argtype_1_1_key:
        # SUBR with 1 required-Argument, 1 optional-Argument and Keyword-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_0_1_key:
        # SUBR with 1 optional-Argument and Keyword-Arguments
        OPT_ARG(key_1);
        if (atomp(args)) goto unbound_optional_key_0;
        goto apply_subr_key;
      case (uintW)subr_argtype_1_2_key:
        # SUBR with 1 required-Argument, 2 optional-Arguments and Keyword-Arguments
        REQ_ARG();
        OPT_ARG(key_2);
        OPT_ARG(key_1);
        if (atomp(args)) goto unbound_optional_key_0;
        goto apply_subr_key;
      unbound_optional_key_2: # Silll 2 optional Arguments, but atomp(args)
        pushSTACK(unbound);
      unbound_optional_key_1: # Still 1 optional Argument, but atomp(args)
        pushSTACK(unbound);
      unbound_optional_key_0: # Before the keywords is atomp(args)
        {
          var uintC count;
          dotimesC(count,TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
        }
        if (!nullp(args)) goto fehler_dotted;
        goto apply_subr_norest;
      default: NOTREACHED;
      #undef OPT_ARG
      #undef REQ_ARG
    }
    # Now the general Version:
    # reserve space on the STACK:
    get_space_on_STACK(sizeof(gcv_object_t) *
                       (uintL)(TheSubr(fun)->req_anz +
                               TheSubr(fun)->opt_anz +
                               TheSubr(fun)->key_anz));
    # evaluate required parameters and push into Stack:
    {
      var uintC count;
      dotimesC(count,TheSubr(fun)->req_anz, {
        if (atomp(args)) goto fehler_zuwenig; # at the end of argument-list?
        pushSTACK(Cdr(args)); # remaining argument-list
        eval(Car(args)); # evaluate next argument
        args = STACK_0; STACK_0 = value1; # and into Stack
      });
    }
    { /* evaluate optional parameters and push into Stack: */
      var uintC count = TheSubr(fun)->opt_anz;
      while (!atomp(args)) { /* argument-list not finished? */
        if (count==0) # all optional parameters supplied with?
          goto optionals_ok;
        count--;
        pushSTACK(Cdr(args)); # remaining argument-list
        eval(Car(args)); # evaluate next argument
        args = STACK_0; STACK_0 = value1; # and into Stack
      }
      # argument-list finished.
      # All further count optional parameters get the "value"
      # #<UNBOUND>, the same for the Keyword-parameters:
      dotimesC(count,count + TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
      if (TheSubr(fun)->rest_flag == subr_rest) { # &REST-Flag?
        # yes -> 0 additional arguments:
        argcount = 0; rest_args_pointer = args_end_pointer;
      }
      # no -> nothing to do
      goto los;
    }
   optionals_ok:
    # process Rest- and Keyword-parameters.
    # args = remaining argument-list (not yet finished)
    if (TheSubr(fun)->key_flag == subr_nokey) {
      # SUBR without KEY
      if (TheSubr(fun)->rest_flag == subr_norest) {
        # SUBR without REST or KEY -> argument-list should be finished
        goto fehler_zuviel;
      } else {
        # SUBR with only REST, without KEY: treatment of remaining arguments
        rest_args_pointer = args_end_pointer;
        argcount = 0; # counter for the remaining arguments
        do {
          check_STACK();
          pushSTACK(Cdr(args)); # remaining argument-list
          eval(Car(args)); # evaluate next argument
          args = STACK_0; STACK_0 = value1; # and into Stack
          argcount++;
        } while (consp(args));
        if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
          goto fehler_zuviel;
      }
    } else
    apply_subr_key: { /* SUBR with Keywords. */
      # args = remaining argument-list (not yet finished)
      # First initialize the Keyword-parameters with #<UNBOUND> , then
      # evaluate the remaining arguments and push into Stack, then
      # assign the Keywords:
      var gcv_object_t* key_args_pointer = args_end_pointer; # Pointer to Keyword-parameters
      # initialize all Keyword-parameters with  #<UNBOUND> :
      {
        var uintC count;
        dotimesC(count,TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
      }
      rest_args_pointer = args_end_pointer; # Pointer to the remaining arguments
      # evaluate all further arguments and into Stack:
      argcount = 0; # counter for the remaining arguments
      do {
        check_STACK();
        pushSTACK(Cdr(args)); # remaining argument-list
        eval(Car(args)); # evaluate next argument
        args = STACK_0; STACK_0 = value1; # and into Stack
        argcount++;
      } while (consp(args));
      if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1))
        goto fehler_zuviel;
      # assign Keywords and poss. discard remaining arguments:
      match_subr_key(fun,argcount,key_args_pointer,rest_args_pointer);
    }
   los: # call function
    # remaining argument-list must be NIL :
    if (!nullp(args)) goto fehler_dotted;
    if (TheSubr(fun)->rest_flag == subr_norest) {
      # SUBR without &REST-Flag:
     apply_subr_norest:
      with_saved_back_trace_subr(fun,STACK,-1,
        (*(subr_norest_function_t*)(TheSubr(fun)->function))(); );
    } else {
      # SUBR with &REST-Flag:
     apply_subr_rest:
      with_saved_back_trace_subr(fun,STACK,
                                 TheSubr(fun)->req_anz + TheSubr(fun)->opt_anz + argcount,
        (*(subr_rest_function_t*)(TheSubr(fun)->function))(argcount,rest_args_pointer); );
    }
    #if STACKCHECKS
    if (!(args_pointer == args_end_pointer)) # Stack cleaned up?
      abort(); # no -> leave to Debugger
    #endif
    unwind(); # unwind EVAL-Frame
    return; # finished
    # Gathered error-messages:
   fehler_zuwenig: # Argument-List args is prematurely an Atom
    if (!nullp(args)) goto fehler_dotted;
    set_args_end_pointer(args_pointer); # clean up STACK
    fehler_eval_zuwenig(TheSubr(fun)->name);
   fehler_zuviel: # Argument-List args is not NIL at the end
    if (atomp(args)) goto fehler_dotted;
    set_args_end_pointer(args_pointer); # clean up STACK
    fehler_eval_zuviel(TheSubr(fun)->name);
   fehler_dotted: # Argument-List args ends with Atom /= NIL
    set_args_end_pointer(args_pointer); # clean up STACK
    fehler_eval_dotted(TheSubr(fun)->name);
  }

# In EVAL: Applies a Closure to an argument-list, cleans up the STACK
# and returns the values.
# eval_closure(fun);
# > fun: function, a Closure
# > STACK-layout: EVAL-Frame, *APPLYHOOK*, argument-list.
# < STACK: cleaned up
# < mv_count/mv_space: values
# changes STACK
# can trigger GC
  local maygc Values eval_closure (object closure)
  {
    var object args = popSTACK(); # argument-list
    skipSTACK(1); # forget value of *APPLYHOOK*
    # STACK-layout: EVAL-Frame.
    check_SP(); check_STACK();
    pushSTACK(closure); # save Closure
    var gcv_object_t* closure_ = &STACK_0; # and memorize, where it is
    var gcv_object_t* STACKbefore = STACK;
    if (simple_bit_vector_p(Atype_8Bit,TheClosure(closure)->clos_codevec)) {
      # closure is a compiled Closure
      var object codevec = TheCclosure(closure)->clos_codevec; # Code-Vector
      # push arguments evaluated into STACK:
      # first a dispatch for the most important cases:
      switch (TheCodevec(codevec)->ccv_signature) {
        # Macro for a required-argument:
        #define REQ_ARG()  \
          { if (atomp(args)) goto fehler_zuwenig;                \
            pushSTACK(Cdr(args)); # remaining arguments          \
            eval(Car(args)); # evaluate next argument            \
            args = STACK_0; STACK_0 = value1; # and into STACK   \
          }
        # Macro for the n-last optional-argument:
        #define OPT_ARG(n)  \
          { if (atomp(args)) goto unbound_optional_##n ;         \
            pushSTACK(Cdr(args)); # remaining arguments          \
            eval(Car(args)); # evaluate next argument            \
            args = STACK_0; STACK_0 = value1; # and into STACK   \
          }
        case (uintB)cclos_argtype_5_0:
          # 5 required-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_4_0:
          # 4 required-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_3_0:
          # 3 required-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_2_0:
          # 2 required-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_1_0:
          # 1 required-Argument
          REQ_ARG();
        case (uintB)cclos_argtype_0_0:
          # no Arguments
        noch_0_opt_args:
          if (!nullp(args)) goto fehler_zuviel;
          goto apply_cclosure_nokey;
        case (uintB)cclos_argtype_4_1:
          # 4 required-Arguments and 1 optional-Argument
          REQ_ARG();
        case (uintB)cclos_argtype_3_1:
          # 3 required-Arguments and 1 optional-Argument
          REQ_ARG();
        case (uintB)cclos_argtype_2_1:
          # 2 required-Arguments and 1 optional-Argument
          REQ_ARG();
        case (uintB)cclos_argtype_1_1:
          # 1 required-Argument and 1 optional-Argument
          REQ_ARG();
        case (uintB)cclos_argtype_0_1:
          # 1 optional-Argument
        noch_1_opt_args:
          OPT_ARG(1);
          goto noch_0_opt_args;
        case (uintB)cclos_argtype_3_2:
          # 3 required-Arguments and 2 optional-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_2_2:
          # 2 required-Arguments and 2 optional-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_1_2:
          # 1 required-Argument and 2 optional-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_0_2:
          # 2 optional-Arguments
        noch_2_opt_args:
          OPT_ARG(2);
          goto noch_1_opt_args;
        case (uintB)cclos_argtype_2_3:
          # 2 required-Arguments and 3 optional-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_1_3:
          # 1 required-Argument and 3 optional-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_0_3:
          # 3 optional-Arguments
        noch_3_opt_args:
          OPT_ARG(3);
          goto noch_2_opt_args;
        case (uintB)cclos_argtype_1_4:
          # 1 required-Argument and 4 optional-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_0_4:
          # 4 optional-Arguments
        noch_4_opt_args:
          OPT_ARG(4);
          goto noch_3_opt_args;
        case (uintB)cclos_argtype_0_5:
          # 5 optional-Arguments
          OPT_ARG(5);
          goto noch_4_opt_args;
        unbound_optional_5: # Still 5 optional Arguments, but atomp(args)
          pushSTACK(unbound);
        unbound_optional_4: # Still 4 optional Arguments, but atomp(args)
          pushSTACK(unbound);
        unbound_optional_3: # Still 3 optional Arguments, but atomp(args)
          pushSTACK(unbound);
        unbound_optional_2: # Still 2 optional Arguments, but atomp(args)
          pushSTACK(unbound);
        unbound_optional_1: # Still 1 optional Argument, but atomp(args)
          pushSTACK(unbound);
          if (!nullp(args)) goto fehler_dotted;
          goto apply_cclosure_nokey;
        case (uintB)cclos_argtype_4_0_rest:
          # 4 required-Arguments, Rest-Parameter
          REQ_ARG();
        case (uintB)cclos_argtype_3_0_rest:
          # 3 required-Arguments, Rest-Parameter
          REQ_ARG();
        case (uintB)cclos_argtype_2_0_rest:
          # 2 required-Arguments, Rest-Parameter
          REQ_ARG();
        case (uintB)cclos_argtype_1_0_rest:
          # 1 required-Argument, Rest-Parameter
          REQ_ARG();
        case (uintB)cclos_argtype_0_0_rest:
          # no Arguments, Rest-Parameter
          if (consp(args)) goto apply_cclosure_rest_nokey;
          if (!nullp(args)) goto fehler_dotted;
          pushSTACK(NIL); # Rest-Parameter := NIL
          goto apply_cclosure_nokey;
        case (uintB)cclos_argtype_4_0_key:
          # 4 required-Arguments, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_3_0_key:
          # 3 required-Arguments, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_2_0_key:
          # 2 required-Arguments, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_1_0_key:
          # 1 required-Argument, Keyword-Arguments
          REQ_ARG();
        noch_0_opt_args_key:
          closure = *closure_; codevec = TheCclosure(closure)->clos_codevec;
        case (uintB)cclos_argtype_0_0_key:
          # only Keyword-Arguments
          if (atomp(args)) goto unbound_optional_key_0;
          goto apply_cclosure_key;
        case (uintB)cclos_argtype_3_1_key:
          # 3 required-Arguments and 1 optional-Argument, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_2_1_key:
          # 2 required-Arguments and 1 optional-Argument, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_1_1_key:
          # 1 required-Argument and 1 optional-Argument, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_0_1_key:
          # 1 optional-Argument, Keyword-Arguments
        noch_1_opt_args_key:
          OPT_ARG(key_1);
          goto noch_0_opt_args_key;
        case (uintB)cclos_argtype_2_2_key:
          # 2 required-Arguments and 2 optional-Arguments, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_1_2_key:
          # 1 required-Argument and 2 optional-Arguments, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_0_2_key:
          # 2 optional-Arguments, Keyword-Arguments
        noch_2_opt_args_key:
          OPT_ARG(key_2);
          goto noch_1_opt_args_key;
        case (uintB)cclos_argtype_1_3_key:
          # 1 required-Argument and 3 optional-Arguments, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_0_3_key:
          # 3 optional-Arguments, Keyword-Arguments
        noch_3_opt_args_key:
          OPT_ARG(key_3);
          goto noch_2_opt_args_key;
        case (uintB)cclos_argtype_0_4_key:
          # 4 optional-Arguments, Keyword-Arguments
          OPT_ARG(key_4);
          goto noch_3_opt_args_key;
        unbound_optional_key_4: # Still 4 optional Arguments, but atomp(args)
          pushSTACK(unbound);
        unbound_optional_key_3: # Still 3 optional Arguments, but atomp(args)
          pushSTACK(unbound);
        unbound_optional_key_2: # Still 2 optional Arguments, but atomp(args)
          pushSTACK(unbound);
        unbound_optional_key_1: # Still 1 optional Argument, but atomp(args)
          pushSTACK(unbound);
        unbound_optional_key_0: # Before the Keywords is atomp(args)
          if (!nullp(args)) goto fehler_dotted;
          goto apply_cclosure_key_noargs;
        case (uintB)cclos_argtype_default:
          # General Version
          break;
        default: NOTREACHED;
        #undef OPT_ARG
        #undef REQ_ARG
      }
      # Now the general Version:
      {
        var uintL req_anz = TheCodevec(codevec)->ccv_numreq; # number of required parameters
        var uintL opt_anz = TheCodevec(codevec)->ccv_numopt; # number of optional parameters
        var uintB flags = TheCodevec(codevec)->ccv_flags; # Flags
        # reserve space on STACK:
        get_space_on_STACK(sizeof(gcv_object_t) * (req_anz+opt_anz));
        # evaluate required parameters and push into Stack:
        {
          var uintC count;
          dotimesC(count,req_anz, {
            if (atomp(args)) goto fehler_zuwenig; # argument-list finished?
            pushSTACK(Cdr(args)); # remaining argument-list
            eval(Car(args)); # evaluate nnext argument
            args = STACK_0; STACK_0 = value1; # and into Stack
          });
        }
        { /* evaluate optional parameters and push into Stack: */
          var uintC count = opt_anz;
          while (!atomp(args)) { /* argument-list not finished? */
            if (count==0) # all optional parameters supplied with?
              goto optionals_ok;
            count--;
            pushSTACK(Cdr(args)); # remaining argument-list
            eval(Car(args)); # evaluate next argument
            args = STACK_0; STACK_0 = value1; # and into Stack
          }
          # argument-list finished.
          if (!nullp(args)) goto fehler_dotted;
          # All further count optional parameters get the "value"
          # #<UNBOUND>, the &REST-parameter gets the value NIL,
          # the Keyword-parameter gets the value #<UNBOUND> :
          dotimesC(count,count, { pushSTACK(unbound); } );
        }
        closure = *closure_; codevec = TheCclosure(closure)->clos_codevec;
        if (flags & bit(0)) # &REST-Flag?
          pushSTACK(NIL); # yes -> initialize with NIL
        if (flags & bit(7)) # &KEY-Flag?
          goto apply_cclosure_key_noargs;
        else
          goto apply_cclosure_nokey_;
       optionals_ok:
        # process Rest- and Keyword-parameters.
        # args = remaining argument-list (not yet finished)
        closure = *closure_; codevec = TheCclosure(closure)->clos_codevec;
        if (flags == 0)
          # Closure without REST or KEY -> argument-list should be finished
          goto fehler_zuviel;
        elif (flags & bit(7)) { # Key-Flag?
          # Closure with Keywords.
          # args = remaining argument-list (not yet finished)
          # First initialize the Keyword-parameters with #<UNBOUND> , then
          # evaluate the remaining arguments and push into Stack, then
          # assign the Keywords:
          # poss. initialize the Rest-Parameter:
          if (flags & bit(0))
            pushSTACK(unbound);
          goto apply_cclosure_key;
        } else
            goto apply_cclosure_rest_nokey;
      }
     apply_cclosure_key_noargs:
      {
        var uintC count = TheCodevec(codevec)->ccv_numkey; # number of Keyword-parameters
        dotimesC(count,count, { pushSTACK(unbound); } ); # initialize with #<UNBOUND>
        interpret_bytecode(closure,codevec,CCV_START_KEY); # interprete bytecode starting at Byte 12
      }
      goto done;
     apply_cclosure_key: # jump to Closure only with &KEY:
      {
        var gcv_object_t* key_args_pointer = args_end_pointer; # Pointer to Keyword-Parameter
        # initialize all Keyword-parameters with #<UNBOUND> :
        {
          var uintC count = TheCodevec(codevec)->ccv_numkey;
          dotimesC(count,count, { pushSTACK(unbound); } );
        }
        var gcv_object_t* rest_args_pointer = args_end_pointer; # Pointer to the remaining arguments
        # evaluate all further arguments and push into Stack:
        var uintL argcount = 0; # counter for the remaining arguments
        do {
          check_STACK();
          pushSTACK(Cdr(args)); # remaining argument-list
          eval(Car(args)); # evaluate next argument
          args = STACK_0; STACK_0 = value1; # and into Stack
          argcount++;
        } while (consp(args));
        # argument-list finished.
        if (!nullp(args)) goto fehler_dotted;
        # assign Keywords, build Rest-Parameter
        # and poss. discard remaining arguments:
        closure = match_cclosure_key(*closure_,argcount,key_args_pointer,rest_args_pointer);
        codevec = TheCclosure(closure)->clos_codevec;
        interpret_bytecode(closure,codevec,CCV_START_KEY); # interprete bytecode starting at Byte 12
      }
      goto done;
     apply_cclosure_rest_nokey:
      # Closure with only REST, without KEY:
      # evaluate remaining arguments one by on, put into list
      # args = remaining argument-list (not yet finished)
      pushSTACK(NIL); # so far evaluated remaining arguments
      pushSTACK(args); # remaining arguments, unevaluated
      do {
        args = STACK_0; STACK_0 = Cdr(args);
        eval(Car(args)); # evaluate next argument
        pushSTACK(value1);
        # and cons onto the list:
        var object new_cons = allocate_cons();
        Car(new_cons) = popSTACK();
        Cdr(new_cons) = STACK_1;
        STACK_1 = new_cons;
      } while (mconsp(STACK_0));
      args = popSTACK();
      # reverse list STACK_0 and use as REST-parameter:
      nreverse(STACK_0);
      # argument-list finished.
      if (!nullp(args)) goto fehler_dotted;
     apply_cclosure_nokey: # jump to Closure without &KEY :
      closure = *closure_; codevec = TheCclosure(closure)->clos_codevec;
     apply_cclosure_nokey_:
      interpret_bytecode(closure,codevec,CCV_START_NONKEY); # interprete bytecode starting at Byte 8
     done:
      #if STACKCHECKC
      if (!(STACK == STACKbefore)) # STACK as before?
        abort(); # no -> go to Debugger
      #endif
      skipSTACK(1); # discard Closure
      unwind(); # unwind EVAL-Frame
      return; # finished
    } else {
      # closure is an interpreted Closure
      var gcv_object_t* args_pointer = args_end_pointer; # Pointer to the arguments
      var uintC args_on_stack = 0; # Anzahl der Argumente
      while (consp(args)) {
        pushSTACK(Cdr(args)); # save rest of list
        eval(Car(args)); # evaluate next element
        args = STACK_0; STACK_0 = value1; # result into STACK
        args_on_stack += 1;
        if (((uintL)~(uintL)0 > ca_limit_1) && (args_on_stack > ca_limit_1))
          goto fehler_zuviel;
      }
      with_saved_back_trace_iclosure(*closure_,args_pointer,args_on_stack,
        funcall_iclosure(*closure_,args_pointer,args_on_stack); );
      skipSTACK(1); # discard Closure
      unwind(); # unwind EVAL-Frame
      return; # finished
    }
    # Gathered errormessages:
   fehler_zuwenig: # Argument-list args is prematurely an Atom
    if (!nullp(args)) goto fehler_dotted;
    setSTACK(STACK = STACKbefore); # clean up STACK
    closure = popSTACK();
    fehler_eval_zuwenig(Closure_name(closure));
   fehler_zuviel: # Argument-list args is not NIL at the end
    if (atomp(args)) goto fehler_dotted;
    setSTACK(STACK = STACKbefore); # clean up STACK
    closure = popSTACK();
    fehler_eval_zuviel(Closure_name(closure));
   fehler_dotted: # Argument-list args ends with Atom /= NIL
    setSTACK(STACK = STACKbefore); # clean up STACK
    closure = popSTACK();
    fehler_eval_dotted(Closure_name(closure));
  }

#ifdef DYNAMIC_FFI
# In EVAL: Applies a Foreign-Function to an argument-list,
# cleans up STACK and returns the values.
# eval_ffunction(fun);
# > fun: function, a Foreign-Function
# > STACK-layout: EVAL-Frame, *APPLYHOOK*, argument-list.
# < STACK: cleaned up
# < mv_count/mv_space: values
# changes STACK
# can trigger GC
local maygc Values eval_ffunction(object ffun) {
  var object args = popSTACK(); # Argument-list
  skipSTACK(1); # skip value of *APPLYHOOK*
  # STACK-layout: EVAL-Frame.
  # (ffun arg ...) --> (FFI::FOREIGN-CALL-OUT ffun arg ...)
  check_SP(); check_STACK();
  pushSTACK(ffun); # Foreign-Function as 1. Argument
  {
    var gcv_object_t* args_pointer = args_end_pointer; # Pointer to the arguments
    var uintC args_on_stack = 1; # number of arguments
    while (consp(args)) {
      pushSTACK(Cdr(args)); # save list-rest
      eval(Car(args)); # evaluate next element
      args = STACK_0; STACK_0 = value1; # result into STACK
      args_on_stack += 1;
      if (((uintL)~(uintL)0 > ca_limit_1) && (args_on_stack > ca_limit_1)) {
        set_args_end_pointer(args_pointer);
        fehler_eval_zuviel(popSTACK());
      }
    }
    funcall(L(foreign_call_out),args_on_stack);
  }
  unwind(); # unwind EVAL-Frame
  return; # finished
}
#endif


#          ----------------------- A P P L Y -----------------------

# later:
local Values apply_subr (object fun, uintC args_on_stack, object other_args);
local Values apply_closure(object fun, uintC args_on_stack, object other_args);

/* UP: Applies a function to its arguments.
 apply(function,args_on_stack,other_args);
 > function: function
 > arguments: args_on_stack arguments on the STACK,
              remaining argument-list in other_args
 < STACK: cleaned up (i.e. STACK is increased by args_on_stack)
 < mv_count/mv_space: values
 changes STACK, can trigger GC */
global maygc Values apply (object fun, uintC args_on_stack, object other_args)
{
 apply_restart:
  /* fun must be a SUBR or a Closure or a Cons (LAMBDA ...) : */
  if (subrp(fun)) { /* SUBR ? */
    return_Values apply_subr(fun,args_on_stack,other_args);
  } else if (closurep(fun)) { /* Closure ? */
    return_Values apply_closure(fun,args_on_stack,other_args);
  } else if (symbolp(fun)) { /* Symbol ? */
    /* apply Symbol: global Definition Symbol_function(fun) applies. */
    var object fdef = Symbol_function(fun);
    if (subrp(fdef)) { /* SUBR -> apply */
      return_Values apply_subr(fdef,args_on_stack,other_args);
    } else if (closurep(fdef)) { /* Closure -> apply */
      return_Values apply_closure(fdef,args_on_stack,other_args);
    } else if (orecordp(fdef)) {
     #ifdef DYNAMIC_FFI
      if (ffunctionp(fdef)) { /* Foreign-Function ? */
        fun = fdef; goto call_ffunction;
      }
     #endif
      switch (Record_type(fdef)) {
        case Rectype_Fsubr:
          fehler_specialform(S(apply),fun);
        case Rectype_Macro:
          fehler_macro(S(apply),fun);
        default: NOTREACHED;
      }
    } else
      /* if no SUBR, no Closure, no FSUBR, no Macro:
         Symbol_function(fun) must be #<UNBOUND> . */
      goto undef;
  } else if (funnamep(fun)) { /* List (SETF symbol) ? */
    /* global Definition (symbol-function (get-setf-symbol symbol)) applies. */
    var object symbol = get(Car(Cdr(fun)),S(setf_function)); /* (get ... 'SYS::SETF-FUNCTION) */
    if (!symbolp(symbol)) /* should be (uninterned) Symbol */
      goto undef; /* else undefined */
    var object fdef = Symbol_function(symbol);
    if (closurep(fdef)) { /* Closure -> apply */
      return_Values apply_closure(fdef,args_on_stack,other_args);
    } else if (subrp(fdef)) { /* SUBR -> apply */
      return_Values apply_subr(fdef,args_on_stack,other_args);
    }
   #ifdef DYNAMIC_FFI
    else if (ffunctionp(fdef)) { /* Foreign-Function ? */
      fun = fdef; goto call_ffunction;
    }
   #endif
    else
      /* Such function-names cannot denote FSUBRs or Macros.
         fdef is presumably #<UNBOUND> . */
      goto undef;
  }
 #ifdef DYNAMIC_FFI
  else if (ffunctionp(fun)) /* Foreign-Function ? */
  call_ffunction: { /* call (SYS::FOREIGN-CALL-OUT foreign-function . args) */
    /* Therefore first shift down the arguments in Stack by 1. */
    var uintC count;
    var gcv_object_t* ptr = &STACK_0;
    dotimesC(count,args_on_stack, {
      *(ptr STACKop -1) = *ptr; ptr skipSTACKop 1;
    });
    *(ptr STACKop -1) = fun;
    skipSTACK(-1);
    return_Values apply_subr(L(foreign_call_out),args_on_stack+1,other_args);
  }
 #endif
  else if (consp(fun) && eq(Car(fun),S(lambda))) /* Cons (LAMBDA ...) ? */
    fehler_lambda_expression(S(apply),fun);
  else {
    pushSTACK(other_args);
    fun = check_funname_replacement(type_error,S(apply),fun);
    other_args = popSTACK();
    goto apply_restart;
  }
  return;
 undef:
  pushSTACK(other_args);
  fun = check_fdefinition(fun,S(apply));
  other_args = popSTACK();
  goto apply_restart;
}

# Error because of dotted argument-list
# > name: name of function
nonreturning_function(local, fehler_apply_dotted, (object name, object end)) {
  pushSTACK(end);
  pushSTACK(name);
  pushSTACK(S(apply));
  fehler(program_error,GETTEXT("~S: argument list given to ~S is dotted (terminated by ~S)"));
}

# Error because of too many arguments
# > name: name of function
nonreturning_function(local, fehler_apply_zuviel, (object name)) {
  pushSTACK(name);
  /* ANSI CL 3.5.1.3. wants a PROGRAM-ERROR here. */
  fehler(program_error,GETTEXT("APPLY: too many arguments given to ~S"));
}

# Error because of too few arguments
# > name: name of function
# > tail: atom at the end of the argument list
nonreturning_function(local, fehler_apply_zuwenig, (object name, object tail)) {
  if (!nullp(tail)) {
    pushSTACK(tail); /* ARGUMENT-LIST-DOTTED slot DATUM */
    pushSTACK(tail); pushSTACK(name);
    fehler(argument_list_dotted,
           GETTEXT("APPLY: dotted argument list given to ~S : ~S"));
  } else {
    pushSTACK(name);
    /* ANSI CL 3.5.1.2. wants a PROGRAM-ERROR here. */
    fehler(program_error,GETTEXT("APPLY: too few arguments given to ~S"));
  }
}

# Error because of too many arguments for a SUBR
# > fun: function, a SUBR
nonreturning_function(local, fehler_subr_zuviel, (object fun));
#define fehler_subr_zuviel(fun)  fehler_apply_zuviel(TheSubr(fun)->name)

# Error because of too few arguments for a SUBR
# > fun: function, a SUBR
# > tail: atom at the end of the argument list
nonreturning_function(local, fehler_subr_zuwenig, (object fun, object tail));
#define fehler_subr_zuwenig(fun,tail)  \
  fehler_apply_zuwenig(TheSubr(fun)->name,tail)

# In APPLY: Applies a SUBR to an argument-list, cleans up STACK
# and returns the values.
# apply_subr(fun,args_on_stack,other_args);
# > fun: function, a SUBR
# > Arguments: args_on_stack Arguments on STACK,
#              remaining argument-list in other_args
# < STACK: cleaned up (i.e. STACK is increased by args_on_stack)
# < mv_count/mv_space: values
# changes STACK, can trigger GC
  local maygc Values apply_subr (object fun, uintC args_on_stack, object args)
  {
    #if STACKCHECKS
    var gcv_object_t* args_pointer = args_end_pointer STACKop args_on_stack; # Pointer to the arguments
    #endif
    var gcv_object_t* key_args_pointer; # Pointer to the Keyword-Arguments
    var gcv_object_t* rest_args_pointer; # Pointer to the remaining Arguments
    var uintL argcount; # number of remaining Arguments
    TRACE_CALL(fun,'A','S');
    # push Arguments on STACK:
    # first a Dispatch for the most important cases:
    switch (TheSubr(fun)->argtype) {
      # Macro for a required-Argument:
      #define REQ_ARG()  \
        { if (args_on_stack>0) { args_on_stack--; }                      \
          elif (consp(args)) { pushSTACK(Car(args)); args = Cdr(args); } \
          else goto fehler_zuwenig;                                      \
        }
      # Macro for the n-last optional-Argument:
      #define OPT_ARG(n)  \
        { if (args_on_stack>0) { args_on_stack--; }                      \
          elif (consp(args)) { pushSTACK(Car(args)); args = Cdr(args); } \
          else goto unbound_optional_##n;                                \
        }
      case (uintW)subr_argtype_6_0:
        # SUBR with 6 required-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_5_0:
        # SUBR with 5 required-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_4_0:
        # SUBR with 4 required-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_3_0:
        # SUBR with 3 required-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_2_0:
        # SUBR with 2 required-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_1_0:
        # SUBR with 1 required-Argument
        REQ_ARG();
      case (uintW)subr_argtype_0_0:
        # SUBR without Arguments
        if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
        goto apply_subr_norest;
      case (uintW)subr_argtype_4_1:
        # SUBR with 4 required-Arguments and 1 optional-Argument
        REQ_ARG();
      case (uintW)subr_argtype_3_1:
        # SUBR with 3 required-Arguments and 1 optional-Argument
        REQ_ARG();
      case (uintW)subr_argtype_2_1:
        # SUBR with 2 required-Arguments and 1 optional-Argument
        REQ_ARG();
      case (uintW)subr_argtype_1_1:
        # SUBR with 1 required-Argument and 1 optional-Argument
        REQ_ARG();
      case (uintW)subr_argtype_0_1:
        # SUBR with 1 optional-Argument
        OPT_ARG(1);
        if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
        goto apply_subr_norest;
      case (uintW)subr_argtype_3_2:
        # SUBR with 3 required-Arguments and 2 optional-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_2_2:
        # SUBR with 2 required-Arguments and 2 optional-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_1_2:
        # SUBR with 1 required-Argument and 2 optional-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_0_2:
        # SUBR with 2 optional-Arguments
        OPT_ARG(2);
        OPT_ARG(1);
        if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
        goto apply_subr_norest;
      case (uintW)subr_argtype_2_3:
        # SUBR with 2 required-Arguments and 3 optional-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_1_3:
        # SUBR with 1 required-Argument and 3 optional-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_0_3:
        # SUBR with 3 optional-Arguments
        OPT_ARG(3);
        OPT_ARG(2);
        OPT_ARG(1);
        if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
        goto apply_subr_norest;
      case (uintW)subr_argtype_0_5:
        # SUBR with 5 optional-Arguments
        OPT_ARG(5);
      case (uintW)subr_argtype_0_4:
        # SUBR with 4 optional-Arguments
        OPT_ARG(4);
        OPT_ARG(3);
        OPT_ARG(2);
        OPT_ARG(1);
        if ((args_on_stack>0) || consp(args)) goto fehler_zuviel;
        goto apply_subr_norest;
      unbound_optional_5: # Still 5 optional Arguments, but args_on_stack=0 and atomp(args)
        pushSTACK(unbound);
      unbound_optional_4: # Still 4 optional Arguments, but args_on_stack=0 and atomp(args)
        pushSTACK(unbound);
      unbound_optional_3: # Still 3 optional Arguments, but args_on_stack=0 and atomp(args)
        pushSTACK(unbound);
      unbound_optional_2: # Still 2 optional Arguments, but args_on_stack=0 and atomp(args)
        pushSTACK(unbound);
      unbound_optional_1: # Still 1 optionals Argument, but args_on_stack=0 and atomp(args)
        pushSTACK(unbound);
        goto apply_subr_norest;
      case (uintW)subr_argtype_3_0_rest:
        # SUBR with 3 required-Arguments and further Arguments
        REQ_ARG();
      case (uintW)subr_argtype_2_0_rest:
        # SUBR with 2 required-Arguments and further Arguments
        REQ_ARG();
      case (uintW)subr_argtype_1_0_rest:
        # SUBR with 1 required-Argument and further Arguments
        REQ_ARG();
      case (uintW)subr_argtype_0_0_rest:
        # SUBR with further Arguments
        if (args_on_stack==0)
          goto apply_subr_rest_onlylist;
        else
          goto apply_subr_rest_withlist;
      case (uintW)subr_argtype_4_0_key:
        # SUBR with 4 required-Arguments and Keyword-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_3_0_key:
        # SUBR with 3 required-Arguments and Keyword-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_2_0_key:
        # SUBR with 2 required-Arguments and Keyword-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_1_0_key:
        # SUBR with 1 required-Argument and Keyword-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_0_0_key:
        # SUBR with Keyword-Arguments
        if ((args_on_stack==0) && atomp(args)) goto unbound_optional_key_0;
        goto apply_subr_key;
      case (uintW)subr_argtype_1_1_key:
        # SUBR with 1 required-Argument, 1 optional-Argument and Keyword-Arguments
        REQ_ARG();
      case (uintW)subr_argtype_0_1_key:
        # SUBR with 1 optional-Argument and Keyword-Arguments
        OPT_ARG(key_1);
        if ((args_on_stack==0) && atomp(args)) goto unbound_optional_key_0;
        goto apply_subr_key;
      case (uintW)subr_argtype_1_2_key:
        # SUBR with 1 required-Argument, 2 optional-Arguments and Keyword-Arguments
        REQ_ARG();
        OPT_ARG(key_2);
        OPT_ARG(key_1);
        if ((args_on_stack==0) && atomp(args)) goto unbound_optional_key_0;
        goto apply_subr_key;
      unbound_optional_key_2: # Still 2 optional Arguments, but args_on_stack=0 and atomp(args)
        pushSTACK(unbound);
      unbound_optional_key_1: # Still 1 optional Argument, but args_on_stack=0 and atomp(args)
        pushSTACK(unbound);
      unbound_optional_key_0: # Before the Keywords is args_on_stack=0 and atomp(args)
        {
          var uintC count;
          dotimesC(count,TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
        }
        goto apply_subr_norest;
      default: NOTREACHED;
      #undef OPT_ARG
      #undef REQ_ARG
    }
    # Now the general Version:
    {
      var uintC key_anz;
      {
        var uintC req_anz = TheSubr(fun)->req_anz;
        var uintC opt_anz = TheSubr(fun)->opt_anz;
        key_anz = TheSubr(fun)->key_anz;
        if (args_on_stack < req_anz) {
          # fewer Arguments there than demanded
          req_anz = req_anz - args_on_stack; # as many as these must go on STACK
          # reserve space on STACK:
          get_space_on_STACK(sizeof(gcv_object_t) * (uintL)(req_anz + opt_anz + key_anz));
          # store required Parameter in Stack:
          {
            var uintC count;
            dotimespC(count,req_anz, {
              if (atomp(args))
                goto fehler_zuwenig;
              pushSTACK(Car(args)); # store next Argument
              args = Cdr(args);
            });
          }
          goto optionals_from_list;
        }
        args_on_stack -= req_anz; # remaining number
        if (args_on_stack < opt_anz) {
          # Arguments in Stack don't last for the optional ones
          opt_anz = opt_anz - args_on_stack; # as many as these must go on STACK
          # reserve space on STACK:
          get_space_on_STACK(sizeof(gcv_object_t) * (uintL)(opt_anz + key_anz));
         optionals_from_list:
          { /* store optional Parameters on Stack: */
            var uintC count = opt_anz;
            while (!atomp(args)) { /* argument-list not finished? */
              if (count==0) # all optional Parameters supplied with?
                goto optionals_ok;
              count--;
              pushSTACK(Car(args)); # store next Argument
              args = Cdr(args);
            }
            # Argument-list finished.
            # All further count optional Parameters receive the "value"
            # #<UNBOUND>, including the Keyword-Parameters:
            dotimesC(count,count + key_anz, { pushSTACK(unbound); } );
            if (TheSubr(fun)->rest_flag == subr_rest) { # &REST-Flag?
              # yes -> 0 additional Arguments:
              argcount = 0; rest_args_pointer = args_end_pointer;
              goto apply_subr_rest;
            } else {
              # no -> nothing to do
              goto apply_subr_norest;
            }
          }
         optionals_ok: # optional Argument OK, continue processing (non-empty) list
          if (TheSubr(fun)->key_flag == subr_nokey) {
            # SUBR without KEY
            if (TheSubr(fun)->rest_flag == subr_norest)
              # SUBR without REST or KEY
              fehler_subr_zuviel(fun); # too many Arguments
            else
              # SUBR with only REST, without KEY
              goto apply_subr_rest_onlylist;
          } else {
            # SUBR with KEY
            key_args_pointer = args_end_pointer;
            {
              var uintC count;
              dotimesC(count,key_anz, { pushSTACK(unbound); } );
            }
            rest_args_pointer = args_end_pointer;
            argcount = 0;
            goto key_from_list;
          }
        }
        args_on_stack -= opt_anz; # remaining number
        if (TheSubr(fun)->key_flag == subr_nokey) {
          # SUBR without KEY
          if (TheSubr(fun)->rest_flag == subr_norest) {
            # SUBR without REST or KEY
            if ((args_on_stack>0) || consp(args)) # still Arguments?
              fehler_subr_zuviel(fun);
            goto apply_subr_norest;
          } else
            # SUBR with only REST, without KEY
            goto apply_subr_rest_withlist;
        } else
          # SUBR with Keywords.
          goto apply_subr_key_;
      }
     apply_subr_key:
      key_anz = TheSubr(fun)->key_anz;
     apply_subr_key_:
      # shift down remaining Arguments on STACK and thus
      # create room for the Keyword-Parameters:
      argcount = args_on_stack;
      get_space_on_STACK(sizeof(gcv_object_t) * (uintL)key_anz);
      {
        var gcv_object_t* new_args_end_pointer = args_end_pointer STACKop -(uintP)key_anz;
        var gcv_object_t* ptr1 = args_end_pointer;
        var gcv_object_t* ptr2 = new_args_end_pointer;
        var uintC count;
        dotimesC(count,args_on_stack, { BEFORE(ptr2) = BEFORE(ptr1); } );
        key_args_pointer = ptr1;
        rest_args_pointer = ptr2;
        dotimesC(count,key_anz, { NEXT(ptr1) = unbound; } );
        set_args_end_pointer(new_args_end_pointer);
      }
     key_from_list: # take remaining Arguments for Keywords from list
      while (consp(args)) {
        check_STACK(); pushSTACK(Car(args)); # push next argument onto Stack
        args = Cdr(args);
        argcount++;
      }
      # assign Keywords and poss. discard remaining arguments:
      match_subr_key(fun,argcount,key_args_pointer,rest_args_pointer);
      if (TheSubr(fun)->rest_flag != subr_norest)
        # SUBR with &REST-Flag:
        goto apply_subr_rest;
      else
        # SUBR without &REST-Flag:
        goto apply_subr_norest;
    }
   apply_subr_rest_onlylist:
    argcount = 0; rest_args_pointer = args_end_pointer;
    goto rest_from_list;
   apply_subr_rest_withlist:
    argcount = args_on_stack;
    rest_args_pointer = args_end_pointer STACKop argcount;
   rest_from_list: # take remaining Arguments from list
    while (consp(args)) {
      check_STACK(); pushSTACK(Car(args)); # next argument onto Stack
      args = Cdr(args);
      argcount++;
    }
    if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1)) # too many arguments?
      goto fehler_zuviel;
   apply_subr_rest:
    if (!nullp(args))
      goto fehler_dotted;
    with_saved_back_trace_subr(fun,STACK,
                               TheSubr(fun)->req_anz + TheSubr(fun)->opt_anz + argcount,
      (*(subr_rest_function_t*)(TheSubr(fun)->function))(argcount,rest_args_pointer); );
    goto done;
   apply_subr_norest:
    if (!nullp(args))
      goto fehler_dotted;
    with_saved_back_trace_subr(fun,STACK,-1,
      (*(subr_norest_function_t*)(TheSubr(fun)->function))(); );
   done:
    #if STACKCHECKS
    if (!(args_pointer == args_end_pointer)) # Stack cleaned up?
      abort(); # no -> go to Debugger
    #endif
    return; # finished
    # gathered error messages:
   fehler_zuwenig: fehler_subr_zuwenig(fun,args);
   fehler_zuviel: fehler_subr_zuviel(fun);
   fehler_dotted: fehler_apply_dotted(TheSubr(fun)->name,args);
  }

# Error because of too many arguments for a Closure
# > closure: function, a Closure
nonreturning_function(local, fehler_closure_zuviel, (object closure));
#define fehler_closure_zuviel(closure)  fehler_apply_zuviel(closure)

# Error because of too few arguments for a Closure
# > closure: function, a Closure
# > tail: atom at the end of the argument list
nonreturning_function(local, fehler_closure_zuwenig, (object closure, object tail));
#define fehler_closure_zuwenig(closure,tail)  fehler_apply_zuwenig(closure,tail)

# In APPLY: Applies a Closure to an argument-list, cleans up STACK
# and returns the values.
# apply_closure(fun,args_on_stack,other_args);
# > fun: function, a Closure
# > Argumente: args_on_stack arguments on STACK,
#              remaining argument-list in other_args
# < STACK: cleaned up (i.e. STACK is increased by args_on_stack)
# < mv_count/mv_space: values
# changes STACK, can trigger GC
  local maygc Values apply_closure (object closure, uintC args_on_stack, object args)
  {
    TRACE_CALL(closure,'A','C');
    if (simple_bit_vector_p(Atype_8Bit,TheClosure(closure)->clos_codevec)) {
      # closure is a compiled Closure
      #if STACKCHECKC
      var gcv_object_t* args_pointer = args_end_pointer STACKop args_on_stack; # Pointer to the arguments
      #endif
      var object codevec = TheCclosure(closure)->clos_codevec; # Code-Vector
      var gcv_object_t* key_args_pointer; # Pointer to the Keyword-arguments
      var gcv_object_t* rest_args_pointer; # Pointer to the remaining arguments
      var uintL argcount; # number of remaining arguments
      check_SP(); check_STACK();
      # put argumente in STACK:
      # first a Dispatch for the most important cases:
      switch (TheCodevec(codevec)->ccv_signature) {
        # Macro for a required-argument:
        #define REQ_ARG()  \
          { if (args_on_stack>0) { args_on_stack--; }                      \
            elif (consp(args)) { pushSTACK(Car(args)); args = Cdr(args); } \
            else goto fehler_zuwenig;                                      \
          }
        # Macro for the n-last optional-argument:
        #define OPT_ARG(n)  \
          { if (args_on_stack>0) { args_on_stack--; }                      \
            elif (consp(args)) { pushSTACK(Car(args)); args = Cdr(args); } \
            else goto unbound_optional_##n;                                \
          }
        case (uintB)cclos_argtype_5_0:
          # 5 required-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_4_0:
          # 4 required-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_3_0:
          # 3 required-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_2_0:
          # 2 required-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_1_0:
          # 1 required-Argument
          REQ_ARG();
        case (uintB)cclos_argtype_0_0:
          # no Arguments
          noch_0_opt_args:
          if (args_on_stack>0) goto fehler_zuviel;
          if (!nullp(args)) {
            if (consp(args))
              goto fehler_zuviel;
            else
              goto fehler_dotted;
          }
          goto apply_cclosure_nokey;
        case (uintB)cclos_argtype_4_1:
          # 4 required-Arguments and 1 optional-Argument
          REQ_ARG();
        case (uintB)cclos_argtype_3_1:
          # 3 required-Arguments and 1 optional-Argument
          REQ_ARG();
        case (uintB)cclos_argtype_2_1:
          # 2 required-Arguments and 1 optional-Argument
          REQ_ARG();
        case (uintB)cclos_argtype_1_1:
          # 1 required-Argument and 1 optional-Argument
          REQ_ARG();
        case (uintB)cclos_argtype_0_1:
          # 1 optional-Argument
          noch_1_opt_args:
          OPT_ARG(1);
          goto noch_0_opt_args;
        case (uintB)cclos_argtype_3_2:
          # 3 required-Arguments and 2 optional-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_2_2:
          # 2 required-Arguments and 2 optional-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_1_2:
          # 1 required-Argument and 2 optional-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_0_2:
          # 2 optional-Arguments
          noch_2_opt_args:
          OPT_ARG(2);
          goto noch_1_opt_args;
        case (uintB)cclos_argtype_2_3:
          # 2 required-Arguments and 3 optional-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_1_3:
          # 1 required-Argument and 3 optional-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_0_3:
          # 3 optional-Arguments
          noch_3_opt_args:
          OPT_ARG(3);
          goto noch_2_opt_args;
        case (uintB)cclos_argtype_1_4:
          # 1 required-Argument and 4 optional-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_0_4:
          # 4 optional-Arguments
          noch_4_opt_args:
          OPT_ARG(4);
          goto noch_3_opt_args;
        case (uintB)cclos_argtype_0_5:
          # 5 optional-Arguments
          OPT_ARG(5);
          goto noch_4_opt_args;
        unbound_optional_5: # Still 5 optional Arguments, but args_on_stack=0 and atomp(args)
          pushSTACK(unbound);
        unbound_optional_4: # Still 4 optional Arguments, but args_on_stack=0 and atomp(args)
          pushSTACK(unbound);
        unbound_optional_3: # Still 3 optional Arguments, but args_on_stack=0 and atomp(args)
          pushSTACK(unbound);
        unbound_optional_2: # Still 2 optional Arguments, but args_on_stack=0 and atomp(args)
          pushSTACK(unbound);
        unbound_optional_1: # Still 1 optional Argument, but args_on_stack=0 and atomp(args)
          pushSTACK(unbound);
          if (!nullp(args)) goto fehler_dotted;
          goto apply_cclosure_nokey;
        case (uintB)cclos_argtype_4_0_rest:
          # 4 required-Arguments, Rest-Parameter
          REQ_ARG();
        case (uintB)cclos_argtype_3_0_rest:
          # 3 required-Arguments, Rest-Parameter
          REQ_ARG();
        case (uintB)cclos_argtype_2_0_rest:
          # 2 required-Arguments, Rest-Parameter
          REQ_ARG();
        case (uintB)cclos_argtype_1_0_rest:
          # 1 required-Argument, Rest-Parameter
          REQ_ARG();
        case (uintB)cclos_argtype_0_0_rest:
          # no Arguments, Rest-Parameter
          goto apply_cclosure_rest_nokey;
        case (uintB)cclos_argtype_4_0_key:
          # 4 required-Arguments, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_3_0_key:
          # 3 required-Arguments, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_2_0_key:
          # 2 required-Arguments, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_1_0_key:
          # 1 required-Argument, Keyword-Arguments
          REQ_ARG();
          noch_0_opt_args_key:
        case (uintB)cclos_argtype_0_0_key:
          # only Keyword-Arguments
          if ((args_on_stack==0) && atomp(args)) goto unbound_optional_key_0;
          goto apply_cclosure_key_withlist;
        case (uintB)cclos_argtype_3_1_key:
          # 3 required-Arguments and 1 optional-Argument, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_2_1_key:
          # 2 required-Arguments and 1 optional-Argument, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_1_1_key:
          # 1 required-Argument and 1 optional-Argument, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_0_1_key:
          # 1 optional-Argument, Keyword-Arguments
          noch_1_opt_args_key:
          OPT_ARG(key_1);
          goto noch_0_opt_args_key;
        case (uintB)cclos_argtype_2_2_key:
          # 2 required-Arguments and 2 optional-Arguments, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_1_2_key:
          # 1 required-Argument and 2 optional-Arguments, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_0_2_key:
          # 2 optional-Arguments, Keyword-Arguments
          noch_2_opt_args_key:
          OPT_ARG(key_2);
          goto noch_1_opt_args_key;
        case (uintB)cclos_argtype_1_3_key:
          # 1 required-Argument and 3 optional-Arguments, Keyword-Arguments
          REQ_ARG();
        case (uintB)cclos_argtype_0_3_key:
          # 3 optional-Arguments, Keyword-Arguments
          noch_3_opt_args_key:
          OPT_ARG(key_3);
          goto noch_2_opt_args_key;
        case (uintB)cclos_argtype_0_4_key:
          # 4 optional-Arguments, Keyword-Arguments
          OPT_ARG(key_4);
          goto noch_3_opt_args_key;
        unbound_optional_key_4: # Still 4 optional Arguments, but args_on_stack=0 and atomp(args)
          pushSTACK(unbound);
        unbound_optional_key_3: # Still 3 optional Arguments, but args_on_stack=0 and atomp(args)
          pushSTACK(unbound);
        unbound_optional_key_2: # Still 2 optional Arguments, but args_on_stack=0 and atomp(args)
          pushSTACK(unbound);
        unbound_optional_key_1: # Still 1 optional Argument, but args_on_stack=0 and atomp(args)
          pushSTACK(unbound);
        unbound_optional_key_0: # Before the Keywords is args_on_stack=0 and atomp(args)
          if (!nullp(args)) goto fehler_dotted;
          goto apply_cclosure_key_noargs;
        case (uintB)cclos_argtype_default:
          # General Version
          break;
        default: NOTREACHED;
        #undef OPT_ARG
        #undef REQ_ARG
      }
      # Now the general Version:
      {
        var uintB flags;
        {
          var uintC req_anz = TheCodevec(codevec)->ccv_numreq; # number of required Parameters
          var uintC opt_anz = TheCodevec(codevec)->ccv_numopt; # number of optional Parameters
          flags = TheCodevec(codevec)->ccv_flags; # Flags
          if (args_on_stack < req_anz) {
            # fewer Arguments than demanded
            req_anz = req_anz - args_on_stack; # as many as these must on STACK
            # reserve space on STACK:
            get_space_on_STACK(sizeof(gcv_object_t) * (uintL)(req_anz + opt_anz));
            # store required Parameters on Stack:
            {
              var uintC count;
              dotimespC(count,req_anz, {
                if (atomp(args))
                  goto fehler_zuwenig;
                pushSTACK(Car(args)); # store next argument
                args = Cdr(args);
              });
            }
            goto optionals_from_list;
          }
          args_on_stack -= req_anz; # remaining number
          if (args_on_stack < opt_anz) {
            # Argumente in Stack don't last for the optional ones
            opt_anz = opt_anz - args_on_stack; # as many as these must go on STACK
            # reserve space on STACK:
            get_space_on_STACK(sizeof(gcv_object_t) * (uintL)opt_anz);
            optionals_from_list:
            { /* store optional parameters on Stack: */
              var uintC count = opt_anz;
              while (!atomp(args)) { /* argument-list not finished? */
                if (count==0) # all optional parameters supplied with?
                  goto optionals_ok;
                count--;
                pushSTACK(Car(args)); # store next argument
                args = Cdr(args);
              }
              # argument-list finished.
              if (!nullp(args)) goto fehler_dotted;
              # All further count optional parameters receive the "value"
              # #<UNBOUND>, the &REST-parameter receives NIL,
              # the Keyword-parameters receive the value #<UNBOUND> :
              dotimesC(count,count, { pushSTACK(unbound); } );
            }
            if (flags & bit(0)) # &REST-Flag?
              pushSTACK(NIL); # yes -> initialize with NIL
            if (flags & bit(7)) # &KEY-Flag?
              goto apply_cclosure_key_noargs;
            else
              goto apply_cclosure_nokey;
           optionals_ok:
            # process Rest- and Keyword-parameters.
            # args = remaining argument-list (not yet finished)
            if (flags == 0)
              # Closure without REST or KEY -> argument-list should be finished
              goto fehler_zuviel;
            # poss. fill the Rest-parameter:
            if (flags & bit(0))
              pushSTACK(args);
            if (flags & bit(7)) { # Key-Flag?
              # Closure with Keywords.
              # args = remaining argument-list (not yet finished)
              # First initialize the Keyword-parameters with #<UNBOUND> ,
              # the store the remaining arguments in Stack,
              # then assign the Keywords:
              key_args_pointer = args_end_pointer; # Pointer to the Keyword-parameters
              # initialize all Keyword-parameters with #<UNBOUND> :
              {
                var uintC count = TheCodevec(codevec)->ccv_numkey;
                dotimesC(count,count, { pushSTACK(unbound); } );
              }
              rest_args_pointer = args_end_pointer; # Pointer to the remaining arguments
              argcount = 0; # counter for the remaining arguments
              goto key_from_list;
            } else
              # Closure with only REST, without KEY:
              goto apply_cclosure_nokey;
          }
          args_on_stack -= opt_anz; # remaining number
          if (flags & bit(7)) # Key-Flag?
            goto apply_cclosure_key_withlist_;
          elif (flags & bit(0))
            goto apply_cclosure_rest_nokey;
          else {
            # Closure without REST or KEY
            if ((args_on_stack>0) || consp(args)) # still arguments?
              goto fehler_zuviel;
            goto apply_cclosure_nokey;
          }
        }
       apply_cclosure_key_noargs:
        {
          var uintC key_anz = TheCodevec(codevec)->ccv_numkey; # number of Keyword-parameters
          if (key_anz > 0) {
            get_space_on_STACK(sizeof(gcv_object_t) * (uintL)key_anz);
            var uintC count;
            dotimespC(count,key_anz, { pushSTACK(unbound); } ); # initialize with #<UNBOUND>
          }
          goto apply_cclosure_key;
        }
       apply_cclosure_key_withlist:
        flags = TheCodevec(codevec)->ccv_flags; # initialize flags!
       apply_cclosure_key_withlist_:
        # Closure with Keywords
        {
          var uintC key_anz = TheCodevec(codevec)->ccv_numkey; # number of Keyword-parameters
          # shift down remaining arguments in STACK and thus
          # create room for the Keyword-parameters
          # (and poss. Rest-parameters):
          var uintL shift = key_anz;
          if (flags & bit(0))
            shift++; # poss. 1 more for Rest-Parameter
          argcount = args_on_stack;
          get_space_on_STACK(sizeof(gcv_object_t) * shift);
          var gcv_object_t* new_args_end_pointer = args_end_pointer STACKop -(uintP)shift;
          var gcv_object_t* ptr1 = args_end_pointer;
          var gcv_object_t* ptr2 = new_args_end_pointer;
          var uintC count;
          dotimesC(count,args_on_stack, { BEFORE(ptr2) = BEFORE(ptr1); } );
          if (flags & bit(0))
            NEXT(ptr1) = args; # Rest-Parameter (preliminary)
          key_args_pointer = ptr1;
          rest_args_pointer = ptr2;
          dotimesC(count,key_anz, { NEXT(ptr1) = unbound; } );
          set_args_end_pointer(new_args_end_pointer);
          if (flags & bit(0))
            # fill Rest-Parameter, less effort than with match_cclosure_key:
            if (args_on_stack > 0) {
              var gcv_object_t* ptr3 = new_args_end_pointer;
              pushSTACK(closure); # save Closure
              pushSTACK(args); # save args
              dotimespC(count,args_on_stack, {
                var object new_cons = allocate_cons();
                Car(new_cons) = BEFORE(ptr3);
                Cdr(new_cons) = Before(key_args_pointer);
                Before(key_args_pointer) = new_cons;
              });
              args = popSTACK();
              closure = popSTACK();
            }
        }
       key_from_list: # remove remaining arguments for Keywords from list
        while (consp(args)) {
          check_STACK(); pushSTACK(Car(args)); # store next argument in Stack
          args = Cdr(args);
          argcount++;
        }
        # argument-list finished.
        if (!nullp(args)) goto fehler_dotted;
        # assign Keywords, build Rest-parameter
        # and poss. discard remaining arguments:
        closure = match_cclosure_key(closure,argcount,key_args_pointer,rest_args_pointer);
        codevec = TheCclosure(closure)->clos_codevec;
       apply_cclosure_key:
        interpret_bytecode(closure,codevec,CCV_START_KEY); # process Bytecode starting at Byte 12
        goto done;
      }
     apply_cclosure_rest_nokey:
      # Closure with only REST, without KEY:
      # still has to cons args_on_stack Arguments from Stack to args:
      pushSTACK(args);
      if (args_on_stack > 0) {
        pushSTACK(closure); # Closure must be saved
        dotimespC(args_on_stack,args_on_stack, {
          var object new_cons = allocate_cons();
          Cdr(new_cons) = STACK_1;
          Car(new_cons) = STACK_2; # cons next argument to it
          STACK_2 = new_cons;
          STACK_1 = STACK_0; skipSTACK(1);
        });
        closure = popSTACK(); codevec = TheCclosure(closure)->clos_codevec;
      }
      goto apply_cclosure_nokey;
     apply_cclosure_nokey: # jump to Closure without &KEY:
      interpret_bytecode(closure,codevec,CCV_START_NONKEY); # process Bytecode starting at Byte 8
     done:
      #if STACKCHECKC
      if (!(args_pointer == args_end_pointer)) # Stack cleaned up?
        abort(); # no -> go to Debugger
      #endif
      return; # finished
    } else {
      # closure is an interpreted Closure
      # reserve space on STACK:
      get_space_on_STACK(sizeof(gcv_object_t) * llength(args));
      while (consp(args)) { # Still Arguments in list?
        pushSTACK(Car(args)); # push next Element in STACK
        args = Cdr(args);
        args_on_stack += 1;
        if (((uintL)~(uintL)0 > ca_limit_1) && (args_on_stack > ca_limit_1))
          goto fehler_zuviel;
      }
      var gcv_object_t* args_pointer = args_end_pointer STACKop args_on_stack;
      with_saved_back_trace_iclosure(closure,args_pointer,args_on_stack,
        funcall_iclosure(closure,args_pointer,args_on_stack); );
      return; # finished
    }
    # Gathered error-messages:
   fehler_zuwenig: fehler_closure_zuwenig(closure,args);
   fehler_zuviel: fehler_closure_zuviel(closure);
   fehler_dotted: fehler_apply_dotted(closure,args);
  }


#        ----------------------- F U N C A L L -----------------------

# later:
local Values funcall_subr (object fun, uintC args_on_stack);
local Values funcall_closure (object fun, uintC args_on_stack);

/* UP: Applies a function to its arguments.
 funcall(function,argcount);
 > function: function
 > Argumente: argcount arguments on STACK
 < STACK: cleaned up (i.e. STACK is increased by argcount)
 < mv_count/mv_space: values
 changes STACK, can trigger GC */
global maygc Values funcall (object fun, uintC args_on_stack)
{
 funcall_restart:
  /* fun must be a SUBR or a Closure or a Cons (LAMBDA ...) : */
  if (subrp(fun)) { /* SUBR ? */
    return_Values funcall_subr(fun,args_on_stack);
  } else if (closurep(fun)) { /* Closure ? */
    return_Values funcall_closure(fun,args_on_stack);
  } else if (symbolp(fun)) { /* Symbol ? */
    /* apply Symbol: global Definition Symbol_function(fun) applies. */
    var object fdef = Symbol_function(fun);
    if (subrp(fdef)) { /* SUBR -> apply */
      return_Values funcall_subr(fdef,args_on_stack);
    } else if (closurep(fdef)) { /* Closure -> apply */
      return_Values funcall_closure(fdef,args_on_stack);
    } else if (orecordp(fdef)) {
     #ifdef DYNAMIC_FFI
      if (ffunctionp(fdef)) { /* Foreign-Function ? */
        fun = fdef; goto call_ffunction;
      }
     #endif
      switch (Record_type(fdef)) {
        case Rectype_Fsubr:
          fehler_specialform(S(funcall),fun);
        case Rectype_Macro:
          fehler_macro(S(funcall),fun);
        default: NOTREACHED;
      }
    } else
      /* if no SUBR, no Closure, no FSUBR, no Macro:
         Symbol_function(fun) must be #<UNBOUND> . */
      goto undef;
  } else if (funnamep(fun)) { /* list (SETF symbol) ? */
    /* global definition (symbol-function (get-setf-symbol symbol)) applies. */
    var object symbol = get(Car(Cdr(fun)),S(setf_function)); /* (get ... 'SYS::SETF-FUNCTION) */
    if (!symbolp(symbol)) /* should be (uninterned) symbol */
      goto undef; /* else undefed */
    var object fdef = Symbol_function(symbol);
    if (closurep(fdef)) { /* Closure -> apply */
      return_Values funcall_closure(fdef,args_on_stack);
    } else if (subrp(fdef)) { /* SUBR -> apply */
      return_Values funcall_subr(fdef,args_on_stack);
    }
   #ifdef DYNAMIC_FFI
    else if (ffunctionp(fdef)) { /* Foreign-Function ? */
      fun = fdef; goto call_ffunction;
    }
   #endif
    else
      /* Such function-names cannot denote FSUBRs or Macros.
         fdef is presumable #<UNBOUND> . */
      goto undef;
  }
 #ifdef DYNAMIC_FFI
  else if (ffunctionp(fun)) /* Foreign-Function ? */
  call_ffunction: { /* call (SYS::FOREIGN-CALL-OUT foreign-function . args) */
    /* First shift down the arguments in Stack by 1. */
    var uintC count;
    var gcv_object_t* ptr = &STACK_0;
    dotimesC(count,args_on_stack, {
      *(ptr STACKop -1) = *ptr; ptr skipSTACKop 1;
    });
    *(ptr STACKop -1) = fun;
    skipSTACK(-1);
    return_Values funcall_subr(L(foreign_call_out),args_on_stack+1);
  }
 #endif
  else if (consp(fun) && eq(Car(fun),S(lambda))) /* Cons (LAMBDA ...) ? */
    fehler_lambda_expression(S(funcall),fun);
  else {
    fun = check_funname_replacement(type_error,S(funcall),fun);
    goto funcall_restart;
  }
  return;
 undef:
  fun = check_fdefinition(fun,S(funcall));
  goto funcall_restart;
}

# In FUNCALL: Applies a SUBR to arguments, cleans up STACK
# and returns the values.
# funcall_subr(fun,args_on_stack);
# > fun: function, a SUBR
# > Arguments: args_on_stack arguments on STACK
# < STACK: cleaned up (i.e. STACK is increased by args_on_stack)
# < mv_count/mv_space: values
# changes STACK, can trigger GC
  local maygc Values funcall_subr (object fun, uintC args_on_stack)
  {
    #if STACKCHECKS
    var gcv_object_t* args_pointer = args_end_pointer STACKop args_on_stack; # Pointer to the arguments
    #endif
    var gcv_object_t* key_args_pointer; # Pointer to the Keyword-arguments
    var gcv_object_t* rest_args_pointer; # Pointer to the remaining arguments
    var uintL argcount; # number of remaining arguments
    TRACE_CALL(fun,'F','S');
    # store arguments in STACK:
    # First a Dispatch for the most important cases:
    switch (TheSubr(fun)->argtype) {
      case (uintW)subr_argtype_0_0:
        # SUBR without Arguments
        if (!(args_on_stack==0)) goto fehler_zuviel;
        goto apply_subr_norest;
      case (uintW)subr_argtype_1_0:
        # SUBR with 1 required-Argument
        if (!(args_on_stack==1)) goto fehler_anzahl;
        goto apply_subr_norest;
      case (uintW)subr_argtype_2_0:
        # SUBR with 2 required-Arguments
        if (!(args_on_stack==2)) goto fehler_anzahl;
        goto apply_subr_norest;
      case (uintW)subr_argtype_3_0:
        # SUBR with 3 required-Arguments
        if (!(args_on_stack==3)) goto fehler_anzahl;
        goto apply_subr_norest;
      case (uintW)subr_argtype_4_0:
        # SUBR with 4 required-Arguments
        if (!(args_on_stack==4)) goto fehler_anzahl;
        goto apply_subr_norest;
      case (uintW)subr_argtype_5_0:
        # SUBR with 5 required-Arguments
        if (!(args_on_stack==5)) goto fehler_anzahl;
        goto apply_subr_norest;
      case (uintW)subr_argtype_6_0:
        # SUBR with 6 required-Arguments
        if (!(args_on_stack==6)) goto fehler_anzahl;
        goto apply_subr_norest;
      case (uintW)subr_argtype_0_1:
        # SUBR with 1 optional-Argument
        if (args_on_stack==1) goto apply_subr_norest;
        elif (args_on_stack>1) goto fehler_zuviel;
        else { pushSTACK(unbound); goto apply_subr_norest; }
      case (uintW)subr_argtype_1_1:
        # SUBR with 1 required-Argument and 1 optional-Argument
        if (args_on_stack==2) goto apply_subr_norest;
        elif (args_on_stack>2) goto fehler_zuviel;
        elif (args_on_stack==0) goto fehler_zuwenig;
        else { pushSTACK(unbound); goto apply_subr_norest; }
      case (uintW)subr_argtype_2_1:
        # SUBR with 2 required-Arguments and 1 optional-Argument
        if (args_on_stack==3) goto apply_subr_norest;
        elif (args_on_stack>3) goto fehler_zuviel;
        elif (args_on_stack<2) goto fehler_zuwenig;
        else { pushSTACK(unbound); goto apply_subr_norest; }
      case (uintW)subr_argtype_3_1:
        # SUBR with 3 required-Arguments and 1 optional-Argument
        if (args_on_stack==4) goto apply_subr_norest;
        elif (args_on_stack>4) goto fehler_zuviel;
        elif (args_on_stack<3) goto fehler_zuwenig;
        else { pushSTACK(unbound); goto apply_subr_norest; }
      case (uintW)subr_argtype_4_1:
        # SUBR with 4 required-Arguments and 1 optional-Argument
        if (args_on_stack==5) goto apply_subr_norest;
        elif (args_on_stack>5) goto fehler_zuviel;
        elif (args_on_stack<4) goto fehler_zuwenig;
        else { pushSTACK(unbound); goto apply_subr_norest; }
      case (uintW)subr_argtype_0_2:
        # SUBR with 2 optional-Arguments
        switch (args_on_stack) {
          case 0: pushSTACK(unbound);
          case 1: pushSTACK(unbound);
          case 2: goto apply_subr_norest;
          default: goto fehler_zuviel;
        }
      case (uintW)subr_argtype_1_2:
        # SUBR with 1 required-Argument and 2 optional-Arguments
        switch (args_on_stack) {
          case 0: goto fehler_zuwenig;
          case 1: pushSTACK(unbound);
          case 2: pushSTACK(unbound);
          case 3: goto apply_subr_norest;
          default: goto fehler_zuviel;
        }
      case (uintW)subr_argtype_2_2:
        # SUBR with 2 required-Arguments and 2 optional-Arguments
        switch (args_on_stack) {
          case 0: goto fehler_zuwenig;
          case 1: goto fehler_zuwenig;
          case 2: pushSTACK(unbound);
          case 3: pushSTACK(unbound);
          case 4: goto apply_subr_norest;
          default: goto fehler_zuviel;
        }
      case (uintW)subr_argtype_3_2:
        # SUBR with 3 required-Arguments and 2 optional-Arguments
        switch (args_on_stack) {
          case 0: goto fehler_zuwenig;
          case 1: goto fehler_zuwenig;
          case 2: goto fehler_zuwenig;
          case 3: pushSTACK(unbound);
          case 4: pushSTACK(unbound);
          case 5: goto apply_subr_norest;
          default: goto fehler_zuviel;
        }
      case (uintW)subr_argtype_0_3:
        # SUBR with 3 optional-Arguments
        switch (args_on_stack) {
          case 0: pushSTACK(unbound);
          case 1: pushSTACK(unbound);
          case 2: pushSTACK(unbound);
          case 3: goto apply_subr_norest;
          default: goto fehler_zuviel;
        }
      case (uintW)subr_argtype_1_3:
        # SUBR with 1 required-Argument and 3 optional-Arguments
        switch (args_on_stack) {
          case 0: goto fehler_zuwenig;
          case 1: pushSTACK(unbound);
          case 2: pushSTACK(unbound);
          case 3: pushSTACK(unbound);
          case 4: goto apply_subr_norest;
          default: goto fehler_zuviel;
        }
      case (uintW)subr_argtype_2_3:
        # SUBR with 2 required-Arguments and 3 optional-Arguments
        switch (args_on_stack) {
          case 0: goto fehler_zuwenig;
          case 1: goto fehler_zuwenig;
          case 2: pushSTACK(unbound);
          case 3: pushSTACK(unbound);
          case 4: pushSTACK(unbound);
          case 5: goto apply_subr_norest;
          default: goto fehler_zuviel;
        }
      case (uintW)subr_argtype_0_4:
        # SUBR with 4 optional-Arguments
        switch (args_on_stack) {
          case 0: pushSTACK(unbound);
          case 1: pushSTACK(unbound);
          case 2: pushSTACK(unbound);
          case 3: pushSTACK(unbound);
          case 4: goto apply_subr_norest;
          default: goto fehler_zuviel;
        }
      case (uintW)subr_argtype_0_5:
        # SUBR with 5 optional-Arguments
        switch (args_on_stack) {
          case 0: pushSTACK(unbound);
          case 1: pushSTACK(unbound);
          case 2: pushSTACK(unbound);
          case 3: pushSTACK(unbound);
          case 4: pushSTACK(unbound);
          case 5: goto apply_subr_norest;
          default: goto fehler_zuviel;
        }
      case (uintW)subr_argtype_0_0_rest:
        # SUBR with further Arguments
        goto apply_subr_rest_ok;
      case (uintW)subr_argtype_1_0_rest:
        # SUBR with 1 required-Argument and further Arguments
        if (args_on_stack==0) goto fehler_zuwenig;
        args_on_stack -= 1;
        goto apply_subr_rest_ok;
      case (uintW)subr_argtype_2_0_rest:
        # SUBR with 2 required-Argumenten and further Arguments
        if (args_on_stack<2) goto fehler_zuwenig;
        args_on_stack -= 2;
        goto apply_subr_rest_ok;
      case (uintW)subr_argtype_3_0_rest:
        # SUBR with 3 required-Argumenten and further Arguments
        if (args_on_stack<3) goto fehler_zuwenig;
        args_on_stack -= 3;
        goto apply_subr_rest_ok;
      case (uintW)subr_argtype_0_0_key:
        # SUBR with Keyword-Arguments
        if (args_on_stack==0) goto unbound_optional_key_0;
        else goto apply_subr_key;
      case (uintW)subr_argtype_1_0_key:
        # SUBR with 1 required-Argument and Keyword-Arguments
        if (args_on_stack==1) goto unbound_optional_key_0;
        elif (args_on_stack<1) goto fehler_zuwenig;
        else { args_on_stack -= 1; goto apply_subr_key; }
      case (uintW)subr_argtype_2_0_key:
        # SUBR with 2 required-Arguments and Keyword-Arguments
        if (args_on_stack==2) goto unbound_optional_key_0;
        elif (args_on_stack<2) goto fehler_zuwenig;
        else { args_on_stack -= 2; goto apply_subr_key; }
      case (uintW)subr_argtype_3_0_key:
        # SUBR with 3 required-Arguments and Keyword-Arguments
        if (args_on_stack==3) goto unbound_optional_key_0;
        elif (args_on_stack<3) goto fehler_zuwenig;
        else { args_on_stack -= 3; goto apply_subr_key; }
      case (uintW)subr_argtype_4_0_key:
        # SUBR with 4 required-Arguments and Keyword-Arguments
        if (args_on_stack==4) goto unbound_optional_key_0;
        elif (args_on_stack<4) goto fehler_zuwenig;
        else { args_on_stack -= 4; goto apply_subr_key; }
      case (uintW)subr_argtype_0_1_key:
        # SUBR with 1 optional-Argument and Keyword-Arguments
        switch (args_on_stack) {
          case 0: goto unbound_optional_key_1;
          case 1: goto unbound_optional_key_0;
          default: args_on_stack -= 1; goto apply_subr_key;
        }
      case (uintW)subr_argtype_1_1_key:
        # SUBR with 1 required-Argument, 1 optional-Argument and Keyword-Arguments
        switch (args_on_stack) {
          case 0: goto fehler_zuwenig;
          case 1: goto unbound_optional_key_1;
          case 2: goto unbound_optional_key_0;
          default: args_on_stack -= 2; goto apply_subr_key;
        }
      case (uintW)subr_argtype_1_2_key:
        # SUBR with 1 required-Argument, 2 optional-Arguments and Keyword-Arguments
        switch (args_on_stack) {
          case 0: goto fehler_zuwenig;
          case 1: goto unbound_optional_key_2;
          case 2: goto unbound_optional_key_1;
          case 3: goto unbound_optional_key_0;
          default: args_on_stack -= 3; goto apply_subr_key;
        }
      unbound_optional_key_2: # Still 2 optional Arguments, but args_on_stack=0
        pushSTACK(unbound);
      unbound_optional_key_1: # Still 1 optional Argument, but args_on_stack=0
        pushSTACK(unbound);
      unbound_optional_key_0: # Before the Keywords is args_on_stack=0
        {
          var uintC count;
          dotimesC(count,TheSubr(fun)->key_anz, { pushSTACK(unbound); } );
        }
        goto apply_subr_norest;
      default: NOTREACHED;
      #undef OPT_ARG
      #undef REQ_ARG
    }
    # Now the general Version:
    {
      var uintC key_anz;
      {
        var uintC req_anz = TheSubr(fun)->req_anz;
        var uintC opt_anz = TheSubr(fun)->opt_anz;
        key_anz = TheSubr(fun)->key_anz;
        if (args_on_stack < req_anz)
          # fewer Arguments than demanded
          goto fehler_zuwenig;
        args_on_stack -= req_anz; # remaining number
        if (args_on_stack <= opt_anz) {
          # Arguments in Stack don't last for the optional ones
          opt_anz = opt_anz - args_on_stack; # as many as these must go on STACK
          if (opt_anz + key_anz > 0) {
            # reserve space on STACK:
            get_space_on_STACK(sizeof(gcv_object_t) * (uintL)(opt_anz + key_anz));
            # All further count optional parameters receive the "value"
            # #<UNBOUND>, including the Keyword-parameters:
            var uintC count;
            dotimespC(count,opt_anz + key_anz, { pushSTACK(unbound); } );
          }
          if (TheSubr(fun)->rest_flag == subr_rest) { # &REST-Flag?
            # yes -> 0 additional Arguments:
            argcount = 0; rest_args_pointer = args_end_pointer;
            goto apply_subr_rest;
          } else {
            # no -> nothing to do
            goto apply_subr_norest;
          }
        }
        args_on_stack -= opt_anz; # remaining number (> 0)
        if (TheSubr(fun)->key_flag == subr_nokey) {
          # SUBR without KEY
          if (TheSubr(fun)->rest_flag == subr_norest)
            # SUBR without REST or KEY
            goto fehler_zuviel; # still Arguments!
          else
            # SUBR with only REST, without KEY
            goto apply_subr_rest_ok;
        } else
          # SUBR with Keywords.
          goto apply_subr_key_;
      }
     apply_subr_key:
      key_anz = TheSubr(fun)->key_anz;
     apply_subr_key_:
      # shift down remaining arguments in STACK and thus
      # create room for the Keyword-parameters:
      argcount = args_on_stack; # (> 0)
      get_space_on_STACK(sizeof(gcv_object_t) * (uintL)key_anz);
      {
        var gcv_object_t* new_args_end_pointer = args_end_pointer STACKop -(uintP)key_anz;
        var gcv_object_t* ptr1 = args_end_pointer;
        var gcv_object_t* ptr2 = new_args_end_pointer;
        var uintC count;
        dotimespC(count,args_on_stack, { BEFORE(ptr2) = BEFORE(ptr1); } );
        key_args_pointer = ptr1;
        rest_args_pointer = ptr2;
        dotimesC(count,key_anz, { NEXT(ptr1) = unbound; } );
        set_args_end_pointer(new_args_end_pointer);
      }
      # assign Keywords and poss. discard remaining Arguments:
      match_subr_key(fun,argcount,key_args_pointer,rest_args_pointer);
      if (TheSubr(fun)->rest_flag != subr_norest)
        # SUBR with &REST-Flag:
        goto apply_subr_rest;
      else
        # SUBR without &REST-Flag:
        goto apply_subr_norest;
    }
   apply_subr_rest_ok:
    argcount = args_on_stack;
    rest_args_pointer = args_end_pointer STACKop argcount;
   apply_subr_rest:
    with_saved_back_trace_subr(fun,STACK,
                               TheSubr(fun)->req_anz + TheSubr(fun)->opt_anz + argcount,
      (*(subr_rest_function_t*)(TheSubr(fun)->function))(argcount,rest_args_pointer); );
    goto done;
   apply_subr_norest:
    with_saved_back_trace_subr(fun,STACK,args_on_stack,
      (*(subr_norest_function_t*)(TheSubr(fun)->function))(); );
   done:
    #if STACKCHECKS
    if (!(args_pointer == args_end_pointer)) # Stack cleaned up?
      abort(); # no -> go to Debugger
    #endif
    return; # finished
    # Gathered error-messages:
   fehler_anzahl:
    if (args_on_stack < TheSubr(fun)->req_anz)
      goto fehler_zuwenig; # too few Arguments
    else
      goto fehler_zuviel; # too many Arguments
   fehler_zuwenig: fehler_subr_zuwenig(fun,NIL);
   fehler_zuviel: fehler_subr_zuviel(fun);
  }

# In FUNCALL: Applies a Closure to Arguments, cleans up STACK
# and returns the values.
# funcall_closure(fun,args_on_stack);
# > fun: function, a Closure
# > Argumente: args_on_stack Arguments on STACK
# < STACK: cleaned up (i.e. STACK is increased by args_on_stack)
# < mv_count/mv_space: values
# changes STACK, can trigger GC
  local maygc Values funcall_closure (object closure, uintC args_on_stack)
  {
    TRACE_CALL(closure,'F','C');
    if (simple_bit_vector_p(Atype_8Bit,TheClosure(closure)->clos_codevec)) {
      # closure is a compiled Closure
      #if STACKCHECKC
      var gcv_object_t* args_pointer = args_end_pointer STACKop args_on_stack; # Pointer to the Arguments
      #endif
      var object codevec = TheCclosure(closure)->clos_codevec; # Code-Vector
      var gcv_object_t* key_args_pointer; # Pointer to the Keyword-Arguments
      var gcv_object_t* rest_args_pointer; # Pointer to the remaining Arguments
      var uintL argcount; # number of remaining Arguments
      check_SP(); check_STACK();
      # store arguments in STACK:
      # First a Dispatch for the most important cases:
      switch (TheCodevec(codevec)->ccv_signature) {
        case (uintB)cclos_argtype_0_0:
          # no Arguments
          if (!(args_on_stack==0)) goto fehler_zuviel;
          goto apply_cclosure_nokey;
        case (uintB)cclos_argtype_1_0:
          # 1 required-Argument
          if (!(args_on_stack==1)) goto fehler_anzahl;
          goto apply_cclosure_nokey;
        case (uintB)cclos_argtype_2_0:
          # 2 required-Arguments
          if (!(args_on_stack==2)) goto fehler_anzahl;
          goto apply_cclosure_nokey;
        case (uintB)cclos_argtype_3_0:
          # 3 required-Arguments
          if (!(args_on_stack==3)) goto fehler_anzahl;
          goto apply_cclosure_nokey;
        case (uintB)cclos_argtype_4_0:
          # 4 required-Arguments
          if (!(args_on_stack==4)) goto fehler_anzahl;
          goto apply_cclosure_nokey;
        case (uintB)cclos_argtype_5_0:
          # 5 required-Arguments
          if (!(args_on_stack==5)) goto fehler_anzahl;
          goto apply_cclosure_nokey;
        case (uintB)cclos_argtype_0_1:
          # 1 optional-Argument
          if (args_on_stack==1) goto apply_cclosure_nokey;
          elif (args_on_stack>1) goto fehler_zuviel;
          else { pushSTACK(unbound); goto apply_cclosure_nokey; }
        case (uintB)cclos_argtype_1_1:
          # 1 required-Argument and 1 optional-Argument
          if (args_on_stack==2) goto apply_cclosure_nokey;
          elif (args_on_stack>2) goto fehler_zuviel;
          elif (args_on_stack==0) goto fehler_zuwenig;
          else { pushSTACK(unbound); goto apply_cclosure_nokey; }
        case (uintB)cclos_argtype_2_1:
          # 2 required-Arguments and 1 optional-Argument
          if (args_on_stack==3) goto apply_cclosure_nokey;
          elif (args_on_stack>3) goto fehler_zuviel;
          elif (args_on_stack<2) goto fehler_zuwenig;
          else { pushSTACK(unbound); goto apply_cclosure_nokey; }
        case (uintB)cclos_argtype_3_1:
          # 3 required-Arguments and 1 optional-Argument
          if (args_on_stack==4) goto apply_cclosure_nokey;
          elif (args_on_stack>4) goto fehler_zuviel;
          elif (args_on_stack<3) goto fehler_zuwenig;
          else { pushSTACK(unbound); goto apply_cclosure_nokey; }
        case (uintB)cclos_argtype_4_1:
          # 4 required-Arguments and 1 optional-Argument
          if (args_on_stack==5) goto apply_cclosure_nokey;
          elif (args_on_stack>5) goto fehler_zuviel;
          elif (args_on_stack<4) goto fehler_zuwenig;
          else { pushSTACK(unbound); goto apply_cclosure_nokey; }
        case (uintB)cclos_argtype_0_2:
          # 2 optional-Arguments
          switch (args_on_stack) {
            case 0: pushSTACK(unbound);
            case 1: pushSTACK(unbound);
            case 2: goto apply_cclosure_nokey;
            default: goto fehler_zuviel;
          }
        case (uintB)cclos_argtype_1_2:
          # 1 required-Argument and 2 optional-Arguments
          switch (args_on_stack) {
            case 0: goto fehler_zuwenig;
            case 1: pushSTACK(unbound);
            case 2: pushSTACK(unbound);
            case 3: goto apply_cclosure_nokey;
            default: goto fehler_zuviel;
          }
        case (uintB)cclos_argtype_2_2:
          # 2 required-Arguments and 2 optional-Arguments
          switch (args_on_stack) {
            case 0: case 1: goto fehler_zuwenig;
            case 2: pushSTACK(unbound);
            case 3: pushSTACK(unbound);
            case 4: goto apply_cclosure_nokey;
            default: goto fehler_zuviel;
          }
        case (uintB)cclos_argtype_3_2:
          # 3 required-Arguments and 2 optional-Arguments
          switch (args_on_stack) {
            case 0: case 1: case 2: goto fehler_zuwenig;
            case 3: pushSTACK(unbound);
            case 4: pushSTACK(unbound);
            case 5: goto apply_cclosure_nokey;
            default: goto fehler_zuviel;
          }
        case (uintB)cclos_argtype_0_3:
          # 3 optional-Arguments
          switch (args_on_stack) {
            case 0: pushSTACK(unbound);
            case 1: pushSTACK(unbound);
            case 2: pushSTACK(unbound);
            case 3: goto apply_cclosure_nokey;
            default: goto fehler_zuviel;
          }
        case (uintB)cclos_argtype_1_3:
          # 1 required-Argument and 3 optional-Arguments
          switch (args_on_stack) {
            case 0: goto fehler_zuwenig;
            case 1: pushSTACK(unbound);
            case 2: pushSTACK(unbound);
            case 3: pushSTACK(unbound);
            case 4: goto apply_cclosure_nokey;
            default: goto fehler_zuviel;
          }
        case (uintB)cclos_argtype_2_3:
          # 2 required-Arguments and 3 optional-Arguments
          switch (args_on_stack) {
            case 0: case 1: goto fehler_zuwenig;
            case 2: pushSTACK(unbound);
            case 3: pushSTACK(unbound);
            case 4: pushSTACK(unbound);
            case 5: goto apply_cclosure_nokey;
            default: goto fehler_zuviel;
          }
        case (uintB)cclos_argtype_0_4:
          # 4 optional-Arguments
          switch (args_on_stack) {
            case 0: pushSTACK(unbound);
            case 1: pushSTACK(unbound);
            case 2: pushSTACK(unbound);
            case 3: pushSTACK(unbound);
            case 4: goto apply_cclosure_nokey;
            default: goto fehler_zuviel;
          }
        case (uintB)cclos_argtype_1_4:
          # 1 required-Argument and 4 optional-Arguments
          switch (args_on_stack) {
            case 0: goto fehler_zuwenig;
            case 1: pushSTACK(unbound);
            case 2: pushSTACK(unbound);
            case 3: pushSTACK(unbound);
            case 4: pushSTACK(unbound);
            case 5: goto apply_cclosure_nokey;
            default: goto fehler_zuviel;
          }
        case (uintB)cclos_argtype_0_5:
          # 5 optional-Arguments
          switch (args_on_stack) {
            case 0: pushSTACK(unbound);
            case 1: pushSTACK(unbound);
            case 2: pushSTACK(unbound);
            case 3: pushSTACK(unbound);
            case 4: pushSTACK(unbound);
            case 5: goto apply_cclosure_nokey;
            default: goto fehler_zuviel;
          }
        case (uintB)cclos_argtype_0_0_rest:
          # no Arguments, Rest-Parameter
          goto apply_cclosure_rest_nokey;
        case (uintB)cclos_argtype_1_0_rest:
          # 1 required-Argument, Rest-Parameter
          if (args_on_stack==0) goto fehler_zuwenig;
          args_on_stack -= 1;
          goto apply_cclosure_rest_nokey;
        case (uintB)cclos_argtype_2_0_rest:
          # 2 required-Arguments, Rest-Parameter
          if (args_on_stack<2) goto fehler_zuwenig;
          args_on_stack -= 2;
          goto apply_cclosure_rest_nokey;
        case (uintB)cclos_argtype_3_0_rest:
          # 3 required-Arguments, Rest-Parameter
          if (args_on_stack<3) goto fehler_zuwenig;
          args_on_stack -= 3;
          goto apply_cclosure_rest_nokey;
        case (uintB)cclos_argtype_4_0_rest:
          # 4 required-Arguments, Rest-Parameter
          if (args_on_stack<4) goto fehler_zuwenig;
          args_on_stack -= 4;
          goto apply_cclosure_rest_nokey;
        case (uintB)cclos_argtype_0_0_key:
          # only Keyword-Arguments
          if (args_on_stack==0) goto unbound_optional_key_0;
          else goto apply_cclosure_key_withargs;
        case (uintB)cclos_argtype_1_0_key:
          # 1 required-Argument, Keyword-Arguments
          if (args_on_stack==1) goto unbound_optional_key_0;
          elif (args_on_stack<1) goto fehler_zuwenig;
          else { args_on_stack -= 1; goto apply_cclosure_key_withargs; }
        case (uintB)cclos_argtype_2_0_key:
          # 2 required-Arguments, Keyword-Arguments
          if (args_on_stack==2) goto unbound_optional_key_0;
          elif (args_on_stack<2) goto fehler_zuwenig;
          else { args_on_stack -= 2; goto apply_cclosure_key_withargs; }
        case (uintB)cclos_argtype_3_0_key:
          # 3 required-Arguments, Keyword-Arguments
          if (args_on_stack==3) goto unbound_optional_key_0;
          elif (args_on_stack<3) goto fehler_zuwenig;
          else { args_on_stack -= 3; goto apply_cclosure_key_withargs; }
        case (uintB)cclos_argtype_4_0_key:
          # 4 required-Arguments, Keyword-Arguments
          if (args_on_stack==4) goto unbound_optional_key_0;
          elif (args_on_stack<4) goto fehler_zuwenig;
          else { args_on_stack -= 4; goto apply_cclosure_key_withargs; }
        case (uintB)cclos_argtype_0_1_key:
          # 1 optional-Argument, Keyword-Arguments
          switch (args_on_stack) {
            case 0: goto unbound_optional_key_1;
            case 1: goto unbound_optional_key_0;
            default: args_on_stack -= 1; goto apply_cclosure_key_withargs;
          }
        case (uintB)cclos_argtype_1_1_key:
          # 1 required-Argument and 1 optional-Argument, Keyword-Arguments
          switch (args_on_stack) {
            case 0: goto fehler_zuwenig;
            case 1: goto unbound_optional_key_1;
            case 2: goto unbound_optional_key_0;
            default: args_on_stack -= 2; goto apply_cclosure_key_withargs;
          }
        case (uintB)cclos_argtype_2_1_key:
          # 2 required-Arguments and 1 optional-Argument, Keyword-Arguments
          switch (args_on_stack) {
            case 0: case 1: goto fehler_zuwenig;
            case 2: goto unbound_optional_key_1;
            case 3: goto unbound_optional_key_0;
            default: args_on_stack -= 3; goto apply_cclosure_key_withargs;
          }
        case (uintB)cclos_argtype_3_1_key:
          # 3 required-Arguments and 1 optional-Argument, Keyword-Arguments
          switch (args_on_stack) {
            case 0: case 1: case 2: goto fehler_zuwenig;
            case 3: goto unbound_optional_key_1;
            case 4: goto unbound_optional_key_0;
            default: args_on_stack -= 4; goto apply_cclosure_key_withargs;
          }
        case (uintB)cclos_argtype_0_2_key:
          # 2 optional-Arguments, Keyword-Arguments
          switch (args_on_stack) {
            case 0: goto unbound_optional_key_2;
            case 1: goto unbound_optional_key_1;
            case 2: goto unbound_optional_key_0;
            default: args_on_stack -= 2; goto apply_cclosure_key_withargs;
          }
        case (uintB)cclos_argtype_1_2_key:
          # 1 required-Argument and 2 optional-Arguments, Keyword-Arguments
          switch (args_on_stack) {
            case 0: goto fehler_zuwenig;
            case 1: goto unbound_optional_key_2;
            case 2: goto unbound_optional_key_1;
            case 3: goto unbound_optional_key_0;
            default: args_on_stack -= 3; goto apply_cclosure_key_withargs;
          }
        case (uintB)cclos_argtype_2_2_key:
          # 2 required-Arguments and 2 optional-Arguments, Keyword-Arguments
          switch (args_on_stack) {
            case 0: case 1: goto fehler_zuwenig;
            case 2: goto unbound_optional_key_2;
            case 3: goto unbound_optional_key_1;
            case 4: goto unbound_optional_key_0;
            default: args_on_stack -= 4; goto apply_cclosure_key_withargs;
          }
        case (uintB)cclos_argtype_0_3_key:
          # 3 optional-Arguments, Keyword-Arguments
          switch (args_on_stack) {
            case 0: goto unbound_optional_key_3;
            case 1: goto unbound_optional_key_2;
            case 2: goto unbound_optional_key_1;
            case 3: goto unbound_optional_key_0;
            default: args_on_stack -= 3; goto apply_cclosure_key_withargs;
          }
        case (uintB)cclos_argtype_1_3_key:
          # 1 required-Argument and 3 optional-Arguments, Keyword-Arguments
          switch (args_on_stack) {
            case 0: goto fehler_zuwenig;
            case 1: goto unbound_optional_key_3;
            case 2: goto unbound_optional_key_2;
            case 3: goto unbound_optional_key_1;
            case 4: goto unbound_optional_key_0;
            default: args_on_stack -= 4; goto apply_cclosure_key_withargs;
          }
        case (uintB)cclos_argtype_0_4_key:
          # 4 optional-Arguments, Keyword-Arguments
          switch (args_on_stack) {
            case 0: goto unbound_optional_key_4;
            case 1: goto unbound_optional_key_3;
            case 2: goto unbound_optional_key_2;
            case 3: goto unbound_optional_key_1;
            case 4: goto unbound_optional_key_0;
            default: args_on_stack -= 4; goto apply_cclosure_key_withargs;
          }
        unbound_optional_key_4: # Still 4 optional Arguments, but args_on_stack=0
          pushSTACK(unbound);
        unbound_optional_key_3: # Still 3 optional Arguments, but args_on_stack=0
          pushSTACK(unbound);
        unbound_optional_key_2: # Still 2 optional Arguments, but args_on_stack=0
          pushSTACK(unbound);
        unbound_optional_key_1: # Still 1 optional Argument, but args_on_stack=0
          pushSTACK(unbound);
        unbound_optional_key_0: # Before the Keywords is args_on_stack=0
          goto apply_cclosure_key_noargs;
        case (uintB)cclos_argtype_default:
          # General Version
          break;
        default: NOTREACHED;
      }
      # Now the general version:
      {
        var uintB flags;
        {
          var uintC req_anz = TheCodevec(codevec)->ccv_numreq; # number of required Parameters
          var uintC opt_anz = TheCodevec(codevec)->ccv_numopt; # number of optional Parameters
          flags = TheCodevec(codevec)->ccv_flags; # Flags
          if (args_on_stack < req_anz)
            # fewer Arguments than demanded
            goto fehler_zuwenig;
          args_on_stack -= req_anz; # remaining number
          if (args_on_stack <= opt_anz) {
            # Arguments in Stack don't last for the optional ones
            opt_anz = opt_anz - args_on_stack; # as many as these must go on STACK
            if (opt_anz > 0) {
              # reserve space on STACK:
              get_space_on_STACK(sizeof(gcv_object_t) * (uintL)opt_anz);
              # All further count optional parameters receive the "value"
              # #<UNBOUND>, the &REST-parameter receives NIL,
              # the Keyword-parameters receive the value #<UNBOUND> :
              var uintC count;
              dotimespC(count,opt_anz, { pushSTACK(unbound); } );
            }
            if (flags & bit(0)) # &REST-Flag?
              pushSTACK(NIL); # yes -> initialize with NIL
            if (flags & bit(7)) # &KEY-Flag?
              goto apply_cclosure_key_noargs;
            else
              goto apply_cclosure_nokey;
          }
          args_on_stack -= opt_anz; # remaining number
          if (flags & bit(7)) # Key-Flag?
            goto apply_cclosure_key_withargs_;
          elif (flags & bit(0))
            goto apply_cclosure_rest_nokey;
          else {
            # Closure without REST or KEY
            if (args_on_stack>0) # still arguments?
              goto fehler_zuviel;
            goto apply_cclosure_nokey;
          }
        }
       apply_cclosure_key_noargs:
        {
          var uintC key_anz = TheCodevec(codevec)->ccv_numkey; # number of Keyword-Parameters
          if (key_anz > 0) {
            get_space_on_STACK(sizeof(gcv_object_t) * (uintL)key_anz);
            var uintC count;
            dotimespC(count,key_anz, { pushSTACK(unbound); } ); # initialize with #<UNBOUND>
          }
          goto apply_cclosure_key;
        }
       apply_cclosure_key_withargs:
        flags = TheCodevec(codevec)->ccv_flags; # initialize Flags!
       apply_cclosure_key_withargs_:
        # Closure with Keywords
        {
          var uintC key_anz = TheCodevec(codevec)->ccv_numkey; # number of Keyword-Parameters
          # shift down remaining arguments in STACK and thus
          # create room for the Keyword-parameters
          # (and poss. Rest-parameters):
          var uintL shift = key_anz;
          if (flags & bit(0))
            shift++; # poss. 1 more for Rest-Parameter
          argcount = args_on_stack;
          get_space_on_STACK(sizeof(gcv_object_t) * shift);
          var gcv_object_t* new_args_end_pointer = args_end_pointer STACKop -(uintP)shift;
          var gcv_object_t* ptr1 = args_end_pointer;
          var gcv_object_t* ptr2 = new_args_end_pointer;
          var uintC count;
          dotimesC(count,args_on_stack, { BEFORE(ptr2) = BEFORE(ptr1); } );
          if (flags & bit(0))
            NEXT(ptr1) = unbound; # Rest-Parameter
          key_args_pointer = ptr1;
          rest_args_pointer = ptr2;
          dotimesC(count,key_anz, { NEXT(ptr1) = unbound; } );
          set_args_end_pointer(new_args_end_pointer);
        }
        # assign Keywords, build Rest-Parameter
        # and poss. discard remaining arguments:
        closure = match_cclosure_key(closure,argcount,key_args_pointer,rest_args_pointer);
        codevec = TheCclosure(closure)->clos_codevec;
       apply_cclosure_key:
        interpret_bytecode(closure,codevec,CCV_START_KEY); # process Bytecode starting at Byte 12
        goto done;
      }
     apply_cclosure_rest_nokey:
      # Closure with only REST, without KEY:
      # still must cons args_on_stack arguments from stack Stack:
      pushSTACK(NIL);
      if (args_on_stack > 0) {
        pushSTACK(closure); # Closure must be saved
        dotimesC(args_on_stack,args_on_stack, {
          var object new_cons = allocate_cons();
          Cdr(new_cons) = STACK_1;
          Car(new_cons) = STACK_2; # cons next argument to it
          STACK_2 = new_cons;
          STACK_1 = STACK_0; skipSTACK(1);
        });
        closure = popSTACK(); codevec = TheCclosure(closure)->clos_codevec;
      }
     apply_cclosure_nokey: # jump to Closure without &KEY:
      interpret_bytecode(closure,codevec,CCV_START_NONKEY); # process Bytecode starting at Byte 8
     done:
      #if STACKCHECKC
      if (!(args_pointer == args_end_pointer)) # Stack cleaned up?
        abort(); # no -> go to Debugger
      #endif
      return; # finished
      # Gathered error-messages:
     fehler_anzahl:
      if (args_on_stack < TheCodevec(codevec)->ccv_numreq)
        goto fehler_zuwenig; # too few arguments
      else
        goto fehler_zuviel; # too many arguments
     fehler_zuwenig: fehler_closure_zuwenig(closure,NIL);
     fehler_zuviel: fehler_closure_zuviel(closure);
    } else {
      /* closure is an interpreted Closure */
      var gcv_object_t* args_pointer = args_end_pointer STACKop args_on_stack;
      with_saved_back_trace_iclosure(closure,args_pointer,args_on_stack,
        funcall_iclosure(closure,args_pointer,args_on_stack); );
    }
  }


#      ---------------------- BYTECODE-INTERPRETER ----------------------

# Interpretes the bytecode of a compiled Closure.
# interpret_bytecode_(closure,codeptr,byteptr);
# > closure: compiled closure
# > codeptr: its Codevector, a Simple-Bit-Vector, pointable
# > byteptr: Start-Bytecodepointer
# < mv_count/mv_space: values
# changes STACK, can trigger GC
  # Syntax of local labels in GNU-C assembler-statements:
  #if (defined(GNU) || defined(INTEL)) && !defined(NO_ASM)
    # LD(x) defines Label with number x
    # LR(x,f) references label with number x forwards
    # LR(x,b) references label with number x backwards
    # The scope of the labels is only one assembler-statement.
    #if defined(I80386) && !defined(UNIX_NEXTSTEP)
      #ifdef ASM_UNDERSCORE
        #define LD(nr)  "LASM%=X" STRING(nr)
        #define LR(nr,fb)  "LASM%=X" STRING(nr)
      #else
        #define LD(nr)  ".LASM%=X" STRING(nr)
        #define LR(nr,fb)  ".LASM%=X" STRING(nr)
      #endif
    #elif defined(ARM)
      #define LD(nr)  "LASM%=X" STRING(nr)
      #define LR(nr,fb)  "LASM%=X" STRING(nr)
    #else
      #define LD(nr)  STRING(nr)
      #define LR(nr,fb)  STRING(nr) STRING(fb)
    #endif
  #endif
  # Persuade GNU-C, to keep closure and byteptr in registers:
  #ifdef GNU
    #ifdef MC680X0
      #define closure_register  "a2"
      #define byteptr_register  "a3"
    #endif
    #ifdef SPARC
      #define closure_register  "%l0"
      #define byteptr_register  "%l1"
    #endif
    #ifdef I80386
      #if (__GNUC__ >= 2) # The register-names have changed
        #define byteptr_register  "%edi"
      #else
        #define byteptr_register  "di"
      #endif
    #endif
    #ifdef ARM
      # Code is better without defining registers for closure and byteptr,
      # says Peter Burwood.
      # not define closure_register  "%r6"
      # not define byteptr_register  "%r7"
      # We have assembler macros below, but if they are used with gcc-2.7.2.1,
      # (setf cdddr) is miscompiled. So here we temporarily disable them.
      #ifndef NO_ASM
        #define NO_ASM
      #endif
    #endif
    #ifdef DECALPHA
      #define byteptr_register  "$14"
    #endif
    #if defined(WIDE) && !defined(WIDE_HARD)
      # An `object' does not fit into a single register, GCC is overcharged.
      #undef closure_register
    #endif
  #endif
  #ifndef closure_register
    #define closure_in  closure
  #endif
  #ifndef byteptr_register
    #define byteptr_in  byteptr
  #endif
  #ifdef DEBUG_BYTECODE
    #define GOTO_ERROR(label)  \
      do {                                              \
        fprintf(stderr,"\n[%s:%d] ",__FILE__,__LINE__); \
        goto label;                                     \
      } while(0)
    #define DEBUG_CHECK_BYTEPTR(nb) do {                                \
      var const uintL b = nb - codeptr->data;                           \
      if ((b < byteptr_min) || (b > byteptr_max)) {                     \
        var uintL bp = byteptr - codeptr->data;                         \
        fprintf(stderr,"\n[%s:%d] ",__FILE__,__LINE__);                 \
        byteptr_bad_jump = b - bp;                                      \
        /*nobject_out(stderr,closure);*/                                \
        /*fprintf(stderr," jump by %d takes %d outside [%d;%d]",byteptr_bad_jump,bp,byteptr_min,byteptr_max);*/ \
        goto fehler_byteptr;                                            \
      }} while(0)
  #else
    #define GOTO_ERROR(label)  goto label
    #define DEBUG_CHECK_BYTEPTR(b)     do{}while(0)
  #endif
  local /*maygc*/ Values interpret_bytecode_ (object closure_in, Sbvector codeptr, const uintB* byteptr_in)
  {
   GCTRIGGER_IF(true, {
     if (*byteptr_in == cod_handler_begin_push)
       GCTRIGGER3(closure_in,handler_args.condition,handler_args.spdepth);
     else
       GCTRIGGER1(closure_in);
   });
   #if defined(STACKCHECKC) || defined(DEBUG_BYTECODE)
    var const uintL byteptr_min = ((Codevec)codeptr)->ccv_flags & bit(7)
      ? CCV_START_KEY : CCV_START_NONKEY;
   #endif
   #ifdef DEBUG_BYTECODE
    var const uintL byteptr_max = sbvector_length(codeptr)-1;
    var sintL byteptr_bad_jump;
   #endif
    # situate argument closure in register:
    #ifdef closure_register
    var object closure __asm__(closure_register);
    closure = closure_in;
    #endif
    # situate argument byteptr in register:
    #ifdef byteptr_register
    var register const uintB* byteptr __asm__(byteptr_register);
    byteptr = byteptr_in;
    #endif
    TRACE_CALL(closure,'B','C');
    # situate closure in STACK, below the arguments:
    var gcv_object_t* closureptr = (pushSTACK(closure), &STACK_0);
    #ifndef FAST_SP
      # If there is no fast SP-Access, one has to introduce
      # an extra pointer:
      var uintL private_SP_length =
        (uintL)(((Codevec)codeptr)->ccv_spdepth_1)
        + jmpbufsize * (uintL)(((Codevec)codeptr)->ccv_spdepth_jmpbufsize);
      var DYNAMIC_ARRAY(private_SP_space,SPint,private_SP_length);
      var SPint* private_SP = &private_SP_space[private_SP_length];
      #undef SP_
      #undef _SP_
      #undef skipSP
      #undef pushSP
      #undef popSP
      #define SP_(n)  (private_SP[n])
      #define _SP_(n)  &SP_(n)
      #define skipSP(n)  (private_SP += (n))
      #define pushSP(item)  (*--private_SP = (item))
      #define popSP(item_zuweisung)  (item_zuweisung *private_SP++)
    #endif
    # var JMPBUF_on_SP(name);  allocates a sp_jmp_buf in SP.
    # FREE_JMPBUF_on_SP();  deallocates it.
    # finish_entry_frame_1(frametype,returner,reentry_statement);  is like
    # finish_entry_frame(frametype,returner,,reentry_statement);  but
    # also private_SP is saved.
    #ifndef FAST_SP
      #define JMPBUF_on_SP(name)  \
        sp_jmp_buf* name = (sp_jmp_buf*)(private_SP -= jmpbufsize);
      #define FREE_JMPBUF_on_SP()  \
        private_SP += jmpbufsize;
      #define finish_entry_frame_1(frametype,returner,reentry_statement)  \
        finish_entry_frame(frametype,*returner, # On entry: returner = private_SP      \
          returner = (sp_jmp_buf*) , # returner is set again on return           \
          { private_SP = (SPint*)returner; reentry_statement } # and private_SP is reconstructed \
          )
    #else
      #ifdef SP_DOWN
        #define JMPBUF_on_SP(name)  \
          sp_jmp_buf* name;                   \
          {var SPint* sp = (SPint*)SP();      \
           sp -= jmpbufsize;                  \
           setSP(sp);                         \
           name = (sp_jmp_buf*)&sp[SPoffset]; \
          }
      #endif
      #ifdef SP_UP
        #define JMPBUF_on_SP(name)  \
          sp_jmp_buf* name;                     \
          {var SPint* sp = (SPint*)SP();        \
           name = (sp_jmp_buf*)&sp[SPoffset+1]; \
           sp += jmpbufsize;                    \
           setSP(sp);                           \
          }
      #endif
      #define FREE_JMPBUF_on_SP()  \
        skipSP(jmpbufsize);
      #define finish_entry_frame_1(frametype,returner,reentry_statement)  \
        finish_entry_frame(frametype,*returner,,reentry_statement)
    #endif
    #
    #ifdef FAST_DISPATCH
      static void* const cod_labels[] = {
                                          #define BYTECODE(code)  &&code,
                                          #include "bytecode.c"
                                          #undef BYTECODE
                                        };
    #endif
    #
    # next Byte to be interpreted
    # > mv_count/mv_space: current values
    # > closureptr: pointer to the compiled closure on Stack
    # > closure: compiled closure
    # > codeptr: its codevector, a Simple-Bit-Vektor, pointable
    #            (no LISP-object, but nevertheless endangered by GC!)
    # > byteptr: pointer to the next byte in code
    #            (no LISP-object, but nevertheless endangered by GC!)
   next_byte:
    # definition by cases, according to byte to be interpreted byte
    #ifndef FAST_DISPATCH
     switch (*byteptr++)
     #define CASE  case (uintB)
    #else # FAST_DISPATCH
     # This is faster by about 2%, because the index-check is dropped.
     goto *cod_labels[*byteptr++];
     #define CASE
     #ifdef FAST_DISPATCH_THREADED
      # The jump-statement  goto next_byte;  can be omitted:
      #define next_byte  *cod_labels[*byteptr++]
     #endif
    #endif
    {
      # Operand-Fetch:
      #   next Byte:
      #     Bit 7 = 0 --> Bits 6..0 are the Operand (7 Bits).
      #     Bit 7 = 1 --> Bits 6..0 and next Byte form the
      #                   Operand (15 Bits).
      #                   For jump-distances: Should this be =0, the next
      #                   4 Bytes form the Operand
      #                   (32 Bits).
      #
      # Macro B_operand(where);
      # moves the next Operand (a Byte as Unsigned Integer)
      # to (uintL)where and advances  bytecodepointer.
        #define B_operand(where)  \
          { where = *byteptr++; }
      #
      # Macro U_operand(where);
      # moves the next Operand (an Unsigned Integer)
      # to (uintL)where or (uintC)where
      # and advances the Bytecodepointer.
        #define U_operand(where)  \
          { where = *byteptr++; # read first Byte              \
            if ((uintB)where & bit(7)) # Bit 7 set?            \
              { where &= ~bit(7); # yes -> delete              \
                where = where << 8;                            \
                where |= *byteptr++; # and read next Byte      \
          }   }
      #if defined(GNU) && defined(MC680X0) && !defined(NO_ASM)
        #undef U_operand
        #define U_operand(where)  \
          __asm__(                 \
            "moveq #0,%0"   "\n\t" \
            "moveb %1@+,%0" "\n\t" \
            "bpl 1f"        "\n\t" \
            "addb %0,%0"    "\n\t" \
            "lslw #7,%0"    "\n\t" \
            "moveb %1@+,%0" "\n"   \
            "1:"                   \
            : "=d" (where), "=a" (byteptr) : "1" (byteptr) )
      #endif
      #if defined(GNU) && defined(SPARC) && !defined(NO_ASM)
        #undef U_operand
        #define U_operand(where)  \
          { var uintL dummy;              \
            __asm__(                      \
              "ldub [%1],%0"       "\n\t" \
              "andcc %0,0x80,%%g0" "\n\t" \
              "be 1f"              "\n\t" \
              " add %1,1,%1"       "\n\t" \
              "sll %0,25,%2"       "\n\t" \
              "ldub [%1],%0"       "\n\t" \
              "srl %2,17,%2"       "\n\t" \
              "add %1,1,%1"        "\n\t" \
              "or %0,%2,%0"        "\n"   \
              "1:"                        \
              : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "ccr" ); \
          }
      #endif
      #if (defined(GNU) || defined(INTEL)) && defined(I80386) && !defined(NO_ASM)
        #if 0
          # In earlier times, the GNU assembler assembled
          # "testb %edx,%edx" as "testb %dl,%dl". This made possible to
          # produce the output in any register.
          #define OUT_EAX  "=q"
          #define EAX      "%0"
          #define AL       "%0"
        #else
          # Now "testb %edx,%edx" is invalid everywhere. The macros must
          # put their result in %eax.
          #define OUT_EAX  "=a"
          #define EAX      "%%eax"
          #define AL       "%%al"
        #endif
        #undef U_operand
        #define U_operand(where)  \
          __asm__(                   \
            "movzbl (%1),"EAX "\n\t" \
            "incl %1"         "\n\t" \
            "testb "AL","AL   "\n\t" \
            "jge "LR(1,f)     "\n\t" \
            "andb $127,"AL    "\n\t" \
            "sall $8,"EAX     "\n\t" \
            "movb (%1),"AL    "\n\t" \
            "incl %1"         "\n"   \
            LD(1)":"                 \
            : OUT_EAX (where), "=r" (byteptr) : "1" (byteptr) );
        # Caution: 1. Der Sun-Assembler doesn't know this Syntax for local labels.
        #              That's why we generate our local labels ourselves.
        # Caution: 2. ccr is changed. How is this to be declared??
      #endif
      #if defined(GNU) && defined(ARM) && !defined(NO_ASM)
        # Macros written by Peter Burwood.
        # Two versions. Which one to choose?
        #        instructions      short case        long case
        # v1:          5           2 + 3 skipped     5
        # v2:          5           3 + 2 skipped     4 + 1 skipped
        # Let's choose the first one. 1-byte operands are most frequent.
        #undef U_operand
        #define U_operand(where)  # (v1) \
          { var uintL dummy;                 \
            __asm__(                         \
              "ldrb   %0,[%1],#1"     "\n\t" \
              "tst    %0,#0x80"       "\n\t" \
              "bicne  %0,%0,#0x80"    "\n\t" \
              "ldrneb %2,[%1],#1"     "\n\t" \
              "orrne  %0,%2,%0,LSL#8"        \
              : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "cc" ); \
          }
        #if 0
        #undef U_operand
        #define U_operand(where)  # (v2) \
          { var uintL dummy;                 \
            __asm__(                         \
              "ldrb   %0,[%1],#1"     "\n\t" \
              "movs   %0,%0,LSL#25"   "\n\t" \
              "movcc  %0,%0,LSR#25"   "\n\t" \
              "ldrcsb %2,[%1],#1"     "\n\t" \
              "orrcs  %0,%2,%0,LSR#17"       \
              : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "cc" ); \
          }
        #endif
      #endif
      #
      # Macro S_operand(where);
      # moves the next Operand (a Signed Integer)
      # to (uintL)where and advances the bytecodepointer.
        #define S_operand(where)  \
          { where = *byteptr++; # read first byte                \
            if ((uintB)where & bit(7))                           \
              # Bit 7 was set                                    \
              { where = where << 8;                              \
                where |= *byteptr++; # subjoin next Byte         \
                # Sign-Extend from 15 to 32 Bits:                \
                where = (sintL)((sintL)(sintWL)((sintWL)where << (intWLsize-15)) >> (intWLsize-15)); \
                if (where == 0)                                  \
                  # special case: 2-Byte-Operand = 0 -> 6-Byte-Operand \
                  { where = (uintL)( ((uintWL)(byteptr[0]) << 8) \
                                    | (uintWL)(byteptr[1])       \
                                   ) << 16                       \
                          | (uintL)( ((uintWL)(byteptr[2]) << 8) \
                                    | (uintWL)(byteptr[3])       \
                                   );                            \
                    byteptr += 4;                                \
              }   }                                              \
              else                                               \
              # Bit 7 was deleted                                \
              { # Sign-Extend from 7 to 32 Bits:                 \
                where = (sintL)((sintL)(sintBWL)((sintBWL)where << (intBWLsize-7)) >> (intBWLsize-7)); \
              }                                                  \
          }
      #if defined(GNU) && defined(MC680X0) && !defined(NO_ASM)
        #undef S_operand
        #define S_operand(where)  \
          __asm__(                   \
            "moveb %1@+,%0"   "\n\t" \
            "bpl 1f"          "\n\t" \
            "lslw #8,%0"      "\n\t" \
            "moveb %1@+,%0"   "\n\t" \
            "addw %0,%0"      "\n\t" \
            "asrw #1,%0"      "\n\t" \
            "bne 2f"          "\n\t" \
            "moveb %1@(2),%0" "\n\t" \
            "swap %0"         "\n\t" \
            "moveb %1@+,%0"   "\n\t" \
            "lsll #8,%0"      "\n\t" \
            "moveb %1@,%0"    "\n\t" \
            "swap %0"         "\n\t" \
            "addql #2,%0"     "\n\t" \
            "moveb %1@+,%0"   "\n\t" \
            "jra 3f"          "\n"   \
            "1:"                "\t" \
            "addb %0,%0"      "\n\t" \
            "asrb #1,%0"      "\n\t" \
            "extw %0"         "\n"   \
            "2:"                "\t" \
            "extl %0"         "\n"   \
            "3:"                     \
            : "=d" (where), "=a" (byteptr) : "1" (byteptr) )
      #endif
      #if defined(GNU) && defined(SPARC) && !defined(NO_ASM)
        #undef S_operand
        #define S_operand(where)  \
          { var uintL dummy;              \
            __asm__(                      \
              "ldub [%1],%0"       "\n\t" \
              "andcc %0,0x80,%%g0" "\n\t" \
              "be 2f"              "\n\t" \
              " add %1,1,%1"       "\n\t" \
              "sll %0,25,%2"       "\n\t" \
              "ldub [%1],%0"       "\n\t" \
              "sra %2,17,%2"       "\n\t" \
              "orcc %2,%0,%0"      "\n\t" \
              "bne 3f"             "\n\t" \
              " add %1,1,%1"       "\n\t" \
              "ldub [%1],%0"       "\n\t" \
              "sll %0,24,%2"       "\n\t" \
              "ldub [%1+1],%0"     "\n\t" \
              "sll %0,16,%0"       "\n\t" \
              "or %2,%0,%2"        "\n\t" \
              "ldub [%1+2],%0"     "\n\t" \
              "sll %0,8,%0"        "\n\t" \
              "or %2,%0,%2"        "\n\t" \
              "ldub [%1+3],%0"     "\n\t" \
              "or %2,%0,%0"        "\n\t" \
              "b 3f"               "\n\t" \
              " add %1,4,%1"       "\n"   \
              "2:"                   "\t" \
              "sll %0,25,%0"       "\n\t" \
              "sra %0,25,%0"       "\n"   \
              "3:"                   "\t" \
              : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "ccr" ); \
          }
      #endif
      #if (defined(GNU) || defined(INTEL)) && defined(I80386) && !defined(NO_ASM)
        #undef S_operand
        #define S_operand(where)  \
          __asm__(                   \
            "movzbl (%1),"EAX "\n\t" \
            "incl %1"         "\n\t" \
            "testb "AL","AL   "\n\t" \
            "jge "LR(1,f)     "\n\t" \
            "sall $8,"EAX     "\n\t" \
            "movb (%1),"AL    "\n\t" \
            "incl %1"         "\n\t" \
            "sall $17,"EAX    "\n\t" \
            "sarl $17,"EAX    "\n\t" \
            "jne "LR(2,f)     "\n\t" \
            "movb (%1),"AL    "\n\t" \
            "sall $8,"EAX     "\n\t" \
            "movb 1(%1),"AL   "\n\t" \
            "sall $8,"EAX     "\n\t" \
            "movb 2(%1),"AL   "\n\t" \
            "sall $8,"EAX     "\n\t" \
            "movb 3(%1),"AL   "\n\t" \
            "addl $4,"EAX     "\n\t" \
            "jmp "LR(2,f)     "\n"   \
            LD(1)":"            "\t" \
            "sall $25,"EAX    "\n\t" \
            "sarl $25,"EAX    "\n"   \
            LD(2)":"                 \
            : OUT_EAX (where), "=r" (byteptr) : "1" (byteptr) );
      #endif
      #if defined(GNU) && defined(ARM) && !defined(NO_ASM)
        # Macro written by Peter Burwood.
        #undef S_operand
        #define S_operand(where)  \
          { var uintL dummy;                  \
            __asm__(                          \
              "ldrb   %0,[%1],#1"      "\n\t" \
              "movs   %0,%0,LSL#25"    "\n\t" \
              "movcc  %0,%0,ASR#25"    "\n\t" \
              "bcc    "LR(1,f)         "\n\t" \
              "ldrb   %2,[%1],#1"      "\n\t" \
              "orr    %0,%0,%2,LSL#17" "\n\t" \
              "movs   %0,%0,ASR#17"    "\n\t" \
              "bne    "LR(1,f)         "\n\t" \
              "ldrb   %0,[%1],#1"      "\n\t" \
              "ldrb   %2,[%1],#1"      "\n\t" \
              "orr    %0,%2,%0,LSL#8"  "\n\t" \
              "ldrb   %2,[%1],#1"      "\n\t" \
              "orr    %0,%2,%0,LSL#8"  "\n\t" \
              "ldrb   %2,[%1],#1"      "\n\t" \
              "orr    %0,%2,%0,LSL#8"  "\n"   \
              LD(1)":"                        \
              : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "cc" ); \
          }
      #endif
      #
      # Macro S_operand_ignore();
      # skips the next Operand (a Signed Integer)
      # and advances the bytecodepointer.
        #define S_operand_ignore()  \
          { var uintB where = *byteptr++; # read first byte          \
            if ((uintB)where & bit(7))                               \
              # Bit 7 war gesetzt                                    \
              { if ((uintB)((where<<1) | *byteptr++) == 0) # next Byte \
                  # special case: 2-Byte-Operand = 0 -> 6-Byte-Operand \
                  { byteptr += 4; }                                  \
          }   }
      #if defined(GNU) && defined(MC680X0) && !defined(NO_ASM)
        #undef S_operand_ignore
        #define S_operand_ignore()  \
          { var uintB where;           \
            __asm__(                   \
              "moveb %1@+,%0"   "\n\t" \
              "bpl 1f"          "\n\t" \
              "addb %0,%0"      "\n\t" \
              "orb %1@+,%0"     "\n\t" \
              "bne 1f"          "\n\t" \
              "addql #4,%1"     "\n"   \
              "1:"                     \
              : "=d" (where), "=a" (byteptr) : "1" (byteptr) ); \
          }
      #endif
      #if defined(GNU) && defined(SPARC) && !defined(NO_ASM)
        #undef S_operand_ignore
        #define S_operand_ignore()  \
          { var uintL where;              \
            var uintL dummy;              \
            __asm__(                      \
              "ldub [%1],%0"       "\n\t" \
              "andcc %0,0x80,%%g0" "\n\t" \
              "be 1f"              "\n\t" \
              " add %1,1,%1"       "\n\t" \
              "sll %0,1,%2"        "\n\t" \
              "ldub [%1],%0"       "\n\t" \
              "orcc %2,%0,%0"      "\n\t" \
              "bne 1f"             "\n\t" \
              " add %1,1,%1"       "\n\t" \
              "add %1,4,%1"        "\n"   \
              "1:"                        \
              : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "ccr" ); \
          }
      #endif
      #if defined(GNU) && defined(ARM) && !defined(NO_ASM)
        # Macro written by Peter Burwood.
        #undef S_operand_ignore
        #define S_operand_ignore()  \
          { var uintL where;                  \
            var uintL dummy;                  \
            __asm__(                          \
              "ldrb   %0,[%1],#1"      "\n\t" \
              "movs   %0,%0,LSL#25"    "\n\t" \
              "bcc    "LR(1,f)         "\n\t" \
              "ldrb   %2,[%1],#1"      "\n\t" \
              "orrs   %0,%2,%0,LSR#24" "\n\t" \
              "addeq  %1,%1,#4"        "\n"   \
              LD(1)":"                        \
              : "=r" (where), "=r" (byteptr), "=r" (dummy) : "1" (byteptr) : "cc" ); \
          }
      #endif
      #
      # Macro L_operand(where);
      # moves the next Operand (a Label)
      # to (uintB*)where and advances the bytecodepointer.
        #define L_operand(Lwhere)  \
          { var uintL where; # variable for the displacement \
            S_operand(where); # Displacement                 \
            Lwhere = byteptr + (sintL)where; # add           \
          }
      #
      # Macro L_operand_ignore();
      # skips the next Operand (a Label)
      # and advances the Bytecodepointer.
        #define L_operand_ignore()  S_operand_ignore()
      #
      # Each of the bytecodes is interpreted:
      # for the most part: mv_count/mv_space = values,
      # closureptr = pointer to the compiled closure in Stack,
      # closure = compiled closure,
      # codeptr = pointer to its codevector,
      # byteptr = pointer to the next Byte in code.
      # (byteptr is no LISP-object, but nevertheless endangered by GC! In order to
      #  make it GC-invariant, CODEPTR must be subtraced from it.
      #  If one adds to Fixnum_0 to it,
      #  one receives the bytenumber as Fixnum.)
      #if 0
        #define CODEPTR  (&codeptr->data[0])
      #else # returns more efficient Code
        #define CODEPTR  (uintB*)(codeptr)
      #endif
      #
      # store context-information:
      # If sth. is called, that can trigger a GC, this must be built into a
      # with_saved_context( ... ) .
        #define with_saved_context(statement)  \
          { var uintL index = byteptr - CODEPTR;                       \
            statement;                                                 \
            closure = *closureptr; # fetch Closure from Stack          \
            codeptr = TheSbvector(TheCclosure(closure)->clos_codevec); \
            byteptr = CODEPTR + index;                                 \
          }
      #
      # ------------------- (1) Constants -----------------------
      CASE cod_nil:                    # (NIL)
        code_nil:
        VALUES1(NIL);
        goto next_byte;
      CASE cod_nil_push:               # (NIL&PUSH)
        pushSTACK(NIL);
        goto next_byte;
      CASE cod_push_nil:               # (PUSH-NIL n)
        {
          var uintC n;
          U_operand(n);
          dotimesC(n,n, { pushSTACK(NIL); } );
        }
        goto next_byte;
      CASE cod_t:                      # (T)
        code_t:
        VALUES1(T);
        goto next_byte;
      CASE cod_t_push:                 # (T&PUSH)
        pushSTACK(T);
        goto next_byte;
      CASE cod_const:                  # (CONST n)
        {
          var uintL n;
          U_operand(n);
          VALUES1(TheCclosure(closure)->clos_consts[n]);
        }
        goto next_byte;
      CASE cod_const_push:             # (CONST&PUSH n)
        {
          var uintL n;
          U_operand(n);
          pushSTACK(TheCclosure(closure)->clos_consts[n]);
        }
        goto next_byte;
      # ------------------- (2) static Variables -----------------------
      CASE cod_load:                   # (LOAD n)
        {
          var uintL n;
          U_operand(n);
          VALUES1(STACK_(n));
        }
        goto next_byte;
      CASE cod_load_push:              # (LOAD&PUSH n)
        {
          var uintL n;
          U_operand(n);
          pushSTACK(STACK_(n));
        }
        goto next_byte;
      CASE cod_loadi:                  # (LOADI k1 k2 n)
        {
          var uintL k1;
          var uintL k2;
          var uintL n;
          U_operand(k1);
          U_operand(k2);
          U_operand(n);
          var gcv_object_t* FRAME = (gcv_object_t*) SP_(k1+jmpbufsize*k2);
          VALUES1(FRAME_(n));
        }
        goto next_byte;
      CASE cod_loadi_push:             # (LOADI&PUSH k1 k2 n)
        {
          var uintL k1;
          var uintL k2;
          var uintL n;
          U_operand(k1);
          U_operand(k2);
          U_operand(n);
          var gcv_object_t* FRAME = (gcv_object_t*) SP_(k1+jmpbufsize*k2);
          pushSTACK(FRAME_(n));
        }
        goto next_byte;
      CASE cod_loadc:                  # (LOADC n m)
        {
          var uintL n;
          var uintL m;
          U_operand(n);
          U_operand(m);
          VALUES1(TheSvector(STACK_(n))->data[1+m]);
        }
        goto next_byte;
      CASE cod_loadc_push:             # (LOADC&PUSH n m)
        {
          var uintL n;
          var uintL m;
          U_operand(n);
          U_operand(m);
          pushSTACK(TheSvector(STACK_(n))->data[1+m]);
        }
        goto next_byte;
      CASE cod_loadv:                  # (LOADV k m)
        {
          var uintC k;
          var uintL m;
          U_operand(k);
          U_operand(m);
          var object venv = TheCclosure(closure)->clos_venv; # VenvConst
          # take (svref ... 0) k times:
          dotimesC(k,k, { venv = TheSvector(venv)->data[0]; } );
          # fetch (svref ... m) :
          VALUES1(TheSvector(venv)->data[m]);
        }
        goto next_byte;
      CASE cod_loadv_push:             # (LOADV&PUSH k m)
        {
          var uintC k;
          var uintL m;
          U_operand(k);
          U_operand(m);
          var object venv = TheCclosure(closure)->clos_venv; # VenvConst
          # take (svref ... 0) k times:
          dotimesC(k,k, { venv = TheSvector(venv)->data[0]; } );
          # fetch (svref ... m) :
          pushSTACK(TheSvector(venv)->data[m]);
        }
        goto next_byte;
      CASE cod_loadic:                 # (LOADIC k1 k2 n m)
        {
          var uintL k1;
          var uintL k2;
          var uintL n;
          var uintL m;
          U_operand(k1);
          U_operand(k2);
          U_operand(n);
          U_operand(m);
          var gcv_object_t* FRAME = (gcv_object_t*) SP_(k1+jmpbufsize*k2);
          VALUES1(TheSvector(FRAME_(n))->data[1+m]);
        }
        goto next_byte;
      CASE cod_store: store:           # (STORE n)
        {
          var uintL n;
          U_operand(n);
          VALUES1(STACK_(n) = value1);
        }
        goto next_byte;
      CASE cod_pop_store:              # (POP&STORE n)
        {
          var uintL n;
          U_operand(n);
          var object obj = popSTACK();
          VALUES1(STACK_(n) = obj);
        }
        goto next_byte;
      CASE cod_storei:                 # (STOREI k1 k2 n)
        {
          var uintL k1;
          var uintL k2;
          var uintL n;
          U_operand(k1);
          U_operand(k2);
          U_operand(n);
          var gcv_object_t* FRAME = (gcv_object_t*) SP_(k1+jmpbufsize*k2);
          VALUES1(FRAME_(n) = value1);
        }
        goto next_byte;
      CASE cod_load_storec:            # (LOAD&STOREC k m n)
        {
          var uintL k;
          U_operand(k);
          value1 = STACK_(k);
        }
      CASE cod_storec:                 # (STOREC n m)
        {
          var uintL n;
          var uintL m;
          U_operand(n);
          U_operand(m);
          TheSvector(STACK_(n))->data[1+m] = value1; mv_count=1;
        }
        goto next_byte;
      CASE cod_storev:                 # (STOREV k m)
        {
          var uintC k;
          var uintL m;
          U_operand(k);
          U_operand(m);
          var object venv = TheCclosure(closure)->clos_venv; # VenvConst
          # take (svref ... 0) k times:
          dotimesC(k,k, { venv = TheSvector(venv)->data[0]; } );
          # save (svref ... m) :
          TheSvector(venv)->data[m] = value1; mv_count=1;
        }
        goto next_byte;
      CASE cod_storeic:                # (STOREIC k1 k2 n m)
        {
          var uintL k1;
          var uintL k2;
          var uintL n;
          var uintL m;
          U_operand(k1);
          U_operand(k2);
          U_operand(n);
          U_operand(m);
          var gcv_object_t* FRAME = (gcv_object_t*) SP_(k1+jmpbufsize*k2);
          TheSvector(FRAME_(n))->data[1+m] = value1; mv_count=1;
        }
        goto next_byte;
      # ------------------- (3) dynamic Variables -----------------------
      CASE cod_getvalue:               # (GETVALUE n)
        {
          var uintL n;
          U_operand(n);
          var object symbol = TheCclosure(closure)->clos_consts[n];
          # The Compiler has already checked, that it's a Symbol.
          if (!boundp(Symbol_value(symbol))) {
            pushSTACK(symbol); # CELL-ERROR slot NAME
            pushSTACK(symbol); pushSTACK(Closure_name(closure));
            fehler(unbound_variable,GETTEXT("~S: symbol ~S has no value"));
          }
          VALUES1(Symbol_value(symbol));
        }
        goto next_byte;
      CASE cod_getvalue_push:          # (GETVALUE&PUSH n)
        {
          var uintL n;
          U_operand(n);
          var object symbol = TheCclosure(closure)->clos_consts[n];
          # The Compiler has already checked, that it's a Symbol.
          if (!boundp(Symbol_value(symbol))) {
            pushSTACK(symbol); # CELL-ERROR slot NAME
            pushSTACK(symbol); pushSTACK(Closure_name(closure));
            fehler(unbound_variable,GETTEXT("~S: symbol ~S has no value"));
          }
          pushSTACK(Symbol_value(symbol));
        }
        goto next_byte;
      CASE cod_setvalue:               # (SETVALUE n)
        {
          var uintL n;
          U_operand(n);
          var object symbol = TheCclosure(closure)->clos_consts[n];
          # The Compiler has already checked, that it's a Symbol.
          if (constant_var_p(TheSymbol(symbol))) {
            pushSTACK(symbol); pushSTACK(Closure_name(closure));
            fehler(error,GETTEXT("~S: assignment to constant symbol ~S is impossible"));
          }
          Symbol_value(symbol) = value1; mv_count=1;
        }
        goto next_byte;
      CASE cod_bind:                   # (BIND n)
        {
          var uintL n;
          U_operand(n);
          dynamic_bind(TheCclosure(closure)->clos_consts[n],value1);
        }
        goto next_byte;
      CASE cod_unbind1:                # (UNBIND1)
        #if STACKCHECKC
        if (!(framecode(STACK_0) == DYNBIND_frame_info))
          GOTO_ERROR(fehler_STACK_putt);
        #endif
        # unwind variable-binding-frame:
        {
          var gcv_object_t* new_STACK = topofframe(STACK_0); # pointer above frame
          var gcv_object_t* frame_end = STACKpointable(new_STACK);
          var gcv_object_t* bindingptr = &STACK_1; # begin of bindings
          # bindingptr loops upwards through the bindings
          until (bindingptr == frame_end) {
            # write back old value:
            Symbol_value(*(bindingptr STACKop 0)) = *(bindingptr STACKop 1);
            bindingptr skipSTACKop 2; # next binding
          }
          # set STACK newly, thus unwind frame:
          setSTACK(STACK = new_STACK);
        }
        goto next_byte;
      CASE cod_unbind:                 # (UNBIND n)
        {
          var uintC n;
          U_operand(n); # n>0
          var gcv_object_t* FRAME = STACK;
          do {
            #if STACKCHECKC
            if (!(framecode(FRAME_(0)) == DYNBIND_frame_info))
              GOTO_ERROR(fehler_STACK_putt);
            #endif
            # unwind variable-binding-frame:
            var gcv_object_t* new_FRAME = topofframe(FRAME_(0)); # pointer above frame
            var gcv_object_t* frame_end = STACKpointable(new_FRAME);
            var gcv_object_t* bindingptr = &FRAME_(1); # begin of the bindings
            # bindingptr loops upwards through the bindings
            until (bindingptr == frame_end) {
              # write back old value:
              Symbol_value(*(bindingptr STACKop 0)) = *(bindingptr STACKop 1);
              bindingptr skipSTACKop 2; # next binding
            }
            FRAME = new_FRAME;
          } until (--n == 0);
          setSTACK(STACK = FRAME); # set STACK newly
        }
        goto next_byte;
      CASE cod_progv:                  # (PROGV)
        {
          var object vallist = value1; # value-list
          var object symlist = popSTACK(); # symbol-list
          pushSP((aint)STACK); # push STACK into SP
          with_saved_context( progv(symlist,vallist); ); # build frame
        }
        goto next_byte;
      # ------------------- (4) Stackoperations -----------------------
      CASE cod_push:                   # (PUSH)
        pushSTACK(value1);
        goto next_byte;
      CASE cod_pop:                    # (POP)
        VALUES1(popSTACK());
        goto next_byte;
      CASE cod_skip:                   # (SKIP n)
        {
          var uintL n;
          U_operand(n);
          skipSTACK(n);
        }
        goto next_byte;
      CASE cod_skipi:                  # (SKIPI k1 k2 n)
        {
          var uintL k1;
          var uintL k2;
          var uintL n;
          U_operand(k1);
          U_operand(k2);
          U_operand(n);
          skipSP(k1+jmpbufsize*k2);
          var gcv_object_t* newSTACK;
          popSP( newSTACK = (gcv_object_t*) );
          setSTACK(STACK = newSTACK STACKop n);
        }
        goto next_byte;
      CASE cod_skipsp:                 # (SKIPSP k1 k2)
        {
          var uintL k1;
          var uintL k2;
          U_operand(k1);
          U_operand(k2);
          skipSP(k1+jmpbufsize*k2);
        }
        goto next_byte;
      # ------------------- (5) Control Flow and Jumps -----------------------
      CASE cod_skip_ret:               # (SKIP&RET n)
        {
          var uintL n;
          U_operand(n);
          skipSTACK(n);
          goto finished; # return (jump) to caller
        }
      CASE cod_skip_retgf:             # (SKIP&RETGF n)
        {
          var uintL n;
          U_operand(n);
          if (((Codevec)codeptr)->ccv_flags & bit(3)) { # call inhibition?
            skipSTACK(n);
            mv_count=1;
            goto finished; # return (jump) to caller
          }
          # It is known (refer to clos.lisp), that this function
          # has no optional parameters, but poss. Rest-parameters.
          # If there's no Rest-parameter: (FUNCALL value1 arg1 ... argr)
          # If there's a  Rest-Parameter: (APPLY value1 arg1 ... argr restarg)
          var uintL r = ((Codevec)codeptr)->ccv_numreq;
          n -= r;
          if (((Codevec)codeptr)->ccv_flags & bit(0)) {
            skipSTACK(n-1); apply(value1,r,popSTACK());
          } else {
            skipSTACK(n); funcall(value1,r);
          }
          goto finished; # return (jump) to caller
        }
      #define JMP()                             \
        { var const uintB* label_byteptr;       \
          L_operand(label_byteptr);             \
          DEBUG_CHECK_BYTEPTR(label_byteptr);   \
          byteptr = label_byteptr;              \
          goto next_byte;                       \
        }
      #define NOTJMP()  \
        { L_operand_ignore(); goto next_byte; }
      jmp1: mv_count=1;
      CASE cod_jmp: jmp:               # (JMP label)
        JMP();
      CASE cod_jmpif:                  # (JMPIF label)
        if (!nullp(value1)) goto jmp;
        notjmp:
        NOTJMP();
      CASE cod_jmpifnot:               # (JMPIFNOT label)
        if (nullp(value1)) goto jmp;
        NOTJMP();
      CASE cod_jmpif1:                 # (JMPIF1 label)
        if (!nullp(value1)) goto jmp1;
        NOTJMP();
      CASE cod_jmpifnot1:              # (JMPIFNOT1 label)
        if (nullp(value1)) goto jmp1;
        NOTJMP();
      CASE cod_jmpifatom:              # (JMPIFATOM label)
        if (atomp(value1)) goto jmp;
        NOTJMP();
      CASE cod_jmpifconsp:             # (JMPIFCONSP label)
        if (consp(value1)) goto jmp;
        NOTJMP();
      CASE cod_jmpifeq:                # (JMPIFEQ label)
        if (eq(popSTACK(),value1)) goto jmp;
        NOTJMP();
      CASE cod_jmpifnoteq:             # (JMPIFNOTEQ label)
        if (!eq(popSTACK(),value1)) goto jmp;
        NOTJMP();
      CASE cod_jmpifeqto:              # (JMPIFEQTO n label)
        {
          var uintL n;
          U_operand(n);
          if (eq(popSTACK(),TheCclosure(closure)->clos_consts[n])) goto jmp;
        }
        NOTJMP();
      CASE cod_jmpifnoteqto:           # (JMPIFNOTEQTO n label)
        {
          var uintL n;
          U_operand(n);
          if (!eq(popSTACK(),TheCclosure(closure)->clos_consts[n])) goto jmp;
        }
        NOTJMP();
      CASE cod_jmphash:                # (JMPHASH n label)
        {
          var uintL n;
          U_operand(n);
          var object hashvalue = # search value1 in the Hash-table
            gethash(value1,TheCclosure(closure)->clos_consts[n],false);
          if (eq(hashvalue,nullobj))
            goto jmp; # not found -> jump to label
          else { /* interpret found Fixnum as label: */
            DEBUG_CHECK_BYTEPTR(byteptr + fixnum_to_V(hashvalue));
            byteptr += fixnum_to_V(hashvalue);
          }
        }
        goto next_byte;
      CASE cod_jmphashv:               # (JMPHASHV n label)
        {
          var uintL n;
          U_operand(n);
          var object hashvalue = # search value1 in the Hash-table
            gethash(value1,TheSvector(TheCclosure(closure)->clos_consts[0])->data[n],false);
          if (eq(hashvalue,nullobj))
            goto jmp; # not found -> jump to label
          else { /* interpret found Fixnum as label: */
            DEBUG_CHECK_BYTEPTR(byteptr + fixnum_to_V(hashvalue));
            byteptr += fixnum_to_V(hashvalue);
          }
        }
        goto next_byte;
      # executes a (JSR label)-command.
      #define JSR()  \
        check_STACK(); check_SP();                                \
        { var const uintB* label_byteptr;                         \
          L_operand(label_byteptr);                               \
          with_saved_context(                                     \
            with_saved_back_trace_cclosure(closure,               \
              interpret_bytecode_(closure,codeptr,label_byteptr); \
          ));                                                     \
        }
      CASE cod_jsr:                    # (JSR label)
        JSR();
        goto next_byte;
      CASE cod_jsr_push:               # (JSR&PUSH label)
        JSR(); pushSTACK(value1);
        goto next_byte;
      CASE cod_jmptail:                # (JMPTAIL m n label)
        {
          var uintL m;
          var uintL n;
          U_operand(m);
          U_operand(n);
          # It is n>=m. Copy m stack-entries upwards by n-m :
          var gcv_object_t* ptr1 = STACK STACKop m;
          var gcv_object_t* ptr2 = STACK STACKop n;
          var uintC count;
          dotimesC(count,m, { NEXT(ptr2) = NEXT(ptr1); } );
          # Now ptr1 = STACK and ptr2 = STACK STACKop (n-m).
          *(closureptr = &NEXT(ptr2)) = closure; # store closure in stack
          setSTACK(STACK = ptr2); # shorten STACK
        }
        JMP(); # jump to label
      # ------------------- (6) Environments and Closures -----------------------
      CASE cod_venv:                   # (VENV)
        # fetch VenvConst from the closure:
        VALUES1(TheCclosure(closure)->clos_venv);
        goto next_byte;
      CASE cod_make_vector1_push:      # (MAKE-VECTOR1&PUSH n)
        {
          var uintL n;
          U_operand(n);
          pushSTACK(value1);
          # create vector:
          var object vec;
          with_saved_context( { vec = allocate_vector(n+1); } );
          # fill first element:
          TheSvector(vec)->data[0] = STACK_0;
          STACK_0 = vec;
        }
        goto next_byte;
      CASE cod_copy_closure:           # (COPY-CLOSURE m n)
        {
          var object oldclos;
          # fetch closure to be copied:
          {
            var uintL m;
            U_operand(m);
            oldclos = TheCclosure(closure)->clos_consts[m];
          }
          # allocate closure of equal length:
          var object newclos;
          pushSTACK(oldclos);
          with_saved_context(
            newclos = allocate_cclosure_copy(oldclos);
          );
          oldclos = popSTACK();
          # copy contents of the old closure into the new one:
          do_cclosure_copy(newclos,oldclos);
          # copy stack content into the new closure:
          {
            var uintL n;
            U_operand(n);
            var gcv_object_t* newptr = &TheCclosure(newclos)->clos_consts[n];
            dotimespL(n,n, { *--newptr = popSTACK(); } );
          }
          VALUES1(newclos);
        }
        goto next_byte;
      CASE cod_copy_closure_push:      # (COPY-CLOSURE&PUSH m n)
        {
          var object oldclos;
          # fetch closure to be copied:
          {
            var uintL m;
            U_operand(m);
            oldclos = TheCclosure(closure)->clos_consts[m];
          }
          # allocate closure of equal length:
          var object newclos;
          pushSTACK(oldclos);
          with_saved_context(
            newclos = allocate_cclosure_copy(oldclos);
          );
          oldclos = popSTACK();
          # copy contents of the old closure into the new one:
          do_cclosure_copy(newclos,oldclos);
          # copy stack content into the new closure:
          {
            var uintL n;
            U_operand(n);
            var gcv_object_t* newptr = &TheCclosure(newclos)->clos_consts[n];
            dotimespL(n,n, { *--newptr = popSTACK(); } );
          }
          pushSTACK(newclos);
        }
        goto next_byte;
      # ------------------- (7) Function Calls -----------------------
      # executes (CALL k n)-command.
      #define CALL()  \
        { var uintC k; # number of arguments                 \
          var uintL n;                                       \
          U_operand(k);                                      \
          U_operand(n);                                      \
          with_saved_context(                                \
            funcall(TheCclosure(closure)->clos_consts[n],k); \
          );                                                 \
        }
      # executes (CALL0 n)-command.
      #define CALL0()  \
        { var uintL n;                                       \
          U_operand(n);                                      \
          with_saved_context(                                \
            funcall(TheCclosure(closure)->clos_consts[n],0); \
          );                                                 \
        }
      # executes (CALL1 n)-command.
      #define CALL1()  \
        { var uintL n;                                       \
          U_operand(n);                                      \
          with_saved_context(                                \
            funcall(TheCclosure(closure)->clos_consts[n],1); \
          );                                                 \
        }
      # executes (CALL2 n)-command.
      #define CALL2()  \
        { var uintL n;                                       \
          U_operand(n);                                      \
          with_saved_context(                                \
            funcall(TheCclosure(closure)->clos_consts[n],2); \
          );                                                 \
        }
      # executes (CALLS1 n)-command.
      #define CALLS1()  \
        { var uintL n;                                                       \
          B_operand(n);                                                      \
          # The compiler has already done the argument-check.                \
         {var Subr fun = FUNTAB1[n];                                         \
          with_saved_context(                                                \
            with_saved_back_trace_subr(subr_tab_ptr_as_object(fun),STACK,-1, \
              (*(subr_norest_function_t*)(fun->function))();                 \
            ));                                                              \
        }}
      # executes (CALLS2 n)-command.
      #define CALLS2()  \
        { var uintL n;                                                       \
          B_operand(n);                                                      \
          # The compiler has already done the argument-check.                \
         {var Subr fun = FUNTAB2[n];                                         \
          with_saved_context(                                                \
            with_saved_back_trace_subr(subr_tab_ptr_as_object(fun),STACK,-1, \
              (*(subr_norest_function_t*)(fun->function))();                 \
            ));                                                              \
        }}
      # executes (CALLSR m n)-command.
      #define CALLSR()  \
        { var uintL m;                                                       \
          var uintL n;                                                       \
          U_operand(m);                                                      \
          B_operand(n);                                                      \
          # The compiler has already done the argument-check.                \
         {var Subr fun = FUNTABR[n];                                         \
          with_saved_context(                                                \
            with_saved_back_trace_subr(subr_tab_ptr_as_object(fun),STACK,-1, \
              (*(subr_rest_function_t*)(fun->function))(m,args_end_pointer STACKop m); \
            ));                                                              \
        }}
      CASE cod_call:                   # (CALL k n)
        CALL();
        goto next_byte;
      CASE cod_call_push:              # (CALL&PUSH k n)
        CALL(); pushSTACK(value1);
        goto next_byte;
      CASE cod_call0:                  # (CALL0 n)
        CALL0();
        goto next_byte;
      CASE cod_call1:                  # (CALL1 n)
        CALL1();
        goto next_byte;
      CASE cod_call1_push:             # (CALL1&PUSH n)
        CALL1(); pushSTACK(value1);
        goto next_byte;
      CASE cod_call2:                  # (CALL2 n)
        CALL2();
        goto next_byte;
      CASE cod_call2_push:             # (CALL2&PUSH n)
        CALL2(); pushSTACK(value1);
        goto next_byte;
      CASE cod_calls1:                 # (CALLS1 n)
        CALLS1();
        goto next_byte;
      CASE cod_calls1_push:            # (CALLS1&PUSH n)
        CALLS1(); pushSTACK(value1);
        goto next_byte;
      CASE cod_calls2:                 # (CALLS2 n)
        CALLS2();
        goto next_byte;
      CASE cod_calls2_push:            # (CALLS2&PUSH n)
        CALLS2(); pushSTACK(value1);
        goto next_byte;
      CASE cod_callsr:                 # (CALLSR m n)
        CALLSR();
        goto next_byte;
      CASE cod_callsr_push:            # (CALLSR&PUSH m n)
        CALLSR(); pushSTACK(value1);
        goto next_byte;
      # executes a (CALLC)-command.
      #define CALLC()  \
        { check_STACK(); check_SP(); # check STACK and SP      \
          with_saved_context(                                  \
            # interprete compiled closure starting at Byte 8   \
            interpret_bytecode(value1,TheCclosure(value1)->clos_codevec,CCV_START_NONKEY); \
          );                                                   \
        }
      # executes a (CALLCKEY)-command.
      #define CALLCKEY()  \
        { check_STACK(); check_SP(); # check STACK and SP      \
          with_saved_context(                                  \
            # interprete compiled closure starting at Byte 12: \
            interpret_bytecode(value1,TheCclosure(value1)->clos_codevec,CCV_START_KEY); \
          );                                                   \
        }
      CASE cod_callc:                  # (CALLC)
        CALLC();
        goto next_byte;
      CASE cod_callc_push:             # (CALLC&PUSH)
        CALLC(); pushSTACK(value1);
        goto next_byte;
      CASE cod_callckey:               # (CALLCKEY)
        CALLCKEY();
        goto next_byte;
      CASE cod_callckey_push:          # (CALLCKEY&PUSH)
        CALLCKEY(); pushSTACK(value1);
        goto next_byte;
      CASE cod_funcall:                # (FUNCALL n)
        {
          var uintL n;
          U_operand(n);
          var object fun = STACK_(n); # Function
          with_saved_context( funcall(fun,n); ); # call Function
          skipSTACK(1); # discard function from Stack
        }
        goto next_byte;
      CASE cod_funcall_push:           # (FUNCALL&PUSH n)
        {
          var uintL n;
          U_operand(n);
          var object fun = STACK_(n); # Function
          with_saved_context( funcall(fun,n); ); # call Function
          STACK_0 = value1; # replace Function in Stack by value
        }
        goto next_byte;
      CASE cod_apply:                  # (APPLY n)
        {
          var uintL n;
          U_operand(n);
          var object fun = STACK_(n); # Function
          with_saved_context( apply(fun,n,value1); ); # call Function
          skipSTACK(1); # discard Function from Stack
        }
        goto next_byte;
      CASE cod_apply_push:             # (APPLY&PUSH n)
        {
          var uintL n;
          U_operand(n);
          var object fun = STACK_(n); # Function
          with_saved_context( apply(fun,n,value1); ); # call Function
          STACK_0 = value1; # replace Function in Stack by value
        }
        goto next_byte;
      # ------------------- (8) optional and Keyword-arguments -----------------------
      CASE cod_push_unbound:           # (PUSH-UNBOUND n)
        {
          var uintC n;
          U_operand(n);
          dotimesC(n,n, { pushSTACK(unbound); } );
        }
        goto next_byte;
      CASE cod_unlist:                 # (UNLIST n m)
        {
          var uintC n;
          var uintC m;
          U_operand(n);
          U_operand(m);
          var object l = value1;
          if (n > 0)
            do {
              if (atomp(l)) goto unlist_unbound;
              pushSTACK(Car(l)); l = Cdr(l);
            } until (--n == 0);
          if (atomp(l))
            goto next_byte;
          else
            fehler_apply_zuviel(S(lambda));
         unlist_unbound:
          if (n > m) fehler_apply_zuwenig(S(lambda),l);
          do { pushSTACK(unbound); } until (--n == 0);
          goto next_byte;
        }
      CASE cod_unliststern:            # (UNLIST* n m)
        {
          var uintC n;
          var uintC m;
          U_operand(n);
          U_operand(m);
          var object l = value1;
          do {
            if (atomp(l)) goto unliststern_unbound;
            pushSTACK(Car(l)); l = Cdr(l);
          } until (--n == 0);
          pushSTACK(l);
          goto next_byte;
         unliststern_unbound:
          if (n > m) fehler_apply_zuwenig(S(lambda),l);
          do { pushSTACK(unbound); } until (--n == 0);
          pushSTACK(NIL);
          goto next_byte;
        }
      CASE cod_jmpifboundp:            # (JMPIFBOUNDP n label)
        {
          var uintL n;
          U_operand(n);
          var object obj = STACK_(n);
          if (!boundp(obj)) goto notjmp;
          VALUES1(obj); JMP();
        }
      CASE cod_boundp:                 # (BOUNDP n)
        {
          var uintL n;
          U_operand(n);
          var object obj = STACK_(n);
          if (!boundp(obj)) goto code_nil; else goto code_t;
        }
      CASE cod_unbound_nil:            # (UNBOUND->NIL n)
        {
          var uintL n;
          U_operand(n);
          if (!boundp(STACK_(n))) { STACK_(n) = NIL; }
        }
        goto next_byte;
      # ------------------- (9) Treatment of multiple values -----------------------
      CASE cod_values0:                # (VALUES0)
        VALUES0;
        goto next_byte;
      CASE cod_values1:                # (VALUES1)
        mv_count = 1;
        goto next_byte;
      CASE cod_stack_to_mv:            # (STACK-TO-MV n)
        {
          var uintL n;
          U_operand(n);
          if (n >= mv_limit) GOTO_ERROR(fehler_zuviele_werte);
          STACK_to_mv(n);
        }
        goto next_byte;
      CASE cod_mv_to_stack:            # (MV-TO-STACK)
        mv_to_STACK(); # push values on Stack
        goto next_byte;
      CASE cod_nv_to_stack:            # (NV-TO-STACK n)
        {
          var uintL n;
          U_operand(n);
          # test for Stack-Overflow:
          get_space_on_STACK(n*sizeof(gcv_object_t));
          # push n values in the Stack:
          var uintC count = mv_count;
          if (n==0) goto nv_to_stack_end; # no value desired -> finished
          # at least 1 value desired
          pushSTACK(value1);
          n--; if (n==0) goto nv_to_stack_end; # only 1 value desired -> finished
          if (count<=1) goto nv_to_stack_fill; # only 1 value present -> fill with NILs
          count--;
          # at least 2 values desired and present
          {
            var object* mvp = &mv_space[1];
            loop {
              pushSTACK(*mvp++);
              n--; if (n==0) goto nv_to_stack_end; # no further value desired -> finished
              count--; if (count==0) goto nv_to_stack_fill; # no further value present -> fill with NILs
            }
          }
          nv_to_stack_fill: # fill up with n>0 NILs as additional values:
          dotimespL(n,n, { pushSTACK(NIL); } );
          nv_to_stack_end: ;
        }
        goto next_byte;
      CASE cod_mv_to_list:             # (MV-TO-LIST)
        with_saved_context(
          # push values on Stack and handicraft list out of it:
          mv_to_list();
        );
        VALUES1(popSTACK());
        goto next_byte;
      CASE cod_list_to_mv:             # (LIST-TO-MV)
        list_to_mv(value1, GOTO_ERROR(fehler_zuviele_werte));
        goto next_byte;
      CASE cod_mvcallp:                # (MVCALLP)
        pushSP((aint)STACK); # save STACK
        pushSTACK(value1); # save function to be executed
        goto next_byte;
      CASE cod_mvcall:                 # (MVCALL)
        {
          var gcv_object_t* FRAME; popSP( FRAME = (gcv_object_t*) ); # Pointer to Arguments and Function
          var object fun = NEXT(FRAME); # Function
          with_saved_context({
            var uintL argcount = # number of arguments on stack
              STACK_item_count(STACK,FRAME);
            if (((uintL)~(uintL)0 > ca_limit_1) && (argcount > ca_limit_1)) {
              pushSTACK(fun);
              pushSTACK(S(multiple_value_call));
              /* ANSI CL 3.5.1.3. wants a PROGRAM-ERROR here. */
              fehler(program_error,
                     GETTEXT("~S: too many arguments given to ~S"));
            }
            # apply Function, lift Stack until below the Function:
            funcall(fun,argcount);
            skipSTACK(1); # discard Function from STACK
          });
        }
        goto next_byte;
      # ------------------- (10) BLOCK -----------------------
      CASE cod_block_open:             # (BLOCK-OPEN n label)
        # occupies 3 STACK-entries and 1 SP-jmp_buf-entry and 2 SP-entries
        {
          var uintL n;
          var sintL label_dist;
          U_operand(n);
          S_operand(label_dist);
          # create Block_Cons:
          var object block_cons;
          with_saved_context(
            block_cons = allocate_cons();
            label_dist += index; # CODEPTR+label_dist is the jump destination
          );
          # fill Block-Cons: (CONST n) as CAR
          Car(block_cons) = TheCclosure(closure)->clos_consts[n];
          # jump destination into SP:
          pushSP(label_dist); pushSP((aint)closureptr);
          # build up CBLOCK-Frame:
          {
            var gcv_object_t* top_of_frame = STACK; # Pointer above Frame
            pushSTACK(block_cons); # Cons ( (CONST n) . ...)
            var JMPBUF_on_SP(returner); # memorize return-point
            finish_entry_frame_1(CBLOCK,returner, goto block_return; );
          }
          # store Framepointer in Block-Cons:
          Cdr(block_cons) = make_framepointer(STACK);
        }
        goto next_byte;
       block_return: # jump to this label takes place, if the previously
                     # built CBLOCK-Frame has catched a RETURN-FROM.
        {
          FREE_JMPBUF_on_SP();
          skipSTACK(2); # unwind CBLOCK-Frame and mark
          Cdr(popSTACK()) = disabled; # Block-Cons as Disabled
          var uintL index;
          # get closure back, byteptr:=label_byteptr :
          popSP(closureptr = (gcv_object_t*) ); popSP(index = );
          closure = *closureptr; # fetch Closure from Stack
          codeptr = TheSbvector(TheCclosure(closure)->clos_codevec);
          DEBUG_CHECK_BYTEPTR(CODEPTR + index);
          byteptr = CODEPTR + index;
        }
        goto next_byte; # continue interpretation at Label
      CASE cod_block_close:            # (BLOCK-CLOSE)
        # unwind CBLOCK-Frame:
        #if STACKCHECKC
        if (!(framecode(STACK_0) == CBLOCK_frame_info))
          GOTO_ERROR(fehler_STACK_putt);
        #endif
        {
          FREE_JMPBUF_on_SP();
          skipSTACK(2); # unwind CBLOCK-Frame and mark
          Cdr(popSTACK()) = disabled; # Block-Cons as Disabled
          skipSP(2); # we know Destination-Closureptr and Destination-Label
        }
        goto next_byte; # at Label continue interpretation
      CASE cod_return_from:            # (RETURN-FROM n)
        {
          var uintL n;
          U_operand(n);
          var object block_cons = TheCclosure(closure)->clos_consts[n];
          if (eq(Cdr(block_cons),disabled))
            fehler_block_left(Car(block_cons));
          # unwind upto Block-Frame, then jump to its routine for freeing:
          #ifndef FAST_SP
          FREE_DYNAMIC_ARRAY(private_SP_space);
          #endif
          unwind_upto(uTheFramepointer(Cdr(block_cons)));
        }
      CASE cod_return_from_i:          # (RETURN-FROM-I k1 k2 n)
        {
          var uintL k1;
          var uintL k2;
          var uintL n;
          U_operand(k1);
          U_operand(k2);
          U_operand(n);
          var gcv_object_t* FRAME = (gcv_object_t*) SP_(k1+jmpbufsize*k2);
          var object block_cons = FRAME_(n);
          if (eq(Cdr(block_cons),disabled))
            fehler_block_left(Car(block_cons));
          # unwind upto Block-Frame, then jump to its routine for freeing:
          #ifndef FAST_SP
          FREE_DYNAMIC_ARRAY(private_SP_space);
          #endif
          unwind_upto(uTheFramepointer(Cdr(block_cons)));
        }
      # ------------------- (11) TAGBODY -----------------------
      CASE cod_tagbody_open:           # (TAGBODY-OPEN n label1 ... labelm)
        # occupies 3+m STACK-Entries and 1 SP-jmp_buf-Entry and 1 SP-Entry
        {
          var uintL n;
          U_operand(n);
          # create Tagbody-Cons:
          var object tagbody_cons;
          with_saved_context(
            tagbody_cons = allocate_cons();
          );
          # fill Tagbody-Cons: Tag-Vector (CONST n) as CAR
          {
            var object tag_vector = TheCclosure(closure)->clos_consts[n];
            var uintL m = Svector_length(tag_vector);
            Car(tagbody_cons) = tag_vector;
            get_space_on_STACK(m*sizeof(gcv_object_t)); # allocate space
          # push all labeli as Fixnums on the STACK:
            var uintL count;
            dotimespL(count,m, {
              var const uintB* label_byteptr;
              L_operand(label_byteptr);
              pushSTACK(fixnum(label_byteptr - CODEPTR));
            });
          }
          # jump destination in the SP:
          pushSP((aint)closureptr);
          # build upCTAGBODY-Frame:
          {
            var gcv_object_t* top_of_frame = STACK; # Pointer above Frame
            pushSTACK(tagbody_cons); # Cons ( (CONST n) . ...)
            var JMPBUF_on_SP(returner); # memorize return-point
            finish_entry_frame_1(CTAGBODY,returner, goto tagbody_go; );
          }
          # store Framepointer in Tagbody-Cons:
          Cdr(tagbody_cons) = make_framepointer(STACK);
        }
        goto next_byte;
       tagbody_go: # jump to this label takes place, if the previously
                   # built CTAGBODY-Frame has catched a GO to Label nr. i.
        {
          var uintL m = Svector_length(Car(STACK_2)); # Number of Labels
          # (I could also declare the m above as 'auto' and use it here.)
          var uintL i = posfixnum_to_V(value1); # Number of Labels
          var uintL index = posfixnum_to_V(STACK_((m-i)+3)); # labeli
          # get closure back, byteptr:=labeli_byteptr :
          closureptr = (gcv_object_t*) SP_(jmpbufsize+0);
          closure = *closureptr; # fetch Closure from Stack
          codeptr = TheSbvector(TheCclosure(closure)->clos_codevec);
          DEBUG_CHECK_BYTEPTR(CODEPTR + index);
          byteptr = CODEPTR + index;
        }
        goto next_byte; # continue interpretation at Label i
      CASE cod_tagbody_close_nil:      # (TAGBODY-CLOSE-NIL)
        VALUES1(NIL); /* value of Tagbody is NIL */
      CASE cod_tagbody_close:          # (TAGBODY-CLOSE)
        # unwind CTAGBODY-Frame:
        #if STACKCHECKC
        if (!(framecode(STACK_0) == CTAGBODY_frame_info))
          GOTO_ERROR(fehler_STACK_putt);
        #endif
        {
          FREE_JMPBUF_on_SP();
          var object tagbody_cons = STACK_2; # Tagbody-Cons
          Cdr(tagbody_cons) = disabled; # mark as Disabled
          skipSTACK(3+Svector_length(Car(tagbody_cons)));
          skipSP(1);
        }
        goto next_byte;
      CASE cod_go:                     # (GO n l)
        {
          var uintL n;
          var uintL l;
          U_operand(n);
          U_operand(l);
          var object tagbody_cons = # (CONST n)
            TheCclosure(closure)->clos_consts[n];
          if (eq(Cdr(tagbody_cons),disabled)) {
            var object tag_vector = Car(tagbody_cons);
            pushSTACK(tag_vector);
            pushSTACK(TheSvector(tag_vector)->data[l]); # label l
            pushSTACK(S(go));
            fehler(control_error,
                   GETTEXT("(~S ~S): the tagbody of the tags ~S has already been left"));
          }
          # value passed to the Tagbody:
          # For CTAGBODY-Frames: 1+l as Fixnum,
          # For ITAGBODY-Frames: the form-list for Tag nr. l.
          var gcv_object_t* FRAME = uTheFramepointer(Cdr(tagbody_cons));
          VALUES1(framecode(FRAME_(0)) == CTAGBODY_frame_info
                 ? fixnum(1+l)
                 : (object)FRAME_(frame_bindings+2*l+1));
          # unwind upto Tagbody-Frame, then jump to its Routine,
          # which then jumps to Label l:
          #ifndef FAST_SP
          FREE_DYNAMIC_ARRAY(private_SP_space);
          #endif
          unwind_upto(FRAME);
        }
      CASE cod_go_i:                   # (GO-I k1 k2 n l)
        {
          var uintL k1;
          var uintL k2;
          var uintL n;
          var uintL l;
          U_operand(k1);
          U_operand(k2);
          U_operand(n);
          U_operand(l);
          var gcv_object_t* FRAME = (gcv_object_t*) SP_(k1+jmpbufsize*k2);
          var object tagbody_cons = FRAME_(n);
          if (eq(Cdr(tagbody_cons),disabled)) {
            var object tag_vector = Car(tagbody_cons);
            pushSTACK(tag_vector);
            pushSTACK(TheSvector(tag_vector)->data[l]); # label l
            pushSTACK(S(go));
            fehler(control_error,
                   GETTEXT("(~S ~S): the tagbody of the tags ~S has already been left"));
          }
          # value passed to Tagbody:
          # For CTAGBODY-Frames 1+l as Fixnum.
          FRAME = uTheFramepointer(Cdr(tagbody_cons));
          VALUES1(fixnum(1+l));
          # unwind upto Tagbody-Frame, then jump to its Routine,
          # which then jumps to Label l:
          #ifndef FAST_SP
          FREE_DYNAMIC_ARRAY(private_SP_space);
          #endif
          unwind_upto(FRAME);
        }
      # ------------------- (12) CATCH and THROW -----------------------
      CASE cod_catch_open:             # (CATCH-OPEN label)
        # occupies 3 STACK-Entries and 1 SP-jmp_buf-Entry and 2 SP-Entries
        {
          var const uintB* label_byteptr;
          L_operand(label_byteptr);
          # save closureptr, label_byteptr:
          pushSP(label_byteptr - CODEPTR); pushSP((aint)closureptr);
        }
        # build up Frame:
        {
          var gcv_object_t* top_of_frame = STACK;
          pushSTACK(value1); # Tag
          var JMPBUF_on_SP(returner); # memorize return-point
          finish_entry_frame_1(CATCH,returner, goto catch_return; );
        }
        goto next_byte;
       catch_return: # jump to this label takes place, if the previoulsy
                     # built Catch-Frame has catched a Throw.
        {
          FREE_JMPBUF_on_SP();
          skipSTACK(3); # unwind CATCH-Frame
          var uintL index;
          # get closure back, byteptr:=label_byteptr :
          popSP(closureptr = (gcv_object_t*) ); popSP(index = );
          closure = *closureptr; # fetch Closure from Stack
          codeptr = TheSbvector(TheCclosure(closure)->clos_codevec);
          DEBUG_CHECK_BYTEPTR(CODEPTR + index);
          byteptr = CODEPTR + index;
        }
        goto next_byte; # continue interpretation at Label
      CASE cod_catch_close:            # (CATCH-CLOSE)
        # a CATCH-Frame has to come:
        #if STACKCHECKC
        if (!(framecode(STACK_0) == CATCH_frame_info))
          GOTO_ERROR(fehler_STACK_putt);
        #endif
        FREE_JMPBUF_on_SP();
        #if STACKCHECKC
        if (!(closureptr == (gcv_object_t*)SP_(0))) # that Closureptr must be the current one
          GOTO_ERROR(fehler_STACK_putt);
        #endif
        skipSP(2); skipSTACK(3); # unwind CATCH-Frame
        goto next_byte;
      CASE cod_throw:                  # (THROW)
        {
          var object tag = popSTACK();
          throw_to(tag);
          pushSTACK(tag);
          pushSTACK(S(throw));
          fehler(control_error,
                 GETTEXT("~S: there is no CATCHer for tag ~S"));
        }
      # ------------------- (13) UNWIND-PROTECT -----------------------
      CASE cod_uwp_open:               # (UNWIND-PROTECT-OPEN label)
        # occupies 2 STACK-Entries and 1 SP-jmp_buf-Entry and 2 SP-Entries
        {
          var const uintB* label_byteptr;
          L_operand(label_byteptr);
          # save closureptr, label_byteptr:
          pushSP(label_byteptr - CODEPTR); pushSP((aint)closureptr);
        }
        # build Frame:
        {
          var gcv_object_t* top_of_frame = STACK;
          var JMPBUF_on_SP(returner); # memorize return-point
          finish_entry_frame_1(UNWIND_PROTECT,returner, goto throw_save; );
        }
        goto next_byte;
       throw_save: # jump to this label takes place, if the previously
                   # built Unwind-Protect-Frame has stopped a Throw.
        # unwind_protect_to_save is to be saved and jumped to at the end.
        #if STACKCHECKC
        if (!(framecode(STACK_0) == UNWIND_PROTECT_frame_info)) {
          fehler(serious_condition,GETTEXT("STACK corrupted"));
        }
        #endif
        # unwind Frame:
        FREE_JMPBUF_on_SP();
        skipSTACK(2);
        {
          var uintL index;
          # get closure back, byteptr:=label_byteptr :
          popSP(closureptr = (gcv_object_t*) );
          popSP(index = );
          # save unwind_protect_to_save:
          pushSP((aint)unwind_protect_to_save.fun);
          pushSP((aint)unwind_protect_to_save.upto_frame);
          pushSP((aint)STACK); # push Pointer above Frame additionally on the SP
          # move all values to the Stack:
          mv_to_STACK();
          # execute Cleanup-Forms:
          closure = *closureptr; # fetch Closure from Stack
          codeptr = TheSbvector(TheCclosure(closure)->clos_codevec);
          DEBUG_CHECK_BYTEPTR(CODEPTR + index);
          byteptr = CODEPTR + index;
        }
        goto next_byte;
      CASE cod_uwp_normal_exit:        # (UNWIND-PROTECT-NORMAL-EXIT)
        #if STACKCHECKC
        if (!(framecode(STACK_0) == UNWIND_PROTECT_frame_info))
          GOTO_ERROR(fehler_STACK_putt);
        if (!(closureptr == (gcv_object_t*)SP_(jmpbufsize+0))) # that Closureptr must be the current one
          GOTO_ERROR(fehler_STACK_putt);
        #endif
        # unwind Frame:
        # nothing to do, because closure and byteptr stay unmodified.
        FREE_JMPBUF_on_SP(); skipSP(2);
        skipSTACK(2);
        # dummy value for 'unwind_protect_to_save':
        pushSP((aint)NULL); pushSP((aint)NULL); # NULL,NULL -> uwp_continue
        pushSP((aint)STACK); # push Pointer above Frame additionally on the SP
        # move all values to the Stack:
        mv_to_STACK();
        # execute Cleanup-Forms:
        goto next_byte;
      CASE cod_uwp_close:              # (UNWIND-PROTECT-CLOSE)
        # jump to this labe takes place at the end of the Cleanup-Forms.
        {
          var gcv_object_t* oldSTACK; # value of STACK before saveing the values
          popSP( oldSTACK = (gcv_object_t*) );
          var uintL mvcount = # number of saved values on Stack
            STACK_item_count(STACK,oldSTACK);
          if (mvcount >= mv_limit) GOTO_ERROR(fehler_zuviele_werte);
          STACK_to_mv(mvcount);
        }
        { /* return to the saved unwind_protect_to_save.fun : */
          var restartf_t fun;
          var gcv_object_t* arg;
          popSP( arg = (gcv_object_t*) ); popSP( fun = (restartf_t) );
          # return to uwp_continue or uwp_jmpback or unwind_upto:
          if (fun != NULL) {
            (*fun)(arg); # return to unwind_upto or similar.
            NOTREACHED;
          }
          if (arg == (gcv_object_t*)NULL) {
            # uwp_continue:
            # jump to this label takes place, if after the execution of
            # the Cleanup-Forms simply interpretation shall continue.
            goto next_byte;
          } else {
            # uwp_jmpback:
            # jump to this label takes place, if after the execution of
            # the Cleanup-Forms interpretation shall continue at the old
            # location in the same Closure.
            DEBUG_CHECK_BYTEPTR(CODEPTR + (uintP)arg);
            byteptr = CODEPTR + (uintP)arg;
            goto next_byte;
          }
        }
      CASE cod_uwp_cleanup:            # (UNWIND-PROTECT-CLEANUP)
        # this is executed, if within the same Closure an execution
        # of the Cleanup-Code is necessary.
        #if STACKCHECKC
        if (!(framecode(STACK_0) == UNWIND_PROTECT_frame_info))
          GOTO_ERROR(fehler_STACK_putt);
        if (!(closureptr == (gcv_object_t*)SP_(jmpbufsize+0))) # that Closureptr must be the current one
          GOTO_ERROR(fehler_STACK_putt);
        #endif
        # closure remains, byteptr:=label_byteptr :
        {
          var uintL index = SP_(jmpbufsize+1);
          # unwind Frame:
          FREE_JMPBUF_on_SP(); skipSP(2);
          skipSTACK(2);
          # Dummy-values for 'unwind_protect_to_save':
          pushSP((aint)NULL); # NULL -> uwp_jmpback
          pushSP(byteptr - CODEPTR);
          pushSP((aint)STACK); # push Pointer above Frame additionally on the SP
          # move all values to the Stack:
          mv_to_STACK();
          # execute Cleanup-Forms:
          DEBUG_CHECK_BYTEPTR(CODEPTR + index);
          byteptr = CODEPTR + index;
        }
        goto next_byte;
      # ------------------- (14) HANDLER-BIND -----------------------
      CASE cod_handler_open:           # (HANDLER-OPEN n)
        # occupies 4 STACK-Entries
        {
          var uintL n;
          U_operand(n);
          # build up Frame:
          var gcv_object_t* top_of_frame = STACK; # Pointer above Frame
          pushSTACK(TheCclosure(closure)->clos_consts[n]);
          pushSTACK(closure);
          pushSTACK(fake_gcv_object((aint)(_SP_(0))));
          finish_frame(HANDLER);
        }
        goto next_byte;
      CASE cod_handler_begin_push:     # (HANDLER-BEGIN&PUSH)
        # builds up SP newly, occupies 1 SP-Entry and
        # starts a new STACK-Region.
        {
          var uintL count = (uintL)posfixnum_to_V(Car(handler_args.spdepth))
                            + jmpbufsize * (uintL)posfixnum_to_V(Cdr(handler_args.spdepth));
          if (count > 0) {
            var SPint* oldsp = handler_args.sp; # was formerly &SP_(0)
            # copy oldsp[0..count-1] to the current SP:
            oldsp skipSPop count;
            dotimespL(count,count, { oldsp skipSPop -1; pushSP(*oldsp); } );
          }
        }
        pushSP((aint)handler_args.stack); # Pointer above Handler-Frame
        VALUES1(handler_args.condition);
        pushSTACK(value1);
        goto next_byte;
      # ------------------- (15) a few Functions -----------------------
      CASE cod_not:                    # (NOT)
        if (nullp(value1)) goto code_t; else goto code_nil;
      CASE cod_eq:                     # (EQ)
        if (!eq(value1,popSTACK())) goto code_nil; else goto code_t;
      CASE cod_car:                    # (CAR)
        {
          var object arg = value1;
          if (consp(arg)) {
            value1 = Car(arg); # CAR of a Cons
          } else if (nullp(arg)) {
            # (CAR NIL) = NIL: value1 remains NIL
          } else
            with_saved_back_trace_subr(L(car),STACK STACKop -1,-1,
              fehler_list(arg); );
          mv_count=1;
        }
        goto next_byte;
      CASE cod_car_push:               # (CAR&PUSH)
        {
          var object arg = value1;
          if (consp(arg)) {
            pushSTACK(Car(arg)); # CAR of a Cons
          } else if (nullp(arg)) {
            pushSTACK(arg); # (CAR NIL) = NIL
          } else
            with_saved_back_trace_subr(L(car),STACK STACKop -1,-1,
              fehler_list(arg); );
        }
        goto next_byte;
      CASE cod_load_car_push:          # (LOAD&CAR&PUSH n)
        {
          var uintL n;
          U_operand(n);
          var object arg = STACK_(n);
          if (consp(arg)) {
            pushSTACK(Car(arg)); # CAR of a Cons
          } else if (nullp(arg)) {
            pushSTACK(arg); # (CAR NIL) = NIL
          } else
            with_saved_back_trace_subr(L(car),STACK STACKop -1,-1,
              fehler_list(arg); );
        }
        goto next_byte;
      CASE cod_load_car_store:         # (LOAD&CAR&STORE m n)
        {
          var uintL m;
          var uintL n;
          U_operand(m);
          U_operand(n);
          var object arg = STACK_(m);
          if (consp(arg)) {
            STACK_(n) = value1 = Car(arg); # CAR of a Cons
          } else if (nullp(arg)) {
            STACK_(n) = value1 = arg; # (CAR NIL) = NIL
          } else
            with_saved_back_trace_subr(L(car),STACK STACKop -1,-1,
              fehler_list(arg); );
          mv_count=1;
        }
        goto next_byte;
      CASE cod_cdr:                    # (CDR)
        {
          var object arg = value1;
          if (consp(arg)) {
            value1 = Cdr(arg); # CDR of a Cons
          } else if (nullp(arg)) {
            # (CDR NIL) = NIL: value1 remains NIL
          } else
            with_saved_back_trace_subr(L(cdr),STACK STACKop -1,-1,
              fehler_list(arg); );
          mv_count=1;
        }
        goto next_byte;
      CASE cod_cdr_push:               # (CDR&PUSH)
        {
          var object arg = value1;
          if (consp(arg)) {
            pushSTACK(Cdr(arg)); # CDR of a Cons
          } else if (nullp(arg)) {
            pushSTACK(arg); # (CDR NIL) = NIL
          } else
            with_saved_back_trace_subr(L(cdr),STACK STACKop -1,-1,
              fehler_list(arg); );
        }
        goto next_byte;
      CASE cod_load_cdr_push:          # (LOAD&CDR&PUSH n)
        {
          var uintL n;
          U_operand(n);
          var object arg = STACK_(n);
          if (consp(arg)) {
            pushSTACK(Cdr(arg)); # CDR of a Cons
          } else if (nullp(arg)) {
            pushSTACK(arg); # (CDR NIL) = NIL
          } else
            with_saved_back_trace_subr(L(cdr),STACK STACKop -1,-1,
              fehler_list(arg); );
        }
        goto next_byte;
      CASE cod_load_cdr_store:         # (LOAD&CDR&STORE n)
        {
          var uintL n;
          U_operand(n);
          var gcv_object_t* arg_ = &STACK_(n);
          var object arg = *arg_;
          if (consp(arg)) {
            *arg_ = value1 = Cdr(arg); # CDR of a Cons
          } else if (nullp(arg)) {
            value1 = arg; # (CDR NIL) = NIL
          } else
            with_saved_back_trace_subr(L(cdr),STACK STACKop -1,-1,
              fehler_list(arg); );
          mv_count=1;
        }
        goto next_byte;
      CASE cod_cons:                   # (CONS)
        {
          pushSTACK(value1);
          # request Cons:
          var object new_cons;
          with_saved_context( { new_cons = allocate_cons(); } );
          # fill Cons:
          Cdr(new_cons) = popSTACK();
          Car(new_cons) = popSTACK();
          VALUES1(new_cons);
        }
        goto next_byte;
      CASE cod_cons_push:              # (CONS&PUSH)
        {
          pushSTACK(value1);
          # request Cons:
          var object new_cons;
          with_saved_context( { new_cons = allocate_cons(); } );
          # fill Cons:
          Cdr(new_cons) = popSTACK();
          Car(new_cons) = STACK_0;
          STACK_0 = new_cons;
        }
        goto next_byte;
      CASE cod_load_cons_store:        # (LOAD&CONS&STORE n)
        {
          var uintL n;
          U_operand(n);
          # request Cons:
          var object new_cons;
          with_saved_context( { new_cons = allocate_cons(); } );
          # fill Cons:
          Car(new_cons) = popSTACK();
          var gcv_object_t* arg_ = &STACK_(n);
          Cdr(new_cons) = *arg_;
          VALUES1(*arg_ = new_cons);
        }
        goto next_byte;
      {var object symbol;
       var object fdef;
       #define CHECK_FDEF()                                                   \
         if (!symbolp(symbol))                                                \
           with_saved_back_trace_subr(L(symbol_function),STACK STACKop -1,-1, \
             symbol = check_symbol(symbol); );                                \
         fdef = Symbol_function(symbol);                                      \
         if (!boundp(fdef))                                                   \
           /* (symbol may be not the actual function-name, for e.g.           \
              (FUNCTION (SETF FOO)) shows as (SYMBOL-FUNCTION '#:|(SETF FOO)|),\
              but that should be enough for the error message.) */            \
           fdef = check_fdefinition(symbol,S(symbol_function))
      CASE cod_symbol_function:        # (SYMBOL-FUNCTION)
        symbol = value1;
        CHECK_FDEF();
        VALUES1(fdef);
        goto next_byte;
      CASE cod_const_symbol_function:  # (CONST&SYMBOL-FUNCTION n)
        {
          var uintL n;
          U_operand(n);
          symbol = TheCclosure(closure)->clos_consts[n];
        }
        CHECK_FDEF();
        VALUES1(fdef);
        goto next_byte;
      CASE cod_const_symbol_function_push: # (CONST&SYMBOL-FUNCTION&PUSH n)
        {
          var uintL n;
          U_operand(n);
          symbol = TheCclosure(closure)->clos_consts[n];
        }
        CHECK_FDEF();
        pushSTACK(fdef);
        goto next_byte;
      CASE cod_const_symbol_function_store: # (CONST&SYMBOL-FUNCTION&STORE n k)
        {
          var uintL n;
          U_operand(n);
          symbol = TheCclosure(closure)->clos_consts[n];
        }
        CHECK_FDEF();
        {
          var uintL k;
          U_operand(k);
          STACK_(k) = value1 = fdef; mv_count=1;
        }
        goto next_byte;
      }
      {var object vec; var object index;
      CASE cod_svref:                  # (SVREF)
        # STACK_0 must be a Simple-Vector:
        if (!simple_vector_p(STACK_0)) goto svref_kein_svector;
        vec = popSTACK(); # Simple-Vector
        index = value1;
        # and the Index must be Fixnum >= 0, < length(vec) :
        {
          var uintV i;
          if (!(posfixnump(index)
                && ((i = posfixnum_to_V(index)) < Svector_length(vec))))
            goto svref_kein_index;
          VALUES1(TheSvector(vec)->data[i]); # indexed Element as value
        }
        goto next_byte;
      CASE cod_svset:                  # (SVSET)
        # STACK_0 must be a Simple-Vector:
        if (!simple_vector_p(STACK_0)) goto svref_kein_svector;
        vec = popSTACK(); # Simple-Vector
        index = value1;
        # and the Index must be a Fixnum >=0, <Length(vec) :
        {
          var uintV i;
          if (!(posfixnump(index)
                && ((i = posfixnum_to_V(index)) < Svector_length(vec))))
            goto svref_kein_index;
          value1 = TheSvector(vec)->data[i] = popSTACK(); # put in new element
          mv_count = 1;
        }
        goto next_byte;
      svref_kein_svector: # Non-Simple-Vector in STACK_0
        fehler_kein_svector(S(svref),STACK_0);
      svref_kein_index: # unsuitable Index in index, for Vector vec
        pushSTACK(vec);
        pushSTACK(index);
        pushSTACK(index); # TYPE-ERROR slot DATUM
        {
          var object tmp;
          pushSTACK(S(integer)); pushSTACK(Fixnum_0); pushSTACK(UL_to_I(Svector_length(vec)));
          tmp = listof(1); pushSTACK(tmp); tmp = listof(3);
          pushSTACK(tmp); # TYPE-ERROR slot EXPECTED-TYPE
        }
        pushSTACK(STACK_(1+2)); # vec
        pushSTACK(STACK_(0+3)); # index
        pushSTACK(S(svref));
        fehler(type_error,GETTEXT("~S: ~S is not a correct index into ~S"));
      }
      CASE cod_list:                   # (LIST n)
        {
          var uintC n;
          U_operand(n);
          with_saved_context( { value1 = listof(n); mv_count=1; } );
        }
        goto next_byte;
      CASE cod_list_push:              # (LIST&PUSH n)
        {
          var uintC n;
          U_operand(n);
          with_saved_context( { object res = listof(n); pushSTACK(res); } );
        }
        goto next_byte;
      CASE cod_liststern:              # (LIST* n)
        {
          var uintC n;
          U_operand(n);
          with_saved_context({
            pushSTACK(value1);
            dotimespC(n,n, {
              var object new_cons = allocate_cons();
              Cdr(new_cons) = popSTACK();
              Car(new_cons) = STACK_0;
              STACK_0 = new_cons;
            });
            value1 = popSTACK(); mv_count=1;
          });
        }
        goto next_byte;
      CASE cod_liststern_push:         # (LIST*&PUSH n)
        {
          var uintC n;
          U_operand(n);
          with_saved_context({
            pushSTACK(value1);
            dotimespC(n,n, {
              var object new_cons = allocate_cons();
              Cdr(new_cons) = popSTACK();
              Car(new_cons) = STACK_0;
              STACK_0 = new_cons;
            });
          });
        }
        goto next_byte;
      # ------------------- (16) combined Operations -----------------------
      CASE cod_nil_store:              # (NIL&STORE n)
        {
          var uintL n;
          U_operand(n);
          STACK_(n) = value1 = NIL; mv_count=1;
        }
        goto next_byte;
      CASE cod_t_store:                # (T&STORE n)
        {
          var uintL n;
          U_operand(n);
          STACK_(n) = value1 = T; mv_count=1;
        }
        goto next_byte;
      CASE cod_calls1_store:           # (CALLS1&STORE n k)
        CALLS1();
        goto store;
      CASE cod_calls2_store:           # (CALLS2&STORE n k)
        CALLS2();
        goto store;
      CASE cod_callsr_store:           # (CALLSR&STORE m n k)
        CALLSR();
        goto store;
      # Increment. Optimized specifically for Fixnums >=0.
      #define INC(arg,statement)  \
        { if (posfixnump(arg) # Fixnum >= 0 and < most-positive-fixnum ? \
              && !eq(arg,fixnum(vbitm(oint_data_len)-1))) {              \
            arg = fixnum_inc(arg,1); statement;                          \
          } else {                                                       \
            with_saved_context(                                          \
              /* funcall(L(einsplus),1): */                              \
              pushSTACK(arg);                                            \
              with_saved_back_trace_subr(L(einsplus),STACK,1,            \
                { C_einsplus(); });                                      \
            );                                                           \
            arg = value1;                                                \
          }                                                              \
        }
      # Decrement. Optimized specifically for Fixnums >=0.
      #define DEC(arg,statement)  \
        { if (posfixnump(arg) && !eq(arg,Fixnum_0)) { # Fixnum > 0 ? \
            arg = fixnum_inc(arg,-1); statement;                     \
          } else {                                                   \
            with_saved_context(                                      \
              /* funcall(L(einsminus),1): */                         \
              pushSTACK(arg);                                        \
              with_saved_back_trace_subr(L(einsminus),STACK,1,       \
                { C_einsminus(); });                                 \
            );                                                       \
            arg = value1;                                            \
          }                                                          \
        }
      CASE cod_load_inc_push:          # (LOAD&INC&PUSH n)
        {
          var uintL n;
          U_operand(n);
          var object arg = STACK_(n);
          INC(arg,); # increment
          pushSTACK(arg);
        }
        goto next_byte;
      CASE cod_load_inc_store:         # (LOAD&INC&STORE n)
        {
          var uintL n;
          U_operand(n);
          var gcv_object_t* arg_ = &STACK_(n);
          var object arg = *arg_;
          INC(arg,mv_count=1); # increment, one value
          value1 = *arg_ = arg;
        }
        goto next_byte;
      CASE cod_load_dec_push:          # (LOAD&DEC&PUSH n)
        {
          var uintL n;
          U_operand(n);
          var object arg = STACK_(n);
          DEC(arg,); # decrement
          pushSTACK(arg);
        }
        goto next_byte;
      CASE cod_load_dec_store:         # (LOAD&DEC&STORE n)
        {
          var uintL n;
          U_operand(n);
          var gcv_object_t* arg_ = &STACK_(n);
          var object arg = *arg_;
          DEC(arg,mv_count=1); # decrement, one value
          value1 = *arg_ = arg;
        }
        goto next_byte;
      CASE cod_call1_jmpif:            # (CALL1&JMPIF n label)
        CALL1();
        if (!nullp(value1)) goto jmp; else goto notjmp;
      CASE cod_call1_jmpifnot:         # (CALL1&JMPIFNOT n label)
        CALL1();
        if (nullp(value1)) goto jmp; else goto notjmp;
      CASE cod_call2_jmpif:            # (CALL2&JMPIF n label)
        CALL2();
        if (!nullp(value1)) goto jmp; else goto notjmp;
      CASE cod_call2_jmpifnot:         # (CALL2&JMPIFNOT n label)
        CALL2();
        if (nullp(value1)) goto jmp; else goto notjmp;
      CASE cod_calls1_jmpif:           # (CALLS1&JMPIF n label)
        CALLS1();
        if (!nullp(value1)) goto jmp; else goto notjmp;
      CASE cod_calls1_jmpifnot:        # (CALLS1&JMPIFNOT n label)
        CALLS1();
        if (nullp(value1)) goto jmp; else goto notjmp;
      CASE cod_calls2_jmpif:           # (CALLS2&JMPIF n label)
        CALLS2();
        if (!nullp(value1)) goto jmp; else goto notjmp;
      CASE cod_calls2_jmpifnot:        # (CALLS2&JMPIFNOT n label)
        CALLS2();
        if (nullp(value1)) goto jmp; else goto notjmp;
      CASE cod_callsr_jmpif:           # (CALLSR&JMPIF m n label)
        CALLSR();
        if (!nullp(value1)) goto jmp; else goto notjmp;
      CASE cod_callsr_jmpifnot:        # (CALLSR&JMPIFNOT m n label)
        CALLSR();
        if (nullp(value1)) goto jmp; else goto notjmp;
      CASE cod_load_jmpif:             # (LOAD&JMPIF n label)
        {
          var uintL n;
          U_operand(n);
          mv_count=1;
          if (!nullp(value1 = STACK_(n))) goto jmp; else goto notjmp;
        }
      CASE cod_load_jmpifnot:          # (LOAD&JMPIFNOT n label)
        {
          var uintL n;
          U_operand(n);
          mv_count=1;
          if (nullp(value1 = STACK_(n))) goto jmp; else goto notjmp;
        }
      CASE cod_apply_skip_ret:         # (APPLY&SKIP&RET n k)
        {
          var uintL n;
          var uintL k;
          U_operand(n);
          U_operand(k);
          var object fun = STACK_(n); # Function
          with_saved_context({
            apply(fun,n,value1); # call Function
            skipSTACK(k+1); # discard Function and others from Stack
            goto finished; # return (jump) to caller
          }); # the context is not restored
        }
      CASE cod_funcall_skip_retgf:     # (FUNCALL&SKIP&RETGF n k)
        {
          var uintL n;
          var uintL k;
          U_operand(n);
          U_operand(k);
          var object fun = STACK_(n); # Function
          var uintL r = ((Codevec)codeptr)->ccv_numreq;
          var uintB flags = ((Codevec)codeptr)->ccv_flags;
          with_saved_context({
            funcall(fun,n); # call Function
            if (flags & bit(3)) { # call inhibition?
              skipSTACK(k+1);
              mv_count=1;
              goto finished; # return (jump) to caller
            }
            k -= r;
            if (flags & bit(0)) {
              skipSTACK(k); apply(value1,r,popSTACK());
            } else {
              skipSTACK(k+1); funcall(value1,r);
            }
            goto finished; # return (jump) to caller
          }); # the context is not restored
        }
      # ------------------- (17) short codes -----------------------
      CASE cod_load0:                  # (LOAD.S 0)
        VALUES1(STACK_(0));
        goto next_byte;
      CASE cod_load1:                  # (LOAD.S 1)
        VALUES1(STACK_(1));
        goto next_byte;
      CASE cod_load2:                  # (LOAD.S 2)
        VALUES1(STACK_(2));
        goto next_byte;
      CASE cod_load3:                  # (LOAD.S 3)
        VALUES1(STACK_(3));
        goto next_byte;
      CASE cod_load4:                  # (LOAD.S 4)
        VALUES1(STACK_(4));
        goto next_byte;
      CASE cod_load5:                  # (LOAD.S 5)
        VALUES1(STACK_(5));
        goto next_byte;
      CASE cod_load6:                  # (LOAD.S 6)
        VALUES1(STACK_(6));
        goto next_byte;
      CASE cod_load7:                  # (LOAD.S 7)
        VALUES1(STACK_(7));
        goto next_byte;
      CASE cod_load8:                  # (LOAD.S 8)
        VALUES1(STACK_(8));
        goto next_byte;
      CASE cod_load9:                  # (LOAD.S 9)
        VALUES1(STACK_(9));
        goto next_byte;
      CASE cod_load10:                 # (LOAD.S 10)
        VALUES1(STACK_(10));
        goto next_byte;
      CASE cod_load11:                 # (LOAD.S 11)
        VALUES1(STACK_(11));
        goto next_byte;
      CASE cod_load12:                 # (LOAD.S 12)
        VALUES1(STACK_(12));
        goto next_byte;
      CASE cod_load13:                 # (LOAD.S 13)
        VALUES1(STACK_(13));
        goto next_byte;
      CASE cod_load14:                 # (LOAD.S 14)
        VALUES1(STACK_(14));
        goto next_byte;
      #if 0
      CASE cod_load15:                 # (LOAD.S 15)
        VALUES1(STACK_(15));
        goto next_byte;
      CASE cod_load16:                 # (LOAD.S 16)
        VALUES1(STACK_(16));
        goto next_byte;
      CASE cod_load17:                 # (LOAD.S 17)
        VALUES1(STACK_(17));
        goto next_byte;
      CASE cod_load18:                 # (LOAD.S 18)
        VALUES1(STACK_(18));
        goto next_byte;
      CASE cod_load19:                 # (LOAD.S 19)
        VALUES1(STACK_(19));
        goto next_byte;
      CASE cod_load20:                 # (LOAD.S 20)
        VALUES1(STACK_(20));
        goto next_byte;
      CASE cod_load21:                 # (LOAD.S 21)
        VALUES1(STACK_(21));
        goto next_byte;
      #endif
      CASE cod_load_push0:             # (LOAD&PUSH.S 0)
        pushSTACK(STACK_(0));
        goto next_byte;
      CASE cod_load_push1:             # (LOAD&PUSH.S 1)
        pushSTACK(STACK_(1));
        goto next_byte;
      CASE cod_load_push2:             # (LOAD&PUSH.S 2)
        pushSTACK(STACK_(2));
        goto next_byte;
      CASE cod_load_push3:             # (LOAD&PUSH.S 3)
        pushSTACK(STACK_(3));
        goto next_byte;
      CASE cod_load_push4:             # (LOAD&PUSH.S 4)
        pushSTACK(STACK_(4));
        goto next_byte;
      CASE cod_load_push5:             # (LOAD&PUSH.S 5)
        pushSTACK(STACK_(5));
        goto next_byte;
      CASE cod_load_push6:             # (LOAD&PUSH.S 6)
        pushSTACK(STACK_(6));
        goto next_byte;
      CASE cod_load_push7:             # (LOAD&PUSH.S 7)
        pushSTACK(STACK_(7));
        goto next_byte;
      CASE cod_load_push8:             # (LOAD&PUSH.S 8)
        pushSTACK(STACK_(8));
        goto next_byte;
      CASE cod_load_push9:             # (LOAD&PUSH.S 9)
        pushSTACK(STACK_(9));
        goto next_byte;
      CASE cod_load_push10:            # (LOAD&PUSH.S 10)
        pushSTACK(STACK_(10));
        goto next_byte;
      CASE cod_load_push11:            # (LOAD&PUSH.S 11)
        pushSTACK(STACK_(11));
        goto next_byte;
      CASE cod_load_push12:            # (LOAD&PUSH.S 12)
        pushSTACK(STACK_(12));
        goto next_byte;
      CASE cod_load_push13:            # (LOAD&PUSH.S 13)
        pushSTACK(STACK_(13));
        goto next_byte;
      CASE cod_load_push14:            # (LOAD&PUSH.S 14)
        pushSTACK(STACK_(14));
        goto next_byte;
      CASE cod_load_push15:            # (LOAD&PUSH.S 15)
        pushSTACK(STACK_(15));
        goto next_byte;
      CASE cod_load_push16:            # (LOAD&PUSH.S 16)
        pushSTACK(STACK_(16));
        goto next_byte;
      CASE cod_load_push17:            # (LOAD&PUSH.S 17)
        pushSTACK(STACK_(17));
        goto next_byte;
      CASE cod_load_push18:            # (LOAD&PUSH.S 18)
        pushSTACK(STACK_(18));
        goto next_byte;
      CASE cod_load_push19:            # (LOAD&PUSH.S 19)
        pushSTACK(STACK_(19));
        goto next_byte;
      CASE cod_load_push20:            # (LOAD&PUSH.S 20)
        pushSTACK(STACK_(20));
        goto next_byte;
      CASE cod_load_push21:            # (LOAD&PUSH.S 21)
        pushSTACK(STACK_(21));
        goto next_byte;
      CASE cod_load_push22:            # (LOAD&PUSH.S 22)
        pushSTACK(STACK_(22));
        goto next_byte;
      CASE cod_load_push23:            # (LOAD&PUSH.S 23)
        pushSTACK(STACK_(23));
        goto next_byte;
      CASE cod_load_push24:            # (LOAD&PUSH.S 24)
        pushSTACK(STACK_(24));
        goto next_byte;
      CASE cod_const0:                 # (CONST.S 0)
        VALUES1(TheCclosure(closure)->clos_consts[0]);
        goto next_byte;
      CASE cod_const1:                 # (CONST.S 1)
        VALUES1(TheCclosure(closure)->clos_consts[1]);
        goto next_byte;
      CASE cod_const2:                 # (CONST.S 2)
        VALUES1(TheCclosure(closure)->clos_consts[2]);
        goto next_byte;
      CASE cod_const3:                 # (CONST.S 3)
        VALUES1(TheCclosure(closure)->clos_consts[3]);
        goto next_byte;
      CASE cod_const4:                 # (CONST.S 4)
        VALUES1(TheCclosure(closure)->clos_consts[4]);
        goto next_byte;
      CASE cod_const5:                 # (CONST.S 5)
        VALUES1(TheCclosure(closure)->clos_consts[5]);
        goto next_byte;
      CASE cod_const6:                 # (CONST.S 6)
        VALUES1(TheCclosure(closure)->clos_consts[6]);
        goto next_byte;
      CASE cod_const7:                 # (CONST.S 7)
        VALUES1(TheCclosure(closure)->clos_consts[7]);
        goto next_byte;
      CASE cod_const8:                 # (CONST.S 8)
        VALUES1(TheCclosure(closure)->clos_consts[8]);
        goto next_byte;
      CASE cod_const9:                 # (CONST.S 9)
        VALUES1(TheCclosure(closure)->clos_consts[9]);
        goto next_byte;
      CASE cod_const10:                # (CONST.S 10)
        VALUES1(TheCclosure(closure)->clos_consts[10]);
        goto next_byte;
      CASE cod_const11:                # (CONST.S 11)
        VALUES1(TheCclosure(closure)->clos_consts[11]);
        goto next_byte;
      CASE cod_const12:                # (CONST.S 12)
        VALUES1(TheCclosure(closure)->clos_consts[12]);
        goto next_byte;
      CASE cod_const13:                # (CONST.S 13)
        VALUES1(TheCclosure(closure)->clos_consts[13]);
        goto next_byte;
      CASE cod_const14:                # (CONST.S 14)
        VALUES1(TheCclosure(closure)->clos_consts[14]);
        goto next_byte;
      CASE cod_const15:                # (CONST.S 15)
        VALUES1(TheCclosure(closure)->clos_consts[15]);
        goto next_byte;
      CASE cod_const16:                # (CONST.S 16)
        VALUES1(TheCclosure(closure)->clos_consts[16]);
        goto next_byte;
      CASE cod_const17:                # (CONST.S 17)
        VALUES1(TheCclosure(closure)->clos_consts[17]);
        goto next_byte;
      CASE cod_const18:                # (CONST.S 18)
        VALUES1(TheCclosure(closure)->clos_consts[18]);
        goto next_byte;
      CASE cod_const19:                # (CONST.S 19)
        VALUES1(TheCclosure(closure)->clos_consts[19]);
        goto next_byte;
      CASE cod_const20:                # (CONST.S 20)
        VALUES1(TheCclosure(closure)->clos_consts[20]);
        goto next_byte;
      #if 0
      CASE cod_const21:                # (CONST.S 21)
        VALUES1(TheCclosure(closure)->clos_consts[21]);
        goto next_byte;
      CASE cod_const22:                # (CONST.S 22)
        VALUES1(TheCclosure(closure)->clos_consts[22]);
        goto next_byte;
      CASE cod_const23:                # (CONST.S 23)
        VALUES1(TheCclosure(closure)->clos_consts[23]);
        goto next_byte;
      CASE cod_const24:                # (CONST.S 24)
        VALUES1(TheCclosure(closure)->clos_consts[24]);
        goto next_byte;
      #endif
      CASE cod_const_push0:            # (CONST&PUSH.S 0)
        pushSTACK(TheCclosure(closure)->clos_consts[0]);
        goto next_byte;
      CASE cod_const_push1:            # (CONST&PUSH.S 1)
        pushSTACK(TheCclosure(closure)->clos_consts[1]);
        goto next_byte;
      CASE cod_const_push2:            # (CONST&PUSH.S 2)
        pushSTACK(TheCclosure(closure)->clos_consts[2]);
        goto next_byte;
      CASE cod_const_push3:            # (CONST&PUSH.S 3)
        pushSTACK(TheCclosure(closure)->clos_consts[3]);
        goto next_byte;
      CASE cod_const_push4:            # (CONST&PUSH.S 4)
        pushSTACK(TheCclosure(closure)->clos_consts[4]);
        goto next_byte;
      CASE cod_const_push5:            # (CONST&PUSH.S 5)
        pushSTACK(TheCclosure(closure)->clos_consts[5]);
        goto next_byte;
      CASE cod_const_push6:            # (CONST&PUSH.S 6)
        pushSTACK(TheCclosure(closure)->clos_consts[6]);
        goto next_byte;
      CASE cod_const_push7:            # (CONST&PUSH.S 7)
        pushSTACK(TheCclosure(closure)->clos_consts[7]);
        goto next_byte;
      CASE cod_const_push8:            # (CONST&PUSH.S 8)
        pushSTACK(TheCclosure(closure)->clos_consts[8]);
        goto next_byte;
      CASE cod_const_push9:            # (CONST&PUSH.S 9)
        pushSTACK(TheCclosure(closure)->clos_consts[9]);
        goto next_byte;
      CASE cod_const_push10:           # (CONST&PUSH.S 10)
        pushSTACK(TheCclosure(closure)->clos_consts[10]);
        goto next_byte;
      CASE cod_const_push11:           # (CONST&PUSH.S 11)
        pushSTACK(TheCclosure(closure)->clos_consts[11]);
        goto next_byte;
      CASE cod_const_push12:           # (CONST&PUSH.S 12)
        pushSTACK(TheCclosure(closure)->clos_consts[12]);
        goto next_byte;
      CASE cod_const_push13:           # (CONST&PUSH.S 13)
        pushSTACK(TheCclosure(closure)->clos_consts[13]);
        goto next_byte;
      CASE cod_const_push14:           # (CONST&PUSH.S 14)
        pushSTACK(TheCclosure(closure)->clos_consts[14]);
        goto next_byte;
      CASE cod_const_push15:           # (CONST&PUSH.S 15)
        pushSTACK(TheCclosure(closure)->clos_consts[15]);
        goto next_byte;
      CASE cod_const_push16:           # (CONST&PUSH.S 16)
        pushSTACK(TheCclosure(closure)->clos_consts[16]);
        goto next_byte;
      CASE cod_const_push17:           # (CONST&PUSH.S 17)
        pushSTACK(TheCclosure(closure)->clos_consts[17]);
        goto next_byte;
      CASE cod_const_push18:           # (CONST&PUSH.S 18)
        pushSTACK(TheCclosure(closure)->clos_consts[18]);
        goto next_byte;
      CASE cod_const_push19:           # (CONST&PUSH.S 19)
        pushSTACK(TheCclosure(closure)->clos_consts[19]);
        goto next_byte;
      CASE cod_const_push20:           # (CONST&PUSH.S 20)
        pushSTACK(TheCclosure(closure)->clos_consts[20]);
        goto next_byte;
      CASE cod_const_push21:           # (CONST&PUSH.S 21)
        pushSTACK(TheCclosure(closure)->clos_consts[21]);
        goto next_byte;
      CASE cod_const_push22:           # (CONST&PUSH.S 22)
        pushSTACK(TheCclosure(closure)->clos_consts[22]);
        goto next_byte;
      CASE cod_const_push23:           # (CONST&PUSH.S 23)
        pushSTACK(TheCclosure(closure)->clos_consts[23]);
        goto next_byte;
      CASE cod_const_push24:           # (CONST&PUSH.S 24)
        pushSTACK(TheCclosure(closure)->clos_consts[24]);
        goto next_byte;
      CASE cod_const_push25:           # (CONST&PUSH.S 25)
        pushSTACK(TheCclosure(closure)->clos_consts[25]);
        goto next_byte;
      CASE cod_const_push26:           # (CONST&PUSH.S 26)
        pushSTACK(TheCclosure(closure)->clos_consts[26]);
        goto next_byte;
      CASE cod_const_push27:           # (CONST&PUSH.S 27)
        pushSTACK(TheCclosure(closure)->clos_consts[27]);
        goto next_byte;
      CASE cod_const_push28:           # (CONST&PUSH.S 28)
        pushSTACK(TheCclosure(closure)->clos_consts[28]);
        goto next_byte;
      CASE cod_const_push29:           # (CONST&PUSH.S 29)
        pushSTACK(TheCclosure(closure)->clos_consts[29]);
        goto next_byte;
      #if 0
      CASE cod_const_push30:           # (CONST&PUSH.S 30)
        pushSTACK(TheCclosure(closure)->clos_consts[30]);
        goto next_byte;
      CASE cod_const_push31:           # (CONST&PUSH.S 31)
        pushSTACK(TheCclosure(closure)->clos_consts[31]);
        goto next_byte;
      CASE cod_const_push32:           # (CONST&PUSH.S 32)
        pushSTACK(TheCclosure(closure)->clos_consts[32]);
        goto next_byte;
      #endif
      CASE cod_store0:                 # (STORE.S 0)
        STACK_(0) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store1:                 # (STORE.S 1)
        STACK_(1) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store2:                 # (STORE.S 2)
        STACK_(2) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store3:                 # (STORE.S 3)
        STACK_(3) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store4:                 # (STORE.S 4)
        STACK_(4) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store5:                 # (STORE.S 5)
        STACK_(5) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store6:                 # (STORE.S 6)
        STACK_(6) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store7:                 # (STORE.S 7)
        STACK_(7) = value1; mv_count=1;
        goto next_byte;
      #if 0
      CASE cod_store8:                 # (STORE.S 8)
        STACK_(8) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store9:                 # (STORE.S 9)
        STACK_(9) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store10:                # (STORE.S 10)
        STACK_(10) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store11:                # (STORE.S 11)
        STACK_(11) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store12:                # (STORE.S 12)
        STACK_(12) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store13:                # (STORE.S 13)
        STACK_(13) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store14:                # (STORE.S 14)
        STACK_(14) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store15:                # (STORE.S 15)
        STACK_(15) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store16:                # (STORE.S 16)
        STACK_(16) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store17:                # (STORE.S 17)
        STACK_(17) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store18:                # (STORE.S 18)
        STACK_(18) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store19:                # (STORE.S 19)
        STACK_(19) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store20:                # (STORE.S 20)
        STACK_(20) = value1; mv_count=1;
        goto next_byte;
      CASE cod_store21:                # (STORE.S 21)
        STACK_(21) = value1; mv_count=1;
        goto next_byte;
      #endif
      # ------------------- miscellaneous -----------------------
      #ifndef FAST_DISPATCH
      default:
      #endif
        /* undefined Code */
        #if defined(GNU) && defined(FAST_SP)
          /* Undo the effect of -fomit-frame-pointer for this function,
             hereby allowing utilization of %sp resp. %esp as private_SP: */
          alloca(1);
        #endif
        pushSTACK(fixnum(byteptr-&codeptr->data[0]-1)); /* bad byte number */
        pushSTACK(closure); # Closure
        fehler(serious_condition,
               GETTEXT("undefined bytecode in ~S at byte ~S"));
      #undef L_operand
      #undef S_operand
      #undef U_operand
      #undef B_operand
      #undef CASE
    }
   #if DEBUG_BYTECODE
   fehler_byteptr:
    pushSTACK(fixnum(byteptr_max));
    pushSTACK(fixnum(byteptr_min));
    pushSTACK(fixnum(byteptr - codeptr->data));
    pushSTACK(sfixnum(byteptr_bad_jump));
    pushSTACK(closure);
    fehler(error,GETTEXT("~S: jump by ~S takes ~S outside [~S;~S]"));
   #endif
   fehler_zuviele_werte:
    pushSTACK(closure);
    fehler(error,GETTEXT("~S: too many return values"));
   #if STACKCHECKC
   fehler_STACK_putt:
    pushSTACK(fixnum(byteptr - codeptr->data - byteptr_min)); /* PC */
    pushSTACK(closure);                       /* FUNC */
    fehler(serious_condition,GETTEXT("Corrupted STACK in ~S at byte ~S"));
   #endif
   finished:
    #undef FREE_JMPBUF_on_SP
    #undef JMPBUF_on_SP
    #ifndef FAST_SP
    FREE_DYNAMIC_ARRAY(private_SP_space);
    #endif
    return;
  }


/* UP: initialize hand-made compiled closures
 init_cclosures();
 can trigger GC */
global maygc void init_cclosures (void) {
  # Build #13Y(00 00 00 00 00 00 00 00 00 01 C5 19 01) ; (CONST 0) (SKIP&RET 1)
  {
    var object codevec = allocate_bit_vector(Atype_8Bit,CCV_START_NONKEY+3);
    TheCodevec(codevec)->ccv_spdepth_1 = 0;
    TheCodevec(codevec)->ccv_spdepth_jmpbufsize = 0;
    TheCodevec(codevec)->ccv_numreq = 0;
    TheCodevec(codevec)->ccv_numopt = 0;
    TheCodevec(codevec)->ccv_flags = 0;
    TheCodevec(codevec)->ccv_signature = cclos_argtype_0_0;
    TheSbvector(codevec)->data[CCV_START_NONKEY+0] = cod_const0;
    TheSbvector(codevec)->data[CCV_START_NONKEY+1] = cod_skip_ret;
    TheSbvector(codevec)->data[CCV_START_NONKEY+2] = 1;
    O(constant_initfunction_code) = codevec;
  }
  # Build #12Y(00 00 00 00 00 00 00 00 11 16 1B 7E) ; L0 (JMP L0)
  {
    var object codevec = allocate_bit_vector(Atype_8Bit,CCV_START_NONKEY+2);
    TheCodevec(codevec)->ccv_spdepth_1 = 0;
    TheCodevec(codevec)->ccv_spdepth_jmpbufsize = 0;
    TheCodevec(codevec)->ccv_numreq = 0;
    TheCodevec(codevec)->ccv_numopt = 0;
    TheCodevec(codevec)->ccv_flags = bit(4)|bit(0);
    TheCodevec(codevec)->ccv_signature = cclos_argtype_0_0_rest;
    TheSbvector(codevec)->data[CCV_START_NONKEY+0] = cod_jmp;
    TheSbvector(codevec)->data[CCV_START_NONKEY+1] = 128 - 2;
    O(endless_loop_code) = codevec;
  }
}


# where is check_SP() or check_STACK() to be inserted??
# is nest_env supposed to receive its target-environment as parameter??
# register-allocation in eval_subr and eval_cclosure etc.??
# eliminate subr_self??

