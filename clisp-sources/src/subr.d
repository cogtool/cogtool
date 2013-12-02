/*
 * list of all SUBRs
 * Bruno Haible 1990-2005
 * Sam Steingold 1998-2005
 */

/* A C-compiled LISP-function is defined by a declaration
   LISPFUN(name,seclass,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords)
 in this file.
 > name: the function name (a C-identifier)
 > seclass: the side-effect class (seclass_t, see lispbibl.d)
 > req_anz: the number of required-parameters (a number)
 > opt_anz: the number of optional parameters (a number)
 > rest_flag: either norest or rest
 > key_flag: either nokey or key or key_allow
 > key_anz: a number (0 if nokey)
 > keywords: either NIL (if nokey) or a expression of the form
             (kw(keyword1),...,kw(keywordn)) */

/* A C-compiled LISP-function with a fixed number of arguments
 is defined by the abbreviating declaration
   LISPFUNN(name,req_anz)
 > name: the function name (a C-identifier)
 > req_anz: the (fixed) number of arguments (a number) */
#define LISPFUNN(name,req_anz)                                  \
  LISPFUN(name,seclass_default,req_anz,0,norest,nokey,0,NIL)
#define LISPFUNNF(name,req_anz)                                 \
  LISPFUN(name,seclass_foldable,req_anz,0,norest,nokey,0,NIL)
#define LISPFUNNR(name,req_anz)                                 \
  LISPFUN(name,seclass_read,req_anz,0,norest,nokey,0,NIL)

/* Additionally, the same declaration plus C-Body must occur in a C-file. */

/* expander for the construction of the extern-declarations: */
#define LISPFUN_A(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords) \
  extern subr_##rest_flag##_function_t C_##name;

/* expander for the construction of the declaration of the C-function: */
#define LISPFUN_B(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords) \
  global Values C_##name subr_##rest_flag##_function_args
#define subr_norest_function_args  (void)
#define subr_rest_function_args  (uintC argcount, gcv_object_t* rest_args_pointer)

/* expander for the declaration of the SUBR-table: */
#define LISPFUN_C(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords) \
  subr_t D_##name;

/* expander for the initialization of the SUBR-table: */
#ifdef TYPECODES
#define LISPFUN_D(name_,sec,req_anz_,opt_anz_,rest_flag_,key_flag_,key_anz_,keywords_) \
  ptr->GCself = subr_tab_ptr_as_object(ptr /* = &subr_tab.D_##name_ */);\
  ptr->rectype = Rectype_Subr;                                          \
  ptr->recflags = 0;                                                    \
  ptr->reclength = subr_length;                                         \
  ptr->recxlength = subr_xlength;                                       \
  ptr->name = S_help_(S_##name_);                                       \
  ptr->keywords = NIL; /* preliminary */                                \
  ptr->function = (lisp_function_t)(&C_##name_);                        \
  ptr->argtype = (uintW)subr_argtype(req_anz_,opt_anz_,subr_##rest_flag_,subr_##key_flag_,NULL); \
  ptr->req_anz = req_anz_;                                              \
  ptr->opt_anz = opt_anz_;                                              \
  ptr->rest_flag = (uintB)subr_##rest_flag_;                            \
  ptr->key_flag = (uintB)subr_##key_flag_;                              \
  ptr->key_anz = key_anz_;                                              \
  ptr->seclass = sec;                                                   \
  ptr++;
#else
#define LISPFUN_D(name_,sec,req_anz_,opt_anz_,rest_flag_,key_flag_,key_anz_,keywords_) \
  ptr->GCself = subr_tab_ptr_as_object(ptr /* = &subr_tab.D_##name_ */);\
  ptr->tfl = xrecord_tfl(Rectype_Subr,0,subr_length,subr_xlength);      \
  ptr->name = S_help_(S_##name_);                                       \
  ptr->keywords = NIL; /* preliminary */                                \
  ptr->function = (lisp_function_t)(&C_##name_);                        \
  ptr->argtype = (uintW)subr_argtype(req_anz_,opt_anz_,subr_##rest_flag_,subr_##key_flag_,NULL); \
  ptr->req_anz = req_anz_;                                              \
  ptr->opt_anz = opt_anz_;                                              \
  ptr->rest_flag = (uintB)subr_##rest_flag_;                            \
  ptr->key_flag = (uintB)subr_##key_flag_;                              \
  ptr->key_anz = key_anz_;                                              \
  ptr->seclass = sec;                                                   \
  ptr++;
#endif
#define LISPFUN_E(name_,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords) \
  ptr->name = S_help_(S_##name_);                                       \
  ptr++;
#ifdef TYPECODES
#ifdef DEBUG_GCSAFETY
#define LISPFUN_F(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords) \
  { gcv_nullobj, /* preliminary */                                      \
    Rectype_Subr, 0, subr_length, subr_xlength,                         \
    gcv_nullobj, /* preliminary */                                      \
    gcv_nullobj, /* preliminary */                                      \
    (lisp_function_t)(&C_##name),                                       \
    0, /* preliminary */                                                \
    req_anz,                                                            \
    opt_anz,                                                            \
    (uintB)subr_##rest_flag,                                            \
    (uintB)subr_##key_flag,                                             \
    key_anz,                                                            \
    sec,                                                                \
  },
#else
#define LISPFUN_F(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords) \
  { { gcv_nullobj }, /* preliminary */                                  \
    Rectype_Subr, 0, subr_length, subr_xlength,                         \
    gcv_nullobj, /* preliminary */                                      \
    gcv_nullobj, /* preliminary */                                      \
    (lisp_function_t)(&C_##name),                                       \
    0, /* preliminary */                                                \
    req_anz,                                                            \
    opt_anz,                                                            \
    (uintB)subr_##rest_flag,                                            \
    (uintB)subr_##key_flag,                                             \
    key_anz,                                                            \
    sec,                                                                \
  },
#endif
#else
#define LISPFUN_F(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords) \
  { gcv_nullobj, /* preliminary */                                      \
    xrecord_tfl(Rectype_Subr,0,subr_length,subr_xlength),               \
    gcv_nullobj, /* preliminary */                                      \
    gcv_nullobj, /* preliminary */                                      \
    (lisp_function_t)(&C_##name),                                       \
    0, /* preliminary */                                                \
    req_anz,                                                            \
    opt_anz,                                                            \
    (uintB)subr_##rest_flag,                                            \
    (uintB)subr_##key_flag,                                             \
    key_anz,                                                            \
    sec,                                                                \
  },
#endif
#ifdef TYPECODES
#ifdef DEBUG_GCSAFETY
#define LISPFUN_G(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords) \
  { subr_tab_ptr_as_object(&subr_tab.D_##name),                         \
    Rectype_Subr, 0, subr_length, subr_xlength,                         \
    S_help_(S_##name),                                                  \
    NIL, /* preliminary */                                              \
    (lisp_function_t)(&C_##name),                                       \
    0, /* preliminary */                                                \
    req_anz,                                                            \
    opt_anz,                                                            \
    (uintB)subr_##rest_flag,                                            \
    (uintB)subr_##key_flag,                                             \
    key_anz,                                                            \
    sec,                                                                \
  },
#else
#define LISPFUN_G(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords) \
  { { subr_tab_ptr_as_object(&subr_tab.D_##name) },                     \
    Rectype_Subr, 0, subr_length, subr_xlength,                         \
    S_help_(S_##name),                                                  \
    NIL, /* preliminary */                                              \
    (lisp_function_t)(&C_##name),                                       \
    0, /* preliminary */                                                \
    req_anz,                                                            \
    opt_anz,                                                            \
    (uintB)subr_##rest_flag,                                            \
    (uintB)subr_##key_flag,                                             \
    key_anz,                                                            \
    sec,                                                                \
  },
#endif
#else
#define LISPFUN_G(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords) \
  { subr_tab_ptr_as_object(&subr_tab.D_##name),                         \
    xrecord_tfl(Rectype_Subr,0,subr_length,subr_xlength),               \
    S_help_(S_##name),                                                  \
    NIL, /* preliminary */                                              \
    (lisp_function_t)(&C_##name),                                       \
    0, /* preliminary */                                                \
    req_anz,                                                            \
    opt_anz,                                                            \
    (uintB)subr_##rest_flag,                                            \
    (uintB)subr_##key_flag,                                             \
    key_anz,                                                            \
    sec,                                                                \
  },
#endif

/* expander for the second initialization of the SUBR-table: */
#define LISPFUN_H(name,sec,req_anz,opt_anz,rest_flag,key_flag,key_anz,keywords_) \
  (subr_##key_flag==subr_key) ?                                         \
  subr_tab.D_##name.keywords =                                          \
    (vec = allocate_vector(key_anz),                                    \
     vecptr = &TheSvector(vec)->data[0],                                \
     (keywords_),                                                       \
     vec) : 0;

/* which expander is used must be specified in the main file.
   the default is #define LISPFUN LISPFUN_B */


/* ---------- SPVW ---------- */
/* no SUBRs */
/* ---------- EVAL ---------- */
LISPFUNNF(funtabref,1)
LISPFUNNR(subr_info,1)
LISPFUN(special_variable_p,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUNNR(add_implicit_block,2)
LISPFUNNR(function_block_name,1)
/* ---------- ARRAY ---------- */
LISPFUNNR(copy_simple_vector,1)
LISPFUN(vector,seclass_no_se,0,0,rest,nokey,0,NIL)
LISPFUN(aref,seclass_read,1,0,rest,nokey,0,NIL)
LISPFUN(store,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUNNR(svref,2)
LISPFUNN(svstore,3)
LISPFUNN(psvstore,3)
LISPFUNNR(row_major_aref,2)
LISPFUNN(row_major_store,3)
LISPFUNNF(array_element_type,1)
LISPFUNNF(array_rank,1)
LISPFUNNR(array_dimension,2)
LISPFUNNR(array_dimensions,1)
LISPFUNNR(array_total_size,1)
LISPFUN(array_in_bounds_p,seclass_read,1,0,rest,nokey,0,NIL)
LISPFUN(array_row_major_index,seclass_read,1,0,rest,nokey,0,NIL)
LISPFUNNF(adjustable_array_p,1)
LISPFUNN(array_displacement,1)
LISPFUN(bit,seclass_read,1,0,rest,nokey,0,NIL)
LISPFUN(sbit,seclass_read,1,0,rest,nokey,0,NIL)
LISPFUN(bit_and,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(bit_ior,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(bit_xor,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(bit_eqv,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(bit_nand,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(bit_nor,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(bit_andc1,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(bit_andc2,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(bit_orc1,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(bit_orc2,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(bit_not,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUNNR(array_has_fill_pointer_p,1)
LISPFUNNR(fill_pointer,1)
LISPFUNN(set_fill_pointer,2)
LISPFUNN(vector_push,2)
LISPFUNN(vector_pop,1)
LISPFUN(vector_push_extend,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(make_array,seclass_read,1,0,norest,key,7,
        (kw(adjustable),kw(element_type),kw(initial_element),
         kw(initial_contents),kw(fill_pointer),
         kw(displaced_to),kw(displaced_index_offset)) )
LISPFUN(adjust_array,seclass_default,2,0,norest,key,6,
        (kw(element_type),kw(initial_element),
         kw(initial_contents),kw(fill_pointer),
         kw(displaced_to),kw(displaced_index_offset)) )
LISPFUNN(vector_init,1)
LISPFUNN(vector_upd,2)
LISPFUNN(vector_endtest,2)
LISPFUNN(vector_fe_init,1)
LISPFUNN(vector_fe_upd,2)
LISPFUNN(vector_fe_endtest,2)
LISPFUNN(vector_length,1)
LISPFUNN(vector_init_start,2)
LISPFUNN(vector_fe_init_end,2)
LISPFUNN(make_bit_vector,1)
/* ---------- CHARSTRG ---------- */
LISPFUNNR(string_info,1)
LISPFUNNF(standard_char_p,1)
LISPFUNNF(graphic_char_p,1)
LISPFUNN(char_width,1)
LISPFUNNF(string_char_p,1)
#if (base_char_code_limit < char_code_limit)
LISPFUNN(base_char_p,1)
#endif
LISPFUNNF(alpha_char_p,1)
LISPFUNNF(upper_case_p,1)
LISPFUNNF(lower_case_p,1)
LISPFUNNF(both_case_p,1)
LISPFUN(digit_char_p,seclass_foldable,1,1,norest,nokey,0,NIL)
LISPFUNNF(alphanumericp,1)
LISPFUN(char_gleich,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(char_ungleich,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(char_kleiner,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(char_groesser,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(char_klgleich,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(char_grgleich,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(char_equal,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(char_not_equal,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(char_lessp,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(char_greaterp,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(char_not_greaterp,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(char_not_lessp,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUNNF(char_code,1)
LISPFUNNF(code_char,1)
LISPFUNNR(character,1)
LISPFUNNF(char_upcase,1)
LISPFUNNF(char_downcase,1)
LISPFUN(digit_char,seclass_foldable,1,1,norest,nokey,0,NIL)
LISPFUNNF(char_int,1)
LISPFUNNF(int_char,1)
LISPFUNNF(char_name,1)
LISPFUNNF(char_invertcase,1)
LISPFUNNR(char,2)
LISPFUNNR(schar,2)
LISPFUNN(store_char,3)
LISPFUNN(store_schar,3)
LISPFUN(string_gleich,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(cs_string_gleich,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_ungleich,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(cs_string_ungleich,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_kleiner,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(cs_string_kleiner,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_groesser,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(cs_string_groesser,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_klgleich,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(cs_string_klgleich,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_grgleich,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(cs_string_grgleich,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_equal,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_not_equal,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_lessp,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_greaterp,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_not_greaterp,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(string_not_lessp,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(search_string_gleich,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(search_string_equal,seclass_read,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(make_string,seclass_no_se,1,0,norest,key,2,
        (kw(initial_element),kw(element_type)) )
LISPFUNNR(string_both_trim,4)
LISPFUN(string_width,seclass_default,1,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(nstring_upcase,seclass_default,1,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(string_upcase,seclass_read,1,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(nstring_downcase,seclass_default,1,0,norest,key,2,
        (kw(start),kw(end)) )
LISPFUN(string_downcase,seclass_read,1,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(nstring_capitalize,seclass_default,1,0,norest,key,2,
        (kw(start),kw(end)) )
LISPFUN(string_capitalize,seclass_read,1,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(nstring_invertcase,seclass_default,1,0,norest,key,2,
        (kw(start),kw(end)) )
LISPFUN(string_invertcase,seclass_read,1,0,norest,key,2, (kw(start),kw(end)) )
LISPFUNNR(string,1)
LISPFUNNR(cs_string,1)
LISPFUNNR(name_char,1)
LISPFUN(substring,seclass_read,2,1,norest,nokey,0,NIL)
LISPFUN(string_concat,seclass_read,0,0,rest,nokey,0,NIL)
/* ---------- CONTROL ---------- */
LISPFUN(exit,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUNNR(symbol_value,1)
LISPFUNNR(symbol_function,1)
LISPFUNNR(fdefinition,1)
LISPFUNNR(boundp,1)
LISPFUNNR(fboundp,1)
LISPFUNNF(special_operator_p,1)
LISPFUNN(set,2)
LISPFUNN(makunbound,1)
LISPFUNN(fmakunbound,1)
LISPFUN(apply,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(funcall,seclass_default,1,0,rest,nokey,0,NIL)
LISPFUN(mapcar,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(maplist,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(mapc,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(mapl,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(mapcan,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(mapcon,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(mapcap,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(maplap,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(values,seclass_no_se,0,0,rest,nokey,0,NIL)
LISPFUNNR(values_list,1)
LISPFUNN(driver,1)
LISPFUNN(unwind_to_driver,1)
LISPFUN(macro_function,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUN(macroexpand,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(macroexpand_1,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUNN(proclaim,1)
LISPFUNN(eval,1)
LISPFUN(evalhook,seclass_default,3,1,norest,nokey,0,NIL)
LISPFUN(applyhook,seclass_default,4,1,norest,nokey,0,NIL)
LISPFUN(constantp,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUNNR(global_symbol_macro_p,1)
LISPFUNNR(function_side_effect,1)
LISPFUNNR(function_name_p,1)
LISPFUNN(check_function_name,2)
LISPFUNN(check_symbol,2)
LISPFUN(parse_body,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUNN(keyword_test,2)
LISPFUN(xor,seclass_foldable,0,0,rest,nokey,0,NIL)
/* ---------- DEBUG ---------- */
LISPFUN(read_form,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(read_eval_print,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUNN(initial_break_driver,1)
LISPFUNN(load,1)
LISPFUNN(frame_up_1,2)
LISPFUNN(frame_up,2)
LISPFUNN(frame_down_1,2)
LISPFUNN(frame_down,2)
LISPFUNN(the_frame,0)
LISPFUNN(same_env_as,2)
LISPFUNN(eval_at,2)
LISPFUNN(eval_frame_p,1)
LISPFUNN(driver_frame_p,1)
LISPFUNN(trap_eval_frame,2)
LISPFUNN(redo_eval_frame,1)
LISPFUNN(return_from_eval_frame,2)
LISPFUNN(describe_frame,2)
LISPFUN(show_stack,seclass_default,0,3,norest,nokey,0,NIL)
LISPFUNN(crash,0)
LISPFUNN(proom,0)
LISPFUNN(gc,0)
/* ---------- ENCODING ---------- */
LISPFUN(make_encoding,seclass_read,0,0,norest,key,5,
        (kw(charset),kw(line_terminator),kw(input_error_action),
         kw(output_error_action),kw(if_does_not_exist)) )
LISPFUNNF(encodingp,1)
LISPFUNNR(charset_typep,2)
LISPFUNNF(encoding_line_terminator,1)
#ifdef UNICODE
LISPFUNNF(encoding_charset,1)
LISPFUN(charset_range,seclass_read,3,1,norest,nokey,0,NIL)
#endif
LISPFUNNR(default_file_encoding,0)
LISPFUNN(set_default_file_encoding,1)
#ifdef UNICODE
LISPFUNNR(pathname_encoding,0)
LISPFUNN(set_pathname_encoding,1)
LISPFUNNR(terminal_encoding,0)
LISPFUNN(set_terminal_encoding,1)
#if defined(HAVE_FFI) || defined(HAVE_AFFI)
LISPFUNNR(foreign_encoding,0)
LISPFUNN(set_foreign_encoding,1)
#endif
LISPFUNNR(misc_encoding,0)
LISPFUNN(set_misc_encoding,1)
#endif
LISPFUN(convert_string_from_bytes,seclass_read,2,0,norest,key,2,
        (kw(start),kw(end)) )
LISPFUN(convert_string_to_bytes,seclass_read,2,0,norest,key,2,
        (kw(start),kw(end)) )
/* ---------- ERROR ---------- */
LISPFUN(error,seclass_default,1,0,rest,nokey,0,NIL)
LISPFUNN(defclcs,1)
LISPFUN(cerror_of_type,seclass_default,3,0,rest,nokey,0,NIL)
LISPFUN(error_of_type,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUNN(invoke_debugger,1)
LISPFUN(clcs_signal,seclass_default,1,0,rest,nokey,0,NIL)
/* ---------- HASHTABL ---------- */
LISPFUN(make_hash_table,seclass_read,0,0,norest,key,9,
        (kw(initial_contents),kw(key_type),kw(value_type),
         kw(warn_if_needs_rehash_after_gc),kw(weak),
         kw(test),kw(size),kw(rehash_size),kw(rehash_threshold)) )
LISPFUN(gethash,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUNN(puthash,3)
LISPFUNN(remhash,2)
LISPFUNN(maphash,2)
LISPFUNN(clrhash,1)
LISPFUNNR(hash_table_count,1)
LISPFUNNR(hash_table_rehash_size,1)
LISPFUNNR(hash_table_rehash_threshold,1)
LISPFUNNR(hash_table_size,1)
LISPFUNNF(hash_table_test,1)
LISPFUNNF(fasthash_stable_p,1)
LISPFUNNR(stablehash_stable_p,1)
LISPFUNNR(hash_table_iterator,1)
LISPFUNN(hash_table_iterate,1)
LISPFUNNR(hash_table_weak_p,1)
LISPFUNN(set_hash_table_weak_p,2)
LISPFUNNR(hash_table_warn_if_needs_rehash_after_gc,1)
LISPFUNN(set_hash_table_warn_if_needs_rehash_after_gc,2)
LISPFUNN(class_gethash,2)
LISPFUN(class_tuple_gethash,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUNN(sxhash,1)
/* ---------- IO ---------- */
LISPFUNN(defio,2)
LISPFUN(copy_readtable,seclass_read,0,2,norest,nokey,0,NIL)
LISPFUN(set_syntax_from_char,seclass_default,2,2,norest,nokey,0,NIL)
LISPFUN(set_macro_character,seclass_default,2,2,norest,nokey,0,NIL)
LISPFUN(get_macro_character,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUN(make_dispatch_macro_character,seclass_default,1,2,norest,nokey,0,NIL)
LISPFUN(set_dispatch_macro_character,seclass_default,3,1,norest,nokey,0,NIL)
LISPFUN(get_dispatch_macro_character,seclass_read,2,1,norest,nokey,0,NIL)
LISPFUNN(readtable_case,1)
LISPFUNN(set_readtable_case,2)
LISPFUNN(lpar_reader,2)
LISPFUNN(rpar_reader,2)
LISPFUNN(string_reader,2)
LISPFUNN(quote_reader,2)
LISPFUNN(line_comment_reader,2)
LISPFUNN(function_reader,3)
LISPFUNN(comment_reader,3)
LISPFUNN(char_reader,3)
LISPFUNN(binary_reader,3)
LISPFUNN(octal_reader,3)
LISPFUNN(hexadecimal_reader,3)
LISPFUNN(radix_reader,3)
LISPFUNN(complex_reader,3)
LISPFUNN(uninterned_reader,3)
LISPFUNN(bit_vector_reader,3)
LISPFUNN(vector_reader,3)
LISPFUNN(array_reader,3)
LISPFUNN(read_eval_reader,3)
LISPFUNN(load_eval_reader,3)
LISPFUNN(label_definition_reader,3)
LISPFUNN(label_reference_reader,3)
LISPFUNN(not_readable_reader,3)
LISPFUNN(syntax_error_reader,3)
LISPFUNNR(featurep,1)
LISPFUNN(feature_reader,3)
LISPFUNN(not_feature_reader,3)
LISPFUNN(structure_reader,3)
LISPFUNN(closure_reader,3)
LISPFUNN(clisp_pathname_reader,3)
LISPFUNN(ansi_pathname_reader,3)
#if defined(UNIX) || defined(WIN32_NATIVE)
LISPFUNN(unix_executable_reader,3)
#endif
LISPFUN(read,seclass_default,0,4,norest,nokey,0,NIL)
LISPFUN(read_preserving_whitespace,seclass_default,0,4,norest,nokey,0,NIL)
LISPFUN(read_delimited_list,seclass_default,1,2,norest,nokey,0,NIL)
LISPFUN(read_line,seclass_default,0,4,norest,nokey,0,NIL)
LISPFUN(read_char,seclass_default,0,4,norest,nokey,0,NIL)
LISPFUN(unread_char,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(peek_char,seclass_default,0,5,norest,nokey,0,NIL)
LISPFUN(listen,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUNN(read_char_will_hang_p,1)
LISPFUN(read_char_no_hang,seclass_default,0,4,norest,nokey,0,NIL)
LISPFUN(clear_input,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUN(read_from_string,seclass_default,1,2,norest,key,3,
        (kw(preserve_whitespace),kw(start),kw(end)) )
LISPFUN(parse_integer,seclass_read,1,0,norest,key,4,
        (kw(start),kw(end),kw(radix),kw(junk_allowed)) )
LISPFUNN(print_structure,2)
LISPFUN(write,seclass_default,1,0,norest,key,17,
        (kw(case),kw(level),kw(length),kw(gensym),kw(escape),kw(radix),
         kw(base),kw(array),kw(circle),kw(pretty),kw(closure),kw(readably),
         kw(lines),kw(miser_width),kw(pprint_dispatch),
         kw(right_margin),kw(stream)))
LISPFUN(prin1,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(print,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(pprint,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(pprint_indent,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(pprint_newline,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(format_tabulate,seclass_default,3,2,norest,nokey,0,NIL)
LISPFUNN(ppprint_logical_block,3)
LISPFUNN(pcirclep,2)
LISPFUN(princ,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(write_to_string,seclass_default,1,0,norest,key,16,
        (kw(case),kw(level),kw(length),kw(gensym),kw(escape),kw(radix),
         kw(base),kw(array),kw(circle),kw(pretty),kw(closure),kw(readably),
         kw(lines),kw(miser_width),kw(pprint_dispatch),kw(right_margin)))
LISPFUNN(prin1_to_string,1)
LISPFUNN(princ_to_string,1)
LISPFUN(write_char,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(write_string,seclass_default,1,1,norest,key,2, (kw(start),kw(end)) )
LISPFUN(write_line,seclass_default,1,1,norest,key,2, (kw(start),kw(end)) )
LISPFUN(terpri,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUN(fresh_line,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUN(elastic_newline,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUN(finish_output,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUN(force_output,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUN(clear_output,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUN(write_unreadable,seclass_default,3,0,norest,key,2,
        (kw(type),kw(identity)) )
LISPFUN(line_position,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUNN(whitespacep,1)
LISPFUN(write_spaces,seclass_default,1,1,norest,nokey,0,NIL)
/* ---------- LIST ---------- */
LISPFUNNR(car,1)
LISPFUNNR(cdr,1)
LISPFUNNR(caar,1)
LISPFUNNR(cadr,1)
LISPFUNNR(cdar,1)
LISPFUNNR(cddr,1)
LISPFUNNR(caaar,1)
LISPFUNNR(caadr,1)
LISPFUNNR(cadar,1)
LISPFUNNR(caddr,1)
LISPFUNNR(cdaar,1)
LISPFUNNR(cdadr,1)
LISPFUNNR(cddar,1)
LISPFUNNR(cdddr,1)
LISPFUNNR(caaaar,1)
LISPFUNNR(caaadr,1)
LISPFUNNR(caadar,1)
LISPFUNNR(caaddr,1)
LISPFUNNR(cadaar,1)
LISPFUNNR(cadadr,1)
LISPFUNNR(caddar,1)
LISPFUNNR(cadddr,1)
LISPFUNNR(cdaaar,1)
LISPFUNNR(cdaadr,1)
LISPFUNNR(cdadar,1)
LISPFUNNR(cdaddr,1)
LISPFUNNR(cddaar,1)
LISPFUNNR(cddadr,1)
LISPFUNNR(cdddar,1)
LISPFUNNR(cddddr,1)
LISPFUN(cons,seclass_no_se,2,0,norest,nokey,0,NIL)
LISPFUN(tree_equal,seclass_default,2,0,norest,key,2, (kw(test),kw(test_not)) )
LISPFUNNF(endp,1)
LISPFUNNR(list_length,1)
LISPFUNNR(list_length_dotted,1)
LISPFUNNR(list_length_proper,1)
LISPFUNNR(nth,2)
LISPFUNNR(first,1)
LISPFUNNR(second,1)
LISPFUNNR(third,1)
LISPFUNNR(fourth,1)
LISPFUNNR(fifth,1)
LISPFUNNR(sixth,1)
LISPFUNNR(seventh,1)
LISPFUNNR(eighth,1)
LISPFUNNR(ninth,1)
LISPFUNNR(tenth,1)
LISPFUNNR(rest,1)
LISPFUNNR(nthcdr,2)
LISPFUNNR(conses_p,2)
LISPFUN(last,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUN(list,seclass_no_se,0,0,rest,nokey,0,NIL)
LISPFUN(liststern,seclass_no_se,1,0,rest,nokey,0,NIL)
LISPFUN(make_list,seclass_no_se,1,0,norest,key,1, (kw(initial_element)) )
LISPFUN(append,seclass_read,0,0,rest,nokey,0,NIL)
LISPFUNNR(copy_list,1)
LISPFUNNR(copy_alist,1)
LISPFUNNR(copy_tree,1)
LISPFUNNR(revappend,2)
LISPFUN(nconc,seclass_default,0,0,rest,nokey,0,NIL)
LISPFUNN(nreconc,2)
LISPFUNN(list_nreverse,1)
LISPFUN(butlast,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUN(nbutlast,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUNNR(ldiff,2)
LISPFUNN(rplaca,2)
LISPFUNN(prplaca,2)
LISPFUNN(rplacd,2)
LISPFUNN(prplacd,2)
LISPFUN(subst,seclass_default,3,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
LISPFUN(subst_if,seclass_default,3,0,norest,key,1, (kw(key)) )
LISPFUN(subst_if_not,seclass_default,3,0,norest,key,1, (kw(key)) )
LISPFUN(nsubst,seclass_default,3,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
LISPFUN(nsubst_if,seclass_default,3,0,norest,key,1, (kw(key)) )
LISPFUN(nsubst_if_not,seclass_default,3,0,norest,key,1, (kw(key)) )
LISPFUN(sublis,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
LISPFUN(nsublis,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
LISPFUNNR(memq,2)
LISPFUN(member,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
LISPFUN(member_if,seclass_default,2,0,norest,key,1, (kw(key)) )
LISPFUN(member_if_not,seclass_default,2,0,norest,key,1, (kw(key)) )
LISPFUNNR(tailp,2)
LISPFUN(adjoin,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
LISPFUN(acons,seclass_no_se,3,0,norest,nokey,0,NIL)
LISPFUN(pairlis,seclass_read,2,1,norest,nokey,0,NIL)
LISPFUN(assoc,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
LISPFUN(assoc_if,seclass_default,2,0,norest,key,1, (kw(key)) )
LISPFUN(assoc_if_not,seclass_default,2,0,norest,key,1, (kw(key)) )
LISPFUN(rassoc,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
LISPFUN(rassoc_if,seclass_default,2,0,norest,key,1, (kw(key)) )
LISPFUN(rassoc_if_not,seclass_default,2,0,norest,key,1, (kw(key)) )
LISPFUNN(list_upd,2)
LISPFUNN(list_endtest,2)
LISPFUNN(list_fe_init,1)
LISPFUNN(list_access,2)
LISPFUNN(list_access_set,3)
LISPFUNN(list_elt,2)
LISPFUNN(list_set_elt,3)
LISPFUNN(list_init_start,2)
LISPFUNN(list_fe_init_end,2)
/* ---------- MISC ---------- */
LISPFUN(lisp_implementation_type,seclass_no_se,0,0,norest,nokey,0,NIL)
LISPFUN(lisp_implementation_version,seclass_no_se,0,0,norest,nokey,0,NIL)
LISPFUN(version,seclass_default,0,1,norest,nokey,0,NIL)
#ifdef MACHINE_KNOWN
LISPFUNN(machinetype,0)
LISPFUNN(machine_version,0)
#endif
#ifdef HAVE_ENVIRONMENT
LISPFUN(get_env,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUNN(set_env,2)
#endif
#ifdef WIN32_NATIVE
LISPFUNN(registry,2)
#endif
LISPFUN(software_type,seclass_no_se,0,0,norest,nokey,0,NIL)
LISPFUN(software_version,seclass_no_se,0,0,norest,nokey,0,NIL)
LISPFUNNF(identity,1)
LISPFUNN(address_of,1)
LISPFUNN(code_address_of,1)
LISPFUNN(process_id,0)
LISPFUNNF(ansi,0)
LISPFUNN(set_ansi,1)
LISPFUN(module_info,seclass_no_se,0,2,norest,nokey,0,NIL)
LISPFUN(argv,seclass_no_se,0,0,norest,nokey,0,NIL)
/* ---------- I18N ---------- */
LISPFUNNR(current_language,0)
LISPFUNN(set_current_language,1)
LISPFUNNR(text,1)
/* ---------- SOCKET ---------- */
#ifdef MACHINE_KNOWN
LISPFUNN(machine_instance,0)
#endif
/* ---------- TIME ---------- */
LISPFUNNR(get_internal_real_time,0)
LISPFUNNR(get_internal_run_time,0)
LISPFUNNR(get_universal_time,0)
#if defined(UNIX) || defined(WIN32)
LISPFUNNR(default_time_zone,2)
#endif
LISPFUNN(sleep,2)
LISPFUNNR(time,0)
LISPFUNNF(delta4,5)
/* ---------- PACKAGE ---------- */
LISPFUNNR(make_symbol,1)
LISPFUNNR(find_package,1)
LISPFUNN(pfind_package,1)
LISPFUNNR(package_name,1)
LISPFUNNR(package_nicknames,1)
LISPFUN(rename_package,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUNNR(package_use_list,1)
LISPFUNNR(package_used_by_list,1)
LISPFUNNR(package_shadowing_symbols,1)
LISPFUNNR(package_lock,1)
LISPFUNNR(package_shortest_name,1)
LISPFUNNR(package_case_sensitive_p,1)
LISPFUNNR(package_case_inverted_p,1)
LISPFUNNR(package_documentation,1)
LISPFUNN(set_package_documentation,2)
LISPFUNN(set_package_case_inverted_p,2)
LISPFUNN(set_package_case_sensitive_p,2)
LISPFUNN(set_package_lock,2)
LISPFUNN(symbol_value_lock,1)
LISPFUNN(check_package_lock,3)
LISPFUNNR(list_all_packages,0)
LISPFUN(intern,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(cs_intern,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(find_symbol,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUN(cs_find_symbol,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUN(unintern,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(export,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(unexport,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUNN(re_export,2)
LISPFUN(import,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(shadowing_import,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(shadow,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(cs_shadow,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(use_package,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(unuse_package,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(make_package,seclass_default,1,0,norest,key,4,
        (kw(nicknames),kw(use),kw(case_sensitive),kw(case_inverted)) )
LISPFUN(cs_make_package,seclass_default,1,0,norest,key,4,
        (kw(nicknames),kw(use),kw(case_sensitive),kw(case_inverted)) )
LISPFUN(pin_package,seclass_default,1,0,norest,key,4,
        (kw(nicknames),kw(use),kw(case_sensitive),kw(case_inverted)) )
LISPFUNN(delete_package,1)
LISPFUNNR(find_all_symbols,1)
LISPFUNNR(cs_find_all_symbols,1)
LISPFUNN(map_symbols,2)
LISPFUNN(map_external_symbols,2)
LISPFUNN(map_all_symbols,1)
LISPFUNN(package_iterator,2)
LISPFUNN(package_iterate,1)
/* ---------- PATHNAME ---------- */
LISPFUN(parse_namestring,seclass_read,1,2,norest,key,3,
        (kw(start),kw(end),kw(junk_allowed)) )
LISPFUNNR(pathname,1)
LISPFUN(pathnamehost,seclass_read,1,0,norest,key,1, (kw(case)))
LISPFUN(pathnamedevice,seclass_read,1,0,norest,key,1, (kw(case)))
LISPFUN(pathnamedirectory,seclass_read,1,0,norest,key,1, (kw(case)))
LISPFUN(pathnamename,seclass_read,1,0,norest,key,1, (kw(case)))
LISPFUN(pathnametype,seclass_read,1,0,norest,key,1, (kw(case)))
LISPFUNNR(pathnameversion,1)
#ifdef LOGICAL_PATHNAMES
LISPFUNNR(logical_pathname,1)
LISPFUN(translate_logical_pathname,seclass_default,1,0,norest,key,1,
        (kw(absolute)))
#endif
LISPFUNNR(file_namestring,1)
LISPFUNNR(directory_namestring,1)
LISPFUNNR(host_namestring,1)
LISPFUN(merge_pathnames,seclass_read,1,2,norest,key,1, (kw(wild)))
LISPFUN(enough_namestring,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUN(make_pathname,seclass_read,0,0,norest,key,8,
        (kw(defaults),kw(case),kw(host),kw(device),kw(directory),
         kw(name),kw(type),kw(version)) )
#ifdef LOGICAL_PATHNAMES
LISPFUN(make_logical_pathname,seclass_read,0,0,norest,key,8,
        (kw(defaults),kw(case),kw(host),kw(device),kw(directory),
         kw(name),kw(type),kw(version)) )
#endif
#ifdef USER_HOMEDIR
LISPFUN(user_homedir_pathname,seclass_default,0,1,norest,nokey,0,NIL)
#endif
LISPFUN(wild_pathname_p,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUNNR(pathname_match_p,2)
LISPFUN(translate_pathname,seclass_default,3,0,norest,key,3,
        (kw(all),kw(merge),kw(absolute)))
LISPFUNN(absolute_pathname,1)
LISPFUNNR(namestring,1)
LISPFUNNR(truename,1)
LISPFUNNR(probe_file,1)
LISPFUNNR(probe_directory,1)
LISPFUNN(delete_file,1)
LISPFUNN(rename_file,2)
LISPFUN(open,seclass_default,1,0,norest,key,6,
        (kw(direction),kw(element_type),kw(if_exists),kw(if_does_not_exist),
         kw(external_format),kw(buffered)) )
LISPFUN(directory,seclass_read,1,0,norest,key,3,
        (kw(if_does_not_exist),kw(circle),kw(full)))
LISPFUN(cd,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUNN(make_dir,1)
LISPFUNN(delete_dir,1)
LISPFUN(ensure_directories_exist,seclass_default,1,0,norest,key,1,
        (kw(verbose)))
LISPFUNNR(file_write_date,1)
LISPFUNNR(file_author,1)
#ifdef UNIX
LISPFUN(execute,seclass_default,1,0,rest,nokey,0,NIL)
#endif
#ifdef HAVE_SHELL
#ifdef WIN32_NATIVE
LISPFUNN(shell_name,0)
#endif
LISPFUN(shell,seclass_default,0,1,norest,nokey,0,NIL)
#endif
#if defined(UNIX) || defined(WIN32_NATIVE)
LISPFUN(launch,seclass_default,1,0,norest,key,9,(kw(element_type),kw(external_format),kw(buffered),kw(arguments),kw(wait),kw(input),kw(output),kw(error),kw(priority)))
#endif
#ifdef WIN32_NATIVE
LISPFUN(shell_execute,seclass_default,0,4,norest,nokey,0,NIL)
#endif
LISPFUNN(savemem,2)
#ifdef DYNAMIC_MODULES
LISPFUNN(dynload_modules,2)
#endif
LISPFUNN(program_name,0)
LISPFUNN(lib_directory,0)
LISPFUNN(set_lib_directory,1)
/* ---------- PREDTYPE ---------- */
LISPFUNNF(eq,2)
LISPFUNNF(eql,2)
LISPFUNNR(equal,2)
LISPFUNNR(equalp,2)
LISPFUNNF(consp,1)
LISPFUNNF(atom,1)
LISPFUNNF(symbolp,1)
LISPFUNNF(stringp,1)
LISPFUNNF(numberp,1)
LISPFUNNR(compiled_function_p,1)
LISPFUNNR(pcompiled_function_p,1)
LISPFUNNF(null,1)
LISPFUNNF(not,1)
LISPFUNNF(closurep,1)
LISPFUNNF(listp,1)
LISPFUNNR(proper_list_p,1)
LISPFUNNF(integerp,1)
LISPFUNNF(fixnump,1)
LISPFUNNF(rationalp,1)
LISPFUNNF(floatp,1)
LISPFUNNF(short_float_p,1)
LISPFUNNF(single_float_p,1)
LISPFUNNF(double_float_p,1)
LISPFUNNF(long_float_p,1)
LISPFUNNF(realp,1)
LISPFUNNF(complexp,1)
LISPFUNNR(streamp,1)
LISPFUNNF(built_in_stream_p,1)
LISPFUNNF(random_state_p,1)
LISPFUNNF(readtablep,1)
LISPFUNNF(hash_table_p,1)
LISPFUNNF(pathnamep,1)
LISPFUNNF(logical_pathname_p,1)
LISPFUNNF(characterp,1)
LISPFUNNF(functionp,1)
LISPFUNNF(packagep,1)
LISPFUNNF(arrayp,1)
LISPFUNNF(simple_array_p,1)
LISPFUNNF(bit_vector_p,1)
LISPFUNNF(vectorp,1)
LISPFUNNF(simple_vector_p,1)
LISPFUNNF(simple_string_p,1)
LISPFUNNF(simple_bit_vector_p,1)
LISPFUNNR(type_of,1)
LISPFUNN(defclos,6)
LISPFUNNR(potential_class_p,1)
LISPFUNNR(defined_class_p,1)
LISPFUNNR(class_of,1)
LISPFUN(find_class,seclass_default,1,2,norest,nokey,0,NIL)
LISPFUNN(typep_class,2)
LISPFUN(expand_deftype,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUNNR(coerce,2)
LISPFUNN(note_new_structure_class,0)
LISPFUNN(note_new_standard_class,0)
LISPFUNN(heap_statistics,0)
LISPFUNN(gc_statistics,0)
LISPFUNN(list_statistics,1)
LISPFUNN(heap_statistics_statistics,1)
LISPFUNN(gc_statistics_statistics,2)
/* ---------- RECORD ---------- */
LISPFUNNR(record_ref,2)
LISPFUNN(record_store,3)
LISPFUNNR(record_length,1)
LISPFUNNR(pstructure_ref,3)
LISPFUNNR(structure_ref,3)
LISPFUNN(structure_store,4)
LISPFUNNR(make_structure,2)
LISPFUNNR(copy_structure,1)
LISPFUNNR(structure_type_p,2)
LISPFUNNR(closure_name,1)
LISPFUNN(set_closure_name,2)
LISPFUNNR(closure_codevec,1)
LISPFUNNR(closure_consts,1)
LISPFUNNR(make_code_vector,1)
LISPFUNNR(make_closure,4)
LISPFUNN(make_constant_initfunction,1)
LISPFUNN(constant_initfunction_p,1)
LISPFUNN(closure_set_seclass,2)
LISPFUNN(set_funcallable_instance_function,2)
LISPFUNN(copy_generic_function,2)
LISPFUNN(generic_function_effective_method_function,1)
LISPFUN(make_load_time_eval,seclass_no_se,1,0,norest,nokey,0,NIL)
LISPFUN(make_symbol_macro,seclass_no_se,1,0,norest,nokey,0,NIL)
LISPFUNNF(symbol_macro_p,1)
LISPFUNN(symbol_macro_expand,1)
LISPFUN(make_global_symbol_macro,seclass_no_se,1,0,norest,nokey,0,NIL)
LISPFUNN(global_symbol_macro_definition,1)
LISPFUNN(make_macro,1)
LISPFUNN(macrop,1)
LISPFUNN(macro_expander,1)
LISPFUNN(make_function_macro,2)
LISPFUNN(function_macro_p,1)
LISPFUNN(function_macro_function,1)
LISPFUNN(function_macro_expander,1)
LISPFUN(finalize,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUNNF(structure_object_p,1)
LISPFUNNF(std_instance_p,1)
LISPFUNNF(funcallable_instance_p,1)
LISPFUNN(allocate_metaobject_instance,2)
LISPFUNN(allocate_std_instance,2)
LISPFUNN(allocate_funcallable_instance,2)
LISPFUN(pallocate_instance,seclass_read,1,0,rest,nokey,0,NIL)
LISPFUNN(pslot_value_using_class,3)
LISPFUNN(pset_slot_value_using_class,4)
LISPFUNN(pslot_boundp_using_class,3)
LISPFUNN(pslot_makunbound_using_class,3)
LISPFUNN(slot_value,2)
LISPFUNN(set_slot_value,3)
LISPFUNN(slot_boundp,2)
LISPFUNN(slot_makunbound,2)
LISPFUNNR(slot_exists_p,2)
LISPFUNNR(standard_instance_access,2)
LISPFUNN(set_standard_instance_access,3)
LISPFUNNF(punbound,0)
LISPFUN(pshared_initialize,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(preinitialize_instance,seclass_default,1,0,rest,nokey,0,NIL)
LISPFUN(pinitialize_instance,seclass_default,1,0,rest,nokey,0,NIL)
LISPFUN(pmake_instance,seclass_default,1,0,rest,nokey,0,NIL)
LISPFUNN(pchange_class,2)
/* ---------- WEAK ---------- */
LISPFUN(make_weak_pointer,seclass_no_se,1,0,norest,nokey,0,NIL)
LISPFUNNF(weak_pointer_p,1)
LISPFUNNR(weak_pointer_value,1)
LISPFUNN(set_weak_pointer_value,2)
LISPFUNN(make_weak_list,1)
LISPFUNNF(weak_list_p,1)
LISPFUNNR(weak_list_list,1)
LISPFUNN(set_weak_list_list,2)
LISPFUNN(make_weak_and_relation,1)
LISPFUNNF(weak_and_relation_p,1)
LISPFUNNR(weak_and_relation_list,1)
LISPFUNN(make_weak_or_relation,1)
LISPFUNNF(weak_or_relation_p,1)
LISPFUNNR(weak_or_relation_list,1)
LISPFUNN(make_weak_mapping,2)
LISPFUNNF(weak_mapping_p,1)
LISPFUNNR(weak_mapping_pair,1)
LISPFUNNR(weak_mapping_value,1)
LISPFUNN(set_weak_mapping_value,2)
LISPFUNN(make_weak_and_mapping,2)
LISPFUNNF(weak_and_mapping_p,1)
LISPFUNNR(weak_and_mapping_pair,1)
LISPFUNNR(weak_and_mapping_value,1)
LISPFUNN(set_weak_and_mapping_value,2)
LISPFUNN(make_weak_or_mapping,2)
LISPFUNNF(weak_or_mapping_p,1)
LISPFUNNR(weak_or_mapping_pair,1)
LISPFUNNR(weak_or_mapping_value,1)
LISPFUNN(set_weak_or_mapping_value,2)
LISPFUN(make_weak_alist,seclass_read,0,0,norest,key,2,
        (kw(type),kw(initial_contents)) )
LISPFUNNF(weak_alist_p,1)
LISPFUNNR(weak_alist_type,1)
LISPFUNNR(weak_alist_contents,1)
LISPFUNN(set_weak_alist_contents,2)
LISPFUN(weak_alist_assoc,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
LISPFUN(weak_alist_rassoc,seclass_default,2,0,norest,key,3,
        (kw(test),kw(test_not),kw(key)) )
LISPFUN(weak_alist_value,seclass_default,2,0,norest,key,2,
        (kw(test),kw(test_not)) )
LISPFUN(set_weak_alist_value,seclass_default,3,0,norest,key,2,
        (kw(test),kw(test_not)) )
/* ---------- SEQUENCE ---------- */
LISPFUNNR(sequencep,1)
LISPFUNN(defseq,1)
LISPFUNNR(elt,2)
LISPFUNN(setelt,3)
LISPFUN(subseq,seclass_read,2,1,norest,nokey,0,NIL)
LISPFUNNR(copy_seq,1)
LISPFUNNR(length,1)
LISPFUNNR(reverse,1)
LISPFUNN(nreverse,1)
LISPFUN(make_sequence,seclass_default,2,0,norest,key,2,
        (kw(initial_element),kw(update)) )
LISPFUN(coerced_subseq,seclass_default,2,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(concatenate,seclass_read,1,0,rest,nokey,0,NIL)
LISPFUN(map,seclass_default,3,0,rest,nokey,0,NIL)
LISPFUN(map_into,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(some,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(every,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(notany,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(notevery,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(reduce,seclass_default,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(initial_value)) )
LISPFUN(fill,seclass_default,2,0,norest,key,2, (kw(start),kw(end)) )
LISPFUN(replace,seclass_default,2,0,norest,key,4,
        (kw(start1),kw(end1),kw(start2),kw(end2)) )
LISPFUN(remove,seclass_default,2,0,norest,key,7,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),
         kw(test_not),kw(count)) )
LISPFUN(remove_if,seclass_default,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(remove_if_not,seclass_default,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(delete,seclass_default,2,0,norest,key,7,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),
         kw(test_not),kw(count)) )
LISPFUN(delete_if,seclass_default,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(delete_if_not,seclass_default,2,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(remove_duplicates,seclass_default,1,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
LISPFUN(delete_duplicates,seclass_default,1,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
LISPFUN(substitute,seclass_default,3,0,norest,key,7,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),
         kw(test_not),kw(count)) )
LISPFUN(substitute_if,seclass_default,3,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(substitute_if_not,seclass_default,3,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(nsubstitute,seclass_default,3,0,norest,key,7,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),
         kw(test_not),kw(count)) )
LISPFUN(nsubstitute_if,seclass_default,3,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(nsubstitute_if_not,seclass_default,3,0,norest,key,5,
        (kw(from_end),kw(start),kw(end),kw(key),kw(count)) )
LISPFUN(find,seclass_default,2,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
LISPFUN(find_if,seclass_default,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
LISPFUN(find_if_not,seclass_default,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
LISPFUN(position,seclass_default,2,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
LISPFUN(position_if,seclass_default,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
LISPFUN(position_if_not,seclass_default,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
LISPFUN(count,seclass_default,2,0,norest,key,6,
        (kw(from_end),kw(start),kw(end),kw(key),kw(test),kw(test_not)) )
LISPFUN(count_if,seclass_default,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
LISPFUN(count_if_not,seclass_default,2,0,norest,key,4,
        (kw(from_end),kw(start),kw(end),kw(key)) )
LISPFUN(mismatch,seclass_default,2,0,norest,key,8,
        (kw(start1),kw(end1),kw(start2),kw(end2),kw(from_end),
         kw(key),kw(test),kw(test_not)) )
LISPFUN(search,seclass_default,2,0,norest,key,8,
        (kw(start1),kw(end1),kw(start2),kw(end2),kw(from_end),
         kw(key),kw(test),kw(test_not)) )
LISPFUN(sort,seclass_default,2,0,norest,key,3, (kw(key),kw(start),kw(end)) )
LISPFUN(stable_sort,seclass_default,2,0,norest,key,3,
        (kw(key),kw(start),kw(end)) )
LISPFUN(merge,seclass_default,4,0,norest,key,1, (kw(key)) )
LISPFUN(read_char_sequence,seclass_default,2,0,norest,key,2,
        (kw(start),kw(end)) )
LISPFUN(write_char_sequence,seclass_default,2,0,norest,key,2,
        (kw(start),kw(end)) )
LISPFUN(read_byte_sequence,seclass_default,2,0,norest,key,4,
        (kw(start),kw(end),kw(no_hang),kw(interactive)) )
LISPFUN(write_byte_sequence,seclass_default,2,0,norest,key,4,
        (kw(start),kw(end),kw(no_hang),kw(interactive)) )
/* ---------- STREAM ---------- */
LISPFUN(symbol_stream,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUNNR(make_synonym_stream,1)
LISPFUNNF(synonym_stream_p,1)
LISPFUNNR(synonym_stream_symbol,1)
LISPFUN(make_broadcast_stream,seclass_read,0,0,rest,nokey,0,NIL)
LISPFUNNF(broadcast_stream_p,1)
LISPFUNNR(broadcast_stream_streams,1)
LISPFUN(make_concatenated_stream,seclass_read,0,0,rest,nokey,0,NIL)
LISPFUNNF(concatenated_stream_p,1)
LISPFUNNR(concatenated_stream_streams,1)
LISPFUNNR(make_two_way_stream,2)
LISPFUNNF(two_way_stream_p,1)
LISPFUNNR(two_way_stream_input_stream,1)
LISPFUNNR(two_way_stream_output_stream,1)
LISPFUNNR(make_echo_stream,2)
LISPFUNNF(echo_stream_p,1)
LISPFUNNR(echo_stream_input_stream,1)
LISPFUNNR(echo_stream_output_stream,1)
LISPFUN(make_string_input_stream,seclass_read,1,2,norest,nokey,0,NIL)
LISPFUNNR(string_input_stream_index,1)
LISPFUN(make_string_output_stream,seclass_read,0,0,norest,key,2,
        (kw(element_type),kw(line_position)))
LISPFUNN(get_output_stream_string,1)
LISPFUNNR(make_string_push_stream,1)
LISPFUNNF(string_stream_p,1)
LISPFUNNR(make_buffered_input_stream,2)
LISPFUNNR(buffered_input_stream_index,1)
LISPFUN(make_buffered_output_stream,seclass_read,1,1,norest,nokey,0,NIL)
#ifdef GENERIC_STREAMS
LISPFUNN(generic_stream_controller,1)
LISPFUNN(make_generic_stream,1)
LISPFUNN(generic_stream_p,1)
#endif
#ifdef KEYBOARD
LISPFUNN(make_keyboard_stream,0)
#endif
LISPFUN(terminal_raw,seclass_default,2,1,norest,nokey,0,NIL)
#ifdef SCREEN
LISPFUNN(make_window,0)
LISPFUNN(window_size,1)
LISPFUNN(window_cursor_position,1)
LISPFUNN(set_window_cursor_position,3)
LISPFUNN(clear_window,1)
LISPFUNN(clear_window_to_eot,1)
LISPFUNN(clear_window_to_eol,1)
LISPFUNN(delete_window_line,1)
LISPFUNN(insert_window_line,1)
LISPFUNN(highlight_on,1)
LISPFUNN(highlight_off,1)
LISPFUNN(window_cursor_on,1)
LISPFUNN(window_cursor_off,1)
#endif
LISPFUNNF(file_stream_p,1)
#ifdef PIPES
LISPFUN(make_pipe_input_stream,seclass_default,1,0,norest,key,3,
        (kw(element_type),kw(external_format),kw(buffered)) )
LISPFUN(make_pipe_output_stream,seclass_default,1,0,norest,key,3,
        (kw(element_type),kw(external_format),kw(buffered)) )
#ifdef PIPES2
LISPFUN(make_pipe_io_stream,seclass_default,1,0,norest,key,3,
        (kw(element_type),kw(external_format),kw(buffered)) )
#endif
#endif
#ifdef X11SOCKETS
LISPFUNN(make_x11socket_stream,2)
LISPFUNN(read_n_bytes,4)
LISPFUNN(write_n_bytes,4)
#endif
#ifdef SOCKET_STREAMS
LISPFUNN(socket_server_close,1)
LISPFUN(socket_server,seclass_default,0,1,norest,key,2,
        (kw(backlog),kw(interface)) )
LISPFUNN(socket_server_port,1)
LISPFUNN(socket_server_host,1)
LISPFUN(socket_accept,seclass_default,1,0,norest,key,4,
        (kw(element_type),kw(external_format),kw(buffered),kw(timeout)) )
LISPFUN(socket_wait,seclass_default,1,2,norest,nokey,0,NIL)
LISPFUN(socket_status,seclass_default,1,2,norest,nokey,0,NIL)
LISPFUN(socket_connect,seclass_default,1,1,norest,key,4,
        (kw(element_type),kw(external_format),kw(buffered),kw(timeout)) )
LISPFUNN(socket_stream_port,1)
LISPFUNN(socket_stream_host,1)
LISPFUN(socket_stream_peer,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(socket_stream_local,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(socket_options,seclass_default,1,0,rest,nokey,0,NIL)
#ifdef HAVE_SHUTDOWN
LISPFUNN(socket_stream_shutdown,2)
#endif
LISPFUN(make_stream,seclass_default,1,0,norest,key,4,
        (kw(direction),kw(element_type),kw(external_format),kw(buffered)) )
LISPFUNN(stream_handles,1)
#endif
LISPFUNNR(built_in_stream_open_p,1)
LISPFUNNR(input_stream_p,1)
LISPFUNNR(output_stream_p,1)
LISPFUNN(stream_element_type_eq,2)
LISPFUNNR(built_in_stream_element_type,1)
LISPFUNN(built_in_stream_set_element_type,2)
LISPFUNNR(stream_external_format,1)
LISPFUN(set_stream_external_format,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUNN(interactive_stream_p,1)
LISPFUN(built_in_stream_close,seclass_default,1,0,norest,key,1, (kw(abort)) )
LISPFUN(read_byte,seclass_default,1,2,norest,nokey,0,NIL)
LISPFUNN(read_byte_lookahead,1)
LISPFUNN(read_byte_will_hang_p,1)
LISPFUN(read_byte_no_hang,seclass_default,1,2,norest,nokey,0,NIL)
LISPFUN(read_integer,seclass_default,2,3,norest,nokey,0,NIL)
LISPFUN(read_float,seclass_default,2,3,norest,nokey,0,NIL)
LISPFUNN(write_byte,2)
LISPFUN(write_integer,seclass_default,3,1,norest,nokey,0,NIL)
LISPFUN(write_float,seclass_default,3,1,norest,nokey,0,NIL)
LISPFUN(file_position,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUNNR(file_length,1)
LISPFUNN(file_string_length,2)
LISPFUNN(line_number,1)
LISPFUN(allow_read_eval,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUNN(defgray,1)
/* ---------- SYMBOL ---------- */
LISPFUNN(putd,2)
LISPFUNN(find_subr,1)
LISPFUNN(proclaim_constant,2)
LISPFUNN(proclaim_symbol_macro,1)
LISPFUN(get,seclass_read,2,1,norest,nokey,0,NIL)
LISPFUN(getf,seclass_read,2,1,norest,nokey,0,NIL)
LISPFUNN(putf,3)
LISPFUNN(remf,2)
LISPFUNNR(get_properties,2)
LISPFUNN(putplist,2)
LISPFUNN(put,3)
LISPFUNN(remprop,2)
LISPFUNNR(symbol_package,1)
LISPFUNNR(symbol_plist,1)
LISPFUN(symbol_name,seclass_no_se,1,0,norest,nokey,0,NIL)
LISPFUNNR(cs_symbol_name,1)
LISPFUNNR(keywordp,1)
LISPFUN(gensym,seclass_read,0,1,norest,nokey,0,NIL)
/* ---------- LISPARIT ---------- */
LISPFUN(decimal_string,seclass_no_se,1,0,norest,nokey,0,NIL)
LISPFUNNF(zerop,1)
LISPFUNNF(plusp,1)
LISPFUNNF(minusp,1)
LISPFUNNF(oddp,1)
LISPFUNNF(evenp,1)
LISPFUN(gleich,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(ungleich,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(kleiner,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(groesser,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(klgleich,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(grgleich,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(max,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(min,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(plus,seclass_foldable,0,0,rest,nokey,0,NIL)
LISPFUN(minus,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUN(mal,seclass_foldable,0,0,rest,nokey,0,NIL)
LISPFUN(durch,seclass_foldable,1,0,rest,nokey,0,NIL)
LISPFUNNF(einsplus,1)
LISPFUNNF(einsminus,1)
LISPFUNNF(conjugate,1)
LISPFUN(gcd,seclass_foldable,0,0,rest,nokey,0,NIL)
LISPFUN(xgcd,seclass_foldable,0,0,rest,nokey,0,NIL)
LISPFUN(lcm,seclass_foldable,0,0,rest,nokey,0,NIL)
LISPFUNNR(exp,1)
LISPFUNNR(expt,2)
LISPFUN(log,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUNNR(sqrt,1)
LISPFUNNF(isqrt,1)
LISPFUNNR(abs,1)
LISPFUNNR(phase,1)
LISPFUNNR(signum,1)
LISPFUNNR(sin,1)
LISPFUNNR(cos,1)
LISPFUNNR(tan,1)
LISPFUNNR(cis,1)
LISPFUNNR(asin,1)
LISPFUNNR(acos,1)
LISPFUN(atan,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUNNR(sinh,1)
LISPFUNNR(cosh,1)
LISPFUNNR(tanh,1)
LISPFUNNR(asinh,1)
LISPFUNNR(acosh,1)
LISPFUNNR(atanh,1)
LISPFUN(float,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUNNF(rational,1)
LISPFUNNF(rationalize,1)
LISPFUNNF(numerator,1)
LISPFUNNF(denominator,1)
LISPFUN(floor,seclass_foldable,1,1,norest,nokey,0,NIL)
LISPFUN(ceiling,seclass_foldable,1,1,norest,nokey,0,NIL)
LISPFUN(truncate,seclass_foldable,1,1,norest,nokey,0,NIL)
LISPFUN(round,seclass_foldable,1,1,norest,nokey,0,NIL)
LISPFUNNF(mod,2)
LISPFUNNF(rem,2)
LISPFUN(ffloor,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUN(fceiling,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUN(ftruncate,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUN(fround,seclass_read,1,1,norest,nokey,0,NIL)
LISPFUNNF(decode_float,1)
LISPFUNNF(scale_float,2)
LISPFUNNF(float_radix,1)
LISPFUN(float_sign,seclass_foldable,1,1,norest,nokey,0,NIL)
LISPFUN(float_digits,seclass_foldable,1,1,norest,nokey,0,NIL)
LISPFUNNF(float_precision,1)
LISPFUNNF(integer_decode_float,1)
LISPFUN(complex,seclass_foldable,1,1,norest,nokey,0,NIL)
LISPFUNNF(realpart,1)
LISPFUNNF(imagpart,1)
LISPFUN(logior,seclass_foldable,0,0,rest,nokey,0,NIL)
LISPFUN(logxor,seclass_foldable,0,0,rest,nokey,0,NIL)
LISPFUN(logand,seclass_foldable,0,0,rest,nokey,0,NIL)
LISPFUN(logeqv,seclass_foldable,0,0,rest,nokey,0,NIL)
LISPFUNNF(lognand,2)
LISPFUNNF(lognor,2)
LISPFUNNF(logandc1,2)
LISPFUNNF(logandc2,2)
LISPFUNNF(logorc1,2)
LISPFUNNF(logorc2,2)
LISPFUNNF(boole,3)
LISPFUNNF(lognot,1)
LISPFUNNF(logtest,2)
LISPFUNNF(logbitp,2)
LISPFUNNF(ash,2)
LISPFUNNF(logcount,1)
LISPFUNNF(integer_length,1)
LISPFUNNR(byte,2)
LISPFUNNR(bytesize,1)
LISPFUNNR(byteposition,1)
LISPFUNNF(ldb,2)
LISPFUNNF(ldb_test,2)
LISPFUNNF(mask_field,2)
LISPFUNNF(dpb,3)
LISPFUNNF(deposit_field,3)
LISPFUN(random,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUN(random_posfixnum,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUN(make_random_state,seclass_default,0,1,norest,nokey,0,NIL)
LISPFUNNF(fakultaet,1)
LISPFUNNF(exquo,2)
LISPFUNNF(mod_expt,3)
LISPFUNN(long_float_digits,0)
LISPFUNN(set_long_float_digits,1)
LISPFUNNR(log2,1)
LISPFUNNR(log10,1)
/* ---------- FOREIGN ---------- */
#ifdef DYNAMIC_FFI
LISPFUNNR(validp,1)
LISPFUNN(set_validp,2)
LISPFUNNR(foreign_pointer,1)
LISPFUNN(set_foreign_pointer,2)
LISPFUNNR(unsigned_foreign_address,1)
LISPFUNNR(foreign_address_unsigned,1)
LISPFUNNR(foreign_address,1)
LISPFUN(foreign_function,seclass_read,2,0,norest,key,1,(kw(name)) )
LISPFUNN(sizeof,1)
LISPFUNN(bitsizeof,1)
LISPFUNN(lookup_foreign_variable,2)
LISPFUN(foreign_variable,seclass_read,2,0,norest,key,1,(kw(name)) )
LISPFUNN(foreign_value,1)
LISPFUNN(set_foreign_value,2)
LISPFUNN(foreign_type,1)
LISPFUN(element,seclass_default,1,0,rest,nokey,0,NIL)
LISPFUNN(deref,1)
LISPFUNN(slot,2)
LISPFUNN(cast,2)
LISPFUNN(offset,3)
LISPFUN(read_memory_as,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(write_memory_as,seclass_default,3,1,norest,nokey,0,NIL)
LISPFUN(exec_on_stack,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUNN(call_with_foreign_string,6)
LISPFUN(foreign_allocate,seclass_default,1,0,norest,key,3,
        (kw(initial_contents),kw(count),kw(read_only)))
LISPFUN(foreign_free,seclass_default,1,0,norest,key,1,(kw(full)))
LISPFUNN(lookup_foreign_function,2)
LISPFUN(foreign_call_out,seclass_default,1,0,rest,nokey,0,NIL)
#if defined(WIN32_NATIVE) || defined(HAVE_DLOPEN)
LISPFUN(foreign_library,seclass_default,1,1,norest,nokey,0,NIL)
LISPFUNN(close_foreign_library,1)
LISPFUNN(foreign_library_variable,4)
LISPFUNN(foreign_library_function,4)
#endif  /* WIN32_NATIVE || HAVE_DLOPEN */
#endif  /* DYNAMIC_FFI */
#ifdef HAVE_AFFI
LISPFUN(affi_libcall,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(mem_read,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(mem_write,seclass_default,3,1,norest,nokey,0,NIL)
LISPFUN(mem_write_vector,seclass_default,2,1,norest,nokey,0,NIL)
LISPFUN(affi_nonzerop,seclass_default,1,0,norest,nokey,0,NIL)
#endif
/* ---------- ZTHREAD ---------- */
#ifdef MULTITHREAD
LISPFUN(make_process,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUN(process_wait,seclass_default,3,0,rest,nokey,0,NIL)
LISPFUNN(call_with_timeout,3)
LISPFUNN(process_yield,0)
LISPFUNN(process_kill,1)
LISPFUN(process_interrupt,seclass_default,2,0,rest,nokey,0,NIL)
LISPFUNN(process_restart,1)
LISPFUNN(processp,1)
LISPFUNN(process_name,1)
LISPFUNN(process_active_p,1)
LISPFUNN(process_state,1)
LISPFUNN(process_whostate,1)
LISPFUNN(current_process,0)
LISPFUNN(list_processes,0)
#endif
