/* Debugging utilities. */

/* DEBUG_SPVW_ASSERT(expression) is an assertion used to debug SPVW. */
#ifdef DEBUG_SPVW
  #define DEBUG_SPVW_ASSERT(expression)  if (!(expression)) abort(); else (void)0/*;*/
#else
  #define DEBUG_SPVW_ASSERT(expression)  (void)0/*;*/
#endif

/* Output a memory range in hexadecimal notation */
local const char hex_table[] = "0123456789ABCDEF";
local void mem_hex_out (const void* buf, uintL count) {
  if (count > 0) {
    var DYNAMIC_ARRAY(cbuf,char,3*count+1);
    var const uintB* ptr1 = (const uintB*) buf;
    var char* ptr2 = &cbuf[0];
    dotimespL(count,count, {
      *ptr2++ = ' ';
      *ptr2++ = hex_table[floor(*ptr1,16)]; *ptr2++ = hex_table[*ptr1 % 16];
      ptr1++;
    });
    *ptr2 = '\0';
    fputs(cbuf,stdout);
    FREE_DYNAMIC_ARRAY(cbuf);
  }
}

/* Output a lisp object in lisp notation to standard output.
 can trigger GC */
global maygc object object_out (object obj) {
  pushSTACK(obj);
  pushSTACK(var_stream(S(terminal_io),strmflags_wr_ch_B)); # *TERMINAL-IO*
  prin1(&STACK_0,STACK_1); # output the object
  terpri(&STACK_0); # output a newline
  skipSTACK(1);
  return popSTACK(); # return the same object
}

#ifdef UNICODE
/* see string_to_asciz() */
local void string_out_ (FILE* out, object str, object encoding) {
  var uintL len;
  var uintL offset;
  var object string = unpack_string_ro(str,&len,&offset);
  var const chart* srcptr;
  unpack_sstring_alloca(string,len,offset, srcptr=);
  var uintL bytelen = cslen(encoding,srcptr,len);
  var DYNAMIC_ARRAY(buffer,uintB,bytelen+1);
  cstombs(encoding,srcptr,len,buffer,bytelen);
  buffer[bytelen] = 0;
  fputs((const char*)buffer,out);
  FREE_DYNAMIC_ARRAY(buffer);
}
#define string_out(o,s) string_out_(o,s,O(terminal_encoding))
#else /* no UNICODE */
local void string_out (FILE* out, object str) {
  var uintL len;
  var uintL offset;
  var object string = unpack_string_ro(str,&len,&offset);
  var const chart* srcptr;
  unpack_sstring_alloca(string,len,offset, srcptr=);
  var DYNAMIC_ARRAY(buffer,uintB,len+1);
  var uintB* destptr = buffer;
  while (len--) *destptr++ = as_cint(*srcptr++);
  *destptr++ = '\0'; /* append NUL byte */
  fputs((const char*)buffer,out);
  FREE_DYNAMIC_ARRAY(buffer);
}
#endif

/* the recursive helper for nobject_out() which does all the work
 must be enclosed in begin_system_call()/end_system_call() */
local void nobject_out1 (FILE* out, object obj, int level) {
  if (level) --level;
  else { fputs("<...>",out); return; }
 #define XOUT(x) nobject_out1(out,x,level)
  if (stringp(obj)) {
    fputc('"',out);
    string_out(out,obj);
    fputc('"',out);
  } else if (charp(obj)) {
    var object name = char_name(char_code(obj));
    fprintf(out,"[%c]",as_cint(char_code(obj)));
    if (!nullp(name)) {
      fputs("=#\\",out);
      string_out(out,name);
    }
  } else if (symbolp(obj)) {
    var object pack = Symbol_package(obj);
    if (nullp(pack)) fputs("#:",out); /* uninterned symbol */
    else if (eq(pack,O(keyword_package))) fputc(':',out);
    else {
      string_out(out,ThePackage(pack)->pack_shortest_name);
      fputs("::",out);
    }
    string_out(out,Symbol_name(obj));
  } else if (simple_vector_p(obj)) {
    var uintL len = vector_length(obj);
    var uintL elt_index = 0;
    fputs("#(",out);
    while (elt_index < len) {
      if (elt_index) fputc(' ',out);
      XOUT(TheSvector(obj)->data[elt_index++]);
    }
    fputc(')',out);
  } else if (consp(obj)) {
    fputc('(',out);
    loop {
      XOUT(Car(obj));
      obj = Cdr(obj);
      if (atomp(obj)) break;
      fputc(' ',out);
    }
    if (!nullp(obj)) {
      fputs(" . ",out);
      XOUT(obj);
    }
    fputc(')',out);
  } else if (functionp(obj)) {
    fputs("#<",out);
    if (subrp(obj)) {
      string_out(out, (((as_oint(subr_tab_ptr_as_object(&subr_tab)) <=
                         as_oint(obj))
                        && (as_oint(obj) <
                            as_oint(subr_tab_ptr_as_object(&subr_tab+1))))
                       ? O(printstring_subr) : O(printstring_addon_subr)));
      obj = TheSubr(obj)->name;
    } else if (cclosurep(obj)) {
      if (Closure_instancep(obj))
        fputs("FUNCALLABLE-INSTANCE",out);
      else
        string_out(out, O(printstring_compiled_closure));
      obj = Closure_name(obj);
    }
    #ifdef DYNAMIC_FFI
      else if (ffunctionp(obj)) {
      string_out(out,O(printstring_ffunction));
      obj = TheFfunction(obj)->ff_name;
    }
    #endif
      else { /* interpreted closure */
      string_out(out,O(printstring_closure));
      obj = TheIclosure(obj)->clos_name;
    }
    fputc(' ',out);
    XOUT(obj);
    fputc('>',out);
  } else if (fsubrp(obj)) {
    fputs("#<",out);
    string_out(out,O(printstring_fsubr));
    fputc(' ',out);
    XOUT(TheFsubr(obj)->name);
    fputc('>',out);
  } else if (pathnamep(obj)) {
    fputs("#(",out); XOUT(S(pathname));
   #define SLOT(s) fputc(' ',out); XOUT(S(K##s)); fputc(' ',out); \
     XOUT(ThePathname(obj)->pathname_##s)
   #if HAS_HOST
    SLOT(host);
   #endif
   #if HAS_DEVICE
    SLOT(device);
   #endif
    SLOT(directory); SLOT(name); SLOT(type); SLOT(version);
   #undef SLOT
    fputs(")",out);
  } else if (logpathnamep(obj)) {
   #ifdef LOGICAL_PATHNAMES
    fputs("#(",out); XOUT(S(logical_pathname));
   #define SLOT(s) fputc(' ',out); XOUT(S(K##s)); fputc(' ',out); \
     XOUT(TheLogpathname(obj)->pathname_##s)
    SLOT(host); SLOT(directory); SLOT(name); SLOT(type); SLOT(version);
   #undef SLOT
    fputc(')',out);
   #endif
  } else if (hash_table_p(obj)) {
    fputs("#(",out); XOUT(S(hash_table));
    fprintf(out," size=%u maxcount=%u mincount=%u free=",
            TheHashtable(obj)->ht_size,
            (uintL)posfixnum_to_V(TheHashtable(obj)->ht_maxcount),
            (uintL)posfixnum_to_V(TheHashtable(obj)->ht_mincount));
    fputs("\n  test=",out);
    if (ht_test_code_user_p(ht_test_code(record_flags(TheHashtable(obj))))) {
      XOUT(TheHashtable(obj)->ht_test); fputc('/',out);
      XOUT(TheHashtable(obj)->ht_hash);
    } else {
      switch (ht_test_code(record_flags(TheHashtable(obj))) & (bit(1)|bit(0))) {
        case 0: XOUT(S(eq)); break;
        case 1: XOUT(S(eql)); break;
        case 2: XOUT(S(equal)); break;
        case 3: XOUT(S(equalp)); break;
        default: abort();
      }
    }
    fputs("\n  KV=",out); XOUT(TheHashtable(obj)->ht_kvtable);
    fputc(')',out);
  } else if (packagep(obj)) {
    fputs("#<",out);
    string_out(out,O(printstring_package));
    fputc(' ',out);
    string_out(out,ThePackage(obj)->pack_name);
    fputc('>',out);
  } else if (weakpointerp(obj)) {
    fputs("#<",out);
    string_out(out,O(printstring_weakpointer));
    fputc(' ',out);
    XOUT(TheWeakpointer(obj)->wp_value);
    fputc(' ',out);
    if (weakpointerp(TheWeakpointer(obj)->wp_cdr)) fputs("#<next wp>",out);
    else XOUT(TheWeakpointer(obj)->wp_cdr);
    fprintf(out," 0x%lx>",as_oint(obj));
  } else if (fpointerp(obj)) {
    fputs("#<",out);
    if (!fp_validp(TheFpointer(obj))) string_out(out,O(printstring_invalid));
    string_out(out,O(printstring_fpointer));
    fprintf(out," 0x%lx>",TheFpointer(obj)->fp_pointer);
  } else if (structurep(obj)) {
    var uintL ii;
    fputs("#<structure",out);
    for(ii=0; ii<Structure_length(obj); ii++) {
      fputc(' ',out);
      XOUT(TheStructure(obj)->recdata[ii]);
    }
    fprintf(out," 0x%lx>",as_oint(obj));
  } else if (instancep(obj)) {
    fputs("#<instance ",out);
    XOUT(TheInstance(obj)->inst_class_version);
    fprintf(out," 0x%lx>",as_oint(obj));
  } else if (fixnump(obj)) fprintf(out,"%d",fixnum_to_V(obj));
  else if (eq(obj,unbound))   string_out(out,O(printstring_unbound));
  else if (eq(obj,nullobj))   fputs("#<NULLOBJ>",out);
  else if (eq(obj,disabled))  string_out(out,O(printstring_disabled_pointer));
  else if (eq(obj,specdecl))  string_out(out,O(printstring_special_reference));
  else if (eq(obj,eof_value)) string_out(out,O(printstring_eof));
  else if (eq(obj,dot_value)) string_out(out,O(printstring_dot));
#if defined(DYNAMIC_FFI)
  else if (faddressp(obj)) {
    fputs("#<",out); string_out(out,O(printstring_faddress)); fputs(" ",out);
    XOUT(TheFaddress(obj)->fa_base);
    fprintf(out," + 0x%lx>",TheFaddress(obj)->fa_offset);
  }
#endif
  else if (as_oint(obj) & wbit(frame_bit_o)) {
    fputs("#<frame ",out);
    switch (framecode(obj)) {
      case DYNBIND_frame_info: fputs("DYNBIND",out); break;
      case ENV1V_frame_info: fputs("ENV1V",out); break;
      case ENV1F_frame_info: fputs("ENV1F",out); break;
      case ENV1B_frame_info: fputs("ENV1B",out); break;
      case ENV1G_frame_info: fputs("ENV1G",out); break;
      case ENV1D_frame_info: fputs("ENV1D",out); break;
      case ENV2VD_frame_info: fputs("ENV2VD",out); break;
      case ENV5_frame_info: fputs("ENV5",out); break;
     #ifdef HAVE_SAVED_REGISTERS
      case CALLBACK_frame_info: fputs("CALLBACK",out); break;
     #endif
      case VAR_frame_info: fputs("VAR",out); break;
      case FUN_frame_info: fputs("FUN",out); break;
      case IBLOCK_frame_info: fputs("IBLOCK",out); break;
      case NESTED_IBLOCK_frame_info: fputs("NESTED_IBLOCK",out); break;
      case ITAGBODY_frame_info: fputs("ITAGBODY",out); break;
      case NESTED_ITAGBODY_frame_info: fputs("NESTED_ITAGBODY",out); break;
      case CBLOCK_CTAGBODY_frame_info: fputs("CBLOCK_CTAGBODY",out); break;
      case APPLY_frame_info: fputs("APPLY",out); break;
      case TRAPPED_APPLY_frame_info: fputs("TRAPPED_APPLY",out); break;
      case EVAL_frame_info: fputs("EVAL",out); break;
      case TRAPPED_EVAL_frame_info: fputs("TRAPPED_EVAL",out); break;
      case CATCH_frame_info: fputs("CATCH",out); break;
      case HANDLER_frame_info: fputs("HANDLER",out); break;
      case UNWIND_PROTECT_frame_info: fputs("UNWIND_PROTECT",out); break;
      case DRIVER_frame_info: fputs("DRIVER",out); break;
      default: fputs("**UNKNOWN**",out);
    }
    fprintf(out," 0x%lx>",as_oint(obj));
  } else if (builtin_stream_p(obj)) {
    fprintf(out,"#<built-in-stream type=%d flags=%d len=%d xlen=%d slen=%d",
            TheStream(obj)->strmtype,TheStream(obj)->strmflags,
            Stream_length(obj),Stream_xlength(obj),strm_len);
    switch (TheStream(obj)->strmtype) {
      case strmtype_pphelp: fputs(" pretty-print-help",out);
        fputs(" modus=",out); XOUT(TheStream(obj)->strm_pphelp_modus);
        fputs(" lpos=",out); XOUT(TheStream(obj)->strm_pphelp_lpos);
        fputs(" strings=",out); XOUT(TheStream(obj)->strm_pphelp_strings);
        break;
      case strmtype_file: fputs(" file",out);
        fputs(" name=",out); XOUT(TheStream(obj)->strm_file_name);
        fputs(" truename=",out); XOUT(TheStream(obj)->strm_file_truename);
        fprintf(out," channel=%d",
                TheHandle(TheStream(obj)->strm_buffered_channel));
        fputs(" eltype=",out); XOUT(TheStream(obj)->strm_eltype);
        fputs(" encoding=",out); XOUT(TheStream(obj)->strm_encoding);
        break;
      default: {
        int ii=0;
        for (; ii < Stream_length(obj) - strm_len; ii++) {
          fprintf(out," %d=",ii);
          XOUT(TheStream(obj)->strm_other[ii]);
        }
      }
    }
    fprintf(out," 0x%lx>",as_oint(obj));
  } else if (encodingp(obj)) {
    fputs("#<encoding eol=",out); XOUT(TheEncoding(obj)->enc_eol);
    fputs(" wce=",out); XOUT(TheEncoding(obj)->enc_towcs_error);
    fputs(" mbe=",out); XOUT(TheEncoding(obj)->enc_tombs_error);
   #ifdef UNICODE
    fputs(" cs=",out); XOUT(TheEncoding(obj)->enc_charset);
   #endif
    fprintf(out," 0x%lx>",as_oint(obj));
  }
  #ifndef TYPECODES
  else if (varobjectp(obj))
    fprintf(out,"#<varobject type=%d address=0x%lx>",
            varobject_type(TheVarobject(obj)),ThePointer(obj));
  #endif
  else fprintf(out,"#<huh?! address=0x%lx>",ThePointer(obj));
 #undef XOUT
}

/* non-consing, STACK non-modifying */
global object nobject_out (FILE* out, object obj) {
  var local int level = 5; /* for debugging */
  begin_system_call();
  if (out == NULL) out = stdout;
  nobject_out1(out,obj,level);
  fflush(out);
  end_system_call();
  return obj;
}

/* use (struct backtrace_t*) and not p_backtrace_t
   so that this is useable from the p_backtrace_t C++ definition */
local int back_trace_depth (const struct backtrace_t *bt) {
  var uintL bt_index = 0;
  var const struct backtrace_t *bt_fast = (bt ? bt : back_trace);
  var const struct backtrace_t *bt_slow = bt_fast;
  while (bt_fast) {
    bt_fast = bt_fast->bt_next; bt_index++;
    if (bt_fast == bt_slow) return -bt_index;
    if (bt_fast) { bt_fast = bt_fast->bt_next; bt_index++; }
    if (bt_fast == bt_slow) return -bt_index;
    bt_slow = bt_slow->bt_next;
  }
  return bt_index;
}

/* print a single struct backtrace_t object
 the caller must do begin_system_call()/end_system_call() ! */
local void bt_out (FILE* out, const struct backtrace_t *bt, uintL bt_index) {
  fprintf(out,"[%d/0x%lx]%s ",bt_index,bt,bt_beyond_stack_p(bt,STACK)?"<":">");
  nobject_out(out,bt->bt_function);
  if (bt->bt_num_arg >= 0)
    fprintf(out," %d args",bt->bt_num_arg);
  if (bt->bt_next)
    fprintf(out," delta: STACK=%d; SP=%d",
            STACK_item_count(top_of_back_trace_frame(bt),top_of_back_trace_frame(bt->bt_next)),
            (((long)((char*)(bt->bt_next) - (char*)bt) ^ SPoffset) - SPoffset) / sizeof(SPint));
  fputc('\n',out);
}

/* print the whole backtrace stack */
local uintL back_trace_out (FILE* out, const struct backtrace_t *bt) {
  var uintL bt_index = 0;
  var const struct backtrace_t *bt_fast = (bt ? bt : back_trace);
  var const struct backtrace_t *bt_slow = bt_fast;
  if (out == NULL) out = stdout;
  begin_system_call();
  while (bt_fast) {
    bt_out(out,bt_fast,bt_index++); bt_fast = bt_fast->bt_next;
    if (bt_fast == bt_slow) {
     circular:
      fprintf(out,"*** error: backtrace circularity detected!\n");
      bt_index = -bt_index;
      break;
    }
    if (bt_fast) {
      bt_out(out,bt_fast,bt_index++);
      bt_fast = bt_fast->bt_next;
    }
    if (bt_fast == bt_slow) goto circular;
    bt_slow = bt_slow->bt_next;
  }
  end_system_call();
  return bt_index;
}

global void back_trace_check (const struct backtrace_t *bt,
                              const char* label, const char* file, int line) {
  if (bt && back_trace_depth(bt)<0) {
    fprintf(stderr,"\n%s:%d:%s: circularity!\n",file,line,label);
    back_trace_out(stderr,bt);
    abort();
  }
}

#if 0 /* These functions are only for debugging. */

/* note that the following will _NOT_ work if CLISP uses O(dynamic_string)
 for DYNAMIC_STRING() because the length of the "dynamic string" will be
 that of its latest allocation, not value of the second argument!!! */
local object find_pack (char* pack_s) {
  if (pack_s) {
    var uintL pack_s_len = asciz_length(pack_s);
    DYNAMIC_STRING(pack,pack_s_len);
    var chart* ptr = TheSnstring(pack)->data;
    while (pack_s_len--) *ptr++ = as_chart(*pack_s++);
    value1 = find_package(pack);
    FREE_DYNAMIC_STRING(pack);
    return value1;
  } else return O(default_package); /* CL */
}

local object find_sym (char* name_s, char* pack_s) {
  var object pack = find_pack(pack_s);
  if (nullp(pack)) return NIL;
  var uintL name_s_len = asciz_length(name_s);
  DYNAMIC_STRING(name,name_s_len);
  { var chart* ptr = TheSnstring(name)->data;
    while (name_s_len--) *ptr++ = as_chart(*name_s++);
  }
  pushSTACK(name); pushSTACK(pack); funcall(L(find_symbol),2);
  FREE_DYNAMIC_STRING(name);
  return value1;
}

#endif

#ifdef DEBUG_SPVW
#define FUN(from,to,name) local to CONCAT(name,_) (from x) { return name(x); }
FUN(chart,cint,as_cint)
FUN(cint,chart,as_chart)
FUN(object,chart,char_code)
FUN(chart,object,code_char)
FUN(object,cint,char_int)
FUN(cint,object,int_char)
FUN(object,oint,as_oint)
FUN(oint,object,as_object)
FUN(object,sintB,Record_type)
FUN(object,uintB,Record_flags)
FUN(object,uintL,Record_length)
FUN(object,sintB,Array_type)
FUN(object,uintL,Srecord_length)
FUN(object,uintL,Xrecord_length)
FUN(object,uintL,Xrecord_xlength)
FUN(object,Cons,TheCons)
FUN(object,Record,TheRecord)
FUN(object,Srecord,TheSrecord)
FUN(object,Xrecord,TheXrecord)
FUN(object,void*,TheMachine)
FUN(object,Stream,TheStream)
FUN(object,object,Car)
FUN(object,object,Cdr)
FUN(object,Symbol,TheSymbol)
FUN(object,Hashtable,TheHashtable)
FUN(object,Dfloat,TheDfloat)
#undef FUN
#endif
