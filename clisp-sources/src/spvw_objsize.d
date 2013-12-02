# Determination of the object size (in bytes) of the various heap objects.

# ------------------------------ Specification --------------------------------

#ifdef TYPECODES

# Returns the typecode of the varobject at a given address.
# typecode_at(addr)

# Because the result of typecode_at may contain symbol flags, any switch
# statement on such a result must contain 'case_symbolwithflags:' instead of
# 'case_symbol:'.

#endif

# Computes the size (in bytes, including header and alignment) of the
# varobject starting at addr. The result is a multiple of varobject_alignment.
#  var uintL heapnr = ...;   [only needed if SPVW_PURE]
#  var_prepare_objsize;      [declaration of some variable, depends on heapnr]
#  objsize(addr)

# Returns the size (in bytes, including header and alignment) of an object.
# varobject_bytelength(obj)
# > obj: heap object of various length
# < result: number of occupied bytes
  global uintM varobject_bytelength (object obj);

# ------------------------------ Implementation -------------------------------

#ifdef TYPECODES

  # Varobjects contain in the first word a pointer to itself, except during GC.
  # (During GC it's a pointer to the new location, but with the same typecode.)
  #define typecode_at(addr)  mtypecode(((Varobject)(addr))->GCself)
  # or (equivalently):
  # define typecode_at(addr)  (((((Varobject)(addr))->header_flags)>>(oint_type_shift%8))&tint_type_mask)

  #define case_symbolwithflags  \
    case symbol_type:                                \
    case symbol_type|bit(var_bit0_t):                \
    case symbol_type|bit(var_bit1_t):                \
    case symbol_type|bit(var_bit1_t)|bit(var_bit0_t)

#endif

# Varobject_aligned_size(HS,ES,C) returns the length of an object of variable
# length with HS=Header-Size, ES=Element-Size, C=Element-Count.
# Varobject_aligned_size(HS,ES,C) = round_up(HS+ES*C,varobject_alignment) .
#define Varobject_aligned_size(HS,ES,C)           \
  ((ES % varobject_alignment) == 0                \
   ? # ES is divisible by varobject_alignment     \
     round_up(HS,varobject_alignment) + (ES)*(C)  \
   : round_up((HS)+(ES)*(C),varobject_alignment)) \

# length of an object, according to type:
#ifdef TYPECODES
  #define size_symbol()  # symbol \
      round_up( sizeof(symbol_), varobject_alignment)
#endif
#define size_sbvector(length)  # simple-bit-vector              \
  ( ceiling( (uintL)(length) + 8*offsetofa(sbvector_,data),     \
             8*varobject_alignment ) * varobject_alignment )
#define size_sb2vector(length)  # simple-2bit-vector            \
  ( ceiling( (uintL)(length) + 4*offsetofa(sbvector_,data),     \
             4*varobject_alignment ) * varobject_alignment )
#define size_sb4vector(length)  # simple-4bit-vector            \
  ( ceiling( (uintL)(length) + 2*offsetofa(sbvector_,data),     \
             2*varobject_alignment ) * varobject_alignment )
#define size_sb8vector(length)  # simple-8bit-vector \
 Varobject_aligned_size(offsetofa(sbvector_,data),1,(uintL)(length))
#define size_sb16vector(length)  # simple-16bit-vector \
 Varobject_aligned_size(offsetofa(sbvector_,data),2,(uintL)(length))
#define size_sb32vector(length)  # simple-32bit-vector \
 Varobject_aligned_size(offsetofa(sbvector_,data),4,(uintL)(length))
#define size_s8string(length)  # simple-8bit-string     \
 Varobject_aligned_size(offsetofa(s8string_,data),      \
                        sizeof(cint8),(uintL)(length))
#define size_s16string(length)  # simple-16bit-string   \
 Varobject_aligned_size(offsetofa(s16string_,data),     \
                        sizeof(cint16),(uintL)(length))
#define size_s32string(length)  # simple-32bit-string   \
 Varobject_aligned_size(offsetofa(s32string_,data),     \
                        sizeof(cint32),(uintL)(length))
#ifdef UNICODE
#define size_sstring(length)  # normal-simple-string \
  size_s32string(length)
#else
#define size_sstring(length)  # normal-simple-string \
  size_s8string(length)
#endif
#define size_svector(length)  # simple-vector                   \
  Varobject_aligned_size(offsetofa(svector_,data),              \
                         sizeof(gcv_object_t),(uintM)(uintL)(length))
#define size_sistring(xlength)  # simple indirect string        \
  Varobject_aligned_size(offsetof(sistring_,data),sizeof(uintB), \
                         sizeof(gcv_object_t)/sizeof(uintB)+(uintL)(xlength))
#define size_iarray(size)  # non-simple array, with                           \
  # size = dimension number + (1 if fill-pointer) + (1 if displaced-offset)   \
  Varobject_aligned_size(offsetofa(iarray_,dims),sizeof(uintL),(uintL)(size))
#define size_lrecord(length)  # Long-Record                     \
  Varobject_aligned_size(offsetofa(record_,recdata),            \
                         sizeof(gcv_object_t),(uintL)(length))
#define size_srecord(length)  # Simple-Record                   \
  Varobject_aligned_size(offsetofa(record_,recdata),            \
                         sizeof(gcv_object_t),(uintL)(length))
#define size_xrecord(length,xlength)  # Extended-Record                 \
  Varobject_aligned_size(offsetofa(record_,recdata),sizeof(uintB),      \
                         (sizeof(gcv_object_t)/sizeof(uintB))                 \
                         *(uintL)(length)+(uintL)(xlength))
#define size_bignum(length)  # Bignum \
  Varobject_aligned_size(offsetofa(bignum_,data),sizeof(uintD),(uintL)(length))
#ifdef TYPECODES
 #ifndef IMMEDIATE_FFLOAT
  #define size_ffloat()  # Single-Float \
      round_up( sizeof(ffloat_), varobject_alignment)
 #endif
 #define size_dfloat()  # Double-Float \
      round_up( sizeof(dfloat_), varobject_alignment)
#else
  #define size_ffloat()  # Single-Float \
      size_xrecord(0,sizeof(ffloat))
  #define size_dfloat()  # Double-Float \
      size_xrecord(0,sizeof(dfloat))
  #endif
  #define size_lfloat(length)  # Long-Float                     \
      Varobject_aligned_size(offsetofa(lfloat_,data),           \
                             sizeof(uintD),(uintL)(length))

# special functions for each type:
inline local uintM objsize_iarray (void* addr) { /* non-simple array */
  var uintL size;
  size = (uintL)iarray_rank((Iarray)addr);
  if (iarray_flags((Iarray)addr) & bit(arrayflags_fillp_bit))
    size++;
  if (iarray_flags((Iarray)addr) & bit(arrayflags_dispoffset_bit))
    size++;
  # size = dimension number + (1 if fill-pointer) + (1 if displaced-offset)
  return size_iarray(size);
}

inline local uintM objsize_s8string (void* addr) { /* mutable S8string */
  var uintL len = sstring_length((S8string)addr);
  var uintL size = size_s8string(len);
 #ifdef HAVE_SMALL_SSTRING
  # Some uprounding, for reallocate_small_string to work.
  if (size_s8string(1) < size_sistring(0)
      && size < size_sistring(0) && len > 0)
    size = size_sistring(0);
 #endif
  return size;
}

inline local uintM objsize_s16string (void* addr) { /* mutable S16string */
  var uintL len = sstring_length((S16string)addr);
  var uintL size = size_s16string(len);
 #ifdef HAVE_SMALL_SSTRING
  # Some uprounding, for reallocate_small_string to work.
  if (size_s16string(1) < size_sistring(0)
      && size < size_sistring(0) && len > 0)
    size = size_sistring(0);
 #endif
  return size;
}

inline local uintM objsize_s32string (void* addr) { /* S32string */
  return size_s32string(sstring_length((S32string)addr));
}

inline local uintM objsize_sstring (void* addr) { /* simple-string */
 #ifdef TYPECODES
  #ifdef HAVE_SMALL_SSTRING
  if (sstring_reallocatedp((Sstring)addr)) goto case_sistring;
  switch ((((Sstring)addr)->tfl >> 3) & 7) {
    case (Sstringtype_8Bit << 1) + 0: goto case_s8string;
    case (Sstringtype_8Bit << 1) + 1: goto case_imm_s8string;
    case (Sstringtype_16Bit << 1) + 0: goto case_s16string;
    case (Sstringtype_16Bit << 1) + 1: goto case_imm_s16string;
    case (Sstringtype_32Bit << 1) + 0: goto case_s32string;
    case (Sstringtype_32Bit << 1) + 1: goto case_s32string;
    default: /*NOTREACHED*/ abort();
  }
  #endif
 #else
  switch (record_type((Record)addr)) {
   #ifdef UNICODE
    case Rectype_S32string: case Rectype_Imm_S32string:
      goto case_s32string;
    #ifdef HAVE_SMALL_SSTRING
    case Rectype_Imm_S8string:
      goto case_imm_s8string;
    case Rectype_S8string:
      goto case_s8string;
    case Rectype_Imm_S16string:
      goto case_imm_s16string;
    case Rectype_S16string:
      goto case_s16string;
    case Rectype_reallocstring:
      goto case_sistring;
    #endif
   #else
    case Rectype_S8string: case Rectype_Imm_S8string:
      goto case_s8string;
   #endif
    default: /*NOTREACHED*/ abort();
  }
 #endif
 #ifdef UNICODE
  case_s32string:
    return size_s32string(sstring_length((S32string)addr));
  #ifdef HAVE_SMALL_SSTRING
  case_imm_s8string:
    return size_s8string(sstring_length((S8string)addr));
  case_s8string:
    return objsize_s8string(addr);
  case_imm_s16string:
    return size_s16string(sstring_length((S16string)addr));
  case_s16string:
    return objsize_s16string(addr);
  case_sistring:
    return size_sistring(sstring_length((Sstring)addr));
  #endif
 #else
  case_s8string:
    return size_s8string(sstring_length((S8string)addr));
 #endif
}

#ifdef SPVW_MIXED

local uintM objsize (void* addr) {
 #ifdef TYPECODES
  switch (typecode_at(addr) & ~bit(garcol_bit_t)) # type of the object
 #else
  switch (record_type((Record)addr)) {
    case_Rectype_Sbvector_above;
    case_Rectype_Sb2vector_above;
    case_Rectype_Sb4vector_above;
    case_Rectype_Sb8vector_above;
    case_Rectype_Sb16vector_above;
    case_Rectype_Sb32vector_above;
    case_Rectype_Svector_above;
    case_Rectype_mdarray_above;
    case_Rectype_obvector_above;
    case_Rectype_ob2vector_above;
    case_Rectype_ob4vector_above;
    case_Rectype_ob8vector_above;
    case_Rectype_ob16vector_above;
    case_Rectype_ob32vector_above;
    case_Rectype_ostring_above;
    case_Rectype_ovector_above;
    case_Rectype_Bignum_above;
    case_Rectype_Lfloat_above;
   #ifdef UNICODE
    case Rectype_S32string: case Rectype_Imm_S32string:
      goto case_s32string;
    #ifdef HAVE_SMALL_SSTRING
    case Rectype_Imm_S8string:
      goto case_imm_s8string;
    case Rectype_S8string:
      goto case_s8string;
    case Rectype_Imm_S16string:
      goto case_imm_s16string;
    case Rectype_S16string:
      goto case_s16string;
    case Rectype_reallocstring:
      goto case_sistring;
    #endif
   #else
    case Rectype_S8string: case Rectype_Imm_S8string:
      goto case_s8string;
   #endif
    default: goto case_record;
  }
  switch (0)
 #endif
  {
   #ifdef TYPECODES
    case_symbolwithflags: # Symbol
      return size_symbol();
   #endif
    case_sbvector: # simple-bit-vector
      return size_sbvector(sbvector_length((Sbvector)addr));
    case_sb2vector: # simple-2bit-vector
      return size_sb2vector(sbvector_length((Sbvector)addr));
    case_sb4vector: # simple-4bit-vector
      return size_sb4vector(sbvector_length((Sbvector)addr));
    case_sb8vector: # simple-8bit-vector
      return size_sb8vector(sbvector_length((Sbvector)addr));
    case_sb16vector: # simple-16bit-vector
      return size_sb16vector(sbvector_length((Sbvector)addr));
    case_sb32vector: # simple-32bit-vector
      return size_sb32vector(sbvector_length((Sbvector)addr));
   #ifdef TYPECODES
    case_sstring: # normal-simple-string
      #ifdef HAVE_SMALL_SSTRING
      if (sstring_reallocatedp((Sstring)addr)) goto case_sistring;
      switch ((((Sstring)addr)->tfl >> 3) & 7) {
        case (Sstringtype_8Bit << 1) + 0: goto case_s8string;
        case (Sstringtype_8Bit << 1) + 1: goto case_imm_s8string;
        case (Sstringtype_16Bit << 1) + 0: goto case_s16string;
        case (Sstringtype_16Bit << 1) + 1: goto case_imm_s16string;
        case (Sstringtype_32Bit << 1) + 0: goto case_s32string;
        case (Sstringtype_32Bit << 1) + 1: goto case_s32string;
        default: /*NOTREACHED*/ abort();
      }
      #endif
   #endif
    /*FALLTHROUGH*/
   #ifdef UNICODE
    case_s32string:
      return size_s32string(sstring_length((S32string)addr));
    #ifdef HAVE_SMALL_SSTRING
    case_imm_s8string:
      return size_s8string(sstring_length((S8string)addr));
    case_s8string:
      return objsize_s8string(addr);
    case_imm_s16string:
      return size_s16string(sstring_length((S16string)addr));
    case_s16string:
      return objsize_s16string(addr);
    case_sistring:
      return size_sistring(sstring_length((Sstring)addr));
    #endif
   #else
    case_s8string:
      return size_s8string(sstring_length((S8string)addr));
   #endif
    case_svector: # simple-vector
      return size_svector(svector_length((Svector)addr));
    case_mdarray: case_obvector: case_ob2vector: case_ob4vector:
    case_ob8vector: case_ob16vector: case_ob32vector: case_ostring:
    case_ovector: # non-simple array
      return objsize_iarray(addr);
    case_record: # Record
      if (record_type((Record)addr) < rectype_longlimit) {
        if (record_type((Record)addr) < rectype_limit)
          return size_srecord(srecord_length((Srecord)addr));
        else
          return size_xrecord(xrecord_length((Xrecord)addr),
                              xrecord_xlength((Xrecord)addr));
      } else
        return size_lrecord(lrecord_length((Lrecord)addr));
    case_bignum: # Bignum
      return size_bignum(bignum_length((Bignum)addr));
  #ifdef TYPECODES
   #ifndef IMMEDIATE_FFLOAT
    case_ffloat: # Single-Float
      return size_ffloat();
   #endif
    case_dfloat: # Double-Float
      return size_dfloat();
  #endif
    case_lfloat: # Long-Float
      return size_lfloat(lfloat_length((Lfloat)addr));
   #ifdef TYPECODES
    case_machine:
    #ifndef SIXBIT_TYPECODES
    case_char:
    case_subr:
    case_system:
    #endif
    case_fixnum:
    case_sfloat:
    #ifdef IMMEDIATE_FFLOAT
    case_ffloat:
    #endif
      # these are direct objects, no pointers.
   #endif
      default: # these are no objects of variable length.
          /*NOTREACHED*/ abort();
  }
}

#define var_prepare_objsize

#endif # SPVW_MIXED

#ifdef SPVW_PURE

inline local uintM objsize_symbol (void* addr) { # Symbol
  return size_symbol();
}
inline local uintM objsize_sbvector (void* addr) { # simple-bit-vector
  return size_sbvector(sbvector_length((Sbvector)addr));
}
inline local uintM objsize_sb2vector (void* addr) { # simple-2bit-vector
  return size_sb2vector(sbvector_length((Sbvector)addr));
}
inline local uintM objsize_sb4vector (void* addr) { # simple-4bit-vector
  return size_sb4vector(sbvector_length((Sbvector)addr));
}
inline local uintM objsize_sb8vector (void* addr) { # simple-8bit-vector
  return size_sb8vector(sbvector_length((Sbvector)addr));
}
inline local uintM objsize_sb16vector (void* addr) { # simple-16bit-vector
  return size_sb16vector(sbvector_length((Sbvector)addr));
}
inline local uintM objsize_sb32vector (void* addr) { # simple-32bit-vector
  return size_sb32vector(sbvector_length((Sbvector)addr));
}
inline local uintM objsize_svector (void* addr) { # simple-vector
  return size_svector(svector_length((Svector)addr));
}
inline local uintM objsize_sxrecord (void* addr) { # Record
  if (record_type((Record)addr) < rectype_limit)
    return size_srecord(srecord_length((Srecord)addr));
  else
    return size_xrecord(xrecord_length((Xrecord)addr),
                        xrecord_xlength((Xrecord)addr));
}
inline local uintM objsize_lrecord (void* addr) { # Lrecord
  return size_lrecord(lrecord_length((Lrecord)addr));
}
inline local uintM objsize_bignum (void* addr) { # Bignum
  return size_bignum(bignum_length((Bignum)addr));
}
#ifndef IMMEDIATE_FFLOAT
inline local uintM objsize_ffloat (void* addr) { # Single-Float
  return size_ffloat();
}
#endif
inline local uintM objsize_dfloat (void* addr) { # Double-Float
  return size_dfloat();
}
inline local uintM objsize_lfloat (void* addr) { # Long-Float
  return size_lfloat(lfloat_length((Lfloat)addr));
}

# table of functions:
typedef uintM (*objsize_func_t) (void* addr);
local objsize_func_t objsize_table[heapcount];

local void init_objsize_table (void) {
  var uintL heapnr;
  for (heapnr=0; heapnr<heapcount; heapnr++) {
    switch (heapnr) {
     case_symbol:
      objsize_table[heapnr] = &objsize_symbol; break;
     case_sbvector:
      objsize_table[heapnr] = &objsize_sbvector; break;
     case_sb2vector:
      objsize_table[heapnr] = &objsize_sb2vector; break;
     case_sb4vector:
      objsize_table[heapnr] = &objsize_sb4vector; break;
     case_sb8vector:
      objsize_table[heapnr] = &objsize_sb8vector; break;
     case_sb16vector:
      objsize_table[heapnr] = &objsize_sb16vector; break;
     case_sb32vector:
      objsize_table[heapnr] = &objsize_sb32vector; break;
     case_sstring:
      objsize_table[heapnr] = &objsize_sstring; break;
     case_svector:
      objsize_table[heapnr] = &objsize_svector; break;
     case_mdarray: case_obvector: case_ob2vector: case_ob4vector:
     case_ob8vector: case_ob16vector: case_ob32vector: case_ostring:
     case_ovector:
      objsize_table[heapnr] = &objsize_iarray; break;
     case_sxrecord:
      objsize_table[heapnr] = &objsize_sxrecord; break;
     case_lrecord:
      objsize_table[heapnr] = &objsize_lrecord; break;
     case_bignum:
      objsize_table[heapnr] = &objsize_bignum; break;
    #ifndef IMMEDIATE_FFLOAT
     case_ffloat:
      objsize_table[heapnr] = &objsize_ffloat; break;
    #endif
     case_dfloat:
      objsize_table[heapnr] = &objsize_dfloat; break;
     case_lfloat:
      objsize_table[heapnr] = &objsize_lfloat; break;
     case_machine:
     case_char:
     case_subr:
     case_system:
     case_fixnum:
     case_sfloat:
    #ifdef IMMEDIATE_FFLOAT
     case_ffloat:
    #endif
      # these are direct objects, no pointers.
      /* case_ratio: */
      /* case_complex: */
      default:
        # these are no objects of variable length.
        objsize_table[heapnr] = (objsize_func_t)&abort; break;
    }
  }
}

#define var_prepare_objsize  \
    var objsize_func_t _objsize_func = objsize_table[heapnr];
#define objsize(addr)  (*_objsize_func)(addr)

#endif # SPVW_PURE

global uintM varobject_bytelength (object obj) {
 #ifdef SPVW_PURE
  var uintL heapnr = typecode(obj);
 #endif
  var_prepare_objsize;
  return objsize(TheVarobject(obj));
}
