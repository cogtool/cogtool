/*
 * Hash-Tables in CLISP
 * Bruno Haible 1990-2005
 * Sam Steingold 1998-2004
 * German comments translated into English: Stefan Kain 2002-01-29
 */

#include "lispbibl.c"
#include "arilev0.c"            /* for Hashcode-Calculation */
#include "aridecl.c"            /* for Short-Floats */


/* Structure of a Hash-Table:
 Pairs (Key . Value) are stored in a vector,
 which is indexed by (hashcode Key).
 For a running MAPHASH to be uninfluenced by a GC, this
 vector is not reorganized because of GC. But as every (hashcode key) can
 change on each GC, we build in an additional indexing-level:
 (hashcode Key) indexes an index-vector; an index points into the
 key-value-vector there, and the (key . value) is located there.
 In order to save memory, we do not store a cons (key . value)
 in the vector, but we simply store key and value consecutively.
 One might want to resolve collisions [several keys have the same
 (hascode Key)] with lists. Due to the fact that the key-value-vector
 (again because of MAPHASH) should be uninfluenced on GC and GC changes
 the set of collisions, we need an additional index-vector,
 called the next-vector, which is interlaced with the key-value-vector
 and which contains a "list"-structure.
 sketch:
   key --> (hashcode key) as index in index-vector.
   Key1 --> 3, Key2 --> 1, Key4 --> 3.
   index-vector      #( nix {indexkey2} nix {indexkey1,indexkey4} nix ... )
                   = #( nix 1 nix 0 nix ... )
   next-vector       #(           3           nix           leer           nix           leer)
   key-value-vector  #( key1 val1 3 key2 val2 nix leer leer leer key4 val4 nix leer leer leer)
 access to a (Key . Value) - pair works as follows:
   index := (aref Index-Vektor (hashcode Key))
   until index = nix
     if (eql Key (aref KVVektor 3*index)) return (aref KVVektor 3*index+1)
     index := (aref Next-Vektor index) ; take "CDR" of the list
            = (aref KVVektor 3*index+2)
   return notfound.
 If the index-vector is enlarged, all hashcodes and the content of
 index-vector and the content of next-vector have to be recalculated.
 If the next-vector and key-value-vector are enlarged, the remaining
 elements can be filled with "leer" , without having to calculate
 a new hashcode.
 In order to have a fast MAPHASH following a CLRHASH or multiple REMHASH,
 when the table contains much fewer elements than its capacity,
 the entries could be kept "left-aligned" in the key-value-vector, i.e.
 all "leer" go to the right. Thus, MAPHASH only needs to graze over the
 elements count-1,...,1,0 of the key-value-vector. But REMHASH must
 - after it has created a gap - copy the last key-value-pair
 (Nummer count-1) into the gap.
 We treat such cases by possibly shrinking the key-value-vector and
 the next-vector on CLRHASH and REMHASH.
 We keep the "leer"-entries in next-vector in a free-"list", so that PUTHASH
 finds a free entry.
 The lengths of index-vector and next-vector do not depend on each other.
 We choose the ratio of their lengths to be 2:1.
 The hash-table is enlarged, when the free-list is empty, i.e.
 COUNT becomes greater than MAXCOUNT. Thereby, MAXCOUNT and SIZE are
 multiplied by REHASH-SIZE (>1).
 The hash-table is reduced, when COUNT < MINCOUNT. Thereby,
 MAXCOUNT and SIZE are multiplied with 1/REHASH-SIZE (<1) . We choose
 MINCOUNT = MAXCOUNT / REHASH-SIZE^2, so that COUNT can vary
 in both directions by the same amount (on a logarithmic scale)
 after the enlargement of the table.

 data-structure of the hash-table (see LISPBIBL.D):
 recflags codes the type and the state of the hash-table:
   Bit 0..3 encode the test and the hash-code function
   Bit 4..6 are state used to emit warnings for not GC-invariant keys
   Bit 7 set, when table must be reorganized after GC
 ht_size                uintL>0 = length of the ITABLE
 ht_maxcount            Fixnum>0 = length of the NTABLE
 ht_kvtable             key-value-vector, a HashedAlist or WeakHashedAlist
                        with 3*MAXCOUNT data fields and
                        hal_itable     index-vector of length SIZE
                        hal_count      number of entries in the table, <=MAXCOUNT
                        hal_freelist   start-index of the free-list
 ht_rehash_size         growth-rate on reorganization. Float >1.1
 ht_mincount_threshold  ratio MINCOUNT/MAXCOUNT = 1/rehash-size^2
 ht_mincount            Fixnum>=0, lower bound for COUNT
 ht_test                hash-table-test - for define-hash-table-test
 ht_hash                hash function  - for define-hash-table-test
 entry "leer" in key-value-vector is = #<UNBOUND>.
 entry "leer" in next-vector is filled by the free-list.
 entry "nix" in index-vector and in next-vector is = #<UNBOUND>. */
#define leer  unbound
#define nix   unbound

#define HT_GOOD_P(ht)                                   \
  (posfixnump(TheHashtable(ht)->ht_maxcount) &&         \
   posfixnump(TheHashtable(ht)->ht_mincount))

/* ============================ Hash functions ============================ */

/* Rotates a hashcode x by n bits to the left (0<n<32).
 rotate_left(n,x) */
#define rotate_left(n,x)  (((x) << (n)) | ((x) >> (32-(n))))

/* mixes two hashcodes.
 one is rotated by 5 bits, then the other one is XOR-ed to it. */
#define misch(x1,x2) (rotate_left(5,x1) ^ (x2))

/* ------------------------------ FASTHASH EQ ------------------------------ */

/* UP: Calculates the FASTHASH-EQ-hashcode of an object.
 hashcode1(obj)
 It is valid only until the next GC.
 (eq X Y) implies (= (hashcode1 X) (hashcode1 Y)).
 > obj: an object
 < result: hashcode, a 32-Bit-number */
local uint32 hashcode1 (object obj);
#if (defined(WIDE_HARD) || defined(WIDE_SOFT)) && defined(TYPECODES)
 #define hashcode1(obj)  ((uint32)untype(obj))
#else
 #define hashcode1(obj)  ((uint32)as_oint(obj)) /* address (Bits 23..0) and typeinfo */
#endif

/* Tests whether hashcode1 of an object is guaranteed to be GC-invariant. */
global bool gcinvariant_hashcode1_p (object obj) {
  return gcinvariant_object_p(obj);
}

/* ----------------------------- STABLEHASH EQ ----------------------------- */

/* UP: Calculates the STABLEHASH-EQ-hashcode of an object.
 hashcode1stable(obj)
 It is valid across GC for instances of STANDARD-STABLEHASH, STRUCTURE-STABLEHASH.
 (eq X Y) implies (= (hashcode1 X) (hashcode1 Y)).
 > obj: an object
 < result: hashcode, a 32-Bit-number */
global uint32 hashcode1stable (object obj) {
  if (instancep(obj)) {
    var object obj_forwarded = obj;
    instance_un_realloc(obj_forwarded);
    /* No need for instance_update here; if someone redefines a class in
       such a way that the hashcode slot goes away, the behaviour is
       undefined. */
    var object cv = TheInstance(obj_forwarded)->inst_class_version;
    var object clas = TheClassVersion(cv)->cv_class;
    if (!nullp(TheClass(clas)->subclass_of_stablehash_p)) {
      /* The hashcode slot is known to be at position 1, thanks to
         :FIXED-SLOT-LOCATIONS. */
      return posfixnum_to_V(TheInstance(obj_forwarded)->other[0]);
    }
  } else if (structurep(obj)) {
    if (!nullp(memq(S(structure_stablehash),TheStructure(obj)->structure_types))) {
      /* The hashcode slot is known to be at position 1, thanks to the way
         slots are inherited in DEFSTRUCT. */
      return posfixnum_to_V(TheStructure(obj)->recdata[1]);
    }
  } else if (symbolp(obj)) {
    var object hashcode = TheSymbol(obj)->hashcode;
    if (eq(hashcode,unbound)) {
      /* The first access to a symbol's hash code computes it. */
      pushSTACK(unbound); C_random_posfixnum(); hashcode = value1;
      TheSymbol(obj)->hashcode = hashcode;
    }
    return posfixnum_to_V(hashcode);
  }
  return hashcode1(obj);
}

/* UP: Tests whether an object is instance of STANDARD-STABLEHASH or
   STRUCTURE-STABLEHASH. */
local inline bool instance_of_stablehash_p (object obj) {
  if (instancep(obj)) {
    var object obj_forwarded = obj;
    instance_un_realloc(obj_forwarded);
    var object cv = TheInstance(obj_forwarded)->inst_class_version;
    var object clas = TheClassVersion(cv)->cv_class;
    return !nullp(TheClass(clas)->subclass_of_stablehash_p);
  } else if (structurep(obj)) {
    return !nullp(memq(S(structure_stablehash),TheStructure(obj)->structure_types));
  }
  return false;
}

/* Tests whether hashcode1stable of an object is guaranteed to be
   GC-invariant. */
global bool gcinvariant_hashcode1stable_p (object obj) {
  return gcinvariant_object_p(obj)
         || instance_of_stablehash_p(obj) || symbolp(obj);
}

/* ----------------------------- FASTHASH EQL ----------------------------- */

/* UP: Calculates the FASTHASH-EQL-hashcode of an object.
 hashcode2(obj)
 It is valid only until the next GC.
 (eql X Y) implies (= (hashcode2 X) (hashcode2 Y)).
 > obj: an object
 < result: hashcode, a 32-Bit-number */
global uint32 hashcode2 (object obj);
/* auxiliary functions for known type:
 Fixnum: fixnum-value */
local uint32 hashcode_fixnum (object obj);
#if 0
local uint32 hashcode_fixnum(object obj) { return hashcode1(obj); }
#else
#define hashcode_fixnum(obj)  hashcode1(obj)
#endif
/* Bignum: length*2 + (MSD*2^16 + LSD) */
local uint32 hashcode_bignum (object obj) {
  var uintL len = (uintL)Bignum_length(obj); /* number of Words */
  return
   #if (intDsize==32)
    misch(TheBignum(obj)->data[0],     /* MSD */
          TheBignum(obj)->data[len-1]) /* and LSD */
   #elif (intDsize==16) || (bn_minlength<4)
    highlow32(TheBignum(obj)->data[0],     /* MSD */
              TheBignum(obj)->data[len-1]) /* and LSD */
   #else  /* (intDsize==8) && (bn_minlength>=4) */
    ( (((uint32)TheBignum(obj)->data[0]) << 24)
      |(((uint32)TheBignum(obj)->data[1]) << 16)
      |(((uint32)TheBignum(obj)->data[2]) << 8)
      |((uint32)TheBignum(obj)->data[len-1]))
   #endif
    + 2*len;                    /* and length*2 */
}
/* Short-Float: internal representation */
local uint32 hashcode_sfloat (object obj);
#if 0
local uint32 hashcode_sfloat(object obj) { return hashcode1(obj); }
#else
#define hashcode_sfloat(obj)  hashcode1(obj)
#endif
/* Single-Float: 32 Bit */
local uint32 hashcode_ffloat (object obj) {
  return ffloat_value(obj);
}
/* Double-Float: leading 32 Bits */
local uint32 hashcode_dfloat (object obj) {
 #ifdef intQsize
  return (uint32)(TheDfloat(obj)->float_value >> 32);
 #else
  return TheDfloat(obj)->float_value.semhi;
 #endif
}
/* Long-Float: mixture of exponent, length, first 32 bits */
extern uint32 hashcode_lfloat (object obj); /* see LFLOAT.D */
/* in general: */
global uint32 hashcode2 (object obj) {
 #ifdef TYPECODES
  if (!numberp(obj)) {          /* a number? */
    /* no -> take EQ-hashcode (for characters, EQL == EQ) : */
    return hashcode1(obj);
  } else {              /* yes -> differentiate according to typecode */
    switch (typecode(obj) & ~(bit(number_bit_t)|bit(sign_bit_t))) {
      case fixnum_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /* Fixnum */
        return hashcode_fixnum(obj);
      case bignum_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /* Bignum */
        return hashcode_bignum(obj);
      case sfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /*Short-Float*/
        return hashcode_sfloat(obj);
      case ffloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /*Single-Float*/
        return hashcode_ffloat(obj);
      case dfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /*Double-Float*/
        return hashcode_dfloat(obj);
      case lfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /* Long-Float */
        return hashcode_lfloat(obj);
      case ratio_type & ~(bit(number_bit_t)|bit(sign_bit_t)): { /* Ratio */
        /* hash both components, mix */
        var uint32 code1 = hashcode2(TheRatio(obj)->rt_num);
        var uint32 code2 = hashcode2(TheRatio(obj)->rt_den);
        return misch(code1,code2);
      }
      case complex_type & ~(bit(number_bit_t)|bit(sign_bit_t)): { /* Complex */
        /* hash both components, mix */
        var uint32 code1 = hashcode2(TheComplex(obj)->c_real);
        var uint32 code2 = hashcode2(TheComplex(obj)->c_imag);
        return misch(code1,code2);
      }
      default: NOTREACHED;
    }
  }
 #else
  if (orecordp(obj))
    switch (Record_type(obj)) {
      case Rectype_Bignum:
        return hashcode_bignum(obj);
      case Rectype_Ffloat:
        return hashcode_ffloat(obj);
      case Rectype_Dfloat:
        return hashcode_dfloat(obj);
      case Rectype_Lfloat:
        return hashcode_lfloat(obj);
      case Rectype_Ratio: {     /* hash both components, mix */
        var uint32 code1 = hashcode2(TheRatio(obj)->rt_num);
        var uint32 code2 = hashcode2(TheRatio(obj)->rt_den);
        return misch(code1,code2);
      }
      case Rectype_Complex: {   /* hash both components, mix */
        var uint32 code1 = hashcode2(TheComplex(obj)->c_real);
        var uint32 code2 = hashcode2(TheComplex(obj)->c_imag);
        return misch(code1,code2);
      }
      default:
        break;
    }
  else if (immediate_number_p(obj)) {
    if (as_oint(obj) & wbit(4))
      return hashcode_sfloat(obj);
    else
      return hashcode_fixnum(obj);
  }
  return hashcode1(obj);
 #endif
}

/* Tests whether hashcode2 of an object is guaranteed to be GC-invariant. */
global bool gcinvariant_hashcode2_p (object obj) {
  return numberp(obj) || gcinvariant_object_p(obj);
}

/* ---------------------------- STABLEHASH EQL ---------------------------- */

/* UP: Calculates the STABLEHASH-EQL-hashcode of an object.
 hashcode2stable(obj)
 It is valid across GC for instances of STANDARD-STABLEHASH, STRUCTURE-STABLEHASH.
 (eql X Y) implies (= (hashcode2stable X) (hashcode2stable Y)).
 > obj: an object
 < result: hashcode, a 32-Bit-number */
global uint32 hashcode2stable (object obj) {
 #ifdef TYPECODES
  if (!numberp(obj)) {          /* a number? */
    /* no -> take EQ-hashcode (for characters, EQL == EQ) : */
    return hashcode1stable(obj);
  } else {              /* yes -> differentiate according to typecode */
    switch (typecode(obj) & ~(bit(number_bit_t)|bit(sign_bit_t))) {
      case fixnum_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /* Fixnum */
        return hashcode_fixnum(obj);
      case bignum_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /* Bignum */
        return hashcode_bignum(obj);
      case sfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /*Short-Float*/
        return hashcode_sfloat(obj);
      case ffloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /*Single-Float*/
        return hashcode_ffloat(obj);
      case dfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /*Double-Float*/
        return hashcode_dfloat(obj);
      case lfloat_type & ~(bit(number_bit_t)|bit(sign_bit_t)): /* Long-Float */
        return hashcode_lfloat(obj);
      case ratio_type & ~(bit(number_bit_t)|bit(sign_bit_t)): { /* Ratio */
        /* hash both components, mix */
        var uint32 code1 = hashcode2(TheRatio(obj)->rt_num);
        var uint32 code2 = hashcode2(TheRatio(obj)->rt_den);
        return misch(code1,code2);
      }
      case complex_type & ~(bit(number_bit_t)|bit(sign_bit_t)): { /* Complex */
        /* hash both components, mix */
        var uint32 code1 = hashcode2(TheComplex(obj)->c_real);
        var uint32 code2 = hashcode2(TheComplex(obj)->c_imag);
        return misch(code1,code2);
      }
      default: NOTREACHED;
    }
  }
 #else
  if (orecordp(obj))
    switch (Record_type(obj)) {
      case Rectype_Bignum:
        return hashcode_bignum(obj);
      case Rectype_Ffloat:
        return hashcode_ffloat(obj);
      case Rectype_Dfloat:
        return hashcode_dfloat(obj);
      case Rectype_Lfloat:
        return hashcode_lfloat(obj);
      case Rectype_Ratio: {     /* hash both components, mix */
        var uint32 code1 = hashcode2(TheRatio(obj)->rt_num);
        var uint32 code2 = hashcode2(TheRatio(obj)->rt_den);
        return misch(code1,code2);
      }
      case Rectype_Complex: {   /* hash both components, mix */
        var uint32 code1 = hashcode2(TheComplex(obj)->c_real);
        var uint32 code2 = hashcode2(TheComplex(obj)->c_imag);
        return misch(code1,code2);
      }
      default:
        break;
    }
  else if (immediate_number_p(obj)) {
    if (as_oint(obj) & wbit(4))
      return hashcode_sfloat(obj);
    else
      return hashcode_fixnum(obj);
  }
  return hashcode1stable(obj);
 #endif
}

/* Tests whether hashcode2stable of an object is guaranteed to be
   GC-invariant. */
global bool gcinvariant_hashcode2stable_p (object obj) {
  return numberp(obj)
         || gcinvariant_object_p(obj)
         || instance_of_stablehash_p(obj) || symbolp(obj);
}

/* ---------------------------- FASTHASH EQUAL ---------------------------- */

/* UP: Calculates the FASTHASH-EQUAL-hashcode of an object.
 hashcode3(obj)
 It is valid only until the next GC, or the next modification
 of the object.
 (equal X Y) implies (= (hashcode3 X) (hashcode3 Y)).
 > obj: an object
 < result: hashcode, a 32-Bit-number */
global uint32 hashcode3 (object obj);
/* auxiliary functions for known type:
 String -> length, first max. 31 characters, utilize last character */
local uint32 hashcode_string (object obj) {
  var uintL len;
  var uintL offset;
  var object string = unpack_string_ro(obj,&len,&offset);
  var uint32 bish_code = 0x33DAE11FUL + len; /* utilize length */
  if (len > 0 && !simple_nilarray_p(string)) {
    SstringDispatch(string,X, {
      var const cintX* ptr = &((SstringX)TheVarobject(string))->data[offset];
      var uintC count = len;
      dotimespC(count,count, {
        var uint32 next_code = (uint32)(*ptr++); /* next character */
        bish_code = misch(bish_code,next_code);  /* add */
      });
    });
  }
  return bish_code;
}
/* bit-vector -> length, first 16 bits, utilize last 16 bits */
local uint32 hashcode_bvector (object obj) {
  var uintL len = vector_length(obj); /* length */
  var uintL index = 0;
  var object sbv = array_displace_check(obj,len,&index);
  /* sbv is the data-vector, index is the index into the data-vector. */
  len = len << sbNvector_atype(sbv);
 #if BIG_ENDIAN_P && (varobject_alignment%2 == 0)
  /* On big-endian-machines one can work with with 16 Bit at a time
   (so long as varobject_alignment is divisible by 2 byte): */
  #define bitpack  16
  #define uint_bitpack  uint16
  #define get32bits_at  highlow32_at
 #else
  /* else one can take only 8 bit at a time: */
  #define bitpack  8
  #define uint_bitpack  uint8
  #define get32bits_at(p) \
          (((((((uint32)((p)[0])<<8)|(uint32)((p)[1]))<<8)|(uint32)((p)[2]))<<8)|(uint32)((p)[3]))
 #endif
  var uint_bitpack* ptr =       /* pointer to the first used word */
    (uint_bitpack*)(&TheSbvector(sbv)->data[0]) + floor(index,bitpack);
  var uintL offset = index%bitpack; /* offset within the word */
  if (len <= 32) { /* length <= 32 -> take all bits: */
    if (len == 0) {
      return 0x8FA1D564UL;
    } else { /* 0<len<=32 */
      var uintL need = offset+len; /* need 'need' bits for now */
      /* need < 48 */
      var uint32 akku12 = 0;    /* 48-Bit-Akku, part 1 and 2 */
      var uint32 akku3 = 0;     /* 48-Bit-Akku, part 3 */
     #if (bitpack==16)
      if (need > 0) {
        akku12 = highlow32_0(*ptr++); /* first 16 bits */
        if (need > 16) {
          akku12 |= (uint32)(*ptr++); /* next 16 bits */
          if (need > 32)
            akku3 = (uint32)(*ptr++); /* last 16 bits */
        }
      }
     #endif
     #if (bitpack==8)
      if (need > 0) {
        akku12 = (uint32)(*ptr++)<<24; /* first 8 bits */
        if (need > 8) {
          akku12 |= (uint32)(*ptr++)<<16; /* next 8 bits */
          if (need > 16) {
            akku12 |= (uint32)(*ptr++)<<8; /* next 8 bits */
            if (need > 24) {
              akku12 |= (uint32)(*ptr++); /* next 8 bits */
              if (need > 32) {
                akku3 = (uint32)(*ptr++)<<8; /* next 8 bits */
                if (need > 40)
                  akku3 |= (uint32)(*ptr++); /* last 8 bits */
              }
            }
          }
        }
      }
     #endif
      /* shift 'need' bits in akku12,akku3 by offset bits to the left: */
      akku12 = (akku12 << offset) | (uint32)high16(akku3 << offset);
      /* 32 bits in akku12 finished.
       mask out irrelevant bits: */
      akku12 = akku12 & ~(bit(32-len)-1);
      /* utilize length: */
      return akku12+len;
    }
  } else { /* length > 32 -> take first and last 16 bits: */
    var uint32 akku12 =            /* 32-bit-akku */
      get32bits_at(ptr) << offset; /* contains at least the first 16 bits */
    offset += len;                 /* end-offset of the bitvector */
    ptr += floor(offset,bitpack);  /* points to the last used word */
    offset = offset%bitpack;       /* end-offset within the word */
    var uint32 akku34 =            /* 32-bit-akku */
      get32bits_at(ptr-(16/bitpack)) << offset; /* contains at least the last 16 bits */
    /* reach for the first 16, last 16 bits and utilize length: */
    return highlow32(high16(akku12),high16(akku34)) + len;
  }
  #undef get32bits_at
  #undef uint_bitpack
  #undef bitpack
}
/* EQUALP-hashcode of a pathname-component. */
#ifdef PATHNAME_WIN32
global uint32 hashcode4 (object obj);
#define hashcode_pathcomp(obj)  hashcode4(obj)
#else
#define hashcode_pathcomp(obj)  hashcode3(obj)
#endif
/* atom -> differentiation by type */
local uint32 hashcode3_atom (object obj) {
 #ifdef TYPECODES
  if (symbolp(obj)) {           /* a symbol? */
    return hashcode1(obj);      /* yes -> take EQ-hashcode */
  } else if (numberp(obj)) {    /* a number? */
    return hashcode2(obj);      /* yes -> take EQL-hashcode */
  } else {
    var tint type = typecode(obj) /* typeinfo */
      & ~bit(notsimple_bit_t);    /* if simple or not, is irrelevant */
    if (type >= (sbvector_type & ~bit(notsimple_bit_t)) /* bit/byte-vector ? */
        && type <= (sb32vector_type & ~bit(notsimple_bit_t)))
      return hashcode_bvector(obj); /* look at it component-wise */
    if (type == (sstring_type & ~bit(notsimple_bit_t))) /* string ? */
      return hashcode_string(obj); /* look at it component-wise */
    if (xpathnamep(obj)) { /* -> look at it component-wise: */
      check_SP();
      var uint32 bish_code = 0xB0DD939EUL;
      var const gcv_object_t* ptr = &((Record)ThePathname(obj))->recdata[0];
      var uintC count;
      dotimespC(count,Xrecord_length(obj), {
        var uint32 next_code = hashcode_pathcomp(*ptr++); /* hashcode of the next component */
        bish_code = misch(bish_code,next_code);           /* add */
      });
      return bish_code;
    }
    /* else: take EQ-hashcode (for characters: EQL == EQ) */
    return hashcode1(obj);
  }
 #else
  if (orecordp(obj))
    switch (Record_type(obj)) {
      case_Rectype_number_above;
      case Rectype_Sbvector: case Rectype_bvector:
      case Rectype_Sb2vector: case Rectype_b2vector:
      case Rectype_Sb4vector: case Rectype_b4vector:
      case Rectype_Sb8vector: case Rectype_b8vector:
      case Rectype_Sb16vector: case Rectype_b16vector:
      case Rectype_Sb32vector: case Rectype_b32vector:
        return hashcode_bvector(obj);
      case Rectype_S8string: case Rectype_Imm_S8string:
      case Rectype_S16string: case Rectype_Imm_S16string:
      case Rectype_S32string: case Rectype_Imm_S32string:
      case Rectype_reallocstring: case Rectype_string:
        return hashcode_string(obj);
     #ifdef LOGICAL_PATHNAMES
      case Rectype_Logpathname:
     #endif
      case Rectype_Pathname: { /* pathname -> look at it component-wise: */
        check_SP();
        var uint32 bish_code = 0xB0DD939EUL;
        var gcv_object_t* ptr = &((Record)ThePathname(obj))->recdata[0];
        var uintC count;
        dotimespC(count,Xrecord_length(obj), {
          var uint32 next_code = hashcode_pathcomp(*ptr++); /* hashcode of the next component */
          bish_code = misch(bish_code,next_code);           /* add */
        });
        return bish_code;
      }
      default:
        break;
    }
  else if (immediate_number_p(obj)) {
  case_number: return hashcode2(obj);
  }
  return hashcode1(obj);
 #endif
}
/* cons -> look at content up to depth 4:
 determine the hashcode of the CAR and the hashcode of the CDR at a time
 and combine them shifted. As shifts we can choose e.g. 16,7,5,3, because
 {0,16} + {0,7} + {0,5} + {0,3} = {0,3,5,7,8,10,12,15,16,19,21,23,24,26,28,31}
 consists of 16 different elements of {0,...,31} . */
/* object, at cons only up to depth 0 */
local inline uint32 hashcode3_cons0 (object obj) {
  if (atomp(obj)) {
    return hashcode3_atom(obj);
  } else {                      /* cons -> hashcode := 1 */
    return 1;
  }
}
/* object, at cons only up to depth 1 */
local inline uint32 hashcode3_cons1 (object obj) {
  if (atomp(obj)) {
    return hashcode3_atom(obj);
  } else {
    /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode3_cons0(Car(obj));
    var uint32 code2 = hashcode3_cons0(Cdr(obj));
    return rotate_left(3,code1) ^ code2;
  }
}
/* object, at cons only up to depth 2 */
local inline uint32 hashcode3_cons2 (object obj) {
  if (atomp(obj)) {
    return hashcode3_atom(obj);
  } else {
    /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode3_cons1(Car(obj));
    var uint32 code2 = hashcode3_cons1(Cdr(obj));
    return rotate_left(5,code1) ^ code2;
  }
}
/* object, at cons only up to depth 3 */
local inline uint32 hashcode3_cons3 (object obj) {
  if (atomp(obj)) {
    return hashcode3_atom(obj);
  } else {
    /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode3_cons2(Car(obj));
    var uint32 code2 = hashcode3_cons2(Cdr(obj));
    return rotate_left(7,code1) ^ code2;
  }
}
/* object, at cons only up to depth 4 */
global uint32 hashcode3 (object obj) {
  if (atomp(obj)) {
    return hashcode3_atom(obj);
  } else {
    /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode3_cons3(Car(obj));
    var uint32 code2 = hashcode3_cons3(Cdr(obj));
    return rotate_left(16,code1) ^ code2;
  }
}

/* Tests whether hashcode3 of an object is guaranteed to be GC-invariant. */
global bool gcinvariant_hashcode3_p (object obj);
local bool gcinvariant_hashcode3_atom_p (object obj) {
  if (numberp(obj) || gcinvariant_object_p(obj))
    return true;
  #ifdef TYPECODES
  var tint type = typecode(obj) /* typeinfo */
    & ~bit(notsimple_bit_t);    /* if simple or not, is irrelevant */
  if (type >= (sbvector_type & ~bit(notsimple_bit_t)) /* bit/byte-vector ? */
      && type <= (sb32vector_type & ~bit(notsimple_bit_t)))
    return true;
  if (type == (sstring_type & ~bit(notsimple_bit_t))) /* string ? */
    return true;
  /* Ignore the pathnames, for simplicity. */
  #else
  if (orecordp(obj))
    switch (Record_type(obj)) {
      case Rectype_Sbvector: case Rectype_bvector:
      case Rectype_Sb2vector: case Rectype_b2vector:
      case Rectype_Sb4vector: case Rectype_b4vector:
      case Rectype_Sb8vector: case Rectype_b8vector:
      case Rectype_Sb16vector: case Rectype_b16vector:
      case Rectype_Sb32vector: case Rectype_b32vector:
      case Rectype_S8string: case Rectype_Imm_S8string:
      case Rectype_S16string: case Rectype_Imm_S16string:
      case Rectype_S32string: case Rectype_Imm_S32string:
      case Rectype_reallocstring: case Rectype_string:
        return true;
      /* Ignore the pathnames, for simplicity. */
      default:
        break;
    }
  #endif
  return false;
}
local inline bool gcinvariant_hashcode3_cons0_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode3_atom_p(obj);
  else
    return true;
}
local inline bool gcinvariant_hashcode3_cons1_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode3_atom_p(obj);
  else
    return gcinvariant_hashcode3_cons0_p(Car(obj))
           && gcinvariant_hashcode3_cons0_p(Cdr(obj));
}
local inline bool gcinvariant_hashcode3_cons2_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode3_atom_p(obj);
  else
    return gcinvariant_hashcode3_cons1_p(Car(obj))
           && gcinvariant_hashcode3_cons1_p(Cdr(obj));
}
local inline bool gcinvariant_hashcode3_cons3_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode3_atom_p(obj);
  else
    return gcinvariant_hashcode3_cons2_p(Car(obj))
           && gcinvariant_hashcode3_cons2_p(Cdr(obj));
}
global bool gcinvariant_hashcode3_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode3_atom_p(obj);
  else
    return gcinvariant_hashcode3_cons3_p(Car(obj))
           && gcinvariant_hashcode3_cons3_p(Cdr(obj));
}

/* --------------------------- STABLEHASH EQUAL --------------------------- */

/* UP: Calculates the STABLEHASH-EQUAL-hashcode of an object.
 hashcode3stable(obj)
 It is valid across GC if all cons-tree leaves are instances of
 STANDARD-STABLEHASH, STRUCTURE-STABLEHASH, but no longer than the next
 modification of the object.
 (equal X Y) implies (= (hashcode3stable X) (hashcode3stable Y)).
 > obj: an object
 < result: hashcode, a 32-Bit-number */
global uint32 hashcode3stable (object obj);
/* atom -> differentiation by type */
local uint32 hashcode3stable_atom (object obj) {
 #ifdef TYPECODES
  if (symbolp(obj)) {           /* a symbol? */
    return hashcode1stable(obj); /* yes -> take EQ-hashcode */
  } else if (numberp(obj)) {    /* a number? */
    return hashcode2(obj);      /* yes -> take EQL-hashcode */
  } else {
    var tint type = typecode(obj) /* typeinfo */
      & ~bit(notsimple_bit_t);    /* if simple or not, is irrelevant */
    if (type >= (sbvector_type & ~bit(notsimple_bit_t)) /* bit/byte-vector ? */
        && type <= (sb32vector_type & ~bit(notsimple_bit_t)))
      return hashcode_bvector(obj); /* look at it component-wise */
    if (type == (sstring_type & ~bit(notsimple_bit_t))) /* string ? */
      return hashcode_string(obj); /* look at it component-wise */
    if (xpathnamep(obj)) { /* -> look at it component-wise: */
      check_SP();
      var uint32 bish_code = 0xB0DD939EUL;
      var const gcv_object_t* ptr = &((Record)ThePathname(obj))->recdata[0];
      var uintC count;
      dotimespC(count,Xrecord_length(obj), {
        var uint32 next_code = hashcode_pathcomp(*ptr++); /* hashcode of the next component */
        bish_code = misch(bish_code,next_code);           /* add */
      });
      return bish_code;
    }
    /* else: take EQ-hashcode (for characters: EQL == EQ) */
    return hashcode1stable(obj);
  }
 #else
  if (orecordp(obj))
    switch (Record_type(obj)) {
      case_Rectype_number_above;
      case Rectype_Sbvector: case Rectype_bvector:
      case Rectype_Sb2vector: case Rectype_b2vector:
      case Rectype_Sb4vector: case Rectype_b4vector:
      case Rectype_Sb8vector: case Rectype_b8vector:
      case Rectype_Sb16vector: case Rectype_b16vector:
      case Rectype_Sb32vector: case Rectype_b32vector:
        return hashcode_bvector(obj);
      case Rectype_S8string: case Rectype_Imm_S8string:
      case Rectype_S16string: case Rectype_Imm_S16string:
      case Rectype_S32string: case Rectype_Imm_S32string:
      case Rectype_reallocstring: case Rectype_string:
        return hashcode_string(obj);
     #ifdef LOGICAL_PATHNAMES
      case Rectype_Logpathname:
     #endif
      case Rectype_Pathname: { /* pathname -> look at it component-wise: */
        check_SP();
        var uint32 bish_code = 0xB0DD939EUL;
        var gcv_object_t* ptr = &((Record)ThePathname(obj))->recdata[0];
        var uintC count;
        dotimespC(count,Xrecord_length(obj), {
          var uint32 next_code = hashcode_pathcomp(*ptr++); /* hashcode of the next component */
          bish_code = misch(bish_code,next_code);           /* add */
        });
        return bish_code;
      }
      default:
        break;
    }
  else if (immediate_number_p(obj)) {
  case_number: return hashcode2(obj);
  }
  return hashcode1stable(obj);
 #endif
}
/* cons -> look at content up to depth 4:
 determine the hashcode of the CAR and the hashcode of the CDR at a time
 and combine them shifted. As Shifts fit e.g. 16,7,5,3,
 because {0,16} + {0,7} + {0,5} + {0,3} = {0,3,5,7,8,10,12,15,16,19,21,23,24,26,28,31}
 consists of 16 different elements of {0,...,31} .
 object, at cons only up to depth 0 */
local uint32 hashcode3stable_cons0 (object obj) {
  if (atomp(obj)) {
    return hashcode3stable_atom(obj);
  } else {                      /* cons -> hashcode := 1 */
    return 1;
  }
}
/* object, at cons only up to depth 1 */
local uint32 hashcode3stable_cons1 (object obj) {
  if (atomp(obj)) {
    return hashcode3stable_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode3stable_cons0(Car(obj));
    var uint32 code2 = hashcode3stable_cons0(Cdr(obj));
    return rotate_left(3,code1) ^ code2;
  }
}
/* object, at cons only up to depth 2 */
local uint32 hashcode3stable_cons2 (object obj) {
  if (atomp(obj)) {
    return hashcode3stable_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode3stable_cons1(Car(obj));
    var uint32 code2 = hashcode3stable_cons1(Cdr(obj));
    return rotate_left(5,code1) ^ code2;
  }
}
/* object, at cons only up to depth 3 */
local uint32 hashcode3stable_cons3 (object obj) {
  if (atomp(obj)) {
    return hashcode3stable_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode3stable_cons2(Car(obj));
    var uint32 code2 = hashcode3stable_cons2(Cdr(obj));
    return rotate_left(7,code1) ^ code2;
  }
}
/* object, at cons only up to depth 4 */
global uint32 hashcode3stable (object obj) {
  if (atomp(obj)) {
    return hashcode3stable_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode3stable_cons3(Car(obj));
    var uint32 code2 = hashcode3stable_cons3(Cdr(obj));
    return rotate_left(16,code1) ^ code2;
  }
}

/* Tests whether hashcode3stable of an object is guaranteed to be
   GC-invariant. */
global bool gcinvariant_hashcode3stable_p (object obj);
local bool gcinvariant_hashcode3stable_atom_p (object obj) {
  if (numberp(obj) || gcinvariant_object_p(obj))
    return true;
  #ifdef TYPECODES
  var tint type = typecode(obj) /* typeinfo */
    & ~bit(notsimple_bit_t);    /* if simple or not, is irrelevant */
  if (type >= (sbvector_type & ~bit(notsimple_bit_t)) /* bit/byte-vector ? */
      && type <= (sb32vector_type & ~bit(notsimple_bit_t)))
    return true;
  if (type == (sstring_type & ~bit(notsimple_bit_t))) /* string ? */
    return true;
  /* Ignore the pathnames, for simplicity. */
  #else
  if (orecordp(obj))
    switch (Record_type(obj)) {
      case Rectype_Sbvector: case Rectype_bvector:
      case Rectype_Sb2vector: case Rectype_b2vector:
      case Rectype_Sb4vector: case Rectype_b4vector:
      case Rectype_Sb8vector: case Rectype_b8vector:
      case Rectype_Sb16vector: case Rectype_b16vector:
      case Rectype_Sb32vector: case Rectype_b32vector:
      case Rectype_S8string: case Rectype_Imm_S8string:
      case Rectype_S16string: case Rectype_Imm_S16string:
      case Rectype_S32string: case Rectype_Imm_S32string:
      case Rectype_reallocstring: case Rectype_string:
        return true;
      /* Ignore the pathnames, for simplicity. */
      default:
        break;
    }
  #endif
  return instance_of_stablehash_p(obj) || symbolp(obj);
}
local inline bool gcinvariant_hashcode3stable_cons0_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode3stable_atom_p(obj);
  else
    return true;
}
local inline bool gcinvariant_hashcode3stable_cons1_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode3stable_atom_p(obj);
  else
    return gcinvariant_hashcode3stable_cons0_p(Car(obj))
           && gcinvariant_hashcode3stable_cons0_p(Cdr(obj));
}
local inline bool gcinvariant_hashcode3stable_cons2_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode3stable_atom_p(obj);
  else
    return gcinvariant_hashcode3stable_cons1_p(Car(obj))
           && gcinvariant_hashcode3stable_cons1_p(Cdr(obj));
}
local inline bool gcinvariant_hashcode3stable_cons3_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode3stable_atom_p(obj);
  else
    return gcinvariant_hashcode3stable_cons2_p(Car(obj))
           && gcinvariant_hashcode3stable_cons2_p(Cdr(obj));
}
global bool gcinvariant_hashcode3stable_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode3stable_atom_p(obj);
  else
    return gcinvariant_hashcode3stable_cons3_p(Car(obj))
           && gcinvariant_hashcode3stable_cons3_p(Cdr(obj));
}

/* ---------------------------- FASTHASH EQUALP ---------------------------- */

/* UP: Calculates the EQUALP-hashcode of an object.
 hashcode4(obj)
 Is is valid only until the next GC or the next modification
 of the object.
 (equalp X Y) implies (= (hashcode4 X) (hashcode4 Y)). */
global uint32 hashcode4 (object obj);
/* auxiliary functions for known type:
 character -> case-insensitive. */
#define hashcode4_char(c)  (0xCAAEACEFUL + (uint32)as_cint(up_case(c)))
/* number: mixture of exponent, length, first 32 bit */
extern uint32 hashcode4_real (object obj); /* see REALELEM.D */
extern uint32 hashcode4_uint32 (uint32 x); /* see REALELEM.D */
extern uint32 hashcode4_uint4 [16];        /* see REALELEM.D */
/* vectors: look at them component-wise */
local uint32 hashcode4_vector_T (object dv, uintL index,
                                 uintL count, uint32 bish_code);
local uint32 hashcode4_vector_Char (object dv, uintL index,
                                    uintL count, uint32 bish_code);
local uint32 hashcode4_vector_Bit (object dv, uintL index,
                                   uintL count, uint32 bish_code);
local uint32 hashcode4_vector_2Bit (object dv, uintL index,
                                    uintL count, uint32 bish_code);
local uint32 hashcode4_vector_4Bit (object dv, uintL index,
                                    uintL count, uint32 bish_code);
local uint32 hashcode4_vector_8Bit (object dv, uintL index,
                                    uintL count, uint32 bish_code);
local uint32 hashcode4_vector_16Bit (object dv, uintL index,
                                     uintL count, uint32 bish_code);
local uint32 hashcode4_vector_32Bit (object dv, uintL index,
                                     uintL count, uint32 bish_code);
local uint32 hashcode4_vector (object dv, uintL index,
                               uintL count, uint32 bish_code);
local uint32 hashcode4_vector_T (object dv, uintL index,
                                 uintL count, uint32 bish_code) {
  if (count > 0) {
    check_SP();
    var const gcv_object_t* ptr = &TheSvector(dv)->data[index];
    dotimespL(count,count, {
      var uint32 next_code = hashcode4(*ptr++); /* next component's hashcode */
      bish_code = misch(bish_code,next_code);   /* add */
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_Char (object dv, uintL index,
                                    uintL count, uint32 bish_code) {
  if (count > 0) {
    SstringDispatch(dv,X, {
      var const cintX* ptr = &((SstringX)TheVarobject(dv))->data[index];
      dotimespL(count,count, {
        var uint32 next_code = hashcode4_char(as_chart(*ptr++)); /*next char*/
        bish_code = misch(bish_code,next_code); /* add */
      });
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_Bit (object dv, uintL index,
                                   uintL count, uint32 bish_code) {
  if (count > 0) {
    var const uintB* ptr = &TheSbvector(dv)->data[index/8];
    dotimespL(count,count, {
      var uint32 next_code =
        hashcode4_uint4[(*ptr >> ((~index)%8)) & (bit(1)-1)]; /* next byte */
      bish_code = misch(bish_code,next_code);                 /* add */
      index++;
      ptr += ((index%8)==0);
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_2Bit (object dv, uintL index,
                                    uintL count, uint32 bish_code) {
  if (count > 0) {
    var const uintB* ptr = &TheSbvector(dv)->data[index/4];
    dotimespL(count,count, {
      var uint32 next_code =
        hashcode4_uint4[(*ptr >> ((~index)%4)) & (bit(2)-1)]; /* next byte */
      bish_code = misch(bish_code,next_code);                 /* add */
      index++;
      ptr += ((index%4)==0);
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_4Bit (object dv, uintL index,
                                    uintL count, uint32 bish_code) {
  if (count > 0) {
    var const uintB* ptr = &TheSbvector(dv)->data[index/2];
    dotimespL(count,count, {
      var uint32 next_code =
        hashcode4_uint4[(*ptr >> ((~index)%2)) & (bit(4)-1)]; /* next byte */
      bish_code = misch(bish_code,next_code);                 /* add */
      index++;
      ptr += ((index%2)==0);
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_8Bit (object dv, uintL index,
                                    uintL count, uint32 bish_code) {
  if (count > 0) {
    var const uintB* ptr = &TheSbvector(dv)->data[index];
    dotimespL(count,count, {
      var uint32 next_code = hashcode4_uint32(*ptr++); /* next byte */
      bish_code = misch(bish_code,next_code);          /* add */
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_16Bit (object dv, uintL index,
                                     uintL count, uint32 bish_code) {
  if (count > 0) {
    var const uint16* ptr = &((uint16*)&TheSbvector(dv)->data[0])[index];
    dotimespL(count,count, {
      var uint32 next_code = hashcode4_uint32(*ptr++); /* next byte */
      bish_code = misch(bish_code,next_code);          /* add */
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector_32Bit (object dv, uintL index,
                                     uintL count, uint32 bish_code) {
  if (count > 0) {
    var const uint32* ptr = &((uint32*)&TheSbvector(dv)->data[0])[index];
    dotimespL(count,count, {
      var uint32 next_code = hashcode4_uint32(*ptr++); /* next byte */
      bish_code = misch(bish_code,next_code);          /* add */
    });
  }
  return bish_code;
}
local uint32 hashcode4_vector (object dv, uintL index,
                               uintL count, uint32 bish_code) {
  switch (Array_type(dv)) {
    case Array_type_svector:    /* simple-vector */
      return hashcode4_vector_T(dv,index,count,bish_code);
    case Array_type_sbvector:   /* simple-bit-vector */
      return hashcode4_vector_Bit(dv,index,count,bish_code);
    case Array_type_sb2vector:
      return hashcode4_vector_2Bit(dv,index,count,bish_code);
    case Array_type_sb4vector:
      return hashcode4_vector_4Bit(dv,index,count,bish_code);
    case Array_type_sb8vector:
      return hashcode4_vector_8Bit(dv,index,count,bish_code);
    case Array_type_sb16vector:
      return hashcode4_vector_16Bit(dv,index,count,bish_code);
    case Array_type_sb32vector:
      return hashcode4_vector_32Bit(dv,index,count,bish_code);
    case Array_type_snilvector: /* (VECTOR NIL) */
      if (count > 0)
        return 0x2116ECD0 + bish_code;
      /*FALLTHROUGH*/
    case Array_type_sstring:    /* simple-string */
      return hashcode4_vector_Char(dv,index,count,bish_code);
    default: NOTREACHED;
  }
}
/* atom -> differentiation by type */
local uint32 hashcode4_atom (object obj) {
 #ifdef TYPECODES
  if (symbolp(obj)) {           /* a symbol? */
    return hashcode1(obj);      /* yes -> take EQ-hashcode */
  } else if (numberp(obj)) {    /* a number? */
    /* yes -> take EQUALP-hashcode */
    if (complexp(obj)) {
      var uint32 code1 = hashcode4_real(TheComplex(obj)->c_real);
      var uint32 code2 = hashcode4_real(TheComplex(obj)->c_imag);
      /* important for combining, because of "complex canonicalization":
         if imagpart=0.0, then hashcode = hashcode4_real(realpart). */
      return code1 ^ rotate_left(5,code2);
    } else {
      return hashcode4_real(obj);
    }
  } else
    switch (typecode(obj))
 #else
  if (orecordp(obj)) {
    if (Record_type(obj) < rectype_longlimit)
      goto case_orecord;
    else
      goto case_lrecord;
  } else if (immediate_number_p(obj)) {
   case_real: return hashcode4_real(obj);
  } else if (charp(obj))
    goto case_char;
  else
    return hashcode1(obj);
  switch (0)
 #endif
  {
    case_bvector:               /* bit-vector */
    case_b2vector:              /* 2bit-vector */
    case_b4vector:              /* 4bit-vector */
    case_b8vector:              /* 8bit-vector */
    case_b16vector:             /* 16bit-vector */
    case_b32vector:             /* 32bit-vector */
    case_string:                /* string */
    case_vector: {              /* (VECTOR T), (VECTOR NIL) */
      /* look at it component-wise: */
      var uintL len = vector_length(obj); /* length */
      var uintL index = 0;
      var object dv = array_displace_check(obj,len,&index);
      /* dv is the data-vector, index is the index into the data-vector. */
      var uint32 bish_code = 0x724BD24EUL + len; /* utilize length */
      return hashcode4_vector(dv,index,len,bish_code);
    }
    case_mdarray: {             /* array with rank /=1 */
      /* rank and dimensions, then look at it component-wise: */
      var uint32 bish_code = 0xF1C90A73UL;
      {
        var uintC rank = Iarray_rank(obj);
        if (rank > 0) {
          var uintL* dimptr = &TheIarray(obj)->dims[0];
          if (Iarray_flags(obj) & bit(arrayflags_dispoffset_bit))
            dimptr++;
          dotimespC(rank,rank, {
            var uint32 next_code = (uint32)(*dimptr++);
            bish_code = misch(bish_code,next_code);
          });
        }
      }
      {
        var uintL len = TheIarray(obj)->totalsize;
        var uintL index = 0;
        var object dv = iarray_displace_check(obj,len,&index);
        return hashcode4_vector(dv,index,len,bish_code);
      }
    }
   #ifdef TYPECODES
    _case_structure
    _case_stream
   #endif
    case_orecord:
      switch (Record_type(obj)) {
        case_Rectype_bvector_above;
        case_Rectype_b2vector_above;
        case_Rectype_b4vector_above;
        case_Rectype_b8vector_above;
        case_Rectype_b16vector_above;
        case_Rectype_b32vector_above;
        case_Rectype_string_above;
        case_Rectype_vector_above;
        case_Rectype_mdarray_above;
        case_Rectype_Closure_above;
        case_Rectype_Instance_above;
       #ifndef TYPECODES
        case_Rectype_Symbol_above;
        case Rectype_Ratio:
        case Rectype_Ffloat: case Rectype_Dfloat: case Rectype_Lfloat:
        case Rectype_Bignum:
          goto case_real;
        case Rectype_Complex: {
          var uint32 code1 = hashcode4_real(TheComplex(obj)->c_real);
          var uint32 code2 = hashcode4_real(TheComplex(obj)->c_imag);
          /* important for combining, because of "complex canonicalization":
             if imagpart=0.0, then hashcode = hashcode4_real(realpart). */
          return code1 ^ rotate_left(5,code2);
        }
       #endif
        default: ;
      }
    /* FIXME: The case that obj is a hash-table should be handled specially. */
    {                           /* look at flags, type, components: */
      var uintC len = SXrecord_nonweak_length(obj);
      var uint32 bish_code =
        0x03168B8D + (Record_flags(obj) << 24) + (Record_type(obj) << 16) + len;
      if (len > 0) {
        check_SP();
        var const gcv_object_t* ptr = &TheRecord(obj)->recdata[0];
        var uintC count;
        dotimespC(count,len, {
          var uint32 next_code = hashcode4(*ptr++); /* next component's hashcode */
          bish_code = misch(bish_code,next_code);   /* add */
        });
      }
      if (Record_type(obj) >= rectype_limit) {
        var uintC xlen = Xrecord_xlength(obj);
        if (xlen > 0) {
          var const uintB* ptr = (uintB*)&TheRecord(obj)->recdata[len];
          dotimespC(xlen,xlen, {
            var uint32 next_code = *ptr++;          /* next byte */
            bish_code = misch(bish_code,next_code); /* add */
          });
        }
      }
      return bish_code;
    }
    case_char:                  /* character */
      return hashcode4_char(char_code(obj));
   #ifdef TYPECODES
    case_machine:               /* machine */
    case_subr:                  /* subr */
    case_system:                /* frame-pointer, small-read-label, system */
   #else
    case_symbol:                /* symbol */
   #endif
    case_closure:               /* closure */
    case_instance:              /* instance */
    case_lrecord:
      /* take EQ-hashcode */
      return hashcode1(obj);
    default: NOTREACHED;
  }
}
/* cons -> look at content up to depth 4:
 determine hashcode of the CAR and hashcode of the CDR at a time
 and combine them shifted. As shifts fit e.g. 16,7,5,3,
 because {0,16} + {0,7} + {0,5} + {0,3} =
         {0,3,5,7,8,10,12,15,16,19,21,23,24,26,28,31}
 consists of 16 different elements of {0,...,31} .
 object, at cons only up to depth 0 */
local uint32 hashcode4_cons0 (object obj) {
  if (atomp(obj)) {
    return hashcode4_atom(obj);
  } else {                      /* cons -> hashcode := 1 */
    return 1;
  }
}
/* object, at cons only up to depth 1 */
local uint32 hashcode4_cons1 (object obj) {
  if (atomp(obj)) {
    return hashcode4_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode4_cons0(Car(obj));
    var uint32 code2 = hashcode4_cons0(Cdr(obj));
    return rotate_left(3,code1) ^ code2;
  }
}
/* object, at cons only up to depth 2 */
local uint32 hashcode4_cons2 (object obj) {
  if (atomp(obj)) {
    return hashcode4_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode4_cons1(Car(obj));
    var uint32 code2 = hashcode4_cons1(Cdr(obj));
    return rotate_left(5,code1) ^ code2;
  }
}
/* object, at cons only up to depth 3 */
local uint32 hashcode4_cons3 (object obj) {
  if (atomp(obj)) {
    return hashcode4_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode4_cons2(Car(obj));
    var uint32 code2 = hashcode4_cons2(Cdr(obj));
    return rotate_left(7,code1) ^ code2;
  }
}
/* object, at cons only up to depth 4 */
global uint32 hashcode4 (object obj) {
  if (atomp(obj)) {
    return hashcode4_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = hashcode4_cons3(Car(obj));
    var uint32 code2 = hashcode4_cons3(Cdr(obj));
    return rotate_left(16,code1) ^ code2;
  }
}

/* Tests whether hashcode4 of an object is guaranteed to be GC-invariant. */
global bool gcinvariant_hashcode4_p (object obj);
local bool gcinvariant_hashcode4_atom_p (object obj) {
  if (numberp(obj) || gcinvariant_object_p(obj))
    return true;
  #ifdef TYPECODES
  var tint type = typecode(obj) /* typeinfo */
    & ~bit(notsimple_bit_t);    /* if simple or not, is irrelevant */
  if (type >= (sbvector_type & ~bit(notsimple_bit_t)) /* bit/byte-vector ? */
      && type <= (sb32vector_type & ~bit(notsimple_bit_t)))
    return true;
  if (type == (sstring_type & ~bit(notsimple_bit_t))) /* string ? */
    return true;
  /* Ignore other types of arrays and records, for simplicity. */
  #else
  if (orecordp(obj))
    switch (Record_type(obj)) {
      case Rectype_Sbvector: case Rectype_bvector:
      case Rectype_Sb2vector: case Rectype_b2vector:
      case Rectype_Sb4vector: case Rectype_b4vector:
      case Rectype_Sb8vector: case Rectype_b8vector:
      case Rectype_Sb16vector: case Rectype_b16vector:
      case Rectype_Sb32vector: case Rectype_b32vector:
      case Rectype_S8string: case Rectype_Imm_S8string:
      case Rectype_S16string: case Rectype_Imm_S16string:
      case Rectype_S32string: case Rectype_Imm_S32string:
      case Rectype_reallocstring: case Rectype_string:
        return true;
      /* Ignore other types of arrays and records, for simplicity. */
      default:
        break;
    }
  #endif
  return false;
}
local inline bool gcinvariant_hashcode4_cons0_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode4_atom_p(obj);
  else
    return true;
}
local inline bool gcinvariant_hashcode4_cons1_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode4_atom_p(obj);
  else
    return gcinvariant_hashcode4_cons0_p(Car(obj))
           && gcinvariant_hashcode4_cons0_p(Cdr(obj));
}
local inline bool gcinvariant_hashcode4_cons2_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode4_atom_p(obj);
  else
    return gcinvariant_hashcode4_cons1_p(Car(obj))
           && gcinvariant_hashcode4_cons1_p(Cdr(obj));
}
local inline bool gcinvariant_hashcode4_cons3_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode4_atom_p(obj);
  else
    return gcinvariant_hashcode4_cons2_p(Car(obj))
           && gcinvariant_hashcode4_cons2_p(Cdr(obj));
}
global bool gcinvariant_hashcode4_p (object obj) {
  if (atomp(obj))
    return gcinvariant_hashcode4_atom_p(obj);
  else
    return gcinvariant_hashcode4_cons3_p(Car(obj))
           && gcinvariant_hashcode4_cons3_p(Cdr(obj));
}

/* ----------------------------- USER DEFINED ----------------------------- */

/* hashcode for user-defined ht_test */
local uint32 hashcode_raw_user (object fun, object obj) {
  pushSTACK(obj); funcall(fun,1);
  value1 = check_uint32(value1);
  return I_to_UL(value1);
}

/* =========================== Hash table record =========================== */

# Specification of the flags in a hash-table:
  #define htflags_test_builtin_B  (bit(1)|bit(0)) # for distinguishing builtin tests
  #define htflags_test_eq_B       (    0 |    0 ) # test is EQ
  #define htflags_test_eql_B      (    0 |bit(0)) # test is EQL
  #define htflags_test_equal_B    (bit(1)|    0 ) # test is EQUAL
  #define htflags_test_equalp_B   (bit(1)|bit(0)) # test is EQUALP
  #define htflags_test_user_B     bit(2) # set for user-defined test
  #define htflags_stablehash_B    bit(3) # hash code of instances of
                                         # STANDARD-STABLEHASH, STRUCTURE-STABLEHASH
                                         # is GC-invariant
  #define htflags_pending_warn_forced_gc_rehash bit(4) # Must call
                                         # warn_forced_gc_rehash at the next
                                         # opportunity
  # define htflags_warn_gc_rehash_B bit(5) # Warn when a key is being added
                                         # whose hash code is not GC-invariant.
  # define htflags_gc_rehash_B    bit(6) # Set after a key has been added
                                         # whose hash code is not GC-invariant.
  # define htflags_invalid_B      bit(7) # Set when the list structure is
                                         # invalid and the table needs a rehash.

# Specification of the two types of Pseudo-Functions:

  # Specification for LOOKUP - Pseudo-Function:
  # lookup(ht,obj,allowgc,&KVptr,&Iptr)
  # > ht: hash-table
  # > obj: object
  # > allowgc: whether GC is allowed during hash lookup
  # < if found: result=true,
  #     KVptr[0], KVptr[1] : key, value in key-value-vector,
  #     KVptr[2] : index of next entry,
  #     *Iptr : previous index pointing to KVptr[0..2]
  # < if not found: result=false,
  #     *Iptr : entry belonging to key in index-vector
  #             or an arbitrary element of the "list" starting there
  # can trigger GC - if allowgc is true
    typedef maygc bool (* lookup_Pseudofun) (object ht, object obj, bool allowgc, gcv_object_t** KVptr_, gcv_object_t** Iptr_);

  # Specification for HASHCODE - Pseudo-Function:
  # hashcode(obj)
  # > obj: object
  # < result: its hash code
    typedef uint32 (* hashcode_Pseudofun) (object obj);

  # Specification for TEST - Pseudo-Function:
  # test(obj1,obj2)
  # > obj1: object
  # > obj2: object
  # < result: true if they are considered equal
    typedef bool (* test_Pseudofun) (object obj1, object obj2);

  # Specification for GCINVARIANT - Pseudo-Function:
  # gcinvariant(obj)
  # > obj: object
  # < result: true if its hash code is guaranteed to be GC-invariant
    typedef bool (* gcinvariant_Pseudofun) (object obj);

# Extract Pseudo-Functions of a hash-table:
#define lookupfn(ht)  \
  (*(lookup_Pseudofun)ThePseudofun(TheHashtable(ht)->ht_lookupfn))
#define hashcodefn(ht)  \
  (*(hashcode_Pseudofun)ThePseudofun(TheHashtable(ht)->ht_hashcodefn))
#define testfn(ht)  \
  (*(test_Pseudofun)ThePseudofun(TheHashtable(ht)->ht_testfn))
#define gcinvariantfn(ht)  \
  (*(gcinvariant_Pseudofun)ThePseudofun(TheHashtable(ht)->ht_gcinvariantfn))

/* UP: Calculates the hashcode of an object with reference to a hashtable.
 hashcode(ht,obj)
 > ht: hash-table
 > obj: object
 < result: index into the index-vector
 can trigger GC - for user-defined ht_test */
local inline /*maygc*/ uintL hashcode_raw (object ht, object obj) {
  var uintB flags = record_flags(TheHashtable(ht));
  GCTRIGGER_IF(flags & htflags_test_user_B, GCTRIGGER2(ht,obj));
  return (flags & (htflags_test_builtin_B | htflags_stablehash_B)
          ? hashcodefn(ht)(obj) /* General built-in hash code */
          : !(flags & htflags_test_user_B)
            ? hashcode1(obj) /* FASTHASH-EQ hashcode */
            : hashcode_raw_user(TheHashtable(ht)->ht_hash,obj));
}
local inline uintL hashcode_cook (uint32 code, uintL size) {
  /* divide raw hashcode CODE by SIZE: */
  var uint32 rest;
  divu_3232_3232(code,size,(void),rest=);
  return rest;
}
local uintL hashcode (object ht, object obj) {
  var uintL size = TheHashtable(ht)->ht_size;
  return hashcode_cook(hashcode_raw(ht,obj),size);
}

/* UP: Calculates the hashcode of an object with reference to a hashtable.
 hashcode_builtin(ht,obj)
 > ht: hash-table with built-in test
 > obj: object
 < result: index into the index-vector */
local inline uintL hashcode_builtin (object ht, object obj) {
  var uintL size = TheHashtable(ht)->ht_size;
  var uintB flags = record_flags(TheHashtable(ht));
  var uint32 coderaw =
    (flags & (htflags_test_builtin_B | htflags_stablehash_B)
     ? hashcodefn(ht)(obj) /* General built-in hash code */
     : hashcode1(obj)); /* FASTHASH-EQ hashcode */
  return hashcode_cook(coderaw,size);
}

/* UP: Calculates the hashcode of an object with reference to a hashtable.
 hashcode_user(ht,obj)
 > ht: hash-table with user-defined test
 > obj: object
 < result: index into the index-vector
 can trigger GC */
local maygc uintL hashcode_user (object ht, object obj) {
  var uintL size = TheHashtable(ht)->ht_size;
  var uint32 coderaw = hashcode_raw_user(TheHashtable(ht)->ht_hash,obj);
  return hashcode_cook(coderaw,size);
}

/* UP: Reorganizes a hash-table, after the hashcodes of the keys
 have been modified by a GC.
 rehash(ht);
 > ht: hash-table
 can trigger GC - for user-defined ht_test */
local /*maygc*/ object rehash (object ht) {
  GCTRIGGER_IF(record_flags(TheHashtable(ht)) & htflags_test_user_B,
               GCTRIGGER1(ht));
  /* fill index-vector with "nix" : */
  var object kvtable = TheHashtable(ht)->ht_kvtable;
  var object Ivektor = TheHashedAlist(kvtable)->hal_itable; /* index-vector */
  {
    var gcv_object_t* ptr = &TheSvector(Ivektor)->data[0];
    var uintL count = TheHashtable(ht)->ht_size; /* SIZE, >0 */
    dotimespL(count,count, { *ptr++ = nix; } );
  }
  /* build up "list"-structure element-wise: */
  var object index = TheHashtable(ht)->ht_maxcount; /* MAXCOUNT */
  var uintL maxcount = posfixnum_to_V(index);
  var gcv_object_t* KVptr = &TheHashedAlist(kvtable)->hal_data[3*maxcount]; /* end of kvtable */
  var object freelist = nix;
  var object count = Fixnum_0;
  var bool user_defined_p =
    ht_test_code_user_p(ht_test_code(record_flags(TheHashtable(ht))));
  while (!eq(index,Fixnum_0)) { /* index=0 -> loop finished */
    /* traverse the key-value-vector and the next-vector.
       index = MAXCOUNT,...,0 (Fixnum),
       KVptr = &TheHashedAlist(kvtable)->hal_data[3*index],
       freelist = freelist up to now,
       count = pair-counter as fixnum. */
    index = fixnum_inc(index,-1); /* decrement index */
    KVptr -= 3;
    var object key = KVptr[0];  /* next key */
    if (!eq(key,leer)) {                 /* /= "leer" ? */
      if (user_defined_p)
        pushSTACK(ht); /* save */
      var uintL hashindex = hashcode(ht,key); /* its hashcode */
      if (user_defined_p) { /* restore - don't have to restore fixnums! */
        /* this implementation favors built-in ht-tests at the expense
           of the user-defined ones */
        ht = popSTACK();
        kvtable = TheHashtable(ht)->ht_kvtable;
        Ivektor = TheHashedAlist(kvtable)->hal_itable;
        KVptr = &TheHashedAlist(kvtable)->hal_data[3*posfixnum_to_V(index)];
      }
      /* "list", that starts at entry hashindex, in order to extend index:
       copy entry from index-vector to the next-vector
       end replace with index (a pointer to this location) : */
      var gcv_object_t* Iptr = &TheSvector(Ivektor)->data[hashindex];
      KVptr[2] = *Iptr;            /* copy entry into the next-vector */
      *Iptr = index;               /* and replace pointer to it */
      count = fixnum_inc(count,1); /* count */
    } else {                 /* lengthen freelist in the next-vector: */
      KVptr[2] = freelist; freelist = index;
    }
  }
  TheHashedAlist(kvtable)->hal_freelist = freelist; /* save freelist */
  TheHashedAlist(kvtable)->hal_count = count; /* save number of pairs for consistency */
  set_ht_valid(TheHashtable(ht)); /* hashtable is now completely organized */
  return ht;
}

/* Warn if a hash table is rehashed because of a GC, degrading performance.
 can trigger GC */
local maygc void warn_forced_gc_rehash (object ht) {
  pushSTACK(NIL); pushSTACK(ht);
  STACK_1 = CLSTEXT("Performance/scalability warning: The hash table ~S needs "
                    "to be rehashed after a garbage collection, since it "
                    "contains key whose hash code is not GC-invariant.");
  funcall(S(warn),2);
}

/* UP: Searches a key in a hash-table.
 hash_lookup_builtin(ht,obj,allowgc,&KVptr,&Iptr)
 > ht: hash-table
 > obj: object
 > allowgc: whether GC is allowed during hash lookup
 < if found: result=true,
     KVptr[0], KVptr[1] : key, value in key-value-vector,
     KVptr[2] : index of next entry,
     *Iptr : previous index pointing to KVptr[0..2]
 < if not found: result=false,
     *Iptr : entry belonging to key in index-vector
             or an arbitrary element of the "list" starting there
 can trigger GC - if allowgc is true */
global /*maygc*/ bool hash_lookup_builtin (object ht, object obj, bool allowgc,
                                           gcv_object_t** KVptr_, gcv_object_t** Iptr_) {
  GCTRIGGER_IF(allowgc, GCTRIGGER2(ht,obj));
  #ifdef GENERATIONAL_GC
  if (!ht_validp(TheHashtable(ht))) { /* hash-table must be reorganized? */
    # Rehash it before the warning, otherwise we risk an endless recursion.
    ht = rehash(ht);
    # Warn if *WARN-ON-HASHTABLE-NEEDING-REHASH-AFTER-GC* is true:
    if (!nullpSv(warn_on_hashtable_needing_rehash_after_gc)) {
      if (allowgc) {
        record_flags_clr(TheHashtable(ht),htflags_pending_warn_forced_gc_rehash);
        pushSTACK(ht); pushSTACK(obj);
        warn_forced_gc_rehash(ht);
        obj = popSTACK(); ht = popSTACK();
        if (!ht_validp(TheHashtable(ht))) /* must be reorganized again? */
          ht = rehash(ht);
      } else {
        # We cannot warn now, because in this call we are not allowed to
        # trigger GC, therefore we delay the call until the next opportunity.
        record_flags_set(TheHashtable(ht),htflags_pending_warn_forced_gc_rehash);
      }
    }
  }
  #endif
  if (allowgc
      && (record_flags(TheHashtable(ht)) & htflags_pending_warn_forced_gc_rehash)) {
    # Now is an opportunity to get rid of the pending warn_forced_gc_rehash task.
    record_flags_clr(TheHashtable(ht),htflags_pending_warn_forced_gc_rehash);
    pushSTACK(ht); pushSTACK(obj);
    warn_forced_gc_rehash(ht);
    obj = popSTACK(); ht = popSTACK();
    if (!ht_validp(TheHashtable(ht))) /* must be reorganized again? */
      ht = rehash(ht);
  }
  ASSERT(ht_validp(TheHashtable(ht)));
  var uintB flags = record_flags(TheHashtable(ht));
  var uintL hashindex = hashcode_builtin(ht,obj); /* calculate hashcode */
  var object kvtable = TheHashtable(ht)->ht_kvtable;
  var gcv_object_t* Nptr =      /* pointer to the current entry */
    &TheSvector(TheHashedAlist(kvtable)->hal_itable)->data[hashindex];
  var gcv_object_t* kvt_data = TheHashedAlist(kvtable)->hal_data;
  while (!eq(*Nptr,nix)) { /* track "list" : "list" finished -> not found */
    var uintL index = posfixnum_to_V(*Nptr); /* next index */
    var gcv_object_t* Iptr = Nptr;
    var gcv_object_t* KVptr = /* pointer to entries in key-value-vector */
      kvt_data + 3*index;
    var object key = KVptr[0];
    /* compare key with obj: */
    if ((flags & htflags_test_builtin_B) == htflags_test_eq_B
        ? eq(key,obj) /* compare with EQ */
        : testfn(ht)(key,obj)) {
      /* object obj found */
      *KVptr_ = KVptr; *Iptr_ = Iptr; return true;
    }
    Nptr = &KVptr[2];         /* pointer to index of next entry */
  }
  /* not found */
  *Iptr_ = Nptr; return false;
}
#ifndef GENERATIONAL_GC
/* can trigger GC - if allowgc is true */
global /*maygc*/ bool hash_lookup_builtin_with_rehash (object ht, object obj, bool allowgc,
                                                       gcv_object_t** KVptr_, gcv_object_t** Iptr_) {
  GCTRIGGER_IF(allowgc, GCTRIGGER2(ht,obj));
  if (!ht_validp(TheHashtable(ht))) { /* hash-table must be reorganized? */
    # Rehash it before the warning, otherwise we risk an endless recursion.
    ht = rehash(ht);
    # Warn if *WARN-ON-HASHTABLE-NEEDING-REHASH-AFTER-GC* is true:
    if (!nullpSv(warn_on_hashtable_needing_rehash_after_gc)) {
      if (allowgc) {
        record_flags_clr(TheHashtable(ht),htflags_pending_warn_forced_gc_rehash);
        pushSTACK(ht); pushSTACK(obj);
        warn_forced_gc_rehash(ht);
        obj = popSTACK(); ht = popSTACK();
        if (!ht_validp(TheHashtable(ht))) /* must be reorganized again? */
          ht = rehash(ht);
      } else {
        # We cannot warn now, because in this call we are not allowed to
        # trigger GC, therefore we delay the call until the next opportunity.
        record_flags_set(TheHashtable(ht),htflags_pending_warn_forced_gc_rehash);
      }
    }
  }
  return hash_lookup_builtin(ht,obj,allowgc,KVptr_,Iptr_);
}
#endif

/* UP: Searches a key in a hash-table with user-defined test.
 hash_lookup_user(ht,obj,allowgc,&KVptr,&Iptr)
 > ht: hash-table
 > obj: object
 > allowgc: whether GC is allowed during hash lookup
 < if found: result=true,
     KVptr[0], KVptr[1] : key, value in key-value-vector,
     KVptr[2] : index of next entry,
     *Iptr : previous index pointing to KVptr[0..2]
 < if not found: result=false,
     *Iptr : entry belonging to key in index-vector
             or an arbitrary element of the "list" starting there
 can trigger GC - if allowgc is true */
global maygc bool hash_lookup_user (object ht, object obj, bool allowgc,
                                    gcv_object_t** KVptr_, gcv_object_t** Iptr_) {
  ASSERT(allowgc);
  pushSTACK(ht); pushSTACK(obj);
  if (!ht_validp(TheHashtable(ht))) /* hash-table must be reorganized */
    ht = rehash(ht);
  obj = STACK_0; /* rehash could trigger GC */
  var uintL hashindex = hashcode_user(ht,obj); /* calculate hashcode */
  obj = popSTACK(); ht = popSTACK();
  var object kvtable = TheHashtable(ht)->ht_kvtable;
  var gcv_object_t* Nptr =      /* pointer to the current entry */
    &TheSvector(TheHashedAlist(kvtable)->hal_itable)->data[hashindex];
  var gcv_object_t* kvt_data = TheHashedAlist(kvtable)->hal_data;
  var uintL i_n; /* Iptr-Nptr FIXME: This is not GC-safe */
  while (!eq(*Nptr,nix)) { /* track "list" : "list" finished -> not found */
    var uintL index = posfixnum_to_V(*Nptr); /* next index */
    var gcv_object_t* Iptr = Nptr;
    var gcv_object_t* KVptr = /* pointer to entries in key-value-vector */
      kvt_data + 3*index;
    Nptr = &KVptr[2];         /* pointer to index of next entry */
    /* compare key with obj: */
    pushSTACK(ht); pushSTACK(obj);
    i_n = Iptr - Nptr;
    pushSTACK(KVptr[0]); pushSTACK(obj); funcall(TheHashtable(ht)->ht_test,2);
    obj = popSTACK(); ht = popSTACK();
    kvtable = TheHashtable(ht)->ht_kvtable;
    kvt_data = TheHashedAlist(kvtable)->hal_data;
    KVptr = kvt_data + 3*index; Nptr = &KVptr[2];
    Iptr = Nptr + i_n;
    if (!nullp(value1)) {
      /* object obj found */
      *KVptr_ = KVptr; *Iptr_ = Iptr; return true;
    }
  }
  /* not found */
  *Iptr_ = Nptr; return false;
}

/* UP: Searches a key in a hash-table.
 hash_lookup(ht,obj,allowgc,&KVptr,&Iptr)
 > ht: hash-table
 > obj: object
 > allowgc: whether GC is allowed during hash lookup
 < if found: result=true,
     KVptr[0], KVptr[1] : key, value in key-value-vector,
     KVptr[2] : index of next entry,
     *Iptr : previous index pointing to KVptr[0..2]
 < if not found: result=false,
     *Iptr : entry belonging to key in index-vector
             or an arbitrary element of the "list" starting there
 can trigger GC - if allowgc is true */
#define hash_lookup(ht,obj,allowgc,KVptr_,Iptr_)  \
  lookupfn(ht)(ht,obj,allowgc,KVptr_,Iptr_)

/* UP: Tests whether the hash code of a given key in a hash table is stable
   i.e. gc-invariant, or not.
 > ht: hash-table
 > obj: object
 < result: true if the key's hash code is gc-invariant */
local inline bool hashcode_gc_invariant_p (object ht, object obj) {
  return gcinvariantfn(ht)(obj);
}

/* Warn if adding an key to a hash table degrades its performance.
 can trigger GC */
local maygc void warn_key_forces_gc_rehash (object ht, object key) {
  pushSTACK(NIL); pushSTACK(ht); pushSTACK(key);
  STACK_2 = CLSTEXT("Performance/scalability warning: The hash table ~S must "
                    "be rehashed after each garbage collection, since its "
                    "key ~S has a hash code that is not GC-invariant.");
  funcall(S(warn),3);
}

/* Macro: Insers a key-value-pair into a hash-table.
 hash_store(key,value);
 > object ht: hash-table
 > object freelist: Start of the free-list in next-vector, /= nix
 > key: key
 > value: value
 > gcv_object_t* Iptr: arbitrary element of the "list", that belongs to key
 can trigger GC */
#define hash_store(key,value)                                           \
  do {                                                                  \
    var uintL index = posfixnum_to_V(freelist);    /* free index */     \
    var object kvtable = TheHashtable(ht)->ht_kvtable;                  \
    /* address of the free entries in key-value-vector: */              \
    var gcv_object_t* KVptr = &TheHashedAlist(kvtable)->hal_data[3*index]; \
    set_break_sem_2();                       /* protect from breaks */  \
    /* increment COUNT: */                                              \
    TheHashedAlist(kvtable)->hal_count = fixnum_inc(TheHashedAlist(kvtable)->hal_count,1); \
    /* save key and value: */                                           \
    *KVptr++ = key; *KVptr++ = value;                                   \
    /* shorten free-list: */                                            \
    TheHashedAlist(kvtable)->hal_freelist = *KVptr;                     \
    /* insert free list-element index into the "list"                   \
     (put it after resize to the list-start,                            \
       because Iptr points into the index-vector,                       \
     else put it to the list-end,                                       \
       because hash_lookup was ended with *Iptr=nix): */                \
    *KVptr = *Iptr; *Iptr = freelist;                                   \
    { /* Set the htflags_gc_rehash_B bit if necessary. */               \
      var bool this_key_forces_gc_rehash = false;                       \
      var uintB flags = record_flags(TheHashtable(ht));                 \
      if (!(flags & htflags_test_user_B) && !(flags & htflags_gc_rehash_B)) \
        if (!hashcode_gc_invariant_p(ht,key)) {                         \
          record_flags_set(TheHashtable(ht),htflags_gc_rehash_B);       \
          this_key_forces_gc_rehash = true;                             \
        }                                                               \
      clr_break_sem_2();                       /* allow breaks again */ \
      if (this_key_forces_gc_rehash)                                    \
        if (record_flags(TheHashtable(ht)) & htflags_warn_gc_rehash_B)  \
          warn_key_forces_gc_rehash(ht,key);                            \
    }                                                                   \
  } while(0)

/* hash_table_weak_type(ht)
 > ht: hash-table
 < result: symbol NIL/:KEY/:VALUE/:KEY-AND-VALUE/:KEY-OR-VALUE */
global object hash_table_weak_type (object ht) {
  var object kvt = TheHashtable(ht)->ht_kvtable;
  if (simple_vector_p(kvt))
    return NIL;
  else {
    switch (Record_type(kvt)) {
      case Rectype_WeakHashedAlist_Key:
        return S(Kkey);
      case Rectype_WeakHashedAlist_Value:
        return S(Kvalue);
      case Rectype_WeakHashedAlist_Either:
        return S(Kkey_and_value);
      case Rectype_WeakHashedAlist_Both:
        return S(Kkey_or_value);
      default: NOTREACHED;
    }
  }
}

/* UP: Allocates the key-value-table for a new hash-table.
 allocate_kvt(weak,maxcount)
 > weak: NIL or :KEY or :VALUE or :KEY-AND-VALUE or :KEY-OR-VALUE
 > maxcount: number of key/value pairs to make room for
 < result: a key-value-table
 can trigger GC */
local inline maygc object allocate_kvt (object weak, uintL maxcount) {
  if (nullp(weak)) {
    var object kvt = allocate_vector(4+3*maxcount);
    TheHashedAlist(kvt)->hal_freelist = nix; /* dummy as free-list */
    return kvt;
  } else {
    var sintB rectype;
    if (eq(weak,S(Kkey))) # :KEY
      rectype = Rectype_WeakHashedAlist_Key;
    else if (eq(weak,S(Kvalue))) # :VALUE
      rectype = Rectype_WeakHashedAlist_Value;
    else if (eq(weak,S(Kkey_and_value))) # :KEY-AND-VALUE
      rectype = Rectype_WeakHashedAlist_Either;
    else if (eq(weak,S(Kkey_or_value))) # :KEY-OR-VALUE
      rectype = Rectype_WeakHashedAlist_Both;
    else
      NOTREACHED;
    var object kvt = allocate_lrecord(rectype,4+3*maxcount,lrecord_type);
    TheWeakHashedAlist(kvt)->wp_cdr = unbound; /* a GC-invariant dummy */
    TheWeakHashedAlist(kvt)->whal_itable = unbound;
    TheWeakHashedAlist(kvt)->whal_count = Fixnum_0;
    TheWeakHashedAlist(kvt)->whal_freelist = nix; /* dummy as free-list */
    var uintL i;
    for (i = 0; i < maxcount; i++) {
      TheWeakHashedAlist(kvt)->whal_data[3*i+0] = unbound;
      TheWeakHashedAlist(kvt)->whal_data[3*i+1] = unbound;
      TheWeakHashedAlist(kvt)->whal_data[3*i+2] = leer;
    }
    activate_weak(kvt); /* add to O(all_weakpointers) */
    return kvt;
  }
}

/* UP: Provides the numbers and vectors for a new hash-table.
 prepare_resize(maxcount,mincount_threshold,weak)
 > maxcount: wished new size MAXCOUNT
 > mincount_threshold: short-float MINCOUNT-THRESHOLD
 > weak: NIL or :KEY or :VALUE or :KEY-AND-VALUE or :KEY-OR-VALUE
 < result: maxcount
 < stack-layout: MAXCOUNT, SIZE, MINCOUNT, index-vector, key-value-vector.
 decreases STACK by 5
 can trigger GC */
local maygc uintL prepare_resize (object maxcount, object mincount_threshold,
                                  object weak) {
 prepare_resize_restart:
  /* check, if maxcount is not a too big fixnum >0 : */
  if (!posfixnump(maxcount))
    goto check_maxcount;
  {
    var uintV maxcountV = posfixnum_to_V(maxcount);
    var uintV sizeV = 2*maxcountV+1;
    /* SIZE odd in order to improve the hash-function! */
    if (!(sizeV <= (uintV)(vbitm(oint_data_len)-1)))
      /* sizeV should fit into a fixnum */
      goto check_maxcount;
    if (!(sizeV <= (uintL)(bitm(intLsize)-1)))
      /* sizeV should fit into an uintL */
      goto check_maxcount;
    /* numbers on the stack: */
    pushSTACK(maxcount);        /* MAXCOUNT */
    pushSTACK(fixnum(sizeV));   /* SIZE */
    /* MINCOUNT := (floor (* maxcount mincount-threshold)) */
    pushSTACK(maxcount); pushSTACK(mincount_threshold); funcall(L(mal),2);
    pushSTACK(value1); funcall(L(floor),1);
    pushSTACK(value1);
    /* stack-layout: MAXCOUNT, SIZE, MINCOUNT.
     allocate new vectors: */
    pushSTACK(allocate_vector(sizeV)); /* supply index-vector */
    pushSTACK(allocate_kvt(weak,maxcountV)); /* supply key-value-vector */
    /* finished. */
    return maxcountV;
  }
 check_maxcount: /* maxcount no fixnum or too big */
  pushSTACK(weak); pushSTACK(mincount_threshold); /* save */
  pushSTACK(NIL); /* no PLACE */
  pushSTACK(maxcount); /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_hashtable_size)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(maxcount);
  check_value(type_error,GETTEXT("Hash table size ~S too large"));
  maxcount = value1;
  mincount_threshold = popSTACK(); weak = popSTACK(); /* restore */
  goto prepare_resize_restart;
}

/* UP: Enlarges or diminishes a hash-table
 resize(ht,maxcount)
 > ht: hash-table
 > maxcount: wished new size MAXCOUNT
 < result: hash-table, EQ to the old one
 can trigger GC */
local maygc object resize (object ht, object maxcount) {
  pushSTACK(ht);
  var uintL maxcountL =
    prepare_resize(maxcount,TheHashtable(ht)->ht_mincount_threshold,
                   hash_table_weak_type(ht));
  /* no GC from now on! */
  var object KVvektor = popSTACK(); /* new key-value-vector */
  var object Ivektor = popSTACK();  /* index-vector */
  var object mincount = popSTACK(); /* MINCOUNT */
  var object size = popSTACK();     /* SIZE */
  maxcount = popSTACK();
  ht = popSTACK();
  TheHashedAlist(KVvektor)->hal_itable = Ivektor; /* enter new index-vector */
  /* Fill new key-value-vector:
   Loop over the old key-value-vector and
   copy all key-value-pairs with key /= "leer" :
   For traversing the old key-value-vector: */
  var uintL oldcount = posfixnum_to_V(TheHashtable(ht)->ht_maxcount);
  var object oldKVvektor = TheHashtable(ht)->ht_kvtable;
  var gcv_object_t* oldKVptr = &TheHashedAlist(oldKVvektor)->hal_data[0];
  /* For traversing the new key-value-vector: */
  var uintL count = maxcountL;
  var gcv_object_t* KVptr = &TheHashedAlist(KVvektor)->hal_data[0];
  /* For counting: */
  var object counter = Fixnum_0;
  dotimesL(oldcount,oldcount, {
    var object nextkey = *oldKVptr++;   /* next key */
    var object nextvalue = *oldKVptr++; /* and value */
    oldKVptr++;
    if (!eq(nextkey,leer)) {
      /* take over the entry into the new key-value-vector: */
      if (count==0) {           /* is the new vector already full? */
        /* There is not enough room!! */
        pushSTACK(ht);          /* hash-table */
        fehler(serious_condition,
               GETTEXT("internal error occured while resizing ~S"));
      }
      count--;
      *KVptr++ = nextkey; *KVptr++ = nextvalue; /* file in new vector */
      *KVptr++ = nix;
      counter = fixnum_inc(counter,1);          /* and count */
    }
  });
  /* Mark 'count' pairs of the new key-value-vector as "leer" : */
  dotimesL(count,count, { *KVptr++ = leer; *KVptr++ = leer; *KVptr++ = leer; } );
  TheHashedAlist(KVvektor)->hal_count = counter; /* enter COUNT (for consistency) */
  /* modify hash-table: */
  set_break_sem_2();                 /* protect from breaks */
  set_ht_invalid(TheHashtable(ht)); /* table must still be reorganized */
  TheHashtable(ht)->ht_size = posfixnum_to_V(size);  /* enter new SIZE */
  TheHashtable(ht)->ht_maxcount = maxcount; /* enter new MAXCOUNT */
  TheHashtable(ht)->ht_kvtable = KVvektor; /* enter new key-value-vector */
  TheHashtable(ht)->ht_mincount = mincount; /* enter new MINCOUNT */
  clr_break_sem_2();                        /* allow breaks again */
  return ht;
}

/* Macro: Enlarges a hash-table until freelist /= nix
 hash_prepare_store(hash_pos,key_pos);
 > int literal: hash-table position in STACK
 > int literal: key position in STACK
 < object ht: hash-table
 < object freelist: start of the free-list in the next-vector, /= nix
 < gcv_object_t* Iptr: arbitrary element of the "list", that belongs to the key
 for EQ/EQL/EQUAL/EQUALP hashtables the hash code changes after GC,
 so the raw hashcode cannot be cached.
 for user-defined hashtables, raw hashcode caching is good
 (especially for the user-defined tables, where hashcode can trigger GC!)
 can trigger GC */
#define hash_prepare_store(hash_pos,key_pos)                            \
  do {                                                                  \
    ht = STACK_(hash_pos);                                              \
    freelist = TheHashedAlist(TheHashtable(ht)->ht_kvtable)->hal_freelist; \
    if (eq(freelist,nix)) { /* free-list = empty "list" ? */            \
      var uintB flags = record_flags(TheHashtable(ht));                 \
      var bool cacheable = ht_test_code_user_p(ht_test_code(flags)); /* not EQ|EQL|EQUAL|EQUALP */ \
      var uintL hc_raw = cacheable ? hashcode_raw(ht,STACK_(key_pos)) : 0; \
      ht = STACK_(hash_pos);    /* hashcode_raw maygc */                \
      do { /* hash-table must still be enlarged: */                     \
        /* calculate new maxcount: */                                   \
        pushSTACK(TheHashtable(ht)->ht_maxcount);                       \
        pushSTACK(TheHashtable(ht)->ht_rehash_size); /* REHASH-SIZE (>1) */ \
        funcall(L(mal),2); /* (* maxcount rehash-size), is > maxcount */ \
        pushSTACK(value1);                                              \
        funcall(L(ceiling),1); /* (ceiling ...), integer > maxcount */  \
        ht = resize(STACK_(hash_pos),value1); /* enlarge table */       \
        ht = rehash(ht); /* and reorganize */                           \
        /* newly calculate the address of the entry in the index-vector: */ \
        { var uintL hashindex =                                         \
            (cacheable ? hashcode_cook(hc_raw,TheHashtable(ht)->ht_size) \
                       : hashcode(ht,STACK_(key_pos)));                 \
          var object kvtable = TheHashtable(ht)->ht_kvtable;            \
          Iptr = &TheSvector(TheHashedAlist(kvtable)->hal_itable)->data[hashindex]; \
          freelist = TheHashedAlist(kvtable)->hal_freelist;             \
        }                                                               \
      } while (eq(freelist,nix));                                       \
    }                                                                   \
  } while(0)

/* UP: Deletes the content of a hash-table.
 clrhash(ht);
 > ht: hash-table */
local void clrhash (object ht) {
  set_break_sem_2();            /* protect from breaks */
  var object kvtable = TheHashtable(ht)->ht_kvtable;
  /* Delete pairs and build up freelist: */
  {
    var object index = TheHashtable(ht)->ht_maxcount; /* MAXCOUNT */
    var uintL maxcount = posfixnum_to_V(index);
    var object freelist = nix;
    if (maxcount > 0) {
      var gcv_object_t* KVptr = &TheHashedAlist(kvtable)->hal_data[3*maxcount]; /* end of kvtable */
      do {
        index = fixnum_inc(index,-1); /* decrement index */
        *--KVptr = freelist;              /* delete next-index */
        *--KVptr = leer; *--KVptr = leer; /* delete key and value */
        freelist = index;
      } while (!eq(index,Fixnum_0));
    }
    TheHashedAlist(kvtable)->hal_freelist = freelist; /* save freelist */
  }
  TheHashedAlist(kvtable)->hal_count = Fixnum_0; /* COUNT := 0 */
  /* Fill index-vector with "nix" : */
  var object Ivektor = TheHashedAlist(kvtable)->hal_itable; /* index-vector */
  {
    var gcv_object_t* ptr = &TheSvector(Ivektor)->data[0];
    var uintL count = TheHashtable(ht)->ht_size; /* SIZE, >0 */
    dotimespL(count,count, { *ptr++ = nix; } );
  }
  record_flags_clr(TheHashtable(ht),htflags_gc_rehash_B); /* no dangerous keys now */
  set_ht_valid(TheHashtable(ht)); /* hashtable is now completely organized */
  clr_break_sem_2();                 /* allow breaks again */
}

/* UP: fetches the value of *eq-hashfunction*. */
local object get_eq_hashfunction (void) {
  var object value = Symbol_value(S(eq_hashfunction));
  if (eq(value,S(fasthash_eq)) || eq(value,S(stablehash_eq)))
    return value;
  else {
    Symbol_value(S(eq_hashfunction)) = S(fasthash_eq);
    pushSTACK(value);                   # TYPE-ERROR slot DATUM
    pushSTACK(O(type_eq_hashfunction)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(S(fasthash_eq));
    pushSTACK(value);
    pushSTACK(S(stablehash_eq)); pushSTACK(S(fasthash_eq));
    pushSTACK(S(eq_hashfunction));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~S: The value of ~S should be ~S or ~S, not ~S.\n"
                   "It has been reset to ~S."));
  }
}

/* UP: fetches the value of *eql-hashfunction*. */
local object get_eql_hashfunction (void) {
  var object value = Symbol_value(S(eql_hashfunction));
  if (eq(value,S(fasthash_eql)) || eq(value,S(stablehash_eql)))
    return value;
  else {
    Symbol_value(S(eql_hashfunction)) = S(fasthash_eql);
    pushSTACK(value);                    # TYPE-ERROR slot DATUM
    pushSTACK(O(type_eql_hashfunction)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(S(fasthash_eql));
    pushSTACK(value);
    pushSTACK(S(stablehash_eql)); pushSTACK(S(fasthash_eql));
    pushSTACK(S(eql_hashfunction));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~S: The value of ~S should be ~S or ~S, not ~S.\n"
                   "It has been reset to ~S."));
  }
}

/* UP: fetches the value of *equal-hashfunction*. */
local object get_equal_hashfunction (void) {
  var object value = Symbol_value(S(equal_hashfunction));
  if (eq(value,S(fasthash_equal)) || eq(value,S(stablehash_equal)))
    return value;
  else {
    Symbol_value(S(equal_hashfunction)) = S(fasthash_equal);
    pushSTACK(value);                      # TYPE-ERROR slot DATUM
    pushSTACK(O(type_equal_hashfunction)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(S(fasthash_equal));
    pushSTACK(value);
    pushSTACK(S(stablehash_equal)); pushSTACK(S(fasthash_equal));
    pushSTACK(S(equal_hashfunction));
    pushSTACK(TheSubr(subr_self)->name);
    fehler(type_error,
           GETTEXT("~S: The value of ~S should be ~S or ~S, not ~S.\n"
                   "It has been reset to ~S."));
  }
}

/* check the :WEAK argument and return it
 can trigger GC */
local maygc object check_weak (object weak) {
 check_weak_restart:
  if (missingp(weak)) return NIL;
  if (eq(weak,S(Kkey)) || eq(weak,S(Kvalue))
      || eq(weak,S(Kkey_and_value)) || eq(weak,S(Kkey_or_value)))
    return weak;
  /* invalid */
  pushSTACK(NIL); /* no PLACE */
  pushSTACK(weak);            /* TYPE-ERROR slot DATUM */
  pushSTACK(O(type_weak_ht)); /* TYPE-ERROR slot EXPECTED-TYPE */
  pushSTACK(NIL); pushSTACK(S(Kkey)); pushSTACK(S(Kvalue));
  pushSTACK(S(Kkey_and_value)); pushSTACK(S(Kkey_or_value));
  pushSTACK(weak); pushSTACK(TheSubr(subr_self)->name);
  check_value(type_error,GETTEXT("~S: argument ~S should be ~S, ~S, ~S, ~S or ~S."));
  weak = value1;
  goto check_weak_restart;
}

/* (MAKE-HASH-TABLE [:test] [:size] [:rehash-size] [:rehash-threshold]
                    [:key-type] [:value-type]
                    [:weak] [:warn-if-needs-rehash-after-gc] [:initial-contents]), CLTL p. 283 */
LISPFUN(make_hash_table,seclass_read,0,0,norest,key,9,
        (kw(initial_contents),kw(key_type),kw(value_type),
         kw(warn_if_needs_rehash_after_gc),kw(weak),
         kw(test),kw(size),kw(rehash_size),kw(rehash_threshold)) )
{ /* The rehash-threshold correlates in our implementation to the
   ratio MAXCOUNT : SIZE = ca. 1 : 2.
   We ignore the rehash-threshold-argument, as both too big values and
   also too small values could be harmful: 0.99 causes on average
   too long access-times; 0.00001 causes, that SIZE = MAXCOUNT/threshold
   could become a bignum too fast.
   The additional initial-contents-argument is an alist = list of
   (key . value) - pairs, that are used to initialize the table.
   STACK layout:
      initial-contents, key-type, value-type,
      warn-if-needs-rehash-after-gc, weak,
      test, size, rehash-size, rehash-threshold. */
  var uintB flags;
  var object lookuppfn;
  var object hashcodepfn;
  var object testpfn;
  var object gcinvariantpfn;
 check_test_restart: { /* check test-argument: */
    var object test = STACK_3;
    if (!boundp(test) || eq(test,S(eql)) || eq(test,L(eql)))
      test = get_eql_hashfunction();
    if (eq(test,S(fasthash_eql))) {
      flags = htflags_test_eql_B; /* FASTHASH-EQL */
      hashcodepfn = P(hashcode2);
      gcinvariantpfn = P(gcinvariant_hashcode2_p);
      testpfn = P(eql);
      lookuppfn = P(hash_lookup_builtin);
    } else if (eq(test,S(stablehash_eql))) {
      flags = htflags_test_eql_B | htflags_stablehash_B; /* STABLEHASH-EQL */
      hashcodepfn = P(hashcode2stable);
      gcinvariantpfn = P(gcinvariant_hashcode2stable_p);
      testpfn = P(eql);
      lookuppfn = P(hash_lookup_builtin);
    } else {
      if (eq(test,S(eq)) || eq(test,L(eq)))
        test = get_eq_hashfunction();
      if (eq(test,S(fasthash_eq))) {
        flags = htflags_test_eq_B; /* FASTHASH-EQ */
        hashcodepfn = unbound; /* specially handled in hashcode_builtin */
        gcinvariantpfn = P(gcinvariant_hashcode1_p);
        testpfn = unbound; /* specially handled in hash_lookup_builtin */
        lookuppfn = P(hash_lookup_builtin);
      } else if (eq(test,S(stablehash_eq))) {
        flags = htflags_test_eq_B | htflags_stablehash_B; /* STABLEHASH-EQ */
        hashcodepfn = P(hashcode1stable);
        gcinvariantpfn = P(gcinvariant_hashcode1stable_p);
        testpfn = unbound; /* specially handled in hash_lookup_builtin */
        lookuppfn = P(hash_lookup_builtin);
      } else {
        if (eq(test,S(equal)) || eq(test,L(equal)))
          test = get_equal_hashfunction();
        if (eq(test,S(fasthash_equal))) {
          flags = htflags_test_equal_B; /* FASTHASH-EQUAL */
          hashcodepfn = P(hashcode3);
          gcinvariantpfn = P(gcinvariant_hashcode3_p);
          testpfn = P(equal);
          lookuppfn = P(hash_lookup_builtin);
        } else if (eq(test,S(stablehash_equal))) {
          flags = htflags_test_equal_B | htflags_stablehash_B; /* STABLEHASH-EQUAL */
          hashcodepfn = P(hashcode3stable);
          gcinvariantpfn = P(gcinvariant_hashcode3stable_p);
          testpfn = P(equal);
          lookuppfn = P(hash_lookup_builtin);
        } else if (eq(test,S(equalp)) || eq(test,L(equalp))) {
          flags = htflags_test_equalp_B; /* EQUALP */
          hashcodepfn = P(hashcode4);
          gcinvariantpfn = P(gcinvariant_hashcode4_p);
          testpfn = P(equalp);
          lookuppfn = P(hash_lookup_builtin);
        } else {
          hashcodepfn = unbound;
          gcinvariantpfn = unbound;
          testpfn = unbound;
          lookuppfn = P(hash_lookup_user);
          if (symbolp(test)) {
            var object ht_test = get(test,S(hash_table_test));
            if (!consp(ht_test)) goto test_error;
            STACK_3 = ht_test;
            flags = htflags_test_user_B; /* user-defined ht_test */
          } else if (consp(test)) {
            flags = htflags_test_user_B; /* ad hoc (user-defined ht_test) */
          } else {
           test_error:
            pushSTACK(NIL); /* no PLACE */
            pushSTACK(test); /* TYPE-ERROR slot DATUM */
            pushSTACK(O(type_hashtable_test)); /* TYPE-ERROR slot EXPECTED-TYPE */
            pushSTACK(test); pushSTACK(S(Ktest));
            pushSTACK(S(make_hash_table));
            check_value(type_error,GETTEXT("~S: illegal ~S argument ~S"));
            STACK_3 = value1;
            goto check_test_restart;
          }
        }
      }
    }
  } /* flags contains the flags for the test. */
 check_size: { /* check size-argument: */
    var object size = STACK_2;
    if (!boundp(size)) {
      STACK_2 = Fixnum_1;       /* 1 as default */
    } else {
      if (!posfixnump(size)) {
        pushSTACK(NIL); /* no PLACE */
        pushSTACK(size); /* TYPE-ERROR slot DATUM */
        pushSTACK(O(type_posfixnum)); /* TYPE-ERROR slot EXPECTED-TYPE */
        pushSTACK(size); pushSTACK(S(Ksize));
        pushSTACK(S(make_hash_table));
        check_value(type_error,GETTEXT("~S: ~S argument should be a fixnum >=0, not ~S"));
        STACK_2 = value1;
        goto check_size;
      }
      /* size is a fixnum >=0 */
      if (eq(size,Fixnum_0))
        STACK_2 = Fixnum_1; /* turn 0 into 1 */
    }
  } /* size is now a fixnum >0. */
  check_rehash_size: { /* (OR (INTEGER 1 *) (FLOAT (1.0) *)) */
    if (!boundp(STACK_1)) { /* default-rehash-size = 1.5s0 */
      STACK_1 = make_SF(0,SF_exp_mid+1,(bit(SF_mant_len)*3)/2);
    } else {
      if (!floatp(STACK_1)) { /* Float is OK */
        if (!integerp(STACK_1) || R_minusp(STACK_1) || eq(STACK_1,Fixnum_0)) {
          /* else it should be a positive integer */
         bad_rehash_size:
          pushSTACK(NIL); /* no PLACE */
          pushSTACK(STACK_(1+1)); /* TYPE-ERROR slot DATUM */
          pushSTACK(O(type_hashtable_rehash_size)); /* EXPECTED-TYPE */
          pushSTACK(STACK_(1+3)); pushSTACK(S(Krehash_size));
          pushSTACK(S(make_hash_table));
          check_value(type_error,GETTEXT("~S: ~S argument should be an integer or a float > 1, not ~S"));
          STACK_1 = value1;
          goto check_rehash_size;
        }
        /* As it is senseless to enlarge a table always only by a fixed
           number of elements (results in disastrous efficiency), we set
           rehash-size := min(1 + rehash-size/size , 2.0) . */
        pushSTACK(STACK_1); /* rehash-size */
        pushSTACK(STACK_(2+1)); /* size */
        funcall(L(durch),2); /* (/ rehash-size size) */
        pushSTACK(value1);
        funcall(L(einsplus),1); /* (1+ ...) */
        pushSTACK(value1);
        pushSTACK(make_SF(0,SF_exp_mid+2,bit(SF_mant_len))); /* 2.0s0 */
        funcall(L(min),2); /* (MIN ... 2.0s0) */
        STACK_1 = value1; /* =: rehash-size */
      }
      /* check (> rehash-size 1) : */
      pushSTACK(STACK_1); /* rehash-size */
      pushSTACK(Fixnum_1); /* 1 */
      funcall(L(groesser),2); /* (> rehash-size 1) */
      if (nullp(value1)) goto bad_rehash_size;
      /* convert rehash-size into a short-float: */
      pushSTACK(STACK_1); /* rehash-size */
      pushSTACK(SF_0); /* 0.0s0 */
      funcall(L(float),2); /* (FLOAT rehash-size 0.0s0) = (COERCE rehash-size 'SHORT-FLOAT) */
      /* enforce (>= rehash-size 1.125s0) : */
      pushSTACK(value1);
      pushSTACK(make_SF(0,SF_exp_mid+1,(bit(SF_mant_len)/8)*9)); /* 1.125s0 */
      funcall(L(max),2); /* (max rehash-size 1.125s0) */
      STACK_1 = value1; /* =: rehash-size */
    }
  } /* rehash-size is a short-float >= 1.125 . */
 check_rehash_threshold: { /* check rehash-threshold: should be real in [0;1]*/
    var object rehash_threshold = STACK_0;
    if (boundp(rehash_threshold)) { /* not specified -> OK */
      if_realp(rehash_threshold, ;, goto bad_rehash_threshold;);
      if (false) {
       bad_rehash_threshold:
        pushSTACK(NIL); /* no PLACE */
        pushSTACK(rehash_threshold); /* TYPE-ERROR slot DATUM */
        pushSTACK(O(type_hashtable_rehash_threshold)); /* TYPE-ERROR slot EXPECTED-TYPE */
        pushSTACK(STACK_1); pushSTACK(S(Krehash_threshold));
        pushSTACK(S(make_hash_table));
        check_value(type_error,GETTEXT("~S: ~S argument should be a real between 0 and 1, not ~S"));
        STACK_0 = value1;
        goto check_rehash_threshold;
      }
      pushSTACK(Fixnum_1);
      pushSTACK(rehash_threshold);
      pushSTACK(Fixnum_0);
      funcall(L(grgleich),3); /* (>= 1 rehash-threshold 0) */
      if (nullp(value1)) goto bad_rehash_threshold;
    }
  }
  { /* If the initial-contents-argument is specified, we set
     size := (max size (length initial-contents)) , so afterwards, when
     the initial-contents are written, the table needs not be enlarged: */
    var object initial_contents = STACK_8;
    if (boundp(initial_contents)) { /* specified ? */
      var uintL initial_length = llength(initial_contents); /* length of the alist */
      if (initial_length > posfixnum_to_V(STACK_2)) /* > size ? */
        STACK_2 = fixnum(initial_length); /* yes -> enlarge size */
    }
  } /* size is a fixnum >0, >= (length initial-contents) . */
  { /* calculate MINCOUNT-THRESHOLD = 1/rehash-size^2 : */
    var object rehash_size = STACK_1;
    pushSTACK(rehash_size);
    pushSTACK(rehash_size);
    funcall(L(mal),2); /* (* rehash-size rehash-size) */
    pushSTACK(value1);
    funcall(L(durch),1); /* (/ ...) */
    STACK_0 = value1;
  }
  /* STACK layout:
      initial-contents, key-type, value-type,
      warn-if-needs-rehash-after-gc, weak,
      test, size, rehash-size, mincount-threshold
    provide vectors etc., with size as MAXCOUNT: [STACK_4 == weak] */
  STACK_4 = check_weak(STACK_4);
  prepare_resize(STACK_2,STACK_0,STACK_4);
  var object ht = allocate_hash_table(); /* new hash-tabelle */
  /* fill: */
  var object kvtable = popSTACK(); /* key-value-vector */
  TheHashtable(ht)->ht_kvtable = kvtable;
  TheHashedAlist(kvtable)->hal_itable = popSTACK();  /* index-vector */
  TheHashtable(ht)->ht_mincount = popSTACK(); /* MINCOUNT */
  TheHashtable(ht)->ht_size = posfixnum_to_V(popSTACK()); /* SIZE */
  TheHashtable(ht)->ht_maxcount = popSTACK(); /* MAXCOUNT */
  /* STACK layout:
     initial-contents, key-type, value-type,
     warn-if-needs-rehash-after-gc, weak,
     test, size, rehash-size, mincount-threshold. */
  TheHashtable(ht)->ht_mincount_threshold = popSTACK(); /*MINCOUNT-THRESHOLD*/
  TheHashtable(ht)->ht_rehash_size = popSTACK(); /* REHASH-SIZE */
  TheHashtable(ht)->ht_lookupfn = lookuppfn;
  TheHashtable(ht)->ht_hashcodefn = hashcodepfn;
  TheHashtable(ht)->ht_testfn = testpfn;
  TheHashtable(ht)->ht_gcinvariantfn = gcinvariantpfn;
  /* STACK layout:
     initial-contents, key-type, value-type,
     warn-if-needs-rehash-after-gc, weak, test, -. */
  if (ht_test_code_user_p(ht_test_code(flags))) { /* user-defined ht_test */
    STACK_0 = ht;
    var object test = coerce_function(Car(STACK_1)); pushSTACK(test);
    var object hash = coerce_function(Cdr(STACK_2));
    ht = STACK_1;
    TheHashtable(ht)->ht_test = popSTACK();
    TheHashtable(ht)->ht_hash = hash;
  }
  /* Use warn-if-needs-rehash-after-gc argument. */
  if (!missingp(STACK_3))
    flags |= htflags_warn_gc_rehash_B;
  record_flags_replace(TheHashtable(ht), flags);
  clrhash(ht);                  /* empty table, COUNT := 0 */
  skipSTACK(6);
  /* stack-layout: initial-contents. */
  {
    pushSTACK(ht);
    while (consp(STACK_1)) { /* if it was specified, so long as it was a cons: */
      var object next = Car(STACK_1); /* alist element */
      if (consp(next)) { /* a cons (Key . Value) ? */
        /* execute (SYSTEM::PUTHASH (car next) hashtable (cdr next)) ,
           whereby the table cannot grow: */
        var gcv_object_t* KVptr;
        var gcv_object_t* Iptr;
        if (hash_lookup(STACK_0,Car(next),true,&KVptr,&Iptr)) { /* search */
          /* already found -> was already contained in the alist further
             on the left, and in alists the first association (left)
             shadows all other associations of the same key. */
          ht = STACK_0; /* restore ht */
        } else { /* not found -> make a new entry: */
          var object freelist = /* start of the free-list in the next-vector */
            TheHashedAlist(TheHashtable(STACK_0)->ht_kvtable)->hal_freelist;
          if (eq(freelist,nix)) { /* empty "list" ? */
            pushSTACK(STACK_0); /* hash-table */
            pushSTACK(S(make_hash_table));
            fehler(serious_condition,
                   GETTEXT("~S: internal error while building ~S"));
          }
          ht = STACK_0; /* restore ht */
          next = Car(STACK_1); /* restore next */
          hash_store(Car(next),Cdr(next)); /* make entry */
        }
      }
      STACK_1 = Cdr(STACK_1); /* pop alist */
    }
    skipSTACK(2); /* drop ht, initial-contents */
  }
  VALUES1(ht); /* hash-table as value */
}

/* UP: Searches an object in a hash-table.
 gethash(obj,ht,allowgc)
 > obj: object, as key
 > ht: hash-table
 > allowgc: whether GC is allowed during hash lookup
            (should be true if the hash-table has a user-defined test)
 < result: if found, belonging value, else nullobj
 can trigger GC - if allowgc is true */
global /*maygc*/ object gethash (object obj, object ht, bool allowgc) {
  GCTRIGGER_IF(allowgc, GCTRIGGER2(obj,ht));
  var gcv_object_t* KVptr;
  var gcv_object_t* Iptr;
  if (hash_lookup(ht,obj,allowgc,&KVptr,&Iptr))
    return KVptr[1]; /* found -> value */
  else
    return nullobj;
}

/* error, if an argument is not a hash-table
 check_hashtable(obj);
 > obj: object
 < hashtable
 can trigger GC */
local maygc object check_hashtable (object obj) {
  while (!hash_table_p(obj)) {
    pushSTACK(NIL); /* no PLACE */
    pushSTACK(obj); /* TYPE-ERROR slot DATUM */
    pushSTACK(S(hash_table)); /* TYPE-ERROR slot EXPECTED-TYPE */
    pushSTACK(obj);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: argument ~S is not a hash table"));
    obj = value1;
  }
  return obj;
}

LISPFUN(gethash,seclass_default,2,1,norest,nokey,0,NIL)
{ /* (GETHASH key hashtable [default]), CLTL p. 284 */
  var object ht = check_hashtable(STACK_1); /* hashtable argument */
  var gcv_object_t* KVptr;
  var gcv_object_t* Iptr;
  /* search key STACK_2 in the hash-table: */
  if (hash_lookup(ht,STACK_2,true,&KVptr,&Iptr)) { /* -> Value as value: */
    VALUES2(KVptr[1], T); /* and T as the 2nd value */
    skipSTACK(3);
  } else {                    /* not found -> default or NIL as value */
    var object def = popSTACK(); /* default */
    VALUES2(!boundp(def) ? NIL : def,
            NIL); /* NIL as the 2nd value */
    skipSTACK(2);
  }
}

LISPFUNN(puthash,3)
{ /* (SYSTEM::PUTHASH key hashtable value) =
 (SETF (GETHASH key hashtable) value), CLTL p. 284 */
  STACK_1 = check_hashtable(STACK_1); /* hashtable argument */
  var gcv_object_t* KVptr;
  var gcv_object_t* Iptr;
  /* search key STACK_2 in the hash-table: */
  if (hash_lookup(STACK_1,STACK_2,true,&KVptr,&Iptr)) { /* -> replace value: */
    VALUES1(KVptr[1] = popSTACK()); skipSTACK(2);
  } else {                      /* not found -> make new entry: */
    var object ht;
    var object freelist;
    hash_prepare_store(1,2); /* ht==STACK_1, obj==STACK_2 */
    hash_store(STACK_2,STACK_0); /* make entry */
    VALUES1(popSTACK()); /* value as value */
    skipSTACK(2);
  }
}

/* UP: Searches a key in a hash-table and returns the last value.
 shifthash(ht,obj,value) == (SHIFTF (GETHASH obj ht) value)
 > ht: hash-table
 > obj: object
 > value: new value
 > allowgc: whether GC is allowed during hash lookup
            (should be true if the hash-table has a user-defined test or
             if the hash-table is not known to already contain a value for obj)
 < result: old value
 can trigger GC - if allowgc is true */
global /*maygc*/ object shifthash (object ht, object obj, object value, bool allowgc) {
  GCTRIGGER_IF(allowgc, GCTRIGGER3(ht,obj,value));
  var gcv_object_t* KVptr;
  var gcv_object_t* Iptr;
  pushSTACK(ht); pushSTACK(obj); pushSTACK(value); /* save args */
  /* search key obj in the hash-table: */
  if (hash_lookup(ht,obj,allowgc,&KVptr,&Iptr)) { /* found -> replace value: */
    var object oldvalue = KVptr[1];
    KVptr[1] = STACK_0; skipSTACK(3);
    return oldvalue;
  } else { /* not found -> build new entry: */
    ASSERT(allowgc);
    var object freelist;
    hash_prepare_store(2,1);  /* ht==STACK_2, obj==STACK_1 */
    hash_store(STACK_1,STACK_0); /* build entry */
    skipSTACK(3);
    return NIL;                 /* default for the old value is NIL */
  }
}

LISPFUNN(remhash,2)
{ /* (REMHASH key hashtable), CLTL p. 284 */
  STACK_0 = check_hashtable(STACK_0); /* hashtable argument */
  var object key = STACK_1; /* key-argument */
  var gcv_object_t* KVptr;
  var gcv_object_t* Iptr;
  /* search key in the hash-table: */
  if (hash_lookup(STACK_0,key,true,&KVptr,&Iptr)) {
    /* found -> drop from the hash-table: */
    var object ht = STACK_0; skipSTACK(2);
    var object kvtable = TheHashtable(ht)->ht_kvtable;
    var object index = *Iptr;   /* index in next-vector */
    /* with KVptr = &TheHashedAlist(kvtable)->hal_data[3*index] */
    set_break_sem_2();          /* protect from breaks */
    *KVptr++ = leer; *KVptr++ = leer; /* empty key and value */
    *Iptr = *KVptr;             /* shorten "list" */
    /* lengthen free-list: */
    *KVptr = TheHashedAlist(kvtable)->hal_freelist;
    TheHashedAlist(kvtable)->hal_freelist = index;
    /* decrement COUNT : */
    TheHashedAlist(kvtable)->hal_count = fixnum_inc(TheHashedAlist(kvtable)->hal_count,-1);
    clr_break_sem_2();          /* allow breaks again */
    /* shrink the hash-table for COUNT < MINCOUNT : */
    if (  posfixnum_to_V(TheHashedAlist(kvtable)->hal_count)
        < posfixnum_to_V(TheHashtable(ht)->ht_mincount)) {
      /* shrink hash-table:
       maxcount := (max (floor (/ maxcount rehash-size)) 1) */
      pushSTACK(ht);            /* save hashtable */
      pushSTACK(TheHashtable(ht)->ht_maxcount);
      pushSTACK(TheHashtable(ht)->ht_rehash_size); /* REHASH-SIZE (>1) */
      funcall(L(durch),2); /* (/ maxcount rehash-size), is < maxcount */
      pushSTACK(value1);
      funcall(L(floor),1); /* (floor ...), an integer >=0, < maxcount */
      var object maxcount = value1;
      if (eq(maxcount,Fixnum_0))
        maxcount = Fixnum_1;       /* turn 0 into 1 */
      resize(popSTACK(),maxcount); /* shrink table */
    }
    VALUES1(T);
  } else {                      /* not found */
    skipSTACK(2); VALUES1(NIL);
  }
}

LISPFUNN(maphash,2)
{ /* (MAPHASH function hashtable), CLTL p. 285 */
  var object ht = check_hashtable(STACK_0); /* hashtable argument */
  /* traverse the key-value-vector in reverse direction and
   call the function for all key-value-pairs with key /= "leer" : */
  var uintL index = 3*posfixnum_to_V(TheHashtable(ht)->ht_maxcount);
  STACK_0 = TheHashtable(ht)->ht_kvtable; /* key-value-vector */
  /* stack-layout: function, key-value-vector. */
  while (index) {
    index -= 3;
    var gcv_object_t* KVptr = &TheHashedAlist(STACK_0)->hal_data[index];
    if (!eq(KVptr[0],leer)) {   /* key /= "leer" ? */
      pushSTACK(KVptr[0]);      /* key as the 1st argument */
      pushSTACK(KVptr[1]);      /* value as the 2nd argument */
      funcall(STACK_(1+2),2);   /* (FUNCALL function Key Value) */
    }
  }
  skipSTACK(2);
  VALUES1(NIL);
}

LISPFUNN(clrhash,1)
{ /* (CLRHASH hashtable), CLTL p. 285 */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  clrhash(ht);                                 /* empty table */
  /* Shrink the hash-table when MINCOUNT > 0 : */
  if (!eq(TheHashtable(ht)->ht_mincount,Fixnum_0))
    ht = resize(ht,Fixnum_1); /* shrink to MAXCOUNT:=1 , so that MINCOUNT:=0 */
  VALUES1(ht); /* hash-table as value */
}

LISPFUNNR(hash_table_count,1)
{ /* (HASH-TABLE-COUNT hashtable), CLTL p. 285, CLtL2 p. 439 */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  var object count = TheHashedAlist(TheHashtable(ht)->ht_kvtable)->hal_count;
  VALUES1(count); /* fixnum COUNT as value */
}

LISPFUNNR(hash_table_rehash_size,1)
{ /* (HASH-TABLE-REHASH-SIZE hashtable), CLtL2 p. 441, dpANS p. 18-7 */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  VALUES1(TheHashtable(ht)->ht_rehash_size); /* short-float REHASH-SIZE */
}

LISPFUNNR(hash_table_rehash_threshold,1)
{ /* (HASH-TABLE-REHASH-THRESHOLD hashtable), CLtL2 p. 441, dpANS p. 18-8 */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  /* As MAKE-HASH-TABLE ignores the :REHASH-THRESHOLD argument, the value
   is irrelevant here and arbitrary. */
  VALUES1(make_SF(0,SF_exp_mid+0,(bit(SF_mant_len)/2)*3)); /* 0.75s0 */
}

LISPFUNNR(hash_table_size,1)
{ /* (HASH-TABLE-SIZE hashtable), CLtL2 p. 441, dpANS p. 18-9 */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  VALUES1(TheHashtable(ht)->ht_maxcount); /* Fixnum MAXCOUNT */
}

LISPFUNNR(hash_table_warn_if_needs_rehash_after_gc,1)
{ /* (HASH-TABLE-WARN-IF-NEEDS-REHASH-AFTER-GC hashtable) */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  VALUES_IF(record_flags(TheHashtable(ht)) & htflags_warn_gc_rehash_B);
}

LISPFUNN(set_hash_table_warn_if_needs_rehash_after_gc,2)
{ /* ((SETF HASH-TABLE-WARN-IF-NEEDS-REHASH-AFTER-GC) val hashtable) */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  var bool warn_p = !nullp(popSTACK());
  if (warn_p)
    record_flags_set(TheHashtable(ht),htflags_warn_gc_rehash_B);
  else
    record_flags_clr(TheHashtable(ht),htflags_warn_gc_rehash_B);
  VALUES_IF(warn_p);
}

/* return the hash table symbol
 or cons (test . hash) for user-defined ht_test
 can trigger GC - for user-defined ht_test */
global maygc object hash_table_test (object ht) {
  var uintB test_code = ht_test_code(record_flags(TheHashtable(ht)));
  switch (test_code) {
    case htflags_test_eq_B:
      return S(fasthash_eq);
    case htflags_test_eq_B | htflags_stablehash_B:
      return S(stablehash_eq);
    case htflags_test_eql_B:
      return S(fasthash_eql);
    case htflags_test_eql_B | htflags_stablehash_B:
      return S(stablehash_eql);
    case htflags_test_equal_B:
      return S(fasthash_equal);
    case htflags_test_equal_B | htflags_stablehash_B:
      return S(stablehash_equal);
    case htflags_test_equalp_B:
      return S(equalp);
    case bit(2): { /* user-defined ==> (test . hash) */
      pushSTACK(ht);
      var object ret = allocate_cons();
      ht = popSTACK();
      Car(ret) = TheHashtable(ht)->ht_test;
      Cdr(ret) = TheHashtable(ht)->ht_hash;
      /* should we do this at all? */
      /*if (subrp(Car(ret))) Car(ret) = TheSubr(Car(ret))->name;
        if (subrp(Cdr(ret))) Cdr(ret) = TheSubr(Cdr(ret))->name;*/
      return ret;
    }
    default: NOTREACHED;
  }
}

LISPFUNNF(hash_table_test,1)
{ /* (HASH-TABLE-TEST hashtable), CLtL2 p. 441, dpANS p. 18-9 */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  VALUES1(hash_table_test(ht)); /* symbol as value */
}

/* (SYSTEM::FASTHASH-STABLE-P obj)
   tests whether obj's FASTHASH-EQ hash code is stable across GCs. */
LISPFUNNF(fasthash_stable_p,1)
{
  var object obj = popSTACK();
  VALUES_IF(gcinvariant_hashcode1_p(obj));
}

/* (SYSTEM::STABLEHASH-STABLE-P obj)
   tests whether obj's STABLEHASH-EQ hash code is stable across GCs. */
LISPFUNNR(stablehash_stable_p,1)
{
  var object obj = popSTACK();
  VALUES_IF(gcinvariant_hashcode1stable_p(obj));
}

/* auxiliary functions for WITH-HASH-TABLE-ITERATOR, CLTL2 p. 439:
 (SYSTEM::HASH-TABLE-ITERATOR hashtable) returns an internal state
 for iterating through a hash-table.
 (SYSTEM::HASH-TABLE-ITERATE internal-state) iterates through a hash-table
 by one, thereby changes internal-state and returns: 3 values
 T, key, value of the next hash-table-entry resp. 1 value NIL at the end. */

LISPFUNNR(hash_table_iterator,1) {
  var object ht = check_hashtable(STACK_0); /* hashtable argument */
  /* An internal state consists of the key-value-vector and an index. */
  STACK_0 = TheHashtable(ht)->ht_kvtable; /* key-value-vector */
  var object maxcount = TheHashtable(ht)->ht_maxcount; /* maxcount */
  var object state = allocate_cons();
  Car(state) = popSTACK();      /* key-value-vector as car */
  Cdr(state) = maxcount;        /* maxcount as cdr */
  VALUES1(state);               /* state as value */
}

LISPFUNN(hash_table_iterate,1) {
  var object state = popSTACK(); /* internal state */
  if (consp(state)) {            /* hopefully a cons */
    var object table = Car(state); /* key-value-vector */
    loop {
      var uintL index = posfixnum_to_V(Cdr(state));
      if (index==0)             /* index=0 -> no more elements */
        break;
      Cdr(state) = fixnum_inc(Cdr(state),-1); /* decrement index */
      var gcv_object_t* KVptr = &TheHashedAlist(table)->hal_data[3*index-3];
      if (!eq(KVptr[0],leer)) { /* Key /= "leer" ? */
        VALUES3(T,
                KVptr[0], /* key as the 2nd value */
                KVptr[1]); /* value as the 3rd value */
        return;
      }
    }
  }
  VALUES1(NIL); return; /* 1 value NIL */
}

LISPFUNNR(hash_table_weak_p,1)
{ /* (EXT:HASH-TABLE-WEAK-P ht) */
  var object ht = check_hashtable(popSTACK()); /* hashtable argument */
  VALUES1(hash_table_weak_type(ht));
}

LISPFUNN(set_hash_table_weak_p,2)
{ /* ((SETF HASH-TABLE-WEAK-P) weak-p ht) */
  STACK_0 = check_hashtable(STACK_0);
  var object val = check_weak(STACK_1); /* weak-p */
  var object ht = STACK_0; /* hashtable argument */
  if (!eq(val,hash_table_weak_type(ht))) {
    var uintL maxcount = posfixnum_to_V(TheHashtable(ht)->ht_maxcount);
    var object new_kvt;
    for (;;) {
      new_kvt = allocate_kvt(val,maxcount);
      /* Check whether the hash-table has not been resized during
         allocate_kvt(). */
      var uintL new_maxcount =
        posfixnum_to_V(TheHashtable(STACK_0)->ht_maxcount);
      if (maxcount == new_maxcount)
        break;
      maxcount = new_maxcount;
    }
    ht = STACK_0;
    var object old_kvt = TheHashtable(ht)->ht_kvtable;
    copy_mem_o(&TheHashedAlist(new_kvt)->hal_data[0],
               &TheHashedAlist(old_kvt)->hal_data[0],
               3*maxcount);
    TheHashedAlist(new_kvt)->hal_itable = TheHashedAlist(old_kvt)->hal_itable;
    TheHashedAlist(new_kvt)->hal_count = TheHashedAlist(old_kvt)->hal_count;
    TheHashedAlist(new_kvt)->hal_freelist = TheHashedAlist(old_kvt)->hal_freelist;
    TheHashtable(ht)->ht_kvtable = new_kvt;
  }
  VALUES1(hash_table_weak_type(ht)); skipSTACK(2);
}

LISPFUNN(class_gethash,2)
{/* (CLOS::CLASS-GETHASH ht object) is like (GETHASH (CLASS-OF object) ht). */
  var object ht = check_hashtable(STACK_1); /* hashtable argument */
  C_class_of();                 /* value1 := (CLASS-OF object) */
  var object clas = value1;
  if (!ht_validp(TheHashtable(ht))) /* hash-table must still be reorganized */
    ht = rehash(ht);
  {
    var uint32 code =           /* calculate hashcode1stable of the class */
      posfixnum_to_V(TheClass(clas)->hashcode);
    var uintL hashindex;
    divu_3232_3232(code,TheHashtable(ht)->ht_size, (void),hashindex = );
    var object kvtable = TheHashtable(ht)->ht_kvtable;
    var gcv_object_t* Nptr =      /* pointer to the current entry */
      &TheSvector(TheHashedAlist(kvtable)->hal_itable)->data[hashindex];
    var gcv_object_t* kvt_data = TheHashedAlist(kvtable)->hal_data;
    while (!eq(*Nptr,nix)) { /* track "list" : "list" finished -> not found */
      var uintL index = posfixnum_to_V(*Nptr); /* next index */
      var gcv_object_t* KVptr = /* pointer to entries in key-value-vector */
        kvt_data + 3*index;
      /* compare key */
      if (eq(KVptr[0],clas)) {
        /* found */
        VALUES2(KVptr[1], T); goto done;
      }
      Nptr = &KVptr[2];         /* pointer to index of next entry */
    }
    /* not found */
    VALUES2(NIL, NIL); /* NIL as the 2nd value */
  }
 done:
  skipSTACK(1);
}

/* (CLOS::CLASS-TUPLE-GETHASH ht object1 ... objectn)
 is like (GETHASH (funcall (hash-tuple-function n) class1 ... classn) ht)
 with classi = (CLASS-OF objecti).
 Definition: n>0, ht is a STABLEHASH-EQUAL-hashtable and
 (hash-tuple-function n) is defined in clos.lisp .
 This function is the core of the dispatch for generic functions. It has to
 be fast and must not cons.

 For 1 < n <= 16,
   (hash-tuple-function n ...) =
   (cons (hash-tuple-function n1 ...) (hash-tuple-function n2 ...)) */
local const uintC tuple_half_1 [17] = {0,0,1,1,2,2,2,3,4,4,4,4,4,5,6,7,8};
local const uintC tuple_half_2 [17] = {0,0,1,2,2,3,4,4,4,5,6,7,8,8,8,8,8};

/* auxiliary function: hashcode of a series of atoms, as if they were
 consed together via (hash-tuple-function n) : */
local uint32 hashcode_tuple (uintC n, const gcv_object_t* args_pointer,
                             uintC depth) {
  if (n==1) {
    var object clas = Next(args_pointer);
    return posfixnum_to_V(TheClass(clas)->hashcode); /* hashcode3stable_atom for classes */
  } else if (n<=16) {
    var uintC n1 = tuple_half_1[n];
    var uintC n2 = tuple_half_2[n]; /* n1 + n2 = n */
    var uint32 code1 = hashcode_tuple(n1,args_pointer,depth+1);
    var uint32 code2 = hashcode_tuple(n2,args_pointer STACKop -(uintP)n1,
                                      depth+1);
    switch (depth) {
      case 0: code1 = rotate_left(16,code1); break;
      case 1: code1 = rotate_left(7,code1); break; /* cf. hashcode3_cons3 */
      case 2: code1 = rotate_left(5,code1); break; /* cf. hashcode3_cons2 */
      case 3: code1 = rotate_left(3,code1); break; /* cf. hashcode3_cons1 */
      default: NOTREACHED;
    }
    return code1 ^ code2;
  } else { /* n>16, depth=0 */
    var uint32 code1 = hashcode_tuple(8,args_pointer,1);
    var uint32 code2 = hashcode_tuple(4,args_pointer STACKop -8,2);
    var uint32 code3 = hashcode_tuple(2,args_pointer STACKop -12,3);
    var uint32 code4 = hashcode_tuple(1,args_pointer STACKop -14,4);
    var uint32 code = 1;                /* cf. hashcode3_cons0 */
    code = rotate_left(3,code4) ^ code; /* cf. hashcode3_cons1 */
    code = rotate_left(5,code3) ^ code; /* cf. hashcode3_cons2 */
    code = rotate_left(7,code2) ^ code; /* cf. hashcode3_cons3 */
    code = rotate_left(16,code1) ^ code;
    return code;
  }
}
/* auxiliary function: Comparison of an object with a series of atoms, as if
 they were consed together via (hash-tuple-function n) : */
local bool equal_tuple (object obj, uintC n, const gcv_object_t* args_pointer) {
  if (n==1) {
    if (eq(obj,Next(args_pointer)))
      return true;
    else
      return false;
  } else if (n<=16) {
    if (consp(obj)) {
      var uintC n1 = tuple_half_1[n];
      var uintC n2 = tuple_half_2[n]; /* n1 + n2 = n */
      if (equal_tuple(Car(obj),n1,args_pointer)
          && equal_tuple(Cdr(obj),n2,args_pointer STACKop -(uintP)n1)
          )
        return true;
    }
    return false;
  } else {                      /* n>16 */
    if (consp(obj) && equal_tuple(Car(obj),8,args_pointer)) {
      obj = Cdr(obj);
      if (consp(obj) && equal_tuple(Car(obj),4,args_pointer STACKop -8)) {
        obj = Cdr(obj);
        if (consp(obj) && equal_tuple(Car(obj),2,args_pointer STACKop -12)) {
          obj = Cdr(obj);
          n-=14; args_pointer skipSTACKop -14;
          /* compare obj with a list of additional atoms: */
          dotimespC(n,n, {
            if (!(consp(obj) && eq(Car(obj),Next(args_pointer))))
              return false;
            obj = Cdr(obj); args_pointer skipSTACKop -1;
          });
          if (nullp(obj))
            /* comparison yields true */
            return true;
        }
      }
    }
    return false;
  }
}

LISPFUN(class_tuple_gethash,seclass_default,2,0,rest,nokey,0,NIL) {
  argcount++; rest_args_pointer skipSTACKop 1; /* arguments: ht {object}+ */
  /* first apply CLASS-OF to each argument: */
  {
    var gcv_object_t* arg_pointer = rest_args_pointer;
    var uintC count;
    dotimespC(count,argcount, {
      pushSTACK(Next(arg_pointer)); C_class_of(); /* (CLASS-OF arg) */
      NEXT(arg_pointer) = value1;                 /* =: arg */
    });
  }
  var object ht = check_hashtable(Before(rest_args_pointer));
  if (!ht_validp(TheHashtable(ht))) /* hash-table must still be reorganized */
    ht = rehash(ht);
  {
    var uint32 code =          /* calculate hashcode of the cons-tree */
      hashcode_tuple(argcount,rest_args_pointer,0);
    var uintL hashindex;
    divu_3232_3232(code,TheHashtable(ht)->ht_size, (void),hashindex = );
    var object kvtable = TheHashtable(ht)->ht_kvtable;
    var gcv_object_t* Nptr =    /* pointer to the current entry */
      &TheSvector(TheHashedAlist(kvtable)->hal_itable)->data[hashindex];
    var gcv_object_t* kvt_data = TheHashedAlist(kvtable)->hal_data;
    while (!eq(*Nptr,nix)) { /* track "list" : "list" finished -> not found */
      var uintL index = posfixnum_to_V(*Nptr); /* next index */
      var gcv_object_t* KVptr = /* pointer to entries in key-value-vector */
        kvt_data + 3*index;
      if (equal_tuple(KVptr[0],argcount,rest_args_pointer)) { /* compare key */
        /* found */
        VALUES1(KVptr[1]); goto fertig; /* Value as value */
      }
      Nptr = &KVptr[2];         /* pointer to index of next entry */
    }
  }
  /* not found */
  VALUES1(NIL);
 fertig:
  set_args_end_pointer(rest_args_pointer STACKop 1); /* clean up STACK */
}

/* UP: Calculates a portable EQUAL-hashcode of an object.
 sxhash(obj)
 It is valid only until the next modification of the object.
 (equal X Y) implies (= (sxhash X) (sxhash Y)).
 > obj: an object
 < result: hashcode, a 32-bit-number */
local uint32 sxhash (object obj);
/* auxiliary functions for known type:
 atom -> fall differentiation by type */
local uint32 sxhash_atom (object obj) {
  #ifdef TYPECODES
  switch (typecode(obj))        /* per type */
  #else
  if (orecordp(obj)) {
    if (Record_type(obj) < rectype_longlimit)
      goto case_orecord;
    else
      goto case_lrecord;
  } else if (consp(obj))
    goto case_cons;
  else if (charp(obj))
    goto case_char;
  else if (fixnump(obj))
    goto case_fixnum;
  else if (short_float_p(obj))
    goto case_sfloat;
  else if (immsubrp(obj))
    goto case_subr;
  else if (machinep(obj))
    goto case_machine;
  else if (small_read_label_p(obj) || systemp(obj))
    goto case_system;
  else switch (0)
  #endif
  {
    case_symbol:                /* symbol */
      /* utilize printname
       (not the home-package, because it is changed on UNINTERN) */
      return hashcode_string(Symbol_name(obj))+0x339B0E4CUL;
    case_cons:
    default:
      /* address may not be used, only utilize the type */
      #ifdef TYPECODES
      return highlow32(typecode(obj),0xDABE); /*typeinfo*2^16+identification*/
      #else
      return highlow32((as_oint(obj)>>oint_type_shift)&(oint_type_mask>>oint_type_shift),0xDABE); /* typeinfo*2^16+identification */
      #endif
    case_bvector:               /* bit-vector */
    case_b2vector:              /* 2bit-vector */
    case_b4vector:              /* 4bit-vector */
    case_b8vector:              /* 8bit-vector */
    case_b16vector:             /* 16bit-vector */
    case_b32vector:             /* 32bit-vector */
      /* bit-vector-content */
      return hashcode_bvector(obj);
    case_string:                /* string */
      /* string-content */
      return hashcode_string(obj);
    case_svector:                                  /* simple-vector */
      /* only utilize the length */
      return Svector_length(obj) + 0x4ECD0A9FUL;
    case_ovector:               /* (vector t) */
    case_mdarray:               /* common array */
      /* multi-dimensional array -> utilize only rank */
      return Iarray_rank(obj) + 0xAAFAFAAEUL;
    case_structure:             /* structure */
      /* utilize only structure-type (Liste (name_1 name_2 ... name_n)) */
      check_SP();
      return sxhash(TheStructure(obj)->structure_types) + 0xAD2CD2AEUL;
    case_stream:                /* stream */
      /* utilize only streamtype */
      return TheStream(obj)->strmtype + 0x3DAEAE55UL;
   {var uint32 bish_code;
    case_closure:               /* closure */
      if (Closure_instancep(obj)) goto instance_only_class;
      /* utilize all elements ?? */
      bish_code = 0xB0DD939EUL; goto record_all;
    case_orecord: {             /* OtherRecord */
      /* utilize record-type, also:
       package: utilize package-name verwerten (not quite OK, as a
                package can be renamed with RENAME-PACKAGE!)
       pathname, byte, loadtimeeval: utilize all components
       hash-table, readtable, random-state, symbol-macro: nothing else */
      var sintB rectype = Record_type(obj);
      switch (rectype) {
        case_Rectype_Symbol_above;
        case_Rectype_bvector_above;
        case_Rectype_b2vector_above;
        case_Rectype_b4vector_above;
        case_Rectype_b8vector_above;
        case_Rectype_b16vector_above;
        case_Rectype_b32vector_above;
        case_Rectype_string_above;
        case_Rectype_Svector_above;
        case_Rectype_ovector_above;
        case_Rectype_mdarray_above;
        case_Rectype_Structure_above;
        case_Rectype_Stream_above;
        case_Rectype_Closure_above;
        case_Rectype_Instance_above;
        case_Rectype_Bignum_above;
        case_Rectype_Ffloat_above;
        case_Rectype_Dfloat_above;
        case_Rectype_Lfloat_above;
        case_Rectype_Ratio_above;
        case_Rectype_Complex_above;
        case_Rectype_Subr_above;
        default: ;
      }
      bish_code = 0xB04D939EUL + rectype;
      switch (rectype) {
        case Rectype_Package: { /* package */
          /* utilize package-name */
          var uint32 next_code = hashcode_string(ThePackage(obj)->pack_name);
          return rotate_left(1,next_code) + bish_code;
        }
        case Rectype_Fsubr:     /* fsubr */
          /* utilize name */
          check_SP(); return sxhash(TheFsubr(obj)->name) + 0xFF3319BAUL;
        case Rectype_Pathname:  /* pathname */
       #ifdef LOGICAL_PATHNAMES
        case Rectype_Logpathname: /* pathname */
       #endif
        case Rectype_Byte:         /* byte */
        case Rectype_Loadtimeeval: /* loadtimeeval */
          goto record_all;
        default:
          return bish_code;
      }
    }
    record_all:
      /* record, in which all elements can be utilized */
      check_SP();
      {
        var gcv_object_t* ptr = &TheRecord(obj)->recdata[0];
        var uintC count = SXrecord_length(obj);
        dotimespC(count,count, {
          /* combine hashcode of the next component: */
          var uint32 next_code = sxhash(*ptr++);
          bish_code = misch(bish_code,next_code);
        });
        return bish_code;
      }
   }
    instance_only_class:
    case_instance: {            /* instance */
      /* utilize only the class */
      var object obj_forwarded = obj;
      instance_un_realloc(obj_forwarded);
      /*instance_update(obj,obj_forwarded); - not needed since we don't access a slot */
      var object cv = TheInstance(obj_forwarded)->inst_class_version;
      var object objclass = TheClassVersion(cv)->cv_newest_class;
      var object objclassname = TheClass(objclass)->classname;
      return sxhash(objclassname) + 0x61EFA249;
    }
    case_lrecord:               /* Long-Record */
      /* utilize record-type and length */
      return 0x8CAA9057UL + (Record_type(obj) << 24) + Lrecord_length(obj);
    case_char:                  /* character */
      /* take EQ-hashcode (for characters EQUAL == EQL == EQ) */
      return hashcode1(obj);
    case_subr:                  /* SUBR */
      /* utilize name */
      check_SP(); return sxhash(TheSubr(obj)->name) + 0xFF3319BAUL;
    case_machine:               /* machine-pointer */
    case_system:                /* frame-pointer, small-read-label, system */
      /* utilize address */
      return hashcode1(obj);
    /* numbers: according to content, like with EQL */
    case_fixnum:                /* fixnum */
      return hashcode_fixnum(obj);
    case_bignum:                /* bignum */
      return hashcode_bignum(obj);
    case_sfloat:                /* short-float */
      return hashcode_sfloat(obj);
    case_ffloat:                /* single-float */
      return hashcode_ffloat(obj);
    case_dfloat:                /* double-float */
      return hashcode_dfloat(obj);
    case_lfloat:                /* Long-Float */
      return hashcode_lfloat(obj);
    case_ratio: {               /* ratio */
      /* hash both components, mix */
      var uint32 code1 = sxhash(TheRatio(obj)->rt_num);
      var uint32 code2 = sxhash(TheRatio(obj)->rt_den);
      return misch(code1,code2);
    }
    case_complex: {             /* complex */
      /* hash both components, mix */
      var uint32 code1 = sxhash(TheComplex(obj)->c_real);
      var uint32 code2 = sxhash(TheComplex(obj)->c_imag);
      return misch(code1,code2);
    }
  }
}
/* cons -> look at content up to depth 4:
 determine the hashcode of the CAR and the hashcode of the CDR at a time
 and combine them shifted. As shifts fit e.g. 16,7,5,3,
 because {0,16} + {0,7} + {0,5} + {0,3}
       = {0,3,5,7,8,10,12,15,16,19,21,23,24,26,28,31}
 consists of 16 different elements of {0,...,31} .
 object, for cons only up to depth 0 */
local uint32 sxhash_cons0 (object obj) {
  if (atomp(obj)) {
    return sxhash_atom(obj);
  } else {                      /* cons -> hashcode := 1 */
    return 1;
  }
}
/* object, for cons only up to depth 1 */
local uint32 sxhash_cons1 (object obj) {
  if (atomp(obj)) {
    return sxhash_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = sxhash_cons0(Car(obj));
    var uint32 code2 = sxhash_cons0(Cdr(obj));
    return rotate_left(3,code1) ^ code2;
  }
}
/* object, for cons only up to depth 2 */
local uint32 sxhash_cons2 (object obj) {
  if (atomp(obj)) {
    return sxhash_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = sxhash_cons1(Car(obj));
    var uint32 code2 = sxhash_cons1(Cdr(obj));
    return rotate_left(5,code1) ^ code2;
  }
}
/* object, for cons only up to depth 3 */
local uint32 sxhash_cons3 (object obj) {
  if (atomp(obj)) {
    return sxhash_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = sxhash_cons2(Car(obj));
    var uint32 code2 = sxhash_cons2(Cdr(obj));
    return rotate_left(7,code1) ^ code2;
  }
}
/* object, for cons only up to depth 4 */
local uint32 sxhash (object obj) {
  if (atomp(obj)) {
    return sxhash_atom(obj);
  } else { /* cons -> determine the hashcode of the CAR and the CDR and mix: */
    var uint32 code1 = sxhash_cons3(Car(obj));
    var uint32 code2 = sxhash_cons3(Cdr(obj));
    return rotate_left(16,code1) ^ code2;
  }
}

LISPFUNN(sxhash,1)
{ /* (SXHASH object), CLTL p. 285 */
  var uint32 sx = sxhash(popSTACK());
  /* ANSI CL (SXHASH doc):
   For any two objects, x and y, both of which are bit vectors,
   characters, conses, numbers, pathnames, strings, or symbols, and which
   are similar, (sxhash x) and (sxhash y) yield the same mathematical
   value even if x and y exist in different Lisp images of the same
   implementation.
   This might be interpreted - assuming that CLISP on Tru64 and CLISP on Win32
   are the same implementations - that (SXHASH (1- (ASH 1 32))) should return
   the same value both on 32-bit platforms (where 4294967295 is a bignum)
   and on 64-bit platforms (where is is a fixnum).
   On 32-bit platforms, hashcode_bignum() is used (returns 3 ==> 3).
   On 64-bit platforms, hashcode_fixnum() is used (returns 4294967175 ==> 135).
   Therefore, limiting ourselves to 24 bits on all platforms
   does not buy us anything anyway. */
#if oint_data_len >= 32
  VALUES1(fixnum(sx));
#elif oint_data_len >= 24
  sx = sx % 0xFFFFFF;
  VALUES1(fixnum(sx));
#else
 #error "sxhash results do not fit in a fixnum"
#endif
}

