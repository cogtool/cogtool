/*
 * Predicates for equality and type tests, types, classes in CLISP
 * Bruno Haible 1990-2005
 * Sam Steingold 1998-2004
 * German comments translated into English: Stefan Kain 2002-09-15
 */

#include "lispbibl.c"
#include "arilev0.c" /* for R_sign */

/* UP: tests for equality of atoms EQL
 eql(obj1,obj2)
 > obj1,obj2: Lisp objects
 < result: true, if objects are eql */
global bool eql (object obj1, object obj2)
{
 start:
  if (eq(obj1,obj2))
    return true; /* (EQ x y) ==> (EQL x y) */
  /* otherwise EQL-equality is only possible, if both are numbers: */
 #ifdef TYPECODES
  if (!(numberp(obj1) && numberp(obj2)))
    return false;
  /* and the type of both has to match: */
  if (typecode(obj1) != typecode(obj2))
    return false;
  switch (typecode(obj1))
 #else
    if (!(orecordp(obj1) && orecordp(obj2)))
      return false;
  if (Record_type(obj1) != Record_type(obj2))
    return false;
  switch (Record_type(obj1)) {
    case_Rectype_Bignum_above;
    case_Rectype_Ratio_above;
    case_Rectype_Complex_above;
    case_Rectype_Ffloat_above;
    case_Rectype_Dfloat_above;
    case_Rectype_Lfloat_above;
    default: goto no;
  }
  switch (0)
 #endif
  {
    case_bignum: { /* Bignums */
      /* compare lengths: */
      var uintC length1 = Bignum_length(obj1);
      if (length1 != Bignum_length(obj2)) goto no;
      /* compare digits: */
      var uintD* ptr1 = &TheBignum(obj1)->data[0];
      var uintD* ptr2 = &TheBignum(obj2)->data[0];
      dotimespC(length1,length1, { if (!(*ptr1++ == *ptr2++)) goto no; });
    }
    return true;
    case_ratio: /* Ratio */
      /* numerator and denominator have to match:
         (and (eql (numerator obj1) (numerator obj2))
              (eql (denominator obj1) (denominator obj2))) */
      if (!eql(TheRatio(obj1)->rt_num,TheRatio(obj2)->rt_num)) goto no;
      /* return eql(TheRatio(obj1)->rt_den,TheRatio(obj2)->rt_den); */
      obj1 = TheRatio(obj1)->rt_den; obj2 = TheRatio(obj2)->rt_den;
      goto start;
    case_complex: /* Complex */
      /* real- and imaginary part have to match:
         (and (eql (realpart obj1) (realpart obj2))
              (eql (imagpart obj1) (imagpart obj2))) */
      if (!eql(TheComplex(obj1)->c_real,TheComplex(obj2)->c_real)) goto no;
      /* return eql(TheComplex(obj1)->c_imag,TheComplex(obj2)->c_imag); */
      obj1 = TheComplex(obj1)->c_imag; obj2 = TheComplex(obj2)->c_imag;
      goto start;
    case_ffloat: /* Single-Floats */
     #ifndef IMMEDIATE_FFLOAT
      if (TheFfloat(obj1)->float_value == TheFfloat(obj2)->float_value)
        return true;
      else
     #endif
        goto no;
    case_dfloat: /* Double-Floats */
     #ifdef intQsize
      if (TheDfloat(obj1)->float_value == TheDfloat(obj2)->float_value)
     #else
      if ((TheDfloat(obj1)->float_value.semhi ==
           TheDfloat(obj2)->float_value.semhi)
          && (TheDfloat(obj1)->float_value.mlo ==
              TheDfloat(obj2)->float_value.mlo))
     #endif
        return true;
      else
        goto no;
    case_lfloat: { /* Long-Floats */
      /* compare lengths: */
      var uintC len1 = Lfloat_length(obj1);
      if (len1 != Lfloat_length(obj2)) goto no;
      /* compare exponents: */
      if (TheLfloat(obj1)->expo != TheLfloat(obj2)->expo) goto no;
      /* compare signs: (LF_sign not usable here.) */
     #ifdef TYPECODES
      if (R_sign(as_object(as_oint(obj1) ^ as_oint(obj2))) < 0) goto no;
     #else
      if (Record_flags(obj1) != Record_flags(obj2)) goto no;
     #endif
      { /* compare digits: */
        var uintD* ptr1 = &TheLfloat(obj1)->data[0];
        var uintD* ptr2 = &TheLfloat(obj2)->data[0];
        dotimespC(len1,len1, { if (!(*ptr1++ == *ptr2++)) goto no; });
      }}
      return true;
      /* case_fixnum: */ /* Fixnums: should already have been EQ */
      /* case_sfloat: */ /* Short-Floats: should already have been EQ */
    default:
      no: return false;
  }
}

/* UP: tests for equality EQUAL
 equal(obj1,obj2)
 > obj1,obj2: Lisp objects
 < result: true, if objects are EQUAL */
global bool equal (object obj1, object obj2)
{
 start:
  if (eql(obj1,obj2))
    return true; /* (EQL x y) ==> (EQUAL x y) */
  /* otherwise EQUAL equality is only possible, if both are structured
     types. Types have to match (including notsimple_bit): */
 #ifdef TYPECODES
  switch (typecode(obj1))
 #else
  if (consp(obj1)) {
    goto case_cons;
  } else if (orecordp(obj1)) {
    goto case_orecord;
  } else
    goto no;
  switch (0)
 #endif
  {
    case_cons: /* compare conses recursively: */
      if (!consp(obj2))
        return false;
      /* CAR and CDR must match:
         (and (equal (car obj1) (car obj2)) (equal (cdr obj1) (cdr obj2))) */
      check_SP();
      if (!equal(Car(obj1),Car(obj2))) goto no;
      /* return equal(Cdr(obj1),Cdr(obj2)); */
      obj1 = Cdr(obj1); obj2 = Cdr(obj2);
      goto start;
    case_sbvector: case_obvector: /* compare bit vectors element-wise: */
      if (!bit_vector_p(Atype_Bit,obj2))
        return false;
      { /* compare lengths: */
        var uintL len1 = vector_length(obj1);
        if (!(len1 == vector_length(obj2))) goto no;
        if (len1 == 0)
          return true;
        { /* compare contents: */
          var uintL index1 = 0;
          var uintL index2 = 0;
          var object sbv1 = array_displace_check(obj1,len1,&index1);
          var object sbv2 = array_displace_check(obj2,len1,&index2);
          /* sbvi is the data vector, indexi the index into the data vector
             for obji (i=1,2). */
          return bit_compare(sbv1,index1,sbv2,index2,len1);
        }
      }
    case_string: /* compare strings element-wise: */
      if (stringp(obj2)) {
        /* compare lengths: */
        var uintL len1 = vector_length(obj1);
        if (len1 != vector_length(obj2)) goto no;
        /* compare content: */
        if (len1 != 0) {
          var uintL index1 = 0;
          var uintL index2 = 0;
          var object ss1 = array_displace_check(obj1,len1,&index1);
          var object ss2 = array_displace_check(obj2,len1,&index2);
          /* ssi is the data vector, indexi the Index into the data vector
             for obji (i=1,2). */
          if (simple_nilarray_p(ss1) || simple_nilarray_p(ss2))
            /* obj1 or obj2 has element type NIL. */
            return eq(ss1,ss2);
          else
            return string_eqcomp(ss1,index1,ss2,index2,len1);
        }
        return true;
      } else {
        return (nil_vector_p(obj2) && vector_length(obj2) == 0
                && vector_length(obj1) == 0);
      }
    case_ovector: /* (VECTOR NIL) is a STRING */
      if ((Iarray_flags(obj1) & arrayflags_atype_mask) == Atype_NIL)
        return (vector_length(obj1) == 0
                && (stringp(obj2) || nil_vector_p(obj2))
                && vector_length(obj2) == 0);
      return false;
    case_orecord:
      switch (Record_type(obj1)) {
        case_Rectype_obvector_above;
        case_Rectype_Sbvector_above;
        case_Rectype_string_above;
        case_Rectype_ovector_above;
        case Rectype_Pathname:
          /* compare pathnames component-wise: */
          if (!pathnamep(obj2)) goto no;
          {
            var gcv_object_t* ptr1 = &TheRecord(obj1)->recdata[0];
            var gcv_object_t* ptr2 = &TheRecord(obj2)->recdata[0];
            var uintC count;
            check_SP();
           #if !defined(PATHNAME_WIN32)
            dotimespC(count,pathname_length, {
              if (!equal(*ptr1++,*ptr2++)) goto no;
            });
           #else /* defined(PATHNAME_WIN32) */
            /* pathname components consist of conses, simple-strings
               and symbols. compare simple-strings case-insensitive: */
            dotimespC(count,pathname_length, {
              if (!equalp(*ptr1++,*ptr2++)) goto no; /* (omits no GC!) */
            });
           #endif
            return true;
          }
       #ifdef LOGICAL_PATHNAMES
        case Rectype_Logpathname:
          /* compare logical pathnames component-wise, too: */
          if (!logpathnamep(obj2)) goto no;
          {
            var gcv_object_t* ptr1 = &TheRecord(obj1)->recdata[0];
            var gcv_object_t* ptr2 = &TheRecord(obj2)->recdata[0];
            var uintC count;
            check_SP();
            dotimespC(count,logpathname_length, {
              if (!equal(*ptr1++,*ptr2++)) goto no;
            });
            return true;
          }
         #endif
        default: goto no;
      }
      /* otherwise, obj1 and obj2 are considered different. */
    default: no:
      return false;
  }
}

/* UP: tests for laxer equality EQUALP
 equalp(obj1,obj2)
 > obj1,obj2: Lisp-objects
 < result: true, if objects are equal */
global bool equalp (object obj1, object obj2);
/* Element-by-element comparisons for various vector types. count > 0. */
local bool elt_compare_T_T (object dv1, uintL index1,
                            object dv2, uintL index2, uintL count);
local bool elt_compare_T_Char (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count);
local bool elt_compare_T_Bit (object dv1, uintL index1,
                              object dv2, uintL index2, uintL count);
local bool elt_compare_T_2Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count);
local bool elt_compare_T_4Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count);
local bool elt_compare_T_8Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count);
local bool elt_compare_T_16Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count);
local bool elt_compare_T_32Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count);
local bool elt_compare_Char_Char (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count);
local bool elt_compare_Bit_Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count);
local bool elt_compare_Bit_2Bit (object dv1, uintL index1,
                                 object dv2, uintL index2, uintL count);
local bool elt_compare_Bit_4Bit (object dv1, uintL index1,
                                 object dv2, uintL index2, uintL count);
local bool elt_compare_Bit_8Bit (object dv1, uintL index1,
                                 object dv2, uintL index2, uintL count);
local bool elt_compare_Bit_16Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count);
local bool elt_compare_Bit_32Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count);
local bool elt_compare_2Bit_2Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count);
local bool elt_compare_2Bit_4Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count);
local bool elt_compare_2Bit_8Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count);
local bool elt_compare_2Bit_16Bit (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count);
local bool elt_compare_2Bit_32Bit (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count);
local bool elt_compare_4Bit_4Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count);
local bool elt_compare_4Bit_8Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count);
local bool elt_compare_4Bit_16Bit (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count);
local bool elt_compare_4Bit_32Bit (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count);
local bool elt_compare_8Bit_8Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count);
local bool elt_compare_8Bit_16Bit (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count);
local bool elt_compare_8Bit_32Bit (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count);
local bool elt_compare_16Bit_16Bit (object dv1, uintL index1,
                                    object dv2, uintL index2, uintL count);
local bool elt_compare_16Bit_32Bit (object dv1, uintL index1,
                                    object dv2, uintL index2, uintL count);
local bool elt_compare_32Bit_32Bit (object dv1, uintL index1,
                                    object dv2, uintL index2, uintL count);
local bool elt_compare (object dv1, uintL index1,
                        object dv2, uintL index2, uintL count);

local bool elt_compare_T_T (object dv1, uintL index1,
                            object dv2, uintL index2, uintL count)
{
  check_SP();
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var const gcv_object_t* ptr2 = &TheSvector(dv2)->data[index2];
  dotimespL(count,count, {
    if (!equalp(*ptr1++,*ptr2++)) goto no;
  });
  return true;
 no: return false;
}
local bool elt_compare_T_Char (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count)
{
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  SstringDispatch(dv2,X, {
    var const cintX* ptr2 = &((SstringX)TheVarobject(dv2))->data[index2];
    dotimespL(count,count, {
      var object elt1 = *ptr1++;
      var chart elt2 = as_chart(*ptr2++);
      if (!(charp(elt1) && chareq(up_case(char_code(elt1)),up_case(elt2))))
        goto no;
    });
  });
  return true;
 no: return false;
}
local bool elt_compare_T_Bit (object dv1, uintL index1,
                              object dv2, uintL index2, uintL count)
{
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var const uintB* ptr2 = &TheSbvector(dv2)->data[index2/8];
  dotimespL(count,count, {
    var object elt1 = *ptr1++;
    var uintB elt2 = (*ptr2 >> ((~index2)%8)) & (bit(1)-1);
    if (!eq(elt1,fixnum(elt2))) goto no;
    index2++;
    ptr2 += ((index2%8)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_T_2Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count)
{
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var const uintB* ptr2 = &TheSbvector(dv2)->data[index2/4];
  dotimespL(count,count, {
    var object elt1 = *ptr1++;
    var uintB elt2 = (*ptr2 >> (2*((~index2)%4))) & (bit(2)-1);
    if (!eq(elt1,fixnum(elt2))) goto no;
    index2++;
    ptr2 += ((index2%4)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_T_4Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count)
{
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var const uintB* ptr2 = &TheSbvector(dv2)->data[index2/2];
  dotimespL(count,count, {
    var object elt1 = *ptr1++;
    var uintB elt2 = (*ptr2 >> (4*((~index2)%2))) & (bit(4)-1);
    if (!eq(elt1,fixnum(elt2))) goto no;
    index2++;
    ptr2 += ((index2%2)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_T_8Bit (object dv1, uintL index1,
                               object dv2, uintL index2, uintL count)
{
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var const uintB* ptr2 = &TheSbvector(dv2)->data[index2];
  dotimespL(count,count, {
    var object elt1 = *ptr1++;
    var uintB elt2 = *ptr2++;
    if (!eq(elt1,fixnum(elt2))) goto no;
  });
  return true;
 no: return false;
}
local bool elt_compare_T_16Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count)
{
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var const uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    var object elt1 = *ptr1++;
    var uint16 elt2 = *ptr2++;
    if (!eq(elt1,fixnum(elt2))) goto no;
  });
  return true;
 no: return false;
}
local bool elt_compare_T_32Bit (object dv1, uintL index1,
                                object dv2, uintL index2, uintL count)
{
  var const gcv_object_t* ptr1 = &TheSvector(dv1)->data[index1];
  var const uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    var object elt1 = *ptr1++;
    var uint32 elt2 = *ptr2++;
    if (!(uint32_p(elt1) && (I_to_uint32(elt1) == elt2)))
      goto no;
  });
  return true;
 no: return false;
}
#define elt_compare_Char_Char(dv1,index1,dv2,index2,count)      \
  string_eqcomp_ci(dv1,index1,dv2,index2,count)
#define elt_compare_Bit_Bit(dv1,index1,dv2,index2,count)        \
  bit_compare(dv1,index1,dv2,index2,count)
local bool elt_compare_Bit_2Bit (object dv1, uintL index1,
                                 object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/8];
  var const uintB* ptr2 = &TheSbvector(dv2)->data[index2/4];
  dotimespL(count,count, {
    var uintB elt1 = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
    var uintB elt2 = (*ptr2 >> (2*((~index2)%4))) & (bit(2)-1);
    if (!(elt1 == elt2)) goto no;
    index1++;
    ptr1 += ((index1%8)==0);
    index2++;
    ptr2 += ((index2%4)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_Bit_4Bit (object dv1, uintL index1,
                                 object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/8];
  var const uintB* ptr2 = &TheSbvector(dv2)->data[index2/2];
  dotimespL(count,count, {
    var uintB elt1 = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
    var uintB elt2 = (*ptr2 >> (4*((~index2)%2))) & (bit(4)-1);
    if (!(elt1 == elt2)) goto no;
    index1++;
    ptr1 += ((index1%8)==0);
    index2++;
    ptr2 += ((index2%2)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_Bit_8Bit (object dv1, uintL index1,
                                 object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/8];
  var const uintB* ptr2 = &TheSbvector(dv2)->data[index2];
  dotimespL(count,count, {
    var uintB elt1 = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
    if (!(elt1 == *ptr2++)) goto no;
    index1++;
    ptr1 += ((index1%8)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_Bit_16Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/8];
  var const uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    var uintB elt1 = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
    if (!(elt1 == *ptr2++)) goto no;
    index1++;
    ptr1 += ((index1%8)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_Bit_32Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/8];
  var const uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    var uintB elt1 = (*ptr1 >> ((~index1)%8)) & (bit(1)-1);
    if (!(elt1 == *ptr2++)) goto no;
    index1++;
    ptr1 += ((index1%8)==0);
  });
  return true;
 no: return false;
}
#define elt_compare_2Bit_2Bit(dv1,index1,dv2,index2,count)      \
  bit_compare(dv1,index1<<1,dv2,index2<<1,count<<1)
local bool elt_compare_2Bit_4Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/4];
  var const uintB* ptr2 = &TheSbvector(dv2)->data[index2/2];
  dotimespL(count,count, {
    var uintB elt1 = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
    var uintB elt2 = (*ptr2 >> (4*((~index2)%2))) & (bit(4)-1);
    if (!(elt1 == elt2)) goto no;
    index1++;
    ptr1 += ((index1%4)==0);
    index2++;
    ptr2 += ((index2%2)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_2Bit_8Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/4];
  var const uintB* ptr2 = &TheSbvector(dv2)->data[index2];
  dotimespL(count,count, {
    var uintB elt1 = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
    if (!(elt1 == *ptr2++)) goto no;
    index1++;
    ptr1 += ((index1%4)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_2Bit_16Bit (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/4];
  var const uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    var uintB elt1 = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
    if (!(elt1 == *ptr2++)) goto no;
    index1++;
    ptr1 += ((index1%4)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_2Bit_32Bit (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/4];
  var const uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    var uintB elt1 = (*ptr1 >> (2*((~index1)%4))) & (bit(2)-1);
    if (!(elt1 == *ptr2++)) goto no;
    index1++;
    ptr1 += ((index1%4)==0);
  });
  return true;
 no: return false;
}
#define elt_compare_4Bit_4Bit(dv1,index1,dv2,index2,count)      \
  bit_compare(dv1,index1<<2,dv2,index2<<2,count<<2)
local bool elt_compare_4Bit_8Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/2];
  var const uintB* ptr2 = &TheSbvector(dv2)->data[index2];
  dotimespL(count,count, {
    var uintB elt1 = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
    if (!(elt1 == *ptr2++)) goto no;
    index1++;
    ptr1 += ((index1%2)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_4Bit_16Bit (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/2];
  var const uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    var uintB elt1 = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
    if (!(elt1 == *ptr2++)) goto no;
    index1++;
    ptr1 += ((index1%2)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_4Bit_32Bit (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1/2];
  var const uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    var uintB elt1 = (*ptr1 >> (4*((~index1)%2))) & (bit(4)-1);
    if (!(elt1 == *ptr2++)) goto no;
    index1++;
    ptr1 += ((index1%2)==0);
  });
  return true;
 no: return false;
}
local bool elt_compare_8Bit_8Bit (object dv1, uintL index1,
                                  object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1];
  var const uintB* ptr2 = &TheSbvector(dv2)->data[index2];
  dotimespL(count,count, {
    if (!(*ptr1++ == *ptr2++)) goto no;
  });
  return true;
 no: return false;
}
local bool elt_compare_8Bit_16Bit (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1];
  var const uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    if (!(*ptr1++ == *ptr2++)) goto no;
  });
  return true;
 no: return false;
}
local bool elt_compare_8Bit_32Bit (object dv1, uintL index1,
                                   object dv2, uintL index2, uintL count)
{
  var const uintB* ptr1 = &TheSbvector(dv1)->data[index1];
  var const uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    if (!(*ptr1++ == *ptr2++)) goto no;
  });
  return true;
 no: return false;
}
local bool elt_compare_16Bit_16Bit (object dv1, uintL index1,
                                    object dv2, uintL index2, uintL count)
{
  var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
  var const uint16* ptr2 = &((uint16*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    if (!(*ptr1++ == *ptr2++)) goto no;
  });
  return true;
 no: return false;
}
local bool elt_compare_16Bit_32Bit (object dv1, uintL index1,
                                    object dv2, uintL index2, uintL count)
{
  var const uint16* ptr1 = &((uint16*)&TheSbvector(dv1)->data[0])[index1];
  var const uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    if (!(*ptr1++ == *ptr2++)) goto no;
  });
  return true;
 no: return false;
}
local bool elt_compare_32Bit_32Bit (object dv1, uintL index1,
                                    object dv2, uintL index2, uintL count)
{
  var const uint32* ptr1 = &((uint32*)&TheSbvector(dv1)->data[0])[index1];
  var const uint32* ptr2 = &((uint32*)&TheSbvector(dv2)->data[0])[index2];
  dotimespL(count,count, {
    if (!(*ptr1++ == *ptr2++)) goto no;
  });
  return true;
 no: return false;
}
local bool elt_compare (object dv1, uintL index1,
                        object dv2, uintL index2, uintL count)
{
  switch (Array_type(dv1)) {
    case Array_type_svector: /* Simple-Vector */
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          return elt_compare_T_T(dv1,index1,dv2,index2,count);
        case Array_type_sbvector: /* Simple-Bit-Vector */
          return elt_compare_T_Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb2vector:
          return elt_compare_T_2Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb4vector:
          return elt_compare_T_4Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb8vector:
          return elt_compare_T_8Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb16vector:
          return elt_compare_T_16Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb32vector:
          return elt_compare_T_32Bit(dv1,index1,dv2,index2,count);
        case Array_type_sstring: /* Simple-String */
          return elt_compare_T_Char(dv1,index1,dv2,index2,count);
        case Array_type_snilvector: /* (VECTOR NIL) */
          /* One can argue that comparing nonexistent elements should yield
             an error, not false. */
          /*fehler_nilarray_retrieve();*/
          return false;
        default: NOTREACHED;
      }
    case Array_type_sbvector: /* Simple-Bit-Vector */
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          return elt_compare_T_Bit(dv2,index2,dv1,index1,count);
        case Array_type_sbvector: /* Simple-Bit-Vector */
          return elt_compare_Bit_Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb2vector:
          return elt_compare_Bit_2Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb4vector:
          return elt_compare_Bit_4Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb8vector:
          return elt_compare_Bit_8Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb16vector:
          return elt_compare_Bit_16Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb32vector:
          return elt_compare_Bit_32Bit(dv1,index1,dv2,index2,count);
        case Array_type_sstring: /* Simple-String */
          return false; /* because count > 0 */
        case Array_type_snilvector: /* (VECTOR NIL) */
          /* One can argue that comparing nonexistent elements should yield
             an error, not false. */
          /*fehler_nilarray_retrieve();*/
          return false;
        default: NOTREACHED;
      }
    case Array_type_sb2vector:
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          return elt_compare_T_2Bit(dv2,index2,dv1,index1,count);
        case Array_type_sbvector: /* Simple-Bit-Vector */
          return elt_compare_Bit_2Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb2vector:
          return elt_compare_2Bit_2Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb4vector:
          return elt_compare_2Bit_4Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb8vector:
          return elt_compare_2Bit_8Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb16vector:
          return elt_compare_2Bit_16Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb32vector:
          return elt_compare_2Bit_32Bit(dv1,index1,dv2,index2,count);
        case Array_type_sstring: /* Simple-String */
          return false; /* because count > 0 */
        case Array_type_snilvector: /* (VECTOR NIL) */
          /* One can argue that comparing nonexistent elements should yield
             an error, not false. */
          /*fehler_nilarray_retrieve();*/
          return false;
        default: NOTREACHED;
      }
    case Array_type_sb4vector:
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          return elt_compare_T_4Bit(dv2,index2,dv1,index1,count);
        case Array_type_sbvector: /* Simple-Bit-Vector */
          return elt_compare_Bit_4Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb2vector:
          return elt_compare_2Bit_4Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb4vector:
          return elt_compare_4Bit_4Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb8vector:
          return elt_compare_4Bit_8Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb16vector:
          return elt_compare_4Bit_16Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb32vector:
          return elt_compare_4Bit_32Bit(dv1,index1,dv2,index2,count);
        case Array_type_sstring: /* Simple-String */
          return false; /* because count > 0 */
        case Array_type_snilvector: /* (VECTOR NIL) */
          /* One can argue that comparing nonexistent elements should yield
             an error, not false. */
          /*fehler_nilarray_retrieve();*/
          return false;
        default: NOTREACHED;
      }
    case Array_type_sb8vector:
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          return elt_compare_T_8Bit(dv2,index2,dv1,index1,count);
        case Array_type_sbvector: /* Simple-Bit-Vector */
          return elt_compare_Bit_8Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb2vector:
          return elt_compare_2Bit_8Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb4vector:
          return elt_compare_4Bit_8Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb8vector:
          return elt_compare_8Bit_8Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb16vector:
          return elt_compare_8Bit_16Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb32vector:
          return elt_compare_8Bit_32Bit(dv1,index1,dv2,index2,count);
        case Array_type_sstring: /* Simple-String */
          return false; /* because count > 0 */
        case Array_type_snilvector: /* (VECTOR NIL) */
          /* One can argue that comparing nonexistent elements should yield
             an error, not false. */
          /*fehler_nilarray_retrieve();*/
          return false;
        default: NOTREACHED;
      }
    case Array_type_sb16vector:
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          return elt_compare_T_16Bit(dv2,index2,dv1,index1,count);
        case Array_type_sbvector: /* Simple-Bit-Vector */
          return elt_compare_Bit_16Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb2vector:
          return elt_compare_2Bit_16Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb4vector:
          return elt_compare_4Bit_16Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb8vector:
          return elt_compare_8Bit_16Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb16vector:
          return elt_compare_16Bit_16Bit(dv1,index1,dv2,index2,count);
        case Array_type_sb32vector:
          return elt_compare_16Bit_32Bit(dv1,index1,dv2,index2,count);
        case Array_type_sstring: /* Simple-String */
          return false; /* because count > 0 */
        case Array_type_snilvector: /* (VECTOR NIL) */
          /* One can argue that comparing nonexistent elements should yield
             an error, not false. */
          /*fehler_nilarray_retrieve();*/
          return false;
        default: NOTREACHED;
      }
    case Array_type_sb32vector:
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          return elt_compare_T_32Bit(dv2,index2,dv1,index1,count);
        case Array_type_sbvector: /* Simple-Bit-Vector */
          return elt_compare_Bit_32Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb2vector:
          return elt_compare_2Bit_32Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb4vector:
          return elt_compare_4Bit_32Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb8vector:
          return elt_compare_8Bit_32Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb16vector:
          return elt_compare_16Bit_32Bit(dv2,index2,dv1,index1,count);
        case Array_type_sb32vector:
          return elt_compare_32Bit_32Bit(dv1,index1,dv2,index2,count);
        case Array_type_sstring: /* Simple-String */
          return false; /* because count > 0 */
        case Array_type_snilvector: /* (VECTOR NIL) */
          /* One can argue that comparing nonexistent elements should yield
             an error, not false. */
          /*fehler_nilarray_retrieve();*/
          return false;
        default: NOTREACHED;
      }
    case Array_type_sstring: /* Simple-String */
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
          return elt_compare_T_Char(dv2,index2,dv1,index1,count);
        case Array_type_sbvector: /* Simple-Bit-Vector */
        case Array_type_sb2vector:
        case Array_type_sb4vector:
        case Array_type_sb8vector:
        case Array_type_sb16vector:
        case Array_type_sb32vector:
          return false; /* because count > 0 */
        case Array_type_sstring: /* Simple-String */
          return elt_compare_Char_Char(dv1,index1,dv2,index2,count);
        case Array_type_snilvector: /* (VECTOR NIL) */
          /* One can argue that comparing nonexistent elements should yield
             an error, not false. */
          /*fehler_nilarray_retrieve();*/
          return false;
        default: NOTREACHED;
      }
    case Array_type_snilvector: /* (VECTOR NIL) */
      switch (Array_type(dv2)) {
        case Array_type_svector: /* Simple-Vector */
        case Array_type_sbvector: /* Simple-Bit-Vector */
        case Array_type_sb2vector:
        case Array_type_sb4vector:
        case Array_type_sb8vector:
        case Array_type_sb16vector:
        case Array_type_sb32vector:
        case Array_type_sstring: /* Simple-String */
          /* One can argue that comparing nonexistent elements should yield
             an error, not false. */
          /*fehler_nilarray_retrieve();*/
          return false;
        case Array_type_snilvector: /* (VECTOR NIL) */
          /* One can argue that comparing nonexistent elements should yield
             an error, not true. But OTOH, we want (equalp (copy-seq x) x)
             to return true without signalling an error. */
          return true;
        default: NOTREACHED;
      }
    default: NOTREACHED;
  }
}
/* test for hash table equality under EQUALP:
   <http://www.lisp.org/HyperSpec/Body/fun_equalp.html>
 EQUALP descends hash-tables by first comparing the count of entries and
 the :test function; if those are the same, it compares the keys of the
 tables using the :test function and then the values of the matching
 keys using equalp recursively.
 However, hash-tables with user-defined test are not compared this way;
 they are EQUALP only if they are EQ. The reason is that a user-defined test
 can trigger GC, thus the entry-by-entry comparison of hash-tables with user-
 defined test can trigger GC. The consequences would be bad: 1. EQUALP could
 trigger GC. 2. When a hash table table with EQUALP test has two keys which
 are hash-tables with user-defined test and another key with a non-GC-invariant
 hash code, a simple hash table lookup could trigger GC, which would
 invalidate the hash table, which would require a rehash of the hash table,
 which would again call EQUALP on the elements, etc. - endless recursion. */
local bool hash_table_equalp (object ht1, object ht2)
{
  var uintB flags1 = ht_test_code(record_flags(TheHashtable(ht1)));
  var uintB flags2 = ht_test_code(record_flags(TheHashtable(ht2)));
  /* Not same built-in test or a user-defined test? */
  if (flags1 != flags2
      || ht_test_code_user_p(flags1) /* || ht_test_code_user_p(flags2) */)
    return false;
  if (!eq(TheHashedAlist(TheHashtable(ht1)->ht_kvtable)->hal_count,
          TheHashedAlist(TheHashtable(ht2)->ht_kvtable)->hal_count))
    return false;
  if (!eq(hash_table_weak_type(ht1),hash_table_weak_type(ht2)))
    return false;
  /* have to traverse keys */
  { # Look whether ht1 is contained in ht2.
    var uintL index = posfixnum_to_V(TheHashtable(ht1)->ht_maxcount);
    var gcv_object_t* KVptr = TheHashedAlist(TheHashtable(ht1)->ht_kvtable)->hal_data;
    for (; index > 0; KVptr += 3, index--)
      if (!eq(KVptr[0],unbound)) {
        var object value_in_ht2 = gethash(KVptr[0],ht2,false);
        if (eq(value_in_ht2,nullobj) || !equalp(KVptr[1],value_in_ht2))
          return false;
      }
  }
  { # Look whether ht2 is contained in ht1.
    var uintL index = posfixnum_to_V(TheHashtable(ht2)->ht_maxcount);
    var gcv_object_t* KVptr = TheHashedAlist(TheHashtable(ht2)->ht_kvtable)->hal_data;
    for (; index > 0; KVptr += 3, index--)
      if (!eq(KVptr[0],unbound)) {
        var object value_in_ht1 = gethash(KVptr[0],ht1,false);
        if (eq(value_in_ht1,nullobj) || !equalp(KVptr[1],value_in_ht1))
          return false;
      }
  }
  return true;
}
/* Now EQUALP itself. */
global bool equalp (object obj1, object obj2)
{
 start:
  if (eq(obj1,obj2))
    return true; /* (EQ x y) ==> (EQUALP x y) */
  /* different cases according to the type of obj1: */
  if (consp(obj1)) {
    if (!consp(obj2)) return false;
    /* compare conses recursively:
       CAR and CDR must match:
       (and (equalp (car obj1) (car obj2)) (equalp (cdr obj1) (cdr obj2))) */
    check_SP();
    if (!equalp(Car(obj1),Car(obj2))) return false;
    /* return equalp(Cdr(obj1),Cdr(obj2)); */
    obj1 = Cdr(obj1); obj2 = Cdr(obj2);
    goto start;
  } else if (symbolp(obj1)) { /* Symbol ? */
    return false; /* yes -> should already have beend EQ to obj2 */
  } else if (numberp(obj1)) {
    if (!numberp(obj2)) return false;
    /* compare numbers via = */
    return number_gleich(obj1,obj2);
  } else {
   #ifdef TYPECODES
    switch (typecode(obj1))
   #else
    if (orecordp(obj1)) {
      if (Record_type(obj1) < rectype_longlimit)
        goto case_orecord;
      else
        goto case_lrecord;
    } else if (charp(obj1)) {
      goto case_char;
    } else
      return false;
    switch (0)
   #endif
    {
      case_bvector: /* bit-vector */
      case_b2vector: /* 2bit-vector */
      case_b4vector: /* 4bit-vector */
      case_b8vector: /* 8bit-vector */
      case_b16vector: /* 16bit-vector */
      case_b32vector: /* 32bit-vector */
      case_string: /* string */
      case_vector: /* (VECTOR T) */
        if (!vectorp(obj2)) return false;
        /* obj1, obj2 both vectors. */
        { /* compare lengths: */
          var uintL len1 = vector_length(obj1);
          if (len1 != vector_length(obj2)) return false;
          /* compare lengths: */
          if (len1 > 0) {
            var uintL index1 = 0; /* start-index into obj1 data vector */
            var uintL index2 = 0; /* start-index into obj2 data vector */
            var object dv1 = array_displace_check(obj1,len1,&index1);
            var object dv2 = array_displace_check(obj2,len1,&index2);
            /* dvi is the data vector, indexi the index into the Data vector
               for obji (i=1,2). */
            return elt_compare(dv1,index1,dv2,index2,len1);
          } else
            return true;
        }
      case_mdarray: /* array of rank /=1 */
        if (!mdarrayp(obj2)) return false;
        /* obj1 and obj2 are arrays of rank /=1.
           Their rank and their dimensions have to match, and
           the elements are then compared like with vectors. */
        { /* compare ranks: */
          var uintC rank1 = Iarray_rank(obj1);
          if (!(rank1 == Iarray_rank(obj2))) return false;
          /* compare dimensions: */
          if (rank1 > 0) {
            var uintL* dimptr1 = &TheIarray(obj1)->dims[0];
            if (Iarray_flags(obj1) & bit(arrayflags_dispoffset_bit))
              dimptr1++;
            var uintL* dimptr2 = &TheIarray(obj2)->dims[0];
            if (Iarray_flags(obj2) & bit(arrayflags_dispoffset_bit))
              dimptr2++;
            dotimespC(rank1,rank1, {
              if (!(*dimptr1++ == *dimptr2++)) return false;
            });
          }
        }
        { /* compare content: */
          var uintL len1 = TheIarray(obj1)->totalsize;
          /* as product of the dimensions,
             it has to be = TheIarray(obj2)->totalsize */
          if (len1 > 0) {
            var uintL index1 = 0; var uintL index2 = 0;
            var object dv1 = iarray_displace_check(obj1,len1,&index1);
            var object dv2 = iarray_displace_check(obj2,len1,&index2);
            /* dvi is the data vector, indexi the index into the data vector
               for obji (i=1,2). */
            return elt_compare(dv1,index1,dv2,index2,len1);
          } else
            return true;
        }
       #ifdef TYPECODES
        _case_structure
        _case_stream
       #endif
        case_orecord:
          /* record, structure, but not closure, instance.
             obj2 must have the same type as obj1, which is Record, and
             has to match in rectype and recflags and reclength with obj1.
             And all components have to be EQUALP. */
          switch (Record_type(obj1)) {
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
            case Rectype_Hashtable:
              if (!hash_table_p(obj2)) return false;
              return hash_table_equalp(obj1,obj2);
            default: ;
          }
         #ifdef TYPECODES
          if (typecode(obj1) != typecode(obj2)) return false;
         #else
          if (!orecordp(obj2)) return false;
         #endif
          { /* obj1 and obj2 both records. */
            var uintC len;
            if (Record_type(obj1) != Record_type(obj2)) return false;
            if (Record_flags(obj1) != Record_flags(obj2)) return false;
            if (Record_type(obj1) < rectype_limit) {
              if ((len=Srecord_length(obj1)) != Srecord_length(obj2)) return false;
            } else {
              if ((len=Xrecord_length(obj1)) != Xrecord_length(obj2)) return false;
              if (Xrecord_xlength(obj1) != Xrecord_xlength(obj2)) return false;
            }
            /* compare the elements recursively (also for PATHNAMEs): */
            check_SP();
            if (len > 0) {
              var gcv_object_t* ptr1 = &TheRecord(obj1)->recdata[0];
              var gcv_object_t* ptr2 = &TheRecord(obj2)->recdata[0];
              var uintC count;
              dotimespC(count,len, {
                if (!equalp(*ptr1++,*ptr2++)) return false;
              });
            }
            /* compare the recxlength extra-elements, too: */
            if (Record_type(obj1) >= rectype_limit) {
              var uintC xlen = Xrecord_xlength(obj1);
              if (xlen > 0) {
                var uintB* ptr1 = (uintB*)&TheRecord(obj1)->recdata[len];
                var uintB* ptr2 = (uintB*)&TheRecord(obj2)->recdata[len];
                dotimespC(xlen,xlen, { if (!(*ptr1++ == *ptr2++)) return false; } );
              }
            }
          }
          return true;
        case_lrecord:
          return false; /* should already have been EQ */
        case_closure: /* closure */
          return false; /* should already have been EQ */
        case_instance: /* instance */
          return false; /* should already have been EQ */
        case_char: /* character */
          if (!charp(obj2)) return false;
          /* obj1, obj2 both characters.
             comparison alike to CHAR-EQUAL: ignore bits and font,
             convert into upper case letters and then compare. */
          if (chareq(up_case(char_code(obj1)),up_case(char_code(obj2))))
            return true;
          else
            return false;
       #ifdef TYPECODES
        case_subr: /* SUBR */
          return false; /* should already have been EQ */
        case_system: /* SYSTEM, small-read-label, FRAME-pointer */
          return false; /* should already have been EQ */
        case_machine: /* machine pointer */
          return false; /* should already have been EQ */
       #endif
        default: NOTREACHED;
    }
  }
}

LISPFUNNF(eq,2)
{ /* (EQ obj1 obj2), CLTL p. 77 */
  VALUES_IF(eq(STACK_0,STACK_1)); skipSTACK(2);
}

LISPFUNNF(eql,2)
{ /* (EQL obj1 obj2), CLTL p. 78 */
  VALUES_IF(eql(STACK_0,STACK_1)); skipSTACK(2);
}

LISPFUNNR(equal,2)
{ /* (EQUAL obj1 obj2), CLTL p. 80 */
  VALUES_IF(equal(STACK_0,STACK_1)); skipSTACK(2);
}

LISPFUNNR(equalp,2)
{ /* (EQUALP obj1 obj2), CLTL p. 81 */
  VALUES_IF(equalp(STACK_0,STACK_1)); skipSTACK(2);
}

LISPFUNNF(consp,1)
{ /* (CONSP object), CLTL p. 74 */
  VALUES_IF(mconsp(STACK_0)); skipSTACK(1);
}

LISPFUNNF(atom,1)
{ /* (ATOM object), CLTL p. 73 */
  VALUES_IF(matomp(STACK_0)); skipSTACK(1);
}

LISPFUNNF(symbolp,1)
{ /* (SYMBOLP object), CLTL p. 73 */
  VALUES_IF(symbolp(STACK_0)); skipSTACK(1);
}

LISPFUNNF(stringp,1)
{ /* (STRINGP object), CLTL p. 75 */
  VALUES_IF(stringp(STACK_0)); skipSTACK(1);
}

LISPFUNNF(numberp,1)
{ /* (NUMBERP object), CLTL p. 74 */
  VALUES_IF(numberp(STACK_0)); skipSTACK(1);
}

LISPFUNNR(compiled_function_p,1) {
  /* (COMPILED-FUNCTION-P object), CLTL p. 76 */
  var object arg = popSTACK();
  /* check for SUBR or compiled closure (excluding funcallable instances) or
     foreign function: */
  VALUES_IF(subrp(arg)
            || (cclosurep(arg) && !Closure_instancep(arg))
            || ffunctionp(arg));
}

LISPFUNNR(pcompiled_function_p,1) {
  /* (SYS::%COMPILED-FUNCTION-P object) */
  var object arg = popSTACK();
  /* check for SUBR or compiled closure or foreign function: */
  VALUES_IF(subrp(arg) || cclosurep(arg) || ffunctionp(arg));
}

LISPFUNNF(null,1)
{ /* (NULL object), CLTL p. 73 */
  VALUES_IF(nullp(STACK_0)); skipSTACK(1);
}

LISPFUNNF(not,1)
{ /* (NOT object), CLTL p. 82 */
  VALUES_IF(nullp(STACK_0)); skipSTACK(1);
}

LISPFUNNF(closurep,1)
{ /* (SYS::CLOSUREP object) */
  VALUES_IF(closurep(STACK_0)); skipSTACK(1);
}

LISPFUNNF(listp,1)
{ /* (LISTP object), CLTL p. 74 */
  VALUES_IF(listp(STACK_0)); skipSTACK(1);
}

/* (EXT:PROPER-LIST-P object) returns true if the object is a proper list,
   i.e. a list which is neither dotted nor circular, i.e. a list which ends
   in NIL. */
LISPFUNNR(proper_list_p,1)
{
  VALUES_IF(proper_list_p(STACK_0)); skipSTACK(1);
}

LISPFUNNF(integerp,1)
{ /* (INTEGERP object), CLTL p. 74 */
  VALUES_IF(integerp(STACK_0)); skipSTACK(1);
}

LISPFUNNF(fixnump,1)
{ /* (SYS::FIXNUMP object) */
  VALUES_IF(fixnump(STACK_0)); skipSTACK(1);
}

LISPFUNNF(rationalp,1)
{ /* (RATIONALP object), CLTL p. 74 */
  var object arg = popSTACK();
  if_rationalp(arg, { VALUES1(T); }, { VALUES1(NIL); } );
}

LISPFUNNF(floatp,1)
{ /* (FLOATP object), CLTL p. 75 */
  VALUES_IF(floatp(STACK_0)); skipSTACK(1);
}

LISPFUNNF(short_float_p,1)
{ /* (SYS::SHORT-FLOAT-P object) */
  VALUES_IF(short_float_p(STACK_0)); skipSTACK(1);
}

LISPFUNNF(single_float_p,1)
{ /* (SYS::SINGLE-FLOAT-P object) */
  VALUES_IF(single_float_p(STACK_0)); skipSTACK(1);
}

LISPFUNNF(double_float_p,1)
{ /* (SYS::DOUBLE-FLOAT-P object) */
  VALUES_IF(double_float_p(STACK_0)); skipSTACK(1);
}

LISPFUNNF(long_float_p,1)
{ /* (SYS::LONG-FLOAT-P object) */
  VALUES_IF(long_float_p(STACK_0)); skipSTACK(1);
}

LISPFUNNF(realp,1)
{ /* (REALP object), CLTL2 p. 101 */
  var object arg = popSTACK();
  if_realp(arg, { VALUES1(T); } , { VALUES1(NIL); } );
}

LISPFUNNF(complexp,1)
{ /* (COMPLEXP object), CLTL p. 75 */
  VALUES_IF(complexp(STACK_0)); skipSTACK(1);
}

/* (STREAMP object), CLTL p. 332 */
/* Not seclass_foldable, because of Gray streams and CHANGE-CLASS. */
LISPFUNNR(streamp,1)
{
  VALUES_IF(streamp(STACK_0)); skipSTACK(1);
}

LISPFUNNF(built_in_stream_p,1)
{ /* (SYS::BUILT-IN-STREAM-P object) */
  VALUES_IF(builtin_stream_p(STACK_0)); skipSTACK(1);
}

LISPFUNNF(random_state_p,1)
{ /* (RANDOM-STATE-P object), CLTL p. 231 */
  VALUES_IF(random_state_p(STACK_0)); skipSTACK(1);
}

LISPFUNNF(readtablep,1)
{ /* (READTABLEP object), CLTL p. 361 */
  VALUES_IF(readtablep(STACK_0)); skipSTACK(1);
}

LISPFUNNF(hash_table_p,1)
{ /* (HASH-TABLE-P object), CLTL p. 284 */
  VALUES_IF(hash_table_p(STACK_0)); skipSTACK(1);
}

LISPFUNNF(pathnamep,1)
{ /* (PATHNAMEP object), CLTL p. 416 */
  VALUES_IF(xpathnamep(STACK_0)); skipSTACK(1);
}

LISPFUNNF(logical_pathname_p,1)
{ /* (SYS::LOGICAL-PATHNAME-P object) */
  VALUES_IF(logpathnamep(STACK_0)); skipSTACK(1);
}

LISPFUNNF(characterp,1)
{ /* (CHARACTERP object), CLTL p. 75 */
  VALUES_IF(charp(STACK_0)); skipSTACK(1);
}

LISPFUNNF(functionp,1)
{ /* (FUNCTIONP object), CLTL p. 76, CLtL2 p. 102-103 */
  var object arg = popSTACK();
  /* SUBR, closure, foreign function, [Symbol, Cons (LAMBDA . ...)]: */
  VALUES_IF(subrp(arg) || closurep(arg) || ffunctionp(arg));
}

LISPFUNNF(packagep,1)
{ /* (PACKAGEP object), CLTL p. 76 */
  VALUES_IF(packagep(STACK_0)); skipSTACK(1);
}

LISPFUNNF(arrayp,1)
{ /* (ARRAYP object), CLTL p. 76 */
  VALUES_IF(arrayp(STACK_0)); skipSTACK(1);
}

LISPFUNNF(simple_array_p,1)
{ /* (SYSTEM::SIMPLE-ARRAY-P object) */
  var object arg = popSTACK();
  VALUES_IF(simplep(arg)
            || (arrayp(arg) /* other arrays, only if all flag bits = 0 */
                && ((Iarray_flags(arg)
                     & (bit(arrayflags_adjustable_bit)
                        | bit(arrayflags_fillp_bit)
                        | bit(arrayflags_displaced_bit)
                        | bit(arrayflags_dispoffset_bit) ))
                    == 0)));
}

LISPFUNNF(bit_vector_p,1)
{ /* (BIT-VECTOR-P object), CLTL p. 75 */
  VALUES_IF(bit_vector_p(Atype_Bit,STACK_0)); skipSTACK(1);
}

LISPFUNNF(vectorp,1)
{ /* (VECTORP object), CLTL p. 75 */
  VALUES_IF(vectorp(STACK_0)); skipSTACK(1);
}

LISPFUNNF(simple_vector_p,1)
{ /* (SIMPLE-VECTOR-P object), CLTL p. 75 */
  VALUES_IF(simple_vector_p(STACK_0)); skipSTACK(1);
}

LISPFUNNF(simple_string_p,1)
{ /* (SIMPLE-STRING-P object), CLTL p. 75 */
  var object arg = popSTACK();
  VALUES_IF(simple_string_p(arg)
            || (stringp(arg)
                && ((Iarray_flags(arg)
                     & (bit(arrayflags_adjustable_bit)
                        | bit(arrayflags_fillp_bit)
                        | bit(arrayflags_displaced_bit)
                        | bit(arrayflags_dispoffset_bit) ))
                    == 0)));
}

LISPFUNNF(simple_bit_vector_p,1)
{ /* (SIMPLE-BIT-VECTOR-P object), CLTL p. 76 */
  VALUES_IF(simple_bit_vector_p(Atype_Bit,STACK_0)); skipSTACK(1);
}

LISPFUNNR(type_of,1)
{ /* (TYPE-OF object), CLTL p. 52 */
  var object arg = popSTACK();
 #ifdef TYPECODES
  switch (typecode(arg))
 #else
  if (orecordp(arg)) {
    goto case_orecord;
  } else if (consp(arg)) {
    goto case_cons;
  } else if (immsubrp(arg)) {
    goto case_subr;
  } else if (charp(arg)) {
    goto case_char;
  } else if (fixnump(arg)) {
    goto case_fixnum;
  } else if (short_float_p(arg)) {
    goto case_sfloat;
  } else if (machinep(arg)) {
    goto case_machine;
  } else if (small_read_label_p(arg)) {
    goto case_small_read_label;
  } else if (systemp(arg)) {
    goto case_system;
  } else
    goto unknown;
  switch (0)
 #endif
  {
    case_cons: /* Cons -> CONS */
      value1 = S(cons); break;
    case_symbol: /* Symbol -> SYMBOL or NULL or BOOLEAN or KEYWORD */
      value1 = (nullp(arg) ? S(null) :
                eq(arg,T) ? S(boolean) :
                eq(Symbol_package(arg),O(keyword_package)) ? S(keyword) :
                S(symbol));
      break;
    case_machine: /* machine pointer -> ADDRESS */
        /* (If not TYPECODES, ADDRESS and FRAME-POINTER
           are not distinguishable.) */
      value1 = S(address); break;
    case_sbvector: /* Simple-Bit-Vector -> (SIMPLE-BIT-VECTOR dim0) */
      pushSTACK(S(simple_bit_vector)); goto vectors;
    case_obvector: /* Bit-Vector -> (BIT-VECTOR dim0) */
      pushSTACK(S(bit_vector)); goto vectors;
    case_sstring: /* Simple-String -> (SIMPLE-[BASE-]STRING dim0) */
     #if (base_char_code_limit == char_code_limit)
      pushSTACK(S(simple_base_string)); goto vectors;
     #else
      pushSTACK(S(simple_string)); goto vectors;
     #endif
    case_svector: /* Simple-Vector -> (SIMPLE-VECTOR dim0) */
      pushSTACK(S(simple_vector)); goto vectors;
    case_ostring: /* other string */
      /* -> ([BASE-]STRING dim0) or (VECTOR NIL dim0) or (SIMPLE-ARRAY NIL (dim0)) */
      {
        var bool simple =
          ((Iarray_flags(arg)
            & (bit(arrayflags_adjustable_bit)
               | bit(arrayflags_fillp_bit)
               | bit(arrayflags_displaced_bit)
               | bit(arrayflags_dispoffset_bit) ))
           == 0);
        switch (Iarray_flags(arg) & arrayflags_atype_mask) {
          case Atype_NIL:
            pushSTACK(array_dimensions(arg)); /* list of dimensions */
            if (simple) {
              {
                var object new_cons = allocate_cons();
                Cdr(new_cons) = NIL; Car(new_cons) = popSTACK();
                pushSTACK(new_cons);
              }
              {
                var object new_cons = allocate_cons();
                Cdr(new_cons) = popSTACK(); Car(new_cons) = NIL;
                pushSTACK(new_cons);
              }
              {
                var object new_cons = allocate_cons();
                Cdr(new_cons) = popSTACK(); Car(new_cons) = S(simple_array);
                value1 = new_cons;
              }
            } else {
              {
                var object new_cons = allocate_cons();
                Cdr(new_cons) = popSTACK(); Car(new_cons) = NIL;
                pushSTACK(new_cons);
              }
              {
                var object new_cons = allocate_cons();
                Cdr(new_cons) = popSTACK(); Car(new_cons) = S(vector);
                value1 = new_cons;
              }
            }
            break;
          case Atype_Char:
            ASSERT(!simple);
           #if (base_char_code_limit == char_code_limit)
            pushSTACK(S(base_string)); goto vectors;
           #else
            pushSTACK(S(string)); goto vectors;
           #endif
          default: NOTREACHED;
        }
      }
      break;
    vectors: /* type of the vector in STACK_0 */
      pushSTACK(array_dimensions(arg)); /* list of dimensions */
      {
        var object new_cons = allocate_cons();
        Cdr(new_cons) = popSTACK(); Car(new_cons) = popSTACK();
        value1 = new_cons;
      }
      break;
    case_ovector: /* other general-vector */
      /* -> (SIMPLE-ARRAY T (dim0)) or (VECTOR T dim0) */
      {
        var bool simple =
          ((Iarray_flags(arg)
            & (bit(arrayflags_adjustable_bit)
               | bit(arrayflags_fillp_bit)
               | bit(arrayflags_displaced_bit)
               | bit(arrayflags_dispoffset_bit) ))
           == 0);
        pushSTACK(array_dimensions(arg)); /* list of dimensions */
        if (simple) {
          {
            var object new_cons = allocate_cons();
            Cdr(new_cons) = NIL; Car(new_cons) = popSTACK();
            pushSTACK(new_cons);
          }
          {
            var object new_cons = allocate_cons();
            Cdr(new_cons) = popSTACK(); Car(new_cons) = T;
            pushSTACK(new_cons);
          }
          {
            var object new_cons = allocate_cons();
            Cdr(new_cons) = popSTACK(); Car(new_cons) = S(simple_array);
            value1 = new_cons;
          }
        } else {
          {
            var object new_cons = allocate_cons();
            Cdr(new_cons) = popSTACK(); Car(new_cons) = T;
            pushSTACK(new_cons);
          }
          {
            var object new_cons = allocate_cons();
            Cdr(new_cons) = popSTACK(); Car(new_cons) = S(vector);
            value1 = new_cons;
          }
        }
      }
      break;
    case_sb2vector: /* simple Byte-Vector -> (SIMPLE-ARRAY (UNSIGNED-BYTE n) (dim0)) */
    case_sb4vector:
    case_sb8vector:
    case_sb16vector:
    case_sb32vector:
      pushSTACK(S(simple_array)); goto arrays;
    case_ob2vector: /* other Byte-Vector -> ([SIMPLE-]ARRAY (UNSIGNED-BYTE n) (dim0)) */
    case_ob4vector:
    case_ob8vector:
    case_ob16vector:
    case_ob32vector:
    case_mdarray: /* other Array -> ([SIMPLE-]ARRAY eltype dims) */
      pushSTACK( ((Iarray_flags(arg)
                   & (  bit(arrayflags_adjustable_bit)
                      | bit(arrayflags_fillp_bit)
                      | bit(arrayflags_displaced_bit)
                      | bit(arrayflags_dispoffset_bit)))
                  == 0)
                 ? S(simple_array)
                 : S(array));
      goto arrays;
    arrays:
      pushSTACK(arg);
      pushSTACK(array_dimensions(arg)); /* list of dimensions */
      STACK_1 = array_element_type(STACK_1); /* eltype */
      value1 = listof(3);
      break;
    case_closure: /* Closure */
      /* -> COMPILED-FUNCTION or FUNCTION or a subclass of
            FUNCALLABLE-STANDARD-OBJECT */
      if (Closure_instancep(arg))
        goto instances;
      if (simple_bit_vector_p(Atype_8Bit,TheClosure(arg)->clos_codevec)) {
        # compiled Closure
        value1 = S(compiled_function);
      } else {
        # interpreted Closure
        value1 = S(function);
      }
      break;
    case_structure: { /* Structure -> type of the Structure */
        var object type = TheStructure(arg)->structure_types;
        /* (name_1 ... name_i-1 name_i). type is name_1. */
        value1 = Car(type);
      }
      break;
    case_stream: /* Stream -> STREAM or according to Stream-type */
      switchu (TheStream(arg)->strmtype) {
        case strmtype_file:     value1 = S(file_stream); break;
        case strmtype_synonym:  value1 = S(synonym_stream); break;
        case strmtype_broad:    value1 = S(broadcast_stream); break;
        case strmtype_concat:   value1 = S(concatenated_stream); break;
        case strmtype_twoway:   value1 = S(two_way_stream); break;
        case strmtype_echo:     value1 = S(echo_stream); break;
        case strmtype_str_in:
        case strmtype_str_out:
        case strmtype_str_push: value1 = S(string_stream); break;
        default:                value1 = S(stream); break;
      }
      break;
    case_orecord: case_lrecord: /* OtherRecord -> PACKAGE, ... */
      switch (Record_type(arg)) {
        case_Rectype_Symbol_above;
        case_Rectype_Sbvector_above;
        case_Rectype_Sb2vector_above;
        case_Rectype_Sb4vector_above;
        case_Rectype_Sb8vector_above;
        case_Rectype_Sb16vector_above;
        case_Rectype_Sb32vector_above;
        case_Rectype_Sstring_above;
        case_Rectype_Svector_above;
        case_Rectype_ostring_above;
        case_Rectype_ovector_above;
        case_Rectype_obvector_above;
        case_Rectype_ob2vector_above;
        case_Rectype_ob4vector_above;
        case_Rectype_ob8vector_above;
        case_Rectype_ob16vector_above;
        case_Rectype_ob32vector_above;
        case_Rectype_mdarray_above;
        case_Rectype_Closure_above;
        case_Rectype_Structure_above;
        case_Rectype_Stream_above;
        case_Rectype_Instance_above;
        case_Rectype_Bignum_above;
        case_Rectype_Ratio_above;
        case_Rectype_Ffloat_above;
        case_Rectype_Dfloat_above;
        case_Rectype_Lfloat_above;
        case_Rectype_Complex_above;
        case_Rectype_Subr_above;
        case Rectype_Hashtable: /* Hash-Table */
          value1 = S(hash_table); break;
        case Rectype_Package: /* Package */
          value1 = S(package); break;
        case Rectype_Readtable: /* Readtable */
          value1 = S(readtable); break;
        case Rectype_Pathname: /* Pathname */
          value1 = S(pathname); break;
       #ifdef LOGICAL_PATHNAMES
        case Rectype_Logpathname: /* Logical Pathname */
          value1 = S(logical_pathname); break;
       #endif
        case Rectype_Random_State: /* Random-State */
          value1 = S(random_state); break;
        case Rectype_Byte: /* Byte */
          value1 = S(byte); break;
        case Rectype_Fsubr: /* Fsubr -> SPECIAL-OPERATOR */
          value1 = S(special_operator); break;
        case Rectype_Loadtimeeval: /* Load-Time-Eval */
          value1 = S(load_time_eval); break;
        case Rectype_Symbolmacro: /* Symbol-Macro */
          value1 = S(symbol_macro); break;
        case Rectype_GlobalSymbolmacro: /* Global-Symbol-Macro */
          value1 = S(global_symbol_macro); break;
        case Rectype_Macro: /* Macro */
          value1 = S(macro); break;
        case Rectype_FunctionMacro: /* FunctionMacro */
          value1 = S(function_macro); break;
        case Rectype_BigReadLabel: /* BigReadLabel -> READ-LABEL */
          value1 = S(read_label); break;
        case Rectype_Encoding: /* Encoding */
          value1 = S(encoding); break;
       #ifdef FOREIGN
        case Rectype_Fpointer: /* Foreign-Pointer-wrapping */
          value1 = S(foreign_pointer); break;
       #endif
       #ifdef DYNAMIC_FFI
        case Rectype_Faddress: /* Foreign-Address */
          value1 = S(foreign_address); break;
        case Rectype_Fvariable: /* Foreign-Variable */
          value1 = S(foreign_variable); break;
        case Rectype_Ffunction: /* Foreign-Function */
          value1 = S(foreign_function); break;
       #endif
        case Rectype_Weakpointer: /* Weak-Pointer */
          value1 = S(weak_pointer); break;
        case Rectype_MutableWeakList: /* mutable Weak-List */
          value1 = S(weak_list); break;
        case Rectype_MutableWeakAlist: /* mutable Weak-Alist */
          value1 = S(weak_alist); break;
        case Rectype_Weakmapping: /* Weak-Mapping */
          value1 = S(weak_mapping); break;
        case Rectype_Finalizer: /* Finalizer (should not occur) */
          value1 = S(finalizer); break;
       #ifdef SOCKET_STREAMS
        case Rectype_Socket_Server: /* Socket-Server */
          value1 = S(socket_server); break;
       #endif
       #ifdef YET_ANOTHER_RECORD
        case Rectype_Yetanother: /* Yetanother -> YET-ANOTHER */
          value1 = S(yet_another); break;
       #endif
        case Rectype_WeakList: /* Weak-List */
          value1 = S(internal_weak_list); break;
        case Rectype_WeakAnd: /* Weak-And-Relation */
          value1 = S(weak_and_relation); break;
        case Rectype_WeakOr: /* Weak-Or-Relation */
          value1 = S(weak_or_relation); break;
        case Rectype_WeakAndMapping: /* Weak-And-Mapping */
          value1 = S(weak_and_mapping); break;
        case Rectype_WeakOrMapping: /* Weak-Or-Mapping */
          value1 = S(weak_or_mapping); break;
        case Rectype_WeakAlist_Key:
        case Rectype_WeakAlist_Value:
        case Rectype_WeakAlist_Either:
        case Rectype_WeakAlist_Both: /* Weak-Alist */
          value1 = S(internal_weak_alist); break;
        case Rectype_WeakHashedAlist_Key:
        case Rectype_WeakHashedAlist_Value:
        case Rectype_WeakHashedAlist_Either:
        case Rectype_WeakHashedAlist_Both: /* Weak-Hashed-Alist */
          value1 = S(internal_weak_hashed_alist); break;
        default: goto unknown;
      }
      break;
    instances:
    case_instance: { /* Instance -> name of the class or the class itself */
        /* (CLtL2 p. 781 top) */
        var object arg_forwarded = arg;
        instance_un_realloc(arg_forwarded);
        /*instance_update(arg,arg_forwarded); - not needed since we don't access a slot */
        var object cv = TheInstance(arg_forwarded)->inst_class_version;
        var object clas = TheClassVersion(cv)->cv_newest_class;
        var object name = TheClass(clas)->classname;
        value1 = (eq(get(name,S(closclass)),clas)
                  /* (GET name 'CLOS::CLOSCLASS) = class ? */
                  ? name : clas);
      }
      break;
    case_char: /* Character -> STANDARD-CHAR or BASE-CHAR or CHARACTER */
     #if (base_char_code_limit < char_code_limit)
      if (as_cint(char_code(arg)) >= base_char_code_limit) {
        value1 = S(character); break;
      }
     #endif
      if (standard_cint_p(as_cint(char_code(arg))))
        value1 = S(standard_char);
      else
        value1 = S(base_char);
      break;
    case_subr: /* SUBR -> COMPILED-FUNCTION */
      value1 = S(compiled_function); break;
   #ifdef TYPECODES
    case_system: /* -> FRAME-POINTER, READ-LABEL, SYSTEM-INTERNAL */
      if (!wbit_test(as_oint(arg),0+oint_addr_shift))
        value1 = S(frame_pointer);
      else
        if (!wbit_test(as_oint(arg),oint_data_len-1+oint_addr_shift))
          value1 = S(read_label);
        else
          value1 = S(system_internal);
      break;
   #else
    case_small_read_label: /* -> READ-LABEL */
      value1 = S(read_label); break;
    case_system: /* -> SYSTEM-INTERNAL */
      value1 = S(system_internal); break;
   #endif
      /* due to the rule 1 in
         <http://www.lisp.org/HyperSpec/Body/fun_type-of.html>,
         we must have (TYPEP X Y) ==> (SUBTYPEP (TYPE-OF X) Y)
         for all "built-in types" Y as listed in table 4-2 in
         <http://www.lisp.org/HyperSpec/Body/sec_4-2-3.html>
         if X is a FIXNUM or a BIGNUM and Y is UNSIGNED-BYTE,
         this means that TYPE-OF must distinguish between positive
         and negative integers: */
    case_fixnum: /* Fixnum -> BIT or FIXNUM+ or FIXNUM- */
      value1 = (eq(arg,Fixnum_0) || eq(arg,Fixnum_1) ? (object)S(bit)
                : positivep(arg) ? (object)O(type_posfixnum)
                : (object)O(type_negfixnum));
      break;
    case_bignum: /* Bignum -> BIGNUM+ or BIGNUM- */
      value1 = positivep(arg) ? O(type_posbignum) : O(type_negbignum); break;
    case_ratio: /* Ratio -> RATIO */
      value1 = S(ratio); break;
    case_sfloat: /* Short-Float -> SHORT-FLOAT */
      value1 = S(short_float); break;
    case_ffloat: /* Single-Float -> SINGLE-FLOAT */
      value1 = S(single_float); break;
    case_dfloat: /* Double-Float -> DOUBLE-FLOAT */
      value1 = S(double_float); break;
    case_lfloat: /* Long-Float -> LONG-FLOAT */
      value1 = S(long_float); break;
    case_complex: /* Complex -> COMPLEX */
      value1 = S(complex); break;
    default:
    unknown: /* unknown type */
      pushSTACK(S(type_of));
      fehler(serious_condition,GETTEXT("~S: unidentifiable type!!!"));
  }
  mv_count=1;
}

LISPFUNN(defclos,6)
{ /* (CLOS::%DEFCLOS <standard-class>-version <structure-class>-version
                     <built-in-class>-version <defined-class> <potential-class>
                     built-in-classes)
   sets the data needed for CLOS::CLASS-P and CLOS:CLASS-OF. */
  /* for CLOS::CLASS-P : */
  O(class_version_standard_class) = STACK_5;
  O(class_version_structure_class) = STACK_4;
  O(class_version_built_in_class) = STACK_3;
  O(class_defined_class) = STACK_2;
  O(class_potential_class) = STACK_1;
  /* for CLOS:CLASS-OF : */
  {
    var gcv_object_t* ptr1 = &TheSvector(STACK_0)->data[0];
    var gcv_object_t* ptr2 = &O(class_array);
    var uintC count;
    dotimesC(count,Svector_length(STACK_0), { /* = &O(class_vector)-&O(class_array)+1 */
      *ptr2++ = *ptr1++;
    });
  }
  value1 = NIL; mv_count=0; skipSTACK(6);
}

LISPFUNNR(potential_class_p,1)
{ /* (CLOS::POTENTIAL-CLASS-P object) tests, if an object is a class or
     a forward-referenced class. */
  var object obj = popSTACK();
  if_potential_class_p(obj, { value1 = T; }, { value1 = NIL; }); mv_count=1;
}

LISPFUNNR(defined_class_p,1)
{ /* (CLOS::DEFINED-CLASS-P object) tests, if an object is a class,
     excluding forward-referenced classes. */
  var object obj = popSTACK();
  if_defined_class_p(obj, { value1 = T; }, { value1 = NIL; }); mv_count=1;
}

/* (CLOS:CLASS-OF object), CLTL2 p. 822,783
   Doesn't trigger GC. */
LISPFUNNR(class_of,1)
{
  var object arg = popSTACK();
 #ifdef TYPECODES
  switch (typecode(arg))
 #else
  if (orecordp(arg)) {
    goto case_orecord;
  } else if (consp(arg)) {
    goto case_cons;
  } else if (immsubrp(arg)) {
    goto case_subr;
  } else if (charp(arg)) {
    goto case_char;
  } else if (fixnump(arg)) {
    goto case_integer;
  } else if (short_float_p(arg)) {
    goto case_float;
  } else if (machinep(arg)) {
    goto case_machine;
  } else if (small_read_label_p(arg)) {
    goto case_system;
  } else if (systemp(arg)) {
    goto case_system;
  } else { goto unknown; }
  switch (0)
 #endif
  {
    instances:
    case_instance: { /* instance -> its class */
      var object arg_forwarded = arg;
      instance_un_realloc(arg_forwarded);
      /*instance_update(arg,arg_forwarded); - not needed since we don't access a slot */
      var object cv = TheInstance(arg_forwarded)->inst_class_version;
      value1 = TheClassVersion(cv)->cv_newest_class;
      break;
    }
    case_structure: { /* Structure -> type of the structure or <t> */
      var object type = TheStructure(arg)->structure_types;
      /* (name_1 ... name_i-1 name_i). type is name_1. */
      while (consp(type)) {
        var object name = Car(type);
        var object clas = get(name,S(closclass)); /* (GET name 'CLOS::CLOSCLASS) */
        if_defined_class_p(clas, { value1 = clas; goto fertig; }, ; );
        type = Cdr(type);
      }
    }
      value1 = O(class_t); break;
    case_cons: /* Cons -> <cons> */
      value1 = O(class_cons); break;
    case_symbol: /* Symbol -> <symbol> or <null> */
      value1 = (nullp(arg) ? O(class_null) : O(class_symbol)); break;
    case_sstring: case_ostring: /* String -> <string> */
      value1 = O(class_string); break;
    case_sbvector: case_obvector: /* Bit-Vector -> <bit-vector> */
      value1 = O(class_bit_vector); break;
    case_sb2vector: case_ob2vector: /* Byte-Vector -> <vector> */
    case_sb4vector: case_ob4vector:
    case_sb8vector: case_ob8vector:
    case_sb16vector: case_ob16vector:
    case_sb32vector: case_ob32vector:
    case_svector: case_ovector: /* General-Vector -> <vector> */
      value1 = O(class_vector); break;
    case_mdarray: /* other Array -> <array> */
      value1 = O(class_array); break;
    case_closure: /* Closure -> <function> or a subclass of
                     <funcallable-standard-object> */
      if (Closure_instancep(arg))
        goto instances;
    case_subr: /* SUBR -> <function> */
      value1 = O(class_function); break;
    case_stream: /* Stream -> <stream> or according to Stream-type */
      switchu (TheStream(arg)->strmtype) {
        case strmtype_file:     value1 = O(class_file_stream); break;
        case strmtype_synonym:  value1 = O(class_synonym_stream); break;
        case strmtype_broad:    value1 = O(class_broadcast_stream); break;
        case strmtype_concat:   value1 = O(class_concatenated_stream); break;
        case strmtype_twoway:   value1 = O(class_two_way_stream); break;
        case strmtype_echo:     value1 = O(class_echo_stream); break;
        case strmtype_str_in:
        case strmtype_str_out:
        case strmtype_str_push: value1 = O(class_string_stream); break;
        default:                value1 = O(class_stream); break;
      }
      break;
    case_orecord: case_lrecord: /* OtherRecord -> <package>, ... */
      switch (Record_type(arg)) {
        case_Rectype_Instance_above;
        case_Rectype_Structure_above;
        case_Rectype_Symbol_above;
        case_Rectype_Sstring_above;
        case_Rectype_ostring_above;
        case_Rectype_Sbvector_above;
        case_Rectype_obvector_above;
        case_Rectype_Sb2vector_above;
        case_Rectype_ob2vector_above;
        case_Rectype_Sb4vector_above;
        case_Rectype_ob4vector_above;
        case_Rectype_Sb8vector_above;
        case_Rectype_ob8vector_above;
        case_Rectype_Sb16vector_above;
        case_Rectype_ob16vector_above;
        case_Rectype_Sb32vector_above;
        case_Rectype_ob32vector_above;
        case_Rectype_Svector_above;
        case_Rectype_ovector_above;
        case_Rectype_mdarray_above;
        case_Rectype_Closure_above;
        case_Rectype_Stream_above;
        case_Rectype_integer_above;
        case_Rectype_Ratio_above;
        case_Rectype_float_above;
        case_Rectype_Complex_above;
        case_Rectype_Subr_above;
        case Rectype_Hashtable: /* Hash-Table */
          value1 = O(class_hash_table); break;
        case Rectype_Package: /* Package */
          value1 = O(class_package); break;
        case Rectype_Readtable: /* Readtable */
          value1 = O(class_readtable); break;
        case Rectype_Pathname: /* Pathname */
          value1 = O(class_pathname); break;
       #ifdef LOGICAL_PATHNAMES
        case Rectype_Logpathname: /* Logical Pathname */
          value1 = O(class_logical_pathname); break;
       #endif
        case Rectype_Random_State: /* Random-State */
          value1 = O(class_random_state); break;
        case Rectype_Byte: /* Byte -> <t> */
        case Rectype_Fsubr: /* Fsubr -> <t> */
        case Rectype_Loadtimeeval: /* Load-Time-Eval -> <t> */
        case Rectype_Symbolmacro: /* Symbol-Macro -> <t> */
        case Rectype_GlobalSymbolmacro: /* Global-Symbol-Macro -> <t> */
        case Rectype_Macro: /* Macro -> <t> */
        case Rectype_FunctionMacro: /* FunctionMacro -> <t> */
        case Rectype_BigReadLabel: /* BigReadLabel -> <t> */
        case Rectype_Encoding: /* Encoding -> <t> */
       #ifdef FOREIGN
        case Rectype_Fpointer: /* Foreign-Pointer-Wrapping -> <t> */
       #endif
       #ifdef DYNAMIC_FFI
        case Rectype_Faddress: /* Foreign-Address -> <t> */
        case Rectype_Fvariable: /* Foreign-Variable -> <t> */
       #endif
        case Rectype_Weakpointer: /* Weak-Pointer -> <t> */
        case Rectype_MutableWeakList: /* mutable Weak-List -> <t> */
        case Rectype_MutableWeakAlist: /* mutable Weak-Alist -> <t> */
        case Rectype_Weakmapping: /* Weak-Mapping -> <t> */
        case Rectype_Finalizer: /* Finalizer -> <t> */
       #ifdef SOCKET_STREAMS
        case Rectype_Socket_Server: /* Socket-Server -> <t> */
       #endif
          value1 = O(class_t); break;
       #ifdef DYNAMIC_FFI
        case Rectype_Ffunction: /* Foreign-Function -> <function> */
          value1 = O(class_function); break;
       #endif
       #ifdef YET_ANOTHER_RECORD
        case Rectype_Yetanother: /* Yetanother -> <t> */
          value1 = O(class_t); break;
       #endif
        case Rectype_WeakList: /* Weak-List -> <t> */
        case Rectype_WeakAnd: /* Weak-And-Relation -> <t> */
        case Rectype_WeakOr: /* Weak-Or-Relation -> <t> */
        case Rectype_WeakAndMapping: /* Weak-And-Mapping -> <t> */
        case Rectype_WeakOrMapping: /* Weak-Or-Mapping -> <t> */
        case Rectype_WeakAlist_Key: /* Weak-Alist -> <t> */
        case Rectype_WeakAlist_Value: /* Weak-Alist -> <t> */
        case Rectype_WeakAlist_Either: /* Weak-Alist -> <t> */
        case Rectype_WeakAlist_Both: /* Weak-Alist -> <t> */
        case Rectype_WeakHashedAlist_Key: /* Weak-Hashed-Alist -> <t> */
        case Rectype_WeakHashedAlist_Value: /* Weak-Hashed-Alist -> <t> */
        case Rectype_WeakHashedAlist_Either: /* Weak-Hashed-Alist -> <t> */
        case Rectype_WeakHashedAlist_Both: /* Weak-Hashed-Alist -> <t> */
          value1 = O(class_t); break;
        default: goto unknown;
      }
      break;
    case_char: /* Character -> <character> */
      value1 = O(class_character); break;
    case_machine: /* machine pointer -> <t> */
    case_system: /* -> <t> */
      value1 = O(class_t); break;
    case_integer: /* Integer -> <integer> */
      value1 = O(class_integer); break;
    case_ratio: /* Ratio -> <ratio> */
      value1 = O(class_ratio); break;
    case_float: /* Float -> <float> */
      value1 = O(class_float); break;
    case_complex: /* Complex -> <complex> */
      value1 = O(class_complex); break;
    default:
    unknown: /* unknown type */
      pushSTACK(S(class_of));
      fehler(serious_condition,GETTEXT("~S: unidentifiable type!!!"));
  }
  if_defined_class_p(value1, ; , {
    pushSTACK(value1);
    pushSTACK(S(class_of));
    fehler(error,GETTEXT("~S: type ~S does not correspond to a class"));
  });
 fertig:
  mv_count=1;
}

LISPFUN(find_class,seclass_default,1,2,norest,nokey,0,NIL)
{ /* (CLOS:FIND-CLASS symbol [errorp [environment]]), CLTL2 p. 843
 (defun find-class (symbol &optional (errorp t) environment)
   (declare (ignore environment)) ; what is the meaning of that environment?
   (unless (symbolp symbol)
     (error-of-type 'type-error
       (ENGLISH "~S: argument ~S is not a symbol")
       'find-class symbol))
   (let ((class (get symbol 'CLOS::CLASS)))
     (if (not (defined-class-p class))
       (if errorp
         (error-of-type 'error
           (ENGLISH "~S: ~S does not name a class")
           'find-class symbol)
         nil)
       class))) */
  STACK_2 = check_symbol(STACK_2);
  var object clas = get(STACK_2,S(closclass)); /* (GET symbol 'CLOS::CLOSCLASS) */
  if_defined_class_p(clas, { value1 = clas; } , {
    if (!nullp(STACK_1)) {
      pushSTACK(STACK_2);
      pushSTACK(S(find_class));
      fehler(error,GETTEXT("~S: ~S does not name a class"));
    }
    value1 = NIL;
  });
  mv_count=1;
  skipSTACK(3);
}

/* typep_class(obj,clas)
 > obj: an object
 > clas: a class object
 < true if the object is an instance of the class, false otherwise
 clobbers value1, mv_count */
global bool typep_class (object obj, object clas) {
  pushSTACK(obj); C_class_of();
  var object objclass = value1;
  /* Look whether clas is a superclass of objclass.
     Equivalent to (CLOS::SUBCLASSP objclass clas), just a bit faster. */
  if (srecord_length(TheClass(objclass)) > built_in_class_length) {
    /* Make a distinction between <semi-standard-class> and <structure-class>:
       Is (class-current-version class) a vector, or is (class-names class)
       a cons? */
    if (matomp(TheClass(objclass)->current_version)) {
      /* <semi-standard-class>. */
      if (nullp(TheClass(objclass)->precedence_list)) /* not yet finalized? */
        NOTREACHED; /* shouldn't happen because obj is already an instance */
      var object superclasses_table = TheClass(objclass)->all_superclasses;
      if (TheHashtable(superclasses_table)->ht_size > 7)
        return !eq(gethash(clas,superclasses_table,false),nullobj);
      /* Few superclasses -> not worth a hash table access. */
    } else {
      /* <structure-class>. */
      /* There are few superclasses. Not worth a hash table access. */
    }
  }
  #if 0
    return !nullp(memq(clas,TheClass(objclass)->precedence_list));
  #else /* inlined, for performance */
    var object l;
    for (l = TheClass(objclass)->precedence_list; consp(l); l = Cdr(l))
      if (eq(Car(l),clas))
        return true;
    return false;
  #endif
}

# (CLOS::TYPEP-CLASS object class)
# == (TYPEP object class) == (CLOS::SUBCLASSP (CLASS-OF object) class)
LISPFUNN(typep_class,2)
{
  var object clas = popSTACK();
  if_defined_class_p(clas, ; , fehler_class(clas); );
  VALUES_IF(typep_class(popSTACK(),clas));
}

/* typep_classname(obj,classname)
 > obj: an object
 > classname: a symbol expected to name a class with "proper name" classname
 < true if the object is an instance of the class, false otherwise
 clobbers value1, mv_count */
global bool typep_classname (object obj, object classname) {
  pushSTACK(obj); C_class_of();
  var object objclass = value1;
  /* Look whether classname names a superclass of objclass.
     Equivalent to (CLOS::SUBCLASSP objclass (find-class classname)),
     just a bit faster. */
  if (srecord_length(TheClass(objclass)) > built_in_class_length) {
    /* Make a distinction between <semi-standard-class> and <structure-class>:
       Is (class-current-version class) a vector, or is (class-names class)
       a cons? */
    if (matomp(TheClass(objclass)->current_version)) {
      /* <semi-standard-class>. */
      if (nullp(TheClass(objclass)->precedence_list)) /* not yet finalized? */
        NOTREACHED; /* shouldn't happen because obj is already an instance */
      var object superclasses_table = TheClass(objclass)->all_superclasses;
      if (TheHashtable(superclasses_table)->ht_size > 7) {
        var object clas = get(classname,S(closclass));
        return !eq(gethash(clas,superclasses_table,false),nullobj);
      }
      /* Few superclasses -> not worth a hash table access. */
    } else {
      /* <structure-class>. */
      /* There are few superclasses. Not worth a hash table access. */
      var object objclassnames = TheClass(objclass)->current_version;
      #if 0
        return !nullp(memq(classname,objclassnames));
      #else /* inlined, for performance */
        var object l;
        for (l = objclassnames; consp(l); l = Cdr(l))
          if (eq(Car(l),classname))
            return true;
        return false;
      #endif
    }
  }
  var object l;
  for (l = TheClass(objclass)->precedence_list; consp(l); l = Cdr(l))
    if (eq(TheClass(Car(l))->classname,classname))
      return true;
  return false;
}


/* UP: expand all DEFTYPE definitions in the type spec
 (recursively, unless once_p is true)
 > type_spec: Lisp object
 < result: the expansion (when not a deftyped type, returns the argument)
 can trigger GC */
global maygc object expand_deftype (object type_spec, bool once_p) {
  var uintV max_depth =
    (posfixnump(Symbol_value(S(deftype_depth_limit)))
     ? posfixnum_to_V(Symbol_value(S(deftype_depth_limit)))
     : posfixnum_to_V(Symbol_value(S(most_positive_fixnum))));
  pushSTACK(type_spec);
 start:
  if (max_depth > 0) max_depth--;
  else { /* too many nested DEFTYPEs */
    /* type_spec is already on the stack */
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,GETTEXT("~S: type definition for ~S exceeds depth limit, maybe recursive"));
  }
  if (symbolp(type_spec)) { /* (GET type-spec 'DEFTYPE-EXPANDER) */
    var object expander = get(type_spec,S(deftype_expander));
    if (boundp(expander)) {
      pushSTACK(type_spec);
      pushSTACK(expander);
      var object new_cons = allocate_cons();
      expander = popSTACK();
      Car(new_cons) = popSTACK(); /* new_cons = (list type-spec) */
      pushSTACK(new_cons); funcall(expander,1); /* call expander */
      type_spec = value1; /* use the return value as the new type-spec */
      if (!once_p) goto start;
    }
  } else if (mconsp(type_spec) && symbolp(Car(type_spec))) {
    /* (GET (CAR type-spec) 'DEFTYPE-EXPANDER) */
    var object expander = get(Car(type_spec),S(deftype_expander));
    if (boundp(expander)) {
      pushSTACK(type_spec); funcall(expander,1); /* call expander */
      type_spec = value1; /* use the return value as the new type-spec */
      if (!once_p) goto start;
    }
  }
  skipSTACK(1);
  return type_spec;
}

LISPFUN(expand_deftype,seclass_default,1,1,norest,nokey,0,NIL)
/* (SYS::EXPAND-DEFTYPE type-spec &optional once-p)
   ==> expanded, user-defined-p */
{
  var object once_p = popSTACK();
  VALUES2(expand_deftype(STACK_0,!missingp(once_p)),
          eq(STACK_0,value1) ? NIL : T);
  skipSTACK(1);
}

/* UP: coerce STACK_1 to result_type (a sequence).
 check that the result is of type type.
 set value1 to the result.
 can trigger GC */
local maygc Values coerce_sequence_check (object type, object result_type) {
  pushSTACK(type);
  /* make new sequence: */
  var object new_seq = (coerce_sequence(STACK_2,result_type,true),value1);
  /* and re-check with TYPEP: */
  pushSTACK(new_seq); pushSTACK(STACK_(0+1)); STACK_(0+2) = new_seq;
  funcall(S(typep),2); /* (TYPEP new_seq type) */
  if (!nullp(value1)) { /* yes -> new_seq is the value */
    value1 = popSTACK();
  } else { /* does not match because of SIMPLE-... -> copy new_seq: */
    funcall(L(copy_seq),1); /* (COPY-SEQ new_seq) */
  }
}

LISPFUNNR(coerce,2)
/* (COERCE object result-type), CLTL p. 51
 Method:
 (TYPEP object result-type) -> return object
  first, expand deftype in result-type
 result-type -- a type symbol:
   type = T -> return object
   type = CHARACTER, STRING-CHAR -> call COERCE_CHAR
   type = BASE-CHAR -> call COERCE_CHAR and check
   type = FLOAT, SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT ->
          use arithmetic conversion
   type = COMPLEX -> check for being a number
   type = FUNCTION -> function name or lambda expression --> function
   type = ARRAY, SIMPLE-ARRAY, VECTOR, SIMPLE-VECTOR, STRING, SIMPLE-STRING,
          BASE-STRING, SIMPLE-BASE-STRING, BIT-VECTOR, SIMPLE-BIT-VECTOR ->
          [adjust result-type to the object as below??]
          convert with COERCE-SEQUENCE, check with TYPEP, and
          copy with COPY-SEQ.
   otherwise convert with COERCE-SEQUENCE
 result-type -- a cons with symbol TYPE as CAR:
   type = AND -> (coerce object (second result-type)), check with TYPEP
   type = FLOAT, SHORT-FLOAT, SINGLE-FLOAT, DOUBLE-FLOAT, LONG-FLOAT ->
          use arithmetic conversion, then check with TYPEP
   type = COMPLEX -> check for being a number
          coerce Re to (second result-type).
          coerce Im to (or (third result-type) (second result-type))
          then call COMPLEX.
   type = ARRAY, SIMPLE-ARRAY, VECTOR, SIMPLE-VECTOR, STRING, SIMPLE-STRING,
          BASE-STRING, SIMPLE-BASE-STRING, BIT-VECTOR, SIMPLE-BIT-VECTOR ->
          result-type an object anpassen, convert with COERCE-SEQUENCE
          (element-type indicated in result-type is also processed),
          check type and possibly copy with COPY-SEQ
          check the result-type.
 otherwise Error. */
{
  /* (TYPEP object result-type): */
  pushSTACK(STACK_1); pushSTACK(STACK_(0+1)); funcall(S(typep),2);
  if (!nullp(value1)) { /* object as the value */
   return_object:
    VALUES1(STACK_1); skipSTACK(2); return;
  }
  STACK_0 = expand_deftype(STACK_0,false);
  if_defined_class_p(STACK_0, { STACK_0 = TheClass(STACK_0)->classname; },);
  /* stack layout: object, result-type. */
  if (matomp(STACK_0)) {
    if (!symbolp(STACK_0)) goto fehler_type;
    /* result-type is a symbol */
    var object result_type = STACK_0;
    if (eq(result_type,T)) /* result-type = T ? */
      goto return_object; /* yes -> object as the value */
    if (eq(result_type,S(character)) || eq(result_type,S(string_char))
        #if (base_char_code_limit == char_code_limit)
        || eq(result_type,S(base_char))
        #endif
       ) { /* result-type = CHARACTER or STRING-CHAR [or BASE-CHAR] ? */
      /* try to convert object to character */
      var object as_char = coerce_char(STACK_1);
      if (nullp(as_char)) {
        pushSTACK(STACK_1); /* TYPE-ERROR slot DATUM */
        pushSTACK(O(type_designator_character)); /* TYPE-ERROR slot EXPECTED-TYPE */
        goto fehler_object;
      }
      VALUES1(as_char); skipSTACK(2); return;
    }
    #if (base_char_code_limit < char_code_limit)
    if (eq(result_type,S(base_char))) { /* result-type = BASE-CHAR ? */
      /* try to convert object to character */
      var object as_char = coerce_char(STACK_1);
      if (!base_char_p(as_char)) {
        pushSTACK(STACK_1); /* TYPE-ERROR slot DATUM */
        pushSTACK(O(type_designator_base_char)); /* TYPE-ERROR slot EXPECTED-TYPE */
        goto fehler_object;
      }
      VALUES1(as_char); skipSTACK(2); return;
    }
    #endif
    if (   eq(result_type,S(float)) /* FLOAT ? */
        || eq(result_type,S(short_float)) /* SHORT-FLOAT ? */
        || eq(result_type,S(single_float)) /* SINGLE-FLOAT ? */
        || eq(result_type,S(double_float)) /* DOUBLE-FLOAT ? */
        || eq(result_type,S(long_float)) /* LONG-FLOAT ? */
       ) { /* convert object to float: */
      VALUES1(coerce_float(STACK_1,result_type));
      skipSTACK(2); return;
    }
    if (eq(result_type,S(complex))) { /* COMPLEX ? */
      if (!numberp(STACK_1)) { /* object must be a number */
        pushSTACK(STACK_1); /* TYPE-ERROR slot DATUM */
        pushSTACK(S(number)); /* TYPE-ERROR slot EXPECTED-TYPE */
        goto fehler_object;
      }
      if (!N_floatp(STACK_1))
        goto return_object;
      VALUES1(F_complex_C(STACK_1));
      skipSTACK(2); return;
    }
    if (eq(result_type,S(function))) { /* FUNCTION ? */
      /* viz. coerce_function() */
      var object fun = STACK_1;
      if (funnamep(fun)) { /* Symbol or (SETF symbol) ? */
        VALUES1(sym_function(fun,NIL)); /* global function definition */
        if (!functionp(value1)) {
          if (functionmacrop(value1))
            VALUES1(TheFunctionMacro(value1)->functionmacro_function);
          else
            VALUES1(check_fdefinition(fun,S(coerce)));
        }
        skipSTACK(2); return;
      }
      if (!(consp(fun) && eq(Car(fun),S(lambda)))) { /* object must be a lambda expression */
        pushSTACK(fun); /* TYPE-ERROR slot DATUM */
        pushSTACK(O(type_designator_function)); /* TYPE-ERROR slot EXPECTED-TYPE */
        goto fehler_object;
      }
      /* empty environment for get_closure: */
      var gcv_environment_t* env;
      make_STACK_env(NIL,NIL,NIL,NIL,O(top_decl_env), env = );
      /* build closure with lambdabody = (cdr fun), name = :LAMBDA : */
      VALUES1(get_closure(Cdr(fun),S(Klambda),false,env));
      skipSTACK(2+5); return;
    }
    if (   eq(result_type,S(array)) /* ARRAY ? */
        || eq(result_type,S(simple_array)) /* SIMPLE-ARRAY ? */
        || eq(result_type,S(vector)) /* VECTOR ? */
        || eq(result_type,S(simple_vector)) /* SIMPLE-VECTOR ? */
        || eq(result_type,S(string)) || eq(result_type,S(cs_string)) /* STRING ? */
        || eq(result_type,S(simple_string)) /* SIMPLE-STRING ? */
        || eq(result_type,S(base_string)) /* BASE-STRING ? */
        || eq(result_type,S(simple_base_string)) /* SIMPLE-BASE-STRING ? */
        || eq(result_type,S(bit_vector)) /* BIT-VECTOR ? */
        || eq(result_type,S(simple_bit_vector)) /* SIMPLE-BIT-VECTOR ? */
       ) { /* adapt result-type to the type of object */
      if (eq(result_type,S(array)) || eq(result_type,S(vector))) { /* ARRAY or VECTOR ? */
        if (vectorp(STACK_1)) /* already a vector? */
          goto return_object; /* -> is a vector and array */
      } else if (eq(result_type,S(simple_array))) { /* SIMPLE-ARRAY ? */
        if (simplep(STACK_1)) /* already a simple-array? */
          goto return_object;
        if (stringp(STACK_1)) /* object is a string */
          result_type = S(simple_string); /* -> result-type := SIMPLE-STRING */
        else if (bit_vector_p(Atype_Bit,STACK_1)) /* object is a bit-vector */
          result_type = S(simple_bit_vector); /* -> result-type := SIMPLE-BIT-VECTOR */
        /* treat byte-vectors here!?? */
      }
      coerce_sequence_check(result_type,result_type);
      skipSTACK(2); return;
    }
    /* result-type is some other symbol */
    /* (coerce-sequence object result-type) */
    coerce_sequence(STACK_1,STACK_0,false);
    if (eq(value1,nullobj)) { /* failed! */
      pushSTACK(STACK_1);     /* TYPE-ERROR slot DATUM (object) */
      pushSTACK(STACK_(0+1)); /* TYPE-ERROR slot EXPECTED-TYPE (result-type) */
      goto fehler_object;
    }
    skipSTACK(2); return;
  } else {
    /* result-type is a cons */
    var object result_type = STACK_0;
    var object type = Car(result_type);
    if (!symbolp(type)) goto fehler_type; /* must be a symbol */
    if (eq(type,S(and))) { /* (AND ...) ? */
      if (matomp(Cdr(result_type))) /* (AND) */
        goto return_object; /* treat like T */
      /* call (COERCE object (second result-type)): */
      pushSTACK(STACK_1); pushSTACK(Car(Cdr(result_type)));
      funcall(L(coerce),2);
     check_return: /* check new-object in value1 and then return it as value: */
      pushSTACK(value1); /* save new-object */
      /* check (TYPEP new-object result-type): */
      pushSTACK(value1); pushSTACK(STACK_(0+1+1)); funcall(S(typep),2);
      if (nullp(value1)) {
        /* STACK_0 = new-object, TYPE-ERROR slot DATUM */
        pushSTACK(STACK_(0+1)); /* TYPE-ERROR slot EXPECTED-TYPE */
        goto fehler_object;
      } else {
        VALUES1(STACK_0); skipSTACK(3); return; /* new-object */
      }
    }
    if (   eq(type,S(float)) /* FLOAT ? */
        || eq(type,S(short_float)) /* SHORT-FLOAT ? */
        || eq(type,S(single_float)) /* SINGLE-FLOAT ? */
        || eq(type,S(double_float)) /* DOUBLE-FLOAT ? */
        || eq(type,S(long_float)) /* LONG-FLOAT ? */
       ) { /* convert object to float */
      value1 = coerce_float(STACK_1,result_type);
      goto check_return; /* and check against result-type */
    }
    if (eq(type,S(complex))) { /* COMPLEX ? */
      if (!numberp(STACK_1)) { /* object must be a number */
        pushSTACK(STACK_1); /* TYPE-ERROR slot DATUM */
        pushSTACK(S(number)); /* TYPE-ERROR slot EXPECTED-TYPE */
        goto fehler_object;
      }
      if (!mconsp(Cdr(result_type))) goto fehler_type; /* (rest result-type) must be a cons */
      result_type = Cdr(result_type);
      var object rtype = Car(result_type); /* type of Re */
      var object itype = /* type of Im, defaults to rtype */
        (mconsp(Cdr(result_type)) ? (object)Car(Cdr(result_type)) : rtype);
      pushSTACK(rtype); pushSTACK(itype);
      /* get Re and coerce to rtype: */
      pushSTACK(STACK_(1+2)); funcall(L(realpart),1);
      pushSTACK(value1); pushSTACK(STACK_(1+1)); funcall(L(coerce),2);
      STACK_1 = value1;
      /* get Im and coerce to itype: */
      pushSTACK(STACK_(1+2)); funcall(L(imagpart),1);
      pushSTACK(value1); pushSTACK(STACK_(0+1)); funcall(L(coerce),2);
      STACK_0 = value1;
      /* call COMPLEX on it: */
      funcall(L(complex),2);
      skipSTACK(2); return;
    }
    if (   eq(type,S(array)) /* ARRAY ? */
        || eq(type,S(simple_array)) /* SIMPLE-ARRAY ? */
        || eq(type,S(vector)) /* VECTOR ? */
        || eq(type,S(simple_vector)) /* SIMPLE-VECTOR ? */
        || eq(type,S(string)) || eq(type,S(cs_string)) /* STRING ? */
        || eq(type,S(simple_string)) /* SIMPLE-STRING ? */
        || eq(type,S(base_string)) /* BASE-STRING ? */
        || eq(type,S(simple_base_string)) /* SIMPLE-BASE-STRING ? */
        || eq(type,S(bit_vector)) /* BIT-VECTOR ? */
        || eq(type,S(simple_bit_vector)) /* SIMPLE-BIT-VECTOR ? */
       ) { /* adapt result-type to the type of object */
      if (eq(type,S(array)) || eq(type,S(simple_array))
          || eq(type,S(vector))) { /* [SIMPLE-]ARRAY or VECTOR ? */
        var object type2 = Cdr(result_type);
        if (nullp(type2))
          goto adjust_eltype;
        if (!consp(type2)) goto fehler_type;
        if (eq(Car(type2),S(mal))) { /* element-type = * (unspecified) ? */
          type2 = Cdr(type2);
         adjust_eltype: /* here type2 = (cddr result-type) */
          /* replace with a suitable element type: */
          pushSTACK(type2);
          pushSTACK(type);
          if (arrayp(STACK_(1+2)))
            pushSTACK(array_element_type(STACK_(1+2)));
          else
            pushSTACK(T);
          result_type = listof(2);
          type = Car(result_type);
          Cdr(Cdr(result_type)) = popSTACK();
        }
      }
      coerce_sequence_check(type,result_type);
      goto check_return;
    }
    /* (coerce-sequence object result-type) */
    coerce_sequence(STACK_1,STACK_0,false);
    if (eq(value1,nullobj)) { /* failed! */
      /* If we got here, we know that type is valid, datum is not of that type
         and we cannot do the coersion. */
      pushSTACK(STACK_1);     /* TYPE-ERROR slot DATUM (object) */
      pushSTACK(STACK_(0+1)); /* TYPE-ERROR slot EXPECTED-TYPE (result-type) */
      goto fehler_object;
    }
    skipSTACK(2); return;
  }
 fehler_type:
  /* due to the TYPEP call which checks result-type this should never happen
     result-type in STACK_0 */
  pushSTACK(S(coerce));
  fehler(error,GETTEXT("~S: invalid type specification ~S"));
 fehler_object:
  /* stack layout: object, result-type, type-error-datum,
     type-error-expected-type. */
  pushSTACK(STACK_2); /* result-type */
  pushSTACK(STACK_(3+1)); /* object */
  pushSTACK(S(coerce));
  fehler(type_error,GETTEXT("~S: ~S cannot be coerced to type ~S"));
}

/* =========================================================================
 * Heap statistics */

/* Notification from defstruct.lisp and clos.lisp. */
LISPFUNN(note_new_structure_class,0)
{
  O(structure_class_count_max) = fixnum_inc(O(structure_class_count_max),1);
}
LISPFUNN(note_new_standard_class,0)
{
  O(standard_class_count_max) = fixnum_inc(O(standard_class_count_max),1);
}
/* These two ..._count_max variables are provided so that we can do heap
 statistics in one pass, without risking a GC, and without a pre-pass which
 determines the number of occurring types.

 (SYSTEM::HEAP-STATISTICS)
 returns a vector containing statistics records about current heap usage
 for each type:
    stat = #( ... (classname num-instances . num-bytes) ...)

 (SYSTEM::GC-STATISTICS)
 returns a list, with one element for each GC (the first for the last GC,
 the second for the second-to-last, etc.), where each element is a vector
 containing statistics records about what the GC could reclaim of each type:
    statlist = ( #( ... (classname num-instances . num-bytes) ...) ...)

 Since GC statistics is a burden on each GC, we perform it only when needed,
 i.e. while the variable SYSTEM::*GC-STATISTICS* is bound to a value > 0.
 When SYSTEM::*GC-STATISTICS* is 0, no statistics are gathered, but old ones
 are still kept. When SYSTEM::*GC-STATISTICS* is negative, no statistics are
 gathered, and old statistics are thrown away.

 The data is gathered in three areas: one for built-in types, one for
 structure types (an AVL tree, indexed by structure name), one for
 standard-class types (an AVL tree, indexed by the class). While the data
 is being gathered, symbols and classes are pushed onto the STACK, but no
 heap allocation takes place. */

typedef struct {
  const gcv_object_t* name; /* pointer to a GC-safe object (e.g. in the STACK) */
  sintL n_instances; /* number of instances */
  sintM n_bytes;     /* number of bytes */
} hs_record_t;

/* The type of an object for statistics purposes is a little more detailed
   than CLASS-OF, but, unlike TYPE-OF, nonparametric. */
enum { /* The values of this enumeration are 0,1,2,...
          When you change this, update LISPOBJ(hs_...) in constobj.d! */
  enum_hs_t,
  enum_hs_cons,
  enum_hs_null,
  enum_hs_symbol,
  enum_hs_simple_bit_vector,
  enum_hs_simple_2bit_vector,
  enum_hs_simple_4bit_vector,
  enum_hs_simple_8bit_vector,
  enum_hs_simple_16bit_vector,
  enum_hs_simple_32bit_vector,
  enum_hs_simple_nilvector,
  enum_hs_simple_string,
  enum_hs_simple_vector,
  enum_hs_bit_vector,
  enum_hs_2bit_vector,
  enum_hs_4bit_vector,
  enum_hs_8bit_vector,
  enum_hs_16bit_vector,
  enum_hs_32bit_vector,
  enum_hs_nilvector,
  enum_hs_string,
  enum_hs_vector,
  enum_hs_simple_array,
  enum_hs_array,
  enum_hs_function,
  enum_hs_file_stream,
  enum_hs_synonym_stream,
  enum_hs_broadcast_stream,
  enum_hs_concatenated_stream,
  enum_hs_two_way_stream,
  enum_hs_echo_stream,
  enum_hs_string_stream,
  enum_hs_stream,
  enum_hs_hash_table,
  enum_hs_package,
  enum_hs_readtable,
  enum_hs_pathname,
 #ifdef LOGICAL_PATHNAMES
  enum_hs_logical_pathname,
 #endif
  enum_hs_random_state,
  enum_hs_byte,
  enum_hs_special_operator,
  enum_hs_load_time_eval,
  enum_hs_symbol_macro,
  enum_hs_global_symbol_macro,
  enum_hs_macro,
  enum_hs_function_macro,
  enum_hs_big_read_label,
  enum_hs_encoding,
 #ifdef FOREIGN
  enum_hs_foreign_pointer,
 #endif
 #ifdef DYNAMIC_FFI
  enum_hs_foreign_address,
  enum_hs_foreign_variable,
  enum_hs_foreign_function,
 #endif
 #ifdef HAVE_SMALL_SSTRING
  enum_hs_realloc_simple_string,
 #endif
  enum_hs_realloc_instance,
  enum_hs_weakpointer,
  enum_hs_weak_list,
  enum_hs_weak_alist,
  enum_hs_weakmapping,
  enum_hs_finalizer,
 #ifdef SOCKET_STREAMS
  enum_hs_socket_server,
 #endif
 #ifdef YET_ANOTHER_RECORD
  enum_hs_yetanother,
 #endif
  enum_hs_internal_weak_list,
  enum_hs_weak_and_relation,
  enum_hs_weak_or_relation,
  enum_hs_weak_and_mapping,
  enum_hs_weak_or_mapping,
  enum_hs_internal_weak_alist,
  enum_hs_internal_weak_hashed_alist,
  enum_hs_system_function,
  enum_hs_bignum,
  enum_hs_ratio,
 #ifndef IMMEDIATE_FFLOAT
  enum_hs_single_float,
 #endif
  enum_hs_double_float,
  enum_hs_long_float,
  enum_hs_complex,
  enum_hs_dummy
};

/* Need an AVL tree for rapidly associating a hs_record_t to its name. */

#define AVLID  heapstat
#define AVL_ELEMENT  hs_record_t
#define AVL_EQUAL(element1,element2)  (eq(*(element1).name,*(element2).name))
#define AVL_KEY  object
#define AVL_KEYOF(element)  (*(element).name)
#define AVL_SIGNED_INT  soint
#define AVL_COMPARE(key1,key2)  (soint)(as_oint(key1)-as_oint(key2))
#define NO_AVL_MEMBER
#define NO_AVL_INSERT
#define NO_AVL_DELETE
#define NO_AVL_DELETE1
#define NO_AVL_LEAST
#define NO_AVL_MOVE
#define NO_AVL_SORT
#include "avl.c"
#include "avl.c"  /* This defines the NODE type. */

typedef struct {
  NODE* tree;
  uintL count;
  NODE* free_nodes;
  uintL free_count;
} hs_sorted_t;

typedef struct {
  bool decrementing; /* incrementing or decrementing */
  hs_sorted_t structure_classes;
  hs_sorted_t standard_classes;
  hs_record_t builtins[(int)enum_hs_dummy];
} hs_locals_t;

/* Initialize a hs_locals_t.
   NB: This does stack allocation on the caller's stack. */
#define init_hs_locals(locals)  \
  var DYNAMIC_ARRAY(free_room,NODE, (locals.structure_classes.free_count = posfixnum_to_V(O(structure_class_count_max))) + (locals.standard_classes.free_count = posfixnum_to_V(O(standard_class_count_max)))); \
  init_hs_locals_rest(&locals,free_room);
#define done_hs_locals(locals)  \
  FREE_DYNAMIC_ARRAY(free_room);

local void init_hs_locals_rest (hs_locals_t* locals, NODE* free_room)
{
  locals->decrementing = false;
  /* Initialize all counters to 0. */
  locals->structure_classes.tree = EMPTY;
  locals->standard_classes.tree = EMPTY;
  locals->structure_classes.count = 0;
  locals->standard_classes.count = 0;
  locals->structure_classes.free_nodes = &free_room[0];
  locals->standard_classes.free_nodes = &free_room[locals->structure_classes.free_count];
  {
    var uintC count;
    var hs_record_t* ptr = &locals->builtins[0];
    var const gcv_object_t* optr = &O(hs_t);
    dotimesC(count,(uintC)enum_hs_dummy, {
      ptr->name = optr;
      ptr->n_instances = 0;
      ptr->n_bytes = 0;
      ptr++; optr++;
    });
  }
  /* Prepare for STACK allocation. */
  get_space_on_STACK(sizeof(gcv_object_t) * (locals->structure_classes.free_count + locals->standard_classes.free_count));
}

/* This is the function we pass to map_heap_objects(). */
local void heap_statistics_mapper (void* arg, object obj, uintM bytelen)
{
  var hs_locals_t* locals = (hs_locals_t*)arg;
  var hs_record_t* pighole; /* `pighole' stands for `pigeon-hole' */
 #ifdef TYPECODES
  switch (typecode(obj))
 #else
  if (orecordp(obj)) {
    goto case_orecord;
  } else if (consp(obj)) {
    goto case_cons;
  } else if (immsubrp(obj)) {
    goto case_subr;
  } else
    switch (0)
 #endif
  {
    instances:
    case_instance: { /* instance */
      if (record_flags(TheInstance(obj)) & instflags_forwarded_B) {
        pighole = &locals->builtins[(int)enum_hs_realloc_instance];
        break;
      }
      var object cv = TheInstance(obj)->inst_class_version;
      var object clas = TheClassVersion(cv)->cv_newest_class;
      var NODE* found = AVL(AVLID,member0)(clas,locals->standard_classes.tree);
      if (found == (NODE*)NULL) {
        if (locals->standard_classes.free_count == 0) { /* shouldn't happen */
          pighole = &locals->builtins[(int)enum_hs_t]; break;
        }
        locals->standard_classes.free_count--;
        found = locals->standard_classes.free_nodes++;
        pushSTACK(clas);
        found->nodedata.value.name        = &STACK_0;
        found->nodedata.value.n_instances = 0;
        found->nodedata.value.n_bytes     = 0;
        locals->standard_classes.tree = AVL(AVLID,insert1)(found,locals->standard_classes.tree);
        locals->standard_classes.count++;
      }
      pighole = &found->nodedata.value;
      break;
    }
    case_structure: { /* Structure */
      var object name = Car(TheStructure(obj)->structure_types);
      var NODE* found = AVL(AVLID,member0)(name,locals->structure_classes.tree);
      if (found == (NODE*)NULL) {
        if (locals->structure_classes.free_count == 0) { /* shouldn't happen */
          pighole = &locals->builtins[(int)enum_hs_t]; break;
        }
        locals->structure_classes.free_count--;
        found = locals->structure_classes.free_nodes++;
        pushSTACK(name);
        found->nodedata.value.name        = &STACK_0;
        found->nodedata.value.n_instances = 0;
        found->nodedata.value.n_bytes     = 0;
        locals->structure_classes.tree = AVL(AVLID,insert1)(found,locals->structure_classes.tree);
        locals->structure_classes.count++;
      }
      pighole = &found->nodedata.value;
      break;
    }
    case_cons: /* Cons */
      pighole = &locals->builtins[(int)enum_hs_cons];
      break;
    case_symbol: /* Symbol */
      if (nullp(obj))
        pighole = &locals->builtins[(int)enum_hs_null];
      else
        pighole = &locals->builtins[(int)enum_hs_symbol];
      break;
    case_sbvector: /* Simple-Bit-Vector */
      pighole = &locals->builtins[(int)enum_hs_simple_bit_vector];
      break;
    case_sb2vector: /* Simple-2Bit-Vector */
      pighole = &locals->builtins[(int)enum_hs_simple_2bit_vector];
      break;
    case_sb4vector: /* Simple-4Bit-Vector */
      pighole = &locals->builtins[(int)enum_hs_simple_4bit_vector];
      break;
    case_sb8vector: /* Simple-8Bit-Vector */
      pighole = &locals->builtins[(int)enum_hs_simple_8bit_vector];
      break;
    case_sb16vector: /* Simple-16Bit-Vector */
      pighole = &locals->builtins[(int)enum_hs_simple_16bit_vector];
      break;
    case_sb32vector: /* Simple-32Bit-Vector */
      pighole = &locals->builtins[(int)enum_hs_simple_32bit_vector];
      break;
    case_sstring: /* Simple-String */
      #ifdef HAVE_SMALL_SSTRING
      if (sstring_reallocatedp(TheSstring(obj))) {
        pighole = &locals->builtins[(int)enum_hs_realloc_simple_string];
        break;
      }
      #endif
      pighole = &locals->builtins[(int)enum_hs_simple_string];
      break;
    case_svector: /* Simple-Vector */
      pighole = &locals->builtins[(int)enum_hs_simple_vector];
      break;
    case_obvector: /* other Bit-Vector */
      pighole = &locals->builtins[(int)enum_hs_bit_vector];
      break;
    case_ob2vector: /* other 2Bit-Vector */
      pighole = &locals->builtins[(int)enum_hs_2bit_vector];
      break;
    case_ob4vector: /* other 4Bit-Vector */
      pighole = &locals->builtins[(int)enum_hs_4bit_vector];
      break;
      case_ob8vector: /* other 8Bit-Vector */
      pighole = &locals->builtins[(int)enum_hs_8bit_vector];
      break;
    case_ob16vector: /* other 16Bit-Vector */
      pighole = &locals->builtins[(int)enum_hs_16bit_vector];
      break;
    case_ob32vector: /* other 32Bit-Vector */
      pighole = &locals->builtins[(int)enum_hs_32bit_vector];
      break;
    case_ostring: /* other String */
      if ((Iarray_flags(obj) & arrayflags_atype_mask) == Atype_NIL) {
        if ((Iarray_flags(obj)
             & (  bit(arrayflags_adjustable_bit)
                | bit(arrayflags_fillp_bit)
                | bit(arrayflags_displaced_bit)
                | bit(arrayflags_dispoffset_bit)))
            == 0)
          pighole = &locals->builtins[(int)enum_hs_simple_nilvector];
        else
          pighole = &locals->builtins[(int)enum_hs_nilvector];
      } else
        pighole = &locals->builtins[(int)enum_hs_string];
      break;
    case_ovector: /* other general-vector */
      pighole = &locals->builtins[(int)enum_hs_vector];
      break;
    case_mdarray: /* other Array */
      if ((Iarray_flags(obj)
           & (  bit(arrayflags_adjustable_bit)
              | bit(arrayflags_fillp_bit)
              | bit(arrayflags_displaced_bit)
              | bit(arrayflags_dispoffset_bit)))
          == 0)
        pighole = &locals->builtins[(int)enum_hs_simple_array];
      else
        pighole = &locals->builtins[(int)enum_hs_array];
      break;
    case_closure: /* Closure */
      if (Closure_instancep(obj))
        goto instances;
      pighole = &locals->builtins[(int)enum_hs_function];
      break;
    case_stream: /* Stream */
      switchu (TheStream(obj)->strmtype) {
        case strmtype_file:
          pighole = &locals->builtins[(int)enum_hs_file_stream]; break;
        case strmtype_synonym:
          pighole = &locals->builtins[(int)enum_hs_synonym_stream]; break;
        case strmtype_broad:
          pighole = &locals->builtins[(int)enum_hs_broadcast_stream]; break;
        case strmtype_concat:
          pighole = &locals->builtins[(int)enum_hs_concatenated_stream]; break;
        case strmtype_twoway:
          pighole = &locals->builtins[(int)enum_hs_two_way_stream]; break;
        case strmtype_echo:
          pighole = &locals->builtins[(int)enum_hs_echo_stream]; break;
        case strmtype_str_in:
        case strmtype_str_out:
        case strmtype_str_push:
          pighole = &locals->builtins[(int)enum_hs_string_stream]; break;
        default:
          pighole = &locals->builtins[(int)enum_hs_stream]; break;
      }
      break;
    case_orecord: case_lrecord: /* OtherRecord */
      switch (Record_type(obj)) {
        case_Rectype_Instance_above;
        case_Rectype_Structure_above;
        case_Rectype_Symbol_above;
        case_Rectype_Sbvector_above;
        case_Rectype_Sb2vector_above;
        case_Rectype_Sb4vector_above;
        case_Rectype_Sb8vector_above;
        case_Rectype_Sb16vector_above;
        case_Rectype_Sb32vector_above;
        case_Rectype_Sstring_above;
        case_Rectype_Svector_above;
        case_Rectype_obvector_above;
        case_Rectype_ob2vector_above;
        case_Rectype_ob4vector_above;
        case_Rectype_ob8vector_above;
        case_Rectype_ob16vector_above;
        case_Rectype_ob32vector_above;
        case_Rectype_ostring_above;
        case_Rectype_ovector_above;
        case_Rectype_mdarray_above;
        case_Rectype_Closure_above;
        case_Rectype_Stream_above;
        case_Rectype_Bignum_above;
        case_Rectype_Ratio_above;
        case_Rectype_Ffloat_above;
        case_Rectype_Dfloat_above;
        case_Rectype_Lfloat_above;
        case_Rectype_Complex_above;
        case_Rectype_Subr_above;
        case Rectype_Hashtable: /* Hash-Table */
          pighole = &locals->builtins[(int)enum_hs_hash_table]; break;
        case Rectype_Package: /* Package */
          pighole = &locals->builtins[(int)enum_hs_package]; break;
        case Rectype_Readtable: /* Readtable */
          pighole = &locals->builtins[(int)enum_hs_readtable]; break;
        case Rectype_Pathname: /* Pathname */
          pighole = &locals->builtins[(int)enum_hs_pathname]; break;
       #ifdef LOGICAL_PATHNAMES
        case Rectype_Logpathname: /* Logical Pathname */
          pighole = &locals->builtins[(int)enum_hs_logical_pathname]; break;
       #endif
        case Rectype_Random_State: /* Random-State */
          pighole = &locals->builtins[(int)enum_hs_random_state]; break;
        case Rectype_Byte: /* Byte */
          pighole = &locals->builtins[(int)enum_hs_byte]; break;
        case Rectype_Fsubr: /* Fsubr */
          pighole = &locals->builtins[(int)enum_hs_special_operator]; break;
        case Rectype_Loadtimeeval: /* Load-Time-Eval */
          pighole = &locals->builtins[(int)enum_hs_load_time_eval]; break;
        case Rectype_Symbolmacro: /* Symbol-Macro */
          pighole = &locals->builtins[(int)enum_hs_symbol_macro]; break;
        case Rectype_GlobalSymbolmacro: /* Global-Symbol-Macro */
          pighole = &locals->builtins[(int)enum_hs_global_symbol_macro]; break;
        case Rectype_Macro: /* Macro */
          pighole = &locals->builtins[(int)enum_hs_macro]; break;
        case Rectype_FunctionMacro: /* FunctionMacro */
          pighole = &locals->builtins[(int)enum_hs_function_macro]; break;
        case Rectype_BigReadLabel: /* BigReadLabel */
          pighole = &locals->builtins[(int)enum_hs_big_read_label]; break;
        case Rectype_Encoding: /* Encoding */
          pighole = &locals->builtins[(int)enum_hs_encoding]; break;
       #ifdef FOREIGN
        case Rectype_Fpointer: /* Foreign-Pointer-Wrapping */
          pighole = &locals->builtins[(int)enum_hs_foreign_pointer]; break;
       #endif
       #ifdef DYNAMIC_FFI
        case Rectype_Faddress: /* Foreign-Address */
          pighole = &locals->builtins[(int)enum_hs_foreign_address]; break;
        case Rectype_Fvariable: /* Foreign-Variable */
          pighole = &locals->builtins[(int)enum_hs_foreign_variable]; break;
        case Rectype_Ffunction: /* Foreign-Function */
          pighole = &locals->builtins[(int)enum_hs_foreign_function]; break;
       #endif
        case Rectype_Weakpointer: /* Weak-Pointer */
          pighole = &locals->builtins[(int)enum_hs_weakpointer]; break;
        case Rectype_MutableWeakList: /* mutable Weak-List */
          pighole = &locals->builtins[(int)enum_hs_weak_list]; break;
        case Rectype_MutableWeakAlist: /* mutable Weak-Alist */
          pighole = &locals->builtins[(int)enum_hs_weak_alist]; break;
        case Rectype_Weakmapping: /* Weak-Mapping */
          pighole = &locals->builtins[(int)enum_hs_weakmapping]; break;
        case Rectype_Finalizer: /* Finalizer */
          pighole = &locals->builtins[(int)enum_hs_finalizer]; break;
       #ifdef SOCKET_STREAMS
        case Rectype_Socket_Server: /* Socket-Server */
          pighole = &locals->builtins[(int)enum_hs_socket_server]; break;
       #endif
       #ifdef YET_ANOTHER_RECORD
        case Rectype_Yetanother: /* Yetanother */
          pighole = &locals->builtins[(int)enum_hs_yetanother]; break;
       #endif
        case Rectype_WeakList: /* Weak-List */
          pighole = &locals->builtins[(int)enum_hs_internal_weak_list]; break;
        case Rectype_WeakAnd: /* Weak-And-Relation */
          pighole = &locals->builtins[(int)enum_hs_weak_and_relation]; break;
        case Rectype_WeakOr: /* Weak-Or-Relation */
          pighole = &locals->builtins[(int)enum_hs_weak_or_relation]; break;
        case Rectype_WeakAndMapping: /* Weak-And-Mapping */
          pighole = &locals->builtins[(int)enum_hs_weak_and_mapping]; break;
        case Rectype_WeakOrMapping: /* Weak-Or-Mapping */
          pighole = &locals->builtins[(int)enum_hs_weak_or_mapping]; break;
        case Rectype_WeakAlist_Key:
        case Rectype_WeakAlist_Value:
        case Rectype_WeakAlist_Either:
        case Rectype_WeakAlist_Both: /* Weak-Alist */
          pighole = &locals->builtins[(int)enum_hs_internal_weak_alist]; break;
        case Rectype_WeakHashedAlist_Key:
        case Rectype_WeakHashedAlist_Value:
        case Rectype_WeakHashedAlist_Either:
        case Rectype_WeakHashedAlist_Both: /* Weak-Hashed-Alist */
          pighole = &locals->builtins[(int)enum_hs_internal_weak_hashed_alist]; break;
        default:
          pighole = &locals->builtins[(int)enum_hs_t]; break;
      }
      break;
    case_subr: /* SUBR */
      pighole = &locals->builtins[(int)enum_hs_system_function];
      break;
    case_bignum: /* Bignum */
      pighole = &locals->builtins[(int)enum_hs_bignum];
      break;
    case_ratio: /* Ratio */
      pighole = &locals->builtins[(int)enum_hs_ratio];
      break;
   #ifndef IMMEDIATE_FFLOAT
    case_ffloat: /* Single-Float */
      pighole = &locals->builtins[(int)enum_hs_single_float];
      break;
   #endif
    case_dfloat: /* Double-Float */
      pighole = &locals->builtins[(int)enum_hs_double_float];
      break;
    case_lfloat: /* Long-Float */
      pighole = &locals->builtins[(int)enum_hs_long_float];
      break;
    case_complex: /* Complex */
      pighole = &locals->builtins[(int)enum_hs_complex];
      break;
    default:
      pighole = &locals->builtins[(int)enum_hs_t]; break;
  }
  if (locals->decrementing) {
    pighole->n_instances -= 1;
    pighole->n_bytes -= bytelen;
  } else {
    pighole->n_instances += 1;
    pighole->n_bytes += bytelen;
  }
}

/* Creates a statistics record for the objects of a given type.
 can trigger GC */
local maygc object heap_statistics_record (object type, sintL n_instances, sintM n_bytes) {
  pushSTACK(type);
  pushSTACK(L_to_I(n_instances));
  pushSTACK(sintM_to_I(n_bytes));
  var object hsr = make_list(2);
  Cdr(Cdr(hsr)) = popSTACK();
  Car(Cdr(hsr)) = popSTACK();
  Car(hsr) = popSTACK();
  return hsr;
}

/* Creates a vector containing the heap statistics result,
 and pushes it onto the STACK.
 can trigger GC */
local maygc void heap_statistics_result (hs_locals_t* locals)
{
  /* Allocate result vector. */
  var uintL length = (uintL)enum_hs_dummy
    + locals->structure_classes.count
    + locals->standard_classes.count;
  pushSTACK(allocate_vector(length));
  var gcv_object_t* result_ = &STACK_0;
  /* Fill result vector. */
  var uintL i = 0;
  {
    var uintC count;
    var hs_record_t* ptr = &locals->builtins[0];
    dotimesC(count,(uintC)enum_hs_dummy, {
      var object hsr =
        heap_statistics_record(*ptr->name,ptr->n_instances,ptr->n_bytes);
      TheSvector(*result_)->data[i] = hsr;
      ptr++; i++;
    });
  }
  {
    var uintL count = locals->structure_classes.count;
    if (count > 0) {
      var NODE* ptr = locals->structure_classes.free_nodes;
      dotimespL(count,count, {
        --ptr;
        var object hsr =
          heap_statistics_record(*ptr->nodedata.value.name,
                                 ptr->nodedata.value.n_instances,
                                 ptr->nodedata.value.n_bytes);
        TheSvector(*result_)->data[i] = hsr;
        i++;
      });
    }
  }
  {
    var uintL count = locals->standard_classes.count;
    if (count > 0) {
      var NODE* ptr = locals->standard_classes.free_nodes;
      dotimespL(count,count, {
        --ptr;
        var object hsr =
          heap_statistics_record(TheClass(*ptr->nodedata.value.name)->classname,
                                 ptr->nodedata.value.n_instances,
                                 ptr->nodedata.value.n_bytes);
        TheSvector(*result_)->data[i] = hsr;
        i++;
      });
    }
  }
}

LISPFUNN(heap_statistics,0)
{
  var hs_locals_t locals;
  init_hs_locals(locals);
  /* Walk through memory. */
  map_heap_objects(&heap_statistics_mapper,&locals);
  /* Allocate and fill result vector. */
  heap_statistics_result(&locals);
  /* Done. */
  VALUES1(popSTACK());
  skipSTACK(locals.structure_classes.count + locals.standard_classes.count);
  done_hs_locals(locals);
}

LISPFUNN(gc_statistics,0)
{
  VALUES1(O(gc_statistics_list));
}

/* UP: keeps statistics on the action of a GC.
 with_gc_statistics(fun);
 > fun: function, that triggers a GC */
global void with_gc_statistics (gc_function_t* fun) {
  var object flag = Symbol_value(S(gc_statistics_stern));
  if (!posfixnump(flag)) {
    /* No need to do statistics, throw old ones away. */
    O(gc_statistics_list) = NIL; fun();
  } else if (eq(flag,Fixnum_0)) {
    /* No need to do statistics, but keep old ones. */
    fun();
  } else {
    /* Do statistics. */
    var hs_locals_t locals;
    init_hs_locals(locals);
    /* Walk through memory. */
    map_heap_objects(&heap_statistics_mapper,&locals);
   #ifdef DEBUG_SPVW
    fprintf(stderr,"[%d] with_gc_statistics: starting a GC...",gc_count);
   #endif
    /* Now do the GC. */
    fun();
   #ifdef DEBUG_SPVW
    fprintf(stderr,"done [%d]\n",free_space());
   #endif
    /* Walk through memory again, this time decrementing. */
    locals.decrementing = true;
    map_heap_objects(&heap_statistics_mapper,&locals);
    /* Now if in the following allocation requests, a GC occurs, we
       might mix up the order of the records in O(gc_statistics_list),
       but that's not relevant.
       But if memory is full, we might be called recursively. A recursive
       depth of 1 is OK (that's normal), but a greater recursion depth
       is a sign of an infinite recursion. In this case we rebind
       SYSTEM::*GC-STATISTICS* to 0. */
    var bool danger = false;
    dynamic_bind(S(recurse_count_gc_statistics),fixnum_inc(Symbol_value(S(recurse_count_gc_statistics)),1)); /* increase sys::*recurse-count-gc-statistics* */
    if (!posfixnump(Symbol_value(S(recurse_count_gc_statistics)))) /* should be a Fixnum >=0 */
      Symbol_value(S(recurse_count_gc_statistics)) = Fixnum_0; /* otherwise emergency correction */
    if (posfixnum_to_V(Symbol_value(S(recurse_count_gc_statistics))) > 3) {
      /* recursion depth too big. */
      danger = true;
      dynamic_bind(S(gc_statistics_stern),Fixnum_0);
    }
    /* Allocate and fill result vector. */
    heap_statistics_result(&locals);
    { /* Push it onto O(gc_statistics_list). */
      var object new_cons = allocate_cons();
      Car(new_cons) = popSTACK(); Cdr(new_cons) = O(gc_statistics_list);
      O(gc_statistics_list) = new_cons;
    }
    /* Done. */
    if (danger) { dynamic_unbind(S(gc_statistics_stern)); }
    dynamic_unbind(S(recurse_count_gc_statistics));
    skipSTACK(locals.structure_classes.count + locals.standard_classes.count);
    done_hs_locals(locals);
  }
}

local Values statistics_statistics (uintL svector_instances,
                                    uintL svector_bytes, uintL cons_instances)
{
  {
    var object hsr = make_list(2);
    Car(hsr) = O(hs_simple_vector);
    Car(Cdr(hsr)) = fixnum(svector_instances);
    Cdr(Cdr(hsr)) = fixnum(svector_bytes);
    pushSTACK(hsr);
  }
  {
    var object hsr = make_list(2);
    Car(hsr) = O(hs_cons);
    Car(Cdr(hsr)) = fixnum(cons_instances);
    Cdr(Cdr(hsr)) = fixnum(cons_instances*sizeof(cons_));
    pushSTACK(hsr);
  }
  VALUES1(vectorof(2));
}

LISPFUNN(list_statistics,1)
{ /* (SYSTEM::LIST-STATISTICS list)
   Return statistics about how much a list uses. */
  statistics_statistics(0,0,llength(popSTACK()));
}

LISPFUNN(heap_statistics_statistics,1)
{ /* (SYSTEM::HEAP-STATISTICS-STATISTICS statistics)
   Return statistics about how much a statistics vector uses. */
  var object obj = popSTACK();
  ASSERT(simple_vector_p(obj));
  statistics_statistics(1,varobject_bytelength(obj),Svector_length(obj)*2);
}

LISPFUNN(gc_statistics_statistics,2)
{ /* (SYSTEM::GC-STATISTICS-STATISTICS statlist1 statlist2)
   Return statistics about how much the GC statistics used up between two calls
   to the function SYSTEM::GC-STATISTICS. */
  var object statlist2 = popSTACK();
  var object statlist1 = popSTACK();
  var uintL svector_instances = 0;
  var uintL svector_bytes = 0;
  var uintL cons_instances = 0;
  while (consp(statlist2) && !eq(statlist2,statlist1)) {
    var object obj = Car(statlist2);
    ASSERT(simple_vector_p(obj));
    svector_instances += 1;
    svector_bytes += varobject_bytelength(obj);
    cons_instances += 1 + Svector_length(obj)*2;
    statlist2 = Cdr(statlist2);
  }
  statistics_statistics(svector_instances,svector_bytes,cons_instances);
}
