/* Recursive marking routine. */

local void gc_mark (object obj)
{
  var object dies = obj; /* current object */
  var object vorg = nullobj; /* predecessor-object */
  IF_DEBUG_GC_MARK(fprintf(stderr,"gc_mark obj = 0x%"PRIoint"x\n", as_oint(obj)));

#define down_pair()                                                     \
  if (in_old_generation(dies,typecode(dies),1))                         \
    goto up; /* do not mark older generation */                         \
  { var gcv_object_t* dies_ = (gcv_object_t*)ThePointer(dies);          \
    if (marked(dies_)) goto up; /* marked -> go up */                   \
    MARK(dies_); /* mark */                                             \
  }                                                                     \
  { var object dies_ = objectplus(dies,(soint)(sizeof(cons_)-sizeof(gcv_object_t))<<(oint_addr_shift-addr_shift)); \
    /* start with the last pointer */                                   \
    var object nachf = *(gcv_object_t*)ThePointer(dies_); /* successor */ \
    *(gcv_object_t*)ThePointer(dies_) = vorg; /* store predecessor */   \
    vorg = dies_; /* current object becomes new predecessor */          \
    dies = nachf; /* successor becomes current object */                \
    goto down; /* and descent */                                        \
  }
#define up_pair()                                       \
  { MARK(ThePointer(vorg)); /* mark again */            \
    dies = vorg; /* Cons becomes object */              \
    vorg = vorvorg; goto up; /* go further up */        \
  }
#define down_varobject(The,first_offset,last_offset)                    \
  if (in_old_generation(dies,typecode(dies),0))                         \
    goto up; /* do not mark older generation */                         \
  { var gcv_object_t* dies_ = (gcv_object_t*)The(dies);                 \
    if (marked(dies_)) goto up; /* marked -> up */                      \
    MARK(dies_); /* mark */                                             \
    mark(pointerplus(dies_,first_offset)); /* mark first pointer */     \
  }                                                                     \
  { var object dies_ = objectplus(dies,(soint)(last_offset)<<(oint_addr_shift-addr_shift)); \
    /* start with the last pointer */                                   \
    var object nachf = *(gcv_object_t*)The(dies_); /* successor */      \
    *(gcv_object_t*)The(dies_) = vorg; /* store predecessor */          \
    vorg = dies_; /* current object becomes new predecessor */          \
    dies = nachf; /* predecessor becomes current object */              \
    goto down; /* and descent */                                        \
  }
#define up_varobject(first_offset)                                      \
  { dies = objectplus(vorg,-(soint)(first_offset)<<(oint_addr_shift-addr_shift)); /* becomes current object */ \
    vorg = vorvorg; goto up; /* go further up */                        \
  }
#define down_nopointers(The)                    \
  if (in_old_generation(dies,typecode(dies),0)) \
    goto up; /* do not mark older generation */ \
  MARK(The(dies)); /* mark */                   \
  goto up; /* and up */
#define down_iarray()                                                   \
  if (in_old_generation(dies,typecode(dies),0))                         \
    goto up; /* do not mark older generation */                         \
  { var gcv_object_t* dies_ = (gcv_object_t*)TheIarray(dies);           \
    if (marked(dies_)) goto up; /* marked -> up */                      \
    MARK(dies_); /* mark */                                             \
  }                                                                     \
  { var object dies_ = objectplus(dies,(soint)(iarray_data_offset)<<(oint_addr_shift-addr_shift)); \
    /* data vector is the first and only pointer */                     \
    var object nachf = *(gcv_object_t*)TheIarray(dies_); /* successor */ \
    *(gcv_object_t*)TheIarray(dies_) = vorg; /* store predecessor */    \
    MARK(TheIarray(dies_)); /* mark first and only pointer */           \
    vorg = dies_; /* current object becomes new predecessor */          \
    dies = nachf; /* predecessor becomes current object */              \
    goto down; /* and descent */                                        \
  }
#define up_iarray()                                                     \
  { dies = objectplus(vorg,-(soint)iarray_data_offset<<(oint_addr_shift-addr_shift)); /* array becomes current object */ \
    vorg = vorvorg; goto up; /* go further up */                        \
  }
#define down_sistring()                                                 \
  if (in_old_generation(dies,typecode(dies),0))                         \
    goto up; /* do not mark older generation */                         \
  { var gcv_object_t* dies_ = (gcv_object_t*)TheSistring(dies);         \
    if (marked(dies_)) goto up; /* marked -> up */                      \
    MARK(dies_); /* mark */                                             \
  }                                                                     \
  { var object dies_ = objectplus(dies,(soint)(sistring_data_offset)<<(oint_addr_shift-addr_shift)); \
    /* data vector is the first and only pointer */                     \
    var object nachf = *(gcv_object_t*)TheSistring(dies_); /* successor */ \
    *(gcv_object_t*)TheSistring(dies_) = vorg; /* store predecessor */  \
    MARK(TheSistring(dies_)); /* mark first and only pointer */         \
    vorg = dies_; /* current object becomes new predecessor */          \
    dies = nachf; /* predecessor becomes current object */              \
    goto down; /* and descent */                                        \
  }
#define up_sistring()                                                   \
  { dies = objectplus(vorg,-(soint)sistring_data_offset<<(oint_addr_shift-addr_shift)); /* array becomes current object */ \
    vorg = vorvorg; goto up; /* go further up */                        \
  }
#define down_svector()                                                  \
  if (in_old_generation(dies,typecode(dies),0))                         \
    goto up; /* do not mark older generation */                         \
  { var gcv_object_t* dies_ = (gcv_object_t*)TheSvector(dies);          \
    if (marked(dies_)) goto up; /* marked -> up */                      \
    MARK(dies_); /* mark */                                             \
  }                                                                     \
  { var uintL len = Svector_length(dies);                               \
    if (len==0) goto up; /* Length 0: up again */                       \
   {var object dies_ = objectplus(dies,((soint)offsetofa(svector_,data) << (oint_addr_shift-addr_shift)) \
    /* the "<< 1" and "/2" are a workaround against a gcc-2.7.2         \
       missed optimization in WIDE_SOFT mode */                         \
      + (((soint)len << 1) * (soint)(sizeof(gcv_object_t)/2) << (oint_addr_shift-addr_shift)) \
      - ((soint)sizeof(gcv_object_t) << (oint_addr_shift-addr_shift)) ); \
    /* start with the last pointer */                                   \
    var object nachf = *(gcv_object_t*)TheSvector(dies_); /* successor */ \
    *(gcv_object_t*)TheSvector(dies_) = vorg; /* store predecessor */   \
    mark(&TheSvector(dies)->data[0]); /* mark first pointer */          \
    vorg = dies_; /* current object becomes new predecessor */          \
    dies = nachf; /* predecessor becomes current object */              \
    goto down; /* and descent */                                        \
  }}
#define up_svector()                            \
  { dies = objectplus(vorg,-(soint)offsetofa(svector_,data)<<(oint_addr_shift-addr_shift)); /* Svector becomes current object */ \
    vorg = vorvorg; goto up; /* go further up */ \
  }
#define down_lrecord()                                                  \
  if (in_old_generation(dies,typecode(dies),0))                         \
    goto up; /* do not mark older generation */                         \
  { var gcv_object_t* dies_ = (gcv_object_t*)TheLrecord(dies);          \
    if (marked(dies_)) goto up; /* marked -> up */                      \
    MARK(dies_); /* marked */                                           \
  }                                                                     \
  { var uintL len = Lrecord_nonweak_length(dies);                       \
    if (len==0) goto up; /* Length 0: up again */                       \
   {var object dies_ = objectplus(dies,((soint)offsetofa(record_,recdata) << (oint_addr_shift-addr_shift)) \
    /* the "<< 1" and "/2" are a workaround against a gcc-2.7.2         \
       missed optimization in WIDE_SOFT mode */                         \
      + (((soint)len << 1) * (soint)(sizeof(gcv_object_t)/2) << (oint_addr_shift-addr_shift)) \
      - ((soint)sizeof(gcv_object_t) << (oint_addr_shift-addr_shift)) ); \
    /* start with the last pointer */                                   \
    var object nachf = *(gcv_object_t*)TheLrecord(dies_); /* successor */ \
    *(gcv_object_t*)TheLrecord(dies_) = vorg; /* store predecessor */   \
    mark(&TheLrecord(dies)->recdata[0]); /* mark first pointer */       \
    vorg = dies_; /* current object becomes new predecessor */          \
    dies = nachf; /* predecessor becomes current object */              \
    goto down; /* and descent */                                        \
  }}
#define up_lrecord()                             \
  { dies = objectplus(vorg,-(soint)offsetofa(record_,recdata)<<(oint_addr_shift-addr_shift)); /* Lrecord becomes current object */ \
    vorg = vorvorg; goto up; /* go further up */ \
  }
#define down_sxrecord()                                                 \
  if (in_old_generation(dies,typecode(dies),0))                         \
    goto up; /* do not mark older generation */                         \
  { var gcv_object_t* dies_ = (gcv_object_t*)TheRecord(dies);           \
    if (marked(dies_)) goto up; /* marked -> up */                      \
    MARK(dies_); /* marked */                                           \
  }                                                                     \
  { var uintL len = SXrecord_nonweak_length(dies);                      \
    if (len==0) goto up; /* Length 0: up again */                       \
   {var object dies_ = objectplus(dies,((soint)offsetofa(record_,recdata) << (oint_addr_shift-addr_shift)) \
    /* the "<< 1" and "/2" are a workaround against a gcc-2.7.2         \
       missed optimization in WIDE_SOFT mode */                         \
      + (((soint)len << 1) * (soint)(sizeof(gcv_object_t)/2) << (oint_addr_shift-addr_shift)) \
      - ((soint)sizeof(gcv_object_t) << (oint_addr_shift-addr_shift)) ); \
    /* start with the last pointer */                                   \
    var object nachf = *(gcv_object_t*)TheRecord(dies_); /* successor */ \
    *(gcv_object_t*)TheRecord(dies_) = vorg; /* store predecessor */    \
    mark(&TheRecord(dies)->recdata[0]); /* mark first pointer */        \
    vorg = dies_; /* current object becomes new predecessor */          \
    dies = nachf; /* predecessor becomes current object */              \
    goto down; /* and descent */                                        \
  }}
#define up_sxrecord()                             \
  { dies = objectplus(vorg,-(soint)offsetofa(record_,recdata)<<(oint_addr_shift-addr_shift)); /* record becomes current object */ \
    vorg = vorvorg; goto up; /* go further up */  \
  }
#ifdef STANDARD_HEAPCODES
#define down_subr()                                                     \
  if (in_old_generation(dies,typecode(dies),0))                         \
    goto up; /* do not mark older generation */                         \
  { var gcv_object_t* dies_ = (gcv_object_t*)TheSubr(dies);             \
    if (marked(dies_)) goto up; /* marked -> up */                      \
    MARK(dies_); /* marked */                                           \
  }                                                                     \
  { var object dies_ = objectplus(dies,((soint)offsetofa(record_,recdata) << (oint_addr_shift-addr_shift)) \
      + ((soint)subr_length * (soint)sizeof(gcv_object_t) << (oint_addr_shift-addr_shift)) \
      - ((soint)sizeof(gcv_object_t) << (oint_addr_shift-addr_shift)) ); \
    /* start with the last pointer */                                   \
    var object nachf = *(gcv_object_t*)TheSubr(dies_); /* successor */  \
    *(gcv_object_t*)TheSubr(dies_) = vorg; /* store predecessor */      \
    mark(&((Record)TheSubr(dies))->recdata[0]); /* mark first pointer */ \
    vorg = dies_; /* current object becomes new predecessor */          \
    dies = nachf; /* predecessor becomes current object */              \
    goto down; /* and descent */                                        \
  }
#define up_subr()                                                       \
  { dies = objectplus(vorg,-(soint)offsetofa(record_,recdata)<<(oint_addr_shift-addr_shift)); /* SUBR becomes current object */ \
    vorg = vorvorg; goto up; /* go further up */  \
  }
#endif

 down: /* entry for further descent.
          dies = object to be marked (engl. this),
          vorg = its predecessor */
  IF_DEBUG_GC_MARK(fprintf(stderr,"down: vorg = 0x%"PRIoint"x, dies = 0x%"PRIoint"x\n",
                           as_oint(vorg), as_oint(dies)));
 #ifdef TYPECODES
  switch (typecode(dies)) {
    case_pair: /* object with exactly two 2 pointers (Cons and similar) */
      down_pair();
    case_symbol: /* Symbol */
      down_varobject(TheSymbol,symbol_objects_offset,
                     sizeof(symbol_)-sizeof(gcv_object_t));
    case_sstring: /* simple-string */
      if (sstring_reallocatedp(TheSstring(dies))) {
        down_sistring();
      }
      /*FALLTHROUGH*/
    case_sbvector: /* simple-bit-vector */
    case_sb2vector: /* simple-2bit-vector */
    case_sb4vector: /* simple-4bit-vector */
    case_sb8vector: /* simple-8bit-vector */
    case_sb16vector: /* simple-16bit-vector */
    case_sb32vector: /* simple-32bit-vector */
    case_bignum: /* bignum */
   #ifndef IMMEDIATE_FFLOAT
    case_ffloat: /* single-float */
   #endif
    case_dfloat: /* double-float */
    case_lfloat: /* long-float */
      /* objects of variable length, that do not contain pointers: */
      down_nopointers(TheVarobject);
    case_mdarray: case_obvector: case_ob2vector: case_ob4vector: case_ob8vector:
    case_ob16vector: case_ob32vector: case_ostring: case_ovector:
      /* arrays, that are not simple: */
      down_iarray();
    case_svector: /* simple-vector */
      down_svector();
    case_lrecord: /* Lrecord */
      down_lrecord();
    case_sxrecord: /* Srecord/Xrecord */
    case_subr: /* SUBR */
      down_sxrecord();
    case_machine: /* machine address */
    case_char: /* character */
    case_system: /* frame-pointer, small-read-label, system */
    case_fixnum: /* fixnum */
    case_sfloat: /* short-float */
   #ifdef IMMEDIATE_FFLOAT
    case_ffloat: /* single-float */
   #endif
      /* These are direct objects, no pointers. */
      goto up;
    default: /* These are no objects. */
      /*NOTREACHED*/ abort();
  }
 #else
  switch (as_oint(dies) & nonimmediate_heapcode_mask) {
    case cons_bias+conses_misaligned: /* cons */
      #ifdef STANDARD_HEAPCODES
      /* NB: (immediate_bias & nonimmediate_heapcode_mask) == cons_bias. */
      if (immediate_object_p(dies)) goto up;
      #endif
      down_pair();
    case varobject_bias+varobjects_misaligned:
      switch (Record_type(dies)) {
        case Rectype_Sbvector:
        case Rectype_Sb2vector:
        case Rectype_Sb4vector:
        case Rectype_Sb8vector:
        case Rectype_Sb16vector:
        case Rectype_Sb32vector:
        case Rectype_S8string: case Rectype_Imm_S8string:
        case Rectype_S16string: case Rectype_Imm_S16string:
        case Rectype_S32string: case Rectype_Imm_S32string:
        case Rectype_Bignum:
        case Rectype_Ffloat:
        case Rectype_Dfloat:
        case Rectype_Lfloat:
          down_nopointers(TheRecord);
        case Rectype_Svector:
          down_svector();
        #ifdef HAVE_SMALL_SSTRING
        case Rectype_reallocstring:
          down_sistring();
        #endif
        case Rectype_mdarray:
        case Rectype_bvector:
        case Rectype_b2vector:
        case Rectype_b4vector:
        case Rectype_b8vector:
        case Rectype_b16vector:
        case Rectype_b32vector:
        case Rectype_string:
        case Rectype_vector:
          down_iarray();
        case Rectype_WeakList:
        case Rectype_WeakAnd:
        case Rectype_WeakOr:
        case Rectype_WeakAndMapping:
        case Rectype_WeakOrMapping:
        case Rectype_WeakAlist_Key:
        case Rectype_WeakAlist_Value:
        case Rectype_WeakAlist_Either:
        case Rectype_WeakAlist_Both:
        case Rectype_WeakHashedAlist_Key:
        case Rectype_WeakHashedAlist_Value:
        case Rectype_WeakHashedAlist_Either:
        case Rectype_WeakHashedAlist_Both: /* Lrecord */
          down_lrecord();
        default: /* Srecord/Xrecord */
          down_sxrecord();
      }
    #ifdef STANDARD_HEAPCODES
    case subr_bias: /* SUBR */
      down_subr();
    #endif
    case machine_bias:
    #ifdef LINUX_NOEXEC_HEAPCODES
    case machine_bias+4:
    #endif
      /* These are direct objects, no pointers. */
      goto up;
    default:
      /*NOTREACHED*/ abort();
  }
 #endif
 up: /* entry for ascent.
        dies = currently marked object, vorg = its predecessor */
  IF_DEBUG_GC_MARK(fprintf(stderr,"up:   vorg = 0x%"PRIoint"x, dies = 0x%"PRIoint"x\n",
                           as_oint(vorg), as_oint(dies)));
  if (eq(vorg,nullobj)) /* ending flag reached? */
    return; /* yes -> finished */
  if (!marked(ThePointer(vorg))) { /* already through? */
    /* no ->
       next element further left (come from 'up', go to 'down')
       dies = currently marked  object, store in *vorg */
    var object vorvorg = *(gcv_object_t*)ThePointer(vorg); /* old predecessor */
    *(gcv_object_t*)ThePointer(vorg) = dies; /* write back component */
    vorg = objectplus(vorg,-(soint)(sizeof(gcv_object_t))<<(oint_addr_shift-addr_shift)); /* go to next component */
    if (marked(ThePointer(vorg))) { /* already marked? */
      dies = /* next component, without mark */
        without_mark_bit(*(gcv_object_t*)ThePointer(vorg));
      *(gcv_object_t*)ThePointer(vorg) = /* further relocate old predecessor, thereby renew mark */
        with_mark_bit(vorvorg);
    } else {
      dies = *(gcv_object_t*)ThePointer(vorg); /* next component, without mark */
      *(gcv_object_t*)ThePointer(vorg) = vorvorg; /* further relocate old predecessor */
    }
    goto down;
  }
  { /* already through -> ascent again */
    var object vorvorg = /* fetch old predecessor, without mark bit */
      without_mark_bit(*(gcv_object_t*)ThePointer(vorg));
    *(gcv_object_t*)ThePointer(vorg) = dies; /* write back first component */
   #ifdef TYPECODES
    switch (typecode(vorg)) {
      case_pair: /* object with exactly two pointers (Cons and similar) */
        up_pair();
      case_symbol: /* Symbol */
        up_varobject(symbol_objects_offset);
      case_svector: /* simple-vector with at least 1 component */
        up_svector();
      case_mdarray: case_obvector: case_ob2vector:
      case_ob4vector: case_ob8vector: case_ob16vector:
      case_ob32vector: case_ostring: case_ovector:
        /* non-simple arrays: */
        up_iarray();
      case_lrecord: /* Lrecord */
        up_lrecord();
      case_sxrecord: /* Srecord/Xrecord */
      case_subr: /* SUBR */
        up_sxrecord();
      case_sstring: /* simple-string */
        { var object vorg_ = objectplus(vorg,-(soint)sistring_data_offset<<(oint_addr_shift-addr_shift));
          if (sstring_reallocatedp(TheSstring(vorg_)))
            up_sistring();
        }
        /*FALLTHROUGH*/
      case_machine: /* machine address */
      case_char: /* character */
      case_system: /* frame-pointer, small-read-label, system */
      case_fixnum: /* fixnum */
      case_sfloat: /* short-float */
     #ifdef IMMEDIATE_FFLOAT
      case_ffloat: /* single-float */
     #endif
        /* These are direct objects, no pointers. */
      case_sbvector: /* simple-bit-vector */
      case_sb2vector: /* simple-2bit-vector */
      case_sb4vector: /* simple-4bit-vector */
      case_sb8vector: /* simple-8bit-vector */
      case_sb16vector: /* simple-16bit-vector */
      case_sb32vector: /* simple-32bit-vector */
      case_bignum: /* bignum */
     #ifndef IMMEDIATE_FFLOAT
      case_ffloat: /* single-float */
     #endif
      case_dfloat: /* double-float */
      case_lfloat: /* long-float */
        /* Objects of variable length, that do not contain pointers. */
      default: /* these are no objects. */
        /*NOTREACHED*/ abort();
    }
   #else
    switch (as_oint(vorg) & nonimmediate_heapcode_mask) {
      case cons_bias+conses_misaligned: /* Cons */
        up_pair();
      case varobject_bias+varobjects_misaligned:
        /* This works only because all varobjects have the same
           objects_offset! */
        up_sxrecord();
      #ifdef STANDARD_HEAPCODES
      case subr_bias: /* SUBR */
        up_subr();
      #endif
      default: /* these are no objects. */
        /*NOTREACHED*/ abort();
    }
   #endif
  }
#undef up_subr
#undef down_subr
#undef up_sxrecord
#undef down_sxrecord
#undef up_svector
#undef down_svector
#undef up_lrecord
#undef down_lrecord
#undef up_iarray
#undef down_iarray
#undef down_nopointers
#undef up_varobject
#undef down_varobject
#undef up_pair
#undef down_pair
}
