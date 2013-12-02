# Detection of circularities. Used by the printer and reader.

# ------------------------------ Specification --------------------------------

# get_circularities(obj,pr_array,pr_closure)
# Returns a table of all circularities of an object.
# A circularity is a sub-object, which can be reached from the given object
# through more than one path.
# > object obj: object
# > bool pr_array: if true, elements of arrays are considered sub-objects
#                     during the recursive traversal.
# > bool pr_closure: if true, elements of closures are considered sub-objects
#                       during the recursive traversal.
# < result: T on stack overflow,
#           NIL if there are no circularities,
#           #(0 ...) a vector of length (n+1), containing the integer 0 and
#                    the n circularities, n>0.
# can trigger GC
global maygc object get_circularities (object obj, bool pr_array, bool pr_closure);

# subst_circ(ptr,alist)
# Resolves #n# references in the object *ptr, using the alist as a replacement
# table.
# > *ptr : object
# > alist : alist (read-label --> object to be substituted)
# < *ptr : object with resolved references
# < result : first invalid reference found, or nullobj if everything is OK
global object subst_circ (gcv_object_t* ptr, object alist);
# Note: This substitution must respect circularities, so that it can be
# applied to circular structures, such as values of #. (in particular
# #.(FIND-CLASS 'FOO)).

# ------------------------------ Implementation -------------------------------

# Common subroutines.

#ifdef MULTITHREAD

# Multi-level bit map, used as a hash set. This is a (slower, but reentrant)
# alternative to using the GC's mark bit.

# Subdividing an address into bit packets.
# Try to reduce the number of bit packets, thus reducing the number of
# indirections. But the first one can bit large (because at the end of the
# indirection chain, we have a single bit per sizeof(gcv_object_t), not a big
# pointer. The last ones (>= 22) can be big, because it's not a problem
# if the size of the bitmap grows linearly with process_size/4MB.
#if (oint_addr_len <= 32)
  #define mlb0 0
  #define mlb1 10
  #define mlb2 14
  #define mlb3 18
  #define mlb4 22
  #define mlb5 32
  #define mlb_levels 5
#else
  #define mlb0 0
  #define mlb1 10
  #define mlb2 14
  #define mlb3 18
  #define mlb4 22
  #define mlb5 33
  #define mlb6 64
  #define mlb_levels 6
#endif
#if (mlb_levels >= 5)
  #define mlbs0  (mlb1-mlb0)  # = 10
  #define mlbs1  (mlb2-mlb1)  # = 4
  #define mlbs2  (mlb3-mlb2)  # = 4
  #define mlbs3  (mlb4-mlb3)  # = 4
  #define mlbs4  (mlb5-mlb4)  # >= 10
  #if (mlb_levels >= 6)
    #define mlbs5  (mlb6-mlb5)  # >= 10
  #endif
#endif

# A multi-level bit map.
# It is a hash set providing one bit for every possible object. The index into
# the table is actually an aint which we begin by dividing by sizeof(gcv_object_t).
# (Since any object on heap has a size >= sizeof(gcv_object_t), distinct objects
# have addresses that differ by at least sizeof(gcv_object_t), this will be
# represented by different bits.)
  #if (mlb_levels == 5)
    typedef uintL***** mlbitmap_base_t;
  #endif
  #if (mlb_levels == 6)
    typedef uintL****** mlbitmap_base_t;
  #endif
  typedef struct {
    mlbitmap_base_t base; # start pointer = address of malloc()ed area
    uintL alloc_size;     # size of malloc()ed area
    uintL used_size;      # size of used part; the remainder of the area is zeroed
    jmp_buf oom_context;  # context to jump to in case of malloc/realloc failure
  } mlbitmap;

# Create a multi-level bit map.
# The caller must initialize bitmap->oom_context himself.
  local void mlb_alloc (mlbitmap* bitmap);

# Add an object to a bitmap.
# Returns true if the object was already present, else false.
  local bool mlb_add (mlbitmap* bitmap, object obj);

# Free a multi-level bit map.
  local void mlb_free (mlbitmap* bitmap);

  local void mlb_alloc (mlbitmap* bitmap)
  {
    bitmap->base = NULL;
    bitmap->alloc_size = 0;
    bitmap->used_size = 0;
  }

  # Expand a bitmap so that its alloc_size becomes >= newsize.
  local uintP mlb_expand (mlbitmap* bitmap, uintL newsize)
  {
    if (newsize < 2*bitmap->alloc_size)
      newsize = 2*bitmap->alloc_size;
    begin_system_call();
    var char* newbase = (bitmap->base==NULL ? malloc(newsize) : realloc((char*)bitmap->base,newsize));
    end_system_call();
    if (newbase==NULL)
      longjmp(bitmap->oom_context,true);
    var uintP delta = (uintP)newbase - (uintP)bitmap->base;
    bzero(newbase+bitmap->alloc_size,newsize-bitmap->alloc_size);
    if (bitmap->base) {
      # Relocate the pointers inside the bitmap.
      # We know that they form a tree,
      # therefore a recursive descent reaches every pointer exactly once.
      if (!(delta == 0)) {
        #if (mlb_levels >= 6)
        uintL****** p5 = (mlbitmap_base_t)newbase;
        uintC count5 = bit(mlbs5);
        for (; count5 > 0; p5++, count5--) {
          uintL***** p4 = *p5;
          if (p4) {
            *p5 = p4 = (uintL*****)((uintP)p4 + delta);
        #else
          uintL***** p4 = (mlbitmap_base_t)newbase;
        #endif
            {
              uintC count4 = bit(mlbs4);
              for (; count4 > 0; p4++, count4--) {
                uintL**** p3 = *p4;
                if (p3) {
                  *p4 = p3 = (uintL****)((uintP)p3 + delta);
                  {
                    uintC count3 = bit(mlbs3);
                    for (; count3 > 0; p3++, count3--) {
                      uintL*** p2 = *p3;
                      if (p2) {
                        *p3 = p2 = (uintL***)((uintP)p2 + delta);
                        {
                          uintC count2 = bit(mlbs2);
                          for (; count2 > 0; p2++, count2--) {
                            uintL** p1 = *p2;
                            if (p1) {
                              *p2 = p1 = (uintL**)((uintP)p1 + delta);
                              {
                                uintC count1 = bit(mlbs1);
                                for (; count1 > 0; p1++, count1--) {
                                  var uintL* p0 = *p1;
                                  if (p0) {
                                    *p1 = p0 = (uintL*)((uintP)p0 + delta);
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
        #if (mlb_levels >= 6)
          }
        }
        #endif
      }
    }
    bitmap->base = (mlbitmap_base_t)newbase;
    bitmap->alloc_size = newsize;
    return delta;
  }

  local bool mlb_add (mlbitmap* bitmap, object obj)
  {
    aint addr = (aint)ThePointer(obj);
    #if (mlb_levels >= 6)
    uintL******* p6 = &bitmap->base;
    if (*p6) {
      uintL****** p5 = &(*p6)[(addr >> mlb5) & (bit(mlbs5)-1)];
    #else
      uintL****** p5 = &bitmap->base;
    #endif
      if (*p5) {
        uintL***** p4 = &(*p5)[(addr >> mlb4) & (bit(mlbs4)-1)];
        if (*p4) {
          uintL**** p3 = &(*p4)[(addr >> mlb3) & (bit(mlbs3)-1)];
          if (*p3) {
            uintL*** p2 = &(*p3)[(addr >> mlb2) & (bit(mlbs2)-1)];
            if (*p2) {
              uintL** p1 = &(*p2)[(addr >> mlb1) & (bit(mlbs1)-1)];
              if (*p1) {
                uintL* p0 = &(*p1)[((addr >> mlb0)/(32*sizeof(gcv_object_t)))
                                   & ((bit(mlbs0)-1)/(32*sizeof(gcv_object_t)))];
                uintL i0 = ((addr >> mlb0)/sizeof(gcv_object_t)) % 32;
                if (*p0 & bit(i0))
                  return true;
                *p0 |= bit(i0);
                  return false;
              }
              {
                const uintL need = bit(mlbs0)/(32*sizeof(gcv_object_t))*sizeof(uintL);
                if (bitmap->used_size + need > bitmap->alloc_size) {
                  var uintP delta = mlb_expand(bitmap,bitmap->used_size+need);
                  p1 = (uintL**)((char*)p1 + delta);
                }
                var char* room = (char*)bitmap->base+bitmap->used_size;
                *p1 = (uintL*)room;
                var uintL* p0 = &(*p1)[((addr >> mlb0)/(32*sizeof(gcv_object_t)))
                                       & ((bit(mlbs0)-1)/(32*sizeof(gcv_object_t)))];
                var uintL i0 = ((addr >> mlb0)/sizeof(gcv_object_t)) % 32;
                *p0 = bit(i0);
                bitmap->used_size += need;
                return false;
              }
            }
            {
              const uintL need = bit(mlbs1)*sizeof(uintL*)
                + bit(mlbs0)/(32*sizeof(gcv_object_t))*sizeof(uintL);
              if (bitmap->used_size + need > bitmap->alloc_size) {
                var uintP delta = mlb_expand(bitmap,bitmap->used_size+need);
                p2 = (uintL***)((char*)p2 + delta);
              }
              var char* room = (char*)bitmap->base+bitmap->used_size;
              *p2 = (uintL**)room; room += bit(mlbs1)*sizeof(uintL*);
              var uintL** p1 = &(*p2)[(addr >> mlb1) & (bit(mlbs1)-1)];
              *p1 = (uintL*)room;
              var uintL* p0 = &(*p1)[((addr >> mlb0)/(32*sizeof(gcv_object_t)))
                                     & ((bit(mlbs0)-1)/(32*sizeof(gcv_object_t)))];
              var uintL i0 = ((addr >> mlb0)/sizeof(gcv_object_t)) % 32;
              *p0 = bit(i0);
              bitmap->used_size += need;
              return false;
            }
          }
          {
            const uintL need = bit(mlbs2)*sizeof(uintL**)
              + bit(mlbs1)*sizeof(uintL*)
              + bit(mlbs0)/(32*sizeof(gcv_object_t))*sizeof(uintL);
            if (bitmap->used_size + need > bitmap->alloc_size) {
              uintP delta = mlb_expand(bitmap,bitmap->used_size+need);
              p3 = (uintL****)((char*)p3 + delta);
            }
            var char* room = (char*)bitmap->base+bitmap->used_size;
            *p3 = (uintL***)room; room += bit(mlbs2)*sizeof(uintL**);
            var uintL*** p2 = &(*p3)[(addr >> mlb2) & (bit(mlbs2)-1)];
            *p2 = (uintL**)room; room += bit(mlbs1)*sizeof(uintL*);
            var uintL** p1 = &(*p2)[(addr >> mlb1) & (bit(mlbs1)-1)];
            *p1 = (uintL*)room;
            var uintL* p0 = &(*p1)[((addr >> mlb0)/(32*sizeof(gcv_object_t)))
                                    & ((bit(mlbs0)-1)/(32*sizeof(gcv_object_t)))];
            var uintL i0 = ((addr >> mlb0)/sizeof(gcv_object_t)) % 32;
            *p0 = bit(i0);
            bitmap->used_size += need;
            return false;
          }
        }
        {
          const uintL need = bit(mlbs3)*sizeof(uintL***)
            + bit(mlbs2)*sizeof(uintL**)
            + bit(mlbs1)*sizeof(uintL*)
            + bit(mlbs0)/(32*sizeof(gcv_object_t))*sizeof(uintL);
          if (bitmap->used_size + need > bitmap->alloc_size) {
            uintP delta = mlb_expand(bitmap,bitmap->used_size+need);
            p4 = (uintL*****)((char*)p4 + delta);
          }
          var char* room = (char*)bitmap->base+bitmap->used_size;
          *p4 = (uintL****)room; room += bit(mlbs3)*sizeof(uintL***);
          var uintL**** p3 = &(*p4)[(addr >> mlb3) & (bit(mlbs3)-1)];
          *p3 = (uintL***)room; room += bit(mlbs2)*sizeof(uintL**);
          var uintL*** p2 = &(*p3)[(addr >> mlb2) & (bit(mlbs2)-1)];
          *p2 = (uintL**)room; room += bit(mlbs1)*sizeof(uintL*);
          var uintL** p1 = &(*p2)[(addr >> mlb1) & (bit(mlbs1)-1)];
          *p1 = (uintL*)room;
          var uintL* p0 = &(*p1)[((addr >> mlb0)/(32*sizeof(gcv_object_t)))
                                 & ((bit(mlbs0)-1)/(32*sizeof(gcv_object_t)))];
          uintL i0 = ((addr >> mlb0)/sizeof(gcv_object_t)) % 32;
          *p0 = bit(i0);
          bitmap->used_size += need;
          return false;
        }
      }
      {
        const uintL need = bit(mlbs4)*sizeof(uintL****)
          + bit(mlbs3)*sizeof(uintL***)
          + bit(mlbs2)*sizeof(uintL**)
          + bit(mlbs1)*sizeof(uintL*)
          + bit(mlbs0)/(32*sizeof(gcv_object_t))*sizeof(uintL);
        if (bitmap->used_size + need > bitmap->alloc_size) {
          uintP delta = mlb_expand(bitmap,bitmap->used_size+need);
          #if (mlb_levels > 5)
          p5 = (uintL******)((char*)p5 + delta);
          #endif
        }
        var char* room = (char*)bitmap->base+bitmap->used_size;
        *p5 = (uintL*****)room; room += bit(mlbs4)*sizeof(uintL****);
        var uintL***** p4 = &(*p5)[(addr >> mlb4) & (bit(mlbs4)-1)];
        *p4 = (uintL****)room; room += bit(mlbs3)*sizeof(uintL***);
        var uintL**** p3 = &(*p4)[(addr >> mlb3) & (bit(mlbs3)-1)];
        *p3 = (uintL***)room; room += bit(mlbs2)*sizeof(uintL**);
        var uintL*** p2 = &(*p3)[(addr >> mlb2) & (bit(mlbs2)-1)];
        *p2 = (uintL**)room; room += bit(mlbs1)*sizeof(uintL*);
        var uintL** p1 = &(*p2)[(addr >> mlb1) & (bit(mlbs1)-1)];
        *p1 = (uintL*)room;
        var uintL* p0 = &(*p1)[((addr >> mlb0)/(32*sizeof(gcv_object_t)))
                                & ((bit(mlbs0)-1)/(32*sizeof(gcv_object_t)))];
        uintL i0 = ((addr >> mlb0)/sizeof(gcv_object_t)) % 32;
        *p0 = bit(i0);
        bitmap->used_size += need;
        return false;
      }
    #if (mlb_levels >= 6)
    }
    {
      const uintL need = bit(mlbs5)*sizeof(uintL*****)
        + bit(mlbs4)*sizeof(uintL****)
        + bit(mlbs3)*sizeof(uintL***)
        + bit(mlbs2)*sizeof(uintL**)
        + bit(mlbs1)*sizeof(uintL*)
        + bit(mlbs0)/(32*sizeof(gcv_object_t))*sizeof(uintL);
      if (bitmap->used_size + need > bitmap->alloc_size) {
        uintP delta = mlb_expand(bitmap,bitmap->used_size+need);
        #if (mlb_levels > 6)
        p6 = (uintL*******)((char*)p6 + delta);
        #endif
      }
      var char* room = (char*)bitmap->base+bitmap->used_size;
      *p6 = (uintL******)room; room += bit(mlbs5)*sizeof(uintL*****);
      var uintL****** p5 = &(*p6)[(addr >> mlb5) & (bit(mlbs5)-1)];
      *p5 = (uintL*****)room; room += bit(mlbs4)*sizeof(uintL****);
      var uintL***** p4 = &(*p5)[(addr >> mlb4) & (bit(mlbs4)-1)];
      *p4 = (uintL****)room; room += bit(mlbs3)*sizeof(uintL***);
      var uintL**** p3 = &(*p4)[(addr >> mlb3) & (bit(mlbs3)-1)];
      *p3 = (uintL***)room; room += bit(mlbs2)*sizeof(uintL**);
      var uintL*** p2 = &(*p3)[(addr >> mlb2) & (bit(mlbs2)-1)];
      *p2 = (uintL**)room; room += bit(mlbs1)*sizeof(uintL*);
      var uintL** p1 = &(*p2)[(addr >> mlb1) & (bit(mlbs1)-1)];
      *p1 = (uintL*)room;
      var uintL* p0 = &(*p1)[((addr >> mlb0)/(32*sizeof(gcv_object_t)))
                           & ((bit(mlbs0)-1)/(32*sizeof(gcv_object_t)))];
      var uintL i0 = ((addr >> mlb0)/sizeof(gcv_object_t)) % 32;
      *p0 = bit(i0);
      bitmap->used_size += need;
      return false;
    }
    #endif
  }

  local void mlb_free (mlbitmap* bitmap)
  {
    if (bitmap->base) {
      begin_system_call();
      free((char*)bitmap->base);
      end_system_call();
    }
  }

#endif


# Implementation of get_circularities.

#ifdef MULTITHREAD

# get_circularities(obj,pr_array,pr_closure)
# Method:
# Traverse the object recursively, noting in a hash set (a multi-level bit map)
# the sub-objects traversed. While doing this, push the circularities onto the
# STACK. Then release the bitmap.
# Allocate a vector for the circularities (this can trigger GC!), move the
# circularities from the STACK into the vector.

# Global variables during get_circularities.
  typedef struct {
    mlbitmap bitmap;
    bool pr_array;
    bool pr_closure;
    uintL counter;
    jmp_buf abbruch_context;
    gcv_object_t* abbruch_STACK;
  } get_circ_global;

# UP: marks the object obj, pushes occurring circularities on the STACK
# and counts them in env->counter.
  local void get_circ_mark (object obj, get_circ_global* env)
  {
   entry:
    #ifdef TYPECODES
    switch (typecode(obj)) # according to type
    #else
    if (orecordp(obj)) {
      goto case_orecord;
    } elif (consp(obj)) {
      goto case_cons;
    } else {
      goto m_end;
    }
    switch (0)
    #endif
    {
      case_cons:
        if (mlb_add(&env->bitmap,obj)) # marked?
          goto m_schon_da;
        {
          var object obj_cdr = Cdr(obj); # components
          var object obj_car = Car(obj);
          if (SP_overflow()) # check SP-depth
            longjmp(env->abbruch_context,true); # abort
          get_circ_mark(obj_car,env); # mark CAR (recursive)
          obj = obj_cdr; goto entry; # mark CDR (tail-end-recursive)
        }
      case_symbol:
        if (mlb_add(&env->bitmap,obj)) # marked?
          if (nullp(Symbol_package(obj))) /* uninterned symbol? */
            goto m_schon_da; # yes -> was already there, memorize
          else
            goto m_end; # no -> was already there, but leave unconsidered
        goto m_end;
      case_bvector: # Bit-Vector
      case_b2vector: # 2Bit-Vector
      case_b4vector: # 4Bit-Vector
      case_b8vector: # 8Bit-Vector
      case_b16vector: # 16Bit-Vector
      case_b32vector: # 32Bit-Vector
      case_string: # String
      case_bignum: # Bignum
      #ifndef IMMEDIATE_FFLOAT
      case_ffloat: # Single-Float
      #endif
      case_dfloat: # Double-Float
      case_lfloat: # Long-Float
      case_ratio: # Ratio
      case_complex: # Complex
        # Object without components that are printed:
        if (mlb_add(&env->bitmap,obj)) # marked?
          goto m_schon_da;
        goto m_end;
      case_svector: # Simple-Vector
        if (mlb_add(&env->bitmap,obj)) # marked?
          goto m_schon_da;
        # so far unmarked
        if (env->pr_array) { # track components?
          var uintL count = Svector_length(obj);
          if (!(count==0)) {
            # mark count>0 components
            var gcv_object_t* ptr = &TheSvector(obj)->data[0];
            if (SP_overflow()) # check SP-depth
              longjmp(env->abbruch_context,true); # abort
            dotimespL(count,count, { get_circ_mark(*ptr++,env); } ); # mark components (recursive)
          }
        }
        goto m_end;
      case_mdarray: case_ovector:
        # non-simple Array with components, that are objects:
        if (mlb_add(&env->bitmap,obj)) # marked?
          goto m_schon_da;
        # so far unmarked
        if (env->pr_array) { # track components?
          obj = TheIarray(obj)->data; goto entry; # mark data-vector (tail-end-recursive)
        } else
          goto m_end;
      case_closure: # Closure
        if (mlb_add(&env->bitmap,obj)) # marked?
          goto m_schon_da;
        # so far unmarked
        if (env->pr_closure) # track components?
          goto m_record_components; # all components are printed (see below)
        else { # only mark the name (tail-end-recursive)
          obj = Closure_name(obj); goto entry;
        }
      case_structure: # Structure
        if (mlb_add(&env->bitmap,obj)) # marked?
          goto m_schon_da;
        # so far unmarked
        goto m_record_components;
      case_stream: # Stream
        if (mlb_add(&env->bitmap,obj)) # marked?
          goto m_schon_da;
        # so far unmarked
        switch (TheStream(obj)->strmtype) {
          case strmtype_broad:
          case strmtype_concat:
            goto m_record_components;
          default:
            goto m_end;
        }
      case_instance: # CLOS-instance
        if (mlb_add(&env->bitmap,obj)) # marked?
          goto m_schon_da;
        # so far unmarked
        goto m_record_components;
      case_orecord: case_lrecord: # mark other record:
        switch (Record_type(obj)) {
          #ifndef TYPECODES
          case_Rectype_Symbol_above;
          case_Rectype_bvector_above;
          case_Rectype_b2vector_above;
          case_Rectype_b4vector_above;
          case_Rectype_b8vector_above;
          case_Rectype_b16vector_above;
          case_Rectype_b32vector_above;
          case_Rectype_string_above;
          case_Rectype_Bignum_above;
          case_Rectype_Ffloat_above;
          case_Rectype_Dfloat_above;
          case_Rectype_Lfloat_above;
          case_Rectype_Ratio_above;
          case_Rectype_Complex_above;
          case_Rectype_Svector_above;
          case_Rectype_mdarray_above;
          case_Rectype_ovector_above;
          #endif
          case_Rectype_Closure_above;
          case_Rectype_Structure_above;
          case_Rectype_Stream_above;
          case_Rectype_Instance_above;
          default: ;
        }
        if (mlb_add(&env->bitmap,obj)) # marked?
          goto m_schon_da;
        # so far unmarked
        switch (Record_type(obj)) {
          case Rectype_Hashtable:
            # Hash-Table: according to Array-Output-Flag
            if (env->pr_array)
              break;
            else
              goto m_end;
          case Rectype_Package:
            # Packages are not printed component-wise
            goto m_end;
          case Rectype_Readtable:
            # Readtables are not printed component-wise
            goto m_end;
          case Rectype_Weakpointer: /* only the value is printed! */
            get_circ_mark(TheWeakpointer(obj)->wp_value,env);
            goto m_end;
          case Rectype_MutableWeakList:
            get_circ_mark(TheMutableWeakList(obj)->mwl_list,env);
            goto m_end;
          case Rectype_MutableWeakAlist:
            get_circ_mark(TheMutableWeakAlist(obj)->mwal_list,env);
            goto m_end;
          case Rectype_Weakmapping:
            get_circ_mark(TheWeakmapping(obj)->wm_value,env);
            get_circ_mark(TheWeakmapping(obj)->wm_key,env);
            goto m_end;
          case Rectype_WeakList:
            {
              var uintL count = Lrecord_length(obj)-2;
              if (count > 0) {
                var gcv_object_t* ptr = &TheWeakList(obj)->wl_elements[0];
                if (SP_overflow()) # check SP-depth
                  longjmp(env->abbruch_context,true); # abort
                dotimespL(count,count, { get_circ_mark(*ptr++,env); } ); # mark elements (recursive)
              }
            }
            goto m_end;
          case Rectype_WeakAnd:
            get_circ_mark(TheWeakAnd(obj)->war_keys_list,env);
            goto m_end;
          case Rectype_WeakOr:
            get_circ_mark(TheWeakOr(obj)->wor_keys_list,env);
            goto m_end;
          case Rectype_WeakAndMapping:
            get_circ_mark(TheWeakAndMapping(obj)->wam_value,env);
            get_circ_mark(TheWeakAndMapping(obj)->wam_keys_list,env);
            goto m_end;
          case Rectype_WeakOrMapping:
            get_circ_mark(TheWeakOrMapping(obj)->wom_value,env);
            get_circ_mark(TheWeakOrMapping(obj)->wom_keys_list,env);
            goto m_end;
          case Rectype_WeakAlist_Key:
          case Rectype_WeakAlist_Value:
          case Rectype_WeakAlist_Either:
          case Rectype_WeakAlist_Both:
          case Rectype_WeakHashedAlist_Key:
          case Rectype_WeakHashedAlist_Value:
          case Rectype_WeakHashedAlist_Either:
          case Rectype_WeakHashedAlist_Both:
            {
              var uintL count = Lrecord_length(obj)-1; # don't mark wp_cdr
              # mark count>0 components, starting from recdata[1]
              var gcv_object_t* ptr = &TheLrecord(obj)->recdata[1];
              if (SP_overflow()) # check SP-depth
                longjmp(env->abbruch_context,true); # abort
              dotimespL(count,count, { get_circ_mark(*ptr++,env); } ); # mark components (recursive)
            }
            goto m_end;
          default: break;
        }
        # Pathnames, Random-States, Bytes, Fsubrs, Loadtimeevals,
        # Symbol-Macros and poss. Hash-Tables are poss.
        # printed component-wise.
       m_record_components: # mark components of a Record:
        {
          var uintC count = Record_length(obj);
          if (!(count==0)) {
            # mark count>0 components
            var gcv_object_t* ptr = &TheRecord(obj)->recdata[0];
            if (SP_overflow()) # check SP-depth
              longjmp(env->abbruch_context,true); # abort
            dotimespC(count,count, { get_circ_mark(*ptr++,env); } ); # mark components (recursive)
          }
        }
        goto m_end;
       m_schon_da:
        # object was marked, but has already been marked.
        # It is a circularity.
        if (STACK_overflow()) # check STACK-depth
          longjmp(env->abbruch_context,true); # abort
        # store object in STACK:
        pushSTACK(obj);
        env->counter++; # and count
        goto m_end;
      #ifdef TYPECODES
      case_machine: # Machine Pointer
      case_char: # Character
      case_subr: # Subr
      case_system: # Frame-pointer, Small-Read-label, system
      case_fixnum: # Fixnum
      case_sfloat: # Short-Float
      #ifdef IMMEDIATE_FFLOAT
      case_ffloat: # Single-Float
      #endif
      #endif
      default:
        # object cannot be marked -> finished
        goto m_end;
       m_end: ; # finished
    }
  }

  global maygc object get_circularities (object obj, bool pr_array, bool pr_closure)
  {
    var get_circ_global my_global; # counter and context (incl. STACK-value)
                                   # for the case of an abort
    set_break_sem_1(); # make Break impossible
    if (!setjmp(my_global.abbruch_context)) { # save context
      bcopy(my_global.abbruch_context,my_global.bitmap.oom_context,sizeof(jmp_buf));
      mlb_alloc(&my_global.bitmap); # allocate bitmap
      my_global.pr_array = pr_array;
      my_global.pr_closure = pr_closure;
      my_global.counter = 0; # counter := 0
      my_global.abbruch_STACK = STACK;
      # the context-backup my_global is now ready.
      get_circ_mark(obj,&my_global); # mark object, push multiple
                                     # structures on the STACK
                                     # count in my_global.counter
      mlb_free(&my_global.bitmap); # free Bitmap
      clr_break_sem_1(); # allow Break again
      var uintL n = my_global.counter; # number of objects on the STACK
      if (n==0) {
        return NIL; # none there -> return NIL and finished
      } else {
        var object vector = allocate_vector(n+1); # vector with n+1 elements
        # fill:
        var gcv_object_t* ptr = &TheSvector(vector)->data[0];
        *ptr++ = Fixnum_0; # first element = Fixnum 0
        # store remaining elements (at least one):
        dotimespL(n,n, { *ptr++ = popSTACK(); } );
        return vector; # vector as result
      }
    } else {
      # after abort because of SP- or STACK-overflow
      setSTACK(STACK = my_global.abbruch_STACK); # reset STACK again
      # the context is now reestablished.
      mlb_free(&my_global.bitmap); # free Bitmap
      clr_break_sem_1(); # Break again possible
      return T; # T as result
    }
  }

#else # !MULTITHREAD

# get_circularities(obj,pr_array,pr_closure)
# Method:
# Mark the object recursively, push the circularities on the STACK,
# unmark the object recursively,
# allocate a vector for the circularities (can trigger GC!),
# fill the circularities from the STACK into the vector.
  typedef struct {
    bool pr_array;
    bool pr_closure;
    uintL counter;
    jmp_buf abbruch_context;
    gcv_object_t* abbruch_STACK;
  } get_circ_global;
  # It has to be accessed from within the two local routines.
  local void get_circ_mark (object obj, get_circ_global* env);
  local void get_circ_unmark (object obj, get_circ_global* env);
  global maygc object get_circularities (object obj, bool pr_array, bool pr_closure)
  {
    var get_circ_global my_global; # counter and context (incl. STACK-value)
                                   # in case of an abort
    set_break_sem_1(); # make Break impossible
    if (!setjmp(my_global.abbruch_context)) { # save context
      my_global.pr_array = pr_array;
      my_global.pr_closure = pr_closure;
      my_global.counter = 0; # counter := 0
      my_global.abbruch_STACK = STACK;
      # the context-backup my_global is now ready.
      get_circ_mark(obj,&my_global); # mark object, push multiple
                                     # structures on the STACK
                                     # count in my_global.counter
      get_circ_unmark(obj,&my_global); # delete marks again
      clr_break_sem_1(); # make Break possible again
      var uintL n = my_global.counter; # number of objects on the STACK
      if (n==0) {
        return NIL; # none there -> return NIL and finished
      } else {
        var object vector = allocate_vector(n+1); # vector with n+1 elements
        # fill:
        var gcv_object_t* ptr = &TheSvector(vector)->data[0];
        *ptr++ = Fixnum_0; # first element = Fixnum 0
        # store remaining elements (at least one):
        dotimespL(n,n, { *ptr++ = popSTACK(); } );
        return vector; # vector as result
      }
    } else {
      # after abort because of SP- or STACK-overflow
      setSTACK(STACK = my_global.abbruch_STACK); # reset STACK again
      # the context is now reestablished.
      get_circ_unmark(obj,&my_global); # delete marks again
      clr_break_sem_1(); # Break is possible again
      return T; # T as result
    }
  }
# UP: marks the object obj, pushes occurring circularities on the STACK
# and counts them in env->counter.
  local void get_circ_mark (object obj, get_circ_global* env)
  {
   entry:
    #ifdef TYPECODES
    switch (typecode(obj)) # according to type
    #else
    if (orecordp(obj)) {
      goto case_orecord;
    } elif (consp(obj)) {
      goto case_cons;
    } else {
      goto m_end;
    }
    switch (0)
    #endif
    {
      case_cons:
        if (marked(TheCons(obj))) # marked?
          goto m_schon_da;
        {
          var object obj_cdr = Cdr(obj); # components (without mark bit)
          var object obj_car = Car(obj);
          mark(TheCons(obj)); # mark
          if (SP_overflow()) # check SP-depth
            longjmp(env->abbruch_context,true); # abort
          get_circ_mark(obj_car,env); # mark CAR (recursive)
          obj = obj_cdr; goto entry; # mark CDR (tail-end-recursive)
        }
      case_symbol:
        if (marked(TheSymbol(obj))) { # marked?
          if (nullp(Symbol_package(obj))) /* uninterned symbol? */
            goto m_schon_da; # yes -> was already there, memorize
          else
            goto m_end; # no -> was already there, but leave unconsidered
        }
        # so far unmarked symbol
        mark(TheSymbol(obj)); # mark
        goto m_end;
      case_bvector: # Bit-Vector
      case_b2vector: # 2Bit-Vector
      case_b4vector: # 4Bit-Vector
      case_b8vector: # 8Bit-Vector
      case_b16vector: # 16Bit-Vector
      case_b32vector: # 32Bit-Vector
      case_string: # String
      case_bignum: # Bignum
      #ifndef IMMEDIATE_FFLOAT
      case_ffloat: # Single-Float
      #endif
      case_dfloat: # Double-Float
      case_lfloat: # Long-Float
      case_ratio: # Ratio
      case_complex: # Complex
        # object without components that are printed:
        if (marked(ThePointer(obj))) # marked?
          goto m_schon_da;
        # so far unmarked
        mark(ThePointer(obj)); # marked
        goto m_end;
      case_svector: # Simple-Vector
        if (marked(TheSvector(obj))) # marked?
          goto m_schon_da;
        # so far unmarked
        mark(TheSvector(obj)); # mark
       m_svector:
        if (env->pr_array) { # track components?
          var uintL count = Svector_length(obj);
          if (!(count==0)) {
            # mark count>0 components
            var gcv_object_t* ptr = &TheSvector(obj)->data[0];
            if (SP_overflow()) # check SP-depth
              longjmp(env->abbruch_context,true); # abort
            dotimespL(count,count, { get_circ_mark(*ptr++,env); } ); # mark components (recursive)
          }
        }
        goto m_end;
      case_mdarray: case_ovector:
        # non-simple array with components that are objects:
        if (marked(TheIarray(obj))) # marked?
          goto m_schon_da;
        # so far unmarked
        mark(TheIarray(obj)); # mark
       m_array:
        if (env->pr_array) { # track components?
          obj = TheIarray(obj)->data; goto entry; # mark data vector (tail-end-recursive)
        } else
          goto m_end;
      case_closure: # Closure
        if (marked(TheClosure(obj))) # marked?
          goto m_schon_da;
        # so far unmarked
        mark(TheClosure(obj)); # mark
        if (env->pr_closure) # track components?
          goto m_record_components; # all components are printed (see below)
        else { # only mark the name (tail-end-recursive)
          obj = Closure_name(obj); goto entry;
        }
      case_structure: # Structure
        if (marked(TheStructure(obj))) # marked?
          goto m_schon_da;
        # so far unmarked
        mark(TheStructure(obj)); # mark
        goto m_record_components;
      case_stream: # Stream
        if (marked(TheStream(obj))) # marked?
          goto m_schon_da;
        # so far unmarked
        mark(TheStream(obj));
        switch (TheStream(obj)->strmtype) {
          case strmtype_broad:
          case strmtype_concat:
            goto m_record_components;
          default:
            goto m_end;
        }
      case_instance: # CLOS-instance
        if (marked(TheInstance(obj))) # marked?
          goto m_schon_da;
        # so far unmarked
        mark(TheInstance(obj)); # mark
        goto m_record_components;
      case_orecord: case_lrecord: # mark other record:
        switch (Record_type(obj)) {
          #ifndef TYPECODES
          case_Rectype_Symbol_above;
          case_Rectype_bvector_above;
          case_Rectype_b2vector_above;
          case_Rectype_b4vector_above;
          case_Rectype_b8vector_above;
          case_Rectype_b16vector_above;
          case_Rectype_b32vector_above;
          case_Rectype_string_above;
          case_Rectype_Bignum_above;
          case_Rectype_Ffloat_above;
          case_Rectype_Dfloat_above;
          case_Rectype_Lfloat_above;
          case_Rectype_Ratio_above;
          case_Rectype_Complex_above;
          case_Rectype_Svector_above;
          case_Rectype_mdarray_above;
          case_Rectype_ovector_above;
          #endif
          case_Rectype_Closure_above;
          case_Rectype_Structure_above;
          case_Rectype_Stream_above;
          case_Rectype_Instance_above;
          default: ;
        }
        if (marked(TheRecord(obj))) # marked?
          goto m_schon_da;
        # so far unmarked
        mark(TheRecord(obj)); # mark
        switch (Record_type(obj)) {
          case Rectype_Hashtable:
            # Hash-Table: according to the Array-Print-Flag
            if (env->pr_array)
              break;
            else
              goto m_end;
          case Rectype_Package:
            # Packages are not printed component-wise
            goto m_end;
          case Rectype_Readtable:
            # Readtables are not printed component-wise
            goto m_end;
          case Rectype_Weakpointer: /* only the value is printed! */
            get_circ_mark(TheWeakpointer(obj)->wp_value,env);
            goto m_end;
          case Rectype_MutableWeakList:
            get_circ_mark(TheMutableWeakList(obj)->mwl_list,env);
            goto m_end;
          case Rectype_MutableWeakAlist:
            get_circ_mark(TheMutableWeakAlist(obj)->mwal_list,env);
            goto m_end;
          case Rectype_Weakmapping:
            get_circ_mark(TheWeakmapping(obj)->wm_value,env);
            get_circ_mark(TheWeakmapping(obj)->wm_key,env);
            goto m_end;
          case Rectype_WeakList:
            {
              var uintL count = Lrecord_length(obj)-2;
              if (count > 0) {
                var gcv_object_t* ptr = &TheWeakList(obj)->wl_elements[0];
                if (SP_overflow()) # check SP-depth
                  longjmp(env->abbruch_context,true); # abort
                dotimespL(count,count, { get_circ_mark(*ptr++,env); } ); # mark elements (recursive)
              }
            }
            goto m_end;
          case Rectype_WeakAnd:
            get_circ_mark(TheWeakAnd(obj)->war_keys_list,env);
            goto m_end;
          case Rectype_WeakOr:
            get_circ_mark(TheWeakOr(obj)->wor_keys_list,env);
            goto m_end;
          case Rectype_WeakAndMapping:
            get_circ_mark(TheWeakAndMapping(obj)->wam_value,env);
            get_circ_mark(TheWeakAndMapping(obj)->wam_keys_list,env);
            goto m_end;
          case Rectype_WeakOrMapping:
            get_circ_mark(TheWeakOrMapping(obj)->wom_value,env);
            get_circ_mark(TheWeakOrMapping(obj)->wom_keys_list,env);
            goto m_end;
          case Rectype_WeakAlist_Key:
          case Rectype_WeakAlist_Value:
          case Rectype_WeakAlist_Either:
          case Rectype_WeakAlist_Both:
          case Rectype_WeakHashedAlist_Key:
          case Rectype_WeakHashedAlist_Value:
          case Rectype_WeakHashedAlist_Either:
          case Rectype_WeakHashedAlist_Both:
            {
              var uintL count = Lrecord_length(obj)-1; # don't mark wp_cdr
              # mark count>0 components, starting from recdata[1]
              var gcv_object_t* ptr = &TheLrecord(obj)->recdata[1];
              if (SP_overflow()) # check SP-depth
                longjmp(env->abbruch_context,true); # abort
              dotimespL(count,count, { get_circ_mark(*ptr++,env); } ); # mark components (recursive)
            }
            goto m_end;
          default: break;
        }
        # Pathnames, Random-States, Bytes, Fsubrs, Loadtimeevals,
        # Symbol-Macros and poss. Hash-Tables are poss.
        # printed component-wise.
       m_record_components: # mark the components of a Record:
        {
          var uintC count = Record_length(obj);
          if (!(count==0)) {
            # mark count>0 components
            var gcv_object_t* ptr = &TheRecord(obj)->recdata[0];
            if (SP_overflow()) # check SP-depth
              longjmp(env->abbruch_context,true); # abort
            dotimespC(count,count, { get_circ_mark(*ptr++,env); } ); # mark components (recursive)
          }
        }
        goto m_end;
       m_schon_da:
        # Object has been marked, but was already marked.
        # It is a circularity.
        if (STACK_overflow()) # check STACK-depth
          longjmp(env->abbruch_context,true); # abort
        # store object with deleted garcol_bit in STACK:
        pushSTACK(without_mark_bit(obj));
        env->counter++; # and count
        goto m_end;
      #ifdef TYPECODES
      case_machine: # Machine Pointer
      case_char: # Character
      case_subr: # Subr
      case_system: # Frame-pointer, Small-Read-label, system
      case_fixnum: # Fixnum
      case_sfloat: # Short-Float
      #ifdef IMMEDIATE_FFLOAT
      case_ffloat: # Single-Float
      #endif
      #endif
      default:
        # Object cannot be marked -> finished
        goto m_end;
     m_end: ; # finished
    }
  }
# UP: Unmarks object obj.
  local void get_circ_unmark (object obj, get_circ_global* env)
  {
   entry:
    #ifdef TYPECODES
    switch (typecode(obj) & ~bit(garcol_bit_t)) # according to typeinfo without garcol_bit
    #else
    if (orecordp(obj)) {
      goto case_orecord;
    } elif (consp(obj)) {
      goto case_cons;
    } else {
      goto u_end;
    }
    switch (0)
    #endif
    {
      case_cons:
        if (!marked(TheCons(obj))) # already unmarked?
          goto u_end;
        unmark(TheCons(obj)); # unmark
        get_circ_unmark(Car(obj),env); # unmark CAR (recursive)
        obj = Cdr(obj); goto entry; # unmark CDR (tail-end-recursive)
      case_bvector: # Bit-Vector
      case_b2vector: # 2Bit-Vector
      case_b4vector: # 4Bit-Vector
      case_b8vector: # 8Bit-Vector
      case_b16vector: # 16Bit-Vector
      case_b32vector: # 32Bit-Vector
      case_string: # String
      case_symbol:
        # unmark symbol. value cell etc. irrelevant for PRINT.
      case_bignum: # Bignum
      #ifndef IMMEDIATE_FFLOAT
      case_ffloat: # Single-Float
      #endif
      case_dfloat: # Double-Float
      case_lfloat: # Long-Float
      case_ratio: # Ratio
      case_complex: # Complex
        # unmark object, that has no marked components:
        unmark(ThePointer(obj)); # unmark
        goto u_end;
      case_svector:
        # unmark Simple-Vector, including its components:
        if (!marked(TheSvector(obj))) # already unmarked?
          goto u_end;
        unmark(TheSvector(obj)); # unmark
       u_svector:
        if (env->pr_array) { # were the components tracked?
          var uintL count = Svector_length(obj);
          if (!(count==0)) {
            # unmark count>0 components
            var gcv_object_t* ptr = &TheSvector(obj)->data[0];
            dotimespL(count,count, { get_circ_unmark(*ptr++,env); } ); # unmark components (recursive)
          }
        }
        goto u_end;
      case_mdarray: case_ovector:
        # non-simple Array with components that are objects:
        if (!marked(TheIarray(obj))) # already unmarked?
          goto u_end;
        unmark(TheIarray(obj)); # unmark
       u_array:
        if (env->pr_array) { # were the components tracked?
          obj = TheIarray(obj)->data; goto entry; # unmark data vector (tail-end-recursive)
        } else
          goto u_end;
      case_closure: # unmark Closure
        if (!marked(TheClosure(obj))) # already unmarked?
          goto u_end;
        unmark(TheClosure(obj)); # unmark
        if (env->pr_closure) # were components tracked?
          goto u_record_components; # all components are printed (see below)
        else { # only unmark the name (tail-end-recursive)
          obj = Closure_name(obj); goto entry;
        }
      case_structure: # unmark structure:
        if (!marked(TheStructure(obj))) # already unmarked?
          goto u_end;
        unmark(TheStructure(obj)); # unmark
        goto u_record_components;
      case_stream: # unmark stream:
        if (!marked(TheStream(obj))) # already unmarked?
          goto u_end;
        unmark(TheStream(obj)); # unmark
        switch (TheStream(obj)->strmtype) {
          case strmtype_broad:
          case strmtype_concat:
            goto u_record_components;
          default:
            goto u_end;
        }
      case_instance: # unmark CLOS-instance:
        if (!marked(TheInstance(obj))) # already unmarked?
          goto u_end;
        unmark(TheInstance(obj)); # unmark
        goto u_record_components;
      case_orecord: case_lrecord: # unmark other record:
        switch (Record_type(obj)) {
          #ifndef TYPECODES
          case_Rectype_bvector_above;
          case_Rectype_b2vector_above;
          case_Rectype_b4vector_above;
          case_Rectype_b8vector_above;
          case_Rectype_b16vector_above;
          case_Rectype_b32vector_above;
          case_Rectype_string_above;
          case_Rectype_Symbol_above;
          case_Rectype_Bignum_above;
          case_Rectype_Ffloat_above;
          case_Rectype_Dfloat_above;
          case_Rectype_Lfloat_above;
          case_Rectype_Ratio_above;
          case_Rectype_Complex_above;
          case_Rectype_Svector_above;
          case_Rectype_mdarray_above;
          case_Rectype_ovector_above;
          #endif
          case_Rectype_Closure_above;
          case_Rectype_Structure_above;
          case_Rectype_Stream_above;
          case_Rectype_Instance_above;
          default: ;
        }
        if (!marked(TheRecord(obj))) # already unmarked?
          goto u_end;
        unmark(TheRecord(obj)); # unmark
        switch (Record_type(obj)) {
          case Rectype_Hashtable:
            # Hash-Table: according to Array-Print-Flag
            if (env->pr_array)
              break;
            else
              goto u_end;
          case Rectype_Package:
            # Packages are not printed component-wise
            goto u_end;
          case Rectype_Readtable:
            # Readtables are not printed component-wise
            goto u_end;
          case Rectype_Weakpointer: /* only the value is printed! */
            get_circ_unmark(TheWeakpointer(obj)->wp_value,env);
            goto u_end;
          case Rectype_MutableWeakList:
            get_circ_unmark(TheMutableWeakList(obj)->mwl_list,env);
            goto u_end;
          case Rectype_MutableWeakAlist:
            get_circ_unmark(TheMutableWeakAlist(obj)->mwal_list,env);
            goto u_end;
          case Rectype_Weakmapping:
            get_circ_unmark(TheWeakmapping(obj)->wm_value,env);
            get_circ_unmark(TheWeakmapping(obj)->wm_key,env);
            goto u_end;
          case Rectype_WeakList:
            {
              var uintL count = Lrecord_length(obj)-2;
              if (count > 0) {
                var gcv_object_t* ptr = &TheWeakList(obj)->wl_elements[0];
                dotimespL(count,count, { get_circ_unmark(*ptr++,env); } ); # mark elements (recursive)
              }
            }
            goto u_end;
          case Rectype_WeakAnd:
            get_circ_unmark(TheWeakAnd(obj)->war_keys_list,env);
            goto u_end;
          case Rectype_WeakOr:
            get_circ_unmark(TheWeakOr(obj)->wor_keys_list,env);
            goto u_end;
          case Rectype_WeakAndMapping:
            get_circ_unmark(TheWeakAndMapping(obj)->wam_value,env);
            get_circ_unmark(TheWeakAndMapping(obj)->wam_keys_list,env);
            goto u_end;
          case Rectype_WeakOrMapping:
            get_circ_unmark(TheWeakOrMapping(obj)->wom_value,env);
            get_circ_unmark(TheWeakOrMapping(obj)->wom_keys_list,env);
            goto u_end;
          case Rectype_WeakAlist_Key:
          case Rectype_WeakAlist_Value:
          case Rectype_WeakAlist_Either:
          case Rectype_WeakAlist_Both:
          case Rectype_WeakHashedAlist_Key:
          case Rectype_WeakHashedAlist_Value:
          case Rectype_WeakHashedAlist_Either:
          case Rectype_WeakHashedAlist_Both:
            {
              var uintL count = Lrecord_length(obj)-1;
              var gcv_object_t* ptr = &TheLrecord(obj)->recdata[1];
              dotimespL(count,count, { get_circ_unmark(*ptr++,env); } ); # unmark components (recursive)
            }
            goto u_end;
          default: break;
        }
        # Pathnames, Random-States, Bytes, Fsubrs, Loadtimeevals,
        # Symbol-Macros and poss. Hash-Tables are poss.
        # printed component-wise.
       u_record_components: # unmark components of a record:
        {
          var uintC count = Record_length(obj);
          if (!(count==0)) {
            # unmark count>0 components
            var gcv_object_t* ptr = &TheRecord(obj)->recdata[0];
            dotimespC(count,count, { get_circ_unmark(*ptr++,env); } ); # unmark components (recursive)
          }
        }
        goto u_end;
      #ifdef TYPECODES
      case_machine: # Machine Pointer
      case_char: # Character
      case_subr: # Subr
      case_system: # Frame-pointer, Small-Read-label, system
      case_fixnum: # Fixnum
      case_sfloat: # Short-Float
      #ifdef IMMEDIATE_FFLOAT
      case_ffloat: # Single-Float
      #endif
      #endif
      default:
        # unmark object that cannot have a mark at all:
        goto u_end;
     u_end: ; # finished
    }
  }

#endif


# Implementation of subst_circ.

#ifdef MULTITHREAD

# Global variables during subst_circ.
  typedef struct {
    mlbitmap bitmap;
    object alist;
    jmp_buf abbruch_context;
    object bad;
  } subst_circ_global;

  local void subst_circ_mark (gcv_object_t* ptr, subst_circ_global* env)
  {
    #if !(defined(NO_SP_CHECK) || defined(NOCOST_SP_CHECK))
    if (SP_overflow()) { # check SP-depth
      env->bad = nullobj; longjmp(env->abbruch_context,true); # abort
    }
    #endif
   enter_subst:
    {
      var object obj = *ptr;
      # fall differentiation by type:
      # Objects without sub-objects (machine pointers, bit-vectors,
      # strings, characters, subrs, integers, floats) contain no
      # references. The same holds true for Symbols and rational numbers (their
      # sub-objects could not have been entered in #n= - Syntax)
      # and complex numbers (for their components only
      # integers, floats, rational numbers are allowed, which means objects,
      # that cannot contain references themselves).
      #ifdef TYPECODES
      switch (typecode(obj))
      #else
      if (orecordp(obj)) {
        goto case_orecord;
      } elif (consp(obj)) {
        goto case_cons;
      } elif (immediate_number_p(obj)) {
        goto case_number;
      } elif (charp(obj)) {
        goto case_char;
      } elif (subrp(obj)) {
        goto case_subr;
      } elif (machinep(obj)) {
        goto case_machine;
      } elif (small_read_label_p(obj)) {
        goto case_small_read_label;
      } elif (systemp(obj)) {
        return;
      } else switch (0)
      #endif
      {
        case_svector: # Simple-Vector
          if (mlb_add(&env->bitmap,obj)) # Object already marked?
            return;
          # traverse all elements:
          {
            var uintL len = Svector_length(obj);
            if (!(len==0)) {
              var gcv_object_t* objptr = &TheSvector(obj)->data[0];
              dotimespL(len,len, { subst_circ_mark(&(*objptr++),env); } );
            }
          }
          return;
        case_mdarray: case_ovector:
          # non-simple array, no string or bit-vector
          if (mlb_add(&env->bitmap,obj)) # object already marked?
            return;
          # traverse data-vector: end-recursive subst_circ_mark(data-vector)
          ptr = &TheIarray(obj)->data; goto enter_subst;
        case_instance: /* Record */
        case_closure: _case_structure _case_stream case_orecord: case_lrecord:
          #ifndef TYPECODES
          switch (Record_type(obj)) {
            case_Rectype_Svector_above;
            case_Rectype_mdarray_above;
            case_Rectype_ovector_above;
            case_Rectype_bvector_above;
            case_Rectype_b2vector_above;
            case_Rectype_b4vector_above;
            case_Rectype_b8vector_above;
            case_Rectype_b16vector_above;
            case_Rectype_b32vector_above;
            case_Rectype_string_above;
            case_Rectype_number_above;
            case_Rectype_Symbol_above;
            default: ;
          }
          #endif
          if (Record_type(obj) == Rectype_BigReadLabel) {
            # BigReadLabel
            # Search read-label obj in the alist:
            var object alist = env->alist;
            while (consp(alist)) {
              var object acons = Car(alist);
              if (eq(Car(acons),obj)) {
                # Found.
                # Replace *ptr = obj = (car acons) with (cdr acons), but
                # leave the mark bit untouched:
                *ptr = (marked(ptr) ? with_mark_bit(Cdr(acons)) : (object)Cdr(acons));
                return;
              }
              alist = Cdr(alist);
            }
            # not found -> abort
            env->bad = obj;
            longjmp(env->abbruch_context,true);
          }
          if (mlb_add(&env->bitmap,obj)) # object already marked?
            return;
          # On replacement of Read-Labels in Hash-Tables their structure
          # is invalidated (because the hash-function of the objects stored
          # in it changes).
          if (Record_type(obj) == Rectype_Hashtable) # a hash-table?
            set_ht_invalid(TheHashtable(obj)); # yes -> note for reorganization
          # traverse all elements:
          {
            var uintC len = Record_nonweak_length(obj);
            if (!(len==0)) {
              var gcv_object_t* objptr = &TheRecord(obj)->recdata[0];
              dotimespC(len,len, { subst_circ_mark(&(*objptr++),env); } );
            }
          }
          return;
        #ifdef TYPECODES
        case_system: # Frame-Pointer or Small-Read-Label or System
          if (!(as_oint(obj) & wbit(0+oint_addr_shift))) {
            # Frame-Pointer
          } else
            # Small-Read-Label or System
            if (as_oint(obj) & wbit(oint_data_len-1+oint_addr_shift)) {
              # System
            } else
        #endif
            case_small_read_label:
              # Small-Read-Label
              {
                # Search read-label obj in the alist:
                var object alist = env->alist;
                while (consp(alist)) {
                  var object acons = Car(alist);
                  if (eq(Car(acons),obj)) {
                    # Found.
                    # Replace *ptr = obj = (car acons) with (cdr acons), but
                    # leave the mark bit untouched:
                    *ptr = (marked(ptr) ? with_mark_bit(Cdr(acons)) : (object)Cdr(acons));
                    return;
                  }
                  alist = Cdr(alist);
                }
                # not found -> abort
                env->bad = obj;
                longjmp(env->abbruch_context,true);
              }
          return;
        case_cons: # Cons
          if (mlb_add(&env->bitmap,obj)) # Object already marked?
            return;
          # recursive: subst_circ_mark(&Car(obj))
          subst_circ_mark(&TheCons(obj)->car,env);
          # end-recursive: subst_circ_mark(&Cdr(obj))
          ptr = &TheCons(obj)->cdr; goto enter_subst;
        case_machine: # Machine Pointer
        case_bvector: # Bit-Vector
        case_b2vector: # 2Bit-Vector
        case_b4vector: # 4Bit-Vector
        case_b8vector: # 8Bit-Vector
        case_b16vector: # 16Bit-Vector
        case_b32vector: # 32Bit-Vector
        case_string: # String
        case_char: # Character
        case_subr: # SUBR
        case_number: # Zahl
        case_symbol: # Symbol
          # Object contains no references -> do nothing
          return;
        default: NOTREACHED;
      }
    }
  }

  global object subst_circ (gcv_object_t* ptr, object alist)
  {
    var subst_circ_global my_global;
    my_global.alist = alist;
    set_break_sem_1(); # disable Break
    if (!setjmp(my_global.abbruch_context)) {
      bcopy(my_global.abbruch_context,my_global.bitmap.oom_context,sizeof(jmp_buf));
      mlb_alloc(&my_global.bitmap);
      subst_circ_mark(ptr,&my_global); # mark and substitute
      mlb_free(&my_global.bitmap);
      clr_break_sem_1(); # allow Break again
      return nullobj;
    } else {
      # abort from within subst_circ_mark()
      mlb_free(&my_global.bitmap);
      clr_break_sem_1(); # allow Break again
      #if !(defined(NO_SP_CHECK) || defined(NOCOST_SP_CHECK))
      if (eq(my_global.bad,nullobj)) {
        SP_ueber();
      }
      #endif
      return my_global.bad; # because of erroneous reference
    }
  }

#else # !MULTITHREAD

#if 0 # without consideration of circularities

  local void subst (gcv_object_t* ptr);
  local object subst_circ_alist;
  local jmp_buf subst_circ_jmpbuf;
  local object subst_circ_bad;
  global object subst_circ (gcv_object_t* ptr, object alist)
  {
    subst_circ_alist = alist;
    begin_setjmp_call();
    if (!setjmp(subst_circ_jmpbuf)) {
      end_setjmp_call(); subst(ptr); return nullobj;
    } else {
      # abort because of erroneous reference
      end_longjmp_call(); return subst_circ_bad;
    }
  }
  local void subst (gcv_object_t* ptr)
  {
    check_SP();
   enter_subst:
    {
      var object obj = *ptr;
      # fall differentiation by type:
      # Objects without sub-objects (machine pointers, bit-vectors,
      # strings, characters, subrs, integers, floats) contain no
      # references. The same holds true for Symbols and rational numbers (their
      # sub-objects could not have been entered in #n= - Syntax)
      # and complex numbers (for their components only
      # integers, floats, rational numbers are allowed, which means: objects,
      # that cannot contain references themselves).
      #ifdef TYPECODES
      switch (mtypecode(*ptr))
      #else
      if (orecordp(obj)) {
        goto case_orecord;
      } elif (consp(obj)) {
        goto case_cons;
      } elif (immediate_number_p(obj)) {
        goto case_number;
      } elif (charp(obj)) {
        goto case_char;
      } elif (subrp(obj)) {
        goto case_subr;
      } elif (machinep(obj)) {
        goto case_machine;
      } elif (small_read_label_p(obj)) {
        goto case_small_read_label;
      } elif (systemp(obj)) {
        return;
      } else switch (0)
      #endif
      {
        case_svector: # Simple-Vector
          # traverse all elements:
          {
            var uintL len = Svector_length(obj);
            if (!(len==0)) {
              var gcv_object_t* objptr = &TheSvector(obj)->data[0];
              dotimespL(len,len, { subst(&(*objptr++)); } );
            }
          }
          break;
        case_mdarray: case_ovector:
          # non-simple array, no string or bit-vector
          # traverse data-vector: end-recursive subst(data-vector)
          ptr = &TheIarray(obj)->data; goto enter_subst;
        case_instance: /* Record */
        case_closure: _case_structure _case_stream case_orecord: case_lrecord:
          #ifndef TYPECODES
          switch (Record_type(obj)) {
            case_Rectype_Svector_above;
            case_Rectype_mdarray_above;
            case_Rectype_ovector_above;
            case_Rectype_bvector_above;
            case_Rectype_b2vector_above;
            case_Rectype_b4vector_above;
            case_Rectype_b8vector_above;
            case_Rectype_b16vector_above;
            case_Rectype_b32vector_above;
            case_Rectype_string_above;
            case_Rectype_number_above;
            case_Rectype_Symbol_above;
            default: ;
          }
          #endif
          if (Record_type(obj) == Rectype_BigReadLabel) {
            # BigReadLabel
            # Search read-label obj in the alist:
            var object alist = subst_circ_alist;
            while (consp(alist)) {
              var object acons = Car(alist);
              if (eq(Car(acons),obj)) {
                # Found.
                # Replace *ptr = obj = (car acons) with (cdr acons):
                *ptr = Cdr(acons);
                return;
              }
              alist = Cdr(alist);
            }
            # not found -> abort
            subst_circ_bad = obj;
            begin_longjmp_call();
            longjmp(subst_circ_jmpbuf,true);
          }
          # traverse all elements:
          {
            var uintC len = Record_nonweak_length(obj);
            if (!(len==0)) {
              var gcv_object_t* objptr = &TheRecord(obj)->recdata[0];
              dotimespC(len,len, { subst(&(*objptr++)); } );
            }
          }
          break;
        #ifdef TYPECODES
        case_system: # Frame-Pointer or Small-Read-Label or System
          if (!(as_oint(obj) & wbit(0+oint_addr_shift))) {
            # Frame-Pointer
          } else
            # Small-Read-Label or System
            if (as_oint(obj) & wbit(oint_data_len-1+oint_addr_shift)) {
              # System
            } else
        #endif
            case_small_read_label:
              # Small-Read-Label
              {
                # Search read-label obj in the alist:
                var object alist = subst_circ_alist;
                while (consp(alist)) {
                  var object acons = Car(alist);
                  if (eq(Car(acons),obj)) {
                    # Found.
                    # Replace *ptr = obj = (car acons) with (cdr acons):
                    *ptr = Cdr(acons);
                    return;
                  }
                  alist = Cdr(alist);
                }
                # not found -> abort
                subst_circ_bad = obj;
                begin_longjmp_call();
                longjmp(subst_circ_jmpbuf,true);
              }
          break;
        case_cons: # Cons
          # recursive: subst(&Car(obj))
          subst(&TheCons(obj)->car);
          # endrecursive: subst(&Cdr(obj))
          ptr = &TheCons(obj)->cdr; goto enter_subst;
        case_machine: # Machine Pointer
        case_bvector: # Bit-Vector
        case_b2vector: # 2Bit-Vector
        case_b4vector: # 4Bit-Vector
        case_b8vector: # 8Bit-Vector
        case_b16vector: # 16Bit-Vector
        case_b32vector: # 32Bit-Vector
        case_string: # String
        case_char: # Character
        case_subr: # SUBR
        case_number: # Zahl
        case_symbol: # Symbol
          # Object contains no references -> do nothing
          break;
        default: NOTREACHED;
      }
    }
  }

#else # with consideration of circularities

# Method:
# Mark the objects recursively, in which the substitution has just been
# performed. Then unmark the object recursively.

  local void subst_circ_mark (gcv_object_t* ptr);
  local void subst_circ_unmark (gcv_object_t* ptr);
  local object subst_circ_alist;
  local jmp_buf subst_circ_jmpbuf;
  local object subst_circ_bad;
  global object subst_circ (gcv_object_t* ptr, object alist)
  {
    subst_circ_alist = alist;
    set_break_sem_1(); # disable Break
    if (!setjmp(subst_circ_jmpbuf)) {
      subst_circ_mark(ptr); # mark and substitute
      subst_circ_unmark(ptr); # delete marks again
      clr_break_sem_1(); # allow Break again
      return nullobj;
    } else {
      # abort from within subst_circ_mark()
      subst_circ_unmark(ptr); # first unmark everything
      clr_break_sem_1(); # allow Break again
      #if !(defined(NO_SP_CHECK) || defined(NOCOST_SP_CHECK))
      if (eq(subst_circ_bad,nullobj)) {
        SP_ueber();
      }
      #endif
      return subst_circ_bad; # because of erroneous reference
    }
  }
  local void subst_circ_mark (gcv_object_t* ptr)
  {
    #if !(defined(NO_SP_CHECK) || defined(NOCOST_SP_CHECK))
    if (SP_overflow()) { # check SP-depth
      subst_circ_bad = nullobj; longjmp(subst_circ_jmpbuf,true); # abort
    }
    #endif
   enter_subst:
    {
      var object obj = without_mark_bit(*ptr);
      # fall differentiation by type:
      # Objects without sub-objects (machine pointers, bit-vectors,
      # strings, characters, subrs, integers, floats) contain no
      # references. The same holds true for Symbols and rational numbers (their
      # sub-objects could not have been entered in #n= - Syntax)
      # and complex numbers (for their components only
      # integers, floats, rational numbers are allowed, which means: objects,
      # that cannot contain references themselves).
      #ifdef TYPECODES
      switch (typecode(obj))
      #else
      if (orecordp(obj)) {
        goto case_orecord;
      } elif (consp(obj)) {
        goto case_cons;
      } elif (immediate_number_p(obj)) {
        goto case_number;
      } elif (charp(obj)) {
        goto case_char;
      } elif (subrp(obj)) {
        goto case_subr;
      } elif (machinep(obj)) {
        goto case_machine;
      } elif (small_read_label_p(obj)) {
        goto case_small_read_label;
      } elif (systemp(obj)) {
        return;
      } else switch (0)
      #endif
      {
        case_svector: # Simple-Vector
          if (marked(TheSvector(obj))) # object already marked?
            return;
          mark(TheSvector(obj)); # mark
          # traverse all elements:
          {
            var uintL len = Svector_length(obj);
            if (!(len==0)) {
              var gcv_object_t* objptr = &TheSvector(obj)->data[0];
              dotimespL(len,len, { subst_circ_mark(&(*objptr++)); } );
            }
          }
          return;
        case_mdarray: case_ovector:
          # non-simple array, no string or bit-vector
          if (marked(TheIarray(obj))) # object already marked?
            return;
          mark(TheIarray(obj)); # mark
          # traverse data-vector: end-recursive subst_circ_mark(data-vector)
          ptr = &TheIarray(obj)->data; goto enter_subst;
        case_closure: _case_structure _case_stream case_orecord: case_instance: case_lrecord: # Record
          #ifndef TYPECODES
          switch (Record_type(obj)) {
            case_Rectype_Svector_above;
            case_Rectype_mdarray_above;
            case_Rectype_ovector_above;
            case_Rectype_bvector_above;
            case_Rectype_b2vector_above;
            case_Rectype_b4vector_above;
            case_Rectype_b8vector_above;
            case_Rectype_b16vector_above;
            case_Rectype_b32vector_above;
            case_Rectype_string_above;
            case_Rectype_number_above;
            case_Rectype_Symbol_above;
            default: ;
          }
          #endif
          if (Record_type(obj) == Rectype_BigReadLabel) {
            # BigReadLabel
            # Search read-label obj in the alist:
            var object alist = subst_circ_alist;
            while (consp(alist)) {
              var object acons = Car(alist);
              if (eq(Car(acons),obj)) {
                # Found.
                # Replace *ptr = obj = (car acons) with (cdr acons), but
                # leave the mark bit untouched:
                *ptr = (marked(ptr) ? with_mark_bit(Cdr(acons)) : (object)Cdr(acons));
                return;
              }
              alist = Cdr(alist);
            }
            # not found -> abort
            subst_circ_bad = obj;
            longjmp(subst_circ_jmpbuf,true);
          }
          if (marked(TheRecord(obj))) # object already marked?
            return;
          mark(TheRecord(obj)); # mark
          # On replacement of Read-Labels in Hash-Tables their structure
          # is invalidated (because the hash-function of the objects stored
          # in it changes).
          if (Record_type(obj) == Rectype_Hashtable) # a Hash-Table ?
            set_ht_invalid(TheHashtable(obj)); # yes -> note for reorganization
          # traverse all elements:
          {
            var uintC len = Record_nonweak_length(obj);
            if (!(len==0)) {
              var gcv_object_t* objptr = &TheRecord(obj)->recdata[0];
              dotimespC(len,len, { subst_circ_mark(&(*objptr++)); } );
            }
          }
          return;
        #ifdef TYPECODES
        case_system: # Frame-Pointer or Small-Read-Label or System
          if (!(as_oint(obj) & wbit(0+oint_addr_shift))) {
            # Frame-Pointer
          } else
            # Small-Read-Label or System
            if (as_oint(obj) & wbit(oint_data_len-1+oint_addr_shift)) {
              # System
            } else
        #endif
            case_small_read_label:
              # Small-Read-Label
              {
                # Search read-label obj in the Alist:
                var object alist = subst_circ_alist;
                while (consp(alist)) {
                  var object acons = Car(alist);
                  if (eq(Car(acons),obj)) {
                    # Found.
                    # Replace *ptr = obj = (car acons) with (cdr acons), but
                    # leave the mark bit untouched:
                    *ptr = (marked(ptr) ? with_mark_bit(Cdr(acons)) : (object)Cdr(acons));
                    return;
                  }
                  alist = Cdr(alist);
                }
                # not found -> abort
                subst_circ_bad = obj;
                longjmp(subst_circ_jmpbuf,true);
              }
          return;
        case_cons: # Cons
          if (marked(TheCons(obj))) # object already marked?
            return;
          mark(TheCons(obj)); # mark
          # recursive: subst_circ_mark(&Car(obj))
          subst_circ_mark(&TheCons(obj)->car);
          # end-recursive: subst_circ_mark(&Cdr(obj))
          ptr = &TheCons(obj)->cdr; goto enter_subst;
        case_machine: # Machine Pointer
        case_bvector: # Bit-Vector
        case_b2vector: # 2Bit-Vector
        case_b4vector: # 4Bit-Vector
        case_b8vector: # 8Bit-Vector
        case_b16vector: # 16Bit-Vector
        case_b32vector: # 32Bit-Vector
        case_string: # String
        case_char: # Character
        case_subr: # SUBR
        case_number: # Zahl
        case_symbol: # Symbol
          # Object contains no references -> do nothing
          return;
        default: NOTREACHED;
      }
    }
  }
  local void subst_circ_unmark (gcv_object_t* ptr)
  {
   enter_subst:
    {
      var object obj = *ptr;
      # fall differentiation according to type, like above:
      #ifdef TYPECODES
      switch (typecode(obj))
      #else
      if (orecordp(obj)) {
        goto case_orecord;
      } elif (consp(obj)) {
        goto case_cons;
      } elif (immediate_number_p(obj)) {
        goto case_number;
      } elif (charp(obj)) {
        goto case_char;
      } elif (subrp(obj)) {
        goto case_subr;
      } elif (machinep(obj)) {
        goto case_machine;
      } elif (small_read_label_p(obj) || systemp(obj)) {
        goto case_system;
      } else switch (0)
      #endif
      {
        case_svector: # Simple-Vector
          if (!marked(TheSvector(obj))) # already unmarked?
            return;
          unmark(TheSvector(obj)); # unmark
          # traverse all elements:
          {
            var uintL len = Svector_length(obj);
            if (!(len==0)) {
              var gcv_object_t* objptr = &TheSvector(obj)->data[0];
              dotimespL(len,len, { subst_circ_unmark(&(*objptr++)); } );
            }
          }
          return;
        case_mdarray: case_ovector:
          # non-simple array, no string or bit-vector
          if (!marked(TheIarray(obj))) # already unmarked?
            return;
          unmark(TheIarray(obj)); # unmark
          # traverse data-vector: end-recursive subst_circ_unmark(data-vector)
          ptr = &TheIarray(obj)->data; goto enter_subst;
        case_instance: /* Record */
        case_closure: _case_structure _case_stream case_orecord: case_lrecord:
          #ifndef TYPECODES
          switch (Record_type(obj)) {
            case_Rectype_Svector_above;
            case_Rectype_mdarray_above;
            case_Rectype_ovector_above;
            case_Rectype_bvector_above;
            case_Rectype_b2vector_above;
            case_Rectype_b4vector_above;
            case_Rectype_b8vector_above;
            case_Rectype_b16vector_above;
            case_Rectype_b32vector_above;
            case_Rectype_string_above;
            case_Rectype_number_above;
            case_Rectype_Symbol_above;
            default: ;
          }
          #endif
          if (Record_type(obj) == Rectype_BigReadLabel)
            return;
          if (!marked(TheRecord(obj))) # already unmarked?
            return;
          unmark(TheRecord(obj)); # unmark
          # traverse all elements:
          {
            var uintC len = Record_nonweak_length(obj);
            if (!(len==0)) {
              var gcv_object_t* objptr = &TheRecord(obj)->recdata[0];
              dotimespC(len,len, { subst_circ_unmark(&(*objptr++)); } );
            }
          }
          return;
        case_cons: # Cons
          if (!marked(TheCons(obj))) # already unmarked?
            return;
          unmark(TheCons(obj)); # unmark
          # recursive: subst_circ_unmark(&Car(obj))
          subst_circ_unmark(&TheCons(obj)->car);
          # end-recursive: subst_circ_unmark(&Cdr(obj))
          ptr = &TheCons(obj)->cdr; goto enter_subst;
        case_system: # Frame-Pointer or Small-Read-Label or System
        case_machine: # Machine Pointer
        case_bvector: # Bit-Vector
        case_b2vector: # 2Bit-Vector
        case_b4vector: # 4Bit-Vector
        case_b8vector: # 8Bit-Vector
        case_b16vector: # 16Bit-Vector
        case_b32vector: # 32Bit-Vector
        case_string: # String
        case_char: # Character
        case_subr: # SUBR
        case_number: # Zahl
        case_symbol: # Symbol
          # Object contains no references -> do nothing
          return;
        default: NOTREACHED;
      }
    }
  }

#endif

#endif
