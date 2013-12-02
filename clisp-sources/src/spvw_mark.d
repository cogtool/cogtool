# Setting mark bits in objects.

# ------------------------------ Specification ---------------------------------

# To mark a heap object (for the purpose of GC or of circularity analysis), we
# set the garcol_bit in the first word (i.e. the GCself header for varobjects).
# Immediate objects can not (and need not) be marked.

# Set the mark bit at a given address.
# mark(addr);
# local void mark (void* addr);

# Clear the mark bit at a given address.
# unmark(addr);
# local void unmark (void* addr);

# Tests the mark bit at a given address.
# marked(addr)
# local bool marked (void* addr);

# Add a mark bit to an object pointer.
# with_mark_bit(obj)
# local object with_mark_bit (object obj);

# Remove a mark bit from an object pointer.
# without_mark_bit(obj)
# local object without_mark_bit (object obj);

# ------------------------------ Implementation --------------------------------

  #if defined(WIDE_STRUCT) || defined(WIDE_AUXI) || defined(OBJECT_STRUCT)
    #define mark(addr)  (((gcv_object_t*)(addr))->one_o |= wbit(garcol_bit_o))
  #else
    #define mark(addr)  (*(gcv_object_t*)(addr) = as_object(as_oint(*(gcv_object_t*)(addr)) | wbit(garcol_bit_o)))
  #endif

  #if defined(WIDE_STRUCT) || defined(WIDE_AUXI) || defined(OBJECT_STRUCT)
    #define unmark(addr)  (((gcv_object_t*)(addr))->one_o &= ~wbit(garcol_bit_o))
  #else
    #define unmark(addr)  (*(gcv_object_t*)(addr) = as_object(as_oint(*(gcv_object_t*)(addr)) & ~wbit(garcol_bit_o)))
  #endif

  #ifdef fast_mtypecode
    #define marked(addr)  (mtypecode(*(gcv_object_t*)(addr)) & bit(garcol_bit_t))
  #else
    #if defined(WIDE_STRUCT) || defined(WIDE_AUXI) || defined(OBJECT_STRUCT)
      #define marked(addr)  (((gcv_object_t*)(addr))->one_o & wbit(garcol_bit_o))
    #else
      #define marked(addr)  (as_oint(*(gcv_object_t*)(addr)) & wbit(garcol_bit_o))
    #endif
  #endif

  #define with_mark_bit(obj)  as_object(as_oint(obj) | wbit(garcol_bit_o))
  #define without_mark_bit(obj)  as_object(as_oint(obj) & ~wbit(garcol_bit_o))
