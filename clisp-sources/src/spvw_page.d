# Memory management data structures, part 1: Page

# ------------------------------ Specification --------------------------------

# A page is a contiguous range of memory that is filled or can be filled with
# objects.

# Low level descriptor of a page.
typedef struct {
  aint start;   # pointer to the occupied space (aligned)
  aint end;     # pointer right after the occupied space (aligned)
  #ifdef DEBUG_GCSAFETY
  struct
  #else
  union
  #endif
        { gcv_object_t firstmarked; uintM l; aint d; void* next; }
        gcpriv; # private variable during GC
} _Page;

# Page descriptor with corresponding management data:
# typedef ... Page;
# This is a structure type containing the fields page_start, page_end,
# page_gcpriv (of the same types as above).

#ifdef SPVW_PAGES

# Number of relevant bits (counted from bit 0 upward) in an address.
# oint_addr_relevant_len

# Smallest size of a page, including its management data.
# min_page_size_brutto

# Size of a normal page (for small objects), in bytes available for objects.
# std_page_size

#endif

# ------------------------------ Implementation -------------------------------


#ifdef SPVW_BLOCKS

typedef _Page Page;
#define page_start   start
#define page_end     end
#define page_gcpriv  gcpriv

#endif


#ifdef SPVW_PAGES

#ifndef VIRTUAL_MEMORY
  # Every page has a header for AVL tree management. The benefit is that the
  # AVL tree management itself does not need to call malloc().
#else # VIRTUAL_MEMORY
  # On systems with virtual memory it is bad if the GC must touch all pages.
  # Therefore the AVL tree management is separate.
  #define AVL_SEPARATE
#endif

#define AVLID  spvw
#define AVL_ELEMENT  uintM
#define AVL_EQUAL(element1,element2)  ((element1)==(element2))
#define AVL_KEY  AVL_ELEMENT
#define AVL_KEYOF(element)  (element)
#define AVL_SIGNED_INT  sintL
#define AVL_COMPARE(key1,key2)  (sintL)((key1)-(key2))
#define NO_AVL_MEMBER0
#define NO_AVL_MEMBER
#define NO_AVL_INSERT
#define NO_AVL_DELETE

#include "avl.c"

typedef struct NODE {
  NODEDATA nodedata;        # NODE for AVL-tree-management
  #define page_room  nodedata.value # free space in this page (in bytes)
  _Page page;       # page-descriptor, consisting of:
  #define page_start  page.start  # pointer to the occupied space (aligned)
  #define page_end    page.end    # pointer to the free space (aligned)
  #define page_gcpriv page.gcpriv # private variable during GC
  aint m_start;     # start address delivered by malloc (unaligned)
  aint m_length;    # specified page-length at malloc (in bytes)
} NODE;
#define HAVE_NODE

#if !defined(AVL_SEPARATE)
  # NODE within the page
  #define sizeof_NODE  sizeof(NODE)
  #define page_start0(page)  round_up((aint)page+sizeof(NODE),varobject_alignment)
  #define free_page(page)  begin_system_call(); free((void*)page->m_start); end_system_call();
#else
  # NODE extra
  #define sizeof_NODE  0
  #define page_start0(page)  round_up(page->m_start,varobject_alignment)
  #define free_page(page)  begin_system_call(); free((void*)page->m_start); free((void*)page); end_system_call();
#endif

#include "avl.c"

typedef NODE Page;

# size of a normal page = minimal page size. Divisible by sizeof(cons_) .
  # If you do not want the offset_pages_len (see above) to grow too large, the page
  # size must not be too small.
  #if (oint_addr_len<=32)
    #define oint_addr_relevant_len  oint_addr_len
  #else
    #if defined(DECALPHA) && (defined(UNIX_OSF) || defined(UNIX_LINUX))
      # all addresses are situated between 1*2^32 and 2*2^32. In fact, it is
      # only a 2^32 size address space.
      #define oint_addr_relevant_len  32
    #endif
    #if (defined(IA64) || defined(AMD64)) && defined(UNIX_LINUX)
      # It is unlikely that more than 4 GB will be used.
      #define oint_addr_relevant_len  32
    #endif
    #if !defined(oint_addr_relevant_len)
      /* safe bet */
      #define oint_addr_relevant_len 32
    #endif
  #endif
  #define min_page_size_brutto  bit(oint_addr_relevant_len/2)
  #define std_page_size  round_down(min_page_size_brutto-sizeof_NODE-(varobject_alignment-1),sizeof(cons_))

#endif
