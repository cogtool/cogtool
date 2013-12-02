# Extern-Deklarationen zu ARILEV1.D

BEGIN_DECLS

#ifdef COPY_LOOPS

extern uintD* copy_loop_up (const uintD* sourceptr, uintD* destptr, uintC count);

extern uintD* copy_loop_down (const uintD* sourceptr, uintD* destptr, uintC count);

#endif

#ifdef FILL_LOOPS

extern uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler);

extern uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler);

#endif

#ifdef CLEAR_LOOPS

extern uintD* clear_loop_up (uintD* destptr, uintC count);

extern uintD* clear_loop_down (uintD* destptr, uintC count);

#endif

#ifdef LOG_LOOPS

extern void or_loop_up (uintD* xptr, const uintD* yptr, uintC count);

extern void xor_loop_up (uintD* xptr, const uintD* yptr, uintC count);

extern void and_loop_up (uintD* xptr, const uintD* yptr, uintC count);

extern void eqv_loop_up (uintD* xptr, const uintD* yptr, uintC count);

extern void nand_loop_up (uintD* xptr, const uintD* yptr, uintC count);

extern void nor_loop_up (uintD* xptr, const uintD* yptr, uintC count);

extern void andc2_loop_up (uintD* xptr, const uintD* yptr, uintC count);

extern void orc2_loop_up (uintD* xptr, const uintD* yptr, uintC count);

extern void not_loop_up (uintD* xptr, uintC count);

#endif

#ifdef TEST_LOOPS

extern /*bool*/int and_test_loop_up (const uintD* xptr, const uintD* yptr, uintC count);

extern /*bool*/int test_loop_up (const uintD* ptr, uintC count);

extern signean compare_loop_up (const uintD* xptr, const uintD* yptr, uintC count);

#endif

#ifdef ADDSUB_LOOPS

extern uintD add_loop_down (const uintD* sourceptr1, const uintD* sourceptr2, uintD* destptr, uintC count);

extern uintD addto_loop_down (const uintD* sourceptr, uintD* destptr, uintC count);

extern uintD inc_loop_down (uintD* ptr, uintC count);

extern uintD sub_loop_down (const uintD* sourceptr1, const uintD* sourceptr2, uintD* destptr, uintC count);

extern uintD subx_loop_down (const uintD* sourceptr1, const uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);

extern uintD subfrom_loop_down (const uintD* sourceptr, uintD* destptr, uintC count);

extern uintD dec_loop_down (uintD* ptr, uintC count);

extern uintD neg_loop_down (uintD* ptr, uintC count);

#endif

#ifdef SHIFT_LOOPS

extern uintD shift1left_loop_down (uintD* ptr, uintC count);

extern uintD shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry);

extern uintD shiftleftcopy_loop_down (const uintD* sourceptr, uintD* destptr, uintC count, uintC i);

extern uintD shift1right_loop_up (uintD* ptr, uintC count, uintD carry);

extern uintD shiftright_loop_up (uintD* ptr, uintC count, uintC i);

extern uintD shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i);

extern uintD shiftrightcopy_loop_up (const uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry);

#endif

#ifdef MUL_LOOPS

extern uintD mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit);

extern void mulu_loop_down (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len);

extern uintD muluadd_loop_down (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len);

extern uintD mulusub_loop_down (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len);

#endif

#ifdef DIV_LOOPS

extern uintD divu_loop_up (uintD digit, uintD* ptr, uintC len);

extern uintD divucopy_loop_up (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len);

#endif

END_DECLS

