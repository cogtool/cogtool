# Externe Routinen zu ARILEV1.D
# Prozessor: VAX
# Compiler: GNU-C oder VAX C
# Assembler: Unix "as"
# Parameter-Übergabe: auf dem Stack 4(ap),8(ap),...
# Rückgabewert: in Register r0
# Register r0-r5 frei verwendbar, zu rettende Register ab r6 sind
#   in der .word-Anweisung anzugeben (Bitmaske: Bit i für Register ri)
# Einstellungen: intCsize=32, intDsize=32.

#ifdef INCLUDED_FROM_C

  #define COPY_LOOPS
  #define FILL_LOOPS
  #define CLEAR_LOOPS
  #define LOG_LOOPS
  #define TEST_LOOPS
  #define ADDSUB_LOOPS

#else

           .globl _copy_loop_up
           .globl _copy_loop_down
           .globl _fill_loop_up
           .globl _fill_loop_down
           .globl _clear_loop_up
           .globl _clear_loop_down
           .globl _or_loop_up
           .globl _xor_loop_up
           .globl _and_loop_up
           .globl _eqv_loop_up
           .globl _nand_loop_up
           .globl _nor_loop_up
           .globl _andc2_loop_up
           .globl _orc2_loop_up
           .globl _not_loop_up
           .globl _and_test_loop_up
           .globl _test_loop_up
           .globl _compare_loop_up
           .globl _add_loop_down
           .globl _addto_loop_down
           .globl _inc_loop_down
           .globl _sub_loop_down
           .globl _subx_loop_down
           .globl _subfrom_loop_down
           .globl _dec_loop_down
           .globl _neg_loop_down

#ifndef __GNUC__ /* mit GNU-C machen wir mulu32() als Macro, der inline multipliziert */
           .globl _mulu32_
#
# Aus gcc-2.3.3/config/vax.md:
# (define_insn ""
#   [(set (match_operand:DI 0 "general_operand" "=g")
#         (plus:DI
#          (mult:DI (sign_extend:DI
#                    (match_operand:SI 1 "nonimmediate_operand" "g"))
#                   (sign_extend:DI
#                    (match_operand:SI 2 "nonimmediate_operand" "g")))
#          (sign_extend:DI (match_operand:SI 3 "nonimmediate_operand" "g"))))]
#   ""
#   "emul %1,%2,%3,%0")
#
           .align 1
# extern struct { uint32 lo; uint32 hi; } mulu32_ (uint32 arg1, uint32 arg2);
# 2^32*hi+lo := arg1*arg2.
_mulu32_:  # Input in r2,r3, Output in r0,mulu32_high
           .word 0
           movl 4(ap),r2
           movl 8(ap),r3
           emul r2,r3,$0,r0 # 2^32*r1 + r0 := r2 * r3 (signed multiplication)
           tstl r2
           bgeq _m1
           addl2 r3,r1
_m1:       tstl r3
           bgeq _m2
           addl2 r2,r1
_m2:       # 2^32*r1 + r0 = r2 * r3 (unsigned multiplication)
           movl r1,_mulu32_high
           ret
#endif

#if 0
#ifndef __GNUC__ /* mit GNU-C machen wir divu_6432_3232() als Macro, der inline dividiert */
           .globl _divu_6432_3232_
#
# Aus gcc-2.3.3/config/vax.md:
# ;This is left out because it is very slow;
# ;we are better off programming around the "lack" of this insn.
# ;(define_insn "divmoddisi4"
# ;  [(set (match_operand:SI 0 "general_operand" "=g")
# ;       (div:SI (match_operand:DI 1 "general_operand" "g")
# ;               (match_operand:SI 2 "general_operand" "g")))
# ;   (set (match_operand:SI 3 "general_operand" "=g")
# ;       (mod:SI (match_operand:DI 1 "general_operand" "g")
# ;               (match_operand:SI 2 "general_operand" "g")))]
# ;  ""
# ;  "ediv %2,%1,%0,%3")
#
# Aus gcc-2.3.3/config/vax.md:
# (define_insn "divsi3"
#   [(set (match_operand:SI 0 "general_operand" "=g,g")
#         (div:SI (match_operand:SI 1 "general_operand" "0,g")
#                 (match_operand:SI 2 "general_operand" "g,g")))]
#   ""
#   "@
#    divl2 %2,%0
#    divl3 %2,%1,%0")
#
           .align 1
# extern struct { uint32 q; uint32 r; } divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
# x = 2^32*xhi+xlo = q*y+r schreiben. Sei bekannt, dass 0 <= x < 2^32*y .
_divu_6432_3232_: # Input in d1,d0,d2, Output in d0,divu_32_rest
           .word 0
           ??
           ret
#endif
#endif

           .align 1
# extern uintD* copy_loop_up (uintD* sourceptr, uintD* destptr, uintC count);
_copy_loop_up: # Input in r1,r0,r2, Output in r0
           .word 0
           movl 4(ap),r1
           movl 8(ap),r0
           movl 12(ap),r2
           sobgeq r2,clu1
           ret
clu1:        movl (r1)+,(r0)+
             sobgeq r2,clu1
           ret

           .align 1
# extern uintD* copy_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
_copy_loop_down: # Input in r1,r0,r2, Output in r0
           .word 0
           movl 4(ap),r1
           movl 8(ap),r0
           movl 12(ap),r2
           sobgeq r2,cld1
           ret
cld1:        movl -(r1),-(r0)
             sobgeq r2,cld1
           ret

           .align 1
# extern uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler);
_fill_loop_up: # Input in r0,r1,r2, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r1,flu1
           ret
flu1:        movl r2,(r0)+
             sobgeq r1,flu1
           ret

           .align 1
# extern uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler);
_fill_loop_down: # Input in r0,r1,r2, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r1,fld1
           ret
fld1:        movl r2,-(r0)
             sobgeq r1,fld1
           ret

           .align 1
# extern uintD* clear_loop_up (uintD* destptr, uintC count);
_clear_loop_up: # Input in r0,r1, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           sobgeq r1,clu2
           ret
clu2:        clrl (r0)+
             sobgeq r1,clu2
           ret

           .align 1
# extern uintD* clear_loop_down (uintD* destptr, uintC count);
_clear_loop_down: # Input in r0,r1, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           sobgeq r1,clu3
           ret
clu3:        clrl -(r0)
             sobgeq r1,clu3
           ret

           .align 1
# extern void or_loop_up (uintD* xptr, uintD* yptr, uintC count);
_or_loop_up: # Input in r0,r1,r2
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r2,olu1
           ret
olu1:        bisl2 (r1)+,(r0)+
             sobgeq r2,olu1
           ret

           .align 1
# extern void xor_loop_up (uintD* xptr, uintD* yptr, uintC count);
_xor_loop_up: # Input in r0,r1,r2
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r2,xlu1
           ret
xlu1:        xorl2 (r1)+,(r0)+
             sobgeq r2,xlu1
           ret

           .align 1
# extern void and_loop_up (uintD* xptr, uintD* yptr, uintC count);
_and_loop_up: # Input in r0,r1,r2
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r2,alu1
           ret
alu1:        mcoml (r1)+,r3
             bicl2 r3,(r0)+
             sobgeq r2,alu1
           ret

           .align 1
# extern void eqv_loop_up (uintD* xptr, uintD* yptr, uintC count);
_eqv_loop_up: # Input in r0,r1,r2
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r2,elu1
           ret
elu1:        xorl2 (r1)+,(r0)
             mcoml (r0),(r0)+
             sobgeq r2,elu1
           ret

           .align 1
# extern void nand_loop_up (uintD* xptr, uintD* yptr, uintC count);
_nand_loop_up: # Input in r0,r1,r2
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r2,nalu1
           ret
nalu1:       mcoml (r0),r3
             mcoml (r1)+,r4
             bisl3 r3,r4,(r0)+
             sobgeq r2,nalu1
           ret

           .align 1
# extern void nor_loop_up (uintD* xptr, uintD* yptr, uintC count);
_nor_loop_up: # Input in r0,r1,r2
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r2,nolu1
           ret
nolu1:       bisl2 (r1)+,(r0)
             mcoml (r0),(r0)+
             sobgeq r2,nolu1
           ret

           .align 1
# extern void andc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
_andc2_loop_up: # Input in r0,r1,r2, verändert r3
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r2,alu2
           ret
alu2:        bicl2 (r1)+,(r0)+
             sobgeq r2,alu2
           ret

           .align 1
# extern void orc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
_orc2_loop_up: # Input in r0,r1,r2, verändert r3
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r2,olu2
           ret
olu2:        mcoml (r1)+,r3
             bisl2 r3,(r0)+
             sobgeq r2,olu2
           ret

           .align 1
# extern void not_loop_up (uintD* xptr, uintC count);
_not_loop_up: # Input in r0,r1
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           sobgeq r1,nlu1
           ret
nlu1:        mcoml (r0),(r0)+
             sobgeq r1,nlu1
           ret

           .align 1
# extern bool and_test_loop_up (uintD* xptr, uintD* yptr, uintC count);
_and_test_loop_up: # Input in r0,r1,r2, verändert r3, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r2,atlu1
           clrl r0
           ret
atlu1:       mcoml (r0)+,r3
             bicl3 r3,(r1)+,r3
             bneq atlu2
             sobgeq r2,atlu1
           clrl r0
           ret
atlu2:     movl $1,r0
           ret

           .align 1
# extern bool test_loop_up (uintD* ptr, uintC count);
_test_loop_up: # Input in r0,r1, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           sobgeq r1,tlu1
           clrl r0
           ret
tlu1:        tstl (r0)+
             bneq tlu2
             sobgeq r1,tlu1
           clrl r0
           ret
tlu2:      movl $1,r0
           ret

           .align 1
# extern signean compare_loop_up (uintD* xptr, uintD* yptr, uintC count);
_compare_loop_up: # Input in r0,r1,r2, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r2,clu4
           clrl r0
           ret
clu4:        cmpl (r0)+,(r1)+
             bneq clu5
             sobgeq r2,clu4
           clrl r0
           ret
clu5:      blssu clu6
           movl $1,r0
           ret
clu6:      movl $-1,r0
           ret

           .align 1
# extern uintD add_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
_add_loop_down: # Input in r0,r1,r2,r3, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           movl 16(ap),r3
           sobgeq r3,ald1
           clrl r0
           ret
ald1:        # Addition ohne Carry:
             addl3 -(r0),-(r1),-(r2)
             bcs ald4
ald2:        sobgeq r3,ald1
           clrl r0
           ret
ald3:        # Addition mit Carry:
             addl3 -(r0),-(r1),-(r2)
             bcs ald5
             incl (r2)
             bcc ald2
ald4:        sobgeq r3,ald3
           movl $1,r0
           ret
ald5:        incl (r2)
             sobgeq r3,ald3
           movl $1,r0
           ret

           .align 1
# extern uintD addto_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
_addto_loop_down: # Input in r0,r1,r2, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r2,atld1
           clrl r0
           ret
atld1:       # Addition ohne Carry:
             addl2 -(r0),-(r1)
             bcs atld4
atld2:       sobgeq r2,atld1
           clrl r0
           ret
atld3:       # Addition mit Carry:
             addl2 -(r0),-(r1)
             bcs atld5
             incl (r1)
             bcc atld2
atld4:       sobgeq r2,atld3
           movl $1,r0
           ret
atld5:       incl (r1)
             sobgeq r2,atld3
           movl $1,r0
           ret

           .align 1
# extern uintD inc_loop_down (uintD* ptr, uintC count);
_inc_loop_down: # Input in r0,r1, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           sobgeq r1,ild1
           movl $1,r0
           ret
ild1:        incl -(r0)
             bcc ild2
             sobgeq r1,ild1
           movl $1,r0
           ret
ild2:      clrl r0
           ret

           .align 1
# extern uintD sub_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
_sub_loop_down: # Input in r0,r1,r2,r3, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           movl 16(ap),r3
           sobgeq r3,sld1
           clrl r0
           ret
sld1:        # Subtraktion ohne Carry:
             subl3 -(r0),-(r1),-(r2) # oder subl3 -(r1),-(r0),-(r2) ??
             bcs sld4
sld2:        sobgeq r3,sld1
           clrl r0
           ret
sld3:        # Subtraktion mit Carry:
             subl3 -(r0),-(r1),-(r2) # dito??
             bcs sld5
             decl (r2)
             bcc sld2
sld4:        sobgeq r3,sld3
           movl $-1,r0
           ret
sld5:        decl (r2)
             sobgeq r3,sld3
           movl $-1,r0
           ret

           .align 1
# extern uintD subx_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
_subx_loop_down: # Input in r0,r1,r2,r3, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           movl 16(ap),r3
           tstl 20(ap)
           beql sld2
           brb sld4

           .align 1
# extern uintD subfrom_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
_subfrom_loop_down: # Input in r0,r1,r2, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           movl 12(ap),r2
           sobgeq r2,sfld1
           clrl r0
           ret
sfld1:       # Subtraktion ohne Carry:
             subl2 -(r0),-(r1)
             bcs sfld4
sfld2:       sobgeq r2,sfld1
           clrl r0
           ret
sfld3:       # Subtraktion mit Carry:
             subl2 -(r0),-(r1)
             bcs sfld5
             decl (r1)
             bcc sfld2
sfld4:       sobgeq r2,sfld3
           movl $-1,r0
           ret
sfld5:       decl (r1)
             sobgeq r2,sfld3
           movl $-1,r0
           ret

           .align 1
# extern uintD dec_loop_down (uintD* ptr, uintC count);
_dec_loop_down: # Input in r0,r1, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           sobgeq r1,dld1
           movl $-1,r0
           ret
dld1:        decl -(r0)
             bcc dld2
             sobgeq r1,dld1
           movl $-1,r0
           ret
dld2:      clrl r0
           ret

           .align 1
# extern uintD neg_loop_down (uintD* ptr, uintC count);
_neg_loop_down: # Input in r0,r1, Output in r0
           .word 0
           movl 4(ap),r0
           movl 8(ap),r1
           # erstes Digit /=0 suchen:
           sobgeq r1,nld1
           clrl r0
           ret
nld1:        tstl -(r0)
             bneq nld2
             sobgeq r1,nld1
           clrl r0
           ret
nld2:      # erstes Digit /=0 gefunden, ab jetzt gibt's Carrys
           mnegl (r0),(r0)
           # alle anderen Digits invertieren:
           sobgeq r1,nld3
           movl $-1,r1
           ret
nld3:        mcoml -(r0),(r0) # geht das??
             sobgeq r1,nld3
           movl $-1,r1
           ret

           .end

#endif

