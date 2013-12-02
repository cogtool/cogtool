# Externe Routinen zu ARILEV1.D
# Prozessor: SPARC 64-bit
# Compiler: GNU-C oder ...
# Parameter-Übergabe: in Registern %o0-%o5.
#   Argumente vom Typ uint8, uint16, uint32 sind bereits vom Aufrufer zu
#   uint64 umgewandelt worden (zero-extend, "srl reg,0,reg").
#   Argumente vom Typ sint8, sint16, sint32 sind bereits vom Aufrufer zu
#   sint64 umgewandelt worden (sign-extend, "sra reg,0,reg").
#   Ergebnisse vom Typ uint8, uint16, uint32 müssen vor Rückgabe zu uint64
#   umgewandelt werden (zero-extend, "srl reg,0,reg").
#   Ergebnisse vom Typ sint8, sint16, sint32 müssen vor Rückgabe zu sint64
#   umgewandelt werden (sign-extend, "sra reg,0,reg").
# Einstellungen: intCsize=32, intDsize=32.

#ifdef INCLUDED_FROM_C

  #define COPY_LOOPS
  #define FILL_LOOPS
  #define CLEAR_LOOPS
  #define LOG_LOOPS
  #define TEST_LOOPS
  #define ADDSUB_LOOPS
  #define SHIFT_LOOPS
  #define MUL_LOOPS
  #define DIV_LOOPS

#else

#ifdef ASM_UNDERSCORE
  #define C(entrypoint) _##entrypoint
#else
  #define C(entrypoint) entrypoint
#endif

  # Indikatoren für Anweisungen (Instruktionen) in Delay-Slots
  # (diese werden VOR der vorigen Instruktion ausgeführt):
  #define _             # Instruktion, die stets ausgeführt wird
  #define __            # Instruktion, die nur im Sprung-Fall ausgeführt wird
  # Abkürzungen für Anweisungen:
  #define ret   return %i7+8  # return from subroutine
  #define retl  jmp %o7+8     # return from leaf subroutine (no save/restore)

  # Avoid "detect global register use not covered .register pseudo-op" error
  .register %g2,#scratch

        .seg "text"

        .global C(mulu16_),C(mulu32_),C(mulu32_unchecked)
        .global C(divu_6432_3232_),C(divu_3216_1616_)
        .global C(copy_loop_up),C(copy_loop_down),C(fill_loop_up),C(fill_loop_down)
        .global C(clear_loop_up),C(clear_loop_down)
        .global C(or_loop_up),C(xor_loop_up),C(and_loop_up),C(eqv_loop_up)
        .global C(nand_loop_up),C(nor_loop_up),C(andc2_loop_up),C(orc2_loop_up)
        .global C(not_loop_up)
        .global C(and_test_loop_up),C(test_loop_up),C(compare_loop_up)
        .global C(add_loop_down),C(addto_loop_down),C(inc_loop_down)
        .global C(sub_loop_down),C(subx_loop_down),C(subfrom_loop_down),C(dec_loop_down)
        .global C(neg_loop_down)
        .global C(shift1left_loop_down),C(shiftleft_loop_down),C(shiftleftcopy_loop_down)
        .global C(shift1right_loop_up),C(shiftright_loop_up),C(shiftrightsigned_loop_up),C(shiftrightcopy_loop_up)
        .global C(mulusmall_loop_down),C(mulu_loop_down),C(muluadd_loop_down),C(mulusub_loop_down)
        .global C(divu_loop_up),C(divucopy_loop_up)

#define LOOP_TYPE  1    # 1: Standard-Schleifen
                        # 2: Schleifen ohne Pointer, nur mit Zähler
#define STANDARD_LOOPS  (LOOP_TYPE==1)
#define COUNTER_LOOPS  (LOOP_TYPE==2)

# extern uint32 mulu16_ (uint16 arg1, uint16 arg2);
# ergebnis := arg1*arg2.
C(mulu16_:) # Input in %o0,%o1, Output in %o0
        umul %o0,%o1,%o2
        retl
       _ srl %o2,0,%o0

# extern struct { uint32 lo; uint32 hi; } mulu32_ (uint32 arg1, uint32 arg2);
# 2^32*hi+lo := arg1*arg2.
C(mulu32_:) # Input in %o0,%o1, Output in %o0,%g1
        umul %o0,%o1,%o2
        rd %y,%g1
        retl
       _ srl %o2,0,%o0

# extern uint32 mulu32_unchecked (uint32 x, uint32 y);
# ergebnis := arg1*arg2 < 2^32.
C(mulu32_unchecked:) # Input in %o0,%o1, Output in %o0
        umul %o0,%o1,%o2
        retl
       _ srl %o2,0,%o0

# extern struct { uint32 q; uint32 r; } divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
# x = 2^32*xhi+xlo = q*y+r schreiben. Sei bekannt, dass 0 <= x < 2^32*y .
C(divu_6432_3232_:) # Input in %o0,%o1,%o2, Output in %o0,%g1
        wr %o0,%g0,%y
        udiv %o1,%o2,%o0        # x durch y dividieren, %o0 := q
        umul %o0,%o2,%g1        # %g1 := (q*y) mod 2^32
        sub %o1,%g1,%g1         # %g1 := (xlo-q*y) mod 2^32 = r
        retl
       _ srl %o0,0,%o0

# extern struct { uint16 q; uint16 r; } divu_3216_1616_ (uint32 x, uint16 y);
# x = q*y+r schreiben. Sei bekannt, dass 0 <= x < 2^16*y .
C(divu_3216_1616_:) # Input in %o0,%o1, Output in %o0 (Rest und Quotient).
        wr %g0,%g0,%y
        udiv %o0,%o1,%o2        # dividieren, Quotient nach %o2
#if 0 # Who says that %y has some meaningful contents after `udiv' ??
        rd %y,%g1               # Rest aus %y
#else
        umul %o2,%o1,%g1        # %g1 := (q*y) mod 2^32
        sub %o0,%g1,%g1         # %g1 := (x-q*y) mod 2^32 = r
#endif
        sll %g1,16,%g1          # in die oberen 16 Bit schieben
        or %o2,%g1,%o0
        retl
       _ srl %o0,0,%o0

# extern uintD* copy_loop_up (uintD* sourceptr, uintD* destptr, uintC count);
C(copy_loop_up:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ nop
1:        ld [%o0],%o3
          add %o0,4,%o0
          st %o3,[%o1]
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o1,4,%o1
2:      retl
       _ mov %o1,%o0
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %o1,4,%o1
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &sourceptr[count]
        sub %o1,%o2,%o1         # %o1 = &destptr[count-1]
1:        ld [%o0+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          bne,pt %xcc,1b
         _ st %o3,[%o1+%o2]     # Digit ablegen
2:      retl
       _ add %o1,4,%o0
#endif

# extern uintD* copy_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
C(copy_loop_down:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %o0,4,%o0
1:        ld [%o0],%o3
          sub %o1,4,%o1
          st %o3,[%o1]
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ sub %o0,4,%o0
2:      retl
       _ mov %o1,%o0
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %o0,4,%o0
        sllx %o2,2,%o2          # %o2 = 4*count
        sub %o0,%o2,%o0         # %o0 = &sourceptr[-count-1]
        sub %o1,%o2,%o1         # %o1 = &destptr[-count]
1:        ld [%o0+%o2],%o3      # nächstes Digit holen
          subcc %o2,4,%o2       # Zähler erniedrigen, Pointer erniedrigen
          bne,pt %xcc,1b
         _ st %o3,[%o1+%o2]     # Digit ablegen
2:      retl
       _ mov %o1,%o0
#endif

# extern uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler);
C(fill_loop_up:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ nop
1:        st %o2,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sub %o0,4,%o0
        sub %g0,%o1,%o1         # %o1 = -count
        sllx %o1,2,%o1          # %o1 = -4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[count-1]
1:        addcc %o1,4,%o1       # Zähler "erniedrigen", Pointer erhöhen
          bne,pt %xcc,1b
         _ st %o2,[%o0+%o1]     # Digit ablegen
2:      retl
       _ add %o0,4,%o0
#endif

# extern uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler);
C(fill_loop_down:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sub %o0,4,%o0
1:        st %o2,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,1b
         _ sub %o0,4,%o0
2:      retl
       _ add %o0,4,%o0
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sllx %o1,2,%o1         # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[-count]
1:        subcc %o1,4,%o1       # Zähler erniedrigen, Pointer erniedrigen
          bne,pt %xcc,1b
         _ st %o2,[%o0+%o1]     # Digit ablegen
2:      retl
       _ nop
#endif

# extern uintD* clear_loop_up (uintD* destptr, uintC count);
C(clear_loop_up:) # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ nop
1:        st %g0,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sub %o0,4,%o0
        sub %g0,%o1,%o1         # %o1 = -count
        sllx %o1,2,%o1          # %o1 = -4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[count-1]
1:        addcc %o1,4,%o1       # Zähler "erniedrigen", Pointer erhöhen
          bne,pt %xcc,1b
         _ st %g0,[%o0+%o1]     # Digit 0 ablegen
2:      retl
       _ add %o0,4,%o0
#endif

# extern uintD* clear_loop_down (uintD* destptr, uintC count);
C(clear_loop_down:) # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sub %o0,4,%o0
1:        st %g0,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,1b
         _ sub %o0,4,%o0
2:      retl
       _ add %o0,4,%o0
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sllx %o1,2,%o1         # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[-count]
1:        subcc %o1,4,%o1       # Zähler erniedrigen, Pointer erniedrigen
          bne,pt %xcc,1b
         _ st %g0,[%o0+%o1]     # Digit 0 ablegen
2:      retl
       _ nop
#endif

# extern void or_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(or_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ nop
1:        ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          or %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
1:        ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          or %o4,%o3,%o3        # beide verknüpfen
          bne,pt %xcc,1b
         _ st %o3,[%o1+%o2]     # Digit ablegen
2:      retl
       _ nop
#endif

# extern void xor_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(xor_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ nop
1:        ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          xor %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
1:        ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          xor %o4,%o3,%o3       # beide verknüpfen
          bne,pt %xcc,1b
         _ st %o3,[%o1+%o2]     # Digit ablegen
2:      retl
       _ nop
#endif

# extern void and_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(and_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ nop
1:        ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          and %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
1:        ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          and %o4,%o3,%o3       # beide verknüpfen
          bne,pt %xcc,1b
         _ st %o3,[%o1+%o2]     # Digit ablegen
2:      retl
       _ nop
#endif

# extern void eqv_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(eqv_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ nop
1:        ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          xnor %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
1:        ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          xnor %o4,%o3,%o3      # beide verknüpfen
          bne,pt %xcc,1b
         _ st %o3,[%o1+%o2]     # Digit ablegen
2:      retl
       _ nop
#endif

# extern void nand_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(nand_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ nop
1:        ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          and %o3,%o4,%o3
          xor %o3,-1,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
1:        ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          and %o4,%o3,%o3       # beide verknüpfen
          xor %o3,-1,%o3
          bne,pt %xcc,1b
         _ st %o3,[%o1+%o2]     # Digit ablegen
2:      retl
       _ nop
#endif

# extern void nor_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(nor_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ nop
1:        ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          or %o3,%o4,%o3
          xor %o3,-1,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
1:        ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          or %o4,%o3,%o3        # beide verknüpfen
          xor %o3,-1,%o3
          bne,pt %xcc,1b
         _ st %o3,[%o1+%o2]     # Digit ablegen
2:      retl
       _ nop
#endif

# extern void andc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(andc2_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ nop
1:        ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          andn %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
1:        ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          andn %o4,%o3,%o3      # beide verknüpfen
          bne,pt %xcc,1b
         _ st %o3,[%o1+%o2]     # Digit ablegen
2:      retl
       _ nop
#endif

# extern void orc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(orc2_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ nop
1:        ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          orn %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %o0,4,%o0
        sub %g0,%o2,%o2         # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
1:        ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          orn %o4,%o3,%o3       # beide verknüpfen
          bne,pt %xcc,1b
         _ st %o3,[%o1+%o2]     # Digit ablegen
2:      retl
       _ nop
#endif

# extern void not_loop_up (uintD* xptr, uintC count);
C(not_loop_up:) # Input in %o0,%o1
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ nop
1:        ld [%o0],%o2
          subcc %o1,1,%o1
          xor %o2,-1,%o2
          st %o2,[%o0]
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ nop
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sub %o0,4,%o0
        sub %g0,%o1,%o1         # %o1 = -count
        sllx %o1,2,%o1          # %o1 = -4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[count-1]
1:        addcc %o1,4,%o1       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o1],%o2      # nächstes Digit holen
          xor %o2,-1,%o2
          bne,pt %xcc,1b
         _ st %o2,[%o0+%o1]     # Digit ablegen
2:      retl
       _ nop
#endif

# extern bool and_test_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(and_test_loop_up:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ nop
1:        ld [%o0],%o3
          ld [%o1],%o4
          add %o0,4,%o0
          andcc %o3,%o4,%g0
          bne,pn %icc,3f
         _ subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o1,4,%o1
2:      retl
       _ mov 0,%o0
3:      retl
       _ mov 1,%o0
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %g0,%o2,%o2        # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
          ld [%o0+%o2],%o3      # nächstes Digit holen
1:        ld [%o1+%o2],%o4      # noch ein Digit holen
          andcc %o3,%o4,%g0     # beide verknüpfen
          bne,pn %icc,3f
         _ addcc %o2,4,%o2      # Zähler "erniedrigen", Pointer erhöhen
          bne,a,pt %xcc,1b
         __ ld [%o0+%o2],%o3    # nächstes Digit holen
2:      retl
       _ mov 0,%o0
3:      retl
       _ mov 1,%o0
#endif

# extern bool test_loop_up (uintD* ptr, uintC count);
C(test_loop_up:) # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ nop
          lduw [%o0],%o2
1:        add %o0,4,%o0
          brnz,pn %o2,3f
         _ subcc %o1,1,%o1
          bne,a,pt %xcc,1b
         __ lduw [%o0],%o2
2:      retl
       _ mov 0,%o0
3:      retl
       _ mov 1,%o0
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sub %g0,%o1,%o1        # %o1 = -count
        sllx %o1,2,%o1          # %o1 = -4*count
        sub %o0,%o1,%o0         # %o0 = &ptr[count]
          lduw [%o0+%o1],%o2    # nächstes Digit holen
1:        brnz,pn %o2,3f        # testen
         _ addcc %o1,4,%o1      # Zähler "erniedrigen", Pointer erhöhen
          bne,a,pt %xcc,1b
         __ lduw [%o0+%o1],%o2  # nächstes Digit holen
2:      retl
       _ mov 0,%o0
3:      retl
       _ mov 1,%o0
#endif

# extern signean compare_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(compare_loop_up:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ nop
          ld [%o0],%o3
1:        ld [%o1],%o4
          add %o0,4,%o0
          subcc %o3,%o4,%g0
          bne,pn %icc,3f
         _ add %o1,4,%o1
          subcc %o2,1,%o2
          bne,a,pt %xcc,1b
         __ ld [%o0],%o3
2:      retl
       _ mov 0,%o0
3:      mov 1,%o0
        movlu %icc,-1,%o0
        retl
       _ sra %o0,0,%o0          # sign-extend %o0
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ sub %g0,%o2,%o2        # %o2 = -count
        sllx %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
          ld [%o0+%o2],%o3      # nächstes Digit holen
1:        ld [%o1+%o2],%o4      # noch ein Digit holen
          subcc %o3,%o4,%g0     # vergleichen
          bne,pn %icc,3f
         _ addcc %o2,4,%o2      # Zähler "erniedrigen", Pointer erhöhen
          bne,a,pt %xcc,1b
         __ ld [%o0+%o2],%o3    # nächstes Digit holen
2:      retl
       _ mov 0,%o0
3:      subcc %o3,%o4,%g0       # nochmals vergleichen
        mov 1,%o0
        movlu %icc,-1,%o0
        retl
       _ sra %o0,0,%o0          # sign-extend %o0
#endif

# extern uintD add_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
C(add_loop_down:) # Input in %o0,%o1,%o2,%o3, verändert %g1, Output in %o0
#if STANDARD_LOOPS
#       srl %o3,0,%o3           # zero-extend %o3 = count
        brz,pn %o3,2f
       _ mov %g0,%g1            # Carry := 0
1:        sub %o0,4,%o0
          lduw [%o0],%o4        # source1-digit, zero-extend
          sub %o1,4,%o1
          lduw [%o1],%o5        # source2-digit, zero-extend
          add %g1,%o4,%g1       # zum Carry addieren
          add %g1,%o5,%g1       # zum Carry addieren
          st %g1,[%o2]          # Digit ablegen
          subcc %o3,1,%o3
          bne,pt %xcc,1b
         _ srlx %g1,32,%g1      # neuer Carry
2:      retl
       _ mov %g1,%o0
#endif
#if COUNTER_LOOPS
#       srl %o3,0,%o3           # zero-extend %o3 = count
        brz,pn %o3,2f
       _ mov %g0,%g1            # Carry := 0
        sub %o0,4,%o0
        sub %o1,4,%o1
        sllx %o3,2,%o3          # %o3 = 4*count
        sub %o0,%o3,%o0         # %o0 = &sourceptr1[-count-1]
        sub %o1,%o3,%o1         # %o1 = &sourceptr2[-count-1]
        sub %o2,%o3,%o2         # %o2 = &destptr[-count]
1:        lduw [%o0+%o3],%o4    # source1-digit, zero-extend
          lduw [%o1+%o3],%o5    # source2-digit, zero-extend
          add %g1,%o4,%g1       # zum Carry addieren
          add %g1,%o5,%g1       # zum Carry addieren
          st %g1,[%o2+%o3]      # Digit ablegen
          subcc %o3,4,%o3
          bne,pt %xcc,1b
         _ srlx %g1,32,%g1      # neuer Carry
2:      retl
       _ mov %g1,%o0
#endif

# extern uintD addto_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
C(addto_loop_down:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ mov %g0,%o5            # Carry := 0
1:        sub %o0,4,%o0
          lduw [%o0],%o3        # source-digit, zero-extend
          sub %o1,4,%o1
          lduw [%o1],%o4        # dest-digit, zero-extend
          add %o5,%o3,%o5       # zum Carry addieren
          add %o5,%o4,%o5       # zum Carry addieren
          st %o5,[%o1]          # Digit ablegen
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ srlx %o5,32,%o5      # neuer Carry
2:      retl
       _ mov %o5,%o0
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ mov %g0,%o5            # Carry := 0
        sub %o0,4,%o0
        sub %o1,4,%o1
        sllx %o2,2,%o2          # %o2 = 4*count
        sub %o0,%o2,%o0         # %o0 = &sourceptr[-count-1]
        sub %o1,%o2,%o1         # %o1 = &destptr[-count-1]
1:        lduw [%o0+%o2],%o3    # source-digit, zero-extend
          lduw [%o1+%o2],%o4    # dest-digit, zero-extend
          add %o5,%o3,%o5       # zum Carry addieren
          add %o5,%o4,%o5       # zum Carry addieren
          st %o5,[%o1+%o2]      # Digit ablegen
          subcc %o2,4,%o2
          bne,pt %xcc,1b
         _ srlx %o5,32,%o5      # neuer Carry
2:      retl
       _ mov %o5,%o0
#endif

# extern uintD inc_loop_down (uintD* ptr, uintC count);
C(inc_loop_down:) # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sub %o0,4,%o0
1:        ld [%o0],%o2
          addcc %o2,1,%o2
          bne,pn %icc,3f
         _ st %o2,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,1b
         _ sub %o0,4,%o0
2:      retl
       _ mov 1,%o0
3:      retl
       _ mov 0,%o0
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sub %o0,4,%o0
        sllx %o1,2,%o1          # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &ptr[-count-1]
          ld [%o0+%o1],%o2      # digit holen
1:        addcc %o2,1,%o2       # incrementieren
          bne,pn %icc,3f
         _ st %o2,[%o0+%o1]     # ablegen
          subcc %o1,4,%o1       # Zähler erniedrigen, Pointer erniedrigen
          bne,a,pt %xcc,1b
         __ ld [%o0+%o1],%o2
2:      retl
       _ mov 1,%o0
3:      retl
       _ mov 0,%o0
#endif

# extern uintD sub_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
C(sub_loop_down:) # Input in %o0,%o1,%o2,%o3, verändert %g1, Output in %o0
#if STANDARD_LOOPS
#       srl %o3,0,%o3           # zero-extend %o3 = count
        brz,pn %o3,2f
       _ mov %g0,%g1            # Carry := 0
1:        sub %o0,4,%o0
          lduw [%o0],%o4        # source1-digit, zero-extend
          sub %o1,4,%o1
          lduw [%o1],%o5        # source2-digit, zero-extend
          add %g1,%o4,%g1       # zum Carry addieren
          sub %g1,%o5,%g1       # vom Carry subtrahieren
          st %g1,[%o2]          # Digit ablegen
          subcc %o3,1,%o3
          bne,pt %xcc,1b
         _ srax %g1,32,%g1      # neuer Carry
2:      retl
       _ srl %g1,0,%o0
#endif
#if COUNTER_LOOPS
#       srl %o3,0,%o3           # zero-extend %o3 = count
        brz,pn %o3,2f
       _ mov %g0,%g1            # Carry := 0
        sub %o0,4,%o0
        sub %o1,4,%o1
        sllx %o3,2,%o3          # %o3 = 4*count
        sub %o0,%o3,%o0         # %o0 = &sourceptr1[-count-1]
        sub %o1,%o3,%o1         # %o1 = &sourceptr2[-count-1]
        sub %o2,%o3,%o2         # %o2 = &destptr[-count]
1:        lduw [%o0+%o3],%o4    # source1-digit, zero-extend
          lduw [%o1+%o3],%o5    # source2-digit, zero-extend
          add %g1,%o4,%g1       # zum Carry addieren
          sub %g1,%o5,%g1       # vom Carry subtrahieren
          st %g1,[%o2+%o3]      # Digit ablegen
          subcc %o3,4,%o3
          bne,pt %xcc,1b
         _ srax %g1,32,%g1      # neuer Carry
2:      retl
       _ srl %g1,0,%o0
#endif

# extern uintD subx_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
C(subx_loop_down:) # Input in %o0,%o1,%o2,%o3,%o4, verändert %g1, Output in %o0
#if STANDARD_LOOPS
#       srl %o3,0,%o3           # zero-extend %o3 = count
        brz,pn %o3,2f
       _ sra %o4,0,%g1          # Carry, sign-extend
1:        sub %o0,4,%o0
          lduw [%o0],%o4        # source1-digit, zero-extend
          sub %o1,4,%o1
          lduw [%o1],%o5        # source2-digit, zero-extend
          add %g1,%o4,%g1       # zum Carry addieren
          sub %g1,%o5,%g1       # vom Carry subtrahieren
          st %g1,[%o2]          # Digit ablegen
          subcc %o3,1,%o3
          bne,pt %xcc,1b
         _ srax %g1,32,%g1      # neuer Carry
2:      retl
       _ srl %g1,0,%o0
#endif
#if COUNTER_LOOPS
#       srl %o3,0,%o3           # zero-extend %o3 = count
        brz,pn %o3,2f
       _ sra %o4,0,%g1          # Carry, sign-extend
        sub %o0,4,%o0
        sub %o1,4,%o1
        sllx %o3,2,%o3          # %o3 = 4*count
        sub %o0,%o3,%o0         # %o0 = &sourceptr1[-count-1]
        sub %o1,%o3,%o1         # %o1 = &sourceptr2[-count-1]
        sub %o2,%o3,%o2         # %o2 = &destptr[-count]
1:        lduw [%o0+%o3],%o4    # source1-digit, zero-extend
          lduw [%o1+%o3],%o5    # source2-digit, zero-extend
          add %g1,%o4,%g1       # zum Carry addieren
          sub %g1,%o5,%g1       # vom Carry subtrahieren
          st %g1,[%o2+%o3]      # Digit ablegen
          subcc %o3,4,%o3
          bne,pt %xcc,1b
         _ srax %g1,32,%g1      # neuer Carry
2:      retl
       _ srl %g1,0,%o0
#endif

# extern uintD subfrom_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
C(subfrom_loop_down:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ mov %g0,%o5            # Carry := 0
1:        sub %o0,4,%o0
          lduw [%o0],%o3        # source-digit, zero-extend
          sub %o1,4,%o1
          lduw [%o1],%o4        # dest-digit, zero-extend
          add %o5,%o3,%o5       # zum Carry addieren
          sub %o5,%o4,%o5       # vom Carry subtrahieren
          st %o5,[%o1]          # Digit ablegen
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ srax %o5,32,%o5      # neuer Carry
2:      retl
       _ srl %o5,0,%o0
#endif
#if COUNTER_LOOPS
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ mov %g0,%o5            # Carry := 0
        sub %o0,4,%o0
        sub %o1,4,%o1
        sllx %o2,2,%o2          # %o2 = 4*count
        sub %o0,%o2,%o0         # %o0 = &sourceptr[-count-1]
        sub %o1,%o2,%o1         # %o1 = &destptr[-count-1]
1:        lduw [%o0+%o2],%o3    # source-digit, zero-extend
          lduw [%o1+%o2],%o4    # dest-digit, zero-extend
          add %o5,%o3,%o5       # zum Carry addieren
          sub %o5,%o4,%o5       # vom Carry subtrahieren
          st %o5,[%o1+%o2]      # Digit ablegen
          subcc %o2,4,%o2
          bne,pt %xcc,1b
         _ srax %o5,32,%o5      # neuer Carry
2:      retl
       _ srl %o5,0,%o0
#endif

# extern uintD dec_loop_down (uintD* ptr, uintC count);
C(dec_loop_down:) # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sub %o0,4,%o0
1:        ld [%o0],%o2
          subcc %o2,1,%o2
          bcc,pn %icc,3f
         _ st %o2,[%o0]
          subcc %o1,1,%o1
          bne,pt %xcc,1b
         _ sub %o0,4,%o0
2:      retl
       _ mov -1,%o0
3:      retl
       _ mov 0,%o0
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sub %o0,4,%o0
        sllx %o1,2,%o1          # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &ptr[-count-1]
          ld [%o0+%o1],%o2      # digit holen
1:        subcc %o2,1,%o2       # decrementieren
          bcc,pn %icc,3f
         _ st %o2,[%o0+%o1]     # ablegen
          subcc %o1,4,%o1       # Zähler erniedrigen, Pointer erniedrigen
          bne,a,pt %xcc,1b
         __ ld [%o0+%o1],%o2
2:      retl
       _ mov -1,%o0
3:      retl
       _ mov 0,%o0
#endif

# extern uintD neg_loop_down (uintD* ptr, uintC count);
C(neg_loop_down:) # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        # erstes Digit /=0 suchen:
        brz,pn %o1,2f
       _ sub %o0,4,%o0
1:        ld [%o0],%o2
          subcc %g0,%o2,%o2
          bne,pn %icc,3f
         _ subcc %o1,1,%o1
          bne,pt %xcc,1b
         _ sub %o0,4,%o0
2:      retl
       _ mov 0,%o0
3:      # erstes Digit /=0 gefunden, ab jetzt gibt's Carrys
        st %o2,[%o0]            # 1 Digit negieren
        # alle anderen Digits invertieren:
        be,pn %xcc,5f
       _ sub %o0,4,%o0
4:        ld [%o0],%o2
          subcc %o1,1,%o1
          xor %o2,-1,%o2
          st %o2,[%o0]
          bne,pt %xcc,4b
         _ sub %o0,4,%o0
5:      mov -1,%o0
        retl
       _ srl %o0,0,%o0
#endif
#if COUNTER_LOOPS
#       srl %o1,0,%o1           # zero-extend %o1 = count
        # erstes Digit /=0 suchen:
        brz,pn %o1,2f
       _ sub %o0,4,%o0
        sllx %o1,2,%o1          # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &ptr[-count-1]
          ld [%o0+%o1],%o2      # digit holen
1:        subcc %g0,%o2,%o2     # negieren, testen
          bne,pn %icc,3f
         _ subcc %o1,4,%o1      # Zähler erniedrigen, Pointer erniedrigen
          bne,a,pt %xcc,1b
         __ ld [%o0+%o1],%o2
2:      retl
       _ mov 0,%o0
3:      # erstes Digit /=0 gefunden, ab jetzt gibt's Carrys
        # alle anderen Digits invertieren:
        add %o1,4,%o1
        st %o2,[%o0+%o1]        # ablegen
        subcc %o1,4,%o1
        be,pn %xcc,5f
       _ nop
          ld [%o0+%o1],%o2
4:        xor %o2,-1,%o2
          st %o2,[%o0+%o1]
          subcc %o1,4,%o1
          bne,a,pt %xcc,4b
         __ ld [%o0+%o1],%o2
5:      mov -1,%o0
        retl
       _ srl %o0,0,%o0
#endif

# extern uintD shift1left_loop_down (uintD* ptr, uintC count);
C(shift1left_loop_down:) # Input in %o0,%o1, Output in %o0
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ mov 0,%o3              # Carry := 0
1:        sub %o0,4,%o0
          lduw [%o0],%o2        # Digit
          subcc %o1,1,%o1
          add %o2,%o2,%o2       # shiften
          or %o3,%o2,%o3        # zum Carry addieren
          st %o3,[%o0]          # Digit ablegen
          bne,pt %xcc,1b
         _ srlx %o3,32,%o3      # neuer Carry
2:      retl
       _ mov %o3,%o0

# extern uintD shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry);
C(shiftleft_loop_down:) # Input in %o0,%o1,%o2,%o3, Output in %o0
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ srl %o3,0,%o3          # zero-extend carry
1:        sub %o0,4,%o0
          lduw [%o0],%o4        # Digit, zero-extend
          subcc %o1,1,%o1
          sllx %o4,%o2,%o4      # shiften
          or %o3,%o4,%o3        # zum Carry addieren
          st %o3,[%o0]          # Digit ablegen
          bne,pt %xcc,1b
         _ srlx %o3,32,%o3      # neuer Carry
2:      retl
       _ mov %o3,%o0

# extern uintD shiftleftcopy_loop_down (uintD* sourceptr, uintD* destptr, uintC count, uintC i);
C(shiftleftcopy_loop_down:) # Input in %o0,%o1,%o2,%o3, Output in %o0
#       srl %o2,0,%o2           # zero-extend %o2 = count
        brz,pn %o2,2f
       _ mov 0,%o4              # Carry := 0
1:        sub %o0,4,%o0
          lduw [%o0],%o5        # Digit, zero-extend
          subcc %o2,1,%o2
          sllx %o5,%o2,%o5      # shiften
          or %o4,%o5,%o4        # zum Carry addieren
          sub %o1,4,%o1
          st %o4,[%o1]          # Digit ablegen
          bne,pt %xcc,1b
         _ srlx %o4,32,%o4      # neuer Carry
2:      retl
       _ mov %o4,%o0

# extern uintD shift1right_loop_up (uintD* ptr, uintC count, uintD carry);
C(shift1right_loop_up:) # Input in %o0,%o1,%o2, Output in %o0
#ifdef SLOWER
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sllx %o2,63,%o2        # Carry
1:        lduw [%o0],%o3        # Digit, zero-extend
          subcc %o1,1,%o1
          sllx %o3,31,%o3       # shiften
          or %o2,%o3,%o2        # und mit altem Carry kombinieren
          srlx %o2,32,%o3
          st %o3,[%o0]          # und ablegen
          sllx %o2,32,%o2       # neuer Carry
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ srlx %o2,32,%o0
#else
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ sll %o2,31,%o2         # Carry
1:        ld [%o0],%o3          # Digit
          subcc %o1,1,%o1
          srl %o3,1,%o4         # shiften
          or %o2,%o4,%o4        # und mit altem Carry kombinieren
          st %o4,[%o0]          # und ablegen
          sll %o3,31,%o2        # neuer Carry
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ mov %o2,%o0
#endif

# extern uintD shiftright_loop_up (uintD* ptr, uintC count, uintC i);
C(shiftright_loop_up:) # Input in %o0,%o1,%o2, verändert %g1, Output in %o0
#ifdef SLOWER
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ or %g0,%g0,%o3         # Carry := 0
        mov 32,%g1
        sub %g1,%o2,%g1         # 32-i
1:        lduw [%o0],%o4        # Digit, zero-extend
          subcc %o1,1,%o1
          sllx %o4,%g1,%o4      # shiften
          or %o3,%o4,%o3        # und mit altem Carry kombinieren
          srlx %o3,32,%o4
          st %o4,[%o0]          # und ablegen
          sllx %o3,32,%o3       # neuer Carry
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ srlx %o3,32,%o0
#else
#       srl %o1,0,%o1           # zero-extend %o1 = count
        brz,pn %o1,2f
       _ or %g0,%g0,%o3         # Carry := 0
        sub %g0,%o2,%g1         # 32-i (mod 32)
1:        ld [%o0],%o4          # Digit
          subcc %o1,1,%o1
          srl %o4,%o2,%o5       # shiften
          or %o3,%o5,%o5        # und mit altem Carry kombinieren
          st %o5,[%o0]          # und ablegen
          sll %o4,%g1,%o3       # neuer Carry
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ mov %o3,%o0
#endif

# extern uintD shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i);
C(shiftrightsigned_loop_up:) # Input in %o0,%o1,%o2, verändert %g1, Output in %o0
#ifdef SLOWER
#       srl %o1,0,%o1           # zero-extend %o1 = count
        mov 32,%g1
        sub %g1,%o2,%g1         # 32-i
        ldsw [%o0],%o3          # erstes Digit, sign-extend
        subcc %o1,1,%o1
        sllx %o3,%g1,%o3        # shiften
        srlx %o3,32,%o4
        st %o4,[%o0]            # und ablegen
        sllx %o3,32,%o3         # neuer Carry
        be,pn %xcc,2f
       _ add %o0,4,%o0
1:        lduw [%o0],%o4        # Digit, zero-extend
          subcc %o1,1,%o1
          sllx %o4,%g1,%o4      # shiften
          or %o3,%o4,%o3        # und mit altem Carry kombinieren
          srlx %o3,32,%o4
          st %o4,[%o0]          # und ablegen
          sllx %o3,32,%o3       # neuer Carry
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ srlx %o3,32,%o0
#else
#       srl %o1,0,%o1           # zero-extend %o1 = count
        ld [%o0],%o4            # erstes Digit
        sub %g0,%o2,%g1         # 32-i (mod 32)
        sra %o4,%o2,%o5         # shiften
        st %o5,[%o0]            # und ablegen
        sll %o4,%g1,%o3         # neuer Carry
        subcc %o1,1,%o1
        be,pn %xcc,2f
       _ add %o0,4,%o0
1:        ld [%o0],%o4          # Digit
          subcc %o1,1,%o1
          srl %o4,%o2,%o5       # shiften
          or %o3,%o5,%o5        # und mit altem Carry kombinieren
          st %o5,[%o0]          # und ablegen
          sll %o4,%g1,%o3       # neuer Carry
          bne,pt %xcc,1b
         _ add %o0,4,%o0
2:      retl
       _ mov %o3,%o0
#endif

# extern uintD shiftrightcopy_loop_up (uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry);
C(shiftrightcopy_loop_up:) # Input in %o0,%o1,%o2,%o3,%o4, verändert %g1,%g2, Output in %o0
#ifdef SLOWER
#       srl %o2,0,%o2           # zero-extend %o2 = count
        sub %g0,%o3,%g1         # 64-i (mod 64)
        brz,pn %o2,2f
       _ sllx %o4,%g1,%o4       # erster Carry
        add %g1,32,%g1          # 32-i
1:        lduw [%o0],%o5        # Digit, zero-extend
          add %o0,4,%o0
          sllx %o5,%g1,%o5      # shiften
          or %o4,%o5,%o4        # und mit altem Carry kombinieren
          srlx %o4,32,%o5
          st %o5,[%o1]          # und ablegen
          sllx %o4,32,%o4       # neuer Carry
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o1,4,%o1
2:      retl
       _ srlx %o4,32,%o0
#else
#       srl %o2,0,%o2           # zero-extend %o2 = count
        sub %g0,%o3,%g1         # 32-i (mod 32)
        brz,pn %o2,2f
       _ sll %o4,%g1,%g2        # erster Carry
1:        ld [%o0],%o4          # Digit
          add %o0,4,%o0
          srl %o4,%o3,%o5       # shiften
          or %g2,%o5,%o5        # und mit altem Carry kombinieren
          st %o5,[%o1]          # und ablegen
          sll %o4,%g1,%g2       # neuer Carry
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o1,4,%o1
2:      retl
       _ mov %g2,%o0
#endif

# extern uintD mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit);
C(mulusmall_loop_down:) # Input in %o0,%o1,%o2,%o3, Output in %o0
#       srl %o2,0,%o2           # zero-extend %o2 = len
        brz,pn %o2,2f
       _ sub %o1,4,%o1
1:        # nächstes Digit [%o1] mit der 6-Bit-Zahl %o0 multiplizieren
          # und kleinen Carry %o3 dazu:
          mov %o0,%y
          ld [%o1],%o4          # Wartetakt!
          addcc %o3,%o3,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%o4,%o5
          mulscc %o5,%g0,%o5
          # Die 26 unteren Bits von %o5 und die 6 oberen Bits von %y
          # ergeben das Resultat. (Die anderen Bits sind Null.)
          sra %o5,26,%o3        # 6 obere Bits von %o5 -> neuer Carry
          tst %o4               # Korrektur, falls %o4 negativ war
          add %o3,%o0,%o4
          movl %icc,%o4,%o3     # (falls %o4 negativ war, noch + %o0)
          rd %y,%o4
          srl %o4,26,%o4        # 6 obere Bits von %y
          sll %o5,6,%o5         # 26 untere Bits von %o5
          or %o5,%o4,%o4        # neues Digit
          st %o4,[%o1]          # ablegen
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ sub %o1,4,%o1
2:      retl
       _ srl %o3,0,%o0

# extern void mulu_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
C(mulu_loop_down:) # Input in %o0,%o1,%o2,%o3, verändert %g1
#       srl %o3,0,%o3           # zero-extend %o3 = len
        mov 0,%o4               # Carry
1:        sub %o1,4,%o1
          ld [%o1],%g1          # nächstes Digit
          sub %o2,4,%o2
          # mit digit multiplizieren: %o0 * %g1 -> %o5|%g1
          umul %g1,%o0,%g1
          rd %y,%o5
          addcc %o4,%g1,%g1     # und bisherigen Carry addieren
          addx %g0,%o5,%o4      # High-Digit gibt neuen Carry
          subcc %o3,1,%o3
          bne,pt %xcc,1b
         _ st %g1,[%o2]         # Low-Digit ablegen
        retl
       _ st %o4,[%o2-4]         # letzten Carry ablegen

# extern uintD muluadd_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
C(muluadd_loop_down:) # Input in %o0,%o1,%o2,%o3, verändert %g1,%g2, Output in %o0
#       srl %o3,0,%o3           # zero-extend %o3 = len
        mov 0,%o4               # Carry
1:        sub %o1,4,%o1
          ld [%o1],%o5          # nächstes source-Digit
          sub %o2,4,%o2
          # mit digit multiplizieren: %o0 * %o5 -> %g2|%g1
          umul %o0,%o5,%g1
          rd %y,%g2
          ld [%o2],%o5          # nächstes dest-digit
          addcc %o4,%g1,%g1     # und bisherigen Carry addieren
          addx %g0,%g2,%o4      # High-Digit gibt neuen Carry
          addcc %o5,%g1,%g1     # addieren
          addx %g0,%o4,%o4
          subcc %o3,1,%o3
          bne,pt %xcc,1b
         _ st %g1,[%o2]         # Low-Digit ablegen
        retl
       _ srl %o4,0,%o0          # letzter Carry

# extern uintD mulusub_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
C(mulusub_loop_down:) # Input in %o0,%o1,%o2,%o3, verändert %g1,%g2, Output in %o0
#       srl %o3,0,%o3           # zero-extend %o3 = len
        mov 0,%o4               # Carry
1:        sub %o1,4,%o1
          ld [%o1],%o5          # nächstes source-Digit
          sub %o2,4,%o2
          # mit digit multiplizieren: %o0 * %o5 -> %g2|%g1
          umul %o0,%o5,%g1
          rd %y,%g2
          ld [%o2],%o5          # nächstes dest-digit
          addcc %o4,%g1,%g1     # und bisherigen Carry addieren
          addx %g0,%g2,%o4      # High-Digit gibt neuen Carry
          subcc %o5,%g1,%o5     # davon das Low-Digit subtrahieren
          addx %g0,%o4,%o4
          subcc %o3,1,%o3
          bne,pt %xcc,1b
         _ st %o5,[%o2]         # dest-Digit ablegen
        retl
       _ srl %o4,0,%o0          # letzter Carry

# extern uintD divu_loop_up (uintD digit, uintD* ptr, uintC len);
C(divu_loop_up:) # Input in %o0,%o1,%o2, verändert %g1, Output in %o0
#       srl %o2,0,%o2           # zero-extend %o2 = len
        brz,pn %o2,2f
       _ mov 0,%o3              # Rest
#       srl %o0,0,%o0           # zero-extend %o0 = digit
1:        lduw [%o1],%o4        # nächstes Digit
          sllx %o3,32,%o3       # Rest als High-Digit
          or %o3,%o4,%o3        # zusammen
          udivx %o3,%o0,%o4     # durch digit dividieren
          st %o4,[%o1]          # Quotient ablegen
          umul %o0,%o4,%g1
          sub %o3,%g1,%o3       # Rest in den unteren 32 Bit von %o3
          subcc %o2,1,%o2
          bne,pt %xcc,1b
         _ add %o1,4,%o1
2:      retl
       _ srl %o3,0,%o0          # Rest als Ergebnis

# extern uintD divucopy_loop_up (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
C(divucopy_loop_up:) # Input in %o0,%o1,%o2,%o3, verändert %g1, Output in %o0
#       srl %o3,0,%o3           # zero-extend %o3 = len
        brz,pn %o3,2f
       _ mov 0,%o4              # Rest
#       srl %o0,0,%o0           # zero-extend %o0 = digit
1:        lduw [%o1],%o5        # nächstes Digit
          add %o1,4,%o1
          sllx %o4,32,%o4       # Rest als High-Digit
          or %o4,%o5,%o4        # zusammen
          udivx %o4,%o0,%o5     # durch digit dividieren
          st %o5,[%o2]          # Quotient ablegen
          umul %o0,%o5,%g1
          sub %o4,%g1,%o4       # Rest in den unteren 32 Bit von %o4
          subcc %o3,1,%o3
          bne,pt %xcc,1b
         _ add %o2,4,%o2
2:      retl
       _ srl %o4,0,%o0          # Rest als Ergebnis

#endif

