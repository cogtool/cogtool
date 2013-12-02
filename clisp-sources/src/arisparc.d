# Externe Routinen zu ARILEV1.D
# Prozessor: SPARC
# Compiler: GNU-C oder SUN-C
# Parameter-Übergabe: in Registern %o0-%o5.
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

#if defined(sparc_v8) || defined(__sparc_v8) || defined(__sparc_v8__)
  #define sparcv8
#endif

#ifdef ASM_UNDERSCORE /* SunOS 4 */
  #define C(entrypoint) _##entrypoint
#else /* SunOS 5 = Solaris 2 */
  #define C(entrypoint) entrypoint
#endif

  # Indikatoren für Anweisungen (Instruktionen) in Delay-Slots
  # (diese werden VOR der vorigen Instruktion ausgeführt):
  #define _             # Instruktion, die stets ausgeführt wird
  #define __            # Instruktion, die nur im Sprung-Fall ausgeführt wird
  # Abkürzungen für Anweisungen:
  #define ret   jmp %i7+8    # return from subroutine
  #define retl  jmp %o7+8    # return from leaf subroutine (no save/restore)

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
                        # 3: entrollte Schleifen
#define STANDARD_LOOPS  (LOOP_TYPE==1)
#define COUNTER_LOOPS  (LOOP_TYPE==2)
#define UNROLLED_LOOPS  (LOOP_TYPE==3)
#define MULU32_INLINE  1  # 1: mulu32-Aufrufe inline in die Schleifen

# extern uint32 mulu16_ (uint16 arg1, uint16 arg2);
# ergebnis := arg1*arg2.
C(mulu16_:) # Input in %o0,%o1, Output in %o0
#ifdef sparcv8
        umul    %o0,%o1,%o0
        retl
       _ nop
#else
        mov     %o1,%y
        nop                     # Wartetakt, nötig z.B. für SUN SPARCstation IPC
        andcc   %g0,%g0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        # Die 17 unteren Bits von %o2 und die 15 oberen Bits von %y
        # ergeben das Resultat. (Die anderen Bits sind Null.)
        rd      %y,%o0
        srl     %o0,17,%o0
        sll     %o2,15,%o2
        retl
       _ or      %o2,%o0,%o0
#endif

# extern struct { uint32 lo; uint32 hi; } mulu32_ (uint32 arg1, uint32 arg2);
# 2^32*hi+lo := arg1*arg2.
C(mulu32_:) # Input in %o0,%o1, Output in %o0,%g1
#ifdef sparcv8
        umul    %o0,%o1,%o0
        retl
       _ rd      %y,%g1
#else
        mov     %o1,%y
        sra     %o0,31,%o3      # Wartetakt, nötig z.B. für SUN SPARCstation IPC
        andcc   %g0,%g0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%g0,%o2
        and     %o3,%o1,%o3     # %o3 = (0 falls %o0>=0, %o1 falls %o0<0)
        add     %o2,%o3,%g1     # hi
        retl
       _ rd      %y,%o0         # lo
#endif

# extern uint32 mulu32_unchecked (uint32 x, uint32 y);
# ergebnis := arg1*arg2 < 2^32.
C(mulu32_unchecked:) # Input in %o0,%o1, Output in %o0
#ifdef sparcv8
        umul    %o0,%o1,%o0
        retl
       _ nop
#else
        subcc   %o0,%o1,%g0
        bcc,a   Ll01
       __ mov     %o1,%y
        # arg1 < arg2, also kann man arg1 < 2^16 annehmen.
        mov     %o0,%y
        nop                     # Wartetakt, nötig z.B. für SUN SPARCstation IPC
        andcc   %g0,%g0,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        mulscc  %o2,%o1,%o2
        # Die 17 unteren Bits von %o2 und die 15 oberen Bits von %y
        # ergeben das Resultat. (Die anderen Bits sind Null.)
        rd      %y,%o0
        srl     %o0,17,%o0
        sll     %o2,15,%o2
        retl
       _ or      %o2,%o0,%o0
Ll01:   # arg1 >= arg2, also kann man arg2 < 2^16 annehmen.
        nop                     # Wartetakt, nötig z.B. für SUN SPARCstation IPC
        andcc   %g0,%g0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        mulscc  %o2,%o0,%o2
        # Die 17 unteren Bits von %o2 und die 15 oberen Bits von %y
        # ergeben das Resultat.
        rd      %y,%o0
        srl     %o0,17,%o0
        sll     %o2,15,%o2
        retl
       _ or      %o2,%o0,%o0
#endif

# extern struct { uint32 q; uint32 r; } divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
# x = 2^32*xhi+xlo = q*y+r schreiben. Sei bekannt, dass 0 <= x < 2^32*y .
C(divu_6432_3232_:) # Input in %o0,%o1,%o2, Output in %o0,%g1
#if defined(sparcv8)
        # Problem: Is udiv worth using (gmp-2.0.2 doesn't use it) ??
        wr      %o0,%g0,%y
        nop                     # wait 1 | Necessary for certain sparcv8
        nop                     # wait 2 | processors such as Ross Hypersparc,
        nop                     # wait 3 | but not for most of the others.
        udiv    %o1,%o2,%o0     # x durch y dividieren, %o0 := q
        umul    %o0,%o2,%g1     # %g1 := (q*y) mod 2^32
        retl
       _ sub     %o1,%g1,%g1    # %g1 := (xlo-q*y) mod 2^32 = r
#else
        # %o0 = xhi, %o1 = xlo, %o2 = y
# Divisions-Einzelschritte:
# %o0|%o1  wird jeweils um 1 Bit nach links geschoben,
# dafür wird rechts in %o1 ein Ergebnisbit (negiert!) reingeschoben.
# Je nachdem wird mit %o3|%o1 statt %o0|%o1 weitergemacht (spart 1 'mov').
# Deswegen muss man den Code doppelt vorsehen: einmal mit %o0, einmal mit %o3.
#define SA0(label) # Vergleichsschritt mit %o0 \
        subcc   %o0,%o2,%o3; \
        bcc     label;       \
       _ addxcc  %o1,%o1,%o1
#define SA1(label) # Vergleichsschritt mit %o3 \
        subcc   %o3,%o2,%o0; \
        bcc     label;       \
       _ addxcc  %o1,%o1,%o1
#define SB0() # Additionsschritt mit %o0 \
        addx    %o0,%o0,%o0
#define SB1() # Additionsschritt mit %o3 \
        addx    %o3,%o3,%o3
# Los geht's:
        addcc   %o2,%o2,%g0     # y = %o2 < 2^31 ?
        bcc     Lsmalldiv       # ja -> "kleine" Division
       _ andcc   %o2,1,%g0      # y = %o2 gerade ?
        be      Levendiv        # ja -> Division durch gerade Zahl
       _ srl     %o2,1,%o2
        # Division durch ungerade Zahl:
        # floor(x / (2*y'-1)) = floor(floor(x/2) / y') + (0 oder 1 oder 2)
        # da  0 <= x/(2*y'-1) - x/(2*y') = x/(2*y'-1) / (2*y') = x/y / (2*y')
        #       < 2^32 / (2*y') < 2^32/y <= 2 .
        add     %o2,1,%o2       # %o2 = ceiling(y/2) = y'
        # Man spart im Vergleich zu Lsmalldiv
        # zu Beginn eine Verdoppelung von %o0|%o1 : addcc %o1,%o1,%o1; SB0()
        # dafür am Schluss mehr zu tun...
        SA0(Lb01)               # Bit 31 des Quotienten bestimmen
La01:   SB0(); SA0(Lb02)        # Bit 30 des Quotienten bestimmen
La02:   SB0(); SA0(Lb03)        # Bit 29 des Quotienten bestimmen
La03:   SB0(); SA0(Lb04)        # Bit 28 des Quotienten bestimmen
La04:   SB0(); SA0(Lb05)        # Bit 27 des Quotienten bestimmen
La05:   SB0(); SA0(Lb06)        # Bit 26 des Quotienten bestimmen
La06:   SB0(); SA0(Lb07)        # Bit 25 des Quotienten bestimmen
La07:   SB0(); SA0(Lb08)        # Bit 24 des Quotienten bestimmen
La08:   SB0(); SA0(Lb09)        # Bit 23 des Quotienten bestimmen
La09:   SB0(); SA0(Lb10)        # Bit 22 des Quotienten bestimmen
La10:   SB0(); SA0(Lb11)        # Bit 21 des Quotienten bestimmen
La11:   SB0(); SA0(Lb12)        # Bit 20 des Quotienten bestimmen
La12:   SB0(); SA0(Lb13)        # Bit 19 des Quotienten bestimmen
La13:   SB0(); SA0(Lb14)        # Bit 18 des Quotienten bestimmen
La14:   SB0(); SA0(Lb15)        # Bit 17 des Quotienten bestimmen
La15:   SB0(); SA0(Lb16)        # Bit 16 des Quotienten bestimmen
La16:   SB0(); SA0(Lb17)        # Bit 15 des Quotienten bestimmen
La17:   SB0(); SA0(Lb18)        # Bit 14 des Quotienten bestimmen
La18:   SB0(); SA0(Lb19)        # Bit 13 des Quotienten bestimmen
La19:   SB0(); SA0(Lb20)        # Bit 12 des Quotienten bestimmen
La20:   SB0(); SA0(Lb21)        # Bit 11 des Quotienten bestimmen
La21:   SB0(); SA0(Lb22)        # Bit 10 des Quotienten bestimmen
La22:   SB0(); SA0(Lb23)        # Bit 9 des Quotienten bestimmen
La23:   SB0(); SA0(Lb24)        # Bit 8 des Quotienten bestimmen
La24:   SB0(); SA0(Lb25)        # Bit 7 des Quotienten bestimmen
La25:   SB0(); SA0(Lb26)        # Bit 6 des Quotienten bestimmen
La26:   SB0(); SA0(Lb27)        # Bit 5 des Quotienten bestimmen
La27:   SB0(); SA0(Lb28)        # Bit 4 des Quotienten bestimmen
La28:   SB0(); SA0(Lb29)        # Bit 3 des Quotienten bestimmen
La29:   SB0(); SA0(Lb30)        # Bit 2 des Quotienten bestimmen
La30:   SB0(); SA0(Lb31)        # Bit 1 des Quotienten bestimmen
La31:   SB0(); SA0(Lb32)        # Bit 0 des Quotienten bestimmen
La32:   SB0()                   # %o0 = x mod (2*y')
        xor     %o1,-1,%o1      # %o1 = floor( floor(x/2) / y') = floor(x/(2*y'))
        add     %o2,%o2,%o2
        sub     %o2,1,%o2       # wieder %o2 = 2*y'-1 = y
        # Quotient und Rest umrechnen:
        # x = %o1 * 2*y' + %o0 = %o1 * (2*y'-1) + (%o0+%o1)
        # Also Quotient = %o1, Rest = %o0+%o1.
        # Noch maximal 2 mal: Quotient += 1, Rest -= y.
        addcc   %o1,%o0,%o0     # Rest mod y bestimmen
        bcc     Ll02            # Additions-Überlauf -> Quotient erhöhen
       _ subcc   %o0,%o2,%o3
        subcc   %o3,%o2,%o0     # muss der Quotient nochmals erhöht werden?
        bcs     Ll03
       _ mov     %o3,%g1
        # Quotient 2 mal erhöhen, Rest %o0
        mov     %o0,%g1
        retl
       _ add     %o1,2,%o0
Ll02:   # kein Additions-Überlauf.
        # Wegen y>=2^31 muss der Quotient noch höchstens 1 mal erhöht werden:
        bcs     Ll04            # %o0 < %o2 -> Rest %o0 und Quotient %o1 OK
       _ mov     %o3,%g1
Ll03:   # Quotient %o1 erhöhen, Rest = %o0-%o2 = %o3
        retl
       _ add     %o1,1,%o0
Ll04:   # Quotient %o1 und Rest %o0 OK
        mov     %o0,%g1
        retl
       _ mov     %o1,%o0
# Parallelschiene zu La01..La32:
Lb01:   SB1(); SA1(La02)
Lb02:   SB1(); SA1(La03)
Lb03:   SB1(); SA1(La04)
Lb04:   SB1(); SA1(La05)
Lb05:   SB1(); SA1(La06)
Lb06:   SB1(); SA1(La07)
Lb07:   SB1(); SA1(La08)
Lb08:   SB1(); SA1(La09)
Lb09:   SB1(); SA1(La10)
Lb10:   SB1(); SA1(La11)
Lb11:   SB1(); SA1(La12)
Lb12:   SB1(); SA1(La13)
Lb13:   SB1(); SA1(La14)
Lb14:   SB1(); SA1(La15)
Lb15:   SB1(); SA1(La16)
Lb16:   SB1(); SA1(La17)
Lb17:   SB1(); SA1(La18)
Lb18:   SB1(); SA1(La19)
Lb19:   SB1(); SA1(La20)
Lb20:   SB1(); SA1(La21)
Lb21:   SB1(); SA1(La22)
Lb22:   SB1(); SA1(La23)
Lb23:   SB1(); SA1(La24)
Lb24:   SB1(); SA1(La25)
Lb25:   SB1(); SA1(La26)
Lb26:   SB1(); SA1(La27)
Lb27:   SB1(); SA1(La28)
Lb28:   SB1(); SA1(La29)
Lb29:   SB1(); SA1(La30)
Lb30:   SB1(); SA1(La31)
Lb31:   SB1(); SA1(La32)
Lb32:   SB1()                   # %o3 = x mod (2*y')
        xor     %o1,-1,%o1      # %o1 = floor( floor(x/2) / y') = floor(x/(2*y'))
        add     %o2,%o2,%o2
        sub     %o2,1,%o2       # wieder %o2 = 2*y'-1 = y
        # Quotient und Rest umrechnen:
        # x = %o1 * 2*y' + %o3 = %o1 * (2*y'-1) + (%o3+%o1)
        # Also Quotient = %o1, Rest = %o3+%o1.
        # Noch maximal 2 mal: Quotient += 1, Rest -= y.
        addcc   %o1,%o3,%o3     # Rest mod y bestimmen
        bcc     Ll05            # Additions-Überlauf -> Quotient erhöhen
       _ subcc   %o3,%o2,%o0
        subcc   %o0,%o2,%o3     # muss der Quotient nochmals erhöht werden?
        bcs     Ll06
       _ mov     %o0,%g1
        # Quotient 2 mal erhöhen, Rest %o3
        mov     %o3,%g1
        retl
       _ add     %o1,2,%o0
Ll05:   # kein Additions-Überlauf.
        # Wegen y>=2^31 muss der Quotient noch höchstens 1 mal erhöht werden:
        bcs     Ll07            # %o3 < %o2 -> Rest %o3 und Quotient %o1 OK
       _ mov     %o0,%g1
Ll06:   # Quotient %o1 erhöhen, Rest = %o3-%o2 = %o0
        retl
       _ add     %o1,1,%o0
Ll07:   # Quotient %o1 und Rest %o3 OK
        mov     %o3,%g1
        retl
       _ mov     %o1,%o0
Lsmalldiv: # Division durch y < 2^31
        addcc   %o1,%o1,%o1
Lc00:   SB0(); SA0(Ld01)        # Bit 31 des Quotienten bestimmen
Lc01:   SB0(); SA0(Ld02)        # Bit 30 des Quotienten bestimmen
Lc02:   SB0(); SA0(Ld03)        # Bit 29 des Quotienten bestimmen
Lc03:   SB0(); SA0(Ld04)        # Bit 28 des Quotienten bestimmen
Lc04:   SB0(); SA0(Ld05)        # Bit 27 des Quotienten bestimmen
Lc05:   SB0(); SA0(Ld06)        # Bit 26 des Quotienten bestimmen
Lc06:   SB0(); SA0(Ld07)        # Bit 25 des Quotienten bestimmen
Lc07:   SB0(); SA0(Ld08)        # Bit 24 des Quotienten bestimmen
Lc08:   SB0(); SA0(Ld09)        # Bit 23 des Quotienten bestimmen
Lc09:   SB0(); SA0(Ld10)        # Bit 22 des Quotienten bestimmen
Lc10:   SB0(); SA0(Ld11)        # Bit 21 des Quotienten bestimmen
Lc11:   SB0(); SA0(Ld12)        # Bit 20 des Quotienten bestimmen
Lc12:   SB0(); SA0(Ld13)        # Bit 19 des Quotienten bestimmen
Lc13:   SB0(); SA0(Ld14)        # Bit 18 des Quotienten bestimmen
Lc14:   SB0(); SA0(Ld15)        # Bit 17 des Quotienten bestimmen
Lc15:   SB0(); SA0(Ld16)        # Bit 16 des Quotienten bestimmen
Lc16:   SB0(); SA0(Ld17)        # Bit 15 des Quotienten bestimmen
Lc17:   SB0(); SA0(Ld18)        # Bit 14 des Quotienten bestimmen
Lc18:   SB0(); SA0(Ld19)        # Bit 13 des Quotienten bestimmen
Lc19:   SB0(); SA0(Ld20)        # Bit 12 des Quotienten bestimmen
Lc20:   SB0(); SA0(Ld21)        # Bit 11 des Quotienten bestimmen
Lc21:   SB0(); SA0(Ld22)        # Bit 10 des Quotienten bestimmen
Lc22:   SB0(); SA0(Ld23)        # Bit 9 des Quotienten bestimmen
Lc23:   SB0(); SA0(Ld24)        # Bit 8 des Quotienten bestimmen
Lc24:   SB0(); SA0(Ld25)        # Bit 7 des Quotienten bestimmen
Lc25:   SB0(); SA0(Ld26)        # Bit 6 des Quotienten bestimmen
Lc26:   SB0(); SA0(Ld27)        # Bit 5 des Quotienten bestimmen
Lc27:   SB0(); SA0(Ld28)        # Bit 4 des Quotienten bestimmen
Lc28:   SB0(); SA0(Ld29)        # Bit 3 des Quotienten bestimmen
Lc29:   SB0(); SA0(Ld30)        # Bit 2 des Quotienten bestimmen
Lc30:   SB0(); SA0(Ld31)        # Bit 1 des Quotienten bestimmen
Lc31:   SB0(); SA0(Ld32)        # Bit 0 des Quotienten bestimmen
Lc32:   mov     %o0,%g1         # Rest aus %o0 in %g1 abspeichern
        retl
       _ xor     %o1,-1,%o0     # Quotient nach %o0
# Parallelschiene zu Lc01..Lc32:
Ld01:   SB1(); SA1(Lc02)
Ld02:   SB1(); SA1(Lc03)
Ld03:   SB1(); SA1(Lc04)
Ld04:   SB1(); SA1(Lc05)
Ld05:   SB1(); SA1(Lc06)
Ld06:   SB1(); SA1(Lc07)
Ld07:   SB1(); SA1(Lc08)
Ld08:   SB1(); SA1(Lc09)
Ld09:   SB1(); SA1(Lc10)
Ld10:   SB1(); SA1(Lc11)
Ld11:   SB1(); SA1(Lc12)
Ld12:   SB1(); SA1(Lc13)
Ld13:   SB1(); SA1(Lc14)
Ld14:   SB1(); SA1(Lc15)
Ld15:   SB1(); SA1(Lc16)
Ld16:   SB1(); SA1(Lc17)
Ld17:   SB1(); SA1(Lc18)
Ld18:   SB1(); SA1(Lc19)
Ld19:   SB1(); SA1(Lc20)
Ld20:   SB1(); SA1(Lc21)
Ld21:   SB1(); SA1(Lc22)
Ld22:   SB1(); SA1(Lc23)
Ld23:   SB1(); SA1(Lc24)
Ld24:   SB1(); SA1(Lc25)
Ld25:   SB1(); SA1(Lc26)
Ld26:   SB1(); SA1(Lc27)
Ld27:   SB1(); SA1(Lc28)
Ld28:   SB1(); SA1(Lc29)
Ld29:   SB1(); SA1(Lc30)
Ld30:   SB1(); SA1(Lc31)
Ld31:   SB1(); SA1(Lc32)
Ld32:   mov     %o3,%g1         # Rest aus %o3 in %g1 abspeichern
        retl
       _ xor     %o1,-1,%o0     # Quotient nach %o0
Levendiv: # Division durch gerades y.
        # x/2 durch y/2 dividieren, Quotient OK, Rest evtl. mit 2 multiplizieren.
        # Es ist schon %o2 = y/2.
        # Man spart im Vergleich zu Lsmalldiv
        # zu Beginn eine Verdoppelung von %o0|%o1 : addcc %o1,%o1,%o1; SB0()
        # dafür am Schluss Bit 0 von x zum Rest dazuschieben.
        SA0(Lf01)               # Bit 31 des Quotienten bestimmen
Le01:   SB0(); SA0(Lf02)        # Bit 30 des Quotienten bestimmen
Le02:   SB0(); SA0(Lf03)        # Bit 29 des Quotienten bestimmen
Le03:   SB0(); SA0(Lf04)        # Bit 28 des Quotienten bestimmen
Le04:   SB0(); SA0(Lf05)        # Bit 27 des Quotienten bestimmen
Le05:   SB0(); SA0(Lf06)        # Bit 26 des Quotienten bestimmen
Le06:   SB0(); SA0(Lf07)        # Bit 25 des Quotienten bestimmen
Le07:   SB0(); SA0(Lf08)        # Bit 24 des Quotienten bestimmen
Le08:   SB0(); SA0(Lf09)        # Bit 23 des Quotienten bestimmen
Le09:   SB0(); SA0(Lf10)        # Bit 22 des Quotienten bestimmen
Le10:   SB0(); SA0(Lf11)        # Bit 21 des Quotienten bestimmen
Le11:   SB0(); SA0(Lf12)        # Bit 20 des Quotienten bestimmen
Le12:   SB0(); SA0(Lf13)        # Bit 19 des Quotienten bestimmen
Le13:   SB0(); SA0(Lf14)        # Bit 18 des Quotienten bestimmen
Le14:   SB0(); SA0(Lf15)        # Bit 17 des Quotienten bestimmen
Le15:   SB0(); SA0(Lf16)        # Bit 16 des Quotienten bestimmen
Le16:   SB0(); SA0(Lf17)        # Bit 15 des Quotienten bestimmen
Le17:   SB0(); SA0(Lf18)        # Bit 14 des Quotienten bestimmen
Le18:   SB0(); SA0(Lf19)        # Bit 13 des Quotienten bestimmen
Le19:   SB0(); SA0(Lf20)        # Bit 12 des Quotienten bestimmen
Le20:   SB0(); SA0(Lf21)        # Bit 11 des Quotienten bestimmen
Le21:   SB0(); SA0(Lf22)        # Bit 10 des Quotienten bestimmen
Le22:   SB0(); SA0(Lf23)        # Bit 9 des Quotienten bestimmen
Le23:   SB0(); SA0(Lf24)        # Bit 8 des Quotienten bestimmen
Le24:   SB0(); SA0(Lf25)        # Bit 7 des Quotienten bestimmen
Le25:   SB0(); SA0(Lf26)        # Bit 6 des Quotienten bestimmen
Le26:   SB0(); SA0(Lf27)        # Bit 5 des Quotienten bestimmen
Le27:   SB0(); SA0(Lf28)        # Bit 4 des Quotienten bestimmen
Le28:   SB0(); SA0(Lf29)        # Bit 3 des Quotienten bestimmen
Le29:   SB0(); SA0(Lf30)        # Bit 2 des Quotienten bestimmen
Le30:   SB0(); SA0(Lf31)        # Bit 1 des Quotienten bestimmen
Le31:   SB0(); SA0(Lf32)        # Bit 0 des Quotienten bestimmen
Le32:   SB0()                   # Bit 0 des Restes bestimmen
        mov     %o0,%g1         # Rest aus %o0 in %g1 abspeichern
        retl
       _ xor     %o1,-1,%o0     # Quotient nach %o0
# Parallelschiene zu Le01..Le32:
Lf01:   SB1(); SA1(Le02)
Lf02:   SB1(); SA1(Le03)
Lf03:   SB1(); SA1(Le04)
Lf04:   SB1(); SA1(Le05)
Lf05:   SB1(); SA1(Le06)
Lf06:   SB1(); SA1(Le07)
Lf07:   SB1(); SA1(Le08)
Lf08:   SB1(); SA1(Le09)
Lf09:   SB1(); SA1(Le10)
Lf10:   SB1(); SA1(Le11)
Lf11:   SB1(); SA1(Le12)
Lf12:   SB1(); SA1(Le13)
Lf13:   SB1(); SA1(Le14)
Lf14:   SB1(); SA1(Le15)
Lf15:   SB1(); SA1(Le16)
Lf16:   SB1(); SA1(Le17)
Lf17:   SB1(); SA1(Le18)
Lf18:   SB1(); SA1(Le19)
Lf19:   SB1(); SA1(Le20)
Lf20:   SB1(); SA1(Le21)
Lf21:   SB1(); SA1(Le22)
Lf22:   SB1(); SA1(Le23)
Lf23:   SB1(); SA1(Le24)
Lf24:   SB1(); SA1(Le25)
Lf25:   SB1(); SA1(Le26)
Lf26:   SB1(); SA1(Le27)
Lf27:   SB1(); SA1(Le28)
Lf28:   SB1(); SA1(Le29)
Lf29:   SB1(); SA1(Le30)
Lf30:   SB1(); SA1(Le31)
Lf31:   SB1(); SA1(Le32)
Lf32:   SB1()
        mov     %o3,%g1         # Rest aus %o0 in %g1 abspeichern
        retl
       _ xor     %o1,-1,%o0     # Quotient nach %o0
#endif

# extern struct { uint16 q; uint16 r; } divu_3216_1616_ (uint32 x, uint16 y);
# x = q*y+r schreiben. Sei bekannt, dass 0 <= x < 2^16*y .
C(divu_3216_1616_:) # Input in %o0,%o1, Output in %o0 (Rest und Quotient).
#if defined(sparcv8)
        # Problem: Is udiv worth using (gmp-2.0.2 doesn't use it) ??
        wr      %g0,%g0,%y
        nop                     # wait 1
        nop                     # wait 2
        nop                     # wait 3
        udiv    %o0,%o1,%o0     # dividieren, Quotient nach %o0
        rd      %y,%o1          # Rest aus %y
        sll     %o1,16,%o1      # in die oberen 16 Bit schieben
        retl
       _ or      %o0,%o1,%o0
#else
        # %o0 = x, %o1 = y
# Divisions-Einzelschritte:
# %o0  wird jeweils um 1 Bit nach links geschoben,
# dafür wird rechts in %o1 ein Ergebnisbit (negiert!) reingeschoben.
# Dann wird auf >= 2^15*y verglichen (nicht auf >= 2^16*y, weil man dann das
# links herausgeschobene Bit mit vergleichen müsste!)
        sll %o1,16,%o1
        srl %o1,1,%o1           # 2^15*y
        sub %g0,%o1,%o2         # zum Addieren statt Subtrahieren: -2^15*y
        # SC0(label) subtrahiert y, schiebt Carry-Bit rechts in %o0 rein
        # (1 falls Subtraktion aufging, 0 sonst).
        # Ging die Subtraktion nicht auf, so müsste man noch 2*y addieren.
        # Das fasst man mit der nächsten Operation zusammen, indem man - statt
        # y zu subtrahieren - y addiert:
        # SC1(label) addiert y, schiebt Carry-Bit rechts in %o0 rein
        # (1 falls Subtraktion aufgegangen wäre, man also wieder im
        # "positiven Bereich" landet, 0 sonst).
#define SC0(label) \
        addcc   %o0,%o2,%o0; \
        bcc     label;       \
       _ addx    %o0,%o0,%o0
#define SC1(label) \
        addcc   %o0,%o1,%o0; \
        bcs     label;       \
       _ addx    %o0,%o0,%o0
        SC0(Lh01)               # Bit 15 des Quotienten bestimmen
Lg01:   SC0(Lh02)               # Bit 14 des Quotienten bestimmen
Lg02:   SC0(Lh03)               # Bit 13 des Quotienten bestimmen
Lg03:   SC0(Lh04)               # Bit 12 des Quotienten bestimmen
Lg04:   SC0(Lh05)               # Bit 11 des Quotienten bestimmen
Lg05:   SC0(Lh06)               # Bit 10 des Quotienten bestimmen
Lg06:   SC0(Lh07)               # Bit 9 des Quotienten bestimmen
Lg07:   SC0(Lh08)               # Bit 8 des Quotienten bestimmen
Lg08:   SC0(Lh09)               # Bit 7 des Quotienten bestimmen
Lg09:   SC0(Lh10)               # Bit 6 des Quotienten bestimmen
Lg10:   SC0(Lh11)               # Bit 5 des Quotienten bestimmen
Lg11:   SC0(Lh12)               # Bit 4 des Quotienten bestimmen
Lg12:   SC0(Lh13)               # Bit 3 des Quotienten bestimmen
Lg13:   SC0(Lh14)               # Bit 2 des Quotienten bestimmen
Lg14:   SC0(Lh15)               # Bit 1 des Quotienten bestimmen
Lg15:   SC0(Lh16)               # Bit 0 des Quotienten bestimmen
Lg16:   # Die oberen 16 Bit von %o0 sind der Rest,
        # die unteren 16 Bit von %o0 sind der Quotient.
        retl
       _ nop
Lh01:   SC1(Lg02)               # Bit 14 des Quotienten bestimmen
Lh02:   SC1(Lg03)               # Bit 13 des Quotienten bestimmen
Lh03:   SC1(Lg04)               # Bit 12 des Quotienten bestimmen
Lh04:   SC1(Lg05)               # Bit 11 des Quotienten bestimmen
Lh05:   SC1(Lg06)               # Bit 10 des Quotienten bestimmen
Lh06:   SC1(Lg07)               # Bit 9 des Quotienten bestimmen
Lh07:   SC1(Lg08)               # Bit 8 des Quotienten bestimmen
Lh08:   SC1(Lg09)               # Bit 7 des Quotienten bestimmen
Lh09:   SC1(Lg10)               # Bit 6 des Quotienten bestimmen
Lh10:   SC1(Lg11)               # Bit 5 des Quotienten bestimmen
Lh11:   SC1(Lg12)               # Bit 4 des Quotienten bestimmen
Lh12:   SC1(Lg13)               # Bit 3 des Quotienten bestimmen
Lh13:   SC1(Lg14)               # Bit 2 des Quotienten bestimmen
Lh14:   SC1(Lg15)               # Bit 1 des Quotienten bestimmen
Lh15:   SC1(Lg16)               # Bit 0 des Quotienten bestimmen
Lh16:   # Noch 2*y addieren:
        add %o0,%o1,%o0
        retl
       _ add %o0,%o1,%o0
#endif

# extern uintD* copy_loop_up (uintD* sourceptr, uintD* destptr, uintC count);
C(copy_loop_up:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll09
       _ nop
Ll08:     ld [%o0],%o3
          add %o0,4,%o0
          st %o3,[%o1]
          subcc %o2,1,%o2
          bne Ll08
         _ add %o1,4,%o1
Ll09:   retl
       _ mov %o1,%o0
#endif
#if COUNTER_LOOPS
        subcc %g0,%o2,%o2       # %o2 = -count
        be Ll09
       _ sub %o1,4,%o1
        sll %o2,2,%o2           # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &sourceptr[count]
        sub %o1,%o2,%o1         # %o1 = &destptr[count-1]
Ll08:     ld [%o0+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          bne Ll08
         _ st %o3,[%o1+%o2]     # Digit ablegen
Ll09:   retl
       _ add %o1,4,%o0
#endif

# extern uintD* copy_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
C(copy_loop_down:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll11
       _ sub %o0,4,%o0
Ll10:     ld [%o0],%o3
          sub %o1,4,%o1
          st %o3,[%o1]
          subcc %o2,1,%o2
          bne Ll10
         _ sub %o0,4,%o0
Ll11:   retl
       _ mov %o1,%o0
#endif
#if COUNTER_LOOPS
        andcc %o2,%o2,%g0
        be Ll11
       _ sub %o0,4,%o0
        sll %o2,2,%o2           # %o2 = 4*count
        sub %o0,%o2,%o0         # %o0 = &sourceptr[-count-1]
        sub %o1,%o2,%o1         # %o1 = &destptr[-count]
Ll10:     ld [%o0+%o2],%o3      # nächstes Digit holen
          subcc %o2,4,%o2       # Zähler erniedrigen, Pointer erniedrigen
          bne Ll10
         _ st %o3,[%o1+%o2]     # Digit ablegen
Ll11:   retl
       _ mov %o1,%o0
#endif

# extern uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler);
C(fill_loop_up:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
        andcc %o1,%o1,%g0
        be Ll13
       _ nop
Ll12:     st %o2,[%o0]
          subcc %o1,1,%o1
          bne Ll12
         _ add %o0,4,%o0
Ll13:   retl
       _ nop
#endif
#if COUNTER_LOOPS
        subcc %g0,%o1,%o1       # %o1 = -count
        be Ll13
       _ sub %o0,4,%o0
        sll %o1,2,%o1           # %o1 = -4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[count-1]
Ll12:     addcc %o1,4,%o1       # Zähler "erniedrigen", Pointer erhöhen
          bne Ll12
         _ st %o2,[%o0+%o1]     # Digit ablegen
Ll13:   retl
       _ add %o0,4,%o0
#endif

# extern uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler);
C(fill_loop_down:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
        andcc %o1,%o1,%g0
        be Ll15
       _ sub %o0,4,%o0
Ll14:     st %o2,[%o0]
          subcc %o1,1,%o1
          bne Ll14
         _ sub %o0,4,%o0
Ll15:   retl
       _ add %o0,4,%o0
#endif
#if COUNTER_LOOPS
        andcc %o1,%o1,%g0
        be Ll15
       _ sll %o1,2,%o1          # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[-count]
Ll14:     subcc %o1,4,%o1       # Zähler erniedrigen, Pointer erniedrigen
          bne Ll14
         _ st %o2,[%o0+%o1]     # Digit ablegen
Ll15:   retl
       _ nop
#endif

# extern uintD* clear_loop_up (uintD* destptr, uintC count);
C(clear_loop_up:) # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
        andcc %o1,%o1,%g0
        be Ll17
       _ nop
Ll16:     st %g0,[%o0]
          subcc %o1,1,%o1
          bne Ll16
         _ add %o0,4,%o0
Ll17:   retl
       _ nop
#endif
#if COUNTER_LOOPS
        subcc %g0,%o1,%o1       # %o1 = -count
        be Ll17
       _ sub %o0,4,%o0
        sll %o1,2,%o1           # %o1 = -4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[count-1]
Ll16:     addcc %o1,4,%o1       # Zähler "erniedrigen", Pointer erhöhen
          bne Ll16
         _ st %g0,[%o0+%o1]     # Digit 0 ablegen
Ll17:   retl
       _ add %o0,4,%o0
#endif

# extern uintD* clear_loop_down (uintD* destptr, uintC count);
C(clear_loop_down:) # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
        andcc %o1,%o1,%g0
        be Ll19
       _ sub %o0,4,%o0
Ll18:     st %g0,[%o0]
          subcc %o1,1,%o1
          bne Ll18
         _ sub %o0,4,%o0
Ll19:   retl
       _ add %o0,4,%o0
#endif
#if COUNTER_LOOPS
        andcc %o1,%o1,%g0
        be Ll19
       _ sll %o1,2,%o1          # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[-count]
Ll18:     subcc %o1,4,%o1       # Zähler erniedrigen, Pointer erniedrigen
          bne Ll18
         _ st %g0,[%o0+%o1]     # Digit 0 ablegen
Ll19:   retl
       _ nop
#endif

# extern void or_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(or_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll21
       _ nop
Ll20:     ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          or %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne Ll20
         _ add %o0,4,%o0
Ll21:   retl
       _ nop
#endif
#if COUNTER_LOOPS
        subcc %g0,%o2,%o2       # %o2 = -count
        be Ll21
       _ sub %o0,4,%o0
        sll %o2,2,%o2           # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
Ll20:     ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          or %o4,%o3,%o3        # beide verknüpfen
          bne Ll20
         _ st %o3,[%o1+%o2]     # Digit ablegen
Ll21:   retl
       _ nop
#endif

# extern void xor_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(xor_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll23
       _ nop
Ll22:     ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          xor %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne Ll22
         _ add %o0,4,%o0
Ll23:   retl
       _ nop
#endif
#if COUNTER_LOOPS
        subcc %g0,%o2,%o2       # %o2 = -count
        be Ll23
       _ sub %o0,4,%o0
        sll %o2,2,%o2           # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
Ll22:     ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          xor %o4,%o3,%o3       # beide verknüpfen
          bne Ll22
         _ st %o3,[%o1+%o2]     # Digit ablegen
Ll23:   retl
       _ nop
#endif

# extern void and_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(and_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll25
       _ nop
Ll24:     ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          and %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne Ll24
         _ add %o0,4,%o0
Ll25:   retl
       _ nop
#endif
#if COUNTER_LOOPS
        subcc %g0,%o2,%o2       # %o2 = -count
        be Ll25
       _ sub %o0,4,%o0
        sll %o2,2,%o2           # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
Ll24:     ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          and %o4,%o3,%o3       # beide verknüpfen
          bne Ll24
         _ st %o3,[%o1+%o2]     # Digit ablegen
Ll25:   retl
       _ nop
#endif

# extern void eqv_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(eqv_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll27
       _ nop
Ll26:     ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          xnor %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne Ll26
         _ add %o0,4,%o0
Ll27:   retl
       _ nop
#endif
#if COUNTER_LOOPS
        subcc %g0,%o2,%o2       # %o2 = -count
        be Ll27
       _ sub %o0,4,%o0
        sll %o2,2,%o2           # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
Ll26:     ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          xnor %o4,%o3,%o3      # beide verknüpfen
          bne Ll26
         _ st %o3,[%o1+%o2]     # Digit ablegen
Ll27:   retl
       _ nop
#endif

# extern void nand_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(nand_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll29
       _ nop
Ll28:     ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          and %o3,%o4,%o3
          xor %o3,-1,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne Ll28
         _ add %o0,4,%o0
Ll29:   retl
       _ nop
#endif
#if COUNTER_LOOPS
        subcc %g0,%o2,%o2       # %o2 = -count
        be Ll29
       _ sub %o0,4,%o0
        sll %o2,2,%o2           # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
Ll28:     ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          and %o4,%o3,%o3       # beide verknüpfen
          xor %o3,-1,%o3
          bne Ll28
         _ st %o3,[%o1+%o2]     # Digit ablegen
Ll29:   retl
       _ nop
#endif

# extern void nor_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(nor_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll31
       _ nop
Ll30:     ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          or %o3,%o4,%o3
          xor %o3,-1,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne Ll30
         _ add %o0,4,%o0
Ll31:   retl
       _ nop
#endif
#if COUNTER_LOOPS
        subcc %g0,%o2,%o2       # %o2 = -count
        be Ll31
       _ sub %o0,4,%o0
        sll %o2,2,%o2           # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
Ll30:     ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          or %o4,%o3,%o3        # beide verknüpfen
          xor %o3,-1,%o3
          bne Ll30
         _ st %o3,[%o1+%o2]     # Digit ablegen
Ll31:   retl
       _ nop
#endif

# extern void andc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(andc2_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll33
       _ nop
Ll32:     ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          andn %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne Ll32
         _ add %o0,4,%o0
Ll33:   retl
       _ nop
#endif
#if COUNTER_LOOPS
        subcc %g0,%o2,%o2       # %o2 = -count
        be Ll33
       _ sub %o0,4,%o0
        sll %o2,2,%o2           # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
Ll32:     ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          andn %o4,%o3,%o3      # beide verknüpfen
          bne Ll32
         _ st %o3,[%o1+%o2]     # Digit ablegen
Ll33:   retl
       _ nop
#endif

# extern void orc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(orc2_loop_up:) # Input in %o0,%o1,%o2
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll35
       _ nop
Ll34:     ld [%o0],%o3
          ld [%o1],%o4
          add %o1,4,%o1
          orn %o3,%o4,%o3
          st %o3,[%o0]
          subcc %o2,1,%o2
          bne Ll34
         _ add %o0,4,%o0
Ll35:   retl
       _ nop
#endif
#if COUNTER_LOOPS
        subcc %g0,%o2,%o2       # %o2 = -count
        be Ll35
       _ sub %o0,4,%o0
        sll %o2,2,%o2           # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count-1]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
Ll34:     ld [%o1+%o2],%o3      # nächstes Digit holen
          addcc %o2,4,%o2       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o2],%o4      # noch ein Digit holen
          orn %o4,%o3,%o3       # beide verknüpfen
          bne Ll34
         _ st %o3,[%o1+%o2]     # Digit ablegen
Ll35:   retl
       _ nop
#endif

# extern void not_loop_up (uintD* xptr, uintC count);
C(not_loop_up:) # Input in %o0,%o1
#if STANDARD_LOOPS
        andcc %o1,%o1,%g0
        be Ll37
       _ nop
Ll36:     ld [%o0],%o2
          subcc %o1,1,%o1
          xor %o2,-1,%o2
          st %o2,[%o0]
          bne Ll36
         _ add %o0,4,%o0
Ll37:   retl
       _ nop
#endif
#if COUNTER_LOOPS
        subcc %g0,%o1,%o1       # %o1 = -count
        be Ll37
       _ sub %o0,4,%o0
        sll %o1,2,%o1           # %o1 = -4*count
        sub %o0,%o1,%o0         # %o0 = &destptr[count-1]
Ll36:     addcc %o1,4,%o1       # Zähler "erniedrigen", Pointer erhöhen
          ld [%o0+%o1],%o2      # nächstes Digit holen
          xor %o2,-1,%o2
          bne Ll36
         _ st %o2,[%o0+%o1]     # Digit ablegen
Ll37:   retl
       _ nop
#endif

# extern bool and_test_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(and_test_loop_up:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll39
       _ nop
Ll38:     ld [%o0],%o3
          ld [%o1],%o4
          add %o0,4,%o0
          andcc %o3,%o4,%g0
          bne Ll40
         _ subcc %o2,1,%o2
          bne Ll38
         _ add %o1,4,%o1
Ll39:   retl
       _ mov 0,%o0
Ll40:   retl
       _ mov 1,%o0
#endif
#if COUNTER_LOOPS
        subcc %g0,%o2,%o2       # %o2 = -count
        be Ll39
       _ sll %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
          ld [%o0+%o2],%o3      # nächstes Digit holen
Ll38:     ld [%o1+%o2],%o4      # noch ein Digit holen
          andcc %o3,%o4,%g0     # beide verknüpfen
          bne Ll40
         _ addcc %o2,4,%o2      # Zähler "erniedrigen", Pointer erhöhen
          bne,a Ll38
         __ ld [%o0+%o2],%o3    # nächstes Digit holen
Ll39:   retl
       _ mov 0,%o0
Ll40:   retl
       _ mov 1,%o0
#endif

# extern bool test_loop_up (uintD* ptr, uintC count);
C(test_loop_up:) # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
        andcc %o1,%o1,%g0
        be Ll42
       _ nop
          ld [%o0],%o2
Ll41:     add %o0,4,%o0
          andcc %o2,%o2,%g0
          bne Ll43
         _ subcc %o1,1,%o1
          bne,a Ll41
         __ ld [%o0],%o2
Ll42:   retl
       _ mov 0,%o0
Ll43:   retl
       _ mov 1,%o0
#endif
#if COUNTER_LOOPS
        subcc %g0,%o1,%o1       # %o1 = -count
        be Ll42
       _ sll %o1,2,%o1          # %o1 = -4*count
        sub %o0,%o1,%o0         # %o0 = &ptr[count]
          ld [%o0+%o1],%o2      # nächstes Digit holen
Ll41:     andcc %o2,%o2,%g0     # testen
          bne Ll43
         _ addcc %o1,4,%o1      # Zähler "erniedrigen", Pointer erhöhen
          bne,a Ll41
         __ ld [%o0+%o1],%o2    # nächstes Digit holen
Ll42:   retl
       _ mov 0,%o0
Ll43:   retl
       _ mov 1,%o0
#endif

# extern signean compare_loop_up (uintD* xptr, uintD* yptr, uintC count);
C(compare_loop_up:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll45
       _ nop
          ld [%o0],%o3
Ll44:     ld [%o1],%o4
          add %o0,4,%o0
          subcc %o3,%o4,%g0
          bne Ll46
         _ add %o1,4,%o1
          subcc %o2,1,%o2
          bne,a Ll44
         __ ld [%o0],%o3
Ll45:   retl
       _ mov 0,%o0
Ll46:   blu Ll47
       _ nop
        retl
       _ mov 1,%o0
Ll47:   retl
       _ mov -1,%o0
#endif
#if COUNTER_LOOPS
        subcc %g0,%o2,%o2       # %o2 = -count
        be Ll45
       _ sll %o2,2,%o2          # %o2 = -4*count
        sub %o0,%o2,%o0         # %o0 = &xptr[count]
        sub %o1,%o2,%o1         # %o1 = &yptr[count]
          ld [%o0+%o2],%o3      # nächstes Digit holen
Ll44:     ld [%o1+%o2],%o4      # noch ein Digit holen
          subcc %o3,%o4,%g0     # vergleichen
          bne Ll46
         _ addcc %o2,4,%o2      # Zähler "erniedrigen", Pointer erhöhen
          bne,a Ll44
         __ ld [%o0+%o2],%o3    # nächstes Digit holen
Ll45:   retl
       _ mov 0,%o0
Ll46:   subcc %o3,%o4,%g0       # nochmals vergleichen
        blu Ll47
       _ nop
        retl
       _ mov 1,%o0
Ll47:   retl
       _ mov -1,%o0
#endif

# extern uintD add_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
C(add_loop_down:) # Input in %o0,%o1,%o2,%o3, verändert %g1, Output in %o0
#if STANDARD_LOOPS
        andcc %o3,%o3,%g0
        be Ll49
       _ mov %g0,%g1            # Carry := 0
        sub %o0,4,%o0
Ll48:     ld [%o0],%o4          # source1-digit
          sub %o1,4,%o1
          ld [%o1],%o5          # source2-digit
          subcc %g0,%g1,%g0     # carry
          addxcc %o4,%o5,%o4    # addieren
          addx %g0,%g0,%g1      # neuer Carry
          sub %o2,4,%o2
          st %o4,[%o2]          # Digit ablegen
          subcc %o3,1,%o3
          bne Ll48
         _ sub %o0,4,%o0
Ll49:   retl
       _ mov %g1,%o0
#endif
#if COUNTER_LOOPS
        andcc %o3,%o3,%g0
        be Ll49
       _ mov %g0,%g1            # Carry := 0
        sub %o0,4,%o0
        sub %o1,4,%o1
        sll %o3,2,%o3           # %o3 = 4*count
        sub %o0,%o3,%o0         # %o0 = &sourceptr1[-count-1]
        sub %o1,%o3,%o1         # %o1 = &sourceptr2[-count-1]
        sub %o2,%o3,%o2         # %o2 = &destptr[-count]
Ll48:     ld [%o0+%o3],%o4      # source1-digit
          ld [%o1+%o3],%o5      # source2-digit
          subcc %g0,%g1,%g0     # carry
          addxcc %o4,%o5,%o4    # addieren
          addx %g0,%g0,%g1      # neuer Carry
          subcc %o3,4,%o3
          bne Ll48
         _ st %o4,[%o2+%o3]     # Digit ablegen
Ll49:   retl
       _ mov %g1,%o0
#endif
#if UNROLLED_LOOPS
        and %o3,7,%o4           # count mod 8
        sll %o4,2,%o5
        sub %o0,%o5,%o0         # %o0 = &sourceptr1[-(count mod 8)]
        sub %o1,%o5,%o1         # %o1 = &sourceptr2[-(count mod 8)]
        sub %o2,%o5,%o2         # %o2 = &destptr[-(count mod 8)]
        sll %o4,4,%o4
        set _add_loop_down+176,%o5
        sub %o5,%o4,%o5
        jmp %o5                 # Sprung nach _add_loop_down+4*(12+4*8-4*(count mod 8))
       _ subcc %g0,%g0,%g0      # carry löschen
Ll48:     subcc %g0,%g1,%g0     # carry
          ld [%o0+28],%o4       # source1-digit
          ld [%o1+28],%o5       # source2-digit
          addxcc %o5,%o4,%o5    # addieren
          st %o5,[%o2+28]       # Digit ablegen
          ld [%o0+24],%o4       # source1-digit
          ld [%o1+24],%o5       # source2-digit
          addxcc %o5,%o4,%o5    # addieren
          st %o5,[%o2+24]       # Digit ablegen
          ld [%o0+20],%o4       # source1-digit
          ld [%o1+20],%o5       # source2-digit
          addxcc %o5,%o4,%o5    # addieren
          st %o5,[%o2+20]       # Digit ablegen
          ld [%o0+16],%o4       # source1-digit
          ld [%o1+16],%o5       # source2-digit
          addxcc %o5,%o4,%o5    # addieren
          st %o5,[%o2+16]       # Digit ablegen
          ld [%o0+12],%o4       # source1-digit
          ld [%o1+12],%o5       # source2-digit
          addxcc %o5,%o4,%o5    # addieren
          st %o5,[%o2+12]       # Digit ablegen
          ld [%o0+8],%o4        # source1-digit
          ld [%o1+8],%o5        # source2-digit
          addxcc %o5,%o4,%o5    # addieren
          st %o5,[%o2+8]        # Digit ablegen
          ld [%o0+4],%o4        # source1-digit
          ld [%o1+4],%o5        # source2-digit
          addxcc %o5,%o4,%o5    # addieren
          st %o5,[%o2+4]        # Digit ablegen
          ld [%o0],%o4          # source1-digit
          ld [%o1],%o5          # source2-digit
          addxcc %o5,%o4,%o5    # addieren
          st %o5,[%o2]          # Digit ablegen
          addx %g0,%g0,%g1      # neuer Carry
          sub %o0,32,%o0
          sub %o1,32,%o1
          subcc %o3,8,%o3       # noch mindestens 8 Digits abzuarbeiten?
          bcc Ll48
         _ sub %o2,32,%o2
        retl
       _ mov %g1,%o0
#endif

# extern uintD addto_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
C(addto_loop_down:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll51
       _ mov %g0,%o5            # Carry := 0
        sub %o0,4,%o0
Ll50:     ld [%o0],%o3          # source-digit
          sub %o1,4,%o1
          ld [%o1],%o4          # dest-digit
          subcc %g0,%o5,%g0     # carry
          addxcc %o4,%o3,%o4    # addieren
          addx %g0,%g0,%o5      # neuer Carry
          st %o4,[%o1]          # Digit ablegen
          subcc %o2,1,%o2
          bne Ll50
         _ sub %o0,4,%o0
Ll51:   retl
       _ mov %o5,%o0
#endif
#if COUNTER_LOOPS
        andcc %o2,%o2,%g0
        be Ll51
       _ mov %g0,%o5            # Carry := 0
        sub %o0,4,%o0
        sub %o1,4,%o1
        sll %o2,2,%o2           # %o2 = 4*count
        sub %o0,%o2,%o0         # %o0 = &sourceptr[-count-1]
        sub %o1,%o2,%o1         # %o1 = &destptr[-count-1]
          ld [%o0+%o2],%o3      # source-digit
Ll50:     ld [%o1+%o2],%o4      # dest-digit
          subcc %g0,%o5,%g0     # carry
          addxcc %o4,%o3,%o4    # addieren
          addx %g0,%g0,%o5      # neuer Carry
          st %o4,[%o1+%o2]      # Digit ablegen
          subcc %o2,4,%o2
          bne,a Ll50
         __ ld [%o0+%o2],%o3    # source-digit
Ll51:   retl
       _ mov %o5,%o0
#endif
#if UNROLLED_LOOPS
        and %o2,7,%o3           # count mod 8
        sll %o3,2,%o4
        sub %o0,%o4,%o0         # %o0 = &sourceptr[-(count mod 8)]
        sub %o1,%o4,%o1         # %o1 = &destptr[-(count mod 8)]
        sll %o3,4,%o3
        set _addto_loop_down+172,%o4
        sub %o4,%o3,%o4
        jmp %o4                 # Sprung nach _addto_loop_down+4*(11+4*8-4*(count mod 8))
       _ subcc %g0,%g0,%g0      # carry löschen
Ll50:     subcc %g0,%o5,%g0     # carry
          ld [%o0+28],%o3       # source-digit
          ld [%o1+28],%o4       # dest-digit
          addxcc %o4,%o3,%o4    # addieren
          st %o4,[%o1+28]       # Digit ablegen
          ld [%o0+24],%o3       # source-digit
          ld [%o1+24],%o4       # dest-digit
          addxcc %o4,%o3,%o4    # addieren
          st %o4,[%o1+24]       # Digit ablegen
          ld [%o0+20],%o3       # source-digit
          ld [%o1+20],%o4       # dest-digit
          addxcc %o4,%o3,%o4    # addieren
          st %o4,[%o1+20]       # Digit ablegen
          ld [%o0+16],%o3       # source-digit
          ld [%o1+16],%o4       # dest-digit
          addxcc %o4,%o3,%o4    # addieren
          st %o4,[%o1+16]       # Digit ablegen
          ld [%o0+12],%o3       # source-digit
          ld [%o1+12],%o4       # dest-digit
          addxcc %o4,%o3,%o4    # addieren
          st %o4,[%o1+12]       # Digit ablegen
          ld [%o0+8],%o3        # source-digit
          ld [%o1+8],%o4        # dest-digit
          addxcc %o4,%o3,%o4    # addieren
          st %o4,[%o1+8]        # Digit ablegen
          ld [%o0+4],%o3        # source-digit
          ld [%o1+4],%o4        # dest-digit
          addxcc %o4,%o3,%o4    # addieren
          st %o4,[%o1+4]        # Digit ablegen
          ld [%o0],%o3          # source-digit
          ld [%o1],%o4          # dest-digit
          addxcc %o4,%o3,%o4    # addieren
          st %o4,[%o1]          # Digit ablegen
          addx %g0,%g0,%o5      # neuer Carry
          sub %o0,32,%o0
          subcc %o2,8,%o2       # noch mindestens 8 Digits abzuarbeiten?
          bcc Ll50
         _ sub %o1,32,%o1
        retl
       _ mov %o5,%o0
#endif

# extern uintD inc_loop_down (uintD* ptr, uintC count);
C(inc_loop_down:) # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
        andcc %o1,%o1,%g0
        be Ll53
       _ sub %o0,4,%o0
Ll52:     ld [%o0],%o2
          addcc %o2,1,%o2
          bne Ll54
         _ st %o2,[%o0]
          subcc %o1,1,%o1
          bne Ll52
         _ sub %o0,4,%o0
Ll53:   retl
       _ mov 1,%o0
Ll54:   retl
       _ mov 0,%o0
#endif
#if COUNTER_LOOPS
        andcc %o1,%o1,%g0
        be Ll53
       _ sub %o0,4,%o0
        sll %o1,2,%o1           # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &ptr[-count-1]
          ld [%o0+%o1],%o2      # digit holen
Ll52:     addcc %o2,1,%o2       # incrementieren
          bne Ll54
         _ st %o2,[%o0+%o1]     # ablegen
          subcc %o1,4,%o1       # Zähler erniedrigen, Pointer erniedrigen
          bne,a Ll52
         __ ld [%o0+%o1],%o2
Ll53:   retl
       _ mov 1,%o0
Ll54:   retl
       _ mov 0,%o0
#endif

# extern uintD sub_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
C(sub_loop_down:) # Input in %o0,%o1,%o2,%o3, verändert %g1, Output in %o0
#if STANDARD_LOOPS
        andcc %o3,%o3,%g0
        be Ll56
       _ mov %g0,%g1            # Carry := 0
        sub %o0,4,%o0
Ll55:     ld [%o0],%o4          # source1-digit
          sub %o1,4,%o1
          ld [%o1],%o5          # source2-digit
          subcc %g0,%g1,%g0     # carry
          subxcc %o4,%o5,%o4    # subtrahieren
          addx %g0,%g0,%g1      # neuer Carry
          sub %o2,4,%o2
          st %o4,[%o2]          # Digit ablegen
          subcc %o3,1,%o3
          bne Ll55
         _ sub %o0,4,%o0
Ll56:   retl
       _ mov %g1,%o0
#endif
#if COUNTER_LOOPS
        andcc %o3,%o3,%g0
        be Ll56
       _ mov %g0,%g1            # Carry := 0
        sub %o0,4,%o0
        sub %o1,4,%o1
        sll %o3,2,%o3           # %o3 = 4*count
        sub %o0,%o3,%o0         # %o0 = &sourceptr1[-count-1]
        sub %o1,%o3,%o1         # %o1 = &sourceptr2[-count-1]
        sub %o2,%o3,%o2         # %o2 = &destptr[-count]
Ll55:     ld [%o0+%o3],%o4      # source1-digit
          ld [%o1+%o3],%o5      # source2-digit
          subcc %g0,%g1,%g0     # carry
          subxcc %o4,%o5,%o4    # subtrahieren
          addx %g0,%g0,%g1      # neuer Carry
          subcc %o3,4,%o3
          bne Ll55
         _ st %o4,[%o2+%o3]     # Digit ablegen
Ll56:   retl
       _ mov %g1,%o0
#endif
#if UNROLLED_LOOPS
        and %o3,7,%o4           # count mod 8
        sll %o4,2,%o5
        sub %o0,%o5,%o0         # %o0 = &sourceptr1[-(count mod 8)]
        sub %o1,%o5,%o1         # %o1 = &sourceptr2[-(count mod 8)]
        sub %o2,%o5,%o2         # %o2 = &destptr[-(count mod 8)]
        sll %o4,4,%o4
        set _sub_loop_down+176,%o5
        sub %o5,%o4,%o5
        jmp %o5                 # Sprung nach _sub_loop_down+4*(12+4*8-4*(count mod 8))
       _ subcc %g0,%g0,%g0      # carry löschen
Ll55:     subcc %g0,%g1,%g0     # carry
          ld [%o0+28],%o4       # source1-digit
          ld [%o1+28],%o5       # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+28]       # Digit ablegen
          ld [%o0+24],%o4       # source1-digit
          ld [%o1+24],%o5       # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+24]       # Digit ablegen
          ld [%o0+20],%o4       # source1-digit
          ld [%o1+20],%o5       # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+20]       # Digit ablegen
          ld [%o0+16],%o4       # source1-digit
          ld [%o1+16],%o5       # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+16]       # Digit ablegen
          ld [%o0+12],%o4       # source1-digit
          ld [%o1+12],%o5       # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+12]       # Digit ablegen
          ld [%o0+8],%o4        # source1-digit
          ld [%o1+8],%o5        # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+8]        # Digit ablegen
          ld [%o0+4],%o4        # source1-digit
          ld [%o1+4],%o5        # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+4]        # Digit ablegen
          ld [%o0],%o4          # source1-digit
          ld [%o1],%o5          # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2]          # Digit ablegen
          addx %g0,%g0,%g1      # neuer Carry
          sub %o0,32,%o0
          sub %o1,32,%o1
          subcc %o3,8,%o3       # noch mindestens 8 Digits abzuarbeiten?
          bcc Ll55
         _ sub %o2,32,%o2
        retl
       _ mov %g1,%o0
#endif

# extern uintD subx_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
C(subx_loop_down:) # Input in %o0,%o1,%o2,%o3,%o4, verändert %g1, Output in %o0
#if STANDARD_LOOPS
        andcc %o3,%o3,%g0
        be Ll58
       _ mov %o4,%g1            # Carry
        sub %o0,4,%o0
Ll57:     ld [%o0],%o4          # source1-digit
          sub %o1,4,%o1
          ld [%o1],%o5          # source2-digit
          subcc %g0,%g1,%g0     # carry
          subxcc %o4,%o5,%o4    # subtrahieren
          addx %g0,%g0,%g1      # neuer Carry
          sub %o2,4,%o2
          st %o4,[%o2]          # Digit ablegen
          subcc %o3,1,%o3
          bne Ll57
         _ sub %o0,4,%o0
Ll58:   retl
       _ mov %g1,%o0
#endif
#if COUNTER_LOOPS
        andcc %o3,%o3,%g0
        be Ll58
       _ mov %o4,%g1            # Carry
        sub %o0,4,%o0
        sub %o1,4,%o1
        sll %o3,2,%o3           # %o3 = 4*count
        sub %o0,%o3,%o0         # %o0 = &sourceptr1[-count-1]
        sub %o1,%o3,%o1         # %o1 = &sourceptr2[-count-1]
        sub %o2,%o3,%o2         # %o2 = &destptr[-count]
Ll57:     ld [%o0+%o3],%o4      # source1-digit
          ld [%o1+%o3],%o5      # source2-digit
          subcc %g0,%g1,%g0     # carry
          subxcc %o4,%o5,%o4    # subtrahieren
          addx %g0,%g0,%g1      # neuer Carry
          subcc %o3,4,%o3
          bne Ll57
         _ st %o4,[%o2+%o3]     # Digit ablegen
Ll58:   retl
       _ mov %g1,%o0
#endif
#if UNROLLED_LOOPS
        and %o3,7,%o5           # count mod 8
        sll %o5,2,%g1
        sub %o0,%g1,%o0         # %o0 = &sourceptr1[-(count mod 8)]
        sub %o1,%g1,%o1         # %o1 = &sourceptr2[-(count mod 8)]
        sub %o2,%g1,%o2         # %o2 = &destptr[-(count mod 8)]
        sll %o5,4,%o5
        set _subx_loop_down+176,%g1
        sub %g1,%o5,%g1
        jmp %g1                 # Sprung nach _subx_loop_down+4*(12+4*8-4*(count mod 8))
       _ subcc %g0,%o4,%g0      # carry initialisieren
Ll57:     subcc %g0,%g1,%g0     # carry
          ld [%o0+28],%o4       # source1-digit
          ld [%o1+28],%o5       # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+28]       # Digit ablegen
          ld [%o0+24],%o4       # source1-digit
          ld [%o1+24],%o5       # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+24]       # Digit ablegen
          ld [%o0+20],%o4       # source1-digit
          ld [%o1+20],%o5       # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+20]       # Digit ablegen
          ld [%o0+16],%o4       # source1-digit
          ld [%o1+16],%o5       # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+16]       # Digit ablegen
          ld [%o0+12],%o4       # source1-digit
          ld [%o1+12],%o5       # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+12]       # Digit ablegen
          ld [%o0+8],%o4        # source1-digit
          ld [%o1+8],%o5        # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+8]        # Digit ablegen
          ld [%o0+4],%o4        # source1-digit
          ld [%o1+4],%o5        # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2+4]        # Digit ablegen
          ld [%o0],%o4          # source1-digit
          ld [%o1],%o5          # source2-digit
          subxcc %o4,%o5,%o4    # subtrahieren
          st %o4,[%o2]          # Digit ablegen
          addx %g0,%g0,%g1      # neuer Carry
          sub %o0,32,%o0
          sub %o1,32,%o1
          subcc %o3,8,%o3       # noch mindestens 8 Digits abzuarbeiten?
          bcc Ll57
         _ sub %o2,32,%o2
        retl
       _ mov %g1,%o0
#endif

# extern uintD subfrom_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
C(subfrom_loop_down:) # Input in %o0,%o1,%o2, Output in %o0
#if STANDARD_LOOPS
        andcc %o2,%o2,%g0
        be Ll60
       _ mov %g0,%o5            # Carry := 0
        sub %o0,4,%o0
Ll59:     ld [%o0],%o3          # source-digit
          sub %o1,4,%o1
          ld [%o1],%o4          # dest-digit
          subcc %g0,%o5,%g0     # carry
          subxcc %o4,%o3,%o4    # subtrahieren
          addx %g0,%g0,%o5      # neuer Carry
          st %o4,[%o1]          # Digit ablegen
          subcc %o2,1,%o2
          bne Ll59
         _ sub %o0,4,%o0
Ll60:   retl
       _ mov %o5,%o0
#endif
#if COUNTER_LOOPS
        andcc %o2,%o2,%g0
        be Ll60
       _ mov %g0,%o5            # Carry := 0
        sub %o0,4,%o0
        sub %o1,4,%o1
        sll %o2,2,%o2           # %o2 = 4*count
        sub %o0,%o2,%o0         # %o0 = &sourceptr[-count-1]
        sub %o1,%o2,%o1         # %o1 = &destptr[-count-1]
          ld [%o0+%o2],%o3      # source-digit
Ll59:     ld [%o1+%o2],%o4      # dest-digit
          subcc %g0,%o5,%g0     # carry
          subxcc %o4,%o3,%o4    # subtrahieren
          addx %g0,%g0,%o5      # neuer Carry
          st %o4,[%o1+%o2]      # Digit ablegen
          subcc %o2,4,%o2
          bne,a Ll59
         __ ld [%o0+%o2],%o3    # source-digit
Ll60:   retl
       _ mov %o5,%o0
#endif
#if UNROLLED_LOOPS
        and %o2,7,%o3           # count mod 8
        sll %o3,2,%o4
        sub %o0,%o4,%o0         # %o0 = &sourceptr[-(count mod 8)]
        sub %o1,%o4,%o1         # %o1 = &destptr[-(count mod 8)]
        sll %o3,4,%o3
        set _subfrom_loop_down+172,%o4
        sub %o4,%o3,%o4
        jmp %o4                 # Sprung nach _subfrom_loop_down+4*(11+4*8-4*(count mod 8))
       _ subcc %g0,%g0,%g0      # carry löschen
Ll59:     subcc %g0,%o5,%g0     # carry
          ld [%o0+28],%o3       # source-digit
          ld [%o1+28],%o4       # dest-digit
          subxcc %o4,%o3,%o4    # subtrahieren
          st %o4,[%o1+28]       # Digit ablegen
          ld [%o0+24],%o3       # source-digit
          ld [%o1+24],%o4       # dest-digit
          subxcc %o4,%o3,%o4    # subtrahieren
          st %o4,[%o1+24]       # Digit ablegen
          ld [%o0+20],%o3       # source-digit
          ld [%o1+20],%o4       # dest-digit
          subxcc %o4,%o3,%o4    # subtrahieren
          st %o4,[%o1+20]       # Digit ablegen
          ld [%o0+16],%o3       # source-digit
          ld [%o1+16],%o4       # dest-digit
          subxcc %o4,%o3,%o4    # subtrahieren
          st %o4,[%o1+16]       # Digit ablegen
          ld [%o0+12],%o3       # source-digit
          ld [%o1+12],%o4       # dest-digit
          subxcc %o4,%o3,%o4    # subtrahieren
          st %o4,[%o1+12]       # Digit ablegen
          ld [%o0+8],%o3        # source-digit
          ld [%o1+8],%o4        # dest-digit
          subxcc %o4,%o3,%o4    # subtrahieren
          st %o4,[%o1+8]        # Digit ablegen
          ld [%o0+4],%o3        # source-digit
          ld [%o1+4],%o4        # dest-digit
          subxcc %o4,%o3,%o4    # subtrahieren
          st %o4,[%o1+4]        # Digit ablegen
          ld [%o0],%o3          # source-digit
          ld [%o1],%o4          # dest-digit
          subxcc %o4,%o3,%o4    # subtrahieren
          st %o4,[%o1]          # Digit ablegen
          addx %g0,%g0,%o5      # neuer Carry
          sub %o0,32,%o0
          subcc %o2,8,%o2       # noch mindestens 8 Digits abzuarbeiten?
          bcc Ll59
         _ sub %o1,32,%o1
        retl
       _ mov %o5,%o0
#endif

# extern uintD dec_loop_down (uintD* ptr, uintC count);
C(dec_loop_down:) # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
        andcc %o1,%o1,%g0
        be Ll62
       _ sub %o0,4,%o0
Ll61:     ld [%o0],%o2
          subcc %o2,1,%o2
          bcc Ll63
         _ st %o2,[%o0]
          subcc %o1,1,%o1
          bne Ll61
         _ sub %o0,4,%o0
Ll62:   retl
       _ mov -1,%o0
Ll63:   retl
       _ mov 0,%o0
#endif
#if COUNTER_LOOPS
        andcc %o1,%o1,%g0
        be Ll62
       _ sub %o0,4,%o0
        sll %o1,2,%o1           # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &ptr[-count-1]
          ld [%o0+%o1],%o2      # digit holen
Ll61:     subcc %o2,1,%o2       # decrementieren
          bcc Ll63
         _ st %o2,[%o0+%o1]     # ablegen
          subcc %o1,4,%o1       # Zähler erniedrigen, Pointer erniedrigen
          bne,a Ll61
         __ ld [%o0+%o1],%o2
Ll62:   retl
       _ mov -1,%o0
Ll63:   retl
       _ mov 0,%o0
#endif

# extern uintD neg_loop_down (uintD* ptr, uintC count);
C(neg_loop_down:) # Input in %o0,%o1, Output in %o0
#if STANDARD_LOOPS
        # erstes Digit /=0 suchen:
        andcc %o1,%o1,%g0
        be Ll65
       _ sub %o0,4,%o0
Ll64:     ld [%o0],%o2
          subcc %g0,%o2,%o2
          bne Ll66
         _ subcc %o1,1,%o1
          bne Ll64
         _ sub %o0,4,%o0
Ll65:   retl
       _ mov 0,%o0
Ll66:   # erstes Digit /=0 gefunden, ab jetzt gibt's Carrys
        st %o2,[%o0]            # 1 Digit negieren
        # alle anderen Digits invertieren:
        be Ll68
       _ sub %o0,4,%o0
Ll67:     ld [%o0],%o2
          subcc %o1,1,%o1
          xor %o2,-1,%o2
          st %o2,[%o0]
          bne Ll67
         _ sub %o0,4,%o0
Ll68:   retl
       _ mov -1,%o0
#endif
#if COUNTER_LOOPS
        # erstes Digit /=0 suchen:
        andcc %o1,%o1,%g0
        be Ll65
       _ sub %o0,4,%o0
        sll %o1,2,%o1           # %o1 = 4*count
        sub %o0,%o1,%o0         # %o0 = &ptr[-count-1]
          ld [%o0+%o1],%o2      # digit holen
Ll64:     subcc %g0,%o2,%o2     # negieren, testen
          bne Ll66
         _ subcc %o1,4,%o1      # Zähler erniedrigen, Pointer erniedrigen
          bne,a Ll64
         __ ld [%o0+%o1],%o2
Ll65:   retl
       _ mov 0,%o0
Ll66:   # erstes Digit /=0 gefunden, ab jetzt gibt's Carrys
        # alle anderen Digits invertieren:
        add %o1,4,%o1
        st %o2,[%o0+%o1]        # ablegen
        subcc %o1,4,%o1
        be Ll68
       _ nop
          ld [%o0+%o1],%o2
Ll67:     xor %o2,-1,%o2
          st %o2,[%o0+%o1]
          subcc %o1,4,%o1
          bne,a Ll67
         __ ld [%o0+%o1],%o2
Ll68:   retl
       _ mov -1,%o0
#endif

# extern uintD shift1left_loop_down (uintD* ptr, uintC count);
C(shift1left_loop_down:) # Input in %o0,%o1, Output in %o0
        andcc %o1,%o1,%g0
        be Ll70
       _ mov 0,%o3              # Carry := 0
        sub %o0,4,%o0
Ll69:     ld [%o0],%o2          # Digit
          subcc %g0,%o3,%g0     # carry
          addxcc %o2,%o2,%o2    # shiften
          addx %g0,%g0,%o3      # neues Carry
          st %o2,[%o0]          # Digit ablegen
          subcc %o1,1,%o1
          bne Ll69
         _ sub %o0,4,%o0
Ll70:   retl
       _ mov %o3,%o0

# extern uintD shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry);
C(shiftleft_loop_down:) # Input in %o0,%o1,%o2,%o3, verändert %g1, Output in %o0
        andcc %o1,%o1,%g0
        be Ll72
       _ sub %g0,%o2,%g1        # 32-i (mod 32)
        sub %o0,4,%o0
Ll71:     ld [%o0],%o4          # Digit
          subcc %o1,1,%o1
          sll %o4,%o2,%o5       # dessen niedere (32-i) Bits
          or %o3,%o5,%o5        # mit dem alten Carry kombinieren
          st %o5,[%o0]          # Digit ablegen
          srl %o4,%g1,%o3       # dessen höchste i Bits liefern den neuen Carry
          bne Ll71
         _ sub %o0,4,%o0
Ll72:   retl
       _ mov %o3,%o0

# extern uintD shiftleftcopy_loop_down (uintD* sourceptr, uintD* destptr, uintC count, uintC i);
C(shiftleftcopy_loop_down:) # Input in %o0,%o1,%o2,%o3, verändert %g1,%g2, Output in %o0
        andcc %o2,%o2,%g0
        be Ll74
       _ mov 0,%o4              # Carry := 0
        sub %g0,%o3,%g1         # 32-i (mod 32)
        sub %o0,4,%o0
Ll73:     ld [%o0],%o5          # Digit
          subcc %o2,1,%o2
          sll %o5,%o3,%g2       # dessen niedere (32-i) Bits
          or %o4,%g2,%g2        # mit dem alten Carry kombinieren
          sub %o1,4,%o1
          st %g2,[%o1]          # Digit ablegen
          srl %o5,%g1,%o4       # dessen höchste i Bits liefern den neuen Carry
          bne Ll73
         _ sub %o0,4,%o0
Ll74:   retl
       _ mov %o4,%o0

# extern uintD shift1right_loop_up (uintD* ptr, uintC count, uintD carry);
C(shift1right_loop_up:) # Input in %o0,%o1,%o2, Output in %o0
        andcc %o1,%o1,%g0
        be Ll76
       _ sll %o2,31,%o2         # Carry
Ll75:     ld [%o0],%o3          # Digit
          subcc %o1,1,%o1
          srl %o3,1,%o4         # shiften
          or %o2,%o4,%o4        # und mit altem Carry kombinieren
          st %o4,[%o0]          # und ablegen
          sll %o3,31,%o2        # neuer Carry
          bne Ll75
         _ add %o0,4,%o0
Ll76:   retl
       _ mov %o2,%o0

# extern uintD shiftright_loop_up (uintD* ptr, uintC count, uintC i);
C(shiftright_loop_up:) # Input in %o0,%o1,%o2, verändert %g1, Output in %o0
        andcc %o1,%o1,%g0
        be Ll78
       _ or %g0,%g0,%o3         # Carry := 0
        sub %g0,%o2,%g1         # 32-i (mod 32)
Ll77:     ld [%o0],%o4          # Digit
          subcc %o1,1,%o1
          srl %o4,%o2,%o5       # shiften
          or %o3,%o5,%o5        # und mit altem Carry kombinieren
          st %o5,[%o0]          # und ablegen
          sll %o4,%g1,%o3       # neuer Carry
          bne Ll77
         _ add %o0,4,%o0
Ll78:   retl
       _ mov %o3,%o0

# extern uintD shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i);
C(shiftrightsigned_loop_up:) # Input in %o0,%o1,%o2, verändert %g1, Output in %o0
        ld [%o0],%o4            # erstes Digit
        sub %g0,%o2,%g1         # 32-i (mod 32)
        sra %o4,%o2,%o5         # shiften
        st %o5,[%o0]            # und ablegen
        sll %o4,%g1,%o3         # neuer Carry
        subcc %o1,1,%o1
        be Ll80
       _ add %o0,4,%o0
Ll79:     ld [%o0],%o4          # Digit
          subcc %o1,1,%o1
          srl %o4,%o2,%o5       # shiften
          or %o3,%o5,%o5        # und mit altem Carry kombinieren
          st %o5,[%o0]          # und ablegen
          sll %o4,%g1,%o3       # neuer Carry
          bne Ll79
         _ add %o0,4,%o0
Ll80:   retl
       _ mov %o3,%o0

# extern uintD shiftrightcopy_loop_up (uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry);
C(shiftrightcopy_loop_up:) # Input in %o0,%o1,%o2,%o3,%o4, verändert %g1,%g2, Output in %o0
        sub %g0,%o3,%g1         # 32-i (mod 32)
        andcc %o2,%o2,%g0
        be Ll82
       _ sll %o4,%g1,%g2        # erster Carry
Ll81:     ld [%o0],%o4          # Digit
          add %o0,4,%o0
          srl %o4,%o3,%o5       # shiften
          or %g2,%o5,%o5        # und mit altem Carry kombinieren
          st %o5,[%o1]          # und ablegen
          sll %o4,%g1,%g2       # neuer Carry
          subcc %o2,1,%o2
          bne Ll81
         _ add %o1,4,%o1
Ll82:   retl
       _ mov %g2,%o0

# extern uintD mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit);
C(mulusmall_loop_down:) # Input in %o0,%o1,%o2,%o3, Output in %o0
        andcc %o2,%o2,%g0
        be Ll85
       _ sub %o1,4,%o1
Ll83:     # nächstes Digit [%o1] mit der 6-Bit-Zahl %o0 multiplizieren
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
          tst %o4               # Korrektur, falls %o4 negativ war
          bge Ll84
         _ sra %o5,26,%o3       # 6 obere Bits von %o5 -> neuer Carry
          add %o3,%o0,%o3       # (falls %o4 negativ war, noch + %o0)
Ll84:     rd %y,%o4
          srl %o4,26,%o4        # 6 obere Bits von %y
          sll %o5,6,%o5         # 26 untere Bits von %o5
          or %o5,%o4,%o4        # neues Digit
          st %o4,[%o1]          # ablegen
          subcc %o2,1,%o2
          bne Ll83
         _ sub %o1,4,%o1
Ll85:   retl
       _ mov %o3,%o0

# extern void mulu_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
#if !MULU32_INLINE
C(mulu_loop_down:) # Input in %i0,%i1,%i2,%i3
        save %sp,-96,%sp
        mov 0,%l0               # Carry
Ll86:     sub %i1,4,%i1
          ld [%i1],%o1          # nächstes Digit
          call _mulu32_         # mit digit multiplizieren
         _ mov %i0,%o0
          addcc %l0,%o0,%o0     # und bisherigen Carry addieren
          addx %g0,%g1,%l0      # High-Digit gibt neuen Carry
          sub %i2,4,%i2
          subcc %i3,1,%i3
          bne Ll86
         _ st %o0,[%i2]         # Low-Digit ablegen
        st %l0,[%i2-4]          # letzten Carry ablegen
        ret
       _ restore
#else
C(mulu_loop_down:) # Input in %o0,%o1,%o2,%o3, verändert %g1
        mov 0,%o4               # Carry
Ll87:     ld [%o1-4],%g1        # nächstes Digit
          # mit digit multiplizieren: %o0 * %g1 -> %o5|%g1
#ifdef sparcv8
          sub     %o1,4,%o1
          umul    %g1,%o0,%g1
          rd      %y,%o5
#else
          mov     %g1,%y
          sub     %o1,4,%o1     # Wartetakt!
          andcc   %g0,%g0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%o0,%o5
          mulscc  %o5,%g0,%o5
          tst     %o0
          bl,a    Ll88
         __ add     %o5,%g1,%o5
Ll88:     rd      %y,%g1
#endif
          addcc %o4,%g1,%g1     # und bisherigen Carry addieren
          addx %g0,%o5,%o4      # High-Digit gibt neuen Carry
          sub %o2,4,%o2
          subcc %o3,1,%o3
          bne Ll87
         _ st %g1,[%o2]         # Low-Digit ablegen
        retl
       _ st %o4,[%o2-4]         # letzten Carry ablegen
#endif

# extern uintD muluadd_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
C(muluadd_loop_down:) # Input in %i0,%i1,%i2,%i3, Output in %i0
#if !MULU32_INLINE
        save %sp,-96,%sp
        mov 0,%l0               # Carry
Ll89:     sub %i1,4,%i1
          ld [%i1],%o1          # nächstes source-Digit
          call _mulu32_         # mit digit multiplizieren
         _ mov %i0,%o0
          sub %i2,4,%i2
          ld [%i2],%o1          # nächstes dest-digit
          addcc %l0,%o0,%o0     # und bisherigen Carry addieren
          addx %g0,%g1,%l0      # High-Digit gibt neuen Carry
          addcc %o1,%o0,%o0     # addieren
          addx %g0,%l0,%l0
          subcc %i3,1,%i3
          bne Ll89
         _ st %o0,[%i2]         # Low-Digit ablegen
        mov %l0,%i0             # letzter Carry
        ret
       _ restore
#else
        save %sp,-96,%sp
        mov 0,%l0               # Carry
#ifndef sparcv8
        sra %i0,31,%l1          # 0 falls %i0>=0, -1 falls %i0<0
#endif
Ll89:     ld [%i1-4],%o1        # nächstes source-Digit
          sub %i1,4,%i1
          # mit digit multiplizieren: %i0 * %o1 -> %o2|%o0
#ifdef sparcv8
          umul    %i0,%o1,%o0
          rd      %y,%o2
#else
          mov     %o1,%y
          and     %o1,%l1,%o3   # Wartetakt!
          andcc   %g0,%g0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%g0,%o2
          add     %o2,%o3,%o2   # %o3 = (0 falls %i0>=0, %o1 falls %i0<0)
          rd      %y,%o0
#endif
          sub %i2,4,%i2
          ld [%i2],%o1          # nächstes dest-digit
          addcc %l0,%o0,%o0     # und bisherigen Carry addieren
          addx %g0,%o2,%l0      # High-Digit gibt neuen Carry
          addcc %o1,%o0,%o0     # addieren
          addx %g0,%l0,%l0
          subcc %i3,1,%i3
          bne Ll89
         _ st %o0,[%i2]         # Low-Digit ablegen
        mov %l0,%i0             # letzter Carry
        ret
       _ restore
#endif

# extern uintD mulusub_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
C(mulusub_loop_down:) # Input in %i0,%i1,%i2,%i3, Output in %i0
#if !MULU32_INLINE
        save %sp,-96,%sp
        mov 0,%l0               # Carry
Ll90:     sub %i1,4,%i1
          ld [%i1],%o1          # nächstes source-Digit
          call _mulu32_         # mit digit multiplizieren
         _ mov %i0,%o0
          sub %i2,4,%i2
          ld [%i2],%o1          # nächstes dest-digit
          addcc %l0,%o0,%o0     # und bisherigen Carry addieren
          addx %g0,%g1,%l0      # High-Digit gibt neuen Carry
          subcc %o1,%o0,%o1     # davon das Low-Digit subtrahieren
          addx %g0,%l0,%l0
          subcc %i3,1,%i3
          bne Ll90
         _ st %o1,[%i2]         # dest-Digit ablegen
        mov %l0,%i0             # letzter Carry
        ret
       _ restore
#else
        save %sp,-96,%sp
        mov 0,%l0               # Carry
#ifndef sparcv8
        sra %i0,31,%l1          # 0 falls %i0>=0, -1 falls %i0<0
#endif
Ll90:     ld [%i1-4],%o1        # nächstes source-Digit
          sub %i1,4,%i1
          # mit digit multiplizieren: %i0 * %o1 -> %o2|%o0
#ifdef sparcv8
          umul    %i0,%o1,%o0
          rd      %y,%o2
#else
          mov     %o1,%y
          and     %o1,%l1,%o3   # Wartetakt!
          andcc   %g0,%g0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%i0,%o2
          mulscc  %o2,%g0,%o2
          add     %o2,%o3,%o2   # %o3 = (0 falls %i0>=0, %o1 falls %i0<0)
          rd      %y,%o0
#endif
          sub %i2,4,%i2
          ld [%i2],%o1          # nächstes dest-digit
          addcc %l0,%o0,%o0     # und bisherigen Carry addieren
          addx %g0,%o2,%l0      # High-Digit gibt neuen Carry
          subcc %o1,%o0,%o1     # davon das Low-Digit subtrahieren
          addx %g0,%l0,%l0
          subcc %i3,1,%i3
          bne Ll90
         _ st %o1,[%i2]         # dest-Digit ablegen
        mov %l0,%i0             # letzter Carry
        ret
       _ restore
#endif

# extern uintD divu_loop_up (uintD digit, uintD* ptr, uintC len);
C(divu_loop_up:) # Input in %i0,%i1,%i2, Output in %i0
        save %sp,-96,%sp
        andcc %i2,%i2,%g0
        be Ll92
       _ mov 0,%g1                 # Rest
Ll91:     mov %g1,%o0              # Rest als High-Digit
          ld [%i1],%o1             # nächstes Digit als Low-Digit
          call C(divu_6432_3232_)  # zusammen durch digit dividieren
         _ mov %i0,%o2
          st %o0,[%i1]             # Quotient ablegen, Rest in %g1
          subcc %i2,1,%i2
          bne Ll91
         _ add %i1,4,%i1
Ll92:   mov %g1,%i0                # Rest als Ergebnis
        ret
       _ restore

# extern uintD divucopy_loop_up (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
C(divucopy_loop_up:) # Input in %i0,%i1,%i2,%i3, Output in %i0
        save %sp,-96,%sp
        andcc %i3,%i3,%g0
        be Ll94
       _ mov 0,%g1                 # Rest
Ll93:     mov %g1,%o0              # Rest als High-Digit
          ld [%i1],%o1             # nächstes Digit als Low-Digit
          call C(divu_6432_3232_)  # zusammen durch digit dividieren
         _ mov %i0,%o2
          st %o0,[%i2]             # Quotient ablegen, Rest in %g1
          add %i1,4,%i1
          subcc %i3,1,%i3
          bne Ll93
         _ add %i2,4,%i2
Ll94:   mov %g1,%i0                # Rest als Ergebnis
        ret
       _ restore

#endif

