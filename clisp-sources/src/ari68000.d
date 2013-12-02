# Externe Routinen zu ARILEV1.D
# Compiler: CC oder GNU-C auf SUN3 oder AMIGA
# Parameter-Übergabe:
#   auf dem Stack: sp@(4), sp@(8), ... (.W-Größen belegen 4 Byte!),
#   Rückgabewert in d0.
# Register a0-a1,d0-d1 frei verwendbar,
# Register a2-a4,d2-d7 müssen gerettet werden.
# Einstellungen: intCsize=16, intDsize=16.

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

           .text

           .globl _copy_loop_up,_copy_loop_down,_fill_loop_up,_fill_loop_down
           .globl _clear_loop_up,_clear_loop_down
           .globl _or_loop_up,_xor_loop_up,_and_loop_up,_eqv_loop_up
           .globl _nand_loop_up,_nor_loop_up,_andc2_loop_up,_orc2_loop_up
           .globl _not_loop_up
           .globl _and_test_loop_up,_test_loop_up,_compare_loop_up
           .globl _add_loop_down,_addto_loop_down,_inc_loop_down
           .globl _sub_loop_down,_subx_loop_down,_subfrom_loop_down,_dec_loop_down
           .globl _neg_loop_down
           .globl _shift1left_loop_down,_shiftleft_loop_down,_shiftleftcopy_loop_down
           .globl _shift1right_loop_up,_shiftright_loop_up,_shiftrightsigned_loop_up,_shiftrightcopy_loop_up
           .globl _mulusmall_loop_down,_mulu_loop_down,_muluadd_loop_down,_mulusub_loop_down
           .globl _divu_loop_up,_divucopy_loop_up

#ifndef __GNUC__ /* mit GNU-C machen wir mulu32() als Macro, der inline multipliziert */
           .globl _mulu32_
! extern struct { uint32 lo; uint32 hi; } mulu32_ (uint32 arg1, uint32 arg2);
! 2^32*hi+lo := arg1*arg2.
mulu32_:   ! Input in d0,d1, Output in d0,mulu32_high
           movel sp@(4),d0
           movel sp@(8),d1
           movel d2,a0
           movel d3,a1
           movel d4,sp@-
           ! d0.L = 2^16*a+b, d1.L = 2^16*c+d -> Produkt
           ! (2^16*a+b)*(2^16*c+d) = 2^32*a*c + 2^16*(a*d+b*c) + b*d
           movel d0,d2
           swap d2      ! d2.W = a
           movel d1,d3
           swap d1      ! d1.W = c
           movel d1,d4
           mulu d2,d1   ! d1.L = a*c
           mulu d3,d2   ! d2.L = a*d
           mulu d0,d4   ! d4.L = b*c
           mulu d3,d0   ! d0.L = b*d
           clrl d3      ! Hilfsregister für Zero-Extend
           swap d2
           movew d2,d3
           addl d3,d1   ! high16(a*d) zu d1.L addieren
           swap d4
           movew d4,d3
           addl d3,d1   ! high16(b*c) zu d1.L addieren
           clrw d2
           addl d2,d0   ! 2^16*low16(a*d) zu d0.L addieren
           bccs 1f
           addql #1,d1
    1:     clrw d4
           addl d4,d0   ! 2^16*low16(b*c) zu d0.L addieren
           bccs 2f
           addql #1,d1
    2:     ! d0.L = lo, d1.L = hi fertig.
           movel d1,(_mulu32_high) ! Adressierung?? Deklaration??
           movel sp@+,d4
           movel a1,d3
           movel a0,d2
           rts
#endif

| extern uintD* copy_loop_up (uintD* sourceptr, uintD* destptr, uintC count);
_copy_loop_up: | Input in a0,a1,d0.W, Output in d0
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movew a0@+,a1@+
    2:       dbra d0,1b
           movel a1,d0
           rts

| extern uintD* copy_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
_copy_loop_down: | Input in a0,a1,d0.W, Output in d0
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movew a0@-,a1@-
    2:       dbra d0,1b
           movel a1,d0
           rts

| extern uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler);
_fill_loop_up: | Input in a0,d0.W,d1.W, Output in d0
           movel sp@(4),a0
           movew sp@(8+2),d0
           movew sp@(12+2),d1
           bras 2f
    1:       movew d1,a0@+
    2:       dbra d0,1b
           movel a0,d0
           rts

| extern uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler);
_fill_loop_down: | Input in a0,d0.W,d1.W, Output in d0
           movel sp@(4),a0
           movew sp@(8+2),d0
           movew sp@(12+2),d1
           bras 2f
    1:       movew d1,a0@-
    2:       dbra d0,1b
           movel a0,d0
           rts

| extern uintD* clear_loop_up (uintD* destptr, uintC count);
_clear_loop_up: | Input in a0,d0.W, verändert d1, Output in d0
           movel sp@(4),a0
           movew sp@(8+2),d0
           moveq #0,d1
           bras 2f
    1:       movew d1,a0@+
    2:       dbra d0,1b
           movel a0,d0
           rts

| extern uintD* clear_loop_down (uintD* destptr, uintC count);
_clear_loop_down: | Input in a0,d0.W, verändert d1, Output in d0
           movel sp@(4),a0
           movew sp@(8+2),d0
           moveq #0,d1
           bras 2f
    1:       movew d1,a0@-
    2:       dbra d0,1b
           movel a0,d0
           rts

| extern void or_loop_up (uintD* xptr, uintD* yptr, uintC count);
_or_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movew a1@+,d1
             orw d1,a0@+
    2:       dbra d0,1b
           rts

| extern void xor_loop_up (uintD* xptr, uintD* yptr, uintC count);
_xor_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movew a1@+,d1
             eorw d1,a0@+
    2:       dbra d0,1b
           rts

| extern void and_loop_up (uintD* xptr, uintD* yptr, uintC count);
_and_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movew a1@+,d1
             andw d1,a0@+
    2:       dbra d0,1b
           rts

| extern void eqv_loop_up (uintD* xptr, uintD* yptr, uintC count);
_eqv_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movew a1@+,d1
             eorw d1,a0@
             notw a0@+
    2:       dbra d0,1b
           rts

| extern void nand_loop_up (uintD* xptr, uintD* yptr, uintC count);
_nand_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movew a1@+,d1
             andw d1,a0@
             notw a0@+
    2:       dbra d0,1b
           rts

| extern void nor_loop_up (uintD* xptr, uintD* yptr, uintC count);
_nor_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movew a1@+,d1
             orw d1,a0@
             notw a0@+
    2:       dbra d0,1b
           rts

| extern void andc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
_andc2_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movew a1@+,d1
             notw d1
             andw d1,a0@+
    2:       dbra d0,1b
           rts

| extern void orc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
_orc2_loop_up: | Input in a0,a1,d0.W, verändert d1
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movew a1@+,d1
             notw d1
             orw d1,a0@+
    2:       dbra d0,1b
           rts

| extern void not_loop_up (uintD* xptr, uintC count);
_not_loop_up: | Input in a0,d0.W
           movel sp@(4),a0
           movew sp@(8+2),d0
           bras 2f
    1:       notw a0@+
    2:       dbra d0,1b
           rts

| extern bool and_test_loop_up (uintD* xptr, uintD* yptr, uintC count);
_and_test_loop_up: | Input in a0,a1,d0.W, verändert d1, Output in d0.W=d0.L
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       movew a0@+,d1
             andw a1@+,d1
             bnes 3f
    2:       dbra d0,1b
           clrl d0
           rts
    3:     moveq #1,d0
           rts

| extern bool test_loop_up (uintD* ptr, uintC count);
_test_loop_up: | Input in a0,d0.W, Output in d0.W=d0.L
           movel sp@(4),a0
           movew sp@(8+2),d0
           bras 2f
    1:       tstw a0@+
             bnes 3f
    2:       dbra d0,1b
           clrl d0
           rts
    3:     moveq #1,d0
           rts

| extern signean compare_loop_up (uintD* xptr, uintD* yptr, uintC count);
_compare_loop_up: | Input in a0,a1,d0.W, Output in d0.W=d0.L
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           bras 2f
    1:       cmpmw a1@+,a0@+
             bnes 3f
    2:       dbra d0,1b
           clrl d0
           rts
    3:     bcss 4f
           moveq #1,d0
           rts
    4:     moveq #-1,d0
           rts

| extern uintD add_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
_add_loop_down: | Input in a0,a1,a2,d0.W, verändert d1,d2, Output in d0.W
           moveml a2/d2,sp@-
           movel sp@(8+4),a0
           movel sp@(8+8),a1
           movel sp@(8+12),a2
           movew sp@(8+16+2),d0
           andb #0x0e,ccr   | X-Bit löschen
           bras 2f
    1:       movew a0@-,d1
             movew a1@-,d2
             addxw d2,d1
             movew d1,a2@-
    2:       dbra d0,1b
           subxw d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           moveml sp@+,a2/d2
           rts

| extern uintD addto_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
_addto_loop_down: | Input in a0,a1,d0.W, Output in d0.W
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           andb #0x0e,ccr   | X-Bit löschen
           bras 2f
    1:       addxw a0@-,a1@-
    2:       dbra d0,1b
           subxw d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           rts

| extern uintD inc_loop_down (uintD* ptr, uintC count);
_inc_loop_down: | Input in a0,d0.W, Output in d0.W
           movel sp@(4),a0
           movew sp@(8+2),d0
           dbra d0,1f          | simuliere gesetzten Carry
                              | d0.W=-1 für Übertrag
           rts
    1:       addqw #1,a0@-
             dbcc d0,1b       | kein Carry -> Schleife abbrechen
           subxw d0,d0       | kein Carry -> d0.W=0, sonst d0.W=-1 für Übertrag
           rts

| extern uintD sub_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
_sub_loop_down: | Input in a0,a1,a2,d0.W, verändert d1,d2, Output in d0.W
           moveml a2/d2,sp@-
           movel sp@(8+4),a0
           movel sp@(8+8),a1
           movel sp@(8+12),a2
           movew sp@(8+16+2),d0
           andb #0x0e,ccr   | X-Bit löschen
           bras 2f
    1:       movew a0@-,d1
             movew a1@-,d2
             subxw d2,d1
             movew d1,a2@-
    2:       dbra d0,1b
           subxw d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           moveml sp@+,a2/d2
           rts

| extern uintD subx_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
_subx_loop_down: | Input in a0,a1,a2,d0.W,d1.W, verändert d2, Output in d0.W
           moveml a2/d2,sp@-
           movel sp@(8+4),a0
           movel sp@(8+8),a1
           movel sp@(8+12),a2
           movew sp@(8+16+2),d0
           movew sp@(8+20+2),d1
           roxrw #1,d1      | X-Bit initialisieren
           bras 2f
    1:       movew a0@-,d1
             movew a1@-,d2
             subxw d2,d1
             movew d1,a2@-
    2:       dbra d0,1b
           subxw d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           moveml sp@+,a2/d2
           rts

| extern uintD subfrom_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
_subfrom_loop_down: | Input in a0,a1,d0.W, Output in d0.W
           movel sp@(4),a0
           movel sp@(8),a1
           movew sp@(12+2),d0
           andb #0x0e,ccr   | X-Bit löschen
           bras 2f
    1:       subxw a0@-,a1@-
    2:       dbra d0,1b
           subxw d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           rts

| extern uintD dec_loop_down (uintD* ptr, uintC count);
_dec_loop_down: | Input in a0,d0.W, Output in d0.W
           movel sp@(4),a0
           movew sp@(8+2),d0
           dbra d0,1f          | simuliere gesetzten Carry
                              | d0.W=-1 als Übertrag
           rts
    1:       subqw #1,a0@-
             dbcc d0,1b       | kein Carry -> Schleife abbrechen
           subxw d0,d0       | kein Carry -> d0.W=0, sonst d0.W=-1 als Übertrag
           rts

| extern uintD neg_loop_down (uintD* ptr, uintC count);
_neg_loop_down: | Input in a0,d0.W, Output in d0.W
           movel sp@(4),a0
           movew sp@(8+2),d0
           andb #0x0e,ccr   | X-Bit löschen
           bras 2f
    1:       negxw a0@-
    2:       dbra d0,1b
           subxw d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           rts

| extern uintD shift1left_loop_down (uintD* ptr, uintC count);
_shift1left_loop_down: | Input in a0,d0.W, Output in d0.W
           movel sp@(4),a0
           movew sp@(8+2),d0
           andb #0x0e,ccr   | X-Bit löschen
           bras 2f
    1:       roxlw a0@-     | Digit a0@- um 1 Bit links schieben, X-Bit als Buffer
    2:       dbra d0,1b
           subxw d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           rts

| extern uintD shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry);
_shiftleft_loop_down: | Input in a0,d0.W,d1.W,d2.W, Output in d0.W
           moveml d2-d3,sp@-
           movel sp@(8+4),a0
           movew sp@(8+8+2),d0
           movew sp@(8+12+2),d1
           movew sp@(8+16+2),d2
           | a0 = ptr, d0.W = count, d1.W = i,
           | d2.W = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           bras 2f
    1:       clrl d3
             movew a0@-,d3  | d3.L = d3.W = neues Digit
             lsll d1,d3      | um i Bits nach links schieben
             orw d2,d3       | d3 enthält die letzten 16+i Bits
             movew d3,a0@   | 16 Bits ablegen
             swap d3
             movew d3,d2     | neuen Übertrag bilden
    2:       dbra d0,1b        | Schleife d0.W mal durchlaufen
           movew d2,d0
           moveml sp@+,d2-d3
           rts

| extern uintD shiftleftcopy_loop_down (uintD* sourceptr, uintD* destptr, uintC count, uintC i);
_shiftleftcopy_loop_down: | Input in a0,a1,d0.W,d1.W, Output in d0.W
           moveml d2-d3,sp@-
           movel sp@(8+4),a0
           movel sp@(8+8),a1
           movew sp@(8+12+2),d0
           movew sp@(8+16+2),d1
           clrw d2
           | a0 = sourceptr, a1 = destptr, d0.W = count, d1.W = i,
           | d2.W = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           bras 2f
    1:       clrl d3
             movew a0@-,d3  | d3.L = d3.W = neues Digit
             lsll d1,d3      | um i Bits nach links schieben
             orw d2,d3       | d3 enthält die letzten 16+i Bits
             movew d3,a1@-  | 16 Bits ablegen
             swap d3
             movew d3,d2     | neuen Übertrag bilden
    2:       dbra d0,1b        | Schleife d0.W mal durchlaufen
           movew d2,d0
           moveml sp@+,d2-d3
           rts

| extern uintD shift1right_loop_up (uintD* ptr, uintC count, uintD carry);
_shift1right_loop_up: | Input in a0,d0.W,d1.W, Output in d0.W
           movel sp@(4),a0
           movew sp@(8+2),d0
           movew sp@(12+2),d1
           roxrw #1,d1       | X-Bit löschen oder setzen, je nach d1.W
           bras 2f
    1:       roxrw a0@+     | Digit a0@+ um 1 Bit rechts schieben, X-Bit als Buffer
    2:       dbra d0,1b
           subxw d0,d0       | -1 falls X gesetzt, 0 falls X gelöscht
           rts

| extern uintD shiftright_loop_up (uintD* ptr, uintC count, uintC i);
_shiftright_loop_up: | Input in a0,d0.W,d1.W, Output in d0.W
           moveml d2-d3,sp@-
           movel sp@(8+4),a0
           movew sp@(8+8+2),d0
           movew sp@(8+12+2),d1
           | a0 = ptr, d0.W = count, d1.W = i,
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           clrl d2           | Übertrag = 0
           bras 2f
    1:       | a0 = Aufwärtszähler Adresse, d0.W = Herabzähler, d1.W = i,
             | d2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             | d3.L = Schiebe-Akku
             clrl d3
             movew a0@,d3   | neue Daten
             swap d3          | nach Bit 31..16(d3), d3.W = 0
             lsrl d1,d3      | Bits 31-i..16-i(d3) sind die neuen Daten
             orl d3,d2       | Bits 31..16-i(d3) sind die bisherigen Daten
             swap d2          | untere 16 Bit ergeben neuen Übertrag,
             movew d2,a0@+  | obere 16 Bit werden abgespeichert
             clrw d2         | d2.L = neuer Übertrag
    2:       dbra d0,1b        | Schleife d0.W mal durchlaufen
           swap d2
           movew d2,d0
           moveml sp@+,d2-d3
           rts

| extern uintD shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i);
_shiftrightsigned_loop_up: | Input in a0,d0.W,d1.W, Output in d0.W
           moveml d2-d3,sp@-
           movel sp@(8+4),a0
           movew sp@(8+8+2),d0
           movew sp@(8+12+2),d1
           | a0 = ptr, d0.W = count, d1.W = i,
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           movew a0@,d2     | erstes Digit
           extl d2           | Vorzeichenbit nach Bit 31..16(d2)
           clrw d2           | Rest von d2.L löschen
           lsrl d1,d2        | d2.W enthält in seinen oberen i Bits das Vorzeichen
           swap d2            | Übertrag mit i Vorzeichenbits initialisiert
           bras 2f
    1:       | a0 = Aufwärtszähler Adresse, d0.W = Herabzähler, d1.W = i,
             | d2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             | d3.L = Schiebe-Akku
             clrl d3
             movew a0@,d3   | neue Daten
             swap d3          | nach Bit 31..16(d3), d3.W = 0
             lsrl d1,d3      | Bits 31-i..16-i(d3) sind die neuen Daten
             orl d3,d2       | Bits 31..16-i(d3) sind die bisherigen Daten
             swap d2          | untere 16 Bit ergeben neuen Übertrag,
             movew d2,a0@+  | obere 16 Bit werden abgespeichert
    2:       clrw d2         | d2.L = neuer Übertrag
             dbra d0,1b        | Schleife d0.W mal durchlaufen
           swap d2
           movew d2,d0
           moveml sp@+,d2-d3
           rts

| extern uintD shiftrightcopy_loop_up (uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry);
_shiftrightcopy_loop_up: | Input in a0,a1,d0.W,d1.W,d2.W, Output in d0.W
           moveml d2-d3,sp@-
           movel sp@(8+4),a0
           movel sp@(8+8),a1
           movew sp@(8+12+2),d0
           movew sp@(8+16+2),d1
           movew sp@(8+20+2),d2
           | a0 = ptr, d0.W = count, d1.W = i,
           | d2.L = Schiebe-Übertrag (i Bits), d3.L = Schiebe-Akku
           swap d2            | carry nach d2.HW
           clrw d2           | Rest von d2.L löschen
           lsrl d1,d2        | d2.W enthält in seinen oberen i Bits das Vorzeichen
           swap d2            | Übertrag mit i Vorzeichenbits initialisiert
           bras 2f
    1:       | a0,a1 = Aufwärtszähler Adresse, d0.W = Herabzähler, d1.W = i,
             | d2.L = Schiebe-Übertrag (obere i Bits, restliche 32-i Bits sind 0)
             | d3.L = Schiebe-Akku
             clrl d3
             movew a0@+,d3  | neue Daten
             swap d3          | nach Bit 31..16(d3), d3.W = 0
             lsrl d1,d3      | Bits 31-i..16-i(d3) sind die neuen Daten
             orl d3,d2       | Bits 31..16-i(d3) sind die bisherigen Daten
             swap d2          | untere 16 Bit ergeben neuen Übertrag,
             movew d2,a1@+  | obere 16 Bit werden abgespeichert
    2:       clrw d2         | d2.L = neuer Übertrag
             dbra d0,1b        | Schleife d0.W mal durchlaufen
           swap d2
           movew d2,d0
           moveml sp@+,d2-d3
           rts

| extern uintD mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit);
_mulusmall_loop_down: # Input in d0.W,a0,d1.W,d2.W, Output in d0.W
           moveml d2-d3,sp@-
           movew sp@(8+4+2),d0
           movel sp@(8+8),a0
           movew sp@(8+12+2),d1
           movew sp@(8+16+2),d2
           extl d2           | carry
           bras 2f
    1:       movew a0@-,d3  | nächstes Digit
             mulu d0,d3       | mit digit multiplizieren
             addl d3,d2      | und zum bisherigen Carry addieren. Kein Überlauf!
             movew d2,a0@   | Low-Digit ablegen
             clrw d2
             swap d2          | High-Digit gibt neuen Carry
    2:       dbra d1,1b
           movew d2,d0       | letzter Carry
           moveml sp@+,d2-d3
           rts

| extern void mulu_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
_mulu_loop_down: | Input in d0.W,a0,a1,d1.W
           moveml d2-d3,sp@-
           movew sp@(8+4+2),d0
           movel sp@(8+8),a0
           movel sp@(8+12),a1
           movew sp@(8+16+2),d1
           clrl d2           | carry
           bras 2f
    1:       movew a0@-,d3  | nächstes Digit
             mulu d0,d3       | mit digit multiplizieren
             addl d3,d2      | und zum bisherigen Carry addieren
             movew d2,a1@-  | Low-Digit ablegen
             clrw d2
             swap d2          | High-Digit gibt neuen Carry
    2:       dbra d1,1b
           movew d2,a1@-    | letzten Carry ablegen
           moveml sp@+,d2-d3
           rts

| extern uintD muluadd_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
_muluadd_loop_down: | Input in d0.W,a0,a1,d1.W, benutzt d2,d3,d4, Output in d0.W
#if 1
           moveml d2-d3,sp@-
           movew sp@(8+4+2),d0
           movel sp@(8+8),a0
           movel sp@(8+12),a1
           movew sp@(8+16+2),d1
           subl d2,d2        | carry := 0, X-Bit löschen, d2.HW stets =0
           bras 2f
    1:       movew a0@-,d3  | nächstes Digit
             mulu d0,d3       | mit digit multiplizieren
             addxl d2,d3     | und bisherigen Carry und X-Bit addieren
             addw d3,a1@-   | Low-Digit zum dest-Digit addieren, X als Übertrag
             swap d3
             movew d3,d2     | High-Digit gibt neuen Carry
    2:       dbra d1,1b
           movew d2,d0       | letzten Carry und
           swap d2            | 0.W und
           addxw d2,d0       | letztes X-Bit addieren
           moveml sp@+,d2-d3
           rts
#else
           moveml d2-d4,sp@-
           movew sp@(12+4+2),d0
           movel sp@(12+8),a0
           movel sp@(12+12),a1
           movew sp@(12+16+2),d1
           clrl d2           | carry
           clrl d4           | d4.HW stets =0
           bras 2f
    1:       movew a0@-,d3  | nächstes Digit
             mulu d0,d3       | mit digit multiplizieren
             addl d3,d2      | und zum bisherigen Carry addieren
             movew a1@-,d4  | nächstes dest-Digit
             addl d4,d2      | dazuaddieren
             movew d2,a1@   | Low-Digit ablegen
             clrw d2
             swap d2          | High-Digit gibt neuen Carry
    2:       dbra d1,1b
           movew d2,d0       | letzten Carry als Ergebnis
           moveml sp@+,d2-d4
           rts
#endif

| extern uintD mulusub_loop_down (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
_mulusub_loop_down: | Input in d0.W,a0,a1,d1.W, benutzt d2,d3,d4, Output in d0.W
           moveml d2-d3,sp@-
           movew sp@(8+4+2),d0
           movel sp@(8+8),a0
           movel sp@(8+12),a1
           movew sp@(8+16+2),d1
           subl d2,d2        | carry := 0, X-Bit löschen, d2.HW stets =0
           bras 2f
    1:       movew a0@-,d3  | nächstes Digit
             mulu d0,d3       | mit digit multiplizieren
             addxl d2,d3     | und bisherigen Carry und X-Bit addieren
             subw d3,a1@-   | Low-Digit vom dest-Digit subtrahieren, X als Übertrag
             swap d3
             movew d3,d2     | High-Digit gibt neuen Carry
    2:       dbra d1,1b
           clrw d0
           addxw d2,d0       | letzter Carry und letztes X-Bit
           moveml sp@+,d2-d3
           rts

| extern uintD divu_loop_up (uintD digit, uintD* ptr, uintC len);
_divu_loop_up: # Input in d0.W,a0,d1.W, Output in d0.W
           movel d2,sp@-
           movew sp@(4+4+2),d0
           movel sp@(4+8),a0
           movew sp@(4+12+2),d1
           clrl d2           | Rest d2.HW := 0
           bras 2f
    1:       movew a0@,d2   | nächst-niedriges Digit mit Rest kombinieren
             divu d0,d2       | und durch digit dividieren
             movew d2,a0@+  | Quotient ablegen, Rest in d2.HW
    2:       dbra d1,1b
           swap d2
           movew d2,d0       | Rest
           movel sp@+,d2
           rts

| extern uintD divucopy_loop_up (uintD digit, uintD* sourceptr, uintD* destptr, uintC len);
_divucopy_loop_up: # Input in d0.W,a0,a1,d1.W, Output in d0.W
           movel d2,sp@-
           movew sp@(4+4+2),d0
           movel sp@(4+8),a0
           movel sp@(4+12),a1
           movew sp@(4+16+2),d1
           clrl d2           | Rest d2.HW := 0
           bras 2f
    1:       movew a0@+,d2  | nächst-niedriges Digit mit Rest kombinieren
             divu d0,d2       | und durch digit dividieren
             movew d2,a1@+  | Quotient ablegen, Rest in d2.HW
    2:       dbra d1,1b
           swap d2
           movew d2,d0       | Rest
           movel sp@+,d2
           rts

#endif

