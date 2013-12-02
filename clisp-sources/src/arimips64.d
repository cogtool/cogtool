# Externe Routinen zu ARILEV1.D
# Prozessor: MIPS 64-bit
# Endianness: irrelevant
# Compiler: GNU-C oder ...
# Parameter-Übergabe: in Registern $4,$5,$6,$7, und auf dem Stack 16($sp),...
# Rückgabewert: in Register $2
# Einstellungen: intCsize=32, intDsize=32.
# Besonderheiten: Nach jedem Ladebefehl ein Wartetakt nötig, bevor der
#   geholte Wert benutzt werden darf.

#ifdef INCLUDED_FROM_C

  #define COPY_LOOPS
  #define FILL_LOOPS
  #define CLEAR_LOOPS
  #define LOG_LOOPS
  #define TEST_LOOPS
  #define ADDSUB_LOOPS

#else

        .text

        .globl copy_loop_up
        .globl copy_loop_down
        .globl fill_loop_up
        .globl fill_loop_down
        .globl clear_loop_up
        .globl clear_loop_down
        .globl or_loop_up
        .globl xor_loop_up
        .globl and_loop_up
        .globl eqv_loop_up
        .globl nand_loop_up
        .globl nor_loop_up
        .globl andc2_loop_up
        .globl orc2_loop_up
        .globl not_loop_up
        .globl and_test_loop_up
        .globl test_loop_up
        .globl compare_loop_up
        .globl add_loop_down
        .globl addto_loop_down
        .globl inc_loop_down
        .globl sub_loop_down
        .globl subx_loop_down
        .globl subfrom_loop_down
        .globl dec_loop_down
        .globl neg_loop_down

#ifndef __GNUC__ /* mit GNU-C machen wir mulu32() als Macro, der inline multipliziert */

# extern struct { uint32 lo; uint32 hi; } mulu32_ (uint32 arg1, uint32 arg2);
# 2^32*hi+lo := arg1*arg2.
        .globl mulu32_
        .align 2
        .ent mulu32_ # Input in $4,$5, Output in $2,mulu32_high
mulu32_:
        multu $5,$4             # arg1 * arg2
        mfhi $6                 # hi
        mflo $2                 # lo
        sw $6,mulu32_high       # hi abspeichern # Adressierung?? Deklaration??
        j $31                   # return
        .end mulu32_

#endif

# extern uintD* copy_loop_up (uintD* sourceptr, uintD* destptr, uintC count);
        .align 2
        .ent copy_loop_up # Input in $4,$5,$6, Output in $2
colu1:    lw $12,($4)           # d = *sourceptr
          daddu $4,4            # sourceptr++
          sw $12,($5)           # *destptr = d
          daddu $5,4            # destptr++
          subu $6,1             # count--
copy_loop_up:
          bnez $6,colu1         # until (count==0)
        move $2,$5              # destptr
        j $31                   # return
        .end copy_loop_up

# extern uintD* copy_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
        .align 2
        .ent copy_loop_down # Input in $4,$5,$6, Output in $2
cold1:    dsubu $4,4            # sourceptr--
          lw $12,($4)           # d = *sourceptr
          dsubu $5,4            # destptr--
          sw $12,($5)           # *destptr = d
          subu $6,1             # count--
copy_loop_down:
          bnez $6,cold1         # until (count==0)
        move $2,$5              # destptr
        j $31                   # return
        .end copy_loop_down

# extern uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler);
        .align 2
        .ent fill_loop_up # Input in $4,$5,$6, Output in $2
flu1:     sw $6,($4)            # *destptr = filler
          daddu $4,4            # destptr++
          subu $5,1             # count--
fill_loop_up:
          bnez $5,flu1          # until (count==0)
        move $2,$4              # destptr
        j $31                   # return
        .end fill_loop_up

# extern uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler);
        .align 2
        .ent fill_loop_down # Input in $4,$5,$6, Output in $2
fld1:     dsubu $4,4            # destptr--
          sw $6,($4)            # *destptr = filler
          subu $5,1             # count--
fill_loop_down:
          bnez $5,fld1          # until (count==0)
        move $2,$4              # destptr
        j $31                   # return
        .end fill_loop_down

# extern uintD* clear_loop_up (uintD* destptr, uintC count);
        .align 2
        .ent clear_loop_up # Input in $4,$5, Output in $2
cllu1:    sw $0,($4)            # *destptr = 0
          daddu $4,4            # destptr++
          subu $5,1             # count--
clear_loop_up:
          bnez $5,cllu1         # until (count==0)
        move $2,$4              # destptr
        j $31                   # return
        .end clear_loop_up

# extern uintD* clear_loop_down (uintD* destptr, uintC count);
        .align 2
        .ent clear_loop_down # Input in $4,$5, Output in $2
clld1:    dsubu $4,4            # destptr--
          sw $0,($4)            # *destptr = 0
          subu $5,1             # count--
clear_loop_down:
          bnez $5,clld1         # until (count==0)
        move $2,$4              # destptr
        j $31                   # return
        .end clear_loop_down

# extern void or_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent or_loop_up # Input in $4,$5,$6
olu1:     lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          daddu $5,4            # yptr++
          or $12,$13            # x |= y
          sw $12,($4)           # *xptr = x
          daddu $4,4            # xptr++
          subu $6,1             # count--
or_loop_up:
          bnez $6,olu1          # until (count==0)
        j $31                   # return
        .end or_loop_up

# extern void xor_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent xor_loop_up # Input in $4,$5,$6
xlu1:     lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          daddu $5,4            # yptr++
          xor $12,$13           # x ^= y
          sw $12,($4)           # *xptr = x
          daddu $4,4            # xptr++
          subu $6,1             # count--
xor_loop_up:
          bnez $6,xlu1          # until (count==0)
        j $31                   # return
        .end xor_loop_up

# extern void and_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent and_loop_up # Input in $4,$5,$6
alu1:     lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          daddu $5,4            # yptr++
          and $12,$13           # x &= y
          sw $12,($4)           # *xptr = x
          daddu $4,4            # xptr++
          subu $6,1             # count--
and_loop_up:
          bnez $6,alu1          # until (count==0)
        j $31                   # return
        .end and_loop_up

# extern void eqv_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent eqv_loop_up # Input in $4,$5,$6
nxlu1:    lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          daddu $5,4            # yptr++
          xor $12,$13           # x ^= y
          nor $12,$0            # x = ~x
          sw $12,($4)           # *xptr = x
          daddu $4,4            # xptr++
          subu $6,1             # count--
eqv_loop_up:
          bnez $6,nxlu1         # until (count==0)
        j $31                   # return
        .end eqv_loop_up

# extern void nand_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent nand_loop_up # Input in $4,$5,$6
nalu1:    lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          daddu $5,4            # yptr++
          and $12,$13           # x &= y        # Gibt es 'nand $12,$13' ??
          nor $12,$0            # x = ~x
          sw $12,($4)           # *xptr = x
          daddu $4,4            # xptr++
          subu $6,1             # count--
nand_loop_up:
          bnez $6,nalu1         # until (count==0)
        j $31                   # return
        .end nand_loop_up

# extern void nor_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent nor_loop_up # Input in $4,$5,$6
nolu1:    lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          daddu $5,4            # yptr++
          nor $12,$13           # x = ~(x|y)
          sw $12,($4)           # *xptr = x
          daddu $4,4            # xptr++
          subu $6,1             # count--
nor_loop_up:
          bnez $6,nolu1         # until (count==0)
        j $31                   # return
        .end nor_loop_up

# extern void andc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent andc2_loop_up # Input in $4,$5,$6
aclu1:    lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          daddu $5,4            # yptr++
          nor $13,$0            # y = ~y
          and $12,$13           # x &= y
          sw $12,($4)           # *xptr = x
          daddu $4,4            # xptr++
          subu $6,1             # count--
andc2_loop_up:
          bnez $6,aclu1         # until (count==0)
        j $31                   # return
        .end andc2_loop_up

# extern void orc2_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent orc2_loop_up # Input in $4,$5,$6
oclu1:    lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          daddu $5,4            # yptr++
          nor $13,$0            # y = ~y
          or $12,$13            # x |= y
          sw $12,($4)           # *xptr = x
          daddu $4,4            # xptr++
          subu $6,1             # count--
orc2_loop_up:
          bnez $6,oclu1         # until (count==0)
        j $31                   # return
        .end orc2_loop_up

# extern void not_loop_up (uintD* xptr, uintC count);
        .align 2
        .ent not_loop_up # Input in $4,$5
nlu1:     lw $12,($4)           # x = *xptr
          subu $5,1             # count--
          nor $12,$0            # x = ~x
          sw $12,($4)           # *xptr = x
          daddu $4,4            # xptr++
not_loop_up:
          bnez $5,nlu1          # until (count==0)
        j $31                   # return
        .end not_loop_up

# extern bool and_test_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent and_test_loop_up # Input in $4,$5,$6
atlu1:    lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          daddu $5,4            # yptr++
          and $12,$13           # x &= y
          bnez $12,atlu3        # if (x) ...
          daddu $4,4            # xptr++
          subu $6,1             # count--
and_test_loop_up:
          bnez $6,atlu1         # until (count==0)
        move $2,$0              # 0
        j $31                   # return
atlu3:  li $2,1                 # 1
        j $31                   # return
        .end and_test_loop_up

# extern bool test_loop_up (uintD* ptr, uintC count);
        .align 2
        .ent test_loop_up # Input in $4,$5
tlu1:     lw $12,($4)           # x = *ptr
          daddu $4,4            # ptr++
          bnez $12,tlu3
          subu $5,1             # count--
test_loop_up:
          bnez $5,tlu1          # until (count==0)
        move $2,$0              # 0
        j $31                   # return
tlu3:   li $2,1                 # 1
        j $31                   # return
        .end test_loop_up

# extern signean compare_loop_up (uintD* xptr, uintD* yptr, uintC count);
        .align 2
        .ent compare_loop_up # Input in $4,$5,$6
cmlu1:    lw $12,($4)           # x = *xptr
          lw $13,($5)           # y = *yptr
          daddu $5,4            # yptr++
          bne $12,$13,cmlu3     # if (!(x==y)) ...
          daddu $4,4            # xptr++
          subu $6,1             # count--
compare_loop_up:
          bnez $6,cmlu1         # until (count==0)
        move $2,$0              # 0
        j $31                   # return
cmlu3:  bltu $12,$13,cmlu4      # if (x<y) ...
        li $2,1                 # 1
        j $31                   # return
cmlu4:  li $2,-1                # -1
        j $31                   # return
        .end compare_loop_up

# extern uintD add_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
        .align 2
        .ent add_loop_down # Input in $4,$5,$6,$7, Output in $2
ald1:     # kein Carry
          dsubu $4,4            # sourceptr1--
          dsubu $5,4            # sourceptr2--
          lw $12,($4)           # source1 = *sourceptr1
          lw $13,($5)           # source2 = *sourceptr2
          dsubu $6,4            # destptr--
          addu $12,$13          # dest = source1 + source2
          sw $12,($6)           # *destptr = dest
          bltu $12,$13,ald4     # if (dest < source2) [also Carry] ...
ald2:
          subu $7,1             # count--
add_loop_down:
          bnez $7,ald1          # until (count==0)
        move $2,$0              # 0
        j $31                   # return
ald3:   # Hier Carry
          dsubu $4,4            # sourceptr1--
          dsubu $5,4            # sourceptr2--
          lw $12,($4)           # source1 = *sourceptr1
          lw $13,($5)           # source2 = *sourceptr2
          dsubu $6,4            # destptr--
          addu $12,$13          # dest = source1 + source2
          addu $12,1            #        + 1
          sw $12,($6)           # *destptr = dest
          bgtu $12,$13,ald2     # if (dest > source2) [also kein Carry] ...
ald4:     subu $7,1             # count--
          bnez $7,ald3          # until (count==0)
        li $2,1                 # 1
        j $31                   # return
        .end add_loop_down

# extern uintD addto_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
        .align 2
        .ent addto_loop_down # Input in $4,$5,$6, Output in $2
atld1:    # kein Carry
          dsubu $4,4            # sourceptr--
          dsubu $5,4            # destptr--
          lw $12,($4)           # source1 = *sourceptr
          lw $13,($5)           # source2 = *destptr
          subu $6,1             # count--
          addu $12,$13          # dest = source1 + source2
          sw $12,($5)           # *destptr = dest
          bltu $12,$13,atld4    # if (dest < source2) [also Carry] ...
addto_loop_down:
atld2:    bnez $6,atld1         # until (count==0)
        move $2,$0              # 0
        j $31                   # return
atld3:  # Hier Carry
          dsubu $4,4            # sourceptr--
          dsubu $5,4            # destptr--
          lw $12,($4)           # source1 = *sourceptr
          lw $13,($5)           # source2 = *destptr
          subu $6,1             # count--
          addu $12,$13          # dest = source1 + source2
          addu $12,1            #        + 1
          sw $12,($5)           # *destptr = dest
          bgtu $12,$13,atld2    # if (dest > source2) [also kein Carry] ...
atld4:    bnez $6,atld3         # until (count==0)
        li $2,1                 # 1
        j $31                   # return
        .end addto_loop_down

# extern uintD inc_loop_down (uintD* ptr, uintC count);
        .align 2
        .ent inc_loop_down # Input in $4,$5, Output in $2
ild1:     dsubu $4,4            # ptr--
          lw $12,($4)           # x = *ptr
          subu $5,1             # count--
          addu $12,1            # x++;
          sw $12,($4)           # *ptr = x
          bnez $12,ild3         # if (!(x==0)) ...
inc_loop_down:
          bnez $5,ild1          # until (count==0)
        li $2,1                 # 1
        j $31                   # return
ild3:   move $2,$0              # 0
        j $31                   # return
        .end inc_loop_down

# extern uintD sub_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count);
        .align 2
        .ent sub_loop_down # Input in $4,$5,$6,$7, Output in $2
sld1:     # kein Carry
          dsubu $4,4            # sourceptr1--
          dsubu $5,4            # sourceptr2--
          lw $12,($4)           # source1 = *sourceptr1
          lw $13,($5)           # source2 = *sourceptr2
          dsubu $6,4            # destptr--
          bltu $12,$13,sld2     # if (source1 < source2) [also Carry] ...
          subu $12,$13          # dest = source1 - source2
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
sub_loop_down:
          bnez $7,sld1          # until (count==0)
        move $2,$0              # 0
        j $31                   # return
sld2:     subu $12,$13          # dest = source1 - source2
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
          bnez $7,sld3          # until (count==0)
        li $2,-1                # -1
        j $31                   # return
sld3:   # Hier Carry
          dsubu $4,4            # sourceptr1--
          dsubu $5,4            # sourceptr2--
          lw $12,($4)           # source1 = *sourceptr1
          lw $13,($5)           # source2 = *sourceptr2
          dsubu $6,4            # destptr--
          bgtu $12,$13,sld4     # if (source1 > source2) [also kein Carry] ...
          subu $12,$13          # dest = source1 - source2
          subu $12,1            #        - 1
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
          bnez $7,sld3          # until (count==0)
        li $2,-1                # -1
        j $31                   # return
sld4:     subu $12,$13          # dest = source1 - source2
          subu $12,1            #        - 1
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
          bnez $7,sld1          # until (count==0)
        move $2,$0              # 0
        j $31                   # return
        .end sub_loop_down

# extern uintD subx_loop_down (uintD* sourceptr1, uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
        .align 2
        .ent subx_loop_down # Input in $4,$5,$6,$7, Output in $2
subx_loop_down:
        lw $12,16($sp)          # carry
        bnez $12,sxld5          # !(carry==0) ?
        b sxld2
sxld1:    # kein Carry
          dsubu $4,4            # sourceptr1--
          dsubu $5,4            # sourceptr2--
          lw $12,($4)           # source1 = *sourceptr1
          lw $13,($5)           # source2 = *sourceptr2
          dsubu $6,4            # destptr--
          bltu $12,$13,sxld3    # if (source1 < source2) [also Carry] ...
          subu $12,$13          # dest = source1 - source2
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
sxld2:    bnez $7,sxld1         # until (count==0)
        move $2,$0              # 0
        j $31                   # return
sxld3:    subu $12,$13          # dest = source1 - source2
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
          bnez $7,sxld4         # until (count==0)
        li $2,-1                # -1
        j $31                   # return
sxld4:  # Hier Carry
          dsubu $4,4            # sourceptr1--
          dsubu $5,4            # sourceptr2--
          lw $12,($4)           # source1 = *sourceptr1
          lw $13,($5)           # source2 = *sourceptr2
          dsubu $6,4            # destptr--
          bgtu $12,$13,sxld6    # if (source1 > source2) [also kein Carry] ...
          subu $12,$13          # dest = source1 - source2
          subu $12,1            #        - 1
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
sxld5:    bnez $7,sxld4         # until (count==0)
        li $2,-1                # -1
        j $31                   # return
sxld6:    subu $12,$13          # dest = source1 - source2
          subu $12,1            #        - 1
          sw $12,($6)           # *destptr = dest
          subu $7,1             # count--
          bnez $7,sxld1         # until (count==0)
        move $2,$0              # 0
        j $31                   # return
        .end subx_loop_down

# extern uintD subfrom_loop_down (uintD* sourceptr, uintD* destptr, uintC count);
        .align 2
        .ent subfrom_loop_down # Input in $4,$5,$6,$7, Output in $2
sfld1:    # kein Carry
          dsubu $4,4            # sourceptr--
          dsubu $5,4            # destptr--
          lw $12,($5)           # source1 = *destptr
          lw $13,($4)           # source2 = *sourceptr
          subu $6,1             # count--
          bltu $12,$13,sfld2    # if (source1 < source2) [also Carry] ...
          subu $12,$13          # dest = source1 - source2
          sw $12,($5)           # *destptr = dest
subfrom_loop_down:
          bnez $6,sfld1         # until (count==0)
        move $2,$0              # 0
        j $31                   # return
sfld2:    subu $12,$13          # dest = source1 - source2
          sw $12,($5)           # *destptr = dest
          bnez $6,sfld3         # until (count==0)
        li $2,-1                # -1
        j $31                   # return
sfld3:  # Hier Carry
          dsubu $4,4            # sourceptr--
          dsubu $5,4            # destptr--
          lw $12,($5)           # source1 = *destptr
          lw $13,($4)           # source2 = *sourceptr
          subu $6,1             # count--
          bgtu $12,$13,sfld4    # if (source1 > source2) [also kein Carry] ...
          subu $12,$13          # dest = source1 - source2
          subu $12,1            #        - 1
          sw $12,($5)           # *destptr = dest
          bnez $6,sfld3         # until (count==0)
        li $2,-1                # -1
        j $31                   # return
sfld4:    subu $12,$13          # dest = source1 - source2
          subu $12,1            #        - 1
          sw $12,($5)           # *destptr = dest
          bnez $6,sfld1         # until (count==0)
        move $2,$0              # 0
        j $31                   # return
        .end subfrom_loop_down

# extern uintD dec_loop_down (uintD* ptr, uintC count);
        .align 2
        .ent dec_loop_down # Input in $4,$5, Output in $2
dld1:     dsubu $4,4            # ptr--
          lw $12,($4)           # x = *ptr
          subu $5,1             # count--
          bnez $12,dld3         # if (!(x==0)) ...
          subu $12,1            # x--;
          sw $12,($4)           # *ptr = x
dec_loop_down:
          bnez $5,dld1          # until (count==0)
        li $2,-1                # -1
        j $31                   # return
dld3:   subu $12,1              # x--;
        sw $12,($4)             # *ptr = x
        move $2,$0              # 0
        j $31                   # return
        .end dec_loop_down

# extern uintD neg_loop_down (uintD* ptr, uintC count);
        .align 2
        .ent neg_loop_down # Input in $4,$5, Output in $2
        # erstes Digit /=0 suchen:
nld1:     dsubu $4,4            # ptr--
          lw $12,($4)           # x = *ptr
          subu $5,1             # count--
          bnez $12,nld3         # if (!(x==0)) ...
neg_loop_down:
          bnez $5,nld1          # until (count==0)
        move $2,$0              # 0
        j $31                   # return
nld3:   # erstes Digit /=0 gefunden, ab jetzt gibt's Carrys
        # 1 Digit negieren:
        subu $12,$0,$12         # x = -x
        sw $12,($4)             # *ptr = x
        # alle anderen Digits invertieren:
        b nld5
nld4:     dsubu $4,4            # xptr--
          lw $12,($4)           # x = *xptr
          subu $5,1             # count--
          nor $12,$0            # x = ~x
          sw $12,($4)           # *xptr = x
nld5:     bnez $5,nld4          # until (count==0)
        li $2,-1                # -1
        j $31                   # return
        .end neg_loop_down

#endif

