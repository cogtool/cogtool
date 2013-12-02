# Definitionen und portabler C-Code zu ARILEV1.D

# Kopierschleife:
# destptr = copy_loop_up(sourceptr,destptr,count);
# kopiert count (uintC>=0) Digits aufwärts von sourceptr nach destptr
# und liefert das neue destptr.
  local uintD* copy_loop_up (const uintD* sourceptr, uintD* destptr, uintC count);
  local uintD* copy_loop_up(sourceptr,destptr,count)
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC count;
    {
      until (count==0) {
        *destptr++ = *sourceptr++; count--;
      }
      return destptr;
    }

# Kopierschleife:
# destptr = copy_loop_down(sourceptr,destptr,count);
# kopiert count (uintC>=0) Digits abwärts von sourceptr nach destptr
# und liefert das neue destptr.
  local uintD* copy_loop_down (const uintD* sourceptr, uintD* destptr, uintC count);
  local uintD* copy_loop_down(sourceptr,destptr,count)
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC count;
    {
      until (count==0) {
        *--destptr = *--sourceptr; count--;
      }
      return destptr;
    }

# Füllschleife:
# destptr = fill_loop_up(destptr,count,filler);
# kopiert count (uintC>=0) mal das Digit filler aufwärts nach destptr
# und liefert das neue destptr.
  local uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler);
  local uintD* fill_loop_up(destptr,count,filler)
    var uintD* destptr;
    var uintC count;
    var uintD filler;
    {
      until (count==0) {
        *destptr++ = filler; count--;
      }
      return destptr;
    }

# Füllschleife:
# destptr = fill_loop_down(destptr,count,filler);
# kopiert count (uintC>=0) mal das Digit filler abwärts nach destptr
# und liefert das neue destptr.
  local uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler);
  local uintD* fill_loop_down(destptr,count,filler)
    var uintD* destptr;
    var uintC count;
    var uintD filler;
    {
      until (count==0) {
        *--destptr = filler; count--;
      }
      return destptr;
    }

# Lösch-Schleife:
# destptr = clear_loop_up(destptr,count);
# löscht count (uintC>=0) Digits aufwärts ab destptr
# und liefert das neue destptr.
  local uintD* clear_loop_up (uintD* destptr, uintC count);
  local uintD* clear_loop_up(destptr,count)
    var uintD* destptr;
    var uintC count;
    {
      until (count==0) {
        *destptr++ = 0; count--;
      }
      return destptr;
    }

# Lösch-Schleife:
# destptr = clear_loop_down(destptr,count);
# löscht count (uintC>=0) Digits abwärts ab destptr
# und liefert das neue destptr.
  local uintD* clear_loop_down (uintD* destptr, uintC count);
  local uintD* clear_loop_down(destptr,count)
    var uintD* destptr;
    var uintC count;
    {
      until (count==0) {
        *--destptr = 0; count--;
      }
      return destptr;
    }

# OR-Schleife:
# or_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch OR.
  local void or_loop_up (uintD* xptr, const uintD* yptr, uintC count);
  local void or_loop_up(xptr,yptr,count)
    var uintD* xptr;
    var const uintD* yptr;
    var uintC count;
    {
      until (count==0) {
        *xptr++ |= *yptr++; count--;
      }
    }

# XOR-Schleife:
# xor_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch XOR.
  local void xor_loop_up (uintD* xptr, const uintD* yptr, uintC count);
  local void xor_loop_up(xptr,yptr,count)
    var uintD* xptr;
    var const uintD* yptr;
    var uintC count;
    {
      until (count==0) {
        *xptr++ ^= *yptr++; count--;
      }
    }

# AND-Schleife:
# and_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch AND.
  local void and_loop_up (uintD* xptr, const uintD* yptr, uintC count);
  local void and_loop_up(xptr,yptr,count)
    var uintD* xptr;
    var const uintD* yptr;
    var uintC count;
    {
      until (count==0) {
        *xptr++ &= *yptr++; count--;
      }
    }

# EQV-Schleife:
# eqv_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch EQV (NOT XOR).
  local void eqv_loop_up (uintD* xptr, const uintD* yptr, uintC count);
  local void eqv_loop_up(xptr,yptr,count)
    var uintD* xptr;
    var const uintD* yptr;
    var uintC count;
    {
      until (count==0) {
        var uintD temp = ~ (*xptr ^ *yptr++);
        *xptr++ = temp;
        count--;
      }
    }

# NAND-Schleife:
# nand_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch NAND (NOT AND).
  local void nand_loop_up (uintD* xptr, const uintD* yptr, uintC count);
  local void nand_loop_up(xptr,yptr,count)
    var uintD* xptr;
    var const uintD* yptr;
    var uintC count;
    {
      until (count==0) {
        var uintD temp = ~ (*xptr & *yptr++);
        *xptr++ = temp;
        count--;
      }
    }

# NOR-Schleife:
# nor_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch NOR (NOT OR).
  local void nor_loop_up (uintD* xptr, const uintD* yptr, uintC count);
  local void nor_loop_up(xptr,yptr,count)
    var uintD* xptr;
    var const uintD* yptr;
    var uintC count;
    {
      until (count==0) {
        var uintD temp = ~ (*xptr | *yptr++);
        *xptr++ = temp;
        count--;
      }
    }

# ANDC2-Schleife:
# andc2_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch ANDC2 (AND NOT).
  local void andc2_loop_up (uintD* xptr, const uintD* yptr, uintC count);
  local void andc2_loop_up(xptr,yptr,count)
    var uintD* xptr;
    var const uintD* yptr;
    var uintC count;
    {
      until (count==0) {
        *xptr++ &= ~(*yptr++); count--;
      }
    }

# ORC2-Schleife:
# orc2_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch ORC2 (OR NOT).
  local void orc2_loop_up (uintD* xptr, const uintD* yptr, uintC count);
  local void orc2_loop_up(xptr,yptr,count)
    var uintD* xptr;
    var const uintD* yptr;
    var uintC count;
    {
      until (count==0) {
        *xptr++ |= ~(*yptr++); count--;
      }
    }

# NOT-Schleife:
# not_loop_up(xptr,count);
# verknüpft count (uintC>0) Digits aufwärts ab xptr mit Ziel ab xptr
# durch NOT.
  local void not_loop_up (uintD* xptr, uintC count);
  local void not_loop_up(xptr,count)
    var uintD* xptr;
    var uintC count;
    {
      do {
        var uintD temp = ~ (*xptr);
        *xptr++ = temp;
        count--;
      } until (count==0);
    }

# AND-Test-Schleife:
# and_test_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr durch AND
# und testet, ob sich dabei ein Digit /=0 ergibt. Ergebnis /=0, falls ja.
  local /*bool*/int and_test_loop_up (const uintD* xptr, const uintD* yptr, uintC count);
  local /*bool*/int and_test_loop_up(xptr,yptr,count)
    var const uintD* xptr;
    var const uintD* yptr;
    var uintC count;
    {
      until (count==0) {
        if (*xptr++ & *yptr++)
          return true;
        count--;
      }
      return false;
    }

# Test-Schleife:
# test_loop_up(ptr,count)
# testet count (uintC>=0) Digits aufwärts ab ptr, ob darunter eines /=0 ist.
# Ergebnis /=0, falls ja.
  local /*bool*/int test_loop_up (const uintD* ptr, uintC count);
  local /*bool*/int test_loop_up(ptr,count)
    var const uintD* ptr;
    var uintC count;
    {
      until (count==0) {
        if (*ptr++)
          return true;
        count--;
      }
      return false;
    }

# Vergleichsschleife:
# result = compare_loop_up(xptr,yptr,count);
# vergleicht nacheinander xptr[0] mit yptr[0], xptr[1] mit yptr[1], usw.,
# insgesamt count Digits, und liefert 0 falls alle gleich sind,
# +1 falls zuerst ein xptr[i]>yptr[i] ist,
# -1 falls zuerst ein xptr[i]<yptr[i] ist.
  local signean compare_loop_up (const uintD* xptr, const uintD* yptr, uintC count);
  local signean compare_loop_up(xptr,yptr,count)
    var const uintD* xptr;
    var const uintD* yptr;
    var uintC count;
    {
      until (count==0) {
        if (!(*xptr++ == *yptr++))
          # verschiedene Digits gefunden
          return (*--xptr > *--yptr ? signean_plus : signean_minus);
        count--;
      }
      return signean_null; # alle Digits gleich
    }

# Additionsschleife:
# übertrag = add_loop_down(sourceptr1,sourceptr2,destptr,count);
# addiert count (uintC>=0) Digits abwärts von sourceptr1, von sourceptr2
# abwärts nach destptr und liefert den Übertrag (0 oder /=0, was 1 bedeutet).
  local uintD add_loop_down (const uintD* sourceptr1, const uintD* sourceptr2, uintD* destptr, uintC count);
  local uintD add_loop_down(sourceptr1,sourceptr2,destptr,count)
    var const uintD* sourceptr1;
    var const uintD* sourceptr2;
    var uintD* destptr;
    var uintC count;
    {
      var uintD carry = 0;
      until (count==0) {
        var uintD source1 = *--sourceptr1;
        var uintD source2 = *--sourceptr2;
        if (carry)
          if (source1 >= (uintD)(~source2)) {
            *--destptr = source1 + source2 + 1; carry = 1;
          } else {
            *--destptr = source1 + source2 + 1; carry = 0;
          }
        else
          if (source1 > (uintD)(~source2)) {
            *--destptr = source1 + source2; carry = 1;
          } else {
            *--destptr = source1 + source2; carry = 0;
          }
        count--;
      }
      return carry;
    }

# Additionsschleife:
# übertrag = addto_loop_down(sourceptr,destptr,count);
# addiert count (uintC>=0) Digits abwärts von sourceptr, von destptr
# abwärts nach destptr und liefert den Übertrag (0 oder /=0, was 1 bedeutet).
  local uintD addto_loop_down (const uintD* sourceptr, uintD* destptr, uintC count);
  local uintD addto_loop_down(sourceptr,destptr,count)
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC count;
    {
      var uintD carry = 0;
      until (count==0) {
        var uintD source1 = *--sourceptr;
        var uintD source2 = *--destptr;
        if (carry)
          if (source1 >= (uintD)(~source2)) {
            *destptr = source1 + source2 + 1; carry = 1;
          } else {
            *destptr = source1 + source2 + 1; carry = 0;
          }
        else
          if (source1 > (uintD)(~source2)) {
            *destptr = source1 + source2; carry = 1;
          } else {
            *destptr = source1 + source2; carry = 0;
          }
        count--;
      }
      return carry;
    }

# Incrementierschleife:
# übertrag = inc_loop_down(ptr,count);
# incrementiert count (uintC>=0) Digits abwärts von ptr, so lange bis kein
# Übertrag mehr auftritt und liefert den Übertrag (0 oder /=0, was 1 bedeutet).
  local uintD inc_loop_down (uintD* ptr, uintC count);
  local uintD inc_loop_down(ptr,count)
    var uintD* ptr;
    var uintC count;
    {
      until (count==0) {
        if (!( ++(*--ptr) == 0 ))
          return 0; # kein weiterer Übertrag
        count--;
      }
      return 1; # weiterer Übertrag
    }

# Subtraktionsschleife:
# übertrag = sub_loop_down(sourceptr1,sourceptr2,destptr,count);
# subtrahiert count (uintC>=0) Digits abwärts von sourceptr1, von sourceptr2
# abwärts nach destptr und liefert den Übertrag (0 oder /=0, was -1 bedeutet).
  local uintD sub_loop_down (const uintD* sourceptr1, const uintD* sourceptr2, uintD* destptr, uintC count);
  local uintD sub_loop_down(sourceptr1,sourceptr2,destptr,count)
    var const uintD* sourceptr1;
    var const uintD* sourceptr2;
    var uintD* destptr;
    var uintC count;
    {
      var uintD carry = 0;
      until (count==0) {
        var uintD source1 = *--sourceptr1;
        var uintD source2 = *--sourceptr2;
        if (carry)
          if (source1 > source2) {
            *--destptr = source1 - source2 - 1; carry = 0;
          } else {
            *--destptr = source1 - source2 - 1; carry = -1;
          }
        else
          if (source1 >= source2) {
            *--destptr = source1 - source2; carry = 0;
          } else {
            *--destptr = source1 - source2; carry = -1;
          }
        count--;
      }
      return carry;
    }

# Subtraktionsschleife:
# übertrag = subx_loop_down(sourceptr1,sourceptr2,destptr,count,carry);
# subtrahiert count (uintC>=0) Digits abwärts von sourceptr1 und addiert
# einen Carry (0 oder -1), von sourceptr2 abwärts nach destptr und
# liefert den Übertrag (0 oder /=0, was -1 bedeutet).
  local uintD subx_loop_down (const uintD* sourceptr1, const uintD* sourceptr2, uintD* destptr, uintC count, uintD carry);
  local uintD subx_loop_down(sourceptr1,sourceptr2,destptr,count,carry)
    var const uintD* sourceptr1;
    var const uintD* sourceptr2;
    var uintD* destptr;
    var uintC count;
    var uintD carry;
    {
      until (count==0) {
        var uintD source1 = *--sourceptr1;
        var uintD source2 = *--sourceptr2;
        if (carry)
          if (source1 > source2) {
            *--destptr = source1 - source2 - 1; carry = 0;
          } else {
            *--destptr = source1 - source2 - 1; carry = -1;
          }
        else
          if (source1 >= source2) {
            *--destptr = source1 - source2; carry = 0;
          } else {
            *--destptr = source1 - source2; carry = -1;
          }
        count--;
      }
      return carry;
    }

# Subtraktionsschleife:
# übertrag = subfrom_loop_down(sourceptr,destptr,count);
# subtrahiert count (uintC>=0) Digits abwärts von sourceptr, von destptr
# abwärts nach destptr (dest := dest - source)
# und liefert den Übertrag (0 oder /=0, was -1 bedeutet).
  local uintD subfrom_loop_down (const uintD* sourceptr, uintD* destptr, uintC count);
  local uintD subfrom_loop_down(sourceptr,destptr,count)
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC count;
    {
      var uintD carry = 0;
      until (count==0) {
        var uintD source1 = *--destptr;
        var uintD source2 = *--sourceptr;
        if (carry)
          if (source1 > source2) {
            *destptr = source1 - source2 - 1; carry = 0;
          } else {
            *destptr = source1 - source2 - 1; carry = -1;
          }
        else
          if (source1 >= source2) {
            *destptr = source1 - source2; carry = 0;
          } else {
            *destptr = source1 - source2; carry = -1;
          }
        count--;
      }
      return carry;
    }

# Decrementierschleife:
# übertrag = dec_loop_down(ptr,count);
# decrementiert count (uintC>=0) Digits abwärts von ptr, so lange bis kein
# Übertrag mehr auftritt und liefert den Übertrag (0 oder -1).
  local uintD dec_loop_down (uintD* ptr, uintC count);
  local uintD dec_loop_down(ptr,count)
    var uintD* ptr;
    var uintC count;
    {
      until (count==0) {
        if (!( (*--ptr)-- == 0 ))
          return 0; # kein weiterer Übertrag
        count--;
      }
      return -1; # weiterer Übertrag
    }

# Negierschleife:
# übertrag = neg_loop_down(ptr,count);
# negiert count (uintC>=0) Digits abwärts von ptr,
# und liefert den Übertrag (0 oder -1).
  local uintD neg_loop_down (uintD* ptr, uintC count);
  local uintD neg_loop_down(ptr,count)
    var uintD* ptr;
    var uintC count;
    {
      # erstes Digit /=0 suchen:
      until (count==0) {
        if (!(*--ptr == 0))
          goto L1;
        count--;
      }
      return 0;
      L1: # erstes Digit /=0 gefunden, ab jetzt gibt's Carrys
      *ptr = - *ptr; count--; # 1 Digit negieren
      until (count==0) { # alle anderen Digits invertieren
        --ptr;
        *ptr = ~ *ptr;
        count--;
      }
      return -1;
    }

# Schiebeschleife um 1 Bit nach links:
# übertrag = shift1left_loop_down(ptr,count);
# schiebt count (uintC>=0) Digits abwärts von ptr um 1 Bit nach links,
# und liefert den Übertrag (0 oder /=0, was 1 bedeutet).
  local uintD shift1left_loop_down (uintD* ptr, uintC count);
  #if HAVE_DD
  local uintD shift1left_loop_down(ptr,count)
    var uintD* ptr;
    var uintC count;
    {
      var uintDD accu = 0;
      until (count==0) {
        accu = ((uintDD)(*--ptr)<<1)+accu; *ptr = lowD(accu);
        accu = (uintDD)(highD(accu));
        count--;
      }
      return (uintD)accu;
    }
  #else
  local uintD shift1left_loop_down(ptr,count)
    var uintD* ptr;
    var uintC count;
    {
      var uintD carry = 0;
      until (count==0) {
        var uintD accu = *--ptr;
        *ptr = (accu<<1) | carry;
        carry = accu>>(intDsize-1);
        count--;
      }
      return carry;
    }
  #endif

# Schiebeschleife um i Bits nach links:
# übertrag = shiftleft_loop_down(ptr,count,i,übertrag_init);
# schiebt count (uintC>=0) Digits abwärts von ptr um i Bits (0<i<intDsize)
# nach links, schiebt dabei die i Bits aus übertrag_init rechts rein,
# und liefert den Übertrag (was links rauskommt, >=0, <2^i).
  local uintD shiftleft_loop_down (uintD* ptr, uintC count, uintC i, uintD carry);
  #if HAVE_DD
  local uintD shiftleft_loop_down(ptr,count,i,carry)
    var uintD* ptr;
    var uintC count;
    var uintC i;
    var uintD carry;
    {
      var uintDD accu = (uintDD)carry;
      until (count==0) {
        accu = ((uintDD)(*--ptr)<<i)+accu; *ptr = lowD(accu);
        accu = (uintDD)(highD(accu));
        count--;
      }
      return (uintD)accu;
    }
  #else
  local uintD shiftleft_loop_down(ptr,count,i,carry)
    var uintD* ptr;
    var uintC count;
    var uintC i;
    var uintD carry;
    {
      var uintC j = intDsize-i;
      until (count==0) {
        var uintD accu = *--ptr;
        *ptr = (accu<<i) | carry;
        carry = accu>>j;
        count--;
      }
      return carry;
    }
  #endif

# Schiebe- und Kopierschleife um i Bits nach links:
# übertrag = shiftleftcopy_loop_down(sourceptr,destptr,count,i);
# kopiert count (uintC>=0) Digits abwärts von sourceptr nach destptr
# und schiebt sie dabei um i Bits (0<i<intDsize) nach links,
# wobei ganz rechts mit i Nullbits aufgefüllt wird,
# und liefert den Übertrag (was links rauskommt, >=0, <2^i).
  local uintD shiftleftcopy_loop_down (const uintD* sourceptr, uintD* destptr, uintC count, uintC i);
  #if HAVE_DD
  local uintD shiftleftcopy_loop_down(sourceptr,destptr,count,i)
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC count;
    var uintC i;
    {
      var uintDD accu = 0;
      until (count==0) {
        accu = ((uintDD)(*--sourceptr)<<i)+accu; *--destptr = lowD(accu);
        accu = (uintDD)(highD(accu));
        count--;
      }
      return (uintD)accu;
    }
  #else
  local uintD shiftleftcopy_loop_down(sourceptr,destptr,count,i)
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC count;
    var uintC i;
    {
      var uintC j = intDsize-i;
      var uintD carry = 0;
      until (count==0) {
        var uintD accu = *--sourceptr;
        *--destptr = (accu<<i) | carry;
        carry = accu>>j;
        count--;
      }
      return carry;
    }
  #endif

# Schiebeschleife um 1 Bit nach rechts:
# übertrag = shift1right_loop_up(ptr,count,übertrag_init);
# schiebt count (uintC>=0) Digits aufwärts von ptr um 1 Bit nach rechts,
# wobei links das Bit übertrag_init (sollte =0 oder =-1 sein) hineingeschoben
# wird, und liefert den Übertrag (0 oder /=0, was 1 bedeutet).
  local uintD shift1right_loop_up (uintD* ptr, uintC count, uintD carry);
  #if HAVE_DD
  local uintD shift1right_loop_up(ptr,count,carry)
    var uintD* ptr;
    var uintC count;
    var uintD carry;
    {
      var uintDD accu = (sintDD)(sintD)carry & ((uintDD)1 << (2*intDsize-1)); # 0 oder bit(2*intDsize-1)
      until (count==0) {
        accu = (highlowDD_0(*ptr)>>1)+accu; *ptr++ = highD(accu);
        accu = highlowDD_0(lowD(accu));
        count--;
      }
      return highD(accu);
    }
  #else
  local uintD shift1right_loop_up(ptr,count,carry)
    var uintD* ptr;
    var uintC count;
    var uintD carry;
    {
      carry = carry << (intDsize-1); # carry zu einem einzigen Bit machen
      until (count==0) {
        var uintD accu = *ptr;
        *ptr++ = (accu >> 1) | carry;
        carry = accu << (intDsize-1);
        count--;
      }
      return carry;
    }
  #endif

# Schiebeschleife um i Bits nach rechts:
# übertrag = shiftright_loop_up(ptr,count,i);
# schiebt count (uintC>=0) Digits aufwärts von ptr um i Bits (0<i<intDsize)
# nach rechts, wobei links Nullen eingeschoben werden,
# und liefert den Übertrag (was rechts rauskommt, als Bits intDsize-1..intDsize-i).
  local uintD shiftright_loop_up (uintD* ptr, uintC count, uintC i);
  #if HAVE_DD
  local uintD shiftright_loop_up(ptr,count,i)
    var uintD* ptr;
    var uintC count;
    var uintC i;
    {
      var uintDD accu = 0;
      until (count==0) {
        # Die oberen i Bits von (uintD)accu bilden hier den Übertrag.
        accu = highlowDD_0(lowD(accu));
        # Die oberen i Bits von (uintDD)accu bilden hier den Übertrag.
        accu = (highlowDD_0(*ptr)>>i)+accu; *ptr++ = highD(accu);
        count--;
      }
      return lowD(accu);
    }
  #else
  local uintD shiftright_loop_up(ptr,count,i)
    var uintD* ptr;
    var uintC count;
    var uintC i;
    {
      var uintC j = intDsize-i;
      var uintD carry = 0;
      until (count==0) {
        var uintD accu = *ptr;
        *ptr++ = (accu >> i) | carry;
        carry = accu << j;
        count--;
      }
      return carry;
    }
  #endif

# Schiebeschleife um i Bits nach rechts:
# übertrag = shiftrightsigned_loop_up(ptr,count,i);
# schiebt count (uintC>0) Digits aufwärts von ptr um i Bits (0<i<intDsize)
# nach rechts, wobei links das MSBit ver-i-facht wird,
# und liefert den Übertrag (was rechts rauskommt, als Bits intDsize-1..intDsize-i).
  local uintD shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i);
  #if HAVE_DD
  local uintD shiftrightsigned_loop_up(ptr,count,i)
    var uintD* ptr;
    var uintC count;
    var uintC i;
    {
      var uintDD accu = # Übertrag mit i Vorzeichenbits initialisieren
                           highlowDD_0(sign_of_sintD((sintD)(*ptr)))>>i;
      do {
        # Die oberen i Bits von (uintD)accu bilden hier den Übertrag.
        accu = highlowDD_0(lowD(accu));
        # Die oberen i Bits von (uintDD)accu bilden hier den Übertrag.
        accu = (highlowDD_0(*ptr)>>i)+accu; *ptr++ = highD(accu);
        count--;
      } until (count==0);
      return lowD(accu);
    }
  #else
  local uintD shiftrightsigned_loop_up(ptr,count,i)
    var uintD* ptr;
    var uintC count;
    var uintC i;
    {
      var uintC j = intDsize-i;
      var uintD carry;
      {
        var uintD accu = *ptr;
        *ptr++ = (sintD)accu >> i;
        carry = accu << j;
        count--;
      }
      until (count==0) {
        var uintD accu = *ptr;
        *ptr++ = (accu >> i) | carry;
        carry = accu << j;
        count--;
      }
      return carry;
    }
  #endif

# Schiebe- und Kopier-Schleife um i Bits nach rechts:
# übertrag = shiftrightcopy_loop_up(sourceptr,destptr,count,i,carry);
# kopiert count (uintC>=0) Digits aufwärts von sourceptr nach destptr
# und schiebt sie dabei um i Bits (0<i<intDsize) nach rechts, wobei carry
# (sozusagen als sourceptr[-1]) die i Bits ganz links bestimmt,
# und liefert den Übertrag (was rechts rauskommt, als Bits intDsize-1..intDsize-i).
  local uintD shiftrightcopy_loop_up (const uintD* sourceptr, uintD* destptr, uintC count, uintC i, uintD carry);
  #if HAVE_DD
  local uintD shiftrightcopy_loop_up(sourceptr,destptr,count,i,carry)
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC count;
    var uintC i;
    var uintD carry;
    {
      var uintDD accu = # Übertrag mit carry initialisieren
                           highlowDD_0(carry)>>i;
      until (count==0) {
        # Die oberen i Bits von (uintD)accu bilden hier den Übertrag.
        accu = highlowDD_0(lowD(accu));
        # Die oberen i Bits von (uintDD)accu bilden hier den Übertrag.
        accu = (highlowDD_0(*sourceptr++)>>i)+accu; *destptr++ = highD(accu);
        count--;
      }
      return lowD(accu);
    }
  #else
  local uintD shiftrightcopy_loop_up(sourceptr,destptr,count,i,carry)
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC count;
    var uintC i;
    var uintD carry;
    {
      var uintC j = intDsize-i;
      carry = carry << j;
      until (count==0) {
        var uintD accu = *sourceptr++;
        *destptr++ = (accu >> i) | carry;
        carry = accu << j;
        count--;
      }
      return carry;
    }
  #endif

# Multiplikations-Einfachschleife:
# Multipliziert eine UDS mit einem kleinen Digit und addiert ein kleines Digit.
# mulusmall_loop_down(digit,ptr,len,newdigit)
# multipliziert die UDS  ptr[-len..-1]  mit digit (>=2, <=36),
# addiert dabei newdigit (>=0, <digit) zur letzten Ziffer,
# und liefert den Carry (>=0, <digit).
  local uintD mulusmall_loop_down (uintD digit, uintD* ptr, uintC len, uintD newdigit);
  #if HAVE_DD
  local uintD mulusmall_loop_down(digit,ptr,len,newdigit)
    var uintD digit;
    var uintD* ptr;
    var uintC len;
    var uintD newdigit;
    {
      var uintDD carry = newdigit;
      until (len==0) {
        # Hier ist 0 <= carry < digit.
        carry = carry + muluD(digit,*--ptr);
        # Hier ist 0 <= carry < 2^intDsize*digit.
        *ptr = lowD(carry);
        carry = (uintDD)highD(carry); # carry := floor(carry/2^intDsize) < digit
        len--;
      }
      return lowD(carry);
    }
  #else
  local uintD mulusmall_loop_down(digit,ptr,len,newdigit)
    var uintD digit;
    var uintD* ptr;
    var uintC len;
    var uintD newdigit;
    {
      var uintD carry = newdigit;
      until (len==0) {
        # Hier ist 0 <= carry < digit.
        var uintD hi;
        var uintD lo;
        muluD(digit,*--ptr,hi=,lo=);
        # Hier ist 0 <= 2^intDsize*hi + lo + carry < 2^intDsize*digit.
        lo += carry; if (lo < carry) { hi += 1; }
        *ptr = lo;
        carry = hi;
        len--;
      }
      return carry;
    }
  #endif

# Multiplikations-Einfachschleife:
# Multipliziert eine UDS mit einem Digit und legt das Ergebnis in einer
# zweiten UDS ab.
# mulu_loop_down(digit,sourceptr,destptr,len);
# multipliziert die UDS  sourceptr[-len..-1]  (len>0)
# mit dem einzelnen  digit
# und legt das Ergebnis in der UDS  destptr[-len-1..-1]  ab.
  local void mulu_loop_down (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len);
  #if HAVE_DD
  local void mulu_loop_down(digit,sourceptr,destptr,len)
    var uintD digit;
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC len;
    {
      var uintDD carry = 0;
      do {
        # Hier ist carry=digit=0 oder 0 <= carry < digit.
        carry = carry + muluD(digit,*--sourceptr);
        # Hier ist carry=digit=0 oder 0 <= carry < 2^intDsize*digit.
        *--destptr = lowD(carry);
        carry = (uintDD)highD(carry); # carry := floor(carry/2^intDsize) < digit
        len--;
      } until (len==0);
      *--destptr = lowD(carry);
    }
  #else
  local void mulu_loop_down(digit,sourceptr,destptr,len)
    var uintD digit;
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC len;
    {
      var uintD carry = 0;
      do {
        # Hier ist carry=digit=0 oder 0 <= carry < digit.
        var uintD hi;
        var uintD lo;
        muluD(digit,*--sourceptr,hi=,lo=);
        # Hier ist 0 <= 2^intDsize*hi + lo + carry < 2^intDsize*digit oder hi=lo=carry=digit=0.
        lo += carry; if (lo < carry) { hi += 1; }
        *--destptr = lo;
        carry = hi;
        len--;
      } until (len==0);
      *--destptr = carry;
    }
  #endif

# Multiplikations-Einfachschleife mit Akkumulation:
# Multipliziert eine UDS mit einem Digit und addiert das Ergebnis zu einer
# zweiten UDS auf.
# muluadd_loop_down(digit,sourceptr,destptr,len);
# multipliziert die UDS  sourceptr[-len..-1]  (len>0)
# mit dem einzelnen digit, legt das Ergebnis in der UDS  destptr[-len..-1]
# ab und liefert den weiteren Übertrag.
  local uintD muluadd_loop_down (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len);
  #if HAVE_DD
  local uintD muluadd_loop_down(digit,sourceptr,destptr,len)
    var uintD digit;
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC len;
    {
      var uintDD carry = 0;
      if (!(digit==0)) {
        do {
          # Hier ist 0 <= carry <= digit.
          carry = carry + muluD(digit,*--sourceptr) + (uintDD)*--destptr;
          # Hier ist 0 <= carry <= 2^intDsize*digit + 2^intDsize-1.
          *destptr = lowD(carry);
          carry = (uintDD)highD(carry); # carry := floor(carry/2^intDsize) <= digit
          len--;
        } until (len==0);
      }
      return lowD(carry);
    }
  #else
  local uintD muluadd_loop_down(digit,sourceptr,destptr,len)
    var uintD digit;
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC len;
    {
      var uintD carry = 0;
      if (!(digit==0)) {
        do {
          # Hier ist 0 <= carry <= digit.
          var uintD hi;
          var uintD lo;
          muluD(digit,*--sourceptr,hi=,lo=);
          # Hier ist 0 <= 2^intDsize*hi + lo + carry + *--destptr <= 2^intDsize*digit+2^intDsize-1.
          lo += carry; if (lo < carry) { hi += 1; }
          carry = *--destptr;
          lo += carry; if (lo < carry) { hi += 1; }
          *destptr = lo;
          carry = hi;
          len--;
        } until (len==0);
      }
      return carry;
    }
  #endif

# Multiplikations-Einfachschleife mit Diminution:
# Multipliziert eine UDS mit einem Digit und subtrahiert das Ergebnis von
# einer zweiten UDS.
# mulusub_loop_down(digit,sourceptr,destptr,len);
# multipliziert die UDS  sourceptr[-len..-1]  (len>0)  mit dem einzelnen
# digit, subtrahiert das Ergebnis von der UDS  destptr[-len..-1]  und liefert
# den weiteren Übertrag (>=0, evtl. von destptr[-len-1] zu subtrahieren).
  local uintD mulusub_loop_down (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len);
  #if HAVE_DD
  local uintD mulusub_loop_down(digit,sourceptr,destptr,len)
    var uintD digit;
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC len;
    {
      var uintDD carry = 0;
      if (!(digit==0)) {
        do {
          # Hier ist 0 <= carry <= digit.
          carry = carry + muluD(digit,*--sourceptr) + (uintD)(~(*--destptr));
          # Hier ist 0 <= carry <= 2^intDsize*digit + 2^intDsize-1.
          *destptr = ~lowD(carry);
          carry = (uintDD)highD(carry); # carry := floor(carry/2^intDsize) <= digit
          # Hier ist 0 <= carry <= digit.
          len--;
        } until (len==0);
        return lowD(carry);
      } else
        return 0; # nichts zu subtrahieren -> kein Übertrag
    }
  #else
  local uintD mulusub_loop_down(digit,sourceptr,destptr,len)
    var uintD digit;
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC len;
    {
      var uintD carry = 0;
      if (!(digit==0)) {
        do {
          # Hier ist 0 <= carry <= digit.
          var uintD hi;
          var uintD lo;
          muluD(digit,*--sourceptr,hi=,lo=);
          # Hier ist 0 <= 2^intDsize*hi + lo + carry + ~(*--destptr) <= 2^intDsize*digit+2^intDsize-1.
          lo += carry; if (lo < carry) { hi += 1; }
          carry = *--destptr;
          *destptr = carry - lo; if (carry < lo) { hi += 1; }
          carry = hi;
          len--;
        } until (len==0);
        return carry;
      } else
        return 0; # nichts zu subtrahieren -> kein Übertrag
    }
  #endif

# Divisions-Einfachschleife:
# Dividiert eine UDS durch ein Digit.
# divu_loop_up(digit,ptr,len)
# dividiert die UDS  ptr[0..len-1] durch digit,
# legt das Ergebnis in derselben UDS ab, und liefert den Rest (>=0, <digit).
  local uintD divu_loop_up (uintD digit, uintD* ptr, uintC len);
  #if HAVE_DD
  local uintD divu_loop_up(digit,ptr,len)
    var uintD digit;
    var uintD* ptr;
    var uintC len;
    {
      var uintD rest = 0;
      until (len==0) {
        divuD(highlowDD(rest,*ptr),digit,*ptr =, rest =);
        ptr++; len--;
      }
      return rest;
    }
  #else
  local uintD divu_loop_up(digit,ptr,len)
    var uintD digit;
    var uintD* ptr;
    var uintC len;
    {
      var uintD rest = 0;
      until (len==0) {
        divuD(rest,*ptr,digit,*ptr =, rest =);
        ptr++; len--;
      }
      return rest;
    }
  #endif

# Divisions-Einfachschleife:
# Dividiert eine UDS durch ein Digit und legt das Ergebnis in einer
# zweiten UDS ab.
# divucopy_loop_up(digit,sourceptr,destptr,len)
# dividiert die UDS  sourceptr[0..len-1]  durch digit,
# legt das Ergebnis in der UDS  destptr[0..len-1]  ab,
# und liefert den Rest (>=0, <digit).
  local uintD divucopy_loop_up (uintD digit, const uintD* sourceptr, uintD* destptr, uintC len);
  #if HAVE_DD
  local uintD divucopy_loop_up(digit,sourceptr,destptr,len)
    var uintD digit;
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC len;
    {
      var uintD rest = 0;
      until (len==0) {
        divuD(highlowDD(rest,*sourceptr++),digit,*destptr++ =, rest =);
        len--;
      }
      return rest;
    }
  #else
  local uintD divucopy_loop_up(digit,sourceptr,destptr,len)
    var uintD digit;
    var const uintD* sourceptr;
    var uintD* destptr;
    var uintC len;
    {
      var uintD rest = 0;
      until (len==0) {
        divuD(rest,*sourceptr++,digit,*destptr++ =, rest =);
        len--;
      }
      return rest;
    }
  #endif

