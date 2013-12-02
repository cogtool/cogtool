# Definitionen und portabler C-Code zu ARILEV1.D, Inline-Version

#ifndef COPY_LOOPS

# Kopierschleife:
# destptr = copy_loop_up(sourceptr,destptr,count);
# kopiert count (uintC>=0) Digits aufwärts von sourceptr nach destptr
# und liefert das neue destptr.
  inline local uintD* copy_loop_up (const uintD* sourceptr, uintD* destptr,
                                    uintC count)
  {
    dotimesC(count,count, {
      *destptr++ = *sourceptr++;
    });
    return destptr;
  }

# Kopierschleife:
# destptr = copy_loop_down(sourceptr,destptr,count);
# kopiert count (uintC>=0) Digits abwärts von sourceptr nach destptr
# und liefert das neue destptr.
  inline local uintD* copy_loop_down (const uintD* sourceptr, uintD* destptr,
                                      uintC count)
  {
    dotimesC(count,count, {
      *--destptr = *--sourceptr;
    });
    return destptr;
  }

#endif

#ifndef FILL_LOOPS

# Füllschleife:
# destptr = fill_loop_up(destptr,count,filler);
# kopiert count (uintC>=0) mal das Digit filler aufwärts nach destptr
# und liefert das neue destptr.
  inline local uintD* fill_loop_up (uintD* destptr, uintC count, uintD filler)
  {
    dotimesC(count,count, {
      *destptr++ = filler;
    });
    return destptr;
  }

# Füllschleife:
# destptr = fill_loop_down(destptr,count,filler);
# kopiert count (uintC>=0) mal das Digit filler abwärts nach destptr
# und liefert das neue destptr.
  inline local uintD* fill_loop_down (uintD* destptr, uintC count, uintD filler)
  {
    dotimesC(count,count, {
      *--destptr = filler;
    });
    return destptr;
  }

#endif

#ifndef CLEAR_LOOPS

# Lösch-Schleife:
# destptr = clear_loop_up(destptr,count);
# löscht count (uintC>=0) Digits aufwärts ab destptr
# und liefert das neue destptr.
  inline local uintD* clear_loop_up (uintD* destptr, uintC count)
  {
    dotimesC(count,count, {
      *destptr++ = 0;
    });
    return destptr;
  }

# Lösch-Schleife:
# destptr = clear_loop_down(destptr,count);
# löscht count (uintC>=0) Digits abwärts ab destptr
# und liefert das neue destptr.
  inline local uintD* clear_loop_down (uintD* destptr, uintC count)
  {
    dotimesC(count,count, {
      *--destptr = 0;
    });
    return destptr;
  }

#endif

#ifndef LOG_LOOPS

# OR-Schleife:
# or_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch OR.
  inline local void or_loop_up (uintD* xptr, const uintD* yptr, uintC count)
  {
    dotimesC(count,count, {
      *xptr++ |= *yptr++;
    });
  }

# XOR-Schleife:
# xor_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch XOR.
  inline local void xor_loop_up (uintD* xptr, const uintD* yptr, uintC count)
  {
    dotimesC(count,count, {
      *xptr++ ^= *yptr++;
    });
  }

# AND-Schleife:
# and_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch AND.
  inline local void and_loop_up (uintD* xptr, const uintD* yptr, uintC count)
  {
    dotimesC(count,count, {
      *xptr++ &= *yptr++;
    });
  }

# EQV-Schleife:
# eqv_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch EQV (NOT XOR).
  inline local void eqv_loop_up (uintD* xptr, const uintD* yptr, uintC count)
  {
    dotimesC(count,count, {
      var uintD temp = ~ (*xptr ^ *yptr++);
      *xptr++ = temp;
    });
  }

# NAND-Schleife:
# nand_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch NAND (NOT AND).
  inline local void nand_loop_up (uintD* xptr, const uintD* yptr, uintC count)
  {
    dotimesC(count,count, {
      var uintD temp = ~ (*xptr & *yptr++);
      *xptr++ = temp;
    });
  }

# NOR-Schleife:
# nor_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch NOR (NOT OR).
  inline local void nor_loop_up (uintD* xptr, const uintD* yptr, uintC count)
  {
    dotimesC(count,count, {
      var uintD temp = ~ (*xptr | *yptr++);
      *xptr++ = temp;
    });
  }

# ANDC2-Schleife:
# andc2_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch ANDC2 (AND NOT).
  inline local void andc2_loop_up (uintD* xptr, const uintD* yptr, uintC count)
  {
    dotimesC(count,count, {
      *xptr++ &= ~(*yptr++);
    });
  }

# ORC2-Schleife:
# orc2_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr
# mit Ziel ab xptr durch ORC2 (OR NOT).
  inline local void orc2_loop_up (uintD* xptr, const uintD* yptr, uintC count)
  {
    dotimesC(count,count, {
      *xptr++ |= ~(*yptr++);
    });
  }

# NOT-Schleife:
# not_loop_up(xptr,count);
# verknüpft count (uintC>0) Digits aufwärts ab xptr mit Ziel ab xptr
# durch NOT.
  inline local void not_loop_up (uintD* xptr, uintC count)
  {
    dotimespC(count,count, {
      var uintD temp = ~ (*xptr);
      *xptr++ = temp;
    });
  }

#endif

#ifndef TEST_LOOPS

# AND-Test-Schleife:
# and_test_loop_up(xptr,yptr,count);
# verknüpft count (uintC>=0) Digits aufwärts ab xptr und ab yptr durch AND
# und testet, ob sich dabei ein Digit /=0 ergibt. Ergebnis /=0, falls ja.
  inline local /*bool*/int and_test_loop_up (const uintD* xptr, const uintD* yptr,
                                             uintC count)
  {
    dotimesC(count,count, {
      if (*xptr++ & *yptr++)
        return true;
    });
    return false;
  }

# Test-Schleife:
# test_loop_up(ptr,count)
# testet count (uintC>=0) Digits aufwärts ab ptr, ob darunter eines /=0 ist.
# Ergebnis /=0, falls ja.
  inline local /*bool*/int test_loop_up (const uintD* ptr, uintC count)
  {
    dotimesC(count,count, {
      if (*ptr++)
        return true;
    });
    return false;
  }

# Vergleichsschleife:
# result = compare_loop_up(xptr,yptr,count);
# vergleicht nacheinander xptr[0] mit yptr[0], xptr[1] mit yptr[1], usw.,
# insgesamt count Digits, und liefert 0 falls alle gleich sind,
# +1 falls zuerst ein xptr[i]>yptr[i] ist,
# -1 falls zuerst ein xptr[i]<yptr[i] ist.
  inline local signean compare_loop_up (const uintD* xptr, const uintD* yptr,
                                        uintC count)
  {
    dotimesC(count,count, {
      if (!(*xptr++ == *yptr++))
        # verschiedene Digits gefunden
        return (*--xptr > *--yptr ? signean_plus : signean_minus);
    });
    return signean_null; # alle Digits gleich
  }

#endif

#ifndef ADDSUB_LOOPS

# Additionsschleife:
# übertrag = add_loop_down(sourceptr1,sourceptr2,destptr,count);
# addiert count (uintC>=0) Digits abwärts von sourceptr1, von sourceptr2
# abwärts nach destptr und liefert den Übertrag (0 oder /=0, was 1 bedeutet).
  inline local uintD add_loop_down (const uintD* sourceptr1,
                                    const uintD* sourceptr2,
                                    uintD* destptr,
                                    uintC count)
  {
    if (!(count==0))
    do {
      {
        var uintD source1 = *--sourceptr1;
        var uintD source2 = *--sourceptr2;
        *--destptr = source1 + source2;
        if (source1 > (uintD)(~source2))
          goto carry_1;
      }
     carry_0:
      count--;
    } until (count==0);
    return 0;
    do {
      {
        var uintD source1 = *--sourceptr1;
        var uintD source2 = *--sourceptr2;
        *--destptr = source1 + source2 + 1;
        if (source1 < (uintD)(~source2))
          goto carry_0;
      }
     carry_1:
      count--;
    } until (count==0);
    return 1;
  }

# Additionsschleife:
# übertrag = addto_loop_down(sourceptr,destptr,count);
# addiert count (uintC>=0) Digits abwärts von sourceptr, von destptr
# abwärts nach destptr und liefert den Übertrag (0 oder /=0, was 1 bedeutet).
  inline local uintD addto_loop_down (const uintD* sourceptr, uintD* destptr,
                                      uintC count)
  {
    if (!(count==0))
    do {
      {
        var uintD source1 = *--sourceptr;
        var uintD source2 = *--destptr;
        *destptr = source1 + source2;
        if (source1 > (uintD)(~source2))
          goto carry_1;
      }
     carry_0:
      count--;
    } until (count==0);
    return 0;
    do {
      {
        var uintD source1 = *--sourceptr;
        var uintD source2 = *--destptr;
        *destptr = source1 + source2 + 1;
        if (source1 < (uintD)(~source2))
          goto carry_0;
      }
     carry_1:
      count--;
    } until (count==0);
    return 1;
  }

# Incrementierschleife:
# übertrag = inc_loop_down(ptr,count);
# incrementiert count (uintC>=0) Digits abwärts von ptr, so lange bis kein
# Übertrag mehr auftritt und liefert den Übertrag (0 oder /=0, was 1 bedeutet).
  inline local uintD inc_loop_down (uintD* ptr, uintC count)
  {
    dotimesC(count,count, {
      if (!( ++(*--ptr) == 0 ))
        return 0; # kein weiterer Übertrag
    });
    return 1; # weiterer Übertrag
  }

# Subtraktionsschleife:
# übertrag = sub_loop_down(sourceptr1,sourceptr2,destptr,count);
# subtrahiert count (uintC>=0) Digits abwärts von sourceptr1, von sourceptr2
# abwärts nach destptr und liefert den Übertrag (0 oder /=0, was -1 bedeutet).
  inline local uintD sub_loop_down (const uintD* sourceptr1,
                                    const uintD* sourceptr2,
                                    uintD* destptr,
                                    uintC count)
  {
    if (!(count==0))
    do {
      {
        var uintD source1 = *--sourceptr1;
        var uintD source2 = *--sourceptr2;
        *--destptr = source1 - source2;
        if (source1 < source2)
          goto carry_1;
      }
     carry_0:
      count--;
    } until (count==0);
    return 0;
    do {
      {
        var uintD source1 = *--sourceptr1;
        var uintD source2 = *--sourceptr2;
        *--destptr = source1 - source2 - 1;
        if (source1 > source2)
          goto carry_0;
      }
     carry_1:
      count--;
    } until (count==0);
    return (uintD)-1;
  }

# Subtraktionsschleife:
# übertrag = subx_loop_down(sourceptr1,sourceptr2,destptr,count,carry);
# subtrahiert count (uintC>=0) Digits abwärts von sourceptr1 und addiert
# einen Carry (0 oder -1), von sourceptr2 abwärts nach destptr und
# liefert den Übertrag (0 oder /=0, was -1 bedeutet).
  inline local uintD subx_loop_down (const uintD* sourceptr1,
                                     const uintD* sourceptr2,
                                     uintD* destptr,
                                     uintC count,
                                     uintD carry)
  {
    if (carry==0) {
      if (!(count==0))
        do {
          {
            var uintD source1 = *--sourceptr1;
            var uintD source2 = *--sourceptr2;
            *--destptr = source1 - source2;
            if (source1 < source2)
              goto carry_1;
          }
         carry_0:
          count--;
        } until (count==0);
      return 0;
    } else {
      if (!(count==0))
        do {
          {
            var uintD source1 = *--sourceptr1;
            var uintD source2 = *--sourceptr2;
            *--destptr = source1 - source2 - 1;
            if (source1 > source2)
              goto carry_0;
          }
         carry_1:
          count--;
        } until (count==0);
      return (uintD)-1;
    }
  }

# Subtraktionsschleife:
# übertrag = subfrom_loop_down(sourceptr,destptr,count);
# subtrahiert count (uintC>=0) Digits abwärts von sourceptr, von destptr
# abwärts nach destptr (dest := dest - source)
# und liefert den Übertrag (0 oder /=0, was -1 bedeutet).
  inline local uintD subfrom_loop_down (const uintD* sourceptr, uintD* destptr,
                                        uintC count)
  {
    if (!(count==0))
    do {
      {
        var uintD source1 = *--destptr;
        var uintD source2 = *--sourceptr;
        *destptr = source1 - source2;
        if (source1 < source2)
          goto carry_1;
      }
     carry_0:
      count--;
    } until (count==0);
    return 0;
    do {
      {
        var uintD source1 = *--destptr;
        var uintD source2 = *--sourceptr;
        *destptr = source1 - source2 - 1;
        if (source1 > source2)
          goto carry_0;
      }
     carry_1:
      count--;
    } until (count==0);
    return (uintD)-1;
  }

# Decrementierschleife:
# übertrag = dec_loop_down(ptr,count);
# decrementiert count (uintC>=0) Digits abwärts von ptr, so lange bis kein
# Übertrag mehr auftritt und liefert den Übertrag (0 oder -1).
  inline local uintD dec_loop_down (uintD* ptr, uintC count)
  {
    dotimesC(count,count, {
      if (!( (*--ptr)-- == 0 ))
        return 0; # kein weiterer Übertrag
    });
    return (uintD)-1; # weiterer Übertrag
  }

# Negierschleife:
# übertrag = neg_loop_down(ptr,count);
# negiert count (uintC>=0) Digits abwärts von ptr,
# und liefert den Übertrag (0 oder -1).
  inline local uintD neg_loop_down (uintD* ptr, uintC count)
  {
    # erstes Digit /=0 suchen:
    until (count==0) {
      if (!(*--ptr == 0)) goto L1;
      count--;
    }
    return 0;
    L1: # erstes Digit /=0 gefunden, ab jetzt gibt's Carrys
    *ptr = - *ptr; count--; # 1 Digit negieren
    dotimesC(count,count, { # alle anderen Digits invertieren
      --ptr; *ptr = ~ *ptr;
    });
    return (uintD)-1;
  }

#endif

#ifndef SHIFT_LOOPS

# Schiebeschleife um 1 Bit nach links:
# übertrag = shift1left_loop_down(ptr,count);
# schiebt count (uintC>=0) Digits abwärts von ptr um 1 Bit nach links,
# und liefert den Übertrag (0 oder /=0, was 1 bedeutet).
  inline local uintD shift1left_loop_down (uintD* ptr, uintC count)
  {
    #if HAVE_DD
      var uintDD accu = 0;
      dotimesC(count,count, {
        accu = ((uintDD)(*--ptr)<<1)+accu; *ptr = lowD(accu);
        accu = (uintDD)(highD(accu));
      });
      return (uintD)accu;
    #else
      var uintD carry = 0;
      dotimesC(count,count, {
        var uintD accu = *--ptr;
        *ptr = (accu<<1) | carry;
        carry = accu>>(intDsize-1);
      });
      return carry;
    #endif
  }

# Schiebeschleife um i Bits nach links:
# übertrag = shiftleft_loop_down(ptr,count,i,übertrag_init);
# schiebt count (uintC>=0) Digits abwärts von ptr um i Bits (0<i<intDsize)
# nach links, schiebt dabei die i Bits aus übertrag_init rechts rein,
# und liefert den Übertrag (was links rauskommt, >=0, <2^i).
  inline local uintD shiftleft_loop_down (uintD* ptr, uintC count, uintC i,
                                          uintD carry)
  {
    #if HAVE_DD
      var uintDD accu = (uintDD)carry;
      dotimesC(count,count, {
        accu = ((uintDD)(*--ptr)<<i)+accu; *ptr = lowD(accu);
        accu = (uintDD)(highD(accu));
      });
      return (uintD)accu;
    #else
      if (count > 0) {
        var uintC j = intDsize-i;
        dotimespC(count,count, {
          var uintD accu = *--ptr;
          *ptr = (accu<<i) | carry;
          carry = accu>>j;
        });
      }
      return carry;
    #endif
  }

# Schiebe- und Kopierschleife um i Bits nach links:
# übertrag = shiftleftcopy_loop_down(sourceptr,destptr,count,i);
# kopiert count (uintC>=0) Digits abwärts von sourceptr nach destptr
# und schiebt sie dabei um i Bits (0<i<intDsize) nach links,
# wobei ganz rechts mit i Nullbits aufgefüllt wird,
# und liefert den Übertrag (was links rauskommt, >=0, <2^i).
  inline local uintD shiftleftcopy_loop_down (const uintD* sourceptr,
                                              uintD* destptr,
                                              uintC count,
                                              uintC i)
  {
    #if HAVE_DD
      var uintDD accu = 0;
      dotimesC(count,count, {
        accu = ((uintDD)(*--sourceptr)<<i)+accu; *--destptr = lowD(accu);
        accu = (uintDD)(highD(accu));
      });
      return (uintD)accu;
    #else
      var uintD carry = 0;
      if (count > 0) {
        var uintC j = intDsize-i;
        dotimespC(count,count, {
          var uintD accu = *--sourceptr;
          *--destptr = (accu<<i) | carry;
          carry = accu>>j;
        });
      }
      return carry;
    #endif
  }

# Schiebeschleife um 1 Bit nach rechts:
# übertrag = shift1right_loop_up(ptr,count,übertrag_init);
# schiebt count (uintC>=0) Digits aufwärts von ptr um 1 Bit nach rechts,
# wobei links das Bit übertrag_init (sollte =0 oder =-1 sein) hineingeschoben
# wird, und liefert den Übertrag (0 oder /=0, was 1 bedeutet).
  inline local uintD shift1right_loop_up (uintD* ptr, uintC count, uintD carry)
  {
    #if HAVE_DD
      var uintDD accu = (sintDD)(sintD)carry & ((uintDD)1 << (2*intDsize-1)); # 0 oder bit(2*intDsize-1)
      dotimesC(count,count, {
        accu = (highlowDD_0(*ptr)>>1)+accu; *ptr++ = highD(accu);
        accu = highlowDD_0(lowD(accu));
      });
      return highD(accu);
    #else
      carry = carry << (intDsize-1); # carry zu einem einzigen Bit machen
      dotimesC(count,count, {
        var uintD accu = *ptr;
        *ptr++ = (accu >> 1) | carry;
        carry = accu << (intDsize-1);
      });
      return carry;
    #endif
  }

# Schiebeschleife um i Bits nach rechts:
# übertrag = shiftright_loop_up(ptr,count,i);
# schiebt count (uintC>=0) Digits aufwärts von ptr um i Bits (0<i<intDsize)
# nach rechts, wobei links Nullen eingeschoben werden,
# und liefert den Übertrag (was rechts rauskommt, als Bits intDsize-1..intDsize-i).
  inline local uintD shiftright_loop_up (uintD* ptr, uintC count, uintC i)
  {
    #if HAVE_DD
      var uintDD accu = 0;
      dotimesC(count,count, {
        # Die oberen i Bits von (uintD)accu bilden hier den Übertrag.
        accu = highlowDD_0(lowD(accu));
        # Die oberen i Bits von (uintDD)accu bilden hier den Übertrag.
        accu = (highlowDD_0(*ptr)>>i)+accu; *ptr++ = highD(accu);
      });
      return lowD(accu);
    #else
      var uintD carry = 0;
      if (count > 0) {
        var uintC j = intDsize-i;
        dotimespC(count,count, {
          var uintD accu = *ptr;
          *ptr++ = (accu >> i) | carry;
          carry = accu << j;
        });
      }
      return carry;
    #endif
  }

# Schiebeschleife um i Bits nach rechts:
# übertrag = shiftrightsigned_loop_up(ptr,count,i);
# schiebt count (uintC>0) Digits aufwärts von ptr um i Bits (0<i<intDsize)
# nach rechts, wobei links das MSBit ver-i-facht wird,
# und liefert den Übertrag (was rechts rauskommt, als Bits intDsize-1..intDsize-i).
  inline local uintD shiftrightsigned_loop_up (uintD* ptr, uintC count, uintC i)
  {
    #if HAVE_DD
      var uintDD accu = # Übertrag mit i Vorzeichenbits initialisieren
                           highlowDD_0(sign_of_sintD((sintD)(*ptr)))>>i;
      dotimespC(count,count, {
        # Die oberen i Bits von (uintD)accu bilden hier den Übertrag.
        accu = highlowDD_0(lowD(accu));
        # Die oberen i Bits von (uintDD)accu bilden hier den Übertrag.
        accu = (highlowDD_0(*ptr)>>i)+accu; *ptr++ = highD(accu);
      });
      return lowD(accu);
    #else
      var uintC j = intDsize-i;
      var uintD carry;
      {
        var uintD accu = *ptr;
        *ptr++ = (sintD)accu >> i;
        carry = accu << j;
        count--;
      }
      dotimesC(count,count, {
        var uintD accu = *ptr;
        *ptr++ = (accu >> i) | carry;
        carry = accu << j;
      });
      return carry;
    #endif
  }

# Schiebe- und Kopier-Schleife um i Bits nach rechts:
# übertrag = shiftrightcopy_loop_up(sourceptr,destptr,count,i,carry);
# kopiert count (uintC>=0) Digits aufwärts von sourceptr nach destptr
# und schiebt sie dabei um i Bits (0<i<intDsize) nach rechts, wobei carry
# (sozusagen als sourceptr[-1]) die i Bits ganz links bestimmt,
# und liefert den Übertrag (was rechts rauskommt, als Bits intDsize-1..intDsize-i).
  inline local uintD shiftrightcopy_loop_up (const uintD* sourceptr,
                                             uintD* destptr,
                                             uintC count,
                                             uintC i,
                                             uintD carry)
  {
    #if HAVE_DD
      var uintDD accu = # Übertrag mit carry initialisieren
                           highlowDD_0(carry)>>i;
      dotimesC(count,count, {
        # Die oberen i Bits von (uintD)accu bilden hier den Übertrag.
        accu = highlowDD_0(lowD(accu));
        # Die oberen i Bits von (uintDD)accu bilden hier den Übertrag.
        accu = (highlowDD_0(*sourceptr++)>>i)+accu; *destptr++ = highD(accu);
      });
      return lowD(accu);
    #else
      var uintC j = intDsize-i;
      carry = carry << j;
      dotimesC(count,count, {
        var uintD accu = *sourceptr++;
        *destptr++ = (accu >> i) | carry;
        carry = accu << j;
      });
      return carry;
    #endif
  }

#endif

#ifndef MUL_LOOPS

# Multiplikations-Einfachschleife:
# Multipliziert eine UDS mit einem kleinen Digit und addiert ein kleines Digit.
# mulusmall_loop_down(digit,ptr,len,newdigit)
# multipliziert die UDS  ptr[-len..-1]  mit digit (>=2, <=36),
# addiert dabei newdigit (>=0, <digit) zur letzten Ziffer,
# und liefert den Carry (>=0, <digit).
  inline local uintD mulusmall_loop_down (uintD digit, uintD* ptr, uintC len,
                                          uintD newdigit)
  {
    #if HAVE_DD
      var uintDD carry = newdigit;
      dotimesC(len,len, {
        # Hier ist 0 <= carry < digit.
        carry = carry + muluD(digit,*--ptr);
        # Hier ist 0 <= carry < 2^intDsize*digit.
        *ptr = lowD(carry);
        carry = (uintDD)highD(carry); # carry := floor(carry/2^intDsize) < digit
      });
      return lowD(carry);
    #else
      var uintD carry = newdigit;
      dotimesC(len,len, {
        # Hier ist 0 <= carry < digit.
        var uintD hi;
        var uintD lo;
        muluD(digit,*--ptr,hi=,lo=);
        # Hier ist 0 <= 2^intDsize*hi + lo + carry < 2^intDsize*digit.
        lo += carry; if (lo < carry) { hi += 1; }
        *ptr = lo;
        carry = hi;
      });
      return carry;
    #endif
  }

# Multiplikations-Einfachschleife:
# Multipliziert eine UDS mit einem Digit und legt das Ergebnis in einer
# zweiten UDS ab.
# mulu_loop_down(digit,sourceptr,destptr,len);
# multipliziert die UDS  sourceptr[-len..-1]  (len>0)
# mit dem einzelnen  digit
# und legt das Ergebnis in der UDS  destptr[-len-1..-1]  ab.
  inline local void mulu_loop_down (uintD digit,
                                    const uintD* sourceptr, uintD* destptr,
                                    uintC len)
  {
    #if HAVE_DD
      var uintDD carry = 0;
      dotimespC(len,len, {
        # Hier ist carry=digit=0 oder 0 <= carry < digit.
        carry = carry + muluD(digit,*--sourceptr);
        # Hier ist carry=digit=0 oder 0 <= carry < 2^intDsize*digit.
        *--destptr = lowD(carry);
        carry = (uintDD)highD(carry); # carry := floor(carry/2^intDsize) < digit
      });
      *--destptr = lowD(carry);
    #else
      var uintD carry = 0;
      dotimespC(len,len, {
        # Hier ist carry=digit=0 oder 0 <= carry < digit.
        var uintD hi;
        var uintD lo;
        muluD(digit,*--sourceptr,hi=,lo=);
        # Hier ist 0 <= 2^intDsize*hi + lo + carry < 2^intDsize*digit oder hi=lo=carry=digit=0.
        lo += carry; if (lo < carry) { hi += 1; }
        *--destptr = lo;
        carry = hi;
      });
      *--destptr = carry;
    #endif
  }

# Multiplikations-Einfachschleife mit Akkumulation:
# Multipliziert eine UDS mit einem Digit und addiert das Ergebnis zu einer
# zweiten UDS auf.
# muluadd_loop_down(digit,sourceptr,destptr,len);
# multipliziert die UDS  sourceptr[-len..-1]  (len>0)
# mit dem einzelnen digit, legt das Ergebnis in der UDS  destptr[-len..-1]
# ab und liefert den weiteren Übertrag.
  inline local uintD muluadd_loop_down (uintD digit,
                                        const uintD* sourceptr, uintD* destptr,
                                        uintC len)
  {
    #if HAVE_DD
      var uintDD carry = 0;
      if (!(digit==0)) {
        dotimespC(len,len, {
          # Hier ist 0 <= carry <= digit.
          carry = carry + muluD(digit,*--sourceptr) + (uintDD)*--destptr;
          # Hier ist 0 <= carry <= 2^intDsize*digit + 2^intDsize-1.
          *destptr = lowD(carry);
          carry = (uintDD)highD(carry); # carry := floor(carry/2^intDsize) <= digit
        });
      }
      return lowD(carry);
    #else
      var uintD carry = 0;
      if (!(digit==0)) {
        dotimespC(len,len, {
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
        });
      }
      return carry;
    #endif
  }

# Multiplikations-Einfachschleife mit Diminution:
# Multipliziert eine UDS mit einem Digit und subtrahiert das Ergebnis von
# einer zweiten UDS.
# mulusub_loop_down(digit,sourceptr,destptr,len);
# multipliziert die UDS  sourceptr[-len..-1]  (len>0)  mit dem einzelnen
# digit, subtrahiert das Ergebnis von der UDS  destptr[-len..-1]  und liefert
# den weiteren Übertrag (>=0, evtl. von destptr[-len-1] zu subtrahieren).
  inline local uintD mulusub_loop_down (uintD digit,
                                        const uintD* sourceptr, uintD* destptr,
                                        uintC len)
  {
    #if HAVE_DD
      var uintDD carry = 0;
      if (!(digit==0)) {
        dotimespC(len,len, {
          # Hier ist 0 <= carry <= digit.
          carry = carry + muluD(digit,*--sourceptr) + (uintD)(~(*--destptr));
          # Hier ist 0 <= carry <= 2^intDsize*digit + 2^intDsize-1.
          *destptr = ~lowD(carry);
          carry = (uintDD)highD(carry); # carry := floor(carry/2^intDsize) <= digit
          # Hier ist 0 <= carry <= digit.
        });
        return lowD(carry);
      } else
        return 0; # nichts zu subtrahieren -> kein Übertrag
    #else
      var uintD carry = 0;
      if (!(digit==0)) {
        dotimespC(len,len, {
          # Hier ist 0 <= carry <= digit.
          var uintD hi;
          var uintD lo;
          muluD(digit,*--sourceptr,hi=,lo=);
          # Hier ist 0 <= 2^intDsize*hi + lo + carry + ~(*--destptr) <= 2^intDsize*digit+2^intDsize-1.
          lo += carry; if (lo < carry) { hi += 1; }
          carry = *--destptr;
          *destptr = carry - lo; if (carry < lo) { hi += 1; }
          carry = hi;
        });
        return carry;
      } else
        return 0; # nichts zu subtrahieren -> kein Übertrag
    #endif
  }

#endif

#ifndef DIV_LOOPS

# Divisions-Einfachschleife:
# Dividiert eine UDS durch ein Digit.
# divu_loop_up(digit,ptr,len)
# dividiert die UDS  ptr[0..len-1] durch digit,
# legt das Ergebnis in derselben UDS ab, und liefert den Rest (>=0, <digit).
  inline local uintD divu_loop_up (uintD digit, uintD* ptr, uintC len)
  {
    #if HAVE_DD
      var uintD rest = 0;
      dotimesC(len,len, {
        divuD(highlowDD(rest,*ptr),digit,*ptr =, rest =); ptr++;
      });
      return rest;
    #else
      var uintD rest = 0;
      dotimesC(len,len, {
        divuD(rest,*ptr,digit,*ptr =, rest =); ptr++;
      });
      return rest;
    #endif
  }

# Divisions-Einfachschleife:
# Dividiert eine UDS durch ein Digit und legt das Ergebnis in einer
# zweiten UDS ab.
# divucopy_loop_up(digit,sourceptr,destptr,len)
# dividiert die UDS  sourceptr[0..len-1]  durch digit,
# legt das Ergebnis in der UDS  destptr[0..len-1]  ab,
# und liefert den Rest (>=0, <digit).
  inline local uintD divucopy_loop_up (uintD digit,
                                       const uintD* sourceptr, uintD* destptr,
                                       uintC len)
  {
    #if HAVE_DD
      var uintD rest = 0;
      dotimesC(len,len, {
        divuD(highlowDD(rest,*sourceptr++),digit,*destptr++ =, rest =);
      });
      return rest;
    #else
      var uintD rest = 0;
      dotimesC(len,len, {
        divuD(rest,*sourceptr++,digit,*destptr++ =, rest =);
      });
      return rest;
    #endif
  }

#endif

