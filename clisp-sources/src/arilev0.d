# Arithmetik, Level 0
# operiert auf einzelnen 16-Bit-Wörtern und 32-Bit-Wörtern (unsigned).

# Vorzeichen einer 32-Bit-Zahl bestimmen
# sign_of_sint32(wert)
# > wert: eine 32-Bit-Zahl
# < sint16 ergebnis: 0 falls wert>=0, -1 falls wert<0.
  extern sint16 sign_of_sint32 (sint32 wert);
# im 68000-Assembler (Input D0.L, Output D0.W):
#   SWAP D0  # Vorzeichen nach Bit 15 schieben
#   EXT.L D0 # nach Bit 31..16 kopieren
#   SWAP D0  # nach Bit 15..0 schieben
  #if defined(GNU) && defined(MC680X0) && !defined(NO_ASM)
    #define sign_of_sint32(wert)  \
      ({var sint32 __wert = (wert);  \
        var sint16 __ergebnis;        \
        __asm__ ("\
          swap %0;  \
          extl %0;  \
          swap %0   \
          " : "=d" (__ergebnis) : "0" (__wert));  \
        __ergebnis;                               \
       })
  #elif defined(SPARC64)
    #define sign_of_sint32(wert)  (((sint64)(sint32)(wert)) >> 63)
  #elif defined(SPARC) || defined(ARM)
    #define sign_of_sint32(wert)  (((sint32)(wert)) >> 31)
  #else
    #define sign_of_sint32(wert)  ((sint32)(wert) >= 0 ? 0 : -1)
  #endif

# Vorzeichen einer 16-Bit-Zahl bestimmen
# sign_of_sint16(wert)
# > wert: eine 16-Bit-Zahl
# < sint16 ergebnis: 0 falls wert>=0, -1 falls wert<0.
  extern sint16 sign_of_sint16 (sint16 wert);
# im 68000-Assembler (Input D0.W, Output D0.W):
#   EXT.L D0 # Vorzeichen nach Bit 31..16 kopieren
#   SWAP D0  # nach Bit 15..0 schieben
  #if defined(GNU) && defined(MC680X0) && !defined(NO_ASM)
    #define sign_of_sint16(wert)  \
      ({var sint16 __wert = (wert);  \
        var sint16 __ergebnis;        \
        __asm__ ("\
          extl %0;  \
          swap %0   \
          " : "=d" (__ergebnis) : "0" (__wert));  \
        __ergebnis;                               \
       })
  #elif defined(SPARC64)
    #define sign_of_sint16(wert)  (((sint64)(sint16)(wert)) >> 63)
  #elif defined(SPARC) || defined(ARM)
    #define sign_of_sint16(wert)  (((sint32)(sint16)(wert)) >> 31)
  #else
    #define sign_of_sint16(wert)  ((sint16)(wert) >= 0 ? 0 : -1)
  #endif

# High-Word einer 32-Bit-Zahl bestimmen
# high16(wert)
  extern uint16 high16 (uint32 wert);
# im 68000-Assembler (Input D0.L, Output D0.W):
#   SWAP D0
  #ifdef GNU
    #if defined(MC680X0) && !defined(NO_ASM)
      #define high16(wert)  \
        ({var uint32 __wert = (wert);  \
          var uint16 __ergebnis;        \
          __asm__ ("\
            swap %0   \
            " : "=d" (__ergebnis) : "0" (__wert));  \
          __ergebnis;                               \
         })
    #endif
  #endif
  #ifndef high16
    #define high16(wert)  ((uint16)((uint32)(wert)>>16))
  #endif

# Low-Word einer 32-Bit-Zahl bestimmen
# low16(wert)
  extern uint16 low16 (uint32 wert);
  #define low16(wert)  ((uint16)(uint32)(wert))

# Eine 32-Bit-Zahl aus ihrem High-Word und ihrem Low-Word bestimmen:
# highlow32(uint16 high, uint16 low)
  extern uint32 highlow32 (uint16 high, uint16 low);
# im 68000-Assembler (Input D0.W,D1.W, Output D0.L):
#   SWAP D0
#   MOVE.W D1,D0
  #ifdef GNU
    #if defined(MC680X0) && !defined(NO_ASM)
      #define highlow32(high,low)  \
        ({var uint16 __high = (high);  \
          var uint16 __low = (low);    \
          var uint32 __ergebnis;       \
          __asm__ __volatile__ ("\
            swap %0;     \
            movew %2,%0  \
            " : "=&d" (__ergebnis) : "0" (__high), "g" (__low));  \
          __ergebnis;                                             \
         })
    #endif
  #endif
  #ifndef highlow32
    #define highlow32(high,low)  \
      (((uint32)(uint16)(high) << 16) | (uint32)(uint16)(low))
  #endif

# Eine 32-Bit-Zahl aus ihrem High-Word und ihrem Low-Word 0 bestimmen:
# highlow32_0(uint16 high)
  extern uint32 highlow32_0 (uint16 high);
  # define highlow32_0(high)  highlow32(high,0)
# im 68000-Assembler (Input D0.W,D1.W, Output D0.L):
#   SWAP D0
#   CLR.W D0
  #ifdef GNU
    #if defined(MC680X0) && !defined(NO_ASM)
      #define highlow32_0(high)  \
        ({var uint16 __high = (high);  \
          var uint32 __ergebnis;       \
          __asm__ __volatile__ ("\
            swap %0;     \
            clrw %0      \
            " : "=d" (__ergebnis) : "0" (__high));  \
          __ergebnis;                               \
         })
    #endif
  #endif
  #ifndef highlow32_0
    #define highlow32_0(high)  ((uint32)(uint16)(high) << 16)
  #endif

#if (intVsize>32)

# High-Word einer 64-Bit-Zahl bestimmen
# high32(wert)
  extern uint32 high32 (uint64 wert);
  #define high32(wert)  ((uint32)((uint64)(wert)>>32))

# Low-Word einer 64-Bit-Zahl bestimmen
# low32(wert)
  extern uint32 low32 (uint64 wert);
  #define low32(wert)  ((uint32)(uint64)(wert))

# Eine 64-Bit-Zahl aus ihrem High-Word und ihrem Low-Word bestimmen:
# highlow64(uint32 high, uint32 low)
  extern uint64 highlow64 (uint32 high, uint32 low);
  #define highlow64(high,low)  \
    (((uint64)(uint32)(high) << 32) | (uint64)(uint32)(low))

# Eine 64-Bit-Zahl aus ihrem High-Word und ihrem Low-Word 0 bestimmen:
# highlow64_0(uint32 high)
  extern uint64 highlow64_0 (uint32 high);
  #define highlow64_0(high)  ((uint64)(uint32)(high) << 32)

#endif

# Multipliziert zwei 16-Bit-Zahlen miteinander und liefert eine 32-Bit-Zahl:
# mulu16(arg1,arg2)
# > arg1, arg2 : zwei 16-Bit-Zahlen
# < ergebnis: eine 32-Bit-Zahl
  extern uint32 mulu16 (uint16 arg1, uint16 arg2);
# in 68000-Assembler (Input D0.W, D1.W, Output D0.L):
#   MULU D1,D0
  #if defined(GNU) || defined(INTEL)
    #if defined(SPARC) && !defined(SPARC64) && defined(FAST_DOUBLE) # Ist das schneller als _mulu16 ??
      #define mulu16(arg1,arg2)  \
        ({var union { double f; uint32 i[2]; } __fi; \
          __fi.f = (double)(sint32)(uint16)(arg1)*(double)(sint32)(uint16)(arg2) \
                   + (double)(4503599627370496.0L); # + 2^52, zum Normalisieren  \
          __fi.i[1]; # untere 32 Bit herausholen (benutzt BIG_ENDIAN_P !)        \
         })
    #elif defined(SPARC64) && !defined(NO_ASM)
      #define mulu16(arg1,arg2)  \
        ({ var register uint64 _result;                         \
           __asm__("umul %1,%2,%0"                              \
                   : "=&r" (_result)                            \
                   : "r" ((uint16)(arg1)), "r" ((uint16)(arg2)) \
                  );                                            \
           (uint32)_result;                                     \
         })
    #elif defined(I80386) && !defined(NO_ASM)
      #define mulu16(arg1,arg2)  \
        ({ var register uint16 _hi;                                         \
           var register uint16 _lo;                                         \
           __asm__("mulw %2"                                                \
                   : "=d" /* %dx */ (_hi), "=a" /* %ax */ (_lo)             \
                   : "rm" ((uint16)(arg1)), "1" /* %eax */ ((uint16)(arg2)) \
                  );                                                        \
           highlow32(_hi,_lo);                                              \
         })
    #endif
  #else
    #if defined(SPARC) || defined(SPARC64)
      extern_C uint32 mulu16_ (uint16 arg1, uint16 arg2); # extern in Assembler
      #define mulu16  mulu16_
    #endif
  #endif
  #ifndef mulu16
    #define mulu16(arg1,arg2)  ((uint32)(uint16)(arg1)*(uint32)(uint16)(arg2))
  #endif

# Multipliziert zwei 24-Bit-Zahlen zusammen und liefert eine 48-Bit-Zahl.
# mulu24(arg1,arg2,hi=,lo=);
# > arg1, arg2 : zwei 24-Bit-Zahlen
# < 2^32*hi+lo : eine 48-Bit-Zahl
  #if defined(SPARC) && !defined(SPARC64) && defined(FAST_DOUBLE)
    #define mulu24(x,y,hi_zuweisung,lo_zuweisung)  \
      { var uint32 _x = (x);                                                            \
        var uint32 _y = (y);                                                            \
        var union { double f; uint32 i[2]; uint16 s[4]; } __fi;                         \
        __fi.f = (double)(sint32)(_x)*(double)(sint32)(_y)                              \
                 + (double)(4503599627370496.0L); # + 2^52, zum Normalisieren           \
        hi_zuweisung __fi.s[1]; # mittlere 16 Bit herausholen, (benutzt BIG_ENDIAN_P !) \
        lo_zuweisung __fi.i[1]; # untere 32 Bit herausholen (benutzt BIG_ENDIAN_P !)    \
      }
  #elif defined(MC680X0) && !defined(MC680Y0)
    # Methode:
    # Sei x = x1*2^16+x0, y = y1*2^16+y0  mit
    # 0 <= x0,y0 < 2^16, 0 <= x1,y1 < 2^8 . Dann ist das Produkt x*y
    # >=0 und < 2^48. Es belegt also 3 16-Bit-Worte.
    # In  x * y = x1*y1*2^32 + (x1*y0+x0*y1)*2^16 + x0*y0
    # bestimmen die ersten beiden Summanden und das High-Word des dritten
    # Summanden die beiden höherwertigen Words des Ergebnisses (und
    # hierbei gibt es keinen Überlauf, da das Produkt in 3 Words passt!),
    # das Low-Word des dritten Summanden ist auch das des Ergebnisses.
    #define mulu24(x,y,hi_zuweisung,lo_zuweisung)  \
      { var uint32 _x = (x);                                       \
        var uint32 _y = (y);                                       \
        var uint32 _erg21; # Teilresultat für Words 2 und 1 des Ergebnisses \
        var uint32 _erg10; # Teilresultat für Words 1 und 0 des Ergebnisses \
        { var uint16 _x1 = high16(_x);                             \
          var uint16 _y1 = high16(_y);                             \
          _erg21 = highlow32_0(mulu16(_x1,_y1))                    \
                  + mulu16(_x1,low16(_y)) + mulu16(low16(_x),_y1); \
        }                                                          \
        _erg10 = mulu16(low16(_x),low16(_y));                      \
        # Teilresultate kombinieren:                               \
        _erg21 += high16(_erg10);                                  \
        hi_zuweisung high16(_erg21);                               \
        lo_zuweisung highlow32(low16(_erg21),low16(_erg10));       \
      }
  #else
    #define mulu24  mulu32
  #endif

# Multipliziert zwei 32-Bit-Zahlen miteinander und liefert eine 64-Bit-Zahl:
# mulu32(arg1,arg2,hi=,lo=);
# > arg1, arg2 : zwei 32-Bit-Zahlen
# < 2^32*hi+lo : eine 64-Bit-Zahl
  extern_C uint32 mulu32_ (uint32 arg1, uint32 arg2); # -> Low-Teil
  extern uint32 mulu32_high;                          # -> High-Teil
# in 68000-Assembler (Input D0.L,D1.L, Output D0.L,D1.L, verändert D2-D4):
#   ; D0.L = 2^16*a+b, D1.L = 2^16*c+d -> Produkt
#   ; (2^16*a+b)*(2^16*c+d) = 2^32*a*c + 2^16*(a*d+b*c) + b*d
#   MOVE.L D0,D2 ! SWAP D2
#   MOVE.L D1,D3 ! SWAP D1
#   MOVE.L D1,D4 ! MULU D2,D1 ; a*c
#   MULU D3,D2 ; a*d
#   MULU D0,D4 ; b*c
#   MULU D3,D0 ; b*d
#   CLR.L D3 ; Hilfsregister für Zero-Extend
#   SWAP D2 ! MOVE.W D2,D3 ! ADD.L D3,D1 ; high16(a*d) zu D1.L addieren
#   SWAP D4 ! MOVE.W D4,D3 ! ADD.L D3,D1 ; high16(b*c) zu D1.L addieren
#   CLR.W D2 ! ADD.L D2,D0 ! BCC.S \1 ! ADDQ.L #1,D1 ! \1: ; 2^16*low16(a*d) zu D0.L addieren
#   CLR.W D4 ! ADD.L D4,D0 ! BCC.S \2 ! ADDQ.L #1,D1 ! \2: ; 2^16*low16(b*c) zu D0.L addieren
#   ; D0.L = lo, D1.L = hi fertig.
# in 68020-Assembler (Input D0.L,D1.L, Output D0.L,D1.L):
#   MULU.L D1,D1:D0
  #if defined(GNU) || defined(INTEL)
    #ifdef MC680X0
      #if !(defined(MC680Y0) && !defined(NO_ASM))
        #define mulu32(x,y,hi_zuweisung,lo_zuweisung)  \
          ({ var uint32 _x = (x);                                          \
             var uint32 _y = (y);                                          \
             var uint16 _x1 = high16(_x);                                  \
             var uint16 _x0 = low16(_x);                                   \
             var uint16 _y1 = high16(_y);                                  \
             var uint16 _y0 = low16(_y);                                   \
             var uint32 _hi = mulu16(_x1,_y1); # obere Portion             \
             var uint32 _lo = mulu16(_x0,_y0); # untere Portion            \
             {var uint32 _mid = mulu16(_x0,_y1); # 1. mittlere Portion     \
              _hi += high16(_mid); _mid = highlow32_0(low16(_mid));        \
              _lo += _mid; if (_lo < _mid) { _hi += 1; } # 64-Bit-Addition \
             }                                                             \
             {var uint32 _mid = mulu16(_x1,_y0); # 2. mittlere Portion     \
              _hi += high16(_mid); _mid = highlow32_0(low16(_mid));        \
              _lo += _mid; if (_lo < _mid) { _hi += 1; } # 64-Bit-Addition \
             }                                                             \
             hi_zuweisung _hi;                                             \
             lo_zuweisung _lo;                                             \
           })
      #else
        #define mulu32(x,y,hi_zuweisung,lo_zuweisung)  \
          ({ var uint32 _x = (x); \
             var uint32 _y = (y); \
             var uint32 _hi;      \
             var uint32 _lo;      \
             __asm__("mulul %3,%0:%1" : "=d" (_hi), "=d"(_lo) : "1" (_x), "dm" (_y) ); \
             hi_zuweisung _hi;    \
             lo_zuweisung _lo;    \
           })
      #endif
    #elif defined(ARM) && 0 # see comment ariarm.d
      #define mulu32(x,y,hi_zuweisung,lo_zuweisung)  \
        ({ lo_zuweisung mulu32_(x,y); # extern in Assembler \
          {var register uint32 _hi __asm__("%r1"/*"%a2"*/); \
           hi_zuweisung _hi;                                \
         }})
    #elif defined(I80386) && !defined(NO_ASM)
      #define mulu32(x,y,hi_zuweisung,lo_zuweisung)  \
        ({ var register uint32 _hi;                                   \
           var register uint32 _lo;                                   \
           __asm__("mull %2"                                          \
                   : "=d" /* %edx */ (_hi), "=a" /* %eax */ (_lo)     \
                   : "rm" ((uint32)(x)), "1" /* %eax */ ((uint32)(y)) \
                  );                                                  \
           hi_zuweisung _hi; lo_zuweisung _lo;                        \
         })
    #elif defined(SPARC64) && !defined(NO_ASM)
      #define mulu32(x,y,hi_zuweisung,lo_zuweisung)  \
        ({ var register uint64 _hi;                            \
           var register uint64 _lo;                            \
           __asm__("umul %2,%3,%0\n\trd %%y,%1"                \
                   : "=&r" (_lo), "=r" (_hi)                   \
                   : "r" ((uint32)(x)), "r" ((uint32)(y))      \
                  );                                           \
           hi_zuweisung (uint32)_hi; lo_zuweisung (uint32)_lo; \
         })
    #elif defined(MIPS) && !defined(NO_ASM)
      #define mulu32(x,y,hi_zuweisung,lo_zuweisung)  \
        ({ var register uint32 _hi;                       \
           var register uint32 _lo;                       \
           __asm__("multu %3,%2 ; mfhi %0 ; mflo %1"      \
                   : "=r" (_hi), "=r" (_lo)               \
                   : "r" ((uint32)(x)), "r" ((uint32)(y)) \
                  );                                      \
           hi_zuweisung _hi; lo_zuweisung _lo;            \
         })
    #elif defined(SPARC)
      #define mulu32(x,y,hi_zuweisung,lo_zuweisung)  \
        ({ lo_zuweisung mulu32_(x,y); # extern in Assembler \
          {var register uint32 _hi __asm__("%g1");          \
           hi_zuweisung _hi;                                \
         }})
    #elif defined(HAVE_LONGLONG) && !defined(ARM)
      #define mulu32(x,y,hi_zuweisung,lo_zuweisung)  \
        ({ var register uint64 _prod = (uint64)(x) * (uint64)(y); \
           hi_zuweisung (uint32)(_prod>>32);                      \
           lo_zuweisung (uint32)(_prod);                          \
         })
    #endif
  #endif
  #ifndef mulu32
    #define mulu32(x,y,hi_zuweisung,lo_zuweisung)  \
      { lo_zuweisung mulu32_(x,y); hi_zuweisung mulu32_high; }
    #if defined(MC680X0) || defined(SPARC) || defined(SPARC64) || defined(ARM) || (defined(I80386) && !defined(BORLAND)) || defined(MIPS) || defined(HPPA) || defined(VAX)
      # mulu32_ extern in Assembler
      #if defined(SPARC) || defined(SPARC64)
        #define mulu32_high  (uint32)(_get_g1()) # Rückgabe im Register %g1
      #elif defined(LISPARIT) && !defined(HPPA) # In arihppa.d ist mulu32_high bereits definiert.
        global uint32 mulu32_high;
      #endif
    #else
      #ifdef LISPARIT
      global uint32 mulu32_high;
      global uint32 mulu32_ (uint32 x, uint32 y) {
        var uint16 x1 = high16(x);
        var uint16 x0 = low16(x);
        var uint16 y1 = high16(y);
        var uint16 y0 = low16(y);
        var uint32 hi = mulu16(x1,y1); # obere Portion
        var uint32 lo = mulu16(x0,y0); # untere Portion
        {
          var uint32 mid = mulu16(x0,y1); # 1. mittlere Portion
          hi += high16(mid); mid = highlow32_0(low16(mid));
          lo += mid; if (lo < mid) { hi += 1; } # 64-Bit-Addition
        }
        {
          var uint32 mid = mulu16(x1,y0); # 2. mittlere Portion
          hi += high16(mid); mid = highlow32_0(low16(mid));
          lo += mid; if (lo < mid) { hi += 1; } # 64-Bit-Addition
        }
        mulu32_high = hi; return lo;
      }
      #endif
    #endif
  #endif

# Multipliziert zwei 32-Bit-Zahlen miteinander und liefert eine 32-Bit-Zahl:
# mulu32_unchecked(arg1,arg2)
# > arg1, arg2 : zwei 32-Bit-Zahlen
# < ergebnis : eine 32-Bit-Zahl
# Es wird vorausgesetzt, dass arg1*arg2 < 2^32.
  #if (defined(GNU) && defined(MC680X0) && !defined(MC680Y0))
    extern uint32 mulu32_unchecked (uint32 x, uint32 y);
    #ifdef LISPARIT
    global uint32 mulu32_unchecked (uint32 x, uint32 y) {
      # Methode:
      # Falls x>=2^16 und y>=2^16 wäre, wäre das Produkt zu groß.
      # Falls x<2^16 : y = y1*2^16+y0 schreiben, (x*y1)*2^16 + (x*y0) bilden.
      # Falls y<2^16 : x = x1*2^16+x0 schreiben, (x1*y)*2^16 + (x0*y) bilden.
      # Falls sogar x<2^16 und y<2^16: nur x*y bilden.
      var uint16 x1 = high16(x);
      var uint16 y1 = high16(y);
      if (x1==0)
        if (y1==0)
          return mulu16((uint16)(x),(uint16)(y));
        else
          return highlow32_0(mulu16((uint16)(x),y1))
                 + mulu16((uint16)(x),low16(y));
      else
        return highlow32_0(mulu16(x1,(uint16)(y)))
               + mulu16(low16(x),(uint16)(y));
    }
    #endif
  #elif defined(GNU) && defined(SPARC64) && !defined(NO_ASM)
    #define mulu32_unchecked(arg1,arg2)  \
      ({ var register uint64 _result;                         \
         __asm__("umul %1,%2,%0"                              \
                 : "=&r" (_result)                            \
                 : "r" ((uint32)(arg1)), "r" ((uint32)(arg2)) \
                );                                            \
         (uint32)_result;                                     \
       })
  #elif defined(SPARC)
    extern_C uint32 mulu32_unchecked (uint32 x, uint32 y); # extern in Assembler
  #else
    # Wir können dafür auch die Bibliotheksroutine des C-Compilers nehmen:
    #define mulu32_unchecked(x,y)  ((uint32)((uint32)(x)*(uint32)(y)))
  #endif

#if (intVsize>32)

# Multipliziert zwei 32-Bit-Zahlen miteinander und liefert eine 64-Bit-Zahl:
# mulu32_64(arg1,arg2)
# > arg1, arg2 : zwei 32-Bit-Zahlen
# < ergebnis : eine 64-Bit-Zahl
  extern_C uint64 mulu32_64 (uint32 arg1, uint32 arg2);
  #if defined(GNU) || defined(INTEL)
    #if defined(I80386) && !defined(NO_ASM)
      #define mulu32_64(x,y)  \
        ({ var register uint32 _hi;                                   \
           var register uint32 _lo;                                   \
           __asm__("mull %2"                                          \
                   : "=d" /* %edx */ (_hi), "=a" /* %eax */ (_lo)     \
                   : "rm" ((uint32)(x)), "1" /* %eax */ ((uint32)(y)) \
                  );                                                  \
           highlow64(_hi,_lo);                                        \
         })
    #elif defined(SPARC64) && !defined(NO_ASM)
      #define mulu32_64(x,y)  \
        ({ var register uint64 _hi;                            \
           var register uint64 _lo;                            \
           __asm__("umul %2,%3,%0\n\trd %%y,%1"                \
                   : "=&r" (_lo), "=r" (_hi)                   \
                   : "r" ((uint32)(x)), "r" ((uint32)(y))      \
                  );                                           \
           highlow64((uint32)_hi,(uint32)_lo);                 \
         })
    #elif defined(MIPS) && !defined(NO_ASM)
      #define mulu32_64(x,y)  \
        ({ var register uint32 _hi;                       \
           var register uint32 _lo;                       \
           __asm__("multu %3,%2 ; mfhi %0 ; mflo %1"      \
                   : "=r" (_hi), "=r" (_lo)               \
                   : "r" ((uint32)(x)), "r" ((uint32)(y)) \
                  );                                      \
           highlow64(_hi,_lo);                            \
         })
    #elif defined(SPARC)
      #define mulu32_64(x,y)  \
        ({ var register uint32 _lo = mulu32_(x,y); # extern in Assembler \
          {var register uint32 _hi __asm__("%g1");                       \
           highlow64(_hi,_lo);                                           \
         }})
    #endif
  #endif
  #ifndef mulu32_64
    #define mulu32_64(x,y)  \
      ((uint64)(x) * (uint64)(y))
  #endif

#endif

# Dividiert eine 16-Bit-Zahl durch eine 16-Bit-Zahl und
# liefert einen 16-Bit-Quotienten und einen 16-Bit-Rest.
# divu_1616_1616(x,y,q=,r=);
# > uint16 x: Zähler
# > uint16 y: Nenner
# < uint16 q: floor(x/y)
# < uint16 r: x mod y
# < x = q*y+r
  #define divu_1616_1616(x,y,q_zuweisung,r_zuweisung)  \
    { var uint16 __x = (x);       \
      var uint16 __y = (y);       \
      q_zuweisung floor(__x,__y); \
      r_zuweisung (__x % __y);    \
    }

# Dividiert eine 32-Bit-Zahl durch eine 16-Bit-Zahl und
# liefert einen 16-Bit-Quotienten und einen 16-Bit-Rest.
# divu_3216_1616(x,y,q=,r=);
# > uint32 x: Zähler
# > uint16 y: Nenner
# > Es sei bekannt, dass 0 <= x < 2^16*y .
# < uint16 q: floor(x/y)
# < uint16 r: x mod y
# < x = q*y+r
  #if defined(SPARC)
    extern_C uint32 divu_3216_1616_ (uint32 x, uint16 y); # -> Quotient q, Rest r
  #else
    extern_C uint16 divu_3216_1616_ (uint32 x, uint16 y); # -> Quotient q
    extern uint16 divu_16_rest;                           # -> Rest r
  #endif
# im 68000-Assembler (Input D0.L,D1.W, Output D0.W,D1.W):
#   DIVU D1,D0    ; D0.L=x / D1.W=y -> q=D0.W, r=D0.H.W
#   MOVE.L D0,D1
#   SWAP D1
  #if defined(GNU) || defined(INTEL)
    #if defined(SPARC64) && !defined(NO_ASM)
      #define divu_3216_1616(x,y,q_zuweisung,r_zuweisung)  \
        ({var uint32 __x = (x);        \
          var uint16 __y = (y);        \
          var uint64 __q;              \
          var uint64 __r;              \
          __asm__ __volatile__ (       \
            "wr %%g0,%%g0,%%y\n\t"     \
            "udiv %2,%3,%0\n\t"        \
            "umul %0,%3,%1\n\t"        \
            "sub %2,%1,%1"             \
            : "=&r" (__q), "=&r" (__r) \
            : "r" (__x), "r" (__y));   \
          q_zuweisung (uint16)__q;     \
          r_zuweisung (uint16)__r;     \
         })
    #elif defined(SPARC) || defined(SPARC64)
      #define divu_3216_1616(x,y,q_zuweisung,r_zuweisung)  \
        ({ var uint32 __qr = divu_3216_1616_(x,y); # extern in Assembler \
           q_zuweisung low16(__qr);  \
           r_zuweisung high16(__qr); \
         })
    #elif defined(MC680X0) && !defined(NO_ASM)
      #define divu_3216_1616(x,y,q_zuweisung,r_zuweisung)  \
        ({var uint32 __x = (x);      \
          var uint16 __y = (y);      \
          var uint32 __qr;           \
          __asm__ __volatile__ ("\
            divu %2,%0   \
            " : "=d" (__qr) : "0" (__x), "dm" (__y));  \
          q_zuweisung low16(__qr);   \
          r_zuweisung high16(__qr);  \
         })
    #elif defined(I80386) && !defined(NO_ASM)
      #define divu_3216_1616(x,y,q_zuweisung,r_zuweisung)  \
        ({var uint32 __x = (x);  \
          var uint16 __y = (y);  \
          var uint16 __q;        \
          var uint16 __r;        \
          __asm__("divw %4"      \
                  : "=a" /* %ax */ (__q), "=d" /* %dx */ (__r) \
                  : "1" /* %dx */ ((uint16)(high16(__x))), "0" /* %ax */ ((uint16)(low16(__x))), "rm" (__y) \
                 );              \
          q_zuweisung __q;       \
          r_zuweisung __r;       \
         })
    #elif defined(ARM) && 0 # see comment ariarm.d
      #define divu_3216_1616(x,y,q_zuweisung,r_zuweisung)  \
        { var uint32 _q = divu_3216_1616_(x,y); # extern in Assembler \
          var register uint32 _r __asm__("%r1"/*"%a2"*/);             \
          q_zuweisung _q; r_zuweisung _r;                             \
        }
    #elif !defined(ARM)
      #define divu_3216_1616(x,y,q_zuweisung,r_zuweisung)  \
        ({var uint32 __x = (x);            \
          var uint16 __y = (y);            \
          var uint16 __q = floor(__x,__y); \
          q_zuweisung __q;                 \
          r_zuweisung (__x - __q * __y);   \
         })
    #endif
  #endif
  #ifndef divu_3216_1616
    #if defined(SPARC) || defined(SPARC64)
      #define divu_3216_1616(x,y,q_zuweisung,r_zuweisung)  \
        { var uint32 __qr = divu_3216_1616_(x,y); # extern in Assembler \
          q_zuweisung low16(__qr);  \
          r_zuweisung high16(__qr); \
        }
    #elif defined(ARM)
      #define divu_3216_1616(x,y,q_zuweisung,r_zuweisung)  \
        { q_zuweisung divu_3216_1616_(x,y); # extern in Assembler \
          r_zuweisung divu_16_rest;                               \
        }
      #ifdef LISPARIT
      global uint16 divu_16_rest;
      #endif
    #else
      #define divu_3216_1616(x,y,q_zuweisung,r_zuweisung)  \
        { q_zuweisung divu_3216_1616_(x,y); r_zuweisung divu_16_rest; }
      #ifdef LISPARIT
      global uint16 divu_16_rest;
      global uint16 divu_3216_1616_ (uint32 x, uint16 y) {
        var uint16 q = floor(x,(uint32)y);
        divu_16_rest = x - (uint32)q * (uint32)y;
        return q;
      }
      #endif
    #endif
  #endif

# Dividiert eine 32-Bit-Zahl durch eine 16-Bit-Zahl und
# liefert einen 32-Bit-Quotienten und einen 16-Bit-Rest.
# divu_3216_3216(x,y,q=,r=);
# > uint32 x: Zähler
# > uint16 y: Nenner
# Es sei bekannt, dass y>0.
# < uint32 q: floor(x/y)
# < uint16 r: x mod y
# < x = q*y+r
  extern_C uint32 divu_3216_3216_ (uint32 x, uint16 y); # -> Quotient q
  extern uint16 divu_16_rest;                           # -> Rest r
# im 68000-Assembler (Input D0.L,D1.W, Output D0.L,D1.W, verändert D2,D3):
#   MOVE.L D0,D2
#   CLR.W D2
#   SWAP D2        ; D2.L = D2.W := D0.H.W = high16(x)
#   DIVU D1,D2     ; durch y dividieren
#   MOVE.W D2,D3   ; Quotient nach D3.W
#   MOVE.W D0,D2   ; Rest (in D2.H.W) mit D0.W = low16(x) kombinieren
#   DIVU D1,D2     ; und wieder durch y dividieren
#   MOVE.W D3,D0   ; ersten Quotiententeil
#   SWAP D0        ; mal 2^16
#   MOVE.W D2,D0   ; plus zweiten Quotiententeil, liefert q
#   SWAP D2
#   MOVE.W D2,D1   ; r = Rest der zweiten Division
# oder (Input D0.L,D1.W, Output D0.L,D1.W, verändert D2,D3):
#   MOVE.L D0,D2   ; x retten
#   CLR.W D0
#   SWAP D0        ; D0.L = D0.W := high16(x)
#   DIVU D1,D0     ; durch y dividieren
#   MOVE.W D0,D3   ; Quotient nach D3.W
#   MOVE.W D2,D0   ; Rest (in D0.H.W) mit D2.W = low16(x) kombinieren
#   DIVU D1,D0     ; und wieder durch y dividieren
#   SWAP D0
#   MOVE.W D0,D1   ; r = Rest der zweiten Division
#   MOVE.W D3,D0
#   SWAP D0        ; beide Quotienten kombinieren, liefert q
  #if defined(GNU) && defined(SPARC64) && !defined(NO_ASM)
    #define divu_3216_3216(x,y,q_zuweisung,r_zuweisung)  \
      ({var uint32 __x = (x);        \
        var uint16 __y = (y);        \
        var uint64 __q;              \
        var uint64 __r;              \
        __asm__ __volatile__ (       \
          "wr %%g0,%%g0,%%y\n\t"     \
          "udiv %2,%3,%0\n\t"        \
          "umul %0,%3,%1\n\t"        \
          "sub %2,%1,%1"             \
          : "=&r" (__q), "=&r" (__r) \
          : "r" (__x), "r" (__y));   \
        q_zuweisung (uint32)__q;     \
        r_zuweisung (uint16)__r;     \
       })
  #elif defined(SPARC) || defined(SPARC64) || defined(I80386)
    #define divu_3216_3216  divu_3232_3232
  #elif 1
    # Methode: (beta = 2^16)
    # x = x1*beta+x0 schreiben.
    # Division mit Rest: x1 = q1*y + r1, wobei 0 <= x1 < beta <= beta*y.
    # Also 0 <= q1 < beta, 0 <= r1 < y.
    # Division mit Rest: (r1*beta+x0) = q0*y + r0, wobei 0 <= r1*beta+x0 < beta*y.
    # Also 0 <= q0 < beta, 0 <= r0 < y
    # und x = x1*beta+x0 = (q1*beta+q0)*y + r0.
    # Setze q := q1*beta+q0 und r := r0.
    #ifdef GNU
      #define divu_3216_3216(x,y,q_zuweisung,r_zuweisung)  \
        ({var uint32 _x = (x);            \
          var uint16 _y = (y);            \
          var uint16 _q1;                 \
          var uint16 _q0;                 \
          var uint16 _r1;                 \
          divu_3216_1616(high16(_x),_y, _q1 = , _r1 = ); \
          divu_3216_1616(highlow32(_r1,low16(_x)),_y, _q0 = , _EMA_ r_zuweisung); \
          q_zuweisung highlow32(_q1,_q0); \
         })
    #else
      #define divu_3216_3216(x,y,q_zuweisung,r_zuweisung)  \
        {var uint32 _x = (x);            \
         var uint16 _y = (y);            \
         var uint16 _q1;                 \
         var uint16 _q0;                 \
         var uint16 _r1;                 \
         divu_3216_1616(high16(_x),_y, _q1 = , _r1 = ); \
         divu_3216_1616(highlow32(_r1,low16(_x)),_y, _q0 = , _EMA_ r_zuweisung); \
         q_zuweisung highlow32(_q1,_q0); \
        }
    #endif
  #else
    #define divu_3216_3216(x,y,q_zuweisung,r_zuweisung)  \
      { q_zuweisung divu_3216_3216_(x,y); r_zuweisung divu_16_rest; }
    #if 0
      # divu_3216_3216_ extern in Assembler
    #else
      #ifdef LISPARIT
      global uint32 divu_3216_3216_ (uint32 x, uint16 y) {
        var uint16 q1;
        var uint16 q0;
        var uint16 r1;
        divu_3216_1616(high16(x),y, q1 = , r1 = );
        divu_3216_1616(highlow32(r1,low16(x)),y, q0 = , divu_16_rest =);
        return highlow32(q1,q0);
      }
      #endif
    #endif
  #endif

# Dividiert eine 32-Bit-Zahl durch eine 32-Bit-Zahl und
# liefert einen 32-Bit-Quotienten und einen 32-Bit-Rest.
# divu_3232_3232(x,y,q=,r=);
# > uint32 x: Zähler
# > uint32 y: Nenner
# Es sei bekannt, dass y>0.
# < uint32 q: floor(x/y)
# < uint32 r: x mod y
# < x = q*y+r
  extern_C uint32 divu_3232_3232_ (uint32 x, uint32 y); # -> Quotient q
  extern uint32 divu_32_rest;                           # -> Rest r
  #if defined(GNU) && defined(SPARC64) && !defined(NO_ASM)
    #define divu_3232_3232(x,y,q_zuweisung,r_zuweisung)  \
      ({var uint32 __x = (x);        \
        var uint32 __y = (y);        \
        var uint64 __q;              \
        var uint64 __r;              \
        __asm__ __volatile__ (       \
          "wr %%g0,%%g0,%%y\n\t"     \
          "udiv %2,%3,%0\n\t"        \
          "umul %0,%3,%1\n\t"        \
          "sub %2,%1,%1"             \
          : "=&r" (__q), "=&r" (__r) \
          : "r" (__x), "r" (__y));   \
        q_zuweisung (uint32)__q;     \
        r_zuweisung (uint32)__r;     \
       })
    #define divu_3232_3232_(x,y)  \
      ({var uint32 __x = (x);        \
        var uint32 __y = (y);        \
        var uint64 __q;              \
        __asm__ __volatile__ (       \
          "wr %%g0,%%g0,%%y\n\t"     \
          "udiv %1,%2,%0"            \
          : "=&r" (__q)              \
          : "r" (__x), "r" (__y));   \
        (uint32)__q;                 \
       })
  #elif defined(SPARC) || defined(SPARC64) || defined(I80386) || defined(HPPA_DIV_WORKS)
    #define divu_3232_3232(x,y,q_zuweisung,r_zuweisung)  \
      divu_6432_3232(0,x,y,_EMA_ q_zuweisung,_EMA_ r_zuweisung)
    #define divu_3232_3232_(x,y) divu_6432_3232_(0,x,y)
  #elif 1
    # Methode: (beta = 2^n = 2^16, n = 16)
    # Falls y < beta, handelt es sich um eine 32-durch-16-Bit-Division.
    # Falls y >= beta:
    # Quotient  q = floor(x/y) < beta  (da 0 <= x < beta^2, y >= beta).
    # y habe genau n+k Bits (1 <= k <= n), d.h. 2^(n+k-1) <= y < 2^(n+k).
    # Schreibe  x = 2^k*x1 + x0  mit  x1 := floor(x/2^k)
    # und       y = 2^k*y1 + y0  mit  y1 := floor(y/2^k)
    # und bilde den Näherungs-Quotienten floor(x1/y1)
    # oder (noch besser) floor(x1/(y1+1)).
    # Wegen 0 <= x1 < 2^(2n) und 0 < 2^(n-1) <= y1 < 2^n
    # und  x1/(y1+1) <= x/y < x1/(y1+1) + 2
    # (denn x1/(y1+1) = (x1*2^k)/((y1+1)*2^k) <= (x1*2^k)/y <= x/y
    # und x/y - x1/(y1+1) = (x+x*y1-x1*y)/(y*(y1+1))
    # = (x+x0*y1-x1*y0)/(y*(y1+1)) <= (x+x0*y1)/(y*(y1+1))
    # <= x/(y*(y1+1)) + x0/y
    # <= 2^(2n)/(2^(n+k-1)*(2^(n-1)+1)) + 2^k/2^(n+k-1)
    # = 2^(n-k+1)/(2^(n-1)+1) + 2^(1-n) <= 2^n/(2^(n-1)+1) + 2^(1-n) < 2 )
    # gilt  floor(x1/(y1+1)) <= floor(x/y) <= floor(x1/(y1+1)) + 2  .
    # Man bildet also  q:=floor(x1/(y1+1))  (ein Shift um n Bit oder
    # eine (2n)-durch-n-Bit-Division, mit Ergebnis q <= floor(x/y) < beta)
    # und x-q*y und muss hiervon noch höchstens 2 mal y abziehen und q
    # incrementieren, um den Quotienten  q = floor(x/y)  und den Rest
    # x-floor(x/y)*y  der Division zu bekommen.
    #define divu_3232_3232(x,y,q_zuweisung,r_zuweisung)  \
      { var uint32 _x = (x);                                                    \
        var uint32 _y = (y);                                                    \
        if (_y <= (uint32)(bit(16)-1))                                          \
          { var uint16 _q1;                                                     \
            var uint16 _q0;                                                     \
            var uint16 _r1;                                                     \
            divu_3216_1616(high16(_x),_y, _q1 = , _r1 = );                      \
            divu_3216_1616(highlow32(_r1,low16(_x)),_y, _q0 = , _EMA_ r_zuweisung); \
            q_zuweisung highlow32(_q1,_q0);                                     \
          }                                                                     \
          else                                                                  \
          { var uint32 _x1 = _x; # x1 := x                                      \
            var uint32 _y1 = _y; # y1 := y                                      \
            var uint16 _q;                                                      \
            do { _x1 = floor(_x1,2); _y1 = floor(_y1,2); } # k erhöhen          \
               until (_y1 <= (uint32)(bit(16)-1)); # bis y1 < beta              \
            { var uint16 _y2 = low16(_y1)+1; # y1+1 bilden                      \
              if (_y2==0)                                                       \
                { _q = high16(_x1); } # y1+1=beta -> ein Shift                  \
                else                                                            \
                { divu_3216_1616(_x1,_y2,_q=,); } # Division von x1 durch y1+1  \
            }                                                                   \
            # _q = q = floor(x1/(y1+1))                                         \
            # x-q*y bilden (eine 16-mal-32-Bit-Multiplikation ohne Überlauf):   \
            _x -= highlow32_0(mulu16(_q,high16(_y))); # q * high16(y) * beta    \
            # gefahrlos, da q*high16(y) <= q*y/beta <= x/beta < beta            \
            _x -= mulu16(_q,low16(_y)); # q * low16(y)                          \
            # gefahrlos, da q*high16(y)*beta + q*low16(y) = q*y <= x            \
            # Noch höchstens 2 mal y abziehen:                                  \
            if (_x >= _y)                                                       \
              { _q += 1; _x -= _y;                                              \
                if (_x >= _y)                                                   \
                  { _q += 1; _x -= _y;                                          \
              }   }                                                             \
            r_zuweisung _x;                                                     \
            q_zuweisung (uint32)(_q);                                           \
      }   }
    #ifdef LISPARIT
    # Dies dient nur noch als Hilfsfunktion für arilev1.d.
    # Die Rückgabe des Restes in divu_32_rest ist also hier nicht nötig.
    global uint32 divu_3232_3232_ (uint32 x, uint32 y) {
      var uint32 q;
      divu_3232_3232(x,y,q=,unused);
      return q;
    }
    #endif
  #else
    #define divu_3232_3232(x,y,q_zuweisung,r_zuweisung)  \
      { q_zuweisung divu_3232_3232_(x,y); r_zuweisung divu_32_rest; }
    #if 0
      # divu_3232_3232_ extern in Assembler
    #else
      #ifdef LISPARIT
      global uint32 divu_3232_3232_ (uint32 x, uint32 y) {
        var uint32 q = floor(x,y);
        divu_32_rest = x - q*y;
        return q;
      }
      #endif
    #endif
  #endif

# Dividiert eine 64-Bit-Zahl durch eine 32-Bit-Zahl und
# liefert einen 32-Bit-Quotienten und einen 32-Bit-Rest.
# divu_6432_3232(xhi,xlo,y,q=,r=);
# > uint32 xhi,xlo: x = 2^32*xhi+xlo = Zähler
# > uint32 y: Nenner
# > Es sei bekannt, dass 0 <= x < 2^32*y .
# < uint32 q: floor(x/y)
# < uint32 r: x mod y
# < x = q*y+r
  extern_C uint32 divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y); # -> Quotient q
  extern uint32 divu_32_rest;                                         # -> Rest r
  #if defined(GNU) || defined(INTEL)
    #if defined(MC680Y0) && !defined(NO_ASM)
      #define divu_6432_3232(xhi,xlo,y,q_zuweisung,r_zuweisung)  \
        ({var uint32 __xhi = (xhi);  \
          var uint32 __xlo = (xlo);  \
          var uint32 __y = (y);      \
          var uint32 __q;            \
          var uint32 __r;            \
          __asm__ __volatile__ ("\
            divul %4,%1:%0   \
            " : "=d" (__q), "=d" (__r) : "1" (__xhi), "0" (__xlo), "dm" (__y));  \
          q_zuweisung __q;           \
          r_zuweisung __r;           \
         })
      #define divu_6432_3232_(xhi,xlo,y) \
        ({var uint32 ___q; divu_6432_3232(xhi,xlo,y,___q=,); ___q; })
    #elif defined(SPARC64) && !defined(NO_ASM)
      #define divu_6432_3232(xhi,xlo,y,q_zuweisung,r_zuweisung)  \
        ({var uint32 __xhi = (xhi);    \
          var uint32 __xlo = (xlo);    \
          var uint32 __y = (y);        \
          var uint64 __q;              \
          var uint64 __r;              \
          __asm__ __volatile__ (       \
            "wr %2,%%g0,%%y\n\t"       \
            "udiv %3,%4,%0\n\t"        \
            "umul %0,%4,%1\n\t"        \
            "sub %3,%1,%1"             \
            : "=&r" (__q), "=&r" (__r) \
            : "r" (__xhi), "r" (__xlo), "r" (__y)); \
          q_zuweisung (uint32)__q;     \
          r_zuweisung (uint32)__r;     \
         })
    #elif defined(SPARC) || defined(SPARC64)
      #define divu_6432_3232(xhi,xlo,y,q_zuweisung,r_zuweisung)  \
        ({ var uint32 _q = divu_6432_3232_(xhi,xlo,y); # extern in Assembler \
           var register uint32 _r __asm__("%g1");                            \
           q_zuweisung _q; r_zuweisung _r;                                   \
         })
    #elif defined(ARM) && 0 # see comment ariarm.d
      #define divu_6432_3232(xhi,xlo,y,q_zuweisung,r_zuweisung)  \
        ({ var uint32 _q = divu_6432_3232_(xhi,xlo,y); # extern in Assembler \
           var register uint32 _r __asm__("%r1"/*"%a2"*/);                   \
           q_zuweisung _q; r_zuweisung _r;                                   \
         })
    #elif defined(I80386) && !defined(NO_ASM)
      #define divu_6432_3232(xhi,xlo,y,q_zuweisung,r_zuweisung)  \
        ({var uint32 __xhi = (xhi);  \
          var uint32 __xlo = (xlo);  \
          var uint32 __y = (y);      \
          var uint32 __q;            \
          var uint32 __r;            \
          __asm__ __volatile__ (     \
             "divl %4"               \
             : "=a" /* %eax */ (__q), "=d" /* %edx */ (__r)               \
             : "1" /* %edx */ (__xhi), "0" /* %eax */ (__xlo), "rm" (__y) \
             );                      \
          q_zuweisung __q;           \
          r_zuweisung __r;           \
         })
      #define divu_6432_3232_(xhi,xlo,y) \
        ({var uint32 ___q; divu_6432_3232(xhi,xlo,y,___q=,); ___q; })
    #elif defined(HAVE_LONGLONG) && !defined(ARM) && !defined(HPPA_DIV_WORKS)
      #define divu_6432_3232(xhi,xlo,y,q_zuweisung,r_zuweisung) \
        ({var uint32 __xhi = (xhi);                           \
          var uint32 __xlo = (xlo);                           \
          var uint64 __x = (uint64)__xhi<<32 | (uint64)__xlo; \
          var uint32 __y = (y);                               \
          var uint32 __q = floor(__x,(uint64)__y);            \
          q_zuweisung __q; r_zuweisung __xlo - __q * __y;     \
         })
      #define divu_6432_3232_(xhi,xlo,y) \
        ({var uint32 ___q; divu_6432_3232(xhi,xlo,y,___q=,); ___q; })
    #endif
  #endif
  #ifndef divu_6432_3232
    #define divu_6432_3232(xhi,xlo,y,q_zuweisung,r_zuweisung)  \
      { q_zuweisung divu_6432_3232_(xhi,xlo,y); r_zuweisung divu_32_rest; }
    #if defined(MC680Y0) || defined(SPARC) || defined(SPARC64) || defined(ARM) || (defined(I80386) && !defined(BORLAND)) || defined(HPPA)
      # divu_6432_3232_ extern in Assembler
      #if defined(SPARC) || defined(SPARC64)
        #define divu_32_rest  (uint32)(_get_g1()) # Rückgabe im Register %g1
      #elif defined(LISPARIT)
        global uint32 divu_32_rest;
      #endif
    #else
      #ifdef LISPARIT
      # Methode:
      # Wie UDS_divide mit intDsize=16, a_len=4, b_len=2.
      global uint32 divu_32_rest;
      global uint32 divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y) {
        if (y <= (uint32)(bit(16)-1)) {
          # 48-durch-16-Bit-Division,
          # aufgebaut aus zwei 32-durch-16-Bit-Divisionen:
          var uint16 q1;
          var uint16 q0;
          var uint16 r1;
          divu_3216_1616(highlow32(low16(xhi),high16(xlo)),y, q1=,r1=);
          divu_3216_1616(highlow32(r1,low16(xlo)),y, q0=, divu_32_rest=(uint32) );
          return highlow32(q1,q0);
        }
        # y>=2^16
        # y shiften:
        var uintL s = 0;
        while ((sint32)y >= 0) {
          y = y<<1; s++;
        }
        # x entsprechend shiften:
        if (!(s==0)) {
          xhi = (xhi << s) | (xlo >> (32-s)); xlo = xlo << s;
        }
        # 64-durch-32-Bit-Division,
        # aufgebaut aus zwei 48-durch-32-Bit-Divisionen.
        # Methode für eine 48-durch-32-Bit-Division x/y mit 0 <= x < 2^16*y :
        # (beta = 2^n = 2^16, n = 16)
        # Wir wissen beta^2/2 <= y < beta^2, Quotient  q = floor(x/y) < beta.
        # Schreibe  x = beta*x1 + x0  mit  x1 := floor(x/beta)
        # und       y = beta*y1 + y0  mit  y1 := floor(y/beta)
        # und bilde den Näherungs-Quotienten floor(x1/y1)
        # oder (noch besser) floor(x1/(y1+1)).
        # Wegen 0 <= x1 < 2^(2n) und 0 < 2^(n-1) <= y1 < 2^n
        # und  x1/(y1+1) <= x/y < x1/(y1+1) + 2
        # (denn x1/(y1+1) = (x1*beta)/((y1+1)*beta) <= (x1*beta)/y <= x/y
        # und x/y - x1/(y1+1) = (x+x*y1-x1*y)/(y*(y1+1))
        # = (x+x0*y1-x1*y0)/(y*(y1+1)) <= (x+x0*y1)/(y*(y1+1))
        # <= x/(y*(y1+1)) + x0/y = (x/y)/(y1+1) + x0/y
        # <= 2^n/(2^(n-1)+1) + 2^n/2^(2n-1) = 2^n/(2^(n-1)+1) + 2^(1-n) < 2 )
        # gilt  floor(x1/(y1+1)) <= floor(x/y) <= floor(x1/(y1+1)) + 2  .
        # Man bildet also  q:=floor(x1/(y1+1))  (ein Shift um n Bit oder
        # eine (2n)-durch-n-Bit-Division, mit Ergebnis q <= floor(x/y) < beta)
        # und x-q*y und muss hiervon noch höchstens 2 mal y abziehen und q
        # incrementieren, um den Quotienten  q = floor(x/y)  und den Rest
        # x-floor(x/y)*y  der Division zu bekommen.
        var uint16 y1_1 = high16(y)+1; # y1+1
        var uint16 q1;
        var uint16 q0;
        var uint32 r;
        # 2^16*xhi+high16(xlo) durch y dividieren:
        {
          var uint16 r16;
          var uint32 r2;
          if (y1_1==0) {
            q1 = high16(xhi); r16 = low16(xhi);
          } else {
            divu_3216_1616(xhi,y1_1, q1=,r16=);
          }
          # q1 = floor(xhi/(y1+1)), r16 = xhi - (y1+1)*q1 (>=0, <=y1)
          # Bilde r := (2^16*xhi+high16(xlo)) - y*q1
          #          = 2^16*(xhi-y1*q1) + high16(xlo) - y0*q1
          #          = 2^16*r16 + 2^16*q1 + high16(xlo) - y0*q1 (>=0)
          # Dies ist < 2^16*y1 + 2^32 <= y + 2^32 <= 3*y, kann überlaufen!
          r = highlow32(r16,high16(xlo)); # 2^16*r16 + high16(xlo) < 2^32
          r2 = highlow32_0(q1) - mulu16(low16(y),q1); # 2^16*q1 - y0*q1 < 2^32
          # 0 <= r+r2 < 3*y. Bei der Addition auf Carry testen!
          # Carry -> jedenfalls y <= r+r2 < y + 2^32 <= 3*y.
          # kein Carry -> jedenfalls 0 <= r+r2 < 2^32 <= 2*y.
          if ((r += r2) < r2) { # addieren, r >= 2^32 ?
            q1 += 1; r -= y;
          }
          # jetzt noch 0 <= r < 2^32 <= 2*y
          if (r >= y) {
            q1 += 1; r -= y;
          }
        } # Quotient q1, Rest r fertig.
        # 2^16*r+low16(xlo) durch y dividieren:
        {
          var uint16 r16;
          var uint32 r2;
          if (y1_1==0) {
            q0 = high16(r); r16 = low16(r);
          } else {
            divu_3216_1616(r,y1_1, q0=,r16=);
          }
          # q0 = floor(r/(y1+1)), r16 = r - (y1+1)*q0 (>=0, <=y1)
          # Bilde r := (2^16*r+low16(xlo)) - y*q0
          #          = 2^16*(r-y1*q0) + low16(xlo) - y0*q0
          #          = 2^16*r16 + 2^16*q0 + low16(xlo) - y0*q0 (>=0)
          # Dies ist < 2^16*y1 + 2^32 <= y + 2^32 <= 3*y, kann überlaufen!
          r = highlow32(r16,low16(xlo)); # 2^16*r16 + low16(xlo) < 2^32
          r2 = highlow32_0(q0) - mulu16(low16(y),q0); # 2^16*q0 - y0*q0 < 2^32
          # 0 <= r+r2 < 3*y. Bei der Addition auf Carry testen!
          # Carry -> jedenfalls y <= r+r2 < y + 2^32 <= 3*y.
          # kein Carry -> jedenfalls 0 <= r+r2 < 2^32 <= 2*y.
          if ((r += r2) < r2) { # addieren, r >= 2^32 ?
            q0 += 1; r -= y;
          }
          # jetzt noch 0 <= r < 2^32 <= 2*y
          if (r >= y) {
            q0 += 1; r -= y;
          }
        } # Quotient q0, Rest r fertig.
        divu_32_rest = r >> s; # Rest
        return highlow32(q1,q0); # Quotient
      }
      #endif
    #endif
  #endif

#if (intVsize>32)

# Dividiert eine 64-Bit-Zahl durch eine 32-Bit-Zahl und
# liefert einen 64-Bit-Quotienten und einen 32-Bit-Rest.
# divu_6432_6432(x,y,q=,r=);
# > uint64 x: Zähler
# > uint32 y: Nenner
# Es sei bekannt, dass y>0.
# < uint64 q: floor(x/y)
# < uint32 r: x mod y
# < x = q*y+r
  extern_C uint64 divu_6432_6432_ (uint64 x, uint32 y); # -> Quotient q
  extern uint32 divu_32_rest;                           # -> Rest r
  #if 1
    # Methode: (beta = 2^32)
    # x = x1*beta+x0 schreiben.
    # Division mit Rest: x1 = q1*y + r1, wobei 0 <= x1 < beta <= beta*y.
    # Also 0 <= q1 < beta, 0 <= r1 < y.
    # Division mit Rest: (r1*beta+x0) = q0*y + r0, wobei 0 <= r1*beta+x0 < beta*y.
    # Also 0 <= q0 < beta, 0 <= r0 < y
    # und x = x1*beta+x0 = (q1*beta+q0)*y + r0.
    # Setze q := q1*beta+q0 und r := r0.
    #ifdef GNU
      #define divu_6432_6432(x,y,q_zuweisung,r_zuweisung)  \
        ({var uint64 _x = (x);            \
          var uint32 _y = (y);            \
          var uint32 _q1;                 \
          var uint32 _q0;                 \
          var uint32 _r1;                 \
          divu_6432_3232(0,high32(_x),_y, _q1 = , _r1 = ); \
          divu_6432_3232(_r1,low32(_x),_y, _q0 = , _EMA_ r_zuweisung); \
          q_zuweisung highlow64(_q1,_q0); \
         })
    #else
      #define divu_6432_6432(x,y,q_zuweisung,r_zuweisung)  \
        {var uint64 _x = (x);            \
         var uint32 _y = (y);            \
         var uint32 _q1;                 \
         var uint32 _q0;                 \
         var uint32 _r1;                 \
         divu_6432_3232(0,high32(_x),_y, _q1 = , _r1 = ); \
         divu_6432_3232(_r1,low32(_x),_y, _q0 = , _EMA_ r_zuweisung); \
         q_zuweisung highlow64(_q1,_q0); \
        }
    #endif
  #else
    #define divu_6432_6432(x,y,q_zuweisung,r_zuweisung)  \
      { q_zuweisung divu_6432_6432_(x,y); r_zuweisung divu_32_rest; }
    #if 0
      # divu_6432_6432_ extern in Assembler
    #else
      #ifdef LISPARIT
      global uint64 divu_6432_6432_ (uint64 x, uint32 y) {
        var uint32 q1;
        var uint32 q0;
        var uint32 r1;
        divu_6432_3232(0,high32(x),y, q1 = , r1 = );
        divu_6432_3232(r1,low32(x),y, q0 = , divu_32_rest =);
        return highlow64(q1,q0);
      }
      #endif
    #endif
  #endif

# Dividiert eine 64-Bit-Zahl durch eine 64-Bit-Zahl und
# liefert einen 64-Bit-Quotienten und einen 64-Bit-Rest.
# divu_6464_6464(x,y,q=,r=);
# > uint64 x: Zähler
# > uint64 y: Nenner
# Es sei bekannt, dass y>0.
# < uint64 q: floor(x/y)
# < uint64 r: x mod y
# < x = q*y+r
  extern_C uint64 divu_6464_6464_ (uint64 x, uint64 y); # -> Quotient q
  extern uint64 divu_64_rest;                           # -> Rest r
  #if 1
    # Methode: (beta = 2^n = 2^32, n = 32)
    # Falls y < beta, handelt es sich um eine 64-durch-32-Bit-Division.
    # Falls y >= beta:
    # Quotient  q = floor(x/y) < beta  (da 0 <= x < beta^2, y >= beta).
    # y habe genau n+k Bits (1 <= k <= n), d.h. 2^(n+k-1) <= y < 2^(n+k).
    # Schreibe  x = 2^k*x1 + x0  mit  x1 := floor(x/2^k)
    # und       y = 2^k*y1 + y0  mit  y1 := floor(y/2^k)
    # und bilde den Näherungs-Quotienten floor(x1/y1)
    # oder (noch besser) floor(x1/(y1+1)).
    # Wegen 0 <= x1 < 2^(2n) und 0 < 2^(n-1) <= y1 < 2^n
    # und  x1/(y1+1) <= x/y < x1/(y1+1) + 2
    # (denn x1/(y1+1) = (x1*2^k)/((y1+1)*2^k) <= (x1*2^k)/y <= x/y
    # und x/y - x1/(y1+1) = (x+x*y1-x1*y)/(y*(y1+1))
    # = (x+x0*y1-x1*y0)/(y*(y1+1)) <= (x+x0*y1)/(y*(y1+1))
    # <= x/(y*(y1+1)) + x0/y
    # <= 2^(2n)/(2^(n+k-1)*(2^(n-1)+1)) + 2^k/2^(n+k-1)
    # = 2^(n-k+1)/(2^(n-1)+1) + 2^(1-n) <= 2^n/(2^(n-1)+1) + 2^(1-n) < 2 )
    # gilt  floor(x1/(y1+1)) <= floor(x/y) <= floor(x1/(y1+1)) + 2  .
    # Man bildet also  q:=floor(x1/(y1+1))  (ein Shift um n Bit oder
    # eine (2n)-durch-n-Bit-Division, mit Ergebnis q <= floor(x/y) < beta)
    # und x-q*y und muss hiervon noch höchstens 2 mal y abziehen und q
    # incrementieren, um den Quotienten  q = floor(x/y)  und den Rest
    # x-floor(x/y)*y  der Division zu bekommen.
    #define divu_6464_6464(x,y,q_zuweisung,r_zuweisung)  \
      { var uint64 _x = (x);                                                    \
        var uint64 _y = (y);                                                    \
        if (_y <= (uint64)(((uint64)1<<32)-1))                                  \
          { var uint32 _q1;                                                     \
            var uint32 _q0;                                                     \
            var uint32 _r1;                                                     \
            divu_6432_3232(0,high32(_x),_y, _q1 = , _r1 = );                    \
            divu_6432_3232(_r1,low32(_x),_y, _q0 = , _EMA_ r_zuweisung);        \
            q_zuweisung highlow64(_q1,_q0);                                     \
          }                                                                     \
          else                                                                  \
          { var uint64 _x1 = _x; # x1 := x                                      \
            var uint64 _y1 = _y; # y1 := y                                      \
            var uint32 _q;                                                      \
            do { _x1 = floor(_x1,2); _y1 = floor(_y1,2); } # k erhöhen          \
               until (_y1 <= (uint64)(((uint64)1<<32)-1)); # bis y1 < beta      \
            { var uint32 _y2 = low32(_y1)+1; # y1+1 bilden                      \
              if (_y2==0)                                                       \
                { _q = high32(_x1); } # y1+1=beta -> ein Shift                  \
                else                                                            \
                { divu_6432_3232(high32(_x1),low32(_x1),_y2,_q=,); } # Division von x1 durch y1+1 \
            }                                                                   \
            # _q = q = floor(x1/(y1+1))                                         \
            # x-q*y bilden (eine 32-mal-64-Bit-Multiplikation ohne Überlauf):   \
            _x -= highlow64_0(mulu32_64(_q,high32(_y))); # q * high32(y) * beta \
            # gefahrlos, da q*high32(y) <= q*y/beta <= x/beta < beta            \
            _x -= mulu32_64(_q,low32(_y)); # q * low32(y)                       \
            # gefahrlos, da q*high32(y)*beta + q*low32(y) = q*y <= x            \
            # Noch höchstens 2 mal y abziehen:                                  \
            if (_x >= _y)                                                       \
              { _q += 1; _x -= _y;                                              \
                if (_x >= _y)                                                   \
                  { _q += 1; _x -= _y;                                          \
              }   }                                                             \
            r_zuweisung _x;                                                     \
            q_zuweisung (uint64)(_q);                                           \
      }   }
  #else
    #define divu_6464_6464(x,y,q_zuweisung,r_zuweisung)  \
      { q_zuweisung divu_6464_6464_(x,y); r_zuweisung divu_64_rest; }
    #if 0
      # divu_6464_6464_ extern in Assembler
    #else
      #ifdef LISPARIT
      global uint64 divu_6464_6464_ (uint64 x, uint64 y) {
        var uint64 q = floor(x,y);
        divu_64_rest = x - q*y;
        return q;
      }
      #endif
    #endif
  #endif

#endif

# Zieht die Ganzzahl-Wurzel aus einer 32-Bit-Zahl und
# liefert eine 16-Bit-Wurzel und einen Rest.
# isqrt_32_16(x,y=,sqrtp=);
# > uint32 x: Radikand, >= 2^30, < 2^32
# < uint16 y: floor(sqrt(x)), >= 2^15, < 2^16
# < bool sqrtp: /=0, falls x=y^2
  # Methode:
  # y := 2^16 als Anfangswert,
  # y := floor((y + floor(x/y))/2) als nächster Wert,
  # solange z := floor(x/y) < y, setze y := floor((y+z)/2).
  # y ist fertig; x=y^2 genau dann, wenn z=y und die letzte Division aufging.
  # (Beweis:
  #  1. Die Folge der y ist streng monoton fallend.
  #  2. Stets gilt y >= floor(sqrt(x)) (denn für alle y>0 ist
  #     y + x/y >= 2*sqrt(x) und daher  floor((y + floor(x/y))/2) =
  #     floor(y/2 + x/(2*y)) >= floor(sqrt(x)) ).
  #  3. Am Schluss gilt x >= y^2.
  # )
  #define isqrt_32_16(x,y_zuweisung,sqrtp_zuweisung)  \
    { var uint32 _x = (x);                                               \
      var uint16 _x1 = high16(_x);                                       \
      var uint16 _y = floor(_x1,2) | bit(16-1);                          \
      loop                                                               \
        { var uint16 _z;                                                 \
          var uint16 _r;                                                 \
          if (_x1 >= _y) # Division _x/_y ergäbe Überlauf -> _z > _y     \
            { unused (sqrtp_zuweisung false); break; }                   \
          divu_3216_1616(_x,_y, _z=,_r=); # Dividiere _x/_y              \
          if (_z >= _y)                                                  \
            { unused (sqrtp_zuweisung (_z == _y) && (_r == 0)); break; } \
          _y = floor((uint16)(_z+_y),2) | bit(16-1); # _y muss >= 2^15 bleiben \
        }                                                                \
      y_zuweisung _y;                                                    \
    }

# Zieht die Ganzzahl-Wurzel aus einer 64-Bit-Zahl und
# liefert eine 32-Bit-Wurzel und einen Rest.
# isqrt_64_32(xhi,xlo,y=,sqrtp=);
# > uint32 xhi,xlo: Radikand x = 2^32*xhi+xlo, >= 2^62, < 2^64
# < uint32 y: floor(sqrt(x)), >= 2^31, < 2^32
# < bool sqrtp: /=0, falls x=y^2
  #if (defined(SPARC) || defined(SPARC64) || defined(MC680Y0) || defined(HPPA))
    # Methode:
    # y := 2^32 als Anfangswert,
    # y := floor((y + floor(x/y))/2) als nächster Wert,
    # solange z := floor(x/y) < y, setze y := floor((y+z)/2).
    # y ist fertig; x=y^2 genau dann, wenn z=y und die letzte Division aufging.
    # (Beweis:
    #  1. Die Folge der y ist streng monoton fallend.
    #  2. Stets gilt y >= floor(sqrt(x)) (denn für alle y>0 ist
    #     y + x/y >= 2*sqrt(x) und daher  floor((y + floor(x/y))/2) =
    #     floor(y/2 + x/(2*y)) >= floor(sqrt(x)) ).
    #  3. Am Schluss gilt x >= y^2.
    # )
    #define isqrt_64_32(xhi,xlo,y_zuweisung,sqrtp_zuweisung)  \
      { var uint32 _xhi = (xhi);                                        \
        var uint32 _xlo = (xlo);                                        \
        var uint32 _y = floor(_xhi,2) | bit(32-1);                      \
        loop                                                            \
          { var uint32 _z;                                              \
            var uint32 _rest;                                           \
            if (_xhi >= _y) # Division _x/_y ergäbe Überlauf -> _z > _y \
              { sqrtp_zuweisung false; break; }                         \
            divu_6432_3232(_xhi,_xlo,_y, _z=,_rest=); # Dividiere _x/_y \
            if (_z >= _y)                                               \
              { sqrtp_zuweisung (_z == _y) && (_rest == 0); break; }    \
            _y = floor(_z+_y,2) | bit(32-1); # _y muss >= 2^31 bleiben  \
          }                                                             \
        y_zuweisung _y;                                                 \
      }
  #else
    # Methode:
    # Wie bei UDS_sqrt mit n=2.
    # y = 2^16*yhi + ylo ansetzen.
    # Dann muss
    #   yhi = floor(y/2^16) = floor(floor(sqrt(x))/2^16)
    #       = floor(sqrt(x)/2^16) = floor(sqrt(x/2^32)) = isqrt(xhi)
    # sein. Es folgt yhi >= 2^15.
    # Danach sucht man das größte ylo >=0 mit
    # x - 2^32*yhi^2 >= 2*2^16*yhi*ylo + ylo^2.
    # Dazu setzen wir  xhi*2^32+xlo := x - 2^32*yhi^2
    # (also xhi := xhi - yhi^2, das ist >=0, <=2*yhi).
    # Die Schätzung für die zweite Ziffer
    #     ylo' := min(2^16-1,floor((xhi*2^32+xlo)/(2*2^16*yhi)))
    # erfüllt ylo'-1 <= ylo <= ylo', ist also um höchstens 1 zu groß.
    # (Beweis: Rechte Ungleichung klar, da  ylo < 2^16  und
    #   xhi*2^32+xlo >= 2*2^16*yhi*ylo + ylo^2 >= 2*2^16*yhi*ylo
    #   ==> (xhi*2^32+xlo)/(2*2^16*yhi) >= ylo  gelten muss.
    #   Linke Ungleichung: Falls floor(...)>=2^16, ist
    #   xhi*2^32+xlo >= 2*2^16*2^16*yhi >= 2*2^16*yhi*(2^16-1) + 2^32
    #                >= 2*2^16*yhi*(2^16-1) + (2^16-1)^2
    #   und xhi*2^32+xlo < 2*2^16*2^16*yhi + (2^16)^2, also
    #   ylo = 2^16-1 = ylo'.
    #   Sonst ist ylo' = floor((xhi*2^32+xlo)/(2*2^16*yhi)), also
    #   xhi*2^32+xlo >= 2*2^16*yhi*ylo' >= 2*2^16*yhi*(ylo'-1) + 2^32
    #                >= 2*2^16*yhi*(ylo'-1) + (ylo'-1)^2,
    #   also ylo >= ylo'-1 nach Definition von ylo.)
    #define isqrt_64_32(xhi,xlo,y_zuweisung,sqrtp_zuweisung)  \
      { var uint32 _xhi = (xhi);                                            \
        var uint32 _xlo = (xlo);                                            \
        var uint16 _yhi;                                                    \
        var uint16 _ylo;                                                    \
        # erste Ziffer berechnen:                                           \
        isqrt_32_16(_xhi,_yhi=,); # yhi := isqrt(xhi)                       \
        _xhi -= mulu16(_yhi,_yhi); # jetzt 0 <= xhi <= 2*yhi                \
        # x = 2^32*yhi^2 + 2^32*xhi + xlo                                   \
        # Schätzung für die zweite Ziffer berechnen:                        \
        # ylo := min(2^16-1,floor((xhi*2^32+xlo)/(2*2^16*yhi))) bilden:     \
       {var uint32 _z = (_xhi << 15) | (_xlo >> 17); # < 2^15*(2*yhi+1)     \
        var uint32 _r = highlow32_0(_yhi);                                  \
        if (_z >= _r)                                                       \
          { _ylo = bit(16)-1; _r = _z - _r + (uint32)_yhi; }                \
          else                                                              \
          { divu_3216_1616(_z,_yhi, _ylo=,_r=); }                           \
        # x = 2^32*yhi^2 + 2*2^16*yhi*ylo + 2^17*r + (xlo mod 2^17),        \
        # 0 <= r < yhi + 2^15                                               \
        _xlo = (_r << 17) | (_xlo & (bit(17)-1));                           \
        # x = 2^32*yhi^2 + 2*2^16*yhi*ylo + 2^32*floor(r/2^15) + xlo        \
        _z = mulu16(_ylo,_ylo); # z = ylo^2                                 \
        # Versuche vom Rest 2^32*floor(r/2^15) + xlo  z zu subtrahieren.    \
        # Falls Rest >= z (d.h. r>=2^15 oder xlo>=z), ist ylo fertig,       \
        # und es gilt x=y^2 genau dann, wenn r<2^15 und xlo=z.              \
        # Sonst (d.h. r<2^15 und xlo<z), muss man ylo erniedrigen. Dazu     \
        # setzt man  ylo := ylo-1, z := z-(2*ylo+1),                        \
        # Rest := Rest + 2^17*yhi = xlo + 2^17*yhi >= 2^32 > z, also x>y^2. \
        if (_r < bit(15))                                                   \
          { if (_xlo < _z)                                                  \
              { _ylo -= 1; sqrtp_zuweisung false; }                         \
              else                                                          \
              { sqrtp_zuweisung (_xlo == _z); }                             \
          }                                                                 \
          else                                                              \
          { sqrtp_zuweisung false; }                                        \
        y_zuweisung highlow32(_yhi,_ylo);                                   \
      }}
  #endif

# Eine 32-Bit-Zahl aus zwei aufeinanderfolgenden 16-Bit-Digits einer UDS
# zusammensetzen: highlow32_at(ptr)
  #if BIG_ENDIAN_P && defined(MC680X0)
    # ptr als 32-Bit-Pointer auffassen und darauf zugreifen
    #define highlow32_at(ptr)  (*(uint32*)(ptr))
  #else
    #define highlow32_at(ptr)  highlow32(((uint16*)(ptr))[0],((uint16*)(ptr))[1])
  #endif

# Eine 32-Bit-Zahl in zwei aufeinanderfolgende 16-Bit-Digits einer UDS abspeichern:
# set_highlow32_at(ptr,value32); wobei ptr und value32 Variablen.
  #if BIG_ENDIAN_P && defined(MC680X0)
    # ptr als 32-Bit-Pointer auffassen und darauf zugreifen
    #define set_highlow32_at(ptr,value32)  (*(uint32*)(ptr)=(value32))
  #else
    #define set_highlow32_at(ptr,value32)  (((uint16*)(ptr))[0]=high16(value32),((uint16*)(ptr))[1]=low16(value32))
  #endif

