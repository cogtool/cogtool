# Hilfsfunktion zur Ausgabe von Integers

# Tabelle: enthält zu jeder Basis b (2 <= b <= 36)
# - eine Kettenbruchapproximation num/den von intDsize*log(2)/log(b)
#   (num/den >= intDsize*log(2)/log(b), mit num <= 2^10)
# - k-1 und b^k mit b^k < 2^intDsize, k maximal.
  typedef struct { /* uintW num,den; */ uintC k_1; uintD b_hoch_k; } power_table_entry;
  local const power_table_entry table [36-2+1] = {
    #if (intDsize==8)
      { /*    8,  1, */ 7-1, 2*2*2*2*2*2*2},
      { /*  106, 21, */ 5-1, 3*3*3*3*3},
      { /*    4,  1, */ 3-1, 4*4*4},
      { /*  789,229, */ 3-1, 5*5*5},
      { /*  359,116, */ 3-1, 6*6*6},
      { /*  436,153, */ 2-1, 7*7},
      { /* 1019,382, */ 2-1, 8*8},
      { /*   53, 21, */ 2-1, 9*9},
      { /*  525,218, */ 2-1, 10*10},
      { /* 1006,435, */ 2-1, 11*11},
      { /*  665,298, */ 2-1, 12*12},
      { /*  988,457, */ 2-1, 13*13},
      { /*  872,415, */ 2-1, 14*14},
      { /*  987,482, */ 2-1, 15*15},
      { /*    2,  1, */ 1-1, 16},
      { /*  869,444, */ 1-1, 17},
      { /*  871,454, */ 1-1, 18},
      { /*  597,317, */ 1-1, 19},
      { /*   87, 47, */ 1-1, 20},
      { /*  989,543, */ 1-1, 21},
      { /*  949,529, */ 1-1, 22},
      { /*  191,108, */ 1-1, 23},
      { /*  930,533, */ 1-1, 24},
      { /*  789,458, */ 1-1, 25},
      { /*  691,406, */ 1-1, 26},
      { /*  461,274, */ 1-1, 27},
      { /*  218,131, */ 1-1, 28},
      { /*  690,419, */ 1-1, 29},
      { /*  494,303, */ 1-1, 30},
      { /*  633,392, */ 1-1, 31},
      { /*    8,  5, */ 1-1, 32},
      { /*  766,483, */ 1-1, 33},
      { /*  629,400, */ 1-1, 34},
      { /*  967,620, */ 1-1, 35},
      { /*  359,232, */ 1-1, 36},
    #endif
    #if (intDsize==16)
      { /*   16,  1, */ 15-1, 2*2*2*2*2*2*2*2*2*2*2*2*2*2*2},
      { /*  212, 21, */ 10-1, 3*3*3*3*3*3*3*3*3*3},
      { /*    8,  1, */  7-1, 4*4*4*4*4*4*4},
      { /*  379, 55, */  6-1, 5*5*5*5*5*5},
      { /*  359, 58, */  6-1, 6*6*6*6*6*6},
      { /*  872,153, */  5-1, 7*7*7*7*7},
      { /* 1019,191, */  5-1, 8*8*8*8*8},
      { /*  106, 21, */  5-1, 9*9*9*9*9},
      { /*  525,109, */  4-1, 10*10*10*10},
      { /* 1013,219, */  4-1, 11*11*11*11},
      { /*  665,149, */  4-1, 12*12*12*12},
      { /*  761,176, */  4-1, 13*13*13*13},
      { /*  685,163, */  4-1, 14*14*14*14},
      { /*  987,241, */  4-1, 15*15*15*15},
      { /*    4,  1, */  3-1, 16*16*16},
      { /*  869,222, */  3-1, 17*17*17},
      { /*  871,227, */  3-1, 18*18*18},
      { /*  113, 30, */  3-1, 19*19*19},
      { /*  174, 47, */  3-1, 20*20*20},
      { /*   51, 14, */  3-1, 21*21*21},
      { /*  653,182, */  3-1, 22*22*22},
      { /*  191, 54, */  3-1, 23*23*23},
      { /*  677,194, */  3-1, 24*24*24},
      { /*  789,229, */  3-1, 25*25*25},
      { /*  691,203, */  3-1, 26*26*26},
      { /*  461,137, */  3-1, 27*27*27},
      { /*  436,131, */  3-1, 28*28*28},
      { /*  359,109, */  3-1, 29*29*29},
      { /*  988,303, */  3-1, 30*30*30},
      { /*  633,196, */  3-1, 31*31*31},
      { /*   16,  5, */  3-1, 32*32*32},
      { /*  203, 64, */  3-1, 33*33*33},
      { /*  629,200, */  3-1, 34*34*34},
      { /*  967,310, */  3-1, 35*35*35},
      { /*  359,116, */  3-1, 36*36*36},
    #endif
    #if (intDsize==32)
      { /*   32,  1, */ 31-1, 2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL*2UL},
      { /*  424, 21, */ 20-1, 3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL*3UL},
      { /*   16,  1, */ 15-1, 4UL*4UL*4UL*4UL*4UL*4UL*4UL*4UL*4UL*4UL*4UL*4UL*4UL*4UL*4UL},
      { /*  758, 55, */ 13-1, 5UL*5UL*5UL*5UL*5UL*5UL*5UL*5UL*5UL*5UL*5UL*5UL*5UL},
      { /*  359, 29, */ 12-1, 6UL*6UL*6UL*6UL*6UL*6UL*6UL*6UL*6UL*6UL*6UL*6UL},
      { /*   57,  5, */ 11-1, 7UL*7UL*7UL*7UL*7UL*7UL*7UL*7UL*7UL*7UL*7UL},
      { /* 1003, 94, */ 10-1, 8UL*8UL*8UL*8UL*8UL*8UL*8UL*8UL*8UL*8UL},
      { /*  212, 21, */ 10-1, 9UL*9UL*9UL*9UL*9UL*9UL*9UL*9UL*9UL*9UL},
      { /*  289, 30, */  9-1, 10UL*10UL*10UL*10UL*10UL*10UL*10UL*10UL*10UL},
      { /*  990,107, */  9-1, 11UL*11UL*11UL*11UL*11UL*11UL*11UL*11UL*11UL},
      { /*  848, 95, */  8-1, 12UL*12UL*12UL*12UL*12UL*12UL*12UL*12UL},
      { /*  761, 88, */  8-1, 13UL*13UL*13UL*13UL*13UL*13UL*13UL*13UL},
      { /* 1017,121, */  8-1, 14UL*14UL*14UL*14UL*14UL*14UL*14UL*14UL},
      { /*  901,110, */  8-1, 15UL*15UL*15UL*15UL*15UL*15UL*15UL*15UL},
      { /*    8,  1, */  7-1, 16UL*16UL*16UL*16UL*16UL*16UL*16UL},
      { /*  869,111, */  7-1, 17UL*17UL*17UL*17UL*17UL*17UL*17UL},
      { /*  683, 89, */  7-1, 18UL*18UL*18UL*18UL*18UL*18UL*18UL},
      { /*  113, 15, */  7-1, 19UL*19UL*19UL*19UL*19UL*19UL*19UL},
      { /*  348, 47, */  7-1, 20UL*20UL*20UL*20UL*20UL*20UL*20UL},
      { /*   51,  7, */  7-1, 21UL*21UL*21UL*21UL*21UL*21UL*21UL},
      { /*  653, 91, */  7-1, 22UL*22UL*22UL*22UL*22UL*22UL*22UL},
      { /*  191, 27, */  7-1, 23UL*23UL*23UL*23UL*23UL*23UL*23UL},
      { /*  677, 97, */  6-1, 24UL*24UL*24UL*24UL*24UL*24UL},
      { /*  379, 55, */  6-1, 25UL*25UL*25UL*25UL*25UL*25UL},
      { /*  851,125, */  6-1, 26UL*26UL*26UL*26UL*26UL*26UL},
      { /*  922,137, */  6-1, 27UL*27UL*27UL*27UL*27UL*27UL},
      { /*  872,131, */  6-1, 28UL*28UL*28UL*28UL*28UL*28UL},
      { /*  718,109, */  6-1, 29UL*29UL*29UL*29UL*29UL*29UL},
      { /*  150, 23, */  6-1, 30UL*30UL*30UL*30UL*30UL*30UL},
      { /*  633, 98, */  6-1, 31UL*31UL*31UL*31UL*31UL*31UL},
      { /*   32,  5, */  6-1, 32UL*32UL*32UL*32UL*32UL*32UL},
      { /*  203, 32, */  6-1, 33UL*33UL*33UL*33UL*33UL*33UL},
      { /*  629,100, */  6-1, 34UL*34UL*34UL*34UL*34UL*34UL},
      { /*  967,155, */  6-1, 35UL*35UL*35UL*35UL*35UL*35UL},
      { /*  359, 58, */  6-1, 36UL*36UL*36UL*36UL*36UL*36UL},
    #endif
    };

# digits_need(len,base) liefert eine obere Abschätzung für die Anzahl der
# Ziffern im Stellenwertsystem der Basis base, die eine UDS der Länge len
# braucht.
  local uintL digits_need (uintC len, uintWL base)
  {
    # 1+ceiling(len * intDsize*log(2)/log(base)) Bytes oder etwas mehr
    var uintL need = 1+floor(len,1024/intDsize); # > ceiling(len*intDsize/1024) >= 0
    switch (base) { # need mit ceiling(1024*log(2)/log(base)) multiplizieren:
      case 2: need = 1024*need; break;
      case 3: need = 647*need; break;
      case 4: need = 512*need; break;
      case 5: need = 442*need; break;
      case 6: need = 397*need; break;
      case 7: need = 365*need; break;
      case 8: need = 342*need; break;
      case 9: need = 324*need; break;
      case 10: need = 309*need; break;
      case 11: need = 297*need; break;
      case 12: need = 286*need; break;
      case 13: need = 277*need; break;
      case 14: need = 269*need; break;
      case 15: need = 263*need; break;
      case 16: need = 256*need; break;
      case 17: need = 251*need; break;
      case 18: need = 246*need; break;
      case 19: need = 242*need; break;
      case 20: need = 237*need; break;
      case 21: need = 234*need; break;
      case 22: need = 230*need; break;
      case 23: need = 227*need; break;
      case 24: need = 224*need; break;
      case 25: need = 221*need; break;
      case 26: need = 218*need; break;
      case 27: need = 216*need; break;
      case 28: need = 214*need; break;
      case 29: need = 211*need; break;
      case 30: need = 209*need; break;
      case 31: need = 207*need; break;
      case 32: need = 205*need; break;
      case 33: need = 203*need; break;
      case 34: need = 202*need; break;
      case 35: need = 200*need; break;
      case 36: need = 199*need; break;
      default: NOTREACHED;
    }
    # Nun gilt need >= len*intDsize*log(2)/log(base).
    need += 1; # Platzbedarf in Bytes
    return need;
  }

# Wandelt eine UDS in ein Stellensystem um.
# UDS_to_DIGITS(MSDptr,len,base, &ergebnis);
# > MSDptr/len/..: eine UDS
# > base: Stellensystem-Basis, 2 <= base <= 36.
# > ergebnis.LSBptr: darunter ist mindestens digits_need(len) Zeichen Platz
# < ergebnis: fertige Folge MSBptr/len/LSBptr von Ziffern
# Die UDS MSDptr/len/.. wird zerstört.
  typedef struct { chart* MSBptr; uintL len; chart* LSBptr; } DIGITS;
  local void UDS_to_DIGITS (uintD* MSDptr, uintC len, uintD base, DIGITS* erg);
# Methode:
# Umwandlung ins Stellensystem der Basis b geht durch Umwandlung ins Stellen-
# system der Basis b^k (k>=1, b^k<2^intDsize, k maximal) vor sich.
# Aufsuchen von k und b^k aus einer Tabelle.
# Reduktion der UWS zu einer NUWS X.
# Falls X=0: die eine Ziffer 0.
# Falls X>0:
#   Dividiere X durch das Wort b^k,
#   (Single-Precision-Division, vgl. UDS_DIVIDE mit n=1:
#     r:=0, j:=m=Länge(X),
#     while j>0 do
#       j:=j-1, r:=r*beta+X[j], X[j]:=floor(r/b^k), r:=r-b^k*q[j].
#     r=Rest.)
#   zerlege den Rest (mit k-1 Divisionen durch b) in k Ziffern, wandle diese
#   Ziffern einzeln in Ascii um und lege sie an die DIGITS an.
#   Teste auf Speicherüberlauf.
#   X := Quotient.
#   Mache aus X wieder eine NUDS (maximal 1 Nulldigit streichen).
#   Dies solange bis X=0.
#   Streiche die führenden Nullen.
  local void UDS_to_DIGITS (uintD* MSDptr, uintC len, uintD base, DIGITS* erg)
  {
    # Aufsuchen von k-1 und b^k aus der Tabelle:
    var const power_table_entry * tableptr = &table[base-2];
    var uintC k_1 = tableptr->k_1; # k-1
    var uintD b_hoch_k = tableptr->b_hoch_k; # b^k
    var chart* erg_ptr = erg->LSBptr;
    begin_arith_call();
    #define next_digit(d)  { *--erg_ptr = ascii(d<10 ? '0'+d : 'A'-10+d); }
    # normalisiere zu einer NUDS:
    loop {
      if (len==0) { # 0 -> eine Ziffer '0'
        next_digit(0); goto fertig;
      }
      if (MSDptr[0]==0) {
        MSDptr++; len--;
      } else
        break;
    }
    loop {
      # Noch die NUDS MSDptr/len/.. mit len>0 abzuarbeiten.
      # Single-Precision-Division durch b^k:
      var uintD rest = divu_loop_up(b_hoch_k,MSDptr,len);
      # Zerlegen des Restes in seine k Ziffern:
      var uintC count = k_1;
      if ((intDsize>=11) || (count>0))
        # (Bei intDsize>=11 ist wegen b<=36 zwangsläufig
        # k = ceiling(intDsize*log(2)/log(b))-1 >= 2, also count = k_1 > 0.)
        do {
          var uintD d;
          #if HAVE_DD
            divuD((uintDD)rest,base,rest=,d=);
          #else
            divuD(0,rest,base,rest=,d=);
          #endif
          next_digit(d);
        } until (--count == 0);
      next_digit(rest); # letzte der k Ziffern ablegen
      # Quotienten normalisieren (max. 1 Digit streichen):
      if (MSDptr[0]==0) {
        MSDptr++; len--;
        if (len==0)
          break;
      }
    }
    #undef next_digit
    # Streiche führende Nullen:
    while (chareq(*erg_ptr,ascii('0'))) {
      erg_ptr++;
    }
   fertig:
    erg->MSBptr = erg_ptr;
    erg->len = erg->LSBptr - erg_ptr;
    end_arith_call();
  }

