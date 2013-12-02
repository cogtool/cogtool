# Multiplikation ganzer Zahlen

# meldet Überlauf bei der Multiplikation:
  nonreturning_function(local, mal_ueberlauf, (void)) {
    fehler(arithmetic_error,
           GETTEXT("overflow during multiplication of large numbers")
          );
  }

# karatsuba_threshold = Länge, ab der die Karatsuba-Multiplikation bevorzugt
# wird. Der Break-Even-Point bestimmt sich aus Zeitmessungen.
# Als Test dient (progn (time (! 5000)) nil), das viele kleine und einige
# ganz große Multiplikationen durchführt. Miss die Runtime.
# Unter Linux mit einem 80486:      Auf einer Sparc 2:
# threshold  time in 0.01 sec.
#      5        125
#      6        116
#      7        107
#      8        101
#      9         99
#     10         98
#     11         97
#     12         96
#     13         97
#     14         97
#     15         97
#     16         98
#     17         98
#     18         98
#     19         98
#     20         99
#     25        103
#     30        109
#     40        115
#     50        122
#     70        132
#    100        151
#    150        164
#    250        183
#    500        203
#   1000        203
# Das Optimum scheint bei karatsuba_threshold = 12 zu liegen.
# Da das Optimum aber vom Verhältnis
#         Zeit für uintD-Multiplikation / Zeit für uintD-Addition
# abhängt und die gemessenen Zeiten auf eine Unterschreitung des Optimums
# empfindlicher reagieren als auf eine Überschreitung des Optimums,
# sind wir vorsichtig und wählen einen Wert etwas über dem Optimum:
#define karatsuba_threshold  16

# Quadriert eine Unsigned-Digit-sequence.
# UDS_square_UDS(len1,LSDptr1, MSDptr=,len=,LSDptr=);
# quadriert die UDS ../len1/LSDptr1.
# Dabei sollte len1>0 sein.
# Ergebnis ist die UDS MSDptr/len/LSDptr, mit len=2*len1, im Stack.
# Dabei wird num_stack erniedrigt.
  #define UDS_square_UDS(len1,LSDptr1, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    {                                                                                         \
      var uintL len_from_UDSmal = 2*(uintL)(len1);                                            \
      var uintD* LSDptr_from_UDSmal;                                                          \
      if ((intWCsize < 32) && (len_from_UDSmal > (uintL)(bitc(intWCsize)-1))) {               \
        RESTORE_NUM_STACK;                                                                    \
        mal_ueberlauf();                                                                      \
      }                                                                                       \
      unused (len_zuweisung len_from_UDSmal);                                                 \
      num_stack_need(len_from_UDSmal,MSDptr_zuweisung,LSDptr_zuweisung LSDptr_from_UDSmal =); \
      square_2loop_down((LSDptr1),(len1),LSDptr_from_UDSmal);                                 \
    }

# Quadrier-Doppelschleife:
# Quadriert eine UDS und legt das Ergebnis in einer anderen UDS ab.
# square_2loop_down(sourceptr,len,destptr);
# quadriert die UDS  sourceptr[-len..-1]  (len1>0)
# und legt das Ergebnis in der UDS  destptr[-2*len..-1]  ab.
# Unterhalb von destptr werden 2*len Digits Platz benötigt.
  local void square_2loop_down (const uintD* sourceptr, uintC len,
                                uintD* destptr);
  local void square_2bigloop_down (const uintD* sourceptr, uintC len,
                                   uintD* destptr);
  local void square_2loop_down (const uintD* sourceptr, uintC len,
                                uintD* destptr)
  {
    if (len==1) {
      var uintD digit = sourceptr[-1];
      #if HAVE_DD
      var uintDD prod = muluD(digit,digit);
      destptr[-1] = lowD(prod); destptr[-2] = highD(prod);
      #else
      muluD(digit,digit, destptr[-2] =, destptr[-1] =);
      #endif
    } elif (len < karatsuba_threshold) {
      # Multiplikation nach Schulmethode
      # Gemischte Produkte:
      # 2*(  x[n-1..1] * x[0] * b^1
      #    + x[n-1..2] * x[1] * b^3
      #    + ...
      #    + x[n-1..n-1] * x[n-2] * b^(2*n-3))
      {
        var const uintD* sourceptr1 = sourceptr;
        var uintD* destptr2 = destptr;
        *--destptr2 = 0;
        var uintC count = len-1;
        {
          var uintD digit = *--sourceptr1;
          mulu_loop_down(digit,sourceptr1,destptr2,count);
        }
        var uintD* destptr1 = destptr - (len+1);
        while (--count > 0) {
          destptr2 -= 2;
          var uintD digit = *--sourceptr1;
          var uintD carry = muluadd_loop_down(digit,sourceptr1,destptr2,count);
          *--destptr1 = carry;
        }
        var uintD carry = shift1left_loop_down(destptr-1,2*len-2);
        destptr1[-1] = (carry==0 ? 0 : 1);
      }
      # Quadrate:
      len = 2*len;
      do {
        len -= 2;
        var uintD digit = *--sourceptr;
        #if HAVE_DD
        var uintDD prod = muluD(digit,digit);
        var uintDD accu = highlowDD(destptr[-2],destptr[-1]);
        accu += prod;
        destptr[-1] = lowD(accu); destptr[-2] = highD(accu);
        destptr -= 2;
        if (accu < prod)
          inc_loop_down(destptr,len);
        #else
        var uintD hi;
        var uintD lo;
        var uintD tmp;
        muluD(digit,digit, hi=,lo=);
        tmp = destptr[-1] + lo; destptr[-1] = tmp;
        if (tmp < lo)
          hi++;
        tmp = destptr[-2] + hi; destptr[-2] = tmp;
        destptr -= 2;
        if (tmp < hi)
          inc_loop_down(destptr,len);
        #endif
      } while (len > 0);
    } else { # len groß
      # Karatsuba-Quadrierung
      # (ausgelagert, um die eigentliche Quadrierfunktion nicht
      # durch zu viele Registervariablen zu belasten):
      square_2bigloop_down(sourceptr,len,destptr);
    }
  }
  local void square_2bigloop_down (const uintD* sourceptr, uintC len,
                                   uintD* destptr)
  # Karatsuba-Quadrierung
  {
    # Es ist 2 <= len.
    SAVE_NUM_STACK
    var uintC prod_len = 2*len;
    var uintD* prod_LSDptr = destptr;
    var uintC k_hi = floor(len,2); # Länge der High-Teile: floor(len/2) >0
    var uintC k_lo = len - k_hi; # Länge der Low-Teile: ceiling(len/2) >0
    # Es gilt k_hi <= k_lo <= len, k_lo + k_hi = len.
    # Summe x1+x0 berechnen:
    var uintD* sum_MSDptr;
    var uintC sum_len = k_lo; # = max(k_lo,k_hi)
    var uintD* sum_LSDptr;
    num_stack_need(sum_len+1,sum_MSDptr=,sum_LSDptr=);
    sum_MSDptr++; # 1 Digit vorne als Reserve
    {
      var uintD carry = # Hauptteile von x1 und x0 addieren:
        add_loop_down(sourceptr-k_lo,sourceptr,sum_LSDptr,k_hi);
      if (!(k_lo==k_hi)) {
        # noch k_lo-k_hi = 1 Digits abzulegen
        sum_MSDptr[0] = sourceptr[-(uintP)k_lo]; # = sourceptr[-(uintP)k_hi-1]
        if (!(carry==0)) {
          if (++(sum_MSDptr[0]) == 0)
            carry=1;
          else
            carry=0;
        }
      }
      if (carry) {
        *--sum_MSDptr = 1;
        sum_len++;
      }
    }
    # Platz für Produkte x0*x0, x1*x1:
    {
      var uintC prodhi_len = 2*k_hi;
      var uintD* prodhi_LSDptr = prod_LSDptr - 2*k_lo;
      # prod_MSDptr/2*len/prod_LSDptr wird zuerst die beiden
      # Produkte x1*x1 in prod_MSDptr/2*k_hi/prodhi_LSDptr
      #      und x0*x0 in prodhi_LSDptr/2*k_lo/prod_LSDptr,
      # dann das Produkt (b^k*x1+x0)*(b^k*x1+x0) enthalten.
      # Platz fürs Produkt (x1+x0)*(x1+x0) belegen:
      var uintD* prodmid_MSDptr;
      var uintC prodmid_len = 2*sum_len;
      var uintD* prodmid_LSDptr;
      num_stack_need(prodmid_len,prodmid_MSDptr=,prodmid_LSDptr=);
      # Produkt (x1+x0)*(x1+x0) berechnen:
      square_2loop_down(sum_LSDptr,sum_len,prodmid_LSDptr);
      # Das Produkt beansprucht  2*k_lo + (0 oder 1) <= 2*sum_len = prodmid_len  Digits.
      # Produkt x0*x0 berechnen:
      square_2loop_down(sourceptr,k_lo,prod_LSDptr);
      # Produkt x1*x1 berechnen:
      square_2loop_down(sourceptr-k_lo,k_hi,prodhi_LSDptr);
      # Und x1*x1 abziehen:
      {
        var uintD carry =
          subfrom_loop_down(prodhi_LSDptr,prodmid_LSDptr,prodhi_len);
        # Carry um maximal prodmid_len-prodhi_len Digits weitertragen:
        if (!(carry==0))
          dec_loop_down(prodmid_LSDptr-prodhi_len,prodmid_len-prodhi_len);
      }
      # Und x0*x0 abziehen:
      {
        var uintD carry =
          subfrom_loop_down(prod_LSDptr,prodmid_LSDptr,2*k_lo);
        # Falls Carry: Produkt beansprucht 2*k_lo+1 Digits.
        # Carry um maximal 1 Digit weitertragen:
        if (!(carry==0))
          prodmid_LSDptr[-2*(uintP)k_lo-1] -= 1;
      }
      # prodmid_LSDptr[-prodmid_len..-1] enthält nun 2*x0*x1.
      # Dies ist < 2 * b^k_lo * b^k_hi = 2 * b^len,
      # passt also in len+1 Digits.
      # prodmid_len, wenn möglich, um maximal 2 verkleinern:
      # (benutzt prodmid_len >= 2*k_lo >= len >= 2)
      if (prodmid_MSDptr[0]==0) {
        prodmid_len--;
        if (prodmid_MSDptr[1]==0)
          prodmid_len--;
      }
      # Nun ist k_lo+prodmid_len <= 2*len .
      # (Denn es war prodmid_len = 2*sum_len <= 2*(k_lo+1)
      #  <= len+3, und nach 2-maliger Verkleinerung jedenfalls
      #  prodmid_len <= len+1. Wegen k_lo < len also
      #  k_lo + prodmid_len <= (len-1)+(len+1) = 2*len.)
      # prodmid*b^k = 2*x0*x1*b^k zu prod = x1*x1*b^(2*k) + x0*x0 addieren:
      {
        var uintD carry =
          addto_loop_down(prodmid_LSDptr,prod_LSDptr-k_lo,prodmid_len);
        if (!(carry==0))
          inc_loop_down(prod_LSDptr-(k_lo+prodmid_len),prod_len-(k_lo+prodmid_len));
      }
    }
    RESTORE_NUM_STACK
  }

# (* x x), wo x ein Integer ist. Ergebnis Integer.
# can trigger GC
  # Methode:
  # x=0 -> Ergebnis 0
  # x Fixnum -> direkt quadrieren
  # sonst: zu WS machen, quadrieren.
  local maygc object I_square_I (object x)
  {
    if (eq(x,Fixnum_0))
      return Fixnum_0;
    if (I_fixnump(x)) {
      var sintV x_ = FN_to_V(x);
      #if (oint_data_len+1 > 32)
      # nur falls x ein Integer mit höchstens 32 Bit ist:
      if ((uintV)((sintV)FN_sign(x) ^ x_) < vbit(31))
      #endif
        {
          # Wert direkt quadrieren:
          var uint32 hi;
          var uint32 lo;
          mulu32((uint32)x_,(uint32)x_,hi=,lo=); # erst unsigned multiplizieren
          if (x_ < 0)
            hi -= 2*(uint32)x_; # dann Korrektur für Vorzeichen
          return L2_to_I(hi,lo);
        }
    }
    var uintD* xMSDptr;
    var uintC xlen;
    var uintD* xLSDptr;
    var uintD* ergMSDptr;
    var uintC erglen;
    var uintD* ergLSDptr;
    I_to_NDS_nocopy(x, xMSDptr = , xlen = , xLSDptr = );
    erglen = 2*xlen;
    if ((intWCsize < 32) && (erglen > (uintL)(bitc(intWCsize)-1)))
      mal_ueberlauf();
    {
      SAVE_NUM_STACK # num_stack retten
      num_stack_need(erglen, ergMSDptr = , ergLSDptr = );
      begin_arith_call();
      {
        var uintC len = xlen;
        var uintD MSD = xMSDptr[0];
        if (MSD == 0) {
          ergMSDptr[0] = 0; ergMSDptr[1] = 0; len--;
        }
        square_2loop_down(xLSDptr,len,ergLSDptr);
        if ((sintD)MSD < 0) {
          subfrom_loop_down(xLSDptr,ergLSDptr-xlen,xlen);
          subfrom_loop_down(xLSDptr,ergLSDptr-xlen,xlen);
        }
      }
      end_arith_call();
      var object result = DS_to_I(ergMSDptr,erglen);
      RESTORE_NUM_STACK # num_stack zurück
      return result;
    }
  }

# Multipliziert zwei Unsigned-Digit-sequences.
# UDS_UDS_mal_UDS(len1,LSDptr1, len2,LSDptr2, MSDptr=,len=,LSDptr=);
# multipliziert die UDS ../len1/LSDptr1 und ../len2/LSDptr2.
# Dabei sollte len1>0 und len2>0 sein.
# Ergebnis ist die UDS MSDptr/len/LSDptr, mit len=len1+len2, im Stack.
# Dabei wird num_stack erniedrigt.
  #define UDS_UDS_mal_UDS(len1,LSDptr1,len2,LSDptr2, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    {                                                                                         \
      var uintL len_from_UDSmal = (uintL)(len1) + (uintL)(len2);                              \
      var uintD* LSDptr_from_UDSmal;                                                          \
      if ((intWCsize < 32) && (len_from_UDSmal > (uintL)(bitc(intWCsize)-1))) {               \
        RESTORE_NUM_STACK;                                                                    \
        mal_ueberlauf();                                                                      \
      }                                                                                       \
      unused (len_zuweisung len_from_UDSmal);                                                 \
      num_stack_need(len_from_UDSmal,MSDptr_zuweisung,LSDptr_zuweisung LSDptr_from_UDSmal =); \
      mulu_2loop_down((LSDptr1),(len1),(LSDptr2),(len2),LSDptr_from_UDSmal);                  \
    }

# Multiplikations-Doppelschleife:
# Multipliziert zwei UDS und legt das Ergebnis in einer dritten UDS ab.
# mulu_2loop_down(sourceptr1,len1,sourceptr2,len2,destptr);
# multipliziert die UDS  sourceptr1[-len1..-1]  (len1>0)
#           mit der UDS  sourceptr2[-len1..-1]  (len2>0)
# und legt das Ergebnis in der UDS  destptr[-len..-1]  (len=len1+len2) ab.
# Unterhalb von destptr werden len Digits Platz benötigt.
  local void mulu_2loop_down (const uintD* sourceptr1, uintC len1,
                              const uintD* sourceptr2, uintC len2,
                              uintD* destptr);
  local void mulu_2bigloop_down (const uintD* sourceptr1, uintC len1,
                                 const uintD* sourceptr2, uintC len2,
                                 uintD* destptr);
  local void mulu_2loop_down (const uintD* sourceptr1, uintC len1,
                              const uintD* sourceptr2, uintC len2,
                              uintD* destptr)
  {
    # len1<=len2 erzwingen:
    if (len1>len2) {
      {
        var const uintD* temp;
        temp = sourceptr1; sourceptr1 = sourceptr2; sourceptr2 = temp;
      }
      {
        var uintC temp;
        temp = len1; len1 = len2; len2 = temp;
      }
    }
    if (len1==1) {
      # nur eine Einfachschleife
      mulu_loop_down(*--sourceptr1,sourceptr2,destptr,len2);
    } elif (len1 < karatsuba_threshold) {
      # Multiplikation nach Schulmethode
      # len2 Digits auf 0 setzen:
      var uintD* destptr2 = clear_loop_down(destptr,len2);
      # äußere Schleife läuft über source1 :
      dotimespC(len1,len1, {
        # innere Schleife läuft über source2 :
        var uintD carry =
          muluadd_loop_down(*--sourceptr1,sourceptr2,destptr,len2);
        *--destptr2 = carry; # UDS um das Carry-Digit verlängern
        destptr--;
      });
    } else { # len1 groß
      # Karatsuba-Multiplikation
      # (ausgelagert, um die eigentliche Multiplikationsfunktion nicht
      # durch zu viele Registervariablen zu belasten):
      mulu_2bigloop_down(sourceptr1,len1,sourceptr2,len2,destptr);
    }
  }
  local void mulu_2bigloop_down (const uintD* sourceptr1, uintC len1,
                                 const uintD* sourceptr2, uintC len2,
                                 uintD* destptr)
  {
    # Karatsuba-Multiplikation
    # Prinzip: (x1*b^k+x0) * (y1*b^k+y0)
    #        = x1*y1 * b^2k + ((x1+x0)*(y1+y0)-x1*y1-x0*y0) * b^k + x0*y0
    # Methode 1 (Collins/Loos, Degel):
    # source2 wird in floor(len2/len1) einzelne UDS mit je einer
    # Länge len3 (len1 <= len3 < 2*len1) unterteilt,
    # jeweils k=floor(len3/2).
    # Methode 2 (Haible):
    # source2 wird in ceiling(len2/len1) einzelne UDS mit je einer
    # Länge len3 (0 < len3 <= len1) unterteilt, jeweils k=floor(len1/2).
    # Aufwand für die hinteren Einzelteile:
    # bei beiden Methoden jeweils 3*len1^2.
    # Aufwand für das vorderste Teil (alles, falls len1 <= len2 < 2*len1)
    # mit r = len1, s = (len2 mod len1) + len1 (>= len1, < 2*len1):
    # bei Methode 1:
    #                       |   :       |  r
    #                    |      :       |  s
    #      (r-s/2)*s/2 + s/2*s/2 + s/2*s/2 = r*s/2 + s^2/4 .
    # bei Methode 2:
    #                       |     :     |  r
    #                    |  |     :     |  s
    #      (s-r)*r + r/2*r/2 + r/2*r/2 + r/2*r/2 = r*s - r^2/4 .
    # Wegen (r*s/2 + s^2/4) - (r*s - r^2/4) = (r-s)^2/4 >= 0
    # ist Methode 2 günstiger.
    # Denkfehler! Dies gilt - wenn überhaupt - nur knapp oberhalb des
    # Break-Even-Points.
    # Im allgemeinen ist der Multiplikationsaufwand für zwei Zahlen der
    # Längen u bzw. v nämlich gegeben durch  min(u,v)^c * max(u,v),
    # wobei  c = log3/log2 - 1 = 0.585...
    # Dadurch wird der Aufwand in Abhängigkeit des Parameters t = k,
    # r/2 <= t <= s/2 (der einzig sinnvolle Bereich), zu
    # (r-t)^c*(s-t) + t^c*(s-t) + t^(1+c).
    # Dessen Optimum liegt (im Bereich r <= s <= 2*r)
    # - im klassischen Fall c=1 tatsächlich stets bei t=r/2 [Methode 2],
    # - im Karatsuba-Fall c=0.6 aber offenbar bei t=s/2 [Methode 1]
    #   oder ganz knapp darunter.
    # Auch erweist sich Methode 1 im Experiment als effizienter.
    # Daher implementieren wir Methode 1 :
    #
    # Es ist 2 <= len2 <= len1.
    var bool first_part = true; # Flag, ob jetzt das erste Teilprodukt berechnet wird
    if (len2 >= 2*len1) {
      SAVE_NUM_STACK
      # Teilprodukte von jeweils len1 mal len1 Digits bilden:
      var uintC k_lo = floor(len1,2); # Länge der Low-Teile: floor(len1/2) >0
      var uintC k_hi = len1 - k_lo; # Länge der High-Teile: ceiling(len1/2) >0
      # Es gilt k_lo <= k_hi <= len1, k_lo + k_hi = len1.
      # Summe x1+x0 berechnen:
      var uintD* sum1_MSDptr;
      var uintC sum1_len = k_hi; # = max(k_lo,k_hi)
      var uintD* sum1_LSDptr;
      num_stack_need(sum1_len+1,sum1_MSDptr=,sum1_LSDptr=);
      sum1_MSDptr++; # 1 Digit vorne als Reserve
      {
        var uintD carry = # Hauptteile von x1 und x0 addieren:
          add_loop_down(&sourceptr1[-(uintP)k_lo],sourceptr1,sum1_LSDptr,k_lo);
        if (!(k_lo==k_hi)) {
          # noch k_hi-k_lo = 1 Digits abzulegen
          sum1_MSDptr[0] = sourceptr1[-(uintP)len1]; # = sourceptr1[-2*(uintP)k_lo-1]
          if (!(carry==0)) {
            if (++(sum1_MSDptr[0]) == 0)
              carry=1;
            else
              carry=0;
          }
        }
        if (carry) {
          *--sum1_MSDptr = 1;
          sum1_len++;
        }
      }
      # Platz für Summe y1+y0 belegen:
      var uintC sum2_maxlen = k_hi+1;
      var uintD* sum2_LSDptr;
      num_stack_need(sum2_maxlen,_EMA_,sum2_LSDptr=);
      # Platz für Produkte x0*y0, x1*y1 belegen:
      var uintD* prod_MSDptr;
      var uintD* prod_LSDptr;
      var uintD* prodhi_LSDptr;
      num_stack_need(2*(uintL)len1,prod_MSDptr=,prod_LSDptr=);
      prodhi_LSDptr = &prod_LSDptr[-2*(uintP)k_lo];
      # prod_MSDptr/2*len1/prod_LSDptr wird zuerst die beiden
      # Produkte x1*y1 in prod_MSDptr/2*k_hi/prodhi_LSDptr
      #      und x0*y0 in prodhi_LSDptr/2*k_lo/prod_LSDptr,
      # dann das Produkt (b^k*x1+x0)*(b^k*y1+y0) enthalten.
      # Platz fürs Produkt (x1+x0)*(y1+y0) belegen:
      var uintD* prodmid_MSDptr;
      var uintD* prodmid_LSDptr;
      num_stack_need(sum1_len+sum2_maxlen,prodmid_MSDptr=,prodmid_LSDptr=);
      # Schleife über die hinteren Einzelteile:
      do {
        # Produkt x0*y0 berechnen:
        mulu_2loop_down(sourceptr1,k_lo,sourceptr2,k_lo,prod_LSDptr);
        # Produkt x1*y1 berechnen:
        mulu_2loop_down(&sourceptr1[-(uintP)k_lo],k_hi,&sourceptr2[-(uintP)k_lo],k_hi,prodhi_LSDptr);
        # Summe y1+y0 berechnen:
        {
          var uintC sum2_len = k_hi; # = max(k_lo,k_hi)
          var uintD* sum2_MSDptr = &sum2_LSDptr[-(uintP)sum2_len];
          {
            var uintD carry = # Hauptteile von y1 und y0 addieren:
              add_loop_down(&sourceptr2[-(uintP)k_lo],sourceptr2,sum2_LSDptr,k_lo);
            if (!(k_lo==k_hi)) {
              # noch k_hi-k_lo = 1 Digits abzulegen
              sum2_MSDptr[0] = sourceptr2[-(uintP)len1]; # = sourceptr2[-2*(uintP)k_lo-1]
              if (!(carry==0)) {
                if (++(sum2_MSDptr[0]) == 0)
                  carry=1;
                else
                  carry=0;
              }
            }
            if (carry) {
              *--sum2_MSDptr = 1;
              sum2_len++;
            }
          }
          # Produkt (x1+x0)*(y1+y0) berechnen:
          mulu_2loop_down(sum1_LSDptr,sum1_len,sum2_LSDptr,sum2_len,prodmid_LSDptr);
          # Das Produkt beansprucht  2*k_hi + (0 oder 1) <= sum1_len + sum2_len  Digits.
          var uintC prodmid_len = sum1_len+sum2_len;
          # Davon x1*y1 abziehen:
          {
            var uintD carry =
              subfrom_loop_down(prodhi_LSDptr,prodmid_LSDptr,2*k_hi);
            # Falls Carry: Produkt beansprucht 2*k_hi+1 Digits.
            # Carry um maximal 1 Digit weitertragen:
            if (!(carry==0))
              prodmid_LSDptr[-2*(uintP)k_hi-1] -= 1;
          }
          # Und x0*y0 abziehen:
          {
            var uintD carry =
              subfrom_loop_down(prod_LSDptr,prodmid_LSDptr,2*k_lo);
            # Carry um maximal prodmid_len-2*k_lo Digits weitertragen:
            if (!(carry==0))
              dec_loop_down(&prodmid_LSDptr[-2*(uintP)k_lo],prodmid_len-2*k_lo);
          }
          # prodmid_LSDptr[-prodmid_len..-1] enthält nun x0*y1+x1*y0.
          # Dies wird zu prod = x1*y1*b^(2*k) + x0*y0 addiert:
          {
            var uintD carry =
              addto_loop_down(prodmid_LSDptr,&prod_LSDptr[-(uintP)k_lo],prodmid_len);
              # (Benutze dabei k_lo+prodmid_len <= k_lo+2*(k_hi+1) = 2*len1-k_lo+2 <= 2*len1 .)
            if (!(carry==0))
              inc_loop_down(&prod_LSDptr[-(uintP)(k_lo+prodmid_len)],2*len1-(k_lo+prodmid_len));
          }
        }
        # Das Teilprodukt zum Gesamtprodukt addieren:
        if (first_part) {
          copy_loop_down(prod_LSDptr,destptr,2*len1);
          destptr -= len1;
          first_part = false;
        } else {
          var uintD carry =
            addto_loop_down(prod_LSDptr,destptr,len1);
          destptr -= len1;
          copy_loop_down(&prod_LSDptr[-(uintP)len1],destptr,len1);
          if (!(carry==0))
            inc_loop_down(destptr,len1);
        }
        sourceptr2 -= len1; len2 -= len1;
      } while (len2 >= 2*len1);
      RESTORE_NUM_STACK
    }
    # Nun ist len1 <= len2 < 2*len1.
    # letztes Teilprodukt von len1 mal len2 Digits bilden:
    {
      SAVE_NUM_STACK
      var uintD* prod_MSDptr;
      var uintC prod_len = len1+len2;
      var uintD* prod_LSDptr;
      num_stack_need((uintL)prod_len,prod_MSDptr=,prod_LSDptr=);
      {
        var uintC k_hi = floor(len2,2); # Länge der High-Teile: floor(len2/2) >0
        var uintC k_lo = len2 - k_hi; # Länge der Low-Teile: ceiling(len2/2) >0
        # Es gilt k_hi <= k_lo <= len1 <= len2, k_lo + k_hi = len2.
        var uintC x1_len = len1-k_lo; # <= len2-k_lo = k_hi <= k_lo
        # Summe x1+x0 berechnen:
        var uintD* sum1_MSDptr;
        var uintC sum1_len = k_lo; # = max(k_lo,k_hi)
        var uintD* sum1_LSDptr;
        num_stack_need(sum1_len+1,sum1_MSDptr=,sum1_LSDptr=);
        sum1_MSDptr++; # 1 Digit vorne als Reserve
        {
          var uintD carry = # x1 und unteren Teil von x0 addieren:
            add_loop_down(&sourceptr1[-(uintP)k_lo],sourceptr1,sum1_LSDptr,x1_len);
          # und den oberen Teil von x0 dazu:
          copy_loop_down(&sourceptr1[-(uintP)x1_len],&sum1_LSDptr[-(uintP)x1_len],k_lo-x1_len);
          if (!(carry==0)) {
            carry = inc_loop_down(&sum1_LSDptr[-(uintP)x1_len],k_lo-x1_len);
            if (carry) {
              *--sum1_MSDptr = 1;
              sum1_len++;
            }
          }
        }
        # Summe y1+y0 berechnen:
        var uintD* sum2_MSDptr;
        var uintC sum2_len = k_lo; # = max(k_lo,k_hi)
        var uintD* sum2_LSDptr;
        num_stack_need(sum2_len+1,sum2_MSDptr=,sum2_LSDptr=);
        sum2_MSDptr++; # 1 Digit vorne als Reserve
        {
          var uintD carry = # Hauptteile von y1 und y0 addieren:
            add_loop_down(&sourceptr2[-(uintP)k_lo],sourceptr2,sum2_LSDptr,k_hi);
          if (!(k_lo==k_hi)) {
            # noch k_lo-k_hi = 1 Digits abzulegen
            sum2_MSDptr[0] = sourceptr2[-(uintP)k_lo]; # = sourceptr2[-(uintP)k_hi-1]
            if (!(carry==0)) {
              if (++(sum2_MSDptr[0]) == 0)
                carry=1;
              else
                carry=0;
            }
          }
          if (carry) {
            *--sum2_MSDptr = 1;
            sum2_len++;
          }
        }
        # Platz für Produkte x0*y0, x1*y1:
        var uintC prodhi_len = x1_len+k_hi;
        var uintD* prodhi_LSDptr = &prod_LSDptr[-2*(uintP)k_lo];
        # prod_MSDptr/len1+len2/prod_LSDptr wird zuerst die beiden
        # Produkte x1*y1 in prod_MSDptr/x1_len+k_hi/prodhi_LSDptr
        #      und x0*y0 in prodhi_LSDptr/2*k_lo/prod_LSDptr,
        # dann das Produkt (b^k*x1+x0)*(b^k*y1+y0) enthalten.
        # Platz fürs Produkt (x1+x0)*(y1+y0) belegen:
        var uintD* prodmid_MSDptr;
        var uintC prodmid_len = sum1_len+sum2_len;
        var uintD* prodmid_LSDptr;
        num_stack_need(prodmid_len,prodmid_MSDptr=,prodmid_LSDptr=);
        # Produkt (x1+x0)*(y1+y0) berechnen:
        mulu_2loop_down(sum1_LSDptr,sum1_len,sum2_LSDptr,sum2_len,prodmid_LSDptr);
        # Das Produkt beansprucht  2*k_lo + (0 oder 1) <= sum1_len + sum2_len = prodmid_len  Digits.
        # Produkt x0*y0 berechnen:
        mulu_2loop_down(sourceptr1,k_lo,sourceptr2,k_lo,prod_LSDptr);
        # Produkt x1*y1 berechnen:
        if (!(x1_len==0)) {
          mulu_2loop_down(&sourceptr1[-(uintP)k_lo],x1_len,&sourceptr2[-(uintP)k_lo],k_hi,prodhi_LSDptr);
          # Und x1*y1 abziehen:
          {
            var uintD carry =
              subfrom_loop_down(prodhi_LSDptr,prodmid_LSDptr,prodhi_len);
            # Carry um maximal prodmid_len-prodhi_len Digits weitertragen:
            if (!(carry==0))
              dec_loop_down(&prodmid_LSDptr[-(uintP)prodhi_len],prodmid_len-prodhi_len);
          }
        } else {
          # Produkt x1*y1=0, nichts abzuziehen
          clear_loop_down(prodhi_LSDptr,prodhi_len);
        }
        # Und x0*y0 abziehen:
        {
          var uintD carry =
            subfrom_loop_down(prod_LSDptr,prodmid_LSDptr,2*k_lo);
          # Falls Carry: Produkt beansprucht 2*k_lo+1 Digits.
          # Carry um maximal 1 Digit weitertragen:
          if (!(carry==0))
            prodmid_LSDptr[-2*(uintP)k_lo-1] -= 1;
        }
        # prodmid_LSDptr[-prodmid_len..-1] enthält nun x0*y1+x1*y0.
        # Dies ist < b^k_lo * b^k_hi + b^x1_len * b^k_lo
        #          = b^len2 + b^len1 <= 2 * b^len2,
        # passt also in len2+1 Digits.
        # Im Fall x1_len=0 ist es sogar < b^k_lo * b^k_hi = b^len2,
        # es passt also in len2 Digits.
        # prodmid_len, wenn möglich, um maximal 2 verkleinern:
        # (benutzt prodmid_len >= 2*k_lo >= len2 >= 2)
        if (prodmid_MSDptr[0]==0) {
          prodmid_len--;
          if (prodmid_MSDptr[1]==0)
            prodmid_len--;
        }
        # Nun ist k_lo+prodmid_len <= len1+len2 .
        # (Denn es war prodmid_len = sum1_len+sum2_len <= 2*(k_lo+1)
        #  <= len2+3, und nach 2-maliger Verkleinerung jedenfalls
        #  prodmid_len <= len2+1. Im Falle k_lo < len1 also
        #  k_lo + prodmid_len <= (len1-1)+(len2+1) = len1+len2.
        #  Im Falle k_lo = len1 aber ist x1_len=0, sum1_len = k_lo, also
        #  war prodmid_len = sum1_len+sum2_len <= 2*k_lo+1 <= len2+2,
        #  nach 2-maliger Verkleinerung jedenfalls prodmid_len <= len2.)
        # prodmid*b^k = (x0*y1+x1*y0)*b^k zu prod = x1*y1*b^(2*k) + x0*y0 addieren:
        {
          var uintD carry =
            addto_loop_down(prodmid_LSDptr,&prod_LSDptr[-(uintP)k_lo],prodmid_len);
          if (!(carry==0))
            inc_loop_down(&prod_LSDptr[-(uintP)(k_lo+prodmid_len)],prod_len-(k_lo+prodmid_len));
        }
      }
      # Das Teilprodukt zum Gesamtprodukt addieren:
      if (first_part) {
        copy_loop_down(prod_LSDptr,destptr,prod_len);
      } else {
        var uintD carry =
          addto_loop_down(prod_LSDptr,destptr,len1);
        destptr -= len1;
        copy_loop_down(&prod_LSDptr[-(uintP)len1],destptr,len2);
        if (!(carry==0))
          inc_loop_down(destptr,len2);
      }
      RESTORE_NUM_STACK
    }
  }

# Multipliziert zwei Digit-sequences.
# DS_DS_mal_DS(MSDptr1,len1,LSDptr1, MSDptr2,len2,LSDptr2, MSDptr=,len=,LSDptr=);
# multipliziert die DS MSDptr1/len1/LSDptr1 und MSDptr2/len2/LSDptr2.
# Dabei sollte len1>0 und len2>0 sein. Alles sollten Variablen sein!
# Ergebnis ist die DS MSDptr/len/LSDptr, mit len=len1+len2, im Stack.
# Dabei wird num_stack erniedrigt.
  # Methode:
  # Erst unsigned multiplizieren. Dann bis zu zwei Subtraktionen.
  # Sei b=2^intDsize, k=len1, l=len2, n=DS1, m=DS2.
  # Gesucht ist n * m.
  # Wir errechnen erst das unsigned-product p (mod b^(k+l)).
  # n>0, m>0: p = n*m,             n*m = p
  # n<0, m>0: p = (n+b^k)*m,       n*m + b^(k+l) = p - b^k * m (mod b^(k+l)).
  # n>0, m<0: p = n*(m+b^l),       n*m + b^(k+l) = p - b^l * n (mod b^(k+l)).
  # n<0, m<0: p = (n+b^k)*(m+b^l),
  #           n*m = p - b^k * (m+b^l) - b^l * (n+b^k) (mod b^(k+l)).
  #define DS_DS_mal_DS(MSDptr1,len1,LSDptr1,MSDptr2,len2,LSDptr2, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung)  \
    {                                                             \
      var uintD* LSDptr0;                                         \
      UDS_UDS_mal_UDS(len1,LSDptr1,len2,LSDptr2, MSDptr_zuweisung,len_zuweisung,LSDptr_zuweisung LSDptr0 = ); \
      if ((sintD)(MSDptr1[0]) < 0) # n<0 ?                        \
        # muss m bzw. m+b^l subtrahieren, um k Digits verschoben: \
        subfrom_loop_down(LSDptr2,&LSDptr0[-(uintP)len1],len2);   \
      if ((sintD)(MSDptr2[0]) < 0) # m<0 ?                        \
        # muss n bzw. n+b^k subtrahieren, um l Digits verschoben: \
        subfrom_loop_down(LSDptr1,&LSDptr0[-(uintP)len2],len1);   \
    }

# (* x y), wo x und y Integers sind. Ergebnis Integer.
# can trigger GC
  # Methode:
  # x=0 oder y=0 -> Ergebnis 0
  # x und y beide Fixnums -> direkt multiplizieren
  # sonst: zu WS machen, multiplizieren.
  local maygc object I_I_mal_I (object x, object y)
  {
    if (eq(x,Fixnum_0) || eq(y,Fixnum_0))
      return Fixnum_0;
    if (I_fixnump(x) && I_fixnump(y)) {
      var sintV x_ = FN_to_V(x);
      var sintV y_ = FN_to_V(y);
      #if (oint_data_len+1 > 32)
      # nur falls x und y Integers mit höchstens 32 Bit sind:
      if (((uintV)((sintV)FN_sign(x) ^ x_) < vbit(31))
          && ((uintV)((sintV)FN_sign(y) ^ y_) < vbit(31)))
      #endif
        {
          # Werte direkt multiplizieren:
          var uint32 hi;
          var uint32 lo;
          mulu32((uint32)x_,(uint32)y_,hi=,lo=); # erst unsigned multiplizieren
          if (x_ < 0)
            hi -= (uint32)y_; # dann Korrektur für Vorzeichen
          if (y_ < 0)
            hi -= (uint32)x_; # (vgl. DS_DS_mal_DS)
          return L2_to_I(hi,lo);
        }
    }
    var uintD* xMSDptr;
    var uintC xlen;
    var uintD* xLSDptr;
    I_to_NDS_nocopy(x, xMSDptr = , xlen = , xLSDptr = );
    var uintD* yMSDptr;
    var uintC ylen;
    var uintD* yLSDptr;
    I_to_NDS_nocopy(y, yMSDptr = , ylen = , yLSDptr = );
    {
      SAVE_NUM_STACK # num_stack retten
      var uintD* ergMSDptr;
      var uintC erglen;
      begin_arith_call();
      DS_DS_mal_DS(xMSDptr,xlen,xLSDptr,yMSDptr,ylen,yLSDptr, ergMSDptr=,erglen=,);
      end_arith_call();
      var object result = DS_to_I(ergMSDptr,erglen);
      RESTORE_NUM_STACK # num_stack zurück
      return result;
    }
  }

# (EXPT x y), wo x Integer, y Integer >0 ist.
# can trigger GC
  local maygc object I_I_expt_I (object x, object y);
  # Methode:
  #   a:=x, b:=y, c:=1. [a^b*c bleibt invariant, = x^y.]
  #   Solange b>1,
  #     falls b ungerade, setze c:=a*c,
  #     setze b:=floor(b/2),
  #     setze a:=a*a.
  #   Wenn b=1, setze c:=a*c.
  #   Liefere c.
  # Oder optimiert:
  #   a:=x, b:=y.
  #   Solange b gerade, setze a:=a*a, b:=b/2. [a^b bleibt invariant, = x^y.]
  #   c:=a.
  #   Solange b:=floor(b/2) >0 ist,
  #     setze a:=a*a, und falls b ungerade, setze c:=a*c.
  #   Liefere c.
  local maygc object I_I_expt_I (object x, object y)
  {
    #if 0 # unoptimiert
      pushSTACK(x); pushSTACK(Fixnum_1); pushSTACK(y);
      # Stackaufbau: a, c, b.
      until (eq(STACK_0,Fixnum_1)) { # solange bis b=1
        if (I_oddp(STACK_0)) # b ungerade?
          STACK_1 = I_I_mal_I(STACK_2,STACK_1); # c:=a*c
        STACK_0 = I_I_ash_I(STACK_0,Fixnum_minus1); # b := (ash b -1) = (floor b 2)
        STACK_2 = I_square_I(STACK_2); # a:=a*a
      }
      skipSTACK(1);
      var object c = popSTACK();
      var object a = popSTACK();
      return I_I_mal_I(a,c); # a*c als Ergebnis
    #else # optimiert
      pushSTACK(x); pushSTACK(y);
      # Stackaufbau: a, b.
      while (!I_oddp(y)) {
        STACK_1 = I_square_I(STACK_1); # a:=a*a
        STACK_0 = y = I_I_ash_I(STACK_0,Fixnum_minus1); # b := (ash b -1)
      }
      pushSTACK(STACK_1); # c:=a
      # Stackaufbau: a, b, c.
      until (eq(y=STACK_1,Fixnum_1)) { # Solange b/=1
        STACK_1 = I_I_ash_I(y,Fixnum_minus1); # b := (ash b -1)
        var object a = STACK_2 = I_square_I(STACK_2); # a:=a*a
        if (I_oddp(STACK_1))
          STACK_0 = I_I_mal_I(a,STACK_0); # evtl. c:=a*c
      }
      var object erg = STACK_0;
      skipSTACK(3);
      return erg;
    #endif
  }

# Fakultät (! n), wo n Fixnum >=0 ist. Ergebnis Integer.
# can trigger GC
  # Methode:
  # n <= 10 -> Ergebnis (Fixnum) aus Tabelle
  # Sonst:
  #   Zweierpotenzen extra am Schluss durch einen Shift um
  #   ord2(n!) = sum(k>=1, floor(n/2^k) ) = n - logcount(n)  Bits.
  #   Für k>=1 wird jede ungerade Zahl m im Intervall n/2^k < m <= n/2^(k-1)
  #   genau k mal gebraucht (als ungerader Anteil von m*2^0,...,m*2^(k-1) ).
  #   Zur Bestimmung des Produkts aller ungeraden Zahlen in einem Intervall
  #   a < m <= b verwenden wir eine rekursive Funktion, die nach Divide-and-
  #   Conquer das Produkt über die Intervalle a < m <= c und c < m <= b
  #   (c := floor((a+b)/2)) bestimmt und beide zusammenmultipliziert. Dies
  #   vermeidet, dass oft große Zahlen mit ganz kleinen Zahlen multipliziert
  #   werden.
  local maygc object FN_fak_I (object n);
  # UP für Fakultät:
  # Bilde das Produkt prod(a < i <= b, 2*i+1), wobei 0 <= a < b klein.
    local object prod_ungerade (uintV a, uintV b)
    {
      var uintV diff = b-a; # Anzahl der Faktoren
      if (diff <= 4) {
        # Produkt iterativ bilden
        var object faktor = fixnum(2*b+1); # 2*b+1 als letzter Faktor
        var object produkt = faktor;
        var uintC count;
        dotimesC(count,diff-1, {
          faktor = fixnum_inc(faktor,-2); # nächster Faktor
          produkt = I_I_mal_I(faktor,produkt); # mit bisherigem Produkt multiplizieren
        });
        return produkt;
      } else {
        # Produkt rekursiv bilden
        var uintV c = floor(a+b,2); # c:=floor((a+b)/2)
        var object teil = prod_ungerade(a,c); # erstes Teilprodukt
        pushSTACK(teil);
        teil = prod_ungerade(c,b); # zweites Teilprodukt
        return I_I_mal_I(popSTACK(),teil); # und beide multiplizieren
      }
    }
  local maygc object FN_fak_I (object n)
  {
    local var const uintV fakul_table [] = {
      1UL,
      1UL,
      1UL*2,
      #if (oint_data_len>=3)
      1UL*2*3,
      #if (oint_data_len>=5)
      1UL*2*3*4,
      #if (oint_data_len>=7)
      1UL*2*3*4*5,
      #if (oint_data_len>=10)
      1UL*2*3*4*5*6,
      #if (oint_data_len>=13)
      1UL*2*3*4*5*6*7,
      #if (oint_data_len>=16)
      1UL*2*3*4*5*6*7*8,
      #if (oint_data_len>=19)
      1UL*2*3*4*5*6*7*8*9,
      #if (oint_data_len>=22)
      1UL*2*3*4*5*6*7*8*9*10,
      #if (oint_data_len>=26)
      1UL*2*3*4*5*6*7*8*9*10*11,
      #if (oint_data_len>=29)
      1UL*2*3*4*5*6*7*8*9*10*11*12,
      #if (oint_data_len>=33)
      ULL(1)*2*3*4*5*6*7*8*9*10*11*12*13,
      #if (oint_data_len>=37)
      ULL(1)*2*3*4*5*6*7*8*9*10*11*12*13*14,
      #if (oint_data_len>=41)
      ULL(1)*2*3*4*5*6*7*8*9*10*11*12*13*14*15,
      #if (oint_data_len>=45)
      ULL(1)*2*3*4*5*6*7*8*9*10*11*12*13*14*15*16,
      #if (oint_data_len>=49)
      ULL(1)*2*3*4*5*6*7*8*9*10*11*12*13*14*15*16*17,
      #if (oint_data_len>=53)
      ULL(1)*2*3*4*5*6*7*8*9*10*11*12*13*14*15*16*17*18,
      #if (oint_data_len>=57)
      ULL(1)*2*3*4*5*6*7*8*9*10*11*12*13*14*15*16*17*18*19,
      #if (oint_data_len>=62)
      ULL(1)*2*3*4*5*6*7*8*9*10*11*12*13*14*15*16*17*18*19*20,
      #if (oint_data_len>=66)
      ...
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      #endif
      };
    var uintV n_ = posfixnum_to_V(n);
    if (n_ < sizeof(fakul_table)/sizeof(uintV)) {
      return fixnum(fakul_table[n_]);
    } else {
      pushSTACK(Fixnum_1); # bisheriges Produkt := 1
      pushSTACK(n);        # n
      pushSTACK(Fixnum_1); # k := 1
      pushSTACK(n);        # obere Intervallgrenze floor(n/2^(k-1))
      loop {
        # Stackaufbau: prod, n, k, floor(n/2^(k-1)).
        # 'n' enthält floor(n/2^(k-1)).
        n = I_I_ash_I(n,Fixnum_minus1); # untere Grenze floor(n/2^k)
        # 'n' enthält floor(n/2^k).
        # Bilde Teilprodukt prod(A < i <= B & oddp(i), i)
        #       = prod(floor((A-1)/2) < i <= floor((B-1)/2), 2*i+1)
        # wobei B = floor(n/2^(k-1)), A = floor(n/2^k) = floor(B/2).
        {
          var uintV b = floor(posfixnum_to_V(STACK_0)-1,2);
          if (b==0) # B=2 oder B=1 -> Produkt fertig
            break;
          var uintV a = floor(posfixnum_to_V(n)-1,2);
          pushSTACK(n);
          var object temp = prod_ungerade(a,b);
          temp = I_I_expt_I(temp,STACK_2); # hoch k nehmen
          STACK_4 = I_I_mal_I(temp,STACK_4); # und aufmultiplizieren
        }
        STACK_2 = fixnum_inc(STACK_2,1); # k:=k+1
        n = popSTACK(); STACK_0 = n;
      }
      skipSTACK(2);
      # Stackaufbau: prod, n.
      var object temp = I_logcount_I(STACK_0); # (logcount n)
      temp = I_I_minus_I(popSTACK(),temp); # (- n (logcount n))
      return I_I_ash_I(popSTACK(),temp); # (ash prod (- n (logcount n)))
    }
  }

