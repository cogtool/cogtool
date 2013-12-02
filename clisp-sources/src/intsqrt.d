# Wurzel aus ganzen Zahlen

# Zieht die Ganzzahl-Wurzel aus einer 32-Bit-Zahl und
# liefert eine 16-Bit-Wurzel.
# UL_sqrt_UW(x)
# > uintL x : Radikand, >=0, <2^32
# < uintWL ergebnis : Wurzel, >=0, <2^16
  local uintWL UL_sqrt_UW (uintL x);
  # Methode:
  # x=0 -> y=0, fertig.
  # y := 2^k als Anfangswert, wobei k>0, k<=16 mit 2^(2k-2) <= x < 2^(2k) sei.
  # y := floor((y + floor(x/y))/2) als nächster Wert,
  # solange z := floor(x/y) < y, setze y := floor((y+z)/2).
  # y ist fertig.
  # (Beweis:
  #  1. Die Folge der y ist streng monoton fallend.
  #  2. Stets gilt y >= floor(sqrt(x)) (denn für alle y>0 ist
  #     y + x/y >= 2*sqrt(x) und daher  floor((y + floor(x/y))/2) =
  #     floor(y/2 + x/(2*y)) >= floor(sqrt(x)) ).
  #  3. Am Schluss gilt x >= y^2.
  # )
  local uintWL UL_sqrt_UW (uintL x)
  {
    if (x==0)
      return 0; # x=0 -> y=0
    var uintC k2; integerlength32(x,k2=); # 2^(k2-1) <= x < 2^k2
    var uintC k1 = floor(k2-1,2); # k1 = k-1, k wie oben
    if (k1 < 16-1) {
      # k < 16
      var uintWL y = (x >> (k1+2)) | bit(k1); # stets 2^(k-1) <= y < 2^k
      loop {
        var uintWL z;
        divu_3216_1616(x,y, z=,); # Dividiere x/y (geht, da x/y < 2^(2k)/2^(k-1) = 2^(k+1) <= 2^16)
        if (z >= y)
          break;
        y = floor(z+y,2); # geht, da z+y < 2*y < 2^(k+1) <= 2^16
      }
      return y;
    } else {
      # k = 16, Vorsicht!
      var uintWL x1 = high16(x);
      var uintWL y = (x >> (16+1)) | bit(16-1); # stets 2^(k-1) <= y < 2^k
      loop {
        var uintWL z;
        if (x1 >= y) # Division x/y ergäbe Überlauf -> z > y
          break;
        divu_3216_1616(x,y, z=,); # Dividiere x/y
        if (z >= y)
          break;
        y = floor(z+y,2);
        if (intWLsize<=16)
          y |= bit(16-1); # y muss >= 2^(k-1) bleiben
      }
      return y;
    }
  }

#if 0 # unbenutzt
# Zieht die Ganzzahl-Wurzel aus einer 64-Bit-Zahl und
# liefert eine 32-Bit-Wurzel.
# UL2_sqrt_UL(x1,x0)
# > uintL2 x = x1*2^32+x0 : Radikand, >=0, <2^64
# < uintL ergebnis : Wurzel, >=0, <2^32
  local uintL UL2_sqrt_UL (uintL x1, uintL x0);
  # Methode:
  # x=0 -> y=0, fertig.
  # y := 2^k als Anfangswert, wobei k>0, k<=32 mit 2^(2k-2) <= x < 2^(2k) sei.
  # y := floor((y + floor(x/y))/2) als nächster Wert,
  # solange z := floor(x/y) < y, setze y := floor((y+z)/2).
  # y ist fertig.
  # (Beweis:
  #  1. Die Folge der y ist streng monoton fallend.
  #  2. Stets gilt y >= floor(sqrt(x)) (denn für alle y>0 ist
  #     y + x/y >= 2*sqrt(x) und daher  floor((y + floor(x/y))/2) =
  #     floor(y/2 + x/(2*y)) >= floor(sqrt(x)) ).
  #  3. Am Schluss gilt x >= y^2.
  # )
  local uintL UL2_sqrt_UL (uintL x1, uintL x0)
  {
    if (x1==0) # x klein?
      return UL_sqrt_UW(x0);
    var uintC k2; integerlength32(x1,k2=); # 2^(k2+32-1) <= x < 2^(k2+32)
    var uintC k = ceiling(k2+32,2); # k wie oben
    if (k < 32) {
      # k < 32
      var uintL y = ((x1 << (32-k)) | (x0 >> k) | bit(k)) >> 1; # stets 2^(k-1) <= y < 2^k
      loop {
        var uintL z;
        divu_6432_3232(x1,x0,y, z=,); # Dividiere x/y (geht, da x/y < 2^(2k)/2^(k-1) = 2^(k+1) <= 2^32)
        if (z >= y)
          break;
        y = floor(z+y,2); # geht, da z+y < 2*y < 2^(k+1) <= 2^32
      }
      return y;
    } else {
      # k = 32, Vorsicht!
      var uintL y = (x1 >> 1) | bit(32-1); # stets 2^(k-1) <= y < 2^k
      loop {
        var uintL z;
        if (x1 >= y) # Division x/y ergäbe Überlauf -> z > y
          break;
        divu_6432_3232(x1,x0,y, z=,); # Dividiere x/y
        if (z >= y)
          break;
        y = floor(z+y,2) | bit(32-1); # y muss >= 2^(k-1) bleiben
      }
      return y;
    }
  }
#endif

# Bildet zu einer Unsigned Digit sequence a die Wurzel
# (genauer: Gaußklammer aus Wurzel aus a).
# UDS_sqrt(a_MSDptr,a_len,a_LSDptr, &b, squarep=)
# > a_MSDptr/a_len/a_LSDptr: eine UDS
# < NUDS b: Gaußklammer der Wurzel aus a
# < squarep: true falls a = b^2, false falls b^2 < a < (b+1)^2.
# a wird nicht modifiziert.
# Vorzeichenerweiterung von b ist erlaubt.
# num_stack wird erniedrigt.
  #define UDS_sqrt(a_MSDptr,a_len,a_LSDptr,b_,squarep_zuweisung)  \
    {                                                           \
      # ceiling(a_len,2) Digits Platz fürs Ergebnis machen:     \
      var uintC _a_len = (a_len);                               \
      num_stack_need_1(ceiling(_a_len,2),(b_)->MSDptr=,);       \
      squarep_zuweisung UDS_sqrt_(a_MSDptr,_a_len,a_LSDptr,b_); \
    }
  local bool UDS_sqrt_ (uintD* a_MSDptr, uintC a_len, uintD* a_LSDptr, DS* b_);
# Methode:
# erst A normalisieren. A=0 --> B=0, fertig.
# Wähle n so, dass beta^(2n-2) <= A < beta^(2n).
# Wähle s (0<=s<16) so, dass beta^(2n)/4 <= A*2^(2s) < beta^(2n).
# Setze A:=A*2^(2s) und kopiere dabei A. Suche B=floor(sqrt(A)).
# Mache Platz für B=[0,b[n-1],...,b[0]], (mit einem Nulldigit Platz davor,
# da dort nicht B, sondern 2*B abgespeichert werden wird).
# Auf den Plätzen [a[2n-1],...,a[2n-2j]] wird die Differenz
# [a[2n-1],...,a[2n-2j]] - [b[n-1],...,b[n-j]] ^ 2 abgespeichert.
# Bestimme b[n-1] = floor(sqrt(a[2n-1]*beta+a[2n-2])) mit Heron/Newton:
#   {x:=beta als vorheriger Anfangswert, dann:}
#   x := floor((beta+a[2n-1])/2)
#   wiederhole: d:=floor((a[2n-1]*beta+a[2n-2])/x).
#               Falls d<beta (kein Überlauf) und d<x,
#                 setze x:=floor((x+d)/2), nochmals.
#   b[n-1]:=x. In B um ein Bit nach links verschoben abspeichern.
# {Wegen a[2n-1]>=beta/4 ist b[n-1]>=beta/2.}
# Erniedrige [a[2n-1],a[2n-2]] um b[n-1]^2.
# Für j=1,...,n:
#   {Hier [b[n-1],...,b[n-j]] = floor(sqrt(altes [a[2n-1],...,a[2n-2j]])),
#     in [a[2n-1],...,a[2n-2j]] steht jetzt der Rest
#     [a[2n-1],...,a[2n-2j]] - [b[n-1],...,b[n-j]]^2, er ist >=0 und
#     und <= 2 * [b[n-1],...,b[n-j]], belegt daher höchstens j Digits und 1 Bit.
#     Daher sind nur [a[2n-j],...,a[2n-2j]] von Belang.}
#   Für j<n: Bestimme die nächste Ziffer:
#     b* := min(beta-1,floor([a[2n-j],...,a[2n-2j-1]]/(2*[b[n-1],...,b[n-j]]))).
#     und [a[2n-j],...,a[2n-2j-1]] :=
#         [a[2n-j],...,a[2n-2j-1]] - b* * 2 * [b[n-1],...,b[n-j]] (>= 0).
#     Im einzelnen:
#       b* := min(beta-1,floor([a[2n-j],a[2n-j-1],a[2n-j-2]]/(2*b[n-1]))),
#       [a[2n-j],...,a[2n-2j-1]] wie angegeben erniedigen.
#       Solange die Differenz <0 ist, setze b* := b* - 1 und
#         erhöhe [a[2n-j],...,a[2n-2j-1]] um 2 * [b[n-1],...,b[n-j]].
#     Erniedrige [a[2n-j],...,a[2n-2j-2]] um b* ^ 2.
#     Tritt dabei ein negativer Carry auf,
#       so setze b* := b* - 1,
#          setze b[n-j-1] := b* (im Speicher um 1 Bit nach links verschoben),
#          erhöhe [a[2n-j],...,a[2n-2j-2]] um 2*[b[n-1],...,b[n-j-1]]+1.
#       Sonst setze b[n-j-1] := b* (im Speicher um 1 Bit nach links verschoben).
#     Nächstes j.
#   Für j=n:
#     Falls [a[n],...,a[0]] = [0,...,0], ist die Wurzel exakt, sonst nicht.
#     Ergebnis ist [b[n-1],...,b[0]] * 2^(-s), schiebe also im Speicher
#       [b[n],...,b[0]] um s+1 Bits nach rechts.
#     Das Ergebnis ist eine NUDS der Länge n.
  local bool UDS_sqrt_ (uintD* a_MSDptr, uintC a_len, uintD* a_LSDptr, DS* b_)
  {
    # A normalisieren:
    while ((a_len>0) && (a_MSDptr[0]==0)) {
      a_MSDptr++; a_len--;
    }
    if (a_len==0) { # A=0 -> B := NUDS 0
      b_->LSDptr = b_->MSDptr; b_->len = 0;
      return true;
    }
    {
      SAVE_NUM_STACK # num_stack retten
      # n und s bestimmen:
      var uintC n = ceiling(a_len,2); # a_len = 2n oder 2n-1, n>0.
      var uintL s;
      {
        var uintD msd = a_MSDptr[0]; # a[2n] bzw. a[2n-1]
        #if 0
        s = 0;
        while /* ((msd & (bit(intDsize-1)|bit(intDsize-2))) ==0) */
              (((sintD)msd >= 0) && ((sintD)(msd<<1) >= 0))
          msd = msd<<2; s++;
        #else
        integerlengthD(msd, s = intDsize - ); s = s>>1;
        #endif
      }
      # Noch ist s nur modulo intDsize/2 bestimmt.
      # A um 2s Bits nach links verschoben kopieren:
      {
        var uintD* new_a_LSDptr;
        num_stack_need(2*(uintL)n,a_MSDptr=,new_a_LSDptr=); # 2n Digits Platz belegen
        begin_arith_call();
        var uintL shiftcount = 2*s;
        if (!((a_len & bit(0)) ==0)) { # a_len ungerade?
          s += intDsize/2; *--new_a_LSDptr = 0; # ja -> ein Nulldigit einschieben
        }
        if (shiftcount==0)
          copy_loop_down(a_LSDptr,new_a_LSDptr,a_len);
        else
          shiftleftcopy_loop_down(a_LSDptr,new_a_LSDptr,a_len,shiftcount);
      }
      # Nun ist A = a_MSDptr/2n/..
      # Platz für B belegen:
      var uintD* b_MSDptr = &(b_->MSDptr)[-1]; # ab hier n+1 Digits Platz
      var uintD b_msd;
      # B = [0,b[n-1],...,b[0]] = b_MSDptr/n+1/..
      # Bestimmung von b[n-1]:
      {
        var uintD a_msd = a_MSDptr[0]; # a[2n-1]
        var uintD a_2msd = a_MSDptr[1]; # a[2n-2]
        #if HAVE_DD
        var uintDD a_msdd = highlowDD(a_msd,a_2msd); # a[2n-1]*beta+a[2n-2]
        #endif
        # Anfangswert: x := floor((beta + a[2n-1])/2)
        var uintD x = floor(a_msd,2) | bit(intDsize-1);
        loop { # Heron-Iterationsschleife
          var uintD d;
          # Dividiere d := floor((a[2n-1]*beta+a[2n-2])/x) :
          if (a_msd>=x) # Überlauf -> d>=beta -> fertig
            break;
          #if HAVE_DD
            divuD(a_msdd,x, d=,);
          #else
            divuD(a_msd,a_2msd,x, d=,);
          #endif
          if (d >= x) # d>=x -> fertig
            break;
          # Nächste Iteration: x := floor((x+d)/2)
          # (Da die Folge der x bekanntlich monoton fallend ist
          # und bei b[n-1] >= beta/2 endet, muss x >= beta/2 werden,
          # d.h. x+d>=beta.)
          #if HAVE_DD
            x = (uintD)(floor((uintDD)x + (uintDD)d, 2));
          #else
            x = floor((uintD)(x+d),2) | bit(intDsize-1);
          #endif
        }
        # x = b[n-1] fertig berechnet.
        b_msd = x;
        # Quadrieren und von [a[2n-1],a[2n-2]] abziehen:
        #if HAVE_DD
          a_msdd -= muluD(x,x);
          a_MSDptr[0] = highD(a_msdd); a_MSDptr[1] = lowD(a_msdd);
        #else
          {
            var uintD x2hi;
            var uintD x2lo;
            muluD(x,x, x2hi=,x2lo=);
            a_MSDptr[0] = a_msd - x2hi;
            if (a_2msd < x2lo)
              a_MSDptr[0] -= 1;
            a_MSDptr[1] = a_2msd - x2lo;
          }
        #endif
        b_MSDptr[0] = 1; b_MSDptr[1] = x<<1; # b[n-1] ablegen
      }
      var uintC j = 0;
      var uintD* a_mptr = &a_MSDptr[0];
      var uintD* a_lptr = &a_MSDptr[2];
      var uintD* b_ptr = &b_MSDptr[2];
      # Wurzel-Hauptschleife
      until (++j == n) { # j=1,...,n
        # b_MSDptr = Pointer auf b[n], b_ptr = Pointer hinter b[n-j].
        # a_mptr = Pointer auf a[2n-j], a_lptr = Pointer hinter a[2n-2j].
        # Bestimme b* :
        var uintD b_stern;
        {
          var uintD a_1d = a_mptr[0]; # a[2n-j], =0 oder =1
          var uintD a_2d = a_mptr[1]; # a[2n-j-1]
          var uintD a_3d = a_mptr[2]; # a[2n-j-2]
          # a[2n-j]*beta^2+a[2n-j-1]*beta+a[2n-j-2] durch 2 dividieren,
          # dann durch b_msd = b[n-1] dividieren:
          #if HAVE_DD
            var uintDD a_123dd = highlowDD(a_2d,a_3d);
            a_123dd = a_123dd>>1; if (!(a_1d==0)) { a_123dd |= bit(2*intDsize-1); }
            if (highD(a_123dd) >= b_msd)
              b_stern = bitm(intDsize)-1; # bei Überlauf: beta-1
            else
              divuD(a_123dd,b_msd, b_stern=,);
          #else
            a_3d = a_3d>>1; if (!((a_2d & bit(0)) ==0)) { a_3d |= bit(intDsize-1); }
            a_2d = a_2d>>1; if (!(a_1d==0)) { a_2d |= bit(intDsize-1); }
            if (a_2d >= b_msd)
              b_stern = bitm(intDsize)-1; # bei Überlauf: beta-1
            else
              divuD(a_2d,a_3d,b_msd, b_stern=,);
          #endif
        }
        # b_stern = b* in der ersten Schätzung.
        a_lptr++; # Pointer hinter a[2n-2j-1]
        # Subtraktion [a[2n-j],...,a[2n-2j-1]] -= b* * [b[n],b[n-1],...,b[n-j]] :
        {
          var uintD carry = mulusub_loop_down(b_stern,b_ptr,a_lptr,j+1);
          if (a_mptr[0] >= carry) {
            a_mptr[0] -= carry;
          } else {
            a_mptr[0] -= carry; # a[2n-j] wird <0
            # negativer Übertrag -> b* nach unten korrigieren:
            loop {
              b_stern = b_stern-1; # b* := b* - 1
              # erhöhe [a[2n-j],...,a[2n-2j-1]] um [b[n],...,b[n-j]]:
              if (!(( addto_loop_down(b_ptr,a_lptr,j+1) ==0)))
                if ((a_mptr[0] += 1) ==0) # Übertrag zu a[2n-j]
                  break; # macht a[2n-j] wieder >=0 -> Subtraktionsergebnis >=0
            }
          }
        }
        # b_stern = b* in der zweiten Schätzung.
        a_mptr++; # Pointer auf a[2n-j-1]
        a_lptr++; # Pointer hinter a[2n-2j-2]
        # Ziehe b* ^ 2 von [a[2n-j],...,a[2n-2j-2]] ab:
        #if HAVE_DD
        {
          var uintDD b_stern_2 = muluD(b_stern,b_stern);
          var uintDD a_12dd = highlowDD(a_lptr[-2],a_lptr[-1]); # a[2n-2j-1]*beta+a[2n-2j-2]
          var uintDD a_12dd_new = a_12dd - b_stern_2;
          a_lptr[-2] = highD(a_12dd_new); a_lptr[-1] = lowD(a_12dd_new);
          if (a_12dd >= b_stern_2)
            goto b_stern_ok;
        }
        #else
        {
          var uintD b_stern_2_hi;
          var uintD b_stern_2_lo;
          muluD(b_stern,b_stern, b_stern_2_hi=,b_stern_2_lo=);
          var uintD a_1d = a_lptr[-2]; # a[2n-2j-1]
          var uintD a_2d = a_lptr[-1]; # a[2n-2j-2]
          var uintD a_1d_new = a_1d - b_stern_2_hi;
          var uintD a_2d_new = a_2d - b_stern_2_lo;
          if (a_2d < b_stern_2_lo)
            a_1d_new -= 1;
          a_lptr[-2] = a_1d_new; a_lptr[-1] = a_2d_new;
          if ((a_1d > b_stern_2_hi)
              || ((a_1d == b_stern_2_hi) && (a_2d >= b_stern_2_lo))
             )
            goto b_stern_ok;
        }
        #endif
        if (true) {
          # muss noch [a[2n-j],...,a[2n-2j]] um 1 erniedrigen:
          if ( dec_loop_down(&a_lptr[-2],j+1) ==0)
            goto b_stern_ok;
          # Subtraktion von b*^2 lieferte negativen Carry
          b_stern = b_stern-1; # b* := b* - 1
          # Workaround gcc-2.7.0 bug on i386 and gcc-2.5.8 bug on hppa.
            #if defined(__GNUC__) && (__GNUC__ == 2) && (__GNUC_MINOR__ <= 7)
              *&b_stern = *&b_stern;
            #endif
          # erhöhe [a[2n-j-1],...,a[2n-2j-2]] um [b[n],...,b[n-j],0] + 2 * b* + 1
          if ((sintD)b_stern < 0)
            b_ptr[-1] |= bit(0); # höchstes Bit von b* in b[n-j] ablegen
          b_ptr[0] = (uintD)(b_stern<<1)+1; # niedrige Bits von b* und eine 1 als b[n-j-1] ablegen
          addto_loop_down(&b_ptr[1],a_lptr,j+2);
          # (a[2n-j] wird nicht mehr gebraucht.)
          b_ptr[0] -= 1; # niedrige Bits von b* in b[n-j-1] ablegen
          b_ptr++;
        } else {
         b_stern_ok:
          # b* als b[n-j-1] ablegen:
          if ((sintD)b_stern < 0)
            b_ptr[-1] |= bit(0); # höchstes Bit von b* in b[n-j] ablegen
          b_ptr[0] = (uintD)(b_stern<<1); # niedrige Bits von b* als b[n-j-1] ablegen
          b_ptr++;
        }
      }
      # b_MSDptr = Pointer auf b[n], b_ptr = Pointer hinter b[0].
      # a_mptr = Pointer auf a[n].
      # Schiebe [b[n],...,b[0]] um s+1 Bits nach rechts:
      if (s == intDsize-1) {
        b_ptr--;
      } else {
        shiftright_loop_up(b_MSDptr,n+1,s+1);
        b_MSDptr++;
      }
      # b = b_MSDptr/n/b_ptr ist fertig, eine NUDS.
      end_arith_call();
      b_->MSDptr = b_MSDptr; b_->len = n; b_->LSDptr = b_ptr;
      # Teste, ob alle a[n],...,a[0]=0 sind. Ja -> Wurzel exakt
      var bool result = (test_loop_up(a_mptr,n+1) ? false : true);
      RESTORE_NUM_STACK # num_stack zurück
      return result;
    }
  }

# Zieht die Wurzel (ISQRT x) aus einem Integer.
# I_isqrt_I(x)
# > x: an Integer >=0
# < STACK_0: (isqrt x)
# < ergebnis: true falls x Quadratzahl, false sonst
# erniedrigt STACK um 1
# can trigger GC
  local maygc bool I_isqrt_I (object x)
  {
    var uintD* x_MSDptr;
    var uintC x_len;
    var uintD* x_LSDptr;
    I_to_NDS_nocopy(x, x_MSDptr=,x_len=,x_LSDptr=); # Digit sequence >=0 zu x
    {
      SAVE_NUM_STACK # num_stack retten
      var DS y;
      var bool squarep;
      UDS_sqrt(x_MSDptr,x_len,x_LSDptr, &y, squarep=); # Wurzel ziehen
      pushSTACK(NUDS_to_I(y.MSDptr,y.len)); # als Integer
      RESTORE_NUM_STACK # num_stack zurück
      return squarep;
    }
  }

# Stellt fest, ob ein Integer >=0 eine Quadratzahl ist.
# I_sqrtp(x)
# > x: ein Integer >=0
# < ergebnis: Integer (sqrt x) falls x Quadratzahl, nullobj sonst
# can trigger GC
  local maygc object I_sqrtp (object x);
# Methode:
# Damit x eine Quadratzahl ist, muss es ==0,1 mod 4 sein, und
# bei ISQRT muss ein Rest 0 herauskommen.
  local maygc object I_sqrtp (object x)
  {
    if (I_I_logbitp(Fixnum_1,x)) # Bit 1 von x gesetzt?
      return nullobj; # ja -> x==2,3 mod 4, also kein Quadrat
    var uintD* x_MSDptr;
    var uintC x_len;
    var uintD* x_LSDptr;
    I_to_NDS_nocopy(x, x_MSDptr=,x_len=,x_LSDptr=); # Digit sequence >=0 zu x
    {
      SAVE_NUM_STACK # num_stack retten
      var DS y;
      var bool squarep;
      UDS_sqrt(x_MSDptr,x_len,x_LSDptr, &y, squarep=); # Wurzel ziehen
      var object result = (squarep ? NUDS_to_I(y.MSDptr,y.len) : nullobj); # als Integer
      RESTORE_NUM_STACK # num_stack zurück
      return result;
    }
  }

# Stellt fest, ob ein Integer >=0 eine n-te Potenz ist.
# I_rootp(x,n)
# > x: ein Integer >=0
# > n: ein Integer >0
# < ergebnis: Integer (expt x (/ n)) falls x eine n-te Potenz, nullobj sonst
# can trigger GC
  local maygc object I_rootp (object x, object n1);
# Methode:
# Falls x=0 oder x=1: x = x^n -> JA, x als Ergebnis.
# Hier also x>1. Suche ein Integer y > 1 mit x=y^n.
# Falls n >= integer_length(x): NEIN. (Da y>=2, müsste x>=2^n gelten.)
# Hier also n>0 klein.
# Solange n gerade ist: x := (sqrt x), n := (/ n 2). x nicht ganz -> NEIN.
# Hier also n>0 ungerade.
# Falls n=1: x = x^n -> JA, x als Ergebnis.
# Falls o := ord2(x) nicht durch n teilbar ist: NEIN.
# Sonst dividiere x durch 2^o, am Schluss y mit 2^(o/n) multiplizieren.
# Hier also n>0 ungerade, x ungerade.
# beta := 2^intDsize, m := ceiling(integer_length(x)/(intDsize*n)).
# Suche ein y mit y>=0, y<beta^m mit  x == y^n mod beta^m :
#   Mit dem Hensel-Lemma. Das Polynom f(X) = X^n-x hat die Diskriminante
#   (-1)^((n-1)*n/2) * Res(X^n-x,n*X^(n-1)) = +- n^n * x^(n-1), und diese ist
#   nicht durch p=2 teilbar. Daher ist das Hensel-Lemma mit p=2 anwendbar.
#   Verwende quadratisches Hensel-Lifting, bei linearem Hensel-Lifting der
#   der Verwaltungsaufwand vergleichsweise größer ist und die schnelle
#   Multiplikation nicht zum Zuge kommt.
#   Sei  y0 mod beta^k  mit  y0^n == x mod beta^k  bekannt. k=m -> fertig.
#   Setze  y == y0 + beta^k*y1 mod beta^2k  an, wobei 2k := min(2*k,m).
#   Dabei wird y1 mod beta^(2k-k) so gewählt, dass mod beta^2k
#   x == y^n == y0^n + n * y0^(n-1) * beta^k*y1. Dazu wird
#   (x - y0^n) mod beta^2k errechnet, durch beta^k dividiert (die Division
#   muss nach Voraussetzung an y0 aufgehen) und
#   y1 := ((x-y0^n)/beta^k) / (n*y0^(n-1)) mod beta^(2k-k) gebildet.
#   Damit hat man  (y0 + beta^k*y1)^n == x mod beta^2k . 2k=m -> fertig.
#   Den Anfang (k=1) bekommt man analog, mit beta:=2 und k=1,k=2,k=4,...
# Dann testet man, ob wirklich x = y^n, und ist fertig.
  local maygc object I_rootp (object x, object n1)
  {
    if (eq(x,Fixnum_0) || eq(x,Fixnum_1)) # x=0 oder x=1 ?
      return x; # ja -> x als Ergebnis
    # Nun ist x>1.
    pushSTACK(x); pushSTACK(n1);
    {
      var object l = I_integer_length_I(x); # (integer-length x)
      if (I_I_comp(STACK_0,l) >= 0) { # n >= (integer-length x) ?
        skipSTACK(2); return nullobj;
      }
    }
    # Nun ist n < (integer-length x). Also passt n in ein uintL.
    var uintL n = I_to_UL(popSTACK());
    var object x = popSTACK();
    while ((n % 2) == 0) { # n gerade?
      x = I_sqrtp(x); # Quadratwurzel ziehen versuchen
      if (eq(x,nullobj))
        return nullobj; # nicht ganzzahlig -> fertig
      n = n >> 1; # n := (/ n 2)
    }
    # Nun ist n ungerade.
    if (n==1)
      return x; # n=1 -> x als Ergebnis
    var uintL oq = 0; # Shift von y am Schluss
    {
      var uintL o = I_ord2(x);
      if (!(o==0)) {
        var uintL o_r; divu_3232_3232(o,n, oq=,o_r=); # o_r = o mod n
        if (!(o_r==0))
          return nullobj; # o nicht durch n teilbar -> fertig
        # oq = o/n.
        # dividiere x durch 2^o:
        var object temp;
        pushSTACK(x); temp = L_to_I(-o); x = I_I_ash_I(popSTACK(),temp);
      }
    }
    # Nun ist n ungerade, x ungerade.
    pushSTACK(x); # x retten
    {
      SAVE_NUM_STACK # num_stack retten
      var uintC n_len;
      var uintD* n_LSDptr;
      # UDS zu n bilden, 0<n_len<=32/intDsize:
      var uintD n_UDS[32/intDsize];
      {
        var uintD* n_MSDptr = &n_UDS[0];
        set_32_Dptr(n_MSDptr,n); n_LSDptr = &n_UDS[32/intDsize];
        n_len = 32/intDsize; # und (zwecks Effizienz) normieren:
        doconsttimes(32/intDsize-1, {
          if (!(*n_MSDptr++ == 0))
            goto n_UDS_ok;
          n_len--;
        });
        n_UDS_ok: ; # n_MSDptr/n_len/n_LSDptr ist NUDS zu n.
      }
      var uintC x_len;
      var uintD* x_LSDptr;
      I_to_NDS_nocopy(x, _EMA_,x_len=,x_LSDptr=); # UDS zu x bilden, x_len>0
      var uintD x_lsd = x_LSDptr[-1]; # letztes Digit von x
      var uintD y_lsd; # n-te Wurzel von x_lsd mod 2^intDsize
      y_lsd = 1; # Wurzel mod 2^1
      # Für k=1,2,4,...:
      # y_lsd := y_lsd + 2^k * (x_lsd-y_lsd^n)/2^k / (n*y_lsd^(n-1))
      #        = y_lsd + (x_lsd-y_lsd^n) / (n*y_lsd^(n-1))
      doconsttimes(log2_intDsize, { # log2(intDsize) Iterationen reichen aus
        var uintD y_lsd_n1 = D_UL_expt_D(y_lsd,n-1); # y_lsd^(n-1)
        var uintD y_lsd_n = D_D_mal2adic_D(y_lsd_n1,y_lsd); # y_lsd^n
        var uintD delta = x_lsd-y_lsd_n; # x_lsd - y_lsd^n
        if (delta==0)
          goto y_lsd_ok;
        y_lsd = y_lsd + D_D_durch2adic_D(delta,D_D_mal2adic_D((uintD)n,y_lsd_n1));
      });
     y_lsd_ok:
      ASSERT(D_UL_expt_D(y_lsd,n)==x_lsd);
      # Nun ist y_lsd^n == x_lsd mod beta=2^intDsize.
      var uintL m = ceiling((uintL)x_len,n); # für y nötige Länge, >0, <=x_len
      var uintD* y_LSDptr;
      {
        var uintD* z1_LSDptr;
        var uintD* z2_LSDptr;
        var uintD* z3_LSDptr;
        num_stack_need_1(m, _EMA_,y_LSDptr=); # Platz für y
        {
          var uintL need = 2*m+(32/intDsize-1); # >= max(2*m,m+32/intDsize)
          num_stack_need(need, _EMA_,z1_LSDptr=); # Platz für Rechenregister 1
          num_stack_need(need, _EMA_,z2_LSDptr=); # Platz für Rechenregister 2
          num_stack_need(need, _EMA_,z3_LSDptr=); # Platz für Rechenregister 3
        }
        begin_arith_call();
        var uintL k = 1; # y ist bisher mod beta^k bekannt
        y_LSDptr[-1] = y_lsd; # Startwert von y
        until (k==m) {
          var uintL k2 = 2*k; if (k2>m) { k2=m; } # k2 = min(2*k,m) > k
          # bisheriges y mod beta^k2 mit n-1 potenzieren:
          # Methode für z := y^(n-1) :
          #   zz:=y, e:=n-1.
          #   Solange e gerade, setze zz:=zz*zz, e:=e/2.
          #   z:=zz.
          #   Solange (e:=floor(e/2)) >0,
          #     setze zz:=zz*zz, und falls e ungerade, setze z:=z*zz.
          var uintL e = n-1; # e:=n-1
          var uintD* free_LSDptr = z1_LSDptr;
          var uintD* zz_LSDptr = z2_LSDptr;
          var uintD* z_LSDptr;
          # Ab jetzt {zz_LSDptr,free_LSDptr} = {z1_LSDptr,z2_LSDptr}.
          clear_loop_down(&y_LSDptr[-(uintP)k],k2-k); # y auf k2 Digits erweitern
          copy_loop_down(y_LSDptr,zz_LSDptr,k2); # zz:=y
          do {
            var uintD* new_zz_LSDptr = free_LSDptr;
            mulu_2loop_down(zz_LSDptr,k2,zz_LSDptr,k2,new_zz_LSDptr); # zz:=zz*zz
            free_LSDptr = zz_LSDptr; zz_LSDptr = new_zz_LSDptr;
            e = e>>1; # e:=e/2
          } while ((e & bit(0)) ==0); # solange e gerade
          z_LSDptr = zz_LSDptr; # z:=zz
          # (zz nicht kopieren; ab der nächsten Veränderung von zz wird
          # {zz_LSDptr,z_LSDptr,free_LSDptr} = {z1_LSDptr,z2_LSDptr,z3_LSDptr}
          # gelten.)
          until ((e = e>>1) == 0) {
            {
              var uintD* new_zz_LSDptr = free_LSDptr;
              mulu_2loop_down(zz_LSDptr,k2,zz_LSDptr,k2,new_zz_LSDptr); # zz:=zz*zz
              free_LSDptr = (z_LSDptr==zz_LSDptr ? z3_LSDptr : zz_LSDptr);
              zz_LSDptr = new_zz_LSDptr;
            }
            if (!((e & bit(0)) == 0)) {
              var uintD* new_z_LSDptr = free_LSDptr;
              mulu_2loop_down(z_LSDptr,k2,zz_LSDptr,k2,new_z_LSDptr); # z:=z*zz
              free_LSDptr = z_LSDptr; z_LSDptr = new_z_LSDptr;
            }
          }
          # z = y^(n-1) mod beta^k2 ist fertig.
          if (z_LSDptr==zz_LSDptr)
            zz_LSDptr = z3_LSDptr; # zz ist jetzt auch frei
          mulu_2loop_down(z_LSDptr,k2,y_LSDptr,k2,free_LSDptr); # y^n
          sub_loop_down(x_LSDptr,free_LSDptr,zz_LSDptr,k2); # zz:=x-y^n
          ASSERT(!test_loop_up(&zz_LSDptr[-(uintP)k],k)); # zz == 0 mod beta^k
          mulu_2loop_down(z_LSDptr,k2-k,n_LSDptr,n_len,free_LSDptr); # n*y^(n-1)
          # Quotienten mod beta^(k2-k) bilden und an y mod beta^k ankleben:
          UDS_UDS_durch2adic_UDS(k2-k,&zz_LSDptr[-(uintP)k],free_LSDptr,&y_LSDptr[-(uintP)k]);
          k = k2; # jetzt gilt y^n == x sogar mod beta^k2.
        }
        end_arith_call();
      }
      # y mit y^n == x mod beta^m ist gefunden.
      var object y = UDS_to_I(&y_LSDptr[-(uintP)m],m); # y als Integer >=0
      RESTORE_NUM_STACK # num_stack zurück
      pushSTACK(y);
    # Stackaufbau: x, y.
    # y^n (mit n ungerade) bilden:
    #   c:=a:=y, b:=n.
    #   Solange b:=floor(b/2) >0 ist,
    #     setze a:=a*a, und falls b ungerade, setze c:=a*c.
    #   Liefere c.
      pushSTACK(y);
      pushSTACK(y);
    } # Stackaufbau: x, y, c, a.
    until ((n = n>>1) == 0) {
      var object a = STACK_0 = I_square_I(STACK_0);
      if (!((n & bit(0)) == 0))
        STACK_1 = I_I_mal_I(a,STACK_1);
    }
    {
      var object temp = STACK_1; skipSTACK(2); # temp = y^n
      # mit x vergleichen:
      if (!(I_I_comp(STACK_1,temp)==0)) {
        # Die ganze Rechnung war umsonst.
        skipSTACK(2); return nullobj;
      }
    }
    # y ist tatsächlich n-te Wurzel von x.
    # Noch mit 2^oq multiplizieren:
    if (oq==0) { # kein Shift nötig?
      var object temp = STACK_0;
      skipSTACK(2); return temp;
    } else {
      var object temp = UL_to_I(oq);
      temp = I_I_ash_I(STACK_0,temp);
      skipSTACK(2); return temp;
    }
  }

