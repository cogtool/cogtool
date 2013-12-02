# Division ganzer Zahlen

# Dividiert zwei Unsigned Digit sequences durcheinander.
# UDS_divide(a_MSDptr,a_len,a_LSDptr, b_MSDptr,b_len,b_LSDptr, &q,&r);
# Die UDS a = a_MSDptr/a_len/a_LSDptr (a>=0) wird durch
# die UDS b = b_MSDptr/b_len/b_LSDptr (b>=0) dividiert:
# a = q * b + r mit 0 <= r < b. Bei b=0 Error.
# q der Quotient, r der Rest.
# q = q_MSDptr/q_len/q_LSDptr, r = r_MSDptr/r_len/r_LSDptr beides
# Normalized Unsigned Digit sequences.
# Vorsicht: q_LSDptr <= r_MSDptr,
#           Vorzeichenerweiterung von r kann q zerstören!
#           Vorzeichenerweiterung von q ist erlaubt.
# a und b werden nicht modifiziert.
# num_stack wird erniedrigt.
  #define UDS_divide(a_MSDptr,a_len,a_LSDptr,b_MSDptr,b_len,b_LSDptr,q_,r_)  \
    { # Platz fürs Ergebnis machen. Brauche maximal a_len+1 Digits.                \
      var uintC _a_len = (a_len);                                                  \
      var uintD* roomptr; num_stack_need_1(_a_len+1,roomptr=,);                    \
      UDS_divide_(a_MSDptr,_a_len,a_LSDptr,b_MSDptr,b_len,b_LSDptr,roomptr,q_,r_); \
    }
  local void UDS_divide_ (uintD* a_MSDptr, uintC a_len, uintD* a_LSDptr,
                          uintD* b_MSDptr, uintC b_len, uintD* b_LSDptr,
                          uintD* roomptr, DS* q_, DS* r_);
# Methode:
# erst a und b normalisieren: a=[a[m-1],...,a[0]], b=[b[n-1],...,b[0]]
# mit m>=0 und n>0 (Stellensystem der Basis beta=2^intDsize).
# Falls m<n, ist q:=0 und r:=a.
# Falls m>=n=1, Single-Precision-Division:
#   r:=0, j:=m,
#   while j>0 do
#     {Hier (q[m-1]*beta^(m-1)+...+q[j]*beta^j) * b[0] + r*beta^j =
#           = a[m-1]*beta^(m-1)+...+a[j]*beta^j und 0<=r<b[0]<beta}
#     j:=j-1, r:=r*beta+a[j], q[j]:=floor(r/b[0]), r:=r-b[0]*q[j].
#   Normalisiere [q[m-1],...,q[0]], liefert q.
# Falls m>=n>1, Multiple-Precision-Division:
#   Es gilt a/b < beta^(m-n+1).
#   s:=intDsize-1-(Nummer des höchsten Bits in b[n-1]), 0<=s<intDsize.
#   Schiebe a und b um s Bits nach links und kopiere sie dabei. r:=a.
#   r=[r[m],...,r[0]], b=[b[n-1],...,b[0]] mit b[n-1]>=beta/2.
#   Für j=m-n,...,0: {Hier 0 <= r < b*beta^(j+1).}
#     Berechne q* :
#       q* := floor((r[j+n]*beta+r[j+n-1])/b[n-1]).
#       Bei Überlauf (q* >= beta) setze q* := beta-1.
#       Berechne c2 := ((r[j+n]*beta+r[j+n-1]) - q* * b[n-1])*beta + r[j+n-2]
#       und c3 := b[n-2] * q*.
#       {Es ist 0 <= c2 < 2*beta^2, sogar 0 <= c2 < beta^2 falls kein
#        Überlauf aufgetreten war. Ferner 0 <= c3 < beta^2.
#        Bei Überlauf und r[j+n]*beta+r[j+n-1] - q* * b[n-1] >= beta,
#        das heißt c2 >= beta^2, kann man die nächste Abfrage überspringen.}
#       Solange c3 > c2, {hier 0 <= c2 < c3 < beta^2} setze
#         q* := q* - 1, c2 := c2 + b[n-1]*beta, c3 := c3 - b[n-2].
#       Falls q* > 0:
#         Setze r := r - b * q* * beta^j, im einzelnen:
#           [r[n+j],...,r[j]] := [r[n+j],...,r[j]] - q* * [b[n-1],...,b[0]].
#           also: u:=0, for i:=0 to n-1 do
#                         u := u + q* * b[i],
#                         r[j+i]:=r[j+i]-(u mod beta) (+ beta, falls Carry),
#                         u:=u div beta (+ 1, falls bei der Subtraktion Carry)
#                 r[n+j]:=r[n+j]-u.
#           {Da stets u = (q* * [b[i-1],...,b[0]] div beta^i) + 1
#                       < q* + 1 <= beta, läuft der Übertrag u nicht über.}
#         Tritt dabei ein negativer Übertrag auf, so setze q* := q* - 1
#           und [r[n+j],...,r[j]] := [r[n+j],...,r[j]] + [0,b[n-1],...,b[0]].
#     Setze q[j] := q*.
#   Normalisiere [q[m-n],..,q[0]] und erhalte den Quotienten q,
#   Schiebe [r[n-1],...,r[0]] um s Bits nach rechts, normalisiere und
#   erhalte den Rest r.
#   Dabei kann q[j] auf dem Platz von r[n+j] liegen.
  local void UDS_divide_ (uintD* a_MSDptr, uintC a_len, uintD* a_LSDptr,
                          uintD* b_MSDptr, uintC b_len, uintD* b_LSDptr,
                          uintD* roomptr, # ab roomptr kommen a_len+1 freie Digits
                          DS* q_, DS* r_)
  {
    # a normalisieren (a_MSDptr erhöhen, a_len erniedrigen):
    while ((a_len>0) && (a_MSDptr[0]==0)) {
      a_MSDptr++; a_len--;
    }
    # b normalisieren (b_MSDptr erhöhen, b_len erniedrigen):
    loop {
      if (b_len==0)
        divide_0();
      if (b_MSDptr[0]==0) {
        b_MSDptr++; b_len--;
      } else
        break;
    }
    # jetzt m=a_len >=0 und n=b_len >0.
    if (a_len < b_len) {
      # m<n: Trivialfall, q=0, r:= Kopie von a.
      var uintD* r_MSDptr = roomptr;
      var uintD* r_LSDptr = &roomptr[a_len];
      # Speicheraufbau: r_MSDptr/0/r_MSDptr/a_len/r_LSDptr
      #                    |     q    |       r      |
      copy_loop_down(a_LSDptr,r_LSDptr,a_len);
      q_->MSDptr = r_MSDptr; q_->len = 0; q_->LSDptr = r_MSDptr; # q = 0, eine NUDS
      r_->MSDptr = r_MSDptr; r_->len = a_len; r_->LSDptr = r_LSDptr; # r = Kopie von a, eine NUDS
      return;
    } elif (b_len==1) {
      # n=1: Single-Precision-Division
      # beta^(m-1) <= a < beta^m  ==>  beta^(m-2) <= a/b < beta^m
      var uintD* q_MSDptr = roomptr;
      var uintD* q_LSDptr = &roomptr[a_len];
      var uintD* r_MSDptr = q_LSDptr;
      var uintD* r_LSDptr = &r_MSDptr[1];
      # Speicheraufbau: q_MSDptr/a_len/q_LSDptr    r_MSDptr/1/r_LSDptr
      #                     |      q      |           |     r    |
      var uintD rest = divucopy_loop_up(b_MSDptr[0],a_MSDptr,q_MSDptr,a_len); # Division durch b[0]
      var uintC r_len;
      if (!(rest==0)) {
        r_MSDptr[0] = rest; r_len=1; # Rest als r ablegen
      } else {
        r_MSDptr = r_LSDptr; r_len=0; # Rest auf 0 normalisieren
      }
      if (q_MSDptr[0]==0) {
        q_MSDptr++; a_len--; # q normalisieren
      }
      q_->MSDptr = q_MSDptr; q_->len = a_len; q_->LSDptr = q_LSDptr; # q ablegen
      r_->MSDptr = r_MSDptr; r_->len = r_len; r_->LSDptr = r_LSDptr; # r ablegen
      return;
    } else {
      # n>1: Multiple-Precision-Division
      # beta^(m-1) <= a < beta^m, beta^(n-1) <= b < beta^n  ==>
      # beta^(m-n-1) <= a/b < beta^(m-n+1).
      var uintL s;
      SAVE_NUM_STACK # num_stack retten
      # s bestimmen:
      {
        var uintD msd = b_MSDptr[0]; # b[n-1]
        #if 0
        s = 0;
        while ((sintD)msd >= 0) {
          msd = msd<<1; s++;
        }
        #else # ein wenig effizienter, Abfrage auf s=0 vorwegnehmen
        if ((sintD)msd < 0) {
          s = 0; goto shift_ok;
        } else {
          integerlengthD(msd, s = intDsize - ); goto shift;
        }
        #endif
      }
      # 0 <= s < intDsize.
      # Kopiere b und schiebe es dabei um s Bits nach links:
      if (!(s==0))
      shift: {
        var uintD* old_b_LSDptr = b_LSDptr;
        num_stack_need(b_len,b_MSDptr=,b_LSDptr=);
        shiftleftcopy_loop_down(old_b_LSDptr,b_LSDptr,b_len,s);
      }
     shift_ok: {
      # Wieder b = b_MSDptr/b_len/b_LSDptr.
      # Kopiere a und schiebe es dabei um s Bits nach links, erhalte r:
      var uintD* r_MSDptr = roomptr;
      var uintD* r_LSDptr = &roomptr[a_len+1];
      # Speicheraufbau:  r_MSDptr/          a_len+1           /r_LSDptr
      #                     |                  r                  |
      # später:          q_MSDptr/a_len-b_len+1/r_MSDptr/b_len/r_LSDptr
      #                     |           q          |       r      |
      if (s==0) {
        copy_loop_down(a_LSDptr,r_LSDptr,a_len); r_MSDptr[0] = 0;
      } else {
        r_MSDptr[0] = shiftleftcopy_loop_down(a_LSDptr,r_LSDptr,a_len,s);
      }
      # Nun r = r_MSDptr/a_len+1/r_LSDptr.
      var uintC j = a_len-b_len; # m-n
      var uintD* r_ptr = &r_LSDptr[-(uintP)j]; # Pointer oberhalb von r[j]
      var uintD* q_MSDptr = r_MSDptr;
      var uintC q_len = j = j+1; # q wird m-n+1 Digits haben
      var uintD b_msd = b_MSDptr[0]; # b[n-1]
      var uintD b_2msd = b_MSDptr[1]; # b[n-2]
      #if HAVE_DD
      var uintDD b_msdd = highlowDD(b_msd,b_2msd); # b[n-1]*beta+b[n-2]
      #endif
      # Divisions-Schleife: (wird m-n+1 mal durchlaufen)
      # j = Herabzähler, b_MSDptr/b_len/b_LSDptr = [b[n-1],...,b[0]], b_len=n,
      # r_MSDptr = Pointer auf r[n+j] = Pointer auf q[j],
      # r_ptr = Pointer oberhalb von r[j].
      do {
        var uintD q_stern;
        var uintD c1;
        if (r_MSDptr[0] < b_msd) { # r[j+n] < b[n-1] ?
          # Dividiere r[j+n]*beta+r[j+n-1] durch b[n-1], ohne Überlauf:
          #if HAVE_DD
            divuD(highlowDD(r_MSDptr[0],r_MSDptr[1]),b_msd, q_stern=,c1=);
          #else
            divuD(r_MSDptr[0],r_MSDptr[1],b_msd, q_stern=,c1=);
          #endif
        } else {
          # Überlauf, also r[j+n]*beta+r[j+n-1] >= beta*b[n-1]
          q_stern = bitm(intDsize)-1; # q* = beta-1
          # Teste ob r[j+n]*beta+r[j+n-1] - (beta-1)*b[n-1] >= beta
          # <==> r[j+n]*beta+r[j+n-1] + b[n-1] >= beta*b[n-1]+beta
          # <==> b[n-1] < floor((r[j+n]*beta+r[j+n-1]+b[n-1])/beta) {<= beta !} ist.
          # Wenn ja, direkt zur Subtraktionschleife.
          # (Andernfalls ist r[j+n]*beta+r[j+n-1] - (beta-1)*b[n-1] < beta
          #  <==> floor((r[j+n]*beta+r[j+n-1]+b[n-1])/beta) = b[n-1] ).
          if ((r_MSDptr[0] > b_msd) || ((c1 = r_MSDptr[1]+b_msd) < b_msd)) {
            # r[j+n] >= b[n-1]+1 oder
            # r[j+n] = b[n-1] und Addition r[j+n-1]+b[n-1] gibt Carry ?
            goto subtract; # ja -> direkt in die Subtraktion
          }
        }
        # q_stern = q*,
        # c1 = (r[j+n]*beta+r[j+n-1]) - q* * b[n-1] (>=0, <beta).
        #if HAVE_DD
          {
            var uintDD c2 = highlowDD(c1,r_MSDptr[2]); # c1*beta+r[j+n-2]
            var uintDD c3 = muluD(b_2msd,q_stern); # b[n-2] * q*
            # Solange c2 < c3, c2 erhöhen, c3 erniedrigen:
            # Rechne dabei mit c3-c2:
            # solange >0, um b[n-1]*beta+b[n-2] erniedrigen.
            # Dies kann wegen b[n-1]*beta+b[n-2] >= beta^2/2
            # höchstens zwei mal auftreten.
            if (c3 > c2) {
              q_stern = q_stern-1; # q* := q* - 1
              if (c3-c2 > b_msdd)
                q_stern = q_stern-1; # q* := q* - 1
            }
          }
        #else
          # Wie oben, nur mit zweigeteilten c2=[c2hi|c2lo] und c3=[c3hi|c3lo]:
          #define c2hi c1
          {
            var uintD c2lo = r_MSDptr[2]; # c2hi*beta+c2lo = c1*beta+r[j+n-2]
            var uintD c3hi;
            var uintD c3lo;
            muluD(b_2msd,q_stern, c3hi=,c3lo=); # c3hi*beta+c3lo = b[n-2] * q*
            if ((c3hi > c2hi) || ((c3hi == c2hi) && (c3lo > c2lo))) {
              q_stern = q_stern-1; # q* := q* - 1
              c3hi -= c2hi; if (c3lo < c2lo) c3hi--; c3lo -= c2lo; # c3 := c3-c2
              if ((c3hi > b_msd) || ((c3hi == b_msd) && (c3lo > b_2msd)))
                q_stern = q_stern-1; # q* := q* - 1
            }
          }
          #undef c2hi
        #endif
        if (!(q_stern==0))
         subtract:
          {
            # Subtraktionsschleife: r := r - b * q* * beta^j
            var uintD carry = mulusub_loop_down(q_stern,b_LSDptr,r_ptr,b_len);
            # Noch r_ptr[-b_len-1] -= carry, d.h. r_MSDptr[0] -= carry
            # durchführen und danach r_MSDptr[0] vergessen:
            if (carry > r_MSDptr[0]) {
              # Subtraktion ergab Übertrag
              q_stern = q_stern-1; # q* := q* - 1
              addto_loop_down(b_LSDptr,r_ptr,b_len); # Additionsschleife
              # r[n+j] samt Carry kann vergessen werden...
            }
          }
        # Berechnung von q* ist fertig.
        *r_MSDptr++ = q_stern; # als q[j] ablegen
        r_ptr++;
      } until (--j == 0);
      # Nun ist q = [q[m-n],..,q[0]] = q_MSDptr/q_len/r_MSDptr
      # und r = [r[n-1],...,r[0]] = r_MSDptr/b_len/r_LSDptr.
      # q normalisieren und ablegen:
      if (q_MSDptr[0]==0) {
        q_MSDptr++; q_len--;
      }
      q_->MSDptr = q_MSDptr; q_->len = q_len; q_->LSDptr = r_MSDptr;
      # Schiebe [r[n-1],...,r[0]] um s Bits nach rechts:
      if (!(s==0)) {
        shiftright_loop_up(r_MSDptr,b_len,s);
      }
      # r normalisieren und ablegen:
      while ((b_len>0) && (r_MSDptr[0]==0)) {
        r_MSDptr++; b_len--;
      }
      r_->MSDptr = r_MSDptr; r_->len = b_len; r_->LSDptr = r_LSDptr;
      RESTORE_NUM_STACK # num_stack zurück
      return;
    }}
  }

# Dividiert zwei Integers x,y >=0 und liefert Quotient und Rest
# der Division x/y. Bei y=0 Error.
# I_I_divide_I_I(x,y);
# > x,y: Integers >=0
# < STACK_1: Quotient q
# < STACK_0: Rest r
# Erniedrigt STACK um 2
# can trigger GC
  local maygc void I_I_divide_I_I (object x, object y)
  {
    if (I_fixnump(x)) {
      # x Fixnum >=0
      if (I_fixnump(y)) {
        # auch y Fixnum >=0
        var uintV x_ = posfixnum_to_V(x);
        var uintV y_ = posfixnum_to_V(y);
        if (y_==0) {
          divide_0();
        } elif (x_ < y_) {
          # Trivialfall: q=0, r=x
          goto trivial;
        } else {
         #if (intVsize>32)
          if (x_ >= vbit(32)) {
            if (y_ < vbit(32)) {
              # 64-durch-32-Bit-Division
              var uint64 q;
              var uint32 r;
              divu_6432_6432(x_,y_,q=,r=);
              pushSTACK(UQ_to_I(q));
              pushSTACK(UL_to_I(r));
            } else {
              # volle 64-durch-64-Bit-Division
              var uint64 q;
              var uint64 r;
              divu_6464_6464(x_,y_,q=,r=);
              pushSTACK(UQ_to_I(q));
              pushSTACK(UQ_to_I(r));
            }
          } else
         #endif
          {
            if (y_ < bit(16)) {
              # 32-durch-16-Bit-Division
              var uint32 q;
              var uint16 r;
              divu_3216_3216(x_,y_,q=,r=);
              pushSTACK(UL_to_I(q));
              pushSTACK(fixnum((uintL)r));
            } else {
              # volle 32-durch-32-Bit-Division
              var uint32 q;
              var uint32 r;
              divu_3232_3232(x_,y_,q=,r=);
              pushSTACK(UL_to_I(q));
              pushSTACK(UL_to_I(r));
            }
          }
        }
      } else {
        # y Bignum >0
       trivial:
        # Trivialfall: q=0, r=x
        pushSTACK(Fixnum_0);
        pushSTACK(x);
      }
    } else {
      # x Bignum -> allgemeine Division:
      var uintD* x_MSDptr;
      var uintC x_len;
      var uintD* x_LSDptr;
      var uintD* y_MSDptr;
      var uintC y_len;
      var uintD* y_LSDptr;
      # x in NDS umwandeln, als UDS auffassen:
      BN_to_NDS_nocopy(x, x_MSDptr=,x_len=,x_LSDptr=);
      {
        # y in NDS umwandeln, als UDS auffassen:
        I_to_NDS_nocopy(y, y_MSDptr=,y_len=,y_LSDptr=);
        # dividieren:
        {
          SAVE_NUM_STACK # num_stack retten
          var DS q;
          var DS r;
          begin_arith_call();
          UDS_divide(x_MSDptr,x_len,x_LSDptr,y_MSDptr,y_len,y_LSDptr, &q,&r);
          end_arith_call();
          # q in Integer umwandeln:
          pushSTACK(NUDS_to_I(q.MSDptr,q.len));
          # r in Integer umwandeln (jetzt erst, nachdem q verwertet ist!):
          pushSTACK(NUDS_to_I(r.MSDptr,r.len));
          RESTORE_NUM_STACK # num_stack zurück
        }
      }
    }
  }

# Fehler, wenn Quotient keine ganze Zahl ist
# > STACK_1: Zähler x
# > STACK_0: Nenner y
  nonreturning_function(local, fehler_exquo, (void)) {
    pushSTACK(S(exquo)); # Wert für Slot OPERATION von ARITHMETIC-ERROR
    pushSTACK(STACK_(1+1)); pushSTACK(STACK_(0+2));
    { var object tmp = listof(2); pushSTACK(tmp); } # Wert für Slot OPERANDS von ARITHMETIC-ERROR
    pushSTACK(STACK_(1+2)); # x
    pushSTACK(STACK_(0+3)); # y
    fehler(arithmetic_error,
           GETTEXT("quotient ~S / ~S is not an integer")
          );
  }

# Dividiert zwei Integers x,y >=0 und liefert den Quotienten x/y >=0.
# Bei y=0 Error. Die Division muss aufgehen, sonst Error.
# I_I_exquopos_I(x,y)
# > x,y: Integers >=0
# < ergebnis: Quotient x/y, ein Integer >=0
# can trigger GC
  local object I_I_exquopos_I (object x, object y);
# Methode:
# (exquopos x y) :==
# (DIVIDE x y) -> q,r
# Falls r<>0, Error.
# Liefere q.
  local maygc object I_I_exquopos_I (object x, object y)
  {
    pushSTACK(y);
    pushSTACK(x);
    # Stackaufbau: y, x.
    I_I_divide_I_I(x,y); # q,r auf den Stack
    # Stackaufbau: y, x, q, r.
    if (!eq(STACK_0,Fixnum_0)) {
      skipSTACK(2); fehler_exquo();
    }
    var object q = STACK_1;
    skipSTACK(4); return q;
  }

# Dividiert zwei Integers x,y und liefert den Quotienten x/y.
# Bei y=0 Error. Die Division muss aufgehen, sonst Error.
# I_I_exquo_I(x,y)
# > x,y: Integers
# < ergebnis: Quotient x/y, ein Integer
# can trigger GC
  local object I_I_exquo_I (object x, object y);
# Methode:
# (exquo x y) :==
# (DIVIDE (abs x) (abs y)) -> q,r
# Falls r<>0, Error.
# Falls x,y verschiedene Vorzeichen haben, liefere -q, sonst q.
  local maygc object I_I_exquo_I (object x, object y)
  {
    pushSTACK(y);
    pushSTACK(x);
    pushSTACK(I_abs_I(y));
    # Stackaufbau: y, x, (abs y).
    x = I_abs_I(STACK_1); # (abs x)
    I_I_divide_I_I(x,STACK_0); # q,r auf den Stack
    # Stackaufbau: y, x, (abs y), q, r.
    if (!eq(STACK_0,Fixnum_0)) {
      skipSTACK(3); fehler_exquo();
    }
    var object q = STACK_1;
    if (!same_sign_p(STACK_3,STACK_4)) {
      # x,y haben verschiedene Vorzeichen
      skipSTACK(5); return I_minus_I(q);
    } else {
      # x,y haben gleiche Vorzeichen
      skipSTACK(5); return q;
    }
  }

# I_I_mod_I(x,y) = (mod x y), wo x,y Integers sind.
# can trigger GC
  local object I_I_mod_I (object x, object y);
# Methode:
# (mod x y) :==
# (DIVIDE (abs x) (abs y)) -> q,r
# Falls r=0, liefere 0.
# Falls x,y verschiedene Vorzeichen haben, setze r:=r-abs(y).
# Falls x<0, setze r:=-r.
# Liefere r.
  local maygc object I_I_mod_I (object x, object y)
  {
    pushSTACK(y);
    pushSTACK(x);
    pushSTACK(I_abs_I(y));
    # Stackaufbau: y, x, (abs y).
    x = I_abs_I(STACK_1); # (abs x)
    I_I_divide_I_I(x,STACK_0); # q,r auf den Stack
    # Stackaufbau: y, x, (abs y), q, r.
    var object r = STACK_0;
    if (!eq(r,Fixnum_0)) {
      if (!same_sign_p(STACK_3,STACK_4)) {
        # x,y haben verschiedene Vorzeichen
        r = I_I_minus_I(r,STACK_2); # r := (- r (abs y))
      }
      if (R_minusp(STACK_3))
        r = I_minus_I(r); # x<0 -> r := (- r)
    }
    skipSTACK(5); return r;
  }

# I_I_rem_I(x,y) = (rem x y), wo x,y Integers sind.
# can trigger GC
  local object I_I_rem_I (object x, object y);
# Methode:
# (rem x y) :==
# (DIVIDE (abs x) (abs y)) -> q,r
# Falls x<0, setze r:=-r.
# Liefere r.
  local maygc object I_I_rem_I (object x, object y)
  {
    pushSTACK(y);
    pushSTACK(x);
    pushSTACK(I_abs_I(y));
    # Stackaufbau: y, x, (abs y).
    x = I_abs_I(STACK_1); # (abs x)
    I_I_divide_I_I(x,STACK_0); # q,r auf den Stack
    # Stackaufbau: y, x, (abs y), q, r.
    var object r = STACK_0;
    if (!eq(r,Fixnum_0)) {
      if (R_minusp(STACK_3))
        r = I_minus_I(r); # x<0 -> r := (- r)
    }
    skipSTACK(5); return r;
  }

# Dividiert zwei Integers x,y und liefert Quotient und Rest
# (q,r) := (floor x y)
# I_I_floor_I_I(x,y);
# > x,y: Integers
# < STACK_1: Quotient q
# < STACK_0: Rest r
# Erniedrigt STACK um 2
# can trigger GC
  local void I_I_floor_I_I (object x, object y);
# Methode:
# (floor x y) :==
# (DIVIDE (abs x) (abs y)) -> q,r
# Falls x,y verschiedene Vorzeichen haben und r<>0,
#   setze q:=q+1 und r:=r-abs(y).
# Falls x<0, setze r:=-r.
# Falls x,y verschiedene Vorzeichen haben, setze q:=-q.
# Liefere q,r.
  local maygc void I_I_floor_I_I (object x, object y)
  {
    pushSTACK(y);
    pushSTACK(x);
    pushSTACK(I_abs_I(y));
    # Stackaufbau: y, x, (abs y).
    x = I_abs_I(STACK_1); # (abs x)
    I_I_divide_I_I(x,STACK_0); # q,r auf den Stack
    # Stackaufbau: y, x, (abs y), q, r.
    if (!same_sign_p(STACK_3,STACK_4))
      # x,y haben verschiedene Vorzeichen
      if (!eq(STACK_0,Fixnum_0)) {
        # r/=0, also r>0.
        STACK_1 = I_1_plus_I(STACK_1); # q := (1+ q)
        STACK_0 = I_I_minus_I(STACK_0,STACK_2); # r := (- r (abs y))
      }
    if (R_minusp(STACK_3)) {
      # x<0
      STACK_0 = I_minus_I(STACK_0); # r := (- r)
      if (!R_minusp(STACK_4)) # y>=0 ?
        goto negate_q; # q := (- q)
    } else {
      # x>=0
      if (R_minusp(STACK_4)) # y<0 ?
        negate_q: { STACK_1 = I_minus_I(STACK_1); } # q := (- q)
    }
    STACK_4 = STACK_1; STACK_3 = STACK_0; skipSTACK(3); # Stack aufräumen
  }

# Dividiert zwei Integers x,y und liefert Quotient und Rest
# (q,r) := (ceiling x y)
# I_I_ceiling_I_I(x,y);
# > x,y: Integers
# < STACK_1: Quotient q
# < STACK_0: Rest r
# Erniedrigt STACK um 2
# can trigger GC
  local void I_I_ceiling_I_I (object x, object y);
# Methode:
# (ceiling x y) :==
# (DIVIDE (abs x) (abs y)) -> q,r
# Falls x,y selbes Vorzeichen haben und r<>0,
#   setze q:=q+1 und r:=r-abs(y).
# Falls x<0, setze r:=-r.
# Falls x,y verschiedene Vorzeichen haben, setze q:=-q.
# Liefere q,r.
  local maygc void I_I_ceiling_I_I (object x, object y)
  {
    pushSTACK(y);
    pushSTACK(x);
    pushSTACK(I_abs_I(y));
    # Stackaufbau: y, x, (abs y).
    x = I_abs_I(STACK_1); # (abs x)
    I_I_divide_I_I(x,STACK_0); # q,r auf den Stack
    # Stackaufbau: y, x, (abs y), q, r.
    if (same_sign_p(STACK_3,STACK_4))
      # x,y haben selbes Vorzeichen
      if (!eq(STACK_0,Fixnum_0)) {
        # r/=0, also r>0.
        STACK_1 = I_1_plus_I(STACK_1); # q := (1+ q)
        STACK_0 = I_I_minus_I(STACK_0,STACK_2); # r := (- r (abs y))
      }
    if (R_minusp(STACK_3)) {
      # x<0
      STACK_0 = I_minus_I(STACK_0); # r := (- r)
      if (!R_minusp(STACK_4)) # y>=0 ?
        goto negate_q; # q := (- q)
    } else {
      # x>=0
      if (R_minusp(STACK_4)) # y<0 ?
        negate_q: { STACK_1 = I_minus_I(STACK_1); } # q := (- q)
    }
    STACK_4 = STACK_1; STACK_3 = STACK_0; skipSTACK(3); # Stack aufräumen
  }

# Dividiert zwei Integers x,y und liefert Quotient und Rest
# (q,r) := (truncate x y)
# I_I_truncate_I_I(x,y);
# > x,y: Integers
# < STACK_1: Quotient q
# < STACK_0: Rest r
# Erniedrigt STACK um 2
# can trigger GC
  local void I_I_truncate_I_I (object x, object y);
# Methode:
# (truncate x y) :==
# (DIVIDE (abs x) (abs y)) -> q,r
# Falls x<0, setze r:=-r.
# Falls x,y verschiedene Vorzeichen haben, setze q:=-q.
# Liefere q,r.
  local maygc void I_I_truncate_I_I (object x, object y)
  {
    pushSTACK(y);
    pushSTACK(x);
    pushSTACK(I_abs_I(y));
    # Stackaufbau: y, x, (abs y).
    x = I_abs_I(STACK_1); # (abs x)
    I_I_divide_I_I(x,popSTACK()); # q,r auf den Stack
    # Stackaufbau: y, x, q, r.
    if (R_minusp(STACK_2)) {
      # x<0
      STACK_0 = I_minus_I(STACK_0); # r := (- r)
      if (!R_minusp(STACK_3)) # y>=0 ?
        goto negate_q; # q := (- q)
    } else {
      # x>=0
      if (R_minusp(STACK_3)) # y<0 ?
        negate_q: { STACK_1 = I_minus_I(STACK_1); } # q := (- q)
    }
    STACK_3 = STACK_1; STACK_2 = STACK_0; skipSTACK(2); # Stack aufräumen
  }

# Dividiert zwei Integers x,y und liefert Quotient und Rest
# (q,r) := (round x y)
# I_I_round_I_I(x,y);
# > x,y: Integers
# < STACK_1: Quotient q
# < STACK_0: Rest r
# Erniedrigt STACK um 2
# can trigger GC
  local void I_I_round_I_I (object x, object y);
# Methode:
# (round x y) :==
# (DIVIDE (abs x) (abs y)) -> q,r
# Setze s:=abs(y)-r.
# Falls (r>s) oder (r=s und q ungerade),
#   (d.h. falls r>abs(y)/2 oder r=abs(y)/2 und q ungerade),
#   setze q:=q+1 und r:=-s (d.h. r:=r-abs(y)).
# {Nun ist abs(r) <= abs(y)/2, bei abs(r)=abs(y)/2 ist q gerade.}
# Falls x<0, setze r:=-r.
# Falls x,y verschiedene Vorzeichen haben, setze q:=-q.
# Liefere q,r.
  local maygc void I_I_round_I_I (object x, object y)
  {
    pushSTACK(y);
    pushSTACK(x);
    pushSTACK(I_abs_I(y));
    # Stackaufbau: y, x, (abs y).
    x = I_abs_I(STACK_1); # (abs x)
    I_I_divide_I_I(x,STACK_0); # q,r auf den Stack
    # Stackaufbau: y, x, (abs y), q, r.
    var object s = I_I_minus_I(STACK_2,STACK_0); # (- (abs y) r)
    var signean comp_r_s = I_I_comp(STACK_0,s); # vergleiche r und s
    if ((comp_r_s>0) || ((comp_r_s==0) && (I_oddp(STACK_1)))) { # (r>s) oder (r=s und q ungerade) ?
      STACK_0 = I_minus_I(s); # r := (- s) = (- r (abs y))
      STACK_1 = I_1_plus_I(STACK_1); # q := (1+ q)
    }
    if (R_minusp(STACK_3)) {
      # x<0
      STACK_0 = I_minus_I(STACK_0); # r := (- r)
      if (!R_minusp(STACK_4)) # y>=0 ?
        goto negate_q; # q := (- q)
    } else {
      # x>=0
      if (R_minusp(STACK_4)) # y<0 ?
        negate_q: { STACK_1 = I_minus_I(STACK_1); } # q := (- q)
    }
    STACK_4 = STACK_1; STACK_3 = STACK_0; skipSTACK(3); # Stack aufräumen
  }

/* (EXT:MOD-EXPT base exponent modulo) = (MOD (EXPT base exponent) modulus)
   where exponent >= 0. */
local maygc object I_I_I_mod_expt_I (object base, object exponent, object modulus)
{
  /* See I_I_expt_I() in intmal.d for algorithm description.
     A much better algorithm is [Cohen, Algorithm 1.2.1.], implemented in
     cln-1.1.5/src/modinteger/cl_MI_std.h. */
  pushSTACK(base); pushSTACK(exponent); pushSTACK(modulus);
  if (eq(exponent,Fixnum_0)) {
    pushSTACK(Fixnum_1); /* result := 1 */
  } else {
    while (!I_oddp(exponent)) {
      STACK_2 = I_square_I(STACK_2); /* base := base * base */
      STACK_2 = I_I_mod_I(STACK_2,STACK_0); /* base := base mod modulus */
      STACK_1 = exponent = I_I_ash_I(STACK_1,Fixnum_minus1); /* exponent /= 2 */
    }
    pushSTACK(STACK_2);           /* result := base */
    /* STACK layout: base exponent modulus result */
    while (!eq(exponent=STACK_2,Fixnum_1)) {
      STACK_2 = I_I_ash_I(exponent,Fixnum_minus1); /* exponent /= 2 */
      var object a = STACK_3 = I_square_I(STACK_3); /* base := base * base */
      a = STACK_3 = I_I_mod_I(a,STACK_1); /* base := base mod modulus */
      if (I_oddp(STACK_2))
        /* we do not take MOD here because this will not grow too much anyway */
        STACK_0 = I_I_mal_I(a,STACK_0); /* result *= base */
    }
  }
  var object result = I_I_mod_I(STACK_0,STACK_1);
  skipSTACK(4);
  return result;
}
