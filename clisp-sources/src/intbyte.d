# BYTE-Operationen auf Integers

# Konstruktor: (I_I_Byte size position), wo size und position Integers sind.
# can trigger GC
  local maygc object I_I_Byte (object size, object position)
  {
    if (!(I_fixnump(size) && !R_minusp(size))) {
      pushSTACK(size); # TYPE-ERROR slot DATUM
     bad_args:
      pushSTACK(O(type_posfixnum)); # TYPE-ERROR slot EXPECTED-TYPE
      pushSTACK(position); pushSTACK(size);
      fehler(type_error,
             GETTEXT("The arguments to BYTE must be fixnums >=0: ~S, ~S")
            );
    } elif (!(I_fixnump(position) && !R_minusp(position))) {
      pushSTACK(position); # TYPE-ERROR slot DATUM
      goto bad_args;
    } else {
      # size, position sind Fixnums >=0, brauchen nicht gerettet zu werden
      var object new_byte = allocate_byte(); # neues Byte allozieren
      # und füllen:
      TheByte(new_byte)->byte_size = size;
      TheByte(new_byte)->byte_position = position;
      return new_byte;
    }
  }

# Fehler, wenn Argument kein Byte.
  nonreturning_function(local, fehler_byte, (object bad)) {
    pushSTACK(bad); # TYPE-ERROR slot DATUM
    pushSTACK(S(byte)); # TYPE-ERROR slot EXPECTED-TYPE
    pushSTACK(bad);
    fehler(type_error,
           GETTEXT("~S is not a BYTE specifier")
          );
  }

# Zugriffsfunktionen:

# Liefert (BYTE-SIZE byte). Das Argument wird überprüft.
  local object Byte_size (object obj)
  {
    if (bytep(obj))
      return TheByte(obj)->byte_size;
    else
      fehler_byte(obj);
  }

# Liefert (BYTE-POSITION byte). Das Argument wird überprüft.
  local object Byte_position (object obj)
  {
    if (bytep(obj))
      return TheByte(obj)->byte_position;
    else
      fehler_byte(obj);
  }

# Byte_to_V_V(byte, size=,position=); wandelt das Byte byte (eine Variable)
# um in size und position, beides uintL >=0, <2^oint_data_len.
  #define Byte_to_V_V(byte, size_zuweisung,position_zuweisung)           \
    {                                                                    \
      if (bytep(byte)) {                                                 \
        size_zuweisung posfixnum_to_V(TheByte(byte)->byte_size);         \
        position_zuweisung posfixnum_to_V(TheByte(byte)->byte_position); \
      } else                                                             \
        fehler_byte(byte);                                               \
    }

# fullbyte_I(p,q) liefert zu p,q die Zahl 2^q-2^p als Integer,
# wobei p und q uintV sind. Bei p<=q ist das Ergebnis also
# ein Integer >=0, bei dem genau die Bits p,...,q-1 gesetzt sind.
# can trigger GC
  local maygc object fullbyte_I (uintV p, uintV q)
  {
    if (p==q)
      return Fixnum_0; # p=q -> 0 als Ergebnis
    else {
      var object Iq = UV_to_I(q); # q als Integer >=0
      var object I2q = I_I_ash_I(Fixnum_1,Iq); # 2^q als Integer
      pushSTACK(I2q); # retten
      var object Ip = UV_to_I(p); # p als Integer >=0
      var object I2p = I_I_ash_I(Fixnum_minus1,Ip); # - 2^p als Integer
      I2q = popSTACK();
      return I_I_plus_I(I2p,I2q); # 2^q und -2^p addieren
    }
  }

# Extrahiere die Bits p,...,q-1 der Zahl x,
# wobei 0 <= p <= q <= l = (integer-length x).
# Ergebnis (wie bei LDB) ein Integer >=0.
# can trigger GC
  local maygc object ldb_extract (object x, uintL p, uintL q)
  {
    var uintD* MSDptr;
    var uintC len;
    var uintD* LSDptr;
    I_to_NDS_nocopy(x, MSDptr=,len=,LSDptr=); # NDS zu x bilden
    # MSDptr erhöhen und len erniedrigen, so dass len = ceiling(q/intDsize) wird:
    {
      var uintL qD = ceiling(q,intDsize); # ceiling(q/intDsize)
      # wegen q<=l ist qD = ceiling(q/intDsize) <= ceiling((l+1)/intDsize) = len, also
      # passt qD ebenso wie len in ein uintC.
      MSDptr += ((uintL)len - qD); # MSDptr um len-qD Digits erhöhen
      len = qD; # len um len-qD erniedrigen
    }
    # LSDptr und len um floor(p/intDsize) erniedrigen:
    {
      var uintL pD = floor(p,intDsize); # floor(p/intDsize)
      LSDptr -= pD;
      len -= pD;
    }
    # Jetzt enthält MSDptr/len/LSDptr genau die maßgeblichen Digits.
    {
      SAVE_NUM_STACK # num_stack retten
      var uintD* newMSDptr;
      {
        var uintL i = p%intDsize; # p mod intDsize
        # Kopiere sie und schiebe sie dabei um i Bits nach rechts:
        num_stack_need_1((uintL)len, newMSDptr=,); # neue UDS newMSDptr/len/..
        if (i==0) {
          copy_loop_up(MSDptr,newMSDptr,len);
        } else {
          begin_arith_call();
          shiftrightcopy_loop_up(MSDptr,newMSDptr,len,i,0);
          end_arith_call();
        }
      }
      # newMSDptr/len/.. = geschobene Kopie der maßgeblichen Digits
      # Ausblenden der Bits mit Nummern >= q-p:
      {
        var uintL bitcount = intDsize*(uintL)len - (q-p);
        # Anzahl vorne auszublendender Bits ( >=0, <= intDsize-1 + intDsize-1 )
        if (bitcount>=intDsize) {
          bitcount -= intDsize; newMSDptr += 1; len -= 1; # intDsize Bits ausblenden
        }
        # Noch 0 <= bitcount < intDsize Bits auszublenden:
        if (bitcount > 0) {
          newMSDptr[0] &= (uintD)(bit(intDsize-bitcount)-1);
        }
      }
      # Jetzt enthält die UDS newMSDptr/len/.. die extrahierten Bits.
      var object result = UDS_to_I(newMSDptr,len); # UDS in Integer umwandeln
      RESTORE_NUM_STACK # num_stack zurück
      return result;
    }
  }

# (LDB byte n), wo n ein Integer ist.
# can trigger GC
  local maygc object I_Byte_ldb_I (object n, object b)
  {
    # Methode:
    # (ldb (byte s p) n) extrahiere die Bits p,...,p+s-1 von n.
    # l:=(integer-length n)
    # Falls l <= p :
    #   Falls n>=0: 0, falls n<0: 2^s - 1 (s Einsenbits).
    # Falls p <= l :
    #   q:=min(p+s,l).
    #   Extrahiere die Bits p,...,q-1 von n.
    #   Falls p+s>l und n<0, füge p+s-l Einsenbits an (addiere 2^s-2^(l-p)).
    var uintV s;
    var uintV p;
    var uintL l;
    Byte_to_V_V(b, s=,p=); # size s und position p bestimmen
    l = I_integer_length(n); # l = (integer-length n)
    if (l<=p) {
      # l<=p
      if (!(R_minusp(n)))
        # n>=0
        return Fixnum_0; # 0 als Ergebnis
      else
        # n<0
        return fullbyte_I(0,s); # 2^s-2^0 als Ergebnis
    } else {
      # l>p
      var object erg;
      pushSTACK(n); # n retten
      {
        var uintV ps = p+s;
        # Bits p,...,q-1 mit q = min(p+s,l) extrahieren:
        erg = ldb_extract(n,p,(ps<l ? ps : l));
        n = popSTACK(); # n zurück
      }
      var uintL lp = l-p;
      if ((s>lp)&&(R_minusp(n))) { # s>l-p und n<0 ?
        pushSTACK(erg); # erg retten
        var object erg2 = fullbyte_I(lp,s);
        # erg2 = Integer-Zahl mit gesetzten Bits l-p,...,s-1
        erg = popSTACK(); # erg zurück
        return I_I_logior_I(erg,erg2); # logisches Oder aus beiden
        # (logisches Exklusiv-Oder oder Addition ginge auch)
      } else
        return erg;
    }
  }

# Teste, ob eines der Bits p,...,q-1 der Zahl x /=0 ist,
# wobei 0 <= p <= q <= l = (integer-length x).
# Ergebnis (wie bei LDB-TEST) false wenn nein, true wenn ja.
  local bool ldb_extract_test (object x, uintL p, uintL q)
  {
    var uintD* MSDptr;
    var uintC len;
    var uintD* LSDptr;
    I_to_NDS_nocopy(x, MSDptr=,len=,LSDptr=); # NDS zu x bilden
    # MSDptr erhöhen und len erniedrigen, so dass len = ceiling(q/intDsize) wird:
    {
      var uintL qD = ceiling(q,intDsize); # ceiling(q/intDsize)
      # wegen q<=l ist qD = ceiling(q/intDsize) <= ceiling((l+1)/intDsize) = len, also
      # passt qD ebenso wie len in ein uintC.
      MSDptr += ((uintL)len - qD); # MSDptr um len-qD Digits erhöhen
      len = qD; # len um len-qD erniedrigen
    }
    # LSDptr und len um floor(p/intDsize) erniedrigen:
    {
      var uintL pD = p/intDsize; # floor(p/intDsize)
      LSDptr -= pD;
      len -= pD;
    }
    # Jetzt enthält MSDptr/len/LSDptr genau die maßgeblichen Digits.
    if (len==0) # len=0 -> keine Bits abzutesten
      return false;
    q = ((q-1)%intDsize); # q := intDsize - (intDsize*ceiling(q/intDsize) - q) - 1
    p = p%intDsize; # p := p - intDsize*floor(p/intDsize)
    # Jetzt ist 0 <= q < intDsize, 0 <= p < intDsize.
    # Vom ersten Digit müssen die vorderen intDsize-1-q Bits unberücksichtigt bleiben.
    # Ein AND 2^(q+1)-1 erreicht dies.
    # Vom letzten Digit müssen die hinteren p Bits unberücksichtigt bleiben.
    # Ein AND -2^p erreicht dies.
    if (--len==0) {
      # 1 Digit maßgeblich, wird von beiden Seiten angeschnitten:
      # Ein AND 2^(q+1)-2^p erreicht dies.
      if (!(((uintD)(bitm(q+1)-bit(p)) & *MSDptr) == 0))
        return true;
      else
        return false;
    }
    # mindestens 2 Digits. Teste erst die Randdigits, dann die inneren:
    if (!(((*MSDptr++ & (uintD)(bitm(q+1)-1)) == 0)
          && ((*--LSDptr & (uintD)(minus_bit(p))) == 0)
       ) )
      return true;
    len--; # die beiden Randdigits sind jetzt abgezogen.
    if (test_loop_up(MSDptr,len))
      return true;
    else
      return false;
  }

# I_Byte_ldb_test(n,byte) führt (LDB-TEST byte n) aus, wobei n ein Integer ist.
# Ergebnis: false wenn nein (also alle fraglichen Bits =0), true wenn ja.
  local bool I_Byte_ldb_test (object n, object b)
  {
    # Methode:
    # (ldb-test (byte s p) n)
    # Falls s=0: =0.
    # Falls s>0:
    #   l:=(integer-length n)
    #   Falls l <= p : Falls n>=0, =0, denn Bits p+s-1..p sind =0.
    #                  Falls n<0, /=0, denn Bits p+s-1..p sind =1.
    #   Falls p < l :
    #     Falls p+s>l, /=0, denn bei n>=0 ist Bit l-1 =1,
    #                       und bei n<0 sind Bits p+s-1..l =1.
    #     Falls p+s<=l,
    #       extrahiere die Bits p,...,p+s-1 von n und teste sie.
    var uintV s;
    var uintV p;
    var uintL l;
    Byte_to_V_V(b, s=,p=); # size s und position p bestimmen
    if (s==0)
      return false;
    l = I_integer_length(n); # l = (integer-length n)
    if (l<=p) {
      # l<=p
      if (!(R_minusp(n)))
        return false; # n>=0
      else
        return true; # n<0
    } else {
      # l>p
      var uintV ps = p+s;
      if (ps>l) # p+s>l ?
        return true;
      # Bits p,...,q-1 mit q = min(p+s,l) = p+s extrahieren und testen:
      return ldb_extract_test(n,p,ps);
    }
  }

# Extrahiere die Bits p,...,q-1 der Zahl x,
# wobei 0 <= p <= q <= l = (integer-length x).
# Ergebnis (wie bei MASK-FIELD) ein Integer >=0.
# can trigger GC
  local maygc object mkf_extract (object x, uintL p, uintL q)
  {
    var uintD* MSDptr;
    var uintC len;
    var uintD* LSDptr;
    I_to_NDS_nocopy(x, MSDptr=,len=,LSDptr=); # NDS zu x bilden
    # MSDptr erhöhen und len erniedrigen, so dass len = ceiling(q/intDsize) wird:
    {
      var uintL qD = ceiling(q,intDsize); # ceiling(q/intDsize)
      # wegen q<=l ist qD = ceiling(q/intDsize) <= ceiling((l+1)/intDsize) = len, also
      # passt qD ebenso wie len in ein uintC.
      MSDptr += ((uintL)len - qD); # MSDptr um len-qD Digits erhöhen
      len = qD; # len um len-qD erniedrigen
    }
    {
      SAVE_NUM_STACK # num_stack retten
      # Platz (len Digits) für die neue UDS bereitstellen:
      var uintD* newMSDptr;
      num_stack_need_1((uintL)len, newMSDptr = ,); # Platz belegen
      {
        var uintL pD = p/intDsize; # floor(p/intDsize), passt in ein uintC
        # Kopiere len-pD Digits aus der DS zu x heraus:
        var uintD* midptr = copy_loop_up(MSDptr,newMSDptr,len-(uintC)pD);
        # Lösche p-intDsize*floor(p/intDsize) Bits im Digit unterhalb von midptr:
        {
          var uintL p_D = p%intDsize;
          if (!(p_D==0)) {
            midptr[-1] &= minus_bit(p_D);
          }
        }
        # Lösche pD Digits darüber:
        clear_loop_up(midptr,pD);
      }
      # Lösche intDsize*ceiling(q/intDsize)-q Bits im ersten Digit:
      {
        var uintL q_D = q%intDsize;
        if (!(q_D==0)) {
          newMSDptr[0] &= (uintD)((1L<<q_D)-1); # intDsize-q_D Bits löschen
        }
      }
      # Jetzt enthält die UDS newMSDptr/len/.. die extrahierten Bits.
      var object result = UDS_to_I(newMSDptr,len);
      RESTORE_NUM_STACK # num_stack zurück
      return result;
    }
  }

# (MASK-FIELD byte n), wo n ein Integer ist.
# can trigger GC
  local maygc object I_Byte_mask_field_I (object n, object b)
  {
    # Methode:
    # (mask-field (byte s p) n) extrahiere die Bits p,...,p+s-1 von n.
    # l:=(integer-length n)
    # Falls l <= p :
    #   Falls n>=0: 0, falls n<0: 2^(p+s) - 2^p (s Einsenbits).
    # Falls p <= l :
    #   q:=min(p+s,l).
    #   Extrahiere die Bits p,...,q-1 von n.
    #   Falls p+s>l und n<0, füge p+s-l Einsenbits an (addiere 2^(p+s)-2^l).
    var uintV s;
    var uintV p;
    var uintL l;
    Byte_to_V_V(b, s=,p=); # size s und position p bestimmen
    var uintV ps = p+s;
    l = I_integer_length(n); # l = (integer-length n)
    if (l<=p) {
      # l<=p
      if (!(R_minusp(n)))
        # n>=0
        return Fixnum_0; # 0 als Ergebnis
      else
        # n<0
        return fullbyte_I(p,ps); # 2^(p+s)-2^p als Ergebnis
    } else {
      # l>p
      var object erg;
      pushSTACK(n); # n retten
      # Bits p,...,q-1 mit q = min(p+s,l) extrahieren:
      erg = mkf_extract(n,p,(ps<l ? ps : l));
      n = popSTACK(); # n zurück
      if ((ps>l)&&(R_minusp(n))) { # p+s>l und n<0 ?
        pushSTACK(erg); # erg retten
        var object erg2 = fullbyte_I(l,ps);
        # erg2 = Integer-Zahl mit gesetzten Bits l,...,p+s-1
        erg = popSTACK(); # erg zurück
        return I_I_logior_I(erg,erg2); # logisches Oder aus beiden
        # (logisches Exklusiv-Oder oder Addition ginge auch)
      } else
        return erg;
    }
  }

# (DEPOSIT-FIELD new byte n), wo n und new Integers sind.
# can trigger GC
  local maygc object I_I_Byte_deposit_field_I (object newbyte, object n, object b)
  {
    # Methode:
    # (DEPOSIT-FIELD newbyte (byte s p) integer)
    #  = (logxor integer
    #            (ash (logxor (ldb (byte s p) newbyte) (ldb (byte s p) integer))
    #                 p
    #    )       )
    pushSTACK(n); # integer in den Stack
    pushSTACK(b); # (byte s p) in den Stack
    var object temp1 = I_Byte_ldb_I(newbyte,b); # (ldb (byte s p) newbyte)
    pushSTACK(temp1); # in den Stack
    temp1 = I_Byte_ldb_I(STACK_2,STACK_1); # (ldb (byte s p) integer)
    temp1 = I_I_logxor_I(popSTACK(),temp1); # beides mit LOGXOR verknüpfen
    temp1 = I_I_ash_I(temp1,Byte_position(popSTACK())); # (ash ... p)
    return I_I_logxor_I(popSTACK(),temp1); # mit integer LOGXORen
  }

# (DPB new byte n), wo n und new Integers sind.
# can trigger GC
  local maygc object I_I_Byte_dpb_I (object newbyte, object n, object b)
  {
    # Methode:
    # (DPB newbyte (byte s p) integer)
    # = (DEPOSIT-FIELD (ASH newbyte p) (byte s p) integer)
    pushSTACK(n); # integer in den Stack
    pushSTACK(b); # (byte s p) in den Stack
    var object temp1 = I_I_ash_I(newbyte,Byte_position(b));
    b = popSTACK();
    n = popSTACK();
    return I_I_Byte_deposit_field_I(temp1,n,b);
  }

