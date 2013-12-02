# Konversionen zwischen Floating-Points

# Konversionen ohne Rundung:

# SF_to_FF(x) wandelt ein Short-Float x in ein Single-Float um.
# can trigger GC
  local maygc object SF_to_FF (object x)
  {
    # Falls
    # 1. Keine Konversion im Exponenten nötig,
    # 2. Vorzeichen/Exponent/Mantisse ist im SF (wie im FF) dicht gepackt,
    # 3. der Shift, der die Mantissen erweitert, schiebt das Vorzeichen nach
    #    Bit 31,
    # kann einfach geshiftet werden.
    #if (SF_exp_len==FF_exp_len) && (SF_exp_low>=FF_exp_low) && (SF_exp_mid==FF_exp_mid) && (SF_exp_high<=FF_exp_high) && (sign_bit_o==SF_exp_len+SF_exp_shift)
      # Dadurch auch 31-sign_bit_o = 31-SF_exp_len-SF_exp_shift
      #                            = 31-FF_exp_len-SF_mant_len-SF_mant_shift
      #                            = FF_mant_len-SF_mant_len-SF_mant_shift
      return
        allocate_ffloat(
          ((uint32)(as_oint(x) >> SF_mant_shift) << (FF_mant_len-SF_mant_len))
                       );
    #else
      # x entpacken:
      var signean sign;
      var sintWL exp;
      var uint32 mant;
      SF_decode(x, { return FF_0; }, sign=,exp=,mant=);
      # Mantisse um 23-16=7 Bits nach links schieben:
      encode_FF(sign,exp,mant<<(FF_mant_len-SF_mant_len), return);
    #endif
  }

# SF_to_DF(x) wandelt ein Short-Float x in ein Double-Float um.
# can trigger GC
  local maygc object SF_to_DF (object x)
  {
    # x entpacken:
    var signean sign;
    var sintWL exp;
    var uint32 mant;
    SF_decode(x, { return DF_0; }, sign=,exp=,mant=);
    # Mantisse um 52-16=36 Nullbits erweitern:
    #ifdef intQsize
    encode_DF(sign,exp,(uint64)mant<<(DF_mant_len-SF_mant_len), return);
    #else
    encode2_DF(sign,exp,mant<<(DF_mant_len-SF_mant_len-32),0, return);
    #endif
  }

# SF_to_LF(x,len) wandelt ein Short-Float x in ein Long-Float mit len Digits um.
# > uintC len: gewünschte Anzahl Digits, >=LF_minlen
# can trigger GC
  local maygc object SF_to_LF (object x, uintC len)
  {
    # x entpacken:
    var signean sign;
    var sintL exp;
    var uint32 mant;
    SF_decode(x, { encode_LF0(len, return); }, sign=,exp=(sintL),mant=);
    # Long-Float allozieren,
    # Mantisse mit intDsize*len-SF_mant_len-1 Nullbits auffüllen:
    var object y = allocate_lfloat(len,exp+LF_exp_mid,sign);
    var uintD* ptr = &TheLfloat(y)->data[0];
    # erste k := ceiling(SF_mant_len+1,intDsize) Digits mit mant füllen:
    mant = mant << (ceiling(SF_mant_len+1,intDsize)*intDsize-(SF_mant_len+1));
    set_max32_Dptr(SF_mant_len+1,ptr,mant);
    clear_loop_up(&ptr[ceiling(SF_mant_len+1,intDsize)],len-ceiling(SF_mant_len+1,intDsize));
    return y;
  }

# FF_to_DF(x) wandelt ein Single-Float x in ein Double-Float um.
# can trigger GC
  local maygc object FF_to_DF (object x)
  {
    # x entpacken:
    var signean sign;
    var sintWL exp;
    var uint32 mant;
    FF_decode(x, { return DF_0; }, sign=,exp=,mant=);
    # Mantisse um 52-23=29 Nullbits erweitern:
    #ifdef intQsize
    encode_DF(sign,exp,(uint64)mant<<(DF_mant_len-FF_mant_len), return);
    #else
    encode2_DF(sign,exp,mant>>(32-(DF_mant_len-FF_mant_len)),mant<<(DF_mant_len-FF_mant_len), return);
    #endif
  }

# FF_to_LF(x,len) wandelt ein Single-Float x in ein Long-Float mit len Digits um.
# > uintC len: gewünschte Anzahl Digits, >=LF_minlen
# can trigger GC
  local maygc object FF_to_LF (object x, uintC len)
  {
    # x entpacken:
    var signean sign;
    var sintL exp;
    var uint32 mant;
    FF_decode(x, { encode_LF0(len, return); }, sign=,exp=(sintL),mant=);
    # Long-Float allozieren,
    # Mantisse mit intDsize*len-FF_mant_len-1 Nullbits auffüllen:
    var object y = allocate_lfloat(len,exp+LF_exp_mid,sign);
    var uintD* ptr = &TheLfloat(y)->data[0];
    # erste k := ceiling(FF_mant_len+1,intDsize) Digits mit mant füllen:
    mant = mant << (ceiling(FF_mant_len+1,intDsize)*intDsize-(FF_mant_len+1));
    set_max32_Dptr(FF_mant_len+1,ptr,mant);
    clear_loop_up(&ptr[ceiling(FF_mant_len+1,intDsize)],len-ceiling(FF_mant_len+1,intDsize));
    return y;
  }

# DF_to_LF(x,len) wandelt ein Double-Float x in ein Long-Float mit len Digits um.
# > uintC len: gewünschte Anzahl Digits, >=LF_minlen
# can trigger GC
  local maygc object DF_to_LF (object x, uintC len)
  {
    # x entpacken:
    var signean sign;
    var sintL exp;
    var uint32 manthi;
    var uint32 mantlo;
    #ifdef intQsize
    var uint64 mant;
    DF_decode(x, { encode_LF0(len, return); }, sign=,exp=(sintL),mant=);
    #else
    DF_decode2(x, { encode_LF0(len, return); }, sign=,exp=(sintL),manthi=,mantlo=);
    #endif
    # Long-Float allozieren,
    # Mantisse mit intDsize*len-DF_mant_len-1 Nullbits auffüllen:
    var object y = allocate_lfloat(len,exp+LF_exp_mid,sign);
    var uintD* ptr = &TheLfloat(y)->data[0];
    # erste k := ceiling(DF_mant_len+1,intDsize) Digits mit mant füllen:
    #define shiftcount  (ceiling(DF_mant_len+1,intDsize)*intDsize-(DF_mant_len+1))
    #ifdef intQsize
    mant = mant<<shiftcount;
    manthi = (uint32)(mant>>32); mantlo = (uint32)mant;
    #else
    manthi = (manthi<<shiftcount) | (mantlo>>(32-shiftcount));
    mantlo = mantlo<<shiftcount;
    #endif
    #undef shiftcount
    set_max32_Dptr(DF_mant_len+1-32,ptr,manthi);
    set_32_Dptr(&ptr[ceiling(DF_mant_len+1-32,intDsize)],mantlo);
    clear_loop_up(&ptr[ceiling(DF_mant_len+1,intDsize)],len-ceiling(DF_mant_len+1,intDsize));
    return y;
  }

# Konversionen mit Rundung:

# FF_to_SF(x) wandelt ein Single-Float x in ein Short-Float um.
  local object FF_to_SF (object x)
  {
    # x entpacken:
    var signean sign;
    var sintWL exp;
    var uint32 mant;
    FF_decode(x, { return SF_0; }, sign=,exp=,mant=);
    # 23-16 Bits wegrunden:
    #define shiftcount  (FF_mant_len-SF_mant_len)
    if ( ((mant & bit(shiftcount-1)) ==0) # Bit 6 war 0 -> abrunden
         || ( ((mant & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 5..0 >0 -> aufrunden
              # round-to-even
              && ((mant & bit(shiftcount)) ==0)
       )    ) {
      # abrunden
      mant = mant >> shiftcount;
    } else {
      # aufrunden
      mant = mant >> shiftcount;
      mant = mant+1;
      if (mant >= bit(SF_mant_len+1)) {
        # Überlauf durchs Runden
        mant = mant>>1; exp = exp+1; # Mantisse rechts schieben
      } 
    }
    #undef shiftcount
    encode_SF(sign,exp,mant, return);
  }

# DF_to_SF(x) wandelt ein Double-Float x in ein Short-Float um.
  local object DF_to_SF (object x)
  {
    # x entpacken:
    var signean sign;
    var sintWL exp;
    #ifdef intQsize
    var uint64 mant;
    DF_decode(x, { return SF_0; }, sign=,exp=,mant=);
    # 52-16=36 Bits wegrunden:
    #define shiftcount  (DF_mant_len-SF_mant_len)
    if ( ((mant & bit(shiftcount-1)) ==0) # Bit 35 war 0 -> abrunden
         || ( ((mant & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 34..0 >0 -> aufrunden
              # round-to-even
              && ((mant & bit(shiftcount)) ==0)
       )    ) {
      # abrunden
      mant = mant >> shiftcount;
    } else {
      # aufrunden
      mant = mant >> shiftcount;
      mant = mant+1;
      if (mant >= bit(SF_mant_len+1)) {
        # Überlauf durchs Runden
        mant = mant>>1; exp = exp+1; # Mantisse rechts schieben
      }
    }
    #undef shiftcount
    encode_SF(sign,exp,mant, return);
    #else
    var uint32 manthi;
    var uint32 mantlo;
    DF_decode2(x, { return SF_0; }, sign=,exp=,manthi=,mantlo=);
    # 52-16=36 Bits wegrunden:
    #define shiftcount  (DF_mant_len-SF_mant_len-32)
    if ( ((manthi & bit(shiftcount-1)) ==0) # Bit 35 war 0 -> abrunden
         || ( ((manthi & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 34..0 >0 -> aufrunden
              && (mantlo==0)
              # round-to-even
              && ((manthi & bit(shiftcount)) ==0)
       )    ) {
      # abrunden
      manthi = manthi >> shiftcount;
    } else {
      # aufrunden
      manthi = manthi >> shiftcount;
      manthi = manthi+1;
      if (manthi >= bit(SF_mant_len+1)) {
        # Überlauf durchs Runden
        manthi = manthi>>1; exp = exp+1; # Mantisse rechts schieben
      }
    }
    #undef shiftcount
    encode_SF(sign,exp,manthi, return);
    #endif
  }

# LF_to_SF(x) wandelt ein Long-Float x in ein Short-Float um.
  local object LF_to_SF (object x)
  {
    # x entpacken:
    var signean sign;
    var sintL exp;
    var uintD* ptr;
    var uintC len;
    var uint32 mant;
    LF_decode(x, { return SF_0; }, sign=,exp=,ptr=,len=,);
    # intDsize*len-SF_mant_len-1 Bits der Mantisse wegrunden:
    # erste k := ceiling(SF_mant_len+2,intDsize) Digits nach mant holen:
    mant = get_max32_Dptr(SF_mant_len+2,ptr);
    ptr += ceiling(SF_mant_len+2,intDsize);
    #define shiftcount  (ceiling(SF_mant_len+2,intDsize)*intDsize-(SF_mant_len+1))
    if ( ((mant & bit(shiftcount-1)) ==0) # Bit 14 war 0 -> abrunden
         || ( ((mant & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 13..0 >0 -> aufrunden
              && !test_loop_up(ptr,len-ceiling(SF_mant_len+2,intDsize)) # weitere Bits /=0 -> aufrunden
              # round-to-even
              && ((mant & bit(shiftcount)) ==0)
       )    ) {
      # abrunden
      mant = mant >> shiftcount;
    } else {
      # aufrunden
      mant = mant >> shiftcount;
      mant = mant+1;
      if (mant >= bit(SF_mant_len+1)) {
        # Überlauf durchs Runden
        mant = mant>>1; exp = exp+1; # Mantisse rechts schieben
      }
    }
    #undef shiftcount
    encode_SF(sign,exp,mant, return);
  }

# DF_to_FF(x) wandelt ein Double-Float x in ein Single-Float um.
# can trigger GC
  local maygc object DF_to_FF (object x)
  {
    # x entpacken:
    var signean sign;
    var sintWL exp;
    #ifdef intQsize
    var uint64 mant;
    DF_decode(x, { return FF_0; }, sign=,exp=,mant=);
    # 52-23=29 Bits wegrunden:
    #define shiftcount  (DF_mant_len-FF_mant_len)
    if ( ((mant & bit(shiftcount-1)) ==0) # Bit 28 war 0 -> abrunden
         || ( ((mant & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 27..0 >0 -> aufrunden
              # round-to-even
              && ((mant & bit(shiftcount)) ==0)
       )    ) {
      # abrunden
      mant = mant >> shiftcount;
    } else {
      # aufrunden
      mant = mant >> shiftcount;
      mant = mant+1;
      if (mant >= bit(FF_mant_len+1)) {
        # Überlauf durchs Runden
        mant = mant>>1; exp = exp+1; # Mantisse rechts schieben
      }
    }
    #undef shiftcount
    encode_FF(sign,exp,mant, return);
    #else
    var uint32 manthi;
    var uint32 mantlo;
    DF_decode2(x, { return FF_0; }, sign=,exp=,manthi=,mantlo=);
    # 52-23=29 Bits wegrunden:
    #define shiftcount  (DF_mant_len-FF_mant_len)
    manthi = (manthi << (32-shiftcount)) | (mantlo >> shiftcount);
    if ( ((mantlo & bit(shiftcount-1)) ==0) # Bit 28 war 0 -> abrunden
         || ( ((mantlo & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 27..0 >0 -> aufrunden
              # round-to-even
              && ((mantlo & bit(shiftcount)) ==0)
       )    ) {
      # abrunden
    } else {
      # aufrunden
      manthi = manthi+1;
      if (manthi >= bit(FF_mant_len+1)) {
        # Überlauf durchs Runden
        manthi = manthi>>1; exp = exp+1; # Mantisse rechts schieben
      }
    }
    #undef shiftcount
    encode_FF(sign,exp,manthi, return);
    #endif
  }

# LF_to_FF(x) wandelt ein Long-Float x in ein Single-Float um.
# can trigger GC
  local maygc object LF_to_FF (object x)
  {
    # x entpacken:
    var signean sign;
    var sintL exp;
    var uintD* ptr;
    var uintC len;
    var uint32 mant;
    LF_decode(x, { return FF_0; }, sign=,exp=,ptr=,len=,);
    # intDsize*len-FF_mant_len-1 Bits der Mantisse wegrunden:
    # erste k := ceiling(FF_mant_len+2,intDsize) Digits nach mant holen:
    mant = get_max32_Dptr(FF_mant_len+2,ptr);
    ptr += ceiling(FF_mant_len+2,intDsize);
    #define shiftcount  (ceiling(FF_mant_len+2,intDsize)*intDsize-(FF_mant_len+1))
    if ( ((mant & bit(shiftcount-1)) ==0) # Bit 7 war 0 -> abrunden
         || ( ((mant & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 6..0 >0 -> aufrunden
              && !test_loop_up(ptr,len-ceiling(FF_mant_len+2,intDsize)) # weitere Bits /=0 -> aufrunden
              # round-to-even
              && ((mant & bit(shiftcount)) ==0)
       )    ) {
      # abrunden
      mant = mant >> shiftcount;
    } else {
      # aufrunden
      mant = mant >> shiftcount;
      mant = mant+1;
      if (mant >= bit(FF_mant_len+1)) {
        # Überlauf durchs Runden
        mant = mant>>1; exp = exp+1; # Mantisse rechts schieben
      }
    }
    #undef shiftcount
    encode_FF(sign,exp,mant, return);
  }

# LF_to_DF(x) wandelt ein Long-Float x in ein Double-Float um.
# can trigger GC
  local maygc object LF_to_DF (object x)
  {
    # x entpacken:
    var signean sign;
    var sintL exp;
    var uintD* ptr;
    var uintC len;
    var uint32 manthi;
    var uint32 mantlo;
    LF_decode(x, { return DF_0; }, sign=,exp=,ptr=,len=,);
    # intDsize*len-DF_mant_len-1 Bits der Mantisse wegrunden:
    # erste k := ceiling(DF_mant_len+2,intDsize) Digits nach manthi,mantlo holen:
    manthi = get_max32_Dptr(DF_mant_len+2-32,ptr);
    mantlo = get_32_Dptr(&ptr[ceiling(DF_mant_len+2-32,intDsize)]);
    ptr += ceiling(DF_mant_len+2,intDsize);
    #define shiftcount  (ceiling(DF_mant_len+2,intDsize)*intDsize-(DF_mant_len+1))
    #ifdef intQsize
    var uint64 mant = ((uint64)manthi << 32) | (uint64)mantlo;
    if ( ((mant & bit(shiftcount-1)) ==0) # Bit 10 war 0 -> abrunden
         || ( ((mant & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 9..0 >0 -> aufrunden
              && !test_loop_up(ptr,len-ceiling(DF_mant_len+2,intDsize)) # weitere Bits /=0 -> aufrunden
              # round-to-even
              && ((mant & bit(shiftcount)) ==0)
       )    ) {
      # abrunden
      mant = mant >> shiftcount;
    } else {
      # aufrunden
      mant = mant >> shiftcount;
      mant = mant+1;
      if (mant >= bit(DF_mant_len+1)) {
        # Überlauf durchs Runden
        mant = mant>>1; exp = exp+1; # Mantisse rechts schieben
      }
    }
    encode_DF(sign,exp,mant, return);
    #else
    if ( ((mantlo & bit(shiftcount-1)) ==0) # Bit 10 war 0 -> abrunden
         || ( ((mantlo & (bit(shiftcount-1)-1)) ==0) # war 1, Bits 9..0 >0 -> aufrunden
              && !test_loop_up(ptr,len-ceiling(DF_mant_len+2,intDsize)) # weitere Bits /=0 -> aufrunden
              # round-to-even
              && ((mantlo & bit(shiftcount)) ==0)
       )    ) {
      # abrunden
      mantlo = (manthi << (32-shiftcount)) | (mantlo >> shiftcount);
      manthi = manthi >> shiftcount;
    } else {
      # aufrunden
      mantlo = (manthi << (32-shiftcount)) | (mantlo >> shiftcount);
      manthi = manthi >> shiftcount;
      mantlo = mantlo+1;
      if (mantlo==0) {
        manthi = manthi+1;
        if (manthi >= bit(DF_mant_len+1-32)) {
          # Überlauf durchs Runden
          manthi = manthi>>1; exp = exp+1; # Mantisse rechts schieben
        }
      }
    }
    encode2_DF(sign,exp,manthi,mantlo, return);
    #endif
    #undef shiftcount
  }

# Konversionen zu IEEE-Floats.

# Fehlermeldung wegen NaN
# fehler_nan();
  nonreturning_function(local, fehler_nan, (void)) {
    fehler(arithmetic_error,
           GETTEXT("floating point NaN occurred")
          );
  }

# IEEE-Single-Float:
# Bit 31 = s, Bits 30..23 = e, Bits 22..0 = m.
#   e=0, m=0: vorzeichenbehaftete 0.0
#   e=0, m/=0: subnormale Zahl,
#     Wert = (-1)^s * 2^(1-126) * [ 0 . 0 m22 ... m0 ]
#   1 <= e <= 254 : normalisierte Zahl,
#     Wert = (-1)^s * 2^(e-126) * [ 0 . 1 m22 ... m0 ]
#   e=255, m=0: vorzeichenbehaftete Infinity
#   e=255, m/=0: NaN

# c_float_to_FF(&val) wandelt ein IEEE-Single-Float val in ein Single-Float um.
# can trigger GC
  global maygc object c_float_to_FF (const ffloatjanus* val_)
  {
    var ffloat val = val_->eksplicit;
    var uintBWL exp = (val >> FF_mant_len) & (bit(FF_exp_len)-1); # e
    if (exp == 0) { # e=0 ?
      # vorzeichenbehaftete 0.0 oder subnormale Zahl
      if (!((val << 1) == 0) && underflow_allowed())
        fehler_underflow();
      else
        return FF_0; # +/- 0.0 -> 0.0
    } elif (exp == 255) { # e=255 ?
      if (!((val << (32-FF_mant_len)) == 0))
        fehler_nan(); # NaN
      else
        fehler_overflow(); # Infinity, Overflow
    } else {
      # Der Exponent muss um FF_exp_mid-126 erhöht werden.
      if ((FF_exp_mid>126) && (exp > FF_exp_high-FF_exp_mid+126))
        fehler_overflow(); # Overflow
      val += (FF_exp_mid - 126) << FF_mant_len;
      return allocate_ffloat(val);
    }
  }

# FF_to_c_float(obj,&val);
# wandelt ein Single-Float obj in ein IEEE-Single-Float val um.
  global void FF_to_c_float (object obj, ffloatjanus* val_)
  {
    var ffloat val = ffloat_value(obj);
    # Der Exponent muss um FF_exp_mid-126 erniedrigt werden.
    if (FF_exp_mid>126) {
      var uintBWL exp = (val >> FF_mant_len) & (bit(FF_exp_len)-1); # e
      if (exp < FF_exp_mid-126+1) {
        if (!(exp == 0)) {
          # produziere denormalisiertes Float
          val = (val & minus_bit(FF_exp_len+FF_mant_len)) # selbes Vorzeichen
                | (0 << FF_mant_len) # Exponent 0
                | (((val & (bit(FF_mant_len)-1)) | bit(FF_mant_len)) # Mantisse shiften
                   >> (FF_exp_mid-126+1 - exp) # shiften
                  );
        }
      } else {
        val -= (FF_exp_mid - 126) << FF_mant_len;
      }
    }
    val_->eksplicit = val;
  }

# IEEE-Double-Float:
# Bit 63 = s, Bits 62..52 = e, Bits 51..0 = m.
#   e=0, m=0: vorzeichenbehaftete 0.0
#   e=0, m/=0: subnormale Zahl,
#     Wert = (-1)^s * 2^(1-1022) * [ 0 . 0 m51 ... m0 ]
#   1 <= e <= 2046 : normalisierte Zahl,
#     Wert = (-1)^s * 2^(e-1022) * [ 0 . 1 m51 ... m0 ]
#   e=2047, m=0: vorzeichenbehaftete Infinity
#   e=2047, m/=0: NaN

# c_double_to_DF(&val) wandelt ein IEEE-Double-Float val in ein Double-Float um.
# can trigger GC
  global maygc object c_double_to_DF (const dfloatjanus* val_)
  {
    var dfloat val; val = val_->eksplicit;
    #ifdef intQsize
    var uintWL exp = (val >> DF_mant_len) & (bit(DF_exp_len)-1); # e
    if (exp == 0) { # e=0 ?
      # vorzeichenbehaftete 0.0 oder subnormale Zahl
      if (!((val << 1) == 0) && underflow_allowed())
        fehler_underflow();
      else
        return DF_0; # +/- 0.0 -> 0.0
    } elif (exp == 2047) { # e=2047 ?
      if (!((val << (64-DF_mant_len)) == 0))
        fehler_nan(); # NaN
      else
        fehler_overflow(); # Infinity, Overflow
    } else {
      # Der Exponent muss um DF_exp_mid-1022 erhöht werden.
      if ((DF_exp_mid>1022) && (exp > DF_exp_high-DF_exp_mid+1022))
        fehler_overflow(); # Overflow
      val += (sint64)(DF_exp_mid - 1022) << DF_mant_len;
      return allocate_dfloat(val);
    }
    #else
    var uintWL exp = (val.semhi >> (DF_mant_len-32)) & (bit(DF_exp_len)-1); # e
    if (exp == 0) { # e=0 ?
      # vorzeichenbehaftete 0.0 oder subnormale Zahl
      if (!(((val.semhi << 1) == 0) && (val.mlo == 0)) && underflow_allowed())
        fehler_underflow();
      else
        return DF_0; # +/- 0.0 -> 0.0
    } elif (exp == 2047) { # e=2047 ?
      if (!(((val.semhi << (64-DF_mant_len)) == 0) && (val.mlo == 0)))
        fehler_nan(); # NaN
      else
        fehler_overflow(); # Infinity, Overflow
    } else {
      # Der Exponent muss um DF_exp_mid-1022 erhöht werden.
      if ((DF_exp_mid>1022) && (exp > DF_exp_high-DF_exp_mid+1022))
        fehler_overflow(); # Overflow
      val.semhi += (sint32)(DF_exp_mid - 1022) << (DF_mant_len-32);
      return allocate_dfloat(val.semhi,val.mlo);
    }
    #endif
  }

# DF_to_c_double(obj,&val);
# wandelt ein Double-Float obj in ein IEEE-Double-Float val um.
  global void DF_to_c_double (object obj, dfloatjanus* val_)
  {
    var dfloat val; val = TheDfloat(obj)->float_value;
    # Der Exponent muss um DF_exp_mid-1022 erniedrigt werden.
    if (DF_exp_mid>1022) {
      #ifdef intQsize
      var uintWL exp = (val >> DF_mant_len) & (bit(DF_exp_len)-1); # e
      if (exp < DF_exp_mid-1022+1) {
        if (!(exp == 0)) {
          # produziere denormalisiertes Float
          val = (val & minus_bit(DF_exp_len+DF_mant_len)) # selbes Vorzeichen
                | ((sint64)0 << DF_mant_len) # Exponent 0
                | (((val & (bit(DF_mant_len)-1)) | bit(DF_mant_len)) # Mantisse shiften
                   >> (DF_exp_mid-1022+1 - exp) # shiften
                  );
        }
      } else {
        val -= (sint64)(DF_exp_mid - 1022) << DF_mant_len;
      }
      #else
      var uintWL exp = (val.semhi >> (DF_mant_len-32)) & (bit(DF_exp_len)-1); # e
      if (exp < DF_exp_mid-1022+1) {
        if (!(exp == 0)) {
          # produziere denormalisiertes Float
          var uintWL shiftcount = DF_exp_mid-1022+1 - exp;
          val.mlo = val.mlo >> shiftcount; # Mantisse shiften
          val.mlo |= val.semhi << (32-shiftcount);
          val.semhi = (val.semhi & minus_bit(DF_exp_len+DF_mant_len-32)) # selbes Vorzeichen
                      | ((sint32)0 << (DF_mant_len-32)) # Exponent 0
                      | (((val.semhi & (bit(DF_mant_len-32)-1)) | bit(DF_mant_len-32)) # Mantisse shiften
                         >> shiftcount # shiften
                        );
        }
      } else {
          val.semhi -= (sint32)(DF_exp_mid - 1022) << (DF_mant_len-32);
      }
      #endif
    }
    val_->eksplicit = val;
  }

