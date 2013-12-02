/* Serialization of integers into a little-endian sequence of bytes */

# Converts a little-endian byte sequence to an unsigned integer.
# > bytesize: number of given 8-bit bytes of the integer,
#             < intDsize/8*uintWC_max
# > bufferptr: address of bytesize bytes of memory
# < result: an integer >= 0 with I_integer_length(result) <= 8*bytesize
global maygc object LEbytes_to_UI (uintL bytesize, const uintB* bufferptr) {
  var gcv_object_t fake; fake = FAKE_8BIT_VECTOR(bufferptr);
  return LESbvector_to_UI(bytesize,&fake);
}

# Converts a little-endian byte sequence to an unsigned integer.
# > bytesize: number of given 8-bit bytes of the integer,
#             < intDsize/8*uintWC_max
# > *buffer_: address of a simple-8bit-vector (or of a fake)
#             containing bytesize bytes of memory
# < result: an integer >= 0 with I_integer_length(result) <= 8*bytesize
global maygc object LESbvector_to_UI (uintL bytesize, const gcv_object_t* buffer_) {
  # Normalize number in buffer:
  var uintB* bufferptr = &TheSbvector(*buffer_)->data[bytesize-1];
  var uintL count = bytesize;
  while ((!(count==0)) && (*bufferptr==0)) { count--; bufferptr--; }
  # Make number:
  if # at most oint_data_len Bits ?
    ((count <= floor(oint_data_len,8))
     || ((count == floor(oint_data_len,8)+1)
         && (*bufferptr < bit(oint_data_len%8)))) {
    # yes -> build Fixnum >=0 :
    var uintV wert = 0;
    until (count==0) { wert = (wert<<8) | *bufferptr--; count--; }
    return fixnum(wert);
  }
  # no -> build Bignum >0 :
  var uintL digitcount = floor(count,(intDsize/8));
  if (((count%(intDsize/8)) > 0) || (*bufferptr & bit(7)))
    digitcount++;
  # As bitsize < intDsize*uintWC_max,
  # digitcount <= ceiling((bitsize+1)/intDsize) <= uintWC_max .
  var object big = allocate_bignum(digitcount,0); # new Bignum >0
  TheBignum(big)->data[0] = 0; # set highest Digit to 0
  # Fill remaining Digits from right to left,
  # thereby translate sequence of Bytes into sequence of uintD:
  bufferptr = &TheSbvector(*buffer_)->data[0];
  #if BIG_ENDIAN_P
  {
    var uintB* bigptr = (uintB*)(&TheBignum(big)->data[digitcount]);
    dotimespL(count,count, { *--bigptr = *bufferptr++; } );
  }
  #else
  {
    var uintD* bigptr = &TheBignum(big)->data[digitcount];
    var uintL count2;
    #define GET_NEXT_BYTE(i)  digit |= ((uintD)(*bufferptr++) << (8*i));
    dotimespL(count2,floor(count,intDsize/8), {
      var uintD digit = 0;
      DOCONSTTIMES(intDsize/8,GET_NEXT_BYTE); # GET_NEXT_BYTE(0..intDsize/8-1)
      *--bigptr = digit;
    });
    #undef GET_NEXT_BYTE
    count2 = count % (intDsize/8);
    if (count2>0) {
      var uintL shiftcount = 0;
      var uintD digit = (uintD)(*bufferptr++);
      dotimesL(count2,count2-1, {
        shiftcount += 8;
        digit |= ((uintD)(*bufferptr++) << shiftcount);
      });
      *--bigptr = digit;
    }
  }
  #endif
  # Since (intDsize/8)*(digitcount-1) <= count <= (intDsize/8)*digitcount
  # everything is filled.
  return big;
}

# Converts a little-endian byte sequence to an integer.
# > bytesize: number of given 8-bit bytes of the integer, > 0,
#             < intDsize/8*uintWC_max
# > bufferptr: address of bytesize bytes of memory
# < result: an integer with I_integer_length(result) < 8*bytesize
global maygc object LEbytes_to_I (uintL bytesize, const uintB* bufferptr) {
  var gcv_object_t fake; fake = FAKE_8BIT_VECTOR(bufferptr);
  return LESbvector_to_I(bytesize,&fake);
}

# Converts a little-endian byte sequence to an integer.
# > bytesize: number of given 8-bit bytes of the integer, > 0,
#             < intDsize/8*uintWC_max
# > *buffer_: address of a simple-8bit-vector (or of a fake)
#             containing bytesize bytes of memory
# < result: an integer with I_integer_length(result) < 8*bytesize
global maygc object LESbvector_to_I (uintL bytesize, const gcv_object_t* buffer_) {
  # Normalize number in buffer:
  var uintB* bufferptr = &TheSbvector(*buffer_)->data[bytesize-1];
  var sintD sign;
  var uintL count = bytesize;
  if (!(*bufferptr & bit(7))) {
    sign = 0;
    # Normalize, highest Bit must remain 0:
    while ((count>=2) && (*bufferptr==0)
           && !(*(bufferptr-1) & bit(7))) {
      count--; bufferptr--;
    }
    # Make number:
    if # at most oint_data_len+1 Bits, count <2^oint_data_len ?
      ((count <= floor(oint_data_len,8))
       || ((count == floor(oint_data_len,8)+1)
           && (*bufferptr < bit(oint_data_len%8)))) {
      # yes -> build Fixnum >=0:
      var uintV value = 0;
      until (count==0) { value = (value<<8) | *bufferptr--; count--; }
      return posfixnum(value);
    }
  } else {
    sign = -1;
    # Normalize, highest Bit must remain 1:
    while ((count>=2) && (*bufferptr==(uintB)(-1))
           && (*(bufferptr-1) & bit(7))) {
      count--; bufferptr--;
    }
    # Make number:
    if # at most oint_data_len+1 Bits, count >=-2^oint_data_len ?
      ((count <= floor(oint_data_len,8))
       || ((count == floor(oint_data_len,8)+1)
           && (*bufferptr >= (uintB)(-bit(oint_data_len%8))))) {
      # yes -> build Fixnum <0:
      var uintV value = (uintV)(sintV)(-1);
      until (count==0) { value = (value<<8) | *bufferptr--; count--; }
      return negfixnum(-wbitm(intVsize)+(oint)value);
    }
  }
  # Make bignum:
  var uintL digitcount = ceiling(count,(intDsize/8));
  # As bitsize < intDsize*uintWC_max,
  # digitcount <= ceiling(bitsize/intDsize) <= uintWC_max .
  var object big = allocate_bignum(digitcount,(sintB)sign);
  TheBignum(big)->data[0] = sign; # set highest Word to sign
  # Fill the remaining Digits from right to left,
  # thereby translate sequence of Bytes into sequence of uintD:
  bufferptr = &TheSbvector(*buffer_)->data[0];
  #if BIG_ENDIAN_P
  {
    var uintB* bigptr = (uintB*)(TheBignum(big)->data+digitcount);
    dotimespL(count,count, { *--bigptr = *bufferptr++; } );
  }
  #else
  {
    var uintD* bigptr = TheBignum(big)->data+digitcount;
    var uintL count2;
    #define GET_NEXT_BYTE(i)  digit |= ((uintD)(*bufferptr++) << (8*i));
    dotimespL(count2,floor(count,intDsize/8), {
      var uintD digit = 0;
      DOCONSTTIMES(intDsize/8,GET_NEXT_BYTE); # GET_NEXT_BYTE(0..intDsize/8-1)
      *--bigptr = digit;
    });
    #undef GET_NEXT_BYTE
    count2 = count % (intDsize/8);
    if (count2>0) {
      var uintL shiftcount = 0;
      var uintD digit = (uintD)(*bufferptr++);
      dotimesL(count2,count2-1, {
        shiftcount += 8;
        digit |= ((uintD)(*bufferptr++) << shiftcount);
      });
      *--bigptr = digit ^ (sign << (shiftcount+8));
    }
  }
  #endif
  # Since (intDsize/8)*(digitcount-1) < count <= (intDsize/8)*digitcount
  # everything is filled.
  return big;
}

# Converts an unsigned integer to a little-endian byte sequence.
# > obj: an integer
# > bitsize: maximum number of bits of the integer
# > bufferptr: pointer to bytesize = ceiling(bitsize,8) bytes of memory
# < false and bufferptr[0..bytesize-1] filled, if obj >= 0 and
#                                              I_integer_length(obj) <= bitsize;
#   true, if obj is out of range
global bool UI_to_LEbytes (object obj, uintL bitsize, uintB* bufferptr) {
  if (!positivep(obj))
    return true;
  # obj is an integer >=0.
  var uintL bytesize = ceiling(bitsize,8);
  # Transfer obj into the buffer:
  {
    var uintL count = bytesize;
    if (posfixnump(obj)) { # obj is a Fixnum >=0
      var uintV value = posfixnum_to_V(obj);
      # check value < 2^bitsize:
      if (!((bitsize>=oint_data_len) || (value < vbit(bitsize))))
        return true;
      # store value in Bitbuffer:
      until (value==0) {
        *bufferptr++ = (uint8)value; value = value>>8; count--;
      }
    } else { # obj is a Bignum >0
      var uintL len = (uintL)Bignum_length(obj);
      # check obj < 2^bitsize:
      if (!((floor(bitsize,intDsize) >= len)
            || ((floor(bitsize,intDsize) == len-1)
                && (TheBignum(obj)->data[0] < bit(bitsize%intDsize)))))
        return true;
      #if BIG_ENDIAN_P
      {
        var uintB* ptr = (uintB*)&TheBignum(obj)->data[len];
        # convert Digit-Length in Byte-Length:
        len = (intDsize/8)*len;
        #define CHECK_NEXT_BYTE(i)  \
          if (((uintB*)(&TheBignum(obj)->data[0]))[i] != 0) goto len_ok; \
          len--;
        DOCONSTTIMES(intDsize/8,CHECK_NEXT_BYTE); # CHECK_NEXT_BYTE(0..intDsize/8-1)
        #undef CHECK_NEXT_BYTE
      len_ok:
        # store obj in Bitbuffer:
        count = count - len;
        dotimespL(len,len, { *bufferptr++ = *--ptr; } );
      }
      #else
      {
        var uintD* ptr = &TheBignum(obj)->data[len];
        len--;
        count -= (intDsize/8)*len;
        dotimesL(len,len, {
          var uintD digit = *--ptr;
          doconsttimes(intDsize/8, {
            *bufferptr++ = (uintB)digit; digit = digit >> 8;
          });
        });
        var uintD digit = *--ptr;
        doconsttimes(intDsize/8, {
          if (digit==0) goto ok;
          *bufferptr++ = (uintB)digit; digit = digit >> 8;
          count--;
        });
      ok: ;
      }
    #endif
    }
    if (count > 0) {
      begin_system_call();
      memset(bufferptr,0,count);
      end_system_call();
    }
  }
  return false;
}

# Converts an integer to a little-endian byte sequence.
# > obj: an integer
# > bitsize: maximum number of bits of the integer, including the sign bit
# > bufferptr: pointer to bytesize = ceiling(bitsize,8) bytes of memory
# < false and bufferptr[0..bytesize-1] filled, if I_integer_length(obj) < bitsize;
#   true, if obj is out of range
global bool I_to_LEbytes (object obj, uintL bitsize, uintB* bufferptr) {
  # obj is an integer.
  var uintL bytesize = ceiling(bitsize,8);
  # Transfer obj into the buffer:
  {
    var uintL count = bytesize;
    var uintV sign = (sintV)(sintL)R_sign(obj);
    if (fixnump(obj)) {
      # obj is a Fixnum
      var uintV value = fixnum_to_V(obj); # >=0 or <0, according to sign
      # check 0 <= value < 2^(bitsize-1) resp. -2^(bitsize-1) <= value < 0:
      value = value^sign;
      if (!((bitsize>oint_data_len) || (value < bit(bitsize-1))))
        return true;
      # store value^sign in Bitbuffer:
      until (value == 0) {
        *bufferptr++ = (uint8)(value^sign); value = value>>8; count--;
      }
      if (count > 0) {
        begin_system_call();
        memset(bufferptr,(uint8)sign,count);
        end_system_call();
      }
    } else {
      # obj is a Bignum
      var uintL len = (uintL)Bignum_length(obj);
      # check -2^(bitsize-1) <= obj < 2^(bitsize-1):
      if (!((floor(bitsize,intDsize) >= len)
            || ((bitsize > intDsize*(len-1))
                && ((TheBignum(obj)->data[0] ^ (uintD)sign) <
                    bit((bitsize%intDsize)-1)))))
        return true;
      #if BIG_ENDIAN_P
      {
        var uintB* ptr = (uintB*)&TheBignum(obj)->data[len];
        # convert Digit-Length in Byte-Length:
        len = (intDsize/8)*len;
        #define CHECK_NEXT_BYTE(i)  \
          if (((uintB*)(&TheBignum(obj)->data[0]))[i] != (uintB)sign) goto len_ok; \
          len--;
        DOCONSTTIMES(intDsize/8,CHECK_NEXT_BYTE); # CHECK_NEXT_BYTE(0..intDsize/8-1)
        #undef CHECK_NEXT_BYTE
      len_ok:
        # store obj in Bitbuffer:
        count = count - len;
        dotimespL(len,len, { *bufferptr++ = *--ptr; } );
      }
      #else
      {
        var uintD* ptr = &TheBignum(obj)->data[len];
        len--;
        count -= (intDsize/8)*len;
        dotimesL(len,len, {
          var uintD digit = *--ptr;
          doconsttimes(intDsize/8, {
            *bufferptr++ = (uintB)digit; digit = digit >> 8;
          });
        });
        var sintD digit = *--ptr;
        doconsttimes(intDsize/8, {
          if (digit == (sintD)sign) goto ok;
          *bufferptr++ = (uintB)digit; digit = digit >> 8;
          count--;
        });
      ok: ;
      }
      #endif
      if (count > 0) {
        begin_system_call();
        memset(bufferptr,(uintB)sign,count);
        end_system_call();
      }
    }
  }
  return false;
}
