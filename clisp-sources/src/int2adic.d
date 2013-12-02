# Operationen mit 2-adischen Integers

# Multipliziert zwei Zahlen mod 2^intDsize.
# D_D_mal2adic_D(a,b)
# > uintD a,b: Zahlen mod 2^intDsize
# < ergebnis: Zahl c mod 2^intDsize mit c == a*b mod 2^intDsize
  local uintD D_D_mal2adic_D (uintD a, uintD b);
#if HAVE_DD
  #define D_D_mal2adic_D(a,b)  lowD(muluD((uintD)(a),(uintD)(b)))
#else
  #if defined(GNU) || defined(INTEL)
    #define D_D_mal2adic_D(a,b)  \
      ({ var uintD __erg;              \
         muluD((a),(b), (void),__erg=);   \
         __erg;                        \
       })
  #else
    #if (intDsize==32)
      #define D_D_mal2adic_D(a,b)  mulu32_(a,b) # lo-Teil von mulu32(a,b)
    #else
      local uintD D_D_mal2adic_D (uintD a, uintD b)
      {
        muluD(a,b, (void),return);
      }
    #endif
  #endif
#endif

# Potenziert eine Zahl mod 2^intDsize.
# D_UL_expt_D(x,y)
# > uintD x: Zahl mod 2^intDsize
# > uintL y: Exponent >0
# < uintD ergebnis: x^y mod 2^intDsize
  local uintD D_UL_expt_D (uintD x, uintL y);
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
  local uintD D_UL_expt_D (uintD a, uintL b)
  {
    while ((b & bit(0)) ==0) {
      a = D_D_mal2adic_D(a,a); b = b>>1;
    }
    var uintD c = a;
    until ((b = b>>1) == 0) {
      a = D_D_mal2adic_D(a,a);
      if (b & bit(0))
        c = D_D_mal2adic_D(a,c);
    }
    return c;
  }

# Dividiert zwei Zahlen mod 2^intDsize.
# D_D_durch2adic_D(a,b)
# > uintD a: Zahl mod 2^intDsize
# > uintD b: ungerade Zahl mod 2^intDsize
# < ergebnis: Zahl c mod 2^intDsize mit b*c == a mod 2^intDsize
  local uintD D_D_durch2adic_D (uintD a, uintD b);
# Methode:
# Konstruiere c Bit für Bit.
# c := 0, d := a.
# Für j=0,...,intDsize:
#   [Hier b*c == a mod 2^j und d = (a-b*c)/2^j.] j=intDsize -> fertig.
#   Falls d ungerade, setze c:=c+2^j und d:=(d-b)/2, sonst d:=d/2.
# Ergebnis c.
#if 1
  local uintD D_D_durch2adic_D (uintD a, uintD b)
  {
    ASSERT(!((b % 2) ==0));
    var uintD c = 0;
    var uintD bit_j = 1; # 2^j
    loop { # Verwende a als Variable d
      if (a & bit(0)) {
        c = c+bit_j; a = a-b;
      }
      a = a>>1;
      bit_j = bit_j << 1;
      if (bit_j == 0) # j=intDsize -> fertig
        break;
    }
    return c;
  }
#else
  local uintD D_D_durch2adic_D (uintD a, uintD b)
  {
    ASSERT(!((b % 2) ==0));
    var uintD bit_j = 1; # 2^j
    var uintD b_j = b-1; # (b-1)*2^j
    loop { # Verwende a als Variable d*2^j+c
      if (a & bit_j)
        a = a - b_j;
      b_j = b_j << 1; bit_j = bit_j << 1;
      if (bit_j == 0) # j=intDsize -> fertig
        break;
    }
    return a;
  }
#endif

# UDS_UDS_durch2adic_UDS(len,a_LSDptr,b_LSDptr,dest_LSDptr);
# dividiert die UDS a_LSDptr[-len..-1] mod 2^(intDsize*len)
# durch die ungerade UDS b_LSDptr[-len..-1] mod 2^(intDsize*len) (len>0)
# und liefert den Quotienten als UDS dest_LSDptr[-len..-1] mod 2^(intDsize*len).
  local void UDS_UDS_durch2adic_UDS (uintC len, const uintD* a_LSDptr, uintD* b_LSDptr, uintD* dest_LSDptr);
# Methode: beta=2^intDsize. Schreibe jeweils x = x[0]*beta^0 + x[1]*beta^1 + ... .
# Um b*c == a mod beta^m zu bestimmen:
# Sei b' := (b mod beta)^(-1) = b[0]^(-1), d := a.
# Für j=0,...,m:
#   [Hier ist d = a - b*c == 0 mod beta^j, und c mod beta^j bekannt.]
#   j=m -> fertig.
#   Setze c[j] := b'*d[j] mod beta, also c := c + (b'*d[j] mod beta)*beta^j.
#   Setze d := d - b * c[j] * beta^j.
# Schließlich dest := c.
  local void UDS_UDS_durch2adic_UDS (uintC len, const uintD* a_LSDptr, uintD* b_LSDptr, uintD* dest_LSDptr)
  {
    var uintD b0inv = D_D_durch2adic_D(1,b_LSDptr[-1]); # b'
    copy_loop_down(a_LSDptr,dest_LSDptr,len); # d := a
    do {
      var uintD digit = dest_LSDptr[-1]; # nächstes d[j]
      #if HAVE_DD
        digit = lowD(muluD(b0inv,digit));
      #else
        muluD(b0inv,digit, (void),digit=);
      #endif
      # digit = nächstes c[j]
      mulusub_loop_down(digit,b_LSDptr,dest_LSDptr,len); # d := d - b * c[j] * beta^j
      # Nun ist dest_LSDptr[-1] = 0.
      *--dest_LSDptr = digit; len--; # c[j] ablegen, nächstes j
    } until (len==0);
  }

