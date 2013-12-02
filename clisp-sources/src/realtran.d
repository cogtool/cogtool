# Transzendente Funktionen für reelle Zahlen

# pi_F_float_F(f) liefert die Zahl pi im selben Float-Format wie f.
# can trigger GC
  local maygc object pi_F_float_F (object f)
  {
    floatcase(f,
              { return O(SF_pi); },
              { return O(FF_pi); },
              { return O(DF_pi); },
              ;
             );
    var object pi = O(LF_pi);
    var uintC f_len = Lfloat_length(f); # gewünschte Länge von Pi
    var uintC oldlen = Lfloat_length(pi); # vorhandene Länge
    var uintC newlen;
    if (f_len < oldlen)
      return LF_shorten_LF(pi,f_len);
    if (f_len == oldlen)
      return pi;
    # Lfloat_length(O(LF_pi)) um mindestens einen konstanten Faktor
    # > 1 wachsen lassen, damit es nicht zu häufig nachberechnet wird:
    oldlen += floor(oldlen,2); # oldlen * 3/2
    newlen = (f_len < oldlen ? oldlen : f_len);
    # gewünschte > vorhandene Länge -> muss nachberechnen:
    # Methode:
    # [Richard P. Brent: Fast multiple-precision evaluation of elementary
    #  functions. J. ACM 23(1976), 242-251.]
    # d=f_len, n:=16*d. Verwende Long-Floats mit 16*(d+1) Mantissenbits.
    # (let* ((a (coerce 1 'long-float)) ; 1
    #        (b (sqrt (scale-float a -1))) ; 2^-(1/2)
    #        (eps (scale-float a (- n))) ; 2^-n
    #        (t (scale-float a -2)) ; 1/4
    #        (x 0)
    #       )
    #   (loop
    #     (when (< (- a b) eps) (return))
    #     (let ((y a))
    #       (setq a (scale-float (+ a b) -1))
    #       (setq b (sqrt (* b y)))
    #       (setq t (- t (scale-float (expt (- a y) 2) x)))
    #     )
    #     (incf x)
    #   )
    #   (/ (expt a 2) t)
    # )
    var uintC len = newlen + 1; # Arbeite mit Long-Floats mit len Digits
    if (uintWCoverflow(len)) { fehler_LF_toolong(); }
    var uintL uexp_limit = LF_exp_mid - intDsize*(uintL)newlen; # LF_exp_mid - n
    # Ein Long-Float ist genau dann betragsmäßig <2^-n, wenn
    # sein Exponent < LF_exp_mid-n = uexp_limit ist.
    {
      var object temp = I_to_LF(Fixnum_1,len,true); # 1 als Long-Float
      pushSTACK(temp); # =: a
      temp = LF_I_scale_float_LF(temp,Fixnum_minus1); # (scale-float a -1)
      pushSTACK(LF_sqrt_LF(temp)); # daraus die Wurzel, =: b
      pushSTACK(Fixnum_0); # x:=0
      temp = LF_I_scale_float_LF(STACK_2,sfixnum(-2)); # (scale-float a -2)
      pushSTACK(temp); # =: t
    }
    # Stackaufbau: a, b, x, t.
    loop {
      {
        var object temp;
        temp = LF_LF_minus_LF(STACK_3,STACK_2); # (- a b)
        if (TheLfloat(temp)->expo < uexp_limit) # Exponent < uexp_limit
          break; # ja -> |a-b| < 2^-n -> fertig
      }
      {
        var object temp;
        temp = LF_LF_plus_LF(STACK_3,STACK_2); # a+b
        temp = LF_I_scale_float_LF(temp,Fixnum_minus1); # (a+b)/2
        pushSTACK(temp); # neues a
      }
      STACK_(2+1) = LF_sqrt_LF(LF_LF_mal_LF(STACK_(3+1),STACK_(2+1))); # b := sqrt(a*b)
      {
        var object temp;
        temp = STACK_(3+1); # altes a
        temp = LF_LF_minus_LF(STACK_(3+1) = STACK_0, temp); # neues a - altes a
        temp = LF_square_LF(temp); # quadieren
        temp = LF_I_scale_float_LF(temp,STACK_(1+1)); # mal 2^x
        skipSTACK(1);
        STACK_0 = LF_LF_minus_LF(STACK_0,temp); # von t subtrahieren
        STACK_1 = fixnum_inc(STACK_1,1); # x:=x+1
      }
    }
    {
      var object temp;
      temp = LF_square_LF(STACK_3); # a quadieren
      temp = LF_LF_durch_LF(temp,STACK_0); # durch t dividieren
      skipSTACK(4);
      # temp = Pi ist fertig.
      temp = O(LF_pi) = LF_shorten_LF(temp,newlen); # wieder verkürzen, als LF_pi abspeichern
      return (f_len < newlen ? LF_shorten_LF(temp,f_len) : temp);
    }
  }

# pi(x) returns the number pi in default-float-format or the format of x
# can trigger GC
local maygc object pi (object x) {
  defaultfloatcase(S(default_float_format),x,
                   return O(SF_pi), # pi as SF
                   return O(FF_pi), # pi as FF
                   return O(DF_pi), # pi as DF
                   return pi_F_float_F(x), # pi as LF in the same format as x
                   ,); # nothing to save
}

# F_atanhx_F(x) liefert zu einem Float x (betragsmäßig <1/2) atanh(x) als Float.
# can trigger GC
  local maygc object F_atanhx_F (object x);
# Methode:
# e := Exponent aus (decode-float x), d := (float-digits x)
# Bei x=0.0 oder e<=-d/2 liefere x
#   (denn bei e<=-d/2 ist x^2 < 2^(-d), also
#   1 <= atanh(x)/x = 1+x^2/3+x^4/5+... < 1+x^2/2 < 1+2^(-d-1) < 1+2^(-d),
#   also ist atanh(x)/x, auf d Bits gerundet, gleich 1.0).
# Bei e<=-sqrt(d) verwende die Potenzreihe
#   atanh(x)/x = sum(j=0..inf,(x^2)^j/(2j+1)):
#   a:=x^2, b:=1, i:=1, sum:=0,
#   while (/= sum (setq sum (+ sum (/ b i)))) do i:=i+2, b:=b*a.
#   Ergebnis x*sum.
# Sonst setze y := x/(1+sqrt(1-x^2)), berechne rekursiv z:=atanh(y)
#   und liefere 2*z = (scale-float z 1).
# Diese Rekursion wird entrekursiviert. Statt k mal hintereinander
#   x := x/(1+sqrt(1-x^2)) zu bilden, arbeitet man lieber mit den Kehrwerten,
#   setzt also x := 1/|x|, dann k mal x := x+sqrt(x^2-1), dann x := +- 1/x.
# Aufwand: asymptotisch d^2.5 .

# F_atanx_F(x) liefert zu einem Float x (betragsmäßig <=1) atan(x) als Float.
# can trigger GC
  local maygc object F_atanx_F (object x);
# Methode:
# e := Exponent aus (decode-float x), d := (float-digits x)
# Bei x=0.0 oder e<=-d/2 liefere x
#   (denn bei e<=-d/2 ist x^2/3 < x^2/2 < 2^(-d)/2 = 2^(-d-1), also
#   1 >= atan(x)/x > 1-x^2/3 > 1-2^(-d-1),
#   also ist atan(x)/x, auf d Bits gerundet, gleich 1.0).
# Bei e<=-sqrt(d) verwende die Potenzreihe
#   atan(x)/x = sum(j=0..inf,(-x^2)^j/(2j+1)):
#   a:=-x^2, b:=1, i:=1, sum:=0,
#   while (/= sum (setq sum (+ sum (/ b i)))) do i:=i+2, b:=b*a.
#   Ergebnis x*sum.
# Sonst setze y := x/(1+sqrt(1+x^2)), berechne rekursiv z:=atan(y)
#   und liefere 2*z = (scale-float z 1).
# Diese Rekursion wird entrekursiviert. Statt k mal hintereinander
#   x := x/(1+sqrt(1+x^2)) zu bilden, arbeitet man lieber mit den Kehrwerten,
#   setzt also x := 1/|x|, dann k mal x := x+sqrt(x^2+1), dann x := +- 1/x.
# Aufwand: asymptotisch d^2.5 .

# Generiert eine Funktion wie F_atanx_F
  #define GEN_F_atanx(name,Fixnum_plusminus1,F_plusminus_F)                         \
    local maygc object CONCAT3(F_,name,_F) (object x)                               \
      { GCTRIGGER1(x);                                                              \
        if (R_zerop(x))                                                             \
          return x;                                                                 \
       {var uintL d = F_float_digits(x);                                            \
        var sintL e = F_exponent_L(x);                                              \
        if (e <= (sintL)(-d)>>1) # e <= -d/2 <==> e <= -ceiling(d/2)                \
          return x; # ja -> x als Ergebnis                                          \
        pushSTACK(x);                                                               \
        # Stackaufbau: x.                                                           \
        {var object k = Fixnum_0; # Rekursionszähler k:=0                           \
         var uintL sqrt_d = UL_sqrt_UW(d); # floor(sqrt(d))                         \
         # Bei e <= -1-floor(sqrt(d)) kann die Potenzreihe angewandt werden.        \
         if (e >= (sintL)(-sqrt_d)) {                                               \
           # e > -1-floor(sqrt(d)) -> muss |x| verkleinern.                         \
           var sintL e_limit = 1+sqrt_d; # 1+floor(sqrt(d))                         \
           pushSTACK(x = F_durch_F(F_abs_F(x))); # 1/|x|                            \
           # Stackaufbau: originales x, neues x.                                    \
           loop {                                                                   \
             # nächstes x nach der Formel x := x+sqrt(x^2 +- 1) berechnen:          \
             x = F_sqrt_F(R_R_plus_R(F_square_F(x),Fixnum_plusminus1));             \
             STACK_0 = x = F_F_plus_F(STACK_0,x);                                   \
             k = fixnum_inc(k,1); # k:=k+1                                          \
             if (F_exponent_L(x) > e_limit)                                         \
               break;                                                               \
           }                                                                        \
           # Schleifenende mit Exponent(x) > 1+floor(sqrt(d)), also                 \
           # x >= 2^(1+floor(sqrt(d))), also 1/x <= 2^(-1-floor(sqrt(d))).          \
           # Nun kann die Potenzreihe auf 1/x angewandt werden.                     \
           {var object x = F_durch_F(popSTACK());                                   \
            if (R_minusp(STACK_0)) { x = F_minus_F(x); } # Vorzeichen wieder rein   \
            STACK_0 = x; # neues x ersetzt altes x                                  \
         } }                                                                        \
         # Stackaufbau: neues x.                                                    \
         # Potenzreihe anwenden:                                                    \
         {var object i = Fixnum_1;                                                  \
          pushSTACK(F_plusminus_F(F_square_F(STACK_0))); # a := -x^2 bzw. x^2       \
          pushSTACK(I_F_float_F(Fixnum_1,STACK_1)); # b := (float 1 x)              \
          pushSTACK(I_F_float_F(Fixnum_0,STACK_2)); # sum := (float 0 x)            \
          # Stackaufbau: x, a, b, sum.                                              \
          loop {                                                                    \
            var object temp;                                                        \
            temp = R_R_durch_R(STACK_1,i); # (/ b i)                                \
            temp = F_F_plus_F(STACK_0,temp); # (+ sum (/ b i))                      \
            if (eql(STACK_0,temp)) # = sum ?                                        \
              break; # ja -> Potenzreihe abbrechen                                  \
            STACK_0 = temp;                                                         \
            STACK_1 = F_F_mal_F(STACK_1,STACK_2); # b := b*a                        \
            i = fixnum_inc(i,2); # i := i+2                                         \
          }                                                                         \
         }                                                                          \
         {var object erg = F_F_mal_F(STACK_0,STACK_3); # sum*x als Ergebnis         \
          skipSTACK(4);                                                             \
          return F_I_scale_float_F(erg,k); # wegen Rekursion noch mal 2^k           \
      }}}}
# F_atanx_F : mit x -> x+sqrt(x^2-1), a = -x^2
  GEN_F_atanx(atanx,Fixnum_1,F_minus_F)
# F_atanhx_F : mit x -> x+sqrt(x^2+1), a = x^2
  GEN_F_atanx(atanhx,Fixnum_minus1,)

# R_R_atan_R(x,y) liefert zu zwei reellen Zahlen x, y den Winkel von (x,y)
# in Polarkoordinaten. Ergebnis rational nur, wenn x>0 und y=0.
# can trigger GC
  local maygc object R_R_atan_R (object x, object y);
# Methode:
# y=0 -> bei x>0: 0 als Ergebnis,
#        bei x<0: pi als Ergebnis.
#        bei x=0: Error.
# x=0 -> bei y>0: pi/2 als Ergebnis.
#        bei y<0: -pi/2 als Ergebnis.
#        bei y=0: Error.
# Falls x und y beide rational: beide in Floats umwandeln.
# 0 <= |y| <= x  ->  atan(y/x)
# 0 <= |x| <= y  ->  pi/2 - atan(x/y)
# 0 <= |x| <= -y  ->  -pi/2 - atan(x/y)
# 0 <= |y| <= -x  ->  für y>=0: pi + atan(y/x), für y<0: -pi + atan(y/x)
  local maygc object R_R_atan_R (object x, object y)
  {
    if (eq(y,Fixnum_0)) {
      # y=0 (exakt)
      if (R_zerop(x)) # x=0 -> Error
        divide_0();
      if (R_minusp(x)) # x<0 -> pi in default-float-format
        return pi(x);
      else { # x>0 -> 0
        if (R_floatp(x))
          return RA_F_exact_contagion_R(Fixnum_0,x);
        else
          return Fixnum_0;
      }
    } elif (eq(x,Fixnum_0)) {
      # x=0 (exakt)
      if (R_zerop(y)) # y=0 -> Error
        divide_0();
      if (R_minusp(y)) # y<0 -> -pi/2
        return F_minus_F(F_I_scale_float_F(pi(y),Fixnum_minus1));
      else # y>0 -> pi/2
        return F_I_scale_float_F(pi(y),Fixnum_minus1);
    }
    pushSTACK(x); pushSTACK(y);
    # Stackaufbau: x, y.
    if (R_rationalp(x) && R_rationalp(y)) {
      # x,y in Floats umwandeln:
      STACK_1 = RA_float_F(x); STACK_0 = RA_float_F(STACK_0);
    }
    # x,y nicht exakt =0, x/y und y/x werden Floats sein.
    pushSTACK(R_abs_R(STACK_1)); y = R_abs_R(STACK_(0+1)); x = popSTACK();
    if (R_R_comp(x,y) >= 0) { # (abs x) und (abs y) vergleichen
      # |x| >= |y|
      var object z = F_atanx_F(R_R_durch_R(STACK_0,STACK_1)); # atan(y/x)
      # Division war erfolgreich, also x/=0.
      if (R_minusp(STACK_1)) {
        # x<0 -> pi bzw. -pi addieren:
        STACK_1 = z; # atan(y/x) retten
        z = pi_F_float_F(z); # pi im selben Float-Format
        if (!R_minusp(STACK_0))
          z = F_F_plus_F(STACK_1,z); # y>=0 -> atan(y/x) + pi
        else
          z = F_F_minus_F(STACK_1,z); # y<0 -> atan(y/x) - pi
      }
      skipSTACK(2);
      return z;
    } else {
      # |x| < |y|
      var object z = F_atanx_F(R_R_durch_R(STACK_1,STACK_0)); # atan(x/y)
      # von pi/2 bzw. -pi/2 subtrahieren:
      STACK_1 = z; # atan(x/y) retten
      z = pi_F_float_F(z); # pi im selben Float-Format
      z = F_I_scale_float_F(z,Fixnum_minus1); # pi/2
      if (R_minusp(STACK_0)) # y<0 -> -pi/2 statt pi/2
        z = F_minus_F(z);
      z = F_F_minus_F(z,STACK_1); # +-pi/2 - atan(x/y)
      skipSTACK(2);
      return z;
    }
  }

# R_atan_R(x) liefert den Arctan einer reellen Zahl x.
# Ergebnis rational nur, wenn x=0.
# can trigger GC
  local maygc object R_atan_R (object x);
# Methode:
# arctan(x) = arctan(X=1,Y=x).
#if 0
  local maygc object R_atan_R (object x)
  { return R_R_atan_R(Fixnum_1,x); }
#else # Macro spart Code
  #define R_atan_R(x)  R_R_atan_R(Fixnum_1,x)
#endif

# F_sinx_F(x) liefert zu einem Float x (betragsmäßig <2) (sin(x)/x)^2 als Float.
# can trigger GC
  local maygc object F_sinx_F (object x);
# Methode:
# e := Exponent aus (decode-float x), d := (float-digits x)
# Bei x=0.0 oder e<=-d/2 liefere 1.0
#   (denn bei e<=-d/2 ist x^2/6 < x^2/4 < 2^(-d)/4 = 2^(-d-2), also
#   1 >= sin(x)/x > 1-x^2/6 > 1-2^(-d-2), also 1 >= (sin(x)/x)^2 > 1-2^(-d-1),
#   also ist (sin(x)/x)^2, auf d Bits gerundet, gleich 1.0).
# Bei e<=-sqrt(d) verwende die Potenzreihe
#   sin(x)/x = sum(j=0..inf,(-x^2)^j/(2j+1)!):
#   a:=-x^2, b:=1, i:=1, sum:=0,
#   while (/= sum (setq sum (+ sum b))) do b:=b*a/((i+1)*(i+2)), i:=i+2.
#   Ergebnis sum^2.
# Sonst setze y := x/2 = (scale-float x -1),
#   berechne rekursiv z:=(sin(y)/y)^2 und liefere z*(1-y^2*z).
# [Die Grenze sqrt(d) ergibt sich so:
#  Man braucht bei der Potenzreihe mit x=2^-k etwa j Glieder, mit
#  k*j*ln 2 + j*(ln j - 1) = d, und der Aufwand beträgt etwa 2.8*(j/2)
#  Multiplikationen von d-Bit-Zahlen. Bei Halbierungen bis x=2^-k ist der
#  Gesamtaufwand etwa 2*(k+e)+1.4*j(k). Dieses minimieren nach k: Soll sein
#  -1.4 = d/dk j(k) = (d/dj k(j))^-1 = - j^2/(d+j)*ln 2, also j^2=2(d+j),
#  grob j=sqrt(2d) und damit k=sqrt(d).]
# Aufwand: asymptotisch d^2.5 .

# F_sinhx_F(x) liefert zu einem Float x (betragsmäßig <2) (sinh(x)/x)^2 als Float.
# can trigger GC
  local maygc object F_sinhx_F (object x);
# Methode:
# e := Exponent aus (decode-float x), d := (float-digits x)
# Bei x=0.0 oder e<=(1-d)/2 liefere 1.0
#   (denn bei e<=(1-d)/2 ist x^2/6 < x^2/4 < 2^(1-d)/4 = 2^(-d-1), also
#   1 <= sinh(x)/x = 1+x^2/6+... < 1+2^(-d-1), also 1 <= (sinh(x)/x)^2 < 1+2^(-d),
#   also ist (sinh(x)/x)^2, auf d Bits gerundet, gleich 1.0).
# Bei e<=-sqrt(d) verwende die Potenzreihe
#   sinh(x)/x = sum(j=0..inf,(x^2)^j/(2j+1)!):
#   a:=x^2, b:=1, i:=1, sum:=0,
#   while (/= sum (setq sum (+ sum b))) do b:=b*a/((i+1)*(i+2)), i:=i+2.
#   Ergebnis sum^2.
# Sonst setze y := x/2 = (scale-float x -1),
#   berechne rekursiv z:=(sinh(y)/y)^2 und liefere z*(1+y^2*z).
# [Die Grenze sqrt(d) ergibt sich so:
#  Man braucht bei der Potenzreihe mit x=2^-k etwa j Glieder, mit
#  k*j*ln 2 + j*(ln j - 1) = d, und der Aufwand beträgt etwa 2.8*(j/2)
#  Multiplikationen von d-Bit-Zahlen. Bei Halbierungen bis x=2^-k ist der
#  Gesamtaufwand etwa 2*(k+e)+1.4*j(k). Dieses minimieren nach k: Soll sein
#  -1.4 = d/dk j(k) = (d/dj k(j))^-1 = - j^2/(d+j)*ln 2, also j^2=2(d+j),
#  grob j=sqrt(2d) und damit k=sqrt(d).]
# Aufwand: asymptotisch d^2.5 .

# Generiert eine Funktion wie F_sinx_F
  #define GEN_F_sinx(name,f,flag,R_R_plusminus_R)                              \
    local maygc object CONCAT3(F_,name,_F) (object x)                          \
      { GCTRIGGER1(x);                                                         \
        if (R_zerop(x))                                                        \
          return I_F_float_F(Fixnum_1,x);                                      \
       {var uintL d = F_float_digits(x);                                       \
        var sintL e = F_exponent_L(x);                                         \
        if (e <= (sintL)(f-d)>>1) # e <= (f-d)/2 <==> e <= -ceiling((d-f)/2) ? \
          return I_F_float_F(Fixnum_1,x); # ja -> 1.0 als Ergebnis             \
        pushSTACK(x);                                                          \
        {# Bei e <= -1-floor(sqrt(d)) kann die Potenzreihe angewandt werden.   \
         var sintL e_limit = -1-UL_sqrt_UW(d); # -1-floor(sqrt(d))             \
         if (e > e_limit) {                                                    \
           # e > -1-floor(sqrt(d)) -> muss |x| verkleinern.                    \
           x = I_I_minus_I(L_to_FN(e_limit),L_to_I(e));                        \
           STACK_0 = F_I_scale_float_F(STACK_0,x); # x := x*2^(e_limit-e)      \
         }                                                                     \
         x = STACK_0; pushSTACK(F_square_F(x));                                \
         # Stackaufbau: x, x^2.                                                \
         # Potenzreihe anwenden:                                               \
         pushSTACK(STACK_0);                                                   \
         if (flag) { STACK_0 = F_minus_F(STACK_0); } # a := -x^2 bzw. x^2      \
         {var object i = Fixnum_1;                                             \
          pushSTACK(I_F_float_F(Fixnum_1,STACK_2)); # b := (float 1 x)         \
          pushSTACK(I_F_float_F(Fixnum_0,STACK_3)); # sum := (float 0 x)       \
          # Stackaufbau: x, x^2, a, b, sum.                                    \
          loop {                                                               \
            var object temp;                                                   \
            temp = F_F_plus_F(STACK_0,STACK_1); # (+ sum b)                    \
            if (eql(STACK_0,temp)) # = sum ?                                   \
              break; # ja -> Potenzreihe abbrechen                             \
            STACK_0 = temp;                                                    \
            STACK_1 = F_F_mal_F(STACK_1,STACK_2); # b := b*a                   \
            temp = I_I_mal_I(fixnum_inc(i,1),fixnum_inc(i,2)); # (i+1)*(i+2)   \
            i = fixnum_inc(i,2); # i := i+2                                    \
            STACK_1 = R_R_durch_R(STACK_1,temp); # b := b/((i+1)*(i+2))        \
          }                                                                    \
         }                                                                     \
         {var object z = F_square_F(STACK_0); # sum^2 als Ergebnis             \
          # Stackaufbau: x, x^2, -, -, -.                                      \
          # Wegen Rekursion noch max(e-e_limit,0) mal z verändern:             \
          if (e > e_limit) {                                                   \
            STACK_4 = z; # z retten                                            \
            do {                                                               \
              z = R_R_plusminus_R(Fixnum_1,F_F_mal_F(STACK_3,z)); # 1 +- x^2*z \
              STACK_4 = F_F_mal_F(STACK_4,z); # mit z multiplizieren           \
              STACK_3 = F_I_scale_float_F(STACK_3,fixnum(2)); # x^2 := x^2*4   \
              z = STACK_4;                                                     \
              e_limit++;                                                       \
            } while (e > e_limit);                                             \
          }                                                                    \
          skipSTACK(5);                                                        \
          return z;                                                            \
      }}}}
# F_sinx_F : mit z -> z*(1-y^2*z), a = -x^2, -d/2
  GEN_F_sinx(sinx,0,true,R_R_minus_R)
# F_sinhx_F : mit z -> z*(1+y^2*z), a = x^2, (1-d)/2
  GEN_F_sinx(sinhx,1,false,R_R_plus_R)

/* F_pi2_round_I_F(x) divides a float x by pi/2.
 both values of (round x (float pi/2 x)) a pushed to the STACK.
 can trigger GC */
local maygc void F_pi2_round_I_F (object x)
{
  if (F_exponent_L(x) < 0) {
    /* Exponent <0 -> |x|<1/2 -> |x/(pi/2)| < 1/2, ==> no division necessary */
    pushSTACK(Fixnum_0); pushSTACK(x); /* quotient 0, remainder x */
  } else {
    pushSTACK(x); /* save x */
   {var object pi = pi_F_float_F(x); /* pi with the appropriate precision */
    pi = F_I_scale_float_F(pi,Fixnum_minus1); /* pi/2 with same precision */
    R_R_round_I_R(popSTACK(),pi); /* divide x by pi/2 */
    x = STACK_1; /* q mod 4 whether q is Fixnum or Bignum: */
   {var uintC m = I_fixnump(x) ? (as_oint(x) >> oint_data_shift) & (bit(2)-1)
     : TheBignum(x)->data[(uintP)Bignum_length(x)-1] & (bit(2)-1);
    STACK_1 = fixnum(m);
   }}}
}

/* compute the sin(r=STACK_0) with precision of STACK_2 */
local maygc object sin_stack (void)
{
  var object x = F_sqrt_F(F_sinx_F(STACK_0)); /* sin(r)/r */
  x = F_F_mal_F(x,STACK_0); /* sin(r) = (sin(r)/r) * r */
  return F_F_float_F(x,STACK_2); /* round */
}

/* compute the cos(r=STACK_0) with precision of STACK_2 */
local maygc object cos_stack (void)
{
  var object s = F_I_scale_float_F(STACK_0,Fixnum_minus1); /* s := r/2 */
  pushSTACK(s);
  s = F_sinx_F(s); /* (sin(s)/s)^2 */
  s = F_F_mal_F(popSTACK(),s); /* (sin(s)/s)^2 * s = sin(s)^2/s */
  s = F_F_mal_F(STACK_0,s); /* sin(s)^2/s * r = 2*sin(s)^2 */
  s = R_R_minus_R(Fixnum_1,s); /* 1 - 2*sin(s)^2 = cos(r) */
  return F_F_float_F(s,STACK_2); /* round */
}

/* R_sin_R(x) returns the sinus (sin x) for a real number x.
 can trigger GC
 Method:
 x rational -> if x=0 return 0, otherwise convert x to a float.
 x float -> increase precision
   (q,r) := (round x (float pi/2 x)), so that |r|<=pi/4.
   if r~0, return +-1 or +-r depending on q, otherwise
   use +-sin_stack() or +-cos_stack() depending on q */
local maygc object R_sin_R (object x)
{
  if (R_rationalp(x)) {
    if (eq(x,Fixnum_0)) { return x; } /* x=0 -> return 0 */
    x = RA_float_F(x); /* convert to a float */
  }
  /* x Float */
  pushSTACK(x); /* save x */
  x = F_extend_F(x); /* increase precision */
  F_pi2_round_I_F(x); /* divide by pi/2 */
  /* stack layout: Argument, q mod 4, r. */
 {var object x = STACK_0;
  var uintC mod4 = posfixnum_to_V(STACK_1); /* q mod 4 */
  if (R_zerop(x) /* r=0.0 -> 1.0 */
      || (F_exponent_L(x) <= (sintL)(-F_float_digits(x))>>1)) /* e <= -d/2 <==> e <= -ceiling(d/2) ? */
    switch (mod4) {
      case 0: x = F_F_float_F(STACK_0,STACK_2); /* (sin x) = r */ break;
      case 1: x = I_F_float_F(Fixnum_1,STACK_2); /* (sin x) = 1 */ break;
      case 2: { /* sin(x) = -r */
        var object minus_r = F_minus_F(STACK_0);
        x = F_F_float_F(minus_r,STACK_2);
        break;
      }
      case 3: x = I_F_float_F(Fixnum_minus1,STACK_2); /* (sin x) = -1 */ break;
    }
  else
    switch (mod4) {
      case 0: x = sin_stack(); /* (sin x) = (sin r) */ break;
      case 1: x = cos_stack(); /* (sin x) = (cos r) */ break;
      case 2: x = F_minus_F(sin_stack()); /* (sin x) = (- (sin r)) */ break;
      case 3: x = F_minus_F(cos_stack()); /* (sin x) = (- (cos r)) */ break;
    }
  skipSTACK(3);
  return x;
 }
}

/* R_cos_R(x) compute the cosinus (cos x) of a real number x.
 can trigger GC
 Method:
 x rational -> if x=0 return 1, otherwise convert x to a float.
 x float -> increase precision,
   (q,r) := (round x (float pi/2 x)), so that |r|<=pi/4.
   if r~0, return +-1 or +-r depending on q, otherwise
   use +-sin_stack() or +-cos_stack() depending on q */
local maygc object R_cos_R (object x)
{
  if (R_rationalp(x)) {
    if (eq(x,Fixnum_0)) { return Fixnum_1; } /* x=0 -> return 1 */
    x = RA_float_F(x); /* convert to a float */
  }
  /* x Float */
  pushSTACK(x); /* save x */
  x = F_extend_F(x); /* increase precision */
  F_pi2_round_I_F(x); /* divide by pi/2 */
  /* stack layout: Argument, q mod 4, r. */
 {var object x = STACK_0;
  var uintC mod4 = posfixnum_to_V(STACK_1); /* q mod 4 */
  if (R_zerop(x) /* r=0.0 -> 1.0 */
      || (F_exponent_L(x) <= (sintL)(-F_float_digits(x))>>1)) /* e <= -d/2 <==> e <= -ceiling(d/2) ? */
    switch (mod4) {
      case 0: x = I_F_float_F(Fixnum_1,STACK_2); /* (cos x) = 1.0 */ break;
      case 1: {/* cos(x) = -r */
        var object minus_r = F_minus_F(STACK_0);
        x = F_F_float_F(minus_r,STACK_2);
        break;
      }
      case 2: x = I_F_float_F(Fixnum_minus1,STACK_2); /* (cos x) = -1 */ break;
      case 3: x = F_F_float_F(STACK_0,STACK_2); /* (cos x) = r */ break;
    }
  else
    switch (mod4) {
      case 0: x = cos_stack(); /* (cos x) = (cos r) */ break;
      case 1: x = F_minus_F(sin_stack()); /* (cos x) = (- (sin r)) */ break;
      case 2: x = F_minus_F(cos_stack()); /* (cos x) = (- (cos r)) */ break;
      case 3: x = sin_stack(); /* (cos x) = (sin r) */ break;
    }
  skipSTACK(3);
  return x;
 }
}

/* R_cos_sin_R_R(x) places ((cos x),(sin x)), on the Stack.
   when start_p, this is a start of a computation,
    so the precision will be raised
   when end_p, this is also the end of the computation,
    so the precision will be lowed back to this number
 can trigger GC
 Method:
 x rational -> if x=0 ==> (1,0), otherwise x ==> float.
 x float -> increase its precision,
   (q,r) := (round x (float pi/2 x)), so that |r|<=pi/4.
   e := the exponent from (decode-float r), d := (float-digits r)
  if r=0.0 or e<=-d/2 then return (1.0 0.0)
       (since e<=-d/2 , r^2/2 < 2^(-d)/2 = 2^(-d-1), thus
        1 >= cos(r) > 1-r^2/2 > 1-2^(-d-1),
        this cos(r), up to d bits, == 1.0
        similarly sin(r) = r + O(r^3)).
  else
   y:=(sin(r/2)/(r/2))^2.
   (cos r) = 1-r^2*y/2.
   (sin r) = r*sqrt(y*(1-y*(r/2)^2)).
   reduce the precision
   if q = 0 mod 4: ((cos r), (sin r))
   if q = 1 mod 4: ((- (sin r)), (cos r))
   if q = 2 mod 4: ((- (cos r)), (- (sin r)))
   if q = 3 mod 4: ((sin r), (- (cos r))) */
local maygc void R_cos_sin_R_R (object x, bool start_p, gcv_object_t *end_p)
{
  if (R_rationalp(x)) {
    if (eq(x,Fixnum_0)) # x=0 -> return (1,0)
      { pushSTACK(Fixnum_1); pushSTACK(Fixnum_0); return; }
    x = RA_float_F(x); /* otherwise turn into a float */
  }
  /* x -- float */
  pushSTACK(x); /* save x */
  if (start_p) /* increase computational precision */
    x = F_extend_F(x);
  F_pi2_round_I_F(x); /* divide by pi/2 */
  /* stack layout: Argument, q mod 4, r. */
  x = STACK_0;
  if (R_zerop(x) /* r=0.0 -> cos=1.0+O(r^2), sin=r+O(r^3) */
      || (F_exponent_L(x) <= (sintL)(-F_float_digits(x))>>1)) { /* e <= -d/2 <==> e <= -ceiling(d/2) ? */
    if (end_p != NULL) {
      pushSTACK(RA_R_float_F(Fixnum_1,*end_p)); /* cos=1 */
      pushSTACK(F_R_float_F(STACK_1,*end_p)); /* sin=r */
    } else {
      pushSTACK(I_F_float_F(Fixnum_1,STACK_0)); /* cos=1 */
      pushSTACK(STACK_1); /* sin=r */
    }
  } else {
    pushSTACK(F_I_scale_float_F(STACK_0,Fixnum_minus1)); /* s := r/2 */
    pushSTACK(F_sinx_F(STACK_0)); /* y := (sin(s)/s)^2 */
    /* Stack layout: Argument, q mod 4, r, s, y. */
    x = F_F_mal_F(STACK_0,STACK_1); /* y*s */
    x = F_F_mal_F(STACK_2,x); /* y*s*r */
    x = R_R_minus_R(Fixnum_1,x); /* 1-y*s*r */
    pushSTACK(end_p != NULL ? F_R_float_F(x,*end_p) : x); /* round and save cos(r) */
    x = F_F_mal_F(STACK_1,STACK_2); /* y*s */
    x = F_F_mal_F(x,STACK_2); /* y*s*s */
    x = R_R_minus_R(Fixnum_1,x); /* 1-y*s*s = cos(s)^2 */
    x = F_F_mal_F(x,STACK_1); /* cos(s)^2*(sin(s)/s)^2 */
    x = F_sqrt_F(x); /* cos(s)*sin(s)/s */
    x = F_F_mal_F(x,STACK_2); /* cos(s)*sin(s) */
    x = F_I_scale_float_F(x,Fixnum_1); /* 2*cos(s)*sin(s) = sin(r) */
    if (end_p != NULL) /* round sin(r) */
      x = F_R_float_F(x,*end_p);
    STACK_2 = STACK_0;
    STACK_1 = x;
    skipSTACK(1);
  } /* stack layout: argument, q mod 4, r, cos(r), sin(r) */
  { /* compute sign */
    var uintC q = posfixnum_to_V(STACK_3);
    switch (q) { /* q mod 4 whether q is Fixnum or Bignum */
      case 0:
        STACK_(2+1) = STACK_0; STACK_(3+1) = STACK_1; break;
      case 1:
        STACK_(3+1) = F_minus_F(STACK_0); STACK_(2+1) = STACK_1; break;
      case 2:
        STACK_(2+1) = F_minus_F(STACK_0);
        STACK_(3+1) = F_minus_F(STACK_1); break;
      case 3:
        STACK_(3+1) = STACK_0; STACK_(2+1) = F_minus_F(STACK_1); break;
    }
  }
  skipSTACK(2+1);
  return;
}

/* R_tan_R(x) compute the tangens (tan x) of a real number x.
 can trigger GC
 Method:
 (/ (sin x) (cos x)) */
local maygc object R_tan_R (object x)
{
  pushSTACK(x);
  R_cos_sin_R_R(x,true,NULL);
  /* stack layout: x, cos(x), sin(x). */
  var object result = R_R_durch_R(STACK_0,STACK_1);
  if (floatp(STACK_0) || floatp(STACK_1))
    result = F_R_float_F(result,STACK_2); /* reduce precision */
  skipSTACK(3); return result;
}

# F_lnx_F(x) liefert zu einem Float x (>=1/2, <=2) ln(x) als Float.
# can trigger GC
  local maygc object F_lnx_F (object x);
# Methode:
# y:=x-1, e := Exponent aus (decode-float y), d := (float-digits y)
# Bei y=0.0 oder e<=-d liefere y
#   (denn bei e<=-d ist y/2 < 2^(-d)/2 = 2^(-d-1), also
#   0 <= y - ln(x) < y^2/2 < 2^(-d-1)*y
#   also ist ln(x)/y, auf d Bits gerundet, gleich y).
# Bei e<=-sqrt(d) verwende die Potenzreihe
#   ln(x) = sum(j=0..inf,(-1)^j*y^(j+1)/(j+1)):
#   a:=-y, b:=y, i:=1, sum:=0,
#   while (/= sum (setq sum (+ sum (/ b i)))) do i:=i+1, b:=b*a.
#   Ergebnis sum.
# Sonst setze y := sqrt(x), berechne rekursiv z:=ln(y)
#   und liefere 2*z = (scale-float z 1).
# Aufwand: asymptotisch d^2.5 .
  local maygc object F_lnx_F (object x)
  {
    pushSTACK(x);
    x = R_R_minus_R(x,Fixnum_1); # y := (- x 1)
    if (R_zerop(x)) { # y=0.0 -> y als Ergebnis
      skipSTACK(1); return x;
    }
    pushSTACK(x);
    # Stackaufbau: x, y.
    var uintL d = F_float_digits(x);
    var sintL e = F_exponent_L(x);
    if (e <= (sintL)(-d)) { # e <= -d ?
      x = STACK_0; skipSTACK(2); return x; # ja -> y als Ergebnis
    }
    var object k = Fixnum_0; # Rekursionszähler k:=0
    { # Bei e <= -1-floor(sqrt(d)) kann die Potenzreihe angewandt werden.
      var sintL e_limit = -1-UL_sqrt_UW(d); # -1-floor(sqrt(d))
      while (e > e_limit) {
        # e > -1-floor(sqrt(d)) -> muss |y| verkleinern.
        var object x = F_sqrt_F(STACK_1); STACK_1 = x; # x := (sqrt x)
        x = R_R_minus_R(x,Fixnum_1); STACK_0 = x; # y := (- x 1) und
        e = F_exponent_L(x); # e neu berechnen
        k = fixnum_inc(k,1); # k:=k+1
      }
    }
    # Stackaufbau: x, y.
    # Potenzreihe anwenden:
    {
      var object i = Fixnum_1;
      pushSTACK(I_F_float_F(Fixnum_0,STACK_1)); # sum := (float 0 x)
      STACK_2 = F_minus_F(STACK_1); # a := -y, b := y
      # Stackaufbau: a, b, sum.
      loop {
        var object temp;
        temp = R_R_durch_R(STACK_1,i); # (/ b i)
        temp = F_F_plus_F(STACK_0,temp); # (+ sum (/ b i))
        if (eql(STACK_0,temp)) # = sum ?
          break; # ja -> Potenzreihe abbrechen
        STACK_0 = temp;
        STACK_1 = F_F_mal_F(STACK_1,STACK_2); # b := b*a
        i = fixnum_inc(i,1); # i := i+1
      }
    }
    var object erg = STACK_0; # sum als Ergebnis
    skipSTACK(3);
    return F_I_scale_float_F(erg,k); # wegen Rekursion noch mal 2^k
  }

# ln2_F_float_F(f) liefert die Zahl ln(2) im selben Float-Format wie f.
# can trigger GC
  local maygc object ln2_F_float_F (object f)
  {
    var object ln2 = O(LF_ln2);
    floatcase(f,
              { return LF_to_SF(ln2); },
              { return LF_to_FF(ln2); },
              { return LF_to_DF(ln2); },
              ;
             );
    var uintC f_len = Lfloat_length(f); # gewünschte Länge von ln(2)
    {
      var uintC len = Lfloat_length(ln2); # vorhandene Länge
      if (f_len < len)
        return LF_shorten_LF(ln2,f_len);
      if (f_len == len)
        return ln2;
    }
    # gewünschte > vorhandene Länge -> muss nachberechnen:
    {
      var uintC len = lf_len_extend(f_len); # einige Digits mehr verlangen
      var object temp = F_lnx_F(I_to_LF(fixnum(2),len,true)); # (ln 2.0)
      # temp = ln(2) ist fertig.
      return O(LF_ln2) = LF_shorten_LF(temp,f_len); # wieder verkürzen, als LF_ln2 abspeichern
    }
  }

# Vergrößert eine Long-Float-Länge n, so dass aus d = intDsize*n
# mindestens d+sqrt(d)+2+(LF_exp_len-1) wird.
# Allgemein: intDsize*n + sqrt(intDsize*n) + 2 + 31 < intDsize*(n+inc)
# <==>       sqrt(intDsize*n) + 33 < intDsize*inc
# <==>       sqrt(intDsize*n) < intDsize*inc - 33
# <==>       intDsize*n < intDsize^2*inc^2 - 66*intDsize*inc + 1089
# <==>       n <= intDsize*inc^2 - 66*inc + floor(1089/intDsize)
  local uintC lf_len_extend2 (uintC n)
  {
    var uintC inc =
      #define FITS(n,k)  \
        ((intDsize*(k) > 33)                                              \
         && ((n) <= (uintL)((intDsize*(k)-66)*(k)+floor(1089,intDsize))))
      #define n_max  (uintL)(bitm(intWCsize)-1)
      #define TEST(i)  FITS(n_max,1UL<<i) || FITS(n,1UL<<i) ? 1UL<<i :
      TEST(0) TEST(1) TEST(2) TEST(3) TEST(4) TEST(5) TEST(6) TEST(7)
      TEST(8) TEST(9) TEST(10) TEST(11) TEST(12) TEST(13)
      (fehler_LF_toolong(),0);
      #undef TEST
      #undef n_max
      #undef FITS
    if ((uintWC)(n = n+inc) < (uintWC)inc) fehler_LF_toolong();
    return n;
  }

# F_extend2_F(x) erweitert die Genauigkeit eines Floats x um eine Stufe
# SF -> FF -> DF -> LF(4) -> LF(5) -> LF(6) -> ...
# Ein Float mit d Mantissenbits und l Exponentenbits wird so zu einem Float
# mit mindestens d+sqrt(d)+2+(l-1) Mantissenbits.
# SF -> DF wegen 17+sqrt(17)+2+7 = 30.2 < 53
# FF -> DF wegen 24+sqrt(24)+2+7 = 37.9 < 53
# DF -> LF(5) wegen 53+sqrt(53)+2+10 = 72.3 < 80
# can trigger GC
  local maygc object F_extend2_F (object x)
  {
    floatcase(x,
              { return SF_to_DF(x); }, # 17+sqrt(17)+2+7 = 30.2 < 53
              { return FF_to_DF(x); }, # 24+sqrt(24)+2+7 = 37.9 < 53
              { return DF_to_LF(x,ceiling(73,intDsize)); }, # 53+sqrt(53)+2+10 = 72.3 < 73
              { return LF_extend_LF(x,lf_len_extend2(Lfloat_length(x))); }
             );
  }

# R_ln_R(x,&end_precision) liefert zu einer reellen Zahl x>0 die Zahl ln(x).
# can trigger GC
# Methode:
# x rational -> bei x=1 0 als Ergebnis, sonst x in Float umwandeln.
# x Float ->
#   d := (float-digits x),
#   Genauigkeit um sqrt(d)+max(integer-length(e)) Bits erhöhen,
#   (m,e) := (decode-float x), so dass 1/2 <= m < 1.
#   m<2/3 -> m:=2m, e:=e-1, so dass 2/3 <= m <= 4/3.
#   ln(m) errechnen, ln(x)=ln(m)+e*ln(2) als Ergebnis.
local maygc object R_ln_R (object x, gcv_object_t* end_p)
{
  if (R_rationalp(x)) {
    if (eq(x,Fixnum_1)) { return Fixnum_0; } /* x=1 -> return 0 */
    x = RA_float_F(x); /* convert to float */
  }
  /* x -- float */
  pushSTACK(x); /* save x */
  x = F_extend2_F(x); /* increase computational precision */
  F_decode_float_F_I_F(x); /* compute m,e,s */
  /* Stack layout: x, m, e, s. */
  if (F_F_comp(STACK_2,
               make_SF(0,0+SF_exp_mid,floor(bit(SF_mant_len+2),3))) /* short-float 2/3 */
      < 0) { /* m < 2/3 -> */
    STACK_2 = F_I_scale_float_F(STACK_2,Fixnum_1); /* double m */
    STACK_1 = I_minus1_plus_I(STACK_1); /* decrement e */
  }
  STACK_2 = F_lnx_F(STACK_2); /* ln(m) in the more accurate float format */
  { var object temp;
    if (!eq(STACK_1,Fixnum_0)) {
      temp = ln2_F_float_F(STACK_0); /* ln(2) in that float format */
      temp = R_R_mal_R(STACK_1,temp); /* e*ln(2) */
      temp = R_R_plus_R(STACK_2,temp); /* ln(m)+e*ln(2) */
    } else {
      /* Avoid computing 0*ln(2) since it triggers a
         warn_floating_point_rational_contagion() call. */
      temp = STACK_2;
    }
    if (end_p != NULL) /* (float ... x) */
      temp = F_R_float_F(temp,*end_p);
    skipSTACK(4);
    return temp;
  }
}
#define F_ln_F  R_ln_R

# I_I_log_RA(a,b) liefert zu Integers a>0, b>1 den Logarithmus log(a,b)
# als exakte rationale Zahl, oder nullobj wenn er irrational ist.
# can trigger GC
  local maygc object I_I_log_RA (object a, object b);
# Methode:
#   log(a,b) soll Bruch c/d mit teilerfremdem c>=0,d>0 ergeben.
#   a=1 -> c=0, d=1.
#   a>=b -> Dividiere a durch b. Rest da -> geht nicht.
#           Sonst log(a,b) = 1+log(a/b,b).
#           Berechne also c/d := log(a/b,b) und setze c:=c+d.
#   1<a<b -> log(a,b) = 1/log(b,a).
#           Berechne also c/d := log(b,a) und vertausche c und d.
# Man konstruiert hierbei eigentlich die Kettenbruchentwicklung von c/d.
# Wegen a>=2^c, b>=2^d sind c,d < (integer-length a,b) < intDsize*2^intWCsize.
# In Matrizenschreibweise:
#   Wenn eine Folge von Divisionsschritten D und Vertauschungsschritten V
#   ausgeführt werden muss, z.B. (a,b) V D D = (1,*), so ist
#     ( c )           ( 0 )             ( 1 1 )           ( 0 1 )
#     ( d )  =  V D D ( 1 )  wobei  D = ( 0 1 )  und  V = ( 1 0 ).
#   Man baut diese Matrizen nun von links nach rechts auf, zum Schluss von
#              ( 0 )
#   rechts mit ( 1 ) multiplizieren.
# Entrekursiviert:
#   Wir werden (a,b) und damit auch c/d = log(a/b) verändern.
#   Invariante: Statt (c,d) wollen wir (uc*c+ud*d,vc*c+vd*d) zurückliefern.
#                                           ( uc ud )
#   D.h. die bisherige Matrix von links ist ( vc vd ).
#   uc:=1, ud:=0, vc:=0, vd:=1.
#   Solange a>1,
#     a>=b -> Dividiere a durch b. Rest da -> geht nicht.
#             Sonst a:=a/b, und (für später c:=c+d) ud:=uc+ud, vd:=vc+vd.
#     1<a<b -> vertausche a und b, uc und ud, vc und vd.
#   Liefere (ud,vd), der Bruch ud/vd ist gekürzt.
  local maygc object I_I_log_RA (object a, object b)
  {
    var uintL uc = 1;
    var uintL ud = 0;
    var uintL vc = 0;
    var uintL vd = 1;
    loop {
      if (eq(a,Fixnum_1)) # a=1 -> Rekursion zu Ende
        break;
      if (I_I_comp(a,b) >=0) {
        # a>=b
        pushSTACK(b);
        I_I_divide_I_I(a,b); # a durch b dividieren
        if (!eq(STACK_0,Fixnum_0)) { # Rest /=0 ?
          skipSTACK(3); return nullobj; # -> fertig
        }
        a = STACK_1; b = STACK_2; skipSTACK(3); # a:=a/b
        ud = uc + ud; vd = vc + vd;
      } else {
        # 1<a<b -> a und b vertauschen
        swap(object, a, b);
        swap(uintL, uc, ud); swap(uintL, vc, vd);
      }
    }
    # a=1 -> c=0,d=1 -> Ergebnis ud/vd
    pushSTACK(UL_to_I(ud)); # ud als Integer
    var object y = UL_to_I(vd); # vd als Integer
    var object x = popSTACK();
    return I_I_to_RA(x,y);
  }

# R_R_log_R(a,b) liefert zu reellen Zahlen a>0, b>0 die Zahl
# log(a,b)=ln(a)/ln(b).
# Ergebnis rational nur, wenn a=1 oder a und b rational.
# can trigger GC
  local maygc object R_R_log_R (object a, object b);
# Methode:
# a und b rational:
#   b=1 -> Error
#   a=1 -> Ergebnis 0
#   b Integer:
#     a Integer: log(a,b) rational errechenbar -> liefern
#     a Ratio: a=a1/a2 mit a1>0, a2>1.
#              a1=1 und log(a2,b) rational errechenbar -> -log(a2,b) liefern
#   b Ratio: a=a1/a2, b=b1/b2 mit a1>0, a2>0, b1>0, b2>1.
#            log(a2,b2) rational errechenbar ->
#               b1=1 -> bei a1=1 liefern, sonst nicht.
#               b1>1 -> log(a1,b1) rational errechenbar und
#                       log(a1,b1)=log(a2,b2) -> liefern, sonst nicht.
#            sonst a1,a2 vertauschen:
#              log(a2/a1,b1/b2) versuchen (wie oben) ->
#                -log(a2/a1,b1/b2) liefern
#   Sonst a und b in Floats umwandeln.
# a Float, b rational -> bei b=1 Error, sonst b := (float b a)
# a rational, b Float -> bei a=1 Ergebnis 0, sonst a := (float a b)
# a,b Floats -> log(a,b) = ln(a)/ln(b)
  local maygc object R_R_log_R (object a, object b)
  {
    pushSTACK(a); pushSTACK(b);
    # Stackaufbau: a, b.
    if (R_rationalp(b)) {
      # b rational
      if (eq(b,Fixnum_1)) # b=1 -> Error
        divide_0();
      if (R_rationalp(a)) {
        # a,b beide rational
        if (eq(a,Fixnum_1)) { # a=1 -> Ergebnis 0
          skipSTACK(2); return Fixnum_0;
        }
        if (RA_integerp(b)) {
          # b Integer
          if (RA_integerp(a)) {
            # a,b beide Integers
            var object x = I_I_log_RA(a,b); # rationalen log(a,b) versuchen
            if (!eq(x,nullobj)) {
              skipSTACK(2); return x;
            }
          } else {
            # a Ratio, b Integer
            if (eq(TheRatio(a)->rt_num,Fixnum_1)) { # a1=1
              var object x = I_I_log_RA(TheRatio(a)->rt_den,b); # rationalen log(a2,b) versuchen
              if (!eq(x,nullobj)) {
                skipSTACK(2); return RA_minus_RA(x);
              }
            }
          }
        } else {
          # a rational, b Ratio
          if (RA_integerp(a)) {
            pushSTACK(a); pushSTACK(a=Fixnum_1);
          } else {
            pushSTACK(TheRatio(a)->rt_num); pushSTACK(a=TheRatio(a)->rt_den);
          }
          pushSTACK(TheRatio(b)->rt_num); pushSTACK(b=TheRatio(b)->rt_den);
          # Stackaufbau: a, b, a1>0, a2>0, b1>0, b2>1.
          {
            var object x = I_I_log_RA(a,b); # rationalen log(a2,b2) versuchen
            if (!eq(x,nullobj)) {
              if (eq(STACK_1,Fixnum_1)) { # b1=1 ?
                if (eq(STACK_3,Fixnum_1)) { # a1=1 ?
                  skipSTACK(6); return x; # ja -> x liefern
                }
              } else {
                pushSTACK(x);
                var object y = I_I_log_RA(STACK_(3+1),STACK_(1+1)); # rationalen log(a1,b1) versuchen
                if ((!eq(y,nullobj)) && eql(STACK_0,y)) { # x=y ?
                  x = STACK_0; skipSTACK(6+1); return x;
                }
                skipSTACK(1);
              }
            }
          }
          {
            var object x = I_I_log_RA(STACK_3,STACK_0); # rationalen log(a1,b2) versuchen
            if (!eq(x,nullobj)) {
              if (eq(STACK_1,Fixnum_1)) { # b1=1 ?
                if (eq(STACK_2,Fixnum_1)) { # a2=1 ?
                  skipSTACK(6); return RA_minus_RA(x); # ja -> -x liefern
                }
              } else {
                pushSTACK(x);
                var object y = I_I_log_RA(STACK_(2+1),STACK_(1+1)); # rationalen log(a2,b1) versuchen
                if ((!eq(y,nullobj)) && eql(STACK_0,y)) { # x=y ?
                  x = STACK_0; skipSTACK(6+1); return RA_minus_RA(x);
                }
                skipSTACK(1);
              }
            }
          }
          skipSTACK(4);
        }
        # a,b beide in Floats umwandeln:
        STACK_1 = RA_float_F(STACK_1); STACK_0 = RA_float_F(STACK_0);
      } else {
        # a Float
        STACK_0 = RA_F_float_F(b,a,true); # b := (float b a)
      }
    } else {
      # b Float
      if (R_rationalp(a)) {
        # a rational
        if (eq(a,Fixnum_1)) { # a=1 -> Ergebnis 0
          skipSTACK(2); return RA_F_exact_contagion_R(Fixnum_0,b);
        }
        STACK_1 = RA_F_float_F(a,b,true); # a := (float a b)
      }
    }
    # Nun a,b beide Floats.
    pushSTACK(R_ln_R(STACK_1,NULL)); /* (ln a) */
    pushSTACK(R_ln_R(STACK_1,NULL)); /* (ln b) */
    STACK_0 = F_F_durch_F(STACK_1,STACK_0); /* (/ (ln a) (ln b)) */
    STACK_1 = R_R_contagion_R(STACK_2,STACK_3);
    var object ret = F_R_float_F(STACK_0,STACK_1);
    skipSTACK(4);
    return ret;
  }

# F_expx_F(x) liefert zu einem Float x (betragsmäßig <1) exp(x) als Float.
# can trigger GC
  local maygc object F_expx_F (object x);
# Methode:
# e := Exponent aus (decode-float x), d := (float-digits x)
# Bei x=0.0 oder e<-d liefere 1.0
#   (denn bei e<=-d-1 ist abs(exp(x)-1) = abs(x)+O(x^2) < 2^(-d-1),
#    also ist exp(x), auf d Bits gerundet, gleich 1.0).
# Bei e<=-sqrt(d) verwende die Potenzreihe
#   exp(x) = sum(j=0..inf,x^j/j!):
#   b:=1, i:=0, sum:=0,
#   while (/= sum (setq sum (+ sum b))) do b:=b*x/(i+1), i:=i+1.
#   Ergebnis sum.
# Sonst setze y := x/2 = (scale-float x -1),
#   berechne rekursiv z:=exp(y) und liefere z^2.
# Aufwand: asymptotisch d^2.5 .
  local maygc object F_expx_F (object x)
  {
    if (R_zerop(x))
      return I_F_float_F(Fixnum_1,x);
    var uintL d = F_float_digits(x);
    var sintL e = F_exponent_L(x);
    if (e < (sintL)(-d)) # e < -d ?
      return I_F_float_F(Fixnum_1,x); # ja -> 1.0 als Ergebnis
    pushSTACK(x);
    # Stackaufbau: x.
    var uintL k = 0; # Rekursionszähler k:=0
    { # Bei e <= -1-floor(sqrt(d)) kann die Potenzreihe angewandt werden.
      var sintL e_limit = -1-UL_sqrt_UW(d); # -1-floor(sqrt(d))
      if (e > e_limit) {
        # e > -1-floor(sqrt(d)) -> muss |x| verkleinern.
        k = e - e_limit;
        var object temp = L_to_I((sintL)(-k));
        STACK_0 = F_I_scale_float_F(STACK_0,temp); # x := x/2^k
        # Neuer Exponent = e-k = e_limit.
      }
    }
    # Potenzreihe anwenden:
    {
      var object i = Fixnum_0;
      pushSTACK(I_F_float_F(Fixnum_1,STACK_0)); # b := (float 1 x)
      pushSTACK(I_F_float_F(Fixnum_0,STACK_1)); # sum := (float 0 x)
      # Stackaufbau: x, b, sum.
      loop {
        var object temp;
        temp = F_F_plus_F(STACK_0,STACK_1); # (+ sum b)
        if (eql(STACK_0,temp)) # = sum ?
          break; # ja -> Potenzreihe abbrechen
        STACK_0 = temp;
        temp = F_F_mal_F(STACK_1,STACK_2); # b*x
        i = fixnum_inc(i,1); # i := i+1
        STACK_1 = R_R_durch_R(temp,i); # b := b*x/i
      }
    }
    var object z = STACK_0; # sum als Ergebnis
    skipSTACK(3);
    # Wegen Rekursion noch k mal quadrieren:
    dotimesL(k,k, { z = F_square_F(z); } );
    return z;
  }

# R_exp_R(x) liefert zu einer reellen Zahl x die Zahl exp(x).
# can trigger GC
# Methode:
# x rational -> bei x=0 1 als Ergebnis, sonst x in Float umwandeln.
# x Float ->
#   d := (float-digits x),
#   Genauigkeit um sqrt(d)+max(integer-length(e)) Bits erhöhen,
#   (q,r) := (floor x ln(2))
#   Ergebnis ist exp(q*ln(2)+r) = (scale-float exp(r) q).
local maygc object R_exp_R (object x, bool start_p, gcv_object_t* end_p)
{
  if (R_rationalp(x)) { /* x rational */
    if (eq(x,Fixnum_0)) { return Fixnum_1; } /* x=0 -> return 1 */
    x = RA_float_F(x); /* ==> float */
  }
  /* x -- float */
  pushSTACK(x); /* save x */
  if (start_p) /* increase computational precision */
    x = F_extend2_F(x);
  /* divide by ln(2) (if 0<=x<1/2 can immediately set q:=0) */
  if ((!R_minusp(x)) && (F_exponent_L(x)<0)) {
    /* x>=0, Exponent <0 -> 0<=x<1/2 -> division not necessary */
    pushSTACK(Fixnum_0); pushSTACK(x);
  } else {
    pushSTACK(x);
    { var object ln2 = ln2_F_float_F(x); /* ln(2) with sufficient accuracy */
      x = popSTACK();
      F_F_floor_I_F(x,ln2); /* x / ln(2) */
    }}
  /* stack layout: original x, q, r. */
  { var object temp = F_expx_F(STACK_0); /* exp(r) */
    temp = F_I_scale_float_F(temp,STACK_1); /* * 2^q */
    if (end_p != NULL) /* (float ... x) als Ergebnis */
      temp = F_R_float_F(temp,*end_p);
    skipSTACK(3);
    return temp;
  }
}

# R_sinh_R(x) liefert zu einer reellen Zahl x die Zahl sinh(x).
# can trigger GC
  local maygc object R_sinh_R (object x);
# Methode:
# x rational -> bei x=0 0 als Ergebnis, sonst x in Float umwandeln.
# x Float -> Genauigkeit erhöhen,
#   e := Exponent aus (decode-float x)
#   falls e<=0: (sinh(x)/x)^2 errechnen, Wurzel ziehen, mit x multiplizieren.
#   falls e>0: y:=exp(x) errechnen, (scale-float (- y (/ y)) -1) bilden.
  local maygc object R_sinh_R (object x)
  {
    if (R_rationalp(x)) {
      # x rational
      if (eq(x,Fixnum_0)) # x=0 -> 0 als Ergebnis
        return x;
      x = RA_float_F(x); # sonst in Float umwandeln
    }
    # x Float
    if (F_exponent_L(x)<=0) { # Exponent e abtesten
      # e<=0
      var object temp;
      pushSTACK(x);
      pushSTACK(temp = F_extend_F(x)); # Rechengenauigkeit erhöhen
      temp = F_sqrt_F(F_sinhx_F(temp)); # Wurzel aus (sinh(x)/x)^2
      temp = F_F_mal_F(temp,STACK_0); # mit genauerem x multiplizieren
      temp = F_F_float_F(temp,STACK_1); # und wieder runden
      skipSTACK(2);
      return temp;
    } else {
      # e>0 -> verwende exp(x)
      var object temp;
      pushSTACK(x);
      pushSTACK(temp = R_exp_R(x,true,NULL)); /* y:=exp(x) */
      temp = F_durch_F(temp); # (/ y)
      temp = F_F_minus_F(popSTACK(),temp); # von y subtrahieren
      temp = F_I_scale_float_F(temp,Fixnum_minus1); /* (scale-float -1) */
      return F_F_float_F(temp,popSTACK());
    }
  }

# R_cosh_R(x) liefert zu einer reellen Zahl x die Zahl cosh(x).
# can trigger GC
  local maygc object R_cosh_R (object x);
# Methode:
# x rational -> bei x=0 1 als Ergebnis, sonst x in Float umwandeln.
# x Float -> Genauigkeit erhöhen,
#   e := Exponent aus (decode-float x), d := (float-digits x)
#   falls x=0.0 oder e<=(1-d)/2 liefere 1.0
#     (denn bei e<=(1-d)/2 ist 1 <= cosh(x) = 1+x^2/2+... < 1+2^(-d),
#      also ist cosh(x), auf d Bits gerundet, gleich 1.0).
#   falls e<=0:
#     y := x/2 = (scale-float x -1), (sinh(y)/y)^2 errechnen,
#     cosh(x) = 1+x*y*(sinh(y)/y)^2 errechnen.
#   falls e>0: y:=exp(x) errechnen, (scale-float (+ y (/ y)) -1) bilden.
  local maygc object R_cosh_R (object x)
  {
    if (R_rationalp(x)) {
      # x rational
      if (eq(x,Fixnum_0)) # x=0 -> 1 als Ergebnis
        return Fixnum_1;
      x = RA_float_F(x); # sonst in Float umwandeln
    }
    # x Float
    var sintL e = F_exponent_L(x);
    if (e > 0) {
      # e>0 -> verwende exp(x)
      var object temp;
      pushSTACK(x);
      pushSTACK(temp = R_exp_R(x,true,NULL)); /* y:=exp(x) */
      temp = F_durch_F(temp); # (/ y)
      temp = F_F_plus_F(popSTACK(),temp); # zu y addieren
      temp = F_I_scale_float_F(temp,Fixnum_minus1); /* (scale-float -1) */
      return F_F_float_F(temp,popSTACK());
    } else {
      # e<=0
      if (R_zerop(x))
        return I_F_float_F(Fixnum_1,x);
      {
        var uintL d = F_float_digits(x);
        if (e <= (sintL)(1-d)>>1) # e <= (1-d)/2 <==> e <= -ceiling((d-1)/2) ?
          return I_F_float_F(Fixnum_1,x); # ja -> 1.0 als Ergebnis
      }
      {
        var object temp;
        pushSTACK(x);
        pushSTACK(temp = F_extend_F(x)); # Rechengenauigkeit erhöhen
        pushSTACK(temp = F_I_scale_float_F(temp,Fixnum_minus1)); # y=(scale-float x -1)
        temp = F_sinhx_F(temp); # (sinh(y)/y)^2
        temp = F_F_mal_F(STACK_0,temp); # mit y multiplizieren
        temp = F_F_mal_F(STACK_1,temp); # mit x multiplizieren
        temp = R_R_plus_R(Fixnum_1,temp); # 1 addieren
        temp = F_F_float_F(temp,STACK_2); # und wieder runden
        skipSTACK(3);
        return temp;
      }
    }
  }

# R_cosh_sinh_R_R(x) liefert ((cosh x),(sinh x)), beide Werte auf dem Stack.
# can trigger GC
# Methode:
# x rational -> bei x=0 (1,0) als Ergebnis, sonst x in Float umwandeln.
# x Float -> Genauigkeit erhöhen,
#   e := Exponent aus (decode-float x), d := (float-digits x)
#   falls x=0.0 oder e<=(1-d)/2 liefere (1.0,x)
#     (denn bei e<=(1-d)/2 ist
#      1 <= sinh(x)/x < cosh(x) = 1+x^2/2+... < 1+2^(-d),
#      also ist cosh(x), auf d Bits gerundet, gleich 1.0
#      und sinh(x), auf d Bits gerundet, gleich x).
#   falls e<=0:
#     y:=(sinh(x)/x)^2 errechnen,
#     cosh(x) = sqrt(1+x^2*y) und sinh(x) = x*sqrt(y) errechnen.
#   falls e>0: y:=exp(x) errechnen,
#     (scale-float (+ y (/ y)) -1) und (scale-float (- y (/ y)) -1) bilden.
#   Genauigkeit wieder verringern.
local maygc void R_cosh_sinh_R_R (object x, gcv_object_t* end_p)
{
  if (R_rationalp(x)) { /* x rational */
    if (eq(x,Fixnum_0)) /* x=0 -> return (1,0) */
      { pushSTACK(Fixnum_1); pushSTACK(Fixnum_0); return; }
    x = RA_float_F(x); /* ==> Float */
  }
  /* x -- float */
  { var sintL e = F_exponent_L(x);
    if (e > 0) { /* e>0 -> use exp(x) */
      var object temp;
      pushSTACK(x);
      pushSTACK(temp = R_exp_R(x,true,NULL)); /* y:=exp(x) */
      pushSTACK(temp = F_durch_F(temp)); /* (/ y) */
      /* stack layout: x, exp(x), exp(-x). */
      temp = F_F_plus_F(STACK_1,temp); /* + y */
      temp = F_I_scale_float_F(temp,Fixnum_minus1); /* /2 */
      STACK_2 = (end_p != NULL ? F_F_float_F(temp,*end_p) : temp); /* cosh */
      temp = F_F_minus_F(STACK_1,STACK_0); /* - y */
      temp = F_I_scale_float_F(temp,Fixnum_minus1); /* /2 */
      STACK_1 = (end_p != NULL ? F_F_float_F(temp,*end_p) : temp); /* sinh */
      skipSTACK(1);
      return;
    } else { /* e<=0 */
      if (R_zerop(x) /* e <= (1-d)/2 <==> e <= -ceiling((d-1)/2) ? */
          || (e <= (sintL)(1-F_float_digits(x))>>1)) {
        x = F_extend_F(x); /* increase computational precision */
        pushSTACK(x); pushSTACK(x);
        if (end_p != NULL) {
          STACK_1 = RA_R_float_F(Fixnum_1,*end_p); /* cosh=1 */
          STACK_0 = F_R_float_F(STACK_0,*end_p); /* sinh=x */
        } else
          STACK_1 = I_F_float_F(Fixnum_1,x);
        return;
      }
      pushSTACK(x);
      { var object temp = F_extend_F(x);
        pushSTACK(temp);
        pushSTACK(F_square_F(temp)); /* x*x */
        pushSTACK(temp = F_sinhx_F(STACK_1)); /* y:=(sinh(x)/x)^2 */
        /* stack layout: original x, x, x^2, y. */
        temp = F_sqrt_F(temp); /* sqrt(y) = sinh(x)/x */
        temp = F_F_mal_F(STACK_2,temp); /* x*sqrt(y) = sinh(x) */
        STACK_2 = (end_p != NULL ? F_F_float_F(temp,STACK_3) : temp); /* restore the precision */
        temp = F_F_mal_F(STACK_1,STACK_0); /* x^2*y */
        temp = F_sqrt_F(R_R_plus_R(Fixnum_1,temp)); /* sqrt(1+x^2*y) */
        STACK_3 = (end_p != NULL ? F_F_float_F(temp,STACK_3) : temp); /* restore the precision */
        skipSTACK(2); return;
      }
    }
  }
}

/* R_tanh_R(x) compute the hyperbolic tangens (tanh x) of a real number x.
 can trigger GC
 Method:
 (/ (sinh x) (cosh x)) */
local maygc object R_tanh_R (object x)
{
  pushSTACK(x);
  R_cosh_sinh_R_R(x,NULL);
  /* stack layout: x, cosh(x), sinh(x). */
  var object result = R_R_durch_R(STACK_0,STACK_1);
  if (floatp(STACK_0) || floatp(STACK_1))
    result = F_R_float_F(result,STACK_2); /* reduce precision */
  skipSTACK(3); return result;
}
