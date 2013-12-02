# Rationale Zahlen

# Liefert zu den Integers a und b mit b>1 und ggT(a,b)=1 den Bruch a/b.
# I_I_to_RT(a,b)
# can trigger GC
  #define I_I_to_RT  make_ratio

# Liefert zu den Integers a und b mit b>0 und ggT(a,b)=1 den Bruch a/b
# (Ratio oder Integer).
# I_I_to_RA(a,b)
# can trigger GC
  local maygc object I_I_to_RA (object a, object b);
# Methode:
# falls b=1, a als Ergebnis, sonst der echte Bruch a/b.
  local maygc object I_I_to_RA (object a, object b)
  {
    if (eq(b,Fixnum_1))
      return a;
    else
      return I_I_to_RT(a,b);
  }
  # define I_I_to_RA(a,b)  (eq(b,Fixnum_1) ? a : I_I_to_RT(a,b))

# Liefert zu den Integers a und b mit b>0 den Bruch a/b (Ratio oder Integer).
# can trigger GC
  local maygc object I_posI_durch_RA (object a, object b);
# Methode:
# d:=ggT(a,b).
# Falls d=1: I_I_to_RA anwenden,
# sonst: I_I_to_RA auf a/d und b/d anwenden.
  local maygc object I_posI_durch_RA (object a, object b)
  {
    pushSTACK(a); pushSTACK(b); # a,b retten
    var object d = I_I_gcd_I(a,b); # ggT(a,b) >0
    if (eq(d,Fixnum_1)) { # d=1 ?
      b = popSTACK(); a = popSTACK();
      return I_I_to_RA(a,b);
    } else {
      # Stackaufbau: a, b.
      pushSTACK(d);
      STACK_2 = I_I_exquo_I(STACK_2,d); # a/d bilden
      d = popSTACK();
      # Stackaufbau: a/d, b.
      b = I_I_exquopos_I(popSTACK(),d); # b/d bilden (b,d>0)
      return I_I_to_RA(popSTACK(),b); # (a/d)/(b/d)
    }
  }

# Liefert zu den Integers a und b den Bruch a/b (Ratio oder Integer).
# I_I_durch_RA(a,b)
# can trigger GC
  local maygc object I_I_durch_RA (object a, object b);
# Methode:
# Falls b=0: Error.
# Falls b>0: I_posI_durch_RA anwenden.
# Falls b<0: I_posI_durch_RA auf (- a) und (- b) anwenden.
  local maygc object I_I_durch_RA (object a, object b)
  {
    if (eq(b,Fixnum_0))
      divide_0();
    if (R_minusp(b)) { # b<0 ?
      pushSTACK(b); a = I_minus_I(a); b = STACK_0; # a := (- a)
      STACK_0 = a; b = I_minus_I(b); a = popSTACK(); # b := (- b)
    }
    return I_posI_durch_RA(a,b);
  }

# Liefert Zähler und Nenner einer rationalen Zahl.
# RA_numden_I_I(r, num=,den=);
# > r: rationale Zahl
# < num: (numerator r)
# < den: (denominator r)
  #define RA_numden_I_I(r,num_zuweisung,den_zuweisung)  \
    {                                                                         \
      if (RA_integerp(r)) {                                                   \
        num_zuweisung r; den_zuweisung Fixnum_1; # Zähler = r, Nenner = 1     \
      } else {                                                                \
        num_zuweisung TheRatio(r)->rt_num; den_zuweisung TheRatio(r)->rt_den; \
      }                                                                       \
    }

# Liefert (- r), wo r eine rationale Zahl ist.
# RA_minus_RA(r)
# can trigger GC
  local maygc object RA_minus_RA (object r);
# Methode:
# r Integer -> klar.
# r = a/b -> Ergebnis (- a)/b
  local maygc object RA_minus_RA (object r)
  {
    if (RA_integerp(r))
      return I_minus_I(r);
    else {
      pushSTACK(TheRatio(r)->rt_den); # b retten
      var object a = TheRatio(r)->rt_num;
      a = I_minus_I(a); # (- a)
      # Immer noch b>1 und ggT(-a,b) = ggT(a,b) = 1
      return I_I_to_RT(a,popSTACK());
    }
  }

# (+ r s), wo r und s rationale Zahlen sind.
# RA_RA_plus_RA(r,s)
# can trigger GC
  local maygc object RA_RA_plus_RA (object r, object s);
# Methode (vgl. [Buchberger, Collins, Loos: Computer Algebra, S.200-201])
# r,s beide Integers -> klar.
# r=a/b, s=c -> Ergebnis (a+b*c)/b
#   (mit b>1 und ggT(a+b*c,b) = ggT(a,b) = 1)
#   Bei c=0 direkt r als Ergebnis.
# r=a, s=c/d -> Ergebnis (a*d+c)/d
#   (mit d>1 und ggT(a*d+c,d) = ggT(c,d) = 1)
#   Bei a=0 direkt s als Ergebnis.
# r=a/b, s=c/d:
#   g:=ggT(b,d)>0.
#   Falls g=1:
#     Ergebnis (a*d+b*c)/(b*d),
#     (mit b*d>1 wegen b>1, d>1, und
#      ggT(a*d+b*c,b*d) = 1
#      wegen ggT(a*d+b*c,b) = ggT(a*d,b) = 1 (wegen ggT(a,b)=1 und ggT(d,b)=1)
#      und   ggT(a*d+b*c,d) = ggT(b*c,d) = 1 (wegen ggT(b,d)=1 und ggT(c,d)=1)
#     )
#   Sonst b' := b/g, d' := d/g. e := a*d'+b'*c, f:= b'*d = b*d'.
#   Es ist g = ggT(g*b',g*d') = g*ggT(b',d'), also ggT(b',d')=1.
#   Es ist r+s = (a*d+b*c)/(b*d) = (nach Kürzen mit g) e/f.
#   Außerdem:
#     ggT(a,b') teilt ggT(a,b)=1, also ggT(a,b')=1. Mit ggT(d',b')=1 folgt
#     1 = ggT(a*d',b') = ggT(a*d'+b'*c,b') = ggT(e,b').
#     ggT(c,d') teilt ggT(c,d)=1, also ggT(c,d')=1. Mit ggT(b',d')=1 folgt
#     1 = ggT(b'*c,d') = ggT(a*d'+b'*c,d') = ggT(e,d').
#     Daher ist ggT(e,f) = ggT(e,b'*d'*g) = ggT(e,g).
#   Errechne daher h=ggT(e,g).
#   Bei h=1 ist e/f das Ergebnis (mit f>1, da d>1, und ggT(e,f)=1),
#   sonst ist (e/h)/(f/h) das Ergebnis.
  local maygc object RA_RA_plus_RA (object r, object s)
  {
    if (RA_integerp(s)) {
      # s ist Integer
      if (eq(s,Fixnum_0)) # s=0 -> r als Ergebnis
        return r;
      if (RA_integerp(r))
        # beides Integers
        return I_I_plus_I(r,s);
      else {
        # r = a/b, s = c.
        var object x = TheRatio(r)->rt_den; # b
        pushSTACK(x); pushSTACK(TheRatio(r)->rt_num); # b und a retten
        x = I_I_mal_I(x,s); # b*c
        x = I_I_plus_I(popSTACK(),x); # a+b*c
        return I_I_to_RT(x,popSTACK()); # Bruch (a+b*c)/b
      }
    } else {
      # s ist Ratio
      if (RA_integerp(r)) {
        # r ist Integer
        if (eq(r,Fixnum_0)) # r=0 -> s als Ergebnis
          return s;
        # r = a, s = c/d.
        var object x = TheRatio(s)->rt_den; # d
        pushSTACK(x); pushSTACK(TheRatio(s)->rt_num); # d und c retten
        x = I_I_mal_I(r,x); # a*d
        x = I_I_plus_I(x,popSTACK()); # a*d+c
        return I_I_to_RT(x,popSTACK()); # Bruch (a*d+c)/d
      } else {
        # r,s beide Ratios
        var object g;
        {
          var object b;
          var object d;
          pushSTACK(TheRatio(r)->rt_num); # a retten
          pushSTACK(b = TheRatio(r)->rt_den); # b retten
          pushSTACK(TheRatio(s)->rt_num); # c retten
          pushSTACK(d = TheRatio(s)->rt_den); # d retten
          # Stackaufbau: a, b, c, d.
          g = I_I_gcd_I(b,d); # g = ggT(b,d) >0 bilden
        }
        if (eq(g,Fixnum_1)) {
          # g=1 -> Ergebnis (a*d+b*c)/(b*d)
          var object x;
          STACK_3 = I_I_mal_I(STACK_3,STACK_0); # a*d
          # Stackaufbau: a*d, b, c, d.
          x = I_I_mal_I(STACK_2,STACK_1); # b*c
          STACK_3 = I_I_plus_I(STACK_3,x); # a*d+b*c
          # Stackaufbau: a*d+b*c, b, c, d.
          x = I_I_mal_I(STACK_2,STACK_0); skipSTACK(3); # b*d
          return I_I_to_RT(popSTACK(),x); # (a*d+b*c)/(b*d)
        } else {
          # g>1
          var object x;
          pushSTACK(g);
          # Stackaufbau: a, b, c, d, g.
          STACK_3 = I_I_exquopos_I(STACK_3,g); # b' := b/g (b,g>0)
          # Stackaufbau: a, b', c, d, g.
          x = I_I_exquopos_I(STACK_1,STACK_0); # d' := d/g (d,g>0)
          STACK_4 = I_I_mal_I(STACK_4,x); # a*d'
          # Stackaufbau: a*d', b', c, d, g.
          x = I_I_mal_I(STACK_3,STACK_2); # b'*c
          STACK_4 = I_I_plus_I(STACK_4,x); # e := a*d'+b'*c
          # Stackaufbau: e, b', c, d, g.
          STACK_3 = I_I_mal_I(STACK_3,STACK_1); # f := b'*d
          # Stackaufbau: e, f, c, d, g.
          x = I_I_gcd_I(STACK_4,STACK_0); skipSTACK(3); # h := ggT(e,g)
          # Stackaufbau: e, f.
          if (eq(x,Fixnum_1)) {
            # h=1
            var object f = popSTACK();
            var object e = popSTACK();
            return I_I_to_RT(e,f); # Bruch e/f bilden
          } else {
            # h>1
            pushSTACK(x);
            # Stackaufbau: e, f, h.
            STACK_2 = I_I_exquo_I(STACK_2,x); # e/h bilden
            # Stackaufbau: e/h, f, h.
            x = popSTACK(); # h
            x = I_I_exquopos_I(popSTACK(),x); # f/h bilden (f,h>0)
            return I_I_to_RA(popSTACK(),x); # (e/h)/(f/h) als Ergebnis
          }
        }
      }
    }
  }

# (- r s), wo r und s rationale Zahlen sind.
# RA_RA_minus_RA(r,s)
# can trigger GC
  local maygc object RA_RA_minus_RA (object r, object s);
#if 0
# Methode:
# (+ r (- s))
  local maygc object RA_RA_minus_RA (object r, object s)
  {
    pushSTACK(r); s = RA_minus_RA(s);
    return RA_RA_plus_RA(popSTACK(),s);
  }
#else
# Methode (vgl. [Buchberger, Collins, Loos: Computer Algebra, S.200-201])
# r,s beide Integers -> klar.
# r=a/b, s=c -> Ergebnis (a-b*c)/b
#   (mit b>1 und ggT(a-b*c,b) = ggT(a,b) = 1)
#   Bei c=0 direkt r als Ergebnis.
# r=a, s=c/d -> Ergebnis (a*d-c)/d
#   (mit d>1 und ggT(a*d-c,d) = ggT(-c,d) = ggT(c,d) = 1)
#   Bei a=0 direkt -s = (-c)/d als Ergebnis.
# r=a/b, s=c/d:
#   g:=ggT(b,d)>0.
#   Falls g=1:
#     Ergebnis (a*d-b*c)/(b*d),
#     (mit b*d>1 wegen b>1, d>1, und
#      ggT(a*d-b*c,b*d) = 1
#      wegen ggT(a*d-b*c,b) = ggT(a*d,b) = 1 (wegen ggT(a,b)=1 und ggT(d,b)=1)
#      und   ggT(a*d-b*c,d) = ggT(b*c,d) = 1 (wegen ggT(b,d)=1 und ggT(c,d)=1)
#     )
#   Sonst b' := b/g, d' := d/g. e := a*d'-b'*c, f:= b'*d = b*d'.
#   Es ist g = ggT(g*b',g*d') = g*ggT(b',d'), also ggT(b',d')=1.
#   Es ist r-s = (a*d-b*c)/(b*d) = (nach Kürzen mit g) e/f.
#   Außerdem:
#     ggT(a,b') teilt ggT(a,b)=1, also ggT(a,b')=1. Mit ggT(d',b')=1 folgt
#     1 = ggT(a*d',b') = ggT(a*d'-b'*c,b') = ggT(e,b').
#     ggT(c,d') teilt ggT(c,d)=1, also ggT(c,d')=1. Mit ggT(b',d')=1 folgt
#     1 = ggT(b'*c,d') = ggT(a*d'-b'*c,d') = ggT(e,d').
#     Daher ist ggT(e,f) = ggT(e,b'*d'*g) = ggT(e,g).
#   Errechne daher h=ggT(e,g).
#   Bei h=1 ist e/f das Ergebnis (mit f>1, da d>1, und ggT(e,f)=1),
#   sonst ist (e/h)/(f/h) das Ergebnis.
  local maygc object RA_RA_minus_RA (object r, object s)
  {
    if (RA_integerp(s)) {
      # s ist Integer
      if (eq(s,Fixnum_0)) # s=0 -> r als Ergebnis
        return r;
      if (RA_integerp(r))
        # beides Integers
        return I_I_minus_I(r,s);
      else {
        # r = a/b, s = c.
        var object x = TheRatio(r)->rt_den; # b
        pushSTACK(x); pushSTACK(TheRatio(r)->rt_num); # b und a retten
        x = I_I_mal_I(x,s); # b*c
        x = I_I_minus_I(popSTACK(),x); # a-b*c
        return I_I_to_RT(x,popSTACK()); # Bruch (a-b*c)/b
      }
    } else {
      # s ist Ratio
      if (RA_integerp(r)) {
        # r ist Integer
        if (eq(r,Fixnum_0)) {
          # r=0 -> -s = (-c)/d als Ergebnis
          pushSTACK(TheRatio(s)->rt_den); # d
          s = I_minus_I(TheRatio(s)->rt_num); # -c
          return I_I_to_RT(s,popSTACK());
        }
        # r = a, s = c/d.
        var object x = TheRatio(s)->rt_den; # d
        pushSTACK(x); pushSTACK(TheRatio(s)->rt_num); # d und c retten
        x = I_I_mal_I(r,x); # a*d
        x = I_I_minus_I(x,popSTACK()); # a*d-c
        return I_I_to_RT(x,popSTACK()); # Bruch (a*d-c)/d
      } else {
        # r,s beide Ratios
        var object g;
        {
          var object b;
          var object d;
          pushSTACK(TheRatio(r)->rt_num); # a retten
          pushSTACK(b = TheRatio(r)->rt_den); # b retten
          pushSTACK(TheRatio(s)->rt_num); # c retten
          pushSTACK(d = TheRatio(s)->rt_den); # d retten
          # Stackaufbau: a, b, c, d.
          g = I_I_gcd_I(b,d); # g = ggT(b,d) >0 bilden
        }
        if (eq(g,Fixnum_1)) {
          # g=1 -> Ergebnis (a*d-b*c)/(b*d)
          var object x;
          STACK_3 = I_I_mal_I(STACK_3,STACK_0); # a*d
          # Stackaufbau: a*d, b, c, d.
          x = I_I_mal_I(STACK_2,STACK_1); # b*c
          STACK_3 = I_I_minus_I(STACK_3,x); # a*d-b*c
          # Stackaufbau: a*d-b*c, b, c, d.
          x = I_I_mal_I(STACK_2,STACK_0); skipSTACK(3); # b*d
          return I_I_to_RT(popSTACK(),x); # (a*d-b*c)/(b*d)
        } else {
          # g>1
          var object x;
          pushSTACK(g);
          # Stackaufbau: a, b, c, d, g.
          STACK_3 = I_I_exquopos_I(STACK_3,g); # b' := b/g (b,g>0)
          # Stackaufbau: a, b', c, d, g.
          x = I_I_exquopos_I(STACK_1,STACK_0); # d' := d/g (d,g>0)
          STACK_4 = I_I_mal_I(STACK_4,x); # a*d'
          # Stackaufbau: a*d', b', c, d, g.
          x = I_I_mal_I(STACK_3,STACK_2); # b'*c
          STACK_4 = I_I_minus_I(STACK_4,x); # e := a*d'-b'*c
          # Stackaufbau: e, b', c, d, g.
          STACK_3 = I_I_mal_I(STACK_3,STACK_1); # f := b'*d
          # Stackaufbau: e, f, c, d, g.
          x = I_I_gcd_I(STACK_4,STACK_0); skipSTACK(3); # h := ggT(e,g)
          # Stackaufbau: e, f.
          if (eq(x,Fixnum_1)) {
            # h=1
            var object f = popSTACK();
            var object e = popSTACK();
            return I_I_to_RT(e,f); # Bruch e/f bilden
          } else {
            # h>1
            pushSTACK(x);
            # Stackaufbau: e, f, h.
            STACK_2 = I_I_exquo_I(STACK_2,x); # e/h bilden
            # Stackaufbau: e/h, f, h.
            x = popSTACK(); # h
            x = I_I_exquopos_I(popSTACK(),x); # f/h bilden (f,h>0)
            return I_I_to_RA(popSTACK(),x); # (e/h)/(f/h) als Ergebnis
          }
        }
      }
    }
  }
#endif

# (1+ r), wo r eine rationale Zahl ist.
# RA_1_plus_RA(r)
# can trigger GC
  local maygc object RA_1_plus_RA (object r);
# Methode:
# Falls r ein Integer ist: I_1_plus_I anwenden
# Falls r = a/b: (a+b)/b, wobei b>1 und ggT(a+b,b)=ggT(a,b)=1 ist.
  local maygc object RA_1_plus_RA (object r)
  {
    if (RA_integerp(r))
      return I_1_plus_I(r);
    else {
      var object x;
      x = TheRatio(r)->rt_den; pushSTACK(x); # b
      x = I_I_plus_I(TheRatio(r)->rt_num,x); # a+b
      return I_I_to_RT(x,popSTACK()); # (a+b)/b
    }
  }

# (1- r), wo r eine rationale Zahl ist.
# RA_minus1_plus_RA(r)
# can trigger GC
  local maygc object RA_minus1_plus_RA (object r);
# Methode:
# Falls r ein Integer ist: I_minus1_plus_I anwenden
# Falls r = a/b: (a-b)/b, wobei b>1 und ggT(a-b,b)=ggT(a,b)=1 ist.
  local maygc object RA_minus1_plus_RA (object r)
  {
    if (RA_integerp(r)) {
      return I_minus1_plus_I(r);
    } else {
      var object x;
      x = TheRatio(r)->rt_den; pushSTACK(x); # b
      x = I_I_minus_I(TheRatio(r)->rt_num,x); # a-b
      return I_I_to_RT(x,popSTACK()); # (a-b)/b
    }
  }

# RA_RA_comp(r,s) vergleicht zwei rationale Zahlen r und s.
# Ergebnis: 0 falls r=s, +1 falls r>s, -1 falls r<s.
# can trigger GC
  local maygc signean RA_RA_comp (object r, object s);
# Methode:
# r,s Integer -> klar
# r<0, s>=0 -> r<s.
# r>=0, s<0 -> r>s.
# r Integer, s Ratio: r=a, s=b/c. Vergleiche a*c und b.
# r Ratio, s Integer: r=a/b, s=c. Vergleiche a und b*c.
# r,s Ratios: r=a/b, s=c/d. Vergleiche a*d und b*c.
  local maygc signean RA_RA_comp (object r, object s)
  {
    # 1. Schritt: Test, ob beides Integers:
    if (RA_integerp(r) && RA_integerp(s))
      return I_I_comp(r,s);
    # r,s nicht beide Integers.
    # 2. Schritt: Test, ob die Vorzeichen bereits das Ergebnis hergeben:
    if (R_minusp(r)) {
      if (!R_minusp(s)) # r<0, s>=0 -> r<s
        return signean_minus;
    } else {
      if (R_minusp(s)) # r>=0, s<0 -> r>s
        return signean_plus;
    }
    # r,s haben gleiches Vorzeichen.
    # 3. Schritt: Fallunterscheidung nach Typen
    if (RA_integerp(r)) {
      # r Integer, s Ratio: r=a, s=b/c. Vergleiche a*c und b.
      pushSTACK(TheRatio(s)->rt_num); # b
      r = I_I_mal_I(r,TheRatio(s)->rt_den); # a*c
      return I_I_comp(r,popSTACK()); # mit b vergleichen
    } elif (RA_integerp(s)) {
      # r Ratio, s Integer: r=a/b, s=c. Vergleiche a und b*c.
      pushSTACK(TheRatio(r)->rt_num); # a
      s = I_I_mal_I(TheRatio(r)->rt_den,s); # b*c
      return I_I_comp(popSTACK(),s); # und a damit vergleichen
    } else {
      # r,s Ratios: r=a/b, s=c/d. Vergleiche a*d und b*c.
      pushSTACK(TheRatio(r)->rt_num); # a
      pushSTACK(TheRatio(s)->rt_den); # d
      # Stackaufbau: a, d.
      var object x = I_I_mal_I(TheRatio(r)->rt_den,TheRatio(s)->rt_num); # b*c
      var object a = STACK_1;
      STACK_1 = x;
      # Stackaufbau: b*c, d.
      x = I_I_mal_I(a,popSTACK()); # a*d
      return I_I_comp(x,popSTACK()); # a*d und b*c vergleichen
    }
  }

# Kehrwert (/ r), wo r eine rationale Zahl ist.
# RA_durch_RA(r)
# can trigger GC
  local maygc object RA_durch_RA (object r);
# Methode:
# r=0 -> Error.
# a:=(numerator r), b:=(denominator r).
# a>0 -> Ergebnis b/a (mit ggT(b,a)=1).
# a<0 -> Ergebnis (- b)/(- a) (mit ggT(-b,-a)=1).
  local maygc object RA_durch_RA (object r)
  {
    if (eq(r,Fixnum_0)) # Test auf 0
      divide_0();
    var object a;
    var object b;
    RA_numden_I_I(r,a=,b=); # a:=(numerator r), b:=(denominator r)
    if (R_minusp(a)) {
      # a<0
      pushSTACK(a); b = I_minus_I(b); a = STACK_0; # b := (- b)
      STACK_0 = b; a = I_minus_I(a); b = popSTACK(); # a := (- a)
    }
    return I_I_to_RA(b,a);
  }

# Liefert (* r r), wo r eine rationale Zahl ist.
# RA_square_RA(r)
# can trigger GC
  local maygc object RA_square_RA (object r);
# Methode:
# r Integer -> klar.
# r = a/b -> Ergebnis a^2/b^2
  local maygc object RA_square_RA (object r)
  {
    if (RA_integerp(r))
      # r Integer
      return I_square_I(r);
    else {
      # r=a/b
      var object a;
      var object b;
      pushSTACK(TheRatio(r)->rt_den); # b retten
      a = TheRatio(r)->rt_num;
      a = I_square_I(a); # a^2
      b = STACK_0; STACK_0 = a;
      b = I_square_I(b); # b^2
      a = popSTACK();
      # Immer noch b^2>1 und ggT(a^2,b^2) = ggT(a,b)^2 = 1
      return I_I_to_RT(a,b);
    }
  }

# Liefert (* r s), wo r und s rationale Zahlen sind.
# RA_RA_mal_RA(r,s)
# can trigger GC
  local maygc object RA_RA_mal_RA (object r, object s);
# Methode (vgl. [Buchberger, Collins, Loos: Computer Algebra, S.201])
# r,s beide Integers -> klar.
# r=a/b, s=c ->
#   Bei c=0 Ergebnis 0.
#   g:=ggT(b,c).
#   Falls g=1: Ergebnis (a*c)/b (mit b>1, ggT(a*c,b)=1).
#   Sonst: b':=b/g, c':=c/g, Ergebnis (a*c')/b' (mit ggT(a*c',b')=1).
# r=a, s=c/d analog.
# r=a/b, s=c/d ->
#   g:=ggT(a,d), h:=ggT(b,c).
#   a':=a/g, d':=d/g (nur bei g>1 bedeutet das Rechnung).
#   b':=b/h, c':=c/h (nur bei h>1 bedeutet das Rechnung).
#   Ergebnis ist = (a'*c')/(b'*d').
  local maygc object RA_RA_mal_RA (object r, object s)
  {
    var object a;
    var object b;
    var object c;
    if (RA_integerp(s)) {
      # s Integer
      if (RA_integerp(r))
        # beides Integer
        return I_I_mal_I(r,s);
      else {
        # r=a/b, s=c
        a = TheRatio(r)->rt_num; b = TheRatio(r)->rt_den; c = s;
        mixed: # Bilde a/b * c
        if (eq(c,Fixnum_0)) # c=0 -> Ergebnis 0
          return c;
        pushSTACK(b); pushSTACK(a); pushSTACK(c);
        # Stackaufbau: b, a, c.
        var object g = I_I_gcd_I(b,c); # g := ggT(b,c)
        if (eq(g,Fixnum_1)) {
          # g=1
          c = popSTACK(); # c
          c = I_I_mal_I(popSTACK(),c); # a*c
          return I_I_to_RT(c,popSTACK()); # (a*c)/b
        } else {
          # g>1
          pushSTACK(g);
          # Stackaufbau: b, a, c, g.
          STACK_3 = I_I_exquopos_I(STACK_3,g); # b' := b/g (b,g>0)
          # Stackaufbau: b', a, c, g.
          g = popSTACK();
          c = I_I_exquo_I(popSTACK(),g); # c' := c/g
          c = I_I_mal_I(popSTACK(),c); # a*c'
          return I_I_to_RA(c,popSTACK()); # (a*c')/b'
        }
      }
    } else {
      # s ist Ratio
      if (RA_integerp(r)) {
        # r=c, s=a/b
        a = TheRatio(s)->rt_num; b = TheRatio(s)->rt_den; c = r;
        goto mixed;
      } else {
        # r,s beide Ratios
        var object d;
        a = TheRatio(r)->rt_num; pushSTACK(a); # a
        pushSTACK(TheRatio(r)->rt_den); # b
        d = TheRatio(s)->rt_den; pushSTACK(d); # d
        pushSTACK(TheRatio(s)->rt_num); # c
        # Stackaufbau: a, b, d, c.
        {
          var object g = I_I_gcd_I(a,d); # g := ggT(a,d)
          if (!eq(g,Fixnum_1)) {
            # bei g>1: dividiere a und d durch g
            a = STACK_3; STACK_3 = g;
            a = I_I_exquo_I(a,g); # a':=a/g
            g = STACK_3; STACK_3 = a;
            STACK_1 = I_I_exquopos_I(STACK_1,g); # d':=d/g (d,g>0)
          }
        }
        # Stackaufbau: a', b, d', c.
        {
          var object h = I_I_gcd_I(STACK_2,STACK_0); # h := ggT(b,c)
          if (!eq(h,Fixnum_1)) {
            # bei h>1: dividiere c und b durch h
            c = STACK_0; STACK_0 = h;
            c = I_I_exquo_I(c,h); # c':=c/h
            h = STACK_0; STACK_0 = c;
            STACK_2 = I_I_exquopos_I(STACK_2,h); # b':=b/h (b,h>0)
          }
        }
        # Stackaufbau: a', b', d', c'.
        c = popSTACK(); STACK_2 = I_I_mal_I(STACK_2,c); # a'*c'
        # Stackaufbau: a'*c', b', d'.
        d = popSTACK(); d = I_I_mal_I(popSTACK(),d); # b'*d'
        # Stackaufbau: a'*c'.
        return I_I_to_RA(popSTACK(),d); # (a'*c')/(b'*d')
      }
    }
  }

# Liefert (/ r s), wo r und s rationale Zahlen sind.
# RA_RA_durch_RA(r,s)
# can trigger GC
  local maygc object RA_RA_durch_RA (object r, object s);
# Methode:
# (* r (/ s))
  local maygc object RA_RA_durch_RA (object r, object s)
  {
    if (RA_integerp(r) && RA_integerp(s)) # r und s Integers?
      return I_I_durch_RA(r,s); # ja -> schnell abhandeln
    pushSTACK(r);
    s = RA_durch_RA(s); # (/ s)
    return RA_RA_mal_RA(popSTACK(),s);
  }

# Liefert ganzzahligen und gebrochenen Anteil einer rationalen Zahl.
# (q,r) := (truncate x)
# RA_truncate_I_RA(x);
# > x: rationale Zahl
# < STACK_1: Quotient q, ein Integer
# < STACK_0: Rest r, eine rationale Zahl
# Erniedrigt STACK um 2
# can trigger GC
  local maygc void RA_truncate_I_RA (object x);
# Methode:
# x Integer -> (q,r) := (x,0)
# x Ratio a/b ->
#   (truncate a b) liefert q und r.
#   Liefere q und r/b (mit b>1 und ggT(r,b)=ggT(r+q*b,b)=ggT(a,b)=1).
  local maygc void RA_truncate_I_RA (object x)
  {
    if (RA_integerp(x)) {
      pushSTACK(x); pushSTACK(Fixnum_0); # (q,r) := (x,0)
    } else {
      var object b = TheRatio(x)->rt_den;
      pushSTACK(b);
      I_I_truncate_I_I(TheRatio(x)->rt_num,b); # (truncate a b)
      # Stackaufbau: b, q, r.
      b = STACK_2;
      STACK_2 = STACK_1; # q unverändert
      var object r = popSTACK();
      STACK_0 = I_I_to_RT(r,b);
    }
  }

# Liefert ganzzahligen und gebrochenen Anteil einer rationalen Zahl.
# (q,r) := (floor x)
# RA_floor_I_RA(x);
# > x: rationale Zahl
# < STACK_1: Quotient q, ein Integer
# < STACK_0: Rest r, eine rationale Zahl
# Erniedrigt STACK um 2
# can trigger GC
  local maygc void RA_floor_I_RA (object x);
# Methode:
# x Integer -> (q,r) := (x,0)
# x Ratio a/b ->
#   (floor a b) liefert q und r.
#   Liefere q und r/b (mit b>1 und ggT(r,b)=ggT(r+q*b,b)=ggT(a,b)=1).
  local maygc void RA_floor_I_RA (object x)
  {
    if (RA_integerp(x)) {
      pushSTACK(x); pushSTACK(Fixnum_0); # (q,r) := (x,0)
    } else {
      var object b = TheRatio(x)->rt_den;
      pushSTACK(b);
      I_I_floor_I_I(TheRatio(x)->rt_num,b); # (floor a b)
      # Stackaufbau: b, q, r.
      b = STACK_2;
      STACK_2 = STACK_1; # q unverändert
      var object r = popSTACK();
      STACK_0 = I_I_to_RT(r,b);
    }
  }

# Liefert ganzzahligen und gebrochenen Anteil einer rationalen Zahl.
# (q,r) := (ceiling x)
# RA_ceiling_I_RA(x);
# > x: rationale Zahl
# < STACK_1: Quotient q, ein Integer
# < STACK_0: Rest r, eine rationale Zahl
# Erniedrigt STACK um 2
# can trigger GC
  local maygc void RA_ceiling_I_RA (object x);
# Methode:
# x Integer -> (q,r) := (x,0)
# x Ratio a/b ->
#   (ceiling a b) liefert q und r.
#   Liefere q und r/b (mit b>1 und ggT(r,b)=ggT(r+q*b,b)=ggT(a,b)=1).
  local maygc void RA_ceiling_I_RA (object x)
  {
    if (RA_integerp(x)) {
      pushSTACK(x); pushSTACK(Fixnum_0); # (q,r) := (x,0)
    } else {
      var object b = TheRatio(x)->rt_den;
      pushSTACK(b);
      I_I_ceiling_I_I(TheRatio(x)->rt_num,b); # (ceiling a b)
      # Stackaufbau: b, q, r.
      b = STACK_2;
      STACK_2 = STACK_1; # q unverändert
      var object r = popSTACK();
      STACK_0 = I_I_to_RT(r,b);
    }
  }

# Liefert ganzzahligen und gebrochenen Anteil einer rationalen Zahl.
# (q,r) := (round x)
# RA_round_I_RA(x);
# > x: rationale Zahl
# < STACK_1: Quotient q, ein Integer
# < STACK_0: Rest r, eine rationale Zahl
# Erniedrigt STACK um 2
# can trigger GC
  local maygc void RA_round_I_RA (object x);
# Methode:
# x Integer -> (q,r) := (x,0)
# x Ratio a/b ->
#   (round a b) liefert q und r.
#   Liefere q und r/b (mit b>1 und ggT(r,b)=ggT(r+q*b,b)=ggT(a,b)=1).
  local maygc void RA_round_I_RA (object x)
  {
    if (RA_integerp(x)) {
      pushSTACK(x); pushSTACK(Fixnum_0); # (q,r) := (x,0)
    } else {
      var object b = TheRatio(x)->rt_den;
      pushSTACK(b);
      I_I_round_I_I(TheRatio(x)->rt_num,b); # (round a b)
      # Stackaufbau: b, q, r.
      b = STACK_2;
      STACK_2 = STACK_1; # q unverändert
      var object r = popSTACK();
      STACK_0 = I_I_to_RT(r,b);
    }
  }

# RA_I_expt_RA(x,y) = (expt x y), wo x eine rationale Zahl und y ein Integer >0 ist.
# can trigger GC
  local object RA_I_expt_RA (object x, object y);
  # Methode:
  # x Integer -> klar
  # x Ratio a/b -> x^y = (a^y)/(b^y), gekürzt, mit b^y>=b>1.
  local maygc object RA_I_expt_RA (object x, object y)
  {
    if (RA_integerp(x))
      return I_I_expt_I(x,y);
    else {
      pushSTACK(y);
      pushSTACK(TheRatio(x)->rt_den);
      x = I_I_expt_I(TheRatio(x)->rt_num,y); # a^y
      y = STACK_1; STACK_1 = x;
      x = I_I_expt_I(popSTACK(),y); # b^y
      return I_I_to_RT(popSTACK(),x); # (a^y)/(b^y)
    }
  }

# Stellt fest, ob eine rationale Zahl >=0 das Quadrat einer rationalen Zahl
# ist.
# RA_sqrtp(x)
# > x: eine rationale Zahl >=0
# < ergebnis: exakte Wurzel (sqrt x) falls x Quadrat, nullobj sonst
# can trigger GC
  local maygc object RA_sqrtp (object x);
# Methode:
# Bei Integers: klar.
# Bei Brüchen a/b : muss a=c^2 und b=d^2 sein. Dann ist die Wurzel = c/d
# (mit ggT(c,d)=1 und d>1).
  local maygc object RA_sqrtp (object x)
  {
    if (RA_integerp(x))
      return I_sqrtp(x);
    else {
      # x ist Ratio
      pushSTACK(TheRatio(x)->rt_num); # Zähler retten
      x = TheRatio(x)->rt_den;
      var object h = I_sqrtp(x); # Nenner auf Quadratzahl testen
      if (eq(h,nullobj)) {
        skipSTACK(1); return nullobj;
      }
      x = STACK_0; STACK_0 = h;
      h = I_sqrtp(x); # Zähler auf Quadratzahl testen
      if (eq(h,nullobj)) {
        skipSTACK(1); return nullobj;
      }
      # beides Quadratzahlen -> Quotient der Wurzeln bilden
      return I_I_to_RT(h,popSTACK());
    }
  }

# Stellt fest, ob eine rationale Zahl >=0 die n-te Potenz einer rationalen Zahl
# ist.
# RA_rootp(x,n)
# > x: eine rationale Zahl >=0
# > n: ein Integer >0
# < ergebnis: exakte n-te Wurzel (expt x (/ n)) falls eine n-te Potenz, nullobj sonst
# can trigger GC
  local maygc object RA_rootp (object x, object n);
# Methode:
# Bei Integers: klar.
# Bei Brüchen a/b : muss a=c^n und b=d^n sein. Dann ist die Wurzel = c/d
# (mit ggT(c,d)=1 und d>1).
  local maygc object RA_rootp (object x, object n)
  {
    if (RA_integerp(x))
      return I_rootp(x,n);
    else {
      # x ist Ratio
      pushSTACK(TheRatio(x)->rt_num); pushSTACK(n); # Zähler und n retten
      x = TheRatio(x)->rt_den;
      var object h = I_rootp(x,n); # Nenner auf n-te Potenz testen
      if (eq(h,nullobj)) {
        skipSTACK(2); return nullobj;
      }
      n = popSTACK(); x = STACK_0; STACK_0 = h;
      h = I_rootp(x,n); # Zähler auf n-te Potenz testen
      if (eq(h,nullobj)) {
        skipSTACK(1); return nullobj;
      }
      # beides n-te Potenzen -> Quotient der Wurzeln bilden
      return I_I_to_RT(h,popSTACK());
    }
  }

