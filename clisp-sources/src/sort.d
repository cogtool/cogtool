# n log(n) - Sortierroutine für CLISP
# Bruno Haible 1992-2003

# Ziel: Eine feste Anzahl n von Elementen zu sortieren,
# mit maximalem Zeitaufwand von O(n log(n)),
# und dies, ohne allzu aufwendige Datenstrukturen aufzubauen.

# Von außen ist einzustellen:
# Identifier SORTID :
#   Identifier, der die Inkarnation dieser Package identifiziert
# Typ SORT_ELEMENT :
#   Typ der Elemente, die zu sortieren sind.
# Typ SORT_KEY :
#   Typ des Key, nach dem sortiert wird.
# Funktion SORT_KEYOF, mit
#   local SORT_KEY SORT_KEYOF (SORT_ELEMENT element);
#   liefert den Sortier-Key eines Elements.
# Funktion SORT_COMPARE, mit
#   local signean SORT_COMPARE (SORT_KEY key1, SORT_KEY key2);
#   liefert >0 falls key1>key2, <0 falls key1<key2, 0 falls key1=key2.
# Funktion SORT_LESS, mit
#   local bool SORT_LESS (SORT_KEY key1, SORT_KEY key2);
#   liefert true falls key1<key2, false falls key1>=key2.

#ifndef SORT
  # Eine Art "SORT-Package"
  #define SORT(incarnation,identifier)  CONCAT4(sort_,incarnation,_,identifier)
#endif

# Quelle: Samuel P. Harbison, Guy L. Steele: C - A Reference Manual, S. 61

# Feststellen, ob element1 < element2 gilt:
  #define less(element1,element2)  \
    SORT_LESS(SORT_KEYOF(element1),SORT_KEYOF(element2))

# sort(v,n); sortiert den Array v[0]..v[n-1] in aufsteigende Reihenfolge.
  local void SORT(SORTID,sort) (SORT_ELEMENT* v, uintL n)
  {
    var SORT_ELEMENT* w = &v[-1];
    # w[1]..w[n] ist dasselbe wie v[0]..v[n-1] .
    # Man fasst die Zahlen 1,...,n so zu einem balancierten Binärbaum
    # zusammen, dass k die Söhne 2*k und 2*k+1 habe.
    # Ein Teilstück w[r]..w[s] heißt sortiert, wenn für alle
    # k mit r <= k <= s gilt:
    #   Falls 2*k <= s, gilt w[k] >= w[2*k], und
    #   falls 2*k+1 <= s, gilt w[k] >= w[2*k+1],
    # d.h. wenn jedes Element einen Wert >= dem Wert seiner Söhne hat.
    # Teilaufgabe:
    #   Sei 0<r<=s und w[r+1]..w[s] bereits sortiert.
    #   Sortiere w[r]..w[s].
    #   Zeitaufwand: max. O(log(s)).
      #define adjust(r,s)  \
        { var uintL i = r;                                                    \
          loop { # w[i] ist im Teilbaum unterhalb von i unterzubringen        \
            var uintL j = 2*i; # ein Sohn von i                               \
            if (j > s) # 2*i und 2*i+1 nicht mehr vorhanden -> fertig         \
              break;                                                          \
            if ((j < s) && less(w[j],w[j+1])) # evtl. j = 2*i+1, der andere Sohn von i \
              j++;                                                            \
            # j ist der Sohn von i mit dem größeren Wert.                     \
            if (less(w[i],w[j])) { # Falls w[i] < w[j],                       \
              swap(SORT_ELEMENT, w[i], w[j]); # w[i] und w[j] vertauschen     \
            }                                                                 \
            # w[i] ist nun der größere der drei Werte w[i],w[2*i],w[2*i+1].   \
            # Jetzt haben wir aber w[j] verkleinert, so dass ein              \
            # tail-rekursives adjust(j,s) nötig wird:                         \
            i = j;                                                            \
          }                                                                   \
        }
    if (n<=1) # nichts zu tun?
      return;
    # Wegen 2*(floor(n/2)+1) > n ist w[floor(n/2)+1]..w[n] bereits sortiert.
    {
      var uintL r;
      for (r = floor(n,2); r>0; r--) {
        # Hier ist w[r+1]..w[n] sortiert.
        adjust(r,n);
        # Hier ist w[r]..w[n] sortiert.
      }
    }
    # Nun ist w[1]..w[n] ein sortierter Baum.
    # Jeweils das höchste Element w[1] entnehmen und ans Ende setzen:
    {
      var uintL s;
      for (s = n-1; s>0; s--) {
        # Hier ist w[1]..w[s+1] ein sortierter Baum, und
        # w[s+2]..w[n] die höchsten Elemente, aufsteigend sortiert.
        swap(SORT_ELEMENT, v[0], v[s]); # w[1] und w[s+1] vertauschen
        # Hier ist w[2]..w[s] ein sortierter Baum, und
        # w[s+1]..w[n] die höchsten Elemente, aufsteigend sortiert.
        adjust(1,s); # w[1] in den Baum hineinsortieren
        # Hier ist w[1]..w[s] ein sortierter Baum, und
        # w[s+1]..w[n] die höchsten Elemente, aufsteigend sortiert.
      }
    }
  }

#undef adjust
#undef less

