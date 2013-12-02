# Liste aller FSUBRs
# Bruno Haible 1990-2000

# Eine Special-Form wird definiert durch eine Deklaration
#   LISPSPECFORM(name,req_anz,opt_anz,body_flag)
# in diesem File.
# Zusätzlich muss in CONTROL.D dieselbe Deklaration samt C-Body stehen.

# name ist der Funktionsname (ein C-Identifier), req_anz die Anzahl der
# required-Parameter (eine Zahl), opt_anz die Anzahl der optional-Parameter
# (eine Zahl), body_flag entweder nobody oder body.


# Expander für die Konstruktion der extern-Deklarationen:
  #define LISPSPECFORM_A(name,req_anz,opt_anz,body_flag)  \
    extern fsubr_function_t C_##name;

# Expander für die Konstruktion der Deklaration der C-Funktion:
  #define LISPSPECFORM_B(name,req_anz,opt_anz,body_flag)  \
    global Values C_##name (void)

# Expander für die Deklaration der FSUBR-Tabelle:
  #define LISPSPECFORM_C(name,req_anz,opt_anz,body_flag)  \
    fsubr_t D_##name;

# Expander für die Initialisierung der FSUBR-Tabelle:
  #define LISPSPECFORM_D(name,req_anz,opt_anz,body_flag)  \
    &C_##name,

# Expander für die Initialisierung der FSUBR-Symbole:
  #define LISPSPECFORM_E(name,req_anz,opt_anz,body_flag)  \
    { offsetof(struct symbol_tab_,S_##name), \
      req_anz,                  \
      opt_anz,                  \
      (uintW)fsubr_##body_flag, \
    },
  #define LISPSPECFORM_F(name,req_anz,opt_anz,body_flag)  \
    { S(name),                  \
      req_anz,                  \
      opt_anz,                  \
      (uintW)fsubr_##body_flag, \
    },

# Welcher Expander benutzt wird, muss vom Hauptfile aus eingestellt werden.
# Default ist   #define LISPSPECFORM LISPSPECFORM_B

# ---------- CONTROL ----------
LISPSPECFORM(eval_when, 1,0,body)
LISPSPECFORM(quote, 1,0,nobody)
LISPSPECFORM(function, 1,1,nobody)
LISPSPECFORM(setq, 0,0,body)
LISPSPECFORM(psetq, 0,0,body)
LISPSPECFORM(progn, 0,0,body)
LISPSPECFORM(prog1, 1,0,body)
LISPSPECFORM(prog2, 2,0,body)
LISPSPECFORM(let, 1,0,body)
LISPSPECFORM(letstern, 1,0,body)
LISPSPECFORM(locally, 0,0,body)
LISPSPECFORM(compiler_let, 1,0,body)
LISPSPECFORM(progv, 2,0,body)
LISPSPECFORM(flet, 1,0,body)
LISPSPECFORM(labels, 1,0,body)
LISPSPECFORM(macrolet, 1,0,body)
LISPSPECFORM(function_macro_let, 1,0,body)
LISPSPECFORM(symbol_macrolet, 1,0,body)
LISPSPECFORM(if, 2,1,nobody)
LISPSPECFORM(when, 1,0,body)
LISPSPECFORM(unless, 1,0,body)
LISPSPECFORM(cond, 0,0,body)
LISPSPECFORM(case, 1,0,body)
LISPSPECFORM(block, 1,0,body)
LISPSPECFORM(return_from, 1,1,nobody)
LISPSPECFORM(tagbody, 0,0,body)
LISPSPECFORM(go, 1,0,nobody)
LISPSPECFORM(multiple_value_list, 1,0,nobody)
LISPSPECFORM(multiple_value_call, 1,0,body)
LISPSPECFORM(multiple_value_prog1, 1,0,body)
LISPSPECFORM(multiple_value_bind, 2,0,body)
LISPSPECFORM(multiple_value_setq, 2,0,nobody)
LISPSPECFORM(catch, 1,0,body)
LISPSPECFORM(unwind_protect, 1,0,body)
LISPSPECFORM(throw, 2,0,nobody)
LISPSPECFORM(declare, 0,0,body)
LISPSPECFORM(the, 2,0,nobody)
LISPSPECFORM(load_time_value, 1,1,nobody)
LISPSPECFORM(and, 0,0,body)
LISPSPECFORM(or, 0,0,body)
# Weitere FSUBRs auch in INIT.LSP (%EXPAND-...) und im Compiler (c-form)
# vermerken!

