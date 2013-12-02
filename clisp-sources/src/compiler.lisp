;; CLISP - compiler
;; Bruno Haible 20.-30.09.1988, 05.-07.10.1988, 10.10.1988, 16.12.1988
;;   Version for KCL 27.06.1989, 05.-07.07.1989
;;   c-VALUES extended on 14.07.1989
;;   label-operand in assemble-LAP corrected on 14.07.1989
;;   ANODE-Components SOURCE, STACKZ eliminated on 14.07.1989
;;     (conditionally dependent on #+CLISP-DEBUG)
;;   Peephole-Optimization-Protocol conditionally dependent on #+PEEPHOLE-DEBUG
;;   Version for CLISP 28.07.1989-11.08.1989
;;   Variable-Optimization 10.03.1991
;; Michael Stoll, September-December 1991:
;;   - Bytecode reworked
;;   - Code-Optimization with reference to Labels/Jumps improved
;;   - small improvement at c-plus/c-minus,
;;     Compilation of CxxxR in succession of (CAR) and (CDR)
;;   - SUBR-calls without Argument-Check at runtime,
;;     SUBRs as constants (via #.#'name)
;;   - Calls of local functions without Argument-Check at run-time
;;   - Recursive calls by subroutine-call JSR, for tail-recursion
;;     JMPTAIL (corresponds to PSETQ with following jump)
;;   - Improvement for the call of a function with Rest-Parameters via APPLY
;; Bruno Haible, February-March 1992:
;;   - more detailed seclass, better PSETQ
;;   - better Constant Folding
;;   - Cross-Compilation
;; Bruno Haible, 03.06.1992:
;;   - Inline-Compilation of calls of global functions
;; Bruno Haible, August 1993:
;;   - support for CLOS: generic functions %GENERIC-FUNCTION-LAMBDA,
;;     Optimization of unused Required-parameters %OPTIMIZE-FUNCTION-LAMBDA
;;   - GENERIC-FLET, GENERIC-LABELS
;;   - Inline-Compilation of (APPLY (FUNCTION ...) ...)
;; Bruno Haible, 09.06.1996:
;;   - Bytecode platform-independent
;; Bruno Haible, 04.12.1998:
;;   - Optimization of generic functions by RETGF
;; further plans:
;;   - change Variable-Environments in a way, so the call of local functions
;;     becomes possible via JSR/JMPTAIL (i.e. belated decision, if
;;     call by CALLC or JSR)
;;   - poss. better Optimization by data-flow-analysis
;;   - Inline-Compilation of calls of local functions

;; Sam Steingold 1999-2005
;; German comments translated into English: Stefan Kain 2001-12-18
;; "z" at the end of a variable name stands for "zustand" (German for "state")

(in-package "COMMON-LISP")
(export '(ext::eval-env) "EXT")
(export '(custom::*package-tasks-treat-specially*) "CUSTOM")
(ext:re-export "CUSTOM" "EXT")
(export '(compile compile-file disassemble))
(pushnew ':compiler *features*)

(in-package "COMPILER")
;; Convention: Write SYSTEM::PNAME for a Symbol, that is "accidentally" in
;; #<PACKAGE SYSTEM>, but which we don't use any further.
;; Write SYS::PNAME, if we assume any properties for the Symbol.
;; Write COMPILER::PNAME, if the Compiler declares the Symbol
;; and it is used by other program parts.
(import '(sys::function-name-p sys::parse-body sys::add-implicit-block
          sys::make-load-time-eval sys::make-macro-expander
          sys::make-funmacro-expander
          sys::analyze-lambdalist sys::specialized-lambda-list-to-ordinary
          sys::closure-name sys::closure-codevec sys::closure-consts
          sys::fixnump sys::short-float-p sys::single-float-p
          sys::double-float-p sys::long-float-p
          sys::search-file sys::date-format sys::line-number
          sys::%funtabref sys::inlinable sys::constant-inlinable
          sys::module-name
          sys::*compiling* sys::*compiling-from-file* sys::*inline-functions*
          sys::*venv* sys::*fenv* sys::*benv* sys::*genv* sys::*denv*
          sys::*toplevel-environment* sys::*toplevel-denv*
          sys::*current-source-file*
          COMPILER::C-PROCLAIM COMPILER::C-PROCLAIM-CONSTANT
          COMPILER::EVAL-WHEN-COMPILE
          COMPILER::C-DEFUN COMPILER::C-PROVIDE COMPILER::C-REQUIRE))

;; some auxilliary functions
(proclaim '(inline env mac-exp))
(defun env () (vector *venv* *fenv*))
(defun mac-exp (mac form &optional (env (env)))
  (funcall *macroexpand-hook* mac form env))

;; Avoid bootstrapping issues with CLOS.
(eval-when (compile load eval)
  (setq clos::*compile-accessor-functions* nil))

(defconstant *keyword-package* (find-package "KEYWORD"))
(defconstant *lisp-package* (find-package "COMMON-LISP"))

;; variables for top-level-call:
(defvar *compiling* nil) ; specifies, if inside the process of compilation
;; (defvar *error-count*) ; number of errors
;; (defvar *warning-count*) ; number of warnings
;; (defvar *style-warning-count*) ; number of style-warnings
(defvar *compile-warnings* t) ; if compiler-warnings are reported
(defvar *compile-verbose* t) ; if compiler-comments are reported
(defvar *compile-print* nil) ; if compiler reports, where he currently is
;; The first argument passed to COMPILE-FILE. Does not necessarily refer to
;; a file!
(defvar *compile-file-pathname* nil) ; CLtL2 p. 680
;; The physical pathname denoting the file being compiled. Determined from
;; *compile-file-pathname*. Includes the file extension.
(defvar *compile-file-resolved-pathname* nil)
;; *compile-file-truename* = (truename *compile-file-resolved-pathname*).
(defvar *compile-file-truename* nil) ; CLtL2 p. 680
(defvar *compile-file-directory* nil) ; for c-REQUIRE
(defvar *compile-file-lineno1* nil)
(defvar *compile-file-lineno2* nil)
(defvar *c-listing-output*) ; Compiler-Listing-Stream or nil
(defvar *c-error-output*) ; Compiler-Error-Stream
;; essentially
;; *c-error-output* = (make-broadcast-stream *error-output* *c-listing-output*)
;; The names of declared dynamic variables
(defvar *known-special-vars* nil)
;; The names and values of constants
(defvar *constant-special-vars* nil)

;; T if called by COMPILE-FILE or LOAD :COMPILE
(defvar *compiling-from-file*)
;; how do we determine what will happen with the compiled code,
;; i.e., will it be evaluated right away or loaded before that?
;; if we are called by COMPILE-FILE, we are writing to *fasoutput-stream*,
;;   *compiling-from-file* is T
;; if we are writing by LOAD :COMPILE, *fasoutput-stream* is nil, but
;;   *compiling-from-file* is T
;; if we are called by COMPILE, both *fasoutput-stream* and
;;   *compiling-from-file* are nil
;; the last 2 cases are when the code can be evaluated right after
;;   compilation, so for them LOAD-TIME-VALUE == EVAL,
;;  ==> defer-eval-after-load == *fasoutput-stream*
;; on the other hand, what do we do with
;;  (eval-when (compile) (funcall (compile nil (lambda () (load-time-value)))))
;;  we need to bind *fasoutput-stream* to NIL in eval-when!

;;; The variables for COMPILE-FILE:
;; The compiler's output file stream, or nil
(defvar *fasoutput-stream* nil)
;; The compiler's library file stream, or nil
(defvar *liboutput-stream* nil)
;; The compiler's C output file name, or nil
(defvar *coutput-file* nil)
;; The compiler's C output file stream, or nil
(defvar *coutput-stream* nil)
;; The names of functions which contained errors
(defvar *functions-with-errors* nil)
;; The names of functions known up to now, modified by the DEFUN macroexpander
;; ((name c-source-point signature side-effect-class) ...)
(defvar *known-functions* nil)
;; The names of unknown functions (up to now)
(defvar *unknown-functions* nil)
;; The names of undeclared dynamic variables (up to now)
(defvar *unknown-free-vars* nil)
;; The names of used obsolete functions (up to now)
(defvar *deprecated-functions* nil)
;; The names of functions declared globally inline (up to now)
(defvar *inline-functions* nil)
;; The names of functions declared globally notinline (up to now)
(defvar *notinline-functions* nil)
;; The alist of global inlinable function definitions
(defvar *inline-definitions* nil)
;; The symbols denoting constants which are declared constant-inline
(defvar *inline-constants* nil)
;; The symbols denoting constants which are declared constant-notinline
(defvar *notinline-constants* nil)
;; The additional user defined declaration types (not really "types")
(defvar *user-declaration-types* nil)
;; The names of modules already compiled
;; (treated as if they were already loaded)
(defvar *compiled-modules* nil)
(defvar *package-tasks-treat-specially* nil
  "Treat package-related operations the same way at compile and load time.")
;; The list of the pending package tasks
(defvar *package-tasks* nil)
;; The data accumulated by the FFI.
(defvar *ffi-module* nil)
;; The load forms generated so far (by `make-load-form').
(defvar *load-forms* nil)

#|
The compiler's target is the virtual machine described in <doc/impbyte.xml>
and <http://clisp.cons.org/impnotes.html#bytecode>.

1. Pass of the Compiler:
   macro-expansion,
   code-generation (symbolically),
   allocation of variables on the STACK or in Closures,
   Optimization on LISP-level.
   Thereafter the stack-layout is definite for each involved function.
   The Information is contained in a graph of ANODEs.
2. Pass of the Compiler:
   resolution of variable references,
   optimization on code-level (peephole-Optimization),
   creation of compiled functional objects.
3. Pass of the Compiler:
   resolution of references between the functional objects.
|#

#| external representation of a Closure:
 #Y(name
    #lengthY(Byte in Hex ... Byte in Hex)
    further constants)
|#

(defun make-closure (&key name codevec consts seclass)
  (sys::%make-closure name (sys::make-code-vector codevec) consts seclass))

;; The instruction list is in <doc/impbyte.xml>.

;; classification of instructions:
;; O = instruction without operand
;; K = numerical operand or
;;     short-operand (then the byte is = short-code-ops[x] + operand)
;; N = numerical operand
;; B = Byte-Operand
;; L = Label-Operand
;; NH = numerical Operand, that references a Hashtable
;; NC = numerical Operand, that references a Handler-Cons
;; LX = as many Label-Operands as specified by the preceding Operand

;; the position in the instruction-table delivers the actual code of the
;; instruction (>= 0, < short-code-base), codes >= short-code-base are
;; occupied by the K-instructions.
(defconstant instruction-table
  '#(;; (1) constants
     (NIL O) (PUSH-NIL N) (T O) (CONST K)
     ;; (2) static variables
     (LOAD K) (LOADI NNN) (LOADC NN) (LOADV NN) (LOADIC NNNN)
     (STORE K) (STOREI NNN) (STOREC NN) (STOREV NN) (STOREIC NNNN)
     ;; (3) dynamic variables
     (GETVALUE N) (SETVALUE N) (BIND N) (UNBIND1 O) (UNBIND N) (PROGV O)
     ;; (4) stack-operations
     (PUSH O) (POP O) (SKIP N) (SKIPI NNN) (SKIPSP NN)
     ;; (5) program flow and jumps
     (SKIP&RET N) (SKIP&RETGF N)
     (JMP L) (JMPIF L) (JMPIFNOT L) (JMPIF1 L) (JMPIFNOT1 L)
     (JMPIFATOM L) (JMPIFCONSP L) (JMPIFEQ L) (JMPIFNOTEQ L)
     (JMPIFEQTO NL) (JMPIFNOTEQTO NL) (JMPHASH NHL) (JMPHASHV NHL) (JSR L)
     (JMPTAIL NNL)
     ;; (6) environments and closures
     (VENV O) (MAKE-VECTOR1&PUSH N) (COPY-CLOSURE NN)
     ;; (7) function-calls
     (CALL NN) (CALL0 N) (CALL1 N) (CALL2 N)
     (CALLS1 B) (CALLS2 B) (CALLSR NB) (CALLC O) (CALLCKEY O)
     (FUNCALL N) (APPLY N)
     ;; (8) optional and keyword-arguments
     (PUSH-UNBOUND N) (UNLIST NN) (UNLIST* NN) (JMPIFBOUNDP NL) (BOUNDP N)
     (UNBOUND->NIL N)
     ;; (9) treatment of multiple values
     (VALUES0 O) (VALUES1 O) (STACK-TO-MV N) (MV-TO-STACK O) (NV-TO-STACK N)
     (MV-TO-LIST O) (LIST-TO-MV O) (MVCALLP O) (MVCALL O)
     ;; (10) BLOCK
     (BLOCK-OPEN NL) (BLOCK-CLOSE O) (RETURN-FROM N) (RETURN-FROM-I NNN)
     ;; (11) TAGBODY
     (TAGBODY-OPEN NLX) (TAGBODY-CLOSE-NIL O) (TAGBODY-CLOSE O) (GO NN)
     (GO-I NNNN)
     ;; (12) CATCH and THROW
     (CATCH-OPEN L) (CATCH-CLOSE O) (THROW O)
     ;; (13) UNWIND-PROTECT
     (UNWIND-PROTECT-OPEN L) (UNWIND-PROTECT-NORMAL-EXIT O)
     (UNWIND-PROTECT-CLOSE O) (UNWIND-PROTECT-CLEANUP O)
     ;; (14) HANDLER
     (HANDLER-OPEN NC) (HANDLER-BEGIN&PUSH O)
     ;; (15) some functions
     (NOT O) (EQ O) (CAR O) (CDR O) (CONS O) (SYMBOL-FUNCTION O) (SVREF O)
     (SVSET O) (LIST N) (LIST* N)
     ;; (16) combined operations
     (NIL&PUSH O) (T&PUSH O) (CONST&PUSH K)
     (LOAD&PUSH K) (LOADI&PUSH NNN) (LOADC&PUSH NN) (LOADV&PUSH NN)
     (POP&STORE N) (GETVALUE&PUSH N) (JSR&PUSH L)
     (COPY-CLOSURE&PUSH NN)
     (CALL&PUSH NN) (CALL1&PUSH N) (CALL2&PUSH N)
     (CALLS1&PUSH B) (CALLS2&PUSH B) (CALLSR&PUSH NB)
     (CALLC&PUSH O) (CALLCKEY&PUSH O)
     (FUNCALL&PUSH N) (APPLY&PUSH N)
     (CAR&PUSH O) (CDR&PUSH O) (CONS&PUSH O)
     (LIST&PUSH N) (LIST*&PUSH N)
     (NIL&STORE N) (T&STORE N) (LOAD&STOREC NNN)
     (CALLS1&STORE BN) (CALLS2&STORE BN) (CALLSR&STORE NBN)
     (LOAD&CDR&STORE N) (LOAD&CONS&STORE N) (LOAD&INC&STORE N)
     (LOAD&DEC&STORE N) (LOAD&CAR&STORE NN)
     (CALL1&JMPIF NL) (CALL1&JMPIFNOT NL)
     (CALL2&JMPIF NL) (CALL2&JMPIFNOT NL)
     (CALLS1&JMPIF BL) (CALLS1&JMPIFNOT BL)
     (CALLS2&JMPIF BL) (CALLS2&JMPIFNOT BL)
     (CALLSR&JMPIF NBL) (CALLSR&JMPIFNOT NBL)
     (LOAD&JMPIF NL) (LOAD&JMPIFNOT NL)
     (LOAD&CAR&PUSH N) (LOAD&CDR&PUSH N) (LOAD&INC&PUSH N) (LOAD&DEC&PUSH N)
     (CONST&SYMBOL-FUNCTION N) (CONST&SYMBOL-FUNCTION&PUSH N)
     (CONST&SYMBOL-FUNCTION&STORE NN)
     (APPLY&SKIP&RET NN) (FUNCALL&SKIP&RETGF NN)))
(dotimes (i (length instruction-table))
  (setf (get (first (svref instruction-table i)) 'INSTRUCTION) i))
(defconstant instruction-codes
  (let ((hashtable (make-hash-table :key-type 'symbol :value-type 'fixnum
                                    :test 'stablehash-eq :warn-if-needs-rehash-after-gc t)))
    (dotimes (i (length instruction-table))
      (setf (gethash (first (svref instruction-table i)) hashtable) i))
    hashtable))

;; K-instructions:
(defconstant instruction-table-K
  '#(LOAD LOAD&PUSH CONST CONST&PUSH STORE))
(defconstant short-code-base 157)
(defconstant short-code-opsize '#(15   25   21   30    8))
(defconstant short-code-ops '#(157  172  197  218  248));256


#|

intermediate language after the 1st pass:
=========================================

1. constants:

   (NIL)                      A0 := NIL, 1 value

   (PUSH-NIL n)               n-times: -(STACK) := NIL, undefined values

   (T)                        A0 := T, 1 value

   (CONST const)              A0 := 'const, 1 value

   (FCONST fnode)             A0 := the compilation result of an fnode, 1 value

   (BCONST block)             A0 := the Block-Cons of this block (a
                              constant from FUNC), 1 value

   (GCONST tagbody)           A0 := the Tagbody-Cons of this tagbody (a
                              constant from FUNC), 1 value

2.,3. variables:

   (GET var venvc stackz)     A0 := var, 1 value
                              (venvc is the current Closure-Venv,
                               stackz is the current stack-state)

   (SET var venvc stackz)     var := A0, 1 value
                              (venvc is the current Closure-Venv,
                               stackz is the current stack-state)

   (STORE n)                  (STACK+4*n) := A0, 1 value

   (GETVALUE symbol)          A0 := (symbol-value 'symbol), 1 value

   (SETVALUE symbol)          (setf (symbol-value 'symbol) A0), 1 value

   (BIND const)               binds const (a Symbol) dynamically to A0.
                              undefined values.

   (UNBIND1)                  unwinds a binding-frame

   (PROGV)                    binds the Symbols in the List (STACK)+
                              dynamically to the values in the List A0 and
                              thereby constructs exactly one binding-frame,
                              undefined values
4. Stack-Operations:

   (PUSH)                     -(STACK) := A0, undefined values

   (POP)                      A0 := (STACK)+, 1 values

   (UNWIND stackz1 stackz2 for-value) Executes an Unwind within a function:
                              cleans the Stack, in order to get from
                              Stack-State stackz1 to Stack-State stackz2.
                              Resolves Frames lying inbetween. for-value
                              specifies, if the values A0/... must be
                              saved thereby.
   (UNWINDSP stackz1 stackz2) modifies the SP, in order to get from
                              Stack-State stackz1 to Stack-State stackz2 .
                              STACK and the values A0/... remain unchanged.

5. Control Flow and Jumps:

   (RET)                      terminates the function with the values A0/...

   (RETGF)                    terminates the function with 1 value A0
                              and poss. calls A0 as function, with the same
                              Arguments as the current function

   (JMP label)                jump to label

   (JMPIF label)              if A0 /= NIL : jump to label.

   (JMPIFNOT label)           if A0 = NIL : jump to label.

   (JMPIF1 label)             if A0 /= NIL : 1 value, jump to label.

   (JMPIFNOT1 label)          if A0 = NIL : 1 value, jump to label.

   (JMPHASH test ((obj1 . label1) ... (objm . labelm)) label . labels)
                              jump to labeli, if A0 = obji (in terms of
                              the specified comparison), else to label.
                              Undefined values.

   (JSR m label)              calls the Code at label as subroutine,
                              with m Arguments on the Stack

   (BARRIER)                  is never reached, counts as leaving-jump

6. Environments and Closures:

   (VENV venvc stackz)        A0 := the Venv, that corresponds to venvc
                              (from the Stack, as Constant from
                               FUNC, or NIL, if not available in FUNC),
                              1 value
                              (stackz is the current Stack-State)
   (MAKE-VECTOR1&PUSH n)      creates a simple-vector with n+1 (n>=0)
                              Components and puts in A0 as Component 0.
                              -(STACK) := the new Vector.
                              Undefined values.
   (COPY-CLOSURE fnode n)     copies the Closure, that corresponds to fnode
                              and in the copy it replaces for i=0,...,n-1 (n>0)
                              the component (CONST i) by (STACK+4*(n-1-i)).
                              STACK := STACK+4*n. A0 := Closure-Copy, 1 value

7. Function-Calls:

   (CALLP)                    starts the construction of a Function-Call-Frame
                              (is discarded in the 2. Pass without replacement)

   (CALL k const)             calls the Function const with k Arguments
                              (STACK+4*(k-1)),...,(STACK+4*0) ,
                              STACK:=STACK+4*k, result is stored to A0/...
   (CALL0 const)              calls the Function const with 0 Arguments,
                              result is stored to A0/...
   (CALL1 const)              calls the Function const with 1 Argument A0,
                              result is stored to A0/...
   (CALL2 const)              calls the Function const with 2 Arguments (STACK)
                              and A0 , STACK:=STACK+4,
                              result is stored to A0/...
   (CALLS1 n)                 calls the Function (FUNTAB n)
   (CALLS2 n)                 resp. (FUNTAB 256+n)
                              (a SUBR without Rest-Parameter),
                              with the correct number of arguments on the STACK
                              STACK is cleaned, result is stored to A0/...
   (CALLSR m n)               calls the Function (FUNTABR n)
                              (a SUBR with Rest-Parameter) ,
                              with the correct number of arguments and
                              additional m remaining arguments on the STACK.
                              STACK is cleaned, result is stored to A0/...
   (CALLC)                    calls the Function A0 (a compiled Closure
                              without Keyword-Parameter). Arguments
                              are already in the right format on the STACK,
                              STACK is cleaned, result is stored to A0/...
   (CALLCKEY)                 calls the Function A0 (a compiled Closure
                              with Keyword-Parameter). Arguments
                              are already in the right format on the STACK,
                              STACK is cleaned, result is stored to A0/...
   (FUNCALLP)                 adds the structure of a FUNCALL-Frame,
                              Function to be called is in A0
   (FUNCALL n)                calls the specified Function with n (n>=0)
                              Arguments (all on the Stack),
                              removes the FUNCALL-Frame,
                              result is stored to A0/...
   (APPLYP)                   adds the structure of an APPLY-Frame,
                              Function to be called is in A0
   (APPLY n)                  calls the specified Function with n (n>=0)
                              Arguments (all on the Stack) and further
                              Arguments (list in A0),
                              removes the APPLY-Frame,
                              result is stored to A0/...

8. optional and Keyword-Arguments:

   (PUSH-UNBOUND n)           n-times: -(STACK) := #<UNBOUND>, undefined values

   (UNLIST n m)               shorten list A0 n times: -(STACK) := (car A0),
                              A0 := (cdr A0). At the last m times A0 may have
                              already reached the end,
                              then -(STACK) := #<UNBOUND> instead.
                              At the end A0 must be NIL.
                              undefined values. 0 <= m <= n.
   (UNLIST* n m)              shorten list A0 n times: -(STACK) := (car A0),
                              A0 := (cdr A0). At the last m times A0 may have
                              already reached the end,
                              then -(STACK) := #<UNBOUND> instead.
                              Then -(STACK) := (nthcdr n A0).
                              undefined values. 0 <= m <= n, n > 0.
   (JMPIFBOUNDP var venvc stackz label)
                              if Variable /= #<UNBOUND> :
                                jump to label, A0 := Variable, 1 value.
                              else undefined values.
                              (stackz is the current Stack-State)
   (BOUNDP var venvc stackz)  A0 := (NIL if Variable=#<UNBOUND>, else T),
                              1 value
                              (stackz is the current Stack-State)

9. Treatment of Multiple Values:

   (VALUES0)                  A0 := NIL, 0 values

   (VALUES1)                  A0 := A0, 1 value

   (STACK-TO-MV n)            fetches n values from (STACK)+ ,
                              STACK:=STACK+4*n, n>1

   (MV-TO-STACK)              Multiple Values A0/A1/... to -(STACK),
                              1. value atop, STACK:=STACK-4*D7.W,
                              after that undefined values

   (NV-TO-STACK n)            the first n values (n>=0) to -(STACK),
                              1. value atop, STACK:=STACK-4*n,
                              undefined values

   (MV-TO-LIST)               Multiple Values A0/... as List to A0,
                              1 value

   (LIST-TO-MV)               A0/... := (values-list A0)

   (MVCALLP)                  prepares a MULTIPLE-VALUE-CALL to the
                              Function in A0

   (MVCALL)                   executes a MULTIPLE-VALUE-CALL with the
                              Arguments lying in the Stack

10. BLOCK:

   (BLOCK-OPEN const label)   Stores a Block-Cons (with CAR=const and CDR=
                              Framepointer) to -(STACK), constructs a
                              Block-Frame. Undefined values.
                              On RETURN to this Frame --> jump to label.
   (BLOCK-CLOSE)              Leave the Block and thereby dismantle a Block-
                              Frame (including the Block-Cons-Variables)
   (RETURN-FROM const)        Leave the Block, whose Block-Cons is specified,
                              with the values A0/...
   (RETURN-FROM block)        Leave the specified Block (its Block-Cons
                              occurs among the BlockConsts of FUNC) with
                              the values A0/...
   (RETURN-FROM block stackz) Leave the specified Block (its Block-Cons
                              occurs in the Stack) with the values A0/...

11. TAGBODY:

   (TAGBODY-OPEN const label1 ... labelm)
                              Stores a Tagbody-Cons (with CAR=const
                              and CDR=Framepointer) on -(STACK), constructs a
                              Tagbody-Frame. Undefined values.
                              On GO with number l ---> jump to labell.
   (TAGBODY-CLOSE-NIL)        Leave the Tagbody and thereby dismantle a
                              Tagbody-Frame (including the Tagbody-Cons-
                              Variables). A0 := NIL, 1 value
   (TAGBODY-CLOSE)            Leave the Tagbody and thereby dismantle a
                              Tagbody-Frame (including the Tagbody-Cons-
                              Variables).
   (GO const l)               jump in Tagbody, whose Tagbody-Cons
                              is specified, to Tag (svref (car const) l)
   (GO tagbody l)             jump in the specified Tagbody to Tag Number
                              (cdr l) in (tagbody-used-far tagbody)
   (GO tagbody l stackz)      jump in the specified Tagbody to Tag Number
                              (cdr l) in (tagbody-used-far tagbody), its
                              Tagbody-Cons is located in the Stack

12. CATCH and THROW:

   (CATCH-OPEN label)         constructs a CATCH-Frame with A0 as Tag;
                              On THROW to this Tag ---> jump to label

   (CATCH-CLOSE)              unwinds a CATCH-Frame

   (THROW)                    executes a THROW to the Catch-Tag (STACK)+,
                              with the values A0/...

13. UNWIND-PROTECT:

   (UNWIND-PROTECT-OPEN label)  constructs a UNWIND-PROTECT-Frame; On
                              Unwind --> jump to label, saving the values

   (UNWIND-PROTECT-NORMAL-EXIT) unwinds an Unwind-Protect-Frame, writes a
                              Continuation-Address to SP, saves the
                              values and starts to execute the following
                              Cleanup-Code
   (UNWIND-PROTECT-CLOSE label) finishes the Cleanup-Code: writes back
                              the saved values, executes an RTS.
                              The Cleanup-Code starts at label.
   (UNWIND-PROTECT-CLEANUP)   unwinds an Unwind-Protect-Frame, writes a
                              Continuation-Address and the PC to SP,
                              saves the values and starts to execute the
                              Cleanup-Code

14. HANDLER:

   (HANDLER-OPEN const stackz label1 ... labelm)
                              constructs a HANDLER-Frame; const contains
                              the Condition-Types; die corresponding
                              Handlers start at labeli
   (HANDLER-BEGIN)            begins a Handler: produces the SP-State
                              as with HANDLER-OPEN,
                              A0 := Condition passed to the Handler, 1 value

15. Some Functions:

   (NOT)                      = (CALL1 #'NOT)

   (EQ)                       = (CALL2 #'EQ)

   (CAR denv)                 = (CALL1 #'CAR)

   (CDR denv)                 = (CALL1 #'CDR)

   (CONS)                     = (CALL2 #'CONS)

   (ATOM)                     = (CALL1 #'ATOM)

   (CONSP)                    = (CALL1 #'CONSP)

   (SYMBOL-FUNCTION denv)     = (CALL1 #'SYMBOL-FUNCTION)

   (SVREF denv)               = (CALL2 #'SVREF)

   (SVSET)                    (setf (svref (STACK) A0) (STACK+4)),
                              A0 := (STACK+4), 1 value, STACK:=STACK+8

   (LIST n)                   = (CALL n #'LIST), n>0

   (LIST* n)                  = (CALL n+1 #'LIST*), n>0


Some Definitions:

n, m, k     an Integer >=0

stackz      a Stack-State (see STACK-MANAGEMENT).
            The Stack-Layout stands firm after the 1. Pass.

venvc       the Environment of the Closure-Variables (see VARIABLE-MANAGEMENT).
            This also stands firm after the 1. Pass.

var         a Variable (see VARIABLE-MANAGEMENT). If it is
            special/constant/lexical, stands firm after the 1. Pass.

const       a Constant

symbol      a Symbol

fun         either (CONST const) a Constant, that is a Symbol,
            or (FUNTAB index) an indexing in the fixed Function-Table.

fnode       an fnode (see FUNCTION-MANAGEMENT)

label       a Label (uninterned Symbol)

block       a Block-Descriptor (see BLOCK-MANAGEMENT)

test        EQ or EQL or EQUAL

for-value   NIL or T

|#

(defconstant function-codes
  (let ((hashtable (make-hash-table :key-type 'symbol :value-type 'fixnum
                                    :test 'stablehash-eq :warn-if-needs-rehash-after-gc t)))
    (dotimes (i (* 3 256))
      (let ((sym (%funtabref i))) ; Name of the Function FUNTAB[i]
        (when sym (setf (gethash sym hashtable) i))))
    hashtable))
(defconstant funtabR-index ; Startindex of FUNTABR with reference to FUNTAB
  (dotimes (i (* 3 256))
    (let ((sym (%funtabref i)))
      (multiple-value-bind (name req opt rest-p) (subr-info sym)
        (declare (ignore name req opt))
        (when rest-p (return i))))))
(defun CALLS-code (funtab-index)
  (if (< funtab-index 256)
    `(CALLS1 ,funtab-index)
    `(CALLS2 ,(- funtab-index 256))))
(defmacro CALLS-code-fun (fun)
  `(load-time-value (CALLS-code (gethash ',fun function-codes))))

;; error message function
(defun compiler-error (caller &optional where)
  (error (TEXT "Compiler bug!! Occurred in ~A~@[ at ~A~].") caller where))


;;;;****                      STACK   MANAGEMENT

;; A Stack-State describes, what will be located in the two Stacks
;; at run-time.
;; Exact Structure:
;;  (item1 ... itemk . fun)
;; In memory this is really a tree-structure!
;; Definitions:
;;  fun = FNODE of the Function, in which the counting takes place.
;;  item = one of the following:
;;   n (Integer >=0) : n Lisp-Objects on the STACK
;;                     occupies n STACK-Entries
;;   (BIND n)        : one Binding-Frame for n Variables,
;;                     occupies 1+2*n STACK-Entries and 0 SP-Entries
;;                     Must be unwound on Unwind explicitely
;;   PROGV           : a Binding-Frame for arbitrary many Variables,
;;                     occupies ? STACK-Entries and 1 SP-Entry (Pointer above
;;                     the Frame = old STACK)
;;                     Must be unwound on Unwind explicitely
;;   CATCH           : a CATCH-Frame
;;                     occupies 3 STACK-Entries and 2+jmpbufsize SP-Entries
;;   UNWIND-PROTECT  : an Unwind-Protect-Frame
;;                     occupies 2 STACK-Entries and 2+jmpbufsize SP-Entries
;;                     Must be unwound on Unwind and Cleanup has to be executed
;
;;   CLEANUP         : during the Cleanup-Phase of an UNWIND-PROTECT
;;                     occupies ? STACK-Entries and 3 SP-Entries
;;                     (the lower one is Pointer above the Frame = old STACK)
;;   BLOCK           : a BLOCK-Frame
;;                     occupies 3 STACK-Entries and 2+jmpbufsize SP-Entries
;;                     Must be unwound on Unwind explicitely
;;   (TAGBODY n)     : a TAGBODY-Frame, that stores n Tags
;;                     occupies 3+n STACK-Entries and 1+jmpbufsize SP-Entries
;;                     Must be unwound on Unwind explicitely
;;   MVCALLP         : Preparation for MVCALL
;;                     occupies 1 STACK-Entry and 1 SP-Entry (Pointer above
;;                     FRAME = STACK)
;;   MVCALL          : many Lisp-Objects
;;                     occupies ? STACK-Entries and 1 SP-Entry (Pointer above
;;                     Frame = old STACK)
;;   ANYTHING        : many Lisp-Objects and Frames
;;                     occupies ? STACK-Entries and 1 SP-Entry (Pointer above
;;                     Frame = old STACK)

(defvar *stackz*)    ; the current Stack-State

;; A SP-Depth k is a cons (k1 . k2) and means k1+jmpbufsize*k2.
(defmacro spd (k1 k2) `(cons ,k1 ,k2))
(defun spd+ (k kd)
  (cons (+ (car k) (car kd))
        (+ (cdr k) (cdr kd))))
(defun spd- (k kd)
  (cons (- (car k) (car kd))
        (- (cdr k) (cdr kd))))
(defun spd<= (k kk)
  (and (<= (car k) (car kk))
       (<= (cdr k) (cdr kk))))
(defun spdmax (k kk)
  (cons (max (car k) (car kk))
        (max (cdr k) (cdr kk))))
#|
;; We cannot simply take the maximum of two depths, have to work with lists.
;; Is depth covered by some of the depths in the list?
 (defun some-spd<= (depth list-of-depths)
  (dolist (x list-of-depths nil)
    (when (spd<= depth x) (return t))))
|#

;; (stackz-fun stackz) extracts from a Stack-State the function, that is
;; currently processed.
#|
 (defun stackz-fun (stackz)
  (loop (when (atom stackz) (return)) (setq stackz (cdr stackz)))
  stackz)
|#
;; equivalent, but faster:
(defun stackz-fun (stackz)
  (if (atom stackz) stackz (cdr (last stackz))))

;; (access-in-stack stackz1 stackz2)
;; For the access to local variables in the Stack:
;; Outcome for two Stack-States stackz1 and stackz2, that lie both within
;; the same function and where stackz1 is "deeper" than stackz2:
;; 2 values: NIL and n, if (stackz2) = (STACK+4*n) starting from stackz1,
;;          k and n,    if (stackz2) = ((SP+4*k)+4*n) starting from stackz1.
;; (If stackz2 begins with BLOCK or TAGBODY, always the access to the
;;  consvar of a Block- resp. Tagbody-Frame is assumed.)
(defun access-in-stack (stackz1 stackz2 &aux (k nil) (n 0) (kd (spd 0 0)))
  (loop ;; looping the stacks upwards:
    ;; current STACK is STACK+4*n (for k=NIL) resp. (SP+4*k)+4*n,
    ;; current SP is SP+4*kd (for k=NIL) resp. SP+4*(k+kd).
    (when (eq stackz1 stackz2) (return))
    (when (atom stackz1) (compiler-error 'access-in-stack "STACKZ-END"))
    (let ((item (pop stackz1)))
      (cond ((integerp item) (setq n (+ n item)))
            ((consp item)
             (case (first item)
               (BIND    (setq n (+ n (+ 1 (* 2 (second item))))))
               (TAGBODY (setq kd (spd+ kd (spd 1 1))
                              n (+ n (+ 3 (second item)))))
               (t (compiler-error 'access-in-stack "STACKZ-LISTITEM"))))
            (t
             (case item
               (PROGV          (setq k (if k (spd+ k kd) kd) kd (spd 1 0) n 0))
               (CATCH          (setq kd (spd+ kd (spd 2 1)) n (+ n 3)))
               (UNWIND-PROTECT (setq kd (spd+ kd (spd 2 1)) n (+ n 2)))
               (CLEANUP        (setq k (if k (spd+ k kd) kd) kd (spd 3 0) n 0))
               (BLOCK          (setq kd (spd+ kd (spd 2 1)) n (+ n 3)))
               (MVCALLP        (setq kd (spd+ kd (spd 1 0)) n (+ n 1)))
               ((MVCALL ANYTHING)
                               (setq k (if k (spd+ k kd) kd) kd (spd 1 0) n 0))
               (t (compiler-error 'access-in-stack "STACKZ-ITEM")))))))
  (when (and (consp stackz2) ; on access to BLOCK- resp. TAGBODY-consvar:
             (or (eq (car stackz2) 'BLOCK)
                 (and (consp (car stackz2))
                      (eq (first (car stackz2)) 'TAGBODY))))
    (incf n 2)) ; consvar is located exactly 2 entries higher than frame start
  (values k n))

;; (may-UNWIND stackz1 stackz2)
;; determines, if (UNWIND stackz1 stackz2 for-value) is legal.
;; Therefore it is necessary, that the Compiler knows exactly about
;;  the Frames between stackz1 and stackz2.
(defun may-UNWIND (stackz1 stackz2)
  (loop
    (when (eq stackz1 stackz2) (return t))
    (when (atom stackz1) (compiler-error 'may-UNWIND "STACKZ-END"))
    (when (eq (car stackz1) 'ANYTHING) (return nil))
    (setq stackz1 (cdr stackz1))))

;; (expand-UNWIND stackz1 stackz2 for-value)
;; returns a piece of code equivalent to (UNWIND stackz1 stackz2 for-value),
;; consisting of
;; (SKIP n), (SKIPI k1 k2 n), (SKIPSP k1 k2), (VALUES0),
;; (UNWIND-PROTECT-CLEANUP), (UNBIND1), (BLOCK-CLOSE), (TAGBODY-CLOSE).
;; It must clean the stack - starting from stackz1 - , so that after it the
;; Stack-State stackz2 is there. If for-value=NIL the values can be
;; discarded thereby.
(defun expand-UNWIND (stackz1 stackz2 for-value
                      &aux (k nil) (n 0) (kd (spd 0 0)) (codelist nil))
  (flet ((here () ; up to here, first of all increment the Stacks
           (if k
             (progn
               (push `(SKIPI ,(car k) ,(cdr k) ,n) codelist)
               (unless (> (car kd) 0)
                 (compiler-error 'expand-UNWIND "SP-depth"))
               (when (or (> (car kd) 1) (> (cdr kd) 0))
                 (push `(SKIPSP ,(- (car kd) 1) ,(cdr kd)) codelist)))
             (progn
               (when (> n 0) (push `(SKIP ,n) codelist))
               (when (or (> (car kd) 0) (> (cdr kd) 0))
                 (push `(SKIPSP ,(car kd) ,(cdr kd)) codelist))))
           (setq k nil n 0 kd (spd 0 0))))
    (loop ;; looping the stacks upwards:
      ;; current STACK is STACK+4*n (for k=NIL) resp. (SP+4*k)+4*n,
      ;; current SP is SP+4*kd (for k=NIL) resp. SP+4*(k+kd).
      (when (eq stackz1 stackz2) (here) (return))
      (when (atom stackz1) (compiler-error 'expand-UNWIND "STACKZ-END"))
      (let ((item (car stackz1)))
        (cond ((integerp item) (setq n (+ n item)))
              ((consp item)
               (case (first item)
                 (BIND    (here) (push '(UNBIND1) codelist))
                 (TAGBODY (here) (push '(TAGBODY-CLOSE) codelist))
                 (t (compiler-error 'expand-UNWIND "STACKZ-LISTITEM"))))
              (t
               (case item
                 (PROGV (here) (push '(UNBIND1) codelist) (setq kd (spd 1 0)))
                 (CATCH (setq kd (spd+ kd (spd 2 1)) n (+ n 3)))
                 (UNWIND-PROTECT
                   (here)
                   (unless for-value
                     ;; When for-value=NIL at the the first occurring
                     ;; UNWIND-PROTECT-Frame a '(VALUES0) is inserted.
                     (setq for-value t)
                     (push '(VALUES0) codelist))
                   (push '(UNWIND-PROTECT-CLEANUP) codelist))
                 (CLEANUP (setq k (if k (spd+ k kd) kd) kd (spd 3 0) n 0))
                 (BLOCK (here) (push '(BLOCK-CLOSE) codelist))
                 (MVCALLP (setq kd (spd+ kd (spd 1 0)) n (+ n 1)))
                 (MVCALL (setq k (if k (spd+ k kd) kd) kd (spd 1 0) n 0))
                 (t (compiler-error 'expand-UNWIND "STACKZ-ITEM"))))))
      (setq stackz1 (cdr stackz1)))
    (nreverse codelist)))

;; (spdepth-difference stackz1 stackz2)
;; returns the difference k of SP at stackz1 and SP at stackz2.
;; In order to increment the SP from stackz1 to stackz2,
;; (SKIPSP k1 k2) is sufficient.
(defun spdepth-difference (stackz1 stackz2 &aux (k (spd 0 0)))
  (loop
    (when (eq stackz1 stackz2) (return))
    (when (atom stackz1) (compiler-error 'spdepth-difference "STACKZ-END"))
    (let ((item (car stackz1)))
      (if (consp item)
        (case (first item)
          (TAGBODY (setq k (spd+ k (spd 1 1)))))
        (case item
          ((PROGV MVCALLP MVCALL ANYTHING) (setq k (spd+ k (spd 1 0))))
          ((CATCH UNWIND-PROTECT BLOCK) (setq k (spd+ k (spd 2 1))))
          (CLEANUP (setq k (spd+ k (spd 3 0)))))))
    (setq stackz1 (cdr stackz1)))
  k)



;;;;****        FUNCTION   ENVIRONMENT   MANAGEMENT

;; passed by the Interpreter: %fenv%

;; Interpreter-Function-Environment has the shape
;; %fenv% = NIL or #(f1 def1 ... fn defn NEXT-ENV), NEXT-ENV likewise
;;
;; So a mapping fi --> defi is established.
;; defi = Closure             implies, that defi is the local
;;                            function-definition of fi
;; defi = #<MACRO expander>   denotes a local Macro.
;; defi = #<FUNCTION-MACRO closure expander>
;;                            denotes a local function-definition
;;                            with alternative Macro-Expander
;; defi = NIL                 implies, that a local function-
;;                            definition is still to be added (cf. LABELS)

;; newly constructed:
(defvar *fenv*)
;; contains the new lexical function-bindings.
;; *fenv* has the same shape as %fenv% and ends with %fenv%:
;; #(f1 def1 ... fn defn NEXT-ENV), which establishes a
;;  a mapping fi --> defi.
;; defi = #<MACRO expander>         denotes a local Macro.
;; defi = (fdescr . var)            implies, that the local function-definition
;;           of fi is located at runtime in the lexical variable var.
;;           fnode is the fnode belonging to fi, still NIL at the beginning.
;; defi = (fdescr . const)          implies, that the local function-definition
;;           of fi is autonomously and is located in the constant const.
;;           fnode is the fnode belonging to fi, still NIL at the beginning.
;; fdescr is a Cons (fnode . lambdadescr),
;;           fnode is the fnode belonging to fi or NIL,
;;           lambdadescr = (LABELS . list of values of c-analyze-lambdalist)
;;           or lambdadescr = (GENERIC . Signature) or NIL.
;; defi = (#<MACRO expander> fdescr . {var|const})
;;                                    denotes a local function-definition
;;                                    with alternative Macro-Expander

;; Search the local function-definition of the symbol f in fenv:
;; result:
;; 1. value: T
;; 2. value: as Macro
;;    Macro-Expander                 if local Macrodefinition available
;;    NIL                            if not
;; 3. value and further values: as function
;;    GLOBAL, Vector, Index, NIL     if defi = (svref Vector Index)
;;                                   (so found in %fenv%)
;;    GLOBAL, Vector, Index, T       ditto as FUNCTION-MACRO
;;    LOCAL, def, fdescr             if def = {var|const} = (cdr (last defi))
;;                                   (so found in *fenv* without %fenv%)
;;    NIL                            if only Macro
;; Or:
;; 1. value: NIL                     if not defined locally.
(defun fenv-search (f &optional (fenv *fenv*))
  (let ((from-inside-macrolet nil))
    (loop
      (cond ((null fenv) (return-from fenv-search 'NIL))
            ((simple-vector-p fenv)
             (do ((l (1- (length fenv)))
                  (i 0 (+ i 2)))
                 ((= i l) (setq fenv (svref fenv i)))
               (if (equal f (svref fenv i))
                 (let ((def (svref fenv (1+ i))))
                   (if (and from-inside-macrolet (not (macrop def)))
                     (c-error (TEXT "Invalid access to the local function definition of ~S from within a ~S definition")
                              f 'macrolet)
                     (return-from fenv-search
                       (if (consp def)
                         (if (macrop (car def))
                           (values 'T
                                   (macro-expander (car def))
                                   'LOCAL (cddr def) (cadr def))
                           (values 'T
                                   'NIL
                                   'LOCAL (cdr def) (car def)))
                         (if (macrop def)
                           (values 'T (macro-expander def) 'NIL)
                           (if (function-macro-p def)
                             (values 'T
                                     (function-macro-expander def)
                                     'GLOBAL fenv (1+ i) 'T)
                             (values 'T
                                     'NIL
                                     'GLOBAL fenv (1+ i) 'NIL))))))))))
            ((and (consp fenv) (eq (car fenv) 'MACROLET))
             (setq from-inside-macrolet t)
             (setq fenv (cdr fenv)))
            (t (compiler-error 'fenv-search fenv))))))
;; Determines, if a function-name is not defined in the
;; Function-Environment fenv and hence refers to the global function.
(defun global-in-fenv-p (s fenv)
  (eq (fenv-search s fenv) 'NIL))

;; The Functions MACROEXPAND-1, MACROEXPAND work:
;; With a Vector consisting of
;; - such a Variable-Environment (concatenated Vectors, with
;;   defi = #<SYMBOL-MACRO expansion> for Symbol-Macro-Definitions),
;; - such a Function-Environment (concatenated Vectors, with
;;   defi = (SYSTEM::MACRO . expander) for Macro-Definitions for fi)

;; (MACROEXPAND-1 form env) expands the given Form in the Macroexpansion-
;; Environment env and returns the form expanded once and T
;; (or form and NIL, if not expandable).
;; (MACROEXPAND form env) expands the given Form in the Macroexpansions-
;; Environment env and returns the Form expanded as many times as possible
;; and T (or form and NIL, if not expandable).
;; (PARSE-BODY body docstring-allowed) analyzes the body and detaches
;; the Declarations and the Docstring (if allowed and if existing) .
;; 3 values: the remaining body-rest, a list of the found declspecs,
;; the Docstring (or NIL).

;;;;****           BLOCK   ENVIRONMENT   MANAGEMENT

;; passed by the Interpreter: %benv%

;; Interpreter-Block-Environment has the shape
;; %benv% = ((name1 . status1) ... (namen . statusn))
;; where namei is a Symbol and statusi is the state
;; of this lexically comprising
;;  Block: #<DISABLED> if the Block has already been left, else a
;;  Pointer in the Stack to the belonging Block-Frame.

;; newly constructed:
(defvar *benv*)

;; *benv* has the shape
;; ((name1 . block1) ... (namen . blockn) . %benv%)
;; where blocki is the Descriptor of the Block with Name namei:
(defstruct (block (:copier nil))
  fnode             ; Function, in which this Block is defined, an FNODE
  label             ; label, which finishes this Block
  stackz            ; Stack-State after the construction of the Block-Frame
  consvar           ; Variable, that lies in the Stack in the Block-Frame and
                    ; which contains the Block-Cons (whose CDR is set to
                    ; #<DISABLED>  on leaving the block)
  used-far          ; flag, indicates, if this Block is left out of a
                    ; different function with RETURN-FROM.
  for-value         ; specifies, if the whole block-construction has to
                    ; return values.
)
#-CLISP-DEBUG (remprop 'block 'clos::closclass)

;; Searches for a block with Name name and returns:
;; NIL                          if not found,
;; Block-Descriptor             if found in *benv* ,
;; Block-Cons (name . status)   if found in %benv% .
(defun benv-search (name &optional (benv *benv*))
  (loop
    (when (atom benv) (return nil))
    (when (eq (caar benv) name)
      (if (block-p (cdar benv))
        (return (cdar benv))
        (return (car benv))))
    (setq benv (cdr benv))))


;;;;****         TAGBODY   ENVIRONMENT   MANAGEMENT

;; passed by the Interpreter: %genv%

;; Interpreter-Tagbody-Environment has the shape
;; %genv% = ((Tagvektor1 . status1) ... (Tagvektorn . statusn))
;; where Tagvektori is a simple-vector, that contains the Tags that can
;; be jumped at, statusi is the state of this lexically comprising Tagbody:
;; #<DISABLED> if the Tagbody has already been left, else a
;; Pointer in the Stack to the belonging Tagbody-Frame.

;; newly constructed:
(defvar *genv*)

;; *genv* has the shape
;; ((Tagvektor1 . tagbody1) ... (Tagvektorn . tagbodyn) . %genv%)
;; where tagbodyi is the Descriptor of Tagbody i:
(defstruct (tagbody (:copier nil))
  fnode             ; Function, in which this Tagbody is defined, an FNODE
  labellist         ; list of Labels, parallel to the  tag-vector
  stackz            ; Stack-State after the construction of the Tagbody-Frame
  consvar           ; Variable, that lies in the Stack in the Tagbody-Frame and
                    ; which contains the Tagbody-Cons (whose CDR is set to
                    ; #<DISABLED> on leaving the Tagbody)
  used-far          ; list of all the Tags that are jumped at with a GO from
                    ; within another function.
)
#-CLISP-DEBUG (remprop 'tagbody 'clos::closclass)

;; Searches for a tag with Namen name and returns:
;; NIL                                         if not found,
;; Tagbody-Descriptor, Index                   if found in *genv* ,
;; Tagbody-Cons (Tagvektor . status), Index    if found in %genv% .
(defun genv-search (name &optional (genv *genv*))
  (loop
    (when (atom genv) (return nil))
    (do* ((v (caar genv))
          (l (length v))
          (i 0 (1+ i)))
         ((= i l))
      (when (eql (svref v i) name)
        (return-from genv-search
          (values (if (tagbody-p (cdar genv)) (cdar genv) (car genv)) i))))
    (setq genv (cdr genv))))


;;;;****       VARIABLE   ENVIRONMENT   MANAGEMENT

;; passed by the Interpreter: %venv%

;; Interpreter-Variablen-Environment has the shape
;; %venv% = NIL or #(v1 val1 ... vn valn NEXT-ENV),
;; NEXT-ENV of the same shape.
(defparameter specdecl
  (eval
   '(let ((*evalhook*
            #'(lambda (form env)
                (declare (ignore form))
                ;; The Evalhook-Mechanism passes the Environment.
                ;; (svref...0) thereof is the Variable-Environment,
                ;; (svref...1) thereof is the associated "value"
                ;; #<SPECIAL REFERENCE> from the *evalhook*-binding.
                (svref (svref env 0) 1))))
     0)))
;; determines, if the Symbol var represents a Special-Variable
(defun proclaimed-special-p (var)
  (or (sys::special-variable-p var)
      (not (null (memq var *known-special-vars*)))))

;; newly constructed:
(defvar *venv*)                  ; Variable-Environment, fine-grained
(defvar *venvc*)                 ; Variable-Environment, coarse-grained

;; *venv* has the same shape as %venv% and ends with %venv%:
;; #(v1 var1 ... vn varn NEXT_ENV), where vari are Variable-Constructs or
;; Symbolmacros or Interpreter-Values and NEXT-ENV has the same shape.

;; *venvc* simulates the Runtime-Variable-Environment at runtime, as far
;; as it involves Closure-Variables.
;; *venvc* has the shape
;; (item1 ... itemn)
;; each item is
;;   NIL :            a LET/LET*/MULTIPLE-VALUE-BIND/Function-Entry/
;;                    FLET/LABELS, that does not create a Closure
;;   fnode :          a new Function
;;   ((var1 ... vark) . stackz) : the variables Var1, ..., Vark get
;;                    into a closure by LET/LET*/MULTIPLE-VALUE-BIND/
;;                    Function-Entry/FLET/LABELS.
;;                    this Closure is located in the Stack; the Stack-State
;;                    is specified, where it is reachable.

;; A Variable is described by either being special or - if lexical -
;; by the Stack-Structure being determined after the creation of the Variable
;; in the Stack resp. the place in the Closure.
(defstruct (var (:copier nil))
  (name nil :read-only t)  ; Symbol
  (specialp nil :read-only t) ; declared special (or bound lexically) ?
  constantp                ; Constant ?
  constant                 ; if Constant: Value and Origin of the Constant
                           ;   (the value is known at Compile-Time)
  usedp                    ; if lexically:
                           ;   was the Variable ever used ?
                           ;   (A list of references to the variable
                           ;    terminated by NIL or T)
  for-value-usedp          ; if lexically:
                           ;   was the Variable ever used for-value ?
  really-usedp             ; if lexically:
                           ;   was the Variable ever really
                           ;   (in order to know the value) used ?
  (assignedp nil)          ; if lexically:
                           ;   was a value ever assigned to the Variable?
  (modified-list '())      ; if lexically: for each SET on the Variable:
                           ;   a List (value-anode set-anode . for-value)
  (replaceable-list '())   ; if lexically:
                           ;   for each movable-Variable, that has the same
                           ;   value during its entire Existence like this one
                           ;   and therefore is replaceable: a
                           ;   List (var init-anode . bind-anode)
  closurep                 ; if lexically:
                           ;   NIL if in the Stack, T if in the Closure
  (stackz nil :read-only t); if lexically:
                           ;   Stack-State after creation of the Variable
                           ;   (if Variable is in Stack: its location in Stack)
  (venvc nil :read-only t) ; if lexically and in the Closure:
                           ;   the *venvc*, in whose first Item this
                           ;   Variable occurs.
  (fnode nil :read-only t) ; function containing this variable, an FNODE
)

;; (venv-search v) searches in *venv* for a Variable with the Symbol v.
;; result:
;; NIL                   if not found
;; SPECIAL               if found as a special-declared variable
;; LOCAL, vector, index  if interpretatively lexically bound, value in vector
;; T, var                if lexically bound, in Stack or in the Closure
(defun venv-search (v &optional (venv *venv*))
  (when (or (constantp v) (proclaimed-special-p v))
    (return-from venv-search 'SPECIAL))
  (let ((from-inside-macrolet nil))
    (loop
      (cond ((null venv) (return-from venv-search 'NIL))
            ((simple-vector-p venv)
             (do ((l (1- (length venv)))
                  (i 0 (+ i 2)))
                 ((= i l) (setq venv (svref venv i)))
               (if (eq v (svref venv i))
                 (let ((val (svref venv (1+ i))))
                   (if (and from-inside-macrolet
                            (not (eq val specdecl))
                            (not (symbol-macro-p val)))
                     (c-error (TEXT "Invalid access to the value of the lexical variable ~S from within a ~S definition")
                              v 'macrolet)
                     (return-from venv-search
                       (if (and (var-p val) #| (eq (var-name val) v) |# )
                         (progn
                           (assert (not (var-specialp val)))
                           (values T val))
                         (if (eq val specdecl)
                           'SPECIAL
                           (values 'LOCAL venv (1+ i))))))))))
            ((and (consp venv) (eq (car venv) 'MACROLET))
             (setq from-inside-macrolet t)
             (setq venv (cdr venv)))
            (t (compiler-error 'venv-search venv))))))

;; (venv-search-macro v) searches in *venv* for a Variable with the Symbol v.
;; result:
;;   if v is a Symbol-Macro:  T, Expansion.
;;   else:                              NIL.
(defun venv-search-macro (v &optional (venv *venv*))
  (multiple-value-bind (a b c) (venv-search v venv)
    (case a
      ((NIL) (symbol-macro-expand v))
      ((LOCAL) (and (symbol-macro-p (svref b c))
                    (values t (sys::%record-ref (svref b c) 0))))
      (t nil))))

;; (push-*venv* var1 ... varn) extends *venv* by var1, ..., varn,
;; so to speak like (dolist (v (list var1 ... varn)) (push v *venv*)).
(defun push-*venv* (&rest varlist)
  (when varlist
    #|; the alternative definition conses less but is longer and thus slower
    (let* ((len (ash (length varlist) 1)) (idx -1)
           (ret (make-array (1+ len))))
      (setf (svref ret len) *venv* *venv* ret)
      (dolist (var varlist *venv*)
        (setf (aref ret (incf idx)) (var-name var)
              (aref ret (incf idx)) (if (var-specialp var) specdecl var))))
    |#
    (let ((l (list *venv*)))
      (dolist (var varlist)
        (setq l (list* (var-name var) (if (var-specialp var) specdecl var) l)))
      (setq *venv* (apply #'vector l)))))

;; (access-in-closure var venvc stackz)
;; returns for a Closure-Variable var, how one can access it
;; (from a location, where the Stack and the Closure-Environment
;;  are described by stackz and venvc):
;; 3 values k, n, m; the Variable is located in (svref ... 1+m)
;;     nil, n, m  : (STACK+4*n)
;;     k, nil, m  : (svref ... 0)^k VenvConst
;;     k, n,   m  : ((SP+4*k)+4*n)
(defun access-in-closure (var venvc stackz &aux (k nil) n)
  ;; coarse loop, determines the Closure-depth k at VenvConst:
  (loop
    (when (eq venvc (var-venvc var)) (return))
    (when (atom venvc)
      (compiler-error 'access-in-closure (list 'loop (var-name var))))
    (let ((item (pop venvc)))
      (if (null k)
        ;; start of count, (not (listp item)) == (fnode-p item)
        (when (not (listp item)) (setq k 0))
        (when (consp item) (incf k))))) ; count
  (if k
    (setq n nil)
    (multiple-value-setq (k n) (access-in-stack stackz (cdr (first venvc)))))
  (let ((m (do ((L (car (first venvc)) (cdr L))
                (i 0 (1+ i)))
               ((eq (car L) var) i)
             (when (null L)
               (compiler-error 'access-in-closure
                               (list 'do (var-name var)))))))
    (values k n m)))

;;;;****             CONSTANT   MANAGEMENT

;; A Constant is a Box with the value of the Constant:
(defstruct (const (:copier nil))
  value         ; value of the Constant
  form          ; form, that evaluates to value
  ltv-form      ; (LOAD-TIME-VALUE form) as it occurs in the source, or nil
  horizon       ; validity range of value and form:
                ; :VALUE  -  only value is valid
                ;            (then form is implicitly: form = `(QUOTE ,value) )
                ; :ALL    -  value and form are both valid
                ; :FORM   -  only form is valid
  ;; For *compiling-from-file* = nil only :VALUE and :ALL are possible.
  ;; What is filled into the Fnode in the 3. Pass, is:
  ;;   For *compiling-from-file* = nil: only value.
  ;;   For *compiling-from-file* /= nil:
  ;;     If (eq horizon ':value), value, else form.
)
(defun const-value-safe (c)
  (if (eq (const-horizon c) :form)
    (compiler-error 'const-value c)
    (const-value c)))

;; In the 2nd Pass Variables with constantp=T are treated as Constants.


;;;;****           DECLARATION    MANAGEMENT

(defparameter *declaration-types*
  '(special ; Bindings
    type ftype function ; Types
    inline notinline ; Function-Compilation
    ignore optimize dynamic-extent ; Compiler-Hints
    declaration ; Additional Declarations
    ;; Types according to table 4-1 :
    array atom base-char base-string bignum bit bit-vector boolean character
    compiled-function complex cons double-float extended-char fixnum
    float function hash-table integer keyword list long-float nil null number
    package pathname random-state ratio rational readtable real sequence
    short-float simple-array simple-base-string simple-bit-vector
    simple-string simple-vector single-float standard-char stream string
    cs-cl:string string-char symbol t vector
    ;; Additional Declarations:
    compile ; statement, that the form resp. function is to be compiled
    sys::source ; the Source-Lambdabody (unexpanded) within a Lambdabody
    sys::in-defun ; indicates, which global function the Code belongs to
    ignorable ; marks variables as possibly ignorable
              ; (aside: Gensym-Variable are always automatically ignorable.)
    sys::read-only)) ; marks Variables as not assigned

;; passed by the Interpreter: %denv%

;; newly constructed:
(defvar *denv*)
;; *denv* has the same shape as %denv% and ends with %denv%.
;; *denv* has the shape (item1 ... itemn), with each item having the
;; construction type (declaration-type argument ...) .
;; special treatment of
;;   SPECIAL : is omitted, noted in *venv* instead.
;;   IGNORE, IGNORABLE : is omitted, worked up itself in the
;;                       processing form instead.
;; Additional Declaration (INLINING symbol) against recursive Inlining.

;; (declared-notinline fun denv) determines, if fun - a Symbol pointing to a
;; global function, which is not shadowed by a local function-definition -
;; is declared as NOTINLINE in denv.
;; How about local functions ??
(defun declared-notinline (fun &optional (denv *denv*))
  (when (member `(INLINING ,fun) *denv* :test #'equal)
    ;; never recursively expand a function inline!
    (return-from declared-notinline t))
  (loop
    (when (atom denv)
      (when *compiling-from-file*
        (when (member fun *notinline-functions* :test #'equal) (return t))
        (when (member fun *inline-functions* :test #'equal) (return nil)))
      (return (eq (get (get-funname-symbol fun) 'inlinable) 'notinline)))
    (let ((declspec (car denv)))
      (when (and (eq (car declspec) 'INLINE)
                 (member fun (cdr declspec) :test #'equal))
        (return nil))
      (when (and (eq (car declspec) 'NOTINLINE)
                 (member fun (cdr declspec) :test #'equal))
        (return t)))
    (setq denv (cdr denv))))

;; (declared-constant-notinline sym denv) determines,
;; if sym - a symbol pointing to a global constant,
;; that is not shadowed by a local variable-definition -
;; is declared as CONSTANT-NOTINLINE in denv.
(defun declared-constant-notinline (sym &optional (denv *denv*))
  (loop
    (when (atom denv)
      (when *compiling-from-file*
        (when (memq sym *notinline-constants*) (return t))
        (when (memq sym *inline-constants*) (return nil)))
      (return (eq (get sym 'constant-inlinable) 'constant-notinline)))
    (let ((declspec (car denv)))
      (when (and (eq (car declspec) 'CONSTANT-INLINE)
                 (memq sym (cdr declspec)))
        (return nil))
      (when (and (eq (car declspec) 'CONSTANT-NOTINLINE)
                 (memq sym (cdr declspec)))
        (return t)))
    (setq denv (cdr denv))))

;; (declared-declaration sym denv) determines whether sym is a valid
;; declaration specifier in denv.
(defun declared-declaration (sym &optional (denv *denv*))
  (loop
    (when (atom denv)
      (return nil))
    (let ((declspec (car denv)))
      (when (and (eq (first declspec) 'DECLARATION) (memq sym (rest declspec)))
        (return t)))
    (setq denv (cdr denv))))

;; top-level optimization settings
(defconstant *optimize* #.(make-hash-table :test 'stablehash-eq))

;; (declared-optimize quality) returns the optimization level for the given
;; quality, as an integer between 0 and 3 (inclusive).
;; quality - one of COMPILATION-SPEED, DEBUG, SAFETY, SPACE, SPEED.
;; Current/future assignments:
;; SAFETY >= 3 => keep function calls even to functions without side-effects
;;                (for ANSI CL 3.5)
;;        >= 2 =>
;;        >= 1 =>
;; DEBUG >= 3 =>
;;       >= 2 => every function has an exit restart [not implemented yet]
;;       >= 1 =>
;; SPACE >= 3 =>
;;       >= 2 =>
;;       >= 1 =>
;; SPEED >= 3 =>
;;       >= 2 =>
;;       >= 1 =>
;; COMPILATION-SPEED >= 3 =>
;;                   >= 2 =>
;;                   >= 1 =>
(defun declared-optimize (quality &optional (denv *denv*))
  (loop
    (when (atom denv)
      (return-from declared-optimize (gethash quality *optimize* 1)))
    (let ((declspec (car denv)))
      (when (eq (car declspec) 'OPTIMIZE)
        ;; We can use ASSOC here, because PROCESS-DECLARATIONS has
        ;; canonicalized the syntax to (quality value).
        (let ((spec (assoc quality (cdr declspec) :test #'eq)))
          (when spec
            (return-from declared-optimize (second spec))))))
    (setq denv (cdr denv))))

;; return 2 values: the quality and the value
;; or issue a warning and return NIL
(defun parse-optimize-quality (spec)
  (macrolet ((broken (&rest args)
               `(progn
                  (funcall (if (boundp '*warning-count*) #'c-warn 'warn) ,@args)
                  (values))))
    (let ((quality spec) (value 3))
      (if (or (symbolp spec)
              (and (consp spec) (symbolp (setq quality (car spec)))
                   (consp (cdr spec)) (realp (setq value (cadr spec)))
                   (null (cddr spec))))
        (if (memq quality '(COMPILATION-SPEED DEBUG SAFETY SPACE SPEED))
          (if (typep value '(INTEGER 0 3))
            (values quality value)
            (broken (TEXT "Not a valid optimization level for ~S, should be one of 0, 1, 2, 3: ~S")
                    quality value))
          (broken (TEXT "~S is not a valid ~S quality.") 'optimize quality))
        (broken (TEXT "Not a valid ~S specifier: ~S") 'optimize spec)))))

;; (process-declarations declspeclist) analyzes the declarations (as they come
;; from PARSE-BODY) and returns:
;; a fresh list of the Special-declared symbols,
;; a fresh list of the Ignore-declared symbols,
;; a fresh list of the Ignorable-declared symbols,
;; a fresh list of the Read-Only-declared symbols,
;; a fresh list of other declaration specifiers.
(defun process-declarations (declspeclist)
  (setq declspeclist (nreverse declspeclist))
  (let ((specials '())
        (ignores '())
        (ignorables '())
        (readonlys '())
        (other '()))
    (dolist (declspec declspeclist)
      (if (or (atom declspec) (cdr (last declspec)))
        (c-warn (TEXT "Bad declaration syntax: ~S~%Will be ignored.")
                declspec)
        (let ((declspectype (car declspec)))
          (if (and (symbolp declspectype)
                   (or (memq declspectype *declaration-types*)
                       (declared-declaration declspectype)
                       (and *compiling-from-file*
                            (memq declspectype *user-declaration-types*))))
            (case declspectype
              ((SPECIAL)
               (dolist (x (cdr declspec))
                 (if (symbolp x)
                   (push x specials)
                   (c-warn (TEXT "Non-symbol ~S may not be declared SPECIAL.")
                           x))))
              ((IGNORE)
               (dolist (x (cdr declspec))
                 (if (symbolp x)
                   (push x ignores)
                   (c-warn (TEXT "Non-symbol ~S may not be declared IGNORE.")
                           x))))
              ((IGNORABLE)
               (dolist (x (cdr declspec))
                 (if (symbolp x)
                   (push x ignorables)
                   (c-warn (TEXT "Non-symbol ~S may not be declared IGNORABLE.")
                           x))))
              ((SYS::READ-ONLY)
               (dolist (x (cdr declspec))
                 (if (symbolp x)
                   (push x readonlys)
                   (c-warn (TEXT "Non-symbol ~S may not be declared READ-ONLY.")
                           x))))
              (t
               ;; Syntax check.
               (case declspectype
                 (TYPE
                  (setq declspec
                        (list* declspectype (second declspec)
                               (mapcan #'(lambda (x)
                                           (if (symbolp x)
                                             (list x)
                                             (progn
                                               (c-warn (TEXT "Non-symbol ~S may not be subject to a TYPE declaration.")
                                                       x)
                                               '())))
                                       (cddr declspec)))))
                 (FTYPE
                  (setq declspec
                        (list* declspectype (second declspec)
                               (mapcan #'(lambda (x)
                                           (if (function-name-p x)
                                             (list x)
                                             (progn
                                               (c-warn (TEXT "~S is not a function name and therefore may not be subject to a FTYPE declaration.")
                                                       x)
                                               '())))
                                       (cddr declspec)))))
                 ((INLINE NOTINLINE)
                  (setq declspec
                        (cons declspectype
                              (mapcan #'(lambda (x)
                                          (if (function-name-p x)
                                            (list x)
                                            (progn
                                              (c-warn (TEXT "~S is not a function name and therefore may not be declared ~S.")
                                                      x declspectype)
                                              '())))
                                      (cdr declspec)))))
                 (OPTIMIZE
                  (setq declspec
                        (cons declspectype
                              ;; Canonicalize, for easier search by quality.
                              (mapcan #'(lambda (x)
                                          (multiple-value-bind (q v)
                                              (parse-optimize-quality x)
                                            (and q `((,q ,v)))))
                                      (cdr declspec)))))
                 (DYNAMIC-EXTENT
                  (setq declspec
                        (cons declspectype
                              (mapcan #'(lambda (x)
                                          (if (or (symbolp x)
                                                  (and (consp x) (eq (car x) 'FUNCTION)
                                                       (consp (cdr x))
                                                       (function-name-p (cadr x))
                                                       (null (cddr x))))
                                            (list x)
                                            (progn
                                              (c-warn (TEXT "Not a valid DYNAMIC-EXTENT specifier: ~S")
                                                      x)
                                              '())))
                                      (cdr declspec)))))
                 (DECLARATION
                  (setq declspec
                        (list* declspectype (second declspec)
                               (mapcan #'(lambda (x)
                                           (if (symbolp x)
                                             (list x)
                                             (progn
                                               (c-warn (TEXT "Non-symbol ~S may not be subject to a DECLARATION declaration.")
                                                       x)
                                               '())))
                                       (cddr declspec)))))
                 (COMPILE
                  (when (cdr declspec)
                    (c-warn (TEXT "The arguments of a COMPILE declaration are ignored: ~S")
                            declspec)
                    (setq declspec '(COMPILE)))))
               (push declspec other)))
            (c-warn (TEXT "Unknown declaration ~S.~%The whole declaration will be ignored.")
                    declspectype declspec)))))
    (values specials ignores ignorables readonlys other)))

;; (push-*denv* declspecs) extends *denv* by the declspecs.
;; declspecs must be a freshly consed list.
(defun push-*denv* (declspecs)
  (setq *denv* (nreconc declspecs *denv*)))


;;;;****             FUNCTION   MANAGEMENT

;; An FNODE contains the necessary Information for a Function:
(defstruct (fnode (:copier nil))
  name            ; Name, a Symbol or (SETF symbol)
  code            ; code of this function (first nothing, then an ANODE,
                  ; then a Closure)
  enclosing       ; function lexically containing this one, or NIL
  ;; Descriptions for the coming Closure:
  venvconst       ; Flag, if the Venv of this function has to be passed
                  ; explicitly on construction (or is always NIL)
  venvc           ; lookout of the Venv, that has to be passed to this function
                  ; on construction (if at all)
  Blocks-Offset   ; number of constants so far
  (Blocks nil)    ; List of Block-Constructs, that have to be passed
                  ; to this function on construction
  Tagbodys-Offset ; number of constants so far
  (Tags nil)      ; list of (tagbody . tag) defined in enclosing functions but
                  ; used by this function
  (Tagbodys nil)  ; List of Tagbody-Constructs, that have to be passed to this
                  ; function on construction
  Keyword-Offset  ; number of local constants so far
                  ; = start offset of the Keywords in FUNC
                  ; (equal to=0 if and only if the function is autonomously)
  (req-anz 0)     ; number of required parameters
  (opt-anz 0)     ; number of optional parameters
  (rest-flag nil) ; Flag, if &REST - Parameter is specified.
  (keyword-flag nil) ; Flag, if &KEY - Parameter is specified.
  (keywords nil)  ; List of Keyword-Constants (in the right order)
  allow-other-keys-flag ; &ALLOW-OTHER-KEYS-Flag
  Consts-Offset   ; number of local constants so far
  (consts nil)    ; List of other constants of this function
                  ; this list is built up foremost in the second pass.
  (consts-forms nil) ; List of poss. Forms, that result in these constants
  (consts-ltv-forms nil) ; List of poss. (load-time-value ...) source forms
                  ; that correspond to these constants
  gf-p            ; Flag, if a generic function is produced
                  ; (implies Blocks-Offset = Tagbodys-Offset = Keyword-Offset
                  ;  = 0 or 1)
  ;; Notification of changes needed to be done in the enclosing function.
  far-used-vars     ; list of variables defined in enclosing functions but
                    ; used by this function
  far-assigned-vars ; list of variables defined in enclosing functions but
                    ; assigned by this function
  far-used-blocks   ; list of blocks defined in enclosing functions but
                    ; used by this function
  far-used-tagbodys ; list of (tagbody . tag) defined in enclosing
                    ; functions but used by this function
)

;; the current function, an FNODE:
(defvar *func*)
;; the Label at the beginning of the Code of the current function:
(defvar *func-start-label*)

;; number of the so far occurred anonymous functions in the current function
;; (Lambda-Expressions):
(defvar *anonymous-count*)

;; *no-code* = T implies, that no Code is to be produced:
(defvar *no-code*)
;; this prevents, that Variables are put into the closure unnecessarily or
;; Optimizations are omitted unnecessarily.

;; Note that a variable is used by an inner fnode.
(defun note-far-used-var (var)
  (if (eq (var-fnode var) *func*)
    (setf (var-closurep var) t
          (var-really-usedp var) t)
    (pushnew var (fnode-far-used-vars *func*))))

;; Note that a variable is assigned by an inner fnode.
(defun note-far-assigned-var (var)
  (if (eq (var-fnode var) *func*)
    (setf (var-closurep var) t)
    (pushnew var (fnode-far-assigned-vars *func*))))

;; Note that a block is used by an inner fnode.
(defun note-far-used-block (block)
  (do ((fnode *func* (fnode-enclosing fnode)))
      ((eq fnode (block-fnode block)))
    (pushnew block (fnode-blocks fnode)))
  (setf (block-used-far block) t))

;; Note that a tag of a tagbody is used by an inner fnode.
(defun note-far-used-tagbody (tagbody+tag)
  (do* ((tagbody (car tagbody+tag)) (tb-fn (tagbody-fnode tagbody))
        (fnode *func* (fnode-enclosing fnode)))
       ((eq tb-fn fnode)
        (pushnew tagbody+tag (tagbody-used-far tagbody)))
    (pushnew tagbody+tag (fnode-Tags fnode))
    (pushnew tagbody (fnode-Tagbodys fnode))))

(defun propagate-far-used (fnode)
  ;; Propagate dependencies of fnode to the enclosing *func*.
  (mapc #'note-far-used-var (fnode-far-used-vars fnode))
  (mapc #'note-far-assigned-var (fnode-far-assigned-vars fnode))
  (mapc #'note-far-used-block (fnode-far-used-blocks fnode))
  (mapc #'note-far-used-tagbody (fnode-far-used-tagbodys fnode))
  ;; Nothing more to propagate, if propagate-far-used is called again.
  (setf (fnode-far-used-vars fnode) nil)
  (setf (fnode-far-assigned-vars fnode) nil)
  (setf (fnode-far-used-blocks fnode) nil)
  (setf (fnode-far-used-tagbodys fnode) nil))


;;;;****                 FORMS   MANAGEMENT

;; On each recursion the following variables are bound dynamically:
(defvar *form*)      ; the current form
(defvar *for-value*) ; if and which values of the Form are relevant:
                     ; NIL : values are irrelevant
                     ; ONE : only the first value is relevant
                     ; ALL : all values are relevant

;; An ANODE is the encoding of the information, that is needed for the
;; compilation of a form.
(defstruct (anode
             (:constructor mk-anode (#+CLISP-DEBUG source
                                     type
                                     #+CLISP-DEBUG sub-anodes
                                     seclass
                                     code
                                     #+CLISP-DEBUG stackz))
             (:copier nil))
  #+CLISP-DEBUG
  source        ; the source pertaining to this Form, mostly a Form
                ; (only needed for debugging purposes)
  type          ; Type of the ANODE (CALL, PRIMOP, VAR, LET, SETQ, ...)
  #+CLISP-DEBUG
  sub-anodes    ; all ANODEs of the sub-forms
  seclass       ; side effect classification
  code          ; generated LAP-Code, a List of LAP-statements and ANODEs
  #+CLISP-DEBUG
  stackz)       ; state of the Stacks on entry into the belonging LAP-Code
;; (make-anode ...) is the same as mk-anode, only that the arguments
;; are marked with keywords and unnecessary components
;; may stand there nevertheless because of #+CLISP-DEBUG.
(defmacro make-anode (&key
                      (source `*form*)
                      type
                      (sub-anodes `'())
                      seclass
                      code
                      (stackz `*stackz*))
  #-CLISP-DEBUG (declare (ignore source sub-anodes stackz))
  `(mk-anode #+CLISP-DEBUG ,source
             ,type
             #+CLISP-DEBUG ,sub-anodes
             ,seclass
             ,code
             #+CLISP-DEBUG ,stackz))

;; A Side-Effect-Class (SECLASS) is EITHER an Indicator
;;   (uses modifies uses-binding):
;; uses = NIL : this Anode can not be influenced by side-effects,
;;        List : this Anode depends on the value of the variables in the list,
;;        T : this Anode can possibly be influenced by every side-effect.
;; modifies = NIL : this Anode produces no side-effects
;;            list : ... produces side-effects only on the values of the
;;                    Variables in the list
;;            T : ... produces side-effects of unknown dimension.
;; uses-binding = NIL or list : this Anode depends on the fact that a binding
;;                              for the listed variables is in effect or not.
;;                              Only dynamic variables are in this list.
;;                T : ... can be influenced by binding any dynamic variable.
;; (Here, variables are VAR-Structures for lexical variables and symbols for
;; dynamic variables.)
;; Consequently:
;;   If the value is uninteresting, an ANODE with SECLASS-modifies=NIL
;;   can be omitted.
;;   For ANODEs with SECLASS, whose uses- and modifies-part are disjoint,
;;   the order of evaluation may be permuted.

;; FIXME1: foldability of compiled closures is not detected
;; FIXME2: RETURN-FROM, GO, HANDLER-BIND ==> *dirty-seclass*

;; SECLASS is a list, so that we can use MAPCAR in SECLASS-OR, SECLASS-WITHOUT.
;; If you modify SECLASS structure, you also need to update
;;  * the comment above
;;  * seclass_object() in lispbibl.d
;;  * parse_seclass() in record.d
(defstruct (seclass (:type list))
  (uses nil :read-only t)
  (modifies nil :read-only t)
  (uses-binding nil :read-only t))

(defconstant *seclass-pure* (make-seclass))
(defconstant *seclass-read* (make-seclass :uses 'T))
(defconstant *seclass-dirty*
  (make-seclass :uses 'T :uses-binding 'T :modifies 'T))

;; Side-Effect-Classes can also be attached to functions, not only to anodes.
;; The SECLASS of a function can also be:
;; NIL, which means that the function is constant-FOLDABLE
;;   (i.e. two calls with identical arguments give the same result,
;;    and calls with constant arguments can be evaluated at compile time;
;;    in particular, no side effects, does not depend on global variables or
;;    such, and does not even look "inside" its arguments).
(proclaim '(inline seclass-foldable-p))
(defun seclass-foldable-p (seclass) (null seclass))
(defconstant *seclass-foldable* NIL)

;; (seclass-or class1 class2) determines the total class of execution
(defun seclass-or (seclass1 seclass2)
  (if (seclass-foldable-p seclass1)
    (if (seclass-foldable-p seclass2)
      *seclass-foldable*
      seclass2)
    (if (seclass-foldable-p seclass2)
      seclass1
      (mapcar #'(lambda (l1 l2)
                  (if (or (eq l1 'T) (eq l2 'T))
                    'T
                    (union l1 l2)))
              seclass1 seclass2))))

;; So that the list of sub-anodes does not have to be calculated, however
;; the anode's side-effect-class belonging to this list can be calculated:
(defmacro anodes-seclass-or (&rest anodeforms)
  (reduce #'(lambda (form1 form2) `(SECLASS-OR ,form1 ,form2)) anodeforms
          :key #'(lambda (anodeform) `(ANODE-SECLASS ,anodeform))
          :initial-value *seclass-foldable*))
(define-modify-macro seclass-or-f (anode) seclass-or-anode)
(defmacro seclass-or-anode (seclass anode)
  `(SECLASS-OR ,seclass (ANODE-SECLASS ,anode)))
(defun anodelist-seclass-or (anodelist)
  (reduce #'seclass-or anodelist :key #'anode-seclass
          :initial-value *seclass-foldable*))

;; side-effects to lexical variables bound further inwards don't count
;; and are therefore eliminated:
(defun seclass-without (seclass varlist)
  ;; (dynamic variables are not eliminated; they are contained in varlist
  ;; as VAR-structures and in seclass as symbols.)
  (if (seclass-foldable-p seclass)
    *seclass-foldable*
    (mapcar #'(lambda (l)
                (if (eq l 'T)
                  'T
                  ;; same as (set-difference l varlist)
                  (remove-if #'(lambda (var) (memq var varlist)) l)))
            seclass)))

;; determines, if the order of evaluation of two anodes can be permuted -
;; so long as the stack-states permit this.
(defun anodes-commute (anode1 anode2)
  (seclasses-commute (anode-seclass anode1) (anode-seclass anode2)))
(defun seclasses-commute (seclass1 seclass2)
  (flet ((disjoint-p (uses modifies)
           (or (null uses) (null modifies)
               (and (not (eq uses 'T)) (not (eq modifies 'T))
                    (null (intersection uses modifies))))))
    (and (disjoint-p (seclass-uses seclass1) (seclass-modifies seclass2))
         (disjoint-p (seclass-uses seclass2) (seclass-modifies seclass1)))))

(proclaim '(inline anode-side-effect-free-p))
(defun anode-side-effect-free-p (anode)
  (null (seclass-modifies (anode-seclass anode))))

;;;;****            AUXILIARY   FUNCTIONS

;; disjoints a function name in Package and String.
(defun get-funname-string+pack (funname)
  (if (atom funname)
    (values (symbol-name funname) (symbol-package funname))
    (values (string-concat "(" (symbol-name (first funname)) " "
                               (symbol-name (second funname)) ")")
            (symbol-package (second funname)))))

;; returns a function name, that is composed by the package and the
;; printname of a given function name, a hyphen and a suffix.
(defun symbol-suffix (funname suffix)
  ;; We have 8 cases:
  ;;
  ;;        \ suffix   |  function name      number
  ;; funname \         |
  ;; --------------------------------------------------------------------
  ;; nil               |  suffix             suffix as uninterned symbol
  ;; #:symbol          |  suffix             concatenate
  ;; pack::symbol      |  concatenate        concatenate
  ;; list              |  concatenate        concatenate
  ;; --------------------------------------------------------------------
  (if (and (symbolp funname) (not (and funname (symbol-package funname)))
           (function-name-p suffix))
    suffix
    (progn
      ;; convert suffix to a string:
      (cond ((symbolp suffix) (setq suffix (symbol-name suffix)))
            ((not (stringp suffix))
             (setq suffix (write-to-string suffix :escape nil :base 10
                                           :radix nil :readably nil))))
      (if funname
        (multiple-value-bind (name pack) (get-funname-string+pack funname)
          ;; build new symbol:
          (let ((new-name (string-concat name "-" suffix)))
            (if pack (intern new-name pack) (make-symbol new-name))))
        (make-symbol suffix)))))

;; (C-COMMENT controlstring . args)
;; issue additional information from the compiler (via FORMAT).
(defun c-comment (cstring &rest args)
  (let ((dest (if *compile-verbose* *c-error-output* *c-listing-output*)))
    (when dest
      (fresh-line dest)
      (apply #'format dest cstring args)
      (elastic-newline dest))))

(defstruct c-source-point
  (lineno1 *compile-file-lineno1*)
  (lineno2 *compile-file-lineno2*)
  (file *compile-file-truename*))

;; (C-SOURCE-LOCATION)
;; returns a description of the location in the source.
(defun c-source-location (&optional (lineno1 *compile-file-lineno1*)
                          (lineno2 *compile-file-lineno2*)
                          (file *compile-file-truename*))
  (if (and file lineno1 lineno2)
    (string-concat
      ;; the first check is: "are we in the same file"?
      ;; we check for `*compile-file-pathname*' too since
      ;; `match-known-unknown-functions' binds `*compile-file-truename*'
      ;; (to pass the right value to this function!)
      (format nil (if (and *compile-file-pathname*
                           (equalp file *compile-file-truename*))
                    #1=""
                    (format nil (TEXT " in file ~S") file)))
      (format nil (if (= lineno1 lineno2)
                    (TEXT " in line ~D")
                    (TEXT " in lines ~D..~D"))
              lineno1 lineno2))
    #1#))

(defun c-source-point-location (point)
  (c-source-location (c-source-point-lineno1 point)
                     (c-source-point-lineno2 point)
                     (c-source-point-file point)))

(defun current-function ()
  (and (boundp '*func*) (fnode-p *func*) (fnode-name *func*)))
;; check whether we are now defining FUN (maybe defining some internals)
(defun defining-p (fun)
  (member `(SYS::IN-DEFUN ,fun) *denv* :test #'equal))
;; check whether we are now inside the DEFUN FUN
(defun in-defun-p (fun)
  (and (equal fun (current-function)) (defining-p fun)))

(defvar *warning-count*)
;;; (C-WARN format-control-string . args)
;;; issue a compilation warning using FORMAT.
(defun c-warn (cstring &rest args)
  (incf *warning-count*)
  (apply #'c-comment
         (string-concat (TEXT "WARNING~@[ in ~A~]~A :") "~%" cstring)
         (current-function) (c-source-location)
         args))

(defvar *style-warning-count*)
; (C-STYLE-WARN controlstring . args)
; issue a style-warning (via FORMAT).
(defun c-style-warn (cstring &rest args)
  (incf *style-warning-count*)
  (apply #'c-warn cstring args))

(defvar *error-count*)
;; (C-ERROR controlstring . args)
;; issue a compiler error (via FORMAT) and terminate the current C-FORM.
(defun c-error (cstring &rest args)
  (incf *error-count*)
  (let ((in-function (current-function)))
    (when in-function
      (when *compiling-from-file*
        (pushnew in-function *functions-with-errors*)))
    (fresh-line *c-error-output*)
    (format *c-error-output* (TEXT "ERROR~@[ in ~S~]~A :")
            in-function (c-source-location))
    (terpri *c-error-output*)
    (apply #'format *c-error-output* cstring args)
    (elastic-newline *c-error-output*))
  (throw 'c-error
    (make-anode :source NIL
                :type 'ERROR
                :sub-anodes '()
                :seclass *seclass-pure*
                :code '((NIL)))))

;; the 2nd return value of `compile' and `compile-file'
(defun compile-warnings-p ()
  (let ((count (+ *error-count* *warning-count*)))
    (if (zerop count) nil count)))

;; the 3rd return value of `compile' and `compile-file'
(defun compile-failure-p ()
  (let ((count (+ *error-count* (- *warning-count* *style-warning-count*))))
    (if (zerop count) nil count)))

;; caught c-error
(defmacro c-error-c (&rest args) `(catch 'c-error (c-error ,@args)))

;; (c-write-lib form) write the form to the lib-file
(defun c-write-lib (form)
  (when (and *compiling-from-file* *liboutput-stream*)
    ;; write form to the liboutput stream:
    (unless (eq (car form) 'progn)
      (compiler-error 'c-write-lib form))
    (dolist (fo (cdr form))
      (unless (constantp fo)
        (write fo :stream *liboutput-stream* :pretty t
                ; :closure t :circle t :array t :gensym t
                ; :escape t :level nil :length nil :radix t
                  :readably t :right-margin 79)
        (terpri *liboutput-stream*)))))
(defun c-eval-and-write-lib (form) (c-write-lib form) (eval form))
(defmacro eval-when-compile (&body body)
  `(eval-when (compile) (c-eval-and-write-lib '(progn ,@body))))

;; Test whether FORM - already known to be a cons - is of the form
;; (QUOTE object)
(defun quote-p (form)
  (and (eq (first form) 'QUOTE) (consp (cdr form)) (null (cddr form))))

;; (l-constantp form) determines, if form may be handled in the compiler
;; as load-time-constant.
(defun l-constantp (form)
  (if (atom form)
    (or (numberp form) (characterp form) (arrayp form)
        (and (symbolp form)
             (cond ((keywordp form) t)
                   ((eq (symbol-package form) *lisp-package*)
                    (constantp form))
                   (t (not (null (assoc form *constant-special-vars*)))))))
    (quote-p form)))

;; (c-constantp form) determines, if form may be handled in the compiler
;; as compile-time-constant, and if the value is known and may be inserted
;; inline.
;; When *compiling-from-file* = nil , this is identical to (l-constantp form) .
(defun c-constantp (form)
  (if (atom form)
    (or (numberp form) (characterp form) (arrayp form)
        (and (symbolp form)
             (cond ((keywordp form) t)
                   ((and *compiling-from-file*
                         (declared-constant-notinline form))
                    nil)
                   ((eq (symbol-package form) *lisp-package*)
                    (constantp form))
                   (t (not (null (assoc form *constant-special-vars*)))))))
    (quote-p form)))

;; (c-constant-value form) returns the value of a constant.
;; (c-constantp form) is required.
(defun c-constant-value (form)
  (if (atom form)
    (cond ((numberp form) form)
          ((characterp form) form)
          ((arrayp form) form)
          ((symbolp form)
           (cond ((keywordp form) form)
                 ((eq (symbol-package form) *lisp-package*)
                  (symbol-value form))
                 (t (cdr (assoc form *constant-special-vars*))))))
    (second form)))

;; (anode-constantp anode) determines, if the Anode returns a constant
;; value (also known at compile-time).
(defun anode-constantp (anode)
  ;; Anode returns a constant value at all events, if its code consisted
  ;; (after TRAVERSE-ANODE) exactly of ((CONST ...)) .
  (let ((code (anode-code anode)))
    (and (consp code) (null (cdr code)) ; list of length 1
         (let ((item (car code)))
            (cond ((consp item)
                   (and (eq (first item) 'CONST)
                        (not (eq (const-horizon (second item)) ':form))))
                  ((anode-p item) (anode-constantp item)))))))

;; (anode-constant-value anode) returns the value of a constant Anode.
(defun anode-constant (anode)
  (let ((item (car (anode-code anode))))
    (cond ((consp item) (second item))
          (t #|(anode-p item)|# (anode-constant item)))))
(defun anode-constant-value (anode)
  (const-value-safe (anode-constant anode)))

;; (new-const value) returns a constant in *func* with the Value value
(defun new-const (value)
  (make-const :horizon ':value :value value))

;; (make-label for-value) returns a fresh label. for-value (NIL/ONE/ALL)
;; indicates, which of the values are needed after the label.
(defun make-label (for-value)
  (let ((label (gensym)))
    (setf (symbol-value label) '()) ; reference list for 2nd Pass := empty
    (setf (get label 'for-value) for-value)
    label))

;; returns a Special-Variable
(defun make-special-var (symbol)
  (make-var :name symbol :specialp t
            :constantp (l-constantp symbol)
            :constant (if (l-constantp symbol)
                        (if (c-constantp symbol)
                          (make-const :horizon ':all
                                      :value (c-constant-value symbol)
                                      :form symbol)
                          (make-const :horizon ':form
                                      :form symbol)))))


;;;;****                     FIRST   PASS

;; (test-list L) determines, if L is a real list, that ends with NIL
;; and has at least l1, but at most l2 elements. Else: Error.
(defun test-list (L &optional (l1 0) (l2 nil))
  (unless (and (listp L) (null (cdr (last L))))
    (c-error (TEXT "Code contains dotted list ~S") L))
  (unless (>= (length L) l1)
    (c-error (TEXT "Form too short, too few arguments: ~S") L))
  (when l2
    (unless (<= (length L) l2)
      (c-error (TEXT "Form too long, too many arguments: ~S") L))))

;; c-form-table contains the handler function (to be called without arguments)
;; for all functions/specialforms/macros, that have to be treated specially.
(defconstant c-form-table
  (let ((hashtable (make-hash-table :key-type 'symbol :value-type '(or symbol function)
                                    :test 'stablehash-eq :warn-if-needs-rehash-after-gc t)))
    (mapc
     #'(lambda (acons) (setf (gethash (car acons) hashtable) (cdr acons)))
     `(;; Special forms:
       (QUOTE . c-QUOTE)
       (PROGN . c-PROGN)
       (LET . ,#'(lambda () (c-LET/LET* nil)))
       (LET* . ,#'(lambda () (c-LET/LET* t)))
       (IF . c-IF)
       (SETQ . c-SETQ)
       (BLOCK . c-BLOCK)
       (RETURN-FROM . c-RETURN-FROM)
       (TAGBODY . c-TAGBODY)
       (GO . c-GO)
       (FUNCTION . c-FUNCTION)
       (MULTIPLE-VALUE-BIND . c-MULTIPLE-VALUE-BIND)
       (MULTIPLE-VALUE-SETQ . c-MULTIPLE-VALUE-SETQ)
       (AND . c-AND)
       (OR . c-OR)
       (WHEN . c-WHEN)
       (UNLESS . c-UNLESS)
       (COND . c-COND)
       (CASE . c-CASE)
       (FCASE . c-CASE)
       (PSETQ . c-PSETQ)
       (MULTIPLE-VALUE-CALL . c-MULTIPLE-VALUE-CALL)
       (PROG1 . c-PROG1)
       (PROG2 . c-PROG2)
       (THE . c-THE)
       (CATCH . c-CATCH)
       (THROW . c-THROW)
       (UNWIND-PROTECT . c-UNWIND-PROTECT)
       (PROGV . c-PROGV)
       (MULTIPLE-VALUE-LIST . c-MULTIPLE-VALUE-LIST)
       (MULTIPLE-VALUE-PROG1 . c-MULTIPLE-VALUE-PROG1)
       (FLET . c-FLET)
       (LABELS . c-LABELS)
       (MACROLET . c-MACROLET)
       (SYSTEM::FUNCTION-MACRO-LET . c-FUNCTION-MACRO-LET)
       (SYMBOL-MACROLET . c-SYMBOL-MACROLET)
       (COMPILER-LET . c-COMPILER-LET)
       (EVAL-WHEN . c-EVAL-WHEN)
       (DECLARE . c-DECLARE)
       (LOAD-TIME-VALUE . c-LOAD-TIME-VALUE)
       (LOCALLY . c-LOCALLY)
       ;; Macros:
       (%GENERIC-FUNCTION-LAMBDA . c-%GENERIC-FUNCTION-LAMBDA)
       (%OPTIMIZE-FUNCTION-LAMBDA . c-%OPTIMIZE-FUNCTION-LAMBDA)
       (CLOS:GENERIC-FLET . c-GENERIC-FLET)
       (CLOS:GENERIC-LABELS . c-GENERIC-LABELS)
       (SYS::%HANDLER-BIND . c-HANDLER-BIND)
       (SYS::CONSTANT-EQL . c-CONSTANT-EQL)
       (WITHOUT-PACKAGE-LOCK . c-WITHOUT-PACKAGE-LOCK)
       ;; Inline-compiled functions:
       (FUNCALL . c-FUNCALL)
       (APPLY . c-APPLY)
       (+ . c-PLUS)
       (* . c-STAR)
       (- . c-MINUS)
       (/ . c-SLASH)
       (= . c-COMPARE-NUMBERS)
       (/= . c-COMPARE-NUMBERS)
       (< . c-COMPARE-NUMBERS)
       (<= . c-COMPARE-NUMBERS)
       (> . c-COMPARE-NUMBERS)
       (>= . c-COMPARE-NUMBERS)
       (SYS::SVSTORE . c-SVSTORE)
       (EQ . c-EQ)
       (EQL . c-EQL)
       (EQUAL . c-EQUAL)
       (MAPCAR . c-MAPCAR)
       (MAPLIST . c-MAPLIST)
       (MAPC . c-MAPC)
       (MAPL . c-MAPL)
       (MAPCAN . c-MAPCAN)
       (MAPCON . c-MAPCON)
       (MAPCAP . c-MAPCAP)
       (MAPLAP . c-MAPLAP)
       (TYPEP . c-TYPEP)
       (FORMAT . c-FORMAT)
       (NTH . c-NTH)
       (SYSTEM::%SETNTH . c-SETNTH)
       ;; Special case for LIST
       (MAP . c-MAP)
       (MAP-INTO . c-MAP-INTO)
       (SOME . c-SOME)
       (EVERY . c-EVERY)
       (NOTANY . c-NOTANY)
       (NOTEVERY . c-NOTEVERY)
       ;; Special case for LIST, and COMPLEMENT tricks
       (REMOVE-IF . c-REMOVE-IF)
       (REMOVE-IF-NOT . c-REMOVE-IF-NOT)
       (DELETE-IF . c-DELETE-IF)
       (DELETE-IF-NOT . c-DELETE-IF-NOT)
       (SUBSTITUTE-IF . c-SUBSTITUTE-IF)
       (SUBSTITUTE-IF-NOT . c-SUBSTITUTE-IF-NOT)
       (NSUBSTITUTE-IF . c-NSUBSTITUTE-IF)
       (NSUBSTITUTE-IF-NOT . c-NSUBSTITUTE-IF-NOT)
       (FIND-IF . c-FIND-IF)
       (FIND-IF-NOT . c-FIND-IF-NOT)
       (POSITION-IF . c-POSITION-IF)
       (POSITION-IF-NOT . c-POSITION-IF-NOT)
       (COUNT-IF . c-COUNT-IF)
       (COUNT-IF-NOT . c-COUNT-IF-NOT)
       ;; COMPLEMENT tricks
       (SUBST-IF . c-SUBST-IF)
       (SUBST-IF-NOT . c-SUBST-IF-NOT)
       (NSUBST-IF . c-NSUBST-IF)
       (NSUBST-IF-NOT . c-NSUBST-IF-NOT)
       (MEMBER-IF . c-MEMBER-IF)
       (MEMBER-IF-NOT . c-MEMBER-IF-NOT)
       (ASSOC-IF . c-ASSOC-IF)
       (ASSOC-IF-NOT . c-ASSOC-IF-NOT)
       (RASSOC-IF . c-RASSOC-IF)
       (RASSOC-IF-NOT . c-RASSOC-IF-NOT)
       (ADJOIN . c-TEST/TEST-NOT)
       (NSET-DIFFERENCE . c-TEST/TEST-NOT)
       (SEARCH . c-TEST/TEST-NOT)
       (ASSOC . c-TEST/TEST-NOT)
       (NSET-EXCLUSIVE-OR . c-TEST/TEST-NOT)
       (SET-DIFFERENCE . c-TEST/TEST-NOT)
       (COUNT . c-TEST/TEST-NOT)
       (NSUBLIS . c-TEST/TEST-NOT)
       (SET-EXCLUSIVE-OR . c-TEST/TEST-NOT)
       (DELETE . c-TEST/TEST-NOT)
       (NSUBST . c-TEST/TEST-NOT)
       (SUBLIS . c-TEST/TEST-NOT)
       (DELETE-DUPLICATES . c-TEST/TEST-NOT)
       (NSUBSTITUTE . c-TEST/TEST-NOT)
       (SUBSETP . c-TEST/TEST-NOT)
       (FIND . c-TEST/TEST-NOT)
       (NUNION . c-TEST/TEST-NOT)
       (SUBST . c-TEST/TEST-NOT)
       (INTERSECTION . c-TEST/TEST-NOT)
       (POSITION . c-TEST/TEST-NOT)
       (SUBSTITUTE . c-TEST/TEST-NOT)
       (MEMBER . c-TEST/TEST-NOT)
       (RASSOC . c-TEST/TEST-NOT)
       (TREE-EQUAL . c-TEST/TEST-NOT)
       (MISMATCH . c-TEST/TEST-NOT)
       (REMOVE . c-TEST/TEST-NOT)
       (UNION . c-TEST/TEST-NOT)
       (NINTERSECTION . c-TEST/TEST-NOT)
       (REMOVE-DUPLICATES . c-TEST/TEST-NOT)
       ;;
       (LDB . c-LDB)
       (LDB-TEST . c-LDB-TEST)
       (MASK-FIELD . c-MASK-FIELD)
       (DPB . c-DPB)
       (DEPOSIT-FIELD . c-DEPOSIT-FIELD)))
    hashtable))
;; This table must contain all Special-Forms:
(do-all-symbols (sym)
  (when (and (special-operator-p sym) (not (gethash sym c-form-table)))
    (compiler-error 'c-form-table sym)))

;; expand (recursively) the compiler macro form
(defun expand-compiler-macro (form)
  (let ((expanded-p nil) fun)
    (tagbody
     reexpand
       (when (and (consp form) (function-name-p (setq fun (car form))))
         (let* ((env (env)) (cmf (compiler-macro-function fun env)))
           (when (and cmf (not (declared-notinline fun)))
             (let ((exp (mac-exp cmf form env)))
               (unless (eq form exp)
                 (setq form exp
                       expanded-p t)
                 (go reexpand)))))))
    (values form expanded-p)))

;; check whether the form is a '(LAMBDA ...)
(proclaim '(inline lambda-form-p))
(defun lambda-form-p (form)
  (and (consp form) (eq (car form) 'LAMBDA) (consp (cdr form))))

;; compiles a form.
;; No code is generated, if no values are needed and the form
;; does not produce side effects.
(defun c-form (*form* &optional (*for-value* *for-value*))
 (let
  ((anode
    (catch 'c-error
      (if (atom *form*)
        (if (symbolp *form*)
          (multiple-value-bind (macrop expansion)
              (venv-search-macro *form* *venv*)
            (if macrop ; Symbol-Macro ?
              (c-form expansion) ; -> expand
              (c-VAR *form*)))
          (c-CONST))
        (let ((fun (first *form*)))
          (if (function-name-p fun)
            (multiple-value-bind (a m f1 f2 f3 f4) (fenv-search fun)
              (declare (ignore f2 f4))
              (if (null a)
                ;; no local definition --> expand-compiler-macro
                (let ((handler
                        (gethash (setq *form* (expand-compiler-macro *form*)
                                       fun (and (consp *form*) (car *form*)))
                                 c-form-table)))
                  (if handler ; found handler function?
                    ;; ==> (symbolp fun) = T
                    (if (or (and (special-operator-p fun)
                                 (not (macro-function fun)))
                            (not (declared-notinline fun)))
                      (funcall handler) ; yes -> call
                      (if (macro-function fun)
                        (c-form (mac-exp (macro-function fun) *form*))
                        ;; normal global function call
                        (c-GLOBAL-FUNCTION-CALL fun)))
                    ;; no -> not a special-form anyway
                    ;; (all those are in the `c-form-table')
                    (if (atom *form*)
                      (c-form *form*)
                      (if (and (symbolp (setq fun (first *form*)))
                               (macro-function fun))
                        ;; global macro
                        (c-form (mac-exp (macro-function fun) *form*))
                        ;; global function
                        (if (and (in-defun-p fun)
                                 (not (declared-notinline fun)))
                          ;; recursive call of the current global function
                          (c-LOCAL-FUNCTION-CALL fun (cons *func* nil)
                                                 (cdr *form*))
                          ;; normal call of the global function
                          (c-GLOBAL-FUNCTION-CALL fun))))))
                (if (and m (not (and f1 (declared-notinline fun))))
                  (c-form (mac-exp m *form*))
                  (case f1
                    (GLOBAL ; found in the interpreter environment %fenv%
                     ; (c-form `(FUNCALL (FUNCTION ,fun) ,@(cdr *form*)))
                     (c-FUNCALL-NOTINLINE `(FUNCTION ,fun) (cdr *form*)))
                    (LOCAL  ; local function (found in *fenv*)
                     ; (c-form `(FUNCALL (FUNCTION ,fun) ,@(cdr *form*)))
                     (c-LOCAL-FUNCTION-CALL fun f3 (cdr *form*)))
                    (t (compiler-error 'c-form f1))))))
            (if (lambda-form-p fun)
              (c-form `(FUNCALL (FUNCTION ,fun) ,@(cdr *form*)))
              #| not: (c-LAMBDA-FUNCTION-CALL fun (cdr *form*)) |#
              (c-error (TEXT "Not the name of a function: ~S") fun))))))))
  #+CLISP-DEBUG (setf (anode-source anode) *form*)
  ;; If no values are needed and no side effects are produced,
  ;; the appendant code can be discarded completely:
  (when (and (null *for-value*) (anode-side-effect-free-p anode))
    (setf (anode-code anode) '())
    (setf (anode-seclass anode) *seclass-pure*))
  anode))

;;; Macroexpand a form.
;;; That is exactly what c-form does later anyway.
;;; (c-form (macroexpand-form form)) == (c-form form).
(defun macroexpand-form (form)
  ;; The difference from  (values (macroexpand form (env)))  is that here
  ;; 1. compiler macros expanders are preferred over normal macro expanders,
  ;; 2. the macros mentioned in 'c-form-table' are not expanded.
  (tagbody
   reexpand
    (setq form (expand-compiler-macro form))
    (if (atom form)
      (if (symbolp form)
        (multiple-value-bind (macrop expansion) (venv-search-macro form *venv*)
          (if macrop
            (progn (setq form expansion) (go reexpand))
            (go done)))
        (go done))
      (let ((fun (first form)))
        (if (function-name-p fun)
          (multiple-value-bind (a m f1 f2 f3 f4) (fenv-search fun)
            (declare (ignore f2 f3 f4))
            (if a               ; locally defined
              (when f1          ; local function?
                (setq m nil))
              ;; no local definition
              (when (and (symbolp fun) (macro-function fun) ; global macro?
                         (not (gethash fun c-form-table)))
                (setq m (macro-function fun))))
            (if m               ; macro?
              (progn
                (setq form (mac-exp m form)) ; -> expand
                (go reexpand))
              (go done)))
          (go done))))
    done
    (return-from macroexpand-form form)))

;; compile NIL (a kind of emergency exit)
(defun c-NIL ()
  (make-anode :type 'NIL
              :sub-anodes '()
              :seclass *seclass-foldable*
              :code '((NIL))))

;; constant as form:
(defun c-CONST ()
  (make-anode :type 'const
              :sub-anodes '()
              :seclass (if (c-constantp *form*) *seclass-foldable* *seclass-pure*)
              :code `((CONST ,(new-const *form*)))))

;; variable as Form:
(defun c-VAR (symbol)
  ;; search the variable in *venv* :
  (multiple-value-bind (a b c) (venv-search symbol)
    (when (eq a 'NIL)
      (c-warn (TEXT "~S is neither declared nor bound,~@
                     it will be treated as if it were declared SPECIAL.")
              symbol)
      (when *compiling-from-file*
        (pushnew symbol *unknown-free-vars* :test #'eq))
      (setq a 'SPECIAL))
    (case a
      (SPECIAL ; special variable
        (let ((var (make-special-var symbol)))
          (make-anode
            :type 'VAR
            :sub-anodes '()
            :seclass (make-seclass
                      :uses (if (and *for-value* (not (var-constantp var)))
                              (list symbol)
                              'NIL))
            :code (if *for-value*
                    (if (var-constantp var)
                      `((CONST ,(make-const
                                 ;; Keywords are not needed in #. syntax
                                 :horizon (if (keywordp symbol) ':value ':all)
                                 :value (c-constant-value symbol)
                                 :form symbol)))
                      `((GETVALUE ,symbol)))
                    '()))))
      (LOCAL ; interpreted, lexical
        (make-anode
          :type 'VAR
          :sub-anodes '()
          :seclass (make-seclass :uses (if *for-value* 'T 'NIL))
          :code (if *for-value*
                  `((CONST ,(new-const b)) ; Vector
                    (PUSH)
                    (CONST ,(new-const c)) ; Index
                    (SVREF NIL))
                  '())))
      ((T) ; lexically in Stack or Closure
        (let* ((var b)
               (get-anode
                 (make-anode
                   :type 'VAR
                   :sub-anodes '()
                   :seclass (make-seclass
                             :uses (if *for-value* (list var) 'NIL))
                   :code (if *for-value*
                           `((GET ,var ,*venvc* ,*stackz*))
                           '()))))
          (push get-anode (var-usedp var))
          (when *for-value*
            (setf (var-for-value-usedp var) t)
            (unless *no-code*
              (unless (eq (stackz-fun (var-stackz var)) (var-fnode var))
                (compiler-error 'c-VAR "VAR-FNODE"))
              (if (eq (var-fnode var) *func*)
                (setf (var-really-usedp var) t)
                (note-far-used-var var))))
          get-anode))
      (t (compiler-error 'c-VAR 'venv-search)))))

;; variable assignment:
(defun c-VARSET (symbol value-anode for-value)
  ;; search the variable in *venv* :
  (multiple-value-bind (a b c) (venv-search symbol)
    (when (eq a 'NIL)
      (c-warn (TEXT "~S is neither declared nor bound,~@
                     it will be treated as if it were declared SPECIAL.")
              symbol)
      (setq a 'SPECIAL))
    (case a
      (SPECIAL ; special variable
        (let ((var (make-special-var symbol)))
          (make-anode :type 'VARSET
                      :sub-anodes '()
                      :seclass
                        (if (var-constantp var)
                          *seclass-pure*
                          (make-seclass :uses-binding (list symbol)
                                        :modifies (list symbol)))
                      :code (if (var-constantp var)
                              (progn
                                (c-warn
                                 (TEXT "The constant ~S may not be assigned to.~@
                                        The assignment will be ignored.")
                                        symbol)
                                '((VALUES1)))
                              `((SETVALUE , symbol))))))
      (LOCAL ; interpreted, lexical
        (make-anode :type 'VARSET
                    :sub-anodes '()
                    :seclass (make-seclass :modifies 'T)
                    :code `((PUSH)
                            (CONST ,(new-const b)) ; Vector
                            (PUSH)
                            (CONST ,(new-const c)) ; Index
                            (SVSET))))
      ((T) ; lexically in Stack or Closure
        (let* ((var b)
               (set-anode
                 (make-anode :type 'VARSET
                             :sub-anodes '()
                             :seclass (make-seclass :modifies (list var))
                             :code `((SET ,var ,*venvc* ,*stackz*)))))
          ;; assignment "uses" the Variable
          (unless (var-usedp var) (setf (var-usedp var) t))
          (setf (var-assignedp var) t)
          (unless *no-code*
            (setf (var-constantp var) nil) ; not constant anymore due to assignment
            (push (list* value-anode set-anode for-value)
                  (var-modified-list var))
            (unless (eq (stackz-fun (var-stackz var)) (var-fnode var))
              (compiler-error 'c-VARSET "VAR-FNODE"))
            (unless (eq (var-fnode var) *func*)
              (note-far-assigned-var var))
            ;; the replacement of a variable innervar by var is not
            ;; allowed, if during the existence of innervar
            ;; a value is assigned to var.
            (setf (var-replaceable-list var)
              (delete-if #'(lambda (innervar-info) ; innervar currently active?
                             (let ((innervar (first innervar-info)))
                               (tailp (var-stackz innervar) *stackz*)))
                         (var-replaceable-list var))))
          set-anode))
      (t (compiler-error 'c-VARSET 'venv-search)))))

;; function calls, for which the function is a Symbol or (SETF symbol) :

(defun make-funname-const (name)
  (if (atom name)
    (new-const name)
    (let ((symbol (second name)))
      (make-const :horizon ':all
                  :value (system::get-setf-symbol symbol)
                  :form `(SYSTEM::GET-SETF-SYMBOL ',symbol)))))

(defun proclaimed-inline-p (fun)
  (eq (get (get-funname-symbol fun) 'inlinable) 'inline))

;; we need to check *known-functions* to make sure that the side-effect
;; class is computed correctly
(defun f-side-effect (fun)
  ;; we start with FUNCTION-SIDE-EFFECT to ensure that FUN is indeed a function
  (multiple-value-bind (seclass fdef name) (function-side-effect fun)
    ;; If SAFETY = 3, ANSI CL 3.5 requires us to signal errors about
    ;; invalid arguments, and the simplest way to implement this
    ;; requirement is to not omit the call. We don't keep track of the
    ;; possibility of errors through the SECLASS currently.
    ;; Therefore pretend the function has arbitrary side-effects.
    (if (>= (declared-optimize 'SAFETY) 3)
      *seclass-dirty*
      ;; For NOTINLINE functions, side effects are unpredictable!
      (if (declared-notinline name)
        *seclass-dirty*
        (let ((kf (assoc name *known-functions* :test #'equal)))
          (if kf
            (fourth kf) ; defined in this compilation unit, use the seclass!
            (if (equal *seclass-dirty* seclass)
              *seclass-dirty*
              ;; seclass is not dirty ==> function is defined
              (if (or (proclaimed-inline-p name)     ; inlined
                      (subr-info fdef) ; SUBR
                      (null name)      ; anonymous
                      (let ((pack (symbol-package (get-funname-symbol name))))
                        (or (null pack) (package-lock pack))))
                seclass
                *seclass-dirty*))))))))

;; global function call, normal (notinline): (fun {form}*)
(defun c-NORMAL-FUNCTION-CALL (fun) ; fun is a symbol or (SETF symbol)
  (test-list *form* 1)
  (let* ((n (length (cdr *form*)))
         #+CLISP-DEBUG (oldstackz *stackz*)
         (*stackz* *stackz*))
    (do ((formlist (cdr *form*))
         (seclass (f-side-effect fun))
         #+CLISP-DEBUG (anodelist '())
         (codelist (list '(CALLP))))
        ((null formlist)
         (push
           `(,@(case n
                 (0 `(CALL0)) (1 `(CALL1)) (2 `(CALL2)) (t `(CALL ,n)))
             ,(make-funname-const fun))
           codelist)
         (make-anode
           :type 'CALL
           :sub-anodes (nreverse anodelist)
           :seclass seclass
           :code (nreverse codelist)
           :stackz oldstackz))
      (let* ((formi (pop formlist))
             (anodei (c-form formi 'ONE)))
        #+CLISP-DEBUG (push anodei anodelist)
        (seclass-or-f seclass anodei)
        (push anodei codelist)
        (push '(PUSH) codelist)
        (push 1 *stackz*)))))

;; Compute the signature of a function object:
;; 1. name
;; 2. req-num
;; 3. opt-num
;; 4. rest-p
;; 5. key-p
;; 6. keyword-list
;; 7. allow-other-keys-p
(defun function-signature (obj &optional no-error)
  ;; what if the function has been redefined in this file already?
  ;; if it has been redefined above, we must use the signature
  ;; from the above definition!
  (let ((kf (assoc obj *known-functions* :test #'equal)) known-sig)
    (when kf        ; use the signature from the above definition!
      (setq known-sig (third kf))
      (return-from function-signature
        (values (first kf)
                (sig-req-num  known-sig)
                (sig-opt-num  known-sig)
                (sig-rest-p   known-sig)
                (sig-keys-p   known-sig)
                (sig-keywords known-sig)
                (sig-allow-p  known-sig)))))
  (when (and (function-name-p obj) (fboundp obj))
    (setq obj (fdefinition obj)))
  (if (closurep obj)
    (if (sys::%compiled-function-p obj)
        ;; compiled closure
        (multiple-value-bind (req-num opt-num rest-p key-p keywords allow-p)
            (signature obj)
          (values (sys::closure-name obj)
                  req-num opt-num rest-p key-p keywords allow-p))
        ;; interpreted closure
        (let ((clos_keywords (sys::%record-ref obj 16)))
          (values (sys::closure-name obj)
                  (sys::%record-ref obj 12) ; req_num
                  (sys::%record-ref obj 13) ; opt_num
                  (sys::%record-ref obj 19) ; rest_flag
                  (not (numberp clos_keywords))
                  (if (not (numberp clos_keywords)) (copy-list clos_keywords))
                  (sys::%record-ref obj 18)))) ; allow_flag
    (cond #+FFI
          ((eq (type-of obj) 'FFI::FOREIGN-FUNCTION)
           (values (function-name obj)
                   (foreign-function-in-arg-count obj) 0 nil nil nil nil))
          (t
           (multiple-value-bind (name req-num opt-num rest-p keywords allow-p)
               (subr-info obj)
             (if name
               (values name req-num opt-num rest-p keywords keywords allow-p)
               (if no-error
                 (values)
                 (coerce obj 'function)))))))) ; error

(defun get-signature (obj)
  (multiple-value-bind (name req-num opt-num rest-p key-p keywords allow-p)
      (function-signature obj)
    (declare (ignore name))
    (make-signature :req-num req-num :opt-num opt-num :rest-p rest-p
                    :keys-p key-p :keywords keywords :allow-p allow-p)))

(defun signature-to-list (req-num opt-num rest-p key-p keywords allow-p)
  (let ((args '()) (count -1))
    (dotimes (i req-num)
      #1=(push (make-symbol (format nil "ARG~D" (incf count))) args))
    (when (plusp opt-num)
      (push '&OPTIONAL args)
      (dotimes (i opt-num) #1#))
    (when rest-p
      (push '&REST args)
      (push 'other-args args))
    (when key-p
      (push '&KEY args)
      (dolist (kw keywords) (push kw args))
      (when allow-p (push '&ALLOW-OTHER-KEYS args)))
    (nreverse args)))

(defun sig-to-list (sig)
  (signature-to-list (sig-req-num sig) (sig-opt-num sig) (sig-rest-p sig)
                     (sig-keys-p sig) (sig-keywords sig) (sig-allow-p sig)))

;; returns the signature of a function from fdescr
(defun fdescr-signature (fdescr)
  (if (cdr fdescr)
    (if (eq (cadr fdescr) 'LABELS)
      ;; defined with LABELS: from the lambda list information
      (multiple-value-bind (reqvar optvar optinit optsvar restvar
                            keyflag keyword keyvar keyinit keysvar
                            allow-other-keys auxvar auxinit)
          (values-list (cddr fdescr))
        (declare (ignore optinit optsvar keyvar keyinit
                         keysvar auxvar auxinit))
        (values (length reqvar) (length optvar)
                (not (eql restvar 0)) keyflag
                keyword allow-other-keys))
      ;; defined with GENERIC-FLET or GENERIC-LABELS: from the signature
      (values-list (cddr fdescr)))
    ;; defined with FLET or IN-DEFUN: from the fnode
    (let ((fnode (car fdescr)))
      (values (fnode-req-anz fnode) (fnode-opt-anz fnode)
              (fnode-rest-flag fnode) (fnode-keyword-flag fnode)
              (fnode-keywords fnode) (fnode-allow-other-keys-flag fnode)))))

;; (test-argument-syntax args applyargs fun
;;                       req opt rest-p key-p keylist allow-p)
;; check whether the arglist ARGS (and maybe additional arguments APPLYARGS)
;; is a valid arglist for the function FUN with the signature
;; (REQ OPT REST-P KEY-P KEYLIST ALLOW-P)
;; The appropriate warnings are printed as necessary
;; Returns:
;;   NO-KEYS           correct syntax, no keywords,
;;   STATIC-KEYS       correct syntax, constant keywords,
;;   DYNAMIC-KEYS      (probably) correct syntax, non-constant keywords
;;   NIL               incorrect syntax,
;; In the first two cases:
;; if (not applyargs):
;;   req <= (length args) <= (req+opt or, if rest-p or key-p, infinity)
;; or if applyargs:
;;   (length args) <= (req+opt or, if rest-p or key-p, infinity).
(defun test-argument-syntax (args applyargs fun req opt rest-p key-p keylist
                             allow-p)
  (unless (and (listp args) (null (cdr (last args))))
    (c-error (TEXT "argument list to function ~S is dotted: ~S")
             fun args))
  (let ((n (length args))
        (reqopt (+ req opt)))
    (unless (and (or applyargs (<= req n)) (or rest-p key-p (<= n reqopt)))
      (c-warn (TEXT "~S was called with ~S~:[~; or more~] arguments, but it requires ~:[~:[from ~S to ~S~;~S~]~;at least ~*~S~] argument~:p.")
              fun n applyargs
              (or rest-p key-p) (eql req reqopt) req reqopt)
      (return-from test-argument-syntax 'NIL))
    (unless key-p (return-from test-argument-syntax 'NO-KEYS))
    ;; has keywords
    (when (<= n reqopt) (return-from test-argument-syntax 'STATIC-KEYS))
    (when rest-p (return-from test-argument-syntax 'DYNAMIC-KEYS))
    (setq n (- n reqopt) args (nthcdr reqopt args))
    (unless (or (evenp n) applyargs)
      (c-warn (TEXT "keyword arguments to function ~S should occur pairwise: ~S")
              fun args)
      (return-from test-argument-syntax 'NIL))
    (do ((keyargs args (cddr keyargs))
         (allow-other-keys-seen nil)
         (allow-flag allow-p)
         (wrong-key-p nil)
         wrong-key)
        ((null keyargs)
         (cond (wrong-key-p
                (c-warn (TEXT "keyword ~S is not allowed for function ~S.~
                             ~%The only allowed keyword~[s are~; is~:;s are~] ~{~S~#[~; and ~S~:;, ~]~}.")
                        wrong-key fun (length keylist) keylist)
                NIL)
               (t 'STATIC-KEYS)))
      (let ((key (first keyargs)))
        (unless (c-constantp key)
          (return-from test-argument-syntax 'DYNAMIC-KEYS))
        (setq key (c-constant-value key))
        (unless (symbolp key)
          (c-warn (TEXT "argument ~S to function ~S is not a symbol")
                  (first keyargs) fun)
          (return-from test-argument-syntax 'DYNAMIC-KEYS))
        (when (and (eq key ':ALLOW-OTHER-KEYS) (not allow-other-keys-seen))
          (setq allow-other-keys-seen t)
          (unless (c-constantp (second keyargs))
            (return-from test-argument-syntax 'DYNAMIC-KEYS))
          (when (c-constant-value (second keyargs)) (setq allow-flag t)))
        (unless (or allow-flag (memq key keylist) wrong-key-p)
          (setq wrong-key-p t)
          (setq wrong-key key))))))

;; try to evaluate FORM for side effects
;; returns NIL on error and T on success
(defmacro try-eval (&body forms)
  `(block try-eval
     (let ((*error-handler*
             #'(lambda (&rest error-args)
                 ;; NB: the warning may be reported twice: if you compile
                 ;; (* 1d30 1d30), this will be called first from c-STAR
                 ;; (because this is a * form), and second from
                 ;; c-DIRECT-FUNCTION-CALL (because this is a constant form)
                 (apply #'c-warn (TEXT "Run time error expected: ~@?")
                        (cdr error-args))
                 (return-from try-eval nil))))
       ,@forms)
     t))

;; (c-DIRECT-FUNCTION-CALL args applyargs fun req opt rest-p key-p keylist
;;                         subr-flag call-code-producer)
;; compiles the processing of the arguments for the direct call of a
;; function (i.e. without argument-check at run-time).
;; (test-argument-syntax ...) must have checked the arguments already
;;             successfully (i.e. with result NO-KEYS or STATIC-KEYS) .
;; args      : list of argument-forms,
;; applyargs : if specified, list of a form for additional arguments,
;; fun       : name of the function to be called (Symbol) ,
;; req,opt,rest-p,key-p,keylist,allow-p
;;           : information about the lambda-list of fun
;; subr-flag : Flag, if fun is a SUBR or a compiled Closure,
;;             (attention: use applyargs only for compiled closures!),
;; call-code-producer : function, that returns the code, which is appended
;;                      at the end and which executes the call.
(defun c-DIRECT-FUNCTION-CALL (args applyargs fun req opt rest-p key-p keylist
                               subr-flag call-code-producer)
  (let ((seclass (f-side-effect fun)))
    (if (and (null *for-value*) (null (seclass-modifies seclass)))
      ;; Do not need to call the function, just evaluate the arguments.
      (progn
        (let ((*no-code* t) (*for-value* 'NIL))
          (funcall call-code-producer))
        (c-form `(PROGN ,@args ,@applyargs)))
      (let ((n (length args))
            (reqopt (+ req opt))
            (codelist '()))
        (let ((*stackz* *stackz*))
          ;; required and given optional parameters:
          (dotimes (i (min n reqopt))
            (let* ((formi (pop args))
                   (anodei (c-form formi 'ONE)))
              (seclass-or-f seclass anodei)
              (push anodei codelist))
            (push '(PUSH) codelist)
            (push 1 *stackz*))
          (if applyargs
            (progn
              (when subr-flag
                (compiler-error 'c-DIRECT-FUNCTION-CALL "APPLY-SUBR"))
              (when key-p (compiler-error 'c-DIRECT-FUNCTION-CALL "APPLY-KEY"))
              (if (>= reqopt n)
                ;; missing optional parameters are initialized from the list:
                (let* ((anz (- reqopt n))
                       (anode1 (c-form (first applyargs) 'ONE))
                       (anode2 (progn
                                 (push (if rest-p (+ anz 1) anz) *stackz*)
                                 (c-unlist rest-p anz (min opt anz)))))
                  (seclass-or-f seclass anode1)
                  (push anode1 codelist)
                  (seclass-or-f seclass anode2)
                  (push anode2 codelist))
                ;; n > reqopt, implies rest-p.
                ;; Passing of the remaining arguments to a compiled Closure:
                ;; as list.
                ;; List consisting of all additional arguments:
                (progn
                  (let ((*stackz* *stackz*)
                        (rest-args args))
                    (loop
                      (when (null rest-args) (return))
                      (let ((anode (c-form (pop rest-args) 'ONE)))
                        (seclass-or-f seclass anode)
                        (push anode codelist))
                      (push '(PUSH) codelist)
                      (push 1 *stackz*))
                    (let ((anode (c-form (first applyargs) 'ONE)))
                      (seclass-or-f seclass anode)
                      (push anode codelist))
                    (push `(LIST* ,(- n reqopt)) codelist))
                  (push '(PUSH) codelist)
                  (push 1 *stackz*))))
            (progn
              ;; missing optional parameters are initialized with #<UNBOUND> :
              (when (> reqopt n)
                (let ((anz (- reqopt n)))
                  (push `(PUSH-UNBOUND ,anz) codelist)
                  (push anz *stackz*)))
              ;; &rest parameter:
              (when rest-p
                (if subr-flag
                  ;; Passing of remaining arguments to a SUBR: one by one
                  (loop
                    (when (null args) (return))
                    (let ((anode (c-form (pop args) 'ONE)))
                      (seclass-or-f seclass anode)
                      (push anode codelist))
                    (push '(PUSH) codelist)
                    (push 1 *stackz*))
                  ;; passing of remaining arguments to a compiled closure:
                  ;; as list
                  (if (null args)
                    ;; empty list
                    (progn
                      (push '(NIL) codelist)
                      (push '(PUSH) codelist)
                      (push 1 *stackz*))
                    ;; list of all further arguments:
                    (progn
                      (let ((*stackz* *stackz*)
                            (rest-args args))
                        (loop
                          (when (null rest-args) (return))
                          (let ((anode (c-form (pop rest-args) 'ONE)))
                            (seclass-or-f seclass anode)
                            (push anode codelist))
                          (push '(PUSH) codelist)
                          (push 1 *stackz*))
                        (push `(LIST ,(- n reqopt)) codelist))
                      (push '(PUSH) codelist)
                      (push 1 *stackz*)))))))
          ;; &key parameter:
          (when key-p
            ;; only if rest-p and key-p at the same time, if n <= reqopt,
            ;; because `test-argument-syntax' (which yielded STATIC-KEYS)
            ;; has excluded the other case already.
            (let ((keyanz (length keylist)))
              ;; First initialize all keys with #<UNBOUND> ,
              ;; then evaluate and assign the arguments in the specified order?
              ;; this is too simple for us. We transpose the arguments, so that
              ;; as many of the (STORE ...) as possible are replaced by (PUSH):
              ;; The arguments for the first Keys are evaluated at first,
              ;; if possible, the last Keys are evaluated at last.
              ;; However, we use only one single (PUSH-UNBOUND ...).
              (let* ((key-positions
                       ;; list of triples (key stack-depth free-p), where
                       ;; stack-depth = keyanz-1...0 and free-p states,
                       ;; if the Slot is already filled.
                       (let ((i keyanz))
                         (mapcar #'(lambda (key) (list key (decf i) t))
                                 keylist)))
                     (anodes
                       ;; list of quadruples (needed key-position anode stackz),
                       ;; where key-position is the stack-depth of the
                       ;; Keyword-Slots or NIL, anode is the Anode for
                       ;; this argument.  The list is kept in the same
                       ;; order, as is specified by the argument-list.
                       ;; exception: needed = NIL for anodes, whose
                       ;; calculation was brought forward or suspended.
                       (let ((L '()))
                         (loop
                           (when (null args) (return))
                           (let* ((key (c-constant-value (pop args)))
                                  (tripel (assoc key key-positions :test #'eq)) ; can be =NIL!
                                  (for-value (third tripel))
                                  (arg (pop args)))
                             ;; for-value /= NIL: Existing Keyword,
                             ;; and the Slot is still empty
                             ;; for-value = NIL: ALLOW-allowed Keyword
                             ;; or Slot already filled
                             (unless for-value
                               (if tripel
                                 (c-warn
                                   (TEXT "~S: ignored duplicate keyword ~S ~S")
                                   fun key arg)
                                 (unless (eq key ':ALLOW-OTHER-KEYS)
                                   (c-warn (TEXT "~S: ignored keyword ~S ~S")
                                           fun key arg))))
                             (let* ((*stackz* (cons 0 *stackz*)) ; 0 will be replaced later
                                    (anode (c-form arg (if for-value 'ONE 'NIL))))
                               (seclass-or-f seclass anode)
                               (push (list t
                                           (and for-value (second tripel))
                                           anode
                                           *stackz*)
                                     L))
                             (when tripel
                               (setf (third tripel) nil))))
                         (nreverse L))))
                (let ((depth1 0)
                      (depth2 0)
                      (codelist-from-end '()))
                  (flet ((commute-with-rest-p (anodeetc anodes anodesr)
                           (let ((anode (third anodeetc)))
                             (do ((anodesr2 anodes (cdr anodesr2)))
                                 ((eq anodesr2 anodesr) t)
                               (unless (anodes-commute
                                        anode (third (car anodesr2)))
                                 (return nil))))))
                    ;; bring forward as much as possible:
                    (do ((anodesr anodes (cdr anodesr)))
                        ((null anodesr))
                      (let ((anodeetc (car anodesr))) ; next Quadruple
                        (when (first anodeetc) ; something else to do?
                          (if (and
                                (or ; no Keyword, i.e. no (STORE ...) necessary?
                                  (null (second anodeetc))
                                  ;; topmost Keyword?
                                  (= (second anodeetc) (- keyanz depth1 1)))
                                ;; does anodeetc commute with previous anodes?
                                (commute-with-rest-p anodeetc anodes anodesr))
                            ;; bring forward:
                            (progn
                              (setf (first (fourth anodeetc))
                                    depth1) ; correct stack depth
                              (push (third anodeetc)
                                    codelist) ; into the code-list
                              (when (second anodeetc)
                                (push '(PUSH) codelist)
                                (incf depth1))
                              ;; we don't need this one anymore:
                              (setf (first anodeetc) nil))
                            ;; else we do nothing.
                            ))))
                    ;; bring backwards as much as possible:
                    (setq anodes (nreverse anodes))
                    (do ((anodesr anodes (cdr anodesr)))
                        ((null anodesr))
                      (let ((anodeetc (car anodesr))) ; next Quadruple
                        (when (first anodeetc) ; still sth. to do?
                          (if (and
                                (or ; no Keyword, i.e. no (STORE ...) necessary?
                                  (null (second anodeetc))
                                  ;; lowest Keyword?
                                  (= (second anodeetc) depth2))
                                ;; does anodeetc commute with all later anodes?
                                (commute-with-rest-p anodeetc anodes anodesr))
                            ;; push to the End:
                            (progn
                              (when (second anodeetc)
                                (push '(PUSH) codelist-from-end)
                                (incf depth2))
                              (setf (first (fourth anodeetc))
                                    (- keyanz depth2)) ; correct stack-depth
                              (push (third anodeetc)
                                    codelist-from-end) ; into the code-list
                              ;; we don't need this one anymore
                              (setf (first anodeetc) nil))
                            ;; else we do nothing.
                            ))))
                    (setq anodes (nreverse anodes))
                    (let ((depth-now (- keyanz depth2)))
                      ;; codelist-from-end decreases the Stack by depth2
                      (when (> depth-now depth1)
                        (push `(PUSH-UNBOUND ,(- depth-now depth1)) codelist))
                      ;; In code-list stack-depth is now depth-now.
                      (dolist (anodeetc anodes)
                        (when (first anodeetc)
                          ;; correct stack-depth
                          (setf (first (fourth anodeetc)) depth-now)
                          (push (third anodeetc) codelist)
                          (when (second anodeetc)
                            (push `(STORE ,(- (second anodeetc) depth2))
                                  codelist)))))
                    ;; now codelist-from-end:
                    (setq codelist (nreconc codelist-from-end codelist)))))
              ;; now all key argument are on the Stack.
              (push keyanz *stackz*)))
          (setq codelist (nreconc codelist (funcall call-code-producer))))
        ;; Constant-Folding: if fun is foldable (i.e.: subr-flag = T and
        ;; key-flag = NIL) and if codelist consists besides the (PUSH)s and the
        ;; Call-Code at the end only of Anodes with code = ((CONST ...)) ?
        (when (and (seclass-foldable-p seclass)
                   (every #'(lambda (code)
                              (or (not (anode-p code)) (anode-constantp code)))
                          codelist))
          ;; try to call function:
          (let ((args (let ((L '())) ; list of (constant) arguments
                        (dolist (code codelist)
                          (when (anode-p code)
                            (push (anode-constant-value code) L)))
                        (nreverse L)))
                resulting-values)
            (when (try-eval
                   (setq resulting-values
                         (multiple-value-list (apply fun args))))
              ;; function called successfully, perform constant folding:
              (return-from c-DIRECT-FUNCTION-CALL
                (c-GLOBAL-FUNCTION-CALL-form
                  `(VALUES ,@(mapcar #'(lambda (x) `(QUOTE ,x))
                                     resulting-values)))))))
        (make-anode
          :type `(DIRECT-CALL ,fun)
          :sub-anodes (remove-if-not #'anode-p codelist)
          :seclass seclass
          :code codelist)))))
(defun c-unlist (rest-p n m)
  (if rest-p
    (if (eql n 0)
      (make-anode :type 'UNLIST*
                  :sub-anodes '()
                  :seclass *seclass-pure*
                  :code '((PUSH)))
      (make-anode :type 'UNLIST*
                  :sub-anodes '()
                  :seclass *seclass-dirty* ; can report Error
                  :code `((UNLIST* ,n ,m))))
    (make-anode :type 'UNLIST
                :sub-anodes '()
                :seclass *seclass-dirty* ; can report Error
                :code `((UNLIST ,n ,m)))))
(defun cclosure-call-code-producer (fun fnode req opt rest-flag key-flag
                                    keylist)
  (if (eq fnode *func*) ; recursive call of the own function
    (let ((call-code
            `((JSR ,(+ req opt (if rest-flag 1 0) (length keylist)) ; number of stack-entries
                   ,*func-start-label*))))
      #'(lambda () call-code))
    ;; call another Cclosure
    #'(lambda ()
        (list
          (c-form `(FUNCTION ,fun) 'ONE)
          (if key-flag '(CALLCKEY) '(CALLC))))))

;; global function call: (fun {form}*)
(defun c-GLOBAL-FUNCTION-CALL-form (*form*)
  (c-GLOBAL-FUNCTION-CALL (first *form*)))
(defun c-GLOBAL-FUNCTION-CALL (fun) ; fun is a Symbol or (SETF symbol)
  (test-list *form* 1)
  (note-function-used fun (cdr *form*) nil)
  (when *compiling-from-file* ; called by COMPILE-FILE?
    ;; take note of PROCLAIM-declarations:
    (when (and (eq fun 'PROCLAIM) (= (length *form*) 2))
      (let ((h (second *form*)))
        (when (c-constantp h)
          (c-form
           `(eval-when-compile (c-PROCLAIM ',(c-constant-value h)))))))
    ;; take note of module requirements:
    (when (and (memq fun '(PROVIDE REQUIRE))
               (every #'c-constantp (rest *form*)))
      (c-form
        `(eval-when-compile
           (,(case fun
               (PROVIDE 'c-PROVIDE)  ; c-PROVIDE instead of PROVIDE
               (REQUIRE 'c-REQUIRE)) ; c-REQUIRE instead of REQUIRE
            ,@(mapcar           ; quote arguments
               #'(lambda (x) (list 'QUOTE (c-constant-value x)))
               (rest *form*))))))
    ;; take note of package-requirements:
    (when (and *package-tasks-treat-specially*
               (memq fun '(MAKE-PACKAGE SYSTEM::%IN-PACKAGE
                           SHADOW SHADOWING-IMPORT EXPORT UNEXPORT
                           USE-PACKAGE UNUSE-PACKAGE IMPORT))
               (every #'c-constantp (rest *form*)))
      (push
        `(,fun
          ,@(mapcar
              ;; Quote the arguments, but only when necessary, because
              ;; ANSI-CL IN-PACKAGE wants unquoted arguments.
              #'(lambda (x)
                  (let ((v (c-constant-value x)))
                    (if (or (numberp v) (characterp v) (arrayp v) (keywordp v))
                      v
                      (list 'QUOTE v))))
              (rest *form*)))
        *package-tasks*)))
  (let* ((args (cdr *form*))    ; arguments
         (n (length args)))     ; number of arguments
    (if (or (not (fboundp fun)) (declared-notinline fun))
      ;; the function arguments will not be checked
      (c-NORMAL-FUNCTION-CALL fun)
      (multiple-value-bind (name req opt rest-p key-p keylist allow-p check)
          (function-signature fun t)
        (setq check (and name
                         (test-argument-syntax args nil fun req opt rest-p
                                               key-p keylist allow-p)))
        (if (and name (equal fun name)) ; function is valid
          (case fun
            ((CAR CDR FIRST REST NOT NULL CONS SVREF VALUES
              CAAR CADR CDAR CDDR CAAAR CAADR CADAR CADDR CDAAR CDADR
              CDDAR CDDDR SECOND THIRD FOURTH CAAAAR CAAADR CAADAR CAADDR
              CADAAR CADADR CADDAR CADDDR CDAAAR CDAADR CDADAR CDADDR
              CDDAAR CDDADR CDDDAR CDDDDR ATOM CONSP
              VALUES-LIST SYS::%SVSTORE EQ SYMBOL-FUNCTION LIST LIST*)
             ;; these here have keylist=NIL, allow-p=NIL and
             ;; (which is not used) opt=0.
             (if check ; (and (<= req n) (or rest-p (<= n (+ req opt))))
               ;; we make the call INLINE.
               (let ((sideeffects ; side-effect-class of the function-execution
                       (if (>= (declared-optimize 'SAFETY) 3)
                         *seclass-dirty* ; see comment in F-SIDE-EFFECT
                         (function-side-effect fun)))) ; no need for a check
                 (if (and (null *for-value*) (null (seclass-modifies sideeffects)))
                   ;; don't have to call the function,
                   ;; only evaluate the arguments
                   (c-form `(PROGN ,@args))
                   (if (and (eq fun 'VALUES) (eq *for-value* 'ONE))
                     (if (= n 0) (c-NIL) (c-form `(PROG1 ,@args)))
                     (let ((seclass sideeffects)
                           (codelist '()))
                       (let ((*stackz* *stackz*))
                         ;; evaluate the arguments and push all to the
                         ;; stack except for the last (because the last
                         ;; one is expected in A0):
                         (loop
                           (when (null args) (return))
                           (let ((anode (c-form (pop args) 'ONE)))
                             (seclass-or-f seclass anode)
                             (push anode codelist))
                           (when args ; not at the end
                             (push '(PUSH) codelist)
                             (push 1 *stackz*)))
                         (setq codelist
                           (nreconc codelist
                             (case fun
                               ((CAR FIRST) `((CAR ,*denv*)))
                               ((CDR REST) `((CDR ,*denv*)))
                               (CAAR `((CAR ,*denv*) (CAR ,*denv*)))
                               ((CADR SECOND) `((CDR ,*denv*) (CAR ,*denv*)))
                               (CDAR `((CAR ,*denv*) (CDR ,*denv*)))
                               (CDDR `((CDR ,*denv*) (CDR ,*denv*)))
                               (CAAAR `((CAR ,*denv*) (CAR ,*denv*) (CAR ,*denv*)))
                               (CAADR `((CDR ,*denv*) (CAR ,*denv*) (CAR ,*denv*)))
                               (CADAR `((CAR ,*denv*) (CDR ,*denv*) (CAR ,*denv*)))
                               ((CADDR THIRD) `((CDR ,*denv*) (CDR ,*denv*) (CAR ,*denv*)))
                               (CDAAR `((CAR ,*denv*) (CAR ,*denv*) (CDR ,*denv*)))
                               (CDADR `((CDR ,*denv*) (CAR ,*denv*) (CDR ,*denv*)))
                               (CDDAR `((CAR ,*denv*) (CDR ,*denv*) (CDR ,*denv*)))
                               (CDDDR `((CDR ,*denv*) (CDR ,*denv*) (CDR ,*denv*)))
                               (CAAAAR `((CAR ,*denv*) (CAR ,*denv*) (CAR ,*denv*) (CAR ,*denv*)))
                               (CAAADR `((CDR ,*denv*) (CAR ,*denv*) (CAR ,*denv*) (CAR ,*denv*)))
                               (CAADAR `((CAR ,*denv*) (CDR ,*denv*) (CAR ,*denv*) (CAR ,*denv*)))
                               (CAADDR `((CDR ,*denv*) (CDR ,*denv*) (CAR ,*denv*) (CAR ,*denv*)))
                               (CADAAR `((CAR ,*denv*) (CAR ,*denv*) (CDR ,*denv*) (CAR ,*denv*)))
                               (CADADR `((CDR ,*denv*) (CAR ,*denv*) (CDR ,*denv*) (CAR ,*denv*)))
                               (CADDAR `((CAR ,*denv*) (CDR ,*denv*) (CDR ,*denv*) (CAR ,*denv*)))
                               ((CADDDR FOURTH) `((CDR ,*denv*) (CDR ,*denv*) (CDR ,*denv*) (CAR ,*denv*)))
                               (CDAAAR `((CAR ,*denv*) (CAR ,*denv*) (CAR ,*denv*) (CDR ,*denv*)))
                               (CDAADR `((CDR ,*denv*) (CAR ,*denv*) (CAR ,*denv*) (CDR ,*denv*)))
                               (CDADAR `((CAR ,*denv*) (CDR ,*denv*) (CAR ,*denv*) (CDR ,*denv*)))
                               (CDADDR `((CDR ,*denv*) (CDR ,*denv*) (CAR ,*denv*) (CDR ,*denv*)))
                               (CDDAAR `((CAR ,*denv*) (CAR ,*denv*) (CDR ,*denv*) (CDR ,*denv*)))
                               (CDDADR `((CDR ,*denv*) (CAR ,*denv*) (CDR ,*denv*) (CDR ,*denv*)))
                               (CDDDAR `((CAR ,*denv*) (CDR ,*denv*) (CDR ,*denv*) (CDR ,*denv*)))
                               (CDDDDR `((CDR ,*denv*) (CDR ,*denv*) (CDR ,*denv*) (CDR ,*denv*)))
                               (ATOM '((ATOM)))
                               (CONSP '((CONSP)))
                               ((NOT NULL) '((NOT)))
                               (CONS '((CONS)))
                               (SVREF `((SVREF ,*denv*)))
                               (SYS::%SVSTORE '((SVSET)))
                               (EQ '((EQ)))
                               (VALUES (case n
                                         (0 '((VALUES0)) )
                                         (1 '((VALUES1)) )
                                         (t `((PUSH) ; also push last argument to the Stack
                                              (STACK-TO-MV ,n)))))
                               (VALUES-LIST '((LIST-TO-MV)))
                               (SYMBOL-FUNCTION `((SYMBOL-FUNCTION ,*denv*)))
                               (LIST (if (plusp n)
                                       `((PUSH) (LIST ,n))
                                       '((NIL))))
                               (LIST* (case n
                                        (1 '((VALUES1)) )
                                        (t `((LIST* ,(1- n))) )))
                               (t (compiler-error 'c-GLOBAL-FUNCTION-CALL
                                                  fun))))))
                       (make-anode
                         :type `(PRIMOP ,fun)
                         :sub-anodes (remove-if-not #'anode-p codelist)
                         :seclass seclass
                         :code codelist)))))
               ;; check failed (wrong argument count) => not INLINE:
               (c-NORMAL-FUNCTION-CALL fun)))
            (t ; is the SUBR fun contained in the FUNTAB?
             (let ((index (gethash fun function-codes)))
               (if index
                 (case check
                   ((NO-KEYS STATIC-KEYS)
                    ;; correct syntax, stack layout is known
                    ;; at compile time ==> INLINE
                    (c-DIRECT-FUNCTION-CALL
                      args nil fun req opt rest-p keylist keylist
                      t ; it is a SUBR
                      (let ((call-code ; call with help from the FUNTAB:
                              (cons
                                (if (not rest-p)
                                  (CALLS-code index)
                                  `(CALLSR ,(max 0 (- n req opt)) ; When n<req+opt another (PUSH-UNBOUND ...) still has to come
                                           ,(- index funtabR-index)))
                                (case fun
                                  (;; functions, that do not return:
                                   (;; control.d:
                                    SYS::DRIVER SYS::UNWIND-TO-DRIVER
                                    ;; debug.d:
                                    ;; SYS::REDO-EVAL-FRAME
                                    ;; SYS::RETURN-FROM-EVAL-FRAME
                                    ;; error.d:
                                    ERROR SYSTEM::ERROR-OF-TYPE
                                    INVOKE-DEBUGGER)
                                   '((BARRIER)))
                                  (t '())))))
                        #'(lambda () call-code))))
                   (t (c-NORMAL-FUNCTION-CALL fun)))
                 ;; not a SUBR
                 (let ((inline-lambdabody (inline-lambdabody fun)))
                   (if (inline-callable-lambdabody-p inline-lambdabody n)
                     ;; inline call of the global function is possible
                     (c-FUNCALL-INLINE fun args nil inline-lambdabody nil)
                     (c-NORMAL-FUNCTION-CALL fun)))))))
          (c-NORMAL-FUNCTION-CALL fun))))))

;; Alist mapping a function name to format string and arguments.
(defvar *deprecated-functions-alist*
  ;; The deprecated ANSI CL functions. More are added in deprecated.lisp.
  '((GENTEMP  "This function creates symbols that cannot be garbage-collected. Use ~S instead." gensym)
    (SET      "This function name is anachronistic. Use ~S ~S instead." setf symbol-value)))

;; note a global function call
;; NAME is function name, ARGS are arguments, APPLY-ARGS are APPLY arguments
;; ARGS == 0 means non-funcall context
;; this adds an unknown function to the `*unknown-functions*' list
;;  an unknown function is (NAME C-SOURCE-POINT . (ARGS . APPLY-ARGS))
;;  or (NAME C-SOURCE-POINT) if non-funcall context
(defun note-function-used (name args apply-args)
  (unless (fboundp name)
    (if *compiling-from-file*
      (let ((kf (assoc name *known-functions* :test #'equal))
            (uf (if (listp args)
                  (list* name (make-c-source-point) args apply-args)
                  (list name (make-c-source-point)))))
        (if kf
          (match-known-unknown-functions uf kf)
          (push uf *unknown-functions*)))
      (unless (defining-p name)
        (c-warn (TEXT "Function ~s is not defined") name))))
  (let ((deprecation-info (assoc name *deprecated-functions-alist* :test #'eq)))
    (when deprecation-info
      (if *compiling-from-file*
        (pushnew name *deprecated-functions* :test #'eq)
        (apply #'c-warn (string-concat
                         (TEXT "Function ~S is deprecated.") " ~@?")
                        deprecation-info)))))

;; note global OPTIMIZE proclamations
;; used by c-PROCLAIM and PROCLAIM in control.d
(defun note-optimize (specs)
  (dolist (quality specs)
    (multiple-value-bind (quality value) (parse-optimize-quality quality)
      (when quality
        (setf (gethash quality *optimize*) value)))))

;; auxiliary function: PROCLAIM on file-compilation, cf. function PROCLAIM
(defun c-PROCLAIM (declspec)
  (when (consp declspec)
    (case (car declspec)
      (SPECIAL
        (dolist (var (cdr declspec))
          (when (symbolp var) (pushnew var *known-special-vars* :test #'eq))))
      (NOTSPECIAL
       (dolist (var (cdr declspec))
         (when (symbolp var) (delete var *known-special-vars* :test #'eq))))
      (INLINE
        (dolist (var (cdr declspec))
          (when (function-name-p var)
            (pushnew var *inline-functions* :test #'equal)
            (setq *notinline-functions* (delete var *notinline-functions*
                                                :test #'equal)))))
      (NOTINLINE
        (dolist (var (cdr declspec))
          (when (function-name-p var)
            (pushnew var *notinline-functions* :test #'equal)
            (setq *inline-functions* (delete var *inline-functions*
                                             :test #'equal)))))
      (CONSTANT-INLINE
        (dolist (var (cdr declspec))
          (when (symbolp var)
            (pushnew var *inline-constants*)
            (setq *notinline-constants* (delete var *notinline-constants*)))))
      (CONSTANT-NOTINLINE
        (dolist (var (cdr declspec))
          (when (symbolp var)
            (pushnew var *notinline-constants*)
            (setq *inline-constants* (delete var *inline-constants*)))))
      (OPTIMIZE (note-optimize (cdr declspec)))
      (DECLARATION
        (dolist (var (cdr declspec))
          (when (symbolp var) (pushnew var *user-declaration-types*
                                       :test #'eq)))))))

;; DEFCONSTANT when compiling
(defun c-PROCLAIM-CONSTANT (symbol initial-value-form) ; ABI
  (when *compiling-from-file*
    (pushnew symbol *known-special-vars* :test #'eq)
    (when (c-constantp initial-value-form)
      (push (cons symbol (c-constant-value initial-value-form))
            *constant-special-vars*))))

;; DEFUN when compiling
(defun c-DEFUN (symbol signature &optional lambdabody (type 'defun)) ; ABI
  (when *compiling* ; c-DEFUN can also be called by the Expander!
    (when *compiling-from-file*
      (let ((kf (assoc symbol *known-functions* :test #'equal)))
        (when (and kf (eq type 'defun))
          ;; The check (eq type 'defun) cuts off
          ;; defmethod forms, which can appear many times in the
          ;; same file.  We could have made a special effort, like this:
          ;;  (let ((def (and (fboundp symbol) (fdefinition symbol))))
          ;;    (when (typep def clos::<standard-generic-function>)
          ;;      (clos::check-signature-congruence
          ;;        def symbol (clos::generic-function-signature def) signature)))
          ;; but it would be a waste of time since the signature
          ;; congruence check will be done at load time anyway and
          ;; - the above check catches only separate `defmethod' forms,
          ;;   but misses `:method's in `defgeneric' forms,
          ;; - the above check works only for already defined generic
          ;;   functions but not for generic functions defined in this file.
          (c-warn (TEXT "Function ~s~% was already defined~a~:[~% with the signature~%~a~% it is being re-defined with a new signature~%~a~;~2*~]")
                  symbol (c-source-point-location (second kf))
                  (equalp signature (third kf))
                  (sig-to-list (third kf))
                  (sig-to-list signature))))
      (pushnew (list symbol (make-c-source-point) signature *seclass-dirty*)
               *known-functions* :test #'equal :key #'car)
      (when lambdabody
        ;; lambdabody given ==> function definition is in the
        ;; top-level environment and can be inlined
        (push (cons symbol lambdabody) *inline-definitions*)))))

;; auxiliary function: PROVIDE on file-compilation, cf. function PROVIDE
(defun c-PROVIDE (module-name)
  (pushnew (module-name module-name) *compiled-modules* :test #'string=))

;; auxiliary function: REQUIRE on file-compilation, cf. function REQUIRE
(defun c-REQUIRE (module-name &optional (pathname nil p-given))
  (setq module-name (module-name module-name))
  (unless (or (member module-name *modules* :test #'string=)
              (member module-name *compiled-modules* :test #'string=))
    (unless p-given (setq pathname (pathname module-name)))
    (flet ((load-lib (file)
             (let* ((*load-paths*
                      (cons (make-pathname :name nil :type nil
                                           :defaults *compile-file-truename*)
                            *load-paths*))
                    (present-files
                      (search-file file (append *source-file-types* '("lib"))))
                    (newest-file (first present-files)))
               ;; if the libfile occurs among the found Files
               ;; and if it is the newest:
               (if (and (consp present-files)
                        (string= (pathname-type newest-file) "lib"))
                 (load newest-file :verbose nil :print nil :echo nil) ; load libfile
                 (let ((fi (or newest-file file)))
                   (if (null *compile-file-directory*)
                     ;; `compile-file' was called without an explicit
                     ;; :output-file arg, so compile `in place'
                     (compile-file fi)
                     ;; `compile-file' was given :output-file,
                     ;; so put the compiled file there
                     (compile-file fi :output-file
                                   (merge-pathnames *compile-file-directory*
                                                    fi))))))))
      (if (atom pathname) (load-lib pathname) (mapcar #'load-lib pathname)))))

;;; auxiliary functions for
;;; LET/LET*/MULTIPLE-VALUE-BIND/Lambda-Expression/FLET/LABELS:

;; Syntax-Analysis:

;; analyzes a parameter-list of LET/LET*, returns:
;; the List of Symbols,
;; the List of Forms.
(defun analyze-letlist (parameters)
  (do ((L parameters (cdr L))
       (symbols nil)
       (forms nil))
      ((null L) (values (nreverse symbols) (nreverse forms)))
    (cond ((symbolp (car L)) (push (car L) symbols) (push nil forms))
          ((and (consp (car L)) (symbolp (caar L))
                (or (null (cdar L))
                    (and (consp (cdar L)) (null (cddar L)))))
           (push (caar L) symbols) (push (cadar L) forms))
          (t (c-error-c (TEXT "Illegal syntax in LET/LET*: ~S")
                        (car L))))))

;; Analyzes a lambda-list of a function (CLtL2 p. 76, ANSI CL 3.4.1.).
;; Returns 13 values:
;; 1. list of required parameters
;; 2. list of optional parameters
;; 3. list of init-forms of the optional parameters
;; 4. list of supplied-vars for the optional parameters (0 for the missing)
;; 5. &rest parameter or 0
;; 6. flag, if keywords are allowed
;; 7. list of keywords
;; 8. list of keyword parameters
;; 9. list of init-forms of the keyword parameters
;; 10. list of supplied-vars for the keyword parameters (0 for the missing)
;; 11. flag, if other keywords are allowed
;; 12. list of &aux variables
;; 13. list of init-forms of the &aux variables
(defun c-analyze-lambdalist (lambdalist)
  (sys::analyze-lambdalist lambdalist
    #'(lambda (form errorstring &rest arguments)
        (declare (ignore form))
        (catch 'c-error
          (apply #'c-error errorstring arguments)))))

(defun lambda-list-to-signature (lambda-list)
  (multiple-value-bind (req opt opt-i opt-p rest
                        key-p keywords key-v key-i key-v-p allow-p)
      (c-analyze-lambdalist lambda-list)
    (declare (ignore opt-i opt-p key-v key-i key-v-p))
    (make-signature :req-num (length req) :opt-num (length opt)
                    :rest-p (not (eql 0 rest)) :keys-p key-p
                    :keywords keywords :allow-p allow-p)))

;; return the inline lambdabody for the function FUN (if any)
(defun inline-lambdabody (fun)
  (or (and *compiling-from-file*
           (cdr (assoc fun *inline-definitions* :test #'equal)))
      (get (get-funname-symbol fun) 'sys::inline-expansion)))

;;; (inline-callable-function-lambda-p form n) or
;;; (inline-callable-function-p form n) check whether the FORM
;;; can be called inline with N (and maybe MORE) arguments
;;; (subject to syntax errors in the lambda list)
;;; form should be already macroexpanded.
(defun inline-callable-function-lambda-p (form n &optional (more nil))
  (and (function-form-p form)
       (let ((funname (second form)))
         ;; funname must be (LAMBDA lambdalist ...)
         (and (lambda-form-p funname)
              (let ((lambdalist (second funname)))
                ;; lambdalist must be a list, with no &KEYs
                ;; (functions with &KEYs cannot be expanded INLINE, since these
                ;; arguments are dynamically bound to the variables.
                ;; it is possible to speed up APPLY with GETF in assembler)
                (and (listp lambdalist)
                     (not (position '&KEY lambdalist))
                     (not (position '&ALLOW-OTHER-KEYS lambdalist))
                     (let ((&opt-pos (position '&OPTIONAL lambdalist))
                           (&rest-pos (position '&REST lambdalist))
                           (&aux-pos (or (position '&AUX lambdalist)
                                         (length lambdalist))))
                       (if &rest-pos
                         (or more (>= n (or &opt-pos &rest-pos)))
                         (if more
                           (<= n (if &opt-pos (- &aux-pos 1) &aux-pos))
                           (if &opt-pos
                             (<= &opt-pos n (- &aux-pos 1))
                             (= n &aux-pos)))))))))))
(defun inline-callable-lambdabody-p (inline-lambdabody n &optional (more nil))
  (and (consp inline-lambdabody)
       (inline-callable-function-lambda-p
        `(FUNCTION (LAMBDA ,@inline-lambdabody)) n more)))
(defun inline-callable-function-p (form n)
  (or (inline-callable-function-lambda-p form n)
      (and (function-form-p form)
           (let ((fun (second form)))
             ;; fun must be a function name with an inline-definition,
             ;; then (FUNCALL fun ...) is converted to (fun ...) compiled inline.
             ;; see c-FUNCALL, c-FUNCTION-CALL, c-GLOBAL-FUNCTION-CALL.
             (and (function-name-p fun) (null (fenv-search fun))
                  (not (and (symbolp fun)
                            (or (special-operator-p fun) (macro-function fun))))
                  (not (declared-notinline fun))
                  (or #| ;; probably not worth it
                      (and (in-defun fun)
                           (multiple-value-bind
                                (req opt rest-flag key-flag keylist allow-flag)
                             (fdescr-signature (cons *func* nil))
                             (declare (ignore keylist allow-flag))
                             (and (<= req n) (or rest-flag (<= n (+ req opt)))
                                  (not key-flag))))
                      |#
                      (inline-callable-lambdabody-p (inline-lambdabody fun) n)))))))

;; specially declared symbols:
(defvar *specials*)   ; list of all symbols recently declared special
(defvar *ignores*)    ; list of all symbols recently declared ignore
(defvar *ignorables*) ; list of all symbols recently declared ignorable
(defvar *readonlys*)  ; list of all symbols recently declared read-only

;; push all symbols for special variables into *venv* :
(defun push-specials ()
  (apply #'push-*venv* (mapcar #'make-special-var *specials*)))

;; checks if a variable is rightly ignore-declared...
(defun ignore-check (var)
  (let ((sym (var-name var)))
    (if (memq sym *ignores*)
      ;; var ignore-declared
      (if (var-specialp var)
        (c-warn (TEXT "Binding variable ~S can cause side effects despite IGNORE declaration~%since it is declared SPECIAL.")
                sym)
        (if (var-for-value-usedp var)
          (c-style-warn (TEXT "variable ~S is used despite IGNORE declaration.")
                        sym)))
      ;; var not ignore-declared
      (unless (memq sym *ignorables*)
        ;; var also not ignorable-declared
        (unless (or (var-specialp var) (var-usedp var))
          ;; var lexically and unused
          (unless (null (symbol-package sym)) ; sym a (gensym) ?
            ;; (symbols without Home-Package do not originate from the user,
            ;; the warning would only cause confusion).
            (c-style-warn (TEXT "variable ~S is not used.~%Misspelled or missing IGNORE declaration?")
                          sym)))))
    (when (memq sym *readonlys*)
      (unless (var-specialp var)
        (when (var-assignedp var)
          (c-warn (TEXT "The variable ~S is assigned to, despite READ-ONLY declaration.")
                  sym))))))

;; returns the Code, that is necessary for the new construction of a
;; Closure and its placement in the Stack:
;; this Code extends the Venv described by (cdr venvc) with closurevars,
;; (cdr stackz) is the current stack-state.
;; After the construction of the Closure venvc resp.
;; stackz are the current states.
(defun c-MAKE-CLOSURE (closurevars venvc stackz)
  (if closurevars
    `((VENV ,(cdr venvc) ,(cdr stackz))
      (MAKE-VECTOR1&PUSH ,(length closurevars)))
    '()))

;;; There are two ways to bind variables:
;; 1. fixed-var: the variable has a position in the Stack, may not be optimized
;;               away. If the Variable is in the Closure, however, its value
;;               must be transfered there; if the Variable is dynamic, a
;;               binding-frame has to be opened up.
;;               occurrence: MULTIPLE-VALUE-BIND, Lambda-Expression (required,
;;               optional, rest, keyword - Parameter)
;; 2. movable-var: the Variable may be optimized away, if it is constant
;;                 (it is either dynamic and constant or lexical
;;                  and bound to a constant and never SETQ-ed). So
;;                 the init-value plays a role.
;;                 occurrence: LET, LET*, Lambda-Expression (optional-svar,
;;                 keyword-svar, aux-Variable)

;; 1. fixed-var

;; binding of a fixed-var:
;; symbol --> Variable
;; Leaves *stackz* unchanged.
(defun bind-fixed-var-1 (symbol)
  (if (or (constantp symbol)
          (proclaimed-special-p symbol)
          (memq symbol *specials*))
    ;; must bind symbol dynamically:
    (progn
      (when (l-constantp symbol)
        (c-error-c (TEXT "Constant ~S cannot be bound.")
                   symbol))
      (make-special-var symbol))
    ;; must bind symbol lexically :
    (make-var :name symbol :specialp nil :constantp nil
              :usedp nil :for-value-usedp nil :really-usedp nil
              :closurep nil
              :stackz *stackz* :venvc *venvc* :fnode *func*)))

;; registers in *stackz*, that a fixed-var is being bound
(defun bind-fixed-var-2 (var)
  (when (and (var-specialp var) (not (var-constantp var)))
    (push '(BIND 1) *stackz*)))

;; returns the Code that binds the variable var to the content of stackdummyvar
;; stackz is the stack-state before the binding of this variable.
(defun c-bind-fixed-var (var stackdummyvar stackz)
  (if (var-specialp var)
    (if (var-constantp var)
      '() ; constant cannot be bound
      `((GET ,stackdummyvar ,*venvc* ,stackz)
        (BIND ,(new-const (var-name var)))))
    ; var lexical, not constant by definition
    (if (var-closurep var)
      `((GET ,stackdummyvar ,*venvc* ,stackz)
        (SET ,var ,*venvc* ,stackz))
      '()))) ; var and stackdummyvar identical

;; creates a Stack-Variable and a Fixed-Variable at a time for each
;; Symbol from the Variable-List symbols and returns both lists as values.
(defun process-fixed-var-list (symbols &optional optimflags)
  (do ((symbolsr symbols (cdr symbolsr))
       (optimflagsr optimflags (cdr optimflagsr))
       (varlist nil) ; list of Variables
       (stackvarlist nil)) ; list of Stackvariables (partly Dummys)
      ((null symbolsr) (values (nreverse varlist) (nreverse stackvarlist)))
    (push 1 *stackz*)
    ;; with constantp=nil and really-usedp=t,
    ;; in order to avoid optimizing it away
    (push (make-var :name (gensym) :specialp nil :constantp nil
                    :usedp nil :for-value-usedp nil
                    :really-usedp (null (car optimflagsr)) :closurep nil
                    :stackz *stackz* :venvc *venvc* :fnode *func*)
          stackvarlist)
    (push (bind-fixed-var-1 (car symbolsr)) varlist)))

;; Eliminates all assignments to an unused Variable.
(defun unmodify-unused-var (var)
  (dolist (modified (var-modified-list var))
    (if (cddr modified)
      ;; value of the assignment is needed
      (let ((set-anode (second modified))) ; Anode of the assignment itself
        (setf (anode-code set-anode) '((VALUES1)))) ; remove assignment
      ;; value of the assignment is not needed
      (progn
        (let ((value-anode (first modified))) ; Anode for assigned value
          (when (anode-side-effect-free-p value-anode)
            (setf (anode-code value-anode) '()))) ; poss. remove value-form
        (let ((set-anode (second modified))) ; Anode of the assignment itself
          (setf (anode-code set-anode) '())))))) ; remove assignment

;; checks and optimizes the variables
;; and returns the list of Closure-Variables (in the right order).
(defun checking-fixed-var-list (varlist &optional optimflaglist)
  (let ((closurevarlist '()))
    (dolist (var varlist (nreverse closurevarlist))
      ;; 1st step: write poss. warnings
      (ignore-check var)
      ;; 2nd step: finally determine variable-location (Stack or Closure),
      ;; poss. optimize
      (unless (var-specialp var)
        ;; only lexical Variables can lie in the Closure,
        ;; only lexical Variables can be optimized
        (if (not (var-really-usedp var))
          ;; Variable lexical and unused
          (progn ; eliminate variable
            (setf (var-closurep var) nil)
            (when (car optimflaglist) ; optimize-able fixed-var?
              (setf (first (var-stackz var)) 0) ; remove from stack
              (setf (car optimflaglist) 'GONE)) ; note as gone
            (unmodify-unused-var var)) ; eliminate assignments to var
          (when (var-closurep var)
            ; variable must lie in the closure
            (push var closurevarlist))))
      (setq optimflaglist (cdr optimflaglist)))))

;; 2. movable-var

;; At the Binding of a Variable var to an Anode anode:
;; Is the lexical Variable being bound to the value at a lexical
;; variable? If so, to which variable?
(defun bound-to-var-p (var anode)
  (if (var-specialp var)
    nil
    ;; var lexically
    (loop
      (unless (eql (length (anode-code anode)) 1) (return nil))
      (setq anode (first (anode-code anode)))
      (unless (anode-p anode)
        (if (and (consp anode) (eq (first anode) 'GET))
          ;; Code at the Anode consists exactly of ((GET outervar ...)).
          (return (second anode))
          (return nil))))))

;; Binding of a movable-var:
;; symbol form-anode --> Variable
;; extends *stackz* exactly by one entry
(defun bind-movable-var (symbol form-anode)
  (if (or (constantp symbol)
          (proclaimed-special-p symbol)
          (memq symbol *specials*))
    ;; must bind symbol dynamically:
    (progn
      (if (l-constantp symbol)
        (progn
          (c-error-c (TEXT "Constant ~S cannot be bound.") symbol)
          (push 0 *stackz*))
        (push '(BIND 1) *stackz*))
      (make-special-var symbol))
    ;; must bind symbol lexically:
    (let ((var
            (progn
              (push 1 *stackz*) ; preliminary: 1 place on the Stack
              (make-var :name symbol :specialp nil
                :constantp (anode-constantp form-anode) ; is set to NIL for assignments
                :constant (if (anode-constantp form-anode)
                            (anode-constant form-anode))
                :usedp nil :for-value-usedp nil :really-usedp nil
                :closurep nil ; is possibly set to T
                :stackz *stackz* :venvc *venvc* :fnode *func*))))
      (let ((outervar (bound-to-var-p var form-anode)))
        (when outervar ; if var is bound to a variable outervar, later
                       ; poss. each reference to var may be converted
                       ; to a reference to outervar.
          (push (list var form-anode) (var-replaceable-list outervar))))
      var)))

;; returns the code, that binds the variable var to A0:
(defun c-bind-movable-var (var)
  (if (var-specialp var)
    (if (var-constantp var)
      '() ; dynamic constants cannot be bound
      `((BIND ,(new-const (var-name var)))))
    (if (var-closurep var)
      ;; write Closure-Variable:
      ;; (var-stackz var) = (0 . ...) is the current stack-state.
      `((SET ,var ,*venvc* ,(var-stackz var)))
      ;; lexical Variable: was poss. eliminated from the Stack
      (if (zerop (first (var-stackz var)))
        '()
        `((PUSH)))))) ; in the Stack: write to the next-lower stack-location

;; returns the code that binds the variable var to the result of the
;; ANODE anode
(defun c-bind-movable-var-anode (var anode)
  (let ((binding-anode
          (make-anode :type 'BIND-MOVABLE
                      :sub-anodes '()
                      :seclass *seclass-pure*
                      :code (c-bind-movable-var var))))
    (let ((outervar (bound-to-var-p var anode)))
      (when outervar ; if var is bound to a Variable outervar, later
                     ; poss. each reference to var may be converted to a
                     ; reference to outervar.
        (dolist (innervar-info (var-replaceable-list outervar))
          (when (eq (first innervar-info) var) ; additionally set binding-anode
            (setf (cddr innervar-info) binding-anode)))))
    (list anode binding-anode)))

;; (process-movable-var-list symbols initforms *-flag) compiles the initforms
;; (like for LET/LET*) and associates them with the variables to symbols.
;; changes *venv* (for *-flag : incrementally, else all at once).
;; returns three values:
;; 1. list of Variables,
;; 2. list of ANODEs for the initforms,
;; 3. list of stack-states after the binding of the variables.
(defun process-movable-var-list (symbols initforms *-flag)
  (do ((symbolsr symbols (cdr symbolsr))
       (initformsr initforms (cdr initformsr))
       (varlist '())
       (anodelist '())
       (stackzlist '()))
      ((null symbolsr)
       (unless *-flag (apply #'push-*venv* varlist)) ; binding at LET
       (values (nreverse varlist) (nreverse anodelist) (nreverse stackzlist)))
    (let* ((initform (car initformsr))
           (anode (c-form initform 'ONE)) ; compile initform
           (var (bind-movable-var (car symbolsr) anode)))
      (push anode anodelist)
      (push var varlist)
      (push *stackz* stackzlist)
      (when *-flag (push-*venv* var))))) ; binding at LET*

;; checks and optimizes the variables (like at LET/LET*)
;; and returns the list of Closure-Variables (in the right order).
(defun checking-movable-var-list (varlist anodelist)
  (do ((varlistr varlist (cdr varlistr))
       (anodelistr anodelist (cdr anodelistr))
       (closurevarlist '()))
      ((null varlistr) (nreverse closurevarlist))
    (let ((var (car varlistr)))
      (when var
        ;; 1st step: write poss. warnings
        (ignore-check var)
        ;; 2nd step: finally determine variable-location
        ;; (Stack or Closure or eliminated)
        (unless (var-specialp var)
          ;; can only be optimized for lexical variables
          (if (var-constantp var)
            ;; Variable lexical and constant
            (progn ; eliminate variable
              (setf (var-closurep var) nil)
              (setf (first (var-stackz var)) 0) ; remove from Stack
              (when (anode-side-effect-free-p (car anodelistr))
                ;; maybe remove initform
                (setf (anode-code (car anodelistr)) '())))
            (if (not (var-really-usedp var))
              ;; Variable lexical and unused
              (progn ; eliminate variable
                (setf (var-closurep var) nil)
                (setf (first (var-stackz var)) 0) ; remove from Stack
                (when (anode-side-effect-free-p (car anodelistr))
                  ;; maybe remove initform
                  (setf (anode-code (car anodelistr)) '()))
                (unmodify-unused-var var)) ; eliminate assignments to var
              (when (var-closurep var)
                ;; Variable must lie in the Closure
                (setf (first (var-stackz var)) 0) ; occupies 0 Stack-Entries
                (push var closurevarlist)))))))))

;; Optimizes a list of variables.
;; (The lexically inner variables have to occur at the end of the list.)
(defun optimize-var-list (vars)
  (unless *no-code*
    (dolist (var (reverse vars))
      (when var
        ;; Optimization (inner variables first):
        ;; If a Variable innervar is bound to the value of var, and if
        ;; during the life-time of innervar neither innervar nor var are
        ;; changed (in order to be able to assure this, both must be
        ;; lexical and in the Stack), innervar can be replaced by var.
        (unless (or (var-specialp var) (var-closurep var))
          ;; var is lexical and in the Stack
          (dolist (innervar-info (var-replaceable-list var))
            (let ((innervar (first innervar-info)))
              ;; innervar is a movable-var, that is initialized with var.
              ;; during the life-time of innervar nothing is assigned to var.
              (unless (or (var-specialp innervar) (var-closurep innervar))
                ;; innervar is lexical and in the Stack
                (when (null (var-modified-list innervar))
                  ;; during the life-time of innervar nothing
                  ;; is assigned to innervar, too.
                  (unless (eql (first (var-stackz innervar)) 0)
                    ;; innervar not yet optimized away?
                    (when (cddr innervar-info)
                      ;; innervar-info consists correctly of three parts?
                      ;; eliminate Variable innervar:
                      ;; remove from Stack:
                      (setf (first (var-stackz innervar)) 0)
                      ;; eliminate initialization and binding of innervar:
                      (setf (anode-code (second innervar-info)) '())
                      (setf (anode-code (cddr innervar-info)) '())
                      ;; the references to Variable innervar are transformed
                      ;; to references to Variable var:
                      (let ((using-var (var-usedp var)))
                        (do ((using-innervar (var-usedp innervar)
                                             (cdr using-innervar)))
                            ((atom using-innervar))
                          (let* ((anode (car using-innervar)) ; type VAR anode
                                 ;; its code, () or ((GET ...))
                                 (code (anode-code anode)))
                            (unless (null code)
                              ;; (anode-code anode) has the shape
                              ;; ((GET innervar ...))
                              (setf (second (car code)) var)
                              (push anode using-var))))
                        (setf (var-usedp var) using-var)))))))))))))

;; builds the code, that binds a list of variables, together with their svars
;; (the same as for Lambdabody- Optional/Key - variables).
(defun c-bind-with-svars (-vars -dummys s-vars -anodes s-anodes -stackzs)
  (do ((-varsr -vars (cdr -varsr)) ; fixed-vars
       (-dummysr -dummys (cdr -dummysr))
       (s-varsr s-vars (cdr s-varsr)) ; movable-vars
       (-anodesr -anodes (cdr -anodesr))
       (s-anodesr s-anodes (cdr s-anodesr))
       (-stackzsr -stackzs (cdr -stackzsr))
       (L '()))
      ((null -varsr) (nreverse L))
    (when (car s-varsr)
      (setq L
        (revappend
          (c-bind-movable-var-anode (car s-varsr) (car s-anodesr))
          L)))
    (setq L
      (revappend
        (let* ((var (car -varsr))
               (stackdummyvar (car -dummysr))
               (anode (car -anodesr))
               (stackz (car -stackzsr))
               (label (make-label 'ONE)))
          (if (var-specialp var)
            `((JMPIFBOUNDP ,stackdummyvar ,*venvc* ,stackz ,label)
              ,anode
              ,label
              ,@(if (var-constantp var)
                  '() ; constant cannot be bound
                  `((BIND ,(new-const (var-name var))))))
            ;; var lexical, not constant by definition
            (if (var-closurep var)
              `((JMPIFBOUNDP ,stackdummyvar ,*venvc* ,stackz ,label)
                ,anode
                ,label
                (SET ,var ,*venvc* ,stackz))
              (if (not (var-really-usedp var))
                ;; Variable was optimized away in checking-fixed-var-list
                (if (anode-side-effect-free-p anode)
                  '()
                  `((JMPIFBOUNDP ,stackdummyvar ,*venvc* ,stackz ,label)
                    ,anode
                    ,label))
                ;; variable available in stack
                `((JMPIFBOUNDP ,stackdummyvar ,*venvc* ,stackz ,label)
                  ,anode
                  (SET ,var ,*venvc* ,stackz)
                  ,label)))))
        L))))

;; add 1 stack slot for closure dummy
(proclaim '(inline closuredummy-add-stack-slot))
(defun closuredummy-add-stack-slot (closurevars closuredummy-stackz
                                    closuredummy-venvc)
  (when closurevars
    (setf (first closuredummy-venvc) (cons closurevars closuredummy-stackz)
          (first closuredummy-stackz) 1)))

;; compile (name lambdalist {declaration|docstring}* {form}*), return the FNODE
(defun c-LAMBDABODY (name lambdabody &optional fenv-cons gf-p reqoptimflags)
  (test-list lambdabody 1)
  (let* ((*func* (make-fnode :name name :enclosing *func* :venvc *venvc*))
         (*stackz* *func*) ; empty stack
         (*venvc* (cons *func* *venvc*))
         (*func-start-label* (make-label 'NIL))
         (*anonymous-count* 0)
         (lalist (car lambdabody))
         (type-decls '())
         (anode (catch 'c-error
    ;; here it starts to become complicated
    (multiple-value-bind (reqvar  optvar optinit optsvar  restvar
                          keyflag keyword keyvar keyinit keysvar
                          allow-other-keys auxvar auxinit)
        (if fenv-cons
          ;; c-analyze-lambdalist was already called at c-LABELS
          (values-list (cddar fenv-cons))
          (progn
            (when *defun-accept-specialized-lambda-list*
              (multiple-value-setq (lalist type-decls)
                (sys::specialized-lambda-list-to-ordinary
                 lalist 'compile)))
            (c-analyze-lambdalist lalist)))
      (setf (fnode-req-anz *func*) (length reqvar)
            (fnode-opt-anz *func*) (length optvar)
            (fnode-rest-flag *func*) (not (eql restvar 0))
            (fnode-keyword-flag *func*) keyflag
            (fnode-keywords *func*) keyword
            (fnode-allow-other-keys-flag *func*) allow-other-keys)
      (when fenv-cons (setf (caar fenv-cons) *func*)) ; Fixup for c-LABELS
      (multiple-value-bind (body-rest declarations)
          (parse-body (cdr lambdabody) t)
        (setq declarations (nreconc type-decls declarations))
        (let ((oldstackz *stackz*)
              (*stackz* *stackz*)
              (*denv* *denv*)
              (*venv* *venv*)
              (*venvc* *venvc*)
              *specials* *ignores* *ignorables* *readonlys* other-decls
              req-vars req-dummys req-stackzs
              opt-vars opt-dummys opt-anodes opts-vars opts-anodes opt-stackzs
              rest-vars rest-dummys rest-stackzs
              key-vars key-dummys key-anodes keys-vars keys-anodes key-stackzs
              aux-vars aux-anodes
              closuredummy-stackz closuredummy-venvc)
          (multiple-value-setq (*specials* *ignores* *ignorables* *readonlys* other-decls)
            (process-declarations declarations))
          ;; visibility of Closure-Dummyvar:
          (push nil *venvc*)
          (setq closuredummy-venvc *venvc*)
          ;; build Stack-Dummy-Variable for reqvar, optvar, restvar, keyvar:
          (multiple-value-setq (req-vars req-dummys)
            (process-fixed-var-list reqvar reqoptimflags))
          (multiple-value-setq (opt-vars opt-dummys)
            (process-fixed-var-list optvar))
          (multiple-value-setq (rest-vars rest-dummys)
            (if (eql restvar 0)
              (values '() '())
              (process-fixed-var-list (list restvar))))
          (multiple-value-setq (key-vars key-dummys)
            (process-fixed-var-list keyvar))
          ;; room for the function itself (below the arguments):
          (push 1 *stackz*)
          ;; room for Closure-Dummyvar:
          (push 0 *stackz*)
          (setq closuredummy-stackz *stackz*)
          ;; activate the bindings of the required-parameters:
          (setq req-stackzs (bind-req-vars req-vars))
          ;; activate the bindings of the optional-parameters/svar:
          (multiple-value-setq (opt-anodes opt-stackzs opts-vars opts-anodes)
            (bind-opt-vars opt-vars opt-dummys optinit optsvar))
          ;; activate the bindings of the rest-parameters:
          (unless (eql restvar 0)
            (setq rest-stackzs (bind-rest-vars rest-vars)))
          ;; activate the bindings of the keyword-parameters/svar:
          (multiple-value-setq (key-anodes key-stackzs keys-vars keys-anodes)
            (bind-opt-vars key-vars key-dummys keyinit keysvar))
          ;; activate the bindings of the Aux-Variables:
          (multiple-value-setq (aux-vars aux-anodes)
            (bind-aux-vars auxvar auxinit))
          (push-specials)
          (push-*denv* other-decls)
          (let* ((body-anode (c-form `(PROGN ,@body-rest) (if gf-p 'ONE 'ALL)))
                 ;; check the variables:
                 (closurevars
                   (append
                     (checking-fixed-var-list req-vars reqoptimflags)
                     (checking-fixed-var-list opt-vars)
                     (checking-movable-var-list opts-vars opts-anodes)
                     (checking-fixed-var-list rest-vars)
                     (checking-fixed-var-list key-vars)
                     (checking-movable-var-list keys-vars keys-anodes)
                     (checking-movable-var-list aux-vars aux-anodes)))
                 (codelist
                   `(,*func-start-label*
                     ,@(c-make-closure closurevars closuredummy-venvc
                                       closuredummy-stackz)
                     ,@(mapcap #'c-bind-fixed-var req-vars req-dummys
                               req-stackzs)
                     ,@(c-bind-with-svars opt-vars opt-dummys opts-vars
                                          opt-anodes opts-anodes opt-stackzs)
                     ,@(mapcap #'c-bind-fixed-var rest-vars rest-dummys
                               rest-stackzs)
                     ,@(c-bind-with-svars key-vars key-dummys keys-vars
                                          key-anodes keys-anodes key-stackzs)
                     ,@(mapcap #'c-bind-movable-var-anode aux-vars aux-anodes)
                     ,body-anode
                     (UNWIND ,*stackz* ,oldstackz t)
                     ,(if gf-p '(RETGF) '(RET))))
                 (anode-list `(,@opt-anodes ,@(remove nil opts-anodes)
                               ,@key-anodes ,@(remove nil keys-anodes)
                               ,@aux-anodes ,body-anode))
                 (anode
                   (make-anode
                     :type 'LAMBDABODY
                     :source lambdabody
                     :sub-anodes anode-list
                     :seclass *seclass-foldable*
                     :stackz oldstackz
                     :code codelist)))
            (closuredummy-add-stack-slot
             closurevars closuredummy-stackz closuredummy-venvc)
            (optimize-var-list (append req-vars opt-vars opts-vars rest-vars
                                       key-vars keys-vars aux-vars))
            (setf (anode-seclass anode) (anodelist-seclass-or anode-list))
            anode))))
    ;; this was the production of the Anode
    )))
    (setf (fnode-code *func*) anode)
    (when reqoptimflags
      (decf (fnode-req-anz *func*) (count 'GONE reqoptimflags)))
    (when (eq (anode-type anode) 'ERROR)
      ;; turn it into a correct function, that does nothing
      (setf (fnode-req-anz *func*) 0
            (fnode-opt-anz *func*) 0
            (fnode-rest-flag *func*) t
            (fnode-keyword-flag *func*) nil
            (fnode-keywords *func*) '()
            (fnode-allow-other-keys-flag *func*) nil
            (anode-code (fnode-code *func*)) `((NIL) (SKIP 2) (RET))))
    (setf (fnode-gf-p *func*) gf-p)
    (setf (fnode-venvconst *func*)
          (not (and (null (fnode-far-used-vars *func*))
                    (null (fnode-far-assigned-vars *func*)))))
    (setf (fnode-Consts-Offset *func*)
      (+ (setf (fnode-Keyword-Offset *func*)
           (+ (setf (fnode-Tagbodys-Offset *func*)
                (+ (setf (fnode-Blocks-Offset *func*)
                     (if (fnode-venvconst *func*) 1 0))
                   (length (fnode-Blocks *func*))))
              (length (fnode-Tagbodys *func*))))
         (length (fnode-Keywords *func*))))
    (when gf-p
      ;; the dispatch of generic functions cannot refer to external blocks and
      ;; tagbodies. the keywords are indeed displaced perforce.
      (when (or (fnode-Blocks *func*) (fnode-Tagbodys *func*))
        (compiler-error 'c-LAMBDABODY "GF"))
      ;; Now (fnode-Keyword-Offset *func*) = (fnode-Tagbodys-Offset *func*) =
      ;;    = (fnode-Blocks-Offset *func*) = (if (fnode-venvconst *func*) 1 0)
    )
    ;; Set list of outer blocks that are needed by *func*.
    (setf (fnode-far-used-blocks *func*)
          (remove-if #'(lambda (block) (eq (block-fnode block) *func*))
                     (fnode-Blocks *func*)))
    ;; Set list of outer tagbodys and tags that are needed by *func*.
    (setf (fnode-far-used-tagbodys *func*)
          (remove-if #'(lambda (tagbody+tag)
                         (eq (tagbody-fnode (car tagbody+tag)) *func*))
                     (fnode-Tags *func*)))
    *func*))
(defun bind-req-vars (req-vars)
  (let ((req-stackzs '()))
    (dolist (var req-vars)
      (push-*venv* var)
      (push *stackz* req-stackzs)
      (bind-fixed-var-2 var))
    (nreverse req-stackzs)))
(defun bind-opt-vars (opt-vars opt-dummys optinit optsvar)
  (let ((opt-anodes '())
        (opt-stackzs '())
        (opts-vars '())
        (opts-anodes '()))
    (do ((opt-varsr opt-vars (cdr opt-varsr))
         (opt-dummysr opt-dummys (cdr opt-dummysr))
         (optinitr optinit (cdr optinitr))
         (optsvarr optsvar (cdr optsvarr)))
        ((null opt-varsr))
      (if (eql (car optsvarr) 0)
        (progn (push nil opts-vars) (push nil opts-anodes))
        (let* ((anode
                 (make-anode
                   :type 'OPTIONAL-SVAR
                   :sub-anodes '()
                   :seclass (make-seclass
                             :uses (list (car opt-dummysr)))
                   :code `((BOUNDP ,(car opt-dummysr) ,*venvc* ,*stackz*))))
               (var (bind-movable-var (car optsvarr) anode)))
          (push anode opts-anodes)
          (push var opts-vars)))
      (push (c-form (car optinitr) 'ONE) opt-anodes)
      (push-*venv* (car opt-varsr))
      (push *stackz* opt-stackzs) (bind-fixed-var-2 (car opt-varsr))
      (unless (eql (car optsvarr) 0) (push-*venv* (car opts-vars))))
    (values
      (nreverse opt-anodes) (nreverse opt-stackzs)
      (nreverse opts-vars) (nreverse opts-anodes))))
(defun bind-rest-vars (rest-vars)
  (let ((rest-stackzs '()))
    (push-*venv* (car rest-vars))
    (push *stackz* rest-stackzs)
    (bind-fixed-var-2 (car rest-vars))
    rest-stackzs)) ; (nreverse rest-stackzs) unnecessary
(defun bind-aux-vars (auxvar auxinit)
  (let ((aux-vars '())
        (aux-anodes '()))
    (do ((auxvarr auxvar (cdr auxvarr))
         (auxinitr auxinit (cdr auxinitr)))
        ((null auxvarr))
      (let* ((initform (car auxinitr))
             (anode (c-form initform 'ONE))
             (var (bind-movable-var (car auxvarr) anode)))
        (push anode aux-anodes)
        (push var aux-vars)
        (push-*venv* var)))
    (values (nreverse aux-vars) (nreverse aux-anodes))))

;; returns the ANODE, that itself returns (on a given current stack-state)
;; the function belonging to an FNODE as value.
(defun c-FNODE-FUNCTION (fnode &optional (*stackz* *stackz*))
  (make-anode
    :type 'FUNCTION
    :sub-anodes '()
    :seclass *seclass-pure*
    :code (if (zerop (fnode-keyword-offset fnode))
            `((FCONST ,fnode))
            `(,@(if (fnode-Venvconst fnode)
                  (prog1 ; Venv has to be passed on construction
                      `((VENV ,(fnode-venvc fnode) ,*stackz*)
                        (PUSH))
                    (push 1 *stackz*)))
              ,@(mapcap ; Block-Conses have to be passed on construction
                  #'(lambda (block)
                      (prog1
                        `(,(if (memq block (fnode-Blocks *func*))
                             `(BCONST ,block)
                             `(GET ,(block-consvar block) ,*venvc* ,*stackz*))
                           (PUSH))
                        (push 1 *stackz*)))
                  (fnode-Blocks fnode))
              ,@(mapcap ; Tagbody-Conses have to be passed on construction
                  #'(lambda (tagbody)
                      (prog1
                          `(,(if (memq tagbody (fnode-Tagbodys *func*))
                               `(GCONST ,tagbody)
                               `(GET ,(tagbody-consvar tagbody)
                                     ,*venvc* ,*stackz*))
                            (PUSH))
                        (push 1 *stackz*)))
                  (fnode-Tagbodys fnode))
              ,@(if (fnode-gf-p fnode)
                  (progn
                    (assert (= (fnode-keyword-offset fnode) 1))
                    `((FCONST ,fnode)
                      (PUSH)
                      ,(CALLS-code-fun SYSTEM::%COPY-GENERIC-FUNCTION)))
                  `((COPY-CLOSURE ,fnode ,(fnode-keyword-offset fnode))))))))


;;;;****        FIRST PASS :   SPECIAL   FORMS

;; compile (PROGN {form}*)
;; no forms -> NIL, exactly one form -> that form,
;; at least two forms -> all in order, only the last form's values
;; count.
(defun c-PROGN ()
  (test-list *form* 1)
  (let ((L (cdr *form*))) ; list of forms
    (cond ((null L) (c-NIL)) ; no form -> NIL
          ((null (cdr L)) (c-form (car L))) ; exactly one form
          (t (do (#+CLISP-DEBUG (anodelist '())
                  (seclass *seclass-foldable*)
                  (codelist '())
                  (Lr L)) ; remaining list of forms
                 ((null Lr)
                  (make-anode
                    :type 'PROGN
                    :sub-anodes (nreverse anodelist)
                    :seclass seclass
                    :code (nreverse codelist)))
               (let* ((formi (pop Lr)) ; i-th form
                      (anodei (c-form formi (if (null Lr) *for-value* 'NIL))))
                 #+CLISP-DEBUG (push anodei anodelist)
                 (seclass-or-f seclass anodei)
                 (push anodei codelist)))))))

;; compile (PROG1 form1 {form}*)
;; on *for-value* the value of form1 has to be saved in the stack
(defun c-PROG1 ()
  (test-list *form* 2)
  (if (or (null *for-value*) (and (eq *for-value* 'ONE) (null (cddr *form*))))
    (c-form `(PROGN ,@(cdr *form*)))
    (let ((anode1 (c-form (second *form*) 'ONE))
          (anode2 (let ((*stackz* (cons 1 *stackz*)))
                    (c-form `(PROGN ,@(cddr *form*)) 'NIL))))
      (make-anode
        :type 'PROG1
        :sub-anodes (list anode1 anode2)
        :seclass (anodes-seclass-or anode1 anode2)
        :code `(,anode1 (PUSH) ,anode2 (POP))))))

;; compile (PROG2 form1 form2 {form}*)
(defun c-PROG2 ()
  (test-list *form* 3)
  (c-form `(PROGN ,(second *form*) (PROG1 ,(third *form*) ,@(cdddr *form*)))))

;; compile (IF form1 form2 [form3])
;; if form1 is a constant, then the Compiler can make the fall differentiation.
(defun c-IF ()
  (test-list *form* 3 4)
  (let ((form1 (second *form*))
        (form2 (third *form*))
        (form3 (fourth *form*))) ; = NIL, if *form* only has length 3
    (let ((anode1 (c-form form1 'ONE)))
      (if (anode-constantp anode1)
        (if (anode-constant-value anode1)
          (prog1 (c-form form2) (let ((*no-code* t)) (c-form form3 'NIL)))
          (prog2 (let ((*no-code* t)) (c-form form2 'NIL)) (c-form form3)))
        (let ((anode2 (c-form form2))
              (label1 (make-label *for-value*)))
          (if form3
            (let ((anode3 (c-form form3))
                  (label2 (make-label 'NIL)))
              (make-anode
                :type 'IF
                :sub-anodes (list anode1 anode2 anode3)
                :seclass (anodes-seclass-or anode1 anode2 anode3)
                :code `(,anode1
                        (JMPIFNOT ,label2)
                        ,anode2
                        (JMP ,label1)
                        ,label2
                        ,anode3
                        ,label1)))
            ;; save one jump on if without else
            (make-anode
              :type 'IF
              :sub-anodes (list anode1 anode2)
              :seclass (anodes-seclass-or anode1 anode2)
              :code `(,anode1
                      (,(if *for-value* 'JMPIFNOT1 'JMPIFNOT) ,label1)
                      ,anode2
                      ,label1))))))))

;; compile (WHEN form1 {form}*)
(defun c-WHEN ()
  (test-list *form* 2)
  (c-form `(IF ,(second *form*) (PROGN ,@(cddr *form*)))))

;; compile (UNLESS form1 {form}*)
(defun c-UNLESS ()
  (test-list *form* 2)
  (c-form `(IF ,(second *form*) NIL (PROGN ,@(cddr *form*)))))

;; compile (AND {form}*)
(defun c-AND ()
  (test-list *form* 1)
  (cond ((null (cdr *form*)) ; no forms
         (make-anode
           :type 'AND
           :sub-anodes '()
           :seclass *seclass-foldable*
           :code '((T))))
        ((null (cddr *form*)) (c-form (second *form*))) ; exactly one form
        (t (do (#+CLISP-DEBUG (anodelist '())
                (seclass *seclass-foldable*)
                (codelist '())
                (Lr (cdr *form*))
                (label (make-label *for-value*))) ; Label at the end
               ((null Lr)
                (push label codelist)
                (make-anode
                  :type 'AND
                  :sub-anodes (nreverse anodelist)
                  :seclass seclass
                  :code (nreverse codelist)))
             (let* ((formi (pop Lr))
                    (anodei (c-form formi (if (null Lr) *for-value* 'ONE))))
               #+CLISP-DEBUG (push anodei anodelist)
               (seclass-or-f seclass anodei)
               (if (null Lr)
                 ;; last form -> take over directly
                 (push anodei codelist)
                 ;; not the last form -> create test
                 (if (anode-constantp anodei)
                   ;; constant /= NIL -> omit, constant NIL -> finished
                   (unless (anode-constant-value anodei)
                     (if *for-value* (push '(NIL) codelist))
                     (let ((*no-code* t))
                       (dolist (form Lr) (c-form form 'NIL)))
                     (setq Lr nil))
                   (progn ; normal test
                     (push anodei codelist)
                     (push `(,(if *for-value* 'JMPIFNOT1 'JMPIFNOT) ,label)
                           codelist)))))))))

;; compile (OR {form}*)
(defun c-OR ()
  (test-list *form* 1)
  (cond ((null (cdr *form*)) ; no forms
         (make-anode
           :type 'OR
           :sub-anodes '()
           :seclass *seclass-foldable*
           :code '((NIL))))
        ((null (cddr *form*)) (c-form (second *form*))) ; exactly one form
        (t (do (#+CLISP-DEBUG (anodelist '())
                (seclass *seclass-foldable*)
                (codelist '())
                (Lr (cdr *form*))
                (label (make-label *for-value*))) ; Label at the end
               ((null Lr)
                ;; If the last anode is known to be side-effect-free and to
                ;; evaluate to NIL, drop it, and turn the jump before it to
                ;; (VALUES1).
                (let ((last-anode (car codelist)))
                  (when (and last-anode (anode-constantp last-anode)
                             (null (anode-constant-value last-anode))
                             (cdr codelist))
                    (pop codelist)
                    (setf (car codelist) '(VALUES1))))
                (push label codelist)
                (make-anode
                  :type 'OR
                  :sub-anodes (nreverse anodelist)
                  :seclass seclass
                  :code (nreverse codelist)))
             (let* ((formi (pop Lr))
                    (anodei (c-form formi (if (null Lr) *for-value* 'ONE))))
               #+CLISP-DEBUG (push anodei anodelist)
               (seclass-or-f seclass anodei)
               (if (null Lr)
                 ;; last form -> take over directly
                 (push anodei codelist)
                 ;; not the last form -> create test
                 (if (anode-constantp anodei)
                   ;; constant NIL -> omit, constant /= NIL -> finished
                   (when (anode-constant-value anodei)
                     (if *for-value* (push anodei codelist))
                     (let ((*no-code* t))
                       (dolist (form Lr) (c-form form 'NIL)))
                     (setq Lr nil))
                   (progn ; normal test
                     (push anodei codelist)
                     (push `(,(if *for-value* 'JMPIF1 'JMPIF) ,label)
                           codelist)))))))))

;; compile (QUOTE object)
(defun c-QUOTE ()
  (test-list *form* 2 2)
  (let ((value (second *form*)))
    (make-anode :type 'QUOTE
                :sub-anodes '()
                :seclass *seclass-foldable*
                :code (if *for-value* `((CONST ,(new-const value))) '()))))

;; compile (THE type form)
(defun c-THE ()
  (test-list *form* 3 3)
  (c-form (third *form*))) ; simply ignore the type-declaration

;; compile (DECLARE {declspec}*)
(defun c-DECLARE ()
  (test-list *form* 1)
  (c-error (TEXT "Misplaced declaration: ~S") *form*))

;; compile (LOAD-TIME-VALUE form [read-only-p])
(defun c-LOAD-TIME-VALUE ()
  (test-list *form* 2 3)
  (let ((form (second *form*))) ; ignore read-only-p
    (make-anode
     :type 'LOAD-TIME-VALUE
     :sub-anodes '()
     :seclass *seclass-pure*
     :code (if *for-value*
             `((CONST ,(if *fasoutput-stream* ; not called right away
                         (if (and (symbolp form) (c-constantp form))
                           (make-const :horizon ':all
                                       :form form
                                       :ltv-form *form*
                                       :value (c-constant-value form))
                           (make-const :horizon ':form
                                       :form form
                                       :ltv-form *form*))
                         (make-const :horizon ':all
                                     :value (eval form)
                                     :form form
                                     :ltv-form *form*))))
             '()))))

;; compile (CATCH tag {form}*)
(defun c-CATCH ()
  (test-list *form* 2)
  (let* ((anode1 (c-form (second *form*) 'ONE))
         (anode2 (let ((*stackz* (cons 'CATCH *stackz*)))
                   (c-form `(PROGN ,@(cddr *form*)))))
         (label (make-label *for-value*)))
    (make-anode :type 'CATCH
                :sub-anodes (list anode1 anode2)
                :seclass (anodes-seclass-or anode1 anode2)
                :code `(,anode1
                        (CATCH-OPEN ,label)
                        ,anode2
                        (CATCH-CLOSE)
                        ,label))))

;; compile (THROW tag form)
(defun c-THROW ()
  (test-list *form* 3 3)
  (let* ((anode1 (c-form (second *form*) 'ONE))
         (anode2 (let ((*stackz* (cons 1 *stackz*)))
                   (c-form (third *form*) 'ALL))))
    (make-anode :type 'THROW
                :sub-anodes (list anode1 anode2)
                :seclass
                  (let ((seclass12 (anodes-seclass-or anode1 anode2)))
                    (make-seclass
                      :uses (seclass-uses seclass12)
                      :uses-binding (seclass-uses-binding seclass12)
                      :modifies 'T))
                :code `(,anode1 (PUSH) ,anode2 (THROW)))))

;; compile (UNWIND-PROTECT form1 {form}*)
(defun c-UNWIND-PROTECT ()
  (test-list *form* 2)
  (let ((anode2 (let ((*stackz* (cons 'CLEANUP *stackz*)) (*for-value* nil))
                  (c-form `(PROGN ,@(cddr *form*)) 'NIL))))
    (if (anode-side-effect-free-p anode2)
      ;; cleanup forms are side-effect free ==> ignore them
      (c-form (second *form*))
      ;; cleanup forms have side effects
      (let ((anode1 (let ((*stackz* (cons 'UNWIND-PROTECT *stackz*)))
                      (c-form (second *form*))))
            (label (make-label 'NIL)))
        (make-anode :type 'UNWIND-PROTECT
                    :sub-anodes (list anode1 anode2)
                    :seclass (anodes-seclass-or anode1 anode2)
                    :code `((UNWIND-PROTECT-OPEN ,label)
                            ,anode1
                            ,@(case *for-value*
                                ((NIL) '((VALUES0)))
                                (ONE '((VALUES1)))
                                ((T) '()))
                            (UNWIND-PROTECT-NORMAL-EXIT)
                            ,label
                            ,anode2
                            (UNWIND-PROTECT-CLOSE ,label)))))))

;; compile (PROGV form1 form2 {form}*)
(defun c-PROGV ()
  (test-list *form* 3)
  (let ((anode1 (c-form (second *form*) 'ONE)))
    ;; if form1 is constant=NIL, one can spare the binding:
    (if (and (anode-constantp anode1) (null (anode-constant-value anode1)))
      (c-form `(PROGN ,(third *form*) (PROGN ,@(cdddr *form*))))
      (let* ((stackz2 (cons 1 *stackz*))
             (anode2 (let ((*stackz* stackz2))
                       (c-form (third *form*) 'ONE)))
             (stackz3 (cons 'PROGV *stackz*))
             (anode3 (let ((*stackz* stackz3))
                       (c-form `(PROGN ,@(cdddr *form*)))))
             (seclass3 (anode-seclass anode3))
             (flag t))
        ;; if anode3 does not depend on any side-effects,
        ;; one can spare the binding:
        (when (and (null (seclass-uses seclass3))
                   (null (seclass-uses-binding seclass3)))
          (setf (first stackz2) 0)
          (setf (first stackz3) 0)
          (setq flag nil))
        (make-anode :type 'PROGV
                    :sub-anodes (list anode1 anode2 anode3)
                    :seclass (anodes-seclass-or anode1 anode2 anode3)
                    :code `(,anode1
                            ,@(if flag '((PUSH)))
                            ,anode2
                            ,@(if flag '((PROGV)))
                            ,anode3
                            ,@(if flag
                                `((UNWIND ,stackz3 ,*stackz* ,*for-value*))
                                ;; is expanded to '((UNBIND1) (SKIPSP 1 0))
                                )))))))

;; compile (MULTIPLE-VALUE-PROG1 form1 {form}*)
;; if values are not needed: simple PROGN. else: if {form}*
;; is free from side-effects, only form1, else: push values of form1
;; on the stack and gather them with function VALUES afterwards.
(defun c-MULTIPLE-VALUE-PROG1 ()
  (test-list *form* 2)
  (case *for-value*
    (ALL
     (let* ((stackz1 (cons 'MVCALLP *stackz*))
            (anode1 (let ((*stackz* stackz1))
                      (c-form (second *form*))))
            (anode2 (let ((*stackz* (cons 'MVCALL *stackz*)))
                      (c-form `(PROGN ,@(cddr *form*)) 'NIL))))
       (make-anode :type 'MULTIPLE-VALUE-PROG1
                   :sub-anodes (list anode1 anode2)
                   :seclass (anodes-seclass-or anode1 anode2)
                   :code
                   (if (anode-side-effect-free-p anode2)
                     (prog2 (setf (first stackz1) 0) `(,anode1))
                     `((CONST ,(make-const :horizon ':all
                                           :value #'values
                                           :form '(function values)))
                       (MVCALLP)
                       ,anode1
                       (MV-TO-STACK)
                       ,anode2
                       (MVCALL))))))
    (ONE (c-form `(PROG1 ,@(cdr *form*))))
    ((NIL) (c-form `(PROGN ,@(cdr *form*))))))

;; compile (MULTIPLE-VALUE-CALL form1 {form}*)
(defun c-MULTIPLE-VALUE-CALL ()
  (test-list *form* 2)
  (if (null (cddr *form*))
    ;; (c-form `(FUNCALL ,(second *form*))) ; 0 Arguments for form1
    (c-FUNCTION-CALL (second *form*) '())
    (let* ((anode1 (c-form (second *form*) 'ONE))
           (seclass (f-side-effect (second *form*)))
           #+CLISP-DEBUG (anodelist (list anode1))
           (codelist '()))
      (push anode1 codelist)
      (push '(MVCALLP) codelist)
      (do ((Lr (cddr *form*))
           (i 0 (1+ i)))
          ((null Lr))
        (let* ((formi (pop Lr))
               (anodei
                 (let ((*stackz* (cons (if (zerop i) 'MVCALLP 'MVCALL)
                                       *stackz*)))
                   (c-form formi 'ALL))))
          #+CLISP-DEBUG (push anodei anodelist)
          (seclass-or-f seclass anodei)
          (push anodei codelist)
          (push '(MV-TO-STACK) codelist)))
      (push '(MVCALL) codelist)
      (make-anode :type 'MULTIPLE-VALUE-CALL
                  :sub-anodes (nreverse anodelist)
                  :seclass seclass
                  :code (nreverse codelist)))))

;; compile (MULTIPLE-VALUE-LIST form)
(defun c-MULTIPLE-VALUE-LIST ()
  (test-list *form* 2 2)
  (if *for-value*
    (let ((anode1 (c-form (second *form*) 'ALL)))
      (make-anode :type 'MULTIPLE-VALUE-LIST
                  :sub-anodes (list anode1)
                  :seclass (anodes-seclass-or anode1)
                  :code `(,anode1 (MV-TO-LIST))))
    (c-form (second *form*))))

;; determines, if a setq-argument-list assigns Symbol-Macros.
(defun setqlist-macrop (l)
  (do ((l l (cddr l)))
      ((null l) nil)
    (let ((s (car l)))
      (when (and (symbolp s) (venv-search-macro s)) (return t)))))

(defvar *compiler-unlocked-packages* nil)
(defun set-check-lock (caller symbol)
  (when (and (not (memq (symbol-package symbol) *compiler-unlocked-packages*))
             (symbol-value-lock symbol))
    (c-warn (TEXT "~S: assignment to the internal special variable ~S")
            caller symbol)))

;; compile (SETQ {symbol form}*)
;; execute all assignments one after the other
(defun c-SETQ ()
  (test-list *form* 1)
  (when (evenp (length *form*))
    (c-error (TEXT "Odd number of arguments to SETQ: ~S") *form*))
  (if (null (cdr *form*))
    (c-NIL) ; (SETQ) == (PROGN) == NIL
    (if (setqlist-macrop (cdr *form*))
      (c-form ; (SETF ...) instead of (SETQ ...), macro-expand
        (funcall (macro-function 'SETF) (cons 'SETF (cdr *form*)) (env)))
      (do ((L (cdr *form*) (cddr L))
           #+CLISP-DEBUG (anodelist '())
           (seclass *seclass-foldable*)
           (codelist '()))
          ((null L)
           (make-anode
             :type 'SETQ
             :sub-anodes (nreverse anodelist)
             :seclass seclass
             :code (nreverse codelist)))
        (let* ((symboli (first L))
               (formi (second L))
               (anodei (c-form formi 'ONE)))
          #+CLISP-DEBUG (push anodei anodelist)
          (if (symbolp symboli)
            (progn
              (set-check-lock 'setq symboli)
              (push anodei codelist)
              (seclass-or-f seclass anodei)
              (let ((setteri (c-VARSET symboli anodei
                                       (and *for-value* (null (cddr L))))))
                (push setteri codelist)
                (seclass-or-f seclass setteri)))
            (progn
              (c-error-c (TEXT "Cannot assign to non-symbol ~S.")
                         symboli)
              (push '(VALUES1) codelist))))))))

;; compile (PSETQ {symbol form}*)
;; save all temporary values on the stack, only then assign them
(defun c-PSETQ ()
  (test-list *form* 1)
  (when (evenp (length *form*))
    (c-error (TEXT "Odd number of arguments to PSETQ: ~S") *form*))
  (if (null (cdr *form*))
    (c-NIL) ; (PSETQ) == (PROGN) == NIL
    (if (setqlist-macrop (cdr *form*))
      (c-form ; (PSETF ...) instead of (PSETQ ...), macro-expand
        (funcall (macro-function 'PSETF) (cons 'PSETF (cdr *form*)) (env)))
      (let ((anodelist '())
            (setterlist '()))
        ;; compile forms and assignments:
        (do ((L (cdr *form*)))
            ((null L))
          (let* ((symboli (pop L))
                 (formi (pop L))
                 (anodei (c-form formi 'ONE)))
            (if (symbolp symboli)
              (progn
                (set-check-lock 'psetq symboli)
                (push anodei anodelist)
                (push (c-VARSET symboli anodei nil) setterlist)
                (push 0 *stackz*))
              (c-error-c (TEXT "Cannot assign to non-symbol ~S.")
                         symboli))))
        ;; try to reorganize them in a fashion, that as few  (PUSH)'s and
        ;; (POP)'s as possible are necessary:
        (let ((codelist1 '())
              (codelist2 '())
              ;; build codelist = (nconc codelist1 (nreverse codelist2))
              (seclass *seclass-foldable*)) ; total side-effect-class of codelist
          (do ((anodelistr anodelist (cdr anodelistr))
               (setterlistr setterlist (cdr setterlistr)))
              ((null anodelistr))
            (let ((anode (car anodelistr))
                  (setter (car setterlistr)))
              ;; Normally, we would have to prepend the anode and a (PUSH)
              ;; in front of codelist; a (POP) and the setter would have
              ;; to be appended after codelist. We try to simplify this:
              (cond ((seclasses-commute (anode-seclass setter) seclass)
                     ;; move the setter in front:
                     (push setter codelist1)
                     (push anode codelist1))
                    ((seclasses-commute (anode-seclass anode) seclass)
                     ;; move the anode to the end:
                     (push anode codelist2)
                     (push setter codelist2))
                    (t ; no simplification possible
                     (push '(PUSH) codelist1)
                     (push anode codelist1)
                     (push '(POP) codelist2)
                     (push setter codelist2)
                     (setf (car *stackz*) 1))) ; need a variable in the stack
              (setq seclass
                (seclass-or seclass
                  (seclass-or (anode-seclass anode) (anode-seclass setter))))
              (setf *stackz* (cdr *stackz*))))
          ;; now *stackz* is again on the old level.
          (when *for-value* (push '(NIL) codelist2))
          (make-anode
            :type 'PSETQ
            :sub-anodes (nreverse anodelist)
            :seclass seclass
            :code (nconc codelist1 (nreverse codelist2))))))))

;; compile (MULTIPLE-VALUE-SETQ ({symbol}*) form)
;; all desired values on the the stack, then pop them ony by one and
;; assign.
(defun c-MULTIPLE-VALUE-SETQ ()
  (test-list *form* 3 3)
  (test-list (second *form*) 0)
  (if (dolist (s (second *form*) nil)
        (when (and (symbolp s) (venv-search-macro s)) (return t)))
    (c-form `(SYSTEM::MULTIPLE-VALUE-SETF ,@(cdr *form*)))
    (let* ((n (length (second *form*)))
           (anode1 (c-form (third *form*) 'ALL))
           (*stackz* *stackz*))
      (if (zerop n)
        (make-anode :type 'MULTIPLE-VALUE-SETQ
                    :sub-anodes (list anode1)
                    :seclass (anodes-seclass-or anode1)
                    :code `(,anode1
                            ,@(if (eq *for-value* 'ALL) '((VALUES1)) '())))
        ;; Here we generate the assignments in reverse order, using (POP)
        ;; instructions, because they are faster than the (LOAD i) instructions
        ;; that would be needed if we did the assignments in left-to-right
        ;; order. However, ANSI CL says the assignments are (conceptually)
        ;; performed in left-to-right order. The only case when this might
        ;; matter is when the symbol list contains duplicates. In this case,
        ;; we have to eliminate some assignments.
        (do ((L (second *form*) (cdr L))
             #+CLISP-DEBUG (anodelist (list anode1))
             (seclass (anode-seclass anode1))
             (codelist '()))
            ((null L)
             (if (= n 1)
               (setq codelist (cdr codelist)) ; discard last (POP)
               (setq codelist (cons `(NV-TO-STACK ,n) codelist)))
             (make-anode
               :type 'MULTIPLE-VALUE-SETQ
               :sub-anodes (nreverse anodelist)
               :seclass seclass
               :code (cons anode1 codelist)))
          (let ((symbol (car L)))
            (if (symbolp symbol)
              (unless (memq symbol (cdr L))
                (let ((setter (c-VARSET symbol
                                (make-anode :type 'NOP
                                            :sub-anodes '()
                                            :seclass *seclass-foldable*
                                            :code '())
                                (and *for-value* (null codelist)))))
                  (set-check-lock 'multiple-value-setq symbol)
                  (push setter codelist)
                  (seclass-or-f seclass setter)))
              (c-error-c (TEXT "Cannot assign to non-symbol ~S.")
                         symbol)))
          (push '(POP) codelist)
          (push 1 *stackz*))))))

;; returns the code for the parallel binding of variables.
;; (car *stackz*) should be = 0, (cdr *stackz*) is poss. extended.
(defun c-parallel-bind-movable-var-anode (varlist anodelist stackzlist
                                          &optional (other-anodes '()))
  ;; Variable may be bound foremost at the end, if it is SPECIAL
  ;; and sequencing Anodes can depend on its value.
  (let ((bind-afterwards nil))
    (append
      (maplap
        #'(lambda (varlistr anodelistr stackzlistr)
            (let ((var (car varlistr))
                  (anode (car anodelistr)))
              (if (and (var-specialp var)
                       (let ((symbol (var-name var)))
                         (some
                           #'(lambda (other-anode)
                               ;; does the value of other-anode possibly depend
                               ;; on the value of var?
                               (let* ((se (anode-seclass other-anode))
                                      (uses (seclass-uses se))
                                      (u-bi (seclass-uses-binding se)))
                                 (or (eq uses 'T) (memq symbol uses)
                                     (eq u-bi 'T) (memq symbol u-bi))))
                           (cdr anodelistr))))
                (let* ((stackz (car stackzlistr))
                       (dummyvar ; auxiliary variable in Stack
                         (make-var :name (gensym) :specialp nil
                                   :closurep nil :stackz stackz)))
                  (push (list dummyvar var (cdr *stackz*)) bind-afterwards)
                  ;; room for 1 more closing binding:
                  (push (car stackz) (cdr *stackz*))
                  ;; memorize room for auxiliary variable in stack:
                  (setf (car stackz) 1)
                  (c-bind-movable-var-anode dummyvar anode))
                (c-bind-movable-var-anode var anode))))
        varlist (append anodelist other-anodes) stackzlist)
      other-anodes
      (mapcap
        #'(lambda (bind)
            (let ((dummyvar (first bind)) ; auxiliary variable in Stack
                  (var (second bind)) ; SPECIAL-Variable
                  ;; stack-state before the construction of closing-binding:
                  (stackz (third bind)))
              `((GET ,dummyvar ,*venvc* ,stackz)
                ,@(c-bind-movable-var var))))
        (nreverse bind-afterwards)))))

;; compile (LET/LET* ({var|(var value)}*) {declaration}* {form}*)
(defun c-LET/LET* (*-flag)
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (multiple-value-bind (body-rest declarations) (parse-body (cddr *form*))
    (let ((oldstackz *stackz*)
          (*stackz* *stackz*)
          (*denv* *denv*)
          (*venv* *venv*)
          (*venvc* *venvc*))
      (multiple-value-bind (*specials* *ignores* *ignorables* *readonlys* other-decls)
          (process-declarations declarations)
        ;; syntax-test of the parameter-list:
        (multiple-value-bind (symbols initforms)
            (analyze-letlist (second *form*))
          (push 0 *stackz*) (push nil *venvc*) ; room for Closure-Dummyvar
          (let ((closuredummy-stackz *stackz*)
                (closuredummy-venvc *venvc*))
            (multiple-value-bind (varlist anodelist stackzlist)
                (process-movable-var-list symbols initforms *-flag)
              (unless *-flag (push 0 *stackz*)) ; room for closing-bindings
              (push-specials)
              (push-*denv* other-decls)
              (let ((body-anode (c-form `(PROGN ,@body-rest)))) ; compile Body
                ;; check the variables:
                (let* ((closurevars
                         (checking-movable-var-list varlist anodelist))
                       (codelist
                         `(,@(c-make-closure closurevars closuredummy-venvc
                                             closuredummy-stackz)
                           ,@(if *-flag
                               ;; sequential binding of variables
                               (mapcap #'c-bind-movable-var-anode
                                       varlist anodelist)
                               ;; parallel binding of variables
                               (c-parallel-bind-movable-var-anode
                                varlist anodelist stackzlist))
                           ,body-anode
                           (UNWIND ,*stackz* ,oldstackz ,*for-value*)))
                       (anode
                         (make-anode
                           :type (if *-flag 'LET* 'LET)
                           :sub-anodes `(,@anodelist ,body-anode)
                           :seclass (seclass-without
                                      (anodelist-seclass-or
                                       `(,@anodelist ,body-anode))
                                      varlist)
                           :stackz oldstackz
                           :code codelist)))
                  (closuredummy-add-stack-slot
                   closurevars closuredummy-stackz closuredummy-venvc)
                  (optimize-var-list varlist)
                  anode)))))))))

;; compile (LOCALLY {declaration}* {form}*)
(defun c-LOCALLY (&optional (c #'c-form)) ; cf. c-LET/LET*
  (test-list *form* 1)
  (multiple-value-bind (body-rest declarations) (parse-body (cdr *form*))
    (let ((*venv* *venv*))
      (multiple-value-bind (*specials* ignores ignorables readonlys other-decls)
          (process-declarations declarations)
        (declare (ignore ignores ignorables readonlys))
        (push-specials)
        (push-*denv* other-decls)
        (funcall c `(PROGN ,@body-rest))))))

;; compile (MULTIPLE-VALUE-BIND ({var}*) form1 {declaration}* {form}*)
(defun c-MULTIPLE-VALUE-BIND ()
  (test-list *form* 3)
  (test-list (second *form*) 0)
  (let ((symbols (second *form*)))
    (dolist (sym symbols)
      (unless (symbolp sym)
        (c-error (TEXT "Only symbols may be used as variables, not ~S")
                 sym)))
    (if (= (length symbols) 1)
      (c-form `(LET ((,(first symbols) ,(third *form*))) ,@(cdddr *form*)))
      (multiple-value-bind (body-rest declarations) (parse-body (cdddr *form*))
        (let ((oldstackz *stackz*)
              (*stackz* *stackz*)
              (*denv* *denv*)
              (*venv* *venv*)
              (*venvc* *venvc*))
          (multiple-value-bind (*specials* *ignores* *ignorables* *readonlys* other-decls)
              (process-declarations declarations)
            (if (null symbols) ; empty variable-list -> bind nothing
              (let ((anode1 (c-form (third *form*) 'NIL)))
                (push-specials)
                (push-*denv* other-decls)
                (let ((anode2 (c-form `(PROGN ,@(cdddr *form*)))))
                  (make-anode :type 'MULTIPLE-VALUE-BIND
                    :sub-anodes (list anode1 anode2)
                    :seclass (anodes-seclass-or anode1 anode2)
                    :code `(,anode1 ,anode2))))
              (let ((anode1 (c-form (third *form*) 'ALL)))
                (push nil *venvc*) ; visibility of Closure-Dummyvar
                (multiple-value-bind (varlist stackvarlist)
                    (process-fixed-var-list symbols)
                  (push 0 *stackz*) ; room for Closure-Dummyvar
                  (let* ((closuredummy-stackz *stackz*)
                         (closuredummy-venvc *venvc*)
                         (stackzlist
                           (do* ((varlistr varlist (cdr varlistr))
                                 (L '()))
                                ((null varlistr) (nreverse L))
                             (let ((var (car varlistr)))
                               (push-*venv* var)
                               (push *stackz* L) (bind-fixed-var-2 var)))))
                    (push-specials)
                    (push-*denv* other-decls)
                    (let* ((body-anode ; compile Body
                             (c-form `(PROGN ,@body-rest)))
                           ; check the variables:
                           (closurevars (checking-fixed-var-list varlist))
                           (codelist ; generate Code
                             `(,anode1
                               (NV-TO-STACK ,(length symbols))
                               ,@(c-make-closure closurevars closuredummy-venvc
                                                 closuredummy-stackz)
                               ,@ ; bind special- or Closure-variables:
                                 (do ((stackvarlistr stackvarlist
                                                     (cdr stackvarlistr))
                                      (stackzlistr stackzlist (cdr stackzlistr))
                                      (varlistr varlist (cdr varlistr))
                                      (L '()))
                                     ((null varlistr) (nreverse L))
                                   (setq L (revappend
                                             (c-bind-fixed-var
                                               (car varlistr)
                                               (car stackvarlistr)
                                               (car stackzlistr))
                                             L)))
                               ,body-anode
                               (UNWIND ,*stackz* ,oldstackz ,*for-value*)))
                           (anode
                             (make-anode
                               :type 'MULTIPLE-VALUE-BIND
                               :sub-anodes (list anode1 body-anode)
                               :seclass (seclass-without
                                          (anodes-seclass-or anode1 body-anode)
                                          varlist)
                               :stackz oldstackz
                               :code codelist)))
                      (closuredummy-add-stack-slot
                        closurevars closuredummy-stackz closuredummy-venvc)
                      (optimize-var-list varlist)
                      anode)))))))))))

;; compile (COMPILER-LET ({var|(var value)}*) {form}*)
(defun c-COMPILER-LET (&optional (c #'c-form))
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (do ((L (second *form*) (cdr L))
       (varlist '())
       (valueslist '()))
      ((null L)
       (progv (nreverse varlist) (nreverse valueslist)
         (funcall c `(PROGN ,@(cddr *form*)) )))
    (cond ((symbolp (car L)) (push (car L) varlist) (push nil valueslist))
          ((and (consp (car L)) (symbolp (caar L))
                (or (null (cdar L))
                    (and (consp (cdar L)) (null (cddar L)))))
           (push (caar L) varlist) (push (eval (cadar L)) valueslist))
          (t (c-error-c (TEXT "Illegal syntax in COMPILER-LET: ~S")
                        (car L))))))

(macrolet ((check-blockname (name)
             `(unless (symbolp ,name)
                (c-error-c (TEXT "Block name must be a symbol, not ~S")
                           ,name)
                (setq ,name NIL)))) ; Default-Blockname

;; compile (BLOCK name {form}*)
(defun c-BLOCK ()
  (test-list *form* 2)
  (let ((name (second *form*)))
    (check-blockname name)
    (let* ((*stackz* (cons 'BLOCK *stackz*)) ; Block-Frame
           (label (make-label *for-value*))
           (block (make-block :fnode *func* :label label
                    :consvar (make-var :name (gensym) :specialp nil
                                       :closurep nil :stackz *stackz*)
                    :stackz *stackz* :used-far nil :for-value *for-value*))
           (*benv* (cons (cons name block) *benv*)) ; activate Block
           (anode (c-form `(PROGN ,@(cddr *form*)))))
      (if (block-used-far block)
        (make-anode :type 'BLOCK
                    :sub-anodes (list anode)
                    :seclass (anodes-seclass-or anode)
                    :code `((BLOCK-OPEN
                             ;; set (gensym) to nil
                             ,(new-const (and (symbol-package name) name))
                             ,label)
                            ,anode
                            (BLOCK-CLOSE)
                            ,label))
        (progn
          (setf (first *stackz*) 0) ; need no Blockframe
          (make-anode :type 'BLOCK
                      :sub-anodes (list anode)
                      :seclass (anodes-seclass-or anode)
                      :code `(,anode ,label)))))))

;; compile (RETURN-FROM name [form])
(defun c-RETURN-FROM ()
  (test-list *form* 2 3)
  (let ((name (second *form*)))
    (check-blockname name)
    (let ((a (benv-search name)))
      (cond ((null a) ; this Blockname is invisible
             (c-error (TEXT "RETURN-FROM block ~S is impossible from here.")
                      name))
            ((block-p a) ; visible in *benv* without %benv%
             (let ((anode (c-form (third *form*) (block-for-value a))))
               (if (and (eq (block-fnode a) *func*)
                        (may-UNWIND *stackz* (cdr (block-stackz a))))
                 ;; same functions
                 (make-anode
                   :type 'RETURN-FROM
                   :sub-anodes (list anode)
                   :seclass *seclass-dirty*
                   :code `(,anode
                           (UNWIND ,*stackz* ,(cdr (block-stackz a))
                                   ,(block-for-value a))
                           (JMP ,(block-label a))))
                 ;; different functions or unknown frames onto the stack
                 (progn
                   (unless *no-code*
                     (note-far-used-block a))
                   (make-anode
                     :type 'RETURN-FROM
                     :sub-anodes (list anode)
                     :seclass *seclass-dirty*
                     :code `(,anode
                             ,@(if (not (block-for-value a)) '((VALUES0)))
                             (RETURN-FROM ,a
                              ,@(if (eq (block-fnode a) *func*)
                                    `(,*stackz*) '()))))))))
            ((consp a) ; visible in %benv%
             (let ((anode (c-form (third *form*) 'ALL)))
               (make-anode
                 :type 'RETURN-FROM
                 :sub-anodes (list anode)
                 :seclass *seclass-dirty*
                 :code `(,anode (RETURN-FROM ,(new-const a))))))
            (t (compiler-error 'c-RETURN-FROM a))))))

) ; macrolet

;; compile (TAGBODY {tag|form}*)
(defun c-TAGBODY ()
  (test-list *form* 1)
  (multiple-value-bind (taglist labellist)
    (do ((L (cdr *form*) (cdr L))
         (taglist '())
         (labellist '()))
        ((null L) (values (nreverse taglist) (nreverse labellist)))
      (let ((item (car L)))
        (if (atom item)
          (if (or (symbolp item) (numberp item))
            ;; Symbol NIL is permitted, because in ANSI CL it is not
            ;; ambiguous anymore.
            ;; Other numbers are permitted, so that - just as 3.3.2 -
            ;; 3.3 is a admissible jump-destination.
            (progn
              (push item taglist)
              (push (make-label 'NIL) labellist))
            (c-error-c
             (TEXT "Only numbers and symbols are valid tags, not ~S")
             item)))))
    (let* ((*stackz* (cons 0 *stackz*)) ; poss. TAGBODY-Frame
           (tagbody (make-tagbody :fnode *func* :labellist labellist
                      :consvar (make-var :name (gensym) :specialp nil
                                         :closurep nil :stackz *stackz*)
                      :stackz *stackz* :used-far nil))
           (*genv* (cons (cons (apply #'vector taglist) tagbody) *genv*))
           ;; activate Tagbody
           (codelist '())
           #+CLISP-DEBUG (anodelist '())
           (seclass *seclass-foldable*))
      ;; compile interior of Tagbody:
      (do ((formlistr (cdr *form*) (cdr formlistr))
           (taglistr taglist)
           (labellistr labellist))
          ((null formlistr)
           #+CLISP-DEBUG (setq anodelist (nreverse anodelist))
           (setq codelist (nreverse codelist)))
        (let ((formi (car formlistr)))
          (if (atom formi)
            (when (and (consp taglistr) (eql formi (car taglistr)))
              ;; retrieve Tag
              (pop taglistr) (push (pop labellistr) codelist))
            (let ((anodei (c-form formi 'NIL)))
              #+CLISP-DEBUG (push anodei anodelist)
              (seclass-or-f seclass anodei)
              (push anodei codelist)))))
      (if (tagbody-used-far tagbody)
        (let ((used-tags (make-array (length taglist) :fill-pointer 0)))
          ;; Collect the used tags and assign indices.
          (dolist (tagbody+tag (tagbody-used-far tagbody))
            (let* ((tag (cdr tagbody+tag))
                   (index (or (position tag used-tags :test #'eql)
                              (vector-push tag used-tags)
                              (compiler-error 'c-TAGBODY tag))))
              (setf (cdr tagbody+tag) index)))
          (setf (tagbody-used-far tagbody) nil)
          (let* ((l (length used-tags))
                 (used-label-list
                   (do ((i 0 (1+ i))
                        (l1 '()))
                       ((= i l) (nreverse l1))
                     (push
                       (elt labellist (position (aref used-tags i) taglist
                                                :test #'eql))
                       l1))))
            (setf (first *stackz*) `(TAGBODY ,l))
            (setq codelist
              `((TAGBODY-OPEN
                  ,(new-const (map 'simple-vector
                                   #'(lambda (tag)
                                       (if (numberp tag)
                                         tag
                                         ;; GENSYM --> NIL so that
                                         ;; *.fas will not contain GENSYMs
                                         (and (symbol-package tag) tag)))
                                   used-tags))
                  ,@used-label-list)
                ,@codelist
                (TAGBODY-CLOSE-NIL)))))
        (when *for-value* (setq codelist `(,@codelist (NIL)))))
      (make-anode :type 'TAGBODY
                  :sub-anodes anodelist
                  :seclass seclass
                  :code codelist))))

;; compile (GO tag)
(defun c-GO ()
  (test-list *form* 2 2)
  (let ((tag (second *form*)))
    (unless (or (symbolp tag) (numberp tag))
      (c-error (TEXT "Tag must be a symbol or a number, not ~S") tag))
    (multiple-value-bind (a b) (genv-search tag)
      (cond ((null a) ; this Tag is invisible
             (c-error (TEXT "GO to tag ~S is impossible from here.") tag))
            ((tagbody-p a) ; visible in *genv* without %genv%
             (if (and (eq (tagbody-fnode a) *func*)
                      (may-UNWIND *stackz* (tagbody-stackz a)))
               ;; same functions
               (make-anode
                 :type 'GO
                 :sub-anodes '()
                 :seclass *seclass-dirty*
                 :code `((UNWIND ,*stackz* ,(tagbody-stackz a) nil)
                         (JMP ,(nth b (tagbody-labellist a)))))
               ;; different functions or unknown frames onto the stack
               (let ((tagbody+tag (cons a tag)))
                 (unless *no-code*
                   (note-far-used-tagbody tagbody+tag))
                 (make-anode
                   :type 'GO
                   :sub-anodes '()
                   :seclass *seclass-dirty*
                   :code `((VALUES0)
                           (GO ,a ,tagbody+tag
                            ,@(if (eq (tagbody-fnode a) *func*)
                                  `(,*stackz*) '())))))))
            ((consp a) ; visible in %genv%
             (make-anode
               :type 'GO
               :sub-anodes '()
               :seclass *seclass-dirty*
               :code `((GO ,(new-const a) ,b))))
            (t (compiler-error 'c-GO a))))))

;; compile (FUNCTION funname)
(defun c-FUNCTION ()
  (test-list *form* 2 3)
  (let* ((longp (cddr *form*)) ; flag, if explicit form (FUNCTION name funname)
         (name (second *form*)))
    (if (and (not longp) (function-name-p name))
      (multiple-value-bind (a m f1 f2 f3 f4) (fenv-search name)
        (if (null a)
          (progn
            (note-function-used name 0 nil)
            (make-anode
              :type 'FUNCTION
              :sub-anodes '()
              :seclass *seclass-read*
              :code (if (and (subr-info name) (not (declared-notinline name)))
                      `((CONST ,(make-const :horizon ':all
                                            :value (symbol-function name)
                                            :form `(FUNCTION ,name))))
                      `((CONST ,(make-funname-const name))
                        (SYMBOL-FUNCTION NIL)))))
          (case f1
            (GLOBAL ; found in %fenv%
             (make-anode
               :type 'FUNCTION
               :sub-anodes '()
               :seclass *seclass-read*
               :code `((CONST ,(new-const f2))
                       (PUSH)
                       (CONST ,(new-const f3))
                       (SVREF NIL)
                       ,@(if f4
                           `((PUSH) ,(CALLS-code-fun FUNCTION-MACRO-FUNCTION))
                           '()))))
            (LOCAL ; found in *fenv* without %fenv%
             (if (const-p f2)
               (make-anode
                 :type 'FUNCTION
                 :sub-anodes '()
                 :seclass *seclass-pure*
                 :code `((FCONST ,(const-value-safe f2))))
               (c-VAR (var-name f2))))
            (t (if (and (null f1) m)
                 (c-error (TEXT "~S is not a function. It is a locally defined macro.")
                          name)
                 (compiler-error 'c-FUNCTION name))))))
      (let ((funname (car (last *form*))))
        (if (lambda-form-p funname)
          (let* ((*no-code* (or *no-code* (null *for-value*)))
                 (fnode
                   (c-lambdabody
                     (if (and longp (function-name-p name))
                       name ; specified function-name
                       (symbol-suffix (fnode-name *func*)
                                      (incf *anonymous-count*)))
                     (cdr funname))))
            (unless *no-code* (propagate-far-used fnode))
            (c-fnode-function fnode))
          (c-error (TEXT "Only symbols and lambda expressions are function names, not ~S")
                   funname))))))

;; compile (%GENERIC-FUNCTION-LAMBDA . lambdabody)
(defun c-%GENERIC-FUNCTION-LAMBDA ()
  (test-list *form* 1)
  (let* ((*no-code* (or *no-code* (null *for-value*)))
         (fnode
           (c-lambdabody
             (symbol-suffix (fnode-name *func*) (incf *anonymous-count*))
             (cdr *form*)
             nil
             t))) ; gf-p = T, build Code for generic function
    (unless *no-code* (propagate-far-used fnode))
    (c-fnode-function fnode)))

;; compile (%OPTIMIZE-FUNCTION-LAMBDA reqoptimflags . lambdabody)
;; reqoptimflags is a list of flags that states, which Required-Parameter
;; of the lambdabody can be optimized away. For each Required-Parameter:
;; NIL: normal,
;; T: Can be optimized away, then it is turned into GONE.
;; NILs at the end of the list can be omitted.
;; The output contains the list of the omitted parameters
;; in addition to the function.
(defmacro %OPTIMIZE-FUNCTION-LAMBDA (reqoptimflags &rest lambdabody)
  (declare (ignore reqoptimflags))
  ;; without compiler: do not optimize
  `(CONS (FUNCTION (LAMBDA ,@lambdabody)) NIL))
(defun c-%OPTIMIZE-FUNCTION-LAMBDA ()
  (test-list *form* 2)
  (let* ((*no-code* (or *no-code* (null *for-value*)))
         (reqoptimflags (copy-list (second *form*)))
         (fnode
           (c-lambdabody
             (symbol-suffix (fnode-name *func*) (incf *anonymous-count*))
             (cddr *form*)
             nil nil reqoptimflags)))
    (unless *no-code* (propagate-far-used fnode))
    (let* ((anode1 (c-fnode-function fnode))
           (resultflags (mapcar #'(lambda (x) (eq x 'GONE)) reqoptimflags))
           (anode2 (let ((*stackz* (cons 1 *stackz*))
                         (*form* `(QUOTE ,resultflags)))
                     (c-QUOTE))))
      (make-anode :type '%OPTIMIZE-FUNCTION-LAMBDA
                  :sub-anodes (list anode1 anode2)
                  :seclass (anodes-seclass-or anode1 anode2)
                  :code `(,anode1 (PUSH) ,anode2 (CONS))))))

;; skip (ignore) all declarations in the beginning of BODY
(defun skip-declarations (body)
  (do ((bo body (cdr bo)))
      ((not (and (consp bo) (consp (car bo)) (eq (caar bo) 'declare)))
       bo)))

(macrolet ((err-syntax (specform fdef)
             `(c-error-c
               (TEXT "Illegal function definition syntax in ~S: ~S")
               ,specform ,fdef))
           (add-fenv (namelist fenvconslist)
             `(do ((namelistr ,namelist (cdr namelistr))
                   (fenvconslistr ,fenvconslist (cdr fenvconslistr))
                   (L nil))
                  ((null namelistr)
                   (apply #'vector (nreverse (cons *fenv* L))))
                (push (car namelistr) L)
                (push (car fenvconslistr) L)))
           (get-anode (type)
             `(let* ((closurevars (checking-movable-var-list varlist anodelist))
                     (anode
                       (make-anode
                         :type ',type
                         :sub-anodes `(,@anodelist ,body-anode)
                         :seclass (seclass-without
                                    (anodelist-seclass-or
                                      `(,@anodelist ,body-anode))
                                    varlist)
                         :code `(,@(c-make-closure closurevars closuredummy-venvc
                                                   closuredummy-stackz)
                                 ,@(mapcap #'c-bind-movable-var-anode
                                           varlist anodelist)
                                 ,body-anode
                                 (UNWIND ,*stackz* ,oldstackz ,*for-value*)))))
               (closuredummy-add-stack-slot closurevars
                                            closuredummy-stackz closuredummy-venvc)
               (optimize-var-list varlist)
               anode)))

;; compile (FLET ({fundef}*) {form}*)
(defun c-FLET ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (multiple-value-bind (namelist fnodelist)
      (do ((fdefsr (second *form*) (cdr fdefsr))
           (L1 '())
           (L2 '()))
          ((null fdefsr) (values (nreverse L1) (nreverse L2)))
        (let ((fdef (car fdefsr)))
          (if (and (consp fdef) (function-name-p (car fdef))
                   (consp (cdr fdef)))
            (let* ((name (car fdef))
                   (fnode (c-lambdabody
                            (symbol-suffix (fnode-name *func*) name)
                            (add-implicit-block name (cdr fdef)))))
              (push name L1)
              (push fnode L2))
            (err-syntax 'FLET fdef))))
    ;; namelist = list of names, fnodelist = list of fnodes of the functions
    (let ((oldstackz *stackz*)
          (*stackz* *stackz*)
          (*venvc* *venvc*)
          (*venv* *venv*))
      (push 0 *stackz*) (push nil *venvc*) ; room for Closure-Dummyvar
      (let ((closuredummy-stackz *stackz*)
            (closuredummy-venvc *venvc*))
        (multiple-value-bind (vfnodelist varlist anodelist *fenv*)
            (do ((namelistr namelist (cdr namelistr))
                 (fnodelistr fnodelist (cdr fnodelistr))
                 (vfnodelist '())
                 (varlist '())
                 (anodelist '())
                 (fenv '()))
                ((null namelistr)
                 (values (nreverse vfnodelist) (nreverse varlist)
                         (nreverse anodelist)
                         (apply #'vector (nreverse (cons *fenv* fenv)))))
              (push (car namelistr) fenv)
              (let ((fnode (car fnodelistr)))
                (if (zerop (fnode-keyword-offset fnode))
                  ;; function-definition is autonomous
                  (push (cons (list fnode) (new-const fnode)) fenv)
                  (progn
                    (push fnode vfnodelist)
                    (push (c-fnode-function fnode) anodelist)
                    (push 1 *stackz*)
                    (let ((var (make-var :name (gensym) :specialp nil
                                 :constantp nil
                                 :usedp t :for-value-usedp t :really-usedp nil
                                 :closurep nil ; later poss. set to T
                                 :stackz *stackz* :venvc *venvc*
                                 :fnode *func*)))
                      (push (cons (list fnode) var) fenv)
                      (push var varlist))))))
          (apply #'push-*venv* varlist) ; activate auxiliary variables
          (let ((body-anode ; compile remaining forms
                  (c-form `(PROGN ,@(skip-declarations (cddr *form*))))))
            (unless *no-code*
              (mapc #'(lambda (var fnode)
                        (when (var-really-usedp var)
                          (propagate-far-used fnode)))
                    varlist vfnodelist))
            (get-anode FLET)))))))

;; compile (LABELS ({fundef}*) {form}*)
(defun c-LABELS ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (let ((oldstackz *stackz*)
        (*stackz* *stackz*)
        (*venvc* *venvc*)
        (*venv* *venv*))
    (push 0 *stackz*) (push nil *venvc*) ; room for Closure-Dummyvar
    (let ((closuredummy-stackz *stackz*)
          (closuredummy-venvc *venvc*))
      (multiple-value-bind
            (namelist varlist lambdanamelist lambdabodylist fenvconslist)
          (do ((fdefsr (second *form*) (cdr fdefsr))
               (L1 '())
               (L2 '())
               (L3 '())
               (L4 '())
               (L5 '()))
              ((null fdefsr)
               (values (nreverse L1) (nreverse L2) (nreverse L3)
                       (nreverse L4) (nreverse L5)))
            (let ((fdef (car fdefsr)))
              (if (and (consp fdef) (function-name-p (car fdef))
                       (consp (cdr fdef)))
                (let ((name (car fdef)))
                  (push name L1)
                  (push 1 *stackz*)
                  (push (make-var :name (gensym) :specialp nil
                                  :constantp nil
                                  :usedp t :for-value-usedp t :really-usedp nil
                                  :closurep nil ; later poss. set to T
                                  :stackz *stackz* :venvc *venvc*
                                  :fnode *func*)
                        L2)
                  (push (symbol-suffix (fnode-name *func*) name) L3)
                  (push (cdr fdef) L4)
                  (push
                    (cons
                      ;; fdescr, consisting of:
                      (cons nil ; room for the FNODE
                        (cons 'LABELS
                          (multiple-value-list ; values from c-analyze-lambdalist
                            (c-analyze-lambdalist
                             (if *defun-accept-specialized-lambda-list*
                               (sys::specialized-lambda-list-to-ordinary
                                 (cadr fdef) 'compile)
                               (cadr fdef))))))
                      ;; Variable
                      (car L2))
                    L5))
                (err-syntax 'LABELS fdef))))
        ;; namelist = list of names, varlist = list of variables,
        ;; lambdanamelist = list of Dummy-names of the functions,
        ;; lambdabodylist = list of Lambda-bodies of the functions,
        ;; fenvconslist = list of Conses (fdescr . var) for *fenv*
        ;; (fdescr still without fnode, which is inserted later).
        (let ((*fenv* ; activate function-name
                (add-fenv namelist fenvconslist)))
          (apply #'push-*venv* varlist) ; activate auxiliary variables
          (let* ((fnodelist ; compile functions
                   (mapcar #'(lambda (name lambdaname lambdabody fenvcons)
                               (c-lambdabody
                                 lambdaname
                                 (add-implicit-block name lambdabody)
                                 fenvcons))
                           namelist lambdanamelist lambdabodylist
                           fenvconslist))
                 (anodelist
                   (mapcar #'(lambda (fnode var)
                               (c-fnode-function fnode (cdr (var-stackz var))))
                           fnodelist varlist))
                 (body-anode ; compile remaining forms
                   (c-form `(PROGN ,@(skip-declarations (cddr *form*))))))
            ;; the variables, for which the function was autonomous, are
            ;; additionally declared as constants:
            (do ((varlistr varlist (cdr varlistr))
                 (fnodelistr fnodelist (cdr fnodelistr)))
                ((null varlistr))
              (let ((var (car varlistr))
                    (fnode (car fnodelistr)))
                (when (zerop (fnode-keyword-offset fnode))
                  ;; function-definition is autonomous
                  (setf (var-constantp var) t)
                  (setf (var-constant var) (new-const fnode)))))
            ;; Determine the functions which are really used.
            ;; Functions with closure variables can pull in other functions.
            (unless *no-code*
              (let ((last-count 0))
                (loop
                  ;; Iterate as long as at least one function has been
                  ;; pulled in.
                  (when (eql last-count
                             (setq last-count (count-if #'var-really-usedp
                                                        varlist)))
                    (return))
                  (do ((varlistr varlist (cdr varlistr))
                       (fnodelistr fnodelist (cdr fnodelistr)))
                      ((null varlistr))
                    (let ((var (car varlistr))
                          (fnode (car fnodelistr)))
                      (unless (zerop (fnode-keyword-offset fnode))
                        ;; function with closure variables
                        (when (var-really-usedp var)
                          (propagate-far-used fnode))))))))
            (get-anode LABELS)))))))

;; compile
;; (SYS::FUNCTION-MACRO-LET ({(name fun-lambdabody macro-lambdabody)}) {form})
(defun c-FUNCTION-MACRO-LET ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (multiple-value-bind (namelist fnodelist macrolist)
      (do ((funmacdefsr (second *form*) (cdr funmacdefsr))
           (L1 '())
           (L2 '())
           (L3 '()))
          ((null funmacdefsr)
           (values (nreverse L1) (nreverse L2) (nreverse L3)))
        (let ((funmacdef (car funmacdefsr)))
          (if (and (consp funmacdef)
                   (symbolp (car funmacdef))
                   (consp (cdr funmacdef)) (consp (second funmacdef))
                   (consp (cddr funmacdef)) (consp (third funmacdef))
                   (null (cdddr funmacdef)))
            (let* ((name (car funmacdef))
                   (fnode (c-lambdabody
                            (symbol-suffix (fnode-name *func*) name)
                            (second funmacdef)))
                   (macro (make-funmacro-expander name (third funmacdef))))
              (push name L1)
              (push fnode L2)
              (push macro L3))
            (err-syntax 'SYSTEM::FUNCTION-MACRO-LET funmacdef))))
    ;; namelist  = list of names,
    ;; fnodelist = list of fnodes of the functions,
    ;; macrolist = list of Macro-Objects of the functions.
    (let ((oldstackz *stackz*)
          (*stackz* *stackz*)
          (*venvc* *venvc*)
          (*venv* *venv*))
      (push 0 *stackz*) (push nil *venvc*) ; room for Closure-Dummyvar
      (let ((closuredummy-stackz *stackz*)
            (closuredummy-venvc *venvc*))
        (multiple-value-bind (vfnodelist varlist anodelist *fenv*)
            (do ((namelistr namelist (cdr namelistr))
                 (fnodelistr fnodelist (cdr fnodelistr))
                 (macrolistr macrolist (cdr macrolistr))
                 (vfnodelist '())
                 (varlist '())
                 (anodelist '())
                 (fenv '()))
                ((null namelistr)
                 (values (nreverse vfnodelist) (nreverse varlist)
                         (nreverse anodelist)
                         (apply #'vector (nreverse (cons *fenv* fenv)))))
              (push (car namelistr) fenv)
              (let ((fnode (car fnodelistr))
                    (macro (car macrolistr)))
                (if (zerop (fnode-keyword-offset fnode))
                  ;; function-definition is autonomous
                  (push (list* macro (list fnode) (new-const fnode)) fenv)
                  (progn
                    (push fnode vfnodelist)
                    (push (c-fnode-function fnode) anodelist)
                    (push 1 *stackz*)
                    (let ((var (make-var :name (gensym) :specialp nil
                                 :constantp nil
                                 :usedp t :for-value-usedp t :really-usedp nil
                                 :closurep nil ; later poss. set to T
                                 :stackz *stackz* :venvc *venvc*
                                 :fnode *func*)))
                      (push (cons macro (cons (list fnode) var)) fenv)
                      (push var varlist))))))
          (apply #'push-*venv* varlist) ; activate auxiliary variables
          (let ((body-anode ; compile remaining forms
                  (c-form `(PROGN ,@(cddr *form*)))))
            (unless *no-code*
              (mapc #'(lambda (var fnode)
                        (when (var-really-usedp var)
                          (propagate-far-used fnode)))
                    varlist vfnodelist))
            (get-anode FUNCTION-MACRO-LET)))))))

;; compile (CLOS:GENERIC-FLET ({genfundefs}*) {form}*)
(defun c-GENERIC-FLET ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (multiple-value-bind (namelist signlist formlist)
      (do ((fdefsr (second *form*) (cdr fdefsr))
           (L1 '())
           (L2 '())
           (L3 '()))
          ((null fdefsr) (values (nreverse L1) (nreverse L2) (nreverse L3)))
        (let ((fdef (car fdefsr)))
          (if (and (consp fdef) (function-name-p (car fdef))
                   (consp (cdr fdef)))
            (let ((name (first fdef)))
              (push name L1)
              (push (clos::defgeneric-lambdalist-callinfo 'clos:generic-flet
                      *form* name (second fdef))
                    L2)
              (push (clos::make-generic-function-form 'clos:generic-flet
                      *form* name (second fdef) (cddr fdef))
                    L3))
            (err-syntax 'CLOS:GENERIC-FLET fdef))))
    ;; namelist = list of Names,
    ;; signlist = list of Signatures of the generic functions,
    ;; formlist = list of Constructor-forms of the generic functions.
    (let ((oldstackz *stackz*)
          (*stackz* *stackz*)
          (*venvc* *venvc*)
          (*venv* *venv*))
      (push 0 *stackz*) (push nil *venvc*) ; room for Closure-Dummyvar
      (let ((closuredummy-stackz *stackz*)
            (closuredummy-venvc *venvc*))
        (multiple-value-bind (varlist anodelist *fenv*)
            (do ((namelistr namelist (cdr namelistr))
                 (signlistr signlist (cdr signlistr))
                 (formlistr formlist (cdr formlistr))
                 (varlist '())
                 (anodelist '())
                 (fenv '()))
                ((null namelistr)
                 (values (nreverse varlist) (nreverse anodelist)
                         (apply #'vector (nreverse (cons *fenv* fenv)))))
              (push (car namelistr) fenv)
              (push (c-form (car formlistr) 'ONE) anodelist)
              (push 1 *stackz*)
              (let ((var (make-var :name (gensym) :specialp nil
                           :constantp nil
                           :usedp t :for-value-usedp t :really-usedp nil
                           :closurep nil ; later poss. set to T
                           :stackz *stackz* :venvc *venvc* :fnode *func*)))
                (push (cons (list* nil 'GENERIC (car signlistr)) var) fenv)
                (push var varlist)))
          (apply #'push-*venv* varlist) ; activate auxiliary variables
          (let ((body-anode ; compile remaining forms
                  (c-form `(PROGN ,@(cddr *form*)))))
            (get-anode CLOS:GENERIC-FLET)))))))

;; compile (CLOS:GENERIC-LABELS ({genfundefs}*) {form}*)
(defun c-GENERIC-LABELS ()
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (let ((oldstackz *stackz*)
        (*stackz* *stackz*)
        (*venvc* *venvc*)
        (*venv* *venv*))
    (push 0 *stackz*) (push nil *venvc*) ; room for Closure-Dummyvar
    (let ((closuredummy-stackz *stackz*)
          (closuredummy-venvc *venvc*))
      (multiple-value-bind (namelist varlist fenvconslist formlist)
          (do ((fdefsr (second *form*) (cdr fdefsr))
               (L1 '())
               (L2 '())
               (L3 '())
               (L4 '()))
              ((null fdefsr)
               (values (nreverse L1) (nreverse L2) (nreverse L3)
                       (nreverse L4)))
            (let ((fdef (car fdefsr)))
              (if (and (consp fdef) (function-name-p (car fdef))
                       (consp (cdr fdef)))
                (let ((name (first fdef)))
                  (push name L1)
                  (push 1 *stackz*)
                  (push (make-var :name (gensym) :specialp nil
                                  :constantp nil
                                  :usedp t :for-value-usedp t :really-usedp nil
                                  :closurep nil ; later poss. set to T
                                  :stackz *stackz* :venvc *venvc*
                                  :fnode *func*)
                        L2)
                  (push (cons
                          ;; fdescr
                          (list* nil 'GENERIC
                                 (clos::defgeneric-lambdalist-callinfo
                                   'clos:generic-labels *form* name (second fdef)))
                          ;; Variable
                          (car L2))
                        L3)
                  (push (clos::make-generic-function-form 'clos:generic-labels
                          *form* name (second fdef) (cddr fdef))
                        L4))
                (err-syntax 'CLOS:GENERIC-LABELS fdef))))
        ;; namelist = liste of Names, varlist = list of Variables,
        ;; fenvconslist = list of Conses (fdescr . var) for *fenv*,
        ;; formlist = list of Constructor-Forms of the generic functions.
        (let ((*fenv* ; activate function-names
                (add-fenv namelist fenvconslist)))
          (apply #'push-*venv* varlist) ; activate auxiliary variables
          (let* ((anodelist
                   (mapcar #'(lambda (form) (c-form form 'ONE)) formlist))
                 (body-anode ; compile remaining forms
                   (c-form `(PROGN ,@(cddr *form*)))))
            (get-anode CLOS:GENERIC-LABELS)))))))

) ; macrolet

;; compile (MACROLET ({macrodef}*) {form}*)
(defun c-MACROLET (&optional (c #'c-form))
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (do ((L1 (second *form*) (cdr L1))
       (L2 '()))
      ((null L1)
       (let ((*fenv* (apply #'vector (nreverse (cons *fenv* L2)))))
         ;; compile the remaining forms:
         (funcall c `(PROGN ,@(skip-declarations (cddr *form*))))))
    (let* ((macrodef (car L1))
           (name (car macrodef)))
      (push name L2)
      (push (make-macro-expander macrodef *form*) L2))))

;; compile (SYMBOL-MACROLET ({symdef}*) {declaration}* {form}*)
(defun c-SYMBOL-MACROLET (&optional (c #'c-form))
  (test-list *form* 2)
  (test-list (second *form*) 0)
  ;; check the syntax of the parameter list:
  (multiple-value-bind (symbols expansions)
      (do ((L (second *form*) (cdr L))
           (symbols nil)
           (expansions nil))
          ((null L) (values (nreverse symbols) (nreverse expansions)))
        (let ((symdef (car L)))
          (if (and (consp symdef) (symbolp (car symdef))
                   (consp (cdr symdef)) (null (cddr symdef)))
            (progn
              (push (first symdef) symbols)
              (push (second symdef) expansions))
            (c-error-c (TEXT "~S: Illegal syntax: ~S")
                       'symbol-macrolet symdef))))
    (let ((*denv* *denv*)
          (*venv*
            (apply #'vector
                   (nconc (mapcan #'(lambda (sym exp)
                                      (list sym (make-symbol-macro exp)))
                                  symbols expansions)
                          (list *venv*)))))
      (multiple-value-bind (body-rest declarations) (parse-body (cddr *form*))
        (multiple-value-bind (*specials* *ignores* *ignorables* *readonlys* other-decls)
            (process-declarations declarations)
          (push-specials)
          (push-*denv* other-decls)
          (dolist (symbol symbols)
            (if (or (constantp symbol) (proclaimed-special-p symbol))
              (c-error-c (TEXT "~S: symbol ~S is declared special and must not be declared a macro")
                         'symbol-macrolet symbol)
              (when (memq symbol *specials*)
                (c-error-c (TEXT "~S: symbol ~S must not be declared SPECIAL and a macro at the same time")
                           'symbol-macrolet symbol))))
          (funcall c `(PROGN ,@body-rest)))))))

;; compile (EVAL-WHEN ({situation}*) {form}*)
(defun c-EVAL-WHEN (&optional (c #'c-form))
  (test-list *form* 2)
  (test-list (second *form*) 0)
  (let ((load-p nil) (compile-p nil) (eval-p nil) (execute-p nil)
        (top-level-p (eq c #'compile-toplevel-form)))
    (dolist (situation (second *form*))
      (fcase equal situation
        ((LOAD) (setq load-p t))
        ((:LOAD-TOPLEVEL) (when top-level-p (setq load-p t)))
        ((COMPILE) (setq compile-p t))
        ((:COMPILE-TOPLEVEL) (when top-level-p (setq compile-p t)))
        ((EVAL) (setq eval-p t))
        ((:EXECUTE) (setq execute-p t))
        (((NOT EVAL) (NOT :EXECUTE)) (setq load-p t compile-p t))
        (((NOT COMPILE)) (setq load-p t eval-p t))
        (((NOT :COMPILE-TOPLEVEL)) (setq load-p t execute-p t))
        (t (c-error (TEXT "~s situation must be ~s, ~s or ~s, but not ~s")
                    'eval-when :load-toplevel :compile-toplevel :execute
                    situation))))
    (let ((form `(PROGN ,@(cddr *form*))))
      (when (and compile-p load-p) (c-write-lib form))
      (when compile-p (eval form))
      (funcall c (if (or load-p (and execute-p (not top-level-p))) form nil)))))

;; compile (COND {clause}*)
(defun c-COND ()
  (test-list *form* 1)
  (c-form
    (let ((clauses (cdr *form*))) ; macroexpand (COND . clauses)
      (if (null clauses)
        'NIL
        (let ((clause (car clauses)))
          (if (atom clause)
            (c-error (TEXT "COND clause without test: ~S")
                     clause)
            (let ((test (car clause)))
              (if (cdr clause)
                `(IF ,test (PROGN ,@(cdr clause)) (COND ,@(cdr clauses)))
                `(OR ,test (COND ,@(cdr clauses)))))))))))

;; compile (CASE keyform {clause}*) and (FCASE test keyform {clause}*)
(defun c-CASE ()
  (test-list *form* 1)
  (let* ((head (first *form*))
         (clauses (rest *form*))
         (test (if (eq head 'case) 'eql (pop clauses)))
         (keyform (pop clauses))
         ;; simplify clauses:
         (newclauses '())
         (allkeys '()))
    (let ((default-passed nil))
      (do ((clauses clauses))
          ((endp clauses))
        (let ((clause (pop clauses)))
          (if (atom clause)
            (c-error (TEXT "CASE clause without objects: ~S")
                     clause)
            (let ((keys (car clause)))
              (if default-passed ; was the Default already there?
                (setq keys nil)
                (if (or (eq keys 'T) (eq keys 'OTHERWISE))
                  (progn
                    (when clauses
                      (c-error-c
                       (TEXT "~S: the ~S clause must be the last one: ~S")
                       'case keys *form*))
                    (setq keys 'T)
                    (setq default-passed t))
                  (let ((newkeys '()))
                    (dolist (key (if (listp keys) keys (list keys)))
                      (if (not (member key allkeys :test test)) ; remove-duplicates
                        (progn (push key allkeys) (push key newkeys))
                        (c-style-warn (TEXT "Duplicate ~S label ~S : ~S")
                                      'case key *form*)))
                    (setq keys (nreverse newkeys)))))
              (push (cons keys (cdr clause)) newclauses)))))
      (unless default-passed (push '(T NIL) newclauses))
      (setq newclauses (nreverse newclauses))
      (setq allkeys (nreverse allkeys)))
    ;; newclauses now contains no double keys, T as keys exactly once,
    ;; and allkeys is the set of all Keys.
    (if (or (<= (length allkeys) 2) ; few Keys -> use TEST directly
            (not (memq test '(eq eql equal equalp)))) ; valid hash tests
      (let ((keyvar (gensym)))
        (labels ((ifify (clauses)
                   (if (null clauses)
                     'NIL
                     `(IF ,(let ((keys (caar clauses)))
                             (if (listp keys)
                               `(OR ,@(mapcar
                                       #'(lambda (key) `(,test ,keyvar ',key))
                                       keys))
                               'T)) ; keys = T, the Default-Case
                        (PROGN ,@(cdar clauses))
                        ,(ifify (cdr clauses))))))
          (c-form
            `(LET ((,keyvar ,keyform)) (PROGN ,keyvar ,(ifify newclauses))))))
      (let ((keyform-anode (c-form keyform 'ONE))
            (default-anode nil)
            (cases '())) ; list of Triples (keylist label anode)
        (dolist (clause newclauses)
          (if (car clause)
            (let ((anode (c-form `(PROGN ,@(cdr clause)))))
              (if (atom (car clause))
                (setq default-anode anode)
                (push (list (car clause) (make-label 'NIL) anode) cases)))
            (let ((*no-code* t)) (c-form `(PROGN ,@(cdr clause)) 'NIL))))
        (setq cases (nreverse cases))
        (if (anode-constantp keyform-anode)
          (let ((value (anode-constant-value keyform-anode)))
            (dolist (case cases default-anode)
              (when (member value (first case) :test test)
                (return (third case)))))
          (let ((default-label (make-label 'NIL))
                (end-label (make-label *for-value*)))
            (when (and (eq test 'EQL) (every #'EQL=EQ allkeys))
              (setq test 'EQ))
            (cond ((eq test 'EQ)
                   (if (every #'(lambda (x)
                                  (or (symbolp x)
                                      (and (integerp x) (<= (integer-length x) 24))
                                      (sys::short-float-p x)
                                      (characterp x)))
                              allkeys)
                     (setq test 'STABLEHASH-EQ)
                     (setq test 'FASTHASH-EQ)))
                  ((eq test 'EQL)
                   (if (every #'(lambda (x)
                                  (or (symbolp x)
                                      (numberp x)
                                      (characterp x)))
                              allkeys)
                     (setq test 'STABLEHASH-EQL)
                     (setq test 'FASTHASH-EQL)))
                  ((eq test 'EQUAL)
                   (if (every #'(lambda (x)
                                  (labels ((stablep (x depth)
                                             (if (atom x)
                                               (or (symbolp x)
                                                   (numberp x)
                                                   (characterp x))
                                               (if (= depth 0)
                                                 t
                                                 (and (stablep (car x) (1- depth))
                                                      (stablep (cdr x) (1- depth)))))))
                                    (stablep x 4)))
                              allkeys)
                     (setq test 'STABLEHASH-EQUAL)
                     (setq test 'FASTHASH-EQUAL))))
            (make-anode
              :type 'CASE
              :sub-anodes `(,keyform-anode ,@(mapcar #'third cases)
                            ,default-anode)
              :seclass
                (anodelist-seclass-or
                  `(,keyform-anode ,@(mapcar #'third cases) ,default-anode))
              :code
                `(,keyform-anode
                  (JMPHASH
                    ,test
                    ,(mapcap ; alist (obji -> labeli)
                       #'(lambda (case)
                           (let ((label (second case)))
                             (mapcar #'(lambda (obj) (cons obj label))
                                     (first case))))
                       cases)
                    ,default-label
                    ,@(mapcar #'second cases)) ; all Labels, without doubles
                  ,@(mapcap
                      #'(lambda (case)
                          `(,(second case) ; Label
                            ,(third case) ; Anode
                            (JMP ,end-label)))
                      cases)
                  ,default-label
                  ,default-anode
                  ,end-label))))))))


;;;;****             FIRST PASS :    MACROS

;;; (SYS::%HANDLER-BIND body-function 'typespec1 handler1 ...)
(defun c-HANDLER-BIND ()
  (test-list *form* 1)
  (let ((body-f (second *form*))
        (types '())
        (handler-labels '())
        (handler-anodes '()))
    (do ((tail (cddr *form*))) ((endp tail))
      (let ((type (pop tail)) (handler (pop tail)))
        (unless (quote-p type) (compiler-error 'c-HANDLER-BIND type))
        ;; the handler is a function with dynamic extent.
        (let ((label (make-label 'ONE)))
          (push (second type) types)
          (push label handler-labels)
          (push
           (let* ((*stackz* (cons 'ANYTHING *stackz*))
                  (oldstackz *stackz*)
                  (*venv* *venv*))
             ;; work place for the function:
             (push 1 *stackz*)
             (let* ((condition-sym (gensym))
                    (condition-anode
                      (make-anode :type 'CONDITION
                                  :sub-anodes '()
                                  :seclass *seclass-read*
                                  :code '())) ; first comes (HANDLER-BEGIN)
                    (condition-var (bind-movable-var condition-sym
                                                     condition-anode)))
               (push-*venv* condition-var)
               (let ((body-anode
                       (c-form `(FUNCALL ,handler ,condition-sym) 'NIL)))
                 ;; Check the variables (must not happen in the closure):
                 (checking-movable-var-list (list condition-var)
                                            (list condition-anode))
                 (let* ((codelist
                          `(,label
                            (HANDLER-BEGIN)
                            ,@(c-bind-movable-var-anode condition-var
                                                        condition-anode)
                            ,body-anode
                            (UNWINDSP ,*stackz* ,*func*) ; (SKIPSP k1 k2)
                            (UNWIND ,*stackz* ,oldstackz NIL) ; (SKIP 2)
                            (RET)))
                        (anode
                          (make-anode
                            :type 'HANDLER
                            :sub-anodes `(,body-anode)
                            :seclass *seclass-dirty* ; actually irrelevant
                            :stackz oldstackz
                            :code codelist)))
                   (optimize-var-list (list condition-var))
                   anode))))
           handler-anodes))))
    (if (null types)
      (c-form `(FUNCALL ,body-f))
      (progn
        (setq types (nreverse types))
        (setq handler-labels (nreverse handler-labels))
        (setq handler-anodes (nreverse handler-anodes))
        (let* ((label (make-label 'NIL))
               (oldstackz *stackz*)
               (*stackz* (cons 4 *stackz*)) ; HANDLER-Frame
               (body-anode (c-form `(FUNCALL ,body-f))))
          (make-anode
            :type 'HANDLER-BIND
            :sub-anodes `(,body-anode ,@handler-anodes)
            :seclass (anodelist-seclass-or `(,body-anode ,@handler-anodes))
            :stackz oldstackz
            :code `((HANDLER-OPEN ,(new-const (coerce types 'vector))
                     ,*stackz* ,@handler-labels)
                    (JMP ,label)
                    ,@handler-anodes
                    ,label
                    ,body-anode
                    (UNWIND ,*stackz* ,oldstackz ,*for-value*))))))))

;; compile (SYS::CONSTANT-EQL form1 form2 form3)
(defun c-CONSTANT-EQL ()
  (test-list *form* 4 4)
  (let ((form1 (second *form*))
        (form23 (cddr *form*)))
    (if (and *compiling-from-file*
             (c-constantp form1)
             (let ((value (c-constant-value form1)))
               (or (stringp value) (bit-vector-p value))))
      (c-form `(SYS::LOOSE-CONSTANT-EQL ,@form23))
      (c-form `(EQL ,@form23)))))

;; compile (without-package-lock (packages) ...)
(defun c-WITHOUT-PACKAGE-LOCK (&optional (c #'c-form))
  (test-list *form* 1)
  (let* ((pack-list (second *form*))
         (*compiler-unlocked-packages*
           (nconc (mapcar #'find-package (or pack-list *system-package-list*))
                  *compiler-unlocked-packages*)))
    ;; Unlock the packages temporarily in the compilation environment.
    (with-no-package-lock-internal pack-list
      ;; Process the form as if this handler would not exist. We cannot use
      ;; macroexpand-form here, because macroexpand-form leaves this form
      ;; invariant - we would get into an endless recursion.
      (funcall c (macroexpand-1 *form*)))))


;;;;****   FIRST PASS :   INLINE   FUNCTIONS   (PRIMOPS)

;; function-calls, that are treated like special forms:

;; First FUNCALL

;; (c-FUNCALL-NOTINLINE funform args) compiles a function-call
;; (FUNCALL funform . args),
;; for which the STACK-Layout of the arguments cannot be determined
;; at compile-time.
(defun c-FUNCALL-NOTINLINE (funform args)
  (test-list args 0)
  (let* ((anode1 (c-form funform 'ONE))
         (*stackz* (cons 1 *stackz*)))
    (do ((formlistr args (cdr formlistr))
         (seclass (f-side-effect funform))
         #+CLISP-DEBUG (anodelist (list anode1))
         (codelist (list '(FUNCALLP) anode1)))
        ((null formlistr)
         (push `(FUNCALL ,(length args)) codelist)
         (make-anode
           :type 'FUNCALL
           :sub-anodes (nreverse anodelist)
           :seclass seclass
           :code (nreverse codelist)))
      (let ((anode (c-form (car formlistr) 'ONE)))
        #+CLISP-DEBUG (push anode anodelist)
        (seclass-or-f seclass anode)
        (push anode codelist))
      (push '(PUSH) codelist)
      (push 1 *stackz*))))

;; (c-FUNCALL-INLINE funform args applyargs lambdabody sameenv) compiles a
;; function-call (FUNCALL funform . args) resp.
;; (APPLY funform . args applyargs) [applyargs a list out of a form],
;; for which the STACK-Layout of the arguments can be determined at
;; compile-time.  sameenv specifies, if lambdabody is to be viewed in
;; the same environment or in the top-level-environment.
(defun c-FUNCALL-INLINE (funform arglist applyarglist lambdabody sameenv)
  (test-list lambdabody 1)
  (multiple-value-bind (reqvar  optvar optinit optsvar  restvar
                        keyflag keyword keyvar keyinit keysvar allow-other-keys
                        auxvar auxinit)
      (let ((lalist (pop lambdabody)) (declarations '()))
        (when *defun-accept-specialized-lambda-list*
          (multiple-value-setq (lalist declarations)
            (sys::specialized-lambda-list-to-ordinary lalist 'compile))
          (push (cons 'DECLARE (nreverse declarations)) lambdabody))
        (c-analyze-lambdalist lalist))
    (when (or keyflag keyword keyvar keyinit keysvar allow-other-keys)
      (compiler-error 'c-FUNCALL-INLINE funform))
    (let ((r (length reqvar)) ; number of required-arguments
          (s (length optvar)) ; number of optional arguments
          (|t| (length arglist))) ; number of specified arguments
      (when (and (null restvar) (> |t| (+ r s)))
        ;; too many arguments specified. Is redressed by introduction
        ;; of several additional optional arguments:
        (c-error-c (TEXT "Too many arguments to ~S") funform)
        (dotimes (i (- |t| (+ r s)))
          (let ((var (gensym)))
            (setq optvar (append optvar (list var)))
            (setq optinit (append optinit (list nil)))
            (setq optsvar (append optsvar (list nil)))
            (incf s)
            (push `(DECLARE (IGNORE ,var)) lambdabody))))
      (when (and (null applyarglist) (< |t| r))
        ;; too few arguments specified. Is redressed by introduction
        ;; of additional arguments:
        (c-error-c (TEXT "Too few arguments to ~S") funform)
        (setq arglist (append arglist
                              (make-list (- r |t|) :initial-element nil)))
        (setq |t| r))
      ;; Now (t>=r or apply-arg is there)
      ;; and (t<=r+s or &rest-Parameter is there).
      (let ((oldstackz *stackz*)
            (oldvenv *venv*)
            (oldfenv *fenv*)
            (oldbenv *benv*)
            (oldgenv *genv*)
            (olddenv *denv*)
            (*stackz* *stackz*)
            (*venv* (and sameenv *venv*))
            (*venvc* *venvc*)
            (*fenv* (and sameenv *fenv*))
            (*benv* (and sameenv *benv*))
            (*genv* (and sameenv *genv*))
            (*denv* (if sameenv
                      *denv*
                      (cons `(INLINING ,funform)
                            (remove-if-not
                             #'(lambda (declspec)
                                 (case (car declspec)
                                   ((DECLARATION SYS::IN-DEFUN INLINING) t)
                                   (t nil)))
                             *denv*)))))
        (multiple-value-bind (body-rest declarations)
            (parse-body lambdabody t)
          (let (*specials* *ignores* *ignorables* *readonlys* other-decls
                req-vars req-anodes req-stackzs
                opt-vars opt-anodes opt-stackzs ; optional and svar together!
                optdefaulted-vars optdefaulted-anodes optdefaulted-stackzs
                rest-vars rest-anodes rest-stackzs
                fixed-anodes fixed-stackz
                reqfixed-vars reqfixed-dummys reqfixed-stackzs
                optfixed-vars optfixed-dummys optfixed-anodes
                optsfixed-vars optsfixed-anodes optfixed-stackzs
                restfixed-vars restfixed-dummys restfixed-stackzs
                aux-vars aux-anodes
                closuredummy-stackz closuredummy-venvc)
            (multiple-value-setq (*specials* *ignores* *ignorables* *readonlys* other-decls)
              (process-declarations declarations))
            (push 0 *stackz*) (push nil *venvc*) ; room for Closure-Dummyvar
            (setq closuredummy-stackz *stackz* closuredummy-venvc *venvc*)
            (flet ((finish-using-applyarg
                       (reqvar optvar optinit optsvar restvar)
                     ;; reqvar and optvar/optinit/optsvar as well as
                     ;; arglist are already partially shortened.
                     ;; Disassembly of the remaining argument-list by
                     ;; means of UNLIST resp. UNLIST*. Hence a
                     ;; stack-layout with fixed look, cf. c-LAMBDABODY.
                     (setq fixed-anodes
                           (list
                             (let ((anode1 (let ((*venv* oldvenv)
                                                 (*fenv* oldfenv)
                                                 (*benv* oldbenv)
                                                 (*genv* oldgenv)
                                                 (*denv* olddenv))
                                             (c-form (first applyarglist)
                                                     'ONE)))
                                   (anode2 (c-unlist (not (eql restvar 0))
                                                     (+ (length reqvar)
                                                        (length optvar))
                                                     (length optvar))))
                               (make-anode
                                 :type 'APPLY-UNLIST
                                 :sub-anodes (list anode1 anode2)
                                 :seclass (anodes-seclass-or anode1 anode2)
                                 :code `(,anode1 ,anode2)))))
                     ;; build Stack-Dummy-Variable for the
                     ;; reqvar,optvar,restvar:
                     (multiple-value-setq (reqfixed-vars reqfixed-dummys)
                       (process-fixed-var-list reqvar))
                     (multiple-value-setq (optfixed-vars optfixed-dummys)
                       (process-fixed-var-list optvar))
                     (multiple-value-setq (restfixed-vars restfixed-dummys)
                       (if (eql restvar 0)
                         (values '() '())
                         (process-fixed-var-list (list restvar))))
                     (push 0 *stackz*) (setq fixed-stackz *stackz*)
                     ;; activate the bindings of the required-parameters:
                     (setq reqfixed-stackzs (bind-req-vars reqfixed-vars))
                     ;; activate the bindings of the optional-parameters/svar:
                     (multiple-value-setq (optfixed-anodes optfixed-stackzs
                                           optsfixed-vars optsfixed-anodes)
                       (bind-opt-vars optfixed-vars optfixed-dummys
                                      optinit optsvar))
                     ;; activate the binding of the rest-parameter:
                     (unless (eql restvar 0)
                       (setq restfixed-stackzs
                             (bind-rest-vars restfixed-vars)))))
              (block main-args
                ;; bind required-parameter:
                (do ((reqvarr reqvar (cdr reqvarr)))
                    ((null reqvarr))
                  (if (null arglist) ; implies, that apply-arg is there
                    (return-from main-args
                      (finish-using-applyarg reqvarr optvar optinit optsvar
                                             restvar))
                    (let* ((form (pop arglist))
                           (anode (let ((*venv* oldvenv)
                                        (*fenv* oldfenv)
                                        (*benv* oldbenv)
                                        (*genv* oldgenv)
                                        (*denv* olddenv))
                                    (c-form form 'ONE)))
                           (var (bind-movable-var (car reqvarr) anode)))
                      (push anode req-anodes)
                      (push var req-vars)
                      (push *stackz* req-stackzs)
                      (push-*venv* var))))
                ;; bind optional parameters and Svars:
                (do ((optvarr optvar (cdr optvarr))
                     (optinitr optinit (cdr optinitr))
                     (optsvarr optsvar (cdr optsvarr)))
                    ((null optvarr))
                  (if (and applyarglist (null arglist))
                    (return-from main-args
                      (finish-using-applyarg '() optvarr optinitr
                                             optsvarr restvar))
                    (let ((svar-init (not (null arglist)))) ; = NIL or T
                      (if svar-init
                        (progn
                          (let ((*no-code* t))
                            (c-form (car optinitr) 'NIL))
                          (let* ((anode
                                   (let ((*venv* oldvenv)
                                         (*fenv* oldfenv)
                                         (*benv* oldbenv)
                                         (*genv* oldgenv)
                                         (*denv* olddenv))
                                     (c-form (pop arglist) 'ONE)))
                                 (var (bind-movable-var (car optvarr) anode)))
                            (push anode opt-anodes)
                            (push var opt-vars)
                            (push *stackz* opt-stackzs)
                            (push-*venv* var)
                            (unless (eql (car optsvarr) 0)
                              (let* ((anode (c-form svar-init 'ONE))
                                     (var (bind-movable-var (car optsvarr) anode)))
                                (push anode opt-anodes)
                                (push var opt-vars)
                                (push *stackz* opt-stackzs)
                                (push-*venv* var)))))
                        (let* ((anode (c-form (car optinitr) 'ONE))
                               (var (bind-movable-var (car optvarr) anode)))
                          (push anode optdefaulted-anodes)
                          (push var optdefaulted-vars)
                          (push *stackz* optdefaulted-stackzs)
                          (push-*venv* var)
                          (unless (eql (car optsvarr) 0)
                            (let* ((anode (c-form svar-init 'ONE))
                                   (var (bind-movable-var (car optsvarr) anode)))
                              (push anode optdefaulted-anodes)
                              (push var optdefaulted-vars)
                              (push *stackz* optdefaulted-stackzs)
                              (push-*venv* var))))))))
                (if (eql restvar 0)
                  ;; consume further arguments:
                  (when applyarglist
                    (return-from main-args
                      (finish-using-applyarg '() '() '() '() restvar)))
                  ;; bind rest-parameter:
                  (let* ((form (if applyarglist
                                 (if arglist
                                   `(LIST* ,@arglist ,@applyarglist)
                                   (first applyarglist))
                                 (if arglist `(LIST ,@arglist) 'NIL)))
                         (anode (let ((*venv* oldvenv)
                                      (*fenv* oldfenv)
                                      (*benv* oldbenv)
                                      (*genv* oldgenv)
                                      (*denv* olddenv))
                                  (c-form form 'ONE)))
                         (var (bind-movable-var restvar anode)))
                    (push anode rest-anodes)
                    (push var rest-vars)
                    (push *stackz* rest-stackzs)
                    (push-*venv* var)))
                (push 0 *stackz*) (setq fixed-stackz *stackz*)))
            (setq req-vars (nreverse req-vars))
            (setq req-anodes (nreverse req-anodes))
            (setq req-stackzs (nreverse req-stackzs))
            (setq opt-vars (nreverse opt-vars))
            (setq opt-anodes (nreverse opt-anodes))
            (setq opt-stackzs (nreverse opt-stackzs))
            (setq optdefaulted-vars (nreverse optdefaulted-vars))
            (setq optdefaulted-anodes (nreverse optdefaulted-anodes))
            (setq optdefaulted-stackzs (nreverse optdefaulted-stackzs))
            ;; activate the bindings of the Aux-Variables:
            (multiple-value-setq (aux-vars aux-anodes)
              (bind-aux-vars auxvar auxinit))
            (push-specials)
            (push-*denv* other-decls)
            (let* ((body-anode (c-form `(PROGN ,@body-rest)))
                   ;; check the variables:
                   (varlist
                     (append req-vars opt-vars optdefaulted-vars rest-vars
                             reqfixed-vars optfixed-vars optsfixed-vars
                             restfixed-vars aux-vars))
                   (closurevars
                     (append
                       (checking-movable-var-list req-vars req-anodes)
                       (checking-movable-var-list opt-vars opt-anodes)
                       (checking-movable-var-list optdefaulted-vars optdefaulted-anodes)
                       (checking-movable-var-list rest-vars rest-anodes)
                       (checking-fixed-var-list reqfixed-vars)
                       (checking-fixed-var-list optfixed-vars)
                       (checking-movable-var-list optsfixed-vars
                                                  optsfixed-anodes)
                       (checking-fixed-var-list restfixed-vars)
                       (checking-movable-var-list aux-vars aux-anodes)))
                   (codelist
                     `(,@(c-make-closure closurevars closuredummy-venvc
                                         closuredummy-stackz)
                       ,@(let ((*stackz* fixed-stackz))
                           (c-parallel-bind-movable-var-anode
                             (append req-vars    opt-vars    rest-vars   )
                             (append req-anodes  opt-anodes  rest-anodes )
                             (append req-stackzs opt-stackzs rest-stackzs)
                             fixed-anodes))
                       ,@(mapcap #'c-bind-movable-var-anode
                                 optdefaulted-vars optdefaulted-anodes)
                       ,@(mapcap #'c-bind-fixed-var reqfixed-vars
                                 reqfixed-dummys reqfixed-stackzs)
                       ,@(c-bind-with-svars optfixed-vars optfixed-dummys
                                            optsfixed-vars optfixed-anodes
                                            optsfixed-anodes optfixed-stackzs)
                       ,@(mapcap #'c-bind-fixed-var restfixed-vars
                                 restfixed-dummys restfixed-stackzs)
                       ,@(mapcap #'c-bind-movable-var-anode aux-vars
                                 aux-anodes)
                       ,body-anode
                       (UNWIND ,*stackz* ,oldstackz ,*for-value*)))
                   (anode
                     (make-anode
                       :type 'FUNCALL
                       :sub-anodes
                         `(,@req-anodes ,@opt-anodes ,@optdefaulted-anodes ,@rest-anodes
                           ,@fixed-anodes ,@optfixed-anodes
                           ,@(remove nil optsfixed-anodes)
                           ,@aux-anodes ,body-anode)
                       :seclass
                         (seclass-without
                           (anodelist-seclass-or
                             `(,@req-anodes ,@opt-anodes ,@optdefaulted-anodes ,@rest-anodes
                               ,@fixed-anodes ,@optfixed-anodes
                               ,@(remove nil optsfixed-anodes)
                               ,@aux-anodes ,body-anode))
                           varlist)
                       :stackz oldstackz
                       :code codelist)))
              (closuredummy-add-stack-slot
               closurevars closuredummy-stackz closuredummy-venvc)
              (optimize-var-list varlist)
              anode)))))))

;; compiles (fun {form}*), whereas fun is a local function.
;; fdescr is the associated information from *fenv*.
(defun c-LOCAL-FUNCTION-CALL (fun fdescr args)
  ;; (test-list args 0) ; that manages in a moment (test-argument-syntax ...)
  ;; fetch Call-Specification:
  (multiple-value-bind (req opt rest-flag key-flag keylist allow-flag)
      (fdescr-signature fdescr)
    (case (test-argument-syntax
           args nil fun req opt rest-flag key-flag keylist allow-flag)
      ((NO-KEYS STATIC-KEYS)
       ;; call INLINE
       (c-DIRECT-FUNCTION-CALL
         args nil fun req opt rest-flag key-flag keylist
         nil ; no SUBR-, but Cclosure-call
         (cclosure-call-code-producer fun (car fdescr) req opt rest-flag
                                      key-flag keylist)))
      (t (c-FUNCALL-NOTINLINE `(FUNCTION ,fun) args)))))

;; check whether (FUNC FORM) can be inlined
;; useful for (CONSTANTLY ...) and (COMPLEMENT ...)
(defun inlinable-function-operation-form-p (form func)
  (and (consp form) (eq (first form) func)
       (consp (rest form)) (null (cddr form))
       (not (fenv-search func))
       (not (declared-notinline func))))

;; (c-FUNCTION-CALL funform arglist) compiles a function call
;; (FUNCALL funform . arglist).
(defun c-FUNCTION-CALL (funform arglist)
  (setq funform (macroexpand-form funform))
  (when (inline-callable-function-lambda-p funform (length arglist))
    ;; call of a Lambda-expression is possible INLINE
    (return-from c-FUNCTION-CALL
      (c-FUNCALL-INLINE funform arglist nil (cdr (second funform)) t)))
  (when (and (inlinable-function-operation-form-p funform 'COMPLEMENT)
             (not (fenv-search 'NOT)))
    ;; (complement fn) -->
    ;; (let ((f fn)) ... #'(lambda (&rest args) (not (apply f args))) ...)
    (return-from c-FUNCTION-CALL
      (c-form `(NOT (FUNCALL ,(second funform) ,@arglist)))))
  (when (inlinable-function-operation-form-p funform 'CONSTANTLY)
    ;; (constantly obj) -->
    ;; (let ((o obj)) ... #'(lambda (&rest a) (declare (ignore a)) o) ...)
    (return-from c-FUNCTION-CALL
      (c-form `(PROG1 ,(second funform) ,@arglist))))
  (when (simple-function-form-p funform) ; #'symbol
    (return-from c-FUNCTION-CALL
      (progn
        (test-list funform 2 2)
        ;; list in more detail, cf. c-FUNCTION ??
        (c-form `(,(second funform) ,@arglist)))))
  ;; call of NOTINLINE
  (c-FUNCALL-NOTINLINE funform arglist))

(defun c-FUNCALL ()
  (test-list *form* 2)
  (c-FUNCTION-CALL (second *form*) (cddr *form*)))

(defun c-APPLY ()
  (test-list *form* 3)
  (let* ((funform (second *form*))
         (arglist (cddr *form*))
         (args (butlast arglist))
         (apply-args (last arglist))
         (n (1- (length arglist)))) ; the minimum number of arguments
    (setq funform (macroexpand-form funform))
    (when (inline-callable-function-lambda-p funform n t)
      (return-from c-APPLY
        (c-FUNCALL-INLINE funform args apply-args (cdr (second funform)) t)))
    (when (and (inlinable-function-operation-form-p funform 'COMPLEMENT)
               (not (fenv-search 'NOT)))
      ;; (complement fn) -->
      ;; (let ((f fn)) ... #'(lambda (&rest args) (not (apply f args))) ...)
      (return-from c-APPLY
        (c-form `(NOT (APPLY ,(second funform) ,@arglist)))))
    (when (inlinable-function-operation-form-p funform 'CONSTANTLY)
      ;; (constantly obj) -->
      ;; (let ((o obj)) ... #'(lambda (&rest a) (declare (ignore a)) o) ...)
      (return-from c-APPLY
        (c-form `(PROG1 ,(second funform) ,@arglist))))
    (when (simple-function-form-p funform) ; #'symbol
      (let ((fun (second funform)))
        (test-list funform 2 2)
        (let (name req opt rest-p key-p keylist allow-p)
          (multiple-value-bind (a m f1 f2 f3) (fenv-search fun)
            (declare (ignore m f1 f2))
            (if a           ; fun is local
              (setq name fun
                    req (fnode-req-anz (car f3))
                    opt (fnode-opt-anz (car f3))
                    rest-p (fnode-rest-flag (car f3))
                    key-p (fnode-keyword-flag (car f3))
                    keylist (fnode-keywords (car f3))
                    allow-p (fnode-allow-other-keys-flag (car f3)))
              (multiple-value-setq (name req opt rest-p key-p keylist allow-p)
                (function-signature fun t))) ; global functions only
            (if (and name (equal fun name))
              (test-argument-syntax args apply-args fun req opt rest-p
                                    key-p keylist allow-p)
              (note-function-used fun args apply-args))))
        (unless (declared-notinline fun) ; can fun be taken INLINE?
          (flet ((c-LOCAL-APPLY (fdescr)
                   (multiple-value-bind
                         (req opt rest-flag key-flag keylist allow-flag)
                       (fdescr-signature fdescr)
                     (unless key-flag
                       ;; without Keyword-Arguments
                       (when (eq 'NO-KEYS
                                 (test-argument-syntax
                                  args apply-args
                                  fun req opt rest-flag key-flag keylist
                                  allow-flag))
                         ;; Syntax correct -> call INLINE
                         (return-from c-APPLY
                           (c-DIRECT-FUNCTION-CALL args apply-args
                             fun req opt rest-flag key-flag keylist
                             nil ; no SUBR-, but Cclosure-Aufruf
                             (cclosure-call-code-producer
                              fun (car fdescr) req opt rest-flag
                              key-flag keylist))))))))
            (multiple-value-bind (a m f1 f2 f3 f4) (fenv-search fun)
              (declare (ignore m f2 f4))
              ;; (APPLY #'fun . args) maybe possible to simplify
              (if (null a)
                ;; global function
                (unless (and (symbolp fun) ; special form or global macro?
                             (or (special-operator-p fun)
                                 (macro-function fun)))
                  (when (in-defun-p fun)
                    ;; recursive call of the current global function
                    (c-LOCAL-APPLY (cons *func* nil)))
                  (let ((inline-lambdabody (inline-lambdabody fun)))
                    (when (inline-callable-lambdabody-p inline-lambdabody n t)
                      ;; inline call of the global function is possible
                      (return-from c-APPLY
                        (c-FUNCALL-INLINE fun args apply-args
                                          inline-lambdabody nil)))))
                (when (eq f1 'LOCAL) ; local function
                  (c-LOCAL-APPLY f3))))))))
    ;; if none of the optimizations was possible:
    (let* ((anode1 (c-form funform 'ONE))
           (seclass (f-side-effect funform))
           (*stackz* (cons 1 *stackz*)))
      (do ((formlistr arglist (cdr formlistr))
           #+CLISP-DEBUG (anodelist (list anode1))
           (codelist (list '(APPLYP) anode1)))
          ((null formlistr)
           (push `(APPLY ,n) codelist)
           (make-anode
             :type 'APPLY
             :sub-anodes (nreverse anodelist)
             :seclass seclass
             :code (nreverse codelist)))
        (let ((anode (c-form (car formlistr) 'ONE)))
          #+CLISP-DEBUG (push anode anodelist)
          (seclass-or-f seclass anode)
          (push anode codelist)
          (when (cdr formlistr)
            (push 1 *stackz*) (push '(PUSH) codelist)))))))

;; macroexpand form and check for it being a constant number
;; signal an error when it is not an number
;; returns 2 values - value and constant-p
(defun c-constant-number (form)
  (let ((expanded (macroexpand-form form)))
    (when (c-constantp expanded)
      (let ((val (c-constant-value expanded)))
        (if (numberp val)
          (return-from c-constant-number (values val t))
          (c-warn
            (TEXT "Arithmetic operand ~s must evaluate to a number, not ~s")
            form val))))
    (values expanded nil)))

;; return two values: the list of numeric constants
;; and the list of other forms
;; This optimization makes compiled code behave differently than interpreted
;; code, but is allowed by CLHS section 12.1.1.1.
(defun c-collect-numeric-constants (forms)
  (let ((consts nil) (others nil))
    (dolist (form forms)
      (multiple-value-bind (val const-p) (c-constant-number form)
        (if const-p (push val consts) (push val others))))
    (values (nreverse consts)   ; not really necessary to nreverse consts
            (nreverse others))))

(defun c-PLUS ()
  (test-list *form* 1)
  (multiple-value-bind (const-sum other-parts)
      (c-collect-numeric-constants (cdr *form*))
    (unless (try-eval (setq const-sum (reduce #'+ const-sum)))
      (return-from c-PLUS (c-GLOBAL-FUNCTION-CALL-form *form*)))
    (cond ((null other-parts)   ; constant addends only
           (c-form const-sum))  ; ==> constant result
          ((eql const-sum 0)    ; const-sum == 0 ==> skip it
           ;; this is a bad optimization: THE is slower than #'+
           ;;(if (cdr other-parts) (c-form `(the number ,@other-parts)) ...)
           (c-GLOBAL-FUNCTION-CALL-form `(+ ,@other-parts)))
          ((null (cdr other-parts)) ; just one non-constant summand
           (case const-sum
             (+1 (c-form `(1+ ,@other-parts)))
             (-1 (c-form `(1- ,@other-parts)))
             (t (c-GLOBAL-FUNCTION-CALL-form `(+ ,const-sum ,@other-parts)))))
          (t (c-GLOBAL-FUNCTION-CALL-form `(+ ,const-sum ,@other-parts))))))

(defun c-STAR ()
  (test-list *form* 1)
  (multiple-value-bind (const-prod other-parts)
      (c-collect-numeric-constants (cdr *form*))
    (unless (try-eval (setq const-prod (reduce #'* const-prod)))
      (return-from c-STAR (c-GLOBAL-FUNCTION-CALL-form *form*)))
    (cond ((null other-parts)     ; constant multiples only
           (c-form const-prod))   ; ==> constant result
          ;; this is a bad optimization: one call to #'* is cheaper than PROGN
          ;;((eql const-prod 0)   ; const-prod == 0 ==> result == 0
          ;; (c-form `(progn ,@(mapcar (lambda (form) `(the number ,form))
          ;;                           other-parts)
          ((eql const-prod 1)   ; const-prod == 1 ==> skip it
           ;; this is a bad optimization: THE is slower than #'*
           ;;(if (cdr other-parts) (c-form `(the number ,@other-parts)) ...)
           (c-GLOBAL-FUNCTION-CALL-form `(* ,@other-parts)))
          ((and (eql const-prod -1) (null (cdr other-parts)))
           (c-GLOBAL-FUNCTION-CALL-form `(- ,@other-parts)))
          (t (c-GLOBAL-FUNCTION-CALL-form `(* ,const-prod ,@other-parts))))))

(defun c-MINUS ()
  (test-list *form* 2)
  (let ((unary-p (= (length *form*) 2))
        (const-sum 0)           ; the constant sum in the tail
        (first-part 0) (other-parts '()))
    (unless unary-p
      (multiple-value-bind (val const-p) (c-constant-number (second *form*))
        (if const-p
          (setq const-sum val)
          (setq first-part val))))
    (multiple-value-bind (consts others)
        (c-collect-numeric-constants (if unary-p (cdr *form*) (cddr *form*)))
      (unless (try-eval (setq const-sum (reduce #'- consts
                                                :initial-value const-sum)))
        (return-from c-MINUS (c-GLOBAL-FUNCTION-CALL-form *form*)))
      (setq other-parts others))
    (if (null other-parts)      ; nothing to subtract
      (let ((*form* `(+ ,const-sum ,first-part))) (c-PLUS))
      (c-GLOBAL-FUNCTION-CALL-form
       `(- ,@(if (eql first-part 0)
               (if (and (eql const-sum 0) (null (cdr other-parts)))
                 '()
                 `(,const-sum))
               (if (eql const-sum 0)
                 `(,first-part)
                 `(,first-part ,(- const-sum))))
           ,@other-parts)))))

(defun c-SLASH ()
  (test-list *form* 2)
  (let ((unary-p (= (length *form*) 2))
        (const-prod 1)          ; the constant product in the tail
        (first-part 1) (other-parts '()))
    (unless unary-p
      (multiple-value-bind (val const-p) (c-constant-number (second *form*))
        (if const-p
          (setq const-prod val)
          (setq first-part val))))
    (multiple-value-bind (consts others)
        (c-collect-numeric-constants (if unary-p (cdr *form*) (cddr *form*)))
      (unless (try-eval (setq const-prod (reduce #'/ consts
                                                 :initial-value const-prod)))
        (return-from c-SLASH (c-GLOBAL-FUNCTION-CALL-form *form*)))
      (setq other-parts others))
    (cond ((null other-parts)      ; no divisors
           (let ((*form* `(* ,const-prod ,first-part))) (c-STAR)))
          ;; this is a bad optimization: one call to #'/ is cheaper than PROGN
          ;;((eql first-part 0)
          ;; (c-form `(progn ,@(mapcar (lambda (form) `(the number ,form))
          ;;                           other-parts)
          ;;                 0)))
          (t (c-GLOBAL-FUNCTION-CALL-form
              `(/ ,@(if (eql first-part 1)
                      (if (and (eql const-prod 1) (null (cdr other-parts)))
                        '()
                        `(,const-prod))
                      (if (eql const-prod 1)
                        `(,first-part)
                        `(,first-part ,(/ const-prod))))
                  ,@other-parts))))))

(defun c-COMPARE-NUMBERS ()
  (test-list *form* 2)
  (if (= (length *form*) 3)
    ;; Optimize (cmp arg1 arg2) by looking whether arg1 or arg2 is known to be
    ;; zero. (Which kind of zero, doesn't matter.)
    (multiple-value-bind (arg-1 const1-p) (c-constant-number (second *form*))
      (if (and const1-p (zerop arg-1))
        (c-GLOBAL-FUNCTION-CALL-form
          (ecase (first *form*)
            (= `(ZEROP ,(third *form*)))
            (/= `(NOT (ZEROP ,(third *form*))))
            (< `(PLUSP ,(third *form*)))
            (<= `(NOT (MINUSP ,(third *form*))))
            (> `(MINUSP ,(third *form*)))
            (>= `(NOT (PLUSP ,(third *form*))))))
        (multiple-value-bind (arg-2 const2-p)
            (c-constant-number (third *form*))
          (c-GLOBAL-FUNCTION-CALL-form
            (if (and const2-p (zerop arg-2))
              (ecase (first *form*)
                (= `(ZEROP ,arg-1))
                (/= `(NOT (ZEROP ,arg-1)))
                (< `(MINUSP ,arg-1))
                (<= `(NOT (PLUSP ,arg-1)))
                (> `(PLUSP ,arg-1))
                (>= `(NOT (MINUSP ,arg-1))))
             ;; Use arg-1 and arg-2 instead of (second *form*), (third *form*),
             ;; to avoid expanding these subforms a second time.
             `(,(first *form*) ,arg-1 ,arg-2))))))
    (c-GLOBAL-FUNCTION-CALL (first *form*))))

(defun c-SVSTORE ()
  (test-list *form* 4 4)
  ;; (sys::svstore arg1 arg2 arg3) -> (sys::%svstore arg3 arg1 arg2)
  (let ((arg1 (second *form*)) (arg2 (third *form*)) (arg3 (fourth *form*))
        (argvar1 (gensym)) (argvar2 (gensym)))
    (c-form
      `(LET* ((,argvar1 ,arg1) (,argvar2 ,arg2))
         (SYS::%SVSTORE ,arg3 ,argvar1 ,argvar2)))))

(defun c-EQ ()
  (test-list *form* 3 3)
  (let ((arg1 (macroexpand-form (second *form*)))
        (arg2 (macroexpand-form (third *form*))))
    (if (and (c-constantp arg1) (c-constantp arg2))
      (c-form `(QUOTE ,(eq (c-constant-value arg1) (c-constant-value arg2))))
      (progn
        (when (c-constantp arg1)
          ;; arg2 being constant is better, so that JMPIFEQTO is possible
          (rotatef arg1 arg2))
        (if (and (c-constantp arg2) (eq (c-constant-value arg2) 'NIL))
          (c-GLOBAL-FUNCTION-CALL-form `(NULL ,arg1))
          (c-GLOBAL-FUNCTION-CALL-form `(EQ ,arg1 ,arg2)))))))

;; EQL is the same as EQ for symbols, fixnums and characters
(defun EQL=EQ (x)
  (or (symbolp x)
      (and (integerp x) (<= (integer-length x) 24))
      ;; Note: Using (fixnump x) here would not generate portable code.
      ;; The minimum fixnum length across architectures in CLISP is 24 bits.
      (sys::short-float-p x)
      (characterp x)))

(defun c-EQL ()
  (test-list *form* 3 3)
  (let ((arg1 (macroexpand-form (second *form*)))
        (arg2 (macroexpand-form (third *form*))))
    (cond ((and (c-constantp arg1) (c-constantp arg2))
           (c-form `(QUOTE ,(eql (c-constant-value arg1)
                                 (c-constant-value arg2)))))
          ((or (and (c-constantp arg1) (EQL=EQ (c-constant-value arg1)))
               (and (c-constantp arg2) (EQL=EQ (c-constant-value arg2))))
           (let ((*form* `(EQ ,arg1 ,arg2))) (c-EQ)))
          (t (c-GLOBAL-FUNCTION-CALL-form `(EQL ,arg1 ,arg2))))))

;; EQUAL is the same as EQL for symbols, numbers and characters
(defun EQUAL=EQL (x) (or (symbolp x) (numberp x) (characterp x)))

(defun c-EQUAL ()
  (test-list *form* 3 3)
  (let ((arg1 (macroexpand-form (second *form*)))
        (arg2 (macroexpand-form (third *form*))))
    (cond ((or (and (c-constantp arg1) (EQUAL=EQL (c-constant-value arg1)))
               (and (c-constantp arg2) (EQUAL=EQL (c-constant-value arg2))))
           (let ((*form* `(EQL ,arg1 ,arg2))) (c-EQL)))
          (t (c-GLOBAL-FUNCTION-CALL-form `(EQUAL ,arg1 ,arg2))))))

;; Forms the inner part of a MAPCAR/MAPC/MAPCAN/MAPCAP-Expansion
(defun c-MAP-on-CARs-inner (innerst-fun blockname endp-value restvars
                            &optional (itemvars '()))
  (if (null restvars)
    (funcall innerst-fun (nreverse itemvars))
    (let ((restvar (car restvars))
          (itemvar (gensym)))
      `(IF (ENDP ,restvar)
         (RETURN-FROM ,blockname ,endp-value)
         (LET ((,itemvar (CAR ,restvar)))
           ,(c-MAP-on-CARs-inner innerst-fun blockname endp-value
                                 (cdr restvars) (cons itemvar itemvars)))))))

;; make shift forms for variables: (a b c) --> (a (cdr a) b (cdr b) c (cdr c))
(defun shift-vars (restvars)
  (mapcap #'(lambda (restvar) `(,restvar (CDR ,restvar))) restvars))

(proclaim '(inline copy-list-lax))
(defun copy-list-lax (obj) ; ABI
  "Like COPY-LIST, but return the argument when it is not a LIST."
  (if (consp obj) (copy-list obj) obj))

;; Forms a MAPCAR/MAPCAN/MAPCAP-Expansion
(defun c-MAP-on-CARs (adjoin-fun funform forms)
  (let ((erg (gensym)) (tail (gensym))
        (blockname (gensym))
        (restvars (gensym-list forms))
        (tag (gensym)) (tmp (gensym)))
    `(LET ((,erg NIL)
           ,@(case adjoin-fun ((CONS)) ((NCONC APPEND) `((,tail nil)))))
       (BLOCK ,blockname
         (LET* ,(mapcar #'list restvars forms)
           (TAGBODY
             ,tag
             ,(c-MAP-on-CARs-inner
               (case adjoin-fun
                 ((CONS)
                  #'(lambda (itemvars)
                      `(SETQ ,erg (,adjoin-fun (FUNCALL ,funform ,@itemvars)
                                               ,erg))))
                 ((NCONC)
                  #'(lambda (itemvars)
                      `(LET ((,tmp (FUNCALL ,funform ,@itemvars)))
                         (IF (CONSP ,erg)
                           (SETF ,tail (LAST ,tail) (CDR ,tail) ,tmp)
                           (SETQ ,erg ,tmp ,tail ,erg)))))
                 ((APPEND)
                  #'(lambda (itemvars)
                      `(LET ((,tmp (COPY-LIST-LAX (FUNCALL ,funform ,@itemvars))))
                         (IF (CONSP ,erg)
                           (SETF ,tail (LAST ,tail) (CDR ,tail) ,tmp)
                           (SETQ ,erg ,tmp ,tail ,erg))))))
               blockname
               'NIL
               restvars)
             (SETQ ,@(shift-vars restvars))
             (GO ,tag))))
       ,(case adjoin-fun
          ((CONS) `(SYS::LIST-NREVERSE ,erg))
          (t erg)))))

;; Forms a MAPLIST/MAPCON/MAPLAP-Expansion
(defun c-MAP-on-LISTs (adjoin-fun funform forms)
  (let ((erg (gensym)) (tail (gensym))
        (blockname (gensym))
        (restvars (gensym-list forms))
        (tag (gensym)) (tmp (gensym)))
    `(LET ((,erg NIL)
           ,@(case adjoin-fun ((CONS)) ((NCONC APPEND) `((,tail nil)))))
       (BLOCK ,blockname
         (LET* ,(mapcar #'list restvars forms)
           (TAGBODY
             ,tag
             (IF (OR ,@(mapcar #'(lambda (restvar) `(ENDP ,restvar)) restvars))
               (RETURN-FROM ,blockname))
             ,(case adjoin-fun
               ((CONS)
                `(SETQ ,erg (,adjoin-fun (FUNCALL ,funform ,@restvars) ,erg)))
               ((NCONC)
                `(LET ((,tmp (FUNCALL ,funform ,@restvars)))
                   (IF (CONSP ,erg)
                     (SETF ,tail (LAST ,tail) (CDR ,tail) ,tmp)
                     (SETQ ,erg ,tmp ,tail ,erg))))
               ((APPEND)
                `(LET ((,tmp (COPY-LIST-LAX (FUNCALL ,funform ,@restvars))))
                   (IF (CONSP ,erg)
                     (SETF ,tail (LAST ,tail) (CDR ,tail) ,tmp)
                     (SETQ ,erg ,tmp ,tail ,erg)))))
             (SETQ ,@(shift-vars restvars))
             (GO ,tag))))
       ,(case adjoin-fun
          ((CONS) `(SYS::LIST-NREVERSE ,erg))
          (t erg)))))

(defun c-MAPC ()
  (test-list *form* 3)
  (let ((funform (macroexpand-form (second *form*))))
    (if (inline-callable-function-p funform (length (cddr *form*)))
      (c-form
        (let* ((tempvar (gensym))
               (forms (cons tempvar (cdddr *form*)))
               (blockname (gensym))
               (restvars (gensym-list forms))
               (tag (gensym)))
          `(LET ((,tempvar ,(third *form*)))
             (BLOCK ,blockname
               (LET* ,(mapcar #'list restvars forms)
                 (TAGBODY
                   ,tag
                   ,(c-MAP-on-CARs-inner
                     #'(lambda (itemvars) `(FUNCALL ,funform ,@itemvars))
                     blockname
                     'NIL
                     restvars)
                   (SETQ ,@(shift-vars restvars))
                   (GO ,tag))))
             ,tempvar)))
      (c-GLOBAL-FUNCTION-CALL-form `(MAPC ,funform ,@(cddr *form*))))))

(defun c-MAPL ()
  (test-list *form* 3)
  (let ((funform (macroexpand-form (second *form*))))
    (if (inline-callable-function-p funform (length (cddr *form*)))
      (c-form
        (let* ((tempvar (gensym))
               (forms (cons tempvar (cdddr *form*)))
               (blockname (gensym))
               (restvars (gensym-list forms))
               (tag (gensym)))
          `(LET ((,tempvar ,(third *form*)))
             (BLOCK ,blockname
               (LET* ,(mapcar #'list restvars forms)
                 (TAGBODY
                   ,tag
                   (IF (OR ,@(mapcar #'(lambda (restvar) `(ENDP ,restvar))
                                     restvars))
                     (RETURN-FROM ,blockname))
                   (FUNCALL ,funform ,@restvars)
                   (SETQ ,@(shift-vars restvars))
                   (GO ,tag))))
             ,tempvar)))
      (c-GLOBAL-FUNCTION-CALL-form `(MAPL ,funform ,@(cddr *form*))))))

(defun c-MAPCAR ()
  (test-list *form* 3)
  (if (null *for-value*)
    (let ((*form* `(MAPC ,@(cdr *form*)))) (c-MAPC))
    (let ((funform (macroexpand-form (second *form*)))
          (forms (cddr *form*)))
      (if (inline-callable-function-p funform (length forms))
        (c-form (c-MAP-on-CARs 'CONS funform forms))
        (c-GLOBAL-FUNCTION-CALL-form `(MAPCAR ,funform ,@forms))))))

(defun c-MAPLIST ()
  (test-list *form* 3)
  (if (null *for-value*)
    (let ((*form* `(MAPL ,@(cdr *form*)))) (c-MAPL))
    (let ((funform (macroexpand-form (second *form*)))
          (forms (cddr *form*)))
      (if (inline-callable-function-p funform (length forms))
        (c-form (c-MAP-on-LISTs 'CONS funform forms))
        (c-GLOBAL-FUNCTION-CALL-form `(MAPLIST ,funform ,@forms))))))

(defun c-MAPCAN ()
  (test-list *form* 3)
  (let ((funform (macroexpand-form (second *form*)))
        (forms (cddr *form*)))
    (if (inline-callable-function-p funform (length forms))
      (c-form (c-MAP-on-CARs 'NCONC funform forms))
      (c-GLOBAL-FUNCTION-CALL-form `(MAPCAN ,funform ,@forms)))))

(defun c-MAPCON ()
  (test-list *form* 3)
  (let ((funform (macroexpand-form (second *form*)))
        (forms (cddr *form*)))
    (if (inline-callable-function-p funform (length forms))
      (c-form (c-MAP-on-LISTs 'NCONC funform forms))
      (c-GLOBAL-FUNCTION-CALL-form `(MAPCON ,funform ,@forms)))))

(defun c-MAPCAP ()
  (test-list *form* 3)
  (if (null *for-value*)
    (let ((*form* `(MAPC ,@(cdr *form*)))) (c-MAPC))
    (let ((funform (macroexpand-form (second *form*)))
          (forms (cddr *form*)))
      (if (inline-callable-function-p funform (length forms))
        (c-form (c-MAP-on-CARs 'APPEND funform forms))
        (c-GLOBAL-FUNCTION-CALL-form `(MAPCAP ,funform ,@forms))))))

(defun c-MAPLAP ()
  (test-list *form* 3)
  (if (null *for-value*)
    (let ((*form* `(MAPL ,@(cdr *form*)))) (c-MAPL))
    (let ((funform (macroexpand-form (second *form*)))
          (forms (cddr *form*)))
      (if (inline-callable-function-p funform (length forms))
        (c-form (c-MAP-on-LISTs 'APPEND funform forms))
        (c-GLOBAL-FUNCTION-CALL-form `(MAPLAP ,funform ,@forms))))))

;; c-TYPEP cf. TYPEP in type.lisp
(defun c-TYPEP () ; cf. TYPEP in type.lisp
  (test-list *form* 3 4)
  (let ((objform (second *form*))
        (typeform (macroexpand-form (third *form*))))
    (when (c-constantp typeform)
      (let ((type (c-constant-value typeform)) h)
        (cond ((symbolp type)
                (cond ; Test for Property TYPE-SYMBOL:
                      ((setq h (assoc type c-typep-alist1))
                        (setq h (cdr h))
                        (return-from c-TYPEP
                          (c-GLOBAL-FUNCTION-CALL-form `(,h ,objform))))
                      ((setq h (assoc type c-typep-alist2))
                        (setq h (cdr h))
                        (return-from c-TYPEP
                          (let ((*form* `(,h ,objform)))
                            (c-FUNCALL-INLINE
                              (symbol-suffix '#:TYPEP (symbol-name type))
                              (list objform)
                              nil
                              h
                              nil))))
                      ; Test for Property TYPE-LIST:
                      ((setq h (assoc type c-typep-alist3))
                        (setq h (cdr h))
                        (let* ((objvar (gensym))
                               (testform (funcall h objvar))
                               (lambdabody `((,objvar) ,testform)))
                          (return-from c-TYPEP
                            (let ((*form* `((lambda ,@lambdabody) ,objform)))
                              (c-FUNCALL-INLINE
                                (symbol-suffix '#:TYPEP (symbol-name type))
                                (list objform)
                                nil
                                lambdabody
                                nil)))))
                      ((setq h (get type 'SYS::DEFTYPE-EXPANDER))
                        (return-from c-TYPEP
                          (c-form `(TYPEP ,objform
                                    ',(funcall h (list type))))))
                      ((setq h (get type 'SYS::DEFSTRUCT-DESCRIPTION))
                        (return-from c-TYPEP
                          (c-form (sys::ds-typep-expansion objform type h))))
                      ((and (setq h (get type 'CLOS::CLOSCLASS))
                            (clos::defined-class-p h)
                            (eq (clos:class-name h) type))
                        (return-from c-TYPEP
                          (c-form
                            (if (clos::structure-class-p h)
                              ;; %STRUCTURE-TYPE-P is a little more efficient
                              ;; than CLOS::TYPEP-CLASS, so prefer it.
                              `(%STRUCTURE-TYPE-P ',type ,objform)
                              `(CLOS::TYPEP-CLASS ,objform
                                 (LOAD-TIME-VALUE (CLOS:FIND-CLASS ',type)))))))))
              ((and (consp type) (symbolp (first type)))
                (catch 'c-TYPEP
                  (cond ((and (eq (first type) 'SATISFIES)
                              (eql (length type) 2))
                         (let ((fun (second type)))
                           (unless (symbolp (second type))
                             (c-warn (TEXT "~S: argument to SATISFIES must be a symbol: ~S")
                                     'typep (second type))
                              (throw 'c-TYPEP nil))
                            (return-from c-TYPEP
                              (c-GLOBAL-FUNCTION-CALL-form `(,fun ,objform)))))
                        ((eq (first type) 'MEMBER)
                          (return-from c-TYPEP
                            (let ((*form* `(CASE ,objform
                                             (,(rest type) T) (t NIL))))
                              (c-CASE))))
                        ((and (eq (first type) 'EQL) (eql (length type) 2))
                          (return-from c-TYPEP
                            (let ((*form* `(EQL ,objform ',(second type))))
                              (c-EQL))))
                        ((and (eq (first type) 'NOT) (eql (length type) 2))
                          (return-from c-TYPEP
                            (c-GLOBAL-FUNCTION-CALL-form
                              `(NOT (TYPEP ,objform ',(second type))))))
                        ((or (eq (first type) 'AND) (eq (first type) 'OR))
                          (return-from c-TYPEP
                            (c-form
                              (let ((objvar (gensym)))
                                `(LET ((,objvar ,objform))
                                   (,(first type) ; AND or OR
                                    ,@(mapcar #'(lambda (typei)
                                                  `(TYPEP ,objvar ',typei))
                                              (rest type))))))))
                        ((setq h (assoc (first type) c-typep-alist3))
                          (setq h (cdr h))
                          (let* ((objvar (gensym))
                                 (testform (apply h objvar (rest type)))
                                 (lambdabody `((,objvar) ,testform)))
                            (return-from c-TYPEP
                              (let ((*form* `((lambda ,@lambdabody) ,objform)))
                                (c-FUNCALL-INLINE
                                  (symbol-suffix '#:TYPEP
                                                 (symbol-name (first type)))
                                  (list objform)
                                  nil
                                  lambdabody
                                  nil))))))))
              ((clos::defined-class-p type)
               (return-from c-TYPEP
                 (c-form `(CLOS::TYPEP-CLASS ,objform
                            ,(if (eq (get (clos:class-name type) 'CLOS::CLOSCLASS) type)
                               `(LOAD-TIME-VALUE (CLOS:FIND-CLASS ',(clos:class-name type)))
                               typeform)))))
              ((clos::eql-specializer-p type)
               (return-from c-TYPEP
                 (let ((*form* `(EQL ,objform ',(clos::eql-specializer-singleton type))))
                   (c-EQL))))
              ;; ((sys::encodingp type) ...) ; not worth optimizing
              )))
    (c-GLOBAL-FUNCTION-CALL-form
      `(TYPEP ,objform ,typeform ,@(cdddr *form*)))))

;; c-FORMAT cf. FORMAT in format.lisp
(defun c-FORMAT ()
  (test-list *form* 3)
  ;; Give a warning for the common error of forgotten destination.
  (let ((destination (second *form*)))
    (when (c-constantp destination)
      (setq destination (c-constant-value destination))
      (unless (or (null destination) (eq destination 'T)
                  (streamp destination)
                  (and (stringp destination)
                       (array-has-fill-pointer-p destination)))
        (c-warn (TEXT "The ~S destination is invalid (not NIL or T or a stream or a string with fill-pointer): ~S")
                (car *form*) destination)))
    (if (and (stringp (third *form*)) (not (fenv-search 'FORMATTER)))
      ;; precompile the format-string at compile-time.
      (cond ((eq destination t) ; avoid calling FORMAT altogether
             (c-GLOBAL-FUNCTION-CALL-form
              `(funcall (FORMATTER ,(third *form*)) *standard-output*
                        ,@(cdddr *form*))))
            ((eq destination nil) ; avoid calling FORMAT altogether
             (c-form ; we do not need full WITH-OUTPUT-TO-STRING here
              `(let ((<format-stream> (make-string-output-stream)))
                 (funcall (FORMATTER ,(third *form*)) <format-stream>
                          ,@(cdddr *form*))
                 (get-output-stream-string <format-stream>))))
            ((c-GLOBAL-FUNCTION-CALL-form
              `(FORMAT ,(second *form*) (FORMATTER ,(third *form*))
                       ,@(cdddr *form*)))))
      (c-GLOBAL-FUNCTION-CALL 'FORMAT))))

;; c-NTH for NTH; c-SETNTH for (SETF NTH)==SYSTEM::%SETNTH
;; mostly for (defstruct (foo (:type list))) accessors
(macrolet ((simple-index-p (ival func)
             `(and ,ival (or (integerp ,ival)
                             (c-warn
                              (TEXT "~S: index should be an integer, not ~S")
                              ,func ,ival))
                   (<= 0 ,ival 9))))
(defun c-NTH ()
  (test-list *form* 3 3)
  (let* ((index (macroexpand-form (second *form*)))
         (list (macroexpand-form (third *form*)))
         (i-val (and (c-constantp index) (c-constant-value index))))
    (if (simple-index-p i-val 'NTH)
      (c-GLOBAL-FUNCTION-CALL-form
        `(,(svref #(FIRST SECOND THIRD FOURTH FIFTH SIXTH SEVENTH EIGHTH
                    NINTH TENTH) i-val)
          ,list))
      (c-GLOBAL-FUNCTION-CALL-form `(NTH ,index ,list)))))
(defun c-SETNTH ()
  (test-list *form* 4 4)
  (let* ((index (macroexpand-form (second *form*)))
         (list (macroexpand-form (third *form*)))
         (value (macroexpand-form (fourth *form*)))
         (i-val (and (c-constantp index) (c-constant-value index))))
    (if (simple-index-p i-val '(SETF NTH))
      (c-form
        `(SETF (,(svref #(FIRST SECOND THIRD FOURTH FIFTH SIXTH SEVENTH
                          EIGHTH NINTH TENTH) i-val) ,list) ,value))
      (c-GLOBAL-FUNCTION-CALL-form `(SYSTEM::%SETNTH ,index ,list ,value)))))
)

;; Tests whether a form is declared to return a list.
(defun declared-list-form-p (form)
  ; For the moment, we don't propagate types, therefore the user has to
  ; write THE explicitly.
  (and (consp form) (eq (first form) 'THE)
       (consp (rest form)) (memq (second form) '(LIST CONS))
       (consp (cddr form)) (null (cdddr form))))

(defun c-MAP ()
  (test-list *form* 4)
  (let ((restype-form (second *form*))
        (fun-form (macroexpand-form (third *form*)))
        (forms (cdddr *form*)))
    (if (and (c-constantp restype-form)
             (memq (c-constant-value restype-form) '(NIL LIST)) ; restype is NIL or LIST
             (every #'declared-list-form-p forms) ; all sequences are lists
             (inline-callable-function-p fun-form (length forms)))
      (ecase (c-constant-value restype-form)
        ((NIL) ; like MAPC
         (c-form
           (let ((blockname (gensym))
                 (restvars (gensym-list forms))
                 (tag (gensym)))
             `(BLOCK ,blockname
                (LET* ,(mapcar #'list restvars forms)
                  (TAGBODY
                    ,tag
                    ,(c-MAP-on-CARs-inner
                      #'(lambda (itemvars) `(FUNCALL ,fun-form ,@itemvars))
                      blockname
                      'NIL
                      restvars)
                    (SETQ ,@(shift-vars restvars))
                    (GO ,tag)))))))
         ((LIST) ; like MAPCAR
          (c-form (c-MAP-on-CARs 'CONS fun-form forms))))
      (c-GLOBAL-FUNCTION-CALL-form `(MAP ,restype-form ,fun-form ,@forms)))))

(defun c-MAP-INTO ()
  (test-list *form* 3)
  (let ((res-form (second *form*))
        (fun-form (macroexpand-form (third *form*)))
        (forms (cdddr *form*)))
    (if (and (declared-list-form-p res-form)
             (every #'declared-list-form-p forms) ; all sequences are lists
             (inline-callable-function-p fun-form (length forms)))
      (c-form
        (let ((blockname (gensym))
              (resultvar (gensym))
              (resrestvar (gensym))
              (restvars (gensym-list forms))
              (tag (gensym)))
          `(LET* ((,resultvar ,res-form)
                  (,resrestvar ,resultvar))
             (BLOCK ,blockname
               (LET* ,(mapcar #'list restvars forms)
                 (TAGBODY
                   ,tag
                   (IF (ENDP ,resrestvar)
                     (RETURN-FROM ,blockname NIL)
                     ,(c-MAP-on-CARs-inner
                       #'(lambda (itemvars)
                           `(SETF (CAR ,resrestvar) (FUNCALL ,fun-form ,@itemvars)))
                       blockname
                       'NIL
                       restvars))
                    (SETQ ,@(shift-vars (cons resrestvar restvars)))
                    (GO ,tag))))
             ,resultvar)))
      (c-GLOBAL-FUNCTION-CALL-form `(MAP-INTO ,res-form ,fun-form ,@forms)))))

(defun c-SOME ()
  (test-list *form* 3)
  (let ((pred-form (macroexpand-form (second *form*)))
        (forms (cddr *form*)))
    (if (inlinable-function-operation-form-p pred-form 'COMPLEMENT)
      ;; (SOME (COMPLEMENT fn) ...) --> (NOTEVERY fn ...)
      (c-form `(NOTEVERY ,(second pred-form) ,@forms))
      (if (and (every #'declared-list-form-p forms) ; all sequences are lists
               (inline-callable-function-p pred-form (length forms)))
        (c-form
          (let ((restvars (gensym-list forms))
                (blockname (gensym))
                (tag (gensym))
                (tmp (gensym)))
            `(LET* ,(mapcar #'list restvars forms)
               (BLOCK ,blockname
                 (TAGBODY
                   ,tag
                   ,(c-MAP-on-CARs-inner
                     #'(lambda (itemvars)
                         `(LET ((,tmp (FUNCALL ,pred-form ,@itemvars)))
                            (IF ,tmp (RETURN-FROM ,blockname ,tmp))))
                     blockname
                     'NIL
                     restvars)
                   (SETQ ,@(shift-vars restvars))
                   (GO ,tag))))))
        (c-GLOBAL-FUNCTION-CALL-form (list* 'SOME pred-form forms))))))

(defun c-EVERY ()
  (test-list *form* 3)
  (let ((pred-form (macroexpand-form (second *form*)))
        (forms (cddr *form*)))
    (if (inlinable-function-operation-form-p pred-form 'COMPLEMENT)
      ;; (EVERY (COMPLEMENT fn) ...) --> (NOTANY fn ...)
      (c-form `(NOTANY ,(second pred-form) ,@forms))
      (if (and (every #'declared-list-form-p forms) ; all sequences are lists
               (inline-callable-function-p pred-form (length forms)))
        (c-form
          (let ((restvars (gensym-list forms))
                (blockname (gensym))
                (tag (gensym)))
            `(LET* ,(mapcar #'list restvars forms)
               (BLOCK ,blockname
                 (TAGBODY
                   ,tag
                   ,(c-MAP-on-CARs-inner
                     #'(lambda (itemvars)
                         `(IF (NOT (FUNCALL ,pred-form ,@itemvars))
                            (RETURN-FROM ,blockname NIL)))
                     blockname
                     'T
                     restvars)
                   (SETQ ,@(shift-vars restvars))
                   (GO ,tag))))))
        (c-GLOBAL-FUNCTION-CALL-form (list* 'EVERY pred-form forms))))))

(defun c-NOTANY ()
  (test-list *form* 3)
  (let ((pred-form (macroexpand-form (second *form*)))
        (forms (cddr *form*)))
    (if (inlinable-function-operation-form-p pred-form 'COMPLEMENT)
      ;; (NOTANY (COMPLEMENT fn) ...) --> (EVERY fn ...)
      (c-form `(EVERY ,(second pred-form) ,@forms))
      (if (and (every #'declared-list-form-p forms) ; all sequences are lists
               (inline-callable-function-p pred-form (length forms)))
        (c-form
          (let ((restvars (gensym-list forms))
                (blockname (gensym))
                (tag (gensym)))
            `(LET* ,(mapcar #'list restvars forms)
               (BLOCK ,blockname
                 (TAGBODY
                   ,tag
                   ,(c-MAP-on-CARs-inner
                     #'(lambda (itemvars)
                         `(IF (FUNCALL ,pred-form ,@itemvars)
                            (RETURN-FROM ,blockname NIL)))
                     blockname
                     'T
                     restvars)
                   (SETQ ,@(shift-vars restvars))
                   (GO ,tag))))))
        (c-GLOBAL-FUNCTION-CALL-form (list* 'NOTANY pred-form forms))))))

(defun c-NOTEVERY ()
  (test-list *form* 3)
  (let ((pred-form (macroexpand-form (second *form*)))
        (forms (cddr *form*)))
    (if (inlinable-function-operation-form-p pred-form 'COMPLEMENT)
      ;; (NOTEVERY (COMPLEMENT fn) ...) --> (AND (SOME fn ...) T)
      (c-form `(AND (SOME ,(second pred-form) ,@forms) 'T))
      (if (and (every #'declared-list-form-p forms) ; all sequences are lists
               (inline-callable-function-p pred-form (length forms)))
        (c-form
          (let ((restvars (gensym-list forms))
                (blockname (gensym))
                (tag (gensym)))
            `(LET* ,(mapcar #'list restvars forms)
               (BLOCK ,blockname
                 (TAGBODY
                   ,tag
                   ,(c-MAP-on-CARs-inner
                     #'(lambda (itemvars)
                         `(IF (NOT (FUNCALL ,pred-form ,@itemvars))
                            (RETURN-FROM ,blockname T)))
                     blockname
                     'NIL
                     restvars)
                   (SETQ ,@(shift-vars restvars))
                   (GO ,tag))))))
        (c-GLOBAL-FUNCTION-CALL-form (list* 'NOTEVERY pred-form forms))))))

(defun c-REMOVE-IF (&optional inverted)
  (test-list *form* 3)
  (let ((pred-form (macroexpand-form (second *form*))))
    (if (inlinable-function-operation-form-p pred-form 'COMPLEMENT)
      ;; (REMOVE-IF (COMPLEMENT fn) ...) --> (REMOVE-IF-NOT fn ...)
      ;; (REMOVE-IF-NOT (COMPLEMENT fn) ...) --> (REMOVE-IF fn ...)
      (c-form (list* (if inverted 'REMOVE-IF 'REMOVE-IF-NOT) (second pred-form)
                     (cddr *form*)))
      (if (and (null (cdddr *form*)) ; no keyword arguments
               (declared-list-form-p (third *form*)) ; sequence a list
               (inline-callable-function-p pred-form 1))
        (c-form
          (let ((result1var (gensym))
                (result2var (gensym))
                (listvar (gensym))
                (starttag (gensym))
                (endtag (gensym)))
            `(LET* ((,result1var NIL)
                    (,result2var ,(third *form*))
                    (,listvar ,result2var))
               (DECLARE (LIST ,listvar))
               (TAGBODY
                 ,starttag
                 (IF (ENDP ,listvar) (GO ,endtag))
                 (IF ,(let ((test `(FUNCALL ,pred-form (CAR ,listvar))))
                        (when inverted (setq test `(NOT ,test)))
                        test)
                   (PROGN
                     (SETQ ,result1var (NRECONC (LDIFF ,result2var ,listvar) ,result1var))
                     (SETQ ,result2var (SETQ ,listvar (CDR ,listvar))))
                   (SETQ ,listvar (CDR ,listvar)))
                 (GO ,starttag)
                 ,endtag)
               (NRECONC ,result1var ,result2var))))
        (c-GLOBAL-FUNCTION-CALL-form
          (list* (if inverted 'REMOVE-IF-NOT 'REMOVE-IF) pred-form
                 (cddr *form*)))))))
(defun c-REMOVE-IF-NOT ()
  (c-REMOVE-IF t))

(defun c-DELETE-IF (&optional inverted)
  (test-list *form* 3)
  (let ((pred-form (macroexpand-form (second *form*))))
    (if (inlinable-function-operation-form-p pred-form 'COMPLEMENT)
      ;; (DELETE-IF (COMPLEMENT fn) ...) --> (DELETE-IF-NOT fn ...)
      ;; (DELETE-IF-NOT (COMPLEMENT fn) ...) --> (DELETE-IF fn ...)
      (c-form (list* (if inverted 'DELETE-IF 'DELETE-IF-NOT) (second pred-form)
                     (cddr *form*)))
      (if (and (null (cdddr *form*)) ; no keyword arguments
               (declared-list-form-p (third *form*)) ; sequence a list
               (inline-callable-function-p pred-form 1))
        (c-form
          (let ((resultvar (gensym))
                (lastvar (gensym))
                (listvar (gensym))
                (starttag (gensym))
                (endtag (gensym)))
            `(LET* ((,resultvar ,(third *form*))
                    (,lastvar NIL)
                    (,listvar ,resultvar))
               (DECLARE (LIST ,listvar))
               (TAGBODY
                 ,starttag
                 (IF (ENDP ,listvar) (GO ,endtag))
                 (IF ,(let ((test `(FUNCALL ,pred-form (CAR ,listvar))))
                        (when inverted (setq test `(NOT ,test)))
                        test)
                   (IF ,lastvar
                     (SETF (CDR ,lastvar) (SETQ ,listvar (CDR ,listvar)))
                     (SETQ ,resultvar (SETQ ,listvar (CDR ,listvar))))
                   (PROGN
                     (SETQ ,lastvar ,listvar)
                     (SETQ ,listvar (CDR ,listvar))))
                 (GO ,starttag)
                 ,endtag)
               ,resultvar)))
        (c-GLOBAL-FUNCTION-CALL-form
          (list* (if inverted 'DELETE-IF-NOT 'DELETE-IF) pred-form
                 (cddr *form*)))))))
(defun c-DELETE-IF-NOT ()
  (c-DELETE-IF t))

(defun c-SUBSTITUTE-IF (&optional inverted)
  (test-list *form* 4)
  (let ((pred-form (macroexpand-form (third *form*))))
    (if (inlinable-function-operation-form-p pred-form 'COMPLEMENT)
      ;; (SUBSTITUTE-IF y (COMPLEMENT fn) ...) --> (SUBSTITUTE-IF-NOT y fn ...)
      ;; (SUBSTITUTE-IF-NOT y (COMPLEMENT fn) ...) --> (SUBSTITUTE-IF y fn ...)
      (c-form (list* (if inverted 'SUBSTITUTE-IF 'SUBSTITUTE-IF-NOT)
                     (second *form*) (second pred-form) (cdddr *form*)))
      (if (and (null (cddddr *form*)) ; no keyword arguments
               (declared-list-form-p (fourth *form*)) ; sequence a list
               (inline-callable-function-p pred-form 1))
        (c-form
          (let ((newitemvar (gensym))
                (result1var (gensym))
                (result2var (gensym))
                (listvar (gensym))
                (starttag (gensym))
                (endtag (gensym)))
            `(LET* ((,newitemvar ,(second *form*))
                    (,result1var NIL)
                    (,result2var ,(fourth *form*))
                    (,listvar ,result2var))
               (DECLARE (LIST ,listvar))
               (TAGBODY
                 ,starttag
                 (IF (ENDP ,listvar) (GO ,endtag))
                 (IF ,(let ((test `(FUNCALL ,pred-form (CAR ,listvar))))
                        (when inverted (setq test `(NOT ,test)))
                        test)
                   (PROGN
                     (SETQ ,result1var (CONS ,newitemvar (NRECONC (LDIFF ,result2var ,listvar) ,result1var)))
                     (SETQ ,result2var (SETQ ,listvar (CDR ,listvar))))
                   (SETQ ,listvar (CDR ,listvar)))
                 (GO ,starttag)
                 ,endtag)
               (NRECONC ,result1var ,result2var))))
        (c-GLOBAL-FUNCTION-CALL-form
          (list* (if inverted 'SUBSTITUTE-IF-NOT 'SUBSTITUTE-IF)
                 (second *form*) pred-form (cdddr *form*)))))))
(defun c-SUBSTITUTE-IF-NOT ()
  (c-SUBSTITUTE-IF t))

(defun c-NSUBSTITUTE-IF (&optional inverted)
  (test-list *form* 4)
  (let ((pred-form (macroexpand-form (third *form*))))
    (if (inlinable-function-operation-form-p pred-form 'COMPLEMENT)
      ;; (NSUBSTITUTE-IF y (COMPLEMENT fn) ...) --> (NSUBSTITUTE-IF-NOT y fn ...)
      ;; (NSUBSTITUTE-IF-NOT y (COMPLEMENT fn) ...) --> (NSUBSTITUTE-IF y fn ...)
      (c-form (list* (if inverted 'NSUBSTITUTE-IF 'NSUBSTITUTE-IF-NOT)
                     (second *form*) (second pred-form) (cdddr *form*)))
      (if (and (null (cddddr *form*)) ; no keyword arguments
               (declared-list-form-p (fourth *form*)) ; sequence a list
               (inline-callable-function-p pred-form 1))
        (c-form
          (let ((newitemvar (gensym))
                (wholevar (gensym))
                (listvar (gensym))
                (starttag (gensym))
                (endtag (gensym)))
            `(LET* ((,newitemvar ,(second *form*))
                    (,wholevar ,(fourth *form*))
                    (,listvar ,wholevar))
               (DECLARE (LIST ,listvar))
               (TAGBODY
                 ,starttag
                 (IF (ENDP ,listvar) (GO ,endtag))
                 (IF ,(let ((test `(FUNCALL ,pred-form (CAR ,listvar))))
                        (when inverted (setq test `(NOT ,test)))
                        test)
                   (SETF (CAR ,listvar) ,newitemvar))
                 (SETQ ,listvar (CDR ,listvar))
                 (GO ,starttag)
                 ,endtag)
               ,wholevar)))
        (c-GLOBAL-FUNCTION-CALL-form
          (list* (if inverted 'NSUBSTITUTE-IF-NOT 'NSUBSTITUTE-IF)
                 (second *form*) pred-form (cdddr *form*)))))))
(defun c-NSUBSTITUTE-IF-NOT ()
  (c-NSUBSTITUTE-IF t))

(defun c-FIND-IF (&optional inverted)
  (test-list *form* 3)
  (let ((pred-form (macroexpand-form (second *form*))))
    (if (inlinable-function-operation-form-p pred-form 'COMPLEMENT)
      ;; (FIND-IF (COMPLEMENT fn) ...) --> (FIND-IF-NOT fn ...)
      ;; (FIND-IF-NOT (COMPLEMENT fn) ...) --> (FIND-IF fn ...)
      (c-form (list* (if inverted 'FIND-IF 'FIND-IF-NOT) (second pred-form)
                     (cddr *form*)))
      (if (and (null (cdddr *form*)) ; no keyword arguments
               (declared-list-form-p (third *form*)) ; sequence a list
               (inline-callable-function-p pred-form 1))
        (c-form
          (let ((listvar (gensym))
                (blockname (gensym))
                (starttag (gensym))
                (elementvar (gensym)))
            `(LET ((,listvar ,(third *form*)))
               (DECLARE (LIST ,listvar))
               (BLOCK ,blockname
                 (TAGBODY
                   ,starttag
                   (IF (ENDP ,listvar) (RETURN-FROM ,blockname NIL))
                   (LET ((,elementvar (CAR ,listvar)))
                     (IF ,(let ((test `(FUNCALL ,pred-form ,elementvar)))
                            (when inverted (setq test `(NOT ,test)))
                            test)
                       (RETURN-FROM ,blockname ,elementvar)))
                   (SETQ ,listvar (CDR ,listvar))
                   (GO ,starttag))))))
        (c-GLOBAL-FUNCTION-CALL-form
          (list* (if inverted 'FIND-IF-NOT 'FIND-IF) pred-form
                 (cddr *form*)))))))
(defun c-FIND-IF-NOT ()
  (c-FIND-IF t))

(defun c-POSITION-IF (&optional inverted)
  (test-list *form* 3)
  (let ((pred-form (macroexpand-form (second *form*))))
    (if (inlinable-function-operation-form-p pred-form 'COMPLEMENT)
      ;; (POSITION-IF (COMPLEMENT fn) ...) --> (POSITION-IF-NOT fn ...)
      ;; (POSITION-IF-NOT (COMPLEMENT fn) ...) --> (POSITION-IF fn ...)
      (c-form (list* (if inverted 'POSITION-IF 'POSITION-IF-NOT)
                     (second pred-form) (cddr *form*)))
      (if (and (null (cdddr *form*)) ; no keyword arguments
               (declared-list-form-p (third *form*)) ; sequence a list
               (inline-callable-function-p pred-form 1))
        (c-form
          (let ((indexvar (gensym))
                (listvar (gensym))
                (blockname (gensym))
                (starttag (gensym)))
            `(LET* ((,indexvar 0) (,listvar ,(third *form*)))
               (DECLARE (INTEGER ,indexvar) (LIST ,listvar))
               (BLOCK ,blockname
                 (TAGBODY
                   ,starttag
                   (IF (ENDP ,listvar) (RETURN-FROM ,blockname NIL))
                   (IF ,(let ((test `(FUNCALL ,pred-form (CAR ,listvar))))
                          (when inverted (setq test `(NOT ,test)))
                          test)
                     (RETURN-FROM ,blockname ,indexvar))
                   (SETQ ,indexvar (+ ,indexvar 1))
                   (SETQ ,listvar (CDR ,listvar))
                   (GO ,starttag))))))
        (c-GLOBAL-FUNCTION-CALL-form
          (list* (if inverted 'POSITION-IF-NOT 'POSITION-IF) pred-form
                 (cddr *form*)))))))
(defun c-POSITION-IF-NOT ()
  (c-POSITION-IF t))

(defun c-COUNT-IF (&optional inverted)
  (test-list *form* 3)
  (let ((pred-form (macroexpand-form (second *form*))))
    (if (inlinable-function-operation-form-p pred-form 'COMPLEMENT)
      ;; (COUNT-IF (COMPLEMENT fn) ...) --> (COUNT-IF-NOT fn ...)
      ;; (COUNT-IF-NOT (COMPLEMENT fn) ...) --> (COUNT-IF fn ...)
      (c-form (list* (if inverted 'COUNT-IF 'COUNT-IF-NOT) (second pred-form)
                     (cddr *form*)))
      (if (and (null (cdddr *form*)) ; no keyword arguments
               (declared-list-form-p (third *form*)) ; sequence a list
               (inline-callable-function-p pred-form 1))
        (c-form
          (let ((countervar (gensym))
                (listvar (gensym))
                (starttag (gensym))
                (endtag (gensym)))
            `(LET* ((,countervar 0) (,listvar ,(third *form*)))
               (DECLARE (INTEGER ,countervar) (LIST ,listvar))
               (TAGBODY
                 ,starttag
                 (IF (ENDP ,listvar) (GO ,endtag))
                 (IF ,(let ((test `(FUNCALL ,pred-form (CAR ,listvar))))
                        (when inverted (setq test `(NOT ,test)))
                        test)
                   (SETQ ,countervar (+ ,countervar 1)))
                 (SETQ ,listvar (CDR ,listvar))
                 (GO ,starttag)
                 ,endtag)
               ,countervar)))
        (c-GLOBAL-FUNCTION-CALL-form
          (list* (if inverted 'COUNT-IF-NOT 'COUNT-IF) pred-form
                 (cddr *form*)))))))
(defun c-COUNT-IF-NOT ()
  (c-COUNT-IF t))

;; c-SUBST-IF, c-SUBST-IF-NOT etc.
(macrolet ((c-seqop (op n)
             (let ((op-if (intern (string-concat (string op) "-IF")
                                  *lisp-package*))
                   (op-if-not (intern (string-concat (string op) "-IF-NOT")
                                      *lisp-package*))
                   (c-op-if (intern (string-concat "C-" (string op) "-IF")))
                   (c-op-if-not (intern (string-concat "C-" (string op)
                                                       "-IF-NOT"))))
               `(progn
                  (defun ,c-op-if ()
                    (test-list *form* ,(+ 1 n))
                    (let ((pred-form (macroexpand-form
                                       ,(case n (2 `(second *form*))
                                                (3 `(third *form*))))))
                      (if (inlinable-function-operation-form-p pred-form
                                                               'COMPLEMENT)
                        ;; (op-if (complement fn) ...) --> (op-if-not fn ...)
                        (c-form ,(case n (2 `(list* ',op-if-not
                                              (second pred-form) (cddr *form*)))
                                         (3 `(list* ',op-if-not (second *form*)
                                              (second pred-form)
                                              (cdddr *form*)))))
                        (c-GLOBAL-FUNCTION-CALL-form
                          ,(case n (2 `(list* ',op-if pred-form (cddr *form*)))
                                   (3 `(list* ',op-if (second *form*) pred-form (cdddr *form*))))))))
                  (defun ,c-op-if-not ()
                    (test-list *form* ,(+ 1 n))
                    (let ((pred-form (macroexpand-form
                                       ,(case n (2 `(second *form*))
                                                (3 `(third *form*))))))
                      (if (inlinable-function-operation-form-p pred-form
                                                               'COMPLEMENT)
                        ;; (op-if-not (complement fn) ...) --> (op-if fn ...)
                        (c-form ,(case n (2 `(list* ',op-if (second pred-form)
                                              (cddr *form*)))
                                         (3 `(list* ',op-if (second *form*)
                                              (second pred-form)
                                              (cdddr *form*)))))
                        (c-GLOBAL-FUNCTION-CALL-form
                          ,(case n (2 `(list* ',op-if-not pred-form (cddr *form*)))
                                   (3 `(list* ',op-if-not (second *form*) pred-form (cdddr *form*))))))))))))

  (c-seqop SUBST 3)
  (c-seqop NSUBST 3)
  (c-seqop MEMBER 2)
  (c-seqop ASSOC 2)
  (c-seqop RASSOC 2)
)

;; :TEST (COMPLEMENT foo) ==> :TEST-NOT foo
;; :TEST-NOT (COMPLEMENT foo) ==> :TEST foo
(defun c-TEST/TEST-NOT ()
  (let ((fun (first *form*)))
    (multiple-value-bind (name req-num opt-num rest-p key-p keywords allow-p)
        (function-signature fun)
      (declare (ignore name rest-p key-p allow-p))
      ;; This optimization assumes opt-num = 0.
      (assert (and (= opt-num 0) (memq ':TEST keywords) (memq ':TEST-NOT keywords)))
      ;; Replace the first occurrence of a (:TEST testform) or
      ;; (:TEST-NOT testform), respectively.
      ;; Do nothing if the call will produce errors at runtime, such as if
      ;; :TEST and :TEST-NOT are both specified.
      ;; Do nothing if the optimization would modify the semantics, such as if
      ;; :TEST or :TEST-NOT is specified more than once.
      (let ((*form* *form*))
        (when (and (>= (length *form*) (+ 1 req-num))
                   (evenp (- (length *form*) (+ 1 req-num))))
          (let ((pos-TEST nil)
                (pos-TEST-NOT nil))
            (do* ((pos (+ 1 req-num) (+ pos 2))
                  (rest (nthcdr pos *form*) (cddr rest)))
                 ((endp rest))
              (let ((key-form (first rest)))
                (when (c-constantp key-form)
                  (let ((key (c-constant-value key-form)))
                    (case key
                      (:TEST (setq pos-TEST (if pos-TEST -1 pos)))
                      (:TEST-NOT (setq pos-TEST-NOT (if pos-TEST-NOT -1 pos))))))))
            (when (and (or pos-TEST pos-TEST-NOT)
                       (not (and pos-TEST pos-TEST-NOT)) ; both specified?
                       (not (eql pos-TEST -1)) ; duplicate :TEST?
                       (not (eql pos-TEST-NOT -1))) ; duplicate :TEST-NOT?
              (let* ((pos (or pos-TEST pos-TEST-NOT))
                     (testform (nth (+ pos 1) *form*)))
                (when (inlinable-function-operation-form-p testform 'COMPLEMENT)
                  (setq *form* (append (subseq *form* 0 pos)
                                       (list* (if pos-TEST ':TEST-NOT ':TEST)
                                              (second testform)
                                              (nthcdr (+ pos 2) *form*)))))))))
        (c-GLOBAL-FUNCTION-CALL fun)))))

;; Recognizes a constant byte specifier and returns it, or NIL.
(defun c-constant-byte-p (form)
  (cond ((c-constantp form)
         (setq form (c-constant-value form))
         (if (eq (type-of form) 'BYTE) form nil))
        ((and (consp form)
              (eq (first form) 'BYTE)
              (consp (cdr form))
              (typep (second form) '(AND (INTEGER 0 *) FIXNUM))
              (consp (cddr form))
              (typep (third form) '(AND (INTEGER 0 *) FIXNUM))
              (null (cdddr form))
              (not (fenv-search 'BYTE))
              (not (declared-notinline 'BYTE)))
         ;; no need to ignore errors, we have checked the arguments
         (byte (second form) (third form)))
        (t nil)))

(defun c-LDB ()
  (test-list *form* 3 3)
  (let* ((arg1-form (macroexpand-form (second *form*)))
         (arg1 (c-constant-byte-p arg1-form)))
    (if arg1
      ;; We optimize (ldb (byte size position) integer) only when position = 0,
      ;; because when position > 0, the expression
      ;; `(logand ,(1- (ash 1 size)) (ash integer ,(- position)))
      ;; is not better and causes more heap allocations than the original
      ;; expression. The expression
      ;; `(ash (logand ,(ash (1- (ash 1 size)) position) integer)
      ;;       ,(- position))
      ;; is even worse.
      ;; The "24" below is an arbitrary limit, to avoid huge integers in
      ;; the code [e.g. for (ldb (byte 30000 0) x)]. In particular, we
      ;; know that for size <= 24, (1- (ash 1 size)) is a posfixnum,
      ;; which makes the LOGAND operation particularly efficient.
      (if (and (= (byte-position arg1) 0) (<= (byte-size arg1) 24))
        (c-GLOBAL-FUNCTION-CALL-form
          `(LOGAND ,(1- (ash 1 (byte-size arg1))) ,(third *form*)))
        (c-GLOBAL-FUNCTION-CALL-form
          `(LDB (QUOTE ,arg1) ,(third *form*))))
      (c-GLOBAL-FUNCTION-CALL-form `(LDB ,arg1-form ,@(cddr *form*))))))

(defun c-LDB-TEST ()
  (test-list *form* 3 3)
  (let* ((arg1-form (macroexpand-form (second *form*)))
         (arg1 (c-constant-byte-p arg1-form)))
    (if arg1
      ;; The "24" below is an arbitrary limit, to avoid huge integers in
      ;; the code [e.g. for (ldb-test (byte 30000 0) x)].
      (if (<= (+ (byte-size arg1) (byte-position arg1)) 24)
        (c-GLOBAL-FUNCTION-CALL-form
         `(LOGTEST ,(ash (1- (ash 1 (byte-size arg1))) (byte-position arg1))
           ,(third *form*)))
        (c-GLOBAL-FUNCTION-CALL-form
          `(LDB-TEST (QUOTE ,arg1) ,(third *form*))))
      (c-GLOBAL-FUNCTION-CALL-form `(LDB-TEST ,arg1-form ,@(cddr *form*))))))

(defun c-MASK-FIELD ()
  (test-list *form* 3 3)
  (let* ((arg1-form (macroexpand-form (second *form*)))
         (arg1 (c-constant-byte-p arg1-form)))
    (if arg1
      ;; We know that for size+position <= 24, (ash (1- (ash 1 size)) position)
      ;; is a posfixnum, which makes the LOGAND operation more efficient than
      ;; the MASK-FIELD operation.
      (if (<= (+ (byte-size arg1) (byte-position arg1)) 24)
        (c-GLOBAL-FUNCTION-CALL-form
          `(LOGAND ,(ash (1- (ash 1 (byte-size arg1))) (byte-position arg1))
            ,(third *form*)))
        (c-GLOBAL-FUNCTION-CALL-form
          `(MASK-FIELD (QUOTE ,arg1) ,(third *form*))))
      (c-GLOBAL-FUNCTION-CALL-form `(MASK-FIELD ,arg1-form ,@(cddr *form*))))))

(defun c-DPB ()
  (test-list *form* 4 4)
  (let* ((arg2-form (macroexpand-form (third *form*)))
         (arg2 (c-constant-byte-p arg2-form)))
    (if arg2
      (c-GLOBAL-FUNCTION-CALL-form
        `(DPB ,(second *form*) (QUOTE ,arg2) ,(fourth *form*)))
      (c-GLOBAL-FUNCTION-CALL-form `(DPB ,(second *form*) ,arg2-form ,@(cdddr *form*))))))

(defun c-DEPOSIT-FIELD ()
  (test-list *form* 4 4)
  (let* ((arg2-form (macroexpand-form (third *form*)))
         (arg2 (c-constant-byte-p arg2-form)))
    (if arg2
      (c-GLOBAL-FUNCTION-CALL-form
        `(DEPOSIT-FIELD ,(second *form*) (QUOTE ,arg2) ,(fourth *form*)))
      (c-GLOBAL-FUNCTION-CALL-form `(DEPOSIT-FIELD ,(second *form*) ,arg2-form ,@(cdddr *form*))))))



;;;;****                     SECOND   PASS

;;; a table of pairs (fnode n).
;;; Each pair indicates, that in the 3rd Pass in Constant Nr. n of the
;;; functional Object of fnode the belonging fnode is to be replaced by the
;;; functional Object that was created by the fnode.
(defvar *fnode-fixup-table*)


;;; turns the ANODE-tree of fnode *func* into a functional object:
(defun pass2 (*func*)
  (when (anode-p (fnode-code *func*))
    ;; if Pass2 has not been executed yet,
    ;; only flatten, optimize and assemble the code
    (let ((code-list (compile-to-LAP)))
      ;; flatten code, split into pieces, optimize, turn into a list
      (when (fnode-gf-p *func*) ; convert CONSTs
        (setq code-list (CONST-to-LOADV code-list)))
      (let ((SPdepth (SP-depth code-list))) ; determine stack requirements
        (setq code-list (insert-combined-LAPs code-list))
        (create-fun-obj *func* (assemble-LAP code-list) SPdepth)))
    ;; do Pass2 on the sub-functions
    (dolist (x (fnode-Consts *func*)) (if (fnode-p x) (pass2 x)))))

#|
pass2 calls the 1st step.

After the 1. step the Code is divided into small part, with each part
stretching from a label to a jump (away from this part) (JMP, JMPCASE,
JMPCASE1-TRUE, JMPCASE1-FALSE, JMPHASH, RETURN-FROM, GO, RET, RETGF,
THROW, BARRIER). The parts are located (as a list in reversed order,
with the label as the last CDR) in vector *code-parts*.

  (symbol-value label) contains a list of the references of the label,
in the form:
 - Index in *code-parts*, if the reference is the corresponding jump
   (away from the part);
 - else opcode, where opcode is the command, in which the label occurs.
After the 1st step the code only contains Tags (Symbols) and Lists made of
Symbols and Numbers. Hence it is OK to work with SUBST and EQUAL.

The 1st step calls the 2nd step as soon as one part is completed.

Then, pass2 calls the 3rd step.  The 3rd step involves optimizations
that call further optimizations on success.

|#

#|
                             1st Step:
          Expansion of Code-Parts, Division of the Code in parts

changed:

 before                           after

 (CONST const)                    (CONST n const)
 (FCONST fnode)                   (CONST n), memorize Fixup for 3. Pass
 (BCONST block)                   (CONST n)
 (GCONST tagbody)                 (CONST n)
 (GET var venvc stackz)           (LOAD n) or (LOADI k1 k2 n)
                                  or (LOADC n m) or (LOADIC k1 k2 n m)
                                  or (LOADV k m) or (GETVALUE n)
                                  or (CONST n) or (CONST n const)
 (SET var venvc stackz)           (STORE n) or (STOREI k1 k2 n)
                                  or (STOREC n m) or (STOREIC k1 k2 n m)
                                  or (STOREV k m) or (SETVALUE n)
 (SETVALUE symbol)                (SETVALUE n)
 (GETVALUE symbol)                (GETVALUE n)
 (BIND const)                     (BIND n)
 (UNWIND stackz1 stackz2 for-value) a sequence of
                                  (SKIP n), (SKIPI k1 k2 n), (SKIPSP k1 k2),
                                  (VALUES0), (UNWIND-PROTECT-CLEANUP),
                                  (UNBIND1), (BLOCK-CLOSE), (TAGBODY-CLOSE)
 (UNWINDSP stackz1 stackz2)       a sequence of (SKIPSP k1 k2)
 (JMPIF label)                    (JMPCASE label new-label) new-label
 (JMPIFNOT label)                 (JMPCASE new-label label) new-label
 (JMPIF1 label)                   (JMPCASE1-TRUE label new-label) new-label
 (JMPIFNOT1 label)                (JMPCASE1-FALSE new-label label) new-label
 (JMPHASH test ((obj1 . label1) ... (objm . labelm)) label . labels)
                                  (JMPHASH n ht label . labels)
                                  with ht = Hash-Tabelle (obji -> labeli)
 (VENV venvc stackz)              (VENV) or (NIL)
                                  or (LOAD n) or (LOADI k1 k2 n)
 (COPY-CLOSURE fnode n)           (COPY-CLOSURE m n), memorize Fixup for Pass3
 (CALLP)                          discarded
 (CALL k fun)                     (CALL k n)
 (CALL0 fun)                      (CALL0 n)
 (CALL1 fun)                      (CALL1 n)
 (CALL2 fun)                      (CALL2 n)
 (FUNCALLP)                       (PUSH)
 (APPLYP)                         (PUSH)
 (JMPIFBOUNDP var venvc stackz label)
                                  (JMPIFBOUNDP n label)
 (BOUNDP var venvc stackz)        (BOUNDP n)
 (BLOCK-OPEN const label)         (BLOCK-OPEN n label)
 (RETURN-FROM const)              (RETURN-FROM n)
 (RETURN-FROM block)              (RETURN-FROM n)
 (RETURN-FROM block stackz)       (RETURN-FROM-I k1 k2 n)
 (TAGBODY-OPEN const label1 ... labelm)
                                  (TAGBODY-OPEN n label1 ... labelm)
 (GO const l)                     (GO n l)
 (GO tagbody (x . l))             (GO n l)
 (GO tagbody (x . l) stackz)      (GO-I k1 k2 n l)
 (HANDLER-OPEN const stackz label1 ... labelm)
                                  (HANDLER-OPEN n v k label1 ... labelm)
 unchanged:
 (NIL)
 (PUSH-NIL n)
 (T)
 (STORE n)
 (UNBIND1)
 (PROGV)
 (PUSH)
 (POP)
 (RET)
 (RETGF)
 (JMP label)
 (JSR m label)
 (BARRIER)
 (MAKE-VECTOR1&PUSH n)
 (CALLS1 n)
 (CALLS2 n)
 (CALLSR m n)
 (CALLC)
 (CALLCKEY)
 (FUNCALL n)
 (APPLY n)
 (PUSH-UNBOUND n)
 (UNLIST n m)
 (UNLIST* n m)
 (VALUES0)
 (VALUES1)
 (STACK-TO-MV n)
 (MV-TO-STACK)
 (NV-TO-STACK n)
 (MV-TO-LIST)
 (LIST-TO-MV)
 (MVCALLP)
 (MVCALL)
 (BLOCK-CLOSE)
 (TAGBODY-CLOSE-NIL)
 (TAGBODY-CLOSE)
 (CATCH-OPEN label)
 (CATCH-CLOSE)
 (THROW)
 (UNWIND-PROTECT-OPEN label)
 (UNWIND-PROTECT-NORMAL-EXIT)
 (UNWIND-PROTECT-CLOSE label)
 (UNWIND-PROTECT-CLEANUP)
 (HANDLER-BEGIN)
 (NOT)
 (EQ)
 (CAR denv)
 (CDR denv)
 (CONS)
 (ATOM)
 (CONSP)
 (SYMBOL-FUNCTION denv)
 (SVREF denv)
 (SVSET)
 (LIST n)
 (LIST* n)

New Operations:

 (JMP label boolvalue)            jump to label, boolvalue describes the 1.
                                  value: FALSE if =NIL, TRUE if /=NIL,
                                  NIL if unknown.

 (JMPCASE label1 label2)          jump to label1, if A0 /= NIL,
                                  resp. to label2, if A0 = NIL.

 (JMPCASE1-TRUE label1 label2)    if A0 /= NIL: jump to label1, 1 value.
                                  if A0 = NIL: jump to label2.

 (JMPCASE1-FALSE label1 label2)   if A0 /= NIL: jump to label1.
                                  if A0 = NIL: jump to label2, 1 value.

 (JMPTAIL m n label)              reduction of the Stack-Frame from n to m,
                                  then jump to label with undefined values.

|#

;; A Vector with Fill-Pointer that contains the code-parts:
(defvar *code-parts*)

;; A Vector of the same length with Fill-Pointer, that contains for each
;; code-part a "Position", where the part should be located finally (0 =
;; right at the beginning, higher values mean: shift further behind).
(defvar *code-positions*)

;; Registers a constant in (fnode-consts *func*) and returns its Index n.
;; value is the Value of the constant,
;; form is a Form with this value or NIL,
;; horizon = :value (then form = NIL) or :all or :form.
(defun value-form-index (value form ltv-form horizon &optional (func *func*))
  (let ((const-list (fnode-consts func))
        (forms-list (fnode-consts-forms func))
        (ltv-forms-list (fnode-consts-ltv-forms func))
        (n (fnode-Consts-Offset func)))
    (if (null const-list)
      (progn
        (setf (fnode-consts func) (list value))
        (setf (fnode-consts-forms func) (list form))
        (setf (fnode-consts-ltv-forms func) (list ltv-form))
        n)
      (loop
        (when (if (eq horizon ':form)
                (and (eql (car forms-list) form)
                     (let ((ltv1 (car ltv-forms-list)) (ltv2 ltv-form))
                       (or (null ltv1) (null ltv2) (eq ltv1 ltv2))))
                ;; When horizon = :value or :all, we will compare only value.
                (eql (car const-list) value))
          (return n))
        (incf n)
        (when (null (cdr const-list))
          (setf (cdr const-list) (list value))
          (setf (cdr forms-list) (list form))
          (setf (cdr ltv-forms-list) (list ltv-form))
          (return n))
        (setq const-list (cdr const-list))
        (setq forms-list (cdr forms-list))
        (setq ltv-forms-list (cdr ltv-forms-list))))))
(defun constvalue-index (value)
  (value-form-index value nil nil ':value))

;; searches a constant in (fnode-Keywords *func*) and in (fnode-Consts *func*),
;; possibly registers it in (fnode-Consts *func*) . Returns its Index n.
(defun kvalue-form-index (value form ltv-form horizon &optional (func *func*))
  (when (and (not (eq horizon ':form)) (symbolp value))
    ;; the search only pays off for Symbols (formerly: Keywords)
    (do ((n (fnode-Keyword-Offset func) (1+ n))
         (L (fnode-Keywords func) (cdr L)))
        ((null L))
      (if (eq (car L) value) (return-from kvalue-form-index n))))
  (value-form-index value form ltv-form horizon func))
(defun kconstvalue-index (value)
  (kvalue-form-index value nil nil ':value))
(defun const-index (const)
  (if (and *compiling-from-file* (not (eq (const-horizon const) ':value)))
    (kvalue-form-index (const-value const) (const-form const)
                       (const-ltv-form const) (const-horizon const))
    (kvalue-form-index (const-value const) nil nil ':value)))

;; (make-const-code const) returns the Code, that moves the value of the
;; constant as 1 value to A0 .
(defun make-const-code (const)
  (unless (eq (const-horizon const) ':form)
    (let ((value (const-value const)))
      (cond ((eq value 'nil) (return-from make-const-code '(NIL) ))
            ((eq value 't) (return-from make-const-code '(T) )))))
  `(CONST ,(const-index const) ,const))

;; (bconst-index block) returns the Index in FUNC,
;; where this Block is located.
(defun bconst-index (block &optional (func *func*))
  ;; (+ (fnode-Blocks-Offset func)
  ;;    (position block (fnode-Blocks func) :test #'eq))
  (do ((n (fnode-Blocks-Offset func) (1+ n))
       (L (fnode-Blocks func) (cdr L)))
      ((eq (car L) block) n)))

;; (gconst-index tagbody) returns the Index in FUNC,
;; where this Tagbody is located.
(defun gconst-index (tagbody &optional (func *func*))
  ;; (+ (fnode-Tagbodys-Offset func)
  ;;    (position tagbody (fnode-Tagbodys func) :test #'eq))
  (do ((n (fnode-Tagbodys-Offset func) (1+ n))
       (L (fnode-Tagbodys func) (cdr L)))
      ((eq (car L) tagbody) n)))

;;; (fconst-index fnode) returns the Index in FUNC, where this fnode is
;;; located in the constants. If necessary, it is inserted and noted in
;;; *fnode-fixup-table* .
(defun fconst-index (fnode &optional (func *func*))
  (if (member fnode (fnode-Consts func))
    (constvalue-index fnode)
    (let ((n (constvalue-index fnode)))
      (push (list func n) *fnode-fixup-table*)
      n)))

;; Auxiliary Variables for recursive call of traverse-anode:

;; the current code-part, a reversed list of instructions, that
;; ends with the Start-Label as last nthcdr.
(defvar *code-part*)

;; and its number (Index in *code-parts*)
(defvar *code-index*)

;; Flag, if "dead Code" (i.e. Code, that is not reachable)
(defvar *dead-code*)

;; For jump-optimization in traverse-anode: List of all already
;; executed Label-Substitutions ((old-label . new-label) ...)
(defvar *label-subst*)

;; The current value, interpreted as boolean value:
;; FALSE if =NIL, TRUE if /=NIL, NIL if unknown.
;; (no restriction on the number of values!)
(defvar *current-value*)

;; List of Variables/Constants, whose values match the current
;; (lexical Variables as VARIABLE-Structures, dynamic Variables as
;; Symbols, Constants as CONST-Structures with horizon = :value or :all).
;; If this list is non-empty, there is exactly 1 value.
(defvar *current-vars*)

;; Each Label (a Gensym-Symbol) has as value a list of all References
;; to label, either as Index i in *code-parts*, if it is
;; the jump (the end) of (aref *code-parts* i) , or
;; as instruction (of a list) in all other cases. If the Label
;; starts a code-part, at (get label 'code-part) the Index in
;; *code-part* of the code-part is written, that starts with this Label. At
;; (get label 'for-value) is specified, how many values have a meaning
;; for a possible jump to the Label (NIL/ONE/ALL).
;; An exception is the "Label" NIL , which represents the entry-point.

;; Substitutes all references to old-label with references to new-label.
(defun label-subst (old-label new-label)
  ;; change all references to old-label:
  (dolist (ref (symbol-value old-label))
    (nsubst new-label old-label
            (rest (if (integerp ref) (first (aref *code-parts* ref)) ref))))
  ;; and register as references to new-label:
  (setf (symbol-value new-label)
        (nconc (symbol-value old-label) (symbol-value new-label)))
  (setf (symbol-value old-label) '())
  ;; no code-part starts with old-label:
  (remprop old-label 'code-part))

;; end current code-part and start a new code-part:
(defun finish-code-part ()
  ;; simplify the current code-part:
  (simplify *code-part*)
  ;; store *code-part* in *code-parts* :
  (vector-push-extend *code-part* *code-parts*)
  (vector-push-extend (incf *code-index*) *code-positions*))

;; emit a jump to Label label.
;; Thus a new code-part is started.
(defun emit-jmp (label)
  ;; with a jump:
  (push `(JMP ,label ,*current-value*) *code-part*)
  (push *code-index* (symbol-value label))
  (finish-code-part))

;; traverses through the Code of an Anode, expands the Code and
;; continues building *code-part* . Adjusts the Variables
;; *current-value* etc. accordingly.
(defun traverse-anode (code)
  (dolist (item code)
    (if (atom item)
      (cond ((symbolp item) ; Label
             (if *dead-code*
               ;; Code can be reached, so it is from now on not dead anymore
               (setq *dead-code* nil)
               (if (symbolp *code-part*)
                 ;; move Label item immediately to Label *code-part*
                 ;; -> can be identified
                 (let ((old-label *code-part*) (new-label item))
                   ;; substitute *code-parts* -> item
                   (label-subst old-label new-label)
                   (setq *label-subst*
                     (acons old-label new-label
                       (nsubst new-label old-label *label-subst*))))
                 ;; Label amid the code-part -> finish current code-part
                 (emit-jmp item)))
             ;; now the current code-part really starts,
             ;; with the label item:
             (setq *code-part* item)
             (setf (get item 'code-part) (fill-pointer *code-parts*))
             ;; jumps to this Label can still occur, so we do not know,
             ;; what A0 contains:
             (setq *current-value* nil *current-vars* '()))
            ((anode-p item)     ; Anode -> recursive
             (traverse-anode (anode-code item)))
            (t (compiler-error 'traverse-anode "ITEM")))
      ;; item is a normal instruction
      (unless *dead-code* ; only reachable code has to be processed
        (nsublis *label-subst* (rest item)) ; perform substitutions so far
        (case (first item)
          (CONST
           (let ((const (second item)))
             (if (eq (const-horizon const) ':form)
               (progn
                 (push (make-const-code const) *code-part*)
                 (setq *current-value* nil *current-vars* '()))
               (let ((cv (const-value const)))
                 (unless ; is (CONST cv) already contained in *current-vars*?
                     (dolist (v *current-vars* nil)
                       (when (and (const-p v) (eq (const-value-safe v) cv))
                         (return t)))
                   (push (make-const-code const) *code-part*)
                   (setq *current-value* (if (null cv) 'FALSE 'TRUE)
                         *current-vars* (list const)))))))
          (FCONST
           (push `(CONST ,(fconst-index (second item))) *code-part*)
           (setq *current-value* 'TRUE *current-vars* '()))
          (BCONST
           (push `(CONST ,(bconst-index (second item))) *code-part*)
           (setq *current-value* 'TRUE *current-vars* '()))
          (GCONST
           (push `(CONST ,(gconst-index (second item))) *code-part*)
           (setq *current-value* 'TRUE *current-vars* '()))
          (GET
           (let ((var (second item))
                 (venvc (third item))
                 (stackz (fourth item)))
             (unless (memq var *current-vars*)
               ;; already the current value = var ?
               (push
                (if (var-constantp var)
                  (let* ((const (var-constant var))
                         (val (const-value-safe const)))
                    (setq *current-value* (if (null val) 'FALSE 'TRUE))
                    (if (fnode-p val)
                      ;; FNODEs as values can (almost) solely
                      ;; originate from LABELS
                      `(CONST ,(fconst-index val))
                      (make-const-code const)))
                  (progn
                    (setq *current-value* nil)
                    (if (var-specialp var)
                      `(GETVALUE ,(kconstvalue-index
                                   (setq var (var-name var))))
                      (if (var-closurep var)
                        (multiple-value-bind (k n m)
                            (access-in-closure var venvc stackz)
                          (if n
                            (if k
                              `(LOADIC ,(car k) ,(cdr k) ,n ,m)
                              `(LOADC ,n ,m))
                            `(LOADV ,k ,(1+ m))))
                        ;; lexical and in Stack, so in the same function
                        (multiple-value-bind (k n)
                            (access-in-stack stackz (var-stackz var))
                          (if k
                            `(LOADI ,(car k) ,(cdr k) ,n)
                            `(LOAD ,n) ))))))
                *code-part*)
               (setq *current-vars* (list var)))))
          (SET
           (let ((var (second item))
                 (venvc (third item))
                 (stackz (fourth item)))
             (unless (memq var *current-vars*)
               ;; already the current value = var ?
               (push
                (if (var-specialp var)
                  `(SETVALUE ,(kconstvalue-index (setq var (var-name var))))
                  (if (var-closurep var)
                    (multiple-value-bind (k n m)
                        (access-in-closure var venvc stackz)
                      (if n
                        (if k
                          `(STOREIC ,(car k) ,(cdr k) ,n ,m)
                          `(STOREC ,n ,m))
                        `(STOREV ,k ,(1+ m))))
                    ;; lexical and in Stack, so in the same function
                    (multiple-value-bind (k n)
                        (access-in-stack stackz (var-stackz var))
                      (if k `(STOREI ,(car k) ,(cdr k) ,n) `(STORE ,n) ))))
                *code-part*)
               (push var *current-vars*)))) ; *current-value* is unchanged
          (GETVALUE
           (let ((symbol (second item)))
             (unless (memq symbol *current-vars*)
               (push `(GETVALUE ,(kconstvalue-index symbol)) *code-part*)
               (setq *current-value* nil *current-vars* (list symbol)))))
          (SETVALUE
           (let ((symbol (second item)))
             (unless (memq symbol *current-vars*)
               (push `(SETVALUE ,(kconstvalue-index symbol)) *code-part*)
               (push symbol *current-vars*)))) ; *current-value* is unchanged
          (BIND
           (push `(BIND ,(const-index (second item))) *code-part*)
           (setq *current-value* nil *current-vars* '())) ; undefined values
          (UNWIND ; multi-line conversion
           (traverse-anode
            (expand-UNWIND (second item) (third item) (fourth item))))
          (UNWINDSP ; multi-line conversion
           (let ((k (spdepth-difference (second item) (third item))))
             (when (or (> (car k) 0) (> (cdr k) 0))
               (push `(SKIPSP ,(car k) ,(cdr k)) *code-part*))))
          ((JMPIF JMPIFNOT JMPIF1 JMPIFNOT1)
           (if (null *current-value*)
             (let ((label (second item))
                   (new-label (make-label 'NIL)))
               (push
                (case (first item)
                  (JMPIF `(JMPCASE ,label ,new-label))
                  (JMPIFNOT `(JMPCASE ,new-label ,label))
                  (JMPIF1 `(JMPCASE1-TRUE ,label ,new-label))
                  (JMPIFNOT1 `(JMPCASE1-FALSE ,new-label ,label)))
                *code-part*)
               (push *code-index* (symbol-value (second item)))
               (push *code-index* (symbol-value new-label))
               (finish-code-part)
               (setf (get new-label 'code-part) (fill-pointer *code-parts*))
               (setq *code-part* new-label)
               ;; *current-value* and *current-vars* remain unchanged.
               )
             ;; boolean value known at jump
             (if (if (eq *current-value* 'FALSE)
                   ;; value=NIL -> omit JMPIF
                   (memq (first item) '(JMPIF JMPIF1))
                   ;; value/=NIL -> omit JMPIFNOT
                   (memq (first item) '(JMPIFNOT JMPIFNOT1)))
               ;; omit jump
               nil
               ;; convert to JMP:
               (progn
                 (when (memq (first item) '(JMPIF1 JMPIFNOT1))
                   (push '(VALUES1) *code-part*)) ; coerce exactly 1 value
                 (emit-jmp (second item))
                 (setq *dead-code* t)))))
          (JMPHASH
           (let ((hashtable (make-hash-table :key-type 't :value-type 'fixnum
                                             :test (second item)))
                 (labels (cddddr item)))
             (dolist (acons (third item))
               (setf (gethash (car acons) hashtable)
                     (position (cdr acons) labels)))
             (push `(JMPHASH ,(constvalue-index hashtable) ,hashtable
                     ,@(cdddr item))
                   *code-part*))
           ;; note down references:
           (dolist (label (cdddr item))
             (push *code-index* (symbol-value label)))
           (finish-code-part)
           (setq *dead-code* t))
          (VENV
           (let ((venvc (second item))
                 (stackz (third item)))
             (loop ; in venvc pass the NILs
              (when (car venvc) (return))
              (setq venvc (cdr venvc)))
             (push
              (if (consp (car venvc)) ; fetch from Stack
                (multiple-value-bind (k n)
                    (access-in-stack stackz (cdr (car venvc)))
                  (if k `(LOADI ,(car k) ,(cdr k) ,n) `(LOAD ,n) ))
                (if (eq (car venvc) *func*)
                  (if (fnode-Venvconst *func*) '(VENV) '(NIL))
                  (compiler-error 'traverse-anode 'VENV)))
              *code-part*)
             (if (equal (car *code-part*) '(NIL))
               (setq *current-value* 'FALSE
                     *current-vars* (list (new-const 'NIL)))
               (setq *current-value* nil *current-vars* '()))))
          (COPY-CLOSURE
           (push `(COPY-CLOSURE ,(fconst-index (second item)) ,(third item))
                 *code-part*)
           (setq *current-value* 'TRUE *current-vars* '()))
          (CALLP) ; is canceled
          (CALL
           (push `(CALL ,(second item) ,(const-index (third item)))
                 *code-part*)
           (setq *current-value* nil *current-vars* '()))
          ((CALL0 CALL1 CALL2)
           (push `(,(first item) ,(const-index (second item)))
                 *code-part*)
           (setq *current-value* nil *current-vars* '()))
          ((FUNCALLP APPLYP)
           (push '(PUSH) *code-part*)
           (setq *current-value* nil *current-vars* '()))
          ((JMPIFBOUNDP BOUNDP)
           (let ((var (second item))
                 (stackz (fourth item)))
             (when (var-closurep var)
               (compiler-error 'traverse-anode 'var-closurep))
             (multiple-value-bind (k n)
                 (access-in-stack stackz (var-stackz var))
               (when k (compiler-error 'traverse-anode 'var-stackz))
               (push `(,(first item) ,n ,@(cddddr item)) *code-part*)
               (when (eq (first item) 'JMPIFBOUNDP)
                 (push (first *code-part*) (symbol-value (fifth item))))
               ;; undefined values
               (setq *current-value* nil *current-vars* '()))))
          (BLOCK-OPEN
           (let ((label (third item)))
             (push `(BLOCK-OPEN ,(const-index (second item)) ,label)
                   *code-part*)
             (push (first *code-part*) (symbol-value label))
             ;; undefined values
             (setq *current-value* nil *current-vars* '())))
          (RETURN-FROM
           (push
            (if (cddr item)
              (multiple-value-bind (k n)
                  (access-in-stack (third item) (block-stackz (second item)))
                `(RETURN-FROM-I ,(car k) ,(cdr k) ,n))
              (if (block-p (second item))
                `(RETURN-FROM ,(bconst-index (second item)))
                `(RETURN-FROM ,(const-index (second item)))))
            *code-part*)
            (finish-code-part)
            (setq *dead-code* t))
          (TAGBODY-OPEN
           (push `(TAGBODY-OPEN ,(const-index (second item)) ,@(cddr item))
                 *code-part*)
           (dolist (label (cddr item)) (push item (symbol-value label)))
           ;; undefined values
           (setq *current-value* nil *current-vars* '()))
          (GO
           (push
            (if (cdddr item)
              (multiple-value-bind (k n)
                  (access-in-stack (fourth item)
                                   (tagbody-stackz (second item)))
                `(GO-I ,(car k) ,(cdr k) ,n ,(cdr (third item))))
              (if (tagbody-p (second item))
                `(GO ,(gconst-index (second item)) ,(cdr (third item)))
                `(GO ,(const-index (second item)) ,(third item))))
            *code-part*)
           (finish-code-part)
           (setq *dead-code* t))
          ((NIL TAGBODY-CLOSE-NIL)
           (push item *code-part*)
           (setq *current-value* 'FALSE
                 *current-vars* (list (new-const 'NIL))))
          (HANDLER-OPEN
           (setq item
                 (let* ((v (const-value-safe (second item)))
                        (k (spdepth-difference (third item) *func*))
                        (l (length v)) (r (make-array (ash l 1)))) ; 2*l
                   ;; Out of v = #(type1 ... typem)
                   ;; make   v = #(type1 nil ... typem nil)
                   (dotimes (i l) (setf (aref r (ash i 1)) (aref v i)))
                   `(HANDLER-OPEN ,(constvalue-index (cons r k)) ,r ,k
                     ,@(cdddr item))))
           (push item *code-part*)
           (dolist (label (cddddr item)) (push item (symbol-value label))))
          (VALUES0
           (push item *code-part*)
           (setq *current-value* 'FALSE *current-vars* '()))
          ((SKIP SKIPI SKIPSP VALUES1 MVCALLP BLOCK-CLOSE TAGBODY-CLOSE
            CATCH-CLOSE UNWIND-PROTECT-NORMAL-EXIT HANDLER-BEGIN
            ;; STORE only on function-arguments within a
            ;; function-call, cf. c-DIRECT-FUNCTION-CALL
            STORE)
           (push item *code-part*))
          ((T)
           (push item *code-part*)
           (setq *current-value* 'TRUE
                 *current-vars* (list (new-const 'T))))
          ((RET RETGF BARRIER THROW)
           (push item *code-part*)
           (finish-code-part)
           (setq *dead-code* t))
          (JMP
           (emit-jmp (second item))
           (setq *dead-code* t))
          (JSR
           (push item *code-part*)
           (push item (symbol-value (third item)))
           (setq *current-value* nil *current-vars* '()))
          (CATCH-OPEN
           (push item *code-part*)
           (push item (symbol-value (second item))))
          ((UNWIND-PROTECT-OPEN UNWIND-PROTECT-CLOSE)
           (push item *code-part*)
           (push item (symbol-value (second item)))
           ;; values are thrown away
           (setq *current-value* nil *current-vars* '()))
          ((PUSH-NIL PROGV PUSH POP MAKE-VECTOR1&PUSH CALLS1 CALLS2 CALLSR
            CALLC CALLCKEY FUNCALL APPLY PUSH-UNBOUND UNLIST UNLIST*
            STACK-TO-MV MV-TO-STACK NV-TO-STACK MV-TO-LIST LIST-TO-MV MVCALL
            NOT EQ CAR CDR ATOM CONSP SYMBOL-FUNCTION SVREF SVSET)
           (push item *code-part*)
           (setq *current-value* nil *current-vars* '()))
          ((CONS LIST LIST*)
           (push item *code-part*)
           (setq *current-value* 'TRUE *current-vars* '()))
          ((UNWIND-PROTECT-CLEANUP)
           (push item *code-part*)
           (setq *current-vars* '())) ; can destroy variable-values
          ((UNBIND1)
           (push item *code-part*)
           ;; can destroy values of dynamic variables
           (setq *current-vars* (delete-if #'symbolp *current-vars*)))
          (t (compiler-error 'traverse-anode "LISTITEM")))))))

;; Auxiliary Functions after the 1st step:

;; if an instruction item is added, that perhaps contains Label-References,
;; then note-references has to be called. This notes the Label-References in
;; item. item belongs to (aref *code-parts* index).
;; if an instruction item is removed, that perhaps contains Label-References,
;; then remove-references has to be called. This notes the cancellation of the
;; Label-References in item. item belongs to (aref *code-parts* index).
;; Also returns the list of the Labels contained in item.
(macrolet ((references ()
             `(case (first item)
                (JMP (end-ref (second item)))
                ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
                 (end-ref (second item)) (end-ref (third item)))
                (JMPHASH (dolist (label (cdddr item)) (end-ref label)))
                ((CATCH-OPEN UNWIND-PROTECT-OPEN UNWIND-PROTECT-CLOSE)
                 (mid-ref (second item)))
                ((JMPIFBOUNDP BLOCK-OPEN JSR) (mid-ref (third item)))
                (JMPTAIL (mid-ref (fourth item)))
                (TAGBODY-OPEN (dolist (label (cddr item)) (mid-ref label)))
                (HANDLER-OPEN
                 (dolist (label (cddddr item)) (mid-ref label))))))
  (defun note-references (item &optional index)
    (macrolet ((end-ref (label) `(push index (symbol-value ,label)))
               (mid-ref (label) `(push item (symbol-value ,label))))
      (references)))
  (defun remove-references (item &optional index &aux (labellist '()))
    (macrolet ((end-ref (label)
                 (let ((labelvar (gensym)))
                   `(let ((,labelvar ,label))
                      (setf (symbol-value ,labelvar)
                            (delete index (symbol-value ,labelvar)))
                      (pushnew ,labelvar labellist))))
               (mid-ref (label)
                 (let ((labelvar (gensym)))
                   `(let ((,labelvar ,label))
                      (setf (symbol-value ,labelvar)
                            (delete item (symbol-value ,labelvar)))
                      (pushnew ,labelvar labellist)))))
      (references)
      labellist))
)

#|
                              2nd Step
                Simplification of Sequences of Operations

This takes place on the reversed code-parts; they are changed
destructively, withal.

Simplification-Rules for Operations:

1. (VALUES1) can be dropped after all instructions, that produce only
   one value in any case, and above all, that use only one value in any case.

2. (SKIP n1) (SKIP n2)                   --> (SKIP n1+n2)
   (SKIPI k1 k2 n1) (SKIP n2)            --> (SKIPI k1 k2 n1+n2)
   (SKIP n1) (SKIPI k1 k2 n2)            --> (SKIPI k1 k2 n2)
   (SKIPI k11 k21 n1) (SKIPI k21 k22 n2) --> (SKIPI k11+k12+1 k21+k22 n2)
   (SKIPSP k11 k21) (SKIPI k21 k22 n)    --> (SKIPI k11+k12 k21+k22 n)
   (SKIPSP k11 k21) (SKIPSP k21 k22)     --> (SKIPSP k11+k12 k21+k22)

3. (NOT) (NOT) (NOT)                 --> (NOT)
   (ATOM) (NOT)                      --> (CONSP)
   (CONSP) (NOT)                     --> (ATOM)

4. (LOAD 0) (SKIP n)                 --> (POP) (SKIP n-1)  for n>1
   (LOAD 0) (SKIP 1)                 --> (POP)             for n=1
   (PUSH) (SKIP n)                   --> (SKIP n-1)  for n>1
   (PUSH) (SKIP 1)                   -->             for n=1
   (NV-TO-STACK n) (SKIP n)          -->
   (NV-TO-STACK n+m) (SKIP n)        --> (NV-TO-STACK m)
   (NV-TO-STACK n) (SKIP n+m)        --> (SKIP m)
   (STORE m) (SKIP n)                --> (VALUES1) (SKIP n) for n>m
   (STORE 0) (POP)                   --> (VALUES1) (SKIP 1)
   (PUSH) (POP)                      --> (VALUES1)
   (POP) (PUSH)                      -->
   (SKIP n) (PUSH)                   --> (SKIP n-1) (STORE 0) for n>1
   (SKIP 1) (PUSH)                   --> (STORE 0)            for n=1

5. (VALUES1)/... (MV-TO-STACK)       --> (VALUES1)/... (PUSH)
   (VALUES0) (MV-TO-STACK)           -->
   (STACK-TO-MV n) (MV-TO-STACK)     -->
   (STACK-TO-MV m) (NV-TO-STACK n)   --> (PUSH-NIL n-m)  for m<n
                                     -->                 for m=n
                                     --> (SKIP m-n)      for m>n
   (NIL)/(VALUES0) (NV-TO-STACK n)   --> (PUSH-NIL n)
   (VALUES1)/... (NV-TO-STACK n)     --> (VALUES1)/... (PUSH) (PUSH-NIL n-1)

6. (PUSH-UNBOUND n) (PUSH-UNBOUND m) --> (PUSH-UNBOUND n+m)

7. (LIST* 1)                         --> (CONS)

|#

;; The Hash-Table one-value-ops contains those instructions,
;; that create exactly one value.
(defconstant one-value-ops
  (let ((ht (make-hash-table :key-type 'symbol :value-type '(eql t)
                             :test 'stablehash-eq :warn-if-needs-rehash-after-gc t)))
    (dolist (op '(NIL T CONST LOAD LOADI LOADC LOADV LOADIC STORE STOREI
                  STOREC STOREV STOREIC GETVALUE SETVALUE POP VENV
                  COPY-CLOSURE BOUNDP VALUES1 MV-TO-LIST TAGBODY-CLOSE-NIL
                  NOT EQ CAR CDR CONS ATOM CONSP SYMBOL-FUNCTION SVREF SVSET
                  LIST LIST*))
      (setf (gethash op ht) t))
    ht))

;; The value for a Key in this Hash-Table indicates, how many values
;; are needed for the execution of the corresponding Operation
;; (cf. *for-value*):
;; NIL : values are discarded.
;; ONE : One value is used, the remaining values are discarded.
;; ALL : All values are used.
;; Operations, that do not change their values, are not
;; listed here.
(defconstant for-value-table
  (let ((ht (make-hash-table :key-type 'symbol :value-type '(member NIL ONE ALL)
                             :test 'stablehash-eq :warn-if-needs-rehash-after-gc t)))
    (dolist (op '(NIL PUSH-NIL T CONST LOAD LOADI LOADC LOADV LOADIC
                  GETVALUE POP JSR JMPTAIL BARRIER VENV COPY-CLOSURE CALL
                  CALL0 CALLS1 CALLS2 CALLSR FUNCALL PUSH-UNBOUND JMPIFBOUNDP
                  BOUNDP VALUES0 STACK-TO-MV MVCALL
                  BLOCK-OPEN TAGBODY-OPEN TAGBODY-CLOSE-NIL GO GO-I
                  UNWIND-PROTECT-OPEN UNWIND-PROTECT-CLOSE
                  HANDLER-OPEN HANDLER-BEGIN
                  LIST))
      (setf (gethash op ht) 'NIL))
    (dolist (op '(STORE STOREI STOREC STOREV STOREIC SETVALUE BIND PROGV PUSH
                  MAKE-VECTOR1&PUSH CALL1 CALL2 CALLC CALLCKEY APPLY UNLIST
                  UNLIST* VALUES1 LIST-TO-MV MVCALLP CATCH-OPEN
                  NOT EQ CAR CDR CONS ATOM CONSP SYMBOL-FUNCTION SVREF SVSET
                  LIST*))
      (setf (gethash op ht) 'ONE))
    (dolist (op '(MV-TO-STACK NV-TO-STACK MV-TO-LIST RETURN-FROM RETURN-FROM-I
                  THROW UNWIND-PROTECT-NORMAL-EXIT))
      (setf (gethash op ht) 'ALL))
    ;; Not in the Table, because they leave the values unchanged:
    ;;           '(UNBIND1 SKIP SKIPI SKIPSP BLOCK-CLOSE TAGBODY-CLOSE
    ;;             CATCH-CLOSE UNWIND-PROTECT-CLEANUP)
    ;; Not in the Table, because they are jumps:
    ;;   ONE:    '(RETGF JMPHASH)
    ;;   ALL:    '(RET JMP JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
    ht))

;; Simplifies a code-part (in reversed order!).
;; The simplification-rules above are processed as long as possible.
;; Result is mostly NIL, or the start-label instead (in order to indicate,
;; that further optimizations are possible), if its Property for-value
;; has been weakened.
(defun simplify (codelist)
  (unless codelist (return-from simplify nil))
  (let ((for-value-at-end
          (let ((item (car codelist)))
            (case (first item)
              (JMP (get (second item) 'for-value))
              ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
                (if (or (and (not (eq (first item) 'JMPCASE1-TRUE))
                             (eq (get (second item) 'for-value) 'ALL))
                        (and (not (eq (first item) 'JMPCASE1-FALSE))
                             (eq (get (third item) 'for-value) 'ALL)))
                  'ALL
                  'ONE))
              ((RETGF JMPHASH) 'ONE)
              ((BARRIER GO GO-I JMPTAIL) 'NIL)
              ((RETURN-FROM RETURN-FROM-I RET THROW) 'ALL)
              (t (compiler-error 'simplify codelist)))))
        (result nil)) ; poss. the start-label
    ;; for-value-at-end indicates, which values are needed before the jump.
    (loop
      (let ((modified nil))
        (let* ((left codelist) (middle (cdr left)) right
               (for-value for-value-at-end))
          ;; Three Pointers traverse through the code-list:
          ;;  ...left.middle.right...
          ;; for-value indicates, which values are needed after
          ;; execution of (car middle), before execution of (car left) .
          (tagbody start
            (when (atom middle) (go end))
            (setq right (cdr middle))
            (macrolet ((replace1 (new) ; replace (car middle) with new
                         `(progn
                            (setf (car middle) ,new)
                            (setq modified t) (go start)))
                       (replace2 (new)
                         ;; replace (car middle) and (car right) with new
                         `(progn
                            ,@(unless (equal new '(car middle))
                                `((setf (car middle) ,new)))
                            (setf (cdr middle) (cdr right))
                            (setq modified t) (go start)))
                       (discard1 () ; discard (car middle)
                         `(progn
                            (setf (cdr left) (setq middle right))
                            (setq modified t) (go start)))
                       (discard2 () ; discard (car middle) and (car right)
                         `(progn
                            (setf (cdr left) (setq middle (cdr right)))
                            (setq modified t) (go start)))
                       (extend2 (new1 new2)
                         ;; replace (car middle) with new1 and new2
                         `(progn
                            (setf (car middle) ,new1)
                            (setf (cdr middle) (cons ,new2 right))
                            (setq modified t) (go start))))
              (when (eq for-value 'NIL)
                ;; before an operation, that needs no values:
                (case (first (car middle))
                  ((NIL T CONST LOAD LOADI LOADC LOADV LOADIC GETVALUE VENV
                    BOUNDP VALUES0 VALUES1 MV-TO-LIST LIST-TO-MV NOT ATOM
                    CONSP)
                    (discard1))
                  ((CAR CDR SYMBOL-FUNCTION)
                   ; CAR, CDR, SYMBOL-FUNCTION cannot always be discarded,
                   ; because it may need to signal an error if SAFETY = 3.
                   (when (< (declared-optimize 'SAFETY (second (car middle))) 3)
                     (discard1)))
                  ((LIST LIST* STACK-TO-MV) ; (LIST n) --> (SKIP n), n>0
                                            ; (LIST* n) --> (SKIP n), n>0
                                            ; (STACK-TO-MV n) --> (SKIP n), n>0
                    (replace1 `(SKIP ,(second (car middle)))))
                  ((POP EQ CONS) (replace1 '(SKIP 1)))
                  ((SVREF)
                   ; SVREF cannot always be discarded, because it may need to
                   ; signal an error if SAFETY = 3.
                   (when (< (declared-optimize 'SAFETY (second (car middle))) 3)
                     (replace1 '(SKIP 1))))))
              (when (eq for-value 'ONE)
                ;; before an operation, that needs only one value:
                (case (first (car middle))
                  (VALUES1 (discard1))
                  (VALUES0 (replace1 '(NIL)))
                  (LIST-TO-MV (replace1 `(CAR NIL)))
                  (STACK-TO-MV ; (STACK-TO-MV n) --> (SKIP n-1) (POP) for n>1
                    (let ((n (second (car middle))))
                      (extend2 '(POP) `(SKIP ,(- n 1)))))))
              (when (consp right)
                ;; peephole comprises (car middle) and (car right), poss. more.
                (case (first (car middle))
                  (VALUES1 ; rule 1
                   (when (gethash (first (car right)) one-value-ops nil)
                     ;; (op ...) (VALUES1) --> (op ...)
                     (discard1)))
                  (NOT ; rule 3
                   (case (first (car right))
                     (NOT
                      (when (and (consp (cdr right))
                                 (equal (cadr right) '(NOT)))
                        ;; (NOT) (NOT) (NOT) --> (NOT)
                        (discard2)))
                     (ATOM (replace2 '(CONSP)))   ; (ATOM) (NOT) --> (CONSP)
                     (CONSP (replace2 '(ATOM))))) ; (CONSP) (NOT) --> (ATOM)
                  (SKIP
                    (let ((n2 (second (car middle)))) ; n2 > 0
                      (case (first (car right))
                        ;; rule 2
                        (SKIP ; (SKIP n1) (SKIP n2) --> (SKIP n1+n2)
                         (let ((n1 (second (car right))))
                           (replace2 `(SKIP ,(+ n1 n2)))))
                        (SKIPI
                         ;; (SKIPI k1 k2 n1) (SKIP n2) --> (SKIPI k1 k2 n1+n2)
                         (let ((k1 (second (car right)))
                               (k2 (third (car right)))
                               (n1 (fourth (car right))))
                           (replace2 `(SKIPI ,k1 ,k2 ,(+ n1 n2)))))
                        ;; rule 4
                        (LOAD ; (LOAD 0) (SKIP n) --> (POP) [(SKIP n-1)]
                         (when (eql (second (car right)) 0)
                           (if (eql n2 1)
                             (replace2 '(POP))
                             (progn (setf (car right) '(POP))
                                    (replace1 `(SKIP ,(- n2 1)))))))
                        (PUSH ; (PUSH) (SKIP n) --> [(SKIP n-1)]
                         (if (eql n2 1)
                           (discard2)
                           (replace2 `(SKIP ,(- n2 1)))))
                        (NV-TO-STACK
                         (let ((n1 (second (car right))))
                           (cond ((> n1 n2)
                                  (replace2 `(NV-TO-STACK ,(- n1 n2))))
                                 ((< n1 n2) (replace2 `(SKIP ,(- n2 n1))))
                                 (t (discard2)))))
                        (STORE
                         ;; (STORE m) (SKIP n) --> (VALUES1) (SKIP n) for n>m
                         (let ((m (second (car right))))
                           (when (> n2 m)
                             (setf (car right) '(VALUES1))
                             (setq modified t) (go start)))))))
                  (SKIPI ; rule 2
                   (case (first (car right))
                     (SKIP ; (SKIP n1) (SKIPI k1 k2 n2) --> (SKIPI k1 k2 n2)
                      (replace2 (car middle)))
                     (SKIPI
                      ;; (SKIPI k11 k21 n1) (SKIPI k21 k22 n2)
                      ;; --> (SKIPI k11+k12+1 k21+k22 n2)
                      (let ((k11 (second (car right)))
                            (k21 (third (car right)))
                            (k12 (second (car middle)))
                            (k22 (third (car middle)))
                            (n2 (third (car middle))))
                        (replace2 `(SKIPI ,(+ k11 k12 1) ,(+ k21 k22) ,n2))))
                     (SKIPSP
                      ;; (SKIPSP k11 k21) (SKIPI k21 k22 n)
                      ;; --> (SKIPI k11+k12 k21+k22 n)
                      (let ((k11 (second (car right)))
                            (k21 (third (car right)))
                            (k12 (second (car middle)))
                            (k22 (third (car middle)))
                            (n2 (fourth (car middle))))
                        (replace2 `(SKIPI ,(+ k11 k12) ,(+ k21 k22) ,n2))))))
                  (SKIPSP ; rule 2
                   (case (first (car right))
                     (SKIPSP
                      ;; (SKIPSP k11 k21) (SKIPSP k21 k22)
                      ;; --> (SKIPSP k11+k12 k21+k22)
                      (let ((k11 (second (car right)))
                            (k21 (third (car right)))
                            (k12 (second (car middle)))
                            (k22 (third (car middle))))
                        (replace2 `(SKIPSP ,(+ k11 k12) ,(+ k21 k22)))))))
                  (POP ; rule 4
                   (cond ((equal (car right) '(STORE 0))
                          ;; (STORE 0) (POP) --> (VALUES1) (SKIP 1)
                          (setf (car right) '(VALUES1))
                          (replace1 '(SKIP 1)))
                         ((equal (car right) '(PUSH))
                          ;; (PUSH) (POP) --> (VALUES1)
                          (replace2 '(VALUES1)))))
                  (PUSH ; rule 4
                   (case (first (car right))
                     (POP (discard2)) ; discard (POP) (PUSH)
                     (SKIP ; (SKIP n) (PUSH) --> [(SKIP n-1)] (STORE 0)
                      (let ((n (second (car right))))
                        (if (eql n 1)
                          (unless (and (consp (cdr right))
                                       (equal (cadr right) '(LOAD 0)))
                            ;; (LOAD 0) (SKIP 1) (PUSH) is treated differently
                            (replace2 '(STORE 0)))
                          (progn (setf (car right) `(SKIP ,(- n 1)))
                                 (replace1 '(STORE 0))))))))
                  (MV-TO-STACK ; rule 5
                   (when (gethash (first (car right)) one-value-ops nil)
                     ;; (car right) returns only one value -->
                     ;; replace (MV-TO-STACK) with (PUSH) :
                     (replace1 '(PUSH)))
                   (case (first (car right))
                     ((VALUES0 STACK-TO-MV) (discard2))))
                  (NV-TO-STACK ; rule 5
                   (let ((n (second (car middle))))
                     (case (first (car right))
                       (STACK-TO-MV
                        (let ((m (second (car right))))
                          (cond ((> n m) (replace2 `(PUSH-NIL ,(- n m))))
                                ((< n m) (replace2 `(SKIP ,(- m n))))
                                (t (discard2)))))
                       ((VALUES0 NIL) (replace2 `(PUSH-NIL ,n)))
                       (t (when (gethash (first (car right)) one-value-ops nil)
                            (extend2 `(PUSH-NIL ,(- n 1)) `(PUSH)))))))
                  (PUSH-UNBOUND ; rule 6
                   (case (first (car right))
                     (PUSH-UNBOUND
                      ;; (PUSH-UNBOUND n) (PUSH-UNBOUND m)
                      ;; --> (PUSH-UNBOUND n+m)
                      (let ((n (second (car right)))
                            (m (second (car middle))))
                        (replace2 `(PUSH-UNBOUND ,(+ n m)))))))
                  (LIST* ; rule 7
                   (when (equal (rest (car middle)) '(1))
                     (replace1 '(CONS)))))))
            (when (atom middle) (go end))
            ;; calculate new for-value, depending on (car middle):
            (setq for-value
                  (gethash (first (car middle)) for-value-table for-value))
            ;; advance:
            (setq left middle middle right)
            (go start)
           end)
          ;; code-part finished: (atom middle)
          (when middle
            ;; middle is the start-label
            (let ((old-for-value (get middle 'for-value)))
              ;; is for-value better than old-for-value ?
              (when (and (not (eq for-value old-for-value))
                         (or (eq old-for-value 'ALL) (eq for-value 'NIL)))
                ;; yes -> return start-label as result hereafter:
                (setf (get middle 'for-value) for-value result middle))))
          ) ; end let*
        (unless modified (return))
        )) ; end let, loop
    (let (codelistr)
      (when (and (eq (first (first codelist)) 'RET)
                 (consp (setq codelistr (cdr codelist)))
                 (or (eq (first (first codelistr)) 'JSR)
                     (and (eq (first (second codelist)) 'SKIP)
                          (consp (setq codelistr (cddr codelist)))
                          (eq (first (first codelistr)) 'JSR))))
        ;; (JSR n label) [(SKIP m)] (RET) --> (JMPTAIL n n+m label)
        (let ((n (second (first codelistr)))
              (label (third (first codelistr)))
              (m (if (eq codelistr (cdr codelist))
                   0
                   (second (second codelist)))))
          (setf (first codelist) `(JMPTAIL ,n ,(+ n m) ,label)))
        (remove-references (first codelistr)) ; (JSR ...) is discarded
        (note-references (first codelist)) ; (JMPTAIL ...) is inserted
        (setf (cdr codelist) (cdr codelistr)) ; discard 1 resp. 2 list-elements
        (setq for-value-at-end 'NIL))) ; JMPTAIL needs no values
    result))

#|
                            3rd Step:
                      General Optimizations

If an Optimization is performed successfully, all the
Optimizations that might apply after this one are retried.

 optimize-part
   - calls the 2nd step: Peephole-Optimization of normal Operations.

 optimize-label
   - code-parts for labels, that are not referenced (anymore), are removed.
   - if a label is referenced by only one single JMP, that does not
     spring from the same code-part, both affected pieces can be concatenated.
 optimize-short
   - if there is a code-part, where the start-label label1 is
     immediately followed by a (JMP label2), then all references of
     label1 are replaced by label2 and the code-part is removed.
   - if there is a code-part, where the start-label label is immediately
     followed by a
        (JMPCASE/JMPCASE1-TRUE/JMPCASE1-FALSE label_true label_false),
     then references (JMPCASE1-TRUE label l) and
     (JMPCASE1-FALSE l label) can be simplified.
   - a short code-part is directly attached to corresponding JMPs to its
     start-label. (A code-part is called "short", if it comprises at
     most 2 instructions and if it is not concluded with a
     JMPHASH (which should not be duplicated).
     HANDLER-OPEN also should not be duplicated.)
 optimize-jmpcase
   - (JMPCASE label label) is simplified to (JMP label).
   - (NOT) [...] (JMPCASE label_true label_false) is simplified to
     [...] (JMPCASE label_false label_true), whereas [...] may only
     contain instructions, that do not change the 1. value, and no
     values are needed at label_true and label_false.
 optimize-value
   - A jump JMPCASE1-TRUE/JMPCASE1-FALSE can be replaced by JMPCASE, if the
     value is not needed at the target-label or only the 1. value is needed.
   - A jump JMPCASE/JMPCASE1-TRUE/JMPCASE1-FALSE can be replaced by a
     JMP, if the current value at this location can be proven to be
     eiter =NIL or /=NIL .
   - A JMP can carry forward the information describing the current
     value to its target-label.

 coalesce
   - coalesce code-parts with the same end (at least 3 instructions).

|#

(defun optimize-part (code)
  (let ((label (simplify code)))
    (when label
      ;; The Property for-value of label was improved.
      (dolist (ref (symbol-value label))
        (when (integerp ref) (optimize-value ref))))))

(defun optimize-label (label &optional (index (get label 'code-part))
                                       (code (aref *code-parts* index))
                                       (lastc (last code)))
  (unless (or code (symbol-value label)) (return-from optimize-label))
  (unless (eq label (cdr lastc)) (compiler-error 'optimize-label label))
  (when label
    ;; label is a Label, it starts the Code
    ;; code = (aref *code-parts* index), and lastc = (last code).
    (let ((refs (symbol-value label))) ; List of References to it
      (cond ((null refs)
             ;; non-referenced Label: remove code-part,
             ;; eliminate references out of this code-part.
             (let ((labellist '())) ; list of labels that have lost references
               (loop
                 (when (atom code) (return))
                 (setq labellist
                       (nreconc labellist
                                (remove-references (pop code) index))))
               (setf (aref *code-parts* index) nil) ; remove code-part
               ;; At Labels with fewer references continue optimization:
               ;; (Caution: This can change *code-parts*.)
               (dolist (olabel labellist)
                 (let* ((oindex (get olabel 'code-part))
                        (ocode (aref *code-parts* oindex)))
                   (when (and ocode (eq (cdr (last ocode)) olabel))
                     (optimize-label olabel oindex ocode))))))
            ((null (cdr refs)) ; Label with only one Reference, by JMP ?
             (let ((ref (first refs)))
               (when (and (integerp ref) ; A JMP is a leaving-jump
                          (eq (first (car (aref *code-parts* ref))) 'JMP)
                          (not (eql index ref))) ; from another code-part
                 ;; append:
                 ;; (aref *code-parts* ref) is put into the "drawer"
                 ;; (aref *code-parts* index) .
                 (setf (cdr lastc) (rest (aref *code-parts* ref)))
                 (setf (aref *code-parts* ref) nil)
                 (let ((new-startlabel (cdr (last lastc))))
                   ;; new Startlabel of (aref *code-parts* index)
                   (when new-startlabel
                     (setf (get new-startlabel 'code-part) index)))
                 ;; deactivate old Startlabel of (aref *code-parts* index):
                 (setf (symbol-value label) '())
                 ;; simplify new code-part:
                 (optimize-part code))))))))

(defun optimize-short (index &optional (code (aref *code-parts* index))
                             &aux      (lastc (last code))
                                       (label (cdr lastc)))
  (when label
    ;; label is a Label, it starts the Code
    ;; code = (aref *code-parts* index), and lastc = (last code).
    (when (eq code lastc)
      ;; One single Operation after the Label.
      (let ((item (car code)))
        (case (first item)
          (JMP ; (JMP ...) immediately behind the Label
           (let ((to-label (second item)))
             (unless (symbol-value to-label)
               (compiler-error 'optimize-short (cons label to-label)))
             (unless (eq label to-label)
               (label-subst label to-label) ; adjust References
               (setf (aref *code-parts* index) nil) ; remove code-piece
               (setf (symbol-value to-label) ; reference is dropped
                     (delete index (symbol-value to-label)))
               (optimize-label to-label) ; possible optimization
               (dolist (refindex (symbol-value to-label))
                 (when (integerp refindex)
                   (let* ((refcode (aref *code-parts* refindex))
                          (ref (car refcode)))
                     (when (and (eq (first ref) 'JMPCASE)
                                (eq (second ref) to-label)
                                (eq (third ref) to-label))
                       ;; save optimization
                       (optimize-jmpcase refindex refcode)))))))
           (return-from optimize-short))
          ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
           (let ((true-label (second item))
                 (false-label (third item)))
             (unless (or (eq label true-label) (eq label false-label))
               ;; simplifly JMPCASE1-references to label:
               (let ((modified-indices '())) ; Indices of modified code-parts
                 (dolist (refindex (symbol-value label))
                   (when (integerp refindex)
                     (let* ((refcode (aref *code-parts* refindex))
                            (ref (car refcode)))
                       (case (first ref)
                         (JMP
                          ;; (JMP label) -->
                          ;; (JMPCASE/... true-label false-label)
                          (setf (car refcode) item)
                          ;; new references to true-label and false-label:
                          (push refindex (symbol-value true-label))
                          (push refindex (symbol-value false-label))
                          (push refindex modified-indices))
                         ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
                          ;; (JMPCASE/... label1 label2)
                          (let (;; TRUE-case: where to jump
                                (label1 (second ref))
                                ;; FALSE-case: where to jump
                                (label2 (third ref))
                                ;; TRUE-case: with (VALUES1) ?
                                (1-true (eq (first ref) 'JMPCASE1-TRUE))
                                ;; FALSE-case: with (VALUES1) ?
                                (1-false (eq (first ref) 'JMPCASE1-FALSE)))
                            (when (eq label label1)
                              ;; the (JMPCASE/... label ...) is simplified
                              ;; to (JMPCASE/... true-label ...).
                              (setq label1 true-label)
                              ;; new reference to true-label:
                              (push refindex (symbol-value true-label))
                              (push refindex modified-indices)
                              (when (eq (first item) 'JMPCASE1-TRUE)
                                (setq 1-true t)))
                            (when (eq label label2)
                              ;; the (JMPCASE/... ... label) is simplified
                              ;; to (JMPCASE/... ... false-label).
                              (setq label2 false-label)
                              ;; new reference to false-label:
                              (push refindex (symbol-value false-label))
                              (push refindex modified-indices)
                              (when (eq (first item) 'JMPCASE1-FALSE)
                                (setq 1-false t)))
                            (unless (eq (get label1 'for-value) 'ALL)
                              (setq 1-true nil))
                            (unless (eq (get label2 'for-value) 'ALL)
                              (setq 1-false nil))
                            (when (and 1-true 1-false)
                              (push '(VALUES1) (cdr refcode))
                              (setq 1-true nil 1-false nil))
                            (setf (car refcode)
                                  `(,(cond (1-true 'JMPCASE1-TRUE)
                                           (1-false 'JMPCASE1-FALSE)
                                           (t 'JMPCASE))
                                     ,label1
                                     ,label2))))
                         (JMPHASH ; JMPHASH has undefined values
                          (compiler-error 'optimize-short ref))))
                     ;; later:
                     ;; (setf (symbol-value label)
                     ;;       (delete refindex (symbol-value label)))
                     ))
                 (setf (symbol-value label)
                       (delete-if #'integerp (symbol-value label)))
                 ;; more optimization feasible because of reduced references:
                 (optimize-label label)
                 ;; poss. further optimization in changed code-parts:
                 (dolist (refindex modified-indices)
                   (simplify (aref *code-parts* refindex))
                   (optimize-value refindex)
                   (optimize-jmpcase refindex
                                     (aref *code-parts* refindex))))))))))
    ;; further "short" code-parts, at most 2 instructions long:
    (when (and (or (eq code lastc) (eq (cdr code) lastc))
               (not (eq (first (car code)) 'JMPHASH))
               (or (eq code lastc)
                   (not (eq (first (cadr code)) 'HANDLER-OPEN))))
      (let ((indices '())) ; we append code to those code-parts, whose indices are in this list.
        (setf (cdr lastc) '()) ; code preliminarily without the label to the end
        (dolist (refindex (symbol-value label))
          (when (and (integerp refindex) (not (eql refindex index))
                     (not (member refindex indices)))
            (let ((refcode (aref *code-parts* refindex)))
              (when (eq (first (car refcode)) 'JMP)
                ;; append:
                (let ((new-code (mapcar #'copy-list code)))
                  (dolist (op new-code) (note-references op refindex))
                  (setf (aref *code-parts* refindex)
                        (nconc new-code (cdr refcode))))
                (setf (symbol-value label) (delete refindex (symbol-value label)))
                (push refindex indices)))))
        (setf (cdr lastc) label) ; set the label to the list-end again
        (when indices
          ;; further possible optimizations:
          (dolist (refindex indices)
            (optimize-part (aref *code-parts* refindex)))
          (optimize-label label)))))) ; label has fewer references -> optimize
;; get-boolean-value tries to determine for a given starting piece of a
;; code-part (an (nthcdr n codelist) with n>=1) , which boolean value is
;; there after its execution:
;; FALSE     surely A0 = NIL,
;; TRUE      surely A0 /= NIL,
;; NIL       can say nothing.
(defun get-boolean-value (code)
  (let ((invert nil)) ; if the boolean value is inverted from here to the end
    ((lambda (value)
       (if invert
         (case value (TRUE 'FALSE) (FALSE 'TRUE) (t NIL))
         value))
     (block value
       (loop ; traverse code-list
         (when (atom code) (return))
         (case (first (car code))
           ((NIL VALUES0 TAGBODY-CLOSE-NIL) ; produce value NIL
            (return-from value 'FALSE)) ; thus we can terminate the loop
           ((T CONS LIST LIST*) ; produce value /= NIL
            ;; (LIST n) and (LIST* n) because of n>0.
            (return-from value 'TRUE)) ; thus we can terminate the loop
           (CONST
            (unless (and (cddr (car code))
                         (eq (const-horizon (third (car code))) ':form))
              ;; (CONST n) produces value /= NIL, because the value
              ;; is already known at Compile-Time and the constant
              ;; NIL in make-const-code has already been treated.
              (return-from value 'TRUE)) ; thus we can terminate the loop
            (return-from value nil))
           (NOT (setq invert (not invert))) ; invert the boolean value later
           ((UNBIND1 SKIP SKIPI SKIPSP STORE STOREI STOREV STOREC STOREIC
             SETVALUE VALUES1 BLOCK-CLOSE TAGBODY-CLOSE CATCH-CLOSE
             UNWIND-PROTECT-CLEANUP)
            ;; no modification of the first value ->
            ;; continue in the code-list
            )
           (t (return-from value nil)))
         (setq code (cdr code)))
       (when code
         ;; code is the start-label.
         ;; Inspect all jumps to the Label code:
         (let ((so-far nil))
           ;; = FALSE, if all jumps so far bring along FALSE,
           ;; = TRUE,  if all jumps so far bring along TRUE,
           ;; = NIL at the beginning.
           ;; If a jump brings along an unknown boolean value,
           ;; the loop can be left instantly.
           (flet ((new (value)
                    (cond ((null so-far) (setq so-far value))
                          ((not (eq value so-far))
                           (return-from value nil)))))
             (dolist (ref (symbol-value code))
               (if (integerp ref)
                 (let ((refcode (first (aref *code-parts* ref)))) ; the jump hither
                   ;; this cannot be a leaving-jump with undefined values.
                   (case (first refcode)
                     (JMP
                      (if (third refcode)
                        ;; value known before the jump
                        (new (third refcode))
                        ;; value unknown before the jump
                        (return-from value nil)))
                     ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
                      (when (eq code (second refcode)) (new 'TRUE))
                      (when (eq code (third refcode)) (new 'FALSE)))
                     (t ;; JMPHASH has undefined values, and the
                        ;; other leaving-jumps contain no Labels.
                      (compiler-error 'get-boolean-value refcode))))
                 (case (first ref)
                   ((JMPIFBOUNDP BLOCK-OPEN CATCH-OPEN)
                    (return-from value nil)) ; can say nothing
                   (t ;; There are undefined values at the Labels in
                      ;; TAGBODY-OPEN, JSR, UNWIND-PROTECT-OPEN,
                      ;; UNWIND-PROTECT-CLOSE.
                    (compiler-error 'get-boolean-value ref))))))))
       nil)))) ; Default: can say nothing

(defun optimize-jmpcase (index code)
  (when (eq (first (car code)) 'JMPCASE)
    ;; Code ends with (JMPCASE ...)
    (let ((true-label (second (car code)))
          (false-label (third (car code))))
      (if (eq true-label false-label)
        ;; (JMPCASE label label) --> (JMP label ..)
        (progn
          (setf (car code) `(JMP ,true-label ,(get-boolean-value (cdr code))))
          ;; double reference becomes a single reference:
          (setf (symbol-value true-label)
                (delete index (symbol-value true-label) :count 1))
          ;; continue optimization:
          (optimize-part code)
          (optimize-short (get true-label 'code-part)))
        (when (and (null (get true-label 'for-value))
                   (null (get false-label 'for-value)))
          ;; try to eliminate NOTs:
          (let ((invert 0)
                (cr1 code)
                (cr2 (cdr code))) ; always cr2 = (cdr cr1)
            (loop
              (when (atom cr2) (return))
              (case (first (car cr2))
                ((UNBIND1 SKIP SKIPI SKIPSP VALUES1 BLOCK-CLOSE TAGBODY-CLOSE
                  CATCH-CLOSE UNWIND-PROTECT-CLEANUP)
                 ;; these operations do not need values and leave
                 ;; the 1. value unmodified
                 (shiftf cr1 cr2 (cdr cr2)))
                (NOT
                 (setf (cdr cr1) (setq cr2 (cdr cr2))) ; discard (NOT)
                 (incf invert))
                (t (return))))
            ;; invert = number of times, how often (NOT) was discarded
            (when (oddp invert)
              ;; permute true-label and false-label:
              (setf (car code) `(JMPCASE ,false-label ,true-label)))
            (when (plusp invert)
              ;; continue optimization:
              (optimize-part code)
              (optimize-short index))))))))

(defun optimize-value (index &optional (code (aref *code-parts* index)))
  (let ((item (car code)))
    (case (first item)
      ((JMPCASE JMPCASE1-TRUE JMPCASE1-FALSE)
       ;; (JMPCASE/... true-label false-label)
       (let ((true-label (second item))
             (false-label (third item)))
         (when (or (and (eq (first item) 'JMPCASE1-TRUE)
                        (not (eq (get true-label 'for-value) 'ALL))
                        ;; value-number 1 is not needed at true-label
                        ;; (JMPCASE1-TRUE ...) --> (JMPCASE ...)
                        )
                   (and (eq (first item) 'JMPCASE1-FALSE)
                        (not (eq (get false-label 'for-value) 'ALL))
                        ;; value-number 1 is not needed at false-label
                        ;; (JMPCASE1-FALSE ...) --> (JMPCASE ...)
                        ))
           (setq item (setf (car code) `(JMPCASE ,@(rest item))))
           ;; further optimizations possible:
           (optimize-jmpcase index code))
         ;; try to detect the boolean value at this place
         ;; and simplify if possible:
         (case (get-boolean-value (cdr code))
           (TRUE                ; jump always aims at true-label
            (unless (eq true-label false-label)
              ;; discard reference to false-label:
              (setf (symbol-value false-label)
                    (delete index (symbol-value false-label))))
            (setf (car code) `(JMP ,true-label TRUE))
            (when (eq (first item) 'JMPCASE1-TRUE)
              (push '(VALUES1) (cdr code))
              (simplify code))
            (optimize-part code) ; further possible optimization
            ;; further possible optimizations:
            (optimize-label false-label) ; because of reduced references
            (optimize-short index)) ; because of optimize-part above
           (FALSE
            (unless (eq true-label false-label)
              ;; discard reference to true-label
              (setf (symbol-value true-label)
                    (delete index (symbol-value true-label))))
            (setf (car code) `(JMP ,false-label FALSE))
            (when (eq (first item) 'JMPCASE1-FALSE)
              (push '(VALUES1) (cdr code))
              (simplify code))
            (optimize-part code) ; further possible optimization
            ;; further possible optimizations:
            (optimize-label true-label) ; because of reduced references
            (optimize-short index))))) ; because of optimize-part above
      (JMP
       (let ((label (second item)))
         (when (get label 'for-value)
           ;; value is required
           (when (null (third item))
             ;; but it is unknown.
             ;; maybe it can be ascertained?
             (let ((value (get-boolean-value (cdr code))))
               (when value
                 (setf (car code) `(JMP ,label ,value))
                 ;; value is now known, maybe it can be utilized:
                 (optimize-value (get label 'code-part)))))))))))

;; coalesce coalesces identical code-pieces in the given code-parts as far as
;; possible and returns as result a flag, if something was changed.
(defun coalesce (&optional (indexlist
                            ;; list of all possible indices
                            (let ((L '()))
                              (dotimes (i (fill-pointer *code-parts*))
                                (push i L))
                              (nreverse L))))
  (let ((parts-ht ; A Hashtable, that realizes the mapping:
                  ; code-end --> list of all indices of code-parts,
                  ;              that end with the same code-piece
         (let ((ht (make-hash-table :value-type 'list
                                    :test #'equal :size (length indexlist))))
           (dolist (index indexlist)
             (let ((code (aref *code-parts* index))) ; a code-piece
               ;; only pieces are coalesced that match in
               ;; at least the last 3 operations, because of the
               ;; simplification-rule for "short" codes-pieces.
               (when (and (consp code) (consp (cdr code)) (consp (cddr code)))
                 (push index
                       (gethash (list* (first code) (second code) (third code))
                                ht '())))))
           ht))
        (modified nil))
    ;; Then, iterate over the possible code-ends:
    (maphash
      #'(lambda (code-beginning indices)
          (declare (ignore code-beginning))
          (when (cdr indices) ; at least two indices with this code-end?
            ;; try to coalesce a code-piece that is as long as possible:
            (let ((codes ; list of code-pieces to be coalesced
                    (mapcar #'(lambda (i) (aref *code-parts* i)) indices))
                  (new-code '()) ; here the common code is collected
                  (new-index (fill-pointer *code-parts*)) ; its index
                  (new-order ; the common piece is sorted at the last part
                   (reduce #'max (mapcar #'(lambda (i)
                                             (aref *code-positions* i))
                                         indices))))
              (loop
               ;; all still match?
               (unless (every #'consp codes) (return))
               (let* ((code1 (first codes)) ; an arbitrary code-piece
                      (code11 (car code1))) ; its last operation
                 (unless (every #'(lambda (code) (equal (car code) code11))
                                (rest codes))
                   (return))
                 ;; yes. shorten all code-pieces from codes by one operation:
                 (mapc #'(lambda (code index) ; delete references
                           (remove-references (car code) index))
                       codes indices)
                 ;; shorten: (setq codes (mapcar #'cdr codes)), or:
                 (mapl #'(lambda (codesr)
                           (setf (car codesr) (cdr (car codesr))))
                       codes)
                 (push code11 new-code) ; lengthen new-code
                 (note-references code11 new-index)))
              (when new-code
                (let* ((new-label (make-label 'ALL))
                       ;; All code-pieces from codes were shortened, they
                       ;; are now lengthened by one (JMP new-label NIL).
                       (jmpop `(JMP ,new-label NIL)))
                  (mapc #'(lambda (code index)
                            (setf (aref *code-parts* index) (cons jmpop code)))
                        codes indices)
                  ;; References to new-label
                  (setf (symbol-value new-label) indices)
                  (setf (get new-label 'code-part) new-index)
                  (vector-push-extend (nreconc new-code new-label)
                                      *code-parts*)
                  (vector-push-extend new-order *code-positions*))
                ;; further possible optimizations:
                (optimize-part (aref *code-parts* new-index))
                (coalesce indices)
                (setq modified t))))) ; change has taken place
      parts-ht)
    modified))

;; The main-function of the 3rd step:
;; Performs all optimizations, and then collects all code-pieces
;; into one single code-list and returns it.
(defun optimize-all ()
  ;; optimizations:
  (loop
    ;; call optimizations:
    ;; if one makes a find, it will also call the optimization-
    ;; steps, that might thereby become possible. Thus they
    ;; have to be listed here only once.
    ;; Caution: *code-parts* and its content can be
    ;; completely changed by the optimizations.
    (do ((index 0 (1+ index)))
        ((eql index (fill-pointer *code-parts*)))
      (let ((code (aref *code-parts* index)))
        (when code
          (let* ((lastc (last code))
                 (label (cdr lastc)))
            (when label
              (unless (eql index (get label 'code-part))
                (compiler-error 'optimize-all 'code-part)))
            (optimize-label label index code lastc))))
      (let ((code (aref *code-parts* index)))
        (when code
          (optimize-jmpcase index code)))
      (let ((code (aref *code-parts* index)))
        (when code
          (optimize-value index code)))
      (let ((code (aref *code-parts* index)))
        (when code
          (optimize-short index code))))
    (unless (coalesce) (return))) ; (coalesce) did nothing -> finished
  ;; collect into one single code-list:
  ;; (The labels now become list-elements in the code instead of NTHCDRs.)
  (let ((start-index 0)) ; Start-"Label" NIL begins code-piece Nr. 0
    ;; if possible, first append at a time a code-piece that starts with label,
    ;; to a code-piece, that ends with a JMP or JMPCASE/... to label.
    (do ((index (fill-pointer *code-parts*)))
        ((eql (decf index) 0))
      ;; index loops through the indices of *code-parts*
      ;; top-down, apart from start-index=0.
      (let ((code (aref *code-parts* index)))
        (when code
          (loop
            ;; Treat the label at the end of code, in code-piece Nr. index:
            (let* ((lastc (last code)) ; last Cons of code
                   (label (cdr lastc)) ; Label at the end of code
                   (refs (symbol-value label)) ; References pointing to it
                   (pos (aref *code-positions* index)) ; Position of code
                   (jmp-ref nil) ; best found JMP-Reference to label so far
                   (jmpcase-ref nil) ; best found JMPCASE-Reference to label so far
                   (jmpcase1-ref nil)) ; best found JMPCASE1-...-Reference to label so far
              (if (null label)
                ;; The Start-Code-Piece was attached to another place!
                (progn
                  (setq start-index index)
                  (return)) ; go for the next index
                (flet ((better (new-ref old-ref)
                         ;; One Reference new-ref is "better" than another
                         ;; old-ref, if it is closer. Withal,
                         ;; forward-references have in general higher priority
                         ;; compared to backward-references.
                         (or (null old-ref) ; no old-ref yet?
                             (let ((old-pos (aref *code-positions* old-ref))
                                   (new-pos (aref *code-positions* new-ref)))
                               (if (> old-pos pos) ; so far only backward-jump?
                                 ;; yes: new-pos is better, if it is
                                 ;; < pos (forward-jump) or
                                 ;; >=pos, <=old-pos (shorter backward-jump).
                                 (<= new-pos old-pos)
                                 ;; no: new-pos is better, if it is
                                 ;; <=pos, >=old-pos (shorter forward-jump).
                                 (<= old-pos new-pos pos))))))
                  (macrolet ((update (old-ref new-ref) ; for determination of the best so far
                               `(when (better ,new-ref ,old-ref)
                                  (setq ,old-ref ,new-ref))))
                    ;; determine the best reference, to which the code-piece
                    ;; can be attached:
                    (dolist (refindex refs)
                      (when (and (integerp refindex)
                                 (not (eql refindex index))) ; do not attach to itself!
                        (let ((refcode1 (car (aref *code-parts* refindex))))
                          (case (first refcode1)
                            (JMP ; attachment possible to (JMP label ...)
                             (update jmp-ref refindex))
                            (JMPCASE ; attachment possible to (JMPCASE ... label ...)
                             (update jmpcase-ref refindex))
                            (JMPCASE1-TRUE ; attachment possible to (JMPCASE1-TRUE ... label)
                             (when (eq label (third refcode1))
                               (update jmpcase1-ref refindex)))
                            (JMPCASE1-FALSE ; attachment possible to (JMPCASE1-FALSE label ...)
                             (when (eq label (second refcode1))
                               (update jmpcase1-ref refindex)))))))
                    (cond (jmp-ref ; attach to (JMP label)
                           (setf (cdr lastc)
                                 (cons label (cdr (aref *code-parts*
                                                        jmp-ref))))
                           (setf (aref *code-parts* jmp-ref) nil)
                           (setq code lastc))
                          (jmpcase1-ref
                           (let* ((refcode (aref *code-parts* jmpcase1-ref))
                                  (refcode1 (car refcode))
                                  (jmpop
                                    (if (eq label (second refcode1))
                                      `(JMPIFNOT1 ,(third refcode1))
                                      `(JMPIF1 ,(second refcode1)))))
                             (setf (cdr lastc) (list* label jmpop
                                                      (cdr refcode)))
                             (setf (aref *code-parts* jmpcase1-ref) nil)
                             (setq code lastc)))
                          (jmpcase-ref
                           (let* ((refcode (aref *code-parts* jmpcase-ref))
                                  (refcode1 (car refcode))
                                  (for-value (or (get (second refcode1)
                                                      'for-value)
                                                 (get (third refcode1)
                                                      'for-value)))
                                  (jmpop
                                    (if (eq label (second refcode1))
                                      `(JMPIFNOT ,(third refcode1) ,for-value)
                                      `(JMPIF ,(second refcode1) ,for-value))))
                             (setf (cdr lastc)
                                   (list* label jmpop (cdr refcode)))
                             (setf (aref *code-parts* jmpcase-ref) nil)
                             (setq code lastc)))
                          (t ; no attachment possible
                           (return))))))))))) ; go for the next index
    ;; make sure, that the start-piece really makes it to the start:
    ;; (this would also work, by executing a
    ;; (setf (aref *code-positions* index) (aref *code-positions* jmp..-ref))
    ;; for each attachment. Why don't we do that??)
    (setf (aref *code-positions* start-index) 0)
    ;; assemble code-list:
    (let ((code-parts (map 'list #'cons *code-parts* *code-positions*)))
      (setq code-parts (delete-if-not #'car code-parts)) ; code=nil means: canceled
      (setq code-parts (sort code-parts #'> :key #'cdr)) ; sort by order
      ;; the pieces are now in the right order, only reversed.
      (let ((codelist '()))
        (dolist (code-part code-parts)
          (let ((code (car code-part)))
            ;; append code to codelist, thereby convert the leaving-jump:
            (let ((item (car code)))
              (case (first item)
                (JMP (setf (car code) `(JMP ,(second item))))
                (JMPCASE ; (JMPCASE true-label false-label)
                         ; --> (JMPIFNOT false-label fv) (JMP true-label)
                  (setq code
                    (list* `(JMP ,(second item))
                           `(JMPIFNOT ,(third item)
                                      ,(or (get (second item) 'for-value)
                                           (get (third item) 'for-value)))
                           (cdr code))))
                (JMPCASE1-TRUE ; (JMPCASE1-TRUE true-label false-label)
                               ; --> (JMPIF1 true-label) (JMP false-label)
                  (setq code
                    (list* `(JMP ,(third item))
                           `(JMPIF1 ,(second item))
                           (cdr code))))
                (JMPCASE1-FALSE ; (JMPCASE1-FALSE true-label false-label)
                                ; --> (JMPIFNOT1 false-label) (JMP true-label)
                  (setq code
                    (list* `(JMP ,(second item))
                           `(JMPIFNOT1 ,(third item))
                           (cdr code))))))
            ;; turn Label into a list-element:
            (let ((lastc (last code)))
              (when (cdr lastc)
                (setf (cdr lastc) (list (cdr lastc)))))
            ;; convert and prepend in front of codelist (that's why we sorted
            ;; with #'> instead of #'< beforehand):
            (setq codelist (nreconc code codelist))))
        codelist))))

#|
;; Debugging hints:
 (in-package "SYSTEM")
 (setq *print-circle* t *suppress-check-redefinition* t)
 (setq *print-length* 10 *print-level* 3)
 (setq *print-length* nil *print-level* nil)
 (setf (package-lock *system-package-list*) nil)
;; avoid stack overflow in trace output (calls CLOS::INSTALL-DISPATCH)
;; by adding all needed PRINT-OBJECT methods before tracing begins:
 (print (list (mk-anode #+CLISP-DEBUG nil nil #+CLISP-DEBUG nil nil nil
                        #+CLISP-DEBUG nil)
              (make-fnode) (make-block)
              (make-tagbody) (make-var) (make-const) (make-c-source-point)
              (make-signature) (clos::make-class-version) clos::<t>
              #'compile))
 (trace (sys::optimize-label :pre-print (list *code-parts* *trace-args*
  (let* ((label (first *trace-args*))
         (index (or (second *trace-args*) (get label 'code-part)))
         (code (or (third *trace-args*) (aref *code-parts* index)))
         (lastc (or (fourth *trace-args*) (last code))))
    (list label index code lastc)))))
 (trace compile-to-lap)
 (trace (traverse-anode :post-print *code-part*))
 (trace (traverse-anode :post-print (cons *code-part* (reverse (coerce *code-parts* 'list)))))
 (trace (optimize-part    :pre-print *code-parts* :post-print *code-parts*)
        (optimize-label   :pre-print *code-parts* :post-print *code-parts*)
        (optimize-short   :pre-print *code-parts* :post-print *code-parts*)
        (optimize-jmpcase :pre-print *code-parts* :post-print *code-parts*)
        (optimize-value   :pre-print *code-parts* :post-print *code-parts*)
        (coalesce         :pre-print *code-parts* :post-print *code-parts*)
        (optimize-all     :pre-print *code-parts* :post-print *code-parts*))
 (trace simplify)
;; Move out suspect code to a separate file which you load interpreted.

;; Special debugging checks:
 (defun optimize-check ()
   (do ((index 0 (1+ index)))
       ((eql index (fill-pointer *code-parts*)))
     (let ((code (aref *code-parts* index)))
       (when code
         (let* ((lastc (last code))
                (label (cdr lastc)))
           (when label
             (unless (eql index (get label 'code-part))
               (compiler-error 'optimize-check 'code-part))))))))
 (trace
 (optimize-part    :pre (optimize-check) :post (optimize-check) :suppress-if t)
 (optimize-label   :pre (optimize-check) :post (optimize-check) :suppress-if t)
 (optimize-short   :pre (optimize-check) :post (optimize-check) :suppress-if t)
 (optimize-jmpcase :pre (optimize-check) :post (optimize-check) :suppress-if t)
 (optimize-value   :pre (optimize-check) :post (optimize-check) :suppress-if t)
 (coalesce         :pre (optimize-check) :post (optimize-check) :suppress-if t)
 (optimize-all     :pre (optimize-check) :post (optimize-check) :suppress-if t)
)
|#

#| How about the following possible optimizations??

10. If there is an (UNWIND-PROTECT-CLEANUP) in front of (JMP label) and
   if there is a (UNWIND-PROTECT-3 cleanup-label) in front of the label,
   so it must be the same UNWIND-PROTECT-Frame; (UNWIND-PROTECT-CLEANUP)
   can be discarded and (JMP label) can be replaced by (JMP newlabel),
   whereas newlabel is a new label, that is located in front
   of (poss. to be supplemented) (UNWIND-PROTECT-2) ahead of
   cleanup-label:
   (UNWIND-PROTECT-CLEANUP) (JMP label) ...
   ... [(UNWIND-PROTECT-2)] cleanup-label
   ... (UNWIND-PROTECT-3 cleanup-label) label
   -->
   (JMP newlabel) ...
   ... newlabel (UNWIND-PROTECT-2) cleanup-label
   ... (UNWIND-PROTECT-3 cleanup-label) label

11. IF there is a (NIL) after a Label label,  each (JMPIFNOT label)
   and each (JMPIFNOT1 label) can be replaced by a (JMPIFNOT1 z) ,
   with z being a new Label after the (NIL) :
          (JMPIFNOT label) ... label (NIL) ...
   -->       (JMPIFNOT1 z) ... label (NIL) z ...

|#

;; Executes Steps 1, 2 and 3:
(defun compile-to-LAP ()
  (let ((*code-parts* (make-array 10 :adjustable t :fill-pointer 0))
        (*code-positions* (make-array 10 :adjustable t :fill-pointer 0)))
    ;; Expands the Code of Fnode *func* and divides it into pieces.
    ;; leaves behind its values in *code-parts* and *code-positions*.
    (let ((*code-part* (list '(START))) ; NIL as Start-"Label"
          (*code-index* 0)
          (*dead-code* nil)
          (*label-subst* '())
          (*current-value* nil)
          (*current-vars* '()))
      (traverse-anode (anode-code (fnode-code *func*))))
    ;; Optimizes in *code-parts* and *code-positions*, then collects the code
    ;; in a list and returns it:
    (let ((code-list (optimize-all)))
      (unless (equal (pop code-list) '(START))
        (compiler-error 'compile-to-LAP 'start))
      code-list)))


#|
                            4. Step:
                      Elimination of (CONST n)

Generic Functions have a fixed size. The constants are stored
in VENV-Const. Transformations in this step:
  (LOADV k m)    -->  (LOADV k+1 m)
  (STOREV k m)   -->  (STOREV k+1 m)
  (CONST n [c])  -->  (LOADV 0 n)
  (VENV)         -->  (LOADV 0 0)
  (JMPHASH n ht label . labels)  -->  (JMPHASHV n ht label . labels)
  (GETVALUE n)         -->  illegal
  (SETVALUE n)         -->  illegal
  (BIND n)             -->  illegal
  (COPY-CLOSURE m n)   -->  illegal
  (CALL k n)           -->  illegal
  (CALL0 n)            -->  illegal
  (CALL1 n)            -->  illegal
  (CALL2 n)            -->  illegal
  (BLOCK-OPEN n label) -->  illegal
  (RETURN-FROM n)      -->  illegal
  (TAGBODY-OPEN n ...) -->  illegal
  (GO n l)             -->  illegal
|#

(defun CONST-to-LOADV (code-list)
  (do ((codelistr code-list (cdr codelistr)))
      ((null codelistr))
    (let ((item (car codelistr)))
      (when (consp item)
        (case (first item)
          ((LOADV STOREV)
           (setf (car codelistr)
                 `(,(first item) ,(1+ (second item)) ,@(cddr item))))
          (CONST
           (setf (car codelistr) `(LOADV 0 ,(second item))))
          (VENV
           (setf (car codelistr) `(LOADV 0 0)))
          (JMPHASH
           (setf (car codelistr) `(JMPHASHV ,@(cdr item))))
          ((GETVALUE SETVALUE BIND COPY-CLOSURE CALL CALL0 CALL1 CALL2
            BLOCK-OPEN RETURN-FROM TAGBODY-OPEN GO)
            (compiler-error 'CONST-to-LOADV "Illegal-in-GF"))))))
  code-list)


#|
                            5. Step:
                   Calculation of Stack-Demand

This step determines, how many SP-Entries the function needs at most.
|#

(defun SP-depth (code-list)
  ;; We have to compute the maximum SP depth in the two spd dimensions
  ;; separately. Instead of walking through the code twice, we walk only once.
  ;; When we see that a label can be reached with depths (d1 . d2) and (e1 . e2)
  ;; we pretend it can be reached with depth ((max d1 e1) . (max d2 e2)).
  ;; Similarly, when we have tracked a label at depths (d1 . d2) and (e1 . e2),
  ;; we pretend having tracked it at depth ((max d1 e1) . (max d2 e2)).
  ;; This is allowed because we are only interested in the two separate maxima.
  ;; Think of two different machines computing the two maxima in parallel.
  (let ((max-depth-1 0) (max-depth-2 0) ; Maximum-Depth so far
        (unseen-label-alist '()) ; Labels, that have not yet been tracked
        (seen-label-alist '()) ; Labels, that have already been tracked
         ;; both Alists ((label . depth) ...)
         ;; It is absolutely possible, that the same code-piece can be executed
         ;; with different SP-Depths (namely, when it ends with
         ;; a leaving-jump THROW, RETURN-FROM, RETURN-FROM-I, GO, GO-I
         ;; or BARRIER)!
         ;; seen-label-alist contains for each label the maximum-depth, with
         ;; which the tracking took place beginning from this Label.
         ;; unseen-label-alist contains for each Label the maximum noted
         ;; depth so far, with which tracking has still to take place
         ;; beginning from this label.
        (middle code-list) ; remaining code-list
        (depth (spd 0 0))) ; current depth
    (macrolet ((check-depth (wanted-depth)
                 ;; checks, if depth equals the depth wanted-depth
                 `(unless (equal depth ,wanted-depth)
                    (compiler-error 'SP-depth (cons depth ,wanted-depth)))))
      (loop
        ;; middle traverses the code-list, from the current position
        ;; to the next leaving-jump, and counts the depth.
        (loop
          (when (null middle) (return))
          (let ((item (car middle)))
            (if (atom item)
              ;; Label
              (let ((h (assoc item seen-label-alist)))
                (if h
                  (if (spd<= depth (cdr h))
                    (return)
                    (setf (cdr h) (setf depth (spdmax depth (cdr h)))))
                  (push (cons item depth) seen-label-alist)))
              ;; Instruction
              (macrolet ((note-label (labelform)
                           ; notice, that label can be reached by jump
                           (let ((label (gensym)))
                             `(let* ((,label ,labelform)
                                     (h (assoc ,label seen-label-alist)))
                                (unless (and h (spd<= depth (cdr h)))
                                  (let ((depth
                                          (if h (spdmax depth (cdr h)) depth)))
                                    (setq h (assoc ,label unseen-label-alist))
                                    (if h
                                      (unless (spd<= depth (cdr h))
                                        (setf (cdr h) (spdmax depth (cdr h))))
                                      (push (cons ,label depth)
                                            unseen-label-alist)))))))
                         (note-inc (amount)
                           ;; notice, that depth can be increased by amount
                           `(progn
                             (setq depth (spd+ depth ,amount))
                             (setq max-depth-1 (max max-depth-1 (car depth)))
                             (setq max-depth-2 (max max-depth-2 (cdr depth)))))
                         (note-dec (amount)
                           ;; notice, that depth can be decreased by amount
                           `(progn
                              (setq depth (spd- depth ,amount))
                              (when (or (minusp (car depth))
                                        (minusp (cdr depth)))
                                (compiler-error 'SP-depth "<0"))))
                         (note-jmp ()
                           ;; notice leaving-jump
                           `(return)))
                (case (first item)
                  (JMP ; (JMP label)
                    (note-label (second item))
                    (note-jmp))
                  ((JMPIF JMPIF1 JMPIFNOT JMPIFNOT1) ; (JMP... label)
                    (note-label (second item)))
                  (JMPIFBOUNDP ; (JMPIFBOUNDP n label)
                    (note-label (third item)))
                  ((JMPHASH JMPHASHV JMPTAIL) ; (JMPHASH.. n ht label . labels), (JMPTAIL m n label)
                    (dolist (label (cdddr item)) (note-label label))
                    (note-jmp))
                  (JSR ; (JSR n label)
                    (let ((depth (spd 0 0))) (note-label (third item))))
                  ((BARRIER THROW RETURN-FROM RETURN-FROM-I GO GO-I) ; (BARRIER), (THROW), (RETURN-FROM n), (RETURN-FROM-I k n), (GO n l), (GO-I k n l)
                    (note-jmp))
                  ((RET RETGF) ; (RET), (RETGF)
                    (check-depth (spd 0 0))
                    (note-jmp))
                  (PROGV ; (PROGV)
                    (note-inc (spd 1 0)))
                  (CATCH-OPEN ; (CATCH-OPEN label)
                    (note-label (second item))
                    (note-inc (spd 2 1)))
                  (CATCH-CLOSE ; (CATCH-CLOSE)
                    (note-dec (spd 2 1)))
                  (UNWIND-PROTECT-OPEN ; (UNWIND-PROTECT-OPEN label)
                    ; actually: (note-inc (spd 2 1))
                    (note-inc (spd 3 0)) (note-label (second item))
                    (note-dec (spd 3 0)) (note-inc (spd 2 1)))
                  (UNWIND-PROTECT-NORMAL-EXIT ; (UNWIND-PROTECT-NORMAL-EXIT), then comes label
                    (note-dec (spd 2 1)) (note-inc (spd 3 0)))
                  (UNWIND-PROTECT-CLOSE ; (UNWIND-PROTECT-CLOSE label)
                    ; actually: (note-dec (spd 3 0))
                    (note-label (second item)) (note-dec (spd 3 0)))
                  (UNWIND-PROTECT-CLEANUP ; (UNWIND-PROTECT-CLEANUP)
                    ; actually: (note-dec (spd 2 1)) (note-inc (spd 3 0)) ... (note-dec (spd 3 0))
                    (note-dec (spd 2 1)))
                  (BLOCK-OPEN ; (BLOCK-OPEN n label)
                    (note-label (third item))
                    (note-inc (spd 2 1)))
                  (BLOCK-CLOSE ; (BLOCK-CLOSE)
                    (note-dec (spd 2 1)))
                  (TAGBODY-OPEN ; (TAGBODY-OPEN n label1 ... labelm)
                    (note-inc (spd 1 1))
                    (dolist (label (cddr item)) (note-label label)))
                  ((TAGBODY-CLOSE-NIL TAGBODY-CLOSE) ; (TAGBODY-CLOSE-NIL), (TAGBODY-CLOSE)
                    (note-dec (spd 1 1)))
                  (HANDLER-OPEN ; (HANDLER-OPEN n v k label1 ... labelm)
                    (check-depth (fourth item))
                    (dolist (label (cddddr item)) (note-label label)))
                  ((MVCALLP HANDLER-BEGIN) ; (MVCALLP), (HANDLER-BEGIN)
                    (note-inc (spd 1 0)))
                  (MVCALL ; (MVCALL)
                    (note-dec (spd 1 0)))
                  (SKIPSP ; (SKIPSP k1 k2)
                    (note-dec (spd (second item) (third item))))
                  (SKIPI ; (SKIPI k1 k2 n)
                    (note-dec (spd (+ (second item) 1) (third item))))))))
          (setq middle (cdr middle)))
        ;; search next label to track:
        (loop
          (when (null unseen-label-alist) ; finished?
            (return-from SP-depth (spd max-depth-1 max-depth-2)))
          (let* ((unseen (pop unseen-label-alist))
                 (label (car unseen))) ; next label to track
            (unless (symbolp label) (compiler-error 'SP-depth "BAD LABEL"))
            (setq depth (cdr unseen))
            (let ((h (assoc label seen-label-alist)))
              (unless (and h (spd<= depth (cdr h)))
                (when h (setq depth (spdmax depth (cdr h))))
                ;; Starting at this label, process the code-list:
                ;; (Thereby (label . depth) is added to seen-label-alist,
                ;; it is already removed from unseen-label-alist.)
                (setq middle (memq label code-list))
                (return)))))))))


#|
                            6. Step:
                 Introduction of Short-Operations

This step works on the code-list and changes is destructively.

1. (ATOM) (JMPIF label NIL)             --> (JMPIFATOM label)
   (ATOM) (JMPIFNOT label NIL)          --> (JMPIFCONSP label)
   (CONSP) (JMPIF label NIL)            --> (JMPIFCONSP label)
   (CONSP) (JMPIFNOT label NIL)         --> (JMPIFATOM label)
   (ATOM)                               --> (PUSH) (CALLS ATOM)
   (CONSP)                              --> (PUSH) (CALLS CONSP)

2. (NIL) (PUSH)                         --> (NIL&PUSH)
   (NIL) (PUSH) ... (NIL) (PUSH)        --> (PUSH-NIL n)
   (NIL) (STORE n)                      --> (NIL&STORE n)
   (PUSH-NIL 1)                         --> (NIL&PUSH)

3. (T) (PUSH)                           --> (T&PUSH)
   (T) (STORE n)                        --> (T&STORE n)

4. (CONST n c)                          --> (CONST n)
   (CONST n) (PUSH)                     --> (CONST&PUSH n)
   (CONST n) (SYMBOL-FUNCTION denv) (PUSH)    --> (CONST&SYMBOL-FUNCTION&PUSH n)
   (CONST n) (SYMBOL-FUNCTION denv) (STORE m) --> (CONST&SYMBOL-FUNCTION&STORE n m)
   (CONST n) (SYMBOL-FUNCTION denv)     --> (CONST&SYMBOL-FUNCTION n)

5. (COPY-CLOSURE n m) (PUSH)            --> (COPY-CLOSURE&PUSH n m)

6. (LOAD n) (PUSH)                      --> (LOAD&PUSH n)
   (LOAD k) (STOREC n m)                --> (LOAD&STOREC k n m)
   (LOAD n) (JMPIF label fv)            --> (LOAD&JMPIF n label)
   (LOAD n) (JMPIFNOT label fv)         --> (LOAD&JMPIFNOT n label)
   (LOAD n) (CAR denv) (PUSH)           --> (LOAD&CAR&PUSH n)
   (LOAD n) (CDR denv) (PUSH)           --> (LOAD&CDR&PUSH n)
   (LOAD n) (CDR denv) (STORE n)        --> (LOAD&CDR&STORE n)
   (LOAD n+1) (CONS) (STORE n)          --> (LOAD&CONS&STORE n)
   (LOAD n) (PUSH) (CALLS 1+) (STORE n) --> (LOAD&INC&STORE n)
   (LOAD n) (PUSH) (CALLS 1-) (STORE n) --> (LOAD&DEC&STORE n)
   (LOAD n) (PUSH) (CALLS 1+) (PUSH)    --> (LOAD&INC&PUSH n)
   (LOAD n) (PUSH) (CALLS 1-) (PUSH)    --> (LOAD&DEC&PUSH n)
   (LOAD n) (CAR denv) (STORE m)        --> (LOAD&CAR&STORE n m)

7. (JMPIFBOUNDP n l) (NIL) (STORE n) l  --> (UNBOUND->NIL n) l

8. (LOADI n1 n2 n3) (PUSH)              --> (LOADI&PUSH n1 n2 n3)
   (LOADC n1 n2) (PUSH)                 --> (LOADC&PUSH n1 n2)
   (LOADV n1 n2) (PUSH)                 --> (LOADV&PUSH n1 n2)

9. (GETVALUE n) (PUSH)                  --> (GETVALUE&PUSH n)

10. (UNBIND1) ... (UNBIND1)             --> (UNBIND n)

11. (CAR denv) (PUSH)                   --> (CAR&PUSH)
    (CDR denv) (PUSH)                   --> (CDR&PUSH)
    (CONS) (PUSH)                       --> (CONS&PUSH)
    (LIST n) (PUSH)                     --> (LIST&PUSH n)
    (LIST* n) (PUSH)                    --> (LIST*&PUSH n)
    (FUNCALL n) (PUS)                   --> (FUNCALL&PUSH n)
    (APPLY n) (PUSH)                    --> (APPLY&PUSH n)

12. (POP) (STORE n)                     --> (POP&STORE n)

13. (SKIP n) (RET)                      --> (SKIP&RET n)
    (SKIP n) (RETGF)                    --> (SKIP&RETGF n)
    (RET)                               --> (SKIP&RET 0)
    (RETGF)                             --> (SKIP&RETGF 0)
    ;; the last 2 can occur despite the Closure itself being still in
    ;; the STACK because the preceding SKIP might have been folded into
    ;; the previous SKIPI

14. (UNWIND-PROTECT-CLOSE label)        --> (UNWIND-PROTECT-CLOSE)

15. (JMPHASH n ht label . labels)       --> (JMPHASH n ht label)
    (JMPHASHV n ht label . labels)      --> (JMPHASHV n ht label)

16. (JSR n label)                       --> (JSR label)
    (JSR n label) (PUSH)                --> (JSR&PUSH label)

17. (CALL m n) (PUSH)                   --> (CALL&PUSH m n)
    (CALL1 n) (PUSH)                    --> (CALL1&PUSH n)
    (CALL2 n) (PUSH)                    --> (CALL2&PUSH n)
    (CALLS1 n) (PUSH)                   --> (CALLS1&PUSH n)
    (CALLS2 n) (PUSH)                   --> (CALLS2&PUSH n)
    (CALLSR m n) (PUSH)                 --> (CALLSR&PUSH m n)
    (CALLC) (PUSH)                      --> (CALLC&PUSH)
    (CALLCKEY) (PUSH)                   --> (CALLCKEY&PUSH)

18. (CALL1 n) (JMPIF label fv)          --> (CALL1&JMPIF n label)
    (CALL1 n) (JMPIFNOT label fv)       --> (CALL1&JMPIFNOT n label)
    (CALL2 n) (JMPIF label fv)          --> (CALL2&JMPIF n label)
    (CALL2 n) (JMPIFNOT label fv)       --> (CALL2&JMPIFNOT n label)
    (CALLS1 n) (JMPIF label fv)         --> (CALLS1&JMPIF n label)
    (CALLS1 n) (JMPIFNOT label fv)      --> (CALLS1&JMPIFNOT n label)
    (CALLS2 n) (JMPIF label fv)         --> (CALLS2&JMPIF n label)
    (CALLS2 n) (JMPIFNOT label fv)      --> (CALLS2&JMPIFNOT n label)
    (CALLSR m n) (JMPIF label fv)       --> (CALLSR&JMPIF m n label)
    (CALLSR m n) (JMPIFNOT label fv)    --> (CALLSR&JMPIFNOT m n label)

19. (CALLS1 n) (STORE k)                --> (CALLS1&STORE n k)
    (CALLS2 n) (STORE k)                --> (CALLS2&STORE n k)
    (CALLSR m n) (STORE k)              --> (CALLSR&STORE m n k)

20. (EQ) (JMPIF label NIL)              --> (JMPIFEQ label)
    (EQ) (JMPIFNOT label NIL)           --> (JMPIFNOTEQ label)
    (CONST n) (EQ) (JMPIF label NIL)    --> (JMPIFEQTO n label)
    (CONST n) (EQ) (JMPIFNOT label NIL) --> (JMPIFNOTEQTO n label)

21. (APPLY n) (SKIP k) (RET)            --> (APPLY&SKIP&RET n k)
    (FUNCALL n) (SKIP k) (RETGF)        --> (FUNCALL&SKIP&RETGF n k)

22. (HANDLER-BEGIN) (PUSH)              --> (HANDLER-BEGIN&PUSH)

23. (BARRIER)                           -->

|#

(defun insert-combined-LAPs (code-list)
  ;; First the ATOM/CONSP-Conversion, because it can introduce PUSHs:
  (do ((crest code-list (cdr crest)))
      ((null crest))
    (let ((item (car crest)))
      (when (consp item)
        (case (first item)
          (CONST ; (CONST n c) -> (CONST n)
           (setf (cddr item) '()))
          ((ATOM CONSP)
           (setq item (first item))
           (if (and #| (consp (cdr crest)) |#
                   (consp (cadr crest))
                   (memq (first (cadr crest)) '(JMPIF JMPIFNOT))
                   (null (third (cadr crest))))
             ;; e.g. (ATOM) (JMPIF label NIL) --> (JMPIFATOM label)
             (setf (car crest)
                   `(,(if (eq (first (cadr crest)) 'JMPIF)
                          (if (eq item 'ATOM) 'JMPIFATOM 'JMPIFCONSP)
                          (if (eq item 'ATOM) 'JMPIFCONSP 'JMPIFATOM))
                      ,(second (cadr crest)))
                   (cdr crest) (cddr crest))
             ;; e.g. (ATOM) --> (PUSH) (CALLS ATOM)
             (setf (car crest) '(PUSH)
                   (cdr crest) (cons (if (eq item 'ATOM)
                                       (CALLS-code-fun atom)
                                       (CALLS-code-fun consp))
                                     (cdr crest)))))))))
  ;; Now the other Conversions: One single run.
  ;; Two pointers loop through the code-list: ...middle.right...
  (do* ((middle code-list right)
        (right (cdr middle) (cdr right)))
       ((null middle))
    (macrolet ((ersetze (length new-code)
                 ;; replaces the next length elements
                 ;; (nth 0 middle) ... (nth (- length 1) middle)
                 ;; by one single element new-code.
                 (assert (typep length '(INTEGER 0 4)))
                 `(progn
                    ,(case length
                       (0 `(setf (cdr middle)
                                 (setq right (cons (car middle) right))
                                 (car middle) ,new-code))
                       (1 `(setf (car middle) ,new-code))
                       (t `(setf (car middle) ,new-code
                                 (cdr middle) (setq right
                                                    ,(case length
                                                       (2 '(cdr right))
                                                       (3 '(cddr right))
                                                       (4 '(cdddr right)))))))
                    (go next))))
      (let ((item (car middle)))
        (when (consp item)
          ;; analysis of the instruction item and the consecutive ones:
          (when (and #| (consp right) |# (consp (car right)))
            ;; normal conversions, with chaining of the arguments:
            (let ((new-op
                    (cdr (assoc (first item)
                                (case (first (car right))
                                  (PUSH  '((T        . T&PUSH)
                                           (CONST    . CONST&PUSH)
                                           (LOADI    . LOADI&PUSH)
                                           (LOADC    . LOADC&PUSH)
                                           (LOADV    . LOADV&PUSH)
                                           (GETVALUE . GETVALUE&PUSH)
                                           (CALL     . CALL&PUSH)
                                           (CALL1    . CALL1&PUSH)
                                           (CALL2    . CALL2&PUSH)
                                           (CALLS1   . CALLS1&PUSH)
                                           (CALLS2   . CALLS2&PUSH)
                                           (CALLSR   . CALLSR&PUSH)
                                           (CALLC    . CALLC&PUSH)
                                           (CALLCKEY . CALLCKEY&PUSH)
                                           (CAR      . CAR&PUSH)
                                           (CDR      . CDR&PUSH)
                                           (CONS     . CONS&PUSH)
                                           (LIST     . LIST&PUSH)
                                           (LIST*    . LIST*&PUSH)
                                           (FUNCALL  . FUNCALL&PUSH)
                                           (APPLY    . APPLY&PUSH)
                                           (COPY-CLOSURE . COPY-CLOSURE&PUSH)
                                           (HANDLER-BEGIN . HANDLER-BEGIN&PUSH)))
                                  (JMPIF
                                   (let ((alist
                                           '((EQ     . JMPIFEQ)
                                             (LOAD   . LOAD&JMPIF)
                                             (CALL1  . CALL1&JMPIF)
                                             (CALL2  . CALL2&JMPIF)
                                             (CALLS1 . CALLS1&JMPIF)
                                             (CALLS2 . CALLS2&JMPIF)
                                             (CALLSR . CALLSR&JMPIF))))
                                     (when (third (car right))
                                       (setq alist (cdr alist)))
                                     (setf (cddr (car right)) '())
                                     alist))
                                  (JMPIFNOT
                                   (let ((alist
                                           '((EQ     . JMPIFNOTEQ)
                                             (LOAD   . LOAD&JMPIFNOT)
                                             (CALL1  . CALL1&JMPIFNOT)
                                             (CALL2  . CALL2&JMPIFNOT)
                                             (CALLS1 . CALLS1&JMPIFNOT)
                                             (CALLS2 . CALLS2&JMPIFNOT)
                                             (CALLSR . CALLSR&JMPIFNOT))))
                                     (when (third (car right))
                                       (setq alist (cdr alist)))
                                     (setf (cddr (car right)) '())
                                     alist))
                                 (STORE '((NIL    . NIL&STORE)
                                          (T      . T&STORE)
                                          (POP    . POP&STORE)
                                          (CALLS1 . CALLS1&STORE)
                                          (CALLS2 . CALLS2&STORE)
                                          (CALLSR . CALLSR&STORE)))
                                 (STOREC '((LOAD . LOAD&STOREC)))
                                 (RET '((SKIP . SKIP&RET)))
                                 (RETGF '((SKIP . SKIP&RETGF))))
                               :test #'eq))))
              (when new-op
                (ersetze 2 `(,new-op ,@(rest item) ,@(rest (car right)))))))
          ;; further conversions:
          (case (first item)
            ((NIL PUSH-NIL)
             (flet ((nilpusher-p (coder)
                      ;; if (NIL) (PUSH) --> 1,
                      ;; if (PUSH-NIL n) --> n,
                      ;; else nil.
                      (and #| (consp coder) |# (consp (car coder))
                           (case (first (car coder))
                             (PUSH-NIL (second (car coder)))
                             ((NIL) (when (equal (cadr coder) '(PUSH))
                                      (setf (cdr coder) (cddr coder))
                                      1))
                             (t nil)))))
               (let ((count (nilpusher-p middle)))
                 (when count
                   (setq right (cdr middle))
                   (loop
                     (let ((next-count (nilpusher-p right)))
                       (unless next-count (return))
                       (incf count next-count))
                     (setq right (cdr right)))
                   (setf (car middle)
                         (if (eql count 1) '(NIL&PUSH) `(PUSH-NIL ,count))
                         (cdr middle) right)
                   (go next)))))
            (CONST
             (when (and #| (consp right) |# (consp (car right)))
               (case (first (car right))
                 (SYMBOL-FUNCTION
                  (let ((n (second item)))
                    (cond ((and #| (consp (cdr right)) |#
                            (equal (cadr right) '(PUSH)))
                           (ersetze 3 `(CONST&SYMBOL-FUNCTION&PUSH ,n)))
                          ((and #| (consp (cdr right)) |#
                            (consp (cadr right))
                            (eq (first (cadr right)) 'STORE))
                           (ersetze 3 `(CONST&SYMBOL-FUNCTION&STORE
                                        ,n ,(second (cadr right)))))
                          (t (ersetze 2 `(CONST&SYMBOL-FUNCTION ,n))))))
                 (EQ
                  (when (and #| (consp (cdr right)) |#
                             (consp (cadr right))
                             (memq (first (cadr right)) '(JMPIF JMPIFNOT))
                             (null (third (cadr right))))
                    (ersetze 3 `(,(if (eq (first (cadr right)) 'JMPIF)
                                    'JMPIFEQTO
                                    'JMPIFNOTEQTO)
                                 ,(second item)
                                 ,(second (cadr right)))))))))
            (LOAD
             (when (and #| (consp right) |# (consp (car right)))
               (let ((n (second item)))
                 (case (first (car right))
                   (CAR
                    (when (and #| (consp (cdr right)) |# (consp (cadr right)))
                      (case (first (cadr right))
                        (PUSH (ersetze 3 `(LOAD&CAR&PUSH ,n)))
                        (STORE
                         (ersetze 3 `(LOAD&CAR&STORE
                                      ,n ,(second (cadr right))))))))
                   (CDR
                    (when (and #| (consp (cdr right)) |# (consp (cadr right)))
                      (case (first (cadr right))
                        (PUSH (ersetze 3 `(LOAD&CDR&PUSH ,n)))
                        (STORE
                         (when (eql n (second (cadr right)))
                           (ersetze 3 `(LOAD&CDR&STORE ,n)))))))
                   (CONS
                    (when (and #| (consp (cdr right)) |# (consp (cadr right))
                               (eq (first (cadr right)) 'STORE)
                               (eql (second (cadr right)) (- n 1)))
                      (ersetze 3 `(LOAD&CONS&STORE ,(- n 1)))))
                   (PUSH
                    (when (and #| (consp (cdr right)) |# (consp (cadr right))
                               (or (equal (cadr right) (CALLS-code-fun 1+))
                                   (equal (cadr right) (CALLS-code-fun 1-)))
                               #| (consp (cddr right)) |# (consp (caddr right)))
                      (when (equal (caddr right) '(PUSH))
                        (ersetze 4 `(,(if (equal (cadr right)
                                                 (CALLS-code-fun 1+))
                                        'LOAD&INC&PUSH
                                        'LOAD&DEC&PUSH)
                                      ,n)))
                      (when (and (eq (first (caddr right)) 'STORE)
                                 (eql (second (caddr right)) n))
                        (ersetze 4 `(,(if (equal (cadr right)
                                                 (CALLS-code-fun 1+))
                                        'LOAD&INC&STORE
                                        'LOAD&DEC&STORE)
                                      ,n))))
                    (ersetze 2 `(LOAD&PUSH ,n)))))))
            (JMPIFBOUNDP ; simplify (JMPIFBOUNDP n l) (NIL) (STORE n) l
             (when (and #| (consp right) |#
                        (equal (car right) '(NIL))
                        #| (consp (cdr right)) |#
                        (consp (cadr right))
                        (eq (first (cadr right)) 'STORE)
                        (eql (second (cadr right)) (second item))
                        #| (consp (cddr right)) |#
                        (eq (caddr right) (third item)))
               (ersetze 3 `(UNBOUND->NIL ,(second item)))))
            (JSR
             (if (and #| (consp right) |# (equal (car right) '(PUSH)))
               (ersetze 2 `(JSR&PUSH ,(third item)))
               (ersetze 1 `(JSR ,(third item)))))
            (UNBIND1
             (let ((count 1))
               (loop
                 (unless (and #| (consp right) |#
                              (equal (car right) '(UNBIND1)))
                   (return))
                 (incf count)
                 (setq right (cdr right)))
               (unless (eql count 1)
                 (setf (car middle) `(UNBIND ,count))
                 (setf (cdr middle) right)
                 (go next))))
            ;; We need these two rules because (RET) and (RETGF) are not
            ;; always preceded by (SKIP n); they can also be preceded by
            ;; (SKIPI k1 k2 n) with n >= 1.
            (RET (ersetze 1 '(SKIP&RET 0)))
            (RETGF (ersetze 1 '(SKIP&RETGF 0)))
            (UNWIND-PROTECT-CLOSE (ersetze 1 '(UNWIND-PROTECT-CLOSE)))
            ((JMPIF JMPIFNOT) (ersetze 1 `(,(first item) ,(second item))))
            ((JMPHASH JMPHASHV)
             (let ((hashtable (third item))
                   (labels (cddddr item)))
               (maphash
                #'(lambda (obj index) ; (gethash obj hashtable) = index
                    (setf (gethash obj hashtable) (nth index labels)))
                hashtable))
             (setf (cddddr item) '()))
            (HANDLER-OPEN
             (do ((v (third item))
                  (labels (cddddr item) (cdr labels))
                  (i 1 (+ i 2)))
                 ((null labels))
               (setf (svref v i) (car labels)))
             (setf (cdddr item) '()))
            (APPLY
             (when (and #| (consp right) |#
                        (consp (car right))
                        (eq (first (car right)) 'SKIP)
                        #| (consp (cdr right)) |#
                        (equal (cadr right) '(RET)))
               (ersetze 3 `(APPLY&SKIP&RET ,(second item)
                                           ,(second (car right))))))
            (FUNCALL
             (when (and #| (consp right) |#
                        (consp (car right))
                        (eq (first (car right)) 'SKIP)
                        #| (consp (cdr right)) |#
                        (equal (cadr right) '(RETGF)))
               (ersetze 3 `(FUNCALL&SKIP&RETGF ,(second item)
                                               ,(second (car right))))))))))
   next ; Here we are finished with (car middle) .
    (when (equal (car right) '(BARRIER))
      ;; discard element (car right)
      (setf (cdr middle) (setq right (cdr right)))))
  code-list)


#|
                                7. Step:
                Conversion of Instructions into a Byte-Sequence

First sub-step: each instruction is prepended by a classification of the
instruction and the length of the instruction (Label-Operands not counted),
each Label is assigned its PC as value.
The operand-lenghts - as far as possible - are determined, Labels occurring in
instructions are replaced by (presumable reference-length . label) .
Thus (BLOCK-OPEN 2 #:G7) --> (NL 2 . (67 2 (1 . #:G7))) .
Further sub-steps:
The code-list is looped over and over again,
with lengthening the jump-references possibly from 1 to 2 or 6 Byte.
Here the code can only be lenghtened, altogether.
Last sub-step:
The jump-references are transformed into distances, and the code-list is
freshly rebuilt as list of bytes.
|#
;; indicates, how many bytes a numeric operand needs:
(defun num-operand-length (n)
  (cond ((< n 128) 1) ; 7 Bit in 1 Byte
        ((< n 32768) 2) ; 15 Bit in 2 Bytes
        (t 6))) ; else 6 Bytes
(defun signed-operand-length (n)
  (cond ((<= -64 n 63) 1) ; 7 Bits in 1 Byte
        ((<= -16384 n 16383) 2) ; 15 Bits in 2 Bytes
        (t 6))) ; else 32 Bits in 6 Bytes
;; assembles a Code-List and returns the Bytecode-List:
(defun assemble-LAP (code-list)
  ; first sub-step:
  (do ((code-listr code-list (cdr code-listr))
       (PC 0))
      ((null code-listr))
    (let ((item (car code-listr)))
      (if (atom item)
        (setf (symbol-value item) PC)
        (let ((instr-code (gethash (first item) instruction-codes)))
          (unless instr-code (compiler-error 'assemble-LAP item))
          (let ((instr-class (second (svref instruction-table instr-code)))
                (instr-length 1))
            (if (and (eq instr-class 'K)
                     (< (second item)
                        (svref short-code-opsize
                               (position (first item) instruction-table-K))))
              (progn
                (setq instr-code
                  (+ (svref short-code-ops
                            (position (first item) instruction-table-K))
                     (second item)))
                (setq instr-class 'O)
                (setq item (list (first item))))
              (case instr-class
                (O)
                ((K N NC) (incf instr-length (num-operand-length
                                              (second item))))
                (B (incf instr-length 1))
                (L (incf PC 1) (push 1 (second item)))
                (NN (incf instr-length (num-operand-length (second item)))
                    (incf instr-length (num-operand-length (third item))) )
                (NB (incf instr-length (num-operand-length (second item)))
                    (incf instr-length 1) )
                (BN (incf instr-length 1)
                    (incf instr-length (num-operand-length (third item))) )
                (NNN (incf instr-length (num-operand-length (second item)))
                     (incf instr-length (num-operand-length (third item)))
                     (incf instr-length (num-operand-length (fourth item))) )
                (NBN (incf instr-length (num-operand-length (second item)))
                     (incf instr-length 1)
                     (incf instr-length (num-operand-length (fourth item))) )
                (NNNN (incf instr-length (num-operand-length (second item)))
                      (incf instr-length (num-operand-length (third item)))
                      (incf instr-length (num-operand-length (fourth item)))
                      (incf instr-length (num-operand-length (fifth item))) )
                (NL (incf instr-length (num-operand-length (second item)))
                    (incf PC 1) (push 1 (third item)) )
                (BL (incf instr-length 1)
                    (incf PC 1) (push 1 (third item)) )
                (NNL (incf instr-length (num-operand-length (second item)))
                     (incf instr-length (num-operand-length (third item)))
                     (incf PC 1) (push 1 (fourth item)) )
                (NBL (incf instr-length (num-operand-length (second item)))
                     (incf instr-length 1)
                     (incf PC 1) (push 1 (fourth item)) )
                (NHL (incf instr-length (num-operand-length (second item)))
                     (incf PC 1) (push 1 (fourth item)) )
                (NLX (incf instr-length (num-operand-length (second item)))
                     (do ((L (cddr item) (cdr L)))
                         ((null L))
                       (incf PC 1) (push 1 (car L))))))
            (incf PC instr-length)
            (setf (car code-listr)
              (list* instr-class instr-length instr-code (cdr item))))))))
  ;; Further sub-steps.
  ;; Here we stretch the code by making the jump lengths longer, as needed.
  (loop
    ;; When there are no more changes in this step, the jump lengths are
    ;; optimal.
    (unless
      (let ((modified nil) (oldPC 0) (PC 0))
        (dolist (item code-list)
          (if (atom item)
            (progn
              (setf (symbol-value item) PC)
              (setf (get item 'seen) t))
            (progn
              (incf oldPC (cadr item))
              (incf PC (cadr item))
              (when (memq (car item) '(L NL BL NNL NBL NHL NLX))
                (let ((itemargs (cdddr item)))
                  (dolist (x (case (car item)
                               (L itemargs)
                               ((NL BL NLX) (cdr itemargs))
                               ((NNL NBL NHL) (cddr itemargs))))
                    (incf oldPC (car x))
                    (incf PC (car x))
                    (let* ((targetlabel (cdr x))
                           (targetPC
                             (if (get targetlabel 'seen)
                               ; backward jump: targetlabel already updated
                               (symbol-value targetlabel)
                               ; forward jump: use minimal estimate
                               (+ (symbol-value targetlabel) (- PC oldPC))))
                           (new-dist (- targetPC PC))
                           (new-jump-length (signed-operand-length new-dist))
                           (jump-length-diff (- new-jump-length (car x))))
                      ;; Synchronize the so far assumed jump-length and
                      ;; the newly calculated one.
                      ;; We have started with jumps of length 1 in the first
                      ;; sub-step, therefore the jump-lengths can increase.
                      ;; They cannot decrease.
                      (when (minusp jump-length-diff)
                        (compiler-error 'assemble-LAP "STRETCH < 0"))
                      (when (plusp jump-length-diff)
                        (setq modified t)
                        (incf PC jump-length-diff)
                        (setf (car x) new-jump-length)))))))))
        (dolist (item code-list)
          (when (atom item)
            (remprop item 'seen)))
        modified)
      (return)))
  ;; last sub-step:
  (let ((byte-list '()) (PC 0))
    (flet ((new-byte (n) (push n byte-list)))
      (flet ((num-operand (n)
               (cond ((< n 128) (new-byte n))
                     ((< n 32768) (new-byte (+ 128 (ldb (byte 7 8) n)))
                                  (new-byte (ldb (byte 8 0) n)))
                     (t (compiler-error 'assemble-LAP "15 BIT"))))
             (label-operand (x)
               (incf PC (car x))
               (let ((dist (- (symbol-value (cdr x)) PC)))
                 (case (car x)
                   (1 (new-byte (ldb (byte 7 0) dist)))
                   (2 (when (zerop dist)
                        (compiler-error 'assemble-LAP "ZERO JUMP"))
                      (new-byte (+ 128 (ldb (byte 7 8) dist)))
                      (new-byte (ldb (byte 8 0) dist)))
                   (6 (new-byte 128) (new-byte 0)
                      (new-byte (ldb (byte 8 24) dist))
                      (new-byte (ldb (byte 8 16) dist))
                      (new-byte (ldb (byte 8 8) dist))
                      (new-byte (ldb (byte 8 0) dist)))))))
        (dolist (item code-list)
          (when (consp item)
            (incf PC (cadr item))
            (new-byte (caddr item))
            (case (car item)
              (O) ; including the 1-Byte-Instructions of Type K
              ((K N) (num-operand (second (cddr item))))
              (B (new-byte (second (cddr item))))
              (L (label-operand (second (cddr item))))
              (NN (num-operand (second (cddr item)))
                  (num-operand (third (cddr item))) )
              (NB (num-operand (second (cddr item)))
                  (new-byte (third (cddr item))) )
              (BN (new-byte (second (cddr item)))
                  (num-operand (third (cddr item))) )
              (NNN (num-operand (second (cddr item)))
                   (num-operand (third (cddr item)))
                   (num-operand (fourth (cddr item))) )
              (NBN (num-operand (second (cddr item)))
                   (new-byte (third (cddr item)))
                   (num-operand (fourth (cddr item))) )
              (NNNN (num-operand (second (cddr item)))
                    (num-operand (third (cddr item)))
                    (num-operand (fourth (cddr item)))
                    (num-operand (fifth (cddr item))) )
              (NL (num-operand (second (cddr item)))
                  (label-operand (third (cddr item))) )
              (BL (new-byte (second (cddr item)))
                  (label-operand (third (cddr item))) )
              (NNL (num-operand (second (cddr item)))
                   (num-operand (third (cddr item)))
                   (label-operand (fourth (cddr item))) )
              (NBL (num-operand (second (cddr item)))
                   (new-byte (third (cddr item)))
                   (label-operand (fourth (cddr item))) )
              (NHL (num-operand (second (cddr item)))
                   (let ((ht (third (cddr item))))
                     (maphash
                       #'(lambda (obj x) ; x = (gethash obj ht)
                           (setf (gethash obj ht) (- (symbol-value x) PC)))
                       ht))
                   (label-operand (fourth (cddr item))))
              (NC (num-operand (second (cddr item)))
                  (let* ((v (third (cddr item)))
                         (m (length v)))
                    (do ((i 1 (+ i 2)))
                        ((>= i m))
                      (setf (svref v i) (symbol-value (svref v i))))))
              (NLX (num-operand (second (cddr item)))
                   (dolist (x (cddr (cddr item))) (label-operand x)) ))))))
    (nreverse byte-list)))

;; the reversion of assemble-LAP : returns for a bytecode-list the belonging
;; code-list. In this, however, each item is prepended by the PC.
(defun disassemble-LAP (byte-list const-list)
  (let ((code-list '()) (PC 0) instr-PC (label-alist '()))
    ; label-alist is a list of Conses (PC . label), in which the PCs are in
    ; strictly decreasing order.
    (flet ((PC->label-a (PC)
             (cons PC (make-symbol (string-concat "L" (prin1-to-string PC)))))
           (next-byte () (incf PC) (pop byte-list)))
      (flet ((num-operand ()
               (let ((a (next-byte)))
                 (cond ((< a 128) a)
                       (t (+ (* 256 (- a 128)) (next-byte))))))
             (label-operand
                  (&optional
                    (dist
                      (let ((a (next-byte)))
                        (cond ((< a 128) (if (< a 64) a (- a 128)))
                              (t (setq a (- a 128))
                                 (unless (< a 64) (setq a (- a 128)))
                                 (setq a (+ (* 256 a) (next-byte)))
                                 (if (zerop a)
                                   (+ (* 256 (+ (* 256 (+ (* 256 (next-byte))
                                                          (next-byte)))
                                                (next-byte)))
                                      (next-byte))
                                   a)))))
                    (label-PC (+ PC dist)))
               ;; search label-PC in label-alist:
               (do* ((L1 nil L2)
                     (L2 label-alist (cdr L2))) ; L1 = nil or L2 = (cdr L1)
                    ((cond
                       ((or (null L2) (> label-PC (caar L2))) ; insert
                        (setq L2 (cons (PC->label-a label-PC) L2))
                        (if L1 (setf (cdr L1) L2) (setq label-alist L2))
                        t)
                       ((= label-PC (caar L2)) t)
                       (t nil))
                     (cdar L2)))))
        (loop
          (when (null byte-list) (return))
          (setq instr-PC PC) ; PC at the start of the instruction
          (let ((instruction
                  (let ((instr-code (next-byte)))
                    (if (>= instr-code short-code-base)
                      (let* ((q (position instr-code short-code-ops
                                          :test #'>= :from-end t))
                             (r (- instr-code (svref short-code-ops q))))
                        (list (svref instruction-table-K q) r))
                      (let* ((table-entry (svref instruction-table instr-code))
                             (instr-name (first table-entry)))
                        (case (second table-entry)
                          (O (list instr-name))
                          ((K N) (list instr-name (num-operand)))
                          (B (list instr-name (next-byte)))
                          (L (list instr-name (label-operand)))
                          (NN (list instr-name (num-operand) (num-operand)))
                          (NB (list instr-name (num-operand) (next-byte)))
                          (BN (list instr-name (next-byte) (num-operand)))
                          (NNN (list instr-name (num-operand) (num-operand) (num-operand)))
                          (NBN (list instr-name (num-operand) (next-byte) (num-operand)))
                          (NNNN (list instr-name (num-operand) (num-operand) (num-operand) (num-operand)))
                          (NL (list instr-name (num-operand) (label-operand)))
                          (BL (list instr-name (next-byte) (label-operand)))
                          (NNL (list instr-name (num-operand) (num-operand) (label-operand)))
                          (NBL (list instr-name (num-operand) (next-byte) (label-operand)))
                          (NHL (let* ((n (num-operand))
                                      (ht (if (eq instr-name 'JMPHASH)
                                            (nth n const-list) ; JMPHASH
                                            (svref (first const-list) n))) ; JMPHASHV
                                      (labels '()))
                                 (maphash
                                   #'(lambda (obj dist)
                                       (declare (ignore obj))
                                       (push (label-operand dist) labels))
                                   ht)
                                 (list* instr-name n (label-operand) labels)))
                          (NC (let* ((n (num-operand))
                                     (v (car (nth n const-list)))
                                     (m (length v))
                                     (labels '()))
                                (do ((i 1 (+ i 2)))
                                    ((>= i m))
                                  (push (label-operand nil (svref v i))
                                        labels))
                                (list* instr-name n (nreverse labels))))
                          (NLX (let* ((n (num-operand))
                                      (m (length (nth n const-list)))
                                      (L '()))
                                 (dotimes (i m) (push (label-operand) L))
                                 (list* instr-name n (nreverse L))))))))))
            (push (cons instr-PC instruction) code-list)))))
    ;; (setq label-alist (sort label-alist #'> :key #'car))
    ;; reverse code-list and insert the Labels:
    (let ((new-code-list '()))
      (loop
        (when (and new-code-list label-alist
                   (= (caar new-code-list) (caar label-alist)))
          (push (car label-alist) new-code-list)
          (setq label-alist (cdr label-alist)))
        (when (null code-list) (return))
        ;; transfer an instruction from code-list to new-code-list:
        (psetq code-list (cdr code-list)
               new-code-list (rplacd code-list new-code-list)))
      new-code-list)))


#|
                           8th Step:
                    create functional object

The function make-closure is required.
|#
;; enters a byte-list as Code into fnode.
(defun create-fun-obj (fnode byte-list SPdepth)
  (setf (fnode-code fnode)
    (make-closure
      :name (fnode-name fnode)
      :codevec
        (macrolet ((as-word (anz)
                     (if *big-endian*
                       ;; BIG-ENDIAN-Processor
                       `(floor ,anz 256)
                       ;; LITTLE-ENDIAN-Processor
                       `(multiple-value-bind (q r) (floor ,anz 256)
                         (values r q)))))
          (multiple-value-call #'list*
            (as-word (car SPdepth))
            (as-word (cdr SPdepth))
            (as-word (fnode-req-anz fnode))
            (as-word (fnode-opt-anz fnode))
            (+ (if (fnode-rest-flag fnode) 1 0)
               (if (fnode-gf-p fnode) 16 0)
               (if (fnode-keyword-flag fnode)
                 (+ 128 (if (fnode-allow-other-keys-flag fnode) 64 0))
                 0))
            (values ; argument-type-shortcut
              (let ((req-anz (fnode-req-anz fnode))
                    (opt-anz (fnode-opt-anz fnode))
                    (rest (fnode-rest-flag fnode))
                    (key (fnode-keyword-flag fnode)))
                (cond ((and (not rest) (not key) (< (+ req-anz opt-anz) 6))
                       (+ (svref '#(1 7 12 16 19 21) opt-anz) req-anz))
                      ((and rest (not key) (zerop opt-anz) (< req-anz 5))
                       (+ 22 req-anz))
                      ((and (not rest) key (< (+ req-anz opt-anz) 5))
                       (+ (svref '#(27 32 36 39 41) opt-anz) req-anz))
                      (t 0))))
            (if (fnode-keyword-flag fnode)
              (multiple-value-call #'values
                (as-word (length (fnode-keywords fnode)))
                (as-word (fnode-Keyword-Offset fnode)))
              (values))
            byte-list))
      :consts
        (let ((l (append
                   (make-list (fnode-Keyword-Offset fnode))
                   (fnode-keywords fnode)
                   (if *fasoutput-stream* ; not called right away
                     (mapcar #'(lambda (value form)
                                 (if form (make-load-time-eval form) value))
                             (fnode-Consts fnode) (fnode-Consts-forms fnode))
                     (fnode-Consts fnode)))))
          (if (fnode-gf-p fnode)
            (list (coerce l 'simple-vector))
            l))
        :seclass (anode-seclass (fnode-code fnode))))
  fnode)

;; Return the signature of the byte-compiled function object
;; values:
;; 1. req-anz
;; 2. opt-anz
;; 3. rest-p
;; 4. key-p
;; 5. keyword-list
;; 6. allow-other-keys-p
;; additionally:
;; 7. byte-list
;; 8. const-list
(defun signature (closure)
  (let ((const-list (closure-consts closure))
        (byte-list (coerce (closure-codevec closure) 'list)))
    (macrolet ((pop2 (listvar)
                 (if *big-endian*
                   ; BIG-ENDIAN-Processor
                   `(+ (* 256 (pop ,listvar)) (pop ,listvar))
                   ; LITTLE-ENDIAN-Processor
                   `(+ (pop ,listvar) (* 256 (pop ,listvar))))))
      (pop byte-list) (pop byte-list)
      (pop byte-list) (pop byte-list)
      (let* ((req-anz (pop2 byte-list))
             (opt-anz (pop2 byte-list))
             (h (pop byte-list))
             (key-p (logbitp 7 h)))
        (pop byte-list)
        (values
          req-anz
          opt-anz
          (logbitp 0 h)
          key-p
          (when key-p
            (let ((kw-count (pop2 byte-list))
                  (kw-offset (pop2 byte-list)))
              (subseq (if (logbitp 4 h) ; generic function?
                        (coerce (first const-list) 'list)
                        const-list)
                      kw-offset (+ kw-offset kw-count))))
          (logbitp 6 h)
          byte-list
          const-list)))))


;;;;****                  THIRD    PASS

(defun pass3 ()
  (dolist (pair *fnode-fixup-table*)
    (let ((code (fnode-code (first pair))) (n (second pair)))
      (macrolet ((closure-const (code n) `(sys::%record-ref ,code (+ 2 ,n))))
        (setf (closure-const code n) (fnode-code (closure-const code n)))))))


;;;;****             TOP - LEVEL   CALL

;;; compile a lambdabody and return its code.
(defun compile-lambdabody (name lambdabody)
  (let ((fnode (c-lambdabody name lambdabody)))
    (assert (null (fnode-far-used-vars fnode)))
    (assert (null (fnode-far-assigned-vars fnode)))
    (assert (null (fnode-far-used-blocks fnode)))
    (assert (null (fnode-far-used-tagbodys fnode)))
    (unless *no-code*
      (let ((*fnode-fixup-table* '()))
        (pass2 fnode)
        (pass3))
      (when *compiling-from-file*
        (let ((kf (assoc name *known-functions* :test #'equal)))
          (when kf ; save seclass for other functions in the file
            (setf (fourth kf) (function-side-effect (fnode-code fnode))))))
      (fnode-code fnode))))

;; COMPILE-LAMBDA & COMPILE-FORM are "top-level", i.e., they bind all
;; compiler variables; they also cannot bind *COMPILING-FROM-FILE* to T
;; because they are called on (declare (compile))
;; this is why we need COMPILE-LAMBDA-HELPER: it will be called by
;; COMPILE-FORM-IN-TOPLEVEL-ENVIRONMENT, which has to bind
;; *COMPILING-FROM-FILE* to T so that the LOAD :COMPILING T would not complain
;; about functiond defined in one form which are calling each other

;; does everything compile-lambda does
;; except that it does not bind *compiling-from-file*
(proclaim '(inline compile-lambda-helper))
(defun compile-lambda-helper
    (name lambdabody %venv% %fenv% %benv% %genv% %denv% error-when-failed-p)
  (let ((*compiling* t)
        (*c-listing-output* nil)
        (*c-error-output* *error-output*)
        (*known-special-vars* '())
        (*constant-special-vars* '())
        (*func* nil)
        (*fenv* %fenv%)
        (*benv* %benv%)
        (*genv* %genv%)
        (*venv* %venv%)
        (*venvc* nil)
        (*denv* %denv%)
        (*error-count* 0) (*warning-count* 0) (*style-warning-count* 0)
        (*no-code* nil))
    (let ((funobj (compile-lambdabody name lambdabody)))
      (when (and error-when-failed-p (not (zerop *error-count*)))
        (let ((form (cons name lambdabody)))
          (error-of-type 'source-program-error
            :form form
            :detail form
            (TEXT "~S cannot be compiled") form)))
      funobj)))

;; is called for (lambda (...) (declare (compile)) ...) and returns a
;; functional object equivalent to this lambda-expression.
(defun compile-lambda (name lambdabody %venv% %fenv% %benv% %genv% %denv%
                       error-when-failed-p)
  (let ((*compiling-from-file* nil))
    (compile-lambda-helper name lambdabody %venv% %fenv% %benv% %genv% %denv%
                           error-when-failed-p)))

;; is called for (let/let*/multiple-value-bind ... (declare (compile)) ...)
;; and returns a functional object, that - called with 0 arguments -
;; executes this form.
(let ((form-count 0))
  (defun compile-form (form %venv% %fenv% %benv% %genv% %denv%)
    (compile-lambda (symbol-suffix '#:COMPILED-FORM (incf form-count))
                    `(() ,form)
                    %venv% %fenv% %benv% %genv% %denv% nil))
  ;; compiles a form in the Toplevel-Environment - only for LOAD :COMPILING T
  (defun compile-form-in-toplevel-environment
      (form &aux (env *toplevel-environment*))
    (let ((*compiling-from-file* t))
      (compile-lambda-helper (symbol-suffix '#:COMPILED-FORM (incf form-count))
                             `(() ,form)
                             (svref env 0)    ; %venv%
                             (svref env 1)    ; %fenv%
                             (svref env 2)    ; %benv%
                             (svref env 3)    ; %genv%
                             (svref env 4)    ; %denv%
                             nil))))

;; Evaluates a form in an environment
(defun eval-env (form &optional (env *toplevel-environment*))
  (evalhook form nil nil env))

;; Common-Lisp-Function COMPILE
(defun compile (name &optional (definition nil svar)
                     &aux (macro-flag nil) (trace-flag nil) (save-flag nil)
                #+clisp-debug (*form* definition))
  (setq name (check-function-name name 'compile))
  (let ((symbol (get-funname-symbol name)))
    (if svar
      ;; Re-Definition of name as function.
      (progn
        ;; if name is traced -> if previously a macro, first untrace.
        (when (and name (setq svar (get symbol 'sys::traced-definition)))
          (if (consp svar)
            (progn
              (warn (TEXT "~S: redefining ~S; it was traced!")
                    'compile name)
              (sys::untrace2 name))
            (setq trace-flag t)))
        (when (sys::%compiled-function-p definition)
          (warn #1=(TEXT "~S is already compiled.") definition)
          (when name
            (if trace-flag
              (setf (get symbol 'sys::traced-definition) definition)
              (setf (symbol-function symbol) definition)))
          (return-from compile (values (or name definition) nil nil)))
        (when name
          (setq save-flag
                (cons `(SETF (FDEFINITION ',name) ',definition)
                      sys::*toplevel-environment*))))
      ;; Compilation of the available Function-/Macro-Definition.
      (progn
        (unless (fboundp symbol)
          (error-of-type 'undefined-function
            :name name
            (TEXT "Undefined function ~S")
            name))
        (if (setq definition (get symbol 'sys::traced-definition))
          (setq trace-flag t)
          (setq definition (symbol-function symbol)))
        (when (macrop definition)
          (setq macro-flag t)
          (setq definition (macro-expander definition)))
        (when (sys::%compiled-function-p definition)
          (warn #1# name)
          (return-from compile (values (or name definition) nil nil)))))
    (unless (or (and (consp definition) (eq (car definition) 'lambda))
                (sys::closurep definition))
      (error-of-type 'error
        (TEXT "Not a lambda expression nor a function: ~S")
        definition))
    (flet ((closure-slot (obj num)
             (if (sys::closurep obj)
               (sys::%record-ref obj num)
               nil)))
      (let ((*compiling* t)
            (*error-count* 0)
            (*warning-count* 0)
            (*style-warning-count* 0)
            (*compiling-from-file* nil)
            (*c-listing-output* nil)
            (*fasoutput-stream* nil) ; compiled code may be called right away
            (*c-error-output* *error-output*)
            (*known-special-vars* '())
            (*constant-special-vars* '())
            (*func* nil)
            (*fenv* (closure-slot definition 5))
            (*benv* (closure-slot definition 6))
            (*genv* (closure-slot definition 7))
            (*venv* (closure-slot definition 4))
            (*venvc* nil)
            (*denv* (or (closure-slot definition 8)
                        *toplevel-denv*))
            (*no-code* nil))
        (let ((lambdabody (or (closure-slot definition 1)
                              (cdr definition))))
          (let ((funobj (compile-lambdabody name lambdabody)))
            (values
              (if (zerop *error-count*)
                (if name
                  (progn
                    (when macro-flag (setq funobj (make-macro funobj)))
                    (if trace-flag
                      (setf (get symbol 'sys::traced-definition) funobj)
                      (setf (symbol-function symbol) funobj))
                    (when save-flag
                      (setf (get symbol 'sys::definition) save-flag))
                    name)
                  funobj)
                (compile-lambdabody '#:***COMPILED-WITH-ERRORS***
                  `((&REST ARGUMENTS) (DECLARE (IGNORE ARGUMENTS))
                    (COMPILED-WITH-ERRORS ',lambdabody))))
              (compile-warnings-p)
              (compile-failure-p))))))))

;; Signals an error.
(defun compiled-with-errors (lambdabody)
  (error-of-type 'source-program-error
    :form lambdabody
    :detail lambdabody
    (TEXT "A function compiled with errors cannot be executed.")))

;; Top-Level-Forms must be written solitary to the .fas-File,
;; because of the semantics of EVAL-WHEN and LOAD-TIME-VALUE.
;; As Top-Level-Forms can be split at EVAL-WHEN, PROGN and LOCALLY,
;; one has to use LET (), in order to circumvent this.

;; Compiles a Top-Level-Form for COMPILE-FILE. The *toplevel-name* is
;; mostly passed unchanged. *toplevel-for-value* indicates, if the value
;; is needed (for LOAD :PRINT T) or not.
(defvar *toplevel-name*)
(defvar *toplevel-for-value*)
(defun compile-toplevel-form (form &optional (*toplevel-name* *toplevel-name*))
  (unless form (return-from compile-toplevel-form))
  (catch 'c-error
    ;; CLtL2 p. 90: "Processing of top-level forms in the file compiler ..."
    ;; 1. step: macro expansion
    (if (atom form)
      (when (symbolp form)
        (multiple-value-bind (macrop expansion) (venv-search-macro form *venv*)
          (when macrop ; Symbol-Macro ?
            (return-from compile-toplevel-form
              (compile-toplevel-form expansion))))) ; -> expand
      (let ((fun (first form)))
        (when (symbolp fun)
          (multiple-value-bind (a m f1 f2 f3 f4) (fenv-search fun)
            (declare (ignore f2 f3 f4))
            (if (null a)
              ;; not defined locally
              (case fun
                (PROGN ; cf. c-PROGN
                 (test-list form 1)
                 (let ((L (cdr form))) ; List of Forms
                   (cond ((null L) (compile-toplevel-form 'NIL)) ; no Form
                         ((null (cdr L)) (compile-toplevel-form (car L))) ; exactly one Form
                         (t (let ((subform-count 0))
                              (do ((Lr L))
                                  ((null Lr))
                                (let* ((subform (pop Lr))
                                       (*toplevel-for-value*
                                         (and *toplevel-for-value* (null Lr))))
                                  (compile-toplevel-form
                                   subform
                                   (symbol-suffix *toplevel-name*
                                                  (incf subform-count)))))))))
                 (return-from compile-toplevel-form))
                ((LOCALLY EVAL-WHEN COMPILER-LET MACROLET SYMBOL-MACROLET
                  WITHOUT-PACKAGE-LOCK)
                 (let ((*form* form))
                   ;; call c-LOCALLY resp. c-EVAL-WHEN resp. c-COMPILER-LET
                   ;; resp. c-MACROLET resp. c-SYMBOL-MACROLET:
                   (funcall (gethash fun c-form-table)
                            #'compile-toplevel-form))
                 (return-from compile-toplevel-form))
                (t (when (macro-function fun) ; global Macro ?
                     (return-from compile-toplevel-form
                       (compile-toplevel-form
                        (mac-exp (macro-function fun) form))))))
              ;; defined locally
              (when (and m (null f1)) ; local Macro, but no local function
                (return-from compile-toplevel-form
                  (compile-toplevel-form (mac-exp m form)))))))))
    ;; 2. step: compile and write
    (when (and (not *toplevel-for-value*) (l-constantp form))
      (return-from compile-toplevel-form))
    (let ((*package-tasks* '()))
      (setq form
        (compile-lambdabody *toplevel-name*
          `(() ,form ,@(if *toplevel-for-value* '() '((VALUES)) ) )))
      (when *c-listing-output*
        (disassemble-closures form *c-listing-output*))
      (when *fasoutput-stream*
        (write form :stream *fasoutput-stream* :pretty t
                  ; :closure t :circle t :array t :gensym t
                  ; :escape t :level nil :length nil :radix t
                    :readably t :right-margin 79)
        (terpri *fasoutput-stream*))
      (when (and *package-tasks-treat-specially* *package-tasks*)
        (c-eval-and-write-lib `(PROGN ,@(nreverse *package-tasks*)))))))

;; open C-Output-File, if not open yet:
(defun prepare-coutput-file ()
  (when (and *compiling-from-file* *coutput-file*)
    (unless *coutput-stream*
      (setq *coutput-stream* (open *coutput-file* :direction :output))
      (format *coutput-stream* "#include \"clisp.h\"~%~%"))
    t))
;; Hook for FFI:
(predefun finalize-coutput-file ())

(defun c-reset-globals ()
  ;; The global variables have to be assigned, not bound!
  (setq *functions-with-errors*  nil
        *known-special-vars*     nil
        *unknown-free-vars*      nil
        *constant-special-vars*  nil
        *known-functions*        nil
        *unknown-functions*      nil
        *deprecated-functions*   nil
        *inline-functions*       nil
        *notinline-functions*    nil
        *inline-definitions*     nil
        *inline-constants*       nil
        *notinline-constants*    nil
        *user-declaration-types* nil
        *compiled-modules*       nil))

;;; compare an element of `*unknown-functions*' with
;;; an element of `*known-functions*'
;;; returns T if the functions match, NIL otherwise
;;; this function should be suitable as a :test argument
;;; for `set-difference'
(defun match-known-unknown-functions (uf kf)
  ;; uf: (function c-source-point arglist . apply-arglist)
  ;; kf: (function c-source-point signature seclass)
  (when (equal (car uf) (car kf))
    (let ((*compile-file-lineno1* (c-source-point-lineno1 (second uf)))
          (*compile-file-lineno2* (c-source-point-lineno2 (second uf)))
          (*compile-file-truename* (c-source-point-file (second uf)))
          (known-sig (third kf)))
      (unless (or (null (cddr uf)) ; nothing to test
                  (test-argument-syntax (caddr uf) (cdddr uf) (car uf)
                                        (sig-req-num  known-sig)
                                        (sig-opt-num  known-sig)
                                        (sig-rest-p   known-sig)
                                        (sig-keys-p   known-sig)
                                        (sig-keywords known-sig)
                                        (sig-allow-p  known-sig))
                  (null (c-source-point-file (second kf))))
        (c-comment (TEXT "[~s was defined~a]")
                   (car kf) (c-source-point-location (second kf))))
      t)))

;; report the compilation problems accumulated so far and reset them
(defun c-report-problems ()
  (when *functions-with-errors*
    (c-comment (TEXT "There were errors in the following functions:~%~{~<~%~:; ~S~>~^~}")
               (nreverse *functions-with-errors*)))
  (setq *unknown-functions*
        (nset-difference *unknown-functions* *known-functions*
                         :test #'match-known-unknown-functions))
  (when *unknown-functions*
    (c-comment (TEXT "The following functions were used but not defined:~%~{~<~%~:; ~S~>~^~}")
               (delete-duplicates (mapcar #'car (nreverse *unknown-functions*))
                                  :test #'equal)))
  (let ((unknown-vars (set-difference *unknown-free-vars*
                                      *known-special-vars*))
        (too-late-vars (intersection *unknown-free-vars*
                                     *known-special-vars*)))
    (when unknown-vars
      (c-comment (TEXT "The following special variables were not defined:~%~{~<~%~:; ~S~>~^~}")
                 (nreverse unknown-vars)))
    (when too-late-vars
      (c-comment (TEXT "The following special variables were defined too late:~%~{~<~%~:; ~S~>~^~}")
                 (nreverse too-late-vars))))
  (when *deprecated-functions*
    (c-comment (TEXT "The following functions were used but are deprecated:~%~:{~<~%~:; ~S - ~@?~>~^~}")
               (mapcar #'(lambda (funname)
                           (assoc funname *deprecated-functions-alist* :test #'eq))
                       (nreverse *deprecated-functions*))))
  (when (boundp '*error-count*) ; then `*warning-count*' is bound too
    (c-comment (TEXT "~D error~:P, ~D warning~:P")
               *error-count* *warning-count*))
  ;; clean-up for the next compilation unit
  (c-reset-globals))

;; non-NIL means that the current compilation is the top call,
;; i.e., it will report the errors/warnings &c.
;; this is T inside the outer-most `with-compilation-unit'/`compile-file'
;; or when `with-compilation-unit' is given non-NIL `:override' argument
(defvar *c-top-call*)

(defmacro with-compilation-unit ((&key override) &body forms)
  `(let ((*c-top-call* (or ,override (not (boundp '*c-top-call*))))
         #+ffi (ffi::*foreign-language* ffi::*foreign-language*)
         (*c-listing-output* nil)
         (*c-error-output* *error-output*))
     ;; clean up from the outer `with-compilation-unit':
     ;; <http://www.lisp.org/HyperSpec/Body/mac_with-compilation-unit.html>
     ;; [CLHS]: If nested dynamically only the outer call to
     ;; `with-compilation-unit' has any effect unless the value associated
     ;; with `:override' is true, in which case warnings are deferred only
     ;; to the end of the innermost call for which override is true.
     (when *c-top-call*
       (c-report-problems))
     (progv (when *c-top-call*
              '(*error-count* *warning-count* *style-warning-count*))
         (when *c-top-call* '(0 0 0))
       (unwind-protect
            (progn ,@forms)
         ;; report the errors and reset
         (when *c-top-call*
           (c-report-problems))))))

;; return a path with the specified type and the rest as in the path argument
(defun merge-extension (type path) (make-pathname :type type :defaults path))

;; Common part of COMPILE-FILE and COMPILE-FILE-PATHNAME.
;; Returns two values:
;; 1. the output file (pathname or stream or NIL),
;; 2. the input file pathname.
(defun compile-file-pathname-helper (file output-file)
  (let ((input-file
          (or (and (not (logical-pathname-p (pathname file)))
                   (first (search-file file *source-file-types*)))
              (merge-pathnames file #.(make-pathname :type "lisp")))))
    (values
      (if (or (null output-file)
              (and (streamp output-file)
                   (open-stream-p output-file)
                   (output-stream-p output-file)))
        output-file
        (let ((tmp (merge-extension "fas" input-file)))
          (if (eq output-file 'T)
            tmp
            ;; Not (merge-pathnames output-file tmp) because that doesn't
            ;; do the right thing when output-file is a relative pathname
            ;; and either *merge-pathnames-ansi* is true or input-file is
            ;; absolute.
            (let ((output-file (merge-pathnames output-file)))
              (make-pathname
               :host (or (pathname-host output-file)
                         (pathname-host tmp))
               :device (if (pathname-host output-file)
                         (pathname-device output-file)
                         (if (pathname-host tmp)
                           (pathname-device tmp)
                           (or (pathname-device output-file)
                               (pathname-device tmp))))
               :directory (if (pathname-host output-file)
                            (pathname-directory output-file)
                            (if (pathname-host tmp)
                              (pathname-directory tmp)
                              (if (pathname-device output-file)
                                (pathname-directory output-file)
                                (if (pathname-device tmp)
                                  (pathname-directory tmp)
                                  (pathname-directory output-file)))))
               :name (or (pathname-name output-file)
                         (pathname-name tmp))
               :type (or (pathname-type output-file)
                         (pathname-type tmp))
               :version (or (pathname-version output-file)
                            (pathname-version tmp)))))))
      input-file)))

;; Common-Lisp-Function COMPILE-FILE
;; file          should be a Pathname/String/Symbol.
;; :output-file  should be nil or t or a Pathname/String/Symbol or
;;               an Output-Stream. Default: t.
;; :listing      should be nil or t or a Pathname/String/Symbol or
;;               an Output-Stream. Default: nil.
;; :warnings     indicates, if the Warnings should also appear on the screen.
;; :verbose      indicates, if the Errors also have to appear on the screen.
(defun compile-file (file &key (output-file 'T) listing
                               ((:warnings *compile-warnings*)
                                *compile-warnings*)
                               ((:verbose *compile-verbose*) *compile-verbose*)
                               ((:print *compile-print*) *compile-print*)
                               (external-format :default)
                          &aux liboutput-file (*coutput-file* nil) input-file
                               (*compile-file-directory*
                                 (if (eq t output-file)
                                   nil
                                   (make-pathname :name nil :type nil
                                                  :defaults output-file)))
                               (new-output-stream nil)
                               (new-listing-stream nil))
  (multiple-value-setq (output-file input-file)
    (compile-file-pathname-helper file output-file))
  (when (and output-file (not (streamp output-file)))
    (setq liboutput-file (merge-extension "lib" output-file))
    (setq *coutput-file* (merge-extension "c" output-file))
    (setq new-output-stream t))
  (when (and listing (not (streamp listing)))
    (setq listing (if (eq listing 'T)
                    (merge-extension "lis" output-file)
                    (merge-pathnames listing)))
    (setq new-listing-stream t))
  (with-open-file (istream input-file :direction :input-immutable
                                      :external-format external-format)
    (let ((listing-stream ; a stream or NIL
            (if new-listing-stream
              (open listing :direction :output)
              (if (streamp listing) listing nil))))
      (unwind-protect
        (let* ((*compile-file-pathname* (merge-pathnames file)) ; as per ANSI
               (*compile-file-resolved-pathname* input-file) ; more useful
               (*compile-file-truename* (truename input-file))
               (*current-source-file* *compile-file-truename*)
               (*compile-file-lineno1* nil)
               (*compile-file-lineno2* nil)
               (*fasoutput-stream* ; a Stream or NIL
                 (if new-output-stream
                   (open output-file :direction :output)
                   (if (streamp output-file) output-file nil)))
               (*liboutput-stream* ; a Stream or NIL
                 (if new-output-stream
                   (open liboutput-file :direction :output)
                   nil))
               (*coutput-stream* nil) ; a Stream or NIL at the moment
               (*ffi-module* nil) ; NIL at the moment
               (*load-forms* (make-hash-table :key-type 't :value-type 't
                                              :test 'eq))
               (compilation-successful nil))
          (when *fasoutput-stream* (sys::allow-read-eval *fasoutput-stream* t))
          (when *liboutput-stream* (sys::allow-read-eval *liboutput-stream* t))
          (unwind-protect
            (with-compilation-unit ()
              (when listing-stream
                (fresh-line listing-stream)
                (format listing-stream
                  (TEXT "Listing of compilation of file ~A~%on ~@? by ~A, version ~A")
                  input-file
                  (date-format)
                  (multiple-value-list (get-decoded-time))
                  ;; List (sec min hour day month year ...)
                  (lisp-implementation-type) (lisp-implementation-version)))
              (let ((*compiling* t)
                    (*compiling-from-file* t)
                    (*package* *package*)
                    (*readtable* *readtable*)
                    (*c-listing-output* listing-stream)
                    (*c-error-output*
                      (if listing-stream
                        (make-broadcast-stream *error-output* listing-stream)
                        *error-output*))
                    (verbose-out
                      (when *compile-verbose*
                        (if listing-stream
                          (make-broadcast-stream *standard-output*
                                                 listing-stream)
                          *standard-output*)))
                    (*func* nil)
                    (*fenv* nil)
                    (*benv* nil)
                    (*genv* nil)
                    (*venv* nil)
                    (*venvc* nil)
                    (*denv* *toplevel-denv*)
                    (*no-code* (and (null *fasoutput-stream*)
                                    (null listing-stream)))
                    (*toplevel-for-value* t)
                    (eof-value istream)
                    (form-count 0))
                (when verbose-out
                  (fresh-line verbose-out)
                  (format verbose-out (TEXT ";; Compiling file ~A ...")
                          input-file)
                  (elastic-newline verbose-out))
                (when *fasoutput-stream*
                  (let ((*package* *keyword-package*))
                    (write `(SYSTEM::VERSION ',(version))
                           :stream *fasoutput-stream*
                         ; :escape t :level nil :length nil :radix t
                           :readably t :right-margin 79 :case ':upcase))
                  (terpri *fasoutput-stream*))
                #+UNICODE
                (flet ((set-utf-8 (stream)
                         ;; Set the stream's encoding to UTF-8,
                         ;; if it supports it.
                         (block try
                           (let ((*error-handler*
                                   #'(lambda (&rest error-args)
                                       (declare (ignore error-args))
                                       (return-from try nil)))
                                 (encoding 'charset:utf-8))
                             (setf (stream-external-format stream) encoding)
                             (write-string "#0Y " stream)
                             (let ((*package* #,(find-package "CHARSET")))
                               (write encoding :stream stream :readably t))
                             (terpri stream)))))
                  (when new-output-stream
                    (when *fasoutput-stream*
                      (set-utf-8 *fasoutput-stream*))
                    (when *liboutput-stream*
                      (set-utf-8 *liboutput-stream*))))
                (loop
                  (peek-char t istream nil eof-value)
                  (setq *compile-file-lineno1* (line-number istream))
                  (let* ((form (read istream nil eof-value))
                         (form-name (write-to-string
                                     form :level 2 :length 3 :pretty nil)))
                    (setq *compile-file-lineno2* (line-number istream))
                    (when (eql form eof-value) (return))
                    (when *compile-print* (format t "~&; ~A~." form-name))
                    (compile-toplevel-form
                     form (symbol-suffix
                           (make-symbol
                            (string-concat
                             (prin1-to-string *compile-file-lineno1*) #3=" "
                             (prin1-to-string *compile-file-lineno2*) #3#
                             form-name))
                           (incf form-count)))))
                (finalize-coutput-file)
                (setq compilation-successful (zerop *error-count*))
                (when verbose-out
                  (cond (compilation-successful
                         (fresh-line verbose-out)
                         (format verbose-out #1=(TEXT ";; Wrote file ~A")
                                 output-file)
                         (when *coutput-stream*
                           (terpri verbose-out)
                           (format verbose-out #1# *coutput-file*))
                         (elastic-newline verbose-out))
                        (new-output-stream
                         (fresh-line verbose-out)
                         (format verbose-out #2=(TEXT ";; Deleted file ~A")
                                 output-file)
                         (when *coutput-stream*
                           (terpri verbose-out)
                           (format verbose-out #2# *coutput-file*))
                         (elastic-newline verbose-out)))
                  (when new-listing-stream
                    (terpri verbose-out)
                    (format verbose-out #1# listing)
                    (elastic-newline verbose-out)))
                (values (and compilation-successful output-file
                             (truename *fasoutput-stream*))
                        (compile-warnings-p)
                        (compile-failure-p))))
            (when new-output-stream
              (close *fasoutput-stream*)
              (close *liboutput-stream*)
              (when *coutput-stream*
                (close *coutput-stream*))
              (unless compilation-successful
                (delete-file output-file) (delete-file liboutput-file)))))
        (when new-listing-stream
          (fresh-line listing-stream)
          (close listing-stream))))))

;; This must be consistent with compile-file (see above)!
(defun compile-file-pathname (file &key (output-file 'T) &allow-other-keys)
  (values (compile-file-pathname-helper file output-file)))

(defun disassemble-closures (closure stream)
  (let ((closures '()))
    (labels ((mark (cl) ; enters a Closure cl (recursive) in closures.
               (push cl closures) ; mark cl
               (dolist (c (closure-consts cl)) ; and all Sub-Closures
                 (when (and (sys::closurep c) (sys::%compiled-function-p c))
                   (unless (member c closures) (mark c)))))) ; mark likewise
      (mark closure)) ; mark Main-Closure
    (dolist (c (nreverse closures)) ; disassemble all Closures
      (disassemble-closure c stream))))

(defun stream-tab (stream tab)
  (sys::write-spaces (let ((pos (sys::line-position stream)))
                       (if pos (max 1 (- tab pos)) 2))
                     stream))
(defun disassemble-closure (closure &optional (stream *standard-output*))
  (fresh-line stream)
  (terpri stream)
  (format stream (TEXT "Disassembly of function ~S") (closure-name closure))
  (multiple-value-bind (req-anz opt-anz rest-p
                        key-p keyword-list allow-other-keys-p
                        byte-list const-list)
      (signature closure)
    (do ((L const-list (cdr L))
         (i 0 (1+ i)))
        ((null L))
      (format stream "~%(CONST ~S) = ~S" i (car L)))
    (terpri stream)
    (format stream (TEXT "~S required argument~:P") req-anz)
    (terpri stream)
    (format stream (TEXT "~S optional argument~:P") opt-anz)
    (terpri stream)
    (if rest-p
      (write-string (TEXT "Rest parameter") stream)
      (write-string (TEXT "No rest parameter") stream))
    (if key-p
      (let ((kw-count (length keyword-list)))
        (terpri stream)
        (format stream (TEXT "~S keyword parameter~:P: ") kw-count)
        (do ((L keyword-list))
            ((endp L))
          (prin1 (pop L) stream)
          (if (endp L) (write-string "." stream) (write-string ", " stream)))
        (when allow-other-keys-p
          (terpri stream)
          (write-string (TEXT "Other keywords are allowed.") stream)))
      (progn
        (terpri stream)
        (write-string (TEXT "No keyword parameters") stream)))
    (let ((const-string-list
            (mapcar #'(lambda (x) (sys::write-to-short-string x 35))
                    const-list))
          (special-vars-read '())
          (special-vars-write '())
          (lap-list (disassemble-LAP byte-list const-list)))
      (dolist (L lap-list)
        (when (consp (cdr L))
          (case (cadr L)        ; instruction
            ((GETVALUE GETVALUE&PUSH)
             (pushnew (nth (caddr L) const-list) special-vars-read))
            ((SETVALUE)
             (pushnew (nth (caddr L) const-list) special-vars-write)))))
      (when special-vars-read
        (terpri stream)
        (format stream (TEXT "reads special variable~P: ~{~S~^ ~}")
                (length special-vars-read) special-vars-read))
      (when special-vars-write
        (terpri stream)
        (format stream (TEXT "writes special variable~P: ~{~S~^ ~}")
                (length special-vars-write) special-vars-write))
      (terpri stream)
      (format stream (TEXT "~S byte-code instruction~:P:") (length lap-list))
      (dolist (L lap-list)
        (let ((PC (car L)) (instr (cdr L)))
          (terpri stream)
          (prin1 PC stream)
          (stream-tab stream 6)
          (princ instr stream) ; write instr, Symbols without Package-Marker!
          (multiple-value-bind (commentp comment)
            (when (consp instr)
              (case (first instr)
                ((CALLS1 CALLS1&PUSH CALLS1&STORE CALLS1&JMPIFNOT CALLS1&JMPIF)
                 (values t (%funtabref (second instr))))
                ((CALLS2 CALLS2&PUSH CALLS2&STORE CALLS2&JMPIFNOT CALLS2&JMPIF)
                 (values t (%funtabref (+ 256 (second instr)))))
                ((CALLSR CALLSR&PUSH CALLSR&STORE CALLSR&JMPIFNOT CALLSR&JMPIF)
                 (values t (%funtabref (+ funtabR-index (third instr)))))
                ((CALL CALL&PUSH)
                 (values 'string (nth (third instr) const-string-list)))
                ((CALL0 CALL1 CALL1&PUSH CALL1&JMPIFNOT CALL1&JMPIF
                  CALL2 CALL2&PUSH CALL2&JMPIFNOT CALL2&JMPIF
                  JMPIFEQTO JMPIFNOTEQTO CONST CONST&PUSH SETVALUE GETVALUE
                  GETVALUE&PUSH BIND CONST&STORE CONST&SYMBOL-FUNCTION&PUSH
                  CONST&SYMBOL-FUNCTION COPY-CLOSURE&PUSH COPY-CLOSURE
                  CONST&SYMBOL-FUNCTION&STORE TAGBODY-OPEN HANDLER-OPEN)
                 (values 'string (nth (second instr) const-string-list)))))
            (when commentp
              (stream-tab stream 42)
              (write-string "; " stream)
              (if (eq commentp 'string)
                (write-string comment stream)
                (prin1 comment stream)))))))
    (elastic-newline stream)))

;; Creates a trampoline bytecode vector for jumping to a given function.
;; Used by set-funcallable-instance-function.
(defun make-trampoline (function)
  (multiple-value-bind (name req-num opt-num rest-p key-p)
      (function-signature function)
    (declare (ignore name))
    ; Don't use optional parameters, since the RETGF instruction doesn't
    ; support them.
    (when (> opt-num 0)
      (setq opt-num 0 rest-p t))
    (let ((fnode
            (make-fnode :name 'trampoline
                        :req-anz req-num
                        :opt-anz opt-num
                        :rest-flag (or rest-p key-p)
                        :code (let ((*form* nil) (*stackz* nil))
                                (make-anode :seclass *seclass-dirty*))
                        :Blocks-Offset 0
                        :Tagbodys-Offset 0
                        :Keyword-Offset 0
                        :Consts-Offset 0)))
      (create-fun-obj fnode
                      (assemble-LAP
                        (list (list 'VENV)
                              (list 'SKIP&RETGF (+ 1 req-num (if (or rest-p key-p) 1 0)))))
                      '(0 . 0))
      (let ((trampoline (fnode-code fnode)))
        (sys::%record-ref trampoline 1)))))

;; The compilation of code using symbol-macros requires venv-search in
;; compiled form.
(unless (compiled-function-p #'venv-search)
  (compile 'venv-search))

(eval-when (compile load eval)
  (setq clos::*compile-accessor-functions* t))
