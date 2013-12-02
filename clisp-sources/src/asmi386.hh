// Assembly language support for i386 CPU.
// Bruno Haible 21.6.1997

// An assembly language file for the i386/i486/i586/i686/786 CPUs:
// On Unix, it is preprocessed and then assembled. NB: This file requires
// an ANSI C or C++ preprocessor which understands C++ comments.
// On Windows, with MSVC, it is preprocessed and then compiled with
// optimization. (The MSVC development environment does not have a separate
// assembler, we must use the C compiler's inline asm extension. Compiling
// without optimization pushes the registers %ebx,%esi,%edi onto the stack
// at function entry and pops them at function exit, which is not what we
// want because it affects the %esp offsets of the function arguments.)

// The assembly language file should
// 1. include a configuration file which defines ASM_UNDERSCORE if appropriate.
//    #ifndef _MSC_VER
//    #include "config.h"
//    #endif
// 2. include this file.
//    #include "asmi386.h"
// 3. define all assembly language code.

// The three different assembler syntaxes for this CPU are a MAJOR annoyance.
// In order not to have to maintain several copies of the assembly language
// code, we use lots of macros which expand into the correct syntax.
// These macros are:
//   C(name)
//           This expands to the name of the C variable or function `name'.
//           On Unix BSD systems, this prepends an underscore.
//   L(label)
//           This expands to the name of a local label, having the name `label'.
//           On Unix ELF systems, where there is no underscore, names beginning
//           with an alphabetic character are automatically exported, so this
//           prepends a dot. Note that when defining a label, the `:' must
//           be inside the parentheses, not outside, because otherwise some
//           ANSI C preprocessor inserts a space between the label and the `:',
//           and some assemblers don't like this.
//   R(reg)
//           This expands to a reference to register `reg'. On Unix, this
//           prepends a % charater.
//   NUM(value)
//           This expands to an immediate value. On Unix, this prepends a $
//           character.
//   ADDR(variable)
//           This expands to an immediate value, the address of some variable
//           or function. On Unix, this prepends a $ character. With MSVC,
//           this prepends the keyword "OFFSET".
//   About operand sizes: On Unix, a suffix to the instruction specifies the
//           size of the operands (for example "movb", "movw", "movl"). With
//           MSVC, there is no such suffix. Instead, the assembler infers the
//           operand size from the names of the registers ("al" vs. "ax" vs.
//           "eax"). This works well in most cases, but in instructions like
//           "mul [esi]" the assembler guesses the operand size: "byte" by
//           default. So it is better to explicitly specify the operand size
//           of memory operands (prefix X1, X2, X4, X8).
//           (Side note about Unix assemblers: Some Unix assemblers allow you
//           to write "testb %eax,%eax" but silently treat this as
//           "testb %al,%al".)
//   X1
//           This prefixes a memory reference of 1 byte.
//   X2
//           This prefixes a memory reference of 2 bytes.
//   X4
//           This prefixes a memory reference of 4 bytes.
//   X8
//           This prefixes a memory reference of 8 bytes.
//   MEM(base)
//           This expands to a memory reference at address `base'.
//   MEM_DISP(base,displacement)
//           This expands to a memory reference at address `base+displacement'.
//   MEM_INDEX(base,index)
//           This expands to a memory reference at address `base+index'.
//   MEM_SHINDEX(base,index,size)
//           This expands to a memory reference at address
//           `base+index*size', where `size' is 1, 2, 4, or 8.
//   MEM_DISP_SHINDEX0(displacement,index,size)
//           This expands to a memory reference at address
//           `displacement+index*size', where `size' is 1, 2, 4, or 8.
//   MEM_DISP_SHINDEX(base,displacement,index,size)
//           This expands to a memory reference at address
//           `base+displacement+index*size', where `size' is 1, 2, 4, or 8.
//   INDIR(value)
//           This expands to an implicit indirection. On Unix, this prepends
//           a * character.
//   INSN1(mnemonic,size_suffix,dst)
//           This expands to an instruction with one operand.
//   INSN2(mnemonic,size_suffix,src,dst)
//           This expands to an instruction with two operands. In our notation,
//           `src' comes first and `dst' second, but they are reversed when
//           expanding to Intel syntax.
//   INSN2MOVX(mnemonic,size_suffix,src,dst)
//           This expands to an instruction with two operands, of type
//           movsbl/movzbl, which in some syntaxes requires a second suffix.
//   INSN2SHCL(mnemonic,size_suffix,src,dst)
//           This expands to an instruction with two operands, of type
//           shrd/shld, which in some syntaxes requires an additional operand
//           %cl.
//   REP, REPZ
//           This expands to a prefix for string instructions.
//   _
//           For instructions which don't have a size suffix, like jump
//           instructions. Expands to nothing. Needed for MSVC, which has
//           problems with empty macro arguments.
//   TEXT()
//           Switch to the code section.
//   ALIGN(log)
//           Align to 2^log bytes.
//   GLOBL(name)
//           Declare `name' to be a global symbol.
//   DECLARE_FUNCTION(name)
//           Declare `name' to be a global function. When assembly language
//           code is compiled into a shared library, ELF linkers need to know
//           which symbols are functions.
//   FUNBEGIN(name)
//           Start the assembly language code for the C function `name'.
//   FUNEND()
//           End the assembly language code for a function.

// Define the C(name) and L(label) macros.
#ifdef _MSC_VER
#define C(entrypoint) entrypoint
#define L(label) L##label
#else
#ifdef ASM_UNDERSCORE
#define C(entrypoint) _##entrypoint
#define L(label) L##label
#else
#define C(entrypoint) entrypoint
#define L(label) .L##label
#endif
#endif

// Define one of these.
// BSD_SYNTAX for GNU assembler version 2.
// ELF_SYNTAX for SVR4 and Solaris assemblers.
// INTEL_SYNTAX for MS assembler.
#ifdef _MSC_VER
#define INTEL_SYNTAX
#else
// On Unix, it happens that the ELF systems (ASM_UNDERSCORE not defined) use
// the ELF syntax, while the BSD systems (ASM_UNDERSCORE defined) use the
// BSD syntax. Neat to know, this saves us from enumerating all the systems.
#ifdef ASM_UNDERSCORE
#define BSD_SYNTAX
#else
#define ELF_SYNTAX
#endif
#endif

#if defined (BSD_SYNTAX) || defined (ELF_SYNTAX)
#define R(r) %r
#define NUM(n) $##n
#define ADDR(a) $##a
#define X1
#define X2
#define X4
#define X8
#define MEM(base)(R(base))
#define MEM_DISP(base,displacement)displacement(R(base))
#define MEM_INDEX(base,index)(R(base),R(index))
#define MEM_SHINDEX(base,index,size)(R(base),R(index),size)
#define MEM_DISP_SHINDEX0(displacement,index,size)displacement(,R(index),size)
#define MEM_DISP_SHINDEX(base,displacement,index,size)displacement(R(base),R(index),size)
#define INDIR(value)*value
#define INSNCONC(mnemonic,size_suffix)mnemonic##size_suffix
#define INSN1(mnemonic,size_suffix,dst)INSNCONC(mnemonic,size_suffix) dst
#define INSN2(mnemonic,size_suffix,src,dst)INSNCONC(mnemonic,size_suffix) src,dst
#define INSN2MOVX(mnemonic,size_suffix,src,dst)INSNCONC(mnemonic,size_suffix##l) src,dst
#if defined(BSD_SYNTAX) || defined(COHERENT)
#define INSN2SHCL(mnemonic,size_suffix,src,dst)INSNCONC(mnemonic,size_suffix) R(cl),src,dst
#define REPZ repe ;
#else
#define INSN2SHCL(mnemonic,size_suffix,src,dst)INSNCONC(mnemonic,size_suffix) src,dst
#define REPZ repz ;
#endif
#define REP rep ;
#if defined(BSD_SYNTAX) && !(defined(__CYGWIN32__) || defined(__MINGW32__))
#define ALIGN(log) .align log,0x90
#endif
#if defined(ELF_SYNTAX) || defined(__CYGWIN32__) || defined(__MINGW32__)
#define ALIGN(log) .align 1<<log
#endif
#endif
#ifdef INTEL_SYNTAX
#define R(r) r
#define NUM(n) n
#define ADDR(a) OFFSET a
#define X1 BYTE PTR
#define X2 WORD PTR
#define X4 DWORD PTR
#define X8 QWORD PTR
#define MEM(base) [base]
#define MEM_DISP(base,displacement) [base+(displacement)]
#define MEM_INDEX(base,index) [base+index]
#define MEM_SHINDEX(base,index,size) [base+index*size]
#define MEM_DISP_SHINDEX0(displacement,index,size) [(displacement)+index*size]
#define MEM_DISP_SHINDEX(base,displacement,index,size) [base+(displacement)+index*size]
#define INDIR(value)value
#define INSNCONC(mnemonic,suffix)mnemonic##suffix
#define INSN1(mnemonic,size_suffix,dst)mnemonic dst
#define INSN2(mnemonic,size_suffix,src,dst)mnemonic dst,src
#define INSN2MOVX(mnemonic,size_suffix,src,dst)INSNCONC(mnemonic,x) dst,src
#define INSN2SHCL(mnemonic,size_suffix,src,dst)mnemonic dst,src,R(cl)
#define REPZ repz
#define REP rep
#define movsl  movs R(eax)
#define stosl  stos R(eax)
#define scasl  scas R(eax)
#define cmpsl  cmpsd
#ifdef _MSC_VER
// No pseudo-ops available in MS inline assembler.
#define ALIGN(log)
#else
#define ALIGN(log) .align log
#endif
#endif

#ifdef _MSC_VER
// No pseudo-ops available in MS inline assembler.
#define TEXT()
#else
#define TEXT() .text
#endif

#ifdef _MSC_VER
#define GLOBL(name)
#else
#define GLOBL(name) .globl name
#endif

// Define the DECLARE_FUNCTION(name) macro.
#ifdef _MSC_VER
#define DECLARE_FUNCTION(name)
#elif defined(__svr4__) || defined(__ELF__) || defined(__NetBSD__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__ROSE__) || defined(_SEQUENT_) || defined(DGUX) || defined(_SCO_COFF) || defined(_SCO_ELF)
#define DECLARE_FUNCTION(name) .type C(name),@function
#else
#define DECLARE_FUNCTION(name)
#endif

// Define the FUNBEGIN(name) and FUNEND() macros.
#ifdef _MSC_VER
// The "naked" attribute avoids the compiler generated prologue and epilogue
// (which saves the registers %ebx,%esi,%edi if no optimization is enabled,
// and those registers among %ebx,%esi,%edi which occur in the asm code
// if optimization is enabled).
#define FUNBEGIN(name) __declspec(naked) void name () { __asm {
#define FUNEND()                                      }       }
#else
#define FUNBEGIN(name) C(name##:)
#define FUNEND()
#endif

#define _

// Here we go!

TEXT()

// Rules about registers:
// Registers %eax,%edx,%ecx may be freely used.
// Registers %ebx,%esi,%edi must be saved before being used.
// Don't fiddle with register %ebp - some platforms don't like this.

