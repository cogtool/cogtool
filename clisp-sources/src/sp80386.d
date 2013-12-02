# Kleine Routine, die den Wert des Maschinenstacks zurückliefert.

  #ifdef _MSC_VER
    #include "asmi386.h"
    #undef ALIGN
    #define ALIGN
  #else
    #ifdef ASM_UNDERSCORE /* defined(__EMX__) || defined(__GO32__) || defined(linux) || defined(__386BSD__) || defined(__NetBSD__) || ... */
      #define C(entrypoint) _##entrypoint
    #else /* defined(sun) || defined(COHERENT) || ... */
      #define C(entrypoint) entrypoint
    #endif
    # Alignment. Note that some assemblers need ".align 3,0x90" whereas other
    # assemblers don't like this syntax. So we put in the "nop"s by hand.
    #if defined(ASM_UNDERSCORE) && !(defined(__CYGWIN32__) || defined(__MINGW32__))
      # BSD syntax assembler
      #define ALIGN  .align 3
    #else
      # ELF syntax assembler
      #define ALIGN  .align 8
    #endif
  #endif

        .text

        .globl C(getSP)

#    extern void* getSP (void);
        ALIGN
C(getSP:)
        leal    4(%esp),%eax    # aktueller Wert von ESP + 4 wegen Unterprogrammaufruf
        ret

