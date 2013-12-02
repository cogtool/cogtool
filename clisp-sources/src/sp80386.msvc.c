

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


    #if defined(ASM_UNDERSCORE) && !(defined(__CYGWIN32__) || defined(__MINGW32__))

      #define ALIGN  .align 3
    #else

      #define ALIGN  .align 8
    #endif
  #endif

        TEXT()

        GLOBL(C(getSP))


        ALIGN
FUNBEGIN(getSP)
        INSN2(lea,l	,X4 MEM_DISP(esp,4),R(eax))
        ret
FUNEND()


