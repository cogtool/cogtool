// Assembly language support for m68k CPU.
// Bruno Haible 29.5.1999

#ifdef ASM_UNDERSCORE
// SunOS, NetBSD, OpenBSD, Linux/a.out
#define C(entrypoint) _##entrypoint
#else
// SVR4, A/UX, AMIX, Atari, Linux/ELF
#define C(entrypoint) entrypoint
#endif

// When assembly language code is compiled into a shared library, ELF linkers
// need to know which symbols are functions.
#if defined(__NetBSD__) || defined(__OpenBSD__) || defined(__ELF__) || defined(__svr4__)
#define DECLARE_FUNCTION(name) .type C(name),@function
#else
#define DECLARE_FUNCTION(name)
#endif
