/* -*- C -*-
Copyright (c) 1996-1999 by Gilbert Baumann, distributed under GPL
Bruno Haible  1998-2000
Sam Steingold 2001-2005
----------------------------------------------------------------------------

   Title:       C implementation of CLX utilizing the Xlib
   Created:     Sat Dec  2 18:04:51 1995
   Author:      Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
   Copying:  (c) copyright 1996 by Gilbert Baumann distributed under GPL.

----------------------------------------------------------------------------

Revision 1.24  1999-10-17  bruno
- Use allocate_bit_vector in place of allocate_byte_vector. Remove ->data
  indirection.

Revision 1.23  1999-10-11  gilbert
- get_font_info_and_display: XGetAtomNames is a new X11R6 addition, use
  XGetAtomName instead.

Revision 1.22  1999-06-06  bruno
- get_font_info_and_display now optionally returns the Lisp font.
  It also fills in the font's encoding.
- New function to_XChar2b, used to convert a character sequence to a font
  index sequence. Works for linear non-iso8859-1 fonts (unlike Motif!).
  Still needs work for chinese or japanese fonts.

Revision 1.21  1999-05-30  bruno
- Add missing begin_callback() in `xlib_io_error_handler'.
- Save subr_self during some calls in xlib:change-property.
- Fix some obvious typos in `font_char_info'.
- Call XFree() when done with the result of XGetAtomName().
- Improved error handling in `get_font_info_and_display'.

Revision 1.20  1999-04-04  bruno
- Modifications for UNICODE.

Revision 1.19  1998-10-19  bruno
- Use the macro `nonreturning_function', not `nonreturning'.

Revision 1.18  1997-06-22  bruno
- replace first preprocessing with CCMP2C; then only comments need to be
  stripped before e2d can be applied.
- compilation in WIDE mode works now (use `eq', not `==' to compare objects)
- fixed buggy ESLOT4 macro (make_xatom wants a `Display*', not an `object')
- DYNAMIC_ARRAY has 3 arguments since 1996-07-22
- allocate_byte_array is now exported from array.d
- typos and spaces here and there

Revision 1.18  1997/06/12  00:23:35  gilbert
- nothing special

Revision 1.17  1997/06/02  16:19:27  gilbert
- Lots of minor tweeks here and there
- First attempt to get the IMAGE implementation right
- Found memory leak in font handling code
- fixed bug in xpm::read-pixmap-from-file occured when shape-mask-p = t
- (xlib:open-display "localhost") works now

Revision 1.16  1996/10/11  14:18:03  gilbert
- Some tweakings to get it smoother compiled under 07-22

Revision 1.15  1996/10/11  10:15:52  gilbert
- removed all GETTEXTs because it does not compile on 07-22
- error hander now calls the Lisp error handler
- fixed invokation of DYNAMIC_ARRAY
- removed all 'reg?' decls
- lots of bug fixes
- changed outlook of xlib:%restore-gcontext-components.
- the clip mask and the dash list are now retrievable from a gcontext
So the gcontext chapter should be complete now. [sans cache-p]

Revision 1.14  1996/10/09  09:03:42  gilbert
- lots of bug fixes
- changed outlook of xlib:%restore-gcontext-components.
- the clip mask and the dash list are now retrievable from a gcontext
So the gcontext chapter should be complete now. [sans cache-p]

Revision 1.13  1996/10/05  01:00:50  gilbert
- begin_call / end_call should be finally added everwhere..
- fixed some serve bugs in XLIB:CHANGE-PROPERTY.
- providing {WINDOW,PIXMAP,DRAWABLE,FONT,COLORMAP}-LOOKUP

Revision 1.12  1996/10/04  03:14:53  gilbert
- Introduced new macro 'STANDARD_XID_OBJECT_LOOK'.
- The {WINDOW,PIXMAP,DRAWABLE,FONT,COLORMAP}-{DISPLAY,PLIST,PLIST-SETTER,ID}
  functions now do proper type checking.
  The corresponding xx-EQUAL functions are based on the XID alone.
  Same needs to be done for PTR objects.
- The silly (and ineffient) 'general_p' function vanished.

Revision 1.11  1996/10/03  03:37:12  gilbert
- all invocations of "TheFpointer()->fp_pointer"
  are now guarded by "fp_validp".

Revision 1.10  1996/10/03  02:45:00  gilbert
- made the get_[su]int?? functions do type checking
- Got rid of get_string and get_stringable.

Revision 1.9  1996/10/02  10:39:45  gilbert
- eliminated get_xatom_name due to GC vulnerability.
- got rid of most get_string due to the same reason
- as always some begin/end_calls added.

Revision 1.8  1996/09/28  21:59:02  gilbert
- Only lots of code movements to gather stuff which belongs together
  also to appear together for better maintainability.

Revision 1.7  1996/09/28  20:52:24  gilbert
- Redone the invoke stuff, because
  YOU HAVE TO SAVE YOUR FILE BEFORE YOU CHECK IT IN!
  [Emacs does not check this 8-{ Realy angry! $%^#@#%$@ ]

Revision 1.6  1996/09/28  20:41:23  gilbert
- added type checking to get_{xid,ptr}_object
- got rid of display_of all now done with the get_foo_and_display
  functions, so get_font_info is now called get_font_info_and_display.
- also get rid of the 'invoke' function, it was considered too
  unportable.

Revision 1.5  1996/09/28  01:45:06  gilbert
Converted all enum definitions into the DEF_ENUM macro for code size.

Revision 1.4  1996/09/27  12:48:33  gilbert
Cleaned up event disassembling code

Revision 1.3  1996/08/02  10:51:40  gilbert
Only for Bruno.

Revision 1.2  1996/07/27  02:25:31  gilbert
*** empty log message ***

Revision 1.1  1996/07/08  15:47:43  gilbert
Initial revision
  ^^^ That is not true! Coding this started actually in Dec 1995.
      (Just around a couple of days befor chrismas?)
*/

/* --- TODO ---
- fake the REPORT-ASYNCHRONOUS-ERRORS slot on displays.

- Garnet seems to loose exposure events from time to time, I do not know
  if it is my fault or a garnet bug? This thing is hard to trace, since
  it seems to depend on how fast the window gets mapped, or how fast the
  garnet code is!

- the get_XXX functions *all* should do type checking

- Most uses of 'fixnum' are wrong, since the actual type is somewhat more
  precise known. fixnums are only correct as indexes into sequences and
  such!


-------
  When passing #<XLIB:GCONTEXT #x00000160>  to FONT-ASCENT it says:
  "Error: XLIB:FONT-ASCENT: NIL is not of type XLIB:FONT"
  Why is two way wrong:
    a. A gcontext should be ok where a font is
    b. Why is dumped NIL and not the gcontext?
  O.k. this was due to the fact that the font was actually never set.
  fix that!

  (Maybe we just pass the gcontext down to Xlib, since Xlib is supposed
  to do the same here.)
-------
  When a display is closed the host name and such sould not
  be available any longer
-------

- there should be a function called closed-p, which checks wether an given
  xid or ptr object is closed or not. When re-incarnating CLISP all CLX
  objects should get closed. get_xxx functions should check on that.  But
  there a some nasty problems:
   a.) when I setup the XFree... request it is not yet at the server
   b.) There may be additional references in the servers queque.
  Maybe I open a pseudo window, just for sneeking for destruction events?

- XLIB:TEXT-EXTENTS and XLIB:TEXT-WIDTH needs fixing for non simple
  string arguments.

- Garnet accidentally used :on/:off for save under values
  [But only sometimes]

- Scan for all funcalls and save subr_self prior to call.

- Is it right that we make the list of displays public?

- we should rethink a bit more the font handling stuff, because several
  situations could arise:

   . A font may not have an fid, this implies, that the font could not be
     hashed, furthermore we should init a finializer to discard also the
     fontinfo when the font object becomes inaccessible.
   . A font may come without per-character information.
     (But only when it has a 0 font id, hence a pseudo font)
   . If we open a pseudo font, we should then enter it into the hash table.
   . ensure that the  font-name slot has some  valid value.
     (It is even  not guaranteed to be available.)

- go thru' the whole source and find all error messages and check that they
  are given right. [fehler wants its arguments backwards!]

- since some make_xxx functions use value1, I should generally push the
  return values onto the stack before actually returning.

- since somebody could now create classes on top of window, check the xid we
  want to read, if it is unbound emit some reasonable error message.  [If it
  is unbound the slot-value function returns an error on its own already.]

- What about the plist slot? (Should we bind it to a useful value?)

- maybe we make the clos-instance representation optional? Since it eats
  some speed. (Fetching a slot value is not the fastest operation.)

- several X11 functions, which return lists may actually pass NULL as  the
  empty sequence?!

- some of the enumerating functions (list XListDepths etc) do not set the
  count_return on failure, so set it yourself to 0 before you make the call,
  so that we do not run into difficulties.

- Maybe we dont put all those symbols in the XLIB package to be able to
  include the MIT-CLX also.  (Generally CLISP lacks some of the nice package
  features Symbolics Common LISP provided.  There were also anonymous or
  invisible packages.)

- errors should be reported in a much more meaningful way.

- the big export list in clx.lisp seems to export some superfluous symbols.

- put also ptr objects into the hashtable.
  Is there any way to get 'em anyhow back?

- the xlib:free-xxx routines should remove that object from the hashtable.
  (or shouldn`t they?)  What does the Xserver with free`ed objects?
  And also they might be still alive, if some other application uses it.
  [Well simply take a look at the MIT code.] [What about GC?!]

- should DISPLAY be a class? What with VISUAL-INFO, COLOR an so on ...

- We should really make DISPLAY-AFTER-FUNCTION to live.
  The is also a XAfterFunction or something like that on the C side of life.

- What exactly is a drawable? a type? a class?

- We should insert a lot more type checks?

- Since type checks are rather expensive think about listening to SAFTY and
  SPEED ...
*/


/* --- DONE ----

- some functions emit errors saying #xDDD using decimal notation!
- upon disconnection we simply get a broken pipe message and exit
  [This is not the disired way, since we want to handle such an error.]
- with-gcontext !!!
- rename the 'font-name' slot of font to 'name'.
- take a look at the CLUE code, what it does with the :xxx option to
  create-xxx functions?!
- DISPLAY-AFTER-FUNCTION setter is needed.
- make display/window/pixmap clos-instances (see the CLUE patches for that)
- put xids into the hashtable and do not build the object new on each
  request.
- plists (partly done)
- get_visual
- how to proceed with visuals? In CLX a visual is just a card29 in CLX it is
  a pointer to a structure.
- Together with the CLX implementation should go a wholine implementation.
  (partly there)
*/

/* --- NOTE ---

  This package is not actually optimized for speed, since my intent and BTW
the overall intent of CLISP is to make the whole beast small.
(Actually you gain speed due to reduced paging).
Also this code is at some places BFI!

  The general idea behind writing this bunch of code, is to provide a CLX
implementation for CLISP, which is feasible of both speed and space.  I
actually use the libX library, because if you want to do graphics on your
machine, you will already have it in memory, so it adds no extra cost.  [One
disadvantage is that I am forced in some places to go into the internals of
the libX, since the CLX specification is more powerful at some places than
the libX implementation. This add another source of in-portability of CLISP,
so *please*, if you encounter compilation problems mail me,
so I could adjust the code ...]

  CLX adds on my machines another ~700k of memory needs for CLISP, but this
implementation add only 70k [*I* know that I am telling lies here, since the
libX11 itself has a size of ~500k; But as long as I have no pure LISP OS but
need the UNIX to boot it ...]  and a great bunch of speed.

  Also having this implemenation should gain a big plus against GCL.  (IMHO
gcl is very bad code compared to CLISP! [gcl is actually akcl]) flame,flame.

BTW It should be fun to write the graph.d routines on top of CLX!
*/

/* --- FUTURE ---

 Xpm Support?

We should also include support for the xpm library to have a nice access to
.xpm files, which is hardly needed, since I do not want to duplicate this
non-trivial code in Lisp.  But we have to think about a Lisp representation
for pixmaps. I could also imagine a `defpixmap` macro.  Just another
question is if we want to put the xpm routines into another package. (called
`x-pixmap` or just 'xpm'). I want also to write some documentation for the
then Lisp xpm routines. But since the xpm library seems to be a changing
thing it is also a question, how we cope with them.

 Incorporation into the FFI?

Since we use could convert now a WINDOW object into a libX11 Window object,
it may be worth offer this also to the FFI.  When I finished this work I
should take a look at the FFI.
*/

/* --- IMPLEMENTATION NOTES ---------------------------------------------------

The following types are only XID`s:
  window, drawable, font, pixmap, cursor, colormap, GContext, keysym

No XID, but data-structures:
  color, display, screen, GC


First I define some data types by providing get_xxx, make_xxx and xxx_p
functions.  Note that this set is not complete, since not all functions are
actually needed.  The consistent name is crucial, since some macros I define
later take a "type" argument, which is concatenated with 'make_' or 'get_'.
This is done to make this file more dense; (Thus save me a lot of redundant
typing.)


  type     | dpy? | XID-P | hashed-p | Note
 ----------+------+-------+----------+------------------------------
  GCONTEXT |  T   | NIL   |   NIL    | Is really a pointer
  WINDOW   |  T   | T     |   T      |
  PIXMAP   |  T   | T     |   T      |
  CURSOR   |  T   | T     |   T      |
  COLORMAP |  T   | T     |   T      |
  FONT     |  T   | T/NIL |   T/NIL  |
  SCREEN   |  T   | NIL   | (should  | Could also been represented as index?
           |      |       |    be)   |
  DISPLAY  |  NIL | NIL   |   NIL    |

  Class Hierarchy
 --------------------
    xlib-object --+--> xid-object --+--> DRAWABLE -+--> WINDOW
                  |                 |              +--> PIXMAP
                  |                 |--> CURSOR
                  |                 +--> COLORMAP
                  |                 +--> FONT
                  |
                  +--> ptr-object --+--> GCONTEXT
                                    |
                                    +--> DISPLAY

 Just in case you prefer a textual representation:

   (defclass xlib-object ()                      (plist display))
   (defclass xid-object  (xlib-object)           (xid))
   (defclass ptr-object  (xlib-object)           (ptr))

   (defclass drawable    (xid-object)            ())
   (defclass window      (drawable)              ())
   (defclass pixmap      (drawable)                 ())
   (defclass cursor      (xid-object)            ())
   (defclass colormap    (xid-object)            ())
   (defclass gcontext    (ptr-object)            ())
   (defclass display     (ptr-object)            ())
   (defclass font        (xid-object)            (font-info name))
*/


/* -- NOTE --
For further for grepability I use the following tags in comments:

 XXX            - really bad and a major risk to safety/usability.
 FIXME          - just fix me, something that ought to be done
                  before next release.
 TODO           - something, which should be done and is already
                  considered useful.
 FUTURE         - something which is just an idea and it is not yet
                  decided, if I ever implement it; It may also later
                  be considered silly.
                  I welcome discussion about those items.
 OK             - the opposite of XXX. Functions which are
                  considered to be totally finished and
                  had undergone a test are marked with this.
 UNDEFINED      - this is thought for undefined functions at whole
 NOTIMPLEMENTED - is thought for not implemented features
                  in an partly defined function.
*/

/* enough bla bla, let's start coding, we have a long long way before us ...*/


#include "clisp.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>          /* XGetVisualInfo */
#include <X11/Xcms.h>   /* forXcmsCCCOfColormap() & XcmsVisualOfCCC() */
#include <stdio.h>              /* sprintf() */
#include <string.h>             /* memcpy(), strchr(), strcpy() */
#include "config.h"
#if defined(TIME_WITH_SYS_TIME)
# include <sys/time.h>
# include <time.h>
#else
# if defined(HAVE_SYS_TIME_H)
#  include <sys/time.h>
# elif defined(HAVE_TIME_H)
#  include <time.h>
# endif
#endif
#if defined(HAVE_SYS_SOCKET_H)
# include <sys/socket.h>
#endif
#if defined(HAVE_NETDB_H)
# include <netdb.h>
#endif
#if defined(HAVE_NETINET_IN_H)
# include <netinet/in.h>
#endif

##if WANT_XSHAPE
/* must include this before DEFMODULE so that DEFCHECKER will work */
#include <X11/extensions/shape.h>
##endif

#define DEBUG_CLX 0

#ifndef FOREIGN
#error FOREIGN is not defined.
#error CLX needs a CLISP built with the foreign pointer datatype support.
#error Go into the main CLISP makefile and add a -DFOREIGN=void*
#error to CFLAGS make variable and rebuild CLISP before coming back here.
#endif

DEFMODULE(clx,"XLIB")

/* ... But first we provide some prototypes for functions, which maybe worth
 included in libX11 and are defined at the end of this file. */
/* might also been called XVisualIDFromVisual**(-1) : */
static Visual *XVisualIDToVisual (Display *dpy, VisualID vid);
/* Find the screen index from a Screen* : */
static int XScreenNo (Display *dpy, Screen *screen);

/* our own forward declaration */
static Display *pop_display (void);

/* Some fix-ups: */

#define NOTIMPLEMENTED NOTREACHED
#define UNDEFINED NOTIMPLEMENTED


/* debug */
#if DEBUG_CLX
#define dprintf(x) do{ printf x; fflush(stdout); }while(0)
#else
#define dprintf(x) do{}while(0)
#endif

/* it is not clear whether we can rely on `writing_to_subprocess' or
 must actually disable SIGPIPE - see src/spvw_sigpipe.d */
#define RELY_ON_WRITING_TO_SUBPROCESS
#if defined(RELY_ON_WRITING_TO_SUBPROCESS)
/* including <signal.h> just for the sake of SIGPIPE
   (which is always there anyway) is a total waste */
# if defined(HAVE_SIGNALS) /* && defined(SIGPIPE) */
extern
# endif
bool writing_to_subprocess;
# define begin_x_call() writing_to_subprocess=true;begin_call()
# define end_x_call()   end_call();writing_to_subprocess=false
#else
# define begin_x_call() begin_call()
# define end_x_call()   end_call()
extern void disable_sigpipe(void);
#endif
#define X_CALL(f) do{ begin_x_call(); f; end_x_call(); }while(0)

/* -------------------------------------------------------------------------
 *  General purpose utilities
 * ------------------------------------------------------------------------- */

/* sugar for funcall (used in macros, so not a macro) */
static inline object funcall1 (object fun, object arg)
{ pushSTACK(arg); funcall(fun,1); return value1; }

nonreturning_function(static,my_type_error,(object type, object datum))
{
  pushSTACK(datum);             /* TYPE-ERROR slot DATUM */
  pushSTACK(type);              /* TYPE-ERROR slot TYPE */
  pushSTACK(type); pushSTACK(datum); pushSTACK(TheSubr(subr_self)->name);
  fehler (type_error, ("~S: ~S is not of type ~S"));
}
nonreturning_function (static, closed_display_error,
                       (object caller, object dpy)) {
  pushSTACK(`XLIB::CLOSED-DISPLAY`);
  pushSTACK(`:DISPLAY`); pushSTACK(dpy);
  pushSTACK(`:CALLER`); pushSTACK(caller);
  funcall(L(error),5);
  abort(); /* ERROR does not return: avoid a compiler warning */
}

/* with_stringable_0 is much like with_string_0, but a symbol is also
 allowed as argument. This macro does type checking and may raise an error. */
#define with_stringable_0_tc(obj, encoding, cvar, body)  \
  do {                                                                 \
    object wsa0_temp =                                                 \
      (symbolp(obj) ? (object)Symbol_name (obj) : (object)(obj));      \
    if (stringp (wsa0_temp)) {                                         \
      with_string_0 (wsa0_temp, encoding, cvar, body);                 \
    } else my_type_error(`(OR STRING SYMBOL)`,obj);                    \
  } while(0)


/* -----------------------------------------------------------------------
 *  Integer data types
 * ----------------------------------------------------------------------- */

/* Huh?! These functions do not check the type?! */
#define make_uint8(i)    uint8_to_I (i)
#define make_uint16(i)   uint16_to_I (i)
#define make_uint29(ul)  UL_to_I (ul)
#define make_uint32(ul)  UL_to_I (ul)

#define make_sint8(i)    sint8_to_I (i)
#define make_sint16(i)   sint16_to_I (i)
#define make_sint32(i)   L_to_I (i)

#define get_bool(obj)   (!nullp(obj))
#define make_bool(b)    ((b)?(T):(NIL))

#define pixel_p(obj)     integerp (obj)
#define get_pixel(obj)   get_uint32(obj)
#define make_pixel(obj)  make_uint32(obj)

#define get_fixnum(obj)  fixnum_to_V (obj) /* WARNING: obj should be a variable (evaluated multiple), and no range check is performed */

#if 0
#define get_uint8(obj)   I_to_uint8(obj)
#define get_uint16(obj)  I_to_uint16(obj)
#define get_uint29(obj)  I_to_UL (obj)
#define get_uint32(obj)  I_to_UL (obj)

#define get_sint8(obj)   I_to_sint8(obj)
#define get_sint16(obj)  I_to_sint16(obj)
#define get_sint32(obj)  I_to_sint32(obj)
#else

#define uint29_p uint32_p /* XXX the actual type checking code is just too weird! */
#define I_to_uint29 I_to_UL     /* XXX ditto */

#define DEFINE_INTEGER_GETTER(type, lspnam)                     \
  static inline type get_##type (object obj) {                  \
    if (type##_p (obj))                                         \
      return I_to_##type (obj);                                 \
    else my_type_error(lspnam,obj);                             \
  }                                                             \
  static inline type get_##type##_0 (object obj) {              \
    return missingp(obj) ? 0 : get_##type(obj);                 \
  }

DEFINE_INTEGER_GETTER (uint8,  `XLIB::CARD8`)
DEFINE_INTEGER_GETTER (uint16, `XLIB::CARD16`)
DEFINE_INTEGER_GETTER (uint29, `XLIB::CARD29`)
DEFINE_INTEGER_GETTER (uint32, `XLIB::CARD32`)

DEFINE_INTEGER_GETTER (sint8,  `XLIB::INT8`)
DEFINE_INTEGER_GETTER (sint16, `XLIB::INT16`)
DEFINE_INTEGER_GETTER (sint32, `XLIB::INT32`)
#endif

static uint32 get_aint32 (object obj)
{ /* This is special routine, which accepts either an uint32 or a sint32.
 However returned is an uint32.
 Used by XLIB:CHANGE-PROPERTY */
  if (uint32_p (obj))
    return I_to_uint32 (obj);
  if (sint32_p (obj))
    return (uint32)I_to_sint32 (obj);
  else my_type_error(`(OR XLIB::INT32 XLIB::CARD32)`,obj);
}


/* -----------------------------------------------------------------------
 *  Displays
 * ----------------------------------------------------------------------- */

/* Objects of type DISPLAY are currently represented as structure; here are the
 slots: The actual defstruct definition in clx.lisp must match. There is a
 warning in the code. */
#define slot_DISPLAY_FOREIGN_POINTER 1
#define slot_DISPLAY_HASH_TABLE      2
#define slot_DISPLAY_PLIST           3
#define slot_DISPLAY_AFTER_FUNCTION  4
#define slot_DISPLAY_ERROR_HANDLER   5

/* The display contains a hash table. All XID objects are entered there, so
 that two XID objects, with equal XID are actually eq. */
static object make_display (Display *dpy)
{ /* Given the C representation of a display create the Lisp one and
 initialize it.  The newly created display is added to XLIB:*DISPLAYS*. */
  pushSTACK(`(XLIB::DISPLAY)`); pushSTACK(fixnum(6));
  funcall(L(make_structure),2); pushSTACK(value1);
  TheStructure(STACK_0)->recdata[slot_DISPLAY_FOREIGN_POINTER]
    = allocate_fpointer (dpy);
#if oint_data_len<29
  pushSTACK(S(Ktest)); pushSTACK(S(stablehash_equal)); /* key is a cons */
#else
  pushSTACK(S(Ktest)); pushSTACK(S(stablehash_eq)); /* key is a fixnum */
#endif
  funcall (L(make_hash_table), 2);
  TheStructure(STACK_0)->recdata[slot_DISPLAY_HASH_TABLE]     = value1;
  TheStructure(STACK_0)->recdata[slot_DISPLAY_PLIST]          = NIL;
  TheStructure(STACK_0)->recdata[slot_DISPLAY_AFTER_FUNCTION] = NIL;
  TheStructure(STACK_0)->recdata[slot_DISPLAY_ERROR_HANDLER]  = NIL;

  /* Now enter the display into the list of all displays: */
  pushSTACK(STACK_0);
  pushSTACK(Symbol_value(`XLIB::*DISPLAYS*`));
  funcall (L(cons), 2);
  Symbol_value(`XLIB::*DISPLAYS*`) = value1;
  return value1 = popSTACK();
}

static object find_display (Display *display)
{ /* Searched the XLIB:*DISPLAY* variable for `display',
 return NIL, if display was not found.
 Used by the error handler, the only callback code here. */
  pushSTACK(Symbol_value (`XLIB::*DISPLAYS*`));
  for (;consp (STACK_0); STACK_0 = Cdr (STACK_0)) {
    pushSTACK(Car(STACK_0));
    if (pop_display() == display)
      return Car (popSTACK());
  }
  skipSTACK(1);
  return NIL;
}

static Bool ensure_living_display (gcv_object_t *objf)
{ /* ensures that the object pointed to by 'objf' is really a display.
 Also the display must be 'alive', meaning that it does not contain
 a fptr from an previous incarnation of CLISP.
 If all that does not hold an error is signaled.
 Finally, returns an indicator of whether the display has been closed.
 'objf' should point into the stack due to GC. */
  if (typep_classname (*objf, `XLIB::DISPLAY`)) { /* Is it a display at all? */
    object fptr = TheStructure(*objf)->recdata[slot_DISPLAY_FOREIGN_POINTER];
    return (fpointerp(fptr) && fp_validp(TheFpointer(fptr))
            && (TheFpointer(fptr)->fp_pointer != NULL));
  }
  /* Fall through -- raise type error */
  my_type_error(`XLIB::DISPLAY`,*(objf));
}

DEFUN(XLIB:CLOSED-DISPLAY-P, display)
{
  VALUES_IF(!ensure_living_display(&STACK_0));
  skipSTACK(1);
}

/* display_hash_table -- return the hashtable of a display object
 > the display object
 < its hash table
 This function is somewhat silly, since it introduces double type checking! */
static object display_hash_table (object dpy)
{
  pushSTACK(dpy);
  if (!ensure_living_display(&(STACK_0)))
    closed_display_error(TheSubr(subr_self)->name,STACK_0);
  return TheStructure (popSTACK())->recdata[slot_DISPLAY_HASH_TABLE];
}

/* pop_display --- return the C Display* of an display object
 > STACK_0: the Lisp DISPLAY object */
static Display *pop_display (void)
{
  if (!ensure_living_display(&(STACK_0)))
    closed_display_error(TheSubr(subr_self)->name,STACK_0);
  STACK_0 = TheStructure (STACK_0)->recdata[slot_DISPLAY_FOREIGN_POINTER];
  return (Display*) TheFpointer(popSTACK())->fp_pointer;
}


/* -----------------------------------------------------------------------
 *  PTR and XID objects
 * ----------------------------------------------------------------------- */

/*  First the ptr ones.

  ptr_objs are screens and gcontexts, these objects are not hashed.
  (which is a bad idea btw).  But on the other hand for gcontexts it is
  not too bad, since you get `em only once.

  Another story are the screen, these could be cached. Or we do not give the
  actual screen structure, but pass simply the index? */
static object make_ptr_obj (object type, object dpy, void *ptr)
{ /* (make-instance type :display dpy :ptr ptr) */
  pushSTACK(type);
  pushSTACK(`:DISPLAY`);  pushSTACK(dpy);
  pushSTACK(`:PTR`);      pushSTACK(allocate_fpointer (ptr));
  funcall(`CLOS::MAKE-INSTANCE`,5);
  return value1;
}

/* return the fp_pointer of the foreign slot
   value1 is set the slot value
 can trigger GC */
static void* foreign_slot (object obj, object slot) {
  pushSTACK(obj); pushSTACK(slot); funcall(L(slot_value), 2);
  return TheFpointer(value1 = check_fpointer(value1,false))->fp_pointer;
}

static void *get_ptr_object_and_display (object type, object obj,
                                         Display **dpyf)
{ /* 'obj'  is the lisp object, whose C representation is returned.
 When 'dpyf' is non-0, the display of 'obj' is also returned and it is
 ensured that it lives. [Otherwise an error is signaled.]
 If 'obj' is not of type 'type', a symbol naming the desired class,
 an error is issued.
 Hence this function ensures, that a proper object is returned, or nothing. */
  pushSTACK(type);
  pushSTACK(obj);

  if (typep_classname (STACK_0, STACK_1)) {
    if (dpyf) { /* do we want the display? */
      pushSTACK(STACK_0); pushSTACK(`XLIB::DISPLAY`);
      funcall(L(slot_value), 2); pushSTACK(value1);
      *dpyf = pop_display ();
    }
    { void * ret = foreign_slot(STACK_0/* 'obj' */,`XLIB::PTR`);
      skipSTACK(2);                    /* clean up */
      return ret;
    }
  } else my_type_error(STACK_1/*type*/,STACK_0/*obj*/);
}

/* Now the XID objects */

static object make_xid_obj_low (gcv_object_t *prealloc, gcv_object_t *type,
                                gcv_object_t *dpy, XID xid)
{
  if (nullp (*prealloc)) {
    /* (make-instance type :display dpy :id xid) */
    pushSTACK(*type);
    pushSTACK(`:DISPLAY`);  pushSTACK(*dpy);
    pushSTACK(`:ID`);       pushSTACK(make_uint29 (xid));
    funcall(`CLOS::MAKE-INSTANCE`,5);
    return value1;
  } else {
    /* TODO: We should check the type of the preallocated object?!
             [Is is a bug or a feature?] */
    pushSTACK(*prealloc);
    pushSTACK(`XLIB::DISPLAY`);
    pushSTACK(*dpy);
    funcall (L(set_slot_value), 3);

    pushSTACK(*prealloc);
    pushSTACK(`XLIB::ID`);
    pushSTACK(make_uint29 (xid));
    funcall (L(set_slot_value), 3);

    return *prealloc;
  }
}

#if oint_data_len<29
DEFVAR(xlib_a_cons,`(NIL . NIL)`);
/* return the XID object for lookup (old) */
static inline object XID_to_object_old (XID xid) {
  Car (O(xlib_a_cons)) = make_uint16 (xid & 0xFFFF); /* lower halfword */
  Cdr (O(xlib_a_cons)) = make_uint16 (xid >> 16);    /* upper halfword */
  return O(xlib_a_cons);
}
/* return the XID object for creation (new) */
static inline object XID_to_object_new (XID xid) {
  pushSTACK(make_uint16 (xid & 0xFFFF)); /* lower halfword */
  pushSTACK(make_uint16 (xid >> 16));    /* upper halfword */
  funcall(L(cons),2);                    /* cons them */
  return value1;
}
#else
#define XID_to_object_old(xid)  fixnum(xid)
#define XID_to_object_new(xid)  fixnum(xid)
#endif

/* find the resource in the display hash table
 < display object, XID number
 > returns dpy->hash-table if NOT found
           nullobj if found, in which case the object found is in value1 */
static object lookup_xid (object dpy, XID xid) {
  if (xid == 0) { /* This is trivial, but is it also right?! */
    VALUES1(NIL);
    return nullobj;
  } else {
    object ht = display_hash_table(dpy);
    value1 = gethash(XID_to_object_old(xid),ht,false);    /* look it up */
    if (!eq(value1,nullobj)) {  /* something found? */
      mv_count = 1;             /* simply return what we found */
      return nullobj;
    } else
      return ht;                /* return the hash-table */
  }
}

/* set the ID to map to RESOURCE on display
 < display hash-table object, XID number, resource object
 can trigger GC */
static void set_resource_id (gcv_object_t *ht, XID xid,
                             gcv_object_t *resource) {
  value1 = XID_to_object_new(xid);
  pushSTACK(value1);                     /* key for puthash */
  pushSTACK(*ht);                        /* hashtable */
  pushSTACK(*resource);                  /* value */
  funcall (L(puthash), 3);               /* put it into the hashtable */
}

/* delete the resource ID from the display
 < display hash-table object, XID number */
static Values delete_resource_id (gcv_object_t *ht, XID xid) {
  pushSTACK(XID_to_object_old(xid)); pushSTACK(*ht); funcall(L(remhash),2);
}

static object make_xid_obj_2 (object type, object dpy, XID xid,
                              object prealloc)
{ /* NOTE: - This code is not reentrant :-( But hence it saves consing
         - may be we want to check the most significant 3 bits in xid, since
           GC inquiry functions return those values, if slot has an unknown
           value.
         - We could add even more safty here
           1. bark if lookup succeed and prealloc was specified.
              [But this  situation  should  not be able  to   occurr,  since a
              prealloc is only given upon creation requests.]  However MIT-CLX
              does this  check  and raises a hash-error,   if something is not
              o.k.  with the  hash  table. [But only if  you  set a debug flag
              somewhere].
           2. If lookup succeeds we could also check the type.
           3. We should check the type of the preallocated object?!
              [Compare to make_ptr_obj] */
  object ht = lookup_xid(dpy,xid);
  if (!eq(ht,nullobj)) { /* allocate and enter object into the hashtable */
    pushSTACK(prealloc);        /* save is save */
    pushSTACK(type);            /* ditto */
    pushSTACK(dpy);             /* ditto */
    pushSTACK(ht);              /* hashtable */
    pushSTACK(make_xid_obj_low (&STACK_3, &STACK_2, &STACK_1, xid));
    set_resource_id(&STACK_1,xid,&STACK_0); /* enter it into the hashtable */
    VALUES1(popSTACK());        /* return freshly allocated structure */
    skipSTACK(4);               /* remove saved prealloc, type, dpy, ht */
  }
  return value1;
}

static XID get_xid_object_and_display (object type, object obj, Display **dpyf)
{
  pushSTACK(type);
  pushSTACK(obj);

  if (typep_classname (STACK_0, STACK_1)) {
    if (dpyf) {                 /* do we want the display? */
      pushSTACK(STACK_0); pushSTACK(`XLIB::DISPLAY`);
      funcall(L(slot_value), 2); pushSTACK(value1);
      *dpyf = pop_display();
    }

    pushSTACK(STACK_0); /* obj already on stack */ pushSTACK(`XLIB::ID`);
    funcall(L(slot_value), 2);
    ASSERT(integerp (value1)); /* FIXME */
    skipSTACK(2);                                      /* clean up */
    return (XID)(get_uint29 (value1));                 /* all done */
  } else my_type_error(STACK_1/*type*/,STACK_0/*obj*/);
}

static object get_display_obj_tc (object type, object obj)
{
  if (typep_classname (obj, type)) {
    pushSTACK(obj); pushSTACK(`XLIB::DISPLAY`);
    funcall(L(slot_value), 2); return value1;
  } else my_type_error(type,obj);
}

static object get_display_obj (object obj)
{ /* XXX type checking [Well on the other hand is it really necessary?]
       I want to use the combined function above. */
  pushSTACK(obj); pushSTACK(`XLIB::DISPLAY`);
  funcall(L(slot_value), 2); return value1;
}


/* -----------------------------------------------------------------------
 *  Specializied getters/makers/predicates
 * ----------------------------------------------------------------------- */

/* Simple Getters */
#define get_xid_object(type,obj) get_xid_object_and_display(type,obj,0)
#define get_ptr_object(type,obj) get_ptr_object_and_display(type,obj,0)

#define get_gcontext(obj) ((GC)      get_ptr_object (`XLIB::GCONTEXT`, obj))
#define get_screen(obj)   ((Screen*) get_ptr_object (`XLIB::SCREEN`,   obj))
#define get_image(obj)    ((XImage*) get_ptr_object (`XLIB::IMAGE`,    obj))
#define get_window(obj)   ((Window)  get_xid_object (`XLIB::WINDOW`,   obj))
#define get_pixmap(obj)   ((Pixmap)  get_xid_object (`XLIB::PIXMAP`,   obj))
#define get_cursor(obj)   ((Cursor)  get_xid_object (`XLIB::CURSOR`,   obj))
#define get_colormap(obj) ((Colormap)get_xid_object (`XLIB::COLORMAP`, obj))
#define get_drawable(obj) ((Drawable)get_xid_object (`XLIB::DRAWABLE`, obj))

/* Combined getters */
#define get_drawable_and_display(obj, dpyf) ((Drawable)get_xid_object_and_display (`XLIB::DRAWABLE`, obj, dpyf))
#define get_window_and_display(obj, dpyf)   ((Window)  get_xid_object_and_display (`XLIB::WINDOW`,   obj, dpyf))
#define get_pixmap_and_display(obj, dpyf)   ((Pixmap)  get_xid_object_and_display (`XLIB::PIXMAP`,   obj, dpyf))
#define get_cursor_and_display(obj, dpyf)   ((Cursor)  get_xid_object_and_display (`XLIB::CURSOR`,   obj, dpyf))
#define get_colormap_and_display(obj, dpyf) ((Colormap)get_xid_object_and_display (`XLIB::COLORMAP`, obj, dpyf))
#define get_gcontext_and_display(obj,dpyf)  ((GC)      get_ptr_object_and_display (`XLIB::GCONTEXT`, obj, dpyf))
#define get_screen_and_display(obj,dpyf)    ((Screen*) get_ptr_object_and_display (`XLIB::SCREEN`,   obj, dpyf))
#define get_font_and_display(obj, dpyf)     ((Font)    get_xid_object_and_display (`XLIB::FONT`,     obj, dpyf))

/* Predicates */
#define drawable_p(obj) (typep_classname (obj, `XLIB::DRAWABLE`))
#define window_p(obj)   (typep_classname (obj, `XLIB::WINDOW`))
#define pixmap_p(obj)   (typep_classname (obj, `XLIB::PIXMAP`))
#define cursor_p(obj)   (typep_classname (obj, `XLIB::CURSOR`))
#define colormap_p(obj) (typep_classname (obj, `XLIB::COLORMAP`))
#define font_p(obj)     (typep_classname (obj, `XLIB::FONT`))
#define gcontext_p(obj) (typep_classname (obj, `XLIB::GCONTEXT`))
#define screen_p(obj)   (typep_classname (obj, `XLIB::SCREEN`))
#define display_p(obj)  (typep_classname (obj, `XLIB::DISPLAY`))
#define color_p(obj)    (typep_classname (obj, `XLIB::COLOR`))

/* Simple Makers */
#define make_xid_obj(a,b,c) make_xid_obj_2(a,b,c,NIL)

#define make_window(dpy,win)   (make_window_2(dpy,win,NIL))
#define make_pixmap(dpy,pix)   (make_pixmap_2(dpy,pix,NIL))
#define make_drawable(dpy,da)  (make_window (dpy, da))
#define make_cursor(dpy,cur)   (make_xid_obj (`XLIB::CURSOR`, dpy, cur))
#define make_colormap(dpy,cm)  (make_xid_obj (`XLIB::COLORMAP`, dpy, cm))
#define make_gcontext(dpy,gc)  (make_ptr_obj (`XLIB::GCONTEXT`, dpy, gc))
#define make_screen(dpy, srcn) (make_ptr_obj (`XLIB::SCREEN`, dpy, srcn))
#define make_font(dpy,fn)      (make_font_with_info (dpy, fn, NIL, NULL))

/* Makers with prealloc */
#define make_window_2(dpy, win, prealloc) (make_xid_obj_2 (`XLIB::WINDOW`, dpy, win, prealloc))
#define make_pixmap_2(dpy, pm, prealloc)  (make_xid_obj_2 (`XLIB::PIXMAP`, dpy, pm, prealloc))


static object make_font_with_info (object dpy, Font fn, object name,
                                   XFontStruct *info)
{ /* This looks much more like assembler, doesn't it? */
  pushSTACK(name);                                 /* save the name */
  pushSTACK(make_xid_obj (`XLIB::FONT`, dpy, fn)); /* make the xid-object and save it */

  /* fetch old FONT-INFO slot */
  pushSTACK(STACK_0);                           /* xid-object */
  pushSTACK(`XLIB::FONT-INFO`);                 /* slot */
  funcall(L(slot_value), 2); /* (slot-value new-xid-object `font-info) */

  /* do not overwrite any already fetched font info */
  if (!fpointerp (value1)) {
    /* o.k allocate a new fpointer */
    pushSTACK(STACK_0);        /* the new xid-object */
    pushSTACK(`XLIB::FONT-INFO`);                       /* the slot */
    pushSTACK(allocate_fpointer (info));                /* new value */
    funcall (L(set_slot_value), 3); /* update the :font-info slot */
  }

  if (!nullp (STACK_1)) {           /* name */
    pushSTACK(STACK_0);             /* the new xid-object */
    pushSTACK(`XLIB::NAME`);        /* the :name slot */
    pushSTACK(STACK_3);             /* [name] new value */
    funcall (L(set_slot_value), 3); /* update the :name slot */
  }

  value1 = STACK_0;             /* return value = new xid-object */
  skipSTACK(2);                 /* clean up */
  return value1;
}

static Font get_font (object obj);

static XFontStruct *get_font_info_and_display (object obj, object* fontf,
                                               Display **dpyf)
{ /* Fetches the font information from a font, if it isn't there
      already, query the server for it.
      Further more if a gcontext is passed in, fetch its font slot instead.
      Does type checking and raises error if unappropriate object passed in.
      If 'fontf' is non-0, also the font as a Lisp object is returned.
      If 'dpyf' is non-0, also the display of the font is returned and it is
      ensured that the display actually lives. */
  XFontStruct *info;
  Display *dpy;
  Font font;

  if (gcontext_p (obj)) {
    /* In all places where a font object is required, a gcontext should
       be accepted too, so fetch the font slot and go on ... */
    pushSTACK(obj); pushSTACK(NIL);
    funcall(``XLIB:GCONTEXT-FONT``,2);
    obj = value1;               /* Now we have the font [or nothing] */
  }

  if (!font_p (obj)) my_type_error(`XLIB::FONT`,obj);

  pushSTACK(obj);               /* save */

  info = (XFontStruct*) foreign_slot(obj,`XLIB::FONT-INFO`);
  if (!info) {
    /* We have no font information already, so go and ask the server for it. */

    pushSTACK(value1);   /* but first save what we found. */

    font = get_font_and_display (STACK_1, &dpy);
    X_CALL(info = XQueryFont (dpy, font));

    if (!info) {
      pushSTACK(STACK_1); pushSTACK(TheSubr (subr_self)->name);
      fehler (error, "~S: Nonexistent font: ~S");
    }

    if (dpyf) *dpyf = dpy;

    ASSERT (fpointerp (STACK_0));

    TheFpointer(STACK_0)->fp_pointer = info; /* Store it in the foreign pointer */
    skipSTACK(1);

#  ifdef UNICODE
    { /* Determine the font's encoding, so we can correctly convert
         characters to indices.
         Call (XLIB:FONT-PROPERTY font "CHARSET_REGISTRY")
         and  (XLIB:FONT-PROPERTY font "CHARSET_ENCODING")
         and translate the resulting pairs to CLISP encodings. */
      Atom xatom;
      unsigned long rgstry;
      unsigned long encdng;
      begin_x_call();
      xatom = XInternAtom (dpy, "CHARSET_REGISTRY", 0);
      if (XGetFontProperty (info, xatom, &rgstry)) {
        xatom = XInternAtom (dpy, "CHARSET_ENCODING", 0);
        if (XGetFontProperty (info, xatom, &encdng)) {
          Atom xatoms[2];
          char* names[2];
          int status;

          xatoms[0] = rgstry;
          xatoms[1] = encdng;
          names[0] = NULL;
          names[1] = NULL;
#        if !defined(HAVE_XGETATOMNAMES)
          names[0] = XGetAtomName (dpy, xatoms[0]);
          names[1] = XGetAtomName (dpy, xatoms[1]);
          status = names[0] && names[1];
#        else
          status = XGetAtomNames (dpy, xatoms, 2, names); /* X11R6 */
#        endif
          if (status) {
            /* this encoding canonicalization was requested by
               Pascal J.Bourguignon <pjb@informatimago.com>
               in <http://article.gmane.org/gmane.lisp.clisp.general:7794> */
            char* whole = (char*) alloca(strlen(names[0])+strlen(names[1])+3);
            if (!strncasecmp(names[0],"iso",3) && names[0][3] != '-') {
              strcpy(whole,"ISO-");
              strcat(whole,names[0]+3);
            } else strcpy(whole,names[0]);
            strcat(whole,"-");
            strcat(whole,names[1]);
            end_x_call();
            pushSTACK(`:CHARSET`);
            pushSTACK(asciz_to_string(whole,GLO(misc_encoding)));
            pushSTACK(`:OUTPUT-ERROR-ACTION`);
            pushSTACK(fixnum(info->default_char));
            funcall(L(make_encoding),4);
            pushSTACK(STACK_0); /* obj */
            pushSTACK(`XLIB::ENCODING`);
            pushSTACK(value1);
            funcall(L(set_slot_value),3);
            begin_x_call();
          }
          if (names[0])
            XFree (names[0]);
          if (names[1])
            XFree (names[1]);
        }
      }
      end_x_call();
    }
#  endif
  } else if (dpyf) /* caller wants the display, so get it! */
    unused get_font_and_display (STACK_0, dpyf);

  if (fontf) *fontf = STACK_0;

  skipSTACK(1);
  return info;              /* all done */
}

static object get_font_name (object obj)
{
  pushSTACK(obj);                       /* the instance */
  pushSTACK(`XLIB::NAME`);              /* slot */
  funcall(L(slot_value), 2);            /* lookup the slot */
  return value1;
}

#define ENSURE_TYPE(datum,booli,type)  if (!booli) my_type_error(type,datum)

static object get_slot (object obj, object slot)
{ /* like gethash(): return nullobj on unbound slot and slot value otherwise */
  pushSTACK(obj); pushSTACK(slot); /* save for SLOT-VALUE */
  pushSTACK(obj); pushSTACK(slot); funcall(L(slot_boundp),2);
  if (nullp(value1)) { skipSTACK(2); return nullobj; }
  funcall(L(slot_value),2); return value1;
}

static Font get_font (object self)
{ /* Does type-checking. */
  object font_id;
  pushSTACK(self);              /* save */
  ENSURE_TYPE (STACK_0, font_p(STACK_0), `XLIB::FONT`);
  font_id = get_slot(STACK_0,`XLIB::ID`);
  if (!eq(font_id,nullobj)) { /* We have already a fid, so return it. */
    skipSTACK(1);               /* clean up */
    ASSERT(integerp(font_id));
    return (XID)(get_uint29(font_id));
  } else { /* No font id => lookup the name & open that font */
    object name = get_font_name(STACK_0/*self*/);
    if (boundp(name)) { /* Ok there is a name ... so try to open the font */
      Font font; Display *dpy = (pushSTACK(STACK_0),pop_display ());
      with_string_0 (name, GLO(misc_encoding), namez, { /* XXX */
          X_CALL(font = XLoadFont(dpy,namez));
        });
      if (font) { /* Hurra! We got a font id, so enter it */
        pushSTACK(`XLIB::ID`); pushSTACK(make_uint29(font));
        funcall(L(set_slot_value),3);
        /* XXX -- We should enter it also into the hash table! */
        return font;            /* all done */
      } else { /* We could not open the font, so emit an error message */
        pushSTACK(TheSubr(subr_self)->name);    /* function name */
        fehler (error, "~S: Cannot open pseudo font ~S");
      }
    } else {                 /* We have no name, tell that the luser. */
      pushSTACK(TheSubr(subr_self)->name);     /* function name */
      fehler (error, "~S: Cannot open pseudo font ~S, since it has no name associated with it.");
    }
  }
}

static Atom get_xatom_general (Display *dpy, object obj, int internp)
{ /* Converts a symbol or a string to an xatom. If 'obj' is no symbol
     nor a string, an error is raised.  if 'internp' is non-0 the atom
     is interned on the server. */
  Atom xatom;

  with_stringable_0_tc (obj, GLO(misc_encoding), atom_name, {
      X_CALL(xatom = XInternAtom (dpy, atom_name, !internp));
    });

  return xatom;
}

#define get_xatom(dpy,obj)          get_xatom_general (dpy, obj, 1) /* interning version */
#define get_xatom_nointern(dpy,obj) get_xatom_general (dpy, obj, 0) /* non-interning version */

static object make_visual (Visual *visual)
{
  XID id;
  X_CALL(id = XVisualIDFromVisual (visual));
  return make_uint29 (id);
}

static Visual *get_visual (Display *dpy, object vid)
{
  /* no begin/end_call here XVisualIDToVisual is defined by us. */
  return XVisualIDToVisual (dpy, get_uint29 (vid));
}


/* -----------------------------------------------------------------------
 * Lots of enums
 * ----------------------------------------------------------------------- */

DEFCHECKER(get_map_state,default=,UNMAPPED=IsUnmapped                   \
           UNVIEWABLE=IsUnviewable VIEWABLE=IsViewable)
#define make_map_state get_map_state_reverse
DEFCHECKER(get_shape,default=,COMPLEX=Complex CONVEX=Convex \
           NON-CONVEX=Nonconvex)
DEFCHECKER(get_W_class,default=,COPY=CopyFromParent INPUT-OUTPUT=InputOutput \
           INPUT-ONLY=InputOnly)
#define make_W_class get_W_class_reverse
DEFCHECKER(get_stack_mode,default=,ABOVE=Above BELOW=Below TOP-IF=TopIf \
           BOTTOM-IF=BottomIf OPPOSITE=Opposite)
#define make_stack_mode get_stack_mode_reverse
DEFCHECKER(get_arc_mode,default=,CHORD=ArcChord PIE-SLICE=ArcPieSlice)
#define make_arc_mode get_arc_mode_reverse
DEFCHECKER(get_line_style,default=,SOLID=LineSolid DASH=LineOnOffDash \
           DOUBLE-DASH=LineDoubleDash)
#define make_line_style get_line_style_reverse
DEFCHECKER(get_cap_style,default=,NOT-LAST=CapNotLast BUTT=CapButt    \
           ROUND=CapRound PROJECTING=CapProjecting)
#define make_cap_style get_cap_style_reverse
DEFCHECKER(get_join_style,default=, \
           MITER=JoinMiter ROUND=JoinRound BEVEL=JoinBevel)
#define make_join_style get_join_style_reverse
DEFCHECKER(get_fill_style,default=,SOLID=FillSolid TILED=FillTiled \
           STIPPLED=FillStippled OPAQUE-STIPPLED=FillOpaqueStippled)
#define make_fill_style get_fill_style_reverse
DEFCHECKER(get_fill_rule,default=, EVEN-ODD=EvenOddRule WINDING=WindingRule)
#define make_fill_rule get_fill_rule_reverse
DEFCHECKER(get_subwindow_mode,default=, \
           CLIP-BY-CHILDREN=ClipByChildren INCLUDE-INFERIORS=IncludeInferiors)
#define make_subwindow_mode get_subwindow_mode_reverse
DEFCHECKER(get_gravity,default=,FORGET=ForgetGravity                    \
           NORTH-WEST=NorthWestGravity NORTH=NorthGravity               \
           NORTH-EAST=NorthEastGravity WEST=WestGravity CENTER=CenterGravity \
           EAST=EastGravity SOUTH-WEST=SouthWestGravity SOUTH=SouthGravity \
           SOUTH-EAST=SouthEastGravity STATIC=StaticGravity)
#define make_gravity get_gravity_reverse
/* NIM: the :static gravity is not mentioned in the CLX manual. */
DEFCHECKER(get_visibility_state,default=, UNOBSCURED=VisibilityUnobscured \
           PARTLY-OBSCURED=VisibilityPartiallyObscured                  \
           FULLY-OBSCURED=VisibilityFullyObscured)
#define make_visibility_state get_visibility_state_reverse
DEFCHECKER(get_top_or_bottom,default=,TOP=PlaceOnTop BOTTOM=PlaceOnBottom)
#define make_top_or_bottom get_top_or_bottom_reverse
DEFCHECKER(get_new_value_or_deleted,default=, \
           NEW-VALUE=PropertyNewValue DELETED=PropertyDelete)
#define make_new_value_or_deleted get_new_value_or_deleted_reverse
DEFCHECKER(get_ordering,default=Unsorted, UNSORTED=Unsorted Y-SORTED=YSorted \
           YX-SORTED=YXSorted YX-BANDED=YXBanded)
DEFCHECKER(get_mapping_request,default=, MODIFIER=MappingModifier \
           KEYBOARD=MappingKeyboard POINTER=MappingPointer)
#define make_mapping_request get_mapping_request_reverse
DEFCHECKER(get_crossing_mode,default=, NORMAL=NotifyNormal GRAB=NotifyGrab \
           UNGRAB=NotifyUngrab WHILE-GRABBED=NotifyWhileGrabbed)
#define make_crossing_mode get_crossing_mode_reverse
/* NIM: :while-grabbed */
DEFCHECKER(get_crossing_kind,default=, ANCESTOR=NotifyAncestor          \
           VIRTUAL=NotifyVirtual INFERIOR=NotifyInferior                \
           NONLINEAR=NotifyNonlinear NONLINEAR-VIRTUAL=NotifyNonlinearVirtual \
           POINTER=NotifyPointer POINTER-ROOT=NotifyPointerRoot         \
           NONE=NotifyDetailNone)
#define make_crossing_kind get_crossing_kind_reverse
/* NIM: :pointer, :pointer-root, :none */
DEFCHECKER(get_focus_mode,default=,NORMAL=NotifyNormal GRAB=NotifyGrab \
           UNGRAB=NotifyUngrab WHILE-GRABBED=NotifyWhileGrabbed)
#define make_focus_mode get_focus_mode_reverse
/* This seems to be the same as crossing_mode, but added the
   :while-grabbed in CLXM Have to justify that by looking into the
   source.  I was complaining 'Strange -- the CLX manual says also
   somthing about :while-grabbed!'  Maybe libX and CLX differ here?  */
DEFCHECKER(get_focus_detail,default=,ANCESTOR=NotifyAncestor            \
           VIRTUAL=NotifyVirtual INFERIOR=NotifyInferior                \
           NONLINEAR=NotifyNonlinear NONLINEAR-VIRTUAL=NotifyNonlinearVirtual \
           POINTER=NotifyPointer POINTER-ROOT=NotifyPointerRoot         \
           NONE=NotifyDetailNone)
#define make_focus_detail get_focus_detail_reverse
/* This seems also to be the same as crossing_kind! */

DEFCHECKER(get_V_class,default=,STATIC-GRAY=StaticGray GRAY-SCALE=GrayScale \
           STATIC-COLOR=StaticColor PSEUDO-COLOR=PseudoColor \
           TRUE-COLOR=TrueColor DIRECT-COLOR=DirectColor)
#define make_V_class get_V_class_reverse
DEFCHECKER(get_backing_store,default=,\
           NOT-USEFUL=NotUseful WHEN-MAPPED=WhenMapped ALWAYS=Always)
#define make_backing_store get_backing_store_reverse
DEFCHECKER(get_switch,default=, OFF=LedModeOff ON=LedModeOn)
#define make_switch get_switch_reverse
DEFCHECKER(get_close_down_mode,default=, DESTROY=DestroyAll \
           RETAIN-PERMANENT=RetainPermanent RETAIN-TEMPORARY=RetainTemporary)
DEFCHECKER(get_draw_direction,default=, \
           LEFT-TO-RIGHT=FontLeftToRight RIGHT-TO-LEFT=FontRightToLeft)
#define make_draw_direction get_draw_direction_reverse

static Bool get_generic_switch (object o)
{ return ! (eq (o, `:NO`) || eq (o, `:OFF`) || nullp (o)); }

#define make_generic_switch make_bool

#define BOOLEEQ(obj,bo) (eq (obj,bo) || eq (obj, Symbol_value (bo)))

static object make_gc_function (int i)
{ /* This Symbol_value thing here is somewhat silly */
  switch (i) {
    case GXclear:        return Symbol_value (`BOOLE-CLR`);   /* 0 */
    case GXand:          return Symbol_value (`BOOLE-AND`);   /* src AND dst */
    case GXandReverse:   return Symbol_value (`BOOLE-ANDC2`); /* src AND NOT dst */
    case GXcopy:         return Symbol_value (`BOOLE-1`);     /* src */
    case GXandInverted:  return Symbol_value (`BOOLE-ANDC1`); /* (NOT src) AND dst */
    case GXnoop:         return Symbol_value (`BOOLE-2`);     /* dst */
    case GXxor:          return Symbol_value (`BOOLE-XOR`);   /* src XOR dst */
    case GXor:           return Symbol_value (`BOOLE-IOR`);   /* src OR dst */
    case GXnor:          return Symbol_value (`BOOLE-NOR`);   /* (NOT src) AND (NOT dst) */
    case GXequiv:        return Symbol_value (`BOOLE-EQV`);   /* (NOT src) XOR dst */
    case GXinvert:       return Symbol_value (`BOOLE-C2`);    /* NOT dst */
    case GXorReverse:    return Symbol_value (`BOOLE-ORC2`);  /* src OR (NOT dst) */
    case GXcopyInverted: return Symbol_value (`BOOLE-C1`);    /* NOT src */
    case GXorInverted:   return Symbol_value (`BOOLE-ORC1`);  /* (NOT src) OR dst */
    case GXnand:         return Symbol_value (`BOOLE-NAND`);  /* (NOT src) OR (NOT dst) */
    case GXset:          return Symbol_value (`BOOLE-SET`);   /* 1 */
    default:
      my_type_error(`XLIB::GC-FUNCTION`,fixnum(i));
  }
}

static int get_gc_function (object obj)
{ /* I hope this translations are right -- could somebody please verify?! */
  if (BOOLEEQ (obj, `BOOLE-CLR`))   return GXclear; /* 0 */
  if (BOOLEEQ (obj, `BOOLE-AND`))   return GXand;   /* src AND dst */
  if (BOOLEEQ (obj, `BOOLE-ANDC2`)) return GXandReverse; /* src AND NOT dst */
  if (BOOLEEQ (obj, `BOOLE-1`))     return GXcopy;       /* src */
  if (BOOLEEQ (obj, `BOOLE-ANDC1`)) return GXandInverted; /* (NOT src) AND dst */
  if (BOOLEEQ (obj, `BOOLE-2`))     return GXnoop;        /* dst */
  if (BOOLEEQ (obj, `BOOLE-XOR`))   return GXxor; /* src XOR dst */
  if (BOOLEEQ (obj, `BOOLE-IOR`))   return GXor;  /* src OR dst */
  if (BOOLEEQ (obj, `BOOLE-NOR`))   return GXnor; /* (NOT src) AND (NOT dst) */
  if (BOOLEEQ (obj, `BOOLE-EQV`))   return GXequiv; /* (NOT src) XOR dst */
  if (BOOLEEQ (obj, `BOOLE-C2`))    return GXinvert; /* NOT dst */
  if (BOOLEEQ (obj, `BOOLE-ORC2`))  return GXorReverse; /* src OR (NOT dst) */
  if (BOOLEEQ (obj, `BOOLE-C1`))    return GXcopyInverted; /* NOT src */
  if (BOOLEEQ (obj, `BOOLE-ORC1`))  return GXorInverted; /* (NOT src) OR dst */
  if (BOOLEEQ (obj, `BOOLE-NAND`))  return GXnand; /* (NOT src) OR (NOT dst) */
  if (BOOLEEQ (obj, `BOOLE-SET`))   return GXset;  /* 1 */

  my_type_error(`XLIB::GC-FUNCTION`,obj);

  /* Garnet seem to run in that ...
   ... not any longer
   return GXcopy; */
}

DEFCHECKER(get_gcontext_key,type=unsigned long,default=, ARC-MODE=GCArcMode \
           BACKGROUND=GCBackground CAP-STYLE=GCCapStyle                 \
           CLIP-MASK=GCClipMask                                         \
           CLIP-X=GCClipXOrigin CLIP-Y=GCClipYOrigin                    \
           DASH-OFFSET=GCDashOffset DASHES=GCDashList                   \
           EXPOSURES=GCGraphicsExposures                                \
           FILL-RULE=GCFillRule FILL-STYLE=GCFillStyle                  \
           FONT=GCFont                                                  \
           FOREGROUND=GCForeground                                      \
           FUNCTION=GCFunction                                          \
           JOIN-STYLE=GCJoinStyle LINE-STYLE=GCLineStyle                \
           LINE-WIDTH=GCLineWidth PLANE-MASK=GCPlaneMask                \
           STIPPLE=GCStipple                                            \
           SUBWINDOW-MODE=GCSubwindowMode                               \
           TILE=GCTile                                                  \
           TS-X=GCTileStipXOrigin TS-Y=GCTileStipXOrigin)


/* -----------------------------------------------------------------------
 *  Masks
 * ----------------------------------------------------------------------- */

DEFCHECKER(check_modifier, default=, bitmasks=both, type=unsigned int,  \
           SHIFT=ShiftMask LOCK=LockMask CONTROL=ControlMask            \
           MOD-1=Mod1Mask MOD-2=Mod2Mask MOD-3=Mod3Mask MOD-4=Mod4Mask  \
           MOD-5=Mod5Mask BUTTON-1=Button1Mask BUTTON-2=Button2Mask     \
           BUTTON-3=Button3Mask BUTTON-4=Button4Mask BUTTON-5=Button5Mask)

static unsigned int get_modifier_mask (object obj)
{
  if (!boundp(obj)) return 0;
  if (eq (obj, `:ANY`)) return AnyModifier;
  if (integerp (obj)) return get_uint16 (obj);
  if (listp(obj)) return check_modifier_from_list(obj);
  my_type_error(`(OR (EQL :ANY) XLIB::CARD16 LIST)`,obj);
}

DEFCHECKER(check_event_mask,default=, bitmasks=both, type=unsigned long, \
           KEY-PRESS=KeyPressMask                                       \
           KEY-RELEASE=KeyReleaseMask                                   \
           BUTTON-PRESS=ButtonPressMask                                 \
           BUTTON-RELEASE=ButtonReleaseMask                             \
           ENTER-WINDOW=EnterWindowMask                                 \
           LEAVE-WINDOW=LeaveWindowMask                                 \
           POINTER-MOTION=PointerMotionMask                             \
           POINTER-MOTION-HINT=PointerMotionHintMask                    \
           BUTTON-1-MOTION=Button1MotionMask                            \
           BUTTON-2-MOTION=Button2MotionMask                            \
           BUTTON-3-MOTION=Button3MotionMask                            \
           BUTTON-4-MOTION=Button4MotionMask                            \
           BUTTON-5-MOTION=Button5MotionMask                            \
           BUTTON-MOTION=ButtonMotionMask                               \
           KEYMAP-STATE=KeymapStateMask                                 \
           EXPOSURE=ExposureMask                                        \
           VISIBILITY-CHANGE=VisibilityChangeMask                       \
           STRUCTURE-NOTIFY=StructureNotifyMask                         \
           RESIZE-REDIRECT=ResizeRedirectMask                           \
           SUBSTRUCTURE-NOTIFY=SubstructureNotifyMask                   \
           SUBSTRUCTURE-REDIRECT=SubstructureRedirectMask               \
           FOCUS-CHANGE=FocusChangeMask PROPERTY-CHANGE=PropertyChangeMask \
           COLORMAP-CHANGE=ColormapChangeMask                           \
           OWNER-GRAB-BUTTON=OwnerGrabButtonMask)
static unsigned long get_event_mask (object obj)
{ /* get_event_mask could handle a numerical and symbolic
   representation of an event mask */
  if (uint32_p (obj)) return get_uint32 (obj);
  if (listp (obj)) return check_event_mask_from_list(obj);
  my_type_error(`(OR XLIB::CARD32 LIST)`,obj);
}

static object make_event_mask (unsigned long mask)
{ return make_uint32 (mask); }


/* -----------------------------------------------------------------------
 *  Various other types
 * ----------------------------------------------------------------------- */

static object make_xatom (Display *dpy, Atom atom)
{
  char *atom_name;
  X_CALL(atom_name = XGetAtomName (dpy, atom));
  value1 = intern_keyword(asciz_to_string(atom_name,GLO(misc_encoding)));
  X_CALL(XFree(atom_name));
  return value1;
}

static Time get_timestamp (object obj)
{ return missingp(obj) ? CurrentTime : get_uint32 (obj); }

static sint32 get_angle (object ang)
{ /* translates the CLX angle representation in radian to X represent
   in sixty-fourth of degree: (round (* (/ ang pi) (* 180 64))) */
  pushSTACK(ang);
  pushSTACK(GLO(FF_pi));
  funcall (L(durch), 2);
  pushSTACK(value1);
  pushSTACK(fixnum(180*64));
  funcall (L(mal), 2);
  return get_sint32(funcall1(L(round),value1));
}

static object make_key_vector (char key_vector[32])
{
  value1= allocate_bit_vector (Atype_Bit, 256);
  X_CALL(memcpy (TheSbvector(value1)->data, key_vector, 32));
  return value1;
}

static void get_key_vector (object obj, char key_vector [32])
{
  NOTIMPLEMENTED;
}

static object make_visual_info (Visual *vis)
{
  pushSTACK(`(XLIB::VISUAL-INFO)`); pushSTACK(fixnum(8));
  funcall(L(make_structure),2); pushSTACK(value1);
  TheStructure(STACK_0)->recdata[1] = make_uint29 (vis->visualid); /* id */
 #ifdef __cplusplus
  TheStructure(STACK_0)->recdata[2] = make_V_class (vis->c_class); /* class */
 #else
  TheStructure(STACK_0)->recdata[2] = make_V_class (vis->class); /* class */
 #endif
  TheStructure(STACK_0)->recdata[3] = make_pixel (vis->red_mask); /* red-mask */
  TheStructure(STACK_0)->recdata[4] = make_pixel (vis->green_mask); /* green-mask */
  TheStructure(STACK_0)->recdata[5] = make_pixel (vis->blue_mask); /* blue-mask */
  TheStructure(STACK_0)->recdata[6] = make_uint8 (vis->bits_per_rgb); /* bits-per-rgb */
  TheStructure(STACK_0)->recdata[7] = make_uint16 (vis->map_entries); /* colormap-entries */
  return popSTACK();
}

static object make_rgb_val (unsigned short value)
{ /* calculate (/ value 65535.0)
   FIXME -- should find more clever way to do this ... */
  pushSTACK(fixnum(value));
  pushSTACK(fixnum(65535));
  funcall (L(durch), 2);
  return value1;
}

static unsigned short get_rgb_val (object value)
{ /* calculate (round (* value 65535))
   FIXME -- should really find more clever way to do this ...
         -- maybe we check the actual type here?! */
  pushSTACK(value);
  pushSTACK(fixnum(0xFFFF));
  funcall (L(mal), 2);
  return get_uint16(funcall1(L(round),value1));
}

static void get_color (Display *dpy, object color, XColor *result)
{
  pushSTACK(color);
  ENSURE_TYPE (STACK_0, color_p (STACK_0), `XLIB::COLOR`);

  result->pixel = 0;
  result->flags = -1;          /* Well set all flags .. just in case; */
  /* in the .h files is something like do_{red,green,blue} ?! */
  result->red   = get_rgb_val (TheStructure (STACK_0)->recdata[1]);
  result->green = get_rgb_val (TheStructure (STACK_0)->recdata[2]);
  result->blue  = get_rgb_val (TheStructure (STACK_0)->recdata[3]);
  skipSTACK(1);
}

static object make_color (XColor *color)
{
  pushSTACK(`(XLIB::COLOR)`); pushSTACK(fixnum(4));
  funcall(L(make_structure),2); pushSTACK(value1);
  TheStructure(STACK_0)->recdata[1] = make_rgb_val (color->red); /* red */
  TheStructure(STACK_0)->recdata[2] = make_rgb_val (color->green); /* green */
  TheStructure(STACK_0)->recdata[3] = make_rgb_val (color->blue); /* blue */
  return popSTACK();
}

/* general_plist_reader (type) -- used by the various xxx-plist functions
 > type the type the argument should have
 > STACK_0 the object in question */
static void general_plist_reader (object type)
{ /* the XLIB object in question is already on the stack */
  if (typep_classname (STACK_0, type)) {
    pushSTACK(`XLIB::PLIST`);
    funcall(L(slot_value), 2);
  } else my_type_error(type,STACK_0);
}

/* general_plist_writer (type) -- used by the various xxx-plist functions
  > type the type the argument should have
  > STACK_1 the object in question
  > STACK_0 the new value for plist */
static void general_plist_writer (object type)
{ /* the XLIB object and the new value are already on the stack */
  if (typep_classname (STACK_1, type)) {
    object new_value = popSTACK();
    pushSTACK(`XLIB::PLIST`);                   /* the slot */
    pushSTACK(new_value);                       /* new value */
    funcall (L(set_slot_value), 3);
  } else my_type_error(type,STACK_0);
}

static void general_lookup (object type)
{
  XID xid = get_uint29 (STACK_0);
  if (!ensure_living_display (&(STACK_1)))
    closed_display_error(TheSubr(subr_self)->name,STACK_1);
  VALUES1(make_xid_obj_2 (type, STACK_1, xid, NIL));
  skipSTACK(2);
}

/* in STANDARD_XID_OBJECT_LOOK & STANDARD_PTR_OBJECT_LOOK the L argument
 is just the upcase of c, CPP cannot do that and we will defer resolving
 this until util/ccmp2c.c is merged into util/modprep.lisp */

/* Defines xxx-{DISPLAY,PLIST,SET-PLIST,P,EQUAL,ID}
 and LOOKUP-xxx for xid objects */
##define STANDARD_XID_OBJECT_LOOK(L,c)                                  \
  DEFUN(XLIB:##L##-DISPLAY,xxx)                                         \
    { VALUES1(get_display_obj_tc(`XLIB::##L`, popSTACK())); }           \
  DEFUN(XLIB:##L##-PLIST,xxx)                                           \
    { general_plist_reader (`XLIB::##L`); }                             \
  DEFUN(XLIB:SET-##L##-PLIST,xxx plist)                                 \
    { general_plist_writer (`XLIB::##L`); }                             \
  DEFUN(XLIB:##L##-P,xxx)                                               \
    { VALUES_IF(c##_p (popSTACK())); }                                  \
  DEFUN(XLIB:##L##-ID,xxx)                                              \
    { VALUES1(make_uint29((XID)get_##c (popSTACK()))); }                \
  DEFUN(XLIB:##L##-EQUAL,xxx yyy)                                       \
    { VALUES_IF(get_##c (popSTACK()) == get_##c (popSTACK())); }        \
  DEFUN(XLIB:LOOKUP-##L,display xxx)                                    \
    { general_lookup (`XLIB::##L##`); }

/* Defines xxx-{DISPLAY,PLIST,SET-PLIST,P,EQUAL}
 However xxx-ID and LOOKUP-xxx are not defined, since the way to
 get the xid and looking it up differs between ptr objects */
##define STANDARD_PTR_OBJECT_LOOK(L,c)                                  \
  DEFUN(XLIB:##L##-DISPLAY,display)                                     \
    { VALUES1(get_display_obj_tc(`XLIB::##L`, popSTACK())); }           \
  DEFUN(XLIB:##L##-PLIST,xxx)                                           \
    { general_plist_reader (`XLIB::##L`); }                             \
  DEFUN(XLIB:SET-##L##-PLIST,xxx plist)                                 \
    { general_plist_writer (`XLIB::##L`); }                             \
  DEFUN(XLIB:##L##-P,xxx)                                               \
    { VALUES_IF(c##_p (popSTACK())); }                                  \
  DEFUN(XLIB:##L##-EQUAL,xxx yyy)                                       \
    { VALUES_IF(get_##c (popSTACK()) == get_##c (popSTACK())); }


/* -----------------------------------------------------------------------
 *  Chapter 1   Data Types
 * ----------------------------------------------------------------------- */

STANDARD_XID_OBJECT_LOOK(WINDOW,window)
STANDARD_XID_OBJECT_LOOK(PIXMAP,pixmap)
STANDARD_XID_OBJECT_LOOK(DRAWABLE,drawable)
STANDARD_XID_OBJECT_LOOK(FONT,font)
STANDARD_XID_OBJECT_LOOK(COLORMAP,colormap)
STANDARD_XID_OBJECT_LOOK(CURSOR,cursor)

STANDARD_PTR_OBJECT_LOOK(GCONTEXT,gcontext)

DEFUN(XLIB:MAKE-EVENT-KEYS, event)
{ VALUES1(check_event_mask_to_list(get_uint32(popSTACK()))); }

DEFUN(XLIB:MAKE-EVENT-MASK,&rest keys)
{
  unsigned long mask = 0;
  while (argcount--) mask |= check_event_mask(popSTACK());
  VALUES1(make_uint32(mask));
}

DEFUN(XLIB:MAKE-STATE-KEYS, event)
{ VALUES1(check_modifier_to_list(get_uint16(popSTACK()))); }

DEFUN(XLIB:MAKE-STATE-MASK, &rest args)
{
  unsigned int mask = 0;
  while (argcount--) mask |= check_modifier(popSTACK());
  VALUES1(make_uint16(mask));
}


/* -----------------------------------------------------------------------
 *  Chapter 2   Displays
 * ----------------------------------------------------------------------- */

int xlib_error_handler (Display*, XErrorEvent*);
int xlib_io_error_handler (Display*);
static Display *x_open_display (char* display_name, int display_number) {
  Display *dpy;

  /* On one hand fetching the DISPLAY variable if in doubt is a nice
   feature -- on the other hand does it conform to the CLX documentation? */
  if (!display_name)
    X_CALL(display_name = getenv ("DISPLAY"));

  if (!display_name) { /* Which display should we open?! */
    pushSTACK(TheSubr(subr_self)->name); /* function name */
    fehler (error, ("~S: Do not know which display to open."));
  }

  {
    int len = asciz_length (display_name);
    DYNAMIC_ARRAY (cname, char, len + 5);

    begin_x_call();
    /* install the error handlers before XOpenDisplay to catch errors there */
    XSetErrorHandler (xlib_error_handler);
    XSetIOErrorHandler (xlib_io_error_handler);
    if (strchr(display_name,':'))
      strcpy(cname, display_name);
    else
      sprintf(cname, "%s:%d",display_name,display_number);
    dpy = XOpenDisplay (cname);
    end_x_call();

    if (!dpy) {
      pushSTACK(asciz_to_string(cname,GLO(misc_encoding))); /* display name */
      pushSTACK(TheSubr(subr_self)->name); /* function name */
      fehler (error, ("~S: Cannot open display ~S.")); /* raise error */
    }

    FREE_DYNAMIC_ARRAY (cname);
  }
  return dpy;
}

DEFUN(XLIB:OPEN-DISPLAY, &rest args)
{ /* (XLIB:OPEN-DISPLAY host &key :display &allow-other-keys) */
  char *display_name = NULL;    /* the host to connect to */
  int  display_number = 0;      /* the display number */
  Display *dpy;
  gcv_object_t *display_arg = NULL;

  if (argcount % 2 != 1) fehler_key_odd(argcount,TheSubr(subr_self)->name);

  if (argcount > 0) {
    pushSTACK(STACK_(argcount-1)); /* the first argument */
    if (!nullp(STACK_0) && !stringp(STACK_0))
      my_type_error(`(OR NULL STRING)`,STACK_0);
    else display_arg = &STACK_(argcount);
    skipSTACK(1);
  }

  { /* Fetch an optional :DISPLAY argument */
    uintC i;
    for (i = 1; i < argcount ; i += 2)
      if (eq (STACK_(i), `:DISPLAY`)) {
        /* keyword found; value is in STACK_(i-1) */
        display_number = get_uint8 (STACK_(i-1));
        break;
      }
  }

  if (display_arg) {
    with_string_0(*display_arg,GLO(misc_encoding),displayz,
                  { dpy = x_open_display(displayz,display_number); });
  } else dpy = x_open_display(NULL,display_number);

# if !defined(RELY_ON_WRITING_TO_SUBPROCESS)
  disable_sigpipe();
# endif

  VALUES1(make_display(dpy));
  skipSTACK(argcount);
}

DEFUN(XLIB:DISPLAY-AUTHORIZATION-DATA, display) /* OK */
{
  skipSTACK(1);
  VALUES1(allocate_string(0));
}

DEFUN(XLIB:DISPLAY-AUTHORIZATION-NAME, display) /* OK */
{
  skipSTACK(1);
  VALUES1(allocate_string(0));
}

DEFUN(XLIB:DISPLAY-BITMAP-FORMAT, display) /* OK */
{
  Display *dpy = pop_display ();

  pushSTACK(`(XLIB::BITMAP-FORMAT)`); pushSTACK(fixnum(4));
  funcall(L(make_structure),2); pushSTACK(value1);
  TheStructure (STACK_0)->recdata[1] = fixnum(BitmapUnit(dpy)); /* unit slot */
  TheStructure (STACK_0)->recdata[2] = fixnum(BitmapPad (dpy)); /* pad slot */
  TheStructure (STACK_0)->recdata[3] =
    (BitmapBitOrder (dpy) == LSBFirst ? T : NIL); /* lsb-first-p slot */
  VALUES1(popSTACK());
}

DEFUN(XLIB:DISPLAY-BYTE-ORDER, display) /* OK */
{
  skipSTACK(1);
  /* To my knowlegde the libX11 opens the display in the local byte sex ... */
  VALUES1((BIG_ENDIAN_P) ? `:MSBFIRST` : `:LSBFIRST`);
}

DEFUN(XLIB:DISPLAY-DISPLAY, display)
{ /* What should this function return?!
     From manual: Returns the /display-number/ for the host
     associated with /display/.
     Not very informative, is it? */
  skipSTACK(1);
  VALUES1(fixnum(0));
}

DEFUN(XLIB:DISPLAY-ERROR-HANDLER, display) /* OK */
{
  ensure_living_display (&(STACK_0));
  VALUES1(TheStructure (STACK_0)->recdata[slot_DISPLAY_ERROR_HANDLER]);
  skipSTACK(1);
}

DEFUN(XLIB:SET-DISPLAY-ERROR-HANDLER, display handler) /* OK */
{
  ensure_living_display (&(STACK_1));
  VALUES1(TheStructure(STACK_1)->recdata[slot_DISPLAY_ERROR_HANDLER]=STACK_0);
  skipSTACK(2);
}

DEFUN(XLIB:DISPLAY-IMAGE-LSB-FIRST-P, display) /* OK */
{
  VALUES_IF(ImageByteOrder (pop_display ()) == LSBFirst);
}

DEFUN(XLIB:DISPLAY-KEYCODE-RANGE, display) /* OK */
{
  int max_kc, min_kc;
  Display *dpy = pop_display ();
  X_CALL(XDisplayKeycodes (dpy, &min_kc, &max_kc));
  VALUES2(fixnum(min_kc),fixnum(max_kc));
}

DEFUN(XLIB:DISPLAY-MAX-KEYCODE, display) /* OK */
{
  funcall(``XLIB:DISPLAY-KEYCODE-RANGE``,1);
  value1 = value2; mv_count = 1;
}

DEFUN(XLIB:DISPLAY-MAX-REQUEST-LENGTH, display) /* OK */
{
  Display *dpy = pop_display ();
  long n;
  X_CALL(n = XMaxRequestSize (dpy));
  VALUES1(make_uint32(n));
}

DEFUN(XLIB:DISPLAY-MIN-KEYCODE, display) /* OK */
{
  funcall(``XLIB:DISPLAY-KEYCODE-RANGE``,1);
  mv_count = 1;
}

DEFUN(XLIB:DISPLAY-MOTION-BUFFER-SIZE, display) /* OK */
{
  Display *dpy = pop_display ();
  unsigned long n;
  X_CALL(n = XDisplayMotionBufferSize (dpy));
  VALUES1(make_uint32(n));      /* remeber pop_display pops */
}

DEFUN(XLIB:DISPLAY-P, display)        /* OK */
{ VALUES_IF(display_p (popSTACK())); }

DEFUN(XLIB:DISPLAY-PIXMAP-FORMATS, display) /* OK */
{
  int cnt = 0;
  int i;
  Display *dpy = pop_display ();
  XPixmapFormatValues *formats;

  X_CALL(formats = XListPixmapFormats (dpy, &cnt));
  for (i = 0; i < cnt; i++) {
    pushSTACK(`(XLIB::PIXMAP-FORMAT)`); pushSTACK(fixnum(4));
    funcall(L(make_structure),2); pushSTACK(value1);
    TheStructure(STACK_0)->recdata[1] = fixnum(formats[i].depth);
    TheStructure(STACK_0)->recdata[2] = fixnum(formats[i].bits_per_pixel);
    TheStructure(STACK_0)->recdata[3] = fixnum(formats[i].scanline_pad);
  }
  if (formats)
    X_CALL(XFree (formats));
  VALUES1(listof(cnt));
}

DEFUN(XLIB:DISPLAY-PROTOCOL-MAJOR-VERSION, display) /* OK */
{ VALUES1(fixnum(ProtocolVersion (pop_display()))); }

DEFUN(XLIB:DISPLAY-PROTOCOL-MINOR-VERSION, display) /* OK */
{ VALUES1(fixnum(ProtocolRevision(pop_display()))); }

DEFUN(XLIB:DISPLAY-PROTOCOL-VERSION, display) /* OK */
{
  Display *dpy = pop_display ();
  VALUES2(fixnum(ProtocolVersion(dpy)),
          fixnum(ProtocolRevision(dpy)));
}

DEFUN(XLIB:DISPLAY-RESOURCE-ID-BASE, display)
{UNDEFINED;} /* ??? */
DEFUN(XLIB:DISPLAY-RESOURCE-ID-MASK, display)
{UNDEFINED;} /* ??? */

DEFUN(XLIB:DISPLAY-ROOTS, display)    /* OK */
{
  Display *dpy;
  int i;
  int cnt;

  pushSTACK(STACK_0);
  dpy = pop_display ();         /* retrieve display pointer */
  cnt = ScreenCount (dpy);      /* number of screens */

  for (i = 0; i < cnt; i++)     /* thru all screens */
    pushSTACK(make_screen (STACK_(i), ScreenOfDisplay (dpy, i)));

  VALUES1(listof(cnt));         /* cons`em together */
  skipSTACK(1);                 /* cleanup and all done */
 }

DEFUN(XLIB:DISPLAY-VENDOR, display)   /* OK */
{
  Display *dpy = pop_display ();
  char *s = ServerVendor (dpy);
  pushSTACK(asciz_to_string (s, GLO(misc_encoding)));
  pushSTACK(make_uint32 (VendorRelease (dpy)));
  value2 = popSTACK();
  value1 = popSTACK();
  mv_count = 2;
}

DEFUN(XLIB:DISPLAY-VENDOR-NAME, display) /* OK */
{ funcall(``XLIB:DISPLAY-VENDOR``,1); }

DEFUN(XLIB:DISPLAY-RELEASE-NUMBER, display) /* OK */
{
  funcall(``XLIB:DISPLAY-VENDOR``,1);
  value1 = value2; mv_count = 1;
}

DEFUN(XLIB::%DISPLAY-XID, display)
{
  Display *dpy = pop_display ();
  XID xid; X_CALL(xid = XAllocID(dpy));
  VALUES1(make_uint29(xid));
}

DEFUN(XLIB:DISPLAY-XID, display)
{ /* This functions returns a function to allocate new resource id's */
  pop_display();
  VALUES1(``XLIB::%DISPLAY-XID``);
}

DEFUN(XLIB::LOOKUP-RESOURCE-ID, display id) { /* used by RGB-COLORMAPS */
  XID resource_id = get_uint29(popSTACK());
  object ht = lookup_xid(popSTACK(),resource_id); /* set value1 if found */
  if (!eq(ht,nullobj)) VALUES1(NIL); /* not found */
}

DEFUN(XLIB::SAVE-ID, display id resource) { /* used by CLUE */
  XID resource_id = get_uint29(STACK_1);
  STACK_2 = display_hash_table(STACK_2);
  set_resource_id(&STACK_2,resource_id,&STACK_0);
  VALUES1(STACK_0);
  skipSTACK(3);
}

DEFUN(XLIB::DEALLOCATE-RESOURCE-ID, display id type) { /* used by CLUE */
  XID resource_id = get_uint29(STACK_1);
  STACK_2 = display_hash_table(STACK_2);
  delete_resource_id(&STACK_2,resource_id); /* sets values */
  skipSTACK(3);
}

DEFUN(XLIB::SET-GCONTEXT-DISPLAY, display gcontext) { /* used by CLUE */
  Display *dpy_orig, *dpy_new;
  GC gcon = get_gcontext_and_display(STACK_0,&dpy_orig);
  pushSTACK(STACK_1); dpy_new = pop_display();
  if (dpy_orig != dpy_new) {
    pushSTACK(allocate_fpointer(dpy_orig));
    pushSTACK(allocate_fpointer(dpy_new));
    pushSTACK(STACK_3)/*gc*/; pushSTACK(STACK_3)/* dpy */;
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,"~S: cannot change dpy of ~S to ~S (~S is not ~S)");
  }
  pushSTACK(STACK_0);           /* GC */
  pushSTACK(`XLIB::DISPLAY`);   /* slot */
  pushSTACK(STACK_3);           /* dpy */
  funcall(L(set_slot_value),3);
  skipSTACK(2);
}

DEFUN(XLIB:DISPLAY-AFTER-FUNCTION, display) /* OK */
{
  ensure_living_display (&(STACK_0));
  VALUES1(TheStructure (STACK_0)->recdata[slot_DISPLAY_AFTER_FUNCTION]);
  skipSTACK(1);
}

int xlib_after_function (Display *display);
DEFUN(XLIB:SET-DISPLAY-AFTER-FUNCTION, display after-function) /* OK */
{ /* TODO - check for function type [Not very important since the
   xlib_after_function should get this error.] */
  object display = STACK_1;
  Display *dpy = (pushSTACK(display),pop_display());
  TheStructure (display)->recdata[slot_DISPLAY_AFTER_FUNCTION] = STACK_0;
  if (nullp (STACK_0)) {
    X_CALL(XSetAfterFunction (dpy, NULL)); /* Q: Is that right?! */
  } else {
    X_CALL(XSetAfterFunction (dpy, xlib_after_function));
  }
  VALUES1(STACK_0);
  skipSTACK(2);
}

DEFUN(XLIB:DISPLAY-FORCE-OUTPUT, display) /* OK */
{
  Display *dpy = pop_display ();
  X_CALL(XFlush(dpy));
  VALUES1(NIL);
}

DEFUN(XLIB:DISPLAY-FINISH-OUTPUT, display) /* OK */
{
  Display *dpy = pop_display ();
  X_CALL(XSync (dpy, 0));
  VALUES1(NIL);
}

DEFUN(XLIB:CLOSE-DISPLAY, display &key ABORT) /* OK */
{ /* We can do nothing meaningful with the :abort option ... or could we? */

  /* if abort is NIL sync with display and remove the display from the
   xlib::*displays* list. Destroy the hash table and to make sure that
   not one single reference to an X object hinders all other from being
   garbage collected [syncing is for fetching the errors now]
   if abort is non-NIL do sync too, but do not report errors, which
   could occur. */

  Display *dpy;
  skipSTACK(1);                                 /* the :abort option */
  pushSTACK(STACK_0);                           /* the display */
  dpy = pop_display ();
  X_CALL(XCloseDisplay (dpy));

  /* Now remove the display from the XLIB:*DISPLAYS* variable
  FIXME we should cdr-down the hash table and mark all clx object known
  as dead. */
  Symbol_value(`XLIB::*DISPLAYS*`) =
    deleteq(Symbol_value(`XLIB::*DISPLAYS*`),STACK_0);
  /* mark the display as closed */
  TheFpointer(TheStructure(STACK_0)->recdata[slot_DISPLAY_FOREIGN_POINTER])
    ->fp_pointer = NULL;
  VALUES1(popSTACK());
}

DEFUN(XLIB:DISPLAY-PLIST, display)    /* OK */
{
  ensure_living_display (&(STACK_0));
  VALUES1(TheStructure (STACK_0)->recdata[slot_DISPLAY_PLIST]);
  skipSTACK(1);
}

DEFUN(XLIB:SET-DISPLAY-PLIST, display plist) /* OK */
{
  ensure_living_display (&(STACK_1));
  VALUES1(TheStructure (STACK_1)->recdata[slot_DISPLAY_PLIST] = STACK_0);
  skipSTACK(2);
}

DEFUN(XLIB:DISPLAY-DEFAULT-SCREEN, display) /* NIM / OK */
{
  Display *dpy = (pushSTACK(STACK_0),pop_display());
  VALUES1(make_screen(STACK_0,DefaultScreenOfDisplay(dpy)));
  skipSTACK(1);
}

DEFUN(XLIB:DISPLAY-NSCREENS, display) /* NIM */
{ VALUES1(fixnum(ScreenCount(pop_display()))); }

DEFUN(XLIB:DISPLAY-INVOKE-AFTER-FUNCTION, display)
{ /* XXX This function does not work at all -- it pushes the hash table
      instead of the after function, but why?  This one seems simply to
      be a a hook to call the after_function
  (funcall (display-after-function dpy) dpy) */

  pushSTACK(STACK_0);
  funcall(`XLIB::DISPLAY-AFTER-FUNCTION`,1);
  funcall(value1,1);
}

DEFUN(XLIB:DISPLAY-HOST, display)
{
  char *name = DisplayString (pop_display ());
  char *s;

  /* Hunt the ':' */
  for (s = name; *s && *s!=':'; s++)
    continue;

  VALUES1(s == name ? ascii_to_string("localhost")
          : n_char_to_string(name, s - name, GLO(misc_encoding)));
}

DEFUN(XLIB:DISPLAY-REPORT-ASYNCHRONOUS-ERRORS, display)
{ /* This function is not actually specified in the CLX refman, but the
      source code says something about it:

      (def-clx-class (display (:include buffer) ..)
       :
         (report-asynchronous-errors            ; When to report asynchronous errors
            `(:immediately) :type list) ; The keywords that can be on this list
         :                                      ; are :IMMEDIATELY, :BEFORE-EVENT-HANDLING,
          )                                     ; and :AFTER-FINISH-OUTPUT
 */
  skipSTACK(1);
  VALUES1(`(:IMMEDIATELY)`);    /* Well, em ... fake it! */
}

DEFUN(XLIB:SET-DISPLAY-REPORT-ASYNCHRONOUS-ERRORS, display value)
{
  VALUES1(STACK_0);
  skipSTACK(2);
}

DEFUN(XLIB:DISPLAY-TRACE, &rest args)
{ /* I do not think I will support this function, since
        - tracing seems not to be possible using the libX11
        - It may even not be wanted by anybody...?!
      BTW in the source of MIT-CLX (trace.lisp) I found a mark
      that display-trace is an obsolete name. */
  UNDEFINED;
}


/* -----------------------------------------------------------------------
 * Chapter 3   Screens
 * ----------------------------------------------------------------------- */

##define DEF_SCREEN_PROP(lspnam, typ, cnam)                     \
     DEFUN(lspnam, screen) {                                    \
       VALUES1(make_##typ(cnam(get_screen(popSTACK()))));       \
     }

DEF_SCREEN_PROP(XLIB:SCREEN-BLACK-PIXEL,          uint32,   BlackPixelOfScreen)
DEF_SCREEN_PROP(XLIB:SCREEN-WHITE-PIXEL,          uint32,   WhitePixelOfScreen)
DEF_SCREEN_PROP(XLIB:SCREEN-EVENT-MASK-AT-OPEN,   uint32,   EventMaskOfScreen)
DEF_SCREEN_PROP(XLIB:SCREEN-HEIGHT,               sint16,   HeightOfScreen)
DEF_SCREEN_PROP(XLIB:SCREEN-HEIGHT-IN-MILLIMETERS,sint16,   HeightMMOfScreen)
DEF_SCREEN_PROP(XLIB:SCREEN-WIDTH,                sint16,   WidthOfScreen)
DEF_SCREEN_PROP(XLIB:SCREEN-WIDTH-IN-MILLIMETERS, sint16,   WidthMMOfScreen)
DEF_SCREEN_PROP(XLIB:SCREEN-MAX-INSTALLED-MAPS,   uint16,   MaxCmapsOfScreen)
DEF_SCREEN_PROP(XLIB:SCREEN-MIN-INSTALLED-MAPS,   uint16,   MinCmapsOfScreen)
DEF_SCREEN_PROP(XLIB:SCREEN-ROOT-DEPTH,           uint16, DefaultDepthOfScreen)
DEF_SCREEN_PROP(XLIB:SCREEN-ROOT-VISUAL,          visual,DefaultVisualOfScreen)
DEF_SCREEN_PROP(XLIB:SCREEN-ROOT-VISUAL-INFO,     visual_info,DefaultVisualOfScreen) /* NIM */
DEF_SCREEN_PROP(XLIB:SCREEN-SAVE-UNDERS-P,        bool,       DoesSaveUnders)


DEFUN(XLIB:SCREEN-BACKING-STORES, screen) /* OK */
{
  int a = DoesBackingStore (get_screen (popSTACK()));
  VALUES1((a == NotUseful) ? `:NEVER` : /* Why :NEVER but not :NOT-USEFUL?! */
          (a == WhenMapped) ? `:WHEN-MAPPED` :
          `:ALWAYS`);
}

DEFUN(XLIB:SCREEN-DEFAULT-COLORMAP, screen) /* OK */
{
  VALUES1(make_colormap(get_display_obj(STACK_0),
                        DefaultColormapOfScreen(get_screen(STACK_0))));
  skipSTACK(1);
}

DEFUN(XLIB:SCREEN-DEPTHS, screen)
{
  Display *dpy;
  Screen *scr = get_screen_and_display (STACK_0, &dpy);
  int *depths;
  int ndepths = 0;
  int i;
  int screen_number;

  begin_x_call();
  screen_number = XScreenNo (dpy, scr);
  depths = XListDepths (dpy, screen_number, &ndepths);
  end_x_call();

  for (i = 0; i < ndepths; i++) {
    XVisualInfo templeight, *visual_infos;
    int n_visual_infos, j;

    pushSTACK(make_uint8 (depths[i]));

    /* Now enumerate the visual infos ... */
    templeight.depth = depths[i];
    n_visual_infos = 0;

    X_CALL(visual_infos = XGetVisualInfo (dpy, VisualDepthMask, &templeight,
                                          &n_visual_infos));

    if (visual_infos) {
      for (j = 0; j < n_visual_infos; j++)
        pushSTACK(make_visual_info (visual_infos[j].visual));

      X_CALL(XFree (visual_infos));
    }

    value1 = listof(n_visual_infos+1); /* cons `em up */
    pushSTACK(value1);
  }

  /* Final cons */
  VALUES1(listof(ndepths));
  if (depths)
    X_CALL(XFree (depths));

  skipSTACK(1);         /* all done */
}

DEFUN(XLIB:SCREEN-P, screen)    /* OK */
{
  VALUES_IF(screen_p (popSTACK()));
}

DEFUN(XLIB:SCREEN-PLIST, screen) /* OK */
{
  general_plist_reader (`XLIB::SCREEN`);
}

DEFUN(XLIB:SET-SCREEN-PLIST, screen plist) /* OK */
{
  general_plist_writer (`XLIB::SCREEN`);
}

DEFUN(XLIB:SCREEN-ROOT, screen) /* OK */
{
  VALUES1(make_window(get_display_obj(STACK_0),
                      RootWindowOfScreen(get_screen(STACK_0))));
  skipSTACK(1);
}

DEFUN(XLIB:VISUAL-INFO, display visual-id)      /* NIM / OK */
{
  VisualID vid;
  Display *dpy = (pushSTACK(STACK_1),pop_display());
  Visual *visual;

  vid = get_uint29 (STACK_0);
  visual = XVisualIDToVisual (dpy, vid);

  if (visual) {
    VALUES1(make_visual_info (visual));
    skipSTACK(2);
  } else {
    pushSTACK(STACK_1); /* display argument */
    pushSTACK(STACK_1); /* visual id argument */
    fehler (error, ("Visual info not found for id #~S in display ~S."));
  }
}

/* After all, no SCREEN-EQUAL ? */


/* -----------------------------------------------------------------------
 *  Chapter 4   Windows and Pixmaps
 * ----------------------------------------------------------------------- */

nonreturning_function(static, error_required_keywords, (object list)) {
  pushSTACK(list); pushSTACK(TheSubr(subr_self)->name);
  fehler(error,"~S: At least ~S must be specified");
}

/* 4.1 Drawables */

/* 4.2 Creating Windows  - 23 keys! */
DEFUN(XLIB:CREATE-WINDOW, &key WINDOW PARENT X Y WIDTH HEIGHT           \
      DEPTH BORDER-WIDTH CLASS VISUAL BACKGROUND BORDER BIT-GRAVITY GRAVITY \
      BACKING-STORE BACKING-PLANES BACKING-PIXEL SAVE-UNDER EVENT-MASK  \
      DO-NOT-PROPAGATE-MASK OVERRIDE-REDIRECT COLORMAP CURSOR)
{
  XSetWindowAttributes attr;
  unsigned long valuemask = 0;
  Visual *visual = CopyFromParent;
  int c_class = CopyFromParent;
  int border_width = 0;
  int depth = CopyFromParent;
  Window parent;
  int x,y,width,height;
  Display *dpy;
  Window win;

#define SLOT(ofs, type, cslot, mask)\
    if (!missingp(STACK_(ofs))) { attr.cslot = get_##type(STACK_(ofs)); valuemask |= mask; }

#if 0
  SLOT ( 0, cursor,     cursor,                 CWCursor);
  SLOT ( 1, colormap,   colormap,               CWColormap);
  SLOT ( 2, switch,     override_redirect,      CWOverrideRedirect);
  SLOT ( 3, uint32,     do_not_propagate_mask,  CWDontPropagate);
  SLOT ( 4, event_mask, event_mask,             CWEventMask);
  SLOT ( 5, switch,     save_under,             CWSaveUnder);
  SLOT ( 6, uint32,     backing_pixel,          CWBackingPixel);
  SLOT ( 7, uint32,     backing_planes,         CWBackingPlanes);
#endif

  if (!missingp(STACK_0))
    { attr.cursor = get_cursor(STACK_0); valuemask |= CWCursor; }
  if (!missingp(STACK_1))
    { attr.colormap = get_colormap (STACK_1); valuemask |= CWColormap; }

  if (!missingp(STACK_2)) { attr.override_redirect     = get_switch (STACK_2);         valuemask |= CWOverrideRedirect; }
  if (!missingp(STACK_3)) { attr.do_not_propagate_mask = get_uint32 (STACK_3);         valuemask |= CWDontPropagate; }
  if (!missingp(STACK_4)) { attr.event_mask            = get_event_mask (STACK_4);     valuemask |= CWEventMask; }
  if (!missingp(STACK_5)) { attr.save_under            = get_generic_switch (STACK_5); valuemask |= CWSaveUnder; }
  if (!missingp(STACK_6)) { attr.backing_pixel         = get_uint32 (STACK_6);         valuemask |= CWBackingPixel; }
  if (!missingp(STACK_7)) { attr.backing_planes        = get_uint32 (STACK_7);         valuemask |= CWBackingPlanes; }
  if (!missingp(STACK_8)) { attr.backing_store         = get_backing_store (STACK_8);  valuemask |= CWBackingStore; }
  if (!missingp(STACK_9)) { attr.win_gravity           = get_gravity (STACK_9);        valuemask |= CWWinGravity; }
  if (!missingp(STACK_10)) { attr.bit_gravity          = get_gravity (STACK_10);       valuemask |= CWBitGravity; }

  if (!missingp(STACK_(11))) { /* :border */
    if (eq(STACK_(11), `:COPY`)) {
      attr.border_pixmap = CopyFromParent;
      valuemask |= CWBorderPixmap;
    } else if (pixmap_p (STACK_(11))) {
      attr.border_pixmap = get_pixmap (STACK_(11));
      valuemask |= CWBorderPixmap;
    } else {
      attr.border_pixel = get_uint32 (STACK_(11));
      valuemask |= CWBorderPixel;
    }
  }

  if (!missingp(STACK_(12))) { /* :background */
    if (eq(STACK_(12), `:NONE`)) {
      attr.background_pixmap = None;
      valuemask |= CWBackPixmap;
    } else if (eq(STACK_(12), `:PARENT-RELATIVE`)) {
      attr.background_pixmap = ParentRelative;
      valuemask |= CWBackPixmap;
    } else if (pixmap_p (STACK_(12))) {
      attr.background_pixmap = get_pixmap (STACK_(12));
      valuemask |= CWBackPixmap;
    } else {
      attr.background_pixel = get_pixel (STACK_(12));
      valuemask |= CWBackPixel;
    }
  }

  if (!missingp(STACK_(14))) /* :class */
    c_class = get_W_class (STACK_(14));
  if (!missingp(STACK_(15))) /* :border-width */
    border_width = get_uint16 (STACK_(15));
  if (!missingp(STACK_(16))) /* :depth */
    depth = get_uint16 (STACK_(16));

  if (!missingp(STACK_(17)))               /* :height */ /* C */
    height = get_uint16 (STACK_(17));
  else
    goto required;

  if (!missingp(STACK_(18)))              /* :width */ /* C */
    width = get_uint16 (STACK_(18));
  else
    goto required;

  if (!missingp(STACK_(19)))          /* :y */ /* C */
    y = get_sint16 (STACK_(19));
  else
    goto required;

  if (!missingp(STACK_(20)))          /* :x */ /* C */
    x = get_sint16 (STACK_(20));
  else
    goto required;

  if (!missingp(STACK_(21))) {               /* :parent */ /* C */
    parent = get_window_and_display (STACK_(21), &dpy);
    pushSTACK(get_display_obj (STACK_(21)));
  } else
    goto required;

  if (!missingp(STACK_(13+1))) /* :visual */
    visual = get_visual (dpy, STACK_(13+1));

  if (!missingp(STACK_(23)))               /* :window */ /* C */
    pushSTACK(STACK_(23));
  else
    pushSTACK(NIL);
#undef SLOT

  X_CALL(win = XCreateWindow (dpy, parent, x,y, width,height, border_width,
                              depth, c_class, visual, valuemask, &attr));

  VALUES1(make_window_2 (STACK_1, win, STACK_0));
  skipSTACK(23 + 2);
  return;

 required:
  error_required_keywords(`(:X :Y :WIDTH :HEIGHT :PARENT)`);
}

##define DEF_DRAWABLE_GEOM_GETTER(type, lspnam, attr)                   \
     DEFUN(XLIB:DRAWABLE-##lspnam, window) {                            \
          Window root;                                                  \
          int x, y;                                                     \
          unsigned int width, height;                                   \
          unsigned int border_width;                                    \
          unsigned int depth;                                           \
          Display *dpy;                                                 \
          Drawable da = get_drawable_and_display (STACK_0, &dpy);       \
          X_CALL(XGetGeometry (dpy, da,                                 \
                               &root, &x, &y, &width, &height,          \
                               &border_width, &depth));                 \
          VALUES1(make_##type(attr));                                   \
          skipSTACK(1);                                                 \
     }

##define DEF_SET_DRAWABLE_GEOM(type, lspnam, attr, mask)        \
     DEFUN(XLIB:SET-DRAWABLE-##lspnam##, window param) {        \
          XWindowChanges values;                                \
          Window win;                                           \
          Display *dpy;                                         \
          /* Why window here vvvv and not drawable? */          \
          win = get_window_and_display (STACK_1, &dpy);         \
          values.attr = get_##type (STACK_0);                   \
          X_CALL(XConfigureWindow (dpy, win, mask, &values));   \
          VALUES1(STACK_0);                                     \
          skipSTACK(2);                                         \
     }

##define DEF_DRAWABLE_GEOM(type, lspnam, attr, mask)            \
     DEF_DRAWABLE_GEOM_GETTER (type, lspnam, attr)              \
     DEF_SET_DRAWABLE_GEOM (type, lspnam, attr, mask)

DEF_DRAWABLE_GEOM (uint16, BORDER-WIDTH, border_width, CWBorderWidth) /* OK */
DEF_DRAWABLE_GEOM_GETTER (uint8, DEPTH, depth)       /* OK */
DEF_DRAWABLE_GEOM (uint16, HEIGHT, height, CWHeight) /* OK */
DEF_DRAWABLE_GEOM (uint16, WIDTH, width, CWWidth)    /* OK */
DEF_DRAWABLE_GEOM (sint16, X, x, CWX)                /* OK */
DEF_DRAWABLE_GEOM (sint16, Y, y, CWY)                /* OK */

DEFUN(XLIB:WINDOW-ALL-EVENT-MASKS, window)
{
  XWindowAttributes attr;
  Display *dpy;
  Window win = get_xid_object_and_display (`XLIB::WINDOW`, STACK_0, &dpy);
  X_CALL(XGetWindowAttributes (dpy, win, &attr));
  VALUES1(make_event_mask (attr.all_event_masks));
  skipSTACK(1);
}

DEFUN(XLIB:SET-WINDOW-BACKGROUND, window background) /*OK*/
{
  XSetWindowAttributes attr;
  unsigned long valuemask = 0;

  if (eq (STACK_0, `:NONE`)) {
    attr.background_pixmap = None; valuemask |= CWBackPixmap;
  } else if (eq (STACK_0, `:PARENT-RELATIVE`)) {
    attr.background_pixmap = ParentRelative; valuemask |= CWBackPixmap;
  } else if (pixmap_p (STACK_0)) {
    attr.background_pixmap = get_pixmap (STACK_0); valuemask |= CWBackPixmap;
  } else if (pixel_p (STACK_0)) {
    attr.background_pixel = get_pixel (STACK_0); valuemask |= CWBackPixel;
  } else my_type_error(`(OR XLIB::PIXMAP XLIB::PIXEL (EQL :NONE) (EQL :PARENT-RELATIVE))`,STACK_0);

  {
    Display *dpy;
    Window win = get_xid_object_and_display (`XLIB::WINDOW`, STACK_1, &dpy);
    X_CALL(XChangeWindowAttributes (dpy, win, valuemask, &attr));
  }
  VALUES1(STACK_0);
  skipSTACK(2);
}

##define DEF_WIN_ATTR_READER(lspnam,typ,slotget)                        \
  DEFUN(XLIB:WINDOW-##lspnam, window) {                                 \
    XWindowAttributes attr;                                             \
    Display *dpy;                                                       \
    Window win = get_xid_object_and_display (`XLIB::WINDOW`, STACK_0, &dpy); \
    X_CALL(XGetWindowAttributes (dpy, win, &attr));                     \
    VALUES1(make_##typ (attr.slotget));                                 \
    skipSTACK(1);                                                       \
  }

##define DEF_WIN_ATTR_READER_2(lspnam,typ,slotget)                      \
      DEFUN(XLIB:WINDOW-##lspnam, window) {                             \
          XWindowAttributes attr;                                       \
          Display *dpy;                                                 \
          Window win = get_window_and_display (STACK_0, &dpy);          \
          X_CALL(XGetWindowAttributes (dpy, win, &attr));               \
          VALUES1(make_##typ (get_display_obj (STACK_0),attr.slotget)); \
          skipSTACK(1);                                         \
     }

##define DEF_WIN_ATTR_WRITER(lspnam,typ,slotset,msk)            \
     DEFUN(XLIB:SET-WINDOW-##lspnam##, window attr) {           \
          XSetWindowAttributes attr;                            \
          Display *dpy;                                         \
          Window win = get_window_and_display (STACK_1, &dpy);  \
          attr.slotset = get_##typ (STACK_0);                   \
          X_CALL(XChangeWindowAttributes (dpy, win, msk, &attr)); \
          VALUES1(STACK_0);                                     \
          skipSTACK(2);                                         \
     }

##define DEF_WIN_ATTR(lspnam, typ, slotget, slotset, msk)       \
    DEF_WIN_ATTR_READER(lspnam, typ, slotget)                   \
    DEF_WIN_ATTR_WRITER(lspnam, typ, slotset, msk)

##define DEF_WIN_ATTR_2(lspnam, typ, slotget, slotset, msk)     \
    DEF_WIN_ATTR_READER_2(lspnam, typ, slotget)                 \
    DEF_WIN_ATTR_WRITER(lspnam, typ, slotset, msk)

DEF_WIN_ATTR (BACKING-PIXEL,         uint32,        backing_pixel,     backing_pixel,     CWBackingPixel)
DEF_WIN_ATTR (BACKING-PLANES,        uint32,        backing_planes,    backing_planes,    CWBackingPlanes)
DEF_WIN_ATTR (BIT-GRAVITY,           gravity,       bit_gravity,       bit_gravity,       CWBitGravity)
DEF_WIN_ATTR (GRAVITY,               gravity,       win_gravity,       win_gravity,       CWWinGravity)
DEF_WIN_ATTR (EVENT-MASK,            event_mask,    your_event_mask,   event_mask,        CWEventMask)
DEF_WIN_ATTR (OVERRIDE-REDIRECT,     switch,        override_redirect, override_redirect, CWOverrideRedirect)
DEF_WIN_ATTR (BACKING-STORE,         backing_store, backing_store,     backing_store,     CWBackingStore)
DEF_WIN_ATTR (DO-NOT-PROPAGATE-MASK, event_mask,    do_not_propagate_mask, do_not_propagate_mask, CWDontPropagate)
DEF_WIN_ATTR (SAVE-UNDER,            generic_switch,  save_under,        save_under,        CWSaveUnder)

DEF_WIN_ATTR_2 (COLORMAP,                  colormap, colormap, colormap, CWColormap)
DEF_WIN_ATTR_WRITER (CURSOR,               cursor, cursor, CWCursor)
DEF_WIN_ATTR_READER (MAP-STATE,            map_state, map_state)
#ifdef __cplusplus
DEF_WIN_ATTR_READER (CLASS,                W_class, c_class)
#else
DEF_WIN_ATTR_READER (CLASS,                W_class, class)
#endif
DEF_WIN_ATTR_READER (COLORMAP-INSTALLED-P, bool, map_installed)
DEF_WIN_ATTR_READER (VISUAL,               visual, visual)
DEF_WIN_ATTR_READER (VISUAL-INFO,          visual_info, visual)/* NIM */

DEFUN(XLIB:WINDOW-CURSOR, window)
{
  pushSTACK(`XLIB::WINDOW-CURSOR`);
  fehler (error, ("~S can only be set"));
}

DEFUN(XLIB:SET-WINDOW-BORDER, arg1 arg2)
{
  Display *dpy;
  Window win   = get_window_and_display (STACK_1, &dpy);
  XSetWindowAttributes attr;
  unsigned long value_mask = 0;

  if (eq (STACK_0, `:COPY`)) {
    attr.border_pixmap = CopyFromParent; value_mask = CWBorderPixmap;
  } else if (pixmap_p (STACK_0)) {
    attr.border_pixmap = get_pixmap (STACK_0); value_mask = CWBorderPixmap;
  } else if (pixel_p (STACK_0)) {
    attr.border_pixel = get_pixel (STACK_0); value_mask = CWBorderPixel;
  } else my_type_error(`(OR XLIB::PIXMAP XLIB::PIXEL (EQL :COPY))`,STACK_0);

  X_CALL(XChangeWindowAttributes (dpy, win, value_mask, &attr));

  VALUES1(STACK_0);
  skipSTACK(2);                /* all done */
}

/* (setf (XLIB:SET-WINDOW-PRIORITY window &optional sibling) mode) */
DEFUN(XLIB:SET-WINDOW-PRIORITY, mode window &optional sibling)
{
  XWindowChanges changes;
  unsigned int value_mask = 0;
  Display *dpy;
  Window win   = get_window_and_display (STACK_1, &dpy);

  if (!missingp(STACK_0)) {
    changes.sibling = get_window (STACK_0); value_mask |= CWSibling;
  }
  changes.stack_mode = get_stack_mode (STACK_2); value_mask |= CWStackMode;
  X_CALL(XConfigureWindow (dpy, win, value_mask, &changes));

  VALUES1(STACK_2);
  skipSTACK(3);         /* all done */
}

/* 4.4  Stacking Order */
DEFUN(XLIB:CIRCULATE-WINDOW-DOWN, window)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  X_CALL(XCirculateSubwindowsDown (dpy, win));
  VALUES1(popSTACK());
}

DEFUN(XLIB:CIRCULATE-WINDOW-UP, window)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  X_CALL(XCirculateSubwindowsUp (dpy, win));
  VALUES1(popSTACK());
}

/* 4.5  Window Hierachy */
DEFUN(XLIB:DRAWABLE-ROOT, window)
{
  Window root;
  Drawable da;
  Display *dpy;
  int x, y;
  unsigned int width, height, border_width, depth;
  da = get_drawable_and_display (STACK_0, &dpy);
  X_CALL(XGetGeometry (dpy, da, &root, &x, &y, &width, &height,
                       &border_width, &depth));
  VALUES1(make_window (get_display_obj (STACK_0), root));
  skipSTACK(1);
}

/* can trigger GC */
static object coerce_result_type (unsigned int stack_count,
                                  gcv_object_t *result_type)
{ /* there are stack_count objects on the STACK, which will be removed
     and collected into a sequence of type *result_type */
  if (eq(*result_type,S(list)) || missingp(*result_type))
    return listof(stack_count);
  else {
    object vec = vectorof(stack_count);
    if (!eq(*result_type,S(vector))) {
      pushSTACK(vec); pushSTACK(*result_type);
      funcall(L(coerce),2);
      return value1;
    } else return vec;
  }
}

DEFUN(XLIB:QUERY-TREE, window &key RESULT-TYPE)
{
  Window win;
  Display *dpy;
  gcv_object_t *dpy_objf, *res_type = &STACK_0;
  Window root;
  Window parent;
  Window *childs;
  unsigned int nchilds, i;
  int status;

  win = get_window_and_display (STACK_1, &dpy);
  pushSTACK(get_display_obj (STACK_1));
  dpy_objf = &(STACK_0);

  X_CALL(status = XQueryTree(dpy,win,&root,&parent,&childs,&nchilds));
  if (status) {
    /* Now push all childrens */
    for (i = 0; i < nchilds; i++)
      pushSTACK(make_window (*dpy_objf, childs[i]));

    if (childs) X_CALL(XFree(childs));

    /* Now cons `em together */
    value1 = coerce_result_type(nchilds,res_type);

    pushSTACK(value1);
    pushSTACK(make_window (*dpy_objf, parent));
    pushSTACK(make_window (*dpy_objf, root));
    value3 = popSTACK();
    value2 = popSTACK();
    value1 = popSTACK();
    mv_count = 3;
  } else {
    /* Wat schall wi nu tun? */
    VALUES1(NIL);
  }

  skipSTACK(3);         /* Nu vi er færdig. */
}

DEFUN(XLIB:REPARENT-WINDOW, window1 window2 x y)
{
  Display *dpy;
  Window win  = get_window_and_display (STACK_3, &dpy);
  Window win2 = get_window (STACK_2);
  int x       = get_sint16 (STACK_1);
  int y       = get_sint16 (STACK_0);
  X_CALL(XReparentWindow (dpy, win, win2, x, y));
  skipSTACK(4);
  value1= NIL; mv_count = 1;
}

DEFUN(XLIB:TRANSLATE-COORDINATES, src src-x src-y dst)
{
  int x,y;
  Window child;
  Window src, dest;
  int src_x, src_y;
  Display *dpy;
  int r;

  src   = get_xid_object_and_display (`XLIB::WINDOW`, STACK_3, &dpy);
  dest  = get_window (STACK_0);
  src_x = get_sint16 (STACK_2);
  src_y = get_sint16 (STACK_1);

  X_CALL(r = XTranslateCoordinates (dpy, src, dest, src_x, src_y,
                                    &x, &y, &child));

  if (r) {
    pushSTACK(make_sint16 (x));
    pushSTACK(make_sint16 (y));
    pushSTACK(make_window (get_display_obj (STACK_5), child));
    value3 = popSTACK();
    value2 = popSTACK();
    value1 = popSTACK();
    mv_count = 3;
  } else
    VALUES3(NIL,NIL,NIL);
  skipSTACK(4);
}

/* 4.6  Mapping Windows */
DEFUN(XLIB:MAP-WINDOW, window)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  X_CALL(XMapWindow (dpy, win));
  skipSTACK(1);
  VALUES1(NIL);
}

DEFUN(XLIB:MAP-SUBWINDOWS, window)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  X_CALL(XMapSubwindows (dpy, win));
  skipSTACK(1);
  VALUES1(NIL);
}

DEFUN(XLIB:UNMAP-WINDOW, window)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  X_CALL(XUnmapWindow (dpy, win));
  skipSTACK(1);
  VALUES1(NIL);
}

DEFUN(XLIB:UNMAP-SUBWINDOWS, window)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  X_CALL(XUnmapSubwindows (dpy, win));
  skipSTACK(1);
  VALUES1(NIL);
}

/* 4.7  Destroying Windows */

DEFUN(XLIB:DESTROY-WINDOW, window)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  X_CALL(XDestroyWindow (dpy, win));
  skipSTACK(1);
  VALUES1(NIL);
}

DEFUN(XLIB:DESTROY-SUBWINDOWS, window)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);
  X_CALL(XDestroySubwindows (dpy, win));
  skipSTACK(1);
  VALUES1(NIL);
}

/* 4.8  Pixmaps */
DEFUN(XLIB:CREATE-PIXMAP, &key PIXMAP WIDTH HEIGHT DEPTH DRAWABLE)
{
  Display *dpy;
  Drawable da;
  Pixmap pm;
  int width,height,depth;

  if (!boundp(STACK_0) || !boundp(STACK_1) ||
      !boundp(STACK_2) || !boundp(STACK_3))
    NOTIMPLEMENTED;

  da     = get_drawable_and_display (STACK_0, &dpy);
  width  = get_uint16 (STACK_3);        /* actually uint15! */
  height = get_uint16 (STACK_2);
  depth  = get_uint16 (STACK_1);

  X_CALL(pm = XCreatePixmap (dpy, da, width, height, depth));

  VALUES1(make_pixmap_2(get_display_obj(STACK_0),pm,
                        (!missingp(STACK_4) ? (object)STACK_4 : NIL)));
  skipSTACK(5);
  return;
}

DEFUN(XLIB:FREE-PIXMAP, pixmap)
{
  Display *dpy;
  Pixmap pix = get_pixmap_and_display (STACK_0, &dpy);
  X_CALL(XFreePixmap (dpy, pix));
  skipSTACK(1);
  VALUES1(NIL);
}



/* -----------------------------------------------------------------------
 *  Chapter 5   Graphics Contexts
 * ----------------------------------------------------------------------- */

/* Since libX does not allow to retrieve the clip-mask or the dashes list any
 how, we save the clip-mask and dashes-list in the gcontext instance extra.

 DASHES-LIST is stored in the additional slot xlib::%dashes and is
    represented as a single uint8 or as a simple vector of uint8's.
    (This allowes us to pass the vector directly into the C routine if
    needed.) However this value could be NIL, then the C rep is suffient.

 FLAME -- I find me always fixing the flaws of the narrow-minded C people,
 not capable of defining any clean and consistent interface. Even worse,
 yesterday I spend a couple of hours of debugging just to recognize, that the
 malloc implmentation of the default Linux libc (version 5.3.9 and up [Yes,
 5.4.7 is even worse]!) is now broken, it messed up *my* memory. (I
 considered it all the time working). I have the strange feeling that the
 more popular Linux becomes the more broken it gets. I want the old days
 back, where only a couple of people messed around with Linux, knowing what
 they do.

 [Ya, back to the 0.96 days (or was it 0.98?), I had only 8MB ram (and ~80MB
 hd) and it was just smooth flying under X; The feeling you get when driving
 an empty 'Autobahn' with a capable car at moderate speed, just smooth,
 effient and relaxing].

 Also the manual says (somewhat foggy):
     Changing the dash-offset or dash-list overrides any previous XSetDashes
     request on the context.  The order in which components ... and bla bla
 Maybe have also to save the dash-offset? */


/* 5.2 Creating Graphics Contexts -- 26 keys*/

DEFUN(XLIB:CREATE-GCONTEXT, &key DRAWABLE FUNCTION PLANE-MASK FOREGROUND \
      BACKGROUND LINE-WIDTH LINE-STYLE CAP-STYLE JOIN-STYLE FILL-STYLE   \
      FILL-RULE ARC-MODE TILE STIPPLE TS-X TS-Y FONT SUBWINDOW-MODE      \
      EXPOSURES CLIP-X CLIP-Y CLIP-MASK CLIP-ORDERING DASH-OFFSET DASHES \
      CACHE-P)
{ /* the keyword list must be in sync with
   (defconstant *GCONTEXT-COMPONENTS* ...) in clx.lisp */
  XGCValues values;
  unsigned long valuemask = 0;
  int non_trivial_clip_mask_p = 0; /* whether user specified a rect-seq */
  int non_trivial_dashes_p = 0; /* whether user specified a sequence as :dashes argument */

#define SLOT(ofs, type, slot, mask)                                     \
  if (!missingp(STACK_(ofs)))                                           \
    { values.slot = get_##type (STACK_(ofs)); valuemask |= mask; }

  /* missing: 0=cache-p */
  SLOT (24, gc_function,     function,           GCFunction);
  SLOT (23, uint32,          plane_mask,         GCPlaneMask);
  SLOT (22, pixel,           foreground,         GCForeground);
  SLOT (21, pixel,           background,         GCBackground);
  SLOT (20, sint16,          line_width,         GCLineWidth);
  SLOT (19, line_style,      line_style,         GCLineStyle);
  SLOT (18, cap_style,       cap_style,          GCCapStyle);
  SLOT (17, join_style,      join_style,         GCJoinStyle);
  SLOT (16, fill_style,      fill_style,         GCFillStyle);
  SLOT (15, fill_rule,       fill_rule,          GCFillRule);
  SLOT (14, arc_mode,        arc_mode,           GCArcMode);
  SLOT (13, pixmap,          tile,               GCTile);
  SLOT (12, pixmap,          stipple,            GCStipple);
  SLOT (11, sint16,          ts_x_origin,        GCTileStipXOrigin);
  SLOT (10, sint16,          ts_y_origin,        GCTileStipYOrigin);
  SLOT ( 9, font,            font,               GCFont);
  SLOT ( 8, subwindow_mode,  subwindow_mode,     GCSubwindowMode);
  SLOT ( 7, bool,            graphics_exposures, GCGraphicsExposures);
  SLOT ( 6, sint16,          clip_x_origin,      GCClipXOrigin);
  SLOT ( 5, sint16,          clip_y_origin,      GCClipYOrigin);
  SLOT ( 4, pixmap,          clip_mask,          GCClipMask);
  SLOT ( 2, sint16,          dash_offset,        GCDashOffset);
#undef SLOT

  /* Handle the :clip-mask argument, :clipordering is only used if
     :clip-mask is a rect-seq. */
  if (boundp(STACK_4)) { /* :clip-mask */
    if (pixmap_p (STACK_4)) {
      values.clip_mask = get_pixmap (STACK_4); valuemask |= GCClipMask;
    } else if (eq (STACK_4, `:NONE`) || eq (STACK_4, NIL)) {
      values.clip_mask = None; valuemask |= GCClipMask;
    } else
      non_trivial_clip_mask_p = 1;
  }

  /* Now handle the :dashes argument, same procedure as above. */
  if (boundp(STACK_1)) {
    if (uint8_p (STACK_1)) { /* simple argument */
      values.dashes = get_uint8 (STACK_1); valuemask |= GCDashList;
    } else
      non_trivial_dashes_p = 1;
  }

  if (!missingp(STACK_(25))) { /* :drawable */
    Display *dpy;
    Drawable da = get_drawable_and_display (STACK_(25), &dpy);
    GC gcon;

    X_CALL(gcon = XCreateGC (dpy, da, valuemask, &values));

    VALUES1(make_gcontext (get_display_obj(STACK_(25)), gcon));

    if (non_trivial_clip_mask_p) {
      /* User specified a clip mask, which is a rect-seq.
         Use the (SETF GCONTEXT-CLIP-MASK) function to set it up. */
      pushSTACK(value1);        /* save gcontext */
      pushSTACK(STACK_5);       /* the :clip-mask argument */
      pushSTACK(STACK_1);       /* the gcontext again */
      pushSTACK(STACK_6);       /* the :clip-ordering argument */
      funcall(``XLIB:SET-GCONTEXT-CLIP-MASK``,3);
      value1 = popSTACK();      /* restore gcontext */
    }

    if (non_trivial_dashes_p) {
      /* Same procedure as above */
      pushSTACK(value1);        /* save gcontext */
      pushSTACK(STACK_2);       /* the :dashes argument */
      pushSTACK(STACK_1);       /* gcontext again */
      funcall(``XLIB:SET-GCONTEXT-DASHES``,2);
      value1 = popSTACK();      /* restore gcontext */
    }
  } else {
    pushSTACK(TheSubr (subr_self)->name);
    fehler (error, "~S: At least :DRAWABLE should be specifed.");
  }
  skipSTACK(26);
}


/* 5.3 Graphics Context Attributes */

/* XGetGCValues (3x11) says:
   [...]
   Also note that an invalid resource ID (with one or more of the three
   most-significant bits set to one) will be returned for GCFont, GCTile, and
   GCStipple if the component has never been explicitly set by the client.
   [...]


 FIXME: What about 64bit (or probably 36bit) architectures?
        [I have to look into the source code of libX, but I am afraid,
         that they think every machine is 32bit] */
#define invalid_xid_p(xid) ((xid) & 0xE0000000)

##define DEF_GCONTEXT_SLOT_GETTER(lspnam, type, slot, mask)     \
      DEFUN(XLIB:GCONTEXT-##lspnam, context) {                  \
          XGCValues values;                                     \
          Display *dpy;                                         \
          GC gcon = get_gcontext_and_display (STACK_0, &dpy);   \
          X_CALL(XGetGCValues (dpy, gcon, mask, &values));      \
          VALUES1(make_##type (values.slot));                   \
          skipSTACK(1);                                         \
      }

##define DEF_GCONTEXT_SLOT_GETTER2(lspnam, type, slot, mask)            \
      DEFUN(XLIB:GCONTEXT-##lspnam, context) {                          \
          XGCValues values;                                             \
          Display *dpy;                                                 \
          GC gc = get_gcontext_and_display (STACK_0, &dpy);             \
          X_CALL(XGetGCValues(dpy,gc,mask,&values));                    \
          VALUES1(invalid_xid_p (values.slot) ? NIL                     \
                  : make_##type (get_display_obj (STACK_0), values.slot)); \
          skipSTACK(1);                                                \
      }

##define DEF_SET_GCONTEXT_SLOT(lspnam, type, slot, mask)        \
      DEFUN(XLIB:SET-GCONTEXT-##lspnam##, context val) {        \
          XGCValues values;                                     \
          Display *dpy;                                         \
          GC gcon = get_gcontext_and_display (STACK_1, &dpy);   \
          values.slot = get_##type (STACK_0);                   \
          X_CALL(XChangeGC (dpy, gcon, mask, &values));         \
          VALUES1(STACK_0);                                     \
          skipSTACK(2);                                         \
      }

##define DEF_GCONTEXT_SLOT(lspnam, type, slot, mask)            \
        DEF_GCONTEXT_SLOT_GETTER (lspnam, type, slot, mask)     \
        DEF_SET_GCONTEXT_SLOT (lspnam, type, slot, mask)

##define DEF_GCONTEXT_SLOT2(lspnam, type, slot, mask)           \
        DEF_GCONTEXT_SLOT_GETTER2 (lspnam, type, slot, mask)    \
        DEF_SET_GCONTEXT_SLOT (lspnam, type, slot, mask)

/*--------------------------------------------------------------------------
                  lisp name      type           C slot             mask
 ---------------------------------------------------------------------------*/
DEF_GCONTEXT_SLOT(ARC-MODE,      arc_mode,      arc_mode,          GCArcMode)
DEF_GCONTEXT_SLOT(BACKGROUND,    pixel,         background,       GCBackground)
DEF_GCONTEXT_SLOT(CAP-STYLE,     cap_style,     cap_style,         GCCapStyle)
DEF_GCONTEXT_SLOT(CLIP-X,        sint16,        clip_x_origin,   GCClipXOrigin)
DEF_GCONTEXT_SLOT(CLIP-Y,        sint16,        clip_y_origin,   GCClipYOrigin)
DEF_GCONTEXT_SLOT(DASH-OFFSET,   uint16,        dash_offset,      GCDashOffset)
DEF_GCONTEXT_SLOT(EXPOSURES,     bool,          graphics_exposures,GCGraphicsExposures)
DEF_GCONTEXT_SLOT(FILL-RULE,     fill_rule,     fill_rule,         GCFillRule)
DEF_GCONTEXT_SLOT(FILL-STYLE,    fill_style,    fill_style,        GCFillStyle)
DEF_GCONTEXT_SLOT(FOREGROUND,    pixel,         foreground,       GCForeground)
DEF_GCONTEXT_SLOT(FUNCTION,      gc_function,   function,          GCFunction)
DEF_GCONTEXT_SLOT(JOIN-STYLE,    join_style,    join_style,        GCJoinStyle)
DEF_GCONTEXT_SLOT(LINE-STYLE,    line_style,    line_style,        GCLineStyle)
DEF_GCONTEXT_SLOT(LINE-WIDTH,    sint16,        line_width,        GCLineWidth)
DEF_GCONTEXT_SLOT(PLANE-MASK,    uint32,        plane_mask,        GCPlaneMask)
DEF_GCONTEXT_SLOT(SUBWINDOW-MODE,subwindow_mode,subwindow_mode,GCSubwindowMode)
DEF_GCONTEXT_SLOT(TS-X,          sint16,        ts_x_origin, GCTileStipXOrigin)
DEF_GCONTEXT_SLOT(TS-Y,          sint16,        ts_y_origin, GCTileStipYOrigin)
DEF_GCONTEXT_SLOT2(STIPPLE,      pixmap,        stipple,           GCStipple)
DEF_GCONTEXT_SLOT2(TILE,         pixmap,        tile,              GCTile)

/* What about getting clip-mask?! */

DEFUN(XLIB:GCONTEXT-CACHE-P, context)
{
  Display *dpy;
  unused get_gcontext_and_display (STACK_0, &dpy);
  /* libX seems to cache all GCs */
  VALUES1(T);
  skipSTACK(1);
}

DEFUN(XLIB:SET-GCONTEXT-CACHE-P, arg1 arg2)
{
  Display *dpy;
  unused get_gcontext_and_display (STACK_1, &dpy);
  if (nullp(STACK_0)) {
    pushSTACK(TheSubr (subr_self)->name);
    fehler (error, "~S: This CLX implemenation does not allow uncached graphics contexts.");
  }
  VALUES1(STACK_0);
  skipSTACK(2);
}

/* xlib:gcontext-dashes gcontext */
DEFUN(XLIB:GCONTEXT-DASHES, context)
{
  unused get_gcontext_and_display (STACK_0, 0); /* only type checking here */

  /* Now see if there is a %dashes slot? */
  get_slot(STACK_0,`XLIB::%DASHES`);
  if (eq(value1,nullobj))
    /* Slot unbound --> oops, not set, so return default value */
    value1 = make_uint8(0);    /* FIXME: right? */
  /* mv_count = 1; - done by get_slot() */
  /* Simply return what is there.  Or better copy it? Well, if the luser
     fools around with what he has found, he shoot only himself in the
     foot, not me. So we need not copy here. */
  skipSTACK(1);
}

DEFUN(XLIB::SET-GCONTEXT-DASHES, gcontext dashes)
{ /* (setf (xlib:gcontext-dashes gcontext) dashes) */
  XGCValues values;
  Display *dpy;
  GC gcon = get_gcontext_and_display (STACK_1, &dpy);

  if (uint8_p (STACK_0)) {
    values.dashes = get_uint8 (STACK_0);
    X_CALL(XChangeGC (dpy, gcon, GCDashList, &values));
    /* Now set the %dashes slot. */
    pushSTACK(STACK_1);        /* The instance, hence the gcontext */
    pushSTACK(`XLIB::%DASHES`);                         /* slot */
    pushSTACK(make_uint8 ((uint8)values.dashes));       /* value */
    funcall (L(set_slot_value), 3);
  } else { /* Now STACK_0 is required to be a non-empty sequence */
    uintC n = get_fixnum(funcall1(L(length),STACK_0));
    if (n < 1) {
      pushSTACK(TheSubr(subr_self)->name);
      fehler (error, "~S: The dash list should be non-empty.");
    }
    { /* FIXME: For efficiency reasons, we should look
              if user gave already a byte vector.
              [probably via with-gcontext]. */
      uintC i;

      /* Allocate a simple vector of uint8's: */
      pushSTACK(allocate_bit_vector (/* eltype: */ Atype_8Bit, /* len: */ n));

      /* Copy the values from the dash-list argument into the
       newly created byte-vector representation */
      pushSTACK(STACK_0); pushSTACK(STACK_2); funcall(L(replace),2);

      /* The XSetDashes routine requires also the dash_offset,
       so retrieve it first. */
      begin_x_call();
      XGetGCValues (dpy, gcon, GCDashOffset, &values);
      XSetDashes (dpy, gcon, values.dash_offset,
                  (char*)(TheSbvector(STACK_1)->data), n);
      end_x_call();

      /* Now install the byte-vector into the %dashes slot: */
      pushSTACK(STACK_2);      /* The instance, hence the gcontext */
      pushSTACK(`XLIB::%DASHES`); /* slot */
      pushSTACK(STACK_2);      /* value, the byte-vector */
      funcall (L(set_slot_value), 3);
      skipSTACK(1);            /* clean up; pop the byte-vector */
    }
  }

  VALUES1(STACK_0);
  skipSTACK(2);
}

DEFUN(XLIB:GCONTEXT-CLIP-MASK, context)
{
  unused get_gcontext_and_display (STACK_0, 0); /* only type checking here */

  get_slot(STACK_0,`XLIB::%CLIP-MASK`);
  if (eq(value1,nullobj)) value1 = `:NONE`;
  skipSTACK(1);
  /* mv_count = 1; - done by get_slot() */
}

/* convert Lisp FOO-SEQ to a C vector */
static int get_seq_len (gcv_object_t *seq, gcv_object_t *type, int size) {
  int num = get_uint32(funcall1(L(length),*seq));
  if (num % size) {
    pushSTACK(fixnum(size)); pushSTACK(fixnum(num)); pushSTACK(*type);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,"~S: Argument is not a proper ~S; length of sequence, ~S, is not a multiple of ~S.");
  }
  return num/size;
}
struct seq_generic { void *seq; int slot; };
static void set_seq (gcv_object_t *l_seq, void *c_seq,
                     map_sequence_function_t mapper) {
  struct seq_generic seq;
  seq.seq = c_seq; seq.slot = 0;
  map_sequence(*l_seq,mapper,(void*)&seq);
}

/* convert Lisp RECT-SEQ to a C vector */
struct seq_rectangle { XRectangle *rectangle; int slot; };
void coerce_into_rectangle (void *arg, object element);
void coerce_into_rectangle (void *arg, object element) {
  struct seq_rectangle *rec = (struct seq_rectangle *)arg;
  switch (rec->slot) {
    case 0: rec->rectangle->x = get_sint16(element);
      rec->slot = 1; break;
    case 1: rec->rectangle->y = get_sint16(element);
      rec->slot = 2; break;
    case 2: rec->rectangle->width = get_uint16(element);
      rec->slot = 3; break;
    case 3: rec->rectangle->height = get_uint16(element);
      rec->slot = 0; rec->rectangle++; break;
  }
}

DEFUN(XLIB:SET-GCONTEXT-CLIP-MASK, clip-mask gcontext &optional ordering)
{ /* (SETF (XLIB:GCONTEXT-CLIP-MASK gcontext &optional ordering) clip-mask) */
  Display *dpy;
  GC gcontext = get_gcontext_and_display (STACK_1, &dpy);

  if (eq (STACK_2, `:NONE`) || eq (STACK_2, NIL)) {
    X_CALL(XSetClipMask (dpy, gcontext, None));
  } else if (pixmap_p (STACK_2)) {
    Pixmap pixmap = get_pixmap (STACK_2);
    X_CALL(XSetClipMask (dpy, gcontext, pixmap));
  } else {
    /* FIXME: We could use a more effient representation for the clip-mask
            in the gcontext.
            We should think about the portability of using a halfword-vector
            and then beam the data directly into the rectangles vector. */
    int ordering = get_ordering(STACK_0);
    int n = get_seq_len(&STACK_2,&`XLIB::RECT-SEQ`,4);
    DYNAMIC_ARRAY (rectangles, XRectangle, n);
    set_seq(&STACK_2,rectangles,coerce_into_rectangle);

    {
      XGCValues values;
      begin_x_call();
      XGetGCValues (dpy, gcontext, GCClipXOrigin|GCClipYOrigin, &values);
      XSetClipRectangles (dpy, gcontext, values.clip_x_origin,
                          values.clip_y_origin, rectangles, n/4, ordering);
      end_x_call();
    }

    /* ok. now copy the value given by user, so if he messes around with
       what he gave as argument, it will not affect the saved value. */
    STACK_2 = funcall1(L(copy_seq),STACK_2);

    FREE_DYNAMIC_ARRAY (rectangles);
  }

  /* Now save the value just set in the %clip-mask slot. */
  pushSTACK(STACK_1);          /* The instance, hence the gcontext */
  pushSTACK(`XLIB::%CLIP-MASK`);                /* slot */
  pushSTACK(STACK_4);                           /* value */
  funcall (L(set_slot_value), 3);

  VALUES1(STACK_2);
  skipSTACK(3);
}

DEFUN(XLIB:GCONTEXT-FONT, context &optional pseudo-p)
{
  Display *dpy;
  GC gc;
  int pseudo_font_p;
  XGCValues values;

  pseudo_font_p = !missingp(STACK_0);
  if (pseudo_font_p) NOTIMPLEMENTED;

  gc = get_gcontext_and_display (STACK_1, &dpy);

  X_CALL(XGetGCValues (dpy, gc, GCFont, &values));

  VALUES1(invalid_xid_p (values.font) ? NIL
          : make_font (get_display_obj (STACK_1), values.font));
  skipSTACK(2);
}

DEFUN(XLIB:SET-GCONTEXT-FONT, font context &optional pseudo-p)
{
  int pseudo_font_p;
  XGCValues values;
  Display *dpy;
  GC gc = get_gcontext_and_display (STACK_1, &dpy);

  pseudo_font_p = !missingp(STACK_0);
  if (pseudo_font_p) NOTIMPLEMENTED;
  values.font = get_font (STACK_2);

  X_CALL(XChangeGC (dpy, gc, GCFont, &values));

  VALUES1(STACK_2);
  skipSTACK(3);
}

/* Standard clx objects look: */

DEFUN(XLIB:GCONTEXT-ID, context)
{
  GContext context;
  GC gc = get_gcontext (popSTACK());
  X_CALL(context = XGContextFromGC(gc));
  VALUES1(make_uint32(context));
}

static void query_best_X (Status (*query) (Display*, Drawable,
                                           unsigned int, unsigned int,
                                           unsigned int *, unsigned int *))
{
  unsigned int width, height, x, y;
  Display *dpy;
  Drawable da = get_drawable_and_display (STACK_0, &dpy);
  x = get_uint16 (STACK_2);
  y = get_uint16 (STACK_1);

  X_CALL(query (dpy, da, x, y, &width, &height));

  pushSTACK(make_uint16 (height));
  pushSTACK(make_uint16 (width));
  VALUES2(STACK_0,STACK_1);
  skipSTACK(5);
}

DEFUN(XLIB:QUERY-BEST-STIPPLE, arg1 arg2 arg3)
{
  query_best_X (XQueryBestStipple);
}

DEFUN(XLIB:QUERY-BEST-TILE, arg1 arg2 arg3)
{
  query_best_X (XQueryBestTile);
}

/* 5.3  Copying Graphics Contexts */
DEFUN(XLIB:COPY-GCONTEXT, arg1 arg2)
{
  Display *dpy;
  GC gcon1 = get_gcontext_and_display (STACK_1, &dpy);
  GC gcon2 = get_gcontext (STACK_0);
  X_CALL(XCopyGC (dpy, gcon1, 0x7FFFFFUL, gcon2));
  VALUES0;
  skipSTACK(2);
}

DEFUN(XLIB:COPY-GCONTEXT-COMPONENTS, gc1 gc2 &rest rest)
{
  unsigned i;
  unsigned long mask = 0;
  GC gcon1, gcon2;
  Display *dpy;

  for (i = 0; i < argcount-2; i++) {
    mask |= get_gcontext_key (STACK_0);
    skipSTACK(1);
  }

  gcon1 = get_gcontext_and_display (STACK_0, &dpy);
  gcon2 = get_gcontext (STACK_1);

  X_CALL(XCopyGC (dpy, gcon2, mask, gcon1));
  VALUES0;
  skipSTACK(2);
}

/* 5.4  Destroying Graphics Contexts */
DEFUN(XLIB:FREE-GCONTEXT, context)
{
  Display *dpy;
  GC gcon = get_gcontext_and_display (STACK_0, &dpy);
  X_CALL(XFreeGC (dpy, gcon));
  skipSTACK(1);
  VALUES1(NIL);
}

/* 5.5  Graphics Context Cache */
DEFUN(XLIB:FORCE-GCONTEXT-CHANGES, context)
{
  Display *dpy;
  GC gcon = get_gcontext_and_display (STACK_0, &dpy);
  X_CALL(XFlushGC(dpy,gcon)); /* This function is actually undocumented */
  skipSTACK(1);
  VALUES1(NIL);
}

/* ----------------------------------------------------------------------------
  WITH-GCONTEXT

  Method: with-gcontext should 'bind' some gcontext slots, so we have to save
  them and restore them after exiting the body (probably within an
  unwind-protect). The core of the with-gcontext macro looks like this:

   (let ((saved (%save-gcontext-compoments gcon mask)))
     (unwind-protect
           ,body
        (%restore-gcontext-compoments gcon saved)))

  %save-g.-c. and %restore-g.-c. work by putting the XGCValues structure into
  a bitvector. Plain and simple.

  clip-mask and the dashes-list are to be saved and restored by the Lisp code.

  Another method would be copy the gcontext and modify the fptr. This would
  then also work with dash-list and clip-mask on gcontexts modified by some C
  code. [I plan to incooperate other C libs here.] */

DEFUN(XLIB:%GCONTEXT-KEY->MASK, key)
{ /* Finds the libX mask bit for given symbolic representation of the slot */
  VALUES1(make_uint32 (get_gcontext_key (popSTACK())));
}

/*  data structure in which the values survive. */
typedef struct {
  uint32 mask;     /* values mask, specifies which values where saved */
  XGCValues values;             /* the values itself in native C rep. */
} saved_gcontext_values;

/* Returns the gcontext components selected by 'components', a mask32, and
   returns them in some compact object, which should be considered opaque. */
DEFUN(XLIB:%SAVE-GCONTEXT-COMPONENTS, gcontext components)
{
  saved_gcontext_values values;
  Display *dpy;
  GC gcontext = get_gcontext_and_display (STACK_1, &dpy);
  values.mask = get_uint32 (STACK_0);

  if (values.mask & GCDashList) {
    /* the dash list itself is saved by Lisp code, but we take care of
       the dash offset. */
    values.mask |= GCDashOffset;
    values.mask &= ~GCDashList; /* do not make any nonsense here. */
  }

  if (values.mask & GCClipMask) {
    /* same story as above. */
    values.mask |= GCClipXOrigin|GCClipYOrigin;
    values.mask &= ~GCClipMask;
  }

  X_CALL(XGetGCValues (dpy, gcontext, values.mask, &values.values));
  /* TODO: What to todo on failure of xgetgcvalues? */

  /* Allocate a new bit vector, which should hold the requested components */
  VALUES1(allocate_bit_vector (Atype_Bit, 8 * sizeof (values)));
  X_CALL(memcpy (TheSbvector (value1)->data, &values, sizeof (values))); /* memcpy considered harmful */
  skipSTACK(2);
}

/* Counterpart of xlib:%save-gcontext-components: Installs the saved values.
  Note that the components mask is not needed, since it is saved together
  with the values to avoid malformed restores. */
DEFUN(XLIB:%RESTORE-GCONTEXT-COMPONENTS, gcontext values)
{
  saved_gcontext_values values;
  Display *dpy;
  GC gcontext = get_gcontext_and_display (STACK_1, &dpy);

  X_CALL(memcpy (&values, TheSbvector (STACK_0)->data, sizeof (values)));

  /* do not attempt to restore invalid resource ids
   Probably we want to reinvalidate them, but that seems not to be possible. */
  if (invalid_xid_p (values.values.font))    values.mask&=~GCFont;
  if (invalid_xid_p (values.values.tile))    values.mask&=~GCTile;
  if (invalid_xid_p (values.values.stipple)) values.mask&=~GCStipple;

  X_CALL(XChangeGC (dpy, gcontext, values.mask, &values.values));

  skipSTACK(2);
  VALUES1(NIL);
}


/* -----------------------------------------------------------------------
 *  Chapter 6   Graphics Operations
 * ----------------------------------------------------------------------- */

/* 6.2  Area and Plane Operations */

DEFUN(XLIB:CLEAR-AREA, drawable &key X Y WIDTH HEIGHT EXPOSURES-P)
{
  Display *dpy;
  Window win      = get_drawable_and_display (STACK_5, &dpy);
  int x           = get_sint16_0 (STACK_4);
  int y           = get_sint16_0 (STACK_3);
  int w           = get_uint16_0 (STACK_2);
  int h           = get_uint16_0 (STACK_1);
  int exposures_p = !missingp(STACK_0);

  X_CALL(XClearArea (dpy, win, x,y,w,h, exposures_p));

  skipSTACK(6);
  VALUES0;
}

DEFUN(XLIB:COPY-AREA, source gcontext source-x source-y width height \
      destination destination-x destination-y)
{
  int    dest_y = get_sint16 (popSTACK());
  int    dest_x = get_sint16 (popSTACK());
  Drawable dest = get_drawable (popSTACK());
  int    height = get_sint16 (popSTACK());
  int     width = get_sint16 (popSTACK());
  int     src_y = get_sint16 (popSTACK());
  int     src_x = get_sint16 (popSTACK());
  GC         gc = get_gcontext (popSTACK());
  Display  *dpy;
  Drawable  src = get_drawable_and_display (popSTACK(), &dpy);

  X_CALL(XCopyArea (dpy, src, dest, gc, src_x, src_y, width, height,
                    dest_x, dest_y));

  VALUES1(NIL);
}

DEFUN(XLIB:COPY-PLANE, source gcontext plane source-x source-y width height \
      destination destination-x destination-y)
{ /* WAS: invoke (XCopyPlane, 10, 'v', "D1dgiiiiidii"); */
  int          dest_y = get_sint16 (STACK_0);
  int          dest_x = get_sint16 (STACK_1);
  Drawable       dest = get_drawable (STACK_2);
  int          height = get_sint16 (STACK_3);
  int           width = get_sint16 (STACK_4);
  int           src_y = get_sint16 (STACK_5);
  int           src_x = get_sint16 (STACK_6);
  unsigned long plane = get_uint32 (STACK_7);
  GC               gc = get_gcontext (STACK_8);
  Display        *dpy;
  Drawable        src = get_drawable_and_display (STACK_9, &dpy);

  X_CALL(XCopyPlane (dpy, src, dest, gc, src_x, src_y, width, height,
                     dest_x, dest_y, plane));

  skipSTACK(10);
  VALUES1(NIL);
}

/* 6.3  Drawing Points */
DEFUN(XLIB:DRAW-POINT, drawable gcontext x y)
{ /* WAS: invoke (XDrawPoint, 4, "D1dgii"); */
  int        y = get_sint16 (popSTACK());
  int        x = get_sint16 (popSTACK());
  GC        gc = get_gcontext (popSTACK());
  Display *dpy;
  Drawable  da = get_drawable_and_display (popSTACK(), &dpy);

  X_CALL(XDrawPoint (dpy, da, gc, x, y));

  VALUES1(NIL);
}

/* convert Lisp POINT-SEQ to a C vector */
struct seq_point { XPoint *point; int slot; };
void coerce_into_point (void *arg, object element);
void coerce_into_point (void *arg, object element) {
  struct seq_point *rec = (struct seq_point *)arg;
  sint16 val = get_sint16(element);
  switch (rec->slot) {
    case 0: rec->point->x = val;
      rec->slot = 1; break;
    case 1: rec->point->y = val;
      rec->slot = 0; rec->point++; break;
  }
}

DEFUN(XLIB:DRAW-POINTS, drawable gcontext points &optional relative-p)
{
  Display   *dpy;
  Drawable    da = get_drawable_and_display (STACK_3, &dpy);
  GC          gc = get_gcontext (STACK_2);
  int relative_p = !missingp(STACK_0);
  int npts = get_seq_len(&STACK_1,&`XLIB::POINT-SEQ`,2);
  DYNAMIC_ARRAY (pts, XPoint, npts);
  set_seq(&STACK_1,pts,coerce_into_point);

  X_CALL(XDrawPoints (dpy, da, gc, pts, npts,
                      relative_p ? CoordModePrevious : CoordModeOrigin));

  FREE_DYNAMIC_ARRAY (pts);

  VALUES1(NIL);
  skipSTACK(4);
}

/* 6.4  Drawing Lines */
DEFUN(XLIB:DRAW-LINE, drawable gcontext x0 y0 x1 y1 &optional relative-p)
{
  int relative_p, x1,y1,x2,y2;
  GC gc;
  Drawable da;
  Display *dpy;

  relative_p = !missingp(STACK_0);
  x1 = get_sint16 (STACK_4); y1 = get_sint16 (STACK_3);
  x2 = get_sint16 (STACK_2); y2 = get_sint16 (STACK_1);
  if (relative_p) { x2 += x1; y2 += y1; }

  da = get_drawable_and_display (STACK_6, &dpy);
  gc = get_gcontext (STACK_5);

  X_CALL(XDrawLine (dpy, da, gc, x1, y1, x2, y2));

  skipSTACK(7);
  VALUES1(NIL);
}

/* DEUTSCH Kuerzester Mathematikerwitz: epsilon kleiner null.
 ENGLISH Shortest mathematician's joke: epsilon less than zero. */

/* XLIB:DRAW-LINES [5]drawable [4]gcontext [3]points &key [2]:relative-p
    [1]:fill-p [0](:shape :complex) */
DEFUN(XLIB:DRAW-LINES, drawable gcontext points &key RELATIVE-P FILL-P SHAPE)
{
  Display   *dpy;
  Drawable    da = get_drawable_and_display (STACK_5, &dpy);
  GC          gc = get_gcontext (STACK_4);
  int relative_p = !missingp(STACK_2);
  int     fill_p = !missingp(STACK_1);
  int      shape = (boundp(STACK_0) ? get_shape(STACK_0) : Complex);
  int npoints = get_seq_len(&STACK_3,&`XLIB::POINT-SEQ`,2);
  DYNAMIC_ARRAY (points, XPoint, npoints);
  set_seq(&STACK_3,points,coerce_into_point);

  begin_x_call();
  if (fill_p)
    XFillPolygon (dpy, da, gc, points, npoints, shape,
                  relative_p ? CoordModePrevious : CoordModeOrigin);
  else
    XDrawLines (dpy, da, gc, points, npoints,
                relative_p ? CoordModePrevious : CoordModeOrigin);
  end_x_call();

  FREE_DYNAMIC_ARRAY (points);

  VALUES1(NIL);
  skipSTACK(6);
}

/* convert Lisp SEG-SEQ to a C vector */
struct seq_segment { XSegment *segment; int slot; };
void coerce_into_segment (void *arg, object element);
void coerce_into_segment (void *arg, object element) {
  struct seq_segment *rec = (struct seq_segment *)arg;
  sint16 val = get_sint16(element);
  switch (rec->slot) {
    case 0: rec->segment->x1 = val;
      rec->slot = 1; break;
    case 1: rec->segment->y1 = val;
      rec->slot = 2; break;
    case 2: rec->segment->x2 = val;
      rec->slot = 3; break;
    case 3: rec->segment->y2 = val;
      rec->slot = 0; rec->segment++; break;
  }
}

DEFUN(XLIB:DRAW-SEGMENTS, drawable gcontext segments)
{
  Display *dpy;
  Drawable da  = get_drawable_and_display (STACK_2, &dpy);
  GC gc        = get_gcontext (STACK_1);
  int nsegments = get_seq_len(&STACK_0,&`XLIB::SEG-SEQ`,4);
  DYNAMIC_ARRAY (segments, XSegment, nsegments);
  set_seq(&STACK_0,segments,coerce_into_segment);
  X_CALL(XDrawSegments (dpy, da, gc, segments, nsegments));
  FREE_DYNAMIC_ARRAY (segments);
  VALUES1(NIL);
  skipSTACK(3);
}

/* 6.5  Drawing Rectangles */
DEFUN(XLIB:DRAW-RECTANGLE, drawable gcontext x y width height &optional fill-p)
{
  Display *dpy;
  int fill_p  = !missingp(STACK_0);
  int x       = get_sint16 (STACK_4);
  int y       = get_sint16 (STACK_3);
  int w       = get_sint16 (STACK_2);
  int h       = get_sint16 (STACK_1);
  GC gcon     = get_gcontext (STACK_5);
  Drawable da = get_drawable_and_display (STACK_6, &dpy);

  X_CALL((fill_p ? XFillRectangle : XDrawRectangle) (dpy,da,gcon,x,y,w,h));

  skipSTACK(7);
  VALUES1(NIL);
}

DEFUN(XLIB:DRAW-RECTANGLES, drawable gcontext rectangles &optional fill-p)
{
  Display *dpy;
  Drawable da  = get_drawable_and_display (STACK_3, &dpy);
  GC gc        = get_gcontext (STACK_2);
  int fill_p   = missingp(STACK_0);
  int nrectangles = get_seq_len(&STACK_1,&`XLIB::RECT-SEQ`,4);
  DYNAMIC_ARRAY (rectangles, XRectangle, nrectangles);
  set_seq(&STACK_1,rectangles,coerce_into_rectangle);
  X_CALL((fill_p ? XFillRectangles : XDrawRectangles)
         (dpy, da, gc, rectangles, nrectangles));
  FREE_DYNAMIC_ARRAY (rectangles);

  VALUES1(NIL);
  skipSTACK(4);
}

/* 6.6  Drawing Arcs */
/* XLIB:DRAW-ARC drawable gcontext x y width height angle1 angle2
       &optional fill-p */
DEFUN(XLIB:DRAW-ARC, &rest args)
{
  int fill_p, x,y,w,h, ang1, ang2;
  GC gcon;
  Display *dpy;
  Drawable da;

  ASSERT ((argcount >= 8) && (argcount <= 9));
  fill_p = (argcount == 9) ? (!nullp (popSTACK())) : 0;
  x = get_sint16 (STACK_5); y = get_sint16 (STACK_4);
  w = get_sint16 (STACK_3); h = get_sint16 (STACK_2);
  ang1 = get_angle (STACK_1); ang2 = get_angle (STACK_0);

  gcon = get_gcontext (STACK_6);
  da = get_drawable_and_display (STACK_7, &dpy);

  X_CALL((fill_p ? XFillArc : XDrawArc) (dpy, da, gcon, x, y, w, h,
                                         ang1, ang2));

  skipSTACK(8);
  VALUES0;
}

/* convert Lisp ARC-SEQ to a C vector */
struct seq_arc { XArc *arc; int slot; };
void coerce_into_arc (void *arg, object element);
void coerce_into_arc (void *arg, object element) {
  struct seq_arc *rec = (struct seq_arc *)arg;
  switch (rec->slot) {
    case 0: rec->arc->x = get_sint16(element);
      rec->slot = 1; break;
    case 1: rec->arc->y = get_sint16(element);
      rec->slot = 2; break;
    case 2: rec->arc->width = get_uint16(element);
      rec->slot = 3; break;
    case 3: rec->arc->height = get_uint16(element);
      rec->slot = 4; break;
    case 4: rec->arc->angle1 = get_angle(element);
      rec->slot = 5; break;
    case 5: rec->arc->angle2 = get_angle(element);
      rec->slot = 0; rec->arc++; break;
  }
}

DEFUN(XLIB:DRAW-ARCS, drawable gcontext arcs &optional fill-p)
{ /* arcs = ((x y width height angle1 angle2) ...) */
  Display *dpy;
  Drawable da  = get_drawable_and_display (STACK_3, &dpy);
  GC gc        = get_gcontext (STACK_2);
  int fill_p   = missingp(STACK_0);
  int narcs = get_seq_len(&STACK_1,&`XLIB::ARC-SEQ`,6);
  DYNAMIC_ARRAY (arcs, XArc, narcs);
  set_seq(&STACK_1,arcs,coerce_into_arc);
  X_CALL((fill_p ? XFillArcs : XDrawArcs) (dpy, da, gc, arcs, narcs));
  FREE_DYNAMIC_ARRAY (arcs);
  VALUES1(NIL);
  skipSTACK(4);
}

/* 6.7  Drawing Text */

/* Conversion from chart array to XChar2b array.
Returns 1 if a char array was generated, or 2 if a XChar2b array was
generated. */
static int to_XChar2b (object font, XFontStruct* font_info, const chart* src,
                       XChar2b* dst, unsigned int count)
{
  object encoding;

  pushSTACK(font); pushSTACK(`XLIB::ENCODING`);
  funcall(L(slot_value), 2); encoding = value1;

  if (font_info->min_byte1 == 0 && font_info->max_byte1 == 0) {
    /* Linear addressing */
    if (!nullp(encoding)/*&& TheEncoding(encoding)->max_bytes_per_char==1*/) {
      /* Special hack: use the font's encoding */
      if (count > 0) {
        cstombs(encoding,src,count,(uintB*)dst,count);
        return 1;
      }
    } else
      while (count > 0) {
        unsigned int c = as_cint(*src);
        if (c >= font_info->min_char_or_byte2 &&
            c <= font_info->max_char_or_byte2)
          dst->byte2 = c;
        else
          dst->byte2 = font_info->default_char;
        dst->byte1 = 0;
        src++; dst++; count--;
      }
  } else {                      /* Matrix addressing */
    unsigned int d = font_info->max_char_or_byte2 - font_info->min_char_or_byte2 + 1;
    while (count > 0) {
      unsigned int c = as_cint(*src);
      dst->byte1 = (c/d) + font_info->min_byte1;
      dst->byte2 = (c%d) + font_info->min_char_or_byte2;
      src++; dst++; count--;
    }
  }
  return 2;
}

static void general_draw_text (int image_p)
{ /* General text drawing routine to not to have to duplicate code for
     DRAW-GLYPHS and DRAW-IMAGE-GLYPHS. */
  int size = 0;            /* 8 or 16, 0="have to look into the font" */

  /* First of all fetch the arguments */
#if 0

  STACK_9= drawable;
  STACK_8= gcontext;
  STACK_7= x;
  STACK_6= y;
  STACK_5= sequence;
  STACK_4= start;
  STACK_3= end;
  STACK_2= translate;

  if (boundp(STACK_1))
    width = get_sint16 (STACK_1);
  else
    width = 17; /* Does not mater we ignore this value either way round. */

  if (boundp(STACK_0) && !eq (STACK_0, `:DEFAULT`)) {
    if (eq (STACK_0, fixnum(8)))  size = 8;
    else if (eq (STACK_0, fixnum(16))) size = 16;
    else my_type_error(`(MEMBER 8 16 :DEFAULT)`,STACK_0);
  }

  /* invoke the translation function:
    XLIB:TRANSLATE-DEFAULT src src-start src-end font dst dst-start */
  pushSTACK(source);
  pushSTACK(fixnum(start));
  pushSTACK(fixnum(end));
  pushSTACK(*font);
  pushSTACK(*dest);
  pushSTACK(fixnum(dest_start));
  funcall (*translate, 6);
  /* Now:  value1 = first-not-done
         value2 = NIL or
                  a new font or
                  a delta
         value3 = NIL or
                  current width */
  new_start = get_fixnum(value1);
  textitem.nchars = new_start - start;
  if (textitem.nchars <= 0) {
    /* This should be an error */
  }

  if (size == 8) {
    textitem.chars = (ptr into destination);
  } else {
    /* If we are on a litte-endian machine, we have to convert it ... */
    textitem.chars = (copy the appropriate portion);
    /* Or we do it at the end of the journey... */

    /* .. otherwise just drop the pointer in: */
    textitem.chars = (ptr into destination);
  }

  textitem.delta = 0;           /* default value for delta */
  textitem.font = None;         /* default value for font */
  if (mv_count >= 2 && !nullp (value2)) {
    if (font_p (value2))   textitem.font = value2;
    if (sint16_p (value2)) textitem.delta = get_fixnum(value2);
    /* type-error-p? */
  }

  if (mv_count >= 3 && !nullp (value3)) {
    /* Currently ignored ... */
  }
#else

  Display *dpy;
  Drawable da = get_drawable_and_display (STACK_9, &dpy);
  GC gcon = get_gcontext (STACK_8);
  int x = get_sint16 (STACK_7);
  int y = get_sint16 (STACK_6);
  STACK_5 = check_string(STACK_5);

  {
    object font;
    XFontStruct* font_info = get_font_info_and_display(STACK_8,&font,0);
    uintL len, offset;
    object s_string = unpack_string_ro(STACK_5,&len,&offset);
    const chart* charptr;
    unpack_sstring_alloca(s_string,len,offset,charptr=);
    { DYNAMIC_ARRAY(str,XChar2b,len);
      if (to_XChar2b(font,font_info,charptr,str,len) == 1)
        X_CALL((image_p ? XDrawImageString : XDrawString)
               (dpy, da, gcon, x, y, (char*)str, len));
      else
        X_CALL((image_p ? XDrawImageString16 : XDrawString16)
               (dpy, da, gcon, x, y, str, len));
      FREE_DYNAMIC_ARRAY(str);
    }
  }

  VALUES0;
  skipSTACK(10);
#endif
}

/* XLIB:DRAW-GLYPH drawable[6] gcontext[5] x[4] y[3] element[2]
       &key :translate[1] :width[0] */
DEFUN(XLIB:DRAW-GLYPH, drawable gcontext x y element \
      &key TRANSLATE WIDTH SIZE)
{
  NOTIMPLEMENTED;
}

/* XLIB:DRAW-GLPYHS drawable[9] gcontext[8] x[7] y[6] sequence[5]
         &key (:start 0)[4] :end[3]
         (:translate #'translate-default)[2] :width[1] (:size :default)[0] */
DEFUN(XLIB:DRAW-GLYPHS, drawable gcontext x y sequence \
      &key START END TRANSLATE WIDTH SIZE)
{
  general_draw_text (0);
}

DEFUN(XLIB:DRAW-IMAGE-GLYPH, drawable gcontext x y element \
      &key TRANSLATE WIDTH SIZE)
{UNDEFINED;}
/* XLIB:DRAW-IMAGE-GLPYHS drawable gcontext x y sequence &key (:start 0) :end
       (:translate #'translate-default) :width (:size :default) */
DEFUN(XLIB:DRAW-IMAGE-GLYPHS, drawable gcontext x y sequence \
      &key START END TRANSLATE WIDTH SIZE)
{
  general_draw_text(1);
}


/* XLIB:TRANSLATE-DEFAULT src src-start src-end font dst dst-start
  This function is not actually specified in the manual, so I include here the
  Lisp code from MIT-CLX for reference:
  (DEFUN(translate-default (src src-start src-end font dst dst-start)
     ;; dst is guaranteed to have room for (- src-end src-start) integer elements,
     ;; starting at dst-start; whether dst holds 8-bit or 16-bit elements depends
     ;; on context.  font is the current font, if known.  The function should
     ;; translate as many elements of src as possible into indexes in the current
     ;; font, and store them into dst.
     ;;
     ;; The first return value should be the src index of the first untranslated
     ;; element.  If no further elements need to be translated, the second return
     ;; value should be nil.  If a horizontal motion is required before further
     ;; translation, the second return value should be the delta in x coordinate.
     ;; If a font change is required for further translation, the second return
     ;; value should be the new font.  If known, the pixel width of the translated
     ;; text can be returned as the third value; this can allow for appending of
     ;; subsequent output to the same protocol request, if no overall width has
     ;; been specified at the higher level.
     ;; (returns values: ending-index
     ;;                  (OR null horizontal-motion font)
     ;;                  (OR null translated-width))
     (declare (type sequence src)
              (type array-index src-start src-end dst-start)
              (type (or null font) font)
              (type vector dst)
              (inline graphic-char-p))
     (declare (values integer (or null integer font) (or null integer)))
     font;;not used
     (if (stringp src)
         (do ((i src-start (index+ i 1))
              (j dst-start (index+ j 1))
              (char))
             ((index>= i src-end)
              i)
             (declare (type array-index i j))
             (if (graphic-char-p (setq char (char src i)))
                 (setf (aref dst j) (char->card8 char))
                 (return i)))
       (do ((i src-start (index+ i 1))
            (j dst-start (index+ j 1))
            (elt))
           ((index>= i src-end)
            i)
           (declare (type array-index i j))
           (setq elt (elt src i))
           (cond ((and (characterp elt) (graphic-char-p elt))
                  (setf (aref dst j) (char->card8 elt)))
                 ((integerp elt)
                  (setf (aref dst j) elt))
                 (t
                  (return i))))))
   */

DEFUN(XLIB:TRANSLATE-DEFAULT, a1 a2 a3 a4 a5 a6) /* NIM */
{UNDEFINED;}

/* Together with the above function go the two below listed functions:
  (Which are indeed trivial, since we are on an ASCII system).
  [We could move it to the Lisp side?] */
DEFUN(XLIB:CARD8->CHAR, code) { funcall (L(code_char), 1); }
DEFUN(XLIB:CHAR->CARD8, char) { funcall (L(char_code), 1); }


/* -----------------------------------------------------------------------
 *  Chapter 7   Images
 * ----------------------------------------------------------------------- */

/* Note: As you will probably see within a second, this code is still in
   development.

 ==== There are still not all image formats supported. But the code
      should be safe (Thus bailing out, when ever some format comes in,
      which is not fully understood.)

 If you have urgent need for some particular format please mail me, I'll
 see that I could implement it then. Unfortunately it is not possible
 for me to test this code fully, since I do not have an X server, which
 could understand all the possible formats. */


DEFCHECKER(get_image_format,default=,                           \
           BITMAP=XYBitmap XY-PIXMAP=XYPixmap Z-PIXMAP=ZPixmap)

static uint16 get_image_width (void)
{
  funcall (`XLIB::IMAGE-WIDTH`, 1);
  return get_uint16 (value1);
}

static uint16 get_image_height (void)
{
  funcall (`XLIB::IMAGE-HEIGHT`, 1);
  return get_uint16 (value1);
}

static uint16 get_image_depth (void)
{
  funcall (`XLIB::IMAGE-DEPTH`, 1);
  return get_uint16 (value1);
}

static void ensure_valid_put_image_args (int src_x, int src_y, int w, int h,
                                        int width, int height)
{ /* ensure that the src_{x,y} and w, h arguments are valid,
   signals error if not. */
  /* I find it much easier to express the valid precondition instead of
   the error condition; */
  if (src_x >= 0 && (src_x + w) <= width &&
      src_y >= 0 && (src_y + h) <= height) {
    /* everything o.k. */
    return;
  } else {
    /* TODO: Be more verbose here. */
    fehler (error, ":SRC-X, :SRC-Y, :WIDTH, :HEIGHT are bad");
  }
}

/* call XPutImage + XDestroyImage */
static void image_put_and_destroy (Display* dpy, Drawable drawable, GC gcontext,
                                   XImage* im, int src_x, int src_y,
                                   int x, int y, unsigned int w, unsigned int h)
{
  begin_x_call();
  XPutImage (dpy, drawable, gcontext, im, src_x, src_y, x,y,w,h);
  XDestroyImage (im); /* Note: XDestroyImage frees im->data for us */
  end_x_call();
  dprintf (("\nXPutImage (.., src_x=%d, src_y=%d, x=%d,y=%d,w=%d,h=%d);",
            src_x, src_y, x,y,w,h));
}

/* allocate memory and create the image */
static XImage* create_image (Display *dpy, unsigned int depth, int bitmap_p,
                             unsigned int width, unsigned int height,
                             int bytes_per_line) {
  /* Allocate memory */
  char *data = (char*) my_malloc (bytes_per_line * height);
  XImage *im;

  /* Actually create the image */
  X_CALL(im = XCreateImage (dpy, 0, depth,
                            (bitmap_p && (depth == 1)) ? XYBitmap : ZPixmap, 0,
                            data, width, height, 32, bytes_per_line));
  if (im == 0) {
    free (data);
    pushSTACK(TheSubr(subr_self)->name);
    fehler (error, "~S: XCreateImage call failed.");
  }
  dprintf (("im.bytes_per_line = %d (vs. %d)",
            im->bytes_per_line, bytes_per_line));
  return im;
}

static void handle_image_z (int src_x, int src_y, int x, int y, int w, int h,
                            GC gcontext, Drawable drawable,
                            int bitmap_p, Display *dpy)
{  /* STACK_0 = the image object; cleans up stack after return
   Handles images in the Z-format. This functions should be considered local to
   xlib:put-image. */
  int width;
  int height;
  int depth;
  char *data;
  int bytes_per_line;
  int ix, iy;
  unsigned long v;
  XImage *im;

  /* First fetch the actual image geometry */
  pushSTACK(STACK_0); width  = get_image_width ();
  pushSTACK(STACK_0); height = get_image_height ();
  pushSTACK(STACK_0); depth  = get_image_depth ();

  /* Now ensure that src_x ... h are valid */
  ensure_valid_put_image_args (src_x, src_y, w, h, width, height);

  /* Calculate the bytes_per_line field */
  switch (depth) {
    case 1: bytes_per_line = ((width+31)/32)*4; break;
    case 8: bytes_per_line = ((width+3)/4)*4; break;
    default:
      pushSTACK(sfixnum(depth));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error, "~S: depth=~S is not supported");
  }

  /* Actually create the image */
  im = create_image (dpy, depth, bitmap_p, width, height, bytes_per_line);

  /* fetch the pixarray */
  pushSTACK(STACK_0);
  funcall (`XLIB::IMAGE-Z-PIXARRAY`, 1);
  pushSTACK(value1);

  /* Now the silly loop
   This loop is anything but efficient.
   On the other hand it works reliabably.
   -- That is more important to me than speed. */
  for (iy = 0; iy < height; iy++)
    for (ix = 0; ix < width; ix++) {
      pushSTACK(STACK_0);
      pushSTACK(fixnum(iy));
      pushSTACK(fixnum(ix));
      funcall (L(aref), 3);
      v = get_uint32 (value1);
      X_CALL(XPutPixel (im, ix, iy, v));
    }
  skipSTACK(1);         /* pixarray */

  image_put_and_destroy (dpy, drawable, gcontext, im, src_x, src_y, x,y,w,h);

  skipSTACK(1);                 /* clean up */
}

#if DEBUG_CLX
/* from Barry Fishman <barry_fishman@att.net>
   http://article.gmane.org/gmane.lisp.clisp.general/7587
 This presumes little-endian bitmaps */
void dump_image (XImage *image)
{ /* test function to print the contents of the bitmap */
  int x, y;
  int height = image->height;
  int width  = image->width;
  int line_len = image->bytes_per_line;

  printf("\n;; Image (%s) %dx%dx%d, bpl= %d, pad= %d:\n",
         ((char*[]){"bitmap","xy-pixmap","z-pixmap"})[image->format],
         width,height,image->depth,line_len,image->bitmap_pad);
  for (y = 0; y < height; y++) {
    char * row = image->data + y * line_len;
    printf(";|");
    for (x = 0; x < width; x++)
      printf( (row[x / 8] & (1 << (x % 8))) ? "*" : " ");
    printf("|\n");
  }
  fflush(stdout);
}
#define DUMP_IMAGE(im) dump_image(im)
#else
#define DUMP_IMAGE(im)
#endif

/* make sure to call DISPLAY-FORCE-OUTPUT after this function! */
DEFUN(XLIB:PUT-IMAGE, drawable gcontext image \
      &key SRC-X SRC-Y X Y WIDTH HEIGHT BITMAP-P)
{ /* This is a *VERY* silly implementation.
   XXX see that the keyword arguments are actually given */
  Display *dpy;
  int src_x         = get_sint32_0(STACK_6);
  int src_y         = get_sint32_0(STACK_5);
  int x             = get_sint32 (STACK_4);
  int y             = get_sint32 (STACK_3);
  int w             = get_sint32 (STACK_2);
  int h             = get_sint32 (STACK_1);
  GC gcontext       = get_gcontext (STACK_8);
  Drawable drawable = get_drawable_and_display (STACK_9, &dpy);
  int bitmap_p      = get_bool (STACK_0);

  /* There seem to be three kinds of images passed in:
   IMAGE-X, IMAGE-XY, IMAGE-Z */

  /* First see if it is an IMAGE-X? */
  pushSTACK(STACK_7); funcall (`XLIB::IMAGE-X-P`, 1);
  if (!nullp (value1)) {
#  if 0
    pushSTACK(STACK_7);
    handle_image_x (src_x, src_y, x, y, w, h, gcontext, drawable, bitmap_p, dpy);
#  endif
    /* image-x stuff
     It seems that images of type image-x are already in the format
     needed by XPutImage. */
    int bytes_per_line, bitmap_pad;
    char *data;
    XImage im;

    /* Now fill in the XImage structure from the slots */
    im.depth     = get_uint8(funcall1(`XLIB::IMAGE-DEPTH`,STACK_7));
    im.width     = get_uint16(funcall1(`XLIB::IMAGE-WIDTH`,STACK_7));
    im.height    = get_uint16(funcall1(`XLIB::IMAGE-HEIGHT`,STACK_7));
    im.format    = get_image_format(funcall1(`XLIB::IMAGE-X-FORMAT`,STACK_7));
    im.bytes_per_line = get_uint16(funcall1(`XLIB::IMAGE-X-BYTES-PER-LINE`,STACK_7));
    im.bitmap_pad     = get_uint8(funcall1(`XLIB::IMAGE-X-PAD`,STACK_7));
    im.bits_per_pixel = get_uint8(funcall1(`XLIB::IMAGE-X-BITS-PER-PIXEL`,
                                           STACK_7));
    im.bitmap_bit_order = nullp(funcall1(`XLIB::IMAGE-X-BIT-LSB-FIRST-P`,
                                         STACK_7)) ? MSBFirst : LSBFirst;
    im.byte_order       = nullp(funcall1(`XLIB::IMAGE-X-BYTE-LSB-FIRST-P`,
                                         STACK_7)) ? MSBFirst : LSBFirst;
    im.bitmap_unit    = get_uint8(funcall1(`XLIB::IMAGE-X-UNIT`,STACK_7));
    im.xoffset        = get_uint8(funcall1(`XLIB::IMAGE-X-LEFT-PAD`,STACK_7));

    if (bitmap_p && im.depth == 1)
      im.format = XYBitmap;

    /* Now fetch data - it *must* be a vector of card8 */
    pushSTACK(STACK_7); funcall (`XLIB::IMAGE-X-DATA`, 1);
    if (simple_bit_vector_p (Atype_8Bit, value1)) {
      im.data = (char*)(TheSbvector(value1)->data);
    } else {
      pushSTACK(`(ARRAY XLIB::CARD8 (*))`);
      pushSTACK(STACK_8);
      pushSTACK(TheSubr (subr_self)->name);
      fehler (error, "~S: Slot :DATA of IMAGE-X ~S is not of type ~S.");
    }

    dprintf(("\n;; put-image: IMAGE-X -> %dx%d+%d+%d",w,h,x,y));
    DUMP_IMAGE(&im);

    X_CALL(XPutImage(dpy,drawable,gcontext,&im,src_x,src_y,x,y,w,h));

    goto raus;
  } else {
    /* handle_image_z (src_x, src_y, x, y, w, h, gcontext, drawable,
     bitmap_p, dpy); image-z or image-xy stuff */
    XImage *im;
    int width, height, depth, format;
    unsigned long fg,bg;

    width  = get_sint32(funcall1(`XLIB::IMAGE-WIDTH`,STACK_7));
    height = get_sint32(funcall1(`XLIB::IMAGE-HEIGHT`,STACK_7));
    depth  = get_sint32(funcall1(`XLIB::IMAGE-DEPTH`,STACK_7));

    {
      XGCValues vals;
      X_CALL(XGetGCValues(dpy,gcontext,GCForeground|GCBackground,&vals));
      fg = vals.foreground;
      bg = vals.background;

      dprintf (("\n;; put-image: IMAGE-XY %dx%dx%d", width,height,depth));
      dprintf ((", fg=%.8x, bg=%.8x", fg,bg));
    }

    {
      char *data;
      int bytes_per_line;
      int ix, iy;
      unsigned long v;

      switch (depth) {
        case 1: bytes_per_line = ((width+31)/32)*4; break;
        case 8: bytes_per_line = ((width+3)/4)*4; break;
        default:
          goto fake;
      }

      im = create_image(dpy,depth,bitmap_p,width,height,bytes_per_line);

      pushSTACK(STACK_7); funcall(`XLIB::IMAGE-XY-P`,1);
      if (!nullp (value1)) {
        pushSTACK(STACK_7); funcall(`XLIB::IMAGE-XY-BITMAP-LIST`,1);
        pushSTACK(value1); funcall (L(car), 1);
        pushSTACK(value1);
      } else {
        pushSTACK(STACK_7); funcall(`XLIB::IMAGE-Z-PIXARRAY`,1);
        pushSTACK(value1);
      }
      dprintf (("\n;im = %.8x",im));

      for (ix = 0; ix < width; ix++)
        for (iy = 0; iy < height; iy++) {
          pushSTACK(STACK_0);
          pushSTACK(fixnum(iy));
          pushSTACK(fixnum(ix));
          funcall (L(aref), 3);
          v = get_uint32 (value1);
          X_CALL(XPutPixel (im, ix, iy, v));
        }
      skipSTACK(1);
      image_put_and_destroy (dpy,drawable,gcontext,im,src_x,src_y,x,y,w,h);
    }

    goto raus;
  }


 fake:
  dprintf ((" --- FAKED"));

 raus:
  skipSTACK(10);
}



/* -----------------------------------------------------------------------
 *  Chapter 8   Fonts and Characters
 * ----------------------------------------------------------------------- */

/* 8.2  Opening Fonts */
DEFUN(XLIB:OPEN-FONT, display font)
{
  Display *dpy = (pushSTACK(STACK_1),pop_display());
  Font font;

  /* XXX Maybe a symbol should be o.k. here too? */

  if (stringp (STACK_0)) {
    with_string_0 (STACK_0, GLO(misc_encoding), font_name, {
        X_CALL(font = XLoadFont (dpy, font_name));      /* Load the font */
      });
    /* make up the LISP representation: */
    VALUES1(make_font_with_info (STACK_1, font, STACK_0, NULL));
  } else my_type_error(S(string),STACK_0);
  skipSTACK(2);
}

/* BTW: Mathematics and alcohol don`t mix -- Don't drink and derive.
 [Found on somebody's signature]
 Put here, because I dislike alcohol. (Err, sometimes ..) */

DEFUN(XLIB:CLOSE-FONT, font)
{ /* FIXME: The manual says something about that fonts are reference
    counted..? */

  Display *dpy;
  Font    font = get_font_and_display (STACK_0, &dpy);

  X_CALL(XUnloadFont (dpy, font));

  VALUES1(NIL);
  skipSTACK(1);
}

DEFUN(XLIB:DISCARD-FONT-INFO, font)
{
  XFontStruct *info;

  info = (XFontStruct*) foreign_slot(STACK_0,`XLIB::FONT-INFO`);
  TheFpointer(value1)->fp_pointer = NULL; /* No longer valid */

  X_CALL(if (info) XFreeFontInfo (NULL, info, 1));

  skipSTACK(1);
  VALUES1(NIL);
}

/* 8.3  Listing Fonts */
DEFUN(XLIB:FONT-PATH, display &key RESULT-TYPE) /* [OK] */
{
  Display *dpy = (pushSTACK(STACK_1), pop_display());
  int npathen, i;
  char **pathen;
  gcv_object_t *res_type = &STACK_0;

  X_CALL(pathen = XGetFontPath (dpy, &npathen));

  for (i = 0; i < npathen; i++)
    pushSTACK(asciz_to_string (pathen[i], GLO(misc_encoding)));
  VALUES1(coerce_result_type(npathen,res_type));

  X_CALL(if (pathen) XFreeFontPath (pathen));

  skipSTACK(2);         /* all done */
}

/* convert a Lisp sequence of pathnames to a C vector */
struct seq_path { char **path; };
void coerce_into_path (void *arg, object element);
void coerce_into_path (void *arg, object element) {
  struct seq_path *rec = (struct seq_path *)arg;
  if (stringp(element)) { coerce_into_path_string:
    with_string_0 (element, GLO(pathname_encoding), frob, {
        uintL j = frob_bytelen+1;
        char *path = (char*)my_malloc(j);
        while (j--) path[j] = frob[j];
        *(rec->path++) = path;
      });
  } else {
    element = physical_namestring(element);
    goto coerce_into_path_string;
  }
}

/*  (SETF (XLIB:FONT-PATH display) new-path)
   == (XLIB:SET-FONT-PATH display  new-path)

  NOTE  - The CLX manual says that pathnames are also o.k. as arguments.
          But I consider it dirty, since the X server may live on an
          entirely different architecture than the client. */
DEFUN(XLIB:SET-FONT-PATH, display new-path)
{
  Display *dpy = (pushSTACK(STACK_1), pop_display());
  int npathen = get_uint32(funcall1(L(length),STACK_0)) , i;
  struct seq_path sp;
  DYNAMIC_ARRAY (pathen, char*, npathen);
  sp.path = pathen;
  map_sequence(STACK_0,coerce_into_path,(void*)&sp);

  begin_x_call();
  XSetFontPath (dpy, pathen, npathen);
  for (i = 0; i < npathen; i++) free (pathen [i]);
  end_x_call();

  FREE_DYNAMIC_ARRAY (pathen);

  VALUES1(STACK_0);
  skipSTACK(2);
}

/*   XLIB:LIST-FONT-NAMES display pattern &key (:max-fonts 65535)
                       (:result-type 'list)
  -> sequence of string. */
DEFUN(XLIB:LIST-FONT-NAMES, display pattern &key MAX-FONTS RESULT-TYPE)
{ /* OK */
  Display *dpy  = (pushSTACK(STACK_3), pop_display ());
  int max_fonts = boundp(STACK_1) ? get_fixnum(STACK_1) : 65535;
  int count = 0, i;
  char **names;
  gcv_object_t *res_type = &STACK_0;

  if (stringp (STACK_2)) {
    with_string_0 (STACK_2, GLO(misc_encoding), pattern, {
        X_CALL(names = XListFonts (dpy, pattern, max_fonts, &count));

        if (count) {
          for (i = 0; i < count; i++)
            pushSTACK(asciz_to_string (names[i], GLO(misc_encoding)));
          X_CALL(XFreeFontNames (names));
        }
        VALUES1(coerce_result_type(count,res_type));
        skipSTACK(4);
      });
  } else my_type_error(S(string),STACK_2);
}

/*   XLIB:LIST-FONTS display pattern &key (:max-fonts 65535)
                  (:result-type 'list)
  returns a sequence of pseudo fonts. */
DEFUN(XLIB:LIST-FONTS, display pattern &key MAX-FONTS RESULT-TYPE)
{
  Display *dpy  = (pushSTACK(STACK_3), pop_display ());
  gcv_object_t *dpyf  = &(STACK_3);
  int max_fonts = boundp(STACK_1) ? get_fixnum(STACK_1) : 65535;
  int count = 0, i;
  char **names;
  XFontStruct *infos;
  gcv_object_t *res_type = &STACK_0;

  if (stringp (STACK_2)) {
    with_string_0 (STACK_2, GLO(misc_encoding), pattern, {
        X_CALL(names = XListFontsWithInfo (dpy, pattern, max_fonts,
                                           &count, &infos));

        if (count) {
          for (i = 0; i < count; i++)
            pushSTACK(make_font_with_info (*dpyf, 0, asciz_to_string (names[i], GLO(misc_encoding)), infos+i));

          X_CALL(XFreeFontNames (names));
        }
      });
    VALUES1(coerce_result_type(count,res_type));
    skipSTACK(4);
  } else my_type_error(S(string),STACK_2);

  /* Hmm ... several question araise here ...
    XListFontsWithInfo(display, pattern, maxnames, count_return, info_return)
   But this function does not return the per character information.
   Should we introduce a new function get_font_per_char_info ?!
 */
}

/* 8.4  Font Attributes */

##define DEF_FONT_ATTR(lspnam, type, cnam)                              \
      DEFUN(XLIB:##lspnam, font) {                                      \
        XFontStruct *info = get_font_info_and_display (STACK_0, 0, 0);  \
        VALUES1(make_##type(info->cnam));                               \
        skipSTACK(1);                                                   \
      }

/* ------------------------------------------------------------------------
 *            lisp name              type           C slot
 * ------------------------------------------------------------------------ */
DEF_FONT_ATTR(FONT-ALL-CHARS-EXIST-P,bool,          all_chars_exist)
DEF_FONT_ATTR(FONT-ASCENT,           sint16,        ascent)
DEF_FONT_ATTR(FONT-DEFAULT-CHAR,     uint16,        default_char)
DEF_FONT_ATTR(FONT-DESCENT,          sint16,        descent)
DEF_FONT_ATTR(FONT-DIRECTION,        draw_direction,direction)
DEF_FONT_ATTR(FONT-MAX-BYTE1,        uint8,         max_byte1)
DEF_FONT_ATTR(FONT-MAX-BYTE2,        uint8,         max_char_or_byte2)/* XXX */
DEF_FONT_ATTR(FONT-MAX-CHAR,         uint16,        max_char_or_byte2)
DEF_FONT_ATTR(FONT-MIN-BYTE1,        uint8,         min_byte1)
DEF_FONT_ATTR(FONT-MIN-BYTE2,        uint8,         min_char_or_byte2)/* XXX */
DEF_FONT_ATTR(FONT-MIN-CHAR,         uint16,        min_char_or_byte2)

DEF_FONT_ATTR(MAX-CHAR-ASCENT,       sint16,        max_bounds.ascent)
DEF_FONT_ATTR(MAX-CHAR-ATTRIBUTES,   uint16,        max_bounds.attributes)
DEF_FONT_ATTR(MAX-CHAR-DESCENT,      sint16,        max_bounds.descent)
DEF_FONT_ATTR(MAX-CHAR-LEFT-BEARING, sint16,        max_bounds.lbearing)
DEF_FONT_ATTR(MAX-CHAR-RIGHT-BEARING,sint16,        max_bounds.rbearing)
DEF_FONT_ATTR(MAX-CHAR-WIDTH,        sint16,        max_bounds.width)

DEF_FONT_ATTR(MIN-CHAR-ASCENT,       sint16,        min_bounds.ascent)
DEF_FONT_ATTR(MIN-CHAR-ATTRIBUTES,   uint16,        min_bounds.attributes)
DEF_FONT_ATTR(MIN-CHAR-DESCENT,      sint16,        min_bounds.descent)
DEF_FONT_ATTR(MIN-CHAR-LEFT-BEARING, sint16,        min_bounds.lbearing)
DEF_FONT_ATTR(MIN-CHAR-RIGHT-BEARING,sint16,        min_bounds.rbearing)
DEF_FONT_ATTR(MIN-CHAR-WIDTH,        sint16,        min_bounds.width)
/* -------------------------------------------------------------------- */

DEFUN(XLIB:FONT-NAME, font)
{
  VALUES1(get_font_name(popSTACK()));
}

DEFUN(XLIB:FONT-PROPERTIES, font)
{
  Display *dpy;
  XFontStruct *font_struct = get_font_info_and_display (STACK_0, 0, &dpy);
  int i;

  for (i = 0; i < font_struct->n_properties; i++) {
    pushSTACK(make_xatom(dpy, font_struct->properties[i].name));
    pushSTACK(make_uint32 (font_struct->properties[i].card32));
  }

  VALUES1(listof(2 * font_struct->n_properties));
  skipSTACK(1);         /* all done */
}

DEFUN(XLIB:FONT-PROPERTY, font name)
{
  Display *dpy;
  XFontStruct *font_struct = get_font_info_and_display (STACK_1, 0, &dpy);
  Atom atom                = get_xatom (dpy, STACK_0);
  unsigned long value, status;

  X_CALL(status = XGetFontProperty (font_struct, atom, &value));
  VALUES1(status ? make_uint32 (value) : NIL);
  skipSTACK(2);
}


/* 8.5  Character Attributes */

static XCharStruct *font_char_info (XFontStruct *fs, unsigned int index)
{
  /* from XLoadFont(3X11):
   *
   *   If the min_byte1 and max_byte1 members are both zero,
   *   min_char_or_byte2 specifies the linear character index corresponding
   *   to the first element of the per_char array, and max_char_or_byte2
   *   specifies the linear character index of the last element.
   *       If either min_byte1 or max_byte1 are nonzero, both min_char_or_byte2
   *   and max_char_or_byte2 are less than 256, and the 2-byte character
   *   index values corresponding to the per_char array element N (counting
   *   from 0) are:
   *
   *         byte1 = N/D + min_byte1
   *         byte2 = N\D + min_char_or_byte2
   *
   *   where:
   *
   *           D = max_char_or_byte2 - min_char_or_byte2 + 1
   *           / = integer division
   *           \ = integer modulus
   */

  if (fs->min_byte1 == 0 && fs->max_byte1 == 0) { /* Linear indexing ... */
    if (index >= fs->min_char_or_byte2 && index <= fs->max_char_or_byte2)
      if (fs->per_char)
        return fs->per_char+(index-fs->min_char_or_byte2);
      else
        return &(fs->min_bounds);
  } else {                      /* Nonlinear indexing .. */
    unsigned char byte1 = (index >> 8) &0xFF; /* Is this right?! */
    unsigned char byte2 = index & 0xFF;
    unsigned int d = fs->max_char_or_byte2 - fs->min_char_or_byte2 + 1;

    if (byte1 >= fs->min_byte1 && byte1 <= fs->max_byte1 &&
        byte2 >= fs->min_char_or_byte2 && byte2 <= fs->max_char_or_byte2) {
      index = (byte1 - fs->min_byte1)*d + (byte2 - fs->min_char_or_byte2);

      if (fs->per_char)
        return fs->per_char+index;
      else
        return &(fs->min_bounds);
    }
  }
  /* BTW these two cases could be handled in one, but I leave it here
     for clarity. */

  /* fall thru' */
  return NULL;
}

##define DEF_CHAR_ATTR(lspnam, type, cnam)                              \
    DEFUN(lspnam, font code) {                                          \
      XFontStruct *font_info = get_font_info_and_display (STACK_1, 0, 0); \
      unsigned int index = get_uint16 (STACK_0);                        \
      XCharStruct *char_info = font_char_info (font_info, index);       \
      if (char_info)                                                    \
        if (char_info->lbearing == 0   &&                               \
            char_info->rbearing == 0   &&                               \
            char_info->width == 0      &&                               \
            char_info->attributes == 0 &&                               \
            char_info->ascent == 0     &&                               \
            char_info->descent == 0)                                    \
          value1 = NIL;                                                 \
        else                                                            \
          value1 = make_##type (char_info->cnam);                       \
      else                                                              \
        value1 = NIL;                                                   \
      mv_count = 1;                                                     \
      skipSTACK(2);                                                     \
    }

DEF_CHAR_ATTR(XLIB:CHAR-LEFT-BEARING,  sint16, lbearing)
DEF_CHAR_ATTR(XLIB:CHAR-RIGHT-BEARING, sint16, rbearing)
DEF_CHAR_ATTR(XLIB:CHAR-WIDTH,         sint16, width)
DEF_CHAR_ATTR(XLIB:CHAR-ATTRIBUTES,    sint16, attributes)
DEF_CHAR_ATTR(XLIB:CHAR-ASCENT,        sint16, ascent)
DEF_CHAR_ATTR(XLIB:CHAR-DESCENT,       sint16, descent)

/* 8.6  Querying Text Size */
DEFUN(XLIB:TEXT-EXTENTS, font obj &key START END TRANSLATE)
{ /* FIXME: Could font be a graphics context?!
     -- yes! This is handled by get_font_info_and_display already */
  object font;
  XFontStruct *font_info = get_font_info_and_display (STACK_4, &font, 0);
  int start = get_uint16_0 (STACK_2);
  int dir;
  int font_ascent, font_descent;
  XCharStruct overall;
  uintL len, offset;
  object s_string = unpack_string_ro(STACK_3=check_string(STACK_3),
                                     &len,&offset);
  const chart* charptr;
  int end = missingp(STACK_1) ? len : get_uint16(STACK_1);
  /* START/END handling should be done via test_string_limits_ro ... */
  if (end > len) end = len;
  if (start > end) start = end;
  unpack_sstring_alloca(s_string,end-start,start+offset,charptr=);
  { DYNAMIC_ARRAY(str,XChar2b,end-start);
    if (to_XChar2b(font,font_info,charptr,str,end-start) == 1)
      X_CALL(XTextExtents (font_info, (char*)str, end-start, &dir,
                           &font_ascent, &font_descent, &overall));
    else
      X_CALL(XTextExtents16 (font_info, str, end-start, &dir,
                             &font_ascent, &font_descent, &overall));
    FREE_DYNAMIC_ARRAY(str);
  }

  pushSTACK(make_sint32(overall.width));      /* width */
  pushSTACK(make_sint16(overall.ascent));     /* ascent */
  pushSTACK(make_sint16(overall.descent));    /* descent */
  pushSTACK(make_sint16(overall.lbearing));   /* left */
  pushSTACK(make_sint16(overall.rbearing));   /* right */
  pushSTACK(make_sint16(font_ascent));        /* font-ascent */
  pushSTACK(make_sint16(font_descent));       /* font-descent */
  pushSTACK(make_draw_direction (dir));       /* direction */
  pushSTACK(NIL);                             /* first-not-done */

  value9 = popSTACK();
  value8 = popSTACK();
  value7 = popSTACK();
  value6 = popSTACK();
  value5 = popSTACK();
  value4 = popSTACK();
  value3 = popSTACK();
  value2 = popSTACK();
  value1 = popSTACK();
  mv_count = 9;
  skipSTACK(5);
}

/* -> width - Type int32
   -> first-not-done - Type array-index or null. */
DEFUN(XLIB:TEXT-WIDTH, font sequence &key START END TRANSLATE)
{
  object font;
  XFontStruct *font_info = get_font_info_and_display (STACK_4, &font, 0);

  if (stringp(STACK_3)) {
    int start = get_uint16_0 (STACK_2);
    int w;
    uintL len, offset;
    object s_string = unpack_string_ro(STACK_3,&len,&offset);
    const chart* charptr;
    int end = missingp(STACK_1) ? len : get_uint16(STACK_1);
    /* START/END handling should be done via test_string_limits_ro ... */
    if (end > len) end = len;
    if (start > end) start = end;
    unpack_sstring_alloca(s_string,end-start,start+offset,charptr=);
    { DYNAMIC_ARRAY(str,XChar2b,end-start);
      if (to_XChar2b(font,font_info,charptr,str,end-start) == 1)
        X_CALL(w = XTextWidth (font_info, (char*)str, end-start));
      else
        X_CALL(w = XTextWidth16 (font_info, str, end-start));
      FREE_DYNAMIC_ARRAY(str);
    }
    VALUES2(make_sint32 (w),NIL);
  } else if (listp (STACK_3)) {
    /* Now the generic case for lists
     XXX -- Fix this also above
     XXX This is faked, isn't it. */
    VALUES2(make_sint32(0),NIL);
  } else if (vectorp (STACK_3)) {
    /* Generic case for vectors.
     XXX faked. */
    int start = get_uint16_0 (STACK_2);
    int end   = missingp(STACK_1) ? vector_length (STACK_3) : get_uint16 (STACK_1);
    VALUES2(make_sint32(0),NIL);
  } else
    my_type_error(`SEQUENCE`,STACK_3);

  skipSTACK(5);
}


/* -----------------------------------------------------------------------
 *  Chapter 9   Colors
 * ----------------------------------------------------------------------- */

/*  9.2  Color Functions

  These functions moved to LISP.
  MAKE-COLOR COLOR-BLUE COLOR-GREEN COLOR-P COLOR-RED COLOR-RGB
 */

/* 9.3  Colormap Functions */
DEFUN(XLIB:CREATE-COLORMAP, visual colormap &optional alloc-p)
{
  int alloc_p  = !missingp(STACK_0);
  Display *dpy;
  Window   win = get_window_and_display (STACK_1, &dpy);
  Visual  *vis = get_visual (dpy, STACK_2);
  Colormap map;

  X_CALL(map = XCreateColormap (dpy, win, vis, alloc_p));

  VALUES1(make_colormap (get_display_obj (STACK_1), map));
  skipSTACK(3);
}

DEFUN(XLIB:COPY-COLORMAP-AND-FREE, colormap)
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (STACK_0, &dpy);
  Colormap  cm2;

  X_CALL(cm2 = XCopyColormapAndFree (dpy, cm));

  VALUES1(make_colormap (get_display_obj (STACK_0), cm2));
  skipSTACK(1);
}

DEFUN(XLIB:FREE-COLORMAP, colormap)
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (popSTACK(), &dpy);

  X_CALL(XFreeColormap (dpy, cm));

  VALUES1(NIL);
}

DEFUN(XLIB:INSTALL-COLORMAP, colormap)
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (popSTACK(), &dpy);

  X_CALL(XInstallColormap (dpy, cm));

  VALUES1(NIL);
}

DEFUN(XLIB:INSTALLED-COLORMAPS, window &key RESULT-TYPE)
{
  Display     *dpy;
  Window       win = get_window_and_display (STACK_1, &dpy);
  gcv_object_t *dpy_objf = &(STACK_1);
  int      num_cms = 0;         /* paranoia */
  int            i;
  Colormap    *cms;
  gcv_object_t *res_type = &STACK_0;

  X_CALL(cms = XListInstalledColormaps (dpy, win, &num_cms));

  /* Now push all colormaps ... */
  for (i = 0; i < num_cms; i++)
    pushSTACK(make_colormap (*dpy_objf, cms[i]));

  if (cms) X_CALL(XFree (cms));

  /* Now cons 'em together */
  VALUES1(coerce_result_type(num_cms,res_type));

  skipSTACK(2);         /* all done */
}

DEFUN(XLIB:UNINSTALL-COLORMAP, colormap)
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (popSTACK(), &dpy);

  X_CALL(XUninstallColormap (dpy, cm));

  VALUES1(NIL);
}

/*
  xlib:colormap-visual-info colormap

  returns the visual-info corresponding to a colormap

  NIM
 */
DEFUN(XLIB:COLORMAP-VISUAL-INFO, colormap)
{
  Display *dpy;
  Colormap cm = get_colormap_and_display (STACK_0, &dpy);
  Visual *vis;

  begin_x_call();
  {
    XcmsCCC ccc = XcmsCCCOfColormap (dpy, cm);
    vis = ccc ? XcmsVisualOfCCC (ccc) : 0;
    /* FIXME: Should we free the XcmsCCC? are they hashed or what? */
  }
  end_x_call();

  VALUES1(vis ? make_visual_info (vis) : NIL);
  skipSTACK(1);
}

nonreturning_function(static, error_no_such_color,
                      (object display, object color)) {
  pushSTACK(display); /* get_display_obj() can trigger GC! */
  pushSTACK(color); /* color argument */
  pushSTACK(TheSubr(subr_self)->name);
  STACK_2 = get_display_obj(STACK_2); /* display argument */
  fehler(error,"~S: Color ~S is unknown to display ~S.");
}

DEFUN(XLIB:ALLOC-COLOR, arg1 arg2)
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (STACK_1, &dpy);
  XColor color;
  int status;

  if (stringp (STACK_0) || symbolp (STACK_0)) {
    XColor exact_color;

    with_stringable_0_tc (STACK_0, GLO(misc_encoding), name, {
        X_CALL(status = XAllocNamedColor(dpy,cm,name,&color,&exact_color));
        if (status) {
          pushSTACK(make_pixel (color.pixel)); /* pixel */
          pushSTACK(make_color (&color));      /* screen color */
          value3 = make_color (&exact_color);   /* exact color */
          value2 = popSTACK();
          value1 = popSTACK();
          mv_count = 3;
        } else
          goto failed;
    });
  } else if (color_p (STACK_0)) {
    get_color (dpy, STACK_0, &color);
    X_CALL(status = XAllocColor(dpy,cm,&color));
    if (status) {
      pushSTACK(make_pixel(color.pixel)); /* pixel */
      value2 = make_color(&color);        /* screen color */
      value3 = STACK_1;         /* exact color (what the luser gave) */
      value1 = popSTACK();
      mv_count = 3;
    } else
      goto failed;
  } else my_type_error(`(OR STRING SYMBOL XLIB::COLOR)`,STACK_0);

  skipSTACK(2);
  return;

 failed: /* I have to see what the MIT-CLX implementation does here ... */
  error_no_such_color(STACK_1,STACK_0);
}

/* XLIB:ALLOC-COLOR-CELLS colormap colors &key (:planes 0) :contiguous-p
                          (:result-type 'list)
 returns
   pixels, masks -- Type sequence of pixels */
DEFUN(XLIB:ALLOC-COLOR-CELLS, colormap colors \
      &key PLANES CONTIGUOUS-P RESULT-TYPE)
{
  Display         *dpy;
  Colormap          cm = get_colormap_and_display (STACK_4, &dpy);
  unsigned int npixels = get_uint32 (STACK_3);
  unsigned int nplanes = get_uint32_0(STACK_2);
  Bool    contiguous_p = !missingp(STACK_1);
  gcv_object_t *res_type = &STACK_0;
  int status;

  /* FIXME -- we should introduce some checks here, since if the luser gave
            nonsense arguments, we might run into real problems. */

  {
    DYNAMIC_ARRAY (plane_masks, unsigned long, nplanes);
    {
      DYNAMIC_ARRAY (pixels, unsigned long, npixels);

      X_CALL(status = XAllocColorCells(dpy,cm,contiguous_p,plane_masks,
                                       nplanes,pixels,npixels));
      if (status) {
        unsigned i;

        for (i = 0; i < nplanes; i++)
          pushSTACK(make_uint32 (plane_masks [i]));
        value1 = coerce_result_type(nplanes,res_type);
        pushSTACK(value1);

        for (i = 0; i < npixels; i++)
          pushSTACK(make_uint32 (pixels [i]));
        VALUES2(coerce_result_type(npixels,res_type),popSTACK());
      } else
        /* Q: Should we raise a x-error-sonstwas condition here? */
        VALUES1(NIL);

      FREE_DYNAMIC_ARRAY (pixels);
    }
    FREE_DYNAMIC_ARRAY (plane_masks);
  }
  skipSTACK(5);
}

/*  XLIB:ALLOC-COLOR-PLANES colormap colors &key (:reds 0) (:greens 0)
       (:blues 0) :contiguous-p (:result-type 'list)
  returns: pixels                          -- Type sequence of pixels
           red-mask, green-mask, blue-mask -- Type pixel. */
DEFUN(XLIB:ALLOC-COLOR-PLANES, colormap colors \
      &key REDS GREENS BLUES CONTIGUOUS-P RESULT-TYPE)
{
  Display         *dpy;
  Colormap          cm = get_colormap_and_display (STACK_6, &dpy);
  unsigned int ncolors = get_uint32 (STACK_5);
  unsigned int   nreds = get_uint32_0(STACK_4);
  unsigned int ngreens = get_uint32_0(STACK_3);
  unsigned int  nblues = get_uint32_0(STACK_2);
  Bool    contiguous_p = !missingp(STACK_1);
  unsigned long red_mask, green_mask, blue_mask;
  gcv_object_t *res_type = &STACK_0;

  {
    int status;
    DYNAMIC_ARRAY (pixels, unsigned long, ncolors);

    X_CALL(status = XAllocColorPlanes(dpy,cm,contiguous_p,pixels,ncolors,
                                      nreds,ngreens,nblues,
                                      &red_mask,&green_mask,&blue_mask));
    if (status) {
      uintC i;
      for (i = 0; i < ncolors; i++)
        pushSTACK(make_uint32 (pixels [i]));
      value1 = coerce_result_type(ncolors,res_type);
      pushSTACK(value1);
      pushSTACK(make_uint32 (red_mask));
      pushSTACK(make_uint32 (green_mask));
      pushSTACK(make_uint32 (blue_mask));
      value1 = STACK_3;
      value2 = STACK_2;
      value3 = STACK_1;
      value4 = STACK_0;
      mv_count = 4;
      skipSTACK(4);
    } else
      VALUES1(NIL);

    FREE_DYNAMIC_ARRAY (pixels);
  }
  skipSTACK(7);
}

/* convert a Lisp sequence of pixels to a C vector */
struct seq_pixel { unsigned long* pixel; };
void coerce_into_pixel (void *arg, object element);
void coerce_into_pixel (void *arg, object element)
{ *(((struct seq_pixel *)arg)->pixel++) = get_pixel(element); }

DEFUN(XLIB:FREE-COLORS, colormap pixels &optional plane-mask)
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (STACK_2, &dpy);
  unsigned long plane_mask = (boundp(STACK_0) ? get_pixel (STACK_0) : 0);
  unsigned int npixels = get_uint32(funcall1(L(length),STACK_1));
  struct seq_pixel sp;
  DYNAMIC_ARRAY (pixels, unsigned long, npixels);
  sp.pixel = pixels;
  map_sequence(STACK_1,coerce_into_pixel,(void*)&sp);

  X_CALL(XFreeColors (dpy, cm, pixels, npixels, plane_mask));

  FREE_DYNAMIC_ARRAY (pixels);

  VALUES1(NIL);
  skipSTACK(3);
}

DEFUN(XLIB:LOOKUP-COLOR, colormap name) /* [OK] */
{
  Display *dpy;
  Colormap  cm = get_colormap_and_display (STACK_1, &dpy);
  XColor exact_color, screen_color;
  int status;

  with_stringable_0_tc (STACK_0, GLO(misc_encoding), name, {
      X_CALL(status = XLookupColor(dpy,cm,name,&exact_color,&screen_color));
      if (status) {
        pushSTACK(make_color (&screen_color));
        value2 = make_color (&exact_color);
        value1 = popSTACK();
        mv_count = 2;
      } else
        error_no_such_color(STACK_1,STACK_0);
    });
  skipSTACK(2);
}

/* convert a Lisp sequence of pixels to a C vector of colors at them */
struct seq_color { Display *dpy; XColor* color; };
void coerce_into_color (void *arg, object element);
void coerce_into_color (void *arg, object element) {
  struct seq_color *sc = (struct seq_color *)arg;
  get_color(sc->dpy,element,sc->color++);
}

DEFUN(XLIB:QUERY-COLORS, colormap pixels &key RESULT-TYPE)
{ /* returns: colors -- Type sequence of color. */
  Display *dpy;
  Colormap cm = get_colormap_and_display (STACK_2, &dpy);
  gcv_object_t *res_type = &STACK_0;
  int ncolors = get_uint32(funcall1(L(length),STACK_1)), i;
  struct seq_color sc;
  DYNAMIC_ARRAY (colors, XColor, ncolors);
  sc.dpy = dpy; sc.color = colors;
  map_sequence(STACK_1,coerce_into_color,(void*)&sc);

  X_CALL(XQueryColors (dpy, cm, colors, ncolors));
  /* FIXME - find what to do with the DoRed, DoGreen, and DoBlue flags?! */

  for (i = 0; i < ncolors; i++)
    pushSTACK(make_color (&(colors[i])));
  VALUES1(coerce_result_type(ncolors,res_type));

  FREE_DYNAMIC_ARRAY (colors);
  skipSTACK(3);                 /* all done */
}

DEFFLAGSET(xlib_rgb, DoRed DoGreen DoBlue)

/*  XLIB:STORE-COLOR colormap pixel color
         &key (:red-p t) (:green-p t) (:blue-p t)  */
DEFUN(XLIB:STORE-COLOR, colormap pixel color &key RED-P GREEN-P BLUE-P)
{
  char flags = xlib_rgb();
  Display *dpy;
  Colormap cm = get_colormap_and_display (STACK_2, &dpy);
  XColor color;

  get_color (dpy, STACK_0, &color);
  color.pixel = get_uint32 (STACK_1);
  color.flags = flags;

  X_CALL(XStoreColor (dpy, cm, &color));

  VALUES0;
  skipSTACK(3);
}

/* convert a Lisp sequence of (pixel color) to a C vector of colors */
struct seq_pixel_color { Display *dpy; XColor* color; int slot; char flags; };
void coerce_into_pixel_color (void *arg, object element);
void coerce_into_pixel_color (void *arg, object element) {
  struct seq_pixel_color *spc = (struct seq_pixel_color *)arg;
  switch (spc->slot) {
    case 0:
      spc->color->pixel = get_pixel(element);
      spc->color->flags = spc->flags;
      spc->slot = 1; break;
    case 1:
      get_color(spc->dpy,element,spc->color++);
      spc->slot = 0; break;
  }
}

/*  XLIB:STORE-COLORS colormap pixel-colors
         &key (:red-p t) (:green-p t) (:blue-p t) */
DEFUN(XLIB:STORE-COLORS, colormap pixel-colors &key RED-P GREEN-P BLUE-P)
{
  char flags = xlib_rgb();
  Display *dpy;
  Colormap cm = get_colormap_and_display (STACK_1, &dpy);
  int ncolors = get_seq_len(&STACK_0,&`XLIB::PIXEL-COLORS-SEQ`,2);
  struct seq_pixel_color spc;
  DYNAMIC_ARRAY (colors, XColor, ncolors);
  spc.dpy = dpy; spc.color = colors; spc.slot = 0; spc.flags = flags;
  map_sequence(STACK_0,coerce_into_pixel_color,(void*)&spc);

  X_CALL(XStoreColors (dpy, cm, colors, ncolors));

  FREE_DYNAMIC_ARRAY (colors);
  VALUES1(NIL);
  skipSTACK(2);
}



/* -----------------------------------------------------------------------
 *  Chapter 10  Cursors
 * ----------------------------------------------------------------------- */

/* 10.2  Creating Cursors */
/*  XLIB:CREATE-CURSOR &key [5]:source [4]:mask [3]:x [2]:y
         [1]:foreground [0]:background

 FIXME: May also here are color names legal?! */
DEFUN(XLIB:CREATE-CURSOR, &key SOURCE MASK X Y FOREGROUND BACKGROUND)
{
  Display *dpy;
  Pixmap source;
  Pixmap mask = None;
  XColor foreground;
  XColor background;
  unsigned int x, y;
  Cursor cursor;

  if (boundp(STACK_5))
    source = get_pixmap_and_display (STACK_5, &dpy);
  else
    goto required;

  if (boundp(STACK_4))
    mask = get_pixmap (STACK_4);

  if (boundp(STACK_3))
    x = get_sint16 (STACK_3);
  else
    goto required;

  if (boundp(STACK_2))
    y = get_sint16 (STACK_2);
  else
    goto required;

  if (boundp(STACK_1))
    get_color (dpy, STACK_1, &foreground);
  else
    goto required;

  if (boundp(STACK_0))
    get_color (dpy, STACK_0, &background);
  else
    goto required;

  X_CALL(cursor = XCreatePixmapCursor (dpy, source, mask, &foreground,
                                       &background, x, y));

  VALUES1(make_cursor (get_display_obj (STACK_5),cursor));
  skipSTACK(6);         /* All done */
  return;

 required:
  error_required_keywords(`(:SOURCE :X :Y :FOREGROUND :BACKGROUND)`);
}

/*  XLIB:CREATE-GLYPH-CURSOR &key [5]:source-font [4]:source-char
       [3]:mask-font [2](:mask-char 0) [1]:foreground [0]:background  */
DEFUN(XLIB:CREATE-GLYPH-CURSOR, &key SOURCE-FONT SOURCE-CHAR MASK-FONT  \
      MASK-CHAR FOREGROUND BACKGROUND)
{
  Display *dpy;
  Font source_font;
  unsigned int source_char;
  Font mask_font = None;
  unsigned int mask_char = 0;
  XColor foreground;
  XColor background;
  Cursor cursor;

  if (boundp(STACK_5))          /* :source-font */
    source_font = get_font_and_display (STACK_5, &dpy);
  else
    goto required;

  if (boundp(STACK_4))          /* :source-char */
    source_char = get_uint16 (STACK_4);
  else
    goto required;

  if (boundp(STACK_3))          /* :mask-font */
    mask_font = get_font (STACK_3);

  if (boundp(STACK_2))          /* :mask-char */
    mask_char = get_uint16 (STACK_2);

  if (boundp(STACK_1))          /* :foreground */
    get_color (dpy, STACK_1, &foreground);
  else
    goto required;

  if (boundp(STACK_0))          /* :background */
    get_color (dpy, STACK_0, &background);
  else
    goto required;

  X_CALL(cursor = XCreateGlyphCursor (dpy, source_font, mask_font, source_char,
                                      mask_char, &foreground, &background));

  VALUES1(make_cursor (get_display_obj (STACK_5),cursor));
  skipSTACK(6);         /* All done */
  return;

 required:
  error_required_keywords(`(:SOURCE-FONT :SOURCE-CHAR :FOREGROUND :BACKGROUND)`);
}

DEFUN(XLIB:FREE-CURSOR, cursor)
{
  Display *dpy;
  Cursor cur = get_cursor_and_display (STACK_0, &dpy);
  X_CALL(XFreeCursor (dpy, cur));
  skipSTACK(1);
  VALUES1(NIL);
}

/* 10.3  Cursor Functions */
DEFUN(XLIB:QUERY-BEST-CURSOR, arg1 arg2 arg3)
{
  query_best_X (XQueryBestCursor);
}

/*   XLIB:RECOLOR-CURSOR cursor foreground background

 FIXME? Are color names also OK here? */
DEFUN(XLIB:RECOLOR-CURSOR, arg1 arg2 arg3)
{
  Display *dpy;
  Cursor cursor = get_cursor_and_display (STACK_2, &dpy);
  XColor foreground,background;

  get_color (dpy, STACK_1, &foreground);
  get_color (dpy, STACK_1, &background);

  X_CALL(XRecolorCursor (dpy, cursor, &foreground, &background));

  VALUES1(NIL);
  skipSTACK(3);         /* all done */
}

/* 10.4 Cursor Attributes */


/* -----------------------------------------------------------------------
 *  Chapter 11  Atoms, Properties and Selections
 * ----------------------------------------------------------------------- */

/* 11.1  Atoms */
DEFUN(XLIB:ATOM-NAME, display atom) /* OK */
{
  Atom atom = get_uint29 (popSTACK());
  Display *dpy = pop_display ();

  VALUES1(make_xatom(dpy,atom));
}

DEFUN(XLIB:FIND-ATOM, display atom) /* OK */
{
  Display *dpy  = (pushSTACK(STACK_1), pop_display ());
  Atom atom     = get_xatom_nointern (dpy, STACK_0);
  skipSTACK(2);
  VALUES1((atom != None) ? make_uint32 (atom) : NIL);
}

DEFUN(XLIB:INTERN-ATOM, display atom) /* OK */
{
  Display *dpy  = (pushSTACK(STACK_1), pop_display ());
  Atom atom     = get_xatom (dpy, STACK_0);
  skipSTACK(2);
  VALUES1((atom != None) ? make_uint32 (atom) : NIL);
}

/* 11.2  Properties */

/* convert a Lisp sequence of (pixel color) to a C vector of colors */
struct seq_map { gcv_object_t *transform; unsigned char *data; int format; };
void coerce_into_map (void *arg, object element);
void coerce_into_map (void *arg, object element) {
  struct seq_map *sm = (struct seq_map *)arg;
  if (!missingp(*(sm->transform))) { /* call the transform function */
    pushSTACK(element); funcall(*(sm->transform),1); element = value1;
  }
  switch (sm->format) {
    case 8:   *((uint8*)(sm->data)) = get_uint8(element);  sm->data++;  break;
    case 16: *((uint16*)(sm->data)) = get_uint16(element); sm->data+=2; break;
    case 32: *((uint32*)(sm->data)) = get_aint32(element); sm->data+=4; break;
      /* NOTE: I am using aint32, here not knowing if that is correct,
         the manual does not specify of which type the property data
         should be. [aint16, aint8 also?]. */
    default:
      NOTREACHED;
  }
}

DEFCHECKER(check_propmode,default=PropModeReplace, REPLACE=PropModeReplace \
           PREPEND=PropModePrepend APPEND=PropModeAppend)
/*   XLIB:CHANGE-PROPERTY window property data type format
          &key (:mode :replace) (:start 0) :end :transform */
DEFUN(XLIB:CHANGE-PROPERTY, window property data type format \
      &key MODE START END TRANSFORM)
{
  Display  *dpy;
  Window    win = get_window_and_display (STACK_8, &dpy);
  Atom property = get_xatom (dpy, STACK_7);
  Atom     type = get_xatom (dpy, STACK_5);
  int    format = get_uint8 (STACK_4);
  int      mode = check_propmode(STACK_3);
  int     start = get_uint32_0 (STACK_2);
  int       end;
  int       len;
  unsigned char *data;

  if (format != 8 && format != 16 && format != 32)
    my_type_error(`(MEMBER 8 16 32)`,STACK_4);

  if (missingp(STACK_1)) /* data argument */
    end = get_uint32(funcall1(L(length),STACK_6));
  else
    end = get_uint32 (STACK_1);

  len = (end-start) * (format/8);

  if (len < 0) {
    pushSTACK(make_sint32 (len));
    pushSTACK(TheSubr (subr_self)->name);
    fehler (error, "~S: How bogus! The effective length (~S) is negative.");
  }

  {
    struct seq_map sm;
    DYNAMIC_ARRAY (data, unsigned char, len ? len : 1);
    sm.transform = &STACK_0; sm.data = data; sm.format = format;
    map_sequence(STACK_6,coerce_into_map,(void*)&sm);

    X_CALL(XChangeProperty (dpy, win, property, type, format, mode, data,
                            (end-start)));

    FREE_DYNAMIC_ARRAY (data);
  }

  VALUES1(NIL);
  skipSTACK(9);
}


DEFUN(XLIB:DELETE-PROPERTY, arg1 arg2)  /* OK */
{
  Display *dpy;
  Window win = get_window_and_display (STACK_1, &dpy);
  Atom atom  = get_xatom_nointern (dpy, STACK_0);

  if (atom != None)
    X_CALL(XDeleteProperty (dpy, win, atom));

  VALUES1(NIL);
  skipSTACK(2);         /* all done */
}

/*   XLIB:GET-PROPERTY window property
        &key :type (:start 0) :end :delete-p (:result-type 'list) :transform
  returns:  data        -- Type sequence
            type        -- Type xatom
            format      -- Type (member 8 16 32)
            bytes-after -- Type card32 */
DEFUN(XLIB:GET-PROPERTY,window property                         \
      &key TYPE START END DELETE-P RESULT-TYPE TRANSFORM) /* OK */
{
  /* input: */
  Display *display;
  Window w;
  Atom property;
  long long_offset, long_length;
  Bool delete_p;
  Atom req_type;
  /* output: */
  Atom actual_type_return;
  int actual_format_return;
  unsigned long nitems_return;
  unsigned long bytes_after_return;
  unsigned char *prop_return = NULL;
  int r;

  w = get_xid_object_and_display (`XLIB::WINDOW`, STACK_7, &display);
  property = get_xatom (display, STACK_6);

  /* How is :start/:end counted?
   CLX counts the same way libX counts [This should be documented.] */
  long_offset = get_uint32_0 (STACK_4);
  long_length = (missingp(STACK_3) ? 0x7FFFFFFF : (get_uint32(STACK_3) - long_offset));
  delete_p = (missingp(STACK_2) ? 0 : 1);
  req_type = (missingp(STACK_5) ? AnyPropertyType : get_xatom (display, STACK_5));

  X_CALL(r = XGetWindowProperty (display, w, property,long_offset,long_length,
                                 delete_p, req_type,
                                 &actual_type_return, &actual_format_return,
                                 &nitems_return, &bytes_after_return,
                                 &prop_return));

  if (actual_type_return == None) {
    pushSTACK(NIL);
    pushSTACK(NIL);
    pushSTACK(fixnum(0));
    pushSTACK(fixnum(0));
  } else {
    if (req_type != AnyPropertyType && actual_type_return != req_type) {
      pushSTACK(NIL);
    } else {
      uintC i;
      gcv_object_t *transform_f = &(STACK_0);
      gcv_object_t *result_type_f = &(STACK_1);

      for (i = 0; i < nitems_return; i++) {
        if (boundp(*transform_f))
          pushSTACK(*transform_f); /* transform function .. */

        switch (actual_format_return) {
          case  8: pushSTACK(make_uint8  (prop_return[i])); break;
          case 16: pushSTACK(make_uint16 (((unsigned short*)prop_return)[i])); break;
          case 32: pushSTACK(make_uint32 (((unsigned long*) prop_return)[i])); break;
          default:
            NOTREACHED;
        }

        if (boundp(*transform_f)) {
          funcall (L(funcall), 2); /* apply the transform function */
          pushSTACK(value1);
        }
      }
      value1 = coerce_result_type(nitems_return,result_type_f);
      pushSTACK(value1);
    }

    if (prop_return)
      X_CALL(XFree (prop_return));

    pushSTACK(make_xatom(display, actual_type_return));
    pushSTACK(make_uint8 (actual_format_return));
    pushSTACK(make_uint32 (bytes_after_return));
  }
  value4 = popSTACK();
  value3 = popSTACK();
  value2 = popSTACK();
  value1 = popSTACK();
  mv_count = 4;
  skipSTACK(8);
}

DEFUN(XLIB:LIST-PROPERTIES, window &key RESULT-TYPE) /* OK */
{
  int num_props, i;
  gcv_object_t *res_type = &STACK_0;

  Display *dpy;
  Window win   = get_window_and_display (STACK_1, &dpy);
  Atom *props;

  X_CALL(props = XListProperties (dpy, win, &num_props));

  /* Now push all properties ... */
  for (i = 0; i < num_props; i++)
    pushSTACK(make_xatom(dpy, props[i]));

  if (props) X_CALL(XFree (props));

  VALUES1(coerce_result_type(num_props,res_type));
  skipSTACK(2);         /* all done */
}

/* convert a Lisp sequence of X Atoms to a C vector */
struct seq_xatom { Display *dpy; Atom* atom; };
void coerce_into_xatom (void *arg, object element);
void coerce_into_xatom (void *arg, object element) {
  struct seq_xatom *sa = (struct seq_xatom *)arg;
  *(sa->atom++) = get_xatom(sa->dpy,element);
}

DEFUN(XLIB:ROTATE-PROPERTIES, window properties &optional delta)
{ /* XLIB:ROTATE-PROPERTIES window properties &optional (delta 1) */
  Display *dpy;
  Window win   = get_window_and_display (STACK_2, &dpy);
  int delta    = (boundp(STACK_0) ? get_sint32 (STACK_0) : 1);
  int num_props = get_uint32(funcall1(L(length),STACK_1));
  struct seq_xatom sa;
  DYNAMIC_ARRAY (props, Atom, num_props);
  sa.dpy = dpy; sa.atom = props;
  map_sequence(STACK_1,coerce_into_xatom,(void*)&sa);

  X_CALL(XRotateWindowProperties (dpy, win, props, num_props, delta));

  FREE_DYNAMIC_ARRAY (props);
  VALUES1(NIL);
  skipSTACK(3);         /* all done */
}

/* 11.3  Selections */

DEFUN(XLIB:CONVERT-SELECTION, selection type requestor &optional property time)
{
  Display *dpy;
  Window requestor = get_window_and_display (STACK_2, &dpy);
  Atom target      = get_xatom (dpy, STACK_3);
  Atom selection   = get_xatom (dpy, STACK_4);
  Atom property    = missingp(STACK_1) ? None : get_xatom (dpy, STACK_1);
  Time time        = get_timestamp (STACK_0);

  X_CALL(XConvertSelection (dpy, selection, target, property,requestor,time));

  VALUES1(NIL);
  skipSTACK(5);         /* all done */
}

DEFUN(XLIB:SELECTION-OWNER, arg1 arg2)
{ /*  XLIB:SELECTION-OWNER display selection */
  Display *dpy = (pushSTACK(STACK_1), pop_display ());
  Atom selection = get_xatom (dpy, STACK_0);
  Window owner;

  X_CALL(owner = XGetSelectionOwner (dpy, selection));

  VALUES1(make_window (STACK_1, owner));
  skipSTACK(2);
}

/*  (SETF (XLIB:SELECTION-OWNER display selection &optional time) owner)
    == (XLIB:SET-SELECTION-OWNER owner display selection &optional time) */
DEFUN(XLIB:SET-SELECTION-OWNER, owner display selection &optional time)
{
  Window owner = get_window (STACK_3);
  Display *dpy = (pushSTACK(STACK_2), pop_display ());
  Atom selection = get_xatom (dpy, STACK_1);
  Time time = get_timestamp (STACK_0);

  X_CALL(XSetSelectionOwner (dpy, selection, owner, time));

  VALUES1(STACK_3);
  skipSTACK(4);
}


/* -----------------------------------------------------------------------
 *  Chapter 12  Events and Input
 * ----------------------------------------------------------------------- */

/* 12.3  Processing Events */

/*
  First of all, we have to enter all the nasty events, together with all its
  slots, type information, etc.  It is up to the two different functions for
  assembling and disassembling event to provide the right macro definitions for
  DEF_EVENT, ESLOT, and ESLOT2; [I want to do it only *ONCE*, so this klugde.]

    DEF_EVENT ( <lisp event key>, <C name of event key>, <C type of struct>, <C name of struct in XEvent> )
        -- start defining an event.

    ESLOT ( <lisp slot name>, <type>, <C slot> )
        -- define a slot

    ESLOT2 ( <lisp slot name>, <type>, <C slot> )
        -- same as ESLOT, but for objects, which needs the display.

    ESLOT3 is just for the key_vector, since you cannot assign arrays in C
           usually, but must pass a pointer.

    ESLOT4 is just used for the atom slot, since get_xatom requires an display
           argument

 (If your preprocessor or your compiler can't eat this, hmm... get a new one.)
 */

#define COMMON_INPUT_EVENT                                              \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT2(`:CHILD`,            window,                 subwindow)      \
    ESLOT2(`:ROOT`,             window,                 root)           \
    ESLOT (`:X`,                sint16,                 x)              \
    ESLOT (`:Y`,                sint16,                 y)              \
    ESLOT (`:ROOT-X`,           sint16,                 x_root)         \
    ESLOT (`:ROOT-Y`,           sint16,                 y_root)         \
    ESLOT (`:STATE`,            uint16,                 state)          \
    ESLOT (`:TIME`,             uint32,                 time)           \
    ESLOT (`:SAME-SCREEN-P`,    bool,                   same_screen)

#define ALL_EVENT_DEFS                                                  \
  DEF_EVENT (`:KEY-PRESS`, KeyPress, XKeyPressedEvent, xkey)            \
    ESLOT (`:CODE`,             uint8,                  keycode)        \
    COMMON_INPUT_EVENT                                                  \
                                                                        \
  DEF_EVENT (`:KEY-RELEASE`, KeyRelease, XKeyReleasedEvent, xkey)       \
    ESLOT (`:CODE`,             uint8,                  keycode)        \
    COMMON_INPUT_EVENT                                                  \
                                                                        \
  DEF_EVENT (`:BUTTON-PRESS`, ButtonPress, XButtonPressedEvent, xbutton) \
    ESLOT (`:CODE`,             uint8,                  button)         \
    COMMON_INPUT_EVENT                                                  \
                                                                        \
  DEF_EVENT (`:BUTTON-RELEASE`, ButtonRelease, XButtonReleasedEvent, xbutton) \
    ESLOT (`:CODE`,             uint8,                  button)         \
    COMMON_INPUT_EVENT                                                  \
                                                                        \
  DEF_EVENT (`:MOTION-NOTIFY`, MotionNotify, XMotionEvent, xmotion)     \
    ESLOT (`:HINT-P`,           bool,                   is_hint)        \
    COMMON_INPUT_EVENT                                                  \
                                                                        \
  DEF_EVENT (`:ENTER-NOTIFY`, EnterNotify, XEnterWindowEvent, xcrossing) \
    ESLOT (`:MODE`,             crossing_mode,          mode)           \
    ESLOT (`:KIND`,             crossing_kind,          detail)         \
    ESLOT (`:FOCUS-P`,          bool,                   focus)          \
    COMMON_INPUT_EVENT                                                  \
                                                                        \
  DEF_EVENT (`:LEAVE-NOTIFY`, LeaveNotify, XLeaveWindowEvent, xcrossing) \
    ESLOT (`:MODE`,             crossing_mode,          mode)           \
    ESLOT (`:KIND`,             crossing_kind,          detail)         \
    ESLOT (`:FOCUS-P`,          bool,                   focus)          \
    COMMON_INPUT_EVENT                                                  \
                                                                        \
  DEF_EVENT (`:FOCUS-IN`, FocusIn, XFocusChangeEvent, xfocus)           \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:MODE`,             focus_mode,             mode)           \
    ESLOT (`:KIND`,             focus_detail,           detail)         \
                                                                        \
  DEF_EVENT (`:FOCUS-OUT`, FocusOut, XFocusChangeEvent, xfocus)         \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:MODE`,             focus_mode,             mode)           \
    ESLOT (`:KIND`,             focus_detail,           detail)         \
                                                                        \
  DEF_EVENT (`:EXPOSURE`, Expose, XExposeEvent, xexpose)                \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:X`,                sint16,                 x)              \
    ESLOT (`:Y`,                sint16,                 y)              \
    ESLOT (`:WIDTH`,            sint16,                 width)          \
    ESLOT (`:HEIGHT`,           sint16,                 height)         \
    ESLOT (`:COUNT`,            uint16,                 count)          \
                                                                        \
  DEF_EVENT (`:GRAPHICS-EXPOSURE`, GraphicsExpose, XGraphicsExposeEvent, xgraphicsexpose) \
    ESLOT2(`:DRAWABLE`,         drawable,               drawable)       \
    ESLOT (`:X`,                sint16,                 x)              \
    ESLOT (`:Y`,                sint16,                 y)              \
    ESLOT (`:WIDTH`,            sint16,                 width)          \
    ESLOT (`:HEIGHT`,           sint16,                 height)         \
    ESLOT (`:COUNT`,            uint16,                 count)          \
    ESLOT (`:MAJOR`,            uint8,                  major_code)     \
    ESLOT (`:MINOR`,            uint16,                 minor_code)     \
                                                                        \
  DEF_EVENT (`:KEYMAP-NOTIFY`, KeymapNotify, XKeymapEvent, xkeymap)     \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT3(`:KEYMAP`,           key_vector,             key_vector)     \
                                                                        \
  DEF_EVENT (`:MAPPING-NOTIFY`, MappingNotify, XMappingEvent, xmapping) \
    ESLOT (`:COUNT`,            uint8,                  count)          \
    ESLOT (`:START`,            uint8,                  first_keycode)  \
    ESLOT (`:REQUEST`,          mapping_request,        request)        \
                                                                        \
  DEF_EVENT (`:NO-EXPOSURE`, NoExpose, XNoExposeEvent, xnoexpose)       \
    ESLOT2(`:DRAWABLE`,         drawable,               drawable)       \
    ESLOT (`:MAJOR`,            uint8,                  major_code)     \
    ESLOT (`:MINOR`,            uint16,                 minor_code)     \
                                                                        \
  DEF_EVENT (`:CIRCULATE-NOTIFY`, CirculateNotify, XCirculateEvent, xcirculate) \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:PLACE`,            top_or_bottom,          place)          \
                                                                        \
  DEF_EVENT (`:CONFIGURE-NOTIFY`, ConfigureNotify, XConfigureEvent, xconfigure) \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:X`,                uint16,                 x)              \
    ESLOT (`:Y`,                uint16,                 y)              \
    ESLOT (`:WIDTH`,            uint16,                 width)          \
    ESLOT (`:HEIGHT`,           uint16,                 height)         \
    ESLOT (`:BORDER-WIDTH`,     uint16,                 border_width)   \
    ESLOT2(`:ABOVE-SIBLING`,    window,                 above)          \
    ESLOT (`:OVERRIDE-REDIRECT-P`,bool,                 override_redirect) \
                                                                        \
  DEF_EVENT (`:CREATE-NOTIFY`, CreateNotify, XCreateWindowEvent, xcreatewindow) \
    ESLOT2(`:PARENT`,           window,                 parent)         \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:X`,                uint16,                 x)              \
    ESLOT (`:Y`,                uint16,                 y)              \
    ESLOT (`:WIDTH`,            uint16,                 width)          \
    ESLOT (`:HEIGHT`,           uint16,                 height)         \
    ESLOT (`:BORDER-WIDTH`,     uint16,                 border_width)   \
                                                                        \
  DEF_EVENT (`:DESTROY-NOTIFY`, DestroyNotify, XDestroyWindowEvent, xdestroywindow) \
    ESLOT2(`:WINDOW`,           window,                 window)         \
                                                                        \
  DEF_EVENT (`:GRAVITY-NOTIFY`, GravityNotify, XGravityEvent, xgravity) \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:X`,                uint16,                 x)              \
    ESLOT (`:Y`,                uint16,                 y)              \
                                                                        \
  DEF_EVENT (`:MAP-NOTIFY`, MapNotify, XMapEvent, xmap)                 \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:OVERRIDE-REDIRECT-P`,bool,                 override_redirect) \
                                                                        \
  DEF_EVENT (`:REPARENT-NOTIFY`, ReparentNotify, XReparentEvent, xreparent) \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT2(`:PARENT`,           window,                 parent)         \
    ESLOT (`:X`,                uint16,                 x)              \
    ESLOT (`:Y`,                uint16,                 y)              \
                                                                        \
  DEF_EVENT (`:UNMAP-NOTIFY`, UnmapNotify, XUnmapEvent, xunmap)         \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:CONFIGURE-P`,      bool,                   from_configure) \
                                                                        \
  DEF_EVENT (`:VISIBILITY-NOTIFY`, VisibilityNotify, XVisibilityEvent, xvisibility) \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:STATE`,            visibility_state,       state)          \
                                                                        \
  DEF_EVENT (`:CIRCULATE-REQUEST`, CirculateRequest, XCirculateRequestEvent, xcirculaterequest) \
    ESLOT2(`:PARENT`,           window,                 parent)         \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:PLACE`,            top_or_bottom,          place)          \
                                                                        \
  DEF_EVENT (`:COLORMAP-NOTIFY`, ColormapNotify, XColormapEvent, xcolormap) \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT2(`:COLORMAP`,         colormap,               colormap)       \
    ESLOT (`:NEW-P`,            bool,                   c_new)          \
    ESLOT (`:INSTALLED-P`,      bool,                   state)          \
                                                                        \
  DEF_EVENT (`:CONFIGURE-REQUEST`, ConfigureRequest, XConfigureRequestEvent, xconfigurerequest) \
    ESLOT2(`:PARENT`,           window,                 parent)         \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:X`,                uint16,                 x)              \
    ESLOT (`:Y`,                uint16,                 y)              \
    ESLOT (`:WIDTH`,            uint16,                 width)          \
    ESLOT (`:HEIGHT`,           uint16,                 height)         \
    ESLOT (`:BORDER-WIDTH`,     uint16,                 border_width)   \
    ESLOT (`:STACK-MODE`,       stack_mode,             detail)         \
    ESLOT2(`:ABOVE-SIBLING`,    window,                 above)          \
    ESLOT (`:VALUE-MASK`,       uint16,                 value_mask)     \
                                                                        \
  DEF_EVENT (`:MAP-REQUEST`, MapRequest, XMapRequestEvent, xmaprequest) \
    ESLOT2(`:PARENT`,           window,                 parent)         \
    ESLOT2(`:WINDOW`,           window,                 window)         \
                                                                        \
  DEF_EVENT (`:RESIZE-REQUEST`, ResizeRequest, XResizeRequestEvent, xresizerequest) \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT (`:WIDTH`,            uint16,                 width)          \
    ESLOT (`:HEIGHT`,           uint16,                 height)         \
                                                                        \
  DEF_EVENT (`:CLIENT-MESSAGE`, ClientMessage, XClientMessageEvent, xclient) \
    /* FIXME missing... */                                              \
                                                                        \
  DEF_EVENT (`:PROPERTY-NOTIFY`, PropertyNotify, XPropertyEvent, xproperty) \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT4(`:ATOM`,             xatom,                  atom)           \
    ESLOT (`:STATE`,            new_value_or_deleted,   state)          \
    ESLOT (`:TIME`,             uint32,                 time)           \
                                                                        \
  DEF_EVENT (`:SELECTION-CLEAR`, SelectionClear, XSelectionClearEvent, xselectionclear) \
    ESLOT2(`:WINDOW`,           window,                 window)         \
    ESLOT4(`:SELECTION`,        xatom,                  selection)      \
    ESLOT (`:TIME`,             uint32,                 time)           \
                                                                        \
  DEF_EVENT (`:SELECTION-NOTIFY`, SelectionNotify, XSelectionEvent, xselection) \
    ESLOT2(`:WINDOW`,           window,                 requestor)      \
    ESLOT4(`:SELECTION`,        xatom,                  selection)      \
    ESLOT4(`:TARGET`,           xatom,                  target)         \
    ESLOT4(`:PROPERTY`,         xatom,                  property)       \
    ESLOT (`:TIME`,             uint32,                 time)           \
                                                                        \
  DEF_EVENT (`:SELECTION-REQUEST`, SelectionRequest, XSelectionRequestEvent, xselectionrequest) \
    ESLOT2(`:REQUESTOR`,        window,                 requestor)      \
    ESLOT4(`:SELECTION`,        xatom,                  selection)      \
    ESLOT4(`:TARGET`,           xatom,                  target)         \
    ESLOT4(`:PROPERTY`,         xatom,                  property)       \
    ESLOT (`:TIME`,             uint32,                 time)           \
  /* vacuous comment to signify the end of the #define */
/* Some field names differ between C and C++. */
#ifndef __cplusplus
  #define c_new new
#endif

static int disassemble_event_on_stack (XEvent *ev, gcv_object_t *dpy_objf)
/* Disassembles an X event onto the stack and returns the number of elements
 * push to the stack. [You can then neatly issue a funcall or list call using
 * these stack elements.] */
{
#define ESLOT(lispname,type,cslot)                                      \
        pushSTACK((lispname));                                          \
        pushSTACK(make_##type (container->cslot));                      \
        cnt += 2;

#define ESLOT2(lispname,type,cslot)                                     \
        pushSTACK((lispname));                                          \
        pushSTACK(make_##type (*dpy_objf, container->cslot));           \
        cnt += 2;

#define ESLOT3 ESLOT

#define ESLOT4(lispname,type,cslot)                                     \
        pushSTACK((lispname));                                          \
        {                                                               \
          Display *dpy = (pushSTACK(*dpy_objf), pop_display());         \
          pushSTACK(make_##type (dpy, container->cslot));               \
        }                                                               \
        cnt += 2;

#define DEF_EVENT(lispname, cname, c_container_type, c_container)       \
   }                                                                    \
 break;                                                                 \
 case cname:                                                            \
   {                                                                    \
     c_container_type *container = &(ev->c_container);                  \
     pushSTACK(`:EVENT-KEY`);                                           \
     pushSTACK((lispname)); cnt += 2;

  int cnt = 0;

  /* These attributes are common to all events (hopefully) */
  pushSTACK(`:DISPLAY`); pushSTACK(STACK_6); cnt += 2;
  pushSTACK(`:EVENT-CODE`); pushSTACK(fixnum(ev->type)); cnt += 2;
  pushSTACK(`:SEND-EVENT-P`); pushSTACK(make_bool (ev->xany.send_event)); cnt += 2;
  pushSTACK(`:EVENT-WINDOW`); pushSTACK(make_window (*dpy_objf, ev->xany.window)); cnt += 2;

  /* BTW I really hate it that the naming convention for events is not
   * consistent , while you have a name for the mask, the event type, the event
   * substructure and all may have different names. (i.e. you have to say
   * 'exposure' and sometimes 'expose') This bothers me really .. :-{}^ */

  switch (ev->type) {
    default: {
        /* Wat nu?       Propabably raise some error?! */

        /* THIS LOOKS STRANGE?!
         Well, the first ALL_EVENTS gives is '}' + 'break;' the last thing is '{', so ... */

        ALL_EVENT_DEFS
      }
      break;

    }
  return cnt;

#undef DEF_EVENT
#undef ESLOT
#undef ESLOT2
#undef ESLOT3
#undef ESLOT4
}

static void travel_queque (Display *dpy, int peek_p, int discard_p,
                           int force_output_p, struct timeval *timeout)
{ /* peek_p == not remove-processed-p
  discard_p == remove-unprocessed-p
  timeout in sec/usec or NULL to block.
 BUGS:
  - take care that discard-current-event will work as expected!
  - also we need an unwind protect here!
  - handler may also be a vector of functions. [How strange?!]
 TODO:
  - I want this routine to be interruptible by user in a continueable fashion.
 Way to go:
  interruptp( { pushSTACK( <subr name> ); tast_break(); goto <continue>; } );
 Hmm... It may be better to use the appropriate XIf... function.
 [What happens if we throw out of `em? Also they also seem to block?! RTFM] */
  XEvent ev;
  int cnt;
  int r;

 travel_queque:

  if (timeout) {
    X_CALL(XEventsQueued(dpy, force_output_p
                         ? QueuedAfterFlush : QueuedAfterReading));
    r = QLength (dpy);

    if (r == 0) {
      int conn;
      fd_set ifds;

      conn = ConnectionNumber (dpy); /* this is the fd. */
      FD_ZERO (&ifds);
      FD_SET (conn, &ifds);
      X_CALL(r = select (conn+1, &ifds, NULL, NULL, timeout));

      if ((r > 0) && FD_ISSET (conn, &ifds)) {
        /* timeout has to reduce by amount waited here for input;
           -- presumably select does that */
      } else {
        /* Nothing there, so just return */
        VALUES1(NIL);
        return;
      }
    }
  }

  /* .. so there is either now an event in the queue or we should hang: */
  X_CALL(XPeekEvent (dpy, &ev));

  cnt = disassemble_event_on_stack (&ev, &(STACK_5));
  /* Now invoke the handler function */
  funcall (STACK_(cnt+4), cnt); /* BUG: This may throw out of our control! */
  /*      We would need something like an unwind protect here
          But only if discard_p == NIL. */


  /* FIXME: We should probably check here, if somebody has already
     discarded this event? */
  X_CALL(XNextEvent (dpy, &ev));

  /* Look what we got. */
  if (nullp(value1)) {
    if (discard_p) {
      /* travel_queque (dpy, peek_p, discard_p, force_output_p, timeout); */
      goto travel_queque;
    } else {
      travel_queque (dpy, peek_p, discard_p, force_output_p, timeout);
      X_CALL(XPutBackEvent (dpy, &ev));
    }
  } else /* Handler successful */
    if (peek_p) X_CALL(XPutBackEvent (dpy, &ev));
}

DEFUN(XLIB:PROCESS-EVENT, display &key HANDLER TIMEOUT PEEK-P DISCARD-P \
      FORCE-OUTPUT-P)
{
  Display *dpy = (pushSTACK(STACK_5), pop_display());
  int force_output_p = (boundp(STACK_0) ? get_bool(STACK_0) : 1);
  int discard_p = !missingp(STACK_1), peek_p = !missingp(STACK_2);
  struct timeval tv;
  struct timeval *timeout = sec_usec(STACK_3,NIL,&tv);

  if (!boundp(STACK_4))
    NOTIMPLEMENTED;

  /* Now go into the recursive event queque travel routine */

  travel_queque (dpy, peek_p, discard_p, force_output_p, timeout);

  /* mv_space and mv_count are set by the return of the handler function */

  skipSTACK(6);
}


/* 12.4  Managing the Event Queue */

static int grasp (object slot, uintC n) {
  uintC o;
  for (o = 1 ; o < n; o += 2)
    if (eq (STACK_(o+1), slot))
      return o;
  return 0;
}

static void encode_event (uintC n, object event_key, Display *dpy, XEvent *ev)
{ /* encodes an event, which lies in the top /n/ stack locations into ev
 event-key is an optional event key to use, it may also be unbound.
 But hey! Without an event key we could not assemble an event?! */
  int ofs;

  pushSTACK(event_key);

#define DEF_EVENT(lnam, cnam, ctype, cslot)             \
  } else if (eq (STACK_0, lnam)) {                      \
    ctype *event = &(ev->cslot);

#define ESLOT(lnam, type, cslot)                        \
    {                                                   \
      if ((ofs = grasp (lnam, n)))                      \
        event->cslot = get_##type (STACK_(ofs));        \
      else                                              \
        event->cslot = 0;                               \
    }

#define ESLOT2(lnam, type, cslot) ESLOT(lnam,type,cslot)
#define ESLOT3(lnam, type, cslot)                       \
    {                                                   \
      if ((ofs = grasp (lnam, n)))                      \
        get_##type (STACK_(ofs), (event->cslot));       \
      else                                              \
        { /* ??? */ }                                   \
    }

#define ESLOT4(lnam, type, cslot)                       \
    {                                                   \
      if ((ofs = grasp (lnam, n)))                      \
        event->cslot = get_##type (dpy, STACK_(ofs));   \
      else                                              \
        event->cslot = 0;                               \
    }

  if(0) {
    /* Same as above in disassemble_event_on_stack this looks strange, but is
     right, since the first thing DEF_EVENT gives is "} else" the last
     thing is "if (..) {", so .... */

    ALL_EVENT_DEFS
  } else my_type_error(`XLIB::EVENT-KEY`,STACK_0);


#undef DEF_EVENT
#undef ESLOT
#undef ESLOT2
#undef ESLOT3
#undef ESLOT4
}

/* (queue-event display event-key &rest args &key append-p send-event-p
                &allow-other-keys)
 The event is put at the head of the queue if append-p is nil, else
 the tail.  Additional arguments depend on event-key, and are as
 specified above with declare-event, except that both resource-ids and
 resource objects are accepted in the event components. */
DEFUN(XLIB:QUEUE-EVENT, &rest args)
{UNDEFINED;}
/* Take a look at XPutBackEvent, but that functions seems only to
 put events on the head of the queue!
 Maybe we should go and build our own event queque?
 Or we fight with the internals of libX?
 But we could travel the whole event queque until we come to a point,
 where the queque has ended; XPutBackEvent the event to be added at end
 and XputBack all other event above that. [Not very fast, but portable]
 also send-event-p is not in the manual. */


/*  -->   discarded-p -- Type boolean
   Discard the current event for DISPLAY.
   Returns NIL when the event queue is empty, else T.
   To ensure events aren't ignored, application code should only call
   this when throwing out of event-case or process-next-event, or from
   inside even-case, event-cond or process-event when :peek-p is T and
   :discard-p is NIL. */
DEFUN(XLIB:DISCARD-CURRENT-EVENT, display)
{ /* FIXME -- here the manual is bit unpreciese
      - Should we hang?
      - Should we return T/NIL before discarding? properly not. */
  Display *dpy = pop_display ();

  if (QLength (dpy)) {   /* no begin/end_call here QLength is a macro */
    XEvent trash_can;
    X_CALL(XNextEvent (dpy, &trash_can));
    value1 = T;
  } else
    value1 = NIL;
  mv_count = 1;
}

/*  XLIB:EVENT-LISTEN display &optional (timeout 0)

  Returns:
     event-count -- Type (or null integer).

  Returns the number of events queued locally. If the event queue is
  empty, event-listen waits for an event to arrive. If timeout is
  non-nil and no event arrives within the specified timeout interval
  (given in seconds), event-listen returns nil; if timeout is nil,
  event-listen will not return until an event arrives. */
DEFUN(XLIB:EVENT-LISTEN, display &optional timeout)
{
  struct timeval tv;
  struct timeval *timeout = sec_usec(popSTACK(),NIL,&tv);
  Display *dpy = pop_display();
  int r;
  XEvent trashcan;

  if (timeout == NULL) { /* Block */
    X_CALL(while (!(r = QLength (dpy))) XPeekEvent (dpy, &trashcan));
    value1 = make_uint32 (r);
  } else {
    r = QLength (dpy);

    if (r) {
      value1 = make_uint32 (r);
    } else {
      /* Wait */
      int conn;
      fd_set ifds;

      conn = ConnectionNumber (dpy); /* this is the fd. */
      FD_ZERO (&ifds);
      FD_SET (conn, &ifds);
      X_CALL(r = select (conn+1, &ifds, NULL, NULL, timeout));
      if ((r > 0) && FD_ISSET (conn, &ifds)) {
        /* RTFS: To flush or not to flush is here the question! */
        X_CALL(r = XEventsQueued (dpy, QueuedAfterReading));
        value1 = make_uint32 (r);
      } else {
        value1 = NIL;
      }
    }
  }
  mv_count = 1;
}

/* 12.5  Sending Events */
/*   XLIB:SEND-EVENT window event-key event-mask &rest event-slots
        &key  propagate-p &allow-other-keys

  NOTE: The MIT-CLX interface specifies a :display argument here, which
  is not necessary */
DEFUN(XLIB:SEND-EVENT, &rest args)
{
  if (argcount < 3) goto too_few;
  {
    XEvent event;
    Display             *dpy;
    Window            window = get_window_and_display (STACK_(argcount-1), &dpy);
    unsigned long event_mask = get_event_mask (STACK_(argcount-3));
    int propagate_p = 0;
    uintC i;

    /* hunt for the :propagate-p */
    for (i = 0; i < argcount; i += 2)
      if (eq (STACK_(i+1), `:PROPAGATE-P`)) {
        propagate_p = get_bool (STACK_(i));
        break;
      }

    encode_event (argcount-3, STACK_(argcount-2), dpy, &event);
    X_CALL(XSendEvent (dpy, window, propagate_p, event_mask, &event));
    /* XSendEvent returns also some Status, should we interpret it?
     If yes: How?! */
    skipSTACK(argcount);
    VALUES1(NIL);
    return;
  }

 too_few:
  NOTIMPLEMENTED;
}

/* 12.6  Pointer Position */

/* XLIB:QUERY-POINTER window
  -> [1] x
     [2] y
     [3] same-screen-p
     [4] child
     [5] state-mask
     [6] root-x
     [8] root-y
     [9] root */
DEFUN(XLIB:QUERY-POINTER, window)
{
  Display *dpy;
  Window   win = get_window_and_display (STACK_0, &dpy);

  Window root, child;
  int root_x, root_y;
  int win_x, win_y;
  unsigned int mask;
  Bool same_screen_p;

  X_CALL(same_screen_p = XQueryPointer (dpy, win, &root, &child, &root_x,
                                        &root_y, &win_x, &win_y, &mask));

  pushSTACK(get_display_obj (STACK_0));
  pushSTACK(make_window (STACK_0, root));
  pushSTACK(make_window (STACK_1, child));

  value1 = make_sint16 (win_x);
  value2 = make_sint16 (win_y);
  value3 = make_bool (same_screen_p);
  value4 = popSTACK();         /* child */
  value5 = make_uint16 (mask);
  value6 = make_sint16 (root_x);
  value7 = make_sint16 (root_y);
  value8 = popSTACK();         /* root */
  mv_count = 8;
  skipSTACK(2);                /* all done */
}

/* -> [1] root-x
      [2] root-y
      [3] root */
DEFUN(XLIB:GLOBAL-POINTER-POSITION, display)
{
  Display *dpy = (pushSTACK(STACK_0), pop_display());
  Window   win;

  Window root, child;
  int root_x, root_y;
  int win_x, win_y;
  unsigned int mask;
  Bool same_screen_p;

  X_CALL(same_screen_p = XQueryPointer
         (dpy, DefaultRootWindow (dpy), &root, &child, &root_x, &root_y,
          &win_x, &win_y, &mask));

  VALUES3(make_sint16(root_x),make_sint16(root_y),make_window(STACK_0,root));
  skipSTACK(1);
}

/* -> [1] x
      [2] y
      [3] same-screen-p
      [4] child */
DEFUN(XLIB:POINTER-POSITION, window)
{
  funcall(``XLIB::QUERY-POINTER``,1);
  mv_count = 4;
}

DEFUN(XLIB:MOTION-EVENTS, window &key START STOP RESULT-TYPE)
{ /* -> (repeat-seq (int16 x) (int16 y) (timestamp time))  */
  Display *dpy;
  Window win = get_window_and_display (STACK_3, &dpy);
  Time start = get_timestamp (STACK_2);
  Time stop = get_timestamp (STACK_1);
  XTimeCoord *events = 0;
  int nevents = 0;
  gcv_object_t *res_type = &STACK_0;

  X_CALL(events = XGetMotionEvents (dpy, win, start, stop, &nevents));

  if (events) {
    int i;
    for (i = 0; i < nevents; i++) {
      pushSTACK(make_sint16 (events[i].x));
      pushSTACK(make_sint16 (events[i].y));
      pushSTACK(make_uint32 (events[i].time));
    }
    /* XXX RTFS: Should I XFree on events? */
  } else
    value1 = NIL;

  VALUES1(coerce_result_type(3*nevents,res_type));
  skipSTACK(4);
}

DEFUN(XLIB:WARP-POINTER, destination x y)
{
  int y = get_sint32 (popSTACK());
  int x = get_sint32 (popSTACK());
  Display *dpy;
  Window dest = get_window_and_display (popSTACK(), &dpy);

  X_CALL(XWarpPointer (dpy, None, dest, 0, 0, 0, 0, x, y));

  VALUES1(NIL);
}

DEFUN(XLIB:WARP-POINTER-RELATIVE, display delta-x delta-y)
{
  int dy = get_sint32 (popSTACK());
  int dx = get_sint32 (popSTACK());
  Display *dpy = pop_display ();

  X_CALL(XWarpPointer (dpy, None, None, 0, 0, 0, 0, dx, dy));

  VALUES1(NIL);
}

/*  XLIB:WARP-POINTER-IF-INSIDE destination destination-x destination-y
                             source source-x source-y
                             &optional (source-width 0) (source-height 0) */
DEFUN(XLIB:WARP-POINTER-IF-INSIDE, destination destination-x destination-y \
      source source-x source-y &optional source-width source-height)
{
  int src_h = get_sint16_0(popSTACK());
  int src_w = get_sint16_0(popSTACK());
  int src_y = get_sint16 (popSTACK());
  int src_x = get_sint16 (popSTACK());
  Window src = get_window (popSTACK());
  int dest_y = get_sint16 (popSTACK());
  int dest_x = get_sint16 (popSTACK());
  Display *dpy;
  Window dest = get_window_and_display (popSTACK(), &dpy);

  X_CALL(XWarpPointer(dpy,src,dest,src_x,src_y,src_w,src_h,dest_x,dest_y));

  VALUES1(NIL);
}

/*  XLIB:WARP-POINTER-RELATIVE-IF-INSIDE x-offset y-offset source
       source-x source-y &optional (source-width 0) (source-height 0) */
DEFUN(XLIB:WARP-POINTER-RELATIVE-IF-INSIDE, x-offset y-offset source \
      source-x source-y &optional source-width source-height)
{
  int src_h = get_sint16_0(popSTACK());
  int src_w = get_sint16_0(popSTACK());
  int src_y = get_sint16 (popSTACK());
  int src_x = get_sint16 (popSTACK());
  Display *dpy;
  Window src = get_window_and_display (popSTACK(), &dpy);
  int y_off = get_sint16 (popSTACK());
  int x_off = get_sint16 (popSTACK());

  X_CALL(XWarpPointer(dpy,src,None,src_x,src_y,src_w,src_h,x_off,y_off));

  VALUES1(NIL);
}

/* 12.7  Managing Input Focus */

DEFCHECKER(check_revert_focus,default=RevertToNone, NONE=RevertToNone   \
           POINTER-ROOT=RevertToPointerRoot PARENT=RevertToParent)

/* btw. why not (SETF INPUT-FOCUS) ?
 FIXME (RTFS): focus and revert-to are actually swapped in manual. */
DEFUN(XLIB:SET-INPUT-FOCUS , dpy focus revert-to &optional time1)
{
  Time time = get_timestamp (popSTACK());
  int revert_to = check_revert_focus(popSTACK());
  Window focus = get_window(popSTACK());
  Display *dpy = pop_display();

  X_CALL(XSetInputFocus (dpy, focus, revert_to, time));

  VALUES1(NIL);
}

/*   -->
  focus     -- Type (or window (member :none :pointer-root))
  revert-to -- Type (or window (member :none :pointer-root :parent))

 In the manual is said, that the revert-to could be a window, but
 the libX11 function just returns a state ?! */
DEFUN(XLIB:INPUT-FOCUS, display)
{
  Display *dpy = (pushSTACK(STACK_0), pop_display());
  Window focus;
  int revert;

  X_CALL(XGetInputFocus (dpy, &focus, &revert));

  /* value1 (= focus) */
  switch (focus) {
    case PointerRoot: pushSTACK(`:POINTER-ROOT`); break;
    case None:        pushSTACK(`:NONE`); break;
    default:          pushSTACK(make_window (STACK_0, focus));
  }

  /* value2 (= revert) */
  pushSTACK(check_revert_focus_reverse(revert));

  value2 = popSTACK();
  value1 = popSTACK();
  mv_count = 2;
}

static void ungrab_X (int (*X)(Display *dpy, Time time))
{
  Time    time = get_timestamp (popSTACK());
  Display *dpy = pop_display ();
  X_CALL(X (dpy, time));
  VALUES1(NIL);
  skipSTACK(2);
}

static inline object grab_to_object (int gr) {
  switch (gr) {
    case AlreadyGrabbed:  return `:ALREADY-GRABBED`;
    case GrabFrozen:      return `:FROZEN`;
    case GrabInvalidTime: return `:INVALID-TIME`;
    case GrabNotViewable: return `:NOT-VIEWABLE`; /* NIM */
    default:              return `:SUCCESS`;
  }
}

DEFUN(XLIB:GRAB-POINTER, window event-mask &key OWNER-P SYNC-POINTER-P \
      SYNC-KEYBOARD-P CONFINE-TO CURSOR TIME)
{
  Display             *dpy;
  Window               win = get_window_and_display (STACK_7, &dpy);
  unsigned long event_mask = get_event_mask (STACK_6);
  Bool             owner_p = !missingp(STACK_5);
  Bool        sync_pointer = missingp(STACK_4);
  Bool       sync_keyboard = missingp(STACK_3);
  Window        confine_to = boundp(STACK_2) ? get_window(STACK_2) : None;
  Cursor            cursor = boundp(STACK_1) ? get_cursor(STACK_1) : None;
  Time                time = get_timestamp (STACK_0);
  int r;

  X_CALL(r = XGrabPointer (dpy, win, owner_p, event_mask, sync_pointer,
                           sync_keyboard, confine_to, cursor, time));

  VALUES1(grab_to_object(r));
  skipSTACK(8);
}

DEFUN(XLIB:UNGRAB-POINTER, window &key TIME)
{ ungrab_X (XUngrabPointer); }

DEFUN(XLIB:CHANGE-ACTIVE-POINTER-GRAB, dpy event-mask &optional cursor time)
{
  Display *dpy = (pushSTACK(STACK_3), pop_display ());
  unsigned long event_mask = get_event_mask (STACK_2);
  Cursor            cursor = boundp(STACK_1) ? get_cursor(STACK_1) : None;
  Time                time = get_timestamp (STACK_0);

  X_CALL(XChangeActivePointerGrab (dpy, event_mask, cursor, time));

  skipSTACK(4);
  VALUES1(NIL);
}

/* 12.9  Grabbing a Button */
/*   XLIB:GRAB-BUTTON window button event-mask &key (:modifiers 0)
        :owner-p :sync-pointer-p :sync-keyboard-p :confine-to :cursor */
DEFUN(XLIB:GRAB-BUTTON, window button event-mask &key MODIFIERS \
      OWNER-P SYNC-POINTER-P SYNC-KEYBOARD-P CONFINE-TO CURSOR)
{
  Display             *dpy;
  Window               win = get_window_and_display (STACK_8, &dpy);
  int               button = !(eq (STACK_7, `:ANY`) ? AnyButton : get_uint8 (STACK_7));
  unsigned long event_mask = get_event_mask (STACK_6);
  unsigned int   modifiers = get_modifier_mask(STACK_5);
  Bool             owner_p = !missingp(STACK_4);
  Bool        sync_pointer = missingp(STACK_3);
  Bool       sync_keyboard = missingp(STACK_2);
  Window        confine_to = boundp(STACK_1) ? get_window(STACK_1) : None;
  Cursor            cursor = boundp(STACK_0) ? get_cursor(STACK_0) : None;

  X_CALL(XGrabButton (dpy, button, modifiers, win, owner_p, event_mask,
                      sync_pointer, sync_keyboard, confine_to, cursor));

  VALUES1(NIL);
  skipSTACK(9);
}

DEFUN(XLIB:UNGRAB-BUTTON, window code &key MODIFIERS)
{
  Display           *dpy;
  Window             win = get_window_and_display (STACK_2, &dpy);
  int               code = (eq (STACK_1, `:ANY`) ? AnyKey : get_uint8(STACK_1));
  unsigned int modifiers = get_modifier_mask(STACK_0);

  X_CALL(XUngrabButton (dpy, code, modifiers, win));

  VALUES1(NIL);
  skipSTACK(3);
}

/* 12.10  Grabbing the Keyboard */
DEFUN(XLIB:GRAB-KEYBOARD, window \
      &key OWNER-P SYNC-POINTER-P SYNC-KEYBOARD-P TIME)
{
  Display         *dpy;
  Window           win = get_window_and_display (STACK_4, &dpy);
  Bool         owner_p = !missingp(STACK_3);
  Bool  sync_pointer_p = missingp(STACK_2) ? GrabModeAsync : GrabModeSync;
  Bool sync_keyboard_p = missingp(STACK_1) ? GrabModeAsync : GrabModeSync;
  Time            time = get_timestamp (STACK_0);
  int r;

  X_CALL(r = XGrabKeyboard (dpy, win, owner_p, sync_pointer_p, sync_keyboard_p,
                            time));
  VALUES1(grab_to_object(r));
  skipSTACK(5);
}

DEFUN(XLIB:UNGRAB-KEYBOARD, window &key TIME)
{ ungrab_X (XUngrabKeyboard); }

/* 12.11  Grabbing a Key */
/*   XLIB:GRAB-KEY window key &key (:modifiers 0) :owner-p :sync-pointer-p
         :sync-keyboard-p */
DEFUN(XLIB:GRAB-KEY, window key &key MODIFIERS OWNER-P SYNC-POINTER-P \
      SYNC-KEYBOARD-P)
{
  Display           *dpy;
  Window             win = get_window_and_display (STACK_5, &dpy);
  int            keycode = get_uint8 (STACK_4);
  unsigned int modifiers = get_modifier_mask(STACK_3);
  Bool           owner_p = !missingp(STACK_2);
  Bool    sync_pointer_p = missingp(STACK_1) ? GrabModeAsync : GrabModeSync;
  Bool   sync_keyboard_p = missingp(STACK_0) ? GrabModeAsync : GrabModeSync;

  X_CALL(XGrabKey (dpy, keycode, modifiers, win, owner_p,
                   sync_keyboard_p, sync_keyboard_p));

  VALUES1(NIL);
  skipSTACK(6);
}

DEFUN(XLIB:UNGRAB-KEY, window code &key MODIFIERS)
{
  Display *dpy;
  Window  win = get_window_and_display (STACK_2, &dpy);
  int    code = (eq (STACK_1, `:ANY`) ? AnyKey : get_uint8(STACK_1));
  unsigned int modifiers = get_modifier_mask(STACK_0);

  X_CALL(XUngrabKey (dpy, code, modifiers, win));

  VALUES1(NIL);
  skipSTACK(3);
}

/* 12.13  Releasing Queued Events */
DEFCHECKER(check_allow_events,default=,ASYNC-POINTER=AsyncPointer       \
           SYNC-POINTER=SyncPointer REPLAY-POINTER=ReplayPointer        \
           ASYNC-KEYBOARD=AsyncKeyboard SYNC-KEYBOARD=SyncKeyboard      \
           REPLAY-KEYBOARD=ReplayKeyboard                               \
           ASYNC-BOTH=AsyncBoth SYNC-BOTH=SyncBoth)
DEFUN(XLIB:ALLOW-EVENTS, display mode &optional time)
{
  Time timestamp = get_timestamp (popSTACK());
  int mode = check_allow_events(popSTACK());
  Display *dpy = pop_display ();

  X_CALL(XAllowEvents (dpy, mode, timestamp));

  VALUES1(NIL);
}


/* -----------------------------------------------------------------------
 *  Chapter 13  Resources
 * ----------------------------------------------------------------------- */

/* Maybe we want simply to drop in the LISP code here?
   -- I did (sds) */

/* 13.3  Basic Resource Database Functions */
##if 0
DEFUN(XLIB:MAKE-RESOURCE-DATABASE,) {UN DEFINED}
DEFUN(XLIB:ADD-RESOURCE, arg1 arg2 arg3) {UN DEFINED}
DEFUN(XLIB:DELETE-RESOURCE, arg1 arg2) {UN DEFINED}
DEFUN(XLIB:MAP-RESOURCE, arg1 arg2 &rest rest) {UN DEFINED}
DEFUN(XLIB:MERGE-RESOURCES, arg1 arg2) {UN DEFINED}

/* 13.4  Accessing Resource Values */
DEFUN(XLIB:GET-RESOURCE, a1 a2 a3 a4 a5) {UN DEFINED}
DEFUN(XLIB:GET-SEARCH-TABLE, arg1 arg2 arg3) {UN DEFINED}
DEFUN(XLIB:GET-SEARCH-RESOURCE, arg1 arg2 arg3) {UN DEFINED}

/* 13.5  Resource Database Files */
DEFUN(XLIB:READ-RESOURCES, a1 a2 *key KEY TEST TEST-NOT)
{UN DEFINED}
DEFUN(XLIB:WRITE-RESOURCES, a1 a2 &key WRITE TEST TEST-NOT)
{UN DEFINED}
##endif


/* -----------------------------------------------------------------------
 *  Chapter 14  Control Functions
 * ----------------------------------------------------------------------- */

/* 14.1  Grabbing the Server */
DEFUN(XLIB:GRAB-SERVER, display)
{
  Display *dpy = pop_display ();
  X_CALL(XGrabServer (dpy));
  VALUES1(NIL);
}

DEFUN(XLIB:UNGRAB-SERVER, display)
{
  Display *dpy = pop_display ();
  X_CALL(XUngrabServer (dpy));
  VALUES1(NIL);
}

/* 14.2  Pointer Control */
DEFUN(XLIB:CHANGE-POINTER-CONTROL, display &key ACCELERATION THRESHOLD)
{
  Bool do_accel = False;
  Bool do_threshold = False;
  int accel_numerator = -1;
  int accel_denominator = -1;
  int threshold = -1;
  Display *dpy;

  if (!missingp(STACK_0)) {
    do_threshold = True;
    threshold = eq(STACK_1,S(Kdefault)) ? -1 : get_sint16 (STACK_0);
  }

  if (!missingp(STACK_1)) {
    do_accel = True;
    if (eq(STACK_1,S(Kdefault)))
      accel_numerator = -1;
    else {
      /* This is basically a translation from this lisp code:

       (do* ((rational (rationalize number))
             (numerator (numerator rational) (ash numerator -1))
             (denominator (denominator rational) (ash denominator -1)))
            ((or (= numerator 1)
                 (and (< (abs numerator) #x8000)
                      (< denominator #x8000)))
             (values
               numerator (min denominator #x7fff)))) */
      pushSTACK(STACK_1); /* 0 (LOAD&PUSH 1) ; argument */
      funcall (L(rationalize), 1); /* 1 (CALLS2&PUSH 177) ; RATIONALIZE */
      pushSTACK(value1);
      pushSTACK(STACK_0);        /* 3 (LOAD&PUSH 0) */
      funcall (L(numerator), 1); /* 4 (CALLS2&PUSH 178) ; NUMERATOR */
      pushSTACK(value1);
      pushSTACK(STACK_1);          /* 6 (LOAD&PUSH 1) */
      funcall (L(denominator), 1); /* 7 (CALLS2&PUSH 179) ; DENOMINATOR */
      pushSTACK(value1);
      goto L21;                /* 9     (JMP L21) */
     L11:                      /* 11    L11 */
      pushSTACK(STACK_1);      /* 11    (LOAD&PUSH 1) */
      pushSTACK(Fixnum_minus1);/* 12 (CONST&PUSH 2) ; -1 */
      funcall (L(ash), 2);     /* 13 (CALLS2&STORE 210 1) ; ASH */
      STACK_1 = value1;
      pushSTACK(STACK_0);      /* 16 (LOAD&PUSH 0) */
      pushSTACK(Fixnum_minus1);/* 17 (CONST&PUSH 2) ; -1 */
      funcall (L(ash), 2);     /* 18 (CALLS2&STORE 210 0) ; ASH */
      STACK_0 = value1;        /* 18 */
     L21:                      /* 21 L21 */
      pushSTACK(STACK_1);      /* 21 (LOAD&PUSH 1) */
      pushSTACK(Fixnum_1);     /* 22 (CONST&PUSH 0) ; 1 */
      funcall (L(gleich), 2);  /* 23 (CALLSR&JMPIF 1 45 L41) ; = */
      if(!nullp(value1)) goto L41;
      pushSTACK(STACK_1);      /* 27 (LOAD&PUSH 1) */
      funcall (L(abs), 1);     /* 28 (CALLS2&PUSH 159) ; ABS */
      pushSTACK(value1);
      pushSTACK(fixnum(0x8000)); /* 30 (CONST&PUSH 1) ; 32768 */
      funcall (L(kleiner), 2);   /* 31 (CALLSR&JMPIFNOT 1 47 L11) ; < */
      if(nullp(value1)) goto L11;
      pushSTACK(STACK_0);        /* 35 (LOAD&PUSH 0) */
      pushSTACK(fixnum(0x8000)); /* 36 (CONST&PUSH 1) ; 32768 */
      funcall (L(kleiner), 2);   /* 37 (CALLSR&JMPIFNOT 1 47 L11) ; < */
      if(nullp(value1)) goto L11;
     L41:                        /* 41    L41 */
      /* rest done in C ... */
      accel_denominator = get_sint16 (popSTACK());
      accel_numerator = get_sint16 (popSTACK());
      if (accel_denominator > 0x7FFF)
        accel_denominator = 0x7FFF;

      skipSTACK(1);            /* right?! */

      /* 41    (LOAD&PUSH 1)
         42    (LOAD&PUSH 1)
         43    (CONST&PUSH 3)                      ; 32767
         44    (CALLSR&PUSH 1 52)                  ; MIN
         47    (STACK-TO-MV 2)
         49    (SKIP&RET 5) */

      /* Bruno: Why could not a compiler create this?! :-) */
    }
  }

  pushSTACK(STACK_2); dpy = pop_display ();

  X_CALL(XChangePointerControl (dpy, do_accel, do_threshold, accel_numerator,
                                accel_denominator, threshold));

  skipSTACK(3);
  VALUES1(NIL);
}

DEFUN(XLIB:POINTER-CONTROL, display)
{
  Display *display = pop_display ();
  int accel_numerator = 0;
  int accel_denominator = 1;
  int threshold = 0;

  X_CALL(XGetPointerControl (display, &accel_numerator, &accel_denominator,
                             &threshold));

  pushSTACK(make_sint32 (threshold));
  pushSTACK(make_sint32 (accel_numerator));
  pushSTACK(make_sint32 (accel_denominator));
  funcall (L(durch), 2);
  value2 = popSTACK();
  mv_count = 2;
}

DEFUN(XLIB:POINTER-MAPPING, display &key RESULT-TYPE)
{ /* Does the Protocol say anything about the maximum number
     of buttons?! Or the other way round: Are there any
     pointing devices with more than five buttons? */
  unsigned char map [5];
  unsigned int nmap, i;
  Display *dpy = (pushSTACK(STACK_1), pop_display());
  gcv_object_t *res_type = &STACK_0;

  X_CALL(nmap = XGetPointerMapping (dpy, map, sizeof (map)/sizeof (map[0])));

  for (i = 0; i < nmap; i++)
    pushSTACK(make_uint8 (map[i]));
  VALUES1(coerce_result_type(nmap,res_type));
  skipSTACK(2);                /* all done */
}

/* convert a Lisp sequence of uint8 to a C vector */
struct seq_uint8 { unsigned char* data; };
void coerce_into_uint8 (void *arg, object element);
void coerce_into_uint8 (void *arg, object element)
{ *(((struct seq_uint8 *)arg)->data++) = get_uint8(element); }

/*  (SETF (XLIB:POINTER-MAPPING display) mapping)
 == (XLIB:SET-POINTER-MAPPING display mapping) */
DEFUN(XLIB:SET-POINTER-MAPPING, arg1 arg2)
{
  Display *dpy = (pushSTACK(STACK_1), pop_display());
  int nmap = get_uint32(funcall1(L(length),STACK_0));
  int result;
  struct seq_uint8 su;
  DYNAMIC_ARRAY (map, unsigned char, nmap);
  su.data = map;
  map_sequence(STACK_0,coerce_into_uint8,(void*)&su);

  X_CALL(result = XSetPointerMapping (dpy, map, nmap));

  /* From XSetPointerMapping(3X11):

     If any of the buttons to be altered are logically in the down state,
     XSetPointerMapping returns MappingBusy, and the mapping is not changed.

     What should we do with that?! */

  FREE_DYNAMIC_ARRAY (map);

  VALUES1(STACK_0);
  skipSTACK(2);                /* all done */

  /* Isn't that all a overdoze for this functions?! (But since my mouse thinks
   from time to time the left button has only been invented just to make
   noise, I need it now and then) */
}

/* 14.3  Keyboard Control */
DEFUN(XLIB:BELL, display &optional percent)
{
  int percent = get_sint16_0(popSTACK());
  Display *dpy = pop_display ();

  X_CALL(XBell (dpy, percent));

  VALUES1(NIL);
}

DEFCHECKER(check_on_off,default=AutoRepeatModeDefault,OFF=AutoRepeatModeOff\
           ON=AutoRepeatModeOn DEFAULT=AutoRepeatModeDefault)
DEFUN(XLIB:CHANGE-KEYBOARD-CONTROL, display \
      &key KEY-CLICK-PERCENT BELL-PERCENT BELL-PITCH BELL-DURATION LED    \
      LED-MODE KEY AUTO-REPEAT-MODE)
{  /* http://www.linuxmanpages.com/man3/XChangeKeyboardControl.3x.php */
  unsigned long value_mask = 0;
  XKeyboardControl xkbc;
#define GETARG(mask,slot,checker)               \
  if (!missingp(STACK_0)) {                     \
    value_mask |= mask;                         \
    xkbc.slot = checker(STACK_0);               \
  }                                             \
  skipSTACK(1)
#define on_p(x) (eq(x,`:ON`))
  GETARG(KBAutoRepeatMode,auto_repeat_mode,check_on_off); /* AUTO-REPEAT-MODE */
  GETARG(KBKey,key,get_uint8);                            /* KEY */
  GETARG(KBLedMode,led_mode,on_p);                        /* LED-MODE */
  GETARG(KBLed,led,get_uint8);                            /* LED */
  GETARG(KBBellDuration,bell_duration,get_uint16); /* BELL-DURATION */
  GETARG(KBBellPitch,bell_pitch,get_uint16);       /* BELL-PITCH */
  GETARG(KBBellPercent,bell_percent,get_uint8);    /* BELL-PERCENT */
  GETARG(KBKeyClickPercent,key_click_percent,get_uint8); /* KEY-CLICK-PERCENT */
#undef on_p
#undef GETARG
  {
    Display *dpy = pop_display();
    X_CALL(XChangeKeyboardControl(dpy,value_mask,&xkbc));
  }
  VALUES0;
}

DEFUN(XLIB:KEYBOARD-CONTROL, display)
{
  Display *dpy = pop_display ();
  XKeyboardState coffee;

  X_CALL(XGetKeyboardControl (dpy, &coffee));

  pushSTACK(make_uint32 (coffee.led_mask));
  value7 = allocate_bit_vector (Atype_Bit, 256);
  X_CALL(memcpy (TheSbvector(value7)->data, coffee.auto_repeats, 32));
  value1 = make_uint8 (coffee.key_click_percent);
  value2 = make_uint8 (coffee.bell_percent);
  value3 = make_uint16 (coffee.bell_pitch);
  value4 = make_uint16 (coffee.bell_duration);
  value5 = popSTACK();
  value6 = (coffee.global_auto_repeat == AutoRepeatModeOn) ? `:ON` : `:OFF`;
  mv_count = 7;
}

DEFUN(XLIB:MODIFIER-MAPPING, display)
{
  Display *dpy = pop_display ();
  XModifierKeymap *more_coffee;
  int i;

  X_CALL(more_coffee = XGetModifierMapping (dpy));

  if (more_coffee) {
    for (i = 1; i <= 8*more_coffee->max_keypermod; i++) {
      pushSTACK(fixnum(more_coffee->modifiermap[i-1]));
      if (i%more_coffee->max_keypermod == 0) {
        value1 = listof(more_coffee->max_keypermod);
        pushSTACK(value1);
      }
    }
    X_CALL(XFreeModifiermap (more_coffee));

    value8 = popSTACK();
    value7 = popSTACK();
    value6 = popSTACK();
    value5 = popSTACK();
    value4 = popSTACK();
    value3 = popSTACK();
    value2 = popSTACK();
    value1 = popSTACK();
    mv_count = 8;
  } else
    VALUES1(NIL);
}

/* NOTE: this function is also different from the manual.
   The manual does not specify the optional argument. */
DEFUN(XLIB:QUERY-KEYMAP, display &optional bit-vector)
{
  Display *dpy = (pushSTACK(STACK_1), pop_display());

  if (boundp(STACK_0)) {
    if (!(simple_bit_vector_p (Atype_Bit, STACK_0)
          && Sbvector_length (STACK_0) == 256))
      my_type_error(`(SIMPLE-BIT-VECTOR 256)`,STACK_0);
  } else
    STACK_0 = allocate_bit_vector (Atype_Bit, 256);

  {
    unsigned char *ptr = (unsigned char *) TheSbvector(STACK_0)->data;
    X_CALL(XQueryKeymap (dpy, (char*) ptr)); /* beam it right into the bit-vector! */
  }

  VALUES1(STACK_0);
  skipSTACK(2);                /* all done */
}

DEFCHECKER(check_set_mod_map,SUCCESS=MappingSuccess FAILED=MappingFailed\
           DEVICE-BUSY=MappingBusy)
DEFUN(XLIB:SET-MODIFIER-MAPPING, display &key SHIFT LOCK CONTROL \
      MOD1 MOD2 MOD3 MOD4 MOD5)
{ /* http://www.linuxmanpages.com/man3/XModifierKeymap.3x.php */
  XModifierKeymap *xmk;
  int i, max_keys_per_mod=0;
  for (i=0; i<8; i++) {
    unsigned int len = get_uint32(funcall1(L(length),STACK_(i)));
    if (len > max_keys_per_mod) max_keys_per_mod = len;
  }
  X_CALL(xmk = XNewModifiermap(max_keys_per_mod));
  if (xmk == NULL) { VALUES0; return; } /* no values */
  for (i=0; i<8; i++) {
    struct seq_uint8 su;
    su.data = xmk->modifiermap + i*8;
    map_sequence(STACK_(8-1-i),coerce_into_uint8,(void*)&su);
  }
  skipSTACK(8);                 /* drop modifier arguments */
  {
    Display *dpy = pop_display ();
    X_CALL(i = XSetModifierMapping(dpy,xmk);
           XFreeModifiermap(xmk));
  }
  VALUES1(check_set_mod_map_reverse(i));
}

/* 14.4  Keyboard Encodings */
static object check_uint32_mx (object data) {
  while (array_atype(data = check_array(data)) != Atype_32Bit
         || array_rank(data) != 2) {
    pushSTACK(NIL);           /* no PLACE */
    pushSTACK(STACK_1);       /* TYPE-ERROR slot DATUM */
    pushSTACK(`(ARRAY (UNSIGNED-BYTE 32) (* *))`); /* EXPECTED-TYPE */
    pushSTACK(STACK_0); pushSTACK(STACK_2);
    pushSTACK(TheSubr(subr_self)->name);
    check_value(type_error,GETTEXT("~S: ~S is not an array of type ~S"));
    data = value1;
  }
  return data;
}

DEFUN(XLIB:CHANGE-KEYBOARD-MAPPING, dpy keysyms &key END FIRST-KEYCODE START)
{
  int start = check_uint_defaulted(popSTACK(),0), end, num_codes;
  int first_keycode = check_uint_defaulted(popSTACK(),start);
  uintL offset = 0, dims[2];
  Display *dpy = (pushSTACK(STACK_2), pop_display());
  KeySym* data_ptr;
  STACK_1 = check_uint32_mx(STACK_1);
  get_array_dimensions(STACK_1,2,dims);
  end = check_uint_defaulted(popSTACK(),dims[0]);
  num_codes = (end-start)*dims[1];
  STACK_0 = array_displace_check(STACK_0,num_codes,&offset);
  data_ptr = (KeySym*)TheSbvector(STACK_0)->data + offset;
  ASSERT(sizeof(uint32) == sizeof(KeySym));
  X_CALL(XChangeKeyboardMapping(dpy,first_keycode,dims[1],data_ptr,num_codes));
  VALUES0; skipSTACK(2);
}

DEFUN(XLIB:KEYBOARD-MAPPING, dpy &key FIRST-KEYCODE START END DATA)
{ /*  http://www.linuxmanpages.com/man3/XGetKeyboardMapping.3x.php */
  Display *dpy = (pushSTACK(STACK_4), pop_display());
  int first_keycode, min_keycode, max_keycode, keysyms_per_keycode;
  KeySym *map, *map1;
  int start, end, num_codes;
  object data_vector;
  void * data_ptr;
  uintL offset = 0;
  X_CALL(XDisplayKeycodes(dpy,&min_keycode,&max_keycode));
  first_keycode = check_uint_defaulted(STACK_3,min_keycode);
  start = check_uint_defaulted(STACK_2,first_keycode);
  end = check_uint_defaulted(STACK_1,1+max_keycode);
  X_CALL(map = XGetKeyboardMapping(dpy,first_keycode,end-start,
                                   &keysyms_per_keycode));
  if (missingp(STACK_0)) {      /* return a fresh array */
    pushSTACK(fixnum(end-start));
    pushSTACK(fixnum(keysyms_per_keycode));
    value1 = listof(2); pushSTACK(value1); /* dims */
    pushSTACK(S(Kelement_type)); pushSTACK(GLO(type_uint32));
    funcall(L(make_array),3); STACK_0 = value1;
  } else {                /* ensure that DATA is a valid uint32 array */
    STACK_0 = check_uint32_mx(STACK_0);
  }
  num_codes = (end-start)*keysyms_per_keycode;
  data_vector = array_displace_check(STACK_0,num_codes,&offset);
  data_ptr = (uint32*)TheSbvector(data_vector)->data + offset;
  ASSERT(sizeof(uint32) == sizeof(KeySym));
  X_CALL(memcpy(data_ptr,map,num_codes*sizeof(uint32)); XFree(map));
  VALUES1(STACK_0);
  skipSTACK(5);
}

static KeySym keycode2keysym (Display *dpy, KeyCode keycode, int index) {
  KeySym keysym;
  X_CALL(keysym = XKeycodeToKeysym (dpy, keycode, index);
  /* There is a comment in MIT-CLX, translate.lisp, which I want to quote here:

      The keysym-mapping is brain dammaged.
      Mappings for both-case alphabetic characters have the
      entry for keysym-index zero set to the uppercase keysym
      (this is normally where the lowercase keysym goes), and the
      entry for keysym-index one is zero.

   Then code continues:
    (cond ((zerop keysym-index)              ; Lowercase alphabetic keysyms
           (keysym-downcase keysym))

   That [above] is already implemented in libX, but not this [below] klugde:
          ((and (zerop keysym) (plusp keysym-index)) ; Get the uppercase keysym
           (aref mapping keycode 0))

   .. so */
         if (keysym == NoSymbol && index > 0)
           keysym = XKeycodeToKeysym (dpy, keycode, 0));
  return keysym;
}

DEFUN(XLIB:KEYCODE->KEYSYM, display keycode keysym-index)
{ /* NOTE: In the Manual this function is called "keycode-keysym" */
  int       index = get_uint8 (popSTACK());
  KeyCode keycode = get_uint8 (popSTACK());
  Display    *dpy = pop_display ();
  KeySym keysym = keycode2keysym(dpy,keycode,index);
   /* I wanted to say
          "value1 = (keysym == NoSymbol) ? NIL : make_uint32 (keysym);",
   but seeing the MIT-CLX code, I better say simply: */
  VALUES1(make_uint32 (keysym == NoSymbol ? 0 : keysym));
}

/*  XLIB:KEYCODE->CHARACTER display keysym &optional (state 0)

 NOTE: The manual calls this function "keycode-keysym"

 This functions is somewhat wired:
   - It should be called KEYSYM->SONSTWAS
   - We could also get a string instead of a single character
   - The modifier bits (the state argument) mentioned in the manual are no
     longer in the Common Lisp standard.

 BTW: How does a LISP program determine in a readable way the name of a
      keysym, I think I should add that. (For an idea inspect e.g the libX11
      functions XStringToKeysym and XKeysymToString).

 Well with normal CLX this goes just like this:

  (xlib:keysym->character dpy 97)     --> #\a
  (xlib:keysym->character dpy 97 4)   --> #\CONTROL-\a       ; 4 is <ctrl>
  (xlib:keysym->character dpy 97 8)   --> #\META-\a          ; 8 is <meta>
  (xlib:keysym->character dpy 65)     --> #\A
  (xlib:keysym->character dpy 65 4)   --> #\CONTROL-A
  (xlib:keysym->character dpy 65 8)   --> #\META-\A
  (xlib:keysym->character dpy #xFF52) --> NIL                ; #xFF52 is <up>

 Had we unicode characters, this function would become more interesting.
  Yeah is there any correspondes between the various latin-n maps in X11
 and unicode?!
  I really want unicode support. */

DEFUN(XLIB:KEYSYM->CHARACTER, display keysym &optional state)
{
  Display *dpy;
  KeySym keysym;

  /* FIXME for now we ignore the state argument: */
  skipSTACK(1);

  keysym = get_uint32 (popSTACK());
  dpy = pop_display ();
  /* Too wired -- I have to browse some more in the manuals ... Back soon. */
  VALUES1(int_char(keysym)); /* how about just int_char ?! */
}

DEFUN(XLIB:KEYSYM-NAME, keysym)
{ /* http://www.xfree86.org/current/XStringToKeysym.3.html */
  KeySym keysym = get_uint32(popSTACK());
  char *name;
  X_CALL(name = XKeysymToString(keysym));
  VALUES1(asciz_to_string(name,GLO(misc_encoding)));
}

/* Return keycodes for keysym, as multiple values

 Hmm. It goes like this:
 (xlib:keysym->keycode dpy 65) --> 38
 (xlib:keysym->keycode dpy #xFF52) --> 148 ; 98 ; 80 ; #xFF52 keysym for <up> */

DEFUN(XLIB:KEYSYM->KEYCODES, display keysym) /* NIM */
{ /* http://www.linuxmanpages.com/man3/XGetKeyboardMapping.3x.php */
  uint32 keysym = get_uint32(popSTACK());
  Display *dpy = pop_display();
  int min_keycode, max_keycode, keysyms_per_keycode, retcount=0, kc = 0;
  KeySym *map, *map1;
  begin_x_call();
  XDisplayKeycodes(dpy,&min_keycode,&max_keycode);
  map = XGetKeyboardMapping(dpy,min_keycode,max_keycode-min_keycode+1,
                            &keysyms_per_keycode);
  end_x_call();
  for (kc = min_keycode, map1 = map; kc <= max_keycode; kc++) {
    int i = 0;
    for (; i<keysyms_per_keycode; i++)
      if (*map1++ == keysym) {
        retcount++;
        pushSTACK(fixnum(kc));
      }
  }
  X_CALL(XFree(map));
  STACK_to_mv(retcount);
}

DEFUN(XLIB:KEYSYM, keysym &rest bytes) { /* see mit-clx/translate.lisp */
  if (uint8_p(STACK_(argcount))) {
    uint32 keysym = get_uint8(STACK_(argcount));
    int count = argcount;
    while (count--) keysym = (keysym<<8) | get_uint8(STACK_(count));
    skipSTACK(argcount+1);
    VALUES1(make_uint32(keysym));
  } else if ((stringp(STACK_(argcount)) || symbolp(STACK_(argcount)))
             && argcount==0) {
    KeySym keysym;
    /* unfortunately, keysyms should be named Hyper_L or Super_R,
       not :left-hyper or :right-super */
    with_stringable_0_tc(STACK_0,GLO(misc_encoding),name, {
        X_CALL(keysym=XStringToKeysym(name));
      });
    skipSTACK(1);
    VALUES1(make_uint32(keysym));
  } else {
    object tmp = listof(argcount+1); pushSTACK(tmp);
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,("~S: invalid arguments ~S"));
  }
}

DEFUN(XLIB:KEYCODE->CHARACTER, display keycode state \
      &key KEYSYM-INDEX KEYSYM-INDEX-FUNCTION) {
  KeyCode keycode = get_uint8(STACK_3);
  Display *dpy = (pushSTACK(STACK_4), pop_display());
  int index;
  if (missingp(STACK_1)) { /* no KEYSYM-INDEX => use KEYSYM-INDEX-FUNCTION */
    object func = missingp(STACK_0) ? ``XLIB::DEFAULT-KEYSYM-INDEX``
      : (object)STACK_0;
    skipSTACK(2);
    funcall(STACK_0,3);
    index = get_sint32(value1);
  } else {
    index = get_sint32(STACK_1);
    skipSTACK(5);
  }
  /* state is ignored, just like in keysym->character */
  VALUES1(int_char(keycode2keysym(dpy,keycode,index)));
}

/* 14.5  Client Termination */
DEFUN(XLIB:ADD-TO-SAVE-SET, window)
{ /* WAS: invoke (XAddToSaveSet, 1, "D1w"); */
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);

  X_CALL(XAddToSaveSet (dpy, win));

  VALUES1(NIL);
  skipSTACK(1);
}

DEFUN(XLIB:CLOSE-DOWN-MODE, display)
{ /* FIXME: This is wrong -- The close down mode could not been asked from the
         server, but you could store it in the display structure. (Like
         MIT-CLX does it.) */
  pushSTACK(`XLIB::CLOSE-DOWN-MODE`);
  fehler (error, ("~S can only be set"));
}

DEFUN(XLIB:SET-CLOSE-DOWN-MODE, mode display)
{
  Display *dpy = pop_display ();
  int mode = get_close_down_mode (STACK_0);

  X_CALL(XSetCloseDownMode (dpy, mode));

  VALUES1(popSTACK());
}

DEFUN(XLIB:KILL-CLIENT, display id)
{
  XID resource_id = get_uint29 (popSTACK());
  Display *dpy = pop_display ();

  X_CALL(XKillClient (dpy, resource_id));

  VALUES1(NIL);
}

DEFUN(XLIB:KILL-TEMPORARY-CLIENTS, display)
{
  Display *dpy = pop_display ();

  X_CALL(XKillClient (dpy, AllTemporary));

  VALUES1(NIL);
}

DEFUN(XLIB:REMOVE-FROM-SAVE-SET, window)
{
  Display *dpy;
  Window win = get_window_and_display (STACK_0, &dpy);

  X_CALL(XRemoveFromSaveSet (dpy, win));

  VALUES1(NIL);
  skipSTACK(1);
}

/* 14.6  Managing Host Access */
DEFUN(XLIB:ACCESS-CONTROL, display)
{
  Display *dpy = pop_display ();
  XHostAddress *hosts;
  Bool state;
  int nhosts;

  begin_x_call();
  hosts = XListHosts (dpy, &nhosts, &state);
  if (hosts) XFree (hosts);
  end_x_call();

  VALUES_IF(state);
}

DEFUN(XLIB::SET-ACCESS-CONTROL, dpy state)
{/* (SETF (XLIB:ACCESS-CONTROL dpy) state) */
  Bool state = get_bool(STACK_0);
  Display *dpy = (pushSTACK(STACK_1), pop_display());

  X_CALL(XSetAccessControl (dpy, state));

  VALUES1(popSTACK()); skipSTACK(1);
}

#if defined(HAVE_SYS_SOCKET_H) && defined(HAVE_NETDB_H) && defined(HAVE_NETINET_IN_H)
/* see module syscalls */
extern Values hostent_to_lisp (struct hostent *he);
DEFUN(XLIB:ACCESS-HOSTS, display &key RESULT-TYPE)
{ /* http://www.linuxmanpages.com/man3/XListHosts.3x.php */
  Display *dpy = (pushSTACK(STACK_1), pop_display());
  gcv_object_t *res_type = &STACK_0;
  XHostAddress *hosts;
  Bool state;
  int nhosts = 0;

  X_CALL(hosts = XListHosts(dpy,&nhosts,&state));
  if (hosts) {
    int i = nhosts;
    XHostAddress *ho = hosts;
    while (i--) {
      if (ho->length) {
        int family;
        switch (ho->family) {
#        if defined(HAVE_IPV6) && defined(FamilyInternet6)
          case FamilyInternet6: /* IPv6 */
            ASSERT(ho->length == sizeof(struct in6_addr));
            family = AF_INET6;
            goto handle_ipv4;
#        endif
          case FamilyInternet:  /* IPv4 */
            ASSERT(ho->length == sizeof(struct in_addr));
            family = AF_INET;
          handle_ipv4: {
              struct hostent *he;
              X_CALL(he = gethostbyaddr((char*)ho->address,ho->length,family));
              hostent_to_lisp(he);
            }
            pushSTACK(value1);
            break;
          default:
            pushSTACK(fixnum(ho->family));
            pushSTACK(allocate_bit_vector(Atype_8Bit,ho->length));
            X_CALL(memcpy(TheSbvector(STACK_0)->data,ho->address,ho->length));
            value1 = listof(2); pushSTACK(value1);
        }
      } else pushSTACK(NIL);
      ho++;
    }
    X_CALL(XFree(hosts));
  }
  VALUES2(coerce_result_type(nhosts,res_type), state ? T : NIL);
  skipSTACK(2);
}

static void lisp_to_XHostAddress (object host, XHostAddress *xha) {
  struct hostent *he;
  if (typep_classname(host,`POSIX:HOSTENT`)) {
    pushSTACK(host); funcall(`POSIX:HOSTENT-NAME`,1);
    he = resolve_host(host);
  } else he = resolve_host(host);
  switch (he->h_addrtype) {
    case AF_INET: xha->family = FamilyInternet;
      xha->length = sizeof(struct in_addr); break;
#  if defined(HAVE_IPV6) && defined(FamilyInternet6)
    case AF_INET6: xha->family = FamilyInternet6;
      xha->length = sizeof(struct in6_addr); break;
#  endif
    default: pushSTACK(fixnum(he->h_addrtype));
      pushSTACK(TheSubr(subr_self)->name);
      fehler(error,GETTEXT("~S: unknown address family ~S"));
  }
  xha->address = he->h_addr_list[0];
}
DEFUN(XLIB:ADD-ACCESS-HOST, display host)
{
  XHostAddress xha;
  Display *dpy;
  lisp_to_XHostAddress(popSTACK(),&xha);
  dpy = pop_display();
  X_CALL(XAddHost(dpy,&xha));
  VALUES0;
}
DEFUN(XLIB:REMOVE-ACCESS-HOST, display host)
{
  XHostAddress xha;
  Display *dpy;
  lisp_to_XHostAddress(popSTACK(),&xha);
  dpy = pop_display();
  X_CALL(XRemoveHost(dpy,&xha));
  VALUES0;
}
#endif  /* HAVE_SYS_SOCKET_H & HAVE_NETDB_H & HAVE_NETINET_IN_H */

/* 14.7  Screen Saver */
DEFUN(XLIB:ACTIVATE-SCREEN-SAVER, display)
{
  X_CALL(XActivateScreenSaver (pop_display ()));
  VALUES1(NIL);
}

DEFUN(XLIB:RESET-SCREEN-SAVER, display)
{
  X_CALL(XResetScreenSaver (pop_display ()));
  VALUES1(NIL);
}

/*  Lots of mixing with :on/:off, :yes/:no, why not T and NIL,
    the natural way?!
 [Was that written by Pascal programmers?] @*~#&%§"  */

/* same for DontAllowExposures ... */
DEFCHECKER(check_yes_no,default=DefaultBlanking,\
           NO=DontPreferBlanking YES=PreferBlanking DEFAULT=DefaultBlanking)

DEFUN(XLIB:SCREEN-SAVER, display)
{
  Display *dpy = pop_display ();
  int timeout;
  int interval;
  int prefer_blanking;
  int allow_exposures;

  X_CALL(XGetScreenSaver (dpy, &timeout, &interval, &prefer_blanking,
                          &allow_exposures));

  value1 = make_sint16 (timeout);
  value2 = make_sint16 (interval);
  value3 = check_yes_no_reverse(prefer_blanking);
  value4 = check_yes_no_reverse(allow_exposures);
  /* Hey?! Manual says :YES/:NO but actual implementation
     does :ON/:OFF! &$#"&! */
  mv_count = 4;
}

DEFUN(XLIB:SET-SCREEN-SAVER, display timeout period blanking exposures)
{
  int exposures = check_yes_no(popSTACK());
  int blanking = check_yes_no(popSTACK());
  int period = get_uint32 (popSTACK());
  int timeout = eq(STACK_0,S(Kdefault)) ? (skipSTACK(1),-1)
    : get_sint32(popSTACK());
  Display *dpy = pop_display();

  X_CALL(XSetScreenSaver (dpy, timeout, period, blanking, exposures));

  VALUES1(NIL);
}


/* -----------------------------------------------------------------------
 *  Chapter 15  Extentsions
 * ----------------------------------------------------------------------- */

/* 15.1  Extentions */
DEFUN(XLIB:LIST-EXTENSIONS, display &key RESULT-TYPE)
{
  int n = 0;
  char **extlist;
  Display *dpy = (pushSTACK(STACK_1), pop_display());
  gcv_object_t *res_type = &STACK_0;

  X_CALL(extlist = XListExtensions (dpy, &n));

  if (extlist) {
    int i;
    for (i = 0; i < n; i++)
      pushSTACK(asciz_to_string (extlist[i], GLO(misc_encoding)));

    X_CALL(XFreeExtensionList (extlist));
  }
  VALUES1(coerce_result_type(n,res_type));
  skipSTACK(2);
}

DEFUN(XLIB:QUERY-EXTENSION, arg1 arg2)
{
  int opcode, event, error;
  Display *dpy = (pushSTACK(STACK_1), pop_display());
  Status r;

  with_stringable_0_tc (STACK_0, GLO(misc_encoding), name, {
      X_CALL(r = XQueryExtension (dpy, name, &opcode, &event, &error));
    });

  if (r)
    VALUES3(make_uint8(opcode),make_uint8(event),make_uint8(error));
  else
    VALUES1(NIL);

  skipSTACK(2);
}


/* -----------------------------------------------------------------------
 *  Chapter 16  Errors
 * ----------------------------------------------------------------------- */

/* These pages are missing :-(
 Not any more but not rather informative */

#if 0
man XErrorEvent says:
 :
 :
       The XErrorEvent structure contains:

       typedef struct {
             int type;
             Display *display;  /* Display the event was read from */
             unsigned long serial;    /* serial number of failed request */
             unsigned char error_code;/* error code of failed request */
             unsigned char request_code;/* Major op-code of failed request */
             unsigned char minor_code;/* Minor op-code of failed request */
             XID resourceid;      /* resource id */
       } XErrorEvent;

   When you receive this event, the structure members are set as follows.

   The serial member is the number of requests, starting from one, sent over
   the network connection since it was opened.  It is the number that was the
   value of NextRequest immediately before the failing call was made.  The
   request_code member is a protocol request of the procedure that failed, as
   defined in <X11/Xproto.h>.
#endif

/* Error handler for errors occured on the display.
 This error handler is installed on all open displays, we simply call up the
 Lisp error handler here found in the ERROR-HANDLER slot in the display. */
int xlib_error_handler (Display *display, XErrorEvent *event)
{
  int f = 11;

  begin_callback ();

  /* find the display. */
  pushSTACK(find_display (display));
  if (nullp (STACK_0))
    NOTREACHED;                 /* hmm? */

  /* find the error handler */
  pushSTACK(TheStructure (STACK_0)->recdata[slot_DISPLAY_ERROR_HANDLER]);

  if (nullp (STACK_0))
    STACK_0 = `XLIB::DEFAULT-ERROR-HANDLER`;
  else if (listp (STACK_0) || vectorp (STACK_0)) { /* sequencep */
    pushSTACK(fixnum(event->error_code));
    funcall (L(elt), 2);
    pushSTACK(value1);
  }

  /* Build the argument list for the error handler: */
  pushSTACK(STACK_1);          /* display */
    pushSTACK(`#(XLIB::UNKNOWN-ERROR XLIB::REQUEST-ERROR XLIB::VALUE-ERROR XLIB::WINDOW-ERROR XLIB::PIXMAP-ERROR XLIB::ATOM-ERROR XLIB::CURSOR-ERROR XLIB::FONT-ERROR XLIB::MATCH-ERROR XLIB::DRAWABLE-ERROR XLIB::ACCESS-ERROR XLIB::ALLOC-ERROR XLIB::COLORMAP-ERROR XLIB::GCONTEXT-ERROR XLIB::ID-CHOICE-ERROR XLIB::NAME-ERROR XLIB::LENGTH-ERROR XLIB::IMPLEMENTATION-ERROR)`);
    pushSTACK(fixnum(event->error_code));
    funcall (L(aref), 2);
  pushSTACK(value1);            /* error code */

  pushSTACK(`:CURRENT-SEQUENCE`); pushSTACK(make_uint16(NextRequest(display)));
  pushSTACK(`:SEQUENCE`);         pushSTACK(make_uint16(event->serial));
  pushSTACK(`:MAJOR`);            pushSTACK(make_uint8 (event->request_code));
  pushSTACK(`:MINOR`);            pushSTACK(make_uint16(event->minor_code));

  switch (event->error_code) {
    case BadColor:              /* colormap-error */
    case BadCursor:             /* cursor-error */
    case BadDrawable:           /* drawable-error */
    case BadFont:               /* font-error */
    case BadGC:                 /* gcontext-error */
    case BadIDChoice:           /* id-choice-error */
    case BadPixmap:             /* pixmap-error */
    case BadWindow:             /* window-error */
      pushSTACK(`:RESOURCE-ID`);
      pushSTACK(make_uint32 (event->resourceid));
      f += 2;
      break;
    case BadAtom:               /* atom-error */
      pushSTACK(`:ATOM-ID`);
      pushSTACK(make_uint32 (event->resourceid));
      f += 2;
      break;
    case BadValue:              /* value-error */
      pushSTACK(`:VALUE`);
      pushSTACK(make_uint32 (event->resourceid));
      f += 2;
      break;
  }

  /* Now call the handler: */
  funcall (L(funcall), f);

  skipSTACK(1);                /* clean up */

  end_callback ();

  return 0;                     /* anything done with this? */
}

int xlib_io_error_handler (Display *display)
{
  begin_callback ();

  pushSTACK(find_display (display));
  fehler (error, "IO Error on display ~S.");
}

int xlib_after_function (Display *display)
{
  begin_callback ();
  pushSTACK(find_display (display));
  funcall(TheStructure(STACK_0)->recdata[slot_DISPLAY_AFTER_FUNCTION],1);
  end_callback ();
  return 0;
}


/* ----------------------------------------------------------------------------
  The Shape Extension
 ---------------------------------------------------------------------------- */

##if WANT_XSHAPE

/* NOTE: The functions in here are my own invents ... */

/* First three little enums (three? I can only see two!) */

DEFCHECKER(get_shape_kind,default=ShapeBounding,        \
           BOUNDING=ShapeBounding CLIP=ShapeClip)
DEFCHECKER(get_shape_operation,default=ShapeSet, SET=ShapeSet UNION=ShapeUnion \
           INTERSECT=ShapeIntersect SUBTRACT=ShapeSubtract INVERT=ShapeInvert)

static Bool ensure_shape_extension (Display *dpy, object dpy_obj, int error_p)
{ /* Ensures that the SHAPE extension is initialized. If it is not available
     and error_p is set raise an appropriate error message. */
  int event_base, error_base, status;
  X_CALL(status = XShapeQueryExtension(dpy,&event_base,&error_base));
  if (status) {
    /* Everything is ok just proceed */
    return True;
  } else {
    if (error_p) {                              /* raise an error */
      pushSTACK(dpy_obj);                       /* the display */
      pushSTACK(TheSubr(subr_self)->name);      /* function name */
      fehler (error, ("~S: Shape extension is not available on display ~S."));
    } else
      return False;
  }
}

/*   XLIB:SHAPE-VERSION display

  =>  major ;
      minor */
DEFUN(XLIB:SHAPE-VERSION, display)
{
  Display *dpy = (pushSTACK(STACK_0), pop_display());
  int major_version, minor_version, status;
  if (ensure_shape_extension (dpy, STACK_0, 0)) { /* Is it there? */
    X_CALL(status = XShapeQueryVersion(dpy,&major_version,&minor_version));
    if (status) {
      VALUES2(make_uint16(major_version),make_uint16(minor_version));
      skipSTACK(1);
      return;                   /* all done */
    }
  }

  /* Just return NIL here */
  VALUES1(NIL);
  skipSTACK(1);
}

/*   XLIB:SHAPE-COMBINE destination source
          &key (:kind :bounding) (:source-kind :bounding)
               (:x-offset 0) (:y-offset 0)
               (:operation :set) (:ordering :unsorted) */
DEFUN(XLIB:SHAPE-COMBINE, destination source \
      &key KIND SOURCE-KIND X-OFFSET Y-OFFSET OPERATION ORDERING)
{
  int ordering = get_ordering(popSTACK());
  int       op = get_shape_operation(popSTACK());
  int    y_off = get_sint16_0(popSTACK());
  int    x_off = get_sint16_0(popSTACK());
  int src_kind = get_shape_kind(popSTACK());
  int     kind = get_shape_kind(popSTACK());
  Display *dpy;
  Window  dest = get_window_and_display (STACK_1, &dpy);

  (void)ensure_shape_extension (dpy, get_display_obj (STACK_1), 1);

  /* Now we have to select on the second arg, which operation is
     actually wanted:
   pixmap -> XShapeCombineMask
   window -> XShapeCombineShape
   sequence of rectangles -> XShapeCombineRectangles

   FIXME: Should we emit an error message if we get keywords, which are
   not applicable? */

  if (pixmap_p (STACK_0)) {
    Pixmap src = get_pixmap (STACK_0);
    X_CALL(XShapeCombineMask(dpy,dest,kind,x_off,y_off,src,op));
  } else if (window_p (STACK_0)) {
    Pixmap src = get_window (STACK_0);
    X_CALL(XShapeCombineShape(dpy,dest,kind,x_off,y_off,src,src_kind,op));
  } else if (listp (STACK_0) || vectorp (STACK_0)) {
    int nrectangles = get_seq_len(&STACK_0,&`XLIB::RECT-SEQ`,4);
    DYNAMIC_ARRAY (rectangles, XRectangle, nrectangles);
    set_seq(&STACK_0,rectangles,coerce_into_rectangle);
    X_CALL(XShapeCombineRectangles(dpy,dest,kind,x_off,y_off,
                                   rectangles,nrectangles,op,ordering));
    FREE_DYNAMIC_ARRAY (rectangles);
  }

  VALUES1(NIL);
  skipSTACK(2);                /* all done */
}

DEFUN(XLIB:SHAPE-OFFSET, destination kind x-offset y-offset)
{
  Display *dpy;
  Window  dest = get_window_and_display (STACK_3, &dpy);
  int     kind = get_shape_kind (STACK_2);
  int x_offset = get_sint16 (STACK_1);
  int y_offset = get_sint16 (STACK_0);

  (void)ensure_shape_extension (dpy, get_display_obj (STACK_3), 1);

  X_CALL(XShapeOffsetShape(dpy,dest,kind,x_offset,y_offset));

  VALUES1(NIL);
  skipSTACK(4);
}

/* -> bounding-shaped-p
      clip-shaped-p
      x-bounding, y-bounding, x-clip, y-clip
      w-bounding, h-bounding, w-clip, h-clip */
DEFUN(XLIB:SHAPE-EXTENTS, window)
{
  Display *dpy;
  Window  window = get_window_and_display(popSTACK(),&dpy);
  Bool bounding_shaped;
  int x_bounding;
  int y_bounding;
  unsigned int w_bounding;
  unsigned int h_bounding;
  Bool clip_shaped;
  int x_clip;
  int y_clip;
  unsigned int w_clip;
  unsigned int h_clip;
  Status status;
  X_CALL(status = XShapeQueryExtents(dpy,window,&bounding_shaped,
                                     &x_bounding,&y_bounding,
                                     &w_bounding,&h_bounding,
                                     &clip_shaped,&x_clip,&y_clip,
                                     &w_clip,&h_clip));
  if (status) VALUES0;
  else {
    value1 = bounding_shaped ? T : NIL;
    value2 = clip_shaped ? T : NIL;
    value3 = fixnum(x_bounding);
    value4 = fixnum(y_bounding);
    value5 = fixnum(x_clip);
    value6 = fixnum(y_clip);
    value7 = fixnum(w_bounding);
    value8 = fixnum(h_bounding);
    value9 = fixnum(w_clip);
    mv_space[9] = fixnum(h_clip); /* 10th value */
    mv_count = 10;
  }
}

/* -> rectangles - (rep-seq (sint16 sint16 sint16 sint16))
      ordering   - (member :unsorted :y-sorted :yx-sorted :yx-banded) */
DEFUN(XLIB:SHAPE-RECTANGLES, window kind)
{
  int kind = get_shape_kind(popSTACK());
  Display *dpy;
  Window  window = get_window_and_display(popSTACK(),&dpy);
  XRectangle *rect;
  int count, ordering, i;
  X_CALL(rect = XShapeGetRectangles(dpy,window,kind,&count,&ordering));
  for (i=count; i; i--, rect++) {
    pushSTACK(fixnum(rect->x));
    pushSTACK(fixnum(rect->y));
    pushSTACK(fixnum(rect->width));
    pushSTACK(fixnum(rect->height));
  }
  value1 = listof(4*count); pushSTACK(value1);
  value2 = get_ordering_reverse(ordering);
  value1 = popSTACK();
  mv_count = 2;
}
##endif


/* -----------------------------------------------------------------------
 *  Not explicitly specified functions
 * ----------------------------------------------------------------------- */

/* I think I will not actually support these functions, until there are needed
 * by some application.
 *
 * Since they are not in the CLX Manual, they are actually undocumented
 * functions of CLX, which should either way round not be used by CLX
 * programs. (But it is strange, that the corresponding symbols are exported
 * from the CLX package!)
 *
 * I may be wrong due to the WM functions, since these seems to be actually
 * used by a couple of applications.
 */

DEFUN(XLIB:ICONIFY-WINDOW, window screen)
{
  Screen  *scr = get_screen (popSTACK());
  Display *dpy;
  Window   win = get_window_and_display (popSTACK(), &dpy);
  X_CALL(XIconifyWindow(dpy,win,XScreenNumberOfScreen(scr)));
  VALUES1(NIL);
}

DEFUN(XLIB:WITHDRAW-WINDOW, window screen)
{
  Screen  *scr = get_screen (popSTACK());
  Display *dpy;
  Window   win = get_window_and_display (popSTACK(), &dpy);
  X_CALL(XWithdrawWindow(dpy,win,XScreenNumberOfScreen(scr)));
  VALUES1(NIL);
}

DEFUN(XLIB:DEFAULT-KEYSYM-INDEX, display keycode state)
{ /* Returns a keysym-index for use with keycode->character */
  int state = get_uint32(popSTACK());
  KeyCode keycode = get_uint8(popSTACK());
  Display *dpy = pop_display();
  /* see comment in keycode2keysym: looks like the only index that
     makes any sense is 0 */
  VALUES1(Fixnum_0);
}

##if 0
/* ??? */
DEFUN(XLIB:DESCRIBE-ERROR, arg1 arg2) {UNDEFINED;}
DEFUN(XLIB:DESCRIBE-EVENT, a1 a2 a3 &optional a4) {UNDEFINED;}
DEFUN(XLIB:DESCRIBE-REPLY, arg1 arg2) {UNDEFINED;}
DEFUN(XLIB:DESCRIBE-REQUEST, arg1 arg2) {UNDEFINED;}
DEFUN(XLIB:DESCRIBE-TRACE, a1 &optional a2) {UNDEFINED;}
DEFUN(XLIB:EVENT-HANDLER, arg1 arg2) {UNDEFINED;}
DEFUN(XLIB:GET-EXTERNAL-EVENT-CODE, arg1 arg2) {UNDEFINED;}
DEFUN(XLIB:MAKE-EVENT-HANDLERS, &key TYPE DEFAULT) {UNDEFINED;}
DEFUN(XLIB:DECODE-CORE-ERROR, a1 a2 &optional a3) {UNDEFINED;}

/* Digging with resources */
DEFUN(XLIB:ROOT-RESOURCES, arg &key DATABASE KEY TEST TEST-NOT) {UNDEFINED;}
DEFUN(XLIB:RESOURCE-DATABASE-TIMESTAMP, arg) {UNDEFINED;}
DEFUN(XLIB:RESOURCE-KEY, arg) {UNDEFINED;}

/* These seem to handle keysym translations */
DEFUN(XLIB:KEYSYM-IN-MAP-P, arg1 arg2 arg3) {UNDEFINED;}
DEFUN(XLIB:KEYSYM-SET, a1) {UNDEFINED;}
DEFUN(XLIB:CHARACTER->KEYSYMS, a1 a2 &optional a3) {UNDEFINED;}
DEFUN(XLIB:CHARACTER-IN-MAP-P, arg1 arg2 arg3) {UNDEFINED;}
DEFUN(XLIB:DEFAULT-KEYSYM-TRANSLATE, arg1 arg2 arg3) {UNDEFINED;}
DEFUN(XLIB:DEFINE-KEYSYM, a1 a2 &key LOWERCASE TRANSLATE MODIFIERS MASK DISPLAY) {UNDEFINED;}
DEFUN(XLIB:DEFINE-KEYSYM-SET, arg1 arg2 arg3) {UNDEFINED;}
DEFUN(XLIB:MAPPING-NOTIFY, a1 a2 a2 a4) {UNDEFINED;}
DEFUN(XLIB:UNDEFINE-KEYSYM, a1 a2 &key DISPLAY MODIFIERS &allow-other-keys){UNDEFINED;}

/* These seem to be some tracing feature */
DEFUN(XLIB:UNTRACE-DISPLAY, display) {UNDEFINED;}
DEFUN(XLIB:SUSPEND-DISPLAY-TRACING, display) {UNDEFINED;}
DEFUN(XLIB:RESUME-DISPLAY-TRACING, display) {UNDEFINED;}
DEFUN(XLIB:SHOW-TRACE, display &key LENGTH SHOW-PROCESS) {UNDEFINED;}
DEFUN(XLIB:TRACE-DISPLAY, display) {UNDEFINED;}

/*  Somewhat bogus ... */
DEFUN(XLIB:SET-WM-RESOURCES, a1 a2 &key WRITE TEST TEST-NOT) {UNDEFINED;}
DEFUN(XLIB:NO-OPERATION, display) {UNDEFINED;}

/* [ MOVED TO LISP */
##if 0
/* All these are defined in manager.lisp and are simply droped in ... */
DEFUN(XLIB:ICON-SIZES, display) {UNDEFINED;}
DEFUN(XLIB:SET-WM-CLASS, arg1 arg2 arg3) {UNDEFINED;}
DEFUN(XLIB:SET-WM-PROPERTIES, &rest args) {UNDEFINED;} /* LISPFUN  (xlib_set_wm_properties, 1, 0, rest, key, 36, (:NAME :ICON-NAME :RESOURCE-NAME :RESOURCE-CLASS :COMMAND :CLIENT-MACHINE :HINTS :NORMAL-HINTS :ZOOM-HINTS :USER-SPECIFIED-POSITION-P :USER-SPECIFIED-SIZE-P :PROGRAM-SPECIFIED-POSITION-P :PROGRAM-SPECIFIED-SIZE-P :X :Y :WIDTH :HEIGHT :MIN-WIDTH :MIN-HEIGHT :MAX-WIDTH :MAX-HEIGHT :WIDTH-INC :HEIGHT-INC :MIN-ASPECT :MAX-ASPECT :BASE-WIDTH :BASE-HEIGHT :WIN-GRAVITY :INPUT :INITIAL-STATE :ICON-PIXMAP :ICON-WINDOW :ICON-X :ICON-Y :ICON-MASK :WINDOW-GROUP)) */
DEFUN(XLIB:MAKE-WM-HINTS, &key INPUT INITIAL-STATE ICON-PIXMAP ICON-WINDOW ICON-X ICON-Y ICON-MASK WINDOW-GROUP FLAGS) {UNDEFINED;}
DEFUN(XLIB:MAKE-WM-SIZE-HINTS, &key USER-SPECIFIED-POSITION-P USER-SPECIFIED-SIZE-P X Y WIDTH HEIGHT MIN-WIDTH MIN-HEIGHT MAX-WIDTH MAX-HEIGHT WIDTH-INC HEIGHT-INC MIN-ASPECT MAX-ASPECT BASE-WIDTH BASE-HEIGHT WIN-GRAVITY PROGRAM-SPECIFIED-POSITION-P PROGRAM-SPECIFIED-SIZE-P) {UNDEFINED;}
DEFUN(XLIB:GET-WM-CLASS, arg) {UNDEFINED;}
DEFUN(XLIB:TRANSIENT-FOR, arg) {UNDEFINED;}
DEFUN(XLIB:WM-CLIENT-MACHINE, arg) {UNDEFINED;}
DEFUN(XLIB:WM-COLORMAP-WINDOWS, arg) {UNDEFINED;}
DEFUN(XLIB:WM-COMMAND, arg) {UNDEFINED;}
DEFUN(XLIB:WM-HINTS, arg) {UNDEFINED;}
DEFUN(XLIB:WM-HINTS-FLAGS, arg) {UNDEFINED;}
DEFUN(XLIB:WM-HINTS-ICON-MASK, arg) {UNDEFINED;}
DEFUN(XLIB:WM-HINTS-ICON-PIXMAP, arg) {UNDEFINED;}
DEFUN(XLIB:WM-HINTS-ICON-WINDOW, arg) {UNDEFINED;}
DEFUN(XLIB:WM-HINTS-ICON-X, arg) {UNDEFINED;}
DEFUN(XLIB:WM-HINTS-ICON-Y, arg) {UNDEFINED;}
DEFUN(XLIB:WM-HINTS-INITIAL-STATE, arg) {UNDEFINED;}
DEFUN(XLIB:WM-HINTS-INPUT, arg) {UNDEFINED;}
DEFUN(XLIB:WM-HINTS-P, arg) {UNDEFINED;}
DEFUN(XLIB:WM-HINTS-WINDOW-GROUP, arg) {UNDEFINED;}
DEFUN(XLIB:WM-ICON-NAME, arg) {UNDEFINED;}
DEFUN(XLIB:WM-NORMAL-HINTS, arg) {UNDEFINED;}
DEFUN(XLIB:WM-PROTOCOLS, arg) {UNDEFINED;}
DEFUN(XLIB:WM-RESOURCES, arg1 arg2 &key KEY TEST TEST-NOT) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-BASE-HEIGHT, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-BASE-WIDTH, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-HEIGHT, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-HEIGHT-INC, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-MAX-ASPECT, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-MAX-HEIGHT, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-MAX-WIDTH, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-MIN-ASPECT, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-MIN-HEIGHT, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-MIN-WIDTH, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-P, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-USER-SPECIFIED-POSITION-P, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-USER-SPECIFIED-SIZE-P, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-WIDTH, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-WIDTH-INC, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-WIN-GRAVITY, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-X, arg) {UNDEFINED;}
DEFUN(XLIB:WM-SIZE-HINTS-Y, arg) {UNDEFINED;}
DEFUN(XLIB:RGB-COLORMAPS, arg1 arg2) {UNDEFINED;}
DEFUN(XLIB:WM-NAME, arg) {UNDEFINED;}
/* ... and properly some more ... */

/* These are simply defstruct generated functions -- moved to Lisp */
DEFUN(XLIB:VISUAL-INFO-BITS-PER-RGB, arg)
DEFUN(XLIB:VISUAL-INFO-BLUE-MASK, arg)
DEFUN(XLIB:VISUAL-INFO-CLASS, arg)
DEFUN(XLIB:VISUAL-INFO-COLORMAP-ENTRIES, arg)
DEFUN(XLIB:VISUAL-INFO-DISPLAY, arg)
DEFUN(XLIB:VISUAL-INFO-GREEN-MASK, arg)
DEFUN(XLIB:VISUAL-INFO-ID, arg)
DEFUN(XLIB:VISUAL-INFO-P, arg)
DEFUN(XLIB:VISUAL-INFO-PLIST, arg)
DEFUN(XLIB:VISUAL-INFO-RED-MASK, arg)

/* These here are defined in Lisp: */
DEFUN(XLIB:CUT-BUFFER, a1 &key BUFFER TYPE RESULT-TYPE TRANSFORM START END) {UNDEFINED;}
DEFUN(XLIB:ROTATE-CUT-BUFFERS, a1 &optional a2 a3) {UNDEFINED;}
DEFUN(XLIB:BITMAP-IMAGE, &optional a1 &rest args)
##endif
/* ] */

/* [ CONSIDERED OBSOLETE */
##if 0
DEFUN(XLIB:WM-ZOOM-HINTS, arg) {UNDEFINED;}
DEFUN(XLIB:SET-STANDARD-PROPERTIES, a1 &rest rest) {UNDEFINED;}
DEFUN(XLIB:GET-STANDARD-COLORMAP, arg1 arg2) {UNDEFINED;}
DEFUN(XLIB:SET-STANDARD-COLORMAP, a1 a2 a3 a4 a5 a6) {UNDEFINED;}
##endif
/* ] */
##endif
/* Puh! That is really lots of typing ...
    ... But what wouldn't I do to get (hopefully) GARNET working? */

/* But we are not finished yet, we have yet to finish the libX11 :-) */


/* -----------------------------------------------------------------------
 *  Fixups of libX
 * ----------------------------------------------------------------------- */

static Visual *XVisualIDToVisual (Display *dpy, VisualID vid)
{ /*PORTABLE-P?*/
  XVisualInfo templeight, *r;
  Visual *result;
  int n;

  templeight.visualid = vid;
  X_CALL(r = XGetVisualInfo (dpy, VisualIDMask, &templeight, &n));
  if (n == 1) {
    result = r->visual;
    X_CALL(XFree (r));
    return result;
  } else {
    X_CALL(if (r) XFree (r));

    /* Maybe we emerge a x-bad-SONSTWAS condition here, since the 0 value
     _is_ meaningful to the libX11; It is CopyFromParent. */
    return 0;
  }
}

static int XScreenNo (Display *dpy, Screen *screen)
{ /* Find the screen number of an screen */
  int i;
  for (i = 0; ScreenCount (dpy); i++)
    if (ScreenOfDisplay (dpy,i) == screen)
      return i;

  /* not found, what should we return?! */
  return 0;                     /* Hier kann nichs schief gehen. */
            /* Und wenn Gilbert sagt, nichs, dann meint er, nix! */
}

/* So, now we could expose this to the compiler. */

/*  Now the somewhat standard tail of my files, which wander out of my
 small loved five-years old box.
         [Cheers! Long live the ISA bus :-]

    Sorry, if you do not have a wide display or a small font and eyes as
    good as mine.

    Most lines are written between two o`clock and five o`clock in the morning.
*/

#define SILLY 1
#if SILLY
int this_is_a_test_for_the_linker_and_the_debugger_and_the_nm_utility__lets_have_a_look_if_they_could_cope_with_this_indeed_very_long_identifer__still_reading_this__if_not_in_the_editor___CONGRATULATIONS;
#endif


/* -----------------------------------------------------------------------
 *  Xpm Interface
 * -----------------------------------------------------------------------
 * Need this for my small sokoban port ... */

##if WANT_XPM
#include <X11/xpm.h>

DEFUN(XPM:READ-FILE-TO-PIXMAP, drawable filename &key SHAPE-MASK-P PIXMAP-P)
{ /* -> pixmap, shape */
  Display     *dpy;
  Drawable      da = get_drawable_and_display (STACK_3, &dpy);
  int shape_mask_p = !missingp(STACK_1);                      /* default NIL */
  int     pixmap_p = boundp(STACK_0) ? get_bool(STACK_0) : 1; /* default T */
  int r;
  Pixmap pixmap = 0;
  Pixmap shape_mask = 0;

  pushSTACK(get_display_obj (STACK_3));

  STACK_3 = physical_namestring(STACK_3);

  with_string_0 (STACK_3, GLO(pathname_encoding), filename, {
      X_CALL(r = XpmReadFileToPixmap (dpy, da, filename,
                                      pixmap_p?&pixmap:NULL,
                                      shape_mask_p?&shape_mask:NULL, NULL));
    });

  if (r != XpmSuccess) { /* http://root.cern.ch/lxr/source/x11/inc/Xpm.h */
    switch (r) {
      case XpmColorError:  pushSTACK(`"color error"`);  break;
      case XpmOpenFailed:  pushSTACK(`"open failed"`);  break;
      case XpmFileInvalid: pushSTACK(`"file invalid"`); break;
      case XpmNoMemory:    pushSTACK(`"no memory"`);    break;
      case XpmColorFailed: pushSTACK(`"color failed"`); break;
      default:             NOTREACHED;
    }
    pushSTACK(STACK_4);         /* pathname */
    pushSTACK(TheSubr(subr_self)->name);
    fehler(error,"~S: Cannot read ~S: ~S");
  }

  if (pixmap)     pushSTACK(make_pixmap (STACK_0, pixmap));
  else pushSTACK(NIL);
  if (shape_mask) pushSTACK(make_pixmap (STACK_1, shape_mask));
  else pushSTACK(NIL);
  VALUES2(STACK_1,STACK_0);
  skipSTACK(7);
}
##endif

void module__clx__init_function_2 (module_t *module);
void module__clx__init_function_2 (module_t *module)
{  /* setze doch `XLIB::*DISPLAYS*` auf NIL ! */
#if 0
  uintC i;

  for (i = 0 ; i < module__clx__object_tab_size; i++) {
    dprintf (("\n;; otab[%d] = '%s' -->",i,
              module__clx__object_tab_initdata[i]));
    pushSTACK(((gcv_object_t *)( & module__clx__object_tab))[i]);
    funcall (L(princ),1);
  }
#endif
}
