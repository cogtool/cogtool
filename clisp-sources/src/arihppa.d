/*
 * External routines for ARILEV1.D
 * Processor: HPPA, because of XMPYU only on HPPA 1.1 (like HP9000/720)
 * Compiler: GNU-C or HP-C
 * Parameter Passing: in registers %arg0,%arg1,%arg2, return value in %ret0.
 * Settings: intCsize=32, intDsize=32.
 */

#ifdef INCLUDED_FROM_C

#else

/* mostly copied out of hppa.s from the PARI/GP-distribution. */

#ifndef __GNUC__ /* with GNU-C we do mulu32() as macro, that multiplies inline */

                .SHORTDATA
                .IMPORT $global$,DATA
                .EXPORT mulu32_high
                .ALIGN 8
mulu32_high     .WORD           /* 8 byte room */
                .WORD

                .CODE
                .EXPORT mulu32_
/* extern struct { uint32 lo; uint32 hi; } mulu32_ (uint32 arg1, uint32 arg2);
   2^32*hi+lo := arg1*arg2. */
mulu32_         .PROC
                .CALLINFO
                .ENTER  /* input in %arg0,%arg1, Output in %ret0,mulu32_high */
                LDIL    L'mulu32_high-$global$,%r1
                LDO     R'mulu32_high-$global$(%r1),%r1
                                                /* %r1 = &x */
                STW     %arg0,0(%r1)            /* store x abspeichern */
                FLDWS   0(%r1),%fr4             /* and load into coprocessor */
                STW     %arg1,0(%r1)            /* store y */
                FLDWS   0(%r1),%fr5             /* and load into coprocessor */
                XMPYU   %fr4,%fr5,%fr6          /* multiply both */
                FSTDS   %fr6,0(%r1)             /* store result (64 bit) */
                LDWS    4(%r1),%ret0            /* low 32 bits as result */
                .LEAVE
                .PROCEND

#endif



                .CODE
                .EXPORT length32
/* returns integer-size (>=1, <=32) of the argument /=0. */
                .label length32
                .PROC
                .CALLINFO
                .ENTRY          /* input in %arg0, output in %ret0 */
                /* y = 1; */
                LDI             1,%ret0
                /* if (x & (bit(31-15)*(bit(16)-1)) == 0) */
                EXTRU,<>        %arg0,15,16,%r0
                SHD,TR          %arg0,%r0,16,%arg0   /* x = x<<(32-16); else */
                ADDI            16,%ret0,%ret0       /* y = y+16; */
                /* if (x & (bit(31-7)*(bit(8)-1)) == 0) */
                EXTRU,<>        %arg0,7,8,%r0
                SHD,TR          %arg0,%r0,24,%arg0   /* x = x<<(32-24); else */
                ADDI            8,%ret0,%ret0        /* y = y+8; */
                /* if (x & (bit(31-3)*(bit(4)-1)) == 0) */
                EXTRU,<>        %arg0,3,4,%r0
                SHD,TR          %arg0,%r0,28,%arg0   /* x = x<<(32-28); else */
                ADDI            4,%ret0,%ret0        /* y = y+4; */
                /* if (x & (bit(31-1)*(bit(2)-1)) == 0) */
                EXTRU,<>        %arg0,1,2,%r0
                SHD,TR          %arg0,%r0,30,%arg0   /* x = x<<(32-30); else */
                ADDI            2,%ret0,%ret0        /* y = y+2; */
                /* if (x & (bit(31-0)*(bit(1)-1)) != 0) */
                EXTRU,=         %arg0,0,1,%r0
                ADDI            1,%ret0,%ret0        /* y = y+1; */
                BV              0(%r2)               /* Return */
                NOP
                .EXIT
                .PROCEND


#if 0 /* we don't need that */
                .CODE
                .EXPORT bfffo
/* Returns the number of the leading nullbits of x. */
bfffo           .PROC
                .CALLINFO
                .ENTER
                /* if (x==0) goto L$0; */
                COMB,=,N        %r0,%arg0,L$0
                /* y = 31; */
                LDI             31,%ret0
                /* if (x & (bit(31-15)*(bit(16)-1)) == 0) */
                EXTRU,<>        %arg0,15,16,%r0
                SHD,TR          %arg0,%r0,16,%arg0   /* x = x<<(32-16); else */
                ADDI            -16,%ret0,%ret0      /* y = y-16; */
                /* if (x & (bit(31-7)*(bit(8)-1)) == 0) */
                EXTRU,<>        %arg0,7,8,%r0
                SHD,TR          %arg0,%r0,24,%arg0   /* x = x<<(32-24); else */
                ADDI            -8,%ret0,%ret0       /* y = y-8; */
                /* if (x & (bit(31-3)*(bit(4)-1)) == 0) */
                EXTRU,<>        %arg0,3,4,%r0
                SHD,TR          %arg0,%r0,28,%arg0   /* x = x<<(32-28); else */
                ADDI            -4,%ret0,%ret0       /* y = y-4; */
                /* if (x & (bit(31-1)*(bit(2)-1)) == 0) */
                EXTRU,<>        %arg0,1,2,%r0
                SHD,TR          %arg0,%r0,30,%arg0   /* x = x<<(32-30); else */
                ADDI            -2,%ret0,%ret0       /* y = y-2; */
                /* if (x & (bit(31-0)*(bit(1)-1)) != 0) */
                EXTRU,=         %arg0,0,1,%r0
                ADDI            -1,%ret0,%ret0       /* y = y-1; */
                /* goto L$1; */
                B,N             L$1
L$0             LDI             32,%ret0
L$1             .LEAVE
                .PROCEND
#endif


#if 0
/* This does not yet work.
   If this works anytime in the future (test especially UDS_to_DIGITS!),
   then change HPPA_DIV_WORKS -> HPPA in arilev0.d. */

                .CODE
                .EXPORT divu_6432_3232_
                .IMPORT divu_32_rest,DATA
/* extern struct { uint32 q; uint32 r; } divu_6432_3232_ (uint32 xhi, uint32 xlo, uint32 y);
   write x = 2^32*xhi+xlo = q*y+r. assume, that 0 <= x < 2^32*y . */
divu_6432_3232_ .PROC
                .CALLINFO
                .ENTER  /* input in %arg0,%arg1,%arg2, output in %ret0,divu_32_rest */
/* Register designations : */
hirem           .REG    %arg0   /* xhi */
lorem           .REG    %arg1   /* xlo */
div             .REG    %arg2   /* y bzw. y' */
origdiv         .REG    %arg3
quo             .REG    %ret0   /* q */
temp1           .REG    %r20
temp2           .REG    %r21
temp3           .REG    %r22

/* division-single-step:
               DS      hirem,div,hirem
 div should be >0 and <2^31 . */
#if 0 /* this is how it is documented */
/* shifts hirem by 1 bit to the left, whereby the Carry is shifted in on the
 right. Then div is added resp. - if V is set - (-div) is added.
 The carry of this addition comes both into the Carry and also into the V-Bit
 (V-Bit inverted, if div >= 2^31). */
#else /* that seems to work */
/* shifts hirem by 1 bit to the left, whereby the Carry is shifted in on the
 right. Then div is subtracted resp. - if V deleted - (-div) is subtracted.
 The carry of this subtraction comes both into the Carry and also
 inverted into the V-bit (not inverted, if div >= 2^31). */
#endif

/* The algorithm is the same as in my arisparc.s:_divu_3216_1616_ :
 if y has been subtracted wrongly, this is remedied by:
 after the next 1-bit-shift - instead of subtracting y -
 add 2*y and subtract y, i.e. add y. */

/* 1 divisions-single-step:
 shift hirem|lorem by 1 bit to the left and (if V is set)
 subtract div = y resp. (if V is deleted) add div = y . */
DS1             .MACRO
                ADDC    lorem,lorem,lorem
                DS      hirem,div,hirem
                .ENDM

/* 4 division-single-steps: */
DS4             .MACRO
                DS1
                DS1
                DS1
                DS1
                .ENDM

/* 32 division-single-steps: */
DS32            .MACRO
                DS4
                DS4
                DS4
                DS4
                DS4
                DS4
                DS4
                DS4
                .ENDM

                COMB,<          div,0,L$50          /* y>=0(signed), i.e. y < 2^31 ? */
                /* yes -> "small" division */
                SUB             0,div,temp1         /* temp1 = -y > 2^31 */
                DS              0,temp1,0           /* set V-bit (successfully feign subtraction) */
                DS32                                /* shift hirem|lorem 32 times, */
                                                    /* each time shift the carry into lorem */
                ADDC            lorem,lorem,lorem   /* shift last carry into lorem */
                /* now, hirem contains the rest r or r-y, lorem contains the quotient q. */
                ADD,>=          0,hirem,0           /* hirem < 0 (signed) ? */
                ADD             hirem,div,hirem     /* yes -> still have to add y */
                ADDIL           L'divu_32_rest-$global$,%dp
                STW             hirem,R'divu_32_rest-$global$(%r1)
                                                    /* save rest r */
                COPY            lorem,quo           /* quotient q */
                .LEAVE

L$50            /* y >= 2^31. reduction by halving x and y. */
                COPY            div,origdiv         /* save y in origdiv */
                EXTRU,<>        div,31,1,temp2      /* temp2 := bit 0 of y */
                B               L$51
                /* division by odd number: */
                /* write y = 2*y'-1. */
                /* floor(x / (2*y'-1)) = floor(floor(x/2) / y') + (0 or 1 or 2) */
                /* as  0 <= x/(2*y'-1) - x/(2*y') = x/(2*y'-1) / (2*y') = x/y / (2*y') */
                /*       < 2^32 / (2*y') < 2^32/y <= 2 . */
                EXTRU           div,30,31,div       /* div := floor(y/2) = bits 31..1 of div */
                ADDB,NSV        temp2,div,L$52      /* div := div+1 = y' */
                EXTRU           lorem,31,1,temp3    /* see below (delay slot) */
                /* special case: signed-overflow on addition, i.e. y' = 2^31.
                   division by 2*y' is especially simple here: */
                COPY            hirem,quo           /* quotient := hirem */
                B               L$53
                COPY            lorem,hirem         /* rest := lorem */

L$51            /* division by even y. */
                /* divide x/2 by y/2 , quotient ok, multiply rest with 2, poss. + 1. */
                EXTRU           lorem,31,1,temp3    /* temp3 := Bit 0 of xlo */
L$52            SHD             hirem,lorem,1,lorem /* shift hirem|lorem by 1 bit ... */
                EXTRU           hirem,30,31,hirem   /* ... to the right */
                /* small division (see above): */
                SUB             0,div,temp1
                DS              0,temp1,0
                DS32
                ADDC            lorem,lorem,lorem
                ADD,>=          0,hirem,0
                ADD             hirem,div,hirem
                /* quotient finished in lorem. */
                COPY            lorem,quo           /* quotient q */
                COMB,=          0,temp2,L$55        /* was temp2=0, i.e. y even? */
                SH1ADD          hirem,temp3,hirem   /* yes -> just do hirem:=2*hirem+temp3 and done */
L$53            /* y was even, now, quo = floor(x / 2*y'). */
                   /* calculate quotient and rest:
                      x = quo * 2*y' + hirem = quo * (2*y'-1) + (quo+hirem)
                      quotient = quo, rest = quo+hirem.
                      Still at most 2 times: quotient += 1, rest -= y. */
                ADDB,NUV,N      quo,hirem,L$54      /* addition-overflow -> incerase quotient */
                                                    /* no delay-slot because of ,N ! */
                SUB             hirem,origdiv,hirem /* rest -= y */
                ADDI            1,quo,quo           /* quotient += 1 */
L$54            /* muss the quotient has to be increased at most 1 time because of y>=2^31 : */
                COMB,<<,N       hirem,origdiv,L$55  /* hirem < y -> increase quotient */
                                                    /* no delay-slot because of ,N ! */
                SUB             hirem,origdiv,hirem /* rest -= y */
                ADDI            1,quo,quo           /* quotient += 1 */

L$55            ADDIL           L'divu_32_rest-$global$,%dp
                STW             hirem,R'divu_32_rest-$global$(%r1)
                                                    /* store rest r */
                .LEAVE
                .PROCEND

#endif


                .END

#endif
