#include "asmsparc.h"
.text
	.align 4
	.global C(__builtin_avcall)
	DECLARE_FUNCTION(__builtin_avcall)
	.proc	04
C(__builtin_avcall:)
	!$PROLOGUE$ 0
	save	%sp, -1152, %sp
	!$PROLOGUE$ 1
	ld	[%i0+20], %g3
	add	%sp, 68, %l2
	add	%g3, -40, %g3
	ld	[%i0+12], %o7
	sub	%g3, %i0, %g3
	cmp	%o7, 16
	bne	L3
	sra	%g3, 2, %l0
	ld	[%i0+8], %g3
	st	%g3, [%l2-4]
L3:
	cmp	%l0, 6
	ble,a	L66
	ld	[%i0+12], %g3
	add	%l0, -6, %l0
	add	%i0, 40, %l1
	mov	24, %o7
L7:
	ld	[%l1+%o7], %g3
	addcc	%l0, -1, %l0
	st	%g3, [%o7+%l2]
	bne	L7
	add	%o7, 4, %o7
	ld	[%i0+12], %g3
L66:
	cmp	%g3, 16
	bne,a	L67
	ld	[%i0], %g3
	ld	[%i0+4], %g3
	andcc	%g3, 16, %g0
	be	L9
	sethi	%hi(-2117607424), %o7
	ld	[%i0+16], %g3
	and	%g3, 4095, %g3
	or	%o7, 8, %o7
	sethi	%hi(-1614774272), %l0
	sethi	%hi(16777216), %l1
	sethi	%hi(-1341120512), %l2
	sethi	%hi(-2115502080), %l3
	st	%g3, [%fp-24]
	std	%l0, [%fp-32]
	st	%l2, [%fp-20]
	st	%o7, [%fp-16]
	st	%l3, [%fp-12]
	add	%fp, -32, %g3
	iflush %g3
	add	%fp, -24, %o7
	iflush %o7
	add	%fp, -16, %g3
	iflush %g3
	add	%fp, -12, %o7
	iflush %o7
	ld	[%i0], %g2
	ld	[%i0+40], %o0
	ld	[%i0+44], %o1
	ld	[%i0+48], %o2
	ld	[%i0+52], %o3
	ld	[%i0+56], %o4
	jmp	%fp-32
	ld	[%i0+60], %o5
L9:
	ld	[%i0], %g3
L67:
	ld	[%i0+40], %o0
	ld	[%i0+44], %o1
	ld	[%i0+48], %o2
	ld	[%i0+52], %o3
	ld	[%i0+56], %o4
	call	%g3, 0
	ld	[%i0+60], %o5
	nop
	ld	[%i0+12], %g3
	cmp	%g3, 1
	be	L11
	mov	%o0, %l2
	cmp	%g3, 0
	be	L63
	cmp	%g3, 2
	be	L64
	cmp	%g3, 3
	be	L64
	cmp	%g3, 4
	be	L64
	cmp	%g3, 5
	be	L65
	cmp	%g3, 6
	be	L65
	cmp	%g3, 7
	be	L63
	cmp	%g3, 8
	be	L63
	cmp	%g3, 9
	be	L63
	cmp	%g3, 10
	be,a	L68
	ld	[%i0+8], %g3
	ld	[%i0+12], %o7
	add	%o7, -11, %g3
	cmp	%g3, 1
	bgu	L32
	cmp	%o7, 13
	ld	[%i0+8], %g3
	st	%l2, [%g3]
	ld	[%i0+8], %o7
	b	L11
	st	%o1, [%o7+4]
L32:
	bne	L34
	cmp	%o7, 14
	ld	[%i0+4], %g3
	andcc	%g3, 32, %g0
	bne,a	L35
	fdtos	%f0, %f0
L35:
	ld	[%i0+8], %g3
	b	L11
	st	%f0, [%g3]
L34:
	bne	L38
	cmp	%o7, 15
	ld	[%i0+8], %g3
	b	L11
	std	%f0, [%g3]
L38:
	be	L63
	cmp	%o7, 16
	bne	L11
	nop
	ld	[%i0+4], %g3
	andcc	%g3, 1, %g0
	be	L43
	andcc	%g3, 2, %g0
	ld	[%i0+16], %g3
	cmp	%g3, 1
	bne	L44
	cmp	%g3, 2
	ld	[%i0+8], %o7
	ldub	[%l2], %g3
	b	L11
	stb	%g3, [%o7]
L44:
	bne	L46
	cmp	%g3, 4
	ld	[%i0+8], %o7
	lduh	[%l2], %g3
	b	L11
	sth	%g3, [%o7]
L46:
	bne	L48
	cmp	%g3, 8
	ld	[%i0+8], %o7
	ld	[%l2], %g3
	b	L11
	st	%g3, [%o7]
L48:
	bne	L50
	add	%g3, 3, %g3
	ld	[%l2], %g3
	ld	[%i0+8], %o7
	st	%g3, [%o7]
	ld	[%i0+8], %l0
	ld	[%l2+4], %g3
	b	L11
	st	%g3, [%l0+4]
L50:
	srl	%g3, 2, %l1
	addcc	%l1, -1, %l1
	bneg	L11
	sll	%l1, 2, %l0
L54:
	ld	[%l0+%l2], %o7
	addcc	%l1, -1, %l1
	ld	[%i0+8], %g3
	st	%o7, [%g3+%l0]
	bpos	L54
	add	%l0, -4, %l0
	b,a	L11
L43:
	be	L11
	nop
	ld	[%i0+16], %g3
	cmp	%g3, 1
	bne	L58
	cmp	%g3, 2
L64:
	ld	[%i0+8], %g3
	b	L11
	stb	%l2, [%g3]
L58:
	bne	L60
	cmp	%g3, 4
L65:
	ld	[%i0+8], %g3
	b	L11
	sth	%l2, [%g3]
L60:
	bne	L11
	nop
L63:
	ld	[%i0+8], %g3
L68:
	st	%l2, [%g3]
L11:
	ret
	restore %g0, 0, %o0
