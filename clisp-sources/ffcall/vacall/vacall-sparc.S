#include "asmsparc.h"
.text
	.align 4
	.global C(__vacall)
	DECLARE_FUNCTION(__vacall)
	.proc	020
C(__vacall:)
	!$PROLOGUE$ 0
	save	%sp, -144, %sp
	!$PROLOGUE$ 1
	add	%fp, 92, %o0
	st	%i2, [%o0-16]
	st	%i3, [%o0-12]
	st	%i4, [%o0-8]
	st	%i5, [%o0-4]
	st	%i0, [%o0-24]
	st	%i1, [%o0-20]
	add	%o0, -24, %o1
	st	%o1, [%fp-44]
	st	%g0, [%fp-48]
	st	%g0, [%fp-40]
	st	%g0, [%fp-36]
	ld	[%o0-28], %o2
	sethi	%hi(C(vacall_function)), %o1
	ld	[%o1+%lo(C(vacall_function))], %o3
	add	%fp, -48, %o0
	call	%o3, 0
	st	%o2, [%fp-16]
	ld	[%fp-36], %o0
	cmp	%o0, 0
	be	L4
	cmp	%o0, 1
	be	L46
	mov	%o0, %o1
	cmp	%o1, 2
	bne	L7
	cmp	%o1, 3
L46:
	b	L4
	ldsb	[%fp-24], %i0
L7:
	bne	L9
	cmp	%o1, 4
	b	L4
	ldub	[%fp-24], %i0
L9:
	bne	L11
	cmp	%o1, 5
	b	L4
	ldsh	[%fp-24], %i0
L11:
	bne	L13
	cmp	%o1, 6
	b	L4
	lduh	[%fp-24], %i0
L13:
	be	L47
	cmp	%o1, 7
	be	L47
	cmp	%o1, 8
	be	L47
	cmp	%o1, 9
	be	L47
	add	%o1, -10, %o0
	cmp	%o0, 1
	bgu	L23
	cmp	%o1, 12
	ld	[%fp-24], %i0
	b	L4
	ld	[%fp-20], %i1
L23:
	bne	L25
	cmp	%o1, 13
	ld	[%fp-48], %o0
	andcc	%o0, 32, %g0
	be	L26
	ld	[%fp-24], %f2
	b	L4
	fstod	%f2, %f0
L26:
	b	L4
	ld	[%fp-24], %f0
L25:
	bne	L29
	cmp	%o1, 14
	b	L4
	ldd	[%fp-24], %f0
L29:
	bne	L31
	cmp	%o1, 15
L47:
	b	L4
	ld	[%fp-24], %i0
L31:
	bne	L4
	ld	[%fp-48], %o0
	andcc	%o0, 24, %g0
	be	L34
	mov	%o0, %o1
	ld	[%fp-32], %o0
	ld	[%i7+8], %o1
	and	%o0, 4095, %o0
	cmp	%o0, %o1
	bne	L4
	ld	[%fp-40], %i0
	b	L4
	add	%i7, 4, %i7
L34:
	andcc	%o1, 1, %g0
	be	L37
	andcc	%o1, 2, %g0
	b	L4
	ld	[%fp-40], %i0
L37:
	be	L39
	ld	[%fp-32], %o0
	cmp	%o0, 1
	bne	L40
	mov	%o0, %o1
	ld	[%fp-40], %o0
	b	L4
	ldub	[%o0], %i0
L40:
	cmp	%o1, 2
	bne	L43
	cmp	%o1, 4
	ld	[%fp-40], %o0
	b	L4
	lduh	[%o0], %i0
L43:
	bne,a	L4
	add	%i7, 4, %i7
	ld	[%fp-40], %o0
	b	L4
	ld	[%o0], %i0
L39:
	add	%i7, 4, %i7
L4:
	ret
	restore
