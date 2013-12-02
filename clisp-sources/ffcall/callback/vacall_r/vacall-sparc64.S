#include "asmsparc.h"
	.file	"vacall-sparc64.c"
	.version	"01.01"
.section	".text"
	.align 4
	.global __vacall_r
	DECLARE_FUNCTION(__vacall_r)
	.type	 __vacall_r,$function
	.proc	020
__vacall_r:
	!$PROLOGUE$ 0
	save	%sp, -448, %sp
	!$PROLOGUE$ 1
	add	%fp, 2223, %o0
	stx	%i5, [%o0-8]
	stx	%i0, [%o0-48]
	stx	%i1, [%o0-40]
	stx	%i2, [%o0-32]
	stx	%i3, [%o0-24]
	stx	%i4, [%o0-16]
	std	%f0, [%fp+1903]
	std	%f2, [%fp+1911]
	std	%f4, [%fp+1919]
	std	%f6, [%fp+1927]
	std	%f8, [%fp+1935]
	std	%f10, [%fp+1943]
	std	%f12, [%fp+1951]
	std	%f14, [%fp+1959]
	std	%f16, [%fp+1967]
	std	%f18, [%fp+1975]
	std	%f20, [%fp+1983]
	std	%f22, [%fp+1991]
	std	%f24, [%fp+1999]
	std	%f26, [%fp+2007]
	std	%f28, [%fp+2015]
	std	%f30, [%fp+2023]
	st	%f1, [%fp+1835]
	st	%f3, [%fp+1839]
	st	%f5, [%fp+1843]
	st	%f7, [%fp+1847]
	st	%f9, [%fp+1851]
	st	%f11, [%fp+1855]
	st	%f13, [%fp+1859]
	st	%f15, [%fp+1863]
	st	%f17, [%fp+1867]
	st	%f19, [%fp+1871]
	st	%f21, [%fp+1875]
	add	%o0, -48, %o0
	st	%f23, [%fp+1879]
	stx	%o0, [%fp+1783]
	st	%f25, [%fp+1883]
	ldx	[%g5+8], %o0
	st	%f27, [%fp+1887]
	st	%f29, [%fp+1891]
	st	%f31, [%fp+1895]
	st	%g0, [%fp+1775]
	stx	%g0, [%fp+1791]
	st	%g0, [%fp+1799]
	st	%g0, [%fp+1831]
	ldx	[%g5], %o2
	call	%o2, 0
	add	%fp, 1775, %o1
	ld	[%fp+1799], %o0
	cmp	%o0, 0
	be,pn	%icc, .LL4
	cmp	%o0, 1
	be,pn	%icc, .LL104
	mov	%o0, %o1
	cmp	%o1, 2
	bne,pt	%icc, .LL7
	cmp	%o1, 3
.LL104:
	ba,pt	%xcc, .LL4
	ldsb	[%fp+1815], %i0
.LL7:
	bne,pt	%icc, .LL9
	cmp	%o1, 4
	ba,pt	%xcc, .LL4
	ldub	[%fp+1815], %i0
.LL9:
	bne,pt	%icc, .LL11
	cmp	%o1, 5
	ba,pt	%xcc, .LL4
	ldsh	[%fp+1815], %i0
.LL11:
	bne,pt	%icc, .LL13
	cmp	%o1, 6
	ba,pt	%xcc, .LL4
	lduh	[%fp+1815], %i0
.LL13:
	bne,pt	%icc, .LL15
	cmp	%o1, 7
	ba,pt	%xcc, .LL4
	ldsw	[%fp+1815], %i0
.LL15:
	bne,pt	%icc, .LL17
	cmp	%o1, 8
	ba,pt	%xcc, .LL4
	lduw	[%fp+1815], %i0
.LL17:
	be,pn	%icc, .LL105
	cmp	%o1, 9
	be,pn	%icc, .LL105
	cmp	%o1, 10
	be,pn	%icc, .LL105
	cmp	%o1, 11
	be,pn	%icc, .LL105
	cmp	%o1, 12
	bne,pt	%icc, .LL27
	cmp	%o1, 13
	ba,pt	%xcc, .LL4
	ld	[%fp+1815], %f0
.LL27:
	bne,pt	%icc, .LL29
	cmp	%o1, 14
	ba,pt	%xcc, .LL4
	ldd	[%fp+1815], %f0
.LL29:
	bne,pt	%icc, .LL31
	cmp	%o1, 15
.LL105:
	ba,pt	%xcc, .LL4
	ldx	[%fp+1815], %i0
.LL31:
	bne,pt	%icc, .LL4
	ld	[%fp+1775], %o0
	mov	%o0, %o1
	and	%o0, 1, %o0
	cmp	%o0, 0
	be,pn	%icc, .LL34
	and	%o1, 1024, %o0
	ba,pt	%xcc, .LL4
	ldx	[%fp+1791], %i0
.LL34:
	cmp	%o0, 0
	be,pn	%icc, .LL4
	ldx	[%fp+1807], %o0
	mov	%o0, %g2
	add	%o0, -1, %o0
	cmp	%o0, 31
	bgu,pn	%xcc, .LL4
	cmp	%g2, 1
	bne,pt	%xcc, .LL38
	cmp	%g2, 2
	ldx	[%fp+1791], %o0
	ldub	[%o0], %o1
	ba,pt	%xcc, .LL4
	sllx	%o1, 56, %i0
.LL38:
	bne,pt	%xcc, .LL40
	cmp	%g2, 3
	ldx	[%fp+1791], %o2
	mov	%o2, %o3
	ldub	[%o2], %o1
	ldub	[%o3+1], %o0
	sllx	%o1, 56, %o1
	sllx	%o0, 48, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i0
.LL40:
	bne,pt	%xcc, .LL42
	cmp	%g2, 4
	ldx	[%fp+1791], %o0
	mov	%o0, %o3
	ldub	[%o0], %o2
	ldub	[%o3+1], %o1
	mov	%o0, %o4
	sllx	%o2, 56, %o2
	sllx	%o1, 48, %o1
	ldub	[%o4+2], %o0
	or	%o2, %o1, %o2
	sllx	%o0, 40, %o0
	ba,pt	%xcc, .LL4
	or	%o2, %o0, %i0
.LL42:
	bne,pt	%xcc, .LL44
	cmp	%g2, 5
	ldx	[%fp+1791], %o2
	mov	%o2, %o3
	ldub	[%o2], %o1
	ldub	[%o3+1], %o0
	mov	%o2, %o4
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	or	%o1, %o0, %o1
	ldub	[%o4+2], %o2
	sllx	%o2, 40, %o2
	ldub	[%o3+3], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 32, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i0
.LL44:
	bne,pt	%xcc, .LL46
	cmp	%g2, 6
	ldx	[%fp+1791], %o2
	mov	%o2, %o3
	ldub	[%o2], %o1
	ldub	[%o3+1], %o0
	mov	%o2, %o4
	sllx	%o0, 48, %o0
	ldub	[%o2+2], %o3
	sllx	%o1, 56, %o1
	sllx	%o3, 40, %o3
	or	%o1, %o0, %o1
	ldub	[%o4+3], %o2
	mov	%o4, %o5
	or	%o1, %o3, %o1
	sllx	%o2, 32, %o2
	ldub	[%o5+4], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 24, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i0
.LL46:
	bne,pt	%xcc, .LL48
	cmp	%g2, 7
	ldx	[%fp+1791], %o2
	mov	%o2, %o3
	ldub	[%o2], %o1
	ldub	[%o3+1], %o0
	mov	%o2, %o4
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	or	%o1, %o0, %o1
	ldub	[%o4+2], %o2
	sllx	%o2, 40, %o2
	ldub	[%o3+3], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 32, %o0
	or	%o1, %o0, %o1
	ldub	[%o4+4], %o2
	sllx	%o2, 24, %o2
	ldub	[%o3+5], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 16, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i0
.LL48:
	bne,pt	%xcc, .LL50
	add	%g2, -8, %o0
	ldx	[%fp+1791], %o2
	mov	%o2, %o3
	ldub	[%o2], %o1
	ldub	[%o3+1], %o0
	mov	%o2, %o4
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	or	%o1, %o0, %o1
	ldub	[%o4+2], %o2
	sllx	%o2, 40, %o2
	ldub	[%o3+3], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 32, %o0
	ldub	[%o4+4], %o3
	mov	%o4, %o5
	or	%o1, %o0, %o1
	sllx	%o3, 24, %o3
	ldub	[%o5+5], %o2
	or	%o1, %o3, %o1
	sllx	%o2, 16, %o2
	ldub	[%o4+6], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 8, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i0
.LL50:
	cmp	%o0, 24
	bgu,pn	%xcc, .LL4
	ldx	[%fp+1791], %o2
	cmp	%g2, 8
	mov	%o2, %o3
	ldub	[%o2], %o0
	ldub	[%o3+1], %o1
	mov	%o2, %o4
	sllx	%o1, 48, %o1
	sllx	%o0, 56, %o0
	or	%o0, %o1, %o0
	ldub	[%o4+2], %o2
	sllx	%o2, 40, %o2
	ldub	[%o3+3], %o1
	or	%o0, %o2, %o0
	sllx	%o1, 32, %o1
	ldub	[%o4+4], %o3
	mov	%o4, %o5
	or	%o0, %o1, %o0
	sllx	%o3, 24, %o3
	ldub	[%o5+5], %o2
	or	%o0, %o3, %o0
	sllx	%o2, 16, %o2
	ldub	[%o4+6], %o1
	or	%o0, %o2, %o0
	sllx	%o1, 8, %o1
	mov	%o4, %o2
	or	%o0, %o1, %o0
	ldub	[%o2+7], %o3
	mov	%o2, %g3
	be,pn	%xcc, .LL4
	or	%o0, %o3, %i0
	cmp	%g2, 9
	bne,pt	%xcc, .LL55
	cmp	%g2, 10
	ldub	[%g3+8], %o0
	ba,pt	%xcc, .LL4
	sllx	%o0, 56, %i1
.LL55:
	bne,pt	%xcc, .LL57
	cmp	%g2, 11
	ldx	[%fp+1791], %o2
	ldub	[%g3+8], %o1
	ldub	[%o2+9], %o0
	sllx	%o1, 56, %o1
	sllx	%o0, 48, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i1
.LL57:
	bne,pt	%xcc, .LL59
	cmp	%g2, 12
	ldx	[%fp+1791], %o0
	ldub	[%g3+8], %o2
	ldub	[%o0+9], %o1
	mov	%o0, %o3
	sllx	%o2, 56, %o2
	sllx	%o1, 48, %o1
	ldub	[%o3+10], %o0
	or	%o2, %o1, %o2
	sllx	%o0, 40, %o0
	ba,pt	%xcc, .LL4
	or	%o2, %o0, %i1
.LL59:
	bne,pt	%xcc, .LL61
	cmp	%g2, 13
	ldx	[%fp+1791], %o2
	ldub	[%g3+8], %o1
	ldub	[%o2+9], %o0
	mov	%o2, %o3
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	or	%o1, %o0, %o1
	ldub	[%o3+10], %o2
	mov	%o3, %o4
	sllx	%o2, 40, %o2
	ldub	[%o4+11], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 32, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i1
.LL61:
	bne,pt	%xcc, .LL63
	cmp	%g2, 14
	ldx	[%fp+1791], %o2
	ldub	[%g3+8], %o1
	mov	%o2, %o4
	ldub	[%o2+9], %o0
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	ldub	[%o4+10], %o3
	mov	%o2, %o5
	or	%o1, %o0, %o1
	sllx	%o3, 40, %o3
	ldub	[%o5+11], %o2
	or	%o1, %o3, %o1
	sllx	%o2, 32, %o2
	ldub	[%o4+12], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 24, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i1
.LL63:
	bne,pt	%xcc, .LL65
	cmp	%g2, 15
	ldx	[%fp+1791], %o2
	ldub	[%g3+8], %o1
	ldub	[%o2+9], %o0
	mov	%o2, %o3
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	mov	%o3, %o4
	or	%o1, %o0, %o1
	ldub	[%o3+10], %o2
	ldub	[%o4+11], %o0
	sllx	%o2, 40, %o2
	sllx	%o0, 32, %o0
	or	%o1, %o2, %o1
	or	%o1, %o0, %o1
	ldub	[%o3+12], %o2
	sllx	%o2, 24, %o2
	ldub	[%o4+13], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 16, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i1
.LL65:
	bne,pt	%xcc, .LL67
	add	%g2, -16, %o0
	ldx	[%fp+1791], %o2
	ldub	[%g3+8], %o1
	ldub	[%o2+9], %o0
	mov	%o2, %o3
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	mov	%o3, %o4
	ldub	[%o3+10], %o2
	or	%o1, %o0, %o1
	sllx	%o2, 40, %o2
	mov	%o3, %o5
	ldub	[%o4+11], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 32, %o0
	ldub	[%o5+12], %o3
	or	%o1, %o0, %o1
	sllx	%o3, 24, %o3
	ldub	[%o4+13], %o2
	or	%o1, %o3, %o1
	sllx	%o2, 16, %o2
	ldub	[%o5+14], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 8, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i1
.LL67:
	cmp	%o0, 16
	bgu,pn	%xcc, .LL4
	ldx	[%fp+1791], %o2
	ldub	[%g3+8], %o0
	ldub	[%o2+9], %o1
	mov	%o2, %o3
	sllx	%o1, 48, %o1
	sllx	%o0, 56, %o0
	mov	%o3, %o4
	ldub	[%o3+10], %o2
	or	%o0, %o1, %o0
	sllx	%o2, 40, %o2
	mov	%o3, %o5
	ldub	[%o4+11], %o1
	or	%o0, %o2, %o0
	sllx	%o1, 32, %o1
	ldub	[%o5+12], %o3
	or	%o0, %o1, %o0
	sllx	%o3, 24, %o3
	ldub	[%o4+13], %o2
	or	%o0, %o3, %o0
	sllx	%o2, 16, %o2
	ldub	[%o5+14], %o1
	or	%o0, %o2, %o0
	sllx	%o1, 8, %o1
	mov	%o4, %o3
	or	%o0, %o1, %o0
	ldub	[%o3+15], %o2
	cmp	%g2, 16
	be,pn	%xcc, .LL4
	or	%o0, %o2, %i1
	cmp	%g2, 17
	bne,pt	%xcc, .LL72
	cmp	%g2, 18
	ldub	[%g3+16], %o0
	ba,pt	%xcc, .LL4
	sllx	%o0, 56, %i2
.LL72:
	bne,pt	%xcc, .LL74
	cmp	%g2, 19
	ldx	[%fp+1791], %o2
	ldub	[%g3+16], %o1
	ldub	[%o2+17], %o0
	sllx	%o1, 56, %o1
	sllx	%o0, 48, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i2
.LL74:
	bne,pt	%xcc, .LL76
	cmp	%g2, 20
	ldx	[%fp+1791], %o0
	ldub	[%g3+16], %o2
	ldub	[%o0+17], %o1
	mov	%o0, %o3
	sllx	%o2, 56, %o2
	sllx	%o1, 48, %o1
	ldub	[%o3+18], %o0
	or	%o2, %o1, %o2
	sllx	%o0, 40, %o0
	ba,pt	%xcc, .LL4
	or	%o2, %o0, %i2
.LL76:
	bne,pt	%xcc, .LL78
	cmp	%g2, 21
	ldx	[%fp+1791], %o2
	ldub	[%g3+16], %o1
	ldub	[%o2+17], %o0
	mov	%o2, %o3
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	or	%o1, %o0, %o1
	ldub	[%o3+18], %o2
	mov	%o3, %o4
	sllx	%o2, 40, %o2
	ldub	[%o4+19], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 32, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i2
.LL78:
	bne,pt	%xcc, .LL80
	cmp	%g2, 22
	ldx	[%fp+1791], %o2
	ldub	[%g3+16], %o1
	mov	%o2, %o4
	ldub	[%o2+17], %o0
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	ldub	[%o4+18], %o3
	mov	%o2, %o5
	or	%o1, %o0, %o1
	sllx	%o3, 40, %o3
	ldub	[%o5+19], %o2
	or	%o1, %o3, %o1
	sllx	%o2, 32, %o2
	ldub	[%o4+20], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 24, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i2
.LL80:
	bne,pt	%xcc, .LL82
	cmp	%g2, 23
	ldx	[%fp+1791], %o2
	ldub	[%g3+16], %o1
	ldub	[%o2+17], %o0
	mov	%o2, %o3
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	mov	%o3, %o4
	or	%o1, %o0, %o1
	ldub	[%o3+18], %o2
	ldub	[%o4+19], %o0
	sllx	%o2, 40, %o2
	sllx	%o0, 32, %o0
	or	%o1, %o2, %o1
	or	%o1, %o0, %o1
	ldub	[%o3+20], %o2
	sllx	%o2, 24, %o2
	ldub	[%o4+21], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 16, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i2
.LL82:
	bne,pt	%xcc, .LL84
	add	%g2, -24, %o0
	ldx	[%fp+1791], %o2
	ldub	[%g3+16], %o1
	ldub	[%o2+17], %o0
	mov	%o2, %o3
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	mov	%o3, %o4
	ldub	[%o3+18], %o2
	or	%o1, %o0, %o1
	sllx	%o2, 40, %o2
	mov	%o3, %o5
	ldub	[%o4+19], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 32, %o0
	ldub	[%o5+20], %o3
	or	%o1, %o0, %o1
	sllx	%o3, 24, %o3
	ldub	[%o4+21], %o2
	or	%o1, %o3, %o1
	sllx	%o2, 16, %o2
	ldub	[%o5+22], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 8, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i2
.LL84:
	cmp	%o0, 8
	bgu,pn	%xcc, .LL4
	ldx	[%fp+1791], %o2
	ldub	[%g3+16], %o0
	ldub	[%o2+17], %o1
	mov	%o2, %o3
	sllx	%o1, 48, %o1
	sllx	%o0, 56, %o0
	mov	%o3, %o4
	ldub	[%o3+18], %o2
	or	%o0, %o1, %o0
	sllx	%o2, 40, %o2
	mov	%o3, %o5
	ldub	[%o4+19], %o1
	or	%o0, %o2, %o0
	sllx	%o1, 32, %o1
	ldub	[%o5+20], %o3
	or	%o0, %o1, %o0
	sllx	%o3, 24, %o3
	ldub	[%o4+21], %o2
	or	%o0, %o3, %o0
	sllx	%o2, 16, %o2
	ldub	[%o5+22], %o1
	or	%o0, %o2, %o0
	sllx	%o1, 8, %o1
	mov	%o4, %o3
	or	%o0, %o1, %o0
	ldub	[%o3+23], %o2
	cmp	%g2, 24
	be,pn	%xcc, .LL4
	or	%o0, %o2, %i2
	cmp	%g2, 25
	bne,pt	%xcc, .LL89
	cmp	%g2, 26
	ldub	[%g3+24], %o0
	ba,pt	%xcc, .LL4
	sllx	%o0, 56, %i3
.LL89:
	bne,pt	%xcc, .LL91
	cmp	%g2, 27
	ldx	[%fp+1791], %o2
	ldub	[%g3+24], %o1
	ldub	[%o2+25], %o0
	sllx	%o1, 56, %o1
	sllx	%o0, 48, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i3
.LL91:
	bne,pt	%xcc, .LL93
	cmp	%g2, 28
	ldx	[%fp+1791], %o0
	ldub	[%g3+24], %o2
	ldub	[%o0+25], %o1
	mov	%o0, %o3
	sllx	%o2, 56, %o2
	sllx	%o1, 48, %o1
	ldub	[%o3+26], %o0
	or	%o2, %o1, %o2
	ba,pt	%xcc, .LL106
	sllx	%o0, 40, %o0
.LL93:
	bne,pt	%xcc, .LL95
	cmp	%g2, 29
	ldx	[%fp+1791], %o2
	ldub	[%g3+24], %o1
	ldub	[%o2+25], %o0
	mov	%o2, %o3
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	or	%o1, %o0, %o1
	ldub	[%o3+26], %o2
	mov	%o3, %o4
	sllx	%o2, 40, %o2
	ldub	[%o4+27], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 32, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i3
.LL95:
	bne,pt	%xcc, .LL97
	cmp	%g2, 30
	ldx	[%fp+1791], %o2
	ldub	[%g3+24], %o1
	mov	%o2, %o4
	ldub	[%o2+25], %o0
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	ldub	[%o4+26], %o3
	mov	%o2, %o5
	or	%o1, %o0, %o1
	sllx	%o3, 40, %o3
	ldub	[%o5+27], %o2
	or	%o1, %o3, %o1
	sllx	%o2, 32, %o2
	ldub	[%o4+28], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 24, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i3
.LL97:
	bne,pt	%xcc, .LL99
	cmp	%g2, 31
	ldx	[%fp+1791], %o2
	ldub	[%g3+24], %o1
	ldub	[%o2+25], %o0
	mov	%o2, %o3
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	mov	%o3, %o4
	or	%o1, %o0, %o1
	ldub	[%o3+26], %o2
	ldub	[%o4+27], %o0
	sllx	%o2, 40, %o2
	sllx	%o0, 32, %o0
	or	%o1, %o2, %o1
	or	%o1, %o0, %o1
	ldub	[%o3+28], %o2
	sllx	%o2, 24, %o2
	ldub	[%o4+29], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 16, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i3
.LL99:
	bne,pt	%xcc, .LL101
	cmp	%g2, 32
	ldx	[%fp+1791], %o2
	ldub	[%g3+24], %o1
	ldub	[%o2+25], %o0
	mov	%o2, %o3
	sllx	%o0, 48, %o0
	sllx	%o1, 56, %o1
	mov	%o3, %o4
	ldub	[%o3+26], %o2
	or	%o1, %o0, %o1
	sllx	%o2, 40, %o2
	mov	%o3, %o5
	ldub	[%o4+27], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 32, %o0
	ldub	[%o5+28], %o3
	or	%o1, %o0, %o1
	sllx	%o3, 24, %o3
	ldub	[%o4+29], %o2
	or	%o1, %o3, %o1
	sllx	%o2, 16, %o2
	ldub	[%o5+30], %o0
	or	%o1, %o2, %o1
	sllx	%o0, 8, %o0
	ba,pt	%xcc, .LL4
	or	%o1, %o0, %i3
.LL101:
	bne,pt	%xcc, .LL4
	ldx	[%fp+1791], %o2
	ldub	[%g3+24], %o0
	ldub	[%o2+25], %o1
	mov	%o2, %o3
	sllx	%o1, 48, %o1
	sllx	%o0, 56, %o0
	mov	%o3, %o4
	ldub	[%o3+26], %o2
	or	%o0, %o1, %o0
	sllx	%o2, 40, %o2
	mov	%o3, %o5
	ldub	[%o4+27], %o1
	or	%o0, %o2, %o0
	sllx	%o1, 32, %o1
	ldub	[%o5+28], %o3
	or	%o0, %o1, %o0
	sllx	%o3, 24, %o3
	ldub	[%o4+29], %o2
	or	%o0, %o3, %o0
	sllx	%o2, 16, %o2
	ldub	[%o5+30], %o1
	or	%o0, %o2, %o0
	sllx	%o1, 8, %o1
	mov	%o4, %o3
	or	%o0, %o1, %o0
	ldub	[%o3+31], %o2
.LL106:
	or	%o0, %o2, %i3
.LL4:
	return	%i7+8
	nop
.LLfe1:
	.size	 __vacall_r,.LLfe1-__vacall_r
	.ident	"GCC: (GNU) 2.95.2 19991024 (release)"
