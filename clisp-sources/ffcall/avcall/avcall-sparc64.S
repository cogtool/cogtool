#include "asmsparc.h"
	.file	"avcall-sparc64.c"
	.version	"01.01"
.section	".text"
	.align 4
	.global __builtin_avcall
	DECLARE_FUNCTION(__builtin_avcall)
	.type	 __builtin_avcall,$function
	.proc	04
__builtin_avcall:
	!$PROLOGUE$ 0
	save	%sp, -2288, %sp
	!$PROLOGUE$ 1
	ldx	[%i0+40], %g3
	ld	[%i0+60], %o7
	add	%g3, -72, %g3
	add	%sp, 136, %g1
	sub	%g3, %i0, %g3
	cmp	%o7, 0
	sdivx	%g3, 8, %g3
	be,pn	%icc, .LL3
	sra	%g3, 0, %l3
	and	%o7, 1, %g3
	cmp	%g3, 0
	be,pn	%icc, .LL161
	and	%o7, 2, %g3
	ld 72(%i0),%f1
	and	%o7, 2, %g3
.LL161:
	cmp	%g3, 0
	be,pn	%icc, .LL162
	and	%o7, 4, %g3
	ld 80(%i0),%f3
	and	%o7, 4, %g3
.LL162:
	cmp	%g3, 0
	be,pn	%icc, .LL163
	and	%o7, 8, %g3
	ld 88(%i0),%f5
	and	%o7, 8, %g3
.LL163:
	cmp	%g3, 0
	be,pn	%icc, .LL164
	and	%o7, 16, %g3
	ld 96(%i0),%f7
	and	%o7, 16, %g3
.LL164:
	cmp	%g3, 0
	be,pn	%icc, .LL165
	and	%o7, 32, %g3
	ld 104(%i0),%f9
	and	%o7, 32, %g3
.LL165:
	cmp	%g3, 0
	be,pn	%icc, .LL166
	and	%o7, 64, %g3
	ld 112(%i0),%f11
	and	%o7, 64, %g3
.LL166:
	cmp	%g3, 0
	be,pn	%icc, .LL167
	and	%o7, 128, %g3
	ld 120(%i0),%f13
	and	%o7, 128, %g3
.LL167:
	cmp	%g3, 0
	be,pn	%icc, .LL168
	and	%o7, 256, %g3
	ld 128(%i0),%f15
	and	%o7, 256, %g3
.LL168:
	cmp	%g3, 0
	be,pn	%icc, .LL169
	and	%o7, 512, %g3
	ld 136(%i0),%f17
	and	%o7, 512, %g3
.LL169:
	cmp	%g3, 0
	be,a,pn	%icc, .LL170
	ld	[%i0+60], %o7
	ld 144(%i0),%f19
	ld	[%i0+60], %o7
.LL170:
	and	%o7, 1024, %g3
	cmp	%g3, 0
	be,pn	%icc, .LL171
	and	%o7, 2048, %g3
	ld 152(%i0),%f21
	and	%o7, 2048, %g3
.LL171:
	cmp	%g3, 0
	be,pn	%icc, .LL172
	sethi	%hi(4096), %g3
	ld 160(%i0),%f23
	sethi	%hi(4096), %g3
.LL172:
	andcc	%o7, %g3, %g0
	be,pn	%icc, .LL173
	sethi	%hi(8192), %g3
	ld 168(%i0),%f25
	sethi	%hi(8192), %g3
.LL173:
	andcc	%o7, %g3, %g0
	be,pn	%icc, .LL174
	sethi	%hi(16384), %g3
	ld 176(%i0),%f27
	sethi	%hi(16384), %g3
.LL174:
	andcc	%o7, %g3, %g0
	be,pn	%icc, .LL175
	sethi	%hi(32768), %g3
	ld 184(%i0),%f29
	sethi	%hi(32768), %g3
.LL175:
	andcc	%o7, %g3, %g0
	be,a,pn	%icc, .LL176
	ld	[%i0+64], %g3
	ld 192(%i0),%f31
.LL3:
	ld	[%i0+64], %g3
.LL176:
	cmp	%g3, 0
	be,pn	%icc, .LL20
	mov	%g3, %o7
	and	%o7, 1, %g3
	cmp	%g3, 0
	be,pn	%icc, .LL177
	and	%o7, 2, %g3
	ldd 72(%i0),%f0
	and	%o7, 2, %g3
.LL177:
	cmp	%g3, 0
	be,pn	%icc, .LL178
	and	%o7, 4, %g3
	ldd 80(%i0),%f2
	and	%o7, 4, %g3
.LL178:
	cmp	%g3, 0
	be,pn	%icc, .LL179
	and	%o7, 8, %g3
	ldd 88(%i0),%f4
	and	%o7, 8, %g3
.LL179:
	cmp	%g3, 0
	be,pn	%icc, .LL180
	and	%o7, 16, %g3
	ldd 96(%i0),%f6
	and	%o7, 16, %g3
.LL180:
	cmp	%g3, 0
	be,pn	%icc, .LL181
	and	%o7, 32, %g3
	ldd 104(%i0),%f8
	and	%o7, 32, %g3
.LL181:
	cmp	%g3, 0
	be,pn	%icc, .LL182
	and	%o7, 64, %g3
	ldd 112(%i0),%f10
	and	%o7, 64, %g3
.LL182:
	cmp	%g3, 0
	be,pn	%icc, .LL183
	and	%o7, 128, %g3
	ldd 120(%i0),%f12
	and	%o7, 128, %g3
.LL183:
	cmp	%g3, 0
	be,pn	%icc, .LL184
	and	%o7, 256, %g3
	ldd 128(%i0),%f14
	and	%o7, 256, %g3
.LL184:
	cmp	%g3, 0
	be,pn	%icc, .LL185
	and	%o7, 512, %g3
	ldd 136(%i0),%f16
	and	%o7, 512, %g3
.LL185:
	cmp	%g3, 0
	be,pn	%icc, .LL186
	and	%o7, 1024, %g3
	ldd 144(%i0),%f18
	and	%o7, 1024, %g3
.LL186:
	cmp	%g3, 0
	be,pn	%icc, .LL187
	and	%o7, 2048, %g3
	ldd 152(%i0),%f20
	and	%o7, 2048, %g3
.LL187:
	cmp	%g3, 0
	be,pn	%icc, .LL188
	sethi	%hi(4096), %g3
	ldd 160(%i0),%f22
	sethi	%hi(4096), %g3
.LL188:
	andcc	%o7, %g3, %g0
	be,pn	%icc, .LL189
	sethi	%hi(8192), %g3
	ldd 168(%i0),%f24
	sethi	%hi(8192), %g3
.LL189:
	andcc	%o7, %g3, %g0
	be,pn	%icc, .LL190
	sethi	%hi(16384), %g3
	ldd 176(%i0),%f26
	sethi	%hi(16384), %g3
.LL190:
	andcc	%o7, %g3, %g0
	be,pn	%icc, .LL191
	sethi	%hi(32768), %g3
	ldd 184(%i0),%f28
	sethi	%hi(32768), %g3
.LL191:
	andcc	%o7, %g3, %g0
	be,pn	%icc, .LL192
	mov	6, %l1
	ldd 192(%i0),%f30
.LL20:
	mov	6, %l1
.LL192:
	cmp	%l1, %l3
	bge,a,pt %icc, .LL193
	ldx	[%i0], %g3
	add	%i0, 72, %l2
.LL40:
	sllx	%l1, 3, %l0
	add	%l1, 1, %g3
	sra	%g3, 0, %l1
	ldx	[%l2+%l0], %o7
	cmp	%l1, %l3
	bl,pt	%icc, .LL40
	stx	%o7, [%g1+%l0]
	ldx	[%i0], %g3
.LL193:
	ldx	[%i0+72], %o0
	ldx	[%i0+80], %o1
	ldx	[%i0+88], %o2
	ldx	[%i0+96], %o3
	ldx	[%i0+104], %o4
	call	%g3, 0
	ldx	[%i0+112], %o5
	nop
	ld	[%i0+24], %g3
	cmp	%g3, 1
	be,pn	%icc, .LL43
	mov	%o0, %g1
	cmp	%g3, 0
	be,pn	%icc, .LL157
	cmp	%g3, 2
	be,pn	%icc, .LL158
	cmp	%g3, 3
	be,pn	%icc, .LL158
	cmp	%g3, 4
	bne,pt	%icc, .LL50
	cmp	%g3, 5
.LL158:
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%g1, [%g3]
.LL50:
	be,pn	%icc, .LL159
	cmp	%g3, 6
	bne,pt	%icc, .LL54
	cmp	%g3, 7
.LL159:
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	sth	%g1, [%g3]
.LL54:
	be,pn	%icc, .LL160
	cmp	%g3, 8
	bne,pt	%icc, .LL58
	cmp	%g3, 9
.LL160:
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	st	%g1, [%g3]
.LL58:
	be,pn	%icc, .LL157
	cmp	%g3, 10
	be,a,pn	%icc, .LL194
	ldx	[%i0+16], %g3
	ld	[%i0+24], %g3
	cmp	%g3, 11
	be,pn	%icc, .LL157
	cmp	%g3, 12
	be,pn	%icc, .LL157
	cmp	%g3, 13
	bne,pt	%icc, .LL68
	cmp	%g3, 14
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	st	%f0, [%g3]
.LL68:
	bne,pt	%icc, .LL70
	cmp	%g3, 15
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	std	%f0, [%g3]
.LL70:
	bne,pt	%icc, .LL72
	cmp	%g3, 16
.LL157:
	ldx	[%i0+16], %g3
.LL194:
	ba,pt	%xcc, .LL43
	stx	%g1, [%g3]
.LL72:
	bne,pt	%icc, .LL43
	nop
	ld	[%i0+8], %o7
	and	%o7, 1, %g3
	cmp	%g3, 0
	be,pn	%icc, .LL75
	and	%o7, 512, %g3
	ldx	[%i0+32], %g3
	cmp	%g3, 1
	bne,pt	%xcc, .LL76
	cmp	%g3, 2
	ldx	[%i0+16], %o7
	ldub	[%g1], %g3
	ba,pt	%xcc, .LL43
	stb	%g3, [%o7]
.LL76:
	bne,pt	%xcc, .LL78
	cmp	%g3, 4
	ldx	[%i0+16], %o7
	lduh	[%g1], %g3
	ba,pt	%xcc, .LL43
	sth	%g3, [%o7]
.LL78:
	bne,pt	%xcc, .LL80
	cmp	%g3, 8
	ldx	[%i0+16], %o7
	ld	[%g1], %g3
	ba,pt	%xcc, .LL43
	st	%g3, [%o7]
.LL80:
	bne,pt	%xcc, .LL82
	add	%g3, 7, %g3
	ldx	[%i0+16], %o7
	ldx	[%g1], %g3
	ba,pt	%xcc, .LL43
	stx	%g3, [%o7]
.LL82:
	srlx	%g3, 3, %g3
	add	%g3, -1, %g3
	cmp	%g3, 0
	bl,pn	%icc, .LL43
	sra	%g3, 0, %g3
.LL86:
	sllx	%g3, 3, %l0
	add	%g3, -1, %l1
	ldx	[%g1+%l0], %g3
	ldx	[%i0+16], %o7
	stx	%g3, [%o7+%l0]
	cmp	%l1, 0
	bge,pt	%icc, .LL86
	sra	%l1, 0, %g3
	ba,pt	%xcc, .LL43
	 nop
.LL75:
	cmp	%g3, 0
	be,pn	%icc, .LL43
	nop
	ldx	[%i0+32], %o7
	add	%o7, -1, %g3
	cmp	%g3, 31
	bgu,pn	%xcc, .LL43
	cmp	%o7, 1
	bne,pt	%xcc, .LL91
	cmp	%o7, 2
	ldx	[%i0+16], %o7
	srax	%g1, 56, %g3
	ba,pt	%xcc, .LL43
	stb	%g3, [%o7]
.LL91:
	bne,pt	%xcc, .LL93
	cmp	%o7, 3
	ldx	[%i0+16], %o7
	srax	%g1, 56, %g3
	stb	%g3, [%o7]
	srax	%g1, 48, %l0
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l0, [%g3+1]
.LL93:
	bne,pt	%xcc, .LL95
	cmp	%o7, 4
	ldx	[%i0+16], %o7
	srax	%g1, 56, %g3
	stb	%g3, [%o7]
	srax	%g1, 48, %l0
	ldx	[%i0+16], %o7
	srax	%g1, 40, %l1
	stb	%l0, [%o7+1]
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l1, [%g3+2]
.LL95:
	bne,pt	%xcc, .LL97
	cmp	%o7, 5
	ldx	[%i0+16], %o7
	srax	%g1, 56, %g3
	stb	%g3, [%o7]
	srax	%g1, 48, %l0
	ldx	[%i0+16], %g3
	srax	%g1, 40, %l1
	stb	%l0, [%g3+1]
	srax	%g1, 32, %l2
	ldx	[%i0+16], %g3
	stb	%l1, [%g3+2]
	ldx	[%i0+16], %o7
	ba,pt	%xcc, .LL43
	stb	%l2, [%o7+3]
.LL97:
	bne,pt	%xcc, .LL99
	cmp	%o7, 6
	ldx	[%i0+16], %o7
	srax	%g1, 56, %g3
	stb	%g3, [%o7]
	srax	%g1, 48, %l0
	ldx	[%i0+16], %g3
	srax	%g1, 40, %l1
	stb	%l0, [%g3+1]
	srax	%g1, 32, %l2
	ldx	[%i0+16], %o7
	srax	%g1, 24, %l0
	stb	%l1, [%o7+2]
	ldx	[%i0+16], %g3
	stb	%l2, [%g3+3]
	ldx	[%i0+16], %o7
	ba,pt	%xcc, .LL43
	stb	%l0, [%o7+4]
.LL99:
	bne,pt	%xcc, .LL101
	cmp	%o7, 7
	ldx	[%i0+16], %o7
	srax	%g1, 56, %g3
	stb	%g3, [%o7]
	srax	%g1, 48, %l0
	ldx	[%i0+16], %g3
	srax	%g1, 40, %o7
	stb	%l0, [%g3+1]
	srax	%g1, 32, %l1
	ldx	[%i0+16], %g3
	srax	%g1, 24, %l0
	stb	%o7, [%g3+2]
	srax	%g1, 16, %l2
	ldx	[%i0+16], %g3
	stb	%l1, [%g3+3]
	ldx	[%i0+16], %o7
	stb	%l0, [%o7+4]
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l2, [%g3+5]
.LL101:
	bne,pt	%xcc, .LL103
	add	%o7, -8, %g3
	ldx	[%i0+16], %o7
	srax	%g1, 56, %g3
	stb	%g3, [%o7]
	srax	%g1, 48, %l0
	ldx	[%i0+16], %g3
	srax	%g1, 40, %o7
	stb	%l0, [%g3+1]
	srax	%g1, 32, %l1
	ldx	[%i0+16], %g3
	srax	%g1, 24, %l0
	stb	%o7, [%g3+2]
	srax	%g1, 16, %l2
	ldx	[%i0+16], %o7
	srax	%g1, 8, %l3
	stb	%l1, [%o7+3]
	ldx	[%i0+16], %g3
	stb	%l0, [%g3+4]
	ldx	[%i0+16], %o7
	stb	%l2, [%o7+5]
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l3, [%g3+6]
.LL103:
	cmp	%g3, 24
	bgu,pn	%xcc, .LL43
	srax	%g1, 56, %g3
	ldx	[%i0+16], %o7
	stb	%g3, [%o7]
	srax	%g1, 48, %l0
	ldx	[%i0+16], %g3
	srax	%g1, 40, %o7
	stb	%l0, [%g3+1]
	srax	%g1, 32, %l1
	ldx	[%i0+16], %g3
	srax	%g1, 24, %l0
	stb	%o7, [%g3+2]
	srax	%g1, 16, %l2
	ldx	[%i0+16], %o7
	srax	%g1, 8, %l3
	stb	%l1, [%o7+3]
	ldx	[%i0+16], %g3
	stb	%l0, [%g3+4]
	ldx	[%i0+16], %o7
	stb	%l2, [%o7+5]
	ldx	[%i0+16], %g3
	stb	%l3, [%g3+6]
	ldx	[%i0+16], %o7
	stb	%g1, [%o7+7]
	ldx	[%i0+32], %g3
	cmp	%g3, 8
	be,pn	%xcc, .LL43
	cmp	%g3, 9
	bne,pt	%xcc, .LL108
	cmp	%g3, 10
	ldx	[%i0+16], %o7
	srax	%o1, 56, %g3
	ba,pt	%xcc, .LL43
	stb	%g3, [%o7+8]
.LL108:
	bne,pt	%xcc, .LL110
	cmp	%g3, 11
	ldx	[%i0+16], %o7
	srax	%o1, 56, %g3
	stb	%g3, [%o7+8]
	srax	%o1, 48, %l0
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l0, [%g3+9]
.LL110:
	bne,pt	%xcc, .LL112
	cmp	%g3, 12
	ldx	[%i0+16], %o7
	srax	%o1, 56, %g3
	stb	%g3, [%o7+8]
	srax	%o1, 48, %l0
	ldx	[%i0+16], %o7
	srax	%o1, 40, %l1
	stb	%l0, [%o7+9]
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l1, [%g3+10]
.LL112:
	bne,pt	%xcc, .LL114
	cmp	%g3, 13
	ldx	[%i0+16], %o7
	srax	%o1, 56, %g3
	stb	%g3, [%o7+8]
	srax	%o1, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o1, 40, %l1
	stb	%l0, [%g3+9]
	srax	%o1, 32, %l2
	ldx	[%i0+16], %g3
	stb	%l1, [%g3+10]
	ldx	[%i0+16], %o7
	ba,pt	%xcc, .LL43
	stb	%l2, [%o7+11]
.LL114:
	bne,pt	%xcc, .LL116
	cmp	%g3, 14
	ldx	[%i0+16], %o7
	srax	%o1, 56, %g3
	stb	%g3, [%o7+8]
	srax	%o1, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o1, 40, %l1
	stb	%l0, [%g3+9]
	srax	%o1, 32, %l2
	ldx	[%i0+16], %o7
	srax	%o1, 24, %l0
	stb	%l1, [%o7+10]
	ldx	[%i0+16], %g3
	stb	%l2, [%g3+11]
	ldx	[%i0+16], %o7
	ba,pt	%xcc, .LL43
	stb	%l0, [%o7+12]
.LL116:
	bne,pt	%xcc, .LL118
	cmp	%g3, 15
	ldx	[%i0+16], %o7
	srax	%o1, 56, %g3
	stb	%g3, [%o7+8]
	srax	%o1, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o1, 40, %o7
	stb	%l0, [%g3+9]
	srax	%o1, 32, %l1
	ldx	[%i0+16], %g3
	srax	%o1, 24, %l0
	stb	%o7, [%g3+10]
	srax	%o1, 16, %l2
	ldx	[%i0+16], %g3
	stb	%l1, [%g3+11]
	ldx	[%i0+16], %o7
	stb	%l0, [%o7+12]
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l2, [%g3+13]
.LL118:
	bne,pt	%xcc, .LL120
	add	%g3, -16, %g3
	ldx	[%i0+16], %o7
	srax	%o1, 56, %g3
	stb	%g3, [%o7+8]
	srax	%o1, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o1, 40, %o7
	stb	%l0, [%g3+9]
	srax	%o1, 32, %l1
	ldx	[%i0+16], %g3
	srax	%o1, 24, %l0
	stb	%o7, [%g3+10]
	srax	%o1, 16, %l2
	ldx	[%i0+16], %o7
	srax	%o1, 8, %l3
	stb	%l1, [%o7+11]
	ldx	[%i0+16], %g3
	stb	%l0, [%g3+12]
	ldx	[%i0+16], %o7
	stb	%l2, [%o7+13]
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l3, [%g3+14]
.LL120:
	cmp	%g3, 16
	bgu,pn	%xcc, .LL43
	srax	%o1, 56, %g3
	ldx	[%i0+16], %o7
	stb	%g3, [%o7+8]
	srax	%o1, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o1, 40, %o7
	stb	%l0, [%g3+9]
	srax	%o1, 32, %l1
	ldx	[%i0+16], %g3
	srax	%o1, 24, %l0
	stb	%o7, [%g3+10]
	srax	%o1, 16, %l2
	ldx	[%i0+16], %o7
	srax	%o1, 8, %l3
	stb	%l1, [%o7+11]
	ldx	[%i0+16], %g3
	stb	%l0, [%g3+12]
	ldx	[%i0+16], %o7
	stb	%l2, [%o7+13]
	ldx	[%i0+16], %g3
	stb	%l3, [%g3+14]
	ldx	[%i0+16], %o7
	stb	%o1, [%o7+15]
	ldx	[%i0+32], %g3
	cmp	%g3, 16
	be,pn	%xcc, .LL43
	cmp	%g3, 17
	bne,pt	%xcc, .LL125
	cmp	%g3, 18
	ldx	[%i0+16], %o7
	srax	%o2, 56, %g3
	ba,pt	%xcc, .LL43
	stb	%g3, [%o7+16]
.LL125:
	bne,pt	%xcc, .LL127
	cmp	%g3, 19
	ldx	[%i0+16], %o7
	srax	%o2, 56, %g3
	stb	%g3, [%o7+16]
	srax	%o2, 48, %l0
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l0, [%g3+17]
.LL127:
	bne,pt	%xcc, .LL129
	cmp	%g3, 20
	ldx	[%i0+16], %o7
	srax	%o2, 56, %g3
	stb	%g3, [%o7+16]
	srax	%o2, 48, %l0
	ldx	[%i0+16], %o7
	srax	%o2, 40, %l1
	stb	%l0, [%o7+17]
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l1, [%g3+18]
.LL129:
	bne,pt	%xcc, .LL131
	cmp	%g3, 21
	ldx	[%i0+16], %o7
	srax	%o2, 56, %g3
	stb	%g3, [%o7+16]
	srax	%o2, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o2, 40, %l1
	stb	%l0, [%g3+17]
	srax	%o2, 32, %l2
	ldx	[%i0+16], %g3
	stb	%l1, [%g3+18]
	ldx	[%i0+16], %o7
	ba,pt	%xcc, .LL43
	stb	%l2, [%o7+19]
.LL131:
	bne,pt	%xcc, .LL133
	cmp	%g3, 22
	ldx	[%i0+16], %o7
	srax	%o2, 56, %g3
	stb	%g3, [%o7+16]
	srax	%o2, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o2, 40, %l1
	stb	%l0, [%g3+17]
	srax	%o2, 32, %l2
	ldx	[%i0+16], %o7
	srax	%o2, 24, %l0
	stb	%l1, [%o7+18]
	ldx	[%i0+16], %g3
	stb	%l2, [%g3+19]
	ldx	[%i0+16], %o7
	ba,pt	%xcc, .LL43
	stb	%l0, [%o7+20]
.LL133:
	bne,pt	%xcc, .LL135
	cmp	%g3, 23
	ldx	[%i0+16], %o7
	srax	%o2, 56, %g3
	stb	%g3, [%o7+16]
	srax	%o2, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o2, 40, %o7
	stb	%l0, [%g3+17]
	srax	%o2, 32, %l1
	ldx	[%i0+16], %g3
	srax	%o2, 24, %l0
	stb	%o7, [%g3+18]
	srax	%o2, 16, %l2
	ldx	[%i0+16], %g3
	stb	%l1, [%g3+19]
	ldx	[%i0+16], %o7
	stb	%l0, [%o7+20]
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l2, [%g3+21]
.LL135:
	bne,pt	%xcc, .LL137
	add	%g3, -24, %g3
	ldx	[%i0+16], %o7
	srax	%o2, 56, %g3
	stb	%g3, [%o7+16]
	srax	%o2, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o2, 40, %o7
	stb	%l0, [%g3+17]
	srax	%o2, 32, %l1
	ldx	[%i0+16], %g3
	srax	%o2, 24, %l0
	stb	%o7, [%g3+18]
	srax	%o2, 16, %l2
	ldx	[%i0+16], %o7
	srax	%o2, 8, %l3
	stb	%l1, [%o7+19]
	ldx	[%i0+16], %g3
	stb	%l0, [%g3+20]
	ldx	[%i0+16], %o7
	stb	%l2, [%o7+21]
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l3, [%g3+22]
.LL137:
	cmp	%g3, 8
	bgu,pn	%xcc, .LL43
	srax	%o2, 56, %g3
	ldx	[%i0+16], %o7
	stb	%g3, [%o7+16]
	srax	%o2, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o2, 40, %o7
	stb	%l0, [%g3+17]
	srax	%o2, 32, %l1
	ldx	[%i0+16], %g3
	srax	%o2, 24, %l0
	stb	%o7, [%g3+18]
	srax	%o2, 16, %l2
	ldx	[%i0+16], %o7
	srax	%o2, 8, %l3
	stb	%l1, [%o7+19]
	ldx	[%i0+16], %g3
	stb	%l0, [%g3+20]
	ldx	[%i0+16], %o7
	stb	%l2, [%o7+21]
	ldx	[%i0+16], %g3
	stb	%l3, [%g3+22]
	ldx	[%i0+16], %o7
	stb	%o2, [%o7+23]
	ldx	[%i0+32], %g3
	cmp	%g3, 24
	be,pn	%xcc, .LL43
	cmp	%g3, 25
	bne,pt	%xcc, .LL142
	cmp	%g3, 26
	ldx	[%i0+16], %o7
	srax	%o3, 56, %g3
	ba,pt	%xcc, .LL43
	stb	%g3, [%o7+24]
.LL142:
	bne,pt	%xcc, .LL144
	cmp	%g3, 27
	ldx	[%i0+16], %o7
	srax	%o3, 56, %g3
	stb	%g3, [%o7+24]
	srax	%o3, 48, %l0
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l0, [%g3+25]
.LL144:
	bne,pt	%xcc, .LL146
	cmp	%g3, 28
	ldx	[%i0+16], %o7
	srax	%o3, 56, %g3
	stb	%g3, [%o7+24]
	srax	%o3, 48, %l0
	ldx	[%i0+16], %o7
	srax	%o3, 40, %l1
	stb	%l0, [%o7+25]
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l1, [%g3+26]
.LL146:
	bne,pt	%xcc, .LL148
	cmp	%g3, 29
	ldx	[%i0+16], %o7
	srax	%o3, 56, %g3
	stb	%g3, [%o7+24]
	srax	%o3, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o3, 40, %l1
	stb	%l0, [%g3+25]
	srax	%o3, 32, %l2
	ldx	[%i0+16], %g3
	stb	%l1, [%g3+26]
	ldx	[%i0+16], %o7
	ba,pt	%xcc, .LL43
	stb	%l2, [%o7+27]
.LL148:
	bne,pt	%xcc, .LL150
	cmp	%g3, 30
	ldx	[%i0+16], %o7
	srax	%o3, 56, %g3
	stb	%g3, [%o7+24]
	srax	%o3, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o3, 40, %l1
	stb	%l0, [%g3+25]
	srax	%o3, 32, %l2
	ldx	[%i0+16], %o7
	srax	%o3, 24, %l0
	stb	%l1, [%o7+26]
	ldx	[%i0+16], %g3
	stb	%l2, [%g3+27]
	ldx	[%i0+16], %o7
	ba,pt	%xcc, .LL43
	stb	%l0, [%o7+28]
.LL150:
	bne,pt	%xcc, .LL152
	cmp	%g3, 31
	ldx	[%i0+16], %o7
	srax	%o3, 56, %g3
	stb	%g3, [%o7+24]
	srax	%o3, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o3, 40, %o7
	stb	%l0, [%g3+25]
	srax	%o3, 32, %l1
	ldx	[%i0+16], %g3
	srax	%o3, 24, %l0
	stb	%o7, [%g3+26]
	srax	%o3, 16, %l2
	ldx	[%i0+16], %g3
	stb	%l1, [%g3+27]
	ldx	[%i0+16], %o7
	stb	%l0, [%o7+28]
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l2, [%g3+29]
.LL152:
	bne,pt	%xcc, .LL154
	cmp	%g3, 32
	ldx	[%i0+16], %o7
	srax	%o3, 56, %g3
	stb	%g3, [%o7+24]
	srax	%o3, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o3, 40, %o7
	stb	%l0, [%g3+25]
	srax	%o3, 32, %l1
	ldx	[%i0+16], %g3
	srax	%o3, 24, %l0
	stb	%o7, [%g3+26]
	srax	%o3, 16, %l2
	ldx	[%i0+16], %o7
	srax	%o3, 8, %l3
	stb	%l1, [%o7+27]
	ldx	[%i0+16], %g3
	stb	%l0, [%g3+28]
	ldx	[%i0+16], %o7
	stb	%l2, [%o7+29]
	ldx	[%i0+16], %g3
	ba,pt	%xcc, .LL43
	stb	%l3, [%g3+30]
.LL154:
	bne,pt	%xcc, .LL43
	srax	%o3, 56, %g3
	ldx	[%i0+16], %o7
	stb	%g3, [%o7+24]
	srax	%o3, 48, %l0
	ldx	[%i0+16], %g3
	srax	%o3, 40, %o7
	stb	%l0, [%g3+25]
	srax	%o3, 32, %l1
	ldx	[%i0+16], %g3
	srax	%o3, 24, %l0
	stb	%o7, [%g3+26]
	srax	%o3, 16, %l2
	ldx	[%i0+16], %o7
	srax	%o3, 8, %l3
	stb	%l1, [%o7+27]
	ldx	[%i0+16], %g3
	stb	%l0, [%g3+28]
	ldx	[%i0+16], %o7
	stb	%l2, [%o7+29]
	ldx	[%i0+16], %g3
	stb	%l3, [%g3+30]
	ldx	[%i0+16], %o7
	stb	%o3, [%o7+31]
.LL43:
	ret
	restore %g0, 0, %o0
.LLfe1:
	.size	 __builtin_avcall,.LLfe1-__builtin_avcall
	.ident	"GCC: (GNU) 2.95.2 19991024 (release)"
