#include "asmmips.h"
	
	.set	nobopt
	.option pic2
	.section	.text
	.text
	.align	2
	.globl	__builtin_avcall
	DECLARE_FUNCTION(__builtin_avcall)
	.ent	__builtin_avcall
__builtin_avcall:
.LFB1:
	.frame	$fp,2096,$31		
	.mask	0xd0010000,-8
	.fmask	0x00000000,0
	subu	$sp,$sp,2096
.LCFI0:
	sd	$fp,2080($sp)
.LCFI1:
	move	$fp,$sp
.LCFI2:
	sd	$31,2088($sp)
.LCFI3:
	sd	$28,2072($sp)
.LCFI4:
	sd	$16,2064($sp)
.LCFI5:
	.set	noat
	lui	$1,%hi(%neg(%gp_rel(__builtin_avcall)))
	addiu	$1,$1,%lo(%neg(%gp_rel(__builtin_avcall)))
	daddu	$gp,$1,$25
	.set	at
	move	$16,$4
	lw	$2,20($16)
	move	$8,$sp
	lw	$3,28($16)
	addu	$2,$2,-72
	subu	$2,$2,$16
	.set	noreorder
	.set	nomacro
	beq	$3,$0,.L3
	sra	$4,$2,3
	.set	macro
	.set	reorder

	andi	$2,$3,0x1
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L121
	andi	$2,$3,0x2
	.set	macro
	.set	reorder

 
	lwc1 $f12,36($16)
 
	andi	$2,$3,0x2
.L121:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L122
	andi	$2,$3,0x4
	.set	macro
	.set	reorder

 
	lwc1 $f13,40($16)
 
	andi	$2,$3,0x4
.L122:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L123
	andi	$2,$3,0x8
	.set	macro
	.set	reorder

 
	lwc1 $f14,44($16)
 
	andi	$2,$3,0x8
.L123:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L124
	andi	$2,$3,0x10
	.set	macro
	.set	reorder

 
	lwc1 $f15,48($16)
 
	andi	$2,$3,0x10
.L124:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L125
	andi	$2,$3,0x20
	.set	macro
	.set	reorder

 
	lwc1 $f16,52($16)
 
	andi	$2,$3,0x20
.L125:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L126
	andi	$2,$3,0x40
	.set	macro
	.set	reorder

 
	lwc1 $f17,56($16)
 
	andi	$2,$3,0x40
.L126:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L127
	andi	$2,$3,0x80
	.set	macro
	.set	reorder

 
	lwc1 $f18,60($16)
 
	andi	$2,$3,0x80
.L127:
	.set	noreorder
	.set	nomacro
	beql	$2,$0,.L128
	lw	$3,32($16)
	.set	macro
	.set	reorder

 
	lwc1 $f19,64($16)
 
.L3:
	
	lw	$3,32($16)
.L128:
	.set	noreorder
	.set	nomacro
	beq	$3,$0,.L12
	andi	$2,$3,0x1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L129
	andi	$2,$3,0x2
	.set	macro
	.set	reorder

 
	ldc1 $f12,72($16)
 
	andi	$2,$3,0x2
.L129:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L130
	andi	$2,$3,0x4
	.set	macro
	.set	reorder

 
	ldc1 $f13,80($16)
 
	andi	$2,$3,0x4
.L130:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L131
	andi	$2,$3,0x8
	.set	macro
	.set	reorder

 
	ldc1 $f14,88($16)
 
	andi	$2,$3,0x8
.L131:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L132
	andi	$2,$3,0x10
	.set	macro
	.set	reorder

 
	ldc1 $f15,96($16)
 
	andi	$2,$3,0x10
.L132:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L133
	andi	$2,$3,0x20
	.set	macro
	.set	reorder

 
	ldc1 $f16,104($16)
 
	andi	$2,$3,0x20
.L133:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L134
	andi	$2,$3,0x40
	.set	macro
	.set	reorder

 
	ldc1 $f17,112($16)
 
	andi	$2,$3,0x40
.L134:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L135
	andi	$2,$3,0x80
	.set	macro
	.set	reorder

 
	ldc1 $f18,120($16)
 
	andi	$2,$3,0x80
.L135:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L136
	dli	$7,0x8		
	.set	macro
	.set	reorder

 
	ldc1 $f19,128($16)
 
.L12:
	dli	$7,0x8		
.L136:
	move	$5,$4
	slt	$2,$7,$5
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L22
	addu	$6,$16,72
	.set	macro
	.set	reorder

.L24:
	dsll	$3,$7,32
	dsra	$3,$3,32
	sll	$3,$3,3
	addu	$2,$6,$3
	ld	$4,0($2)
	daddu	$7,$7,1
	addu	$3,$3,$8
	slt	$2,$7,$5
	.set	noreorder
	.set	nomacro
	bne	$2,$0,.L24
	sd	$4,-64($3)
	.set	macro
	.set	reorder

.L22:
 
	ld $4,72($16)
	ld $5,80($16)
	ld $6,88($16)
	ld $7,96($16)
	ld $8,104($16)
	ld $9,112($16)
	ld $10,120($16)
	ld $11,128($16)
 
	lw	$25,0($16)
	
	jal	$31,$25
	lw	$4,12($16)
	move	$7,$2
	li	$2,1			
	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L120
	move	$2,$0
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$0,.L116
	li	$2,2			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L117
	li	$2,3			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L117
	li	$2,4			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L117
	li	$2,5			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L118
	li	$2,6			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L118
	li	$2,7			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L119
	li	$2,8			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L119
	li	$2,9			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L119
	li	$2,10			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L119
	li	$2,11			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L116
	li	$2,12			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L116
	li	$2,13			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L52
	li	$2,14			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	b	.L27
	s.s	$f0,0($2)
	.set	macro
	.set	reorder

.L52:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L54
	li	$2,15			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	
	s.d	$f0,0($2)
	.set	noreorder
	.set	nomacro
	b	.L120
	move	$2,$0
	.set	macro
	.set	reorder

.L54:
	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L119
	li	$2,16			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L120
	move	$2,$0
	.set	macro
	.set	reorder

	lw	$4,4($16)
	
	andi	$2,$4,0x1
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L59
	li	$2,1			
	.set	macro
	.set	reorder

	lw	$3,16($16)
	
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L60
	li	$2,2			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	dsll	$3,$7,32
	dsra	$3,$3,32
	lbu	$4,0($3)
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$4,0($2)
	.set	macro
	.set	reorder

.L60:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L62
	li	$2,4			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	dsll	$3,$7,32
	dsra	$3,$3,32
	lhu	$4,0($3)
	.set	noreorder
	.set	nomacro
	b	.L27
	sh	$4,0($2)
	.set	macro
	.set	reorder

.L62:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L64
	li	$2,8			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	dsll	$3,$7,32
	dsra	$3,$3,32
	lw	$4,0($3)
	.set	noreorder
	.set	nomacro
	b	.L27
	sw	$4,0($2)
	.set	macro
	.set	reorder

.L64:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L66
	addu	$2,$3,7
	.set	macro
	.set	reorder

	lw	$2,8($16)
	dsll	$3,$7,32
	dsra	$3,$3,32
	lw	$4,4($3)
	.set	noreorder
	.set	nomacro
	b	.L27
	sw	$4,0($2)
	.set	macro
	.set	reorder

.L66:
	srl	$5,$2,3
	addu	$5,$5,-1
	.set	noreorder
	.set	nomacro
	bltz	$5,.L27
	sll	$2,$5,3
	.set	macro
	.set	reorder

	dsll	$3,$7,32
	dsra	$3,$3,32
	addu	$6,$2,$3
.L70:
	ld	$4,0($6)
	addu	$6,$6,-8
	lw	$3,8($16)
	sll	$2,$5,3
	addu	$5,$5,-1
	addu	$2,$2,$3
	.set	noreorder
	.set	nomacro
	bgez	$5,.L70
	sd	$4,0($2)
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L120
	move	$2,$0
	.set	macro
	.set	reorder

.L59:
	andi	$2,$4,0x200
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L27
	andi	$2,$4,0x4
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L74
	li	$2,1			
	.set	macro
	.set	reorder

	lw	$3,16($16)
	
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L75
	li	$2,2			
	.set	macro
	.set	reorder

.L117:
	lw	$3,8($16)
	andi	$2,$7,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$2,0($3)
	.set	macro
	.set	reorder

.L75:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L77
	li	$2,4			
	.set	macro
	.set	reorder

.L118:
	lw	$3,8($16)
	andi	$2,$7,0xffff
	.set	noreorder
	.set	nomacro
	b	.L27
	sh	$2,0($3)
	.set	macro
	.set	reorder

.L77:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L79
	li	$2,8			
	.set	macro
	.set	reorder

.L119:
	lw	$3,8($16)
	dsll	$2,$7,32
	dsra	$2,$2,32
	.set	noreorder
	.set	nomacro
	b	.L27
	sw	$2,0($3)
	.set	macro
	.set	reorder

.L79:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,.L120
	move	$2,$0
	.set	macro
	.set	reorder

.L116:
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	b	.L27
	sd	$7,0($2)
	.set	macro
	.set	reorder

.L74:
	lw	$4,16($16)
	
	addu	$2,$4,-1
	sltu	$2,$2,16
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L27
	li	$2,1			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L84
	li	$2,2			
	.set	macro
	.set	reorder

	lw	$3,8($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$2,0($3)
	.set	macro
	.set	reorder

.L84:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L86
	li	$2,3			
	.set	macro
	.set	reorder

	lw	$3,8($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	lw	$4,8($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$2,1($4)
	.set	macro
	.set	reorder

.L86:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L88
	li	$2,4			
	.set	macro
	.set	reorder

	lw	$3,8($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	lw	$4,8($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	sb	$2,1($4)
	lw	$3,8($16)
	dsra	$2,$7,40
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$2,2($3)
	.set	macro
	.set	reorder

.L88:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L90
	li	$2,5			
	.set	macro
	.set	reorder

	lw	$3,8($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	lw	$4,8($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	sb	$2,1($4)
	lw	$3,8($16)
	dsra	$2,$7,40
	andi	$2,$2,0x00ff
	sb	$2,2($3)
	lw	$4,8($16)
	dsra	$2,$7,32
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$2,3($4)
	.set	macro
	.set	reorder

.L90:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L92
	li	$2,6			
	.set	macro
	.set	reorder

	lw	$3,8($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	lw	$4,8($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	sb	$2,1($4)
	lw	$3,8($16)
	dsra	$2,$7,40
	andi	$2,$2,0x00ff
	sb	$2,2($3)
	lw	$4,8($16)
	dsra	$2,$7,32
	andi	$2,$2,0x00ff
	sb	$2,3($4)
	lw	$3,8($16)
	dsra	$2,$7,24
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$2,4($3)
	.set	macro
	.set	reorder

.L92:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L94
	li	$2,7			
	.set	macro
	.set	reorder

	lw	$3,8($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	lw	$4,8($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	sb	$2,1($4)
	lw	$3,8($16)
	dsra	$2,$7,40
	andi	$2,$2,0x00ff
	sb	$2,2($3)
	lw	$4,8($16)
	dsra	$2,$7,32
	andi	$2,$2,0x00ff
	sb	$2,3($4)
	lw	$3,8($16)
	dsra	$2,$7,24
	andi	$2,$2,0x00ff
	sb	$2,4($3)
	lw	$4,8($16)
	dsra	$2,$7,16
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$2,5($4)
	.set	macro
	.set	reorder

.L94:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L96
	addu	$2,$4,-8
	.set	macro
	.set	reorder

	lw	$3,8($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	lw	$4,8($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	sb	$2,1($4)
	lw	$3,8($16)
	dsra	$2,$7,40
	andi	$2,$2,0x00ff
	sb	$2,2($3)
	lw	$4,8($16)
	dsra	$2,$7,32
	andi	$2,$2,0x00ff
	sb	$2,3($4)
	lw	$3,8($16)
	dsra	$2,$7,24
	andi	$2,$2,0x00ff
	sb	$2,4($3)
	lw	$4,8($16)
	dsra	$2,$7,16
	andi	$2,$2,0x00ff
	sb	$2,5($4)
	lw	$3,8($16)
	dsra	$2,$7,8
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$2,6($3)
	.set	macro
	.set	reorder

.L96:
	sltu	$2,$2,9
	.set	noreorder
	.set	nomacro
	beq	$2,$0,.L27
	dsra	$2,$7,56
	.set	macro
	.set	reorder

	lw	$4,8($16)
	andi	$2,$2,0x00ff
	sb	$2,0($4)
	lw	$5,8($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	sb	$2,1($5)
	lw	$4,8($16)
	dsra	$2,$7,40
	andi	$2,$2,0x00ff
	sb	$2,2($4)
	lw	$5,8($16)
	dsra	$2,$7,32
	andi	$2,$2,0x00ff
	sb	$2,3($5)
	lw	$4,8($16)
	dsra	$2,$7,24
	andi	$2,$2,0x00ff
	sb	$2,4($4)
	lw	$5,8($16)
	dsra	$2,$7,16
	andi	$2,$2,0x00ff
	sb	$2,5($5)
	lw	$6,8($16)
	dsra	$2,$7,8
	andi	$2,$2,0x00ff
	sb	$2,6($6)
	lw	$4,8($16)
	andi	$2,$7,0x00ff
	sb	$2,7($4)
	lw	$4,16($16)
	li	$2,8			
	.set	noreorder
	.set	nomacro
	beq	$4,$2,.L27
	li	$2,9			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L101
	li	$2,10			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	dsra	$3,$3,56
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$3,8($2)
	.set	macro
	.set	reorder

.L101:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L103
	li	$2,11			
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	lw	$5,8($16)
	dsra	$3,$3,48
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$3,9($5)
	.set	macro
	.set	reorder

.L103:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L105
	li	$2,12			
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	lw	$5,8($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($5)
	lw	$4,8($16)
	dsra	$3,$3,40
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$3,10($4)
	.set	macro
	.set	reorder

.L105:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L107
	li	$2,13			
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	lw	$5,8($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($5)
	lw	$4,8($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	lw	$5,8($16)
	dsra	$3,$3,32
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$3,11($5)
	.set	macro
	.set	reorder

.L107:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L109
	li	$2,14			
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	lw	$5,8($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($5)
	lw	$4,8($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	lw	$5,8($16)
	dsra	$2,$3,32
	andi	$2,$2,0x00ff
	sb	$2,11($5)
	lw	$4,8($16)
	dsra	$3,$3,24
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$3,12($4)
	.set	macro
	.set	reorder

.L109:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L111
	li	$2,15			
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	lw	$5,8($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($5)
	lw	$4,8($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	lw	$5,8($16)
	dsra	$2,$3,32
	andi	$2,$2,0x00ff
	sb	$2,11($5)
	lw	$4,8($16)
	dsra	$2,$3,24
	andi	$2,$2,0x00ff
	sb	$2,12($4)
	lw	$5,8($16)
	dsra	$3,$3,16
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$3,13($5)
	.set	macro
	.set	reorder

.L111:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L113
	li	$2,16			
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	lw	$5,8($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($5)
	lw	$4,8($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	lw	$5,8($16)
	dsra	$2,$3,32
	andi	$2,$2,0x00ff
	sb	$2,11($5)
	lw	$4,8($16)
	dsra	$2,$3,24
	andi	$2,$2,0x00ff
	sb	$2,12($4)
	lw	$5,8($16)
	dsra	$2,$3,16
	andi	$2,$2,0x00ff
	sb	$2,13($5)
	lw	$4,8($16)
	dsra	$3,$3,8
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	b	.L27
	sb	$3,14($4)
	.set	macro
	.set	reorder

.L113:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,.L120
	move	$2,$0
	.set	macro
	.set	reorder

	lw	$4,8($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	lw	$5,8($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($5)
	lw	$4,8($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	lw	$5,8($16)
	dsra	$2,$3,32
	andi	$2,$2,0x00ff
	sb	$2,11($5)
	lw	$4,8($16)
	dsra	$2,$3,24
	andi	$2,$2,0x00ff
	sb	$2,12($4)
	lw	$5,8($16)
	dsra	$2,$3,16
	andi	$2,$2,0x00ff
	sb	$2,13($5)
	lw	$6,8($16)
	dsra	$2,$3,8
	andi	$2,$2,0x00ff
	sb	$2,14($6)
	lw	$4,8($16)
	andi	$3,$3,0x00ff
	sb	$3,15($4)
.L27:
	move	$2,$0
.L120:
	move	$sp,$fp
	ld	$31,2088($sp)
	ld	$fp,2080($sp)
	ld	$28,2072($sp)
	ld	$16,2064($sp)
	
	.set	noreorder
	.set	nomacro
	j	$31
	addu	$sp,$sp,2096
	.set	macro
	.set	reorder

.LFE1:
	.end	__builtin_avcall
