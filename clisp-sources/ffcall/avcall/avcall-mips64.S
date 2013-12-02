#include "asmmips.h"
	.file	1 "avcall-mips64.c"
	.set	nobopt
	.text
	.align	2
	.globl	__builtin_avcall
	DECLARE_FUNCTION(__builtin_avcall)

	.text
	.text
	.ent	__builtin_avcall
__builtin_avcall:
	.frame	$fp,2104,$31		
	.mask	0xc0010000,-8
	.fmask	0x00000000,0
	dsubu	$sp,$sp,2104
	sd	$16,2080($sp)
	move	$16,$4
	sd	$31,2096($sp)
	sd	$fp,2088($sp)
	ld	$2,40($16)
	dli	$3,0x8		
	dsubu	$2,$2,96
	dsubu	$2,$2,$16
	ddiv	$0,$2,$3
	move	$fp,$sp
	move	$8,$sp
	mflo	$2
	
	.set	noreorder
	beql	$3,$0,1f
	break	7
1:
	.set	reorder
	lw	$3,52($16)
	dsll	$4,$2,32
	dsra	$4,$4,32
	.set	noreorder
	.set	nomacro
	beq	$3,$0,$L3
	andi	$2,$3,0x1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L121
	andi	$2,$3,0x2
	.set	macro
	.set	reorder

 
	lwc1 $f12,60($16)
 
	andi	$2,$3,0x2
$L121:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L122
	andi	$2,$3,0x4
	.set	macro
	.set	reorder

 
	lwc1 $f13,64($16)
 
	andi	$2,$3,0x4
$L122:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L123
	andi	$2,$3,0x8
	.set	macro
	.set	reorder

 
	lwc1 $f14,68($16)
 
	andi	$2,$3,0x8
$L123:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L124
	andi	$2,$3,0x10
	.set	macro
	.set	reorder

 
	lwc1 $f15,72($16)
 
	andi	$2,$3,0x10
$L124:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L125
	andi	$2,$3,0x20
	.set	macro
	.set	reorder

 
	lwc1 $f16,76($16)
 
	andi	$2,$3,0x20
$L125:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L126
	andi	$2,$3,0x40
	.set	macro
	.set	reorder

 
	lwc1 $f17,80($16)
 
	andi	$2,$3,0x40
$L126:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L127
	andi	$2,$3,0x80
	.set	macro
	.set	reorder

 
	lwc1 $f18,84($16)
 
	andi	$2,$3,0x80
$L127:
	.set	noreorder
	.set	nomacro
	beql	$2,$0,$L128
	lw	$3,56($16)
	.set	macro
	.set	reorder

 
	lwc1 $f19,88($16)
 
$L3:
	
	lw	$3,56($16)
$L128:
	.set	noreorder
	.set	nomacro
	beq	$3,$0,$L12
	andi	$2,$3,0x1
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L129
	andi	$2,$3,0x2
	.set	macro
	.set	reorder

 
	ldc1 $f12,96($16)
 
	andi	$2,$3,0x2
$L129:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L130
	andi	$2,$3,0x4
	.set	macro
	.set	reorder

 
	ldc1 $f13,104($16)
 
	andi	$2,$3,0x4
$L130:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L131
	andi	$2,$3,0x8
	.set	macro
	.set	reorder

 
	ldc1 $f14,112($16)
 
	andi	$2,$3,0x8
$L131:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L132
	andi	$2,$3,0x10
	.set	macro
	.set	reorder

 
	ldc1 $f15,120($16)
 
	andi	$2,$3,0x10
$L132:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L133
	andi	$2,$3,0x20
	.set	macro
	.set	reorder

 
	ldc1 $f16,128($16)
 
	andi	$2,$3,0x20
$L133:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L134
	andi	$2,$3,0x40
	.set	macro
	.set	reorder

 
	ldc1 $f17,136($16)
 
	andi	$2,$3,0x40
$L134:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L135
	andi	$2,$3,0x80
	.set	macro
	.set	reorder

 
	ldc1 $f18,144($16)
 
	andi	$2,$3,0x80
$L135:
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L136
	dli	$7,0x8		
	.set	macro
	.set	reorder

 
	ldc1 $f19,152($16)
 
$L12:
	dli	$7,0x8		
$L136:
	move	$5,$4
	slt	$2,$7,$5
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L22
	daddu	$6,$16,96
	.set	macro
	.set	reorder

$L24:
	dsll	$3,$7,3
	daddu	$2,$6,$3
	ld	$4,0($2)
	daddu	$7,$7,1
	daddu	$3,$3,$8
	slt	$2,$7,$5
	.set	noreorder
	.set	nomacro
	bne	$2,$0,$L24
	sd	$4,-64($3)
	.set	macro
	.set	reorder

$L22:
 
	ld $4,96($16)
	ld $5,104($16)
	ld $6,112($16)
	ld $7,120($16)
	ld $8,128($16)
	ld $9,136($16)
	ld $10,144($16)
	ld $11,152($16)
 
	ld	$25,0($16)
	
	jal	$31,$25
	lw	$4,24($16)
	move	$7,$2
	li	$2,1			
	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L120
	move	$2,$0
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$0,$L116
	li	$2,2			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L117
	li	$2,3			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L117
	li	$2,4			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L117
	li	$2,5			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L118
	li	$2,6			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L118
	li	$2,7			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L119
	li	$2,8			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L119
	li	$2,9			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L116
	li	$2,10			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L116
	li	$2,11			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L116
	li	$2,12			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L116
	li	$2,13			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L52
	li	$2,14			
	.set	macro
	.set	reorder

	ld	$2,16($16)
	.set	noreorder
	.set	nomacro
	j	$L27
	s.s	$f0,0($2)
	.set	macro
	.set	reorder

$L52:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L54
	li	$2,15			
	.set	macro
	.set	reorder

	ld	$2,16($16)
	
	s.d	$f0,0($2)
	.set	noreorder
	.set	nomacro
	j	$L120
	move	$2,$0
	.set	macro
	.set	reorder

$L54:
	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L116
	li	$2,16			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L120
	move	$2,$0
	.set	macro
	.set	reorder

	lw	$4,8($16)
	
	andi	$2,$4,0x1
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L59
	dli	$2,0x1		
	.set	macro
	.set	reorder

	ld	$3,32($16)
	
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L60
	dli	$2,0x2		
	.set	macro
	.set	reorder

	ld	$3,16($16)
	lbu	$2,0($7)
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$2,0($3)
	.set	macro
	.set	reorder

$L60:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L62
	dli	$2,0x4		
	.set	macro
	.set	reorder

	ld	$3,16($16)
	lhu	$2,0($7)
	.set	noreorder
	.set	nomacro
	j	$L27
	sh	$2,0($3)
	.set	macro
	.set	reorder

$L62:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L64
	dli	$2,0x8		
	.set	macro
	.set	reorder

	ld	$3,16($16)
	lw	$2,0($7)
	.set	noreorder
	.set	nomacro
	j	$L27
	sw	$2,0($3)
	.set	macro
	.set	reorder

$L64:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L66
	daddu	$2,$3,7
	.set	macro
	.set	reorder

	ld	$3,16($16)
	ld	$2,0($7)
	.set	noreorder
	.set	nomacro
	j	$L27
	sd	$2,0($3)
	.set	macro
	.set	reorder

$L66:
	dsll	$6,$2,29
	dsra	$6,$6,32
	addu	$6,$6,-1
	.set	noreorder
	.set	nomacro
	bltz	$6,$L120
	move	$2,$0
	.set	macro
	.set	reorder

$L70:
	dsll	$5,$6,3
	ld	$3,16($16)
	daddu	$2,$7,$5
	ld	$4,0($2)
	addu	$6,$6,-1
	daddu	$3,$3,$5
	.set	noreorder
	.set	nomacro
	bgez	$6,$L70
	sd	$4,0($3)
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L120
	move	$2,$0
	.set	macro
	.set	reorder

$L59:
	andi	$2,$4,0x200
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L27
	andi	$2,$4,0x4
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L74
	dli	$2,0x1		
	.set	macro
	.set	reorder

	ld	$3,32($16)
	
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L75
	dli	$2,0x2		
	.set	macro
	.set	reorder

$L117:
	ld	$3,16($16)
	andi	$2,$7,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$2,0($3)
	.set	macro
	.set	reorder

$L75:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L77
	dli	$2,0x4		
	.set	macro
	.set	reorder

$L118:
	ld	$3,16($16)
	andi	$2,$7,0xffff
	.set	noreorder
	.set	nomacro
	j	$L27
	sh	$2,0($3)
	.set	macro
	.set	reorder

$L77:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L79
	dli	$2,0x8		
	.set	macro
	.set	reorder

$L119:
	ld	$3,16($16)
	dsll	$2,$7,32
	dsra	$2,$2,32
	.set	noreorder
	.set	nomacro
	j	$L27
	sw	$2,0($3)
	.set	macro
	.set	reorder

$L79:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L120
	move	$2,$0
	.set	macro
	.set	reorder

$L116:
	ld	$2,16($16)
	.set	noreorder
	.set	nomacro
	j	$L27
	sd	$7,0($2)
	.set	macro
	.set	reorder

$L74:
	ld	$4,32($16)
	
	dsubu	$2,$4,1
	sltu	$2,$2,16
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L27
	dli	$2,0x1		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L84
	dli	$2,0x2		
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$2,0($3)
	.set	macro
	.set	reorder

$L84:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L86
	dli	$2,0x3		
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	ld	$4,16($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$2,1($4)
	.set	macro
	.set	reorder

$L86:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L88
	dli	$2,0x4		
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	ld	$4,16($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	sb	$2,1($4)
	ld	$3,16($16)
	dsra	$2,$7,40
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$2,2($3)
	.set	macro
	.set	reorder

$L88:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L90
	dli	$2,0x5		
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	ld	$4,16($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	sb	$2,1($4)
	ld	$3,16($16)
	dsra	$2,$7,40
	andi	$2,$2,0x00ff
	sb	$2,2($3)
	ld	$4,16($16)
	dsra	$2,$7,32
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$2,3($4)
	.set	macro
	.set	reorder

$L90:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L92
	dli	$2,0x6		
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	ld	$4,16($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	sb	$2,1($4)
	ld	$3,16($16)
	dsra	$2,$7,40
	andi	$2,$2,0x00ff
	sb	$2,2($3)
	ld	$4,16($16)
	dsra	$2,$7,32
	andi	$2,$2,0x00ff
	sb	$2,3($4)
	ld	$3,16($16)
	dsra	$2,$7,24
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$2,4($3)
	.set	macro
	.set	reorder

$L92:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L94
	dli	$2,0x7		
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	ld	$4,16($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	sb	$2,1($4)
	ld	$3,16($16)
	dsra	$2,$7,40
	andi	$2,$2,0x00ff
	sb	$2,2($3)
	ld	$4,16($16)
	dsra	$2,$7,32
	andi	$2,$2,0x00ff
	sb	$2,3($4)
	ld	$3,16($16)
	dsra	$2,$7,24
	andi	$2,$2,0x00ff
	sb	$2,4($3)
	ld	$4,16($16)
	dsra	$2,$7,16
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$2,5($4)
	.set	macro
	.set	reorder

$L94:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L96
	dsubu	$2,$4,8
	.set	macro
	.set	reorder

	ld	$3,16($16)
	dsra	$2,$7,56
	andi	$2,$2,0x00ff
	sb	$2,0($3)
	ld	$4,16($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	sb	$2,1($4)
	ld	$3,16($16)
	dsra	$2,$7,40
	andi	$2,$2,0x00ff
	sb	$2,2($3)
	ld	$4,16($16)
	dsra	$2,$7,32
	andi	$2,$2,0x00ff
	sb	$2,3($4)
	ld	$3,16($16)
	dsra	$2,$7,24
	andi	$2,$2,0x00ff
	sb	$2,4($3)
	ld	$4,16($16)
	dsra	$2,$7,16
	andi	$2,$2,0x00ff
	sb	$2,5($4)
	ld	$3,16($16)
	dsra	$2,$7,8
	andi	$2,$2,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$2,6($3)
	.set	macro
	.set	reorder

$L96:
	sltu	$2,$2,9
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L27
	dsra	$2,$7,56
	.set	macro
	.set	reorder

	ld	$4,16($16)
	andi	$2,$2,0x00ff
	sb	$2,0($4)
	ld	$5,16($16)
	dsra	$2,$7,48
	andi	$2,$2,0x00ff
	sb	$2,1($5)
	ld	$4,16($16)
	dsra	$2,$7,40
	andi	$2,$2,0x00ff
	sb	$2,2($4)
	ld	$5,16($16)
	dsra	$2,$7,32
	andi	$2,$2,0x00ff
	sb	$2,3($5)
	ld	$4,16($16)
	dsra	$2,$7,24
	andi	$2,$2,0x00ff
	sb	$2,4($4)
	ld	$5,16($16)
	dsra	$2,$7,16
	andi	$2,$2,0x00ff
	sb	$2,5($5)
	ld	$6,16($16)
	dsra	$2,$7,8
	andi	$2,$2,0x00ff
	sb	$2,6($6)
	ld	$4,16($16)
	andi	$2,$7,0x00ff
	sb	$2,7($4)
	ld	$4,32($16)
	dli	$2,0x8		
	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L27
	dli	$2,0x9		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L101
	dli	$2,0xa		
	.set	macro
	.set	reorder

	ld	$2,16($16)
	dsra	$3,$3,56
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$3,8($2)
	.set	macro
	.set	reorder

$L101:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L103
	dli	$2,0xb		
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$5,16($16)
	dsra	$3,$3,48
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$3,9($5)
	.set	macro
	.set	reorder

$L103:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L105
	dli	$2,0xc		
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$5,16($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($5)
	ld	$4,16($16)
	dsra	$3,$3,40
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$3,10($4)
	.set	macro
	.set	reorder

$L105:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L107
	dli	$2,0xd		
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$5,16($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($5)
	ld	$4,16($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	ld	$5,16($16)
	dsra	$3,$3,32
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$3,11($5)
	.set	macro
	.set	reorder

$L107:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L109
	dli	$2,0xe		
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$5,16($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($5)
	ld	$4,16($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	ld	$5,16($16)
	dsra	$2,$3,32
	andi	$2,$2,0x00ff
	sb	$2,11($5)
	ld	$4,16($16)
	dsra	$3,$3,24
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$3,12($4)
	.set	macro
	.set	reorder

$L109:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L111
	dli	$2,0xf		
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$5,16($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($5)
	ld	$4,16($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	ld	$5,16($16)
	dsra	$2,$3,32
	andi	$2,$2,0x00ff
	sb	$2,11($5)
	ld	$4,16($16)
	dsra	$2,$3,24
	andi	$2,$2,0x00ff
	sb	$2,12($4)
	ld	$5,16($16)
	dsra	$3,$3,16
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$3,13($5)
	.set	macro
	.set	reorder

$L111:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L113
	dli	$2,0x10		
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$5,16($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($5)
	ld	$4,16($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	ld	$5,16($16)
	dsra	$2,$3,32
	andi	$2,$2,0x00ff
	sb	$2,11($5)
	ld	$4,16($16)
	dsra	$2,$3,24
	andi	$2,$2,0x00ff
	sb	$2,12($4)
	ld	$5,16($16)
	dsra	$2,$3,16
	andi	$2,$2,0x00ff
	sb	$2,13($5)
	ld	$4,16($16)
	dsra	$3,$3,8
	andi	$3,$3,0x00ff
	.set	noreorder
	.set	nomacro
	j	$L27
	sb	$3,14($4)
	.set	macro
	.set	reorder

$L113:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L120
	move	$2,$0
	.set	macro
	.set	reorder

	ld	$4,16($16)
	dsra	$2,$3,56
	andi	$2,$2,0x00ff
	sb	$2,8($4)
	ld	$5,16($16)
	dsra	$2,$3,48
	andi	$2,$2,0x00ff
	sb	$2,9($5)
	ld	$4,16($16)
	dsra	$2,$3,40
	andi	$2,$2,0x00ff
	sb	$2,10($4)
	ld	$5,16($16)
	dsra	$2,$3,32
	andi	$2,$2,0x00ff
	sb	$2,11($5)
	ld	$4,16($16)
	dsra	$2,$3,24
	andi	$2,$2,0x00ff
	sb	$2,12($4)
	ld	$5,16($16)
	dsra	$2,$3,16
	andi	$2,$2,0x00ff
	sb	$2,13($5)
	ld	$6,16($16)
	dsra	$2,$3,8
	andi	$2,$2,0x00ff
	sb	$2,14($6)
	ld	$4,16($16)
	andi	$3,$3,0x00ff
	sb	$3,15($4)
$L27:
	move	$2,$0
$L120:
	move	$sp,$fp
	ld	$31,2096($sp)
	ld	$fp,2088($sp)
	ld	$16,2080($sp)
	
	.set	noreorder
	.set	nomacro
	j	$31
	daddu	$sp,$sp,2104
	.set	macro
	.set	reorder

	.end	__builtin_avcall
