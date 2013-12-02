#include "asmmips.h"
	.file	1 "avcall-mips.c"
	.set	nobopt
	.text
	.align	2
	.globl	__builtin_avcall
	DECLARE_FUNCTION(__builtin_avcall)

	.text
	.text
	.ent	__builtin_avcall
__builtin_avcall:
	.frame	$fp,32,$31		
	.mask	0xc0010000,-8
	.fmask	0x00000000,0
	subu	$sp,$sp,32
	sw	$fp,20($sp)
	move	$fp,$sp
	sw	$31,24($sp)
	sw	$16,16($sp)
	addu	$sp,$sp,-1024
	move	$16,$4
	move	$4,$sp
	lw	$3,20($16)
	lw	$2,4($16)
	addu	$3,$3,-48
	subu	$3,$3,$16
	andi	$2,$2,0x400
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L3
	sra	$3,$3,2
	.set	macro
	.set	reorder

 
	l.d $f12,32($16)
 
	lw	$2,4($16)
	
	andi	$2,$2,0x800
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L65
	li	$6,4			
	.set	macro
	.set	reorder

 
	l.d $f14,40($16)
 
$L3:
	li	$6,4			
$L65:
	slt	$2,$6,$3
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L6
	addu	$5,$4,16
	.set	macro
	.set	reorder

	addu	$4,$16,64
	addu	$6,$3,-4
$L8:
	lw	$2,0($4)
	addu	$4,$4,4
	addu	$6,$6,-1
	sw	$2,0($5)
	.set	noreorder
	.set	nomacro
	bne	$6,$0,$L8
	addu	$5,$5,4
	.set	macro
	.set	reorder

$L6:
	lw	$25,0($16)
	lw	$4,48($16)
	lw	$5,52($16)
	lw	$6,56($16)
	lw	$7,60($16)
	jal	$31,$25
	move	$6,$2
	lw	$4,12($16)
	li	$2,1			
	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L64
	move	$2,$0
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$0,$L61
	li	$2,2			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L62
	li	$2,3			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L62
	li	$2,4			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L62
	li	$2,5			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L63
	li	$2,6			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L63
	li	$2,7			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L61
	li	$2,8			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L61
	li	$2,9			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L61
	li	$2,10			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L61
	addu	$2,$4,-11
	.set	macro
	.set	reorder

	sltu	$2,$2,2
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L32
	li	$2,13			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	
	sw	$6,0($2)
	lw	$4,8($16)
	.set	noreorder
	.set	nomacro
	j	$L11
	sw	$3,4($4)
	.set	macro
	.set	reorder

$L32:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L34
	li	$2,14			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	j	$L11
	s.s	$f0,0($2)
	.set	macro
	.set	reorder

$L34:
	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L36
	li	$2,15			
	.set	macro
	.set	reorder

	lw	$2,8($16)
	
	s.d	$f0,0($2)
	.set	noreorder
	.set	nomacro
	j	$L64
	move	$2,$0
	.set	macro
	.set	reorder

$L36:
	.set	noreorder
	.set	nomacro
	beq	$4,$2,$L61
	li	$2,16			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$4,$2,$L64
	move	$2,$0
	.set	macro
	.set	reorder

	lw	$3,4($16)
	
	andi	$2,$3,0x1
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L41
	li	$2,1			
	.set	macro
	.set	reorder

	lw	$3,16($16)
	
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L42
	li	$2,2			
	.set	macro
	.set	reorder

	lw	$3,8($16)
	lbu	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L11
	sb	$2,0($3)
	.set	macro
	.set	reorder

$L42:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L44
	li	$2,4			
	.set	macro
	.set	reorder

	lw	$3,8($16)
	lhu	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L11
	sh	$2,0($3)
	.set	macro
	.set	reorder

$L44:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L46
	li	$2,8			
	.set	macro
	.set	reorder

	lw	$3,8($16)
	lw	$2,0($6)
	.set	noreorder
	.set	nomacro
	j	$L11
	sw	$2,0($3)
	.set	macro
	.set	reorder

$L46:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L48
	addu	$2,$3,3
	.set	macro
	.set	reorder

	lw	$3,8($16)
	lw	$2,0($6)
	
	sw	$2,0($3)
	lw	$4,8($16)
	lw	$2,4($6)
	.set	noreorder
	.set	nomacro
	j	$L11
	sw	$2,4($4)
	.set	macro
	.set	reorder

$L48:
	srl	$5,$2,2
	addu	$5,$5,-1
	.set	noreorder
	.set	nomacro
	bltz	$5,$L11
	sll	$2,$5,2
	.set	macro
	.set	reorder

	addu	$6,$2,$6
$L52:
	lw	$2,0($6)
	addu	$6,$6,-4
	sll	$3,$5,2
	lw	$4,8($16)
	addu	$5,$5,-1
	addu	$3,$3,$4
	.set	noreorder
	.set	nomacro
	bgez	$5,$L52
	sw	$2,0($3)
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L64
	move	$2,$0
	.set	macro
	.set	reorder

$L41:
	andi	$2,$3,0x2
	.set	noreorder
	.set	nomacro
	beq	$2,$0,$L11
	li	$2,1			
	.set	macro
	.set	reorder

	lw	$3,16($16)
	
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L56
	li	$2,2			
	.set	macro
	.set	reorder

$L62:
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	j	$L11
	sb	$6,0($2)
	.set	macro
	.set	reorder

$L56:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L58
	li	$2,4			
	.set	macro
	.set	reorder

$L63:
	lw	$2,8($16)
	.set	noreorder
	.set	nomacro
	j	$L11
	sh	$6,0($2)
	.set	macro
	.set	reorder

$L58:
	.set	noreorder
	.set	nomacro
	bne	$3,$2,$L64
	move	$2,$0
	.set	macro
	.set	reorder

$L61:
	lw	$2,8($16)
	
	sw	$6,0($2)
$L11:
	move	$2,$0
$L64:
	move	$sp,$fp
	lw	$31,24($sp)
	lw	$fp,20($sp)
	lw	$16,16($sp)
	
	.set	noreorder
	.set	nomacro
	j	$31
	addu	$sp,$sp,32
	.set	macro
	.set	reorder

	.end	__builtin_avcall
