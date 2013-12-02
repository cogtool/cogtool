#include "asmmips.h"
	.file	1 "vacall-mips.c"
	.set	nobopt
	.text
	.align	2
	.globl	__vacall
	DECLARE_FUNCTION(__vacall)

	.extern	vacall_function, 4

	.text
	.text
	.ent	__vacall
__vacall:
	.frame	$fp,88,$31		
	.mask	0xc0000000,-4
	.fmask	0x00000000,0
	subu	$sp,$sp,88
	sw	$fp,80($sp)
	move	$fp,$sp
	sw	$31,84($sp)
	lw	$25,vacall_function
	addu	$8,$fp,104
	sw	$4,-16($8)
	addu	$4,$fp,88
	sw	$5,-12($8)
	sw	$6,-8($8)
	sw	$7,-4($8)
	sw	$4,20($fp)
	addu	$4,$fp,16
	s.d	$f12,64($fp)
	s.d	$f14,72($fp)
	s.s	$f12,56($fp)
	s.s	$f14,60($fp)
	sw	$0,16($fp)
	sw	$0,24($fp)
	sw	$0,28($fp)
	sw	$8,48($fp)
	.set	noreorder
	.set	nomacro
	jal	$31,$25
	sw	$0,52($fp)
	.set	macro
	.set	reorder

	lw	$5,28($fp)
	
	.set	noreorder
	.set	nomacro
	beq	$5,$0,$L4
	li	$4,1			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L40
	li	$4,2			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L7
	li	$4,3			
	.set	macro
	.set	reorder

	lb	$2,40($fp)
	j	$L4
$L7:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L9
	li	$4,4			
	.set	macro
	.set	reorder

$L40:
	lbu	$2,40($fp)
	j	$L4
$L9:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L11
	li	$4,5			
	.set	macro
	.set	reorder

	lh	$2,40($fp)
	j	$L4
$L11:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L13
	li	$4,6			
	.set	macro
	.set	reorder

	lhu	$2,40($fp)
	j	$L4
$L13:
	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L41
	li	$4,7			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L41
	li	$4,8			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L41
	li	$4,9			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$5,$4,$L41
	addu	$4,$5,-10
	.set	macro
	.set	reorder

	sltu	$4,$4,2
	.set	noreorder
	.set	nomacro
	beq	$4,$0,$L23
	li	$4,12			
	.set	macro
	.set	reorder

	lw	$2,40($fp)
	lw	$3,44($fp)
	j	$L4
$L23:
	lw	$5,28($fp)
	
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L25
	li	$4,13			
	.set	macro
	.set	reorder

	l.s	$f0,40($fp)
	j	$L4
$L25:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L27
	li	$4,14			
	.set	macro
	.set	reorder

	l.d	$f0,40($fp)
	j	$L4
$L27:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L29
	li	$4,15			
	.set	macro
	.set	reorder

$L41:
	lw	$2,40($fp)
	j	$L4
$L29:
	bne	$5,$4,$L4
	lw	$5,16($fp)
	
	andi	$4,$5,0x1
	.set	noreorder
	.set	nomacro
	beq	$4,$0,$L32
	andi	$4,$5,0x2
	.set	macro
	.set	reorder

	lw	$2,24($fp)
	j	$L4
$L32:
	.set	noreorder
	.set	nomacro
	beq	$4,$0,$L4
	li	$4,1			
	.set	macro
	.set	reorder

	lw	$5,32($fp)
	
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L35
	li	$4,2			
	.set	macro
	.set	reorder

	lw	$4,24($fp)
	
	lbu	$2,0($4)
	j	$L4
$L35:
	.set	noreorder
	.set	nomacro
	bne	$5,$4,$L37
	li	$4,4			
	.set	macro
	.set	reorder

	lw	$4,24($fp)
	
	lhu	$2,0($4)
	j	$L4
$L37:
	bne	$5,$4,$L4
	lw	$4,24($fp)
	
	lw	$2,0($4)
$L4:
	move	$sp,$fp
	lw	$31,84($sp)
	lw	$fp,80($sp)
	
	.set	noreorder
	.set	nomacro
	j	$31
	addu	$sp,$sp,88
	.set	macro
	.set	reorder

	.end	__vacall
