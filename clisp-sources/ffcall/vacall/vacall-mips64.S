#include "asmmips.h"
	.file	1 "vacall-mips64.c"
	.set	nobopt
	.text
	.align	2
	.globl	__vacall
	DECLARE_FUNCTION(__vacall)

	.extern	vacall_function, 8

	.text
	.text
	.ent	__vacall
__vacall:
	.frame	$sp,224,$31		
	.mask	0x80030000,-8
	.fmask	0x00000000,0
	ld	$25,vacall_function
	dsubu	$sp,$sp,224
	daddu	$12,$sp,288
	sd	$31,216($sp)
	sd	$17,208($sp)
	sd	$16,200($sp)
	sd	$4,-64($12)
	daddu	$4,$sp,32
	daddu	$13,$sp,224
	sd	$5,-56($12)
	sd	$6,-48($12)
	sd	$7,-40($12)
	sd	$8,-32($12)
	sd	$9,-24($12)
	sd	$10,-16($12)
	sd	$11,-8($12)
	s.d	$f12,136($sp)
	s.d	$f13,144($sp)
	s.d	$f14,152($sp)
	s.d	$f15,160($sp)
	s.d	$f16,168($sp)
	s.d	$f17,176($sp)
	s.d	$f18,184($sp)
	s.d	$f19,192($sp)
	s.s	$f12,100($sp)
	s.s	$f13,104($sp)
	s.s	$f14,108($sp)
	s.s	$f15,112($sp)
	s.s	$f16,116($sp)
	s.s	$f17,120($sp)
	s.s	$f18,124($sp)
	s.s	$f19,128($sp)
	sw	$0,32($sp)
	sd	$13,40($sp)
	sd	$0,48($sp)
	sw	$0,56($sp)
	sd	$12,88($sp)
	.set	noreorder
	.set	nomacro
	jal	$31,$25
	sw	$0,96($sp)
	.set	macro
	.set	reorder

	lw	$13,56($sp)
	
	.set	noreorder
	.set	nomacro
	beq	$13,$0,$L4
	li	$12,1			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$13,$12,$L87
	li	$12,2			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L7
	li	$12,3			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L4
	lb	$2,72($sp)
	.set	macro
	.set	reorder

$L7:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L9
	li	$12,4			
	.set	macro
	.set	reorder

$L87:
	.set	noreorder
	.set	nomacro
	j	$L4
	lbu	$2,72($sp)
	.set	macro
	.set	reorder

$L9:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L11
	li	$12,5			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L4
	lh	$2,72($sp)
	.set	macro
	.set	reorder

$L11:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L13
	li	$12,6			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L4
	lhu	$2,72($sp)
	.set	macro
	.set	reorder

$L13:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L15
	li	$12,7			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L4
	lw	$2,72($sp)
	.set	macro
	.set	reorder

$L15:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L17
	li	$12,8			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L4
	lwu	$2,72($sp)
	.set	macro
	.set	reorder

$L17:
	.set	noreorder
	.set	nomacro
	beq	$13,$12,$L88
	li	$12,9			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$13,$12,$L88
	li	$12,10			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$13,$12,$L88
	lw	$13,56($sp)
	.set	macro
	.set	reorder

	li	$12,11			
	.set	noreorder
	.set	nomacro
	beq	$13,$12,$L88
	li	$12,12			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L27
	li	$12,13			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L4
	l.s	$f0,72($sp)
	.set	macro
	.set	reorder

$L27:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L29
	li	$12,14			
	.set	macro
	.set	reorder

	l.d	$f0,72($sp)
	.set	noreorder
	.set	nomacro
	j	$L92
	ld	$31,216($sp)
	.set	macro
	.set	reorder

$L29:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L31
	li	$12,15			
	.set	macro
	.set	reorder

$L88:
	.set	noreorder
	.set	nomacro
	j	$L4
	ld	$2,72($sp)
	.set	macro
	.set	reorder

$L31:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L92
	ld	$31,216($sp)
	.set	macro
	.set	reorder

	lw	$12,32($sp)
	
	andi	$13,$12,0x1
	.set	noreorder
	.set	nomacro
	beq	$13,$0,$L34
	move	$17,$12
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	j	$L92
	ld	$2,48($sp)
	.set	macro
	.set	reorder

$L34:
	andi	$12,$17,0x400
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L4
	andi	$12,$17,0x4
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L37
	ld	$13,64($sp)
	.set	macro
	.set	reorder

	dli	$12,0x1		
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L38
	dli	$12,0x2		
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	.set	noreorder
	.set	nomacro
	j	$L4
	lbu	$2,0($12)
	.set	macro
	.set	reorder

$L38:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L40
	dli	$12,0x4		
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	.set	noreorder
	.set	nomacro
	j	$L4
	lhu	$2,0($12)
	.set	macro
	.set	reorder

$L40:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L42
	dli	$12,0x8		
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	.set	noreorder
	.set	nomacro
	j	$L4
	lwu	$2,0($12)
	.set	macro
	.set	reorder

$L42:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,$L92
	ld	$31,216($sp)
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	.set	noreorder
	.set	nomacro
	j	$L95
	ld	$2,0($12)
	.set	macro
	.set	reorder

$L37:
	dsubu	$12,$13,1
	sltu	$12,$12,16
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L46
	move	$24,$13
	.set	macro
	.set	reorder

	dli	$12,0x1		
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L47
	dli	$12,0x2		
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	lbu	$13,0($12)
	.set	noreorder
	.set	nomacro
	j	$L46
	dsll	$2,$13,56
	.set	macro
	.set	reorder

$L47:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L49
	dli	$12,0x3		
	.set	macro
	.set	reorder

	ld	$14,48($sp)
	
	lbu	$13,0($14)
	lbu	$12,1($14)
	dsll	$13,$13,56
	dsll	$12,$12,48
	.set	noreorder
	.set	nomacro
	j	$L46
	or	$2,$13,$12
	.set	macro
	.set	reorder

$L49:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L51
	dli	$12,0x4		
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	lbu	$15,0($12)
	lbu	$13,1($12)
	lbu	$14,2($12)
	dsll	$15,$15,56
	dsll	$13,$13,48
	or	$15,$15,$13
	dsll	$14,$14,40
	.set	noreorder
	.set	nomacro
	j	$L46
	or	$2,$15,$14
	.set	macro
	.set	reorder

$L51:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L53
	dli	$12,0x5		
	.set	macro
	.set	reorder

	ld	$16,48($sp)
	
	lbu	$12,0($16)
	lbu	$13,1($16)
	lbu	$14,2($16)
	lbu	$15,3($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	.set	noreorder
	.set	nomacro
	j	$L46
	or	$2,$12,$15
	.set	macro
	.set	reorder

$L53:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L55
	dli	$12,0x6		
	.set	macro
	.set	reorder

	ld	$16,48($sp)
	
	lbu	$12,0($16)
	lbu	$13,1($16)
	lbu	$15,2($16)
	lbu	$14,4($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$15,$15,40
	lbu	$13,3($16)
	or	$12,$12,$15
	dsll	$14,$14,24
	.set	noreorder
	.set	nomacro
	j	$L89
	dsll	$13,$13,32
	.set	macro
	.set	reorder

$L55:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L57
	dli	$12,0x7		
	.set	macro
	.set	reorder

	ld	$16,48($sp)
	
	lbu	$12,0($16)
	lbu	$13,1($16)
	lbu	$14,2($16)
	lbu	$15,3($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	lbu	$13,4($16)
	or	$12,$12,$15
	lbu	$14,5($16)
	dsll	$13,$13,24
	or	$12,$12,$13
	dsll	$14,$14,16
	.set	noreorder
	.set	nomacro
	j	$L46
	or	$2,$12,$14
	.set	macro
	.set	reorder

$L57:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L59
	dsubu	$12,$24,8
	.set	macro
	.set	reorder

	ld	$16,48($sp)
	
	lbu	$12,0($16)
	lbu	$13,1($16)
	lbu	$14,2($16)
	lbu	$15,4($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	lbu	$13,3($16)
	or	$12,$12,$14
	dsll	$15,$15,24
	lbu	$14,6($16)
	dsll	$13,$13,32
	or	$12,$12,$13
	lbu	$13,5($16)
	or	$12,$12,$15
	dsll	$14,$14,8
	dsll	$13,$13,16
$L89:
	or	$12,$12,$13
	.set	noreorder
	.set	nomacro
	j	$L46
	or	$2,$12,$14
	.set	macro
	.set	reorder

$L59:
	sltu	$12,$12,9
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L46
	ld	$16,48($sp)
	.set	macro
	.set	reorder

	
	lbu	$12,0($16)
	lbu	$13,1($16)
	lbu	$14,2($16)
	lbu	$15,3($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	lbu	$13,4($16)
	or	$12,$12,$15
	lbu	$14,5($16)
	lbu	$15,7($16)
	dsll	$13,$13,24
	or	$12,$12,$13
	lbu	$13,6($16)
	dsll	$14,$14,16
	or	$12,$12,$14
	dsll	$13,$13,8
	or	$12,$12,$13
	dli	$13,0x8		
	.set	noreorder
	.set	nomacro
	beq	$24,$13,$L46
	or	$2,$12,$15
	.set	macro
	.set	reorder

	dli	$12,0x9		
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L64
	dli	$12,0xa		
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	.set	noreorder
	.set	nomacro
	j	$L46
	dsll	$3,$12,56
	.set	macro
	.set	reorder

$L64:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L66
	dli	$12,0xb		
	.set	macro
	.set	reorder

	lbu	$13,8($16)
	lbu	$12,9($16)
	dsll	$13,$13,56
	dsll	$12,$12,48
	.set	noreorder
	.set	nomacro
	j	$L46
	or	$3,$13,$12
	.set	macro
	.set	reorder

$L66:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L68
	dli	$12,0xc		
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$14,10($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	.set	noreorder
	.set	nomacro
	j	$L46
	or	$3,$12,$14
	.set	macro
	.set	reorder

$L68:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L70
	dli	$12,0xd		
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$14,10($16)
	lbu	$15,11($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	.set	noreorder
	.set	nomacro
	j	$L90
	dsll	$15,$15,32
	.set	macro
	.set	reorder

$L70:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L72
	dli	$12,0xe		
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$15,10($16)
	lbu	$14,12($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$15,$15,40
	lbu	$13,11($16)
	or	$12,$12,$15
	dsll	$14,$14,24
	.set	noreorder
	.set	nomacro
	j	$L91
	dsll	$13,$13,32
	.set	macro
	.set	reorder

$L72:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L74
	dli	$12,0xf		
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$14,10($16)
	lbu	$15,11($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	lbu	$13,12($16)
	or	$12,$12,$15
	lbu	$14,13($16)
	dsll	$13,$13,24
	or	$12,$12,$13
	dsll	$14,$14,16
	.set	noreorder
	.set	nomacro
	j	$L46
	or	$3,$12,$14
	.set	macro
	.set	reorder

$L74:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L76
	dli	$12,0x10		
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$14,10($16)
	lbu	$15,12($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	lbu	$13,11($16)
	or	$12,$12,$14
	dsll	$15,$15,24
	lbu	$14,14($16)
	dsll	$13,$13,32
	or	$12,$12,$13
	lbu	$13,13($16)
	or	$12,$12,$15
	dsll	$14,$14,8
	dsll	$13,$13,16
$L91:
	or	$12,$12,$13
	.set	noreorder
	.set	nomacro
	j	$L46
	or	$3,$12,$14
	.set	macro
	.set	reorder

$L76:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L93
	andi	$12,$17,0x2000
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	lbu	$13,9($16)
	lbu	$14,10($16)
	lbu	$15,11($16)
	dsll	$12,$12,56
	dsll	$13,$13,48
	or	$12,$12,$13
	dsll	$14,$14,40
	or	$12,$12,$14
	dsll	$15,$15,32
	lbu	$13,12($16)
	or	$12,$12,$15
	lbu	$14,13($16)
	lbu	$15,15($16)
	dsll	$13,$13,24
	or	$12,$12,$13
	lbu	$13,14($16)
	dsll	$14,$14,16
	or	$12,$12,$14
	dsll	$13,$13,8
	or	$12,$12,$13
$L90:
	or	$3,$12,$15
$L46:
	andi	$12,$17,0x2000
$L93:
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L79
	dli	$12,0x4		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L80
	dli	$12,0x8		
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	.set	noreorder
	.set	nomacro
	j	$L79
	l.s	$f0,0($12)
	.set	macro
	.set	reorder

$L80:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L94
	andi	$12,$17,0x4000
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	l.s	$f0,0($12)
	l.s	$f2,4($12)
$L79:
	andi	$12,$17,0x4000
$L94:
	.set	noreorder
	.set	nomacro
	beq	$12,$0,$L4
	dli	$12,0x8		
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L84
	dli	$12,0x10		
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	l.d	$f0,0($12)
	.set	noreorder
	.set	nomacro
	j	$L92
	ld	$31,216($sp)
	.set	macro
	.set	reorder

$L84:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,$L92
	ld	$31,216($sp)
	.set	macro
	.set	reorder

	ld	$12,48($sp)
	
	l.d	$f0,0($12)
	l.d	$f2,8($12)
$L4:
	ld	$31,216($sp)
$L92:
$L95:
	ld	$17,208($sp)
	ld	$16,200($sp)
	
	.set	noreorder
	.set	nomacro
	j	$31
	daddu	$sp,$sp,224
	.set	macro
	.set	reorder

	.end	__vacall
