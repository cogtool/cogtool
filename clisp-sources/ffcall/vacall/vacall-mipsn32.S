#include "asmmips.h"
	
	.set	nobopt
	.option pic2
	.section	.text
	.text
	.align	2
	.globl	__vacall
	DECLARE_FUNCTION(__vacall)
	.ent	__vacall
__vacall:
.LFB1:
	.frame	$sp,256,$31		
	.mask	0x90030000,-8
	.fmask	0x00000000,0
	subu	$sp,$sp,256
.LCFI0:
	sd	$31,184($sp)
.LCFI1:
	sd	$28,176($sp)
.LCFI2:
	sd	$17,168($sp)
.LCFI3:
	sd	$16,160($sp)
.LCFI4:
	.set	noat
	lui	$1,%hi(%neg(%gp_rel(__vacall)))
	addiu	$1,$1,%lo(%neg(%gp_rel(__vacall)))
	daddu	$gp,$1,$25
	.set	at
	lw	$25,vacall_function
	sd	$4,192($sp)
	addu	$4,$sp,16
	addu	$12,$sp,192
	sw	$12,20($sp)
	addu	$12,$sp,256
	sd	$5,200($sp)
	sd	$6,208($sp)
	sd	$7,216($sp)
	sd	$8,224($sp)
	sd	$9,232($sp)
	sd	$10,240($sp)
	sd	$11,248($sp)
	s.d	$f12,96($sp)
	s.d	$f13,104($sp)
	s.d	$f14,112($sp)
	s.d	$f15,120($sp)
	s.d	$f16,128($sp)
	s.d	$f17,136($sp)
	s.d	$f18,144($sp)
	s.d	$f19,152($sp)
	s.s	$f12,64($sp)
	s.s	$f13,68($sp)
	s.s	$f14,72($sp)
	s.s	$f15,76($sp)
	s.s	$f16,80($sp)
	s.s	$f17,84($sp)
	s.s	$f18,88($sp)
	s.s	$f19,92($sp)
	sw	$0,16($sp)
	sw	$0,24($sp)
	sw	$0,28($sp)
	sw	$12,56($sp)
	sw	$0,60($sp)
	jal	$31,$25
	lw	$13,28($sp)
	
	.set	noreorder
	.set	nomacro
	beq	$13,$0,.L4
	li	$12,1			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$13,$12,.L87
	li	$12,2			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L7
	li	$12,3			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L4
	lb	$2,40($sp)
	.set	macro
	.set	reorder

.L7:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L9
	li	$12,4			
	.set	macro
	.set	reorder

.L87:
	.set	noreorder
	.set	nomacro
	b	.L4
	lbu	$2,40($sp)
	.set	macro
	.set	reorder

.L9:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L11
	li	$12,5			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L4
	lh	$2,40($sp)
	.set	macro
	.set	reorder

.L11:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L13
	li	$12,6			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L4
	lhu	$2,40($sp)
	.set	macro
	.set	reorder

.L13:
	.set	noreorder
	.set	nomacro
	beq	$13,$12,.L88
	li	$12,7			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$13,$12,.L89
	li	$12,8			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$13,$12,.L88
	li	$12,9			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L21
	li	$12,10			
	.set	macro
	.set	reorder

.L89:
	.set	noreorder
	.set	nomacro
	b	.L4
	lwu	$2,40($sp)
	.set	macro
	.set	reorder

.L21:
	.set	noreorder
	.set	nomacro
	beq	$13,$12,.L90
	lw	$13,28($sp)
	.set	macro
	.set	reorder

	li	$12,11			
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L25
	li	$12,12			
	.set	macro
	.set	reorder

.L90:
	.set	noreorder
	.set	nomacro
	b	.L4
	ld	$2,40($sp)
	.set	macro
	.set	reorder

.L25:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L27
	li	$12,13			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L4
	l.s	$f0,40($sp)
	.set	macro
	.set	reorder

.L27:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L29
	li	$12,14			
	.set	macro
	.set	reorder

	l.d	$f0,40($sp)
	b	.L4
.L29:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L31
	li	$12,15			
	.set	macro
	.set	reorder

.L88:
	.set	noreorder
	.set	nomacro
	b	.L4
	lw	$2,40($sp)
	.set	macro
	.set	reorder

.L31:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L4
	lw	$12,16($sp)
	.set	macro
	.set	reorder

	
	andi	$13,$12,0x1
	.set	noreorder
	.set	nomacro
	beq	$13,$0,.L34
	move	$17,$12
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L4
	lw	$2,24($sp)
	.set	macro
	.set	reorder

.L34:
	andi	$12,$17,0x400
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L4
	andi	$12,$17,0x4
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L37
	lw	$13,32($sp)
	.set	macro
	.set	reorder

	li	$12,1			
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L38
	li	$12,2			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	.set	noreorder
	.set	nomacro
	b	.L4
	lbu	$2,0($12)
	.set	macro
	.set	reorder

.L38:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L40
	li	$12,4			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	.set	noreorder
	.set	nomacro
	b	.L4
	lhu	$2,0($12)
	.set	macro
	.set	reorder

.L40:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L42
	li	$12,8			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	.set	noreorder
	.set	nomacro
	b	.L4
	lwu	$2,0($12)
	.set	macro
	.set	reorder

.L42:
	.set	noreorder
	.set	nomacro
	bne	$13,$12,.L4
	lw	$12,24($sp)
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	b	.L4
	ld	$2,0($12)
	.set	macro
	.set	reorder

.L37:
	addu	$12,$13,-1
	sltu	$12,$12,16
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L46
	move	$24,$13
	.set	macro
	.set	reorder

	li	$12,1			
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L47
	li	$12,2			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	
	lbu	$13,0($12)
	.set	noreorder
	.set	nomacro
	b	.L46
	dsll	$2,$13,56
	.set	macro
	.set	reorder

.L47:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L49
	li	$12,3			
	.set	macro
	.set	reorder

	lw	$14,24($sp)
	
	lbu	$13,0($14)
	lbu	$12,1($14)
	dsll	$13,$13,56
	dsll	$12,$12,48
	.set	noreorder
	.set	nomacro
	b	.L46
	or	$2,$13,$12
	.set	macro
	.set	reorder

.L49:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L51
	li	$12,4			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	
	lbu	$15,0($12)
	lbu	$13,1($12)
	lbu	$14,2($12)
	dsll	$15,$15,56
	dsll	$13,$13,48
	or	$15,$15,$13
	dsll	$14,$14,40
	.set	noreorder
	.set	nomacro
	b	.L46
	or	$2,$15,$14
	.set	macro
	.set	reorder

.L51:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L53
	li	$12,5			
	.set	macro
	.set	reorder

	lw	$16,24($sp)
	
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
	b	.L46
	or	$2,$12,$15
	.set	macro
	.set	reorder

.L53:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L55
	li	$12,6			
	.set	macro
	.set	reorder

	lw	$16,24($sp)
	
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
	b	.L91
	dsll	$13,$13,32
	.set	macro
	.set	reorder

.L55:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L57
	li	$12,7			
	.set	macro
	.set	reorder

	lw	$16,24($sp)
	
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
	b	.L46
	or	$2,$12,$14
	.set	macro
	.set	reorder

.L57:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L59
	addu	$12,$24,-8
	.set	macro
	.set	reorder

	lw	$16,24($sp)
	
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
.L91:
	or	$12,$12,$13
	.set	noreorder
	.set	nomacro
	b	.L46
	or	$2,$12,$14
	.set	macro
	.set	reorder

.L59:
	sltu	$12,$12,9
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L46
	lw	$16,24($sp)
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
	li	$13,8			
	.set	noreorder
	.set	nomacro
	beq	$24,$13,.L46
	or	$2,$12,$15
	.set	macro
	.set	reorder

	li	$12,9			
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L64
	li	$12,10			
	.set	macro
	.set	reorder

	lbu	$12,8($16)
	.set	noreorder
	.set	nomacro
	b	.L46
	dsll	$3,$12,56
	.set	macro
	.set	reorder

.L64:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L66
	li	$12,11			
	.set	macro
	.set	reorder

	lbu	$13,8($16)
	lbu	$12,9($16)
	dsll	$13,$13,56
	dsll	$12,$12,48
	.set	noreorder
	.set	nomacro
	b	.L46
	or	$3,$13,$12
	.set	macro
	.set	reorder

.L66:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L68
	li	$12,12			
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
	b	.L46
	or	$3,$12,$14
	.set	macro
	.set	reorder

.L68:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L70
	li	$12,13			
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
	b	.L92
	dsll	$15,$15,32
	.set	macro
	.set	reorder

.L70:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L72
	li	$12,14			
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
	b	.L93
	dsll	$13,$13,32
	.set	macro
	.set	reorder

.L72:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L74
	li	$12,15			
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
	b	.L46
	or	$3,$12,$14
	.set	macro
	.set	reorder

.L74:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L76
	li	$12,16			
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
.L93:
	or	$12,$12,$13
	.set	noreorder
	.set	nomacro
	b	.L46
	or	$3,$12,$14
	.set	macro
	.set	reorder

.L76:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L94
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
.L92:
	or	$3,$12,$15
.L46:
	andi	$12,$17,0x2000
.L94:
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L79
	li	$12,4			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L80
	li	$12,8			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	.set	noreorder
	.set	nomacro
	b	.L79
	l.s	$f0,0($12)
	.set	macro
	.set	reorder

.L80:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L95
	andi	$12,$17,0x4000
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	
	l.s	$f0,0($12)
	l.s	$f2,4($12)
.L79:
	andi	$12,$17,0x4000
.L95:
	.set	noreorder
	.set	nomacro
	beq	$12,$0,.L4
	li	$12,8			
	.set	macro
	.set	reorder

	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L84
	li	$12,16			
	.set	macro
	.set	reorder

	lw	$12,24($sp)
	
	l.d	$f0,0($12)
	b	.L4
.L84:
	.set	noreorder
	.set	nomacro
	bne	$24,$12,.L4
	lw	$12,24($sp)
	.set	macro
	.set	reorder

	
	l.d	$f0,0($12)
	l.d	$f2,8($12)
.L4:
	ld	$31,184($sp)
	ld	$28,176($sp)
	ld	$17,168($sp)
	ld	$16,160($sp)
	
	.set	noreorder
	.set	nomacro
	j	$31
	addu	$sp,$sp,256
	.set	macro
	.set	reorder

.LFE1:
	.end	__vacall
