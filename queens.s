.data
L39: .asciiz " O"
L40: .asciiz " ."
L50: .asciiz "\n"
L56: .asciiz "\n"
.text
printboard:
	sw $fp, 0($sp)
	move $fp, $sp
	addi $sp, $sp, -4
L103:
	sw $a0, 0($fp)
	li $a2, 0
L51:
	li $t2, 1
	lw $t4, 0($fp)
	lw $t4, 4($t4)
	addi $t4, $t4, -1
	blt $a2, $t4, L54
L55:
	li $t2, 0
L54:
	li $t4, 0
	beq $t2, $t4, L36
L52:
	li $a3, 0
L45:
	li $t2, 1
	lw $t4, 0($fp)
	lw $t4, 4($t4)
	addi $t4, $t4, -1
	blt $a3, $t4, L48
L49:
	li $t2, 0
L48:
	li $t4, 0
	beq $t2, $t4, L38
L46:
	lw $t4, 0($fp)
	lw $t2, 12($t4)
	li $t4, 4
	mul $t4, $a2, $t4
	add $t4, $t2, $t4
	lw $t4, 0($t4)
	beq $t4, $a3, L42
L43:
	la $a0, L40
L44:
	la $t4, print
	jalr $t4
	j L45
L42:
	la $a0, L39
	j L44
L38:
	la $t4, print
	la $a0, L50
	jalr $t4
	j L51
L36:
	la $t4, print
	la $a0, L56
	jalr $t4
	j L123
L123:
	
	move $sp, $fp
	lw $fp, 0($sp)
	jr $ra
try:
	sw $fp, 0($sp)
	move $fp, $sp
	addi $sp, $sp, -8
L126:
	sw $a0, 0($fp)
	move $a2, $a1
	lw $a1, 0($fp)
	lw $a1, 4($a1)
	beq $a2, $a1, L84
L85:
	li $a3, 0
L78:
	li $t4, 1
	lw $a1, 0($fp)
	lw $a1, 4($a1)
	addi $a1, $a1, -1
	blt $a3, $a1, L81
L82:
	li $t4, 0
L81:
	li $a1, 0
	beq $t4, $a1, L66
L79:
	li $a1, 1
	lw $t4, 0($fp)
	lw $t2, 8($t4)
	li $t4, 4
	mul $t4, $a3, $t4
	add $t4, $t2, $t4
	lw $t2, 0($t4)
	li $t4, 0
	beq $t2, $t4, L68
L69:
	li $a1, 0
L68:
	li $t5, 1
	lw $t4, 0($fp)
	lw $t7, 16($t4)
	add $t2, $a3, $a2
	li $t4, 4
	mul $t4, $t2, $t4
	add $t4, $t7, $t4
	lw $t2, 0($t4)
	li $t4, 0
	beq $t2, $t4, L71
L72:
	li $t5, 0
L71:
	and $a1, $a1, $t5
	li $t5, 1
	lw $t4, 0($fp)
	lw $t7, 20($t4)
	addi $t4, $a3, 7
	sub $t2, $t4, $a2
	li $t4, 4
	mul $t4, $t2, $t4
	add $t4, $t7, $t4
	lw $t2, 0($t4)
	li $t4, 0
	beq $t2, $t4, L74
L75:
	li $t5, 0
L74:
	and $t4, $a1, $t5
	li $a1, 0
	beq $t4, $a1, L77
L76:
	lw $a1, 0($fp)
	lw $t4, 8($a1)
	li $a1, 4
	mul $a1, $a3, $a1
	add $t4, $t4, $a1
	li $a1, 1
	sw $a1, 0($t4)
	lw $a1, 0($fp)
	lw $t4, 16($a1)
	add $t2, $a3, $a2
	li $a1, 4
	mul $a1, $t2, $a1
	add $t4, $t4, $a1
	li $a1, 1
	sw $a1, 0($t4)
	lw $a1, 0($fp)
	lw $t4, 20($a1)
	addi $a1, $a3, 7
	sub $t2, $a1, $a2
	li $a1, 4
	mul $a1, $t2, $a1
	add $t4, $t4, $a1
	li $a1, 1
	sw $a1, 0($t4)
	lw $a1, 0($fp)
	lw $t4, 12($a1)
	li $a1, 4
	mul $a1, $a2, $a1
	add $a1, $t4, $a1
	sw $a3, 0($a1)
	la $t4, try
	lw $a0, 0($fp)
	addi $a1, $a2, 1
	jalr $t4
	lw $a1, 0($fp)
	lw $t4, 8($a1)
	li $a1, 4
	mul $a1, $a3, $a1
	add $t4, $t4, $a1
	li $a1, 0
	sw $a1, 0($t4)
	lw $a1, 0($fp)
	lw $t2, 16($a1)
	add $t4, $a3, $a2
	li $a1, 4
	mul $a1, $t4, $a1
	add $t4, $t2, $a1
	li $a1, 0
	sw $a1, 0($t4)
	lw $a1, 0($fp)
	lw $t2, 20($a1)
	addi $a1, $a3, 7
	sub $t4, $a1, $a2
	li $a1, 4
	mul $a1, $t4, $a1
	add $t4, $t2, $a1
	li $a1, 0
	sw $a1, 0($t4)
L77:
	j L78
L84:
	la $a1, printboard
	lw $a0, 0($fp)
	jalr $a1
L86:
	j L214
L66:
	li $v0, 0
	j L86
L214:
	
	move $sp, $fp
	lw $fp, 0($sp)
	jr $ra
main:
	sw $fp, 0($sp)
	move $fp, $sp
	addi $sp, $sp, -24
L215:
	sw $a0, 0($fp)
	li $a1, 8
	sw $a1, 4($fp)
	addi $a3, $fp, 8
	la $t4, initArray
	lw $a0, 4($fp)
	li $a1, 0
	jalr $t4
	sw $v0, 0($a3)
	addi $a3, $fp, 12
	la $t4, initArray
	lw $a0, 4($fp)
	li $a1, 0
	jalr $t4
	sw $v0, 0($a3)
	addi $a3, $fp, 16
	la $t2, initArray
	lw $t4, 4($fp)
	lw $a1, 4($fp)
	add $a1, $t4, $a1
	addi $a0, $a1, -1
	li $a1, 0
	jalr $t2
	sw $v0, 0($a3)
	addi $a3, $fp, 20
	la $t2, initArray
	lw $t4, 4($fp)
	lw $a1, 4($fp)
	add $a1, $t4, $a1
	addi $a0, $a1, -1
	li $a1, 0
	jalr $t2
	sw $v0, 0($a3)
	la $t4, try
	move $a0, $fp
	li $a1, 0
	jalr $t4
	j L216
L216:
	
	move $sp, $fp
	lw $fp, 0($sp)
	jr $ra