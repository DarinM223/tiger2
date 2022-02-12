.text
initArray:
	li $a2, 4
	mul $a0, $a0, $a2
	li $v0, 9
	syscall
	move $v1, $v0
	add $a0, $a0, $v0
	_initArray_0:
	sw $a1, ($v1)
	add $v1, $v1, 4
	bne $v1, $a0, _initArray_0
	jr $ra

allocRecord:
  li $a2, 4
  mul $a0, $a0, $a2
  li $v0, 9
  syscall
  jr $ra

printi:
    li $v0, 1
    syscall
    jr $ra

print:
    li $v0, 4
    syscall
    jr $ra

flush:
    jr $ra

strcmp:
    strcmptest:
    lb $a2 ($a0)
    lb $a3 ($a1)
    beq $a2, $zero, strcmpend
    beq $a3, $zero, strcmpend
    bgt $a2, $a3  strcmpgreat
    blt $a2, $a3  strcmpless
    add $a0, $a0, 1
    add $a1, $a1, 1
    j strcmptest
    strcmpgreat:
    li $v0, 1
    jr $ra
    strcmpless:
    li $v0, -1
    jr $ra
    strcmpend:
    bne $a2 $zero strcmpgreat
    bne $a3 $zero strcmpless
    li $v0, 0
    jr $ra

size:
    move $v0, $zero
    sizeloop:
    lb $a1 ($a0)
    beq $a1, $zero sizeexit
    add $v0, $v0, 1
    add $a0, $a0, 1
    j sizeloop
    sizeexit:
    jr $ra

ord:
    lb $a1,($a0)
    li $v0,-1
    beqz $a1,Lrunt5
    lb $v0,($a0)
    Lrunt5:
    jr $ra

getchar:
    li $v0, 9
    li $a0, 2
    syscall
    move $a0, $v0
    li $a1, 2
    li $v0, 8
    syscall
    move $v0, $a0
    jr $ra

chr:
    move $a1, $a0
    li $v0, 9
    li $a0, 2
    syscall
    sb $a1 ($v0)
    sb $zero 1($v0)
    jr $ra

exit:
    li $v0, 10
    syscall

substring:
    add $a1, $a0, $a1
    move $a3, $a1
    li $v0, 9
    add $a2, $a2, 1
    move $a0, $a2
    add $a0, $a0, 1
    syscall
    # got a new string in $v0
    add $a2,$a2,$a3
    add $a2,$a2,-1
    move $a0, $v0
    substringcopy:
    beq $a1 $a2 substringexit
    lb $a3 ($a1)
    sb $a3 ($a0)
    add $a1, $a1, 1
    add $a0, $a0, 1
    j substringcopy
    substringexit:
    sb $zero, ($a0)
    jr $ra

copy:
    copyloop:
    lb $a2, ($a1)
    beq $zero, $a2 copyexit
    sb $a2, ($a0)
    add $a0,$a0,1
    add $a1,$a1,1
    j copyloop
    copyexit:
    sb $zero, ($a0)
    move $v0, $a0
    jr $ra

concat:
    sw $a0, -4($sp)
    sw $a1, -8($sp)
    sw $ra, -12($sp)
    jal size
    li $a3, 1
    add $a3,$a3,$v0
    lw $a0, -8($sp)
    jal size
    add $a3, $a3, $v0
    move $a0, $a3
    li $v0, 9
    syscall
    move $a3, $v0
    move $a0, $v0
    lw   $a1, -4($sp)
    jal copy
    move $a0, $v0
    lw $a1, -8($sp)
    jal copy
    move $v0, $a3
    lw $ra, -12($sp)
    jr $ra
.data
L30: .asciiz " O"
L31: .asciiz " ."
L41: .asciiz "\n"
L47: .asciiz "\n"
.text
printboard:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -32
L97:
	sw $a0, 0($fp)
	sw $ra, -8($fp)
	sw $s0, -12($fp)
	sw $s1, -16($fp)
	li $s0, 0
L42:
	li $t4, 1
	lw $a1, 0($fp)
	lw $a1, -8($a1)
	addi $a1, $a1, -1
	ble $s0, $a1, L45
L46:
	li $t4, 0
L45:
	li $a1, 0
	beq $t4, $a1, L27
L43:
	li $s1, 0
L36:
	li $t4, 1
	lw $a1, 0($fp)
	lw $a1, -8($a1)
	addi $a1, $a1, -1
	ble $s1, $a1, L39
L40:
	li $t4, 0
L39:
	li $a1, 0
	beq $t4, $a1, L29
L37:
	lw $a1, 0($fp)
	lw $t4, -16($a1)
	li $a1, 4
	mul $a1, $s0, $a1
	add $a1, $t4, $a1
	lw $a1, 0($a1)
	beq $a1, $s1, L33
L34:
	la $a0, L31
L35:
	la $a1, print
	jalr $a1
	addi $s1, $s1, 1
	j L36
L33:
	la $a0, L30
	j L35
L29:
	la $a1, print
	la $a0, L41
	jalr $a1
	addi $s0, $s0, 1
	j L42
L27:
	la $a1, print
	la $a0, L47
	jalr $a1
	lw $s1, -16($fp)
	lw $s0, -12($fp)
	lw $ra, -8($fp)
	j L119
L119:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
try:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -32
L128:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	lw $a1, 0($fp)
	lw $t4, -8($a1)
	lw $a1, -8($fp)
	beq $a1, $t4, L76
L77:
	li $a1, 0
	sw $a1, -16($fp)
L70:
	li $t2, 1
	lw $a1, 0($fp)
	lw $a1, -8($a1)
	addi $t4, $a1, -1
	lw $a1, -16($fp)
	ble $a1, $t4, L73
L74:
	li $t2, 0
L73:
	li $a1, 0
	beq $t2, $a1, L58
L71:
	li $a1, 1
	lw $t4, 0($fp)
	lw $t2, -12($t4)
	li $a3, 4
	lw $t4, -16($fp)
	mul $t4, $t4, $a3
	add $t4, $t2, $t4
	lw $t2, 0($t4)
	li $t4, 0
	beq $t2, $t4, L60
L61:
	li $a1, 0
L60:
	li $t7, 1
	lw $t4, 0($fp)
	lw $a3, -20($t4)
	lw $t2, -8($fp)
	lw $t4, -16($fp)
	add $t2, $t4, $t2
	li $t4, 4
	mul $t4, $t2, $t4
	add $t4, $a3, $t4
	lw $t2, 0($t4)
	li $t4, 0
	beq $t2, $t4, L63
L64:
	li $t7, 0
L63:
	and $a1, $a1, $t7
	li $a3, 1
	lw $t4, 0($fp)
	lw $t2, -24($t4)
	lw $t4, -16($fp)
	addi $t7, $t4, 7
	lw $t4, -8($fp)
	sub $t7, $t7, $t4
	li $t4, 4
	mul $t4, $t7, $t4
	add $t4, $t2, $t4
	lw $t2, 0($t4)
	li $t4, 0
	beq $t2, $t4, L66
L67:
	li $a3, 0
L66:
	and $t4, $a1, $a3
	li $a1, 0
	beq $t4, $a1, L69
L68:
	lw $a1, 0($fp)
	lw $t2, -12($a1)
	li $t4, 4
	lw $a1, -16($fp)
	mul $a1, $a1, $t4
	add $t4, $t2, $a1
	li $a1, 1
	sw $a1, 0($t4)
	lw $a1, 0($fp)
	lw $t2, -20($a1)
	lw $t4, -8($fp)
	lw $a1, -16($fp)
	add $t4, $a1, $t4
	li $a1, 4
	mul $a1, $t4, $a1
	add $t4, $t2, $a1
	li $a1, 1
	sw $a1, 0($t4)
	lw $a1, 0($fp)
	lw $t4, -24($a1)
	lw $a1, -16($fp)
	addi $t2, $a1, 7
	lw $a1, -8($fp)
	sub $t2, $t2, $a1
	li $a1, 4
	mul $a1, $t2, $a1
	add $t4, $t4, $a1
	li $a1, 1
	sw $a1, 0($t4)
	lw $a1, 0($fp)
	lw $t2, -16($a1)
	li $t4, 4
	lw $a1, -8($fp)
	mul $a1, $a1, $t4
	add $t4, $t2, $a1
	lw $a1, -16($fp)
	sw $a1, 0($t4)
	la $t4, try
	lw $a0, 0($fp)
	lw $a1, -8($fp)
	addi $a1, $a1, 1
	jalr $t4
	lw $a1, 0($fp)
	lw $t2, -12($a1)
	li $t4, 4
	lw $a1, -16($fp)
	mul $a1, $a1, $t4
	add $t4, $t2, $a1
	li $a1, 0
	sw $a1, 0($t4)
	lw $a1, 0($fp)
	lw $t2, -20($a1)
	lw $t4, -8($fp)
	lw $a1, -16($fp)
	add $t4, $a1, $t4
	li $a1, 4
	mul $a1, $t4, $a1
	add $t4, $t2, $a1
	li $a1, 0
	sw $a1, 0($t4)
	lw $a1, 0($fp)
	lw $t4, -24($a1)
	lw $a1, -16($fp)
	addi $t2, $a1, 7
	lw $a1, -8($fp)
	sub $t2, $t2, $a1
	li $a1, 4
	mul $a1, $t2, $a1
	add $t4, $t4, $a1
	li $a1, 0
	sw $a1, 0($t4)
L69:
	lw $a1, -16($fp)
	addi $a1, $a1, 1
	sw $a1, -16($fp)
	j L70
L76:
	la $a1, printboard
	lw $a0, 0($fp)
	jalr $a1
L78:
	lw $ra, -12($fp)
	j L217
L58:
	li $v0, 0
	j L78
L217:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
main:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -48
L244:
	sw $a0, 0($fp)
	sw $ra, -28($fp)
	sw $s0, -32($fp)
	li $a1, 8
	sw $a1, -8($fp)
	addi $s0, $fp, -12
	la $t4, initArray
	lw $a0, -8($fp)
	li $a1, 0
	jalr $t4
	sw $v0, 0($s0)
	addi $s0, $fp, -16
	la $t4, initArray
	lw $a0, -8($fp)
	li $a1, 0
	jalr $t4
	sw $v0, 0($s0)
	addi $s0, $fp, -20
	la $t2, initArray
	lw $t4, -8($fp)
	lw $a1, -8($fp)
	add $a1, $t4, $a1
	addi $a0, $a1, -1
	li $a1, 0
	jalr $t2
	sw $v0, 0($s0)
	addi $s0, $fp, -24
	la $t2, initArray
	lw $t4, -8($fp)
	lw $a1, -8($fp)
	add $a1, $t4, $a1
	addi $a0, $a1, -1
	li $a1, 0
	jalr $t2
	sw $v0, 0($s0)
	la $t4, try
	move $a0, $fp
	li $a1, 0
	jalr $t4
	lw $s0, -32($fp)
	lw $ra, -28($fp)
	j L245
L245:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra