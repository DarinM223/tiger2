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
S28: .asciiz " O"
S30: .asciiz " ."
S39: .asciiz "\n"
.text
printboard17:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -32
L93:
	sw $a0, 0($fp)
	sw $ra, -8($fp)
	sw $s0, -12($fp)
	sw $s1, -16($fp)
	li $s1, 0
L39:
	li $a2, 1
	lw $s0, 0($fp)
	lw $s0, -8($s0)
	addi $s0, $s0, -1
	ble $s1, $s0, L42
L43:
	li $a2, 0
L42:
	beqz $a2, L27
L40:
	li $s0, 0
L34:
	li $t3, 1
	lw $a2, 0($fp)
	lw $a2, -8($a2)
	addi $a2, $a2, -1
	ble $s0, $a2, L37
L38:
	li $t3, 0
L37:
	beqz $t3, L29
L35:
	lw $a2, 0($fp)
	lw $t3, -16($a2)
	li $a2, 4
	mul $a2, $s1, $a2
	add $a2, $t3, $a2
	lw $a2, 0($a2)
	beq $a2, $s0, L31
L32:
	la $a0, S30
L33:
	la $a2, print
	jalr $a2
	addi $s0, $s0, 1
	j L34
L31:
	la $a0, S28
	j L33
L29:
	la $s0, print
	la $a0, S39
	jalr $s0
	addi $s0, $s1, 1
	move $s1, $s0
	j L39
L27:
	la $s0, print
	la $a0, S39
	jalr $s0
	lw $s1, -16($fp)
	lw $s0, -12($fp)
	lw $ra, -8($fp)
	j L113
L113:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
try20:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -32
L122:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	sw $s0, -16($fp)
	lw $s0, 0($fp)
	lw $a2, -8($s0)
	lw $s0, -8($fp)
	beq $s0, $a2, L72
L73:
	li $s0, 0
L66:
	li $t3, 1
	lw $a2, 0($fp)
	lw $a2, -8($a2)
	addi $a2, $a2, -1
	ble $s0, $a2, L69
L70:
	li $t3, 0
L69:
	beqz $t3, L54
L67:
	li $a2, 1
	lw $t3, 0($fp)
	lw $t7, -12($t3)
	li $t3, 4
	mul $t3, $s0, $t3
	add $t3, $t7, $t3
	lw $t3, 0($t3)
	beqz $t3, L56
L57:
	li $a2, 0
L56:
	li $a1, 1
	lw $t3, 0($fp)
	lw $t5, -20($t3)
	lw $t3, -8($fp)
	add $t7, $s0, $t3
	li $t3, 4
	mul $t3, $t7, $t3
	add $t3, $t5, $t3
	lw $t3, 0($t3)
	beqz $t3, L59
L60:
	li $a1, 0
L59:
	and $a2, $a2, $a1
	li $a1, 1
	lw $t3, 0($fp)
	lw $t5, -24($t3)
	addi $t7, $s0, 7
	lw $t3, -8($fp)
	sub $t7, $t7, $t3
	li $t3, 4
	mul $t3, $t7, $t3
	add $t3, $t5, $t3
	lw $t3, 0($t3)
	beqz $t3, L62
L63:
	li $a1, 0
L62:
	and $a2, $a2, $a1
	beqz $a2, L65
L64:
	lw $a2, 0($fp)
	lw $t3, -12($a2)
	li $a2, 4
	mul $a2, $s0, $a2
	add $t3, $t3, $a2
	li $a2, 1
	sw $a2, 0($t3)
	lw $a2, 0($fp)
	lw $t7, -20($a2)
	lw $a2, -8($fp)
	add $t3, $s0, $a2
	li $a2, 4
	mul $a2, $t3, $a2
	add $t3, $t7, $a2
	li $a2, 1
	sw $a2, 0($t3)
	lw $a2, 0($fp)
	lw $t7, -24($a2)
	addi $t3, $s0, 7
	lw $a2, -8($fp)
	sub $t3, $t3, $a2
	li $a2, 4
	mul $a2, $t3, $a2
	add $t3, $t7, $a2
	li $a2, 1
	sw $a2, 0($t3)
	lw $a2, 0($fp)
	lw $t7, -16($a2)
	li $t3, 4
	lw $a2, -8($fp)
	mul $a2, $a2, $t3
	add $a2, $t7, $a2
	sw $s0, 0($a2)
	la $t3, try20
	lw $a0, 0($fp)
	lw $a2, -8($fp)
	addi $a1, $a2, 1
	jalr $t3
	lw $a2, 0($fp)
	lw $t3, -12($a2)
	li $a2, 4
	mul $a2, $s0, $a2
	add $t3, $t3, $a2
	li $a2, 0
	sw $a2, 0($t3)
	lw $a2, 0($fp)
	lw $t7, -20($a2)
	lw $a2, -8($fp)
	add $t3, $s0, $a2
	li $a2, 4
	mul $a2, $t3, $a2
	add $t3, $t7, $a2
	li $a2, 0
	sw $a2, 0($t3)
	lw $a2, 0($fp)
	lw $t7, -24($a2)
	addi $t3, $s0, 7
	lw $a2, -8($fp)
	sub $t3, $t3, $a2
	li $a2, 4
	mul $a2, $t3, $a2
	add $t3, $t7, $a2
	li $a2, 0
	sw $a2, 0($t3)
L65:
	addi $s0, $s0, 1
	j L66
L72:
	la $s0, printboard17
	lw $a0, 0($fp)
	jalr $s0
L74:
	lw $s0, -16($fp)
	lw $ra, -12($fp)
	j L206
L54:
	li $v0, 0
	j L74
L206:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
main:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -48
L221:
	sw $a0, 0($fp)
	sw $ra, -28($fp)
	sw $s0, -32($fp)
	li $s0, 8
	sw $s0, -8($fp)
	addi $s0, $fp, -12
	la $a2, initArray
	lw $a0, -8($fp)
	li $a1, 0
	jalr $a2
	sw $v0, 0($s0)
	addi $s0, $fp, -16
	la $a2, initArray
	lw $a0, -8($fp)
	li $a1, 0
	jalr $a2
	sw $v0, 0($s0)
	addi $s0, $fp, -20
	la $t7, initArray
	lw $t3, -8($fp)
	lw $a2, -8($fp)
	add $a2, $t3, $a2
	addi $a0, $a2, -1
	li $a1, 0
	jalr $t7
	sw $v0, 0($s0)
	addi $s0, $fp, -24
	la $t7, initArray
	lw $t3, -8($fp)
	lw $a2, -8($fp)
	add $a2, $t3, $a2
	addi $a0, $a2, -1
	li $a1, 0
	jalr $t7
	sw $v0, 0($s0)
	la $s0, try20
	move $a0, $fp
	li $a1, 0
	jalr $s0
	lw $s0, -32($fp)
	lw $ra, -28($fp)
	j L222
L222:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra