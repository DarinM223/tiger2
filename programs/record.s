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
.text
sum15:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -28
L52:
	sw $a0, 0($fp)
	sw $ra, -8($fp)
	sw $s0, -12($fp)
	lw $t4, 0($fp)
	lw $t4, -8($t4)
	beq $a1, $t4, L27
L28:
	lw $s0, 0($a1)
	la $t4, sum15
	lw $a0, 0($fp)
	lw $a1, 4($a1)
	jalr $t4
	add $v0, $s0, $v0
L29:
	lw $s0, -12($fp)
	lw $ra, -8($fp)
	j L61
L27:
	li $v0, 0
	j L29
L61:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
.data
S23: .asciiz "0"
.text
main:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -44
L66:
	sw $a0, 0($fp)
	sw $ra, -12($fp)
	sw $s0, -16($fp)
	sw $s1, -20($fp)
	sw $s2, -24($fp)
	sw $s3, -28($fp)
	li $a1, 0
	sw $a1, -8($fp)
	la $a1, allocRecord
	li $a0, 8
	jalr $a1
	move $s0, $v0
	li $a1, 0
	sw $a1, 0($s0)
	addi $a1, $s0, 4
	move $s2, $a1
	la $a1, allocRecord
	li $a0, 8
	jalr $a1
	move $s3, $v0
	li $a1, 1
	sw $a1, 0($s3)
	addi $a1, $s3, 4
	move $s1, $a1
	la $a1, allocRecord
	li $a0, 8
	jalr $a1
	li $a1, 2
	sw $a1, 0($v0)
	lw $a1, -8($fp)
	sw $a1, 4($v0)
	sw $v0, 0($s1)
	sw $s3, 0($s2)
	move $s1, $s0
	la $a1, ord
	la $a0, S23
	jalr $a1
	move $s2, $v0
	la $t4, sum15
	move $a0, $fp
	move $a1, $s1
	jalr $t4
	la $a1, chr
	add $a0, $s2, $v0
	jalr $a1
	la $a1, print
	move $a0, $v0
	jalr $a1
	lw $s3, -28($fp)
	lw $s2, -24($fp)
	lw $s1, -20($fp)
	lw $s0, -16($fp)
	lw $ra, -12($fp)
	j L67
L67:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra