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
L28: .asciiz "0"
L29: .asciiz "0"
L30: .asciiz "0"
L31: .asciiz "0"
.text
incr:
	sw $fp, 0($sp)
	move $fp, $sp
	addi $sp, $sp, -32
L59:
	sw $a0, 4($fp)
	sw $a1, -4($fp)
	sw $ra, -8($fp)
	li $t4, 10
	lw $a1, -4($fp)
	beq $a1, $t4, L38
L39:
	li $a1, 0
	sw $a1, -12($fp)
L32:
	li $t2, 1
	li $t4, 9
	lw $a1, -12($fp)
	ble $a1, $t4, L35
L36:
	li $t2, 0
L35:
	li $a1, 0
	beq $t2, $a1, L27
L33:
	la $a1, ord
	la $a0, L28
	jalr $a1
	la $t4, chr
	lw $a1, -4($fp)
	add $a0, $v0, $a1
	jalr $t4
	la $a1, print
	move $a0, $v0
	jalr $a1
	la $a1, ord
	la $a0, L29
	jalr $a1
	la $t4, chr
	lw $a1, -12($fp)
	add $a0, $v0, $a1
	jalr $t4
	la $a1, print
	move $a0, $v0
	jalr $a1
	la $t4, incr
	lw $a0, 4($fp)
	lw $a1, -4($fp)
	addi $a1, $a1, 1
	jalr $t4
	la $a1, ord
	la $a0, L30
	jalr $a1
	la $t4, chr
	lw $a1, -4($fp)
	add $a0, $v0, $a1
	jalr $t4
	move $a0, $v0
	la $a1, print
	jalr $a1
	la $a1, ord
	la $a0, L31
	jalr $a1
	la $t4, chr
	lw $a1, -12($fp)
	add $a0, $v0, $a1
	jalr $t4
	move $a0, $v0
	la $a1, print
	jalr $a1
	lw $a1, -12($fp)
	addi $a1, $a1, 1
	sw $a1, -12($fp)
	j L32
L38:
	li $v0, 0
L40:
	lw $ra, -8($fp)
	j L95
L27:
	li $v0, 0
	j L40
L95:
	
	move $sp, $fp
	lw $fp, 0($sp)
	jr $ra
main:
	sw $fp, 0($sp)
	move $fp, $sp
	addi $sp, $sp, -24
L109:
	sw $a0, 4($fp)
	sw $ra, -4($fp)
	la $t4, incr
	move $a0, $fp
	li $a1, 0
	jalr $t4
	lw $ra, -4($fp)
	j L110
L110:
	
	move $sp, $fp
	lw $fp, 0($sp)
	jr $ra