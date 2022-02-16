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
S29: .asciiz "0"
S31: .asciiz "9"
.text
isdigit15:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -36
L159:
	sw $a0, 0($fp)
	sw $ra, -8($fp)
	sw $s0, -12($fp)
	sw $s1, -16($fp)
	sw $s2, -20($fp)
	li $s1, 1
	la $t4, ord
	lw $a1, 0($fp)
	lw $a1, 0($a1)
	lw $a0, -8($a1)
	jalr $t4
	move $s2, $v0
	la $a1, ord
	la $a0, S29
	jalr $a1
	bge $s2, $v0, L29
L30:
	li $s1, 0
L29:
	move $s2, $s1
	li $s0, 1
	la $t4, ord
	lw $a1, 0($fp)
	lw $a1, 0($a1)
	lw $a0, -8($a1)
	jalr $t4
	move $s1, $v0
	la $a1, ord
	la $a0, S31
	jalr $a1
	ble $s1, $v0, L32
L33:
	li $s0, 0
L32:
	and $v0, $s2, $s0
	lw $s2, -20($fp)
	lw $s1, -16($fp)
	lw $s0, -12($fp)
	lw $ra, -8($fp)
	j L179
L179:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
.data
S37: .asciiz " "
S40: .asciiz "\n"
.text
readint13:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -40
L189:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	sw $s0, -16($fp)
	sw $s1, -20($fp)
	sw $s2, -24($fp)
	li $s0, 0
L50:
	li $s1, 1
	la $t4, strcmp
	lw $a1, 0($fp)
	lw $a0, -8($a1)
	la $a1, S37
	jalr $t4
	beqz $v0, L44
L45:
	li $s1, 0
L44:
	li $s2, 1
	la $t4, strcmp
	lw $a1, 0($fp)
	lw $a0, -8($a1)
	la $a1, S40
	jalr $t4
	beqz $v0, L47
L48:
	li $s2, 0
L47:
	or $a1, $s1, $s2
	beqz $a1, L49
L51:
	lw $a1, 0($fp)
	addi $s1, $a1, -8
	la $a1, getchar
	jalr $a1
	sw $v0, 0($s1)
	j L50
L49:
	lw $a1, -8($fp)
	addi $s1, $a1, 0
	la $t4, isdigit15
	move $a0, $fp
	lw $a1, 0($fp)
	lw $a1, -8($a1)
	jalr $t4
	sw $v0, 0($s1)
L53:
	la $t4, isdigit15
	move $a0, $fp
	lw $a1, 0($fp)
	lw $a1, -8($a1)
	jalr $t4
	beqz $v0, L52
L54:
	li $a1, 10
	mul $s1, $s0, $a1
	la $t4, ord
	lw $a1, 0($fp)
	lw $a0, -8($a1)
	jalr $t4
	add $s1, $s1, $v0
	la $a1, ord
	la $a0, S29
	jalr $a1
	sub $a1, $s1, $v0
	move $s0, $a1
	lw $a1, 0($fp)
	addi $s1, $a1, -8
	la $a1, getchar
	jalr $a1
	sw $v0, 0($s1)
	j L53
L52:
	move $v0, $s0
	lw $s2, -24($fp)
	lw $s1, -20($fp)
	lw $s0, -16($fp)
	lw $ra, -12($fp)
	j L235
L235:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
readlist20:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -32
L246:
	sw $a0, 0($fp)
	sw $ra, -8($fp)
	sw $s0, -12($fp)
	sw $s1, -16($fp)
	la $a1, allocRecord
	li $a0, 4
	jalr $a1
	li $a1, 0
	sw $a1, 0($v0)
	move $s0, $v0
	la $t4, readint13
	lw $a0, 0($fp)
	move $a1, $s0
	jalr $t4
	move $s1, $v0
	lw $a1, 0($s0)
	beqz $a1, L74
L73:
	la $a1, allocRecord
	li $a0, 8
	jalr $a1
	move $s0, $v0
	sw $s1, 0($s0)
	addi $a1, $s0, 4
	move $s1, $a1
	la $a1, readlist20
	lw $a0, 0($fp)
	jalr $a1
	sw $v0, 0($s1)
	move $v0, $s0
L75:
	lw $s1, -16($fp)
	lw $s0, -12($fp)
	lw $ra, -8($fp)
	j L260
L74:
	lw $a1, 0($fp)
	addi $s1, $a1, -8
	la $a1, getchar
	jalr $a1
	sw $v0, 0($s1)
	li $v0, 0
	j L75
L260:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
merge21:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -44
L272:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $a2, -12($fp)
	sw $ra, -16($fp)
	sw $s0, -20($fp)
	sw $s1, -24($fp)
	sw $s2, -28($fp)
	lw $a1, -8($fp)
	beqz $a1, L96
L97:
	lw $a1, -12($fp)
	beqz $a1, L92
L93:
	lw $a1, -8($fp)
	lw $t4, 0($a1)
	lw $a1, -12($fp)
	lw $a1, 0($a1)
	blt $t4, $a1, L88
L89:
	la $a1, allocRecord
	li $a0, 8
	jalr $a1
	move $s2, $v0
	lw $a1, -12($fp)
	lw $a1, 0($a1)
	sw $a1, 0($s2)
	addi $s1, $s2, 4
	la $t2, merge21
	lw $a0, 0($fp)
	lw $a1, -8($fp)
	lw $t4, -12($fp)
	lw $a2, 4($t4)
	jalr $t2
	sw $v0, 0($s1)
L90:
	move $v0, $s2
L94:
L98:
	lw $s2, -28($fp)
	lw $s1, -24($fp)
	lw $s0, -20($fp)
	lw $ra, -16($fp)
	j L284
L96:
	lw $v0, -12($fp)
	j L98
L92:
	lw $v0, -8($fp)
	j L94
L88:
	la $a1, allocRecord
	li $a0, 8
	jalr $a1
	move $s2, $v0
	lw $a1, -8($fp)
	lw $a1, 0($a1)
	sw $a1, 0($s2)
	addi $s1, $s2, 4
	la $t4, merge21
	lw $a0, 0($fp)
	lw $a1, -8($fp)
	lw $a1, 4($a1)
	lw $a2, -12($fp)
	jalr $t4
	sw $v0, 0($s1)
	j L90
L284:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
.data
.text
f25:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -32
L316:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	sw $s0, -16($fp)
	lw $a1, -8($fp)
	bgtz $a1, L109
L110:
	li $v0, 0
	lw $s0, -16($fp)
	lw $ra, -12($fp)
	j L317
L109:
	la $t2, f25
	lw $a0, 0($fp)
	li $t4, 10
	lw $a1, -8($fp)
	div $a1, $a1, $t4
	jalr $t2
	li $t4, 10
	lw $a1, -8($fp)
	div $t4, $a1, $t4
	li $a1, 10
	mul $t4, $t4, $a1
	lw $a1, -8($fp)
	sub $a1, $a1, $t4
	move $s0, $a1
	la $a1, ord
	la $a0, S29
	jalr $a1
	la $a1, chr
	add $a0, $s0, $v0
	jalr $a1
	move $a0, $v0
	la $a1, print
	jalr $a1
	j L110
L317:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
.data
S73: .asciiz "-"
.text
printint24:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -28
L344:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	lw $a1, -8($fp)
	bltz $a1, L125
L126:
	lw $a1, -8($fp)
	bgtz $a1, L121
L122:
	la $a1, print
	la $a0, S29
	jalr $a1
L123:
L127:
	lw $ra, -12($fp)
	j L347
L125:
	la $a1, print
	la $a0, S73
	jalr $a1
	la $t2, f25
	move $a0, $fp
	li $t4, 0
	lw $a1, -8($fp)
	sub $a1, $t4, $a1
	jalr $t2
	j L127
L121:
	la $t4, f25
	move $a0, $fp
	lw $a1, -8($fp)
	jalr $t4
	j L123
L347:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
.data
.text
printlist26:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -28
L361:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	lw $a1, -8($fp)
	beqz $a1, L138
L139:
	la $t4, printint24
	lw $a0, 0($fp)
	lw $a1, -8($fp)
	lw $a1, 0($a1)
	jalr $t4
	la $a1, print
	la $a0, S37
	jalr $a1
	la $t4, printlist26
	lw $a0, 0($fp)
	lw $a1, -8($fp)
	lw $a1, 4($a1)
	jalr $t4
L140:
	lw $ra, -12($fp)
	j L370
L138:
	la $a1, print
	la $a0, S40
	jalr $a1
	j L140
L370:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
main:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -36
L379:
	sw $a0, 0($fp)
	sw $ra, -12($fp)
	sw $s0, -16($fp)
	sw $s1, -20($fp)
	addi $s1, $fp, -8
	la $a1, getchar
	jalr $a1
	sw $v0, 0($s1)
	move $s1, $fp
	la $a1, readlist20
	move $a0, $fp
	jalr $a1
	move $s0, $v0
	la $a1, readlist20
	move $a0, $fp
	jalr $a1
	move $a2, $v0
	la $t4, merge21
	move $a0, $fp
	move $a1, $s0
	jalr $t4
	la $t4, printlist26
	move $a0, $s1
	move $a1, $v0
	jalr $t4
	lw $s1, -20($fp)
	lw $s0, -16($fp)
	lw $ra, -12($fp)
	j L380
L380:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra