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
	li $s0, 1
	la $t3, ord
	lw $a2, 0($fp)
	lw $a2, 0($a2)
	lw $a0, -8($a2)
	jalr $t3
	move $s2, $v0
	la $a2, ord
	la $a0, S29
	jalr $a2
	bge $s2, $v0, L29
L30:
	li $s0, 0
L29:
	move $s2, $s0
	li $s1, 1
	la $a2, ord
	lw $s0, 0($fp)
	lw $s0, 0($s0)
	lw $a0, -8($s0)
	jalr $a2
	move $s0, $v0
	la $a2, ord
	la $a0, S31
	jalr $a2
	ble $s0, $v0, L32
L33:
	li $s1, 0
L32:
	and $v0, $s2, $s1
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
	li $s1, 0
L50:
	li $s0, 1
	la $t3, strcmp
	lw $a2, 0($fp)
	lw $a0, -8($a2)
	la $a1, S37
	jalr $t3
	beqz $v0, L44
L45:
	li $s0, 0
L44:
	li $s2, 1
	la $t3, strcmp
	lw $a2, 0($fp)
	lw $a0, -8($a2)
	la $a1, S40
	jalr $t3
	beqz $v0, L47
L48:
	li $s2, 0
L47:
	or $s0, $s0, $s2
	beqz $s0, L49
L51:
	lw $s0, 0($fp)
	addi $s0, $s0, -8
	la $a2, getchar
	jalr $a2
	sw $v0, 0($s0)
	j L50
L49:
	lw $s0, -8($fp)
	addi $s0, $s0, 0
	la $t3, isdigit15
	move $a0, $fp
	lw $a2, 0($fp)
	lw $a1, -8($a2)
	jalr $t3
	sw $v0, 0($s0)
L53:
	la $a2, isdigit15
	move $a0, $fp
	lw $s0, 0($fp)
	lw $a1, -8($s0)
	jalr $a2
	beqz $v0, L52
L54:
	li $s0, 10
	mul $s0, $s1, $s0
	la $t3, ord
	lw $a2, 0($fp)
	lw $a0, -8($a2)
	jalr $t3
	add $s0, $s0, $v0
	la $a2, ord
	la $a0, S29
	jalr $a2
	sub $s0, $s0, $v0
	move $s1, $s0
	lw $s0, 0($fp)
	addi $s0, $s0, -8
	la $a2, getchar
	jalr $a2
	sw $v0, 0($s0)
	j L53
L52:
	move $v0, $s1
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
	la $s0, allocRecord
	li $a0, 4
	jalr $s0
	li $s0, 0
	sw $s0, 0($v0)
	move $s1, $v0
	la $s0, readint13
	lw $a0, 0($fp)
	move $a1, $s1
	jalr $s0
	move $s0, $v0
	lw $a2, 0($s1)
	beqz $a2, L74
L73:
	la $a2, allocRecord
	li $a0, 8
	jalr $a2
	move $s1, $v0
	sw $s0, 0($s1)
	addi $s0, $s1, 4
	la $a2, readlist20
	lw $a0, 0($fp)
	jalr $a2
	sw $v0, 0($s0)
	move $v0, $s1
L75:
	lw $s1, -16($fp)
	lw $s0, -12($fp)
	lw $ra, -8($fp)
	j L260
L74:
	lw $s0, 0($fp)
	addi $s0, $s0, -8
	la $a2, getchar
	jalr $a2
	sw $v0, 0($s0)
	li $v0, 0
	j L75
L260:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
merge21:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -40
L272:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $a2, -12($fp)
	sw $ra, -16($fp)
	sw $s0, -20($fp)
	sw $s1, -24($fp)
	lw $s0, -8($fp)
	beqz $s0, L96
L97:
	lw $s0, -12($fp)
	beqz $s0, L92
L93:
	lw $s0, -8($fp)
	lw $a2, 0($s0)
	lw $s0, -12($fp)
	lw $s0, 0($s0)
	blt $a2, $s0, L88
L89:
	la $s0, allocRecord
	li $a0, 8
	jalr $s0
	move $s1, $v0
	lw $s0, -12($fp)
	lw $s0, 0($s0)
	sw $s0, 0($s1)
	addi $s0, $s1, 4
	la $t3, merge21
	lw $a0, 0($fp)
	lw $a1, -8($fp)
	lw $a2, -12($fp)
	lw $a2, 4($a2)
	jalr $t3
	sw $v0, 0($s0)
	move $v0, $s1
L90:
L94:
L98:
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
	la $s0, allocRecord
	li $a0, 8
	jalr $s0
	move $s1, $v0
	lw $s0, -8($fp)
	lw $s0, 0($s0)
	sw $s0, 0($s1)
	addi $s0, $s1, 4
	la $t3, merge21
	lw $a0, 0($fp)
	lw $a2, -8($fp)
	lw $a1, 4($a2)
	lw $a2, -12($fp)
	jalr $t3
	sw $v0, 0($s0)
	move $v0, $s1
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
L314:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	sw $s0, -16($fp)
	lw $s0, -8($fp)
	bgtz $s0, L109
L110:
	li $v0, 0
	lw $s0, -16($fp)
	lw $ra, -12($fp)
	j L315
L109:
	la $t3, f25
	lw $a0, 0($fp)
	li $a2, 10
	lw $s0, -8($fp)
	div $a1, $s0, $a2
	jalr $t3
	li $a2, 10
	lw $s0, -8($fp)
	div $a2, $s0, $a2
	li $s0, 10
	mul $a2, $a2, $s0
	lw $s0, -8($fp)
	sub $s0, $s0, $a2
	la $a2, ord
	la $a0, S29
	jalr $a2
	la $a2, chr
	add $a0, $s0, $v0
	jalr $a2
	la $s0, print
	move $a0, $v0
	jalr $s0
	j L110
L315:
	
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
L342:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	lw $a2, -8($fp)
	bltz $a2, L125
L126:
	lw $a2, -8($fp)
	bgtz $a2, L121
L122:
	la $a2, print
	la $a0, S29
	jalr $a2
L123:
L127:
	lw $ra, -12($fp)
	j L345
L125:
	la $a2, print
	la $a0, S73
	jalr $a2
	la $t7, f25
	move $a0, $fp
	li $t3, 0
	lw $a2, -8($fp)
	sub $a1, $t3, $a2
	jalr $t7
	j L127
L121:
	la $a2, f25
	move $a0, $fp
	lw $a1, -8($fp)
	jalr $a2
	j L123
L345:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
.data
.text
printlist26:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -28
L359:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	lw $a2, -8($fp)
	beqz $a2, L138
L139:
	la $t3, printint24
	lw $a0, 0($fp)
	lw $a2, -8($fp)
	lw $a1, 0($a2)
	jalr $t3
	la $a2, print
	la $a0, S37
	jalr $a2
	la $t3, printlist26
	lw $a0, 0($fp)
	lw $a2, -8($fp)
	lw $a1, 4($a2)
	jalr $t3
L140:
	lw $ra, -12($fp)
	j L368
L138:
	la $a2, print
	la $a0, S40
	jalr $a2
	j L140
L368:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
main:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -36
L377:
	sw $a0, 0($fp)
	sw $ra, -12($fp)
	sw $s0, -16($fp)
	sw $s1, -20($fp)
	addi $s0, $fp, -8
	la $a2, getchar
	jalr $a2
	sw $v0, 0($s0)
	move $s0, $fp
	la $a2, readlist20
	move $a0, $fp
	jalr $a2
	move $s1, $v0
	la $a2, readlist20
	move $a0, $fp
	jalr $a2
	la $t3, merge21
	move $a0, $s0
	move $a1, $s1
	move $a2, $v0
	jalr $t3
	move $a1, $v0
	la $s0, printlist26
	move $a0, $fp
	jalr $s0
	lw $s1, -20($fp)
	lw $s0, -16($fp)
	lw $ra, -12($fp)
	j L378
L378:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra