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
L29: .asciiz "9"
.text
isdigit:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -36
L169:
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
	la $a0, L28
	jalr $a1
	bge $s2, $v0, L31
L32:
	li $s1, 0
L31:
	move $s2, $s1
	li $s0, 1
	la $t4, ord
	lw $a1, 0($fp)
	lw $a1, 0($a1)
	lw $a0, -8($a1)
	jalr $t4
	move $s1, $v0
	la $a1, ord
	la $a0, L29
	jalr $a1
	ble $s1, $v0, L34
L35:
	li $s0, 0
L34:
	and $v0, $s2, $s0
	lw $s2, -20($fp)
	lw $s1, -16($fp)
	lw $s0, -12($fp)
	lw $ra, -8($fp)
	j L189
L189:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
.data
L45: .asciiz " "
L46: .asciiz "\n"
L57: .asciiz "0"
.text
readint:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -40
L199:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	sw $s0, -16($fp)
	sw $s1, -20($fp)
	sw $s2, -24($fp)
	li $s0, 0
L54:
	li $s1, 1
	la $t4, strcmp
	lw $a1, 0($fp)
	lw $a0, -8($a1)
	la $a1, L45
	jalr $t4
	beqz $v0, L48
L49:
	li $s1, 0
L48:
	li $s2, 1
	la $t4, strcmp
	lw $a1, 0($fp)
	lw $a0, -8($a1)
	la $a1, L46
	jalr $t4
	beqz $v0, L51
L52:
	li $s2, 0
L51:
	or $a1, $s1, $s2
	beqz $a1, L53
L55:
	lw $a1, 0($fp)
	addi $s1, $a1, -8
	la $a1, getchar
	jalr $a1
	sw $v0, 0($s1)
	j L54
L53:
	lw $a1, -8($fp)
	addi $s1, $a1, 0
	la $t4, isdigit
	move $a0, $fp
	lw $a1, 0($fp)
	lw $a1, -8($a1)
	jalr $t4
	sw $v0, 0($s1)
L58:
	la $t4, isdigit
	move $a0, $fp
	lw $a1, 0($fp)
	lw $a1, -8($a1)
	jalr $t4
	beqz $v0, L56
L59:
	li $a1, 10
	mul $s1, $s0, $a1
	la $t4, ord
	lw $a1, 0($fp)
	lw $a0, -8($a1)
	jalr $t4
	add $s1, $s1, $v0
	la $a1, ord
	la $a0, L57
	jalr $a1
	sub $a1, $s1, $v0
	move $s0, $a1
	lw $a1, 0($fp)
	addi $s1, $a1, -8
	la $a1, getchar
	jalr $a1
	sw $v0, 0($s1)
	j L58
L56:
	move $v0, $s0
	lw $s2, -24($fp)
	lw $s1, -20($fp)
	lw $s0, -16($fp)
	lw $ra, -12($fp)
	j L245
L245:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
readlist:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -32
L256:
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
	la $t4, readint
	lw $a0, 0($fp)
	move $a1, $s0
	jalr $t4
	move $s1, $v0
	lw $a1, 0($s0)
	beqz $a1, L79
L78:
	la $a1, allocRecord
	li $a0, 8
	jalr $a1
	move $s0, $v0
	sw $s1, 0($s0)
	addi $a1, $s0, 4
	move $s1, $a1
	la $a1, readlist
	lw $a0, 0($fp)
	jalr $a1
	sw $v0, 0($s1)
	move $v0, $s0
L80:
	lw $s1, -16($fp)
	lw $s0, -12($fp)
	lw $ra, -8($fp)
	j L270
L79:
	lw $a1, 0($fp)
	addi $s1, $a1, -8
	la $a1, getchar
	jalr $a1
	sw $v0, 0($s1)
	li $v0, 0
	j L80
L270:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
merge:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -44
L282:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $a2, -12($fp)
	sw $ra, -16($fp)
	sw $s0, -20($fp)
	sw $s1, -24($fp)
	sw $s2, -28($fp)
	lw $a1, -8($fp)
	beqz $a1, L101
L102:
	lw $a1, -12($fp)
	beqz $a1, L97
L98:
	lw $a1, -8($fp)
	lw $t4, 0($a1)
	lw $a1, -12($fp)
	lw $a1, 0($a1)
	blt $t4, $a1, L93
L94:
	la $a1, allocRecord
	li $a0, 8
	jalr $a1
	move $s2, $v0
	lw $a1, -12($fp)
	lw $a1, 0($a1)
	sw $a1, 0($s2)
	addi $s1, $s2, 4
	la $t2, merge
	lw $a0, 0($fp)
	lw $a1, -8($fp)
	lw $t4, -12($fp)
	lw $a2, 4($t4)
	jalr $t2
	sw $v0, 0($s1)
L95:
L99:
L103:
	move $v0, $s2
	lw $s2, -28($fp)
	lw $s1, -24($fp)
	lw $s0, -20($fp)
	lw $ra, -16($fp)
	j L294
L101:
	lw $s2, -12($fp)
	j L103
L97:
	lw $s2, -8($fp)
	j L99
L93:
	la $a1, allocRecord
	li $a0, 8
	jalr $a1
	move $s2, $v0
	lw $a1, -8($fp)
	lw $a1, 0($a1)
	sw $a1, 0($s2)
	addi $s1, $s2, 4
	la $t4, merge
	lw $a0, 0($fp)
	lw $a1, -8($fp)
	lw $a1, 4($a1)
	lw $a2, -12($fp)
	jalr $t4
	sw $v0, 0($s1)
	j L95
L294:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
.data
L114: .asciiz "0"
.text
f:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -32
L326:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	sw $s0, -16($fp)
	lw $a1, -8($fp)
	bgtz $a1, L115
L116:
	li $v0, 0
	lw $s0, -16($fp)
	lw $ra, -12($fp)
	j L327
L115:
	la $t2, f
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
	la $a0, L114
	jalr $a1
	la $a1, chr
	add $a0, $s0, $v0
	jalr $a1
	move $a0, $v0
	la $a1, print
	jalr $a1
	j L116
L327:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
.data
L126: .asciiz "-"
L127: .asciiz "0"
.text
printint:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -28
L354:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	lw $a1, -8($fp)
	bltz $a1, L133
L134:
	lw $a1, -8($fp)
	bgtz $a1, L129
L130:
	la $a1, print
	la $a0, L127
	jalr $a1
L131:
L135:
	lw $ra, -12($fp)
	j L357
L133:
	la $a1, print
	la $a0, L126
	jalr $a1
	la $t2, f
	move $a0, $fp
	li $t4, 0
	lw $a1, -8($fp)
	sub $a1, $t4, $a1
	jalr $t2
	j L135
L129:
	la $t4, f
	move $a0, $fp
	lw $a1, -8($fp)
	jalr $t4
	j L131
L357:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
.data
L145: .asciiz "\n"
L146: .asciiz " "
.text
printlist:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -28
L371:
	sw $a0, 0($fp)
	sw $a1, -8($fp)
	sw $ra, -12($fp)
	lw $a1, -8($fp)
	beqz $a1, L148
L149:
	la $t4, printint
	lw $a0, 0($fp)
	lw $a1, -8($fp)
	lw $a1, 0($a1)
	jalr $t4
	la $a1, print
	la $a0, L146
	jalr $a1
	la $t4, printlist
	lw $a0, 0($fp)
	lw $a1, -8($fp)
	lw $a1, 4($a1)
	jalr $t4
L150:
	lw $ra, -12($fp)
	j L380
L148:
	la $a1, print
	la $a0, L145
	jalr $a1
	j L150
L380:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra
main:
	sw $fp, -4($sp)
	move $fp, $sp
	addi $sp, $sp, -36
L389:
	sw $a0, 0($fp)
	sw $ra, -12($fp)
	sw $s0, -16($fp)
	sw $s1, -20($fp)
	addi $s1, $fp, -8
	la $a1, getchar
	jalr $a1
	sw $v0, 0($s1)
	move $s1, $fp
	la $a1, readlist
	move $a0, $fp
	jalr $a1
	move $s0, $v0
	la $a1, readlist
	move $a0, $fp
	jalr $a1
	la $t4, merge
	move $a0, $fp
	move $a1, $s0
	move $a2, $v0
	jalr $t4
	move $a1, $v0
	la $t4, printlist
	move $a0, $s1
	jalr $t4
	lw $s1, -20($fp)
	lw $s0, -16($fp)
	lw $ra, -12($fp)
	j L390
L390:
	
	move $sp, $fp
	lw $fp, -4($sp)
	jr $ra