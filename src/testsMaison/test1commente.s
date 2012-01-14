	.text
main:
	move $fp, $sp
	sw   $ra, -4($fp)
	add  $sp, $fp, -12
	li   $v0, 9 // alloue b
	li   $a0, 4
	syscall
	la   $t0, descr_general_B
	sw   $t0, 0($v0)
	add  $t0, $fp, -8
	sw   $v0, 0($t0) // stocke b
	li   $v0, 9 // alloue A
	li   $a0, 8
	syscall
	la   $t0, descr_general_A
	sw   $t0, 0($v0)
	sw   $v0, 0($sp) // stocke A sur la pile
	sub  $sp, $sp, 4
	la   $t0, descr_general_A // cherche le constructeur de A
	lw   $t0, 4($t0)
	jalr   $t0 // lance le constructeur
	add  $sp, $sp, 4
	lw   $v0, 0($sp) // recharge l'objet A
	lw   $v0, 4($v0) // charge son champ a          o_O
	sw   $v0, 0($sp) // le charge sur la pile 
	sub  $sp, $sp, 4
	add  $v0, $fp, -8 // recharge b
	lw   $v0, 0($v0)
	sw   $v0, 0($sp) // le charge sur la pile
	sub  $sp, $sp, 4
	la   $t0, descr_meth_B // cherche la methode renvoit_zero
	lw   $t0, 0($t0)
	jalr   $t0  // lance cette methode
	add  $sp, $sp, 4 // libere la pile de b
	add  $sp, $sp, 4 // libere la pile de A
	lw   $t0, 0($sp) // recharge A
	sw   $v0, 0($t0) // charge son champ a
fin_main:
	add  $sp, $sp, 12
	lw   $ra, -4($fp)
	jr   $ra
debut_meth_1:
	sw   $fp, 0($sp)
	sub  $fp, $sp, 4
	sw   $ra, -4($fp)
	add  $sp, $fp, -8
	li   $v0, 9
	li   $a0, 8
	syscall
	la   $t0, descr_general_String
	sw   $t0, 0($v0)
	la   $t0, string_1
	sw   $t0, 4($v0)
	bnez   $v0, print_preparation_1
	la   $a0, print_failure
	jal  print
	li   $v0, 17
	li   $a0, 2
	syscall
print_preparation_1:
	lw   $a0, 4($v0)
	jal  print
fin_meth_1:
	add  $sp, $sp, 8
	lw   $ra, -4($fp)
	add  $sp, $sp, 4
	lw   $fp, 0($sp)
	jr   $ra
debut_meth_0:
	sw   $fp, 0($sp)
	sub  $fp, $sp, 4
	sw   $ra, -4($fp)
	add  $sp, $fp, -8
	li   $v0, 9
	li   $a0, 8
	syscall
	la   $t0, descr_general_String
	sw   $t0, 0($v0)
	la   $t0, string_0
	sw   $t0, 4($v0)
	bnez   $v0, print_preparation_0
	la   $a0, print_failure
	jal  print
	li   $v0, 17
	li   $a0, 2
	syscall
print_preparation_0:
	lw   $a0, 4($v0)
	jal  print
	li   $v0, 0
	j  fin_meth_0
fin_meth_0:
	add  $sp, $sp, 8
	lw   $ra, -4($fp)
	add  $sp, $sp, 4
	lw   $fp, 0($sp)
	jr   $ra
print:
	li   $v0, 4
	syscall
	jr   $ra
instanceof:
	beqz   $a0, instanceof_true
instanceof_aux:
	lw   $a0, 0($a0)
	beq   $a0, $a1, instanceof_true
	beqz   $a0, instanceof_false
	j  instanceof_aux
instanceof_true:
	li   $v0, 1
	jr   $ra
instanceof_false:
	li   $v0, 0
	jr   $ra
cast:
	move $v0, $a0
	beqz   $a0, cast_true
cast_aux:
	lw   $a0, 0($a0)
	beq   $a0, $a1, cast_true
	beqz   $a0, cast_false
	j  cast_aux
cast_true:
	jr   $ra
cast_false:
	la   $a0, cast_failure
	jal  print
	li   $v0, 17
	li   $a0, 2
	syscall
String_equals:
	lw   $a0, 4($sp)
	lw   $a1, 8($sp)
	beq   $a0, $a1, String_equals_true
	beqz   $a0, String_equals_false
	beqz   $a1, String_equals_false
	lw   $a0, 4($a0)
	lw   $a1, 4($a1)
String_equals_boucle:
	bne   $a0, $a1, String_equals_false
	beqz   $a0, String_equals_true
	add  $a0, $a0, 4
	add  $a1, $a1, 4
	j  String_equals_boucle
String_equals_false:
	li   $v0, 0
	jr   $ra
String_equals_true:
	li   $v0, 1
	jr   $ra
throw_division_by_zero_failure:
	la   $a0, division_by_zero_failure
	jal  print
	li   $a0, 2
	li   $v0, 17
	syscall
String_concat:
	li   $t0, 1
	lw   $a0, 4($a0)
	lw   $a1, 4($a1)
	sw   $a0, 0($sp)
	sw   $a1, -4($sp)
String_concat_boucle1:
	lb   $t1, 0($a0)
	beqz   $t1, String_concat_boucle2
	add  $t0, $t0, 1
	add  $a0, $a0, 1
	j  String_concat_boucle1
String_concat_boucle2:
	lb   $t1, 0($a1)
	beqz   $t1, String_concat_allocs
	add  $t0, $t0, 1
	add  $a1, $a1, 1
	j  String_concat_boucle2
String_concat_allocs:
	li   $v0, 9
	move $a0, $t0
	syscall
	move $t0, $v0
	lw   $a0, 0($sp)
	lw   $a1, -4($sp)
String_concat_boucle3:
	lb   $t1, 0($a0)
	beqz   $t1, String_concat_avantboucle4
	sb   $t1, 0($t0)
	add  $t0, $t0, 1
	add  $a0, $a0, 1
	j  String_concat_boucle3
String_concat_avantboucle4:
	lb   $t1, 0($a1)
	sb   $t1, 0($t0)
String_concat_boucle4:
	beqz   $t1, String_concat_allocs2
	add  $t0, $t0, 1
	add  $a1, $a1, 1
	lb   $t1, 0($a1)
	sb   $t1, 0($t0)
	j  String_concat_boucle4
String_concat_allocs2:
	move $t0, $v0
	li   $v0, 9
	li   $a0, 8
	syscall
	sw   $t0, 4($v0)
	la   $t0, descr_general_String
	sw   $t0, 0($v0)
	jr   $ra
	.data
string_1:
	.asciiz "Creation de a\n"
string_0:
	.asciiz "Renvoit 0\n"
cast_failure:
	.asciiz "Cast failure"
print_failure:
	.asciiz "Erreur : la chaine passee en argument est un pointeur egal a NULL"
division_by_zero_failure:
	.asciiz "Division by zero failure"
null:
	.word 0
descr_general_B:
	.word 0
descr_meth_B:
	.word debut_meth_0
descr_general_A:
	.word 0
	.word debut_meth_1
descr_meth_A:
descr_general_String:
	.word 0
descr_meth_String:
	.word String_equals
