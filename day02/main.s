.section .data
	filename:	.asciz "input"
	lines:		.quad 2500	# Length of the file hardcoded
	#filename:	.asciz "test-input"
	#lines:		.quad 9		# Length of the file hardcoded
	total:		.quad 0
	int_template:	.asciz "%d\n"

.section .bss
	file: .skip 8
	line: .skip 4

.section .text
.global main
main:
	# Prologue
	pushq	%rbp
	movq	%rsp, %rbp
	# Callee-saved
	pushq	%rbx

	movq	$0644, %rdx	# Argument #3: mode
	movq	$0, %rsi	# Argument #2: read
	movq	$filename, %rdi	# Argument #1: filename
	movq	$2, %rax	# sys_open
	syscall

	movq	%rax, file	# Make file the file descriptor

	movq	lines, %rbx
	read_loop:
		movq	$4, %rdx	# Argument #3: buffer size
		movq	$line, %rsi	# Argument #2: data to read to
		movq	file, %rdi	# Argument #1: file descriptor
		movq	$0, %rax	# sys_read
		syscall

		movq	$line, %rdi
		call	solve

		decq	%rbx
	jnz	read_loop

	movq	total, %rsi
	movq	$int_template, %rdi
	movq	$0, %rax
	call	printf

	movq	file, %rdi
	movq	$3, %rax	# sys_close
	syscall

exit:
	movq	$0, %rdi	# Argument #1: exit code
	movq	$60, %rax	# sys_exit
	syscall

	# Epilogue
	# Callee-saved
	popq	%rbx

	movq	%rbp, %rsp
	popq	%rbp

solve:
	# Prologue
	pushq	%rbp
	movq	%rsp, %rbp

	pushq	%r12
	pushq	%r13

	movzbq	0(%rdi), %r12	# R12B is the opponent's play
	movzbq	2(%rdi), %r13	# R13B is your play

	subb	$'A, %r12b
	subb	$'X, %r13b
	incb	%r12b
	incb	%r13b

	#---------------#
	# 1 - rock      #
	# 2 - paper     #
	# 3 - scissors  #
	#---------------#

	addq	%r13, total
	
	cmpb	%r13b, %r12b		# If you play the same, it's a draw
	je	draw

	decb	%r13b
	cmpb	%r13b, %r12b		# If you play one higher, you win
	je	win
	incb	%r13b

	movq	$0, %rax
	cmpb	$1, %r13b		# If you play a rock
	je	played_rock
	jmp	not_played_rock

	played_rock:
		cmpb	$3, %r12b	# And the opponent plays scissors
		je	win		# Then you also win
	not_played_rock:
	jmp	lose			# Otherwise you lose
	
	win:
		incb	%r13b
		addq	$6, total
		jmp	end_win_lose_draw
	lose:
		jmp	end_win_lose_draw
	draw:
		addq	$3, total
		jmp	end_win_lose_draw
	end_win_lose_draw:
	
/*
	subq	$8, %rsp
	movb	%r12b, (%rsp)
	addb	$'0, (%rsp)
	movq	$1, %rdx	# Argument #3: buffer size
	movq	%rsp, %rsi	# Argument #2: data to write
	movq	$1, %rdi	# Argument #1: file descriptor
	movq	$1, %rax	# sys_write
	syscall
	addq	$8, %rsp

	subq	$8, %rsp
	movb	%r13b, (%rsp)
	addb	$'0, (%rsp)
	movq	$1, %rdx	# Argument #3: buffer size
	movq	%rsp, %rsi	# Argument #2: data to write
	movq	$1, %rdi	# Argument #1: file descriptor
	movq	$1, %rax	# sys_write
	syscall
	addq	$8, %rsp

	# Print newlines
	subq	$8, %rsp
	movb	$'\n, (%rsp)
	movq	$1, %rdx	# Argument #3: buffer size
	movq	%rsp, %rsi	# Argument #2: data to write
	movq	$1, %rdi	# Argument #1: file descriptor
	movq	$1, %rax	# sys_write
	syscall
	addq	$8, %rsp
*/

	popq	%r13
	popq	%r12

	# Epilogue
	movq	%rbp, %rsp
	popq	%rbp

	retq
