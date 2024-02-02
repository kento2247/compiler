IO:
	.string "%lld"
	.text
	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp
	movq $80, %rdi
	callq malloc
	pushq %rax
	movq %rbp, %rax
	leaq -8(%rax), %rax
	popq (%rax)
	pushq $3
	pushq %rbp
	callq func
	addq $16, %rsp
	pushq %rax
	movq %rbp, %rax
	leaq -16(%rax), %rax
	popq (%rax)
	movq %rbp, %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq  %rsi
	leaq IO(%rip), %rdi
	movq $0, %rax
	callq printf
	.data
L1:	.string "\n"
	.text
	leaq L1(%rip), %rdi
	movq $0, %rax
	callq printf
	leaveq
	retq
func:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %rbp, %rax
	leaq 24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $2
	popq %rax
	imulq (%rsp), %rax
	movq %rax, (%rsp)
	popq %rax
	leaveq
	retq
