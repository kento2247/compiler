IO:
	.string "%lld"
	.text
	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	pushq $5
	movq %rbp, %rax
	leaq -16(%rax), %rax
	popq (%rax)
	pushq $3
	movq %rbp, %rax
	leaq -24(%rax), %rax
	popq (%rax)
	pushq $2
	movq %rbp, %rax
	leaq -32(%rax), %rax
	popq (%rax)
	movq %rbp, %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	movq %rbp, %rax
	leaq -24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rbx
	popq %rax
	cqto
	idivq %rbx
	pushq %rdx
	movq %rbp, %rax
	leaq -40(%rax), %rax
	popq (%rax)
	movq %rbp, %rax
	leaq -40(%rax), %rax
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
	movq %rbp, %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	movq %rbp, %rax
	leaq -32(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rbx
	popq %rax
	movq $1, %rdx
factorial_loop0:
	cmpq $0, %rbx
	je factorial_end0
	imulq %rax, %rdx
	decq %rbx
	jmp factorial_loop0
factorial_end0:
	pushq %rdx
	movq %rbp, %rax
	leaq -40(%rax), %rax
	popq (%rax)
	movq %rbp, %rax
	leaq -40(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq  %rsi
	leaq IO(%rip), %rdi
	movq $0, %rax
	callq printf
	.data
L2:	.string "\n"
	.text
	leaq L2(%rip), %rdi
	movq $0, %rax
	callq printf
	movq %rbp, %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq  %rsi
	leaq IO(%rip), %rdi
	movq $0, %rax
	callq printf
	.data
L3:	.string "\n"
	.text
	leaq L3(%rip), %rdi
	movq $0, %rax
	callq printf
	movq %rbp, %rax
	leaq -16(%rax), %rax
	movq (%rax), %rbx
	addq $1, (%rax)
	pushq %rbx
	movq %rbp, %rax
	leaq -40(%rax), %rax
	popq (%rax)
	movq %rbp, %rax
	leaq -40(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq  %rsi
	leaq IO(%rip), %rdi
	movq $0, %rax
	callq printf
	.data
L4:	.string "\n"
	.text
	leaq L4(%rip), %rdi
	movq $0, %rax
	callq printf
	movq %rbp, %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq  %rsi
	leaq IO(%rip), %rdi
	movq $0, %rax
	callq printf
	.data
L5:	.string "\n"
	.text
	leaq L5(%rip), %rdi
	movq $0, %rax
	callq printf
	movq %rbp, %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	movq %rbp, %rax
	leaq -40(%rax), %rax
	popq %rbx
	addq %rbx, (%rax)
	movq %rbp, %rax
	leaq -40(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq  %rsi
	leaq IO(%rip), %rdi
	movq $0, %rax
	callq printf
	.data
L6:	.string "\n"
	.text
	leaq L6(%rip), %rdi
	movq $0, %rax
	callq printf
	movq %rbp, %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq  %rsi
	leaq IO(%rip), %rdi
	movq $0, %rax
	callq printf
	.data
L7:	.string "\n"
	.text
	leaq L7(%rip), %rdi
	movq $0, %rax
	callq printf
	pushq $1
	movq %rbp, %rax
	leaq -40(%rax), %rax
	popq (%rax)
L9:
	subq $16, %rsp
	movq %rbp, %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	movq %rbp, %rax
	leaq -40(%rax), %rax
	popq %rbx
	addq %rbx, (%rax)
	movq %rbp, %rax
	leaq -40(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $10
	popq %rax
	popq %rbx
	cmpq %rax, %rbx
	jge L8
	jmp L9
L8:
	movq %rbp, %rax
	leaq -40(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq  %rsi
	leaq IO(%rip), %rdi
	movq $0, %rax
	callq printf
	.data
L10:	.string "\n"
	.text
	leaq L10(%rip), %rdi
	movq $0, %rax
	callq printf
	pushq $1
	movq %rbp, %rax
	leaq -40(%rax), %rax
	popq (%rax)
	pushq $0
	movq %rbp, %rax
	leaq -8(%rax), %rax
	popq (%rax)
L11:
	pushq $5
	popq %rbx
	movq %rbp, %rax
	leaq -8(%rax), %rax
	cmpq (%rax), %rbx
	je L12
	movq %rbp, %rax
	leaq -40(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $3
	popq %rax
	imulq (%rsp), %rax
	movq %rax, (%rsp)
	movq %rbp, %rax
	leaq -40(%rax), %rax
	popq (%rax)
	movq %rbp, %rax
	leaq -8(%rax), %rax
	addq $1, (%rax)
	jmp L11
L12:
	movq %rbp, %rax
	leaq -40(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq  %rsi
	leaq IO(%rip), %rdi
	movq $0, %rax
	callq printf
	.data
L13:	.string "\n"
	.text
	leaq L13(%rip), %rdi
	movq $0, %rax
	callq printf
	leaveq
	retq
