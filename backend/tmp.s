IO:
	.string "%lld"
	.text
	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp
	pushq $1
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
	pushq $5
	pushq $2
	popq %rbx
	popq %rax
	cqto
	idivq %rbx
	pushq %rdx
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
L2:	.string "\n"
	.text
	leaq L2(%rip), %rdi
	movq $0, %rax
	callq printf
	pushq $5
	pushq $2
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
L3:	.string "\n"
	.text
	leaq L3(%rip), %rdi
	movq $0, %rax
	callq printf
	pushq $2
	movq %rbp, %rax
	leaq -16(%rax), %rax
	popq %rbx
	addq %rbx, (%rax)
	movq %rbp, %rax
	leaq -16(%rax), %rax
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
	movq (%rax), %rbx
	addq $1, (%rax)
	pushq %rbx
	movq %rbp, %rax
	leaq -16(%rax), %rax
	movq (%rax), %rbx
	addq $1, (%rax)
	pushq %rbx
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
	pushq $0
	movq %rbp, %rax
	leaq -16(%rax), %rax
	popq (%rax)
L7:
	subq $16, %rsp
	movq %rbp, %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $1
	popq %rax
	addq %rax, (%rsp)
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
L8:	.string "\n"
	.text
	leaq L8(%rip), %rdi
	movq $0, %rax
	callq printf
	movq %rbp, %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $5
	popq %rax
	popq %rbx
	cmpq %rax, %rbx
	jge L6
	jmp L7
L6:
	pushq $0
	movq %rbp, %rax
	leaq -8(%rax), %rax
	popq (%rax)
L9:
	pushq $5
	popq %rbx
	movq %rbp, %rax
	leaq -8(%rax), %rax
	cmpq (%rax), %rbx
	je L10
	subq $16, %rsp
	movq %rbp, %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq  %rsi
	leaq IO(%rip), %rdi
	movq $0, %rax
	callq printf
	.data
L11:	.string "\n"
	.text
	leaq L11(%rip), %rdi
	movq $0, %rax
	callq printf
	movq %rbp, %rax
	leaq -8(%rax), %rax
	addq $1, (%rax)
	jmp L9
L10:
	leaveq
	retq
