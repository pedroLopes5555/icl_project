	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	call F_foo
	addq $0, %rsp
	movq %rax, %rdi
	call P_print
	call P_print_newline
	call F_foo
	addq $0, %rsp
	movq %rax, %rdi
	movq %rdi, %rbx
	call F_foo
	addq $0, %rsp
	movq %rax, %rdi
	movq %rdi, %rcx
	movq 8(%rbx), %rax
	cmpq 8(%rcx), %rax
	sete %al
	movzbq %al, %rdi
	call P_alloc_int
	movq %rax, %rdi
	call P_print
	call P_print_newline
	call F_foo
	addq $0, %rsp
	movq %rax, %rdi
	movq %rdi, %rbx
	call F_foo
	addq $0, %rsp
	movq %rax, %rdi
	movq %rdi, %rcx
	movq 8(%rbx), %rax
	cmpq 8(%rcx), %rax
	setne %al
	movzbq %al, %rdi
	call P_alloc_int
	movq %rax, %rdi
	call P_print
	call P_print_newline
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret
F_foo:
	pushq %rbp
	movq %rsp, %rbp
	andq $-8, %rsp
	movq $1, %rdi
	call P_alloc_int
	movq %rax, %rdi
	movq %rdi, -8(%rbp)
L_1:
	movq %rbp, %rsp
	popq %rbp
	ret

P_print_int:
      pushq   %rbp
      movq    %rsp, %rbp
      andq    $-16, %rsp
      movq    %rdi, %rsi
      movq    $S_message_int, %rdi
      xorq    %rax, %rax
      call    printf
      movq    %rbp, %rsp
      popq    %rbp
      ret
P_print:
      # arg to print is in %rdi;
      # for now, let us assume it is always an integer
      # and let us ignore the tag
      pushq   %rbp
      movq    %rsp, %rbp
      movq    8(%rdi), %rdi
      call    P_print_int
      movq    %rbp, %rsp
      popq    %rbp
      ret
P_alloc_int:
      pushq   %rbp
      movq    %rsp, %rbp
      pushq   %rdi        # the integer n to allocate is stored in %rdi
                          # and now we push it into the stack;
      andq    $-16, %rsp  # stack alignment;
      movq    $16, %rdi   # how many bytes you want to allocate;
      call    malloc      # the new allocated address is in %rax,
                          # which is a 16 bytes = 2 * 8 = 2 * 64 bits
                          # segment;
      movq    $2, (%rax)  # put the tag of an integer block
                          # in the address pointed by %rax;
      movq    -8(%rbp), %rdi # get back the value of n, which is now on
                             # the stack, at address %rbp - 8 bytes;
      movq    %rdi, 8(%rax)  # put the value of n at address
                             # %rax + 8 bytes;
      #### Now, we have the following, contiguous block allocated:
      ####
      ####        +---------+---------+
      ####        |    2    |    n    |
      ####        +---------+---------+
      ####        | 8 bytes | 8 bytes |
      ####        +---------+---------+
      ####
      movq    %rbp, %rsp
      popq    %rbp
      ret                 # the result is in %rax
P_print_newline:
      pushq   %rbp
      movq    %rsp, %rbp
      andq    $-16, %rsp
      movq    $S_newline, %rdi
      xorq    %rax, %rax
      call    printf
      movq    %rbp, %rsp
      popq    %rbp
      ret
	.data

S_message_int:
  .string    "%d"
S_newline:
  .string    "\n"
S_StringNone:
.string    "None"
C_None:
  .quad   0
