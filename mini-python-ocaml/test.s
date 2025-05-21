	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $1, %rdi
	call P_alloc_int
	movq %rax, %rdi
	movq %rdi, %rbx
	movq $2, %rdi
	call P_alloc_int
	movq %rax, %rdi
	movq %rdi, %rcx
	movq 8(%rbx), %rax
	cmpq 8(%rcx), %rax
	setl %al
	movzbq %al, %rdi
	call P_alloc_bool
	movq %rax, %rdi
	call P_print
	call P_print_newline
	xorq %rax, %rax
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

P_print_bool:
      pushq   %rbp
      movq    %rsp, %rbp
      andq    $-16, %rsp
      movq    %rdi, %rsi
      cmpq    $0, 8(%rdi)
      je      P_print_false
      movq    $S_StringTrue, %rdi
      jmp     P_print_end
P_print_false:
      movq    $S_StringFalse, %rdi
      jmp     P_print_end
P_print_end:
      xorq    %rax, %rax
      call    printf
      movq    %rbp, %rsp
      popq    %rbp
      ret
P_print:
      pushq   %rbp
      movq    %rsp, %rbp

      movq    (%rdi), %rax         # Load the tag from the first 8 bytes of the boxed value into %rax

      cmpq    $1, %rax             # Check if tag == 1 (boolean)
      call P_print_bool
      jmp P_print_end


      cmpq    $2, %rax             # Check if tag == 2 (integer)
      call    P_print_int_dispatch
      jmp     P_print_end
      # TODO: Add other types here (e.g., string, list)

      # Fallback or unknown type
      jmp     P_print_end          # No-op or could add error printing

P_print_int_dispatch:
      movq    8(%rdi), %rdi        # Unbox integer into %rdi
      call    P_print_int
      jmp     P_print_end


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
P_alloc_bool:
      pushq   %rbp
      movq    %rsp, %rbp
      pushq   %rdi              # push boolean value (0 or 1)
      andq    $-16, %rsp        # align stack
      movq    $16, %rdi         # request 16 bytes
      call    malloc
      movq    $1, (%rax)        # store tag for bool at offset 0
      movq    -8(%rbp), %rdi    # recover original boolean value
      movq    %rdi, 8(%rax)     # store boolean value at offset 8
      movq    %rbp, %rsp
      popq    %rbp
      ret

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
S_StringTrue:
.string    "True"
S_StringFalse:
.string    "False"
C_None:
  .quad   0
C_False:
  .quad   1
  .quad   0
C_True:
  .quad   1
  .quad   1

