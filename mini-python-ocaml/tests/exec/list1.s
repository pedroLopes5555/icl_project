	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	andq $-8, %rsp
	movq $333, %rdi
	call P_alloc_int
	movq %rax, %rdi
	pushq %rdi
	movq $222, %rdi
	call P_alloc_int
	movq %rax, %rdi
	pushq %rdi
	movq $111, %rdi
	call P_alloc_int
	movq %rax, %rdi
	pushq %rdi
	movq $3, %rdi
	call P_alloc_list
	popq %rdi
	movq %rdi, 16(%rax)
	popq %rdi
	movq %rdi, 24(%rax)
	popq %rdi
	movq %rdi, 32(%rax)
	movq %rax, %rdi
	movq %rdi, -8(%rbp)
	movq -8(%rbp), %rdi
	movq %rdi, %rbx
	movq $1, %rdi
	call P_alloc_int
	movq %rax, %rdi
	movq %rdi, %rcx
	movq 8(%rbx), %rax
	shlq $3, %rcx
	addq %rcx, %rax
	movq 8(%rax), %rdi
	call P_alloc_int
	movq %rax, %rdi
	call P_print
	call P_print_newline
	xorq %rax, %rax
	movq %rbp, %rsp
	popq %rbp
	ret

P_test: # argument in %rdi
      pushq   %rbp
      movq    %rsp, %rbp
      movq    (%rdi), %rax
      cmpq    $0, %rax
      je      E_test
      movq    8(%rdi), %rax
E_test:
      movq    %rbp, %rsp
      popq    %rbp
      ret
P_print_list:
    pushq   %rbp
    movq    %rsp, %rbp
    pushq   %r12           # save callee-saved registers
    pushq   %r13
    pushq   %r14

    movq    %rdi, %r12     # %r12 = list base address

    movq    8(%r12), %r13  # %r13 = length of list
    xorq    %r14, %r14     # %r14 = loop index (i = 0)

.loop_start:
    cmpq    %r14, %r13     # if i >= length, exit loop
    jge     .loop_end

    movq    %r14, %rdi
    shl     $3, %rdi       # offset = i * 8
    addq    $16, %rdi      # offset = 16 + i * 8
    addq    %r12, %rdi     # address of list[i]
    movq    (%rdi), %rdi   # dereference to get actual element pointer

    call    P_print        # print element

    # Optional: print a space (or comma) after each element
    # unless it’s the last one
    incq    %r14
    cmpq    %r14, %r13
    je      .no_separator

    movq    $S_space, %rdi
    xorq    %rax, %rax
    call    printf

.no_separator:
    jmp     .loop_start

.loop_end:
    popq    %r14
    popq    %r13
    popq    %r12
    movq    %rbp, %rsp
    popq    %rbp
    ret

P_print_None:
      pushq   %rbp
      movq    %rsp, %rbp
      andq    $-16, %rsp
      movq    $S_message_None, %rdi
      xorq    %rax, %rax
      call    printf
      movq    %rbp, %rsp
      popq    %rbp
      ret
P_print_string:
      pushq   %rbp
      movq    %rsp, %rbp
      andq    $-16, %rsp
      movq    %rdi, %rsi
      movq    $S_message_string, %rdi
      xorq    %rax, %rax
      call    printf
      movq    %rbp, %rsp
      popq    %rbp
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
P_print_Bool:
      pushq   %rbp
      movq    %rsp, %rbp
      andq    $-16, %rsp       # align stack
      movq    8(%rdi), %rax    # load the boolean value
      cmpq    $0, %rax
      jne     1f               # if not zero, it's True
      movq    $S_message_False, %rdi
      jmp     2f
1:
      movq    $S_message_True, %rdi
2:
      xorq    %rax, %rax       # clear %rax as required by printf
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
      cmpq    $0, (%rdi) # is this None?
      je      0f
      cmpq    $1, (%rdi) # is this bool?
      je      1f
      cmpq    $2, (%rdi) # is this integer?
      je      2f
      cmpq    $3, (%rdi) # is this String?
      je      3f
      cmpq    $4, (%rdi) # is this List?
      je      4f
0:
      call    P_print_None
      jmp     E_print
1:
      call    P_print_Bool
      jmp     E_print
3:
      leaq    16(%rdi), %rdi
      call    P_print_string
      jmp     E_print
2:
      movq    8(%rdi), %rdi
      call    P_print_int
4:
      call    P_print_list
E_print:
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

P_alloc_bool:
      pushq   %rbp
      movq    %rsp, %rbp
      pushq   %rdi        # the boolean value is in %rdi
      andq    $-16, %rsp  # stack alignment;
      movq    $16, %rdi   # how many bytes you want to allocate;
      call    malloc      # the new allocated address is in %rax,
                          # which is a 16 bytes = 2 * 8 = 2 * 64 bits
                          # segment;
      movq    $1, (%rax)  # put the tag of a boolean block
                          # in the address pointed by %rax;
      movq    -8(%rbp), %rdi # get back the value of n, which is now on
                             # the stack, at address %rbp - 8 bytes;
      movq    %rdi, 8(%rax)  # put the value of n at address
                              # %rax + 8 bytes; 
      #### Now, we have the following, contiguous block allocated:
      ####    +---------+---------+ 
      ####    |    1    |    n    |
      ####    +---------+---------+
      ####    | 8 bytes | 8 bytes |
      ####    +---------+---------+
      ####
      movq    %rbp, %rsp
      popq    %rbp
      ret                 # the result is in %rax

P_alloc_list:
      pushq   %rbp
      movq    %rsp, %rbp
      pushq   %rdi        # the length of the list
      andq    $-16, %rsp  # stack alignment;
      shl     $3, %rdi    # 8 * %rdi
      addq    $16, %rdi   # %rdi = 16 + 8 * length of the list
      call    malloc
      movq    $4, (%rax)
      movq    -8(%rbp), %rdi
      movq    %rdi, 8(%rax)
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


# NEW THINGS --------------------------------------------------

P_get_iter:
    pushq   %rbp
    movq    %rsp, %rbp

    movq    $16, %rdi          # size of iterator struct = 2 * 8 bytes
    call    malloc             # allocate 16 bytes, result in %rax

    movq    %rdi, -8(%rbp)     # save original list pointer (%rdi) on stack
    movq    %rax, %rdi         # %rdi = allocated iterator

    movq    -8(%rbp), %rax     # reload list pointer into %rax
    movq    %rax, 0(%rdi)      # store list pointer into iterator[0]

    movq    $0, 8(%rdi)        # initialize index to 0

    movq    %rdi, %rax         # return iterator pointer in %rax

    movq    %rbp, %rsp
    popq    %rbp
    ret

P_iter_next:
    pushq   %rbp
    movq    %rsp, %rbp

    # load iterator fields
    movq    0(%rdi), %rax      # %rax = list pointer
    movq    8(%rdi), %rcx      # %rcx = current index

    # load length of list
    movq    8(%rax), %rdx      # %rdx = length

    cmpq    %rcx, %rdx         # compare index vs length
    jge     .no_more_elements

    # calculate element address: list + 16 + 8*index
    leaq    16(%rax), %rsi
    movq    %rcx, %r8
    shl     $3, %r8            # index * 8
    addq    %r8, %rsi          # pointer to element[index]

    movq    (%rsi), %rax       # load element pointer

    # increment iterator index
    incq    8(%rdi)

    movq    %rbp, %rsp
    popq    %rbp
    ret

.no_more_elements:
    # free iterator structure, since done iterating
    movq    %rdi, %rdi
    call    free

    movq    $0, %rax           # return NULL (0)
    movq    %rbp, %rsp
    popq    %rbp
    ret


P_set_item:
    pushq   %rbp
    movq    %rsp, %rbp

    # unbox int key: key value stored at 8(%rsi)
    movq    8(%rsi), %rcx

    # calculate address of list element: container + 16 + 8*key
    leaq    16(%rdi), %r8
    movq    %rcx, %r9
    shl     $3, %r9
    addq    %r9, %r8

    # store value pointer into element[key]
    movq    %rdx, (%r8)

    movq    %rbp, %rsp
    popq    %rbp
    ret

# NEW THINGS --------------------------------------------------

	.data

S_message_int:
.string    "%d"
S_message_string:
  .string    "%s"
S_message_None:
  .string    "None"
S_newline:
  .string    "\n"
S_message_False:
  .string    "False"
S_message_True:
  .string    "True"
S_space:
    .string " "

C_None:
  .quad       0
C_False:
  .quad       1
  .quad       0
C_True:
  .quad       1
  .quad       1
