open Format
open X86_64
open Ast

let debug = ref false
(*
none:
  []

bool
  [1][n]
  1 -> tag
  n -> value

int:
  [2][n]
  2 -> tag

string:
  [3][l][][][][][]
        ---------
            |
            V
       string value

  3 -> tag
  l -> lengh

list:
  [4][n][e1][e2][e3][e4][e5]
  4 -> tag
  n -> length
  ek -> pointer to the k-th element

*)
let preamble = "
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
"


let data_inline = "
S_message_int:
.string    \"%d\"
S_message_string:
  .string    \"%s\"
S_message_None:
  .string    \"None\"
S_newline:
  .string    \"\\n\"
S_message_False:
  .string    \"False\"
S_message_True:
  .string    \"True\"
S_space:
    .string \" \"

C_None:
  .quad       0
C_False:
  .quad       1
  .quad       0
C_True:
  .quad       1
  .quad       1
"




let leave =
  movq (reg rbp) (reg rsp) ++ popq rbp ++ ret

let enter =
  pushq (reg rbp) ++ movq (reg rsp) (reg rbp)


let new_label = 
  let c = ref 0 in
  (*print the lables to the console*)
  let rec incr c = 
    if !debug then printf "Label %d\n" !c;
    c := !c + 1 in

  fun () -> incr c; "L_" ^ (string_of_int !c)



type env_alloc = {
  mutable nb_local: int;
  mutable nb_total: int;
}

let empty_alloc_env () ={ 
  nb_local = 0;
  nb_total = 0 
}

let alloc_var env v =
  if v.v_ofs = -1 then begin
    env.nb_local <- env.nb_local + 1;
    v.v_ofs <- -8 * env.nb_local;
    env.nb_total <- env.nb_total + 1;
  end


let rec alloc_vars env (s: Ast.tstmt) = 
  match s with
  | TSif (_ , s1, s2) -> alloc_vars env s1; alloc_vars env s2
  | TSreturn _ -> ()
  | TSassign (x, _) -> alloc_var env x
  | TSprint _ -> ()
  | TSblock sl -> List.iter (alloc_vars env) sl
  | TSfor (x, _, s) -> alloc_var env x; alloc_vars env s
  | TSeval _ -> ()
  | TSset (_, _, _) -> ()




type string_env = (string, string) Hashtbl.t

let string_env : string_env = Hashtbl.create 16

let new_string =
  let c = ref 0 in
  fun s -> incr c;
    let l = "S_" ^ (string_of_int !c) in
    Hashtbl.add string_env l s;
    l


let rec fold_i f i acc l =
  match l with
  | [] -> acc
  | x :: r -> fold_i f (i + 1) (f i acc x) r



let rec compile_expr (e: Ast.texpr) =
  match e with
  | TEcst (Cint n) ->
      movq (imm64 n) (reg rdi) ++ call "P_alloc_int" ++
      movq (reg rax) (reg rdi)
  | TEcst Cnone -> 
    movq (ilab "C_None") (reg rdi)
  | TEcst Cbool b -> 
    movq (ilab (if b then "C_True" else "C_False")) (reg rdi) 
  | TEvar x -> 
      movq (ind ~ofs:x.v_ofs rbp) (reg rdi)
  | TEcst (Cstring s) ->
    let l = new_string s in
    movq (ilab l) (reg rdi)
  
  | TEbinop (binop, e1, e2) -> 
    begin match binop with
    | Badd ->
        compile_expr e1 ++               (* result in %rdi *)
        movq (reg rdi) (reg rbx) ++      (* save boxed e1 in %rbx *)
        compile_expr e2 ++               (* result in %rdi *)
        movq (reg rdi) (reg rcx) ++      (* save boxed e2 in %rcx *)

        movq (ind ~ofs:8 rbx) (reg rax) ++   (* unbox e1: value -> %rax *)
        addq (ind ~ofs:8 rcx) (reg rax) ++   (* add unboxed e2 *)
        movq (reg rax) (reg rdi) ++
        call "P_alloc_int" ++
        movq (reg rax) (reg rdi)

    | Bsub ->
        compile_expr e1 ++               (* result in %rdi *)
        movq (reg rdi) (reg rbx) ++      (* save boxed e1 in %rbx *)
        compile_expr e2 ++               (* result in %rdi *)
        movq (reg rdi) (reg rcx) ++      (* save boxed e2 in %rcx *)

        movq (ind ~ofs:8 rbx) (reg rax) ++   (* unbox e1: value -> %rax *)
        subq (ind ~ofs:8 rcx) (reg rax) ++   (* add unboxed e2 *)
        movq (reg rax) (reg rdi) ++
        call "P_alloc_int" ++
        movq (reg rax) (reg rdi)

    | Bmul ->
        compile_expr e1 ++               (* result in %rdi *)
        movq (reg rdi) (reg rbx) ++      (* save boxed e1 in %rbx *)
        compile_expr e2 ++               (* result in %rdi *)
        movq (reg rdi) (reg rcx) ++      (* save boxed e2 in %rcx *)

        movq (ind ~ofs:8 rbx) (reg rax) ++   (* unbox e1: value -> %rax *)
        imulq (ind ~ofs:8 rcx) (reg rax) ++   (* add unboxed e2 *)
        movq (reg rax) (reg rdi) ++
        call "P_alloc_int" ++
        movq (reg rax) (reg rdi)


    | Bdiv ->
        compile_expr e1 ++
        movq (reg rdi) (reg rbx) ++         (* boxed e1 -> %rbx *)
        compile_expr e2 ++
        movq (reg rdi) (reg rcx) ++         (* boxed e2 -> %rcx *)

        movq (ind ~ofs:8 rbx) (reg rax) ++  (* unbox e1 (dividend) -> %rax *)
        cqto ++                             (* sign-extend %rax into %rdx *)
        idivq (ind ~ofs:8 rcx) ++           (* divide by unboxed e2 (divisor) *)

        movq (reg rax) (reg rdi) ++         (* result (quotient) -> %rdi *)
        call "P_alloc_int" ++
        movq (reg rax) (reg rdi)


    | Bmod ->
        compile_expr e1 ++
        movq (reg rdi) (reg rbx) ++
        compile_expr e2 ++
        movq (reg rdi) (reg rcx) ++

        movq (ind ~ofs:8 rbx) (reg rax) ++
        cqto ++
        idivq (ind ~ofs:8 rcx) ++           (* %rdx now contains the remainder *)

        movq (reg rdx) (reg rdi) ++
        call "P_alloc_int" ++
        movq (reg rax) (reg rdi)


    | Beq ->
        compile_expr e1 ++            (* compute e1, boxed pointer in %rdi *)
        movq (reg rdi) (reg rbx) ++   (* save e1 boxed pointer in %rbx *)
        compile_expr e2 ++            (* compute e2, boxed pointer in %rdi *)
        movq (reg rdi) (reg rcx) ++   (* save e2 boxed pointer in %rcx *)

        movq (ind ~ofs:8 rbx) (reg rax) ++  (* load unboxed e1 value into %rax *)
        cmpq (ind ~ofs:8 rcx) (reg rax) ++  (* compare unboxed e2 to %rax *)
        sete (reg al) ++
        movzbq (reg al) rdi ++
        call "P_alloc_bool" ++           (* box the 0/1 integer *)
        movq (reg rax) (reg rdi)


    | Bneq ->
        compile_expr e1 ++            (* compute e1, boxed pointer in %rdi *)
        movq (reg rdi) (reg rbx) ++   (* save e1 boxed pointer in %rbx *)
        compile_expr e2 ++            (* compute e2, boxed pointer in %rdi *)
        movq (reg rdi) (reg rcx) ++   (* save e2 boxed pointer in %rcx *)

        movq (ind ~ofs:8 rbx) (reg rax) ++  (* load unboxed e1 value into %rax *)
        cmpq (ind ~ofs:8 rcx) (reg rax) ++  (* compare unboxed e2 to %rax *)
        setne (reg al) ++
        movzbq (reg al) rdi ++
        call "P_alloc_bool" ++           (* box the 0/1 integer *)
        movq (reg rax) (reg rdi)


    | Blt ->
        compile_expr e1 ++            (* compute e1, boxed pointer in %rdi *)
        movq (reg rdi) (reg rbx) ++   (* save e1 boxed pointer in %rbx *)
        compile_expr e2 ++            (* compute e2, boxed pointer in %rdi *)
        movq (reg rdi) (reg rcx) ++   (* save e2 boxed pointer in %rcx *)

        movq (ind ~ofs:8 rbx) (reg rax) ++  (* load unboxed e1 value into %rax *)
        cmpq (ind ~ofs:8 rcx) (reg rax) ++  (* compare unboxed e2 to %rax *)
        setl (reg al) ++
        movzbq (reg al) rdi ++
        call "P_alloc_bool" ++           (* box the 0/1 integer *)
        movq (reg rax) (reg rdi)

    
    | Ble ->
        compile_expr e1 ++            (* compute e1, boxed pointer in %rdi *)
        movq (reg rdi) (reg rbx) ++   (* save e1 boxed pointer in %rbx *)
        compile_expr e2 ++            (* compute e2, boxed pointer in %rdi *)
        movq (reg rdi) (reg rcx) ++   (* save e2 boxed pointer in %rcx *)

        movq (ind ~ofs:8 rbx) (reg rax) ++  (* load unboxed e1 value into %rax *)
        cmpq (ind ~ofs:8 rcx) (reg rax) ++  (* compare unboxed e2 to %rax *)
        setle (reg al) ++
        movzbq (reg al) rdi ++
        call "P_alloc_bool" ++           (* box the 0/1 integer *)
        movq (reg rax) (reg rdi)


    | Bgt ->
        compile_expr e1 ++            (* compute e1, boxed pointer in %rdi *)
        movq (reg rdi) (reg rbx) ++   (* save e1 boxed pointer in %rbx *)
        compile_expr e2 ++            (* compute e2, boxed pointer in %rdi *)
        movq (reg rdi) (reg rcx) ++   (* save e2 boxed pointer in %rcx *)

        movq (ind ~ofs:8 rbx) (reg rax) ++  (* load unboxed e1 value into %rax *)
        cmpq (ind ~ofs:8 rcx) (reg rax) ++  (* compare unboxed e2 to %rax *)
        setg (reg al) ++
        movzbq (reg al) rdi ++
        call "P_alloc_bool" ++           (* box the 0/1 integer *)
        movq (reg rax) (reg rdi)


    | Bge ->
        compile_expr e1 ++            (* compute e1, boxed pointer in %rdi *)
        movq (reg rdi) (reg rbx) ++   (* save e1 boxed pointer in %rbx *)
        compile_expr e2 ++            (* compute e2, boxed pointer in %rdi *)
        movq (reg rdi) (reg rcx) ++   (* save e2 boxed pointer in %rcx *)

        movq (ind ~ofs:8 rbx) (reg rax) ++  (* load unboxed e1 value into %rax *)
        cmpq (ind ~ofs:8 rcx) (reg rax) ++  (* compare unboxed e2 to %rax *)
        setge (reg al) ++
        movzbq (reg al) rdi ++
        call "P_alloc_bool" ++           (* box the 0/1 integer *)
        movq (reg rax) (reg rdi)


    | Band ->
        let lbl_false = new_label () in
        let lbl_end = new_label () in

        compile_expr e1 ++
        movq (reg rdi) (reg rbx) ++          (* save e1 boxed *)
        movq (ind ~ofs:8 rbx) (reg rax) ++   (* unbox e1 *)
        testq (reg rax) (reg rax) ++
        je lbl_false ++                      (* if e1 == 0 jump to false *)

        compile_expr e2 ++
        movq (reg rdi) (reg rcx) ++          (* save e2 boxed *)
        movq (ind ~ofs:8 rcx) (reg rax) ++   (* unbox e2 *)
        testq (reg rax) (reg rax) ++
        je lbl_false ++                      (* if e2 == 0 jump to false *)

        movq (imm64 1L) (reg rdi) ++
        call "P_alloc_bool" ++
        movq (reg rax) (reg rdi) ++
        jmp lbl_end ++

        label lbl_false ++
        movq (imm64 0L) (reg rdi) ++
        call "P_alloc_bool" ++
        movq (reg rax) (reg rdi) ++

        label lbl_end


    | Bor ->
        let lbl_true = new_label () in
        let lbl_end = new_label () in

        compile_expr e1 ++
        movq (reg rdi) (reg rbx) ++          (* save e1 boxed *)
        movq (ind ~ofs:8 rbx) (reg rax) ++   (* unbox e1 *)
        testq (reg rax) (reg rax) ++
        jne lbl_true ++                      (* if e1 != 0 → true *)

        compile_expr e2 ++
        movq (reg rdi) (reg rcx) ++          (* save e2 boxed *)
        movq (ind ~ofs:8 rcx) (reg rax) ++   (* unbox e2 *)
        testq (reg rax) (reg rax) ++
        jne lbl_true ++                      (* if e2 != 0 → true *)

        movq (imm64 0L) (reg rdi) ++
        call "P_alloc_bool" ++
        movq (reg rax) (reg rdi) ++
        jmp lbl_end ++

        label lbl_true ++
        movq (imm64 1L) (reg rdi) ++
        call "P_alloc_bool" ++
        movq (reg rax) (reg rdi) ++

        label lbl_end

        
    end


  | TEunop (_, _) -> assert false (* TODO *)


  | TEcall (fn, el) -> 
    (*method to alocate*)  
    let push_arg e = 
        compile_expr e ++
        pushq (reg rdi) in
        (*run the list allocating*)
      List.fold_left (fun a e -> push_arg e ++ a) nop el ++
      call("F_" ^ fn.fn_name)  ++
      addq (imm(8 * List.length el)) (reg rsp) ++ 
      (*por cada push o rsp baixa 8 bites, 
      depois do call temos que restablecer o rsp*)
      movq(reg rax) (reg rdi)
  
  | TElist el ->
      let push_e acc e =
        compile_expr e ++ pushq (reg rdi) ++ acc in
      let pop_e i a e =
        let ofs = 16 + i * 8 in
        a ++ popq rdi ++ movq (reg rdi) (ind ~ofs rax) in
      List.fold_left push_e nop el ++
      movq (imm (List.length el)) (reg rdi) ++
      call "P_alloc_list" ++
      fold_i pop_e 0 nop el ++
      movq (reg rax) (reg rdi)


  | TErange _ -> assert false (* TODO *)


  | TEget (e1, e2) ->
      compile_expr e1 ++
      movq (reg rdi) (reg rbx) ++          (* save boxed e1 in %rbx *)
      compile_expr e2 ++
      movq (reg rdi) (reg rcx) ++          (* save boxed e2 in %rcx *)

      movq (ind ~ofs:8 rbx) (reg rax) ++   (* unbox e1: value -> %rax *)
      shlq (imm 3) (reg rcx) ++            (* multiply index by 8 *)
      addq (reg rcx) (reg rax) ++          (* add index to base address *)
      movq (ind ~ofs:8 rax) (reg rdi) ++   (* dereference to get element *)
      call "P_alloc_int" ++                 (* box the result *)
      movq (reg rax) (reg rdi)

let rec compile_stmt exit_lbl (s: Ast.tstmt) =
  match s with
  | TSif (e, s1, s2) ->
      let l_else = new_label () in
      let l_end = new_label () in
      compile_expr e ++
      call "P_test" ++
      testq (reg rax) (reg rax) ++
      jz l_else ++
      compile_stmt exit_lbl s1 ++
      jmp l_end ++
      label l_else ++
      compile_stmt exit_lbl s2 ++
      label l_end
  | TSreturn e -> 
      compile_expr e ++
      movq (reg rdi) (reg rax) ++
      jmp exit_lbl
      (*return the value of e in rax*)
      (*jmp exit_lbl is not necessary, but it is a good practice*)
      (*to have a label to return to, even if it is the end of the function*)
  | TSassign (x, e) -> 
      let ofs = x.v_ofs in (*x.v_ofs <> -1*)
      compile_expr e ++ (*value of e in %rdi*)
      movq (reg rdi) (ind ~ofs rbp)
  | TSprint e ->
      compile_expr e ++ call "P_print" ++ call "P_print_newline"
  | TSblock sl ->
      List.fold_left (fun c s -> c ++ compile_stmt exit_lbl s) nop sl
  | TSfor (_, _, _) -> assert false (* TODO *)
  | TSeval e -> 
    compile_expr e 
  | TSset (_, _, _) -> assert false (* TODO *)

let compile_tdef (fn, tstmt) =
(* allocate all the params in fn.fnparams*)
let ofs = ref 8 in  
let alloc_param v =
    ofs := !ofs + 8;
    v.v_ofs <- !ofs in
    List.iter alloc_param fn.fn_params;
  (* allocate the local variables in the function body*)
  let env = empty_alloc_env () in
  alloc_vars env tstmt;
  let exit_lbl = new_label () in
  let body = compile_stmt exit_lbl tstmt in
  let lbl = label ("F_" ^ fn.fn_name) in
  let locals = 
    if env.nb_total = 0 then nop
    else
      andq (imm(-8 * env.nb_total)) (reg rsp) in

  (*put everything together*)
  lbl ++ 
  enter ++ 
  locals ++
  (*compile the function body*) 
  body ++ 
  label exit_lbl ++ 
  leave



let compile_main (fn, tstmt) =
  let l = new_label () in
  let env = empty_alloc_env () in
  alloc_vars env tstmt;
  let locals = 
    if(env.nb_total = 0) then nop
    else
      andq (imm(-8 * env.nb_total)) (reg rsp) in 
  enter ++ 
  (*modify the rsp according to the total number 
  of variables i want to allocate*)
  locals ++
  (*compile the function body*)
  compile_stmt l tstmt ++ 
  xorq (reg rax) (reg rax)


let inline_strings () =
  let alloc_string l s acc =
    label l ++ dquad [3; String.length s] ++ string s ++ acc in
  Hashtbl.fold alloc_string string_env nop



let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  match p with
  | [] -> assert false
  | m :: r ->
    let cmain = compile_main m ++ leave in
    let cfile =
      List.fold_left (fun a td -> a ++ compile_tdef td) nop r in
    { text = globl "main" ++ label "main" ++ cmain ++
             cfile ++ inline preamble;
      data = inline data_inline ++ inline_strings (); }