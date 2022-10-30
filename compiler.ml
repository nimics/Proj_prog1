
(*

transformer AST en code assembleur 

*)

open Asyntax
open X86_64

(* opérations push et pop sur les floats *)
let pushf reg = movsd !%reg (ind ~ofs:(-8) rsp) ++ subq (imm 8) !%rsp
let popf reg = movsd (ind rsp) !%reg ++ addq (imm 8) !%rsp

let compile_main abr =
  let d = ref nop in
  let i = ref 0 in (* compte les floattants *)
  let need_exp = ref 0 in
  let rec compile abr =
    match abr with
    (* opérations des entiers *)
    | F_Int(k) ->
        movq (imm k) !%rdi
    | Plus(e) ->
        compile e
    | Minus(e) ->
        compile e ++ negq !%rdi
    | Add(e1, e2) ->
        compile e1 ++ pushq !%rdi ++ compile e2 ++ popq rsi ++ addq !%rsi !%rdi
    | Sub(e1, e2) ->
        compile e1 ++ pushq !%rdi ++ compile e2 ++ movq !%rdi !%rsi ++  popq rdi ++ subq !%rsi !%rdi
    | Mul(e1, e2) ->
        compile e1 ++ pushq !%rdi ++ compile e2 ++ popq rsi ++ imulq !%rsi !%rdi
    | Div(e1, e2) ->
        compile e1 ++ pushq !%rdi ++ compile e2 ++ popq rsi ++ movq !%rsi !%rax ++ cqto ++ idivq !%rdi ++ movq !%rax !%rdi
    | Mod(e1, e2) ->
        compile e1 ++ pushq !%rdi ++ compile e2 ++ popq rsi ++ movq !%rsi !%rax ++ cqto ++  idivq !%rdi ++ movq !%rdx !%rdi
        (* idivq rdi = rax@rdx / rdi, qutotient dans rax, reste dans rdx *)
    | Exp(e1, e2) ->
      begin need_exp := 1 ; compile e1 ++ pushq !%rdi ++ compile e2 ++ movq !%rdi !%rsi ++ popq rdx ++ movq (imm64 1L) !%rdi ++  jmp "compare" ++ ret ++ label "nextmain" end
      (* opérations des floats *)
      | F_Float(x) ->
          begin incr i ; d := !d ++ label (".LC" ^ (string_of_int(!i))) ++ (double x) ; movsd  (rellab (".LC" ^ (string_of_int(!i)))) !%xmm0 end
      | Adddot(e1, e2) ->
          compile e1 ++ pushf xmm0 ++ compile e2 ++ popf xmm1 ++ addsd !%xmm1 !%xmm0
      | Subdot(e1, e2) ->
          compile e1 ++ pushf xmm0 ++ compile e2 ++ movsd !%xmm0 !%xmm1  ++ popf xmm0 ++ subsd !%xmm1 !%xmm0
      | Muldot(e1, e2) ->
          compile e1 ++ pushf xmm0 ++ compile e2 ++ popf xmm1 ++ mulsd !%xmm1 !%xmm0
      (* conversions *)
      | Int(e) ->
          compile e ++ cvtsd2si !%xmm0 !%rdi
      | Float(e) ->
          compile e ++ cvtsi2sd !%rdi !%xmm0
    in let res = compile abr
    in (res, !d, need_exp)
  ;;
  
  
  
  let comp abr =
    let asttype = check abr in
    let (code, d, need_exp) = compile_main abr in
  { text = begin
    if asttype = I then globl "main" ++ label "main" ++ code ++ call "print_int" ++ ret ++
    inline "
print_int:
        movq    %rdi, %rsi
        movq    $S_int, %rdi
        xorq    %rax, %rax
        call    printf
        ret
"  else
globl "main" ++ label "main" ++ pushq !%rbp ++ code ++ call "print_float" ++ popq rbp ++ ret ++
inline "
print_float:
        movq    %rsp, %rbp
        movq    %rdi, %xmm0
        movl    $S_float, %edi
        movl    $1, %eax
        call    printf
        nop
        leave
        ret
" end ++ begin
if need_exp = ref 1 then
label "compare" ++ subq (imm64 1L) !%rsi ++ cmpq (imm64 0L) !%rsi ++ jge "exponent" ++ jmp "nextmain" ++ ret ++ label "exponent" ++ imulq !%rdx !%rdi ++ jmp "compare" ++ ret
else nop
end ; data = label "S_int" ++ string "%d" ++ label "S_float" ++ string "%f" ++ d ;}

