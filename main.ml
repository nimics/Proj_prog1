(* entry point *)

open Lexer
open Parser
open Asyntax
open Compiler
open X86_64

let check_format f =
  let n = String.length f in
  if not(n > 3 && f.[n-1] = 'p' && f.[n-2] = 'x' && f.[n-3] = 'e') then failwith "You are not using the good format"
  else ()
;;


let main file =
  let fp = open_in file in
  check_format file;
  try
    let lexbuf = Lexing.from_channel fp in
    let ast = Parser.main Lexer.token lexbuf in
    print_in_file "test.s" (comp ast)
  with
   | Lexer.Eof ->
      exit 0
;;

main Sys.argv.(1);;