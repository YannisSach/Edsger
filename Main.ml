open Ast
open Lexer

let _ =
  try
    let i = Array.length Sys.argv in
    let lexbuf = Lexing.from_channel stdin in
    (
      match i with 
      | 1 ->     let _ = SS.add "stdin" in 
                 Parser.start Lexer.edsger lexbuf
      | 2 -> if(Sys.argv.(1) = "p") then
               (
                 let _ = SS.add "stdin" in 
                 let _ = Parser.start Lexer.edsger lexbuf in
                 print_teliko !program_tree;
               )
             else
               (
                 let lexbuf = Lexing.from_channel (open_in (Sys.argv.(1))) in
                 let _ = SS.add (Sys.argv.(1)) in
                 Parser.start Lexer.edsger lexbuf 
               )
      | 3 -> if (Sys.argv.(1) = "p") then(
               let lexbuf = Lexing.from_channel (open_in (Sys.argv.(2))) in
               let _ = SS.add (Sys.argv.(1)) in
               let _ = Parser.start Lexer.edsger lexbuf in
               print_teliko !program_tree
             )
             else(
               Printf.printf "Unkown parameter: %s.\nQuiting now...\n" Sys.argv.(1);
               exit(0)
             )
      |_ -> (Printf.printf "Too many arguments!\nQuiting now...\n"; exit(0))
    );
    Scopes.check_program  !program_tree
  with Parsing.Parse_error ->
    Printf.printf "Parsing error"



