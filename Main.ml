open Ast
open Lexer

 let _ =
          try
            let i = Array.length Sys.argv in
            let lexbuf = Lexing.from_channel stdin in
            (if (i>1) then 
              (let lexbuf = Lexing.from_channel (open_in (Sys.argv.(1))) in
               let _ = SS.add (Sys.argv.(1)) in
               Parser.start Lexer.edsger lexbuf )
            else
              let _ = SS.add "stdin" in 
              Parser.start Lexer.edsger lexbuf);
            print_teliko !program_tree;
            Scopes.check_program  !program_tree;
              
          with Parsing.Parse_error ->
            Printf.printf "Parsing error"



