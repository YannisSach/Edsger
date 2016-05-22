open Ast


 let _ =
          try
            let lexbuf = Lexing.from_channel stdin in
            let _ = Parser.start Lexer.edsger lexbuf in
            print_teliko !program_tree;
            Scopes.check_program  !program_tree;
              
          with Parsing.Parse_error ->
            Printf.printf "Parsing error"



