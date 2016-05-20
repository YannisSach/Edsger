open Ast


 let _ =
          try
            let lexbuf = Lexing.from_channel stdin in
            let _ = Parser.start Lexer.edsger lexbuf in
            print_teliko !program_tree;
            Semantic.check_scopes !program_tree;
              
          with Parsing.Parse_error ->
                let (lexbuf, name)  = Queue.top Lexer.file_queue in
                Printf.printf "Error at file %s \n" name



