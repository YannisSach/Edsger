
 let _ =
          try
            let lexbuf = Lexing.from_channel stdin in
            let _ = Queue.push (lexbuf,"stdin") Lexer.file_queue in
            Parser.start Lexer.edsger lexbuf;
          with Parsing.Parse_error ->
                let (lexbuf, name)  = Queue.top Lexer.file_queue in
                Printf.printf "Error at file %s \n" name



