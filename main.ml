
 let _ =
          try
            let lexbuf = Lexing.from_channel stdin in
            let _ = Queue.push (lexbuf,"stdin") Lexer.file_queue in
            (Parser.start Lexer.edsger lexbuf;
             let _ = Queue.pop Lexer.file_queue in
            while not (Queue.is_empty Lexer.file_queue) do
              let (lexbuf, name)  = Queue.top Lexer.file_queue in
              let _ = Printf.printf "Reading %s\n" name 
              in ( Parser.start Lexer.edsger lexbuf ; 
                   let _ = Queue.pop Lexer.file_queue in
                   ()
                 )
            done)
          with Parsing.Parse_error ->
                let (lexbuf, name)  = Queue.top Lexer.file_queue in
                Printf.printf "Error at file %s \n" name



