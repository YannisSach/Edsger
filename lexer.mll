(* scanner for Edsger *)
{
  open Printf
  open Queue
  open Lexing
  open Parser
  exception Eof
(*this is a stack containing a Lexing.lexbuf and the name of the header file*)
  let file_set = Queue.create ()
  let file_queue  = Queue.create()
  let not_in_queue file queue =  
  let rec find_element file queue =
         if (Queue.is_empty queue) then true
         else
           let (_,name) = Queue.pop queue
           in if (compare file name) == 0 then false
              else find_element file queue
  and queue' = Queue.copy file_queue
      in find_element file queue' 

}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let escape_char = '\\' ('n' | 't' | 'r' | '0'| '\'' | '\"' |('x' (digit|['A'-'F' 'a'-'f']) (digit|['A'-'F' 'a'-'f'])) | '\\')
let escape = ['\\' '\n' '\'' '\n' '\r' '\t' '\b' '\ '] 
(* let op = '=' | "==" | "!=" | '>' | '<' | ">=" | "<="    | '&' | '!' | "&&" | "||" | '?' | ':' | ',' | "++" | "--" | "+=" | "*=" | "/=" | "%=" *)


rule edsger = parse
           | digit+ as integer
			 { T_INT(int_of_string integer) }

             | digit+ '.' digit+ ('e' ("-"|"+")? digit+)? as double
                                  { T_DOUBLE(float_of_string double)
                                  }
         | "if"		{T_IF}
         | "bool"	{T_BOOLTYPE}
         | "break"  	{T_BREAK}
         | "char"	{T_CHARTYPE}
         | "continue"	{T_CONTINUE}
         | "delete"	{T_DELETE}	
         | "double"	{T_DOUBLETYPE}
         | "false"	{T_FALSE}
         | "int"	{T_INTTYPE}
         | "new"	{T_NEW}
         | "NULL"	{T_NULL}
         | "return"	{T_RETURN}
         | "true"	{T_TRUE}
         | "else"	{T_ELSE}
         | "for"	{T_FOR}
         | "byref"	{T_BYREF}
    (*     | "#include"*)
         | "void" 	{T_VOID}
         | '\'' [^ '\\' '\"' '\''] '\'' as character  
         | '\'' escape_char '\'' as character
                      { T_CHAR(character.[1])
                      }
         | '\"' ([^ '\n' '\t' '\r' '\"']| escape_char)* '\"' as string (*na to doume xana*)
                                         {
						T_STRING(string)
                                         }
         | id
                   { T_ID }
        (* | op  		{T_OP} *)
	 | '+'		{T_PLUS}
	 | '-'		{T_MINUS}
	 | '*'		{T_TIMES}
	 | '/' 		{T_DIV}
	 | '%'  	{T_MOD}
	 | '=' 		{T_ASS}
	 | "=="		{T_EQ}
	 | "!="		{T_DIF}
	 | '>' 		{T_BIG}
	 | '<'		{T_SMALL}
	 | ">="		{T_BEQ}
	 | "<="		{T_SEQ}
	 | '&'		{T_AMP}
	 | '!' 		{T_EX}
	 | "&&"		{T_AND}
	 | "||"		{T_OR}
	 | '?'		{T_QUES}
	 | ':'		{T_COLON}
	 | ','		{T_COMMA}
	 | "++"		{T_PP}
	 | "--"		{T_MM}
	 | "+="		{T_PEQ}
	 | "-="		{T_MIEQ}
	 | "*="		{T_TEQ}
	 | "/="		{T_DEQ}
	 | "%="		{T_MEQ}
         | ';'		{T_SEMIC}
         | '('		{T_LP}
         | ')'		{T_RP}
         | '['		{T_LSB}
         | ']'		{T_RSB}
         | '{' 		{T_LB}
         | '}' 		{T_RB}
         | [' ' '\t' '\n' '\r' ] { edsger lexbuf } 
         | "//" [^ '\n']*  { edsger lexbuf } (* eat up one-line comments *)
         | "/*" _* "*/" {edsger lexbuf}  (*multi line comment*)
         | _ as c
                  { printf "Unrecognized character: %c\n" c;
                    edsger lexbuf
                  }
         |"#include \"" id ".h\"" as include_rule (*{edsger lexbuf}*)
                                       {
                                         let len = (String.length include_rule)- 10 -1
                                         in let file = String.sub include_rule 10 len
                                                in if (not_in_queue file file_set)
                                                   then 
                                                     ( let lexbuf' = Lexing.from_channel (open_in file)
                                                       in Queue.push (lexbuf', file) file_queue ; 
                                                          Queue.push file file_set;
                                                          edsger lexbuf
                                                     )
                                                   else 
                                                     (Printf.printf "Include file: %s is already in parsing queue!\n" file; edsger lexbuf) (* A cycle just broken *)
                                       }
             | eof 	{ T_EOF}

               
