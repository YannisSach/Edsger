exception Terminate
val antistoixise_types_fun : string -> Types.typ
val antistoixise_type_dec : string -> Types.typ
val check_pinak : string -> string -> string -> Types.typ
val check_pointer : string -> string -> Types.typ
val check_type : string -> Types.typ
val check_program : Ast.declaration list option -> unit
val check_declarations : Ast.declaration list -> unit
val check_declaration : Ast.declaration -> unit
val check_declarators : Ast.type_t -> Ast.declarator list -> unit
val check_declarator : Ast.type_t -> Ast.declarator -> unit
val registerParams : Symbol.entry -> Ast.parameter -> unit
val check_statements : Ast.statement list -> unit
val check_statement : Ast.statement -> unit
