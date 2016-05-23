exception Terminate
val antistoixise_types_fun : Typoi.result_type -> Types.typ
val check_program : Typoi.declaration list option -> unit
val check_declarations : Typoi.declaration list -> unit
val check_declaration : Typoi.declaration -> unit
val check_declarators : Typoi.type_t -> Typoi.declarator list -> unit
val check_declarator : Typoi.type_t -> Typoi.declarator -> unit
val registerParams : Symbol.entry -> Typoi.parameter -> unit
val check_statements : Typoi.statement list -> unit
val check_statement : Typoi.statement -> unit
