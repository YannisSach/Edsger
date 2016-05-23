open Format
open Typoi
open Symbol
open Types
open Identifier
open Str
(*open Symbtest*)

exception Terminate


let in_loop = ref 0	(* we set this var in order to know if we are in a loop because of break and continue *)

(* to compile with Str we use str.cma in ocaml *)
(*
let closeFunctionScope entry =
    (match entry with
    | ENTRY_function (inf) ->
      inf.function_scope <- !currentScope
    | _-> internal "error! Not a function :("; raise Terminate);
    closeScope();
*)

(* apo to string pou einai to type_t ginetai se Type poy exoyme apo to Types aplws prepei na vroume gia pinakes kai pointers *)
let rec antistoixise_types_fun t =
    let r  = Str.regexp "\\(char\\|int\\|bool\\|double\\|void\\)\\(\\**\\)" in
    	let tipos = Str.replace_first r "\\1" t 
	and point = Str.replace_first r "\\2" t in
	    check_pointer tipos point;

and antistoixise_type_dec t =
     let r  = Str.regexp "\\(char\\|int\\|bool\\|double\\)\\(\\**\\)\\[?\\(\\([0-9]*\\)\\)\\]?" in
    	let tipos = Str.replace_first r "\\1" t 
	and point = Str.replace_first r "\\2" t
	and pinak = Str.replace_first r "\\3" t in
	    check_pinak tipos point pinak;

and check_pinak ty poin pinak = match pinak with
| "" -> check_pointer ty poin
| t  -> TYPE_array ((check_pointer ty poin), (int_of_string t ))

and check_pointer ty poin = match String.length (poin) with
| 0 -> (check_type ty);
| 1 -> TYPE_pointer (check_type ty)
| _ -> TYPE_pointer (check_pointer ty (String.sub poin 1 ((String.length poin)-1)))

and check_type ty = match ty with
| "void" -> TYPE_proc
| "int" -> TYPE_int
| "double" -> TYPE_double
| "bool" -> TYPE_bool
| "char" -> TYPE_char
| _ -> TYPE_none

and fun_is_forward entries = match entries with
| [] -> ()
| [t] -> ()
| t :: rest ->
fun_is_forward rest

and  check_program t = match t with
| None -> printf("empty");
| Some tree -> initSymbolTable 256;
ignore(openScope());
check_declarations (tree); Symbtest.printSymbolTable();
ignore(closeScope());

and  check_declarations t = match t with
| [] -> ()
| [x] -> check_declaration x
| x::rest -> check_declaration x; 
  check_declarations rest

and check_declaration t = match t with
| Variable_dec (ty, decs) ->
  check_declarators ty decs;
| Function_dec (ty, name, params)->
  let t = ( newFunction (id_make name) true) in
      printf("Fun dec\n");
    (*  Symbtest.printSymbolTable(); 
      printf("After fun dec\n");*)
      ignore(openScope());
(*      Symbtest.printSymbolTable(); *)
      ignore(List.map (registerParams t) params);
      ignore(endFunctionHeader t (antistoixise_types_fun ty));
      ignore(forwardFunction t);
      closeScope();
| Function_def (ty, name, params, decls, stms) ->
  printf("Function definition");
  let t = ( Symbol.newFunction (id_make name) true) in (* t is fun entry (ty, t)=a, params *)
(*      Symbtest.printSymbolTable(); *)
      ignore(openScope());
(*      Symbtest.printSymbolTable();*)
      ignore(List.map (registerParams t) params);  
(*      Symbtest.printSymbolTable();*)
      ignore(endFunctionHeader t (antistoixise_types_fun ty));
(*    Symbtest.printSymbolTable();*)
      check_declarations decls;
      Symbtest.printSymbolTable();
      check_statements stms;
      Symbtest.printSymbolTable();
(*      let info = t.entry_info in
      (match info with
      |ENTRY_function inf ->
      		  inf.entry_scope <- !currentScope
      | _ -> printf "error! Not a function :(" raise Terminate);*)
      Symbtest.printSymbolTable();
      closeScope();
      Symbtest.printSymbolTable();

and check_declarators ty decs = match decs with
| [] ->printf("emppty"); ()
| [dec] -> check_declarator ty dec
| dec :: rest -> printf("2"); check_declarator ty dec;
check_declarators ty rest

(* if we have an array we must have an int expression and the type of the array *)
and check_declarator ty dec = match dec with
| Simple_declarator name->
  ignore (newVariable(id_make name) (antistoixise_types_fun ty) true);
(*| Complex_declarator (name, exp) ->
  ignore (newVariable(id_make name) (TYPE_array((antistoixise_types_fun ty), exp)) true);
*)

(*  t is the function entry aka t in our case *)
and registerParams t param  = match param with
    | By_val_param (typ, name)->
      ignore (newParameter (id_make name) (antistoixise_types_fun typ) PASS_BY_VALUE t true)      	  
    | By_ref_param (typ, name)->
      ignore (newParameter (id_make name) (antistoixise_types_fun typ) PASS_BY_REFERENCE t true)


and check_statements stms = match stms with
| [] -> ()
| stm :: rest ->
  check_statement stm;
  check_statements rest;

and check_statement stm = ();
(* match stm with
| Simple_expression exp -> check_expression exp;
| Statements -> check_statements
| If_stmt (exp, stm) ->
  check_bool_exp exp; (* prepei na dei ama einai typoy bool i exp *)
  check_statement stm;
| If_else_stmt (exp, stm1, stm2) ->
  check_bool_exp exp;
  check_statement stm1;
  checl_statement stm2;
| For_loop ->
  
| Branch (s1, s2)->
  check_loop s1; (* checks if we are in a loop *)
  (match s2 ->
  | None ->
  | Some s ->
  )
| Return ex -> match ex with
  | None -> check_fun_type (Type_none)
  | Some expr -> check_fun_type (check_type_exp ex);

*)
