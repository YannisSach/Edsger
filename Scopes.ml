open Format
open Ast
open Symbol
open Types
open Identifier
open Str
open TypeInference

exception Terminate

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

and  check_program t = match t with
| None -> printf("empty");
| Some tree -> check_declarations (tree)

and  check_declarations t = match t with
| [] -> ()
| x::rest -> check_declaration x

and check_declaration t = match t with
| Variable_dec (ty, decs) ->
  check_declarators ty decs;
| Function_dec (ty, name, params)->
  let t = ( newFunction (id_make name) true) in
      ignore(openScope());
      ignore(List.map (registerParams t) params);
      ignore(endFunctionHeader t (ty));
      ignore(forwardFunction t);
      closeScope();
| Function_def (ty, name, params, decls, stms) ->
  let t = ( Symbol.newFunction (id_make name) true) in (* t is fun entry (ty, t)=a, params *)
      ignore(openScope());
      ignore(List.map (registerParams t) params);  
      ignore(endFunctionHeader t (ty));
      check_declarations decls;
      check_statements stms;
(*      let info = t.entry_info in
      (match info with
      |ENTRY_function inf ->
      		  inf.entry_scope <- !currentScope
      | _ -> printf "error! Not a function :(" raise Terminate);*)
      closeScope();


and check_declarators ty decs = match decs with
| [] -> ()
| [dec] -> check_declarator ty dec
| dec :: rest -> check_declarator ty dec;
check_declarators ty decs

(* if we have an array we must have an int expression and the type of the array *)
and check_declarator ty dec = match dec with
|Simple_declarator name->
  ignore (newVariable (id_make name) ( ty) true);
|Complex_declarator (name, exp) ->
  if (equalType (findType exp) TYPE_int) then 
  ignore (newVariable (id_make name) (TYPE_array(ty,0)) true) (*we don't care for the size just typechecking... *)
  else Printf.printf "Error in array declaration"


(*  t is the function entry aka t in our case *)
and registerParams t param  = match param with
    | By_val_param (typ, name)->
      ignore (newParameter (id_make name) (typ) PASS_BY_VALUE t true)      	  
    | By_ref_param (typ, name)->
      ignore (newParameter (id_make name) (typ) PASS_BY_REFERENCE t true)


and check_statements stms = match stms with
| [] -> ()
| stm :: rest ->
  check_statement stm;
  check_statements rest;

and check_statement stm = 
match stm with
| Simple_expression None -> ()
| Simple_expression Some exp -> ignore (findType exp)
| Statements stm-> check_statements stm
| If_stmt (exp, stm) ->
  ignore (equalType (findType exp) TYPE_bool); (* prepei na dei ama einai typoy bool i exp *)
  check_statement stm;
| If_else_stmt (exp, stm1, stm2) ->
  ignore (equalType (findType exp) TYPE_bool);
  check_statement stm1;
  check_statement stm2;
| For_loop (tag,e1,e2,e3,s) ->
   (match e1 with
         None -> ()
        |Some e -> ignore (findType e)
   );
   (match e2 with
         None -> ()
        |Some e -> ignore (equalType (findType e) TYPE_bool)
   );
   (match e3 with
         None -> ()
        |Some e -> ignore (findType e)
   );
   check_statement s
  
| Branch (e1, e2)->
  (*check_loop s1; (* checks if we are in a loop *)*)
  (match e2 with
  None -> ()
  | Some tag -> ()
  )
| Return ex -> match ex with
  | None -> () (*ignore (check_fun_type (TYPE_proc))*)
  | Some expr ->() (* ignore (check_fun_type (findType expr))*)

(* and check_fun_type name typ = *)
(*     let t = lookupEntry (id_make name) LOOKUP_ALL_SCOPES true in *)
(*     	match t with *)
(* 	| ENTRY_function inf -> *)
(* 	  try *)
(* 		Types.equalType inf.function_result typ *)
(* 	  with Not_found -> *)
(* 	       Printf.printf "Function result and return result are not the same type"; *)
(* 	       raise Exit *)
(* 	| _ -> Printf.printf "something went wrong" *)
