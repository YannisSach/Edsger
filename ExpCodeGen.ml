open Types
open Symbol
open Ast
open Identifier
open Llvm
open Error
open Char
       
exception Type_error of string 

let context = global_context ()
let the_module = create_module context "LLVM IR"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let int_type = i16_type context
let double_type =  x86fp80_type context
let char_type = i8_type context
let bool_type = i1_type context
let fun_names : string list ref = ref []

let rec print_list = function 
    [] -> ()
| (e)::l -> print_string e ; Printf.printf " " ; print_list l
                                      

                                      
let rec find_function fun_name fun_name_list =
  match fun_name_list with
  | [] -> ( match lookup_function fun_name the_module with
              | Some calle -> calle
              | None -> raise (Type_error "unnknown function refernced"))
            
  | x::rest -> ( let to_found = String.concat "_" (fun_name::fun_name_list) in
                 match lookup_function to_found the_module with
                 |Some callee -> callee
                 | None -> find_function fun_name (List.tl fun_name_list))

                    
let rec code_gen_exp exp =
  match exp with
    Id name ->  let v =
                  try
                    Hashtbl.find named_values name;
                  with Not_found ->  raise (Type_error "Variable not found!"); in 
                v                                                                            
  | Int int -> let value = const_int int_type int in
               (* let llvalue = build_store value "inttmp" builder in *) (* dump_value value; *)
               value;
  | Double double -> const_float double_type double
  | Char char -> const_int char_type (code char)(* const_string context (escaped char) *)
  | String str -> build_global_stringptr str "strtmp" builder
  (* let value = const_string context str in (\*Changing array type to pointer type*\) *)
  (* let pointer = build_alloca (pointer_type char_type) "pointertmp" builder in *)
  (* build_store pointer value builder *)
  | Bool bool -> const_int bool_type (bool_to_int bool)
  | Constant_exp exp -> code_gen_exp exp
  | Function_call (called_fun, args) ->
     (* Look up the name in the module table. *)
   let callee = find_function called_fun ( !fun_names) in 
   let params = params callee in
   let args = Array.of_list args in
   (* If argument mismatch error. *)
   let args = Array.mapi 
                ( fun i a -> let e = code_gen_exp args.(i) in
                             match args.(i) with
                             | Id _ -> (let typical_param = String.length(string_of_lltype (type_of(params.(i)))) in
                                        let actual_param = String.length( string_of_lltype ( type_of(e))) in
                                        if (typical_param = actual_param) then
                                          e
                                        else
                                          build_load e "argtmp" builder)
                             | _-> e 
                               
                ) args 
   in
   build_call callee args "" builder
| Array (e1, e2) -> let pointer = code_gen_exp e1 in
                    dump_value pointer;
                    let index = code_gen_exp e2 in
                    let index = Array.of_list [index] in
                    let ir = build_gep pointer index "arraytmp" builder in
                    dump_value ir;
                    ir
          
| Unary_op (op,e) -> (match op with
                      | "&" -> code_gen_exp e (* FIX THIS *)
                        | "*" -> let exp_ir = code_gen_exp e in
                                 let tmp_ir = build_load exp_ir "loadtmp" builder in
                                 tmp_ir
                        | "-" -> let expr = Binary_op(Int 0, "-", e) in
                                 code_gen_exp expr
                        | "+" -> code_gen_exp e
                        | "!" -> let exp = code_gen_exp e in
                                 const_neg exp (* Semantic !!!! *)
                        | _ -> raise (Type_error "Wrong operator")
                       )

  |Binary_op (e1, op, e2) ->  let ir1 = code_gen_exp e1 in
                              let ir1 = if (is_pointer e1) then build_load ir1 "loadtmp" builder
                                        else ir1 in
                              let ir2 = code_gen_exp e2 in
                              let ir2 = if (is_pointer e2) then build_load ir2 "loadtmp" builder
                                        else ir2 in
                              (match op with
                               | "+" -> build_add ir1 ir2 "addtmp" builder
                               | "-" -> build_sub ir1 ir2 "subtmp" builder
                               | "*" -> build_mul ir1 ir2 "multmp" builder
                               | "/" -> build_sdiv ir1 ir2 "sdivtmp" builder
                               | "%" -> build_srem ir1 ir2 "sremtmp" builder
                               | "==" -> if(is_double ir1) then build_fcmp Llvm.Fcmp.Oeq ir1 ir2 "icmpeqtmp" builder
                                         else    build_icmp Llvm.Icmp.Eq ir1 ir2 "icmpeqtmp" builder
                               | "!=" -> if(is_double ir1) then build_fcmp Llvm.Fcmp.One ir1 ir2 "icmpeqtmp" builder
                                         else build_icmp Llvm.Icmp.Ne ir1 ir2 "icmpeqtmp" builder
                               | "<" ->  if(is_double ir1) then build_fcmp Llvm.Fcmp.Olt ir1 ir2 "icmpslttmp" builder
                                         else build_icmp Llvm.Icmp.Slt ir1 ir2 "icmpslttmp" builder
                               | ">" -> if(is_double ir1) then build_fcmp Llvm.Fcmp.Ogt ir1 ir2 "icmpsgttmp" builder
                                        else build_icmp Llvm.Icmp.Sgt ir1 ir2 "icmpsgttmp" builder
                               | "<=" -> if(is_double ir1) then build_fcmp Llvm.Fcmp.Ole ir1 ir2 "icmpsletmp" builder
                                         else build_icmp Llvm.Icmp.Sle ir1 ir2 "icmpsletmp" builder
                               | ">=" -> if(is_double ir1) then build_fcmp Llvm.Fcmp.Oge ir1 ir2 "icmpsgetmp" builder
                                         else build_icmp Llvm.Icmp.Sge ir1 ir2 "icmpsgetmp" builder
                               | "&&" -> build_and ir1 ir2 "andtmp" builder
                               | "||" -> build_or ir1 ir2 "ortmp" builder
                               | "," -> ir2 (* this is probably wrong *)
                               | _ -> (Error.error "%s: Unkown binary operator while producing IR" op; const_null int_type)
                              )
                                                           
                                  

  |Prefix_unary_as (op, e) -> (*Only Id's are acceptable*)
    let ir = code_gen_exp e in
    (match op with
     | "++" ->
        let expr = Binary_op(e,"+",Int 1) in
        let exp = code_gen_exp expr in
        build_store exp ir builder
     | "--" ->
        let expr = Binary_op(e,"-",Int 1) in
        let exp = code_gen_exp expr in
        build_store exp ir builder
     | _ -> Error.error "%s: Don't know what to do with prefix operator:" op; const_null int_type
    )

  |Postfix_unary_as (e, op) ->
    let ir = code_gen_exp e in
    (match op with 
     | "++" -> let expr = Binary_op(e,"+",Int 1) in
               let exp = code_gen_exp expr in
               build_store exp ir builder
     | "--" -> let expr = Binary_op(e,"-",Int 1) in
               let exp = code_gen_exp expr in
               build_store exp ir builder
     | _ -> Error.error "%s: Don't know what to do with prefix operator:" op; const_null int_type
    )
      
  |Binary_as (e1, op, e2) -> (*Postfix and Prefix unary ass have the same result as long as there is no other assignment in the statement...*)
    (*WARNING: We may assign pointer values not actual values!!!*)
    (match op with
     | "=" ->
        (* Printf.printf ("Simple assignment\n"); *)
        let lhs = code_gen_exp e1 in
        (match e2 with
         | Postfix_unary_as (e, op) ->
            let rhs = code_gen_exp e in
            (* dump_value lhs; *)
            (* dump_value rhs; *)
            let res = build_store rhs lhs builder in
            code_gen_exp e2
         | Prefix_unary_as(op, e) ->
            let _ = code_gen_exp e2 in
            let rhs = code_gen_exp e in
            let res = build_store rhs lhs builder in
            res
         | _ ->
            let rhs = code_gen_exp e2 in
            let rhs =
              if(is_pointer e2) then build_load rhs "loadtmp" builder
              else rhs
            in
            let res = build_store rhs lhs builder in
            (* dump_value res; *)
            res
        )
     | _ ->
        Printf.printf ("Complex asssignment\n");
        let lhs = code_gen_exp e1 in
        let expr = Binary_op (e1,op,e2) in
        let rhs = code_gen_exp expr in
        build_store lhs rhs builder
    )
      
  |Casting (t1, e1) -> let value = code_gen_exp e1 in
                       const_bitcast value (findLltype t1)
  |Question (e1,e2,e3) -> 
    let condition = code_gen_exp e1 in
    (* let zero = const_int integer_type 0  in *)
    let cond_val = condition (* build_fcmp Fcmp.One condition zero "ifcond" builder *) in
    let start_bb = insertion_block builder in (* start_bb contains the basic block *)
    let the_function = block_parent start_bb in
    let then_bb = append_block context "then" the_function in (* creates the then block *)
    position_at_end then_bb builder;

    let then_val = code_gen_exp e1 in

    let new_then_bb = insertion_block builder in

    let else_bb = append_block context "else" the_function in
    position_at_end else_bb builder;
                             
    let else_val = code_gen_exp e2 in
    let new_else_bb = insertion_block builder in

    let merge_bb = append_block context "ifcont" the_function in
    position_at_end merge_bb builder;

    let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in 
    let phi = build_phi incoming "iftmp" builder in
    position_at_end start_bb builder;
    ignore (build_cond_br cond_val then_bb else_bb builder);

    position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
    position_at_end new_else_bb builder; ignore (build_br merge_bb builder);

    (* Finally, set the builder to the end of the merge block. *)
    position_at_end merge_bb builder;
    phi
  (* const_null null_type;  *)
  (* e1 is i=0; e2 is i<10; e3=i++ and s the statements *)
                             

  |New_op (t, None) -> let ty = findLltype t in (*find types*)
                       build_malloc ty "malloctmp" builder
                                  
  |New_op (t, Some e) -> let ty = findLltype t in
                         let ir = code_gen_exp e in
                         build_array_malloc ty ir "mallocarraytmp" builder

  |Delete_op e ->
    let ir = code_gen_exp e in
    build_free ir builder
             
  |Paren_expression e -> code_gen_exp e

and convert_to_typical_types t = 
  match t with 
  | TYPE_array (t,_) -> TYPE_pointer t
  | _ -> t 

and convert_type_to_char t =
  match t with
  | TYPE_int -> "i"
  | TYPE_double -> "d"
  | TYPE_array (t,_) -> String.concat "" ["a"; (convert_type_to_char t)]
  | TYPE_char -> "c"
  | TYPE_bool -> "b"
  | TYPE_pointer x -> String.concat "" ["p" ;(convert_type_to_char x)]
  | _ -> ""

and create_suffix type_list = 
  let suffix = List.map (fun x -> convert_type_to_char x) type_list in 
  String.concat "" suffix 

and findLltype ty =
  match ty with
  | TYPE_int -> i16_type context
  | TYPE_double -> double_type
  | TYPE_array (t,n) -> let t = findLltype t in
                        array_type t n
  | TYPE_char -> i8_type context
  | TYPE_bool -> i1_type context
  | TYPE_pointer x -> let t = findLltype x in
                      pointer_type t
  | _ -> (Error.error "Unknown type"; i1_type context)

and bool_to_int n =
  match n with
  | false -> 0
  | true -> 1
    
and is_pointer ex =
  match ex with
  | Id _ -> true
  | Array _ -> true
  | Unary_op("*",e)->true
  | _ -> false

and is_double ir =
  let ty = string_of_lltype (type_of ir) in
  if((String.compare ty "x86_fp80") == 0) then true
  else false
                                             
                     
         (* Printf.printf "%s" ty; *)
         (* let re = Str.regexp_string "x86fp80" in *)
         (* (\* let re = Str.regexp_string "double" in *\) *)
         (* try ignore (Str.search_forward re ty 0); *)
         (*     Printf.printf "true\n"; true *)
         (* with Not_found ->false  *)
  

  
  
  
                     
