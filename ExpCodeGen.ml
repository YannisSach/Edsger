open Types
open Symbol
open Ast
open Identifier
open Llvm
open Error
open Char
       
exception Type_error of string 
type environment = Global of (string list)| Nested of (string list * environment)
module SS = Set.Make(String)
let context = global_context ()
let the_module = create_module context "LLVM IR"
let builder = builder context
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let int_type = i16_type context
let double_type =  x86fp80_type context
let char_type = i8_type context
let bool_type = i8_type context
let fun_names : string list ref = ref []
let env:environment ref = ref (Global ([]))
let global_decs : (Ast.declaration) list ref = ref []

let rec findLltype ty =
  match ty with
  | TYPE_int -> i16_type context
  | TYPE_double -> double_type
  | TYPE_array (t,n) -> let t = findLltype t in
                        array_type t n
  | TYPE_char -> i8_type context
  | TYPE_bool -> i8_type context
  | TYPE_pointer x -> let t = findLltype x in
                      pointer_type t
  | _ -> (Error.error "Unknown type"; i1_type context)

           
let print_hashtbl named_vales =
  Printf.printf ("Printing Hashtbl:\n");
  Hashtbl.iter (fun str llval -> Printf.printf ">%s\n" str) named_values

let contains s1 s2 =
  let re = Str.regexp_string s2
  in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

               
(*Environment Interface*)
let rec update_env_without_hashtbl name env =
  (* let name_env = String.concat  "_" [name;"env"] in *)
  (* let name_to_env = match env with Global(_) -> name | _ -> name_env in *)
  (* let _ = Hashtbl.add named_values name_to_env llval in  *)(*just to get it's type??*)
  match env with
  | Global (names) -> Global(name::names)
  | Nested (names,e) -> Nested (name::names,e)

and clear_env env_list =
  (* match env with *)
  (* |Global([])  *)
  (*  |Nested([],_) -> () *)
  (* |Global(h::t) -> Hashtbl.remove named_values h; clear_env (Global(t)) *)
  (* |Nested(h::t,a)->Hashtbl.remove named_values h; clear_env (Nested(t,a)) *)
  List.iter (Hashtbl.remove named_values) env_list
            
and get_env_of_called env args params =
  let argscnt = List.length args in
  let paramscnt = Array.length params in
  let cnt = paramscnt - argscnt in
  let rec walk env =
    let l = List.length (env_to_list env) in
    if (l=cnt) then env else
      match env with
      | Global([])-> Global([])
      | Global(h::t)-> walk (Global(t))
      | Nested ([],e) -> walk e
      | Nested ((h::t),e) -> walk (Nested (t,e))
  in walk env 
          
and update_env name env=
  let name_env = name in(* String.concat  "_" [name;"env"] in *)
  let name_to_env = match env with Global(_) -> name | _ -> name_env in
  match env with
  | Global (names) -> Global(name_to_env::names)
  | Nested (names,e) -> Nested (name_to_env::names,e)

                               
and update_env_with_params params en =
  let names = get_param_names params
  in List.iter (fun x-> env:=update_env x en) names

               
and env_to_set env =
  let rec walk env acc =
    match env with
    | Global (names) -> let set_to_add = SS.of_list names in SS.union set_to_add acc (*a global value may be shadowed*)
    | Nested (names,env) -> let set_to_add = SS.of_list names in
                            
                            let new_acc = SS.union set_to_add acc in
                            walk env new_acc
  in walk env SS.empty

          
and difference_with_env env params =
  let param_set = SS.of_list (get_param_names params) in
  let env_set = env_to_set env in
  SS.elements (SS.diff env_set param_set )

and get_env_params_types env global_decs =
  let has_name_in_dec name dec =
    match dec with
    |Ast.Simple_declarator(n) -> (name = n)
    |Ast.Complex_declarator (n,_) -> (name = n)
  in
  let has_name_in_var_dec name element =
    match element with
    |Ast.Variable_dec(ty,decl) -> (try ignore(List.find (has_name_in_dec name) decl); true
                                   with Not_found -> false)
    |_ -> raise Not_found

  in
  let find_type_from_global name gl =
    (* Printf.printf "Searching for %s\n" name; *)
    (* Printf.printf "Global length: %d\n" (List.length gl); *)
    let dec = List.find (has_name_in_var_dec name) gl (* with Not_found -> Ast.Variable_dec(TYPE_none,[]) *)
    in match dec with
       | Ast.Variable_dec(ty,l) -> (let wanted = List.find (has_name_in_dec name) l in
                                    let ty = findLltype ty in
                                    match wanted with
                                    |Ast.Simple_declarator _ -> pointer_type ty
                                    |Ast.Complex_declarator _ -> pointer_type (pointer_type ty))
       | _ -> Printf.printf "unexpected type"; int_type
  in
  let find_type name =
    try
      let v =
        try Hashtbl.find named_values name
        with Not_found -> raise Not_found
      in (type_of v)
    with Not_found ->  find_type_from_global name global_decs
  in 
  List.map find_type env

           

and env_to_list env =
  SS.elements (env_to_set env)
              
and print_env_pars env =
  let l = env_to_list env in
  let _  = match env with Nested _ -> Printf.printf "Nested:\n" | Global _ -> Printf.printf "Global:\n" in
  List.iter (fun x -> Printf.printf "List:%s\n" x) l
            
and remove_env env =
  match env with
  | Global (names) -> Global([])
  | Nested (names,env) -> env

and env_to_id_list env =
  let env_list = env_to_list env in
  List.map (fun x -> Id(x)) env_list 
           
(*Probably we don't need this*)
and get_args_diff_env args env=
  let args_set = SS.of_list args in
  let env_set = env_to_set env in
  let new_env_set = SS.diff env_set args_set in
  SS.elements new_env_set

and get_param_names params =
  let get_param_name p =
    match p with
    | Ast.By_val_param(_,name) -> name
    | Ast.By_ref_param(_,name) -> name
  in List.map get_param_name params
              

(*End of environment interface*)

              
let rec print_list = function 
    [] -> ()
  | (e)::l -> print_string e ; Printf.printf " " ; print_list l
                                                              
let rec find_function fun_name fun_name_list =
  match fun_name_list with
  | [] -> ( match lookup_function fun_name the_module with
            | Some calle -> calle
            | None -> raise (Type_error "unknown function refernced"))
            
  | x::rest -> ( let to_found = String.concat "_" (fun_name::fun_name_list) in
                 match lookup_function to_found the_module with
                 |Some callee -> callee
                 | None -> find_function fun_name (List.tl fun_name_list))

                 
let rec code_gen_exp exp =
  match exp with
    Id name ->  let v =
                  (* Printf.printf "Trying to find %s...\n" name; *)
                  (* print_hashtbl named_values; *)
                  try
                    Hashtbl.find named_values name
                                 (* with Not_found -> raise (Type_error "Variable not found!") in *)
                  with Not_found -> raise (Type_error "Variable not found") in
                v                                                                            
  | Int int -> let value = const_int int_type int in
               (* let llvalue = build_store value "inttmp" builder in *) (* dump_value value; *)
               value;
  | Double double -> const_float double_type double
  | Char char -> const_int char_type (code char)(* const_string context (escaped char) *)
  | String "NULL" -> const_pointer_null (pointer_type int_type)
  | String str -> build_global_stringptr str "strtmp" builder
  | Bool bool -> const_int bool_type (bool_to_int bool)
  | Constant_exp exp -> code_gen_exp exp
| Function_call (called_fun, args) ->
   (* Look up the name in the module table. *)
   let create_param p = By_val_param(TYPE_none,"dum") in
   let callee = find_function called_fun (!fun_names) in
   let params = params callee in
   let tmp = if(is_void callee) then "" else "calltmp" in
   let env_of_called = get_env_of_called !env args params in
   let env_args = env_to_id_list (env_of_called) in
   (* let find_name_of_gl_decs decs = List.map (fun x -> match x with | Ast.Simple_declarator(name)-> name | Ast.Complex_declarator (name,_) ->  name) decs in *)
   (* let names_global_pars = List.flatten (List.map (fun x-> match x with | Ast.Variable_dec(ty, decs) -> find_name_of_gl_decs decs) !global_decs)  in *)
   (* let global_pars = List.map (fun x -> Id(x)) names_global_pars in *)
   (* let args = args@global_pars@env_args_wo_global in *)
   let args = args@env_args in
   let args = Array.of_list args in
   (* If argument mismatch error. *)
   let args = Array.mapi 
                ( fun i a -> let e = code_gen_exp args.(i) in (*used to be i*)
                             if(is_pointer args.(i)) then
                               (let typical_param = String.length(string_of_lltype (type_of(params.(i)))) in
                                let actual_param = String.length( string_of_lltype ( type_of(e))) in
                                if (typical_param = actual_param) then
                                  e
                                else
                                  build_load e "argtmp" builder
                               )
                             else e
                                      
                ) args 
   in
   build_call callee args tmp builder
| Array (e1, e2) -> let pointer = code_gen_exp e1 in
                    let act_array = build_load pointer "act_ar" builder in
                    let index = code_gen_exp e2 in
                    let index = if (is_pointer e2) then build_load index "tmpindex" builder
                                else index in
                    let index = Array.of_list [index] in
                    let ir = build_gep act_array index "arraytmp" builder in
                    (* dump_value ir; *)
                    ir 
| Unary_op (op,e) -> (match op with
                      | "&" ->  code_gen_exp e 
                      (* FIX THIS *)
                      | "*" -> let exp_ir = code_gen_exp e in
                               let tmp_ir = build_load exp_ir "loadtmp" builder in
                               tmp_ir
                      | "-" -> let expr = Binary_op(Int 0, "-", e) in
                               code_gen_exp expr
                      | "+" -> code_gen_exp e
                      | "!" -> let exp = code_gen_exp e in
                               build_not exp "nottmp" builder (* Semantic !!!! *)
                      | _ -> raise (Type_error "Wrong operator")
                     )

|Binary_op (e1, op, e2) ->  let ir1 = code_gen_exp e1 in 
                            let ir2 = code_gen_exp e2 in
                            (
                              let ir1 = if (is_pointer e1) then build_load ir1 "loadtmp" builder
                                        else ir1 in
                              let ir2 = if (is_pointer e2) then build_load ir2 "loadtmp" builder
                                        else ir2 in
                              (match op with
                               | "+" ->  if(is_double ir1) then build_fadd ir1 ir2 "faddtmp" builder
                                         else if (is_op_with_pointer ir1) then code_gen_exp (Array(e1,e2))
                                         else if (is_op_with_pointer ir2) then code_gen_exp (Array(e2,e1))
                                         else build_add ir1 ir2 "addtmp" builder
                               | "-" ->  if(is_double ir1) then build_fsub ir1 ir2 "fsubtmp" builder
                                         else if (is_op_with_pointer ir1) then code_gen_exp (Array(e1,e2))
                                         else if (is_op_with_pointer ir2) then code_gen_exp (Array(e2,e1))
                                         else build_sub ir1 ir2 "subtmp" builder
                               | "*" ->  if(is_double ir1) then build_fmul ir1 ir2 "fmultmp" builder
                                         else build_mul ir1 ir2 "multmp" builder
                               | "/" ->  if(is_double ir1) then build_fdiv ir1 ir2 "fdivtmp" builder
                                         else build_sdiv ir1 ir2 "sdivtmp" builder
                               | "%" -> build_srem ir1 ir2 "sremtmp" builder
                               | "==" -> if (e1 = String "NULL") then build_is_null ir2 "is_null" builder
                                         else if (e2 = String "NULL") then build_is_null ir1 "is_null" builder
                                         else if ((e1 = String "NULL") && (e2 = String "NULL")) then code_gen_exp (Bool true)
                                         else
                                           if(is_double ir1) then build_fcmp Llvm.Fcmp.Oeq ir1 ir2 "icmpeqtmp" builder
                                           else
                                             let ir1 = if(is_op_with_pointer ir1) then build_ptrtoint ir1 (int_type) "ptrtoint" builder
                                                       else ir1 in
                                             let ir2 = if (is_op_with_pointer ir2) then build_ptrtoint ir2 (int_type) "ptrtoint" builder
                                                       else ir2 in
                                             build_icmp Llvm.Icmp.Eq ir1 ir2 "icmpeqtmp" builder
                               | "!=" -> if (e1 = String "NULL") then build_is_null ir2 "is_null" builder
                                         else if (e2 = String "NULL") then build_is_null ir1 "is_null" builder
                                         else if ((e1 = String "NULL") && (e2 = String "NULL")) then code_gen_exp (Bool false)
                                         else
                                           if(is_double ir1) then build_fcmp Llvm.Fcmp.One ir1 ir2 "icmpeqtmp" builder
                                           else
                                             let ir1 = if(is_op_with_pointer ir1) then build_ptrtoint ir1 (int_type) "ptrtoint" builder
                                                       else ir1 in
                                             let ir2 = if (is_op_with_pointer ir2) then build_ptrtoint ir2 (int_type) "ptrtoint" builder
                                                       else ir2 in
                                             build_icmp Llvm.Icmp.Ne ir1 ir2 "icmpeqtmp" builder
                               | "<" ->  if(is_double ir1) then build_fcmp Llvm.Fcmp.Olt ir1 ir2 "icmpslttmp" builder
                                         else let ir1 = if(is_op_with_pointer ir1) then build_ptrtoint ir1 (int_type) "ptrtoint" builder
                                                        else ir1 in
                                              let ir2 = if (is_op_with_pointer ir2) then build_ptrtoint ir2 (int_type) "ptrtoint" builder
                                                        else ir2 in
                                              build_icmp Llvm.Icmp.Slt ir1 ir2 "icmpslttmp" builder
                               | ">" -> if(is_double ir1) then build_fcmp Llvm.Fcmp.Ogt ir1 ir2 "icmpsgttmp" builder
                                        else let ir1 = if(is_op_with_pointer ir1) then build_ptrtoint ir1 (int_type) "ptrtoint" builder
                                                       else ir1 in
                                             let ir2 = if (is_op_with_pointer ir2) then build_ptrtoint ir2 (int_type) "ptrtoint" builder
                                                       else ir2 in
                                             build_icmp Llvm.Icmp.Sgt ir1 ir2 "icmpsgttmp" builder
                               | "<=" -> if(is_double ir1) then build_fcmp Llvm.Fcmp.Ole ir1 ir2 "icmpsletmp" builder
                                         else let ir1 = if(is_op_with_pointer ir1) then build_ptrtoint ir1 (int_type) "ptrtoint" builder
                                                        else ir1 in
                                              let ir2 = if (is_op_with_pointer ir2) then build_ptrtoint ir2 (int_type) "ptrtoint" builder
                                                        else ir2 in
                                              build_icmp Llvm.Icmp.Sle ir1 ir2 "icmpsletmp" builder
                               | ">=" -> if(is_double ir1) then build_fcmp Llvm.Fcmp.Oge ir1 ir2 "icmpsgetmp" builder
                                         else let ir1 = if(is_op_with_pointer ir1) then build_ptrtoint ir1 (int_type) "ptrtoint" builder
                                                        else ir1 in
                                              let ir2 = if (is_op_with_pointer ir2) then build_ptrtoint ir2 (int_type) "ptrtoint" builder
                                                        else ir2 in
                                              build_icmp Llvm.Icmp.Sge ir1 ir2 "icmpsgetmp" builder
                               | "&&" -> let ir1_i1 = build_trunc_or_bitcast ir1 (i1_type context) "first_cast" builder in
                                         let ir2_i1 = build_trunc_or_bitcast ir2 (i1_type context) "second_cast" builder in
                                         build_and ir1_i1 ir2_i1 "andtmp" builder
                               | "||" -> let ir1_i1 = build_trunc_or_bitcast ir1 (i1_type context) "first_cast" builder in
                                         let ir2_i1 = build_trunc_or_bitcast ir2 (i1_type context)"second_cast" builder in
                                         build_or ir1_i1 ir2_i1 "ortmp" builder
                               | "," -> (* dump_value ir1; dump_value ir2;*) 
                                  ir2 (* this is probably wrong *)
                               | _ -> (Error.error "%s: Unkown binary operator while producing IR" op ;const_null int_type)
                              )
                            )  
                                  

|Prefix_unary_as (op, e) -> (*Only Id's are acceptable*) (* print_string "vrike prefix unary reee"; *)
  let ir = code_gen_exp e in
  (* dump_type (type_of ir); *)
  (match op with
   | "++" ->
      (* dump_value ir; *)
      (* print_string("before search"); *)
      let expr = if (is_double_pointer ir) then Binary_op(e, "+", Double 1.0)
                                                         (* else if (is_op_with_pointer ir) then (print_string"vrike pointer"; Array(e,Int 1)) *)
                 else Binary_op(e,"+",Int 1) in
      let exp = code_gen_exp expr in
      let _ = build_store exp ir builder in
      (* dump_value ir; *)
      (* print_string "telos prefix"; *)
      exp
   | "--" ->
      let expr = if (is_double_pointer ir) then Binary_op(e, "+", Double 1.0)
                 else Binary_op(e,"-",Int 1) in
      let exp = code_gen_exp expr in
      let _ = build_store exp ir builder in
      exp
   | _ -> Error.error "%s: Don't know what to do with prefix operator:" op; const_null int_type
  )

|Postfix_unary_as (e, op) ->
  let ir = code_gen_exp e in
  (match op with 
   | "++" -> let expr = if (is_double_pointer ir) then Binary_op(e, "+", Double 1.0)
                        else Binary_op(e,"+",Int 1) in
             let exp = code_gen_exp expr in
             (* dump_value exp; *)
             let _ = build_store exp ir builder in
             let sube = if (is_double_pointer exp) then Binary_op(e, "-", Double 1.0)
                        else Binary_op(e,"-",Int 1) in
             let sub = code_gen_exp sube in
             sub
   | "--" -> let expr = if (is_double_pointer ir) then Binary_op(e, "-", Double 1.0)
                        else Binary_op(e,"-",Int 1) in
             let exp = code_gen_exp expr in
             let _ = build_store exp ir builder in
             let adde = if (is_double_pointer exp) then Binary_op(e, "+", Double 1.0)
                        else Binary_op(e,"+",Int 1) in
             let add = code_gen_exp adde in
             add

   | _ -> Error.error "%s: Don't know what to do with prefix operator:" op; const_null int_type
  )
      
|Binary_as (e1, op, e2) -> (*Postfix and Prefix unary ass have the same result as long as there is no other assignment in the statement...*)
  (*WARNING: We may assign pointer values not actual values!!!*)
  (* let _ = match e1 with *)
  (*   | Id name -> Printf.printf "%s\n" name *)
  (*   | _ -> Printf.printf "Too complex\n" *)
  (* in *)
  (match op with
   | "=" ->
      (* Printf.printf ("Simple assignment\n"); *)
        
      (match e2 with
       | Postfix_unary_as (e, op) ->
          (* print_string "postfix operator"; *)
          let rhs' = code_gen_exp e in
          let rhs = if(is_pointer e) then((* print_string "it is"; *) build_load rhs' "loadtmp" builder)
                    else rhs' in
          let lhs = code_gen_exp e1 in
          let _ = build_store rhs lhs builder in
          (* dump_type (type_of rhs'); *)
          let oper =( match op with
                      | "++" -> "+"
                      | _ -> "-" )in
          let adde = if(is_double_pointer rhs') then( Binary_op(e, oper,Double 1.0))
                     else ( (* print_string "kati ginetai"; *) Binary_op(e, oper,Int 1)) in
          let add = code_gen_exp adde in
                                                               
            
          let _ = build_store add rhs' builder in
          lhs
       | Prefix_unary_as(op, e) ->
          let ret_val = code_gen_exp e2 in
          let rhs = (* if (is_op_with_pointer ret_val) then ret_val *) ret_val in
          (* dump_value ret_val; *)
          let lhs = code_gen_exp e1 in
          let _ = build_store rhs lhs builder in
          lhs
       | String "NULL"-> let lhs = code_gen_exp e1 in
                         let lhs4del = build_load lhs "loadforNull" builder in
                         let ty = type_of lhs4del in
                         delete_instruction lhs4del;
                         let null = const_null ty in
                         build_store null lhs builder
       | _ -> 
          let rhs = code_gen_exp e2 in
          let rhs = (* dump_type (type_of(rhs)); *)
            if(is_pointer e2) then build_load rhs "loadtmp" builder
            else rhs
          in
          let lhs = code_gen_exp e1 in
          (* dump_value lhs; *)
          let _ = build_store rhs lhs builder in
          (* dump_value res; *)
          lhs
      )
   | _ ->
      (* Printf.printf ("Complex asssignment\n"); *)
      let op = (match op with
                | "+=" -> "+"
                | "-=" -> "-"
                | "*=" -> "*"
                | "/=" -> "/"
                | "%=" -> "%"
                | _ -> ("Unkown Operator")) in 
      let expr = Binary_op (e1,op,e2) in
      let rhs = code_gen_exp expr in
      let lhs = code_gen_exp e1 in
      build_store  rhs lhs builder
  )
      
|Casting (t1, e1) ->let exp =  code_gen_exp e1 in
                    let ty_exp = string_of_lltype(type_of exp) in
                    let exp = try
                        (ignore(String.index ty_exp '*');
                         build_load exp "loadcastingtmp" builder)
                      with Not_found -> ( exp) in
                    (* print_string ty_exp; *)
                    if (contains ty_exp "x86_fp80") then (  
                      (* print_string ("found double"); *)
                      let ty = findLltype t1 in
                      build_fptosi exp ty "castingtmp" builder)
                    else if (contains ty_exp  "i16") then
                      ( (* print_string ("found int"); *)
                        match t1 with
                        | TYPE_double -> let ty = findLltype t1 in  (* if we want double cast *)
                                         build_sitofp exp ty "castingtmp" builder
                        | _ -> let ty = findLltype t1 in
                               build_trunc_or_bitcast exp ty "castingtmp" builder )
                    else if (contains ty_exp "i8") then
                      ((* print_string ("found char"); *)
                       match t1 with
                       | TYPE_int -> let ty = findLltype t1 in
                                     build_sext_or_bitcast exp ty "castingtmp" builder
                       | TYPE_double -> let ty = findLltype t1 in
                                        build_sitofp exp ty "castingtmp" builder
                       | _ -> let ty = findLltype t1 in
                              build_trunc_or_bitcast exp ty "castingtmp" builder )
                    else
                      ((* print_string ("mpike sto else"); *)
                       match t1 with
                       | TYPE_double -> let ty = findLltype t1 in
                                        build_sitofp exp ty "castingtmp" builder
                       | _ -> let ty = findLltype  t1 in
                              build_sext_or_bitcast exp ty "castingtmp" builder)          
                         
|Question (e1,e2,e3) ->  let condition = code_gen_exp e1 in
                         let zero = if (String.contains(string_of_lltype(type_of condition)) '1') then  const_int (i1_type context) 0
                                    else const_int (bool_type) 0 in
                         let cond_val = if (is_pointer e1) then build_load condition "loadcon" builder
                                        else condition in
                         let cond_val = build_icmp Icmp.Ne cond_val zero "ifcond" builder in
                         (* let cond_val = condition (\* build_fcmp Fcmp.One condition zero "ifcond" builder *\) in *)
                         let start_bb = insertion_block builder in (* start_bb contains the basic block *)
                         let the_function = block_parent start_bb in
                         let then_bb = append_block context "then" the_function in (* creates the then block *)
                         position_at_end then_bb builder;

                         let then_val =  code_gen_exp e2 in
                         let then_val = if (is_pointer e2) then build_load then_val "loadtmp" builder else then_val in
                         let new_then_bb = insertion_block builder in

                         let else_bb = append_block context "else" the_function in
                         position_at_end else_bb builder;
                         let else_val = code_gen_exp e3 in
                         let else_val = if (is_pointer e3) then build_load else_val "loadtmp" builder else else_val in
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
                           
                          
|New_op (t, None) -> let ty = findLltype t in (*find types*)
                     build_malloc (ty) "malloctmp" builder
                                    
|New_op (t, Some e) -> let ty = findLltype t in
                       let ir = code_gen_exp e in
                       let ir = if is_pointer e then build_load ir "loadtmp" builder else ir in
                       build_array_alloca (ty) ir "mallocarraytmp" builder

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


and bool_to_int n =
  match n with
  | false -> 0
  | true -> 1
              
and is_pointer ex =
  match ex with
  | Id _ -> true
  | Array _ -> true
  | Unary_op("*",e)->true
  | Binary_as(e,_,_) -> is_pointer e
  | Ast.Paren_expression(e) -> is_pointer e
  | _ -> false

and is_binary_as ex =
  match ex with
  | Binary_as(_,_,_) -> true
  | _ -> false
           
and is_double ir =
  let ty = string_of_lltype (type_of ir) in
  if((String.compare ty "x86_fp80") == 0) then true
  else false

and is_op_with_pointer ir =
  let ty = string_of_lltype (type_of ir) in
  if ((contains ty "*")) then  true
  else false
         
and is_void callee =
  let ty = string_of_lltype (type_of callee) in
  let re = Str.regexp_string "void" in
  try ignore (Str.search_forward re ty 0); true
  with Not_found -> false

and is_double_pointer ir =
  let ty = string_of_lltype (type_of ir) in
  if (contains ty "x86") then true
  else false
