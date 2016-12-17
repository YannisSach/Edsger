open Llvm
open Types
open Scopes
open ExpCodeGen

exception Error of string

let context = global_context ()
let the_module = ExpCodeGen.the_module
let builder =  ExpCodeGen.builder
let named_values:(string, llvalue) Hashtbl.t = ExpCodeGen.named_values
let integer_type  = i16_type context
let null_type = i1_type context
let bool_type = i1_type context
let char_type = i8_type context
let double_type = ExpCodeGen.double_type
let fun_names = ExpCodeGen.fun_names                        
let fun_bbs : llbasicblock list ref = ref []                                      
let returns = ref true
let continue_tags : (string *llbasicblock) list ref = ref []
let break_tags : (string * llbasicblock ) list ref = ref []
let old_bindings : (string * llvalue) list ref = ref []
(* this fun takes the type of the elem and returns the lltype *)

                                                     
let rec type_to_lltype ty = match ty with
    
  | Types.TYPE_double -> double_type
  | Types.TYPE_int -> integer_type
  | Types.TYPE_bool -> bool_type
  | Types.TYPE_char -> i8_type context (* NOT SURE *)
  | Types.TYPE_array (t,n) -> let typ =type_to_lltype t in
                              array_type typ n
  | Types.TYPE_pointer x -> let t = type_to_lltype x in
                            pointer_type t
  | Types.TYPE_proc -> void_type context
  | _ -> null_type                        

           
and param_type par = match par with
  | Ast.By_val_param(ty,name) -> type_to_lltype ty
  | Ast.By_ref_param(ty,name) -> let typ = type_to_lltype ty in
                                 pointer_type typ

and find_block stag tuple = match tuple with
  | [] -> raise Not_found
  | (tag, tag_bb):: rest -> if stag = tag then tag_bb
                            else
                              find_block stag rest
let rec print_list = function 
    [] -> ()
  | (e,s)::l -> print_string e ; Printf.printf " " ; print_list l
                                                                
let create_entry_block_alloca the_function var_name ty =
  let typ = type_to_lltype ty in
  let builder = builder_at context (instr_begin (entry_block the_function)) in (* we make a builder which points at the first instruction of the block and allocates memory there *)
  build_alloca typ var_name builder

let rec is_return_last_instuction stmts = match stmts with
  | [] -> false
  | [Ast.Return _] -> true
  | _::tail -> is_return_last_instuction tail
                                         
(* HAVE TO CHANGE IT TO UNIT *)
let rec codegen_program prog =
  match prog with
  | Some decs -> codegen_declars decs
  | None -> codegen_declars []

and codegen_declars x = match x with
    [] -> const_null null_type
  | x::rest -> 
     ignore(codegen_declaration x);
     
     codegen_declars rest
and remove_variable_delcarations x = match x with
    [] -> ()
  | x::rest ->
     match x with
     |Ast.Variable_dec(ty,decs) ->
       ignore(
           List.map (fun dec -> match dec with
                                | Ast.Simple_declarator(name)
                                  | Ast.Complex_declarator(name,_)-> Hashtbl.remove named_values name 
                    ) decs
         )
     |_-> remove_variable_delcarations rest
                                       
                                       
and codegen_declaration x = match x with
  (* maybe we dont need the fun dec but never mind *)
  | Ast.Function_dec(ty, name, parameters) ->
     let llpars = Array.of_list (List.map param_type parameters) in   (* llpars is an array of the params *)
     let fun_typ = type_to_lltype ty in                           (* THATS WRONG HAVE TO CHANGE WITH MATCH *) 
     let ft = function_type fun_typ llpars in
     let fn_name = String.concat "_" (name::!fun_names) in
     let f = declare_function fn_name ft the_module in
     Array.iteri ( fun i a ->
                   let p = (List.nth parameters i) in
                   match p with
                   | Ast.By_val_param(ty, name)->
                      (* HAVE TO SEE THE WAY WE TAKE IT *)
                      set_value_name name a;
                      Hashtbl.add named_values name a
                   | Ast.By_ref_param(ty, name)->
                      set_value_name name a;
                      Hashtbl.add named_values name a
                 )
                 (params f); f
  | Ast.Variable_dec(ty, decs) ->
     let _ = block_parent (insertion_block builder) in
     let typos = type_to_lltype ty in (*we want the type of the vars *)
     let value = init_value ty in (* NOT SURE IF WE WANT THE POINTER TO BE NULL AT FIRST *)
     let _ = List.map (fun dec ->
                 match dec with
                 | Ast.Simple_declarator(name) ->
                    let alloca = build_alloca typos name builder in
                    ignore(build_store value alloca builder);
                    Hashtbl.add named_values name alloca;
                    if(List.mem name !environment) then () else environment := name::!environment
                 | Ast.Complex_declarator(name, exp) ->
                    let leng = code_gen_exp exp in
                    (* dump_value leng; *)(* we have the length of the array in llvalue *)
                    let decl = build_alloca (pointer_type typos) name builder in
                    let alloca = build_array_malloc (typos) leng "allocatmp" builder in (* or build array malloc/alloca *)
                    let _ = build_store alloca decl builder in
                    Hashtbl.add named_values name decl;
                    if(List.mem name !environment) then () else environment := name::!environment
               (* HAVE TO CHECK IT AGAIN *)
               ) decs; in
     const_null null_type;

| Ast.Function_def(ty, name, parameters, decls, stms) ->
   old_bindings := [];
     
   let llpars = Array.of_list (List.map param_type parameters) in 
   let fun_typ = type_to_lltype ty in (* THATS WRONG HAVE TO CHANGE WITH MATCH *)
   let ft = function_type fun_typ llpars in (* creates a function *)
   let fn_name = String.concat "_" (name::!fun_names) in
   let the_fun = (match lookup_function fn_name the_module with
                  | None -> fun_names:= name :: !fun_names;
                            declare_function fn_name ft the_module
                  | Some f -> fun_names := name :: !fun_names; f ) in
   (* create a new basic block for the function *)
   let label = String.concat "_" [name;"entry"] in
     
   let bb= append_block context label the_fun in
   fun_bbs := bb :: !fun_bbs;
   position_at_end bb builder; (* point at the end of the new created block *)
   (* we initialize the params and add them to Hashtable *)
   let _ = Array.iteri(fun i a ->
               let n = (List.nth parameters i) in
               match n with
               | Ast.By_val_param(ty, name) ->
                  let typ = type_to_lltype ty in
                  set_value_name name a;
                  let alloca = build_alloca typ name builder in
                  ignore(build_store a alloca builder);
                  Hashtbl.add named_values name alloca
               | Ast.By_ref_param(ty, name) ->
                  set_value_name name a;
                  Hashtbl.add named_values name a)
                      (params the_fun)in

     
   let _ = codegen_declars decls in
   if (ty = TYPE_proc) then
     returns := false (* we have set a ret point yes *)
   else
     returns := true;
   let bb = List.hd !fun_bbs in
   position_at_end bb builder;
   let _ =  codegen_states stms  in

   fun_bbs := List.tl !fun_bbs;
   let next_bb = try List.hd !fun_bbs
                 with Failure ("hd") -> bb in
   fun_names := List.tl !fun_names;
     
   (* List.iter (fun (var_name, old_value) -> *)
   (*     Hashtbl.add named_values var_name old_value *)
   (*   ) !old_bindings; *)
   (*YANNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNIS*)
   (* we initialize the params and add them to Hashtable *)
   Array.iteri(fun i a ->
       let n = (List.nth parameters i) in
       match n with
       | Ast.By_val_param(ty, name) ->
          Hashtbl.remove named_values name
       | Ast.By_ref_param(ty, name) ->
          Hashtbl.remove named_values name )
              (params the_fun);
   (*FREE DECLS*)
   remove_variable_delcarations decls;
   (*YANNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNIS*)   

   if !returns = false then 
     (ignore(build_ret_void builder); position_at_end next_bb builder; the_fun)
   else
     (
       let _ = if (is_return_last_instuction stms) then () else ignore(build_ret (init_value ty) builder) in
       position_at_end next_bb builder; the_fun );


and codegen_states st = match st with
  | [] -> const_null null_type
  | x::rest -> ignore(codegen_statement x);
               codegen_states rest
                              
and codegen_statement st = match st with
  | Ast.Simple_expression(None) -> const_null integer_type
  | Ast.Simple_expression(Some exp) -> code_gen_exp exp
  | Ast.Statements sts -> codegen_states sts
  | Ast.If_stmt(cond, stm) ->
     let condition = code_gen_exp cond in
     let _ = const_int integer_type 0 in
     let cond_val = if (is_pointer cond) then build_load condition "loadcon" builder
                    else condition in

     (* let cond_val = cond (\* build_fcmp Fcmp.One cond zero "ifcond" builder *\) in *)

     let start_bb = insertion_block builder in
     let the_function = block_parent start_bb in

     (* to calculate the llvalue of current block *)
     let bef_bb = insertion_block builder in
     let _ = value_of_block bef_bb in
     
     let then_bb = append_block context "then" the_function in (* we create the then block *)
     position_at_end then_bb builder; (* we put builder at the end of blovk then_bb *)

     let _ = codegen_statement stm in  (* we create the code for the then statement *)
     let new_then_bb = insertion_block builder in (* we save the block in new_then_bb *)

     let merge_bb = append_block context "ifcont" the_function in (* we create the block after the if *)
     position_at_end merge_bb builder;

     (* let incoming = [(then_val, new_then_bb); (bef_bb_val,bef_bb)] in *)
     (* let phi = build_phi incoming "iftmp" builder in *)

     position_at_end start_bb builder; (* we return to the starting block *)
     ignore (build_cond_br cond_val then_bb merge_bb builder); (* we concat the two blocks *)

     position_at_end new_then_bb builder; ignore (build_br merge_bb builder); (* we set the then pointer to the endif block *)
     position_at_end merge_bb builder; (* we set the builder to the end of common block *)
     (* phi *)
     const_null null_type;
     

  | Ast.If_else_stmt(cond, stm1, stm2) ->
     let condition = code_gen_exp cond in
     
     let _ = const_int integer_type 0  in
     let cond_val = if (is_pointer cond) then build_load condition "loadcon" builder
                    else condition in
     (* let cond_val = condition (\* build_fcmp Fcmp.One condition zero "ifcond" builder *\) in *)

     let start_bb = insertion_block builder in (* start_bb contains the basic block *)
     let the_function = block_parent start_bb in
     let then_bb = append_block context "then" the_function in (* creates the then block *)
     position_at_end then_bb builder;

     let _ = codegen_statement stm1 in

     let new_then_bb = insertion_block builder in

     let else_bb = append_block context "else" the_function in
     position_at_end else_bb builder;
     
     let _ = codegen_statement stm2 in
     let new_else_bb = insertion_block builder in

     let merge_bb = append_block context "ifcont" the_function in
     position_at_end merge_bb builder;

     (* let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in *)
     (* let phi = build_phi incoming "iftmp" builder in *)
     position_at_end start_bb builder;
     ignore (build_cond_br cond_val then_bb else_bb builder);

     position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
     position_at_end new_else_bb builder; ignore (build_br merge_bb builder);

     (* Finally, set the builder to the end of the merge block. *)
     position_at_end merge_bb builder;

     (* phi *)
     const_null null_type;
  (* e1 is i=0; e2 is i<10; e3=i++ and s the statements *)
  | Ast.For_loop(tag, e1, e2, e3, s) ->

     (* 1 we calculate the first exp *)
     let _ = match e1 with
       | None -> () (* NOT SURE IF IGNORE WORKS *)
       | Some exp -> ignore ( code_gen_exp exp) in

     (* now we make a new block in order to have only the condition *)
     let preheader_bb = insertion_block builder in
     let the_function = block_parent preheader_bb in

     let loop_cond_bb = append_block context "loopcondition" the_function in
     position_at_end preheader_bb builder;

     (* now we make the branch *)
     ignore (build_br loop_cond_bb builder);

     position_at_end loop_cond_bb builder;
     (* 2 now we have to check the condition *)
     let loop_cond =  match e2 with
       | None -> const_null null_type
       | Some exp -> let tmp = code_gen_exp exp in if (is_pointer exp) then build_load tmp "loadcon" builder else
                                                     tmp
                                                       
     in
     
     let _ = const_int integer_type 0 in
     let cond_val = loop_cond (* build_fcmp Fcmp.One loop_cond zero "loopcond" builder *) in

     (* 3 now we point to the block in which we are *)
     let preheader_bb = insertion_block builder in
     let the_function = block_parent preheader_bb in
     
     (* we create the loop block *)
     let loop_bb = append_block context "loopbody" the_function in
     position_at_end loop_bb builder;
     
     let step_bb= append_block context "loopstepblock" the_function in
     position_at_end step_bb builder;
     let _ =(match tag with
             | None -> let tag_bb = insertion_block builder in
                       continue_tags := ("$$$", tag_bb) :: !continue_tags
             | Some str -> let tag_bb = insertion_block builder in
                           continue_tags := (str, tag_bb) :: !continue_tags;
                           continue_tags := ("$$$",tag_bb) :: !continue_tags) in               

     let afterloop_bb = append_block context "afterloop" the_function in
     position_at_end afterloop_bb builder;
     (* we have to create the after loop block *)
     ignore ( match tag with
              | None -> let tag_bb = insertion_block builder in 
                        break_tags := ("$$$", tag_bb) :: !break_tags
              | Some str -> let tag_bb = insertion_block builder in
                            break_tags := (str, tag_bb) :: !break_tags;
                            break_tags := ("$$$",tag_bb) :: !break_tags
            );

     position_at_end loop_bb builder;
     ignore (codegen_statement s);
     ignore(build_br step_bb builder);

     position_at_end step_bb builder;
     (* now we calculate the step *)
     let _ = match e3 with
       | None -> const_null null_type 
       | Some exp -> code_gen_exp exp in

     
     position_at_end step_bb builder; (* we point to the afterloop block *)

     (* builder goes at the end of the loop block *)
     ignore(build_br loop_cond_bb builder);

     (* go again to check condition *)
     position_at_end loop_cond_bb builder;
     (* 4 now we again point at the end of the condition in order to check where to go *)
     position_at_end preheader_bb builder;
     ignore(build_cond_br cond_val loop_bb afterloop_bb builder);

     position_at_end afterloop_bb builder;
     continue_tags:= List.tl (!continue_tags);
     break_tags:= List.tl(!break_tags);
     const_null null_type
                
  | Ast.Branch(str1, str2)-> ( match str1 with
                               | "continue" -> (match str2 with
                                                | None -> ( try
                                                              let con_bb = find_block "$$$" (!continue_tags) in                
                                                              ignore ( build_br con_bb builder); const_null null_type
                                                            with Not_found->
                                                              (
                                                                Printf.printf "falseNotag"; const_null null_type))
                                                            
                                                | Some tag -> try
                                                              let con_bb = find_block tag (!continue_tags) in
                                                              ignore(build_br con_bb builder); const_null null_type
                                                            with Not_found ->
                                                              (Printf.printf "false"; const_null null_type)
                                               )
                               | "break" -> (match str2 with
                                               
                                             | None -> ( try
                                                           let br_bb = find_block "$$$" (!break_tags) in                
                                                           ignore ( build_br br_bb builder); const_null null_type
                                                         with Not_found->
                                                           (
                                                             Printf.printf "falseNotag"; const_null null_type))
                                             | Some tag -> try
                                                           let br_bb = find_block tag (!break_tags) in
                                                           ignore(build_br br_bb builder); const_null null_type
                                                         with Not_found ->
                                                           (Printf.printf "false"; const_null null_type)
                                                             
                                            )
                               | _ -> raise (Type_error "Something went wrong");
                             )
  (* in Return we have a variable name somehow and we save there the result *)
  | Ast.Return(ex) -> match ex with
                      | Some exp ->
                         let ret_val = code_gen_exp exp in
                         let ret_val =
                           if(ExpCodeGen.is_pointer exp) then build_load ret_val "ret" builder
                           else ret_val
                         in
                         build_ret ret_val builder 
                      | None -> returns := true;
                                build_ret_void builder

and init_value ty = (match ty with
                     | TYPE_int  -> (const_int integer_type 0)
                     | TYPE_double  -> const_float double_type 0.0
                     | TYPE_bool  -> const_int bool_type 0
                     | TYPE_char  -> (const_int char_type 0)(* const_string context "" *)
                     | TYPE_pointer x  ->
                        ( let lltype = type_to_lltype (TYPE_pointer x) in
                          let lltype = const_pointer_null lltype in
                          (* dump_value lltype; *) lltype)
                     |_ -> raise (Type_error "problem with value allocation"))

