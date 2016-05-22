open Types
open Symbol
open Ast
open Identifier


exception Type_error of string 

let rec findType exp =
  match exp with
    Id name -> let e = lookupEntry (id_make name) LOOKUP_ALL_SCOPES true in
               (match e.entry_info with
                  ENTRY_variable variable_info ->  variable_info.variable_type
                | _ -> raise (Type_error "This is not a variable")
                             )

  | Int int -> TYPE_int
  | Double double -> TYPE_double (*We should add it to Types*)
  | Char char -> TYPE_char (*Same with this one*)
  | String str -> TYPE_none
  | Bool bool -> TYPE_bool (* ... *)
  | Constant_exp exp -> findType exp
  | Function_call (name, exps)->
     let _ = List.map findType exps in
     let e = lookupEntry (id_make name) LOOKUP_ALL_SCOPES true in 
     (match e.entry_info with
        ENTRY_function function_info -> function_info.function_result
      | _ -> raise (Type_error "This function has no result")
     )
  | Array (e1, e2) -> let ty = findType e1 in
                      TYPE_array (ty,0)
  | Unary_op (op,e) -> (match op with
                          "&" -> TYPE_pointer (findType e)
                        | "*" -> let ty = (findType e) in
                                 (match ty with 
                                   TYPE_pointer x -> x
                                 | _ -> raise (Type_error "Error finding pointers"))
                        | "-"
                          | "+" -> findType e
                        | _ -> raise (Type_error "Wrong operator")
                       )

  |Binary_op (e1, op, e2) -> let t1 = findType e1 in
                             let t2 = findType e2 in
                             (match op with
                                "+"  
                              | "-"  
                                | "*" 
                                | "/" ->  (match t1, t2 with 
                                             TYPE_int, TYPE_int 
                                           | TYPE_double, TYPE_double 
                                             | TYPE_pointer _, TYPE_int -> t1

                                           | _,_ ->raise (Type_error "Invalid arguments for binary operator"))
                              | "%" -> (match t1, t2 with
                                          TYPE_int, TYPE_int -> t1
                                        | _, _ -> raise (Type_error "Invalid arguments for binary operator"))
                              | "=="
                                | "!=" ->  if (equalType t1 t2 ) then TYPE_bool else raise (Type_error"Invalid arguments in comparison")
                              |"<"
                               |">"
                               | "<="
                               | ">=" -> (match t1,t2 with
                                            TYPE_int, TYPE_int 
                                          | TYPE_double, TYPE_double
                                            | TYPE_pointer _ , TYPE_pointer _ 
                                            | TYPE_bool, TYPE_bool  -> TYPE_bool
                                          | _, _ -> raise (Type_error"Invalid arguments in comparison"))
                              |"&&"
                               | "||" -> (match t1, t2 with
                                            TYPE_bool, TYPE_bool -> TYPE_bool
                                           |_,_ -> raise (Type_error "Invalid arguments in logical comparison"))
                              | "," -> t2
                              | _ -> raise (Type_error "Unkown binary operator")
                             )

  |Prefix_unary_as (op, e) ->
    let ty = findType e in 
    (match ty with 
       TYPE_int 
      |TYPE_double 
       |TYPE_pointer _ -> ty
      | _ -> raise (Type_error "Unary assignement with wrong type")
    )
  |Postfix_unary_as (e, op) ->
    let ty = findType e in 
    (match ty with 
       TYPE_int 
      |TYPE_double 
       |TYPE_pointer _ -> ty
      | _ -> raise (Type_error "Unary assignement with wrong type")
    )
  |Binary_as (e1, op, e2) ->
    let t1 = findType e1 in 
    let t2 = findType e2 in 
    (match op with 
       "=" -> if ( equalType t1 t2 ) then t1 else raise (Type_error "Binary assignment wrong types")
     | "*=" 
       | "/=" -> 
        (match t1,t2 with 
           TYPE_int , TYPE_int 
         | TYPE_double, TYPE_double -> TYPE_double
         | _, _ -> raise (Type_error "Binary assignment wrong types")
        )
     | "%=" ->
           (match t1,t2 with 
            TYPE_int, TYPE_int -> t1
            | _, _ -> raise (Type_error "Binary assignment wrong types")
           )
       | "+="
       | "-=" -> 
        (match t1, t2 with
           TYPE_int, TYPE_int
         | TYPE_double, TYPE_double 
           |TYPE_pointer _ , TYPE_int -> t1
         |_,_ -> raise (Type_error "Binary assignment wrong types")   
        )
       | _ -> raise (Type_error "Wrong operator in binary assignement")
    )
  |Casting (t1, e1) -> let _ = findType e1 in t1
  |Question (e1,e2,e3) -> let t1 = findType e1 in
                          let t2 = findType e2 in
                          let t3 = findType e3 in
                          (if (equalType t1 TYPE_bool) then 
                            (if (equalType t2 t3) then t2 else raise (Type_error "Wrong types in question"))
                          else raise (Type_error "Wrong types in question"))   
  |New_op (t, None) -> t
  |New_op (t, Some e) -> 
    if (equalType (findType e) TYPE_int) then t else raise (Type_error "Wrong types in new operator")
  |Delete_op e ->
    let t = findType e in
    (match t with
       TYPE_pointer _ -> t
     | _ -> raise (Type_error "Wrong wrong arguments in delete")
    )
  |Paren_expression e -> findType e

                                                
      
                                                        


       



     


                               
                               
                               
                               
                               
