open Types
open Symbol

exception type_error of string 

let findType exp = let correct = checkTypes exp in
                   if (not correct) then raise Exception "Error in Typecheking\n"
                   else (
                     match exp with
                       Id name -> let e = lookupEntry !tab (id_make name) in
                                  (match e.entry_info with
                                     ENTRY_variable variable_info ->  variable_info.variable_type
                                   | _ -> raise type_error "This is not a variable";  ())

                     | Int int -> TYPE_int
                     | Double double -> TYPE_double (*We should add it to Types*)
                     | Char char -> TYPE_char (*Same with this one*)
                     | Bool bool -> TYPE_bool (* ... *)
                     | Constant_exp exp -> findType exp
                     | Function_call (name, exps)->
                        let _ = List.map findType exps in
                        let e = lookupEntry !tab (id_make name) in 
                        (match e.entry_info with
                           ENTRY_function function_info -> function_info.function_result
                         | _ -> raise type_error "This function has no result"
                        )
                     | Array (e1, e2) -> let ty = findType e1 in
                                         TYPE_array (ty,0)
                     | Unary_op (op,e) -> (match op with
                                             "&" -> TYPE_pointer (findType e)
                                           | "*" -> TYPE_pointer (x) -> x
                                           | "-"
                                             | "+" -> findType e
                                          )

                     |Binary_op e1 op e2 -> let t1 = findType e1 in
                                            let t2 = findType e2 in
                                            (match op with
                                               "+"  
                                             | "-"  
                                               | "*" 
                                               | "/" ->  (match t1, t2 with 
                                                            TYPE_int, TYPE_int 
                                                            | TYPE_float, TYPE_float 
                                                              | TYPE_pointer , TYPE_int -> t1

                                                          | _,_ raise type_error "Invalid arguments for binary operator")
                                             | '%' -> (match t1, t2 with
                                                         TYPE_int, TYPE_int -> t1
                                                       | _, _ -> "Invalid arguments for binary operator")
                                             | "=="
                                               | "!=" ->  if (equalType t1 t2 ) then TYPE_bool else raise "Invalid arguments in comparison"
                                             |"<"
                                              |">"
                                              | "<="
                                              | ">=" -> (match t1,t2 with
                                                           TYPE_int, TYPE_int 
                                                         | TYPE_double, TYPE_double
                                                           | TYPE_pointer, TYPE_pointer
                                                           | TYPE_bool, TYPE_bool  -> TYPE_bool
                                                         | _, _ -> raise "Invalid arguments in comparison")
                                             |"&&"
                                              | "||" -> (match t1, t2 with
                                                           TYPE_bool, TYPE_bool -> TYPE_bool
                                                          |_,_ -> raise "Invalid arguments in logical comparison")
                                             | "," -> t2

                                            )

                                              
                                              
                                              
                                              
                                              
