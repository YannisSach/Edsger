type file = Program of program and 
program = Declarations of declaration list and
declaration =  |Variable_dec of (type_t * declarator list)
               |Function_dec of (result_type * string * parameter list)
               |Function_def of (result_type * string * parameter list * declaration list * statement list) and
type_t = Type of string and
declarator = Simple_declarator of string
           |Complex_declarator of (string * expression) and
result_type = type_t and
parameter = By_val_param of (string * string)
          | By_ref_param of (string * string) and
statement = Expression of expression option
          |Statments of statement list
          |If_stmt of (expression * statement)
          |If_else_stmt of (expression * statement * statement)   
          |For_loop of (string option * expression option * expression option  * statement)
          |Branch of (string * string option)
          |Return of expression option and

expression = Id of string
          |Paren_expression of expression
          |Literal of string
          |Constant_exp of expression
          |Function_call of (expression list * expression * expression)
          |Array of (expression * expression)
          |Unary_op of (string * expression)
          |Binary_op of (expression * string * expression)
          |Prefix_unary_as of (string * expression)
          |Postfix_unary_as of (expression * string)
          |Binary_as of (expression * string * expression)
          |Casting of (type_t * expression)
          |Question of (expression * expression * expression)
          |New_op of (type_t * expression option )
          |Delete_op of expression 








 





