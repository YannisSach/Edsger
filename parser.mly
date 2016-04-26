%token T_QUES
%token EOL
%token T_EOF
%token T_PLUS T_MINUS T_TIMES T_DIV T_MOD
%token T_ID
%token <int> T_INT
%token <float> T_DOUBLE
%token <char> T_CHAR
%token <string> T_STRING
        %token T_IF
        %token T_BOOLTYPE        
        %token T_BREAK      
        %token T_CHARTYPE        
        %token T_CONTINUE        
        %token T_DELETE        
        %token T_DOUBLETYPE
        %token T_FALSE        
        %token T_INTTYPE
        %token T_NEW
        %token T_NULL
        %token T_RETURN
        %token T_TRUE
        %token T_ELSE
        %token T_FOR
        %token T_VOID
        %token T_BYREF
        %token T_SEMIC
        %token T_PLUS T_MINUS        /* lowest precedence */
        %token T_TIMES T_DIV T_MOD
        %token T_PP  T_MM 
        %token T_AMP T_EX
        %token T_AND T_OR
        %token T_ASS T_PEQ  T_MIEQ T_TEQ  T_DEQ T_MEQ
        %token T_COMMA 
        %token T_EQ T_DIF
        %token T_BIG T_SMALL T_BEQ T_SEQ  T_QUES T_COLON
        %token T_LP T_RP
        %token T_RSB T_LSB
        %token T_RB T_LB
        
/*--------------------------ORDER---------------------------*/
        %nonassoc EXPLIST /*Na to ksanadoume ayto*/
      /*DANGLING IF */
        %nonassoc IF_STMNT
        %nonassoc T_ELSE 
      /* ------------*/
        %left T_COMMA 
        %nonassoc TIMES /*for type_t shift/reduce conflict */
        %right T_ASS T_PEQ T_MIEQ T_TEQ T_DEQ T_MEQ
        %nonassoc T_QUES
        %right COMPIF
        %left T_OR
        %left T_AND
        %nonassoc T_EQ T_DIF T_BIG T_SMALL T_SEQ T_BEQ
        %right ASSIGNMENT
        %left OPERATOR /* all binary_operators for shift/reduce*/ 
        %left T_PLUS T_MINUS       
        %left T_TIMES T_DIV T_MOD
        %nonassoc CASTING /*bonus*/
        %nonassoc T_PP T_MM
        %nonassoc T_NEW T_DELETE
        %nonassoc UNOP
        %nonassoc T_LSB T_RSB /*see http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual026.html how conflicts are resolved */
        %nonassoc POSTFIX
        %nonassoc PREFIX

        %start start             /* the entry point */

        %type <unit> start 
        %%
        start: program T_EOF {()}

        program:
            program declaration {()} 
            | declaration {()}
                    ;

        declaration:
          variable_declaration  {()}
          |function_declaration  {()}
          |function_definition {()}
        ;                                                  ;

        variable_declaration: 
          type_t declarators T_SEMIC {()}   
        ;
 
       /* type_t: 
              basic_type {()}
              |type_t T_TIMES %prec TIMES{()}
                      ;
                        */
       type_t:
        basic_type star %prec TIMES{()}
                      ;
       star:
             {()}
             |star T_TIMES{()}
                       ;
                       
        basic_type: 
                    T_INTTYPE  {()}
                    | T_CHARTYPE  {()}
                    | T_BOOLTYPE  {()} 
                    | T_DOUBLETYPE {()}
                ;

        declarator: 
          T_ID {()}
          | T_ID T_LSB constant_expression T_RSB {()}
                ;
        
        declarators:
          declarator T_COMMA declarators {()}
          |declarator {()}
        ;
        
        function_declaration:
         type_t T_ID T_LP T_RP T_SEMIC {()} 
         |type_t T_ID T_LP parameter_list T_RP T_SEMIC {()}
         |T_VOID T_ID T_LP T_RP T_SEMIC {()} 
         |T_VOID T_ID T_LP parameter_list T_RP T_SEMIC {()}
                
        ;

/*NEVER USED replace result_type with type_t | Void */
          /*result_type: 
         type_t {()}
         |T_VOID {()}
        ;*/

          parameter_list:
            parameter parameters {()}
        ;

           parameters :
             {()}
             |T_COMMA parameter parameters {()}
         ;

           parameter : 
             T_BYREF type_t T_ID {()}
             |type_t T_ID {()}
                     ;

                       function_definition: 
                         type_t T_ID T_LP T_RP T_LB declarations statements T_RB {()}
                         |  type_t T_ID T_LP parameter_list T_RP T_LB declarations statements T_RB {()}

                         |T_VOID T_ID T_LP T_RP T_LB declarations statements T_RB {()}
                         |T_VOID T_ID T_LP parameter_list T_RP T_LB declarations statements T_RB {()}
                     ;

        declarations:
             {()}
          |declaration declarations {()}
                     ;


       statements:
             {()}
                   |statements statement {()}
                     ;
        statement: 
          T_SEMIC{()}
          | expression T_SEMIC {()}
          | T_LB statements T_RB {()}
          | T_IF T_LP expression T_RP statement %prec IF_STMNT{()} 
          | T_IF T_LP expression T_RP statement T_ELSE statement {()} 
          | optional_t_id_t_colon T_FOR T_LP optional_expression T_SEMIC optional_expression T_SEMIC optional_expression T_RP statement {()}
          | T_CONTINUE  T_SEMIC  {()}
          | T_CONTINUE T_ID T_SEMIC  {()} 
          | T_BREAK  T_SEMIC  {()}
          | T_BREAK T_ID T_SEMIC {()}
          | T_RETURN  T_SEMIC {()}
          | T_RETURN expression T_SEMIC {()}
                     ;
        optional_t_id_t_colon:
             {()}
          |T_ID T_COLON {()}
                     ;

        optional_expression:
             {()}
          |expression {()}
                     ;
        expression:
            T_ID {()}
          | T_LP expression T_RP  {()}
          | T_TRUE {()}| T_FALSE {()} | T_NULL  {()}
          | T_INT  {()}| T_CHAR {()} | T_DOUBLE  {()}| T_STRING  {()}
          | T_ID T_LP T_RP  {()}
          | T_ID T_LP expression_list T_RP {()}
          | expression T_LSB expression T_RSB %prec POSTFIX {()}
          | unary_operator expression %prec UNOP  {()}
          | expression binary_operator expression %prec OPERATOR {()}
          | expression T_COMMA expression %prec OPERATOR {()}
          | unary_assignment expression %prec PREFIX {()}
          | expression unary_assignment %prec POSTFIX  {()}
          | expression binary_assignment expression %prec ASSIGNMENT {()}
          | T_LP type_t T_RP expression %prec CASTING {()}
          | expression T_QUES expression T_COLON expression %prec COMPIF  {()} 
          | T_NEW type_t  {()}
          | T_NEW basic_type star T_TIMES expression {()}
          | T_NEW type_t T_LSB expression T_RSB %prec POSTFIX {()}
          | T_DELETE expression {()}
        ;

expression_list:
	expression  %prec EXPLIST {()}
        |expression_list T_COMMA expression  {()}
	;


constant_expression:
	expression {()}
	;

unary_operator:
	T_AMP  {()}| T_TIMES {()} | T_PLUS  {()}| T_MINUS  {()}| T_EX {()}
	;

binary_operator:
	T_TIMES  {()}| T_DIV  {()}| T_MOD  {()}| T_PLUS  {()}| T_MINUS  {()}| T_BIG  {()}| T_SMALL  {()}| T_BEQ  {()}| T_SEQ  {()}| T_EQ  {()}| T_DIF  {()}| T_AND  {()}| T_OR  {()} /*| T_COMMA {()} removed because of shift/conflict*/
	;

unary_assignment:
	T_PP  {()}| T_MM {()}
	;

binary_assignment:
	T_ASS  {()}| T_TEQ  {()}| T_DEQ  {()}| T_MEQ  {()}| T_PEQ  {()}| T_MIEQ {()};
