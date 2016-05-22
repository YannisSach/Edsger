FLAGS = -v

all: parser lexer compiler 
	ocamlc -o compiler Error.cmo Hashcons.cmo Identifier.cmo Types.cmo Symbol.cmo str.cma TypeInference.cmo Scopes.cmo Ast.cmo Lexer.cmo Parser.cmo Main.cmo

lexer: parser Lexer.mll       # generates lexer.ml
	ocamllex Lexer.mll
	ocamlc -c Parser.mli
	ocamlc -c Lexer.ml

parser: Parser.mly ast  # generates parser.ml and parser.mli
	ocamlyacc $(FLAGS) Parser.mly
	ocamlc -c Parser.mli
	ocamlc -c Parser.ml


compiler: parser ast scopes
	ocamlc -c Parser.mli
	ocamlc -c Main.ml


symbtest : Symbtest.ml
	ocamlc -i Symbtest.ml > Symbtest.mli
	ocamlc -c Symbtest.mli
	ocamlc -c Symbtest.ml

scopes:  Scopes.ml symbol type_inference
	ocamlc -i Scopes.ml > Scopes.mli	
	ocamlc -c Scopes.mli
	ocamlc -c Scopes.ml

ast: Ast.mli Ast.ml types
	ocamlc -c Ast.mli
	ocamlc -c Ast.ml

types: Types.mli Types.ml
	ocamlc -c Types.mli
	ocamlc -c Types.ml

hashcons: Hashcons.mli Hashcons.ml
	ocamlc -c Hashcons.mli
	ocamlc -c Hashcons.ml

error: Error.ml Error.mli
	ocamlc -c Error.mli
	ocamlc -c Error.ml

identifier: hashcons Identifier.ml Identifier.mli
	ocamlc -c Identifier.mli
	ocamlc -c Identifier.ml


symbol: identifier error types Symbol.ml Symbol.mli
	ocamlc -c Symbol.mli
	ocamlc -c Symbol.ml

type_inference : TypeInference.ml symbol
	ocamlc -c TypeInference.ml


clean: 
	$(RM) *.cmo *.cmx *.o *.cmi Parser.mli  Parser.ml Parser.output Lexer.ml 


