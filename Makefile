FLAGS = -v

all: parser lexer compiler 
	ocamlc -o compiler Ast.cmo Lexer.cmo Parser.cmo Main.cmo

lexer: parser Lexer.mll       # generates lexer.ml
	ocamllex Lexer.mll
	ocamlc -c Parser.mli
	ocamlc -c Lexer.ml

parser: Parser.mly ast  # generates parser.ml and parser.mli
	ocamlyacc $(FLAGS) Parser.mly
	ocamlc -c Parser.mli
	ocamlc -c Parser.ml


compiler: parser ast
	ocamlc -c Parser.mli
	ocamlc -c Main.ml

ast: Ast.mli Ast.ml
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


symbol: identifier error Symbol.ml Symbol.mli
	ocamlc -c Symbol.mli
	ocamlc -c Symbol.ml



type_inference : TypeInference.ml symbol
	ocamlc -c TypeInference.ml


clean: 
	$(RM) *.cmo *.cmx *.o *.cmi Parser.mli  Parser.ml Parser.output Lexer.ml 


