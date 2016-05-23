FLAGS = -v

all: error hashcons id types symbol symbtest typoi scopes parser lexer compiler 
#ocamlc -o compiler typoi.cmo lexer.cmo parser.cmo main.cmo
	ocamlc -o compiler Error.cmo  Hashcons.cmo Identifier.cmo Types.cmo Symbol.cmo str.cma Symbtest.cmo  scopes.cmo  typoi.cmo lexer.cmo parser.cmo  main.cmo
#	ocamlc -o compiler Error.cmo  Hashcons.cmo Identifier.cmo Types.cmo Symbol.cmo str.cma scopes.cmo  typoi.cmo lexer.cmo parser.cmo  main.cmo
lexer: parser lexer.mll       # generates lexer.ml
	ocamllex lexer.mll
	ocamlc -c parser.mli
	ocamlc -c lexer.ml

parser: parser.mly typoi  # generates parser.ml and parser.mli
	ocamlyacc $(FLAGS) parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml


compiler: parser typoi
	ocamlc -c parser.mli
	ocamlc -c main.ml

typoi: typoi.mli typoi.ml 
	ocamlc -c typoi.mli
	ocamlc -c typoi.ml

symbtest : Symbtest.ml
	ocamlc -i Symbtest.ml > Symbtest.mli
	ocamlc -c Symbtest.mli
	ocamlc -c Symbtest.ml

error: Error.mli Error.ml
	ocamlc -c Error.mli
	ocamlc -c Error.ml
hashcons: Hashcons.mli Hashcons.ml
	ocamlc -c Hashcons.mli
	ocamlc -c Hashcons.ml

id: Identifier.mli Identifier.ml
	ocamlc -c Identifier.mli
	ocamlc -c Identifier.ml

types: Types.mli Types.ml
	ocamlc -c Types.mli
	ocamlc -c Types.ml

symbol: Symbol.mli Symbol.ml
	ocamlc -c Symbol.mli
	ocamlc -c Symbol.ml

scopes:  scopes.ml
	ocamlc -i scopes.ml > scopes.mli	
	ocamlc -c scopes.mli
	ocamlc -c scopes.ml

clean: 
	$(RM) *.cmo *.cmx *.o *.cmi parser.mli compiler parser.ml parser.output lexer.ml *.cmi semantic typoi.cmo typoi.cmi 
