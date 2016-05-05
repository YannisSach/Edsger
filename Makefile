FLAGS = -v

all: parser lexer compiler 
	ocamlc -o compiler lexer.cmo parser.cmo main.cmo

lexer: parser lexer.mll       # generates lexer.ml
	ocamllex lexer.mll
	ocamlc -c parser.mli
	ocamlc -c lexer.ml

parser: parser.mly semantic  # generates parser.ml and parser.mli
	ocamlyacc $(FLAGS) parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml


compiler: parser semantic
	ocamlc -c parser.mli
	ocamlc -c main.ml

semantic: semantic.ml 
	ocamlc -c semantic.mli

clean: 
	$(RM) *.cmo *.cmx *.o *.cmi parser.mli compiler parser.ml parser.output lexer.ml *.cmi semantic
