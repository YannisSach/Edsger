all: lexer.mll parser.mly main.ml 
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc -v parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c  lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o main lexer.cmo parser.cmo main.cmo

sem: semantic.ml
	ocamlopt -o semantic semantic.ml

clean: 
	$(RM) *.cmo *.cmx *.o *.cmi *.mli main parser.ml parser.output lexer.ml *.cmi semantic
