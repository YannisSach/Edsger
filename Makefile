main: lexer.mll parser.mly main.ml 
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc -v parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c  lexer.ml
	ocamlc -c parser.ml
	ocamlc -c main.ml
	ocamlc -o main parser.cmo lexer.cmo main.cmo

clean: 
	$(RM) *.cmo *.mli main parser.ml parser.output lexer.ml *.cmi 
