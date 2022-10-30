all : aritha rapport.pdf

aritha : x86_64.ml x86_64.mli lexer.mll parser.mly compiler.ml main.ml asyntax.ml
	ocamllex lexer.mll
	ocamlyacc parser.mly
	ocamlopt x86_64.mli x86_64.ml asyntax.ml parser.mli parser.ml lexer.ml compiler.ml main.ml -o aritha

rapport.pdf : rapport.tex
	tex rapport.tex

expr : expression.exp
	nano expression.exp

test : aritha expression.exp
	./aritha expression.exp
	gcc -no-pie test.s -o test
	./test

clean:
	rm -rf aritha parser.ml parser.mli lexer.ml *.o *.cmo *.cmx *.cmi *.cmo *~
