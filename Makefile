# pre req
syntax.cmo:
	ocamlc -c syntax.ml;

reduce.cmo:
	ocamlc -c reduce.ml;

unify.cmo:
	ocamlc -c unify.ml;

typescheme.cmo:
	ocamlc -c typescheme.ml;

poly.cmo: unify.cmo
	ocamlc -c poly.ml;

prog.cmo:
	ocamlc -c prog.ml;

all.cmo:
	ocamlc -c all.ml;

lexer.ml:
	ocamllex lexer.mll

parser.ml:
	ocamlyacc parser.mly

parser.cmi: syntax.cmo parser.ml
	ocamlc -c parser.mli syntax.cmo

lexer.cmo: lexer.ml
	ocamlc -c lexer.ml

parser.cmo: parser.ml
	ocamlc -c parser.ml

compiler.cmo:
	ocamlc -c compiler.ml

#main
clean: 
	rm all;	rm compiler; rm *.cmo; rm *.cmi; rm parser.ml; rm parser.mli; rm lexer.ml;

body: syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prog.cmo all.cmo
	ocamlc -g -o all syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prog.cmo all.cmo


compiler: syntax.cmo reduce.cmo parser.cmi poly.cmo lexer.cmo parser.cmo compiler.cmo
	ocamlc -g -o compiler syntax.cmo reduce.cmo unify.cmo poly.cmo lexer.cmo parser.cmo compiler.cmo 

all: body compiler
	

#all: syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prog.cmo all.cmo
#	ocamlc -g -o all syntax.cmo reduce.cmo unify.cmo typescheme.cmo poly.cmo prog.cmo all.cmo

