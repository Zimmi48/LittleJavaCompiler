CMO=ast.cmo lexer.cmo parser.cmo typeClass.cmo type.cmo mips.cmo compile.cmo main.cmo
GENERATED=lexer.ml parser.ml parser.mli
BIN=pjava
FLAGS=

all: $(BIN)

$(BIN):$(CMO)
	ocamlc $(FLAGS) -o $(BIN) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly  

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c  $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir -v --infer $<

#.mly.mli:
#	ocamlyacc -v $<

clean:
	rm -f *.cm[io] *.o *~ *.annot *.conflicts *.output *.automaton $(BIN) $(GENERATED)

.depend depend:ast.cmo $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend


