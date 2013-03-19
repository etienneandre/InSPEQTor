###############################################################
#
#                    Inspeqtor
#
#  Blublu
#
#  LSV
#  Author: Etienne Andre
#  Created:       2009/04/27
#  Last modified: 2009/04/28
#  Ocaml version: 3.09.3
###############################################################

# CONSTANTS
OCAMLIB_PATH = /usr/lib/ocaml

# FILES
.PREFIXES : +.
.SUFFIXES : .cmo .cmi .ml .mli

FILES =  Global.+ Constraint.+ AbstractStructure.+ InspeqtorLexer.+ InspeqtorParser.+ InspeqtorPrinter.+ INSPEQTOR.+
FILESCMI = Global.cmi Constraint.cmi AbstractStructure.cmi


all:
	make parser
	make compil
	make rmtpf
	make rmuseless


header:
	ocamlc -c Global.mli
	ocamlc -c Global.ml

	ocamlc -c Constraint.mli
	ocamlc -c Constraint.ml
	
	ocamlc -c AbstractStructure.mli
	ocamlc -c AbstractStructure.ml


parser:
	make header
	ocamllex InspeqtorLexer.mll       # generates InspeqtorLexer.ml
	ocamlyacc InspeqtorParser.mly     # generates InspeqtorParser.ml and InspeqtorParser.mli

compil:
	make header

	ocamlc -c InspeqtorParser.mli
	ocamlc -c InspeqtorLexer.ml
	ocamlc -c InspeqtorParser.ml
	ocamlc -c InspeqtorPrinter.mli
	ocamlc -c InspeqtorPrinter.ml
	
	ocamlc -c INSPEQTOR.ml
	ocamlc -I $(OCAMLIB_PATH) nums.cma str.cma unix.cma -o INSPEQTOR $(FILES:+=cmo)

exe:
	make compil
	make rmtpf
	make rmuseless
	./INSPEQTOR

count:
	make clean
	python lineCounter.py

clean:
	make rmtpf
	make rmuseless
	rm -rf InspeqtorLexer.ml InspeqtorParser.ml InspeqtorParser.mli
	rm -rf INSPEQTOR

rmuseless:
	rm -rf $(FILES:+=cmo) $(FILES:+=cmi) $(FILES:+=o)
	rm -rf $(FILESCMI)

rmtpf:
	rm -rf *~


