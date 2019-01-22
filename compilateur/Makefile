# Inspiré d'un template de makefile
# de Pierre Weis, projet Cristal, INRIA Rocquencourt


# Usage of this Makefile:
# To incrementally recompile the system, type
#     make
# To recompute dependancies between modules, type
#     make depend
# To remove the executable and all the compiled files, type
#     make clean
# To compile using the native code compiler
#     make opt



# User variables
################

# The Caml sources (including camlyacc and camllex source files)

SOURCES = ast.ml compiler.ml lexer.mll typer.ml parser.mly main.ml

# The executable file to generate

EXEC = compiler


# Advanced user variables
#########################

# The Caml compilers.
# You may fix here the path to access the Caml compiler on your machine
# You may also have to add various -I options.
CAMLC = ocamlfind ocamlc -package core -linkpkg -thread
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLLEX = ocamllex
CAMLYACC = menhir


# Compilation
#############

all: depend $(EXEC)

opt : $(EXEC).opt

SOURCES1 = $(SOURCES:.mly=.ml)
SOURCES2 = $(SOURCES1:.mll=.ml)
OBJS = $(SOURCES2:.ml=.cmo)
OPTOBJS = $(SOURCES2:.ml=.cmx)

$(EXEC): $(OBJS)
	$(CAMLC) -o $(EXEC) $(OBJS)

$(EXEC).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(EXEC) $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

.mll.cmo:
	$(CAMLLEX) $<
	$(CAMLC) -c $*.ml

.mll.cmx:
	$(CAMLLEX) $<
	$(CAMLOPT) -c $*.ml

.mly.cmo:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli
	$(CAMLC) -c $*.ml

.mly.cmx:
	$(CAMLYACC) $<
	$(CAMLOPT) -c $*.mli
	$(CAMLOPT) -c $*.ml

.mly.cmi:
	$(CAMLYACC) $<
	$(CAMLC) -c $*.mli

.mll.ml:
	$(CAMLLEX) $<

.mly.ml:
	$(CAMLYACC) $<

clean:
	rm parser.ml
	rm parser.mli
	rm lexer.ml
	rm -f *.cm[iox] *~ .*~ #*#
	rm -f $(EXEC)
	rm -f $(EXEC).opt

depend: $(SOURCES2)
	$(CAMLDEP) *.mli *.ml > .depend

include .depend
