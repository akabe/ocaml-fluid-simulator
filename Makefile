CMPFLAGS=-annot -bin-annot -g -w A-4-33-41-42-43-34-44-45 -safe-string -strict-sequence -strict-formats
OCAMLC=ocamlc $(CMPFLAGS)
OCAMLOPT=ocamlopt $(CMPFLAGS)
OCAMLDEP=ocamldep -native
OCAMLLEX=ocamllex
OCAMLYACC=ocamlyacc

TARGET=simulate.out
XOBJS=cell.cmx rules.cmx mat.cmx lga.cmx renderer.cmx main.cmx

.PHONY: all clean depend

all: $(TARGET)

$(TARGET): $(XOBJS)
	$(OCAMLOPT) graphics.cmxa $^ -o $@

rules.ml: gen_rules.ml
	ocaml gen_rules.ml > $@

.SUFFIXES: .mly .mll .mli .ml .cmi .cmo .cmx

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

clean:
	rm -f rules.ml *.cm* *.annot *.o *.out

depend: rules.ml
	$(OCAMLDEP) *.mli *.ml > .depend

-include .depend
