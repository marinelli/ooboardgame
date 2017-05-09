
OCAML = ocamlopt
LIBS = str.cmxa
MODS = ooboardgame.ml

PROG = play


.PHONY: all
all: $(PROG)


$(PROG): $(MODS)
	$(OCAML) -w +a -o $(PROG) $(LIBS) $(MODS)


.PHONY: clean
clean:
	rm *.o *.cmx *.cmi $(PROG)

