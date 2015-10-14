OCB = ocamlbuild $(OCB_FLAGS)
OCB_FLAGS = -r -use-ocamlfind -pkgs str -tag bin_annot -tag debug -I src

EXE = dimacs-solver
EXE_BYTE = $(addsuffix .byte, $(EXE))

.PHONY: all byte

all: byte

byte:
	$(OCB) $(EXE_BYTE)

clean:
	$(OCB) -clean

