OCB_FLAGS = -tag bin_annot -I src
OCB = ocamlbuild $(OCB_FLAGS)

EXE = dimacs-solver
EXE_BYTE = $(addsuffix .byte, $(EXE))

.PHONY: all byte

all: byte

byte:
	$(OCB) $(EXE_BYTE)

clean:
	$(OCB) -clean

