OCB_FLAGS = -tag bin_annot -I src
OCB = ocamlbuild $(OCB_FLAGS)

EXE = dimacs-solver
EXE_BYTE = $(addsuffix .byte, $(EXE))

all: $(EXE_BYTE)

clean:
	$(OCB) -clean

%.byte:
	$(OCB) $@
