# The parser
CAMLYACC=ocamlyacc
CAMLLEX=ocamllex
CAMLC=ocamlc
CAMLBUILD=ocamlbuild

INCLUDES=-I parsing -I coq

all: main.byte

main.byte:
	$(CAMLBUILD) $(INCLUDES) main.byte


.PHONY: all main.byte
