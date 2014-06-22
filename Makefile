# The parser
CAMLYACC=ocamlyacc
CAMLLEX=ocamllex
CAMLC=ocamlc
CAMLBUILD=ocamlbuild

INCLUDES=-I parsing -I coq

all: main.byte

main.byte:
	$(CAMLBUILD) $(INCLUDES) main.byte

tests:
	./tests/run_tests.py


.PHONY: all main.byte tests
