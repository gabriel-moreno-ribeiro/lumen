# Builds with the plain OCaml compiler; no opam packages needed.
OCAMLOPT ?= ocamlopt
SRC = src/ast.ml src/lexer.ml src/parser.ml src/str_split.ml src/unix_time.ml src/interp.ml
FLAGS = -O3 -I src -w +a-4-9-40-41-42-44-45-70

.PHONY: all test clean examples

all: bin/lumen

bin/lumen: $(SRC) src/main.ml
	@mkdir -p bin
	$(OCAMLOPT) $(FLAGS) -o bin/lumen $(SRC) src/main.ml

bin/test: $(SRC) test/test.ml
	@mkdir -p bin
	$(OCAMLOPT) $(FLAGS) -o bin/test $(SRC) test/test.ml

test: bin/test
	./bin/test

examples: bin/lumen
	@for f in examples/*.lm; do echo "== $$f"; ./bin/lumen $$f; done

clean:
	rm -rf bin src/*.cm* src/*.o test/*.cm* test/*.o
