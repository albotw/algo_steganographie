build:
	ocamlfind ocamlc -o test -package num -linkpkg test.ml

test:
	./test > out