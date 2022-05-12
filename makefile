build:
	ocamlfind ocamlc -o RSA -package num -linkpkg RSA.ml

setup:
	opam init
	eval `opam env`
	opam switch create 4.14.0
	opam install dune utop ocaml-lsp-server graphics nums