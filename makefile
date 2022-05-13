build:
	ocamlfind ocamlc -o RSA -package num,camlimages,camlimages.jpeg,camlimages.graphics,graphics -linkpkg RSA.ml

setup:
	opam init
	eval `opam env`
	opam switch create 4.14.0
	opam install dune utop ocaml-lsp-server graphics num