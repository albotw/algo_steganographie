all:
	ocamlfind ocamlc -o RSA -package num,camlimages,camlimages.jpeg,camlimages.graphics,graphics -linkpkg RSA.ml

clean:
	rm RSA.cmi
	rm RSA.cmo
	rm RSA
	rm mario_res.bmp

setup:
	opam init
	eval `opam env`
	opam switch create 4.14.0
	eval `opam env`

install-libs:
	opam install dune utop ocaml-lsp-server graphics num camlimages