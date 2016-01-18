ocamlbuild -clean
ocamlbuild -use-menhir -use-ocamlfind -pkg core -pkg ppx_deriving.std -tag thread semant.native
./semant.native ../testprogs/typeck.tig
