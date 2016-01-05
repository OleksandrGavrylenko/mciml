ocamlbuild -use-menhir -use-ocamlfind -pkg core -pkg ppx_deriving.std -tag thread tigparse.native
./tigparse.native ../testprogs/queens.tig
