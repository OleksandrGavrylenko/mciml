ocamllex tiger.mll
ocamlfind opt tokens.ml tiger.ml -package ppx_deriving.std -linkpkg -o example
./example << EOF
/* Comment */ ThisisanId 0.1234 1234 "string \n with \n newlines "
EOF

rm *.cmx *.cmi *.o
