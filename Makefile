client.native: .FORCE
	ocamlbuild -use-ocamlfind src/client.native

.FORCE:

clean:
	rm -Rf _build/ *.native

