client.native: .FORCE
	ocamlbuild examples/client.native

.FORCE:

clean:
	rm -Rf _build/ *.native

