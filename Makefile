cartpole.native: .FORCE
	ocamlbuild examples/cartpole.native

.FORCE:

clean:
	rm -Rf _build/ *.native

