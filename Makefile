main:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main.byte

pulker:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main.byte && ./main.byte
	
test:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal test.byte && ./test.byte

clean:
	ocamlbuild -clean
