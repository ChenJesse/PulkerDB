main:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main.byte

play:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal main.byte && ./main.byte
	
test:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal test.byte && ./test.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean
