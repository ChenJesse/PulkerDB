main:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal repl.byte 
	
test:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal test.byte && ./test.byte

check:
	bash checkenv.sh && bash checktypes.sh

clean:
	ocamlbuild -clean
