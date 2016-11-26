SRC_FILES = $(wildcard *.ml*)

test:
	ocamlbuild -lflags "-warn-error +a" -cflags "-warn-error +a -thread" -libs threads -pkgs oUnit,yojson,str,ZMQ,unix,threads,base64 test_main.byte ; ./test_main.byte

clean:
	ocamlbuild -clean
