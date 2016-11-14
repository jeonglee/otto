SRC_FILES = $(wildcard *.ml*)

test:
	ocamlbuild -lflags "-warn-error +a" -cflags "-warn-error +a -thread" -libs threads -pkgs oUnit,yojson,ZMQ,unix,threads test_main.byte ; ./test_main.byte

clean:
	ocamlbuild -clean
