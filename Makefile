SRC_FILES = $(wildcard *.ml*)

test:
	ocamlbuild -lflags "-warn-error +a" -cflags "-warn-error +a" -pkgs oUnit,yojson test_main.byte ; ./test_main.byte

clean:
	ocamlbuild -clean
