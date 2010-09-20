all: 
	ocamlbuild -cflags -for-pack,Yaxpo yaxpo.cma yaxpo.cmxa

install: all
	ocamlfind install yaxpo META _build/yaxpo.cma _build/yaxpo.cmxa _build/yaxpo.cmi _build/yaxpo.a

uninstall:
	ocamlfind remove yaxpo

reinstall:
	make uninstall
	make install

clean:
	ocamlbuild -clean

doc:
	ocamlbuild yaxpo.docdir/index.html

