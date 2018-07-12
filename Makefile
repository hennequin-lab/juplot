byte:
	ocamlbuild -use-ocamlfind juplot.cmo

install:
	ocamlfind remove juplot
	ocamlfind install juplot META _build/juplot.cmi _build/juplot.cmo

uninstall:
	ocamlfind remove juplot

clean:
	ocamlbuild -clean

all: clean byte
