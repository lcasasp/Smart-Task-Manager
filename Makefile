.PHONY: test check

build:
	dune build src
utop:
	OCAMLRUNPARAM=b dune utop src
run: 
	dune exec bin/main.exe
test:
	OCAMLRUNPARAM=b dune exec test/main.exe
doc: 
	dune build @doc
opendoc: doc
	@bash opendoc.sh	wack 
zip:
	rm -f TaskManager.zip
	zip -r TaskManager.zip . -x@exclude.lst