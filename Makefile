# DO NOT EDIT THIS FILE

.PHONY: test check

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/suite.exe

game: 
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean
	rm -f 2048.zip

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh

zip:
	rm -f 2048.zip
	zip -r 2048.zip . -x@exclude.lst

count:
	dune clean
	cloc --by-file --include-lang=OCaml .

# clean:
# 	dune clean
# 	rm -f dna.zip
