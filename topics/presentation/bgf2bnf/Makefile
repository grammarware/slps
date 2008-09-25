all:
	make clean
	make test

build:

test:
	find ../../fl/lci/snapshot/ -name "*.bgf" | xargs -n1 ../../../shared/tools/bgf2bnf
	find ../../java/lci/snapshot/ -name "*.bgf" | xargs -n1 ../../../shared/tools/bgf2bnf

clean:
	rm -f *.bnf
