validator = ../../../shared/tools/checkxml

all:
	make clean
	make test
	make check

test:
	python ../../convergence/lci/lci.py bnf.lcf architecture

check:
	${validator} lcf bnf.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${validator} bgf
	find ../../fl/lci/snapshot/ -name "*.bgf" | xargs -n1 ../../../shared/tools/bgf2bnf > /dev/null
	find ../../java/lci/snapshot/ -name "*.bgf" | xargs -n1 ../../../shared/tools/bgf2bnf > /dev/null

clean:
	rm -f *.bnf bgf/* architecture* xbgf.log

