validator = ../../../shared/tools/validate

all:
	make clean
	make test
	make check

test:
	../../../shared/tools/lci bnf.lcf architecture

check:
	${validator} lcf bnf.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  */*.bgf  | xargs -n1 ${validator} bgf

clean:
	rm -f *.bnf bgf/* architecture* xbgf.log

