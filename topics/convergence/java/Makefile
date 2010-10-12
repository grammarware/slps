tools = ../../../shared/tools
validator = ${tools}/validate

build:

test:
	${tools}/lci java.lcf architecture

check:
	${validator} lcf java.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${validator} bgf

debug:
	${tools}/bgf2bnf snapshot/impl1.bgf 1.bnf
	${tools}/bgf2bnf snapshot/impl2.bgf 2.bnf
	${tools}/bgf2bnf snapshot/impl3.bgf 3.bnf
	${tools}/bgf2bnf snapshot/read1.bgf q.bnf
	${tools}/bgf2bnf snapshot/read2.bgf w.bnf
	${tools}/bgf2bnf snapshot/read3.bgf e.bnf

clean:
	rm -f *~
	rm -f bgf/*
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f java.log
