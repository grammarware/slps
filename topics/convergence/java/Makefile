tools = ../../../shared/tools

build:

test:
	${tools}/lci java.lcf architecture

check:
	${tools}/checkxml lcf java.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${tools}/checkxml xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${tools}/checkxml bgf

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
