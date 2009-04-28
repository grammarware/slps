validator = ../../../shared/tools/checkxml

build:

test:
	../../../shared/tools/lci java.lcf architecture

check:
	${validator} lcf java.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${validator} bgf

debug:
	../../../shared/tools/bgf2bnf snapshot/app1.bgf 1.bnf
	../../../shared/tools/bgf2bnf snapshot/app2.bgf 2.bnf
	../../../shared/tools/bgf2bnf snapshot/app3.bgf 3.bnf
	../../../shared/tools/bgf2bnf snapshot/doc1.bgf q.bnf
	../../../shared/tools/bgf2bnf snapshot/doc2.bgf w.bnf
	../../../shared/tools/bgf2bnf snapshot/doc3.bgf e.bnf

clean:
	rm -f *~
	rm -f bgf/*
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f java.log

