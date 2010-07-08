validator     = ../../../shared/tools/checkxml
prettyprinter = ../../../shared/tools/bgf2bnf
framework     = ../../../shared/tools/lci

build:

test:
	${framework} java.lcf architecture

check:
	${validator} lcf java.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${validator} bgf

debug:
	${prettyprinter} snapshot/impl1.bgf 1.bnf
	${prettyprinter} snapshot/impl2.bgf 2.bnf
	${prettyprinter} snapshot/impl3.bgf 3.bnf
	${prettyprinter} snapshot/read1.bgf q.bnf
	${prettyprinter} snapshot/read2.bgf w.bnf
	${prettyprinter} snapshot/read3.bgf e.bnf

clean:
	rm -f *~
	rm -f bgf/*
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f java.log
