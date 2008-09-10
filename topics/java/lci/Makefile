validator = ../../../shared/tools/checkxml

build:

diff: test
	gdt bgf/jls2.prefer3.mkSignature.addFeatures2to3.bgf bgf/jls3.fixErrors3.mkSignature.make3converge.bgf

test:
	python ../../convergence/lci/lci.py java.lcf architecture
	@rm -f architecture*.dot

check:
	${validator} lcf java.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${validator} bgf

clean:
	rm -f *~
	rm -f bgf/*
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f java.log

