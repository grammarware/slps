validator = ../../../shared/tools/checkxml

build:

test:
	python ../../convergence/lci/lci.py fl.lcf architecture
	@rm -f architecture*.dot

check:
	${validator} lcf fl.lcf
	ls -1 xbgf/*.xbgf | xargs -1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -1 ${validator} bgf

clean:
	rm -f *~
	rm -f testset/* bgf/*
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f fl.log

