schemata = ../../../shared/xsd

build:

test:
	python ../../convergence/lci/lci.py fl.lcf architecture
	@rm -f architecture*.dot
	@mv *.bgf bgf
	@mv sample* testset

check:
	xmllint --noout --schema ${schemata}/lcf.xsd fl.lcf
	ls xbgf/*.xbgf | xargs xmllint --noout --schema ${schemata}/xbgf.xsd
	ls bgf/*.bgf | xargs xmllint --noout --schema ${schemata}/bgf.xsd

clean:
	rm -f *~
	rm -f sample* testset/* *.bgf bgf/*.bgf
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f fl.log

