validator = ../../../download/msv/msv.jar
schemata = ../../../shared/xsd

build:

test:
	python ../../convergence/lci/lci.py xml fl.lcf architecture

check:
	java -jar ${validator} -warning ${schemata}/lcf.xsd fl.lcf
	ls xbgf/*.xbgf | xargs java -jar ${validator} ${schemata}/xbgf.xsd
	ls bgf/*.bgf | xargs java -jar ${validator} ${schemata}/bgf.xsd

clean:
	rm -f *~
	rm -f bgf/*.bgf
	rm -rf bin
	rm -rf obj
	rm -f *.user
	rm -f *.suo
	rm -f sample*
	rm -f architecture*

