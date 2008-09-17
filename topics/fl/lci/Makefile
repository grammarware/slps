tools = ../../../shared/tools
validator = ${tools}/checkxml

build:

test:
	python ../../convergence/lci/lci.py fl.lcf architecture
	@rm -f architecture*.dot

check:
	${validator} lcf fl.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${validator} bgf

generate:
	${tools}/casexbgf 3 bgf/jaxb.trimJaxb.bgf xbgf/renameJaxb.xbgf	
	${tools}/casexbgf 3 bgf/om.trimOm.bgf xbgf/renameOm.xbgf	
	${tools}/casexbgf 3 bgf/sdf.bgf xbgf/renameSdf.xbgf	
	${tools}/casexbgf 3 bgf/xsd.trimXsd.bgf xbgf/renameXsd.xbgf	

clean:
	rm -f *~
	rm -f codesamples/* xmlsamples/* bgf/*
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f fl.log

