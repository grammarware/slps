tools = ../../../shared/tools
validator = ${tools}/checkxml

build:

test:
	${tools}/lci fl.lcf architecture
	@rm -f architecture*.dot

check:
	${validator} lcf fl.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${validator} bgf

clean:
	rm -f *~
	rm -f codesamples/* xmlsamples/* bgf/*
	rm -f xbgf/stripTerminals.xbgf xbgf/stripSelectors.xbgf
	rm -f xbgf/rename-om.xbgf xbgf/rename-sdf.xbgf xbgf/rename-jaxb.xbgf xbgf/rename-xsd.xbgf xbgf/rename-model.xbgf
	rm -f xbgf/reroot-lower.xbgf xbgf/reroot-upper.xbgf
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f fl.log

