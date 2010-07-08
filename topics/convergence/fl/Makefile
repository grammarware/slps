tools = ../../../shared/tools

build:

test:
	${tools}/lci fl.lcf architecture

check:
	${tools}/checkxml lcf fl.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${tools}/checkxml xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${tools}/checkxml bgf

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

