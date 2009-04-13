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
	rm -f codesamples/* xmlsamples/* bgf/* xbgf/stripTerminals.xbgf xbgf/stripSelectors.xbgf
	rm -f xbgf/renameOm.xbgf xbgf/renameSdf.xbgf xbgf/renameJaxb.xbgf xbgf/renameXsd.xbgf
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f fl.log

