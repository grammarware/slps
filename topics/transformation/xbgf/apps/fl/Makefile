shared = ../../../../../shared
fl = ../../../../fl
tools = ${shared}/tools
xsd = ${shared}/xsd
xbgf = ${fl}/lci/xbgf
final = xsd3.bgf

build: ${final}

${final}:
# fl.xsd
	${tools}/xsd2bgf ${fl}/xsd/fl.xsd xsd0.bgf
	${tools}/xbgf xsd0.bgf ${xbgf}/downcase.xbgf xsd1.bgf
	${tools}/xbgf xsd1.bgf ${xbgf}/massageXsd.xbgf xsd2.bgf
	${tools}/xbgf xsd2.bgf ${xbgf}/preferJava.xbgf xsd3.bgf
# factorial.xml
	${tools}/xsd2btf ${fl}/xsd/fl.xsd ${fl}/xsd/factorial.xml xml0.btf
	${tools}/xbtf xml0.btf ${xbgf}/downcase.xbgf xml1.btf
	${tools}/xbtf xml1.btf ${xbgf}/massageXsd.xbgf xml2.btf
	${tools}/xbtf xml2.btf ${xbgf}/preferJava.xbgf xml3.btf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf ${final}

clean:
	rm -f *~
	rm -f *.bgf
	rm -f *.btf
