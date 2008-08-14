root = ../../..
shared = ${root}/shared
fl = ..
tools = ${shared}/tools
xsd = ${shared}/xsd
xbgf = ${fl}/lci/xbgf
finalxsd = xsd4.bgf
finalxml = xml4.btf

build: ${finalxsd} ${finalxml}

test:
	swipl -q -f TestEvaluator.pro -t main -- 120 ${finalxml} fac 5

# fl.xsd
${finalxsd}:
	${tools}/xsd2bgf ${fl}/xsd/fl.xsd xsd0.bgf
	${tools}/xbgf xsd0.bgf ${xbgf}/trimXsd.xbgf xsd1.bgf
	${tools}/xbgf xsd1.bgf ${xbgf}/downcase.xbgf xsd2.bgf
	${tools}/xbgf xsd2.bgf ${xbgf}/massageXsd.xbgf xsd3.bgf
	${tools}/xbgf xsd3.bgf ${xbgf}/mkSignature.xbgf xsd4.bgf

# factorial.xml
${finalxml}:
	${tools}/xsd2btf ${fl}/xsd/fl.xsd ${fl}/xsd/factorial.xml xml0.btf
	${tools}/xbtf xml0.btf ${xbgf}/trimXsd.xbgf xml1.btf
	${tools}/xbtf xml1.btf ${xbgf}/downcase.xbgf xml2.btf
	${tools}/xbtf xml2.btf ${xbgf}/massageXsd.xbgf xml3.btf
	${tools}/xbtf xml3.btf ${xbgf}/mkSignature.xbgf xml4.btf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf ${finalxsd}
	${tools}/showbtf ${finalxml}

clean:
	rm -f *~
	rm -f *.bgf
	rm -f *.btf
