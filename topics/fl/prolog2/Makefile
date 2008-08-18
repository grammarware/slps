root = ../../..
shared = ${root}/shared
fl = ..
tools = ${shared}/tools
xsd = ${shared}/xsd
xbgf = ${fl}/lci/xbgf
finalxsd = xsd1.bgf
finalxml = xml1.btf

build: ${finalxsd} ${finalxml}

test:
	swipl -q -f TestEvaluator.pro -t main -- 120 ${finalxml} fac 5

append.xbgf:
	appendxbgf append.xbgf \
		${xbgf}/trimXsd.xbgf \
		${xbgf}/downcase.xbgf \
		${xbgf}/massageXsd.xbgf \
		${xbgf}/mkSignature.xbgf

# fl.xsd
${finalxsd}: append.xbgf
	${tools}/xsd2bgf ${fl}/xsd/fl.xsd xsd0.bgf
	${tools}/xbgf xsd0.bgf append.xbgf xsd1.bgf

# factorial.xml
${finalxml}: append.xbgf
	${tools}/xml2btf ${fl}/xsd/fl.xsd ${fl}/xsd/factorial.xml xml0.btf
	${tools}/xbtf xml0.btf append.xbgf xml1.btf

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
	rm -f *.xbgf
