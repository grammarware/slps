shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd
final = xbgf3.bgf

build: ${final}

${final}: *.xbgf
	${tools}/xsd2bgf ${xsd}/xbgf.xsd xbgf0.bgf
	${tools}/xbgf xbgf0.bgf rename.xbgf xbgf1.bgf
	${tools}/xbgf xbgf1.bgf normalize.xbgf xbgf2.bgf
	${tools}/xbgf xbgf2.bgf strip.xbgf xbgf3.bgf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf ${final}

clean:
	rm -f *~
	rm -f *.bgf
