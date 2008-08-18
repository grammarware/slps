shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd
final = bgf3.bgf

build: ${final}

${final}: *.xbgf
	${tools}/xsd2bgf ${xsd}/bgf.xsd bgf0.bgf
	${tools}/xbgf bgf0.bgf normalize.xbgf bgf1.bgf
	${tools}/xbgf bgf1.bgf rename.xbgf bgf2.bgf
	${tools}/xbgf bgf2.bgf strip.xbgf bgf3.bgf

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
