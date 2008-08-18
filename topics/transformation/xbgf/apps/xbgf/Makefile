shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd
final = xbgf6.bgf
bgf = ../bgf

build: ${final}

${final}: *.xbgf
	${tools}/xsd2bgf ${xsd}/xbgf.xsd xbgf0.bgf
	${tools}/xbgf xbgf0.bgf ${bgf}/normalize.xbgf xbgf1.bgf
	${tools}/xbgf xbgf1.bgf ${bgf}/rename.xbgf xbgf2.bgf
	${tools}/xbgf xbgf2.bgf rename.xbgf xbgf3.bgf
	${tools}/xbgf xbgf3.bgf reroot.xbgf xbgf4.bgf
	${tools}/xbgf xbgf4.bgf normalize.xbgf xbgf5.bgf
	${tools}/xbgf xbgf5.bgf strip.xbgf xbgf6.bgf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf ${final}

clean:
	rm -f *~
	rm -f *.bgf
