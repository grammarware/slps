shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd
final = btf6.bgf
bgf = ../bgf

build: ${final}

${final}: *.xbgf
	${tools}/xsd2bgf ${xsd}/btf.xsd btf0.bgf
	${tools}/xbgf btf0.bgf ${bgf}/normalize.xbgf btf1.bgf
	${tools}/xbgf btf1.bgf ${bgf}/rename.xbgf btf2.bgf
	${tools}/xbgf btf2.bgf rename.xbgf btf3.bgf
	${tools}/xbgf btf3.bgf normalize.xbgf btf4.bgf
	${tools}/xbgf btf4.bgf strip.xbgf btf5.bgf
	${tools}/xbgf btf5.bgf reroot.xbgf btf6.bgf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf ${final}

clean:
	rm -f *~
	rm -f *.bgf
