shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd
final = btf3.bgf

build: ${final}

${final}: *.xbgf
	${tools}/xsd2bgf ${xsd}/btf.xsd btf0.bgf
	${tools}/xbgf btf0.bgf rename.xbgf btf1.bgf
	${tools}/xbgf btf1.bgf normalize.xbgf btf2.bgf
	${tools}/xbgf btf2.bgf strip.xbgf btf3.bgf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf ${final}

clean:
	rm -f *~
	rm -f *.bgf
