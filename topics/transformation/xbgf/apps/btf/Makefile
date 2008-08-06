shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd

build: btf1.bgf

btf1.bgf: *.xbgf
	${tools}/xsd2bgf ${xsd}/btf.xsd btf0.bgf
	${tools}/xbgf btf0.bgf btf1.xbgf btf1.bgf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf btf1.bgf

clean:
	rm -f *~
	rm -f *.bgf
