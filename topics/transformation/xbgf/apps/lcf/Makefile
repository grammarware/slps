shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd

build: lcf1.bgf

lcf1.bgf: *.xbgf
	${tools}/xsd2bgf ${xsd}/lcf.xsd lcf0.bgf
	${tools}/xbgf lcf1.xbgf lcf0.bgf lcf1.bgf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf lcf1.bgf

clean:
	rm -f *~
	rm -f *.bgf
