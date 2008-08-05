shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd

build: xbgf2.bgf

xbgf2.bgf: *.xbgf
	${tools}/xsd2bgf ${xsd}/xbgf.xsd xbgf0.bgf
	${tools}/xbgf xbgf0.bgf xbgf1.xbgf xbgf1.bgf
	${tools}/xbgf xbgf1.bgf xbgf2.xbgf xbgf2.bgf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf xbgf2.bgf

clean:
	rm -f *~
	rm -f *.bgf
