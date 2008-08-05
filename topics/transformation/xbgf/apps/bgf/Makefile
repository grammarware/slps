shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd

build: bgf2.bgf

bgf2.bgf: *.xbgf
	${tools}/xsd2bgf ${xsd}/bgf.xsd bgf0.bgf
	${tools}/xbgf bgf0.bgf bgf1.xbgf bgf1.bgf
	${tools}/xbgf bgf1.bgf bgf2.xbgf bgf2.bgf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf bgf2.bgf

clean:
	rm -f *~
	rm -f *.bgf
