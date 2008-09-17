include ../include.mk

tools = ../../../shared/tools
xbgf = ../lci/xbgf

build: limit.bgf limit.btf

test:
	swipl -q -f TestEvaluator.pro -t main -- 120 limit.btf fac 5

append.xbgf:
	${tools}/appendxbgf append.xbgf \
		${xbgf}/trimXsd.xbgf \
		${xbgf}/renameXsd.xbgf \
		${xbgf}/massageXsd.xbgf \
		${xbgf}/mkSignature.xbgf
	${tools}/checkxml xbgf append.xbgf

limit.bgf: append.xbgf
	${tools}/xsd2bgf ${schema} source.bgf
	${tools}/checkxml bgf source.bgf
	${tools}/xbgf append.xbgf source.bgf limit.bgf
	${tools}/checkxml bgf limit.bgf

limit.btf: append.xbgf
	${tools}/xml2btf ${schema} ${xfactorial} source.btf
	${tools}/checkbtf source.btf
	${tools}/xbtf append.xbgf source.btf limit.btf
	${tools}/checkbtf limit.btf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf limit.bgf
	${tools}/showpnf limit.btf

clean:
	rm -f *~
	rm -f *.bgf
	rm -f *.btf
	rm -f *.xbgf

