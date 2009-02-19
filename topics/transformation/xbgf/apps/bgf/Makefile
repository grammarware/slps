shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd
final = bgf1.bgf

build: ${final}

append.xbgf:
	${tools}/appendxbgf append1.xbgf \
		normalize.xbgf \
		rename.xbgf
	${tools}/appendxbgf append2.xbgf \
		designate.xbgf \
		reroot.xbgf

${final}: append.xbgf
	${tools}/xsd2bgf ${xsd}/bgf.xsd bgf0.bgf
	${tools}/xbgf append1.xbgf bgf0.bgf bgf0s.bgf
	${tools}/stripsxbgf bgf0s.bgf strip.xbgf
	${tools}/xbgf strip.xbgf bgf0s.bgf bgf1_.bgf
	${tools}/xbgf append2.xbgf bgf1_.bgf bgf1.bgf

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
	rm -f append.xbgf
