shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd
final = bgf1.bgf

build: ${final}

append.xbgf:
	${tools}/appendxbgf append.xbgf \
		normalize.xbgf \
		rename.xbgf \
		strip.xbgf \
		designate.xbgf \
		reroot.xbgf

${final}: append.xbgf
	${tools}/xsd2bgf ${xsd}/bgf.xsd bgf0.bgf
	${tools}/xbgf append.xbgf bgf0.bgf bgf1.bgf

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
