shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd
final = bgf1.bgf

build: ${final}

append.xbgf:
	appendxbgf append.xbgf \
		normalize.xbgf \
		rename.xbgf \
		strip.xbgf

${final}: append.xbgf
	${tools}/xsd2bgf ${xsd}/bgf.xsd bgf0.bgf
	${tools}/xbgf bgf0.bgf append.xbgf bgf1.bgf

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
