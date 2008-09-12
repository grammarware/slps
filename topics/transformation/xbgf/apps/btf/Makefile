shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd
final = btf1.bgf

build: ${final}

append.xbgf:
	cd ../bgf; make append.xbgf
	appendxbgf append.xbgf \
		../bgf/normalize.xbgf \
		../bgf/rename.xbgf \
		rename.xbgf \
		reroot.xbgf \
		normalize.xbgf
#		strip.xbgf

${final}: append.xbgf
	${tools}/xsd2bgf ${xsd}/btf.xsd btf0.bgf
	${tools}/xbgf append.xbgf btf0.bgf btf1.bgf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf ${final}

clean:
	rm -f *~
	rm -f *.bgf
	rm -f append.xbgf
