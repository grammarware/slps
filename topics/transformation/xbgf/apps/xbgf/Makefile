shared = ../../../../../shared
tools = ${shared}/tools
xsd = ${shared}/xsd
final = xbgf1.bgf

build: ${final}

append.xbgf:
	cd ../bgf; make append.xbgf
	${tools}/appendxbgf append.xbgf \
		../bgf/normalize.xbgf \
		../bgf/rename.xbgf \
		rename.xbgf \
		reroot.xbgf \
		unchainF.xbgf \
		pnfF.xbgf \
                strip.xbgf \
		../bgf/designate.xbgf

${final}: append.xbgf
	${tools}/xsd2bgf ${xsd}/xbgf.xsd xbgf0.bgf
	${tools}/xbgf append.xbgf xbgf0.bgf xbgf1.bgf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf ${final}

clean:
	rm -f *~
	rm -f *.log
	rm -f *.bgf
	rm -f append.xbgf
