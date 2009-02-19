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
		inline.xbgf \
		rename.xbgf \
		reroot.xbgf \
		unchainF.xbgf \
		pnfF.xbgf

${final}: append.xbgf
	${tools}/xsd2bgf ${xsd}/xbgf.xsd xbgf0.bgf
	${tools}/xbgf append.xbgf xbgf0.bgf xbgf0s.bgf
	${tools}/stripsxbgf xbgf0s.bgf strip.xbgf
	${tools}/xbgf strip.xbgf xbgf0s.bgf xbgf1_.bgf
	${tools}/xbgf ../bgf/designate.xbgf xbgf1_.bgf xbgf1.bgf

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
	rm -f append.xbgf strip.xbgf
