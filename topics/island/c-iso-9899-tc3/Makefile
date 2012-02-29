all:
	make c.html

c.1.bgf:
	cp ../../grammars/c/iso-9899-tc3/grammar.bgf c.1.bgf

c.2.bgf: refactor.exbgf c.1.bgf
	exbgf2xbgf refactor.exbgf c.1.bgf refactor.xbgf
	xbgf refactor.xbgf c.1.bgf c.2.bgf

c.3.bgf: skeletify.exbgf c.2.bgf
	exbgf2xbgf skeletify.exbgf c.2.bgf skeletify.xbgf
	xbgf skeletify.xbgf c.2.bgf c.3.bgf

c.4.bgf: c.3.bgf
	subgrammar c.3.bgf translation-unit c.4.bgf

c.html: c.4.bgf
	bgf2html c.4.bgf c.html

clean:
	rm -f ~* *.html *.bgf refactor.xbgf skeletify.xbgf
