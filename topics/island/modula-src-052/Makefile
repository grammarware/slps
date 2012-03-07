all:
	make modula.html

modula.1.bgf:
	cp ../../grammars/modula/src-052/grammar.bgf modula.1.bgf

modula.2.bgf: refactor.exbgf modula.1.bgf
	exbgf refactor.exbgf modula.1.bgf modula.2.bgf

modula.3.bgf: skeletonise.exbgf modula.2.bgf
	exbgf skeletonise.exbgf modula.2.bgf modula.3.bgf

modula.4.bgf: modula.3.bgf
	subgrammar modula.3.bgf Compilation modula.4.bgf

modula.html: modula.4.bgf
	bgf2html modula.4.bgf modula.html

clean:
	rm -f ~* *.html *.bgf refactor.xbgf skeletonise.xbgf
