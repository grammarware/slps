all:
	make cs.html

cs.1.bgf:
	cp ../../grammars/csharp/ecma-334-1/grammar.bgf cs.1.bgf

cs.2.bgf: cs.1.bgf
	subgrammar cs.1.bgf compilation-unit cs.2.bgf

cs.3.bgf: cs.2.bgf adapt.using.exbgf
	exbgf adapt.using.exbgf cs.2.bgf cs.3.bgf

cs.4.bgf: cs.3.bgf adapt.global.exbgf
	exbgf adapt.global.exbgf cs.3.bgf cs.4.bgf

cs.5.bgf: cs.4.bgf adapt.namespace.exbgf
	exbgf adapt.namespace.exbgf cs.4.bgf cs.5.bgf

cs.6.bgf: cs.5.bgf simplify.namespace.exbgf
	exbgf simplify.namespace.exbgf cs.5.bgf cs.6.bgf

cs.7.bgf: cs.6.bgf skeletonise.exbgf
	exbgf skeletonise.exbgf cs.6.bgf cs.7.bgf

cs.9.bgf: cs.7.bgf
	subgrammar cs.7.bgf compilation-unit cs.8.bgf
	../../mutation/inline-trivial/inline.py cs.8.bgf inline-chains.xbgf
	xbgf inline-chains.xbgf cs.8.bgf cs.9.bgf

cs.a.bgf: cs.9.bgf
	../../mutation/naming/enforce.py l- C! cs.9.bgf enforce-naming.xbgf
	xbgf enforce-naming.xbgf cs.9.bgf cs.a.bgf

cs.adt1.bgf: cs.8.bgf
	../../mutation/anf/anf.py cs.8.bgf cs.anf.xbgf
	xbgf cs.anf.xbgf cs.8.bgf cs.adt1.bgf
	bgf2html cs.adt1.bgf cs1.html

cs.adt2.bgf: cs.7.bgf
	../../mutation/anf/anf.py cs.7.bgf cs.anf.xbgf
	xbgf cs.anf.xbgf cs.7.bgf cs.adt2.bgf
	bgf2html cs.adt2.bgf cs2.html

cs.html: cs.9.bgf
	bgf2html cs.9.bgf cs.html

clean:
	rm -f ~* *.html *.bgf *.xbgf
