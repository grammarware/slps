all:
	make fl.html

fl.1.bgf:
	rsc2bgf ../../fl/rascal1/FL.rsc fl.1.bgf

fl.2.bgf: fl.1.bgf
	../../../shared/generators/reroot2top fl.1.bgf reroot.xbgf
	xbgf reroot.xbgf fl.1.bgf fl.2.bgf

fl.3.bgf: refactor.xbgf fl.2.bgf
	xbgf refactor.xbgf fl.2.bgf fl.3.bgf

fl.4.bgf: skeletify.xbgf fl.3.bgf
	xbgf skeletify.xbgf fl.3.bgf fl.4.bgf

fl.5.bgf: fl.4.bgf
	subgrammar fl.4.bgf Program fl.5.bgf

fl.html: fl.5.bgf
	bgf2html fl.5.bgf fl.html

clean:
	rm -f ~* *.html *.bgf reroot.xbgf