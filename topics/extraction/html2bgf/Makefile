build:

test:
	@python getpre.py ../../java/jls1/syntax.kw ../../java/jls1/app.html parse.html
	@python html2bgf.py parse.html jls1.bgf | grep "problems encountered"
	@python getpre.py ../../java/jls1/collect.kw ../../java/jls1/doc.html parse.html
	@python html2bgf.py parse.html jls1c.bgf | grep "problems encountered"
	@python html2bgf.py ../../java/jls2/app.html jls2.bgf | grep "problems encountered"
	@python getpre.py ../../java/jls2/collect.kw ../../java/jls2/doc.html parse.html
	@python html2bgf.py parse.html jls2c.bgf | grep "problems encountered"
	@python html2bgf.py ../../java/jls3/app.html jls3.bgf | grep "problems encountered"
	@python getpre.py ../../java/jls3/collect.kw ../../java/jls3/doc.html parse.html
	@python html2bgf.py parse.html jls3c.bgf | grep "problems encountered"
	@../../../shared/tools/checkxml bgf jls1.bgf
	@../../../shared/tools/checkxml bgf jls2.bgf
	@../../../shared/tools/checkxml bgf jls3.bgf
	@../../../shared/tools/checkxml bgf jls1c.bgf
	@../../../shared/tools/checkxml bgf jls2c.bgf
	@../../../shared/tools/checkxml bgf jls3c.bgf

compare:
	../../../shared/tools/gdt jls1.bgf  ../../java/lci/snapshot/jls1app.bgf
	../../../shared/tools/gdt jls2.bgf  ../../java/lci/snapshot/jls2app.bgf
	../../../shared/tools/gdt jls3.bgf  ../../java/lci/snapshot/jls3app.bgf
	../../../shared/tools/gdt jls1c.bgf ../../java/lci/snapshot/jls1doc.bgf
	../../../shared/tools/gdt jls2c.bgf ../../java/lci/snapshot/jls2doc.bgf
	../../../shared/tools/gdt jls3c.bgf ../../java/lci/snapshot/jls3doc.bgf
clean:
	rm -f *.bgf parse.html
