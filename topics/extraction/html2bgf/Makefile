build:

test:
	@python getpre.py ../../java/jls1/syntax.kw ../../java/jls1/syntax.html parse.html
	@python html2bgf.py parse.html jls1.bgf | grep "problems encountered"
	@python getpre.py ../../java/jls1/collect.kw ../../java/jls1/collected.html parse.html
	@python html2bgf.py parse.html jls1c.bgf | grep "problems encountered"
	@python html2bgf.py ../../java/jls2/syntax.html jls2.bgf | grep "problems encountered"
	@python getpre.py ../../java/jls2/collect.kw ../../java/jls2/collected.html parse.html
	@python html2bgf.py parse.html jls2c.bgf | grep "problems encountered"
	@python html2bgf.py ../../java/jls3/syntax.html jls3.bgf | grep "problems encountered"
	@python getpre.py ../../java/jls3/collect.kw ../../java/jls3/collected.html parse.html
	@python html2bgf.py parse.html jls3c.bgf | grep "problems encountered"
	@../../../shared/tools/checkxml bgf jls1.bgf
	@../../../shared/tools/checkxml bgf jls2.bgf
	@../../../shared/tools/checkxml bgf jls3.bgf
	@../../../shared/tools/checkxml bgf jls1c.bgf
	@../../../shared/tools/checkxml bgf jls2c.bgf
	@../../../shared/tools/checkxml bgf jls3c.bgf

clean:
	rm -f *.bgf parse.html
