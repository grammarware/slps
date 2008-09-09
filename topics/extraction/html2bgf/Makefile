build:

test:
	python html2bgf.py ../../java/jls2/syntax.html jls2.bgf
	python html2bgf.py ../../java/jls3/syntax.html jls3.bgf
	../../../shared/tools/checkxml bgf jls2.bgf
	../../../shared/tools/checkxml bgf jls3.bgf

clean:
	rm -f *.bgf
