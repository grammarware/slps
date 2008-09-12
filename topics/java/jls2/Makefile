all:
	python ../../extraction/html2bgf/html2bgf.py syntax.html jls2.bgf
	python ../../extraction/html2bgf/getpre.py collect.kw collected.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html jls2c.bgf
	../../../shared/tools/checkxml bgf jls2.bgf
	../../../shared/tools/checkxml bgf jls2c.bgf

clean:
	rm -f *.bgf parse.html
