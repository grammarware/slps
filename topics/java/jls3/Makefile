all:
	python ../../extraction/html2bgf/html2bgf.py syntax.html jls3.bgf
	python ../../extraction/html2bgf/getpre.py collect.kw collected.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html jls3c.bgf
	../../../shared/tools/checkxml bgf jls3.bgf
	../../../shared/tools/checkxml bgf jls3c.bgf

clean:
	rm -f *.bgf parse.html
