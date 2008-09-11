all:
	python ../../extraction/html2bgf/xpathpre.py syntax.kw syntax.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html jls1.bgf
	python ../../extraction/html2bgf/xpathpre.py collect.kw collected.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html jls1c.bgf
	../../../shared/tools/checkxml bgf jls1.bgf
	../../../shared/tools/checkxml bgf jls1c.bgf

clean:
	rm -f *.bgf parse.html
