all:
	python ../../extraction/html2bgf/html2bgf.py app.html jls2a.bgf
	python ../../extraction/html2bgf/getpre.py collect.kw doc.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html jls2d.bgf
	../../../shared/tools/checkxml bgf jls2a.bgf
	../../../shared/tools/checkxml bgf jls2d.bgf

clean:
	rm -f *.bgf parse.html
