all:
	python ../../extraction/html2bgf/html2bgf.py app.html jls3a.bgf
	python ../../extraction/html2bgf/getpre.py collect.kw doc.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html jls3d.bgf
	../../../shared/tools/checkxml bgf jls3a.bgf
	../../../shared/tools/checkxml bgf jls3d.bgf

clean:
	rm -f *.bgf parse.html
