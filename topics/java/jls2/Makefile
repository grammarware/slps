all:
	make app
	make doc
	make test

app:
	python ../../extraction/html2bgf/html2bgf.py app.html jls2a.bgf

doc:
	python ../../extraction/html2bgf/getpre.py collect.kw doc.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html jls2d.bgf

test:
	../../../shared/tools/checkxml bgf jls2a.bgf
	../../../shared/tools/checkxml bgf jls2d.bgf

clean:
	rm -f *.bgf parse.html
