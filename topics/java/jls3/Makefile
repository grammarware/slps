all:
	make app
	make doc
	make test

app:
	python ../../extraction/html2bgf/html2bgf.py app.html jls3a.bgf

doc:
	python ../../extraction/html2bgf/getpre.py collect.kw doc.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html jls3d.bgf

test:
	../../../shared/tools/checkxml bgf jls3a.bgf
	../../../shared/tools/checkxml bgf jls3d.bgf

clean:
	rm -f *.bgf parse.html
