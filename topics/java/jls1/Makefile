all:
	make app
	make doc
	make test

app:
	python ../../extraction/html2bgf/getpre.py syntax.kw app.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html jls1a.bgf

doc:
	python ../../extraction/html2bgf/getpre.py collect.kw doc.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html jls1d.bgf

test:
	../../../shared/tools/checkxml bgf jls1a.bgf
	../../../shared/tools/checkxml bgf jls1d.bgf

clean:
	rm -f *.bgf parse.html
