all:
	make app
	make doc
	make test

app:
	python ../../extraction/html2bgf/getpre.py syntax.kw app.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html app1.bgf

doc:
	python ../../extraction/html2bgf/getpre.py collect.kw doc.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html doc1.bgf

test:
	../../../shared/tools/checkxml bgf app1.bgf
	../../../shared/tools/checkxml bgf doc1.bgf

clean:
	rm -f *.bgf parse.html
