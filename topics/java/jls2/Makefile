all:
	make app
	make doc
	make test

debug:
	python ../../extraction/html2bgf/html2bgf.py app.html app2.bgf -v
	python ../../extraction/html2bgf/getpre.py collect.kw doc.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html doc2.bgf -v

app:
	python ../../extraction/html2bgf/html2bgf.py app.html app2.bgf

doc:
	python ../../extraction/html2bgf/getpre.py collect.kw doc.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html doc2.bgf

test:
	../../../shared/tools/checkxml bgf app2.bgf
	../../../shared/tools/checkxml bgf doc2.bgf

clean:
	rm -f *.bgf parse.html
