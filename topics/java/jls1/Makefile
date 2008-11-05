all:
	make app
	make doc
	make test

debug:
	python ../../extraction/html2bgf/getpre.py syntax.kw app.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html app1.bgf -v
	python ../../extraction/html2bgf/getpre.py collect.kw doc.html parse.html 
	python ../../extraction/html2bgf/html2bgf.py parse.html doc1.bgf -v

app:
	@echo "Extracting app1" > /dev/stderr
	python ../../extraction/html2bgf/getpre.py syntax.kw app.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html app1.bgf

doc:
	@echo "Extracting doc1" > /dev/stderr
	python ../../extraction/html2bgf/getpre.py collect.kw doc.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html doc1.bgf

test:
	../../../shared/tools/checkxml bgf app1.bgf
	../../../shared/tools/checkxml bgf doc1.bgf

clean:
	rm -f *.bgf parse.html
