all:
	make app
	make doc
	make test

debug:
	python ../../extraction/html2bgf/html2bgf.py app.html app3.bgf -v
	python ../../extraction/html2bgf/getpre.py collect.kw doc.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html doc3.bgf -v

app:
	@echo "Extracting app3" > /dev/stderr
	python ../../extraction/html2bgf/html2bgf.py app.html app3.bgf

doc:
	@echo "Extracting doc3" > /dev/stderr
	python ../../extraction/html2bgf/getpre.py collect.kw doc.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html doc3.bgf

test:
	../../../shared/tools/checkxml bgf app3.bgf
	../../../shared/tools/checkxml bgf doc3.bgf

clean:
	rm -f *.bgf parse.html
