all:
	make impl
	make read
	make test

debug:
	python ../../extraction/html2bgf/getpre.py syntax.kw impl.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html impl1.bgf -v
	python ../../extraction/html2bgf/getpre.py collect.kw read.html parse.html 
	python ../../extraction/html2bgf/html2bgf.py parse.html read1.bgf -v

impl:
	@echo "Extracting impl1" > /dev/stderr
	python ../../extraction/html2bgf/getpre.py syntax.kw impl.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html impl1.bgf

read:
	@echo "Extracting read1" > /dev/stderr
	python ../../extraction/html2bgf/getpre.py collect.kw read.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html read1.bgf

test:
	../../../shared/tools/checkxml bgf impl1.bgf
	../../../shared/tools/checkxml bgf read1.bgf

clean:
	rm -f *.bgf parse.html
