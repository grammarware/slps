all:
	make impl
	make read
	make test

debug:
	python ../../extraction/html2bgf/html2bgf.py impl.html impl3.bgf -v
	python ../../extraction/html2bgf/getpre.py collect.kw read.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html read3.bgf -v

impl:
	@echo "Extracting impl3" > /dev/stderr
	python ../../extraction/html2bgf/html2bgf.py impl.html impl3.bgf

read:
	@echo "Extracting read3" > /dev/stderr
	python ../../extraction/html2bgf/getpre.py collect.kw read.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html read3.bgf

test:
	../../../shared/tools/checkxml bgf impl3.bgf
	../../../shared/tools/checkxml bgf read3.bgf

clean:
	rm -f *.bgf parse.html
