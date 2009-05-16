all:
	make impl
	make read
	make test

debug:
	python ../../extraction/html2bgf/html2bgf.py impl.html impl2.bgf -v
	python ../../extraction/html2bgf/getpre.py collect.kw read.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html read2.bgf -v

impl:
	@echo "Extracting impl2" > /dev/stderr
	python ../../extraction/html2bgf/html2bgf.py impl.html impl2.bgf

read:
	@echo "Extracting read2" > /dev/stderr
	python ../../extraction/html2bgf/getpre.py collect.kw read.html parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html read2.bgf

test:
	../../../shared/tools/checkxml bgf impl2.bgf
	../../../shared/tools/checkxml bgf read2.bgf

clean:
	rm -f *.bgf parse.html
