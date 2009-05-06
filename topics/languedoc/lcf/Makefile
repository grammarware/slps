validator = ../../shared/tools/checkxml

build: tex
	pdflatex -interaction=batchmode lcf
	pdflatex -interaction=batchmode lcf

tex:
	../../../shared/tools/xsd2ldf ../../../shared/xsd/lcf.xsd lcf.ldf
	../../../shared/tools/xldf lcf.xldf lcf.ldf lcf.ready.ldf
	../../../shared/tools/ldf2tex lcf.ready.ldf lcf.tex

rebuild:
	make clean
	make build

clean:
	rm -f lcf*.bgf lcf*.ldf lcf.html lcf.fo lcf.pdf *.xbnf xldf-tmp*.*
	rm -f *.aux *.log *.toc *.tex printed_for_xldf.tmp

check:
	ls -1 *.xbgf | xargs -n1 ${validator} xbgf
	ls -1 xldf/*.xldf | xargs -n1 ${validator} xldf
	ls -1 *.bgf  | xargs -n1 ${validator} bgf
	ls -1 *.ldf  | xargs -n1 ${validator} ldf
