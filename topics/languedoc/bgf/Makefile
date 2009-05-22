validator = ../../shared/tools/checkxml

build: tex
	pdflatex -interaction=batchmode bgf
	pdflatex -interaction=batchmode bgf

tex:
	../../../shared/tools/xsd2ldf ../../../shared/xsd/bgf.xsd bgf.ldf
	../../../shared/tools/xldf bgf.xldf bgf.ldf bgf.ready.ldf
	../../../shared/tools/ldf2tex bgf.ready.ldf bgf.tex

rebuild:
	make clean
	make build

clean:
	rm -f bgf*.bgf bgf*.ldf bgf.html bgf.fo bgf.pdf
	rm -f *.aux *.log *.toc *.tex printed_for_xldf.tmp

check:
	ls -1 *.xbgf | xargs -n1 ${validator} xbgf
	ls -1 xldf/*.xldf | xargs -n1 ${validator} xldf
	ls -1 *.bgf  | xargs -n1 ${validator} bgf
	ls -1 *.ldf  | xargs -n1 ${validator} ldf
