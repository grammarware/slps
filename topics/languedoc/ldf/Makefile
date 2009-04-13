validator = ../../shared/tools/checkxml

build: tex
	pdflatex -interaction=batchmode ldf
	pdflatex -interaction=batchmode ldf

tex:
	../../../shared/tools/xsd2ldf ../../../shared/xsd/ldf.xsd ldf.ldf
	../../../shared/tools/xldf ldf.xldf ldf.ldf ldf.ready.ldf
	../../../shared/tools/ldf2tex ldf.ready.ldf ldf.tex

rebuild:
	make clean
	make build

clean:
	rm -f ldf*.bgf ldf*.ldf ldf.html ldf.fo ldf.pdf *.xbnf bgf*.bgf
	rm -f *.aux *.log *.toc *.tex printed_for_xldf.tmp

check:
	ls -1 *.xbgf | xargs -n1 ${validator} xbgf
	ls -1 xldf/*.xldf | xargs -n1 ${validator} xldf
	ls -1 *.bgf  | xargs -n1 ${validator} bgf
	ls -1 *.ldf  | xargs -n1 ${validator} ldf
