validator = ../../shared/tools/checkxml

build: ldf.ldf
	xsltproc ../../../shared/xsl/ldf2tex.xslt ldf.ldf > ldf.tex
	pdflatex -interaction=batchmode ldf
	pdflatex -interaction=batchmode ldf

ldf.ldf:
	../../../shared/tools/xsd2bgf ../../../shared/xsd/ldf.xsd ldf.bgf
	../../../topics/extraction/xsd2ldf/ldfgen.py ../../../shared/xsd/ldf.xsd ldf.bgf ldf.ldf

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
