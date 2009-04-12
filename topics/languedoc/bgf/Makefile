validator = ../../shared/tools/checkxml

build: bgf.ldf
	xsltproc ../../../shared/xsl/ldf2tex.xslt bgf.ldf > bgf.tex
	pdflatex -interaction=batchmode bgf
	pdflatex -interaction=batchmode bgf

bgf.ldf:
	../../../shared/tools/xsd2bgf ../../../shared/xsd/bgf.xsd bgf.bgf
	../../../topics/extraction/xsd2ldf/ldfgen.py ../../../shared/xsd/bgf.xsd bgf.bgf bgf.ldf

rebuild:
	make clean
	make build

clean:
	rm -f ldf*.bgf ldf*.ldf bgf.html bgf.fo bgf.pdf *.xbnf bgf*.bgf
	rm -f *.aux *.log *.toc *.tex printed_for_xldf.tmp

check:
	ls -1 *.xbgf | xargs -n1 ${validator} xbgf
	ls -1 xldf/*.xldf | xargs -n1 ${validator} xldf
	ls -1 *.bgf  | xargs -n1 ${validator} bgf
	ls -1 *.ldf  | xargs -n1 ${validator} ldf
