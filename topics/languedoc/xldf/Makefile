validator = ../../shared/tools/checkxml

build: xldf.ldf
	../../../shared/tools/xsd2bgf ../../../shared/xsd/xldf.xsd xldf.bgf
	../../../topics/extraction/xsd2ldf/ldfgen.py ../../../shared/xsd/xldf.xsd xldf.bgf xldf.ldf
	../../../shared/tools/xldf xldf.xldf xldf.ldf xldf_ready.ldf
	xsltproc ../../../shared/xsl/ldf2tex.xslt xldf_ready.ldf > xldf.tex
	pdflatex -interaction=batchmode xldf
	pdflatex -interaction=batchmode xldf

rebuild:
	make clean
	make build

clean:
	rm -f xldf*.bgf xldf*.ldf xldf.html xldf.fo xldf.pdf *.xbnf
	rm -f bgf*.bgf ldf*.bgf ldf*.ldf ldf.fo ldf.pdf
	rm -f *.aux *.log *.toc *.tex printed_for_xldf.tmp

check:
	ls -1 *.xbgf | xargs -n1 ${validator} xbgf
	ls -1 xldf/*.xldf | xargs -n1 ${validator} xldf
	ls -1 *.bgf  | xargs -n1 ${validator} bgf
	ls -1 *.ldf  | xargs -n1 ${validator} ldf
