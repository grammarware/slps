validator = ../../../shared/tools/checkxml
xldf = ../../../shared/tools/xldf
xbgf = ../../../shared/tools/xbgf

build: xbgf.ldf
	${xldf} xldf/completeIntro.xldf xbgf.ldf xbgf1.ldf
	${xldf} xldf/completeFoldSection.xldf xbgf1.ldf xbgf2.ldf
	${xldf} xldf/completeRefSection.xldf xbgf2.ldf xbgf3.ldf
	${xldf} xldf/completeIncSection.xldf xbgf3.ldf xbgf4.ldf
	${xldf} xldf/completeDecSection.xldf xbgf4.ldf xbgf5.ldf
	${xldf} xldf/completeRevSections.xldf xbgf5.ldf xbgf6.ldf
	${xldf} xldf/completeDecorSection.xldf xbgf6.ldf xbgf7.ldf
	xsltproc ../../../shared/xsl/ldf2tex.xslt xbgf7.ldf > xbgf.tex
	pdflatex -interaction=batchmode xbgf
	pdflatex -interaction=batchmode xbgf

xbgf.ldf: 
	../../../shared/tools/xsd2bgf ../../../shared/xsd/xbgf.xsd xbgf.bgf
	../../../topics/extraction/xsd2ldf/ldfgen.py ../../../shared/xsd/xbgf.xsd xbgf.bgf xbgf.ldf

rebuild:
	make clean
	make build

clean:
	rm -f xbgf*.bgf xbgf*.ldf xbgf.html xbgf.fo xbgf.pdf *.xbnf
	rm -f bgf*.bgf ldf*.bgf ldf*.ldf ldf.fo ldf.pdf xldf-tmp*
	rm -f *.aux *.log *.toc *.tex printed_for_xldf.tmp

check:
	ls -1 *.xbgf | xargs -n1 ${validator} xbgf
	ls -1 xldf/*.xldf | xargs -n1 ${validator} xldf
	ls -1 *.bgf  | xargs -n1 ${validator} bgf
	ls -1 *.ldf  | xargs -n1 ${validator} ldf
