validator = ../../../shared/tools/checkxml
xldf = ../../../shared/tools/xldf
xbgf = ../../../shared/tools/xbgf

build: bnfbgf.pdf
	../../../shared/tools/xsd2bgf ../../../shared/xsd/xbgf.xsd xbgf.bgf
	../../../topics/extraction/xsd2ldf/ldfgen.py ../../../shared/xsd/xbgf.xsd xbgf.bgf xbgf.ldf
	${xldf} xbgf.xldf xbgf.ldf xbgf.ready.ldf
	xsltproc ../../../shared/xsl/ldf2tex.xslt xbgf.ready.ldf > xbgf.tex
	pdflatex -interaction=batchmode xbgf
	pdflatex -interaction=batchmode xbgf

bnfbgf.pdf:
	cd ../../presentation/prettyprint ; make
	cp ../../presentation/prettyprint/architecture_large.pdf bnfbgf.pdf

rebuild:
	make clean
	make build

clean:
	rm -f xbgf*.bgf xbgf*.ldf xbgf.html xbgf.fo xbgf.pdf *.xbnf
	rm -f bgf*.bgf ldf*.bgf ldf*.ldf ldf.fo ldf.pdf xldf-tmp*
	rm -f *.aux *.log *.toc *.tex printed_for_xldf.tmp bnfbgf.pdf

check:
	ls -1 *.xbgf | xargs -n1 ${validator} xbgf
	ls -1 xldf/*.xldf | xargs -n1 ${validator} xldf
	ls -1 *.bgf  | xargs -n1 ${validator} bgf
	ls -1 *.ldf  | xargs -n1 ${validator} ldf
