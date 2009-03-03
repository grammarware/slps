validator = ../../shared/tools/checkxml

build: bgf_pretty.bgf xbgf.ldf
	python ../../topics/transformation/xldf/xldf.py xldf/completeIntro.xldf xbgf.ldf xbgf1.ldf
	python ../../topics/transformation/xldf/xldf.py xldf/completeFoldSection.xldf xbgf1.ldf xbgf2.ldf
	python ../../topics/transformation/xldf/xldf.py xldf/completeRefSection.xldf xbgf2.ldf xbgf3.ldf
	python ../../topics/transformation/xldf/xldf.py xldf/completeIncSection.xldf xbgf3.ldf xbgf4.ldf
	python ../../topics/transformation/xldf/xldf.py xldf/completeDecSection.xldf xbgf4.ldf xbgf5.ldf
	python ../../topics/transformation/xldf/xldf.py xldf/completeRevSections.xldf xbgf5.ldf xbgf6.ldf
	python ../../topics/transformation/xldf/xldf.py xldf/completeDecorSection.xldf xbgf6.ldf xbgf7.ldf
	xsltproc ../../shared/xsl/ldf2tex.xslt xbgf7.ldf > xbgf.tex
	pdflatex -interaction=batchmode xbgf
	pdflatex -interaction=batchmode xbgf

bgf_pretty.bgf:
	../../shared/tools/xsd2bgf ../../shared/xsd/bgf.xsd bgf.bgf
	../../shared/tools/xbgf beautify_bgf.xbgf bgf.bgf bgf_pretty.bgf

xbgf.ldf:
	../../shared/tools/xsd2bgf ../../shared/xsd/xbgf.xsd xbgf.bgf
	../../shared/tools/xbgf beautify_xbgf.xbgf xbgf.bgf xbgf_1.bgf
	../../topics/extraction/xsd2ldf/ldfgen.py ../../shared/xsd/xbgf.xsd xbgf_1.bgf xbgf.ldf

rebuild:
	make clean
	make build

clean:
	rm -f xbgf*.bgf xbgf*.ldf xbgf.html xbgf.fo xbgf.pdf *.xbnf
	rm -f bgf*.bgf ldf*.bgf ldf*.ldf ldf.fo ldf.pdf
	rm -f *.aux *.log *.toc *.tex printed_for_xldf.tmp

check:
	ls -1 *.xbgf | xargs -n1 ${validator} xbgf
	ls -1 xldf/*.xldf | xargs -n1 ${validator} xldf
	ls -1 *.bgf  | xargs -n1 ${validator} bgf
	ls -1 *.ldf  | xargs -n1 ${validator} ldf
