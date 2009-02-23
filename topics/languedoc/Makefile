build:
	../../shared/tools/xsd2bgf ../../shared/xsd/ldf.xsd ldf.bgf
	../../topics/extraction/xsd2ldf/ldfgen.py ../../shared/xsd/ldf.xsd ldf.bgf ldf.ldf
	../../shared/tools/ldf2pdf ldf.ldf ldf.pdf

test:

xbgf:
	../../shared/tools/xsd2bgf ../../shared/xsd/xbgf.xsd xbgf.bgf
	../../shared/tools/xbgf beautify.xbgf xbgf.bgf xbgf_1.bgf
	../../topics/extraction/xsd2ldf/ldfgen.py ../../shared/xsd/xbgf.xsd xbgf_1.bgf xbgf.ldf
	python ../../topics/transformation/xldf/xldf.py complete.xldf xbgf.ldf xbgf_1.ldf
	xsltproc ../../shared/xsl/ldf2xhtml.xslt xbgf_1.ldf | python ../../topics/presentation/ldf2pdf/closemeta.py > xbgf.html
	xsltproc ../../shared/xsl/xhtml2fo.xslt xbgf.html > xbgf.fo
	../../download/fop/fop -q xbgf.fo xbgf.pdf

tex:
	../../shared/tools/xsd2bgf ../../shared/xsd/xbgf.xsd xbgf.bgf
	../../shared/tools/xbgf beautify.xbgf xbgf.bgf xbgf_1.bgf
	../../topics/extraction/xsd2ldf/ldfgen.py ../../shared/xsd/xbgf.xsd xbgf_1.bgf xbgf.ldf
	python ../../topics/transformation/xldf/xldf.py complete.xldf xbgf.ldf xbgf_1.ldf
	xsltproc ../../shared/xsl/ldf2tex.xslt xbgf_1.ldf > xbgf.tex
	pdflatex -interaction=batchmode xbgf
	pdflatex -interaction=batchmode xbgf

clean:
	rm -f xbgf*.bgf xbgf*.ldf xbgf.html xbgf.fo xbgf.pdf
	rm -f ldf*.bgf ldf*.ldf ldf.fo ldf.pdf
	rm -f *.aux *.log *.toc *.tex

