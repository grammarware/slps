validator = ../../../shared/tools/checkxml

build: bnfbgf.pdf tex html
	pdflatex -interaction=batchmode xbgf
	pdflatex -interaction=batchmode xbgf

slices:
	cp ../../presentation/prettyprint/xbgf/stripTerminals.xbgf sliced/
	python ../../../shared/python/xbgfSlice.py sliced/stripTerminals.xbgf

ldf: slices
	../../../shared/tools/xsd2ldf ../../../shared/xsd/xbgf.xsd xbgf.ldf
	../../../shared/tools/xldf xbgf.xldf xbgf.ldf xbgf.ready.ldf
	
tex: ldf
	../../../shared/tools/ldf2tex xbgf.ready.ldf xbgf.tex
	
html: ldf
	../../../shared/tools/repp xbgf.ready.ldf ../../../shared/tools/xbgf2html xbgf.hyper.ldf
	../../../shared/tools/ldf2html xbgf.hyper.ldf xbgf.html
	perl -pi -w -e 's/~\\cite{AgileWeb,DeanCMS03,Adaptation,PLEX}//g;' xbgf.html

bnfbgf.pdf:
	cd ../../presentation/prettyprint ; make
	cp ../../presentation/prettyprint/architecture_large.pdf bnfbgf.pdf

rebuild:
	make clean
	make build

clean:
	rm -f xbgf*.bgf xbgf*.ldf xbgf.html xbgf.fo xbgf.pdf *.xbnf
	rm -f bgf*.bgf ldf*.bgf ldf*.ldf ldf.fo ldf.pdf xldf-tmp*
	rm -f *.aux *.log *.toc *.tex printed_for_xldf.tmp bnfbgf.pdf sliced/*.xbgf

check:
	ls -1 *.xbgf | xargs -n1 ${validator} xbgf
	ls -1 xldf/*.xldf | xargs -n1 ${validator} xldf
	ls -1 *.bgf  | xargs -n1 ${validator} bgf
	ls -1 *.ldf  | xargs -n1 ${validator} ldf
