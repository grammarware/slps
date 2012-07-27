all:
	pdflatex paper
	bibtex paper
	pdflatex -interaction=batchmode paper
	pdflatex -interaction=batchmode paper
	chmod 644 paper.pdf

clean:
	rm -f paper.pdf *.dvi *.aux *.log *.toc *.bbl *.blg *.out *.bak *~

