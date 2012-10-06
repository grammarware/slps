all: guided.pdf

%.pdf: %.dot
	dot -Tpdf $*.dot > $*.pdf

clean:
	rm -f *.back *.pdf *.dot
