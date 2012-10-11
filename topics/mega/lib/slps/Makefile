all:

pdf: guided.megal.pdf

%.pdf: %.dot
	dot -Tpdf $*.dot > $*.pdf

clean:
	rm -f *.back.megal *.pdf *.dot
