all: annotation.megal.pdf configuration.megal.pdf deserialization.megal.pdf generation.megal.pdf read.megal.pdf serialization.megal.pdf technology.megal.pdf types.megal.pdf write.megal.pdf

%.pdf: %.dot
	dot -Tpdf $*.dot > $*.pdf

clean:
	rm -f *.back *.pdf *.dot
