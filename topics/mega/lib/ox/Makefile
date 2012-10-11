all:

pdf: annotation.megal.pdf configuration.megal.pdf deserialization.megal.pdf generation.megal.pdf read.megal.pdf serialization.megal.pdf technology.megal.pdf types.megal.pdf write.megal.pdf

build:
	cp ../../../../../101repo/capabilities/Object_XML_mapping/MegaL/*.megal .

%.pdf: %.dot
	dot -Tpdf $*.dot > $*.pdf

clean:
	rm -f *.back.megal *.dot *.pdf
