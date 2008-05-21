schema = ../../repository/fl.xsd

all:
	@echo Testing implementation `pwd`
	make fl
	@echo Done.

foo:
	ajc Demo.java Tracer.aj
# PrettyPrinter.aj Evaluator.aj

fl: ${schema} Makefile
	rm -rf fl
	xjc ${schema}

clean:
	rm -f *~
