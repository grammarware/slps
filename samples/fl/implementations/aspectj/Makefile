schema = ../../repository/fl.xsd

all:
	@echo Testing implementation `pwd`
	make fl
	@echo Done.

foo:
	ajc Test.java fl/*.java PrettyPrinter.aj Evaluator.aj

fl: ${schema}
	rm -rf fl
	xjc ${schema}

clean:
	rm -f *~
