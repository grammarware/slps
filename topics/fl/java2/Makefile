include ../include.mk
parserfiles = Factory.java FLParser.java FLLexer.java
types = fl/types/Program.java

build:

test: \
		TestIO.class \
		TestEvaluator.class \
		TestOptimizer.class
	@echo Testing implementation `pwd`
	java -ea TestIO ${factorial} ${temp}
	diff ${factorial} ${temp}
	rm -f temp.txt
	java -ea TestEvaluator ${factorial} ${apply} 120
	java -ea TestOptimizer ${unoptimized} ${optimized}
	@echo Done.

TestIO.class: PrettyPrinter.java ${parserfiles} TestIO.java Makefile
	javac TestIO.java

TestEvaluator.class: Evaluator.java ${parserfiles} TestEvaluator.java Makefile
	javac TestEvaluator.java

TestOptimizer.class: Optimizer.java ${parserfiles} TestOptimizer.java Makefile
	javac TestOptimizer.java

FLParser.java: FL.g
	java org.antlr.Tool FL.g

FLScanner.java: FLParser.java

%.java: %.t ${types} Makefile
	tom $*

${types}: FL.gom Makefile
	gom FL.gom

clean:
	rm -f *~
	rm -f *.java
	rm -f *.class
	rm -f *.tokens
	rm -f *__.g
	rm -rf fl
