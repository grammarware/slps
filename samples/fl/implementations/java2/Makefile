repository = ../../repository
factorial = ${repository}/factorial.txt
temp = temp.txt
apply = ${repository}/fac5.txt
unoptimized = ${repository}/sample88a.txt
optimized = ${repository}/sample88b.txt
parserfiles = Factory.java FLParser.java FLLexer.java
types = fl/types/Program.java

all: test

test: \
		TestPrettyPrinter.class \
		TestEvaluator.class \
		TestOptimizer.class
	@echo Testing implementation `pwd`
	java -ea TestPrettyPrinter ${factorial} ${temp}
	java -ea TestEvaluator ${factorial} ${apply} 120
	java -ea TestOptimizer ${unoptimized} ${optimized}
	diff ${factorial} ${temp}
	rm -f temp.txt
	@echo Done.

TestPrettyPrinter.class: PrettyPrinter.java ${parserfiles} TestPrettyPrinter.java Makefile
	javac TestPrettyPrinter.java

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
