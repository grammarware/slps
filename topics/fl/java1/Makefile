include ../include.mk

build: classes jar

jar: classes
	jar cf javafl.jar *.class types/*.class

test: classes
	@echo Testing implementation `pwd`
	java -cp $$CLASSPATH:havafl.jar -ea TestIO ${factorial} ${temp}
	diff ${factorial} ${temp}
	rm -f temp.txt
	java -ea TestEvaluator ${factorial} ${apply} 120
	@echo Done.

classes: TestIO.class TestEvaluator.class

TestIO.class: TestIO.java FLParser.java types/*.java
	javac TestIO.java

TestEvaluator.class: TestEvaluator.java FLParser.java types/*.java
	javac TestEvaluator.java

FLScanner.java: FLParser.java

FLParser.java: FL.g
	make generate

generate:
	java org.antlr.Tool FL.g

clean:
	rm -f *~
	rm -f types/*~
	rm -f *.class
	rm -f *.jar
	rm -f types/*.class
	rm -f *.tokens
	rm -f *Lexer.java
	rm -f *Parser.java
	rm -f *__.g
	rm -f temp.txt
	rm -f output.txt
