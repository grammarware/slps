include ../include.mk

jar = javafl.jar
cp = -cp $$CLASSPATH:../../../download/antlrworks-1.1.7.jar:${jar}

build: ${jar}

${jar}: FLParser.java FLLexer.java *.java types/*.java
	cd types; javac *.java
	javac *.java
	jar cf ${jar} *.class types/*.class
	rm *.class

test: ${jar}
	@echo Testing implementation `pwd`
	java ${cp} -ea TestIO ${factorial} ${temp}
	diff ${factorial} ${temp}
	rm -f temp.txt
	java ${cp} -ea TestEvaluator ${factorial} ${apply} 120
	@echo Done.

FLLexer.java: FLParser.java

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
