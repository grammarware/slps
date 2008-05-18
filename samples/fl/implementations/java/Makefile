factorial = ../../repository/factorial.txt
temp = temp.txt

all: test

test:
	@echo Testing implementation `pwd`
	@make Test.class
	java -ea Test ${factorial} ${temp}
	@diff ${factorial} ${temp}
	@rm -f temp.txt
	@echo Done.

Test.class: *.java types/*.java *.g
	java org.antlr.Tool TFPL.g
	javac Test.java TFPLLexer.java TFPLParser.java

clean:
	rm -f *~
	rm -f types/*~
	rm -f *.class
	rm -f types/*.class
	rm -f *.tokens
	rm -f *Lexer.java
	rm -f *Parser.java
	rm -f *__.g
	rm -f temp.txt
	rm -f output.txt
