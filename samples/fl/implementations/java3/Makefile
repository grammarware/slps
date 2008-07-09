include ../include.mk

all: test

test:  TestIO.class TestEvaluator.class
	@echo Testing implementation `pwd`
	java -ea TestIO ${xfactorial} ${temp}
	diff ${factorial} ${temp}
	rm -f temp.txt
	java -ea TestEvaluator ${xfactorial} ${xapply} 120
	@echo Done.

TestIO.class TestEvaluator.class: *.java *.aj fl/*.java Makefile
	ajc -source 1.5 *.java fl/*.java *.aj

generate:
	rm -f fl/*.class fl/*.java
	xjc ${schema}

clean:
	rm -f *~
	rm -f fl/*~
	rm -f ajcore*
	rm -f fl/ajcore*
	rm -f *.class
	rm -f fl/*.class
