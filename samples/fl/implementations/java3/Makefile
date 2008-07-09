include ../include.mk

all: test

test:
	@echo Testing implementation `pwd`
	make fl
	make TestIO.class
	make TestEvaluator.class
	java -ea TestIO ${xfactorial} ${temp}
	diff ${factorial} ${temp}
	rm -f temp.txt
	java -ea TestEvaluator ${xfactorial} ${xapply} 120
	@echo Done.

TestIO.class TestEvaluator.class: fl *.java *.aj fl/*.java Makefile
	ajc -source 1.5 *.java fl/*.java *.aj

fl:
	xjc ${schema}

clean:
	rm -f *~
	rm -f fl/*~
	rm -f ajcore*
	rm -f *.class
	rm -rf fl
