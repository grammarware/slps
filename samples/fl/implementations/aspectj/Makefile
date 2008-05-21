repository = ../../repository
factorial = ${repository}/factorial.txt
xfactorial =  ${repository}/factorial.xml
schema =  ${repository}/fl.xsd
temp = temp.txt
apply = ${repository}/fac5.xml

all: test

test:
	@echo Testing implementation `pwd`
	make Test.class
	java -ea Test ${xfactorial} ${temp} ${apply}
	@diff ${factorial} ${temp}
	@rm -f temp.txt
	@echo Done.

Test.class: *.java *.aj fl/*.java Makefile
	ajc -source 1.5 Test.java fl/*.java PrettyPrinter.aj Evaluator.aj

fl: ${schema}
	rm -rf fl
	xjc ${schema}

clean:
	rm -f *~
	rm -f fl/*~
	rm -f ajcore*
	rm -f fl/ajcore*
	rm -f *.class
	rm -f fl/*.class
