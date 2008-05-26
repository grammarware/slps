include ../include.mk

all: test

test: TestIO.exe TestEvaluator.exe
	@echo Testing implementation `pwd`
	./TestIO.exe ${xfactorial} ${temp}
	diff ${factorial} ${temp}
	rm -f temp.txt
	./TestEvaluator.exe ${xfactorial} ${xapply} 120
	@echo Done.

TestIO.exe: *.cs Makefile
	csc TestIO.cs PrettyPrinter.cs fl.cs

TestEvaluator.exe: *.cs Makefile
	csc TestEvaluator.cs Evaluator.cs fl.cs

fl.cs: ${schema}
	xsd.exe /classes ${schema}

clean:
	rm -rf *~
	rm -rf obj
	rm -rf bin
	rm -rf *.suo
	rm -rf *.exe
	rm -f temp.txt
