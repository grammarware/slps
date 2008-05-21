repository = ../../repository
factorial = ${repository}/factorial.txt
xfactorial =  ${repository}/factorial.xml
schema =  ${repository}/fl.xsd
temp = temp.txt
apply = ${repository}/fac5.xml

all: test

test: 
	@echo Testing implementation `pwd`
	make Program.exe
	./Program.exe ${xfactorial} ${temp} ${apply}
	diff ${factorial} ${temp}
	rm -f temp.txt
	@echo Done.

Program.exe: *.cs Makefile
	csc Program.cs PrettyPrinter.cs Evaluator.cs fl.cs

fl.cs: ${schema}
	xsd.exe /classes ${schema}

clean:
	rm -rf *~
	rm -rf obj
	rm -rf bin
	rm -rf *.suo
	rm -rf Program.exe
	rm -f temp.txt
