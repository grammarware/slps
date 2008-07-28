include ../include.mk

all: test

test: TestIO.exe
	@echo Testing implementation `pwd`
	./TestIO.exe ${xfactorial} ${temp}
	diff ${factorial} ${temp}
	rm -f temp.txt
	@echo Done.

TestIO.exe: *.cs Makefile
	csc TestIO.cs PrettyPrinter.cs

clean:
	rm -rf *~
	rm -rf obj
	rm -rf bin
	rm -rf *.suo
	rm -rf *.exe
	rm -f temp.txt
