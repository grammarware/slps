factorial = ../../repository/factorial.txt
xfactorial = ../../repository/factorial.xml
temp = temp.txt

all: test

test: 
	@echo Testing implementation `pwd`
	make Program.exe
	./Program.exe ${xfactorial} ${temp}
	diff ${factorial} ${temp}
	rm -f temp.txt
	@echo Done.

Program.exe: *.cs Makefile
	csc Program.cs PrettyPrinter.cs

clean:
	rm -rf *~
	rm -rf obj
	rm -rf bin
	rm -rf *.suo
	rm -rf Program.exe
	rm -f temp.txt
