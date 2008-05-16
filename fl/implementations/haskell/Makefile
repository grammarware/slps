factorial = ../../repository/factorial.txt
temp = temp.txt

all: run-test

run-test: Main
	./Main ${factorial} ${temp}
	diff ${factorial} ${temp}
	rm -f temp.txt

Main: *.hs Makefile
	ghc --make Main.hs 

clean:
	rm -rf *~
	rm -rf Main
	rm -rf *.o
	rm -rf *.hi
	rm -f temp.txt
