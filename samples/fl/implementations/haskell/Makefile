factorial = ../../repository/factorial.txt
temp = temp.txt

all: test

test:
	@echo Testing implementation `pwd`
	@make Main
	@./Main ${factorial} ${temp}
	@diff ${factorial} ${temp}
	@rm -f temp.txt
	@echo Done.

Main: *.hs Makefile
	@ghc --make Main.hs 

clean:
	rm -rf *~
	rm -rf Main
	rm -rf *.o
	rm -rf *.hi
	rm -f temp.txt
