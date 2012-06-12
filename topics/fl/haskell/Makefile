include ../include.mk

all: test

test: TestIO TestEvaluator TestOptimizer
	@echo Testing implementation `pwd`
	./TestIO ${factorial} ${temp}
	diff ${factorial} ${temp}
	rm -f temp.txt
	./TestEvaluator ${factorial} ${apply} `cat ../shared/fac5b.txt`
	./TestOptimizer ${unoptimized} ${optimized}
	@echo Done.

TestIO: *.hs Makefile
	ghc --make TestIO.hs 

TestEvaluator: *.hs Makefile
	ghc --make TestEvaluator.hs 

TestOptimizer: *.hs Makefile
	ghc --make TestOptimizer.hs 

clean:
	rm -rf *~
	rm -rf TestIO
	rm -rf TestEvaluator
	rm -rf TestOptimizer
	rm -rf *.o
	rm -rf *.hi
	rm -f temp.txt
