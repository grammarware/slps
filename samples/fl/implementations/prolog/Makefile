include ../include.mk

all: test

test: 
	@echo Testing implementation `pwd`
	swipl -q -f TestIO.pro -t "main('${factorial}','${temp}')"
	swipl -q -f TestEvaluator.pro -t "main('${factorial}','${apply}',120)"
	swipl -q -f TestOptimizer.pro -t "main('${unoptimized}','${optimized}')"
	diff ${factorial} ${temp}
	rm -f temp.txt
	@echo Done.

clean:
	rm -rf *~
	rm -f temp.txt
