test:
	@txl ../shared/factorial.txt FL.Txl 2> /dev/null
	@/bin/echo -n "evaluate x = " >sample.fl
	@cat ../shared/sample88a.txt >> sample.fl
	@txl sample.fl FL_optimiser.Txl 2> /dev/null
	@/bin/echo -n "evaluate x = " >sample.fl
	@cat ../shared/sample42a.txt >> sample.fl
	@txl sample.fl FL_optimiser.Txl 2> /dev/null
	@cp ../shared/factorial.txt fac5.fl
	@/bin/echo -n "evaluate x = " >>fac5.fl
	@cat ../shared/fac5.txt >> fac5.fl
	@txl fac5.fl FL_evaluator.Txl

extract:
	../../../shared/tools/txl2bgf FL.Txl fl.bgf
