gbtf-options = root uppy tc pc nc bc uc
include ../Makefile.include

cj:
	@echo Habelitz:
	@make sc.count.hab
	@make rc.count.hab
	@make nc.count.hab
	@make bc.count.hab
	@make cdbc.count.hab
	@echo Parr:
	@make sc.count.par
	@make rc.count.par
	@make nc.count.par
	@make bc.count.par
	@make cdbc.count.par

%.count.hab:
	@/bin/echo -n '        ' $*:
	@find . -name 'habelitz.bgf-*-$*-*.btf' | wc -l

%.count.par:
	@/bin/echo -n '        ' $*:
	@find . -name 'parr.bgf-*-$*-*.btf' | wc -l

execute:
	time ls -1 habelitz.bgf-*.src | xargs -n1 ./java.run habelitz.jar > results/h2h.res 2>&1
	time ls -1 habelitz.bgf-*.src | xargs -n1 ./java.run parr.jar     > results/h2p.res 2>&1
	time ls -1 parr.bgf-*.src     | xargs -n1 ./java.run habelitz.jar > results/p2h.res 2>&1
	time ls -1 parr.bgf-*.src     | xargs -n1 ./java.run parr.jar     > results/p2p.res 2>&1
	./report-results.py
