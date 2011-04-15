gbtf-options = root tc pc nc bc uc
include ../Makefile.include

jt:
	ls -1 *.bgf | xargs -n1 -I _ make _.jt
	make convert

%.jt:
	gbtf $* root sc
	gbtf $* root rc
	gbtf $* root nc
	gbtf $* root bc
	gbtf $* root cdbc

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

executez:
	time ls -1 habelitz.bgf-*.src | xargs -n1 ./java.run habelitz.jar > results/z2z.res 2>&1
	time ls -1 habelitz.bgf-*.src | xargs -n1 ./java.run parr.jar     > results/z2r.res 2>&1
	time ls -1 habelitz.bgf-*.src | xargs -n1 ./java.run stahl.jar    > results/z2l.res 2>&1
	time ls -1 habelitz.bgf-*.src | xargs -n1 ./java.run studman.jar  > results/z2n.res 2>&1
executer:
	time ls -1 parr.bgf-*.src     | xargs -n1 ./java.run habelitz.jar > results/r2z.res 2>&1
	time ls -1 parr.bgf-*.src     | xargs -n1 ./java.run parr.jar     > results/r2r.res 2>&1
	time ls -1 parr.bgf-*.src     | xargs -n1 ./java.run stahl.jar    > results/r2l.res 2>&1
	time ls -1 parr.bgf-*.src     | xargs -n1 ./java.run studman.jar  > results/r2n.res 2>&1
executel:
	time ls -1 stahl.bgf-*.src    | xargs -n1 ./java.run habelitz.jar > results/l2z.res 2>&1
	time ls -1 stahl.bgf-*.src    | xargs -n1 ./java.run parr.jar     > results/l2r.res 2>&1
	time ls -1 stahl.bgf-*.src    | xargs -n1 ./java.run stahl.jar    > results/l2l.res 2>&1
	time ls -1 stahl.bgf-*.src    | xargs -n1 ./java.run studman.jar  > results/l2n.res 2>&1
executen:
	time ls -1 studman.bgf-*.src  | xargs -n1 ./java.run habelitz.jar > results/n2z.res 2>&1
	time ls -1 studman.bgf-*.src  | xargs -n1 ./java.run parr.jar     > results/n2r.res 2>&1
	time ls -1 studman.bgf-*.src  | xargs -n1 ./java.run stahl.jar    > results/n2l.res 2>&1
	time ls -1 studman.bgf-*.src  | xargs -n1 ./java.run studman.jar  > results/n2n.res 2>&1

report:
	./report-results.py
