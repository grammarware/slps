gbtf-options = sc root strip uppy
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