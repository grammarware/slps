all:
	make build
	make test1

check:
	ls -1 tests/*/*.xbgf | xargs -n1 ../../../shared/tools/validate xbgf
	ls -1 tests/*/*.bgf | xargs -n1 ../../../shared/tools/validate bgf
	ls -1 tests/*/*.cbgf | xargs -n1 ../../../shared/tools/validate cbgf

build:
	cp ../../convergence/xbgf/fl/snapshot/rascal.bgf tests/bad-f/g1.bgf
	cp ../../convergence/xbgf/fl/snapshot/rascal.bgf tests/bad-d/g1.bgf
	cp ../../convergence/xbgf/fl/snapshot/rascal.bgf tests/independent/g1.bgf
	cp ../../convergence/xbgf/fl/snapshot/rascal.bgf tests/input/g1.bgf
	cp ../../convergence/xbgf/fl/snapshot/rascal.bgf tests/incompatible/g1.bgf

test1:
	@./run1 bad-f || echo OUTCOME is ERROR
	@./run1 bad-d || echo OUTCOME is ERROR
	@./run1 independent || echo OUTCOME is ERROR
	@./run1 input || echo OUTCOME is ERROR
	@./run1 incompatible || echo OUTCOME is ERROR

quiet:
	make test1 | grep OUTCOME

clean:
	rm -f tests/*/g?.bgf tests/*/*.cbgf tests/*/d-1*.xbgf tests/*/e.xbgf tests/*/g.xbgf
