all:
	make build
	make test

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

test:
	./testperform bad-f
	./testperform bad-d
	./testperform independent
	./testperform input
	./testperform incompatible

quiet:
	make test | grep OUTCOME

clean:
	rm -f tests/*/g?.bgf tests/*/*.cbgf tests/*/d-1*.xbgf
