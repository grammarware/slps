all:
	make build
	make test

build:
	cp ../../convergence/xbgf/fl/snapshot/rascal.bgf tests/bad-f/g1.bgf
	cp ../../convergence/xbgf/fl/snapshot/rascal.bgf tests/bad-d/g1.bgf
	cp ../../convergence/xbgf/fl/snapshot/rascal.bgf tests/independent/g1.bgf
	cp ../../convergence/xbgf/fl/snapshot/rascal.bgf tests/input/g1.bgf
	cp ../../convergence/xbgf/fl/snapshot/rascal.bgf tests/incompatible/g1.bgf

test:
	./testperform bad-f || echo "Ok"
	./testperform bad-d || echo "Ok"
	./testperform independent || echo "Ok"
	./testperform input || echo "Ok"
	./testperform incompatible || echo "Ok"

quiet:
	make test 2> /dev/null | grep OUTCOME

clean:
	rm -f tests/*/g?.bgf tests/*/*.cbgf tests/*/d-1*.xbgf
