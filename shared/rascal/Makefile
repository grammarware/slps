all:
	echo "Use Rascal"

build:
	make clean
	cp ../../topics/convergence/xbgf/*/xbgf/*.xbgf tests/xbgf/
	cp ../../topics/transformation/xbgf/tests/*.xbgf tests/xbgf/

check:
	echo "Validating input files..."
	ls -1 tests/xbgf/*.xbgf | xargs -n1 ../tools/validate xbgf
	echo "Validating output files..."
	ls -1 res/xbgf-res/*.xbgf | xargs -n1 ../tools/validate xbgf

count:
	wc -l src/*.rsc src/*/*.rsc src/*/*/*.rsc

clean:
	rm -f tests/*/*.xbgf
