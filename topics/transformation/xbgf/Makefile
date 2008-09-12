build:
	cd tests; make build
	cd apps; make build
	cd ../../fl/lci; make build

rebuild:
	make clean
	make build

test:
	cd apps; make test
	cd tests; make test
	cd ../../fl/lci; make test

clean:
	rm -f *~
	cd tests; make clean
	cd apps; make clean
	cd ../../fl/lci; make clean

