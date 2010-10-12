basedir = ../../fl/java1
package = types
target = tests/fl

build:
	cd slps/java2bgf; javac *.java

test:
	make clean-tests
	../../../shared/tools/java2bgf ${basedir} ${package} ${target}.bgf
	../../../shared/tools/validate bgf ${target}.bgf
	../../../shared/tools/showbgf ${target}.bgf > ${target}.out
	diff ${target}.out ${target}.baseline

clean:
	make clean-tests
	rm -f *~
	rm -f slps/java2bgf/*.class

clean-tests:
	rm -f tests/*.bgf
	rm -f tests/*.out
