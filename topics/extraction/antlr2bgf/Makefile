source = ../../fl/java1/fl.g
target = tests/fl
antlr = antlrworks-1.1.7.jar
cp = -cp ../../${antlr} org.antlr.Tool
pkg = slps/antlr2bgf

build:
	cp ../../../download/${antlr} .
	cd ${pkg}; java ${cp} StrippedANTLR.g
	cd ${pkg}; javac *.java

test:
	make clean-tests
	../../../shared/tools/antlr2bgf ${source} ${target}.bgf
	../../../shared/tools/showbgf ${target}.bgf > ${target}.out
	diff ${target}.out ${target}.baseline

clean:
	make clean-tests
	rm -f recovered.xml
	rm -f *~
	rm -f ${antlr}
	rm -f ${pkg}/*.class
	rm -f ${pkg}/*.tokens
	rm -f ${pkg}/*Lexer.java
	rm -f ${pkg}/*Parser.java
	rm -f ${pkg}/*__.g

clean-tests:
	rm -f tests/*.bgf
	rm -f tests/*.out
