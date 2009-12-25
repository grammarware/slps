source = ../../fl/java1/FL.g
target = tests/fl
pkg = slps/antlr2bgf

build:
	cd ${pkg}; java org.antlr.Tool StrippedANTLR.g
	cd ${pkg}; javac *.java

test:
	make clean-tests
	../../../shared/tools/antlr2bgf ${source} ${target}.bgf
	../../../shared/tools/bgf2bnf ${target}.bgf ${target}.out
	diff ${target}.out ${target}.baseline

clean:
	make clean-tests
	rm -f recovered.xml
	rm -f *~
	rm -f ${pkg}/*.class
	rm -f ${pkg}/*.tokens
	rm -f ${pkg}/*Lexer.java
	rm -f ${pkg}/*Parser.java
	rm -f ${pkg}/*__.g

clean-tests:
	rm -f tests/*.bgf
	rm -f tests/*.out
