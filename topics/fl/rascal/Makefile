build:

test:
	cd src && java -Xmx1G -Xss32m -jar ../../../../download/rascal.jar FL ../../shared/factorial.txt ../../shared/fac5.txt > fl.result
	diff ../shared/fac5b.txt src/fl.result
	cd src && java -Xmx1G -Xss32m -jar ../../../../download/rascal.jar FL ../../shared/sample42a.txt > fl.result
	diff -b ../shared/sample42b.txt src/fl.result
	cd src && java -Xmx1G -Xss32m -jar ../../../../download/rascal.jar FL ../../shared/sample88a.txt > fl.result
	diff -b ../shared/sample88b.txt src/fl.result
