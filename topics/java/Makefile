all:
	@echo "Run 'make rebuild' if you want to renew the sources from their URLs."
	@echo NOT recommended UNLESS you really know what you are doing!
	@echo "Run 'make debug' to execute all extractors in debug mode"
	@echo "Run 'make test' to execute all extractors and diff the results with LCI snapshot"

debug:
	cd jls1 ; make debug
	cd jls2 ; make debug
	cd jls3 ; make debug

test:
	cd jls1 ; make all
	cd jls2 ; make all
	cd jls3 ; make all
	../../shared/tools/gdts jls1/impl1.bgf lci/snapshot/impl1.bgf
	../../shared/tools/gdts jls2/impl2.bgf lci/snapshot/impl2.bgf
	../../shared/tools/gdts jls3/impl3.bgf lci/snapshot/impl3.bgf
	../../shared/tools/gdts jls1/read1.bgf lci/snapshot/read1.bgf
	../../shared/tools/gdts jls2/read2.bgf lci/snapshot/read2.bgf
	../../shared/tools/gdts jls3/read3.bgf lci/snapshot/read3.bgf

rebuild:
	curl -k http://java.sun.com/reads/books/jls/first_edition/html/19.read.html > jls1/impl.html
	curl -k http://java.sun.com/reads/books/jls/second_edition/html/syntax.read.html > jls2/impl.html
	curl -k http://java.sun.com/reads/books/jls/third_edition/html/syntax.html > jls3/impl.html
	curl -k http://java.sun.com/reads/books/jls/first_edition/html/4.read.html   >jls1/read.html
	curl -k http://java.sun.com/reads/books/jls/first_edition/html/6.read.html  >>jls1/read.html
	curl -k http://java.sun.com/reads/books/jls/first_edition/html/7.read.html  >>jls1/read.html
	curl -k http://java.sun.com/reads/books/jls/first_edition/html/8.read.html  >>jls1/read.html
	curl -k http://java.sun.com/reads/books/jls/first_edition/html/9.read.html  >>jls1/read.html
	curl -k http://java.sun.com/reads/books/jls/first_edition/html/10.read.html >>jls1/read.html
	curl -k http://java.sun.com/reads/books/jls/first_edition/html/14.read.html >>jls1/read.html
	curl -k http://java.sun.com/reads/books/jls/first_edition/html/15.read.html >>jls1/read.html
	curl -k http://java.sun.com/reads/books/jls/second_edition/html/typesValues.read.html >jls2/read.html
	curl -k http://java.sun.com/reads/books/jls/second_edition/html/names.read.html >>jls2/read.html
	curl -k http://java.sun.com/reads/books/jls/second_edition/html/packages.read.html >>jls2/read.html
	curl -k http://java.sun.com/reads/books/jls/second_edition/html/classes.read.html >>jls2/read.html
	curl -k http://java.sun.com/reads/books/jls/second_edition/html/interfaces.read.html >>jls2/read.html
	curl -k http://java.sun.com/reads/books/jls/second_edition/html/arrays.read.html >>jls2/read.html
	curl -k http://java.sun.com/reads/books/jls/second_edition/html/statements.read.html >>jls2/read.html
	curl -k http://java.sun.com/reads/books/jls/second_edition/html/expressions.read.html >>jls2/read.html
	curl -k http://java.sun.com/reads/books/jls/third_edition/html/typesValues.html >jls3/read.html
	curl -k http://java.sun.com/reads/books/jls/third_edition/html/names.html >>jls3/read.html
	curl -k http://java.sun.com/reads/books/jls/third_edition/html/packages.html >>jls3/read.html
	curl -k http://java.sun.com/reads/books/jls/third_edition/html/classes.html >>jls3/read.html
	curl -k http://java.sun.com/reads/books/jls/third_edition/html/interfaces.html >>jls3/read.html
	curl -k http://java.sun.com/reads/books/jls/third_edition/html/arrays.html >>jls3/read.html
	curl -k http://java.sun.com/reads/books/jls/third_edition/html/statements.html >>jls3/read.html
	curl -k http://java.sun.com/reads/books/jls/third_edition/html/expressions.html >>jls3/read.html

