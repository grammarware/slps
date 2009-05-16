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
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/19.doc.html > jls1/impl.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/syntax.doc.html > jls2/impl.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/syntax.html > jls3/impl.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/4.doc.html   >jls1/read.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/6.doc.html  >>jls1/read.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/7.doc.html  >>jls1/read.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/8.doc.html  >>jls1/read.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/9.doc.html  >>jls1/read.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/10.doc.html >>jls1/read.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/14.doc.html >>jls1/read.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/15.doc.html >>jls1/read.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/typesValues.doc.html >jls2/read.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/names.doc.html >>jls2/read.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/packages.doc.html >>jls2/read.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/classes.doc.html >>jls2/read.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/interfaces.doc.html >>jls2/read.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/arrays.doc.html >>jls2/read.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/statements.doc.html >>jls2/read.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/expressions.doc.html >>jls2/read.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/typesValues.html >jls3/read.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/names.html >>jls3/read.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/packages.html >>jls3/read.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/classes.html >>jls3/read.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/interfaces.html >>jls3/read.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/arrays.html >>jls3/read.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/statements.html >>jls3/read.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/expressions.html >>jls3/read.html

