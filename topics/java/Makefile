all:
	@echo "Run 'make rebuild' if you want to renew the sources from their URLs."
	@echo NOT recommended UNLESS you really know what you are doing!
	@echo "Run 'make test' to execute all extractors and diff the results with LCI snapshot"

test:
	cd jls1 ; make all
	cd jls2 ; make all
	cd jls3 ; make all
	../../shared/tools/gdts jls1/jls1a.bgf lci/snapshot/jls1app.bgf
	../../shared/tools/gdts jls2/jls2a.bgf lci/snapshot/jls2app.bgf
	../../shared/tools/gdts jls3/jls3a.bgf lci/snapshot/jls3app.bgf
	../../shared/tools/gdts jls1/jls1d.bgf lci/snapshot/jls1doc.bgf
	../../shared/tools/gdts jls2/jls2d.bgf lci/snapshot/jls2doc.bgf
	../../shared/tools/gdts jls3/jls3d.bgf lci/snapshot/jls3doc.bgf

rebuild:
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/19.doc.html > jls1/app.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/syntax.doc.html > jls2/app.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/syntax.html > jls3/app.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/4.doc.html   >jls1/doc.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/6.doc.html  >>jls1/doc.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/7.doc.html  >>jls1/doc.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/8.doc.html  >>jls1/doc.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/9.doc.html  >>jls1/doc.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/10.doc.html >>jls1/doc.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/14.doc.html >>jls1/doc.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/15.doc.html >>jls1/doc.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/typesValues.doc.html >jls2/doc.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/names.doc.html >>jls2/doc.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/packages.doc.html >>jls2/doc.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/classes.doc.html >>jls2/doc.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/interfaces.doc.html >>jls2/doc.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/arrays.doc.html >>jls2/doc.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/statements.doc.html >>jls2/doc.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/expressions.doc.html >>jls2/doc.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/typesValues.html >jls3/doc.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/names.html >>jls3/doc.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/packages.html >>jls3/doc.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/classes.html >>jls3/doc.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/interfaces.html >>jls3/doc.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/arrays.html >>jls3/doc.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/statements.html >>jls3/doc.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/expressions.html >>jls3/doc.html

