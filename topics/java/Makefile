all:
	@echo "Run 'make rebuild' if you want to renew the sources from their URLs."
	@echo NOT recommended UNLESS you really know what you are doing!
	@echo "Run 'make test' to execute all extractors and diff the results with LCI snapshot"

test:
	cd jls1 ; make
	cd jls2 ; make
	cd jls3 ; make
	../../shared/tools/gdt jls1/jls1.bgf lci/snapshot/jls1.bgf
	../../shared/tools/gdt jls2/jls2.bgf lci/snapshot/jls2.bgf
	../../shared/tools/gdt jls3/jls3.bgf lci/snapshot/jls3.bgf

rebuild:
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/19.doc.html > jls1/syntax.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/syntax.doc.html > jls2/syntax.html
	curl -k http://java.sun.com/docs/books/jls/third_edition/html/syntax.html > jls3/syntax.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/4.doc.html   >jls1/collected.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/6.doc.html  >>jls1/collected.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/7.doc.html  >>jls1/collected.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/8.doc.html  >>jls1/collected.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/9.doc.html  >>jls1/collected.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/10.doc.html >>jls1/collected.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/14.doc.html >>jls1/collected.html
	curl -k http://java.sun.com/docs/books/jls/first_edition/html/15.doc.html >>jls1/collected.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/typesValues.doc.html >jls2/collected.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/names.doc.html >>jls2/collected.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/packages.doc.html >>jls2/collected.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/classes.doc.html >>jls2/collected.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/interfaces.doc.html >>jls2/collected.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/arrays.doc.html >>jls2/collected.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/statements.doc.html >>jls2/collected.html
	curl -k http://java.sun.com/docs/books/jls/second_edition/html/expressions.doc.html >>jls2/collected.html

