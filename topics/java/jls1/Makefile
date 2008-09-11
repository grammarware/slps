all:
	python ../../extraction/html2bgf/xpathpre.py LALR -Difficulties <syntax.html >parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html jls1.bgf
	python ../../extraction/html2bgf/xpathpre.py 4 6 7 8 9 10 14 15 <collected.html >parse.html
	python ../../extraction/html2bgf/html2bgf.py parse.html jls1c.bgf
	../../../shared/tools/checkxml bgf jls1.bgf
	../../../shared/tools/checkxml bgf jls1c.bgf

clean:
	rm -f *.bgf parse.html
