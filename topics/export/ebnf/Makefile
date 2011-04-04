test:
	ls -1 ../../storage/bgf/tests/*.bgf | xargs -n1 ./testperform

speedpy:
	ls -1 ../../storage/bgf/tests/*.bgf | xargs -n1 -I _ python export.py _ /dev/null

speedxslt:
	ls -1 ../../storage/bgf/tests/*.bgf | xargs -n1 -I _ xsltproc ../../../shared/xsl/bgf2bnf.xslt _ 

clean:
	rm -f *.bnf