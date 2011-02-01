all:
	make clean
	make test

build:

test:
	find ../../transformation/xbgf/tests/ -name "*.xbgf" | xargs -I {} -n1 galax-run perxbgfcommand.xq -serialize wf -doc xsd=../../../shared/xsd/xbgf.xsd -doc xbgf={} -language dxq | grep -v "glx:result"

clean:
	rm -f *.bnf
