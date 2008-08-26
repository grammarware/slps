include ../include.mk

test:
	cp ${xapply} .
	cp ${xfactorial} .
	@echo Testing pretty-printer...
	galax-run pprint.xq -serialize wf | grep -v "glx:result"
	@echo Testing evaluator...
	galax-run eval.xq -serialize wf > fac5.evaluated.xml
	cat fac5.evaluated.xml
	diff fac5.evaluated.xml baseline/fac5.xml

clean:
	rm -f *.xml

