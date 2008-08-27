include ../include.mk

test:
	@echo Testing pretty-printer...
	galax-run pprint.xq -serialize wf -doc input=${xfactorial} | grep -v "glx:result"
	@echo Testing evaluator...
	galax-run eval.xq -serialize wf -doc context=${xfactorial} -doc evalexpr=${xapply}> fac5.evaluated.xml
	cat fac5.evaluated.xml
	diff fac5.evaluated.xml baseline/fac5.xml

clean:
	rm -f *.xml

