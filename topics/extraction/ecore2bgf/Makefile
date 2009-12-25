source1 = ../../fl/emf1/fl.ecore
target1 = fl1
source2 = ../../fl/emf2/model/fl.ecore
target2 = fl2

test:
	make clean
	../../../shared/tools/ecore2bgf ${source1} ${target1}.bgf
	../../../shared/tools/ecore2bgf ${source2} ${target2}.bgf
	../../../shared/tools/bgf2bnf ${target1}.bgf ${target1}.out
	../../../shared/tools/bgf2bnf ${target2}.bgf ${target2}.out
	diff ${target1}.out ${target1}.baseline
	diff ${target2}.out ${target2}.baseline

clean:
	rm -f *.bgf *.out
