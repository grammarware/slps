source1 = ../../fl/emf1/fl.ecore
target1 = fl1
source2 = ../../fl/emf2/model/fl.ecore
target2 = fl2

test:
	make clean
	../../../shared/tools/ecore2bgf ${source1} ${target1}.bgf
	../../../shared/tools/ecore2bgf ${source2} ${target2}.bgf
	../../../shared/tools/gdt ${target1}.bgf ${target1}.baseline
	../../../shared/tools/gdt ${target2}.bgf ${target2}.baseline

clean:
	rm -f *.bgf *~
