all:
	# ../../recovery/hunter/hunter.py test1/test.src test1/test.edd test1/test.out
	./hunterx.py test1/test.src test1/test.edd test1/test.out
	./rpl2htm.py recovery.rpl recovery2.html
	./rpl2htm-min.py recovery.rpl recovery3.html

rest:
	gdt test1/test.bgf test1/test.out
	@echo ----------
	@cat test1/test.src
	@echo ----------
	@bgfpp test1/test.out test1/test.edd

test1/test.src:
	echo No copy!
	# cp ../../recovery/hunter/test1/tests/8-t2nt-none-lower.bgf test1/test.bgf
	# 	cp ../../recovery/hunter/test1/tests/8-t2nt-none-lower.edd test1/test.edd
	# 	cp ../../recovery/hunter/test1/tests/8-t2nt-none-lower.src test1/test.src

bgf-validate:
	ls -1 *.bgf | xargs -n1 ${tooldir}/validate bgf

clean:
	rm -f *~ *.bgf *.edd *.out *.src
