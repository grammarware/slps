i = -i:.:../../../implementation/NielsonN07/Haskell:../../../implementation/NielsonN07/Haskell/src

test:
	make Main.test
	cat Main.ref

%.test:
	@echo Testing $*.hs
	@ghci -v0 ${i} $*.hs < ../../../implementation/NielsonN07/Haskell/include/Main.in > $*.out
	@diff -b $*.out $*.ref
	@rm $*.out

%.ghci:
	ghci ${i} $*.hs

clean:
	rm -rf *.out
