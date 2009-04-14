pl	  = pl -T12345 -L12345 -G23456 -f
fst 	  = ${pl} ../main.pl         --
t	  = ibm-transformed

no-all:

fail:
	make add-fails.out

add-fails.in: ../../../grammars/vscobolii/scratch/${t}.init
	@cp $< $@

%.out: %.in %.fst
	@${fst} $*.fst $*.in $*.out

clean:
	@rm -f *~
	@rm -f *.in
	@rm -f *.out
