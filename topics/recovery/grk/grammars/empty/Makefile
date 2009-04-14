# Prolog programs for the several phases
pl	  = pl -T12345 -L12345 -G23456 -f
many2one  = ${pl} ../../tools/dia2ast/many2one.pl --
fst 	  = ${pl} ../../tools/fst/main.pl         --
ast2ebnf  = ${pl} ../../tools/ast2ebnf.pl         --
ast2lll   = ${pl} ../../tools/ast2lll.pl          --
ast2dcg	  = ${pl} ../../tools/ast2dcg.pl          --
ast2dia   = ${pl} ../../tools/ast2dia/main.pl     --
reduced   = ${pl} ../../tools/reduced.pl          --
nop       = ${pl} ../../tools/nop.pl              --

all: grammar.lll

grammar.transformed: grammar.empty script.fst
	@${fst} script.fst grammar.empty grammar.transformed

grammar.ebnf: grammar.transformed
	mkdir -p scratch
	@${ast2ebnf} $< $@ scratch/grammar.mods scratch out

grammar.lll: grammar.transformed
	@${ast2lll} grammar.transformed grammar.lll

clean:
	@rm -rf *~
	@rm -rf grammar.transformed
	@rm -rf grammar.ebnf
	@rm -rf grammar.lll
	@rm -rf scratch
