FST = /usr/local/bin/fst
EXP = /usr/local/bin/lllexport

all: grammars/Final.lll
	$(EXP) -f sdf -q Default grammars/Final.lll > sdf/GEN-csharp-Main.sdf
	$(EXP) -f html grammars/Final.lll > doc/csharp.html

grammars/Deyaccified.lll: CSHARP1.0.LLL trafo/deyaccify.trafo
	$(FST) trafo/deyaccify.trafo CSHARP1.0.LLL > $@

grammars/Adapted.lll: grammars/Deyaccified.lll trafo/adapt.trafo
	$(FST) trafo/adapt.trafo grammars/Deyaccified.lll > $@

grammars/Disambiguated.lll: grammars/Adapted.lll trafo/disambiguate.trafo
	$(FST) trafo/disambiguate.trafo grammars/Adapted.lll > $@

grammars/Fixed.lll: grammars/Disambiguated.lll trafo/fixerrors.trafo
	$(FST) trafo/fixerrors.trafo grammars/Disambiguated.lll > $@

grammars/WithExpression.lll: grammars/Fixed.lll trafo/expressions.trafo
	$(FST) trafo/expressions.trafo grammars/Fixed.lll > $@

grammars/Final.lll: grammars/WithExpression.lll trafo/modularise.trafo
	$(FST) trafo/modularise.trafo grammars/WithExpression.lll > $@

clean:
	rm -f *.lll grammars/*.lll sdf/GEN-*.sdf doc/*.html