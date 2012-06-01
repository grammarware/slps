@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module syntax::XBGF

import syntax::BGF;

alias XBGFSequence = list[XBGFCommand];

data XBGFCommand =
	  abridge(BGFProduction p)
	| abstractize(BGFProduction p) // marked
	| addH(BGFProduction p) // marked
	| addV(BGFProduction p)
	| anonymize(BGFProduction p)
	| appear(BGFProduction p) // marked
	| chain(BGFProduction p)
	| clone(str x, str y, BGFContext w)
	| concatT(list[str] xs, str y, BGFContext w)
	| concretize(BGFProduction p) // marked
	| deanonymize(BGFProduction p)
	| define(list[BGFProduction] ps)
	| designate(BGFProduction p)
	| detour(BGFProduction p)
	| deyaccify(str x)
	| disappear(BGFProduction p) // marked
	| distribute(BGFContext w)
	| downgrade(BGFProduction p1,BGFProduction p2) // p1 is marked
	| eliminate(str x)
	| equate(str x, str y)
	| extract(BGFProduction p, BGFContext w)
	| factor(BGFExpression e1, BGFExpression e2, BGFContext w)
	| fold(str x, BGFContext w)
	| horizontal(BGFContext w)
	| \import(list[BGFProduction] ps)
	| inject(BGFProduction p) // marked
	| inline(str x)
	| introduce(list[BGFProduction] ps)
	| iterate(BGFProduction p)
	| lassoc(BGFProduction p)
	| massage(BGFExpression e1, BGFExpression e2, BGFContext w)
	| narrow(BGFExpression e1, BGFExpression e2, BGFContext w)
	| permute(BGFProduction p)
	| project(BGFProduction p) // marked
	| rassoc(BGFProduction p)
	| redefine(list[BGFProduction] ps)
	| removeH(BGFProduction p) // marked
	| removeV(BGFProduction p)
	| renameL(str x, str y, BGFContext w)
	| renameN(str x, str y, BGFContext w)
	| renameS(str x, str y, BGFContext w)
	| renameT(str x, str y, BGFContext w)
	| replace(BGFExpression e1, BGFExpression e2, BGFContext w)
	| reroot(list[str] xs)
	//| splitN(list[BGFProduction] ps, list[BGFProduction] qs, BGFContext w)
	| splitN(str x, list[BGFProduction] ps, BGFContext w)
	| splitT(str x, list[str] ys, BGFContext w)
	| unchain(BGFProduction p)
	| undefine(list[str] xs)
	| unfold(str x, BGFContext w)
	| unite(str x, str y)
	| unlabel(str x) // ???
	| upgrade(BGFProduction p1, BGFProduction p2) // p1 is marked
	| vertical(BGFContext w)
	| widen(BGFExpression e1, BGFExpression e2, BGFContext w)
	| yaccify(list[BGFProduction] ps)
	// legacy
	| atomic(list[XBGFCommand] exprs)
	| strip(str a)
;

data BGFContext =
	globally()
;
