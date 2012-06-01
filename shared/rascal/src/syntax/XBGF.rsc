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
	| clone(str x, str y, XBGFContext w)
	| concatT(list[str] xs, str y, XBGFContext w)
	| concretize(BGFProduction p) // marked
	| deanonymize(BGFProduction p)
	| define(list[BGFProduction] ps)
	| designate(BGFProduction p)
	| detour(BGFProduction p)
	| deyaccify(str x)
	| disappear(BGFProduction p) // marked
	| distribute(XBGFContext w)
	| downgrade(BGFProduction p1,BGFProduction p2) // p1 is marked
	| eliminate(str x)
	| equate(str x, str y)
	| extract(BGFProduction p, XBGFContext w)
	| factor(BGFExpression e1, BGFExpression e2, XBGFContext w)
	| fold(str x, XBGFContext w)
	| horizontal(XBGFContext w)
	| \import(list[BGFProduction] ps)
	| inject(BGFProduction p) // marked
	| inline(str x)
	| introduce(list[BGFProduction] ps)
	| iterate(BGFProduction p)
	| lassoc(BGFProduction p)
	| massage(BGFExpression e1, BGFExpression e2, XBGFContext w)
	| narrow(BGFExpression e1, BGFExpression e2, XBGFContext w)
	| permute(BGFProduction p)
	| project(BGFProduction p) // marked
	| rassoc(BGFProduction p)
	| redefine(list[BGFProduction] ps)
	| removeH(BGFProduction p) // marked
	| removeV(BGFProduction p)
	| renameL(str x, str y, XBGFContext w)
	| renameN(str x, str y, XBGFContext w)
	| renameS(str x, str y, XBGFContext w)
	| renameT(str x, str y, XBGFContext w)
	| replace(BGFExpression e1, BGFExpression e2, XBGFContext w)
	| reroot(list[str] xs)
	//| splitN(list[BGFProduction] ps, list[BGFProduction] qs, XBGFContext w)
	| splitN(str x, list[BGFProduction] ps, XBGFContext w)
	| splitT(str x, list[str] ys, XBGFContext w)
	| unchain(BGFProduction p)
	| undefine(list[str] xs)
	| unfold(str x, XBGFContext w)
	| unite(str x, str y)
	| unlabel(str x) // ???
	| upgrade(BGFProduction p1, BGFProduction p2) // p1 is marked
	| vertical(XBGFContext w)
	| widen(BGFExpression e1, BGFExpression e2, XBGFContext w)
	| yaccify(list[BGFProduction] ps)
	// legacy
	| atomic(list[XBGFCommand] steps)
	| strip(str a)
;

data XBGFContext =
	globally()
	| inlabel(str l)
	| innt(str x)
	// TODO: combination
;
