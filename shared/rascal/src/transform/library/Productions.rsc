@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::library::Productions

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::Results;
import transform::library::Util;
import transform::library::Brutal;
import diff::GDT;

XBGFResult runAddV(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (production(_,str x,_) := p1)
	{
		<ps1,ps2,ps3> = splitPbyW(g.prods,innt(x));
		if (isEmpty(ps2))
			r = add(r,problemStr("Nonterminal must be defined",x));
		if (p1 in ps2)
			r = add(r,problemProd("Production rule is already present",p1));
		if (production(str l,_,_) := p1 && l != "")
			if (production(str l,_,_) <- ps)
				r = add(r,problemStr("Another production rule with the same label is already present",l));
		return <r,grammar(g.roots, ps1 + ps2 + p1 + ps3)>;
	}
}

XBGFResult runDefine(list[BGFProduction] ps1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if ({str n} := definedNs(ps1))
	{
		if (n notin usedNs(g.prods))
			r = add(r,problemStr("Nonterminal must not be fresh, use introduce instead",n));
		return <r,grammar(g.roots, g.prods + ps1)>;
	}
	else
		return <problem("Multiple defined nonterminals found"),g>;
}

XBGFResult runEliminate(str x, BGFGrammar g)
{
	XBGFOutcome r = ok();
	// TODO: can we eliminate root?
	if (x in g.roots)
		r = add(r,problemStr("Cannot eliminate root nonterminal",x));
	if (x notin definedNs(g.prods))
		r = add(r,problemStr("Nonterminal must be defined",x));
	<ps1,_,ps3> = splitPbyW(g.prods,innt(x));
	if (x in usedNs(ps1+ps3))
		r = add(r,problemStr("Nonterminal must not be used",x));
	return <r,grammar(g.roots, ps1 + ps3)>;
}

XBGFResult runImportG(list[BGFProduction] ps1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	defs1 = definedNs(ps1);
	defs12 = defs1 & definedNs(g.prods);
	du12 = defs1 & usedNs(g.prods);
	if (!isEmpty(defs12))
		r = add(r,problemProds("Import clashes with existing definitions", defs12));
	if (!isEmpty(du12))
		r = add(r,problemProds("Import clashes with existing definitions", du12));
	return <r,grammar(g.roots, ps1 + g.prods)>;
}

XBGFResult runIntroduce(list[BGFProduction] ps, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if ({str n} := definedNs(ps))
	{
		if (n in usedNs(g.prods))
			r = notFreshN(r,n);
		if (n in definedNs(g.prods))
			r = add(r,problemStr("Definition for nonterminal clashes with existing definition",n));
		return <r,grammar(g.roots, g.prods + ps)>;
	}
	else
		return <problem("Multiple defined nonterminals found"),g>;
}

XBGFResult runRedefine(list[BGFProduction] ps1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	// inlined superposition of undefine and define, with two exceptional details:
	// (1) if ps1.n is a root, undefine would have stripped it from this status
	// (2) redefine preserves original order of production rules
	if ({str x} := definedNs(ps1))
	{
		if (x notin definedNs(g.prods))
			r = add(r,problemStr("Nonterminal must be defined",x));
		if (x notin usedNs(g.prods))
			r = add(r,problemStr("Nonterminal must be used",x));
		<ps3,_,ps4> = splitPbyW(g.prods,innt(x));
		return <r,grammar(g.roots, ps3 + ps1 + ps4)>; 
	}
}

XBGFResult runRemoveV(BGFProduction p, BGFGrammar g)
{
	XBGFOutcome r = ok();
	<_,ps2,_> = splitPbyW(g.prods, innt(p.lhs));
	if (isEmpty(ps2))
		r = freshN(r, p.lhs);
	if (!inProds(p,ps2))
		r = notFoundP(r,p);
	if ([p] == ps2)
		r = add(r,problemStr("Cannot remove the last production rule with removeV, use undefine or eliminate",p.lhs));
	return <r,grammar(g.roots, g.prods - p)>;
}

//TODO: undefine only one nonterminal per transformation
XBGFResult runUndefine(list[str] xs, BGFGrammar g)
{
	XBGFOutcome r = ok();
	list[BGFProduction] myps = g.prods;
	list[str] rs = g.roots;
	for (str x <- xs)
	{
		if (x notin definedNs(g.prods))
			r = add(r,"Nonterminal must be defined.",x);
		if (x notin usedNs(g.prods))
			r = add(r,"Nonterminal must be used.",x);
		//throw "Cannot undefine root nonterminal <x>."; // check was not in xbgf1.pro
		rs -= x;
		<ps1,_,ps3> = splitPbyW(myps,innt(x));
		myps = ps1 + ps3;
	}
	return <r,grammar(g.roots,myps)>;
}
