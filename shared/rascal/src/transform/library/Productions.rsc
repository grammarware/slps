@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{addV,define,eliminate,importG,introduce,redefine,removeV,undefine}
module transform::library::Productions

import lib::Rascalware;
import language::BGF;
import language::XScope;
import language::XOutcome;
import transform::library::Util;
import transform::library::Brutal;
import diff::GDT;

XBGFResult runAddV(BGFProduction p1, BGFGrammar g)
{
	if (production(_,str x,_) := p1)
	{
		<ps1,ps2,ps3> = splitPbyW(g.prods,innt(x));
		if (isEmpty(ps2))
			return <problemStr("Nonterminal must be defined",x),g>;
		if (p1 in ps2)
			return <problemProd("Production rule is already present",p1),g>;
		if (production(str l,_,_) := p1 && l != "")
			if (production(str l,_,_) <- ps)
				return <problemStr("Another production rule with the same label is already present",l),g>;
		return <ok(),grammar(g.roots, ps1 + ps2 + p1 + ps3)>;
	}
}

XBGFResult runDefine(list[BGFProduction] ps1, BGFGrammar g)
{
	if ({str n} := definedNs(ps1))
	{
		if (n notin usedNs(g.prods))
			return <problemStr("Nonterminal must not be fresh, use introduce instead",n),g>;
		return <ok(),grammar(g.roots, g.prods + ps1)>;
	}
	else
		return <problem("Multiple defined nonterminals found"),g>;
}

XBGFResult runEliminate(str x, BGFGrammar g)
{
	// TODO: can we eliminate root?
	if (x in g.roots)
		return <problemStr("Cannot eliminate root nonterminal",x),g>;
	if (x notin definedNs(g.prods))
		return <freshName(x),g>;
	<ps1,_,ps3> = splitPbyW(g.prods,innt(x));
	if (x in usedNs(ps1+ps3))
		return <notFreshName(x),g>;
	return <ok(),grammar(g.roots, ps1 + ps3)>;
}

XBGFResult runImportG(list[BGFProduction] ps1, BGFGrammar g)
{
	defs1 = definedNs(ps1);
	defs12 = defs1 & definedNs(g.prods);
	du12 = defs1 & usedNs(g.prods);
	if (!isEmpty(defs12))
		return <problemStrs("Import clashes with existing definitions", toList(defs12)),g>;
	if (!isEmpty(du12))
		return <problemStrs("Import clashes with existing definitions", toList(du12)),g>;
	return <ok(),grammar(g.roots, ps1 + g.prods)>;
}

XBGFResult runIntroduce(list[BGFProduction] ps, BGFGrammar g)
{
	if ({str n} := definedNs(ps))
	{
		if (n in usedNs(g.prods))
			return <notFreshN(n),g>;
		if (n in definedNs(g.prods))
			return <problemStr("Definition for nonterminal clashes with existing definition",n),g>;
		return <ok(),grammar(g.roots, g.prods + ps)>;
	}
	else
		return <problem("Multiple defined nonterminals found"),g>;
}

XBGFResult runRedefine(list[BGFProduction] ps1, BGFGrammar g)
{
	// inlined superposition of undefine and define, with two exceptional details:
	// (1) if ps1.n is a root, undefine would have stripped it from this status
	// (2) redefine preserves original order of production rules
	if ({str x} := definedNs(ps1))
	{
		if (x notin definedNs(g.prods))
			return <freshName(x),g>;
		if (x notin usedNs(g.prods))
			return <problemStr("Nonterminal must be used",x),g>;
		<ps3,_,ps4> = splitPbyW(g.prods,innt(x));
		return <ok(),grammar(g.roots, ps3 + ps1 + ps4)>; 
	}
}

XBGFResult runRemoveV(BGFProduction p, BGFGrammar g)
{
	<_,ps2,_> = splitPbyW(g.prods, innt(p.lhs));
	if (isEmpty(ps2))
		return <freshN(p.lhs),g>;
	if (!inProds(p,ps2))
		return <notFoundP(p),g>;
	if ([p] == ps2)
		return <problemStr("Cannot remove the last production rule with removeV, use undefine or eliminate",p.lhs),g>;
	return <ok(),grammar(g.roots, g.prods - p)>;
}

//TODO: undefine only one nonterminal per transformation
XBGFResult runUndefine(list[str] xs, BGFGrammar g)
{
	list[BGFProduction] myps = g.prods;
	list[str] rs = g.roots;
	for (str x <- xs)
	{
		if (x notin definedNs(g.prods))
			return <problemStr("Nonterminal must be defined.",x),g>;
		if (x notin usedNs(g.prods))
			return <problemStr("Nonterminal must be used.",x),g>;
		//throw "Cannot undefine root nonterminal <x>."; // check was not in xbgf1.pro
		rs -= x;
		<ps1,_,ps3> = splitPbyW(myps,innt(x));
		myps = ps1 + ps3;
	}
	return <ok(),grammar(g.roots,myps)>;
}
