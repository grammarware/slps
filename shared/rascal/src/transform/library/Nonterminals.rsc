@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{clone,equate,renameN,reroot,splitN,unite}
module transform::library::Nonterminals

import lib::Rascalware;
import language::BGF;
import language::XScope;
import language::XOutcome;
import transform::library::Util;
import transform::library::Brutal;
import diff::GDT;

XBGFResult runClone(str x, str y, XBGFScope w, BGFGrammar g)
{
	// TODO
	return <ok(),g>;
}

XBGFResult runEquate(str x, str y, BGFGrammar g)
{
	if (x == y)
		return <problemStr("Nonterminal is already equated with itself.",x),g>;
	<ps1x,ps2x,ps3x> = splitPbyW(g.prods,innt(x));
	<_,ps2y,_> = splitPbyW(g.prods,innt(y));
	XBGFResult rep = runRenameN(x,y,grammar([],ps2x));
	if (ok() !:= rep.r) return rep;
	gxy = rep.g;
	gyy = grammar([],ps2y);
	if (!gdts(gxy,gyy))
		return <problemStr2("Definitions of nonterminals must be equal.",x,y),g>;
	if (x in usedNs(ps1x + ps3x))
		return transform::library::Brutal::runReplace(nonterminal(x),nonterminal(y),globally(),grammar(g.roots - x,ps1x + ps3x));
	else
		return <ok(),grammar(g.roots - x,ps1x + ps3x)>;
}

XBGFResult runRenameN(str x, str y, BGFGrammar g)
{
	ns = allNs(g.prods);
	if (x notin ns)
		return <freshN(x),g>;
	if (y in ns)
		return <notFreshN(y),g>;
	return
		<ok(),performRenameN(x,y,g)>;
}

BGFGrammar performRenameN(str x, str y, BGFGrammar g)
{
	list[BGFProduction] ps1,ps2,ps3,ps4;
	list[str] rs2;
	if ([*L1, x, *L2] := g.roots) rs2 = L1 + y + L2;
	else rs2 = g.roots;
	if (x in definedNs(g.prods))
	{
		<ps1,ps2,ps3> = splitPbyW(g.prods,innt(x));
		ps4 = ps1 + [production(l,y,e) | p <- ps2, production(str l,x,BGFExpression e) := p] + ps3;
	}
	else
		ps4 = g.prods;
	if (x in usedNs(ps4))
		return grammar(rs2,transform::library::Brutal::performReplace(nonterminal(x),nonterminal(y),ps4));
	else
		return grammar(rs2,ps4);
}

XBGFResult runReroot(list[str] xs, BGFGrammar g)
{
	if (seteq(xs, g.roots))
		return <problemStrs("Vacuous reroot",xs),g>;
	// xbgf1.pro only asked for it to be a subset of allNs, not definedNs; we're more strict here
	if (subset(xs,definedNs(g.prods)))
		return <ok(),grammar(xs, g.prods)>;
	else
		return <problemStrs("Not all nonterminals are defined",xs),g>;
}

XBGFResult runSplitN(str x, list[BGFProduction] ps0, XBGFScope w, BGFGrammar g)
{
	if ({str y} := definedNs(ps0))
	{
		if (x notin definedNs(g.prods))
			return <freshN(x),g>;
		if (y in allNs(g.prods))
			return <notFreshN(y),g>;
		<ps2,ps3,ps4> = splitPbyW(g.prods,innt(x));
		list[BGFProduction] ps5 = [production(l,x,e) | p <- ps0, production(str l,y,BGFExpression e) := p];
		if (x in g.roots) rs2 = g.roots + y; else rs2 = g.roots;
		g = grammar(rs2,ps2 + (ps3 - ps5) + ps0 + ps4);
		if (nowhere() := w)
			return <ok(),g>;
		else
			return transform::library::Brutal::runReplace(nonterminal(x),nonterminal(y),w,g);
	}
	else
		return <problem("Splitting into more than two nonterminals not supported"),g>;
		// TODO OR NOT TODO
}

XBGFResult runUnite(str x, str y, BGFGrammar g)
{
	if (x == y)
		return <problemStr("Nonterminal is already united with itself",x),g>;
	used = allNs(g.prods);
	if (x notin used)
		return <freshN(x),g>;
	if (y notin used)
		return <freshN(y),g>;
	<ps1x,ps2x,ps3x> = splitPbyW(g.prods, innt(x));
	list[BGFProduction] ps4x = ps1x + [production(l,y,e) | p <- ps2x, production(str l,x,BGFExpression e) := p] + ps3x;
	if (x in usedNs(ps4x))
		return transform::library::Brutal::runReplace(nonterminal(x),nonterminal(y),globally(),grammar(g.roots,ps4x));
	else
		return <ok(),grammar(g.roots,ps4x)>;
}

