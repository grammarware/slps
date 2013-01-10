@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::library::Nonterminals

import syntax::BGF;
import syntax::XBGF;
import transform::Results;
import transform::library::Util;
import transform::library::Brutal;
import diff::GDT;
import List; // size

XBGFResult runEquate(str x, str y, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (x == y)
		r = problemStr("Nonterminal is already equated with itself.",x);
	<ps1x,ps2x,ps3x> = splitPbyW(g.prods,innt(x));
	<_,ps2y,_> = splitPbyW(g.prods,innt(y));
	XBGFResult rep = runRenameN(x,y,grammar([],ps2x));
	r = add(r,rep.r);
	gxy = rep.g;
	gyy = grammar([],ps2y);
	if (!gdts(gxy,gyy))
		r = add(r,problemStr2("Definitions of nonterminals must be equal.",x,y));
	if (x in usedNs(ps1x + ps3x))
		return add(r,transform::library::Brutal::runReplace(nonterminal(x),nonterminal(y),globally(),grammar(g.roots - x,ps1x + ps3x)));
	else
		return <r,grammar(g.roots - x,ps1x + ps3x)>;
}

XBGFResult runRenameN(str x, str y, BGFGrammar g)
{
	XBGFOutcome r = ok();
	ns = allNs(g.prods);
	if (x notin ns)
		r = freshN(r,x);
	if (y in ns)
		r = notFreshN(r,y);
	return
		<r,performRenameN(x,y,g)>;
}

BGFGrammar performRenameN(str x, str y, grammar(rs, ps))
{
	list[BGFProduction] ps1,ps2,ps3,ps4;
	list[str] rs2;
	if ([*L1, x, *L2] := rs) rs2 = L1 + y + L2;
	else rs2 = rs;
	if (x in definedNs(ps))
	{
		<ps1,ps2,ps3> = splitPbyW(ps,innt(x));
		ps4 = ps1 + [production(l,y,e) | p <- ps2, production(str l,x,BGFExpression e) := p] + ps3;
	}
	else
		ps4 = ps; 
	if (x in usedNs(ps4))
		return grammar(rs2,transform::library::Brutal::performReplace(nonterminal(x),nonterminal(y),ps4));
	else
		return grammar(rs2,ps4);
}

XBGFResult runSplitN(str x, list[BGFProduction] ps0, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if ({str y} := definedNs(ps0))
	{
		if (x notin definedNs(g.prods))
			r = freshN(r,x);
		if (y in allNs(g.prods))
			r = notFreshN(r,y);
		<ps2,ps3,ps4> = splitPbyW(g.prods,innt(x));
		list[BGFProduction] ps5 = [production(l,x,e) | p <- ps0, production(str l,y,BGFExpression e) := p];
		if (x in g.roots) rs2 = g.roots + y; else rs2 = g.roots;
		g = grammar(rs2,ps2 + (ps3 - ps5) + ps0 + ps4);
		if (nowhere() := w)
			return <r,g>;
		else
			return add(r,transform::library::Brutal::runReplace(nonterminal(x),nonterminal(y),w,g));
	}
	else
		return <problem("Splitting into more than two nonterminals not supported"),g>;
		// TODO OR NOT TODO
}

