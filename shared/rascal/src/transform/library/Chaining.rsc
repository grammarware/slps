@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::library::Chaining

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::Results;
import transform::library::Util;
import transform::library::Brutal;
import diff::GDT;

XBGFResult runAbridge(BGFProduction p, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (production(_,x,nonterminal(x)) !:= p)
		r = add(r,problemProd("Production cannot be abridged.",p));
	if (!inProds(p,g.prods))
		r = notFoundP(r,p);
	return <r,grammar(g.roots, g.prods - p)>;
}

XBGFResult runChain(BGFProduction p, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (production(str l,str n1,nonterminal(str n2)) := p)
		{
			if (n1 == n2)
				r = add(r,problem("Do not introduce reflexive chain productions with chain, use detour instead"));
			if (n2 in allNs(g.prods))
				r = add(r,problemStr("Nonterminal must be fresh",n2));
			list[BGFProduction] ps1,ps2,ps3;
			if (l != "") <ps1,ps2,ps3> = splitPbyW(g.prods,inlabel(l));
			else <ps1,ps2,ps3> = splitPbyW(g.prods,innt(n1));
			if ([production(l,n1,e)] := ps2)
				return <r,grammar(g.roots, ps1 + p + production("",n2,e) + ps3)>;
			else
				return <add(r,problemProds("Production rule has unexpected form",ps2)),g>;
		}
	else
		return <problemProd("Not a chain production rule.",p),g>;
}

XBGFResult runDetour(BGFProduction p, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (production(_,x,nonterminal(x)) := p)
	{
		// xbgf1.pro only aksed for x to be used, not necessarily defined; we're more strict here
		if (x notin definedNs(g.prods))
			r = freshN(r,x);
		<ps1,ps2,ps3> = splitPbyW(g.prods,innt(x));
		return <r,grammar(g.roots, ps1 + ps2 + p + ps3)>;
	}
	else
		return <problemProd("Not a reflexive chain production rule",p),g>;
}

XBGFResult runUnchain(BGFProduction p, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (production(str l,str n1,nonterminal(str n2)) := p)
		{
			if (n1 == n2)
				r = add(r,problem("Do not remove reflexive chain productions with chain, use abridge instead"));
			if (n2 in g.roots)
				r = add(r,problemStr("Nonterminal must not be root",n2));
			if (!inProds(p,g.prods))
				r = notFoundP(r,p);
			//if (n2 in allNs(ps)) r = notFreshN(r,n2);
			list[BGFProduction] ps1,ps2,ps3;
			<ps1,ps2,ps3> = splitPbyW(g.prods - p,innt(n2));
			if (len(ps2) != 1)
				r = add(r,problemStr("Nonterminal must occur exactly once",n2));
			if (l == "")
				l = n2;
			if ([production(_,n2,e)] := ps2)
				return <r,grammar(g.roots, ps1 + production(l,n1,e) + ps3)>;
			else
				return <add(r,problemProds("Production rules have unexpected form",ps2)),g>;
		}
	else
		return <add(r,problemProd("Not a chain production rule",p)),g>;
}

