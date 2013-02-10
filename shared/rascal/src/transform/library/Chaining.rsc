@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{abridge,chain,detour,unchain}
module transform::library::Chaining

import lib::Rascalware;
import language::BGF;
import language::XScope;
import language::XOutcome;
import transform::library::Util;
import transform::library::Brutal;
import diff::GDT;

XBGFResult runAbridge(BGFProduction p, BGFGrammar g)
{
	if (production(_,x,nonterminal(x)) !:= p)
		return <problemProd("Production cannot be abridged.",p),g>;
	if (!inProds(p,g.prods))
		return <notFoundP(p),g>;
	return <ok(),grammar(g.roots, g.prods - p)>;
}

XBGFResult runChain(BGFProduction p, BGFGrammar g)
{
	if (production(str l,str n1,nonterminal(str n2)) := p)
		{
			if (n1 == n2)
				return <problem("Do not introduce reflexive chain productions with chain, use detour instead"),g>;
			if (n2 in allNs(g.prods))
				return <notFreshName(n2),g>;
			list[BGFProduction] ps1,ps2,ps3;
			if (l != "") <ps1,ps2,ps3> = splitPbyW(g.prods,inlabel(l));
			else <ps1,ps2,ps3> = splitPbyW(g.prods,innt(n1));
			if ([production(l,n1,e)] := ps2)
				return <ok(),grammar(g.roots, ps1 + p + production("",n2,e) + ps3)>;
			else
				return <problemProds("Production rule has unexpected form",ps2),g>;
		}
	else
		return <problemProd("Not a chain production rule.",p),g>;
}

XBGFResult runDetour(BGFProduction p, BGFGrammar g)
{
	if (production(_,x,nonterminal(x)) := p)
	{
		// xbgf1.pro only aksed for x to be used, not necessarily defined; we're more strict here
		if (x notin definedNs(g.prods))
			return <freshN(r,x),g>;
		<ps1,ps2,ps3> = splitPbyW(g.prods,innt(x));
		return <ok(),grammar(g.roots, ps1 + ps2 + p + ps3)>;
	}
	else
		return <problemProd("Not a reflexive chain production rule",p),g>;
}

XBGFResult runUnchain(BGFProduction p, BGFGrammar g)
{
	if (production(str l,str n1,nonterminal(str n2)) := p)
		{
			if (n1 == n2)
				return <problem("Do not remove reflexive chain productions with chain, use abridge instead"),g>;
			if (n2 in g.roots)
				return <problemStr("Nonterminal must not be root",n2),g>;
			if (!inProds(p,g.prods))
				return <notFoundP(r,p),g>;
			//if (n2 in allNs(ps)) r = notFreshN(r,n2);
			list[BGFProduction] ps1,ps2,ps3;
			<ps1,ps2,ps3> = splitPbyW(g.prods - p,innt(n2));
			if (len(ps2) != 1)
				return <problemStr("Nonterminal must occur exactly once",n2),g>;
			if (l == "")
				l = n2;
			if ([production(_,n2,e)] := ps2)
				return <ok(),grammar(g.roots, ps1 + production(l,n1,e) + ps3)>;
			else
				return <problemProds("Production rules have unexpected form",ps2),g>;
		}
	else
		return <problemProd("Not a chain production rule",p),g>;
}

