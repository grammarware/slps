@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::library::Sequential

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::Results;
import transform::library::Util;
import transform::library::Brutal;
import diff::GDT;

XBGFResult runAppear(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	p2 = demark(p1);
	if (!inProds(p2,g.prods))
		r = notFoundP(r,p2);
	for (/marked(e) := p1)
		if (optional(_) !:= e && star(_) !:= e)
			r = add(r,problemProd("Production rule does not have an optional part marked",p1));
	return <r,grammar(g.roots, replaceP(g.prods,p2,unmark(p1)))>;
}

XBGFResult runDisappear(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	p2 = unmark(p1);
	if (!inProds(p2,g.prods))
		r = notFoundP(r,p2);
	for (/marked(e) := p1)
		if (optional(_) !:= e && star(_) !:= e)
			r = add(r,problemProd("Production rule does not have an optional part marked",p2));
	return <r,grammar(g.roots, replaceP(g.prods,p2,demark(p1)))>;
}

XBGFResult runInject(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	p2 = demark(p1);
	if (!inProds(p2,g.prods))
		r = notFoundP(r,p2);
	return <r,grammar(g.roots, replaceP(g.prods,p2,unmark(p1)))>;
}

XBGFResult runPermute(BGFProduction p, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (production(str l, str n, sequence(L1)) := p)
	{
		<ps1,ps2,ps3> = splitPbyW(g.prods,innt(n));
		if ([production(_, n, sequence(L2))] := ps2)
		{
			if (seteq(L1,L2))
				return <r,grammar(g.roots, ps1 + p + ps3)>;
			else
				r = add(r,problemExpr2("Phrases must be permutations of each other",sequence(L1),sequence(L2)));
		}
		else
			r = add(r,problemProds("Permutation requires a single sequence instead of",ps2));
	}
	else
		r = add(r,problemProd("Permutation parameter requires a sequence instead of",p));
	return <r,g>;
}

XBGFResult runProject(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	p2 = unmark(p1);
	if (!inProds(p2,g.prods))
		r = notFoundP(r,p2);
	return <r, grammar(g.roots, replaceP(g.prods, p2, demark(p1)))>;
}

