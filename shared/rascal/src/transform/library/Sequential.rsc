@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{appear,disappear,inject,permute,project}
module transform::library::Sequential

import lib::Rascalware;
import language::BGF;
import language::XScope;
import language::XOutcome;
import transform::library::Util;
import transform::library::Brutal;
import diff::GDT;

XBGFResult runAppear(BGFProduction p1, BGFGrammar g)
{
	p2 = demark(p1);
	if (!inProds(p2,g.prods))
		return <notFoundP(p2),g>;
	for (/marked(e) := p1)
		if (optional(_) !:= e && star(_) !:= e)
			return <problemProd("Production rule does not have an optional part marked",p1),g>;
	return <ok(),grammar(g.roots, replaceP(g.prods,p2,unmark(p1)))>;
}

XBGFResult runDisappear(BGFProduction p1, BGFGrammar g)
{
	p2 = unmark(p1);
	if (!inProds(p2,g.prods))
		return <notFoundP(p2),g>;
	for (/marked(e) := p1)
		if (optional(_) !:= e && star(_) !:= e)
			return <problemProd("Production rule does not have an optional part marked",p2),g>;
	return <ok(),grammar(g.roots, replaceP(g.prods,p2,demark(p1)))>;
}

XBGFResult runInject(BGFProduction p1, BGFGrammar g)
{
	p2 = demark(p1);
	if (!inProds(p2,g.prods))
		return <notFoundP(p2),g>;
	return <ok(),grammar(g.roots, replaceP(g.prods,p2,unmark(p1)))>;
}

XBGFResult runPermute(BGFProduction p, BGFGrammar g)
{
	if (production(str l, str n, sequence(L1)) := p)
	{
		<ps1,ps2,ps3> = splitPbyW(g.prods,innt(n));
		if ([production(_, n, sequence(L2))] := ps2)
		{
			if (seteq(L1,L2))
				return <ok(),grammar(g.roots, ps1 + p + ps3)>;
			else
				return <problemExpr2("Phrases must be permutations of each other",sequence(L1),sequence(L2)),g>;
		}
		else
			return <problemProds("Permutation requires a single sequence instead of",ps2),g>;
	}
	else
		return <problemProd("Permutation parameter requires a sequence instead of",p),g>;
	// TODO check if really dead code
	return <ok(),g>;
}

XBGFResult runProject(BGFProduction p1, BGFGrammar g)
{
	p2 = unmark(p1);
	if (!inProds(p2,g.prods))
		return <notFoundP(p2),g>;
	return <ok(), grammar(g.roots, replaceP(g.prods, p2, demark(p1)))>;
}

