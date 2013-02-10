@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{addH,horizontal,removeH,vertical}
module transform::library::Disjunction

//import lib::Rascalware;
import language::BGF;
import language::XScope;
import language::XOutcome;
import transform::library::Util;

XBGFResult runAddH(BGFProduction p1, BGFGrammar g)
{
	p2 = unmark(p1);
	p3 = demarkH(p1);
	if (!inProds(p3,g.prods))
		return <notFoundP(p3),g>;
	return <ok(),grammar(g.roots, replaceP(g.prods,p3,p2))>;
}

XBGFResult runHorizontal(XBGFScope w, BGFGrammar g)
{
	// For xbgf1.pro, the context must be strictly vertical. Here we are more relaxed. 
	<ps1,ps2,ps3> = splitPbyW(g.prods,w);
	list[BGFExpression] es4 = [];
	for (production(str l, str x, BGFExpression e) <- ps2)
		if (choice(L) := e)
			es4 += L;
		elseif (l == "")
			es4 += e;
		else
			es4 += selectable(l,e);
	if (innt(str x) := w)
		return <ok(),grammar(g.roots,ps1 + production("",x,choice(es4)) + ps3)>;
	else
		return <problemScope("Scope for horizontal must be a nonterminal",w),g>;
}

XBGFResult runRemoveH(BGFProduction p1, BGFGrammar g)
{
	p2 = unmark(p1);
	if (!inProds(p2, g.prods))
		return <notFoundP(p2),g>;
	return <ok(),grammar(g.roots, replaceP(g.prods,p2,demarkH(p1)))>;
}

XBGFResult runVertical(XBGFScope w, BGFGrammar g)
{
	<ps1,ps2,ps3> = splitPbyW(g.prods, w);
	ps4 = [];
	for (production(str l, str x, BGFExpression e) <- ps2)
		if (choice(L) := e)
			for (se <- L)
				if (selectable(str s, BGFExpression e2) := se)
					if (/production(s,_,_) := g.prods)
						return <problemStr("Outermost selector clashes with an existing label",s),g>;
					elseif (/production(s,_,_) := ps4)
						return <problemStr("Outermost selectors ambiguous",s),g>;
					else
						ps4 += production(s,x,e2);
				else
					ps4 += production("",x,se);
		else ps4 += production(l,x,e);
	return <ok(),grammar(g.roots, ps1 + ps4 + ps3)>;
}
