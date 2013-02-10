@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{extract,fold,inline,unfold,downgrade,upgrade}
module transform::library::Folding

import lib::Rascalware;
import language::BGF;
import language::XScope;
import language::XOutcome;
import normal::BGF;
import transform::library::Util;
import transform::library::Brutal;

XBGFResult runExtract(production(str l, str x, BGFExpression rhs), XBGFScope w, BGFGrammar g)
{
	if (x in definedNs(g.prods))
		return <notFreshN(x),g>;
	// TODO hard to check if rhs occurs in the grammar; it was somehow done in xbgf1.pro 
	XBGFResult rep = transform::library::Brutal::runReplace(rhs,nonterminal(x),w,g);
	if (ok() !:= rep.r) return rep;
	else return <ok(),grammar(rep.g.roots,rep.g.prods + production(l,x,rhs))>;
}

XBGFResult runFold(str x, XBGFScope w, BGFGrammar g)
{
	if (<_,[production(_, x, BGFExpression rhs)],_> := splitPbyW(g.prods,innt(x)))
		return transform::library::Brutal::runReplace(rhs,nonterminal(x),comboscope(notinnt(x),w),g);
	else 
		return <problemStr("Nonterminal must be defined horizontally prior to folding.",x),g>;
}

XBGFResult runInline(str x, BGFGrammar g)
{
	if (<ps1,[production(str l, x, BGFExpression rhs)],ps2> := splitPbyW(g.prods,innt(x)))
	{
		if (l=="")
			return transform::library::Brutal::runReplace(nonterminal(x),rhs,globally(),grammar(g.roots,ps1+ps2));
		else
			return transform::library::Brutal::runReplace(nonterminal(x),selectable(l,rhs),globally(),grammar(g.roots,ps1+ps2));
	}
	else 
		return <problemStr("Nonterminal must be defined horizontally prior to inlining.",x),g>;
}

XBGFResult runUnfold(str x, XBGFScope w, BGFGrammar g)
{
	if (<_,[production(str l, x, BGFExpression rhs)],_> := splitPbyW(g.prods,innt(x)))
	{
		if (l=="")
			return transform::library::Brutal::runReplace(nonterminal(x),rhs,comboscope(notinnt(x),w),g);
		else
			return transform::library::Brutal::runReplace(nonterminal(x),selectable(l,rhs),comboscope(notinnt(x),w),g);
	}
	else
		return <problemStr("Nonterminal must be defined horizontally prior to unfolding.",x),g>;
}

// Liberal forms of folding
XBGFResult runDowngrade(BGFProduction p1, BGFProduction p2, BGFGrammar g)
{
	if (/marked(nonterminal(str x)) := p1)
		if (production(str l,x,BGFExpression e) := p2)
		{
			p3 = visit(p1){case marked(_) => e};
			return <ok(),grammar(g.roots,replaceP(g.prods,unmark(p1),normalise(p3)))>;
		}
		else
			return <problemProd2("Production rules do not agree on nonterminal",p1,p2),g>;
	else
		return <problemProd("Production rule does not have a single nonterminal marked",p1),g>;
}

XBGFResult runUpgrade(BGFProduction p1, BGFProduction p2, BGFGrammar g)
{
	if (/marked(nonterminal(str x)) := p1)
		if (production(str l,x,BGFExpression e) := p2)
		{
			p3 = visit(p1){case marked(_) => e};
			p3 = normalise(p3);
			return <ok(),grammar(g.roots,replaceP(g.prods,p3,unmark(p1)))>;
		}
		else
			return <problemProd2("Production rules do not agree on nonterminal",p1,p2),g>;
	else
		return <problemProd("Production rule must have one single nonterminal marked",p1),g>;
}
