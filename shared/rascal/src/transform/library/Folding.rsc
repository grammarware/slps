@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::library::Folding

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::Results;
import transform::library::Util;
import transform::library::Brutal;

XBGFResult runExtract(production(str l, str x, BGFExpression rhs), XBGFScope w, grammar(rs,ps))
{
	XBGFOutcome r = ok();
	if (x in definedNs(ps))
		r = notFreshN(r,x);
	// TODO hard to check if rhs occurs in the grammar; it was somehow done in xbgf1.pro 
	XBGFResult rep = transform::library::Brutal::runReplace(rhs,nonterminal(x),w,grammar(rs,ps));
	return <add(r,rep.r),grammar(rep.g.roots,rep.g.prods + production(l,x,rhs))>;
}

XBGFResult runFold(str x, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (<_,[production(_, x, BGFExpression rhs)],_> := splitPbyW(g.prods,innt(x)))
		return add(r,transform::library::Brutal::runReplace(rhs,nonterminal(x),comboscope(notinnt(x),w),g));
	else 
		return <problemStr("Nonterminal must be defined horizontally prior to folding.",x),g>;
}

XBGFResult runInline(str x, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (<ps1,[production(_, x, BGFExpression rhs)],ps2> := splitPbyW(g.prods,innt(x)))
		return add(r,transform::library::Brutal::runReplace(nonterminal(x),rhs,globally(),grammar(g.roots,ps1+ps2)));
	else 
		return <problemStr("Nonterminal must be defined horizontally prior to inlining.",x),g>;
}

XBGFResult runUnfold(str x, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (<_,[production(_, x, BGFExpression rhs)],_> := splitPbyW(g.prods,innt(x)))
		return add(r,transform::library::Brutal::runReplace(nonterminal(x),rhs,comboscope(notinnt(x),w),g));
	else
		return <problemStr("Nonterminal must be defined horizontally prior to unfolding.",x),g>;
}

