@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::library::Labels

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::Results;
import transform::library::Util;

 // true labels
XBGFResult runDesignate(production(str l,str n,BGFExpression e), BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (l == "")
		r = add(r,problemProd("Production rule must me labelled, use unlabel instead",production(l,n,e)));
	if (production("",n,e) notin g.prods)
		// throw "Production rule defining <n> as <e> not found.";
		r = add(r,problemProd("Production rule not found, use renameL instead",production("",n,e)));
	return <r,grammar(g.roots,replaceP(g.prods,production("",n,e),production(l,n,e)))>;
}

XBGFResult runRenameL(str x, str y, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (x == "")
		r = add(r, problem("Source label must not be empty for renaming, use designate"));
	if (y == "")
		r = add(r, problem("Target label must not be empty for renaming, use unlabel"));
	if (len([p | p <- g.prods, production(x, _, _) := p]) != 1)
		r = add(r, problemStr("Source name for renaming must be uniquely used",x));
	if (len([p | p <- g.prods, production(y, _, _) := p]) != 0)
		r = add(r, problemStr("Target name for renaming must be fresh",y));
	<ps1,ps2,ps3> = splitPbyW(g.prods, inlabel(x));
	if ([production(x, str n, BGFExpression e)] := ps2)
		return <r,grammar(g.roots, ps1 + production(y, n, e) + ps3)>;
	else
		return <add(r,problemStr("Label not found or not unique",x)),g>; // the latter should never happen
}

XBGFResult runUnlabel(str x, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (x == "")
		r = add(r,problem("Please specify which label to unlabel"));
	<ps1,ps2,ps3> = splitPbyW(g.prods, inlabel(x));
	if ([production(str l, str x, BGFExpression e)] := ps2)
		return <r,grammar(g.roots, ps1 + production("", x, e) + ps3)>;
	else
		return <add(r,problemStr("Label not found or not unique",x)),g>; // the latter should never happen
}

// selectors for subexpressions
XBGFResult runAnonymize(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	p2 = unmark(p1);
	p3 = demarkS(p1);
	if (!inProds(p2,g.prods))
		r = notFoundP(r,p1);
	return <r,grammar(g.roots, replaceP(g.prods,p2,p3))>;
}

XBGFResult runDeanonymize(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	p2 = unmark(p1);
	p3 = demarkS(p1);
	if (!inProds(p3,g.prods))
		r = notFoundP(r,p1);
	return <r,grammar(g.roots, replaceP(g.prods,p3,p2))>;
}

XBGFResult runRenameS(str x, str y, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	<ps1,ps2,ps3> = splitPbyW(g.prods, w);
	if (/selectable(x,_) !:= ps2)
		r = freshName("Source name",r,x);
	if (/selectable(y,_) := ps2)
		r = notFreshName("Target name",r,y);
	ps4 = visit(ps2){case selectable(x,BGFExpression e) => selectable(y,e)}
	return <r,grammar(g.roots, ps1 + ps4 + ps3)>;
}
