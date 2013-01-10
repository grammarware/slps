@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::library::Terminals

import syntax::BGF;
import syntax::XBGF;
import normal::BGF;
import transform::Results;
import transform::library::Util;
import transform::library::Brutal;
import transform::library::Sequential;
import diff::GDT;

XBGFResult runAbstractize(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	p2 = unmark(p1);
	if (!inProds(p2,g.prods))
		r = notFoundP(r,p2);
	for (/marked(e) := p1)
		if (terminal(_) !:= e)
			r = add(r, problem("Abstractize only works with marked terminals, use project instead."));
	return add(r,transform::library::Sequential::runProject(p1,grammar(g.roots, g.prods)));
}

XBGFResult runConcatT(list[str] xs, str y, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	// TODO
	return <r,g>;
}

XBGFResult runConcretize(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	p2 = demark(p1);
	if (!inProds(p2,g.prods))
		r = notFoundP(r,p2);
	for (/marked(e) := p1)
		if (terminal(_) !:= e)
			r = add(r,problem("Concretize only works with marked terminals, use inject instead."));
	return add(r,transform::library::Sequential::runInject(p1,g));
}

XBGFResult runRenameT(str x, str y, BGFGrammar g)
{
	XBGFOutcome r = ok();
	ts = allTs(g.prods);
	if (x notin ts)
		r = freshName("Source name",r,x);
	if (y in ts)
	r = notFreshName("Target name",r,y);
	return add(r,transform::library::Brutal::runReplace(terminal(x),terminal(y),globally(),g));
}

XBGFResult runSplitT(str x, list[str] ys, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	<ps1,ps2,ps3> = splitPbyW(g.prods, w);
	BGFGrammar g2 	= transform::library::Brutal::runReplace(terminal(x),sequence([terminal(y) | y <- ys]),grammar([],ps2));
	XBGFResult repl = transform::library::Brutal::runReplace(terminal(x),sequence([terminal(y) | y <- ys]),grammar([],ps2));
	r = add(r,repl.r);
	if (grammar(_, ps4) := repl.g)
		return <r,grammar(g.roots,ps1 + normalise(ps2) + ps3)>;
}
