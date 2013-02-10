@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{narrow,widen}
module transform::library::Width

import language::BGF;
import language::XScope;
import language::XOutcome;
import transform::library::Brutal;

// narrow-equivalence (the reverse, widen-equivalence, is hereby also implicitly defined)
bool narrowing(anything(),_) = true;
bool narrowing(star(e),plus(e)) = true;
bool narrowing(star(e),optional(e)) = true;
bool narrowing(star(e),e) = true;
bool narrowing(plus(e),e) = true;
bool narrowing(optional(e),e) = true;
default bool narrowing(_,_) = false;

XBGFResult runNarrow(BGFExpression e1, BGFExpression e2, XBGFScope w, BGFGrammar g)
{
	if (!narrowing(e1,e2))
		return <problemExpr2("Expressions are not in narrowing relation.",e1,e2),g>;
	return transform::library::Brutal::runReplace(e1,e2,w,g);
}

XBGFResult runWiden(BGFExpression e1, BGFExpression e2, XBGFScope w, BGFGrammar g)
{
	if (!narrowing(e2,e1))
		return <problemExpr2("Expressions are not in widening relation",e2,e1),g>;
	return transform::library::Brutal::runReplace(e1,e2,w,g); 
}
