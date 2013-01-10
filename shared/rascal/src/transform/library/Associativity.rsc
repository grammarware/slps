@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::library::Associativity

import syntax::BGF;
import syntax::XBGF;
import transform::Results;
import transform::library::Util;

bool admit(sequence([nonterminal(n),nonterminal(x),nonterminal(n)]),
		   sequence([nonterminal(n),star(sequence([nonterminal(x),nonterminal(n)]))])) = true;
bool admit(sequence([nonterminal(n),nonterminal(x),nonterminal(n)]),
		   sequence([star(sequence([nonterminal(n),nonterminal(x)])),nonterminal(n)])) = true;
bool admit(sequence([nonterminal(n),nonterminal(n)]),
		   plus(nonterminal(n))) = true;
default bool admit(BGFExpression e1, BGFExpression e2) = false;

// NB: rassoc and lassoc are the same when they work on the grammar level
// the differences can only be observed on the instance level
XBGFResult runRAssoc(BGFProduction p, BGFGrammar g) = runAssoc(p,g);
XBGFResult runLAssoc(BGFProduction p, BGFGrammar g) = runAssoc(p,g);

XBGFResult runAssoc(production(str l, str x, BGFExpression e1), BGFGrammar g)
{
	XBGFOutcome r = ok();
	<ps1,ps2,ps3> = splitPbyW(g.prods,comboscope(inlabel(l),innt(x)));
	if ([production(l, x, BGFExpression e2)] := ps2)
		if (admit(e1,e2))
			return <r,grammar(g.roots,ps1 + production(l, x, e1) + ps3)>;
		else
			return <problemProd("Production rule must admit associativity transformation",production(l,x,e1)),g>;
	else
		return <problemPinProds("Cannot find the right production rule to match",production(l,x,e1),ps2),g>;
}

XBGFResult runIterate(production(str l, str x, BGFExpression e1), BGFGrammar g)
{
	XBGFOutcome r = ok();
	<ps1,ps2,ps3> = splitPbyW(g.prods,comboscope(inlabel(l),innt(x)));
	if ([production(l, x, BGFExpression e2)] := ps2)
		if (admit(e2,e1))
			return <r,grammar(g.roots,ps1 + production(l, x, e1) + ps3)>;
		else
			return <problemProd("Production rule must admit associativity transformation",production(l,x,e1)),g>;
	else
		return <problemPinProds("Cannot find the right production rule to match",production(l,x,e1),ps2),g>;
}

