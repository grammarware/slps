@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::Results

import syntax::BGF;
import syntax::XBGF;

data XBGFOutcome
	= ok()
	| problem(str msg)
	| problemXBGF(str msg, XBGFCommand xbgf)
	| problemProd(str msg, BGFProduction p)
	| problemProd2(str msg, BGFProduction p1, BGFProduction p2)
	| problemPinProds(str msg, BGFProduction p, list[BGFProduction] ps)
	| problemProds(str msg, list[BGFProduction] ps)
	| problemProds2(str msg, list[BGFProduction] ps1, list[BGFProduction] ps2)
	| problemExpr(str msg, BGFExpression e)
	| problemExpr2(str msg, BGFExpression e1, BGFExpression e2)
	| problemStr(str msg, str x)
	| problemStr2(str msg, str x, str y)
	| problemStrs(str msg, list[str] xs)
	| problemScope(str msg, XBGFScope w)
	| manyProblems(list[XBGFOutcome] lst)
	;

alias XBGFResult = tuple[XBGFOutcome r,BGFGrammar g];

// rules for addition
public XBGFOutcome add(ok(), XBGFOutcome y) = y;
public XBGFOutcome add(XBGFOutcome x, ok()) = x;
public XBGFOutcome add(manyProblems(list[XBGFOutcome] xs),manyProblems(list[XBGFOutcome] ys)) = manyProblems(xs+ys);
public XBGFOutcome add(XBGFOutcome x,manyProblems(list[XBGFOutcome] ys)) = manyProblems([x]+ys);
public XBGFOutcome add(manyProblems(list[XBGFOutcome] xs),XBGFOutcome y) = manyProblems(xs+y);
public default XBGFOutcome add(XBGFOutcome x, XBGFOutcome y) = manyProblems([x,y]);

// adding two results will give both error sets and the last grammar
public XBGFResult add(XBGFResult x, XBGFResult y) = <add(x.r,y.r),y.g>;

public XBGFResult add(XBGFOutcome x, XBGFResult y) = <add(x,y.r),y.g>;

public void thw(ok()) {}
public void thw(problem(str msg)) {throw msg;}
public void thw(problemXBGF(str msg, XBGFCommand xbgf)) {throw msg+":\n<xbgf>";}
public void thw(problemProd(str msg, BGFProduction p)) {throw msg+":\n<p>";}
public void thw(problemProd2(str msg, BGFProduction p1, BGFProduction p2)) {throw msg+":\n\t<p1>\nand\n\t<p2>";}
public void thw(problemPinProds(str msg, BGFProduction p, list[BGFProduction] ps)) {throw msg+"\n\t<p>\nin\n\t<ps>";}
public void thw(problemProds(str msg, list[BGFProduction] ps)) {throw msg+":\n<ps>";}
public void thw(problemProds2(str msg, list[BGFProduction] ps1, list[BGFProduction] ps2)) {throw msg+":\n<ps1>\nvs\n<ps2>";}
public void thw(problemStr(str msg, str x)) {throw msg+": <x>";}
public void thw(problemStr2(str msg, str x, str y)) {throw msg+": <x> and <y>";}
public void thw(problemStrs(str msg, list[str] xs)) {throw msg+":\n\t<xs>";}
public void thw(problemExpr(str msg, BGFExpression e)) {throw msg+":\n\t<e>";}
public void thw(problemExpr2(str msg, BGFExpression e1, BGFExpression e2)) {throw msg+":\n<e1> and <e2>";}
public void thw(problemScope(str msg, XBGFScope w)) {throw "<msg> in <w>";}
public void thw(manyProblems(list[XBGFOutcome] lst)) {for (x <- lst) thw(x);} // will not work
public default void thw(XBGFOutcome x) {}
