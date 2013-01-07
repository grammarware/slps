@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::Results

import syntax::BGF;
//import syntax::XBGF;

data XBGFOutcome
	= ok()
	| problemProd(str msg, BGFProduction p)
	| problemExpr(str msg, BGFExpression e)
	| problemExpr2(str msg, BGFExpression e1, BGFExpression e2)
	| problemNT(str msg, str x)
	| problemNT2(str msg, str x, str y)
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
public void thw(problemProd(str msg, BGFProduction p)) {throw msg+":\n<p>";}
public void thw(problemNT(str msg, str x)) {throw msg+": <x>";}
public void thw(problemNT2(str msg, str x, str y)) {throw msg+": <x> and <y>";}
public void thw(problemExpr(str msg, BGFExpression e)) {throw msg+":\n<e>";}
public void thw(problemExpr2(str msg, BGFExpression e1, BGFExpression e2)) {throw msg+":\n<e1> and <e2>";}
public void thw(manyProblems(list[XBGFOutcome] lst)) {for (x <- lst) thw(x);} // will not work
public default void thw(XBGFOutcome x) {}
