@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{negotiated transformation}
module language::XOutcome

import lib::Rascalware;
import language::BGF;
import language::XBGF;

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
	;

alias XBGFResult = tuple[XBGFOutcome r,BGFGrammar g];
