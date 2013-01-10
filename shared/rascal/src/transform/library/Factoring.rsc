@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::library::Factoring

import syntax::BGF;
import syntax::XBGF;
import normal::BGF;
import diff::GDT;
import transform::Results;
import transform::library::Util;
import transform::library::Brutal;

XBGFResult runFactor(BGFExpression e1, BGFExpression e2, XBGFScope w, g)
{
	XBGFOutcome r = ok();
	e3 = normalise(transform::library::Factoring::makeDistributed(e1));
	e4 = normalise(transform::library::Factoring::makeDistributed(e2));
	if (!eqE(e3, e4))
		r = problemExpr2("Expressions must be related by distribution.",e1,e2);
	return add(r,transform::library::Brutal::runReplace(e1,e2,w,g));
}

BGFProduction makeDistributed(production(str l, str x, BGFExpression e)) = production(l, x, makeDistributed(e));

BGFExpression makeDistributed(BGFExpression e1)
{
	if (choice(L1) := e1) // excessive normalisation
	{
		list[BGFExpression] Ln = [];
		for (e2 <- L1)
		{
			e3 = makeDistributed(e2);
			if (choice(L2) := e3)
				Ln += L2;
			else
				Ln += e2; // TODO or e3?
		}
		return choice(Ln);
	}
	elseif (sequence(L1) := e1)
	{
		list[list[BGFExpression]] Ln = [[]];
		for (e2 <- L1)
		{
			e3 = makeDistributed(e2);
			if (choice(L2) := e3)
				{
					Lm = [];
					for (e4 <- L2)
						Lm += [Li + e4 | Li <- Ln];
					Ln = Lm;
				}
			else
				Ln = [Li + e3 | Li <- Ln]; // TODO or e2?
		}
		return choice([sequence(Li) | Li <- Ln]);
	}
	else
		return e1;
}
