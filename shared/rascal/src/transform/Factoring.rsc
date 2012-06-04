@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::Factoring

import syntax::BGF;

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
