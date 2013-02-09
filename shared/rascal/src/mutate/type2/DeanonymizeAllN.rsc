@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{DeanonymizeAllN}
module mutate::type2::DeanonymizeAllN

import syntax::BGF;
import normal::BGF;

BGFGrammar DeanonymizeAllN(BGFGrammar g) = grammar(g.roots,normalise([production(p.label,p.lhs,rewriteExpr(p.rhs)) | p<-g.prods]));

BGFExpression rewriteExpr(BGFExpression e)
{
	// TODO test thoroughly: if does not work, write your own traversal explicitly
	return top-down visit(e)
	{
		case sequence([*L1,nonterminal(n),*L2]) => sequence([*L1,selectable(n,nonterminal(n)),*L2])
		case choice([*L1,nonterminal(n),*L2]) => choice([rewriteExpr(choice(L1)),selectable(n,nonterminal(n)),rewriteExpr(choice(L2))])
		// ???
	}
}
