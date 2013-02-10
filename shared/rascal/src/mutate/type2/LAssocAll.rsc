@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{LAssocAll}
module mutate::type2::LAssocAll

import language::BGF;

BGFGrammar LAssocAll(BGFGrammar g) = AssocAll(g);

// RAssocAll and LAssocAll are indistinguishable on the grammar level
BGFGrammar AssocAll(BGFGrammar g)
{
	return visit(g)
	{
		case production(str label, str lhs, sequence([nonterminal(str n),star(sequence([nonterminal(str x),nonterminal(n)]))]))
			=> production(label, lhs, sequence([nonterminal(n),nonterminal(x),nonterminal(n)]));
		case production(str label, str lhs, sequence([star(sequence([nonterminal(n),nonterminal(x)])),nonterminal(n)]))
			=> production(label, lhs, sequence([nonterminal(n),nonterminal(x),nonterminal(n)]));
		case production(str label, str lhs, plus(nonterminal(str n)))
			=> production(label, lhs, sequence([nonterminal(n),nonterminal(n)]));
	}
}
