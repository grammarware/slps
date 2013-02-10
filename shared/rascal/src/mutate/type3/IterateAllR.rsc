@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{IterateAllL}
module mutate::type3::IterateAllR

import language::BGF;

BGFGrammar IterateAllR(BGFGrammar g)
{
	return visit(g)
	{
		case production(str label, str lhs, sequence([nonterminal(str n),nonterminal(x),nonterminal(n)]))
			=> production(label, lhs, sequence([nonterminal(str n),star(sequence([nonterminal(str x),nonterminal(n)]))]))
	}
}
