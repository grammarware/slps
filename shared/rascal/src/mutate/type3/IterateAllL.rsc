@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{IterateAllL}
module mutate::type3::IterateAllL

import language::BGF;

BGFGrammar IterateAllL(BGFGrammar g)
{
	return visit(g)
	{
		case production(str label, str lhs, sequence([nonterminal(str n),nonterminal(x),nonterminal(n)]))
			=> production(label, lhs, sequence([star(sequence([nonterminal(n),nonterminal(x)])),nonterminal(n)]))
	}
}
