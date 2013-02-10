@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{IterateAllP}
module mutate::type3::IterateAllP

import language::BGF;

BGFGrammar IterateAllP(BGFGrammar g)
{
	return visit(g)
	{
		case production(str label, str lhs, sequence([nonterminal(n),nonterminal(n)]))
			=> production(label, lhs, plus(nonterminal(str n)))
	}
}
