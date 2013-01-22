@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{Skeletonise}
module mutate::Skeletonise

import lib::Rascalware;
import syntax::BGF;
import mutate::SubGrammar;

// leaves only lexical conjunctive clauses in the grammar
public BGFGrammar skeletonise(BGFGrammar g)
{
	nps = visit(g.prods)
	{
		case production(str label, str lhs, allof([selectable("lex", e1),e2]))
			=> production(label,lhs,selectable(lhs,e1))
	};
	return subgrammar(grammar(g.roots,nps));
}
