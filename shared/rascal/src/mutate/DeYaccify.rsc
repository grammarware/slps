@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{deyaccifyAll}
module mutate::DeYaccify

import syntax::BGF;

// NB: the deyaccification mutation works on horizontal productions,
// while the deyaccification transformation works on vertical ones!!!

public BGFGrammar deyaccifyAll(BGFGrammar g)
	= visit(g)
	{
		// N = N X | X   =>   N = X+
		case production(label, lhs, choice([sequence([nonterminal(n),x]),x]))
			=> production(label, lhs, plus(x))
		// N = X | N X   =>   N = X+
		case production(label, lhs, choice([x,sequence([nonterminal(n),x])]))
			=> production(label, lhs, plus(x))
		// N = X N | X   =>   N = X+
		case production(label, lhs, choice([sequence([x,nonterminal(n)]),x]))
			=> production(label, lhs, plus(x))
		// N = X | X N   =>   N = X+
		case production(label, lhs, choice([x,sequence([x,nonterminal(n)])]))
			=> production(label, lhs, plus(x))
		// N = X N | e   =>   N = X*
		case production(label, lhs, choice([sequence([x,nonterminal(n)]),epsilon()]))
			=> production(label, lhs, star(x))
		// N = e | X N   =>   N = X*
		case production(label, lhs, choice([epsilon(),sequence([x,nonterminal(n)])]))
			=> production(label, lhs, star(x))
		// N = N X | e   =>   N = X*
		case production(label, lhs, choice([sequence([nonterminal(n),x]),epsilon()]))
			=> production(label, lhs, star(x))
		// N = e | N X   =>   N = X*
		case production(label, lhs, choice([epsilon(),sequence([nonterminal(n),x])]))
			=> production(label, lhs, star(x))
		// N = N X | Y   =>   N = Y X*
		case production(label, lhs, choice([sequence([nonterminal(n),x]),y]))
			=> production(label, lhs, sequence([y,star(x)]))
		// N = Y | N X   =>   N = Y X*
		case production(label, lhs, choice([y,sequence([nonterminal(n),x])]))
			=> production(label, lhs, sequence([y,star(x)]))
		// N = X N | Y   =>   N = X* Y
		case production(label, lhs, choice([sequence([x,nonterminal(n)]),y]))
			=> production(label, lhs, sequence([star(x),y]))
		// N = Y | X N   =>   N = X* Y
		case production(label, lhs, choice([y,sequence([x,nonterminal(n)])]))
			=> production(label, lhs, sequence([star(x),y]))
	};
