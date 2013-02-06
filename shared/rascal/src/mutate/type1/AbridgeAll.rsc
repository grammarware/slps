@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{AbridgeAll}
module mutate::type1::AbridgeAll

import syntax::BGF;

BGFGrammar AbridgeAll(BGFGrammar g) = grammar(g.roots, g.prods - [p | p:production(_,x,nonterminal(x)) <- g.prods]);