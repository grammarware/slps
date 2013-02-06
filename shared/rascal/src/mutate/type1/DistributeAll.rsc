@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{DistributeAll}
module mutate::type1::DistributeAll

import syntax::BGF;
import transform::library::Factoring;

BGFGrammar DistributeAll(BGFGrammar g) = grammar(g.roots, [transform::library::Factoring::makeDistributed(p) | p <- g.prods]);
