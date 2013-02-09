@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{RetireLs}
module mutate::type1::RetireLs

import language::BGF;

BGFGrammar RetireLs(BGFGrammar g) = grammar(g.roots, [production("", p.lhs, p.rhs) | p <- g.prods]);
