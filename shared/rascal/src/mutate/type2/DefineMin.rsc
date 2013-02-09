@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{DefineMin}
module mutate::type2::DefineMin

import syntax::BGF;
import analyse::Metrics;

BGFGrammar DefineMin(BGFGrammar g) = grammar(g.roots,g.prods + [production("",n,empty()) | n<-bottomNs()]);
