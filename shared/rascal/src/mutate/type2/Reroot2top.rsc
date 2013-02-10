@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{Reroot2top}
module mutate::type2::Reroot2top

import analyse::Metrics;
import transform::library::Util;
import language::BGF;

BGFGrammar Reroot2top(BGFGrammar g) = grammar(topNs(g), g.prods);
