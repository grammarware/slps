@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{RAssocAll}
module mutate::type2::RAssocAll

import language::BGF;
import mutate::type2::LAssocAll;

BGFGrammar RAssocAll(BGFGrammar g) = mutate::type2::LAssocAll::AssocAll(g);
