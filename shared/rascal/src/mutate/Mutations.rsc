@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{SubGrammar, DeYaccifyAll, UnchainAll, InlineLazy, InlinePlus}
module mutate::Mutations

extend mutate::SubGrammar;
extend mutate::Skeletonise;
extend mutate::DeYaccify;
extend mutate::InlineChains;
extend mutate::InlineLazy;
extend mutate::InlinePlus;

import syntax::BGF;
import normal::BGF;
import IO;

alias MutationList = list[BGFGrammar(BGFGrammar)];

public BGFGrammar mutate(MutationList script, BGFGrammar g)
{
	for (m <- script)
	{
		println("[Mutation] <m>");
		g = normalise(m(g));
	}
	println("[Mutation] Done.");
	return g;
}
