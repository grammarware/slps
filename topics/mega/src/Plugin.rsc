@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module Plugin

import structure::MegaGrammar;

import util::IDE;
import ParseTree;
import IO;

public void main()
{
    registerLanguage("MegaL", "megal", MegaModel(str input, loc org) {return parse(#MegaModel, input, org);});
	println("MegaL is registered");
}

public void tryit()
{
	println(parse(#MegaModel,|home:///projects/slps/topics/mega/lib/ox/annotation.megal|));
}