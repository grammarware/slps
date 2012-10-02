@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module Plugin

import MegaGrammar;

import util::IDE;
import ParseTree;
import IO;

public void main()
{
    registerLanguage("MegaL", "megal", MegaModel(str input, loc org) {return parse(#MegaModel, input, org);});
	println("MegaL is registered");
}

public void t()
{
	parse(#MegaModel,|project://megal/tests/annotation.megal|);
}