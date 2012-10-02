@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module MegaParser

import MegaGrammar;
import Ambiguity;
import ParseTree;
import String;
import IO;

// public void main()
// {
//     registerLanguage("MegaL", "megal", MegaModel(str input, loc org) {return parse(#MegaModel, input, org);});
// 	println("MegaL is registered");
// }

public void main(list[str] args)
{
	loc base = |cwd:///../tests|;
	for (f <- listEntries(base), endsWith(f,".megal"))
	{
		println("Parsing <base+f>...");
		parse(#MegaModel,base+f);
	}
}