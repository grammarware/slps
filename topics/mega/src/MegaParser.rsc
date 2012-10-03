@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module MegaParser

import structure::MegaGrammar;
import structure::MegaADT;
import io::ReadMegaL;
import Ambiguity;
import ParseTree;
import String;
import IO;

public void main(list[str] args)
{
	loc base = |cwd:///../tests|;
	for (f <- listEntries(base), endsWith(f,".megal"))
	{
		println("Getting a parse tree of <f>...");
		readPT(base+f);
		println("Getting an abstract syntax tree of <f>...");
		readAST(base+f);
	}
}