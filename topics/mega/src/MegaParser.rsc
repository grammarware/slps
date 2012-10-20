@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module MegaParser

import structure::MegaGrammar;
import structure::MegaADT;
import io::MegaL;
import Ambiguity;
import ParseTree;
import String;
import IO;

public void main(list[str] args)
{
	for(loc base <- [|cwd:///../lib/ox|,|cwd:///../lib/slps|], f <- listEntries(base), endsWith(f,".megal"))
	{
		println("Getting a parse tree of <f>...");
		readPT(base+f);
		println("Getting an abstract syntax tree of <f>...");
		AMegaModel ast = readAST(base+f);
		println("Serialising an abstract syntax tree of <f>...");
		writeAST(base+(f+".back"),ast);
		println("Generating a Graphviz digraph for <f>...");
		writeDot(base+(f+".dot"),ast);
	}
}