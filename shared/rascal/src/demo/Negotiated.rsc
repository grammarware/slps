@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module demo::Negotiated

import String;
import IO;
import syntax::BGF;
import syntax::XBGF;
import diff::GDT;
import transform::library::Test;
import transform::NegotiatedXBGF;
import transform::XBGF;
import export::BNF;

public void main()
{
	runRenameExample();
}

public void runRenameExample()
{
	str sep = "--------------------\n";
	BGFGrammar g1 = grammar([],[
		production("one","foo",val(integer())),
		production("two","expr",sequence([nonterminal("foo"),terminal("+"),nonterminal("foo")]))
	]);
	println("<sep><pp(g1)>\n<sep>");
	XBGFSequence x = [renameN("expression","foo")];
	println("<sep><pp(transformAnyway(x,g1))>\n<sep>");
}
