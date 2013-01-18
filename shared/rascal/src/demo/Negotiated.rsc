@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{negotiated transformation}
module demo::Negotiated

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::NegotiatedXBGF;
import transform::XBGF;
import export::XBNF;
import export::BNF;

BGFGrammar g1 = grammar([],[
	production("one","num",val(integer())),
	production("two","expression",sequence([nonterminal("num"),terminal("+"),nonterminal("num")])),
	production("six","assign",sequence([nonterminal("expression"),terminal("="),nonterminal("expression"),terminal(".")]))
]);
XBGFSequence x = [
	renameN("expression","e"),
	extract(production("ten","rhs",sequence([nonterminal("e"),terminal(".")])),globally())
];
XBGFSequence e = [
	renameN("expression","expr"),
	renameN("num","e")
];

public void main()
{
	runRenameExample1(g1,x);
}

public void runRenameExample1()
{
	print("<pad("Original")><pp(g1)><pad("Transformation12")><ppxs(x)>\n<pad("Application")>");
	g2 = vtransform(x,g1);
	print("<pad("Result")><pp(g2)><pad("")>");
}

public void runRenameExample2()
{
	print("<pad("Original")><pp(g1)><pad("TransformationE")><ppxs(e)>\n<pad("Application")>");
	g2 = vtransform(e,g1);
	print("<pad("Evolved original")><pp(g2)>");
	print("<pad("Transformation12")><ppxs(x)>\n<pad("Attempted application")>");
	g3 = vtransform(x,g2);
}

public void runRenameExample3()
{
	print("<pad("Original")><pp(g1)><pad("TransformationE")><ppxs(e)>\n<pad("Application")>");
	g2 = vtransform(e,g1);
	print("<pad("Evolved original")><pp(g2)>");
	print("<pad("Transformation12")><ppxs(x)>\n<pad("Negotiated application")>");
	g3 = transformAnyway(x,g2);
	print("<pad("Final result")><pp(g3)><pad("")>");
}
