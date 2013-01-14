@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module demo::Negotiated

import lib::Rascalware;
import String;
//import IO;
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
	str sep = "--------------------";
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
	print("<pad("Original")><pp(g1)>\n<pad("Transformation12")><ppxs(x)>\n<pad("Application")>");
	g2 = transformAnyway(x,g1);
	print("<pad("Intended esult")><pp(g2)>\n<pad("")>");
	
	print("<pad("TransformationE")><ppxs(e)>\n<pad("Application")>");
	g3 = transformAnyway(e,g1);
	print("<pad("Evolved original")><pp(g3)>\n<pad("Transformation12")><ppxs(x)>\n<pad("Application")>");
	g4 = transformAnyway(x,g3);
	print("<pad("Final result")><pp(g4)>\n<pad("")>");
	//println("<sep><pp(transformAnyway(x,g1))>\n<sep>");
}

str pad("") = center("",50,"-")+"\n";
default str pad(str x) = center(" <x>: ",50,"-")+"\n";

str ppxs(XBGFSequence xs) = mapjoin(ppx,xs,"\n");
str ppx(renameN(x,y)) = "renameN(<x>,<y>);";
str ppx(extract(p,globally())) = "extract(<pp(p)>);";
default str ppx(XBGFCommand s) = "??<s>??";
