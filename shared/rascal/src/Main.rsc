@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module Main

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
	//<_,g1,g2> = transform::library::Test::test_data["renameN"];
	BGFGrammar g1 = grammar([],[
		production("one","foo",val(integer())),
		production("two","expr",nonterminal("foo"))
	]);
	XBGFSequence x = [renameN("expression","foo")];
	//XBGFSequence x = [renameN("expr","int")];
	
	//iprintln();
	println("------------\n<pp(transformAnyway(x,g1))>");
	//
	//<p,a,g3> = transform::NegotiatedXBGF::attemptTransform(x,g1);
	//iprintln(x);
	//println(g1);
	//println(g3);
	//println(g2);
	//println(diff::GDT::gdtv(g3,g2));
	//println(p);
	//println(a);
}
