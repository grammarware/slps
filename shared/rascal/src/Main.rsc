@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module Main

import String;
import IO;
import syntax::BGF;
import syntax::XBGF;
import diff::GDT;
import transform::library::Test;
import transform::NegotiatedXBGF;

public void main()
{
	<_,g1,g2> = transform::library::Test::test_data["renameN"];
	XBGFSequence x = [renameN("expression","exp")];
	//XBGFSequence x = [renameN("expr","int")];
	
	<p,a,g3> = transform::NegotiatedXBGF::attemptTransform(x,g1);
	iprintln(x);
	println(g1);
	println(g3);
	println(g2);
	println(diff::GDT::gdtv(g3,g2));
	println(p);
	println(a);
}
