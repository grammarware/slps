@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@doc{This simple program reads is the whole test suite for XBGF from the corresponding SLPS directory
and generates a Rascal file that contains identically behaving test functions which can be invoked with
:test
command.}
module Sync

import String;
import IO;
import io::ReadBGF;
import io::ReadXBGF;

public void main()
{
	loc base = |home:///projects/slps/topics/transformation/xbgf/tests|;
	str buffer = "@contributor{Super Awesome Automated XBGF Test Suite Synchroniser}
		'@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
		'module transform::XBGFTest
		'
		'import IO;
		'import syntax::BGF;
		'import syntax::XBGF;
		'import transform::XBGF;
		'import diff::GDT;
		'
		'map[str,tuple[XBGFSequence,BGFGrammar,BGFGrammar]] test_data = (
		'",
		buffer2 = "",
		buffer3 = "";
	for (f <- listEntries(base), endsWith(f,".xbgf"))
	{
		xbgf = readXBGF(base+f);
		bgf = readBGF(base+replaceLast(f,".xbgf",".bgf"));
		bl = readBGF(base+replaceLast(f,".xbgf",".baseline"));
		//buffer += "test bool test_<replaceLast(f,".xbgf","")>() { return gdt(transform(<xbgf>,<bgf>),<bl>); }\n";
		buffer += "\"<replaceLast(f,".xbgf","")>\": \<<xbgf>,<bgf>,<bl>\>,\n";
		buffer2 += "test bool test_<replaceLast(f,".xbgf","")>() { \<xbgf,bgf1,bgf2\> = test_data[\"<replaceLast(f,".xbgf","")>\"]; return gdt(transform(xbgf,bgf1),bgf2); }\n";
		buffer3 += "void show_<replaceLast(f,".xbgf","")>() { \<xbgf,bgf1,bgf2\> = test_data[\"<replaceLast(f,".xbgf","")>\"]; println(\"Input \<bgf1\>\");println(\"Transformations: \<xbgf\>\");println(\"Expected output \<bgf2\>\");println(\"Actual output \<transform(xbgf,bgf1)\>\"); }\n";
	}
	writeFile(|project://slps/src/transform/XBGFTest.rsc|, replaceLast(buffer,",","")+");\n\n"+buffer3+"\n\n"+buffer2);
}

