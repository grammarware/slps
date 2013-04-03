@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@doc{This simple program reads is the whole test suite for XBGF from the corresponding SLPS directory
and generates a Rascal file that contains identically behaving test functions which can be invoked with
:test
command.}
module metarascal::SyncTest

import String;
import IO;
import io::ReadBGF;
import io::ReadXBGF;

public void main()
{
	loc base = |home:///projects/slps/topics/transformation/xbgf/tests|;
	str buffer = "@contributor{Super Awesome Automated XBGF Test Suite Synchroniser}
		'@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
		'module transform::Test
		'
		'import IO;
		'import language::BGF;
		'import language::XBGF;
		'import language::XScope;
		'import transform::XBGF;
		'import diff::GDT;
		'import export::BNF;
		'import export::XBNF;
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
		buffer += "\"<replaceLast(f,".xbgf","")>\": \<<xbgf>,<bgf>,<bl>\>,\n";
		buffer2 += "test bool test_<replaceLast(f,".xbgf","")>() { \<xbgf,bgf1,bgf2\> = test_data[\"<replaceLast(f,".xbgf","")>\"]; return gdts(transform(xbgf,bgf1),bgf2); }\n";
		buffer3 += "void show_<replaceLast(f,".xbgf","")>() { \<xbgf,bgf1,bgf2\> = test_data[\"<replaceLast(f,".xbgf","")>\"]; println(\"Input grammar: \<pp(bgf1)\>\");println(\"Transformations: \<ppxs(xbgf)\>\");println(\"Expected output grammar: \<pp(bgf2)\>\");bgf3=transform(xbgf,bgf1);println(\"Actual output grammar: \<pp(bgf3)\>\"); gdtv(bgf3,bgf2); }\n";
	}
	writeFile(|project://slps/src/transform/Test.rsc|, "<replaceLast(buffer,",","")>);\n\n<buffer3>\n\n<buffer2>");
}

