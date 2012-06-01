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
	int cx = 1;
	str buffer = "";
	for (f <- listEntries(base), endsWith(f,".xbgf"))
	{
		xbgf = readXBGF(base+f);
		bgf = readBGF(base+replaceLast(f,".xbgf",".bgf"));
		bl = readBGF(base+replaceLast(f,".xbgf",".baseline"));
		buffer += "test bool x<cx>() { return transform(<xbgf>,<bgf>)==<bl>; }\n";
		cx+=1;
	}
	writeFile(|project://slps/src/transform/XBGFTest.rsc|,
		"@contributor{Super Awesome Automated XBGF Test Suite Synchroniser}
		'@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
		'module transform::XBGFTest
		'
		'import syntax::BGF;
		'import syntax::XBGF;
		'import transform::XBGF;
		'
		'"+replaceAll(buffer,"import","\\import"));
}

