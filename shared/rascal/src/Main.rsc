@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module Main

import String;
import IO;
import syntax::BGF;
import syntax::XBGF;
import io::ReadBGF;
import io::ReadXBGF;
import io::WriteXBGF;
import transform::XBGF;
//import lang::xml::DOM;

public void main()
{
	XBGFSequence x;
	x = readXBGF(|project://slps/tests/all/abridge.xbgf|);
	println(x);
	g1 = readBGF(|project://slps/tests/all/abridge.bgf|);
	g2 = readBGF(|project://slps/tests/all/abridge.baseline|);
	//writeXBGF(x,|project://slps/tests/all/abridge.bgf|);
	g3 = transform(x,g1);
	println(g3);
	println(g2);
}

public bool tryAll()
{
	loc base = |project://slps/tests/xbgf|;
	loc outp = |project://slps/tests/xbgf-res|;
	for (f <- listEntries(base))
	{
		if (f == ".gitignore") continue;
		println(f);
		writeXBGF(readXBGF(base+f),outp+f);
	}
	return true;
}
