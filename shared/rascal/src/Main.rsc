@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module Main

import IO;
import syntax::BGF;
import syntax::XBGF;
import io::ReadXBGF;
import io::WriteXBGF;
//import lang::xml::DOM;

public void main()
{
	XBGFSequence x;
	x = readXBGF(|project://slps/tests/long.xbgf|);
	iprintln(x);
	writeXBGF(x,|project://slps/res/long.xbgf|);
}

public bool tryAll()
{
	loc base = |project://slps/tests|;
	loc outp = |project://slps/res|;
	for (f <- listEntries(base))
	{
		if (f == ".gitignore") continue;
		println(f);
		writeXBGF(readXBGF(base+f),outp+f);
	}
	return true;
}

