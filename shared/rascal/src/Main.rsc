@contributor{Vadim Zaytsev - vadim@grammarware.net - CWI}
module Main

import IO;
import syntax::BGF;
import syntax::XBGF;
import io::ReadXBGF;
//import lang::xml::DOM;

public void main()
{
	println(readXBGF(|project://slps/tests/undefine1.xbgf|));
}

public bool tryAll()
{
	loc base = |project://slps/tests|;
	for (f <- listEntries(base))
	{
		if (f == ".gitignore") continue;
		println(f);
		println(readXBGF(base+f));
	}
	return true;
}

