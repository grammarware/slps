@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module FL

import Concrete;
import Abstract;
import IO;
import ParseTree;

public void main(list[str] args)
{
	loc src = |cwd:///|+args[0];
	do(src);
}

public void main() = do(|home:///projects/slps/topics/fl/shared/factorial.txt|);

void do(loc src)
{
	println("The Factorial Language: Rascal implementation.");
	c = parse(#Program,src);
	println("Parsing: done.");
	a = implode(#FLPrg,c);
	println("Imploding: done.");
}
