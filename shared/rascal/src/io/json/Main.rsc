@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module io::json::Main

import io::json::Syntax;
import io::json::Parse;
import IO;
import ParseTree;

public void main(list[str] args)
{
	loc src = |cwd:///|+args[0];
	println(io::json::Parse::str2jso(readFile(src)));
	// PT = parse(#BasicType,src);
	println("Parsing completed.");
}

public void go()
{
	loc src = |project://slps/src/last1000.json|;
	println(io::json::Parse::str2jso(readFile(src)));
	println("Parsing completed.");
}