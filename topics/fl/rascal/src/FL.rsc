@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module FL

import Concrete;
import Abstract;
import Optimiser;
import Evaluator;
import PPrinter;

import IO;
import String;
import ParseTree;
import Exception;

public void main(list[str] args)
{
	do([|cwd:///|+a | a <- args],false);
}

public void main()
{
	println("The Factorial Language: Rascal implementation.");
	do([
		|home:///projects/slps/topics/fl/shared/factorial.txt|,
		|home:///projects/slps/topics/fl/shared/sample42a.txt|,
		|home:///projects/slps/topics/fl/shared/sample88a.txt|,
		|home:///projects/slps/topics/fl/shared/fac5.txt|
	],true);
}

void do(list[loc] files, bool say)
{
	set[FLFun] store = {};
	for (src <- files)
	{
		if (say) println("Processing <src>...");
		try
		{
			c = parse(#Program,trim(readFile(src)));
			if (say) println("Parsing as a program: done.");
			if (say) print(c);
			a = implode(#FLPrg,c);
			if (say) println("Imploding: done.");
			b = Optimiser::simplify(a);
			if (say) println("Simplification: done.");
			if (say) println(pp(b));
			for (prg(fs) := b, FLFun f <- fs)
				store += f; 
		}
		catch IO(s):
		{
			println("Failed: file not found.");
			return;
		}
		catch ParseError(s):
		{
			try
				proceed(parse(#Expr,trim(readFile(src))),store,say);
			catch ParseError(z):
			{
				println("Failed: parse error.");
				return;
			}
		}
		
	}
}

void proceed(Expr e, set[FLFun] store, bool say)
{
	if (say) println("Parsing as an expression: done.");
	if (say) println(e);
	a = implode(#FLExpr,e);
	if (say) println("Imploding: done.");
	b = Optimiser::simplify(a);
	if (say) println("Simplification: done.");
	if (say) println(pp(b));
	c = Evaluator::eval(b,store);
	if (say) println("Evaluation: done.");
	println(pp(c));
}