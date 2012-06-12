@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module Concrete

import IO;
import ParseTree;

syntax Program = Function+ LO;
syntax Function = Name Name+ "=" Expr Newline ;
syntax Expr =
	binary: Expr Ops Expr
	| apply: Name Expr+
	| ifThenElse: "if" Expr "then" Expr "else" Expr
	| "(" Expr ")"
	| argument: Name
	| literal: Int ;
syntax Ops =
	minus: "-"
	| plus: "+"
	| equal: "==" ;
lexical Name = [a-z]+ !>> [a-z] ;
lexical Int = [0] | [1-9][0-9]* !>> [0-9] ;
lexical Newline = "\n" ;
layout LO = [\ \t]* ;

public void main(list[str] args)
{
	loc src = |cwd:///|+args[0];
	do(src);
}

public void main() = do(|home:///projects/slps/topics/fl/shared/factorial.txt|);

void do(loc src)
{
	parse(#Program,src);
	println("Done.");
}
