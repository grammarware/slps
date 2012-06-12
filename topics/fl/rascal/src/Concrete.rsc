@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module Concrete

syntax Program = prg: {Function "\n"}+ "\n"*;
syntax Function = fun: Name Name+ "=" Expr ;
syntax Expr
	= binary: Expr Ops Expr
	| apply: Name Expr+
	| ifThenElse: "if" Expr "then" Expr "else" Expr
	| bracket "(" Expr ")"
	| argument: Name
	| literal: Int
	;
syntax Ops
	= minus: "-"
	| plus: "+"
	| equal: "=="
	;

lexical Name = [a-z]+ !>> [a-z] ;
lexical Int = [0] | [1-9][0-9]* !>> [0-9] ;

layout LO = [\ \t]* !>> [\ \t];
keyword FLKwd = "if" | "then" | "else" ;
