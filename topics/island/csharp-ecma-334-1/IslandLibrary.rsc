@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module IslandLibrary

import IO;

lexical Something = ![]*;

syntax NotSemicolon = notsem: AnythingButSemicolon Something;
lexical AnythingButSemicolon = ![;]* !>> ![;];

syntax NotRightParenthesis = notpar: AnythingButParenthesis Something;
lexical AnythingButParenthesis = ![(]* !>> ![(];

syntax NotRightSquareBracket = notsb: AnythingButSquareBracket Something;
lexical AnythingButSquareBracket = ![\]]* !>> ![\]];

syntax NotLeftCurly = notsb: AnythingButLeftCurly Something;
lexical AnythingButLeftCurly = ![{]* !>> ![{];

syntax NotWhitespace = notws: AnythingButWhitespace " " Something;
lexical AnythingButWhitespace = ![\ \t]* !>> ![\ \t]; 

syntax BalancedCurlies = balcur: BCElement* RightCurly Something;
//NotLeftCurly CurlyBlock NotRightCurly
lexical BCElement = CurlyBlock | NotCurly;
lexical NotCurly = ![{}];
lexical NotRightCurly = ![}]* !>> ![}];
lexical RightCurly = [}];
lexical CurlyBlock = "{" BCElement* "}";

//keyword Kwd = "function";
//start syntax Fun = function: FunKwd Name "(" Args ")" Body;
//lexical FunKwd = "function";
//lexical Name = [a-zA-Z]+ !>> [a-zA-Z];
//syntax Args = {Name ","}*;
//syntax Body = "{" Name+ "}";
layout L = [\ ]*;

public NotRightParenthesis p(str s)
{
	return parse(#NotRightParenthesis,s);
}

data NWS = notws(str,str);
data NRP = notpar(str,str);
data NSC = notsem(str,str);
data BCs = balcur(str,list[str],str,str);

public BCs getAST(str s)
{
	return implode(#BCs,parse(#BalancedCurlies,s));
	//return implode(#NRP,parse(#NotRightParenthesis,s));
}

public void main()
{
	iprintln(getAST("abc"));
}
