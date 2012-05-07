@contributor{bgf2src automated exporter - SLPS}
module CS

import IO;
import ValueIO;
import List;
//import IslandLibrary;
//import ParseTree;

layout L = [\ \t\r\n]* !>> [\ \t\r\n]; 

syntax CompilationUnit
        = 
        ("using" NotSemicolon ";")*
        ("[" "assembly" ":" NotRightSquareBracket "]")*
        NamespaceMemberDeclaration*
 ;
 
syntax NamespaceMemberDeclaration
        = 
        ("[" NotRightSquareBracket "]")* Modifier* NamespaceMemberMain
 ;
syntax Modifier
        = "abstract"
        | "sealed"
        | "new"
        | "public"
        | "protected"
        | "internal"
        | "private"
 ;
syntax NamespaceMemberMain
        = "namespace" NotWhitespace NotLeftCurly? "{" BalancedCurlies "}" ";"?
        | "class" NotWhitespace NotLeftCurly? "{" BalancedCurlies? "}" ";"?
        | "struct" NotWhitespace NotLeftCurly? "{" BalancedCurlies? "}" ";"?
        | "interface" NotWhitespace NotLeftCurly? "{" BalancedCurlies? "}" ";"?
        | "enum" NotWhitespace NotLeftCurly? "{" BalancedCurlies "}" ";"?
        | "delegate" NotWhitespace NotWhitespace "(" NotRightParenthesis? ")" ";"
 ;

syntax NotSemicolon = NotSemicolonChunk+ () >> [;];
lexical NotSemicolonChunk = ![;\ \t\r\n]+ >> [;\ \t\r\n];

syntax NotRightSquareBracket = NotRightSquareBracketChunk+ () >> [\]];
lexical NotRightSquareBracketChunk = ![\]\ \t\r\n]+ >> [\]\ \t\r\n];

lexical NotWhitespace = ![\ \t\r\n]+ >> [\ \t\r\n]; 

syntax NotLeftCurlyOpt = NotLeftCurlyChunk* () >> [{];
syntax NotLeftCurly = NotLeftCurlyChunk+ () >> [{];
lexical NotLeftCurlyChunk = ![{\ \t\r\n]+ >> [{\ \t\r\n];

syntax NotRightParenthesisOpt = NotRightParenthesisChunk* () >> [)];
lexical NotRightParenthesisChunk = ![)\ \t\r\n]+ >> [)\ \t\r\n];

syntax TryItHere = "{" BalancedCurliesOpt "}";

syntax BalancedCurliesOpt = BCElement* () >> "}";
syntax BalancedCurlies = BCElement+ () >> "}";
lexical BCElement = CurlyBlock | NotCurly;
lexical NotCurly = ![{}];
//lexical NotCurly = ![{}\ \t\r\n];
//lexical NotRightCurly = ![}]* >> [}];
//lexical RightCurly = [}];
lexical CurlyBlock = "{" BCElement* "}";

public list[set[Tree]] ga2(str s)
{
 return for (/amb(x) := parse(#NamespaceMemberMain,s)) append x;
}

public list[set[Tree]] ga(str s)
{
 return for (/amb(x) := parse(#CompilationUnit,s)) append x;
}

public void wv(str s)
{
	int cx = 1;
	list[set[Tree]] lst = ga(s);
	for (set[Tree] st <- lst)
	{
		//println("==================== AMB ====================");
		str cxs = "<cx>";
		writeFile(|project://island/src/value.<cxs>.txt|,
			"==================== AMB ====================\n");
		for (Tree t <- st)
		{
		appendToFile(|project://island/src/value.<cxs>.txt|,t);
		appendToFile(|project://island/src/value.<cxs>.txt|,
			"\n-------------------- WITH --------------------\n");
		}
		cx +=1;
		//iprintln(e);
		//println("-------------------- WITH --------------------");
	}
}

public void main()
{
	y = for (/amb(x) := parse(#CompilationUnit,|project://island/src/test.cs|)) append x;
	if (size(y)==0)
	{
		println("Victory is ours.");
		return;
	}
	int cx = 1;
	
	for (e <- y)
	{
		println("-----------------------------------------");
		str cxs = "<cx>";
		writeTextValueFile(|project://island/src/val<cxs>|,e);
		cx +=1;
		iprintln(e);
		println("--------");
	}
	
}