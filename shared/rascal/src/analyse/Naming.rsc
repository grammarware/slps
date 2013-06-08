@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Naming

import language::BGF;
import ParseTree;

lexical CamelCase = CapitalWord+ ;
lexical LowerCase = [a-z]+ !>> [a-zA-Z] ;
lexical UpperCase = [A-Z]+ !>> [a-zA-Z] ;
lexical MixedCase = LowerWord CapitalWord* ;
lexical LowerWord = [a-z]+ !>> [a-z];
lexical CapitalWord = [A-Z] [a-z]* !>> [a-z];
lexical Word = [a-zA-Z]+ !>> [a-zA-Z];
lexical MultipleWords
	= {Word [\ ]}+
	| {Word [\-]}+
	| {Word [_]}+
	;

bool recognise(&T nt, str s)
{
	try
	{
		parse(nt,s);
		return true;
	}
	catch: return false;
}

public set[str] camelcases(SGrammar g) = {n | str n <- g.prods, recognise(#CamelCase,n)};
public set[str] mixedcases(SGrammar g) = {n | str n <- g.prods, recognise(#MixedCase,n)};
public set[str] lowercases(SGrammar g) = {n | str n <- g.prods, recognise(#LowerCase,n)};
public set[str] uppercases(SGrammar g) = {n | str n <- g.prods, recognise(#UpperCase,n)};
public set[str] multiwords(SGrammar g) = {n | str n <- g.prods, recognise(#MultipleWords,n)};
