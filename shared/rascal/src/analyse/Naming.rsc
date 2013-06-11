@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Naming

import language::BGF;
import ParseTree;

lexical StrictCamelCase = StrictCapitalWord+ ;
lexical StrictLowerCase = [a-z]+ !>> [a-zA-Z] ;
lexical StrictUpperCase = [A-Z]+ !>> [a-zA-Z] ;
lexical StrictMixedCase = StrictLowerWord StrictCapitalWord* ;

lexical CamelCase = CapitalWord+ ;
lexical LowerCase = [a-z0-9_/\-\ ]+ !>> [a-zA-Z0-9_/\-\ ] ;
lexical UpperCase = [A-Z0-9_/\-\ ]+ !>> [a-zA-Z0-9_/\-\ ] ;
lexical MixedCase = LowerWord CapitalWord* ;

lexical StrictMultipleWords
	= StrictAnyWord StrictCapitalWord+
	| StrictWord Sep {StrictWord Sep}+
	;
lexical MultipleWords
	= AnyWord CapitalWord+
	| Word Sep {Word Sep}+
	;
lexical Sep = [ _ / \ \- ];

lexical StrictLowerWord = [a-z]+ !>> [a-z];
lexical StrictCapitalWord = [A-Z] [a-z]* !>> [a-z];
lexical StrictWord = [a-zA-Z]+ !>> [a-zA-Z];
lexical StrictAnyWord = [a-zA-Z] [a-z]* !>> [a-z];

lexical LowerWord = [a-z_] [a-z0-9_/\-\ ]* !>> [a-z0-9_/\-\ ];
lexical CapitalWord = [A-Z] [a-z0-9_/\-\ ]* !>> [a-z0-9_/\-\ ];
lexical Word = [a-zA-Z] [a-zA-Z0-9]* !>> [a-zA-Z0-9];
lexical AnyWord = [a-zA-Z] [a-z0-9]* !>> [a-z0-9];

bool recognise(&T nt, str s)
{
	try
	{
		parse(nt,s);
		return true;
	}
	catch: return false;
}

public set[str] camelcases1(SGrammar g) = {n | str n <- g.prods, recognise(#CamelCase,n)};
public set[str] mixedcases1(SGrammar g) = {n | str n <- g.prods, recognise(#MixedCase,n)};
public set[str] lowercases1(SGrammar g) = {n | str n <- g.prods, recognise(#LowerCase,n)};
public set[str] uppercases1(SGrammar g) = {n | str n <- g.prods, recognise(#UpperCase,n)};
public set[str] camelcases2(SGrammar g) = {n | str n <- g.prods, recognise(#StrictCamelCase,n)};
public set[str] mixedcases2(SGrammar g) = {n | str n <- g.prods, recognise(#StrictMixedCase,n)};
public set[str] lowercases2(SGrammar g) = {n | str n <- g.prods, recognise(#StrictLowerCase,n)};
public set[str] uppercases2(SGrammar g) = {n | str n <- g.prods, recognise(#StrictUpperCase,n)};

// The next one is a bit more tricky. What we want, is:
//  - more than one word
//  - camelcase, mixedcase or separated
//  - separators can be spaces, dashes, slashes and underscores
public set[str] multiwords1(SGrammar g) = {n | str n <- g.prods, recognise(#MultipleWords,n)};
public set[str] multiwords2(SGrammar g) = {n | str n <- g.prods, recognise(#StrictMultipleWords,n)};

public map[str name,set[str](SGrammar) fun] NamingPatterns =
	(
		"CamelCase":			camelcases1,			// CamelCase
		"MixedCase":			mixedcases1,			// mixedCase
		"LowerCase":			lowercases1,			// lowercase
		"UpperCase":			uppercases1,			// UPPERCASE
		"MultiWord":			multiwords1,			// Multiple_Words (includes camelcases and mixedcases with two or more words)
		"CamelCaseLax":			camelcases2,			// CamelCase
		"MixedCaseLax":			mixedcases2,			// mixedCase
		"LowerCaseLax":			lowercases2,			// lowercase
		"UpperCaseLax":			uppercases2,			// UPPERCASE
		"MultiWordLax":			multiwords2				// Multiple_Words (includes camelcases and mixedcases with two or more words)
	);
