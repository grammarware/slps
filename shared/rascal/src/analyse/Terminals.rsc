@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Terminals

import lib::Rascalware;
import language::BGF;
import Map;

public set[str] preterminals(SGrammar g) = {n | str n <- domain(g.prods), !isEmpty(g.prods[n]), allofterminals(g.prods[n])};

// lower level classifiers
public bool allterminals(BGFExprList xs) = ( true | it && terminal(_) := e | e <- xs );

public bool allofterminals(BGFProdSet ps)  = ( true | it && allofterminals(p.rhs) | p <- ps );
public bool allofterminals(BGFExprList xs) = ( true | it && allofterminals(e) | e <- xs );

public bool allofterminals(terminal(_)) = true;
public bool allofterminals(sequence(L)) = allofterminals(L);
public bool allofterminals(choice(L)) = allofterminals(L);
public bool allofterminals(allof(L)) = allofterminals(L); // hardly necessary
public bool allofterminals(optional(e)) = allofterminals(e);
public bool allofterminals(plus(e)) = allofterminals(e);
public bool allofterminals(star(e)) = allofterminals(e);
public bool allofterminals(seplistplus(e,s)) = allofterminals(e) && allofterminals(s);
public bool allofterminals(sepliststar(e,s)) = allofterminals(e) && allofterminals(s);
public default bool allofterminals(BGFExpression e) = false;

// the following micropatterns do not tolerate folding
public set[str] names0(SGrammar g) = {n | str n <- domain(g.prods),
	{production(_,n,star(choice(L)))} := g.prods[n],
	!isEmpty(L),
	allterminals(L)}
;
public set[str] names1(SGrammar g) = {n | str n <- domain(g.prods),
	{production(_,n,plus(choice(L)))} := g.prods[n],
	!isEmpty(L),
	alltrivial(L)}
;
public set[str] names2(SGrammar g) = {n | str n <- domain(g.prods),
	{production(_,n,sequence([choice(L1),star(choice(L2))]))} := g.prods[n],
	!isEmpty(L1),
	!isEmpty(L2),
	alltrivial(L1),
	alltrivial(L2)}
;
public set[str] names3(SGrammar g) = {n | str n <- domain(g.prods), {p} := g.prods[n], lookslikeint(p.rhs)};
bool lookslikeint(plus(choice(L2))) = alldigits(L2);
bool lookslikeint(sequence([optional(choice(L3)),plus(choice(L4))])) = allops(L3) && alldigits(L4);
bool lookslikeint(choice([terminal("0"),sequence([choice(L5),star(choice(L6))])])) = alldigits(L5) && alldigits(L6);
bool lookslikeint(choice(L1)) = alldigits(L1); // OOPS: probably not safe for next versions of Rascal
bool lookslikeint(sequence([optional(choice(L7)),choice([terminal("0"),sequence([choice(L8),star(choice(L9))])])])) = allops(L7) && alldigits(L8) && alldigits(L9);
default bool lookslikeint(BGFExpression e) = false;
// underscore for Eiffel’s sake
bool alldigits(BGFExprList es) = ( !isEmpty(es) | it && terminal(x) := e && /^[0-9A-Fa-f\_]$/ := x | e <- es );

// bool isSpecial(str x) = /[\ \@\#\$\&\%\=\\\/\.\,\:\;\*\+\-\_\!\?\(\)\[\]\{\}\<\>\~\'\"\^\|“”‘’]+/ := x; //'
// \‘\’
bool isSpecial(str x) = /[a-zA-Z]/ !:= x;
bool isWord(str x) = /^[\w\.\-\#]+$/ := x;

// Usually operators consist or non-alphanumeric symbols
public set[str] oneop(SGrammar g) = {n | str n <- domain(g.prods),
	{production(_,n,terminal(x))} := g.prods[n],
	isSpecial(x)};
public set[str] manyops(SGrammar g) = {n | str n <- domain(g.prods), aremanyops(g.prods[n])};
bool allops(BGFExprList es) = ( !isEmpty(es) | it && terminal(x) := e && isSpecial(x) | e <- es );
bool aremanyops({production(_,n,choice(L))}) = allops(L);
default bool aremanyops(BGFProdSet ps) = ( !isEmpty(ps) | it && isspecialprod(p) | p <- ps );
bool isspecialprod(production(_,_,terminal(str x))) = isSpecial(x);
bool isspecialprod(production(_,_,choice(L))) = allops(L);
default bool isspecialprod(BGFProduction p) = false;

public set[str] mixedops(SGrammar g) = {n | str n <- domain(g.prods),
	{production(_,n,choice(L))} := g.prods[n],
	[*L1,*L2] := L,
	(allops(L1) && allkeywords(L2))
	||
	(allops(L2) && allkeywords(L1))
	// ||
	// 		(
	// 			{*S1,*S2} := g.prods[n]
	// 		&&
	// 			allops(S1)
	// 		&&
	// 			allkeywords(S2)
	// 		)
};

// Unlike operators, the keywords consists purely of letters
public set[str] onekeyword(SGrammar g) = {n | str n <- domain(g.prods),
	{production(_,n,terminal(x))} := g.prods[n],
	isWord(x)}
;
public set[str] manykeywords(SGrammar g) = {n | str n <- domain(g.prods),
	(
		(
			{production(_,n,choice(L))} := g.prods[n]
		&&
			allkeywords(L)
		)
	||
		allkeywords(g.prods[n])
	)
};
bool allkeywords(BGFExprList es) = ( !isEmpty(es) | it && isKeyword(e) | e <- es );
bool allkeywords(BGFProdSet ps)  = ( !isEmpty(ps) | it &&
(
	(production(_,_,terminal(x)) := p && isWord(x))
	||
	(production(_,_,choice(IL)) := p && allkeywords(IL))
) | p <- ps );

bool isKeyword(terminal(x)) = isWord(x);
bool isKeyword(sequence(L)) = allkeywords(L); // SIC!
default bool isKeyword(BGFExpression e) = false;

public set[str] tokens(SGrammar g) = {n | str n <- domain(g.prods), 
	{production(_,n,plus(choice(L)))} := g.prods[n],
	!isEmpty(L),
	allterminals(L),
	!alltrivial(L)
};
public set[str] findwords(SGrammar g) = {n | str n <- domain(g.prods), prods2words(g.prods[n])};
bool prods2words({production(_,n,plus(choice(L1)))}) = allkeywords(L1);
bool prods2words({production(_,n,star(choice(L2)))}) = allkeywords(L2);
bool prods2words({production(_,n,sequence(L3))}) = allkeywords(L3);
bool prods2words({production(_,n,sequence([terminal(x),star(choice(L4))]))}) = isWord(x) && allkeywords(L4);
// simpleDerivationSet ::= ("#all" | ("list" | "union" | "restriction")*) ;
default bool prods2words(BGFProdSet ps) = false;

public set[str] modifiers(SGrammar g) = {n | str n <- domain(g.prods), alllookslikemodifier(g.prods[n])};
bool alllookslikemodifier(BGFProdSet ps)  = ( !isEmpty(ps) | it && lookslikemodifier(p.rhs) | p <- ps );
bool lookslikemodifier(choice([BGFExpression x,BGFExpression y,sequence([x,y])])) = true;
bool lookslikemodifier(choice([optional(BGFExpression x),sequence([x,BGFExpression y]),y])) = true;
bool lookslikemodifier(choice(
	[sequence([optional(BGFExpression x),BGFExpression y]),
	sequence([y,optional(x)])])) = true;
bool lookslikemodifier(choice(
	[sequence([optional(BGFExpression x),BGFExpression y]),
	sequence([y,x])])) = true;
bool lookslikemodifier(choice(
	[sequence([optional(BGFExpression x),optional(BGFExpression y),BGFExpression z]),
	sequence([optional(y),optional(x),z]),
	sequence([optional(x),z,optional(y)]),
	sequence([optional(y),z,optional(x)]),
	sequence([z,optional(x),optional(y)]),
	sequence([z,optional(y),optional(x)])])) = true;
default bool lookslikemodifier(BGFExpression p) = false;

public set[str] ranges(SGrammar g) = {n | str n <- domain(g.prods),
	(
		(
			{production(_,n,choice(L))} := g.prods[n]
		&&
			alltrivial(L)
		)
	||
		alltrivial(g.prods[n])
	)
};

bool alltrivial(BGFExprList es) = ( !isEmpty(es) | it && terminal(x) := e && len(x)==1 | e <- es );
bool alltrivial(BGFProdSet ps)  = ( !isEmpty(ps) | it && 
(
	(production(_,_,terminal(x)) := p && len(x)==1)
	||
	(production(_,_,choice(L)) := p && alltrivial(L))
) | p <- ps);

public set[str] statements(SGrammar g) = {n | str n <- domain(g.prods),
	{production(_,n,sequence([*L,terminal(";")]))} := g.prods[n],
	allterminals(L)
};

// micropatterns about concrete syntax and terminal symbols
public map[str name,set[str](SGrammar) fun] ConcretePatterns = 
	(
		// "LiteralNillable":		names0,					// identifier names [a-z]*
		"NumericLiteral":		names3,					// identifier names ("+"|"-")? ("0"|...|"9")+
		"LiteralSimple":		names1,					// identifier names [a-z]+
		"LiteralFirstRest":		names2,					// identifier names [a-z][a-zA-Z_]*
		"Keyword":				onekeyword,				// defined as "word"
		"Keywords":				manykeywords,			// defined as ("word1" | "word2" | …), horizontal or vertical
		"Operator":				oneop,					// defined as ">="
		"Operators":			manyops,				// defined as (">=" | "<=" | …), horizontal or vertical
		"OperatorsMixed":		mixedops,				// mix non-alphanumerics and keywords
		"Range":				ranges,					// simple range [a-z]
		"EmptyStatement":		statements,				// T … T ";"
		"Words":				findwords,				// ( "word1" | "word2" | … )+
		"Modifiers":			modifiers,				// ( "public" | "static" | "public" "static" )
		"Tokens":				tokens,					// ( "\n" | "\r" | … )+
		"Preterminal":			preterminals			// defined with terminals (a superset of all of the above)
	);

public set[str] nowknownconcrete(SGrammar g)
	= onekeyword(g)
	+ manykeywords(g)
	+ names1(g)
	+ names2(g)
	+ oneop(g)
	+ manyops(g)
	+ mixedops(g)
	+ ranges(g)
	+ statements(g)
	+ findwords(g)
	+ tokens(g)
	+ modifiers(g)
	;
