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
public set[str] names3(SGrammar g) = {n | str n <- domain(g.prods),
	{production(_,n,sequence([optional(choice(L1)),plus(choice(L2))]))} := g.prods[n],
	!isEmpty(L1),
	!isEmpty(L2),
	allterminals(L1),
	allofterminals(L2)}
;

// bool isSpecial(str x) = /[\ \@\#\$\&\%\=\\\/\.\,\:\;\*\+\-\_\!\?\(\)\[\]\{\}\<\>\~\'\"\^\|“”‘’]+/ := x; //'
// \‘\’

// Usually operators consist or non-alphanumeric symbols
public set[str] oneop(SGrammar g) = {n | str n <- domain(g.prods),
	{production(_,n,terminal(x))} := g.prods[n],
	/[a-zA-Z]/ !:= x};
public set[str] manyops(SGrammar g) = {n | str n <- domain(g.prods),
	(
		(
			{production(_,n,choice(L))} := g.prods[n]
		&&
			allops(L)
		)
	||
		allops(g.prods[n])
	)
};
bool allops(BGFExprList es) = ( !isEmpty(es) | it && terminal(x) := e && /[a-zA-Z]/ !:= x | e <- es );
bool allops(BGFProdSet ps)  = ( !isEmpty(ps) | it && 
(
	(production(_,_,terminal(x)) := p && /[a-zA-Z]/ !:= x)
	||
	(production(_,_,choice(L)) := p && allops(L))
) | p <- ps );

public set[str] mixedops(SGrammar g) = {n | str n <- domain(g.prods),
	{production(_,n,choice(L))} := g.prods[n],
	[*L1,*L2] := L,
	allops(L1),
	allkeywords(L2)
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
	/^[a-zA-Z]+$/ := x}
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
bool allkeywords(BGFExprList es) = ( !isEmpty(es) | it && terminal(x) := e && /^[a-zA-Z]+$/ := x | e <- es );
bool allkeywords(BGFProdSet ps)  = ( !isEmpty(ps) | it &&
(
	(production(_,_,terminal(x)) := p && /^[a-zA-Z]+$/ := x)
	||
	(production(_,_,choice(L)) := p && allkeywords(L))
) | p <- ps );

public set[str] words(SGrammar g) = {n | str n <- domain(g.prods),
	(
		{production(_,n,plus(choice(L1)))} := g.prods[n]
	&&
		allkeywords(L1)
	) || (
		{production(_,n,star(choice(L2)))} := g.prods[n]
	&&
		allkeywords(L2)
	)
};

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
		"LiteralNillable":		names0,					// identifier names [a-z]*
		"LiteralSigned":		names3,					// identifier names ("+"|"-")? ("0"|...|"9")+
		"LiteralSimple":		names1,					// identifier names [a-z]+
		"LiteralFirstRest":		names2,					// identifier names [a-z][a-zA-Z_]*
		"Keyword":				onekeyword,				// defined as "word"
		"Keywords":				manykeywords,			// defined as ("word1" | "word2" | …), horizontal or vertical
		"Operator":				oneop,					// defined as ">="
		"Operators":			manyops,				// defined as (">=" | "<=" | …), horizontal or vertical
		"OperatorsMixed":		mixedops,				// mix non-alphanumerics and keywords
		"Range":				ranges,					// simple range [a-z]
		"Statement":			statements,				// T … T ";"
		"Words":				words,					// ( "word1" | "word2" | … )+
		"Preterminal":			preterminals			// defined with terminals
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
	+ words(g)
	;
