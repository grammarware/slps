@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module demo::IslandBoolean

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::XBGF;
import diff::GDT;
import io::ReadBGF;
import io::WriteBGF;
import export::BNF;
import export::XPNF;
import mutate::Mutations;
import IO;

public void ppAll()
{
	println(export::XPNF::ppxs(afterMutations+m2+m3));
}

public void go()
{
	g1 = readBGF(|home:///mutatedF1.bgf|);
	g2 = vtransform(skeletonise,g1);
	println(pp(subgrammar(g2,"compilation-unit")));
}

public void gold()
{
	//g0 = readBGF(|home:///projects/slps/topics/grammars/csharp/ecma-334-1/grammar.bgf|);
	g0 = readBGF(|home:///projects/slps/topics/grammars/csharp/ecma-334-1/good.bgf|);
	println(gdtv(grammar(["compilation-unit"],g0.prods),subgrammar(g0,"compilation-unit")));
	g1 = mutate([
		deyaccifyAll,
		unchainAll,
		inlinePlus
		//,inlineLazy
	//],subgrammar(g0,"compilation-unit"));
	],g0);
	//writeBGF(g1,|home:///mutated2.bgf|);
	g2 = vtransform(afterMutations,g1);
	//println(pp(transform([afterMutations[0],afterMutations[1],afterMutations[2],afterMutations[3],afterMutations[4]],g1)));
	//g2 = readBGF(|home:///mutated.bgf|);
	g3 = vtransform(m2,g2);
	//g3 = readBGF(|home:///mutated4.bgf|);
	// TODO: insert m4 at the start of m3
	g4 = vtransform(m3,g3);
	//println(pp(subgrammar(g2,"namespace-member-declaration")));
	//println(pp(g4));
	//for (p:production(_,"attribute-section",_) <- g4.prods)		iprintln(p);
	//println(pp(g3));
	writeBGF(g4,|home:///mutatedF2.bgf|);
	//iprintln(afterMutations);
}

XBGFSequence m3 = [
	splitT(",]",[",","]"],innt("attribute-section")),
	factor(
		choice([
			sequence([
				terminal("["),
				optional(nonterminal("attribute-target-specifier")),
				nonterminal("attribute-list"),
				terminal("]")
				]),
			sequence([
				terminal("["),
				optional(nonterminal("attribute-target-specifier")),
				nonterminal("attribute-list"),
				terminal(","),
				terminal("]")
				])
		]),
		sequence([
			terminal("["),
			choice([
			sequence([
				optional(nonterminal("attribute-target-specifier")),
				nonterminal("attribute-list")
				]),
			sequence([
				optional(nonterminal("attribute-target-specifier")),
				nonterminal("attribute-list"),
				terminal(",")
				])
			]),
			terminal("]")
		]),
		innt("attribute-section")
	),
	extract(
		production("","attribute-section-insides",choice([
			sequence([
				optional(nonterminal("attribute-target-specifier")),
				nonterminal("attribute-list")
				]),
			sequence([
				optional(nonterminal("attribute-target-specifier")),
				nonterminal("attribute-list"),
				terminal(",")
				])
		])),
		globally()
	),
	inline("namespace-declaration"),
	inline("type-declaration"),
	bypass()
];

XBGFSequence m2 = [
	inline("class-declaration"),
	inline("struct-declaration"),
	inline("interface-declaration"),
	inline("enum-declaration"),
	inline("delegate-declaration"),
	renameN("class-modifier","modifier"),
	unite("struct-modifier","modifier"),
	unite("interface-modifier","modifier"),
	unite("enum-modifier","modifier"),
	unite("delegate-modifier","modifier"),
	factor(
		choice([
			sequence([
				star(nonterminal("attribute-section")),
				star(nonterminal("modifier")),
				terminal("class"),
				nonterminal("identifier"),
				optional(nonterminal("class-base")),
				nonterminal("class-body"),
				optional(terminal(";"))
				]),
			sequence([
				star(nonterminal("attribute-section")),
				star(nonterminal("modifier")),
				terminal("struct"),
				nonterminal("identifier"),
				optional(nonterminal("struct-interfaces")),
				nonterminal("struct-body"),
				optional(terminal(";"))
				]),
			sequence([
				star(nonterminal("attribute-section")),
				star(nonterminal("modifier")),
				terminal("interface"),
				nonterminal("identifier"),
				optional(nonterminal("interface-base")),
				nonterminal("interface-body"),
				optional(terminal(";"))
				]),
			sequence([
				star(nonterminal("attribute-section")),
				star(nonterminal("modifier")),
				terminal("enum"),
				nonterminal("identifier"),
				optional(nonterminal("enum-base")),
				nonterminal("enum-body"),
				optional(terminal(";"))
				]),
			sequence([
				star(nonterminal("attribute-section")),
				star(nonterminal("modifier")),
				terminal("delegate"),
				nonterminal("type"),
				nonterminal("identifier"),
				terminal("("),
				optional(nonterminal("formal-parameter-list")),
				terminal(")"),
				terminal(";")
				])
		]),
		sequence([
			star(nonterminal("attribute-section")),
			star(nonterminal("modifier")),
			choice([
				sequence([
					terminal("class"),
					nonterminal("identifier"),
					optional(nonterminal("class-base")),
					nonterminal("class-body"),
					optional(terminal(";"))
					]),
				sequence([
					terminal("struct"),
					nonterminal("identifier"),
					optional(nonterminal("struct-interfaces")),
					nonterminal("struct-body"),
					optional(terminal(";"))
					]),
				sequence([
					terminal("interface"),
					nonterminal("identifier"),
					optional(nonterminal("interface-base")),
					nonterminal("interface-body"),
					optional(terminal(";"))
					]),
				sequence([
					terminal("enum"),
					nonterminal("identifier"),
					optional(nonterminal("enum-base")),
					nonterminal("enum-body"),
					optional(terminal(";"))
					]),
				sequence([
					terminal("delegate"),
					nonterminal("type"),
					nonterminal("identifier"),
					terminal("("),
					optional(nonterminal("formal-parameter-list")),
					terminal(")"),
					terminal(";")
					])
			])
		]),
		innt("type-declaration")),
	extract(
		production("","type-declaration-insides",choice([
		sequence([
			terminal("class"),
			nonterminal("identifier"),
			optional(nonterminal("class-base")),
			nonterminal("class-body"),
			optional(terminal(";"))
			]),
		sequence([
			terminal("struct"),
			nonterminal("identifier"),
			optional(nonterminal("struct-interfaces")),
			nonterminal("struct-body"),
			optional(terminal(";"))
			]),
		sequence([
			terminal("interface"),
			nonterminal("identifier"),
			optional(nonterminal("interface-base")),
			nonterminal("interface-body"),
			optional(terminal(";"))
			]),
		sequence([
			terminal("enum"),
			nonterminal("identifier"),
			optional(nonterminal("enum-base")),
			nonterminal("enum-body"),
			optional(terminal(";"))
			]),
		sequence([
			terminal("delegate"),
			nonterminal("type"),
			nonterminal("identifier"),
			terminal("("),
			optional(nonterminal("formal-parameter-list")),
			terminal(")"),
			terminal(";")
			])
		])),
		globally()),
	// TODO: maybe just make unite more advanced?
	vertical(innt("modifier")),
	horizontal(innt("modifier")),
	bypass()
];


XBGFSequence afterMutations = [
	// adapt using
	inline("using-alias-directive"),
	inline("using-namespace-directive"),
	factor(
		choice([
			sequence([
				terminal("using"),
				nonterminal("identifier"),
				terminal("="),
				nonterminal("namespace-or-type-name"),
				terminal(";")
			]),
			sequence([
				terminal("using"),
				nonterminal("namespace-name"),
				terminal(";")
			])
		]),
		sequence([
			terminal("using"),
			choice([
				nonterminal("namespace-name"),
				sequence([
					nonterminal("identifier"),
					terminal("="),
					nonterminal("namespace-or-type-name")
				])
			]),
			terminal(";")
		]),
		innt("using-directive")),
	extract(
		production(
		"",
		"using-directive-insides",
		choice([
				nonterminal("namespace-name"),
				sequence([
					nonterminal("identifier"),
					terminal("="),
					nonterminal("namespace-or-type-name")
				])
			])),
		globally()),
	inline("using-directive"),
	// adapt global attribute
	splitT(",]",[",","]"],innt("global-attribute-section")),
	factor(
		choice([
			sequence([
				terminal("["),
				nonterminal("global-attribute-target-specifier"),
				nonterminal("attribute-list"),
				terminal("]")
			]),
			sequence([
				terminal("["),
				nonterminal("global-attribute-target-specifier"),
				nonterminal("attribute-list"),
				terminal(","),
				terminal("]")
			])
		]),
		sequence([
			terminal("["),
			nonterminal("global-attribute-target-specifier"),
			choice([
				nonterminal("attribute-list"),
				sequence([
					nonterminal("attribute-list"),
					terminal(",")
				])
			]),
			terminal("]")
		]),
		innt("global-attribute-section")),
	inline("global-attribute-target-specifier"),
	inline("global-attribute-target"),
	extract(
		production(
		"",
		"global-attribute-section-insides",
		choice([
			nonterminal("attribute-list"),
			sequence([
				nonterminal("attribute-list"),
				terminal(",")
				])
			])),
		globally()),
	// adapt namespace
	bypass()
	];

XBGFSequence skeletonise = [
		// addC([] using-directive-insides ::= not-semicolon ;)
		addC(production("lex-UD","using-directive-insides",nonterminal("not-semicolon"))),
		// addC( global-attribute-section-insides ::= not-right-square-bracket ;)
		addC(production("lex-GAS","global-attribute-section-insides",nonterminal("not-right-square-bracket"))),
		// addC( attribute-section-insides ::= not-right-square-bracket ;)
		addC(production("lex-AS","attribute-section-insides",nonterminal("not-right-square-bracket"))),
		// addC( struct-interfaces ::= not-left-curly ;)
		addC(production("lex-SI","struct-interfaces",nonterminal("not-left-curly"))),
		// addC( class-base ::= not-left-curly ;)
		addC(production("lex-CB","class-base",nonterminal("not-left-curly"))),
		// addC( interface-base ::= not-left-curly ;)
		addC(production("lex-IB","interface-base",nonterminal("not-left-curly"))),
		// addC( enum-base ::= not-left-curly ;)
		addC(production("lex-EB","enum-base",nonterminal("not-left-curly"))),
		// addC( enum-body-insides ::= balanced-curlies ;)
		//addC(production("","enum-body-insides",nonterminal("balanced-curlies"))),
		// addC( namespace-body-insides ::= balanced-curlies ;)
		//addC(production("","namespace-body-insides",nonterminal("balanced-curlies"))),
		// addC( class-member-declarations ::= balanced-curlies ;)
		//addC(production("","class-member-declarations",nonterminal("balanced-curlies"))),
		// addC( struct-member-declarations ::= balanced-curlies ;)
		//addC(production("","struct-member-declarations",nonterminal("balanced-curlies"))),
		// addC( interface-member-declarations ::= balanced-curlies ;)
		//addC(production("","interface-member-declarations",nonterminal("balanced-curlies"))),
		// addC( formal-parameter-list ::= not-right-parenthesis ;)
		addC(production("lex-FPL","formal-parameter-list",nonterminal("not-right-parenthesis"))),
		// addC( qualified-identifier ::= not-whitespace ;)
		addC(production("lex-QI","qualified-identifier",nonterminal("not-whitespace"))),
		// define( identifier ::= not-whitespace ;)
		define([production("lex-I","identifier",nonterminal("not-whitespace"))]),
		// addC( type ::= not-whitespace ;)
		//addC(production("","type",nonterminal("not-whitespace")))
		bypass()
	];

BGFGrammar doTrafo(BGFGrammar g)
	= vtransform(
	AdaptUsing
	+AdaptGlobal
	+AdaptNamespace
	+SimplifyNamespace
	+Skeletonise,
	g);

public void run()
{
	g1 = readBGF(|home:///projects/slps/topics/grammars/csharp/ecma-334-1/grammar.bgf|);
	g2 = subgrammar(doTrafo(g1),"compilation-unit"); 
	println(pp(g2));
	writeBGF(g2,|home:///unmutated.bgf|);
}
