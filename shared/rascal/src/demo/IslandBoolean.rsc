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
	g1 = readBGF(|home:///mutatedF2.bgf|);
	g1.roots = ["compilation-unit"];
	g2 = vtransform(addlex,g1);
	g3 = mutate([skeletonise],g2);
	//println(pp(subgrammar(g2,"compilation-unit")));
	println(pp(g3));
	for (p:production(_,"namespace-body",_)<- g3.prods)
		iprintln(p);
}

public void gold()
{
	//g0 = readBGF(|home:///projects/slps/topics/grammars/csharp/ecma-334-1/grammar.bgf|);
	//g0 = readBGF(|home:///projects/slps/topics/grammars/csharp/ecma-334-1/good.bgf|);
	//println(gdtv(grammar(["compilation-unit"],g0.prods),subgrammar(g0,"compilation-unit")));
	//g1 = mutate([
	//	deyaccifyAll,
	//	unchainAll,
	//	inlinePlus
	//],g0);
	//g2 = vtransform(afterMutations,g1);
	//g3 = vtransform(m2,g2);
	//writeBGF(g3,|home:///mutated3.bgf|);
	g3 = readBGF(|home:///mutated3.bgf|);
	g4 = vtransform(m3,g3);
	for (p:production(_,"namespace-member-declaration",_) <- g4.prods)	iprintln(p);
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
	vertical(innt("namespace-member-declaration")),
	appear(production("","namespace-member-declaration",sequence([
  		marked(star(nonterminal("attribute-section"))),
      	marked(star(nonterminal("modifier"))),
      	nonterminal("namespace-declaration")
    ]))),
	inline("type-declaration"),
	horizontal(innt("namespace-member-declaration")),
	factor(
		choice([
	      sequence([
	          star(nonterminal("attribute-section")),
	          star(nonterminal("modifier")),
	          nonterminal("namespace-declaration")
	        ]),
	      sequence([
	          star(nonterminal("attribute-section")),
	          star(nonterminal("modifier")),
	          nonterminal("type-declaration-insides")
	        ])
	    ]),
	    sequence([
	        star(nonterminal("attribute-section")),
            star(nonterminal("modifier")),
		    choice([
		          nonterminal("namespace-declaration"),
		          nonterminal("type-declaration-insides")
		    ])
		]),
	    innt("namespace-member-declaration")
	),
	extract(production("","namespace-member-declaration-insides",choice([
		          nonterminal("namespace-declaration"),
		          nonterminal("type-declaration-insides")
		    ])),globally()),
	//inline("namespace-declaration"),
	//extract(production("","namespace-body-insides",
	//	sequence([
	//		star(sequence([
	//			terminal("using"),
	//			nonterminal("using-directive-insides"),
	//			terminal(";")
	//		])),
	//		star(nonterminal("namespace-member-declaration"))
	//	])),
	//	globally()),
	//inline("namespace-body"),
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

XBGFSequence addlex = [
		// addC([] using-directive-insides ::= not-semicolon ;)
		addC(production("lex","using-directive-insides",nonterminal("not-semicolon"))),
		// addC( global-attribute-section-insides ::= not-right-square-bracket ;)
		addC(production("lex","global-attribute-section-insides",nonterminal("not-right-square-bracket"))),
		// addC( attribute-section-insides ::= not-right-square-bracket ;)
		addC(production("lex","attribute-section-insides",nonterminal("not-right-square-bracket"))),
		// addC( struct-interfaces ::= not-left-curly ;)
		addC(production("lex","struct-interfaces",nonterminal("not-left-curly"))),
		// addC( class-base ::= not-left-curly ;)
		addC(production("lex","class-base",nonterminal("not-left-curly"))),
		// addC( interface-base ::= not-left-curly ;)
		addC(production("lex","interface-base",nonterminal("not-left-curly"))),
		// addC( enum-base ::= not-left-curly ;)
		addC(production("lex","enum-base",nonterminal("not-left-curly"))),
		// addC( enum-body-insides ::= balanced-curlies ;)
		//addC(production("","enum-body-insides",nonterminal("balanced-curlies"))),
		// addC( namespace-body-insides ::= balanced-curlies ;)
		// ???
		//addC(production("lex","namespace-body-insides",nonterminal("balanced-curlies"))),
		// addC( class-member-declarations ::= balanced-curlies ;)
		//addC(production("","class-member-declarations",nonterminal("balanced-curlies"))),
		// addC( struct-member-declarations ::= balanced-curlies ;)
		//addC(production("","struct-member-declarations",nonterminal("balanced-curlies"))),
		// addC( interface-member-declarations ::= balanced-curlies ;)
		//addC(production("","interface-member-declarations",nonterminal("balanced-curlies"))),
		// addC( formal-parameter-list ::= not-right-parenthesis ;)
		addC(production("lex","formal-parameter-list",nonterminal("not-right-parenthesis"))),
		// addC( qualified-identifier ::= not-whitespace ;)
		addC(production("lex","qualified-identifier",nonterminal("not-whitespace"))),
		// define( identifier ::= not-whitespace ;)
		define([production("","identifier",nonterminal("not-whitespace"))]),
		// addC( type ::= not-whitespace ;)
		//addC(production("","type",nonterminal("not-whitespace")))
		// a bit more brutal than it could be
		addC(production("lex","namespace-member-declaration-insides",
		sequence([
			nonterminal("not-whitespace"),
			nonterminal("not-whitespace"),
			nonterminal("not-left-curly"),
			nonterminal("balanced-curlies"),
			optional(terminal(";"))
		])
		// TODO: delegates
)),
		
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
