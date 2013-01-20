@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module demo::IslandBoolean

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::XBGF;
//import diff::GDT;
import io::ReadBGF;
import io::WriteBGF;
import export::BNF;
import mutate::Mutations;
import IO;

public void go()
{
		//g1 = readBGF(|home:///projects/slps/topics/grammars/csharp/ecma-334-1/grammar.bgf|);
		//gr = mutate([
		//	deyaccifyAll,
		//	unchainAll,
		//	inlinePlus
		//	,inlineLazy
		//],subgrammar(g1,"compilation-unit"));
		//writeBGF(gr,|home:///mutated.bgf|);
	gr = readBGF(|home:///mutated.bgf|);
	println(pp(gr));
	for (p:production(_,"namespace-member-declaration",_) <- gr.prods)		iprintln(p);
	g2 = vtransform(afterMutations,gr);
	println(pp(g2));
	iprintln(afterMutations);
}

XBGFSequence afterMutations = [
	// adapt using
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
				choice([
					nonterminal("identifier"),
					sequence([
						nonterminal("namespace-or-type-name"),
						terminal("."),
						nonterminal("identifier")
					])
				]),
				terminal(";")
			])
		]),
		sequence([
			terminal("using"),
			choice([
				nonterminal("identifier"),
				sequence([
					nonterminal("namespace-or-type-name"),
					terminal("."),
					nonterminal("identifier")
				]),
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
			nonterminal("identifier"),
			sequence([
				nonterminal("namespace-or-type-name"),
				terminal("."),
				nonterminal("identifier")
				]),
			sequence([
				nonterminal("identifier"),
				terminal("="),
				nonterminal("namespace-or-type-name")
				])
			])),
		globally()),
	inline("using-directive"),
	// adapt global attribute
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
		innt("compilation-unit")),
	inline("global-attribute-target-specifier"),
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

//XBGFSequence

XBGFSequence AdaptUsing = [
		// vertical(innt(using-directives))
		vertical(innt("using-directives")),
		// deyaccify(using-directives)
		deyaccify("using-directives"),
		// inline(using-directives)
		inline("using-directives"),
		// inline(using-alias-directive)
		inline("using-alias-directive"),
		// inline(using-namespace-directive)
		inline("using-namespace-directive"),
		// massage(using-directive+?,using-directive*,globally())
		massage(optional(plus(nonterminal("using-directive"))),star(nonterminal("using-directive")),globally()),
		// factor(
		// 	(("using" (identifier "=" namespace-or-type-name) ";") | ("using" (namespace-name) ";")),
		//	 ("using" ((identifier "=" namespace-or-type-name) | (namespace-name)) ";")
		//,globally())
		factor(choice([sequence([terminal("using"),sequence([nonterminal("identifier"),terminal("="),nonterminal("namespace-or-type-name")]),terminal(";")]),sequence([terminal("using"),sequence([nonterminal("namespace-name")]),terminal(";")])]),sequence([terminal("using"),choice([sequence([nonterminal("identifier"),terminal("="),nonterminal("namespace-or-type-name")]),sequence([nonterminal("namespace-name")])]),terminal(";")]),globally()),
		// extract(using-directive-insides ::= ((identifier "=" namespace-or-type-name) | (namespace-name)) ;,globally())
		extract(production("","using-directive-insides",choice([sequence([nonterminal("identifier"),terminal("="),nonterminal("namespace-or-type-name")]),sequence([nonterminal("namespace-name")])])),globally()),
		// vertical(innt(using-directive-insides))
		vertical(innt("using-directive-insides")),
		// inline(using-directive)
		inline("using-directive"),
		bypass()
	];

XBGFSequence AdaptGlobal = [
		// vertical(innt(global-attribute-sections))
		vertical(innt("global-attribute-sections")),
		// deyaccify(global-attribute-sections)
		deyaccify("global-attribute-sections"),
		// unchain(global-attributes ::= global-attribute-sections ;)
		unchain(production("","global-attributes",nonterminal("global-attribute-sections"))),
		// inline(global-attributes)
		inline("global-attributes"),
		// massage(global-attribute-section+?,global-attribute-section*,globally())
		massage(optional(plus(nonterminal("global-attribute-section"))),star(nonterminal("global-attribute-section")),globally()),
		// factor((("[" global-attribute-target-specifier attribute-list "]") | ("[" global-attribute-target-specifier (attribute-list ",") "]")),("[" global-attribute-target-specifier (attribute-list | (attribute-list ",")) "]"),globally())
		factor(choice([sequence([terminal("["),nonterminal("global-attribute-target-specifier"),nonterminal("attribute-list"),terminal("]")]),sequence([terminal("["),nonterminal("global-attribute-target-specifier"),sequence([nonterminal("attribute-list"),terminal(",")]),terminal("]")])]),sequence([terminal("["),nonterminal("global-attribute-target-specifier"),choice([nonterminal("attribute-list"),sequence([nonterminal("attribute-list"),terminal(",")])]),terminal("]")]),globally()),
		// extract(global-attribute-section-insides ::= (attribute-list | (attribute-list ",")) ;,globally())
		extract(production("","global-attribute-section-insides",choice([nonterminal("attribute-list"),sequence([nonterminal("attribute-list"),terminal(",")])])),globally()),
		// vertical(innt(global-attribute-section-insides))
		vertical(innt("global-attribute-section-insides")),
		// inline(global-attribute-section)
		inline("global-attribute-section"),
		// inline(global-attribute-target-specifier)
		inline("global-attribute-target-specifier"),
		// inline(global-attribute-target)
		inline("global-attribute-target"),
		bypass()
	];

XBGFSequence AdaptNamespace = [
		// vertical(innt(namespace-member-declarations))
		vertical(innt("namespace-member-declarations")),
		// deyaccify(namespace-member-declarations)
		deyaccify("namespace-member-declarations"),
		// inline(namespace-declaration)
		inline("namespace-declaration"),
		// inline(type-declaration)
		inline("type-declaration"),
		// inline(namespace-member-declarations)
		inline("namespace-member-declarations"),
		// inline(class-declaration)
		inline("class-declaration"),
		// inline(struct-declaration)
		inline("struct-declaration"),
		// inline(interface-declaration)
		inline("interface-declaration"),
		// inline(enum-declaration)
		inline("enum-declaration"),
		// inline(delegate-declaration)
		inline("delegate-declaration"),
		// massage(namespace-member-declaration+?,namespace-member-declaration*,globally())
		massage(optional(plus(nonterminal("namespace-member-declaration"))),star(nonterminal("namespace-member-declaration")),globally()),
		// vertical(innt(attribute-sections))
		vertical(innt("attribute-sections")),
		// deyaccify(attribute-sections)
		deyaccify("attribute-sections"),
		// inline(attribute-sections)
		inline("attribute-sections"),
		// inline(attributes)
		inline("attributes"),
		// massage(attribute-section+?,attribute-section*,globally())
		massage(optional(plus(nonterminal("attribute-section"))),star(nonterminal("attribute-section")),globally()),
		// vertical(innt(class-modifiers))
		vertical(innt("class-modifiers")),
		// deyaccify(class-modifiers)
		deyaccify("class-modifiers"),
		// vertical(innt(struct-modifiers))
		vertical(innt("struct-modifiers")),
		// deyaccify(struct-modifiers)
		deyaccify("struct-modifiers"),
		// vertical(innt(interface-modifiers))
		vertical(innt("interface-modifiers")),
		// deyaccify(interface-modifiers)
		deyaccify("interface-modifiers"),
		// vertical(innt(enum-modifiers))
		vertical(innt("enum-modifiers")),
		// deyaccify(enum-modifiers)
		deyaccify("enum-modifiers"),
		// vertical(innt(delegate-modifiers))
		vertical(innt("delegate-modifiers")),
		// deyaccify(delegate-modifiers)
		deyaccify("delegate-modifiers"),
		// renameN(class-modifier,modifier)
		renameN("class-modifier","modifier"),
		// unite(struct-modifier,modifier)
		unite("struct-modifier","modifier"),
		// unite(interface-modifier,modifier)
		unite("interface-modifier","modifier"),
		// unite(enum-modifier,modifier)
		unite("enum-modifier","modifier"),
		// unite(delegate-modifier,modifier)
		unite("delegate-modifier","modifier"),
		// horizontal(innt(modifier))
		horizontal(innt("modifier")),
		// inline(class-modifiers)
		inline("class-modifiers"),
		// inline(struct-modifiers)
		inline("struct-modifiers"),
		// inline(interface-modifiers)
		inline("interface-modifiers"),
		// inline(enum-modifiers)
		inline("enum-modifiers"),
		// inline(delegate-modifiers)
		inline("delegate-modifiers"),
		// massage(modifier+?,modifier*,globally())
		massage(optional(plus(nonterminal("modifier"))),star(nonterminal("modifier")),globally()),
		// factor((("[" (attribute-target-specifier? attribute-list) "]") | ("[" (attribute-target-specifier? attribute-list ",") "]")),("[" ((attribute-target-specifier? attribute-list) | (attribute-target-specifier? attribute-list ",")) "]"),globally())
		factor(choice([sequence([terminal("["),sequence([optional(nonterminal("attribute-target-specifier")),nonterminal("attribute-list")]),terminal("]")]),sequence([terminal("["),sequence([optional(nonterminal("attribute-target-specifier")),nonterminal("attribute-list"),terminal(",")]),terminal("]")])]),sequence([terminal("["),choice([sequence([optional(nonterminal("attribute-target-specifier")),nonterminal("attribute-list")]),sequence([optional(nonterminal("attribute-target-specifier")),nonterminal("attribute-list"),terminal(",")])]),terminal("]")]),globally()),
		// extract(attribute-section-insides ::= ((attribute-target-specifier? attribute-list) | (attribute-target-specifier? attribute-list ",")) ;,globally())
		extract(production("","attribute-section-insides",choice([sequence([optional(nonterminal("attribute-target-specifier")),nonterminal("attribute-list")]),sequence([optional(nonterminal("attribute-target-specifier")),nonterminal("attribute-list"),terminal(",")])])),globally()),
		// vertical(innt(attribute-section-insides))
		vertical(innt("attribute-section-insides")),
		// inline(attribute-section)
		inline("attribute-section"),
		// inline(class-body)
		inline("class-body"),
		// inline(struct-body)
		inline("struct-body"),
		// inline(interface-body)
		inline("interface-body"),
		// factor((("{" (enum-member-declarations?) "}") | ("{" (enum-member-declarations ",") "}")),("{" ((enum-member-declarations?) | (enum-member-declarations ",")) "}"),globally())
		factor(choice([sequence([terminal("{"),sequence([optional(nonterminal("enum-member-declarations"))]),terminal("}")]),sequence([terminal("{"),sequence([nonterminal("enum-member-declarations"),terminal(",")]),terminal("}")])]),sequence([terminal("{"),choice([sequence([optional(nonterminal("enum-member-declarations"))]),sequence([nonterminal("enum-member-declarations"),terminal(",")])]),terminal("}")]),globally()),
		// extract(enum-body-insides ::= ((enum-member-declarations?) | (enum-member-declarations ",")) ;,globally())
		extract(production("","enum-body-insides",choice([sequence([optional(nonterminal("enum-member-declarations"))]),sequence([nonterminal("enum-member-declarations"),terminal(",")])])),globally()),
		// vertical(innt(enum-body-insides))
		vertical(innt("enum-body-insides")),
		// inline(enum-body)
		inline("enum-body"),
		// extract(namespace-body-insides ::= ("using" using-directive-insides ";")* namespace-member-declaration* ;,globally())
		extract(production("","namespace-body-insides",sequence([star(sequence([terminal("using"),nonterminal("using-directive-insides"),terminal(";")])),star(nonterminal("namespace-member-declaration"))])),globally()),
		// inline(namespace-body)
		inline("namespace-body"),
		bypass()
	];

XBGFSequence SimplifyNamespace = [
		// vertical(innt(namespace-member-declaration))
		vertical(innt("namespace-member-declaration")),
		// appear(namespace-member-declaration ::= <("[" attribute-section-insides "]")*> "namespace" qualified-identifier "{" namespace-body-insides "}" ";"? ;)
		appear(production("","namespace-member-declaration",sequence([marked(star(sequence([terminal("["),nonterminal("attribute-section-insides"),terminal("]")]))),terminal("namespace"),nonterminal("qualified-identifier"),terminal("{"),nonterminal("namespace-body-insides"),terminal("}"),optional(terminal(";"))]))),
		// appear(namespace-member-declaration ::= ("[" attribute-section-insides "]")* <modifier*> "namespace" qualified-identifier "{" namespace-body-insides "}" ";"? ;)
		appear(production("","namespace-member-declaration",sequence([star(sequence([terminal("["),nonterminal("attribute-section-insides"),terminal("]")])),marked(star(nonterminal("modifier"))),terminal("namespace"),nonterminal("qualified-identifier"),terminal("{"),nonterminal("namespace-body-insides"),terminal("}"),optional(terminal(";"))]))),
		// appear(namespace-member-declaration ::= ("[" attribute-section-insides "]")* modifier* "namespace" qualified-identifier <class-base?> "{" namespace-body-insides "}" ";"? ;)
		appear(production("","namespace-member-declaration",sequence([star(sequence([terminal("["),nonterminal("attribute-section-insides"),terminal("]")])),star(nonterminal("modifier")),terminal("namespace"),nonterminal("qualified-identifier"),marked(optional(nonterminal("class-base"))),terminal("{"),nonterminal("namespace-body-insides"),terminal("}"),optional(terminal(";"))]))),
		// horizontal(innt(namespace-member-declaration))
		horizontal(innt("namespace-member-declaration")),
		// factor(((("[" attribute-section-insides "]")* modifier* ("namespace" qualified-identifier class-base? "{" namespace-body-insides "}" ";"?)) | (("[" attribute-section-insides "]")* modifier* ("class" identifier class-base? "{" class-member-declarations? "}" ";"?)) | (("[" attribute-section-insides "]")* modifier* ("struct" identifier struct-interfaces? "{" struct-member-declarations? "}" ";"?)) | (("[" attribute-section-insides "]")* modifier* ("interface" identifier interface-base? "{" interface-member-declarations? "}" ";"?)) | (("[" attribute-section-insides "]")* modifier* ("enum" identifier enum-base? "{" enum-body-insides "}" ";"?)) | (("[" attribute-section-insides "]")* modifier* ("delegate" type identifier "(" formal-parameter-list? ")" ";"))),(("[" attribute-section-insides "]")* modifier* (("namespace" qualified-identifier class-base? "{" namespace-body-insides "}" ";"?) | ("class" identifier class-base? "{" class-member-declarations? "}" ";"?) | ("struct" identifier struct-interfaces? "{" struct-member-declarations? "}" ";"?) | ("interface" identifier interface-base? "{" interface-member-declarations? "}" ";"?) | ("enum" identifier enum-base? "{" enum-body-insides "}" ";"?) | ("delegate" type identifier "(" formal-parameter-list? ")" ";"))),globally())
		factor(choice([sequence([star(sequence([terminal("["),nonterminal("attribute-section-insides"),terminal("]")])),star(nonterminal("modifier")),sequence([terminal("namespace"),nonterminal("qualified-identifier"),optional(nonterminal("class-base")),terminal("{"),nonterminal("namespace-body-insides"),terminal("}"),optional(terminal(";"))])]),sequence([star(sequence([terminal("["),nonterminal("attribute-section-insides"),terminal("]")])),star(nonterminal("modifier")),sequence([terminal("class"),nonterminal("identifier"),optional(nonterminal("class-base")),terminal("{"),optional(nonterminal("class-member-declarations")),terminal("}"),optional(terminal(";"))])]),sequence([star(sequence([terminal("["),nonterminal("attribute-section-insides"),terminal("]")])),star(nonterminal("modifier")),sequence([terminal("struct"),nonterminal("identifier"),optional(nonterminal("struct-interfaces")),terminal("{"),optional(nonterminal("struct-member-declarations")),terminal("}"),optional(terminal(";"))])]),sequence([star(sequence([terminal("["),nonterminal("attribute-section-insides"),terminal("]")])),star(nonterminal("modifier")),sequence([terminal("interface"),nonterminal("identifier"),optional(nonterminal("interface-base")),terminal("{"),optional(nonterminal("interface-member-declarations")),terminal("}"),optional(terminal(";"))])]),sequence([star(sequence([terminal("["),nonterminal("attribute-section-insides"),terminal("]")])),star(nonterminal("modifier")),sequence([terminal("enum"),nonterminal("identifier"),optional(nonterminal("enum-base")),terminal("{"),nonterminal("enum-body-insides"),terminal("}"),optional(terminal(";"))])]),sequence([star(sequence([terminal("["),nonterminal("attribute-section-insides"),terminal("]")])),star(nonterminal("modifier")),sequence([terminal("delegate"),nonterminal("type"),nonterminal("identifier"),terminal("("),optional(nonterminal("formal-parameter-list")),terminal(")"),terminal(";")])])]),sequence([star(sequence([terminal("["),nonterminal("attribute-section-insides"),terminal("]")])),star(nonterminal("modifier")),choice([sequence([terminal("namespace"),nonterminal("qualified-identifier"),optional(nonterminal("class-base")),terminal("{"),nonterminal("namespace-body-insides"),terminal("}"),optional(terminal(";"))]),sequence([terminal("class"),nonterminal("identifier"),optional(nonterminal("class-base")),terminal("{"),optional(nonterminal("class-member-declarations")),terminal("}"),optional(terminal(";"))]),sequence([terminal("struct"),nonterminal("identifier"),optional(nonterminal("struct-interfaces")),terminal("{"),optional(nonterminal("struct-member-declarations")),terminal("}"),optional(terminal(";"))]),sequence([terminal("interface"),nonterminal("identifier"),optional(nonterminal("interface-base")),terminal("{"),optional(nonterminal("interface-member-declarations")),terminal("}"),optional(terminal(";"))]),sequence([terminal("enum"),nonterminal("identifier"),optional(nonterminal("enum-base")),terminal("{"),nonterminal("enum-body-insides"),terminal("}"),optional(terminal(";"))]),sequence([terminal("delegate"),nonterminal("type"),nonterminal("identifier"),terminal("("),optional(nonterminal("formal-parameter-list")),terminal(")"),terminal(";")])])]),globally()),
		// extract(namespace-member-main ::= (("namespace" qualified-identifier class-base? "{" namespace-body-insides "}" ";"?) | ("class" identifier class-base? "{" class-member-declarations? "}" ";"?) | ("struct" identifier struct-interfaces? "{" struct-member-declarations? "}" ";"?) | ("interface" identifier interface-base? "{" interface-member-declarations? "}" ";"?) | ("enum" identifier enum-base? "{" enum-body-insides "}" ";"?) | ("delegate" type identifier "(" formal-parameter-list? ")" ";")) ;,globally())
		extract(production("","namespace-member-main",choice([sequence([terminal("namespace"),nonterminal("qualified-identifier"),optional(nonterminal("class-base")),terminal("{"),nonterminal("namespace-body-insides"),terminal("}"),optional(terminal(";"))]),sequence([terminal("class"),nonterminal("identifier"),optional(nonterminal("class-base")),terminal("{"),optional(nonterminal("class-member-declarations")),terminal("}"),optional(terminal(";"))]),sequence([terminal("struct"),nonterminal("identifier"),optional(nonterminal("struct-interfaces")),terminal("{"),optional(nonterminal("struct-member-declarations")),terminal("}"),optional(terminal(";"))]),sequence([terminal("interface"),nonterminal("identifier"),optional(nonterminal("interface-base")),terminal("{"),optional(nonterminal("interface-member-declarations")),terminal("}"),optional(terminal(";"))]),sequence([terminal("enum"),nonterminal("identifier"),optional(nonterminal("enum-base")),terminal("{"),nonterminal("enum-body-insides"),terminal("}"),optional(terminal(";"))]),sequence([terminal("delegate"),nonterminal("type"),nonterminal("identifier"),terminal("("),optional(nonterminal("formal-parameter-list")),terminal(")"),terminal(";")])])),globally()),
		// vertical(innt(namespace-member-main))
		vertical(innt("namespace-member-main")),
		// horizontal(innt(namespace-member-main))
		horizontal(innt("namespace-member-main")),
		bypass()
	];
XBGFSequence Skeletonise = [
		// redefine([] using-directive-insides ::= not-semicolon ;)
		redefine([production("","using-directive-insides",nonterminal("not-semicolon"))]),
		// redefine([] global-attribute-section-insides ::= not-right-square-bracket ;)
		redefine([production("","global-attribute-section-insides",nonterminal("not-right-square-bracket"))]),
		// redefine([] attribute-section-insides ::= not-right-square-bracket ;)
		redefine([production("","attribute-section-insides",nonterminal("not-right-square-bracket"))]),
		// redefine([] struct-interfaces ::= not-left-curly ;)
		redefine([production("","struct-interfaces",nonterminal("not-left-curly"))]),
		// redefine([] class-base ::= not-left-curly ;)
		redefine([production("","class-base",nonterminal("not-left-curly"))]),
		// redefine([] interface-base ::= not-left-curly ;)
		redefine([production("","interface-base",nonterminal("not-left-curly"))]),
		// redefine([] enum-base ::= not-left-curly ;)
		redefine([production("","enum-base",nonterminal("not-left-curly"))]),
		// redefine([] enum-body-insides ::= balanced-curlies ;)
		redefine([production("","enum-body-insides",nonterminal("balanced-curlies"))]),
		// redefine([] namespace-body-insides ::= balanced-curlies ;)
		redefine([production("","namespace-body-insides",nonterminal("balanced-curlies"))]),
		// redefine([] class-member-declarations ::= balanced-curlies ;)
		redefine([production("","class-member-declarations",nonterminal("balanced-curlies"))]),
		// redefine([] struct-member-declarations ::= balanced-curlies ;)
		redefine([production("","struct-member-declarations",nonterminal("balanced-curlies"))]),
		// redefine([] interface-member-declarations ::= balanced-curlies ;)
		redefine([production("","interface-member-declarations",nonterminal("balanced-curlies"))]),
		// redefine([] formal-parameter-list ::= not-right-parenthesis ;)
		redefine([production("","formal-parameter-list",nonterminal("not-right-parenthesis"))]),
		// redefine([] qualified-identifier ::= not-whitespace ;)
		redefine([production("","qualified-identifier",nonterminal("not-whitespace"))]),
		// define([] identifier ::= not-whitespace ;)
		define([production("","identifier",nonterminal("not-whitespace"))]),
		// redefine([] type ::= not-whitespace ;)
		redefine([production("","type",nonterminal("not-whitespace"))])
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

