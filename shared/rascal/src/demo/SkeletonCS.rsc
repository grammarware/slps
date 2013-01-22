@contributor{BGF2Rascal automated exporter - SLPS - http://github.com/grammarware/slps/wiki/BGF2Rascal}
module demo::SkeletonCS

import ParseTree;
import util::IDE;
import IO;

layout WS = [\t-\n\r\ ]* !>> [\t-\n\r\ ];

start syntax CompilationUnit = UsingDirective* usingDirectives GlobalAttributeSection* NamespaceMemberDeclaration*;

syntax UsingDirective = "using" LexNotSemicolon usingDirectiveInsides ";";

syntax GlobalAttributeSection = "[" "assembly" ":" LexNotRightSquareBracket globalAttributeSectionInsides "]";

syntax NamespaceMemberDeclaration = AttributeSection* Modifier* NamespaceMemberDeclarationInsides;

lexical LexNotSemicolon = ![;]* !>> ![;];

syntax Modifier = "new"
 | "public"
 | "protected"
 | "internal"
 | "private"
 | "abstract"
 | "sealed";

syntax AttributeSection = "[" LexNotRightSquareBracket attributeSectionInsides "]";

syntax NamespaceMemberDeclarationInsides = NamespaceMemberName namespaceMemberName LexNotWhitespace identifier1 LexNotLeftCurly? identifier2 LexBalancedCurlies namespaceMemberInsides ";"?;

lexical LexNotLeftCurly = ![{]* !>> ![{];

syntax NamespaceMemberName = "namespace"
 | "class"
 | "struct"
 | "interface"
 | "enum"
 | "delegate";

lexical LexNotWhitespace = ![\ \t]* !>> ![\ \t];

lexical LexBalancedCurlies = [{] (LexBalancedCurlies| LexNotCurly)* [}];

lexical LexNotRightSquareBracket = ![\]]* !>> ![\]];

lexical LexNotCurly = ![{}]* !>> ![{}];


public void main()
{
	registerLanguage("CSharp", "cs", CompilationUnit(str input, loc org) {return parse(#CompilationUnit, input, org);});
	println("Language registered.");
	registerContributions("CSharp",{popup(menu("CSharp",[edit("using2dot",using2dot)]))});
	println("Menu item registered.");}

public str using2dot(CompilationUnit cu, loc z)
{
	list[str] us = [];
	for (UsingDirective ud <- cu.usingDirectives)
		us += ["<ud.usingDirectiveInsides>"];
	return "<us>";
}

public void tryit()
{
	r = parse(#CompilationUnit, |home:///projects/slps/shared/rascal/src/demo/Program.cs|);
	println(r);
	println(using2dot(r,|cwd:///|));
	//println("Language registered.");
}