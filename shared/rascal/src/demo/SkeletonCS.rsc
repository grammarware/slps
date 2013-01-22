@contributor{BGF2Rascal automated exporter - SLPS - http://github.com/grammarware/slps/wiki/BGF2Rascal}
module demo::SkeletonCS

import ParseTree;
import util::IDE;
import IO;

layout WS = [\t\n\ ]* !>> [\t\n\ ];

start syntax CompilationUnit =
	("using" LexNotSemicolon ";")*
	GlobalAttributeSection*
	NamespaceMemberDeclaration*;

syntax GlobalAttributeSection = "[" "assembly" ":" LexNotRightSquareBracket "]";

syntax NamespaceMemberDeclaration = AttributeSection* Modifier* NamespaceMemberDeclarationInsides;

//syntax LexNotSemicolon = LexNotSemicolonChunk+ () >> [;];
//lexical LexNotSemicolonChunk = ![;\ \t\n]* >> ![;\ \t\n];
lexical LexNotSemicolon =  ![;]* !>> ![;];

syntax Modifier = @category="Constant" "new"
 | "public"
 | "protected"
 | "internal"
 | "private"
 | "abstract"
 | "sealed";

syntax AttributeSection = "[" LexNotRightSquareBracket "]";

syntax NamespaceMemberDeclarationInsides = MemberName LexNotWhitespace LexNotLeftCurly LexBalancedCurlies ";"?;

syntax MemberName = "namespace" | "class" | "struct" | "interface" | "enum" | "delegate";

lexical LexNotLeftCurly = ![{]* !>> ![{];

lexical LexNotWhitespace = ![\ \t]* !>> ![\ \t];

lexical LexBalancedCurlies = [{] (LexBalancedCurlies| LexNotCurly)* [}];

lexical LexNotRightSquareBracket = ![\]]* !>> ![\]];

lexical LexNotCurly = ![{}]* !>> ![{}];


public void main()
{
	registerLanguage("Name", "cs", CompilationUnit(str input, loc org) {return parse(#CompilationUnit, input, org);});
	println("Language registered.");
}

public void tryit()
{
	println(parse(#CompilationUnit, |home:///projects/slps/shared/rascal/src/demo/Program.cs|));
	//println("Language registered.");
}