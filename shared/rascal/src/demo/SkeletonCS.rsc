@contributor{BGF2Rascal automated exporter - SLPS - http://github.com/grammarware/slps/wiki/BGF2Rascal}
@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module demo::SkeletonCS

import ParseTree;
import util::IDE;
import IO;
import Exception;
import String;

//extend lang::std::Layout; // + \0xFEFF

lexical Whitespace 
  = [\u0009-\u000D \u0020 \u0085 \u00A0 \u1680 \u180E \u2000-\u200A \u2028 \u2029 \u202F \u205F \u3000 \uFEFF]
  ; 
layout Standard 
  = WhitespaceOrComment*
   !>> [\u0009-\u000D \u0020 \u0085 \u00A0 \u1680 \u180E \u2000-\u200A \u2028 \u2029 \u202F \u205F \u3000 \uFEFF]
   !>> "//";
  
syntax WhitespaceOrComment 
  = whitespace: Whitespace
  | comment: Comment
  ; 
lexical Comment
	= @category="Comment" "//" ![\n\r]* $
	| @category="Comment" "/*" CommentElement* "*/"
	;
lexical CommentElement = ![*] | [*] !>> "/";

//layout WS = [\t-\n\r\ \uFEFF]* !>> [\t-\n\r\ \uFEFF];

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
 | "sealed"
 | "static" // added from another version of C#
;

syntax AttributeSection = "[" LexNotRightSquareBracket attributeSectionInsides "]";

syntax NamespaceMemberDeclarationInsides = NamespaceMemberName namespaceMemberName LexNotWhitespace? identifier1 LexNotLeftCurly? identifier2 LexBalancedCurlies namespaceMemberInsides ";"?;

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

public str using2dot(start[CompilationUnit] cu, loc z)
{
	list[str] us = [];
	for (UsingDirective ud <- cu.top.usingDirectives)
		us += ["<ud.usingDirectiveInsides>"];
	return "<us>";
}

public void tryit()
{
	r = parse(#start[CompilationUnit], |home:///projects/slps/shared/rascal/src/demo/sample.cs|);
	println(r);
	println(using2dot(r,|cwd:///|));
	//println("Language registered.");
}

public void tryall()
{
	loc dir = |home:///projects/fodder/csharp/mono/|;
	int good = 0, bad = 0;
	for (f <- listEntries(dir))
	{
		if (!endsWith(f,".cs")) continue;
		println("Processing <f>...");
		//if (good+bad % 100 == 0)
		println("<good+bad> files processed: <good> successes, <bad> failures.");
		try
		{
			parse(#start[CompilationUnit], dir+f);
			good += 1;
		}
		catch ParseError(_):
		//catch:
			bad += 1;
	}
	println("Grand total: <good> files parsed, <bad> files failed.");
}