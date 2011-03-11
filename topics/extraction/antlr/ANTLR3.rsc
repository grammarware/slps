module ANTLR3

import IO;
import SourceEditor;

//syntax Grammar = Comment? "grammar" Name ";" HeaderSection* Rule+ LayoutList?;
syntax ANTLR3Grammar = "grammar" ANTLR3Name ";" ANTLR3HeaderSection* ANTLR3Rule+;
 
syntax ANTLR3HeaderSection = ANTLR3UselessHeaderSection | ANTLR3TokensHeaderSection;
syntax ANTLR3UselessHeaderSection = @category="Comment" ANTLR3SectionName ANTLR3CurlyBlock;
syntax ANTLR3TokensHeaderSection = "tokens" "{" ANTLR3TokenDefinition+ "}";

syntax ANTLR3TokenDefinition
 = ANTLR3RealTokenDefinition
 | ANTLR3FakeTokenDefinition
 | ANTLR3UselessTokenDefinition;
syntax ANTLR3UselessTokenDefinition = ANTLR3Name ";" ;
syntax ANTLR3RealTokenDefinition    = ANTLR3Name "=" ANTLR3QuotedBlock ";" ;
syntax ANTLR3FakeTokenDefinition    = ANTLR3Name ":" ANTLR3QuotedBlock ";" ;

syntax ANTLR3SectionName
 = "options"
 | "@header"
 | "@members"
 | "@lexer::header"
 | "@lexer::members"
 ;

syntax ANTLR3Rule = "fragment"? ANTLR3Name ANTLR3Arguments? ANTLR3Options? ":" {ANTLR3Alternative "|"}+ ";";

syntax ANTLR3Alternative = ANTLR3Predicate? ANTLR3Symbol* ANTLR3Rewrite? ANTLR3OptionalCurlyBlock?;

syntax ANTLR3Symbol
 = ANTLR3Name ANTLR3Arguments?
 | ANTLR3Symbol ANTLR3Modifier
 | ANTLR3Name "=" ANTLR3Symbol
 | ANTLR3OptionsSemicolon? ANTLR3Terminal
 | "~"? "(" {ANTLR3Alternative "|"}+ ")" ANTLR3CurlyBlock?
 | @category="Variable" ANTLR3QuotedBlock ".." ANTLR3QuotedBlock
 ;
 
syntax ANTLR3Modifier = "*" | "?" | "+" | ANTLR3UselessModifier;
syntax ANTLR3UselessModifier = "!" | "^";
syntax ANTLR3Terminal = @category="Variable" ANTLR3QuotedBlock;
syntax ANTLR3Name = @category="Constant" lex [A-Za-z0-9_@]+ # [A-Za-z0-9_@];

syntax ANTLR3Predicate = @category="Comment" ANTLR3ParenthesisBlock "=\>";
syntax ANTLR3Arguments = @category="Comment" ANTLR3SquareBlock;
syntax ANTLR3Rewrite = @category="Comment" "-\>" ANTLR3RewrittenTree;
syntax ANTLR3RewrittenTree = "^" ANTLR3ParenthesisBlock | ANTLR3Name;
syntax ANTLR3OptionsSemicolon = @category="Comment" ANTLR3Options ":";
syntax ANTLR3Options = @category="Comment" "options" ANTLR3CurlyBlock;
syntax ANTLR3OptionalCurlyBlock = @category="Comment" ANTLR3CurlyBlock "?"?;

syntax ANTLR3Layout
 = lex [\ \t\n\r]
 | @category="Comment" lex "//" ![\n]* [\n]
 | @category="Comment" lex "/*" ANTLR3CommentChar* "*/" ;
  
syntax ANTLR3CommentChar = lex ![*] | lex ANTLR3Asterisk ;
syntax ANTLR3Asterisk = lex [*] # [/] ;
layout ANTLR3LayoutList = lex ANTLR3Layout* # [\ \t\n\r] # "/*" # "//";

syntax ANTLR3CurlyBlock  = "{" ANTLR3CurlyBlockElement* "}";
syntax ANTLR3CurlyBlockElement  = ![{}] | ANTLR3CurlyBlock;
syntax ANTLR3SquareBlock = "[" ANTLR3SquareBlockElement* "]";
syntax ANTLR3SquareBlockElement = ![\[\]] | ANTLR3SquareBlock;
syntax ANTLR3ParenthesisBlock = "(" ANTLR3ParenthesisBlockElement* ")";
syntax ANTLR3ParenthesisBlockElement = ![()] | ANTLR3ParenthesisBlock;
syntax ANTLR3QuotedBlock  = [\'] ANTLR3QuotedBlockSymbol* [\'];
syntax ANTLR3QuotedBlockSymbol = lex ![\'\\] | lex [\\][\'rtnu\\\"];
 
public ANTLR3Grammar agrammar(str i, loc s){return parse(#ANTLR3Grammar, i, s);}

public void enableAntlr()
{
	registerLanguage("ANTLR3","ag",agrammar);
	registerContributions("ANTLR3",{popup(menu("ANTLR",[edit("simplify!", simplify2str)]))});
}

public ANTLR3Grammar simplify(ANTLR3Grammar g, loc z)
{
 return top-down visit(g)
 { 
  //case `grammar <ANTLR3Name n> ; <ANTLR3HeaderSection+ _> <ANTLR3Rule+ rs>`
  // => (ANTLRGrammar)`grammar <ANTLR3Name n>; <ANTLR3Rule+ rs>`
  // syntax ANTLR3HeaderSection = ANTLR3UselessHeaderSection | ANTLR3TokensHeaderSection;
  //case (ANTLR3HeaderSection+)`<ANTLR3UselessHeaderSection _> <ANTLR3TokensHeaderSection t>` => (ANTLR3HeaderSection+)`<ANTLR3TokensHeaderSection t>`
  //case (ANTLR3HeaderSection+)`<ANTLR3TokensHeaderSection t> <ANTLR3UselessHeaderSection _>` => (ANTLR3HeaderSection+)`<ANTLR3TokensHeaderSection t>`
  case (ANTLR3TokenDefinition)`<ANTLR3Name n> = <ANTLR3QuotedBlock v> ;`
    => (ANTLR3TokenDefinition)`<ANTLR3Name n> : <ANTLR3QuotedBlock v> ;`
  case (ANTLR3Rule)`fragment <ANTLR3Name n> <ANTLR3Arguments? _> <ANTLR3Options? _> : <{ANTLR3Alternative "|"}+ as>;`
  	=> (ANTLR3Rule)`<ANTLR3Name n>: <{ANTLR3Alternative "|"}+ as>;`
  case (ANTLR3Rule)`<ANTLR3Name n> <ANTLR3Arguments _> <ANTLR3Options? _> : <{ANTLR3Alternative "|"}+ as>;`
  	=> (ANTLR3Rule)`<ANTLR3Name n>: <{ANTLR3Alternative "|"}+ as>;`
  case (ANTLR3Rule)`<ANTLR3Name n> <ANTLR3Options _> : <{ANTLR3Alternative "|"}+ as>;`
  	=> (ANTLR3Rule)`<ANTLR3Name n>: <{ANTLR3Alternative "|"}+ as>;`
  case (ANTLR3Alternative)`<ANTLR3Predicate _> <ANTLR3Symbol+ ss> <ANTLR3Rewrite? _> <ANTLR3OptionalCurlyBlock? _>`
  	=> (ANTLR3Alternative)`<ANTLR3Symbol+ ss>`
  case (ANTLR3Alternative)`<ANTLR3Symbol+ ss> <ANTLR3Rewrite _> <ANTLR3OptionalCurlyBlock? _>`
  	=> (ANTLR3Alternative)`<ANTLR3Symbol+ ss>`
  case (ANTLR3Alternative)`<ANTLR3Symbol+ ss> <ANTLR3OptionalCurlyBlock _>`
  	=> (ANTLR3Alternative)`<ANTLR3Symbol+ ss>`
  case (ANTLR3Alternative)`<ANTLR3Name n> <ANTLR3Arguments _>`
  	=> (ANTLR3Symbol)`<ANTLR3Name n>`
  case (ANTLR3Alternative)`<ANTLR3Symbol* s> <ANTLR3Rewrite r>`
	=> (ANTLR3Symbol)`<ANTLR3Symbol* s>`
  case (ANTLR3Alternative)`<ANTLR3Symbol s> <ANTLR3UselessModifier _>`
	=> s
  case (ANTLR3Alternative)`<ANTLR3OptionsSemicolon _> <ANTLR3Terminal t>`
	=> (ANTLR3Symbol)`<ANTLR3Terminal t>`
 };
}

public str simplify2str(ANTLR3Grammar g, loc z)
{
   return "<simplify(g,z)>";
}

// import SourceEditor;
// import ANTLR;
// registerLanguage("ANTLR","ag",agrammar);
// registerContributions("ANTLR",{popup(menu("ANTLR",[edit("simplify!", simplify2str)]))});
// a=parse(#ANTLRGrammar,|project://antlr/src/Java.ag|);
// writeTextValueFile(|project://antlr/src/Test.ag|,parse(#ANTLRGrammar,|project://antlr/src/Java.ag|));
