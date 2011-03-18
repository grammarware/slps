module ANTLR3

import IO;
import SourceEditor;

//syntax Grammar = Comment? "grammar" Name ";" HeaderSection* Rule+ LayoutList?;
syntax ANTLR3Grammar = "grammar" ANTLR3Name ";" ANTLR3Header ANTLR3Rule+;
syntax ANTLR3Header = ANTLR3HeaderSection*;
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
 | "@rulecatch"
 | "@lexer::header"
 | "@lexer::members"
 | "@parser::header"
 | "@parser::members"
 ;

syntax ANTLR3Rule = "fragment"? ANTLR3Name ANTLR3Arguments? ANTLR3RuleHeader* ":" {ANTLR3Alternative "|"}+ ";";

syntax ANTLR3RuleHeader
 = ANTLR3Options
 | @category="Comment" "@after" ANTLR3CurlyBlock
 | @category="Comment" "@init" ANTLR3CurlyBlock
 ;

syntax ANTLR3Alternative = ANTLR3AltStart? ANTLR3Symbol* ANTLRAltTail?;

syntax ANTLRAltTail
 = ANTLR3Rewrite ANTLR3OptionalCurlyBlock?
 | ANTLR3OptionalCurlyBlock ANTLR3Rewrite?
 | ANTLR3Rewrite ANTLR3Rewrite // don't ask me why someone would want to do this, but they do
 ;

syntax ANTLR3Symbol
 = ANTLR3Name
 | @category="Variable" ANTLR3QuotedBlock
 | "~"? "(" {ANTLR3Alternative "|"}+ ")"
 | @category="Variable" ANTLR3QuotedBlock ".." ANTLR3QuotedBlock
 | ANTLR3Symbol ANTLR3Modifier
 > ANTLR3Prefix ANTLR3Symbol
 ;
 
syntax ANTLR3Prefix
 = ANTLR3Name ANTLR3SelectorSymbol
 | @category="Comment" ANTLR3UselessPrefix
 ;

syntax ANTLR3SelectorSymbol
 = "="
 | "+="
 ;
 
syntax ANTLR3UselessPrefix
 = ANTLR3OptionsSemicolon
 ;
 
syntax ANTLR3Modifier = "*" | "?" | "+" | @category="Comment" ANTLR3UselessModifier;
syntax ANTLR3UselessModifier = "!" | "^" | ANTLR3CurlyBlock | ANTLR3Arguments;
syntax ANTLR3Terminal = @category="Variable" ANTLR3QuotedBlock;
syntax ANTLR3Name = @category="Constant" lex [A-Za-z0-9_@]+ # [A-Za-z0-9_@] | @category="Constant" lex "." # ".";

syntax ANTLR3AltStart
 = @category="Comment" ANTLR3ParenthesisBlock "=\>"
 | @category="Comment" ANTLR3CurlyBlock;
syntax ANTLR3Arguments = @category="Comment" "returns"? ANTLR3SquareBlock;
syntax ANTLR3Rewrite = @category="Comment" "-\>" ANTLR3OptionalCurlyBlock? ANTLR3RewrittenTree;
syntax ANTLR3RewrittenTree = "^" ANTLR3ParenthesisBlock ANTLR3Modifier? ANTLR3Symbol* | ANTLR3Symbol+;
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
syntax ANTLR3QuotedBlockSymbol = lex ![\'\\] | lex [\\][\'rtnfu?\\\"] | lex "\\.";
 
public ANTLR3Grammar agrammar(str i, loc s){return parse(#ANTLR3Grammar, i, s);}

public void enableAntlr()
{
	registerLanguage("ANTLR3","ag",agrammar);
	registerContributions("ANTLR3",{popup(menu("ANTLR",[edit("simplify!", simplify2str)]))});
}

public ANTLR3Grammar simplify(ANTLR3Grammar g)
{
 return innermost visit(g)
 { 
  case (ANTLR3TokenDefinition)`<ANTLR3Name n> = <ANTLR3QuotedBlock v> ;`
    => (ANTLR3TokenDefinition)`<ANTLR3Name n> : <ANTLR3QuotedBlock v> ;`
  // syntax ANTLR3Rule = "fragment"? ANTLR3Name ANTLR3Arguments? ANTLR3RuleHeader* ":" {ANTLR3Alternative "|"}+ ";";
  case (ANTLR3Rule)`fragment <ANTLR3Name n> <ANTLR3Arguments? _> <ANTLR3RuleHeader* _> : <{ANTLR3Alternative "|"}+ as>;`
    => (ANTLR3Rule)`<ANTLR3Name n>: <{ANTLR3Alternative "|"}+ as>;`
  case (ANTLR3Rule)`<ANTLR3Name n> <ANTLR3Arguments _> <ANTLR3RuleHeader* _> : <{ANTLR3Alternative "|"}+ as>;`
  	=> (ANTLR3Rule)`<ANTLR3Name n>: <{ANTLR3Alternative "|"}+ as>;`
  // the following rule loops infinitely!
  //case (ANTLR3Rule)`<ANTLR3Name n> <ANTLR3RuleHeader+ _> : <{ANTLR3Alternative "|"}+ as>;`
  //	=> (ANTLR3Rule)`<ANTLR3Name n>: <{ANTLR3Alternative "|"}+ as>;`
  // syntax ANTLR3Alternative = ANTLR3AltStart? ANTLR3Symbol* ANTLRAltTail?;
  case (ANTLR3Alternative)`<ANTLR3AltStart _> <ANTLR3Symbol+ ss> <ANTLRAltTail? _>`
  	=> (ANTLR3Alternative)`<ANTLR3Symbol+ ss>`
  case (ANTLR3Alternative)`<ANTLR3Symbol+ ss> <ANTLRAltTail _>`
  	=> (ANTLR3Alternative)`<ANTLR3Symbol+ ss>`
  //	syntax ANTLR3Symbol = "~"? "(" {ANTLR3Alternative "|"}+ ")"
  //case (ANTLR3Symbol)`~ (<{ANTLR3Alternative "|"}+ as>)`
  //  => (ANTLR3Symbol)`(<{ANTLR3Alternative "|"}+ as>)`
  //	syntax ANTLR3Symbol = ANTLR3Symbol ANTLR3Modifier
  case (ANTLR3Symbol)`<ANTLR3Symbol s> <ANTLR3UselessModifier _>`
	=> s
  //	syntax ANTLR3Symbol = ANTLR3Prefix ANTLR3Symbol
  case (ANTLR3Symbol)`<ANTLR3UselessPrefix _> <ANTLR3Symbol s>`
	=> s
  // the following does not work yet Ñ see http://bugs.meta-environment.org/show_bug.cgi?id=1072
  case (ANTLR3Header)`<ANTLR3HeaderSection* before> <ANTLR3UselessHeaderSection _> <ANTLR3HeaderSection* after>`
    => (ANTLR3Header)`<ANTLR3HeaderSection* before><ANTLR3HeaderSection* after>`
 };
}

public str simplify2str(ANTLR3Grammar g, loc z)
{
   return "<simplify(g)>";
}

// import SourceEditor;
// import ANTLR;
// registerLanguage("ANTLR","ag",agrammar);
// registerContributions("ANTLR",{popup(menu("ANTLR",[edit("simplify!", simplify2str)]))});
// a=parse(#ANTLRGrammar,|project://antlr/src/Java.ag|);
// writeTextValueFile(|project://antlr/src/Test.ag|,parse(#ANTLRGrammar,|project://antlr/src/Java.ag|));
