module ANTLR

import IO;
import SourceEditor;

//syntax Grammar = Comment? "grammar" Name ";" HeaderSection* Rule+ LayoutList?;
syntax ANTLRGrammar = "grammar" Name ";" HeaderSection* Rule+;
 
syntax HeaderSection = @category="Comment" SectionName CurlyBlock;

syntax SectionName
 = "options"
 | "tokens"
 | "@header"
 | "@members"
 | "@lexer::header"
 | "@lexer::members"
 ;

syntax Rule = Fragment? Name Arguments? Options? ":" {Alternative "|"}+ ";";
syntax Fragment = "fragment";

syntax Alternative = Predicate? Symbol* Rewrite? OptionalCurlyBlock?;

syntax Symbol
 = Name Arguments?
 | Symbol Modifier
 | Name "=" Symbol
 | OptionsSemicolon? Terminal
 | "~"? "(" {Alternative "|"}+ ")" CurlyBlock?
 | @category="Variable" QuotedBlock ".." QuotedBlock
 ;
 
syntax Modifier = "*" | "?" | "+" | "!" | "^";
syntax Terminal = @category="Variable" QuotedBlock;
syntax Name = @category="Constant" lex [A-Za-z0-9_@]+ # [A-Za-z0-9_@];

syntax Predicate = @category="Comment" ParenthesisBlock "=\>";
syntax Arguments = @category="Comment" SquareBlock;
syntax Rewrite = @category="Comment" "-\>" RewrittenTree;
syntax RewrittenTree = "^" ParenthesisBlock | Name;
syntax OptionsSemicolon = @category="Comment" Options ":";
syntax Options = @category="Comment" "options" CurlyBlock;
syntax OptionalCurlyBlock = @category="Comment" CurlyBlock "?"?;

syntax Layout
 = lex [\ \t\n\r]
 | @category="Comment" lex "//" ![\n]* [\n]
 | @category="Comment" lex "/*" CommentChar* "*/" ;
  
syntax CommentChar = lex ![*] | lex Asterisk ;
syntax Asterisk = lex [*] # [/] ;
layout LayoutList = lex Layout* # [\ \t\n\r] # "/*" # "//";

syntax CurlyBlock  = "{" CurlyBlockElement* "}";
syntax CurlyBlockElement  = ![{}] | CurlyBlock;
syntax SquareBlock = "[" SquareBlockElement* "]";
syntax SquareBlockElement = ![\[\]] | SquareBlock;
syntax ParenthesisBlock = "(" ParenthesisBlockElement* ")";
syntax ParenthesisBlockElement = ![()] | ParenthesisBlock;
syntax QuotedBlock  = [\'] QuotedBlockSymbol* [\'];
syntax QuotedBlockSymbol = lex ![\'\\] | lex [\\][\'rtnu\\\"];
 
public ANTLRGrammar agrammar(str i, loc s){return parse(#ANTLRGrammar, i, s);}

public ANTLRGrammar simplify(ANTLRGrammar g, loc z)
{
 return top-down visit(g)
 { 
  case `grammar <Name n> ; <HeaderSection+ _> <Rule+ rs>`
  	=> (ANTLRGrammar)`grammar <Name n>; <Rule+ rs>`
  case `<Fragment _> <Name n> <Arguments? _> <Options? _> : <{Alternative "|"}+ as>;`
  	=> (Rule)`<Name n>: <{Alternative "|"}+ as>;`
  case `<Name n> <Arguments _> <Options? _> : <{Alternative "|"}+ as>;`
  	=> (Rule)`<Name n>: <{Alternative "|"}+ as>;`
  case `<Name n> <Options _> : <{Alternative "|"}+ as>;`
  	=> (Rule)`<Name n>: <{Alternative "|"}+ as>;`
  case `<Predicate _> <Symbol+ ss> <Rewrite? _> <OptionalCurlyBlock? _>`
  	=> (Alternative)`<Symbol+ ss>`
  case `<Symbol+ ss> <Rewrite _> <OptionalCurlyBlock? _>`
  	=> (Alternative)`<Symbol+ ss>`
  case `<Symbol+ ss> <OptionalCurlyBlock _>`
  	=> (Alternative)`<Symbol+ ss>`
  case `<Name n> <Arguments _>`
  	=> (Symbol)`<Name n>`
  case `<Symbol* s> <Rewrite r>`
	=> (Symbol)`<Symbol* s>`
  case `<Symbol s>!`
	=> s
  case `<Symbol s>^`
	=> s
  case `<OptionsSemicolon _> <Terminal t>`
	=> (Symbol)`<Terminal t>`
 };
}

public str simplify2str(ANTLRGrammar g, loc z)
{
   return "<simplify(g,z)>";
}

// import SourceEditor;
// import ANTLR;
// registerLanguage("ANTLR","ag",agrammar);
// registerContributions("ANTLR",{popup(menu("ANTLR",[edit("simplify!", simplify2str)]))});
// a=parse(#ANTLRGrammar,|project://antlr/src/Java.ag|);
// writeTextValueFile(|project://antlr/src/Test.ag|,parse(#ANTLRGrammar,|project://antlr/src/Java.ag|));
