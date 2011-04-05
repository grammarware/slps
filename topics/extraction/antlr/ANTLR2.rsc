module ANTLR2

import util::IDE;

//syntax ANTLRGrammar = Parser Lexer;
syntax ANTLRGrammar = ANTLRParser;
syntax ANTLRParser = "class" ANTLRName "extends" "Parser" ";" ANTLRHeader? ANTLRRule+;
syntax ANTLRHeader = ANTLRHeaderSection* ANTLRSemanticPredicate;

// syntax ANTLRLexer = "class" Name "extends" "Lexer" ";" HeaderSection* CurlyBlock? Rule+;

syntax ANTLRKeywords
 = "class" | "extends" | "options" | "tokens" | "fragment" | "protected" | "@header" | "@members" ;
 
syntax ANTLRHeaderSection = @category="Comment" ANTLRSectionName ANTLRCurlyBlock;

syntax ANTLRSectionName
 = "options"
 | "tokens"
 | "@header"
 | "@members"
 ;

syntax ANTLRRule = ANTLRRulePrefix? ANTLRName ANTLRArguments? ANTLRRuleModifier? ":" ANTLRAlternatives ";";
syntax ANTLRAlternatives = {ANTLRAlternative "|"}+;
syntax ANTLRRulePrefix = "fragment" | "protected";
syntax ANTLRRuleModifier = ANTLRSemanticPredicate | ANTLROptions ANTLRSemanticPredicate?;

syntax ANTLRAlternative = ANTLRSemanticPredicate? ANTLRSyntacticPredicate? ANTLRSymbol* ANTLRRewrite?;// | SemanticPredicate;

syntax ANTLRSymbol
 = ANTLRName
 | @category="Variable" ANTLRQuotedBlock
 | "~"? "(" ANTLRAlternatives ")"
 | ANTLRSymbol ANTLRModifier
 > ANTLRPrefix ANTLRSymbol
 | @category="Variable" ANTLRQuotedBlock ".." ANTLRQuotedBlock
 ;
 
syntax ANTLRPrefix
 = ANTLRName ":"
 | ANTLRUselessPrefix;

syntax ANTLRUselessPrefix
 = ANTLROptions ":" ANTLRPredicate?
 ;
 
syntax ANTLRPredicate
 = ANTLRSyntacticPredicate
 | ANTLRSemanticPredicate
 ;
 
syntax ANTLRModifier = "*" | "?" | "+" | @category="Comment" ANTLRUselessModifier;
syntax ANTLRUselessModifier = "!" | "^" | ANTLRSemanticPredicate | ANTLRSquareBlock ;
syntax ANTLRName = @category="Constant" lex [A-Za-z0-9_@]+ - ANTLRKeywords # [A-Za-z0-9_@];

syntax ANTLRSyntacticPredicate = @category="Comment" ANTLRParenthesisBlock "=\>";
syntax ANTLRArguments = @category="Comment" ANTLRSquareBlock | "!" ANTLRSquareBlock?;
syntax ANTLRRewrite = @category="Comment" "-\>" ANTLRRewrittenTree;
syntax ANTLRRewrittenTree = "^" ANTLRParenthesisBlock | ANTLRName;
syntax ANTLROptions = @category="Comment" "options" ANTLRCurlyBlock;
syntax ANTLRSemanticPredicate
 = @category="Comment" ANTLRCurlyBlock
 | @category="Comment" "{" ANTLRCurlyBlockElement* "}" "?";

syntax ANTLRLayout
 = lex [\ \t\n\r]
 | @category="Comment" lex "//" ![\n]* [\n]
 | @category="Comment" lex "/*" ANTLRCommentChar* "*/" ;
  
syntax ANTLRCommentChar = lex ![*] | lex ANTLRAsterisk ;
syntax ANTLRAsterisk = lex [*] # [/] ;
layout ANTLRLayoutList = lex ANTLRLayout* # [\ \t\n\r] # "/*" # "//";

syntax ANTLRCurlyBlock  = "{" ANTLRCurlyBlockElement* "}" # "?";
syntax ANTLRCurlyBlockElement  = ![{}] | ANTLRCurlyBlock;
syntax ANTLRSquareBlock = "[" ANTLRSquareBlockElement* "]";
syntax ANTLRSquareBlockElement = ![\[\]] | ANTLRSquareBlock;
syntax ANTLRParenthesisBlock = "(" ANTLRParenthesisBlockElement* ")";
syntax ANTLRParenthesisBlockElement = ![()] | ANTLRParenthesisBlock;
syntax ANTLRQuotedBlock  = [\"] ANTLRQuotedBlockSymbol* [\"];
syntax ANTLRQuotedBlockSymbol = lex ![\"\\] | lex [\\][\'rtnuf\\\"];
 
public ANTLRGrammar agrammar(str i, loc s){return parse(#ANTLRGrammar, i, s);}

public void enableAntlr()
{
	registerLanguage("ANTLR2","ag",agrammar);
	registerContributions("ANTLR2",{popup(menu("ANTLR",[edit("simplify!", simplify2str)]))});
}

public ANTLRGrammar simplify(ANTLRGrammar g)
{
 return innermost visit(g)
 { 
  // "class" Name "extends" "Parser" ";" HeaderSection* SemanticPredicate? Rule+;
  case (ANTLRParser)`class <ANTLRName n> extends Parser; <ANTLRHeader _> <ANTLRRule+ rs>`
  	=> (ANTLRParser)`class <ANTLRName n> extends Parser; <ANTLRRule+ rs>`
  // RulePrefix? Name Arguments? RuleModifier? ":" {Alternative "|"}+ ";"
  case (ANTLRRule)`<ANTLRRulePrefix _> <ANTLRName n> <ANTLRArguments? _> <ANTLRRuleModifier? _> : <ANTLRAlternatives as>;`
  	=> (ANTLRRule)`<ANTLRName n>: <ANTLRAlternatives as>;`
  case (ANTLRRule)`<ANTLRName n> <ANTLRArguments _> <ANTLRRuleModifier? _> : <ANTLRAlternatives as>;`
  	=> (ANTLRRule)`<ANTLRName n>: <ANTLRAlternatives as>;`
  case (ANTLRRule)`<ANTLRName n> <ANTLRRuleModifier _> : <ANTLRAlternatives as>;`
  	=> (ANTLRRule)`<ANTLRName n>: <ANTLRAlternatives as>;`
  // SemanticPredicate? SyntacticPredicate? Symbol* Rewrite?;
  case (ANTLRAlternative)`<ANTLRSemanticPredicate _> <ANTLRSyntacticPredicate? _> <ANTLRSymbol+ ss> <ANTLRRewrite? _>`
	=> (ANTLRAlternative)`<ss>`
  case (ANTLRAlternative)`<ANTLRSyntacticPredicate _> <ANTLRSymbol+ ss> <ANTLRRewrite? _>`
	=> (ANTLRAlternative)`<ss>`
  case (ANTLRAlternative)`<ANTLRSymbol+ ss> <ANTLRRewrite _>`
	=> (ANTLRAlternative)`<ss>`
  // Prefix+ Symbol
  case (ANTLRSymbol)`<ANTLRUselessPrefix _> <ANTLRSymbol s>`
	=> s
  // ANTLRSymbol ANTLRModifier+
  case (ANTLRSymbol)`<ANTLRSymbol s> <ANTLRUselessModifier _>`
	=> s
  // "~"? "(" {Alternative "|"}+ ")"
  case (ANTLRSymbol)`~ (<ANTLRAlternatives as>)`
	=> (ANTLRSymbol)`(<ANTLRAlternatives as>)`
 };
}

public str simplify2str(ANTLRGrammar g, loc z)
{
   return "<simplify(g)>";
}

// import SourceEditor;
// import ANTLR;
// registerLanguage("ANTLR","ag",agrammar);
// registerContributions("ANTLR",{popup(menu("ANTLR",[edit("simplify!", simplify2str)]))});
// a=parse(#ANTLRGrammar,|project://antlr/src/Java.ag|);
// writeTextValueFile(|project://antlr/src/Test.ag|,parse(#ANTLRGrammar,|project://antlr/src/Java.ag|));
// writeFile(|project://antlr/src/S1.out.ag|,simplify(parse(#ANTLRGrammar,|project://antlr/src/S1.ag|)));
