module LLL1

start syntax LLL1Grammar = LLL1LayoutList LLL1Production* LLL1LayoutList;
syntax LLL1Production = LLL1Nonterminal ":" {LLL1Definition "|"}+ ";";
syntax LLL1Definition = LLL1Symbol+;
syntax LLL1Symbol
 = nonterminal: LLL1Nonterminal
 | terminal: LLL1Terminal
 
 | optional: LLL1Symbol "?"
 | star: LLL1Symbol "*"
 | plus: LLL1Symbol "+";


lexical LLL1Terminal = "\"" LLL1TerminalSymbol* "\"";
lexical LLL1TerminalSymbol = ![\"];
lexical LLL1Nonterminal = [A-Za-z_01-9\-/]+ !>> [A-Za-z_01-9\-/];
layout LLL1LayoutList = LLL1Layout* !>> [\t-\n \r \ ];
lexical LLL1Layout = [\t-\n \r \ ];
