module LLL2

start syntax LLL2Grammar = LLL2LayoutList LLL2Production* ps LLL2LayoutList;
syntax LLL2Production = LLL2Nonterminal lhs ":" LLL2Alternatives rhs ";";
syntax LLL2Alternatives = {LLL2Definition "|"}+;
syntax LLL2Definition = LLL2Symbol+;
syntax LLL2Symbol
 = nonterminal: LLL2Nonterminal
 | terminal: LLL2Terminal
 | group: "(" LLL2Alternatives ias ")"
 | optional: LLL2Symbol "?"
 | star: LLL2Symbol "*"
 | plus: LLL2Symbol "+"
 | sepliststar: "{" LLL2Symbol LLL2Symbol "}*"
 | seplistplus: "{" LLL2Symbol LLL2Symbol "}+";
lexical LLL2Terminal = "\"" LLL2TerminalSymbol* "\"";
lexical LLL2TerminalSymbol = ![\"];
lexical LLL2Nonterminal = [A-Za-z_01-9/\-]+ !>> [A-Za-z_01-9/\-];
layout LLL2LayoutList = LLL2Layout* !>> [\t-\n \r \ ];
lexical LLL2Layout = [\t-\n \r \ ];
