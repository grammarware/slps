@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module lib::Tolerant

import syntax::BGF;

BGFGrammar library = grammar([],
	[
		production("","lex-not-semicolon",star(not(terminal(";")))),
		production("","lex-not-right-square-bracket",star(not(terminal("]")))),
		production("","lex-not-left-curly",star(not(terminal("{")))),
		production("","lex-not-curly",star(not(choice([terminal("{"),terminal("}")])))),
		production("","lex-not-whitespace",star(not(choice([terminal(" "),terminal("\t")])))),
		production("","lex-balanced-curlies",
			sequence([
				terminal("{"),
				star(choice([
					nonterminal("lex-balanced-curlies"),
					nonterminal("lex-not-curly")
				])),
				terminal("}")
			])
		)
	]
);