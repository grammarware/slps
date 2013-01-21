@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module lib::Tolerant

import syntax::BGF;

BGFGrammar library = grammar([],
	[
		production("","not-semicolon",star(not(terminal(";")))),
		production("","not-right-square-bracket",star(not(terminal("]")))),
		production("","not-left-curly",star(not(terminal("{")))),
		production("","not-curly",star(not(choice([terminal("{"),terminal("}")])))),
		production("","not-whitespace",star(not(choice([terminal(" "),terminal("\t")])))),
		production("","balanced-curlies",
			sequence([
				terminal("{"),
				star(choice([
					nonterminal("balanced-curlies"),
					nonterminal("not-curly")
				])),
				terminal("}")
			])
		)
	]
);