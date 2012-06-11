@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module ConvergePython

import syntax::BGF;
import syntax::XBGF;
import io::ReadBGF;
import io::WriteXBGF;
import transform::XBGF;
import diff::GDT;

void main()
{
	g1 = readBGF(|home:///projects/slps/topics/convergence/xbgf/fl/bgf/py.bgf|);
	g2 = readBGF(|home:///projects/slps/topics/convergence/xbgf/fl/bgf/antlr.anonymize.rename.bgf|);
	XBGFSequence x1 =
	[
		eliminate("_Literal"),
		vertical(innt("atom")),
		renameN("literal","int"),
		renameN("operators","ops"),
		inline("_IF"),
		inline("_THEN"),
		inline("_ELSE"),
		undefine(["int","name"]), //not sure
		define([production("","StringEnd",epsilon())]),
		inline("StringEnd")
	];
	XBGFSequence x2 =
	[
		vertical(innt("expr")),
		designate(production("binary","expr",nonterminal("binary"))),
		designate(production("apply","expr",nonterminal("apply"))),
		designate(production("ifThenElse","expr",nonterminal("ifThenElse"))),
		horizontal(innt("expr")),
		inject(production("","function",sequence([nonterminal("name"),plus(nonterminal("name")),terminal("="),nonterminal("expr"),marked(plus(nonterminal("newline")))])))
	];
	gdtv(transform(x1 + x2,g1),g2);
	writeXBGF(x1,|home:///projects/slps/topics/convergence/xbgf/fl/xbgf/rename-python.xbgf|);
	writeXBGF(x2,|home:///projects/slps/topics/convergence/xbgf/fl/xbgf/designate-python.xbgf|);
}