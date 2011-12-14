module EliminateGroup

import LLL1;
import LLL2;
import IO;
import ParseTree;
import List;

public void main(list[str] args)
{
	str currentNT = "FOO";
	LLL2Grammar g = parse(#LLL2Grammar,readFile(|cwd:///|+args[0]));
	//println(g);
	list[LLL2Production] addprods = [];
	int cx = 0;
	LLL2Grammar g2 = top-down visit(g)
	{
		case LLL2Production p: currentNT = "<p.lhs>";
		case LLL2Symbol s:
			if(s is group)
			{
				cx += 1;
				LLL2Nonterminal lhs = parse(#LLL2Nonterminal,"<currentNT><cx>");
				LLL2Alternatives rhs = s.ias;
				addprods += `<lhs>:<rhs>;`;
				insert lhs;
			}
	};
	if(LLL2Production* rules := g2.ps)
	{
		for(LLL2Production p <- addprods)
			rules = (LLL2Production*)`<rules><p>`;
		g2.ps = rules;
	}
	writeFile(|cwd:///|+args[1],g2);
	println("Done: <size(addprods)> new production rules added.");
}
