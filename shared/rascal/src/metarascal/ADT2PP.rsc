@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@doc{This tool takes an Abstract Data Type defined in Rascal, and generates a minimal
pretty-printer for it, relying on pattern-driven dispatch and the presence of pp() functions
for all inner types}
@wiki{ADT2PP}
module metarascal::ADT2PP

import lib::Rascalware;
import IO;
import syntax::BGF;
import extract::RascalADT2BGF;

public void main(list[str] args) = main(|cwd:///|+args[0], |cwd:///|+args[1]);

public void main(loc rsc1, loc rsc2)
{
	BGFGrammar g = extract::RascalADT2BGF::process(rsc1);
	writeFile(rsc2,
	"@contributor{ADT2PP}
	'module PrettyPrinter // feel free to change that after moving the file
	'
	'// put relevant imports here: ADT definitions and all necessary pp() functions!
	'<for (production(_, str n, choice(rhs)) <- g.prods){><for (selectable(s,e) <- rhs){>
	'public str ppx(<n>::<s>(<showargs(e)>)) = \"<s>(<showppd(e)>)\";<}>
	'public default str ppx(<n> smth) = \"??\<smth\>??\";
	'<}>
	'");
}

str showargs(epsilon()) = "";
str showargs(selectable(s,e)) = "<showtype(e)> <s>";
str showargs(sequence(es)) = mapjoin(showargs,es,", ");
default str showargs(BGFExpression e) = "<e>";

str showppd(epsilon()) = "";
str showppd(selectable(s,val(e))) = "\<<s>\>";
str showppd(selectable(s,e)) = "\<pp(<s>)\>";
str showppd(sequence(es)) = mapjoin(showppd,es,",");
default str showppd(BGFExpression e) = "<e>";

str showtype(val(string())) = "str";
str showtype(val(integer())) = "int";
str showtype(star(e)) = "list[<showtype(e)>]";
str showtype(nonterminal(n)) = "<n>";
default str showtype(e) = ""; // silence is gold

public void tst2() = main(
	|project://slps/src/syntax/XBGF.rsc|,
	|project://slps/src/export/XBNF2.rsc|
);

public void tst3() = main(
	|project://slps/src/syntax/BGF.rsc|,
	|project://slps/src/export/Rascal.rsc|
);
