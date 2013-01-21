@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@contributor{ADT2PP}
@wiki{BGF2Rascal}
module export::Rascal

import lib::Rascalware;
import syntax::BGF;
import String;

public str pprsc(BGFGrammar g) =
"@contributor{BGF2Rascal automated exporter - SLPS - http://github.com/grammarware/slps/wiki/BGF2Rascal}
'module Name
'
'import ParseTree;
'import util::IDE;
'
'<for(p<-g.prods){><pprsc(p)><}>
";

public str pprsc(BGFProduction p)
{
	//if (/not(_) := p)
	if (startsWith(p.lhs,"lex-"))
		return "lexical <p.lhs> = <pprscl(p.rhs)>;\n\n";
	else
		return "syntax <p.lhs> = <pprscstop(p.rhs)>;\n\n";
 //= "syntax <p.lhs> = <pprsc(p.rhs)>;\n\n";
}

public str pprsc(BGFProduction p) = "syntax <p.lhs> = <pprsc(p.rhs)>;\n\n";

str pprscstop(BGFExpression::choice(BGFExprList exprs)) = "<mapjoin(pprscsbr,exprs,"\n | ")>";
default str pprscstop(BGFExpression e) = pprscs(e);

str pprscsbr(e:choice(BGFExprList exprs)) = "(<pprscs(e)>)";
str pprscsbr(e:sequence(BGFExprList exprs)) = "(<pprscs(e)>)";
default str pprscsbr(BGFExpression e) = pprscs(e);

public str pprscs(BGFExpression::epsilon()) = "epsilon()";
public str pprscs(BGFExpression::empty()) = "empty()";
public str pprscs(BGFExpression::val(BGFValue v)) = "val(<pprscs(v)>)";
public str pprscs(BGFExpression::anything()) = "anything()";
public str pprscs(BGFExpression::terminal(str t)) = "\"<t>\"";
public str pprscs(BGFExpression::nonterminal(str t)) = t;
public str pprscs(BGFExpression::selectable(str selector, BGFExpression expr)) = "selectable(<selector>,<pprscs(expr)>)";
public str pprscs(BGFExpression::sequence(BGFExprList exprs)) = "<mapjoin(pprscs,exprs," ")>";
public str pprscs(BGFExpression::choice(BGFExprList exprs)) = "<mapjoin(pprscsbr,exprs," | ")>";
public str pprscs(BGFExpression::allof(BGFExprList exprs)) = "allof(<pprscs(exprs)>)";
public str pprscs(BGFExpression::marked(BGFExpression expr)) = "ERROR";
public str pprscs(BGFExpression::optional(BGFExpression expr)) = "<pprscsbr(expr)>?";
public str pprscs(BGFExpression::not(BGFExpression expr)) = "!<pprscs(expr)>";
public str pprscs(BGFExpression::plus(BGFExpression expr)) = "<pprscsbr(expr)>+";
public str pprscs(BGFExpression::star(BGFExpression expr)) = "<pprscsbr(expr)>*";
public str pprscs(BGFExpression::seplistplus(BGFExpression expr, BGFExpression sep)) = "seplistplus(<pprscs(expr)>,<pprscs(sep)>)";
public str pprscs(BGFExpression::sepliststar(BGFExpression expr, BGFExpression sep)) = "sepliststar(<pprscs(expr)>,<pprscs(sep)>)";
public default str pprscs(BGFExpression smth) = "??<smth>??";

public str pprsct(str t)
{
	str c;
	switch(t)
	{
		case " ": c = "\\ ";
		case "\t": c = "\\t";
		case "[": c = "\\[";
		case "]": c = "\\]";
		default: c = t;  
	}
	return c;
}

str pprsclin(terminal(str t)) = pprsct(t);
default str pprsclin(BGFExpression) = "ERROR";

public str pprscl(BGFExpression::not(e)) = "!<pprscl(e)>";
public str pprscl(BGFExpression::terminal(str t)) = "[<pprsct(t)>]";

public str pprscl(BGFExpression::choice(BGFExprList exprs)) = "[<mapjoin(pprsclin,exprs,"")>]";
public str pprscl(BGFExpression::star(BGFExpression e)) = "<pprscl(e)>* !\>\> <pprscl(e)>";
//public str pprscl(BGFExpression::star(e1:not(BGFExpression e2))) = "<pprscl(e1)>* !\>\> <pprscl(e2)>";
//

//public str pprscl(BGFExpression::epsilon()) = "epsilon()";
//public str pprscl(BGFExpression::empty()) = "empty()";
//public str pprscl(BGFExpression::val(BGFValue v)) = "val(<pprscl(v)>)";
//public str pprscl(BGFExpression::anything()) = "anything()";
//public str pprscl(BGFExpression::nonterminal(str t)) = "nonterminal(<t>)";
//public str pprscl(BGFExpression::selectable(str selector, BGFExpression expr)) = "selectable(<selector>,<pprscl(expr)>)";
//public str pprscl(BGFExpression::sequence(BGFExprList exprs)) = "<mapjoin(pprscl,exprs," ")>";
//public str pprscl(BGFExpression::choice(BGFExprList exprs)) = "[<mapjoin(pprscl,exprs," ")>]";
//public str pprscl(BGFExpression::allof(BGFExprList exprs)) = "allof(<pprscl(exprs)>)";
//public str pprscl(BGFExpression::marked(BGFExpression expr)) = "ERROR";
//public str pprscl(BGFExpression::optional(BGFExpression expr)) = "<pprscl(expr)>?";
//public str pprscl(BGFExpression::not(BGFExpression expr)) = "!<pprscl(expr)>";
//public str pprscl(BGFExpression::plus(BGFExpression expr)) = "<pprscl(expr)>+";
//public str pprscl(BGFExpression::star(BGFExpression expr)) = "<pprscl(expr)>*";
//public str pprscl(BGFExpression::seplistplus(BGFExpression expr, BGFExpression sep)) = "seplistplus(<pprscl(expr)>,<pprscl(sep)>)";
//public str pprscl(BGFExpression::sepliststar(BGFExpression expr, BGFExpression sep)) = "sepliststar(<pprscl(expr)>,<pprscl(sep)>)";
public default str pprscl(BGFExpression smth) = "??<smth>??";

public str pprsc(BGFExprList es) = mapjoin(pprsc,es," ");

public str pprsc(BGFValue::string()) = "string()";
public str pprsc(BGFValue::integer()) = "integer()";
public default str pprsc(BGFValue smth) = "??<smth>??";

