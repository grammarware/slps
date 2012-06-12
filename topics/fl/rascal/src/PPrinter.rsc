@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module PPrinter

import Abstract;
import List;

// joins a list of strings with a separator
public str joinStrings(list[str] ss, str w) = (ss[0] | it + w + s | s <- tail(ss));

public str pp(prg(list[FLFun] fs)) = joinStrings([pp(f) | f <- fs],"\n");
public str pp(fun(str f, list[str] args, FLExpr body)) = "<f> <joinStrings(args," ")> = <pp(body)>";

public str pp(binary(FLExpr e1, minus(), FLExpr e2)) = "<pp(e1)> - <pp(e2)>";
public str pp(binary(FLExpr e1, plus(), FLExpr e2)) = "<pp(e1)> + <pp(e2)>";
public str pp(binary(FLExpr e1, equal(), FLExpr e2)) = "<pp(e1)>==<pp(e2)>";
public str pp(apply(str f, list[FLExpr] vargs)) = "<f> <joinStrings([pp2(a) | a <- vargs]," ")>";
public str pp(ifThenElse(FLExpr c, FLExpr t, FLExpr e)) = "if <pp(c)> then <pp(t)> else <pp(e)>";
public str pp(argument(str a)) = "<a>";
public str pp(literal(int i)) = "<i>";
public default str pp(FLExpr e) = "UNCOVERED[<e>]";

public str pp2(argument(str a)) = "<a>";
public str pp2(literal(int i)) = "<i>";
public default str pp2(FLExpr e) = "(<pp(e)>)";