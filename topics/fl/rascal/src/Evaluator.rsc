@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module Evaluator

import Abstract;
import Optimiser;
import PPrinter;

import List;
import String;

FLExpr eval(binary(literal(int x), plus(), literal(int y)), _) = literal(x+y);
FLExpr eval(binary(literal(int x), minus(), literal(int y)), _) = literal(x-y);
FLExpr eval(binary(literal(int x), equal(), literal(int y)), _) = literal(x==y ? 1 : 0);
default FLExpr eval(binary(FLExpr e1, FLOp op, FLExpr e2), set[FLFun] fs) = binary(eval(e1,fs), op, eval(e2,fs));

FLExpr eval(apply(str f, list[FLExpr] vargs), set[FLFun] fs)
{
	if (fun(f, list[str] args, FLExpr body) <- fs)
	{
		if (size(args) != size(vargs))
			throw "Use and definition of function <f> do not agree.";
		e = body;
		for (i <- [0..size(args)-1])
			e = substarg(e, args[i], eval(vargs[i], fs));
		return eval(Optimiser::simplify(eval(Optimiser::simplify(e), fs)),fs);
	}
	return apply(f,[eval(v,fs) | v <- vargs]); // function not in store
}

default FLExpr eval(FLExpr e, set[FLFun] fs) = e;

FLExpr substarg(FLExpr f, str name, FLExpr valu)
{
	return visit(f)
	{
		case argument(name) => valu
	}
}
