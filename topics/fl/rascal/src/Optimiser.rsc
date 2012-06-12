@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module Optimiser

import Abstract;

FLPrg simplify(FLPrg p)
{
	return visit(p)
	{
		case FLExpr e => simplify(e)
	}
}

FLExpr simplify(FLExpr p)
{
	return visit(p)
	{
		case binary(FLExpr e,plus(),literal(0)) => e
		case binary(literal(0),plus(),FLExpr e) => e
		case binary(FLExpr e,minus(),literal(0)) => e
		case binary(literal(int x),equal(),literal(int y)) => x == y ? literal(1) : literal(0)
		case binary(FLExpr e1,equal(),FLExpr e2) => e1 == e2 ? 1 : binary(e1,equal(),e2)
		case ifThenElse(literal(int x),FLExpr e1,FLExpr e2) => x == 0 ? e2 : e1
		case ifThenElse(FLExpr c,FLExpr e1,FLExpr e2) => e1 == e2 ? e1 : ifThenElse(c,e1,e2)
		case ifThenElse(binary(literal(0),equal(),FLExpr c),FLExpr e1,FLExpr e2) => ifThenElse(c,e2,e1)
		case ifThenElse(binary(FLExpr c,equal(),literal(0)),FLExpr e1,FLExpr e2) => ifThenElse(c,e2,e1)
	}
}
