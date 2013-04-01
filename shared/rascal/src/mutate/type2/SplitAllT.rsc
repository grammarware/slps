@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{SplitAllT}
module mutate::type2::SplitAllT

import String;
import language::BGF;

BGFGrammar SplitAllT(BGFGrammar g)
{
	BGFProdList ps;
	ps = innermost visit(g.prods)
	{
		case terminal(x) => resplit(x)
	}
	return grammar (g.roots, ps);
}

BGFExpression resplit(str x)
{
	list[str] xs = splitIfSplittable(x);
	if (len(xs)!=1)
		return sequence([terminal(z) | z <- xs]);
	else
		return terminal(x);
}

public list[str] splitIfSplittable(str x)
{
	if (size(x)==1) return [x];
	list[str] xs = [];
	int c;
	int l = charAt(x,0);
	str cur = stringChar(l);
	for (int i <- [1..size(x)-1])
	{
		c = charAt(x,i);
		if (alpha(l) && alpha(c))
			cur += stringChar(c);
		elseif (digit(l) && digit(c))
			cur += stringChar(c);
		else
		{
			xs += cur;
			l = c;
			cur = stringChar(l);
		}
	}
	if (size(cur)!=0)
		xs += cur;
	return xs;
}

bool alpha(int x) = (((x <= 122) && (x >= 97)) || ((x <= 90) && (x >= 65)));
bool digit(int x) = ((x <= 57) && (x >= 48));
