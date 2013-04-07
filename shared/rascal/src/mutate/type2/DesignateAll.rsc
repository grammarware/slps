@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{DesignateAll}
module mutate::type2::DesignateAll

import language::BGF;
import Map;

BGFGrammar DesignateAll(BGFGrammar g)
{
	map[str,int] nums = ();
	BGFProdList ps = [];
	for (p<-g.prods)
	{
		if (p.lhs notin domain(nums))
			nums[p.lhs] = 1;
		if (p.label=="")
		{
			label = "<p.lhs><nums[p.lhs]>";
			nums[p.lhs] += 1;
		}
		else
			label = p.label;
		ps += production(label,p.lhs,p.rhs);
	}
	return grammar(g.roots,ps);
}