@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@doc{Grammar Diff Tool}
module diff::GDT

import syntax::BGF;
import normal::BGF;
import List;
import IO;

public bool gdt(grammar(rs1,ps1), grammar(rs2,ps2))
{
	ps3 = normalise(ps1);
	ps4 = normalise(ps2);
	if (toSet(rs1)!=toSet(rs2)) return false;
	if (toSet(ps3)==toSet(ps4)) return true;
	unmatched1 = ps3 - ps4;
	unmatched2 = ps4 - ps3;
	for (u <- unmatched1)
		if (production(l,x,choice(L1)) := u)
			for (production(l,x,choice(L2)) <- unmatched2)
				if (toSet(L1) == toSet(L2)) {unmatched2 -= production(l,x,choice(L2));unmatched1 -= u;break;}
	if (isEmpty(unmatched1) && isEmpty(unmatched2)) return true;
	// TODO keep trying?
	return false;
}
