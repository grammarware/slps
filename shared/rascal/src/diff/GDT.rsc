@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@doc{Grammar Diff Tool}
module diff::GDT

import syntax::BGF;
import List;

public bool gdt(grammar(rs1,ps1), grammar(rs2,ps2))
{
	if (toSet(rs1)==toSet(rs2) && toSet(ps1)==toSet(ps2)) return true;
	return false;
}
