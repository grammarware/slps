@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@doc{Grammar Diff Tool}
module diff::GDT

import syntax::BGF;
import normal::BGF;
import List;
import IO;

// expression equality
public bool eqE(choice([BGFExpression e1]), choice([BGFExpression e2])) = eqE(e1,e2);
public bool eqE(choice(L1), choice(L2))
{
	//println("We\'re at eqE with:\n <L1>\nvs\n <L2>...");
	for (x <- L1, y <- L2)
		if (eqE(x,y))
			return eqE(choice(L1 - x), choice(L2 - y));
	//println("Unmatched <L1> with <L2> :(");
	return false;
}
public bool eqE(sequence(L1), sequence(L2))
{
	if (size(L1) != size(L2)) return false;
	for (i <- [0..size(L1)-1])
		//if (choice(L3) := L1[i] && choice(L4) := L2[i])
		if (!eqE(L1[i],L2[i])) return false;
	return true;
}
public bool eqE(BGFExpression e1, BGFExpression e2) = e1 == e2; // default


public bool eqP(production(str l,str x, BGFExpression e1), production(l,x, BGFExpression e2)) = eqE(e1,e2);
public bool eqP(BGFExpression p1, BGFExpression p2) = p1 == p2;

// generic differ, returns unmatched production rules
tuple[list[BGFProduction],list[BGFProduction]] gdt(list[BGFProduction] ps1, list[BGFProduction] ps2)
{
	ps3 = normalise(ps1);
	ps4 = normalise(ps2);
	if (toSet(ps3)==toSet(ps4)) return <[],[]>;
	unmatched1 = ps3 - ps4;
	unmatched2 = ps4 - ps3;
	for (u <- unmatched1)
		if (production(str l,str x,BGFExpression e1) := u)
			for (production(l,x,BGFExpression e2) <- unmatched2)
				if (eqE(e1,e2))
					{
						unmatched2 -= production(l,x,e2);
						unmatched1 -= u;
						break;
					}
	return <unmatched1,unmatched2>;
}

// silent
public bool gdts(grammar(rs1,ps1), grammar(rs2,ps2))
{
	if (toSet(rs1)!=toSet(rs2)) return false;
	<unmatched1,unmatched2> = gdt(ps1,ps2);
	if (isEmpty(unmatched1) && isEmpty(unmatched2)) return true;
	// TODO keep trying?
	return false;
}

// verbose
public bool gdtv(grammar(rs1,ps1), grammar(rs2,ps2))
{
	if (toSet(rs1)!=toSet(rs2))
	{
		println("Different roots: <rs1> vs <rs2>.");
		return false;
	}
	<unmatched1,unmatched2> = gdt(ps1,ps2);
	if (isEmpty(unmatched1) && isEmpty(unmatched2)) return true;
	println("Grammars differ!");
	for (u <- unmatched1)
		println(u);
	println("vs");
	for (u <- unmatched2)
		println(u);
	// TODO keep trying?
	return false;
}