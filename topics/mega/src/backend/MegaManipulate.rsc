@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module backend::MegaManipulate

import structure::MegaADT;

import List;
import Set;

public MegaDeclaration getByName(str name, AMegaModel m)
{
	list[MegaDeclaration] mine = getAllByName(name,m);
	if(size(mine)==1) return mine[0];
	else throw "Declaration for <name> not found or not unique: <mine>"; 
}

public list[MegaDeclaration] getAllByName(str name, AMegaModel m) = [d | d <- m.decls, d.id == name];

public list[&T] mergeLists(list[&T] xs, list[&T] ys) = toList(toSet(xs)+toSet(ys));