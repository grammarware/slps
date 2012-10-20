@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module structure::MegaConstraints

import structure::MegaADT;
import backend::MegaManipulate;

import io::MegaL;
import IO;   // println
import List; // size

public void main()
{
	AMegaModel m = readFlat(|project://megal/lib/ox/root.megal|);
	println(m);
	println({*{r.x,r.y} | r <- m.rels});
	for (n <- {*{r.x,r.y} | r <- m.rels})
		println("<n> ::= <getAllByName(n,m)>;");
	println(areAllDefined(m));
	println(allAmbiguous(m));
	println(allUndefined(m));
}

public bool isValid(AMegaModel m) = areAllDefined(m);

public bool areAllDefined(AMegaModel m) = (true | it && size(getAllByName(name,m))==1 | name <- {*{r.x,r.y} | r <- m.rels});

public list[str] allUndefined(AMegaModel m) = ([] | size(getAllByName(name,m))==0?(it+name):it | name <- {*{r.x,r.y} | r <- m.rels});

public list[str] allAmbiguous(AMegaModel m) = ([] | size(getAllByName(name,m))>1?(it+name):it | name <- {*{r.x,r.y} | r <- m.rels});
