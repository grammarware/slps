@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@doc{grammarware's own version of Rascal library structure}
module lib::Rascalware

import Relation;
import String;
import List;
import Set;
import Map;
import IO;
import util::Math;

// one length to rule them all
public int len(list[&T] x)     = List::size(x);
public int len(set[&T] x)      = Set::size(x);
public int len(map[&T1,&T2] x) = Map::size(x);
public int len(rel[&T1,&T2] x) = Relation::size(x);
public int len(str x)          = String::size(x);

// one isempty to rule them all
public bool isEmpty(list[&T] x)     = List::isEmpty(x);
public bool isEmpty(set[&T] x)      = Set::isEmpty(x);
public bool isEmpty(map[&T1,&T2] x) = Map::isEmpty(x);
public bool isEmpty(rel[&T1,&T2] x) = Relation::isEmpty(x);
public bool isEmpty(str x)          = (x=="");

// one min to rule them all
public int min(int x, int y)        = util::Math::min(x,y);
public int min(int x, int y, int z) = List::min([x,y,z]);
public &T min(list[&T] xs)          = List::min(xs);
public &T min(set[&T] xs)           = Set::min(xs);

public void print(str s)   = IO::print(s);
public void println(str s) = IO::println(s);

// joins a list of strings with a separator
public str joinStrings([], _) = "";
public str joinStrings(list[str] ss, str w) = (ss[0] | it + w + s | s <- tail(ss));

public str mapjoin(&T1(&T2) f, list[&T2] xs, str sep) = joinStrings(mapper(xs,f),sep);
public str mapjoin(&T1(&T2) f, set[&T2] xs, str sep) = joinStrings(toList(mapper(xs,f)),sep);

//public bool multiseteq(list[&T] xs, list[&T] ys) = sort(xs) == sort(ys);
public bool multiseteq(list[&T] xs, list[&T] ys) = sort(xs) == sort(ys);

public bool seteq(list[&T] xs, list[&T] ys) = toSet(xs) == toSet(ys);

public bool subset(set[&T] xs, set[&T] ys) = xs <= ys;
public bool subset(list[&T] xs, set[&T] ys) = toSet(xs) <= ys;
public bool subset(set[&T] xs, list[&T] ys) = xs <= toSet(ys);
public bool subset(list[&T] xs, list[&T] ys) = toSet(xs) <= toSet(ys);

public set[&T] toSet(list[&T] x) = List::toSet(x);
public list[&T] toList(set[&T] x) = Set::toList(x);

public &T getOneFrom(set[&T] xs)  = Set::getOneFrom(xs);
public &T getOneFrom(list[&T] xs) = List::getOneFrom(xs);

public list[&T] slice(list[&T] lst, int begin, int l) = List::slice(lst,begin,l);

public str replace(str w, map[str,str] m)
{
	for (k <- m)
		w = String::replaceAll(w,k,m[k]);
	return w;
}

// fancy lines in output or debug messages
public str pad("") = center("",50,"-")+"\n";
public default str pad(str x) = center(" <x>: ",50,"-")+"\n";


// classic Levenshtein distance: done for negotiated grammar transformations, but possibly of greater use
public int levenshtein(str x, str y)
{
	if (size(x) < size(y)) return levenshtein(y,x);
	if (x=="") return size(y);
	
	prow = [0..size(y)];
	for (i <- [0..size(x)-1])
	{
		crow = [i+1];
		for (j <- [0..size(y)-1])
			if (charAt(x,i) == charAt(y,j))
				crow += min([ prow[j+1]+1, crow[j]+1, prow[j]   ]);
			else
				crow += min([ prow[j+1]+1, crow[j]+1, prow[j]+1 ]);
		prow = crow;
	} 
	return prow[size(prow)-1];
}

