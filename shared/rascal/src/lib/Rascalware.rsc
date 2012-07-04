@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@doc{grammarware's own version of Rascal library structure}
module lib::Rascalware

import Set;
import Relation;
import List;
import IO;

// one length to rule them all
public int len(list[&T] x)   = List::size(x);
public int len(set[&T] x)    = Set::size(x);
public int len(rel[&T,&T] x) = Relation::size(x);

// one isempty to rule them all
public bool isEmpty(list[&T] x)   = List::isEmpty(x);
public bool isEmpty(set[&T] x)    = Set::isEmpty(x);
public bool isEmpty(rel[&T,&T] x) = Relation::isEmpty(x);

public void print(str s)   = IO::print(s);
public void println(str s) = IO::println(s);

// joins a list of strings with a separator
public str joinStrings(list[str] ss, str w) = (ss[0] | it + w + s | s <- tail(ss));

//public bool multiseteq(list[&T] xs, list[&T] ys) = sort(xs) == sort(ys);
public bool multiseteq(list[&T] xs, list[&T] ys) = sort(xs) == sort(ys);
//}

public set[&T] toSet(list[&T] x) = List::toSet(x);
public list[&T] toList(set[&T] x) = Set::toList(x);
