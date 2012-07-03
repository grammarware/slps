@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@doc{grammarware's own version of Rascal library structure}
module lib::Rascalware

import Set;
import Relation;
import List;
import IO;

// one length to rule them all
public int len(list[&T] l)   = List::size(l);
public int len(set[&T] l)    = Set::size(l);
public int len(rel[&T,&T] l) = Relation::size(l);

// one isempty to rule them all
public bool isEmpty(list[&T] l)   = List::isEmpty(l);
public bool isEmpty(set[&T] l)    = Set::isEmpty(l);
public bool isEmpty(rel[&T,&T] l) = Relation::isEmpty(l);

public void print(str s)   = IO::print(s);
public void println(str s) = IO::println(s);

// joins a list of strings with a separator
public str joinStrings(list[str] ss, str w) = (ss[0] | it + w + s | s <- tail(ss));

//public bool multiseteq(list[&T] xs, list[&T] ys) = sort(xs) == sort(ys);
public bool multiseteq(list[&T] xs, list[&T] ys) = sort(xs) == sort(ys);
//}
