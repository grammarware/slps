tree(T) --> tags(T).

tag(xml(N)) -->
 string("<"),
 tagname(N),
 string(">"),
 tags(L),
 string("</"),
 tagname(N),
 string(">").

tags(tlist(H,T)) -->
 tag(H),
 tags(T).
tags([]) --> [].

string([],X,X).
string([H|T1],[H|T2],X) :- string(T1,T2,X).

spaces --> [0' ], spaces. %'
spaces --> [].

tagname(N) -->
 letter(H), letters(T),
 { atom_chars(N,[H|T]) }.

letters([H|T]) --> letter(H), letters(T).
letters([]) --> [].
letter(H,[H|T],T) :- H >= 0'a, H =< 0'z.
